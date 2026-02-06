// CLI pour le projet ECRIN - analyse première vague.
//
// Usage:
//
//	go run ecrin.go              # Lance le menu interactif
//	go run ecrin.go metadata     # Récupère les métadonnées REDCap
//	go run ecrin.go instruments  # Affiche la liste des instruments
//	go run ecrin.go export       # Exporte les données en CSV
//	go run ecrin.go diffusion    # Récupère les paramètres de diffusion
//	go run ecrin.go rapport      # Compile le rapport Quarto en PDF
//	go run ecrin.go clean        # Supprime les fichiers générés
package main

import (
	"bufio"
	"crypto/sha256"
	"encoding/csv"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/briandowns/spinner"
	"github.com/fatih/color"
	"github.com/manifoldco/promptui"
	"github.com/urfave/cli/v2"
)

const dataDir = "data"
const reportsDir = "reports"

var (
	green  = color.New(color.FgGreen).SprintFunc()
	cyan   = color.New(color.FgCyan).SprintFunc()
	yellow = color.New(color.FgYellow).SprintFunc()
	bold   = color.New(color.Bold).SprintFunc()
)

type Instrument struct {
	Name  string `json:"instrument_name"`
	Label string `json:"instrument_label"`
}

type Variable struct {
	FormName       string `json:"form_name"`
	FieldName      string `json:"field_name"`
	FieldLabel     string `json:"field_label"`
	FieldType      string `json:"field_type"`
	Choices        string `json:"select_choices_or_calculations"`
	Required       string `json:"required_field"`
	BranchingLogic string `json:"branching_logic"`
	Identifier     string `json:"identifier"` // "y" si PHI (Protected Health Information)
}

// InstrumentConfig décrit un instrument REDCap ayant des champs de diffusion
type InstrumentConfig struct {
	Name           string   // Nom technique (ex: "researcher_profile")
	Label          string   // Label humain (ex: "Researcher Profile")
	IDLevelField   string   // Champ identification_level (ex: "researcher_profile_identification_level")
	AudienceField  string   // Champ data_audience (ex: "researcher_profile_data_audience")
	HasIdentifiers bool     // true si l'instrument a des champs identifier="y"
	FileFields     []string // Champs de type "file" (ex: ["portrait"])
}

// StatsDetail détaille les participants comptés en statistiques seules
type StatsDetail struct {
	NoResponse       int // Pas de choix de diffusion
	AudienceFiltered int // Exclus par audience (rapport Public uniquement)
	Aggregated       int // Choix "Aggregated only"
}

// Total retourne le nombre total de participants en statistiques seules
func (s StatsDetail) Total() int {
	return s.NoResponse + s.AudienceFiltered + s.Aggregated
}

// InstrumentResult contient les résultats du téléchargement d'un instrument
type InstrumentResult struct {
	Config        InstrumentConfig
	IdentRecords  []map[string]any
	PseudoRecords []map[string]any
	AnonRecords   []map[string]any
	Stats         StatsDetail
	Files         []string // Chemins des fichiers téléchargés
}

// buildInstrumentConfigs détecte automatiquement les instruments avec diffusion
func buildInstrumentConfigs(instruments []Instrument, metadata []Variable) []InstrumentConfig {
	// Trouver les instruments ayant un champ _identification_level
	instrumentsWithDiffusion := make(map[string]bool)
	for _, v := range metadata {
		if strings.HasSuffix(v.FieldName, "_identification_level") {
			instrumentsWithDiffusion[v.FormName] = true
		}
	}

	var configs []InstrumentConfig
	for _, inst := range instruments {
		if !instrumentsWithDiffusion[inst.Name] {
			continue
		}

		config := InstrumentConfig{
			Name:          inst.Name,
			Label:         inst.Label,
			IDLevelField:  inst.Name + "_identification_level",
			AudienceField: inst.Name + "_data_audience",
		}

		// Détecter les champs identifiants et fichiers
		for _, v := range metadata {
			if v.FormName != inst.Name {
				continue
			}
			if v.Identifier == "y" {
				config.HasIdentifiers = true
			}
			if v.FieldType == "file" {
				config.FileFields = append(config.FileFields, v.FieldName)
			}
		}

		configs = append(configs, config)
	}
	return configs
}

type task struct {
	spinner *spinner.Spinner
	steps   []string
	current int
}

func newTask(steps ...string) *task {
	s := spinner.New(spinner.CharSets[14], 100*time.Millisecond)
	_ = s.Color("cyan")
	return &task{spinner: s, steps: steps, current: -1}
}

func (t *task) run(index int) {
	if t.current >= 0 {
		t.spinner.Stop()
		fmt.Printf("  %s %s\n", green("✓"), t.steps[t.current])
	}
	t.current = index
	t.spinner.Suffix = " " + t.steps[index]
	t.spinner.Start()
}

func (t *task) done() {
	if t.current >= 0 {
		t.spinner.Stop()
		fmt.Printf("  %s %s\n", green("✓"), t.steps[t.current])
	}
}

func (t *task) fail() {
	if t.current >= 0 {
		t.spinner.Stop()
		fmt.Printf("  %s %s\n", color.RedString("✗"), t.steps[t.current])
	}
}

func loadEnv() {
	file, err := os.Open(".env")
	if err != nil {
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		parts := strings.SplitN(line, "=", 2)
		if len(parts) == 2 {
			key := strings.TrimSpace(parts[0])
			value := strings.TrimSpace(parts[1])
			value = strings.Trim(value, `"'`)
			os.Setenv(key, value)
		}
	}
}

func getConfig() (apiURL, token string, err error) {
	apiURL = os.Getenv("REDCAP_API_URL")
	token = os.Getenv("REDCAP_TOKEN")

	if token == "" {
		return "", "", fmt.Errorf("REDCAP_TOKEN non défini. Créez un fichier .env avec REDCAP_TOKEN=votre_token")
	}

	if apiURL == "" {
		return "", "", fmt.Errorf("REDCAP_API_URL non défini, créez un fichier .env avec REDCAP_API_URL=https://")
	}

	return apiURL, token, nil
}

func redcapRequest(apiURL, token, content string, extra map[string]string) ([]byte, error) {
	data := url.Values{}
	data.Set("token", token)
	data.Set("content", content)
	data.Set("format", "json")
	for k, v := range extra {
		data.Set(k, v)
	}

	resp, err := http.PostForm(apiURL, data)
	if err != nil {
		return nil, fmt.Errorf("erreur requête: %w", err)
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("erreur lecture: %w", err)
	}

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("erreur API (status %d): %s", resp.StatusCode, string(body))
	}

	return body, nil
}

func getInstruments(apiURL, token string) ([]Instrument, error) {
	body, err := redcapRequest(apiURL, token, "instrument", nil)
	if err != nil {
		return nil, err
	}

	var instruments []Instrument
	if err := json.Unmarshal(body, &instruments); err != nil {
		return nil, fmt.Errorf("erreur parsing instruments: %w", err)
	}
	return instruments, nil
}

func getMetadata(apiURL, token string) ([]Variable, error) {
	body, err := redcapRequest(apiURL, token, "metadata", nil)
	if err != nil {
		return nil, err
	}

	var metadata []Variable
	if err := json.Unmarshal(body, &metadata); err != nil {
		return nil, fmt.Errorf("erreur parsing metadata: %w", err)
	}
	return metadata, nil
}

func hashID(id string) string {
	h := sha256.Sum256([]byte(id))
	return hex.EncodeToString(h[:])[:16]
}

func hashRecordIDs(records []map[string]any, idField string) {
	for _, r := range records {
		if v, ok := r[idField]; ok {
			originalID := fmt.Sprintf("%v", v)
			r["_original_id"] = originalID // Conserver l'ID original pour les téléchargements de fichiers
			r[idField] = hashID(originalID)
		}
	}
}

func getRecords(apiURL, token string, idField string) ([]map[string]any, error) {
	extra := map[string]string{
		"rawOrLabel": "label",
	}
	body, err := redcapRequest(apiURL, token, "record", extra)
	if err != nil {
		return nil, err
	}

	var records []map[string]any
	if err := json.Unmarshal(body, &records); err != nil {
		return nil, fmt.Errorf("erreur parsing records: %w", err)
	}
	hashRecordIDs(records, idField)
	return records, nil
}

func getRecordsWithFields(apiURL, token string, fields []string) ([]map[string]any, error) {
	return getRecordsWithFieldsOpts(apiURL, token, fields, true)
}

func getRecordsWithFieldsRaw(apiURL, token string, fields []string) ([]map[string]any, error) {
	return getRecordsWithFieldsOpts(apiURL, token, fields, false)
}

func getRecordsWithFieldsOpts(apiURL, token string, fields []string, hashIDs bool) ([]map[string]any, error) {
	extra := map[string]string{
		"rawOrLabel": "label",
		"fields":     strings.Join(fields, ","),
	}
	body, err := redcapRequest(apiURL, token, "record", extra)
	if err != nil {
		return nil, err
	}

	var records []map[string]any
	if err := json.Unmarshal(body, &records); err != nil {
		return nil, fmt.Errorf("erreur parsing records: %w", err)
	}
	// Le premier champ est toujours l'identifiant
	if hashIDs && len(fields) > 0 {
		hashRecordIDs(records, fields[0])
	}
	return records, nil
}

func findDiffusionFields(metadata []Variable) []Variable {
	var fields []Variable
	for _, v := range metadata {
		if strings.HasSuffix(v.FieldName, "_identification_level") || strings.HasSuffix(v.FieldName, "_audience") {
			fields = append(fields, v)
		}
	}
	return fields
}

// downloadFile télécharge un fichier depuis REDCap
func downloadFile(apiURL, token, recordID, fieldName, destPath string) error {
	data := url.Values{}
	data.Set("token", token)
	data.Set("content", "file")
	data.Set("action", "export")
	data.Set("record", recordID)
	data.Set("field", fieldName)

	resp, err := http.PostForm(apiURL, data)
	if err != nil {
		return fmt.Errorf("erreur requête: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("erreur API (status %d): %s", resp.StatusCode, string(body))
	}

	// Créer le fichier de destination
	out, err := os.Create(destPath)
	if err != nil {
		return fmt.Errorf("erreur création fichier: %w", err)
	}
	defer out.Close()

	_, err = io.Copy(out, resp.Body)
	if err != nil {
		return fmt.Errorf("erreur écriture fichier: %w", err)
	}

	return nil
}

// splitFieldsByIdentifier sépare les champs d'un formulaire en identifiants et non-identifiants
func splitFieldsByIdentifier(metadata []Variable, formName string) (identifiers, nonIdentifiers []string) {
	for _, v := range metadata {
		if v.FormName == formName && v.FieldType != "descriptive" &&
			!strings.HasSuffix(v.FieldName, "_identification_level") &&
			!strings.HasSuffix(v.FieldName, "_data_audience") {
			if v.Identifier == "y" {
				identifiers = append(identifiers, v.FieldName)
			} else {
				nonIdentifiers = append(nonIdentifiers, v.FieldName)
			}
		}
	}
	return
}

// getFormFields retourne tous les champs d'un formulaire (hors descriptive et champs de contrôle diffusion)
func getFormFields(metadata []Variable, formName string) []string {
	var fields []string
	for _, v := range metadata {
		if v.FormName == formName && v.FieldType != "descriptive" &&
			!strings.HasSuffix(v.FieldName, "_identification_level") &&
			!strings.HasSuffix(v.FieldName, "_data_audience") {
			fields = append(fields, v.FieldName)
		}
	}
	return fields
}

func saveJSON(data any, filepath string) error {
	file, err := os.Create(filepath)
	if err != nil {
		return err
	}
	defer file.Close()

	encoder := json.NewEncoder(file)
	encoder.SetIndent("", "  ")
	encoder.SetEscapeHTML(false)
	return encoder.Encode(data)
}

func truncateString(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-1] + "…"
}

func printRecordsTable(records []map[string]any, columns []string, maxRows int) {
	if len(records) == 0 || len(columns) == 0 {
		return
	}

	// Calculer les largeurs de colonnes
	colWidths := make([]int, len(columns))
	maxColWidth := 25
	for i, col := range columns {
		colWidths[i] = len(col)
	}

	rowCount := len(records)
	if rowCount > maxRows {
		rowCount = maxRows
	}

	for i := 0; i < rowCount; i++ {
		for j, col := range columns {
			if v, ok := records[i][col]; ok {
				valStr := fmt.Sprintf("%v", v)
				if len(valStr) > colWidths[j] {
					colWidths[j] = len(valStr)
				}
			}
		}
	}

	// Limiter la largeur max
	for i := range colWidths {
		if colWidths[i] > maxColWidth {
			colWidths[i] = maxColWidth
		}
	}

	// Afficher l'en-tête
	fmt.Print("  ")
	for i, col := range columns {
		fmt.Printf("%-*s ", colWidths[i], truncateString(col, colWidths[i]))
	}
	fmt.Println()

	// Ligne de séparation
	fmt.Print("  ")
	for i := range columns {
		fmt.Print(strings.Repeat("─", colWidths[i]) + " ")
	}
	fmt.Println()

	// Afficher les lignes
	for i := 0; i < rowCount; i++ {
		fmt.Print("  ")
		for j, col := range columns {
			val := ""
			if v, ok := records[i][col]; ok {
				val = fmt.Sprintf("%v", v)
			}
			fmt.Printf("%-*s ", colWidths[j], truncateString(val, colWidths[j]))
		}
		fmt.Println()
	}

	if len(records) > maxRows {
		fmt.Printf("  %s\n", color.HiBlackString("... et %d autres lignes", len(records)-maxRows))
	}
}

func printRecordsStats(records []map[string]any, columns []string) {
	if len(records) == 0 {
		return
	}

	fmt.Printf("\n  %s\n", bold("Statistiques:"))

	for _, col := range columns {
		valueCounts := make(map[string]int)
		emptyCount := 0

		for _, r := range records {
			if v, ok := r[col]; ok {
				valStr := fmt.Sprintf("%v", v)
				if valStr == "" || valStr == "<nil>" {
					emptyCount++
				} else {
					valueCounts[valStr]++
				}
			} else {
				emptyCount++
			}
		}

		// Afficher les stats pour cette colonne
		fmt.Printf("\n    %s:\n", cyan(col))

		if emptyCount > 0 {
			pct := float64(emptyCount) * 100 / float64(len(records))
			fmt.Printf("      %s %d (%.0f%%)\n", color.HiBlackString("Vide:"), emptyCount, pct)
		}

		// Trier par fréquence et afficher les valeurs
		type kv struct {
			Key   string
			Value int
		}
		var sorted []kv
		for k, v := range valueCounts {
			sorted = append(sorted, kv{k, v})
		}
		sort.Slice(sorted, func(i, j int) bool {
			return sorted[i].Value > sorted[j].Value
		})

		// Afficher max 5 valeurs
		maxDisplay := 5
		if len(sorted) < maxDisplay {
			maxDisplay = len(sorted)
		}
		for i := 0; i < maxDisplay; i++ {
			pct := float64(sorted[i].Value) * 100 / float64(len(records))
			fmt.Printf("      %s: %d (%.0f%%)\n", truncateString(sorted[i].Key, 30), sorted[i].Value, pct)
		}
		if len(sorted) > 5 {
			fmt.Printf("      %s\n", color.HiBlackString("... et %d autres valeurs", len(sorted)-5))
		}
	}
}

func generateVariablesCSV(metadata []Variable, filepath string) error {
	file, err := os.Create(filepath)
	if err != nil {
		return err
	}
	defer file.Close()

	writer := csv.NewWriter(file)
	defer writer.Flush()

	writer.Write([]string{"instrument", "variable", "label", "type", "choices", "required", "branching_logic"})

	for _, v := range metadata {
		writer.Write([]string{
			v.FormName,
			v.FieldName,
			v.FieldLabel,
			v.FieldType,
			v.Choices,
			v.Required,
			v.BranchingLogic,
		})
	}
	return nil
}

func generateReport(instruments []Instrument, metadata []Variable, filepath string) error {
	file, err := os.Create(filepath)
	if err != nil {
		return err
	}
	defer file.Close()

	varsByInstrument := make(map[string][]Variable)
	for _, v := range metadata {
		varsByInstrument[v.FormName] = append(varsByInstrument[v.FormName], v)
	}

	fmt.Fprintf(file, "%s\n", strings.Repeat("=", 60))
	fmt.Fprintf(file, "RAPPORT DES VARIABLES REDCAP\n")
	fmt.Fprintf(file, "Généré le %s\n", time.Now().Format("2006-01-02 15:04"))
	fmt.Fprintf(file, "%s\n\n", strings.Repeat("=", 60))

	fmt.Fprintf(file, "Nombre d'instruments : %d\n", len(instruments))
	fmt.Fprintf(file, "Nombre total de variables : %d\n\n", len(metadata))

	for _, inst := range instruments {
		vars := varsByInstrument[inst.Name]

		fmt.Fprintf(file, "%s\n", strings.Repeat("-", 60))
		fmt.Fprintf(file, "INSTRUMENT : %s\n", inst.Label)
		fmt.Fprintf(file, "Nom technique : %s\n", inst.Name)
		fmt.Fprintf(file, "Variables : %d\n", len(vars))
		fmt.Fprintf(file, "%s\n\n", strings.Repeat("-", 60))

		for _, v := range vars {
			required := "  "
			if v.Required == "y" {
				required = "★ "
			}

			fmt.Fprintf(file, "%s• %s [%s]\n", required, v.FieldName, v.FieldType)
			fmt.Fprintf(file, "    Label: %s\n", v.FieldLabel)

			if v.Choices != "" && (v.FieldType == "dropdown" || v.FieldType == "radio" || v.FieldType == "checkbox") {
				fmt.Fprintf(file, "    Choix: %s\n", v.Choices)
			}

			if v.BranchingLogic != "" {
				fmt.Fprintf(file, "    Condition: %s\n", v.BranchingLogic)
			}

			fmt.Fprintln(file)
		}
		fmt.Fprintln(file)
	}

	return nil
}

func cmdInstruments(c *cli.Context) error {
	apiURL, token, err := getConfig()
	if err != nil {
		return err
	}

	t := newTask("Récupération des instruments")
	fmt.Println()
	t.run(0)

	instruments, err := getInstruments(apiURL, token)
	if err != nil {
		t.fail()
		return err
	}
	t.done()

	fmt.Printf("\n%d instruments trouvés\n\n", len(instruments))
	fmt.Printf("%-30s %s\n", bold("Nom technique"), bold("Label"))
	fmt.Println(strings.Repeat("─", 70))
	for _, inst := range instruments {
		fmt.Printf("%-30s %s\n", cyan(inst.Name), inst.Label)
	}
	fmt.Println()
	return nil
}

func cmdExport(c *cli.Context) error {
	apiURL, token, err := getConfig()
	if err != nil {
		return err
	}

	t := newTask(
		"Récupération des métadonnées",
		"Récupération des enregistrements",
		"Export CSV",
	)

	fmt.Printf("\n%s\n\n", bold("Export des données REDCap"))
	os.MkdirAll(dataDir, 0755)

	// Step 1: Métadonnées
	t.run(0)
	metadata, err := getMetadata(apiURL, token)
	if err != nil {
		t.fail()
		return err
	}
	idField := metadata[0].FieldName

	// Step 2: Récupération
	t.run(1)
	records, err := getRecords(apiURL, token, idField)
	if err != nil {
		t.fail()
		return err
	}

	if len(records) == 0 {
		t.done()
		fmt.Printf("\n%s Aucune donnée à exporter.\n\n", yellow("⚠"))
		return nil
	}

	// Step 3: Export
	t.run(2)
	var keys []string
	for k := range records[0] {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	timestamp := time.Now().Format("2006-01-02_1504")
	outPath := filepath.Join(dataDir, fmt.Sprintf("data_export_%s.csv", timestamp))

	file, err := os.Create(outPath)
	if err != nil {
		t.fail()
		return fmt.Errorf("erreur création fichier: %w", err)
	}
	defer file.Close()

	writer := csv.NewWriter(file)
	writer.Write(keys)

	for _, record := range records {
		var row []string
		for _, k := range keys {
			if v, ok := record[k]; ok {
				row = append(row, fmt.Sprintf("%v", v))
			} else {
				row = append(row, "")
			}
		}
		writer.Write(row)
	}
	writer.Flush()
	t.done()

	fmt.Printf("\n%s %s enregistrements exportés\n", green("✓"), bold(fmt.Sprintf("%d", len(records))))
	fmt.Printf("  → %s\n\n", cyan(outPath))

	// Aperçu des données
	fmt.Printf("  %s\n\n", bold("Aperçu des données:"))
	previewCols := keys
	if len(previewCols) > 6 {
		previewCols = previewCols[:6]
	}
	printRecordsTable(records, previewCols, 5)

	return nil
}

func cmdMetadata(c *cli.Context) error {
	apiURL, token, err := getConfig()
	if err != nil {
		return err
	}

	t := newTask(
		"Récupération des instruments",
		"Récupération du dictionnaire de données",
		"Génération du CSV récapitulatif",
		"Génération du rapport",
	)

	fmt.Printf("\n%s\n", bold("Récupération des métadonnées REDCap"))
	fmt.Printf("  URL: %s\n\n", cyan(apiURL))
	os.MkdirAll(dataDir, 0755)
	os.MkdirAll(reportsDir, 0755)

	// Step 1: Instruments
	t.run(0)
	instruments, err := getInstruments(apiURL, token)
	if err != nil {
		t.fail()
		return err
	}
	if err := saveJSON(instruments, filepath.Join(dataDir, "instruments.json")); err != nil {
		t.fail()
		return err
	}

	// Step 2: Metadata
	t.run(1)
	metadata, err := getMetadata(apiURL, token)
	if err != nil {
		t.fail()
		return err
	}
	if err := saveJSON(metadata, filepath.Join(dataDir, "metadata.json")); err != nil {
		t.fail()
		return err
	}

	// Step 3: CSV
	t.run(2)
	if err := generateVariablesCSV(metadata, filepath.Join(reportsDir, "variables_recap.csv")); err != nil {
		t.fail()
		return err
	}

	// Step 4: Rapport
	t.run(3)
	if err := generateReport(instruments, metadata, filepath.Join(reportsDir, "rapport_variables.txt")); err != nil {
		t.fail()
		return err
	}
	t.done()

	// Summary
	fmt.Printf("\n%s Terminé!\n\n", green("✓"))
	fmt.Printf("  %s %d instruments\n", bold("Instruments:"), len(instruments))
	fmt.Printf("  %s %d variables\n\n", bold("Variables:"), len(metadata))

	fmt.Printf("  %s\n", bold("Fichiers générés:"))
	for _, dir := range []string{dataDir, reportsDir} {
		entries, _ := os.ReadDir(dir)
		for _, e := range entries {
			fmt.Printf("    → %s\n", cyan(filepath.Join(dir, e.Name())))
		}
	}

	// Aperçu des variables (tableau)
	fmt.Printf("\n  %s\n\n", bold("Aperçu des variables:"))
	fmt.Printf("  %-25s %-15s %-30s\n", bold("Variable"), bold("Type"), bold("Instrument"))
	fmt.Printf("  %s %s %s\n", strings.Repeat("─", 25), strings.Repeat("─", 15), strings.Repeat("─", 30))
	maxVars := 10
	if len(metadata) < maxVars {
		maxVars = len(metadata)
	}
	for i := 0; i < maxVars; i++ {
		v := metadata[i]
		fmt.Printf("  %-25s %-15s %-30s\n",
			truncateString(v.FieldName, 25),
			v.FieldType,
			truncateString(v.FormName, 30))
	}
	if len(metadata) > 10 {
		fmt.Printf("  %s\n", color.HiBlackString("... et %d autres variables", len(metadata)-10))
	}

	// Résumé par instrument
	fmt.Printf("\n  %s\n\n", bold("Résumé par instrument:"))
	varsByInstrument := make(map[string]int)
	typesByInstrument := make(map[string]map[string]int)
	for _, v := range metadata {
		varsByInstrument[v.FormName]++
		if typesByInstrument[v.FormName] == nil {
			typesByInstrument[v.FormName] = make(map[string]int)
		}
		typesByInstrument[v.FormName][v.FieldType]++
	}

	for _, inst := range instruments {
		count := varsByInstrument[inst.Name]
		types := typesByInstrument[inst.Name]

		// Construire la liste des types
		var typeList []string
		for t, c := range types {
			typeList = append(typeList, fmt.Sprintf("%s:%d", t, c))
		}
		sort.Strings(typeList)

		fmt.Printf("    %s (%d variables)\n", cyan(inst.Label), count)
		fmt.Printf("      Types: %s\n", strings.Join(typeList, ", "))
	}
	fmt.Println()

	return nil
}

func cmdDiffusion(c *cli.Context) error {
	apiURL, token, err := getConfig()
	if err != nil {
		return err
	}

	t := newTask(
		"Récupération des métadonnées",
		"Identification des champs de diffusion",
		"Récupération des valeurs",
		"Export CSV",
	)

	fmt.Printf("\n%s\n\n", bold("Récupération des paramètres de diffusion REDCap"))
	os.MkdirAll(dataDir, 0755)

	// Step 1: Metadata
	t.run(0)
	metadata, err := getMetadata(apiURL, token)
	if err != nil {
		t.fail()
		return err
	}

	// Step 2: Find diffusion fields
	t.run(1)
	diffusionFields := findDiffusionFields(metadata)
	if len(diffusionFields) == 0 {
		t.fail()
		fmt.Printf("\n%s Aucun champ de diffusion trouvé (patterns: *_identification_level, *_audience)\n\n", yellow("⚠"))
		return nil
	}

	// Step 3: Get records
	t.run(2)
	recordIDField := metadata[0].FieldName
	fieldNames := []string{recordIDField}
	for _, f := range diffusionFields {
		fieldNames = append(fieldNames, f.FieldName)
	}

	records, err := getRecordsWithFields(apiURL, token, fieldNames)
	if err != nil {
		t.fail()
		return err
	}

	// Step 4: Export CSV
	t.run(3)
	outPath := filepath.Join(dataDir, "diffusion.csv")
	file, err := os.Create(outPath)
	if err != nil {
		t.fail()
		return fmt.Errorf("erreur création fichier: %w", err)
	}
	defer file.Close()

	writer := csv.NewWriter(file)
	writer.Write(fieldNames)

	for _, record := range records {
		var row []string
		for _, fn := range fieldNames {
			if v, ok := record[fn]; ok {
				row = append(row, fmt.Sprintf("%v", v))
			} else {
				row = append(row, "")
			}
		}
		writer.Write(row)
	}
	writer.Flush()
	t.done()

	// Summary
	fmt.Printf("\n%s Terminé!\n\n", green("✓"))
	fmt.Printf("  %s %d champs de diffusion trouvés:\n", bold("Diffusion:"), len(diffusionFields))
	for _, f := range diffusionFields {
		fmt.Printf("    • %s (%s)\n", cyan(f.FieldName), f.FormName)
	}
	fmt.Printf("\n  %s %d enregistrements\n", bold("Participants:"), len(records))
	fmt.Printf("  %s %s\n", bold("Fichier:"), cyan(outPath))

	// Aperçu des données (tableau)
	fmt.Printf("\n  %s\n\n", bold("Aperçu des données:"))
	printRecordsTable(records, fieldNames, 5)

	// Statistiques sur les champs de diffusion (sans l'ID)
	statFields := fieldNames[1:]
	printRecordsStats(records, statFields)
	fmt.Println()

	return nil
}

const rapportQmd = "rapport-analyse-premiere-vague.qmd"

type AudienceType string

const (
	AudiencePublic     AudienceType = "public"
	AudienceChercheurs AudienceType = "chercheurs"
)

func runQuarto(args ...string) error {
	cmd := exec.Command("quarto", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func cmdRapportPDF(c *cli.Context) error {
	t := newTask("Compilation du rapport en PDF")
	fmt.Println()
	t.run(0)
	err := runQuarto("render", rapportQmd, "--to", "pdf")
	if err != nil {
		t.fail()
		return fmt.Errorf("erreur Quarto: %w", err)
	}
	t.done()
	fmt.Println()
	return nil
}

func cmdRapportHTML(c *cli.Context) error {
	t := newTask("Compilation du rapport en HTML")
	fmt.Println()
	t.run(0)
	err := runQuarto("render", rapportQmd, "--to", "html")
	if err != nil {
		t.fail()
		return fmt.Errorf("erreur Quarto: %w", err)
	}
	t.done()
	fmt.Println()
	return nil
}

func cmdRapportPreview(c *cli.Context) error {
	fmt.Printf("\n%s\n", bold("Lancement du preview Quarto"))
	fmt.Printf("  Appuyez sur Ctrl+C pour arrêter\n\n")
	return runQuarto("preview", rapportQmd)
}

func selectAudience() (AudienceType, error) {
	items := []struct {
		Name        string
		Value       AudienceType
		Description string
	}{
		{Name: "Public", Value: AudiencePublic, Description: "Rapport destiné au grand public"},
		{Name: "Chercheurs", Value: AudienceChercheurs, Description: "Rapport restreint aux chercheurs autorisés"},
	}

	templates := &promptui.SelectTemplates{
		Label:    "{{ . }}",
		Active:   "▸ {{ .Name | cyan }} - {{ .Description }}",
		Inactive: "  {{ .Name | white }} - {{ .Description | faint }}",
		Selected: "{{ .Name | green | bold }}",
	}

	prompt := promptui.Select{
		Label:     "Pour quelle audience souhaitez-vous générer le rapport ?",
		Items:     items,
		Templates: templates,
		Size:      len(items),
	}

	idx, _, err := prompt.Run()
	if err != nil {
		return "", err
	}

	return items[idx].Value, nil
}

// translateFieldType traduit un type de champ REDCap en français
func translateFieldType(fieldType string) string {
	switch fieldType {
	case "text":
		return "Texte"
	case "radio":
		return "Choix unique"
	case "dropdown":
		return "Liste déroulante"
	case "checkbox":
		return "Cases à cocher"
	case "yesno":
		return "Oui/Non"
	case "file":
		return "Fichier"
	case "notes":
		return "Zone de texte"
	case "sql":
		return "Liste dynamique"
	default:
		return fieldType
	}
}

// generateInstrumentVariablesTable génère un tableau markdown des variables d'un instrument
func generateInstrumentVariablesTable(metadata []Variable, formName string, hasIdentifiers bool) string {
	var b strings.Builder
	if hasIdentifiers {
		b.WriteString("| Variable | Type | Identifiante |\n")
		b.WriteString("|----------|------|-------------|\n")
	} else {
		b.WriteString("| Variable | Type |\n")
		b.WriteString("|----------|------|\n")
	}
	for _, v := range metadata {
		if v.FormName == formName && v.FieldType != "descriptive" {
			if hasIdentifiers {
				identMark := "Non"
				if v.Identifier == "y" {
					identMark = "**Oui**"
				}
				b.WriteString(fmt.Sprintf("| `%s` | %s | %s |\n", v.FieldName, translateFieldType(v.FieldType), identMark))
			} else {
				b.WriteString(fmt.Sprintf("| `%s` | %s |\n", v.FieldName, translateFieldType(v.FieldType)))
			}
		}
	}
	return b.String()
}

// generateInstrumentSection génère la section QMD pour un instrument
func generateInstrumentSection(result InstrumentResult, metadata []Variable, sectionIndex int) string {
	name := result.Config.Name
	label := result.Config.Label
	prefix := fmt.Sprintf("%s_%d", name, sectionIndex)

	nIdent := len(result.IdentRecords)
	nPseudo := len(result.PseudoRecords)
	nAnon := len(result.AnonRecords)
	nStats := result.Stats.Total()
	nTotal := nIdent + nPseudo + nAnon + nStats

	if nTotal == 0 {
		return fmt.Sprintf("\n## %s\n\nAucune donnée pour cet instrument.\n\n", label)
	}

	var b strings.Builder

	// Titre de section
	b.WriteString(fmt.Sprintf("\n## %s\n\n", label))

	// Tableau des variables
	varTable := generateInstrumentVariablesTable(metadata, name, result.Config.HasIdentifiers)
	b.WriteString("### Variables de l'instrument\n\n")
	b.WriteString(varTable)
	b.WriteString("\n")

	// Statistiques par niveau via R
	b.WriteString(fmt.Sprintf("### Statistiques\n\n"))
	b.WriteString(fmt.Sprintf("```{r}\n#| label: stats-%s\n#| results: asis\n\n", prefix))

	// Charger les CSV de données
	b.WriteString(fmt.Sprintf(`ident_%s <- tryCatch(read_csv("../data/vague2_%s_identifiables.csv", show_col_types = FALSE), error = function(e) tibble())
`, prefix, name))
	b.WriteString(fmt.Sprintf(`pseudo_%s <- tryCatch(read_csv("../data/vague3_%s_pseudonymises.csv", show_col_types = FALSE), error = function(e) tibble())
`, prefix, name))
	b.WriteString(fmt.Sprintf(`anon_%s <- tryCatch(read_csv("../data/vague4_%s_anonymises.csv", show_col_types = FALSE), error = function(e) tibble())
`, prefix, name))

	// Charger le CSV des statistiques
	b.WriteString(fmt.Sprintf(`stats_%s <- tryCatch(read_csv("../data/vague5_%s_statistiques.csv", show_col_types = FALSE), error = function(e) tibble(categorie = character(), nombre = integer()))
`, prefix, name))

	b.WriteString(fmt.Sprintf(`
n_ident <- nrow(ident_%s)
n_pseudo <- nrow(pseudo_%s)
n_anon <- nrow(anon_%s)

# Lire le détail des statistiques depuis le CSV
get_stat <- function(df, cat) {
  row <- df[df$categorie == cat, ]
  if (nrow(row) > 0) row$nombre[1] else 0L
}
n_sans_reponse <- get_stat(stats_%s, "sans_reponse")
n_filtre_audience <- get_stat(stats_%s, "filtre_audience")
n_agreges <- get_stat(stats_%s, "agreges")
n_stats <- n_sans_reponse + n_filtre_audience + n_agreges
n_total <- n_ident + n_pseudo + n_anon + n_stats

cat(paste0("- **Total** : ", n_total, " enregistrement(s)\n"))
cat(paste0("- **Identifiables** : ", n_ident, "\n"))
cat(paste0("- **Pseudonymisés** : ", n_pseudo, "\n"))
cat(paste0("- **Anonymisés** : ", n_anon, "\n"))
if (n_stats > 0) {
  cat(paste0("- **Statistiques seules** : ", n_stats, "\n"))
  if (n_sans_reponse > 0) cat(paste0("  - Sans réponse de diffusion : ", n_sans_reponse, "\n"))
  if (n_filtre_audience > 0) cat(paste0("  - Filtrés par audience : ", n_filtre_audience, "\n"))
  if (n_agreges > 0) cat(paste0("  - Agrégés uniquement : ", n_agreges, "\n"))
}
`, prefix, prefix, prefix, prefix, prefix, prefix))

	b.WriteString("```\n\n")

	// Tableau des profils identifiables
	if nIdent > 0 {
		b.WriteString("### Données identifiables\n\n")
		b.WriteString(fmt.Sprintf("```{r}\n#| label: ident-%s\n#| results: asis\n\n", prefix))
		b.WriteString(fmt.Sprintf(`if (nrow(ident_%s) > 0) {
  display_cols <- names(ident_%s)
  # Limiter à 6 colonnes pour la lisibilité
  if (length(display_cols) > 6) display_cols <- display_cols[1:6]
  kable(ident_%s[, display_cols], caption = paste0("Identifiables (", nrow(ident_%s), ")")) %%>%%
    kable_styling(latex_options = c("striped", "hold_position", "scale_down")) %%>%%
    print()
}
`, prefix, prefix, prefix, prefix))
		b.WriteString("```\n\n")
	}

	// Tableau des profils pseudonymisés
	if nPseudo > 0 {
		b.WriteString("### Données pseudonymisées\n\n")
		b.WriteString(fmt.Sprintf("```{r}\n#| label: pseudo-%s\n#| results: asis\n\n", prefix))
		b.WriteString(fmt.Sprintf(`if (nrow(pseudo_%s) > 0) {
  display_cols <- names(pseudo_%s)
  if (length(display_cols) > 6) display_cols <- display_cols[1:6]
  kable(pseudo_%s[, display_cols], caption = paste0("Pseudonymisés (", nrow(pseudo_%s), ")")) %%>%%
    kable_styling(latex_options = c("striped", "hold_position", "scale_down")) %%>%%
    print()
}
`, prefix, prefix, prefix, prefix))
		b.WriteString("```\n\n")
	}

	// Comptage anonymisés
	if nAnon > 0 {
		b.WriteString("### Données anonymisées\n\n")
		b.WriteString(fmt.Sprintf("```{r}\n#| label: anon-%s\n#| results: asis\n\n", prefix))
		b.WriteString(fmt.Sprintf(`cat(paste0("**", nrow(anon_%s), " enregistrement(s) anonymisé(s)**.\n\n"))
`, prefix))
		b.WriteString("```\n\n")
	}

	return b.String()
}

// generateReportQmd génère le rapport Quarto unique regroupant tous les instruments
func generateReportQmd(audience AudienceType, metadata []Variable, results []InstrumentResult) (string, error) {
	watermark := ""
	if audience == AudienceChercheurs {
		watermark = `
header-includes:
  - \usepackage{draftwatermark}
  - \SetWatermarkText{Restricted - Authorized researchers only}
  - \SetWatermarkScale{0.25}
  - \SetWatermarkColor[gray]{0.9}`
	}

	audienceLabel := "Grand public"
	if audience == AudienceChercheurs {
		audienceLabel = "Chercheurs autorisés"
	}

	var b strings.Builder

	// En-tête YAML
	b.WriteString(fmt.Sprintf(`---
title: "Rapport ECRIN - Première vague"
subtitle: "Audience : %s"
author: "Plateforme ECRIN"
date: "%s"
lang: fr
format:
  pdf:
    toc: true
    toc-depth: 2
    documentclass: article
    papersize: a4
    geometry:
      - margin=2.5cm
    colorlinks: true%s
execute:
  echo: false
  warning: false
  message: false
---

`, audienceLabel, time.Now().Format("2006-01-02"), watermark))

	// Setup R
	b.WriteString("```{r}\n#| label: setup\n#| include: false\n\nlibrary(tidyverse)\nlibrary(knitr)\nlibrary(kableExtra)\n```\n\n")

	// Introduction
	b.WriteString(fmt.Sprintf("Ce rapport présente les données de la première vague ECRIN pour l'audience **%s**.\n\n", audienceLabel))

	// Résumé global
	b.WriteString("## Résumé\n\n")
	b.WriteString("| Instrument | Identifiables | Pseudonymisés | Anonymisés | Statistiques seules | Total |\n")
	b.WriteString("|------------|:---:|:---:|:---:|:---:|:---:|\n")
	for _, r := range results {
		nI := len(r.IdentRecords)
		nP := len(r.PseudoRecords)
		nA := len(r.AnonRecords)
		nStats := r.Stats.Total()
		nT := nI + nP + nA + nStats
		b.WriteString(fmt.Sprintf("| %s | %d | %d | %d | %d | **%d** |\n", r.Config.Label, nI, nP, nA, nStats, nT))
	}
	b.WriteString("\n")

	// Sections par instrument
	for i, r := range results {
		section := generateInstrumentSection(r, metadata, i)
		b.WriteString(section)
	}

	// Pied de page
	b.WriteString(fmt.Sprintf("\n---\n\n*Rapport généré le %s*\n", time.Now().Format("2006-01-02 15:04")))

	// Écrire le fichier QMD
	qmdPath := filepath.Join(reportsDir, "rapport-ecrin.qmd")
	if err := os.WriteFile(qmdPath, []byte(b.String()), 0644); err != nil {
		return "", fmt.Errorf("erreur écriture QMD: %w", err)
	}

	return qmdPath, nil
}

// downloadInstrumentData télécharge les données d'un instrument depuis REDCap
// en respectant les niveaux d'identification, et exporte les CSV par niveau
func downloadInstrumentData(apiURL, token string, metadata []Variable, idField string, audience AudienceType, config InstrumentConfig) (InstrumentResult, error) {
	result := InstrumentResult{Config: config}

	identifiers, nonIdentifiers := splitFieldsByIdentifier(metadata, config.Name)
	allFormFields := getFormFields(metadata, config.Name)

	// Champs à utiliser selon que l'instrument a des identifiants ou non
	// Si pas d'identifiants, tous les champs sont "non-identifiants"
	dataFields := nonIdentifiers
	if !config.HasIdentifiers {
		dataFields = allFormFields
	}

	// Champs de nom provenant de researcher_profile, ajoutés aux instruments
	// sans identifiants propres quand le participant choisit "Identifiable"
	nameFields := []string{"last_name", "first_name", "middle_name"}

	// 1. Télécharger les champs de contrôle pour classifier les participants
	controlFields := []string{idField, config.IDLevelField, config.AudienceField}
	controlRecords, err := getRecordsWithFieldsRaw(apiURL, token, controlFields)
	if err != nil {
		return result, fmt.Errorf("[%s] erreur récupération champs de contrôle: %w", config.Name, err)
	}

	// Classer les IDs par niveau d'identification
	var identIDs, pseudoIDs, anonIDs, aggregatedIDs []string
	for _, r := range controlRecords {
		id := fmt.Sprintf("%v", r[idField])
		idLevel, _ := r[config.IDLevelField].(string)
		dataAudience, _ := r[config.AudienceField].(string)

		// Enregistrements sans choix de diffusion → statistiques seules
		if idLevel == "" {
			result.Stats.NoResponse++
			continue
		}

		// Filtrer par audience si rapport Public :
		// les participants "Researchers only" sont comptés en statistiques mais pas téléchargés
		if audience == AudiencePublic && dataAudience != "General public" {
			result.Stats.AudienceFiltered++
			continue
		}

		switch {
		case strings.Contains(idLevel, "Identifiable"):
			identIDs = append(identIDs, id)
		case strings.Contains(idLevel, "Pseudonymised"):
			pseudoIDs = append(pseudoIDs, id)
		case strings.Contains(idLevel, "Anonymised"):
			anonIDs = append(anonIDs, id)
		case strings.Contains(idLevel, "Aggregated"):
			aggregatedIDs = append(aggregatedIDs, id)
		}
	}

	// 2. Identifiables : tous les champs + ID original
	// Pour les instruments sans identifiants propres, on ajoute last_name, first_name, middle_name
	if len(identIDs) > 0 {
		var fieldsToDownload []string
		if config.HasIdentifiers {
			fieldsToDownload = append([]string{idField}, identifiers...)
			fieldsToDownload = append(fieldsToDownload, nonIdentifiers...)
		} else {
			fieldsToDownload = append([]string{idField}, nameFields...)
			fieldsToDownload = append(fieldsToDownload, allFormFields...)
		}

		records, err := getRecordsWithFieldsAndIDs(apiURL, token, fieldsToDownload, identIDs)
		if err != nil {
			return result, fmt.Errorf("[%s] erreur récupération identifiables: %w", config.Name, err)
		}

		if len(records) > 0 {
			var csvFields []string
			if config.HasIdentifiers {
				csvFields = append(identifiers, nonIdentifiers...)
			} else {
				csvFields = append(nameFields, allFormFields...)
			}
			csvPath := filepath.Join(dataDir, fmt.Sprintf("vague2_%s_identifiables.csv", config.Name))
			if err := writeInstrumentCSV(csvPath, records, csvFields, idField, false); err != nil {
				return result, err
			}
			result.IdentRecords = records
		}
	}

	// 3. Pseudonymisés : champs non-identifiants + ID hashé
	if len(pseudoIDs) > 0 {
		fieldsToDownload := append([]string{idField}, dataFields...)

		records, err := getRecordsWithFieldsAndIDs(apiURL, token, fieldsToDownload, pseudoIDs)
		if err != nil {
			return result, fmt.Errorf("[%s] erreur récupération pseudonymisés: %w", config.Name, err)
		}

		if len(records) > 0 {
			csvFields := append([]string{"hashed_id"}, dataFields...)
			csvPath := filepath.Join(dataDir, fmt.Sprintf("vague3_%s_pseudonymises.csv", config.Name))
			if err := writeInstrumentCSV(csvPath, records, csvFields, idField, true); err != nil {
				return result, err
			}
			result.PseudoRecords = records
		}
	}

	// 4. Anonymisés : champs non-identifiants sans ID
	if len(anonIDs) > 0 {
		fieldsToDownload := append([]string{idField}, dataFields...) // ID juste pour la requête
		records, err := getRecordsWithFieldsAndIDs(apiURL, token, fieldsToDownload, anonIDs)
		if err != nil {
			return result, fmt.Errorf("[%s] erreur récupération anonymisés: %w", config.Name, err)
		}

		if len(records) > 0 {
			csvPath := filepath.Join(dataDir, fmt.Sprintf("vague4_%s_anonymises.csv", config.Name))
			if err := writeInstrumentCSV(csvPath, records, dataFields, idField, false); err != nil {
				return result, err
			}
			result.AnonRecords = records
		}
	}

	// 5. Agrégés : comptage seulement
	if len(aggregatedIDs) > 0 {
		result.Stats.Aggregated += len(aggregatedIDs)
	}

	// 6. Export CSV des statistiques
	if result.Stats.Total() > 0 {
		csvPath := filepath.Join(dataDir, fmt.Sprintf("vague5_%s_statistiques.csv", config.Name))
		f, err := os.Create(csvPath)
		if err != nil {
			return result, err
		}
		defer f.Close()
		w := csv.NewWriter(f)
		defer w.Flush()
		w.Write([]string{"categorie", "nombre"})
		w.Write([]string{"sans_reponse", fmt.Sprintf("%d", result.Stats.NoResponse)})
		w.Write([]string{"filtre_audience", fmt.Sprintf("%d", result.Stats.AudienceFiltered)})
		w.Write([]string{"agreges", fmt.Sprintf("%d", result.Stats.Aggregated)})
		w.Write([]string{"total", fmt.Sprintf("%d", result.Stats.Total())})
	}

	return result, nil
}

// writeInstrumentCSV écrit un CSV générique pour un instrument
func writeInstrumentCSV(path string, records []map[string]any, fields []string, idField string, includeHashedID bool) error {
	file, err := os.Create(path)
	if err != nil {
		return err
	}
	defer file.Close()

	writer := csv.NewWriter(file)
	defer writer.Flush()

	writer.Write(fields)
	for _, r := range records {
		var row []string
		for _, f := range fields {
			if f == "hashed_id" && includeHashedID {
				if id, ok := r[idField]; ok {
					row = append(row, hashID(fmt.Sprintf("%v", id)))
				} else {
					row = append(row, "")
				}
			} else if v, ok := r[f]; ok {
				row = append(row, fmt.Sprintf("%v", v))
			} else {
				row = append(row, "")
			}
		}
		writer.Write(row)
	}
	return nil
}

// downloadInstrumentFiles télécharge les fichiers (portraits, publications, papers) pour les profils identifiables
func downloadInstrumentFiles(apiURL, token string, records []map[string]any, idField string, config InstrumentConfig) ([]string, error) {
	if len(config.FileFields) == 0 || len(records) == 0 {
		return nil, nil
	}

	filesDir := filepath.Join(dataDir, "fichiers", config.Name)
	os.MkdirAll(filesDir, 0755)

	var downloadedFiles []string
	for _, r := range records {
		recordID := fmt.Sprintf("%v", r[idField])

		for _, fieldName := range config.FileFields {
			// Vérifier que le champ a une valeur
			val, _ := r[fieldName].(string)
			if val == "" || val == "<nil>" {
				continue
			}

			// Cas spécial: portrait avec portrait_choice
			if fieldName == "portrait" {
				choice, _ := r["portrait_choice"].(string)
				if !strings.Contains(choice, "picture") && !strings.Contains(choice, "2") {
					continue
				}
			}

			ext := filepath.Ext(val)
			if ext == "" {
				ext = ".bin"
			}
			hashedID := hashID(recordID)
			destPath := filepath.Join(filesDir, fmt.Sprintf("%s_%s%s", hashedID, fieldName, ext))

			if err := downloadFile(apiURL, token, recordID, fieldName, destPath); err != nil {
				continue // Ignorer les erreurs de téléchargement
			}

			// Normaliser le DPI pour les images
			if strings.HasSuffix(strings.ToLower(ext), ".jpg") || strings.HasSuffix(strings.ToLower(ext), ".jpeg") || strings.HasSuffix(strings.ToLower(ext), ".png") {
				normalizeImageDPI(destPath)
			}

			downloadedFiles = append(downloadedFiles, destPath)
		}
	}

	return downloadedFiles, nil
}

// getRecordsWithFieldsAndIDs récupère des enregistrements spécifiques avec des champs spécifiques
func getRecordsWithFieldsAndIDs(apiURL, token string, fields []string, recordIDs []string) ([]map[string]any, error) {
	data := url.Values{}
	data.Set("token", token)
	data.Set("content", "record")
	data.Set("format", "json")
	data.Set("type", "flat")
	data.Set("rawOrLabel", "label")
	data.Set("fields", strings.Join(fields, ","))
	data.Set("records", strings.Join(recordIDs, ","))

	resp, err := http.PostForm(apiURL, data)
	if err != nil {
		return nil, fmt.Errorf("erreur requête: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("erreur API (status %d): %s", resp.StatusCode, string(body))
	}

	var records []map[string]any
	if err := json.NewDecoder(resp.Body).Decode(&records); err != nil {
		return nil, fmt.Errorf("erreur décodage JSON: %w", err)
	}

	return records, nil
}

// normalizeImageDPI utilise ImageMagick pour fixer le DPI à 72
func normalizeImageDPI(imagePath string) {
	cmd := exec.Command("magick", "mogrify", "-density", "72", imagePath)
	cmd.Run()
}

// cleanOutputDir nettoie les dossiers data/ et reports/ avant un nouveau téléchargement
func cleanOutputDir() error {
	patterns := []string{
		filepath.Join(dataDir, "vague*_*.csv"),
		filepath.Join(dataDir, "fichiers"),
		filepath.Join(reportsDir, "rapport-ecrin.*"),
	}

	for _, pattern := range patterns {
		files, _ := filepath.Glob(pattern)
		for _, f := range files {
			info, err := os.Stat(f)
			if err != nil {
				continue
			}
			if info.IsDir() {
				os.RemoveAll(f)
			} else {
				os.Remove(f)
			}
		}
	}

	return nil
}

func cmdRapportProfils(c *cli.Context) error {
	apiURL, token, err := getConfig()
	if err != nil {
		return err
	}

	fmt.Printf("\n%s\n\n", bold("Génération du rapport ECRIN"))

	// Sélection de l'audience
	audience, err := selectAudience()
	if err != nil {
		if err == promptui.ErrInterrupt {
			fmt.Println("\nAnnulé.")
			return nil
		}
		return err
	}

	fmt.Printf("\n  Audience sélectionnée: %s\n\n", cyan(string(audience)))

	os.MkdirAll(dataDir, 0755)
	os.MkdirAll(reportsDir, 0755)

	t := newTask(
		"Nettoyage des données précédentes",
		"Récupération des métadonnées",
		"Détection des instruments",
		"Téléchargement des données par instrument",
		"Téléchargement des fichiers",
		"Génération du template Quarto",
		"Compilation du rapport en PDF",
	)

	// Step 0: Nettoyage
	t.run(0)
	cleanOutputDir()

	// Step 1: Métadonnées et instruments
	t.run(1)
	instruments, err := getInstruments(apiURL, token)
	if err != nil {
		t.fail()
		return err
	}
	metadata, err := getMetadata(apiURL, token)
	if err != nil {
		t.fail()
		return err
	}
	idField := metadata[0].FieldName

	// Step 2: Détecter les instruments avec diffusion
	t.run(2)
	configs := buildInstrumentConfigs(instruments, metadata)
	if len(configs) == 0 {
		t.done()
		fmt.Printf("\n%s Aucun instrument avec diffusion trouvé.\n\n", yellow("⚠"))
		return nil
	}
	for _, cfg := range configs {
		fmt.Printf("  %s %s", green("✓"), cfg.Label)
		if cfg.HasIdentifiers {
			fmt.Printf(" (avec champs identifiants)")
		}
		if len(cfg.FileFields) > 0 {
			fmt.Printf(" (fichiers: %s)", strings.Join(cfg.FileFields, ", "))
		}
		fmt.Println()
	}

	// Step 3: Télécharger les données pour chaque instrument
	t.run(3)
	var results []InstrumentResult
	for _, cfg := range configs {
		result, err := downloadInstrumentData(apiURL, token, metadata, idField, audience, cfg)
		if err != nil {
			t.fail()
			return err
		}
		nTotal := len(result.IdentRecords) + len(result.PseudoRecords) + len(result.AnonRecords) + result.Stats.Total()
		fmt.Printf("  %s %s: %d enregistrements", green("✓"), cfg.Label, nTotal)
		if result.Stats.Total() > 0 {
			fmt.Printf(" (dont %d en statistiques seules)", result.Stats.Total())
		}
		fmt.Println()
		results = append(results, result)
	}

	// Step 4: Télécharger les fichiers pour les profils identifiables
	t.run(4)
	for i, result := range results {
		if len(result.Config.FileFields) > 0 && len(result.IdentRecords) > 0 {
			files, err := downloadInstrumentFiles(apiURL, token, result.IdentRecords, idField, result.Config)
			if err == nil {
				results[i].Files = files
				fmt.Printf("  %s %s: %d fichiers téléchargés\n", green("✓"), result.Config.Label, len(files))
			}
		}
	}

	// Step 5: Générer le QMD
	t.run(5)
	qmdPath, err := generateReportQmd(audience, metadata, results)
	if err != nil {
		t.fail()
		return err
	}

	// Step 6: Compiler en PDF
	t.run(6)
	if err := runQuarto("render", qmdPath, "--to", "pdf"); err != nil {
		t.fail()
		return fmt.Errorf("erreur Quarto: %w", err)
	}
	t.done()

	pdfPath := strings.TrimSuffix(qmdPath, ".qmd") + ".pdf"
	fmt.Printf("\n%s Rapport généré!\n", green("✓"))
	fmt.Printf("  → %s\n\n", cyan(pdfPath))

	return nil
}

func cmdClean(c *cli.Context) error {
	fmt.Printf("\n%s\n\n", bold("Nettoyage des fichiers générés"))

	patterns := []string{
		dataDir,
		reportsDir,
		"*.pdf",
		"*.html",
		"*_files",
		".quarto",
		"*_cache",
	}

	for _, pattern := range patterns {
		matches, _ := filepath.Glob(pattern)
		for _, match := range matches {
			info, err := os.Stat(match)
			if err != nil {
				continue
			}
			if info.IsDir() {
				os.RemoveAll(match)
			} else {
				os.Remove(match)
			}
			fmt.Printf("  %s %s\n", color.RedString("✗"), match)
		}
	}

	fmt.Printf("\n%s Nettoyage terminé\n\n", green("✓"))
	return nil
}

type menuItem struct {
	Name        string
	Description string
	Action      func() error
}

func runInteractiveMenu() error {
	items := []menuItem{
		{Name: "Métadonnées", Description: "Récupère les métadonnées complètes (instruments, variables)", Action: func() error { return cmdMetadata(nil) }},
		{Name: "Instruments", Description: "Affiche la liste des instruments", Action: func() error { return cmdInstruments(nil) }},
		{Name: "Export", Description: "Exporte les données en CSV", Action: func() error { return cmdExport(nil) }},
		{Name: "Diffusion", Description: "Récupère les paramètres de diffusion", Action: func() error { return cmdDiffusion(nil) }},
		{Name: "Rapport Profils", Description: "Génère le rapport des profils chercheurs (PDF)", Action: func() error { return cmdRapportProfils(nil) }},
		{Name: "Rapport complet PDF", Description: "Compile le rapport Quarto complet en PDF", Action: func() error { return cmdRapportPDF(nil) }},
		{Name: "Rapport complet HTML", Description: "Compile le rapport Quarto complet en HTML", Action: func() error { return cmdRapportHTML(nil) }},
		{Name: "Preview", Description: "Lance le preview Quarto", Action: func() error { return cmdRapportPreview(nil) }},
		{Name: "Nettoyage", Description: "Supprime les fichiers générés", Action: func() error { return cmdClean(nil) }},
		{Name: "Quitter", Description: "Ferme le programme", Action: nil},
	}

	templates := &promptui.SelectTemplates{
		Label:    "{{ . }}",
		Active:   "▸ {{ .Name | cyan }} - {{ .Description }}",
		Inactive: "  {{ .Name | white }} - {{ .Description | faint }}",
		Selected: "{{ .Name | green | bold }}",
	}

	for {
		fmt.Printf("\n%s\n", bold("═══════════════════════════════════════"))
		fmt.Printf("  %s - Menu principal\n", bold("ECRIN"))
		fmt.Printf("%s\n\n", bold("═══════════════════════════════════════"))

		prompt := promptui.Select{
			Label:     "Choisissez une action",
			Items:     items,
			Templates: templates,
			Size:      len(items),
		}

		idx, _, err := prompt.Run()
		if err != nil {
			if err == promptui.ErrInterrupt {
				fmt.Println("\nAu revoir!")
				return nil
			}
			return err
		}

		selected := items[idx]
		if selected.Action == nil {
			fmt.Println("\nAu revoir!")
			return nil
		}

		if err := selected.Action(); err != nil {
			fmt.Fprintf(os.Stderr, "\n%s %v\n", color.RedString("Erreur:"), err)
		}

		fmt.Print("\nAppuyez sur Entrée pour continuer...")
		bufio.NewReader(os.Stdin).ReadBytes('\n')

		// Clear terminal
		cmd := exec.Command("clear")
		cmd.Stdout = os.Stdout
		cmd.Run()
	}
}

func main() {
	loadEnv()

	app := &cli.App{
		Name:    "ecrin",
		Usage:   "CLI pour le projet ECRIN - analyse première vague",
		Version: "1.0.0",
		Commands: []*cli.Command{
			{
				Name:    "metadata",
				Aliases: []string{"m"},
				Usage:   "Récupère les métadonnées complètes (instruments, variables)",
				Action:  cmdMetadata,
			},
			{
				Name:    "instruments",
				Aliases: []string{"i"},
				Usage:   "Affiche la liste des instruments",
				Action:  cmdInstruments,
			},
			{
				Name:    "export",
				Aliases: []string{"e"},
				Usage:   "Exporte les données en CSV",
				Action:  cmdExport,
			},
			{
				Name:    "diffusion",
				Aliases: []string{"d"},
				Usage:   "Récupère les paramètres de diffusion (*_identification_level, *_audience)",
				Action:  cmdDiffusion,
			},
			{
				Name:    "rapport",
				Aliases: []string{"r"},
				Usage:   "Compile le rapport Quarto",
				Subcommands: []*cli.Command{
					{
						Name:   "pdf",
						Usage:  "Compile en PDF (défaut)",
						Action: cmdRapportPDF,
					},
					{
						Name:   "html",
						Usage:  "Compile en HTML",
						Action: cmdRapportHTML,
					},
					{
						Name:   "preview",
						Usage:  "Lance le preview Quarto",
						Action: cmdRapportPreview,
					},
					{
						Name:   "profils",
						Usage:  "Génère le rapport des profils chercheurs (sélection d'audience)",
						Action: cmdRapportProfils,
					},
				},
				Action: cmdRapportPDF,
			},
			{
				Name:   "clean",
				Usage:  "Supprime les fichiers générés",
				Action: cmdClean,
			},
		},
		Action: func(c *cli.Context) error {
			return runInteractiveMenu()
		},
	}

	if err := app.Run(os.Args); err != nil {
		fmt.Fprintf(os.Stderr, "\n%s %v\n\n", color.RedString("Erreur:"), err)
		os.Exit(1)
	}
}
