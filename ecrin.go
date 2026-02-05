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

const outputDir = "data"

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
			r[idField] = hashID(fmt.Sprintf("%v", v))
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
	if len(fields) > 0 {
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
	os.MkdirAll(outputDir, 0755)

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
	outPath := filepath.Join(outputDir, fmt.Sprintf("data_export_%s.csv", timestamp))

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
	os.MkdirAll(outputDir, 0755)

	// Step 1: Instruments
	t.run(0)
	instruments, err := getInstruments(apiURL, token)
	if err != nil {
		t.fail()
		return err
	}
	if err := saveJSON(instruments, filepath.Join(outputDir, "instruments.json")); err != nil {
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
	if err := saveJSON(metadata, filepath.Join(outputDir, "metadata.json")); err != nil {
		t.fail()
		return err
	}

	// Step 3: CSV
	t.run(2)
	if err := generateVariablesCSV(metadata, filepath.Join(outputDir, "variables_recap.csv")); err != nil {
		t.fail()
		return err
	}

	// Step 4: Rapport
	t.run(3)
	if err := generateReport(instruments, metadata, filepath.Join(outputDir, "rapport_variables.txt")); err != nil {
		t.fail()
		return err
	}
	t.done()

	// Summary
	fmt.Printf("\n%s Terminé!\n\n", green("✓"))
	fmt.Printf("  %s %d instruments\n", bold("Instruments:"), len(instruments))
	fmt.Printf("  %s %d variables\n\n", bold("Variables:"), len(metadata))

	fmt.Printf("  %s\n", bold("Fichiers générés:"))
	entries, _ := os.ReadDir(outputDir)
	for _, e := range entries {
		fmt.Printf("    → %s\n", cyan(filepath.Join(outputDir, e.Name())))
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
	os.MkdirAll(outputDir, 0755)

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
	outPath := filepath.Join(outputDir, "diffusion.csv")
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

func cmdClean(c *cli.Context) error {
	fmt.Printf("\n%s\n\n", bold("Nettoyage des fichiers générés"))

	patterns := []string{
		outputDir,
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
		{Name: "Rapport PDF", Description: "Compile le rapport Quarto en PDF", Action: func() error { return cmdRapportPDF(nil) }},
		{Name: "Rapport HTML", Description: "Compile le rapport Quarto en HTML", Action: func() error { return cmdRapportHTML(nil) }},
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
