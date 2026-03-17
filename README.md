# Analyse Première Vague - ECRIN

## Prérequis

- [R](https://www.r-project.org) ≥ 4.1 — `brew install r`
- [Quarto](https://quarto.org) — `brew install --cask quarto`
- [uv](https://docs.astral.sh/uv/) — `brew install uv`
- Packages R : `httr2`, `jsonlite`, `digest`, `readr`, `dplyr`, `stringr`, `fs`

```r
install.packages(c("httr2", "jsonlite", "digest", "readr", "dplyr", "stringr", "fs"))
```

```bash
uv python install   # installe Python (géré par uv, pas besoin de brew install python)
uv sync             # crée le venv et installe les dépendances Python
```

## Configuration

Créer un fichier `.env` à la racine du projet :

```env
REDCAP_API_URL=https://votre-instance-redcap.fr/api/
REDCAP_TOKEN=votre_token_api
```

## Workflow recommandé — CLI Python (`ecrin.py`)

Le CLI Python orchestre le pipeline de façon déclarative : un fichier YAML déclare le rapport souhaité, le script vérifie ce qui a changé sur REDCap depuis le dernier téléchargement et n'agit que sur ce qui est nécessaire.

### 1. Créer et activer un stack

```bash
uv run ecrin.py stack init public       # génère stacks/public.yml
uv run ecrin.py stack select public     # active ce stack
```

Éditer `stacks/public.yml` :

```yaml
rapport:
  titre: "Rapport ECRIN — Première vague"
  audience: "public"        # "public" | "chercheurs"
  format: [pdf]
  output: "reports/rapport-ecrin-public.pdf"

instruments: auto           # détection automatique depuis REDCap
```

### 2. Visualiser le plan avant d'agir

```bash
uv run ecrin.py preview
```

```
Previewing updates for stack public (audience : public)

  + profils_chercheurs   à télécharger (jamais téléchargé)
  + institutions         à télécharger (jamais téléchargé)

2 actions, 0 instrument en cache
Utilisez `up` pour exécuter.
```

### 3. Exécuter

```bash
uv run ecrin.py up
```

Au prochain run, seuls les instruments modifiés sur REDCap sont re-téléchargés :

```
  ~ profils_chercheurs   à re-télécharger (3 enregistrements modifiés depuis 2026-03-17)
  ✓ institutions         en cache (intègre, inchangé)
```

### Autres commandes

```bash
uv run ecrin.py stack ls        # liste les stacks
uv run ecrin.py state           # état du stack actif et des données REDCap
uv run ecrin.py refresh         # resynchronise le state avec REDCap
uv run ecrin.py log             # historique des up
uv run ecrin.py rollback        # restaure l'état précédent (N-1)
uv run ecrin.py cancel          # annule un up interrompu
uv run ecrin.py destroy         # détruit toutes les ressources du stack actif
```

## CLI R — commandes legacy

Le CLI R reste disponible pour les opérations ponctuelles.

Lancer le menu interactif :

```bash
Rscript ecrin.R
```

| Commande                          | Description                                           |
| --------------------------------- | ----------------------------------------------------- |
| `Rscript ecrin.R metadata`        | Récupère instruments, métadonnées et dictionnaire CSV |
| `Rscript ecrin.R export`          | Pipeline complet des vagues (sans rapport)            |
| `Rscript ecrin.R diffusion`       | Récupère les paramètres de diffusion                  |
| `Rscript ecrin.R rapport profils` | Génère le rapport des profils (PDF)                   |
| `Rscript ecrin.R rapport pdf`     | Compile le rapport Quarto en PDF                      |
| `Rscript ecrin.R rapport html`    | Compile le rapport Quarto en HTML                     |
| `Rscript ecrin.R rapport preview` | Lance le preview interactif Quarto                    |
| `Rscript ecrin.R clean`           | Supprime les fichiers générés                         |

## Rapport ECRIN

Le rapport regroupe les données de **tous les instruments** ayant des préférences de diffusion. Chaque instrument dispose de ses propres champs :

- **`{instrument}_identification_level`** : Identifiable, Pseudonymised, Anonymised, Aggregated only
- **`{instrument}_data_audience`** : General public, Authenticated researchers only

### Instruments gérés

Les instruments sont détectés automatiquement depuis les métadonnées REDCap :

| Instrument             |                      Champs identifiants                      | Fichiers                           |
| ---------------------- | :-----------------------------------------------------------: | ---------------------------------- |
| **Researcher Profile** | Oui (`last_name`, `first_name`, `email`, `orcid`, `portrait`) | `portrait`                         |
| **Research Questions** |                              Non                              | -                                  |
| **Publications**       |                              Non                              | `publications` (PDF)               |
| **Project Proposal**   |                              Non                              | `paper1`, `paper2`, `paper3` (PDF) |

### Matrice de téléchargement des données

| Rapport        | identification_level | audience         | Champs identifiants | Champs non-identifiants | User ID     |
| -------------- | -------------------- | ---------------- | ------------------- | ----------------------- | ----------- |
| **Public**     | Identifiable         | General public   | ✅ Téléchargés      | ✅ Téléchargés          | ✅ Original |
| **Public**     | Identifiable         | Researchers only | ❌ Exclu            | 📊 Statistiques         | ❌ Exclu    |
| **Public**     | Pseudonymised        | General public   | ❌ Non téléchargés  | ✅ Téléchargés          | 🔒 Hashé    |
| **Public**     | Pseudonymised        | Researchers only | ❌ Exclu            | 📊 Statistiques         | ❌ Exclu    |
| **Public**     | Anonymised           | General public   | ❌ Non téléchargés  | ✅ Téléchargés          | ❌ Aucun    |
| **Public**     | Anonymised           | Researchers only | ❌ Exclu            | 📊 Statistiques         | ❌ Exclu    |
| **Public**     | Aggregated only      | \*               | ❌ Non téléchargés  | 📊 Statistiques         | ❌ Aucun    |
| **Chercheurs** | Identifiable         | \*               | ✅ Téléchargés      | ✅ Téléchargés          | ✅ Original |
| **Chercheurs** | Pseudonymised        | \*               | ❌ Non téléchargés  | ✅ Téléchargés          | 🔒 Hashé    |
| **Chercheurs** | Anonymised           | \*               | ❌ Non téléchargés  | ✅ Téléchargés          | ❌ Aucun    |
| **Chercheurs** | Aggregated only      | \*               | ❌ Non téléchargés  | 📊 Statistiques         | ❌ Aucun    |

**Note :** pour les instruments **sans champs identifiants** (Research Questions, Publications, Project Proposal), tous les champs sont traités comme non-identifiants.

**Légende :**

- ✅ Téléchargés : données complètes récupérées depuis REDCap
- ❌ Non téléchargés : champs jamais récupérés (protection des données)
- 📊 Statistiques : seul le comptage est inclus (aucune donnée individuelle)
- 🔒 Hashé : ID transformé en hash SHA256 tronqué (16 caractères)

### Processus de téléchargement en vagues

Pour **chaque instrument**, le téléchargement s'effectue en **5 vagues successives** :

```
Vague 1 : Champs de contrôle
│  Requête : {instrument}_identification_level + {instrument}_data_audience
│  But : classifier chaque participant → identIDs, pseudoIDs, anonIDs, aggregatedIDs
│  Détection de changement (CLI Python) : dateRangeBegin=last_downloaded_at
│
├─ Vague 2 : Identifiables
│  Requête : tous les champs (uniquement identIDs)
│  Export : vague2_{instrument}_identifiables.csv
│
├─ Vague 3 : Pseudonymisés
│  Requête : champs non-identifiants uniquement (uniquement pseudoIDs)
│  Export : vague3_{instrument}_pseudonymises.csv (ID hashé)
│
├─ Vague 4 : Anonymisés
│  Requête : champs non-identifiants uniquement (uniquement anonIDs)
│  Export : vague4_{instrument}_anonymises.csv (aucun ID)
│
└─ Vague 5 : Agrégés
   Comptage seulement
   Export : vague5_{instrument}_statistiques.csv
```

**Filtrage par audience :** pour un rapport **Public**, seuls les participants ayant choisi `General public` sont inclus. Pour un rapport **Chercheurs**, tous les participants sont inclus.

**Principe de sécurité :** les champs identifiants ne sont **jamais demandés** à l'API REDCap pour les vagues 3, 4 et 5.

### Structure des fichiers

```
contexts/
  public.yml                    # déclaration du rapport public
  chercheurs.yml                # déclaration du rapport chercheurs
  public.state.json             # état du contexte (gitignored)

downloads/
  metadata/
    instruments.json
    metadata.json
    dictionnaire.csv
  data/
    vague2_{instrument}_identifiables.csv
    vague3_{instrument}_pseudonymises.csv
    vague4_{instrument}_anonymises.csv
    vague5_{instrument}_statistiques.csv
    fichiers/{instrument}/      # portraits, publications, papers
    .redcap.state.json          # état des données REDCap (gitignored)
    .apply-log.json             # historique des apply (gitignored)
    .backup/                    # snapshot N-1 pour rollback (gitignored)

reports/
  rapport-ecrin-public.qmd
  rapport-ecrin-public.pdf
```

### Gestion du cache (CLI Python)

Le CLI Python maintient deux états distincts :

**`downloads/data/.redcap.state.json`** — état des données REDCap sur disque, indépendant de tout rapport. Contient pour chaque instrument : date du dernier téléchargement, comptages, et SHA-256 de chaque CSV.

**`contexts/{nom}.state.json`** — état du dernier `apply` pour ce contexte. Contient un snapshot de la configuration YAML utilisée.

À chaque `preview` ou `apply`, pour chaque instrument :

1. Vérification de l'intégrité des CSV (SHA-256 vs state) → `✗ corrompu` si différent
2. Appel REDCap avec `dateRangeBegin=last_downloaded_at` → `~ modifié` si des enregistrements ont changé
3. Sinon → `✓ en cache`

### Backup et rollback

Avant chaque téléchargement, `apply` sauvegarde automatiquement les CSV existants et l'état REDCap dans `downloads/data/.backup/` (un seul niveau N-1).

Si un `apply` est interrompu (crash réseau, etc.), `preview` et `apply` affichent un avertissement. Pour restaurer :

```bash
uv run ecrin.py rollback
```
