# Analyse Première Vague - ECRIN

## Prérequis

- [Homebrew](https://brew.sh) - `/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"`
- [Task](https://taskfile.dev) - `brew install go-task`
- [Go](https://go.dev) - `brew install go`
- [Quarto](https://quarto.org) - `brew install --cask quarto`

## Configuration

Créer un fichier `.env` à la racine du projet avec les variables suivantes :

```env
REDCAP_API_URL=https://votre-instance-redcap.fr/api/
REDCAP_API_TOKEN=votre_token_api
```

## Commandes disponibles

Afficher la liste des tâches :

```bash
task
```

### Setup

| Commande | Description |
|----------|-------------|
| `task setup` | Installe les dépendances (Go) |

### REDCap

| Commande | Description |
|----------|-------------|
| `task redcap:build` | Compile le binaire redcap-metadata |
| `task redcap:metadata` | Récupère les instruments et variables depuis REDCap |
| `task redcap:instruments` | Affiche la liste des instruments REDCap |
| `task redcap:export` | Exporte les données REDCap en CSV |

Les fichiers générés sont placés dans le dossier `redcap_metadata/` :
- `instruments.json` - Liste des instruments
- `metadata.json` - Métadonnées complètes
- `variables_recap.csv` - Récapitulatif des variables
- `rapport_variables.txt` - Rapport textuel des variables

### Rapport Quarto

| Commande | Description |
|----------|-------------|
| `task rapport` | Compile le rapport en PDF |
| `task rapport:html` | Compile le rapport en HTML |
| `task rapport:preview` | Lance le preview interactif Quarto |

### Nettoyage

| Commande | Description |
|----------|-------------|
| `task clean` | Supprime les fichiers générés (PDF, HTML, metadata) |
| `task clean:cache` | Supprime le cache Quarto |

## Workflow typique

```bash
# 1. Installer les dépendances
task setup

# 2. Récupérer les métadonnées REDCap
task redcap:metadata

# 3. Exporter les données
task redcap:export

# 4. Générer le rapport
task rapport
```
