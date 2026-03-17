# Analyse Première Vague - ECRIN

## Prérequis

- [R](https://www.r-project.org) ≥ 4.1 — `brew install r`
- [Quarto](https://quarto.org) — `brew install --cask quarto`
- Packages R : `httr2`, `jsonlite`, `digest`, `readr`, `dplyr`, `stringr`, `fs`

```r
install.packages(c("httr2", "jsonlite", "digest", "readr", "dplyr", "stringr", "fs"))
```

## Configuration

Créer un fichier `.env` à la racine du projet avec les variables suivantes :

```env
REDCAP_API_URL=https://votre-instance-redcap.fr/api/
REDCAP_TOKEN=votre_token_api
```

## Commandes disponibles

Lancer le menu interactif :

```bash
Rscript ecrin.R
```

### REDCap

| Commande                               | Description                                         |
| -------------------------------------- | --------------------------------------------------- |
| `Rscript ecrin.R metadata`             | Récupère les instruments et variables depuis REDCap |
| `Rscript ecrin.R instruments`          | Affiche la liste des instruments REDCap             |
| `Rscript ecrin.R export`               | Exporte les données REDCap en CSV                   |
| `Rscript ecrin.R diffusion`            | Récupère les paramètres de diffusion                |

Les fichiers sont répartis dans deux dossiers :

**`data/`** - Données téléchargées depuis REDCap :

- `instruments.json` - Liste des instruments
- `metadata.json` - Métadonnées complètes

**`reports/`** - Fichiers générés :

- `variables_recap.csv` - Récapitulatif des variables
- `rapport_variables.txt` - Rapport textuel des variables

### Rapport Quarto

| Commande                               | Description                               |
| -------------------------------------- | ----------------------------------------- |
| `Rscript ecrin.R rapport profils`      | Génère le rapport des profils (avec audience) |
| `Rscript ecrin.R rapport pdf`          | Compile le rapport Quarto en PDF          |
| `Rscript ecrin.R rapport html`         | Compile le rapport Quarto en HTML         |
| `Rscript ecrin.R rapport preview`      | Lance le preview interactif Quarto        |

### Nettoyage

| Commande                  | Description                              |
| ------------------------- | ---------------------------------------- |
| `Rscript ecrin.R clean`   | Supprime les fichiers générés            |

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

Cette matrice s'applique **par instrument** et définit quelles données sont téléchargées depuis REDCap selon :

1. Le choix de l'utilisateur générant le rapport (Public ou Chercheurs)
2. Les préférences du participant (identification_level et audience)

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

**Note :** pour les instruments **sans champs identifiants** (Research Questions, Publications, Project Proposal), tous les champs sont traités comme non-identifiants. La distinction porte uniquement sur la présence ou non du User ID.

**Légende :**

- ✅ Téléchargés : données complètes récupérées depuis REDCap
- ❌ Non téléchargés : champs jamais récupérés (protection des données)
- 📊 Statistiques : seul le comptage est inclus (aucune donnée individuelle)
- 🔒 Hashé : ID transformé en hash SHA256 tronqué (16 caractères)
- ❌ Aucun : pas d'identifiant dans l'export

### Processus de téléchargement en vagues

Ce processus est déclenché par la commande `Rscript ecrin.R rapport profils` (ou via le menu interactif > "Rapport Profils").

Pour **chaque instrument**, le téléchargement s'effectue en **5 vagues successives** :

```
Vague 1 : Champs de contrôle
│  Requête : {instrument}_identification_level + {instrument}_data_audience
│  But : classifier chaque participant → identIDs, pseudoIDs, anonIDs, aggregatedIDs
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
```

**Filtrage par audience :** pour un rapport **Public**, seuls les participants ayant choisi `General public` sont inclus (filtrage appliqué dès la vague 1). Pour un rapport **Chercheurs**, tous les participants sont inclus.

**Principe de sécurité :** les champs identifiants ne sont **jamais demandés** à l'API REDCap pour les vagues 3, 4 et 5.

### Structure des fichiers générés

Les deux répertoires `data/` et `reports/` sont nettoyés avant chaque génération de rapport.

**`data/`** - Données téléchargées depuis REDCap :

| Fichier                                 | Contenu                                                |
| --------------------------------------- | ------------------------------------------------------ |
| `vague2_{instrument}_identifiables.csv` | Tous les champs + ID original                          |
| `vague3_{instrument}_pseudonymises.csv` | Champs non-identifiants + ID hashé                     |
| `vague4_{instrument}_anonymises.csv`    | Champs non-identifiants sans ID                        |
| `fichiers/{instrument}/`                | Fichiers téléchargés (portraits, publications, papers) |

**`reports/`** - Rapport unique :

| Fichier             | Contenu                                       |
| ------------------- | --------------------------------------------- |
| `rapport-ecrin.qmd` | Template Quarto généré (tous les instruments) |
| `rapport-ecrin.pdf` | Rapport compilé en PDF                        |

## Workflow typique

```bash
# 1. Récupérer les métadonnées REDCap
Rscript ecrin.R metadata

# 2. Exporter les données
Rscript ecrin.R export

# 3. Générer le rapport des profils
Rscript ecrin.R rapport profils
```
