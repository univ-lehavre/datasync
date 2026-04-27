# Contribuer au projet

## Prérequis

- R ≥ 4.1
- Packages de développement :

```r
install.packages(c("styler", "lintr"))
```

## Structure du code

```
ecrin.R          # Point d'entrée : sources, commandes CLI, menu
R/
  config.R       # Imports, constantes, couleurs console, load_env(), get_config()
  api_redcap.R   # Communication avec l'API REDCap
  instruments.R  # Détection et modélisation des instruments
  download.R     # Pipeline de téléchargement par niveaux d'identification
  rapport.R      # Génération des rapports (CSV, texte, Quarto QMD)
  display.R      # Affichage console (tableaux, statistiques)
```

Règle de sourcing : tous les `source()` sont centralisés dans `ecrin.R`. Les fichiers `R/*.R` ne se sourcent pas entre eux.

## Pre-commit hook

Un hook git est installé dans `.git/hooks/pre-commit`. Il s'exécute automatiquement à chaque commit sur les fichiers `.R` stagés :

1. **styler** — reformate le code et re-stage les fichiers modifiés
2. **lintr** — bloque le commit si des problèmes de style subsistent

Si le commit est bloqué par lintr, corriger les erreurs signalées puis relancer `git commit`.

## Style de code

La configuration lintr est dans [`.lintr`](.lintr) :

- Longueur de ligne max : **120 caractères**
- Nommage : **snake_case** pour les fonctions et variables, **SCREAMING_SNAKE_CASE** pour les constantes globales (`DATA_DIR`, `REPORTS_DIR`, `RAPPORT_QMD`)
- Longueur des noms d'objets : max **40 caractères**

Pour reformater et vérifier manuellement, utiliser [lint.R](lint.R) :

```bash
Rscript lint.R        # lint uniquement
Rscript lint.R fix    # styler puis lint
```

## Ajouter une commande CLI

1. Écrire la fonction `cmd_ma_commande()` dans `ecrin.R`
2. L'ajouter dans le `switch(cmd, ...)` de `main()`
3. L'ajouter dans la liste `items` de `run_interactive_menu()`
4. Mettre à jour le commentaire d'usage en tête de `ecrin.R`

## Ajouter un module

1. Créer `R/mon_module.R` avec les fonctions concernées
2. Ajouter `source(file.path(script_dir, "R/mon_module.R"))` dans `ecrin.R`, en respectant l'ordre des dépendances
3. Ne pas mettre de `source()` dans le module lui-même

## Variables d'environnement

Le fichier `.env` (non versionné) est chargé par `load_env()` au démarrage. Variables requises :

```env
REDCAP_API_URL=https://votre-instance-redcap.fr/api/
REDCAP_TOKEN=votre_token_api
```

Ne jamais committer de token ou d'URL d'instance REDCap dans le dépôt.
