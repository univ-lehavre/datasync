#!/usr/bin/env python
"""Extraction des références bibliographiques depuis un fichier texte brut.
Usage: python tasks/extract_biblio.py --task /path/to/task.json
Sortie stdout JSON: {"ok": true, "n_refs": 42, "refs": [...]}

Task JSON:
  text_path   : chemin vers le fichier .txt extrait (hashed_id.txt)
  hashed_id   : identifiant du document
"""

import argparse
import json
import sys

import refextract


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--task", required=True)
    args = parser.parse_args()

    with open(args.task, encoding="utf-8") as f:
        task = json.load(f)

    for field in ("text_path", "hashed_id"):
        if field not in task:
            print(json.dumps({"ok": False, "error": f"Champ manquant : {field}"}))
            sys.exit(1)

    with open(task["text_path"], encoding="utf-8") as f:
        text = f.read()

    try:
        raw_refs = refextract.extract_references_from_string(text)
    except Exception as e:
        print(json.dumps({"ok": False, "error": str(e)}))
        sys.exit(1)

    refs = []
    for r in raw_refs:
        # refextract retourne des listes pour chaque champ
        def first(key):
            vals = r.get(key, [])
            return vals[0] if vals else None

        refs.append({
            "hashed_id":  task["hashed_id"],
            "raw":        first("raw_ref"),
            "title":      first("title"),
            "author":     first("author"),
            "year":       first("year"),
            "journal":    first("journal"),
            "doi":        first("doi"),
            "reportnum":  first("reportnum"),
        })

    print(json.dumps({"ok": True, "n_refs": len(refs), "refs": refs}, ensure_ascii=False))


if __name__ == "__main__":
    main()
