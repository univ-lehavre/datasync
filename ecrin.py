#!/usr/bin/env -S uv run python
"""CLI ECRIN — orchestrateur déclaratif style Pulumi.

Usage:
  ecrin.py new <nom>            # Crée un nouveau stack et l'active
  ecrin.py stack init <nom>     # Crée un nouveau stack (sans l'activer)
  ecrin.py stack select <nom>   # Active un stack
  ecrin.py stack ls             # Liste les stacks
  ecrin.py preview              # Affiche le plan sans exécuter
  ecrin.py up                   # Télécharge et maintient les données du stack
  ecrin.py refresh              # Resynchronise le state avec REDCap
  ecrin.py cancel               # Annule un up interrompu
  ecrin.py rollback             # Restaure l'état précédent (N-1)
  ecrin.py log                  # Affiche l'historique des up
  ecrin.py destroy              # Détruit toutes les ressources du stack actif
  ecrin.py state                # Affiche l'état courant
"""

from __future__ import annotations

import hashlib
import json
import os
import re
import shutil
import subprocess
import tempfile
from datetime import datetime, timezone
from pathlib import Path

import typer
import yaml
from rich.console import Console
from rich.table import Table

# ---------------------------------------------------------------------------
# Constantes
# ---------------------------------------------------------------------------
STACKS_DIR = Path("stacks")
ECRIN_DIR = Path(".ecrin")
ACTIVE_STACK_FILE = ECRIN_DIR / "active-stack"
SCHEMA_VERSION = 1

app = typer.Typer(help="Orchestrateur déclaratif ECRIN", no_args_is_help=True)
stack_app = typer.Typer(help="Gestion des stacks", no_args_is_help=True)
app.add_typer(stack_app, name="stack")

console = Console()


# ---------------------------------------------------------------------------
# Helpers — chemins par stack
# ---------------------------------------------------------------------------


def stack_root_dir(name: str) -> Path:
    return ECRIN_DIR / name


def stack_downloads_dir(name: str) -> Path:
    return stack_root_dir(name) / "downloads"


def stack_instrument_dir(stack_name: str, instrument_name: str) -> Path:
    return stack_downloads_dir(stack_name) / instrument_name


def stack_metadata_dir(name: str) -> Path:
    return stack_downloads_dir(name) / "metadata"


def redcap_state_file(name: str) -> Path:
    return stack_root_dir(name) / ".redcap.state.json"


def backup_dir(name: str) -> Path:
    return stack_root_dir(name) / ".backup"


def up_in_progress_file(name: str) -> Path:
    return stack_root_dir(name) / ".up-in-progress.json"


def up_log_file(name: str) -> Path:
    return stack_root_dir(name) / ".up-log.json"


# ---------------------------------------------------------------------------
# Helpers — fichiers
# ---------------------------------------------------------------------------


def load_json(path: Path) -> dict:
    if not path.exists():
        return {}
    with path.open() as f:
        return json.load(f)


def save_json(path: Path, data: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(".tmp")
    with tmp.open("w") as f:
        json.dump(data, f, indent=2, ensure_ascii=False)
    tmp.replace(path)


def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(65536), b""):
            h.update(chunk)
    return f"sha256:{h.hexdigest()}"


# ---------------------------------------------------------------------------
# Helpers — stack actif
# ---------------------------------------------------------------------------


def get_active_stack() -> str | None:
    if ACTIVE_STACK_FILE.exists():
        name = ACTIVE_STACK_FILE.read_text().strip()
        return name if name else None
    return None


def require_active_stack() -> str:
    name = get_active_stack()
    if not name:
        console.print("[red]Aucun stack actif. Utilisez `stack select <nom>`.[/red]")
        raise typer.Exit(1)
    yml_path = STACKS_DIR / f"{name}.yml"
    if not yml_path.exists():
        console.print(f"[red]Fichier de stack introuvable : {yml_path}[/red]")
        raise typer.Exit(1)
    return name


def load_stack_config(name: str) -> dict:
    yml_path = STACKS_DIR / f"{name}.yml"
    with yml_path.open() as f:
        return yaml.safe_load(f)


def stack_state_path(name: str) -> Path:
    return stack_root_dir(name) / "state.json"


# ---------------------------------------------------------------------------
# Helpers — appels R
# ---------------------------------------------------------------------------


def r_tasks_dir() -> Path:
    return Path(__file__).parent / "tasks"


def run_r_task(script: str, task: dict) -> dict:
    """Exécute un script tasks/ avec un JSON de tâche, retourne le JSON de sortie."""
    script_path = r_tasks_dir() / script
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".json", delete=False, encoding="utf-8"
    ) as tmp:
        json.dump(task, tmp, ensure_ascii=False)
        tmp_path = tmp.name

    try:
        result = subprocess.run(
            ["Rscript", str(script_path), "--task", tmp_path],
            capture_output=True,
            text=True,
        )
        if result.returncode != 0:
            stderr = result.stderr
            if "<!DOCTYPE" in stderr or "<html" in stderr.lower() or "invalid char in json" in stderr:
                console.print(
                    "[red]L'API REDCap a renvoyé une page HTML au lieu de JSON.[/red]\n"
                    "  Vérifiez que vous êtes connecté au VPN."
                )
            else:
                console.print(f"[red]Erreur R ({script}):[/red]\n{stderr}")
            raise typer.Exit(1)
        stdout = result.stdout.strip()
        match = re.search(r"\{[^{}]*(?:\{[^{}]*\}[^{}]*)?\}", stdout, re.DOTALL)
        if not match:
            match_all = re.findall(r"(\{.*\})", stdout, re.DOTALL)
            if match_all:
                return json.loads(match_all[-1])
            console.print(f"[red]Sortie R invalide ({script}):\n{stdout}[/red]")
            raise typer.Exit(1)
        return json.loads(match.group())
    finally:
        os.unlink(tmp_path)


def get_api_config() -> dict:
    """Lit REDCAP_API_URL et REDCAP_TOKEN depuis l'environnement ou le .env."""
    env_file = Path(".env")
    env: dict[str, str] = {}
    if env_file.exists():
        for line in env_file.read_text().splitlines():
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            if "=" in line:
                k, _, v = line.partition("=")
                env[k.strip()] = v.strip().strip("\"'")
    api_url = os.environ.get("REDCAP_API_URL") or env.get("REDCAP_API_URL", "")
    token = os.environ.get("REDCAP_TOKEN") or env.get("REDCAP_TOKEN", "")
    if not api_url or not token:
        console.print("[red]REDCAP_API_URL ou REDCAP_TOKEN non défini.[/red]")
        raise typer.Exit(1)
    return {"api_url": api_url, "token": token}


# ---------------------------------------------------------------------------
# Helpers — état REDCap (par stack)
# ---------------------------------------------------------------------------


def load_redcap_state(stack_name: str) -> dict:
    return load_json(redcap_state_file(stack_name))


def save_redcap_state(stack_name: str, state: dict) -> None:
    save_json(redcap_state_file(stack_name), state)


def instrument_state_key(instrument_name: str, audience: str) -> str:
    return f"{instrument_name}__{audience}"


def compute_expected_csv_files(instrument_name: str, data_dir: Path) -> list[Path]:
    return [
        data_dir / "identifiables.csv",
        data_dir / "pseudonymises.csv",
        data_dir / "anonymises.csv",
        data_dir / "statistiques.csv",
    ]


def make_up_id() -> str:
    """Génère un identifiant unique pour un up : YYYYMMDD-HHMMSS-<hash6>."""
    now = datetime.now(timezone.utc)
    ts = now.strftime("%Y%m%d-%H%M%S")
    h = hashlib.sha256(ts.encode()).hexdigest()[:6]
    return f"{ts}-{h}"


def backup_current_state(stack_name: str, up_id: str, instruments: list[str]) -> None:
    """Copie les CSV existants et le state REDCap dans .backup/ avant écrasement."""
    bkp_dir = backup_dir(stack_name)
    bkp_dir.mkdir(parents=True, exist_ok=True)

    for inst_name in instruments:
        inst_data_dir = stack_instrument_dir(stack_name, inst_name)
        inst_bkp_dir = bkp_dir / inst_name
        inst_bkp_dir.mkdir(parents=True, exist_ok=True)
        for csv_path in compute_expected_csv_files(inst_name, inst_data_dir):
            if csv_path.exists():
                shutil.copy2(csv_path, inst_bkp_dir / csv_path.name)

    state_file = redcap_state_file(stack_name)
    if state_file.exists():
        shutil.copy2(state_file, bkp_dir / ".redcap.state.json")

    save_json(bkp_dir / ".backup-meta.json", {
        "backed_up_at": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S"),
        "up_id": up_id,
        "instruments": instruments,
    })


def restore_backup(stack_name: str) -> bool:
    """Restaure les CSV et le state depuis .backup/. Retourne True si succès."""
    bkp_dir = backup_dir(stack_name)
    meta_path = bkp_dir / ".backup-meta.json"
    if not meta_path.exists():
        return False

    meta = load_json(meta_path)
    instruments = meta.get("instruments", [])

    for inst_name in instruments:
        inst_data_dir = stack_instrument_dir(stack_name, inst_name)
        inst_data_dir.mkdir(parents=True, exist_ok=True)
        inst_bkp_dir = bkp_dir / inst_name
        for csv_path in compute_expected_csv_files(inst_name, inst_data_dir):
            backup_csv = inst_bkp_dir / csv_path.name
            if backup_csv.exists():
                shutil.copy2(backup_csv, csv_path)
            elif csv_path.exists():
                csv_path.unlink()

    backup_state = bkp_dir / ".redcap.state.json"
    state_file = redcap_state_file(stack_name)
    if backup_state.exists():
        shutil.copy2(backup_state, state_file)
    elif state_file.exists():
        state_file.unlink()

    return True


def check_interrupted_up(stack_name: str) -> dict | None:
    """Retourne les métadonnées du up interrompu, ou None."""
    flag = up_in_progress_file(stack_name)
    if flag.exists():
        return load_json(flag)
    return None


def append_up_log(stack_name: str, entry: dict) -> None:
    """Ajoute une entrée au log de up (append-only)."""
    log_path = up_log_file(stack_name)
    log_path.parent.mkdir(parents=True, exist_ok=True)
    log: list = []
    if log_path.exists():
        with log_path.open() as f:
            log = json.load(f)
    log.append(entry)
    save_json(log_path, log)


# ---------------------------------------------------------------------------
# Calcul du diff (cœur de preview/up)
# ---------------------------------------------------------------------------


def compute_diff(cfg: dict, api_cfg: dict, stack_name: str, audience: str) -> list[dict]:
    """Retourne une liste d'actions par instrument.

    Chaque entrée : { "instrument": {...}, "action": "ok"|"new"|"modified"|"corrupted", "reason": "..." }
    """
    redcap_state = load_redcap_state(stack_name)
    meta_dir = stack_metadata_dir(stack_name)

    metadata_path = meta_dir / "metadata.json"
    instruments_path = meta_dir / "instruments.json"

    # Toujours télécharger les métadonnées, mais ne signaler le changement que si le SHA-256 diffère
    dict_path = meta_dir / "dictionnaire.csv"
    old_hashes = {
        "metadata.json": redcap_state.get("_metadata", {}).get("metadata.json", ""),
        "instruments.json": redcap_state.get("_metadata", {}).get("instruments.json", ""),
        "dictionnaire.csv": redcap_state.get("_metadata", {}).get("dictionnaire.csv", ""),
    }
    run_r_task("fetch_metadata.R", {**api_cfg, "metadata_dir": str(meta_dir)})
    new_hashes = {
        "metadata.json": sha256_file(metadata_path) if metadata_path.exists() else "",
        "instruments.json": sha256_file(instruments_path) if instruments_path.exists() else "",
        "dictionnaire.csv": sha256_file(dict_path) if dict_path.exists() else "",
    }
    if new_hashes != old_hashes:
        redcap_state["_metadata"] = new_hashes
        save_redcap_state(stack_name, redcap_state)
        if any(old_hashes.values()):
            console.print("  [yellow]~[/yellow] Métadonnées REDCap modifiées — structure mise à jour")
    with instruments_path.open() as f:
        all_instruments = json.load(f)
    with metadata_path.open() as f:
        metadata_raw = json.load(f)

    instrument_filter = cfg.get("instruments", "auto")

    id_level_fields = {
        row["form_name"]: row["field_name"]
        for row in metadata_raw
        if row["field_name"].endswith("_identification_level")
    }
    audience_fields = {
        row["form_name"]: row["field_name"]
        for row in metadata_raw
        if row["field_name"].endswith("_data_audience")
    }
    id_field = metadata_raw[0]["field_name"] if metadata_raw else "record_id"

    detected = []
    for inst in all_instruments:
        name = inst["instrument_name"]
        if name not in id_level_fields:
            continue
        if instrument_filter != "auto" and name not in instrument_filter:
            continue
        has_identifiers = any(
            row.get("identifier") == "y" and row["form_name"] == name
            for row in metadata_raw
        )
        file_fields = [
            row["field_name"]
            for row in metadata_raw
            if row["form_name"] == name and row["field_type"] == "file"
        ]
        detected.append({
            "name": name,
            "label": inst["instrument_label"],
            "id_level_field": id_level_fields[name],
            "audience_field": audience_fields.get(name, ""),
            "has_identifiers": has_identifiers,
            "file_fields": file_fields,
        })

    diff = []
    for inst in detected:
        key = instrument_state_key(inst["name"], audience)
        inst_state = redcap_state.get(key)
        inst_data_dir = stack_instrument_dir(stack_name, inst["name"])
        expected_csvs = compute_expected_csv_files(inst["name"], inst_data_dir)

        # 1. Vérifier intégrité des fichiers existants (CSV + binaires)
        if inst_state and inst_state.get("files"):
            corrupted_files = []
            for rel, expected_hash in inst_state["files"].items():
                if rel.startswith("files/"):
                    full_path = inst_data_dir / rel
                else:
                    full_path = inst_data_dir / rel
                if full_path.exists():
                    if sha256_file(full_path) != expected_hash:
                        corrupted_files.append(rel)
                else:
                    corrupted_files.append(f"{rel} (manquant)")
            if corrupted_files:
                diff.append({
                    "instrument": inst,
                    "action": "corrupted",
                    "reason": f"fichiers corrompus : {', '.join(corrupted_files)}",
                })
                continue

        # 2. Jamais téléchargé
        if not inst_state:
            diff.append({"instrument": inst, "action": "new", "reason": "jamais téléchargé"})
            continue

        # 3. Changements REDCap via dateRangeBegin
        downloaded_at = inst_state.get("downloaded_at", "")
        # REDCap attend "YYYY-MM-DD HH:MM:SS" (espace, pas T)
        date_range_begin = downloaded_at.replace("T", " ") if downloaded_at else ""
        task = {
            **api_cfg,
            "id_field": id_field,
            "id_level_field": inst["id_level_field"],
            "audience_field": inst["audience_field"],
            "date_range_begin": date_range_begin,
        }
        result = run_r_task("fetch_control.R", task)
        if result.get("has_changes"):
            changed_ids = result.get("changed_record_ids", [])
            n_changed = len(changed_ids)
            n_total = result.get("n_total", "?")
            diff.append({
                "instrument": inst,
                "action": "modified",
                "reason": (
                    f"{n_changed}/{n_total} enregistrement(s) modifié(s) depuis {downloaded_at[:10]}"
                    if n_changed > 0
                    else f"nouveaux enregistrements détectés (total : {n_total}) depuis {downloaded_at[:10]}"
                ),
                "changed_record_ids": changed_ids,
            })
        else:
            n_total = result.get("n_total", "?")
            diff.append({
                "instrument": inst,
                "action": "ok",
                "reason": f"en cache — {n_total} enregistrement(s), aucun changement depuis {downloaded_at[:10]}",
            })

    return diff


# ---------------------------------------------------------------------------
# Commande new
# ---------------------------------------------------------------------------

NEW_STACK_TEMPLATE = """\
# Stack ECRIN — {date}

# Audience cible
# "public"      → participants ayant choisi "General public"
# "chercheurs"  → tous les participants (filtre audience respecté)
# "admin"       → tous les participants sans filtre (toutes les données)
audience: "{audience}"

# Instruments à inclure
# "auto"  → détection automatique depuis REDCap
#           (tous les instruments ayant {{instrument}}_identification_level)
# liste   → sélection manuelle par nom technique REDCap
instruments: auto
# instruments:
#   - profils_chercheurs
#   - questions_recherche
#   - publications
#   - propositions_projets

# Champs textuels à analyser par NLP (instrument: champ)
# Exécuté automatiquement lors d'un `up` et via la commande `nlp`
# nlp_fields:
#   - instrument: research_questions
#     field: research_questions
"""


@app.command()
def new(name: str = typer.Argument(..., help="Nom du stack")) -> None:
    """Crée un nouveau stack et l'active (équivalent de stack init + stack select)."""
    STACKS_DIR.mkdir(parents=True, exist_ok=True)
    yml_path = STACKS_DIR / f"{name}.yml"
    if yml_path.exists():
        console.print(f"[yellow]Le stack '{name}' existe déjà.[/yellow]")
        raise typer.Exit(1)

    content = NEW_STACK_TEMPLATE.format(
        date=datetime.now(timezone.utc).strftime("%Y-%m-%d"),
        audience="public",
    )
    yml_path.write_text(content, encoding="utf-8")

    ECRIN_DIR.mkdir(parents=True, exist_ok=True)
    ACTIVE_STACK_FILE.write_text(name)

    console.print(f"[green]✓[/green] Stack [cyan]{name}[/cyan] créé et activé : {yml_path}")
    console.print(f"  Éditez [cyan]{yml_path}[/cyan] puis lancez : [cyan]ecrin.py preview[/cyan]")


# ---------------------------------------------------------------------------
# Commandes stack
# ---------------------------------------------------------------------------


@stack_app.command("init")
def stack_init(name: str = typer.Argument(..., help="Nom du stack")) -> None:
    """Crée un nouveau stack avec un fichier YAML d'exemple."""
    STACKS_DIR.mkdir(parents=True, exist_ok=True)
    yml_path = STACKS_DIR / f"{name}.yml"
    if yml_path.exists():
        console.print(f"[yellow]Le stack '{name}' existe déjà.[/yellow]")
        raise typer.Exit(1)

    content = NEW_STACK_TEMPLATE.format(
        date=datetime.now(timezone.utc).strftime("%Y-%m-%d"),
        audience="public",
    )
    yml_path.write_text(content, encoding="utf-8")

    console.print(f"[green]✓[/green] Stack '{name}' créé : {yml_path}")
    console.print(
        f"  Éditez {yml_path} puis lancez : [cyan]ecrin.py stack select {name}[/cyan]"
    )


@stack_app.command("select")
def stack_select(name: str = typer.Argument(..., help="Nom du stack à activer")) -> None:
    """Active un stack existant."""
    yml_path = STACKS_DIR / f"{name}.yml"
    if not yml_path.exists():
        console.print(f"[red]Stack '{name}' introuvable ({yml_path}).[/red]")
        raise typer.Exit(1)
    ECRIN_DIR.mkdir(parents=True, exist_ok=True)
    ACTIVE_STACK_FILE.write_text(name)
    console.print(f"[green]✓[/green] Stack actif : [cyan]{name}[/cyan]")


@stack_app.command("ls")
def stack_ls() -> None:
    """Liste tous les stacks disponibles."""
    if not STACKS_DIR.exists():
        console.print("Aucun stack. Créez-en un avec [cyan]stack init <nom>[/cyan].")
        return

    active = get_active_stack()
    table = Table(show_header=True, header_style="bold")
    table.add_column("", width=2)
    table.add_column("Stack")
    table.add_column("Audience")
    table.add_column("Dernier up")
    table.add_column("État")

    for yml_path in sorted(STACKS_DIR.glob("*.yml")):
        name = yml_path.stem
        marker = "[cyan]*[/cyan]" if name == active else " "
        try:
            cfg = load_stack_config(name)
            audience = cfg.get("audience", "?")
        except Exception:  # noqa: BLE001
            audience = "?"
        st = load_json(stack_state_path(name))
        last_up = st.get("last_up", "jamais")[:10] if st else "jamais"
        status = "[green]✓ à jour[/green]" if st else "[yellow]+ nouveau[/yellow]"
        table.add_row(marker, name, audience, last_up, status)

    console.print(table)


# ---------------------------------------------------------------------------
# Commande preview
# ---------------------------------------------------------------------------


@app.command()
def preview() -> None:
    """Calcule et affiche le plan sans rien exécuter."""
    name = require_active_stack()
    cfg = load_stack_config(name)
    api_cfg = get_api_config()

    audience = cfg.get("audience", "public")

    interrupted = check_interrupted_up(name)
    if interrupted:
        console.print(
            f"[yellow]⚠ Le up {interrupted.get('up_id', '?')} a été interrompu "
            f"({interrupted.get('started_at', '?')[:19]}).[/yellow]\n"
            "  L'état des données est potentiellement incohérent.\n"
            "  Utilisez [cyan]ecrin.py rollback[/cyan] pour restaurer l'état précédent,\n"
            "  ou [cyan]ecrin.py cancel[/cyan] pour annuler proprement,\n"
            "  ou [cyan]ecrin.py up[/cyan] pour réessayer.\n"
        )

    console.print(f"\n[bold]Previewing updates for stack[/bold] [cyan]{name}[/cyan] "
                  f"(audience : {audience})\n")

    diff = compute_diff(cfg, api_cfg, name, audience)

    if not diff:
        console.print("  [yellow]Aucun instrument détecté.[/yellow]")
        return

    nlp_fields = cfg.get("nlp_fields") or []
    redcap_state = load_redcap_state(name)
    nlp_missing: list[str] = []
    for entry in nlp_fields:
        inst_dir = stack_instrument_dir(name, entry["instrument"])
        field = entry["field"]
        nlp_dir = inst_dir / f"nlp-{field}"
        key = instrument_state_key(entry["instrument"], audience)
        inst_files = redcap_state.get(key, {}).get("files", {})
        has_sha = any(k.startswith(f"nlp-{field}/") for k in inst_files)
        if not nlp_dir.exists() or not has_sha:
            nlp_missing.append(f"{entry['instrument']}/{field}")

    n_actions = sum(1 for d in diff if d["action"] != "ok")
    n_ok = sum(1 for d in diff if d["action"] == "ok")

    for d in diff:
        inst = d["instrument"]
        action = d["action"]
        reason = d["reason"]
        if action == "ok":
            inst_key = instrument_state_key(inst["name"], audience)
            inst_files = redcap_state.get(inst_key, {}).get("files", {})
            inst_nlp_missing = [
                e["field"] for e in nlp_fields
                if e["instrument"] == inst["name"]
                and (
                    not (stack_instrument_dir(name, inst["name"]) / f"nlp-{e['field']}").exists()
                    or not any(k.startswith(f"nlp-{e['field']}/") for k in inst_files)
                )
            ]
            nlp_note = f" [yellow](NLP manquant : {', '.join(inst_nlp_missing)})[/yellow]" if inst_nlp_missing else ""
            console.print(f"  [green]✓[/green] {inst['label']:<35} {reason}{nlp_note}")
        elif action == "new":
            console.print(f"  [blue]+[/blue] {inst['label']:<35} à télécharger ({reason})")
        elif action == "modified":
            console.print(f"  [yellow]~[/yellow] {inst['label']:<35} à re-télécharger ({reason})")
        elif action == "corrupted":
            console.print(f"  [red]✗[/red] {inst['label']:<35} corrompu — {reason}")

    console.print()
    if n_actions == 0 and not nlp_missing:
        console.print("[green]Tout est à jour. Aucune action nécessaire.[/green]")
    elif n_actions == 0 and nlp_missing:
        console.print(
            f"[yellow]NLP manquant pour : {', '.join(nlp_missing)}[/yellow]\n"
            f"Utilisez [cyan]ecrin.py up[/cyan] pour générer."
        )
    else:
        console.print(
            f"{n_actions} action(s), {n_ok} instrument(s) en cache\n"
            f"Utilisez [cyan]ecrin.py up[/cyan] pour exécuter."
        )


# ---------------------------------------------------------------------------
# Commande up
# ---------------------------------------------------------------------------


@app.command()
def up() -> None:
    """Télécharge et maintient les données du stack actif."""
    name = require_active_stack()
    cfg = load_stack_config(name)
    api_cfg = get_api_config()

    audience = cfg.get("audience", "public")

    interrupted = check_interrupted_up(name)
    if interrupted:
        console.print(
            f"[yellow]⚠ Le up {interrupted.get('up_id', '?')} a été interrompu "
            f"({interrupted.get('started_at', '?')[:19]}).[/yellow]\n"
            "  Réessai en cours — le backup existant sera conservé.\n"
        )

    console.print(f"\n[bold]Updating stack[/bold] [cyan]{name}[/cyan] (audience : {audience})\n")

    diff = compute_diff(cfg, api_cfg, name, audience)
    metadata_path = stack_metadata_dir(name) / "metadata.json"
    metadata = load_json(metadata_path)

    if not diff:
        console.print("[yellow]Aucun instrument détecté.[/yellow]")
        raise typer.Exit(1)

    redcap_state = load_redcap_state(name)
    now = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M:%S")
    up_id = make_up_id()

    instruments_to_download = [
        d["instrument"]["name"] for d in diff if d["action"] != "ok"
    ]
    if instruments_to_download and not interrupted:
        backup_current_state(name, up_id, instruments_to_download)

    save_json(up_in_progress_file(name), {
        "up_id": up_id,
        "started_at": now,
        "stack": name,
        "instruments_to_download": instruments_to_download,
    })

    log_actions: list[dict] = []

    for d in diff:
        inst = d["instrument"]
        action = d["action"]
        key = instrument_state_key(inst["name"], audience)

        if action == "ok":
            console.print(f"  [green]✓[/green] {inst['label']:<35} en cache")
            log_actions.append({"instrument": inst["name"], "status": "cached"})
            continue

        if action == "new":
            symbol = "[blue]+[/blue]"
            reason_str = d["reason"]
        elif action == "modified":
            symbol = "[yellow]~[/yellow]"
            reason_str = d["reason"]
        else:
            symbol = "[red]✗[/red]"
            reason_str = d["reason"]
        console.print(f"  {symbol} {inst['label']:<35} {reason_str}")
        console.print(f"    téléchargement en cours...")

        inst_data_dir = stack_instrument_dir(name, inst["name"])
        inst_data_dir.mkdir(parents=True, exist_ok=True)

        for nlp_dir in inst_data_dir.glob("nlp-*/"):
            shutil.rmtree(nlp_dir)

        dict_csv_path = stack_metadata_dir(name) / "dictionnaire.csv"
        task = {
            **api_cfg,
            "audience": audience,
            "instrument": inst,
            "metadata_path": str(metadata_path),
            "dict_path": str(dict_csv_path),
            "data_dir": str(inst_data_dir),
        }
        dl_result = run_r_task("download_instrument.R", task)

        if inst.get("file_fields") and dl_result.get("n_ident", 0) > 0:
            ident_csv = inst_data_dir / "identifiables.csv"
            if ident_csv.exists():
                file_task = {
                    **api_cfg,
                    "ident_csv_path": str(ident_csv),
                    "instrument": inst,
                    "metadata_path": str(metadata_path),
                    "data_dir": str(inst_data_dir),
                    "files_dir": str(inst_data_dir / "files"),
                }
                file_result = run_r_task("download_files.R", file_task)
                console.print(
                    f"    [green]✓[/green] {file_result.get('n_files', 0)} fichier(s) téléchargé(s)"
                )

        # Vérification SHA-256 — CSV
        file_hashes = {
            csv_path.name: sha256_file(csv_path)
            for csv_path in compute_expected_csv_files(inst["name"], inst_data_dir)
            if csv_path.exists()
        }
        # Vérification SHA-256 — fichiers binaires
        bin_dir = inst_data_dir / "files"
        if bin_dir.exists():
            for bin_path in sorted(bin_dir.iterdir()):
                if bin_path.is_file():
                    file_hashes[f"files/{bin_path.name}"] = sha256_file(bin_path)

        redcap_state[key] = {
            "downloaded_at": now,
            "counts": {
                "n_ident": dl_result.get("n_ident", 0),
                "n_pseudo": dl_result.get("n_pseudo", 0),
                "n_anon": dl_result.get("n_anon", 0),
                "n_stats_no_response": dl_result.get("n_stats_no_response", 0),
                "n_stats_audience_filtered": dl_result.get("n_stats_audience_filtered", 0),
                "n_stats_aggregated": dl_result.get("n_stats_aggregated", 0),
            },
            "files": file_hashes,
        }
        save_redcap_state(name, redcap_state)

        console.print(
            f"    [green]✓[/green] {dl_result.get('n_ident', 0)} ident, "
            f"{dl_result.get('n_pseudo', 0)} pseudo, "
            f"{dl_result.get('n_anon', 0)} anon"
        )
        if file_hashes:
            console.print(f"    [green]✓[/green] {len(file_hashes)} CSV vérifié(s) (SHA-256)")

        nlp_entries = [
            e for e in (cfg.get("nlp_fields") or [])
            if e["instrument"] == inst["name"]
        ]
        anon_csv = inst_data_dir / "anonymises.csv"
        if nlp_entries and anon_csv.exists():
            for entry in nlp_entries:
                field = entry["field"]
                output_dir = inst_data_dir / f"nlp-{field}"
                output_dir.mkdir(parents=True, exist_ok=True)
                nlp_task = {
                    "csv_path": str(anon_csv),
                    "field": field,
                    "id_field": metadata[0]["field_name"] if metadata else "record_id",
                    "output_dir": str(output_dir),
                }
                nlp_result = run_r_task("nlp_text.R", nlp_task)
                langues = nlp_result.get("langues", {})
                lda_k = nlp_result.get("lda_k", {})
                lang_summary = ", ".join(
                    f"{lang}:{n}(k={lda_k[lang]})" if lang in lda_k else f"{lang}:{n}"
                    for lang, n in langues.items()
                )
                nlp_hashes = {
                    f"nlp-{field}/{f.name}": sha256_file(f)
                    for f in output_dir.iterdir()
                    if f.is_file()
                }
                redcap_state[key]["files"].update(nlp_hashes)
                save_redcap_state(name, redcap_state)
                console.print(f"    [green]✓[/green] NLP {field} — {lang_summary or 'vide'}")

        log_actions.append({"instrument": inst["name"], "status": action})

    instruments_used = [d["instrument"]["name"] for d in diff]
    save_json(stack_state_path(name), {
        "schema_version": SCHEMA_VERSION,
        "stack": name,
        "last_up": now,
        "up_id": up_id,
        "instruments_used": instruments_used,
        "config_snapshot": {
            "audience": audience,
            "instruments": cfg.get("instruments", "auto"),
        },
    })

    up_in_progress_file(name).unlink(missing_ok=True)

    append_up_log(name, {
        "up_id": up_id,
        "updated_at": now,
        "stack": name,
        "audience": audience,
        "instruments": instruments_used,
        "actions": log_actions,
        "outcome": "success",
    })

    console.print(f"\n[green]✓[/green] [bold]Stack[/bold] [cyan]{name}[/cyan] [bold]à jour.[/bold]")
    console.print(f"  Données dans : [cyan]{stack_downloads_dir(name)}[/cyan]\n")


# ---------------------------------------------------------------------------
# Commande refresh
# ---------------------------------------------------------------------------


@app.command()
def refresh() -> None:
    """Resynchronise le state avec REDCap (re-télécharge les métadonnées, recalcule les SHA-256)."""
    name = require_active_stack()
    cfg = load_stack_config(name)
    api_cfg = get_api_config()
    audience = cfg.get("audience", "public")

    console.print(f"\n[bold]Refreshing stack[/bold] [cyan]{name}[/cyan]\n")

    meta_dir = stack_metadata_dir(name)
    console.print("  Récupération des métadonnées REDCap...")
    run_r_task("fetch_metadata.R", {**api_cfg, "metadata_dir": str(meta_dir)})
    console.print("  [green]✓[/green] Métadonnées mises à jour")

    redcap_state = load_redcap_state(name)
    meta_files = ["metadata.json", "instruments.json", "dictionnaire.csv"]
    redcap_state["_metadata"] = {
        f: sha256_file(meta_dir / f) for f in meta_files if (meta_dir / f).exists()
    }
    save_redcap_state(name, redcap_state)
    updated = 0
    for key, inst_state in redcap_state.items():
        if not key.endswith(f"__{audience}"):
            continue
        inst_name = key.replace(f"__{audience}", "")
        inst_dir = stack_instrument_dir(name, inst_name)
        for nlp_dir in inst_dir.glob("nlp-*/"):
            shutil.rmtree(nlp_dir)
        file_hashes = {
            csv_path.name: sha256_file(csv_path)
            for csv_path in compute_expected_csv_files(inst_name, inst_dir)
            if csv_path.exists()
        }
        bin_dir = inst_dir / "files"
        if bin_dir.exists():
            for bin_path in sorted(bin_dir.iterdir()):
                if bin_path.is_file():
                    file_hashes[f"files/{bin_path.name}"] = sha256_file(bin_path)
        if file_hashes != inst_state.get("files", {}):
            redcap_state[key]["files"] = file_hashes
            updated += 1
            console.print(f"  [yellow]~[/yellow] {inst_name} — SHA-256 mis à jour")
        else:
            console.print(f"  [green]✓[/green] {inst_name} — intègre")

    save_redcap_state(name, redcap_state)
    console.print(f"\n[green]✓[/green] Refresh terminé ({updated} fichier(s) mis à jour).\n")


# ---------------------------------------------------------------------------
# Commande cancel
# ---------------------------------------------------------------------------


@app.command()
def cancel() -> None:
    """Annule un up interrompu en restaurant le backup et en supprimant le flag."""
    name = require_active_stack()
    interrupted = check_interrupted_up(name)
    if not interrupted:
        console.print("[yellow]Aucun up en cours ou interrompu.[/yellow]")
        return

    up_id = interrupted.get("up_id", "?")
    started_at = interrupted.get("started_at", "?")[:19]
    console.print(f"\n[bold]Cancel[/bold] — up {up_id} démarré le {started_at}\n")

    meta_path = backup_dir(name) / ".backup-meta.json"
    if meta_path.exists():
        console.print("  Restauration du backup...")
        ok = restore_backup(name)
        if ok:
            console.print("  [green]✓[/green] Backup restauré")
        else:
            console.print("  [yellow]Aucun backup à restaurer.[/yellow]")

    up_in_progress_file(name).unlink(missing_ok=True)

    append_up_log(name, {
        "up_id": f"cancel-{make_up_id()}",
        "updated_at": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S"),
        "stack": interrupted.get("stack", "—"),
        "audience": "—",
        "instruments": interrupted.get("instruments_to_download", []),
        "actions": [],
        "outcome": "cancelled",
    })

    console.print(f"[green]✓[/green] Up {up_id} annulé.\n")


# ---------------------------------------------------------------------------
# Commande rollback
# ---------------------------------------------------------------------------


@app.command()
def rollback(
    yes: bool = typer.Option(False, "--yes", "-y", help="Confirmer sans prompt interactif"),
) -> None:
    """Restaure l'état précédent (N-1) : CSV et état REDCap."""
    name = require_active_stack()
    bkp_dir = backup_dir(name)
    meta_path = bkp_dir / ".backup-meta.json"
    if not meta_path.exists():
        console.print("[yellow]Aucun backup disponible.[/yellow]")
        raise typer.Exit(1)

    meta = load_json(meta_path)
    backed_up_at = meta.get("backed_up_at", "—")[:19]
    up_id = meta.get("up_id", "—")
    instruments = meta.get("instruments", [])

    console.print(f"\n[bold]Rollback[/bold] — backup du {backed_up_at} (up {up_id})\n")
    console.print("  Sera restauré :")
    for inst_name in instruments:
        inst_data_dir = stack_instrument_dir(name, inst_name)
        for csv_path in compute_expected_csv_files(inst_name, inst_data_dir):
            backup_csv = bkp_dir / inst_name / csv_path.name
            if backup_csv.exists():
                console.print(f"    [cyan]←[/cyan] {csv_path}")
            else:
                console.print(f"    [yellow]-[/yellow] {csv_path} (absent du backup — sera supprimé)")
    if (bkp_dir / ".redcap.state.json").exists():
        console.print(f"    [cyan]←[/cyan] {redcap_state_file(name)}")
    console.print()

    if not yes:
        typer.confirm("Confirmer le rollback ?", abort=True)

    ok = restore_backup(name)
    if not ok:
        console.print("[red]Échec du rollback.[/red]")
        raise typer.Exit(1)

    up_in_progress_file(name).unlink(missing_ok=True)

    append_up_log(name, {
        "up_id": f"rollback-{make_up_id()}",
        "updated_at": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S"),
        "stack": name,
        "audience": "—",
        "instruments": instruments,
        "actions": [{"instrument": i, "status": "rolled_back"} for i in instruments],
        "outcome": "rollback",
    })

    console.print(f"[green]✓[/green] Rollback effectué (backup du {backed_up_at}).\n")


# ---------------------------------------------------------------------------
# Commande log
# ---------------------------------------------------------------------------


@app.command(name="log")
def show_log() -> None:
    """Affiche l'historique des up du stack actif."""
    name = require_active_stack()
    log_path = up_log_file(name)
    if not log_path.exists():
        console.print("[yellow]Aucun historique. Lancez `up` une première fois.[/yellow]")
        return

    with log_path.open() as f:
        entries: list[dict] = json.load(f)

    if not entries:
        console.print("[yellow]Historique vide.[/yellow]")
        return

    table = Table(show_header=True, header_style="bold")
    table.add_column("Up ID")
    table.add_column("Date")
    table.add_column("Stack")
    table.add_column("Audience")
    table.add_column("Instruments")
    table.add_column("Résultat")

    for entry in reversed(entries):
        outcome = entry.get("outcome", "?")
        if outcome == "success":
            outcome_str = "[green]✓ success[/green]"
        elif outcome == "rollback":
            outcome_str = "[cyan]← rollback[/cyan]"
        elif outcome == "cancelled":
            outcome_str = "[yellow]✗ cancelled[/yellow]"
        else:
            outcome_str = f"[red]{outcome}[/red]"

        actions = entry.get("actions", [])
        n_dl = sum(1 for a in actions if a.get("status") not in ("cached", "rolled_back"))
        n_cache = sum(1 for a in actions if a.get("status") == "cached")
        inst_summary = f"{n_dl} téléchargé(s), {n_cache} en cache" if actions else "—"

        table.add_row(
            entry.get("up_id", "—"),
            entry.get("updated_at", "—")[:19],
            entry.get("stack", "—"),
            entry.get("audience", "—"),
            inst_summary,
            outcome_str,
        )

    console.print(table)


# ---------------------------------------------------------------------------
# Commande destroy
# ---------------------------------------------------------------------------


@app.command()
def destroy(
    yes: bool = typer.Option(False, "--yes", "-y", help="Confirmer sans prompt interactif"),
) -> None:
    """Détruit toutes les données du stack actif."""
    name = require_active_stack()
    root_dir = stack_root_dir(name)
    state_file = stack_state_path(name)

    console.print(f"\n[bold]Destroying stack[/bold] [cyan]{name}[/cyan]\n")

    if not root_dir.exists() and not state_file.exists():
        console.print("[yellow]Rien à supprimer pour ce stack.[/yellow]")
        return

    console.print("  Sera supprimé :")
    if root_dir.exists():
        console.print(f"    [red]-[/red] {root_dir}/ (données + state + backup)")
    console.print()

    if not yes:
        typer.confirm("Confirmer la destruction ?", abort=True)

    if root_dir.exists():
        shutil.rmtree(root_dir)

    console.print(f"[green]✓[/green] Stack [cyan]{name}[/cyan] détruit.\n")


# ---------------------------------------------------------------------------
# Commande state
# ---------------------------------------------------------------------------


@app.command()
def state() -> None:
    """Affiche l'état courant (stack actif + données REDCap)."""
    name = require_active_stack()
    cfg = load_stack_config(name)
    audience = cfg.get("audience", "public")

    st = load_json(stack_state_path(name))
    redcap_state = load_redcap_state(name)

    console.print(f"\n[bold]Stack actif :[/bold] [cyan]{name}[/cyan]")
    if st:
        console.print(f"  Dernier up : {st.get('last_up', '—')[:19]}")
        console.print(f"  Données    : {stack_downloads_dir(name)}")
    else:
        console.print("  [yellow]Jamais exécuté.[/yellow]")

    console.print(f"\n[bold]Données REDCap (audience : {audience}) :[/bold]")
    audience_keys = [k for k in redcap_state if k.endswith(f"__{audience}")]
    if not audience_keys:
        console.print("  [yellow]Aucune donnée en cache.[/yellow]")
    else:
        table = Table(show_header=True, header_style="bold")
        table.add_column("Instrument")
        table.add_column("Téléchargé le", justify="right")
        table.add_column("Ident", justify="right")
        table.add_column("Pseudo", justify="right")
        table.add_column("Anon", justify="right")
        for key in audience_keys:
            val = redcap_state[key]
            inst_name = key.replace(f"__{audience}", "")
            counts = val.get("counts", {})
            table.add_row(
                inst_name,
                val.get("downloaded_at", "—")[:10],
                str(counts.get("n_ident", 0)),
                str(counts.get("n_pseudo", 0)),
                str(counts.get("n_anon", 0)),
            )
        console.print(table)
    console.print()



# ---------------------------------------------------------------------------
# Point d'entrée
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    app()
