library(shiny)
library(sigmajs)
library(dplyr)
library(tidyr)
library(readr)
library(igraph)
library(htmlwidgets)

# ── Données ────────────────────────────────────────────────────────────────────

data_dir <- normalizePath(
  file.path("..", "..", ".ecrin", "admin", "downloads", "project_proposal")
)

proj <- read_csv(file.path(data_dir, "identifiables.csv"), show_col_types = FALSE)

# Topics LDA des papers
lda_topics_path <- file.path(data_dir, "nlp-papers", "lda_topics.csv")
lda_indiv_path <- file.path(data_dir, "nlp-papers", "lda_individus.csv")
lda_biblio_path <- file.path(data_dir, "nlp-papers", "biblio_refs.csv")
if (file.exists(lda_topics_path) && file.exists(lda_indiv_path) && file.exists(lda_biblio_path)) {
  lda_labels <- read_csv(lda_topics_path, show_col_types = FALSE) |>
    group_by(topic) |>
    slice_min(rang, n = 3) |>
    summarise(lda_label = paste(term, collapse = ", "), .groups = "drop") |>
    mutate(lda_id = paste0("LDA_", topic))
  # lda_individus$userid = ref_id dans biblio_refs → hashed_id → acronym
  biblio <- read_csv(lda_biblio_path, show_col_types = FALSE) |>
    select(ref_id, hashed_id) |>
    distinct()
  source_ids <- read_csv(file.path(data_dir, "nlp-papers", "00_source.csv"),
    show_col_types = FALSE
  ) |> pull(id)
  # Table projet → LDA (n >= 2 chunks par topic)
  lda_proj <- read_csv(lda_indiv_path, show_col_types = FALSE) |>
    filter(userid %in% source_ids) |>
    select(userid, topic_dominant) |>
    left_join(biblio, by = c("userid" = "ref_id")) |>
    filter(!is.na(hashed_id)) |>
    count(hashed_id, topic_dominant) |>
    filter(n >= 2) |>
    left_join(
      lda_labels |> select(topic, lda_id),
      by = c("topic_dominant" = "topic")
    ) |>
    filter(!is.na(lda_id)) |>
    select(hashed_id, lda_id)
} else {
  lda_labels <- tibble(topic = integer(), lda_label = character(), lda_id = character())
  lda_proj <- tibble(hashed_id = character(), lda_id = character())
}

# Colonnes checkbox ECR/EUNICoast depuis l'export brut REDCap (séparateur ;)
raw_path <- normalizePath(file.path("..", "..", "ECRINV1_DATA_2026-03-18_1018.csv"),
  mustWork = FALSE
)
if (file.exists(raw_path)) {
  raw <- read_delim(raw_path, delim = ";", show_col_types = FALSE, name_repair = "minimal") |>
    mutate(acronym = trimws(acronym)) |>
    select(
      acronym,
      ecr_integration___1, ecr_integration___2, ecr_integration___3,
      eunicoast_integration___1, eunicoast_integration___2, eunicoast_integration___3,
      eunicoast_integration___4, eunicoast_integration___5
    ) |>
    filter(!is.na(acronym) & acronym != "")
  proj <- proj |> left_join(raw, by = "acronym")
} else {
  proj <- proj |> mutate(
    ecr_integration___1 = NA_real_, ecr_integration___2 = NA_real_, ecr_integration___3 = NA_real_,
    eunicoast_integration___1 = NA_real_, eunicoast_integration___2 = NA_real_,
    eunicoast_integration___3 = NA_real_, eunicoast_integration___4 = NA_real_,
    eunicoast_integration___5 = NA_real_
  )
}

prof_path <- file.path(dirname(data_dir), "researcher_profile", "identifiables.csv")
if (file.exists(prof_path)) {
  prof <- read_csv(prof_path, show_col_types = FALSE) |>
    mutate(
      full_name = paste0(
        stringr::str_to_title(first_name), " ",
        stringr::str_to_title(last_name)
      ),
      institution = dplyr::coalesce(
        dplyr::if_else(institution_other == "" | is.na(institution_other), NA_character_, institution_other),
        dplyr::if_else(institution == "" | is.na(institution), NA_character_, institution)
      )
    ) |>
    select(hashed_id, full_name, institution)
} else {
  prof <- proj |> transmute(hashed_id, full_name = hashed_id, institution = NA_character_)
}

proj <- proj |>
  left_join(prof, by = "hashed_id") |>
  mutate(full_name = coalesce(full_name, hashed_id))

# ── Structure fixe ECR / EUNICoast ─────────────────────────────────────────────

ecr_axes <- c(
  "Maritime and port issues",
  "The city of tomorrow: architecture, urban planning, sustainable construction and design",
  "Transitions, risks and hazards"
)

hub_axes <- c(
  "Identities, Local Knowledge, and Cultural Heritage in Islands & Coastal Communities",
  "Blue Circular Economy, Port Logistics, and Sustainable Blue Tourism",
  "Governance, Planning, Management, and Monitoring of Islands and Coastal Communities",
  "Health, Biodiversity Protection, Nature-based Solutions, and Sustainable Exploration of Coastal/Marine Resources",
  "Engineered and Data-driven Solutions for Coastal Infrastructures, Marine Renewable Energy, Marine Safety, and Navigation Systems" # nolint: line_length_linter
)

# ── Palettes ───────────────────────────────────────────────────────────────────

type_pal <- c(
  chercheur   = "#2166ac",
  projet      = "#f4a261",
  institution = "#1a9850",
  ecr         = "#e63946",
  hub         = "#457b9d",
  cptmp       = "#999999",
  eunicoast   = "#999999",
  lda         = "#17becf",
  topic       = "#d6604d",
  keyword     = "#4dac26",
  method      = "#7b2d8b",
  region      = "#e08214"
)

type_labels <- c(
  cptmp       = "CPTMP",
  eunicoast   = "EUNICoast",
  chercheur   = "Chercheur",
  projet      = "Projet",
  institution = "Affiliation",
  ecr         = "Axe ECR",
  hub         = "Hub EUNICoast",
  lda         = "Topic LDA (papers)",
  topic       = "Topic",
  keyword     = "Mot-clé",
  method      = "Méthode",
  region      = "Région"
)

layout_choices <- c(
  "ForceAtlas2 (dynamique)" = "forceatlas2",
  "Fruchterman-Reingold"    = "fr",
  "Kamada-Kawai"            = "kk",
  "Cercle"                  = "circle",
  "Grille"                  = "grid",
  "Aléatoire"               = "random"
)

# ── Sanitisation des IDs nœuds ─────────────────────────────────────────────────
# sigmajs plante silencieusement si les IDs contiennent des espaces / caractères spéciaux

make_id <- function(x) {
  gsub("[^A-Za-z0-9_]", "_", trimws(as.character(x)))
}

# Tables de correspondance label → id pour les types dont l'ID n'est pas déjà simple
ecr_ids <- setNames(make_id(ecr_axes), ecr_axes)
hub_ids <- setNames(make_id(hub_axes), hub_axes)

# ── Construction du graphe ─────────────────────────────────────────────────────

build_graph <- function(data, show_types, hide_isolated = FALSE) {
  nodes <- bind_rows(
    if ("cptmp" %in% show_types) {
      tibble(id = "CPTMP", label = "", node_type = "cptmp", size = 12, image = "cptmp.png")
    },
    if ("eunicoast" %in% show_types) {
      tibble(id = "EUNICoast", label = "", node_type = "eunicoast", size = 12, image = "eunicoast.png")
    },
    if ("ecr" %in% show_types) {
      tibble(id = unname(ecr_ids), label = ecr_axes, node_type = "ecr", size = 6, image = NA_character_)
    },
    if ("hub" %in% show_types) {
      tibble(id = unname(hub_ids), label = hub_axes, node_type = "hub", size = 6, image = NA_character_)
    },
    if ("chercheur" %in% show_types) {
      data |>
        distinct(full_name) |>
        transmute(id = make_id(full_name), label = full_name, node_type = "chercheur", size = 5, image = NA_character_)
    },
    if ("projet" %in% show_types) {
      data |>
        filter(!is.na(acronym) & acronym != "") |>
        distinct(acronym) |>
        transmute(id = make_id(acronym), label = acronym, node_type = "projet", size = 4, image = NA_character_)
    },
    if ("institution" %in% show_types) {
      data |>
        filter(!is.na(institution) & institution != "") |>
        distinct(institution) |>
        transmute(
          id = make_id(institution), label = institution,
          node_type = "institution", size = 4, image = NA_character_
        )
    },
    if ("lda" %in% show_types) {
      lda_labels |>
        filter(!is.na(lda_id)) |>
        transmute(id = make_id(lda_id), label = lda_label, node_type = "lda", size = 3, image = NA_character_)
    },
    if ("topic" %in% show_types) {
      data |>
        filter(!is.na(topic) & topic != "") |>
        distinct(topic) |>
        transmute(id = make_id(topic), label = topic, node_type = "topic", size = 4, image = NA_character_)
    },
    if ("keyword" %in% show_types) {
      data |>
        pivot_longer(keyword1:keyword3, values_to = "v") |>
        filter(!is.na(v) & v != "") |>
        distinct(v) |>
        transmute(id = make_id(v), label = v, node_type = "keyword", size = 2, image = NA_character_)
    },
    if ("method" %in% show_types) {
      data |>
        pivot_longer(method1:method3, values_to = "v") |>
        filter(!is.na(v) & v != "") |>
        distinct(v) |>
        transmute(id = make_id(v), label = v, node_type = "method", size = 2, image = NA_character_)
    },
    if ("region" %in% show_types) {
      data |>
        pivot_longer(region1:region3, values_to = "v") |>
        filter(!is.na(v) & v != "") |>
        distinct(v) |>
        transmute(id = make_id(v), label = v, node_type = "region", size = 2, image = NA_character_)
    }
  ) |>
    distinct(id, .keep_all = TRUE) |>
    mutate(color = type_pal[node_type])

  empty_edges <- tibble(id = character(), source = character(), target = character())

  mk_edges <- function(cond, pfx, src, tgt) {
    if (!cond) {
      return(empty_edges)
    }
    d <- tibble(source = as.character(src), target = as.character(tgt))
    d |> mutate(id = paste0(pfx, row_number()))
  }

  edges <- bind_rows(
    mk_edges(
      all(c("cptmp", "ecr") %in% show_types),
      "ce", rep("CPTMP", length(ecr_ids)), unname(ecr_ids)
    ),
    mk_edges(
      all(c("eunicoast", "hub") %in% show_types),
      "eh", rep("EUNICoast", length(hub_ids)), unname(hub_ids)
    ),
    {
      # Projet → axes ECR via colonnes checkbox ___1/2/3
      if (!all(c("projet", "ecr") %in% show_types)) {
        empty_edges
      } else {
        d <- data |> filter(!is.na(acronym) & acronym != "")
        bind_rows(lapply(seq_along(ecr_axes), function(i) {
          col <- paste0("ecr_integration___", i)
          if (!col %in% names(d)) {
            return(empty_edges)
          }
          rows <- d[!is.na(d[[col]]) & d[[col]] == 1, ]
          if (nrow(rows) == 0) {
            return(empty_edges)
          }
          tibble(
            id = paste0("pe", i, "_", seq_len(nrow(rows))),
            source = make_id(rows$acronym), target = ecr_ids[ecr_axes[i]]
          )
        }))
      }
    },
    {
      # Projet → hubs EUNICoast via colonnes checkbox ___1/2/3/4/5
      if (!all(c("projet", "hub") %in% show_types)) {
        empty_edges
      } else {
        d <- data |> filter(!is.na(acronym) & acronym != "")
        bind_rows(lapply(seq_along(hub_axes), function(i) {
          col <- paste0("eunicoast_integration___", i)
          if (!col %in% names(d)) {
            return(empty_edges)
          }
          rows <- d[!is.na(d[[col]]) & d[[col]] == 1, ]
          if (nrow(rows) == 0) {
            return(empty_edges)
          }
          tibble(
            id = paste0("ph", i, "_", seq_len(nrow(rows))),
            source = make_id(rows$acronym), target = hub_ids[hub_axes[i]]
          )
        }))
      }
    },
    mk_edges(
      all(c("chercheur", "projet") %in% show_types), "cp",
      make_id(data$full_name[!is.na(data$acronym) & data$acronym != ""]),
      make_id(data$acronym[!is.na(data$acronym) & data$acronym != ""])
    ),
    mk_edges(
      all(c("chercheur", "institution") %in% show_types), "ci",
      make_id(data$full_name[!is.na(data$institution) & data$institution != ""]),
      make_id(data$institution[!is.na(data$institution) & data$institution != ""])
    ),
    mk_edges(
      all(c("chercheur", "topic") %in% show_types) && !"projet" %in% show_types, "ct",
      make_id(data$full_name[!is.na(data$topic) & data$topic != ""]),
      make_id(data$topic[!is.na(data$topic) & data$topic != ""])
    ),
    mk_edges(
      all(c("projet", "topic") %in% show_types), "pt",
      make_id(data$acronym[!is.na(data$acronym) & data$acronym != "" & !is.na(data$topic) & data$topic != ""]),
      make_id(data$topic[!is.na(data$acronym) & data$acronym != "" & !is.na(data$topic) & data$topic != ""])
    ),
    {
      if (!all(c("projet", "lda") %in% show_types)) {
        empty_edges
      } else {
        proj_acronyms <- data |>
          filter(!is.na(acronym) & acronym != "") |>
          distinct(hashed_id, acronym)
        lda_proj |>
          inner_join(proj_acronyms, by = "hashed_id") |>
          transmute(id = paste0("pl", row_number()), source = make_id(acronym), target = make_id(lda_id))
      }
    },
    {
      if (!all(c("projet", "keyword") %in% show_types)) {
        empty_edges
      } else {
        data |>
          filter(!is.na(acronym) & acronym != "") |>
          pivot_longer(keyword1:keyword3, values_to = "v") |>
          filter(!is.na(v) & v != "") |>
          transmute(id = paste0("ck_", row_number()), source = make_id(acronym), target = make_id(v))
      }
    },
    {
      if (!all(c("projet", "method") %in% show_types)) {
        empty_edges
      } else {
        data |>
          filter(!is.na(acronym) & acronym != "") |>
          pivot_longer(method1:method3, values_to = "v") |>
          filter(!is.na(v) & v != "") |>
          transmute(id = paste0("pm_", row_number()), source = make_id(acronym), target = make_id(v))
      }
    },
    {
      if (!all(c("projet", "region") %in% show_types)) {
        empty_edges
      } else {
        data |>
          filter(!is.na(acronym) & acronym != "") |>
          pivot_longer(region1:region3, values_to = "v") |>
          filter(!is.na(v) & v != "") |>
          transmute(id = paste0("pr_", row_number()), source = make_id(acronym), target = make_id(v))
      }
    }
  ) |>
    filter(source %in% nodes$id & target %in% nodes$id) |>
    distinct(source, target, .keep_all = TRUE) |>
    mutate(color = "#cccccc")

  connected <- union(edges$source, edges$target)
  nodes <- nodes |> filter(!hide_isolated | id %in% connected)

  degree <- table(c(edges$source, edges$target))
  nodes <- nodes |>
    mutate(
      size = 2 + 3 * sqrt(as.integer(degree[id]))
    ) |>
    mutate(size = ifelse(is.na(size), 2, size))

  list(nodes = nodes, edges = edges)
}

apply_igraph_layout <- function(nodes, edges, layout_fn) {
  if (nrow(nodes) == 0) {
    return(mutate(nodes, x = numeric(0), y = numeric(0)))
  }
  g <- graph_from_data_frame(
    d        = edges |> select(from = source, to = target),
    vertices = nodes |> select(name = id),
    directed = FALSE
  )
  coords <- layout_fn(g)
  nodes |> mutate(x = coords[, 1] * 10, y = coords[, 2] * 10)
}

# ── JS : renderer image pour nœuds CPTMP / EUNICoast ──────────────────────────

image_renderer_js <- "
function(el, x) {
  // Renderer sigma pour les nœuds image (CPTMP / EUNICoast)
  sigma.canvas.nodes.image = (function() {
    var _cache = {};
    var _loading = {};
    return function(node, context, settings) {
      var prefix = settings('prefix') || '';
      var size   = node[prefix + 'size'] || 1;
      var nx     = node[prefix + 'x'];
      var ny     = node[prefix + 'y'];
      if (node.image && node.image !== 'NA') {
        var img = _cache[node.image];
        if (!img) {
          if (!_loading[node.image]) {
            _loading[node.image] = true;
            var i = new Image();
            i.onload = function() {
              _cache[node.image] = i;
              var w = HTMLWidgets.find('#' + el.id);
              var s = w ? w.getChart() : null;
              if (s) s.refresh();
            };
            i.src = node.image;
          }
          context.fillStyle = node.color || '#999';
          context.beginPath();
          context.arc(nx, ny, size, 0, Math.PI * 2, true);
          context.closePath();
          context.fill();
          return;
        }
        context.save();
        context.beginPath();
        context.arc(nx, ny, size, 0, Math.PI * 2, true);
        context.closePath();
        context.clip();
        var iw = img.naturalWidth  || img.width;
        var ih = img.naturalHeight || img.height;
        var scale = (size * 2) / Math.max(iw, ih);
        context.drawImage(img, nx - iw * scale / 2, ny - ih * scale / 2, iw * scale, ih * scale);
        context.restore();
        context.strokeStyle = '#ffffff';
        context.lineWidth   = size * 0.1;
        context.beginPath();
        context.arc(nx, ny, size, 0, Math.PI * 2, true);
        context.closePath();
        context.stroke();
      } else {
        context.fillStyle = node.color || '#ccc';
        context.beginPath();
        context.arc(nx, ny, size, 0, Math.PI * 2, true);
        context.closePath();
        context.fill();
      }
    };
  })();

  var widget = HTMLWidgets.find('#' + el.id);
  var s = widget ? widget.getChart() : null;
  if (s) {
    s.bind('downNode', function() {
      if (s.isForceAtlas2Running()) s.stopForceAtlas2();
    });
    el.addEventListener('mouseup', function() {
      var cur = HTMLWidgets.find('#' + el.id);
      var sc = cur ? cur.getChart() : null;
      if (sc && sc.supervisor && sc.isForceAtlas2Running()) sc.startForceAtlas2();
    });
    el.addEventListener('touchend', function() {
      var cur = HTMLWidgets.find('#' + el.id);
      var sc = cur ? cur.getChart() : null;
      if (sc && sc.supervisor && sc.isForceAtlas2Running()) sc.startForceAtlas2();
    });

    // ── Surbrillance au survol ──────────────────────────────────────────────
    s.bind('overNode', function(e) {
      var nodeId = e.data.node.id;
      var neighbors = {};
      neighbors[nodeId] = true;
      s.graph.edges().forEach(function(edge) {
        if (edge.source === nodeId) neighbors[edge.target] = true;
        if (edge.target === nodeId) neighbors[edge.source] = true;
      });
      s.graph.nodes().forEach(function(node) {
        node.originalColor = node.originalColor || node.color;
        node.color = neighbors[node.id] ? node.originalColor : 'rgba(200,200,200,0.15)';
      });
      s.graph.edges().forEach(function(edge) {
        edge.originalColor = edge.originalColor || edge.color;
        edge.color = (edge.source === nodeId || edge.target === nodeId)
          ? 'rgba(100,100,100,0.6)'
          : 'rgba(200,200,200,0.05)';
      });
      s.refresh({ skipIndexation: true });
    });

    s.bind('outNode', function() {
      s.graph.nodes().forEach(function(node) {
        if (node.originalColor) { node.color = node.originalColor; delete node.originalColor; }
      });
      s.graph.edges().forEach(function(edge) {
        if (edge.originalColor) { edge.color = edge.originalColor; delete edge.originalColor; }
      });
      s.refresh({ skipIndexation: true });
    });
  }
}
"

# ── UI ─────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: sans-serif; margin: 0; overflow: hidden; }
    .layout-wrapper { display: flex; height: 100vh; }
    .sidebar {
      width: 240px; min-width: 240px; height: 100vh; overflow-y: auto;
      background: #f8f9fa; border-right: 1px solid #dee2e6;
      padding: 14px 14px 20px; box-sizing: border-box;
    }
    .sidebar h6 { font-size: 11px; text-transform: uppercase; color: #888; margin: 16px 0 6px; letter-spacing: 0.05em; }
    .sidebar h6:first-child { margin-top: 0; }
    #sigma-container { flex: 1; height: 100vh; }
    .legend-item { display: flex; align-items: center; gap: 7px; font-size: 13px; margin-bottom: 5px; }
    .legend-dot { width: 12px; height: 12px; border-radius: 50%; flex-shrink: 0; }
    .form-group { margin-bottom: 6px; }
    .irs--shiny .irs-bar { background: #2166ac; border-color: #2166ac; }
    .irs--shiny .irs-handle { background: #2166ac; }
  "))),
  div(
    class = "layout-wrapper",
    div(
      class = "sidebar",
      h6("Filtres"),
      checkboxGroupInput("show_types",
        label = "Types de nœuds",
        choices = setNames(names(type_labels), type_labels),
        selected = setdiff(names(type_labels), c("institution", "cptmp", "eunicoast", "hub"))
      ),
      selectInput("filter_chercheur",
        label = "Chercheur",
        choices = c("Tous" = "", sort(unique(proj$full_name))),
        selected = "",
        width = "100%"
      ),
      h6("Layout"),
      selectInput("layout_algo",
        label = NULL,
        choices = layout_choices,
        selected = "forceatlas2",
        width = "100%"
      ),
      conditionalPanel(
        condition = "input.layout_algo == 'forceatlas2'",
        sliderInput("gravity", "Gravity", min = 0.1, max = 5, value = 1, step = 0.1, width = "100%"),
        sliderInput("scaling", "Compacité", min = 0.5, max = 10, value = 2, step = 0.5, width = "100%"),
        sliderInput("slowdown", "Slow down", min = 1, max = 20, value = 5, step = 1, width = "100%"),
        sliderInput("edge_weight", "Edge weight", min = 0, max = 1, value = 0, step = 0.1, width = "100%"),
        div(
          style = "display: flex; gap: 6px; margin-top: 8px;",
          actionButton("relayout", "Relancer",
            class = "btn btn-primary btn-sm", style = "flex: 1;"
          ),
          actionButton("toggle_force", "⏸",
            class = "btn btn-outline-secondary btn-sm", style = "width: 36px;",
            title = "Pause / Reprendre le layout"
          )
        )
      ),
      conditionalPanel(
        condition = "input.layout_algo != 'forceatlas2'",
        actionButton("relayout_static", "Relancer",
          class = "btn btn-primary btn-sm", style = "width: 100%; margin-top: 4px;"
        )
      ),
      h6("Zoom"),
      div(
        style = "display: flex; gap: 6px;",
        tags$button("＋",
          class = "btn btn-outline-secondary btn-sm", style = "flex:1;",
          onclick = "var s=HTMLWidgets.find('#graph').getChart();s.cameras[0].goTo({ratio:s.cameras[0].ratio/1.5});"
        ),
        tags$button("−",
          class = "btn btn-outline-secondary btn-sm", style = "flex:1;",
          onclick = "var s=HTMLWidgets.find('#graph').getChart();s.cameras[0].goTo({ratio:s.cameras[0].ratio*1.5});"
        ),
        tags$button("⌂",
          class = "btn btn-outline-secondary btn-sm", style = "flex:1;",
          onclick = "var s=HTMLWidgets.find('#graph').getChart();s.cameras[0].goTo({x:0,y:0,ratio:1});",
          title = "Réinitialiser la vue"
        )
      ),
      h6("Légende"),
      lapply(names(type_labels), function(t) {
        div(
          class = "legend-item",
          if (t == "cptmp") {
            tags$img(src = "cptmp.png", width = 16, height = 16, style = "border-radius:50%;flex-shrink:0;")
          } else if (t == "eunicoast") {
            tags$img(src = "eunicoast.png", width = 16, height = 16, style = "border-radius:50%;flex-shrink:0;")
          } else {
            span(class = "legend-dot", style = sprintf("background:%s;", type_pal[t]))
          },
          type_labels[t]
        )
      })
    ),
    div(
      id = "sigma-container",
      sigmajsOutput("graph", width = "100%", height = "100%")
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  filtered_data <- reactive({
    d <- proj
    if (!is.null(input$filter_chercheur) && input$filter_chercheur != "") {
      d <- d |> filter(full_name == input$filter_chercheur)
    }
    d
  })

  graph_data <- reactive({
    input$relayout
    input$relayout_static
    show <- input$show_types
    if (is.null(show) || length(show) == 0) show <- names(type_labels)
    build_graph(filtered_data(), show, hide_isolated = TRUE)
  })

  output$graph <- renderSigmajs({
    g <- graph_data()
    algo <- input$layout_algo
    if (nrow(g$nodes) == 0) {
      return(sigmajs())
    }

    layout_fn <- switch(algo,
      fr = igraph::layout_with_fr,
      kk = igraph::layout_with_kk,
      circle = igraph::layout_in_circle,
      grid = igraph::layout_on_grid,
      random = igraph::layout_randomly,
      function(x) matrix(runif(vcount(x) * 2, -10, 10), ncol = 2)
    )

    nodes <- if (algo == "forceatlas2" || nrow(g$edges) == 0) {
      g$nodes |> mutate(x = runif(n(), -10, 10), y = runif(n(), -10, 10))
    } else {
      tryCatch(
        apply_igraph_layout(g$nodes, g$edges, layout_fn),
        error = function(e) g$nodes |> mutate(x = runif(n(), -10, 10), y = runif(n(), -10, 10))
      )
    }

    # Affecter le type renderer "image" aux nœuds avec image
    nodes <- nodes |> mutate(
      type = if_else(!is.na(image) & image != "NA", "image", "circle")
    )

    sg <- sigmajs() |>
      sg_nodes(nodes, id, label, size, color, x, y, type, image) |>
      sg_edges(g$edges, id, source, target, color) |>
      sg_settings(
        defaultEdgeColor  = "#cccccc",
        minNodeSize       = 2,
        maxNodeSize       = 14,
        labelThreshold    = 2,
        drawEdges         = TRUE,
        defaultLabelColor = "#333333"
      ) |>
      sg_drag_nodes() |>
      onRender(image_renderer_js)

    if (algo == "forceatlas2") {
      sg <- sg |> sg_force_start(
        worker              = TRUE,
        randomize           = TRUE,
        gravity             = input$gravity,
        scalingRatio        = input$scaling,
        slowDown            = input$slowdown,
        edgeWeightInfluence = input$edge_weight
      )
      force_running(TRUE)
      updateActionButton(session, "toggle_force", label = "⏸")
    }

    sg
  })

  force_running <- reactiveVal(TRUE)

  observeEvent(input$toggle_force, {
    proxy <- sigmajsProxy("graph")
    if (force_running()) {
      sg_force_stop_p(proxy)
      force_running(FALSE)
      updateActionButton(session, "toggle_force", label = "▶")
    } else {
      sg_force_start_p(proxy,
        gravity             = input$gravity,
        scalingRatio        = input$scaling,
        slowDown            = input$slowdown,
        edgeWeightInfluence = input$edge_weight
      )
      force_running(TRUE)
      updateActionButton(session, "toggle_force", label = "⏸")
    }
  })

  observeEvent(
    list(input$gravity, input$scaling, input$slowdown, input$edge_weight),
    {
      if (force_running() && input$layout_algo == "forceatlas2") {
        proxy <- sigmajsProxy("graph")
        sg_force_start_p(proxy,
          gravity             = input$gravity,
          scalingRatio        = input$scaling,
          slowDown            = input$slowdown,
          edgeWeightInfluence = input$edge_weight
        )
      }
    },
    ignoreInit = TRUE
  )
}

shinyApp(ui, server)
