library(shiny)
library(visNetwork)
library(dplyr)
library(tidyr)
library(readr)
library(igraph)

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
  biblio <- read_csv(lda_biblio_path, show_col_types = FALSE) |>
    select(ref_id, hashed_id) |>
    mutate(ref_id = as.character(ref_id)) |>
    distinct()
  source_ids <- read_csv(file.path(data_dir, "nlp-papers", "00_source.csv"),
    show_col_types = FALSE
  ) |> pull(id)
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
    ecr_integration___1 = NA_real_, ecr_integration___2 = NA_real_,
    ecr_integration___3 = NA_real_,
    eunicoast_integration___1 = NA_real_, eunicoast_integration___2 = NA_real_,
    eunicoast_integration___3 = NA_real_, eunicoast_integration___4 = NA_real_,
    eunicoast_integration___5 = NA_real_
  )
}

prof_path <- file.path(dirname(data_dir), "researcher_profile", "identifiables.csv")
rq_path <- file.path(dirname(data_dir), "research_questions", "identifiables.csv")
pub_path <- file.path(dirname(data_dir), "publications", "identifiables.csv")
pub_files <- file.path(dirname(data_dir), "publications", "files")
addResourcePath("pub_files", pub_files)

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

rq_data <- if (file.exists(rq_path)) {
  read_csv(rq_path, show_col_types = FALSE) |> select(hashed_id, research_questions)
} else {
  tibble(hashed_id = character(), research_questions = character())
}

prof <- prof |>
  left_join(rq_data, by = "hashed_id")

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
  "Health, Biodiversity Protection, Nature-based Solutions, and Sustainable Exploration of Coastal/Marine Resources", # nolint: line_length_linter
  "Engineered and Data-driven Solutions for Coastal Infrastructures, Marine Renewable Energy, Marine Safety, and Navigation Systems" # nolint: line_length_linter
)

# ── Palettes ───────────────────────────────────────────────────────────────────

type_pal <- c(
  chercheur   = "#1f77b4", # bleu
  projet      = "#ff7f0e", # orange vif
  institution = "#2ca02c", # vert
  ecr         = "#d62728", # rouge
  hub         = "#9467bd", # violet
  cptmp       = "#8c564b", # brun
  eunicoast   = "#7f7f7f", # gris
  lda         = "#17becf", # cyan
  topic       = "#bcbd22", # jaune-vert
  keyword     = "#e377c2", # rose
  method      = "#393b79", # bleu nuit
  region      = "#00b09b" # turquoise
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

# Tailles de police de base (modulées par window.visFontScale côté JS)
# JS handlers pour visEvents (extraits pour respecter la limite de 120 car.)
js_select_node <- "function(p) {
  Shiny.setInputValue('graph_click_node', {id: p.nodes[0]}, {priority: 'event'});
  var s = window.visFontScale || 1;
  var net = this;
  net.body.data.nodes.update(
    net.body.data.nodes.getIds().map(function(id) { return {id: id, font: {size: 11 * s}}; })
  );
  var sel = p.nodes[0];
  var neighbors = net.getConnectedNodes(sel);
  neighbors.push(sel);
  net.body.data.nodes.update(neighbors.map(function(id) { return {id: id, font: {size: 18 * s}}; }));
}"
js_deselect_node <- "function(p) {
  Shiny.setInputValue('graph_click_node', {id: null}, {priority: 'event'});
  var s = window.visFontScale || 1;
  this.body.data.nodes.update(
    this.body.data.nodes.getIds().map(function(id) { return {id: id, font: {size: 11 * s}}; })
  );
}"
js_hover_node <- "function(p) {
  var net = this;
  var selected = net.getSelectedNodes();
  if (selected.length > 0) return;
  var s = window.visFontScale || 1;
  var neighbors = net.getConnectedNodes(p.node);
  neighbors.push(p.node);
  net.body.data.nodes.update(neighbors.map(function(id) { return {id: id, font: {size: 18 * s}}; }));
}"
js_blur_node <- "function(p) {
  var net = this;
  var selected = net.getSelectedNodes();
  if (selected.length > 0) return;
  var s = window.visFontScale || 1;
  net.body.data.nodes.update(
    net.body.data.nodes.getIds().map(function(id) { return {id: id, font: {size: 11 * s}}; })
  );
}"

layout_choices <- c(
  "ForceAtlas2 (dynamique)" = "forceatlas2",
  "Fruchterman-Reingold"    = "fr",
  "Kamada-Kawai"            = "kk",
  "Cercle"                  = "circle",
  "Grille"                  = "grid",
  "Aléatoire"               = "random"
)

make_id <- function(x) {
  gsub("[^A-Za-z0-9_]", "_", trimws(as.character(x)))
}

ecr_ids <- setNames(make_id(ecr_axes), ecr_axes)
hub_ids <- setNames(make_id(hub_axes), hub_axes)

# ── Construction du graphe ─────────────────────────────────────────────────────

build_graph <- function(data, show_types, hide_isolated = FALSE) {
  nodes <- bind_rows(
    if ("cptmp" %in% show_types) {
      tibble(id = "CPTMP", label = "CPTMP", node_type = "cptmp", size = 12, image = "cptmp.png")
    },
    if ("eunicoast" %in% show_types) {
      tibble(
        id = "EUNICoast", label = "EUNICoast", node_type = "eunicoast",
        size = 12, image = "eunicoast.png"
      )
    },
    if ("ecr" %in% show_types) {
      tibble(
        id = unname(ecr_ids), label = ecr_axes,
        node_type = "ecr", size = 6, image = NA_character_
      )
    },
    if ("hub" %in% show_types) {
      tibble(
        id = unname(hub_ids), label = hub_axes,
        node_type = "hub", size = 6, image = NA_character_
      )
    },
    if ("chercheur" %in% show_types) {
      data |>
        distinct(full_name) |>
        transmute(
          id = make_id(full_name), label = full_name,
          node_type = "chercheur", size = 5, image = NA_character_
        )
    },
    if ("projet" %in% show_types) {
      data |>
        filter(!is.na(acronym) & acronym != "") |>
        distinct(acronym) |>
        transmute(
          id = make_id(acronym), label = acronym,
          node_type = "projet", size = 4, image = NA_character_
        )
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
        transmute(
          id = make_id(lda_id), label = lda_label,
          node_type = "lda", size = 3, image = NA_character_
        )
    },
    if ("topic" %in% show_types) {
      data |>
        filter(!is.na(topic) & topic != "") |>
        distinct(topic) |>
        transmute(
          id = make_id(topic), label = topic,
          node_type = "topic", size = 4, image = NA_character_
        )
    },
    if ("keyword" %in% show_types) {
      data |>
        pivot_longer(keyword1:keyword3, values_to = "v") |>
        filter(!is.na(v) & v != "") |>
        distinct(v) |>
        transmute(
          id = make_id(v), label = v,
          node_type = "keyword", size = 2, image = NA_character_
        )
    },
    if ("method" %in% show_types) {
      data |>
        pivot_longer(method1:method3, values_to = "v") |>
        filter(!is.na(v) & v != "") |>
        distinct(v) |>
        transmute(
          id = make_id(v), label = v,
          node_type = "method", size = 2, image = NA_character_
        )
    },
    if ("region" %in% show_types) {
      data |>
        pivot_longer(region1:region3, values_to = "v") |>
        filter(!is.na(v) & v != "") |>
        distinct(v) |>
        transmute(
          id = make_id(v), label = v,
          node_type = "region", size = 2, image = NA_character_
        )
    }
  ) |>
    distinct(id, .keep_all = TRUE) |>
    mutate(color = type_pal[node_type])

  empty_edges <- tibble(from = character(), to = character(), id = character())

  mk_edges <- function(cond, pfx, src, tgt) {
    if (!cond) {
      return(empty_edges)
    }
    d <- tibble(from = as.character(src), to = as.character(tgt))
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
            from = make_id(rows$acronym), to = ecr_ids[ecr_axes[i]]
          )
        }))
      }
    },
    {
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
            from = make_id(rows$acronym), to = hub_ids[hub_axes[i]]
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
      make_id(data$acronym[
        !is.na(data$acronym) & data$acronym != "" & !is.na(data$topic) & data$topic != ""
      ]),
      make_id(data$topic[
        !is.na(data$acronym) & data$acronym != "" & !is.na(data$topic) & data$topic != ""
      ])
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
          transmute(id = paste0("pl", row_number()), from = make_id(acronym), to = make_id(lda_id))
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
          transmute(id = paste0("ck_", row_number()), from = make_id(acronym), to = make_id(v))
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
          transmute(id = paste0("pm_", row_number()), from = make_id(acronym), to = make_id(v))
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
          transmute(id = paste0("pr_", row_number()), from = make_id(acronym), to = make_id(v))
      }
    }
  ) |>
    filter(from %in% nodes$id & to %in% nodes$id) |>
    distinct(from, to, .keep_all = TRUE)

  connected <- union(edges$from, edges$to)
  nodes <- nodes |> filter(!hide_isolated | id %in% connected)

  degree <- table(c(edges$from, edges$to))
  nodes <- nodes |>
    mutate(degree = as.numeric(degree[id])) |>
    mutate(degree = ifelse(is.na(degree), 0, degree))

  # Betweenness centrality via igraph
  if (nrow(edges) > 0) {
    g_ig <- graph_from_data_frame(
      d = edges |> select(from, to),
      vertices = nodes |> select(name = id),
      directed = FALSE
    )
    btw <- betweenness(g_ig, normalized = TRUE)
    nodes <- nodes |> mutate(betweenness = as.numeric(btw[id]))
  } else {
    nodes <- nodes |> mutate(betweenness = 0)
  }
  nodes <- nodes |> mutate(value = degree + 1L)

  list(nodes = nodes, edges = edges)
}

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
    .sidebar h6 {
      font-size: 11px; text-transform: uppercase; color: #888;
      margin: 16px 0 6px; letter-spacing: 0.05em;
    }
    .sidebar h6:first-child { margin-top: 0; }
    #graph-container { flex: 1; height: 100vh; }
    .legend-item { display: flex; align-items: center; gap: 7px; font-size: 13px; margin-bottom: 5px; }
    .legend-dot { width: 12px; height: 12px; border-radius: 50%; flex-shrink: 0; }
    .form-group { margin-bottom: 6px; }
    #node-card {
      position: fixed; bottom: 20px; right: 20px; z-index: 1000;
      width: 320px; background: #fff; border-radius: 8px;
      box-shadow: 0 4px 20px rgba(0,0,0,0.18); padding: 16px;
      font-size: 13px; line-height: 1.5; display: none;
    }
    #node-card.visible { display: block; }
    #node-card .card-name { font-size: 15px; font-weight: 600; margin-bottom: 4px; }
    #node-card .card-inst { color: #666; margin-bottom: 10px; font-size: 12px; }
    #node-card .card-section {
      font-size: 11px; text-transform: uppercase; color: #999;
      letter-spacing: 0.05em; margin-bottom: 4px;
    }
    #node-card .card-text { color: #333; margin-bottom: 10px; }
    #node-card .card-close {
      position: absolute; top: 10px; right: 12px; cursor: pointer;
      color: #aaa; font-size: 16px; line-height: 1;
    }
    #node-card .card-close:hover { color: #333; }
  "))),
  tags$script(HTML("
    window.visFontScale = 1;
    Shiny.addCustomMessageHandler('node_card_toggle', function(msg) {
      var card = document.getElementById('node-card');
      if (msg.visible) {
        card.classList.add('visible');
      } else {
        card.classList.remove('visible');
      }
    });
    Shiny.addCustomMessageHandler('font_scale', function(msg) {
      window.visFontScale = msg.scale;
      var el = document.getElementById('graphgraph');
      if (!el || !el.chart) return;
      el.chart.body.data.nodes.update(
        el.chart.body.data.nodes.getIds().map(function(id) {
          return {id: id, font: {size: 11 * msg.scale}};
        })
      );
    });
  ")),
  div(
    class = "layout-wrapper",
    div(
      class = "sidebar",
      h6("Filtres"),
      checkboxGroupInput("show_types",
        label = "Types de nœuds",
        choices = setNames(names(type_labels), type_labels),
        selected = setdiff(names(type_labels), c("institution", "cptmp", "eunicoast", "hub", "lda"))
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
        sliderInput("gravity", "Gravity", min = 1, max = 100, value = 50, step = 1, width = "100%"),
        sliderInput("spring_length", "Spring length", min = 10, max = 300, value = 100, step = 10, width = "100%"),
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
      h6("Taille des nœuds"),
      sliderInput("font_scale", "Échelle du texte",
        min = 0.5, max = 3, value = 1, step = 0.1, width = "100%"
      ),
      selectInput("node_size_metric",
        label = NULL,
        choices = c("Degré" = "degree", "Betweenness" = "betweenness"),
        selected = "degree",
        width = "100%"
      ),
      h6("Légende"),
      lapply(names(type_labels), function(t) {
        div(
          class = "legend-item",
          `data-node-type` = t,
          style = "cursor: pointer;",
          if (t %in% c("cptmp", "eunicoast")) {
            tags$img(
              src = paste0(t, ".png"), width = 16, height = 16,
              style = "border-radius:50%;flex-shrink:0;"
            )
          } else {
            span(class = "legend-dot", style = sprintf("background:%s;", type_pal[t]))
          },
          type_labels[t]
        )
      }),
      tags$script(HTML("
        function getLegendEl() { return document.getElementById('graphgraph'); }
        document.addEventListener('mouseover', function(e) {
          var item = e.target.closest('.legend-item[data-node-type]');
          if (!item) return;
          var type = item.getAttribute('data-node-type');
          var el = getLegendEl();
          if (!el || !el.chart || !el.myclick) return;
          var ids = el.chart.body.data.nodes.getIds().filter(function(id) {
            return el.chart.body.data.nodes.get(id).node_type === type;
          });
          el.chart.selectNodes(ids);
          el.myclick({nodes: ids});
          el.chart.body.data.nodes.update(
            el.chart.body.data.nodes.getIds().map(function(id) {
              return {id: id, font: {size: ids.indexOf(id) !== -1 ? 18 : 11}};
            })
          );
        });
        document.addEventListener('mouseout', function(e) {
          var item = e.target.closest('.legend-item[data-node-type]');
          if (!item) return;
          var el = getLegendEl();
          if (!el || !el.chart || !el.myclick) return;
          el.chart.unselectAll();
          el.myclick({nodes: []});
          el.chart.body.data.nodes.update(
            el.chart.body.data.nodes.getIds().map(function(id) { return {id: id, font: {size: 11}}; })
          );
        });
      "))
    ),
    div(
      id = "graph-container",
      visNetworkOutput("graph", width = "100%", height = "100%"),
      div(
        id = "node-card",
        span(
          class = "card-close",
          onclick = "document.getElementById('node-card').classList.remove('visible');",
          "×"
        ),
        uiOutput("node_card_content")
      )
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

  force_running <- reactiveVal(TRUE)

  output$graph <- renderVisNetwork({
    g <- graph_data()
    algo <- input$layout_algo

    if (nrow(g$nodes) == 0) {
      return(visNetwork(
        tibble(id = 1, label = "Aucun nœud"),
        tibble(from = integer(), to = integer())
      ))
    }

    metric <- if (!is.null(input$node_size_metric)) input$node_size_metric else "degree"

    # Forme des nœuds : circularImage pour cptmp/eunicoast, dot sinon
    nodes <- g$nodes |>
      mutate(
        shape = if_else(node_type %in% c("cptmp", "eunicoast"), "circularImage", "dot"),
        font.size = 11,
        value = if (metric == "betweenness") betweenness * 100 + 1 else degree + 1
      )

    edges <- g$edges

    net <- visNetwork(nodes, edges) |>
      visNodes(
        scaling = list(min = 8, max = 30),
        borderWidth = 1.5,
        color = list(border = "white", highlight = list(border = "white")),
        font = list(size = 11, color = "#333333", bold = list(mod = ""))
      ) |>
      visEdges(
        smooth = list(enabled = TRUE, type = "continuous"),
        color  = list(color = "#cccccc", highlight = "#888888", hover = "#888888")
      ) |>
      visOptions(
        highlightNearest = list(
          enabled   = TRUE,
          degree    = 1,
          hover     = TRUE,
          algorithm = "hierarchical"
        )
      ) |>
      visInteraction(
        dragNodes     = TRUE,
        dragView      = TRUE,
        zoomView      = TRUE,
        hover         = TRUE,
        tooltipDelay  = 200
      ) |>
      visEvents(
        selectNode   = js_select_node,
        deselectNode = js_deselect_node,
        hoverNode    = js_hover_node,
        blurNode     = js_blur_node
      )

    if (algo == "forceatlas2") {
      net <- net |>
        visPhysics(
          solver = "forceAtlas2Based",
          forceAtlas2Based = list(
            gravitationalConstant = -input$gravity,
            springLength          = input$spring_length,
            springConstant        = 0.08,
            damping               = 0.4,
            avoidOverlap          = 0.5
          ),
          stabilization = list(enabled = TRUE, iterations = 200)
        )
      force_running(TRUE)
      updateActionButton(session, "toggle_force", label = "⏸")
    } else {
      layout_fn <- switch(algo,
        fr     = igraph::layout_with_fr,
        kk     = igraph::layout_with_kk,
        circle = igraph::layout_in_circle,
        grid   = igraph::layout_on_grid,
        random = igraph::layout_randomly,
        igraph::layout_randomly
      )
      g_igraph <- graph_from_data_frame(
        d        = g$edges |> select(from, to),
        vertices = g$nodes |> select(name = id),
        directed = FALSE
      )
      coords <- layout_fn(g_igraph)
      nodes_with_pos <- nodes |>
        mutate(x = coords[, 1] * 500, y = coords[, 2] * 500)

      net <- visNetwork(nodes_with_pos, edges) |>
        visNodes(
          scaling = list(min = 8, max = 30),
          borderWidth = 1.5,
          color = list(border = "white", highlight = list(border = "white")),
          font = list(size = 11, color = "#333333", bold = list(mod = ""))
        ) |>
        visEdges(
          smooth = list(enabled = TRUE, type = "continuous"),
          color  = list(color = "#cccccc", highlight = "#888888", hover = "#888888")
        ) |>
        visOptions(
          highlightNearest = list(
            enabled   = TRUE,
            degree    = 1,
            hover     = TRUE,
            algorithm = "hierarchical"
          )
        ) |>
        visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE, hover = TRUE) |>
        visEvents(
          selectNode   = js_select_node,
          deselectNode = js_deselect_node
        ) |>
        visPhysics(enabled = FALSE)
    }

    net
  })

  # ── Pause / reprise layout ───────────────────────────────────────────────────

  observeEvent(input$toggle_force, {
    if (force_running()) {
      visNetworkProxy("graph") |> visPhysics(enabled = FALSE)
      force_running(FALSE)
      updateActionButton(session, "toggle_force", label = "▶")
    } else {
      visNetworkProxy("graph") |>
        visPhysics(
          enabled = TRUE,
          solver = "forceAtlas2Based",
          forceAtlas2Based = list(
            gravitationalConstant = -input$gravity,
            springLength          = input$spring_length,
            springConstant        = 0.08,
            damping               = 0.4,
            avoidOverlap          = 0.5
          )
        )
      force_running(TRUE)
      updateActionButton(session, "toggle_force", label = "⏸")
    }
  })

  observeEvent(
    list(input$gravity, input$spring_length),
    {
      if (force_running() && input$layout_algo == "forceatlas2") {
        visNetworkProxy("graph") |>
          visPhysics(
            enabled = TRUE,
            solver = "forceAtlas2Based",
            forceAtlas2Based = list(
              gravitationalConstant = -input$gravity,
              springLength          = input$spring_length,
              springConstant        = 0.08,
              damping               = 0.4,
              avoidOverlap          = 0.5
            )
          )
      }
    },
    ignoreInit = TRUE
  )

  # ── Facteur d'échelle texte ──────────────────────────────────────────────────

  observeEvent(input$font_scale, {
    session$sendCustomMessage("font_scale", list(scale = input$font_scale))
  })

  # ── Carte nœud ──────────────────────────────────────────────────────────────

  selected_node_id <- reactiveVal(NULL)

  observeEvent(input$graph_click_node, {
    nid <- input$graph_click_node$id
    if (!is.null(nid) && !is.na(nid)) {
      selected_node_id(nid)
    } else {
      selected_node_id(NULL)
    }
  })

  observe({
    session$sendCustomMessage("node_card_toggle", list(visible = !is.null(selected_node_id())))
  })

  output$node_card_content <- renderUI({
    nid <- selected_node_id()
    if (is.null(nid)) {
      return(NULL)
    }

    g <- graph_data()
    row_node <- g$nodes[g$nodes$id == nid, ]
    if (nrow(row_node) == 0) {
      return(NULL)
    }

    node_type <- row_node$node_type[1]
    label <- row_node$label[1]

    if (node_type == "chercheur") {
      hid <- proj$hashed_id[proj$full_name == label][1]
      row <- prof[prof$hashed_id == hid, ]
      inst <- if (nrow(row) > 0 && !is.na(row$institution[1]) && row$institution[1] != "") row$institution[1] else NULL
      rq <- if (nrow(row) > 0 && !is.na(row$research_questions[1])) row$research_questions[1] else NULL
      pub_file <- file.path(pub_files, paste0(hid, "_publications.pdf"))
      pub <- if (file.exists(pub_file)) paste0("pub_files/", hid, "_publications.pdf") else NULL

      tagList(
        div(class = "card-name", label),
        if (!is.null(inst)) div(class = "card-inst", inst),
        if (!is.null(rq)) {
          tagList(
            div(class = "card-section", "Questions de recherche"),
            div(class = "card-text", rq)
          )
        },
        if (!is.null(pub)) {
          tagList(
            div(class = "card-section", "Publications"),
            div(
              class = "card-text",
              tags$a(href = pub, target = "_blank", "Voir les publications")
            )
          )
        }
      )
    } else if (node_type == "projet") {
      row <- proj[!is.na(proj$acronym) & proj$acronym == label, ]
      row <- row[1, ]
      title <- if (!is.na(row$title[1]) && row$title[1] != "") row$title[1] else NULL
      abstract <- if (!is.na(row$abstract[1]) && row$abstract[1] != "") row$abstract[1] else NULL

      tagList(
        div(class = "card-name", label),
        if (!is.null(title)) div(class = "card-inst", title),
        if (!is.null(abstract)) {
          tagList(
            div(class = "card-section", "Résumé"),
            div(class = "card-text", abstract)
          )
        }
      )
    } else {
      return(NULL)
    }
  })
  outputOptions(output, "node_card_content", suspendWhenHidden = FALSE)
}

shinyApp(ui, server)
