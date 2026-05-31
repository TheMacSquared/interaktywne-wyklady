# Projekt badawczy: myślenie przed modelowaniem

library(shiny)
library(ggplot2)
library(dplyr)
library(broom)
library(tidyr)

.find_app_dir <- function() {
  for (i in seq_len(sys.nframe())) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  }
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("--file=", "", file_arg))))
  }
  getwd()
}
app_dir <- .find_app_dir()
project_root <- dirname(app_dir)

source(file.path(project_root, "R", "palette.R"),        local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),     local = TRUE)
source(file.path(project_root, "R", "shared.R"),         local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)
lc_apply_ggplot_defaults()

source(file.path(app_dir, "modules", "helpers.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch1_ciekawosc.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch2_hipotezy.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch3_pomiar.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch4_sprawdzenia.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch5_iteracja.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch6_model_kontrolny.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch7_checklist.R"), local = TRUE)

header_extras <- tagList(
  tags$style(HTML("
    .research-step {
      background: var(--upwr-panel);
      border-left: 4px solid var(--upwr-szalwia);
      padding: 12px 16px;
      margin: 16px 0;
      border-radius: 0 6px 6px 0;
    }
    .research-step .step-number {
      display: inline-flex;
      width: 28px;
      height: 28px;
      border-radius: 50%;
      align-items: center;
      justify-content: center;
      background: var(--upwr-szalwia);
      color: #fff;
      font-weight: 700;
      margin-right: 8px;
    }
    .construct-map {
      display: grid;
      grid-template-columns: repeat(4, minmax(0, 1fr));
      gap: 10px;
      margin: 16px 0;
    }
    .construct-cell {
      background: var(--upwr-surface);
      border: 1px solid var(--upwr-rule);
      border-radius: 8px;
      padding: 12px;
      min-height: 118px;
    }
    .construct-cell h4 {
      margin-top: 0;
      margin-bottom: 8px;
      font-size: calc(14px * var(--lc-font-scale));
      color: var(--upwr-ink);
    }
    .construct-cell p {
      margin: 0;
      font-size: calc(13px * var(--lc-font-scale));
      line-height: 1.45;
      color: var(--upwr-ink-soft);
    }
    .design-option {
      background: var(--upwr-surface);
      border: 1px solid var(--upwr-rule);
      border-radius: 8px;
      padding: 16px;
      margin: 12px 0;
    }
    .question-card {
      background: var(--upwr-surface);
      border: 1px solid var(--upwr-rule);
      border-radius: 8px;
      padding: 16px;
      margin: 12px 0;
    }
    .question-card h4 {
      margin-top: 0;
      margin-bottom: 8px;
      font-size: calc(16px * var(--lc-font-scale));
      color: var(--upwr-ink);
    }
    .research-ladder {
      display: grid;
      grid-template-columns: repeat(3, minmax(0, 1fr));
      gap: 12px;
      margin: 18px 0;
    }
    .research-ladder > div {
      background: var(--upwr-surface);
      border: 1px solid var(--upwr-rule);
      border-radius: 8px;
      padding: 14px;
    }
    .research-ladder strong {
      display: block;
      margin-bottom: 6px;
      color: var(--upwr-ink);
    }
    .two-plot-grid {
      display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 16px;
    }
    .data-legend {
      display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 8px 14px;
      margin-top: 10px;
    }
    .data-legend-item {
      border-left: 3px solid var(--upwr-szalwia);
      background: var(--upwr-surface);
      padding: 8px 10px;
      border-radius: 0 6px 6px 0;
      font-size: calc(13px * var(--lc-font-scale));
      line-height: 1.35;
    }
    .data-legend-item code {
      font-weight: 700;
    }

    /* --- Tablica tropów: narastający widok zbiorczy całej wiązki --- */
    .tropy-board { margin: 16px 0; }
    .tropy-board td, .tropy-board th {
      vertical-align: top;
      color: var(--upwr-ink);
      font-size: calc(13px * var(--lc-font-scale));
      line-height: 1.4;
    }
    .tropy-row-off { opacity: 0.6; }
    .tropy-row-on  { opacity: 1; transition: opacity .25s; }
    .tropy-muted   { color: var(--upwr-ink-subtle); font-style: italic; }
    .tropy-verdict {
      display: inline-block;
      padding: 2px 8px;
      border-radius: 999px;
      font-weight: 700;
      font-size: calc(12px * var(--lc-font-scale));
    }
    .tropy-verdict-on  { background: var(--upwr-sage-tint);   color: var(--upwr-sage); }
    .tropy-verdict-off { background: var(--upwr-accent-tint); color: var(--upwr-accent); }

    /* --- Rozłożone karty tropów: wszystkie hipotezy/wyniki naraz --- */
    .trop-stack { display: grid; gap: 14px; margin: 16px 0; }
    .trop-card {
      background: var(--upwr-surface);
      border: 1px solid var(--upwr-rule);
      border-left: 4px solid var(--upwr-szalwia);
      border-radius: 8px;
      padding: 14px 16px;
    }
    .trop-card h4 {
      margin: 0 0 6px 0;
      font-size: calc(15px * var(--lc-font-scale));
      color: var(--upwr-ink);
    }
    .trop-card p {
      margin: 4px 0;
      font-size: calc(13px * var(--lc-font-scale));
      line-height: 1.45;
      color: var(--upwr-ink-soft);
    }
    .trop-card .trop-alt {
      margin: 6px 0 0 0;
      padding-left: 18px;
    }
    .trop-card .trop-alt li {
      font-size: calc(12.5px * var(--lc-font-scale));
      color: var(--upwr-ink-soft);
      line-height: 1.4;
    }

    @media (max-width: 992px) {
      .construct-map { grid-template-columns: 1fr; }
      .research-ladder { grid-template-columns: 1fr; }
      .two-plot-grid { grid-template-columns: 1fr; }
      .data-legend { grid-template-columns: 1fr; }
    }
  "))
)

# Kolejność: po celu (ch1) od razu tropy (ch2). Model kontrolny (ch6) wchodzi
# przed checklist (ch7), bo checklist domyka cały projekt i musi być ostatni.
# Numery w hero każdego modułu są ustawione zgodnie z TĄ kolejnością (1..7).
.chapters <- list(ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui,
                  ch6_ui, ch7_ui)

ui <- lecture_page(
  lecture_id    = "projekt-badawczy",
  lecture_num   = "12",
  lecture_title = "Projekt badawczy",
  module_label  = "Moduł XII",
  chapters      = .chapters,
  header_extras = header_extras
)

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)

  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch6_server(input, output, session)
  ch7_server(input, output, session)
}

shinyApp(ui = ui, server = server)
