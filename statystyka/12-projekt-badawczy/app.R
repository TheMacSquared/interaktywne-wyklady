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
source(file.path(app_dir, "modules", "ch2_pytanie.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch3_hipotezy.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch4_pomiar.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch5_sprawdzenia.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch6_iteracja.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch_projekt_badania.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch7_checklist.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch8_dodatek_model.R"), local = TRUE)

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
    @media (max-width: 992px) {
      .construct-map { grid-template-columns: 1fr; }
      .research-ladder { grid-template-columns: 1fr; }
      .two-plot-grid { grid-template-columns: 1fr; }
      .data-legend { grid-template-columns: 1fr; }
    }
  "))
)

.chapters <- list(ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui, ch6_ui,
                  ch_projekt_ui, ch7_ui, ch8_ui)

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
  ch_projekt_server(input, output, session)
  ch7_server(input, output, session)
  ch8_server(input, output, session)
}

shinyApp(ui = ui, server = server)
