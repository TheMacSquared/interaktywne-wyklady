# Case studies - analizy statystyczne od A do Z
# Każdy rozdział to kompletna analiza jednego zbioru danych

library(shiny)
library(ggplot2)
library(dplyr)
library(rstatix)
library(broom)
library(tidyr)
library(AER)

# ============================================================================
# MODUŁY
# ============================================================================

.find_app_dir <- function() {
  # Katalog aplikacji rozpoznajemy po tym, że jego rodzic zawiera R/lecture_layout.R.
  has_project_root <- function(dir) {
    file.exists(file.path(dirname(dir), "R", "lecture_layout.R"))
  }

  candidates <- character(0)
  # 1) ofile w stosie wywołań (source())
  for (i in seq_len(sys.nframe())) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) candidates <- c(candidates, dirname(normalizePath(ofile)))
  }
  # 2) Rscript --file=...
  file_arg <- grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    candidates <- c(candidates, dirname(normalizePath(sub("--file=", "", file_arg[[1]]))))
  }
  # 3) Katalog roboczy — shiny::runApp() ustawia go na katalog aplikacji.
  candidates <- c(candidates, getwd())

  # Pierwszy kandydat leżący w projekcie; gdy żaden nie pasuje, zachowaj stare zachowanie.
  # Bez tego uruchomienie przez wrapper (np. rozszerzenie Shiny dla VS Code) trafia do
  # katalogu wrappera, bo --file= wskazuje jego skrypt, a nie app.R.
  valid <- Filter(has_project_root, candidates)
  if (length(valid) > 0) valid[[1]] else candidates[[1]]
}
app_dir <- .find_app_dir()

project_root <- dirname(app_dir)

source(file.path(project_root, "R", "palette.R"),        local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),     local = TRUE)
source(file.path(project_root, "R", "shared.R"),         local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)

lc_apply_ggplot_defaults()

source(file.path(app_dir, "modules", "helpers.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch1_caschools.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch2_pingwiny.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch3_titanic.R"),   local = TRUE)

# Style mapy przebiegu analizy i schematu zakłócenia.
case_extras <- tags$style(HTML("
  .case-pipeline {
    list-style: none; margin: 12px 0 18px; padding: 0;
    display: grid; grid-auto-flow: column; grid-auto-columns: minmax(0, 1fr);
    gap: 14px; counter-reset: none;
  }
  .cp-step {
    position: relative; display: flex; flex-direction: column; gap: 3px;
    border-top: 4px solid var(--cp-c); padding: 8px 2px 0;
  }
  .cp-step:not(:last-child)::after {
    content: '›'; position: absolute; right: -12px; top: 2px;
    font: 400 22px/1 var(--upwr-sans); color: var(--upwr-ink-subtle);
  }
  .cp-num  { font: 600 13px/1 var(--upwr-mono); color: var(--cp-c); }
  .cp-name { font-weight: 600; font-size: 15px; line-height: 1.2; color: var(--upwr-ink); }
  .cp-tools { font-size: 13px; line-height: 1.3; color: var(--upwr-ink-soft); }
  .cp-ref  { font: 12px/1.2 var(--upwr-mono); color: var(--upwr-ink-subtle); }
  .case-pipeline.is-mini { gap: 4px; margin: 4px 0 14px; }
  .case-pipeline.is-mini .cp-step { border-top-width: 3px; padding-top: 5px; opacity: .45; }
  .case-pipeline.is-mini .cp-step::after { content: none; }
  .case-pipeline.is-mini .cp-step.is-done { opacity: .7; }
  .case-pipeline.is-mini .cp-step.is-current { opacity: 1; border-top-width: 6px; }
  .case-pipeline.is-mini .cp-name { font-size: 12.5px; font-weight: 500; }
  .case-pipeline.is-mini .is-current .cp-name { font-weight: 700; }
  @media (max-width: 700px) {
    .case-pipeline:not(.is-mini) { grid-auto-flow: row; gap: 8px; }
    .case-pipeline:not(.is-mini) .cp-step { border-top: 0; border-left: 4px solid var(--cp-c);
      padding: 2px 0 2px 10px; }
    .case-pipeline:not(.is-mini) .cp-step::after { content: none; }
    .case-pipeline.is-mini .cp-name { display: none; }
  }

  .case-confounder { display: block; width: 100%; max-width: 560px; height: auto; margin: 4px auto; }
  .cf-node rect { fill: color-mix(in srgb, var(--cf-c) 16%, var(--upwr-surface));
    stroke: var(--cf-c); stroke-width: 2; }
  .cf-node text { fill: var(--upwr-ink); font: 600 17px var(--upwr-sans); }
  .cf-edge { stroke: var(--upwr-ink-soft); stroke-linecap: butt; }
  .cf-edge.is-open { stroke-dasharray: 7 6; stroke: var(--upwr-ink-subtle); }
  .cf-head { fill: var(--upwr-ink-soft); }
  .cf-val { fill: var(--upwr-ink); font: 500 15px var(--upwr-mono);
    paint-order: stroke; stroke: var(--upwr-surface); stroke-width: 5px; stroke-linejoin: round; }
"))

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch1_ui, ch2_ui, ch3_ui)

ui <- lecture_page(
  lecture_id    = "case-studies",
  lecture_num   = "08",
  lecture_title = "Case studies",
  module_label  = "Statystyka",
  chapters      = .chapters,
  header_extras = case_extras
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)
  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
}

shinyApp(ui = ui, server = server)
