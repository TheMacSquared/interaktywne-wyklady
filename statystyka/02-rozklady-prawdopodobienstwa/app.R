# Rozklady prawdopodobienstwa - interaktywny przewodnik
# Scrollowalny skrypt z osadzonymi widgetami do nauczania rozkladow prawdopodobienstwa

library(shiny)
library(ggplot2)
library(dplyr)
library(jsonlite)

# ============================================================================
# MODULY
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

# Globalne defaulty ggplot2 — motyw upwr + IBM Plex Sans + kolory geom-ów
lc_apply_ggplot_defaults()

source(file.path(app_dir, "modules", "helpers.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch1_most.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch2_ev_var.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch3_dyskretne.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch4_ciagle.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch5_normalny.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch6_ctg.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch8_quiz.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch9_cwiczenia.R"), local = TRUE)

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch1_ui, ch2_ev_var_ui, ch3_ui, ch4_ui,
                  ch5_ui, ch6_ui, ch8_ui, ch9_ui)

ui <- lecture_page(
  lecture_id    = "rozklady-prawdopodobienstwa",
  lecture_num   = "02",
  lecture_title = "Rozkłady prawdopodobieństwa",
  module_label  = "Statystyka",
  chapters      = .chapters,
  # Chart.js do wykresu dystrybuanty (rozdz. 4, Ryc. 4.3); sceny w scenes.js / scenes.css
  header_extras = tagList(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.js"),
    includeScript(file.path(app_dir, "modules", "cdf_chart.js")),
    includeScript(file.path(app_dir, "modules", "experiment.js")),
    # Sceny „od intuicji do formalizmu” (PROTOTYPY 2026-10-08)
    tags$style(HTML(paste(readLines(file.path(app_dir, "modules", "scenes.css"), warn = FALSE), collapse = "\n"))),
    includeScript(file.path(app_dir, "modules", "scenes.js")),
    tags$style(HTML("
      .lc-exp-svg { display: block; width: 100%; height: auto; }
      .lc-exp-die { fill: var(--upwr-surface); stroke: var(--upwr-ink-subtle); stroke-width: 1.5; }
      .lc-exp-die.is-hit { fill: var(--upwr-accent-tint); stroke: var(--upwr-accent); stroke-width: 3; }
      .lc-exp-pip { fill: var(--upwr-ink); }
      .lc-exp-pip.is-hit { fill: var(--upwr-accent); }
      .lc-exp-q { font: 700 26px var(--upwr-mono); fill: var(--upwr-ink-subtle); }
      .lc-exp-read { font: 700 20px var(--upwr-mono); fill: var(--upwr-accent); }
      .lc-exp-read.is-plain { font: 500 15px var(--upwr-mono); fill: var(--upwr-ink-soft); }
      .lc-exp-sub { font: 13px var(--upwr-sans); fill: var(--upwr-ink-subtle); }
      .lc-exp-log { font: 500 15px var(--upwr-mono); fill: var(--upwr-ink-soft); }
      .lc-exp-log.is-hit { fill: var(--upwr-accent); font-weight: 700; }
      .lc-exp-log.is-x { fill: var(--upwr-ink); font-weight: 700; }
      .lc-exp-grid { stroke: var(--upwr-rule-soft); }
      .lc-exp-axis { stroke: var(--upwr-ink-subtle); }
      .lc-exp-tick { font: 12px var(--upwr-mono); fill: var(--upwr-ink-subtle); }
      .lc-exp-tick.is-x { font-size: 14px; fill: var(--upwr-ink-soft); }
      .lc-exp-val { font: 600 12px var(--upwr-mono); fill: var(--upwr-ink-soft); }
      .lc-exp-bar { fill: var(--upwr-accent); opacity: .85; }
      .lc-exp-theory { fill: var(--upwr-surface); stroke: var(--upwr-ink); stroke-width: 2; }
      .lc-exp-prob { font: 11px var(--upwr-mono); fill: var(--upwr-ink-subtle); }
      .lc-exp-axtitle { font: 13px var(--upwr-sans); fill: var(--upwr-ink-soft); }
      .lc-exp-n { font: 13px var(--upwr-mono); fill: var(--upwr-ink-subtle); }
      .lc-exp-ball { fill: var(--upwr-accent); }
      .lc-stepper-head .lc-toolbar > .lc-grp { flex: 0 0 auto; }
      .lc-stepper-head .lc-toolbar .lc-seg { flex-wrap: nowrap; }
      .lc-exp-cloud { fill: var(--upwr-ink-subtle); opacity: .55; stroke: none; }
      .lc-exp-drop { stroke: var(--upwr-cat-niebo); stroke-width: 2; stroke-linecap: round; }
      .lc-exp-sun { fill: var(--upwr-single-alt); }
      .lc-exp-sunray { stroke: var(--upwr-single-alt); stroke-width: 2.5; stroke-linecap: round; }
      .lc-exp-person { fill: var(--upwr-ink-soft); }
      .lc-exp-scale { fill: var(--upwr-rule); stroke: var(--upwr-ink-subtle); }
      .lc-exp-gauge { fill: none; stroke: var(--upwr-rule); stroke-width: 6; }
      .lc-exp-gaugetick { stroke: var(--upwr-ink-subtle); stroke-width: 1.5; }
      .lc-exp-needle { stroke: var(--upwr-accent); stroke-width: 3; stroke-linecap: round; }
      .lc-exp-needlehub { fill: var(--upwr-accent); }
      .lc-exp-trail { stroke: var(--upwr-accent); stroke-width: 2; opacity: .55; }
      .lc-exp-curve { stroke: var(--upwr-ink); stroke-width: 2.5; }
      .lc-seg button[aria-pressed='true'] { background: var(--upwr-accent); color: var(--upwr-surface); }
      .lc-exp-slot { fill: none; stroke: var(--upwr-rule); stroke-width: 1.5; stroke-dasharray: 3 4; }
      .lc-exp-event { fill: var(--upwr-ink-subtle); stroke: var(--upwr-surface); stroke-width: 1.5; }
      .lc-exp-event.is-hit { fill: var(--upwr-accent); }
    ")),
    # Dwa wykresy obok siebie, gdy panel ma miejsce; pod sobą na telefonie.
    tags$style(HTML("
      .lc-cdf { container-type: inline-size; }
      .lc-cdf-grid { display: grid; grid-template-columns: minmax(0, 1fr); gap: 1.2em; }
      .lc-cdf-canvas { position: relative; aspect-ratio: 1.5 / 1; }
      @container (min-width: 560px) {
        .lc-cdf-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
        .lc-cdf-canvas { aspect-ratio: 1.15 / 1; }
      }
    ")),
    # PROTOTYP dwumian-ciagi / mapa-dyskretnych / poisson-wykladniczy: style rycin 3.2a, 3.8a, 4.4a
    tags$style(HTML("
      .bseq { display: flex; justify-content: center; align-items: flex-end; gap: 1.6em;
              padding: .6em 0 .2em; flex-wrap: wrap; }
      .bseq-col { display: flex; flex-direction: column; align-items: center; }
      .bseq-stack { display: flex; flex-direction: column; justify-content: flex-end; gap: 5px; }
      .bseq-row { display: flex; gap: 3px; }
      .bseq-tile { width: 1.15em; height: 1.15em; border-radius: 3px;
                   background: color-mix(in srgb, var(--upwr-cat-indygo) 22%, var(--upwr-surface));
                   border: 1.5px solid var(--upwr-cat-indygo); box-sizing: border-box; }
      .bseq-tile.is-hit { background: var(--upwr-cat-terakota); border-color: var(--upwr-cat-terakota); }
      .bseq-count { margin-top: .6em; font: 600 1.25em var(--upwr-mono); color: var(--upwr-cat-szalwia); }
      .bseq-k { font: .85em var(--upwr-mono); color: var(--upwr-ink-subtle); }

      .dmap { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 1em; }
      @media (max-width: 720px) { .dmap { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
      .dmap-card { border-top: 4px solid var(--dm-c); padding: .7em .2em 0; display: flex;
                   flex-direction: column; gap: .6em; }
      .dmap-q { font-size: .95em; color: var(--upwr-ink); min-height: 2.6em; }
      .dmap-spark { width: 100%; height: 4.2em; display: block; }
      .dmap-spark rect { fill: var(--dm-c); }
      .dmap-spark .dmap-axis { stroke: var(--upwr-ink-subtle); stroke-width: 1; vector-effect: non-scaling-stroke; }
      .dmap.is-5 { grid-template-columns: repeat(5, minmax(0, 1fr)); }
      .dmap.is-5 .dmap-q { min-height: 4.2em; }
      @media (max-width: 720px) { .dmap.is-5 { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
      .dmap-spark .dmap-area { fill: var(--dm-c); fill-opacity: .35; stroke: none; }
      .dmap-spark .dmap-curve { fill: none; stroke: var(--dm-c); stroke-width: 2; vector-effect: non-scaling-stroke; }
      .dmap-name { font-weight: 600; color: var(--dm-c); }
      .dmap-par { display: block; font: 400 .9em var(--upwr-sans); color: var(--upwr-ink-subtle); }

      .evt { display: block; width: 100%; height: auto; }
      .evt-hour { fill: var(--upwr-surface); }
      .evt-hour.is-alt { fill: var(--upwr-surface-sunken); }
      .evt-axis { stroke: var(--upwr-ink-subtle); stroke-width: 1.5; }
      .evt-dot { fill: var(--upwr-ink); }
      .evt-drop { stroke: var(--upwr-rule); stroke-width: 1; stroke-dasharray: 2 3; }
      .evt-count { font: 700 26px var(--upwr-mono); fill: var(--upwr-cat-szalwia); }
      .evt-gap line { stroke: var(--upwr-cat-terakota); stroke-width: 2.5; stroke-linecap: round; }
      .evt-tick { font: 12px var(--upwr-mono); fill: var(--upwr-ink-subtle); }
      .evt-lab { font: 13px var(--upwr-sans); }
      .evt-lab.is-count { fill: var(--upwr-cat-szalwia); }
      .evt-lab.is-gap { fill: var(--upwr-cat-terakota); }
    "))
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  lc <- lecture_server(.chapters, input, output, session)

  # ==========================================================================
  # CHAPTER SERVERS
  # ==========================================================================

  ch1_server(input, output, session)
  ch2_ev_var_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch6_server(input, output, session)
  ch8_server(input, output, session)
  ch9_server(input, output, session)

}

shinyApp(ui = ui, server = server)
