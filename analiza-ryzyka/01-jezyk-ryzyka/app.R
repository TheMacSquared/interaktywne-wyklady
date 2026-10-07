# Język ryzyka — interaktywny wykład
# Od zagrożenia i ekspozycji do zdarzeń oraz prawdopodobieństwa.

library(shiny)
library(ggplot2)
library(dplyr)

# ==========================================================================
# BOOTSTRAP PROJEKTU
# ==========================================================================

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
source(file.path(project_root, "R", "bananpol.R"),       local = TRUE)
source(file.path(project_root, "R", "risk_block.R"),     local = TRUE)

lc_apply_ggplot_defaults()

# ==========================================================================
# MODUŁY
# ==========================================================================

source(file.path(app_dir, "modules", "helpers.R"), local = TRUE)
source(file.path(app_dir, "modules", "block.R"),   local = TRUE)

.chapters <- jezyk_chapters

ui <- lecture_page(
  lecture_id    = "jezyk-ryzyka",
  lecture_num   = "01",
  lecture_title = "Od zagrożenia do prawdopodobieństwa",
  module_label  = "Analiza ryzyka · Bananpol",
  chapters      = .chapters,
  header_extras = tagList(
    includeScript(file.path(app_dir, "modules", "omega.js")),
    tags$style(HTML("
      .lc-om-stage { margin: .6em 0 .2em; }
      .lc-om .lc-seg button[aria-pressed='true'] { background: var(--upwr-accent); color: var(--upwr-surface); }
      .lc-om-svg { display: block; width: 100%; max-width: 760px; height: auto; margin: 0 auto; }
      .lc-om-omega { fill: var(--upwr-panel); stroke: var(--upwr-ink-subtle); stroke-width: 1.5; }
      .lc-om-gap { fill: var(--upwr-panel); }
      .lc-om-gate { font: 12px var(--upwr-mono); fill: var(--upwr-ink-subtle); }
      .lc-om-set { font: italic 700 22px var(--upwr-sans); fill: var(--upwr-ink-soft); }
      .lc-om-setsub { font: 12px var(--upwr-mono); fill: var(--upwr-ink-subtle); }
      .lc-om-set.is-a, .lc-om-setsub.is-a { fill: var(--upwr-accent); }
      .lc-om-set.is-c, .lc-om-setsub.is-c { fill: var(--upwr-cat-szalwia); }
      .lc-om .lc-reads { margin: .2em 0 0; }
      .lc-om-stepper .lc-om-count { min-width: 2.6em; justify-content: center; font-weight: 700; cursor: default; }
      .lc-om-stepper button { font-size: 1.05em; min-width: 2.4em; justify-content: center; }
      .lc-om-a { fill: none; stroke: var(--upwr-accent); stroke-width: 2; stroke-dasharray: 6 4; }
      .lc-om-box { fill: #d8b98a; stroke: #a07c4a; stroke-width: 1; }
      .lc-om-wood { fill: #a07c4a; }
      .lc-om-strap { stroke: var(--upwr-ink-soft); stroke-width: 2.5; stroke-linecap: round; }
      .lc-om-strap.is-bad { stroke: var(--upwr-cat-terakota); }
      .lc-om-num { font: 600 11px var(--upwr-mono); fill: var(--upwr-ink); }
      .lc-om-heat { fill: var(--upwr-cat-niebo); }
      .lc-om-mark { fill: none; stroke: var(--upwr-ink-subtle); stroke-width: 2; }
      .lc-om-mark.is-final { stroke: var(--upwr-ink); stroke-width: 3; }
      .lc-om-omega-l { font: italic 700 15px var(--upwr-sans); fill: var(--upwr-ink); }
      .lc-om-status { margin: .3em 0 0; min-height: 1.5em; color: var(--upwr-ink-soft); }
      .lc-om-status.is-hit { color: var(--upwr-accent); }
    "))
  )
)

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)
  jezyk_server(input, output, session)
}

shinyApp(ui = ui, server = server)
