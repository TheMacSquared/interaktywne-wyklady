# 02-warunki — pełny szkic wykładu

library(shiny)
library(ggplot2)
library(dplyr)

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

source(file.path(project_root, "R", "palette.R"), local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"), local = TRUE)
source(file.path(project_root, "R", "shared.R"), local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)
source(file.path(project_root, "R", "bananpol.R"), local = TRUE)
source(file.path(project_root, "R", "risk_math.R"), local = TRUE)
source(file.path(project_root, "R", "risk_block.R"), local = TRUE)
source(file.path(app_dir, "modules", "monty_server.R"), local = TRUE)
source(file.path(app_dir, "modules", "block.R"), local = TRUE)

lc_apply_ggplot_defaults()

.chapters <- warunki_chapters

ui <- lecture_page(
  lecture_id = "warunki",
  lecture_num = "02",
  lecture_title = "Warunki zmieniają ocenę",
  module_label = "Analiza ryzyka · Bananpol",
  chapters = .chapters,
  header_extras = tagList(
    includeScript(file.path(app_dir, "modules", "filtr.js")),
    tags$style(HTML("
      .lc-cal-board { position: relative; width: 100%; margin: .4em 0 .2em; }
      .lc-cal-cell { position: absolute; left: 0; top: 0; border-radius: 2px;
        background: color-mix(in srgb, var(--upwr-cat-szalwia) 22%, var(--upwr-surface));
        transition: transform .7s cubic-bezier(.2, .7, .2, 1), opacity .45s ease; }
      .lc-cal-cell.is-hot { background: var(--upwr-cat-bursztyn); }
      .lc-cal-cell.is-inc::after { content: ''; position: absolute; inset: 22%; border-radius: 50%;
        background: var(--upwr-accent); }
      .lc-cal-board.is-instant .lc-cal-cell { transition: none !important; }
      .lc-cal-month, .lc-cal-day, .lc-cal-group { position: absolute; white-space: nowrap;
        font-family: var(--upwr-mono); color: var(--upwr-ink-subtle); line-height: 1; }
      .lc-cal-month { top: 0; }
      .lc-cal-day { left: 0; display: flex; align-items: center; }
      .lc-cal-month, .lc-cal-day { transition: opacity .4s ease; }
      .lc-cal-board.is-sorted .lc-cal-month, .lc-cal-board.is-sorted .lc-cal-day { opacity: 0; }
      .lc-cal-group { font-weight: 600; opacity: 0; transition: opacity .4s ease .3s; }
      .lc-cal-group.is-hot { color: var(--upwr-cat-bursztyn); }
      .lc-cal-group.is-cold { transform: translateX(-100%); }
      .lc-cal-board.is-sorted .lc-cal-group { opacity: 1; }
      .lc-cal .lc-action[aria-pressed='true'] { background: var(--upwr-surface); color: var(--upwr-accent); }
      .lc-cal .lc-read[hidden] { display: none; }
      .lc-cal-status { margin: .3em 0 0; min-height: 1.5em; color: var(--upwr-ink-soft); }
      .lc-mz-svg { display: block; width: 100%; max-width: 680px; height: auto; margin: .4em auto 0; }
      .lc-mz-grid { stroke: var(--upwr-rule); stroke-width: 1; }
      .lc-mz-tick { font: 11px var(--upwr-mono); fill: var(--upwr-ink-subtle); }
      .lc-mz-axis { font: 12px var(--upwr-mono); fill: var(--upwr-ink-soft); }
      .lc-mz-hot { fill: color-mix(in srgb, var(--upwr-cat-bursztyn) 45%, var(--upwr-surface)); }
      .lc-mz-cold { fill: color-mix(in srgb, var(--upwr-cat-szalwia) 22%, var(--upwr-surface)); }
      .lc-mz-inc { fill: var(--upwr-accent); }
      .lc-mz-sep { stroke: var(--upwr-surface); stroke-width: 2; }
      .lc-mz-ref { stroke: var(--upwr-ink); stroke-width: 1.5; stroke-dasharray: 6 4; }
      .lc-mz-lab { font: 600 12px var(--upwr-mono); fill: var(--upwr-ink-soft); }
      .lc-mz-lab.is-hot { fill: var(--upwr-cat-bursztyn); }
      @media (prefers-reduced-motion: reduce) {
        .lc-cal-cell, .lc-cal-month, .lc-cal-day, .lc-cal-group { transition: none !important; }
      }
    "))
  )
)

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)
  warunki_server(input, output, session)
}

shinyApp(ui = ui, server = server)
