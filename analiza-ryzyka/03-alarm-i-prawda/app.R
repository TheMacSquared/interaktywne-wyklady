# 03-alarm-i-prawda — pełny szkic wykładu

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
source(file.path(app_dir, "modules", "stop_mianowniki.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_szanse.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_prog.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_serpuchow_case.R"), local = TRUE)
source(file.path(app_dir, "modules", "block.R"), local = TRUE)

lc_apply_ggplot_defaults()

.chapters <- alarm_chapters

ui <- lecture_page(
  lecture_id = "alarm-i-prawda",
  lecture_num = "03",
  lecture_title = "Alarm i prawda",
  module_label = "Analiza ryzyka · Bananpol",
  chapters = .chapters,
  header_extras = tagList(
    includeScript(file.path(app_dir, "modules", "serpuchow.js")),
    includeScript(file.path(app_dir, "modules", "notacja.js")),
    tags$style(HTML("
      .lc-pm { font-family: var(--upwr-mono); font-size: .9em; white-space: nowrap;
        padding: .08em .35em; border-radius: 4px;
        background: color-mix(in srgb, var(--upwr-cat-indygo) 9%, var(--upwr-surface)); }
      .lc-chapter :is(p, li):has(.lc-pm) { text-align: left; }
      .lc-pm-p { color: var(--upwr-ink-subtle); }
      .lc-pm-ev { color: var(--upwr-ink); font-weight: 600; }
      .lc-pm-bar { color: var(--upwr-accent); font-weight: 700; padding: 0 .3em; }
      .lc-pm-cond { color: var(--upwr-cat-indygo); font-weight: 600; }
      .lc-sp-head { display: flex; justify-content: space-between; align-items: baseline; margin: .2em 0 .3em; }
      .lc-sp-name { font: 600 1.02em var(--upwr-sans); color: var(--upwr-ink); }
      .lc-sp-step { font: .8em var(--upwr-mono); color: var(--upwr-ink-subtle); }
      .lc-sp-svg { display: block; width: 100%; max-width: 760px; height: auto; margin: 0 auto; }
      .lc-sp-screen { fill: #121a1b; stroke: var(--upwr-secondary); stroke-width: 2; }
      .lc-sp-mono { font: 600 13px var(--upwr-mono); fill: #7fc98f; letter-spacing: .08em; }
      .lc-sp-scanline { stroke: #7fc98f; stroke-opacity: .25; }
      .lc-sp-status { font: 700 15px var(--upwr-mono); letter-spacing: .12em; }
      .lc-sp-status.is-alarm { fill: #e4573d; animation: lc-sp-blink 1s steps(2, start) infinite; }
      .lc-sp-status.is-calm { fill: #7fc98f; }
      @keyframes lc-sp-blink { to { visibility: hidden; } }
      .lc-sp-earth { fill: #1d3329; stroke: #7fc98f; stroke-width: 1.5; }
      .lc-sp-sat { fill: #c9d2cf; }
      .lc-sp-panel { fill: #6a9dc4; }
      .lc-sp-missile { opacity: 0; transition: opacity .5s ease; }
      .lc-sp-missile.is-on { opacity: 1; }
      .lc-sp-missile.is-ghost { opacity: .25; }
      .lc-sp-trail { stroke: #e4573d; stroke-width: 3; stroke-dasharray: 6 4; }
      .lc-sp-tip { fill: #e4573d; }
      .lc-sp-reveal { opacity: 0; transition: opacity .8s ease; }
      .lc-sp-reveal.is-on { opacity: 1; }
      .lc-sp-sun { fill: #e8c25a; }
      .lc-sp-cloud { fill: #dfe6e3; fill-opacity: .85; }
      .lc-sp-ray { stroke: #e8c25a; stroke-width: 1.2; stroke-dasharray: 3 4; }
      .lc-sp-count { font: 600 15px var(--upwr-mono); fill: var(--upwr-ink); }
      .lc-sp-face { fill: color-mix(in srgb, var(--upwr-cat-bursztyn) 25%, var(--upwr-surface)); stroke: var(--upwr-ink-soft); stroke-width: 1.5; }
      .lc-sp-body { fill: var(--upwr-secondary); }
      .lc-sp-who { font: 13px var(--upwr-sans); fill: var(--upwr-ink-soft); }
      .lc-sp-card { opacity: 0; transition: opacity .5s ease; }
      .lc-sp-card.is-on { opacity: 1; }
      .lc-sp-card.is-dim { opacity: .35; }
      .lc-sp-card rect { fill: var(--upwr-surface); stroke: var(--upwr-secondary); stroke-width: 1.5; }
      .lc-sp-card text { font: 600 14px var(--upwr-sans); fill: var(--upwr-ink); }
      .lc-sp-card.is-proc rect { stroke: var(--upwr-accent); stroke-width: 2; }
      .lc-sp-card.is-proc text { fill: var(--upwr-accent); }
      .lc-sp-card.is-decyzja rect { fill: var(--upwr-cat-szalwia); stroke: var(--upwr-cat-szalwia); }
      .lc-sp-card.is-decyzja text { fill: #ffffff; }
      .lc-sp-caption { min-height: 3em; margin: .5em 0 .3em; color: var(--upwr-ink-soft); }
      .lc-sp-nav { display: flex; justify-content: space-between; }
      .lc-sp-nav .lc-action[disabled] { opacity: .35; cursor: default; }
      .a3-tree-svg { display: block; width: 100%; max-width: 760px; height: auto; margin: .2em auto .4em; }
      .a3-tree-svg text { font-family: var(--upwr-sans); fill: var(--upwr-ink); }
      .a3-tree-svg .a3-tree-p { font: 13px var(--upwr-mono); fill: var(--upwr-ink-soft); }
      .a3-tree-svg .a3-tree-edge { stroke: var(--upwr-reference); stroke-width: 2; fill: none; }
      .a3-tree-svg .a3-tree-edge.is-dim { opacity: .4; }
      .a3-tree-svg .a3-tree-node rect { fill: var(--upwr-surface); stroke: var(--upwr-secondary); stroke-width: 1.5; }
      .a3-tree-svg .a3-tree-node.is-dim { opacity: .45; }
      .a3-tree-svg .a3-tree-node text { font: 600 15px var(--upwr-sans); }
      .a3-tree-svg .a3-tree-brace { stroke: var(--upwr-ink-soft); stroke-width: 1.5; fill: none; }
      .a3-tree-svg .a3-tree-sum { font: 600 15px var(--upwr-mono); }
      @media (prefers-reduced-motion: reduce) {
        .lc-sp-svg *, .lc-sp-status.is-alarm { transition: none !important; animation: none !important; }
      }
    "))
  )
)

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)
  alarm_server(input, output, session)
}

shinyApp(ui = ui, server = server)
