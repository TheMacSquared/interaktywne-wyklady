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
source(file.path(app_dir, "modules", "stop_detektory.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_serwis.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_przyklady.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_zrodla.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_dwiehale.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_drugi.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_sygnaly.R"), local = TRUE)
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
    includeScript(file.path(app_dir, "modules", "dziennik.js")),
    includeScript(file.path(app_dir, "modules", "telefon.js")),
    includeScript(file.path(app_dir, "modules", "wzory.js")),
    includeScript(file.path(app_dir, "modules", "drzewo.js")),
    includeScript(file.path(app_dir, "modules", "serwis.js")),
    includeScript(file.path(app_dir, "modules", "przyklady.js")),
    includeScript(file.path(app_dir, "modules", "dwiehale.js")),
    includeScript(file.path(app_dir, "modules", "drugi.js")),
    includeScript(file.path(app_dir, "modules", "sygnaly.js")),
    tags$style(HTML("
      .lc-pm { font-family: var(--upwr-mono); font-size: .9em; white-space: nowrap;
        padding: .08em .35em; border-radius: 4px;
        background: color-mix(in srgb, var(--upwr-cat-indygo) 9%, var(--upwr-surface)); }
      .lc-chapter :is(p, li):has(.lc-pm) { text-align: left; }
      .lc-pm-p { color: var(--upwr-ink-subtle); }
      .lc-pm-ev { color: var(--upwr-ink); font-weight: 600; }
      .lc-pm-bar { color: var(--upwr-accent); font-weight: 700; padding: 0 .3em; }
      .lc-pm-cond { color: var(--upwr-cat-indygo); font-weight: 600; }
      .lc-tf-clock { font: 700 34px var(--upwr-mono); fill: #7fc98f; letter-spacing: .06em; }
      .lc-tf-limit { stroke: #e4573d; stroke-width: 1.5; stroke-dasharray: 6 4; }
      .lc-tf-axis { font: 12px var(--upwr-mono); fill: #e4573d; }
      .lc-tf-line { fill: none; stroke: #7fc98f; stroke-width: 2.5; }
      .lc-tf-dot { fill: #e4573d; }
      .lc-tf-banana { fill: #f2cf4a; stroke: #8a6d1a; stroke-width: 1.2; stroke-linejoin: round; }
      .lc-tf-tip, .lc-tf-stem { fill: #5c4a1e; }
      .lc-tf-phone { fill: var(--upwr-ink-soft); }
      .lc-tf-card { opacity: 0; transition: opacity .5s ease; }
      .lc-tf-card.is-on { opacity: 1; }
      .lc-tf-card rect { fill: var(--upwr-surface); stroke-width: 2; }
      .lc-tf-num { font: 700 24px var(--upwr-mono); }
      .lc-tf-unit { font: 11px var(--upwr-mono); fill: var(--upwr-ink-subtle); }
      .lc-tf-text { font: 600 12.5px var(--upwr-sans); fill: var(--upwr-ink); }
      .lc-tf-term { font: 12px var(--upwr-sans); fill: var(--upwr-ink-subtle); }
      .lc-tf-card.is-base rect { stroke: var(--upwr-accent); fill: var(--upwr-accent-tint); }
      .lc-tf-card.is-base .lc-tf-num { fill: var(--upwr-accent); }
      .lc-tf-card.is-sens rect { stroke: var(--upwr-accent); }
      .lc-tf-card.is-sens .lc-tf-num { fill: var(--upwr-accent); }
      .lc-tf-card.is-fpr rect { stroke: var(--upwr-single-alt); }
      .lc-tf-card.is-fpr .lc-tf-num { fill: var(--upwr-single-alt); }
      .lc-tf-card.is-ask rect { stroke: var(--upwr-ink); stroke-dasharray: 6 4; }
      .lc-tf-card.is-ask .lc-tf-num { fill: var(--upwr-ink); }
      .lc-wz-body { display: grid; grid-template-columns: minmax(0, 1.05fr) minmax(0, 1fr); gap: 28px; align-items: center; margin: .4em 0 .6em; }
      @media (max-width: 760px) { .lc-wz-body { grid-template-columns: 1fr; } }
      .lc-wz-grid { display: grid; grid-template-columns: auto 1fr 1fr; gap: 8px; }
      .lc-wz-head { text-align: center; font: 600 14px var(--upwr-sans); color: var(--upwr-ink-soft); padding-bottom: 2px; }
      .lc-wz-side { display: flex; align-items: center; justify-content: flex-end; text-align: right; font: 600 14px var(--upwr-sans); color: var(--upwr-ink-soft); padding-right: 4px; }
      .lc-wz-cell { border-radius: 8px; padding: 12px 10px; text-align: center; border: 3px solid var(--upwr-rule);
        background: var(--upwr-surface); transition: background .35s ease, border-color .35s ease, opacity .35s ease; }
      .lc-wz-cell .lc-wz-abbr { font: 700 22px var(--upwr-mono); color: var(--upwr-ink); }
      .lc-wz-cell .lc-wz-n { font: 600 16px var(--upwr-mono); color: var(--upwr-ink-soft); }
      .lc-wz-cell .lc-wz-desc { font: 12px var(--upwr-sans); color: var(--upwr-ink-subtle); }
      .lc-wz-cell.is-off { opacity: .35; }
      .lc-wz-cell.is-den { border-color: var(--upwr-cat-kurkuma); background: color-mix(in srgb, var(--upwr-cat-kurkuma) 22%, var(--upwr-surface)); }
      .lc-wz-cell.is-num { background: color-mix(in srgb, var(--upwr-cat-niebo) 45%, var(--upwr-surface)); }
      .lc-wz-formula { display: flex; flex-direction: column; gap: 14px; }
      .lc-wz-row { display: flex; align-items: center; gap: 12px; flex-wrap: wrap; }
      .lc-wz-symbol { font-size: 1.15em; }
      .lc-wz-eq { font: 600 22px var(--upwr-mono); color: var(--upwr-ink-soft); }
      .lc-wz-frac { display: inline-flex; flex-direction: column; align-items: center; gap: 5px; }
      .lc-wz-bar { align-self: stretch; height: 2px; background: var(--upwr-ink); }
      .lc-wz-sum { display: inline-flex; align-items: center; gap: 5px; }
      .lc-wz-op { font: 600 16px var(--upwr-mono); color: var(--upwr-ink-soft); }
      .lc-wz-chip { font: 700 17px var(--upwr-mono); padding: 2px 8px; border-radius: 5px; color: var(--upwr-ink); }
      .lc-wz-chip.is-num { background: color-mix(in srgb, var(--upwr-cat-niebo) 45%, var(--upwr-surface)); }
      .lc-wz-chip.is-den { background: color-mix(in srgb, var(--upwr-cat-kurkuma) 30%, var(--upwr-surface)); box-shadow: inset 0 0 0 2px var(--upwr-cat-kurkuma); }
      .lc-wz-value { font: 700 24px var(--upwr-mono); color: var(--upwr-ink); }
      .lc-wz-note { margin: 0; color: var(--upwr-ink-soft); }
      .lc-dz-svg { display: block; width: 100%; max-width: 820px; height: auto; margin: 0 auto; }
      .lc-dz-book { fill: var(--upwr-surface); stroke: var(--upwr-rule); stroke-width: 1.5; }
      .lc-dz-cell { fill: color-mix(in srgb, var(--upwr-ink-subtle) 22%, var(--upwr-surface));
        transition: transform 1.1s cubic-bezier(.2, .7, .2, 1), opacity .6s ease, fill .4s ease; }
      .lc-dz-cell.is-event { fill: var(--upwr-accent-tint); stroke: var(--upwr-accent); stroke-width: 1.2; }
      .lc-dz-cell.is-hit { fill: var(--upwr-accent); }
      .lc-dz-cell.is-false { fill: var(--upwr-single-alt); }
      .lc-dz-cell.is-faded { opacity: .12; }
      .lc-dz-pile { font: 600 15px var(--upwr-sans); opacity: 0; transition: opacity .5s ease .7s; }
      .lc-dz-pile.is-true { fill: var(--upwr-accent); }
      .lc-dz-pile.is-false { fill: var(--upwr-single-alt); }
      .lc-dz-svg.is-piles .lc-dz-pile { opacity: 1; }
      .lc-dz-readout { font: 600 15px var(--upwr-mono); color: var(--upwr-ink); margin: .2em 0 .6em; min-height: 1.4em; }
      .a3-tree-svg [data-k='b-quiet'] { display: none; }
      .a3-tree-svg.is-quiet [data-k='b-quiet'] { display: inline; }
      .a3-tree-svg [data-k] { transition: opacity .35s ease; }
      .a3-tree-svg.is-stepped [data-k]:not(.is-lit) { opacity: .15; }
      .a3-tree-svg.is-stepped [data-k].is-lit { opacity: 1; }
      .a3-tree-svg.is-quiet .a3-tree-edge.is-lit[data-k='e-miss'],
      .a3-tree-svg.is-quiet .a3-tree-edge.is-lit[data-k='e-tn'] { stroke: var(--upwr-cat-szalwia) !important; stroke-width: 8px !important; }
      .a3-tree-svg.is-quiet .a3-tree-node.is-lit[data-k='n-miss'] rect,
      .a3-tree-svg.is-quiet .a3-tree-node.is-lit[data-k='n-tn'] rect { stroke: var(--upwr-cat-szalwia); stroke-width: 2.5; }
      .lc-tr-lines { display: flex; flex-direction: column; gap: 8px; margin: .3em 0 .8em; min-height: 6em; }
      .lc-tr-line { display: flex; flex-wrap: wrap; align-items: baseline; gap: .2em 1em; opacity: .5; }
      .lc-tr-line.is-new { opacity: 1; }
      .lc-tr-tex { font: 600 18px var(--upwr-mono); color: var(--upwr-ink); }
      .lc-tr-note { color: var(--upwr-ink-subtle); font-size: .92em; }
      @media (prefers-reduced-motion: reduce) { .lc-dz-svg * { transition: none !important; } }
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
