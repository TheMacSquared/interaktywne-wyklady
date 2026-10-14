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
    includeScript(file.path(app_dir, "modules", "iloczyn.js")),
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
      .lc-il .lc-seg button[aria-pressed='true'] { background: var(--upwr-accent); color: var(--upwr-surface); }
      .lc-il-svg { display: block; width: 100%; max-width: 720px; height: auto; margin: .4em auto .6em; }
      .lc-il-svg text { font-family: var(--upwr-sans); fill: var(--upwr-ink); }
      .lc-il-svg .lc-il-num { font-family: var(--upwr-mono); fill: var(--upwr-ink-soft); }
      .lc-il-edge { stroke: var(--upwr-reference); stroke-width: 2; stroke-linecap: round; }
      .lc-il-edge.is-off { opacity: .35; }
      .lc-il-edge.is-first { stroke: var(--upwr-cat-bursztyn); stroke-width: 5; }
      .lc-il-edge.is-second { stroke: var(--upwr-cat-niebo); stroke-width: 5; }
      .lc-il-pill rect { fill: var(--upwr-surface); stroke: var(--upwr-rule); stroke-width: 1.5; }
      .lc-il-pill text { font-family: var(--upwr-mono) !important; font-weight: 600; }
      .lc-il-pill.is-off { opacity: .45; }
      .lc-il-pill.is-first rect { stroke: var(--upwr-cat-bursztyn); stroke-width: 2.5; }
      .lc-il-pill.is-first text { fill: var(--upwr-cat-bursztyn); }
      .lc-il-pill.is-second rect { stroke: var(--upwr-cat-niebo); stroke-width: 2.5; }
      .lc-il-pill.is-second text { fill: var(--upwr-cat-niebo); }
      .lc-il-node rect { fill: var(--upwr-surface); stroke: var(--upwr-secondary); stroke-width: 1.5; }
      .lc-il-node.is-on rect { stroke: var(--upwr-cat-bursztyn); stroke-width: 2.5; }
      .lc-il-node.is-dim { opacity: .45; }
      .lc-il-node.is-leaf { cursor: pointer; outline: none; }
      .lc-il-node.is-leaf:hover rect { stroke: var(--upwr-accent); }
      .lc-il-node.is-leaf:focus-visible rect { stroke: var(--upwr-cat-niebo); stroke-width: 3; }
      .lc-il-node.is-sel rect { fill: var(--upwr-accent); stroke: var(--upwr-accent); }
      .lc-il-node.is-sel text { fill: var(--upwr-surface); }
      .lc-il .lc-formula-box { margin-left: auto; margin-right: auto; }
      @media (max-width: 600px) {
        .lc-il .lc-formula-box.lc-annot { padding: 14px 8px; column-gap: 6px; }
        .lc-il .lc-annot-symbol { font-size: 15px; white-space: nowrap; }
        .lc-il .lc-annot-value { font-size: 21px; }
        .lc-il .lc-annot-op { font-size: 17px; }
        .lc-il .lc-annot-note { font-size: 12.5px; hyphens: auto; }
      }
      .lc-il-status { margin: .3em 0 0; min-height: 1.5em; color: var(--upwr-ink-soft); font-size: .92em; }
      .lc-ratio { margin: .8em 0; }
      .lc-ratio-svg { display: block; width: 100%; max-width: 640px; height: auto; margin: 0 auto; }
      .lc-ratio figcaption { text-align: center; color: var(--upwr-ink-soft); font-size: .92em; margin-top: .3em; }
      .lc-ratio-omega { fill: var(--upwr-surface); stroke: var(--upwr-ink-subtle); stroke-width: 1.5; }
      .lc-ratio-omega.is-new { stroke: var(--upwr-cat-bursztyn); stroke-width: 2; stroke-dasharray: 6 4; }
      .lc-ratio-b { fill: color-mix(in srgb, var(--upwr-cat-bursztyn) 30%, var(--upwr-surface)); stroke: var(--upwr-cat-bursztyn); stroke-width: 2; }
      .lc-ratio-a { fill: none; stroke: var(--upwr-accent); stroke-width: 2; }
      .lc-ratio-ab { fill: var(--upwr-accent); fill-opacity: .85; stroke: none; }
      .lc-ratio-set { font: italic 700 18px var(--upwr-sans); fill: var(--upwr-ink-soft); }
      .lc-ratio-set.is-b, .lc-ratio-num.is-b { fill: var(--upwr-cat-bursztyn); }
      .lc-ratio-set.is-a, .lc-ratio-num.is-a { fill: var(--upwr-accent); }
      .lc-ratio-num { font: 600 13px var(--upwr-mono); fill: var(--upwr-ink-soft); }
      .lc-ratio-num.is-ab { fill: var(--upwr-surface); }
      .lc-ratio-eq { font: 13px var(--upwr-mono); fill: var(--upwr-ink); }
      .lc-ratio-eq.is-res { font-weight: 700; fill: var(--upwr-accent); }
      .lc-ratio-arrow { stroke: var(--upwr-ink-subtle); stroke-width: 2; }
      .lc-ratio-head { fill: var(--upwr-ink-subtle); }
      .lc-chain2 { margin: 1em 0; }
      .lc-chain2-svg { display: block; width: 100%; max-width: 640px; height: auto; margin: 0 auto; }
      .lc-chain2 figcaption { text-align: center; color: var(--upwr-ink-soft); font-size: .92em; margin-top: .3em; }
      .lc-chain2-node rect { fill: var(--upwr-surface); stroke: var(--upwr-secondary); stroke-width: 1.5; }
      .lc-chain2-node text { font: 15px var(--upwr-sans); fill: var(--upwr-ink); }
      .lc-chain2-node .lc-chain2-n { font: 600 15px var(--upwr-mono); fill: var(--upwr-ink-soft); }
      .lc-chain2-node.is-1 rect { stroke: var(--upwr-cat-bursztyn); stroke-width: 2.5; }
      .lc-chain2-node.is-2 rect { stroke: var(--upwr-cat-niebo); stroke-width: 2.5; }
      .lc-chain2-node.is-leaf rect { fill: var(--upwr-accent); stroke: var(--upwr-accent); }
      .lc-chain2-node.is-leaf text { fill: var(--upwr-surface); }
      .lc-chain2-node.is-off { opacity: .45; }
      .lc-chain2-edge { stroke: var(--upwr-reference); stroke-width: 2; stroke-linecap: round; }
      .lc-chain2-edge.is-off { opacity: .45; }
      .lc-chain2-edge.is-1 { stroke: var(--upwr-cat-bursztyn); stroke-width: 5; }
      .lc-chain2-edge.is-2 { stroke: var(--upwr-cat-niebo); stroke-width: 5; }
      .lc-chain2-edge.is-3 { stroke: var(--upwr-cat-szalwia); stroke-width: 5; }
      .lc-chain2-p { font: 700 14px var(--upwr-mono); }
      .lc-chain2-p.is-1, .lc-chain2-eq .is-1 { fill: var(--upwr-cat-bursztyn); }
      .lc-chain2-p.is-2, .lc-chain2-eq .is-2 { fill: var(--upwr-cat-niebo); }
      .lc-chain2-p.is-3, .lc-chain2-eq .is-3 { fill: var(--upwr-cat-szalwia); }
      .lc-chain2-den { font: 12px var(--upwr-mono); fill: var(--upwr-ink-subtle); }
      .lc-chain2-eq { font: 600 16px var(--upwr-mono); fill: var(--upwr-ink); }
      .lc-chain2-eq .is-res { fill: var(--upwr-accent); font-weight: 700; }
      .lc-cal-year { position: absolute; font-family: var(--upwr-mono); font-weight: 600; color: var(--upwr-ink-soft); line-height: 1; transition: opacity .4s ease; }
      .lc-cal-board.is-sorted .lc-cal-year { opacity: 0; }
      .lc-od-head { display: flex; justify-content: space-between; align-items: baseline; margin: .2em 0 .3em; }
      .lc-od-name-out { font: 600 1.02em var(--upwr-sans); color: var(--upwr-ink); }
      .lc-od-step { font: .8em var(--upwr-mono); color: var(--upwr-ink-subtle); }
      .lc-od-svg { display: block; width: 100%; max-width: 720px; height: auto; margin: 0 auto; }
      .lc-od-person { transition: opacity .4s ease; }
      .lc-od-person.is-off { opacity: .35; }
      .lc-od-face { fill: color-mix(in srgb, var(--upwr-cat-bursztyn) 25%, var(--upwr-surface)); stroke: var(--upwr-ink-soft); stroke-width: 1.5; }
      .lc-od-body { fill: var(--upwr-secondary); }
      .lc-od-body.is-vest { fill: color-mix(in srgb, var(--upwr-cat-kurkuma, #d4a72c) 85%, var(--upwr-surface)); stroke: var(--upwr-ink-soft); stroke-width: 1; }
      .lc-od-tie { fill: var(--upwr-accent); }
      .lc-od-helmet { fill: var(--upwr-cat-bursztyn); }
      .lc-od-name { font: 13px var(--upwr-sans); fill: var(--upwr-ink-soft); }
      .lc-od-person.is-on .lc-od-name { font-weight: 700; fill: var(--upwr-ink); }
      .lc-od-bubble { opacity: 0; transition: opacity .3s ease; }
      .lc-od-person.is-on .lc-od-bubble { opacity: 1; }
      .lc-od-bubble rect, .lc-od-bubble path { fill: var(--upwr-surface); stroke: var(--upwr-ink-soft); stroke-width: 1.5; }
      .lc-od-bubble text { font: 700 17px var(--upwr-sans); fill: var(--upwr-accent); }
      .lc-od-board { fill: color-mix(in srgb, #a07c4a 22%, var(--upwr-surface)); stroke: #a07c4a; stroke-width: 2; transition: stroke .3s ease; }
      .lc-od-svg.is-all .lc-od-board { stroke: var(--upwr-ink); stroke-width: 3; }
      .lc-od-cell rect { fill: color-mix(in srgb, var(--upwr-cat-szalwia) 22%, var(--upwr-surface)); transition: opacity .4s ease; }
      .lc-od-cell.is-hot rect { fill: var(--upwr-cat-bursztyn); }
      .lc-od-cell circle { fill: var(--upwr-accent); }
      .lc-od-cell { transition: opacity .4s ease; }
      .lc-od-cell.is-dim { opacity: .12; }
      .lc-od-cell.is-den rect { stroke: var(--upwr-ink); stroke-width: 1.5; }
      .lc-od-frac { font: 700 24px var(--upwr-mono); fill: var(--upwr-ink); }
      .lc-od-frac .is-num { fill: var(--upwr-accent); }
      .lc-od-caption { min-height: 3em; margin: .5em 0 .3em; color: var(--upwr-ink-soft); }
      .lc-od-nav { display: flex; justify-content: space-between; }
      .lc-od-nav .lc-action[disabled] { opacity: .35; cursor: default; }
      @media (prefers-reduced-motion: reduce) {
        .lc-cal-cell, .lc-cal-month, .lc-cal-day, .lc-cal-group, .lc-od-cell, .lc-od-person, .lc-od-bubble { transition: none !important; }
      }
    "))
  )
)

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)
  warunki_server(input, output, session)
}

shinyApp(ui = ui, server = server)
