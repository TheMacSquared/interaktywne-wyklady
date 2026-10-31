# Statystyka opisowa - interaktywny przewodnik
# Scrollowalny skrypt z osadzonymi widgetami do nauczania statystyk opisowych

library(shiny)
library(ggplot2)
library(dplyr)
library(e1071)  # for skewness, kurtosis
library(gridExtra)
library(jsonlite)

# ============================================================================
# ETYKIETY TYPÓW ZMIENNYCH
# (kolory: type_colors z R/palette.R)
# ============================================================================

type_labels <- c(
  "nominalna"           = "Jakościowa nominalna",
  "porzadkowa"          = "Jakościowa porządkowa",
  "ilosciowa_dyskretna" = "Ilościowa dyskretna",
  "ilosciowa_ciagla"    = "Ilościowa ciągła"
)

# ============================================================================
# MODULY
# ============================================================================

# Ustal katalog aplikacji niezaleznie od sposobu uruchomienia
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

source(file.path(project_root, "R", "palette.R"),          local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),       local = TRUE)
source(file.path(project_root, "R", "shared.R"),           local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"),   local = TRUE)
source(file.path(project_root, "R", "ankieta.R"),          local = TRUE)

# Globalne defaulty ggplot2 — motyw + kolory geom-ów (upwr_single = burgund)
lc_apply_ggplot_defaults()

source(file.path(app_dir, "modules", "helpers.R"),        local = TRUE)
source(file.path(app_dir, "modules", "scene_helpers.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch1_typy.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch2_jakosciowe.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch3_polozenie.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch4_rozrzut.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch5_ksztalt.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch7_quiz.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch8_cwiczenia.R"),  local = TRUE)

# ============================================================================
# GLOBAL UI HEADER (CSS, JS, Chart.js)
# ============================================================================

# App-specyficzne extras — przekazywane do lecture_page() jako header_extras
# (CSS i JS layoutu są już inkludowane przez lecture_page)
app_extras <- tagList(
  # Sceny SVG (autobusy w rozdz. 4, Ryc. 4.1)
  tags$style(HTML(paste(readLines(file.path(app_dir, "modules", "scenes.css"), warn = FALSE), collapse = "\n"))),
  includeScript(file.path(app_dir, "modules", "scenes.js")),
  tags$style(HTML("
    .lc-stepper-head .lc-toolbar:has([data-sc-act]) > .lc-grp { flex: 0 0 auto; }
    .lc-stepper-head .lc-toolbar:has([data-sc-act]) .lc-seg { flex-wrap: nowrap; }
    .lc-seg button[data-sc-opt][aria-pressed='true'] { background: var(--upwr-accent); color: var(--upwr-surface); }
  ")),
  # Szósta obserwacja we wzorach (rozdz. 3, Ryc. 3.5)
  tags$style(HTML("
    .ch3-six-row { display: flex; flex-wrap: wrap; gap: 8px; margin: 12px 0; }
    .ch3-six-cell { display: flex; flex-direction: column; align-items: center; gap: 2px;
      min-width: 76px; padding: 6px 10px; border: 1px solid var(--upwr-rule);
      background: var(--upwr-surface); }
    .ch3-six-idx { font: 12px var(--upwr-mono); color: var(--upwr-ink-subtle); }
    .ch3-six-val { font: 600 16px var(--upwr-mono); color: var(--upwr-ink); }
    .ch3-six-cell.is-input { border-style: dashed; border-color: var(--upwr-accent); }
    .ch3-six-cell.is-input .form-control { width: 96px; padding: 2px 6px; text-align: center;
      font: 600 16px var(--upwr-mono); }
    .ch3-six-cell.is-mid { border-color: var(--upwr-cat-indygo); border-width: 2px;
      background: color-mix(in srgb, var(--upwr-cat-indygo) 12%, var(--upwr-surface)); }
    .ch3-six-cell.is-new .ch3-six-val, .ch3-six-new { color: var(--upwr-accent); }
    .ch3-six-label { margin-top: 14px; font-size: 14px; color: var(--upwr-ink-soft); }
    .ch3-six-label + .lc-formula-box { overflow-x: auto; }
    .ch3-six-row.is-sorted + .lc-formula-box { overflow-x: auto; }
  ")),
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.js"),
  includeScript(file.path(project_root, "R", "lc_charts.js")),
  # Ryciny liczone w przeglądarce (Chart.js): Ryc. 3.4, 3.6, 3.9, 3.10, 5.5
  includeScript(file.path(app_dir, "modules", "chart_utils.js")),
  includeScript(file.path(app_dir, "modules", "salary_chart.js")),
  includeScript(file.path(app_dir, "modules", "percentile_chart.js")),
  includeScript(file.path(app_dir, "modules", "guess_chart.js")),
  includeScript(file.path(app_dir, "modules", "kurtosis_chart.js")),
  tags$style(HTML("
    .lc-salary-hist, .lc-pctl-hist { position: relative; width: 100%; aspect-ratio: 2.6 / 1; max-height: 300px; touch-action: none; }
    .lc-salary-strip, .lc-pctl-box { position: relative; width: 100%; aspect-ratio: 6 / 1; min-height: 92px; max-height: 130px; touch-action: none; }
    .lc-guess-canvas { position: relative; width: 100%; aspect-ratio: 1.9 / 1; max-height: 360px; touch-action: none; margin-top: .4em; }
    .lc-kurt-main { position: relative; width: 100%; aspect-ratio: 2 / 1; max-height: 340px; touch-action: none; }
    .lc-kurt-tail { position: relative; width: 100%; aspect-ratio: 3 / 1; max-height: 210px; }
    @media (max-width: 640px) {
      .lc-salary-hist, .lc-pctl-hist { aspect-ratio: 1.7 / 1; }
      .lc-salary-strip, .lc-pctl-box { aspect-ratio: 3.6 / 1; }
      .lc-guess-canvas, .lc-kurt-main { aspect-ratio: 1.4 / 1; }
      .lc-kurt-tail { aspect-ratio: 2 / 1; }
    }
    .lc-salary .lc-tbl-block { margin-top: 1em; }
    .lc-salary .lc-action:disabled, .lc-guess .lc-action:disabled { opacity: .35; }
    .lc-pctl .lc-seg button[aria-pressed='true'], .lc-kurt .lc-seg button[aria-pressed='true'] {
      background: var(--upwr-accent); color: var(--upwr-surface); }
    .lc01-line-sw { display: inline-block; width: 1.5em; height: 0; margin-right: .4em;
      border-top: 3px solid var(--lc-sw); vertical-align: middle; }
    .lc01-line-sw.is-dashed { border-top-style: dashed; }
    .lc01-line-sw.is-dotted { border-top-style: dotted; }
  ")),
  includeScript(file.path(app_dir, "modules", "scenario_chart.js")),
  tags$style(HTML("

  /* Taxonomy tree (HTML/CSS) */
  .taxonomy-tree { overflow-x: auto; padding: 10px 0; }
  .taxonomy-tree, .taxonomy-tree ul, .taxonomy-tree li {
    list-style: none; margin: 0; padding: 0;
  }
  .taxonomy-tree > ul { padding-top: 0; }
  .taxonomy-tree ul {
    display: flex; justify-content: center;
    padding-top: 20px; position: relative;
  }
  .taxonomy-tree ul ul::before {
    content: ''; position: absolute; top: 0; left: 50%;
    border-left: 2px solid var(--upwr-rule); height: 20px;
  }
  .taxonomy-tree li {
    display: flex; flex-direction: column; align-items: center;
    position: relative; padding: 20px 8px 0; text-align: center;
  }
  .taxonomy-tree > ul > li { padding-top: 0; }
  .taxonomy-tree li::before, .taxonomy-tree li::after {
    content: ''; position: absolute; top: 0;
    border-top: 2px solid var(--upwr-rule); width: 50%; height: 20px;
  }
  .taxonomy-tree li::before { right: 50%; }
  .taxonomy-tree li::after { left: 50%; border-left: 2px solid var(--upwr-rule); }
  .taxonomy-tree li:first-child::before { border: none; }
  .taxonomy-tree li:last-child::after { border: none; }
  .taxonomy-tree li:last-child::before {
    border-right: 2px solid var(--upwr-rule); border-radius: 0 5px 0 0;
  }
  .taxonomy-tree li:first-child::after { border-radius: 5px 0 0 0; }
  .taxonomy-tree li:only-child::before,
  .taxonomy-tree li:only-child::after { display: none; }
  .taxonomy-tree li:only-child { padding-top: 0; }

  .tax-node {
    background: var(--upwr-surface-sunken); border: 2px solid var(--upwr-ink-soft); border-radius: 8px;
    padding: 10px 18px; font-weight: 700; font-size: 15px; color: var(--upwr-ink);
    white-space: nowrap;
  }
  .tax-node small { font-weight: 400; font-size: 12px; color: var(--upwr-reference); }
  .tax-leaf {
    border-radius: 8px; padding: 12px 16px; font-weight: 700; font-size: 14px;
    color: white; cursor: pointer; transition: all 0.2s; white-space: nowrap;
  }
  .tax-leaf:hover {
    transform: translateY(-3px); box-shadow: 0 4px 12px rgba(0,0,0,0.2);
  }

  @media (max-width: 600px) {
    .taxonomy-tree li { padding: 16px 2px 0; }
    .tax-node { padding: 6px 8px; font-size: 11px; }
    .tax-node small { font-size: 9px; }
    .tax-leaf {
      padding: 8px 6px; font-size: 11px;
      transform: rotate(-25deg); margin-top: 6px;
    }
    .tax-leaf:hover { transform: rotate(-25deg) translateY(-3px); }

  }

  /* Drabina operacji (Ryc. 3.10) */
  .ops-ladder {
    display: grid; grid-template-columns: repeat(4, minmax(0, 1fr));
    gap: 14px; align-items: end; max-width: 640px; margin: 8px auto 4px;
  }
  .ops-col { display: flex; flex-direction: column; gap: 8px; }
  .ops-stack { display: flex; flex-direction: column; gap: 4px; }
  .ops-step {
    height: 78px; border-radius: 6px; color: #fff; background: var(--ops-c);
    display: flex; flex-direction: column; align-items: center; justify-content: center;
  }
  /* Odziedziczone: jasne tło w kolorze typu, napis w tym kolorze */
  .ops-step.is-inherited {
    background: color-mix(in srgb, var(--ops-c) 18%, var(--upwr-surface));
    color: var(--ops-c);
  }
  .ops-sym { font: 700 26px/1.1 var(--upwr-mono); letter-spacing: .08em; }
  .ops-stat { font-size: 13px; margin-top: 4px; }
  .ops-label {
    text-align: center; font-weight: 700; font-size: 14px; color: var(--upwr-ink);
    border-top: 3px solid var(--ops-c); padding-top: 6px;
  }
  @media (max-width: 600px) {
    .ops-ladder { gap: 6px; }
    .ops-step { height: 62px; }
    .ops-sym { font-size: 18px; }
    .ops-stat { font-size: 11px; }
    .ops-label { font-size: 12px; }
  }

  /* Siatka statystyk (Ryc. 5.6) */
  /* Dwie tabele obok siebie w Ryc. 5.7 */
  .ch5-full-pair { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 28px; align-items: start; }
  .ch5-full-pair h4 { margin: 0 0 6px; }
  @media (max-width: 640px) { .ch5-full-pair { grid-template-columns: 1fr; gap: 14px; } }

  .tmap {
    display: grid; grid-template-columns: minmax(110px, 0.9fr) repeat(2, minmax(0, 1fr));
    gap: 6px; max-width: 520px; margin: 8px auto 4px;
  }
  .tmap-head {
    text-align: center; font-weight: 700; font-size: 14px; color: var(--upwr-ink);
    border-bottom: 3px solid var(--upwr-rule); padding: 4px 0 6px;
  }
  .tmap-row {
    display: flex; align-items: center; font-size: 13px; line-height: 1.3;
    color: var(--upwr-ink-soft); padding-right: 6px;
  }
  .tmap-cell {
    display: flex; flex-wrap: wrap; gap: 6px; align-content: center; justify-content: center;
    min-height: 64px; padding: 8px; border-radius: 6px;
    background: var(--upwr-surface-sunken);
  }
  .tmap-chip {
    --tmap-c: var(--upwr-ink-soft);
    font-size: 14px; font-weight: 600; padding: 3px 10px; border-radius: 999px;
    color: var(--tmap-c); border: 1.5px solid var(--tmap-c);
    background: color-mix(in srgb, var(--tmap-c) 10%, var(--upwr-surface));
  }
  .tmap-none { color: var(--upwr-reference); font-size: 18px; }
  @media (max-width: 600px) {
    .tmap { grid-template-columns: 78px repeat(2, minmax(0, 1fr)); gap: 4px; }
    .tmap-row, .tmap-head { font-size: 11px; }
    .tmap-chip { font-size: 11px; padding: 2px 6px; }
  }

  @keyframes taxDetailFade {
    from { opacity: 0; transform: translateY(-6px); }
    to   { opacity: 1; transform: translateY(0); }
  }

  "))
) # end app_extras

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch1_ui, ch2_ui, ch3_ui, ch4_ui,
                  ch5_ui, ch7_ui, ch8_ui)

ui <- lecture_page(
  lecture_id    = "typy-danych",
  lecture_num   = "01",
  lecture_title = "Statystyka opisowa",
  module_label  = "Statystyka",
  chapters      = .chapters,
  header_extras = app_extras
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # ==========================================================================
  # NAWIGACJA MIEDZY ROZDZIALAMI
  # ==========================================================================

  lc <- lecture_server(.chapters, input, output, session)

  # Nawigacja między rozdziałami idzie przez lc_chapter_next() w modułach
  # (sendCustomMessage("switchToChapter", ...) → lc__switch_chapter).
  # Stare observeEvent(input$chN_next) zostały usunięte razem z buttonami.

  # ==========================================================================
  # VARIABLE TRACKER
  # ==========================================================================

  output$tracker_ch3 <- renderUI({
    var_name <- input$tracked_var
    req(var_name)
    vals <- student_data[[var_name]]
    label <- variable_meta[[var_name]]$label

    lc_tracker(label, c("Średnia" = lc_fmt(mean(vals), 2), "Mediana" = lc_fmt(median(vals), 2)))
  })

  output$tracker_ch4 <- renderUI({
    var_name <- input$tracked_var
    req(var_name)
    vals <- student_data[[var_name]]
    label <- variable_meta[[var_name]]$label

    lc_tracker(label, c("x̄" = lc_fmt(mean(vals), 2), "Me" = lc_fmt(median(vals), 2),
                       "SD" = lc_fmt(sd(vals), 2), "IQR" = lc_fmt(IQR(vals), 2)))
  })

  output$tracker_ch5 <- renderUI({
    var_name <- input$tracked_var
    req(var_name)
    vals <- student_data[[var_name]]
    label <- variable_meta[[var_name]]$label
    sk <- round(e1071::skewness(vals), 2)
    ku <- round(e1071::kurtosis(vals), 2)

    lc_tracker(label, c("x̄" = lc_fmt(mean(vals), 2), "SD" = lc_fmt(sd(vals), 2),
                       "Skośność" = lc_fmt(sk, 2), "Kurtoza" = lc_fmt(ku, 2)))
  })

  # ==========================================================================
  # CHAPTER SERVERS
  # ==========================================================================

  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch7_server(input, output, session)
  ch8_server(input, output, session)

} # end server

shinyApp(ui = ui, server = server)
