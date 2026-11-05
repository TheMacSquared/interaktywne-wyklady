# Testowanie hipotez - interaktywny przewodnik
# Scrollowalny skrypt z osadzonymi widgetami do nauczania testow hipotez

library(shiny)
library(ggplot2)
library(dplyr)
library(rstatix)
library(broom)
library(tidyr)

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

source(file.path(project_root, "R", "palette.R"),          local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),       local = TRUE)
source(file.path(project_root, "R", "shared.R"),           local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"),   local = TRUE)

# Globalne defaulty ggplot2 — motyw upwr + IBM Plex Sans + kolory geom-ów
lc_apply_ggplot_defaults()

addResourcePath("assets", file.path(app_dir, "assets"))

source(file.path(app_dir, "modules", "helpers.R"),              local = TRUE)
source(file.path(app_dir, "modules", "ch1_logika.R"),           local = TRUE)
source(file.path(app_dir, "modules", "ch2_hipotezy.R"),         local = TRUE)
source(file.path(app_dir, "modules", "ch2_jedna_ilosciowa.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch3_jedna_jakosciowa.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch4_korelacja.R"),        local = TRUE)
source(file.path(app_dir, "modules", "ch5_dwie_jakosciowe.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch6_dwie_grupy.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch7_anova.R"),            local = TRUE)
source(file.path(app_dir, "modules", "ch10_sila_efektu.R"),     local = TRUE)
source(file.path(app_dir, "modules", "ch_drzewo.R"),            local = TRUE)
source(file.path(app_dir, "modules", "ch9_cwiczenia.R"),        local = TRUE)

# ============================================================================
# LOKALNE STYLE I JS
# ============================================================================

header_extras <- tagList(
  # Sceny SVG (ANOVA, rozdz. 09): modules/scenes.js + modules/scenes.css
  tags$style(HTML(paste(readLines(file.path(app_dir, "modules", "scenes.css"), warn = FALSE), collapse = "\n"))),
  includeScript(file.path(app_dir, "modules", "scenes.js")),
  # Wykresy rozkładu statystyki testowej (Ryc. 2.4, 3.3): ECharts przez lcCharts.
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/echarts/6.1.0/echarts.min.js"),
  includeScript(file.path(project_root, "R", "lc_charts.js")),
  includeScript(file.path(app_dir, "modules", "ws_charts.js")),
  tags$style(HTML("
    .ws-chart-wrap {
      position: relative;
      width: 100%;
      height: 320px;
    }
    .ws-chart-wrap > .lc-ec { position: absolute; inset: 0; }

    .ch2-step-panel {
      animation: ch2StepEnter 360ms cubic-bezier(.2, .75, .25, 1);
      transform-origin: top center;
    }

    .ch2-step-panel .lc-formula-box {
      animation: ch2SoftPop 320ms ease-out both;
    }

    @keyframes ch2StepEnter {
      from { opacity: 0; transform: translateY(10px); }
      to   { opacity: 1; transform: translateY(0); }
    }

    @keyframes ch2SoftPop {
      from { opacity: 0; transform: translateY(6px) scale(.985); }
      to   { opacity: 1; transform: translateY(0) scale(1); }
    }

    @media (prefers-reduced-motion: reduce) {
      .ch2-step-panel,
      .ch2-step-panel .lc-formula-box {
        animation: none !important;
        transition: none !important;
        transform: none !important;
      }
    }
  ")),
)

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch1_ui, ch2h_ui, ch1d_ui, ch2_ui, ch3_ui, ch4_ui,
                  ch5_ui, ch6_ui, ch7_ui, ch10_ui, ch_drzewo_ui, ch9_ui)

ui <- lecture_page(
  lecture_id    = "wnioskowanie-statystyczne",
  lecture_num   = "05",
  lecture_title = "Testowanie hipotez",
  module_label  = "Statystyka",
  chapters      = .chapters,
  header_extras = header_extras
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  lc <- lecture_server(.chapters, input, output, session)

  # Nawigacja między rozdziałami idzie przez lc_chapter_next() w modułach
  # (sendCustomMessage("switchToChapter", ...) → lc__switch_chapter).

  # ==========================================================================
  # CHAPTER SERVERS
  # ==========================================================================

  ch1_server(input, output, session)
  ch2h_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch6_server(input, output, session)
  ch7_server(input, output, session)
  ch10_server(input, output, session)
  ch_drzewo_server(input, output, session)
  ch9_server(input, output, session)

}

shinyApp(ui = ui, server = server)
