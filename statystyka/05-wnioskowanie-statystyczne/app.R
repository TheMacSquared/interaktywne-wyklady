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
  # Katalog aplikacji rozpoznajemy po tym, że jego rodzic zawiera R/course.R.
  has_project_root <- function(dir) {
    file.exists(file.path(dirname(dir), "R", "course.R"))
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
  # 3) Katalog roboczy — scripts/build_static.R ustawia go na katalog wykładu.
  candidates <- c(candidates, getwd())

  # Pierwszy kandydat leżący w projekcie; gdy żaden nie pasuje, zachowaj stare zachowanie.
  # Bez tego uruchomienie przez wrapper (np. rozszerzenie Shiny dla VS Code) trafia do
  # katalogu wrappera, bo --file= wskazuje jego skrypt, a nie app.R.
  valid <- Filter(has_project_root, candidates)
  if (length(valid) > 0) valid[[1]] else candidates[[1]]
}
app_dir <- .find_app_dir()
project_root <- dirname(app_dir)
shared_root <- file.path(dirname(project_root), "shared")

source(file.path(project_root, "R", "course.R"), local = TRUE)

source(file.path(shared_root, "palette.R"),          local = TRUE)
source(file.path(shared_root, "theme_upwr.R"),       local = TRUE)
source(file.path(shared_root, "shared.R"),           local = TRUE)
source(file.path(shared_root, "lecture_layout.R"),   local = TRUE)
source(file.path(shared_root, "lc_stat_figures.R"),  local = TRUE)

# Globalne defaulty ggplot2 — motyw upwr + IBM Plex Sans + kolory geom-ów
lc_apply_ggplot_defaults()

addResourcePath("assets", file.path(app_dir, "assets"))

source(file.path(app_dir, "modules", "helpers.R"),              local = TRUE)
source(file.path(app_dir, "modules", "figures.R"),              local = TRUE)
source(file.path(app_dir, "modules", "proto05.R"),              local = TRUE)  # sceny Ryc. 3.1, 7.1, 9.1
source(file.path(app_dir, "modules", "ch1_panels.R"),           local = TRUE)  # Ryc. 1.1, 1.3
source(file.path(app_dir, "modules", "ch2_panels.R"),           local = TRUE)  # Ryc. 2.1–2.6
source(file.path(app_dir, "modules", "ch3_panels.R"),           local = TRUE)  # Ryc. 3.8
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
  # Sceny SVG ANOVA (Ryc. 9.3, 9.4, 9.6): modules/scenes.js + modules/scenes.css
  tags$style(HTML(paste(readLines(file.path(app_dir, "modules", "scenes.css"), warn = FALSE), collapse = "\n"))),
  includeScript(file.path(app_dir, "modules", "scenes.js")),
  # Sceny Ryc. 3.1, 7.1, 9.1 (dawne prototypy 5A, 5E, 5D): modules/proto05.js + modules/proto05.css
  tags$style(HTML(paste(readLines(file.path(app_dir, "modules", "proto05.css"), warn = FALSE), collapse = "\n"))),
  includeScript(file.path(app_dir, "modules", "proto05.js")),
  # Panele rozdziałów 01–03 (ch1_panels.R, ch2_panels.R, ch3_panels.R): sam CSS, bez JS
  tags$style(HTML(paste(readLines(file.path(app_dir, "modules", "panels.css"), warn = FALSE), collapse = "\n"))),
  # Wykresy rozkładu statystyki testowej (Ryc. 3.4, 3.6): ECharts przez lcCharts.
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/echarts/6.1.0/echarts.min.js"),
  includeScript(file.path(shared_root, "lc_charts.js")),
  includeScript(file.path(app_dir, "modules", "ws_charts.js")),
  # Ryciny liczone w przeglądarce: silnik lcSF, rozkłady, testy i ryciny wykładu.
  includeScript(file.path(shared_root, "lc_dist.js")),
  includeScript(file.path(shared_root, "lc_stats.js")),
  includeScript(file.path(shared_root, "lc_stat_figures.js")),
  includeScript(file.path(app_dir, "modules", "figures.js")),
  lc_sf_styles(),
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
