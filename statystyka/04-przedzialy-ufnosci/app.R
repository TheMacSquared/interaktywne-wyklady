# Przedziały ufności — interaktywny przewodnik
# Scrollowalny skrypt z osadzonymi widgetami do nauczania przedziałów ufności

library(shiny)
library(ggplot2)
library(dplyr)
library(rstatix)
library(broom)

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

source(file.path(shared_root, "palette.R"),        local = TRUE)
source(file.path(shared_root, "theme_upwr.R"),     local = TRUE)
source(file.path(shared_root, "shared.R"),         local = TRUE)
source(file.path(shared_root, "lecture_layout.R"), local = TRUE)

# Globalne defaulty ggplot2 — motyw upwr + IBM Plex Sans + kolory geom-ów
lc_apply_ggplot_defaults()

source(file.path(app_dir, "modules", "helpers.R"),       local = TRUE)
source(file.path(shared_root, "lc_stat_figures.R"), local = TRUE)
source(file.path(app_dir, "modules", "ci_construction.R"), local = TRUE)
# Ryciny wstawiane w tekst: przed rozdziałami, bo rozdziały wstawiają ich UI.
for (.f in c("common", paste0("ch", 1:5))) {
  source(file.path(app_dir, "modules", paste0("figures_", .f, ".R")), local = TRUE)
}
source(file.path(app_dir, "modules", "ci_explorers.R"), local = TRUE)
source(file.path(app_dir, "modules", "visual_scenes.R"), local = TRUE)
source(file.path(app_dir, "modules", "cases_common.R"), local = TRUE)
source(file.path(app_dir, "modules", "panels.R"), local = TRUE)  # panele do zatrzymania (rozdz. 01, 02, 05)
source(file.path(app_dir, "modules", "ch1_estymacja.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch2_idea.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch3_srednia.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch4_proporcja.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch5_czynniki.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch7_cwiczenia.R"), local = TRUE)

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui, ch7_ui)

ui <- lecture_page(
  lecture_id    = "przedzialy-ufnosci",
  lecture_num   = "04",
  lecture_title = "Przedziały ufności",
  module_label  = "Statystyka",
  chapters      = .chapters,
  # Sceny SVG z grupką spod sali (scenes.js): rozdz. 1–3
  header_extras = tagList(
    tags$style(HTML(paste(readLines(file.path(app_dir, "modules", "scenes.css"), warn = FALSE), collapse = "\n"))),
    includeScript(file.path(app_dir, "modules", "scenes.js")),
    # Wykresy JS: ECharts SVG ze wspólną obsługą lc_charts.js
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/echarts/6.1.0/echarts.min.js"),
    includeScript(file.path(shared_root, "lc_charts.js")),
    tags$style(HTML(paste(readLines(file.path(app_dir, "modules", "ci_explorers.css"), warn = FALSE), collapse = "\n"))),
    # Panele do zatrzymania (rozdz. 01, 02, 05): modules/panels.R + panels.css, bez JS
    tags$style(HTML(paste(readLines(file.path(app_dir, "modules", "panels.css"), warn = FALSE), collapse = "\n"))),
    tags$style(HTML(paste(readLines(file.path(app_dir, "modules", "visual_scenes.css"), warn = FALSE), collapse = "\n"))),
    includeScript(file.path(app_dir, "modules", "visual_scenes.js")),
    # Ryc. 3.2 i 3.3 (kropki na osi, klamra przedziału): modules/fig_32.js + fig_32.css
    tags$style(HTML(paste(readLines(file.path(app_dir, "modules", "fig_32.css"), warn = FALSE), collapse = "\n"))),
    includeScript(file.path(app_dir, "modules", "fig_32.js")),
    # Ryc. 4.1 i 4.3 (paski 100%, ramka i klamra przedziału): modules/fig_41.js + fig_41.css
    tags$style(HTML(paste(readLines(file.path(app_dir, "modules", "fig_41.css"), warn = FALSE), collapse = "\n"))),
    includeScript(file.path(app_dir, "modules", "fig_41.js")),
    lapply(file.path(app_dir, "modules", c("fig_ch2.js", "fig_ch4.js",
                                           "ci_explorers.js")),
           includeScript),
    # Ryciny liczone w przeglądarce: silnik lcSF, rozkłady lcDist, ryciny wykładu.
    includeScript(file.path(shared_root, "lc_dist.js")),
    includeScript(file.path(shared_root, "lc_stat_figures.js")),
    # Wspólny wygląd przedziałów (klamra, estymata, ± SE, stos kropek) dla rycin lcSF.
    includeScript(file.path(app_dir, "modules", "ci_look.js")),
    lapply(file.path(app_dir, "modules", c("sf_ch12.js", "sf_steps.js", "sf_examples.js",
                                           "sf_ch5.js")),
           includeScript),
    lc_sf_styles(),
    tags$style(HTML("
      .lc-stepper-head .lc-toolbar > .lc-grp { flex: 0 0 auto; }
      .lc-stepper-head .lc-toolbar .lc-seg { flex-wrap: nowrap; }
      .lc-seg button[aria-pressed='true'] { background: var(--upwr-accent); color: var(--upwr-surface); }
      /* Wykresy ECharts wypełniają kontener z aspect-ratio. */
      .lc-ec { position: absolute; inset: 0; }
      .fig-zero-canvas { position: relative; aspect-ratio: 2.4 / 1; touch-action: none; margin-top: .6em; }
      .fig-zero[data-lc-chart-large='true'] .fig-zero-canvas { aspect-ratio: 1.65 / 1; }
      .fig-zero .fig-zero-range { width: 100%; cursor: pointer; }
      @media (max-width: 640px) { .fig-zero-canvas { aspect-ratio: 1.4 / 1; } }
    "))
  )
)
