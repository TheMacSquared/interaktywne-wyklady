# Związki dwóch zmiennych
# Opis związku według typów zmiennych: tabela krzyżowa, grupy, korelacja

library(shiny)
library(ggplot2)
library(dplyr)
library(jsonlite)

# ============================================================================
# BOOTSTRAP PROJEKTU
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

source(file.path(project_root, "R", "palette.R"),        local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),     local = TRUE)
source(file.path(project_root, "R", "shared.R"),         local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)
source(file.path(project_root, "R", "ankieta.R"),        local = TRUE)

lc_apply_ggplot_defaults()

addResourcePath("assets", file.path(app_dir, "assets"))

# ============================================================================
# MODUŁY
# ============================================================================

source(file.path(app_dir, "modules", "helpers.R"),             local = TRUE)
source(file.path(app_dir, "modules", "ch1_mapa.R"),            local = TRUE)
source(file.path(app_dir, "modules", "ch2_dwie_jakosciowe.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch3_grupy.R"),           local = TRUE)
source(file.path(app_dir, "modules", "ch4_korelacja.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch5_pulapki.R"),         local = TRUE)

.chapters <- list(ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui)

ui <- lecture_page(
  lecture_id    = "zwiazki-zmiennych",
  lecture_num   = "02",
  lecture_title = "Związki dwóch zmiennych",
  module_label  = "Statystyka",
  chapters      = .chapters,
  # Sceny SVG (kontrola drogowa, pensje w branżach, staż a pensja): rozdz. 2–4.
  # Chart.js do wykresów rozrzutu (Ryc. 4.3, 4.4, 5.3, 5.4): scatter_charts.js.
  header_extras = tagList(
    tags$style(HTML(paste(readLines(file.path(app_dir, "modules", "scenes.css"), warn = FALSE), collapse = "\n"))),
    includeScript(file.path(app_dir, "modules", "scenes.js")),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.js"),
    includeScript(file.path(app_dir, "modules", "scatter_charts.js")),
    tags$style(HTML("
      .lc-scat-canvas { position: relative; aspect-ratio: 1.8 / 1; touch-action: none; }
      .lc-outlier .lc-scat-canvas, .lc-simpson .lc-scat-canvas { aspect-ratio: 1.7 / 1; }
      @media (max-width: 640px) { .lc-scat-canvas, .lc-outlier .lc-scat-canvas, .lc-simpson .lc-scat-canvas { aspect-ratio: 1.15 / 1; } }
      .lc-scat .lc-seg button[aria-pressed='true'] { background: var(--upwr-accent); color: var(--upwr-surface); }
      .lc-scat [hidden] { display: none !important; }
    "))
  )
)

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)

  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
}

shinyApp(ui = ui, server = server)
