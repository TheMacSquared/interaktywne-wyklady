# Język ryzyka — interaktywny wykład
# Od zagrożenia i ekspozycji do zdarzeń oraz prawdopodobieństwa.

library(shiny)
library(ggplot2)
library(dplyr)
library(jsonlite)

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

lc_apply_ggplot_defaults()

# ==========================================================================
# MODUŁY
# ==========================================================================

source(file.path(app_dir, "modules", "helpers.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch1_sytuacja.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch2_czestosc.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch3_przestrzen.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch4_zbiory.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch5_decyzja.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch6_sciaga.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch7_quiz.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch8_cwiczenia.R"), local = TRUE)

.chapters <- list(
  ch1_ui,
  ch2_ui,
  ch3_ui,
  ch4_ui,
  ch5_ui,
  ch6_ui,
  ch7_ui,
  ch8_ui
)

ui <- lecture_page(
  lecture_id    = "jezyk-ryzyka",
  lecture_num   = "01",
  lecture_title = "Od zagrożenia do prawdopodobieństwa",
  module_label  = "Analiza ryzyka · Bananpol",
  chapters      = .chapters
)

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)

  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch6_server(input, output, session)
  ch7_server(input, output, session)
  ch8_server(input, output, session)
}

shinyApp(ui = ui, server = server)
