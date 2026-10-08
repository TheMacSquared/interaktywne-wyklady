# Co czyni dobry zbiór danych?
# Interaktywny wykład oparty o case studies - ocena jakości danych do analiz statystycznych

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(AER)
library(palmerpenguins)
library(ISLR)
library(fivethirtyeight)

# ============================================================================
# MODUŁY
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

source(file.path(project_root, "R", "palette.R"),        local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),     local = TRUE)
source(file.path(project_root, "R", "shared.R"),         local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)

lc_apply_ggplot_defaults()

source(file.path(app_dir, "modules", "helpers.R"),            local = TRUE)
source(file.path(app_dir, "modules", "scene_helpers.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch0_wprowadzenie.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch1_katalog.R"),        local = TRUE)
source(file.path(app_dir, "modules", "ch2_szkoly.R"),         local = TRUE)
source(file.path(app_dir, "modules", "ch3_grupa.R"),          local = TRUE)
source(file.path(app_dir, "modules", "ch4_pingwiny.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch5_tarantino.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch6_hotel.R"),          local = TRUE)
source(file.path(app_dir, "modules", "ch7_wynagrodzenia.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch8_ankieta.R"),        local = TRUE)
source(file.path(app_dir, "modules", "ch9_laboratorium.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch10_studenci.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch11_kawiarnia.R"),     local = TRUE)
source(file.path(app_dir, "modules", "ch12_sciaga.R"),        local = TRUE)

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch0_ui, ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui, ch6_ui,
                  ch7_ui, ch8_ui, ch9_ui, ch10_ui, ch11_ui, ch12_ui)

ui <- lecture_page(
  lecture_id    = "dobre-dane",
  lecture_num   = "07",
  lecture_title = "Co czyni dobry zbiór danych?",
  module_label  = "Statystyka",
  chapters      = .chapters,
  # PROTOTYP SCENY (2026-10-08): sceny SVG (rozdz. 11, „Zbierz sezon”)
  header_extras = tagList(
    tags$style(HTML(paste(readLines(file.path(app_dir, "modules", "scenes.css"), warn = FALSE), collapse = "\n"))),
    includeScript(file.path(app_dir, "modules", "scenes.js")),
    tags$style(HTML("
      .lc-stepper-head .lc-toolbar > .lc-grp { flex: 0 0 auto; }
      .lc-stepper-head .lc-toolbar .lc-seg { flex-wrap: nowrap; }
      .lc-seg button[aria-pressed='true'] { background: var(--upwr-accent); color: var(--upwr-surface); }
    "))
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)

  ch0_server(input, output, session)
  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch6_server(input, output, session)
  ch7_server(input, output, session)
  ch8_server(input, output, session)
  ch9_server(input, output, session)
  ch10_server(input, output, session)
  ch11_server(input, output, session)
  ch12_server(input, output, session)
}

shinyApp(ui = ui, server = server)
