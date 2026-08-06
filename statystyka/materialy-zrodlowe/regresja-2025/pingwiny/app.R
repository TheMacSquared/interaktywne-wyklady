# Regresja - interaktywny przewodnik
# Scrollowalny skrypt z osadzonymi widgetami do nauczania regresji liniowej i logistycznej

library(shiny)
library(ggplot2)
library(dplyr)
library(broom)

# ============================================================================
# MODULY
# ============================================================================

.find_app_dir <- function() {
  for (i in seq_len(sys.nframe())) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  }
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("--file=", "", file_arg))))
  }
  getwd()
}
app_dir <- .find_app_dir()
project_root <- dirname(app_dir)

source(file.path(project_root, "R", "palette.R"),        local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),     local = TRUE)
source(file.path(project_root, "R", "shared.R"),         local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)
lc_apply_ggplot_defaults()

source(file.path(app_dir, "modules", "helpers.R"),         local = TRUE)
source(file.path(app_dir, "modules", "ch1_liniowa.R"),     local = TRUE)
source(file.path(app_dir, "modules", "ch2_jakosc.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch3_wieloraka.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch3a_interakcje.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch4_porownanie.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch5_logistyczna.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch6_sciaga.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch7_cwiczenia.R"),   local = TRUE)

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch1_ui, ch2_ui, ch3_ui, ch3a_ui, ch4_ui, ch5_ui, ch6_ui, ch7_ui)

ui <- lecture_page(
  lecture_id    = "regresja",
  lecture_num   = "06",
  lecture_title = "Regresja — pingwiny",
  module_label  = "Moduł VI",
  chapters      = .chapters
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  lc <- lecture_server(.chapters, input, output, session)

  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch3a_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch6_server(input, output, session)
  ch7_server(input, output, session)
}

shinyApp(ui = ui, server = server)
