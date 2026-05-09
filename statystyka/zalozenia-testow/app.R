# Zalozenia testow - interaktywny przewodnik
# Scrollowalny skrypt: zalozenia wszystkich poznanych metod i alternatywy

library(shiny)
library(ggplot2)
library(dplyr)
library(rstatix)
library(broom)
library(lmtest)

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

source(file.path(project_root, "R", "palette.R"),          local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),       local = TRUE)
source(file.path(project_root, "R", "shared.R"),           local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"),   local = TRUE)

lc_apply_ggplot_defaults()

source(file.path(app_dir, "modules", "helpers.R"),          local = TRUE)
source(file.path(app_dir, "modules", "ch1_normalnosc.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch2_wariancje.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch3_regresja.R"),     local = TRUE)
source(file.path(app_dir, "modules", "ch4_chi_fisher.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch5_mapa.R"),         local = TRUE)
source(file.path(app_dir, "modules", "ch6_sciaga.R"),       local = TRUE)

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui, ch6_ui)

ui <- lecture_page(
  lecture_id    = "zalozenia-testow",
  lecture_num   = "04a",
  lecture_title = "Założenia testów",
  module_label  = "Moduł IV",
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
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch6_server(input, output, session)
}

shinyApp(ui = ui, server = server)
