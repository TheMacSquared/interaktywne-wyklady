# Regresja light - 45-minutowy crash course
# Osobna, kompaktowa wersja wykładu. Pełna aplikacja zostaje w app.R.

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

source(file.path(app_dir, "modules", "helpers.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch2_jakosc.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch0_light.R"), local = TRUE)

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch0_light_ui)

ui <- lecture_page(
  lecture_id    = "regresja",
  lecture_num   = "06L",
  lecture_title = "Regresja light",
  module_label  = "Moduł VI",
  chapters      = .chapters
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  lecture_server(.chapters, input, output, session)
  ch2_server(input, output, session)
  ch0_light_server(input, output, session)
}

shinyApp(ui = ui, server = server)
