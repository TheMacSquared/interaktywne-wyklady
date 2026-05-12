# Optymalizacja liniowa — wykład 06.

library(shiny)
library(ggplot2)
library(dplyr)

.find_app_dir <- function() {
  for (i in seq_len(sys.nframe())) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  }
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(dirname(normalizePath(sub("--file=", "", file_arg))))
  getwd()
}

app_dir <- .find_app_dir()
project_root <- dirname(app_dir)

source(file.path(project_root, "R", "palette.R"),                local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),             local = TRUE)
source(file.path(project_root, "R", "shared.R"),                 local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"),         local = TRUE)
source(file.path(project_root, "R", "econometrics_helpers.R"),   local = TRUE)
lc_apply_ggplot_defaults()

source(file.path(app_dir, "modules", "ch1_decyzja.R"),     local = TRUE)
source(file.path(app_dir, "modules", "ch2_model_postac.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch3_graficzna.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch4_cwiczenie.R"),   local = TRUE)

.chapters <- list(ch1_ui, ch2_ui, ch3_ui, ch4_ui)

ui <- lecture_page(
  lecture_id    = "optymalizacja-liniowa",
  lecture_num   = "06",
  lecture_title = "Optymalizacja liniowa",
  module_label  = "Rozdział 06",
  chapters      = .chapters
)

server <- function(input, output, session) {
  lecture_server(.chapters, input, output, session)

  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
}

shinyApp(ui = ui, server = server)
