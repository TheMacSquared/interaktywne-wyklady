# Regresja liniowa i KMNK — wykład interaktywny.

library(shiny)
library(ggplot2)
library(dplyr)
library(broom)

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

source(file.path(project_root, "R", "palette.R"), local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"), local = TRUE)
source(file.path(project_root, "R", "shared.R"), local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)
source(file.path(project_root, "R", "econometrics_helpers.R"), local = TRUE)
lc_apply_ggplot_defaults()

source(file.path(app_dir, "modules", "helpers.R"),         local = TRUE)
source(file.path(app_dir, "modules", "ch1_rownanie.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch2_dopasowanie.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch3_estymatory.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch4_zalozenia.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch5_cwiczenie.R"),   local = TRUE)

.chapters <- list(ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui)

ui <- lecture_page(
  lecture_id    = "regresja-kmnk",
  lecture_num   = "02",
  lecture_title = "Regresja liniowa i KMNK",
  module_label  = "Rozdział 02",
  chapters      = .chapters
)

server <- function(input, output, session) {
  lecture_server(.chapters, input, output, session)
  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
}

shinyApp(ui = ui, server = server)
