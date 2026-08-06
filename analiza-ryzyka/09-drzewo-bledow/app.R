# 09-drzewo-bledow — pełny szkic wykładu

library(shiny)
library(ggplot2)
library(dplyr)

.find_app_dir <- function() {
  for (i in seq_len(sys.nframe())) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) {
      return(dirname(normalizePath(ofile)))
    }
  }
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("--file=", "", file_arg[[1]]))))
  }
  getwd()
}

app_dir <- .find_app_dir()
project_root <- dirname(app_dir)

source(file.path(project_root, "R", "palette.R"), local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"), local = TRUE)
source(file.path(project_root, "R", "shared.R"), local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)
source(file.path(project_root, "R", "bananpol.R"), local = TRUE)
source(file.path(project_root, "R", "risk_math.R"), local = TRUE)
source(file.path(project_root, "R", "risk_block.R"), local = TRUE)
source(file.path(app_dir, "modules", "block.R"), local = TRUE)

lc_apply_ggplot_defaults()

.chapters <- fta_chapters

ui <- lecture_page(
  lecture_id = "drzewo-bledow",
  lecture_num = "09",
  lecture_title = "Analiza drzewa błędów",
  module_label = "Analiza ryzyka · Bananpol",
  chapters = .chapters
)

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)
  fta_server(input, output, session)
}

shinyApp(ui = ui, server = server)
