# 09-drzewo-bledow — pełny szkic wykładu

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
  if (length(file_arg)) return(dirname(normalizePath(sub("--file=", "", file_arg[[1]]))))
  getwd()
}

app_dir <- .find_app_dir()
project_root <- dirname(app_dir)

source(file.path(project_root, "R", "palette.R"), local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"), local = TRUE)
source(file.path(project_root, "R", "shared.R"), local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)
source(file.path(project_root, "R", "bananpol.R"), local = TRUE)
source(file.path(project_root, "R", "course_factory.R"), local = TRUE)
source(file.path(project_root, "R", "course_catalog.R"), local = TRUE)

lc_apply_ggplot_defaults()

config <- risk_course_catalog[["drzewo-bledow"]]
.chapters <- risk_lecture_chapters(config)

ui <- lecture_page(
  lecture_id = config$lecture_id,
  lecture_num = config$num,
  lecture_title = config$title,
  module_label = "Analiza ryzyka · Bananpol",
  chapters = .chapters
)

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)
  risk_lecture_server(config, input, output, session)
}

shinyApp(ui = ui, server = server)
