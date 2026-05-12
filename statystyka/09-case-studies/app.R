# Case studies - analizy statystyczne od A do Z
# Kazdy rozdzial to kompletna analiza jednego zbioru danych

library(shiny)
library(ggplot2)
library(dplyr)
library(rstatix)
library(broom)
library(tidyr)
library(AER)

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

source(file.path(app_dir, "modules", "helpers.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch1_caschools.R"), local = TRUE)

# ============================================================================
# LOKALNE STYLE
# ============================================================================

header_extras <- tagList(
  tags$style(HTML("
  /* Case study specific */
  .analysis-step {
    background: var(--upwr-panel); border-left: 4px solid var(--upwr-szalwia);
    padding: 12px 16px; margin: 15px 0; border-radius: 0 6px 6px 0;
  }
  .analysis-step .step-number {
    display: inline-block; background: var(--upwr-szalwia); color: white;
    width: 28px; height: 28px; border-radius: 50%; text-align: center;
    line-height: 28px; font-weight: bold; margin-right: 8px;
  }
  "))
)

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch1_ui)

ui <- lecture_page(
  lecture_id    = "case-studies",
  lecture_num   = "09",
  lecture_title = "Case studies",
  module_label  = "Moduł IX",
  chapters      = .chapters,
  header_extras = header_extras
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)
  ch1_server(input, output, session)
}

shinyApp(ui = ui, server = server)
