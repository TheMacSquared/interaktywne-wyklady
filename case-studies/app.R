# Case studies - analizy statystyczne od A do Z
# Kazdy rozdzial to kompletna analiza jednego zbioru danych

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(rstatix)
library(broom)
library(tidyr)
library(AER)

# ============================================================================
# KOLORY
# ============================================================================

# Kolory specyficzne dla case studies
col_explore    <- "#3498db"    # eksploracja danych
col_test       <- "#9b59b6"    # testowanie hipotez
col_model      <- "#27ae60"    # modelowanie
col_conclude   <- "#f39c12"    # wnioski
col_highlight  <- "#e74c3c"    # podkreslenie

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

source(file.path(project_root, "R", "shared.R"),           local = TRUE)
source(file.path(app_dir, "modules", "helpers.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch1_caschools.R"), local = TRUE)

# ============================================================================
# GLOBAL UI HEADER (CSS, JS)
# ============================================================================

global_header <- tagList(
  withMathJax(),
  tags$head(
    includeCSS(file.path(project_root, "R", "shared_styles.css")),
    tags$style(HTML("
  /* Case study specific */
  .analysis-step {
    background: #f0f7ee; border-left: 4px solid #27ae60;
    padding: 12px 16px; margin: 15px 0; border-radius: 0 6px 6px 0;
  }
  .analysis-step .step-number {
    display: inline-block; background: #27ae60; color: white;
    width: 28px; height: 28px; border-radius: 50%; text-align: center;
    line-height: 28px; font-weight: bold; margin-right: 8px;
  }
  ")),
    includeScript(file.path(project_root, "R", "shared_toc.js"))
  ))

# ============================================================================
# UI
# ============================================================================

ui <- navbarPage(
  "Case studies",
  id = "main_nav",
  theme = bs_theme(bootswatch = "sandstone"),
  header = global_header,
  ch1_ui
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  ch1_server(input, output, session)
}

shinyApp(ui = ui, server = server)
