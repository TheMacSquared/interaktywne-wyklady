# Symulacje statystyczne - interaktywny przewodnik
# Bootstrap, Jackknife, Permutacje, Cross-Validation, Monte Carlo

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstatix)
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

source(file.path(app_dir, "modules", "helpers.R"),                local = TRUE)
source(file.path(app_dir, "modules", "ch1_idea.R"),               local = TRUE)
source(file.path(app_dir, "modules", "ch2_bootstrap_ci.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch3_bootstrap_jednopr.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch4_permutacje.R"),         local = TRUE)
source(file.path(app_dir, "modules", "ch5_jackknife.R"),          local = TRUE)
source(file.path(app_dir, "modules", "ch6_cv.R"),                 local = TRUE)
source(file.path(app_dir, "modules", "ch7_monte_carlo.R"),        local = TRUE)
source(file.path(app_dir, "modules", "ch8_kiedy.R"),              local = TRUE)
source(file.path(app_dir, "modules", "ch9_sciaga.R"),             local = TRUE)
source(file.path(app_dir, "modules", "ch10_cwiczenia.R"), local = TRUE)

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui,
                  ch6_ui, ch7_ui, ch8_ui, ch9_ui, ch10_ui)

ui <- lecture_page(
  lecture_id    = "symulacje-statystyczne",
  lecture_num   = "06",
  lecture_title = "Symulacje statystyczne",
  module_label  = "Moduł VI",
  chapters      = .chapters
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)

  # ==========================================================================
  # CHAPTER SERVERS
  # ==========================================================================

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

}

shinyApp(ui = ui, server = server)
