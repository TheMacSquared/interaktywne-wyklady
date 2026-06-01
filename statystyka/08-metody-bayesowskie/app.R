# Metody bayesowskie - interaktywny przewodnik
# Prior, posterior, Bayes Factor, HDI, regresja bayesowska
# Konsekwentne porownanie z podejsciem czestosciowym

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstatix)
library(broom)
library(BayesFactor)

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

source(file.path(app_dir, "modules", "helpers.R"),              local = TRUE)
source(file.path(app_dir, "modules", "ch1_intuicja.R"),         local = TRUE)
source(file.path(app_dir, "modules", "ch2_bf_vs_p.R"),          local = TRUE)
source(file.path(app_dir, "modules", "ch3_hdi_vs_ci.R"),        local = TRUE)
source(file.path(app_dir, "modules", "ch4_jedna_proba.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch5_dwie_grupy.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch6_anova.R"),            local = TRUE)
source(file.path(app_dir, "modules", "ch7_tabele.R"),           local = TRUE)
source(file.path(app_dir, "modules", "ch8_korelacja.R"),        local = TRUE)
source(file.path(app_dir, "modules", "ch9_regresja_lin.R"),     local = TRUE)
source(file.path(app_dir, "modules", "ch10_regresja_log.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch11_sciaga.R"),          local = TRUE)
source(file.path(app_dir, "modules", "ch12_cwiczenia.R"), local = TRUE)

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui, ch6_ui,
                  ch7_ui, ch8_ui, ch9_ui, ch10_ui, ch11_ui, ch12_ui)

ui <- lecture_page(
  lecture_id    = "metody-bayesowskie",
  lecture_num   = "08",
  lecture_title = "Metody bayesowskie",
  module_label  = "Moduł VIII",
  chapters      = .chapters
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)

  # CHAPTER SERVERS
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
  ch11_server(input, output, session)
  ch12_server(input, output, session)
}

shinyApp(ui = ui, server = server)
