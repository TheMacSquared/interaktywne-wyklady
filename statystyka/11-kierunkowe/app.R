# Material kierunkowy - rozszerzenia statystyki dla kierunkow
# Osobny modul: jeden chapter na kierunek

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
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

source(file.path(app_dir, "modules", "helpers.R"),          local = TRUE)
source(file.path(app_dir, "modules", "ch1_rolnictwo.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch2_zywnosc.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch3_bezpieczenstwo.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch4_srodowisko.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch5_wodna.R"),        local = TRUE)
source(file.path(app_dir, "modules", "ch6_oze.R"),          local = TRUE)

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui, ch6_ui)

ui <- lecture_page(
  lecture_id    = "kierunkowe",
  lecture_num   = "11",
  lecture_title = "Kierunkowe",
  module_label  = "Moduł XI",
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
