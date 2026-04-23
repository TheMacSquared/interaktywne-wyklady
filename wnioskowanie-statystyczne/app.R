# Testowanie hipotez - interaktywny przewodnik
# Scrollowalny skrypt z osadzonymi widgetami do nauczania testow hipotez

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(rstatix)
library(broom)
library(tidyr)

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

source(file.path(project_root, "R", "palette.R"),          local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),       local = TRUE)
source(file.path(project_root, "R", "shared.R"),           local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"),   local = TRUE)

# Globalne defaulty ggplot2 — motyw upwr + Atkinson + kolory geom-ów
lc_apply_ggplot_defaults()

addResourcePath("assets", file.path(app_dir, "assets"))

source(file.path(app_dir, "modules", "helpers.R"),              local = TRUE)
source(file.path(app_dir, "modules", "ch1_logika.R"),           local = TRUE)
source(file.path(app_dir, "modules", "ch2_hipotezy.R"),         local = TRUE)
source(file.path(app_dir, "modules", "ch2_jedna_ilosciowa.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch3_jedna_jakosciowa.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch4_korelacja.R"),        local = TRUE)
source(file.path(app_dir, "modules", "ch5_dwie_jakosciowe.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch6_dwie_grupy.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch7_anova.R"),            local = TRUE)
source(file.path(app_dir, "modules", "ch_drzewo.R"),            local = TRUE)
source(file.path(app_dir, "modules", "ch8_sciaga.R"),           local = TRUE)
source(file.path(app_dir, "modules", "ch9_cwiczenia.R"),        local = TRUE)

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch1_ui, ch2h_ui, ch2_ui, ch3_ui, ch4_ui,
                  ch5_ui, ch6_ui, ch7_ui, ch_drzewo_ui, ch8_ui, ch9_ui)

ui <- lecture_page(
  lecture_id    = "wnioskowanie-statystyczne",
  lecture_num   = "04",
  lecture_title = "Testowanie hipotez",
  module_label  = "Moduł IV",
  chapters      = .chapters
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  lc <- lecture_server(.chapters, input, output, session)

  # Nawigacja między rozdziałami idzie przez lc_chapter_next() w modułach
  # (sendCustomMessage("switchToChapter", ...) → lc__switch_chapter).

  # ==========================================================================
  # CHAPTER SERVERS
  # ==========================================================================

  ch1_server(input, output, session)
  ch2h_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch6_server(input, output, session)
  ch7_server(input, output, session)
  ch_drzewo_server(input, output, session)
  ch8_server(input, output, session)
  ch9_server(input, output, session)

}

shinyApp(ui = ui, server = server)
