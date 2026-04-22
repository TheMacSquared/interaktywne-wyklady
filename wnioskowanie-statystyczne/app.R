# Wnioskowanie statystyczne - interaktywny przewodnik
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

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch1_ui, ch2h_ui, ch2_ui, ch3_ui, ch4_ui,
                  ch5_ui, ch6_ui, ch7_ui, ch_drzewo_ui, ch8_ui)

ui <- lecture_page(
  lecture_id    = "wnioskowanie-statystyczne",
  lecture_num   = "03",
  lecture_title = "Wnioskowanie statystyczne",
  module_label  = "Moduł III",
  chapters      = .chapters
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  lc <- lecture_server(.chapters, input, output, session)

  # Stara nawigacja z przycisków chapter-transition — tymczasowo utrzymana,
  # zanim zmigrujemy moduły na lc_chapter_next() (Etap 3).
  observeEvent(input$ch1_next,        { lc$switch_to("ch-hipotezy")         })
  observeEvent(input$ch2h_next,       { lc$switch_to("ch-jedna-ilosciowa")  })
  observeEvent(input$ch2_next,        { lc$switch_to("ch-jedna-jakosciowa") })
  observeEvent(input$ch3_next,        { lc$switch_to("ch-korelacja")        })
  observeEvent(input$ch4_next,        { lc$switch_to("ch-dwie-jakosciowe")  })
  observeEvent(input$ch5_next,        { lc$switch_to("ch-dwie-grupy")       })
  observeEvent(input$ch6_next,        { lc$switch_to("ch-anova")            })
  observeEvent(input$ch7_next,        { lc$switch_to("ch-drzewo")           })
  observeEvent(input$ch_drzewo_next,  { lc$switch_to("ch-sciaga")           })

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

}

shinyApp(ui = ui, server = server)
