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
# KOLORY
# ============================================================================

# Kolory specyficzne dla testowania hipotez
col_h0         <- "#3498db"    # niebieski - hipoteza zerowa / rozklad pod H0
col_h1         <- "#e74c3c"    # czerwony - hipoteza alternatywna / obszar odrzucenia
col_pvalue     <- "#f39c12"    # pomaranczowy - p-wartosc
col_accept     <- "#27ae60"    # zielony - brak podstaw do odrzucenia
col_reject     <- "#e74c3c"    # czerwony - odrzucenie H0
col_effect     <- "#9b59b6"    # fioletowy - wielkosc efektu
col_paired     <- "#1abc9c"    # morski - dane parowe

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
source(file.path(app_dir, "modules", "ch8_sciaga.R"),           local = TRUE)

# ============================================================================
# GLOBAL UI HEADER (CSS, JS)
# ============================================================================

global_header <- tagList(
  withMathJax(),
  tags$head(
    includeCSS(file.path(project_root, "R", "shared_styles.css")),
    includeScript(file.path(project_root, "R", "shared_toc.js"))
  ))

# ============================================================================
# UI
# ============================================================================

ui <- navbarPage(
  "Wnioskowanie statystyczne",
  id = "main_nav",
  theme = bs_theme(bootswatch = "sandstone"),
  header = global_header,
  ch1_ui,
  ch2h_ui,
  ch2_ui,
  ch3_ui,
  ch4_ui,
  ch5_ui,
  ch6_ui,
  ch7_ui,
  ch8_ui
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # ==========================================================================
  # NAWIGACJA MIEDZY ROZDZIALAMI
  # ==========================================================================

  observeEvent(input$ch1_next, {
    updateNavbarPage(session, "main_nav", selected = "2. Formu\u0142owanie hipotez")
  })
  observeEvent(input$ch2h_next, {
    updateNavbarPage(session, "main_nav", selected = "3. Jedna zmienna ilo\u015bciowa")
  })
  observeEvent(input$ch2_next, {
    updateNavbarPage(session, "main_nav", selected = "4. Jedna zmienna jako\u015bciowa")
  })
  observeEvent(input$ch3_next, {
    updateNavbarPage(session, "main_nav", selected = "5. Dwie zmienne ilo\u015bciowe")
  })
  observeEvent(input$ch4_next, {
    updateNavbarPage(session, "main_nav", selected = "6. Dwie zmienne jako\u015bciowe")
  })
  observeEvent(input$ch5_next, {
    updateNavbarPage(session, "main_nav", selected = "7. Ilo\u015bciowa vs jako\u015bciowa")
  })
  observeEvent(input$ch6_next, {
    updateNavbarPage(session, "main_nav", selected = "8. ANOVA")
  })
  observeEvent(input$ch7_next, {
    updateNavbarPage(session, "main_nav", selected = "9. \u015aci\u0105ga")
  })

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
  ch8_server(input, output, session)

}

shinyApp(ui = ui, server = server)
