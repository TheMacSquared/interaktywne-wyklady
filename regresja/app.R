# Regresja - interaktywny przewodnik
# Scrollowalny skrypt z osadzonymi widgetami do nauczania regresji liniowej i logistycznej

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(rstatix)
library(broom)

# ============================================================================
# KOLORY
# ============================================================================

# Kolory specyficzne dla regresji
col_fit        <- "#3498db"    # niebieski - linia dopasowania
col_residual   <- "#e74c3c"    # czerwony - reszty
col_data       <- "#2c3e50"    # ciemny - punkty danych
col_predict    <- "#27ae60"    # zielony - predykcja
col_ci_band    <- "#3498db"    # niebieski - pasmo ufnosci
col_logit      <- "#9b59b6"    # fioletowy - krzywa logistyczna
col_model_a    <- "#3498db"    # niebieski - model A
col_model_b    <- "#e74c3c"    # czerwony - model B

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
source(file.path(app_dir, "modules", "helpers.R"),         local = TRUE)
source(file.path(app_dir, "modules", "ch1_liniowa.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch2_wieloraka.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch3_logistyczna.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch4_porownanie.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch5_sciaga.R"),     local = TRUE)

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
  "Regresja",
  id = "main_nav",
  theme = bs_theme(bootswatch = "sandstone"),
  header = global_header,
  ch1_ui,
  ch2_ui,
  ch3_ui,
  ch4_ui,
  ch5_ui
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  observeEvent(input$ch1_next, {
    updateNavbarPage(session, "main_nav", selected = "2. Regresja wieloraka")
  })
  observeEvent(input$ch2_next, {
    updateNavbarPage(session, "main_nav", selected = "3. Regresja logistyczna")
  })
  observeEvent(input$ch3_next, {
    updateNavbarPage(session, "main_nav", selected = "4. Por\u00f3wnanie modeli")
  })
  observeEvent(input$ch4_next, {
    updateNavbarPage(session, "main_nav", selected = "5. \u015aci\u0105ga")
  })

  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
}

shinyApp(ui = ui, server = server)
