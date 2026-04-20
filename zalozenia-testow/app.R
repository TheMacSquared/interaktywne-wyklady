# Zalozenia testow - interaktywny przewodnik
# Scrollowalny skrypt: zalozenia wszystkich poznanych metod i alternatywy

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(rstatix)
library(broom)
library(lmtest)

# ============================================================================
# KOLORY
# ============================================================================

col_ok         <- "#27ae60"    # zielony - zalozenie spelnione
col_fail       <- "#e74c3c"    # czerwony - zalozenie naruszone
col_test       <- "#3498db"    # niebieski - dane/test
col_alt        <- "#9b59b6"    # fioletowy - alternatywa

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

source(file.path(app_dir, "modules", "helpers.R"),          local = TRUE)
source(file.path(app_dir, "modules", "ch1_normalnosc.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch2_wariancje.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch3_regresja.R"),     local = TRUE)
source(file.path(app_dir, "modules", "ch4_chi_fisher.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch5_mapa.R"),         local = TRUE)
source(file.path(app_dir, "modules", "ch6_sciaga.R"),       local = TRUE)

# ============================================================================
# GLOBAL UI HEADER (CSS, JS)
# ============================================================================

global_header <- tagList(
  withMathJax(),
  tags$head(
    includeCSS(file.path(project_root, "R", "shared_styles.css")),
    tags$style(HTML("
  /* Decision table */
  .decision-table { width: 100%; border-collapse: collapse; margin: 15px 0; }
  .decision-table th {
    background: #3498db; color: white; padding: 10px 12px;
    text-align: left; font-size: 14px;
  }
  .decision-table td {
    padding: 8px 12px; border-bottom: 1px solid #dee2e6; font-size: 14px;
  }
  .decision-table tr:nth-child(even) { background: #f8f9fa; }
  .decision-table tr:hover { background: #eaf4fc; }
  ")),
    includeScript(file.path(project_root, "R", "shared_toc.js"))
  ))

# ============================================================================
# UI
# ============================================================================

ui <- navbarPage(
  "Za\u0142o\u017cenia test\u00f3w",
  id = "main_nav",
  theme = bs_theme(bootswatch = "sandstone"),
  header = global_header,
  ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui, ch6_ui
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  observeEvent(input$ch1_next, {
    updateNavbarPage(session, "main_nav", selected = "2. Jednorodne wariancje")
  })
  observeEvent(input$ch2_next, {
    updateNavbarPage(session, "main_nav", selected = "3. Za\u0142o\u017cenia regresji")
  })
  observeEvent(input$ch3_next, {
    updateNavbarPage(session, "main_nav", selected = "4. Za\u0142o\u017cenia \u03c7\u00b2 i Fishera")
  })
  observeEvent(input$ch4_next, {
    updateNavbarPage(session, "main_nav", selected = "5. Mapa metod")
  })
  observeEvent(input$ch5_next, {
    updateNavbarPage(session, "main_nav", selected = "6. \u015aci\u0105ga")
  })

  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch6_server(input, output, session)
}

shinyApp(ui = ui, server = server)
