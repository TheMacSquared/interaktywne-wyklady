# Przedzialy ufnosci - interaktywny przewodnik
# Scrollowalny skrypt z osadzonymi widgetami do nauczania przedzialow ufnosci

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(rstatix)
library(broom)

# ============================================================================
# KOLORY
# ============================================================================

# Kolory specyficzne dla przedzialow ufnosci
col_ci         <- "#3498db"    # niebieski - przedzial ufnosci
col_miss       <- "#e74c3c"    # czerwony - przedzial nie trafil
col_hit        <- "#27ae60"    # zielony - przedzial trafil
col_estimate   <- "#f39c12"    # pomaranczowy - estymata punktowa
col_true       <- "#9b59b6"    # fioletowy - prawdziwy parametr

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
source(file.path(app_dir, "modules", "ch1_estymacja.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch2_idea.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch3_srednia.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch4_proporcja.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch5_czynniki.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch6_sciaga.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch7_cwiczenia.R"), local = TRUE)

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
  "Przedziały ufności",
  id = "main_nav",
  theme = bs_theme(bootswatch = "sandstone"),
  header = global_header,
  ch1_ui,
  ch2_ui,
  ch3_ui,
  ch4_ui,
  ch5_ui,
  ch6_ui,
  ch7_ui
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # ==========================================================================
  # NAWIGACJA MIEDZY ROZDZIALAMI
  # ==========================================================================

  observeEvent(input$ch1_next, {
    updateNavbarPage(session, "main_nav", selected = "2. Idea przedziałów")
  })
  observeEvent(input$ch2_next, {
    updateNavbarPage(session, "main_nav", selected = "3. Przedział dla średniej")
  })
  observeEvent(input$ch3_next, {
    updateNavbarPage(session, "main_nav", selected = "4. Przedział dla proporcji")
  })
  observeEvent(input$ch4_next, {
    updateNavbarPage(session, "main_nav", selected = "5. Co wpływa na szerokość?")
  })
  observeEvent(input$ch5_next, {
    updateNavbarPage(session, "main_nav", selected = "6. Ściąga")
  })
  observeEvent(input$ch6_to_ch7, {
    updateNavbarPage(session, "main_nav", selected = "7. Ćwiczenia")
  })

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

}

shinyApp(ui = ui, server = server)
