# Rozklady prawdopodobienstwa - interaktywny przewodnik
# Scrollowalny skrypt z osadzonymi widgetami do nauczania rozkladow prawdopodobienstwa

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(jsonlite)

# ============================================================================
# KOLORY
# ============================================================================

# Kolory dla typow rozkladow
col_discrete   <- "#3498db"    # niebieski - rozklady dyskretne
col_continuous <- "#27ae60"    # zielony - rozklady ciagle
col_normal     <- "#9b59b6"    # fioletowy - rozklad normalny
col_binomial   <- "#e67e22"    # pomaranczowy - dwumianowy
col_poisson    <- "#1abc9c"    # morski - Poissona
col_uniform    <- "#3498db"    # niebieski - jednostajny
col_exponential <- "#e74c3c"   # czerwony - wykladniczy
col_geometric  <- "#8e44ad"    # ciemny fiolet - geometryczny
col_t_student  <- "#c0392b"    # ciemny czerwony - t-Studenta
col_chi_sq     <- "#d35400"    # ciemny pomaranczowy - chi-kwadrat
col_lognormal  <- "#16a085"    # ciemny turkusowy - log-normalny

# Paleta kolorow do overlayow scenariuszy (do 5 scenariuszy na wykresie)
col_scenario <- c("#3498db", "#e74c3c", "#27ae60", "#f39c12", "#9b59b6")

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
source(file.path(app_dir, "modules", "ch1_most.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch2_ev_var.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch3_dyskretne.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch4_ciagle.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch5_normalny.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch6_ctg.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch7_sciaga.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch8_quiz.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch9_cwiczenia.R"), local = TRUE)

# ============================================================================
# GLOBAL UI HEADER (CSS, JS)
# ============================================================================

global_header <- tagList(
  withMathJax(),
  tags$head(
    includeCSS(file.path(project_root, "R", "shared_styles.css")),
    tags$style(HTML("
  /* Distribution card */
  .dist-card {
    border: 2px solid #dee2e6; border-radius: 8px;
    padding: 12px; margin-bottom: 15px; background: white;
  }
  ")),
    includeScript(file.path(project_root, "R", "shared_toc.js"))
  ))

# ============================================================================
# UI
# ============================================================================

ui <- navbarPage(
  "Rozk\u0142ady prawdopodobie\u0144stwa",
  id = "main_nav",
  theme = bs_theme(bootswatch = "sandstone"),
  header = global_header,
  ch1_ui,
  ch2_ev_var_ui,
  ch3_ui,
  ch4_ui,
  ch5_ui,
  ch6_ui,
  ch7_ui,
  ch8_ui,
  ch9_ui
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # ==========================================================================
  # NAWIGACJA MIEDZY ROZDZIALAMI
  # ==========================================================================

  observeEvent(input$ch1_next, {
    updateNavbarPage(session, "main_nav", selected = "2. Wart. oczekiwana i wariancja")
  })
  observeEvent(input$ch2ev_next, {
    updateNavbarPage(session, "main_nav", selected = "3. Rozk\u0142ady dyskretne")
  })
  observeEvent(input$ch3_next, {
    updateNavbarPage(session, "main_nav", selected = "4. Rozk\u0142ady ci\u0105g\u0142e")
  })
  observeEvent(input$ch4_next, {
    updateNavbarPage(session, "main_nav", selected = "5. Rozk\u0142ad normalny")
  })
  observeEvent(input$ch5_next, {
    updateNavbarPage(session, "main_nav", selected = "6. Centralne Tw. Graniczne")
  })
  observeEvent(input$ch6_next, {
    updateNavbarPage(session, "main_nav", selected = "7. \u015aci\u0105ga")
  })
  observeEvent(input$ch7_next, {
    updateNavbarPage(session, "main_nav", selected = "8. Quiz")
  })
  observeEvent(input$ch8_to_ch9, {
    updateNavbarPage(session, "main_nav", selected = "9. \u0106wiczenia")
  })

  # ==========================================================================
  # CHAPTER SERVERS
  # ==========================================================================

  ch1_server(input, output, session)
  ch2_ev_var_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch6_server(input, output, session)
  ch7_server(input, output, session)
  ch8_server(input, output, session)
  ch9_server(input, output, session)

}

shinyApp(ui = ui, server = server)
