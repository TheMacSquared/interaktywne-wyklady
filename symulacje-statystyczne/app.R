# Symulacje statystyczne - interaktywny przewodnik
# Bootstrap, Jackknife, Permutacje, Cross-Validation, Monte Carlo

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstatix)
library(broom)

# ============================================================================
# KOLORY
# ============================================================================

# Kolory specyficzne dla symulacji statystycznych
col_bootstrap  <- "#3498db"    # niebieski  -- bootstrap / jackknife
col_classical  <- "#f39c12"    # pomaranczowy -- klasyczne metody (kontrast)
col_null_dist  <- "#95a5a6"    # szary -- rozklad pod H0
col_observed   <- "#e74c3c"    # czerwony -- obserwowana statystyka
col_resample   <- "#f39c12"    # pomaranczowy -- jedna proba bootstrapowa
col_cv_train   <- "#27ae60"    # zielony -- MSE treningowe
col_cv_test    <- "#9b59b6"    # fioletowy -- MSE CV (out-of-sample)

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
source(file.path(app_dir, "modules", "ch10_cwiczenia.R"),         local = TRUE)

# ============================================================================
# GLOBAL UI HEADER (CSS, JS)
# ============================================================================

global_header <- tagList(
  withMathJax(),
  tags$head(
    includeCSS(file.path(project_root, "R", "shared_styles.css")),
    tags$style(HTML("
  /* Symulacje: formula-box override */
  .formula-box { font-family: monospace; }
  ")),
    includeScript(file.path(project_root, "R", "shared_toc.js"))
  ))

# ============================================================================
# UI
# ============================================================================

ui <- navbarPage(
  "Symulacje statystyczne",
  id     = "main_nav",
  theme  = bs_theme(bootswatch = "sandstone"),
  header = global_header,
  ch1_ui,
  ch2_ui,
  ch3_ui,
  ch4_ui,
  ch5_ui,
  ch6_ui,
  ch7_ui,
  ch8_ui,
  ch9_ui,
  ch10_ui
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # ==========================================================================
  # NAWIGACJA MIEDZY ROZDZIALAMI
  # ==========================================================================

  observeEvent(input$ch1_next, {
    updateNavbarPage(session, "main_nav",
                     selected = "2. Bootstrap — przedziały")
  })
  observeEvent(input$ch2_next, {
    updateNavbarPage(session, "main_nav",
                     selected = "3. Bootstrap jednej próby")
  })
  observeEvent(input$ch3_next, {
    updateNavbarPage(session, "main_nav",
                     selected = "4. Testy permutacyjne")
  })
  observeEvent(input$ch4_next, {
    updateNavbarPage(session, "main_nav",
                     selected = "5. Jackknife")
  })
  observeEvent(input$ch5_next, {
    updateNavbarPage(session, "main_nav",
                     selected = "6. Cross-validation")
  })
  observeEvent(input$ch6_next, {
    updateNavbarPage(session, "main_nav",
                     selected = "7. Monte Carlo")
  })
  observeEvent(input$ch7_next, {
    updateNavbarPage(session, "main_nav",
                     selected = "8. Kiedy stosować?")
  })
  observeEvent(input$ch8_next, {
    updateNavbarPage(session, "main_nav",
                     selected = "9. Ściąga")
  })
  observeEvent(input$ch9_to_ch10, {
    updateNavbarPage(session, "main_nav",
                     selected = "10. Ćwiczenia")
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
  ch8_server(input, output, session)
  ch9_server(input, output, session)
  ch10_server(input, output, session)

}

shinyApp(ui = ui, server = server)
