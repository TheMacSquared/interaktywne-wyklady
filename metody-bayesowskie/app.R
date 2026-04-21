# Metody bayesowskie - interaktywny przewodnik
# Prior, posterior, Bayes Factor, HDI, regresja bayesowska
# Konsekwentne porownanie z podejsciem czestosciowym

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstatix)
library(broom)
library(BayesFactor)
library(rstanarm)

# ============================================================================
# KOLORY
# ============================================================================

col_primary    <- "#3498db"
col_secondary  <- "#e74c3c"
col_success    <- "#27ae60"
col_warning    <- "#f39c12"
col_dark       <- "#2c3e50"
col_purple     <- "#9b59b6"
col_teal       <- "#1abc9c"

# Paleta paradygmatow
col_frequentist <- "#e74c3c"    # czerwony - freq
col_bayesian    <- "#9b59b6"    # fioletowy - bayes
col_prior       <- "#95a5a6"    # szary - prior
col_likelihood  <- "#f39c12"    # pomaranczowy - likelihood
col_posterior   <- "#9b59b6"    # fioletowy - posterior
col_hdi         <- "#f39c12"    # pomaranczowy - HDI
col_reference   <- "#2c3e50"    # ciemny - linia odniesienia (0, mu0, OR=1)

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

source(file.path(project_root, "R", "shared.R"),               local = TRUE)

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
source(file.path(app_dir, "modules", "ch12_cwiczenia.R"),       local = TRUE)

# ============================================================================
# GLOBAL UI HEADER (CSS, JS)
# ============================================================================

global_header <- tagList(
  withMathJax(),
  tags$head(
    includeCSS(file.path(project_root, "R", "shared_styles.css")),
    tags$style(HTML("
  /* Metody bayesowskie: formula-box override */
  .formula-box { font-family: monospace; }
  ")),
    includeScript(file.path(project_root, "R", "shared_toc.js"))
  ))

# ============================================================================
# UI
# ============================================================================

ui <- navbarPage(
  "Metody bayesowskie",
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
  ch10_ui,
  ch11_ui,
  ch12_ui
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # NAWIGACJA MIEDZY ROZDZIALAMI
  observeEvent(input$ch1_next, {
    updateNavbarPage(session, "main_nav", selected = "2. BF vs p-wartość")
  })
  observeEvent(input$ch2_next, {
    updateNavbarPage(session, "main_nav", selected = "3. HDI vs CI")
  })
  observeEvent(input$ch3_next, {
    updateNavbarPage(session, "main_nav", selected = "4. Jedna próba")
  })
  observeEvent(input$ch4_next, {
    updateNavbarPage(session, "main_nav", selected = "5. Dwie grupy")
  })
  observeEvent(input$ch5_next, {
    updateNavbarPage(session, "main_nav", selected = "6. ANOVA")
  })
  observeEvent(input$ch6_next, {
    updateNavbarPage(session, "main_nav", selected = "7. Tabele krzyżowe")
  })
  observeEvent(input$ch7_next, {
    updateNavbarPage(session, "main_nav", selected = "8. Korelacja")
  })
  observeEvent(input$ch8_next, {
    updateNavbarPage(session, "main_nav", selected = "9. Regresja liniowa")
  })
  observeEvent(input$ch9_next, {
    updateNavbarPage(session, "main_nav", selected = "10. Regresja logistyczna")
  })
  observeEvent(input$ch10_next, {
    updateNavbarPage(session, "main_nav", selected = "11. Ściąga")
  })
  observeEvent(input$ch11_to_ch12, {
    updateNavbarPage(session, "main_nav", selected = "12. Ćwiczenia")
  })

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
