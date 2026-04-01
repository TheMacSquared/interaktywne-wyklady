# Prototyp: Quarto narracja + Shiny widgety
# Jeden app, nawigacja navbarPage + navbarMenu per wykład

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Załaduj helpery
source("modules/helpers.R", local = TRUE)

# Załaduj moduły rozdziałów
source("modules/home.R", local = TRUE)
source("modules/ch1_estymacja.R", local = TRUE)
source("modules/ch2_idea.R", local = TRUE)
source("modules/ch_reg1_liniowa.R", local = TRUE)

# ===========================================================================
# UI
# ===========================================================================

ui <- navbarPage(
  "Interaktywne wykłady ze statystyki",
  id = "main_nav",
  theme = bs_theme(bootswatch = "sandstone"),
  header = tagList(
    tags$head(tags$link(rel = "stylesheet", href = "styles.css"))
  ),

  home_ui,

  # Wykład 3: Przedziały ufności (prototyp — 2 rozdziały)
  navbarMenu("3. Przedziały ufności",
    ch1_ui,
    ch2_ui
  ),

  # Wykład 5: Regresja (prototyp — 1 rozdział)
  navbarMenu("5. Regresja",
    ch_reg1_ui
  )
)

# ===========================================================================
# SERVER
# ===========================================================================

server <- function(input, output, session) {
  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch_reg1_server(input, output, session)
}

shinyApp(ui = ui, server = server)
