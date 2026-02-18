# Portal - Interaktywne Narzędzia do Nauczania Statystyki
# Strona główna z linkami do wszystkich aplikacji

library(shiny)

# ============================================================================
# DEFINICJE APLIKACJI
# ============================================================================

app_categories <- list(
  list(
    title = "Podstawy statystyki opisowej",
    icon = "bar-chart",
    apps = list(
      list(
        id = "losowanie-spoznienia",
        title = "Symulator zbierania danych",
        description = "Symulacja zbierania danych o spóźnieniach autobusu. Populacja vs próba, zmienność próbkowania.",
        concepts = c("Populacja vs próba", "Zmienność próbkowania", "Rozkłady skośne")
      ),
      list(
        id = "histogram-builder",
        title = "Budowanie histogramu",
        description = "Interaktywne budowanie histogramu krok po kroku. Wizualizacja danych i binning.",
        concepts = c("Wizualizacja danych", "Binning", "Częstości")
      ),
      list(
        id = "box-plot-builder",
        title = "Wykres pudełkowy",
        description = "Konstrukcja wykresu pudełkowego krok po kroku z wyjaśnieniami.",
        concepts = c("Kwartyle", "IQR", "Outliery")
      ),
      list(
        id = "srednia-vs-mediana",
        title = "Średnia vs Mediana",
        description = "Porównanie miar centralności i ich wrażliwości na wartości odstające.",
        concepts = c("Średnia", "Mediana", "Outliery")
      ),
      list(
        id = "odchylenie-standardowe",
        title = "Odchylenie standardowe",
        description = "Budowanie intuicji o odchyleniu standardowym i rozproszeniu danych.",
        concepts = c("Rozproszenie danych", "Reguła 68-95-99.7")
      ),
      list(
        id = "moments-explorer",
        title = "Momenty rozkładu",
        description = "Eksploracja momentów rozkładu: skośność i kurtoza.",
        concepts = c("Skośność", "Kurtoza")
      ),
      list(
        id = "gra-estymacja",
        title = "Gra w estymację",
        description = "Gra w zgadywanie statystyk - rozwijanie intuicji statystycznej.",
        concepts = c("Intuicja statystyczna", "Gamifikacja")
      )
    )
  ),
  list(
    title = "Rozkłady prawdopodobieństwa",
    icon = "area-chart",
    apps = list(
      list(
        id = "distribution-explorer",
        title = "Rozkłady teoretyczne",
        description = "Wizualizacja rozkładów: normalny, t, chi-kwadrat, gamma, beta, Poisson.",
        concepts = c("Rozkłady ciągłe", "Rozkłady dyskretne", "Parametry rozkładów")
      ),
      list(
        id = "sampling-explorer",
        title = "Próbkowanie z rozkładów",
        description = "Próbkowanie i porównanie prawdopodobieństwa empirycznego z teoretycznym.",
        concepts = c("Próbkowanie", "Prawdopodobieństwo empiryczne vs teoretyczne")
      )
    )
  ),
  list(
    title = "Testowanie hipotez",
    icon = "check-square-o",
    apps = list(
      list(
        id = "zalozenia-testow",
        title = "Założenia testów",
        description = "Weryfikacja założeń testów statystycznych: normalność, homogeniczność wariancji.",
        concepts = c("Normalność", "Homogeniczność wariancji", "Outliery")
      ),
      list(
        id = "test-t-builder",
        title = "Test t krok po kroku",
        description = "Budowanie testu t od hipotez przez statystykę testową po decyzję.",
        concepts = c("Hipotezy", "Statystyka t", "Wartość p")
      ),
      list(
        id = "chi-kwadrat-builder",
        title = "Test chi-kwadrat",
        description = "Test niezależności chi-kwadrat z tabelami kontyngencji.",
        concepts = c("Tabele kontyngencji", "Test niezależności")
      ),
      list(
        id = "korelacja-builder",
        title = "Korelacja",
        description = "Porównanie korelacji Pearsona i Spearmana - związki liniowe i monotoniczne.",
        concepts = c("Korelacja Pearsona", "Korelacja Spearmana")
      )
    )
  ),
  list(
    title = "Regresja i modelowanie",
    icon = "line-chart",
    apps = list(
      list(
        id = "regresja-interakcja",
        title = "Regresja z interakcją",
        description = "Porównanie modelu addytywnego i z interakcją zmiennych.",
        concepts = c("Interakcja zmiennych", "Test F", "Model liniowy")
      )
    )
  )
)

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f7fa;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }

      .header-section {
        background: linear-gradient(135deg, #2c3e50 0%, #3498db 100%);
        color: white;
        padding: 40px 20px;
        margin: -20px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 10px 10px;
      }

      .header-section h1 {
        font-size: 2.2em;
        margin-bottom: 10px;
        font-weight: 300;
      }

      .header-section p {
        font-size: 1.1em;
        opacity: 0.9;
        max-width: 700px;
        margin: 0 auto;
      }

      .category-section {
        margin-bottom: 30px;
      }

      .category-title {
        font-size: 1.4em;
        color: #2c3e50;
        border-bottom: 3px solid #3498db;
        padding-bottom: 8px;
        margin-bottom: 20px;
        font-weight: 600;
      }

      .app-card {
        background: white;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 15px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        transition: all 0.3s ease;
        border-left: 4px solid #3498db;
        cursor: pointer;
        text-decoration: none;
        display: block;
        color: inherit;
      }

      .app-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 16px rgba(0,0,0,0.15);
        border-left-color: #2c3e50;
        text-decoration: none;
        color: inherit;
      }

      .app-card h4 {
        color: #2c3e50;
        margin-top: 0;
        margin-bottom: 8px;
        font-size: 1.15em;
      }

      .app-card p {
        color: #666;
        margin-bottom: 10px;
        font-size: 0.95em;
        line-height: 1.5;
      }

      .concept-tag {
        display: inline-block;
        background: #ecf0f1;
        color: #2c3e50;
        padding: 3px 10px;
        border-radius: 12px;
        font-size: 0.8em;
        margin-right: 5px;
        margin-bottom: 3px;
      }

      .app-link-btn {
        display: inline-block;
        background: #3498db;
        color: white;
        padding: 6px 16px;
        border-radius: 4px;
        text-decoration: none;
        font-size: 0.9em;
        float: right;
        margin-top: -5px;
      }

      .app-link-btn:hover {
        background: #2c3e50;
        color: white;
        text-decoration: none;
      }

      .footer {
        text-align: center;
        color: #999;
        padding: 20px;
        margin-top: 30px;
        font-size: 0.9em;
      }

      .stats-bar {
        display: flex;
        justify-content: center;
        gap: 40px;
        margin-top: 20px;
      }

      .stat-item {
        text-align: center;
      }

      .stat-number {
        font-size: 2em;
        font-weight: 700;
      }

      .stat-label {
        font-size: 0.85em;
        opacity: 0.8;
      }
    "))
  ),

  div(class = "header-section",
    h1("Interaktywne Narzędzia do Nauczania Statystyki"),
    p("Zbiór aplikacji R Shiny do interaktywnego nauczania statystyki na zajęciach akademickich.
       Każda aplikacja ilustruje kluczowe koncepcje statystyczne poprzez wizualizację i eksperymentowanie."),
    div(class = "stats-bar",
      div(class = "stat-item",
        div(class = "stat-number", "14"),
        div(class = "stat-label", "aplikacji")
      ),
      div(class = "stat-item",
        div(class = "stat-number", "4"),
        div(class = "stat-label", "kategorie")
      ),
      div(class = "stat-item",
        div(class = "stat-number", "30+"),
        div(class = "stat-label", "koncepcji")
      )
    )
  ),

  div(class = "container-fluid",
    uiOutput("categories_ui")
  ),

  div(class = "footer",
    "Projekt edukacyjny | Interaktywne Wykłady ze Statystyki"
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  output$categories_ui <- renderUI({
    category_panels <- lapply(app_categories, function(cat) {
      app_cards <- lapply(cat$apps, function(app) {
        concept_tags <- lapply(app$concepts, function(concept) {
          tags$span(class = "concept-tag", concept)
        })

        tags$a(
          href = paste0("/", app$id, "/"),
          class = "app-card",
          h4(app$title),
          p(app$description),
          div(concept_tags),
          span(class = "app-link-btn", "Otwórz")
        )
      })

      div(class = "category-section",
        h3(class = "category-title", cat$title),
        div(app_cards)
      )
    })

    do.call(tagList, category_panels)
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)
