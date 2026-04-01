# Strona główna

home_ui <- tabPanel("Strona główna",
  icon = icon("home"),
  fluidRow(column(8, offset = 2,
    br(),
    h2("Interaktywne wykłady ze statystyki"),
    hr(),

    h4("O skrypcie"),
    p("Niniejszy skrypt zawiera interaktywne materiały do kursu statystyki.
       Każdy wykład łączy teorię z symulacjami i widgetami, które pozwalają
       eksperymentować z pojęciami statystycznymi w czasie rzeczywistym."),

    h4("Jak korzystać"),
    tags$ul(
      tags$li(tags$strong("Nawigacja"), " — użyj zakładek na górze"),
      tags$li(tags$strong("Widgety"), " — suwaki, przyciski i selektory pozwalają zmieniać parametry"),
      tags$li(tags$strong("Symulacje"), " — kliknij przycisk, aby uruchomić losowanie")
    ),

    hr(),
    h4("Spis wykładów"),

    tags$ol(
      tags$li(tags$strong("Estymacja punktowa"), " — od próby do populacji, estymator w akcji"),
      tags$li(tags$strong("Idea przedziałów ufności"), " — 100 CI, budowa przedziału, interpretacja"),
      tags$li(tags$em("Przedział dla średniej"), " — (w przygotowaniu)"),
      tags$li(tags$em("Przedział dla proporcji"), " — (w przygotowaniu)"),
      tags$li(tags$em("Co wpływa na szerokość?"), " — (w przygotowaniu)"),
      tags$li(tags$em("Ściąga"), " — (w przygotowaniu)")
    )
  ))
)
