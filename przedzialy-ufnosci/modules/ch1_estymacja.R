# ============================================================================
# CHAPTER 1: Od proby do populacji
# ============================================================================

ch1_ui <- tabPanel("1. Od próby do populacji",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Wiemy już, że średnia z próby zbiega do rozkładu normalnego (CTG).
       Teraz wykorzystamy to do szacowania parametrów populacji."
    ),

    div(class = "section-title", "Estymacja — od próby do populacji"),

    div(class = "narrative",
      p("W statystyce rzadko znamy parametry całej populacji.
        Zamiast tego pobieramy ", tags$b("próbę"), " i na jej podstawie
        ", tags$b("szacujemy"), " (estymujemy) nieznany parametr."),
      p("Na przykład: nie znamy średniego wzrostu wszystkich studentów
        w Polsce, ale możemy zmierzyć 100 osób i obliczyć średnią z próby ",
        withMathJax("\\(\\bar{x}\\)"), " jako ", tags$b("estymator"),
        " średniej populacyjnej ", withMathJax("\\(\\mu\\)"), ".")
    ),

    # ========================================================================
    # WIDGET 1: Estymator w akcji
    # ========================================================================
    div(class = "section-title", "Estymator w akcji"),

    div(class = "narrative",
      p("Zobaczmy, jak działa estymacja. Znamy prawdziwe ",
        withMathJax("\\(\\mu\\)"), " populacji (fioletowa linia).
        Za każdym razem losujemy próbę i obliczamy ",
        withMathJax("\\(\\bar{x}\\)"), ".")
    ),

    div(class = "widget-block",
      h4("Losowanie prób z populacji"),
      fluidRow(
        column(4,
          selectInput("ch1_dist", "Rozkład populacji:",
            choices = c(
              "Normalny (wzrost)"           = "normal",
              "Wykładniczy (prawoskośny)" = "exponential",
              "Jednostajny"                 = "uniform",
              "Dwumodalny"                  = "bimodal"
            ),
            selected = "normal"
          ),
          sliderInput("ch1_n", "Wielkość próby (n):",
                      min = 5, max = 200, value = 30, step = 5),
          hr(),
          div(style = "display: flex; flex-direction: column; gap: 8px;",
            actionButton("ch1_draw_1", "Pobierz 1 próbę",
                         class = "btn-primary", width = "100%"),
            actionButton("ch1_draw_20", "Pobierz 20 prób",
                         class = "btn-warning", width = "100%"),
            actionButton("ch1_reset", "Reset",
                         class = "btn-outline-secondary", width = "100%")
          ),
          br(),
          uiOutput("ch1_count_info")
        ),
        column(8,
          plotOutput("ch1_estimates_plot", height = "400px"),
          uiOutput("ch1_estimates_stats")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Obserwacja:"),
      " Każda próba daje inny wynik! Ale średnie z prób
        skupiają się wokół prawdziwego ", withMathJax("\\(\\mu\\)"),
      ". Im większe n, tym bliżej."
    ),

    # ========================================================================
    # SEKCJA 2: Trzy własności dobrego estymatora (tylko tekst)
    # ========================================================================
    div(class = "section-title", "Trzy własności dobrego estymatora"),

    div(class = "narrative",
      p("Skąd wiemy, czy dany estymator jest \"dobry\"? Statystycy oceniają estymatory
        względem trzech podstawowych własności: ", tags$b("nieobciążoności"), ", ",
        tags$b("efektywności"), " i ", tags$b("zgodności"), ".")
    ),

    # (1) Nieobciazonosc
    div(class = "widget-block",
      h4("(1) Nieobciążonosć"),
      div(class = "narrative",
        p("Estymator ", withMathJax("\\(\\hat{\\theta}\\)"), " parametru ",
          withMathJax("\\(\\theta\\)"), " jest ", tags$b("nieobciążony"), ", gdy:"),
        div(class = "formula-box",
          withMathJax("$$E[\\hat{\\theta}] = \\theta$$")
        ),
        p("Czyli: ", tags$em("średnio"),
          " (z bardzo wielu hipotetycznych prób) trafia dokładnie w prawdziwy parametr.
          Brak systematycznego błędu w jedną stronę."),
        p(tags$b("Przykład:"),
          " średnia z próby ", withMathJax("\\(\\bar{x}\\)"),
          " jest nieobciążonym estymatorem średniej populacji ",
          withMathJax("\\(\\mu\\)"),
          ". Jakkolwiek pojedynczy ", withMathJax("\\(\\bar{x}\\)"),
          " może być większy lub mniejszy od ", withMathJax("\\(\\mu\\)"),
          ", to średnia ze ", tags$em("wszystkich możliwych"), " prób równa się dokładnie ",
          withMathJax("\\(\\mu\\)"), "."),
        p(tags$b("Kontrprzykład:"),
          " wariancja z próby liczona ze wzoru ",
          withMathJax("\\(\\frac{1}{n}\\sum(x_i - \\bar{x})^2\\)"),
          " jest ", tags$em("obciążona"),
          " (średnio zaniża prawdziwą wariancję populacji). Dlatego
          standardowo dzielimy przez ", withMathJax("\\(n-1\\)"),
          " zamiast przez ", withMathJax("\\(n\\)"),
          " — to poprawka, która czyni estymator nieobciążonym.")
      )
    ),

    # (2) Efektywnosc
    div(class = "widget-block",
      h4("(2) Efektywność"),
      div(class = "narrative",
        p("Spośród wszystkich estymatorów nieobciążonych najlepszy jest ten,
          który ma ", tags$b("najmniejszą wariancję"),
          " — czyli najmniej waha się z próby na próbę.
          Taki estymator nazywamy ", tags$b("efektywnym"), "."),
        p("Intuicja: dwa estymatory mogą być ", tags$em("średnio"),
          " równie celne (oba nieobciążone), ale jeden może
          regularnie dać wynik bliższy prawdy, a drugi często strzelać
          daleko — w różne strony, co po uśrednieniu się znosi.
          Wybieramy ten ", tags$em("ciasny"), "."),
        p(tags$b("Przykład:"),
          " dla rozkładu normalnego zarówno średnia, jak i mediana z próby
          są nieobciążone. Ale średnia ma mniejszą wariancję — dokładnie ",
          tags$b("π/2 ≈ 1.57 razy mniejszą"),
          " niż mediana. Dlatego w fizyce, chemii i każdym laboratoryjnym
          pomiarze standardem jest średnia arytmetyczna."),
        p(tags$b("Uwaga:"),
          " efektywność zależy od rozkładu danych. Dla danych z outlierami
          mediana może być efektywniejsza niż średnia.")
      )
    ),

    # (3) Zgodnosc
    div(class = "widget-block",
      h4("(3) Zgodność"),
      div(class = "narrative",
        p("Estymator jest ", tags$b("zgodny"),
          ", gdy z rosnącą wielkością próby zbiega do prawdziwego parametru:"),
        div(class = "formula-box",
          withMathJax("$$\\hat{\\theta}_n \\xrightarrow{p} \\theta \\quad \\text{gdy} \\quad n \\to \\infty$$")
        ),
        p("Innymi słowy: dla bardzo dużej próby estymator trafia w parametr
          ", tags$em("prawie na pewno"),
          ". Im więcej obserwacji, tym mniejszy rozrzut estymatora wokół prawdy."),
        p(tags$b("Przykład:"),
          " średnia z próby jest zgodnym estymatorem średniej populacji.
          Z prawa wielkich liczb wiemy, że ", withMathJax("\\(\\bar{x} \\to \\mu\\)"),
          " gdy ", withMathJax("\\(n \\to \\infty\\)"),
          ". Dla średniej obowiązuje wzór ",
          withMathJax("\\(SD(\\bar{x}) = \\sigma/\\sqrt{n}\\)"),
          " — odchylenie standardowe maleje proporcjonalnie do ",
          withMathJax("\\(1/\\sqrt{n}\\)"), "."),
        p(tags$b("Praktyczna konsekwencja:"),
          " żeby zmniejszyć niepewność estymatora dwa razy, musisz ",
          tags$b("czterokrotnie"), " zwiększyć próbę.
          To dlaczego duże badania są takie drogie.")
      )
    ),

    div(class = "callout-info",
      tags$strong("Hierarchia własności:"),
      " Najpierw chcemy, żeby estymator był ", tags$b("nieobciążony"),
      " (trafiał średnio w cel). Spośród nieobciążonych wybieramy ten ",
      tags$b("najefektywniejszy"),
      " (najmniej się waha). I oczywiście chcemy, żeby był ",
      tags$b("zgodny"), " — czyli trafiał dokładniej, gdy zbieramy więcej danych."
    ),

    # ========================================================================
    # WIDGET 3: Dlaczego punkt nie wystarczy?
    # ========================================================================
    div(class = "section-title", "Dlaczego sam punkt nie wystarczy?"),

    div(class = "narrative",
      p("Nawet najlepszy estymator punktowy zmienia się z próby na próbę.
        Podanie samej liczby ", withMathJax("\\(\\bar{x} = 171.3\\)"),
        " nie mówi nic o tym, jak bardzo możemy się mylić."),
      p("Potrzebujemy czegoś więcej — ", tags$b("przedziału"),
        ", który powie: ", tags$em("\"z 95% pewnością prawdziwa wartość leży między ... a ...\""),
        ".")
    ),

    div(class = "widget-block",
      h4("Wahania estymatora"),
      fluidRow(
        column(4,
          sliderInput("ch1_fluct_n", "Wielkość próby (n):",
                      min = 5, max = 200, value = 10, step = 5),
          helpText("Każde kliknięcie losuje nową próbę.
                    Obserwuj, jak bardzo skacze estymata."),
          actionButton("ch1_fluct_draw", "Losuj próbę",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch1_fluct_plot", height = "300px")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Wniosek:"),
      " Estymacja punktowa to za mało. Potrzebujemy ",
      tags$b("przedziału ufności"), " — zakresu wartości,
      który z określonym prawdopodobieństwem zawiera prawdziwy parametr."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: jak skonstruować taki przedział?"),
      actionButton("ch1_next", "Dalej → 2. Idea przedziałów",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  # --- Widget 1: Estymator w akcji ---
  ch1_estimates <- reactiveVal(data.frame(
    i = integer(0), xbar = numeric(0)
  ))

  draw_samples <- function(k) {
    dist <- input$ch1_dist
    n <- input$ch1_n
    params <- get_population_params(dist)
    old <- ch1_estimates()
    new_rows <- lapply(seq_len(k), function(j) {
      samp <- generate_population_sample(dist, n)
      data.frame(i = nrow(old) + j, xbar = mean(samp))
    })
    ch1_estimates(rbind(old, do.call(rbind, new_rows)))
  }

  observeEvent(input$ch1_draw_1, draw_samples(1))
  observeEvent(input$ch1_draw_20, draw_samples(20))
  observeEvent(input$ch1_reset, {
    ch1_estimates(data.frame(i = integer(0), xbar = numeric(0)))
  })
  observeEvent(input$ch1_dist, {
    ch1_estimates(data.frame(i = integer(0), xbar = numeric(0)))
  })

  output$ch1_count_info <- renderUI({
    n_est <- nrow(ch1_estimates())
    div(class = "stat-box", style = paste0("background:", col_primary, ";"),
        paste0("Prób: ", n_est))
  })

  output$ch1_estimates_plot <- renderPlot({
    est <- ch1_estimates()
    params <- get_population_params(input$ch1_dist)

    if (nrow(est) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Pobierz próbę'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      ggplot(est, aes(x = xbar)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = col_ci, alpha = 0.6, color = "white") +
        geom_vline(xintercept = params$mu, color = col_true,
                   linewidth = 1.5, linetype = "dashed") +
        annotate("text", x = params$mu, y = Inf, vjust = 2,
                 label = paste0("μ = ", params$mu),
                 color = col_true, fontface = "bold", size = 5) +
        geom_vline(xintercept = mean(est$xbar), color = col_estimate,
                   linewidth = 1.5, linetype = "solid") +
        annotate("text", x = mean(est$xbar), y = Inf, vjust = 4,
                 label = paste0("średnia x̄ = ", round(mean(est$xbar), 2)),
                 color = col_estimate, fontface = "bold", size = 5) +
        labs(title = "Rozkład estymat średniej",
             x = expression(bar(x)), y = "Gęstość") +
        theme_educational()
    }
  })

  output$ch1_estimates_stats <- renderUI({
    est <- ch1_estimates()
    if (nrow(est) == 0) return(NULL)
    params <- get_population_params(input$ch1_dist)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_true, ";"),
          paste0("μ = ", round(params$mu, 2))),
      div(class = "stat-box", style = paste0("background:", col_estimate, ";"),
          paste0("Śr. estymat = ", round(mean(est$xbar), 2))),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("SD estymat = ", round(sd(est$xbar), 2)))
    )
  })

  # --- Sekcja 2: tylko tekst, brak server logic ---

  # --- Widget 3: Wahania estymatora ---
  ch1_fluct_history <- reactiveVal(data.frame(
    draw = integer(0), xbar = numeric(0)
  ))

  observeEvent(input$ch1_fluct_draw, {
    samp <- generate_population_sample("normal", input$ch1_fluct_n)
    old <- ch1_fluct_history()
    ch1_fluct_history(rbind(old, data.frame(
      draw = nrow(old) + 1, xbar = mean(samp)
    )))
  })

  observeEvent(input$ch1_fluct_n, {
    ch1_fluct_history(data.frame(draw = integer(0), xbar = numeric(0)))
  })

  output$ch1_fluct_plot <- renderPlot({
    df <- ch1_fluct_history()
    params <- get_population_params("normal")

    if (nrow(df) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Losuj próbę'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      ggplot(df, aes(x = draw, y = xbar)) +
        geom_hline(yintercept = params$mu, color = col_true,
                   linewidth = 1.2, linetype = "dashed") +
        geom_point(color = col_estimate, size = 3) +
        geom_line(color = col_estimate, alpha = 0.5) +
        annotate("text", x = max(df$draw), y = params$mu,
                 label = paste0("μ = ", params$mu),
                 vjust = -1, color = col_true, fontface = "bold") +
        labs(title = paste0("Kolejne estymaty średniej (n = ", input$ch1_fluct_n, ")"),
             x = "Numer losowania", y = expression(bar(x))) +
        theme_educational()
    }
  })
}
