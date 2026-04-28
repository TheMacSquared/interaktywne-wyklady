# ============================================================================
# CHAPTER 1: Od proby do populacji
# ============================================================================

ch1_ui <- list(
  id    = "ch-estymacja",
  num   = "01",
  title = "Od próby do populacji",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 01 · Przedziały ufności",
      num    = "01",
      title  = "Od próby do populacji.",
      lead   = "Wiemy już, że średnia z próby zbiega do rozkładu normalnego
                (CTG). Teraz wykorzystamy to do szacowania parametrów populacji."
    ),

    lc_h2("ch1-estymacja", "Estymacja — od próby do populacji"),

    tagList(
      p("W statystyce rzadko znamy parametry całej populacji.
        Zamiast tego pobieramy ", tags$strong("próbę"), " i na jej podstawie
        ", tags$strong("szacujemy"), " (estymujemy) nieznany parametr."),
      p("Na przykład: nie znamy średniego wzrostu wszystkich studentów
        w Polsce, ale możemy zmierzyć 100 osób i obliczyć średnią z próby ",
        withMathJax("\\(\\bar{x}\\)"), " jako ", tags$strong("estymator"),
        " średniej populacyjnej ", withMathJax("\\(\\mu\\)"), ".")
    ),

    lc_h2("ch1-estymator", "Estymator w akcji"),

    tagList(
      p("Zobaczmy, jak działa estymacja. Znamy prawdziwe ",
        withMathJax("\\(\\mu\\)"), " populacji (wrzosowa linia).
        Za każdym razem losujemy próbę i obliczamy ",
        withMathJax("\\(\\bar{x}\\)"), ".")
    ),

    figure_panel(
      label = "Ryc. 1.1", title = "Losowanie prób z populacji",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch1_dist", "Rozkład populacji:",
            choices = c(
              "Normalny (wzrost)"         = "normal",
              "Wykładniczy (prawoskośny)" = "exponential",
              "Jednostajny"               = "uniform",
              "Dwumodalny"                = "bimodal"
            ),
            selected = "normal"
          ),
          sliderInput("ch1_n", "Wielkość próby (n):",
                      min = 5, max = 200, value = 30, step = 5),
          hr(),
          lc_stack(gap = "md",
            actionButton("ch1_draw_1", "Pobierz 1 próbę",
                         class = "lc-btn-primary", width = "100%"),
            actionButton("ch1_draw_20", "Pobierz 20 prób",
                         class = "lc-btn-warning", width = "100%"),
            actionButton("ch1_reset", "Reset",
                         class = "lc-btn-secondary-outline", width = "100%")
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

    inline_callout(label = "Obserwacja", color = "ok",
      "Każda próba daje inny wynik. Ale średnie z prób skupiają się
       wokół prawdziwego μ — im większe n, tym bliżej."
    ),

    lc_h2("ch1-wlasnosci", "Trzy własności dobrego estymatora"),

    tagList(
      p("Skąd wiemy, czy dany estymator jest „dobry”? Statystycy oceniają
        estymatory względem trzech podstawowych własności: ",
        tags$strong("nieobciążoności"), ", ",
        tags$strong("efektywności"), " i ", tags$strong("zgodności"), "."),

      lc_h3("(1) Nieobciążoność"),
      p("Estymator ", withMathJax("\\(\\hat{\\theta}\\)"), " parametru ",
        withMathJax("\\(\\theta\\)"), " jest ", tags$strong("nieobciążony"),
        ", gdy:"),
      lc_formula_box(
        withMathJax("$$E[\\hat{\\theta}] = \\theta$$")
      ),
      p("Czyli: ", tags$em("średnio"),
        " (z bardzo wielu hipotetycznych prób) trafia dokładnie w prawdziwy
        parametr. Brak systematycznego błędu w jedną stronę."),
      p(tags$strong("Przykład:"),
        " średnia z próby ", withMathJax("\\(\\bar{x}\\)"),
        " jest nieobciążonym estymatorem średniej populacji ",
        withMathJax("\\(\\mu\\)"),
        ". Jakkolwiek pojedynczy ", withMathJax("\\(\\bar{x}\\)"),
        " może być większy lub mniejszy od ", withMathJax("\\(\\mu\\)"),
        ", to średnia ze ", tags$em("wszystkich możliwych"),
        " prób równa się dokładnie ", withMathJax("\\(\\mu\\)"), "."),
      p(tags$strong("Kontrprzykład:"),
        " wariancja z próby liczona ze wzoru ",
        withMathJax("\\(\\frac{1}{n}\\sum(x_i - \\bar{x})^2\\)"),
        " jest ", tags$em("obciążona"),
        " (średnio zaniża prawdziwą wariancję populacji). Dlatego
        standardowo dzielimy przez ", withMathJax("\\(n-1\\)"),
        " zamiast przez ", withMathJax("\\(n\\)"),
        " — to poprawka, która czyni estymator nieobciążonym."),

      lc_h3("(2) Efektywność"),
      p("Spośród wszystkich estymatorów nieobciążonych najlepszy jest ten,
        który ma ", tags$strong("najmniejszą wariancję"),
        " — czyli najmniej waha się z próby na próbę. Taki estymator nazywamy ",
        tags$strong("efektywnym"), "."),
      p("Intuicja: dwa estymatory mogą być ", tags$em("średnio"),
        " równie celne (oba nieobciążone), ale jeden może regularnie dać
        wynik bliższy prawdy, a drugi często strzelać daleko — w różne
        strony, co po uśrednieniu się znosi. Wybieramy ten ",
        tags$em("ciasny"), "."),
      p(tags$strong("Przykład:"),
        " dla rozkładu normalnego zarówno średnia, jak i mediana z próby
        są nieobciążone. Ale średnia ma mniejszą wariancję — dokładnie ",
        tags$strong("π/2 ≈ 1.57 razy mniejszą"),
        " niż mediana. Dlatego w fizyce, chemii i każdym laboratoryjnym
        pomiarze standardem jest średnia arytmetyczna."),
      p(tags$strong("Uwaga:"),
        " efektywność zależy od rozkładu danych. Dla danych z outlierami
        mediana może być efektywniejsza niż średnia."),

      lc_h3("(3) Zgodność"),
      p("Estymator jest ", tags$strong("zgodny"),
        ", gdy z rosnącą wielkością próby zbiega do prawdziwego parametru:"),
      lc_formula_box(
        withMathJax("$$\\hat{\\theta}_n \\xrightarrow{p} \\theta \\quad \\text{gdy} \\quad n \\to \\infty$$")
      ),
      p("Innymi słowy: dla bardzo dużej próby estymator trafia w parametr ",
        tags$em("prawie na pewno"),
        ". Im więcej obserwacji, tym mniejszy rozrzut estymatora wokół prawdy."),
      p(tags$strong("Przykład:"),
        " średnia z próby jest zgodnym estymatorem średniej populacji.
        Z prawa wielkich liczb wiemy, że ", withMathJax("\\(\\bar{x} \\to \\mu\\)"),
        " gdy ", withMathJax("\\(n \\to \\infty\\)"),
        ". Dla średniej obowiązuje wzór ",
        withMathJax("\\(SD(\\bar{x}) = \\sigma/\\sqrt{n}\\)"),
        " — odchylenie standardowe maleje proporcjonalnie do ",
        withMathJax("\\(1/\\sqrt{n}\\)"), "."),
      p(tags$strong("Praktyczna konsekwencja:"),
        " żeby zmniejszyć niepewność estymatora dwa razy, musisz ",
        tags$strong("czterokrotnie"), " zwiększyć próbę.
        To dlatego duże badania są takie drogie.")
    ),

    inline_callout(label = "Hierarchia", color = "wskazowka",
      "Najpierw chcemy, żeby estymator był nieobciążony (trafiał średnio
       w cel). Spośród nieobciążonych wybieramy najefektywniejszy
       (najmniej się waha). I oczywiście chcemy, żeby był zgodny —
       trafiał dokładniej, gdy zbieramy więcej danych."
    ),

    lc_h2("ch1-punkt-nie-wystarczy", "Dlaczego sam punkt nie wystarczy?"),

    tagList(
      p("Nawet najlepszy estymator punktowy zmienia się z próby na próbę.
        Podanie samej liczby ", withMathJax("\\(\\bar{x} = 171.3\\)"),
        " nie mówi nic o tym, jak bardzo możemy się mylić."),
      p("Potrzebujemy czegoś więcej — ", tags$strong("przedziału"),
        ", który powie: ",
        tags$em("„z 95% pewnością prawdziwa wartość leży między … a …”"),
        ".")
    ),

    figure_panel(
      label = "Ryc. 1.2", title = "Wahania estymatora",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch1_fluct_n", "Wielkość próby (n):",
                      min = 5, max = 200, value = 10, step = 5),
          helpText("Każde kliknięcie losuje nową próbę. Obserwuj,
                    jak bardzo skacze estymata."),
          actionButton("ch1_fluct_draw", "Losuj próbę",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch1_fluct_plot", height = "300px")
        )
      )
    ),

    inline_callout(label = "Wniosek", color = "uwaga",
      "Estymacja punktowa to za mało. Potrzebujemy przedziału
       ufności — zakresu wartości, który z określonym
       prawdopodobieństwem zawiera prawdziwy parametr."
    ),

    lc_chapter_next(
      num       = "02",
      title     = "Idea przedziałów",
      lead      = "jak skonstruować przedział ufności i co on naprawdę mówi",
      target_id = "ch-idea"
    )
  )
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
    lc_stat_box("Prób", n_est, color = col_ci)
  })

  output$ch1_estimates_plot <- renderPlot({
    est <- ch1_estimates()
    params <- get_population_params(input$ch1_dist)

    if (nrow(est) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Pobierz próbę'",
                 size = 6, color = upwr_reference) +
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
        theme_upwr()
    }
  })

  output$ch1_estimates_stats <- renderUI({
    est <- ch1_estimates()
    if (nrow(est) == 0) return(NULL)
    params <- get_population_params(input$ch1_dist)
    tagList(
      lc_stat_box("μ", round(params$mu, 2), color = col_true),
      lc_stat_box("Śr. estymat", round(mean(est$xbar), 2), color = col_estimate),
      lc_stat_box("SD estymat", round(sd(est$xbar), 2), color = upwr_secondary)
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
                 size = 6, color = upwr_reference) +
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
        theme_upwr()
    }
  })
}
