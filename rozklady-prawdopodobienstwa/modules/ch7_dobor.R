# ============================================================================
# CHAPTER 7: Dobor rozkladu
# ============================================================================

ch7_ui <- tabPanel("7. Dob\u00f3r rozk\u0142adu",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "CTG wyja\u015bnia dominacj\u0119 rozk\u0142adu normalnego. Teraz pytanie praktyczne:
       jak dopasowa\u0107 rozk\u0142ad do konkretnej sytuacji?"
    ),

    div(class = "section-title", "Dob\u00f3r rozk\u0142adu \u2014 kt\u00f3ry model pasuje?"),

    div(class = "narrative",
      p("Ka\u017cdy rozk\u0142ad ma sw\u00f3j 'naturalny habitat' \u2014 typ danych i sytuacj\u0119,
        kt\u00f3r\u0105 najlepiej opisuje. Kluczowe pytania przy doborze:"),
      tags$ol(
        tags$li("Czy zmienna jest ", tags$b("dyskretna"), " czy ", tags$b("ci\u0105g\u0142a"), "?"),
        tags$li("Jaki jest ", tags$b("kszta\u0142t"), " danych? (symetryczny, sko\u015bny, p\u0142aski)"),
        tags$li("Jaki ", tags$b("mechanizm generuje"), " dane? (zliczanie, pomiar, oczekiwanie)")
      )
    ),

    # ========================================================================
    # WIDGET 1: Drzewo decyzyjne
    # ========================================================================
    div(class = "section-title", "Drzewo decyzyjne"),

    div(class = "widget-block",
      h4("Kt\u00f3ry rozk\u0142ad wybra\u0107?"),
      fluidRow(
        column(4,
          radioButtons("ch7_tree_choice", "Wybierz typ zmiennej:",
            choices = c(
              "Zmienna dyskretna" = "discrete",
              "Zmienna ci\u0105g\u0142a"   = "continuous"
            ),
            selected = "discrete"
          ),
          conditionalPanel(
            condition = "input.ch7_tree_choice == 'discrete'",
            radioButtons("ch7_disc_type", "Jaki mechanizm?",
              choices = c(
                "Ka\u017cdy wynik jednakowo prawdop." = "d_uniform",
                "Sta\u0142a liczba pr\u00f3b, sukces/pora\u017cka" = "d_binomial",
                "Zliczanie zdarze\u0144 w czasie/przestrzeni" = "d_poisson",
                "Ile pr\u00f3b do pierwszego sukcesu" = "d_geometric"
              )
            )
          ),
          conditionalPanel(
            condition = "input.ch7_tree_choice == 'continuous'",
            radioButtons("ch7_cont_type", "Jaki kszta\u0142t/mechanizm?",
              choices = c(
                "Symetryczny dzwon" = "c_normal",
                "Czas oczekiwania (prawosko\u015bny)" = "c_exponential",
                "Ka\u017cda warto\u015b\u0107 w przedziale jednakowo" = "c_uniform",
                "Ci\u0119\u017ckie ogony (wnioskowanie)" = "c_t_student",
                "Suma kwadrat\u00f3w (testy \u03c7\u00b2)" = "c_chi_sq",
                "Dane prawosko\u015bne, dodatnie" = "c_lognormal"
              )
            )
          )
        ),
        column(8,
          uiOutput("ch7_tree_info"),
          plotOutput("ch7_tree_plot", height = "250px")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Dopasuj rozklad do danych (gra)
    # ========================================================================
    div(class = "section-title", "Gra: dopasuj rozk\u0142ad do danych"),

    div(class = "narrative",
      p("Sp\u00f3jrz na histogram danych i odgadnij, jaki rozk\u0142ad je wygenerowa\u0142.
        Po wybraniu odpowiedzi zobaczysz, jak dobrze pasuje krzywa teoretyczna.")
    ),

    div(class = "widget-block",
      h4("Dopasuj rozk\u0142ad"),
      fluidRow(
        column(4,
          actionButton("ch7_game_next", "Nowy zestaw danych",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          selectInput("ch7_game_guess", "Tw\u00f3j typ:",
            choices = c(
              "Wybierz..." = "",
              "Normalny" = "normal",
              "Wyk\u0142adniczy" = "exponential",
              "Jednostajny" = "uniform",
              "Dwumianowy" = "binomial",
              "Poissona" = "poisson",
              "Geometryczny" = "geometric",
              "Log-normalny" = "lognormal"
            ),
            selected = ""
          ),
          actionButton("ch7_game_check", "Sprawd\u017a!",
                       class = "btn-success", width = "100%"),
          br(), br(),
          uiOutput("ch7_game_feedback")
        ),
        column(8,
          plotOutput("ch7_game_plot", height = "350px")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: Wykres Q-Q
    # ========================================================================
    div(class = "section-title", "Wykres Q-Q \u2014 diagnostyka normalno\u015bci"),

    div(class = "narrative",
      p("Wykres kwantyl-kwantyl (Q-Q) por\u00f3wnuje kwantyle danych z kwantylami
        rozk\u0142adu normalnego. Je\u015bli punkty le\u017c\u0105 na linii prostej \u2014
        dane s\u0105 w przybli\u017ceniu normalne."),
      tags$ul(
        tags$li(tags$b("Punkty na linii"), " = dane normalne"),
        tags$li(tags$b("Krzywa w g\u00f3r\u0119/d\u00f3\u0142"), " = dane sko\u015bne"),
        tags$li(tags$b("Kszta\u0142t S"), " = z\u0142e ogony (za ci\u0119\u017ckie lub za lekkie)")
      )
    ),

    div(class = "widget-block",
      h4("Eksplorator wykresu Q-Q"),
      fluidRow(
        column(4,
          selectInput("ch7_qq_dist", "Rozk\u0142ad \u017ar\u00f3d\u0142owy:",
            choices = c(
              "Normalny"                = "normal",
              "Wyk\u0142adniczy (prawosko\u015bny)" = "exponential",
              "Ci\u0119\u017ckie ogony (t, df=3)"   = "heavy_tail",
              "Jednostajny (lekkie ogony)" = "uniform",
              "Log-normalny (prawosko\u015bny)" = "lognormal"
            ),
            selected = "normal"
          ),
          sliderInput("ch7_qq_n", "Wielko\u015b\u0107 pr\u00f3by:",
                      min = 30, max = 500, value = 100, step = 10),
          actionButton("ch7_qq_resample", "Losuj nowy zestaw",
                       class = "btn-primary", width = "100%"),
          hr(),
          uiOutput("ch7_qq_verdict")
        ),
        column(8,
          plotOutput("ch7_qq_plot", height = "350px")
        )
      )
    ),

    # --- Transition ---
    div(class = "chapter-transition",
      p("Mamy teraz pe\u0142en obraz rozk\u0142ad\u00f3w prawdopodobie\u0144stwa.
        Na koniec \u2014 kompaktowa \u015bci\u0105ga ze wszystkimi wzorami."),
      actionButton("ch7_next", "Dalej: 8. \u015aci\u0105ga \u2192",
                   class = "btn-primary btn-lg")
    ),

    br(), br()
  ))
)

# --------------------------------------------------------------------------
# Chapter 7 Server
# --------------------------------------------------------------------------

ch7_server <- function(input, output, session) {

  # --- Widget 1: Drzewo decyzyjne ---
  selected_dist <- reactive({
    if (input$ch7_tree_choice == "discrete") {
      input$ch7_disc_type
    } else {
      input$ch7_cont_type
    }
  })

  output$ch7_tree_info <- renderUI({
    dist <- selected_dist()

    info <- switch(dist,
      "d_uniform" = list(
        name = "Jednostajny dyskretny",
        desc = "Ka\u017cdy z k wynik\u00f3w ma P = 1/k",
        example = "Rzut kostk\u0105, losowanie cyfry",
        r_func = "sample(1:k, n, replace=TRUE)"
      ),
      "d_binomial" = list(
        name = "Dwumianowy B(n, p)",
        desc = "Liczba sukces\u00f3w w n niezale\u017cnych pr\u00f3bach",
        example = "Wadliwe produkty w partii, poprawne odpowiedzi",
        r_func = "rbinom(n, size, prob)"
      ),
      "d_poisson" = list(
        name = "Poissona Pois(\u03bb)",
        desc = "Liczba zdarze\u0144 w ustalonym czasie/przestrzeni",
        example = "Klienci na godzin\u0119, b\u0142\u0119dy na stronie",
        r_func = "rpois(n, lambda)"
      ),
      "c_normal" = list(
        name = "Normalny N(\u03bc, \u03c3)",
        desc = "Symetryczny, dzwonowaty, suma wielu ma\u0142ych efekt\u00f3w",
        example = "Wzrost, IQ, b\u0142\u0119dy pomiarowe",
        r_func = "rnorm(n, mean, sd)"
      ),
      "c_exponential" = list(
        name = "Wyk\u0142adniczy Exp(\u03bb)",
        desc = "Czas mi\u0119dzy zdarzeniami (bezpami\u0119ciowy)",
        example = "Czas do awarii, oczekiwanie na autobus",
        r_func = "rexp(n, rate)"
      ),
      "c_uniform" = list(
        name = "Jednostajny ci\u0105g\u0142y U(a, b)",
        desc = "Ka\u017cda warto\u015b\u0107 w [a,b] jednakowo prawdopodobna",
        example = "Generator liczb losowych, b\u0142\u0105d zaokr\u0105glenia",
        r_func = "runif(n, min, max)"
      ),
      "d_geometric" = list(
        name = "Geometryczny Geom(p)",
        desc = "Liczba pr\u00f3b do pierwszego sukcesu",
        example = "Ile rzut\u00f3w do sz\u00f3stki, pr\u00f3by egzaminu do zdania",
        r_func = "rgeom(n, prob) + 1"
      ),
      "c_t_student" = list(
        name = "t-Studenta t(df)",
        desc = "Jak normalny, ale z ci\u0119\u017cszymi ogonami; kluczowy we wnioskowaniu",
        example = "Test t, przedzia\u0142y ufno\u015bci przy ma\u0142ych pr\u00f3bach",
        r_func = "rt(n, df)"
      ),
      "c_chi_sq" = list(
        name = "Chi-kwadrat \u03c7\u00b2(df)",
        desc = "Suma kwadrat\u00f3w zmiennych N(0,1); nieujemny, prawosko\u015bny",
        example = "Test niezale\u017cno\u015bci, test dopasowania, estymacja wariancji",
        r_func = "rchisq(n, df)"
      ),
      "c_lognormal" = list(
        name = "Log-normalny LogN(\u03bc, \u03c3)",
        desc = "ln(X) ~ N(\u03bc, \u03c3); zawsze dodatni, prawosko\u015bny",
        example = "Dochody, ceny akcji, czasy reakcji",
        r_func = "rlnorm(n, meanlog, sdlog)"
      )
    )

    div(class = "dist-card",
      h4(info$name, style = "margin-top: 0;"),
      p(tags$strong("Opis: "), info$desc),
      p(tags$strong("Przyk\u0142ad: "), info$example),
      p(tags$strong("R: "), tags$code(info$r_func))
    )
  })

  output$ch7_tree_plot <- renderPlot({
    dist <- selected_dist()

    switch(dist,
      "d_uniform" = {
        df <- data.frame(x = 1:6, p = rep(1/6, 6))
        ggplot(df, aes(x = factor(x), y = p)) +
          geom_col(fill = col_uniform, color = "white", alpha = 0.85, width = 0.6) +
          labs(title = "Jednostajny dyskretny (kostka)", x = "k", y = "P(X=k)") +
          theme_prob(base_size = 12)
      },
      "d_binomial" = {
        x <- 0:20; p <- dbinom(x, 20, 0.3)
        df <- data.frame(x = x, p = p)
        ggplot(df, aes(x = x, y = p)) +
          geom_col(fill = col_binomial, color = "white", alpha = 0.85, width = 0.7) +
          labs(title = "B(20, 0.3)", x = "k", y = "P(X=k)") +
          theme_prob(base_size = 12)
      },
      "d_poisson" = {
        x <- 0:15; p <- dpois(x, 4)
        df <- data.frame(x = x, p = p)
        ggplot(df, aes(x = x, y = p)) +
          geom_col(fill = col_poisson, color = "white", alpha = 0.85, width = 0.7) +
          labs(title = "Pois(4)", x = "k", y = "P(X=k)") +
          theme_prob(base_size = 12)
      },
      "c_normal" = {
        x <- seq(-4, 4, length.out = 500)
        df <- data.frame(x = x, y = dnorm(x))
        ggplot(df, aes(x, y)) +
          geom_area(fill = col_normal, alpha = 0.3) +
          geom_line(color = col_normal, linewidth = 1.2) +
          labs(title = "N(0, 1)", x = "x", y = "f(x)") +
          theme_prob(base_size = 12)
      },
      "c_exponential" = {
        x <- seq(0, 8, length.out = 500)
        df <- data.frame(x = x, y = dexp(x, 1))
        ggplot(df, aes(x, y)) +
          geom_area(fill = col_exponential, alpha = 0.3) +
          geom_line(color = col_exponential, linewidth = 1.2) +
          labs(title = "Exp(1)", x = "x", y = "f(x)") +
          theme_prob(base_size = 12)
      },
      "c_uniform" = {
        x <- seq(-1, 11, length.out = 500)
        df <- data.frame(x = x, y = dunif(x, 0, 10))
        ggplot(df, aes(x, y)) +
          geom_area(fill = col_uniform, alpha = 0.3) +
          geom_line(color = col_uniform, linewidth = 1.2) +
          labs(title = "U(0, 10)", x = "x", y = "f(x)") +
          theme_prob(base_size = 12)
      },
      "d_geometric" = {
        x <- 1:20; p <- dgeom(x - 1, 0.2)
        df <- data.frame(x = x, p = p)
        ggplot(df, aes(x = x, y = p)) +
          geom_col(fill = col_geometric, color = "white", alpha = 0.85, width = 0.7) +
          labs(title = "Geom(0.2)", x = "k", y = "P(X=k)") +
          theme_prob(base_size = 12)
      },
      "c_t_student" = {
        x <- seq(-5, 5, length.out = 500)
        df <- data.frame(x = x, y = dt(x, df = 3))
        ggplot(df, aes(x, y)) +
          geom_area(fill = col_t_student, alpha = 0.3) +
          geom_line(color = col_t_student, linewidth = 1.2) +
          labs(title = "t(df=3)", x = "x", y = "f(x)") +
          theme_prob(base_size = 12)
      },
      "c_chi_sq" = {
        x <- seq(0.01, 20, length.out = 500)
        df <- data.frame(x = x, y = dchisq(x, df = 5))
        ggplot(df, aes(x, y)) +
          geom_area(fill = col_chi_sq, alpha = 0.3) +
          geom_line(color = col_chi_sq, linewidth = 1.2) +
          labs(title = "\u03c7\u00b2(df=5)", x = "x", y = "f(x)") +
          theme_prob(base_size = 12)
      },
      "c_lognormal" = {
        x <- seq(0.01, 10, length.out = 500)
        df <- data.frame(x = x, y = dlnorm(x, 0, 0.6))
        ggplot(df, aes(x, y)) +
          geom_area(fill = col_lognormal, alpha = 0.3) +
          geom_line(color = col_lognormal, linewidth = 1.2) +
          labs(title = "LogN(0, 0.6)", x = "x", y = "f(x)") +
          theme_prob(base_size = 12)
      }
    )
  })

  # --- Widget 2: Gra dopasowania ---
  game_data <- reactiveVal(NULL)

  generate_game_data <- function() {
    dists <- c("normal", "exponential", "uniform", "binomial", "poisson",
                "geometric", "lognormal")
    chosen <- sample(dists, 1)

    data <- switch(chosen,
      "normal"      = rnorm(500, mean = 50, sd = 10),
      "exponential" = rexp(500, rate = 0.2),
      "uniform"     = runif(500, min = 10, max = 60),
      "binomial"    = rbinom(500, size = 40, prob = 0.3),
      "poisson"     = rpois(500, lambda = 7),
      "geometric"   = rgeom(500, prob = 0.15) + 1,
      "lognormal"   = rlnorm(500, meanlog = 3, sdlog = 0.6)
    )

    game_data(list(data = data, true_dist = chosen, checked = FALSE))
    updateSelectInput(session, "ch7_game_guess", selected = "")
  }

  observeEvent(input$ch7_game_next, generate_game_data())

  # Inicjalizacja
  observe({
    if (is.null(game_data())) generate_game_data()
  })

  output$ch7_game_plot <- renderPlot({
    gd <- game_data()
    req(gd)

    df <- data.frame(x = gd$data)
    is_discrete <- gd$true_dist %in% c("binomial", "poisson", "geometric")

    p <- ggplot(df, aes(x = x))

    if (is_discrete) {
      p <- p + geom_bar(fill = col_primary, color = "white", alpha = 0.7)
    } else {
      p <- p + geom_histogram(bins = 30, fill = col_primary, color = "white", alpha = 0.7)
    }

    if (gd$checked && input$ch7_game_guess == gd$true_dist) {
      # Nakladamy krzywa teoretyczna
      if (gd$true_dist == "normal") {
        x_seq <- seq(min(gd$data), max(gd$data), length.out = 200)
        theo_df <- data.frame(x = x_seq,
                              y = dnorm(x_seq, mean(gd$data), sd(gd$data)) * length(gd$data) *
                                diff(range(gd$data)) / 30)
        p <- p + geom_line(data = theo_df, aes(x, y), color = col_secondary, linewidth = 1.5)
      }
    }

    p + labs(title = "Dane \u2014 jaki to rozk\u0142ad?",
             x = "Warto\u015b\u0107", y = "Liczebno\u015b\u0107") +
      theme_prob()
  })

  observeEvent(input$ch7_game_check, {
    gd <- game_data()
    req(gd, input$ch7_game_guess != "")
    gd$checked <- TRUE
    game_data(gd)
  })

  output$ch7_game_feedback <- renderUI({
    gd <- game_data()
    req(gd, gd$checked)
    guess <- input$ch7_game_guess

    dist_names <- c(
      "normal" = "Normalny", "exponential" = "Wyk\u0142adniczy",
      "uniform" = "Jednostajny", "binomial" = "Dwumianowy",
      "poisson" = "Poissona", "geometric" = "Geometryczny",
      "lognormal" = "Log-normalny"
    )

    if (guess == gd$true_dist) {
      div(class = "callout-success",
        tags$strong("Brawo!"),
        paste0(" To rzeczywi\u015bcie rozk\u0142ad ", dist_names[gd$true_dist], ".")
      )
    } else {
      div(class = "callout-danger",
        tags$strong("Nie tym razem."),
        paste0(" To by\u0142 rozk\u0142ad ", dist_names[gd$true_dist],
               ", a Twoja odpowied\u017a: ", dist_names[guess], ".")
      )
    }
  })

  # --- Widget 3: Wykres Q-Q ---
  qq_data <- reactiveVal(NULL)

  generate_qq_data <- function() {
    n <- input$ch7_qq_n
    dist <- input$ch7_qq_dist

    data <- switch(dist,
      "normal"      = rnorm(n),
      "exponential" = rexp(n, 1),
      "heavy_tail"  = rt(n, df = 3),
      "uniform"     = runif(n, -2, 2),
      "lognormal"   = rlnorm(n, 0, 0.6)
    )
    qq_data(data)
  }

  observe({
    input$ch7_qq_dist
    input$ch7_qq_n
    generate_qq_data()
  })

  observeEvent(input$ch7_qq_resample, generate_qq_data())

  output$ch7_qq_plot <- renderPlot({
    data <- qq_data()
    req(data)

    df <- data.frame(x = data)

    ggplot(df, aes(sample = x)) +
      stat_qq(color = col_primary, size = 2, alpha = 0.6) +
      stat_qq_line(color = col_secondary, linewidth = 1) +
      labs(title = "Wykres Q-Q (vs rozk\u0142ad normalny)",
           x = "Kwantyle teoretyczne (normalny)",
           y = "Kwantyle z danych") +
      theme_prob()
  })

  output$ch7_qq_verdict <- renderUI({
    dist <- input$ch7_qq_dist

    verdict <- switch(dist,
      "normal"      = list(class = "callout-success",
                           text = "Punkty powinny le\u017ce\u0107 blisko linii \u2014 dane s\u0105 normalne."),
      "exponential" = list(class = "callout-warning",
                           text = "Krzywa w g\u00f3r\u0119 \u2014 dane s\u0105 prawosko\u015bne, ogon prawy za ci\u0119\u017cki."),
      "heavy_tail"  = list(class = "callout-danger",
                           text = "Kszta\u0142t S \u2014 oba ogony s\u0105 za ci\u0119\u017ckie (wi\u0119cej ekstrem\u00f3w ni\u017c w normalnym)."),
      "uniform"     = list(class = "callout-info",
                           text = "Kszta\u0142t odwr\u00f3conego S \u2014 ogony za lekkie (za ma\u0142o ekstrem\u00f3w)."),
      "lognormal"   = list(class = "callout-warning",
                           text = "Krzywa w g\u00f3r\u0119 \u2014 dane s\u0105 prawosko\u015bne. Podobnie jak wyk\u0142adniczy, ale z innym mechanizmem (multiplikatywny).")
    )

    div(class = verdict$class, verdict$text)
  })

}
