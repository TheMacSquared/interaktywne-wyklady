# ============================================================================
# CHAPTER 7: Sciaga
# ============================================================================

ch7_ui <- tabPanel("7. \u015aci\u0105ga",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Podsumowanie \u2014 rozk\u0142ady prawdopodobie\u0144stwa"),

    div(class = "narrative",
      p("Kompletna \u015bci\u0105ga ze wszystkimi rozk\u0142adami, wzorami i regu\u0142ami
        decyzyjnymi om\u00f3wionymi w trakcie wyk\u0142adu.")
    ),

    # --- Tabela 1: Rozklady dyskretne ---
    div(class = "section-title", "Rozk\u0142ady dyskretne"),

    div(class = "widget-block",
      tableOutput("ch7_discrete_table")
    ),

    # --- Tabela 2: Rozklady ciagle ---
    div(class = "section-title", "Rozk\u0142ady ci\u0105g\u0142e"),

    div(class = "widget-block",
      tableOutput("ch7_continuous_table")
    ),

    # --- Tabela 3: Kluczowe wzory ---
    div(class = "section-title", "Kluczowe wzory"),

    div(class = "widget-block",
      withMathJax(
        h4("Aksjomaty prawdopodobie\u0144stwa"),
        tags$ol(
          tags$li("\\(P(A) \\geq 0\\) dla ka\u017cdego zdarzenia A"),
          tags$li("\\(P(\\Omega) = 1\\) (pewno\u015b\u0107)"),
          tags$li("\\(P(A \\cup B) = P(A) + P(B)\\) dla zdarze\u0144 roz\u0142\u0105cznych")
        ),

        hr(),
        h4("Warto\u015b\u0107 oczekiwana i wariancja"),
        helpText("Dyskretne: $$E(X) = \\sum_x x \\cdot P(X=x), \\quad Var(X) = E[(X - E(X))^2]$$"),
        helpText("Ci\u0105g\u0142e: $$E(X) = \\int_{-\\infty}^{\\infty} x \\cdot f(x) \\, dx$$"),

        hr(),
        h4("Standaryzacja"),
        helpText("$$z = \\frac{x - \\mu}{\\sigma}, \\quad \\text{gdzie } z \\sim N(0, 1) \\text{ je\u015bli } x \\sim N(\\mu, \\sigma)$$"),

        hr(),
        h4("Centralne Twierdzenie Graniczne"),
        helpText("$$\\bar{X}_n \\xrightarrow{d} N\\left(\\mu, \\frac{\\sigma}{\\sqrt{n}}\\right) \\quad \\text{dla } n \\to \\infty$$"),
        p("Niezale\u017cnie od rozk\u0142adu populacji (o ile ma sko\u0144czon\u0105 wariancj\u0119).")
      )
    ),

    # --- Interaktywne drzewo decyzyjne ---
    div(class = "section-title", "Drzewo decyzyjne \u2014 kt\u00f3ry rozk\u0142ad?"),

    div(class = "narrative",
      p("Ka\u017cdy rozk\u0142ad ma sw\u00f3j 'naturalny habitat'. Kluczowe pytania przy doborze:"),
      tags$ol(
        tags$li("Czy zmienna jest ", tags$b("dyskretna"), " czy ", tags$b("ci\u0105g\u0142a"), "?"),
        tags$li("Jaki jest ", tags$b("kszta\u0142t"), " danych? (symetryczny, sko\u015bny, p\u0142aski)"),
        tags$li("Jaki ", tags$b("mechanizm generuje"), " dane? (zliczanie, pomiar, oczekiwanie)")
      )
    ),

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

    # --- Tabela 5: Funkcje R ---
    div(class = "section-title", "Funkcje R \u2014 rodzina d/p/q/r"),

    div(class = "widget-block",
      div(class = "callout-info",
        tags$strong("Konwencja nazw w R:"),
        tags$ul(
          tags$li(tags$b("d"), " \u2014 density/PMF: prawdopodobie\u0144stwo lub g\u0119sto\u015b\u0107 w punkcie"),
          tags$li(tags$b("p"), " \u2014 CDF: prawdopodobie\u0144stwo skumulowane P(X \u2264 x)"),
          tags$li(tags$b("q"), " \u2014 kwantyl: odwrotno\u015b\u0107 CDF (jaki x daje P = p?)"),
          tags$li(tags$b("r"), " \u2014 random: losowanie z rozk\u0142adu")
        )
      ),
      tableOutput("ch7_r_functions_table")
    ),

    # --- Przybli\u017cenia ---
    div(class = "section-title", "Przybli\u017cenia mi\u0119dzy rozk\u0142adami"),

    div(class = "widget-block",
      tableOutput("ch7_approx_table"),
      div(class = "callout-warning",
        tags$strong("Praktyczna regu\u0142a:"),
        " Przybli\u017cenie normalnym stosuj, gdy np \u2265 5 i n(1-p) \u2265 5
          (dla dwumianowego) lub \u03bb \u2265 20 (dla Poissona)."
      )
    ),

    # --- Transition ---
    div(class = "chapter-transition",
      p("Masz ju\u017c pe\u0142en przegl\u0105d rozk\u0142ad\u00f3w. Czas sprawdzi\u0107,
        czy potrafisz rozpozna\u0107 je w praktyce!"),
      actionButton("ch7_next", "Dalej: 8. Quiz \u2192",
                   class = "btn-primary btn-lg")
    ),

    br(), br()
  ))
)

# --------------------------------------------------------------------------
# Chapter 7 Server
# --------------------------------------------------------------------------

ch7_server <- function(input, output, session) {

  # --- Interaktywne drzewo decyzyjne ---
  ch7_selected_dist <- reactive({
    if (input$ch7_tree_choice == "discrete") {
      input$ch7_disc_type
    } else {
      input$ch7_cont_type
    }
  })

  output$ch7_tree_info <- renderUI({
    dist <- ch7_selected_dist()

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
        example = "Czas do awarii, czas mi\u0119dzy wiadomo\u015bciami",
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
    dist <- ch7_selected_dist()

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

  # --- Tabele statyczne ---
  output$ch7_discrete_table <- renderTable({
    data.frame(
      a = c("Jednostajny dyskretny", "Dwumianowy B(n, p)", "Poissona Pois(\u03bb)", "Geometryczny Geom(p)"),
      b = c("k (liczba wynik\u00f3w)", "n (pr\u00f3by), p (prawdop.)", "\u03bb (\u015brednia zdarze\u0144)", "p (prawdop. sukcesu)"),
      c = c("P(X=x) = 1/k", "P(X=k) = C(n,k) p^k (1-p)^(n-k)", "P(X=k) = \u03bb^k e^(-\u03bb) / k!", "P(X=k) = (1-p)^(k-1) \u00b7 p"),
      d = c("(k+1)/2", "np", "\u03bb", "1/p"),
      e = c("(k\u00b2-1)/12", "np(1-p)", "\u03bb", "(1-p)/p\u00b2"),
      f = c("Kostka, losy", "Wadliwe produkty, testy", "Klienci/h, b\u0142\u0119dy/strona", "Ile pr\u00f3b do sukcesu"),
      stringsAsFactors = FALSE
    ) %>% setNames(c("Rozk\u0142ad", "Parametry", "PMF", "E(X)", "Var(X)", "Przyk\u0142ad"))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch7_continuous_table <- renderTable({
    data.frame(
      a = c("Jednostajny U(a, b)", "Wyk\u0142adniczy Exp(\u03bb)", "Normalny N(\u03bc, \u03c3)",
            "t-Studenta t(df)", "Chi-kwadrat \u03c7\u00b2(df)", "Log-normalny LogN(\u03bc, \u03c3)"),
      b = c("a, b (granice)", "\u03bb (rate)", "\u03bc (\u015brednia), \u03c3 (odch. std.)",
            "df (stopnie swobody)", "df (stopnie swobody)", "\u03bc (meanlog), \u03c3 (sdlog)"),
      c = c("f(x) = 1/(b-a)", "f(x) = \u03bb e^(-\u03bbx)", "f(x) = krzywa Gaussa",
            "krzywa t (ci\u0119\u017csze ogony)", "prawosko\u015bna, nieujemna", "prawosko\u015bna, dodatnia"),
      d = c("(a+b)/2", "1/\u03bb", "\u03bc", "0 (df>1)", "df", "exp(\u03bc+\u03c3\u00b2/2)"),
      e = c("(b-a)\u00b2/12", "1/\u03bb\u00b2", "\u03c3\u00b2", "df/(df-2)", "2\u00b7df", "(e^\u03c3\u00b2-1)\u00b7e^(2\u03bc+\u03c3\u00b2)"),
      f = c("Generator losowy", "Czas oczekiwania", "Wzrost, IQ, pomiary",
            "Test t, przedzia\u0142y ufno\u015bci", "Testy \u03c7\u00b2, wariancja", "Dochody, ceny, czasy reakcji"),
      stringsAsFactors = FALSE
    ) %>% setNames(c("Rozk\u0142ad", "Parametry", "PDF", "E(X)", "Var(X)", "Przyk\u0142ad"))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch7_r_functions_table <- renderTable({
    data.frame(
      a = c("Normalny", "Dwumianowy", "Poissona", "Geometryczny",
            "Wyk\u0142adniczy", "Jednostajny", "t-Studenta", "Chi-kwadrat", "Log-normalny"),
      b = c("dnorm(x, mean, sd)", "dbinom(x, size, prob)", "dpois(x, lambda)", "dgeom(x, prob)",
            "dexp(x, rate)", "dunif(x, min, max)", "dt(x, df)", "dchisq(x, df)", "dlnorm(x, meanlog, sdlog)"),
      c = c("pnorm(q, mean, sd)", "pbinom(q, size, prob)", "ppois(q, lambda)", "pgeom(q, prob)",
            "pexp(q, rate)", "punif(q, min, max)", "pt(q, df)", "pchisq(q, df)", "plnorm(q, meanlog, sdlog)"),
      d = c("qnorm(p, mean, sd)", "qbinom(p, size, prob)", "qpois(p, lambda)", "qgeom(p, prob)",
            "qexp(p, rate)", "qunif(p, min, max)", "qt(p, df)", "qchisq(p, df)", "qlnorm(p, meanlog, sdlog)"),
      e = c("rnorm(n, mean, sd)", "rbinom(n, size, prob)", "rpois(n, lambda)", "rgeom(n, prob)",
            "rexp(n, rate)", "runif(n, min, max)", "rt(n, df)", "rchisq(n, df)", "rlnorm(n, meanlog, sdlog)"),
      stringsAsFactors = FALSE
    ) %>% setNames(c("Rozk\u0142ad", "d (g\u0119sto\u015b\u0107/PMF)", "p (CDF)", "q (kwantyl)", "r (losowanie)"))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch7_approx_table <- renderTable({
    data.frame(
      a = c("Dwumianowy \u2192 Poissona", "Dwumianowy \u2192 Normalny", "Poissona \u2192 Normalny",
            "t-Studenta \u2192 Normalny", "\u03c7\u00b2 \u2192 Normalny"),
      b = c("n du\u017ce, p ma\u0142e (\u03bb = np)", "np \u2265 5 i n(1-p) \u2265 5", "\u03bb \u2265 20",
            "df \u2265 30", "df \u2265 30"),
      c = c("B(1000, 0.002) \u2248 Pois(2)", "B(100, 0.3) \u2248 N(30, 4.58)", "Pois(25) \u2248 N(25, 5)",
            "t(50) \u2248 N(0, 1)", "\u03c7\u00b2(40) \u2248 N(40, 8.94)"),
      stringsAsFactors = FALSE
    ) %>% setNames(c("Przybli\u017cenie", "Warunek", "Przyk\u0142ad"))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

}
