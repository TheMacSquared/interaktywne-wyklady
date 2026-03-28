# ============================================================================
# CHAPTER 8: Sciaga
# ============================================================================

ch8_ui <- tabPanel("8. \u015aci\u0105ga",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Podsumowanie \u2014 rozk\u0142ady prawdopodobie\u0144stwa"),

    div(class = "narrative",
      p("Kompletna \u015bci\u0105ga ze wszystkimi rozk\u0142adami, wzorami i regu\u0142ami
        decyzyjnymi om\u00f3wionymi w trakcie wyk\u0142adu.")
    ),

    # --- Tabela 1: Rozklady dyskretne ---
    div(class = "section-title", "Rozk\u0142ady dyskretne"),

    div(class = "widget-block",
      tableOutput("ch8_discrete_table")
    ),

    # --- Tabela 2: Rozklady ciagle ---
    div(class = "section-title", "Rozk\u0142ady ci\u0105g\u0142e"),

    div(class = "widget-block",
      tableOutput("ch8_continuous_table")
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

    # --- Tabela 4: Drzewo decyzyjne ---
    div(class = "section-title", "Drzewo decyzyjne \u2014 kt\u00f3ry rozk\u0142ad?"),

    div(class = "widget-block",
      tableOutput("ch8_decision_table")
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
      tableOutput("ch8_r_functions_table")
    ),

    # --- Przybli\u017cenia ---
    div(class = "section-title", "Przybli\u017cenia mi\u0119dzy rozk\u0142adami"),

    div(class = "widget-block",
      tableOutput("ch8_approx_table"),
      div(class = "callout-warning",
        tags$strong("Praktyczna regu\u0142a:"),
        " Przybli\u017cenie normalnym stosuj, gdy np \u2265 5 i n(1-p) \u2265 5
          (dla dwumianowego) lub \u03bb \u2265 20 (dla Poissona)."
      )
    ),

    br(), br()
  ))
)

# --------------------------------------------------------------------------
# Chapter 8 Server
# --------------------------------------------------------------------------

ch8_server <- function(input, output, session) {

  output$ch8_discrete_table <- renderTable({
    data.frame(
      a = c("Jednostajny dyskretny", "Dwumianowy B(n, p)", "Poissona Pois(\u03bb)"),
      b = c("k (liczba wynik\u00f3w)", "n (pr\u00f3by), p (prawdop.)", "\u03bb (\u015brednia zdarze\u0144)"),
      c = c("P(X=x) = 1/k", "P(X=k) = C(n,k) p^k (1-p)^(n-k)", "P(X=k) = \u03bb^k e^(-\u03bb) / k!"),
      d = c("(k+1)/2", "np", "\u03bb"),
      e = c("(k\u00b2-1)/12", "np(1-p)", "\u03bb"),
      f = c("Kostka, losy", "Wadliwe produkty, testy", "Klienci/h, b\u0142\u0119dy/strona"),
      stringsAsFactors = FALSE
    ) %>% setNames(c("Rozk\u0142ad", "Parametry", "PMF", "E(X)", "Var(X)", "Przyk\u0142ad"))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch8_continuous_table <- renderTable({
    data.frame(
      a = c("Jednostajny U(a, b)", "Wyk\u0142adniczy Exp(\u03bb)", "Normalny N(\u03bc, \u03c3)"),
      b = c("a, b (granice)", "\u03bb (rate)", "\u03bc (\u015brednia), \u03c3 (odch. std.)"),
      c = c("f(x) = 1/(b-a)", "f(x) = \u03bb e^(-\u03bbx)", "f(x) = krzywa Gaussa"),
      d = c("(a+b)/2", "1/\u03bb", "\u03bc"),
      e = c("(b-a)\u00b2/12", "1/\u03bb\u00b2", "\u03c3\u00b2"),
      f = c("Generator losowy", "Czas oczekiwania", "Wzrost, IQ, pomiary"),
      stringsAsFactors = FALSE
    ) %>% setNames(c("Rozk\u0142ad", "Parametry", "PDF", "E(X)", "Var(X)", "Przyk\u0142ad"))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch8_decision_table <- renderTable({
    data.frame(
      a = c("Dyskretna, r\u00f3wne P", "Dyskretna, n pr\u00f3b tak/nie",
            "Dyskretna, zdarzenia w czasie", "Ci\u0105g\u0142a, symetryczny dzwon",
            "Ci\u0105g\u0142a, czas oczekiwania", "Ci\u0105g\u0142a, r\u00f3wne P w przedziale"),
      b = c("Jednostajny dyskretny", "Dwumianowy B(n, p)",
            "Poissona Pois(\u03bb)", "Normalny N(\u03bc, \u03c3)",
            "Wyk\u0142adniczy Exp(\u03bb)", "Jednostajny U(a, b)"),
      c = c("rzut kostk\u0105, losowanie", "kontrola jako\u015bci, ankieta tak/nie",
            "klienci/h, wypadki/miesi\u0105c", "wzrost, wyniki test\u00f3w",
            "czas do awarii, mi\u0119dzy zg\u0142oszeniami", "generator liczb, b\u0142\u0105d zaokr."),
      stringsAsFactors = FALSE
    ) %>% setNames(c("Sytuacja", "Rozk\u0142ad", "Przyk\u0142ad"))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch8_r_functions_table <- renderTable({
    data.frame(
      a = c("Normalny", "Dwumianowy", "Poissona",
            "Wyk\u0142adniczy", "Jednostajny"),
      b = c("dnorm(x, mean, sd)", "dbinom(x, size, prob)", "dpois(x, lambda)",
            "dexp(x, rate)", "dunif(x, min, max)"),
      c = c("pnorm(q, mean, sd)", "pbinom(q, size, prob)", "ppois(q, lambda)",
            "pexp(q, rate)", "punif(q, min, max)"),
      d = c("qnorm(p, mean, sd)", "qbinom(p, size, prob)", "qpois(p, lambda)",
            "qexp(p, rate)", "qunif(p, min, max)"),
      e = c("rnorm(n, mean, sd)", "rbinom(n, size, prob)", "rpois(n, lambda)",
            "rexp(n, rate)", "runif(n, min, max)"),
      stringsAsFactors = FALSE
    ) %>% setNames(c("Rozk\u0142ad", "d (g\u0119sto\u015b\u0107/PMF)", "p (CDF)", "q (kwantyl)", "r (losowanie)"))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch8_approx_table <- renderTable({
    data.frame(
      a = c("Dwumianowy \u2192 Poissona", "Dwumianowy \u2192 Normalny", "Poissona \u2192 Normalny"),
      b = c("n du\u017ce, p ma\u0142e (\u03bb = np)", "np \u2265 5 i n(1-p) \u2265 5", "\u03bb \u2265 20"),
      c = c("B(1000, 0.002) \u2248 Pois(2)", "B(100, 0.3) \u2248 N(30, 4.58)", "Pois(25) \u2248 N(25, 5)"),
      stringsAsFactors = FALSE
    ) %>% setNames(c("Przybli\u017cenie", "Warunek", "Przyk\u0142ad"))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

}
