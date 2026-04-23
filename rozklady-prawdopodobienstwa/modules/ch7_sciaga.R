# ============================================================================
# CHAPTER 7: Sciaga
# ============================================================================

ch7_ui <- list(
  id = "ch-sciaga", num = "07", title = "Ściąga",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 07 · Rozkłady prawdopodobieństwa",
      num    = "07",
      title  = "Ściąga.",
      lead   = "Kompletna ściąga ze wszystkimi rozkładami, wzorami
                i regułami decyzyjnymi omówionymi w trakcie wykładu."
    ),

    # --- Tabela 1: Rozklady dyskretne ---
    h2(id = "ch7-dyskretne", class = "section-title", "Rozkłady dyskretne"),

    figure_panel(
      label = "Tab. 7.1",
      title = "Rozkłady dyskretne — zestawienie",
      full_width = TRUE,
      tableOutput("ch7_discrete_table")
    ),

    # --- Tabela 2: Rozklady ciagle ---
    h2(id = "ch7-ciagle", class = "section-title", "Rozkłady ciągłe"),

    figure_panel(
      label = "Tab. 7.2",
      title = "Rozkłady ciągłe — zestawienie",
      full_width = TRUE,
      tableOutput("ch7_continuous_table")
    ),

    # --- Tabela 3: Kluczowe wzory ---
    h2(id = "ch7-wzory", class = "section-title", "Kluczowe wzory"),

    figure_panel(
      label = "Wzory",
      title = "Kluczowe wzory",
      full_width = TRUE,
      withMathJax(
        h4("Aksjomaty prawdopodobieństwa"),
        tags$ol(
          tags$li("\\(P(A) \\geq 0\\) dla każdego zdarzenia A"),
          tags$li("\\(P(\\Omega) = 1\\) (pewność)"),
          tags$li("\\(P(A \\cup B) = P(A) + P(B)\\) dla zdarzeń rozłącznych")
        ),

        hr(),
        h4("Wartość oczekiwana i wariancja"),
        helpText("Dyskretne: $$E(X) = \\sum_x x \\cdot P(X=x), \\quad Var(X) = E[(X - E(X))^2]$$"),
        helpText("Ciągłe: $$E(X) = \\int_{-\\infty}^{\\infty} x \\cdot f(x) \\, dx$$"),

        hr(),
        h4("Standaryzacja"),
        helpText("$$z = \\frac{x - \\mu}{\\sigma}, \\quad \\text{gdzie } z \\sim N(0, 1) \\text{ jeśli } x \\sim N(\\mu, \\sigma)$$"),

        hr(),
        h4("Centralne Twierdzenie Graniczne"),
        helpText("$$\\bar{X}_n \\xrightarrow{d} N\\left(\\mu, \\frac{\\sigma}{\\sqrt{n}}\\right) \\quad \\text{dla } n \\to \\infty$$"),
        p("Niezależnie od rozkładu populacji (o ile ma skończoną wariancję).")
      )
    ),

    # --- Interaktywne drzewo decyzyjne ---
    h2(id = "ch7-drzewo", class = "section-title", "Drzewo decyzyjne — który rozkład?"),

    div(class = "narrative",
      p("Każdy rozkład ma swój 'naturalny habitat'. Kluczowe pytania przy doborze:"),
      tags$ol(
        tags$li("Czy zmienna jest ", tags$b("dyskretna"), " czy ", tags$b("ciągła"), "?"),
        tags$li("Jaki jest ", tags$b("kształt"), " danych? (symetryczny, skośny, płaski)"),
        tags$li("Jaki ", tags$b("mechanizm generuje"), " dane? (zliczanie, pomiar, oczekiwanie)")
      )
    ),

    figure_panel(
      label = "Ryc. 7.1",
      title = "Który rozkład wybrać?",
      full_width = TRUE,
      fluidRow(
        column(4,
          radioButtons("ch7_tree_choice", "Wybierz typ zmiennej:",
            choices = c(
              "Zmienna dyskretna" = "discrete",
              "Zmienna ciągła"   = "continuous"
            ),
            selected = "discrete"
          ),
          conditionalPanel(
            condition = "input.ch7_tree_choice == 'discrete'",
            radioButtons("ch7_disc_type", "Jaki mechanizm?",
              choices = c(
                "Każdy wynik jednakowo prawdop." = "d_uniform",
                "Stała liczba prób, sukces/porażka" = "d_binomial",
                "Zliczanie zdarzeń w czasie/przestrzeni" = "d_poisson",
                "Ile prób do pierwszego sukcesu" = "d_geometric"
              )
            )
          ),
          conditionalPanel(
            condition = "input.ch7_tree_choice == 'continuous'",
            radioButtons("ch7_cont_type", "Jaki kształt/mechanizm?",
              choices = c(
                "Symetryczny dzwon" = "c_normal",
                "Czas oczekiwania (prawoskośny)" = "c_exponential",
                "Każda wartość w przedziale jednakowo" = "c_uniform",
                "Ciężkie ogony (wnioskowanie)" = "c_t_student",
                "Suma kwadratów (testy χ²)" = "c_chi_sq",
                "Dane prawoskośne, dodatnie" = "c_lognormal"
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

    lc_chapter_next(
      num       = "08",
      title     = "Quiz",
      lead      = "sprawdź, czy potrafisz rozpoznać rozkłady w praktyce.",
      target_id = "ch-quiz"
    )
  )
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
        desc = "Każdy z k wyników ma P = 1/k",
        example = "Rzut kostką, losowanie cyfry",
        r_func = "sample(1:k, n, replace=TRUE)"
      ),
      "d_binomial" = list(
        name = "Dwumianowy B(n, p)",
        desc = "Liczba sukcesów w n niezależnych próbach",
        example = "Wadliwe produkty w partii, poprawne odpowiedzi",
        r_func = "rbinom(n, size, prob)"
      ),
      "d_poisson" = list(
        name = "Poissona Pois(λ)",
        desc = "Liczba zdarzeń w ustalonym czasie/przestrzeni",
        example = "Klienci na godzinę, błędy na stronie",
        r_func = "rpois(n, lambda)"
      ),
      "c_normal" = list(
        name = "Normalny N(μ, σ)",
        desc = "Symetryczny, dzwonowaty, suma wielu małych efektów",
        example = "Wzrost, IQ, błędy pomiarowe",
        r_func = "rnorm(n, mean, sd)"
      ),
      "c_exponential" = list(
        name = "Wykładniczy Exp(λ)",
        desc = "Czas między zdarzeniami (bezpamięciowy)",
        example = "Czas do awarii, czas między wiadomościami",
        r_func = "rexp(n, rate)"
      ),
      "c_uniform" = list(
        name = "Jednostajny ciągły U(a, b)",
        desc = "Każda wartość w [a,b] jednakowo prawdopodobna",
        example = "Generator liczb losowych, błąd zaokrąglenia",
        r_func = "runif(n, min, max)"
      ),
      "d_geometric" = list(
        name = "Geometryczny Geom(p)",
        desc = "Liczba prób do pierwszego sukcesu",
        example = "Ile rzutów do szóstki, próby egzaminu do zdania",
        r_func = "rgeom(n, prob) + 1"
      ),
      "c_t_student" = list(
        name = "t-Studenta t(df)",
        desc = "Jak normalny, ale z cięższymi ogonami; kluczowy we wnioskowaniu",
        example = "Test t, przedziały ufności przy małych próbach",
        r_func = "rt(n, df)"
      ),
      "c_chi_sq" = list(
        name = "Chi-kwadrat χ²(df)",
        desc = "Suma kwadratów zmiennych N(0,1); nieujemny, prawoskośny",
        example = "Test niezależności, test dopasowania, estymacja wariancji",
        r_func = "rchisq(n, df)"
      ),
      "c_lognormal" = list(
        name = "Log-normalny LogN(μ, σ)",
        desc = "ln(X) ~ N(μ, σ); zawsze dodatni, prawoskośny",
        example = "Dochody, ceny akcji, czasy reakcji",
        r_func = "rlnorm(n, meanlog, sdlog)"
      )
    )

    div(class = "dist-card",
      h4(info$name, style = "margin-top: 0;"),
      p(tags$strong("Opis: "), info$desc),
      p(tags$strong("Przykład: "), info$example),
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
          theme_educational(base_size = 12)
      },
      "d_binomial" = {
        x <- 0:20; p <- dbinom(x, 20, 0.3)
        df <- data.frame(x = x, p = p)
        ggplot(df, aes(x = x, y = p)) +
          geom_col(fill = col_binomial, color = "white", alpha = 0.85, width = 0.7) +
          labs(title = "B(20, 0.3)", x = "k", y = "P(X=k)") +
          theme_educational(base_size = 12)
      },
      "d_poisson" = {
        x <- 0:15; p <- dpois(x, 4)
        df <- data.frame(x = x, p = p)
        ggplot(df, aes(x = x, y = p)) +
          geom_col(fill = col_poisson, color = "white", alpha = 0.85, width = 0.7) +
          labs(title = "Pois(4)", x = "k", y = "P(X=k)") +
          theme_educational(base_size = 12)
      },
      "c_normal" = {
        x <- seq(-4, 4, length.out = 500)
        df <- data.frame(x = x, y = dnorm(x))
        ggplot(df, aes(x, y)) +
          geom_area(fill = col_normal, alpha = 0.3) +
          geom_line(color = col_normal, linewidth = 1.2) +
          labs(title = "N(0, 1)", x = "x", y = "f(x)") +
          theme_educational(base_size = 12)
      },
      "c_exponential" = {
        x <- seq(0, 8, length.out = 500)
        df <- data.frame(x = x, y = dexp(x, 1))
        ggplot(df, aes(x, y)) +
          geom_area(fill = col_exponential, alpha = 0.3) +
          geom_line(color = col_exponential, linewidth = 1.2) +
          labs(title = "Exp(1)", x = "x", y = "f(x)") +
          theme_educational(base_size = 12)
      },
      "c_uniform" = {
        x <- seq(-1, 11, length.out = 500)
        df <- data.frame(x = x, y = dunif(x, 0, 10))
        ggplot(df, aes(x, y)) +
          geom_area(fill = col_uniform, alpha = 0.3) +
          geom_line(color = col_uniform, linewidth = 1.2) +
          labs(title = "U(0, 10)", x = "x", y = "f(x)") +
          theme_educational(base_size = 12)
      },
      "d_geometric" = {
        x <- 1:20; p <- dgeom(x - 1, 0.2)
        df <- data.frame(x = x, p = p)
        ggplot(df, aes(x = x, y = p)) +
          geom_col(fill = col_geometric, color = "white", alpha = 0.85, width = 0.7) +
          labs(title = "Geom(0.2)", x = "k", y = "P(X=k)") +
          theme_educational(base_size = 12)
      },
      "c_t_student" = {
        x <- seq(-5, 5, length.out = 500)
        df <- data.frame(x = x, y = dt(x, df = 3))
        ggplot(df, aes(x, y)) +
          geom_area(fill = col_t_student, alpha = 0.3) +
          geom_line(color = col_t_student, linewidth = 1.2) +
          labs(title = "t(df=3)", x = "x", y = "f(x)") +
          theme_educational(base_size = 12)
      },
      "c_chi_sq" = {
        x <- seq(0.01, 20, length.out = 500)
        df <- data.frame(x = x, y = dchisq(x, df = 5))
        ggplot(df, aes(x, y)) +
          geom_area(fill = col_chi_sq, alpha = 0.3) +
          geom_line(color = col_chi_sq, linewidth = 1.2) +
          labs(title = "χ²(df=5)", x = "x", y = "f(x)") +
          theme_educational(base_size = 12)
      },
      "c_lognormal" = {
        x <- seq(0.01, 10, length.out = 500)
        df <- data.frame(x = x, y = dlnorm(x, 0, 0.6))
        ggplot(df, aes(x, y)) +
          geom_area(fill = col_lognormal, alpha = 0.3) +
          geom_line(color = col_lognormal, linewidth = 1.2) +
          labs(title = "LogN(0, 0.6)", x = "x", y = "f(x)") +
          theme_educational(base_size = 12)
      }
    )
  })

  # --- Tabele statyczne ---
  output$ch7_discrete_table <- renderTable({
    data.frame(
      a = c("Jednostajny dyskretny", "Dwumianowy B(n, p)", "Poissona Pois(λ)", "Geometryczny Geom(p)"),
      b = c("k (liczba wyników)", "n (próby), p (prawdop.)", "λ (średnia zdarzeń)", "p (prawdop. sukcesu)"),
      c = c(
        "Rzut kostką (każda ściana = 1/6), losowanie numeru w loterii, losowy przydział do grup eksperymentalnych",
        "Liczba wadliwych produktów w partii 100 sztuk, ile osób z 50 odpowie „tak” w ankiecie, skuteczność leku u n pacjentów",
        "Liczba klientów wchodzących do sklepu na godzinę, zgłoszenia na helpdesk dziennie, literówki na stronie tekstu",
        "Ile razy rzucać monetą, aż wypadnie orzeł; ile CV wysłać, zanim dostaniesz zaproszenie na rozmowę"
      ),
      stringsAsFactors = FALSE
    ) %>% setNames(c("Rozkład", "Parametry", "Przykłady zastosowań"))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch7_continuous_table <- renderTable({
    data.frame(
      a = c("Jednostajny U(a, b)", "Wykładniczy Exp(λ)", "Normalny N(μ, σ)",
            "t-Studenta t(df)", "Chi-kwadrat χ²(df)", "Log-normalny LogN(μ, σ)"),
      b = c("a, b (granice)", "λ (rate)", "μ (średnia), σ (odch. std.)",
            "df (stopnie swobody)", "df (stopnie swobody)", "μ (meanlog), σ (sdlog)"),
      c = c(
        "Generator liczb pseudolosowych, czas przyjazdu autobusu w obrębie rozkładu, błąd zaokrąglenia",
        "Czas do następnej awarii maszyny, odstęp między wiadomościami na czacie, czas oczekiwania na obsługę w kolejce",
        "Wzrost dorosłych w populacji, wyniki testu IQ, błędy pomiarowe w laboratorium, ciśnienie krwi",
        "Wnioskowanie o średniej przy małych próbach (n < 30), przedziały ufności, porównanie średnich dwóch grup",
        "Test niezależności cech w tabeli krzyżowej, test zgodności rozkładu, estymacja wariancji populacji",
        "Rozkład dochodów w populacji, ceny akcji na giełdzie, czasy reakcji w eksperymencie psychologicznym"
      ),
      stringsAsFactors = FALSE
    ) %>% setNames(c("Rozkład", "Parametry", "Przykłady zastosowań"))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

}
