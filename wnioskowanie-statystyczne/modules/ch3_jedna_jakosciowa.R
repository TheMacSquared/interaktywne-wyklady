# ============================================================================
# CHAPTER 4: Jedna zmienna jakosciowa — test dwumianowy
# ============================================================================

ch3_ui <- tabPanel("4. Jedna zmienna jakościowa",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Testowaliśmy średnią jednej zmiennej ilościowej.
       A co, gdy zmienna jest jakościowa — binarna (tak/nie, spełnia/nie spełnia)?"
    ),

    # ========================================================================
    # Wprowadzenie
    # ========================================================================
    div(class = "section-title", "Od pytania do testu dwumianowego"),

    div(class = "narrative",
      p("Gdy zmienna ma dwie kategorie (sukces/porażka, tak/nie, spełnia/nie spełnia),
        pytamy o proporcję w populacji."),
      p("Narzędzie: ", tags$b("test dwumianowy"),
        " — porównuje obserwowany odsetek z wartością referencyjną p₀."),
      p("Test dwumianowy jest dokładny — nie opiera się na przybliżeniu normalnym,
        działa nawet przy małych próbach."),
      div(class = "formula-box",
        p(withMathJax("\\(H_0: p = p_0\\)"), " — ",
          withMathJax("\\(H_a: p \\neq p_0\\)")),
        p(withMathJax("\\(\\text{Statystyka: } k \\text{ (liczba sukcesów w } n \\text{ próbach)}\\)")),
        p(withMathJax("\\(\\text{p-wartość: } P(K \\leq k \\text{ lub } K \\geq k) \\text{ przy } K \\sim B(n, p_0)\\)"))
      )
    ),

    # ========================================================================
    # WIDGET 1: Test dwumianowy dwustronny (krokowy)
    # ========================================================================
    div(class = "section-title", "Test dwumianowy — krok po kroku"),

    div(class = "widget-block",
      fluidRow(
        column(4,
          selectInput("ch3_scenario", "Scenariusz:",
            choices = c(
              "Jakość wody (p₀ = 80%)" = "water_quality",
              "Zdawalność egzaminu (p₀ = 60%)" = "exam_pass",
              "Kiedyłkość nasion (p₀ = 90%)" = "germination",
              "Produkty poza normą (p₀ = 3%)" = "defects"
            ),
            selected = "water_quality"
          ),
          sliderInput("ch3_n", "Wielkość próby (n):",
                      min = 20, max = 200, value = 50, step = 10),
          actionButton("ch3_new_sample", "Losuj próbę",
                       class = "btn-primary", width = "100%"),
          hr(),
          h5("Kroki testu:"),
          div(style = "display: flex; flex-direction: column; gap: 6px;",
            actionButton("ch3_step1", "1. Dane",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch3_step2", "2. Proporcja z próby",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch3_step3", "3. Rozkład pod H₀",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch3_step4", "4. p-wartość i decyzja",
                         class = "btn-outline-primary", width = "100%")
          )
        ),
        column(8,
          uiOutput("ch3_hypothesis_panel"),
          plotOutput("ch3_step_plot", height = "350px"),
          uiOutput("ch3_step_info")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Co zrobiliśmy?"),
      tags$ol(
        tags$li("Zebraliśmy dane — ile sukcesów w n próbach"),
        tags$li("Obliczyliśmy proporcję z próby: ", withMathJax("\\(\\hat{p} = k/n\\)")),
        tags$li("Sprawdziliśmy jak wygląda rozkład dwumianowy pod H₀"),
        tags$li("Policzyliśmy p-wartość — jak prawdopodobny jest nasz wynik (lub bardziej skrajny) jeśli H₀ prawdziwa")
      )
    ),

    # ========================================================================
    # WIDGET 2: Test dwumianowy jednostronny (te same dane)
    # ========================================================================
    div(class = "section-title", "A jeśli znamy kierunek?"),

    div(class = "narrative",
      p("Tak jak przy teście t — czasem nie pytamy „czy różni się?”,
        ale „czy jest większa / mniejsza niż p₀?”"),
      p("Użyjemy tych samych danych co powyżej, ale zmienimy pytanie na kierunkowe.")
    ),

    div(class = "widget-block",
      fluidRow(
        column(4,
          helpText("Dane: te same co w teście dwustronnym powyżej."),
          hr(),
          h5("Kroki testu:"),
          div(style = "display: flex; flex-direction: column; gap: 6px;",
            actionButton("ch3b_step1", "1. Dane",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch3b_step2", "2. Proporcja z próby",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch3b_step3", "3. Rozkład pod H₀",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch3b_step4", "4. p-wartość i decyzja",
                         class = "btn-outline-primary", width = "100%")
          )
        ),
        column(8,
          uiOutput("ch3b_hypothesis_panel"),
          plotOutput("ch3b_step_plot", height = "350px"),
          uiOutput("ch3b_step_info")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Dwustronny a jednostronny — proporcje:"),
      tags$ul(
        tags$li(tags$b("Dwustronny (≠):"),
          " p-wartość liczymy po obu stronach rozkładu. Bezpieczniejszy."),
        tags$li(tags$b("Jednostronny (> lub <):"),
          " p-wartość liczymy tylko po jednej stronie. Mocniejszy, ale ślepy na efekt w drugą stronę.")
      ),
      p("Te same dane, ten sam wynik k/n, ale ", tags$b("inna p-wartość"),
        " — bo inaczej zadane pytanie!")
    ),

    # ========================================================================
    # WIDGET 3: Porownanie — test dwumianowy vs test proporcji
    # ========================================================================
    div(class = "section-title", "Test dwumianowy a test proporcji"),

    div(class = "narrative",
      p("W Jamovi i wielu podręcznikach spotkasz też ",
        tags$b("test proporcji (z-test)"),
        ". Działa na przybliżeniu normalnym:"),
      div(class = "formula-box",
        p(withMathJax("\\(z = \\frac{\\hat{p} - p_0}{\\sqrt{p_0(1-p_0)/n}}\\)"))
      ),
      p("Porównajmy oba testy na tych samych danych:")
    ),

    div(class = "widget-block",
      h4("Porównanie wyników"),
      actionButton("ch3_compare", "Porównaj testy", class = "btn-primary", width = "100%"),
      br(), br(),
      uiOutput("ch3_compare_result")
    ),

    div(class = "callout-info",
      tags$strong("Kiedy który?"),
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th(""), tags$th("Test dwumianowy"), tags$th("Test proporcji (z-test)"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$b("Metoda")),
            tags$td("Dokładny — liczy z rozkładu B(n, p₀)"),
            tags$td("Przybliżony — używa rozkładu normalnego")
          ),
          tags$tr(
            tags$td(tags$b("Małe n")),
            tags$td(style = "background: var(--upwr-sage-tint);", "Działa zawsze"),
            tags$td(style = "background: var(--upwr-accent-tint);", "Może być niedokładny")
          ),
          tags$tr(
            tags$td(tags$b("Duże n")),
            tags$td("Działa, ale wolniejszy"),
            tags$td(style = "background: var(--upwr-sage-tint);", "Daje praktycznie ten sam wynik")
          ),
          tags$tr(
            tags$td(tags$b("W Jamovi")),
            tags$td("Binomial test"),
            tags$td("Proportion test (N Outcomes)")
          )
        )
      ),
      p("Reguła kciuka: jeśli ", withMathJax("\\(np_0 \\geq 10\\)"), " i ",
        withMathJax("\\(n(1-p_0) \\geq 10\\)"),
        " — oba testy dadzą praktycznie ten sam wynik.")
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: związek między dwiema zmiennymi ilościowymi"),
      actionButton("ch3_next", "Dalej → 5. Dwie zmienne ilościowe",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  # --- Parametry scenariuszy ---
  scenario_params <- list(
    water_quality = list(
      p0 = 0.80, p_true = 0.85, n_default = 50,
      success_label = "spełnia normę", failure_label = "nie spełnia",
      title = "Jakość próbek wody",
      question = "Czy odsetek próbek spełniających normy różni się od deklarowanych 80%?",
      h0_text = "\\(H_0: p = 0.80\\) (odsetek zgodny z deklaracją)",
      h1_text = "\\(H_a: p \\neq 0.80\\) (odsetek odbiega od deklaracji)",
      question_1s = "Czy odsetek próbek spełniających normy jest wyższy niż 80%?",
      h0_text_1s = "\\(H_0: p \\leq 0.80\\)",
      h1_text_1s = "\\(H_a: p > 0.80\\)",
      alt_1s = "greater"),
    exam_pass = list(
      p0 = 0.60, p_true = 0.68, n_default = 50,
      success_label = "zdał", failure_label = "nie zdał",
      title = "Zdawalność egzaminu",
      question = "Czy zdawalność różni się od 60% (wartość historyczna)?",
      h0_text = "\\(H_0: p = 0.60\\) (zdawalność typowa)",
      h1_text = "\\(H_a: p \\neq 0.60\\) (zdawalność odbiega od normy)",
      question_1s = "Czy zdawalność jest wyższa niż historyczne 60%?",
      h0_text_1s = "\\(H_0: p \\leq 0.60\\)",
      h1_text_1s = "\\(H_a: p > 0.60\\)",
      alt_1s = "greater"),
    germination = list(
      p0 = 0.90, p_true = 0.86, n_default = 50,
      success_label = "wykiełkowało", failure_label = "nie wykiełkowało",
      title = "Kiełkowalność nasion",
      question = "Czy kiełkowalność partii nasion różni się od deklarowanych 90%?",
      h0_text = "\\(H_0: p = 0.90\\) (kiełkowalność zgodna z deklaracją)",
      h1_text = "\\(H_a: p \\neq 0.90\\) (kiełkowalność odbiega)",
      question_1s = "Czy kiełkowalność jest niższa niż deklarowane 90%?",
      h0_text_1s = "\\(H_0: p \\geq 0.90\\)",
      h1_text_1s = "\\(H_a: p < 0.90\\)",
      alt_1s = "less"),
    defects = list(
      p0 = 0.03, p_true = 0.06, n_default = 50,
      success_label = "poza normą", failure_label = "w normie",
      title = "Kontrola jakości produktów",
      question = "Czy odsetek produktów nie spełniających normy różni się od dopuszczalnych 3%?",
      h0_text = "\\(H_0: p = 0.03\\) (odsetek wadliwych zgodny z normą)",
      h1_text = "\\(H_a: p \\neq 0.03\\) (odsetek odbiega od normy)",
      question_1s = "Czy odsetek produktów poza normą przekracza dopuszczalne 3%?",
      h0_text_1s = "\\(H_0: p \\leq 0.03\\)",
      h1_text_1s = "\\(H_a: p > 0.03\\)",
      alt_1s = "greater")
  )

  # --- Wspoldzielone dane ---
  ch3_data <- reactiveVal(NULL)  # list(k, n)
  ch3_step <- reactiveVal(0)
  ch3b_step <- reactiveVal(0)

  observeEvent(input$ch3_new_sample, {
    par <- scenario_params[[input$ch3_scenario]]
    n <- input$ch3_n
    k <- rbinom(1, n, par$p_true)
    ch3_data(list(k = k, n = n))
    ch3_step(0)
    ch3b_step(0)
  })

  observeEvent(input$ch3_scenario, {
    ch3_data(NULL)
    ch3_step(0)
    ch3b_step(0)
  })

  observeEvent(input$ch3_step1, ch3_step(1))
  observeEvent(input$ch3_step2, ch3_step(2))
  observeEvent(input$ch3_step3, ch3_step(3))
  observeEvent(input$ch3_step4, ch3_step(4))

  observeEvent(input$ch3b_step1, ch3b_step(1))
  observeEvent(input$ch3b_step2, ch3b_step(2))
  observeEvent(input$ch3b_step3, ch3b_step(3))
  observeEvent(input$ch3b_step4, ch3b_step(4))

  # =============================================
  # WIDGET 1: Dwustronny
  # =============================================

  output$ch3_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch3_scenario]]
    d <- ch3_data()

    tagList(
      div(class = "callout-info", style = "font-size: 16px;",
        p(tags$b("Pytanie potoczne:")),
        p(tags$em(paste0("„", par$question, "”")))
      ),
      div(class = "formula-box",
        p(tags$b("Hipoteza formalna (dwustronna):")),
        p(withMathJax(par$h0_text)),
        p(withMathJax(par$h1_text))
      ),
      if (is.null(d)) {
        div(style = "text-align: center; margin: 10px 0; color: var(--upwr-reference);",
          p(tags$em("Kliknij „Losuj próbę”"))
        )
      }
    )
  })

  output$ch3_step_plot <- renderPlot({
    d <- ch3_data()
    step <- ch3_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d)) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0

    if (step == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Próba gotowa! Klikaj kroki po kolei.",
                 size = 5, color = upwr_reference) +
        theme_void()
    } else if (step <= 2) {
      # Krok 1-2: slupki sukces/porazka
      df <- data.frame(
        kat = c(par$success_label, par$failure_label),
        count = c(k, n - k)
      )
      df$kat <- factor(df$kat, levels = c(par$success_label, par$failure_label))

      p <- ggplot(df, aes(x = kat, y = count, fill = kat)) +
        geom_col(alpha = 0.8, width = 0.6) +
        geom_text(aes(label = count), vjust = -0.5, size = 5, fontface = "bold") +
        scale_fill_manual(values = c(col_accept, col_reject)) +
        labs(title = par$title, x = NULL, y = "Liczba") +
        theme_educational() +
        theme(legend.position = "none")

      if (step >= 2) {
        phat <- k / n
        p <- p +
          annotate("text", x = 1.5, y = max(k, n - k) * 0.7,
                   label = paste0("p̂ = ", k, "/", n, " = ", round(phat, 3)),
                   size = 5, color = col_pvalue, fontface = "bold")
      }
      p
    } else {
      # Krok 3-4: rozklad dwumianowy pod H0
      x_vals <- 0:n
      probs <- dbinom(x_vals, n, p0)
      df <- data.frame(x = x_vals, prob = probs)

      # Wyznacz skrajne wartosci (dwustronnie)
      if (step == 4) {
        p_lower <- pbinom(k, n, p0)
        p_upper <- 1 - pbinom(k - 1, n, p0)
        # Dwustronna p-wartosc
        p_val <- binom.test(k, n, p0, alternative = "two.sided")$p.value
        df$extreme <- dbinom(x_vals, n, p0) <= dbinom(k, n, p0)
      } else {
        df$extreme <- FALSE
      }

      ggplot(df, aes(x = x, y = prob, fill = extreme)) +
        geom_col(width = 0.8, alpha = 0.7) +
        geom_vline(xintercept = k, color = col_reject, linewidth = 1.2) +
        scale_fill_manual(values = c("TRUE" = col_pvalue, "FALSE" = col_h0),
                          guide = "none") +
        annotate("text", x = k, y = max(probs) * 0.9,
                 label = paste0("k = ", k),
                 hjust = if (k > n * p0) -0.2 else 1.2,
                 color = col_reject, fontface = "bold") +
        labs(title = paste0("Rozkład B(", n, ", ", p0, ") pod H₀"),
             x = "Liczba sukcesów", y = "Prawdopodobieństwo") +
        theme_educational()
    }
  })

  output$ch3_step_info <- renderUI({
    d <- ch3_data()
    step <- ch3_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d) || step == 0) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0; phat <- k / n

    info <- switch(as.character(step),
      "1" = tagList(
        div(class = "stat-box", style = paste0("background:", col_h0, ";"),
            paste0("n = ", n)),
        div(class = "stat-box", style = paste0("background:", col_accept, ";"),
            paste0(par$success_label, ": ", k)),
        div(class = "stat-box", style = paste0("background:", col_reject, ";"),
            paste0(par$failure_label, ": ", n - k)),
        p("Mamy ", n, " obserwacji. Ile z nich to sukcesy?")
      ),
      "2" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
            paste0("p̂ = ", k, "/", n, " = ", round(phat, 3))),
        div(class = "stat-box", style = paste0("background:", col_dark, ";"),
            paste0("p₀ = ", p0)),
        p("Proporcja z próby: ", tags$b(round(phat, 3)),
          ". Wartość referencyjna: ", tags$b(p0),
          ". Różnica: ", tags$b(round(phat - p0, 3)),
          ". Ale czy to dużo?")
      ),
      "3" = tagList(
        p("Rozkład dwumianowy B(", n, ", ", p0,
          ") pokazuje ile sukcesów ",
          tags$em("spodziewalibyśmy się"), " gdyby H₀ była prawdziwa."),
        p("Czerwona linia = nasz wynik k = ", tags$b(k),
          ". Czy wypada w centrum czy na obrzeżach?")
      ),
      "4" = {
        test <- binom.test(k, n, p0, alternative = "two.sided")
        res <- format_test_result(test$p.value)
        tagList(
          div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
              paste0("p = ", format.pval(test$p.value, digits = 4))),
          p(style = paste0("color: ", res$color, "; font-weight: bold; font-size: 16px;"),
            res$decision),
          p(res$explanation)
        )
      }
    )
    div(class = "callout-info", info)
  })

  # =============================================
  # WIDGET 2: Jednostronny (te same dane)
  # =============================================

  output$ch3b_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch3_scenario]]
    d <- ch3_data()

    tagList(
      div(class = "callout-info", style = "font-size: 16px;",
        p(tags$b("Pytanie potoczne (kierunkowe):")),
        p(tags$em(paste0("„", par$question_1s, "”")))
      ),
      div(class = "formula-box",
        p(tags$b("Hipoteza formalna (jednostronna!):")),
        p(withMathJax(par$h0_text_1s)),
        p(withMathJax(par$h1_text_1s))
      ),
      if (is.null(d)) {
        div(style = "text-align: center; margin: 10px 0; color: var(--upwr-reference);",
          p(tags$em("Najpierw wylosuj próbę w teście dwustronnym powyżej"))
        )
      }
    )
  })

  output$ch3b_step_plot <- renderPlot({
    d <- ch3_data()
    step <- ch3b_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d) || step == 0) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0

    if (step <= 2) {
      df <- data.frame(
        kat = c(par$success_label, par$failure_label),
        count = c(k, n - k)
      )
      df$kat <- factor(df$kat, levels = c(par$success_label, par$failure_label))

      p <- ggplot(df, aes(x = kat, y = count, fill = kat)) +
        geom_col(alpha = 0.8, width = 0.6) +
        geom_text(aes(label = count), vjust = -0.5, size = 5, fontface = "bold") +
        scale_fill_manual(values = c(col_accept, col_reject)) +
        labs(title = par$title, x = NULL, y = "Liczba") +
        theme_educational() +
        theme(legend.position = "none")

      if (step >= 2) {
        phat <- k / n
        p <- p +
          annotate("text", x = 1.5, y = max(k, n - k) * 0.7,
                   label = paste0("p̂ = ", round(phat, 3), " (te same dane)"),
                   size = 5, color = col_pvalue, fontface = "bold")
      }
      p
    } else {
      # Krok 3-4: rozklad z zaznaczonym jednym ogonem
      x_vals <- 0:n
      probs <- dbinom(x_vals, n, p0)
      df <- data.frame(x = x_vals, prob = probs)

      if (step == 4) {
        if (par$alt_1s == "greater") {
          df$extreme <- x_vals >= k
        } else {
          df$extreme <- x_vals <= k
        }
      } else {
        df$extreme <- FALSE
      }

      ggplot(df, aes(x = x, y = prob, fill = extreme)) +
        geom_col(width = 0.8, alpha = 0.7) +
        geom_vline(xintercept = k, color = col_reject, linewidth = 1.2) +
        scale_fill_manual(values = c("TRUE" = col_pvalue, "FALSE" = col_h0),
                          guide = "none") +
        annotate("text", x = k, y = max(probs) * 0.9,
                 label = paste0("k = ", k),
                 hjust = if (k > n * p0) -0.2 else 1.2,
                 color = col_reject, fontface = "bold") +
        labs(title = paste0("Rozkład B(", n, ", ", p0, ") pod H₀"),
             subtitle = paste0("Test jednostronny (",
                               if (par$alt_1s == "greater") "prawy" else "lewy",
                               " ogon)"),
             x = "Liczba sukcesów", y = "Prawdopodobieństwo") +
        theme_educational()
    }
  })

  output$ch3b_step_info <- renderUI({
    d <- ch3_data()
    step <- ch3b_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d) || step == 0) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0; phat <- k / n
    dir_label <- if (par$alt_1s == "greater") "większa" else "mniejsza"

    info <- switch(as.character(step),
      "1" = tagList(
        div(class = "stat-box", style = paste0("background:", col_h0, ";"),
            paste0("n = ", n, " (te same dane co wyżej)")),
        p("Pytamy, czy proporcja sukcesów jest ", dir_label, " niż p₀ = ", p0, ".")
      ),
      "2" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
            paste0("p̂ = ", round(phat, 3), " (ta sama wartość!)")),
        p("Statystyki takie same — dane się nie zmieniły.
          Zmieniło się tylko pytanie (kierunek).")
      ),
      "3" = tagList(
        p("Ten sam rozkład B(", n, ", ", p0,
          "), ale teraz patrzymy tylko na ",
          tags$b(if (par$alt_1s == "greater") "prawy" else "lewy"), " ogon.")
      ),
      "4" = {
        test <- binom.test(k, n, p0, alternative = par$alt_1s)
        res <- format_test_result(test$p.value)
        tagList(
          div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
              paste0("p = ", format.pval(test$p.value, digits = 4),
                     " (jednostronnie!)")),
          p(style = paste0("color: ", res$color, "; font-weight: bold; font-size: 16px;"),
            res$decision),
          p(res$explanation),
          p(tags$em("Porównaj z testem dwustronnym wyżej — te same dane,
            ale inna p-wartość!"))
        )
      }
    )
    div(class = "callout-info", info)
  })

  # =============================================
  # WIDGET 3: Porownanie dwumianowy vs proporcji
  # =============================================

  output$ch3_compare_result <- renderUI({
    req(input$ch3_compare)
    d <- isolate(ch3_data())
    par <- isolate(scenario_params[[input$ch3_scenario]])

    if (is.null(d)) {
      return(div(class = "callout-warning",
        "Najpierw wylosuj próbę w widgecie powyżej."))
    }

    k <- d$k; n <- d$n; p0 <- par$p0; phat <- k / n

    # Test dwumianowy
    binom_res <- binom.test(k, n, p0, alternative = "two.sided")

    # Test proporcji (z-test z poprawką ciągłości)
    prop_res <- prop.test(k, n, p = p0, alternative = "two.sided", correct = TRUE)

    # Statystyka z ręcznie
    z_stat <- (phat - p0) / sqrt(p0 * (1 - p0) / n)

    # Warunki przybliżenia normalnego
    np0 <- n * p0
    nq0 <- n * (1 - p0)
    ok <- np0 >= 10 && nq0 >= 10

    div(
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th(""), tags$th("Test dwumianowy"), tags$th("Test proporcji (z)"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$b("Dane")),
            tags$td(paste0("k = ", k, ", n = ", n)),
            tags$td(paste0("k = ", k, ", n = ", n))
          ),
          tags$tr(
            tags$td(tags$b("Statystyka")),
            tags$td(paste0("k = ", k, " (dokładna)")),
            tags$td(paste0("z = ", round(z_stat, 3)))
          ),
          tags$tr(
            tags$td(tags$b("p-wartość")),
            tags$td(tags$b(format.pval(binom_res$p.value, digits = 4))),
            tags$td(tags$b(format.pval(prop_res$p.value, digits = 4)))
          ),
          tags$tr(
            tags$td(tags$b("Decyzja")),
            tags$td(style = paste0("color:", format_test_result(binom_res$p.value)$color),
                    format_test_result(binom_res$p.value)$decision),
            tags$td(style = paste0("color:", format_test_result(prop_res$p.value)$color),
                    format_test_result(prop_res$p.value)$decision)
          )
        )
      ),
      div(class = if (ok) "callout-success" else "callout-danger",
        p(tags$b("Warunki przybliżenia normalnego: "),
          withMathJax(paste0("\\(np_0 = ", round(np0, 1), "\\)")),
          " i ",
          withMathJax(paste0("\\(n(1-p_0) = ", round(nq0, 1), "\\)")),
          if (ok) " — oba ≥ 10, przybliżenie działa dobrze."
          else " — warunek niespiełniony! Test proporcji może być niedokładny.")
      )
    )
  })
}
