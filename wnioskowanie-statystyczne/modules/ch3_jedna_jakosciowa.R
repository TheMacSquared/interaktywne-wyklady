# ============================================================================
# CHAPTER 4: Jedna zmienna jakosciowa — test dwumianowy
# ============================================================================

ch3_ui <- tabPanel("4. Jedna zmienna jako\u015bciowa",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Testowali\u015bmy \u015bredni\u0105 jednej zmiennej ilo\u015bciowej.
       A co, gdy zmienna jest jako\u015bciowa \u2014 binarna (tak/nie, spe\u0142nia/nie spe\u0142nia)?"
    ),

    # ========================================================================
    # Wprowadzenie
    # ========================================================================
    div(class = "section-title", "Od pytania do testu dwumianowego"),

    div(class = "narrative",
      p("Gdy zmienna ma dwie kategorie (sukces/pora\u017cka, tak/nie, spe\u0142nia/nie spe\u0142nia),
        pytamy o proporcj\u0119 w populacji."),
      p("Narz\u0119dzie: ", tags$b("test dwumianowy"),
        " \u2014 por\u00f3wnuje obserwowany odsetek z warto\u015bci\u0105 referencyjn\u0105 p\u2080."),
      p("Test dwumianowy jest dok\u0142adny \u2014 nie opiera si\u0119 na przybli\u017ceniu normalnym,
        dzia\u0142a nawet przy ma\u0142ych pr\u00f3bach."),
      div(class = "formula-box",
        p(withMathJax("\\(H_0: p = p_0 \\quad\\text{vs}\\quad H_a: p \\neq p_0\\)")),
        p(withMathJax("\\(\\text{Statystyka: } k \\text{ (liczba sukces\u00f3w w } n \\text{ pr\u00f3bach)}\\)")),
        p(withMathJax("\\(\\text{p-warto\u015b\u0107: } P(K \\leq k \\text{ lub } K \\geq k) \\text{ przy } K \\sim B(n, p_0)\\)"))
      )
    ),

    # ========================================================================
    # WIDGET 1: Test dwumianowy dwustronny (krokowy)
    # ========================================================================
    div(class = "section-title", "Test dwumianowy \u2014 krok po kroku"),

    div(class = "widget-block",
      fluidRow(
        column(4,
          selectInput("ch3_scenario", "Scenariusz:",
            choices = c(
              "Jako\u015b\u0107 wody (p\u2080 = 80%)" = "water_quality",
              "Zdawalno\u015b\u0107 egzaminu (p\u2080 = 60%)" = "exam_pass",
              "Kiedy\u0142ko\u015b\u0107 nasion (p\u2080 = 90%)" = "germination",
              "Produkty poza norm\u0105 (p\u2080 = 3%)" = "defects"
            ),
            selected = "water_quality"
          ),
          sliderInput("ch3_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 20, max = 200, value = 50, step = 10),
          actionButton("ch3_new_sample", "Losuj pr\u00f3b\u0119",
                       class = "btn-primary", width = "100%"),
          hr(),
          h5("Kroki testu:"),
          div(style = "display: flex; flex-direction: column; gap: 6px;",
            actionButton("ch3_step1", "1. Dane",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch3_step2", "2. Proporcja z pr\u00f3by",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch3_step3", "3. Rozk\u0142ad pod H\u2080",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch3_step4", "4. p-warto\u015b\u0107 i decyzja",
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
      tags$strong("Co zrobili\u015bmy?"),
      tags$ol(
        tags$li("Zebrali\u015bmy dane \u2014 ile sukces\u00f3w w n pr\u00f3bach"),
        tags$li("Obliczyli\u015bmy proporcj\u0119 z pr\u00f3by: ", withMathJax("\\(\\hat{p} = k/n\\)")),
        tags$li("Sprawdzili\u015bmy jak wygl\u0105da rozk\u0142ad dwumianowy pod H\u2080"),
        tags$li("Policzyli\u015bmy p-warto\u015b\u0107 \u2014 jak prawdopodobny jest nasz wynik (lub bardziej skrajny) je\u015bli H\u2080 prawdziwa")
      )
    ),

    # ========================================================================
    # WIDGET 2: Test dwumianowy jednostronny (te same dane)
    # ========================================================================
    div(class = "section-title", "A je\u015bli znamy kierunek?"),

    div(class = "narrative",
      p("Tak jak przy te\u015bcie t \u2014 czasem nie pytamy \u201eczy r\u00f3\u017cni si\u0119?\u201d,
        ale \u201eczy jest wi\u0119ksza / mniejsza ni\u017c p\u2080?\u201d"),
      p("U\u017cyjemy tych samych danych co powy\u017cej, ale zmienimy pytanie na kierunkowe.")
    ),

    div(class = "widget-block",
      fluidRow(
        column(4,
          helpText("Dane: te same co w te\u015bcie dwustronnym powy\u017cej."),
          hr(),
          h5("Kroki testu:"),
          div(style = "display: flex; flex-direction: column; gap: 6px;",
            actionButton("ch3b_step1", "1. Dane",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch3b_step2", "2. Proporcja z pr\u00f3by",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch3b_step3", "3. Rozk\u0142ad pod H\u2080",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch3b_step4", "4. p-warto\u015b\u0107 i decyzja",
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
      tags$strong("Dwustronny vs jednostronny \u2014 proporcje:"),
      tags$ul(
        tags$li(tags$b("Dwustronny (\u2260):"),
          " p-warto\u015b\u0107 liczymy po obu stronach rozk\u0142adu. Bezpieczniejszy."),
        tags$li(tags$b("Jednostronny (> lub <):"),
          " p-warto\u015b\u0107 liczymy tylko po jednej stronie. Mocniejszy, ale \u015blepy na efekt w drug\u0105 stron\u0119.")
      ),
      p("Te same dane, ten sam wynik k/n, ale ", tags$b("inna p-warto\u015b\u0107"),
        " \u2014 bo inaczej zadane pytanie!")
    ),

    # ========================================================================
    # WIDGET 3: Porownanie — test dwumianowy vs test proporcji
    # ========================================================================
    div(class = "section-title", "Test dwumianowy vs test proporcji"),

    div(class = "narrative",
      p("W Jamovi i wielu podr\u0119cznikach spotkasz te\u017c ",
        tags$b("test proporcji (z-test)"),
        ". Dzia\u0142a na przybli\u017ceniu normalnym:"),
      div(class = "formula-box",
        p(withMathJax("\\(z = \\frac{\\hat{p} - p_0}{\\sqrt{p_0(1-p_0)/n}}\\)"))
      ),
      p("Por\u00f3wnajmy oba testy na tych samych danych:")
    ),

    div(class = "widget-block",
      h4("Por\u00f3wnanie wynik\u00f3w"),
      actionButton("ch3_compare", "Por\u00f3wnaj testy", class = "btn-primary", width = "100%"),
      br(), br(),
      uiOutput("ch3_compare_result")
    ),

    div(class = "callout-info",
      tags$strong("Kiedy kt\u00f3ry?"),
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th(""), tags$th("Test dwumianowy"), tags$th("Test proporcji (z-test)"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$b("Metoda")),
            tags$td("Dok\u0142adny \u2014 liczy z rozk\u0142adu B(n, p\u2080)"),
            tags$td("Przybli\u017cony \u2014 u\u017cywa rozk\u0142adu normalnego")
          ),
          tags$tr(
            tags$td(tags$b("Ma\u0142e n")),
            tags$td(style = "background: #eafaf1;", "Dzia\u0142a zawsze"),
            tags$td(style = "background: #fdedec;", "Mo\u017ce by\u0107 niedok\u0142adny")
          ),
          tags$tr(
            tags$td(tags$b("Du\u017ce n")),
            tags$td("Dzia\u0142a, ale wolniejszy"),
            tags$td(style = "background: #eafaf1;", "Daje praktycznie ten sam wynik")
          ),
          tags$tr(
            tags$td(tags$b("W Jamovi")),
            tags$td("Binomial test"),
            tags$td("Proportion test (N Outcomes)")
          )
        )
      ),
      p("Regu\u0142a kciuka: je\u015bli ", withMathJax("\\(np_0 \\geq 10\\)"), " i ",
        withMathJax("\\(n(1-p_0) \\geq 10\\)"),
        " \u2014 oba testy dadz\u0105 praktycznie ten sam wynik.")
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: zwi\u0105zek mi\u0119dzy dwiema zmiennymi ilo\u015bciowymi"),
      actionButton("ch3_next", "Dalej \u2192 5. Dwie zmienne ilo\u015bciowe",
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
      success_label = "spe\u0142nia norm\u0119", failure_label = "nie spe\u0142nia",
      title = "Jako\u015b\u0107 pr\u00f3bek wody",
      question = "Czy odsetek pr\u00f3bek spe\u0142niaj\u0105cych normy r\u00f3\u017cni si\u0119 od deklarowanych 80%?",
      h0_text = "\\(H_0: p = 0.80\\) (odsetek zgodny z deklaracj\u0105)",
      h1_text = "\\(H_a: p \\neq 0.80\\) (odsetek odbiega od deklaracji)",
      question_1s = "Czy odsetek pr\u00f3bek spe\u0142niaj\u0105cych normy jest wy\u017cszy ni\u017c 80%?",
      h0_text_1s = "\\(H_0: p \\leq 0.80\\)",
      h1_text_1s = "\\(H_a: p > 0.80\\)",
      alt_1s = "greater"),
    exam_pass = list(
      p0 = 0.60, p_true = 0.68, n_default = 50,
      success_label = "zda\u0142", failure_label = "nie zda\u0142",
      title = "Zdawalno\u015b\u0107 egzaminu",
      question = "Czy zdawalno\u015b\u0107 r\u00f3\u017cni si\u0119 od 60% (warto\u015b\u0107 historyczna)?",
      h0_text = "\\(H_0: p = 0.60\\) (zdawalno\u015b\u0107 typowa)",
      h1_text = "\\(H_a: p \\neq 0.60\\) (zdawalno\u015b\u0107 odbiega od normy)",
      question_1s = "Czy zdawalno\u015b\u0107 jest wy\u017csza ni\u017c historyczne 60%?",
      h0_text_1s = "\\(H_0: p \\leq 0.60\\)",
      h1_text_1s = "\\(H_a: p > 0.60\\)",
      alt_1s = "greater"),
    germination = list(
      p0 = 0.90, p_true = 0.86, n_default = 50,
      success_label = "wykie\u0142kowa\u0142o", failure_label = "nie wykie\u0142kowa\u0142o",
      title = "Kie\u0142kowalno\u015b\u0107 nasion",
      question = "Czy kie\u0142kowalno\u015b\u0107 partii nasion r\u00f3\u017cni si\u0119 od deklarowanych 90%?",
      h0_text = "\\(H_0: p = 0.90\\) (kie\u0142kowalno\u015b\u0107 zgodna z deklaracj\u0105)",
      h1_text = "\\(H_a: p \\neq 0.90\\) (kie\u0142kowalno\u015b\u0107 odbiega)",
      question_1s = "Czy kie\u0142kowalno\u015b\u0107 jest ni\u017csza ni\u017c deklarowane 90%?",
      h0_text_1s = "\\(H_0: p \\geq 0.90\\)",
      h1_text_1s = "\\(H_a: p < 0.90\\)",
      alt_1s = "less"),
    defects = list(
      p0 = 0.03, p_true = 0.06, n_default = 50,
      success_label = "poza norm\u0105", failure_label = "w normie",
      title = "Kontrola jako\u015bci produkt\u00f3w",
      question = "Czy odsetek produkt\u00f3w nie spe\u0142niaj\u0105cych normy r\u00f3\u017cni si\u0119 od dopuszczalnych 3%?",
      h0_text = "\\(H_0: p = 0.03\\) (odsetek wadliwych zgodny z norm\u0105)",
      h1_text = "\\(H_a: p \\neq 0.03\\) (odsetek odbiega od normy)",
      question_1s = "Czy odsetek produkt\u00f3w poza norm\u0105 przekracza dopuszczalne 3%?",
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
        p(tags$em(paste0("\u201e", par$question, "\u201d")))
      ),
      div(class = "formula-box",
        p(tags$b("Hipoteza formalna (dwustronna):")),
        p(withMathJax(par$h0_text)),
        p(withMathJax(par$h1_text))
      ),
      if (is.null(d)) {
        div(style = "text-align: center; margin: 10px 0; color: #7f8c8d;",
          p(tags$em("Kliknij \u201eLosuj pr\u00f3b\u0119\u201d"))
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
                 label = "Pr\u00f3ba gotowa! Klikaj kroki po kolei.",
                 size = 5, color = "#7f8c8d") +
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
                   label = paste0("p\u0302 = ", k, "/", n, " = ", round(phat, 3)),
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
        labs(title = paste0("Rozk\u0142ad B(", n, ", ", p0, ") pod H\u2080"),
             x = "Liczba sukces\u00f3w", y = "Prawdopodobie\u0144stwo") +
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
            paste0("p\u0302 = ", k, "/", n, " = ", round(phat, 3))),
        div(class = "stat-box", style = paste0("background:", col_dark, ";"),
            paste0("p\u2080 = ", p0)),
        p("Proporcja z pr\u00f3by: ", tags$b(round(phat, 3)),
          ". Warto\u015b\u0107 referencyjna: ", tags$b(p0),
          ". R\u00f3\u017cnica: ", tags$b(round(phat - p0, 3)),
          ". Ale czy to du\u017co?")
      ),
      "3" = tagList(
        p("Rozk\u0142ad dwumianowy B(", n, ", ", p0,
          ") pokazuje ile sukces\u00f3w ",
          tags$em("spodziewaliby\u015bmy si\u0119"), " gdyby H\u2080 by\u0142a prawdziwa."),
        p("Czerwona linia = nasz wynik k = ", tags$b(k),
          ". Czy wypada w centrum czy na obrze\u017cach?")
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
        p(tags$em(paste0("\u201e", par$question_1s, "\u201d")))
      ),
      div(class = "formula-box",
        p(tags$b("Hipoteza formalna (jednostronna!):")),
        p(withMathJax(par$h0_text_1s)),
        p(withMathJax(par$h1_text_1s))
      ),
      if (is.null(d)) {
        div(style = "text-align: center; margin: 10px 0; color: #7f8c8d;",
          p(tags$em("Najpierw wylosuj pr\u00f3b\u0119 w te\u015bcie dwustronnym powy\u017cej"))
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
                   label = paste0("p\u0302 = ", round(phat, 3), " (te same dane)"),
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
        labs(title = paste0("Rozk\u0142ad B(", n, ", ", p0, ") pod H\u2080"),
             subtitle = paste0("Test jednostronny (",
                               if (par$alt_1s == "greater") "prawy" else "lewy",
                               " ogon)"),
             x = "Liczba sukces\u00f3w", y = "Prawdopodobie\u0144stwo") +
        theme_educational()
    }
  })

  output$ch3b_step_info <- renderUI({
    d <- ch3_data()
    step <- ch3b_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d) || step == 0) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0; phat <- k / n
    dir_label <- if (par$alt_1s == "greater") "wi\u0119ksza" else "mniejsza"

    info <- switch(as.character(step),
      "1" = tagList(
        div(class = "stat-box", style = paste0("background:", col_h0, ";"),
            paste0("n = ", n, " (te same dane co wy\u017cej)")),
        p("Pytamy, czy proporcja sukces\u00f3w jest ", dir_label, " ni\u017c p\u2080 = ", p0, ".")
      ),
      "2" = tagList(
        div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
            paste0("p\u0302 = ", round(phat, 3), " (ta sama warto\u015b\u0107!)")),
        p("Statystyki takie same \u2014 dane si\u0119 nie zmieni\u0142y.
          Zmieni\u0142o si\u0119 tylko pytanie (kierunek).")
      ),
      "3" = tagList(
        p("Ten sam rozk\u0142ad B(", n, ", ", p0,
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
          p(tags$em("Por\u00f3wnaj z testem dwustronnym wy\u017cej \u2014 te same dane,
            ale inna p-warto\u015b\u0107!"))
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
        "Najpierw wylosuj pr\u00f3b\u0119 w widgecie powy\u017cej."))
    }

    k <- d$k; n <- d$n; p0 <- par$p0; phat <- k / n

    # Test dwumianowy
    binom_res <- binom.test(k, n, p0, alternative = "two.sided")

    # Test proporcji (z-test z poprawk\u0105 ci\u0105g\u0142o\u015bci)
    prop_res <- prop.test(k, n, p = p0, alternative = "two.sided", correct = TRUE)

    # Statystyka z r\u0119cznie
    z_stat <- (phat - p0) / sqrt(p0 * (1 - p0) / n)

    # Warunki przybli\u017cenia normalnego
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
            tags$td(paste0("k = ", k, " (dok\u0142adna)")),
            tags$td(paste0("z = ", round(z_stat, 3)))
          ),
          tags$tr(
            tags$td(tags$b("p-warto\u015b\u0107")),
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
        p(tags$b("Warunki przybli\u017cenia normalnego: "),
          withMathJax(paste0("\\(np_0 = ", round(np0, 1), "\\)")),
          " i ",
          withMathJax(paste0("\\(n(1-p_0) = ", round(nq0, 1), "\\)")),
          if (ok) " \u2014 oba \u2265 10, przybli\u017cenie dzia\u0142a dobrze."
          else " \u2014 warunek niespie\u0142niony! Test proporcji mo\u017ce by\u0107 niedok\u0142adny.")
      )
    )
  })
}
