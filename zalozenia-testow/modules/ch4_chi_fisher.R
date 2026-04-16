# ============================================================================
# CHAPTER 4: Zalozenia chi-kwadrat i Fishera
# ============================================================================

ch4_ui <- tabPanel("4. Za\u0142o\u017cenia \u03c7\u00b2 i Fishera",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Testy dla zmiennych jako\u015bciowych maj\u0105 prostsze, ale wa\u017cne za\u0142o\u017cenia
       dotycz\u0105ce minimalnych liczno\u015bci."
    ),

    div(class = "section-title", "Za\u0142o\u017cenia testu \u03c7\u00b2"),

    div(class = "narrative",
      p("Test chi-kwadrat (zgodno\u015bci i niezale\u017cno\u015bci) wymaga:"),
      tags$ol(
        tags$li(tags$b("Niezale\u017cno\u015b\u0107 obserwacji"), " \u2014 ka\u017cda obserwacja nale\u017cy do jednej kategorii"),
        tags$li(tags$b("Oczekiwane liczno\u015bci \u2265 5"), " \u2014 w ka\u017cdej kom\u00f3rce tabeli"),
        tags$li("Pr\u00f3ba losowa z populacji")
      ),
      p("Gdy oczekiwane liczno\u015bci < 5, test \u03c7\u00b2 jest niedok\u0142adny.")
    ),

    # ========================================================================
    # WIDGET 1: Wizualizacja efektu malych licznosci
    # ========================================================================
    div(class = "section-title", "Efekt ma\u0142ych liczno\u015bci"),

    div(class = "widget-block",
      h4("Symulacja: \u03c7\u00b2 vs Fisher przy ma\u0142ych n"),
      fluidRow(
        column(4,
          sliderInput("ch4_n", "Wielko\u015b\u0107 pr\u00f3by:", min = 10, max = 200, value = 20, step = 5),
          helpText("Generujemy 500 pr\u00f3b z H\u2080 prawdziw\u0105 (brak zwi\u0105zku).
                    Sprawdzamy, jak cz\u0119sto ka\u017cdy test fa\u0142szywie odrzuci H\u2080."),
          actionButton("ch4_sim", "Symuluj",
                       class = "btn-warning", width = "100%")
        ),
        column(8,
          uiOutput("ch4_sim_results"),
          plotOutput("ch4_sim_plot", height = "250px")
        )
      )
    ),

    div(class = "callout-danger",
      tags$strong("Problem:"),
      " Przy ma\u0142ych n, test \u03c7\u00b2 mo\u017ce dawa\u0107 za du\u017co lub za ma\u0142o fa\u0142szywych alarm\u00f3w
        (niekontrolowany b\u0142\u0105d I rodzaju). Test Fishera zachowuje si\u0119 poprawnie."
    ),

    # ========================================================================
    # WIDGET 2: Kiedy ktory?
    # ========================================================================
    div(class = "section-title", "Kiedy \u03c7\u00b2, kiedy Fisher?"),

    div(class = "callout-success",
      tags$strong("Zasady:"),
      tags$ul(
        tags$li("Wszystkie oczekiwane \u2265 5 \u2192 ", tags$b("test \u03c7\u00b2")),
        tags$li("Kt\u00f3rakolwiek oczekiwana < 5 \u2192 ", tags$b("test Fishera")),
        tags$li("Tabela 2\u00d72 z ma\u0142ym n \u2192 zawsze ", tags$b("Fisher")),
        tags$li("Du\u017ca tabela + ma\u0142e oczekiwane \u2192 ",
                tags$b("\u03c7\u00b2 z symulacj\u0105 Monte Carlo"),
                " (", tags$code("chisq.test(simulate.p.value = TRUE)"), ")")
      )
    ),

    div(class = "callout-info",
      tags$strong("Za\u0142o\u017cenia testu Fishera:"),
      p("Test Fishera nie ma za\u0142o\u017ce\u0144 dotycz\u0105cych minimalnych liczno\u015bci \u2014
        jest testem ", tags$b("dok\u0142adnym"), ". Wymaga jedynie niezale\u017cno\u015bci obserwacji
        i sta\u0142ych sum brzegowych.")
    ),

    # ========================================================================
    div(class = "section-title", "Za\u0142o\u017cenia korelacji"),

    div(class = "narrative",
      tags$table(class = "table table-bordered", style = "font-size: 14px;",
        tags$thead(
          tags$tr(tags$th("Test"), tags$th("Za\u0142o\u017cenia"), tags$th("Alternatywa"))
        ),
        tags$tbody(
          tags$tr(
            tags$td("Pearson"),
            tags$td("Liniowo\u015b\u0107 zwi\u0105zku, normalno\u015b\u0107 dwuwymiarowa, brak outlier\u00f3w"),
            tags$td("Spearman (rangi)")
          ),
          tags$tr(
            tags$td("Spearman"),
            tags$td("Monotoniczno\u015b\u0107 zwi\u0105zku (s\u0142absze ni\u017c liniowo\u015b\u0107)"),
            tags$td("Kendall tau (jeszcze bardziej odporny)")
          )
        )
      )
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: kompleksowa mapa metod z za\u0142o\u017ceniami"),
      actionButton("ch4_next", "Dalej \u2192 5. Mapa metod",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  ch4_sim_data <- reactiveVal(NULL)

  observeEvent(input$ch4_sim, {
    n <- input$ch4_n
    n_sims <- 500

    results <- sapply(1:n_sims, function(i) {
      # H0 prawdziwa: brak zwiazku
      x <- sample(c("A", "B"), n, replace = TRUE)
      y <- sample(c("Tak", "Nie"), n, replace = TRUE)
      tab <- table(x, y)

      p_chi <- tryCatch(chisq.test(tab, correct = FALSE)$p.value, error = function(e) NA)
      p_fisher <- fisher.test(tab)$p.value

      c(p_chi = p_chi, p_fisher = p_fisher)
    })

    results_df <- data.frame(t(results))
    ch4_sim_data(results_df)
  })

  output$ch4_sim_results <- renderUI({
    df <- ch4_sim_data()
    if (is.null(df)) return(NULL)

    fpr_chi <- mean(df$p_chi < 0.05, na.rm = TRUE) * 100
    fpr_fisher <- mean(df$p_fisher < 0.05, na.rm = TRUE) * 100

    chi_color <- if (abs(fpr_chi - 5) <= 2) col_ok else col_fail
    fisher_color <- if (abs(fpr_fisher - 5) <= 2) col_ok else col_fail

    tagList(
      div(class = "stat-box", style = paste0("background:", chi_color, ";"),
          paste0("\u03c7\u00b2: ", round(fpr_chi, 1), "% fa\u0142szywych alarm\u00f3w")),
      div(class = "stat-box", style = paste0("background:", fisher_color, ";"),
          paste0("Fisher: ", round(fpr_fisher, 1), "% fa\u0142szywych alarm\u00f3w")),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          "Oczekiwane: 5%")
    )
  })

  output$ch4_sim_plot <- renderPlot({
    df <- ch4_sim_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Symuluj'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      long <- data.frame(
        test = rep(c("\u03c7\u00b2", "Fisher"), each = nrow(df)),
        p = c(df$p_chi, df$p_fisher)
      )
      long <- long[!is.na(long$p), ]

      ggplot(long, aes(x = p, fill = test)) +
        geom_histogram(breaks = seq(0, 1, by = 0.05), alpha = 0.6,
                       color = "white", position = "identity") +
        geom_vline(xintercept = 0.05, color = col_fail, linetype = "dashed") +
        scale_fill_manual(values = c(col_test, col_alt), name = NULL) +
        labs(title = paste0("Rozk\u0142ad p-warto\u015bci (H\u2080 prawdziwa, n = ", input$ch4_n, ")"),
             x = "p-warto\u015b\u0107", y = "Liczba") +
        theme_educational() +
        theme(legend.position = "top")
    }
  })
}
