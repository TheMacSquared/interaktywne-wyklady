# ============================================================================
# CHAPTER 4: Zalozenia chi-kwadrat i Fishera
# ============================================================================

ch4_ui <- lecture_chapter(
  id = "ch-chi-fisher",
  num = "04",
  title = "Założenia χ² i Fishera",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 04 · Założenia testów",
      num    = "04",
      title  = "Założenia χ² i Fishera.",
      lead   = "Testy dla zmiennych jakościowych mają prostsze, ale ważne założenia dotyczące minimalnych liczności."
    ),

    lc_h2("ch4-zalozenia-chi", "Założenia testu χ²"),

    tagList(
      p("Test chi-kwadrat (zgodności i niezależności) wymaga:"),
      tags$ol(
        tags$li(tags$b("Niezależność obserwacji"), " — każda obserwacja należy do jednej kategorii"),
        tags$li(tags$b("Oczekiwane liczności ≥ 5"), " — w każdej komórce tabeli"),
        tags$li("Próba losowa z populacji")
      ),
      p("Gdy oczekiwane liczności < 5, test χ² jest niedokładny.")
    ),

    # ========================================================================
    # WIDGET 1: Wizualizacja efektu malych licznosci
    # ========================================================================
    lc_h2("ch4-male-licznosci", "Efekt małych liczności"),

    figure_panel(
      label = "Ryc. 4.1",
      title = "Symulacja: χ² vs Fisher przy małych n",
      fluidRow(
        column(4,
          sliderInput("ch4_n", "Wielkość próby:", min = 10, max = 200, value = 20, step = 5),
          helpText("Generujemy 500 prób z H₀ prawdziwą (brak związku).
                    Sprawdzamy, jak często każdy test fałszywie odrzuci H₀."),
          actionButton("ch4_sim", "Symuluj",
                       class = "lc-btn-warning", width = "100%")
        ),
        column(8,
          uiOutput("ch4_sim_results"),
          zoom_plot_ui("ch4_sim_plot", height = "250px")
        )
      )
    ),

    lc_feedback(type = "danger",
      tags$strong("Problem:"),
      " Przy małych n, test χ² może dawać za dużo lub za mało fałszywych alarmów
        (niekontrolowany błąd I rodzaju). Test Fishera zachowuje się poprawnie."
    ),

    # ========================================================================
    # WIDGET 2: Kiedy ktory?
    # ========================================================================
    lc_h2("ch4-kiedy", "Kiedy χ², kiedy Fisher?"),

    lc_feedback(type = "ok",
      tags$strong("Zasady:"),
      tags$ul(
        tags$li("Wszystkie oczekiwane ≥ 5 → ", tags$b("test χ²")),
        tags$li("Którakolwiek oczekiwana < 5 → ", tags$b("test Fishera")),
        tags$li("Tabela 2×2 z małym n → zawsze ", tags$b("Fisher")),
        tags$li("Duża tabela + małe oczekiwane → ",
                tags$b("χ² z symulacją Monte Carlo"),
                " (", tags$code("chisq.test(simulate.p.value = TRUE)"), ")")
      )
    ),

    lc_feedback(type = "info",
      tags$strong("Założenia testu Fishera:"),
      p("Test Fishera nie ma założeń dotyczących minimalnych liczności —
        jest testem ", tags$b("dokładnym"), ". Wymaga jedynie niezależności obserwacji
        i stałych sum brzegowych.")
    ),

    # ========================================================================
    lc_h2("ch4-korelacja", "Założenia korelacji"),

    tagList(
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 14px;",
        tags$thead(
          tags$tr(tags$th("Test"), tags$th("Założenia"), tags$th("Alternatywa"))
        ),
        tags$tbody(
          tags$tr(
            tags$td("Pearson"),
            tags$td("Liniowość związku, normalność dwuwymiarowa, brak outlierów"),
            tags$td("Spearman (rangi)")
          ),
          tags$tr(
            tags$td("Spearman"),
            tags$td("Monotoniczność związku (słabsze niż liniowość)"),
            tags$td("Kendall tau (jeszcze bardziej odporny)")
          )
        )
      )
    ),

    lc_chapter_next(
      num = "05",
      title = "Mapa metod",
      lead = "szybkie przejście od metody do założeń i alternatyw.",
      target_id = "ch-mapa"
    )
  )
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
      lc_stat_box("χ²", round(fpr_chi, 1), "% fałszywych alarmów", color = chi_color),
      lc_stat_box("Fisher", round(fpr_fisher, 1), "% fałszywych alarmów", color = fisher_color),
      lc_stat_box("Oczekiwane", "5%", color = upwr_secondary)
    )
  })

  zoom_plot_server("ch4_sim_plot", reactive({
    df <- ch4_sim_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Symuluj'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      long <- data.frame(
        test = rep(c("χ²", "Fisher"), each = nrow(df)),
        p = c(df$p_chi, df$p_fisher)
      )
      long <- long[!is.na(long$p), ]

      ggplot(long, aes(x = p, fill = test)) +
        geom_histogram(breaks = seq(0, 1, by = 0.05), alpha = 0.6,
                       color = "white", position = "identity") +
        geom_vline(xintercept = 0.05, color = col_fail, linetype = "dashed") +
        scale_fill_manual(values = c(col_test, col_alt), name = NULL) +
        labs(
             x = "p-wartość", y = "Liczba") +
        theme_upwr() +
        theme(legend.position = "top")
    }
  }))
}
