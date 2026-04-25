# Tab 3: Grupa — za mało danych (n=8), zły zbiór

ch3_ui <- lecture_chapter(id = "ch3", num = "3", title = "Grupa", content = tagList(
  fluidRow(column(8, offset = 2,

    lc_h2("sec-01", "Ankieta na grupie"),

    div(class = "lc-prose",
      p("Kolega zbiera dane do projektu. Dzień przed deadline'em pyta 8 znajomych
        ze swojej grupy. Oto co uzyskał:")
    ),

    lc_h2("sec-02", "Podgląd danych"),

    div(class = "lc-figure-panel",
      DT::dataTableOutput("tab2_table")
    ),

    lc_h2("sec-03", "Ile obserwacji naprawdę potrzebujesz?"),

    div(class = "lc-figure-panel",
      sliderInput("tab2_n", "Liczba obserwacji:", min = 5, max = 200, value = 8, step = 1),
      fluidRow(
        column(6, plotOutput("tab2_hist", height = "280px")),
        column(6, plotOutput("tab2_ci", height = "280px"))
      ),
      plotOutput("tab2_power", height = "280px")
    ),

    lc_h2("sec-04", "Werdykt"),

    div(class = "lc-feedback lc-feedback-danger",
      tags$strong("Problem:"), " n = 8 to zdecydowanie za mało.",
      tags$br(),
      "Przy tak małej próbie moc testu wynosi ok. 10-15% - nawet duża różnica ",
      "między grupami będzie nieistotna statystycznie.",
      tags$br(), tags$br(),
      tags$strong("Zasada:"), " Liczy się n na grupę, nie n ogólne! ",
      "Jeśli porównujesz 3 grupy i masz n = 30, to tylko 10 na grupę - wciąż za mało.",
      tags$br(),
      "Minimum 20-30 obserwacji w każdej podgrupie, którą chcesz analizować. ",
      "Regresja z k predyktorami potrzebuje n > 10k + 50."
    ),

    div(class = "chapter-transition",
      p("Zobaczmy teraz zbiór, który radzi sobie lepiej."),
      actionButton("ch2_next", "Dalej: 4. Pingwiny →",
                   class = "lc-btn-primary lc-btn-lg")
    ),

    div(style = "height: 40px;")
  ))))

ch3_server <- function(input, output, session) {

  output$tab2_table <- DT::renderDataTable({
    datatable(round_df(small_data), options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })

  # Slider simulations
  sim_data <- reactive({
    n <- input$tab2_n
    set.seed(42)
    data.frame(
      godziny = rnorm(n, 15, 5),
      oceny = rnorm(n, 3.8, 0.6)
    )
  })

  output$tab2_hist <- renderPlot({
    d <- sim_data()
    ggplot(d, aes(x = oceny)) +
      geom_histogram(bins = max(5L, round(input$tab2_n / 5)), fill = data_primary, color = "white", alpha = 0.8) +
      labs(title = paste0("Histogram (n = ", input$tab2_n, ")"), x = "Średnia ocen", y = "Liczebność") +
      theme_upwr(base_size = 14)
  })

  output$tab2_ci <- renderPlot({
    ns <- seq(5, 200, by = 5)
    ci_widths <- 2 * qt(0.975, ns - 1) * 0.6 / sqrt(ns)  # assuming SD = 0.6
    df_ci <- data.frame(n = ns, ci_width = ci_widths)

    ggplot(df_ci, aes(x = n, y = ci_width)) +
      geom_line(color = data_bad, linewidth = 1.2) +
      geom_point(data = df_ci[df_ci$n == max(ns[ns <= input$tab2_n]), ],
                 color = data_bad, size = 4) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = data_good) +
      annotate("text", x = 150, y = 0.55, label = "Akceptowalna szerokość", color = data_good, size = 4) +
      labs(title = "Szerokość 95% CI", x = "Liczba obserwacji (n)", y = "Szerokość CI") +
      theme_upwr(base_size = 14)
  })

  output$tab2_power <- renderPlot({
    ns <- seq(5, 200, by = 5)
    # Power simulation: detect effect size d=0.5
    powers <- sapply(ns, function(n) {
      set.seed(123)
      rejections <- replicate(500, {
        x <- rnorm(n / 2, 0, 1)
        y <- rnorm(n / 2, 0.5, 1)  # effect size d = 0.5
        t.test(x, y)$p.value < 0.05
      })
      mean(rejections)
    })
    df_pow <- data.frame(n = ns, power = powers)

    ggplot(df_pow, aes(x = n, y = power)) +
      geom_line(color = data_primary, linewidth = 1.2) +
      geom_point(data = df_pow[df_pow$n == max(ns[ns <= input$tab2_n]), ],
                 color = data_primary, size = 4) +
      geom_hline(yintercept = 0.8, linetype = "dashed", color = data_good) +
      annotate("text", x = 150, y = 0.83, label = "Moc = 80% (standard)", color = data_good, size = 4) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(title = "Moc testu (effect size d = 0.5)", x = "Liczba obserwacji (n)", y = "Moc testu") +
      theme_upwr(base_size = 14)
  })

}
