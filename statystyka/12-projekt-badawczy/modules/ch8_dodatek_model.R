ch8_ui <- lecture_chapter(id = "ch8", num = "9", title = "Dodatek: model kontrolny", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 09 · Dodatek na przyszłość",
      num = "09",
      title = "Model kontrolny na później.",
      lead = "To nie jest część głównego flow dzisiejszych zajęć. To zapowiedź:
              kiedy poznamy regresję wieloczynnikową, wrócimy do tych samych pytań
              z narzędziem do jednoczesnego uwzględniania kilku tropów."
    ),

    lc_h2("sec-01", "Po co model kontrolny?"),

    div(class = "lc-prose",
      p("W głównej części wykładu sprawdzaliśmy tropy pojedynczo: korelacja,
        różnice między dwiema grupami, proste porównania. To dobry start
        badawczy, ale świat rzadko zmienia się jedną zmienną naraz."),
      p("Regresja wieloczynnikowa pozwala zapytać: czy trop związany z `beauty`
        pozostaje widoczny, gdy jednocześnie uwzględnimy np. wiek, płeć,
        native speaker status, poziom kursu i response rate?")
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Na dziś:"),
      p("Nie traktujemy tego rozdziału jako wymaganej metody. To mapa miejsca,
        do którego dojdziemy później.")
    ),

    div(class = "lc-figure-panel",
      h4("Seria modeli kontrolnych"),
      actionButton("ch8_build", "Pokaż serię modeli", class = "lc-btn-primary"),
      br(), br(),
      uiOutput("ch8_models_table"),
      zoom_plot_ui("ch8_beta_plot", height = "280px")
    ),

    div(class = "lc-figure-panel",
      h4("Własny model kontrolny"),
      fluidRow(
        column(4,
          checkboxGroupInput("ch8_vars", "Dodaj kontrole:",
            choices = c(
              "Płeć" = "gender",
              "Wiek" = "age",
              "Mniejszość" = "minority",
              "Native speaker" = "native",
              "Tenure track" = "tenure",
              "Poziom kursu" = "division",
              "Credits" = "credits",
              "Liczba odpowiedzi" = "students",
              "Response rate" = "response.rate"
            ),
            selected = c("gender", "age", "native", "division", "credits", "response.rate")
          ),
          uiOutput("ch8_custom_metrics")
        ),
        column(8,
          uiOutput("ch8_custom_coefs"),
          zoom_plot_ui("ch8_custom_coef_plot", height = "260px")
        )
      )
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Łącznik z dzisiejszym warsztatem:"),
      p("Model nie wymyśla pytania za nas. Najpierw potrzebujemy hipotez,
        mapy alternatywnych wyjaśnień i sensownego pomiaru. Dopiero potem
        model ma co robić.")
    ),

    lc_h2("sec-02", "Co model mówi nam dalej?"),

    lc_feedback(
      tags$p(tags$strong("Jeśli efekt beauty przeżywa kontrolę, rodzi to kolejne pytanie:")),
      tags$p("Czy to przyczynowość? Czy atrakcyjność powoduje wyższe oceny, czy tylko z nimi współwystępuje?"),
      tags$p("Dane obserwacyjne nie mogą same odpowiedzieć na to pytanie. Żeby odpowiedzieć mocniej, potrzebujemy innego projektu badania."),
      type = "warning"
    ),

    div(style = "height: 40px;")
  )))
)

ch8_server <- function(input, output, session) {
  model_series <- reactiveVal(NULL)

  observeEvent(input$ch8_build, {
    models <- list(
      list(label = "1: beauty", model = lm(eval ~ beauty, data = tr_data)),
      list(label = "2: + cechy osoby", model = lm(eval ~ beauty + gender + age + minority + native + tenure, data = tr_data)),
      list(label = "3: + kontekst kursu", model = lm(eval ~ beauty + gender + age + minority + native + tenure + division + credits + students, data = tr_data)),
      list(label = "4: + response rate", model = lm(eval ~ beauty + gender + age + minority + native + tenure + division + credits + students + response.rate, data = tr_data))
    )
    model_series(tr_model_table(models))
  }, ignoreInit = FALSE)

  output$ch8_models_table <- renderUI({
    df <- model_series()
    rows <- lapply(seq_len(nrow(df)), function(i) {
      tags$tr(
        tags$td(df$model[i]),
        tags$td(round(df$beta_beauty[i], 3)),
        tags$td(tr_fmt_p(df$p_beauty[i])),
        tags$td(round(df$adj_r2[i], 3)),
        tags$td(round(df$aic[i], 0))
      )
    })
    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      tags$thead(tags$tr(
        tags$th("Model"), tags$th("β beauty"), tags$th("p"),
        tags$th("adj.R²"), tags$th("AIC")
      )),
      tags$tbody(rows)
    )
  })

  zoom_plot_server("ch8_beta_plot", reactive({
    df <- model_series()
    df$model <- factor(df$model, levels = df$model)
    ggplot(df, aes(x = model, y = beta_beauty, fill = p_beauty < 0.05)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = 0, color = proj_col_ref) +
      scale_fill_manual(values = c("TRUE" = proj_col_ctrl, "FALSE" = proj_col_ref),
                        guide = "none") +
      labs(x = NULL, y = "Współczynnik przy beauty") +
      theme_upwr() +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  }))

  custom_model <- reactive({
    vars <- input$ch8_vars
    rhs <- paste(c("beauty", vars), collapse = " + ")
    lm(as.formula(paste("eval ~", rhs)), data = tr_data)
  })

  output$ch8_custom_metrics <- renderUI({
    m <- custom_model()
    g <- broom::glance(m)
    coefs <- broom::tidy(m)
    p_beauty <- coefs$p.value[coefs$term == "beauty"]
    lc_stat_grid(
      lc_stat_box("adj.R²", round(g$adj.r.squared, 3), color = proj_col_ctrl),
      lc_stat_box("AIC", round(AIC(m), 0), color = proj_col_warn),
      lc_stat_box("β beauty", round(coefs$estimate[coefs$term == "beauty"], 3),
                  color = proj_col_hyp),
      lc_stat_box("p beauty", tr_fmt_p(p_beauty), color = proj_col_data),
      columns = 2
    )
  })

  output$ch8_custom_coefs <- renderUI({
    coefs <- broom::tidy(custom_model())
    coefs <- coefs[coefs$term != "(Intercept)", ]
    rows <- lapply(seq_len(nrow(coefs)), function(i) {
      tags$tr(
        tags$td(tr_label_term(coefs$term[i])),
        tags$td(round(coefs$estimate[i], 3)),
        tags$td(round(coefs$std.error[i], 3)),
        tags$td(tr_fmt_p(coefs$p.value[i]))
      )
    })
    tags$table(class = "lc-table lc-table-bordered",
      tags$thead(tags$tr(tags$th("Predyktor"), tags$th("β"), tags$th("SE"), tags$th("p"))),
      tags$tbody(rows)
    )
  })

  zoom_plot_server("ch8_custom_coef_plot", reactive({
    coefs <- broom::tidy(custom_model(), conf.int = TRUE)
    coefs <- coefs[coefs$term != "(Intercept)", ]
    coefs$label <- tr_label_term(coefs$term)
    coefs$label <- factor(coefs$label, levels = rev(coefs$label))
    ggplot(coefs, aes(x = estimate, y = label)) +
      geom_vline(xintercept = 0, color = proj_col_ref, linetype = "dashed") +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2,
                     color = proj_col_ref) +
      geom_point(color = proj_col_hyp, size = 2.5) +
      labs(x = "Współczynnik z 95% CI", y = NULL) +
      theme_upwr()
  }))
}
