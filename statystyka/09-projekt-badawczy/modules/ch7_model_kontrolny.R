ch7_ui <- lecture_chapter(id = "ch7", num = "7", title = "Model kontrolny", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 07 · Stabilność efektu",
      num = "07",
      title = "Cała wiązka w jednym modelu.",
      lead = "W rozdziale 5 sprawdzaliśmy tropy pojedynczo. Teraz sprawdzamy je
              jednocześnie: czy efekt utrzymuje się, gdy kontrolujemy pozostałe?"
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Przypomnienie celu:"),
      p(tags$em(tr_goal))
    ),

    lc_h2("sec-01", "Po co model kontrolny?"),

    div(class = "lc-prose",
      p("W rozdziale 5 sprawdzaliśmy tropy pojedynczo: ", gloss("korelacja"), ", różnice między
        dwiema grupami, proste porównania. To dobry start, ale świat rzadko
        zmienia się jedną zmienną naraz — a tablica ", gloss("zmienna zakłócająca", "zakłócaczy"), " pokazała, że tropy
        się przeplatają."),
      p(gloss("regresja wieloraka", "Regresja wieloczynnikowa"), " pozwala zapytać wprost: czy trop związany z
        `beauty` pozostaje widoczny, gdy jednocześnie uwzględnimy wiek, płeć,
        native speaker status, poziom kursu i response rate?")
    ),

    div(class = "lc-figure-panel",
      h4("Seria modeli kontrolnych"),
      div(class = "lc-prose",
        p("Dodajemy ", gloss("zmienna kontrolna", "kontrole"), " warstwami i patrzymy, co dzieje się ze ", gloss("współczynnik regresji", "współczynnikiem"), "
          przy `beauty`: czy słabnie, czy się trzyma.")
      ),
      uiOutput("ch7_models_table"),
      zoom_plot_ui("ch7_beta_plot", height = "280px")
    ),

    div(class = "lc-figure-panel",
      h4("Własny model kontrolny"),
      div(class = "control-model-layout",
        div(class = "control-model-sidebar",
          checkboxGroupInput("ch7_vars", "Dodaj kontrole:",
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
          uiOutput("ch7_custom_metrics")
        ),
        div(class = "control-model-results",
          uiOutput("ch7_custom_coefs"),
          zoom_plot_ui("ch7_custom_coef_plot", height = "260px")
        )
      )
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Rola modelu:"),
      p("Model nie zastępuje sformułowania pytania. Wymaga wcześniejszych ", gloss("hipoteza badawcza", "hipotez"), ",
        alternatywnych wyjaśnień i poprawnego pomiaru — dopiero wtedy jego wynik
        da się sensownie zinterpretować.")
    ),

    lc_h2("sec-02", "Co model mówi nam dalej?"),

    lc_feedback(
      tags$p(tags$strong("Jeśli efekt beauty przeżywa kontrolę, rodzi to kolejne pytanie:")),
      tags$p("Czy to ", gloss("przyczynowość"), "? Czy atrakcyjność powoduje wyższe oceny, czy tylko z nimi współwystępuje?"),
      tags$p(gloss("dane obserwacyjne", "Dane obserwacyjne"), " nie odpowiedzą na to pytanie — pokazują współwystępowanie, nie przyczynę. To granica, której ten zbiór nie przekroczy, i trzeba ją uczciwie zapisać we wniosku."),
      type = "warning"
    ),

    lc_chapter_next("08", "Od konspektu do wniosku",
      "Wracamy do konspektu i dopisujemy, co wyniki zmieniły w interpretacji celu.",
      "ch8"),

    div(style = "height: 40px;")
  )))
)

ch7_server <- function(input, output, session) {
  # Seria modeli liczona od razu — to materiał do omówienia z projekcji,
  # nie nagroda za kliknięcie.
  model_series <- reactive({
    models <- list(
      list(label = "1: beauty", model = lm(eval ~ beauty, data = tr_data)),
      list(label = "2: + cechy osoby", model = lm(eval ~ beauty + gender + age + minority + native + tenure, data = tr_data)),
      list(label = "3: + kontekst kursu", model = lm(eval ~ beauty + gender + age + minority + native + tenure + division + credits + students, data = tr_data)),
      list(label = "4: + response rate", model = lm(eval ~ beauty + gender + age + minority + native + tenure + division + credits + students + response.rate, data = tr_data))
    )
    tr_model_table(models)
  })

  output$ch7_models_table <- renderUI({
    df <- model_series()
    df$p_label <- lc_pval(df$p_beauty)
    lc_table_split(df,
      cols = list(
        lc_col("model", "Model", "row"),
        lc_col("beta_beauty", "β beauty", digits = 3),
        lc_col("p_label", "p"),
        lc_col("adj_r2", "adj.R²", digits = 3),
        lc_col("aic", "AIC")
      ),
      groups = list(c("beta_beauty", "p_label"), c("adj_r2", "aic")),
      label = "Seria modeli kontrolnych"
    )
  })

  zoom_plot_server("ch7_beta_plot", reactive({
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
    vars <- input$ch7_vars
    rhs <- paste(c("beauty", vars), collapse = " + ")
    lm(as.formula(paste("eval ~", rhs)), data = tr_data)
  })

  output$ch7_custom_metrics <- renderUI({
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
    ) |>
      tagAppendAttributes(class = "control-model-metrics")
  })

  output$ch7_custom_coefs <- renderUI({
    coefs <- broom::tidy(custom_model())
    coefs <- coefs[coefs$term != "(Intercept)", ]
    significant <- !is.na(coefs$p.value) & coefs$p.value < 0.05
    lc_table(
      data.frame(
        term = tr_label_term(coefs$term),
        estimate = coefs$estimate,
        se = coefs$std.error,
        p = lc_pval(coefs$p.value),
        stringsAsFactors = FALSE
      ),
      cols = list(
        lc_col("term", "Predyktor", "row"),
        lc_col("estimate", "β", digits = 3),
        lc_col("se", "SE", digits = 3),
        lc_col("p", "p")
      ),
      # Istotne predyktory (p < 0,05) wyróżnione jak dotąd całym wierszem.
      row_class = lapply(significant, function(s) if (s) "is-best")
    )
  })

  zoom_plot_server("ch7_custom_coef_plot", reactive({
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
