ch5_ui <- lecture_chapter(id = "ch5", num = "5", title = "Pierwsze sprawdzenia", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 05 · Proste testy",
      num = "05",
      title = "Pierwsze sprawdzenia w danych.",
      lead = "Test statystyczny nie jest finałem. Jest sposobem na sprawdzenie,
              czy pierwszy trop ma kontakt z danymi."
    ),

    lc_h2("sec-01", "Wybieramy pytanie, potem narzędzie"),

    div(class = "lc-figure-panel",
      h4("Wybierz pytanie badawcze"),
      radioButtons("ch5_question", NULL,
        choices = c(
          "Czy prowadzący oceniani jako atrakcyjniejsi mają wyższe oceny kursu?" = "beauty_eval",
          "Czy kobiety i mężczyźni prowadzący są oceniani podobnie?" = "gender_eval",
          "Czy status native speaker wiąże się z oceną kursu?" = "native_eval",
          "Czy prowadzący z grup mniejszościowych są oceniani inaczej?" = "minority_eval",
          "Czy poziom kursu zmienia oceny studenckie?" = "division_eval",
          "Czy kursy jednopunktowe są oceniane inaczej niż większe kursy?" = "credits_eval",
          "Czy oceny różnią się między grupami wieku prowadzących?" = "age_group_eval"
        ),
        selected = "beauty_eval"
      ),
      uiOutput("ch5_analysis_plan")
    ),

    lc_h2("sec-02", "Zanim zinterpretujesz wynik: sprawdź zmienne zakłócające"),

    div(class = "lc-prose",
      p("Kiedy mamy już pytanie i pierwsze narzędzie, warto zapytać: co może
        mieszać w interpretacji? Zmienna Z jest kandydatem na zmienną zakłócającą,
        jeśli wiąże się jednocześnie z X i Y."),
      p("Tu pokazujemy to na głównym tropie `beauty` → `eval`: sprawdzamy, czy
        wybrana zmienna ma zauważalny związek z atrakcyjnością i oceną kursu.")
    ),

    div(class = "lc-figure-panel",
      h4("Kandydat na zmienną zakłócającą"),
      selectInput("ch5_confounder", "Sprawdź zmienną:",
        choices = setNames(tr_confounder_vars, tr_labels[tr_confounder_vars]),
        selected = "gender"
      ),
      div(class = "two-plot-grid",
        zoom_plot_ui("ch5_confounder_beauty", height = "300px"),
        zoom_plot_ui("ch5_confounder_eval", height = "300px")
      ),
      uiOutput("ch5_confounder_feedback")
    ),

    div(class = "lc-figure-panel",
      h4("Dane i wynik"),
      fluidRow(
        column(7, zoom_plot_ui("ch5_plot", height = "360px")),
        column(5, uiOutput("ch5_result"))
      )
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Jak czytamy wynik?"),
      p("Nie pytamy tylko: 'czy p < 0,05?'. Pytamy: czy wynik wzmacnia trop,
        osłabia go, czy każe nam zmienić pytanie?")
    ),

    lc_h2("sec-03", "Co ten wynik mówi nam dalej?"),

    lc_feedback(
      tags$p(tags$strong("Wynik sugeruje dwa nowe pytania:")),
      tags$ol(
        tags$li("Czy efekt beauty utrzymuje się, gdy kontrolujemy inne czynniki, np. wiek, płeć i typ kursu?"),
        tags$li("Czy response rate wpływa na wynik, czyli czy słyszymy tylko część studentów?")
      ),
      tags$p("Te pytania prowadzą do kolejnego etapu: lepszego projektu badania albo modelu z kontrolami."),
      type = "info"
    ),

    lc_chapter_next("06", "Wynik nie kończy badania",
      "Po wyniku robimy to, co robi badacz: dopisujemy kolejne hipotezy i brakujące dane.",
      "ch6"),
    div(style = "height: 40px;")
  )))
)

ch5_server <- function(input, output, session) {
  .confounder_plot <- function(var, y_var, y_label) {
    x <- tr_data[[var]]
    if (is.numeric(x)) {
      ggplot(tr_data, aes(x = .data[[var]], y = .data[[y_var]])) +
        geom_point(color = proj_col_ref, alpha = 0.45, size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = proj_col_ctrl,
                    fill = proj_col_ctrl, alpha = 0.12) +
        labs(x = unname(tr_labels[var]), y = y_label) +
        theme_upwr()
    } else {
      ggplot(tr_data, aes(x = .data[[var]], y = .data[[y_var]], fill = .data[[var]])) +
        geom_boxplot(alpha = 0.65, outlier.alpha = 0.25) +
        geom_jitter(width = 0.12, alpha = 0.16, size = 1) +
        scale_fill_manual(values = rep(c(proj_col_data, proj_col_hyp, proj_col_ctrl),
                                       length.out = length(unique(x)))) +
        labs(x = unname(tr_labels[var]), y = y_label) +
        theme_upwr() +
        theme(legend.position = "none")
    }
  }

  zoom_plot_server("ch5_confounder_beauty", reactive({
    .confounder_plot(input$ch5_confounder, "beauty", "Ocena atrakcyjności (beauty)")
  }))

  zoom_plot_server("ch5_confounder_eval", reactive({
    .confounder_plot(input$ch5_confounder, "eval", "Ocena kursu (eval)")
  }))

  output$ch5_confounder_feedback <- renderUI({
    row <- tr_confounder_row(input$ch5_confounder)
    if (row$confounder) {
      type <- "warning"
      msg <- "Kandydat na zmienną zakłócającą — warto kontrolować w kolejnym kroku."
    } else if (row$beauty_linked && !row$eval_linked) {
      type <- "info"
      msg <- "Koreluje tylko z X (`beauty`) — nie jest zmienną zakłócającą dla głównej relacji w tym prostym sensie."
    } else if (!row$beauty_linked && row$eval_linked) {
      type <- "info"
      msg <- "Koreluje tylko z Y (`eval`) — może być ważna dla oceny, ale nie tłumaczy związku beauty-eval."
    } else {
      type <- "ok"
      msg <- "Nie wygląda na zmienną zakłócającą dla głównej relacji według tej roboczej reguły."
    }
    lc_feedback(
      tags$p(tags$strong(row$label)),
      tags$p("Związek z beauty: ", tags$code(row$beauty_label),
             "; związek z eval: ", tags$code(row$eval_label), "."),
      tags$p(HTML(gsub("`([^`]+)`", "<code>\\1</code>", msg))),
      type = type
    )
  })

  selected_analysis <- reactive({
    switch(input$ch5_question,
      "beauty_eval" = list(
        kind = "cor", x = "beauty", method = "cor",
        test = "korelacja Pearsona",
        analysis = "wykres punktowy + korelacja",
        note = "Obie zmienne są ilościowe, więc zaczynamy od związku liniowego."
      ),
      "gender_eval" = list(
        kind = "two", x = "gender", method = "t",
        test = "test t dla dwóch grup",
        analysis = "boxplot + porównanie średnich",
        note = "Pytanie porównuje dwie grupy prowadzących."
      ),
      "native_eval" = list(
        kind = "two", x = "native", method = "wilcox",
        test = "Mann-Whitney",
        analysis = "boxplot + porównanie rozkładów",
        note = "Używamy wariantu odpornego na nierówne i skośne grupy."
      ),
      "minority_eval" = list(
        kind = "two", x = "minority", method = "wilcox",
        test = "Mann-Whitney",
        analysis = "boxplot + porównanie rozkładów",
        note = "To pytanie dotyczy sprawiedliwości ocen, więc interpretujemy wynik szczególnie ostrożnie."
      ),
      "division_eval" = list(
        kind = "two", x = "division", method = "t",
        test = "test t dla dwóch grup",
        analysis = "boxplot + porównanie średnich",
        note = "W tych danych poziom kursu ma dwie kategorie: niższy i wyższy."
      ),
      "credits_eval" = list(
        kind = "two", x = "credits", method = "wilcox",
        test = "Mann-Whitney",
        analysis = "boxplot + porównanie rozkładów",
        note = "Porównujemy kursy jednopunktowe z kursami o większej liczbie punktów."
      ),
      "age_group_eval" = list(
        kind = "multi", x = "age_group", method = "anova",
        test = "ANOVA jednoczynnikowa",
        analysis = "boxplot + porównanie średnich w trzech grupach wieku",
        note = "Tu mamy trzy uporządkowane grupy: do 40, 41-55 i powyżej 55 lat."
      )
    )
  })

  output$ch5_analysis_plan <- renderUI({
    a <- selected_analysis()
    lc_feedback(
      tags$p(tags$strong("Proponowana analiza: "), a$analysis),
      tags$p(tags$strong("Test: "), a$test),
      tags$p(a$note),
      type = "info"
    )
  })

  zoom_plot_server("ch5_plot", reactive({
    a <- selected_analysis()
    if (a$kind == "cor") {
      ggplot(tr_data, aes(x = beauty, y = eval)) +
        geom_point(color = proj_col_ref, alpha = 0.45, size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = proj_col_ctrl,
                    fill = proj_col_ctrl, alpha = 0.12) +
        labs(x = "Ocena atrakcyjności (beauty)", y = "Ocena kursu (eval)") +
        theme_upwr()
    } else {
      var <- a$x
      ggplot(tr_data, aes(x = .data[[var]], y = eval, fill = .data[[var]])) +
        geom_boxplot(alpha = 0.65, outlier.alpha = 0.25) +
        geom_jitter(width = 0.12, alpha = 0.16, size = 1) +
        scale_fill_manual(values = rep(c(proj_col_data, proj_col_hyp, proj_col_ctrl),
                                       length.out = length(unique(tr_data[[var]])))) +
        labs(x = unname(tr_labels[var]), y = "Ocena kursu (eval)") +
        theme_upwr() +
        theme(legend.position = "none")
    }
  }))

  output$ch5_result <- renderUI({
    a <- selected_analysis()
    if (a$kind == "cor") {
      res <- cor.test(tr_data$beauty, tr_data$eval)
      effect <- paste0("r = ", round(unname(res$estimate), 3))
      tagList(
        lc_stat_grid(
          lc_stat_box("Pytanie", "związek dwóch zmiennych", color = proj_col_data),
          lc_stat_box("r", round(unname(res$estimate), 3), color = proj_col_ctrl),
          lc_stat_box("p", tr_fmt_p(res$p.value), color = proj_col_warn),
          columns = 1
        ),
        div(class = "lc-feedback lc-feedback-info",
          tags$strong("Interpretacja badawcza:"),
          p(tr_research_verdict(res$p.value, effect)),
          p("Kolejne pytanie: czy ten związek wygląda podobnie w różnych grupach?")
        )
      )
    } else if (a$kind == "two") {
      var <- a$x
      res <- tr_group_test(var, a$method)
      diff <- tr_mean_diff(var)
      effect <- paste0("różnica średnich = ", round(diff$diff, 3),
                       " (", diff$group2, " minus ", diff$group1, ")")
      tagList(
        lc_stat_grid(
          lc_stat_box("Test", res$name, color = proj_col_data),
          lc_stat_box("Śr. 1", paste0(diff$group1, ": ", round(diff$mean1, 2)), color = proj_col_ref),
          lc_stat_box("Śr. 2", paste0(diff$group2, ": ", round(diff$mean2, 2)), color = proj_col_ctrl),
          lc_stat_box("p", tr_fmt_p(res$p), color = proj_col_warn),
          columns = 1
        ),
        div(class = "lc-feedback lc-feedback-info",
          tags$strong("Interpretacja badawcza:"),
          p(tr_research_verdict(res$p, effect)),
          p("Kolejne pytanie: czy grupy prowadzą podobne kursy?")
        )
      )
    } else {
      var <- a$x
      res <- tr_multi_group_test(var, a$method)
      means <- aggregate(eval ~ tr_data[[var]], data = tr_data, mean)
      names(means) <- c("group", "mean")
      tagList(
        lc_stat_grid(
          lc_stat_box("Test", res$name, color = proj_col_data),
          lc_stat_box("p", tr_fmt_p(res$p), color = proj_col_warn),
          columns = 1
        ),
        tags$table(class = "lc-table lc-table-bordered",
          tags$tbody(lapply(seq_len(nrow(means)), function(i) {
            tags$tr(tags$td(as.character(means$group[i])), tags$td(round(means$mean[i], 2)))
          }))
        ),
        div(class = "lc-feedback lc-feedback-info",
          tags$strong("Interpretacja badawcza:"),
          p(tr_research_verdict(res$p, "co najmniej jedna grupa wieku wygląda inaczej")),
          p("Kolejne pytanie: czy to efekt wieku, doświadczenia, typu kursów, czy oczekiwań studentów?")
        )
      )
    }
  })
}
