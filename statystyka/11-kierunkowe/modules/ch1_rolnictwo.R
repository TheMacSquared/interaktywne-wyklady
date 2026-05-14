# ============================================================================
# CHAPTER 1: Rolnictwo
# ============================================================================

ch1_ui <- lecture_chapter(
  id = "ch-rolnictwo",
  num = "01",
  title = "Rolnictwo",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 01 · Materiał kierunkowy",
      num    = "01",
      title  = "ANOVA dwuczynnikowa i bloki.",
      lead   = "Gdy pytanie brzmi nie tylko „czy nawóz działa?”, ale też „czy działa tak samo dla każdej odmiany i na każdym fragmencie pola?”."
    ),

    lc_h2("ch1-interakcja", "Drugi czynnik zmienia pytanie"),
    p("ANOVA jednoczynnikowa porównywała średnie między grupami. W doświadczeniu rolniczym rzadko mamy tylko jedną przyczynę: dawka azotu, odmiana, stanowisko i rok potrafią działać jednocześnie."),
    p("W układzie dwuczynnikowym pytamy o efekt nawożenia, efekt odmiany oraz ", tags$em("interakcję"), ": czy różnica między dawkami N jest taka sama dla wszystkich odmian. Jeśli linie na wykresie interakcji nie są równoległe, interpretacja samych efektów głównych robi się podejrzana."),

    figure_panel(
      label = "Ryc. 1.1", title = "Doświadczenie nawozowe: dawka N × odmiana + bloki",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch1_interaction", "Siła interakcji:", min = 0, max = 10, value = 4, step = 1),
          sliderInput("ch1_block_sd", "Niejednorodność pola (bloki):", min = 0, max = 8, value = 3, step = 0.5),
          uiOutput("ch1_anova_note")
        ),
        column(8,
          zoom_plot_ui("ch1_interaction_plot", height = "340px"),
          uiOutput("ch1_anova_table")
        )
      )
    ),

    lc_h2("ch1-rcbd", "Po co blokować pole?"),
    p("Blok w RCBD nie jest kolejnym zabiegiem agrotechnicznym. To sposób, żeby odjąć z analizy znaną, nieinteresującą zmienność: różnice wilgotności, zasobności gleby albo spadku terenu."),

    figure_panel(
      label = "Ryc. 1.2", title = "Co zabiera z reszt blok?",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Ten sam zestaw danych można opisać modelem z blokiem albo bez bloku. Jeśli blok tłumaczy dużą część zmienności, test dla zabiegów staje się czystszy."),
          checkboxInput("ch1_show_interaction_part", "Pokaż interakcję jako osobny składnik", value = TRUE),
          uiOutput("ch1_block_gain")
        ),
        column(8, zoom_plot_ui("ch1_variance_plot", height = "310px"))
      )
    ),

    lc_feedback(type = "info",
      tags$strong("Praktyczna reguła:"),
      " jeśli interakcja jest silna, nie opowiadaj osobno „najlepsza dawka” i „najlepsza odmiana”. Opowiadaj pary: która dawka dla której odmiany."
    )
  )
)

ch1_server <- function(input, output, session) {
  ch1_data <- reactive({
    generate_agri_rcbd(input$ch1_interaction, input$ch1_block_sd)
  })

  zoom_plot_server("ch1_interaction_plot", reactive({
    d <- ch1_data()
    ggplot(d, aes(nitrogen, yield, color = cultivar, group = cultivar)) +
      stat_summary(fun = mean, geom = "line", linewidth = 1.15) +
      stat_summary(fun = mean, geom = "point", size = 3) +
      geom_point(aes(shape = block), position = position_jitter(width = 0.08), alpha = 0.45) +
      scale_color_manual(values = upwr_cat_n(3)) +
      labs(x = "Dawka azotu (kg N/ha)", y = "Plon (dt/ha)", color = "Odmiana", shape = "Blok")
  }))

  output$ch1_anova_table <- renderUI({
    d <- ch1_data()
    fit <- aov(yield ~ nitrogen * cultivar + block, data = d)
    tab <- broom::tidy(fit) |>
      transmute(
        Składnik = term,
        `df` = df,
        `F` = statistic,
        `p` = p.value
      )
    mini_table(tab, digits = 3)
  })

  output$ch1_anova_note <- renderUI({
    if (input$ch1_interaction >= 6) {
      lc_feedback(type = "warning", "Linie mocno się rozchodzą: efekt dawki zależy od odmiany.")
    } else {
      lc_feedback(type = "ok", "Linie są względnie równoległe: efekty główne dają się czytać spokojniej.")
    }
  })

  zoom_plot_server("ch1_variance_plot", reactive({
    d <- ch1_data()
    fit <- aov(yield ~ nitrogen * cultivar + block, data = d)
    tab <- broom::tidy(fit) |>
      filter(!is.na(sumsq)) |>
      mutate(
        part = case_when(
          term == "nitrogen" ~ "Dawka N",
          term == "cultivar" ~ "Odmiana",
          term == "nitrogen:cultivar" ~ "Interakcja",
          term == "block" ~ "Blok",
          term == "Residuals" ~ "Reszta",
          TRUE ~ term
        )
      )
    if (!isTRUE(input$ch1_show_interaction_part)) {
      tab <- tab |>
        mutate(part = if_else(part == "Interakcja", "Reszta", part)) |>
        group_by(part) |>
        summarise(sumsq = sum(sumsq), .groups = "drop")
    }
    tab <- tab |>
      mutate(share = sumsq / sum(sumsq),
             part = factor(part, levels = c("Dawka N", "Odmiana", "Interakcja", "Blok", "Reszta")))
    ggplot(tab, aes(part, share, fill = part)) +
      geom_col(width = 0.62) +
      geom_text(aes(label = fmt_pct(share, 0)), vjust = -0.35, size = 4) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max(tab$share) * 1.18)) +
      scale_fill_manual(values = upwr_cat_n(length(levels(droplevels(tab$part)))), guide = "none") +
      labs(x = NULL, y = "Udział w sumie kwadratów")
  }))

  output$ch1_block_gain <- renderUI({
    d <- ch1_data()
    no_block <- aov(yield ~ nitrogen * cultivar, data = d)
    with_block <- aov(yield ~ nitrogen * cultivar + block, data = d)
    mse_no <- broom::tidy(no_block) |> filter(term == "Residuals") |> pull(meansq)
    mse_block <- broom::tidy(with_block) |> filter(term == "Residuals") |> pull(meansq)
    gain <- 1 - mse_block / mse_no
    lc_stat_box("Spadek wariancji reszt", fmt_pct(gain, 0),
                caption = "po dodaniu bloku do modelu", color = upwr_secondary)
  })
}
