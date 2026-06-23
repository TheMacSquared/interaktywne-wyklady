# ============================================================================
# CHAPTER 4: Sezonowość — wzorce cykliczne
# ============================================================================

ch4_ui <- list(
  id    = "ch-sezonowosc",
  num   = "04",
  title = "Sezonowość: wzorce cykliczne",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 04 · Szeregi czasowe",
      num    = "04",
      title  = "Sezonowość.",
      lead   = "Powtarzający się wzorzec o stałym okresie. Jak go zobaczyć,
                zmierzyć i odróżnić od cyklu?"
    ),

    lc_h2("ch4-co-to-sezonowosc", "Sezonowość vs. cykl"),

    tagList(
      lc_p("Sezonowość i cykl to dwie różne rzeczy — często mylone:"),
      tags$ul(
        tags$li(tags$strong("Sezonowość"),
          " — wzorzec o stałym, z góry określonym okresie (rok, tydzień, dzień).
          Każdy styczeń jest chłodniejszy niż lipiec — zawsze. Każda niedziela ma
          niższą sprzedaż niż sobota — zawsze."),
        tags$li(tags$strong("Cykl"),
          " — wzorzec o zmiennym, nieznanym z góry okresie.
          Boom i recesja w gospodarce mogą trwać różnie długo.
          Cykle nie są sezonowością — nie mają stałego rytmu.")
      ),
      margin_callout(label = "Zapamiętaj", color = "uwaga",
        "Sezonowość: stały okres → można ją szacować i odejmować.
         Cykl: zmienny okres → trudniej modelować."
      )
    ),

    lc_h2("ch4-seasonal-subseries", "Seasonal subseries plot"),

    tagList(
      lc_p("Najpotężniejszy wykres do wizualizacji sezonowości to ",
        tags$strong("seasonal subseries plot"), " — każdy miesiąc rysowany osobno.
        Średnia każdego miesiąca to niebieska linia pozioma wewnątrz kolumny.
        Jeśli wszystkie kolumny są identyczne — brak sezonowości.
        Jeśli różnią się systematycznie — mamy wyraźny wzorzec.")
    ),

    figure_panel(
      label = "Ryc. 4.1", title = "Seasonal subseries — wybierz szereg",
      full_width = TRUE,
      fluidRow(
        column(3,
          selectInput("ch4_ss_data", "Szereg:",
                      choices = .ts_dataset_choices[c("warszawa", "bezrobocie", "noclegi", "sprzedaz")],
                      selected = "noclegi"),
          uiOutput("ch4_ss_summary")
        ),
        column(9,
          zoom_plot_ui("ch4_ss_plot", height = "300px")
        )
      )
    ),

    lc_h2("ch4-heatmap", "Heatmapa: rok × miesiąc"),

    tagList(
      lc_p("Heatmapa rok × miesiąc to drugi sposób na wizualizację sezonowości.
        Kolory pokazują wartość dla każdej komórki (rok, miesiąc) jednocześnie —
        pozwala zobaczyć zarówno wzorzec sezonowy (w poziomie), jak i trend (w pionie).")
    ),

    figure_panel(
      label = "Ryc. 4.2", title = "Heatmapa rok × miesiąc",
      full_width = TRUE,
      fluidRow(
        column(3,
          selectInput("ch4_hm_data", "Szereg:",
                      choices = .ts_dataset_choices[c("warszawa", "bezrobocie", "noclegi", "sprzedaz")],
                      selected = "noclegi")
        ),
        column(9,
          zoom_plot_ui("ch4_hm_plot", height = "340px")
        )
      )
    ),

    lc_h2("ch4-miara", "Mierzenie siły sezonowości"),

    tagList(
      lc_p("Jak mocna jest sezonowość w szeregu? Prosty wskaźnik to stosunek odchylenia
        standardowego składowej sezonowej do odchylenia standardowego oryginału po usunięciu trendu.
        Wartości bliskie 1 oznaczają dominującą sezonowość; bliskie 0 — brak wzorca.")
    ),

    figure_panel(
      label = "Ryc. 4.3", title = "Siła sezonowości dla czterech zbiorów danych",
      full_width = TRUE,
      zoom_plot_ui("ch4_strength_plot", height = "220px")
    ),

    lc_h2("ch4-add-vs-mult", "Addytywna czy multiplikatywna?"),

    tagList(
      lc_p("Kluczowy wybór przy modelowaniu: czy amplituda wahań sezonowych ",
        tags$em("rośnie"), " razem z poziomem szeregu?"),
      tags$ul(
        tags$li(tags$strong("Addytywna:"),
          " amplituda stała — np. temperatura zawsze waha się w tym samym zakresie."),
        tags$li(tags$strong("Multiplikatywna:"),
          " amplituda proporcjonalna do poziomu — np. sprzedaż: szczyt grudniowy
          jest coraz wyższy w wartościach bezwzględnych, bo cały rynek rośnie.")
      ),
      lc_p("Praktyczny test: jeśli na wykresie amplituda wahań rośnie — stosuj model multiplikatywny
        lub transformację logarytmiczną (log linearyzuje ten efekt).")
    ),

    lc_chapter_next(
      num       = "05",
      title     = "ACF: pamięć szeregu",
      lead      = "funkcja autokorelacji — lag plot, interpretacja, wzorce",
      target_id = "ch-acf"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  ch4_get_ts_df <- function(key) {
    df   <- .ts_datasets[[key]]$get_df()
    df$month <- as.integer(format(df$date, "%m"))
    df$year  <- as.integer(format(df$date, "%Y"))
    df$value <- df[[2]]
    df
  }

  output$ch4_ss_summary <- renderUI({
    key <- input$ch4_ss_data
    df  <- ch4_get_ts_df(key)
    monthly_means <- tapply(df$value, df$month, mean, na.rm = TRUE)
    best_m  <- which.max(monthly_means)
    worst_m <- which.min(monthly_means)
    month_pl <- c("sty", "lut", "mar", "kwi", "maj", "cze",
                  "lip", "sie", "wrz", "paź", "lis", "gru")
    lc_stat_grid(
      lc_stat_box("Max (miesiąc)",  month_pl[best_m],  color = unname(upwr_cat["szalwia"])),
      lc_stat_box("Min (miesiąc)",  month_pl[worst_m], color = unname(upwr_cat["terakota"])),
      columns = 2
    )
  })

  zoom_plot_server("ch4_ss_plot", reactive({
    key  <- input$ch4_ss_data
    info <- .ts_datasets[[key]]
    df   <- ch4_get_ts_df(key)

    month_pl <- c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze",
                  "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")
    df$month_label <- factor(month_pl[df$month], levels = month_pl)

    monthly_means <- df |>
      dplyr::group_by(month_label) |>
      dplyr::summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")

    ggplot(df, aes(x = year, y = value)) +
      geom_line(color = upwr_secondary, linewidth = 0.7, alpha = 0.7) +
      geom_hline(data = monthly_means, aes(yintercept = mean_val),
                 color = unname(upwr_cat["niebo"]), linewidth = 1.1) +
      facet_wrap(~ month_label, nrow = 2, scales = "free_x") +
      labs(x = NULL, y = info$unit) +
      theme_upwr() +
      theme(strip.text = element_text(size = 9),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  }))

  zoom_plot_server("ch4_hm_plot", reactive({
    key  <- input$ch4_hm_data
    info <- .ts_datasets[[key]]
    df   <- ch4_get_ts_df(key)

    month_pl <- c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze",
                  "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")
    df$month_label <- factor(month_pl[df$month], levels = rev(month_pl))

    ggplot(df, aes(x = year, y = month_label, fill = value)) +
      geom_tile(color = "white", linewidth = 0.3) +
      scale_fill_gradient2(
        low      = upwr_seq_burgundy[2],
        mid      = upwr_panel,
        high     = unname(upwr_cat["niebo"]),
        midpoint = median(df$value, na.rm = TRUE),
        name     = info$unit
      ) +
      labs(x = NULL, y = NULL, title = info$label) +
      theme_upwr() +
      theme(axis.text.x = element_text(size = 8),
            legend.position = "right")
  }))

  zoom_plot_server("ch4_strength_plot", reactive({
    keys <- c("warszawa", "bezrobocie", "noclegi", "sprzedaz")
    strength <- vapply(keys, function(k) {
      ts_obj  <- .ts_datasets[[k]]$get_ts()
      stl_res <- tryCatch(stl(ts_obj, s.window = "periodic", robust = TRUE),
                          error = function(e) NULL)
      if (is.null(stl_res)) return(NA_real_)
      seasonal <- stl_res$time.series[, "seasonal"]
      remainder <- stl_res$time.series[, "remainder"]
      s_var <- var(seasonal, na.rm = TRUE)
      r_var <- var(remainder, na.rm = TRUE)
      max(0, 1 - r_var / (s_var + r_var))
    }, numeric(1))

    labels <- vapply(keys, function(k) .ts_datasets[[k]]$label, character(1))

    df_str <- data.frame(
      label    = labels,
      strength = strength
    )
    df_str$label <- factor(df_str$label, levels = df_str$label[order(df_str$strength)])

    ggplot(df_str, aes(x = strength, y = label)) +
      geom_col(fill = upwr_accent, width = 0.55) +
      geom_text(aes(label = paste0(round(strength * 100, 0), "%")),
                hjust = -0.1, color = upwr_ink, size = 3.5) +
      scale_x_continuous(limits = c(0, 1.1), labels = scales::percent) +
      labs(x = "Siła sezonowości", y = NULL,
           title = "Im bliżej 100%, tym silniejsza sezonowość") +
      theme_upwr()
  }))
}
