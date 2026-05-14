# ============================================================================
# CHAPTER 2: Zmienne jakościowe
# ============================================================================

ch2_ui <- list(
  id = "ch-jakosciowe", num = "02", title = "Zmienne jakościowe",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 02 · Statystyka opisowa",
      num    = "02",
      title  = "Zmienne jakościowe.",
      lead   = "Zmienne jakościowe opisują cechy, nie liczby. Podstawowym narzędziem
                ich opisu jest tabela częstości — zobaczmy krok po kroku, jak ją
                zbudować na przykładzie zmiennej „kierunek studiów”."
    ),

    # ========================================================================
    # WIDGET 1: Frequency table step-by-step
    # ========================================================================
    figure_panel(
      label = "Ryc. 2.1",
      title = "Tabela częstości — krok po kroku",
      radioButtons("ch2_freq_var", "Wybierz zmienną:",
        choices = c(
          "Kierunek studiów (nominalna)" = "kierunek",
          "Zadowolenie ze studiów (porządkowa)" = "zadowolenie"
        ),
        selected = "kierunek", inline = TRUE
      ),
      div(class = "step-buttons",
        actionButton("ch2_freq_s1", "1. Surowe dane",
                     class = "lc-btn-outline"),
        actionButton("ch2_freq_s2", "2. Zliczanie",
                     class = "lc-btn-outline"),
        actionButton("ch2_freq_s3", "3. Częstości względne",
                     class = "lc-btn-outline"),
        actionButton("ch2_freq_s4", "4. Skumulowane",
                     class = "lc-btn-outline")
      ),
      actionButton("ch2_freq_reset", "Reset", class = "lc-btn-secondary lc-btn-sm"),
      uiOutput("ch2_freq_explanation"),
      tableOutput("ch2_freq_table")
    ),

    # ========================================================================
    # WIDGET 1b: Nominal vs Ordinal comparison
    # ========================================================================
    lc_h2("ch2-nominalna-vs-porzadkowa", "Nominalna vs porządkowa — czy kolejność ma znaczenie?"),

    tagList(
      p("Zanim przejdziemy do wizualizacji, zatrzymajmy sie na waznym rozróżnieniu.
        Zmienne jakościowe dzielimy na ", tags$b("nominalne"), " (kategorie bez naturalnej
        kolejnośći) i ", tags$b("porzadkowe"), " (kategorie z logiczna kolejnośćia).
        Ta roznica ma praktyczne konsekwencje.")
    ),

    figure_panel(
      label = "Ryc. 2.2",
      title = "Czy kolejność kategorii ma znaczenie?",

      checkboxInput("ch2_ord_shuffle", "Losowa kolejność kategorii", value = FALSE),

      fluidRow(
        column(6,
          h5(style = paste0("text-align: center; color: ", type_colors["nominalna"], ";"), "Nominalna: Kierunek studiów"),
          zoom_plot_ui("ch2_ord_nom_plot", height = "300px")
        ),
        column(6,
          h5(style = paste0("text-align: center; color: ", type_colors["porzadkowa"], ";"), "Porzadkowa: Zadowolenie"),
          zoom_plot_ui("ch2_ord_ord_plot", height = "300px")
        )
      ),

      uiOutput("ch2_ord_explanation")
    ),

    # --- Narrative before Widget 2 ---
    lc_h2("ch2-kolowy-vs-slupkowy", "Wykres kołowy vs słupkowy"),

    tagList(
      p("Jak wizualizować zmienne jakościowe? Porównajmy wykres kołowy ze słupkowym
        w trzech scenariuszach -- od latwego do trudnego. Zobaczysz, dlaczego
        wykres słupkowy jest ", tags$b("zawsze"), " co najmniej tak samo czytelny.")
    ),

    # ========================================================================
    # WIDGET 2: Pie vs Bar -- scenario comparison
    # ========================================================================
    figure_panel(
      label = "Ryc. 2.3",
      title = "Pie vs Bar — trzy scenariusze porównawcze",
      div(style = "display: flex; gap: 8px; margin-bottom: 15px; flex-wrap: wrap;",
        actionButton("ch2_sc1", "1. Duze różnice",
                     class = "lc-btn-outline"),
        actionButton("ch2_sc2", "2. Podobne wartości",
                     class = "lc-btn-outline"),
        actionButton("ch2_sc3", "3. Podobne + zle kolory",
                     class = "lc-btn-outline")
      ),
      fluidRow(
        column(6,
          h5(style = "text-align: center; color: var(--upwr-reference);", "Wykres kołowy"),
          div(style = "position: relative; width: 100%; height: 320px;",
            tags$canvas(id = "ch2_pie_canvas")
          ),
          uiOutput("ch2_scenario_pie_verdict")
        ),
        column(6,
          h5(style = "text-align: center; color: var(--upwr-reference);", "Wykres słupkowy -- te same dane"),
          div(style = "position: relative; width: 100%; height: 320px;",
            tags$canvas(id = "ch2_bar_canvas")
          ),
          uiOutput("ch2_scenario_bar_verdict")
        )
      ),
      div(style = "display: flex; flex-wrap: wrap; gap: 14px; font-size: 12px; color: var(--upwr-reference); margin-top: 8px;",
        id = "ch2_legend",
        uiOutput("ch2_scenario_legend")
      )
    ),

    # --- Narrative before Widget 4 ---
    lc_h2("ch2-kolory", "Manipulacja kolorami"),

    tagList(
      p("Kolory na wykresie mogą manipulowac odbiorem danych. Zobaczmy,
        jak ten sam zestaw danych moze wyglądać zupełnie inaczej w
        zależności od doboru palety kolorow.")
    ),

    # ========================================================================
    # WIDGET 4: Color manipulation demo
    # ========================================================================
    figure_panel(
      label = "Ryc. 2.4",
      title = "Jak kolory zmieniają percepcję danych",
      fluidRow(
        column(4,
          selectInput("ch2_color_palette", "Paleta kolorów:",
            choices = c(
              "Neutralna (szara)" = "neutral",
              "Ciepła (podkreśla Informatykę)" = "warm",
              "Zimna (podkreśla Biologię)" = "cool",
              "Stronnicza" = "biased",
              "--- Klasyczne palety R ---" = "sep1",
              "Viridis" = "viridis",
              "Set2 (ColorBrewer)" = "set2",
              "Okabe-Ito (colorblind-safe)" = "okabe_ito",
              "Tableau 10" = "tableau"
            ),
            selected = "neutral"
          ),
          actionButton("ch2_color_random", "Losowe kolory",
                       class = "lc-btn-secondary-outline", width = "100%")
        ),
        column(8, zoom_plot_ui("ch2_color_plot", height = "380px"))
      ),
      lc_feedback(type = "warning",
        tags$b("Pamiętaj: "),
        "Wybór kolorów nie jest neutralny. Intensywne, cieplejsze barwy
         przyciągają uwagę, a jasne/szare marginalizują kategorie.",
        tags$br(), tags$br(),
        tags$b("Dobre praktyki: "),
        tags$ul(
          tags$li(tags$b("Viridis"), " -- percepcyjnie równomierna (różnice
            wartości = różnice w kolorze), czytelna w skali szarości
            i bezpieczna dla daltonistów. Domyślna w wielu pakietach R."),
          tags$li(tags$b("Okabe-Ito"), " -- paleta zaprojektowana specjalnie
            pod kątem daltoniśtów (ok. 8% mężczyzn). Klasyczny wybór
            w publikacjach naukowych."),
          tags$li(tags$b("ColorBrewer (Set2, Set3, Paired...)"), " -- rodzina palet
            stworzonych przez kartografę Cynthia Brewer. W R dostępne przez ",
            tags$code("scale_fill_brewer()"), "."),
          tags$li(tags$b("Tableau 10"), " -- standard w narzędziach BI,
            zbalansowana jasność i kontrast.")
        )
      )
    ),

    # ========================================================================
    # WIDGET 4b: Cross-tabulation
    # ========================================================================
    lc_h2("ch2-krzyzowa", "Tabela krzyżowa — dwie zmienne jednocześnie"),

    tagList(
      p("Dotychczas analizowalismy po jednej zmiennej. Ale często chcemy
        zbadac ", tags$b("zaleznosc miedzy dwiema zmiennymi jakościowymi"),
        ". Sluzy do tego tabela krzyzowa (kontyngencji).")
    ),

    figure_panel(
      label = "Ryc. 2.5",
      title = "Tabela krzyżowa",
      fluidRow(
        column(4,
          selectInput("ch2_cross_row", "Zmienna w wierszach:",
            choices = c("Płeć" = "plec", "Kierunek" = "kierunek",
                        "Grupa krwi" = "grupa_krwi"),
            selected = "plec"
          )
        ),
        column(4,
          selectInput("ch2_cross_col", "Zmienna w kolumnach:",
            choices = c("Kierunek" = "kierunek", "Płeć" = "plec",
                        "Grupa krwi" = "grupa_krwi"),
            selected = "kierunek"
          )
        ),
        column(4,
          radioButtons("ch2_cross_type", "Pokaz:",
            choices = c("Liczebnośći" = "counts",
                        "% wierszowe" = "row_pct",
                        "% kolumnowe" = "col_pct"),
            selected = "counts", inline = TRUE
          )
        )
      ),
      tableOutput("ch2_cross_table"),
      fluidRow(
        column(6,
          radioButtons("ch2_cross_chart", NULL,
            choices = c("Wykres slupkowy" = "bar", "Heatmapa" = "heatmap"),
            selected = "bar", inline = TRUE
          )
        )
      ),
      zoom_plot_ui("ch2_cross_plot", height = "350px")
    ),

    # --- Narrative before Widget 5 ---
    lc_h2("ch2-dominanta", "Dominanta (moda)"),

    tagList(
      p("Dominanta (moda) to jedyna miara tendencji centralnej dla
        zmiennych nominalnych. Jest to wartość (kategoria), ktora
        występuje najczęściej w zbiorze danych.")
    ),

    # ========================================================================
    # WIDGET 5: Mode (dominanta)
    # ========================================================================
    figure_panel(
      label = "Ryc. 2.6",
      title = "Dominanta — najczęściej występująca kategoria",
      actionButton("ch2_mode_resample", "Losuj nowe proporcje",
                   class = "lc-btn-primary"),
      zoom_plot_ui("ch2_mode_plot", height = "350px"),
      uiOutput("ch2_mode_text")
    ),

    lc_chapter_next(
      num       = "03",
      title     = "Statystyki położenia",
      lead      = "zmienne jakościowe mamy za sobą — pora na narzędzia dla ilościowych.",
      target_id = "ch-polozenie"
    ),

    # Bottom spacer
    lc_spacer("lg")

  )
)

# --------------------------------------------------------------------------
# Chapter 2 Server
# --------------------------------------------------------------------------

ch2_server <- function(input, output, session) {

  ch2_freq_step <- reactiveVal(0)
  ch2_scenario_idx <- reactiveVal(1)
  ch2_random_colors <- reactiveVal(NULL)
  ch2_mode_data <- reactiveVal(NULL)

  # --- Initialise reactive values that need data ---
  observe({
    if (is.null(ch2_mode_data())) {
      ch2_mode_data(student_data$kierunek)
    }
  })

  # ========================================================================
  # Widget 1: Frequency table step-by-step
  # ========================================================================

  observeEvent(input$ch2_freq_s1, { ch2_freq_step(1) })
  observeEvent(input$ch2_freq_s2, { ch2_freq_step(2) })
  observeEvent(input$ch2_freq_s3, { ch2_freq_step(3) })
  observeEvent(input$ch2_freq_s4, { ch2_freq_step(4) })
  observeEvent(input$ch2_freq_reset, { ch2_freq_step(0) })
  observeEvent(input$ch2_freq_var, { ch2_freq_step(0) })

  output$ch2_freq_explanation <- renderUI({
    step <- ch2_freq_step()
    var_name <- input$ch2_freq_var
    is_ord <- (!is.null(var_name) && var_name == "zadowolenie")
    var_label <- if (is_ord) "zadowolenie" else "kierunek"

    if (step == 0) {
      lc_feedback(type = "info",
          "Kliknij kolejne przyciski, aby zbudować tabelę częstości krok po kroku.")
    } else if (step == 1) {
      lc_feedback(type = "info",
          tags$b("Krok 1: Surowe dane. "),
          "Tak wyglądają pierwsze obserwacje zmiennej ",
          tags$code(var_label), ". Każdy wiersz to odpowiedź jednego studenta.",
          if (is_ord) tagList(
            tags$br(),
            tags$em("Uwaga: kategorie mają naturalną kolejność -- od
                    'Bardzo niezadowolony' do 'Bardzo zadowolony'.")
          )
      )
    } else if (step == 2) {
      lc_feedback(type = "info",
          tags$b("Krok 2: Zliczanie. "),
          "Liczymy, ile razy występuje każda kategoria. To są ",
          tags$b("częstości bezwzględne"), " (liczebności).",
          if (is_ord) tagList(
            tags$br(),
            tags$em("Kategorie są uporządkowane -- ich kolejność w tabeli
                    ma znaczenie.")
          )
      )
    } else if (step == 3) {
      lc_feedback(type = "info",
          tags$b("Krok 3: Częstości względne. "),
          "Dzielimy każdą liczebność przez całkowitą liczbę obserwacji (n = ",
          nrow(student_data), "). Wynik możemy wyrazić jako ułamek lub procent.")
    } else if (step == 4) {
      if (is_ord) {
        lc_feedback(type = "ok",
          tags$b("Krok 4: Częstości skumulowane. "),
          "Sumujemy częstości narastająco. ",
          tags$b("Dla zmiennej porządkowej to ma głęboki sens!"),
          tags$br(), tags$br(),
          "Możemy powiedzieć np.: ",
          tags$em("'X% studentów jest neutralnych lub bardziej zadowolonych'"),
          " albo ",
          tags$em("'Y% studentów jest niezadowolonych lub bardzo niezadowolonych'"),
          ".",
          tags$br(), tags$br(),
          "Skumulowany procent daje sensowną interpretację ",
          tags$b("tylko wtedy, gdy kategorie mają naturalną kolejność."))
      } else {
        lc_feedback(type = "warning",
          tags$b("Krok 4: Częstości skumulowane. "),
          "Sumujemy częstości narastająco. ",
          tags$b("Ale uwaga!"), " Dla zmiennej ",
          tags$b("nominalnej"), " kolejność kategorii jest umowna.",
          tags$br(), tags$br(),
          "Stwierdzenie '72% studentów studiuje Informatykę lub wcześniej'
           nie ma sensu -- bo co znaczy 'wcześniej' w liście kierunków?",
          tags$br(), tags$br(),
          tags$em("Przełącz na zmienną porządkową (Zadowolenie), żeby
                  zobaczyć, kiedy skumulowany procent jest naprawdę przydatny."))
      }
    }
  })

  output$ch2_freq_table <- renderTable({
    step <- ch2_freq_step()
    if (step == 0) return(NULL)

    var_name <- input$ch2_freq_var
    is_ord <- (!is.null(var_name) && var_name == "zadowolenie")
    x <- if (is_ord) student_data$zadowolenie else student_data$kierunek
    col_label <- if (is_ord) "Zadowolenie" else "Kierunek"

    if (step == 1) {
      sample_vals <- head(x, 20)
      df <- data.frame(Nr = 1:20, V = as.character(sample_vals))
      names(df) <- c("Nr", col_label)
      return(df)
    }

    counts <- table(x)
    df <- data.frame(
      Kategoria = names(counts),
      Liczebnosc = as.integer(counts)
    )
    names(df) <- c("Kategoria", "Liczebność")

    if (step >= 3) {
      df[["Częst. względna"]] <- round(df[["Liczebność"]] / sum(df[["Liczebność"]]), 3)
      df[["Procent (%)"]] <- round(df[["Częst. względna"]] * 100, 1)
    }

    if (step >= 4) {
      df[["Skumul. liczebność"]] <- cumsum(df[["Liczebność"]])
      df[["Skumul. procent (%)"]] <- round(cumsum(df[["Częst. względna"]]) * 100, 1)
    }

    df
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")


  # ========================================================================
  # Widget 1b: Nominal vs Ordinal comparison
  # ========================================================================

  zoom_plot_server("ch2_ord_nom_plot", reactive({
    df <- data.frame(kierunek = student_data$kierunek)
    lvls <- levels(df$kierunek)
    if (isTRUE(input$ch2_ord_shuffle)) {
      lvls <- sample(lvls)
    }
    df$kierunek <- factor(df$kierunek, levels = lvls)
    ggplot(df, aes(x = kierunek)) +
      geom_bar(fill = type_colors["nominalna"], color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)),
                vjust = -0.5, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Kierunek", y = "Liczebność") +
      theme()
  }))

  zoom_plot_server("ch2_ord_ord_plot", reactive({
    df <- data.frame(zadowolenie = student_data$zadowolenie)
    lvls <- levels(df$zadowolenie)
    if (isTRUE(input$ch2_ord_shuffle)) {
      lvls <- sample(lvls)
    }
    df$zadowolenie <- factor(df$zadowolenie, levels = lvls)
    short_labels <- c(
      "Bardzo niezadowolony" = "B. niezad.",
      "Niezadowolony"        = "Niezad.",
      "Neutralny"            = "Neutr.",
      "Zadowolony"           = "Zad.",
      "Bardzo zadowolony"    = "B. zad."
    )
    ggplot(df, aes(x = zadowolenie)) +
      geom_bar(fill = type_colors["porzadkowa"], color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)),
                vjust = -0.5, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      scale_x_discrete(labels = function(x) short_labels[x]) +
      labs(x = "Zadowolenie", y = "Liczebność") +
      theme()
  }))

  output$ch2_ord_explanation <- renderUI({
    if (isTRUE(input$ch2_ord_shuffle)) {
      lc_feedback(type = "warning",
        tags$strong("Losowa kolejność: "),
        "Dla ", tags$b("kierunku studiow"), " (zmienna nominalna) zmiana kolejnośći
         nie zmienia interpretacji -- kategorie nie maja naturalnego porzadku.
         Ale dla ", tags$b("zadowolenia"), " (zmienna porządkowa) losowa kolejność
         jest mylaca! Tracimy informacje o naturalnym porzadku od 'bardzo niezadowolony'
         do 'bardzo zadowolony'."
      )
    } else {
      lc_feedback(type = "info",
        tags$strong("Domyslna kolejność: "),
        "Kierunek studiów pokazujemy w kolejnośći alfabetycznej (umownej) --
         moglibymy uzyc dowolnej innej. Zadowolenie natomiast ma naturalny
         porzadek: od 'bardzo niezadowolony' do 'bardzo zadowolony'. ",
        tags$em("Wlacz 'Losowa kolejność', zeby zobaczyc różnice!")
      )
    }
  })

  # ========================================================================
  # Widget 2: Pie vs Bar -- scenario comparison (Chart.js)
  # ========================================================================

  observeEvent(input$ch2_sc1, { ch2_scenario_idx(1) })
  observeEvent(input$ch2_sc2, { ch2_scenario_idx(2) })
  observeEvent(input$ch2_sc3, { ch2_scenario_idx(3) })

  ch2_current_scenario <- reactive({
    pie_vs_bar_scenarios[[ch2_scenario_idx()]]
  })

  # Send scenario data to Chart.js via custom message
  observe({
    s <- ch2_current_scenario()
    session$sendCustomMessage("render_scenario", list(
      labels = as.list(s$labels),
      data   = as.list(s$data),
      colors = as.list(s$colors)
    ))
  })

  output$ch2_scenario_pie_verdict <- renderUI({
    s <- ch2_current_scenario()
    badge_style <- if (s$pie_ok) "background: var(--upwr-sage-tint); color: var(--upwr-sage);" else
                                 "background: var(--upwr-accent-tint); color: var(--upwr-accent);"
    badge_text  <- if (s$pie_ok) "OK" else "Problem"
    div(style = "text-align: center; font-size: 13px; color: var(--upwr-reference); margin-top: 6px;",
      tags$span(style = paste0("display: inline-block; font-size: 11px; padding: 2px 8px;
                                 border-radius: 6px; font-weight: 500; margin-right: 4px; ",
                                badge_style), badge_text),
      s$pie_verdict
    )
  })

  output$ch2_scenario_bar_verdict <- renderUI({
    s <- ch2_current_scenario()
    div(style = "text-align: center; font-size: 13px; color: var(--upwr-reference); margin-top: 6px;",
      tags$span(style = "display: inline-block; font-size: 11px; padding: 2px 8px;
                         border-radius: 6px; font-weight: 500; margin-right: 4px;
                         background: var(--upwr-sage-tint); color: var(--upwr-sage);", "OK"),
      s$bar_verdict
    )
  })

  output$ch2_scenario_legend <- renderUI({
    s <- ch2_current_scenario()
    legend_items <- mapply(function(label, color, value) {
      tags$span(style = "display: flex; align-items: center; gap: 4px;",
        tags$span(style = paste0("width: 10px; height: 10px; border-radius: 2px;
                                   flex-shrink: 0; background: ", color, ";")),
        paste0(label, " ", value, "%")
      )
    }, s$labels, s$colors, s$data, SIMPLIFY = FALSE)
    tagList(legend_items)
  })

  # ========================================================================
  # Widget 4: Color manipulation demo
  # ========================================================================

  observeEvent(input$ch2_color_random, {
    # Paleta o gwarantowanym kontraście na białym tle
    safe_colors <- c(
      "#e6194B", "#3cb44b", "#4363d8", "#f58231", "#911eb4",
      "#42d4f4", "#f032e6", "#bfef45", "#fabed4", "#469990",
      "#dcbeff", "#9A6324", "#800000", "#aaffc3", "#808000",
      "#000075", "#a9a9a9", "#e6beff", "#ffd8b1", "#fffac8"
    )
    ch2_random_colors(sample(safe_colors, 4))
  })

  # Reset random colors when palette selector changes
  observeEvent(input$ch2_color_palette, {
    ch2_random_colors(NULL)
  })

  zoom_plot_server("ch2_color_plot", reactive({
    df <- data.frame(kierunek = student_data$kierunek)
    df_counts <- as.data.frame(table(df$kierunek))
    names(df_counts) <- c("Kierunek", "n")

    levels_order <- levels(student_data$kierunek)
    if (is.null(levels_order)) levels_order <- unique(as.character(student_data$kierunek))

    rand_cols <- ch2_random_colors()
    palette_choice <- input$ch2_color_palette

    if (!is.null(rand_cols)) {
      fill_colors <- setNames(rand_cols, levels_order)
      subtitle <- "Losowa paleta kolorow"
    } else if (palette_choice == "neutral") {
      fill_colors <- setNames(rep(upwr_reference, 4), levels_order)
      subtitle <- "Neutralna - wszystkie kategorie rowne"
    } else if (palette_choice == "warm") {
      fill_colors <- setNames(
        ifelse(levels_order == "Informatyka", upwr_accent, upwr_rule),
        levels_order
      )
      subtitle <- "Ciepla paleta - uwaga przyciagana do Informatyki"
    } else if (palette_choice == "cool") {
      fill_colors <- setNames(
        ifelse(levels_order == "Biologia", upwr_cat["indygo"], upwr_rule),
        levels_order
      )
      subtitle <- "Zimna paleta - uwaga przyciagana do Biologii"
    } else if (palette_choice == "biased") {
      biggest <- df_counts$Kierunek[which.max(df_counts$n)]
      smallest <- df_counts$Kierunek[which.min(df_counts$n)]
      cols <- setNames(rep(upwr_reference, 4), levels_order)
      cols[as.character(biggest)]  <- upwr_accent
      cols[as.character(smallest)] <- upwr_secondary
      fill_colors <- cols
      subtitle <- paste0("Stronnicza - ", biggest,
                         " wyróżniona, ", smallest, " wyciszona")
    } else if (palette_choice == "viridis") {
      fill_colors <- setNames(
        c("#440154", "#31688e", "#35b779", "#fde725")[1:length(levels_order)],
        levels_order)
      subtitle <- "Viridis -- percepcyjnie równomierna, colorblind-safe"
    } else if (palette_choice == "set2") {
      fill_colors <- setNames(
        c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")[1:length(levels_order)],
        levels_order)
      subtitle <- "Set2 (ColorBrewer) -- popularny domyślny wybór"
    } else if (palette_choice == "okabe_ito") {
      fill_colors <- setNames(
        c("#E69F00", "#56B4E9", "#009E73", "#CC79A7")[1:length(levels_order)],
        levels_order)
      subtitle <- "Okabe-Ito -- zaprojektowana specjalnie dla daltonistów"
    } else if (palette_choice == "tableau") {
      fill_colors <- setNames(
        c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2")[1:length(levels_order)],
        levels_order)
      subtitle <- "Tableau 10 -- standard w wizualizacji danych"
    } else {
      fill_colors <- setNames(rep(upwr_reference, length(levels_order)), levels_order)
      subtitle <- ""
    }

    ggplot(df_counts, aes(x = Kierunek, y = n, fill = Kierunek)) +
      geom_col(color = "white", width = 0.7) +
      geom_text(aes(label = n), vjust = -0.5, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      scale_fill_manual(values = fill_colors, guide = "none") +
      labs(x = "Kierunek", y = "Liczebność")
  }))


  # ========================================================================
  # Widget 4b: Cross-tabulation

  output$ch2_cross_table <- renderTable({
    row_var <- input$ch2_cross_row
    col_var <- input$ch2_cross_col
    req(row_var, col_var, row_var != col_var)

    tbl <- table(student_data[[row_var]], student_data[[col_var]])

    if (input$ch2_cross_type == "counts") {
      df <- as.data.frame.matrix(tbl)
      df <- cbind(data.frame(` ` = rownames(df), check.names = FALSE), df)
    } else if (input$ch2_cross_type == "row_pct") {
      pct <- round(prop.table(tbl, margin = 1) * 100, 1)
      df <- as.data.frame.matrix(pct)
      df[] <- lapply(df, function(x) paste0(x, "%"))
      df <- cbind(data.frame(` ` = rownames(df), check.names = FALSE), df)
    } else {
      pct <- round(prop.table(tbl, margin = 2) * 100, 1)
      df <- as.data.frame.matrix(pct)
      df[] <- lapply(df, function(x) paste0(x, "%"))
      df <- cbind(data.frame(` ` = rownames(df), check.names = FALSE), df)
    }
    df
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")

  zoom_plot_server("ch2_cross_plot", reactive({
    row_var <- input$ch2_cross_row
    col_var <- input$ch2_cross_col
    chart_type <- input$ch2_cross_chart
    req(row_var, col_var, row_var != col_var)

    df <- data.frame(
      row = student_data[[row_var]],
      col = student_data[[col_var]]
    )

    row_label <- c("plec" = "Płeć", "kierunek" = "Kierunek", "grupa_krwi" = "Grupa krwi")
    col_label <- row_label

    if (!is.null(chart_type) && chart_type == "heatmap") {
      # Heatmap (geom_tile)
      tbl <- table(df$row, df$col)
      if (input$ch2_cross_type == "row_pct") {
        tbl <- prop.table(tbl, margin = 1) * 100
        fill_label <- "% wierszowy"
        fmt <- function(x) paste0(round(x, 1), "%")
      } else if (input$ch2_cross_type == "col_pct") {
        tbl <- prop.table(tbl, margin = 2) * 100
        fill_label <- "% kolumnowy"
        fmt <- function(x) paste0(round(x, 1), "%")
      } else {
        fill_label <- "Liczebność"
        fmt <- function(x) as.character(x)
      }
      heat_df <- as.data.frame(as.table(tbl))
      names(heat_df) <- c("Wiersz", "Kolumna", "Wartosc")

      ggplot(heat_df, aes(x = Kolumna, y = Wiersz, fill = Wartosc)) +
        geom_tile(color = "white", linewidth = 1.5) +
        scale_fill_upwr_seq(variant = "burgundy", name = fill_label) +
        labs(x = col_label[col_var], y = row_label[row_var]) +
                theme(
          panel.grid = element_blank(),
          axis.text = element_text(size = 12)
        )
    } else {
      # Grouped bar chart
      ggplot(df, aes(x = row, fill = col)) +
        geom_bar(position = "dodge", alpha = 0.85, color = "white") +
        scale_fill_upwr() +
        labs(x = row_label[row_var], y = "Liczebność", fill = col_label[col_var]) +
                theme(legend.position = "top")
    }
  }))

  # Widget 5: Mode (dominanta)
  # ========================================================================

  observeEvent(input$ch2_mode_resample, {
    probs <- runif(4)
    probs <- probs / sum(probs)
    new_data <- sample(
      c("Informatyka", "Biologia", "Psychologia", "Ekonomia"),
      200, replace = TRUE, prob = probs
    )
    ch2_mode_data(factor(new_data,
      levels = c("Informatyka", "Biologia", "Psychologia", "Ekonomia")))
  })

  zoom_plot_server("ch2_mode_plot", reactive({
    req(ch2_mode_data())
    x <- ch2_mode_data()
    df_counts <- as.data.frame(table(x))
    names(df_counts) <- c("Kierunek", "n")
    mode_cat <- df_counts$Kierunek[which.max(df_counts$n)]

    df_counts$is_mode <- ifelse(df_counts$Kierunek == mode_cat,
                                "Dominanta", "Inne")

    ggplot(df_counts, aes(x = Kierunek, y = n, fill = is_mode)) +
      geom_col(color = "white", width = 0.7, alpha = 0.9) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      scale_fill_manual(
        values = c("Dominanta" = type_colors["nominalna"], "Inne" = upwr_rule),
        guide = "none"
      ) +
      labs(x = "Kierunek", y = "Liczebność")
  }))

  output$ch2_mode_text <- renderUI({
    req(ch2_mode_data())
    x <- ch2_mode_data()
    counts <- table(x)
    mode_cat <- names(counts)[which.max(counts)]
    mode_n   <- max(counts)
    total_n  <- sum(counts)
    mode_pct <- round(mode_n / total_n * 100, 1)

    lc_feedback(type = "info",
      tags$b("Dominanta: "), mode_cat,
      tags$br(),
      paste0("Wystepuje ", mode_n, " razy (", mode_pct, "% z ", total_n,
             " obserwacji)."),
      tags$br(),
      tags$em("Dla zmiennych nominalnych dominanta to jedyna sensowna miara
              tendencji centralnej - nie mozemy obliczyć średniej ani mediany
              z nazw kategorii.")
    )
  })

}
