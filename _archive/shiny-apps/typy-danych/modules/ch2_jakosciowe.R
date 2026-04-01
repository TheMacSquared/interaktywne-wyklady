# ============================================================================
# CHAPTER 2: Zmienne jakościowe
# ============================================================================

ch2_ui <- tabPanel("2. Zmienne jakościowe",
  fluidRow(column(8, offset = 2,

    # --- Introduction ---
    div(class = "chapter-recap",
      "W poprzednim rozdziale poznalismy cztery typy zmiennych. Teraz zajmiemy sie
       pierwszym z nich -- zmiennymi jakościowymi."
    ),
    div(class = "section-title", "Zmienne jakościowe"),

    div(class = "narrative",
      p("Zmienne jakościowe opisują cechy, nie liczby. Podstawowym narzędziem
        ich opisu jest tabela częstości. Zobaczmy krok po kroku, jak ja
        zbudować na przykładzie zmiennej ", tags$b("kierunek studiow"), ".")
    ),

    # ========================================================================
    # WIDGET 1: Frequency table step-by-step
    # ========================================================================
    div(class = "widget-block",
      h4("Tabela cz\u0119sto\u015bci - krok po kroku"),
      radioButtons("ch2_freq_var", "Wybierz zmienn\u0105:",
        choices = c(
          "Kierunek studi\u00f3w (nominalna)" = "kierunek",
          "Zadowolenie ze studi\u00f3w (porz\u0105dkowa)" = "zadowolenie"
        ),
        selected = "kierunek", inline = TRUE
      ),
      div(class = "step-buttons",
        actionButton("ch2_freq_s1", "1. Surowe dane",
                     class = "btn-outline-primary"),
        actionButton("ch2_freq_s2", "2. Zliczanie",
                     class = "btn-outline-primary"),
        actionButton("ch2_freq_s3", "3. Cz\u0119sto\u015bci wzgl\u0119dne",
                     class = "btn-outline-primary"),
        actionButton("ch2_freq_s4", "4. Skumulowane",
                     class = "btn-outline-primary")
      ),
      actionButton("ch2_freq_reset", "Reset", class = "btn-secondary btn-sm"),
      uiOutput("ch2_freq_explanation"),
      tableOutput("ch2_freq_table")
    ),

    # ========================================================================
    # WIDGET 1b: Nominal vs Ordinal comparison
    # ========================================================================
    div(class = "section-title", "Nominalna vs porządkowa -- czy kolejność ma znaczenie?"),

    div(class = "narrative",
      p("Zanim przejdziemy do wizualizacji, zatrzymajmy sie na waznym rozróżnieniu.
        Zmienne jakościowe dzielimy na ", tags$b("nominalne"), " (kategorie bez naturalnej
        kolejnośći) i ", tags$b("porzadkowe"), " (kategorie z logiczna kolejnośćia).
        Ta roznica ma praktyczne konsekwencje.")
    ),

    div(class = "widget-block",
      h4("Czy kolejność kategorii ma znaczenie?"),

      checkboxInput("ch2_ord_shuffle", "Losowa kolejność kategorii", value = FALSE),

      fluidRow(
        column(6,
          h5(style = "text-align: center; color: #e74c3c;", "Nominalna: Kierunek studiów"),
          plotOutput("ch2_ord_nom_plot", height = "300px")
        ),
        column(6,
          h5(style = "text-align: center; color: #f39c12;", "Porzadkowa: Zadowolenie"),
          plotOutput("ch2_ord_ord_plot", height = "300px")
        )
      ),

      uiOutput("ch2_ord_explanation"),

    ),

    # --- Narrative before Widget 2 ---
    div(class = "section-title", "Wykres kołowy vs słupkowy"),

    div(class = "narrative",
      p("Jak wizualizować zmienne jakościowe? Porównajmy wykres kołowy ze słupkowym
        w trzech scenariuszach -- od latwego do trudnego. Zobaczysz, dlaczego
        wykres słupkowy jest ", tags$b("zawsze"), " co najmniej tak samo czytelny.")
    ),

    # ========================================================================
    # WIDGET 2: Pie vs Bar -- scenario comparison
    # ========================================================================
    div(class = "widget-block",
      h4("Trzy scenariusze porównawcze"),
      div(style = "display: flex; gap: 8px; margin-bottom: 15px; flex-wrap: wrap;",
        actionButton("ch2_sc1", "1. Duze różnice",
                     class = "btn-outline-primary"),
        actionButton("ch2_sc2", "2. Podobne wartości",
                     class = "btn-outline-primary"),
        actionButton("ch2_sc3", "3. Podobne + zle kolory",
                     class = "btn-outline-primary")
      ),
      fluidRow(
        column(6,
          h5(style = "text-align: center; color: #5f5e5a;", "Wykres kołowy"),
          div(style = "position: relative; width: 100%; height: 320px;",
            tags$canvas(id = "ch2_pie_canvas")
          ),
          uiOutput("ch2_scenario_pie_verdict")
        ),
        column(6,
          h5(style = "text-align: center; color: #5f5e5a;", "Wykres słupkowy -- te same dane"),
          div(style = "position: relative; width: 100%; height: 320px;",
            tags$canvas(id = "ch2_bar_canvas")
          ),
          uiOutput("ch2_scenario_bar_verdict")
        )
      ),
      div(style = "display: flex; flex-wrap: wrap; gap: 14px; font-size: 12px; color: #5f5e5a; margin-top: 8px;",
        id = "ch2_legend",
        uiOutput("ch2_scenario_legend")
      )
    ),

    # --- Narrative before Widget 4 ---
    div(class = "section-title", "Manipulacja kolorami"),

    div(class = "narrative",
      p("Kolory na wykresie mogą manipulowac odbiorem danych. Zobaczmy,
        jak ten sam zestaw danych moze wyglądać zupełnie inaczej w
        zależności od doboru palety kolorow.")
    ),

    # ========================================================================
    # WIDGET 4: Color manipulation demo
    # ========================================================================
    div(class = "widget-block",
      h4("Jak kolory zmieniaja percepcje danych"),
      fluidRow(
        column(4,
          selectInput("ch2_color_palette", "Paleta kolor\u00f3w:",
            choices = c(
              "Neutralna (szara)" = "neutral",
              "Ciep\u0142a (podkre\u015bla Informatyk\u0119)" = "warm",
              "Zimna (podkre\u015bla Biologi\u0119)" = "cool",
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
                       class = "btn-outline-secondary", width = "100%")
        ),
        column(8, plotOutput("ch2_color_plot", height = "380px"))
      ),
      div(class = "callout-warning",
        tags$b("Pami\u0119taj: "),
        "Wyb\u00f3r kolor\u00f3w nie jest neutralny. Intensywne, cieplejsze barwy
         przyci\u0105gaj\u0105 uwag\u0119, a jasne/szare marginalizuj\u0105 kategorie.",
        tags$br(), tags$br(),
        tags$b("Dobre praktyki: "),
        tags$ul(
          tags$li(tags$b("Viridis"), " -- percepcyjnie r\u00f3wnomierna (r\u00f3\u017cnice
            warto\u015bci = r\u00f3\u017cnice w kolorze), czytelna w skali szaro\u015bci
            i bezpieczna dla daltonist\u00f3w. Domy\u015blna w wielu pakietach R."),
          tags$li(tags$b("Okabe-Ito"), " -- paleta zaprojektowana specjalnie
            pod k\u0105tem daltoni\u015bt\u00f3w (ok. 8% m\u0119\u017cczyzn). Klasyczny wyb\u00f3r
            w publikacjach naukowych."),
          tags$li(tags$b("ColorBrewer (Set2, Set3, Paired...)"), " -- rodzina palet
            stworzonych przez kartograf\u0119 Cynthia Brewer. W R dost\u0119pne przez ",
            tags$code("scale_fill_brewer()"), "."),
          tags$li(tags$b("Tableau 10"), " -- standard w narz\u0119dziach BI,
            zbalansowana jasno\u015b\u0107 i kontrast.")
        )
      )
    ),

    # ========================================================================
    # WIDGET 4b: Cross-tabulation
    # ========================================================================
    div(class = "section-title", "Tabela krzyzowa -- dwie zmienne jednoczesnie"),

    div(class = "narrative",
      p("Dotychczas analizowalismy po jednej zmiennej. Ale często chcemy
        zbadac ", tags$b("zaleznosc miedzy dwiema zmiennymi jakościowymi"),
        ". Sluzy do tego tabela krzyzowa (kontyngencji).")
    ),

    div(class = "widget-block",
      h4("Tabela krzyzowa"),
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
      plotOutput("ch2_cross_plot", height = "350px")
    ),

    # --- Narrative before Widget 5 ---
    div(class = "section-title", "Dominanta (moda)"),

    div(class = "narrative",
      p("Dominanta (moda) to jedyna miara tendencji centralnej dla
        zmiennych nominalnych. Jest to wartość (kategoria), ktora
        występuje najczęściej w zbiorze danych.")
    ),

    # ========================================================================
    # WIDGET 5: Mode (dominanta)
    # ========================================================================
    div(class = "widget-block",
      h4("Dominanta - najcz\u0119\u015bciej wyst\u0119puj\u0105ca kategoria"),
      actionButton("ch2_mode_resample", "Losuj nowe proporcje",
                   class = "btn-primary"),
      plotOutput("ch2_mode_plot", height = "350px"),
      uiOutput("ch2_mode_text")
    ),

    div(class = "chapter-transition",
      p("Zmienne jakościowe opisaliśmy tabelami częstości i dominanta.
        A co ze zmiennymi ilościowymi? Potrzebujemy nowych narzedzi -- statystyk polozenia."),
      actionButton("ch2_next", "Dalej: 3. Statystyki polozenia \u2192",
                   class = "btn-primary btn-lg")
    ),

    # Bottom spacer
    div(style = "height: 60px;")

  )) # end column / fluidRow
) # end ch2 tabPanel

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
      div(class = "callout-info",
          "Kliknij kolejne przyciski, aby zbudowa\u0107 tabel\u0119 cz\u0119sto\u015bci krok po kroku.")
    } else if (step == 1) {
      div(class = "callout-info",
          tags$b("Krok 1: Surowe dane. "),
          "Tak wygl\u0105daj\u0105 pierwsze obserwacje zmiennej ",
          tags$code(var_label), ". Ka\u017cdy wiersz to odpowied\u017a jednego studenta.",
          if (is_ord) tagList(
            tags$br(),
            tags$em("Uwaga: kategorie maj\u0105 naturaln\u0105 kolejno\u015b\u0107 -- od
                    'Bardzo niezadowolony' do 'Bardzo zadowolony'.")
          )
      )
    } else if (step == 2) {
      div(class = "callout-info",
          tags$b("Krok 2: Zliczanie. "),
          "Liczymy, ile razy wyst\u0119puje ka\u017cda kategoria. To s\u0105 ",
          tags$b("cz\u0119sto\u015bci bezwzgl\u0119dne"), " (liczebno\u015bci).",
          if (is_ord) tagList(
            tags$br(),
            tags$em("Kategorie s\u0105 uporz\u0105dkowane -- ich kolejno\u015b\u0107 w tabeli
                    ma znaczenie.")
          )
      )
    } else if (step == 3) {
      div(class = "callout-info",
          tags$b("Krok 3: Cz\u0119sto\u015bci wzgl\u0119dne. "),
          "Dzielimy ka\u017cd\u0105 liczebno\u015b\u0107 przez ca\u0142kowit\u0105 liczb\u0119 obserwacji (n = ",
          nrow(student_data), "). Wynik mo\u017cemy wyrazi\u0107 jako u\u0142amek lub procent.")
    } else if (step == 4) {
      if (is_ord) {
        div(class = "callout-success",
          tags$b("Krok 4: Cz\u0119sto\u015bci skumulowane. "),
          "Sumujemy cz\u0119sto\u015bci narastaj\u0105co. ",
          tags$b("Dla zmiennej porz\u0105dkowej to ma g\u0142\u0119boki sens!"),
          tags$br(), tags$br(),
          "Mo\u017cemy powiedzie\u0107 np.: ",
          tags$em("'X% student\u00f3w jest neutralnych lub bardziej zadowolonych'"),
          " albo ",
          tags$em("'Y% student\u00f3w jest niezadowolonych lub bardzo niezadowolonych'"),
          ".",
          tags$br(), tags$br(),
          "Skumulowany procent daje sensown\u0105 interpretacj\u0119 ",
          tags$b("tylko wtedy, gdy kategorie maj\u0105 naturaln\u0105 kolejno\u015b\u0107."))
      } else {
        div(class = "callout-warning",
          tags$b("Krok 4: Cz\u0119sto\u015bci skumulowane. "),
          "Sumujemy cz\u0119sto\u015bci narastaj\u0105co. ",
          tags$b("Ale uwaga!"), " Dla zmiennej ",
          tags$b("nominalnej"), " kolejno\u015b\u0107 kategorii jest umowna.",
          tags$br(), tags$br(),
          "Stwierdzenie '72% student\u00f3w studiuje Informatyk\u0119 lub wcze\u015bniej'
           nie ma sensu -- bo co znaczy 'wcze\u015bniej' w li\u015bcie kierunk\u00f3w?",
          tags$br(), tags$br(),
          tags$em("Prze\u0142\u0105cz na zmienn\u0105 porz\u0105dkow\u0105 (Zadowolenie), \u017ceby
                  zobaczy\u0107, kiedy skumulowany procent jest naprawd\u0119 przydatny."))
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
    names(df) <- c("Kategoria", "Liczebno\u015b\u0107")

    if (step >= 3) {
      df[["Cz\u0119st. wzgl\u0119dna"]] <- round(df[["Liczebno\u015b\u0107"]] / sum(df[["Liczebno\u015b\u0107"]]), 3)
      df[["Procent (%)"]] <- round(df[["Cz\u0119st. wzgl\u0119dna"]] * 100, 1)
    }

    if (step >= 4) {
      df[["Skumul. liczebno\u015b\u0107"]] <- cumsum(df[["Liczebno\u015b\u0107"]])
      df[["Skumul. procent (%)"]] <- round(cumsum(df[["Cz\u0119st. wzgl\u0119dna"]]) * 100, 1)
    }

    df
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")


  # ========================================================================
  # Widget 1b: Nominal vs Ordinal comparison
  # ========================================================================

  output$ch2_ord_nom_plot <- renderPlot({
    df <- data.frame(kierunek = student_data$kierunek)
    lvls <- levels(df$kierunek)
    if (isTRUE(input$ch2_ord_shuffle)) {
      lvls <- sample(lvls)
    }
    df$kierunek <- factor(df$kierunek, levels = lvls)
    ggplot(df, aes(x = kierunek)) +
      geom_bar(fill = col_nominal, color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)),
                vjust = -0.5, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Kierunek", y = "Liczebność") +
      theme_minimal(base_size = 14)
  })

  output$ch2_ord_ord_plot <- renderPlot({
    df <- data.frame(zadowolenie = student_data$zadowolenie)
    lvls <- levels(df$zadowolenie)
    if (isTRUE(input$ch2_ord_shuffle)) {
      lvls <- sample(lvls)
    }
    df$zadowolenie <- factor(df$zadowolenie, levels = lvls)
    ggplot(df, aes(x = zadowolenie)) +
      geom_bar(fill = col_ordinal, color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)),
                vjust = -0.5, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) +
      labs(x = "Zadowolenie", y = "Liczebność") +
      theme_minimal(base_size = 14)
  })

  output$ch2_ord_explanation <- renderUI({
    if (isTRUE(input$ch2_ord_shuffle)) {
      div(class = "callout-warning",
        tags$strong("Losowa kolejność: "),
        "Dla ", tags$b("kierunku studiow"), " (zmienna nominalna) zmiana kolejnośći
         nie zmienia interpretacji -- kategorie nie maja naturalnego porzadku.
         Ale dla ", tags$b("zadowolenia"), " (zmienna porządkowa) losowa kolejność
         jest mylaca! Tracimy informacje o naturalnym porzadku od 'bardzo niezadowolony'
         do 'bardzo zadowolony'."
      )
    } else {
      div(class = "callout-info",
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
    badge_style <- if (s$pie_ok) "background: #EAF3DE; color: #3B6D11;" else
                                 "background: #FCEBEB; color: #A32D2D;"
    badge_text  <- if (s$pie_ok) "OK" else "Problem"
    div(style = "text-align: center; font-size: 13px; color: #5f5e5a; margin-top: 6px;",
      tags$span(style = paste0("display: inline-block; font-size: 11px; padding: 2px 8px;
                                 border-radius: 6px; font-weight: 500; margin-right: 4px; ",
                                badge_style), badge_text),
      s$pie_verdict
    )
  })

  output$ch2_scenario_bar_verdict <- renderUI({
    s <- ch2_current_scenario()
    div(style = "text-align: center; font-size: 13px; color: #5f5e5a; margin-top: 6px;",
      tags$span(style = "display: inline-block; font-size: 11px; padding: 2px 8px;
                         border-radius: 6px; font-weight: 500; margin-right: 4px;
                         background: #EAF3DE; color: #3B6D11;", "OK"),
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
    # Paleta o gwarantowanym kontra\u015bcie na bia\u0142ym tle
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

  output$ch2_color_plot <- renderPlot({
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
      fill_colors <- setNames(rep("#95a5a6", 4), levels_order)
      subtitle <- "Neutralna - wszystkie kategorie rowne"
    } else if (palette_choice == "warm") {
      fill_colors <- setNames(
        ifelse(levels_order == "Informatyka", "#e74c3c", "#d5d8dc"),
        levels_order
      )
      subtitle <- "Ciepla paleta - uwaga przyciagana do Informatyki"
    } else if (palette_choice == "cool") {
      fill_colors <- setNames(
        ifelse(levels_order == "Biologia", "#2980b9", "#d5d8dc"),
        levels_order
      )
      subtitle <- "Zimna paleta - uwaga przyciagana do Biologii"
    } else if (palette_choice == "biased") {
      biggest <- df_counts$Kierunek[which.max(df_counts$n)]
      smallest <- df_counts$Kierunek[which.min(df_counts$n)]
      cols <- setNames(rep("#bdc3c7", 4), levels_order)
      cols[as.character(biggest)]  <- "#e74c3c"
      cols[as.character(smallest)] <- "#2c3e50"
      fill_colors <- cols
      subtitle <- paste0("Stronnicza - ", biggest,
                         " wyr\u00f3\u017cniona, ", smallest, " wyciszona")
    } else if (palette_choice == "viridis") {
      fill_colors <- setNames(
        c("#440154", "#31688e", "#35b779", "#fde725")[1:length(levels_order)],
        levels_order)
      subtitle <- "Viridis -- percepcyjnie r\u00f3wnomierna, colorblind-safe"
    } else if (palette_choice == "set2") {
      fill_colors <- setNames(
        c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")[1:length(levels_order)],
        levels_order)
      subtitle <- "Set2 (ColorBrewer) -- popularny domy\u015blny wyb\u00f3r"
    } else if (palette_choice == "okabe_ito") {
      fill_colors <- setNames(
        c("#E69F00", "#56B4E9", "#009E73", "#CC79A7")[1:length(levels_order)],
        levels_order)
      subtitle <- "Okabe-Ito -- zaprojektowana specjalnie dla daltonist\u00f3w"
    } else if (palette_choice == "tableau") {
      fill_colors <- setNames(
        c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2")[1:length(levels_order)],
        levels_order)
      subtitle <- "Tableau 10 -- standard w wizualizacji danych"
    } else {
      fill_colors <- setNames(rep("#95a5a6", length(levels_order)), levels_order)
      subtitle <- ""
    }

    ggplot(df_counts, aes(x = Kierunek, y = n, fill = Kierunek)) +
      geom_col(color = "white", width = 0.7) +
      geom_text(aes(label = n), vjust = -0.5, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      scale_fill_manual(values = fill_colors, guide = "none") +
      labs(title = "Kierunek studiów",
           subtitle = subtitle,
           x = "Kierunek", y = "Liczebność") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(color = "#7f8c8d", face = "italic"))
  })


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

  output$ch2_cross_plot <- renderPlot({
    row_var <- input$ch2_cross_row
    col_var <- input$ch2_cross_col
    chart_type <- input$ch2_cross_chart
    req(row_var, col_var, row_var != col_var)

    df <- data.frame(
      row = student_data[[row_var]],
      col = student_data[[col_var]]
    )

    row_label <- c("plec" = "P\u0142e\u0107", "kierunek" = "Kierunek", "grupa_krwi" = "Grupa krwi")
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
        fill_label <- "Liczebno\u015b\u0107"
        fmt <- function(x) as.character(x)
      }
      heat_df <- as.data.frame(as.table(tbl))
      names(heat_df) <- c("Wiersz", "Kolumna", "Wartosc")

      ggplot(heat_df, aes(x = Kolumna, y = Wiersz, fill = Wartosc)) +
        geom_tile(color = "white", linewidth = 1.5) +
        geom_text(aes(label = fmt(Wartosc)), size = 5, fontface = "bold") +
        scale_fill_gradient(low = "#eaf2f8", high = "#2980b9", name = fill_label) +
        labs(x = col_label[col_var], y = row_label[row_var]) +
        theme_minimal(base_size = 14) +
        theme(
          panel.grid = element_blank(),
          axis.text = element_text(size = 12)
        )
    } else {
      # Grouped bar chart
      ggplot(df, aes(x = row, fill = col)) +
        geom_bar(position = "dodge", alpha = 0.85, color = "white") +
        scale_fill_brewer(palette = "Set2") +
        labs(x = row_label[row_var], y = "Liczebno\u015b\u0107", fill = col_label[col_var]) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top")
    }
  })

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

  output$ch2_mode_plot <- renderPlot({
    req(ch2_mode_data())
    x <- ch2_mode_data()
    df_counts <- as.data.frame(table(x))
    names(df_counts) <- c("Kierunek", "n")
    mode_cat <- df_counts$Kierunek[which.max(df_counts$n)]

    df_counts$is_mode <- ifelse(df_counts$Kierunek == mode_cat,
                                "Dominanta", "Inne")

    ggplot(df_counts, aes(x = Kierunek, y = n, fill = is_mode)) +
      geom_col(color = "white", width = 0.7, alpha = 0.9) +
      geom_text(aes(label = n), vjust = -0.5, size = 5, fontface = "bold") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      scale_fill_manual(
        values = c("Dominanta" = col_nominal, "Inne" = "#d5d8dc"),
        guide = "none"
      ) +
      labs(title = "Kierunek studiów - dominanta",
           x = "Kierunek", y = "Liczebność") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
  })

  output$ch2_mode_text <- renderUI({
    req(ch2_mode_data())
    x <- ch2_mode_data()
    counts <- table(x)
    mode_cat <- names(counts)[which.max(counts)]
    mode_n   <- max(counts)
    total_n  <- sum(counts)
    mode_pct <- round(mode_n / total_n * 100, 1)

    div(class = "callout-info",
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
