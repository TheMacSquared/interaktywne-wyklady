# ============================================================================
# CHAPTER 1: Czas ma znaczenie
# ============================================================================

.ch1_series_info <- list(
  warszawa = list(
    title  = "Temperatura w Warszawie",
    unit   = "°C",
    color  = "niebo",
    char   = "Silna sezonowość (±22°C), powolny trend ocieplenia. Prognozowalna.",
    quiz_answer = "C"
  ),
  bezrobocie = list(
    title  = "Stopa bezrobocia w Polsce",
    unit   = "%",
    color  = "bursztyn",
    char   = "Trend malejący z dużymi epizodami historycznymi (2002, COVID). Umiarkowanie prognozowalna.",
    quiz_answer = "B"
  ),
  noclegi = list(
    title  = "Noclegi turystyczne w Polsce",
    unit   = "tys.",
    color  = "szalwia",
    char   = "Silna sezonowość wakacyjna + wyraźny dołek COVID 2020. Prognozowalna w normalnych warunkach.",
    quiz_answer = "C"
  ),
  pszenica = list(
    title  = "Ceny pszenicy skupu",
    unit   = "PLN/dt",
    color  = "terakota",
    char   = "Cykliczność + zmienna wariancja. Trudna do prognozowania (zdarzenia losowe jak 2022).",
    quiz_answer = "A"
  )
)

ch1_ui <- list(
  id    = "ch-motywacja",
  num   = "01",
  title = "Czas ma znaczenie",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 01 · Szeregi czasowe",
      num    = "01",
      title  = "Czas ma znaczenie.",
      lead   = "Przez cały kurs zakładaliśmy, że obserwacje są od siebie niezależne.
                Co jeśli ta obserwacja pochodzi z tej samej osoby, miejsca lub systemu,
                co poprzednia — tylko dzień, miesiąc lub rok później?"
    ),

    lc_h2("ch1-kiedy-czas", "Kiedy kolejność obserwacji ma znaczenie?"),

    tagList(
      lc_p("Wyobraź sobie dwa pytania: ", tags$em("'Ile wynosi średnia temperatura w Warszawie?'"),
        " i ", tags$em("'Jaka temperatura będzie w Warszawie w lutym przyszłego roku?'"),
        " Pierwsze pytanie to statystyka opisowa — wystarczy zbiór liczb.
        Drugie jest zupełnie inne: wymaga zrozumienia, ", tags$em("jak"), " temperatura zmienia
        się w czasie i co z niej wynika dla przyszłości."),
      lc_p("To jest istota analizy szeregów czasowych: zbiór pomiarów zebranych
        w ustalonym porządku chronologicznym, gdzie czas niesie informację,
        której sama wartość nie zawiera. Temperatura w lutym następuje po styczeń,
        nie jest od niego niezależna — wchodzi w grę ",
        tags$em("autokorelacja"),
        ": każda obserwacja jest powiązana z poprzednimi."),
      inline_callout(
        label = "Definicja", color = "wskazowka",
        p(tags$strong("Szereg czasowy"), " (ang. ", tags$em("time series"), ") to ciąg obserwacji
          tej samej zmiennej, zebranych w kolejnych, równoodległych momentach czasu.
          Zapis: ", tags$em("x₁, x₂, …, xₙ"), " gdzie indeks dolny to czas, nie numer obiektu.")
      )
    ),

    lc_h2("ch1-cztery-szeregi", "Cztery szeregi, cztery charaktery"),

    tagList(
      lc_p("Spójrzmy na cztery realne polskie szeregi. Każdy opowiada inną historię —
        ma inny ", tags$em("charakter"), ": proporcje trendu, sezonowości i losowości.
        Kliknij przycisk pod wykresem, żeby zobaczyć opis.")
    ),

    figure_panel(
      label = "Ryc. 1.1", title = "Wybierz szereg i poznaj jego charakter",
      full_width = TRUE,
      fluidRow(
        column(3,
          div(
            style = "display: flex; flex-direction: column; gap: 8px; margin-bottom: 16px;",
            actionButton("ch1_ser_warszawa",   "Temperatura Warszawa",    class = "lc-btn-outline", width = "100%"),
            actionButton("ch1_ser_bezrobocie", "Bezrobocie PL",           class = "lc-btn-outline", width = "100%"),
            actionButton("ch1_ser_noclegi",    "Noclegi turystyczne",     class = "lc-btn-outline", width = "100%"),
            actionButton("ch1_ser_pszenica",   "Ceny pszenicy",           class = "lc-btn-outline", width = "100%")
          ),
          uiOutput("ch1_ser_desc")
        ),
        column(9,
          zoom_plot_ui("ch1_ser_plot", height = "300px")
        )
      )
    ),

    lc_h2("ch1-kolejnosc", "Eksperyment: kolejność ma znaczenie"),

    tagList(
      lc_p("Oto prosty test: weźmy dane o temperaturze i wymieszajmy je losowo.
        Średnia, mediana, odchylenie standardowe — wszystko to samo.
        Ale ", tags$em("czy możemy prognozować?"),
        " Obejrzyj oba wykresy i odpowiedz na pytanie quizowe poniżej.")
    ),

    figure_panel(
      label = "Ryc. 1.2", title = "Dane w czasie vs. dane wymieszane losowo",
      full_width = TRUE,
      fluidRow(
        column(6,
          zoom_plot_ui("ch1_order_ts_plot",  height = "250px"),
          p(style = "text-align:center; font-size:12px; color: var(--upwr-reference);",
            "Dane chronologicznie")
        ),
        column(6,
          zoom_plot_ui("ch1_order_rnd_plot", height = "250px"),
          p(style = "text-align:center; font-size:12px; color: var(--upwr-reference);",
            "Te same dane w losowej kolejności")
        )
      )
    ),

    lc_h2("ch1-quiz", "Quiz: co widać w szeregu?"),

    figure_panel(
      label = "Quiz 1.1", title = "Która cecha szeregu umożliwia prognozowanie?",
      uiOutput("ch1_quiz_tiles"),
      uiOutput("ch1_quiz_feedback")
    ),

    lc_h2("ch1-trzy-pytania", "Trzy pytania w analizie szeregów"),

    tagList(
      lc_p("Ten wykład odpowie na trzy pytania, które pojawiają się przy każdym szeregu czasowym:"),
      tags$ol(
        tags$li(tags$strong("Jaki jest charakter szeregu?"),
          " — Czy ma trend? Sezonowość? Czy jest stacjonarny? (Rozdziały 2–7)"),
        tags$li(tags$strong("Jak go modelować?"),
          " — ARIMA czy wygładzanie wykładnicze? Jaki rząd modelu? (Rozdziały 8–11)"),
        tags$li(tags$strong("Jak prognozować i oceniać?"),
          " — Horyzont, wachlarz niepewności, metryki błędu. (Rozdziały 12–14)")
      ),
      lc_p("Zanim jednak dojdziemy do modeli, potrzebujemy narzędzi do czytania szeregu.
        Zaczniemy od rozkładu na składowe: trend, sezonowość i reszta.")
    ),

    lc_chapter_next(
      num       = "02",
      title     = "Anatomia szeregu: dekompozycja",
      lead      = "trend, sezonowość i reszta — składowe każdego szeregu",
      target_id = "ch-dekompozycja"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  ch1_selected_series <- reactiveVal("warszawa")

  observeEvent(input$ch1_ser_warszawa,   ch1_selected_series("warszawa"))
  observeEvent(input$ch1_ser_bezrobocie, ch1_selected_series("bezrobocie"))
  observeEvent(input$ch1_ser_noclegi,    ch1_selected_series("noclegi"))
  observeEvent(input$ch1_ser_pszenica,   ch1_selected_series("pszenica"))

  zoom_plot_server("ch1_ser_plot", reactive({
    key  <- ch1_selected_series()
    info <- .ch1_series_info[[key]]
    df   <- .ts_datasets[[key]]$get_df()
    col  <- unname(upwr_cat[info$color])

    ggplot(df, aes(x = date, y = .data[[names(df)[2]]])) +
      geom_line(color = col, linewidth = 0.85) +
      labs(x = NULL, y = info$unit,
           title = info$title) +
      theme_upwr()
  }))

  output$ch1_ser_desc <- renderUI({
    key  <- ch1_selected_series()
    info <- .ch1_series_info[[key]]
    lc_feedback(type = "info",
      tags$strong(info$title), tags$br(),
      info$char
    )
  })

  zoom_plot_server("ch1_order_ts_plot", reactive({
    df <- .ts_datasets[["warszawa"]]$get_df()
    df_sub <- df[df$date >= as.Date("2005-01-01") & df$date <= as.Date("2019-12-01"), ]
    ggplot(df_sub, aes(x = date, y = temp)) +
      geom_line(color = unname(upwr_cat["niebo"]), linewidth = 0.9) +
      labs(x = NULL, y = "°C", title = "Temperatura (chronologicznie)") +
      theme_upwr()
  }))

  zoom_plot_server("ch1_order_rnd_plot", reactive({
    set.seed(1234)
    df <- .ts_datasets[["warszawa"]]$get_df()
    df_sub <- df[df$date >= as.Date("2005-01-01") & df$date <= as.Date("2019-12-01"), ]
    df_rnd <- df_sub
    df_rnd$temp <- sample(df_rnd$temp)
    ggplot(df_rnd, aes(x = date, y = temp)) +
      geom_line(color = upwr_reference, linewidth = 0.85) +
      labs(x = NULL, y = "°C", title = "Temperatura (kolejność losowa)") +
      theme_upwr()
  }))

  ch1_quiz_answered <- reactiveVal(FALSE)
  ch1_quiz_answer   <- reactiveVal(NULL)

  .ch1_choices <- list(
    list(letter = "A", value = "A", text = "Średnia — im wyższa średnia, tym łatwiej prognozować"),
    list(letter = "B", value = "B", text = "Wariancja — małe wahania oznaczają dobry szereg"),
    list(letter = "C", value = "C", text = "Autokorelacja — wartości blisko siebie w czasie są ze sobą powiązane"),
    list(letter = "D", value = "D", text = "Rozkład normalny — szereg normalny jest prognozowalny")
  )

  output$ch1_quiz_tiles <- renderUI({
    if (ch1_quiz_answered()) return(NULL)
    div(class = "quiz-tiles quiz-cols-4",
      lapply(.ch1_choices, function(opt) {
        actionButton(paste0("ch1_tile_", opt$value),
          tagList(
            div(class = "tile-letter", opt$letter),
            div(class = "tile-text",   opt$text)
          ),
          class = "quiz-tile"
        )
      })
    )
  })

  observe({
    lapply(.ch1_choices, function(opt) {
      local({
        val <- opt$value
        observeEvent(input[[paste0("ch1_tile_", val)]], {
          if (ch1_quiz_answered()) return()
          ch1_quiz_answer(val)
          ch1_quiz_answered(TRUE)
        }, ignoreInit = TRUE)
      })
    })
  })

  output$ch1_quiz_feedback <- renderUI({
    req(ch1_quiz_answered())
    if (ch1_quiz_answer() == "C") {
      lc_feedback(type = "ok",
        tags$strong("C — Poprawnie! "),
        "Autokorelacja to klucz. Jeśli dzisiejsza wartość jest powiązana z wczorajszą,
         możemy wykorzystać przeszłość do przewidywania przyszłości. Wykres chronologiczny
         temperatury pokazuje regularny wzorzec — wymieszany już go nie posiada."
      )
    } else {
      lc_feedback(type = "warning",
        tags$strong("Nie do końca. "),
        "Prognozowanie jest możliwe nie ze względu na poziom czy rozkład wartości,
         ale dlatego, że obserwacje blisko siebie w czasie są ze sobą powiązane (autokorelacja).
         Losowo wymieszane dane mają tę samą średnią i wariancję — ale żadnej struktury czasowej."
      )
    }
  })
}
