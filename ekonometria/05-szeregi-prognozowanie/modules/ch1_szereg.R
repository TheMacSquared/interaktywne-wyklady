# ============================================================================
# ROZDZIAŁ 1: Szereg czasowy
# ============================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

ch1_ui <- lecture_chapter(
  id = "ch-szereg",
  num = "01",
  title = "Szereg czasowy",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 05 · Prognozy",
      num = "01",
      title = "Dane uporządkowane w czasie.",
      lead = "Sprzedaż piekarni w styczniu, lutym, marcu… Każda obserwacja ma swoją datę, a kolejność liczb to nie przypadek — to informacja. Szeregi czasowe wymagają innego podejścia niż zwykła regresja."
    ),

    lc_h2("ch1-co-to", "Co to jest szereg czasowy?"),
    lc_p("Szereg czasowy to ciąg obserwacji jednej zmiennej w równych odstępach czasu. Liczy się nie tylko sama wartość, ale i to, kiedy została zmierzona — bo następna obserwacja zwykle „pamięta”, co było wcześniej."),
    tags$ul(
      tags$li("kwartalna sprzedaż firmy odzieżowej,"),
      tags$li("miesięczna stopa bezrobocia w danych GUS,"),
      tags$li("dzienne kursy walut na rynku FX,"),
      tags$li("roczne plony pszenicy w gospodarstwie.")
    ),

    lc_h2("ch1-skladniki", "Trzy składniki szeregu"),
    lc_p("Klasyczna dekompozycja rozkłada obserwowaną wartość na trzy elementy. Każdy z nich opisuje inny rodzaj zachowania:"),
    lc_formula_box(
      withMathJax(helpText("$$Y_t = T_t + S_t + \\varepsilon_t$$")),
      tags$ul(
        tags$li(strong("T_t"), " — trend: długookresowy kierunek zmian (rośnie, spada, stoi)."),
        tags$li(strong("S_t"), " — sezonowość: powtarzalny wzorzec (kwartalny, miesięczny, tygodniowy)."),
        tags$li(strong("ε_t"), " — wahania losowe: część nieprzewidywalna, której model nie tłumaczy.")
      )
    ),

    lc_h2("ch1-przyklady", "Cztery typowe wzorce"),
    lc_p("Zanim dopasujemy jakikolwiek model, warto zobaczyć, jak wyglądają „czyste” warianty: sam trend, sama sezonowość, oba razem oraz sam szum bez struktury."),
    figure_panel(
      label = "Ryc. 1.1",
      title = "Wzorce składników szeregu",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          selectInput("ch1_pattern", "Pokaż wzorzec:",
            choices = c(
              "Tylko trend" = "trend",
              "Tylko sezonowość" = "season",
              "Trend + sezonowość" = "both",
              "Sam szum" = "noise"
            ),
            selected = "both"
          )
        ),
        column(
          8,
          plotOutput("ch1_plot", height = "360px"),
          uiOutput("ch1_feedback")
        )
      )
    ),

    inline_callout(
      label = "Wskazówka",
      color = "wskazowka",
      "Przed dopasowaniem modelu zawsze narysuj szereg. Często gołym okiem widać trend i sezonowość — wtedy wiesz, jakie składniki dodać do równania, zanim policzysz pierwszy współczynnik."
    ),

    lc_chapter_next(
      num = "02",
      title = "Modele dynamiczne i prognoza",
      lead = "trend, sezonowość i przewidywanie",
      target_id = "ch-prognoza"
    )
  )
)

ch1_server <- function(input, output, session) {
  ch1_df <- reactive({
    pat <- input$ch1_pattern %||% "both"
    params <- switch(pat,
      trend  = list(trend = 0.8,  season = 0,  noise = 2),
      season = list(trend = 0.0,  season = 10, noise = 2),
      both   = list(trend = 0.8,  season = 10, noise = 2),
      noise  = list(trend = 0.0,  season = 0,  noise = 6),
      list(trend = 0.8, season = 10, noise = 2)
    )
    eco_ts_data(n = 40, trend = params$trend, season = params$season,
                noise = params$noise, seed = 101)
  })

  output$ch1_plot <- renderPlot({
    ggplot(ch1_df(), aes(t, y)) +
      geom_line(color = upwr_secondary, linewidth = 0.7) +
      geom_point(color = upwr_secondary, size = 2) +
      labs(x = "Okres (kwartał)", y = "Wartość Y") +
      theme_upwr()
  })

  output$ch1_feedback <- renderUI({
    pat <- input$ch1_pattern %||% "both"
    msg <- switch(pat,
      trend  = "Widać systematyczny wzrost z okresu na okres — to czysty trend. Nie ma powtarzalnego wzorca rocznego, więc model będzie potrzebował tylko zmiennej czasu t.",
      season = "Wartości oscylują wokół stałego poziomu z regularnym, czteroletnim pulsem. To czysta sezonowość — bez trendu. Model potrzebuje tylko zmiennych kwartalnych.",
      both   = "Linia wznosi się, ale z wyraźnymi kwartalnymi wahaniami. Tak wygląda większość realnych szeregów ekonomicznych — sprzedaż, zatrudnienie, zużycie energii. Model musi mieć i trend, i sezonowość.",
      noise  = "Brak kierunku, brak wzorca — wartości skaczą losowo wokół poziomu. Tu prognoza punktowa to po prostu średnia, a przedział ufności jest szeroki.",
      "Wybierz wzorzec, żeby zobaczyć interpretację."
    )
    lc_feedback(type = "info", lc_p(msg))
  })
}
