# ==========================================================================
# ROZDZIAŁ 3: PRZESTRZEŃ ZDARZEŃ I DEFINICJA KLASYCZNA
# ==========================================================================

ch3_ui <- lecture_chapter(
  id = "ch-przestrzen",
  num = "03",
  title = "Spośród czego liczymy?",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 03 · Język ryzyka",
      num = "03",
      title = "Mianownik jest częścią modelu.",
      lead = "Definicja klasyczna działa wtedy, gdy potrafimy wymienić wyniki i
              uzasadnić, że są jednakowo możliwe. W Bananpolu nadaje się do
              losowania palety do kontroli, nie do przewidywania każdego wypadku."
    ),

    margin_callout(
      label = "Eksperyment",
      "Inspektor losuje dokładnie jedną z 24 palet. Każda paleta ma taki sam
       numer w generatorze losowym i tę samą szansę wyboru.",
      color = "ok"
    ),

    lc_h2("ch3-przestrzen", "Wyniki i zdarzenia"),
    lc_p(
      "Przestrzeń wyników zawiera wszystkie palety, które mogą zostać wybrane.
       Zdarzenie A jest podzbiorem: paletami z uszkodzonym zabezpieczeniem
       ładunku. Losujemy paletę, a nie uszkodzenie."
    ),

    lc_formula_box(
      withMathJax("$$P(A)=\\frac{|A|}{|\\Omega|}
                   =\\frac{\\text{liczba wyników sprzyjających}}
                   {\\text{liczba jednakowo możliwych wyników}}$$")
    ),

    lc_h2("ch3-slownik", "Te same pojęcia w języku formalnym"),
    lc_p(
      "Podręczniki rachunku prawdopodobieństwa używają kilku stałych nazw.
       Wszystkie już znasz z przykładu palety — tutaj tylko je porządkujemy."
    ),
    figure_panel(
      label = "Słownik",
      title = "Losowanie palety w terminologii formalnej",
      full_width = TRUE,
      tags$table(
        class = "lc-table lc-table-striped lc-table-bordered",
        tags$thead(tags$tr(
          tags$th("Termin"),
          tags$th("Znaczenie"),
          tags$th("W Bananpolu")
        )),
        tags$tbody(
          tags$tr(tags$td("Doświadczenie losowe"), tags$td("Powtarzalna procedura o niepewnym wyniku"), tags$td("Losowanie jednej palety do kontroli")),
          tags$tr(tags$td("Wynik elementarny"), tags$td("Pojedynczy, niepodzielny wynik doświadczenia"), tags$td("Numer wylosowanej palety")),
          tags$tr(tags$td("Przestrzeń wyników Ω"), tags$td("Zbiór wszystkich wyników elementarnych"), tags$td("Wszystkie 24 palety")),
          tags$tr(tags$td("Zdarzenie A"), tags$td("Dowolny podzbiór przestrzeni Ω"), tags$td("Palety z uszkodzonym zabezpieczeniem")),
          tags$tr(tags$td("Zdarzenie pewne"), tags$td("Cała przestrzeń Ω — zachodzi zawsze"), tags$td("Wylosowano którąś z 24 palet")),
          tags$tr(tags$td("Zdarzenie niemożliwe"), tags$td("Zbiór pusty ∅ — nie zachodzi nigdy"), tags$td("Wylosowano paletę numer 25"))
        )
      )
    ),

    lc_p(
      "Z definicji klasycznej wynikają trzy podstawowe własności. Możesz je
       sprawdzić suwakiem poniżej: ustaw 0 palet sprzyjających (zdarzenie
       niemożliwe), potem 24 (zdarzenie pewne)."
    ),
    lc_formula_box(
      withMathJax("$$P(\\Omega)=1,\\qquad P(\\emptyset)=0,\\qquad 0\\le P(A)\\le 1$$"),
      tags$p("Prawdopodobieństwo zdarzenia pewnego wynosi 1, niemożliwego 0,
             a każdego innego zdarzenia — wartość pomiędzy.")
    ),

    lc_h2("ch3-paletki", "Zbuduj zdarzenie na siatce palet"),
    lc_p(
      "Zmieniaj liczbę palet z uszkodzonym zabezpieczeniem. Siatka pokazuje
       pełny mianownik, zdarzenie A oraz jego dopełnienie."
    ),

    figure_panel(
      label = "Interakcja 3",
      title = "Losowa kontrola jednej palety",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput(
            "ch3_favourable",
            "Palety z uszkodzonym zabezpieczeniem",
            min = 0,
            max = 24,
            value = 6,
            step = 1
          ),
          uiOutput("ch3_stats"),
          lc_feedback(
            type = "info",
            "Zdarzenie A: wylosowana paleta ma uszkodzone zabezpieczenie."
          )
        ),
        column(
          8,
          zoom_plot_ui("ch3_grid", height = "430px")
        )
      )
    ),

    lc_h2("ch3-granica", "Kiedy ten iloraz nie wystarcza"),
    lc_p(
      "Palety są jednakowo możliwe, bo wymusza to procedura losowania. Realne
       awarie maszyn, pożary i upadki nie tworzą zwykle listy symetrycznych
       przypadków. Ich prawdopodobieństwa zależą od warunków, ekspozycji,
       historii i zabezpieczeń. Wtedy potrzebujemy danych albo innego modelu."
    ),

    lc_feedback(
      type = "warning",
      tags$strong("Pułapka:"),
      " „jedna z 24 palet” opisuje losowanie do kontroli. Nie oznacza, że
        ryzyko uszkodzenia każdej palety powstało z klasycznej symetrii."
    ),

    lc_chapter_next(
      num = "04",
      title = "Zdarzenia się łączą",
      lead = "Przetłumaczymy słowa „lub”, „i” oraz „nie” na działania na zbiorach.",
      target_id = "ch-zbiory"
    )
  )
)

ch3_server <- function(input, output, session) {
  pallet_data <- reactive({
    req(input$ch3_favourable)
    build_pallet_grid(input$ch3_favourable, total = 24L, columns = 6L)
  })

  output$ch3_stats <- renderUI({
    req(input$ch3_favourable)
    favourable <- as.integer(input$ch3_favourable)
    probability <- classical_probability(favourable, 24L)

    lc_stat_grid(
      lc_stat_box("Licznik |A|", favourable, color = upwr_cat[["terakota"]]),
      lc_stat_box("Mianownik |Ω|", 24, color = upwr_secondary),
      lc_stat_box("P(A)", format_probability_pl(probability), color = upwr_accent),
      lc_stat_box("P(Aᶜ)", format_probability_pl(1 - probability), color = upwr_cat[["szalwia"]]),
      columns = 2
    )
  })

  pallet_plot <- reactive({
    data <- pallet_data()
    data$status <- ifelse(data$favourable, "event", "complement")

    ggplot(data, aes(x = column, y = -row, fill = status)) +
      geom_tile(colour = "white", linewidth = 2, width = 0.92, height = 0.92) +
      geom_text(aes(label = id), colour = "white", fontface = "bold", size = 4) +
      scale_fill_manual(
        values = c(
          "complement" = upwr_reference,
          "event" = upwr_cat[["terakota"]]
        ),
        breaks = c("complement", "event"),
        labels = expression("Dopełnienie " * A^c, "Zdarzenie A")
      ) +
      coord_equal() +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      labs(
        title = "Przestrzeń 24 jednakowo możliwych wyników",
        subtitle = "Kolor wskazuje, czy wynik należy do zdarzenia A",
        x = NULL,
        y = NULL,
        fill = NULL
      ) +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom"
      )
  })

  zoom_plot_server(
    "ch3_grid",
    pallet_plot,
    alt = paste(
      "Siatka 24 palet. Część palet należy do zdarzenia",
      "wylosowania palety z uszkodzonym zabezpieczeniem."
    )
  )
}
