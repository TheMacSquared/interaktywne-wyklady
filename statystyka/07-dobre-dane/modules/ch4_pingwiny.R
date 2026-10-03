# Tab 4: Pingwiny — palmerpenguins, dobry zbiór

# Polskie etykiety pomiarów (wartości = nazwy kolumn w danych)
.ch4_var_labels <- c(
  bill_length_mm    = "Długość dzioba, mm (bill_length_mm)",
  bill_depth_mm     = "Wysokość dzioba, mm (bill_depth_mm)",
  flipper_length_mm = "Długość płetwy, mm (flipper_length_mm)",
  body_mass_g       = "Masa ciała, g (body_mass_g)"
)

ch4_ui <- lecture_chapter(id = "ch4", num = "4", title = "Pingwiny", content = tagList(
  fluidRow(column(8, offset = 2,

    lc_chapter_hero(
      kicker = "Rozdział 04 · Co czyni dobry zbiór danych?",
      num    = "04",
      title  = "Pingwiny z Antarktydy.",
      lead   = "Dobry zbiór nie musi być idealny. Ważne, żeby braki i ograniczenia
                były jawne, małe i możliwe do uzasadnienia."
    ),

    lc_h2("sec-01", "Opis"),

    lc_p("Pingwiny poznaliśmy w wykładzie 06, gdzie na ich przykładzie
      widzieliśmy, co gatunek robi z linią regresji. Zbiór opisuje 344
      pingwiny trzech gatunków (Adelie, Chinstrap i Gentoo) z trzech wysp
      archipelagu Palmera na Antarktydzie, mierzone w latach 2007–2009 przez
      zespół stacji badawczej Palmer. Ma 8 zmiennych: gatunek, wyspę, płeć,
      rok pomiaru oraz cztery pomiary ciała: długość i wysokość dzioba,
      długość płetwy i masę ciała. Dane udostępnili Horst, Hill i Gorman
      (2020)."),

    lc_p("Naturalne pytania dotyczą różnic między gatunkami i płciami oraz
      związków między pomiarami: czy gatunki różnią się masą, czy dłuższa
      płetwa idzie w parze z większą masą. W wykładzie 06 analizowaliśmy
      333 pingwiny, a nie 344. Ten rozdział pokazuje, skąd wzięła się ta
      różnica i czy usunięcie 11 osobników było uzasadnione."),

    lc_h2("sec-02", "Podgląd danych"),

    lc_p("W podglądzie zwróć uwagę na komórki z wartością NA i na to, czy
      każdy wiersz opisuje jednego pingwina."),

    figure_panel(
      label = "Tab. 4.1",
      title = "Pingwiny: 344 osobniki",
      uiOutput("tab3_table")
    ),

    lc_p("Każdy wiersz to jeden osobnik, a zmienne mają jasne nazwy z jednostką
      w nazwie (mm, g). Zbiór łączy zmienne jakościowe (gatunek, wyspa, płeć)
      z ilościowymi pomiarami, więc nadaje się zarówno do porównań grup, jak
      i do korelacji. Już w pierwszych wierszach widać jednak puste komórki."),

    lc_h2("sec-03", "Czy są braki danych?"),

    lc_p("Przy ocenie ", gloss("braki danych", "braków danych"), " liczy się
      nie tylko ich odsetek w każdej zmiennej, ale też to, czy skupiają się
      w kilku wierszach, czy rozkładają po całym zbiorze."),

    figure_panel(
      label = "Ryc. 4.1",
      title = "Odsetek braków w każdej zmiennej",
      zoom_plot_ui("tab3_missing", height = "250px")
    ),

    lc_p("Braki są nieliczne i skupione. Cztery pomiary ciała mają po 2 braki
      (0.6%), a płeć 11 braków (3.2%). Dwa pingwiny nie mają żadnego pomiaru,
      a 9 kolejnych ma pomiary, ale nie ma zapisanej płci. Kompletnych wierszy
      jest 333 z 344, czyli 96.8%. To problem „Braki danych (NA)” z katalogu
      z rozdziału 1, ale w łagodnej postaci: dotyczy 11 wierszy i żadna
      zmienna nie traci istotnej części informacji."),

    lc_h2("sec-04", "Eksploracja"),

    lc_p("Przy porównaniu gatunków patrzymy, czy grupy różnią się położeniem
      i rozrzutem oraz czy w którejś nie ma podejrzanych wartości."),

    figure_panel(
      label = "Ryc. 4.2",
      title = "Pomiary ciała według gatunku",
      fluidRow(
        column(4, selectInput("tab3_var", "Zmienna:",
          choices = stats::setNames(names(.ch4_var_labels), .ch4_var_labels))),
        column(8, zoom_plot_ui("tab3_boxplot", height = "300px"))
      )
    ),

    lc_p("Gatunki wyraźnie się różnią. Mediana długości dzioba wynosi 38.8 mm
      u Adelie, 49.5 mm u Chinstrap i 47.3 mm u Gentoo, a mediana masy ciała
      3700 g u Adelie i Chinstrap oraz 5000 g u Gentoo. Pojedyncze punkty poza
      wąsami leżą blisko pudełek i mają realistyczne wartości (masa od 2700
      do 6300 g), więc nie wyglądają na błędy pomiaru. Grupy nie są
      jednak równoliczne: w pełnym zbiorze jest 152 pingwinów Adelie, 124
      Gentoo i tylko 68 Chinstrap. Do tego gatunek i wyspa są ze sobą
      splecione. Chinstrap żyje w danych tylko na wyspie Dream, Gentoo tylko
      na Biscoe, a na Torgersen występuje wyłącznie Adelie. Porównanie wysp
      jest więc w dużej mierze porównaniem gatunków."),

    lc_h2("sec-05", "Werdykt"),

    lc_p("Braki można tu usunąć razem z całymi wierszami. Dotyczą 3.2% obserwacji,
      a po ich usunięciu zostaje 333 pingwinów: 146 Adelie, 68 Chinstrap
      i 119 Gentoo. Proporcje gatunków prawie się nie zmieniają, więc
      usunięcie nie przesuwa wyników w stronę któregoś z nich. Dlatego
      w wykładzie 06 pracowaliśmy na 333 obserwacjach. Przy analizach, które
      nie używają płci, wystarczy usunąć tylko 2 wiersze bez pomiarów."),

    lc_p("Z 68 obserwacjami w najmniejszej grupie mają sens test t dla dwóch
      gatunków, ", gloss("ANOVA"), " dla trzech, test chi-kwadrat dla gatunku
      i płci, korelacja pomiarów oraz regresja. W każdej analizie łączącej
      gatunki trzeba jednak uwzględnić gatunek. W wykładzie 06 widzieliśmy, że
      bez niego związek długości z wysokością dzioba odwraca znak, co jest
      przykładem ", gloss("paradoks Simpsona", "paradoksu Simpsona"), "."),

    inline_callout(label = "Werdykt",
      "Dobry zbiór: drobne, jawne braki (11 z 344 wierszy) można usunąć,
      a jedynym poważnym zastrzeżeniem jest konieczność uwzględniania
      gatunku w analizach."),

    lc_chapter_next(
      num = "05",
      title = "Filmy Tarantino",
      lead = "Następny zbiór ma prawie dwa tysiące wierszy, ale wiersz nie
              jest w nim jednostką, którą chcemy porównywać.",
      target_id = "ch5"
    ),

    div(style = "height: 40px;")
  ))))

ch4_server <- function(input, output, session) {

  output$tab3_table <- renderUI({
    dd_data_table(round_df(penguins), page_size = 8, page = input$tab3_table_page, page_input = "tab3_table_page")
  })

  zoom_plot_server("tab3_missing", reactive({
    miss_pct <- sapply(penguins, function(x) mean(is.na(x)) * 100)
    df_miss <- data.frame(variable = names(miss_pct), pct = miss_pct)

    ggplot(df_miss, aes(x = reorder(variable, -pct), y = pct)) +
      geom_col(fill = data_primary) +
      labs( x = NULL, y = "% braków") +
      theme_upwr(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  }))

  zoom_plot_server("tab3_boxplot", reactive({
    req(input$tab3_var)
    ggplot(penguins %>% filter(!is.na(.data[[input$tab3_var]])),
           aes(x = species, y = .data[[input$tab3_var]], fill = species)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = c(data_primary, data_mixed, data_good)) +
      labs(x = "Gatunek", y = .ch4_var_labels[[input$tab3_var]]) +
      theme_upwr(base_size = 14) +
      theme(legend.position = "none")
  }))

}
