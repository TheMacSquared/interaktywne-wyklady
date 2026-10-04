# Tab 6: Hotel — oceny hotelu boutique, brak zmienności

ch6_ui <- lecture_chapter(id = "ch6", num = "6", title = "Hotel", content = tagList(

  lc_chapter_hero(
    kicker = "Rozdział 06 · Co czyni dobry zbiór danych?",
    num    = "06",
    title  = "Oceny hotelu boutique.",
    lead   = "Osiemdziesiąt recenzji to rozsądna próba, ale liczba wierszy
              nie pomoże, jeśli zmienne prawie się nie różnią. Bez zmienności
              nie ma czego wyjaśniać ani porównywać."
  ),

  lc_h2("sec-01", "Opis"),

  lc_p("Portal rezerwacyjny zebrał opinie gości ekskluzywnego hotelu boutique
    po ostatnim sezonie. Zbiór ma 80 recenzji i pięć zmiennych. Właściciel
    chciałby wiedzieć, co wpływa na ocenę hotelu: czy goście z droższych
    pokojów są bardziej zadowoleni, czy dłuższy pobyt idzie w parze z inną
    ceną za noc, czy goście z różnych krajów oceniają hotel inaczej."),

  tags$ul(
    tags$li(tags$code("ocena_ogolna"), " — ocena hotelu na skali 1–5,"),
    tags$li(tags$code("typ_pokoju"), " — rodzaj pokoju (trzy kategorie),"),
    tags$li(tags$code("dlugosc_pobytu"), " — liczba nocy,"),
    tags$li(tags$code("cena_za_noc"), " — cena za noc w złotych,"),
    tags$li(tags$code("kraj_goscia"), " — kraj pochodzenia gościa.")
  ),

  lc_h2("sec-02", "Podgląd danych"),

  lc_p("Przeglądając tabelę, zwróć uwagę, jak często w kolejnych wierszach
    powtarzają się te same wartości."),

  figure_panel(
    label = "Tab. 6.1",
    title = "Hotel: 80 recenzji",
    uiOutput("tab5_table")
  ),

  lc_p("Na pierwszy rzut oka tabela wygląda porządnie: nie ma braków, typy
    zmiennych są jasne, a jeden wiersz to jeden gość. W tej samej kolumnie
    ciągle wracają jednak te same wartości: ocena 4 albo 5, Apartament
    Premium, Polska, jedna noc. Problem tego zbioru nie leży w błędach,
    tylko w tym, że zmienne prawie się nie zmieniają. Przejdziemy więc
    przez nie po kolei."),

  lc_h2("sec-03", "Zmienna 1: Ocena ogólna"),

  lc_p("Ocena ogólna jest zmienną, którą chcemy wyjaśnić, więc najpierw
    sprawdźmy, czy jej wartości w ogóle się różnią."),

  figure_panel(
    label = "Ryc. 6.1",
    title = "Rozkład oceny ogólnej",
    lc_plot("tab5_plot_zadowolenie", max_height = "300px")
  ),

  lc_p("73 z 80 gości, czyli 91%, wystawiło ocenę 4 albo 5. Ocenę 3 dało
    sześć osób, ocenę 1 jedna, a dwójki nie wystawił nikt. Skala 1–5
    działa tu w praktyce jak skala dwustopniowa. To problem z katalogu
    nazwany brak zmienności: jeśli prawie wszyscy odpowiadają tak samo,
    zmienna nie mówi, co różnicuje pobyty, a żaden predyktor nie ma czego
    wyjaśniać."),

  lc_h2("sec-04", "Zmienna 2: Typ pokoju"),

  lc_p("Przy zmiennej jakościowej, którą chcemy dzielić gości na grupy,
    patrzymy, czy każda grupa ma dość obserwacji do porównania."),

  figure_panel(
    label = "Ryc. 6.2",
    title = "Liczebność typów pokoju",
    lc_plot("tab5_plot_departament", max_height = "300px")
  ),

  lc_p("59 gości (74%) nocowało w Apartamencie Premium, 17 (21%) w pokoju
    standardowym, a tylko 4 (5%) w ekonomicznym. To ",
    gloss("niezbalansowane grupy"), ". Porównanie ocen między trzema typami
    pokojów opierałoby się w jednej grupie na czterech osobach, a przy
    tak małej grupie ", gloss("moc testu"), " jest bardzo niska.
    Dodatkowo w każdej grupie oceny są skupione przy 4 i 5, więc nawet
    przy lepszym balansie nie byłoby czego porównywać."),

  lc_h2("sec-05", "Zmienna 3: Długość pobytu"),

  lc_p("Przy zmiennej ilościowej patrzymy na rozpiętość wartości: czy
    obejmuje zakres, w którym może pojawić się jakaś zależność. Panel
    pozwala zobaczyć te same dane na osi obejmującej pobyty do dwóch
    tygodni."),

  figure_panel(
    label = "Ryc. 6.3",
    title = "Rozkład długości pobytu",
    lc_toolbar(
      lc_segmented("tab5_staz_view", NULL,
        choices = c("Dane" = "normal", "Pełna skala (1–14 nocy)" = "wide"))
    ),
    lc_plot("tab5_plot_staz", max_height = "300px")
  ),

  lc_p("Wszyscy goście zostali na 1–3 noce: 49 osób na jedną, 21 na dwie,
    10 na trzy. Mediana to jedna noc. Na pełnej skali cały zbiór mieści się
    w lewym rogu wykresu. Sama wąska rozpiętość nie jest błędem, bo tak może
    wyglądać klientela tego hotelu. Jest to jednak kolejna odmiana braku
    zmienności: gdy predyktor przyjmuje tylko trzy bliskie sobie wartości,
    wykrycie jego związku z czymkolwiek staje się bardzo trudne."),

  lc_h2("sec-06", "Zmienna 4: Cena za noc"),

  lc_p("Cena za noc to druga zmienna ilościowa; sprawdźmy, czy ona ma
    rozrzut."),

  figure_panel(
    label = "Ryc. 6.4",
    title = "Rozkład ceny za noc",
    lc_plot("tab5_plot_wynagrodzenie", max_height = "300px")
  ),

  lc_p("Tu obraz jest inny. Ceny wahają się od 208 do 641 zł, mediana
    wynosi 473.5 zł, a odchylenie standardowe około 83 zł. To jedyna zmienna
    w zbiorze z wyraźnym rozrzutem. Sama jednak nic nie wyjaśni: żeby coś
    z niej wynikało, musimy powiązać ją z inną zmienną, która też się
    zmienia."),

  lc_h2("sec-07", "Zmienna 5: Kraj gościa"),

  lc_p("Ostatnia zmienna jest jakościowa, więc znów patrzymy na liczebność
    grup."),

  figure_panel(
    label = "Ryc. 6.5",
    title = "Liczebność gości według kraju",
    lc_plot("tab5_plot_plec", max_height = "300px")
  ),

  lc_p("69 gości (86%) to turyści z Polski. Z Wielkiej Brytanii przyjechały
    4 osoby, z Niemiec 3, z Francji i z pozostałych krajów po 2. Tak jak
    przy typie pokoju, są to niezbalansowane grupy: porównanie krajów
    opierałoby się na garstce osób. W praktyce kraj gościa jest niemal
    stałą, czyli znów brakiem zmienności."),

  lc_h2("sec-08", "Próba szukania zależności"),

  lc_p("Cena za noc jest jedyną zmienną z rozrzutem, a jedynym ilościowym
    kandydatem do jej wyjaśnienia jest długość pobytu. Na wykresie
    rozrzutu zwróć uwagę, jaką część osi poziomej zajmują dane."),

  figure_panel(
    label = "Ryc. 6.6",
    title = "Cena za noc a długość pobytu",
    lc_plot("tab5_scatter", max_height = "300px")
  ),

  lc_p("Punkty układają się w trzy pionowe kolumny nad wartościami 1, 2 i 3.
    Współczynnik korelacji wynosi 0.09, a nachylenie prostej to około 11 zł
    na każdą dodatkową noc, przy czym pasmo ufności mieści zarówno wzrost,
    jak i spadek ceny. Prosta kończy się na trzeciej nocy, bo dalej nie ma
    danych, a reszta osi pozostaje pusta. Z tego wykresu nie wynika ani to,
    że zależność istnieje, ani to, że jej nie ma."),

  lc_h2("sec-09", "Gdyby dane miały większą zmienność"),

  lc_p("Brak związku w danych nie musi oznaczać braku związku w świecie.
    Panel poniżej symuluje hotel, w którym każda dodatkowa noc obniża cenę
    za noc średnio o 25 zł, czyli zależność jest wbudowana w dane. Suwak
    zwiększa rozrzut długości pobytu, od prawdziwych 1–3 nocy do około
    dwóch tygodni."),

  figure_panel(
    label = "Ryc. 6.7",
    title = "Ta sama zależność przy większym rozrzucie pobytów",
    lc_slider("tab5_sd_mult", "Mnożnik rozrzutu danych", 1, 5, 1, 0.5),
    lc_plot("tab5_scatter_sim", max_height = "300px")
  ),

  lc_p("Przy mnożniku 1 długość pobytu zostaje taka jak w danych i mimo
    wbudowanego efektu korelacja wynosi -0.06, czyli praktycznie zero.
    Przy mnożniku 3 pobyty sięgają około 8 nocy, a korelacja spada do -0.49.
    Przy mnożniku 5 pobyty sięgają prawie 14 nocy, a korelacja wynosi -0.76
    i spadek ceny widać gołym okiem. Efekt był cały czas taki sam, zmienił
    się tylko zakres predyktora. To samo obserwowaliśmy w wykładzie 06
    przy regresji: im węższy zakres zmiennej objaśniającej, tym słabiej
    dane pozwalają oszacować nachylenie."),

  lc_h2("sec-10", "Werdykt"),

  lc_p("W tym zbiorze nie ma błędów ani braków, a mimo to nie nadaje się
    on do szukania zależności. Zmienna, którą chcemy wyjaśnić, czyli
    ocena, jest skupiona przy maksimum. Typ pokoju i kraj gościa to
    skrajnie niezbalansowane grupy, a długość pobytu przyjmuje tylko trzy
    wartości. Jedyna zmienna z rozrzutem, cena za noc, nie ma partnera,
    z którym można by ją sensownie powiązać."),

  lc_p("Tego nie da się naprawić czyszczeniem, bo brakującej zmienności nie
    dopiszemy. Z tych danych można co najwyżej opisać klientelę hotelu
    w kategoriach z wykładu 01: rozkład ocen, udziały typów pokojów i krajów.
    Do pytania o to, co wpływa na ocenę, potrzebny byłby zbiór obejmujący
    gości o różnych doświadczeniach, na przykład z kilku hoteli albo
    z dłuższego okresu."),

  lc_note("Werdykt",
    "Zbiór nie nadaje się do analizy zależności: prawie wszystkie zmienne
    mają zbyt małą zmienność albo skrajnie niezbalansowane grupy."),

  lc_chapter_next(
    num = "07",
    title = "Wynagrodzenia",
    lead = "Dla porównania duży zbiór, w którym zmienność, liczebność grup
            i kompletność są takie, jak powinny.",
    target_id = "ch7"
  )
))

ch6_server <- function(input, output, session) {

  output$tab5_table <- renderUI({
    dd_data_table(round_df(hotel_data), page_size = 10, page = input$tab5_table_page, page_input = "tab5_table_page")
  })

  zoom_plot_server("tab5_plot_zadowolenie", reactive({
    ggplot(hotel_data, aes(x = factor(ocena_ogolna))) +
      geom_bar(fill = data_bad, alpha = 0.85) +
      scale_x_discrete(limits = c("1","2","3","4","5")) +
      labs(
        x = "Ocena ogólna", y = "Liczba gości"
      ) +
      theme_upwr(base_size = 14)
  }))

  zoom_plot_server("tab5_plot_departament", reactive({
    typ_counts <- hotel_data %>%
      count(typ_pokoju) %>%
      mutate(pct = round(100 * n / sum(n)),
             typ_pokoju = reorder(typ_pokoju, -n))
    ggplot(typ_counts, aes(x = typ_pokoju, y = n)) +
      geom_col(fill = data_bad, alpha = 0.85) +
      geom_text(aes(label = paste0(pct, "%")), vjust = -0.4, size = 4.5) +
      labs(
           x = "Typ pokoju", y = "Liczba gości") +
      theme_upwr(base_size = 14)
  }))

  tab5_staz_view <- reactive({
    v <- input$tab5_staz_view
    if (is.null(v)) "normal" else v
  })

  zoom_plot_server("tab5_plot_staz", reactive({
    med_pobytu <- median(hotel_data$dlugosc_pobytu)
    p <- ggplot(hotel_data, aes(x = dlugosc_pobytu)) +
      geom_bar(fill = data_mixed, alpha = 0.85, width = 0.6) +
      geom_vline(xintercept = med_pobytu, color = data_reference, linetype = "dashed", linewidth = 1) +
      annotate("text", x = med_pobytu, y = Inf, label = paste0("mediana = ", med_pobytu, " noc"),
               vjust = 2, hjust = -0.1, size = 4, color = data_reference) +
      scale_x_continuous(breaks = 1:14) +
      labs(
        x = "Długość pobytu (noce)", y = "Liczba gości"
      ) +
      theme_upwr(base_size = 14)
    if (tab5_staz_view() == "wide") p <- p + scale_x_continuous(limits = c(1, 14), breaks = seq(1, 14, 2))
    p
  }))

  zoom_plot_server("tab5_plot_wynagrodzenie", reactive({
    med_cena <- median(hotel_data$cena_za_noc)
    ggplot(hotel_data, aes(x = cena_za_noc)) +
      geom_histogram(bins = 15, fill = data_primary, color = "white", alpha = 0.85) +
      geom_vline(xintercept = med_cena, color = data_reference, linetype = "dashed", linewidth = 1) +
      annotate("text", x = med_cena, y = Inf, label = paste0("mediana = ", med_cena, " zł"),
               vjust = 2, hjust = -0.1, size = 4, color = data_reference) +
      labs(
        x = "Cena za noc (zł)", y = "Liczba gości"
      ) +
      theme_upwr(base_size = 14)
  }))

  zoom_plot_server("tab5_plot_plec", reactive({
    kraj_counts <- hotel_data %>%
      count(kraj_goscia) %>%
      mutate(pct = round(100 * n / sum(n)))
    ggplot(kraj_counts, aes(x = reorder(kraj_goscia, -n), y = n)) +
      geom_col(fill = data_bad, alpha = 0.85) +
      geom_text(aes(label = paste0(pct, "%  (n=", n, ")")), vjust = -0.4, size = 4.5) +
      labs(
           x = "Kraj gościa", y = "Liczba gości") +
      theme_upwr(base_size = 14)
  }))

  zoom_plot_server("tab5_scatter", reactive({
    ggplot(hotel_data, aes(x = dlugosc_pobytu, y = cena_za_noc)) +
      geom_point(alpha = 0.5, size = 3, color = data_reference) +
      geom_smooth(method = "lm", color = data_bad, se = TRUE) +
      scale_x_continuous(limits = c(1, 14), breaks = seq(1, 14, 2)) +
      labs(
           x = "Długość pobytu (noce)", y = "Cena za noc (zł)") +
      theme_upwr(base_size = 14)
  }))

  zoom_plot_server("tab5_scatter_sim", reactive({
    mult <- input$tab5_sd_mult
    set.seed(42)
    spread <- (mult - 1) * 3
    sim_pobytu <- pmax(1, hotel_data$dlugosc_pobytu + runif(hotel_n, -spread, spread))
    sim_cena   <- hotel_data$cena_za_noc - (sim_pobytu - mean(sim_pobytu)) * 25 +
                    rnorm(hotel_n, 0, 40)

    ggplot(data.frame(x = sim_pobytu, y = sim_cena), aes(x, y)) +
      geom_point(alpha = 0.5, size = 3, color = data_reference) +
      geom_smooth(method = "lm", color = data_primary, se = TRUE) +
      labs(
           x = "Długość pobytu (symulowane noce)", y = "Cena za noc (zł)") +
      theme_upwr(base_size = 14)
  }))

}
