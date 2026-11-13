# ============================================================================
# CHAPTER 3: Zmienna ilościowa w grupach zmiennej jakościowej
# ============================================================================

ch3_num_choices <- c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga",
                     "Średnia ocen" = "srednia_ocen",
                     "Czas dojazdu (min)" = "czas_dojazdu",
                     "Wydatki miesięczne (zł)" = "wydatki")
ch3_grp_choices <- c("Płeć" = "plec", "Kierunek" = "kierunek", "Rok studiów" = "rok_studiow")

ch3_ui <- list(
  id = "ch-grupy", num = "03", title = "Ilościowa i jakościowa",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Związki dwóch zmiennych",
      num    = "03",
      title  = "Liczby w grupach.",
      lead   = "Pensja to liczba, branża to kategoria. Czy branża mówi coś
                o pensji, sprawdzamy, porównując rozkłady pensji w kolejnych
                branżach: ich środki, rozrzut i to, jak bardzo na siebie
                zachodzą."
    ),

    lc_p("Gdy jedna zmienna jest ilościowa, a druga jakościowa, kategorie
      drugiej dzielą obserwacje na grupy. W każdej grupie zmienna ilościowa
      ma swój rozkład, a do jego opisu mamy już wszystkie narzędzia
      z wykładu 01: średnią, medianę, odchylenie standardowe, wykres
      pudełkowy. Związek polega na tym, że te rozkłady się różnią."),

    lc_h2("ch3-branze", "Pensje w trzech branżach"),

    figure_panel(
      label = "Ryc. 3.1",
      width_mode = "text",
      frames_widget("ch3_pay", "Ile zarabia absolwent?",
        steps = c("Pensje", "Według branży", "Typowa pensja", "Pudełka"),
        config = list(kind = "branze", names = branch_levels,
                      b = as.integer(pay_data$branza) - 1L, x = pay_data$pensja,
                      xmin = 3, xmax = 17,
                      stats = lapply(seq_len(nrow(pay_stats)), function(k) as.list(pay_stats[k, ])),
                      aria = "Pensje 90 absolwentów rozdzielają się na trzy branże; przy każdej pojawia się mediana i wykres pudełkowy"))
    ),

    lc_p("W kadrze 2 pensje rozdzieliły się na trzy ", gloss("rozkład warunkowy",
      "rozkłady warunkowe"), ", po jednym na branżę. Najprościej porównać je
      przez wartość typową: mediana pensji wynosi 5.8 tys. zł w branży rolniczej,
      7.7 tys. zł w biotechnologii i 9.5 tys. zł w branży kosmicznej. Różnica
      median między branżą kosmiczną a rolniczą, 3.7 tys. zł, to liczba, która
      opisuje siłę związku w tych danych. Można też porównać średnie: w branży
      kosmicznej średnia wynosi 10.0 tys. zł i jest wyższa od mediany, bo kilka
      bardzo wysokich pensji ciągnie ją w górę. Przy prawoskośnych zarobkach
      mediana lepiej opisuje typowego absolwenta."),

    lc_p(gloss("wykres pudełkowy", "Pudełka"), " z kadru 4 pokazują więcej niż
      środek. Pudełko branży rolniczej jest wąskie: połowa absolwentów zarabia
      tam od 5.2 do 6.2 tys. zł. W branży kosmicznej środkowa połowa rozciąga się
      od 7.8 do 11.1 tys. zł, więc pensje różnią się tam znacznie bardziej.
      Rozkłady branż się przy tym zachodzą: 11 z 30 absolwentów branży kosmicznej
      zarabia mniej niż najlepiej opłacany absolwent branży rolniczej
      (8.3 tys. zł)."),

    lc_warn("Pułapka",
      "Różnica median opisuje typowe osoby w grupach, nie każdą osobę. Zdanie
       „w branży kosmicznej zarabia się więcej” nie znaczy, że każdy
       absolwent tej branży zarabia więcej niż każdy absolwent rolnictwa."
    ),

    lc_h2("ch3-ankieta", "Grupy w ankiecie"),

    lc_p("Panel poniżej porównuje grupy w ankiecie studentów. Wybierz zmienną
      ilościową i zmienną, która dzieli studentów na grupy. Pudełka stoją
      obok siebie na wspólnej osi, a tabela pod wykresem podaje liczebność,
      średnią, medianę i odchylenie standardowe w każdej grupie."),

    figure_panel(
      label = "Ryc. 3.2",
      title = "Zmienna ilościowa w grupach",
      width_mode = "text",
      # Wykres, tabela i podpis liczą się w przeglądarce (survey_charts.js,
      # rycina „d02-box”).
      lc_sf_figure("d02-box", ratio = "2/1", max_height = "440px",
        config = list(num = as.list(setNames(names(ch3_num_choices), ch3_num_choices)),
                      grp = as.list(setNames(names(ch3_grp_choices), ch3_grp_choices))),
        control = list(
          lc_group("Zmienna",
            lc_select("ch3_num", ch3_num_choices, selected = "wzrost",
                      aria_label = "Zmienna ilościowa")),
          lc_group("Grupy",
            lc_select("ch3_grp", ch3_grp_choices, selected = "plec",
                      aria_label = "Zmienna dzieląca na grupy"))
        ),
        after = tagList(
          tags$div(`data-role` = "table"),
          tags$div(`data-role` = "caption")
        )
      )
    ),

    lc_p("Wzrost i płeć są wyraźnie związane: mediana wzrostu wynosi 166.0 cm
      u kobiet i 177.1 cm u mężczyzn, a pudełka prawie na siebie nie
      zachodzą. Tylko 3 z 99 kobiet są wyższe od mediany mężczyzn. Średnia ocen
      i kierunek studiów to przeciwny przypadek: mediany leżą między 3.75
      a 3.95, a pudełka stoją niemal w jednej linii. Znajomość kierunku prawie
      nic nie mówi o średniej ocen. W grupach osób niebinarnych i tych, które
      nie podały płci, jest tylko 13 i 6 osób, więc ich mediany mogą łatwo
      wypaść daleko od pozostałych bez żadnego związku z płcią."),

    lc_chapter_next(
      num       = "04",
      title     = "Dwie zmienne ilościowe",
      lead      = "staż, pensja i współczynnik korelacji.",
      target_id = "ch-korelacja"
    )
  )
)

ch3_server <- function(input, output, session) {

  scene_texts(input, output, "ch3_pay", list(
    tagList("Rok po studiach 90 absolwentów podaje swoją pensję brutto. Każda kropka
      to jedna osoba; połowa zarabia mniej niż 7.3 tys. zł."),
    tagList("Ci sami absolwenci pracują w trzech branżach: rolniczej,
      biotechnologicznej i kosmicznej. Kropki przesuwają się do swoich rzędów,
      a każda zostaje przy swojej pensji."),
    tagList("W każdej branży zaznaczamy medianę: pensję, którą połowa absolwentów
      przekracza, a połowa nie."),
    tagList("Pudełka obejmują środkową połowę pensji w każdej branży. Rozkłady
      różnią się położeniem i szerokością, ale wyraźnie na siebie zachodzą.")
  ))

  # Pudełka w grupach z ankiety (Ryc. 3.2) liczą się w przeglądarce:
  # survey_charts.js, rycina „d02-box”.
}
