# Tab 9: Badania laboratoryjne — błędy danych, zbiór mieszany

ch9_ui <- lecture_chapter(id = "ch9", num = "9", title = "Laboratorium", content = tagList(

  lc_chapter_hero(
    kicker = "Rozdział 09 · Co czyni dobry zbiór danych?",
    num    = "09",
    title  = "Badania laboratoryjne.",
    lead   = "Nie każda wartość odstająca jest błędem. Ten rozdział oddziela
              wartości niemożliwe od rzadkich, ale prawdziwych obserwacji."
  ),

  lc_h2("sec-01", "Opis"),

  lc_p("Zbiór zawiera wyniki badań laboratoryjnych 150 pacjentów: wiek, płeć,
    stężenie hemoglobiny, stężenie glukozy i ciśnienie skurczowe. Wyniki
    przepisywano ręcznie z papierowych kart do arkusza kalkulacyjnego.
    Chcemy sprawdzić, czy stężenie hemoglobiny zmienia się z wiekiem. To
    pytanie o zależność dwóch ", gloss("zmienna ilościowa", "zmiennych ilościowych"), ", na które w wykładzie 06
    odpowiadała ", gloss("regresja liniowa"), "."),

  lc_h2("sec-02", "Podgląd danych"),

  lc_p("Przeglądając tabelę, zwróć uwagę na wartości, które nie mieszczą się
    w zakresie możliwym dla danej wielkości."),

  figure_panel(
    label = "Ryc. 9.1",
    title = "Wyniki badań 150 pacjentów",
    uiOutput("tab8_table")
  ),

  lc_p("W 150 wierszach trudno wypatrzyć pojedyncze usterki, ale proste
    sprawdzenie zakresów zdradza je od razu. Najmniejsza wartość hemoglobiny
    to -14.2 g/dL, największa 1420 g/dL, a najstarszy pacjent ma 108 lat.
    Pozostałe zmienne też warto sprawdzić tą samą metodą: minimum i maksimum
    każdej kolumny, zanim policzymy cokolwiek innego."),

  lc_h2("sec-03", "Wiek a hemoglobina"),

  lc_p("Wykres rozrzutu z prostą regresji pokazuje, co dałaby analiza
    uruchomiona bez sprawdzenia danych. Zwróć uwagę na skalę osi pionowej."),

  figure_panel(
    label = "Ryc. 9.2",
    title = "Hemoglobina względem wieku, dane surowe",
    lc_plot("tab8_scatter_raw", ratio = "1.8/1", max_height = "350px")
  ),

  lc_p("Prawie wszystkie punkty zbijają się w wąski pas przy dole wykresu,
    bo oś musi pomieścić wartość 1420. Prosta dopasowana do surowych danych
    ma nachylenie -0.69 g/dL na rok, a ", gloss("współczynnik determinacji", "R²"),
    " wynosi 0.010: wiek wyjaśniałby 1% zmienności hemoglobiny. Oba wyniki
    opisują głównie jeden błędny wpis, a nie pacjentów. To problem, który
    w katalogu z rozdziału 1 nazwaliśmy błędami i literówkami w danych."),

  lc_h2("sec-04", "Szukanie wartości odstających"),

  lc_p("Pojedyncze podejrzane wartości najłatwiej wskazać na ",
    gloss("wykres pudełkowy", "wykresach pudełkowych"), ". Jako punkty
    zaznaczone są obserwacje leżące dalej niż 1.5 ",
    gloss("rozstęp międzykwartylowy", "IQR"), " od ", gloss("kwartyl", "kwartyli"), ", tak jak
    w wykładzie 01. Zwróć uwagę, które z zaznaczonych punktów są niemożliwe,
    a które tylko nietypowe."),

  figure_panel(
    label = "Ryc. 9.3",
    title = "Rozkłady czterech zmiennych ilościowych",
    lc_plots(
      lc_plot("tab8_box_hemoglobina", max_height = "260px"),
      lc_plot("tab8_box_glukoza", max_height = "260px"),
      lc_plot("tab8_box_wiek", max_height = "260px"),
      lc_plot("tab8_box_cisnienie", max_height = "260px")
    )
  ),

  lc_p("Wykresy wskazują osiem punktów: cztery dla hemoglobiny, dwa dla
    glukozy i po jednym dla wieku i ciśnienia. Reguła pudełka nie odróżnia
    błędu od rzadkiej wartości. Hemoglobina 18.7 g/dL jest zaznaczona tak samo
    jak -14.2 g/dL, choć pierwsza to wysoki, ale możliwy wynik, a druga nie
    może wystąpić. Zero w kolumnie hemoglobiny wygląda z kolei na kod braku
    danych wpisany jako liczba. O tym, co jest błędem, decyduje wiedza o tym,
    co mierzymy, a nie sam wykres."),

  lc_p("Panel poniżej usuwa sześć wierszy uznanych za błędy wpisywania
    i rysuje wykres rozrzutu jeszcze raz."),

  figure_panel(
    label = "Ryc. 9.4",
    title = "Hemoglobina względem wieku po usunięciu błędów",
    checkboxInput("tab8_clean", "Usuń podejrzane obserwacje", value = FALSE),
    conditionalPanel("input.tab8_clean",
      lc_plot("tab8_scatter_clean", ratio = "1.8/1", max_height = "350px")
    )
  ),

  lc_p("Po usunięciu sześciu wierszy zostaje 144 pacjentów, a obraz się
    zmienia. Nachylenie wynosi -0.048 g/dL na rok, czyli około 0.5 g/dL na
    dekadę, a R² rośnie z 0.010 do 0.231. Sześć błędnych wpisów, 4% wierszy,
    wystarczyło, żeby ukryć wyraźną zależność."),

  lc_p("Usuwanie całych wierszy nie jest jedynym wyjściem. Błąd w glukozie
    czy w ciśnieniu nie psuje hemoglobiny tego samego pacjenta, więc w takim
    przypadku można zamienić na ", gloss("braki danych", "brak danych"), " tylko błędną wartość i zachować
    resztę rekordu. Jeśli w oryginalnych kartach da się odnaleźć prawdziwy
    wynik, najlepiej go po prostu poprawić."),

  lc_note("Zasada", rule = TRUE,
    "Błąd danych popraw albo usuń. Prawdziwą ", gloss("wartość odstająca", "wartość odstającą"), "
     zostaw i sprawdź, jak wpływa na wynik."
  ),

  lc_h2("sec-05", "Ćwiczenie: błąd czy prawdziwa wartość odstająca?"),

  lc_p("Ta sama liczba może być błędem albo prawdziwą, choć rzadką obserwacją.
    Zależy to od zmiennej, od jej jednostki i od reszty rekordu. Poniżej
    pięć podejrzanych wpisów z tego zbioru. Oceń każdy z nich przed
    sprawdzeniem odpowiedzi."),

  figure_panel(
    label = "Ćwiczenie",
    title = "Błąd czy prawdziwa wartość odstająca?",
    uiOutput("tab8_quiz"),
    lc_action("tab8_check_quiz", "Sprawdź odpowiedzi", variant = "solid"),
    uiOutput("tab8_quiz_result")
  ),

  lc_p("Przy każdym wpisie pomaga to samo pytanie: czy taka wartość może
    wystąpić u żywego człowieka? Jeśli nie, mamy błąd i warto szukać jego
    źródła, na przykład zgubionej kropki dziesiętnej albo przypadkowego
    minusa. Jeśli tak, obserwacja zostaje w danych, nawet gdy bardzo odstaje
    od reszty."),

  lc_h2("sec-06", "Werdykt"),

  lc_p("Struktura zbioru jest dobra: 150 pacjentów, cztery zmienne ilościowe
    z jasnymi jednostkami i płeć (88 kobiet, 62 mężczyzn). Problemy wynikają
    z ręcznego przepisywania i dotyczą sześciu wpisów. Po ich poprawieniu
    albo usunięciu dane nadają się do regresji hemoglobiny względem wieku,
    do porównania kobiet i mężczyzn ", gloss("test t", "testem t"), " z wykładu 04 i do sprawdzenia
    założeń z wykładu 05. Glukozę 310 mg/dL zostawiamy: to prawdziwy pacjent,
    a usunięcie go byłoby ukrywaniem niewygodnych danych."),

  lc_note("Werdykt",
    "Zbiór dobry po czyszczeniu: błędy przepisywania trzeba znaleźć i poprawić,
     a prawdziwe wartości odstające zostawić."
  ),

  lc_chapter_next(
    num = "10",
    title = "Ankieta studencka",
    lead = "Po dwóch zbiorach z usterkami czas na wzorzec: ankietę, której dane
            nadają się do analizy bez czyszczenia.",
    target_id = "ch10"
  )
))

ch9_server <- function(input, output, session) {

  output$tab8_table <- renderUI({
    dd_data_table(round_df(lab_data), page_size = 10, page = input$tab8_table_page, page_input = "tab8_table_page")
  })

  error_rows <- c(3, 17, 28, 42, 55, 71)

  lab_clean <- reactive({
    if (input$tab8_clean) lab_data[-error_rows, ] else lab_data
  })

  zoom_plot_server("tab8_scatter_raw", reactive({
    model <- lm(hemoglobina ~ wiek, data = lab_data)
    r2    <- round(summary(model)$r.squared, 3)
    ggplot(lab_data, aes(x = wiek, y = hemoglobina)) +
      geom_point(alpha = 0.5, color = data_reference) +
      geom_smooth(method = "lm", color = data_bad, se = TRUE) +
      labs(
           x = "Wiek (lata)", y = "Hemoglobina (g/dL)") +
      theme_upwr(base_size = 14)
  }))

  make_boxplot <- function(var, label, unit = "") {
    ggplot(lab_data, aes(y = .data[[var]])) +
      geom_boxplot(fill = data_mixed, alpha = 0.7, width = 0.4) +
      labs(
           y = if (nchar(unit) > 0) paste0(label, " (", unit, ")") else label) +
      theme_upwr(base_size = 13) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

  zoom_plot_server("tab8_box_hemoglobina", reactive({ make_boxplot("hemoglobina", "Hemoglobina", "g/dL") }))
  zoom_plot_server("tab8_box_glukoza", reactive({ make_boxplot("glukoza",     "Glukoza",     "mg/dL") }))
  zoom_plot_server("tab8_box_wiek", reactive({ make_boxplot("wiek",        "Wiek",        "lata") }))
  zoom_plot_server("tab8_box_cisnienie", reactive({ make_boxplot("cisnienie",   "Ciśnienie skurczowe", "mmHg") }))

  zoom_plot_server("tab8_scatter_clean", reactive({
    d     <- lab_clean()
    model <- lm(hemoglobina ~ wiek, data = d)
    r2    <- round(summary(model)$r.squared, 3)
    ggplot(d, aes(x = wiek, y = hemoglobina)) +
      geom_point(alpha = 0.5, color = data_reference) +
      geom_smooth(method = "lm", color = data_good, se = TRUE) +
      labs(
           x = "Wiek (lata)", y = "Hemoglobina (g/dL)") +
      theme_upwr(base_size = 14)
  }))

  output$tab8_quiz <- renderUI({
    tagList(
      h4("Sklasyfikuj każdą podejrzaną obserwację: błąd danych czy prawdziwa wartość odstająca?"),
      lc_caption("Czytaj cały rekord, nie tylko podejrzaną liczbę."),
      tags$ol(
        tags$li(
          paste0("Hemoglobina: -14.2 g/dL | Wiek: ", lab_data$wiek[3], " lat | Płeć: ", lab_data$plec[3]),
          lc_segmented("tab8_q1", NULL, choices = c("Błąd danych", "Prawdziwa wartość odstająca"))
        ),

        tags$li(
          paste0("Hemoglobina: 1420 g/dL | Wiek: ", lab_data$wiek[17], " lat | Płeć: ", lab_data$plec[17]),
          lc_segmented("tab8_q2", NULL, choices = c("Błąd danych", "Prawdziwa wartość odstająca"))
        ),

        tags$li(
          paste0("Ciśnienie skurczowe: -70 mmHg | Wiek: ", lab_data$wiek[42], " lat | Płeć: ", lab_data$plec[42]),
          lc_segmented("tab8_q3", NULL, choices = c("Błąd danych", "Prawdziwa wartość odstająca"))
        ),

        tags$li(
          paste0("Glukoza: 11 000 mg/dL | Wiek: ", lab_data$wiek[28], " lat | Płeć: ", lab_data$plec[28]),
          lc_segmented("tab8_q4", NULL, choices = c("Błąd danych", "Prawdziwa wartość odstająca"))
        ),

        tags$li(
          paste0("Glukoza: 310 mg/dL | Wiek: ", lab_data$wiek[100], " lat | Płeć: ", lab_data$plec[100],
                 " | Hemoglobina: ", lab_data$hemoglobina[100], " g/dL"),
          lc_segmented("tab8_q5", NULL, choices = c("Błąd danych", "Prawdziwa wartość odstająca"))
      )
      )
    )
  })

  output$tab8_quiz_result <- renderUI({
    req(input$tab8_check_quiz > 0)
    isolate({
      answers <- c(input$tab8_q1, input$tab8_q2, input$tab8_q3, input$tab8_q4, input$tab8_q5)
      correct <- c("Błąd danych", "Błąd danych", "Błąd danych",
                   "Błąd danych", "Prawdziwa wartość odstająca")
      explanations <- c(
        "Ujemna hemoglobina jest fizycznie niemożliwa. Prawdopodobnie minus pojawił się przez błąd klawiatury lub importu. Błąd danych.",
        "Hemoglobina 1420 g/dL — norma to 12–17 g/dL. Zgubiona kropka dziesiętna: powinno być 14.20. Błąd danych (błąd zapisu).",
        "Ujemne ciśnienie tętnicze jest niemożliwe fizjologicznie. Znak minus musiał pojawić się przez błąd wprowadzania. Błąd danych.",
        "Glukoza 11 000 mg/dL — norma to 70–110 mg/dL, a nawet w śpiączce cukrzycowej rzadko przekracza 1000. Powinno być 110 mg/dL (3 zera za dużo). Błąd danych.",
        "Glukoza 310 mg/dL jest wysoka, ale medycznie możliwa — taki poziom zdarza się u pacjentów z niekontrolowaną cukrzycą. Reszta parametrów wygląda spójnie. Prawdziwa wartość odstająca: warto ją odnotować, ale nie usuwać."
      )

      items <- sapply(1:5, function(i) {
        ok   <- answers[i] == correct[i]
        mark <- if (ok) "<span class='lc-status-ok'>Dobrze.</span>" else
                        "<span class='lc-status-danger'>Źle.</span>"
        paste0("<div style='padding: 6px 0; border-bottom: 1px solid var(--upwr-rule);'>",
               "<b>Pyt. ", i, ":</b> ", mark, " ", explanations[i], "</div>")
      })

      score <- sum(answers == correct)
      lc_status(
        lc_verdict(type = if (score >= 4) "ok" else "warning", paste0("Wynik: ", score, "/5")),
        HTML(paste(items, collapse = ""))
      )
    })
  })

}
