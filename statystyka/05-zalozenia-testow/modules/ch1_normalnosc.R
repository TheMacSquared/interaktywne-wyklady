# ============================================================================
# CHAPTER 1: Normalność rozkładu
# ============================================================================

ch1_ui <- lecture_chapter(
  id = "ch-normalnosc",
  num = "01",
  title = "Normalność rozkładu",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 01 · Założenia testów",
      num    = "01",
      title  = "Normalność rozkładu.",
      lead   = "Dane z prawdziwych badań prawie nigdy nie układają się w idealny
                dzwon i nie muszą. Liczy się to, czy ich kształt może zepsuć wnioski
                z testu, a to najlepiej widać na wykresie."
    ),

    lc_p("Każdy test z wykładu 04 kończył się krótką listą założeń: test t, ANOVA
      i korelacja Pearsona zakładały rozkład zbliżony do normalnego, test t
      Studenta i klasyczna ANOVA — także równe wariancje w grupach, a test χ² —
      niezbyt małe liczebności oczekiwane. Wtedy przyjmowaliśmy je na wiarę.
      Ten wykład pokazuje, jak je sprawdzić i co zrobić, gdy nie są spełnione.
      Zaczynamy od normalności, bo to założenie pojawia się najczęściej i jest
      najczęściej źle rozumiane."),

    lc_h2("ch1-metody", "Które metody wymagają normalności?"),

    lc_p(gloss("test t", "Test t"), " nie analizuje pojedynczych obserwacji, tylko ich średnią.
      Założenie normalności jest mu potrzebne po to, żeby statystyka t miała
      rozkład t-Studenta, a to zależy od rozkładu średniej z próby. Jeśli dane
      pochodzą z ", gloss("rozkład normalny", "rozkładu normalnego"), ", średnia
      ma rozkład normalny przy każdej liczebności. Jeśli nie, z pomocą przychodzi ",
      gloss("centralne twierdzenie graniczne"), " z wykładu 02: rozkład średniej
      z próby zbliża się do normalnego, gdy próba rośnie, niezależnie od kształtu
      rozkładu wyjściowego."),

    lc_p("Tempo tego zbliżania zależy od kształtu danych. W wykładzie 02 widzieliśmy,
      że ", gloss("skośność"), " średniej maleje jak skośność rozkładu wyjściowego
      podzielona przez √n. Rozkład prawoskośny z panelu w następnej sekcji ma
      skośność 1,41, więc średnia z 10 obserwacji ma skośność 0,45, a średnia
      z 50 obserwacji — 0,20. Dla rozkładu symetrycznego skośność średniej jest
      zerowa od początku. Im bardziej skośny rozkład wyjściowy i im więcej w nim ",
      gloss("wartość odstająca", "wartości odstających"), ", tym większej próby
      potrzeba, żeby przybliżenie było dobre. Jednej liczby obserwacji, od
      której można przestać patrzeć na dane, nie ma."),

    lc_p("Co dokładnie sprawdzamy, zależy od metody:"),

    tags$ul(
      tags$li(strong("Test t jednej próby:"), " rozkład badanej zmiennej."),
      tags$li(strong("Test t dwóch grup:"), " rozkład zmiennej osobno w każdej
        grupie, a nie w połączonych danych. Dwie grupy o różnych średnich dają
        razem rozkład dwumodalny, nawet gdy każda z osobna jest normalna."),
      tags$li(strong(gloss("test t dla prób zależnych", "Test t sparowany"), ":"),
        " rozkład różnic w parach, a nie obu pomiarów osobno."),
      tags$li(strong(gloss("ANOVA"), ":"), " rozkład zmiennej w każdej grupie,
        czyli rozkład ", gloss("reszta", "reszt"), ": odchyleń obserwacji od
        średniej ich grupy."),
      tags$li(strong(gloss("korelacja Pearsona", "Korelacja Pearsona"), ":"),
        " sam współczynnik r można policzyć zawsze, ale test i przedział ufności
        zakładają, że obie zmienne razem mają rozkład normalny dwuwymiarowy.
        W praktyce oglądamy rozkład każdej zmiennej i wykres rozrzutu."),
      tags$li(strong(gloss("regresja liniowa", "Regresja liniowa"), ":"),
        " rozkład reszt, a nie samych zmiennych. Wrócimy do tego w wykładzie 06.")
    ),

    lc_p("Test t i ANOVA dobrze znoszą łagodne odchylenia od normalności, zwłaszcza
      gdy grupy mają podobne liczebności. Ta ", gloss("odporność"), " ma jednak
      granice: silna skośność albo kilka wartości odstających w małej próbie
      potrafią zmienić wynik. Dlatego pytanie nie brzmi „czy dane są normalne”,
      tylko „czy ich kształt jest na tyle daleki od normalnego, że zagraża
      wnioskom przy tej liczebności”. Odpowiedź zaczyna się od wykresu."),

    # ========================================================================
    # WIDGET 1: Wizualne sprawdzanie normalności
    # ========================================================================
    lc_h2("ch1-wizualnie", "Wizualne sprawdzanie normalności"),

    lc_p("Pierwszym narzędziem jest histogram z dorysowaną krzywą normalną o tej
      samej średniej i tym samym odchyleniu standardowym co dane. Pokazuje ogólny
      kształt, ale przy małej próbie jest poszarpany i zależy od szerokości
      przedziałów. Dokładniejszy jest ",
      gloss("wykres kwantyl-kwantyl"), " (Q-Q). Powstaje tak: obserwacje
      sortujemy od najmniejszej do największej i każdej przypisujemy miejsce,
      w którym powinna leżeć, gdyby dane pochodziły z rozkładu normalnego.
      Najmniejsza obserwacja z 50 powinna leżeć około 2,3 odchylenia
      standardowego poniżej średniej, środkowa — przy średniej, największa —
      około 2,3 odchylenia powyżej. Na osi poziomej są te oczekiwane położenia
      (kwantyle teoretyczne, w odchyleniach standardowych), na osi pionowej
      faktyczne wartości (kwantyle próbkowe). Każdy punkt to jedna obserwacja."),

    lc_p("Jeśli rozkład jest normalny, punkty układają się wzdłuż prostej. Prosta
      na wykresie przechodzi przez punkty odpowiadające pierwszemu i trzeciemu
      kwartylowi, więc dopasowuje się do środka danych, a odchylenia widać na
      końcach. Typowe wzory:"),

    tags$ul(
      tags$li(strong("Prawoskośność:"), " punkty tworzą łuk wygięty w dół, oba
        końce leżą nad prostą. Najmniejsze wartości są większe, niż przewiduje
        rozkład normalny (krótki lewy ogon), a największe znacznie większe
        (długi prawy ogon)."),
      tags$li(strong("Ciężkie ogony:"), " kształt litery S, lewy koniec pod
        prostą, prawy nad nią. Skrajnych wartości jest więcej i są dalej, niż
        przewiduje rozkład normalny."),
      tags$li(strong("Lekkie ogony:"), " odwrócone S, lewy koniec nad prostą,
        prawy pod nią. Tak wygląda rozkład jednostajny, w którym wartości
        skrajnych brakuje."),
      tags$li(strong("Dwa skupienia:"), " dwa płaskie odcinki rozdzielone
        stromym skokiem w środku wykresu."),
      tags$li(strong("Pojedyncza wartość odstająca:"), " jeden punkt daleko od
        prostej przy prawie idealnym ułożeniu reszty.")
    ),

    lc_p("Panel losuje próbę z wybranego rozkładu i rysuje obok siebie histogram
      i wykres Q-Q."),

    figure_panel(
      label = "Ryc. 1.1",
      title = "Histogram + Q-Q plot",
      fluidRow(
        column(4,
          selectInput("ch1_dist", "Rozkład danych:",
            choices = c(
              "Normalny" = "normal",
              "Prawoskośny" = "skewed",
              "Ciężkie ogony" = "heavy_tail",
              "Dwumodalny" = "bimodal",
              "Jednostajny" = "uniform"
            ),
            selected = "normal"
          ),
          lc_slider("ch1_n", "Wielkość próby (n)", 10, 200, 50, 10),
          lc_action("ch1_gen", "Generuj dane", variant = "solid")
        ),
        column(8,
          zoom_plot_ui("ch1_normality_plots", height = "350px")
        )
      )
    ),

    lc_p("Nawet dane wylosowane z rozkładu normalnego nie leżą idealnie na prostej.
      Na środku punkty trzymają się jej blisko, a na końcach odchylają się w obie
      strony, bo skrajnych obserwacji jest mało i każda z nich jest losowa.
      Po wyborze rozkładu prawoskośnego, o ciężkich ogonach, dwumodalnego albo
      jednostajnego pojawiają się opisane wyżej wzory. Przy n = 200 są wyraźne
      za każdym losowaniem. Przy n = 10 trudno je odróżnić od przypadkowych
      wahań, a histogram z kilkoma słupkami niewiele mówi. Wykres trzeba więc
      czytać jako cały wzór, a nie punkt po punkcie, i pamiętać, że przy małej
      próbie żadna metoda nie oceni kształtu rozkładu pewnie."),

    # ========================================================================
    # WIDGET 2: Testy normalności
    # ========================================================================
    lc_h2("ch1-testy-formalne", "Test formalny jako pomoc"),

    lc_p("Ocena wykresu jest subiektywna, dlatego kusi, żeby zastąpić ją liczbą. ",
      gloss("test Shapiro-Wilka", "Test Shapiro-Wilka"), " sprawdza hipotezę
      zerową, że dane pochodzą z rozkładu normalnego. Jego statystyka W mierzy,
      jak blisko prostej leżą punkty wykresu Q-Q: wartość 1 oznacza idealną
      zgodność, a im mniejsza, tym większe odchylenie. Mała p-wartość jest
      sygnałem, że kształt danych odbiega od normalnego. Panel poniżej liczy
      test dla danych wylosowanych w poprzednim panelu."),

    figure_panel(
      label = "Ryc. 1.2",
      title = "Shapiro–Wilk — wynik obok Q-Q plotu",
      fluidRow(
        column(4,
          helpText("Używa danych z widgetu powyżej."),
          lc_action("ch1_test_norm", "Policz test Shapiro–Wilka", variant = "solid")
        ),
        column(8,
          uiOutput("ch1_norm_results")
        )
      )
    ),

    lc_p("Wynik pojedynczego losowania niewiele mówi o samym teście, więc
      sprawdziliśmy go na 5000 próbach dla każdego rozkładu z panelu, przy
      α = 0,05. Dla danych normalnych test odrzuca H₀ w 5% prób, zgodnie
      z poziomem istotności. Przy n = 50 wykrywa rozkład prawoskośny w 95% prób,
      jednostajny w 74%, a rozkład o ciężkich ogonach w 63%. Przy n = 10 te same
      odsetki spadają do 24%, 9% i 19%: test w większości prób nie zauważa
      odchylenia, które przy małej próbie jest najgroźniejsze. Przy n = 200
      wykrywa każde z nich prawie zawsze, także rozkład jednostajny, który dla
      testu t przy takiej liczebności nie stanowi żadnego problemu, bo jest
      symetryczny i nie ma wartości odstających."),

    lc_p("Test normalności ma więc tę samą słabość co każdy test istotności: jego
      wynik zależy od liczebności. Przy małej próbie ma niską ",
      gloss("moc testu", "moc"), " i przepuszcza poważne odchylenia, przy dużej
      wykrywa odchylenia bez praktycznego znaczenia. Jak w wykładzie 04, brak
      podstaw do odrzucenia H₀ nie dowodzi, że H₀ jest prawdziwa: p > 0,05
      w teście Shapiro-Wilka nie znaczy, że dane są normalne. Test odpowiada
      też na inne pytanie niż to, które nas interesuje. Sprawdza, czy rozkład
      jest dokładnie normalny, a nas obchodzi, czy jest wystarczająco bliski
      normalnemu dla wybranej metody."),

    inline_callout(
      label = "Zasada",
      "O normalności decyduj na podstawie wykresu Q-Q, wartości odstających,
       liczebności i wymagań metody. Test Shapiro-Wilka traktuj jako uzupełnienie
       wykresu, a nie rozstrzygnięcie."
    ),

    # ========================================================================
    # WIDGET 3: Co robić, gdy naruszone?
    # ========================================================================
    lc_h2("ch1-naruszenia", "Gdy normalność jest naruszona"),

    lc_p("Jeśli wykres pokazuje wyraźne odchylenie, najpierw trzeba ustalić jego
      źródło. Pojedyncza wartość odstająca bywa błędem wprowadzania danych albo
      pomiaru i wtedy poprawia się dane, a nie metodę. Wartości odstającej, która
      jest prawdziwą obserwacją, nie usuwa się tylko dlatego, że psuje wykres.
      Łagodna skośność przy umiarkowanej liczebności zwykle nie wymaga zmiany
      analizy, bo test t i ANOVA są na nią odporne. Gdy odchylenie jest silne,
      a próba mała, są dwie drogi: przekształcić dane albo użyć testu, który
      normalności nie zakłada."),

    lc_p("Pierwsza droga to ", gloss("transformacja logarytmiczna"), ". Logarytm
      ściska duże wartości silniej niż małe, więc skraca długi prawy ogon. Działa
      tylko dla danych dodatnich i ma sens tam, gdzie naturalne są porównania
      względne: dochody, ceny, stężenia, czasy. Panel losuje dane prawoskośne
      z rozkładu gamma (przesuniętego o 1, żeby wszystkie wartości były dodatnie)
      i zestawia wykres Q-Q danych surowych (po lewej) z wykresem Q-Q ich
      logarytmów (po prawej)."),

    figure_panel(
      label = "Ryc. 1.3",
      title = "Efekt transformacji logarytmicznej",
      fluidRow(
        column(4,
          helpText("Generujemy dane prawoskośne i je logarytmujemy."),
          lc_slider("ch1_trans_n", "n", 30, 200, 80, 10),
          lc_action("ch1_transform", "Generuj i transformuj", variant = "solid")
        ),
        column(8,
          zoom_plot_ui("ch1_transform_plots", height = "300px")
        )
      )
    ),

    lc_p("Na lewym wykresie widać łuk typowy dla prawoskośności. Po logarytmowaniu
      punkty leżą znacznie bliżej prostej. Teoretyczna skośność spada z 1,41
      do −0,30, czyli logarytm nie tylko usunął prawy ogon, ale lekko przechylił
      rozkład w drugą stronę: przy większym n końce prawego wykresu mogą
      układać się nieco pod prostą. Transformacja nie gwarantuje więc rozkładu
      normalnego, tylko zmienia jego kształt."),

    lc_p("Ma też koszt interpretacyjny. Po logarytmowaniu test porównuje średnie
      logarytmów, a różnica średnich logarytmów odpowiada ilorazowi, a nie
      różnicy, na skali wyjściowej. Wynik trzeba więc opisać jako różnicę
      względną („o 20% więcej”), nie bezwzględną. Dlatego transformację stosuje
      się wtedy, gdy porównanie względne ma sens merytoryczny, a nie tylko po to,
      żeby wykres Q-Q wyglądał lepiej."),

    lc_p("Druga droga to ", gloss("test nieparametryczny", "testy nieparametryczne"),
      ". Zamiast surowych wartości analizują one ", gloss("ranga", "rangi"), ",
      czyli pozycje obserwacji po posortowaniu, więc wartości odstające i długie
      ogony nie mają na nie większego wpływu niż inne obserwacje. Każdy test
      z wykładu 04 dla zmiennej ilościowej ma swój odpowiednik rangowy:
      testy t jednej próby i dla par — ", gloss("test Wilcoxona"),
      ", test t dwóch grup — ", gloss("test Manna-Whitneya"), ", ANOVA — ",
      gloss("test Kruskala-Wallisa"), ", korelacja Pearsona — ",
      gloss("korelacja Spearmana"), ". Odpowiadają jednak na trochę inne pytanie
      niż testy, które zastępują. Porównują położenie całych rozkładów albo
      rang, a nie średnie, więc wynik opisuje się inaczej."),

    lc_p("Test Wilcoxona dla jednej próby i dla par też ma założenie: rozkład
      (w teście dla par — rozkład różnic) powinien być symetryczny. Przy silnej
      skośności nie rozwiązuje więc problemu automatycznie. Mapa w rozdziale 04
      zestawia wszystkie metody z ich założeniami i alternatywami. Normalność
      nie jest jednak jedynym założeniem testu t dwóch grup i ANOVA. Drugim
      jest podobny rozrzut w porównywanych grupach."),

    lc_chapter_next(
      num = "02",
      title = "Jednorodne wariancje",
      lead = "założenie równego rozrzutu między porównywanymi grupami.",
      target_id = "ch-wariancje"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  ch1_data <- reactiveVal(NULL)

  observeEvent(input$ch1_gen, {
    ch1_data(generate_test_data(input$ch1_n, input$ch1_dist))
  })

  zoom_plot_server("ch1_normality_plots", reactive({
    x <- ch1_data()
    if (is.null(x)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Generuj dane”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      df <- data.frame(x = x)

      p1 <- ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 20,
                       fill = col_test, alpha = 0.6, color = "white") +
        stat_function(fun = dnorm, args = list(mean = mean(x), sd = sd(x)),
                      color = col_ok, linewidth = 1.2, linetype = "dashed") +
        labs(
             x = "Wartość", y = "Gęstość") +
        theme_upwr()

      p2 <- ggplot(df, aes(sample = x)) +
        stat_qq(color = col_test, alpha = 0.6) +
        stat_qq_line(color = col_ok, linewidth = 1) +
        labs(
             x = "Kwantyle teoretyczne", y = "Kwantyle próbkowe") +
        theme_upwr()

      gridExtra::arrangeGrob(p1, p2, ncol = 2)
    }
  }))

  # --- Widget 2: Testy normalności ---
  output$ch1_norm_results <- renderUI({
    req(input$ch1_test_norm)
    x <- isolate(ch1_data())
    if (is.null(x)) return(lc_feedback(type = "warning", "Najpierw wygeneruj dane."))

    sw <- shapiro_test(data.frame(value = x), value)
    sw_color <- if (sw$p >= 0.05) col_ok else col_fail
    w_txt <- gsub(".", ",", formatC(sw$statistic, format = "f", digits = 3),
                  fixed = TRUE)

    lc_feedback(type = "info",
      p(tags$strong("Shapiro–Wilk:"), " W = ", w_txt,
        ", p = ", format_p_value(sw$p)),
      p(style = paste0("color:", sw_color, ";"),
        if (sw$p >= 0.05) {
          "Test nie wykrył odstępstwa. To nie dowodzi, że rozkład jest normalny."
        } else {
          "Test wykrył odstępstwo. Jego rodzaj i wagę oceń na wykresie Q-Q."
        })
    )
  })

  # --- Widget 3: Transformacja ---
  ch1_trans_data <- reactiveVal(NULL)

  observeEvent(input$ch1_transform, {
    x <- rgamma(input$ch1_trans_n, shape = 2, scale = 5) + 1
    ch1_trans_data(x)
  })

  zoom_plot_server("ch1_transform_plots", reactive({
    x <- ch1_trans_data()
    if (is.null(x)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Generuj i transformuj”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      log_x <- log(x)

      p1 <- ggplot(data.frame(x = x), aes(sample = x)) +
        stat_qq(color = col_fail, alpha = 0.5) +
        stat_qq_line(color = col_fail) +
        labs(x = "Kwantyle teoretyczne", y = "Kwantyle próbkowe") +
        theme_upwr()

      p2 <- ggplot(data.frame(x = log_x), aes(sample = x)) +
        stat_qq(color = col_ok, alpha = 0.5) +
        stat_qq_line(color = col_ok) +
        labs(x = "Kwantyle teoretyczne", y = "Kwantyle próbkowe") +
        theme_upwr()

      gridExtra::arrangeGrob(p1, p2, ncol = 2)
    }
  }))
}
