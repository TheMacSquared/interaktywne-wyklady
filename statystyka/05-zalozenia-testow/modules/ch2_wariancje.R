# ============================================================================
# CHAPTER 2: Jednorodność wariancji
# ============================================================================

ch2_ui <- lecture_chapter(
  id = "ch-wariancje",
  num = "02",
  title = "Jednorodne wariancje",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · Założenia testów",
      num    = "02",
      title  = "Jednorodne wariancje.",
      lead   = "Dwie grupy mogą mieć podobne średnie i zupełnie inny rozrzut. Część
                testów zakłada, że rozrzut jest wszędzie taki sam, ale zwykle mają
                wariant, który tego założenia nie potrzebuje."
    ),

    lc_p("Poprzedni rozdział dotyczył kształtu rozkładu. Testy porównujące grupy
      mają jeszcze jedno założenie, tym razem o rozrzucie: w wykładzie 04 test t
      w wersji Studenta i klasyczna ANOVA zakładały, że wariancje we wszystkich
      grupach są podobne. Ten rozdział wyjaśnia, skąd to założenie się bierze,
      jak wygląda jego naruszenie, jak je sprawdzać i co zrobić, gdy wariancje
      się różnią."),

    lc_h2("ch2-homoscedastycznosc", "Homoscedastyczność — równe wariancje"),

    lc_p("Założenie jednorodnych ", gloss("wariancja", "wariancji"), ", czyli ",
      gloss("homoskedastyczność", "homoscedastyczność"), ", mówi, że w populacji
      każda z porównywanych grup ma tę samą wariancję \\(\\sigma^2\\). Średnie
      mogą się różnić, rozrzut wokół nich ma być taki sam. Przeciwny przypadek,
      w którym wariancje się różnią, nazywamy ",
      gloss("heteroskedastyczność", "heteroscedastycznością"), "."),

    lc_p("Po co testowi takie założenie? Jeśli wariancje są równe, obie grupy
      szacują tę samą wielkość i można je połączyć w jedną, dokładniejszą
      wariancję wspólną. Tak działa ", gloss("test t", "test t"), " w wersji
      Studenta:"),

    lc_formula_box(withMathJax(
      "$$s_p^2 = \\frac{(n_1 - 1)\\,s_1^2 + (n_2 - 1)\\,s_2^2}{n_1 + n_2 - 2}, \\qquad
        t = \\frac{\\bar{x}_1 - \\bar{x}_2}{s_p \\sqrt{\\dfrac{1}{n_1} + \\dfrac{1}{n_2}}}$$"
    )),

    lc_p("Wariancja wspólna \\(s_p^2\\) to średnia ważona wariancji z grup, a wagami
      są ich stopnie swobody. Liczniejsza grupa ma więc większy wpływ na wynik.
      Gdy wariancje w populacji naprawdę się różnią, \\(s_p^2\\) nie szacuje
      niczego konkretnego, a błąd standardowy różnicy średnich wychodzi źle.
      Na tej samej zasadzie ", gloss("ANOVA"), " łączy wariancje wszystkich grup
      w mianowniku statystyki F. W ", gloss("regresja liniowa", "regresji liniowej"),
      " (wykład 06) to samo założenie dotyczy ", gloss("reszta", "reszt"), ": ich
      rozrzut ma być taki sam dla wszystkich wartości predyktora."),

    # ========================================================================
    # WIDGET 1: Wizualizacja
    # ========================================================================
    lc_h2("ch2-naruszenie", "Jak wygląda naruszenie?"),

    lc_p("Zanim sięgniemy po test, warto zobaczyć, jak różne wariancje wyglądają
      na wykresie i jak bardzo wariancje z próby wahają się nawet wtedy, gdy
      w populacji są równe. Panel losuje dwie grupy z rozkładów normalnych
      o średnich 170 i 175 i o odchyleniach standardowych ustawionych suwakami,
      a pod wykresem podaje odchylenia z próby i iloraz większej wariancji
      do mniejszej."),

    figure_panel(
      label = "Ryc. 2.1",
      title = "Dwie grupy o różnej wariancji",
      fluidRow(
        column(4,
          lc_slider("ch2_sd1", "SD grupy A", 2, 30, 10, 1),
          lc_slider("ch2_sd2", "SD grupy B", 2, 30, 10, 1),
          lc_slider("ch2_n_per", "n (na grupę)", 15, 100, 40, 5),
          lc_action("ch2_gen", "Generuj dane", variant = "solid")
        ),
        column(8,
          zoom_plot_ui("ch2_boxplot", height = "300px"),
          uiOutput("ch2_var_stats")
        )
      )
    ),

    lc_p("Naruszenie widać na wykresie pudełkowym od razu: przy odchyleniach 10
      i 30 jedno pudełko jest kilka razy wyższe od drugiego, a punkty jednej grupy
      rozlewają się daleko poza zakres drugiej. Trudniej ocenić przypadki
      pośrednie, bo wariancja z próby sama jest zmienną losową. Przy jednakowych
      odchyleniach w populacji i 15 obserwacjach w grupie iloraz większej
      wariancji z próby do mniejszej przekracza 2 w mniej więcej co piątym
      losowaniu. Przy 40 obserwacjach zdarza się to w około 3% losowań, przy 100
      praktycznie wcale. Wygeneruj dane kilka razy przy tych samych ustawieniach,
      a zobaczysz, że w małych próbach wyraźna różnica w rozrzucie może być
      dziełem przypadku."),

    # ========================================================================
    # WIDGET 2: Testy
    # ========================================================================
    lc_h2("ch2-testy", "Testy jednorodności wariancji"),

    lc_p("Ocena na oko nie mówi, czy różnicę w rozrzucie da się wytłumaczyć
      przypadkiem. Formalne testy jednorodności wariancji mają we wszystkich
      grupach tę samą hipotezę zerową:"),

    lc_formula_box(withMathJax(
      "$$H_0: \\sigma_1^2 = \\sigma_2^2 = \\ldots = \\sigma_k^2 \\qquad
        H_a: \\text{co najmniej jedna wariancja jest inna}$$"
    )),

    lc_p(gloss("test Levene'a", "Test Levene'a"), " zamienia każdą obserwację na
      jej odległość od środka własnej grupy i porównuje średnie tych odległości
      zwykłą ANOVA, stąd statystyka F. W klasycznej wersji środkiem grupy jest
      średnia, w wersji odpornej (Browna-Forsythe'a) mediana, co czyni test mało
      wrażliwym na skośność i wartości odstające. Programy statystyczne różnią
      się tym, którą wersję liczą domyślnie. Panel poniżej mierzy odległości
      od mediany. ",
      gloss("test Bartletta", "Test Bartletta"), " porównuje wariancje
      bezpośrednio i ma statystykę o rozkładzie w przybliżeniu χ². Gdy dane pochodzą z rozkładu
      normalnego, wykrywa różnice wariancji nieco łatwiej niż Levene, ale przy
      rozkładach skośnych lub o ciężkich ogonach odrzuca H₀ zbyt często,
      bo myli brak normalności z nierównymi wariancjami. Dlatego w praktyce
      częściej używa się testu Levene'a."),

    figure_panel(
      label = "Ryc. 2.2",
      title = "Levene i Bartlett",
      fluidRow(
        column(4,
          helpText("Panel testuje dane wygenerowane na Ryc. 2.1."),
          lc_action("ch2_test_var", "Testuj", variant = "solid")
        ),
        column(8,
          uiOutput("ch2_test_results")
        )
      )
    ),

    lc_p("Wynik testu wariancji zależy w dużej mierze od liczebności. Gdy
      w populacji odchylenia wynoszą 10 i 15, czyli wariancje różnią się
      ponad dwukrotnie, test Levene'a wykrywa tę różnicę w około 20% losowań
      przy 15 obserwacjach w grupie, w około 60% przy 40 i w około 95% przy 100.
      W małych próbach test ma więc małą ", gloss("moc testu", "moc"), " i często
      nie zauważa różnicy, która ma znaczenie dla testu t. W bardzo dużych
      próbach jest odwrotnie: odrzuca H₀ już przy różnicach tak małych, że
      żadnemu testowi nie szkodzą."),

    lc_p("Tak jak w wykładzie 04, brak podstaw do odrzucenia H₀ nie dowodzi,
      że H₀ jest prawdziwa. Nieistotny wynik testu Levene'a nie znaczy, że
      wariancje są równe, tylko że dane nie przemawiają wyraźnie przeciw temu.
      Test formalny warto czytać razem z wykresem i z ilorazem wariancji z próby,
      a nie jako automatyczny przełącznik między metodami."),

    # ========================================================================
    # WIDGET 3: Co robić?
    # ========================================================================
    lc_h2("ch2-nierowne", "Gdy wariancje są nierówne"),

    lc_p("Skoro test wariancji bywa zawodny, lepiej sięgnąć po metodę, która
      równych wariancji w ogóle nie wymaga. Dla dwóch grup jest nią ",
      gloss("test t Welcha", "test t Welcha"), " znany z wykładu 04. Każda grupa
      zachowuje w nim własną wariancję: błąd standardowy to
      \\(\\sqrt{s_1^2/n_1 + s_2^2/n_2}\\), a stopnie swobody liczy wzór
      Welcha–Satterthwaite'a:"),

    lc_formula_box(withMathJax(
      "$$df = \\frac{\\left(\\dfrac{s_1^2}{n_1} + \\dfrac{s_2^2}{n_2}\\right)^2}
        {\\dfrac{(s_1^2/n_1)^2}{n_1 - 1} + \\dfrac{(s_2^2/n_2)^2}{n_2 - 1}}$$"
    )),

    lc_p("Gdy wariancje i liczebności są podobne, wzór daje prawie \\(n_1 + n_2 - 2\\),
      czyli tyle co wersja Studenta. Im bardziej różnią się składniki \\(s_1^2/n_1\\)
      i \\(s_2^2/n_2\\), tym mniej stopni swobody, a więc ostrożniejsza decyzja. Panel liczy oba testy na
      danych z Ryc. 2.1."),

    figure_panel(
      label = "Ryc. 2.3",
      title = "Test t Studenta vs Welcha",
      fluidRow(
        column(4,
          helpText("Test t Studenta zakłada równe wariancje, test Welcha
                    tego nie zakłada."),
          lc_action("ch2_compare_t", "Porównaj testy", variant = "solid")
        ),
        column(8,
          uiOutput("ch2_t_comparison")
        )
      )
    ),

    lc_p("Statystyka t wychodzi w obu testach identyczna. To nie przypadek: przy
      równych liczebnościach grup oba wzory dają ten sam błąd standardowy.
      Różnią się tylko stopnie swobody, a z nimi p-wartość. Przy 40 obserwacjach
      w grupie wersja Studenta ma zawsze 78 stopni swobody, a Welch przy
      odchyleniach 10 i 30 zwykle w okolicach 48. Przy równych grupach wersja
      Studenta jest więc w dużej mierze odporna na nierówne wariancje:
      w symulacji z odchyleniami 10 i 30 i 40 obserwacjami w grupie, przy
      równych średnich, odrzuca prawdziwą H₀ w około 5,5% losowań, a Welch
      w około 5,2%. Ponieważ średnie w panelu zawsze różnią się o 5, oba testy przy
      domyślnych ustawieniach odrzucają H₀ w podobnej części losowań, około 60%."),

    lc_p("Kłopot pojawia się, gdy grupy mają różne liczebności, a tego panel
      nie pokazuje. W symulacji z 20 obserwacjami w grupie o odchyleniu 20
      i 80 obserwacjami w grupie o odchyleniu 5, przy równych średnich,
      test Studenta odrzuca prawdziwą H₀ w około 29% losowań zamiast w 5%.
      Wariancja wspólna jest zdominowana przez liczniejszą grupę o małym
      rozrzucie, więc błąd standardowy wychodzi za mały. Gdy odwrócimy układ
      i większy rozrzut ma liczniejsza grupa, test Studenta prawie nigdy nie
      odrzuca H₀ (około 0,1%) i traci moc. Test Welcha w obu układach trzyma
      się poziomu 5%."),

    lc_p("Dlatego coraz częściej zaleca się używanie testu Welcha domyślnie,
      bez wstępnego sprawdzania wariancji. Gdy wariancje są równe, Welch traci
      względem wersji Studenta niewiele mocy, a gdy się różnią, chroni przed
      dużymi błędami. Procedura dwuetapowa, w której wynik testu Levene'a
      decyduje o wyborze wersji testu t, dziedziczy słabości testu Levene'a:
      w małych próbach przepuszcza nierówne wariancje. Ten kurs przyjmuje
      stanowisko „Welch domyślnie”. Wersja Studenta nie jest jednak błędem,
      gdy grupy są równoliczne albo gdy plan badania daje dobre powody, by
      oczekiwać równych wariancji."),

    lc_p("Dla trzech i więcej grup odpowiednikiem jest ANOVA Welcha. Po niej
      naturalnie pasuje ",
      gloss("test Games-Howella", "test post hoc Games-Howella"), ", który też
      nie zakłada równych wariancji. Panel ANOVA w wykładzie 04 łączył klasyczną
      ANOVA z Games-Howellem; przy wyraźnie różnych wariancjach spójniejsza
      jest para ANOVA Welcha i Games-Howell. W regresji (wykład 06) nierówny
      rozrzut reszt obsługują odporne błędy standardowe."),

    lc_p(gloss("test Manna-Whitneya", "Test Manna-Whitneya"), ", ",
      gloss("test nieparametryczny", "test nieparametryczny"), ", bywa podawany
      jako lekarstwo na nierówne wariancje, ale nim nie jest. Porównuje rangi,
      a nie średnie, i przy różnym rozrzucie w grupach także myli się częściej,
      niż obiecuje α: w opisanym wyżej układzie 20 i 80 obserwacji odrzuca
      prawdziwą H₀ równych średnich w około 15% losowań. Jego miejsce wśród
      alternatyw pokazuje mapa w rozdziale 04."),

    inline_callout(label = "Zasada",
      "Wersję testu wybieraj przed analizą, na podstawie planu badania, a nie
       wyniku testu Levene'a. Gdy nie ma dobrych powodów, by zakładać równe
       wariancje, wybierz wariant Welcha."
    ),

    lc_chapter_next(
      num = "03",
      title = "Założenia χ², Fishera i korelacji",
      lead = "liczebności oczekiwane w tabelach, wybór między χ² a Fisherem i założenia korelacji.",
      target_id = "ch-chi-fisher"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  ch2_data <- reactiveVal(NULL)

  observeEvent(input$ch2_gen, {
    ch2_data(generate_two_groups(
      n1 = input$ch2_n_per, n2 = input$ch2_n_per,
      sd1 = input$ch2_sd1, sd2 = input$ch2_sd2
    ))
  })

  zoom_plot_server("ch2_boxplot", reactive({
    df <- ch2_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Generuj dane”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      ggplot(df, aes(x = group, y = value, fill = group)) +
        geom_boxplot(alpha = 0.6) +
        geom_jitter(width = 0.15, alpha = 0.3) +
        scale_fill_manual(values = c(col_test, col_alt)) +
        labs(x = "Grupa", y = "Wartość") +
        theme_upwr() +
        theme(legend.position = "none")
    }
  }))

  output$ch2_var_stats <- renderUI({
    df <- ch2_data()
    if (is.null(df)) return(NULL)
    stats <- df %>% group_by(group) %>%
      summarise(sd = sd(value), var = var(value), .groups = "drop")
    ratio <- max(stats$var) / min(stats$var)

    tagList(
      lc_stat_box("SD(A)", round(stats$sd[1], 2), color = col_test),
      lc_stat_box("SD(B)", round(stats$sd[2], 2), color = col_alt),
      lc_stat_box("Iloraz wariancji", round(ratio, 2),
                  color = if (ratio < 4) col_ok else col_fail)
    )
  })

  # --- Testy ---
  output$ch2_test_results <- renderUI({
    req(input$ch2_test_var)
    df <- isolate(ch2_data())
    if (is.null(df)) return(lc_feedback(type = "warning", "Najpierw wygeneruj dane."))

    lev <- rstatix::levene_test(df, value ~ group)
    bart <- bartlett.test(value ~ group, data = df)

    lev_color <- if (lev$p >= 0.05) col_ok else col_fail
    bart_color <- if (bart$p.value >= 0.05) col_ok else col_fail

    lc_feedback(type = "info",
      fluidRow(
        column(6,
          p(tags$strong("Test Levene'a:")),
          p(paste0("F = ", round(lev$statistic, 3))),
          p(paste0("p = ", format_p_value(lev$p))),
          p(style = paste0("color:", lev_color, "; font-weight: bold;"),
            if (lev$p >= 0.05) "Brak podstaw do odrzucenia H₀" else "Odrzucamy H₀: wariancje różne")
        ),
        column(6,
          p(tags$strong("Test Bartletta:")),
          p(paste0("χ² = ", round(bart$statistic, 3))),
          p(paste0("p = ", format_p_value(bart$p.value))),
          p(style = paste0("color:", bart_color, "; font-weight: bold;"),
            if (bart$p.value >= 0.05) "Brak podstaw do odrzucenia H₀" else "Odrzucamy H₀: wariancje różne")
        )
      )
    )
  })

  # --- Porównanie t ---
  output$ch2_t_comparison <- renderUI({
    req(input$ch2_compare_t)
    df <- isolate(ch2_data())
    if (is.null(df)) return(lc_feedback(type = "warning", "Najpierw wygeneruj dane."))

    t_classic <- t_test(df, value ~ group, var.equal = TRUE)
    t_welch <- t_test(df, value ~ group, var.equal = FALSE)

    lc_feedback(type = "info",
      fluidRow(
        column(6,
          p(tags$strong("Test t Studenta"), " (zakłada równe wariancje):"),
          p(paste0("t(", round(t_classic$df, 1), ") = ",
                   round(t_classic$statistic, 3))),
          p(paste0("p = ", format_p_value(t_classic$p)))
        ),
        column(6,
          p(tags$strong("Test Welcha"), " (nie zakłada równych wariancji):"),
          p(paste0("t(", round(t_welch$df, 1), ") = ",
                   round(t_welch$statistic, 3))),
          p(paste0("p = ", format_p_value(t_welch$p)))
        )
      )
    )
  })
}
