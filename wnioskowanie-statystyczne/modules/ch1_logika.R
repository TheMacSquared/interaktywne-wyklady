# ============================================================================
# CHAPTER 1: Logika testowania hipotez
# ============================================================================

ch1_ui <- list(
  id = "ch-logika", num = "01", title = "Logika testowania",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 01 · Testowanie hipotez",
      num    = "01",
      title  = "Logika testowania.",
      lead   = "„Czy telefon na biurku wpływa na koncentrację?” — statystyk zaczyna nie
                od wzorów, tylko od pytania. Najpierw zobaczymy, skąd bierze się
                potrzeba testu, a w kolejnym kroku nazwiemy hipotezy."
    ),

    # ========================================================================
    # SEKCJA 0: Case study otwierajacy
    # ========================================================================
    lc_h2("ch1-case", "Case study: telefon a koncentracja"),

    tagList(
      p("Wyobraźcie sobie następujący eksperyment na waszej uczelni
        (inspirowany badaniem Ward et al., 2017):"),
      tags$ul(
        tags$li("80 studentów losowo przydzielonych do dwóch grup po 40 osób"),
        tags$li(tags$b("Grupa A:"), " telefon schowany w plecaku"),
        tags$li(tags$b("Grupa B:"), " telefon leży na biurku (ekranem w dół, wyciszony)"),
        tags$li("Wszyscy rozwiązują ten sam test koncentracji (0–100 punktów)")
      ),
      p("Nikt nie używa telefonu. Różnica jest ",
        tags$em("tylko"), " w tym, czy telefon leży w zasięgu wzroku.")
    ),

    figure_panel(
      label = "Ryc. 1.1",
      title = "Wyniki eksperymentu",
      fluidRow(
        column(4,
          actionButton("ch1_case_generate", "Przeprowadź eksperyment",
                       class = "lc-btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch1_case_stats")
        ),
        column(8,
          plotOutput("ch1_case_plot", height = "350px")
        )
      )
    ),

    inline_callout(
      label = "Pytanie kluczowe",
      "Średnia w grupie „biurko” jest niższa. Ale czy to nie może być przypadek?
       Gdybyśmy powtórzyli eksperyment z innymi 80 osobami, różnica mogłaby być
       w drugą stronę. Testowanie hipotez rozstrzyga: czy obserwowana różnica
       jest zbyt duża, żeby być przypadkiem?",
      color = "uwaga"
    ),

    # ========================================================================
    # SEKCJA 1: Logika testowania z odniesieniem do case study
    # ========================================================================
    lc_h2("ch1-logika", "Testowanie hipotez — logika rozumowania"),

    tagList(
      p("Testowanie hipotez statystycznych przypomina proces sądowy:"),
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th("Element"), tags$th("Sąd"), tags$th("Nasz eksperyment z telefonem"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$b("H₀")),
            tags$td("Oskarżony jest niewinny"),
            tags$td("Telefon nie wpływa na koncentrację (różnica = 0)")
          ),
          tags$tr(
            tags$td(tags$b("Hₐ")),
            tags$td("Oskarżony jest winny"),
            tags$td("Telefon wpływa na koncentrację (różnica ≠ 0)")
          ),
          tags$tr(
            tags$td(tags$b("Dane")),
            tags$td("Dowody złożone w sądzie"),
            tags$td("Wyniki testu 80 studentów")
          )
        )
      ),
      p("Na razie najważniejsze jest samo uporządkowanie pytania: ",
        tags$b("jaki stan uznajemy za domyślny"),
        " i ",
        tags$b("co byłoby sygnałem efektu"),
        ". W następnym rozdziale zapiszemy to jako parę H₀/Hₐ. Dopiero potem
        wrócimy do błędów, p-wartości i formalnej decyzji.")
    ),

    lc_chapter_next(
      num       = "02",
      title     = "Od pytania do hipotezy",
      lead      = "najpierw nazywamy H₀ i Hₐ, a dopiero potem podejmujemy decyzję.",
      target_id = "ch-hipotezy"
    )

  )
)

# ============================================================================
# CHAPTER 3: Bledy, p-wartosc i decyzja
# ============================================================================

ch1d_ui <- list(
  id = "ch-decyzja", num = "03", title = "Błędy, p-wartość i decyzja",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Testowanie hipotez",
      num    = "03",
      title  = "Błędy, p-wartość i decyzja.",
      lead   = "Skoro mamy już H₀ i Hₐ, możemy zapytać, jakie błędy grożą przy decyzji, czym jest p-wartość i jak poprawnie sformułować werdykt."
    ),

    # ========================================================================
    # SEKCJA 1: Bledy I i II rodzaju
    # ========================================================================
    lc_h2("ch1-bledy", "Błędy I i II rodzaju"),

    tagList(
      p("Zanim nauczymy się podejmować decyzje w testach hipotez, musimy
        zauważyć coś fundamentalnego: ", tags$b("rzeczywistość i nasza decyzja
        to dwie różne rzeczy"), ". Test statystyczny daje nam werdykt —
        ale werdykt może się nie zgadzać z tym, co naprawdę jest w świecie."),
      p(tags$b("Analogia: alarm przeciwpożarowy.")),
      p("Wyobraź sobie czujnik dymu. W świecie są dwa możliwe stany: ",
        tags$em("pożar faktycznie trwa"), " albo ", tags$em("nic się nie pali"),
        ". Czujnik może podjąć dwie decyzje: ",
        tags$em("włączyć alarm"), " albo ", tags$em("milczeć"),
        ". To daje cztery kombinacje — dwie trafne i ", tags$b("dwa błędy"), ":"),
      tags$ul(
        tags$li(tags$b("Fałszywy alarm"), " — czujnik wyje, choć nie ma pożaru.
                Przykry, ale bezpieczny błąd."),
        tags$li(tags$b("Przegapiony pożar"), " — czujnik milczy, choć pali się
                naprawdę. Groźny błąd.")
      ),
      p("Zauważ, że nie możemy wyeliminować obu błędów jednocześnie:
        jeśli zwiększymy czułość czujnika (więcej fałszywych alarmów),
        rzadziej będzie przegapiał pożar. Jeśli zmniejszymy czułość
        (mniej fałszywych alarmów), częściej przegapi prawdziwy pożar.
        To ten sam kompromis, który pojawi się w testach statystycznych."),
      p("W testowaniu hipotez mamy dokładnie tę samą strukturę: dwa możliwe
        stany świata (H₀ prawdziwa / H₀ fałszywa) i dwie możliwe decyzje
        (nie odrzucamy / odrzucamy H₀). Cztery kombinacje — dwie dobre,
        dwie błędne:"),
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th(""), tags$th("H₀ prawdziwa"),
                  tags$th("H₀ fałszywa"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("Nie odrzucamy H₀")),
            tags$td(style = "background: var(--upwr-sage-tint);", "OK (trafna negacja)"),
            tags$td(style = "background: var(--upwr-accent-tint);", "Błąd II rodzaju (β)")
          ),
          tags$tr(
            tags$td(tags$strong("Odrzucamy H₀")),
            tags$td(style = "background: var(--upwr-accent-tint);", "Błąd I rodzaju (α)"),
            tags$td(style = "background: var(--upwr-sage-tint);", "OK (moc = 1−β)")
          )
        )
      ),
      p(tags$b("Błąd I rodzaju (α):"),
        " odrzucamy H₀, choć jest prawdziwa — fałszywy alarm.
        W analogii sądowej: skazujemy niewinnego. W nauce: publikujemy odkrycie,
        którego ", tags$em("nie"), " ma. Ryzyko tego błędu kontrolujemy sami,
        ustalając poziom istotności ", withMathJax("\\(\\alpha\\)"),
        " — zwykle 0,05 (5%)."),
      p(tags$b("Błąd II rodzaju (β):"),
        " nie odrzucamy H₀, choć jest fałszywa — przegapiony efekt.
        W analogii sądowej: uniewinniamy winnego. W nauce: nie wykrywamy
        realnej zależności. Ryzyko tego błędu (", withMathJax("\\(\\beta\\)"),
        ") zależy od wielkości efektu, rozrzutu danych i wielkości próby."),
      p(tags$b("Moc testu "), withMathJax("\\(1 - \\beta\\)"),
        ": prawdopodobieństwo wykrycia efektu, gdy ten ", tags$em("naprawdę"),
        " istnieje. Moc rośnie z: (1) większą próbą n, (2) większym efektem
        (różnicą rzeczywistą między grupami), (3) mniejszym rozrzutem w grupach."),
      lc_formula_box(
        p(withMathJax("\\(\\alpha\\)"),
          " = P(odrzucamy ", withMathJax("\\(H_0\\)"), " | ",
          withMathJax("\\(H_0\\)"), " prawdziwa)"),
        p(withMathJax("\\(\\beta\\)"),
          " = P(nie odrzucamy ", withMathJax("\\(H_0\\)"), " | ",
          withMathJax("\\(H_0\\)"), " fałszywa)"),
        p("Moc = ", withMathJax("\\(1 - \\beta\\)"),
          " = P(odrzucamy ", withMathJax("\\(H_0\\)"), " | ",
          withMathJax("\\(H_0\\)"), " fałszywa)")
      ),
      p(tags$b("Kompromis α–β:"),
        " te dwa ryzyka są ze sobą związane — zmniejszenie ",
        withMathJax("\\(\\alpha\\)"),
        " (np. z 0,05 do 0,01) redukuje fałszywe alarmy, ale zwiększa ryzyko
        przegapiania prawdziwych efektów (", withMathJax("\\(\\beta\\)"),
        " rośnie). Jedyny sposób, by zmniejszyć oba jednocześnie — zwiększyć n."),
      p("Konwencje: w badaniach stosuje się ",
        withMathJax("\\(\\alpha = 0{,}05\\)"),
        " i planuje próbę tak, by moc ", withMathJax("\\(1 - \\beta \\geq 0{,}80\\)"),
        " (czyli ", withMathJax("\\(\\beta \\leq 0{,}20\\)"),
        "). Poniżej możesz pobawić się tymi wartościami i zobaczyć, jak zmienia się
        obszar błędów dla różnych poziomów istotności, wielkości efektu i n.")
    ),

    div(style = "text-align: center; margin: 15px 0;",
      tags$img(src = "assets/type-error.jpg", style = "width: 100%; border-radius: 8px;")
    ),

    lc_h2("ch1-moc", "Wizualizacja α, β i mocy testu"),

    tagList(
      p("Żeby zobaczyć, co kryje się pod literami α i β, rozrysujmy ",
        tags$b("dwa rozkłady"), " obok siebie — rozkłady ",
        tags$em("średniej z próby"), ":"),
      tags$ul(
        tags$li(tags$b("Niebieski"), " — rozkład średniej, gdy H₀ jest prawdziwa
                (telefon nie ma wpływu; średnia populacyjna = wartość referencyjna)."),
        tags$li(tags$b("Burgundowy"), " — rozkład średniej, gdy Hₐ jest prawdziwa
                (telefon ", tags$em("naprawdę"), " wpływa o konkretną liczbę punktów).")
      ),
      p(tags$b("Punkt krytyczny"), " (czarna przerywana pionowa linia) to wartość
        na osi średnich, powyżej której odrzucamy H₀ — wynika on bezpośrednio z ",
        withMathJax("\\(\\alpha\\)"),
        ": to kwantyl rozkładu niebieskiego odcinający 5% w prawym ogonie."),
      p(tags$b("Cztery obszary na wykresie:")),
      tags$ul(
        tags$li(tags$b("α (szary w niebieskim, prawy ogon):"),
                " pole pod niebieskim rozkładem na prawo od punktu krytycznego
                — fałszywe alarmy, gdy H₀ jest prawdziwa."),
        tags$li(tags$b("1 − α (niebieski, lewa strona):"),
                " trafne negacje — H₀ prawdziwa i nie odrzucamy."),
        tags$li(tags$b("β (burgundowy, lewa strona punktu krytycznego):"),
                " pole pod burgundowym rozkładem po złej stronie — przegapione efekty."),
        tags$li(tags$b("Moc (zielony obszar, prawa strona):"),
                " pole pod burgundowym rozkładem po prawej stronie punktu krytycznego
                — trafne wykrycia efektu.")
      ),
      p("Przesuwając suwaki zauważysz kilka mechanik:"),
      tags$ul(
        tags$li("Zmniejszenie ", withMathJax("\\(\\alpha\\)"),
                " przesuwa punkt krytyczny w prawo → mniej fałszywych alarmów,
                ale ", tags$em("więcej"), " przegapionych efektów (β rośnie, moc spada)."),
        tags$li("Większa różnica średnich oddala od siebie oba rozkłady → moc rośnie,
                β maleje."),
        tags$li("Większa próba n zwęża oba rozkłady (błąd standardowy ∝ 1/√n) →
                znów rośnie moc.")
      )
    ),

    figure_panel(
      label = "Ryc. 3.1",
      title = "Moc testu i błędy",
      fluidRow(
        column(4,
          sliderInput("ch1_alpha", "α (poziom istotności):",
                      min = 0.01, max = 0.20, value = 0.05, step = 0.01)
        ),
        column(4,
          sliderInput("ch1_effect", "Różnica średnich (pkt):",
                      min = 0, max = 15, value = 7, step = 1)
        ),
        column(4,
          sliderInput("ch1_power_n", "n (na grupę):",
                      min = 10, max = 200, value = 40, step = 5)
        )
      ),
      plotOutput("ch1_power_plot", height = "380px"),
      uiOutput("ch1_power_stats")
    ),

    inline_callout(
      label = "Kompromis",
      tagList(
        "Zmniejszenie α redukuje błąd I rodzaju, ale zwiększa błąd II rodzaju. ",
        "Jedyny sposób na zmniejszenie obu naraz: ", tags$b("zwiększenie n"), "!"
      ),
      color = "uwaga"
    ),

    # ========================================================================
    # SEKCJA 2: P-wartość (po błędach — bo α jest już zdefiniowane)
    # ========================================================================
    lc_h2("ch1-pvalue", "Co to jest p-wartość?"),

    tagList(
      p("Wiemy już, że ryzyko błędu I rodzaju ", withMathJax("\\(\\alpha\\)"),
        " ustalamy sami — zwykle na 5%. Ale jak z danego eksperymentu wyciągnąć ",
        tags$em("decyzję"), ": odrzucić H₀ czy nie? Służy do tego ",
        tags$b("p-wartość"), "."),
      p("Eksperyment z telefonem dał pewną różnicę średnich między grupami.
        Czy to dowód, że telefon wpływa na koncentrację? A może gdybyśmy powtórzyli
        badanie z innymi studentami, różnica wyszłaby mniejsza, większa albo w drugą stronę?
        p-wartość formalizuje tę intuicję — mierzy, jak ",
        tags$em("zaskakująca"),
        " jest nasza obserwacja w świecie, w którym H₀ byłaby prawdziwa."),
      p(tags$b("Definicja formalna:")),
      lc_formula_box(
        p(withMathJax(
          "\\(p = P(|\\bar{X}_A - \\bar{X}_B| \\geq |d_{\\text{obs}}| \\mid H_0)\\)"
        )),
        p("czyli: prawdopodobieństwo zaobserwowania różnicy co najmniej tak
          skrajnej jak nasza (", withMathJax("\\(d_{\\text{obs}}\\)"), "), ",
          tags$em("gdyby H₀ była prawdziwa"), ".")
      ),
      p("„Co najmniej tak skrajnej” oznacza tu ",
        tags$em("w obie strony"), " — i na plus, i na minus. Hₐ z naszego eksperymentu
        mówi tylko, że telefon „wpływa” na koncentrację, bez wskazania kierunku,
        więc każde wystarczająco duże odchylenie od zera — w dół albo w górę —
        jest dla nas tak samo zaskakujące. Wariant jednostronny (gdy z góry zakładamy
        kierunek różnicy) pojawi się w następnym rozdziale, kiedy przyjrzymy się
        formułowaniu hipotez."),
      p(tags$b("Jak to obliczyć?"),
        " W praktyce używamy statystyki testowej (np. t, χ², F) i znanych rozkładów
        pod H₀ — ale dla intuicji najlepiej wyobrazić sobie, że ",
        tags$em("wielokrotnie powtarzamy eksperyment"),
        " w świecie, gdzie H₀ jest prawdziwa. Każdy powtórzony eksperyment da inną
        różnicę średnich — losowy szum. Rozkład tych różnic ",
        tags$b("pod H₀"), " pokazuje, co „normalne” bez żadnego efektu."),
      p(tags$b("Reguła decyzyjna:"),
        " jeśli ", withMathJax("\\(p < \\alpha\\)"), " — mówimy, że zaobserwowana
        różnica jest ", tags$em("zbyt skrajna"),
        ", by ją wytłumaczyć samym przypadkiem i odrzucamy H₀. W przeciwnym razie
        brak podstaw do odrzucenia — co ", tags$em("nie"), " znaczy „H₀ jest prawdziwa”,
        tylko „nasze dane jej nie wykluczają”."),
      p("Poniższy widget pokazuje tę intuicję wizualnie: symulujemy setki eksperymentów
        w świecie bez efektu i patrzymy, jak daleko od zera naprawdę „nasza” różnica wypada
        na tle rozkładu losowych różnic.")
    ),

    figure_panel(
      label = "Ryc. 3.2",
      title = "Powtórzone eksperymenty pod H₀",
      fluidRow(
        column(4,
          sliderInput("ch1_sim_n", "n (na grupę):",
                      min = 10, max = 100, value = 40, step = 5),
          hr(),
          lc_stack(gap = "md",
            actionButton("ch1_sim_10", "Powtórz 10 razy",
                         class = "lc-btn-primary", width = "100%"),
            actionButton("ch1_sim_200", "Powtórz 200 razy",
                         class = "lc-btn-warning", width = "100%"),
            actionButton("ch1_sim_reset", "Reset",
                         class = "lc-btn-secondary-outline", width = "100%")
          ),
          br(),
          uiOutput("ch1_sim_info")
        ),
        column(8,
          plotOutput("ch1_sim_plot", height = "350px"),
          uiOutput("ch1_sim_stats")
        )
      )
    ),

    inline_callout(
      label = "Jak to czytać",
      "Każdy słupek to różnica średnich z jednego symulowanego eksperymentu,
       w którym telefon nie ma wpływu. Czerwona linia to różnica z prawdziwego
       eksperymentu. p-wartość = jaki odsetek tych słupków jest co najmniej tak
       daleko od zera jak nasza czerwona linia?"
    ),

    figure_panel(
      label = "Ryc. 3.3",
      title = "Co naprawdę oznacza p-wartość?",
      p("Załóżmy, że w badaniu wyszło ", tags$b("p = 0,03"),
        ". Które zdanie jest poprawną interpretacją?"),
      radioButtons("ch1_pvalue_meaning", NULL,
        choices = c(
          "Jest 3% szans, że H₀ jest prawdziwa." = "h0_prob",
          "Jest 3% szans, że wynik jest przypadkowy." = "random_prob",
          "Gdyby H₀ była prawdziwa, taki lub bardziej skrajny wynik pojawiłby się w 3% powtórzeń." = "tail_prob"
        ),
        selected = character(0)
      ),
      uiOutput("ch1_pvalue_meaning_feedback")
    ),

    # ========================================================================
    # SEKCJA 3: Quiz - decyzja
    # ========================================================================
    lc_h2("ch1-decyzja", "Decyzja w praktyce"),

    tagList(
      p("Znamy już regułę porównania p-wartości z poziomem istotności.
        Ale sama decyzja „odrzucamy / nie odrzucamy” to nie wszystko —
        trzeba ją też poprawnie sformułować słowami. Dwa możliwe werdykty:"),
      p(tags$b("Jeśli "), withMathJax("\\(p < \\alpha\\)"), tags$b(":")),
      lc_formula_box(
        tags$em("„Na przyjętym poziomie istotności α odrzucamy hipotezę zerową
                na rzecz hipotezy alternatywnej.”")
      ),
      p(tags$b("Jeśli "), withMathJax("\\(p \\geq \\alpha\\)"), tags$b(":")),
      lc_formula_box(
        tags$em("„Na przyjętym poziomie istotności α nie mamy podstaw do
                odrzucenia hipotezy zerowej.”")
      ),
      p("Zwróć uwagę na szczegół w drugim werdykcie: mówimy ",
        tags$em("„nie mamy podstaw do odrzucenia”"), ", a ", tags$b("nie"),
        tags$em(" „H₀ jest prawdziwa”"),
        ". Brak dowodu to nie dowód braku — może efekt istnieje, ale nasza próba
        była za mała albo efekt za słaby, żeby go wykryć. Stąd tak ostrożny język."),
      p(tags$b("Ale to jeszcze nie koniec interpretacji."),
        " Werdykt o hipotezach to krok formalny — trzeba go dodatkowo ",
        tags$em("przetłumaczyć z powrotem na język pytania badawczego"),
        ". Jeśli H₀ brzmiała „średni czas dojazdu jest równy 30 minut”, a Hₐ
        „różni się od 30 minut”, to po odrzuceniu H₀ mówimy: ",
        tags$em("„średni czas dojazdu statystycznie istotnie różni się od 30 minut”"),
        " — a nie: „odrzuciliśmy hipotezę zerową”. W raportach i publikacjach
        zawsze wracamy do języka problemu."),
      p("Hipotezy umiemy już zapisać, teraz domykamy drugi ruch: jak wrócić
        z formalnego werdyktu do języka badania. Na razie poćwiczmy samo
        podjęcie decyzji na podstawie p-wartości:")
    ),

    figure_panel(
      label = "Ryc. 3.4",
      title = "Quiz: odrzucić czy nie?",
      uiOutput("ch1_quiz_scenario"),
      p("Twoja decyzja:"),
      uiOutput("ch1_quiz_options"),
      uiOutput("ch1_quiz_feedback"),
      actionButton("ch1_quiz_next", "Nowy scenariusz", class = "lc-btn-secondary-outline")
    ),

    lc_chapter_next(
      num       = "04",
      title     = "Test t jednej próby",
      lead      = "pierwszy konkretny test — średnia wobec wartości referencyjnej.",
      target_id = "ch-jedna-ilosciowa"
    )
  )
)


# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  # --- Sekcja 0: Case study ---
  ch1_case_data <- reactiveVal(NULL)

  observeEvent(input$ch1_case_generate, {
    ch1_case_data(generate_phone_data(40))
  })

  # Generuj dane na starcie
  observe({
    if (is.null(ch1_case_data())) {
      ch1_case_data(generate_phone_data(40))
    }
  })

  output$ch1_case_plot <- renderPlot({
    d <- ch1_case_data()
    if (is.null(d)) return(NULL)

    ggplot(d, aes(x = grupa, y = koncentracja, fill = grupa, color = grupa)) +
      geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5) +
      geom_jitter(width = 0.15, alpha = 0.5, size = 2) +
      scale_fill_manual(values = c(col_accept, col_pvalue)) +
      scale_color_manual(values = c(col_accept, col_pvalue)) +
      labs(title = "Wyniki testu koncentracji",
           x = NULL, y = "Wynik (0–100 pkt)") +
      theme(legend.position = "none") +
      coord_cartesian(ylim = c(20, 100))
  })

  output$ch1_case_stats <- renderUI({
    d <- ch1_case_data()
    if (is.null(d)) return(NULL)

    stats <- d %>%
      group_by(grupa) %>%
      summarise(m = round(mean(koncentracja), 1),
                s = round(sd(koncentracja), 1),
                .groups = "drop")

    diff_val <- round(stats$m[1] - stats$m[2], 1)

    tagList(
      lc_stat_box("Plecak", stats$m[1], " pkt (s=", stats$s[1], ")", color = col_accept),
      lc_stat_box("Biurko", stats$m[2], " pkt (s=", stats$s[2], ")", color = col_pvalue),
      lc_stat_box("Różnica", diff_val, " pkt", color = upwr_secondary)
    )
  })

  # Oblicz prawdziwa roznice z case study
  ch1_observed_diff <- reactive({
    d <- ch1_case_data()
    if (is.null(d)) return(0)
    means <- tapply(d$koncentracja, d$grupa, mean)
    unname(means["Telefon w plecaku"] - means["Telefon na biurku"])
  })

  # --- Widget 1: Histogram roznic z powtorzonych eksperymentow ---
  ch1_sim_diffs <- reactiveVal(numeric(0))

  do_simulations <- function(k) {
    n <- input$ch1_sim_n
    new_diffs <- sapply(seq_len(k), function(i) {
      g1 <- rnorm(n, mean = 70, sd = 13)
      g2 <- rnorm(n, mean = 70, sd = 13)
      mean(g1) - mean(g2)
    })
    ch1_sim_diffs(c(ch1_sim_diffs(), new_diffs))
  }

  observeEvent(input$ch1_sim_10, do_simulations(10))
  observeEvent(input$ch1_sim_200, do_simulations(200))
  observeEvent(input$ch1_sim_reset, ch1_sim_diffs(numeric(0)))

  output$ch1_sim_info <- renderUI({
    n_s <- length(ch1_sim_diffs())
    obs <- round(ch1_observed_diff(), 1)
    tagList(
      lc_stat_box("Eksperymentów", n_s, color = col_h0),
      lc_stat_box("Obs. różnica", obs, " pkt", color = col_reject)
    )
  })

  output$ch1_sim_plot <- renderPlot({
    diffs <- ch1_sim_diffs()
    obs <- ch1_observed_diff()

    if (length(diffs) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij „Powtórz” —\nsymulujemy eksperymenty bez efektu",
                 size = 5, color = upwr_reference) +
        theme_void()
    } else {
      df <- data.frame(diff = diffs, extreme = abs(diffs) >= abs(obs))
      ggplot(df, aes(x = diff, fill = extreme)) +
        geom_histogram(bins = 30, color = "white") +
        geom_vline(xintercept = obs, color = col_reject,
                   linewidth = 1.5, linetype = "solid") +
        geom_vline(xintercept = -obs, color = col_reject,
                   linewidth = 1, linetype = "dashed") +
        scale_fill_manual(values = c("TRUE" = col_pvalue, "FALSE" = col_h0),
                          labels = c("TRUE" = "co najmniej tak skrajne",
                                     "FALSE" = "bliżej zera"),
                          name = NULL) +
        labs(title = expression(paste("Różnice średnich z symulowanych eksperymentów (", H[0], " prawdziwa)")),
             subtitle = "Czerwona linia = prawdziwa różnica z eksperymentu",
             x = "Różnica średnich (grupa A − grupa B)", y = "Liczba") +
                theme(legend.position = "top")
    }
  })

  output$ch1_sim_stats <- renderUI({
    diffs <- ch1_sim_diffs()
    if (length(diffs) == 0) return(NULL)
    obs <- ch1_observed_diff()
    alpha <- input$ch1_alpha
    if (is.null(alpha)) alpha <- 0.05
    n_extreme <- sum(abs(diffs) >= abs(obs))
    pval <- n_extreme / length(diffs)
    tagList(
      lc_stat_box(
        "Błąd I",
        alpha * 100, "%",
        caption = paste0("p ≈ ", round(pval, 3),
                         " (", n_extreme, "/", length(diffs),
                         " eksperymentów co najmniej tak skrajnych)"),
        color = col_pvalue
      )
    )
  })

  output$ch1_pvalue_meaning_feedback <- renderUI({
    choice <- input$ch1_pvalue_meaning
    if (is.null(choice) || identical(choice, character(0))) return(NULL)

    if (identical(choice, "tail_prob")) {
      lc_feedback(type = "ok",
        tags$b("Tak."),
        " p-wartość zakłada, że H₀ jest prawdziwa, i pyta o częstość danych
        co najmniej tak skrajnych jak nasze."
      )
    } else {
      lc_feedback(type = "danger",
        tags$b("Nie."),
        " p-wartość nie mówi, jakie jest prawdopodobieństwo H₀ ani
        prawdopodobieństwo „przypadkowości” wyniku. To prawdopodobieństwo
        danych przy założeniu H₀."
      )
    }
  })

  # --- Widget 2: Moc testu ---
  output$ch1_power_plot <- renderPlot({
    alpha <- input$ch1_alpha
    diff_means <- input$ch1_effect  # roznica srednich w punktach
    n <- input$ch1_power_n
    sigma <- 13  # stale odchylenie std (ukryte)
    mu0 <- 70
    mu1 <- mu0 + diff_means

    x <- seq(mu0 - 4 * sigma / sqrt(n), max(mu1, mu0) + 4 * sigma / sqrt(n), length.out = 500)
    se <- sigma / sqrt(n)

    y_h0 <- dnorm(x, mean = mu0, sd = se)
    y_h1 <- dnorm(x, mean = mu1, sd = se)

    crit <- mu0 + qnorm(1 - alpha / 2) * se

    df_plot <- data.frame(
      x = rep(x, 2),
      y = c(y_h0, y_h1),
      dist = rep(c("H0: brak efektu", "Ha: telefon wpływa"), each = 500)
    )

    p <- ggplot(df_plot, aes(x = x, y = y, color = dist)) +
      geom_line(linewidth = 1.2) +
      geom_vline(xintercept = crit, linetype = "dashed", color = upwr_secondary) +
      scale_color_manual(values = c(col_h0, col_h1), name = "Rozkład") +
      labs(title = paste0("Moc testu (n = ", n, " na grupę, różnica = ", diff_means,
                          " pkt, alpha = ", alpha, ")"),
           x = "Średnia koncentracja w próbie", y = "Gęstość") +
      theme(legend.position = "top")

    # Shade rejection region under H0
    shade_h0 <- data.frame(x = x[x >= crit], y = y_h0[x >= crit])
    if (nrow(shade_h0) > 0) {
      p <- p + geom_area(data = shade_h0, aes(x = x, y = y),
                         fill = col_reject, alpha = 0.2, inherit.aes = FALSE)
    }

    # Shade power under H1
    shade_h1 <- data.frame(x = x[x >= crit], y = y_h1[x >= crit])
    if (nrow(shade_h1) > 0) {
      p <- p + geom_area(data = shade_h1, aes(x = x, y = y),
                         fill = col_accept, alpha = 0.2, inherit.aes = FALSE)
    }

    p
  })

  output$ch1_power_stats <- renderUI({
    alpha <- input$ch1_alpha
    diff_means <- input$ch1_effect
    n <- input$ch1_power_n
    sigma <- 13
    se <- sigma / sqrt(n)
    power <- pnorm(diff_means / se - qnorm(1 - alpha / 2))

    tagList(
      lc_stat_box("Moc", round(power * 100, 1), "%", color = col_accept),
      lc_stat_box("Błąd II", round((1 - power) * 100, 1), "%", color = upwr_secondary)
    )
  })

  # --- Widget 3: Quiz (tiles) ---
  ch1_quiz_data <- reactiveVal(NULL)
  ch1_quiz_answered <- reactiveVal(FALSE)
  ch1_quiz_selected <- reactiveVal(NULL)

  generate_quiz <- function() {
    scenarios <- list(
      list(p = 0.003, alpha = 0.05,
           context = "Badanie wpływu kawy na czas reakcji: p = 0.003, α = 0.05"),
      list(p = 0.12, alpha = 0.05,
           context = "Czy notatki ręczne dają lepsze wyniki niż na laptopie? p = 0.12, α = 0.05"),
      list(p = 0.048, alpha = 0.05,
           context = "Korelacja między ilością snu a oceną z egzaminu: p = 0.048, α = 0.05"),
      list(p = 0.06, alpha = 0.01,
           context = "Czy kierunek studiów wpływa na zarobki po 5 latach? ANOVA: p = 0.06, α = 0.01"),
      list(p = 0.001, alpha = 0.01,
           context = "Czy płeć wpływa na wybór specjalizacji? χ²: p = 0.001, α = 0.01"),
      list(p = 0.052, alpha = 0.05,
           context = "Porównanie skuteczności dwóch metod nauki: p = 0.052, α = 0.05")
    )
    ch1_quiz_data(scenarios[[sample(length(scenarios), 1)]])
    ch1_quiz_answered(FALSE)
    ch1_quiz_selected(NULL)
  }

  observe({ generate_quiz() })
  observeEvent(input$ch1_quiz_next, { generate_quiz() })

  output$ch1_quiz_scenario <- renderUI({
    sc <- ch1_quiz_data()
    if (is.null(sc)) return(NULL)
    lc_feedback(type = "info",
      p(tags$strong("Scenariusz:"), sc$context)
    )
  })

  ch1_quiz_choices <- list(
    list(letter = "A", value = "reject", text = "Odrzucamy H₀"),
    list(letter = "B", value = "fail_to_reject", text = "Brak podstaw do odrzucenia H₀")
  )

  output$ch1_quiz_options <- renderUI({
    ch1_quiz_data()
    if (ch1_quiz_answered()) return(NULL)
    div(class = "quiz-tiles quiz-cols-2",
      lapply(ch1_quiz_choices, function(opt) {
        actionButton(paste0("ch1_tile_", opt$value),
          tagList(
            div(class = "tile-letter", opt$letter),
            div(class = "tile-text", opt$text)
          ),
          class = "quiz-tile"
        )
      })
    )
  })

  observe({
    for (opt in ch1_quiz_choices) {
      local({
        val <- opt$value
        observeEvent(input[[paste0("ch1_tile_", val)]], {
          if (ch1_quiz_answered()) return()
          ch1_quiz_selected(val)
          ch1_quiz_answered(TRUE)
        }, ignoreInit = TRUE)
      })
    }
  })

  output$ch1_quiz_feedback <- renderUI({
    req(ch1_quiz_answered())
    sc <- ch1_quiz_data()
    answer <- ch1_quiz_selected()
    if (is.null(sc) || is.null(answer)) return(NULL)

    correct <- if (sc$p < sc$alpha) "reject" else "fail_to_reject"
    if (answer == correct) {
      lc_feedback(type = "ok",
        tags$strong("Poprawnie!"),
        p(paste0("p = ", sc$p, " ", ifelse(sc$p < sc$alpha, "<", "≥"),
                 " α = ", sc$alpha))
      )
    } else {
      lc_feedback(type = "danger",
        tags$strong("Nie! "),
        p(paste0("p = ", sc$p, " ", ifelse(sc$p < sc$alpha, "<", "≥"),
                 " α = ", sc$alpha, ". Zatem: ",
                 ifelse(correct == "reject", "odrzucamy H₀",
                        "brak podstaw do odrzucenia H₀"), "."))
      )
    }
  })
}
