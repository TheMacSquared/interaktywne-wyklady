# ============================================================================
# CHAPTER 2: Formulowanie hipotez statystycznych
# ============================================================================

ch2h_ui <- list(
  id = "ch-hipotezy", num = "02", title = "Od pytania do hipotezy",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 02 · Wnioskowanie statystyczne",
      num    = "02",
      title  = "Od pytania do hipotezy.",
      lead   = "„Wydaje mi się, że coś z nimi jest nie tak — co robić?” Sformalizowanie
                potocznego pytania w parę H₀ / Hₐ to pierwszy krok każdego testu.
                Ćwiczymy tłumaczenie z języka codziennego."
    ),

    h2(id = "ch2h-zasada", class = "section-title", "Zasada: od potocznego do formalnego"),

    div(class = "narrative",
      p("W badaniach pytania są formułowane swobodnym językiem:"),
      tags$ul(
        tags$li(em("\"Czy mężczyźni są wyżsi od kobiet?\"")),
        tags$li(em("\"Czy korepetycje pomagają?\"")),
        tags$li(em("\"Czy lodów sprzedaje się więcej w ciepłe dni?\""))
      ),
      p("Ale test statystyczny wymaga formalnych hipotez:
        precyzyjnych stwierdzeń o parametrach populacji, które można
        zweryfikować danymi."),
      p("Najważniejsza zasada: H₀ zawsze zawiera znak równości
        (=, ≤, ≥). Hₐ zawiera to, co chcemy wykazać (≠, >, <).")
    ),

    margin_callout(
      label = "Szablon",
      tagList(
        tags$p(withMathJax("\\(H_0\\)"), ": brak efektu / brak różnicy / brak związku"),
        tags$p(withMathJax("\\(H_a\\)"), ": jest efekt / jest różnica / jest związek")
      )
    ),

    # ========================================================================
    # WIDGET 1: Pytanie badawcze -> hipoteza (galeria przykladow)
    # ========================================================================
    h2(id = "ch2h-galeria", class = "section-title", "Galeria przykładów: pytanie → hipoteza"),

    div(class = "narrative",
      p("Przeglądaj przykłady — każdy pokazuje, jak przejść
        od potocznego pytania do formalnych hipotez.")
    ),

    figure_panel(
      label = "Ryc. 2.1",
      title = "Przykłady formułowania hipotez",
      fluidRow(
        column(4,
          selectInput("ch2h_example", "Wybierz przykład:",
            choices = c(
              "Telefon a koncentracja" = "ex1",
              "Nawóz a plony" = "ex2",
              "Szkolenie BHP a wypadki" = "ex3",
              "Klasy gleby a norma" = "ex4",
              "Azotany wzdłuż rzeki" = "ex5",
              "Zagospodarowanie a strefa miasta" = "ex6",
              "Opakowanie a trwałość jogurtu" = "ex7",
              "Zużycie wody wobec normy" = "ex8",
              "Jakość wody > 80% normy?" = "ex9",
              "Sen a ocena z egzaminu" = "ex10"
            ),
            selected = "ex1"
          )
        ),
        column(8,
          uiOutput("ch2h_example_display")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Cwiczenie - sformuluj hipoteze (potoczne -> formalne)
    # ========================================================================
    h2(id = "ch2h-cwiczenie", class = "section-title", "Ćwiczenie: sformułuj hipotezę"),

    div(class = "narrative",
      p("Dostajesz pytanie badawcze w języku potocznym.
        Twój cel: wybrać poprawną parę H₀/Hₐ.")
    ),

    figure_panel(
      label = "Ryc. 2.2",
      title = "Quiz: pytanie → hipoteza",
      uiOutput("ch2h_quiz_question"),
      uiOutput("ch2h_quiz_options"),
      uiOutput("ch2h_quiz_feedback"),
      actionButton("ch2h_quiz_next", "Następne pytanie", class = "btn-outline-secondary")
    ),

    # ========================================================================
    # WIDGET 3: Hipoteza -> co badamy? (odwrotny kierunek)
    # ========================================================================
    h2(id = "ch2h-odwrotnie", class = "section-title", "Odwrotnie: hipoteza → co badamy?"),

    div(class = "narrative",
      p("Teraz odwrotnie: widzisz formalną hipotezę statystyczną.
        Co właściwie badamy? Jak opisać to przystępnym językiem?")
    ),

    figure_panel(
      label = "Ryc. 2.3",
      title = "Quiz: hipoteza → interpretacja",
      uiOutput("ch2h_rev_question"),
      uiOutput("ch2h_rev_options"),
      uiOutput("ch2h_rev_feedback"),
      actionButton("ch2h_rev_next", "Następne pytanie", class = "btn-outline-secondary")
    ),

    # ========================================================================
    # WIDGET 4: Jednostronny vs dwustronny
    # ========================================================================
    h2(id = "ch2h-jedno-dwustronny", class = "section-title", "Test jednostronny a dwustronny"),

    div(class = "narrative",
      p("Sformułowanie Hₐ decyduje, czy test jest jedno- czy dwustronny:"),
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th("Typ"), tags$th("Hₐ"), tags$th("Przykład"), tags$th("Kiedy?"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("Dwustronny")),
            tags$td(withMathJax("\\(\\mu_1 \\neq \\mu_2\\)")),
            tags$td("\"Czy grupy się różnią?\""),
            tags$td("Gdy nie wiesz, w którą stronę — domyślny wybór")
          ),
          tags$tr(
            tags$td(tags$strong("Prawostronny")),
            tags$td(withMathJax("\\(\\mu_1 > \\mu_2\\)")),
            tags$td("\"Czy lek jest lepszy od placebo?\""),
            tags$td("Gdy masz silne podstawy teoretyczne dla kierunku")
          ),
          tags$tr(
            tags$td(tags$strong("Lewostronny")),
            tags$td(withMathJax("\\(\\mu_1 < \\mu_2\\)")),
            tags$td("\"Czy nowa metoda jest szybsza?\""),
            tags$td("Gdy oczekujesz niższej wartości")
          )
        )
      )
    ),

    figure_panel(
      label = "Ryc. 2.4",
      title = "Wizualizacja: jedno- i dwustronny",
      fluidRow(
        column(4,
          radioButtons("ch2h_sided", "Typ testu:",
            choices = c(
              "Dwustronny (≠)" = "two.sided",
              "Prawostronny (>)" = "greater",
              "Lewostronny (<)" = "less"
            ),
            selected = "two.sided"
          ),
          sliderInput("ch2h_alpha", "α:",
                      min = 0.01, max = 0.10, value = 0.05, step = 0.01)
        ),
        column(8,
          plotOutput("ch2h_sided_plot", height = "300px")
        )
      )
    ),

    margin_callout(
      label = "W wątpliwości",
      "Używaj testu dwustronnego. Test jednostronny jest mocniejszy
       (większa moc), ale ryzykowny: jeśli efekt jest w przeciwnym kierunku,
       nie możesz go wykryć. Test jednostronny powinien być zaplanowany
       przed zbieraniem danych.",
      color = "uwaga"
    ),

    # ========================================================================
    # Typowe bledy
    # ========================================================================
    h2(id = "ch2h-bledy", class = "section-title", "Typowe błędy przy formułowaniu hipotez"),

    div(class = "callout-danger",
      tags$ol(
        tags$li(
          tags$b("H₀ z nierównością:"),
          " źle: ", withMathJax("\\(H_0: \\mu_1 \\neq \\mu_2\\)"),
          ". H₀ zawsze zawiera = (ewentualnie ≤ lub ≥)."
        ),
        tags$li(
          tags$b("Hipoteza o próbie zamiast populacji:"),
          " źle: \"H₀: średnia w próbie = 170\". Hipotezy dotyczą parametrów populacji, nie statystyk z próby."
        ),
        tags$li(
          tags$b("Brak precyzji:"),
          " źle: \"H₀: dane są dobre\". Hipoteza musi precyzyjnie określać parametr i wartość."
        ),
        tags$li(
          tags$b("Zmiana hipotezy po zobaczeniu danych (HARKing):"),
          " Hipotezy formułujemy PRZED analizą, nie po!"
        ),
        tags$li(
          tags$b("Mylące H₀ i Hₐ:"),
          " Hₐ to to, co chcesz wykazać. H₀ to \"stan domyślny\" (brak efektu). Nie odwracaj ich."
        )
      )
    ),

    lc_chapter_next(
      num       = "03",
      title     = "Test t jednej próby",
      lead      = "pierwszy konkretny test — średnia wobec wartości referencyjnej.",
      target_id = "ch-jedna-ilosciowa"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch2h_server <- function(input, output, session) {

  # --- Baza przykaldow ---
  examples <- list(
    ex1 = list(
      question = "\"Czy telefon na biurku obniża koncentrację?\"",
      context = "Eksperyment: 40 osób z telefonem w plecaku, 40 z telefonem na biurku. Zmienna: wynik testu koncentracji (0–100 pkt).",
      h0 = "\\(H_0: \\mu_{plecak} = \\mu_{biurko}\\)",
      h1 = "\\(H_a: \\mu_{plecak} \\neq \\mu_{biurko}\\)",
      h1_alt = "lub jednostronnie: \\(H_a: \\mu_{plecak} > \\mu_{biurko}\\)",
      test = "Test t niezależny (różne osoby w każdej grupie)",
      tip = "To nasz eksperyment z rozdziału 1! Dwie niezależne grupy, zmienna ilościowa."
    ),
    ex2 = list(
      question = "\"Czy nowy nawoz daje wyższe plony?\"",
      context = "Doświadczenie polowe: 30 poletek z nowym nawozem, 30 kontrolnych. Zmienna: plon pszenicy (t/ha).",
      h0 = "\\(H_0: \\mu_{nowy} = \\mu_{kontrola}\\)",
      h1 = "\\(H_a: \\mu_{nowy} > \\mu_{kontrola}\\)",
      h1_alt = "",
      test = "Test t niezależny",
      tip = "Jednostronny, bo interesuje nas tylko czy nowy nawóz jest LEPSZY. Dwie niezależne grupy poletek."
    ),
    ex3 = list(
      question = "\"Czy szkolenie BHP zmniejszyło liczbę wypadków?\"",
      context = "20 zakładów mierzonych PRZED i PO szkoleniu. Zmienna: liczba wypadków/miesiąc.",
      h0 = "\\(H_0: \\mu_d = 0\\) (\\(d\\) = po − przed)",
      h1 = "\\(H_a: \\mu_d < 0\\) (zmniejszenie)",
      h1_alt = "",
      test = "Test t parowy (te same zakłady, dwa pomiary)",
      tip = "Te same jednostki mierzone dwa razy → test parowy, nie niezależny!"
    ),
    ex4 = list(
      question = "\"Czy rozkład klas gleby odpowiada normom?\"",
      context = "W gminie zmapowano 200 działek. Klasy gleby: I–VI. Norma krajowa mówi ile % powinno być w każdej klasie.",
      h0 = "\\(H_0: p_I = 0.05, p_{II} = 0.10, \\ldots\\) (zgodnie z normą)",
      h1 = "\\(H_a:\\) co najmniej jedno \\(p_i\\) odbiega od normy",
      h1_alt = "",
      test = "Test χ² zgodności",
      tip = "H₀ określa oczekiwany rozkład. Jedna zmienna jakościowa (klasa gleby)."
    ),
    ex5 = list(
      question = "\"Czy stężenie azotanow rośnie z odległością od źródła?\"",
      context = "Pomiary w 40 punktach wzdłuż rzeki. Zmienna X: km od źródła. Zmienna Y: stężenie NO₃ (mg/l).",
      h0 = "\\(H_0: \\rho = 0\\) (brak korelacji)",
      h1 = "\\(H_a: \\rho \\neq 0\\)",
      h1_alt = "lub jednostronnie: \\(H_a: \\rho > 0\\)",
      test = "Korelacja Pearsona (lub Spearman)",
      tip = "Dwie zmienne ilościowe → korelacja. Uwaga: korelacja ≠ przyczynowość (może inne źródła zanieczyszczeń leżą dalej)."
    ),
    ex6 = list(
      question = "\"Czy typ zagospodarowania działki zależy od strefy miasta?\"",
      context = "Plan zagospodarowania: 300 działek. Zmienne: strefa (centrum/przedmieścia/obrzeża) i typ (mieszkaniowa/usługowa/przemysłowa/zielona).",
      h0 = "\\(H_0:\\) strefa i typ zagospodarowania są niezależne",
      h1 = "\\(H_a:\\) strefa i typ są powiązane",
      h1_alt = "",
      test = "Test χ² niezależności (lub Fisher przy małych n)",
      tip = "Dwie jakościowe → tabela kontyngencji → χ² niezależności."
    ),
    ex7 = list(
      question = "\"Czy rodzaj opakowania wpływa na trwałość jogurtu?\"",
      context = "Eksperyment: 3 typy opakowań (szkło/plastik/karton), po 20 próbek. Zmienna: dni do przeterminowania.",
      h0 = "\\(H_0: \\mu_1 = \\mu_2 = \\mu_3\\)",
      h1 = "\\(H_a:\\) co najmniej jedna para średnich się różni",
      h1_alt = "",
      test = "ANOVA jednoczynnikowa",
      tip = "Więcej niż 2 grupy → ANOVA. NIE wykonuj wielu testów t parami — to pompuje błąd I rodzaju!"
    ),
    ex8 = list(
      question = "\"Czy średnie zużycie wody spełnia normę 150 l/osobę/dobę?\"",
      context = "Pomiary z 60 gospodarstw domowych w gminie. Zmienna: zużycie wody (l/osobę/dobę). Norma projektowa: 150.",
      h0 = "\\(H_0: \\mu = 150\\)",
      h1 = "\\(H_a: \\mu \\neq 150\\)",
      h1_alt = "",
      test = "Test t jednej próby",
      tip = "Jedna zmienna ilościowa, pytamy czy średnia różni się od wartości projektowej."
    ),
    ex9 = list(
      question = "\"Czy ponad 80% próbek wody spełnia normy jakości?\"",
      context = "Laboratorium przebadało 120 próbek wody pitnej. Zmienna: spełnia/nie spełnia (binarna).",
      h0 = "\\(H_0: p \\leq 0.8\\)",
      h1 = "\\(H_a: p > 0.8\\)",
      h1_alt = "",
      test = "Test dwumianowy (lub test proporcji)",
      tip = "Zmienna binarna, pytanie o proporcję → test dwumianowy. Jednostronny, bo \"ponad\"."
    ),
    ex10 = list(
      question = "\"Czy dłuższy sen = lepsza ocena z egzaminu?\"",
      context = "Ankieta wśród 150 studentów. Zmienna X: godziny snu przed egzaminem. Zmienna Y: ocena z egzaminu.",
      h0 = "\\(H_0: \\rho = 0\\) (brak korelacji)",
      h1 = "\\(H_a: \\rho \\neq 0\\)",
      h1_alt = "lub jednostronnie: \\(H_a: \\rho > 0\\)",
      test = "Korelacja Pearsona (lub Spearman)",
      tip = "Dwie zmienne ilościowe → korelacja. Ale uwaga: korelacja ≠ przyczynowość."
    )
  )

  # --- Widget 1: Galeria ---
  output$ch2h_example_display <- renderUI({
    ex <- examples[[input$ch2h_example]]
    if (is.null(ex)) return(NULL)

    tagList(
      div(class = "callout-info",
        p(tags$strong("Pytanie badawcze: "), ex$question),
        p(tags$em("Kontekst: "), ex$context)
      ),
      div(class = "formula-box",
        p(tags$strong("Hipoteza zerowa: "), withMathJax(ex$h0)),
        p(tags$strong("Hipoteza alternatywna: "), withMathJax(ex$h1)),
        if (nchar(ex$h1_alt) > 0) p(tags$em(withMathJax(ex$h1_alt)))
      ),
      div(class = "callout-success",
        p(tags$strong("Test: "), ex$test),
        p(tags$strong("Wskazówka: "), ex$tip)
      )
    )
  })

  # --- Widget 2: Quiz pytanie -> hipoteza ---
  quiz_bank <- list(
    list(
      question = "Badacz chce sprawdzić, czy studenci informatyki śpią mniej niż średnio 7 godzin.",
      options = c(
        "A" = "H₀: μ = 7, Hₐ: μ ≠ 7",
        "B" = "H₀: μ ≥ 7, Hₐ: μ < 7",
        "C" = "H₀: μ < 7, Hₐ: μ ≥ 7"
      ),
      correct = "B",
      explanation = "\"Mniej niż 7\" to hipoteza alternatywna (Hₐ: μ < 7). H₀ zawiera równość (≥)."
    ),
    list(
      question = "Firma farmaceutyczna bada, czy nowy lek różni się skutecznością od istniejącego.",
      options = c(
        "A" = "H₀: μ_nowy = μ_stary, Hₐ: μ_nowy ≠ μ_stary",
        "B" = "H₀: μ_nowy ≠ μ_stary, Hₐ: μ_nowy = μ_stary",
        "C" = "H₀: μ_nowy = μ_stary, Hₐ: μ_nowy > μ_stary"
      ),
      correct = "A",
      explanation = "\"Różni się\" (bez kierunku) = test dwustronny. H₀: brak różnicy, Hₐ: jest różnica."
    ),
    list(
      question = "Nauczyciel chce zbadać, czy rozkład ocen na egzaminie odpowiadał krzywej normalnej: 10% niedostatecznych, 20% dostatecznych, 40% dobrych, 20% bardzo dobrych, 10% celujących.",
      options = c(
        "A" = "H₀: rozkład ocen jest normalny, Hₐ: nie jest normalny",
        "B" = "H₀: p₁=0.1, p₂=0.2, p₃=0.4, p₄=0.2, p₅=0.1; Hₐ: co najmniej jedno pi różne",
        "C" = "H₀: μ = 3.5, Hₐ: μ ≠ 3.5"
      ),
      correct = "B",
      explanation = "To test χ² zgodności — H₀ określa konkretne proporcje, Hₐ: rozkład się różni."
    ),
    list(
      question = "Dietetyk bada, czy płeć wpływa na preferencje dietetyczne (wege/mięso/różne).",
      options = c(
        "A" = "H₀: μ_K = μ_M, Hₐ: μ_K ≠ μ_M",
        "B" = "H₀: dieta i płeć są niezależne, Hₐ: dieta i płeć są powiązane",
        "C" = "H₀: proporcje diet są równe, Hₐ: nie są równe"
      ),
      correct = "B",
      explanation = "Dwie zmienne jakościowe → test niezależności. H₀: niezależność, Hₐ: powiązanie."
    ),
    list(
      question = "Producent twierdzi, że co najmniej 95% produktów spełnia normy. Kontrola chce to zweryfikować.",
      options = c(
        "A" = "H₀: p = 0.95, Hₐ: p ≠ 0.95",
        "B" = "H₀: p ≥ 0.95, Hₐ: p < 0.95",
        "C" = "H₀: p < 0.95, Hₐ: p ≥ 0.95"
      ),
      correct = "B",
      explanation = "Kontrola chce sprawdzić, czy odsetek jest niższy niż deklarowane 95%. Hₐ: p < 0.95."
    )
  )

  ch2h_quiz_idx <- reactiveVal(1)
  ch2h_quiz_answered <- reactiveVal(FALSE)
  ch2h_quiz_selected <- reactiveVal(NULL)

  observe({ ch2h_quiz_idx() })  # initialize
  observeEvent(input$ch2h_quiz_next, {
    ch2h_quiz_idx(sample(length(quiz_bank), 1))
    ch2h_quiz_answered(FALSE)
    ch2h_quiz_selected(NULL)
  })

  output$ch2h_quiz_question <- renderUI({
    q <- quiz_bank[[ch2h_quiz_idx()]]
    div(class = "callout-info",
      p(tags$strong("Pytanie:"), q$question)
    )
  })

  output$ch2h_quiz_options <- renderUI({
    ch2h_quiz_idx()
    if (ch2h_quiz_answered()) return(NULL)
    q <- quiz_bank[[ch2h_quiz_idx()]]
    letters <- c("A", "B", "C")
    div(class = "quiz-tiles quiz-cols-3",
      lapply(letters, function(l) {
        actionButton(paste0("ch2h_qtile_", l),
          tagList(
            div(class = "tile-letter", l),
            div(class = "tile-text", q$options[l])
          ),
          class = "quiz-tile"
        )
      })
    )
  })

  observe({
    for (l in c("A", "B", "C")) {
      local({
        val <- l
        observeEvent(input[[paste0("ch2h_qtile_", val)]], {
          if (ch2h_quiz_answered()) return()
          ch2h_quiz_selected(val)
          ch2h_quiz_answered(TRUE)
        }, ignoreInit = TRUE)
      })
    }
  })

  output$ch2h_quiz_feedback <- renderUI({
    req(ch2h_quiz_answered())
    q <- quiz_bank[[ch2h_quiz_idx()]]
    answer <- ch2h_quiz_selected()
    if (answer == q$correct) {
      div(class = "callout-success",
        tags$strong("Poprawnie!"),
        p(q$explanation))
    } else {
      div(class = "callout-danger",
        tags$strong("Nie! "),
        p("Poprawna odpowiedź: ", q$correct, "."),
        p(q$explanation))
    }
  })

  # --- Widget 3: Quiz hipoteza -> interpretacja ---
  rev_bank <- list(
    list(
      hypothesis = "H₀: μ = 36.6, Hₐ: μ ≠ 36.6",
      context = "Badanie grupy pacjentów. Zmienna: temperatura ciała (°C).",
      options = c(
        "A" = "Czy średnia temperatura pacjentów różni się od normy 36.6°C?",
        "B" = "Czy temperatura każdego pacjenta wynosi 36.6°C?",
        "C" = "Czy rozkład temperatury jest normalny?"
      ),
      correct = "A",
      explanation = "Test t jednej próby: porównanie średniej populacyjnej z wartością referencyjną."
    ),
    list(
      hypothesis = "H₀: ρ = 0, Hₐ: ρ ≠ 0",
      context = "Dane: 200 studentów. Zmienne: godziny nauki i wyniki egzaminu.",
      options = c(
        "A" = "Czy godziny nauki powodują lepsze wyniki?",
        "B" = "Czy istnieje związek liniowy między godzinami nauki a wynikami?",
        "C" = "Czy studenci uczą się wystarczająco dużo?"
      ),
      correct = "B",
      explanation = "ρ = 0 to brak korelacji. Testujemy związek liniowy (Pearson). Uwaga: korelacja ≠ przyczynowość!"
    ),
    list(
      hypothesis = "H₀: płeć i preferencje są niezależne, Hₐ: są powiązane",
      context = "Ankieta: 300 osób. Zmienne: płeć (K/M) i ulubiony gatunek filmu.",
      options = c(
        "A" = "Czy kobiety oglądają więcej filmów?",
        "B" = "Czy płeć wpływa na preferencje filmowe?",
        "C" = "Czy rozkład płci jest równomierny?"
      ),
      correct = "B",
      explanation = "Test χ² niezależności: czy istnieje związek między dwiema zmiennymi jakościowymi."
    ),
    list(
      hypothesis = "H₀: μ₁ = μ₂ = μ₃, Hₐ: co najmniej jedna średnia różna",
      context = "Badanie wyników egzaminu w trzech grupach ćwiczeniowych.",
      options = c(
        "A" = "Czy wyniki egzaminu są normalne?",
        "B" = "Czy prowadzący wpływają na wyniki? (ANOVA)",
        "C" = "Czy trzecia grupa jest najlepsza?"
      ),
      correct = "B",
      explanation = "ANOVA: porównanie średnich w 3+ grupach. Hₐ nie mówi, która grupa jest najlepsza — to rola post-hoc."
    ),
    list(
      hypothesis = "H₀: μ_d = 0, Hₐ: μ_d > 0 (d = po − przed)",
      context = "20 uczniów, mierzeni przed i po kursie szybkiego czytania. Zmienna: słowa/min.",
      options = c(
        "A" = "Czy kurs poprawił szybkość czytania?",
        "B" = "Czy uczniowie czytają szybciej niż średnia populacyjna?",
        "C" = "Czy różnica między uczniami jest istotna?"
      ),
      correct = "A",
      explanation = "Test t parowy (jednostronny): te same osoby przed/po. μ_d > 0 oznacza poprawa (po > przed)."
    )
  )

  ch2h_rev_idx <- reactiveVal(1)
  ch2h_rev_answered <- reactiveVal(FALSE)
  ch2h_rev_selected <- reactiveVal(NULL)

  observeEvent(input$ch2h_rev_next, {
    ch2h_rev_idx(sample(length(rev_bank), 1))
    ch2h_rev_answered(FALSE)
    ch2h_rev_selected(NULL)
  })

  output$ch2h_rev_question <- renderUI({
    q <- rev_bank[[ch2h_rev_idx()]]
    div(class = "callout-info",
      p(tags$strong("Kontekst: "), q$context),
      div(class = "formula-box",
        p(tags$strong("Hipotezy: "), q$hypothesis)
      )
    )
  })

  output$ch2h_rev_options <- renderUI({
    ch2h_rev_idx()
    if (ch2h_rev_answered()) return(NULL)
    q <- rev_bank[[ch2h_rev_idx()]]
    letters <- c("A", "B", "C")
    div(class = "quiz-tiles quiz-cols-3",
      lapply(letters, function(l) {
        actionButton(paste0("ch2h_rtile_", l),
          tagList(
            div(class = "tile-letter", l),
            div(class = "tile-text", q$options[l])
          ),
          class = "quiz-tile"
        )
      })
    )
  })

  observe({
    for (l in c("A", "B", "C")) {
      local({
        val <- l
        observeEvent(input[[paste0("ch2h_rtile_", val)]], {
          if (ch2h_rev_answered()) return()
          ch2h_rev_selected(val)
          ch2h_rev_answered(TRUE)
        }, ignoreInit = TRUE)
      })
    }
  })

  output$ch2h_rev_feedback <- renderUI({
    req(ch2h_rev_answered())
    q <- rev_bank[[ch2h_rev_idx()]]
    answer <- ch2h_rev_selected()
    if (answer == q$correct) {
      div(class = "callout-success",
        tags$strong("Poprawnie!"),
        p(q$explanation))
    } else {
      div(class = "callout-danger",
        tags$strong("Nie! "),
        p("Poprawna odpowiedź: ", q$correct, "."),
        p(q$explanation))
    }
  })

  # --- Widget 4: Jednostronny vs dwustronny ---
  output$ch2h_sided_plot <- renderPlot({
    alpha <- input$ch2h_alpha
    sided <- input$ch2h_sided
    x <- seq(-4, 4, length.out = 500)
    y <- dnorm(x)
    df <- data.frame(x = x, y = y)

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_line(color = col_h0, linewidth = 1.2)

    if (sided == "two.sided") {
      crit <- qnorm(1 - alpha / 2)
      shade_left <- df[df$x <= -crit, ]
      shade_right <- df[df$x >= crit, ]
      p <- p +
        geom_area(data = shade_left, fill = col_reject, alpha = 0.4) +
        geom_area(data = shade_right, fill = col_reject, alpha = 0.4) +
        geom_vline(xintercept = c(-crit, crit), linetype = "dashed", color = col_reject) +
        labs(title = paste0("Dwustronny: α/2 = ", alpha/2, " na każdym ogonie"))
    } else if (sided == "greater") {
      crit <- qnorm(1 - alpha)
      shade <- df[df$x >= crit, ]
      p <- p +
        geom_area(data = shade, fill = col_reject, alpha = 0.4) +
        geom_vline(xintercept = crit, linetype = "dashed", color = col_reject) +
        labs(title = paste0("Prawostronny: całe α = ", alpha, " na prawym ogonie"))
    } else {
      crit <- qnorm(alpha)
      shade <- df[df$x <= crit, ]
      p <- p +
        geom_area(data = shade, fill = col_reject, alpha = 0.4) +
        geom_vline(xintercept = crit, linetype = "dashed", color = col_reject) +
        labs(title = paste0("Lewostronny: całe α = ", alpha, " na lewym ogonie"))
    }

    p +
      labs(x = "Statystyka testowa (z)", y = "Gęstość") +
      annotate("text", x = 0, y = max(y) * 0.5, label = "Nie odrzucamy H₀",
               color = col_accept, fontface = "bold", size = 5) +
      theme()
  })
}
