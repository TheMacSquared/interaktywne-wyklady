# ============================================================================
# CHAPTER 2: Formulowanie hipotez statystycznych
# ============================================================================

ch2h_ui <- list(
  id = "ch-hipotezy", num = "02", title = "Od pytania do hipotezy",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 02 · Testowanie hipotez",
      num    = "02",
      title  = "Od pytania do hipotezy.",
      lead   = "„Wydaje mi się, że coś z nimi jest nie tak — co robić?” Sformalizowanie
                potocznego pytania w parę H₀ / Hₐ to pierwszy krok każdego testu.
                Ćwiczymy tłumaczenie z języka codziennego."
    ),

    lc_h2("ch2h-zasada", "Zasada: od potocznego do formalnego"),

    tagList(
      p("W badaniach pytania są formułowane swobodnym językiem:"),
      tags$ul(
        tags$li(em("„Czy mężczyźni są wyżsi od kobiet?”")),
        tags$li(em("„Czy korepetycje pomagają?”")),
        tags$li(em("„Czy lodów sprzedaje się więcej w ciepłe dni?”"))
      ),
      p("Test statystyczny wymaga jednak precyzyjnych hipotez",
        " — stwierdzeń, które można zweryfikować danymi. Żeby sformułować
        hipotezę, musimy odpowiedzieć na dwa pytania:"),
      tags$ol(
        tags$li(tags$b("Co mierzymy lub porównujemy?"),
                " Czy chodzi o średnią, proporcję, korelację,
                czy różnicę między grupami?"),
        tags$li(tags$b("Jaki jest kierunek pytania?"),
                " Czy pytamy o różnicę w ogóle, wartość większą,
                wartość mniejszą, czy zgodność z wartością odniesienia?")
      ),
      p("W zapisie formalnym pierwsza decyzja mówi nam, jaki ",
        "parametr",
        " pojawi się we wzorze, a druga — jaki ",
        "znak",
        " połączy go z wartością odniesienia albo z drugim parametrem."),
      p("H₀ (hipoteza zerowa) zawsze zawiera znak równości",
        " (=, ≤, ≥) — reprezentuje „stan domyślny”, brak efektu.
        Hₐ (hipoteza alternatywna) to dopełnienie",
        " — to, co chcemy wykazać (≠, >, <).")
    ),

    lc_h2("ch2h-rozbior", "Rozbiór przykładu: telefon a koncentracja"),

    tagList(
      p("Pytanie z poprzedniego rozdziału: "),
      lc_feedback(type = "info", style = "font-size: 17px;",
        tags$em("„Czy telefon na biurku wpływa na koncentrację?”")
      ),
      p("Krok 1 — parametr. Co porównujemy? Mamy dwie grupy
        (plecak / biurko) i każdej mierzymy wynik testu koncentracji.
        Interesuje nas ", "średnia",
        " koncentracja w populacji — osobno dla grupy „plecak” i „biurko”."),
      p("Krok 2 — relacja. Pytanie pyta neutralnie — ",
        tags$em("czy w ogóle"),
        " jest jakiś wpływ. Nie zakładamy z góry kierunku, więc Hₐ to po prostu
        „średnie się różnią”. Relacja w Hₐ to znak „≠”."),
      p("Krok 3 — sformułowanie:"),
      lc_formula_box(
        p(tags$b("H₀ (stan domyślny):"),
          " średnia koncentracja w grupie z telefonem na biurku jest ",
          "równa",
          " średniej koncentracji w grupie z telefonem w plecaku."),
        p(tags$b("Hₐ (to, co chcemy wykazać):"),
          " średnia koncentracja w grupie z telefonem na biurku ",
          "różni się od",
          " średniej koncentracji w grupie z telefonem w plecaku.")
      ),
      p("Zauważ, że H₀ i Hₐ są przeciwstawne",
        " — razem wyczerpują wszystkie możliwości („są równe” albo „różnią się”).
        To kluczowa zasada przy formułowaniu hipotez: jedna jest dokładnym
        zaprzeczeniem drugiej."),
      p("To jest test dwustronny",
        " — Hₐ pyta tylko, czy jest jakaś różnica, bez zakładania kierunku.
        O tym, kiedy warto użyć wariantu jednostronnego (Hₐ wskazującej
        konkretny kierunek), powiemy w sekcji „Test jednostronny a dwustronny”
        niżej."),
    ),

    lc_h2("ch2h-formalizm", "Od hipotezy słownej do zapisu formalnego"),

    tagList(
      p("Słowna wersja hipotez nie jest etapem „mniej statystycznym”.
        To uporządkowanie sensu badania. Dopiero kiedy wiemy, ",
        "jaki parametr",
        " badamy i ",
        "jaka relacja",
        " nas interesuje, możemy przejść do pełnego zapisu formalnego."),
      p("Najpierw nazywamy parametry. W przykładzie z telefonem możemy oznaczyć:"),
      lc_formula_box(
        p(withMathJax("\\(\\mu_{plecak}\\)"),
          " — średnia koncentracja w populacji studentów, gdy telefon jest w plecaku"),
        p(withMathJax("\\(\\mu_{biurko}\\)"),
          " — średnia koncentracja w populacji studentów, gdy telefon leży na biurku")
      ),
      p("Dopiero potem zapisujemy hipotezy. Ponieważ pytanie brzmi „czy telefon
        na biurku wpływa na koncentrację?”, hipoteza alternatywna jest dwustronna:"),
      lc_formula_box(
        p(withMathJax("\\(H_0: \\mu_{biurko} = \\mu_{plecak}\\)")),
        p(withMathJax("\\(H_a: \\mu_{biurko} \\neq \\mu_{plecak}\\)"))
      ),
      p("Pełny formalizm składa się więc z trzech elementów: ",
        "definicji symboli",
        ", ",
        "hipotezy zerowej",
        " i ",
        "hipotezy alternatywnej",
        ". Bez definicji symboli sam wzór jest nieczytelny: ",
        withMathJax("\\(\\mu_1 < \\mu_2\\)"),
        " nic nie mówi, jeśli nie wiemy, czym są grupa 1 i grupa 2."),
      p("W praktyce warto iść zawsze tą samą ścieżką: ",
        tags$em("pytanie badawcze → hipotezy słowne → definicja parametrów → zapis formalny"),
        ". To chroni przed najczęstszym błędem: mechanicznym wpisaniem znaków
        matematycznych bez zrozumienia, co dokładnie porównujemy.")
    ),

    inline_callout(
      label = "Szablon",
      tagList(
        tags$p(tags$b("H₀:"), " parametr ", tags$b("=/≤/≥"), " wartość"),
        tags$p(tags$b("Hₐ:"), " parametr ", tags$b("≠/>/<"), " wartość")
      )
    ),

    # ========================================================================
    # WIDGET 1: Galeria przykładów (język naturalny) — część dwustronna
    # ========================================================================
    lc_h2("ch2h-galeria", "Galeria: sformułuj hipotezy sam"),

    tagList(
      p("Dla każdego pytania zastanów się, jaki parametr porównujemy i jakiej
        relacji szuka Hₐ. Pisz sobie na boku w języku naturalnym (bez greki).
        Potem kliknij „Pokaż odpowiedź” i porównaj."),
      p("Na razie ćwiczymy hipotezy nieskierowane",
        " — Hₐ mówi „średnie się różnią” albo „zmienne są powiązane”, bez
        zakładania kierunku. Przykłady, w których pytanie z góry wskazuje
        kierunek (i Hₐ jest jednostronna), pojawią się dalej, po sekcji
        o teście jednostronnym i dwustronnym.")
    ),

    hypothesis_practice("ch2h_gal", list(
      list(
        question = "60 gospodarstw domowych, pomiary zużycia wody.
                    Norma projektowa: 150 l na osobę na dobę.
                    Czy średnie zużycie w naszej gminie spełnia normę?",
        h0 = "Średnie zużycie wody w gminie jest równe 150 l na osobę na dobę.",
        ha = "Średnie zużycie wody w gminie różni się od 150 l na osobę na dobę.",
        note = "Dwustronny — „spełnia normę” oznacza „nie odbiega” w żadną stronę."
      ),
      list(
        question = "Plan zagospodarowania: 300 działek podzielono według strefy
                    (centrum / przedmieścia / obrzeża) i typu (mieszkaniowa /
                    usługowa / przemysłowa / zielona). Czy typ zagospodarowania
                    zależy od strefy miasta?",
        h0 = "Typ zagospodarowania działki i strefa miasta są niezależne.",
        ha = "Typ zagospodarowania działki i strefa miasta są ze sobą powiązane.",
        note = "Dwie zmienne jakościowe — pytamy o niezależność vs powiązanie. Nie ma tu „kierunku”, więc nie mówimy o jedno- / dwustronności."
      ),
      list(
        question = "Eksperyment: 3 typy opakowań jogurtu (szkło / plastik / karton),
                    po 20 próbek każdego. Czy rodzaj opakowania wpływa na trwałość?",
        h0 = "Średnia trwałość jogurtu jest taka sama dla wszystkich trzech typów opakowań.",
        ha = "Co najmniej jeden typ opakowania ma inną średnią trwałość niż pozostałe.",
        note = "Trzy grupy — porównanie wielu średnich. Hₐ mówi tylko „co najmniej jedna jest inna”, nie która — nie ma tu jedno-/dwustronności w klasycznym sensie."
      )
    )),

    # ========================================================================
    # WIDGET 4: Jednostronny vs dwustronny
    # ========================================================================
    lc_h2("ch2h-jedno-dwustronny", "Test jednostronny a dwustronny"),

    tagList(
      p("Sformułowanie Hₐ decyduje, czy test jest jedno- czy dwustronny:"),
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 15px;",
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

    inline_callout(
      label = "W wątpliwości",
      "Używaj testu dwustronnego. Test jednostronny jest mocniejszy
       (większa moc), ale ryzykowny: jeśli efekt jest w przeciwnym kierunku,
       nie możesz go wykryć. Test jednostronny powinien być zaplanowany
       przed zbieraniem danych.",
      color = "uwaga"
    ),

    tagList(
      p("W teście dwustronnym poziom istotności dzielimy na dwa ogony rozkładu —
         po α/2 na każdym. W teście jednostronnym całe α leży po jednej stronie.
         Suwakiem niżej możesz zmienić α i przekonać się, jak rośnie albo maleje
         obszar odrzucenia.")
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
          div(class = "ws-chart-wrap",
            tags$canvas(id = "ch2h_sided_chart")
          )
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Galeria przykładów — jednostronne
    # ========================================================================
    lc_h2("ch2h-galeria-jedno", "Galeria: hipotezy jednostronne"),

    tagList(
      p("A teraz przykłady, w których pytanie z góry wskazuje kierunek
        — i Hₐ jest jednostronna. Zwróć uwagę, jak H₀ jest dopełnieniem Hₐ:
        jeśli Hₐ mówi „wyższy”, to H₀ obejmuje wszystko, co nie jest „wyższe”
        (czyli „nie wyższy niż”, ≤). Razem H₀ i Hₐ wyczerpują wszystkie
        możliwości.")
    ),

    hypothesis_practice("ch2h_gal_one", list(
      list(
        question = "Doświadczenie polowe: 30 poletek z nowym nawozem,
                    30 kontrolnych. Czy nowy nawóz daje wyższe plony?",
        h0 = "Średni plon na poletkach z nowym nawozem jest nie wyższy niż średni plon na poletkach kontrolnych.",
        ha = "Średni plon na poletkach z nowym nawozem jest wyższy niż średni plon na poletkach kontrolnych.",
        note = "Jednostronny — pytamy tylko o „wyższe”, nie o ogólną różnicę. H₀ to dopełnienie Hₐ: „nie wyższy niż” = równy lub niższy."
      ),
      list(
        question = "20 zakładów, w których mierzono liczbę wypadków przed
                    i po szkoleniu BHP. Czy szkolenie zmniejszyło liczbę wypadków?",
        h0 = "Średnia liczba wypadków po szkoleniu jest nie niższa niż przed szkoleniem.",
        ha = "Średnia liczba wypadków po szkoleniu jest niższa niż przed szkoleniem.",
        note = "Jednostronny („zmniejszyło”). Uwaga: te same zakłady mierzone dwa razy — w praktyce użyjemy testu t dla danych sparowanych."
      ),
      list(
        question = "Laboratorium przebadało 120 próbek wody pitnej.
                    Czy ponad 80% próbek spełnia normy jakości?",
        h0 = "Odsetek próbek spełniających normy w populacji jest nie wyższy niż 80%.",
        ha = "Odsetek próbek spełniających normy w populacji jest wyższy niż 80%.",
        note = "Jednostronny („ponad”). Parametr to proporcja, nie średnia."
      ),
      list(
        question = "Ankieta wśród 150 studentów: godziny snu przed egzaminem
                    i ocena z egzaminu. Czy dłuższy sen wiąże się z lepszą oceną?",
        h0 = "Nie ma dodatniego związku między godzinami snu a oceną z egzaminu (związek zerowy lub ujemny).",
        ha = "Im więcej snu, tym wyższa ocena z egzaminu (dodatni związek).",
        note = "Jednostronny — „dłuższy → lepsza” wskazuje kierunek dodatniego związku."
      )
    )),

    # ========================================================================
    # Typowe bledy
    # ========================================================================
    lc_h2("ch2h-bledy", "Typowe błędy przy formułowaniu hipotez"),

    tagList(
      p("Kilka pułapek, na które warto uważać przy formułowaniu hipotez:"),
      tags$ol(
        tags$li(
          tags$b("H₀ z nierównością."),
          " Źle: ", withMathJax("\\(H_0: \\mu_1 \\neq \\mu_2\\)"),
          ". H₀ zawsze zawiera znak równości (=, ewentualnie ≤ lub ≥)."
        ),
        tags$li(
          tags$b("Hipoteza o próbie zamiast populacji."),
          " Źle: „H₀: średnia w próbie = 170”. Hipotezy zawsze dotyczą parametrów",
          tags$em(" populacji"), ", nie statystyk z konkretnej próby."
        ),
        tags$li(
          tags$b("Brak precyzji."),
          " Źle: „H₀: dane są dobre”. Hipoteza musi precyzyjnie określać parametr
          i wartość odniesienia — inaczej nie da się jej sprawdzić danymi."
        ),
        tags$li(
          tags$b("Zmiana hipotezy po zobaczeniu danych (HARKing)."),
          " Hipotezy formułujemy ", tags$em("przed"), " analizą, nie po.
          Dopasowywanie H₀/Hₐ do wyniku jest intelektualnym oszustwem."
        ),
        tags$li(
          tags$b("Mylące H₀ i Hₐ."),
          " Hₐ to to, co chcemy wykazać. H₀ to „stan domyślny” (brak efektu).
          Nie odwracaj ich — odrzucenie „H₀: jest efekt” nie miałoby sensu."
        )
      )
    ),

    lc_chapter_next(
      num       = "03",
      title     = "Błędy, p-wartość i decyzja",
      lead      = "jak przejść od H₀ i Hₐ do formalnego werdyktu.",
      target_id = "ch-decyzja"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch2h_server <- function(input, output, session) {

  # --- Widget: Jednostronny vs dwustronny ---
  observe({
    req(input$ch2h_alpha, input$ch2h_sided)
    alpha <- input$ch2h_alpha
    sided <- input$ch2h_sided

    crit <- if (sided == "two.sided") {
      qnorm(1 - alpha / 2)
    } else if (sided == "greater") {
      qnorm(1 - alpha)
    } else {
      qnorm(alpha)
    }

    session$sendCustomMessage("ws_sided_chart", list(
      id = "ch2h_sided_chart",
      sided = sided,
      alpha = alpha,
      crit = crit
    ))
  })
}
