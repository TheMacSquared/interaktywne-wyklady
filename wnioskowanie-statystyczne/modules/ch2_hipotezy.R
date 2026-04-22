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
        tags$li(em("„Czy mężczyźni są wyżsi od kobiet?”")),
        tags$li(em("„Czy korepetycje pomagają?”")),
        tags$li(em("„Czy lodów sprzedaje się więcej w ciepłe dni?”"))
      ),
      p("Test statystyczny wymaga jednak ", tags$b("precyzyjnych hipotez"),
        " — stwierdzeń, które można zweryfikować danymi. Żeby sformułować
        hipotezę, musimy określić dwie rzeczy:"),
      tags$ol(
        tags$li(tags$b("Parametr"),
                " — co właściwie porównujemy? Średnią? Proporcję?
                Korelację? Różnicę między grupami?"),
        tags$li(tags$b("Relację"),
                " — jaki znak ma łączyć parametr z wartością odniesienia lub
                drugim parametrem? Równość (=), nierówność (≠), większe (>)
                albo mniejsze (<)?")
      ),
      p("H₀ (hipoteza zerowa) zawsze zawiera znak ", tags$b("równości"),
        " (=, ≤, ≥) — reprezentuje „stan domyślny”, brak efektu.
        Hₐ (hipoteza alternatywna) to ", tags$b("dopełnienie"),
        " — to, co chcemy wykazać (≠, >, <).")
    ),

    h2(id = "ch2h-rozbior", class = "section-title", "Rozbiór przykładu: telefon a koncentracja"),

    div(class = "narrative",
      p("Pytanie z poprzedniego rozdziału: "),
      div(class = "callout-info", style = "font-size: 17px;",
        tags$em("„Czy telefon na biurku obniża koncentrację?”")
      ),
      p(tags$b("Krok 1 — parametr.")," Co porównujemy? Mamy dwie grupy
        (plecak / biurko) i każdej mierzymy wynik testu koncentracji.
        Interesuje nas ", tags$b("średnia"),
        " koncentracja w populacji — osobno dla grupy „plecak” i „biurko”."),
      p(tags$b("Krok 2 — relacja."), " Pytanie mówi „obniża”, nie „różni się”.
        To kierunkowe pytanie — szukamy ", tags$em("konkretnego"),
        " kierunku: czy grupa „biurko” ma ", tags$em("niższą"), " średnią
        niż „plecak”. Relacja w Hₐ to znak „<”."),
      p(tags$b("Krok 3 — sformułowanie:")),
      div(class = "formula-box",
        p(tags$b("H₀ (stan domyślny):"),
          " średnia koncentracja w grupie z telefonem w plecaku jest ",
          tags$b("równa"),
          " średniej koncentracji w grupie z telefonem na biurku."),
        p(tags$b("Hₐ (to, co chcemy wykazać):"),
          " średnia koncentracja w grupie z telefonem na biurku jest ",
          tags$b("niższa"),
          " niż średnia koncentracji w grupie z telefonem w plecaku.")
      ),
      p("To jest test ", tags$b("jednostronny"),
        " — Hₐ wskazuje konkretny kierunek. Gdyby pytanie brzmiało „czy telefon ",
        tags$em("wpływa"), " na koncentrację” (neutralnie, bez kierunku), Hₐ
        musiałaby być ", tags$em("„średnie różnią się”"),
        " — to byłby test dwustronny."),
      p(tags$b("Uwaga:"), " formalnego zapisu z greką (μ, ρ, p) i znakami
        matematycznymi nauczymy się później — teraz chodzi o to, żeby
        ", tags$em("słowami"),
        " rozłożyć pytanie badawcze na parametr i relację.")
    ),

    margin_callout(
      label = "Szablon",
      tagList(
        tags$p(tags$b("H₀:"), " parametr ", tags$b("=/≤/≥"), " wartość"),
        tags$p(tags$b("Hₐ:"), " parametr ", tags$b("≠/>/<"), " wartość")
      )
    ),

    # ========================================================================
    # WIDGET 1: Galeria przykładów (język naturalny)
    # ========================================================================
    h2(id = "ch2h-galeria", class = "section-title", "Galeria: sformułuj hipotezy sam"),

    div(class = "narrative",
      p("Dla każdego pytania zastanów się, jaki parametr porównujemy i jakiej
        relacji szuka Hₐ. Pisz sobie na boku w języku naturalnym (bez greki).
        Potem kliknij „Pokaż odpowiedź” i porównaj.")
    ),

    hypothesis_practice("ch2h_gal", list(
      list(
        question = "Doświadczenie polowe: 30 poletek z nowym nawozem,
                    30 kontrolnych. Czy nowy nawóz daje wyższe plony?",
        h0 = "Średni plon na poletkach z nowym nawozem jest równy średniemu plonowi na poletkach kontrolnych.",
        ha = "Średni plon na poletkach z nowym nawozem jest wyższy niż średni plon na poletkach kontrolnych.",
        note = "Jednostronny — pytamy tylko o „wyższe”, nie o ogólną różnicę."
      ),
      list(
        question = "20 zakładów, w których mierzono liczbę wypadków przed
                    i po szkoleniu BHP. Czy szkolenie zmniejszyło liczbę wypadków?",
        h0 = "Średnia liczba wypadków przed szkoleniem jest równa średniej liczbie wypadków po szkoleniu.",
        ha = "Średnia liczba wypadków po szkoleniu jest niższa niż przed szkoleniem.",
        note = "Jednostronny („zmniejszyło”). Uwaga: te same zakłady mierzone dwa razy — w praktyce użyjemy testu parowego."
      ),
      list(
        question = "60 gospodarstw domowych, pomiary zużycia wody.
                    Norma projektowa: 150 l na osobę na dobę.
                    Czy średnie zużycie w naszej gminie spełnia normę?",
        h0 = "Średnie zużycie wody w gminie jest równe 150 l na osobę na dobę.",
        ha = "Średnie zużycie wody w gminie różni się od 150 l na osobę na dobę.",
        note = "Dwustronny — „spełnia normę” oznacza „nie odbiega” w żadną stronę."
      ),
      list(
        question = "Laboratorium przebadało 120 próbek wody pitnej.
                    Czy ponad 80% próbek spełnia normy jakości?",
        h0 = "Odsetek próbek spełniających normy w populacji jest równy 80%.",
        ha = "Odsetek próbek spełniających normy w populacji jest wyższy niż 80%.",
        note = "Jednostronny („ponad”). Parametr to proporcja, nie średnia."
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
        question = "Ankieta wśród 150 studentów: godziny snu przed egzaminem
                    i ocena z egzaminu. Czy dłuższy sen wiąże się z lepszą oceną?",
        h0 = "Nie ma związku między godzinami snu a oceną z egzaminu.",
        ha = "Im więcej snu, tym wyższa ocena z egzaminu (dodatni związek).",
        note = "Jednostronny — „dłuższy → lepsza” wskazuje kierunek dodatniego związku."
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

    margin_callout(
      label = "W wątpliwości",
      "Używaj testu dwustronnego. Test jednostronny jest mocniejszy
       (większa moc), ale ryzykowny: jeśli efekt jest w przeciwnym kierunku,
       nie możesz go wykryć. Test jednostronny powinien być zaplanowany
       przed zbieraniem danych.",
      color = "uwaga"
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

    # ========================================================================
    # Typowe bledy
    # ========================================================================
    h2(id = "ch2h-bledy", class = "section-title", "Typowe błędy przy formułowaniu hipotez"),

    div(class = "narrative",
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

  # --- Widget: Jednostronny vs dwustronny ---
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
      annotate("text", x = 0, y = max(y) * 0.5, label = "Nie odrzucamy H0",
               color = col_accept, fontface = "bold", size = 5) +
      theme()
  })
}
