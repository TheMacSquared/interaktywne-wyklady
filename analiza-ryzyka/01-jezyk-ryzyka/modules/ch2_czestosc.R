# ==========================================================================
# ROZDZIAŁ 2: CZĘSTOŚĆ EMPIRYCZNA I PRAWDOPODOBIEŃSTWO
# ==========================================================================

ch2_ui <- lecture_chapter(
  id = "ch-czestosc",
  num = "02",
  title = "Od obserwacji do modelu",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · Język ryzyka",
      num = "02",
      title = "Jeden miesiąc może kłamać.",
      lead = "Częstość obserwowana zmienia się od serii do serii. Dopiero wraz
              z liczbą porównywalnych okresów zaczyna odsłaniać stabilny wzorzec."
    ),

    margin_callout(
      label = "Jednostka obserwacji",
      "Jedna próba oznacza jedną 8-godzinną zmianę w konkretnym korytarzu.
       Zdarzenie rejestrowe: co najmniej jedno poślizgnięcie (utrata
       przyczepności i upadek) podczas tej zmiany. Rejestr zlicza zmiany ze
       zdarzeniem, nie pojedyncze poślizgnięcia.",
      color = "ok"
    ),

    lc_h2("ch2-mianownik", "Najpierw ustal mianownik"),
    lc_p(
      "Zdanie „były trzy poślizgnięcia” nie pozwala porównać dwóch magazynów.
       Potrzebujemy wiedzieć, w ilu porównywalnych zmianach mogły wystąpić, jak
       zdefiniowano zdarzenie i czy obserwacje dotyczą tych samych warunków."
    ),

    lc_p(
      "Mianownik jest tak samo ważny jak licznik, bo zmienia pytanie. Trzy
       poślizgnięcia na 40 zmian to inna sytuacja niż trzy na 400 zmian, choć
       licznik jest identyczny. Równie ważne jest, co liczymy w liczniku: w
       rejestrze Bananpolu zliczamy zmiany, w których doszło do co najmniej
       jednego poślizgnięcia. Zmiana z dwoma upadkami liczy się raz. Taki wybór
       sprawia, że każda obserwacja kończy się jednym z dwóch wyników — zdarzenie
       zaszło albo nie — i że częstość zawsze leży między 0 a 1."
    ),
    risk_definition("1.2", "Częstość empiryczna", c(
      "Niech w n porównywalnych, niezależnie przeprowadzonych obserwacjach
       zdarzenie A zaszło n_A razy. Częstością empiryczną (względną) zdarzenia A
       nazywamy iloraz n_A / n, oznaczany p̂ₙ (czytaj: p z daszkiem).",
      "Częstość jest wynikiem konkretnej serii obserwacji. Inna seria tej samej
       długości da zwykle inną wartość, dlatego piszemy p̂ z daszkiem — to
       oszacowanie, a nie sam parametr modelu."
    )),
    risk_formula(
      "\\widehat{p}_n=\\frac{n_A}{n}=\\frac{\\text{liczba zmian ze zdarzeniem}}{\\text{liczba obserwowanych zmian}}",
      num = "1.1",
      legend = c(
        "n" = "liczba porównywalnych obserwacji (tutaj: zmian)",
        "n_A" = "liczba obserwacji, w których zaszło zdarzenie A",
        "\\widehat{p}_n" = "częstość empiryczna po n obserwacjach"
      )
    ),
    risk_example("1.2", "Który korytarz jest bardziej śliski?",
      problem = c(
        "W korytarzu przy dojrzewalni obserwowano 40 zmian; w 3 z nich doszło
         do poślizgnięcia. W korytarzu przy pakowni obserwowano 120 zmian; zdarzenie
         wystąpiło w 5 z nich. Kierownik pakowni twierdzi, że u niego jest gorzej,
         bo „było więcej wypadków”. Oblicz częstości i oceń ten argument."
      ),
      steps = c(
        "Dojrzewalnia: ze wzoru (1.1) p̂ = 3/40 = 0,075.",
        "Pakownia: p̂ = 5/120 ≈ 0,042.",
        "Licznik jest większy w pakowni, ale mianownik jest trzykrotnie większy.
         Na zmianę przypada tam mniej zdarzeń.",
        "Porównanie ma sens tylko wtedy, gdy obie serie używają tej samej definicji
         zdarzenia i tej samej jednostki obserwacji (zmiana w jednym korytarzu)."
      ),
      answer = "0,075 wobec około 0,042 — to dojrzewalnia ma wyższą częstość. Argument
        „więcej wypadków” pomija mianownik. Seria 40 zmian jest jednak krótka, więc
        różnica może częściowo wynikać z przypadku; to sprawdzi symulacja poniżej."
    ),

    lc_h2("ch2-symulacja", "Zobacz stabilizację częstości"),
    lc_p(
      "Aplikacja wylosowała i ukryła modelowe prawdopodobieństwo. Dodawaj kolejne
       fikcyjne zmiany i spróbuj oszacować je na podstawie częstości empirycznej.
       Małe serie mogą wyglądać dramatycznie albo podejrzanie dobrze. Gdy uznasz,
       że danych jest dość, odsłoń wartość przyjętą w modelu."
    ),

    risk_try("zacznij od kilku kliknięć „Dodaj 1 zmianę” i zapisz częstość po
      10 zmianach. Potem dodawaj po 100 i po 1000. Obserwuj, jak zmienia się
      zakres wahań linii. Odsłoń modelowe P dopiero, gdy wpiszesz swoje
      oszacowanie. Na koniec kliknij „Nowa seria” i porównaj początek nowej
      linii z poprzednią."),

    figure_panel(
      label = "Ćwiczenie 2",
      title = "Teoria kontra kolejne zmiany w Bananpolu",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          lc_stack(
            actionButton("ch2_add_1", "Dodaj 1 zmianę", class = "lc-btn-primary", width = "100%"),
            actionButton("ch2_add_10", "Dodaj 10 zmian", class = "lc-btn-primary", width = "100%"),
            actionButton("ch2_add_100", "Dodaj 100 zmian", class = "lc-btn-primary", width = "100%"),
            actionButton("ch2_add_1000", "Dodaj 1000 zmian", class = "lc-btn-primary", width = "100%"),
            actionButton("ch2_reveal", "Odsłoń modelowe P", class = "lc-btn-secondary-outline", width = "100%"),
            actionButton("ch2_reset", "Nowa seria (reset)", class = "lc-btn-secondary-outline", width = "100%")
          ),
          uiOutput("ch2_stats")
        ),
        column(
          8,
          zoom_plot_ui("ch2_convergence", height = "440px")
        )
      )
    ),

    lc_p(
      "Na początku serii linia skacze gwałtownie: po jednej zmianie częstość
       wynosi 0 albo 1, a po kilku zmianach jedno zdarzenie przesuwa ją o
       kilkanaście punktów procentowych. Z każdą kolejną setką zmian pojedyncza
       obserwacja waży coraz mniej, więc linia uspokaja się i zbliża do poziomu,
       który po odsłonięciu okazuje się modelowym prawdopodobieństwem. Dwie
       różne serie mogą na początku wyglądać zupełnie inaczej, a po tysiącu zmian
       leżą blisko siebie."
    ),
    lc_p(
      "To zachowanie ma nazwę: prawo wielkich liczb. Mówi ono, że przy
       niezależnych i porównywalnych obserwacjach częstość empiryczna p̂ₙ z
       coraz większym prawdopodobieństwem leży blisko prawdopodobieństwa p,
       gdy n rośnie. Nie mówi natomiast, że w krótkiej serii częstość będzie
       bliska p, ani że po serii „pechowych” zmian nastąpi seria „szczęśliwych”,
       która wyrówna wynik. Stabilizacja bierze się z rozcieńczania, a nie z
       kompensowania."
    ),
    risk_derivation("jak szybko częstość się stabilizuje", c(
      "Typowe odchylenie częstości p̂ₙ od prawdopodobieństwa p wynosi około
       √(p(1 − p)/n). Wzór wyprowadzimy w wykładzie 04 przy rozkładzie
       dwumianowym; tutaj wystarczy jego skutek.",
      "Przy p = 0,10 typowe odchylenie wynosi około 0,095 po 10 zmianach, 0,030
       po 100 zmianach i 0,0095 po 1000 zmianach. Aby zmniejszyć rozrzut
       dziesięciokrotnie, potrzeba stukrotnie więcej obserwacji. Dlatego seria
       40 zmian z przykładu 1.2 nie wystarcza, by rozstrzygnąć, który korytarz
       jest naprawdę bardziej śliski."
    ), lines = c(
      "n = 10:    √(0,1 · 0,9 / 10)   ≈ 0,095",
      "n = 100:   √(0,1 · 0,9 / 100)  = 0,030",
      "n = 1000:  √(0,1 · 0,9 / 1000) ≈ 0,0095"
    )),
    risk_check("j1_chk_seria",
      "Model przyjmuje P = 0,08 poślizgnięcia na zmianę. W ostatnich 20 zmianach nie było ani jednego zdarzenia. Co z tego wynika?",
      c(
        "Model jest błędny, bo częstość wyniosła 0" = "wrong",
        "Taka seria jest przy P = 0,08 całkiem możliwa; 20 zmian to za mało, by odrzucić model" = "possible",
        "Następne zmiany muszą przynieść więcej zdarzeń, żeby wyrównać średnią" = "compensate"
      ),
      correct = "possible",
      explanation = "Przy niezależnych zmianach seria 20 zmian bez zdarzenia ma prawdopodobieństwo 0,92²⁰ ≈ 0,19 — zdarza się mniej więcej w co piątej takiej serii (rachunek pokażemy w wykładzie 04). Częstość 0 z krótkiej serii nie przeczy modelowi.",
      hints = c(
        wrong = "Częstość z krótkiej serii mocno się waha. Przypomnij sobie początek linii w symulacji.",
        compensate = "Prawo wielkich liczb działa przez rozcieńczanie, nie przez wyrównywanie. Zmiany nie „pamiętają” poprzednich wyników."
      )
    ),

    lc_feedback(
      type = "info",
      tags$strong("Aha:"),
      " prawdopodobieństwo jest własnością modelu, a częstość jest wynikiem
        konkretnej serii obserwacji. Nie oczekujemy, że w każdej małej serii
        będą identyczne."
    ),

    lc_feedback(
      type = "warning",
      tags$strong("Ważne:"),
      " stabilizacja częstości nie naprawia złej definicji zdarzenia, zmiany
        warunków ani błędów rejestracji. Więcej danych nie zastępuje dobrego
        modelu obserwacji."
    ),

    lc_p(
      "Częstość empiryczna odpowiada więc na pytanie „jak często to się
       zdarzało w tych obserwacjach?”. Prawdopodobieństwo odpowiada na pytanie
       „jak często spodziewamy się tego w porównywalnych warunkach?”. Przejście od
       pierwszego do drugiego wymaga założenia, że przyszłe zmiany będą podobne do
       obserwowanych. W następnym rozdziale zobaczymy sytuację, w której
       prawdopodobieństwo można przypisać bez żadnych obserwacji — samym
       rozumowaniem o symetrii."
    ),

    lc_chapter_next(
      num = "03",
      title = "Przestrzeń zdarzeń",
      lead = "Zobaczymy, kiedy wolno liczyć przypadki sprzyjające.",
      target_id = "ch-przestrzen"
    )
  )
)

ch2_server <- function(input, output, session) {
  probability_candidates <- seq(0.01, 0.30, by = 0.01)
  history <- reactiveVal(integer())
  model_probability <- reactiveVal(sample(probability_candidates, 1L))
  probability_revealed <- reactiveVal(FALSE)

  add_days <- function(n) {
    history(append_bernoulli_history(history(), n, model_probability()))
  }

  observeEvent(input$ch2_add_1, add_days(1L))
  observeEvent(input$ch2_add_10, add_days(10L))
  observeEvent(input$ch2_add_100, add_days(100L))
  observeEvent(input$ch2_add_1000, add_days(1000L))
  observeEvent(input$ch2_reveal, probability_revealed(TRUE))
  observeEvent(input$ch2_reset, {
    history(integer())
    model_probability(sample(setdiff(probability_candidates, model_probability()), 1L))
    probability_revealed(FALSE)
  })

  output$ch2_stats <- renderUI({
    observed <- history()
    n <- length(observed)
    events <- sum(observed)
    frequency <- if (n == 0) NA_real_ else mean(observed)

    lc_stat_grid(
      lc_stat_box("Obserwowane zmiany", format(n, big.mark = " ")),
      lc_stat_box("Zmiany ze zdarzeniem", format(events, big.mark = " ")),
      lc_stat_box(
        "Częstość empiryczna",
        if (is.na(frequency)) "—" else format_probability_pl(frequency),
        color = upwr_cat[["niebo"]]
      ),
      lc_stat_box(
        "Prawdopodobieństwo modelowe",
        if (probability_revealed()) {
          format_probability_pl(model_probability())
        } else {
          "Ukryte"
        },
        color = upwr_accent
      ),
      columns = 2
    )
  })

  convergence_plot <- reactive({
    data <- cumulative_frequency(history())
    revealed <- probability_revealed()

    plot <- ggplot(data, aes(x = trial, y = frequency)) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(
        title = "Częstość poślizgnięć w kolejnych zmianach",
        subtitle = if (revealed) {
          "Linia przerywana: prawdopodobieństwo przyjęte w modelu"
        } else {
          "Modelowe prawdopodobieństwo pozostaje ukryte"
        },
        x = "Liczba obserwowanych zmian",
        y = "Skumulowana częstość zdarzenia"
      )

    if (revealed) {
      plot <- plot + geom_hline(
        yintercept = model_probability(),
        colour = upwr_accent,
        linewidth = 0.9,
        linetype = "dashed"
      )
    }

    if (nrow(data) == 0) {
      plot +
        annotate(
          "text",
          x = 1,
          y = 0.55,
          label = "Dodaj pierwsze obserwacje",
          colour = upwr_secondary,
          size = 5
        ) +
        scale_x_continuous(limits = c(0, 2))
    } else {
      plot + geom_line(linewidth = 0.8, colour = upwr_cat[["niebo"]])
    }
  })

  zoom_plot_server(
    "ch2_convergence",
    convergence_plot,
    alt = "Wykres skumulowanej częstości poślizgnięć w kolejnych zmianach. Po odsłonięciu modelu pojawia się linia modelowego prawdopodobieństwa."
  )
}
