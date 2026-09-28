# ==========================================================================
# ROZDZIAŁ 3: PRZESTRZEŃ ZDARZEŃ I DEFINICJA KLASYCZNA
# ==========================================================================

ch3_ui <- lecture_chapter(
  id = "ch-przestrzen",
  num = "03",
  title = "Spośród czego liczymy?",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 03 · Język ryzyka",
      num = "03",
      title = "Mianownik jest częścią modelu.",
      lead = "Definicja klasyczna działa wtedy, gdy potrafimy wymienić wyniki i
              uzasadnić, że są jednakowo możliwe. W Bananpolu nadaje się do
              losowania palety do kontroli, nie do przewidywania każdego wypadku."
    ),

    margin_callout(
      label = "Eksperyment",
      "Inspektor losuje dokładnie jedną z 24 palet. Każda paleta ma własny
       numer w generatorze losowym i tę samą szansę wyboru.",
      color = "ok"
    ),

    lc_p(
      "Do Bananpolu przyjechała dostawa 24 palet. Inspektor nie ma czasu
       skontrolować wszystkich, więc losuje jedną i sprawdza zabezpieczenie
       ładunku. Przed losowaniem chce wiedzieć, jak duża jest szansa, że trafi na
       paletę z uszkodzonym zabezpieczeniem, jeśli w dostawie jest ich sześć.
       Tym razem nie potrzebujemy rejestru z poprzednich miesięcy: odpowiedź
       wynika z samej konstrukcji losowania. Żeby ją zapisać porządnie,
       potrzebujemy trzech pojęć — doświadczenia, przestrzeni i zdarzenia."
    ),

    lc_h2("ch3-przestrzen", "Wyniki i zdarzenia"),
    risk_definition("1.3", "Doświadczenie losowe i przestrzeń zdarzeń elementarnych", c(
      "Doświadczenie losowe to procedura, którą można (przynajmniej w myśli)
       powtarzać w tych samych warunkach i której wyniku nie znamy z góry.
       Każdy możliwy, niepodzielny wynik doświadczenia nazywamy zdarzeniem
       elementarnym i oznaczamy ω.",
      "Zbiór wszystkich zdarzeń elementarnych to przestrzeń zdarzeń
       elementarnych Ω. Musi ona spełniać dwa warunki: każdy przebieg
       doświadczenia kończy się jakimś elementem Ω (wyczerpywalność) i nigdy
       dwoma naraz (wzajemne wykluczanie)."
    )),
    lc_p(
      "Przestrzeń wyników zawiera wszystkie palety, które mogą zostać wybrane.
       Zdarzenie A jest podzbiorem: paletami z uszkodzonym zabezpieczeniem
       ładunku. Losujemy paletę, a nie uszkodzenie."
    ),
    risk_definition("1.4", "Zdarzenie losowe", c(
      "Zdarzeniem losowym nazywamy podzbiór A przestrzeni Ω. Mówimy, że
       zdarzenie A zaszło, jeśli wynik doświadczenia ω należy do A (ω ∈ A).",
      "Szczególne przypadki: zdarzenie elementarne {ω} zawiera jeden wynik;
       zdarzenie pewne to cała przestrzeń Ω — zachodzi zawsze; zdarzenie
       niemożliwe to zbiór pusty ∅ — nie zachodzi nigdy. Liczbę elementów
       zdarzenia A oznaczamy |A|."
    )),
    lc_p(
      "W losowaniu palety Ω = {1, 2, …, 24}, czyli |Ω| = 24. Jeśli uszkodzone
       zabezpieczenie mają palety 1–6, to A = {1, 2, 3, 4, 5, 6} i |A| = 6.
       Zauważ, że zdarzenie nie jest „rzeczą, która się dzieje”, tylko zbiorem
       wyników, przy których uznajemy, że coś zaszło. Ta zmiana perspektywy
       pozwala potem na zdarzeniach wykonywać działania jak na zbiorach."
    ),
    lc_p(
      "Pozostaje pytanie, ile wynosi P(A). Jeśli procedura losowania sprawia, że
       żadna paleta nie jest wyróżniona, to każdej z 24 palet przypisujemy tę samą
       szansę 1/24. Zdarzenie A obejmuje sześć takich jednakowo możliwych wyników,
       więc jego prawdopodobieństwo to 6 · 1/24. Uogólnienie tego rozumowania to
       klasyczna definicja prawdopodobieństwa, pochodząca od Laplace’a."
    ),
    risk_definition("1.5", "Klasyczna definicja prawdopodobieństwa", c(
      "Jeżeli przestrzeń Ω jest skończona, a wszystkie zdarzenia elementarne są
       jednakowo możliwe, to prawdopodobieństwem zdarzenia A ⊆ Ω nazywamy
       iloraz liczby zdarzeń elementarnych sprzyjających A do liczby wszystkich
       zdarzeń elementarnych (wzór 1.2)."
    )),

    risk_formula(
      "P(A)=\\frac{|A|}{|\\Omega|}=\\frac{\\text{liczba wyników sprzyjających}}{\\text{liczba jednakowo możliwych wyników}}",
      num = "1.2",
      legend = c(
        "|A|" = "liczba zdarzeń elementarnych należących do A",
        "|\\Omega|" = "liczba wszystkich zdarzeń elementarnych"
      )
    ),
    lc_p(
      "Dla dostawy Bananpolu P(A) = 6/24 = 0,25. Oba założenia definicji są
       tu spełnione z konstrukcji: palet jest skończenie wiele, a równe szanse
       gwarantuje generator losowy, który przypisuje każdej palecie jeden numer.
       Gdyby inspektor wybierał „na oko” paletę stojącą najbliżej drzwi, drugie
       założenie przestałoby obowiązywać, choć liczby 6 i 24 by się nie zmieniły."
    ),
    risk_example("1.3", "Losowanie zmiany do audytu",
      problem = c(
        "Audytor losuje jedną zmianę z tygodnia roboczego: jeden z pięciu dni
         (poniedziałek–piątek) i jedną z trzech zmian (ranna, popołudniowa,
         nocna); każda para dzień–zmiana ma tę samą szansę. Wypisz przestrzeń Ω
         i oblicz prawdopodobieństwa zdarzeń A — wylosowano zmianę nocną oraz
         B — wylosowano piątek."
      ),
      steps = c(
        "Zdarzeniem elementarnym jest para (dzień, zmiana), np. (pon, ranna).
         Samo „poniedziałek” nie jest wynikiem elementarnym, bo nie mówi, którą
         zmianę wylosowano.",
        "Ω zawiera 5 · 3 = 15 par, wszystkie jednakowo możliwe — definicja 1.5 ma
         zastosowanie.",
        "A = {(pon, nocna), (wt, nocna), (śr, nocna), (czw, nocna), (pt, nocna)},
         |A| = 5, więc ze wzoru (1.2) P(A) = 5/15 = 1/3 ≈ 0,333.",
        "B = {(pt, ranna), (pt, popołudniowa), (pt, nocna)}, |B| = 3, więc
         P(B) = 3/15 = 0,2."
      ),
      answer = "|Ω| = 15, P(A) = 1/3, P(B) = 0,2. Do tych samych zdarzeń wrócimy
        w przykładzie 1.4, łącząc je spójnikami „i” oraz „lub”."
    ),

    lc_h2("ch3-slownik", "Te same pojęcia w języku formalnym"),
    lc_p(
      "Podręczniki rachunku prawdopodobieństwa używają kilku stałych nazw.
       Wszystkie już znasz z przykładu palety — tutaj tylko je porządkujemy."
    ),
    figure_panel(
      label = "Słownik",
      title = "Losowanie palety w terminologii formalnej",
      full_width = TRUE,
      tags$table(
        class = "lc-table lc-table-striped lc-table-bordered",
        tags$thead(tags$tr(
          tags$th("Termin"),
          tags$th("Znaczenie"),
          tags$th("W Bananpolu")
        )),
        tags$tbody(
          tags$tr(tags$td("Doświadczenie losowe"), tags$td("Powtarzalna procedura o niepewnym wyniku"), tags$td("Losowanie jednej palety do kontroli")),
          tags$tr(tags$td("Wynik elementarny"), tags$td("Pojedynczy, niepodzielny wynik doświadczenia"), tags$td("Numer wylosowanej palety")),
          tags$tr(tags$td("Przestrzeń wyników Ω"), tags$td("Zbiór wszystkich wyników elementarnych"), tags$td("Wszystkie 24 palety")),
          tags$tr(tags$td("Zdarzenie A"), tags$td("Dowolny podzbiór przestrzeni Ω"), tags$td("Palety z uszkodzonym zabezpieczeniem")),
          tags$tr(tags$td("Zdarzenie pewne"), tags$td("Cała przestrzeń Ω — zachodzi zawsze"), tags$td("Wylosowano którąś z 24 palet")),
          tags$tr(tags$td("Zdarzenie niemożliwe"), tags$td("Zbiór pusty ∅ — nie zachodzi nigdy"), tags$td("Wylosowano paletę numer 25"))
        )
      )
    ),

    lc_p(
      "Z definicji klasycznej wynikają trzy podstawowe własności. Możesz je
       sprawdzić suwakiem poniżej: ustaw 0 palet sprzyjających (zdarzenie
       niemożliwe), potem 24 (zdarzenie pewne)."
    ),
    risk_formula(
      "P(\\Omega)=1,\\qquad P(\\emptyset)=0,\\qquad 0\\le P(A)\\le 1",
      num = "1.3"
    ),
    lc_p(
      "Prawdopodobieństwo zdarzenia pewnego wynosi 1, niemożliwego 0,
       a każdego innego zdarzenia — wartość pomiędzy."
    ),
    risk_derivation("własności (1.3) z definicji klasycznej", c(
      "Wystarczy policzyć elementy. Zdarzenie pewne to cała przestrzeń, więc
       jego licznik jest równy mianownikowi. Zbiór pusty nie ma elementów.
       Każde zdarzenie A jest podzbiorem Ω, więc ma od 0 do |Ω| elementów."
    ), lines = c(
      "P(Ω) = |Ω| / |Ω| = 1",
      "P(∅) = |∅| / |Ω| = 0 / |Ω| = 0",
      "∅ ⊆ A ⊆ Ω   ⇒   0 ≤ |A| ≤ |Ω|   ⇒   0 ≤ P(A) ≤ 1"
    )),
    lc_p(
      "Własności (1.3) są prostym, ale skutecznym testem poprawności każdego
       rachunku w tym kursie. Jeśli wynik wychodzi ujemny albo większy od
       jedności, błąd jest gdzieś wcześniej: w liczniku, w mianowniku albo w
       tym, że dodano coś dwa razy. Ten ostatni przypadek zobaczymy w następnym
       rozdziale na diagramie Venna."
    ),

    lc_h2("ch3-paletki", "Zbuduj zdarzenie na siatce palet"),
    lc_p(
      "Zmieniaj liczbę palet z uszkodzonym zabezpieczeniem. Siatka pokazuje
       pełny mianownik, zdarzenie A oraz jego dopełnienie."
    ),
    risk_try("ustaw 6 palet i odczytaj P(A) oraz P(Aᶜ). Potem przesuń suwak na
      0 i na 24 i sprawdź, które kafelki zmieniają kolor. Na koniec dodaj w
      pamięci obie wartości P(A) i P(Aᶜ) dla kilku ustawień suwaka."),

    figure_panel(
      label = "Ćwiczenie 3",
      title = "Losowa kontrola jednej palety",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput(
            "ch3_favourable",
            "Palety z uszkodzonym zabezpieczeniem",
            min = 0,
            max = 24,
            value = 6,
            step = 1
          ),
          uiOutput("ch3_stats"),
          lc_feedback(
            type = "info",
            "Zdarzenie A: wylosowana paleta ma uszkodzone zabezpieczenie."
          )
        ),
        column(
          8,
          zoom_plot_ui("ch3_grid", height = "430px")
        )
      )
    ),

    lc_p(
      "Siatka pokazuje całe Ω naraz: 24 kafelki to mianownik, kafelki w kolorze
       zdarzenia A to licznik. Przy sześciu uszkodzonych paletach P(A) = 0,25, a
       pozostałe 18 kafelków tworzy dopełnienie Aᶜ o prawdopodobieństwie 0,75.
       Przy ustawieniu 0 zdarzenie A staje się zbiorem pustym, a przy 24 —
       całą przestrzenią; obie skrajności to własności (1.3). Niezależnie od
       położenia suwaka P(A) i P(Aᶜ) sumują się do 1, bo każdy kafelek ma
       dokładnie jeden z dwóch kolorów. Tę obserwację zapiszemy jako wzór (1.4)
       w następnym rozdziale."
    ),

    lc_h2("ch3-granica", "Kiedy ten iloraz nie wystarcza"),
    lc_p(
      "Palety są jednakowo możliwe, bo wymusza to procedura losowania. Realne
       awarie maszyn, pożary i upadki nie tworzą zwykle listy symetrycznych
       przypadków. Ich prawdopodobieństwa zależą od warunków, ekspozycji,
       historii i zabezpieczeń. Wtedy potrzebujemy danych albo innego modelu."
    ),

    lc_p(
      "Najczęstszy błąd polega na tym, że wypisujemy możliwe wyniki, liczymy je
       i dzielimy, nie sprawdzając, czy są jednakowo możliwe. Każdą zmianę można
       opisać jako „było poślizgnięcie” albo „nie było”, ale to nie znaczy, że
       każdy z tych wyników ma szansę 1/2. Założenie równych szans nie wynika z
       liczby wyników, tylko z mechanizmu doświadczenia: z generatora losowego,
       symetrii urządzenia albo procedury wyboru. Tam, gdzie takiego mechanizmu
       nie ma, wracamy do częstości z rozdziału 02 albo budujemy model."
    ),
    risk_check("j1_chk_symetria",
      "Kolega proponuje: „Zmiana może skończyć się wypadkiem albo nie, więc P(wypadek) = 1/2 z definicji klasycznej”. Co jest nie tak?",
      c(
        "Nic — dwa wyniki, jeden sprzyjający, więc 1/2" = "ok",
        "Przestrzeń jest źle wypisana, bo ma tylko dwa elementy" = "size",
        "Nic nie uzasadnia, że oba wyniki są jednakowo możliwe" = "symmetry"
      ),
      correct = "symmetry",
      explanation = "Definicja 1.5 wymaga jednakowo możliwych zdarzeń elementarnych. Dwuelementowa przestrzeń jest w porządku, ale żaden mechanizm nie nadaje wypadkowi i jego brakowi równych szans — dlatego to prawdopodobieństwo trzeba oszacować z danych.",
      hints = c(
        ok = "Sprawdź oba warunki definicji 1.5. Który z nich nie jest tu uzasadniony?",
        size = "Przestrzeń dwuelementowa jest dopuszczalna — rzut monetą też ma dwa wyniki. Czym moneta różni się od zmiany w magazynie?"
      )
    ),

    lc_feedback(
      type = "warning",
      tags$strong("Pułapka:"),
      " „jedna z 24 palet” opisuje losowanie do kontroli. Nie oznacza, że
        ryzyko uszkodzenia każdej palety powstało z klasycznej symetrii."
    ),

    lc_chapter_next(
      num = "04",
      title = "Zdarzenia się łączą",
      lead = "Przetłumaczymy słowa „lub”, „i” oraz „nie” na działania na zbiorach.",
      target_id = "ch-zbiory"
    )
  )
)

ch3_server <- function(input, output, session) {
  pallet_data <- reactive({
    req(input$ch3_favourable)
    build_pallet_grid(input$ch3_favourable, total = 24L, columns = 6L)
  })

  output$ch3_stats <- renderUI({
    req(input$ch3_favourable)
    favourable <- as.integer(input$ch3_favourable)
    probability <- classical_probability(favourable, 24L)

    lc_stat_grid(
      lc_stat_box("Licznik |A|", favourable, color = upwr_cat[["terakota"]]),
      lc_stat_box("Mianownik |Ω|", 24, color = upwr_secondary),
      lc_stat_box("P(A)", format_probability_pl(probability), color = upwr_accent),
      lc_stat_box("P(Aᶜ)", format_probability_pl(1 - probability), color = upwr_cat[["szalwia"]]),
      columns = 2
    )
  })

  pallet_plot <- reactive({
    data <- pallet_data()
    data$status <- ifelse(data$favourable, "event", "complement")

    ggplot(data, aes(x = column, y = -row, fill = status)) +
      geom_tile(colour = "white", linewidth = 2, width = 0.92, height = 0.92) +
      geom_text(aes(label = id), colour = "white", fontface = "bold", size = 4) +
      scale_fill_manual(
        values = c(
          "complement" = upwr_reference,
          "event" = upwr_cat[["terakota"]]
        ),
        breaks = c("complement", "event"),
        labels = expression("Dopełnienie " * A^c, "Zdarzenie A")
      ) +
      coord_equal() +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      labs(
        title = "Przestrzeń 24 jednakowo możliwych wyników",
        subtitle = "Kolor wskazuje, czy wynik należy do zdarzenia A",
        x = NULL,
        y = NULL,
        fill = NULL
      ) +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom"
      )
  })

  zoom_plot_server(
    "ch3_grid",
    pallet_plot,
    alt = paste(
      "Siatka 24 palet. Część palet należy do zdarzenia",
      "wylosowania palety z uszkodzonym zabezpieczeniem."
    )
  )
}
