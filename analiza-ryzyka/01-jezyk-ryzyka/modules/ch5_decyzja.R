# ==========================================================================
# ROZDZIAŁ 5: OD PRAWDOPODOBIEŃSTWA DO DECYZJI
# ==========================================================================

ch5_ui <- lecture_chapter(
  id = "ch-decyzja",
  num = "05",
  title = "Od liczby do decyzji",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 05 · Język ryzyka",
      num = "05",
      title = "Który problem powinien być pierwszy?",
      lead = "Dyrektor Bananpolu dostał dwa wyniki dotyczące bezpieczeństwa.
              Obejrzyj dwa przypadki i wybierz najlepiej uzasadniony wniosek.
              Dopiero potem porównamy możliwe sposoby rozumowania."
    ),

    lc_h2("ch5-porownanie", "Dwa problemy dyrektora Bananpolu"),
    lc_p(
      "Poniższe liczby są fikcyjne i służą wyłącznie temu ćwiczeniu. Porównaj
       oba przypadki, zwracając uwagę na dokładne brzmienie każdej odpowiedzi."
    ),

    lc_stat_grid(
      lc_stat_box(
        "A · Poślizgnięcie",
        "P = 0,08 na zmianę",
        caption = "Możliwy skutek: od braku urazu do złamania",
        color = upwr_cat[["bursztyn"]]
      ),
      lc_stat_box(
        "B · Kolizja z wózkiem",
        "P = 0,002 na zmianę",
        caption = "Możliwy skutek: ciężki lub śmiertelny uraz",
        color = upwr_cat[["terakota"]]
      ),
      columns = 2
    ),

    lc_p(
      "Obie liczby łatwiej porównać jako częstości naturalne. P = 0,08 na zmianę
       oznacza w modelu około 80 zmian z poślizgnięciem na 1000 zmian w tym
       korytarzu. P = 0,002 oznacza około 2 zmiany z kolizją na 1000 zmian w
       strefie transportu. Poślizgnięcie jest więc czterdzieści razy częstsze.
       Pytanie brzmi, czy ta różnica sama rozstrzyga, czym dyrektor powinien
       zająć się najpierw."
    ),
    risk_try("zanim klikniesz „Sprawdź rozumowanie”, zapisz jednym zdaniem,
      czego brakuje w każdej z trzech pierwszych odpowiedzi. Potem wybierz
      odpowiedź i porównaj swoje uzasadnienie z komentarzem."),

    figure_panel(
      label = "Decyzja",
      title = "Jaki priorytet można teraz uzasadnić?",
      radioButtons(
        "ch5_priority",
        "Wybierz najlepiej uzasadnione stwierdzenie",
        choices = c(
          "Najpierw A, bo ma większe prawdopodobieństwo" = "a",
          "Najpierw B, bo może mieć cięższy skutek" = "b",
          "Oba problemy mają takie samo ryzyko" = "equal",
          "Same prawdopodobieństwa nie wystarczają do ustalenia priorytetu" = "insufficient"
        ),
        selected = character(0)
      ),
      actionButton("ch5_check", "Sprawdź rozumowanie", class = "lc-btn-primary"),
      uiOutput("ch5_feedback")
    ),

    lc_p(
      "Każda z pierwszych trzech odpowiedzi opiera się na jednym wymiarze
       problemu. Większe prawdopodobieństwo poślizgnięcia jest faktem, ale nie
       mówi nic o skutkach. Cięższy skutek kolizji też jest faktem, ale pomija
       to, jak rzadko do niej dochodzi. Stwierdzenie o „takim samym ryzyku”
       wymagałoby reguły, która zamienia prawdopodobieństwo i skutek na jedną
       liczbę — a takiej reguły nikt jeszcze nie ustalił. Poprawna odpowiedź nie
       jest uchylaniem się od decyzji, tylko wskazaniem, jakich informacji
       brakuje, żeby ją podjąć."
    ),

    margin_callout(
      label = "Granica wykładu",
      "Ten kurs buduje przede wszystkim składową probabilistyczną analizy.
       Skutków nie zamieniamy automatycznie w pieniądze ani punkty.",
      color = "uwaga"
    ),

    lc_h2("ch5-horyzont", "Prawdopodobieństwo zawsze ma horyzont"),
    lc_p(
      "Obie liczby dotyczą jednej zmiany. Dyrektor myśli jednak w horyzoncie
       roku: ile razy w ciągu 250 zmian może dojść do kolizji? Porównywanie
       prawdopodobieństw liczonych dla różnych horyzontów — na zmianę, na
       miesiąc, na rok — jest jednym z najczęstszych błędów w raportach
       bezpieczeństwa. Każde prawdopodobieństwo musi mieć podaną jednostkę tak
       samo jak częstość z definicji 1.2 ma swój mianownik."
    ),
    lc_p(
      "Przejście od jednej zmiany do roku wymaga założeń o tym, jak zmiany są
       ze sobą powiązane; zrobimy to porządnie w wykładzie 04. Już teraz wzory
       z tego wykładu pozwalają jednak wykryć błąd, który pojawia się bardzo
       często: mnożenie prawdopodobieństwa na zmianę przez liczbę zmian."
    ),
    risk_example("1.6", "Czy 250 · 0,002 to prawdopodobieństwo w roku?",
      problem = c(
        "Analityk pisze: „P(kolizji na zmianę) = 0,002, w roku jest 250 zmian,
         więc P(co najmniej jednej kolizji w roku) = 250 · 0,002 = 0,5”. Tą samą
         metodą dla poślizgnięcia dostałby 250 · 0,08. Oceń ten rachunek."
      ),
      steps = c(
        "Zdarzenie „co najmniej jedna kolizja w roku” to suma zdarzeń K₁ ∪ K₂ ∪ … ∪ K₂₅₀,
         gdzie Kᵢ oznacza kolizję na i-tej zmianie.",
        "Dodawanie prawdopodobieństw jest poprawne tylko dla zdarzeń rozłącznych
         (aksjomat (1.7)). Kolizje na różnych zmianach nie są rozłączne — w roku
         mogą zdarzyć się dwie.",
        "Ze wzoru (1.5) dla dwóch zdarzeń: P(K₁ ∪ K₂) = P(K₁) + P(K₂) − P(K₁ ∩ K₂)
         ≤ P(K₁) + P(K₂). Suma prawdopodobieństw jest więc tylko górnym
         ograniczeniem, bo pomija odjęcie części wspólnych.",
        "Dla poślizgnięcia 250 · 0,08 = 20 — liczba większa od 1, co łamie
         własność (1.3). To ostateczny dowód, że metoda jest błędna.",
        "Przy dodatkowym założeniu niezależności zmian (wykład 04) i wzorze (1.4):
         P(co najmniej jednej kolizji) = 1 − 0,998²⁵⁰ ≈ 0,394, a dla poślizgnięcia
         1 − 0,92²⁵⁰ — praktycznie 1."
      ),
      answer = "0,5 to górne ograniczenie, nie prawdopodobieństwo; przy niezależnych
        zmianach właściwa wartość to około 0,39. Iloczyn 250 · 0,002 = 0,5 ma
        jednak inną, poprawną interpretację: średnio pół kolizji na rok, czyli
        około jedna kolizja na dwa lata."
    ),
    lc_p(
      "Przykład pokazuje, że nawet bez danych o skutkach porównanie wymaga
       uzgodnienia horyzontu. W skali roku poślizgnięcie w tym korytarzu jest w
       modelu praktycznie pewne, a kolizja ma szansę mniej więcej 2 do 5. Obie
       liczby są wysokie, ale opisują zupełnie różne zdarzenia — i to prowadzi
       nas do profilu ryzyka."
    ),

    lc_h2("ch5-profil", "Profil dwóch problemów"),
    lc_p(
      "Dla obu problemów warto zapisać profil: definicję zdarzenia,
       prawdopodobieństwo wraz z horyzontem, możliwe skutki, liczbę osób
       eksponowanych, niepewność danych, działające bariery oraz dostępne
       działania. Dopiero wtedy można zastosować jawne kryteria priorytetu."
    ),

    figure_panel(
      label = "Profil ryzyka",
      full_width = TRUE,
      tags$table(
        class = "lc-table lc-table-striped lc-table-bordered",
        tags$thead(tags$tr(
          tags$th("Pytanie"), tags$th("Poślizgnięcie"), tags$th("Kolizja z wózkiem")
        )),
        tags$tbody(
          tags$tr(tags$td("Co może się zdarzyć?"), tags$td("Upadek w korytarzu"), tags$td("Potrącenie pieszego")),
          tags$tr(tags$td("Kto jest eksponowany?"), tags$td("Osoby korzystające z przejścia"), tags$td("Piesi w strefie transportu")),
          tags$tr(tags$td("Jakie są skutki?"), tags$td("Różna dotkliwość urazu"), tags$td("Możliwy uraz ciężki")),
          tags$tr(tags$td("Jakie bariery działają?"), tags$td("Sprzątanie i oznakowanie"), tags$td("Separacja ruchu i ograniczenie prędkości")),
          tags$tr(tags$td("Czego nie wiemy?"), tags$td("Kompletność rejestru"), tags$td("Ruch pieszych i zdarzenia bliskie wypadku"))
        )
      )
    ),

    lc_h2("ch5-macierz", "Jak czytać macierz ryzyka"),
    lc_p(
      "Kategorie „rzadkie”, „możliwe”, „poważne” i „katastrofalne” pomagają
       porządkować dyskusję, ale są skalami porządkowymi. Iloczyn numerów pól
       1–5 nie staje się automatycznie ilościową miarą ryzyka. Granice kategorii
       i reguły decyzji muszą być jawne."
    ),

    lc_p(
      "Problem z iloczynem numerów pól łatwo zobaczyć na przykładzie. Zdarzenie
       o częstości „5 — prawie pewne” i skutku „2 — drobny” dostaje 10 punktów,
       tak samo jak zdarzenie o częstości „2 — rzadkie” i skutku „5 —
       katastrofalny”. Równa liczba punktów nie oznacza równego ryzyka — oznacza
       tylko, że tak wypadła arytmetyka na numerach kategorii. Odstępy między
       kategoriami też nie są równe: przejście od „rzadkiego” do „możliwego” może
       oznaczać dziesięciokrotny wzrost prawdopodobieństwa, a od „drobnego” do
       „poważnego” — zupełnie inny rodzaj szkody."
    ),
    risk_check("j1_chk_macierz",
      "W macierzy 5 × 5 zdarzenie X ma pole (prawdopodobieństwo 5, skutek 2), a zdarzenie Y — pole (2, 5). Oba dostają iloczyn 10. Co z tego wynika?",
      c(
        "Oba zdarzenia mają to samo ryzyko" = "same",
        "Iloczyn numerów kategorii porządkowych nie jest miarą ryzyka; potrzebne są jawne reguły priorytetu" = "ordinal",
        "Y jest ważniejsze, bo skutek zawsze przeważa" = "severity"
      ),
      correct = "ordinal",
      explanation = "Numery kategorii są etykietami porządku, a nie wielkościami, które wolno mnożyć. Równy iloczyn nie oznacza równego ryzyka — decyzja wymaga jawnych kryteriów, np. progu dla ciężkich skutków niezależnie od częstości.",
      hints = c(
        same = "Czy odstęp między kategoriami 1 i 2 musi być taki sam jak między 4 i 5? Co wtedy znaczy iloczyn?",
        severity = "To może być rozsądna reguła, ale trzeba ją jawnie przyjąć. Sama macierz jej nie zawiera."
      )
    ),

    lc_feedback(
      type = "ok",
      tags$strong("Dobra praktyka:"),
      " wynik probabilistyczny kończ zdaniem: co ten wynik zmienia, jakiego
        skutku dotyczy i które założenie jest najważniejsze."
    ),

    lc_chapter_next(
      num = "06",
      title = "Ściąga",
      lead = "Zbierzemy cały wykład w jedną mapę pojęć i pytań.",
      target_id = "ch-sciaga"
    )
  )
)

ch5_server <- function(input, output, session) {
  check_count <- reactiveVal(0L)

  observeEvent(input$ch5_check, {
    check_count(check_count() + 1L)
  })

  output$ch5_feedback <- renderUI({
    req(check_count() > 0)
    answer <- input$ch5_priority
    if (is.null(answer)) answer <- ""
    is_correct <- identical(answer, "insufficient")

    lc_feedback(
      type = if (is_correct) "ok" else "warning",
      tags$strong(if (is_correct) "Dobrze." else "To zbyt szybki ranking."),
      if (is_correct) {
        paste(
          "Prawdopodobieństwa odpowiadają tylko na część pytania.",
          "Priorytet wymaga jawnych kryteriów skutku, ekspozycji, barier i wykonalności działań."
        )
      } else {
        paste(
          "Wybrany argument może być ważny, ale sam nie wystarcza.",
          "Najpierw uzupełnij profil obu problemów i kryteria decyzji."
        )
      }
    )
  })
}
