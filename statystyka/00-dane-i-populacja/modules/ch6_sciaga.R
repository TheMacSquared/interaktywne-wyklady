# ============================================================================
# CHAPTER 6: Ściąga
# ============================================================================

# Tabela tekstowa: pierwsza kolumna jako nagłówek wiersza, na wąskim karty.
.ch6_cheat_table <- function(df, labels) {
  keys <- names(df)
  cols <- lapply(seq_along(keys), function(i) {
    lc_col(keys[i], labels[i], if (i == 1) "row" else "text")
  })
  lc_table(df, cols, narrow = "cards")
}

.ch6_terms_table <- .ch6_cheat_table(
  data.frame(
    a = c("Obserwacja", "Jednostka obserwacji", "Zmienna", "Populacja",
          "Próba", "Operat losowania", "Parametr", "Statystyka",
          "Zmienność próbkowa", "Obciążenie"),
    b = c("Wszystko, co zapisano o jednej jednostce (jeden wiersz tabeli).",
          "To, czego dotyczy jeden wiersz: osoba, firma, dzień, przejazd.",
          "Cecha zapisana dla każdej obserwacji (jedna kolumna tabeli).",
          "Wszystkie jednostki, o których chcemy wyciągnąć wniosek; liczebność N.",
          "Zbadana część populacji; liczebność n.",
          "Lista jednostek, z której losujemy próbę.",
          "Liczba opisująca populację; stała, zwykle nieznana.",
          "Liczba policzona z próby; znana, zmienia się od próby do próby.",
          "Różnice wartości statystyki między kolejnymi próbami.",
          "Systematyczny błąd w jedną stronę, którego nie usuwa większe n."),
    c = c("Jeden student wydziału z jego rokiem, dojazdem i pracą.",
          "Osoba albo pojedynczy przejazd: zależy od pytania.",
          "Czas dojazdu w minutach.",
          "Wszyscy studenci, którzy pisali egzamin, N = 2400.",
          "50 losowych kolegów, do których zadzwoniłeś.",
          "Lista obecności na egzaminie.",
          "p: odsetek wszystkich studentów, którzy zdali.",
          "p̂: odsetek zdających w wywołanej grupce.",
          "p̂ = 0.64 w jednej grupce, 0.72 w następnej.",
          "Ankieta w bibliotece zawyża odsetek zdających."),
    stringsAsFactors = FALSE
  ),
  c("Pojęcie", "Co to jest", "Przykład")
)

.ch6_notation_table <- .ch6_cheat_table(
  data.frame(
    a = c("Liczebność", "Średnia", "Odchylenie standardowe", "Odsetek"),
    b = c("N", "μ", "σ", "p"),
    c = c("n", "x̄", "s", "p̂"),
    stringsAsFactors = FALSE
  ),
  c("Miara", "Populacja (parametr)", "Próba (statystyka)")
)

.ch6_sampling_table <- .ch6_cheat_table(
  data.frame(
    a = c("Losowanie proste", "Losowanie warstwowe", "Próba wygodna"),
    b = c("Każda jednostka z operatu ma tę samą szansę.",
          "Losowanie osobno w każdej warstwie, proporcjonalnie do jej wielkości.",
          "Badamy tych, do których łatwo dotrzeć (np. siedzących w bibliotece), albo tych, którzy sami się zgłosili."),
    c = c("Brak obciążenia; rozrzut maleje z n.",
          "Brak obciążenia; proporcje warstw dokładne; mniejszy rozrzut, gdy warstwy się różnią.",
          "Zwykle obciążona; większe n zwęża rozrzut wokół złej wartości."),
    stringsAsFactors = FALSE
  ),
  c("Sposób", "Jak działa", "Skutek dla statystyki")
)

.ch6_mistakes_table <- .ch6_cheat_table(
  data.frame(
    a = c("Pomiary tej samej osoby liczone jako osobne osoby",
          "Wynik z próby podany jako pewny fakt o populacji",
          "„Mamy 10 000 odpowiedzi, więc wynik jest wiarygodny”",
          "Populacja określona dopiero po zebraniu danych"),
    b = c("Zawyża n i udaje więcej informacji, niż jest w danych.",
          "Statystyka zmienia się od próby do próby; parametr znamy tylko w przybliżeniu.",
          "Duże n nie usuwa obciążenia, gdy próba nie była losowa.",
          "Nie wiadomo, o kim jest wniosek ani czy operat obejmował wszystkich."),
    c = c("Ustal jednostkę obserwacji na podstawie pytania.",
          "Podawaj wynik z miarą niepewności (wykład 03).",
          "Najpierw zapytaj, jak powstała próba, potem patrz na n.",
          "Zdefiniuj populację i operat przed badaniem."),
    stringsAsFactors = FALSE
  ),
  c("Błąd", "Dlaczego to błąd", "Co zrobić zamiast")
)

ch6_ui <- list(
  id    = "ch-sciaga",
  num   = "06",
  title = "Ściąga",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 06 · Dane i populacja",
      num    = "06",
      title  = "Wszystko na jednej stronie.",
      lead   = "Pojęcia, oznaczenia, sposoby doboru próby i typowe błędy z tego
                wykładu, zebrane do szybkiego przeglądu przed kolejnymi
                wykładami."
    ),

    lc_h2("ch6-pojecia", "Pojęcia"),
    .ch6_terms_table,

    lc_h2("ch6-oznaczenia", "Oznaczenia"),
    .ch6_notation_table,

    lc_h2("ch6-dobor", "Dobór próby"),
    .ch6_sampling_table,

    lc_h2("ch6-bledy", "Typowe błędy"),
    .ch6_mistakes_table,

    lc_chapter_next(
      num       = "07",
      title     = "Quiz",
      lead      = "populacja, próba, parametr czy statystyka?",
      target_id = "ch-quiz"
    )
  )
)

ch6_server <- function(input, output, session) {}
