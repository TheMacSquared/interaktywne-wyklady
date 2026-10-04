# ============================================================================
# CHAPTER 0: MAPA WYKŁADU — wstęp dla czytelnika
# ============================================================================

.regression_topics <- data.frame(
  order = 1:12,
  topic = c(
    "Po co model regresji?",
    "Regresja liniowa w praktyce",
    "Jak czytać output",
    "Reszty, R² i RMSE",
    "Założenia i granice predykcji",
    "Regresja wieloraka",
    "Predyktory jakościowe",
    "Pominięta zmienna i paradoks Simpsona",
    "Interakcje",
    "Porównywanie modeli",
    "Regresja logistyczna",
    "Ściąga i ćwiczenia"
  ),
  chapter = c(
    "01", "01", "01", "02", "02", "03",
    "03B", "03B", "03B", "04", "05", "06–07"
  ),
  level = c(
    "rdzeń", "rdzeń", "rdzeń", "rdzeń", "pogłębienie", "rdzeń",
    "pogłębienie", "pogłębienie", "pogłębienie", "rdzeń", "rdzeń", "rdzeń"
  ),
  stringsAsFactors = FALSE
)

ch0_map_ui <- list(
  id = "ch-map",
  num = "00",
  title = "Mapa wykładu",
  duration = "5–10 min",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 00 · Regresja",
      num = "00",
      title = "Od związku do przewidywania.",
      lead = paste(
        "Korelacja mówi, że dwie zmienne idą w parze. Regresja mówi, o ile",
        "zmienia się jedna, gdy zmienia się druga, i pozwala tę zmianę przewidzieć."
      )
    ),

    lc_h2("mapa-zaczep", "Skąd przychodzimy"),

    lc_p("W wykładzie 04 ", gloss("korelacja"), " opisywała siłę i kierunek
      liniowego związku dwóch zmiennych jedną liczbą r. Ta liczba nie mówi
      jednak, o ile średnio wzrośnie wynik ucznia, gdy dochód okręgu wzrośnie
      o tysiąc dolarów, ani jakiego wyniku spodziewać się w okręgu, którego nie
      było w danych. Na oba pytania odpowiada ", gloss("regresja liniowa"), ":
      prosta dopasowana do danych ma nachylenie w jednostkach zmiennych
      i nadaje się do przewidywania."),

    lc_p("Z wykładu 05 przychodzą założenia. Normalność, równe wariancje
      i niezależność obserwacji wracają tu w nowej roli: w regresji sprawdza się
      je na resztach, czyli odległościach punktów od dopasowanej prostej.
      Rozdział 02 pokazuje, jak czytać reszty i co oznacza, gdy założenia nie są
      spełnione."),

    lc_h2("mapa-czytanie", "Jak czytać ten wykład"),

    lc_p("Rozdziały oznaczone w tabeli jako rdzeń tworzą jedną historię: od prostej
      z jednym predyktorem, przez ocenę dopasowania i model z wieloma
      predyktorami, po porównywanie modeli i ",
      gloss("regresja logistyczna", "regresję logistyczną"), " dla wyników
      binarnych. Pogłębienia rozwijają wybrane wątki: granice przewidywania,
      ", gloss("zmienna jakościowa", "zmienne jakościowe"), " jako predyktory,
      pominięte zmienne i interakcje. Przy pierwszym czytaniu można je pominąć
      bez utraty głównego wątku, a wrócić do nich przy własnym projekcie
      z wykładu 09."),

    lc_h2("mapa-tematy", "Mapa tematów"),

    lc_table(.regression_topics[, c("order", "topic", "chapter", "level")],
      cols = list(
        lc_col("order", "Nr", "row"),
        lc_col("topic", "Temat", "text"),
        lc_col("chapter", "Rozdział", "text"),
        lc_col("level", "Poziom", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_h2("mapa-przypadki", "Dwa zbiory danych"),

    lc_p("Wykład opiera się na dwóch zbiorach. Dane CASchools o kalifornijskich
      okręgach szkolnych niosą większość rozdziałów: na nich dopasowujemy pierwszą
      prostą, sprawdzamy reszty i budujemy model wieloraki. Dane o pingwinach
      pojawiają się tam, gdzie potrzebne są wyraźne naturalne grupy — przy
      predyktorach jakościowych, paradoksie Simpsona i interakcjach."),

    lc_table(
      data.frame(
        c1 = c("CASchools", "Palmer Penguins"),
        c2 = c(
          "regresję prostą, diagnostykę, model wieloraki i kontekst społeczny",
          "predyktory jakościowe, paradoks Simpsona i interakcje"
        ),
        c3 = c(
          "wynik wymaga ostrożnej interpretacji i dobrze otwiera rozmowę o przyczynowości",
          "trzy naturalne grupy tworzą czytelny mechanizm wizualny"
        )
      ),
      cols = list(
        lc_col("c1", "Przypadek", "row"),
        lc_col("c2", "Najlepiej pokazuje", "text"),
        lc_col("c3", "Dlaczego właśnie te dane", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_chapter_next(
      num = "01",
      title = "Regresja liniowa",
      lead = "Zaczynamy od pytania, prostej i interpretacji współczynników.",
      target_id = "ch-liniowa"
    )
  )
)

ch0_map_server <- function(input, output, session) {
  # Rozdział jest statyczną mapą — nie ma elementów reaktywnych.
  invisible(NULL)
}
