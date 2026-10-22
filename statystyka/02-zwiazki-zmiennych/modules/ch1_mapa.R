# ============================================================================
# CHAPTER 1: Mapa — co to znaczy, że dwie zmienne są związane
# ============================================================================

ch1_ui <- list(
  id = "ch-mapa", num = "01", title = "Mapa",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 01 · Związki dwóch zmiennych",
      num    = "01",
      title  = "Jedna zmienna mówi coś o drugiej.",
      lead   = "Czy mężczyźni częściej dostają mandaty? Czy absolwenci jednej
                branży zarabiają więcej niż drugiej? Czy pensja rośnie ze stażem?
                Każde z tych pytań dotyczy dwóch zmiennych naraz, a sposób,
                w jaki na nie odpowiadamy, zależy od ich typu."
    ),

    lc_p("W wykładzie 01 każdą zmienną opisywaliśmy osobno: liczyliśmy
      częstości, średnie i odchylenia, rysowaliśmy histogramy i wykresy
      słupkowe. Większość ciekawych pytań dotyczy jednak dwóch zmiennych
      mierzonych na tych samych obiektach. Pytamy wtedy, czy znajomość
      jednej z nich mówi nam coś o drugiej."),

    lc_p("Jeśli mówi, zmienne są ze sobą związane. ",
      gloss("związek zmiennych", "Związek"), " widać zawsze w ten sam sposób:
      dzielimy obserwacje według wartości jednej zmiennej i porównujemy,
      jak w tych grupach rozkłada się druga. Odsetek mandatów wśród kobiet
      i wśród mężczyzn, pensje w trzech branżach, pensje osób z krótkim
      i z długim stażem. Gdy rozkłady w grupach się różnią, zmienne są
      związane; gdy są takie same, związku nie ma."),

    lc_h2("ch1-mapa", "Trzy pary typów"),

    lc_p("Z dwóch typów zmiennych, jakościowego i ilościowego, powstają trzy
      pary. Dla każdej inaczej układa się dane, inny wykres pokazuje związek
      i inna liczba go streszcza. Następne trzy rozdziały omawiają je po
      kolei."),

    lc_table(
      data.frame(
        para   = c("Dwie jakościowe", "Ilościowa i jakościowa", "Dwie ilościowe"),
        przyklad = c("płeć kierowcy × mandat albo pouczenie",
                     "branża × pensja absolwenta",
                     "staż × pensja"),
        opis   = c("tabela krzyżowa, procenty w wierszach",
                   "średnie i mediany w grupach, ich różnica",
                   "współczynnik korelacji r"),
        wykres = c("słupki w grupach, mapa ciepła",
                   "pudełka obok siebie",
                   "wykres rozrzutu"),
        stringsAsFactors = FALSE
      ),
      list(
        lc_col("para", "Para zmiennych", "row"),
        lc_col("przyklad", "Przykład", "text"),
        lc_col("opis", "Liczby", "text"),
        lc_col("wykres", "Wykres", "text")
      ),
      narrow = "cards",
      prose = TRUE,
      caption = "Opis związku dwóch zmiennych według typów."
    ),

    lc_p("Ostatni rozdział zbiera pułapki: sytuacje, w których liczba opisująca
      związek wprowadza w błąd, choć policzono ją poprawnie."),

    lc_h2("ch1-opis", "Opis, nie wnioskowanie"),

    lc_p("W tym wykładzie wszystko dotyczy próby, którą mamy przed sobą.
      Mówimy, że w danym dniu patrol wystawił mężczyznom mandat w 31% kontroli,
      a kobietom w 18%. Nie mówimy jeszcze, czy taka różnica utrzymałaby się
      u wszystkich kierowców, czy mogła powstać przypadkiem. To pytanie
      wnioskowania, które w wykładzie 00 oddzieliliśmy od opisu. Wrócimy do
      niego w wykładzie 05, gdzie dla każdej z trzech par poznamy test:
      χ² dla dwóch zmiennych jakościowych, test t i ANOVA dla porównania grup
      oraz test korelacji. Każdy z nich zaczyna od liczby, którą poznamy tutaj."),

    lc_chapter_next(
      num       = "02",
      title     = "Dwie zmienne jakościowe",
      lead      = "kontrola drogowa i tabela krzyżowa.",
      target_id = "ch-dwie-jakosciowe"
    )
  )
)

ch1_server <- function(input, output, session) {}
