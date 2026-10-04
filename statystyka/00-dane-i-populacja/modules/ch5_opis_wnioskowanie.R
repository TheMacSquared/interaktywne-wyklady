# ============================================================================
# CHAPTER 5: Opis i wnioskowanie
# ============================================================================

ch5_ui <- list(
  id    = "ch-opis-wnioskowanie",
  num   = "05",
  title = "Opis i wnioskowanie",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 05 · Dane i populacja",
      num    = "05",
      title  = "Dwa pytania, które zadajemy danym.",
      lead   = "Pierwsze brzmi: jak wyglądają dane, które mamy. Drugie: co z nich
                wynika dla całej populacji. Odpowiedzią na pierwsze zajmuje się
                statystyka opisowa, na drugie wnioskowanie statystyczne.
                Ten podział porządkuje cały kurs."
    ),

    lc_p("Pojęcia z tego wykładu łączą się w jeden schemat. Mamy populację
      i interesujący nas parametr. Losujemy próbę, zapisujemy obserwacje
      w tabeli i liczymy statystykę. Statystyka trafia w parametr tylko
      w przybliżeniu, bo zależy od tego, kto trafił do próby. Każdy kolejny
      wykład zajmuje się jednym odcinkiem tej drogi."),

    lc_h2("ch5-dwa-zadania", "Opis próby i wniosek o populacji"),

    lc_p(gloss("statystyka opisowa", "Statystyka opisowa"), " streszcza
      dane, które mamy: liczy średnie, odsetki i miary rozrzutu, rysuje
      wykresy. Jej wynik dotyczy tylko zbadanych obserwacji. Zdanie
      „w naszej próbie 19 z 50 osób pracuje zarobkowo” jest opisem i jest
      prawdziwe bez żadnych dodatkowych założeń."),

    lc_p(gloss("wnioskowanie statystyczne", "Wnioskowanie statystyczne"),
      " idzie dalej: na podstawie próby mówi coś o populacji. Zdanie
      „na wydziale pracuje zarobkowo od 25% do 51% studentów” jest
      wnioskiem. Wnioskowanie zawsze niesie niepewność, bo inna próba
      dałaby inny wynik, i zawsze opiera się na założeniu, że próba
      powstała w sposób, który nie faworyzuje żadnej grupy."),

    lc_note("Przykład",
      "Gdy prowadzący liczy średnią ocen z kolokwium w swojej grupie, a grupa
       jest jedynym, co go interesuje, wykonuje opis: grupa jest wtedy
       populacją, a średnia parametrem. Gdy tę samą średnią traktuje jako
       informację o wszystkich studentach kierunku, wnioskuje i musi
       zapytać, czy jego grupa jest dla nich reprezentatywna."
    ),

    lc_h2("ch5-mapa", "Mapa kursu"),

    lc_p("Tabela pokazuje, gdzie w kursie wracają pojęcia z tego wykładu.
      Wykłady 01 i 07 dotyczą głównie opisu i jakości danych, wykłady
      od 02 do 06 wnioskowania, a 08 i 09 łączą jedno z drugim
      w pełnej analizie."),

    lc_table(
      data.frame(
        num   = c("01", "02", "03", "04", "05", "06", "07", "08–09"),
        title = c("Statystyka opisowa", "Rozkłady prawdopodobieństwa",
                  "Przedziały ufności", "Testowanie hipotez",
                  "Założenia testów", "Regresja", "Dobre dane",
                  "Case studies, projekt badawczy"),
        link  = c(
          "Rodzaje zmiennych i statystyki opisujące próbę: x̄, s, p̂.",
          "Dlaczego statystyki zmieniają się od próby do próby i jak bardzo.",
          "Zakres, w którym prawdopodobnie leży parametr, policzony z jednej próby.",
          "Czy różnica w próbie świadczy o różnicy w populacji.",
          "Kiedy przybliżenia z wykładów 03 i 04 przestają działać.",
          "Parametry modelu opisującego związek zmiennych.",
          "Jednostka obserwacji, operat i dobór próby w prawdziwych danych.",
          "Cała droga: od pytania o populację do wniosku z próby."
        ),
        stringsAsFactors = FALSE
      ),
      list(
        lc_col("num", "Nr", "row"),
        lc_col("title", "Wykład", "text"),
        lc_col("link", "Co robi z pojęciami z wykładu 00", "text")
      ),
      narrow = "cards",
      prose = TRUE,
      caption = "Pojęcia z wykładu 00 w kolejnych wykładach."
    ),

    lc_recap(
      "Wiersz tabeli to obserwacja, kolumna to zmienna. Jednostkę obserwacji
       wyznacza pytanie, a od niej zależy n.",
      "Populacja to wszyscy, o których chcemy coś powiedzieć; próba to ci,
       których zbadaliśmy.",
      "Parametr opisuje populację i zwykle go nie znamy. Statystyka opisuje
       próbę i zmienia się od próby do próby.",
      "Losowanie chroni przed obciążeniem, duże n zmniejsza zmienność
       próbkową. Jedno nie zastąpi drugiego.",
      "Opis dotyczy próby, wnioskowanie przenosi wynik na populację
       i zawsze niesie niepewność."
    ),

    lc_chapter_next(
      num       = "06",
      title     = "Ściąga",
      lead      = "wszystkie pojęcia w jednym miejscu",
      target_id = "ch-sciaga"
    )
  )
)

ch5_server <- function(input, output, session) {}
