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

    lc_p("Pojęcia z tego wykładu łączą się w jeden schemat. Mamy ", gloss("populacja", "populację"), "
      i interesujący nas ", gloss("parametr"), ". Losujemy ", gloss("próba", "próbę"), ", zapisujemy ", gloss("obserwacja", "obserwacje"), "
      w tabeli i liczymy ", gloss("statystyka", "statystykę"), ". Statystyka trafia w parametr tylko
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

    figure_panel(
      label = "Ryc. 5.1",
      width_mode = "text",
      scene_widget("ch5_wniosek", "Od opisu próby do wniosku o populacji",
        steps = c("Opis", "Wniosek", "Powtarzamy", "Parametr"),
        labels = c("Wylosuj próbę", "Wylosuj próbę", "Wylosuj próbę", "Wylosuj próbę"),
        options = list(list(name = "n", label = "Liczebność próby (n)", from = 3,
                            values = c(20, 50, 200), selected = 50)),
        more_from = 3, more = c("+10" = "m10", "+100" = "m100"),
        config = list(kind = "infer", n = 50, pr = as.integer(faculty$praca),
                      aria = "Próba studentów, odsetek pracujących w próbie i zakres, w którym leży odsetek w populacji"))
    ),

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
      "Wzorzec w próbie może być dziełem przypadku. Jak często sam szum
       daje taki obraz, zależy od n.",
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

ch5_server <- function(input, output, session) {
  scene_texts(input, output, "ch5_wniosek", list(
    tagList("Losujemy próbę i liczymy, ilu studentów w niej pracuje. Zdanie „w naszej próbie pracuje k z n osób”
      to opis: dotyczy tylko zbadanych osób i jest prawdziwe bez żadnych założeń."),
    tagList("Wniosek idzie krok dalej: z ", tags$code("p̂", .noWS = "outside"), " próbujemy powiedzieć coś o całym wydziale,
      czyli o parametrze ", tags$code("p", .noWS = "outside"), ". Zamiast jednej liczby podajemy zakres, w którym ",
      tags$code("p", .noWS = "outside"), " prawdopodobnie leży. Jak go policzyć, pokaże wykład 03."),
    tagList("Każda próba daje inne ", tags$code("p̂", .noWS = "outside"), " i inny zakres. Dokładaj próby i patrz, jak zakresy
      przesuwają się w lewo i w prawo. Zmień n: większa próba daje węższe zakresy."),
    tagList("Odsłaniamy parametr ", tags$code("p", .noWS = "outside"), ". Zakresy, które go nie obejmują, są czerwone.
      Większość trafia, ale nie wszystkie: wniosek zawsze niesie niepewność i dlatego nie jest tak pewny jak opis.")
  ))
}
