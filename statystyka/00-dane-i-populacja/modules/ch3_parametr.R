# ============================================================================
# CHAPTER 3: Parametr i statystyka
# ============================================================================

ch3_ui <- list(
  id    = "ch-parametr",
  num   = "03",
  title = "Parametr i statystyka",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Dane i populacja",
      num    = "03",
      title  = "Liczba, której nie znamy, i liczba, którą mamy.",
      lead   = "Odsetek studentów, którzy zdali egzamin, to dla całego wydziału jedna,
                konkretna liczba, ale zwykle nikt jej nie zna. Z próby
                liczymy jej odpowiednik i ta liczba za każdym razem wychodzi
                trochę inna. Pierwszą nazywamy parametrem, drugą statystyką."
    ),

    lc_p("Populacja i próba to zbiory jednostek. Statystyka nie zatrzymuje się
      jednak na zbiorach, tylko streszcza je liczbami: ", gloss("średnia", "średnią"), ", odsetkiem,
      rozrzutem. Ta sama formuła, na przykład „odsetek osób, które pracują”,
      daje inną liczbę, gdy liczymy ją dla całej populacji, a inną, gdy dla
      próby. Te dwie liczby mają osobne nazwy i osobne oznaczenia."),

    lc_h2("ch3-definicje", "Dwie liczby, dwa oznaczenia"),

    lc_p(gloss("parametr", "Parametr"), " to liczba opisująca populację,
      na przykład średni czas dojazdu wszystkich studentów wydziału albo
      odsetek pracujących wśród nich. Dla danej populacji parametr ma jedną,
      stałą wartość. Zazwyczaj jej nie znamy, bo nie zbadaliśmy wszystkich.
      ", gloss("statystyka", "Statystyka"), " to liczba policzona z próby
      tą samą formułą, na przykład średni czas dojazdu albo odsetek
      pracujących wśród wylosowanych osób. Statystykę zawsze znamy, bo
      liczymy ją z danych, które mamy w ręku."),

    lc_p("Żeby nie mylić tych liczb, parametry oznaczamy zwykle literami
      greckimi, a statystyki łacińskimi albo symbolem z daszkiem.
      Te same oznaczenia wracają w każdym kolejnym wykładzie."),

    lc_table(
      data.frame(
        what  = c("Średnia", "Odchylenie standardowe", "Odsetek (proporcja)"),
        param = c("μ (mi)", "σ (sigma)", "p"),
        stat  = c("x̄ (x z kreską)", "s", "p̂ (p z daszkiem)"),
        stringsAsFactors = FALSE
      ),
      list(
        lc_col("what", "Miara", "row"),
        lc_col("param", "Parametr (populacja)", "text"),
        lc_col("stat", "Statystyka (próba)", "text")
      ),
      prose = TRUE,
      caption = "Oznaczenia parametrów i odpowiadających im statystyk."
    ),

    lc_p("Średnią i odchylenie standardowe dokładnie zdefiniujemy
      w wykładzie 01. Odsetek jest prosty już teraz: to liczba jednostek
      z daną cechą podzielona przez liczbę wszystkich jednostek. Jeśli
      w próbie 50 osób pracuje 19, to p̂ = 19/50 = 0.38."),

    lc_h2("ch3-zmiennosc", "Każda grupka daje inny wynik"),

    lc_p("W prawdziwym badaniu mamy jedną próbę i jedną wartość statystyki.
      Żeby zobaczyć, co to znaczy, wróćmy do egzaminu ze statystyki. Cały
      wydział siedzi za drzwiami z napisem EGZAMIN i nie widzimy, kto zdał.
      Możemy tylko poprosić, żeby wyszła grupka osób, i zapytać każdą o wynik.
      W naszych drzwiach, inaczej niż w życiu, można to robić do woli,
      a na końcu zajrzeć do środka."),

    figure_panel(
      label = "Ryc. 3.1",
      width_mode = "text",
      scene_widget("ch3_worek", "Grupka z egzaminu: od jednej próby do rozkładu p̂",
        steps = c("Grupka", "Statystyka", "Powtarzamy", "Drzwi"),
        labels = c("Wywołaj grupkę", "Wywołaj grupkę", "Wywołaj grupkę", "Wywołaj grupkę"),
        options = list(list(name = "n", label = "Osób w grupce (n)",
                            values = c(10, 25, 100), selected = 25)),
        config = list(kind = "bag", p = round(pop_zdal, 4), n = 25,
                      aria = "Drzwi z napisem egzamin, grupka osób, które wyszły, i histogram odsetków p̂ z kolejnych grupek"))
    ),

    lc_p("Prawdziwy odsetek zdających na wydziale wynosi p = ",
      paste0(lc_fmt(pop_zdal, 3), ". Przy n = 25 kolejne wartości p̂ wypadają typowo
      w przedziale od około ", lc_fmt(pop_zdal - 2 * sqrt(pop_zdal * (1 - pop_zdal) / 25), 2),
      " do ", lc_fmt(pop_zdal + 2 * sqrt(pop_zdal * (1 - pop_zdal) / 25), 2), ", a przy n = 100
      od około ", lc_fmt(pop_zdal - 2 * sqrt(pop_zdal * (1 - pop_zdal) / 100), 2), " do ",
      lc_fmt(pop_zdal + 2 * sqrt(pop_zdal * (1 - pop_zdal) / 100), 2), ". Parametr się
      nie zmienia: to ci sami studenci i ta sama liczba. Zmienia się tylko grupka,
      a razem z nią statystyka. To zjawisko nazywamy "),
      gloss("zmienność próbkowa", "zmiennością próbkową"), "."),

    lc_p("Większa grupka nie usuwa zmienności próbkowej, ale ją zmniejsza.
      Wiedząc, jak duża jest ta zmienność, można z jednej próby powiedzieć,
      w jakim zakresie prawdopodobnie leży parametr. Na tym pomyśle zbudowane
      są przedziały ufności z wykładu 03."),

    lc_note("Zasada", rule = TRUE,
      "Parametr opisuje populację, jest stały i zwykle nieznany. Statystyka
       opisuje próbę, jest znana i zmienia się od próby do próby."
    ),

    lc_h2("ch3-obciazenie", "Kiedy duże n nie pomaga"),

    lc_p("Grupki zza drzwi były losowane uczciwie: każdy student miał tę samą
      szansę. Taki sposób to ", gloss("losowanie proste", "losowanie proste"),
      ". Bywa uzupełniane ", gloss("losowanie warstwowe", "losowaniem warstwowym"),
      ", w którym losuje się osobno w grupach, na przykład na każdym roku
      studiów, tak by ich udział w próbie zgadzał się z populacją. Szczegóły
      techniczne zostawiamy na boku. Ważne jest, czym te sposoby różnią się
      od ", gloss("próba wygodna", "próby wygodnej"), ": ankiety rozdanej
      tam, gdzie łatwo dotrzeć, i wypełnionej przez tych, którzy się zgłosili.
      Panel pokazuje różnicę na tłumie studentów."),

    figure_panel(
      label = "Ryc. 3.2",
      width_mode = "text",
      scene_widget("ch3_latarka", "Losowanie i latarka: dwa sposoby, dwie średnie",
        steps = c("Losowanie", "Latarka", "Powtarzamy"),
        labels = c("Losuj próbę", "Świeć latarką", "Próba obu rodzajów"),
        options = list(list(name = "n", label = "Liczebność próby (n)",
                            values = c(20, 50, 300), selected = 50, from = 3)),
        more_from = 3,
        config = list(kind = "spot", n = 50, mu = round(pop_mu, 3),
                      d = faculty$dojazd, a = as.integer(faculty$akademik),
                      aria = "Tłum studentów, losowanie i latarka świecąca na akademik, średnie czasu dojazdu z prób"))
    ),

    lc_p("W 1936 roku tygodnik Literary Digest zebrał ponad dwa miliony
      odpowiedzi i błędnie wskazał zwycięzcę wyborów prezydenckich w USA.
      Ankietę wysłano czytelnikom, właścicielom telefonów i samochodów,
      a odpowiedziała część z nich. Dwa miliony odpowiedzi dały bardzo małą
      zmienność próbkową, ale wokół złej wartości. Taki systematyczny błąd
      w jedną stronę nazywamy ", gloss("obciążenie", "obciążeniem"), "."),

    lc_note("Zasada", rule = TRUE,
      "Losowanie zabezpiecza przed obciążeniem, a duże n zmniejsza
       zmienność próbkową. Jedno nie zastąpi drugiego."
    ),

    lc_p("Do tego, jak próby bywają zniekształcone, wracamy w wykładzie 07.
      Teraz zobaczymy, że nawet uczciwie wylosowana próba potrafi
      pokazać wzorzec, którego w populacji nie ma."),

    lc_chapter_next(
      num       = "04",
      title     = "Przypadek czy wzorzec?",
      lead      = "czy to, co widać w próbie, widać też w populacji",
      target_id = "ch-przypadek"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  scene_texts(input, output, "ch3_worek", list(
    tagList("Za drzwiami z napisem EGZAMIN siedzi cały wydział. Nie widzimy, kto zdał. Wywołaj grupkę:
      wyjdzie kilka osób i powie, jak im poszło. Uśmiechnięta zielona buźka to ktoś, kto zdał, smutna czerwona to ktoś, kto nie zdał."),
    tagList("Liczymy, ilu z grupki zdało, i dzielimy przez liczbę osób. To statystyka ",
      tags$code("p̂", .noWS = "outside"), ": znamy ją, bo grupka stoi przed nami. Wywołaj kilka grupek
      i porównaj wyniki."),
    tagList("Grupka wraca za drzwi, wywołujemy kolejną i znowu liczymy ",
      tags$code("p̂", .noWS = "outside"), ". Każda grupka spada żetonem nad swoją wartością. Dokładaj po 10,
      100 i 1000, a potem zmień liczbę osób w grupce."),
    tagList("Otwieramy drzwi: ", tags$code("p", .noWS = "outside"), " to prawdziwy odsetek tych, którzy zdali.
      Parametr jest jeden i stały, a statystyka skacze wokół niego. Im większa grupka,
      tym ciaśniej skupiają się wyniki, ale z jednej grupki nie wiemy, po której
      stronie ", tags$code("p", .noWS = "outside"), " leży.")
  ))

  scene_texts(input, output, "ch3_latarka", list(
    tagList("Każda kropka to student wydziału, ciemne kropki to mieszkańcy akademika. Pionowa
      linia to prawdziwa średnia czasu dojazdu wszystkich, czyli parametr μ. Losujemy próbę
      tak, że każdy ma równą szansę, i liczymy jej średnią x̄."),
    tagList("Teraz ankietę rozdajemy tam, gdzie najłatwiej: przy akademiku. Latarka oświetla
      tych, którzy stoją blisko, i tylko oni trafiają do próby. Mieszkańcy akademika dojeżdżają
      krótko, więc średnia z takiej próby ucieka w lewo od μ."),
    tagList("Dokładaj próby obu rodzajów. Średnie z losowania rozkładają się wokół μ,
      średnie z latarki wokół zbyt niskiej wartości. Zwiększ n: obie chmury się zwężają,
      ale latarka trafia coraz pewniej obok μ. Duże n zmniejsza zmienność, ale nie usuwa obciążenia.")
  ))
}
