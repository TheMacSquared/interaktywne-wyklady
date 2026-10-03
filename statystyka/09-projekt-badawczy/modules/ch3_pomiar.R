ch3_ui <- lecture_chapter(id = "ch3", num = "3", title = "Pomiar", content = tagList(
  lc_chapter_hero(
    kicker = "Rozdział 03 · Pomiar",
    num = "03",
    title = "Ocena z ankiety to jeszcze nie jakość nauczania.",
    lead = "Pojęcie z hipotezy i zmienna w tabeli to dwie różne rzeczy.
            Różnicę między nimi trzeba nazwać przed analizą, bo wraca
            potem we wniosku jako ograniczenie."
  ),

  lc_h2("sec-01", "Pojęcie → wskaźnik → zmienna → ograniczenie"),

  lc_p("Cel z rozdziału 1 mówi o jakości nauczania, a hipotezy z rozdziału 2
    o atrakcyjności, sprawiedliwości ocen i reprezentatywności ankiety.
    W tabeli nie ma żadnej z tych rzeczy wprost. Są kolumny eval, beauty,
    minority czy response.rate, które mają te pojęcia przybliżać. Zanim
    cokolwiek policzymy, trzeba ustalić, jak dokładne jest to przybliżenie."),

  lc_p("Drogę od pojęcia do danych opisujemy w czterech krokach. Pojęcie to
    to, o czym mówi hipoteza, na przykład jakość nauczania. Wskaźnik to
    obserwowalna rzecz, która ma to pojęcie odzwierciedlać, na przykład
    ogólna ocena kursu wystawiona przez studentów. Zmienna to konkretna
    kolumna w tabeli wraz ze swoją skalą i typem (wykład 01). Ograniczenie
    to wszystko, co po drodze zgubiliśmy: część pojęcia, której zmienna
    nie obejmuje, i rzeczy, które zmienna mierzy przy okazji. Panel rozpisuje
    te cztery kroki dla czterech kluczowych pojęć naszego projektu."),

  figure_panel(
    label = "Ryc. 3.1",
    title = "Od pojęcia do zmiennej",
    uiOutput("ch3_construct_maps")
  ),

  lc_p("Ograniczenia widać też w samych liczbach. Eval ma skalę od 1 do 5,
    ale w danych przyjmuje wartości od 2.1 do 5.0, ze średnią 4.00. Aż 83%
    kursów ma ocenę między 3.3 a 4.7, więc różnice, które
    będziemy badać, rozgrywają się na wąskim odcinku skali. Beauty nie jest
    cechą kursu, tylko oceną osoby: ma średnią 0 z definicji (wartości
    przesunięto) i ", gloss("odchylenie standardowe"), " 0.79, a u każdego prowadzącego
    jest identyczna na wszystkich jego kursach."),

  lc_p("Przy pojęciu sprawiedliwości ocen kluczowe są liczebności grup.
    64 kursy z mniejszością to w rzeczywistości kursy 12 prowadzących,
    a 28 kursów prowadzonych przez osoby, dla których angielski nie jest
    językiem ojczystym, to kursy zaledwie 7 osób. Porównanie takich grup jest
    w dużej mierze porównaniem kilku konkretnych ludzi. Response rate
    waha się od 10.4% do 100%, z medianą 76.9%; w 39 kursach ankietę
    wypełniła mniej niż połowa zapisanych. Wiemy, ile osób odpowiedziało,
    ale nie wiemy, kim one były."),

  lc_note("Zasada", rule = TRUE,
    "Wniosek formułujemy w języku zmiennych. Mówimy, że kursy danej grupy
     prowadzących mają wyższą średnią ocenę z ankiety, a nie, że ci
     prowadzący lepiej uczą."
  ),

  lc_h2("sec-02", "Trzy pytania do każdej zmiennej"),

  lc_p("Tę samą analizę warto przeprowadzić we własnym projekcie dla każdej
    zmiennej, która trafia do hipotez. Pomagają w tym trzy pytania.
    Pierwsze: czy zmienna naprawdę mierzy to, o czym mówi hipoteza, czy
    tylko coś, co się z tym wiąże. Drugie: kto i kiedy wykonał pomiar.
    Trzecie: jakie inne zjawisko mogłoby dać w danych taki sam wynik."),

  lc_p("Dla eval odpowiedzi są następujące. Zmienna mierzy ogólne
    wrażenie studentów z kursu, a jakość nauczania jest tylko jednym z jego
    składników.
    Pomiaru dokonali studenci, którzy zdecydowali się wypełnić ankietę,
    a nie wszyscy zapisani. Wysoką ocenę mógłby dać zarówno dobrze
    prowadzony kurs, jak i kurs łatwy, z łagodnym ocenianiem albo
    z sympatycznym prowadzącym. Odpowiedzi na trzecie pytanie to gotowe
    alternatywne wyjaśnienia; część z nich już jest w naszej wiązce,
    a reszta trafi do konspektu jako ograniczenia."),

  lc_chapter_next("04", "Konspekt pracy badawczej",
    "Mamy cel, tropy i pomiar. Teraz składamy z nich pełny konspekt przed analizą.",
    "ch4")
  )
)

ch3_server <- function(input, output, session) {
  output$ch3_construct_maps <- renderUI({
    maps <- list(
      list(name = "Jakość nauczania", cells = list(
        c("Pojęcie", "Jakość nauczania: czy zajęcia realnie pomagają studentom uczyć się."),
        c("Wskaźnik", "Ogólna ocena kursu wystawiona przez studentów."),
        c("Zmienna", "`eval`: średnia z ankiet, skala 1–5."),
        c("Ograniczenie", "Może mierzyć satysfakcję, łatwość, sympatię lub oczekiwaną ocenę.")
      )),
      list(name = "Atrakcyjność", cells = list(
        c("Pojęcie", "Atrakcyjność jako możliwe źródło obciążenia ocen."),
        c("Wskaźnik", "Średnia ocena wyglądu wystawiona przez panel sześciu studentów."),
        c("Zmienna", "`beauty`: ocena wyglądu przesunięta tak, by średnia wynosiła 0."),
        c("Ograniczenie", "To ocena społeczna, nie obiektywna cecha osoby.")
      )),
      list(name = "Sprawiedliwość ocen", cells = list(
        c("Pojęcie", "Sprawiedliwość oceniania prowadzących."),
        c("Wskaźnik", "Porównanie ocen między grupami prowadzących."),
        c("Zmienna", "`gender`, `native`, `minority`, `tenure`."),
        c("Ograniczenie", "Przynależność do grupy to etykieta, nie pomiar samego traktowania.")
      )),
      list(name = "Reprezentatywność opinii", cells = list(
        c("Pojęcie", "Reprezentatywność opinii studentów."),
        c("Wskaźnik", "Odsetek zapisanych osób, które wypełniły ankietę."),
        c("Zmienna", "`response.rate`."),
        c("Ograniczenie", "Nie wiemy, kto nie odpowiedział i dlaczego.")
      ))
    )
    blocks <- lapply(maps, function(m) {
      cells <- lapply(m$cells, function(item) {
        div(class = "construct-cell",
          h4(item[[1]]),
          p(HTML(gsub("`([^`]+)`", "<code>\\1</code>", item[[2]])))
        )
      })
      tagList(
        tags$h4(m$name),
        div(class = "construct-map", cells)
      )
    })
    div(blocks)
  })

}
