# ============================================================================
# CHAPTER 5: Pułapki korelacji
# ============================================================================

ch5_ui <- list(
  id = "ch-pulapki", num = "05", title = "Pułapki",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 05 · Związki dwóch zmiennych",
      num    = "05",
      title  = "Pułapki korelacji.",
      lead   = "Jedna liczba streszcza całą chmurę punktów, więc łatwo
                ją przecenić. Pięć klasycznych sytuacji pokazuje, kiedy
                poprawnie policzone r opowiada nieprawdziwą historię."
    ),

    lc_p("Współczynnik \\(r\\) mówi, jak ściśle punkty trzymają się prostej.
      Nie mówi, jaki kształt ma chmura, czy wynik nie zależy od pojedynczego
      punktu ani czy jedna zmienna wpływa na drugą. Te pytania rozstrzyga się
      poza samym \\(r\\): patrząc na wykres i zastanawiając się, skąd pochodzą
      dane."),

    lc_h2("ch5-anscombe", "Ta sama liczba, różne dane"),

    lc_p("Pierwszą pułapkę pokazał statystyk Francis Anscombe w 1973 roku.
      Zbudował cztery zbiory po 11 punktów o niemal identycznych statystykach.
      W każdym średnia \\(x\\) wynosi 9, średnia \\(y\\) 7.50, ", gloss("wariancja"),
      " \\(x\\) 11, wariancja \\(y\\) 4.12–4.13, korelacja 0.816–0.817,
      a prosta dopasowana do punktów to \\(y = 3 + 0.5x\\)."),

    figure_panel(
      label = "Ryc. 5.1",
      title = "Kwartet Anscombe’a",
      tags$img(src = "assets/anscombe-quartet.png",
               alt = "Kwartet Anscombe’a: cztery bardzo różne chmury punktów o niemal identycznych statystykach opisowych i korelacji.",
               tabindex = "0", role = "button")
    ),

    lc_p("Tylko zbiór 1 wygląda tak, jak sugeruje \\(r \\approx 0.82\\): chmura
      punktów rozproszona wokół prostej. W zbiorze 2 punkty leżą na gładkim łuku,
      związek jest więc niemal doskonały, ale nie liniowy. W zbiorze 3 dziesięć
      punktów leży na jednej prostej (bez jedenastego punktu \\(r\\) wynosiłoby
      1.000), a jeden punkt odstaje i obniża korelację. W zbiorze 4 dziesięć
      punktów ma to samo \\(x = 8\\), a całą korelację tworzy jedenasty punkt
      z \\(x = 19\\). Same liczby nie odróżnią tych sytuacji, wykres
      odróżnia je od razu."),

    lc_note("Zasada", rule = TRUE,
      "Przed interpretacją r obejrzyj wykres rozrzutu."
    ),

    lc_h2("ch5-nieliniowosc", "Związek, którego r nie widzi"),

    lc_p("Zbiór 2 prowadzi do drugiej pułapki. Współczynnik Pearsona mierzy tylko
      związek liniowy, więc związek silny, ale zakrzywiony, może dać \\(r\\)
      bliskie zeru. Na wykresie poniżej \\(y\\) zależy od \\(x\\) kwadratowo:
      punkty układają się w literę U."),

    figure_panel(
      label = "Ryc. 5.2",
      title = "Nieliniowość przy r ≈ 0",
      tags$img(src = "assets/correlation-nonlinear.png",
               alt = "Silna zależność w kształcie litery U, dla której korelacja liniowa jest bliska zeru.",
               tabindex = "0", role = "button")
    ),

    lc_p("Choć \\(y\\) jest niemal wyznaczone przez \\(x\\), \\(r\\) wynosi -0.004.
      Lewa połowa wykresu ma trend malejący, prawa rosnący, a iloczyny odchyleń
      w liczniku \\(r\\) z obu połówek wzajemnie się znoszą: punkty rozkładają się
      po równo między ćwiartki zgodne i niezgodne. Wartość \\(r\\) bliska zeru
      oznacza brak związku liniowego, a nie brak związku."),

    lc_h2("ch5-odstajaca", "Jeden punkt"),

    lc_p("Zbiory 3 i 4 pokazały, że pojedynczy punkt potrafi wyraźnie zmienić
      \\(r\\). ", gloss("wartość odstająca", "Wartość odstająca"), " ma tak duży
      wpływ, bo jej odchylenia od średnich są duże w obu zmiennych naraz, a ich
      iloczyn może przeważyć całą resztę sumy w liczniku. Panel losuje 50 punktów
      z dwóch niezależnych zmiennych, czyli takich, między którymi związku nie ma.
      Drugi przycisk dopisuje punkt leżący o 15 jednostek dalej niż największe
      \\(x\\) i największe \\(y\\) w danych. Dopisany punkt można potem
      przeciągać po wykresie."),

    figure_panel(
      label = "Ryc. 5.3",
      title = "Wpływ wartości odstającej na r",
      tags$div(class = "lc-scat lc-outlier", `data-n` = "50", `data-sd` = "3", `data-shift` = "15",
        lc_toolbar(
          tags$button(type = "button", class = "lc-action is-solid", `data-o-new` = "1",
                      tags$span("Nowe dane (brak związku)")),
          tags$button(type = "button", class = "lc-action is-solid", `data-o-add` = "1",
                      tags$span("Dodaj wartość odstającą")),
          lc_readouts(
            lc_readout("r", tags$span(`data-role` = "r", "–"), color = "var(--upwr-accent)"),
            lc_readout("r bez odstających", tags$span(`data-role` = "r0", "–"),
                       color = "var(--upwr-ink-subtle)"),
            lc_readout("Wartości odstające", tags$span(`data-role` = "k", "0"))
          )
        ),
        tags$div(class = "lc-scat-canvas",
          tags$canvas(`data-role` = "plot",
                      `aria-label` = "Wykres rozrzutu 50 punktów bez związku z prostą dopasowania; dopisane wartości odstające można przeciągać")),
        lc_caption("Przeciągnij dopisany punkt. Przerywana prosta pokazuje dopasowanie bez wartości odstających.")
      )
    ),

    lc_p("Bez dodatkowego punktu \\(r\\) z 50 obserwacji leży zwykle blisko zera:
      w 90% losowań między -0.23 a 0.23. Jeden dopisany punkt podnosi je typowo
      do około 0.52, a w 90% losowań do wartości między 0.39 a 0.63. Drugi taki
      punkt, położony jeszcze dalej, podnosi \\(r\\) do około 0.8. Jeden punkt
      na 51 zamienia brak związku w związek umiarkowany. Wartości odstającej nie
      usuwa się jednak automatycznie. Najpierw trzeba ustalić, czy to błąd
      pomiaru, czy prawdziwa, nietypowa obserwacja. Bezpieczną praktyką jest
      podanie wyniku z tym punktem i bez niego."),

    lc_h2("ch5-pozorna", "Korelacja to nie przyczyna"),

    lc_p("Ostatnie dwie pułapki dotyczą interpretacji, a nie liczenia. Nawet silna
      i liniowa korelacja nie mówi, czy jedna zmienna wpływa na drugą.
      Klasyczny przykład: w dni, gdy sprzedaje się więcej lodów, dochodzi też
      do większej liczby utonięć. Lody nie powodują utonięć. Obie liczby rosną,
      gdy jest ciepło, bo wtedy więcej osób kupuje lody i więcej osób się kąpie.
      Temperatura jest tu ", gloss("zmienna zakłócająca", "zmienną zakłócającą"),
      ": wpływa na obie zmienne i wytwarza między nimi ",
      gloss("korelacja pozorna", "korelację pozorną"), ". Wniosek
      o ", gloss("przyczynowość", "przyczynowości"), " wymaga eksperymentu albo
      kontroli zmiennych zakłócających, a sam współczynnik korelacji nie zapewnia
      żadnego z nich. Wiele absurdalnych, a przy tym silnych korelacji zebrał
      Tyler Vigen: ",
      tags$a(href = "https://www.tylervigen.com/spurious-correlations",
             target = "_blank",
             "Spurious Correlations →"), "."),

    lc_h2("ch5-simpson", "Paradoks Simpsona"),

    lc_p("Zmienna zakłócająca potrafi nawet odwrócić kierunek związku. Takie
      odwrócenie nazywa się ", gloss("paradoks Simpsona", "paradoksem Simpsona"),
      ". Wracamy do stażu i pensji z rozdziału 4, ale tym razem z trzech branż,
      po 40 osób z każdej. Firma biotechnologiczna z wykresu rozrzutu to jedna
      z nich. W widoku „Razem” wszystkie punkty analizujemy łącznie, w widoku
      „Osobno” każdą branżę oddzielnie."),

    figure_panel(
      label = "Ryc. 5.4",
      title = "Paradoks Simpsona",
      tags$div(class = "lc-scat lc-simpson", `data-config` = scatter_config(simpson_config),
        lc_toolbar(
          lc_group("Widok", tags$div(class = "lc-seg", role = "group",
            tags$button(type = "button", `data-s-view` = "all", `aria-pressed` = "true", "Razem"),
            tags$button(type = "button", `data-s-view` = "split", `aria-pressed` = "false", "Osobno"))),
          lc_readouts(
            lc_readout("r razem", tags$span(`data-role` = "r", "-0.42"), color = "var(--upwr-secondary)"),
            lapply(seq_along(branch_levels), function(i) {
              tagAppendAttributes(
                lc_readout(branch_levels[[i]], tags$span(`data-role` = paste0("r", i - 1L), ""),
                           color = paste0("var(", simpson_config$colors[[i]], ")"), swatch = TRUE),
                `data-s-group` = i - 1L, hidden = NA)
            })
          )
        ),
        tags$div(class = "lc-scat-canvas",
          tags$canvas(`data-role` = "plot",
                      `aria-label` = "Staż a pensja 120 pracowników trzech branż: prosta dla wszystkich razem albo osobna prosta dla każdej branży")),
        lc_status(
          tagAppendAttributes(lc_verdict(tags$strong("Razem:"), type = "warning"), `data-role` = "verdict"),
          tags$span(`data-role` = "caption", " r = -0.42. Dłuższy staż, niższa pensja?")
        )
      )
    ),

    lc_p("W danych połączonych \\(r = -0.42\\): wygląda na to, że im dłuższy staż,
      tym niższa pensja. W każdej branży osobno korelacja jest jednak dodatnia:
      0.79 w rolniczej, 0.66 w biotechnologii i 0.69 w kosmicznej. Odwrócenie
      bierze się z różnic między branżami. Branża kosmiczna jest młoda: średni
      staż wynosi tam 4.6 roku, a średnia pensja 14.2 tys. zł. W branży rolniczej
      ludzie pracują średnio 16.4 roku i zarabiają średnio 7.4 tys. zł. Po
      połączeniu grup, czyli ", gloss("agregacja", "agregacji"), ", korelacja
      porównuje głównie branże między sobą, a nie pracowników w obrębie branży.
      Branża jest tu zmienną zakłócającą. Pytanie, czy kolejny rok stażu wiąże
      się z wyższą pensją, dotyczy związku w obrębie branży. Więcej
      o paradoksie: ",
      tags$a(href = "https://en.wikipedia.org/wiki/Simpson%27s_paradox",
             target = "_blank", "Wikipedia →"), ", ",
      tags$a(href = "https://www.youtube.com/watch?v=ebEkn-BiW5k",
             target = "_blank", "film TED-Ed →"), "."),

    lc_p("Pięć pułapek układa się w dwie grupy. Kwartet Anscombe’a, nieliniowość
      i wartości odstające pokazują, że \\(r\\) może źle opisywać dane, a chroni
      przed tym wykres rozrzutu. Korelacja pozorna i paradoks Simpsona pokazują,
      że nawet trafnie policzone \\(r\\) może źle opisywać mechanizm. Przed tym
      chroni dopiero wiedza o tym, jak powstały dane i jakie zmienne pominięto.
      To samo dotyczy tabel krzyżowych i porównań grup z rozdziałów 2 i 3:
      różnica w mandatach albo w pensjach też może wynikać z trzeciej zmiennej."),

    lc_recap(
      "Dwie zmienne są związane, gdy rozkład jednej różni się w zależności
       od wartości drugiej.",
      "Dwie jakościowe: tabela krzyżowa i procenty liczone w obrębie grup,
       nie liczebności.",
      "Ilościowa i jakościowa: rozkłady w grupach, ich mediany lub średnie
       i różnica między nimi; pudełka pokazują, jak bardzo grupy się zachodzą.",
      "Dwie ilościowe: wykres rozrzutu i r. Znak r to kierunek, |r| to siła
       związku liniowego, a nie nachylenie.",
      "Przed interpretacją r obejrzyj wykres; związek w danych nie jest jeszcze
       przyczyną.",
      "Wszystkie te liczby opisują próbę. Czy związek wykracza poza nią,
       rozstrzygają testy z wykładu 05."
    )
  )
)

# Ryc. 5.3 i 5.4 liczą się w przeglądarce (scatter_charts.js); serwer bez wyjść.
ch5_server <- function(input, output, session) {
  invisible(NULL)
}
