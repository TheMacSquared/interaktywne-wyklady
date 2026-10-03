# ============================================================================
# CHAPTER 6: Dwie zmienne ilościowe (korelacja Pearsona)
# ============================================================================

ch4_ui <- list(
  id = "ch-korelacja", num = "06", title = "Korelacja",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 06 · Testowanie hipotez",
      num    = "06",
      title  = "Korelacja.",
      lead   = "Więcej snu, lepsza ocena; cieplejszy dzień, więcej sprzedanych lodów.
                Współczynnik korelacji streszcza taki związek dwóch zmiennych
                ilościowych jedną liczbą, a test mówi, czy da się go odróżnić
                od przypadku. Ta sama liczba łatwo jednak wprowadza w błąd, jeśli
                nie spojrzy się na wykres."
    ),

    lc_p("Dotąd każdy test dotyczył jednej zmiennej: w rozdziale 4 średniej,
      w rozdziale 5 proporcji. Hipoteza zerowa wskazywała konkretną wartość
      parametru, a test sprawdzał, czy próba do niej pasuje. Od tego rozdziału
      pytamy o związek dwóch zmiennych mierzonych na tych samych obiektach:
      czy studenci, którzy śpią dłużej, mają lepsze oceny, czy plon rośnie
      z ilością nawadniania. Zaczynamy od sytuacji, w której obie zmienne
      są ilościowe."),

    # ========================================================================
    # Wprowadzenie: współczynnik korelacji
    # ========================================================================
    lc_h2("ch4-pearson", "Współczynnik korelacji Pearsona"),

    lc_p("Związek dwóch ", gloss("zmienna ilościowa", "zmiennych ilościowych"),
      " najpierw się rysuje. Na wykresie rozrzutu każdy punkt to jedna
      obserwacja: współrzędna pozioma to wartość pierwszej zmiennej, pionowa
      wartość drugiej. Jeśli punkty układają się wzdłuż rosnącej prostej,
      zmienne rosną razem. Jeśli wzdłuż malejącej, wysokim wartościom jednej
      towarzyszą niskie wartości drugiej. Jedną liczbą opisuje to współczynnik ",
      gloss("korelacja Pearsona", "korelacji Pearsona"), " \\(r\\):"),

    lc_formula_box(withMathJax(
      "$$r = \\frac{\\sum (x_i - \\bar{x})(y_i - \\bar{y})}{\\sqrt{\\sum(x_i-\\bar{x})^2 \\cdot \\sum(y_i-\\bar{y})^2}}$$"
    )),

    lc_p("Licznik sumuje iloczyny odchyleń od średnich. Punkt, który leży powyżej
      średniej w obu zmiennych albo poniżej w obu, dodaje do sumy wartość
      dodatnią. Punkt, który w jednej zmiennej leży powyżej średniej, a w drugiej
      poniżej, dodaje wartość ujemną. Mianownik sprowadza wynik do skali
      niezależnej od jednostek, dlatego \\(r\\) zawsze leży między -1 a +1
      i nie zmienia się, gdy wzrost zapiszemy w metrach zamiast w centymetrach.
      Wartość +1 oznacza, że wszystkie punkty leżą dokładnie na rosnącej prostej,
      -1 że na malejącej, a 0 brak związku liniowego."),

    lc_p("Znak \\(r\\) mówi o kierunku związku. Trzy panele poniżej pokazują
      po 90 punktów o korelacji -0.6, 0 i +0.6."),

    figure_panel(
      label = "Ryc. 6.1",
      title = "Kierunek korelacji",
      tags$img(src = "assets/correlation-direction.png",
               alt = "Trzy wykresy punktowe pokazujące korelację dodatnią, brak korelacji liniowej i korelację ujemną.",
               tabindex = "0", role = "button",
               style = "width: 100%; border-radius: 4px;")
    ),

    lc_p("Przy \\(r = 0\\) prosta dopasowana do punktów jest pozioma: znajomość
      \\(x\\) nic nie mówi o tym, czy \\(y\\) wypadnie powyżej, czy poniżej
      średniej. Przy -0.6 i +0.6 trend widać wyraźnie, choć punkty wciąż
      rozpraszają się szeroko wokół prostej. Ten rozrzut opisuje druga
      informacja zawarta w \\(r\\)."),

    lc_p("Wartość bezwzględna \\(|r|\\) mówi o sile związku liniowego, czyli
      o tym, jak ciasno punkty skupiają się wokół prostej. Trzy panele poniżej
      mają ten sam trend i różnią się tylko rozrzutem punktów."),

    figure_panel(
      label = "Ryc. 6.2",
      title = "Siła korelacji",
      tags$img(src = "assets/correlation-scatter.png",
               alt = "Trzy chmury punktów o rosnącej sile korelacji liniowej.",
               tabindex = "0", role = "button",
               style = "width: 100%; border-radius: 4px;")
    ),

    lc_p("Przy \\(r = 0.31\\) trend ledwie widać w chmurze punktów, przy 0.69
      jest wyraźny, a przy 0.94 punkty prawie układają się w linię. Kwadrat
      współczynnika ma prostą interpretację: \\(r^2\\) to część zmienności
      \\(y\\), którą da się przypisać liniowemu związkowi z \\(x\\). Nazywa się go ",
      gloss("współczynnik determinacji", "współczynnikiem determinacji"),
      ". Dla \\(r = 0.31\\) to około 10%, dla 0.69 około 48%, a dla 0.94 około
      88%. Wrócimy do niego w wykładzie 06 o regresji."),

    lc_p("Siła związku to jednak nie to samo co nachylenie prostej. Trzy panele
      poniżej mają nachylenia 0.4, 0.8 i 1.6, a punkty w każdym równie ciasno
      trzymają się prostej."),

    figure_panel(
      label = "Ryc. 6.3",
      title = "r nie zależy od nachylenia",
      tags$img(src = "assets/correlation-strength.png",
               alt = "Trzy zależności liniowe o różnych nachyleniach, ale podobnej sile korelacji.",
               tabindex = "0", role = "button",
               style = "width: 100%; border-radius: 4px;")
    ),

    lc_p("Mimo czterokrotnej różnicy nachyleń \\(r\\) wynosi w panelach 0.96, 0.96
      i 0.95. Współczynnik korelacji nie mówi, o ile wzrośnie \\(y\\), gdy \\(x\\)
      wzrośnie o jednostkę. Mówi tylko, jak ściśle punkty trzymają się prostej.
      Pytanie „o ile?” należy do regresji."),

    lc_p("Wszystkie dotychczasowe wartości \\(r\\) policzono z próby. Następne
      pytanie jest takie samo jak w poprzednich rozdziałach: co wynik z próby
      mówi o populacji."),

    # ========================================================================
    # Od współczynnika do testu
    # ========================================================================
    lc_h2("ch4-test", "Od współczynnika do testu"),

    lc_p("Współczynnik \\(r\\) z próby jest estymatorem korelacji w populacji,
      oznaczanej grecką literą \\(\\rho\\) (ro). Jak każda statystyka z próby,
      \\(r\\) zmienia się od próby do próby. Nawet gdy w populacji związku nie ma
      (\\(\\rho = 0\\)), \\(r\\) z próby prawie nigdy nie wychodzi dokładnie zero.
      Przy 40 parach obserwacji wartości od -0.31 do 0.31 pojawiają się wtedy
      w 95% prób. Test korelacji rozstrzyga, czy obserwowane \\(r\\) leży na tyle
      daleko od zera, że trudno je wytłumaczyć samym losowaniem próby."),

    lc_p(gloss("hipoteza zerowa", "Hipoteza zerowa"), " mówi, że w populacji nie
      ma związku liniowego. ", gloss("hipoteza alternatywna", "Hipoteza alternatywna"),
      " zależy od pytania, tak samo jak w teście t i w teście proporcji:"),

    lc_formula_box(withMathJax(
      "$$\\begin{aligned}
        &\\text{dwustronna:}   &&H_0: \\rho = 0    &&H_a: \\rho \\neq 0 \\\\
        &\\text{prawostronna:} &&H_0: \\rho \\leq 0 &&H_a: \\rho > 0 \\\\
        &\\text{lewostronna:}  &&H_0: \\rho \\geq 0 &&H_a: \\rho < 0
      \\end{aligned}$$"
    )),

    lc_p("Wariant dwustronny pyta o jakikolwiek związek, jednostronne o związek
      w określonym kierunku: dodatni, gdy obie zmienne rosną razem, albo ujemny,
      gdy jedna rośnie, a druga maleje. We wszystkich trzech ",
      gloss("statystyka testowa", "statystyka testowa"), " jest ta sama:
      \\(r\\) przeliczone na skalę rozkładu t."),

    lc_formula_box(withMathJax(
      "$$t = \\frac{r\\sqrt{n-2}}{\\sqrt{1-r^2}}, \\qquad df = n - 2$$"
    )),

    lc_p("Gdy H₀: \\(\\rho = 0\\) jest prawdziwa, statystyka \\(t\\) ma ",
      gloss("rozkład t-Studenta"), " o \\(n - 2\\) ",
      gloss("stopnie swobody", "stopniach swobody"), ". Wzór łączy dwa składniki.
      Im większe \\(|r|\\), tym większe \\(|t|\\). Przy tym samym \\(r\\) statystyka
      rośnie razem z pierwiastkiem z liczebności próby. Ta sama korelacja 0.3
      przy 15 parach obserwacji jest zgodna z przypadkiem, a przy 100 parach
      już nie."),

    lc_p("Dalej postępujemy jak w każdym teście. ", gloss("p-wartość", "P-wartość"),
      " to prawdopodobieństwo, że przy prawdziwej H₀ statystyka \\(t\\) wypadnie
      co najmniej tak daleko od zera jak obserwowana. Jeśli jest mniejsza od ",
      gloss("poziom istotności", "poziomu istotności"), " α = 0.05, ustalonego
      przed zebraniem danych, odrzucamy H₀. Oprócz \\(r\\), \\(t\\)
      i p-wartości warto podać ", gloss("przedział ufności"),
      " dla \\(\\rho\\). Jak w wykładzie 03, przedział mówi więcej
      niż sama decyzja, bo pokazuje, jak silny może być związek w populacji.
      Przedział, który nie obejmuje zera, w praktyce idzie w parze z odrzuceniem
      H₀ w teście dwustronnym. Oba wyniki liczy się jednak innymi przybliżeniami,
      więc w przypadkach granicznych mogą się rozminąć."),

    lc_p("Test zakłada, że pary obserwacji są od siebie niezależne, związek jest
      liniowy, a obie zmienne mają rozkład zbliżony do normalnego, bez silnych
      wartości odstających. Sprawdzaniem założeń zajmuje się wykład 05. Pojawi
      się tam też ", gloss("korelacja Spearmana"), ", stosowana, gdy te warunki
      nie są spełnione. Co się dzieje przy związku nieliniowym albo przy
      wartości odstającej, zobaczymy w sekcji o pułapkach korelacji."),

    # ========================================================================
    # Ćwiczenie: sformułuj hipotezy
    # ========================================================================
    lc_h2("ch4-cwiczenie", "Ćwiczenie: sformułuj hipotezy"),

    lc_p("Zanim przeprowadzimy test na danych, przećwicz pierwszy krok:
      zamianę pytania na parę hipotez. Najważniejsze jest to, czy pytanie
      wskazuje kierunek związku. Dla każdej sytuacji zapisz H₀ i Hₐ, a potem
      porównaj swoją odpowiedź z rozwiązaniem."),

    hypothesis_practice("ch4", list(
      list(
        question = "Producent lodów podejrzewa, że sprzedaż rośnie wraz
                    ze średnią temperaturą dnia. Zbiera dane z 60 dni.",
        h0 = "\\(H_0: \\rho \\leq 0\\) (brak dodatniego związku)",
        ha = "\\(H_a: \\rho > 0\\) (wyższa temperatura → wyższa sprzedaż)",
        note = "Jednostronny — pytanie jest kierunkowe („rośnie wraz z”)."
      ),
      list(
        question = "Czy istnieje jakikolwiek związek między liczbą godzin snu
                    a oceną z egzaminu?",
        h0 = "\\(H_0: \\rho = 0\\) (brak związku liniowego)",
        ha = "\\(H_a: \\rho \\neq 0\\) (jest związek)",
        note = "Dwustronny — pytamy neutralnie, bez zakładania kierunku."
      ),
      list(
        question = "Inżynier bada, czy większe stężenie dodatku X skraca
                    trwałość produktu na półce.",
        h0 = "\\(H_0: \\rho \\geq 0\\)",
        ha = "\\(H_a: \\rho < 0\\) (więcej dodatku → krótsza trwałość)",
        note = "Jednostronny (lewostronny) — hipoteza kierunkowa ujemna."
      )
    )),

    lc_p("W każdej z tych sytuacji kierunek hipotezy alternatywnej wynika ze słów
      pytania, a nie z danych. Ustala się go, zanim ktokolwiek zobaczy wykres
      rozrzutu."),

    # ========================================================================
    # WIDGET 1: Test korelacji dwustronny (krokowy)
    # ========================================================================
    lc_h2("ch4-krok", "Test korelacji — krok po kroku"),

    lc_p("Panel przeprowadza test dwustronny na danych symulowanych. Każdy
      scenariusz losuje pary obserwacji z populacji o zadanej korelacji: od 0.45
      (sen a ocena) do 0.6 (nawadnianie a plon), a w scenariuszu szkoleń BHP
      -0.55. Kolejne kroki prowadzą od wykresu rozrzutu przez \\(r\\)
      i statystykę \\(t\\) do decyzji."),

    figure_panel(
      label = "Ryc. 6.4",
      title = "Test korelacji — krok po kroku",
      uiOutput("ch4_hypothesis_panel"),
      lc_step_widget("ch4_test",
        steps = c("Dane (wykres rozrzutu)", "Korelacja z próby",
                  "Statystyka testowa", "p-wartość i decyzja"),
        toolbar = lc_toolbar(
          selectInput("ch4_scenario", "Scenariusz",
            choices = c(
              "Sen a ocena z egzaminu" = "sleep_grade",
              "Azotany a odległość od źródła" = "nitrate_dist",
              "Nawadnianie a plon" = "irrigation_yield",
              "Stężenie konserwantu a trwałość" = "preserv_shelf",
              "Szkolenie BHP a wypadki (IB)" = "training_accidents"
            ),
            selected = "sleep_grade"
          ),
          lc_slider("ch4_n", "Wielkość próby (n)", 15, 100, 40, 5),
          lc_action("ch4_new_sample", "Losuj próbę", icon = "shuffle", variant = "solid")
        ),
        plot_id = "ch4_step_plot"
      )
    ),

    lc_p("W scenariuszu domyślnym (\\(\\rho = 0.45\\), \\(n = 40\\)) wartość
      krytyczna statystyki \\(t\\) wynosi 2.02, co odpowiada \\(|r|\\) około 0.31.
      Próba, w której \\(r\\) wyszłoby dokładnie 0.45, dałaby \\(t = 3.11\\)
      i p-wartość 0.004, a więc odrzucenie H₀. Wylosowane \\(r\\) rozrzuca się
      jednak wokół 0.45: w 90% prób leży między 0.24 a 0.63. Test odrzuca H₀
      w około 87% prób. W pozostałych związek w populacji istnieje, ale próba
      nie wystarcza, żeby go wykazać. To ",
      gloss("błąd drugiego rodzaju"), " z rozdziału 3. Przy \\(n = 15\\) test
      odrzuca H₀ tylko w około 42% prób, przy \\(n = 100\\) praktycznie zawsze."),

    lc_p("Wynik nieistotny zapisujemy więc jako brak podstaw do odrzucenia H₀,
      a nie jako dowód, że korelacji nie ma. Przy \\(n = 15\\) i \\(\\rho = 0.45\\)
      taki wynik pojawia się częściej niż w co drugiej próbie. Odrzucenie H₀
      też mówi niewiele o samej korelacji: tylko tyle, że tak dużego \\(|r|\\)
      trudno się spodziewać, gdy w populacji \\(\\rho = 0\\). Ile wynosi
      \\(\\rho\\), lepiej pokazuje przedział ufności."),

    # ========================================================================
    # WIDGET 2: Test jednostronny (te same dane)
    # ========================================================================
    lc_h2("ch4-jednostronny", "A jeśli znamy kierunek?"),

    lc_p("Pytanie badawcze często wskazuje kierunek: nie „czy sen ma związek
      z oceną?”, tylko „czy więcej snu wiąże się z wyższą oceną?”. Wtedy
      stosujemy ", gloss("test jednostronny"), ". Hipoteza alternatywna obejmuje
      tylko jeden kierunek, a obszar odrzucenia leży w całości w jednym ogonie
      rozkładu t. Panel poniżej używa tej samej próby co test dwustronny powyżej,
      zmienia się tylko para hipotez."),

    figure_panel(
      label = "Ryc. 6.5",
      title = "Test korelacji jednostronny",
      uiOutput("ch4b_hypothesis_panel"),
      lc_step_widget("ch4b_test",
        steps = c("Dane", "Korelacja z próby", "Statystyka testowa",
                  "p-wartość i decyzja"),
        toolbar = lc_toolbar(
          helpText("Dane: te same co w teście dwustronnym powyżej.")
        ),
        plot_id = "ch4b_step_plot"
      )
    ),

    lc_p("Wartości \\(r\\) i \\(t\\) są w obu panelach identyczne, bo zależą tylko
      od danych. Zmienia się p-wartość. Gdy \\(r\\) ma znak zgodny z Hₐ,
      jednostronna p-wartość jest połową dwustronnej: dla \\(r = 0.45\\)
      i \\(n = 40\\) wynosi 0.002 zamiast 0.004. Wartość krytyczna spada z 2.02
      do 1.69, więc do odrzucenia H₀ wystarcza \\(|r|\\) około 0.26 zamiast 0.31."),

    lc_p("Ceną jest ślepota na drugi kierunek. Jeśli próba pokaże korelację
      przeciwnego znaku, nawet silną, jednostronna p-wartość przekroczy 0.5
      i H₀ nie odrzucimy. Dlatego kierunek wybiera się przed zebraniem danych,
      na podstawie pytania badawczego. Wybranie go po obejrzeniu wykresu
      dzieliłoby p-wartość na pół bez żadnego uzasadnienia."),

    # ========================================================================
    # Pułapki korelacji
    # ========================================================================
    lc_h2("ch4-pulapki", "Pułapki korelacji"),

    lc_p("Istotny test mówi tylko tyle, że \\(r\\) z próby trudno wytłumaczyć
      przypadkiem. Nie mówi, jaki kształt ma związek, czy nie zależy od
      pojedynczego punktu ani czy jedna zmienna wpływa na drugą. Te pytania
      trzeba rozstrzygnąć poza testem: patrząc na wykres i zastanawiając się,
      skąd pochodzą dane. Poniżej pięć klasycznych sytuacji, w których \\(r\\)
      wprowadza w błąd."),

    # --- 1. Kwartet Anscombe'a ---
    lc_p("Pierwszą pokazał statystyk Francis Anscombe w 1973 roku. Zbudował cztery
      zbiory po 11 punktów o niemal identycznych statystykach. W każdym średnia
      \\(x\\) wynosi 9, średnia \\(y\\) 7.50, wariancja \\(x\\) 11, wariancja
      \\(y\\) 4.12–4.13, korelacja 0.816–0.817, a prosta regresji to
      \\(y = 3 + 0.5x\\)."),

    figure_panel(
      label = "Ryc. 6.6",
      title = "Kwartet Anscombe’a",
      tags$img(src = "assets/anscombe-quartet.png",
               alt = "Kwartet Anscombe’a: cztery bardzo różne chmury punktów o niemal identycznych statystykach opisowych i korelacji.",
               tabindex = "0", role = "button",
               style = "width: 100%; border-radius: 4px;")
    ),

    lc_p("Tylko zbiór 1 wygląda tak, jak sugeruje \\(r \\approx 0.82\\): chmura
      punktów rozproszona wokół prostej. W zbiorze 2 punkty leżą na gładkim łuku,
      związek jest więc niemal doskonały, ale nie liniowy. W zbiorze 3 dziesięć
      punktów leży na jednej prostej (bez jedenastego punktu \\(r\\) wynosiłoby
      1.000), a jeden punkt odstaje i obniża korelację. W zbiorze 4 dziesięć
      punktów ma to samo \\(x = 8\\), a całą korelację tworzy jedenasty punkt
      z \\(x = 19\\). Statystyki opisowe i test korelacji nie odróżnią tych
      sytuacji, wykres odróżnia je od razu."),

    inline_callout(label = "Zasada",
      "Zanim zinterpretujesz r albo wynik testu korelacji, obejrzyj wykres
       rozrzutu."
    ),

    # --- 2. Nieliniowość przy r ≈ 0 ---
    lc_p("Zbiór 2 prowadzi do drugiej pułapki. Współczynnik Pearsona mierzy tylko
      związek liniowy, więc związek silny, ale zakrzywiony, może dać \\(r\\)
      bliskie zeru. Na wykresie poniżej \\(y\\) zależy od \\(x\\) kwadratowo:
      punkty układają się w literę U."),

    figure_panel(
      label = "Ryc. 6.7",
      title = "Nieliniowość przy r ≈ 0",
      tags$img(src = "assets/correlation-nonlinear.png",
               alt = "Silna zależność w kształcie litery U, dla której korelacja liniowa jest bliska zeru.",
               tabindex = "0", role = "button",
               style = "max-width: 500px; width: 100%; border-radius: 4px;")
    ),

    lc_p("Choć \\(y\\) jest niemal wyznaczone przez \\(x\\), \\(r\\) wynosi -0.004.
      Lewa połowa wykresu ma trend malejący, prawa rosnący, a iloczyny odchyleń
      w liczniku \\(r\\) z obu połówek wzajemnie się znoszą. Test korelacji nie
      odrzuciłby tu H₀, choć zależność jest bardzo silna. Wartość \\(r\\) bliska
      zeru oznacza brak związku liniowego, a nie brak związku."),

    # --- 3. Wartość odstająca (widget interaktywny) ---
    lc_p("Zbiory 3 i 4 pokazały, że pojedynczy punkt potrafi wyraźnie zmienić
      \\(r\\). ", gloss("wartość odstająca", "Wartość odstająca"), " ma tak duży
      wpływ, bo jej odchylenia od średnich są duże w obu zmiennych naraz, a ich
      iloczyn może przeważyć całą resztę sumy w liczniku. Panel losuje 50 punktów
      z dwóch niezależnych zmiennych, czyli z populacji, w której \\(\\rho = 0\\).
      Drugi przycisk dopisuje punkt leżący o 15 jednostek dalej niż największe
      \\(x\\) i największe \\(y\\) w danych."),

    figure_panel(
      label = "Ryc. 6.8",
      title = "Wpływ wartości odstającej na r",
      fluidRow(
        column(4,
          lc_action("ch4_gen_outlier", "Nowe dane (brak korelacji)", variant = "solid"),
          lc_action("ch4_add_outlier", "Dodaj wartość odstającą", variant = "solid"),
          br(), br(),
          uiOutput("ch4_outlier_r")
        ),
        column(8,
          zoom_plot_ui("ch4_outlier_plot", height = "300px")
        )
      )
    ),

    lc_p("Bez dodatkowego punktu \\(r\\) z 50 obserwacji leży zwykle blisko zera:
      w 90% losowań między -0.23 a 0.23. Jeden dopisany punkt podnosi je typowo
      do około 0.52, a w 90% losowań do wartości między 0.39 a 0.63. Drugi taki
      punkt, położony jeszcze dalej, podnosi \\(r\\) do około 0.8. Przy
      51 obserwacjach \\(r = 0.52\\) daje p-wartość około 0.0001, więc test
      wskazuje związek, którego w populacji nie ma. Wartości odstającej nie
      usuwa się jednak automatycznie. Najpierw trzeba ustalić, czy to błąd
      pomiaru, czy prawdziwa, nietypowa obserwacja. Bezpieczną praktyką jest
      podanie wyniku z tym punktem i bez niego."),

    # --- 4. Korelacja pozorna ---
    lc_p("Ostatnie dwie pułapki dotyczą interpretacji, a nie liczenia. Nawet silna,
      liniowa i istotna korelacja nie mówi, czy jedna zmienna wpływa na drugą.
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

    # --- 5. Paradoks Simpsona ---
    lc_p("Zmienna zakłócająca potrafi nawet odwrócić kierunek związku. Takie
      odwrócenie nazywa się ", gloss("paradoks Simpsona", "paradoksem Simpsona"),
      ". Panel pokazuje symulowane dane 210 uczniów z trzech szkół, po 70
      z każdej: liczbę godzin nauki w tygodniu i wynik egzaminu. W widoku
      globalnym wszystkie punkty analizujemy razem, w drugim widoku osobno
      w każdej szkole."),

    figure_panel(
      label = "Ryc. 6.9",
      title = "Paradoks Simpsona",
      div(class = "step-buttons",
        lc_action("ch4_simpson_global", "Spojrzenie globalne", variant = "outline"),
        lc_action("ch4_simpson_groups", "Paradoks", variant = "outline")
      ),
      lc_plot("ch4_simpson_plot", ratio = "1.5/1", max_height = "420px"),
      uiOutput("ch4_simpson_caption")
    ),

    lc_p("W danych połączonych \\(r = -0.49\\): wygląda na to, że im więcej
      nauki, tym gorszy wynik. W każdej szkole osobno korelacja jest jednak
      dodatnia: 0.78 w szkole słabej, 0.62 w średniej i 0.57 w silnej.
      Odwrócenie bierze się z różnic między szkołami. Uczniowie szkoły słabej
      uczą się średnio 24 godziny tygodniowo i zdobywają średnio 47 punktów,
      uczniowie szkoły silnej uczą się 9 godzin i zdobywają 82 punkty, bo
      materiał przychodzi im łatwiej. Po połączeniu grup, czyli ",
      gloss("agregacja", "agregacji"), ", korelacja porównuje głównie szkoły
      między sobą, a nie uczniów w obrębie szkoły. Poziom szkoły jest tu zmienną
      zakłócającą. Pytanie, czy dodatkowa godzina nauki pomaga uczniowi,
      dotyczy związku w obrębie szkoły. Więcej o paradoksie: ",
      tags$a(href = "https://en.wikipedia.org/wiki/Simpson%27s_paradox",
             target = "_blank", "Wikipedia →"), ", ",
      tags$a(href = "https://www.youtube.com/watch?v=ebEkn-BiW5k",
             target = "_blank", "film TED-Ed →"), "."),

    lc_p("Pięć pułapek układa się w dwie grupy. Kwartet Anscombe’a, nieliniowość
      i wartości odstające pokazują, że \\(r\\) może źle opisywać dane, a chroni
      przed tym wykres rozrzutu. Korelacja pozorna i paradoks Simpsona pokazują,
      że nawet trafnie policzone \\(r\\) może źle opisywać mechanizm. Przed tym
      chroni dopiero wiedza o tym, jak powstały dane i jakie zmienne pominięto."),

    lc_h2("ch4-cas", "Ćwiczenia", "CASchools — korelacja Pearsona"),

    lc_p("Na koniec trzy zadania na prawdziwych danych o szkołach w Kalifornii.
      W każdym zadaniu, zanim odsłonisz rozwiązanie, przewidź znak i siłę
      korelacji, a potem przeprowadź test."),

    lc_note("Dane",
      p("420 okręgów szkolnych Kalifornii (1998–1999). Plik: ",
        tags$code("dane/caschools.csv"), "."),
      p("Zmienne w zadaniach: ", tags$code("read"), " i ", tags$code("math"),
        " (wyniki testów), ", tags$code("income"),
        " (dochód okręgu, tys. USD), ", tags$code("student_teacher_ratio"),
        " (liczba uczniów na nauczyciela).")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 3 — Jak silnie czytanie i matematyka idą w parze?"),
      p("Oblicz korelację Pearsona między ", tags$code("read"), " i ", tags$code("math"),
        ". Zanim klikniesz: czy spodziewasz się korelacji dodatniej czy ujemnej?
        Silnej czy słabej? Zanotuj przewidywanie i sprawdź wynik."),
      lc_more("Rozwiązanie", uiOutput("cas_ch4_sol3"))
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 4 — Czy zamożniejsze okręgi uczą się lepiej?"),
      p("Oblicz korelację Pearsona między ", tags$code("income"), " a ", tags$code("read"),
        ". Jaki znak ma r? Czy korelacja jest istotna? Czy możesz wyciągnąć wniosek
        przyczynowy — że wyższy dochód ", tags$em("powoduje"), " lepsze wyniki?"),
      lc_more("Rozwiązanie", uiOutput("cas_ch4_sol4"))
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 5 — Czy przeładowane klasy szkodzą wynikom?"),
      p("Oblicz korelację Pearsona między ", tags$code("student_teacher_ratio"),
        " (STR) a ", tags$code("read"),
        ". Dlaczego korelacja jest ujemna? Czy jest istotna statystycznie?
        Czy silna praktycznie? Pomyśl, co może być zmienną zakłócającą."),
      lc_more("Rozwiązanie", uiOutput("cas_ch4_sol5"))
    ),

    lc_p("Trzy korelacje z tych samych danych pokazują trzy różne sytuacje. Przy
      420 okręgach nawet słaba korelacja daje bardzo małą p-wartość, dlatego
      obok p-wartości zawsze podaje się samo \\(r\\), najlepiej z przedziałem
      ufności. Jak opisywać siłę efektu, pokazuje rozdział 10."),

    lc_p("Korelacja wymaga dwóch zmiennych ilościowych. Gdy obie zmienne są
      jakościowe, na przykład płeć i wybrany kierunek studiów, nie ma czego
      wstawić do wzoru na \\(r\\). Związek opisuje wtedy tabela kontyngencji,
      a sprawdza go test χ² z następnego rozdziału."),

    lc_chapter_next(
      num       = "07",
      title     = "Test χ² niezależności",
      lead      = "związek między dwiema zmiennymi jakościowymi.",
      target_id = "ch-dwie-jakosciowe"
    )
  )
)

# ============================================================================
# DANE — CASchools (wczytane raz przy ładowaniu modułu)
# ============================================================================

.ch4_cas <- read.csv(file.path(app_dir, "dane", "caschools.csv"),
                     stringsAsFactors = FALSE)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  # --- Parametry scenariuszy ---
  scenario_params <- list(
    sleep_grade = list(
      r_true = 0.45, xlab = "Godziny snu", ylab = "Ocena z egzaminu",
      x_mean = 7,   x_sd = 1.5,
      y_mean = 70,  y_sd = 12,
      title = "Sen a ocena",
      question = "Czy istnieje związek między ilością snu a oceną z egzaminu?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak związku liniowego)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest związek)",
      question_1s = "Czy więcej snu wiąże się z wyższą oceną?",
      h0_text_1s = "\\(H_0: \\rho \\leq 0\\)",
      h1_text_1s = "\\(H_a: \\rho > 0\\)",
      alt_1s = "greater"),
    nitrate_dist = list(
      r_true = 0.55, xlab = "Odległość od źródła (km)", ylab = "Stężenie azotanów (mg/l)",
      x_mean = 15,  x_sd = 8,
      y_mean = 30,  y_sd = 12,
      title = "Azotany wzdłuż rzeki",
      question = "Czy stężenie azotanów jest powiązane z odległością od źródła?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak związku)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest związek)",
      question_1s = "Czy stężenie azotanów rośnie z odległością od źródła?",
      h0_text_1s = "\\(H_0: \\rho \\leq 0\\)",
      h1_text_1s = "\\(H_a: \\rho > 0\\)",
      alt_1s = "greater"),
    irrigation_yield = list(
      r_true = 0.60, xlab = "Nawadnianie (mm/tydzień)", ylab = "Plon (t/ha)",
      x_mean = 25,  x_sd = 10,
      y_mean = 6,   y_sd = 1.5,
      title = "Nawadnianie a plon",
      question = "Czy ilość nawadniania jest powiązana z plonem?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak związku)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest związek)",
      question_1s = "Czy większe nawadnianie daje wyższe plony?",
      h0_text_1s = "\\(H_0: \\rho \\leq 0\\)",
      h1_text_1s = "\\(H_a: \\rho > 0\\)",
      alt_1s = "greater"),
    preserv_shelf = list(
      r_true = 0.50, xlab = "Stężenie konserwantu (mg/kg)", ylab = "Trwałość (dni)",
      x_mean = 200, x_sd = 60,
      y_mean = 30,  y_sd = 8,
      title = "Konserwant a trwałość",
      question = "Czy stężenie konserwantu wpływa na trwałość produktu?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak związku)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest związek)",
      question_1s = "Czy większe stężenie konserwantu wydłuża trwałość?",
      h0_text_1s = "\\(H_0: \\rho \\leq 0\\)",
      h1_text_1s = "\\(H_a: \\rho > 0\\)",
      alt_1s = "greater"),
    training_accidents = list(
      r_true = -0.55,
      xlab = "Godziny szkolenia BHP / rok",
      ylab = "Liczba wypadków / 100 prac. / rok",
      x_mean = 20,  x_sd = 7,
      y_mean = 8,   y_sd = 3,
      title = "Szkolenie BHP a wypadki",
      question = "Czy liczba godzin szkolenia BHP wiąże się z liczbą wypadków?",
      h0_text = "\\(H_0: \\rho = 0\\) (brak związku)",
      h1_text = "\\(H_a: \\rho \\neq 0\\) (jest związek)",
      question_1s = "Czy więcej godzin szkolenia BHP wiąże się z mniejszą liczbą wypadków?",
      h0_text_1s = "\\(H_0: \\rho \\geq 0\\)",
      h1_text_1s = "\\(H_a: \\rho < 0\\)",
      alt_1s = "less")
  )

  # --- Współdzielone dane ---
  # Jedna próbka dla testu dwustronnego i jednostronnego; po zmianie
  # scenariusza albo n stara próbka nie pasuje już do opisu pytania.
  ch4_data_state <- reactiveVal(NULL)
  ch4_data <- reactive({
    state <- ch4_data_state()
    if (is.null(state)) return(NULL)
    req(input$ch4_scenario, input$ch4_n)

    if (!identical(state$scenario, input$ch4_scenario) ||
        !isTRUE(state$n == input$ch4_n)) {
      return(NULL)
    }

    state$data
  })

  # Kroki widgetów (1..4) żyją w przeglądarce; nowa próba ani zmiana
  # scenariusza nie cofa kroku.
  ch4_step <- lc_step_server("ch4_test", input)$step
  ch4b_step <- lc_step_server("ch4b_test", input)$step

  observeEvent(input$ch4_new_sample, {
    req(input$ch4_scenario, input$ch4_n)
    par <- scenario_params[[input$ch4_scenario]]
    req(!is.null(par))
    n <- input$ch4_n
    ch4_data_state(list(
      scenario = input$ch4_scenario,
      n = n,
      data = generate_correlation_data(n, par$r_true, "linear",
                                       x_mean = par$x_mean, x_sd = par$x_sd,
                                       y_mean = par$y_mean, y_sd = par$y_sd)
    ))
  }, ignoreInit = TRUE)

  # Kroki 1–2: wykres rozrzutu; prosta MNK nowa w kroku 2. Rama z danych.
  ch4_scatter_plot <- function(d, par, step) {
    pad <- function(v) range(v) + c(-1, 1) * diff(range(v)) * 0.06
    ggplot(d, aes(x = x, y = y)) +
      step_layer(geom_point, "data", size = 2.5) +
      step_show(step, 2, step_layer(geom_smooth, step_role(step, 2), method = "lm",
                                    formula = y ~ x, se = FALSE)) +
      labs(x = par$xlab, y = par$ylab) +
      step_frame(xlim = pad(d$x), ylim = pad(d$y))
  }

  # =============================================
  # WIDGET 1: Test dwustronny
  # =============================================

  output$ch4_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch4_scenario]]
    d <- ch4_data()
    tagList(
      lc_status(
        p(tags$b("Pytanie potoczne:")),
        p(tags$em(paste0("„", par$question, "”")))
      ),
      lc_formula_box(
        p(tags$b("Hipoteza formalna (dwustronna):")),
        p(withMathJax(par$h0_text)),
        p(withMathJax(par$h1_text))
      ),
      if (is.null(d)) {
        div(style = "text-align: center; margin: 10px 0; color: var(--upwr-reference);",
          p(tags$em("Kliknij „Losuj próbę”"))
        )
      }
    )
  })

  zoom_plot_server("ch4_step_plot", reactive({
    d <- ch4_data()
    step <- ch4_step()
    par <- scenario_params[[input$ch4_scenario]]

    if (is.null(d)) return(NULL)

    if (step <= 2) {
      ch4_scatter_plot(d, par, step)
    } else {
      n <- nrow(d)
      r_val <- cor(d$x, d$y)
      t_stat <- r_val * sqrt(n - 2) / sqrt(1 - r_val^2)
      step_null_plot(t_stat, df = n - 2, type = "t",
                     phase = if (step == 3) "stat" else "decision")
    }
  }))

  output$ch4_test_text <- renderUI({
    d <- ch4_data()
    step <- ch4_step()
    par <- scenario_params[[input$ch4_scenario]]

    if (is.null(d)) return(NULL)

    n <- nrow(d)
    r_val <- cor(d$x, d$y)
    t_stat <- r_val * sqrt(n - 2) / sqrt(1 - r_val^2)
    p_val <- 2 * pt(-abs(t_stat), df = n - 2)

    switch(as.character(step),
      "1" = tagList(
        "n = ", step_num(n), " par obserwacji. Każdy punkt to jedna obserwacja
        z dwiema wartościami: ", paste0(par$xlab, " i ", par$ylab, ". Czy widać trend?")
      ),
      "2" = tagList(
        "Korelacja z próby: r = ", step_num(lc_fmt(r_val, 3)),
        ". Ale czy to wystarczająco daleko od zera, by odrzucić H₀?"
      ),
      "3" = tagList(
        paste0("t = ", lc_fmt(r_val, 3), " · √", n - 2, " / √(1 − ",
               lc_fmt(r_val^2, 3), ") = "),
        step_num(lc_fmt(t_stat, 3)),
        paste0(". Zamieniamy r na statystykę t, żeby móc porównać z rozkładem t(",
               n - 2, ").")
      ),
      "4" = step_verdict(p_val)
    )
  })

  # =============================================
  # WIDGET 2: Jednostronny (te same dane)
  # =============================================

  output$ch4b_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch4_scenario]]
    d <- ch4_data()
    tagList(
      lc_status(
        p(tags$b("Pytanie potoczne (kierunkowe):")),
        p(tags$em(paste0("„", par$question_1s, "”")))
      ),
      lc_formula_box(
        p(tags$b("Hipoteza formalna (jednostronna):")),
        p(withMathJax(par$h0_text_1s)),
        p(withMathJax(par$h1_text_1s))
      ),
      if (is.null(d)) {
        div(style = "text-align: center; margin: 10px 0; color: var(--upwr-reference);",
          p(tags$em("Najpierw wylosuj próbę w teście dwustronnym powyżej"))
        )
      }
    )
  })

  zoom_plot_server("ch4b_step_plot", reactive({
    d <- ch4_data()
    step <- ch4b_step()
    par <- scenario_params[[input$ch4_scenario]]

    if (is.null(d)) return(NULL)

    n <- nrow(d)
    r_val <- cor(d$x, d$y)
    t_stat <- r_val * sqrt(n - 2) / sqrt(1 - r_val^2)

    if (step <= 2) {
      ch4_scatter_plot(d, par, step)
    } else {
      step_null_plot(t_stat, df = n - 2, type = "t", alternative = par$alt_1s,
                     phase = if (step == 3) "stat" else "decision")
    }
  }))

  output$ch4b_test_text <- renderUI({
    d <- ch4_data()
    step <- ch4b_step()
    par <- scenario_params[[input$ch4_scenario]]

    if (is.null(d)) return(NULL)

    n <- nrow(d)
    r_val <- cor(d$x, d$y)
    t_stat <- r_val * sqrt(n - 2) / sqrt(1 - r_val^2)
    p_val <- pt(t_stat, df = n - 2, lower.tail = (par$alt_1s == "less"))

    switch(as.character(step),
      "1" = tagList(
        "n = ", step_num(n), " (te same dane co wyżej). Te same obserwacje,
        ale pytamy o kierunek związku."
      ),
      "2" = tagList(
        "r = ", step_num(lc_fmt(r_val, 3)), ", tak samo jak w teście dwustronnym.
        Zmieniło się tylko pytanie."
      ),
      "3" = tagList(
        "t = ", step_num(lc_fmt(t_stat, 3)), ", bez zmian. Obszar odrzucenia
        leży teraz tylko w ",
        if (par$alt_1s == "greater") "prawym" else "lewym", " ogonie."
      ),
      "4" = tagList(
        "Jednostronnie: ", step_verdict(p_val)
      )
    )
  })

  # =============================================
  # Pułapka: wartość odstająca
  # =============================================
  ch4_outlier_data <- reactiveVal(NULL)

  observeEvent(input$ch4_gen_outlier, {
    ch4_outlier_data(generate_correlation_data(50, 0, "none"))
  })

  observeEvent(input$ch4_add_outlier, {
    df <- ch4_outlier_data()
    if (is.null(df)) return()
    outlier <- data.frame(x = max(df$x) + 15, y = max(df$y) + 15)
    ch4_outlier_data(rbind(df, outlier))
  })

  zoom_plot_server("ch4_outlier_plot", reactive({
    df <- ch4_outlier_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Nowe dane”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      n_base <- 50
      r_val <- cor(df$x, df$y)

      ggplot(df, aes(x = x, y = y)) +
        geom_point(color = ifelse(seq_len(nrow(df)) > n_base, col_reject, col_h0),
                   size = ifelse(seq_len(nrow(df)) > n_base, 4, 2.5),
                   alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE, color = col_reject, alpha = 0.5) +
        labs(
             x = "X", y = "Y") +
        theme()
    }
  }))

  output$ch4_outlier_r <- renderUI({
    df <- ch4_outlier_data()
    if (is.null(df)) return(NULL)
    r_val <- cor(df$x, df$y)
    n_outliers <- max(0, nrow(df) - 50)
    tagList(
      lc_stat_box("r", round(r_val, 3), color = col_h0),
      lc_stat_box("Wartości odstające", n_outliers, color = col_reject)
    )
  })

  # --- Ćwiczenia CASchools ---

  .cas_cor <- function(x, y) {
    ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
    n <- length(x); r <- cor(x, y)
    t_val <- r * sqrt((n - 2) / (1 - r^2)); df <- n - 2
    p_val <- 2 * pt(-abs(t_val), df)
    list(r = r, t = t_val, df = df, p = p_val, n = n, r2 = r^2)
  }

  output$cas_ch4_sol3 <- renderUI({
    r <- .cas_cor(.ch4_cas$read, .ch4_cas$math)
    tagList(
      tags$ul(
        tags$li(sprintf("r = %.3f, t(%d) = %.3f, p %s %s",
          r$r, r$df, r$t,
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
        tags$li(sprintf("R² = %.3f → czytanie wyjaśnia %.1f%% wariancji wyników z matematyki",
                        r$r2, 100 * r$r2))
      ),
      tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀"),
      p(tags$b("Interpretacja:"), " ",
        sprintf("r = %.3f — korelacja silnie dodatnia.
          Okręgi z lepszymi wynikami z czytania osiągają też wyższe wyniki z matematyki
          (%.1f%% wspólnej wariancji). Obie zmienne mierzą ogólny poziom edukacji.",
          r$r, 100 * r$r2))
    )
  })

  output$cas_ch4_sol4 <- renderUI({
    r <- .cas_cor(.ch4_cas$income, .ch4_cas$read)
    tagList(
      tags$ul(
        tags$li(sprintf("r = %.3f, t(%d) = %.3f, p %s %s",
          r$r, r$df, r$t,
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
        tags$li(sprintf("R² = %.3f — dochód wyjaśnia %.1f%% wariancji wyników",
                        r$r2, 100 * r$r2))
      ),
      tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀"),
      p(tags$b("Korelacja ≠ przyczynowość:"),
        " Korelacja jest istotna i dodatnia — bogatsze okręgi mają wyższe wyniki.
        Jednak nie możemy stwierdzić, że dochód ", tags$em("powoduje"),
        " lepsze wyniki. Trzecia zmienna (jakość nauczycieli, kapitał kulturowy rodziny)
        może tłumaczyć obie. Potrzeba badania eksperymentalnego lub quasi-eksperymentalnego.")
    )
  })

  output$cas_ch4_sol5 <- renderUI({
    r <- .cas_cor(.ch4_cas$student_teacher_ratio, .ch4_cas$read)
    tagList(
      tags$ul(
        tags$li(sprintf("r = %.3f, t(%d) = %.3f, p %s %s",
          r$r, r$df, r$t,
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
        tags$li(sprintf("R² = %.3f — STR wyjaśnia %.1f%% wariancji wyników",
                        r$r2, 100 * r$r2))
      ),
      tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀"),
      p(tags$b("Interpretacja:"), " ",
        sprintf("r = %.3f — korelacja ujemna: wyższy STR (więcej uczniów na nauczyciela)
          wiąże się z niższymi wynikami z czytania. STR wyjaśnia tylko %.1f%%
          wariancji. STR często odzwierciedla zamożność okręgu, więc dochód może
          być zmienną zakłócającą tej zależności.",
          r$r, 100 * r$r2))
    )
  })

  # ---- Widget Paradoks Simpsona ----
  ch4_simpson_data <- local({
    set.seed(42)
    schools <- c("Szkoła słaba", "Szkoła średnia", "Szkoła silna")
    school_levels <- c(slaba = 48, srednia = 65, silna = 82)
    study_means   <- c(slaba = 24, srednia = 17, silna =  9)
    study_sd <- 4.5
    within_slope <- 1.4
    within_noise <- 7
    n_per_group <- 70

    rows <- lapply(seq_along(school_levels), function(i) {
      key <- names(school_levels)[i]
      study <- pmax(0.5, rnorm(n_per_group, mean = study_means[key], sd = study_sd))
      score <- school_levels[key] +
               within_slope * (study - mean(study)) +
               rnorm(n_per_group, 0, within_noise)
      data.frame(
        szkola  = factor(schools[i], levels = schools),
        godziny = study,
        wynik   = score
      )
    })
    do.call(rbind, rows)
  })

  ch4_simpson_view <- reactiveVal("global")
  observeEvent(input$ch4_simpson_global, ch4_simpson_view("global"))
  observeEvent(input$ch4_simpson_groups, ch4_simpson_view("groups"))

  zoom_plot_server("ch4_simpson_plot", reactive({
    df <- ch4_simpson_data
    view <- ch4_simpson_view()

    school_colors <- c(
      "Szkoła słaba"   = unname(upwr_cat["terakota"]),
      "Szkoła średnia" = unname(upwr_cat["bursztyn"]),
      "Szkoła silna"   = unname(upwr_cat["niebo"])
    )

    if (view == "global") {
      ggplot(df, aes(x = godziny, y = wynik)) +
        geom_point(color = "grey75", size = 2.4, alpha = 0.85) +
        geom_smooth(method = "lm", se = FALSE,
                    color = upwr_secondary, linewidth = 1.4) +
        labs(x = "Godziny nauki / tydzień", y = "Wynik z egzaminu") +
        theme_upwr()
    } else {
      ggplot(df, aes(x = godziny, y = wynik, color = szkola)) +
        geom_smooth(method = "lm", se = FALSE,
                    color = upwr_secondary, linewidth = 1.0,
                    linetype = "dashed", alpha = 0.5,
                    aes(group = 1)) +
        geom_point(size = 2.4, alpha = 0.85) +
        geom_smooth(method = "lm", se = FALSE, linewidth = 1.2,
                    aes(group = szkola)) +
        scale_color_manual(values = school_colors, name = NULL) +
        labs(x = "Godziny nauki / tydzień", y = "Wynik z egzaminu") +
        theme_upwr() +
        theme(legend.position = "top")
    }
  }))

  output$ch4_simpson_caption <- renderUI({
    df <- ch4_simpson_data
    view <- ch4_simpson_view()
    r_global <- round(cor(df$godziny, df$wynik), 2)

    if (view == "global") {
      lc_status(
        lc_verdict(tags$strong("Spojrzenie globalne:"), type = "warning"),
        sprintf(" r = %s. Więcej godzin nauki → niższy wynik z egzaminu?",
                format(r_global, nsmall = 2))
      )
    } else {
      r_per_school <- df %>%
        group_by(szkola) %>%
        summarise(r = cor(godziny, wynik), .groups = "drop")
      r_text <- paste0(r_per_school$szkola, ": r = ", round(r_per_school$r, 2),
                       collapse = "; ")

      lc_status(
        lc_verdict(tags$strong("Podział na szkoły:"), type = "ok"),
        " w każdej szkole z osobna więcej nauki → wyższy wynik (",
        r_text,
        ")."
      )
    }
  })
}
