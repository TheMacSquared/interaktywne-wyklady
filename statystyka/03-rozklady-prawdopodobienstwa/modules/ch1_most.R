# ============================================================================
# CHAPTER 1: Od danych do prawdopodobieństwa
# ============================================================================

ch1_ui <- list(
  id = "ch-most", num = "01", title = "Od danych do prawdopodobieństwa",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 01 · Rozkłady prawdopodobieństwa",
      num    = "01",
      title  = "Od danych do prawdopodobieństwa.",
      lead   = "Histogram z ankiety opisuje dwustu konkretnych studentów. Żeby
                powiedzieć coś o następnym, potrzebujemy modelu: reguły, która
                mówi, jak często pojawia się każdy wynik. Taką regułą jest rozkład
                prawdopodobieństwa, a częstości z danych są jego przybliżeniem."
    ),

    lc_p("W poprzednim wykładzie opisywaliśmy dane z ankiety: liczyliśmy
      częstości względne kategorii, rysowaliśmy histogramy, obliczaliśmy ",
      gloss("średnia", "średnią"), " i ",
      gloss("odchylenie standardowe"), ". Wszystkie te liczby dotyczą jednej
      konkretnej ", gloss("próba", "próby"), ". Inna grupa studentów dałaby trochę inne wyniki.
      W tym rozdziale przechodzimy od opisu danych do modelu, który je
      wytwarza. Zaczniemy od rzutów kostką, bo tam model znamy z góry
      i możemy sprawdzić, jak dane się do niego zbliżają."),

    # ========================================================================
    # WIDGET 1: Stabilizacja częstości (rzut kostką)
    # ========================================================================
    lc_h2("ch1-pwl", "Prawo wielkich liczb w akcji"),

    lc_p("Wynik rzutu kostką oznaczmy literą \\(X\\). To ",
      gloss("zmienna losowa"), ": jej wartość zależy od przypadku, a przed
      rzutem znamy tylko możliwe wyniki, od 1 do 6. Dla uczciwej kostki każda
      ścianka ma tę samą szansę, więc \\(P(X = k) = 1/6 \\approx 0.167\\)
      dla każdego \\(k\\). Po \\(n\\) rzutach możemy policzyć, ile razy wypadła
      ścianka \\(k\\), i obliczyć ",
      gloss("częstość względna", "częstość względną"), ",
      dokładnie tak jak w ", gloss("tabela częstości", "tabeli częstości"), " z poprzedniego wykładu.
      ", gloss("prawo wielkich liczb", "Prawo wielkich liczb"), " mówi, że
      wraz ze wzrostem liczby rzutów częstość względna zbliża się do
      prawdopodobieństwa."),

    lc_p("Panel rzuca wirtualną kostką. Każdy rzut kładzie kafelek nad
      wylosowaną ścianką, a przerywana linia pokazuje, ile kafelków
      przypadałoby na ściankę, gdyby każda wypadała dokładnie w 1/6 rzutów.
      Po 60 rzutach kafelki zastępują słupki częstości względnych."),

    s3p_11a_ui,

    lc_p("Po kilku rzutach częstości są chaotyczne: jedna ścianka może nie
      wypaść ani razu, inna kilka razy z rzędu. Po kilkuset rzutach słupki
      przylegają do linii 1/6. Jak szybko to następuje, sprawdza drugi panel.
      Powtarza on serię 1000 rzutów wiele razy i w każdej serii śledzi
      częstość szóstek. Żółty pas to zakres, w którym zgodnie z rachunkiem
      prawdopodobieństwa mieści się około 95% serii."),

    s3p_11c_ui,

    lc_p("Każda seria błądzi inaczej, ale pas zwęża się w przewidywalnym tempie.
      Przy 10 rzutach częstość szóstek odchyla się od 1/6 typowo o około 0.12,
      czyli prawie o tyle, ile wynosi samo prawdopodobieństwo. Przy 100 rzutach
      typowe odchylenie spada do 0.037, przy 1000 do 0.012, a przy 10 000
      do 0.004. Stukrotnie więcej rzutów daje dziesięciokrotnie mniejszy błąd."),

    lc_p("Prawo wielkich liczb nie mówi, że kostka „wyrównuje” wyniki. Jeśli
      szóstka długo nie wypadała, w kolejnym rzucie nadal ma szansę 1/6.
      Częstość zbliża się do prawdopodobieństwa dlatego, że początkowe
      nadwyżki i niedobory toną w coraz większej liczbie rzutów, a nie
      dlatego, że ktoś je odrabia."),

    # ========================================================================
    # WIDGET 0: Rozkład empiryczny vs teoretyczny
    # ========================================================================
    lc_h2("ch1-rozklad-emp", "Rozkład empiryczny vs teoretyczny"),

    lc_p("Częstości policzone z danych, czy to dla ścianek kostki, grup krwi,
      czy przedziałów wzrostu, tworzą ", gloss("rozkład empiryczny"), ": opis
      tego, jak często każda wartość pojawiła się w zebranej próbie. Jego
      odpowiednikiem po stronie modelu jest ", gloss("rozkład teoretyczny"),
      ": reguła, która każdej możliwej wartości przypisuje prawdopodobieństwo.
      Model może dotyczyć zmiennej dowolnego typu. Dla uczciwej kostki są to
      prawdopodobieństwa 1/6, dla grupy krwi udziały grup w populacji, a dla
      wzrostu krzywa opisana wzorem i kilkoma parametrami."),

    lc_p("Dla zmiennych o kilku wartościach oba rozkłady porównujemy słupek po
      słupku, tak jak przy kostce. ", gloss("zmienna ciągła", "Zmienne ciągłe"), ", takie jak wzrost czy czas
      dojazdu, przyjmują wartości z całego przedziału. Rozkład empiryczny
      pokazuje dla nich ", gloss("histogram"), ", a teoretyczny gładka krzywa
      gęstości. Żeby dały się porównać, oba rysujemy w skali gęstości: łączne
      pole słupków histogramu wynosi 1 i tak samo pole pod krzywą. Wtedy pole
      nad dowolnym przedziałem odpowiada odsetkowi obserwacji w tym przedziale.
      Krzywym gęstości przyjrzymy się dokładniej w rozdziale 4."),

    lc_p("Panel losuje próbę z jednego z czterech modeli. Dwa są ciągłe
      i rysują histogram oraz krzywą gęstości. Dwa są dyskretne: liczby
      książek przeczytanych w semestrze (rozkład Poissona) i liczby trafionych
      pytań w teście (rozkład dwumianowy). Dla nich dane pokazują słupki
      częstości, a model punkty z prawdopodobieństwami. Model można nałożyć
      na dane."),

    figure_panel(
      label = "Ryc. 1.3",
      title = "Histogram (dane) vs krzywa gęstości (model)",
      full_width = TRUE,
      lc_sf_figure("d03-emp", max_height = "380px",
        control = list(
          lc_group("Rozkład źródłowy", lc_select("ch1_emp_dist",
            choices = c(
              "Wzrost studentów (normalny, ciągła)"            = "normal",
              "Czas dojazdu (skośny, ciągła)"                  = "skewed",
              "Książki przeczytane w semestrze (Poissona, dyskretna)" = "poisson",
              "Trafione pytania w teście (dwumianowy, dyskretna)"     = "binomial"
            ),
            selected = "normal", aria_label = "Rozkład źródłowy")),
          lc_sf_slider("ch1_emp_n", "Wielkość próby", 20, 5000, 200, 20),
          checkboxInput("ch1_show_hist", "Dane z próby (histogram lub słupki)", value = TRUE),
          checkboxInput("ch1_show_density", "Model (krzywa gęstości lub prawdopodobieństwa)", value = FALSE)),
        toolbar = lc_sf_action("new", "ch1_emp_resample", "Losuj nową próbę", icon = "shuffle", variant = "solid"))
    ),

    lc_p("Wzrost losujemy z rozkładu normalnego o średniej 170 cm
      i odchyleniu standardowym 8 cm. To wartości bliskie tym z ankiety,
      gdzie średni wzrost wynosił 171.1 cm, a odchylenie standardowe 8.1 cm.
      Według modelu 68% osób ma od 162 do 178 cm. Czas dojazdu pochodzi
      z rozkładu skośnego, tego samego, z którego wygenerowano dane ankiety.
      Model ma średnią 35 min i medianę 31.7 min, a w ankiecie wyszło
      35.7 i 32.9 min. Model przewiduje, że 8.8% osób dojeżdża dłużej niż
      godzinę. W ankiecie takich osób było 19 na 200, czyli 9.5%."),

    lc_p("Przy próbie liczącej 200 obserwacji histogram ma zarys krzywej,
      ale jest poszarpany, a każde nowe losowanie zmienia wysokość słupków.
      Przy kilku tysiącach obserwacji słupki układają się niemal dokładnie
      pod krzywą i kolejne losowania prawie się od siebie nie różnią. To samo
      prawo wielkich liczb co przy kostce, tylko zastosowane do przedziałów
      zamiast pojedynczych ścianek. Rozkład teoretyczny jest ideałem, a dane
      są jego niedoskonałym odbiciem, tym wierniejszym, im większa próba."),

    # ========================================================================
    # WIDGET 2: Częstości vs prawdopodobieństwo
    # ========================================================================
    lc_h2("ch1-czestosci", "Częstości vs prawdopodobieństwo"),

    lc_p("Histogram z krzywą porównywaliśmy na oko. Dla zmiennej o kilku
      wynikach zgodność danych z modelem da się zmierzyć jedną liczbą:
      największą różnicą między częstością względną a prawdopodobieństwem.
      Model nie musi przy tym przypisywać wszystkim wynikom tej samej szansy.
      Obok uczciwej kostki panel pokazuje kostkę obciążoną, na której
      szóstka wypada z prawdopodobieństwem 0.5, a każda z pozostałych ścianek
      z prawdopodobieństwem 0.1, oraz rzut monetą."),

    lc_p("Słupki pokazują częstości z wylosowanych obserwacji, punkty
      połączone linią — prawdopodobieństwa z modelu. Pod wykresem panel
      podaje największą różnicę między nimi."),

    figure_panel(
      label = "Ryc. 1.4",
      title = "Porównanie: teoria vs obserwacja",
      full_width = TRUE,
      lc_sf_figure("d03-freq", max_height = "350px", legend = TRUE,
        control = list(
          lc_segmented("ch1_scenario", "Scenariusz", choices = c(
              "Uczciwa kostka"   = "fair",
              "Obciążona kostka" = "loaded",
              "Moneta"           = "coin"
            ), selected = "fair"),
          lc_sf_slider("ch1_n_obs", "Liczba obserwacji", 10, 5000, 100, 10)),
        toolbar = lc_sf_action("new", "ch1_resample", "Losuj ponownie", icon = "shuffle", variant = "solid"),
        after = lc_caption(lc_sf_value("diff", "")))
    ),

    lc_p("Przy 100 rzutach uczciwą kostką największa różnica przekracza 0.05
      mniej więcej w dwóch losowaniach na trzy. Przy 500 rzutach zdarza się
      to już tylko w około 2 losowaniach na 100. Dla monety przy 100 rzutach
      próg 0.05 zostaje przekroczony rzadziej, w około jednym losowaniu
      na trzy, bo największą różnicę wybieramy spośród dwóch wyników,
      a nie sześciu."),

    lc_p("Obciążona kostka pokazuje drugą stronę tej zależności. Przy 100
      rzutach częstość szóstek waha się typowo o 0.05 wokół 0.5, więc nie da
      się jej pomylić z wartością 1/6 ≈ 0.167, jakiej oczekiwalibyśmy od
      uczciwej kostki. Częstości nie tylko przybliżają znany model, ale
      pozwalają też odróżnić jeden model od drugiego. Na tym pomyśle opiera
      się ", gloss("wnioskowanie statystyczne"), ", któremu poświęcimy kolejne wykłady."),

    # ========================================================================
    # WIDGET 3: Czym jest rozkład?
    # ========================================================================
    lc_h2("ch1-rozklad", "Czym jest rozkład prawdopodobieństwa?"),

    lc_p("Każdy z modeli w tym rozdziale przypisywał możliwym wynikom ich
      prawdopodobieństwa. Taki kompletny opis nazywamy ",
      gloss("rozkład prawdopodobieństwa", "rozkładem prawdopodobieństwa"),
      ". Dla zmiennej o skończonej liczbie wyników jest to po prostu lista
      wartości \\(k\\) i prawdopodobieństw \\(P(X = k)\\). Nie każda lista
      liczb jest rozkładem. Prawdopodobieństwa muszą być nieujemne i muszą
      sumować się do 1, bo któryś z wyników na pewno wystąpi."),

    lc_formula_annotated(
      "⟦1|P(X = k)⟧ \\geq 0 \\quad \\text{dla każdego } k, \\qquad ⟦2|\\sum_{k}⟧ P(X = k) = 1",
      notes  = list("prawdopodobieństwo wyniku k", "suma po wynikach"),
      colors = c("indygo", "bursztyn")
    ),

    lc_p("Przykład: uczciwa kostka sześcienna. Zmienna \\(X\\) przyjmuje wartości
      od 1 do 6 i każda ma to samo prawdopodobieństwo. Tabela pokazuje cały
      rozkład."),

    tags$div(style = "max-width:600px; margin:0 auto;",
      lc_table(
        local({
          df <- data.frame(matrix(c("P(X = k)", rep("1/6", 6)), nrow = 1),
                           stringsAsFactors = FALSE)
          names(df) <- c("k", as.character(1:6))
          df
        }),
        cols = c(
          list(lc_col("k", "k", "row")),
          lapply(as.character(1:6), function(k) lc_col(k, k, "text"))
        ),
        prose = TRUE,
        fit = TRUE,
        label = "Rozkład prawdopodobieństwa uczciwej kostki"
      )
    ),

    lc_p("Sprawdź, które z tabelek poniżej są rozkładami prawdopodobieństwa,
      a potem odsłoń werdykty."),

    s3p_15b_ui,

    lc_note("Zasada", rule = TRUE,
      "Rozkład prawdopodobieństwa opisuje model, a nie dane. Częstości
       z próby przybliżają ten model tym lepiej, im większa jest próba."
    ),

    lc_p("Rozkład zawiera całą informację o zmiennej losowej, ale tak jak
      histogram jest za obszerny, żeby go streścić w jednym zdaniu.
      W statystyce opisowej streszczaliśmy dane średnią i odchyleniem
      standardowym. Ich odpowiedniki dla rozkładów poznamy w następnym
      rozdziale."),

    lc_chapter_next(
      num       = "02",
      title     = "Wartość oczekiwana i wariancja",
      lead      = "czego się spodziewać i jak mierzyć rozrzut wyników.",
      target_id = "ch-ev-var"
    )
  )
)

# --------------------------------------------------------------------------
# Chapter 1 Server
# --------------------------------------------------------------------------

ch1_server <- function(input, output, session) {

  # Ryc. 1.3 i 1.4 losuje i rysuje sim_charts.js w przeglądarce.
}
