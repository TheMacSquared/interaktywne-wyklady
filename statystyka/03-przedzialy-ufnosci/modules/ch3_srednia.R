# ============================================================================
# CHAPTER 3: Przedział dla średniej
# ============================================================================

ch3_ui <- list(
  id    = "ch-srednia",
  num   = "03",
  title = "Przedział dla średniej",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Przedziały ufności",
      num    = "03",
      title  = "Przedział dla średniej.",
      lead   = "Przedział dla średniej składa się z trzech liczb: średniej z próby,
                błędu standardowego i wartości krytycznej. Gdy odchylenie
                standardowe populacji trzeba oszacować z danych, wartość krytyczną
                bierzemy z rozkładu t-Studenta, a nie z rozkładu normalnego."
    ),

    lc_p("W poprzednim rozdziale przedziały ufności pojawiały się jako gotowe
      siatki: scena mierzyła grupkę, zarzucała przedział i sprawdzała, czy
      złapał μ. Teraz zajrzymy do środka. Zobaczymy, z czego składa się
      przedział dla średniej, zbudujemy go krok po kroku, a potem rozszerzymy
      tę samą konstrukcję na różnicę dwóch średnich."),

    lc_h2("ch3-wzor", "Wzór"),

    lc_p("Punktem wyjścia jest ", gloss("centralne twierdzenie graniczne"), " z wykładu 02.
      Średnia z próby \\(\\bar{x}\\) leży w około 95% prób nie dalej niż
      1.96 błędu standardowego \\(\\sigma/\\sqrt{n}\\) od średniej ", gloss("populacja", "populacji"), " μ.
      Skoro μ rzadko leży dalej od \\(\\bar{x}\\) niż 1.96 błędu standardowego,
      możemy odwrócić to zdanie: zakres \\(\\bar{x} \\pm 1.96 \\cdot \\sigma/\\sqrt{n}\\)
      obejmuje μ w 95% prób. Przeszkoda jest jedna: σ populacji zwykle nie znamy."),

    lc_p("Zastępujemy je więc ", gloss("odchylenie standardowe", "odchyleniem
      standardowym"), " z próby \\(s\\). Wynik \\(s/\\sqrt{n}\\) to ",
      gloss("błąd standardowy"), " średniej (SE) oszacowany z danych. Ponieważ
      \\(s\\) też zmienia się z próby na próbę, do niepewności średniej
      dochodzi niepewność samego SE. Z tego powodu mnożnik 1.96 z ", gloss("rozkład normalny", "rozkładu
      normalnego"), " zastępujemy ", gloss("kwantyl", "kwantylem"), " ", gloss("rozkład t-Studenta",
      "rozkładu t-Studenta"), " z \\(n - 1\\) ", gloss("stopnie swobody",
      "stopniami swobody"), ", który poznaliśmy w wykładzie 02. Tak powstaje ",
      gloss("przedział ufności"), " dla średniej populacji:"),

    lc_formula_box(
      withMathJax("$$CI = \\bar{x} \\pm t^*_{\\alpha/2,\\, n-1} \\cdot \\frac{s}{\\sqrt{n}}$$")
    ),

    lc_p("Środkiem przedziału jest średnia z próby \\(\\bar{x}\\). Jego połowę
      szerokości, czyli ", gloss("margines błędu"), " \\(ME = t^* \\cdot SE\\),
      wyznaczają dwa czynniki. Błąd standardowy mówi, jak bardzo średnia
      waha się z próby na próbę. ", gloss("wartość krytyczna", "Wartość krytyczna"),
      " \\(t^*\\) mówi, ile błędów standardowych trzeba odłożyć w każdą stronę,
      żeby osiągnąć wybrany ", gloss("poziom ufności"), ". Dla 95% jest to
      kwantyl 0.975 rozkładu t."),

    lc_p("Rozkład t ma cięższe ogony niż rozkład normalny, więc jego kwantyle
      są większe, a przedział szerszy. Różnica szybko maleje wraz z liczebnością
      próby. W wykładzie 02 kwantyl 0.975 wynosił 3.18 dla df = 3 i 2.04 dla
      df = 30. Dla próby 25 osób (df = 24) jest to 2.06, a przy bardzo dużych
      próbach wartość zbliża się do 1.96 z rozkładu normalnego."),

    lc_p("Przy grupce 5 osób różnica między 1.96 a t* decyduje o tym, jak często siatka łapie μ."),

    # PROTOTYP SCENY (2026-10-08): Za mała siatka — 1.96 zamiast t* przy n = 5
    figure_panel(
      label = "Prototyp sceny",
      width_mode = "text",
      scene_widget("ch3_siatka", "Za mała siatka: 1.96 czy t*",
        steps = c("Powtarzamy", "Pokrycie"),
        labels = c("Zarzuć siatkę", "Zarzuć siatkę"),
        more_from = 1,
        options = list(
          list(name = "mult", label = "Mnożnik",
               values = c("1.96" = "z", "t*" = "t"), selected = "z", from = 1)
        ),
        config = list(kind = "net", mode = "mult", mu = net_world$mu, sigma = net_world$sigma,
                      n = 5L, mult = "z", tq = net_tq, z = 1.96,
                      xmin = 140, xmax = 200, height = 550,
                      aria = "Grupki 5 osób zarzucają siatki x̄ ± mnożnik · s/√n na oś wzrostu; siatki układają się pod osią, a w drugim kroku widać μ i odsetek trafień przy mnożniku 1.96 albo t*"))
    ),

    lc_p("Programy statystyczne liczą przedział dla średniej zawsze z rozkładu t,
      więc nie trzeba wybierać między wersją z a t. Średnią i granice
      przedziału program poda za nas, ale żeby przedział dobrze odczytać,
      warto raz zobaczyć, jak powstaje."),

    lc_h2("ch3-budowa", "Budowa przedziału — krok po kroku"),

    lc_p("Panel składa przedział z próby 25 pomiarów wzrostu losowanej
      z populacji o średniej μ = 170 cm i odchyleniu standardowym σ = 10 cm.
      Kolejne kroki dodają do wykresu średnią, pasek ±1 SE i pełny 95% przedział.
      Przycisk „Nowa próba” losuje kolejne 25 osób."),

    figure_panel(
      label = "Ryc. 3.1",
      full_width = TRUE,
      lc_step_widget("ch3_step",
        title = "Konstruowanie przedziału",
        steps = c("Próba", "Średnia", "± SE", "Przedział"),
        toolbar = lc_toolbar(
          lc_action("ch3_step_new_sample", "Nowa próba", icon = "shuffle",
                    variant = "outline")
        ),
        plot_id = "ch3_step_plot"
      )
    ),

    lc_p("Pojedyncze pomiary rozrzucone są szeroko, zwykle o kilka do
      kilkunastu centymetrów od 170 cm. Średnia z 25 pomiarów jest dużo
      stabilniejsza: przy σ = 10 cm jej błąd standardowy wynosi około
      \\(10/\\sqrt{25} = 2\\) cm, pięć razy mniej niż rozrzut pojedynczych
      osób. Uśrednianie znosi większość przypadkowych odchyleń w górę i w dół."),

    lc_p("Pasek ±1 SE obejmuje μ tylko w około dwóch trzecich prób. Żeby
      osiągnąć 95%, mnożymy SE przez \\(t^*\\) = 2.06 (df = 24). Przy SE
      równym około 2 cm margines błędu wynosi około 4.1 cm, a cały przedział
      ma około 8 cm szerokości. Większa część tego poszerzenia wynika
      z wyboru poziomu ufności: dla 95% potrzeba około dwóch błędów
      standardowych, a nie jednego. Za oszacowanie σ z próby płacimy tylko
      różnicą między 1.96 a 2.06. Każda nowa próba daje inną średnią, inne
      \\(s\\) i inny przedział. Tak jak w scenie z siatkami z poprzedniego rozdziału,
      mniej więcej co dwudziesty z nich ominie μ = 170 cm."),

    lc_h2("ch3-roznica", "Budowa przedziału dla różnicy średnich"),

    lc_p("Pojedyncza średnia rzadko jest celem badania. Częściej porównujemy
      dwie grupy: mężczyzn i kobiety, lek i placebo, dwóch dostawców.
      Interesuje nas wtedy różnica średnich populacji \\(\\mu_1 - \\mu_2\\),
      a jej estymatą jest różnica średnich z prób \\(\\bar{x}_1 - \\bar{x}_2\\).
      Obie średnie mają własną niepewność. Dla niezależnych prób
      ", gloss("wariancja", "wariancje"), " ", gloss("estymator", "estymatorów"), " się dodają, więc błąd standardowy różnicy to
      pierwiastek z sumy kwadratów błędów standardowych obu średnich:"),

    lc_formula_box(
      withMathJax("$$CI = (\\bar{x}_1 - \\bar{x}_2) \\pm t^* \\cdot \\sqrt{\\frac{s_1^2}{n_1} + \\frac{s_2^2}{n_2}}$$")
    ),

    lc_p("Każda grupa ma tu własne odchylenie standardowe, nie zakładamy
      równych wariancji. To wersja Welcha. Liczbę stopni swobody dla \\(t^*\\)
      wyznacza wtedy osobny wzór (Welcha–Satterthwaite'a). Wynik zwykle
      nie jest liczbą całkowitą i leży między \\(\\min(n_1, n_2) - 1\\)
      a \\(n_1 + n_2 - 2\\). Panele w tym wykładzie liczą wersję Welcha.
      Klasyczna wersja Studenta, w wielu programach domyślna, zakłada równe
      wariancje i łączy je w jedną. Przy równych licznościach obie wersje
      mają ten sam błąd standardowy, a różnią się tylko liczbą stopni swobody."),

    lc_p("Panel porównuje wzrost 25 mężczyzn i 25 kobiet. Próby losowane są
      z populacji o średnich 178 cm (σ = 7 cm) i 165 cm (σ = 6 cm), więc
      prawdziwa różnica wynosi 13 cm. Od trzeciego kroku dolny panel przechodzi
      na skalę różnicy, na której zero oznacza brak różnicy."),

    figure_panel(
      label = "Ryc. 3.2",
      full_width = TRUE,
      lc_step_widget("ch3_dstep",
        title = "Konstruowanie CI dla różnicy",
        steps = c("Dwie próby", "Dwie średnie", "Różnica", "± SE", "Przedział"),
        toolbar = lc_toolbar(
          lc_action("ch3_dstep_new_sample", "Nowe próby", icon = "shuffle",
                    variant = "outline")
        ),
        plot_id = "ch3_dstep_plot",
        ratio = "2/1"
      )
    ),

    lc_p("Przy tych parametrach błąd standardowy różnicy wynosi około
      \\(\\sqrt{7^2/25 + 6^2/25} \\approx 1.84\\) cm, liczba stopni swobody
      Welcha około 47, a \\(t^* \\approx 2.01\\). Margines błędu to około
      3.7 cm, więc przedział leży typowo w okolicach 9–17 cm, daleko od zera.
      Przy tak dużej różnicy wniosek, że mężczyźni są średnio wyżsi, nie
      zależy od tego, którą próbę wylosujemy."),

    lc_p("Warto zauważyć jedną rzecz. Błędy standardowe obu średnich to
      około 1.4 cm i 1.2 cm, razem 2.6 cm. Błąd standardowy różnicy jest
      mniejszy, bo dodają się wariancje, a nie błędy standardowe:
      \\(\\sqrt{1.4^2 + 1.2^2} \\approx 1.84\\). Działa to jak
      w trójkącie prostokątnym: przeciwprostokątna jest krótsza niż suma
      przyprostokątnych. Ta własność ma praktyczne
      skutki przy porównywaniu przedziałów dwóch grup na wykresie."),

    lc_h2("ch3-scenariusze", "Dwa CI grup czy CI różnicy? — trzy scenariusze"),

    lc_p("Wyniki dla dwóch grup można odczytać na dwa sposoby. Pierwszy to
      narysować przedział ufności dla każdej grupy osobno i sprawdzić, czy
      się nakrywają. Drugi to policzyć przedział dla różnicy
      \\(\\mu_1 - \\mu_2\\) i sprawdzić, czy zawiera zero. Zwykle oba
      sposoby prowadzą do tego samego wniosku, ale nie zawsze. Trzy przykłady
      z technologii żywności pokazują oba spojrzenia na tych samych danych.
      Górny wykres to przedziały grup z zaznaczonym na żółto odcinkiem, na
      którym się nakrywają, dolny to przedział różnicy (wersja Welcha)."),

    # --- Scenariusz A ---
    figure_panel(
      label = "Przykład A",
      title = "Dwaj dostawcy mąki — zgodne sygnały, różnica istotna",
      p("Zakład piekarniczy porównuje dwóch dostawców mąki pszennej typu 550
        pod względem zawartości białka (%). Pobrano po 40 partii od każdego dostawcy."),
      lc_plot("ch3_comp_A_plot", ratio = "1.8/1", max_height = "340px"),
      uiOutput("ch3_comp_A_verdict")
    ),

    # --- Scenariusz B ---
    figure_panel(
      label = "Przykład B",
      title = "Jogurt w szkle i w plastiku — zgodne sygnały, brak różnicy",
      p("Technolog sprawdza, czy materiał opakowania wpływa na zawartość tłuszczu (%)
        w jogurcie naturalnym po 7 dniach przechowywania. Po 30 próbek z każdego typu."),
      lc_plot("ch3_comp_B_plot", ratio = "1.8/1", max_height = "340px"),
      uiOutput("ch3_comp_B_verdict")
    ),

    # --- Scenariusz C (pułapka) ---
    figure_panel(
      label = "Przykład C",
      title = "Dwie linie płatków — pułapka wzrokowa",
      p("Zakład sprawdza, czy dwie linie produkcyjne płatków śniadaniowych
        dają produkt o tej samej zawartości błonnika (g / 100 g).
        Po 120 partii z każdej linii."),
      lc_plot("ch3_comp_C_plot", ratio = "1.8/1", max_height = "340px"),
      uiOutput("ch3_comp_C_verdict")
    ),

    lc_p("W scenariuszu A oba spojrzenia się zgadzają. Przedziały dostawców,
      [11.76, 12.16] i [10.80, 11.15], są rozłączne, a przedział różnicy
      [0.72, 1.25] leży w całości powyżej zera. Mąka dostawcy A zawiera
      średnio o 0.7–1.3 punktu procentowego więcej białka."),

    lc_p("W scenariuszu B też nie ma sprzeczności, choć wynik jest bliski
      granicy. Przedziały dla szkła [3.03, 3.17] i plastiku [3.11, 3.28]
      nakładają się, a przedział różnicy [-0.21, 0.01] obejmuje zero, choć
      jego górna granica leży tuż nad nim. Dane nie dają podstaw, by twierdzić,
      że opakowanie wpływa na zawartość tłuszczu. Nie dowodzą jednak, że
      wpływu nie ma: zgodne z nimi są zarówno brak różnicy, jak i różnica
      około 0.2 punktu procentowego na korzyść plastiku."),

    lc_p("Scenariusz C to pułapka. Przedziały linii, [9.24, 9.61] i [8.91, 9.25],
      nakładają się na odcinku zaledwie 0.01 g, ale się nakładają. Patrząc na nie,
      łatwo uznać, że linie produkują podobne płatki. Tymczasem przedział
      różnicy [0.09, 0.59] nie zawiera zera: linia 1 daje płatki bogatsze
      w błonnik. Źródłem rozbieżności jest własność, którą widzieliśmy przy
      wzroście. Błąd standardowy różnicy to"),

    lc_formula_box(
      withMathJax("$$SE_{różnicy} = \\sqrt{SE_1^2 + SE_2^2} \\;<\\; SE_1 + SE_2$$")
    ),

    lc_p("W scenariuszu C błędy standardowe grup wynoszą 0.093 i 0.086 g.
      Ich suma to 0.179, a pierwiastek z sumy kwadratów tylko 0.127. Dlatego
      połowa szerokości przedziału różnicy (0.25 g) jest wyraźnie mniejsza
      niż suma połówek przedziałów obu grup (0.36 g). Przedziały grup mogą
      się więc lekko nakładać, mimo że różnica jest istotna. Rozłączne
      przedziały grup są mocnym sygnałem różnicy, ale nakładające się nie
      rozstrzygają niczego."),

    lc_note("Zasada", rule = TRUE,
      "Porównując dwie grupy, patrz na przedział różnicy. Nakładanie się
       przedziałów grup nie dowodzi, że różnicy nie ma."
    ),

    lc_h2("ch3-case-studies", "Case studies — jak interpretować CI w praktyce"),

    lc_p("Na koniec kilka realistycznych sytuacji do samodzielnej analizy.
      W każdej przedział buduje się w tych samych krokach co wyżej. Po
      ostatnim kroku można wybrać hipotezę: na wykresie pojawia się
      zacieniowany obszar wartości, które ją spełniają. Jeśli cały przedział
      leży w tym obszarze, dane potwierdzają hipotezę. Jeśli cały leży poza
      nim, dane ją wykluczają. Jeśli przedział przecina granicę, dane nie
      rozstrzygają. Przed kliknięciem „Pokaż werdykt” warto odpowiedzieć samodzielnie.
      Przypadki A dotyczą jednej średniej, B różnicy dwóch średnich, a C
      wielu grup naraz."),

    lc_h3("Przedział dla jednej średniej", num = "A"),

    figure_panel(
      label = "Przykład A1",
      title = "Wzrost studentów — czytanie pojedynczego CI",
      p("Zmierzono wzrost 30 studentów. Średnia z próby wynosi ",
        withMathJax("\\(\\bar{x} = 173.4\\)"), " cm, odchylenie standardowe ",
        withMathJax("\\(s = 8.2\\)"), " cm. Zbudujmy przedział dla średniego
        wzrostu i sprawdźmy dwie hipotezy."),
      uiOutput("ch3_caseA1_widget")
    ),

    figure_panel(
      label = "Przykład A2",
      title = "Ten sam pomiar, trzy różne wielkości próby",
      p("Trzy badania mierzą stężenie zanieczyszczenia (µg/m³). Wszystkie
        dały średnią 32.0 i s = 8.0, ale różnią się liczebnością próby:
        n = 10, 50 i 200. Kolejne kroki dodają przedziały jeden po drugim."),
      uiOutput("ch3_caseA2_widget")
    ),

    lc_h3("Przedział dla różnicy średnich", num = "B"),

    figure_panel(
      label = "Przykład B1",
      title = "Test leku na ciśnienie — CI dla różnicy nie obejmuje 0",
      p("Badamy nowy lek na obniżenie ciśnienia krwi. ",
        tags$b("Lek:"), " n = 40, średnie obniżenie 12.3 mmHg, s = 4.5. ",
        tags$b("Placebo:"), " n = 40, średnie obniżenie 4.1 mmHg, s = 4.2."),
      uiOutput("ch3_caseB1_widget")
    ),

    figure_panel(
      label = "Przykład B2",
      title = "Dwa nawozy — CI dla różnicy obejmuje 0",
      p("Porównano plon kukurydzy dla dwóch nawozów. ",
        tags$b("Nawóz X:"), " n = 25, średnia 8.4 t/ha, s = 1.2. ",
        tags$b("Nawóz Y:"), " n = 25, średnia 8.1 t/ha, s = 1.3."),
      uiOutput("ch3_caseB2_widget")
    ),

    figure_panel(
      label = "Przykład B3",
      title = "Pułapka nakładających się CI",
      p("Zmierzono czas reakcji w dwóch grupach po 150 osób. ",
        tags$b("Grupa A:"), " średnia 350 ms, s = 45. ",
        tags$b("Grupa B:"), " średnia 362 ms, s = 45. Przedziały
        obu grup nakładają się. Czy różnica jest istotna?"),
      uiOutput("ch3_caseB3_widget")
    ),

    figure_panel(
      label = "Przykład B4",
      title = "Istotne statystycznie ≠ ważne praktycznie",
      p("Bardzo duże badanie porównuje IQ w dwóch województwach. ",
        tags$b("Wojew. A:"), " n = 20 000, średnia 100.4, s = 15. ",
        tags$b("Wojew. B:"), " n = 20 000, średnia 100.0, s = 15.
        Różnica 0.4 pkt IQ — dużo czy mało?"),
      uiOutput("ch3_caseB4_widget")
    ),

    lc_h3("Wiele grup — forest plot", num = "C"),

    figure_panel(
      label = "Przykład C1",
      title = "Cztery metody nauczania — czy któraś wystaje?",
      p("Porównano średni wynik egzaminu (0–40 pkt) studentów uczących
        się czterema metodami, po 25 osób w każdej grupie. Kolejne kroki
        dodają punkty, średnie i przedziały."),
      uiOutput("ch3_caseC1_widget")
    ),

    figure_panel(
      label = "Przykład C2",
      title = "Pięć oddziałów szpitalnych — czas oczekiwania",
      p("Zmierzono średni czas oczekiwania na konsultację (minuty) w pięciu
        oddziałach szpitala. Który wymaga interwencji?"),
      uiOutput("ch3_caseC2_widget")
    ),

    lc_p("Przypadki powtarzają wnioski z całego rozdziału. W A2 przedział
      zwęża się wraz z liczebnością próby: ma około 11.4 µg/m³ szerokości
      przy n = 10, 4.5 przy n = 50 i 2.2 przy n = 200. Czterokrotnie większa
      próba daje mniej więcej dwukrotnie węższy przedział, bo błąd
      standardowy maleje jak \\(1/\\sqrt{n}\\). B3 to pułapka ze scenariusza C:
      przedziały grup się nakładają, a przedział różnicy [-22.2, -1.8] ms
      leży w całości poniżej zera. B4 pokazuje drugą stronę dużych prób.
      Przy 20 000 osób w grupie przedział różnicy [0.11, 0.69] pkt nie
      zawiera zera, ale różnica 0.4 punktu to około 0.03 odchylenia
      standardowego IQ. Wynik jest istotny statystycznie, ale nie ma ",
      gloss("istotność praktyczna", "istotności praktycznej"), "."),

    lc_p("Przy wielu grupach przedziały zestawia się na jednej osi w ",
      gloss("forest plot", "forest plocie"), ". Taki wykres to szybka mapa:
      rozłączne przedziały wskazują wyraźne różnice, a nakładające się
      nie rozstrzygają. W C1 rozłączne przedziały mają tylko metoda tradycyjna
      i tutoring. W C2 SOR (średnio 75 min, przedział [69.4, 80.6]) odstaje od
      wszystkich pozostałych oddziałów, a wśród pozostałych rozłączne
      przedziały mają jeszcze trzy pary. Żeby rozstrzygnąć konkretną parę grup,
      liczymy przedział dla różnicy ich średnich."),

    lc_p("Ta sama konstrukcja, estymata ± wartość krytyczna × błąd standardowy,
      działa także dla odsetków. Przy proporcjach błąd standardowy zależy jednak
      od samego szacowanego odsetka, co przy małych próbach sprawia kłopoty.
      Tym zajmiemy się w następnym rozdziale."),

    lc_chapter_next(
      num       = "04",
      title     = "Przedział dla proporcji",
      lead      = "CI dla odsetków: Wald, Wilson, Clopper-Pearson",
      target_id = "ch-proporcja"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  # --- PROTOTYP SCENY (2026-10-08): Za mała siatka ---
  scene_texts(input, output, "ch3_siatka", list(
    tagList("Grupki po 5 osób. Dorzuć +100 i +1000 siatek z mnożnikiem 1.96."),
    tagList("Przerywana linia to ", tags$code("μ", .noWS = "outside"), ". Przy 1.96 pokrycie wynosi około ",
      paste0(round(100 * net_cov_z[["5"]]), "%. Przełącz mnożnik na "),
      tags$code("t*", .noWS = "outside"), " = ", lc_fmt(net_tq[["5"]], 2), " i dorzuć +1000.")
  ))

  # --- Widget 1: Budowa przedziału krok po kroku ---
  # Krok widgetu (1..4) żyje w przeglądarce; nowa próba nie zmienia kroku.
  ch3_step <- lc_step_server("ch3_step", input)$step

  generate_step_sample <- function() {
    set.seed(sample.int(.Machine$integer.max, 1))
    generate_population_sample("normal", 25)
  }
  ch3_step_sample <- reactiveVal(generate_step_sample())

  observeEvent(input$ch3_step_new_sample, {
    ch3_step_sample(generate_step_sample())
  })

  # Grubość paska przedziału: element wprowadzany w kroku grubszy niż znany.
  ch3_bar_lw <- function(role) if (role == "new") 1.8 else 1.1

  # Etykieta w kolorze roli, krojem wykresu.
  ch3_role_text <- function(x, y, label, role, size, hjust = 0.5) {
    annotate("text", x = x, y = y, label = label, hjust = hjust,
             colour = STEP_ROLES[[role]]$colour, fontface = "bold", size = size)
  }

  zoom_plot_server("ch3_step_plot", reactive({
    step <- ch3_step()
    samp <- ch3_step_sample()

    xbar <- mean(samp)
    s <- sd(samp)
    n <- length(samp)
    se <- s / sqrt(n)
    t_star <- qt(0.975, df = n - 1)
    me <- t_star * se

    # Stała oś X dla wszystkich kroków (oparta na surowych danych + CI)
    xlims <- range(c(samp, xbar - 1.2 * me, xbar + 1.2 * me))
    pad <- diff(xlims) * 0.05
    xlims <- c(xlims[1] - pad, xlims[2] + pad)

    # Jitter punktów na Y (deterministyczny na podstawie wartości)
    set.seed(42)
    jitter_y <- runif(n, min = 0.55, max = 0.90)
    samp_df <- data.frame(x = samp, y = jitter_y)

    # Oddzielne poziomy Y - każdy element na swojej linii
    Y_MEAN <- 0.38
    Y_SE   <- 0.12
    Y_CI   <- -0.18

    # Krok 1+: surowe punkty z próby
    p <- ggplot() +
      step_layer(geom_point, "data", data = samp_df,
                 mapping = aes(x = x, y = y), size = 3) +
      labs(x = "Wzrost (cm)", y = NULL) +
      step_frame(xlim = xlims, ylim = c(-0.45, 0.98), y_axis = FALSE)

    # Krok 2+: pionowa linia prowadząca + diament średniej
    if (step >= 2) {
      role <- step_role(step, 2)
      p <- p +
        step_line("known", xintercept = xbar) +
        step_layer(geom_point, role, data = data.frame(x = xbar, y = Y_MEAN),
                   mapping = aes(x = x, y = y), size = 7, shape = 18) +
        ch3_role_text(xbar, Y_MEAN - 0.13, "x̄", role = role,
                   size = 5)
    }

    # Krok 3+: przedział +/- SE (węższy)
    if (step >= 3) {
      role <- step_role(step, 3)
      p <- p +
        step_layer(geom_errorbar, role,
                   data = data.frame(xmin = xbar - se, xmax = xbar + se, y = Y_SE),
                   mapping = aes(xmin = xmin, xmax = xmax, y = y),
                   width = 0.07, linewidth = ch3_bar_lw(role)) +
        ch3_role_text(xbar, Y_SE - 0.10, "± SE", role = role,
                   size = 4.5)
    }

    # Krok 4: pełny CI (t* * SE, szerszy)
    if (step >= 4) {
      p <- p +
        step_layer(geom_errorbar, "new",
                   data = data.frame(xmin = xbar - me, xmax = xbar + me, y = Y_CI),
                   mapping = aes(xmin = xmin, xmax = xmax, y = y),
                   width = 0.10, linewidth = 2.2) +
        ch3_role_text(xbar, Y_CI - 0.13, "95% CI", role = "new",
                   size = 5)
    }

    p
  }))

  output$ch3_step_text <- renderUI({
    step <- ch3_step()
    samp <- ch3_step_sample()

    xbar <- mean(samp)
    s <- sd(samp)
    n <- length(samp)
    se <- s / sqrt(n)
    t_star <- qt(0.975, df = n - 1)
    me <- t_star * se

    switch(as.character(step),
      "1" = withMathJax(paste0(
        n, " pomiarów wzrostu, każda kropka to jedna osoba. ",
        "\\(\\bar{x} = ", round(xbar, 2), "\\) cm, \\(s = ", round(s, 2), "\\) cm.")),
      "2" = withMathJax(paste0(
        "Średnia z próby \\(\\bar{x} = ", round(xbar, 2),
        "\\) cm to estymata punktowa μ. Inna próba dałaby inną wartość.")),
      "3" = withMathJax(paste0(
        "\\(SE = s/\\sqrt{n} = ", round(s, 2), "/\\sqrt{", n, "} = ",
        round(se, 2), "\\) cm. Pasek \\(\\bar{x} \\pm SE\\) to tylko około 67% ufności.")),
      "4" = {
        covers <- (xbar - me <= 170) & (170 <= xbar + me)
        withMathJax(paste0(
          "\\(t^* = ", round(t_star, 3), "\\) (df = ", n - 1, "), ",
          "\\(ME = ", round(t_star, 3), " \\cdot ", round(se, 2), " = ",
          round(me, 2), "\\) cm. 95% CI: [", round(xbar - me, 2), " ; ",
          round(xbar + me, 2), "]. ",
          if (covers) "Ten przedział zawiera μ = 170 cm."
          else "Ten przedział nie zawiera μ = 170 cm."))
      }
    )
  })

  # --- Widget 2: Budowa przedziału dla różnicy średnich ---
  # Krok widgetu (1..5) żyje w przeglądarce; nowe próby nie zmieniają kroku.
  ch3_dstep <- lc_step_server("ch3_dstep", input)$step

  generate_diff_samples <- function() {
    list(
      men   = rnorm(25, mean = 178, sd = 7),
      women = rnorm(25, mean = 165, sd = 6)
    )
  }
  ch3_dstep_samples <- reactiveVal(generate_diff_samples())

  observeEvent(input$ch3_dstep_new_sample, {
    ch3_dstep_samples(generate_diff_samples())
  })

  zoom_plot_server("ch3_dstep_plot", reactive({
    step <- ch3_dstep()
    samples <- ch3_dstep_samples()

    men <- samples$men
    women <- samples$women
    n1 <- length(men); n2 <- length(women)
    x1 <- mean(men); x2 <- mean(women)
    s1 <- sd(men);   s2 <- sd(women)
    diff_val <- x1 - x2
    se <- sqrt(s1^2 / n1 + s2^2 / n2)
    df_w <- (s1^2 / n1 + s2^2 / n2)^2 /
            ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
    t_star <- qt(0.975, df = df_w)
    me <- t_star * se

    # Mężczyźni: dane (niebo), kobiety: druga grupa (bursztyn)
    col_men <- STEP_ROLES$data$colour
    col_women <- STEP_ROLES$group$colour

    # ---- GÓRNY PANEL: dwie grupy na skali wzrostu ----
    xlims_top <- range(c(men, women))
    pad_top <- diff(xlims_top) * 0.06
    xlims_top <- c(xlims_top[1] - pad_top, xlims_top[2] + pad_top)

    set.seed(42)
    jitter_men <- runif(n1, min = 1.55, max = 2.05)
    jitter_women <- runif(n2, min = 0.75, max = 1.25)
    men_df <- data.frame(x = men, y = jitter_men)
    women_df <- data.frame(x = women, y = jitter_women)

    # Etykiety grup po lewej; krok 1+: punkty
    p_top <- ggplot() +
      annotate("text", x = xlims_top[1], y = 1.8, label = "Mężczyźni",
               hjust = 0, fontface = "bold", size = 4.5, color = col_men) +
      annotate("text", x = xlims_top[1], y = 1.0, label = "Kobiety",
               hjust = 0, fontface = "bold", size = 4.5, color = col_women) +
      step_layer(geom_point, "data", data = men_df,
                 mapping = aes(x = x, y = y), size = 3) +
      step_layer(geom_point, "group", data = women_df,
                 mapping = aes(x = x, y = y), size = 3) +
      labs(x = "Wzrost (cm)", y = NULL) +
      step_frame(xlim = xlims_top, ylim = c(0.35, 2.25), y_axis = FALSE)

    # Krok 2+: średnie (diamenty + linie)
    if (step >= 2) {
      role <- step_role(step, 2)
      means_df <- data.frame(x = c(x1, x2), y = c(1.8, 1.0))
      p_top <- p_top +
        step_layer(geom_segment, role,
                   data = data.frame(x = c(x1, x2), y = 0.4, yend = 2.1),
                   mapping = aes(x = x, xend = x, y = y, yend = yend),
                   linetype = "22") +
        step_layer(geom_point, role, data = means_df,
                   mapping = aes(x = x, y = y), size = 7, shape = 18) +
        ch3_role_text(x1, 2.15, paste0("x̄₁ = ", round(x1, 2)), role = role,
                   size = 4.5) +
        ch3_role_text(x2, 0.55, paste0("x̄₂ = ", round(x2, 2)), role = role,
                   size = 4.5)
    }

    library(patchwork)

    # Kroki 1-2: tylko górny panel; miejsce dolnego zostaje puste (stała rama)
    if (step < 3) {
      return((p_top / plot_spacer()) + plot_layout(heights = c(2, 1)))
    }

    # ---- DOLNY PANEL: różnica w skali wycentrowanej na 0 ----
    # Limity: obejmij 0 i CI z marginesem
    xlims_bot <- range(c(0, diff_val - 1.3 * me, diff_val + 1.3 * me))
    pad_bot <- diff(xlims_bot) * 0.08
    xlims_bot <- c(xlims_bot[1] - pad_bot, xlims_bot[2] + pad_bot)

    # Krok 3+: punkt różnicy
    role_diff <- step_role(step, 3)
    p_bot <- ggplot() +
      step_line("known", xintercept = 0) +
      ch3_role_text(0, 0.45, "0 = brak różnicy", role = "known",
                 hjust = -0.1, size = 4) +
      step_layer(geom_point, role_diff, data = data.frame(x = diff_val, y = 0),
                 mapping = aes(x = x, y = y), size = 7, shape = 18) +
      ch3_role_text(diff_val, -0.22, paste0("x̄₁ − x̄₂ = ", round(diff_val, 2)),
                 role = role_diff, size = 4.5) +
      labs(x = "Różnica średnich (cm)  —  Mężczyźni − Kobiety",
           y = NULL) +
      step_frame(xlim = xlims_bot, ylim = c(-0.55, 0.55), y_axis = FALSE)

    # Krok 4+: wąski przedział SE
    if (step >= 4) {
      role <- step_role(step, 4)
      p_bot <- p_bot +
        step_layer(geom_errorbar, role,
                   data = data.frame(xmin = diff_val - se, xmax = diff_val + se, y = 0),
                   mapping = aes(xmin = xmin, xmax = xmax, y = y),
                   width = 0.08, linewidth = ch3_bar_lw(role)) +
        ch3_role_text(diff_val, 0.17, paste0("± SE = ±", round(se, 2)), role = role,
                   size = 4)
    }

    # Krok 5: pełen CI
    if (step >= 5) {
      p_bot <- p_bot +
        step_layer(geom_errorbar, "new",
                   data = data.frame(xmin = diff_val - me, xmax = diff_val + me, y = 0),
                   mapping = aes(xmin = xmin, xmax = xmax, y = y),
                   width = 0.14, linewidth = 2.2, alpha = 0.6) +
        ch3_role_text(diff_val, -0.42,
                   paste0("95% CI: [", round(diff_val - me, 2),
                          " ; ", round(diff_val + me, 2), "]"),
                   role = "new", size = 4.8)
    }

    # Połącz patchworkiem
    (p_top / p_bot) + plot_layout(heights = c(2, 1))
  }))

  output$ch3_dstep_text <- renderUI({
    step <- ch3_dstep()
    samples <- ch3_dstep_samples()

    men <- samples$men
    women <- samples$women
    n1 <- length(men); n2 <- length(women)
    x1 <- mean(men); x2 <- mean(women)
    s1 <- sd(men);   s2 <- sd(women)
    diff_val <- x1 - x2
    se <- sqrt(s1^2 / n1 + s2^2 / n2)
    df_w <- (s1^2 / n1 + s2^2 / n2)^2 /
            ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
    t_star <- qt(0.975, df = df_w)
    me <- t_star * se

    switch(as.character(step),
      "1" = paste0(n1, " mężczyzn (niebieskie punkty) i ", n2,
                   " kobiet (bursztynowe). Każdy punkt to jedna osoba."),
      "2" = withMathJax(paste0(
        "\\(\\bar{x}_1 = ", round(x1, 2), "\\) cm, \\(\\bar{x}_2 = ",
        round(x2, 2), "\\) cm. Każda średnia ma własną niepewność.")),
      "3" = withMathJax(paste0(
        "\\(\\bar{x}_1 - \\bar{x}_2 = ", round(diff_val, 2),
        "\\) cm. Dolny panel to skala różnicy: linia 0 oznacza brak różnicy.")),
      "4" = withMathJax(paste0(
        "\\(SE = \\sqrt{", round(s1, 2), "^2/", n1, " + ", round(s2, 2), "^2/", n2,
        "} = ", round(se, 2), "\\) cm, mniej niż suma SE obu średnich (",
        round(s1 / sqrt(n1) + s2 / sqrt(n2), 2), " cm).")),
      "5" = {
        covers_zero <- (diff_val - me <= 0) & (0 <= diff_val + me)
        withMathJax(paste0(
          "df Welcha ≈ ", round(df_w, 1), ", \\(t^* = ", round(t_star, 3),
          "\\), \\(ME = ", round(t_star, 3), " \\cdot ", round(se, 2), " = ",
          round(me, 2), "\\) cm. 95% CI: [", round(diff_val - me, 2), " ; ",
          round(diff_val + me, 2), "] cm. ",
          if (covers_zero) "Przedział obejmuje 0."
          else paste0("Przedział nie obejmuje 0: mężczyźni są średnio wyżsi o co najmniej ",
                      round(diff_val - me, 1), " cm.")))
      }
    )
  })

  # ============================================================================
  # WIDGET 3: CASE STUDIES (konstruktory krok po kroku + hipotezy)
  # ============================================================================

  # ---- Helpery statystyczne ----
  ci_mean <- function(xbar, s, n, conf = 0.95) {
    t_star <- qt(1 - (1 - conf) / 2, df = n - 1)
    me <- t_star * s / sqrt(n)
    list(lower = xbar - me, upper = xbar + me, me = me,
         t_star = t_star, se = s / sqrt(n))
  }
  ci_diff_means <- function(x1, s1, n1, x2, s2, n2, conf = 0.95) {
    se <- sqrt(s1^2 / n1 + s2^2 / n2)
    df_w <- (s1^2 / n1 + s2^2 / n2)^2 /
            ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
    t_star <- qt(1 - (1 - conf) / 2, df = df_w)
    diff <- x1 - x2
    me <- t_star * se
    list(diff = diff, lower = diff - me, upper = diff + me,
         me = me, df = df_w, se = se, t_star = t_star)
  }

  # ---- Werdykt hipotezy ----
  # dir = "gt" (CI > bound), "lt" (CI < bound)
  # Zwraca: "yes" / "no" / "maybe"
  hypothesis_verdict <- function(lower, upper, bound, dir) {
    if (dir == "gt") {
      if (lower > bound)      "yes"
      else if (upper < bound) "no"
      else                    "maybe"
    } else {  # lt
      if (upper < bound)      "yes"
      else if (lower > bound) "no"
      else                    "maybe"
    }
  }

  verdict_class <- function(v) {
    switch(v, "yes" = "ok", "no" = "danger",
           "maybe" = "warning")
  }
  verdict_label <- function(v) {
    switch(v, "yes" = "TAK", "no" = "NIE", "maybe" = "NIEPEWNE")
  }

  # Kolor obszaru hipotezy (fioletowawy)
  col_hyp <- "#8e44ad"

  # ---- CONFIG case'ow ----
  # Każdy case: type, data, xlab, steps (labele przycisków), hypotheses (lista 2)
  # hypotheses: list of list(text, bound, dir, interval_fn)
  # Dla "single_mean" / "diff_means" / "compare_n" / "forest" interval_fn
  # wyciąga (lower, upper) z konfiguracji.

  cases_config <- list(
    A1 = list(
      type = "single_mean",
      data = list(xbar = 173.4, s = 8.2, n = 30),
      xlab = "Wzrost (cm)",
      steps = c("1. Próba", "2. Średnia", "3. ± SE", "4. Przedział"),
      hypotheses = list(
        list(text = "Średni wzrost przekracza 168 cm",
             bound = 168, dir = "gt",
             explain_yes = "Dolna granica CI (≈ 170.3) leży powyżej 168, cały przedział jest w obszarze hipotezy. Z 95% ufnością średni wzrost w populacji przekracza 168 cm."),
        list(text = "Średni wzrost przekracza 180 cm",
             bound = 180, dir = "gt",
             explain_no = "Górna granica CI (≈ 176.5) leży poniżej 180, cały przedział jest poza obszarem hipotezy. Dane wykluczają średni wzrost powyżej 180 cm.")
      )
    ),
    A2 = list(
      type = "compare_n",
      data = list(xbar = 32.0, s = 8.0, ns = c(10, 50, 200)),
      xlab = "Stężenie (µg/m³)",
      steps = c("1. n = 10", "2. n = 50", "3. n = 200"),
      hypotheses = list(
        list(text = "Stężenie przekracza 25 µg/m³",
             bound = 25, dir = "gt",
             explain_yes = "Wszystkie trzy przedziały leżą powyżej 25, nawet najszerszy (n = 10) ma dolną granicę ≈ 26.3. Każde z badań potwierdza hipotezę, większe n daje tylko węższy przedział."),
        list(text = "Stężenie przekracza 35 µg/m³",
             bound = 35, dir = "gt",
             explain_no = "Werdykt dotyczy najdokładniejszego badania: dla n = 200 górna granica (≈ 33.1) leży poniżej 35. Dla n = 10 przedział sięga do 37.7 i przecina 35, więc samo małe badanie by tego nie rozstrzygnęło.")
      )
    ),
    B1 = list(
      type = "diff_means",
      data = list(x1 = 12.3, s1 = 4.5, n1 = 40, x2 = 4.1, s2 = 4.2, n2 = 40,
                  label1 = "Lek", label2 = "Placebo",
                  unit = "mmHg", diff_label = "Lek − placebo"),
      xlab = "Obniżenie ciśnienia (mmHg)",
      steps = c("1. Próby", "2. Średnie", "3. Różnica", "4. ± SE", "5. Przedział"),
      hypotheses = list(
        list(text = "Lek skuteczniej obniża ciśnienie niż placebo (różnica > 0)",
             bound = 0, dir = "gt",
             explain_yes = "Cały przedział dla różnicy (≈ 6.3–10.1 mmHg) leży powyżej 0: lek obniża ciśnienie skuteczniej niż placebo."),
        list(text = "Lek działa o więcej niż 12 mmHg lepiej niż placebo",
             bound = 12, dir = "gt",
             explain_no = "Górna granica CI (≈ 10.1) leży poniżej 12. Efekt leku jest wyraźny, ale mniejszy, niż głosi hipoteza.")
      )
    ),
    B2 = list(
      type = "diff_means",
      data = list(x1 = 8.4, s1 = 1.2, n1 = 25, x2 = 8.1, s2 = 1.3, n2 = 25,
                  label1 = "Nawóz X", label2 = "Nawóz Y",
                  unit = "t/ha", diff_label = "X − Y"),
      xlab = "Plon (t/ha)",
      steps = c("1. Próby", "2. Średnie", "3. Różnica", "4. ± SE", "5. Przedział"),
      hypotheses = list(
        list(text = "Różnica plonów jest mniejsza niż 2 t/ha",
             bound = 2, dir = "lt",
             explain_yes = "Cały przedział dla różnicy (≈ -0.4–1.0 t/ha) leży poniżej 2 t/ha. Nawet jeśli któryś nawóz jest lepszy, przewaga nie sięga 2 t/ha."),
        list(text = "Nawóz X daje plon większy o ponad 2 t/ha niż Y",
             bound = 2, dir = "gt",
             explain_no = "Górna granica CI (≈ 1.0) leży poniżej 2. Przedział obejmuje też wartości ujemne, więc dane nie mówią nawet, który nawóz jest lepszy.")
      )
    ),
    B3 = list(
      type = "diff_means",
      data = list(x1 = 350, s1 = 45, n1 = 150, x2 = 362, s2 = 45, n2 = 150,
                  label1 = "Grupa A", label2 = "Grupa B",
                  unit = "ms", diff_label = "A − B"),
      xlab = "Czas reakcji (ms)",
      steps = c("1. Próby", "2. Średnie", "3. Różnica", "4. ± SE", "5. Przedział"),
      hypotheses = list(
        list(text = "Grupa A reaguje szybciej niż B (różnica < 0)",
             bound = 0, dir = "lt",
             explain_yes = "Przedziały grup się nakładają, ale przedział dla różnicy (≈ -22.2 do -1.8 ms) leży w całości poniżej 0. Grupa A reaguje szybciej."),
        list(text = "Grupa A jest szybsza o co najmniej 25 ms",
             bound = -25, dir = "lt",
             explain_no = "Dolna granica CI (≈ -22) nie sięga -25, cały przedział leży powyżej tej wartości. Grupa A jest szybsza o około 2–22 ms, nie o 25 ms lub więcej.")
      )
    ),
    B4 = list(
      type = "diff_means",
      data = list(x1 = 100.4, s1 = 15, n1 = 20000, x2 = 100.0, s2 = 15, n2 = 20000,
                  label1 = "Wojew. A", label2 = "Wojew. B",
                  unit = "pkt IQ", diff_label = "A − B"),
      xlab = "IQ (punkty)",
      steps = c("1. Próby", "2. Średnie", "3. Różnica", "4. ± SE", "5. Przedział"),
      hypotheses = list(
        list(text = "Województwo A ma wyższe średnie IQ niż B (różnica > 0)",
             bound = 0, dir = "gt",
             explain_yes = "Przy n = 20 000 w każdej grupie przedział (≈ 0.11–0.69 pkt) jest bardzo wąski i nie obejmuje 0. Różnica jest istotna statystycznie."),
        list(text = "Różnica wynosi co najmniej 1 punkt IQ",
             bound = 1, dir = "gt",
             explain_no = "Cały przedział leży poniżej 1 (górna granica ≈ 0.7). Różnica 0.4 punktu IQ to około 0.03 SD.")
      )
    ),
    C1 = list(
      type = "forest",
      data = list(
        groups = c("Tradycyjna", "E-learning", "Flipped class", "Tutoring"),
        means  = c(28.5, 30.2, 31.8, 33.4),
        sds    = c(5.2, 5.8, 5.5, 4.9),
        ns     = c(25, 25, 25, 25)
      ),
      xlab = "Średni wynik egzaminu (0–40 pkt)",
      steps = c("1. Punkty", "2. Średnie", "3. CI"),
      hypotheses = list(
        list(kind = "pairwise",
             text = "Które metody nauczania różnią się istotnie?",
             unit = "pkt")
      )
    ),
    C2 = list(
      type = "forest",
      data = list(
        groups = c("Kardiologia", "Neurologia", "Ortopedia", "Pulmonologia", "SOR"),
        means  = c(22, 28, 25, 31, 75),
        sds    = c(8, 10, 9, 11, 25),
        ns     = c(60, 55, 70, 50, 80)
      ),
      xlab = "Średni czas oczekiwania (min)",
      steps = c("1. Punkty", "2. Średnie", "3. CI"),
      hypotheses = list(
        list(kind = "pairwise",
             text = "Które oddziały różnią się istotnie czasem oczekiwania?",
             unit = "min")
      )
    )
  )


  # ---- Helper: narysuj pasek CI dla pojedynczej średniej ----
  # step: 0 = nic, 1 = punkty, 2 = +średnia, 3 = +SE, 4 = +CI
  # hypothesis: NULL lub list(bound, dir)
  plot_single_mean_step <- function(data, step, xlab,
                                     hypothesis = NULL, title = NULL) {
    xbar <- data$xbar; s <- data$s; n <- data$n
    ci <- ci_mean(xbar, s, n)
    se <- ci$se; me <- ci$me
    t_star <- ci$t_star

    # Generujemy "fake" punkty z parametrów (reprodukowalnie)
    set.seed(42)
    samp <- rnorm(n, mean = xbar, sd = s)
    samp <- (samp - mean(samp)) / sd(samp) * s + xbar  # wymuś dokładnie xbar, s

    # Limity
    xlims <- range(c(samp, xbar - 1.2 * me, xbar + 1.2 * me))
    if (!is.null(hypothesis)) {
      xlims <- range(c(xlims, hypothesis$bound))
    }
    pad <- diff(xlims) * 0.05
    xlims <- c(xlims[1] - pad, xlims[2] + pad)

    set.seed(7)
    jitter_y <- runif(n, min = 0.15, max = 0.55)
    samp_df <- data.frame(x = samp, y = jitter_y)

    p <- ggplot() +
      coord_cartesian(xlim = xlims, ylim = c(-0.55, 0.75)) +
      labs(x = xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Obszar hipotezy (pod spodem wszystkiego)
    if (!is.null(hypothesis)) {
      if (hypothesis$dir == "gt") {
        p <- p + annotate("rect",
                          xmin = hypothesis$bound, xmax = Inf,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      } else {
        p <- p + annotate("rect",
                          xmin = -Inf, xmax = hypothesis$bound,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      }
      p <- p +
        geom_vline(xintercept = hypothesis$bound, color = col_hyp,
                   linewidth = 1, linetype = "solid") +
        annotate("text", x = hypothesis$bound, y = 0.68,
                 label = paste0(if (hypothesis$dir == "gt") "≥ " else "≤ ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    }

    if (step >= 1) {
      p <- p + geom_point(data = samp_df, aes(x = x, y = y),
                          color = col_ci, size = 3, alpha = 0.7)
    }
    if (step >= 2) {
      p <- p +
        geom_vline(xintercept = xbar, color = col_estimate,
                   linewidth = 1, linetype = "dotted") +
        geom_point(aes(x = xbar, y = 0), color = col_estimate,
                   size = 7, shape = 18) +
        annotate("text", x = xbar, y = -0.18,
                 label = paste0("x̄ = ", round(xbar, 2)),
                 color = col_estimate, fontface = "bold", size = 5)
    }
    if (step >= 3) {
      p <- p +
        geom_errorbar(aes(xmin = xbar - se, xmax = xbar + se, y = 0),
                       width = 0.06, color = col_hit, linewidth = 1.8, orientation = "y") +
        annotate("text", x = xbar, y = 0.14,
                 label = paste0("± SE = ±", round(se, 2)),
                 color = col_hit, fontface = "bold", size = 4)
    }
    if (step >= 4) {
      p <- p +
        geom_errorbar(aes(xmin = xbar - me, xmax = xbar + me, y = 0),
                       width = 0.12, color = col_ci, linewidth = 2.2,
                       alpha = 0.6, orientation = "y") +
        annotate("text", x = xbar, y = -0.38,
                 label = paste0("95% CI: [", round(xbar - me, 2),
                                " ; ", round(xbar + me, 2), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    }

    p
  }

  # ---- Plot dla compare_n ----
  plot_compare_n_step <- function(data, step, xlab, hypothesis = NULL) {
    xbar <- data$xbar; s <- data$s; ns <- data$ns

    # Każdy step = jeden dodatkowy CI
    ci_list <- lapply(ns, function(n) {
      ci <- ci_mean(xbar, s, n)
      list(n = n, lower = ci$lower, upper = ci$upper, me = ci$me)
    })

    visible_k <- step  # ile CI pokazujemy
    if (visible_k < 1) visible_k <- 0
    if (visible_k > length(ns)) visible_k <- length(ns)

    all_lowers <- sapply(ci_list, function(c) c$lower)
    all_uppers <- sapply(ci_list, function(c) c$upper)
    xlims <- c(min(all_lowers), max(all_uppers))
    if (!is.null(hypothesis)) {
      xlims <- range(c(xlims, hypothesis$bound))
    }
    pad <- diff(xlims) * 0.1
    xlims <- c(xlims[1] - pad, xlims[2] + pad)

    y_positions <- seq_along(ns)

    p <- ggplot() +
      coord_cartesian(xlim = xlims, ylim = c(0.3, length(ns) + 0.7)) +
      labs(x = xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    if (!is.null(hypothesis)) {
      if (hypothesis$dir == "gt") {
        p <- p + annotate("rect",
                          xmin = hypothesis$bound, xmax = Inf,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      } else {
        p <- p + annotate("rect",
                          xmin = -Inf, xmax = hypothesis$bound,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      }
      p <- p +
        geom_vline(xintercept = hypothesis$bound, color = col_hyp,
                   linewidth = 1, linetype = "solid") +
        annotate("text", x = hypothesis$bound, y = length(ns) + 0.5,
                 label = paste0(if (hypothesis$dir == "gt") "≥ " else "≤ ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    }

    if (visible_k >= 1) {
      rows_df <- data.frame(
        y = sapply(seq_len(visible_k), function(i) y_positions[i]),
        lower = sapply(seq_len(visible_k), function(i) ci_list[[i]]$lower),
        upper = sapply(seq_len(visible_k), function(i) ci_list[[i]]$upper),
        n = sapply(seq_len(visible_k), function(i) ci_list[[i]]$n),
        xbar_val = xbar
      )
      p <- p +
        geom_errorbar(data = rows_df,
                       aes(xmin = lower, xmax = upper, y = y),
                       width = 0.12, color = col_ci, linewidth = 1.8, orientation = "y") +
        geom_point(data = rows_df,
                   aes(x = xbar_val, y = y),
                   color = col_estimate, size = 5, shape = 18)
      # Labelki n i granic CI dodajemy przez annotate (jeden po drugim)
      for (i in seq_len(visible_k)) {
        ci <- ci_list[[i]]
        y <- y_positions[i]
        p <- p +
          annotate("text", x = xlims[1], y = y,
                   label = paste0("n = ", ci$n),
                   hjust = 0, fontface = "bold", size = 4.5,
                   color = upwr_secondary) +
          annotate("text", x = ci$upper, y = y + 0.22,
                   label = paste0("[", round(ci$lower, 2), " ; ",
                                  round(ci$upper, 2), "]"),
                   hjust = 1, size = 3.8, color = col_ci,
                   fontface = "bold")
      }
    }

    p
  }

  # ---- Plot dla diff_means ----
  # step 1=próby, 2=średnie, 3=różnica, 4=+SE, 5=+CI
  plot_diff_means_step <- function(data, step, xlab, hypothesis = NULL) {
    x1 <- data$x1; s1 <- data$s1; n1 <- data$n1
    x2 <- data$x2; s2 <- data$s2; n2 <- data$n2

    cid <- ci_diff_means(x1, s1, n1, x2, s2, n2)
    diff_val <- cid$diff
    se <- cid$se; me <- cid$me

    col_g1 <- col_ci
    col_g2 <- col_miss

    # Generuj reprezentatywne próbki z parametrów
    set.seed(11)
    samp1 <- rnorm(n1, mean = x1, sd = s1)
    samp1 <- (samp1 - mean(samp1)) / sd(samp1) * s1 + x1
    set.seed(17)
    samp2 <- rnorm(n2, mean = x2, sd = s2)
    samp2 <- (samp2 - mean(samp2)) / sd(samp2) * s2 + x2

    # Gdy n > 80, pokazujemy losowa podproke (dla czytelnosci)
    max_show <- 80
    if (n1 > max_show) samp1 <- sample(samp1, max_show)
    if (n2 > max_show) samp2 <- sample(samp2, max_show)

    # ---- GÓRNY PANEL ----
    xlims_top <- range(c(samp1, samp2))
    pad_t <- diff(xlims_top) * 0.06
    xlims_top <- c(xlims_top[1] - pad_t, xlims_top[2] + pad_t)

    set.seed(42)
    jit1 <- runif(length(samp1), 1.55, 2.05)
    set.seed(43)
    jit2 <- runif(length(samp2), 0.75, 1.25)

    p_top <- ggplot() +
      coord_cartesian(xlim = xlims_top, ylim = c(0.35, 2.25)) +
      labs(x = xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      annotate("text", x = xlims_top[1], y = 1.8, label = data$label1,
               hjust = 0, fontface = "bold", size = 4.5, color = col_g1) +
      annotate("text", x = xlims_top[1], y = 1.0, label = data$label2,
               hjust = 0, fontface = "bold", size = 4.5, color = col_g2)

    if (step >= 1) {
      p_top <- p_top +
        geom_point(data = data.frame(x = samp1, y = jit1),
                   aes(x = x, y = y), color = col_g1, size = 3, alpha = 0.7) +
        geom_point(data = data.frame(x = samp2, y = jit2),
                   aes(x = x, y = y), color = col_g2, size = 3, alpha = 0.7)
    }
    if (step >= 2) {
      p_top <- p_top +
        geom_segment(aes(x = x1, xend = x1, y = 0.4, yend = 2.1),
                     color = col_g1, linetype = "dotted", linewidth = 0.8) +
        geom_segment(aes(x = x2, xend = x2, y = 0.4, yend = 2.1),
                     color = col_g2, linetype = "dotted", linewidth = 0.8) +
        geom_point(aes(x = x1, y = 1.8), color = col_g1,
                   size = 7, shape = 18) +
        geom_point(aes(x = x2, y = 1.0), color = col_g2,
                   size = 7, shape = 18) +
        annotate("text", x = x1, y = 2.15,
                 label = paste0("x̄₁ = ", round(x1, 2)),
                 color = col_g1, fontface = "bold", size = 4.5) +
        annotate("text", x = x2, y = 0.55,
                 label = paste0("x̄₂ = ", round(x2, 2)),
                 color = col_g2, fontface = "bold", size = 4.5)
    }

    if (step < 3) {
      return(p_top)
    }

    # ---- DOLNY PANEL ----
    xlims_bot <- range(c(0, diff_val - 1.3 * me, diff_val + 1.3 * me))
    if (!is.null(hypothesis)) {
      xlims_bot <- range(c(xlims_bot, hypothesis$bound))
    }
    pad_b <- diff(xlims_bot) * 0.1
    xlims_bot <- c(xlims_bot[1] - pad_b, xlims_bot[2] + pad_b)

    p_bot <- ggplot() +
      coord_cartesian(xlim = xlims_bot, ylim = c(-0.55, 0.65)) +
      labs(x = paste0("Różnica (", data$unit, ")  —  ", data$diff_label),
           y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Obszar hipotezy
    if (!is.null(hypothesis)) {
      if (hypothesis$dir == "gt") {
        p_bot <- p_bot + annotate("rect",
                                   xmin = hypothesis$bound, xmax = Inf,
                                   ymin = -Inf, ymax = Inf,
                                   fill = col_hyp, alpha = 0.15)
      } else {
        p_bot <- p_bot + annotate("rect",
                                   xmin = -Inf, xmax = hypothesis$bound,
                                   ymin = -Inf, ymax = Inf,
                                   fill = col_hyp, alpha = 0.15)
      }
      p_bot <- p_bot +
        geom_vline(xintercept = hypothesis$bound, color = col_hyp,
                   linewidth = 1, linetype = "solid") +
        annotate("text", x = hypothesis$bound, y = 0.55,
                 label = paste0(if (hypothesis$dir == "gt") "≥ " else "≤ ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    } else {
      # linia zero gdy brak hipotezy
      p_bot <- p_bot +
        geom_vline(xintercept = 0, color = col_true,
                   linewidth = 1, linetype = "dashed") +
        annotate("text", x = 0, y = 0.55, label = "0 = brak różnicy",
                 color = col_true, fontface = "bold", size = 4, hjust = -0.1)
    }

    p_bot <- p_bot +
      geom_point(aes(x = diff_val, y = 0), color = col_estimate,
                 size = 7, shape = 18) +
      annotate("text", x = diff_val, y = -0.22,
               label = paste0("x̄₁ − x̄₂ = ", round(diff_val, 2)),
               color = col_estimate, fontface = "bold", size = 4.5)

    if (step >= 4) {
      p_bot <- p_bot +
        geom_errorbar(aes(xmin = diff_val - se, xmax = diff_val + se, y = 0),
                       width = 0.08, color = col_hit, linewidth = 1.8, orientation = "y") +
        annotate("text", x = diff_val, y = 0.17,
                 label = paste0("± SE = ±", round(se, 2)),
                 color = col_hit, fontface = "bold", size = 4)
    }
    if (step >= 5) {
      p_bot <- p_bot +
        geom_errorbar(aes(xmin = diff_val - me, xmax = diff_val + me, y = 0),
                       width = 0.14, color = col_ci, linewidth = 2.2,
                       alpha = 0.6, orientation = "y") +
        annotate("text", x = diff_val, y = -0.42,
                 label = paste0("95% CI: [", round(diff_val - me, 2),
                                " ; ", round(diff_val + me, 2), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    }

    library(patchwork)
    (p_top / p_bot) + plot_layout(heights = c(2, 1))
  }

  # ---- Plot dla forest (wiele grup) ----
  plot_forest_step <- function(data, step, xlab, hypothesis = NULL) {
    groups <- data$groups
    means <- data$means
    sds <- data$sds
    ns <- data$ns
    k <- length(groups)

    ci_list <- lapply(seq_len(k), function(i) {
      ci <- ci_mean(means[i], sds[i], ns[i])
      list(lower = ci$lower, upper = ci$upper)
    })
    all_lowers <- sapply(ci_list, function(c) c$lower)
    all_uppers <- sapply(ci_list, function(c) c$upper)

    # Limity
    xlims <- range(c(all_lowers, all_uppers))
    if (!is.null(hypothesis)) {
      xlims <- range(c(xlims, hypothesis$bound))
    }
    pad <- diff(xlims) * 0.12
    xlims <- c(xlims[1] - pad, xlims[2] + pad)

    # Wygeneruj fake punkty dla każdej grupy
    points_df <- do.call(rbind, lapply(seq_len(k), function(i) {
      set.seed(50 + i)
      samp <- rnorm(ns[i], mean = means[i], sd = sds[i])
      samp <- (samp - mean(samp)) / sd(samp) * sds[i] + means[i]
      if (ns[i] > 60) samp <- sample(samp, 60)
      set.seed(100 + i)
      jit <- runif(length(samp), min = i - 0.25, max = i + 0.25)
      data.frame(x = samp, y = jit, group = groups[i])
    }))

    y_positions <- seq_len(k)
    group_df <- data.frame(group = groups, y = y_positions,
                            mean = means, lower = all_lowers, upper = all_uppers)

    p <- ggplot() +
      coord_cartesian(xlim = xlims, ylim = c(0.3, k + 0.7)) +
      labs(x = xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Etykiety grup
    p <- p +
      annotate("text", x = xlims[1], y = y_positions,
               label = groups, hjust = 0, fontface = "bold", size = 4.5,
               color = upwr_secondary)

    # Obszar hipotezy
    if (!is.null(hypothesis)) {
      if (hypothesis$dir == "gt") {
        p <- p + annotate("rect",
                          xmin = hypothesis$bound, xmax = Inf,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      } else {
        p <- p + annotate("rect",
                          xmin = -Inf, xmax = hypothesis$bound,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      }
      p <- p +
        geom_vline(xintercept = hypothesis$bound, color = col_hyp,
                   linewidth = 1, linetype = "solid") +
        annotate("text", x = hypothesis$bound, y = k + 0.45,
                 label = paste0(if (hypothesis$dir == "gt") "≥ " else "≤ ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    }

    # Krok 1+: punkty
    if (step >= 1) {
      p <- p + geom_point(data = points_df, aes(x = x, y = y),
                          color = col_ci, size = 2.3, alpha = 0.55)
    }
    # Krok 2+: średnie
    if (step >= 2) {
      p <- p + geom_point(data = group_df, aes(x = mean, y = y),
                          color = col_estimate, size = 6, shape = 18)
    }
    # Krok 3+: CI
    if (step >= 3) {
      p <- p + geom_errorbar(data = group_df,
                               aes(xmin = lower, xmax = upper, y = y),
                               width = 0.18, color = col_ci, linewidth = 1.8, orientation = "y")
    }

    p
  }

  # ---- Liczba "core" kroków budowy CI (bez hipotez) ----
  n_core_steps <- function(cfg) length(cfg$steps)
  # ---- Wykres case'a: krok budowy CI i (na ostatnim kroku) obszar hipotezy ----
  render_case_plot <- function(cfg, step, hyp_idx) {
    hypothesis <- NULL
    plot_step <- step
    if (!is.null(hyp_idx)) {
      hyp_obj <- cfg$hypotheses[[hyp_idx]]
      # Hipoteza pairwise (forest) nie ma bound/dir — nie rysujemy obszaru.
      if (is.null(hyp_obj$kind) || hyp_obj$kind != "pairwise") {
        hypothesis <- hyp_obj
      }
    }

    switch(cfg$type,
      "single_mean" = plot_single_mean_step(cfg$data, plot_step, cfg$xlab,
                                             hypothesis = hypothesis),
      "compare_n"   = plot_compare_n_step(cfg$data, plot_step, cfg$xlab,
                                           hypothesis = hypothesis),
      "diff_means"  = plot_diff_means_step(cfg$data, plot_step, cfg$xlab,
                                            hypothesis = hypothesis),
      "forest"      = plot_forest_step(cfg$data, plot_step, cfg$xlab,
                                        hypothesis = hypothesis)
    )
  }

  # ---- Pairwise: szybka macierz rozłącznych CI dla forest plot ----
  # TRUE oznacza rozłączne 95% CI, co jest mocnym sygnałem różnicy.
  # FALSE jest wynikiem nierozstrzygającym: nakładanie CI nie dowodzi braku różnicy.
  forest_pairwise_matrix <- function(data) {
    k <- length(data$groups)
    cis <- lapply(seq_len(k), function(i) {
      ci_mean(data$means[i], data$sds[i], data$ns[i])
    })
    m <- matrix(FALSE, nrow = k, ncol = k,
                dimnames = list(data$groups, data$groups))
    for (i in seq_len(k)) for (j in seq_len(k)) {
      if (i == j) next
      # Szybki sygnał różnicy: CI[i] i CI[j] są rozłączne.
      m[i, j] <- (cis[[i]]$upper < cis[[j]]$lower) ||
                 (cis[[j]]$upper < cis[[i]]$lower)
    }
    m
  }

  # Tabelka HTML dla macierzy pairwise
  render_pairwise_table <- function(mat) {
    groups <- rownames(mat)
    k <- length(groups)
    sym <- function(i, j) if (i == j) "—" else if (mat[i, j]) "✓" else "×"
    cls <- function(i, j) if (i == j) "is-dim" else if (mat[i, j]) "is-best" else NA
    df <- data.frame(group = groups)
    cell_class <- list()
    for (j in seq_len(k)) {
      key <- paste0("g", j)
      df[[key]] <- vapply(seq_len(k), function(i) sym(i, j), character(1))
      cell_class[[key]] <- vapply(seq_len(k), function(i) cls(i, j), character(1))
    }
    lc_table(df,
      cols = c(list(lc_col("group", "", "row")),
               lapply(seq_len(k), function(j) lc_col(paste0("g", j), groups[j], "text"))),
      cell_class = cell_class, fit = TRUE)
  }


  # Narracja "jak w raporcie" dla pairwise
  pairwise_narrative <- function(data, mat, unit = "") {
    groups <- data$groups
    means <- data$means
    k <- length(groups)
    unit_str <- if (nzchar(unit)) paste0(" ", unit) else ""

    # Wyciągnij pary z rozłącznymi CI z górnego trójkąta.
    diff_pairs <- list()
    for (i in seq_len(k - 1)) for (j in seq(i + 1, k)) {
      if (mat[i, j]) {
        if (means[i] > means[j]) {
          diff_pairs[[length(diff_pairs) + 1]] <- list(hi = groups[i], lo = groups[j])
        } else {
          diff_pairs[[length(diff_pairs) + 1]] <- list(hi = groups[j], lo = groups[i])
        }
      }
    }
    n_diff <- length(diff_pairs)

    if (n_diff == 0) {
      return(paste0(
        "Żadna para nie ma rozłącznych 95% CI. Ta szybka ocena nie wskazała ",
        "oczywistych różnic, ale nakładanie się przedziałów nie dowodzi ich braku. ",
        "Aby rozstrzygnąć konkretną parę, policz przedział dla różnicy średnich."
      ))
    }

    # Para jako tekst „wyższa > niższa”.
    pair_str <- function(pp) paste0(pp$hi, " > ", pp$lo)
    join_pairs <- function(strs) {
      if (length(strs) == 1) strs
      else paste0(paste(strs[-length(strs)], collapse = ", "), " oraz ", strs[length(strs)])
    }

    # Sprawdź, czy jedna grupa odstaje od WSZYSTKICH innych (np. SOR vs reszta)
    standout_idx <- which(sapply(seq_len(k), function(i) all(mat[i, -i])))
    if (length(standout_idx) == 1) {
      i <- standout_idx
      others <- means[-i]
      direction <- if (means[i] > max(others)) "wyższą" else "niższą"
      rest_pairs <- Filter(function(pp) pp$hi != groups[i] && pp$lo != groups[i], diff_pairs)
      rest_txt <- if (length(rest_pairs) == 0) {
        ", a ich CI nakładają się — ten wykres nie rozstrzyga różnic między nimi."
      } else {
        paste0(". Wśród nich rozłączne CI mają jeszcze: ",
               join_pairs(sapply(rest_pairs, pair_str)),
               "; pozostałe pary się nakładają.")
      }
      return(paste0(
        "Spośród wszystkich badanych grup wyraźnie odstaje ",
        groups[i], " (średnia ", round(means[i], 1), unit_str,
        ") — szybkie porównanie wskazuje na ", direction, " wartość niż w każdej z pozostałych grup ",
        "(jej 95% CI nie nakłada się z żadnym innym). ",
        "Pozostałe grupy mają średnie w przedziale ",
        round(min(others), 1), "–", round(max(others), 1), unit_str,
        rest_txt
      ))
    }

    # Wymień konkretne pary z rozłącznymi CI
    pairs_inline <- join_pairs(sapply(diff_pairs, pair_str))

    intro <- if (n_diff == 1) {
      "Spośród wszystkich porównań jedynie jedna para ma rozłączne 95% CI: "
    } else {
      paste0("Rozłączne 95% CI, wskazujące wyraźne różnice, mają ",
             n_diff, " pary: ")
    }

    paste0(
      intro, pairs_inline, ". ",
      "Dla pozostałych par 95% CI nakładają się. To wynik nierozstrzygający, ",
      "a nie dowód braku różnicy; konkretną parę sprawdzamy przedziałem dla różnicy średnich."
    )
  }

  # ---- Opis hipotezy i werdykt pod widgetem case'a ----
  render_case_explain <- function(cfg, hyp_idx, revealed, widget_id) {
    if (is.null(hyp_idx)) return(NULL)
    hyp <- cfg$hypotheses[[hyp_idx]]

    # Najpierw sama treść hipotezy (czas na dyskusję), werdykt po kliknięciu.
    if (!revealed) {
      return(lc_status(
        p(tags$strong(paste0("Hipoteza ", hyp_idx, ":")), " ", hyp$text),
        p("Spójrz na wykres: gdzie leży CI względem obszaru hipotezy?
          Co o tym sądzicie?"),
        lc_action(paste0(widget_id, "_reveal"), "Pokaż werdykt", variant = "solid")
      ))
    }

    if (!is.null(hyp$kind) && hyp$kind == "pairwise") {
      mat <- forest_pairwise_matrix(cfg$data)
      narrative <- pairwise_narrative(cfg$data, mat,
                                       unit = if (!is.null(hyp$unit)) hyp$unit else "")
      return(lc_status(
        p(tags$strong("Hipoteza:"), " ", hyp$text),
        p(tags$strong("Szybka mapa porównań:")),
        lc_caption("✓ = rozłączne CI: wyraźny sygnał różnicy; ",
                   "× = CI nakładają się: wykres nie rozstrzyga"),
        render_pairwise_table(mat),
        p(tags$strong("Jak to opisać na tym etapie:")),
        p(tags$em(HTML(narrative)))
      ))
    }

    verdict <- compute_verdict_for_case(cfg, hyp)
    label <- verdict_label(verdict)

    body <- if (verdict == "yes" && !is.null(hyp$explain_yes)) {
      p(hyp$explain_yes)
    } else if (verdict == "no" && !is.null(hyp$explain_no)) {
      p(hyp$explain_no)
    } else {
      p("CI przecina granicę hipotezy — nie możemy jednoznacznie
        stwierdzić, czy jest prawdziwa.")
    }

    lc_status(
      p(tags$strong(paste0("Hipoteza ", hyp_idx, ":")), " ", hyp$text),
      p(tags$strong("Werdykt:"), " ",
        if (verdict %in% c("yes", "no")) lc_verdict(label, type = if (verdict == "yes") "ok" else "danger") else label),
      body
    )
  }

  # ---- Werdykt dla case'a ----
  compute_verdict_for_case <- function(cfg, hyp) {
    switch(cfg$type,
      "single_mean" = {
        ci <- ci_mean(cfg$data$xbar, cfg$data$s, cfg$data$n)
        hypothesis_verdict(ci$lower, ci$upper, hyp$bound, hyp$dir)
      },
      "compare_n" = {
        # Werdykt bazujemy na najwęższym (największym n) CI
        # (= najbardziej precyzyjnym oszacowaniu)
        largest_n <- max(cfg$data$ns)
        ci <- ci_mean(cfg$data$xbar, cfg$data$s, largest_n)
        hypothesis_verdict(ci$lower, ci$upper, hyp$bound, hyp$dir)
      },
      "diff_means" = {
        cid <- ci_diff_means(cfg$data$x1, cfg$data$s1, cfg$data$n1,
                              cfg$data$x2, cfg$data$s2, cfg$data$n2)
        hypothesis_verdict(cid$lower, cid$upper, hyp$bound, hyp$dir)
      },
      "forest" = {
        # Znajdź odpowiednią grupę
        idx <- which(cfg$data$groups == hyp$which)
        ci <- ci_mean(cfg$data$means[idx], cfg$data$sds[idx], cfg$data$ns[idx])
        hypothesis_verdict(ci$lower, ci$upper, hyp$bound, hyp$dir)
      }
    )
  }

  # ---- Widget krokowy case'a: pasek budowy CI, hipotezy jako przełączniki ----
  register_case <- function(case_id) {
    cfg <- cases_config[[case_id]]
    n_core <- length(cfg$steps)
    widget_id <- paste0("ch3_case", case_id)
    step <- lc_step_server(widget_id, input)$step
    # Hipoteza liczy się tylko na ostatnim kroku (pełny przedział).
    hyp_idx <- reactive({
      h <- input[[paste0(widget_id, "_hyp")]]
      if (is.null(h) || step() < n_core) NULL else as.integer(h)
    })
    revealed <- reactiveVal(FALSE)
    observeEvent(input[[paste0(widget_id, "_hyp")]], revealed(FALSE), ignoreNULL = FALSE)
    observeEvent(input[[paste0(widget_id, "_reveal")]], revealed(TRUE))

    hyp_choices <- stats::setNames(as.character(seq_along(cfg$hypotheses)),
                                   paste("Hipoteza", seq_along(cfg$hypotheses)))
    output[[paste0(widget_id, "_widget")]] <- renderUI({
      lc_step_widget(widget_id,
        steps = sub("^[0-9]+\\.\\s*", "", cfg$steps),
        plot_id = paste0(widget_id, "_plot"),
        ratio = if (cfg$type %in% c("single_mean", "compare_n")) "2.4/1" else "1.6/1",
        toolbar = lc_toolbar(
          lc_step_from(n_core, lc_chips(paste0(widget_id, "_hyp"), hyp_choices, label = "Sprawdź"))
        ),
        extra = uiOutput(paste0(widget_id, "_explain"))
      )
    })
    output[[paste0(widget_id, "_text")]] <- renderUI({
      if (step() == n_core) "Przedział gotowy. Wybierz hipotezę, żeby sprawdzić ją na wykresie."
    })
    zoom_plot_server(paste0(widget_id, "_plot"), reactive(
      render_case_plot(cfg, step(), hyp_idx())
    ))
    output[[paste0(widget_id, "_explain")]] <- renderUI(
      render_case_explain(cfg, hyp_idx(), revealed(), widget_id)
    )
  }

  for (cid in names(cases_config)) {
    register_case(cid)
  }

  # ==========================================================================
  # WIDGET 2B: NAKŁADAJĄCE SIĘ CI GRUP vs CI RÓŻNICY
  # Trzy statyczne scenariusze z dziedziny technologii żywności
  # ==========================================================================

  # --- Dane (statyczne, przygotowane z ustalonymi seedami) ---
  ch3_comp_data <- list(
    A = list(
      g1_name = "Dostawca A",
      g2_name = "Dostawca B",
      unit    = "zawartość białka (%)",
      g1 = c(13.17,11.08,11.38,11.55,11.22,11.23,12.25,11.73,11.89,13.11,
             12.01,13.43,13.17,11.99,12.94,12.08,11.26,11.62,11.80,12.39,
             12.30,12.22,12.58,10.97,12.56,11.91,12.25,12.16,11.21,11.63,
             11.28,12.23,11.87,11.75,11.55,11.46,12.40,11.14,11.71,11.99),
      g2 = c(11.63,10.48,10.73,10.11,10.67,10.66,11.71,11.25,10.96,11.46,
             10.74,10.90,11.12,11.92,11.33,11.19, 9.96,11.09,11.00,10.36,
             10.95,11.00,11.23,11.32,11.09,11.57,11.36,11.59,11.66,11.32,
             11.16,10.35,10.53,10.38, 9.92,10.10,10.37,10.57,10.86,12.35)
    ),
    B = list(
      g1_name = "Szkło",
      g2_name = "Plastik",
      unit    = "zawartość tłuszczu (%)",
      g1 = c(3.03,3.19,2.80,2.84,3.47,2.95,3.51,3.34,3.17,2.93,
             2.97,3.09,2.80,3.12,2.89,3.18,3.12,3.40,3.03,3.02,
             3.01,3.18,3.07,3.27,3.20,3.18,3.13,2.99,3.12,2.93),
      g2 = c(2.93,2.98,3.38,2.82,2.99,3.33,3.16,3.60,3.06,3.12,
             2.80,3.22,3.43,2.99,3.43,3.12,2.66,3.43,3.39,3.26,
             3.41,3.15,3.01,3.33,3.25,3.35,3.17,3.32,3.58,3.23)
    ),
    C = list(
      g1_name = "Linia 1",
      g2_name = "Linia 2",
      unit    = "zawartość błonnika (g / 100 g)",
      # Wygenerowane: set.seed(3); round(rnorm(120, 9.3, 1.0), 2), round(rnorm(120, 9.0, 1.0), 2)
      g1 = c(8.34,8.99,10.62,7.52,9.70,10.56,10.25, 9.04, 8.49,10.13,
             7.94,10.82,10.78, 7.98,11.47, 9.16,10.82, 8.87, 7.83, 8.53,
             8.35,12.44, 9.78, 9.70,11.17, 8.54, 9.25, 8.43,10.52, 8.06,
             8.64, 8.22,10.67, 9.71, 9.80,10.42,10.19,11.20,10.62, 9.83,
             8.61, 9.36, 9.62, 9.18, 9.76, 9.53,10.91,10.57, 9.08, 9.46,
             10.25, 7.21, 9.78, 9.64, 8.19,10.27, 9.46, 9.29,10.25, 7.76,
             11.24, 9.04, 9.85,10.18,11.01, 8.86, 9.24, 9.28, 9.09, 8.29,
             9.87,10.10, 9.17, 9.41,11.07, 9.18, 9.19,10.50, 9.79, 8.65,
             8.85, 9.22,10.81,10.12, 8.36, 8.92, 8.70,10.63, 9.81, 9.07,
             9.44, 9.43, 8.67,11.13, 7.88,10.15, 9.56, 9.78, 8.75, 9.82,
             8.51, 8.41, 9.77, 9.08, 7.55, 9.04,10.43, 7.98, 7.29, 8.58,
             8.12, 8.16, 9.74, 9.25, 8.69,10.36, 7.22,10.15, 9.52, 8.95),
      g2 = c(8.30,10.15, 9.77, 7.74, 9.45, 7.68, 7.64, 9.92, 9.36, 8.28,
             9.14,10.19, 9.85, 7.83, 9.09, 9.45, 7.63, 9.09, 8.52, 9.26,
             8.20,10.05,11.76, 8.75,10.03, 9.95, 9.54, 8.60, 8.70, 7.74,
             8.94, 9.74, 9.41,10.51, 7.75, 7.99, 9.08, 9.20, 8.13, 7.76,
             8.31, 9.10, 9.40,10.24, 9.16,11.00, 9.25,10.82, 8.96, 9.42,
             8.10,10.32, 9.01, 8.35, 9.36,10.20, 9.71, 7.18, 8.33, 8.24,
             9.56, 8.10, 8.83, 9.32, 8.00, 8.33, 8.20,10.19,10.94, 8.35,
             8.87, 7.86, 9.56, 9.09,10.51, 9.30, 7.26, 8.50, 8.16, 8.30,
             8.45, 8.04,10.66, 7.19,10.16, 8.73, 8.61, 9.17, 8.85, 9.57,
             9.28, 9.64,10.54, 8.79,11.00,10.24, 9.75,10.32, 8.96, 7.31,
             9.35, 8.64, 9.17,10.40, 7.64, 9.23, 9.86, 9.59, 9.48, 9.27,
             8.32, 8.92, 9.76, 9.57, 8.72, 8.07, 7.55, 9.74, 9.43, 9.32)
    )
  )

  # Helper: liczy CI dla dwóch grup + CI różnicy (Welch)
  ch3_comp_cis <- function(g1, g2, conf = 0.95) {
    alpha <- 1 - conf
    n1 <- length(g1); n2 <- length(g2)
    m1 <- mean(g1);   m2 <- mean(g2)
    s1 <- sd(g1);     s2 <- sd(g2)
    se1 <- s1 / sqrt(n1); se2 <- s2 / sqrt(n2)
    ci1 <- m1 + c(-1, 1) * qt(1 - alpha / 2, n1 - 1) * se1
    ci2 <- m2 + c(-1, 1) * qt(1 - alpha / 2, n2 - 1) * se2
    se_d <- sqrt(se1^2 + se2^2)
    df_w <- (se1^2 + se2^2)^2 / (se1^4 / (n1 - 1) + se2^4 / (n2 - 1))
    ci_d <- (m1 - m2) + c(-1, 1) * qt(1 - alpha / 2, df_w) * se_d
    list(m1 = m1, m2 = m2, ci1 = ci1, ci2 = ci2,
         md = m1 - m2, ci_d = ci_d,
         overlap_lo = max(ci1[1], ci2[1]),
         overlap_hi = min(ci1[2], ci2[2]))
  }

  # Helper: plot trzech CI (grupa 1, grupa 2, różnica) z paskiem nakładania
  ch3_comp_plot <- function(scenario_key) {
    dat <- ch3_comp_data[[scenario_key]]
    cis <- ch3_comp_cis(dat$g1, dat$g2)

    # Rama wykresu: górny panel (CI grup), dolny panel (CI różnicy)
    # Zrobimy w jednym plocie z facet_grid
    df_groups <- data.frame(
      row   = c(2, 1),
      label = c(dat$g1_name, dat$g2_name),
      mean  = c(cis$m1, cis$m2),
      lo    = c(cis$ci1[1], cis$ci2[1]),
      hi    = c(cis$ci1[2], cis$ci2[2]),
      panel = "CI grup (osobno)"
    )
    df_diff <- data.frame(
      row   = 1.5,
      label = paste0(dat$g1_name, " − ", dat$g2_name),
      mean  = cis$md,
      lo    = cis$ci_d[1],
      hi    = cis$ci_d[2],
      panel = "CI różnicy"
    )

    overlap_present <- cis$overlap_lo <= cis$overlap_hi
    df_overlap <- if (overlap_present) {
      data.frame(xmin = cis$overlap_lo, xmax = cis$overlap_hi, panel = "CI grup (osobno)")
    } else {
      NULL
    }

    p_groups <- ggplot(df_groups, aes(y = row)) +
      { if (!is.null(df_overlap))
          geom_rect(data = df_overlap,
                    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                    inherit.aes = FALSE,
                    fill = "#f1c40f", alpha = 0.25)
      } +
      geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.18,
                     color = col_ci, linewidth = 1.4, orientation = "y") +
      geom_point(aes(x = mean), color = col_estimate, size = 4) +
      geom_text(aes(x = mean, label = sprintf("%.2f", mean)),
                vjust = -1.2, color = col_estimate, fontface = "bold", size = 4.2) +
      scale_y_continuous(breaks = df_groups$row, labels = df_groups$label,
                         limits = c(0.5, 2.5)) +
      labs(x = dat$unit, y = NULL) +
      theme_upwr() +
      theme(plot.title = element_text(size = 13, face = "bold"))

    p_diff <- ggplot(df_diff, aes(y = row)) +
      geom_vline(xintercept = 0, color = col_true,
                 linewidth = 1.0, linetype = "dashed") +
      annotate("text", x = 0, y = 2.3, label = "0",
               color = col_true, fontface = "bold", size = 4.5) +
      geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.18,
                     color = col_ci, linewidth = 1.4, orientation = "y") +
      geom_point(aes(x = mean), color = col_estimate, size = 4) +
      geom_text(aes(x = mean, label = sprintf("%.2f", mean)),
                vjust = -1.2, color = col_estimate, fontface = "bold", size = 4.2) +
      scale_y_continuous(breaks = df_diff$row, labels = df_diff$label,
                         limits = c(0.5, 2.5)) +
      labs(
           x = paste("różnica —", dat$unit), y = NULL) +
      theme_upwr() +
      theme(plot.title = element_text(size = 13, face = "bold"))

    # Układ jeden pod drugim
    gridExtra::arrangeGrob(p_groups, p_diff, ncol = 1, heights = c(1, 1))
  }

  # Helper: werdykt tekstowy
  ch3_comp_verdict <- function(scenario_key) {
    dat <- ch3_comp_data[[scenario_key]]
    cis <- ch3_comp_cis(dat$g1, dat$g2)
    overlap_present <- cis$overlap_lo <= cis$overlap_hi
    diff_excludes_0 <- !(cis$ci_d[1] <= 0 & 0 <= cis$ci_d[2])
    overlap_w <- if (overlap_present) cis$overlap_hi - cis$overlap_lo else 0

    fmt <- function(x) sprintf("%.2f", x)
    ci_txt <- function(ci) paste0("[", fmt(ci[1]), "; ", fmt(ci[2]), "]")

    # Same fakty liczbowe; wnioski ze scenariuszy są w narracji pod nimi.
    lc_status(
      p(lc_verdict(tags$b("Co widzimy:"), type = if (scenario_key == "C") "warning" else "ok")),
      tags$ul(
        tags$li(dat$g1_name, ": średnia ", fmt(cis$m1),
                ", 95% CI ", ci_txt(cis$ci1)),
        tags$li(dat$g2_name, ": średnia ", fmt(cis$m2),
                ", 95% CI ", ci_txt(cis$ci2)),
        tags$li("CI różnicy (", dat$g1_name, " − ", dat$g2_name, "): ",
                ci_txt(cis$ci_d))
      ),
      tags$ul(
        tags$li("Czy CI grup się nakrywają? ",
                tags$b(if (overlap_present)
                  paste0("TAK (na odcinku szerokości ", fmt(overlap_w), ")")
                else "NIE")),
        tags$li("Czy CI różnicy zawiera 0? ",
                tags$b(if (diff_excludes_0) "NIE" else "TAK"))
      )
    )
  }

  zoom_plot_server("ch3_comp_A_plot", reactive({ ch3_comp_plot("A") }))
  zoom_plot_server("ch3_comp_B_plot", reactive({ ch3_comp_plot("B") }))
  zoom_plot_server("ch3_comp_C_plot", reactive({ ch3_comp_plot("C") }))
  output$ch3_comp_A_verdict <- renderUI({ ch3_comp_verdict("A") })
  output$ch3_comp_B_verdict <- renderUI({ ch3_comp_verdict("B") })
  output$ch3_comp_C_verdict <- renderUI({ ch3_comp_verdict("C") })

}
