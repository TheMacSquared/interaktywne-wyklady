# ============================================================================
# CHAPTER 6: Zmienna ilościowa w dwóch grupach — test t dwóch grup
# ============================================================================

ch6_ui <- list(
  id = "ch-dwie-grupy", num = "08", title = "Test t dwóch grup",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 08 · Testowanie hipotez",
      num    = "08",
      title  = "Test t dwóch grup.",
      lead   = "Średnie dwóch grup w próbie prawie nigdy nie wychodzą identyczne.
                Test t mierzy różnicę między nimi w błędach standardowych i mówi,
                czy jest większa, niż mogłoby ją zrobić samo losowanie. Gdy te same
                osoby zmierzono dwa razy, ten sam test liczy się na różnicach w parach."
    ),

    lc_p("W rozdziale 04 porównywaliśmy średnią jednej próby z ustaloną wartością
      \\(\\mu_0\\). W poprzednim rozdziale badaliśmy związek dwóch zmiennych
      jakościowych. Teraz łączymy oba wątki: mamy ", gloss("zmienna ilościowa", "zmienną ilościową"), ", na przykład
      wzrost, i ", gloss("zmienna jakościowa", "zmienną jakościową"), " z dwiema kategoriami, na przykład płeć.
      Pytanie brzmi, czy średnia zmiennej ilościowej jest taka sama w obu grupach."),

    lc_h2("ch6-intro", "Test t dla dwóch prób niezależnych"),

    lc_p("Próby są niezależne, gdy w obu grupach są różne osoby i wynik jednej
      osoby nic nie mówi o wyniku innej: kobiety i mężczyźni, dwie odmiany
      pszenicy, dwie partie towaru. Parametrem, o który pytamy, jest różnica
      średnich populacji \\(\\mu_1 - \\mu_2\\). Tak jak w teście jednej próby,
      hipotezę zapisujemy w jednym z trzech wariantów, zależnie od brzmienia
      ", gloss("pytanie badawcze", "pytania badawczego"), "."),

    lc_formula_box(
      p(strong("Dwustronna"), " (grupy różnią się w dowolną stronę):"),
      p(withMathJax("\\(H_0: \\mu_1 = \\mu_2 \\quad\\)"),
        withMathJax("\\(H_a: \\mu_1 \\neq \\mu_2\\)"))
    ),
    lc_formula_box(
      p(strong("Prawostronna"), " (grupa 1 ma wyższą średnią):"),
      p(withMathJax("\\(H_0: \\mu_1 \\leq \\mu_2 \\quad\\)"),
        withMathJax("\\(H_a: \\mu_1 > \\mu_2\\)"))
    ),
    lc_formula_box(
      p(strong("Lewostronna"), " (grupa 1 ma niższą średnią):"),
      p(withMathJax("\\(H_0: \\mu_1 \\geq \\mu_2 \\quad\\)"),
        withMathJax("\\(H_a: \\mu_1 < \\mu_2\\)"))
    ),

    lc_p(gloss("hipoteza zerowa", "Hipoteza zerowa"), " \\(\\mu_1 = \\mu_2\\) to to samo co \\(\\mu_1 - \\mu_2 = 0\\).
      Mamy więc znów jedną liczbę z próby, różnicę \\(\\bar{x}_1 - \\bar{x}_2\\),
      i wartość, z którą ją porównujemy: zero. ",
      gloss("statystyka testowa", "Statystyka testowa"), " mówi, ile ",
      gloss("błąd standardowy", "błędów standardowych"), " dzieli tę różnicę od zera.
      Błąd standardowy różnicy znamy z wykładu 03, z ", gloss("przedział ufności", "przedziału ufności"), " dla
      różnicy średnich: ", gloss("wariancja", "wariancje"), " obu średnich się dodają."),

    lc_formula_box(withMathJax(
      "$$t = \\frac{\\bar{x}_1 - \\bar{x}_2}{\\sqrt{\\dfrac{s_1^2}{n_1} + \\dfrac{s_2^2}{n_2}}}$$"
    )),

    lc_p("Każda grupa ma tu własne ", gloss("odchylenie standardowe"), ", czyli nie zakładamy
      równych wariancji. To ",
      gloss("test t Welcha", "test t Welcha"), ", ten sam wariant co w przedziale
      dla różnicy z wykładu 03. Przy prawdziwej H₀ statystyka ma w przybliżeniu
      rozkład t, a liczbę ", gloss("stopnie swobody", "stopni swobody"), " wyznacza
      wzór Welcha–Satterthwaite'a. Wynik zwykle nie jest liczbą całkowitą
      i leży między \\(\\min(n_1, n_2) - 1\\) a \\(n_1 + n_2 - 2\\). Wszystkie panele
      tego rozdziału liczą właśnie wersję Welcha."),

    lc_p("Klasyczna wersja Studenta zakłada równe wariancje w obu grupach, łączy
      je w jedno wspólne odchylenie standardowe i używa \\(n_1 + n_2 - 2\\)
      stopni swobody. Przy podobnych wariancjach i licznościach obie wersje
      dają prawie to samo, przy wyraźnie różnych mogą się rozejść. Wracamy do tego w wykładzie 05."),

    lc_p("Decyzja przebiega jak w poprzednich rozdziałach. ",
      gloss("p-wartość", "P-wartość"), " to prawdopodobieństwo, że przy
      prawdziwej H₀ dostalibyśmy statystykę co najmniej tak odległą od zera jak
      nasza. Jeśli jest mniejsza niż ustalony przed analizą ", gloss("poziom istotności"), ",
      zwykle α = 0.05, odrzucamy H₀. ", gloss("test dwustronny", "Test dwustronny"), " przy α = 0.05 i 95% przedział
      ufności dla różnicy z wykładu 03 mają ten sam błąd standardowy i te same
      stopnie swobody, więc dają zgodną odpowiedź: H₀ odrzucamy dokładnie wtedy,
      gdy przedział dla \\(\\mu_1 - \\mu_2\\) nie obejmuje zera."),

    # ========================================================================
    # Cwiczenie: sformuluj hipotezy
    # ========================================================================
    lc_h2("ch6-cwiczenie", "Ćwiczenie: sformułuj hipotezy"),

    lc_p("Przed obejrzeniem testu w działaniu zapisz hipotezy dla trzech pytań
      badawczych. Zwróć uwagę, czy pytanie wskazuje kierunek różnicy i czy
      w obu grupach są na pewno różne osoby. Swoją odpowiedź porównaj
      z rozwiązaniem po kliknięciu „Pokaż odpowiedź”."),

    hypothesis_practice("ch6", list(
      list(
        question = "Czy mężczyźni jeżdżą szybciej niż kobiety? (średnia prędkość
                    przekroczenia limitu z fotoradarów, próba 200 kierowców)",
        h0 = "\\(H_0: \\mu_M \\leq \\mu_K\\)",
        ha = "\\(H_a: \\mu_M > \\mu_K\\) (mężczyźni szybciej)",
        note = "Jednostronny — pytanie jest kierunkowe."
      ),
      list(
        question = "Czy średni plon pszenicy odmiany X różni się od odmiany Y?",
        h0 = "\\(H_0: \\mu_X = \\mu_Y\\)",
        ha = "\\(H_a: \\mu_X \\neq \\mu_Y\\)",
        note = "Dwustronny — pytanie neutralne, nie wskazuje kierunku."
      ),
      list(
        question = "20 uczniów zmierzono przed i po kursie szybkiego czytania
                    (słowa na minutę). Czy kurs poprawił wyniki?",
        h0 = "\\(H_0: \\mu_d \\leq 0\\) (d = po − przed)",
        ha = "\\(H_a: \\mu_d > 0\\) (poprawa)",
        note = "Dane sparowane, a nie niezależne — te same osoby zmierzono dwa razy,
                więc analizujemy różnice."
      )
    )),

    lc_p("Trzecie pytanie ma inną budowę danych niż dwa pierwsze. Wrócimy do
      niego w sekcji o teście dla danych sparowanych."),

    # ========================================================================
    # WIDGET 1: Test t niezalezny
    # ========================================================================
    lc_h2("ch6-niezalezny", "Test t niezależny"),

    lc_p("Panel losuje próbę studentów z symulowanej populacji i porównuje
      kobiety z mężczyznami. W populacji kobiety mają średnio 166 cm wzrostu
      (σ = 6 cm), a mężczyźni 178 cm (σ = 7 cm). Średnia waga to 62 kg i 78 kg.
      Średnia ocen i czas dojazdu od płci nie zależą, więc dla tych dwóch
      zmiennych H₀ jest prawdziwa. Suwak ustala liczebność każdej z dwóch
      grup."),

    figure_panel(
      label = "Ryc. 8.1",
      title = "Porównanie dwóch grup",
      uiOutput("ch6_ind_hypothesis"),
      lc_step_widget("ch6_ind",
        steps = c("Dane", "Średnie w grupach", "Statystyka t", "p-wartość i decyzja"),
        toolbar = lc_toolbar(
          selectInput("ch6_ind_var", "Zmienna ilościowa",
            choices = c(
              "Wzrost" = "wzrost",
              "Waga" = "waga",
              "Średnia ocen" = "srednia_ocen",
              "Czas dojazdu" = "czas_dojazdu"
            ),
            selected = "wzrost"
          ),
          lc_slider("ch6_ind_n", "n (na grupę)", 15, 100, 40, 5),
          lc_action("ch6_run_ind_t", "Losuj próbę", icon = "shuffle", variant = "solid")
        ),
        plot_id = "ch6_ind_boxplot",
        extra = uiOutput("ch6_ind_table")
      )
    ),

    lc_p("Przy ustawieniach startowych losujemy 40 kobiet i 40 mężczyzn.
      Dla wzrostu błąd standardowy różnicy wynosi wtedy około
      \\(\\sqrt{6^2/40 + 7^2/40} \\approx 1.5\\) cm, a różnica w populacji
      to 12 cm, czyli około ośmiu błędów standardowych. Statystyka t wychodzi
      daleko w ogonie rozkładu t, p-wartość jest znikoma i H₀ odrzucamy
      praktycznie przy każdym losowaniu. Podobnie jest z wagą. Znak t zależy tylko
      od kolejności odejmowania: panel odejmuje średnie w kolejności
      alfabetycznej grup, czyli kobiety minus mężczyźni, dlatego dla wzrostu
      t jest ujemne."),

    lc_p("Ciekawiej jest dla średniej ocen i czasu dojazdu. Tu różnica
      w populacji wynosi zero, a mimo to średnie w próbie nigdy nie są równe.
      Zwykle test nie daje podstaw do odrzucenia H₀, ale mniej więcej co
      dwudzieste losowanie da p < 0.05. To ",
      gloss("błąd pierwszego rodzaju", "błąd pierwszego rodzaju"), " z rozdziału 03,
      którego prawdopodobieństwo ustaliliśmy, wybierając α. Z drugiej strony
      brak podstaw do odrzucenia H₀ nie dowodzi, że średnie są równe.
      Mówi tylko, że ta próba nie wystarcza, by wykazać różnicę."),

    lc_p("Wynik testu zawiera statystykę t, niecałkowitą liczbę stopni swobody (znak, że to wersja
      Welcha) i p-wartość. Ile wart jest wynik 12 cm w praktyce, to pytanie
      o ", gloss("wielkość efektu"), ", którym zajmiemy się w rozdziale 10."),

    # ========================================================================
    # WIDGET 2: Test t parowy
    # ========================================================================
    lc_h2("ch6-parowy", "Test t dla prób zależnych (sparowany)"),

    lc_p("Test niezależny zakłada, że w obu grupach są różne osoby. Często
      jednak mierzymy te same osoby dwa razy: przed interwencją i po niej,
      lewą i prawą rękę, ten sam produkt w dwóch laboratoriach. Takie ",
      gloss("próby zależne", "próby zależne"), " nie są dwiema niezależnymi grupami.
      Student, który przed korepetycjami miał wysoki wynik, zwykle ma wysoki
      wynik także po nich. Tę informację wykorzystuje ",
      gloss("test t dla prób zależnych", "test t dla danych sparowanych"), "."),

    lc_p("Pomysł jest prosty. Dla każdej osoby liczymy różnicę
      \\(d_i = x_{\\text{po},i} - x_{\\text{przed},i}\\) i dalej pracujemy już
      tylko na tych różnicach. Hipotezy dotyczą średniej różnicy w populacji
      \\(\\mu_d\\), na przykład \\(H_0: \\mu_d = 0\\) i \\(H_a: \\mu_d \\neq 0\\).
      To jest ", gloss("test t"), " jednej próby z rozdziału 04 z wartością odniesienia
      \\(\\mu_0 = 0\\), policzony na kolumnie różnic:"),

    lc_formula_box(withMathJax(
      "$$t = \\frac{\\bar{d}}{s_d / \\sqrt{n}}, \\qquad df = n - 1$$"
    )),

    lc_p("Tu \\(\\bar{d}\\) i \\(s_d\\) to średnia i odchylenie standardowe
      różnic, a \\(n\\) to liczba par. Panel generuje wyniki studentów przed
      korepetycjami i po nich. Wyniki przed mają średnią 50 pkt i odchylenie
      12 pkt. Wynik po to wynik przed plus efekt ustawiony suwakiem plus losowy
      szum o odchyleniu 8 pkt. Linie łączą dwa pomiary tej samej osoby."),

    figure_panel(
      label = "Ryc. 8.2",
      title = "Test t dla danych sparowanych: przed i po",
      lc_toolbar(
        lc_slider("ch6_paired_n", "Liczba studentów", 10, 50, 25, 5),
        lc_slider("ch6_paired_effect", "Efekt interwencji (pkt)", 0, 15, 5, 1),
        lc_action("ch6_run_paired", "Generuj i testuj", variant = "solid")
      ),
      lc_plot("ch6_paired_plot", max_height = "300px"),
      uiOutput("ch6_paired_result")
    ),

    lc_p("Przy ustawieniach startowych (25 studentów, efekt 5 pkt) różnice mają
      odchylenie standardowe około 8 pkt, więc błąd standardowy średniej
      różnicy to około \\(8 / \\sqrt{25} = 1.6\\) pkt. Efekt 5 pkt to około
      trzech błędów standardowych i test wykrywa go w mniej więcej 85%
      losowań. To jest ", gloss("moc testu", "moc testu"), " z rozdziału 03
      przy tych ustawieniach. Przy efekcie 0 H₀ jest prawdziwa i odrzucamy ją tylko
      w około 5% losowań. Panel liczy różnicę jako po − przed, więc gdy wyniki
      rosną, statystyka t jest dodatnia. Test ma sens tylko wtedy, gdy w danych
      każdemu pomiarowi „przed” odpowiada pomiar „po” tej samej osoby: różnice
      liczymy w parach, a nie między dowolnymi obserwacjami z obu momentów."),

    # ========================================================================
    # WIDGET 3: Sparowane vs. niesparowane — te same dane, inny wynik
    # ========================================================================
    lc_h2("ch6-compare", "Dlaczego sparowanie ma znaczenie?"),

    lc_p("Gdybyśmy dane z poprzedniego panelu potraktowali jak dwie niezależne
      grupy, test porównywałby średnie przed i po, a za zmienność uznałby całe
      zróżnicowanie studentów. Wyniki przed mają odchylenie 12 pkt, wyniki po
      około 14 pkt, więc błąd standardowy różnicy wyniósłby około 3.75 pkt
      zamiast 1.6 pkt. Ten sam efekt 5 pkt dałby t około 1.3 zamiast 3.1
      i zwykle nie byłby istotny. Sparowanie usuwa różnice między osobami,
      bo każda osoba jest porównywana sama ze sobą. Zostaje tylko zmienność
      zmiany. Analiza niesparowana byłaby tu zresztą błędna także formalnie,
      bo pomiary tej samej osoby nie są niezależne."),

    lc_p("Sparowanie chroni też przed drugim problemem: zmianą składu grup
      między pomiarami. Wyobraź sobie badanie, w którym 20 pacjentom zmierzono
      ciśnienie przed nową dietą. Po trzech miesiącach na kontrolę wróciło
      tylko 15 osób. Pięciu pacjentów z najwyższym ciśnieniem wyjściowym się
      nie zgłosiło. Te same dane można przeanalizować na dwa sposoby."),

    tags$ul(
      tags$li(strong("Niesparowane:"),
        " porównujemy 20 pomiarów przed z 15 pomiarami po, jak dwie niezależne grupy."),
      tags$li(strong("Sparowane:"),
        " bierzemy tylko 15 pacjentów z oboma pomiarami i liczymy różnice.")
    ),

    figure_panel(
      label = "Ryc. 8.3",
      title = "Błąd wykruszania próby: sparowane vs. niesparowane na tych samych danych",
      lc_plots(
        tags$div(
          tags$h4("Analiza niesparowana (n₁ = 20, n₂ = 15)"),
          lc_plot("ch6_compare_ind_plot", max_height = "260px"),
          uiOutput("ch6_compare_ind_result")
        ),
        tags$div(
          tags$h4("Analiza sparowana (15 par)"),
          lc_plot("ch6_compare_paired_plot", max_height = "260px"),
          uiOutput("ch6_compare_paired_result")
        )
      )
    ),

    lc_p("Analiza niesparowana pokazuje średnią 148.8 mmHg przed i 140.9 mmHg
      po, czyli spadek o prawie 8 mmHg. Test daje t(30) = 2.17 i p = 0.038,
      więc przy α = 0.05 odrzucamy H₀ i dieta wygląda na skuteczną. Analiza
      sparowana mówi coś innego. U 15 pacjentów, którzy wrócili, ciśnienie
      spadło średnio o 0.9 mmHg (odchylenie różnic 3.9 mmHg), t(14) = -0.86,
      p = 0.405. Nie ma podstaw do odrzucenia H₀."),

    lc_p("Skąd ta rozbieżność? Pięciu nieobecnych pacjentów miało ciśnienie
      wyjściowe od 161 do 178 mmHg. W analizie niesparowanej podnoszą średnią
      przed, ale w grupie po już ich nie ma. Pozorny spadek ciśnienia to
      w większości zmiana składu grupy, a nie efekt diety. Analiza sparowana
      porównuje każdego pacjenta z nim samym, więc odejście tych pięciu osób
      nie tworzy sztucznej różnicy. Ma to swoją cenę: wynik dotyczy tylko
      pacjentów, którzy wrócili, i nic nie mówi o tych z najwyższym ciśnieniem."),

    lc_note("Zasada", rule = TRUE,
      "Gdy te same jednostki zmierzono dwa razy, analizuj różnice w parach.
       Rodzaj testu wynika z planu badania, a nie z tego, który daje mniejszą
       p-wartość."
    ),

    lc_p("Oba warianty testu t opierają się na założeniach. Obserwacje (a w teście
      sparowanym pary) muszą być od siebie niezależne. Średnie, a w teście
      sparowanym średnia różnic, powinny mieć w przybliżeniu ", gloss("rozkład normalny"), ".
      Przy dużych próbach zapewnia to ",
      gloss("centralne twierdzenie graniczne", "centralne twierdzenie graniczne"), ", ale im bardziej skośny rozkład
      i im więcej wartości odstających, tym większej próby potrzeba. Wersja
      Studenta zakłada dodatkowo równe wariancje. Sprawdzaniu tych założeń
      i ", gloss("test nieparametryczny", "testom nieparametrycznym"), ",
      których używa się, gdy założenia zawodzą, poświęcony jest wykład 05."),

    lc_h2("ch6-cas", "Ćwiczenia", "CASchools — test t dwóch grup"),

    lc_p("Na koniec dwa zadania na prawdziwych danych. W obu porównujemy
      średni wynik z czytania między dwiema grupami okręgów szkolnych,
      a grupy są niezależne. Zapisz hipotezy, wykonaj test t Welcha
      i dopiero potem porównaj swój wynik z rozwiązaniem."),

    lc_note("Dane",
      p("420 okręgów szkolnych Kalifornii (1998–1999). Plik: ",
        tags$code("dane/caschools.csv"), "."),
      p("Zmienne w zadaniach: ", tags$code("read"),
        " (wyniki z czytania), ", tags$code("grades"),
        " (typ szkoły: KK-06/KK-08), ",
        tags$code("student_teacher_ratio"), " (STR, liczba uczniów na nauczyciela).")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 6 — Czy typ szkoły różnicuje wyniki z czytania?"),
      p("Okręgi dzielą się na szkoły ", tags$code("KK-06"), " i ", tags$code("KK-08"),
        ". Przetestuj, czy średnie wyniki ", tags$code("read"),
        " różnią się między grupami. Wykonaj test t dla prób niezależnych.
        Zapisz: t, df, p. Czy różnica jest istotna?"),
      lc_more("Rozwiązanie", uiOutput("cas_ch6_sol6"))
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 7 — Duże klasy czy małe — czy STR ma znaczenie?"),
      p("Utwórz zmienną binarną: ",
        tags$code("high_str = (student_teacher_ratio > 20)"),
        ". Porównaj wyniki ", tags$code("read"),
        " między okręgami z dużym (STR > 20) i małym (STR ≤ 20) stosunkiem.
        Czy różnica jest istotna? Jak duże jest przesunięcie w punktach?
        Skąd może wynikać ta różnica?"),
      lc_more("Rozwiązanie", uiOutput("cas_ch6_sol7"))
    ),

    lc_p("Istotny wynik testu mówi tylko, że różnica średnich w populacji
      najpewniej nie jest zerowa. Nie mówi, czy jest duża, ani skąd się bierze.
      Przy dużych próbach nawet niewielka różnica może dać małą p-wartość,
      a grupy okręgów mogą różnić się także innymi cechami, na przykład
      zamożnością. Pierwszym problemem zajmiemy się w rozdziale 10, drugim
      w wykładzie o regresji."),

    lc_p("Test t porównuje dokładnie dwie grupy. Gdy grup jest więcej,
      kusi, by porównać je parami kilkoma testami t. Następny rozdział pokazuje,
      dlaczego to zły pomysł i jak porównać wszystkie średnie jednym testem."),

    lc_chapter_next(
      num       = "09",
      title     = "ANOVA",
      lead      = "trzy grupy i więcej porównane jednym testem",
      target_id = "ch-anova"
    )
  )
)

# ============================================================================
# DANE — CASchools (wczytane raz przy ładowaniu modułu)
# ============================================================================

.ch6_cas <- read.csv(file.path(app_dir, "dane", "caschools.csv"),
                     stringsAsFactors = FALSE)

# Statyczne dane do widgetu porównania sparowany vs. niesparowany (Ryc. 8.3)
.ch6_compare <- local({
  przed_paired  <- c(132, 138, 145, 141, 136, 152, 143, 139,
                     147, 134, 148, 141, 137, 144, 150)
  po_paired     <- c(130, 140, 142, 143, 127, 154, 142, 144,
                     140, 133, 145, 142, 135, 149, 148)
  przed_dropout <- c(164, 170, 175, 161, 178)

  pairs <- data.frame(id = 1:15, przed = przed_paired, po = po_paired)

  ind_data <- data.frame(
    wartosc = c(przed_paired, przed_dropout, po_paired),
    grupa   = factor(
      c(rep("Przed", 20), rep("Po", 15)),
      levels = c("Przed", "Po")
    ),
    typ = c(rep("para", 15), rep("dropout", 5), rep("para", 15))
  )

  long_pairs <- pivot_longer(pairs, cols = c(przed, po),
                             names_to = "moment", values_to = "cisnienie") %>%
    mutate(moment = factor(moment,
                           levels = c("przed", "po"),
                           labels = c("Przed", "Po")))

  list(pairs = pairs, long_pairs = long_pairs, ind_data = ind_data)
})

# ============================================================================
# SERVER
# ============================================================================

ch6_server <- function(input, output, session) {

  # Shared independent data
  ch6_ind_data_state <- reactiveVal(NULL)
  ch6_ind_data <- reactive({
    state <- ch6_ind_data_state()
    if (is.null(state)) return(NULL)
    req(input$ch6_ind_var, input$ch6_ind_n)

    if (!identical(state$var, input$ch6_ind_var) ||
        !isTRUE(state$n_per_group == input$ch6_ind_n)) {
      return(NULL)
    }

    state$data
  })

  # Krok widgetu (1..4) żyje w przeglądarce; nowa próba ani zmiana
  # zmiennej nie cofa kroku.
  ch6_ind_step <- lc_step_server("ch6_ind", input)$step

  observeEvent(input$ch6_run_ind_t, {
    req(input$ch6_ind_var, input$ch6_ind_n)
    n <- input$ch6_ind_n
    data <- generate_student_data(n * 2, equal_sex = TRUE)
    ch6_ind_data_state(list(
      var = input$ch6_ind_var,
      n_per_group = n,
      data = data
    ))
  }, ignoreInit = TRUE)

  # Shared paired data
  ch6_paired_data_state <- reactiveVal(NULL)
  ch6_paired_data <- reactive({
    state <- ch6_paired_data_state()
    if (is.null(state)) return(NULL)
    req(input$ch6_paired_n, input$ch6_paired_effect)

    if (!isTRUE(state$n == input$ch6_paired_n) ||
        !isTRUE(state$effect == input$ch6_paired_effect)) {
      return(NULL)
    }

    state$data
  })

  observeEvent(input$ch6_run_paired, {
    req(input$ch6_paired_n, input$ch6_paired_effect)
    ch6_paired_data_state(list(
      n = input$ch6_paired_n,
      effect = input$ch6_paired_effect,
      data = generate_paired_data(input$ch6_paired_n, input$ch6_paired_effect)
    ))
  }, ignoreInit = TRUE)

  # --- Widget 1: Test t niezależny ---
  ch6_ind_var_label <- function(var) {
    switch(var,
      "wzrost" = "Wzrost (cm)",
      "waga" = "Waga (kg)",
      "srednia_ocen" = "Średnia ocen",
      "czas_dojazdu" = "Czas dojazdu (min)",
      var
    )
  }

  # Nazwa zmiennej do zdań („średnia zmiennej „wzrost”…”) — bez kłopotów z rodzajem.
  ch6_ind_var_name <- function(var) {
    switch(var,
      "wzrost" = "wzrost",
      "waga" = "waga",
      "srednia_ocen" = "średnia ocen",
      "czas_dojazdu" = "czas dojazdu",
      var
    )
  }

  ch6_ind_stats <- reactive({
    data <- ch6_ind_data()
    req(data)
    var <- input$ch6_ind_var
    formula <- as.formula(paste(var, "~ plec"))
    tidy_res <- as.data.frame(rstatix::t_test(data, formula))
    means <- data %>%
      dplyr::group_by(plec) %>%
      dplyr::summarise(
        n = dplyr::n(),
        m = mean(.data[[var]], na.rm = TRUE),
        s = sd(.data[[var]], na.rm = TRUE),
        .groups = "drop"
      )
    list(test = tidy_res, means = means)
  })

  output$ch6_ind_hypothesis <- renderUI({
    var_name <- ch6_ind_var_name(input$ch6_ind_var)
    lc_formula_box(
      p(tags$b("Hipoteza formalna (dwustronna):")),
      p(withMathJax("\\(H_0: \\mu_{K} = \\mu_{M}\\)"),
        " — średnia zmiennej „", var_name, "” jest taka sama u kobiet i mężczyzn."),
      p(withMathJax("\\(H_a: \\mu_{K} \\neq \\mu_{M}\\)"),
        " — średnia zmiennej „", var_name, "” różni się między grupami.")
    )
  })

  zoom_plot_server("ch6_ind_boxplot", reactive({
    data <- ch6_ind_data()
    if (is.null(data)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Losuj próbę”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      var <- input$ch6_ind_var
      var_label <- ch6_ind_var_label(var)
      step <- ch6_ind_step()

      if (step <= 2) {
        # Pierwsza grupa w roli danych, druga w roli grupy; rama z danych.
        groups <- sort(unique(data$plec))
        roles <- c("data", "group")
        y_rng <- range(data[[var]], na.rm = TRUE)
        y_pad <- diff(y_rng) * 0.08
        jitter <- position_jitter(width = 0.15, height = 0, seed = 1)

        p <- ggplot(data, aes(x = plec, y = .data[[var]]))
        for (i in seq_along(groups)) {
          sub <- data[data$plec == groups[i], ]
          p <- p +
            step_result(geom_boxplot, data = sub, width = 0.5, alpha = 0.35,
                        outlier.shape = NA,
                        fill = STEP_ROLES[[roles[i]]]$colour) +
            step_layer(geom_point, roles[i], data = sub, position = jitter,
                       size = 1.5, alpha = 0.5)
        }

        if (step >= 2) {
          means <- ch6_ind_stats()$means
          means$hj <- ifelse(seq_len(nrow(means)) == 1, 1.15, -0.15)
          p <- p +
            step_layer(geom_point, "new", data = means,
                       mapping = aes(x = plec, y = m), inherit.aes = FALSE,
                       shape = 23, size = 4, fill = "white", stroke = 1.2) +
            geom_text(
              data = means,
              aes(x = plec, y = m, label = paste0("bar(x) == ", lc_fmt(m, 1)), hjust = hj),
              inherit.aes = FALSE, parse = TRUE,
              nudge_y = diff(y_rng) * 0.04,
              colour = STEP_ROLES$new$colour,
              fontface = "bold"
            )
        }
        p +
          labs(x = "Płeć", y = var_label) +
          step_frame(xlim = c(0.4, length(groups) + 0.6),
                     ylim = y_rng + c(-y_pad, y_pad))
      } else {
        st <- ch6_ind_stats()
        step_null_plot(st$test$statistic, df = st$test$df, type = "t",
                       phase = if (step == 3) "stat" else "decision")
      }
    }
  }))

  output$ch6_ind_text <- renderUI({
    data <- ch6_ind_data()
    step <- ch6_ind_step()
    if (is.null(data)) return(NULL)

    var <- input$ch6_ind_var
    var_name <- ch6_ind_var_name(var)
    st <- ch6_ind_stats()
    tidy_res <- st$test
    means <- st$means
    higher <- means$plec[which.max(means$m)]
    lower <- means$plec[which.min(means$m)]
    diff_val <- lc_fmt(max(means$m) - min(means$m), 2)

    switch(as.character(step),
      "1" = tagList(
        "n = ", step_num(nrow(data)), " osób łącznie. Każdy punkt to jedna osoba.
        Najpierw patrzymy, czy grupy wizualnie wyglądają na przesunięte względem siebie."
      ),
      "2" = tagList(
        "Różnica średnich w próbie wynosi ", step_num(diff_val),
        ". Test pyta, czy taka różnica jest duża względem zmienności w grupach."
      ),
      "3" = tagList(
        "t = ", step_num(lc_fmt(tidy_res$statistic, 3)),
        paste0(" (df Welcha = ", lc_fmt(tidy_res$df, 1), "). "),
        "Tyle błędów standardowych dzieli średnie obu grup (kobiety − mężczyźni)."
      ),
      "4" = tagList(
        paste0("Wynik testu t niezależnego: t(", lc_fmt(tidy_res$df, 1), ") = "),
        step_num(lc_fmt(tidy_res$statistic, 3)), ". ", step_verdict(tidy_res$p), " ",
        tags$strong("Werdykt:", .noWS = "outside"),
        if (tidy_res$p < 0.05) {
          tagList(
            " średnia zmiennej „", var_name, "” różni się istotnie między grupami — ",
            "w próbie była wyższa w grupie ", as.character(higher),
            " niż ", as.character(lower), " o ", step_num(diff_val), "."
          )
        } else {
          tagList(
            " nie ma podstaw, by twierdzić, że średnia zmiennej „", var_name,
            "” różni się między grupami. Obserwowana w próbie różnica ",
            step_num(diff_val), " (na korzyść grupy ", as.character(higher),
            ") mieści się w zakresie wahań losowych."
          )
        }
      )
    )
  })

  # Tabela średnich pod wykresem (krok 2)
  output$ch6_ind_table <- renderUI({
    data <- ch6_ind_data()
    if (is.null(data) || ch6_ind_step() != 2) return(NULL)
    means <- ch6_ind_stats()$means
    lc_table(
      data.frame(group = as.character(means$plec), n = means$n,
                 m = means$m, s = means$s),
      cols = list(
        lc_col("group", "Grupa", "row"),
        lc_col("n", "n"),
        lc_col("m", "x̄", digits = 2),
        lc_col("s", "s", digits = 2)
      )
    )
  })

  # --- Widget 2: Test t parowy ---
  zoom_plot_server("ch6_paired_plot", reactive({
    data <- ch6_paired_data()
    if (is.null(data)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Generuj i testuj”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      # Connected dot plot
      long <- data %>%
        pivot_longer(cols = c(wynik_przed, wynik_po),
                     names_to = "moment", values_to = "wynik") %>%
        mutate(moment = factor(moment,
                               levels = c("wynik_przed", "wynik_po"),
                               labels = c("Przed", "Po")))

      ggplot(long, aes(x = moment, y = wynik)) +
        geom_line(aes(group = student), alpha = 0.3, color = col_paired) +
        geom_point(aes(color = moment), size = 2.5, alpha = 0.7) +
        scale_color_manual(values = c(col_h0, col_reject)) +
        labs(
             x = "Moment", y = "Wynik") +
                theme(legend.position = "none")
    }
  }))

  output$ch6_paired_result <- renderUI({
    data <- ch6_paired_data()
    if (is.null(data)) return(NULL)

    long <- data %>%
      pivot_longer(cols = c(wynik_przed, wynik_po),
                   names_to = "moment", values_to = "wynik")
    # Kolejność poziomów: t liczone dla różnicy po − przed, jak w tabeli.
    long$moment <- factor(long$moment,
                          levels = c("wynik_po", "wynik_przed"))

    result <- rstatix::t_test(long, wynik ~ moment, paired = TRUE)
    tidy_res <- as.data.frame(result)

    mean_diff <- mean(data$wynik_po - data$wynik_przed)
    res <- format_test_result(tidy_res$p)
    direction <- if (mean_diff > 0) "wzrosły" else if (mean_diff < 0) "spadły" else "nie zmieniły się"

    lc_status(
      p(tags$strong("Wynik testu t dla danych sparowanych:")),
      p(paste0("Średnia różnica (po − przed): ", round(mean_diff, 2), " pkt")),
      p(paste0("t(", tidy_res$df, ") = ", round(tidy_res$statistic, 3))),
      ui_p_value(tidy_res$p),
      p(lc_verdict(tags$strong(res$decision), type = res$verdict)),
      if (tidy_res$p < 0.05) {
        p(tags$strong("Werdykt:"),
          " wyniki istotnie się zmieniły — średnio ", direction,
          " o ", round(abs(mean_diff), 2), " pkt.")
      } else {
        p(tags$strong("Werdykt:"),
          " nie ma podstaw, by twierdzić, że wyniki się zmieniły. ",
          "Obserwowana w próbie zmiana (", round(mean_diff, 2),
          " pkt) mieści się w zakresie wahań losowych.")
      }
    )
  })

  # --- Widget: porównanie sparowany vs. niesparowany (Ryc. 8.3) ---

  zoom_plot_server("ch6_compare_ind_plot", reactive({
    d <- .ch6_compare$ind_data
    d$kolor <- factor(
      ifelse(d$typ == "dropout", "Brak kontroli (n = 5)", as.character(d$grupa)),
      levels = c("Przed", "Po", "Brak kontroli (n = 5)")
    )
    means <- d %>% group_by(grupa) %>% summarise(m = mean(wartosc), .groups = "drop")

    ggplot(d, aes(x = grupa, y = wartosc)) +
      geom_boxplot(aes(fill = grupa), alpha = 0.35, outlier.alpha = 0,
                   width = 0.45, color = "grey40") +
      geom_jitter(aes(color = kolor, shape = kolor),
                  width = 0.12, alpha = 0.8, size = 2.2) +
      geom_text(data = means,
                aes(y = m, label = paste0("bar(x) == ", round(m, 1)),
                    hjust = ifelse(as.integer(grupa) == 1, 1.15, -0.15)),
                parse = TRUE, nudge_y = 2, color = upwr_secondary, fontface = "bold", size = 3.5) +
      scale_fill_manual(values = c("Przed" = col_h0, "Po" = col_reject)) +
      scale_color_manual(values = c("Przed"             = col_h0,
                                    "Po"                = col_reject,
                                    "Brak kontroli (n = 5)" = "#8B1A1A")) +
      scale_shape_manual(values = c("Przed" = 16, "Po" = 16,
                                    "Brak kontroli (n = 5)" = 17)) +
      labs(x = NULL, y = "Ciśnienie skurczowe (mmHg)", color = NULL, shape = NULL) +
      guides(fill = "none") +
      theme(legend.position = "bottom", legend.text = element_text(size = 9))
  }))

  output$ch6_compare_ind_result <- renderUI({
    d <- .ch6_compare$ind_data
    result <- rstatix::t_test(d, wartosc ~ grupa)
    res <- format_test_result(result$p)
    smry <- d %>% group_by(grupa) %>%
      summarise(n = n(), m = round(mean(wartosc), 1), s = round(sd(wartosc), 1),
                .groups = "drop")
    tagList(
      lc_table(
        data.frame(group = as.character(smry$grupa), n = smry$n, m = smry$m, s = smry$s),
        cols = list(
          lc_col("group", "Grupa", "row"),
          lc_col("n", "n"),
          lc_col("m", "x̄", digits = 1),
          lc_col("s", "s", digits = 1)
        )
      ),
      p(paste0("t(", round(result$df, 0), ") = ", round(result$statistic, 3))),
      ui_p_value(result$p),
      p(lc_verdict(tags$strong(res$decision), type = res$verdict))
    )
  })

  zoom_plot_server("ch6_compare_paired_plot", reactive({
    long <- .ch6_compare$long_pairs
    ggplot(long, aes(x = moment, y = cisnienie)) +
      geom_line(aes(group = id), alpha = 0.3, color = col_paired) +
      geom_point(aes(color = moment), size = 2.5, alpha = 0.8) +
      scale_color_manual(values = c("Przed" = col_h0, "Po" = col_reject)) +
      labs(x = NULL, y = "Ciśnienie skurczowe (mmHg)") +
      theme(legend.position = "none")
  }))

  output$ch6_compare_paired_result <- renderUI({
    # t dla różnicy po − przed, zgodnie z tabelą.
    long <- .ch6_compare$long_pairs %>%
      mutate(moment = factor(moment, levels = c("Po", "Przed")))
    result <- rstatix::t_test(long, cisnienie ~ moment, paired = TRUE)
    res <- format_test_result(result$p)
    diffs <- .ch6_compare$pairs$po - .ch6_compare$pairs$przed
    tagList(
      lc_table(
        data.frame(
          c1 = c("po − przed"),
          c2 = I(list(
            15
          )),
          c3 = I(list(
            round(mean(diffs), 2)
          )),
          c4 = I(list(
            round(sd(diffs), 2)
          ))
        ),
        cols = list(
          lc_col("c1", "Miara", "row"),
          lc_col("c2", "n", "text"),
          lc_col("c3", "d̄", "num"),
          lc_col("c4", "s_d", "num")
        ),
        narrow = "cards"
      ),
      p(paste0("t(14) = ", round(result$statistic, 3))),
      ui_p_value(result$p),
      p(lc_verdict(tags$strong(res$decision), type = res$verdict))
    )
  })

  # --- Ćwiczenia CASchools ---

  .cas_t2samp <- function(x, grp) {
    grp <- as.factor(grp); lvls <- levels(grp)
    x1 <- x[grp == lvls[1]]; x2 <- x[grp == lvls[2]]
    n1 <- length(x1); n2 <- length(x2)
    m1 <- mean(x1); m2 <- mean(x2); s1 <- sd(x1); s2 <- sd(x2)
    se <- sqrt(s1^2/n1 + s2^2/n2)
    t_val <- (m1 - m2) / se
    df <- (s1^2/n1 + s2^2/n2)^2 /
          ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
    p_val <- 2 * pt(-abs(t_val), df)
    sp <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2))
    d <- (m1 - m2) / sp
    list(lvls=lvls, n1=n1, n2=n2, m1=m1, m2=m2, s1=s1, s2=s2,
         t=t_val, df=df, p=p_val, d=d)
  }

  output$cas_ch6_sol6 <- renderUI({
    df2 <- .ch6_cas[!is.na(.ch6_cas$read) & !is.na(.ch6_cas$grades), ]
    r <- .cas_t2samp(df2$read, df2$grades)
    tagList(
      p(tags$b("H₀:"), " μ(KK-06) = μ(KK-08) · ",
        tags$b("Hₐ:"), " μ(KK-06) ≠ μ(KK-08)"),
      tags$ul(
        tags$li(sprintf("%s: n = %d, x̄ = %.2f, s = %.2f",
                        r$lvls[1], r$n1, r$m1, r$s1)),
        tags$li(sprintf("%s: n = %d, x̄ = %.2f, s = %.2f",
                        r$lvls[2], r$n2, r$m2, r$s2)),
        tags$li(sprintf("t(%s) = %.3f, p %s %s",
          round(r$df, 1), r$t,
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
      ),
      lc_verdict(tags$strong(if (r$p < 0.05) "Odrzucamy H₀" else "Brak podstaw do odrzucenia H₀"),
                 type = if (r$p < 0.05) "danger" else "ok"),
      p(tags$b("Interpretacja:"), " ",
        sprintf(
          "Różnica %.2f pkt jest %s (p %s 0.05).",
          abs(r$m1 - r$m2),
          if (r$p < 0.05) "istotna" else "nieistotna",
          if (r$p < 0.05) "<" else ">"
        ))
    )
  })

  output$cas_ch6_sol7 <- renderUI({
    high_str <- .ch6_cas$student_teacher_ratio > 20
    r <- .cas_t2samp(.ch6_cas$read, high_str)
    m_lo <- .ch6_cas$read[!high_str]; m_hi <- .ch6_cas$read[high_str]
    tagList(
      p(tags$b("H₀:"), " μ(STR ≤ 20) = μ(STR > 20) · ",
        tags$b("Hₐ:"), " μ(STR ≤ 20) ≠ μ(STR > 20)"),
      tags$ul(
        tags$li(sprintf("STR ≤ 20: n = %d, x̄ = %.2f",
                        sum(!high_str), mean(m_lo))),
        tags$li(sprintf("STR > 20: n = %d, x̄ = %.2f",
                        sum(high_str), mean(m_hi))),
        tags$li(sprintf("Różnica: %.2f pkt",
                        mean(m_lo) - mean(m_hi))),
        tags$li(sprintf("t(%s) = %.3f, p %s %s",
          round(r$df, 1), r$t,
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
      ),
      lc_verdict(tags$strong(if (r$p < 0.05) "Odrzucamy H₀" else "Brak podstaw do odrzucenia H₀"),
                 type = if (r$p < 0.05) "danger" else "ok"),
      p(tags$b("Uwaga:"),
        " STR > 20 mają często okręgi biedniejsze. Część różnicy może wynikać
        z dochodu, który działa tu jak zmienna zakłócająca. Żeby to sprawdzić,
        potrzeba regresji, która uwzględnia dochód obok STR.")
    )
  })

}
