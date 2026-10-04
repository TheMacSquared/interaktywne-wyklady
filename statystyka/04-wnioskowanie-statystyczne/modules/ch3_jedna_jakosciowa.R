# ============================================================================
# CHAPTER 5: Jedna zmienna jakościowa — test dwumianowy
# ============================================================================

ch3_ui <- list(
  id = "ch-jedna-jakosciowa", num = "05", title = "Test proporcji",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 05 · Testowanie hipotez",
      num    = "05",
      title  = "Test proporcji.",
      lead   = "Odsetek zdających, udział wadliwych sztuk, kiełkowalność nasion:
                wiele hipotez dotyczy proporcji, a nie średniej. Liczba sukcesów
                w próbie ma wtedy znany rozkład, więc p-wartość można policzyć
                dokładnie, bez przybliżenia normalnego."
    ),

    lc_p("Test t z poprzedniego rozdziału sprawdzał hipotezę o średniej zmiennej
      ilościowej. Wiele pytań dotyczy jednak zmiennej o dwóch kategoriach:
      próbka wody spełnia normę albo nie, student zdał albo nie zdał, produkt
      jest wadliwy albo sprawny. Parametrem populacji jest wtedy proporcja
      \\(p\\), czyli odsetek sukcesów, a jej estymatorem ",
      gloss("proporcja z próby", "proporcja z próby"), " \\(\\hat{p} = k/n\\),
      gdzie \\(k\\) to liczba sukcesów w próbie liczącej \\(n\\) obserwacji.
      W wykładzie 03 budowaliśmy dla \\(p\\) przedział ufności. Teraz pytanie
      brzmi inaczej: czy \\(p\\) jest równe konkretnej wartości referencyjnej
      \\(p_0\\), na przykład deklaracji producenta, normie albo wartości
      historycznej."),

    # ========================================================================
    # Wprowadzenie
    # ========================================================================
    lc_h2("ch3-pytanie", "Od pytania do testu dwumianowego"),

    lc_p("Hipotezy zapisujemy tak samo jak w teście t, tylko w miejscu
      \\(\\mu_0\\) stoi \\(p_0\\). Wariant wynika z brzmienia pytania i, jak
      w rozdziale 02, ustala się go przed zebraniem danych."),

    lc_formula_box(
      p(strong("Dwustronna"), " (proporcja różni się od ",
        withMathJax("\\(p_0\\)"), "):"),
      p(withMathJax("\\(H_0: p = p_0 \\quad\\)"),
        withMathJax("\\(H_a: p \\neq p_0\\)"))
    ),
    lc_formula_box(
      p(strong("Prawostronna"), " (proporcja wyższa niż ",
        withMathJax("\\(p_0\\)"), "):"),
      p(withMathJax("\\(H_0: p \\leq p_0 \\quad\\)"),
        withMathJax("\\(H_a: p > p_0\\)"))
    ),
    lc_formula_box(
      p(strong("Lewostronna"), " (proporcja niższa niż ",
        withMathJax("\\(p_0\\)"), "):"),
      p(withMathJax("\\(H_0: p \\geq p_0 \\quad\\)"),
        withMathJax("\\(H_a: p < p_0\\)"))
    ),

    lc_p("Do oceny hipotezy potrzebujemy ",
      gloss("statystyka testowa", "statystyki testowej"), " i jej rozkładu
      przy prawdziwej H₀. W teście t średnią trzeba było standaryzować
      i sięgać po rozkład t. Tu jest prościej: statystyką testową jest sama
      liczba sukcesów \\(k\\). Z wykładu 02 wiemy, że jeśli każda z \\(n\\)
      niezależnych obserwacji jest sukcesem z prawdopodobieństwem \\(p_0\\),
      liczba sukcesów \\(K\\) ma ", gloss("rozkład dwumianowy"),
      " \\(B(n, p_0)\\) z wartością oczekiwaną \\(E(K) = np_0\\). Ten rozkład
      znamy dokładnie, więc prawdopodobieństwo każdego możliwego wyniku przy
      prawdziwej H₀ możemy po prostu policzyć."),

    lc_formula_box(withMathJax(
      "$$P(K = j) = \\binom{n}{j}\\, p_0^{\\,j} (1-p_0)^{n-j}, \\qquad j = 0, 1, \\ldots, n$$"
    )),

    lc_p(gloss("p-wartość", "P-wartość"), " to prawdopodobieństwo, że przy
      prawdziwej H₀ próba da wynik co najmniej tak skrajny jak obserwowany.
      W teście dwustronnym za co najmniej tak skrajne uznajemy wszystkie
      wyniki, które przy H₀ są nie bardziej prawdopodobne niż nasze \\(k\\),
      po obu stronach rozkładu. P-wartość jest sumą ich prawdopodobieństw."),

    lc_formula_box(withMathJax(
      "$$p = \\sum_{j:\\ P(K = j)\\, \\leq\\, P(K = k)} P(K = j)$$"
    )),

    lc_p("Tak zbudowany ", gloss("test dwumianowy"), " jest testem dokładnym:
      p-wartość pochodzi wprost z rozkładu dwumianowego, a nie z przybliżenia
      normalnego. Dlatego działa także przy małych próbach i przy proporcjach
      bliskich 0 lub 1, czyli tam, gdzie w wykładzie 03 zawodził przedział
      Walda. Jak zawsze, p-wartość mówi, jak nietypowe byłyby nasze dane,
      gdyby H₀ była prawdziwa. Nie jest prawdopodobieństwem, że H₀ jest
      prawdziwa."),

    # ========================================================================
    # Ćwiczenie: sformułuj hipotezy
    # ========================================================================
    lc_h2("ch3-cwiczenie", "Ćwiczenie: sformułuj hipotezy"),

    lc_p("Zanim policzymy pierwszy test, warto przećwiczyć krok, od którego
      wszystko się zaczyna: zamiana pytania potocznego na parę hipotez
      o \\(p\\). W każdym przykładzie zdecyduj, czy pytanie wskazuje kierunek,
      zapisz H₀ i Hₐ, a dopiero potem odsłoń odpowiedź."),

    hypothesis_practice("ch3", list(
      list(
        question = "Producent deklaruje, że 80% słoików jego dżemu spełnia
                    wymóg minimalnej zawartości owoców. Kontrola sprawdza,
                    czy ten odsetek się zgadza.",
        h0 = "\\(H_0: p = 0.80\\)",
        ha = "\\(H_a: p \\neq 0.80\\)",
        note = "Dwustronny — interesuje nas każde odchylenie od deklaracji."
      ),
      list(
        question = "W standardowej produkcji 3% opakowań jest wadliwych.
                    Sprawdzamy, czy nowa linia produkcyjna generuje więcej braków.",
        h0 = "\\(H_0: p \\leq 0.03\\) (nie gorzej niż standard)",
        ha = "\\(H_a: p > 0.03\\) (więcej wadliwych)",
        note = "Jednostronny (prawostronny) — pytamy tylko o pogorszenie."
      ),
      list(
        question = "Rolnik twierdzi, że kiełkuje mu co najmniej 90% nasion.
                    Chcemy sprawdzić, czy ta deklaracja jest prawdziwa
                    (z perspektywy klienta, który ryzykuje zakup słabszych nasion).",
        h0 = "\\(H_0: p \\geq 0.90\\)",
        ha = "\\(H_a: p < 0.90\\)",
        note = "Jednostronny (lewostronny) — klienta martwi tylko to, że jest gorzej."
      )
    )),

    lc_p("We wszystkich trzech przykładach H₀ zawiera znak równości,
      a wartość \\(p_0\\) pochodzi spoza danych: z deklaracji, ze standardu
      produkcji albo z obietnicy sprzedawcy. Kierunek Hₐ wynika z tego, które
      odchylenie ma dla pytającego konsekwencje."),

    # ========================================================================
    # WIDGET 1: Test dwumianowy dwustronny (krokowy)
    # ========================================================================
    lc_h2("ch3-krok", "Test dwumianowy — krok po kroku"),

    lc_p("Panel przeprowadza test dwumianowy na symulowanych danych. Każdy
      scenariusz ma wartość referencyjną \\(p_0\\) i prawdziwy odsetek,
      z którego losowana jest próba. W domyślnym scenariuszu jakości wody
      \\(p_0 = 0.8\\), a próbki pochodzą z populacji, w której normę spełnia
      85% z nich. H₀ jest więc fałszywa, ale to, czy próba to pokaże, zależy
      od losowania i od \\(n\\). Kolejne kroki pokazują dane, rozkład
      \\(B(n, p_0)\\) przy prawdziwej H₀ i wyniki składające się na
      p-wartość."),

    figure_panel(
      label = "Ryc. 5.1",
      title = "Test dwumianowy — krok po kroku",
      uiOutput("ch3_hypothesis_panel"),
      lc_step_widget("ch3_test",
        steps = c("Dane", "Rozkład pod H₀", "p-wartość i decyzja"),
        toolbar = lc_toolbar(
          selectInput("ch3_scenario", "Scenariusz",
            choices = c(
              "Jakość wody (p₀ = 80%)" = "water_quality",
              "Zdawalność egzaminu (p₀ = 60%)" = "exam_pass",
              "Kiełkowalność nasion (p₀ = 90%)" = "germination",
              "Produkty poza normą (p₀ = 3%)" = "defects",
              "Używanie kasków na budowie (p₀ = 95%, IB)" = "helmets"
            ),
            selected = "water_quality"
          ),
          lc_slider("ch3_n", "Wielkość próby (n)", 20, 200, 50, 10),
          lc_action("ch3_new_sample", "Losuj próbę", icon = "shuffle", variant = "solid")
        ),
        plot_id = "ch3_step_plot"
      )
    ),

    lc_p("Przy \\(n = 50\\) i \\(p_0 = 0.8\\) rozkład z kroku 2 ma środek
      w \\(np_0 = 40\\) sukcesach, a jego odchylenie standardowe wynosi
      \\(\\sqrt{50 \\cdot 0.8 \\cdot 0.2} \\approx 2.8\\). Prawdziwy
      odsetek 85% daje średnio 42.5 sukcesu, czyli niecałe jedno odchylenie
      od środka. Typowa próba trafia więc w gęstą część rozkładu: dla
      \\(k = 43\\) dwustronna p-wartość wynosi 0.38, a poniżej 0.05 spada
      dopiero od \\(k = 46\\) (0.033) w górę albo od \\(k = 34\\) w dół.
      Takie wyniki zdarzają się przy prawdziwym odsetku 85% w około 11% prób.
      W pozostałych decyzja brzmi „brak podstaw do odrzucenia H₀”, chociaż
      H₀ jest fałszywa."),

    lc_p("Ten przykład dobrze pokazuje, czego brak odrzucenia nie oznacza.
      Nie dowodzi, że \\(p = 0.8\\). Mówi tylko, że 50 próbek nie wystarcza,
      by odróżnić 80% od 85%. Przy \\(n = 200\\) ten sam test wykrywa różnicę
      w około 39% prób, bo rozkład \\(\\hat{p}\\) zwęża się wraz z \\(n\\).
      To ", gloss("moc testu"), " z rozdziału 03. Próg \\(\\alpha = 0.05\\)
      jest przy tym umową ustaloną przed analizą, podobnie jak poziom
      ufności 95% w wykładzie 03."),

    lc_p("Wynik testu warto zestawić z 95-procentowym przedziałem
      Cloppera-Pearsona z wykładu 03, który dla tej próby wynosi od 0.73
      do 0.94. Obejmuje on \\(p_0 = 0.8\\), co zgadza się z decyzją testu:
      wartość, której przedział nie wyklucza, nie zostaje odrzucona."),

    # ========================================================================
    # WIDGET 2: Test dwumianowy jednostronny (te same dane)
    # ========================================================================
    lc_h2("ch3-jednostronny", "A jeśli znamy kierunek?"),

    lc_p("Pytanie z panelu było dwustronne: czy odsetek różni się od 80%.
      Często jednak liczy się tylko jeden kierunek. Instytucję chwalącą się
      jakością wody interesuje, czy jest lepiej niż 80%, a klienta kupującego
      nasiona, czy jest gorzej niż obiecane 90%. Jak w teście t, kierunek
      wynika z pytania, a nie z danych. Wtedy ", gloss("test jednostronny"),
      " liczy p-wartość tylko w jednym ogonie: jako prawdopodobieństwo wyniku
      co najmniej tak skrajnego w kierunku Hₐ."),

    lc_formula_box(withMathJax(
      "$$H_a: p > p_0: \\ \\ p = P(K \\geq k) \\qquad\\qquad H_a: p < p_0: \\ \\ p = P(K \\leq k)$$"
    )),

    lc_p("Panel używa tej samej próby co test dwustronny powyżej. Zmienia się
      tylko pytanie, a razem z nim zbiór wyników uznanych za skrajne."),

    figure_panel(
      label = "Ryc. 5.2",
      title = "Test dwumianowy jednostronny",
      uiOutput("ch3b_hypothesis_panel"),
      lc_step_widget("ch3b_test",
        steps = c("Dane", "Rozkład pod H₀", "p-wartość i decyzja"),
        toolbar = lc_toolbar(
          lc_caption("Dane: te same co w teście dwustronnym powyżej.")
        ),
        plot_id = "ch3b_step_plot"
      )
    ),

    lc_p("Weźmy próbę, w której normę spełnia 45 z 50 próbek wody. Test
      dwustronny daje p = 0.079, więc nie odrzuca H₀. Prawostronny,
      z Hₐ: \\(p > 0.8\\), daje p = 0.048 i odrzuca. Te same dane prowadzą
      do różnych decyzji, bo odpowiadają na różne pytania. Jednostronna
      p-wartość nie jest tu dokładnie połową dwustronnej. Rozkład
      \\(B(50;\\ 0.8)\\) jest lewoskośny, a test dwustronny dolicza
      z lewego ogona wyniki nie bardziej prawdopodobne niż \\(k = 45\\),
      a nie ich lustrzane odbicie."),

    lc_p("Przewaga testu jednostronnego ma swoją cenę, omówioną w rozdziale 02.
      Test z Hₐ: \\(p > 0.8\\) nie zauważy odsetka wyraźnie niższego niż 80%.
      A jeśli kierunek wybiera się po obejrzeniu danych, prawdopodobieństwo
      fałszywego alarmu przekracza deklarowane α."),

    # ========================================================================
    # WIDGET 3: Porównanie — test dwumianowy vs test proporcji
    # ========================================================================
    lc_h2("ch3-porownanie", "Test dwumianowy a test proporcji"),

    lc_p("Wiele podręczników i programów zamiast testu dwumianowego podaje
      test proporcji, nazywany też z-testem. Zastępuje on rozkład dwumianowy
      ", gloss("rozkład normalny", "rozkładem normalnym"), ", czyli korzysta
      z tego samego przybliżenia, na którym w wykładzie 03 opierał się
      ", gloss("przedział Walda"), ". Statystyka testowa mierzy, o ile
      błędów standardowych \\(\\hat{p}\\) odbiega od \\(p_0\\)."),

    lc_formula_box(withMathJax(
      "$$z = \\frac{\\hat{p} - p_0}{\\sqrt{p_0(1-p_0)/n}}$$"
    )),

    lc_p("W odróżnieniu od przedziału Walda błąd standardowy liczymy z \\(p_0\\),
      a nie z \\(\\hat{p}\\), bo p-wartość wyznacza się przy założeniu, że H₀
      jest prawdziwa. Przy prawdziwej H₀ statystyka \\(z\\) ma w przybliżeniu ",
      gloss("standardowy rozkład normalny"), ", więc dwustronna p-wartość to
      \\(P(|Z| \\geq |z|)\\). Panel zestawia oba testy dla próby wylosowanej
      w panelu Ryc. 5.1. W kolumnie z-testu panel stosuje poprawkę na
      ciągłość, którą część programów włącza domyślnie: zmniejsza różnicę \\(\\hat{p} - p_0\\) o \\(1/(2n)\\), żeby
      złagodzić zastąpienie słupków ciągłą krzywą. Jej p-wartość różni się
      więc nieco od tej, którą dałby sam wzór."),

    figure_panel(
      label = "Ryc. 5.3",
      title = "Porównanie wyników: dwumianowy vs z-test",
      lc_toolbar(lc_action("ch3_compare", "Porównaj testy", variant = "solid")),
      uiOutput("ch3_compare_result")
    ),

    lc_p("Wynik porównania zależy od scenariusza. Przy jakości wody
      (\\(p_0 = 0.8\\), \\(n = 50\\)) i \\(k = 43\\) oba testy dają
      p = 0.38. Inaczej przy produktach poza normą: \\(p_0 = 0.03\\), więc
      przy \\(n = 50\\) spodziewamy się przy H₀ średnio 1.5 wadliwej sztuki,
      a rozkład \\(B(50;\\ 0.03)\\) jest silnie prawoskośny. Dla \\(k = 4\\)
      test dwumianowy daje p = 0.063, z-test z poprawką 0.097, a z-test
      wprost ze wzoru powyżej 0.038. Trzy odpowiedzi na to samo pytanie
      lądują po obu stronach progu 0.05."),

    lc_p("Wniosek jest taki sam jak przy przedziale Walda: im bliżej 0 lub 1
      leży \\(p_0\\) i im mniejsza jest próba, tym gorzej działa przybliżenie
      normalne. Panel pokazuje dlatego oczekiwane przy H₀ liczby sukcesów
      \\(np_0\\) i porażek \\(n(1-p_0)\\). Gdy któraś z nich jest mała,
      rozkład dwumianowy jest wyraźnie skośny i z-test może się mylić. Przy
      dużych próbach i proporcjach z dala od krańców oba testy dają
      praktycznie ten sam wynik."),

    lc_table(
      data.frame(
        c1 = c(
          "Metoda",
          "Mała próba, p₀ blisko 0 lub 1",
          "Duża próba, p₀ z dala od 0 i 1"
        ),
        c2 = c("Dokładny — liczy z rozkładu B(n, p₀)", "Działa", "Działa"),
        c3 = c(
          "Przybliżony — używa rozkładu normalnego",
          "Może być niedokładny",
          "Daje praktycznie ten sam wynik"
        )
      ),
      cols = list(
        lc_col("c1", "", "row"),
        lc_col("c2", "Test dwumianowy", "text"),
        lc_col("c3", "Test proporcji (z-test)", "text")
      ),
      cell_class = list(c2 = c(NA, "is-best", "is-best"), c3 = c(NA, "is-base", "is-best")),
      narrow = "cards",
      prose = TRUE
    ),

    lc_p("Skoro test dwumianowy działa zawsze, a przy dużych próbach daje to
      samo co z-test, przy jednej proporcji nie ma powodu z niego rezygnować.
      Z-test warto jednak rozpoznawać, bo podaje go wiele źródeł, a jego
      konstrukcja, różnica podzielona przez błąd standardowy, jest taka sama
      jak w teście t. Założenia testu dwumianowego, przede wszystkim
      niezależność obserwacji, omawiamy w wykładzie 05."),

    # ========================================================================
    # Ćwiczenia CASchools
    # ========================================================================
    lc_h2("ch3-cas", "Ćwiczenia", "CASchools — test proporcji"),

    lc_p("Na koniec dwa zadania na prawdziwych danych. Tym razem nic nie
      losujemy: liczbę sukcesów \\(k\\) i liczebność \\(n\\) trzeba odczytać z pliku.
      W każdym zadaniu zapisz hipotezy przed policzeniem p-wartości
      i przed zajrzeniem do rozwiązania."),

    lc_note("Dane",
      p("420 okręgów szkolnych Kalifornii (1998–1999). Plik: ",
        tags$code("dane/caschools.csv"), "."),
      p(strong("Zmienne w zadaniach:"), " ", tags$code("grades"),
        " (typ szkoły: KK-06 lub KK-08), ",
        tags$code("lunch"), " (% uczniów z dotacją do obiadów — wskaźnik ubóstwa).")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie A — Czy większość okręgów obejmuje klasy tylko do 6.?"),
      p("Okręgi dzielą się na szkoły klas KK-06 i KK-08. Przetestuj
        dwustronnie, czy odsetek okręgów KK-06 różni się od 50%.
        Sformułuj H₀ i Hₐ, oblicz p-wartość testem dwumianowym (α = 0.05).
        Jak zinterpretować wynik?"),
      lc_more("Rozwiązanie", uiOutput("cas_ch3_sol_a"))
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie B — Czy więcej niż 30% okręgów ma wysoki poziom ubóstwa?"),
      p("Przyjmij, że okręg ma wysoki poziom ubóstwa, gdy ", tags$code("lunch > 50"),
        ". Przetestuj jednostronnie (prawostronnie),
        czy odsetek takich okręgów przekracza normę 30%.
        Sformułuj H₀ i Hₐ, wykonaj test dwumianowy. Jaki wniosek?"),
      lc_more("Rozwiązanie", uiOutput("cas_ch3_sol_b"))
    ),

    lc_p("W obu zadaniach pełna odpowiedź ma trzy części: hipotezy ustalone
      przed analizą, p-wartość z testu dwumianowego i zdanie o odsetku
      w populacji okręgów, a nie tylko o liczbach z tabeli. Dotąd test
      dotyczył zawsze jednej zmiennej porównywanej z wartością referencyjną.
      Następny rozdział zaczyna od pytań o związek dwóch zmiennych."),

    lc_chapter_next(
      num       = "06",
      title     = "Korelacja",
      lead      = "związek między dwiema zmiennymi ilościowymi.",
      target_id = "ch-korelacja"
    )
  )
)

# ============================================================================
# DANE — CASchools (wczytane raz przy ładowaniu modułu)
# ============================================================================

.ch3_cas <- read.csv(file.path(app_dir, "dane", "caschools.csv"),
                     stringsAsFactors = FALSE)

# Liczba z przecinkiem dziesiętnym do tekstów rozwiązań.
.ch3_dec <- function(x, digits) {
  formatC(x, format = "f", digits = digits)
}

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  # --- Parametry scenariuszy ---
  scenario_params <- list(
    water_quality = list(
      p0 = 0.80, p_true = 0.85, n_default = 50,
      success_label = "spełnia normę", failure_label = "nie spełnia",
      title = "Jakość próbek wody",
      question = "Czy odsetek próbek spełniających normy różni się od deklarowanych 80%?",
      h0_text = "\\(H_0: p = 0.80\\) (odsetek zgodny z deklaracją)",
      h1_text = "\\(H_a: p \\neq 0.80\\) (odsetek odbiega od deklaracji)",
      question_1s = "Czy odsetek próbek spełniających normy jest wyższy niż 80%?",
      h0_text_1s = "\\(H_0: p \\leq 0.80\\)",
      h1_text_1s = "\\(H_a: p > 0.80\\)",
      alt_1s = "greater"),
    exam_pass = list(
      p0 = 0.60, p_true = 0.68, n_default = 50,
      success_label = "zdał", failure_label = "nie zdał",
      title = "Zdawalność egzaminu",
      question = "Czy zdawalność różni się od 60% (wartość historyczna)?",
      h0_text = "\\(H_0: p = 0.60\\) (zdawalność typowa)",
      h1_text = "\\(H_a: p \\neq 0.60\\) (zdawalność odbiega od normy)",
      question_1s = "Czy zdawalność jest wyższa niż historyczne 60%?",
      h0_text_1s = "\\(H_0: p \\leq 0.60\\)",
      h1_text_1s = "\\(H_a: p > 0.60\\)",
      alt_1s = "greater"),
    germination = list(
      p0 = 0.90, p_true = 0.86, n_default = 50,
      success_label = "wykiełkowało", failure_label = "nie wykiełkowało",
      title = "Kiełkowalność nasion",
      question = "Czy kiełkowalność partii nasion różni się od deklarowanych 90%?",
      h0_text = "\\(H_0: p = 0.90\\) (kiełkowalność zgodna z deklaracją)",
      h1_text = "\\(H_a: p \\neq 0.90\\) (kiełkowalność odbiega)",
      question_1s = "Czy kiełkowalność jest niższa niż deklarowane 90%?",
      h0_text_1s = "\\(H_0: p \\geq 0.90\\)",
      h1_text_1s = "\\(H_a: p < 0.90\\)",
      alt_1s = "less"),
    defects = list(
      p0 = 0.03, p_true = 0.06, n_default = 50,
      success_label = "poza normą", failure_label = "w normie",
      title = "Kontrola jakości produktów",
      question = "Czy odsetek produktów niespełniających normy różni się od dopuszczalnych 3%?",
      h0_text = "\\(H_0: p = 0.03\\) (odsetek wadliwych zgodny z normą)",
      h1_text = "\\(H_a: p \\neq 0.03\\) (odsetek odbiega od normy)",
      question_1s = "Czy odsetek produktów poza normą przekracza dopuszczalne 3%?",
      h0_text_1s = "\\(H_0: p \\leq 0.03\\)",
      h1_text_1s = "\\(H_a: p > 0.03\\)",
      alt_1s = "greater"),
    helmets = list(
      p0 = 0.95, p_true = 0.88, n_default = 80,
      success_label = "nosi kask", failure_label = "bez kasku",
      title = "Używanie kasków na budowie",
      question = "Czy odsetek pracowników używających kasków odbiega od zakładanych 95%?",
      h0_text = "\\(H_0: p = 0.95\\) (odsetek zgodny z wymaganiem)",
      h1_text = "\\(H_a: p \\neq 0.95\\) (odsetek odbiega od wymagania)",
      question_1s = "Czy odsetek pracowników używających kasków jest niższy niż wymagane 95%?",
      h0_text_1s = "\\(H_0: p \\geq 0.95\\)",
      h1_text_1s = "\\(H_a: p < 0.95\\)",
      alt_1s = "less")
  )

  # --- Współdzielone dane ---
  # Jedna próbka dla testu dwustronnego i jednostronnego; po zmianie
  # scenariusza albo n stara próbka nie jest już zgodna z pytaniem.
  ch3_data_state <- reactiveVal(NULL)
  ch3_data <- reactive({
    state <- ch3_data_state()
    if (is.null(state)) return(NULL)
    req(input$ch3_scenario, input$ch3_n)

    if (!identical(state$scenario, input$ch3_scenario) ||
        !isTRUE(state$n == input$ch3_n)) {
      return(NULL)
    }

    list(k = state$k, n = state$n)
  })

  # Kroki widgetów (1..3) żyją w przeglądarce; nowa próba ani zmiana
  # scenariusza nie cofa kroku.
  ch3_step <- lc_step_server("ch3_test", input)$step
  ch3b_step <- lc_step_server("ch3b_test", input)$step

  observeEvent(input$ch3_new_sample, {
    req(input$ch3_scenario, input$ch3_n)
    par <- scenario_params[[input$ch3_scenario]]
    req(!is.null(par))
    n <- input$ch3_n
    k <- rbinom(1, n, par$p_true)
    ch3_data_state(list(
      scenario = input$ch3_scenario,
      n = n,
      k = k
    ))
  }, ignoreInit = TRUE)

  # Krok 1: słupki sukces / porażka (dane i druga kategoria) z proporcją.
  ch3_counts_plot <- function(k, n, par, phat_label) {
    df <- data.frame(
      kat = factor(c(par$success_label, par$failure_label),
                   levels = c(par$success_label, par$failure_label)),
      count = c(k, n - k)
    )
    y_top <- max(k, n - k) * 1.2

    ggplot(df, aes(x = kat, y = count)) +
      step_result(geom_col, data = df[1, ], width = 0.6) +
      step_result(geom_col, data = df[2, ], width = 0.6,
                  fill = STEP_ROLES$group$colour) +
      geom_text(aes(label = count), vjust = -0.5, size = 5, fontface = "bold",
                family = lc_mono_family, colour = STEP_ROLES$known$colour) +
      step_symbol_label(1.5, max(k, n - k) * 0.7, phat_label, role = "new",
                        hjust = 0.5, size = 5) +
      labs(x = NULL, y = "Liczba") +
      step_frame(xlim = c(0.4, 2.6), ylim = c(0, y_top))
  }

  # Kroki 2–3: rozkład dwumianowy pod H₀; extreme = słupki p-wartości.
  # Rama: zakres z niepomijalnym prawdopodobieństwem plus wynik k.
  ch3_binom_plot <- function(k, n, p0, extreme, step) {
    df <- data.frame(x = 0:n, prob = dbinom(0:n, n, p0), extreme = extreme)
    shown <- df$x[df$prob >= max(df$prob) * 1e-3]
    xlim <- range(c(shown, k)) + c(-1.5, 1.5)
    y_top <- max(df$prob) * 1.15
    k_role <- step_role(step, 2)

    ggplot(df, aes(x = x, y = prob)) +
      step_result(geom_col, data = df[!(df$extreme & step >= 3), ], width = 0.8) +
      step_show(step, 3, step_result(geom_col, data = df[df$extreme, ], width = 0.8,
                                     fill = STEP_ROLES$new$colour)) +
      step_line(k_role, xintercept = k, helper = FALSE) +
      step_label(k, y_top * 0.9, paste0("k = ", k), role = k_role,
                 hjust = if (k > n * p0) -0.2 else 1.2) +
      labs(x = "Liczba sukcesów", y = "Prawdopodobieństwo") +
      step_frame(xlim = xlim, ylim = c(0, y_top))
  }

  # =============================================
  # WIDGET 1: Dwustronny
  # =============================================

  output$ch3_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch3_scenario]]
    d <- ch3_data()

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
        lc_empty("Kliknij „Losuj próbę”")
      }
    )
  })

  zoom_plot_server("ch3_step_plot", reactive({
    d <- ch3_data()
    step <- ch3_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d)) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0

    if (step == 1) {
      ch3_counts_plot(k, n, par,
                      paste0("hat(p) == '", k, "/", n, " = ", lc_fmt(k / n, 3), "'"))
    } else {
      # Wartości co najmniej tak mało prawdopodobne jak k (dwustronnie)
      extreme <- dbinom(0:n, n, p0) <= dbinom(k, n, p0)
      ch3_binom_plot(k, n, p0, extreme, step)
    }
  }))

  output$ch3_test_text <- renderUI({
    d <- ch3_data()
    step <- ch3_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d)) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0; phat <- k / n

    switch(as.character(step),
      "1" = tagList(
        "n = ", step_num(n), paste0(", ", par$success_label, ": "), step_num(k),
        paste0(". Proporcja z próby: p̂ = ", k, "/", n, " = "), step_num(lc_fmt(phat, 3)),
        ". Wartość referencyjna: p₀ = ", step_num(p0),
        ". Różnica: ", step_num(lc_fmt(phat - p0, 3)), ". Ale czy to dużo?"
      ),
      "2" = tagList(
        paste0("Rozkład B(", n, "; ", lc_fmt(p0, 2), ") — liczba sukcesów, "),
        "jakiej należałoby się spodziewać przy prawdziwej H₀. Pionowa linia: nasz wynik k = ",
        step_num(k), "."
      ),
      "3" = step_verdict(binom.test(k, n, p0, alternative = "two.sided")$p.value)
    )
  })

  # =============================================
  # WIDGET 2: Jednostronny (te same dane)
  # =============================================

  output$ch3b_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch3_scenario]]
    d <- ch3_data()

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
        lc_empty("Najpierw wylosuj próbę w teście dwustronnym powyżej")
      }
    )
  })

  zoom_plot_server("ch3b_step_plot", reactive({
    d <- ch3_data()
    step <- ch3b_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d)) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0

    if (step == 1) {
      ch3_counts_plot(k, n, par,
                      paste0("hat(p) == ", lc_fmt(k / n, 3), " ~ '(te same dane)'"))
    } else {
      # Jeden ogon: wartości co najmniej tak skrajne jak k w kierunku Hₐ
      extreme <- if (par$alt_1s == "greater") 0:n >= k else 0:n <= k
      ch3_binom_plot(k, n, p0, extreme, step)
    }
  }))

  output$ch3b_test_text <- renderUI({
    d <- ch3_data()
    step <- ch3b_step()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d)) return(NULL)

    k <- d$k; n <- d$n; p0 <- par$p0; phat <- k / n

    switch(as.character(step),
      "1" = tagList(
        "n = ", step_num(n), ", p̂ = ", step_num(lc_fmt(phat, 3)),
        " — te same dane co w teście dwustronnym."
      ),
      "2" = tagList(
        paste0("Ten sam rozkład B(", n, "; ", lc_fmt(p0, 2), "), ale teraz liczy się tylko ",
               if (par$alt_1s == "greater") "prawy" else "lewy", " ogon.")
      ),
      "3" = tagList(
        "Jednostronnie: ",
        step_verdict(binom.test(k, n, p0, alternative = par$alt_1s)$p.value), " ",
        tags$em("Porównaj z p-wartością testu dwustronnego wyżej.")
      )
    )
  })

  # =============================================
  # WIDGET 3: Porównanie dwumianowy vs proporcji
  # =============================================

  output$ch3_compare_result <- renderUI({
    req(input$ch3_compare)
    d <- ch3_data()
    par <- scenario_params[[input$ch3_scenario]]

    if (is.null(d)) {
      return(lc_caption(
               "Najpierw wylosuj próbę w widgecie powyżej."
             ))
    }

    k <- d$k; n <- d$n; p0 <- par$p0; phat <- k / n

    # Test dwumianowy
    binom_res <- binom.test(k, n, p0, alternative = "two.sided")

    # Test proporcji (z-test z poprawką ciągłości)
    prop_res <- suppressWarnings(prop.test(k, n, p = p0, alternative = "two.sided", correct = TRUE))

    # Statystyka z z tą samą poprawką na ciągłość co prop.test (Yates):
    # |k - np₀| pomniejszone o 0.5 (nie więcej niż sama różnica).
    diff_k <- k - n * p0
    z_stat <- sign(diff_k) * (abs(diff_k) - min(0.5, abs(diff_k))) / sqrt(n * p0 * (1 - p0))

    # Warunki przybliżenia normalnego
    np0 <- n * p0
    nq0 <- n * (1 - p0)
    ok <- np0 >= 10 && nq0 >= 10

    div(
      lc_table(
        data.frame(
          row = c("Dane", "Statystyka", "p-wartość", "Decyzja"),
          binom = c(paste0("k = ", k, ", n = ", n), paste0("k = ", k, " (dokładna)"),
                    format_p_value(binom_res$p.value),
                    format_test_result(binom_res$p.value)$decision),
          prop = c(paste0("k = ", k, ", n = ", n), paste0("z = ", round(z_stat, 3), " (z poprawką na ciągłość)"),
                   format_p_value(prop_res$p.value),
                   format_test_result(prop_res$p.value)$decision)
        ),
        cols = list(
          lc_col("row", "", "row"),
          lc_col("binom", "Test dwumianowy", "text"),
          lc_col("prop", "Test proporcji (z)", "text")
        ),
        cell_class = list(
          binom = c(NA, NA, NA, if (binom_res$p.value < 0.05) "is-base" else NA),
          prop = c(NA, NA, NA, if (prop_res$p.value < 0.05) "is-base" else NA)
        )
      ),
      lc_status(
        p(lc_verdict(tags$b("Oczekiwane przy H₀:"), type = if (ok) "ok" else "danger"), " ",
          withMathJax(paste0("\\(np_0 = ", round(np0, 1), "\\)")),
          " sukcesów i ",
          withMathJax(paste0("\\(n(1-p_0) = ", round(nq0, 1), "\\)")),
          " porażek",
          if (ok) " — obie liczby są duże, przybliżenie normalne działa dobrze."
          else " — jedna z liczb jest mała, więc test proporcji może być niedokładny.")
      )
    )
  })

  # --- Ćwiczenia CASchools ---

  output$cas_ch3_sol_a <- renderUI({
    r <- local({
      k <- sum(.ch3_cas$grades == "KK-06")
      n <- nrow(.ch3_cas)
      p_obs <- k / n
      bt <- binom.test(k, n, p = 0.5, alternative = "two.sided")
      list(k = k, n = n, p_obs = p_obs, p_val = bt$p.value,
           ci_lo = bt$conf.int[1], ci_hi = bt$conf.int[2])
    })
    tagList(
      p(tags$b("H₀:"), " p_KK06 = 0.5 · ", tags$b("Hₐ:"), " p_KK06 ≠ 0.5"),
      tags$ul(
        tags$li(sprintf("k = %d, n = %d, p̂ = %s (%s%%)",
                        r$k, r$n, .ch3_dec(r$p_obs, 3), .ch3_dec(100 * r$p_obs, 1))),
        tags$li(sprintf("p %s %s (test dwumianowy, dwustronny)",
          if (r$p_val < 0.001) "<" else "=",
          if (r$p_val < 0.001) "0.001" else .ch3_dec(r$p_val, 4))),
        tags$li(sprintf("95%% przedział ufności: [%s; %s]",
                        .ch3_dec(r$ci_lo, 3), .ch3_dec(r$ci_hi, 3)))
      ),
      lc_verdict(tags$strong(if (r$p_val < 0.05) "Odrzucamy H₀" else "Brak podstaw do odrzucenia H₀"),
                 type = if (r$p_val < 0.05) "danger" else "ok"),
      p(tags$b("Interpretacja:"), " ",
        sprintf("%s%% okręgów to szkoły KK-06. %s",
          .ch3_dec(100 * r$p_obs, 1),
          if (r$p_val >= 0.05) {
            "Dane nie dają podstaw, by twierdzić, że odsetek różni się od 50% (p ≥ 0.05)."
          } else if (r$p_obs > 0.5) {
            "Odsetek istotnie różni się od 50% (p < 0.05): okręgi KK-06 stanowią większość."
          } else {
            "Odsetek istotnie różni się od 50% (p < 0.05), ale w przeciwną stronę, niż sugeruje pytanie: okręgów KK-06 jest wyraźnie mniej niż połowa."
          }))
    )
  })

  output$cas_ch3_sol_b <- renderUI({
    r <- local({
      high_lunch <- .ch3_cas$lunch > 50
      k <- sum(high_lunch)
      n <- length(high_lunch)
      p_obs <- k / n
      bt <- binom.test(k, n, p = 0.30, alternative = "greater")
      list(k = k, n = n, p_obs = p_obs, p_val = bt$p.value,
           ci_lo = bt$conf.int[1], ci_hi = bt$conf.int[2])
    })
    tagList(
      p(tags$b("H₀:"), " p_ubóstwo ≤ 0.30 · ",
        tags$b("Hₐ:"), " p_ubóstwo > 0.30"),
      tags$ul(
        tags$li(sprintf("k = %d okręgów z lunch > 50, n = %d, p̂ = %s (%s%%)",
                        r$k, r$n, .ch3_dec(r$p_obs, 3), .ch3_dec(100 * r$p_obs, 1))),
        tags$li(sprintf("p %s %s (test dwumianowy, prawostronny)",
          if (r$p_val < 0.001) "<" else "=",
          if (r$p_val < 0.001) "0.001" else .ch3_dec(r$p_val, 4))),
        tags$li(sprintf("Dolna granica jednostronnego 95%% przedziału ufności: %s",
                        .ch3_dec(r$ci_lo, 3)))
      ),
      lc_verdict(tags$strong(if (r$p_val < 0.05) "Odrzucamy H₀" else "Brak podstaw do odrzucenia H₀"),
                 type = if (r$p_val < 0.05) "danger" else "ok"),
      p(tags$b("Interpretacja:"), " ",
        sprintf("%s%% okręgów ma wysoki poziom ubóstwa (lunch > 50). %s",
          .ch3_dec(100 * r$p_obs, 1),
          if (r$p_val < 0.05) {
            "Odsetek ten istotnie przekracza normę 30% (p < 0.05)."
          } else {
            "Dane nie dają podstaw, by twierdzić, że odsetek przekracza normę 30% (p ≥ 0.05)."
          }))
    )
  })
}
