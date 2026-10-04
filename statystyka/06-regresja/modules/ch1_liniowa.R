# ============================================================================
# CHAPTER 1: Regresja liniowa prosta
# ============================================================================

# Dane CASchools są wczytywane w helpers.R jako .cas_data / .cas_labels
# (wspólne dla ch1 i ch2).

.ch1_pred_specs <- list(
  read_students = list(
    label = "Czytanie ~ liczba uczniów",
    x = "students", y = "read", default = 2000, step = 100,
    unit = "uczniów",
    question = "Jaki będzie przewidywany średni wynik z czytania w okręgu o takiej liczbie uczniów?"
  ),
  math_income = list(
    label = "Matematyka ~ dochód okręgu",
    x = "income", y = "math", default = 15, step = 1,
    unit = "tys. USD dochodu",
    question = "Jaki będzie przewidywany średni wynik z matematyki przy takim dochodzie okręgu?"
  ),
  read_str = list(
    label = "Czytanie ~ uczniowie na nauczyciela",
    x = "student_teacher_ratio", y = "read", default = 20, step = 0.5,
    unit = "uczniów na nauczyciela",
    question = "Jaki będzie przewidywany średni wynik z czytania przy takim STR?"
  ),
  math_expenditure = list(
    label = "Matematyka ~ wydatki na ucznia",
    x = "expenditure", y = "math", default = 6000, step = 100,
    unit = "wydatków na ucznia",
    question = "Jaki będzie przewidywany średni wynik z matematyki przy takich wydatkach na ucznia?"
  )
)

.ch1_pred_choices <- setNames(names(.ch1_pred_specs), vapply(.ch1_pred_specs, `[[`, character(1), "label"))

ch1_ui <- list(
  id    = "ch-liniowa",
  num   = "01",
  title = "Regresja liniowa prosta",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 01 · Regresja",
      num    = "01",
      title  = "Regresja liniowa prosta.",
      lead   = "Korelacja mówi, jak ściśle dwie zmienne trzymają się prostej, ale nie
                mówi, o ile zmienia się jedna, gdy rośnie druga. Regresja zamienia
                chmurę punktów w równanie prostej. Z niego odczytamy tempo zmian
                i przewidzimy wynik dla nowej obserwacji."
    ),

    # ========================================================================
    # Od korelacji do regresji
    # ========================================================================
    lc_h2("ch1-od-korelacji", "Od korelacji do regresji"),

    lc_p("W wykładzie 04 (rozdział 06) związek dwóch zmiennych ilościowych
      opisywaliśmy współczynnikiem korelacji \\(r\\). Mówił on o kierunku i sile
      związku liniowego, a test korelacji sprawdzał, czy \\(r\\) da się odróżnić
      od zera. Jedno pytanie zostało tam bez odpowiedzi: o ile zmienia się
      \\(Y\\), gdy \\(X\\) rośnie o jednostkę. Trzy chmury punktów o nachyleniach
      0.4, 0.8 i 1.6 miały prawie to samo \\(r \\approx 0.96\\). Pytanie
      „o ile?” należy do regresji."),

    lc_p(gloss("regresja liniowa", "Regresja liniowa"), " opisuje związek prostą.
      Zanim się ją dopasuje, trzeba obejrzeć wykres rozrzutu. Prosta ma sens
      tylko wtedy, gdy chmura punktów układa się w przybliżeniu wzdłuż linii.
      Przy krzywiźnie, dwóch oddzielnych chmurach albo pojedynczej wartości
      odstającej prosta źle opisze dane, choć jej współczynniki da się policzyć
      zawsze. To te same pułapki, które w wykładzie 04 psuły współczynnik
      korelacji."),

    lc_p("Gdy jest jedna zmienna objaśniająca \\(X\\), mówimy o ",
      gloss("regresja prosta", "regresji liniowej prostej"), ". Model zapisuje
      każdą wartość \\(Y\\) jako punkt na prostej plus odchylenie losowe:"),

    lc_formula_box(withMathJax(
      "$$Y = \\beta_0 + \\beta_1 X + \\varepsilon$$"
    )),

    lc_p("\\(\\beta_0\\) to ", gloss("wyraz wolny"), ", czyli wysokość prostej
      w punkcie \\(X = 0\\). \\(\\beta_1\\) to ",
      gloss("współczynnik regresji", "nachylenie"), ": o ile zmienia się średnie
      \\(Y\\), gdy \\(X\\) rośnie o jednostkę. \\(\\varepsilon\\) to błąd losowy,
      czyli ta część \\(Y\\), której prosta nie wyjaśnia. Greckie litery oznaczają
      nieznane ", gloss("parametr", "parametry"), " populacji. Z próby liczymy
      ich ", gloss("estymator", "estymatory"), ", oznaczane łacińskimi literami
      \\(b_0\\) i \\(b_1\\), tak jak w wykładzie 03 średnia z próby szacowała
      średnią populacji."),

    lc_p("Panel poniżej generuje dane z tego modelu dla wybranych wartości
      \\(\\beta_0\\), \\(\\beta_1\\) i \\(\\sigma\\), czyli odchylenia
      standardowego błędu losowego."),

    figure_panel(
      label = "Ryc. 1.0", title = "Co robią β₀, β₁ i szum?",
      full_width = TRUE,
      lc_toolbar(
        lc_slider("ch1_beta_b0", "β₀ (punkt startu)", -10, 20, 5, 1),
        lc_slider("ch1_beta_b1", "β₁ (nachylenie)", -3, 3, 1, 0.25),
        lc_slider("ch1_beta_sigma", "Szum σ", 0, 8, 2, 0.5)
      ),
      lc_plot("ch1_beta_plot", max_height = "320px"),
      uiOutput("ch1_beta_info")
    ),

    lc_p("\\(\\beta_0\\) przesuwa całą prostą w górę i w dół, nie zmieniając jej
      kąta. \\(\\beta_1\\) obraca prostą: przy wartościach dodatnich prosta rośnie,
      przy ujemnych opada, a przy zerze jest pozioma i znajomość \\(X\\) nic nie
      mówi o \\(Y\\). \\(\\sigma\\) nie rusza prostej, tylko rozrzuca punkty wokół
      niej. Przy \\(\\sigma = 0\\) wszystkie punkty leżą na linii, przy dużym
      \\(\\sigma\\) trend ginie w szumie. Korelacja \\(r\\) zależy od nachylenia
      i od szumu jednocześnie, dlatego sama nie wystarcza, żeby odtworzyć
      nachylenie."),

    lc_p("W panelu to my ustawialiśmy parametry i patrzyliśmy na dane.
      W praktyce jest odwrotnie: znamy tylko chmurę punktów i musimy z niej
      odczytać \\(b_0\\) i \\(b_1\\)."),

    # ========================================================================
    # Regresja z korelacji
    # ========================================================================
    lc_h2("ch1-korelacja-regresja", "Regresja z korelacji"),

    lc_p("Do wyznaczenia prostej wystarczą liczby, które już umiemy policzyć:
      dwie średnie, dwa odchylenia standardowe i współczynnik korelacji.
      Nachylenie i wyraz wolny wyznacza się tak:"),

    lc_formula_box(withMathJax(
      "$$b_1 = r \\cdot \\frac{s_Y}{s_X}, \\qquad b_0 = \\bar{y} - b_1 \\bar{x}$$"
    )),

    lc_p("Pierwszy wzór czyta się tak: \\(r\\) mówi, o ile odchyleń
      standardowych zmienia się średnio \\(Y\\), gdy \\(X\\) rośnie o jedno
      odchylenie standardowe. Mnożenie przez \\(s_Y / s_X\\) przelicza to na
      jednostki obu zmiennych. Drugi wzór sprawia, że prosta przechodzi przez
      punkt \\((\\bar{x}, \\bar{y})\\): obserwacji o przeciętnym \\(X\\) prosta
      przypisuje przeciętne \\(Y\\). Panel buduje prostą w tej kolejności na
      losowej próbie 65 punktów."),

    figure_panel(
      label = "Ryc. 1.1",
      full_width = TRUE,
      lc_step_widget("ch1_corr",
        title = "Jak policzyć regresję z korelacji?",
        steps = c("Dane", "Średnie X i Y", "Odchylenia standardowe", "Korelacja r",
                  "Nachylenie b₁", "Wyraz wolny b₀"),
        toolbar = lc_toolbar(
          lc_action("ch1_corr_new", "Nowa próba", icon = "shuffle", variant = "outline")
        ),
        plot_id = "ch1_corr_plot",
        ratio = "2/1",
        extra = uiOutput("ch1_corr_info")
      )
    ),

    lc_p("Odchylenia standardowe są zawsze dodatnie, więc znak \\(b_1\\) jest
      taki sam jak znak \\(r\\), a \\(b_1 = 0\\) dokładnie wtedy, gdy \\(r = 0\\).
      Korelacja ustala kierunek i siłę związku, a iloraz odchyleń standardowych
      przelicza ją na jednostki \\(X\\) i \\(Y\\). Prosta otrzymana w ten sposób
      jest dokładnie tą, którą program statystyczny podaje w tabeli wyników
      regresji z jednym predyktorem."),

    lc_p("Taka tabela zawiera dla każdego współczynnika cztery liczby:
      estymatę, ", gloss("błąd standardowy"), ", statystykę \\(t\\)
      i p-wartość. Do końca rozdziału nauczymy się czytać je wszystkie.
      Zaczniemy od najprostszej czynności: odczytania z tabeli dwóch estymat
      i narysowania prostej, którą opisują."),

    # ========================================================================
    # Ćwiczenie: narysuj prostą z tabeli
    # ========================================================================
    lc_h2("ch1-rysuj-z-tabeli", "Ćwiczenie: narysuj prostą z tabeli"),

    lc_p("Tabela w ćwiczeniu zawiera dwie liczby: wyraz wolny i współczynnik
      przy \\(X\\). Prostą wyznaczają dowolne dwa jej punkty, więc wystarczy
      podstawić do równania dwie wartości \\(X\\) i zaznaczyć na wykresie
      otrzymane \\(Y\\). Po odsłonięciu odpowiedzi panel dorysuje poprawną prostą
      i dane, z których ją policzono."),

    figure_panel(
      label = "Ćwiczenie", title = "Kliknij dwa punkty, przez które przechodzi prosta",
      full_width = TRUE,
      lc_toolbar(
        lc_action("ch1_draw_reset", "Wyczyść punkty", variant = "outline"),
        lc_action("ch1_draw_reveal", "Pokaż odpowiedź", variant = "solid"),
        lc_action("ch1_draw_new", "Nowe ćwiczenie", variant = "solid"),
        lc_readouts(uiOutput("ch1_draw_stats"))
      ),
      zoom_plot_ui("ch1_draw_plot", height = "360px",
                     click = "ch1_draw_plot_click"),
      uiOutput("ch1_draw_table"),
      uiOutput("ch1_draw_feedback"),
      lc_caption("Przeczytaj tabelę współczynników. Potem kliknij na wykresie dwa punkty, które wyznaczają prostą regresji.")
    ),

    lc_p("Najłatwiej liczy się punkty dla okrągłych wartości \\(X\\). Jeśli Twoja
      prosta rozminęła się z poprawną, sprawdź najpierw znak nachylenia, a potem
      wysokość, na której prosta przecina pionową oś \\(X = 0\\). Punkty danych
      pojawiają się dopiero po odsłonięciu odpowiedzi, bo do narysowania prostej
      nie są potrzebne: wystarczą dwa współczynniki."),

    lc_p("Narysowanie prostej z gotowych współczynników jest więc mechaniczne.
      Otwarte zostaje pytanie, skąd się te współczynniki biorą. Przez chmurę
      punktów da się przeprowadzić nieskończenie wiele prostych, a tabela podaje
      jedną."),

    # ========================================================================
    # Najmniejsze kwadraty
    # ========================================================================
    lc_h2("ch1-ols-krok", "Najmniejsze kwadraty — krok po kroku"),

    lc_p("Kryterium wyboru nazywa się ",
      gloss("metoda najmniejszych kwadratów", "metodą najmniejszych kwadratów"),
      " (MNK, ang. OLS). Dla każdej prostej można zmierzyć, o ile każdy punkt
      mija się z nią w pionie, podnieść te odległości do kwadratu i zsumować.
      Spośród wszystkich prostych wybieramy tę, dla której suma kwadratów
      jest najmniejsza:"),

    lc_formula_box(withMathJax(
      "$$SS_{res} = \\sum_{i=1}^{n} (y_i - \\hat{y}_i)^2 = \\sum_{i=1}^{n} (y_i - b_0 - b_1 x_i)^2 \\;\\to\\; \\min$$"
    )),

    lc_p("Odległości mierzy się w pionie, bo model przewiduje \\(Y\\) na
      podstawie \\(X\\) i to w \\(Y\\) popełnia pomyłki. Rozwiązaniem tego
      zadania są wzory z poprzedniej sekcji. Panel pokazuje kolejne etapy
      na losowej próbie 70 punktów: dane, średnią \\(Y\\) jako model bez
      predyktora, prostą MNK, odległości punktów od prostej i porównanie
      z inną prostą."),

    figure_panel(
      label = "Ryc. 1.1b",
      full_width = TRUE,
      lc_step_widget("ch1_ols",
        title = "Jak linia staje się modelem",
        steps = c("Dane", "Średnia Y", "Linia regresji", "Reszty",
                  "Wynik modelu", "Inna prosta?"),
        toolbar = lc_toolbar(
          lc_action("ch1_ols_new", "Nowa próba", icon = "shuffle", variant = "outline")
        ),
        plot_id = "ch1_ols_plot",
        ratio = "2/1"
      )
    ),

    lc_p("Pozioma linia na wysokości \\(\\bar{y}\\) to najprostszy model:
      każdej obserwacji przewiduje to samo. Prosta MNK wykorzystuje \\(X\\)
      i mija się z punktami mniej. Przerywana prosta z ostatniego kroku
      przechodzi przez ten sam punkt \\((\\bar{x}, \\bar{y})\\), ale jest
      o 55% mniej stroma. Ona też jest modelem, bo dla każdego \\(X\\) daje
      przewidywane \\(\\hat{Y}\\), tylko jej suma kwadratów jest większa.
      Tak samo przegra każda inna prosta: prosta MNK jest z definicji tą
      o najmniejszej sumie kwadratów."),

    # ========================================================================
    # Reszty
    # ========================================================================
    lc_h2("ch1-reszty", "Reszty i dlaczego kwadraty"),

    lc_p("Pionowe odcinki z czwartego kroku to ", gloss("reszta", "reszty"),
      ". Reszta to różnica między wartością zaobserwowaną a wartością
      przewidzianą przez model:"),

    lc_formula_box(withMathJax(
      "$$e_i = y_i - \\hat{y}_i$$"
    )),

    lc_p("Reszta dodatnia oznacza punkt nad prostą, czyli obserwację, dla której
      model zaniżył wynik. Reszta ujemna oznacza punkt pod prostą i wynik
      zawyżony. Reszty prostej MNK sumują się do zera. To samo dotyczy jednak
      każdej prostej przechodzącej przez punkt \\((\\bar{x}, \\bar{y})\\), także
      przerywanej z panelu, więc sama suma reszt nie wskaże najlepszej prostej.
      Znaki trzeba usunąć. Można by sumować wartości bezwzględne, ale MNK
      sumuje kwadraty, i to z dwóch powodów."),

    lc_p("Pierwszy powód: kwadrat rośnie szybciej niż sama odległość. Reszta
      równa 4 wnosi do sumy 16, tyle co szesnaście reszt równych 1. Prosta MNK
      woli więc kilka umiarkowanych pomyłek niż jedną dużą. Ma to drugą stronę:
      pojedyncza wartość odstająca potrafi mocno pociągnąć prostą ku sobie,
      podobnie jak w wykładzie 04 jedna wartość odstająca zmieniała \\(r\\)."),

    lc_p("Drugi powód: suma kwadratów prowadzi do jawnego rozwiązania, czyli do
      wzorów \\(b_1 = r \\cdot s_Y / s_X\\) i \\(b_0 = \\bar{y} - b_1 \\bar{x}\\).
      Minimalizacja sumy wartości bezwzględnych daje regresję medianową.
      To sensowna metoda, mniej wrażliwa na wartości odstające, ale bez
      prostego wzoru."),

    lc_p("Reszty są też głównym narzędziem oceny modelu. Wykład 05 zapowiadał,
      że w modelach założenia dotyczą reszt, a nie samych zmiennych, i że
      sprawdza się je tymi samymi narzędziami: wykresem Q-Q, porównaniem
      rozrzutu i wykresem reszt względem wartości przewidywanych. Jeśli reszty
      układają się w łuk albo w wachlarz, prosta źle opisuje dane. Zajmiemy się
      tym w rozdziale 02."),

    # ========================================================================
    # p-wartość dla nachylenia
    # ========================================================================
    lc_h2("ch1-pvalue", "p-wartość dla nachylenia"),

    lc_p("Współczynnik \\(b_1\\) policzony z próby jest estymatorem nachylenia
      \\(\\beta_1\\) w populacji i jak każdy estymator zmienia się od próby do
      próby. Nawet gdy w populacji \\(X\\) nie jest związane z \\(Y\\)
      (\\(\\beta_1 = 0\\)), \\(b_1\\) z próby prawie nigdy nie wychodzi dokładnie
      zero. Trzeba więc rozstrzygnąć, czy obserwowane nachylenie leży na tyle
      daleko od zera, że trudno je wytłumaczyć samym losowaniem próby.
      Hipotezy są następujące:"),

    lc_formula_box(withMathJax(
      "$$H_0: \\beta_1 = 0 \\qquad H_a: \\beta_1 \\neq 0$$"
    )),

    lc_p("Zmienność \\(b_1\\) między próbami mierzy jego błąd standardowy
      \\(SE(b_1)\\). Jest on tym mniejszy, im ciaśniej punkty trzymają się
      prostej, im więcej jest obserwacji i im szerzej rozciągają się wartości
      \\(X\\). ", gloss("statystyka testowa", "Statystyka testowa"), " ma tę samą
      budowę co w teście t z wykładu 04: estymata podzielona przez swój błąd
      standardowy. Mówi, ile błędów standardowych dzieli \\(b_1\\) od zera."),

    lc_formula_box(withMathJax(
      "$$t = \\frac{b_1}{SE(b_1)}, \\qquad df = n - 2$$"
    )),

    lc_p("Przy prawdziwej H₀ statystyka ma ", gloss("rozkład t-Studenta"), " o ",
      gloss("stopnie swobody", "stopniach swobody"), " \\(n - 2\\): dwa stopnie
      swobody zużywa oszacowanie \\(b_0\\) i \\(b_1\\). ",
      gloss("p-wartość", "P-wartość"), " liczy się i interpretuje jak w każdym
      teście t. Tabela wyników ma też wiersz dla wyrazu wolnego z własnym
      \\(t\\) i p-wartością. Testuje on hipotezę \\(\\beta_0 = 0\\), czyli pyta
      o przewidywanie przy \\(X = 0\\), co rzadko jest interesujące."),

    lc_p("Panel pokazuje cztery symulowane scenariusze o znanym prawdziwym
      nachyleniu."),

    figure_panel(
      label = "Ryc. 1.2", title = "Kiedy nachylenie jest istotne?",
      full_width = TRUE,
      lc_toolbar(
        selectInput("ch1_pval_scenario", "Scenariusz",
            choices = c(
              "Wyraźny dodatni wpływ" = "strong_positive",
              "Brak wpływu" = "none",
              "Wyraźny ujemny wpływ" = "strong_negative",
              "Ten sam trend, mała próba i większy szum" = "small_sample"
            ),
            selected = "strong_positive"
          ),
        lc_readouts(uiOutput("ch1_pval_stats"))
      ),
      lc_plot("ch1_pval_plot", max_height = "360px"),
      uiOutput("ch1_pval_table"),
      uiOutput("ch1_pval_verdict")
    ),

    lc_p("W scenariuszu z wyraźnym dodatnim wpływem prawdziwe nachylenie wynosi
      1.25, a z 70 punktów wychodzi \\(b_1 = 1.02\\) przy \\(SE = 0.16\\).
      Statystyka \\(t = 6.28\\), a p-wartość jest mniejsza niż 0.001.
      W scenariuszu bez wpływu prawdziwe nachylenie to zero, a próba daje
      \\(b_1 = -0.14\\), \\(t = -0.62\\) i \\(p = 0.54\\). Nachylenie
      z próby nie jest zerowe, ale mieści się w zakresie wahań losowych."),

    lc_p("Najwięcej uczy ostatni scenariusz. Prawdziwe nachylenie jest takie samo
      jak w pierwszym, ale punktów jest tylko 14, a szum jest większy. Estymata
      \\(b_1 = 1.14\\) leży blisko prawdziwej wartości, lecz \\(SE = 0.56\\)
      jest ponad trzy razy większy niż w pierwszym scenariuszu. Wychodzi
      \\(t = 2.04\\) i \\(p = 0.063\\). Brak istotności nie oznacza tu braku
      efektu, tylko zbyt małą próbę, żeby go wykazać."),

    lc_p("Więcej niż sama decyzja mówi ", gloss("przedział ufności"),
      " dla \\(\\beta_1\\), zbudowany jak w wykładzie 03: estymata plus minus
      wartość krytyczna razy błąd standardowy."),

    lc_formula_box(withMathJax(
      "$$b_1 \\pm t^*_{\\alpha/2,\\, n-2} \\cdot SE(b_1)$$"
    )),

    lc_p("Panel go nie pokazuje, ale łatwo go policzyć z tabeli. Dla pierwszego
      scenariusza 95% przedział ufności to od 0.70 do 1.35. Dla małej próby
      sięga od -0.07 do 2.35: obejmuje zero, ale też wartości prawie dwa razy
      większe od prawdziwego nachylenia. Dane z małej próby nie wykluczają
      ani braku związku, ani silnego związku."),

    lc_p("Test nachylenia ma jeszcze jedną własność. W regresji prostej jest
      równoważny testowi korelacji z wykładu 04. Statystyka
      \\(t = b_1 / SE(b_1)\\) ma dokładnie tę samą wartość co
      \\(t = r\\sqrt{n-2} / \\sqrt{1-r^2}\\), te same stopnie swobody \\(n - 2\\)
      i tę samą p-wartość. W pierwszym scenariuszu \\(r = 0.61\\) i oba wzory
      dają \\(t = 6.28\\). Nie ma w tym przypadku: \\(\\beta_1 = 0\\) dokładnie
      wtedy, gdy korelacja w populacji jest zerowa. Na pytanie „czy jest
      związek?” regresja prosta odpowiada tak samo jak korelacja. Dodaje
      odpowiedź na pytanie „o ile?”."),

    # ========================================================================
    # CASchools
    # ========================================================================
    lc_h2("ch1-caschool", "Regresja na danych CASchools"),

    lc_p("W symulacji znaliśmy prawdziwe nachylenie, bo sami je ustawiliśmy.
      W prawdziwych danych widzimy tylko próbę. Zbiór CASchools opisuje 420
      okręgów szkolnych w Kalifornii z lat 90. Każdy wiersz to jeden okręg,
      a kolumny opisują m.in. średni dochód w okręgu w tysiącach dolarów,
      wydatki na ucznia, liczbę uczniów na nauczyciela (STR), odsetek uczniów
      uczących się angielskiego jako drugiego języka, odsetek uczniów
      z dotacją do obiadu oraz średnie wyniki testów z czytania
      i matematyki. Na tych danych ekonomiści edukacji sprawdzali, czy mniejsze
      klasy poprawiają wyniki."),

    lc_p("Panel jest ćwiczeniem. Dla wybranej pary zmiennych najpierw oceń
      z wykresu i tabeli znak nachylenia i p-wartość, a dopiero potem porównaj
      swoją ocenę z odpowiedzią."),

    figure_panel(
      label = "Ryc. 1.4", title = "CASchools: od tabeli wyników do interpretacji",
      full_width = TRUE,
      lc_toolbar(
        selectInput("ch1_cas_x", "Zmienna X",
            choices = c(
              "Dochód okręgu (income)" = "income",
              "Uczniowie na nauczyciela (STR)" = "student_teacher_ratio",
              "Wydatki na ucznia (expenditure)" = "expenditure",
              "Udział uczniów z angielskim jako drugim językiem (english)" = "english",
              "Uczniowie z dotacją do obiadu (lunch)" = "lunch",
              "Komputery" = "computer",
              "Zakres klas (grades)" = "grades"
            ),
            selected = "income"
          ),
        selectInput("ch1_cas_y", "Zmienna Y",
            choices = c(
              "Czytanie (read)" = "read",
              "Matematyka (math)" = "math",
              "Dochód okręgu (income)" = "income",
              "Wydatki na ucznia (expenditure)" = "expenditure",
              "Uczniowie na nauczyciela (STR)" = "student_teacher_ratio"
            ),
            selected = "read"
          ),
        lc_action("ch1_cas_reveal", "Pokaż odpowiedź", variant = "solid")
      ),
      lc_plot("ch1_cas_plot", max_height = "360px"),
      uiOutput("ch1_cas_table"),
      uiOutput("ch1_cas_summary"),
      uiOutput("ch1_cas_answer"),
      lc_caption("Wybierz zmienne, obejrzyj wykres i tabelę regresji. Najpierw samodzielnie zdecyduj, czy X istotnie przewiduje Y, a potem pokaż odpowiedź.")
    ),

    lc_p("Dla domyślnej pary, wyniku z czytania i dochodu okręgu,
      \\(b_1 = 1.94\\): okręg zamożniejszy o 1 tys. USD ma przeciętnie wynik
      z czytania wyższy o niecałe 2 punkty. \\(SE = 0.10\\), \\(t = 19.9\\),
      a p-wartość jest mniejsza niż 0.001. Korelacja tych zmiennych wynosi
      \\(r = 0.70\\) i test korelacji dałby dokładnie tę samą p-wartość.
      Wykres pokazuje jednak coś, czego tabela nie zdradza. Najbiedniejsze
      i najbogatsze okręgi leżą przeważnie poniżej prostej, a okręgi o średnim
      dochodzie powyżej. Związek jest wygięty i prosta jest tylko jego
      przybliżeniem. Takie wzorce wychwytuje analiza reszt w rozdziale 02."),

    lc_p("Dla liczby uczniów na nauczyciela nachylenie przy czytaniu wynosi
      -2.62: okręgi, w których na nauczyciela przypada o jednego ucznia więcej,
      mają przeciętnie wynik niższy o 2.6 punktu (\\(p < 0.001\\)). To jeszcze
      nie dowód, że mniejsze klasy poprawiają wyniki. Okręgi z mniejszymi
      klasami są przeciętnie zamożniejsze (korelacja STR z dochodem wynosi
      -0.23), a regresja prosta nie odróżnia wpływu klas od wpływu dochodu.
      Do tego potrzebny jest model z kilkoma predyktorami z rozdziału 03."),

    lc_p("Zmienna „Zakres klas” ma tylko dwie wartości, KK-06 i KK-08. Panel
      koduje je jako 0 i 1. Wtedy prosta łączy średnie obu grup, a \\(b_1\\)
      jest różnicą tych średnich. Predyktorami jakościowymi zajmuje się
      rozdział 03B."),

    # ========================================================================
    # Predykcja
    # ========================================================================
    lc_h2("ch1-predykcja", "Predykcja z modelu"),

    lc_p("Dotąd regresja służyła do opisu związku: czy istnieje, w którą stronę
      idzie i czy jest istotny. Równanie prostej ma też drugie zastosowanie,
      przewidywanie. Po podstawieniu wartości \\(X\\) do równania dostajemy ",
      gloss("wartość przewidywana", "wartość przewidywaną"), ":"),

    lc_formula_box(withMathJax(
      "$$\\hat{Y} = b_0 + b_1 X$$"
    )),

    lc_p("Wartość przewidywana szacuje średnie \\(Y\\) wśród obiektów o danym
      \\(X\\), a nie wynik pojedynczego obiektu. Model czytania od dochodu
      przewiduje dla okręgu o dochodzie 20 tys. USD wynik 664.1. Nie znaczy
      to, że każdy taki okręg osiągnie 664 punkty, tylko że okręgi o tym
      dochodzie osiągają przeciętnie około 664, a pojedyncze rozrzucają się
      wokół tej wartości o tyle, ile wynoszą reszty."),

    lc_p("Panel podaje tabelę współczynników dla kilku modeli. Policz
      \\(\\hat{Y}\\) samodzielnie, zanim odsłonisz odpowiedź."),

    figure_panel(
      label = "Ryc. 1.5", title = "Użyj równania regresji do przewidywania",
      full_width = TRUE,
      lc_toolbar(
        selectInput("ch1_pred_case", "Model",
            choices = .ch1_pred_choices,
            selected = "read_students"
          ),
        lc_action("ch1_pred_reveal", "Pokaż odpowiedź", variant = "solid"),
        lc_readouts(uiOutput("ch1_pred_stats"))
      ),
      lc_plot("ch1_pred_plot", max_height = "360px"),
      uiOutput("ch1_pred_table"),
      uiOutput("ch1_pred_x_input"),
      uiOutput("ch1_pred_question"),
      uiOutput("ch1_pred_answer"),
      lc_caption("Wybierz gotowy model, ustaw wartość X i spróbuj policzyć przewidywane Y z równania regresji.")
    ),

    lc_p("W domyślnym modelu wynik z czytania zależy od liczby uczniów
      w okręgu. Nachylenie wynosi około -0.001 punktu na ucznia, więc dla
      okręgu z 2000 uczniów model przewiduje 655.6. To prawie tyle, ile wynosi
      średni wynik z czytania we wszystkich okręgach (655.0). Przy słabym
      związku (\\(r = -0.19\\)) prosta jest prawie pozioma i przewidywanie
      niewiele różni się od średniej. Małe nachylenie nie musi jednak znaczyć
      małego efektu, bo zależy od jednostek: różnica 10 000 uczniów przekłada
      się już na około 10 punktów."),

    lc_p("Przewidywanie ma oparcie w danych tylko w zakresie \\(X\\), który
      wystąpił w próbie. Liczba uczniów w okręgach CASchools wynosi od 81
      do 27 176. Poza tym przedziałem nie wiadomo, czy związek nadal jest
      liniowy. Taką ", gloss("ekstrapolacja", "ekstrapolację"), " omawia
      rozdział 02."),

    # ========================================================================
    # Co zostawiamy na potem
    # ========================================================================
    lc_h2("ch1-co-dalej", "Co zostawiamy na potem"),

    lc_p("W tym rozdziale przeszliśmy od chmury punktów do równania prostej,
      odczytaliśmy z tabeli wyników estymaty, błędy standardowe i p-wartości
      i użyliśmy równania do przewidywania. Kilka pytań zostało otwartych."),

    lc_p("Pierwsze: czy prostej można ufać. Reszty pokazują, czy model pasuje
      do danych, ", gloss("współczynnik determinacji", "współczynnik determinacji"),
      " \\(R^2\\) mówi, jaką część zmienności \\(Y\\) model wyjaśnia, a RMSE,
      jak duże są typowe pomyłki. Tym zajmuje się rozdział 02."),

    lc_p("Drugie: co zrobić, gdy na wynik wpływa kilka zmiennych naraz, na
      przykład liczba uczniów na nauczyciela, wydatki i dochód. Regresja
      wieloraka z rozdziału 03 ocenia każdą z nich przy stałych pozostałych.
      Rozdział 03B pokazuje, co się dzieje, gdy ważną zmienną pominiemy,
      i jak włączyć do modelu zmienną jakościową."),

    lc_p("Trzecie: kiedy bogatszy model jest lepszy, a kiedy tylko lepiej
      dopasowany do przypadkowych szczegółów próby. Do tego służą skorygowane
      \\(R^2\\), kryteria AIC i BIC oraz podział danych na część uczącą
      i testową w rozdziale 04. Rozdział 05 przenosi regresję na zmienną
      \\(Y\\) o dwóch wartościach."),

    lc_chapter_next(
      num       = "02",
      title     = "Jakość modelu",
      lead      = "reszty, R², RMSE — diagnostyka pojedynczego modelu",
      target_id = "ch-jakosc"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  zoom_plot_server("ch1_beta_plot", reactive({
    set.seed(101)
    x <- seq(0, 10, length.out = 80)
    y_true <- input$ch1_beta_b0 + input$ch1_beta_b1 * x
    y <- y_true + rnorm(length(x), 0, input$ch1_beta_sigma)
    df <- data.frame(x = x, y = y, y_true = y_true)

    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = upwr_secondary, alpha = 0.45) +
      geom_line(aes(y = y_true), color = unname(upwr_cat["niebo"]), linewidth = 1.3) +
      annotate("segment", x = 0, xend = 0, y = 0, yend = input$ch1_beta_b0,
               color = unname(upwr_cat["bursztyn"]), linewidth = 1.1) +
      annotate("text", x = 0.6, y = input$ch1_beta_b0,
               label = paste0("β₀ = ", input$ch1_beta_b0),
               hjust = 0, color = unname(upwr_cat["bursztyn"]), fontface = "bold") +
      labs(x = "X", y = "Y") +
      theme_upwr()
  }))

  output$ch1_beta_info <- renderUI({
    b1 <- input$ch1_beta_b1
    change <- if (b1 > 0) {
      paste0("rośnie o ", abs(b1))
    } else if (b1 < 0) {
      paste0("maleje o ", abs(b1))
    } else {
      "nie zmienia się"
    }
    lc_status(
      p(tags$strong("Interpretacja:"),
        paste0(" gdy X wzrasta o 1, oczekiwane Y ", change,
               ". Szum σ = ", input$ch1_beta_sigma,
               " rozprasza punkty wokół linii."))
    )
  })

  # --- Widget: regresja z korelacji ---
  # Krok widgetu (1..5) żyje w przeglądarce; nowa próba nie zmienia kroku.
  ch1_corr_data <- reactiveVal(generate_regression_data(n = 65, beta0 = 8, beta1 = 1.6, sigma = 4))
  # Pierwszy krok paska („Dane”) to stan 0 rysunku: sama chmura punktów.
  ch1_corr_pos <- lc_step_server("ch1_corr", input)$step
  ch1_corr_step <- reactive(ch1_corr_pos() - 1L)

  observeEvent(input$ch1_corr_new, {
    ch1_corr_data(generate_regression_data(n = 65, beta0 = 8, beta1 = 1.6, sigma = 4))
  })

  # Etykieta w kolorze roli (krój wykresu, jak dotąd).
  ch1_role_text <- function(x, y, label, role, ...) {
    annotate("text", x = x, y = y, label = label,
             colour = STEP_ROLES[[role]]$colour, fontface = "bold", ...)
  }

  zoom_plot_server("ch1_corr_plot", reactive({
    df <- ch1_corr_data()
    step <- ch1_corr_step()
    x_bar <- mean(df$x)
    y_bar <- mean(df$y)
    r <- cor(df$x, df$y)
    b1 <- r * sd(df$y) / sd(df$x)
    b0 <- y_bar - b1 * x_bar
    slope_x0 <- x_bar + 0.4
    slope_x1 <- slope_x0 + 1
    slope_y0 <- b0 + b1 * slope_x0
    slope_y1 <- b0 + b1 * slope_x1
    slope_y_mid <- (slope_y0 + slope_y1) / 2
    slope_y_pad <- max(1.8, abs(b1) * 1.4)

    # Stała rama z pełnych danych (z punktem x = 0 i b₀ z kroku 5).
    pad <- function(v, m = 0.05) v + c(-1, 1) * diff(v) * m
    frame <- step_frame(
      xlim = pad(range(c(df$x, 0))),
      ylim = pad(range(c(df$y, 0, b0, b1 * min(df$x))))
    )
    marker_df <- function(x, y) data.frame(x = x, y = y)

    # W kroku 4 dane ustępują konstrukcji nachylenia (tło).
    p <- ggplot(df, aes(x = x, y = y)) +
      step_layer(geom_point, if (step == 4) "background" else "data",
                 size = if (step == 4) 1.8 else 2.2) +
      labs(x = "X", y = "Y")

    if (step >= 1 && !(step %in% c(4, 5))) {
      role <- step_role(step, 1)
      p <- p +
        step_line(role, xintercept = x_bar) +
        step_line(role, yintercept = y_bar)
    }
    if (step == 2) {
      p <- p +
        step_layer(geom_segment, "new", mapping = aes(xend = x_bar, yend = y),
                   linetype = "22", alpha = 0.35, linewidth = 0.5) +
        step_layer(geom_segment, "new", mapping = aes(xend = x, yend = y_bar),
                   linetype = "22", alpha = 0.35, linewidth = 0.5) +
        ch1_role_text(x_bar, max(df$y), "odchylenia X", "new",
                      hjust = -0.05, vjust = 1) +
        ch1_role_text(min(df$x), y_bar, "odchylenia Y", "new",
                      hjust = 0, vjust = -0.7)
    }
    if (step == 3) {
      p <- p + step_layer(geom_abline, "new", intercept = b0, slope = b1,
                          linewidth = 1.3)
    }
    if (step == 4) {
      p <- p +
        step_layer(geom_abline, "known", intercept = b0, slope = b1,
                   linewidth = 1.8) +
        step_layer(geom_segment, "new",
                   data = data.frame(x = slope_x0, xend = slope_x1,
                                     y = slope_y0, yend = slope_y0),
                   mapping = aes(x = x, xend = xend, y = y, yend = yend),
                   linewidth = 1.2,
                   arrow = arrow(length = grid::unit(0.12, "inches"))) +
        step_layer(geom_segment, "new",
                   data = data.frame(x = slope_x1, xend = slope_x1,
                                     y = slope_y0, yend = slope_y1),
                   mapping = aes(x = x, xend = xend, y = y, yend = yend),
                   linewidth = 1.2,
                   arrow = arrow(length = grid::unit(0.12, "inches"))) +
        geom_point(
          data = marker_df(c(slope_x0, slope_x1, slope_x1),
                           c(slope_y0, slope_y0, slope_y1)),
          colour = STEP_ROLES$known$colour, fill = "white",
          shape = 21, stroke = 1.1, size = 3.2
        ) +
        ch1_role_text((slope_x0 + slope_x1) / 2, slope_y0, "ΔX = 1", "new",
                      vjust = 1.6) +
        ch1_role_text(slope_x1, (slope_y0 + slope_y1) / 2,
                      paste0("ΔY = b₁ = ", round(b1, 2)), "new", hjust = -0.08)
    }
    if (step == 5) {
      p <- p +
        step_layer(geom_abline, "known", intercept = 0, slope = b1,
                   linewidth = 1.2, linetype = "22") +
        step_layer(geom_abline, "known", intercept = b0, slope = b1,
                   linewidth = 1.5) +
        step_layer(geom_segment, "new",
                   data = data.frame(x = 0, xend = 0, y = 0, yend = b0),
                   mapping = aes(x = x, xend = xend, y = y, yend = yend),
                   linewidth = 1.2,
                   arrow = arrow(length = grid::unit(0.12, "inches"),
                                 ends = "both")) +
        geom_point(
          data = marker_df(0, c(0, b0)),
          colour = STEP_ROLES$known$colour, fill = "white",
          shape = 21, stroke = 1.1, size = 3
        ) +
        ch1_role_text(min(df$x), b1 * min(df$x), "b[0] == 0", "known",
                      parse = TRUE, hjust = 0, vjust = -0.6) +
        ch1_role_text(0.15, b0 / 2, paste0("b[0] == ", round(b0, 2)), "new",
                      parse = TRUE, hjust = 0)
    }

    # Krok 4 przybliża trójkąt nachylenia; pozostałe kroki mają wspólną ramę.
    if (step == 4) {
      p + coord_cartesian(
            xlim = c(slope_x0 - 1.1, slope_x1 + 1.45),
            ylim = c(slope_y_mid - slope_y_pad, slope_y_mid + slope_y_pad)
          ) +
        theme(legend.position = "none")
    } else {
      p + frame
    }
  }))

  # Opis kroku: statystyki z dotychczasowych kroków (dawniej kafelki).
  output$ch1_corr_text <- renderUI({
    df <- ch1_corr_data()
    step <- ch1_corr_step()

    x_bar <- mean(df$x)
    y_bar <- mean(df$y)
    sx <- sd(df$x)
    sy <- sd(df$y)
    r <- cor(df$x, df$y)
    b1 <- r * sy / sx
    b0 <- y_bar - b1 * x_bar

    if (step == 0) {
      return(paste0("Chmura ", nrow(df), " punktów: każdy punkt to jedna obserwacja (X, Y)."))
    }

    stat <- function(label, value) tagList(label, " = ", tags$b(value, .noWS = "outside"))
    stats <- list(
      if (step >= 1) stat("x̄", round(x_bar, 2)),
      if (step >= 1) stat("ȳ", round(y_bar, 2)),
      if (step >= 2) stat("sX", round(sx, 2)),
      if (step >= 2) stat("sY", round(sy, 2)),
      if (step >= 3) stat("r", round(r, 3))
    )
    stats <- Filter(Negate(is.null), stats)
    parts <- list(stats[[1]])
    for (s in stats[-1]) parts <- c(parts, list(", ", s))

    tagList(parts, HTML("."))
  })

  # Wzory nachylenia i wyrazu wolnego pod opisem kroku (kroki 4–5).
  output$ch1_corr_info <- renderUI({
    df <- ch1_corr_data()
    step <- ch1_corr_step()
    if (step < 4) return(NULL)

    x_bar <- mean(df$x)
    y_bar <- mean(df$y)
    sx <- sd(df$x)
    sy <- sd(df$y)
    r <- cor(df$x, df$y)
    b1 <- r * sy / sx
    b0 <- y_bar - b1 * x_bar

    tagList(
      lc_formula_box(
        withMathJax(helpText(sprintf("$$b_1 = r \\cdot \\frac{s_Y}{s_X} = %.3f \\cdot \\frac{%.2f}{%.2f} = %.3f$$",
                                     r, sy, sx, b1)))
      ),
      if (step >= 5) lc_formula_box(
        withMathJax(helpText(sprintf("$$b_0 = \\bar{y} - b_1\\bar{x} = %.2f - %.3f \\cdot %.2f = %.2f$$",
                                     y_bar, b1, x_bar, b0)))
      ),
      if (step >= 5) lc_status(
        p(tags$strong("Końcowy model:")),
        withMathJax(sprintf("$$\\hat{Y} = %.2f %s %.3fX$$", b0,
                            if (b1 < 0) "-" else "+", abs(b1)))
      )
    )
  })

  # --- Ćwiczenie: narysuj prostą z tabeli współczynników ---
  ch1_draw_model <- reactiveVal(NULL)
  ch1_draw_points <- reactiveVal(data.frame(x = numeric(), y = numeric()))
  ch1_draw_revealed <- reactiveVal(FALSE)

  .ch1_new_draw_model <- function() {
    beta0 <- runif(1, 2.5, 8.5)
    beta1 <- sample(c(-1.6, -1.2, -0.8, 0.8, 1.2, 1.6), 1)
    x <- runif(35, -4.5, 4.5)
    y <- beta0 + beta1 * x + rnorm(length(x), 0, 1.2)
    list(
      beta0 = beta0,
      beta1 = beta1,
      data = data.frame(x = x, y = y)
    )
  }

  ch1_draw_model(.ch1_new_draw_model())

  observeEvent(input$ch1_draw_new, {
    ch1_draw_model(.ch1_new_draw_model())
    ch1_draw_points(data.frame(x = numeric(), y = numeric()))
    ch1_draw_revealed(FALSE)
  })

  observeEvent(input$ch1_draw_reset, {
    ch1_draw_points(data.frame(x = numeric(), y = numeric()))
    ch1_draw_revealed(FALSE)
  })

  observeEvent(input$ch1_draw_reveal, {
    req(nrow(ch1_draw_points()) == 2)
    ch1_draw_revealed(TRUE)
  })

  observeEvent(input$ch1_draw_plot_click, {
    if (ch1_draw_revealed()) return()
    click <- input$ch1_draw_plot_click
    pts <- ch1_draw_points()
    new_pt <- data.frame(x = click$x, y = click$y)
    if (nrow(pts) >= 2) {
      pts <- new_pt
    } else {
      pts <- rbind(pts, new_pt)
    }
    ch1_draw_points(pts)
  })

  output$ch1_draw_table <- renderUI({
    model <- ch1_draw_model()
    lc_table(
      data.frame(
        c1 = c("wyraz wolny", "X"),
        c2 = I(list(
          sprintf("%.2f", model$beta0),
          sprintf("%.2f", model$beta1)
        ))
      ),
      cols = list(
        lc_col("c1", "Zmienna", "row"),
        lc_col("c2", "Estymata", "num")
      )
    )
  })

  zoom_plot_server("ch1_draw_plot", reactive({
    model <- ch1_draw_model()
    pts <- ch1_draw_points()
    revealed <- ch1_draw_revealed()
    x_min <- -5
    x_max <- 5
    y_min <- -4
    y_max <- 17
    grid_df <- data.frame(x = c(x_min, x_max), y = c(y_min, y_max))

    p <- ggplot(grid_df, aes(x = x, y = y)) +
      geom_blank() +
      geom_hline(yintercept = 0, color = upwr_rule, linewidth = 0.6) +
      geom_vline(xintercept = 0, color = upwr_rule, linewidth = 0.6) +
      coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE) +
      scale_x_continuous(breaks = seq(x_min, x_max, by = 1)) +
      scale_y_continuous(breaks = seq(y_min, y_max, by = 1)) +
      labs(x = "X", y = "Y") +
      theme_upwr()

    if (revealed) {
      p <- p +
        geom_point(data = model$data, aes(x = x, y = y),
                   inherit.aes = FALSE,
                   color = upwr_secondary, alpha = 0.45, size = 2)
    }

    if (nrow(pts) > 0) {
      p <- p +
        geom_point(data = pts, aes(x = x, y = y),
                   inherit.aes = FALSE,
                   color = unname(upwr_cat["terakota"]),
                   fill = "white", shape = 21, stroke = 1.2, size = 3.6) +
        geom_text(data = pts, aes(x = x, y = y, label = seq_len(nrow(pts))),
                  inherit.aes = FALSE,
                  color = unname(upwr_cat["terakota"]),
                  fontface = "bold", vjust = -1)
    }

    if (nrow(pts) == 2 && abs(diff(pts$x)) >= 0.05) {
      user_b1 <- diff(pts$y) / diff(pts$x)
      user_b0 <- pts$y[1] - user_b1 * pts$x[1]
      p <- p +
        geom_abline(intercept = user_b0, slope = user_b1,
                    color = unname(upwr_cat["terakota"]),
                    linewidth = 1.4, linetype = "longdash")
      if (revealed) {
        p <- p +
        geom_abline(intercept = model$beta0, slope = model$beta1,
                    color = unname(upwr_cat["niebo"]), linewidth = 1.5) +
        annotate("text", x = x_min + 0.25, y = y_max - 0.8,
                 label = "poprawna prosta", hjust = 0,
                 color = unname(upwr_cat["niebo"]), fontface = "bold") +
        annotate("text", x = x_min + 0.25, y = y_max - 1.8,
                 label = "Twoja prosta", hjust = 0,
                 color = unname(upwr_cat["terakota"]), fontface = "bold")
      } else {
        p <- p +
          annotate("text", x = x_min + 0.25, y = y_max - 0.8,
                   label = "Twoja prosta", hjust = 0,
                   color = unname(upwr_cat["terakota"]), fontface = "bold") +
          annotate("text", x = 0, y = y_max - 1,
                   label = "Kliknij „Pokaż odpowiedź”",
                   color = upwr_reference, size = 5)
      }
    } else if (nrow(pts) == 2) {
      p <- p + annotate("text", x = 0, y = y_max - 1,
                        label = "Wybierz punkty bardziej oddalone poziomo",
                        color = unname(upwr_cat["terakota"]), size = 5)
    } else {
      p <- p + annotate("text", x = 0, y = y_max - 1,
                        label = "Kliknij dwa punkty na wykresie",
                        color = upwr_reference, size = 5)
    }

    p
  }))

  output$ch1_draw_feedback <- renderUI({
    pts <- ch1_draw_points()
    if (nrow(pts) < 2) {
      return(lc_caption(
               if (nrow(pts) == 0) {
          "Kliknij pierwszy punkt prostej."
        } else {
          "Kliknij drugi punkt prostej."
        },
               tone = "info"
             ))
    }
    if (!ch1_draw_revealed()) {
      return(lc_caption(
               "Gotowe. Kliknij „Pokaż odpowiedź”, żeby porównać z modelem."
             ))
    }

    lc_caption(
      "Porównaj czerwoną przerywaną prostą z niebieską poprawną prostą.",
      tone = "ok"
    )
  })

  output$ch1_draw_stats <- renderUI({
    model <- ch1_draw_model()
    pts <- ch1_draw_points()
    if (nrow(pts) < 2 || !ch1_draw_revealed()) return(NULL)

    if (abs(diff(pts$x)) < 0.05) {
      return(lc_caption(
               "Punkty mają prawie ten sam X. Wybierz dwa punkty bardziej oddalone poziomo."
             ))
    }

    user_b1 <- diff(pts$y) / diff(pts$x)
    user_b0 <- pts$y[1] - user_b1 * pts$x[1]
    tagList(
        lc_readout("Twoje b₀", round(user_b0, 2), color = unname(upwr_cat["terakota"])),
        lc_readout("Poprawne b₀", round(model$beta0, 2), color = unname(upwr_cat["niebo"])),
        lc_readout("Twoje b₁", round(user_b1, 2), color = unname(upwr_cat["terakota"])),
        lc_readout("Poprawne b₁", round(model$beta1, 2), color = unname(upwr_cat["niebo"]))
    )
  })

  # --- Widget: OLS krok po kroku ---
  # Krok widgetu (1..6) żyje w przeglądarce; nowa próba nie zmienia kroku.
  ch1_ols_data <- reactiveVal(generate_regression_data(n = 70, beta0 = 4, beta1 = 1.4, sigma = 4))
  ch1_ols_step <- lc_step_server("ch1_ols", input)$step

  observeEvent(input$ch1_ols_new, {
    ch1_ols_data(generate_regression_data(n = 70, beta0 = 4, beta1 = 1.4, sigma = 4))
  })

  zoom_plot_server("ch1_ols_plot", reactive({
    df <- ch1_ols_data()
    step <- ch1_ols_step()
    model <- lm(y ~ x, data = df)
    coefs <- coef(model)
    df$fitted <- fitted(model)
    df$resid <- residuals(model)
    mean_y <- mean(df$y)
    alt_b1 <- coefs[2] * 0.45
    alt_b0 <- mean_y - alt_b1 * mean(df$x)
    df$alt_fitted <- alt_b0 + alt_b1 * df$x

    # Stała rama z pełnych danych (z prostą z kroku 6).
    pad <- function(v, m = 0.05) v + c(-1, 1) * diff(v) * m
    frame <- step_frame(xlim = pad(range(df$x)),
                        ylim = pad(range(c(df$y, df$fitted, df$alt_fitted))))

    p <- ggplot(df, aes(x = x, y = y)) +
      step_layer(geom_point, "data", size = 2) +
      labs(x = "X", y = "Y")

    if (step >= 2) {
      p <- p + step_line(step_role(step, 2), yintercept = mean_y)
    }
    if (step >= 3) {
      p <- p + step_layer(geom_smooth, step_role(step, 3), method = "lm",
                          formula = y ~ x, se = FALSE, linewidth = 1.2)
    }
    if (step >= 4) {
      p <- p + step_layer(geom_segment, step_role(step, 4),
                          mapping = aes(xend = x, yend = fitted), alpha = 0.35)
    }
    if (step >= 6) {
      p <- p +
        step_layer(geom_abline, "new", intercept = alt_b0, slope = alt_b1,
                   linewidth = 1.1, linetype = "longdash") +
        step_layer(geom_segment, "new", mapping = aes(xend = x, yend = alt_fitted),
                   alpha = 0.22) +
        annotate("text", x = min(df$x), y = max(df$y),
                 label = "inna prosta", hjust = 0, vjust = 1,
                 colour = STEP_ROLES$new$colour, fontface = "bold") +
        annotate("text", x = min(df$x), y = max(df$y) - 0.1 * diff(range(df$y)),
                 label = "MNK", hjust = 0, vjust = 1,
                 colour = STEP_ROLES$known$colour, fontface = "bold")
    }
    p + frame
  }))

  output$ch1_ols_text <- renderUI({
    df <- ch1_ols_data()
    step <- ch1_ols_step()
    model <- lm(y ~ x, data = df)
    coefs <- coef(model)
    sse <- sum(residuals(model)^2)
    alt_b1 <- coefs[2] * 0.45
    alt_b0 <- mean(df$y) - alt_b1 * mean(df$x)
    alt_sse <- sum((df$y - (alt_b0 + alt_b1 * df$x))^2)
    ss_res <- tagList("SS", tags$sub("res", .noWS = "before"))
    if (step == 6) {
      return(tagList(
        ss_res, " prostej MNK = ", tags$b(round(sse, 1)), ", ", ss_res,
        " innej prostej = ", tags$b(round(alt_sse, 1)),
        " (+", round((alt_sse / sse - 1) * 100, 1), "%)."
      ))
    }
    switch(as.character(step),
      "1" = "Najpierw mamy tylko punkty: pary obserwacji X i Y.",
      "2" = "Pozioma linia to średnia Y. To najprostszy model bez predyktora.",
      "3" = "Prosta MNK: najmniejsza suma kwadratów pionowych odległości od punktów.",
      "4" = "Każdy odcinek to reszta: obserwacja minus predykcja.",
      "5" = tagList("Model: Ŷ = ", tags$b(round(coefs[1], 2)), " + ",
                    tags$b(round(coefs[2], 2)), "X; ", ss_res, " = ",
                    tags$b(round(sse, 1)), ".")
    )
  })

  # --- Widget: p-wartość dla nachylenia ---
  ch1_pval_data <- reactive({
    scenario <- input$ch1_pval_scenario
    if (is.null(scenario)) scenario <- "strong_positive"
    seed <- switch(scenario,
      "strong_positive" = 3101,
      "none" = 3102,
      "strong_negative" = 3103,
      "small_sample" = 3104
    )
    set.seed(seed)

    params <- switch(scenario,
      "strong_positive" = list(n = 70, beta0 = 6, beta1 = 1.25, sigma = 3.0,
                               title = "Duży efekt i umiarkowany szum"),
      "none" = list(n = 70, beta0 = 6, beta1 = 0.00, sigma = 4.2,
                    title = "Brak systematycznego trendu"),
      "strong_negative" = list(n = 70, beta0 = 8, beta1 = -1.15, sigma = 3.0,
                               title = "Ujemne nachylenie"),
      "small_sample" = list(n = 14, beta0 = 6, beta1 = 1.25, sigma = 5.0,
                            title = "Trend podobny, ale mniej danych")
    )

    x <- runif(params$n, -4, 4)
    y <- params$beta0 + params$beta1 * x + rnorm(params$n, 0, params$sigma)
    data.frame(x = x, y = y, title = params$title)
  })

  ch1_pval_model <- reactive({
    lm(y ~ x, data = ch1_pval_data())
  })

  output$ch1_pval_table <- renderUI({
    model <- ch1_pval_model()
    coefs <- broom::tidy(model)
    coefs$term <- ifelse(coefs$term == "(Intercept)", "wyraz wolny", "X")

    coefs$p_txt <- lc_pval(coefs$p.value)
    lc_table(as.data.frame(coefs),
      cols = list(
        lc_col("term", "Zmienna", "row"),
        lc_col("estimate", "Estymata", digits = 2),
        lc_col("statistic", "t", digits = 2),
        lc_col("p_txt", "p")
      )
    )
  })

  zoom_plot_server("ch1_pval_plot", reactive({
    df <- ch1_pval_data()
    model <- ch1_pval_model()
    p_val <- broom::tidy(model)$p.value[2]
    is_sig <- p_val < 0.05
    line_color <- if (is_sig) unname(upwr_cat["niebo"]) else upwr_reference

    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = upwr_secondary, alpha = 0.55, size = 2.1) +
      geom_hline(yintercept = mean(df$y), color = unname(upwr_cat["bursztyn"]),
                 linetype = "dashed", linewidth = 0.9) +
      geom_smooth(method = "lm", se = TRUE,
                  color = line_color, fill = line_color,
                  linewidth = 1.4, alpha = 0.16) +
      annotate("label", x = min(df$x), y = max(df$y),
               hjust = 0, vjust = 1,
               label = if (is_sig) "p < 0.05: nachylenie istotne" else "p ≥ 0.05: brak istotności",
               color = line_color, fill = "white", linewidth = 0) +
      labs(x = "X", y = "Y") +
      theme_upwr()
  }))

  output$ch1_pval_verdict <- renderUI({
    model <- ch1_pval_model()
    coefs <- broom::tidy(model)
    p_val <- coefs$p.value[2]
    b1 <- coefs$estimate[2]
    is_sig <- p_val < 0.05

    if (is_sig) {
      lc_status(
        lc_verdict(tags$strong("Wniosek: "), type = "ok"),
        sprintf("odrzucamy H₀. Nachylenie b₁ = %.2f jest istotnie różne od zera.", b1)
      )
    } else {
      lc_status(
        lc_verdict(tags$strong("Wniosek: "), type = "warning"),
        sprintf("nie odrzucamy H₀. Dane nie dają mocnych podstaw, by uznać nachylenie b₁ = %.2f za różne od zera.", b1)
      )
    }
  })

  output$ch1_pval_stats <- renderUI({
    model <- ch1_pval_model()
    coefs <- broom::tidy(model)
    p_val <- coefs$p.value[2]

    tagList(
      lc_readout("b₁", round(coefs$estimate[2], 2), color = unname(upwr_cat["szalwia"])),
      lc_readout("SE(b₁)", round(coefs$std.error[2], 2), color = upwr_secondary),
      lc_readout("t", round(coefs$statistic[2], 2), color = unname(upwr_cat["bursztyn"])),
      lc_readout("p-wartość", if (p_val < 0.001) "< 0.001" else round(p_val, 3), color = if (p_val < 0.05) unname(upwr_cat["niebo"]) else upwr_reference)
    )
  })

  # --- CASchools: output + quiz interpretacyjny ---
  ch1_cas_revealed <- reactiveVal(FALSE)

  observeEvent(input$ch1_cas_reveal, {
    ch1_cas_revealed(TRUE)
  })

  observeEvent(list(input$ch1_cas_x, input$ch1_cas_y), {
    ch1_cas_revealed(FALSE)
  }, ignoreInit = TRUE)

  ch1_cas_model <- reactive({
    req(input$ch1_cas_x, input$ch1_cas_y)
    validate(need(input$ch1_cas_x != input$ch1_cas_y, "Wybierz dwie różne zmienne."))
    if (identical(input$ch1_cas_x, "grades")) {
      df <- .cas_data
      df$grades01 <- ifelse(df$grades == "KK-08", 1, 0)
      lm(as.formula(paste(input$ch1_cas_y, "~ grades01")), data = df)
    } else {
      form <- as.formula(paste(input$ch1_cas_y, "~", input$ch1_cas_x))
      lm(form, data = .cas_data)
    }
  })

  zoom_plot_server("ch1_cas_plot", reactive({
    req(input$ch1_cas_x, input$ch1_cas_y)
    validate(need(input$ch1_cas_x != input$ch1_cas_y, "Wybierz dwie różne zmienne."))

    if (identical(input$ch1_cas_x, "grades")) {
      df <- .cas_data
      df$grades01 <- ifelse(df$grades == "KK-08", 1, 0)
      model <- ch1_cas_model()
      pred_df <- data.frame(
        grades = c("KK-06", "KK-08"),
        grades01 = c(0, 1)
      )
      pred_df$pred <- predict(model, newdata = pred_df)

      ggplot(df, aes(x = grades, y = .data[[input$ch1_cas_y]])) +
        geom_jitter(width = 0.12, height = 0, color = upwr_secondary,
                    alpha = 0.42, size = 1.8) +
        stat_summary(fun = mean, geom = "point",
                     color = unname(upwr_cat["terakota"]), size = 3.4) +
        geom_crossbar(data = pred_df,
                      aes(x = grades, y = pred, ymin = pred, ymax = pred),
                      inherit.aes = FALSE,
                      color = unname(upwr_cat["niebo"]), fill = NA,
                      linewidth = 0.8, width = 0.55) +
        labs(
          x = unname(.cas_labels[input$ch1_cas_x]),
          y = unname(.cas_labels[input$ch1_cas_y])
        ) +
        theme_upwr()
    } else {
      ggplot(.cas_data, aes(x = .data[[input$ch1_cas_x]], y = .data[[input$ch1_cas_y]])) +
        geom_point(color = upwr_secondary, alpha = 0.45, size = 1.8) +
        geom_smooth(method = "lm", se = TRUE,
                    color = unname(upwr_cat["niebo"]),
                    fill = unname(upwr_cat["niebo"]), alpha = 0.15) +
        labs(
          x = unname(.cas_labels[input$ch1_cas_x]),
          y = unname(.cas_labels[input$ch1_cas_y])
        ) +
        theme_upwr()
    }
  }))

  output$ch1_cas_table <- renderUI({
    req(input$ch1_cas_x, input$ch1_cas_y)
    if (input$ch1_cas_x == input$ch1_cas_y) {
      return(lc_caption(
               "Wybierz dwie różne zmienne."
             ))
    }

    model <- ch1_cas_model()
    coefs <- broom::tidy(model)
    coefs$term <- ifelse(coefs$term == "(Intercept)", "wyraz wolny",
                         ifelse(coefs$term == "grades01", "Zakres klas: KK-08 vs KK-06",
                                unname(.cas_labels[input$ch1_cas_x])))

    coefs$p_txt <- lc_pval(coefs$p.value)
    lc_table(as.data.frame(coefs),
      cols = list(
        lc_col("term", "Zmienna", "row"),
        lc_col("estimate", "Estymata", digits = 3),
        lc_col("std.error", "SE", digits = 3),
        lc_col("statistic", "t", digits = 2),
        lc_col("p_txt", "p")
      )
    )
  })

  output$ch1_cas_answer <- renderUI({
    req(input$ch1_cas_x, input$ch1_cas_y)
    if (input$ch1_cas_x == input$ch1_cas_y) return(NULL)

    if (!ch1_cas_revealed()) {
      return(lc_caption(
               "Zanim odsłonisz odpowiedź, odczytaj z tabeli znak b₁ i p-wartość."
             ))
    }

    model <- ch1_cas_model()
    coefs <- broom::tidy(model)
    p_val <- coefs$p.value[2]
    b1 <- coefs$estimate[2]
    x_label <- unname(.cas_labels[input$ch1_cas_x])
    y_label <- unname(.cas_labels[input$ch1_cas_y])
    relation <- if (b1 > 0) "dodatni" else "ujemny"

    if (p_val < 0.05) {
      lc_status(
        lc_verdict(tags$strong("Odpowiedź: "), type = "ok"),
        if (identical(input$ch1_cas_x, "grades")) {
          sprintf("tak, %s istotnie przewiduje %s. Okręgi KK-08 różnią się od KK-06 średnio o %.3f punktu, p = %.3g.",
                  x_label, y_label, b1, p_val)
        } else {
          sprintf("tak, %s istotnie przewiduje %s. Efekt jest %s: b₁ = %.3f, p = %.3g.",
                  x_label, y_label, relation, b1, p_val)
        }
      )
    } else {
      lc_status(
        lc_verdict(tags$strong("Odpowiedź: "), type = "warning"),
        if (identical(input$ch1_cas_x, "grades")) {
          sprintf("nie mamy podstaw, by uznać różnicę między KK-08 i KK-06 w %s za istotną: b₁ = %.3f, p = %.3g.",
                  y_label, b1, p_val)
        } else {
          sprintf("nie mamy podstaw, by uznać wpływ %s na %s za istotny: b₁ = %.3f, p = %.3g.",
                  x_label, y_label, b1, p_val)
        }
      )
    }
  })

  output$ch1_cas_summary <- renderUI({
    req(input$ch1_cas_x, input$ch1_cas_y)
    if (input$ch1_cas_x == input$ch1_cas_y || !ch1_cas_revealed()) return(NULL)

    model <- ch1_cas_model()
    coefs <- broom::tidy(model)
    x_label <- unname(.cas_labels[input$ch1_cas_x])
    y_label <- unname(.cas_labels[input$ch1_cas_y])

    if (identical(input$ch1_cas_x, "grades")) {
      b0 <- coefs$estimate[1]
      b1 <- coefs$estimate[2]
      y0 <- b0
      y1 <- b0 + b1

      return(tagList(
        lc_readouts(
          lc_readout("b₀", round(b0, 2), color = upwr_secondary),
          lc_readout("b₁", round(b1, 3), color = unname(upwr_cat["szalwia"])),
          lc_readout("p dla b₁", signif(coefs$p.value[2], 3), color = unname(upwr_cat["bursztyn"]))
        ),
        lc_formula_box(
          withMathJax(helpText(sprintf(
            "$$\\hat{Y} = %.2f %+ .2f \\cdot X_{\\text{KK-08}}$$",
            b0, b1
          ))),
          p(tags$strong("Kodowanie:"), " KK-06 = 0, KK-08 = 1."),
          withMathJax(helpText(sprintf(
            "$$\\text{KK-06: } \\hat{Y} = %.2f %+ .2f \\cdot 0 = %.2f$$",
            b0, b1, y0
          ))),
          withMathJax(helpText(sprintf(
            "$$\\text{KK-08: } \\hat{Y} = %.2f %+ .2f \\cdot 1 = %.2f$$",
            b0, b1, y1
          )))
        ),
        lc_status(
          p(tags$strong("Interpretacja: "),
            paste0("w tym kodowaniu b₀ to średni przewidywany ", y_label,
                   " dla KK-06, a b₁ to różnica KK-08 minus KK-06."))
        )
      ))
    }

    tagList(
      lc_readouts(
        lc_readout("b₀", round(coefs$estimate[1], 2), color = upwr_secondary),
        lc_readout("b₁", round(coefs$estimate[2], 3), color = unname(upwr_cat["szalwia"])),
        lc_readout("p dla b₁", signif(coefs$p.value[2], 3), color = unname(upwr_cat["bursztyn"]))
      ),
      lc_status(
        p(tags$strong("Interpretacja: "),
          paste0("gdy ", x_label, " rośnie o 1, przewidywane ", y_label,
                 " zmienia się średnio o ", round(coefs$estimate[2], 3), "."))
      )
    )
  })

  # --- CASchools: pierwsza predykcja z modelu ---
  ch1_pred_revealed <- reactiveVal(FALSE)

  ch1_pred_spec <- reactive({
    case <- input$ch1_pred_case
    if (is.null(case)) case <- "read_students"
    .ch1_pred_specs[[case]]
  })

  ch1_pred_model <- reactive({
    spec <- ch1_pred_spec()
    form <- as.formula(paste(spec$y, "~", spec$x))
    lm(form, data = .cas_data)
  })

  observeEvent(input$ch1_pred_reveal, {
    req(input$ch1_pred_x)
    ch1_pred_revealed(TRUE)
  })

  observeEvent(list(input$ch1_pred_case, input$ch1_pred_x), {
    ch1_pred_revealed(FALSE)
  }, ignoreInit = TRUE)

  output$ch1_pred_x_input <- renderUI({
    spec <- ch1_pred_spec()
    x_vals <- .cas_data[[spec$x]]
    numericInput(
      "ch1_pred_x",
      label = paste0("Wartość X (", spec$unit, "):"),
      value = spec$default,
      min = floor(min(x_vals, na.rm = TRUE)),
      max = ceiling(max(x_vals, na.rm = TRUE)),
      step = spec$step
    )
  })

  output$ch1_pred_question <- renderUI({
    spec <- ch1_pred_spec()
    req(input$ch1_pred_x)
    lc_status(
      p(tags$strong("Pytanie: "), spec$question),
      p("Podstaw do równania wartość X = ",
        tags$strong(input$ch1_pred_x), " i spróbuj policzyć przewidywane Y.")
    )
  })

  # Współczynnik do ręcznego liczenia: małe wartości z cyframi znaczącymi
  # (b₁ = -0.00097 zamiast -0.001), duże z dwoma miejscami po kropce.
  .ch1_fmt_coef <- function(x) {
    if (abs(x) >= 1) sprintf("%.2f", x) else format(signif(x, 3), scientific = FALSE)
  }

  output$ch1_pred_table <- renderUI({
    spec <- ch1_pred_spec()
    model <- ch1_pred_model()
    coefs <- broom::tidy(model)
    coefs$term <- ifelse(coefs$term == "(Intercept)", "wyraz wolny", unname(.cas_labels[spec$x]))

    lc_table(
      data.frame(term = coefs$term,
                 estimate = vapply(coefs$estimate, .ch1_fmt_coef, character(1))),
      cols = list(
        lc_col("term", "Zmienna", "row"),
        lc_col("estimate", "Estymata")
      )
    )
  })

  zoom_plot_server("ch1_pred_plot", reactive({
    spec <- ch1_pred_spec()
    model <- ch1_pred_model()
    coefs <- coef(model)
    x0 <- input$ch1_pred_x
    if (is.null(x0)) x0 <- spec$default
    y_hat <- unname(coefs[1] + coefs[2] * x0)

    p <- ggplot(.cas_data, aes(x = .data[[spec$x]], y = .data[[spec$y]])) +
      geom_point(color = upwr_secondary, alpha = 0.42, size = 1.8) +
      geom_smooth(method = "lm", se = TRUE,
                  color = unname(upwr_cat["niebo"]),
                  fill = unname(upwr_cat["niebo"]), alpha = 0.15) +
      labs(
        x = unname(.cas_labels[spec$x]),
        y = unname(.cas_labels[spec$y])
      ) +
      theme_upwr()

    if (ch1_pred_revealed()) {
      p <- p +
        geom_vline(xintercept = x0, color = unname(upwr_cat["bursztyn"]),
                   linetype = "dashed", linewidth = 0.9) +
        geom_hline(yintercept = y_hat, color = unname(upwr_cat["terakota"]),
                   linetype = "dashed", linewidth = 0.9) +
        annotate("point", x = x0, y = y_hat,
                 color = unname(upwr_cat["terakota"]),
                 fill = "white", shape = 21, stroke = 1.2, size = 4) +
        annotate("text", x = x0, y = y_hat,
                 label = paste0("Ŷ = ", round(y_hat, 1)),
                 hjust = -0.1, vjust = -0.8,
                 color = unname(upwr_cat["terakota"]), fontface = "bold")
    }

    p
  }))

  output$ch1_pred_answer <- renderUI({
    spec <- ch1_pred_spec()
    req(input$ch1_pred_x)

    if (!ch1_pred_revealed()) {
      return(lc_caption(
               "Odpowiedź jest ukryta. Najpierw policz predykcję z tabeli współczynników."
             ))
    }

    coefs <- coef(ch1_pred_model())
    x0 <- input$ch1_pred_x
    # Liczymy z wartości pokazanych w tabeli, żeby rachunek ręczny się zgadzał.
    b0_txt <- .ch1_fmt_coef(coefs[1])
    b1_txt <- .ch1_fmt_coef(abs(coefs[2]))
    y_hat <- as.numeric(b0_txt) + sign(coefs[2]) * as.numeric(b1_txt) * x0
    x_label <- unname(.cas_labels[spec$x])
    y_label <- unname(.cas_labels[spec$y])

    tagList(
      lc_status(
        p(tags$strong("Odpowiedź:")),
        withMathJax(sprintf("$$\\hat{Y} = %s %s %s \\cdot %s = %.2f$$",
                  b0_txt, if (coefs[2] < 0) "-" else "+", b1_txt,
                  format(x0), y_hat)),
        p(sprintf("Dla %s = %s przewidywane %s wynosi %.2f.",
                  x_label, format(x0), y_label, y_hat))
      )
    )
  })

  output$ch1_pred_stats <- renderUI({
    if (!ch1_pred_revealed()) return(NULL)
    spec <- ch1_pred_spec()
    model <- ch1_pred_model()
    coefs <- coef(model)
    x0 <- input$ch1_pred_x
    y_hat <- unname(coefs[1] + coefs[2] * x0)

    tagList(
      lc_readout("b₀", .ch1_fmt_coef(coefs[1]), color = upwr_secondary),
      lc_readout("b₁", .ch1_fmt_coef(coefs[2]), color = unname(upwr_cat["szalwia"])),
      lc_readout(paste("X:", unname(.cas_labels[spec$x])), round(x0, 2), color = unname(upwr_cat["bursztyn"])),
      lc_readout(paste("Ŷ:", unname(.cas_labels[spec$y])), round(y_hat, 2), color = unname(upwr_cat["terakota"]))
    )
  })
}
