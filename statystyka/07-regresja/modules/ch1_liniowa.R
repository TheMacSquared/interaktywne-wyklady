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

# Prosta Y = β₀ + β₁X, trójkąt nachylenia i błąd ε
# jednego punktu. Kolory jak w znacznikach wzorów: β₀ terakota, β₁ indygo,
# ε bursztyn, obserwacje niebo.
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

    lc_p("W wykładzie 02 związek dwóch ", gloss("zmienna ilościowa", "zmiennych ilościowych"), "
      opisywaliśmy współczynnikiem ", gloss("korelacja", "korelacji"), " \\(r\\). Mówił on o kierunku i sile
      związku liniowego, a test korelacji z wykładu 05 sprawdzał, czy \\(r\\) da się odróżnić
      od zera. Jedno pytanie zostało bez odpowiedzi: o ile zmienia się
      \\(Y\\), gdy \\(X\\) rośnie o jednostkę. Trzy chmury punktów o nachyleniach
      0.4, 0.8 i 1.6 miały prawie to samo \\(r \\approx 0.96\\). Pytanie
      „o ile?” należy do regresji."),

    lc_p(gloss("regresja liniowa", "Regresja liniowa"), " opisuje związek prostą.
      Zanim się ją dopasuje, trzeba obejrzeć wykres rozrzutu. Prosta ma sens
      tylko wtedy, gdy chmura punktów układa się w przybliżeniu wzdłuż linii.
      Przy krzywiźnie, dwóch oddzielnych chmurach albo pojedynczej wartości
      odstającej prosta źle opisze dane, choć jej współczynniki da się policzyć
      zawsze. To te same pułapki, które w wykładzie 05 psuły współczynnik
      korelacji."),

    lc_p("Gdy jest jedna zmienna objaśniająca \\(X\\), mówimy o ",
      gloss("regresja prosta", "regresji liniowej prostej"), ". Model zapisuje
      każdą wartość \\(Y\\) jako punkt na prostej plus odchylenie losowe:"),

    # Model regresji prostej ze znacznikami symboli
    lc_formula_annotated(
      "⟦1|Y⟧ = ⟦2|\\beta_0⟧ + ⟦3|\\beta_1⟧ ⟦4|X⟧ + ⟦5|\\varepsilon⟧",
      notes  = list("zmienna zależna", "wyraz wolny", "nachylenie", "predyktor",
                    "błąd losowy"),
      colors = c("niebo", "terakota", "indygo", "niebo", "bursztyn"),
      above  = c(2, 4)
    ),

    # Rycina z β₀, β₁ i ε na jednej prostej
    figure_panel(
      label = "Ryc. 1.1", title = "Z czego składa się prosta regresji",
      width_mode = "text",
      ch1_anatomy_fig()
    ),

    lc_p("\\(\\beta_0\\) to ", gloss("wyraz wolny"), ", czyli wysokość prostej
      w punkcie \\(X = 0\\). \\(\\beta_1\\) to ",
      gloss("współczynnik regresji", "nachylenie"), ": o ile zmienia się średnie
      \\(Y\\), gdy \\(X\\) rośnie o jednostkę. \\(\\varepsilon\\) to błąd losowy,
      czyli ta część \\(Y\\), której prosta nie wyjaśnia. Greckie litery oznaczają
      nieznane ", gloss("parametr", "parametry"), " populacji. Z próby liczymy
      ich ", gloss("estymator", "estymatory"), ", oznaczane łacińskimi literami
      \\(b_0\\) i \\(b_1\\), tak jak w wykładzie 04 średnia z próby szacowała
      średnią populacji."),

    lc_p("Panel poniżej generuje dane z tego modelu dla wybranych wartości
      \\(\\beta_0\\), \\(\\beta_1\\) i rozrzutu błędu losowego \\(\\varepsilon\\), czyli jego ",
      gloss("odchylenie standardowe", "odchylenia standardowego"), "."),

    figure_panel(
      label = "Ryc. 1.2", title = "Co robią β₀, β₁ i szum?",
      full_width = TRUE,
      # Wykres i opis liczy przeglądarka (modules/figures.js, rycina s07_beta).
      lc_sf_figure("s07_beta", config = ch1_beta_config,
        control = list(
          lc_sf_slider("ch1_beta_b0", "β₀ (punkt startu)", -10, 20, 5, 1),
          lc_sf_slider("ch1_beta_b1", "β₁ (nachylenie)", -3, 3, 1, 0.25),
          lc_sf_slider("ch1_beta_sigma", "Szum ε", 0, 8, 2, 0.5)
        ),
        ratio = "2.6/1", max_height = "320px",
        after = tags$div(`data-role` = "info"))
    ),

    lc_p("\\(\\beta_0\\) przesuwa całą prostą w górę i w dół, nie zmieniając jej
      kąta. \\(\\beta_1\\) obraca prostą: przy wartościach dodatnich prosta rośnie,
      przy ujemnych opada, a przy zerze jest pozioma i znajomość \\(X\\) nic nie
      mówi o \\(Y\\). Błąd \\(\\varepsilon\\) nie rusza prostej, tylko rozrzuca punkty
      wokół niej. Bez błędu wszystkie punkty leżą na linii, przy dużym rozrzucie
      \\(\\varepsilon\\) trend ginie w szumie. Korelacja \\(r\\) zależy od nachylenia
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

    # B₁ i b₀ z korelacji
    lc_formula_annotated(
      "⟦1|b_1⟧ = ⟦2|r⟧ \\cdot \\frac{⟦3|s_Y⟧}{⟦4|s_X⟧}, \\qquad ⟦5|b_0⟧ = ⟦6|\\bar{y}⟧ - b_1 ⟦7|\\bar{x}⟧",
      notes  = list("nachylenie", "korelacja", "odch. std. Y", "odch. std. X",
                    "wyraz wolny", "średnia Y", "średnia X"),
      colors = c("indygo", "accent", "wrzos", "wrzos", "terakota", "accent", "accent"),
      above  = c(1, 3, 6)
    ),

    lc_p("Pierwszy wzór czyta się tak: \\(r\\) mówi, o ile odchyleń
      standardowych zmienia się średnio \\(Y\\), gdy \\(X\\) rośnie o jedno
      odchylenie standardowe. Mnożenie przez \\(s_Y / s_X\\) przelicza to na
      jednostki obu zmiennych. Drugi wzór sprawia, że prosta przechodzi przez
      punkt \\((\\bar{x}, \\bar{y})\\): obserwacji o przeciętnym \\(X\\) prosta
      przypisuje przeciętne \\(Y\\). Panel buduje prostą w tej kolejności na
      losowej próbie 65 punktów."),

    figure_panel(
      label = "Ryc. 1.3",
      full_width = TRUE,
      # Losowanie, kroki i opisy liczy przeglądarka (modules/figures.js, rycina s07_corr).
      lc_sf_figure("s07_corr", bare = TRUE, body =
      lc_step_widget("ch1_corr",
        title = "Jak policzyć regresję z korelacji?",
        steps = c("Dane", "Średnie X i Y", "Odchylenia standardowe", "Korelacja r",
                  "Nachylenie b₁", "Wyraz wolny b₀"),
        toolbar = lc_toolbar(
          lc_sf_action("new", "ch1_corr_new", "Nowa próba", icon = "shuffle", variant = "outline"),
          lc_chart_zoom_button()
        ),
        body = lc_sf_chart(0, "2/1", "460px"),
        extra = tags$div(`data-role` = "info")
      ))
    ),

    lc_p("Odchylenia standardowe są zawsze dodatnie, więc znak \\(b_1\\) jest
      taki sam jak znak \\(r\\), a \\(b_1 = 0\\) dokładnie wtedy, gdy \\(r = 0\\).
      Korelacja ustala kierunek i siłę związku, a iloraz odchyleń standardowych
      przelicza ją na jednostki \\(X\\) i \\(Y\\). Prosta otrzymana w ten sposób
      jest dokładnie tą, którą program statystyczny podaje w tabeli wyników
      regresji z jednym ", gloss("predyktor", "predyktorem"), "."),

    # ========================================================================
    # Regresja do średniej
    # ========================================================================
    lc_h2("ch1-do-sredniej", "Regresja do średniej"),

    lc_p("Wzór na nachylenie ma ważną konsekwencję. Gdy obie zmienne mają tę
      samą skalę i podobny rozrzut, \\(s_Y / s_X \\approx 1\\) i nachylenie
      jest po prostu równe \\(r\\). Jeśli korelacja nie jest idealna,
      \\(r < 1\\), więc prosta przewiduje dla obserwacji o skrajnym \\(X\\)
      wartość \\(Y\\) bliższą średniej niż samo \\(X\\). To zjawisko nazywa
      się regresją do średniej i od niego pochodzi nazwa całej metody."),

    lc_p("Zanim zobaczymy to na danych, zróbmy doświadczenie: ta sama grupa
      studentów pisze dwa kolokwia."),

    # PROTOTYP SCENY (2026-10-08): napisz kolokwium — regresja do średniej
    figure_panel(
      label = "Prototyp sceny",
      width_mode = "text",
      scene_widget("ch1_sc_rtm", "Dwa kolokwia: co się dzieje z najlepszymi?",
        steps = c("Kolokwium", "Najlepsi", "Drugie podejście", "Umiejętność"),
        labels = c("Napisz kolokwium", "Napisz kolokwium", "Napisz oba kolokwia", "Napisz oba kolokwia"),
        options = list(
          list(name = "luck", label = "Udział losu",
               values = c("brak" = "0", "mały" = "3", "średni" = "7", "duży" = "12"), selected = "7"),
          list(name = "grp", label = "Wybieramy", from = 2,
               values = c("10 najlepszych" = "top", "10 najsłabszych" = "bottom"), selected = "top")
        ),
        config = list(kind = "rtm", n = 100L, k = 10L, mu = 60, sd_skill = 9, luck = 7, grp = "top",
                      aria = "Wyniki stu studentów z dwóch kolokwiów, strzałki od pierwszego do drugiego wyniku dziesięciu wybranych i histogram zmian ich średniej"),
        texts = list(
        tagList("Każda kropka to student: wynik to ukryta umiejętność plus los dnia. Napisz kolokwium
        kilka razy."),
        tagList("Trójkąt to średnia 10 najlepszych. Za chwilę ci sami napiszą kolokwium jeszcze raz."),
        tagList("Drugie kolokwium, ta sama trudność, nowy los dnia. Zmiana średniej wybranych spada
        do histogramu. Dokładaj grupy, potem zmień udział losu albo wybierz 10 najsłabszych."),
        tagList("Puste kółka to prawdziwa umiejętność wybranych, linia przerywana to jej średnia.
        Najlepszym dopisało szczęście, najsłabszym go zabrakło, więc w drugim podejściu wracają
        do swojej umiejętności. Bez losu nie ma powrotu.")
        )
      )
    ),

    lc_p("Panel pokazuje 100 studentów i ich wyniki z dwóch kolokwiów o tej
      samej skali. Średnia wynosi około 61 punktów na obu, a korelacja wyników
      \\(r = 0.59\\). Linia przerywana to przekątna „bez zmian”: student na
      niej dostał na obu kolokwiach tyle samo. Strzałka prowadzi od przekątnej
      do średniego wyniku wybranej grupy na drugim kolokwium."),

    figure_panel(
      label = "Ryc. 1.4", title = "Najlepsi i najsłabsi na drugim kolokwium",
      full_width = TRUE,
      ch1_rtm_fig()
    ),

    lc_p("Dziesięciu najlepszych na pierwszym kolokwium miało średnio 79.7
      punktu, a na drugim 69.6. Dziewięciu z nich wypadło gorzej. Dziesięciu
      najsłabszych poprawiło się z 43.5 do 51.9 punktu, i też dziewięciu
      z nich. Średnia wszystkich studentów prawie się nie zmieniła (60.7
      i 60.8). Prosta regresji przewidywała ten ruch: dla średniego wyniku
      79.7 daje 72.2 punktu, a dla 43.5 daje 50.6."),

    lc_p("Nikt tu nie obniżył lotów ani nagle się nie nauczył. Wynik kolokwium
      to umiejętność plus forma dnia, a grupa wybrana za skrajny wynik to
      w dużej części osoby, którym akurat dopisało szczęście albo które miały
      gorszy dzień. Za drugim razem ich szczęście jest przeciętne, więc wynik
      wraca w stronę średniej. Działa to w obie strony: dziesięciu najlepszych
      na drugim kolokwium miało na pierwszym średnio 65.6 punktu."),

    lc_note("Pułapka",
      "Regresja do średniej łatwo udaje skutek działania. Jeśli najsłabszym
       po pierwszym kolokwium zaproponowano konsultacje, a najlepszych
       pochwalono, to poprawa pierwszych i spadek drugich pojawią się nawet
       wtedy, gdy ani konsultacje, ani pochwała niczego nie zmieniły. Żeby
       ocenić efekt, potrzebna jest grupa porównawcza wybrana tak samo,
       która go nie dostała."
    ),

    # ========================================================================
    # Ćwiczenie: narysuj prostą z tabeli
    # ========================================================================
    lc_h2("ch1-rysuj-z-tabeli", "Ćwiczenie: narysuj prostą z tabeli"),

    lc_p("Tabela wyników regresji zawiera dla każdego współczynnika cztery liczby:
      ", gloss("estymata", "estymatę"), ", ", gloss("błąd standardowy"), ", statystykę \\(t\\)
      i p-wartość. Do końca rozdziału nauczymy się czytać je wszystkie.
      Zaczniemy od najprostszej czynności: odczytania z tabeli dwóch estymat
      i narysowania prostej, którą opisują."),

    lc_p("Tabela w ćwiczeniu zawiera dwie liczby: wyraz wolny i współczynnik
      przy \\(X\\). Prostą wyznaczają dowolne dwa jej punkty, więc wystarczy
      podstawić do równania dwie wartości \\(X\\) i zaznaczyć na wykresie
      otrzymane \\(Y\\). Po odsłonięciu odpowiedzi panel dorysuje poprawną prostą
      i dane, z których ją policzono."),

    figure_panel(
      label = "Ćwiczenie", title = "Kliknij dwa punkty, przez które przechodzi prosta",
      full_width = TRUE,
      # Kliknięcia, punkty i porównanie liczy przeglądarka (modules/figures.js, rycina s07_draw).
      lc_sf_figure("s07_draw", ratio = "1.7/1", max_height = "400px",
        toolbar = tagList(
          lc_sf_action("reset", "ch1_draw_reset", "Wyczyść punkty", variant = "outline"),
          lc_sf_action("reveal", "ch1_draw_reveal", "Pokaż odpowiedź", variant = "solid"),
          lc_sf_action("new", "ch1_draw_new", "Nowe ćwiczenie", variant = "solid"),
          lc_readouts(tags$div(`data-role` = "stats", style = "display: contents"))
        ),
        after = tagList(
          lc_sf_fill_table(lc_table(
            data.frame(c1 = c("wyraz wolny", "X"), c2 = I(list("@@draw_b0@@", "@@draw_b1@@"))),
            cols = list(lc_col("c1", "Zmienna", "row"), lc_col("c2", "Estymata", "num"))
          )),
          tags$div(`data-role` = "feedback")
        )),
      lc_caption("Przeczytaj tabelę współczynników. Potem kliknij na wykresie dwa punkty, które wyznaczają prostą regresji.")
    ),

    lc_p("Najłatwiej liczy się punkty dla okrągłych wartości \\(X\\). Jeśli prosta
      dopasowana ręcznie rozminęła się z poprawną, sprawdź najpierw znak nachylenia, a potem
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

    # Suma kwadratów reszt
    lc_formula_annotated(
      "⟦1|SS_{res}⟧ = \\sum_{i=1}^{n} (⟦2|y_i⟧ - ⟦3|\\hat{y}_i⟧)^2 = \\sum_{i=1}^{n} (y_i - ⟦4|b_0⟧ - ⟦5|b_1⟧ ⟦6|x_i⟧)^2 \\;\\to\\; \\min",
      notes  = list("suma kwadratów reszt", "obserwacja", "wartość przewidywana",
                    "wyraz wolny", "nachylenie", "predyktor"),
      colors = c("bursztyn", "niebo", "wrzos", "terakota", "indygo", "niebo"),
      above  = c(2, 5)
    ),

    lc_p("Odległości mierzy się w pionie, bo model przewiduje \\(Y\\) na
      podstawie \\(X\\) i to w \\(Y\\) popełnia pomyłki. Rozwiązaniem tego
      zadania są wzory z poprzedniej sekcji. Panel pokazuje kolejne etapy
      na losowej próbie 70 punktów: dane, średnią \\(Y\\) jako model bez
      predyktora, prostą MNK, odległości punktów od prostej i porównanie
      z inną prostą."),

    figure_panel(
      label = "Ryc. 1.5",
      full_width = TRUE,
      # Losowanie, kroki i opisy liczy przeglądarka (modules/figures.js, rycina s07_ols).
      lc_sf_figure("s07_ols", bare = TRUE, body =
      lc_step_widget("ch1_ols",
        title = "Jak linia staje się modelem",
        steps = c("Dane", "Średnia Y", "Linia regresji", "Reszty",
                  "Wynik modelu", "Inna prosta?"),
        toolbar = lc_toolbar(
          lc_sf_action("new", "ch1_ols_new", "Nowa próba", icon = "shuffle", variant = "outline"),
          lc_chart_zoom_button()
        ),
        body = lc_sf_chart(0, "2/1", "460px")
      ))
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

    # Reszta
    lc_formula_annotated(
      "⟦1|e_i⟧ = ⟦2|y_i⟧ - ⟦3|\\hat{y}_i⟧",
      notes  = list("reszta", "obserwacja", "wartość przewidywana"),
      colors = c("bursztyn", "niebo", "wrzos")
    ),

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
      pojedyncza ", gloss("wartość odstająca"), " potrafi mocno pociągnąć prostą ku sobie,
      podobnie jak w wykładzie 05 jedna wartość odstająca zmieniała \\(r\\)."),

    lc_p("Drugi powód: suma kwadratów prowadzi do jawnego rozwiązania, czyli do
      wzorów \\(b_1 = r \\cdot s_Y / s_X\\) i \\(b_0 = \\bar{y} - b_1 \\bar{x}\\).
      Minimalizacja sumy wartości bezwzględnych daje regresję medianową.
      To sensowna metoda, mniej wrażliwa na wartości odstające, ale bez
      prostego wzoru."),

    lc_p("Reszty są też głównym narzędziem oceny modelu. Wykład 06 zapowiadał,
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
      budowę co w teście t z wykładu 05: estymata podzielona przez swój błąd
      standardowy. Mówi, ile błędów standardowych dzieli \\(b_1\\) od zera."),

    # Statystyka t dla nachylenia
    lc_formula_annotated(
      "⟦1|t⟧ = \\frac{⟦2|b_1⟧}{⟦3|SE(b_1)⟧}, \\qquad df = ⟦4|n⟧ - 2",
      notes  = list("statystyka testowa", "nachylenie z próby", "błąd standardowy",
                    "liczba obserwacji"),
      colors = c("accent", "indygo", "wrzos", "szalwia"),
      above  = c(2)
    ),

    lc_p("Przy prawdziwej H₀ statystyka ma ", gloss("rozkład t-Studenta"), " o ",
      gloss("stopnie swobody", "stopniach swobody"), " \\(n - 2\\): dwa stopnie
      swobody zużywa oszacowanie \\(b_0\\) i \\(b_1\\). ",
      gloss("p-wartość", "P-wartość"), " liczy się i interpretuje jak w każdym
      teście t. Tabela wyników ma też wiersz dla wyrazu wolnego z własnym
      \\(t\\) i p-wartością. Testuje on hipotezę \\(\\beta_0 = 0\\), czyli pyta
      o przewidywanie przy \\(X = 0\\), co rzadko jest interesujące."),

    lc_p("Zanim przejdziemy do tabeli, zobaczmy, jak bardzo \\(b_1\\) skacze
      między próbami."),

    # PROTOTYP SCENY (2026-10-08): zbierz grupę — b₁ od próby do próby
    figure_panel(
      label = "Prototyp sceny",
      width_mode = "text",
      scene_widget("ch1_sc_slope", "Godziny nauki i wynik: nachylenie z kolejnych grup",
        steps = c("Grupa", "Powtarzamy", "Prawdziwe β₁"),
        labels = c("Zbierz grupę", "Zbierz grupę", "Zbierz grupę"),
        more_from = 2,
        options = list(
          list(name = "n", label = "Studentów w grupie (n)",
               values = c("14" = "14", "70" = "70"), selected = "14"),
          list(name = "world", label = "Świat",
               values = c("nauka pomaga" = "yes", "nie pomaga (β₁ = 0)" = "no"), selected = "yes")
        ),
        config = list(kind = "slope", beta0 = 35, beta1 = 2.5, sigma = 8, xmin = 1, xmax = 12,
                      n = 14L, world = "yes",
                      crit = list(`14` = round(qt(0.975, 12), 4), `70` = round(qt(0.975, 68), 4)),
                      aria = "Wykres rozrzutu godzin nauki i wyniku egzaminu dla losowej grupy studentów z prostą MNK i histogram nachyleń z kolejnych grup"),
        texts = list(
        tagList("Każdy punkt to jeden student, prosta to MNK dla tej grupy. Zbierz kilka grup i
        porównaj ", tags$code("b₁", .noWS = "outside"), "."),
        tagList("Każde ", tags$code("b₁", .noWS = "outside"), " spada do histogramu. Dokładaj grupy,
        potem przełącz n z 14 na 70 i porównaj rozrzut."),
        tagList("Linia przerywana to prawdziwe ", tags$code("β₁", .noWS = "outside"), ". Żetony w kolorze
        akcentu to grupy z p < 0.05. Przełącz świat na „nie pomaga”: około 5% grup i tak pokazuje
        istotne nachylenie.")
        )
      )
    ),

    lc_p("Panel pokazuje cztery symulowane scenariusze o znanym prawdziwym
      nachyleniu."),

    figure_panel(
      label = "Ryc. 1.6", title = "Kiedy nachylenie jest istotne?",
      full_width = TRUE,
      # Opis wykresu, tabelę i wniosek dla każdego scenariusza liczy R przy budowaniu (modules/figures.R).
      ch1_pval_fig()
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
      " dla \\(\\beta_1\\), zbudowany jak w wykładzie 04: estymata plus minus
      ", gloss("wartość krytyczna"), " razy błąd standardowy."),

    # Przedział ufności dla β₁
    lc_formula_annotated(
      "⟦1|b_1⟧ \\pm ⟦2|t^*_{\\alpha/2,\\, n-2}⟧ \\cdot ⟦3|SE(b_1)⟧",
      notes  = list("nachylenie z próby", "wartość krytyczna", "błąd standardowy"),
      colors = c("indygo", "szalwia", "wrzos")
    ),

    lc_p("Panel go nie pokazuje, ale łatwo go policzyć z tabeli. Dla pierwszego
      scenariusza 95% przedział ufności to od 0.70 do 1.35. Dla małej próby
      sięga od -0.07 do 2.35: obejmuje zero, ale też wartości prawie dwa razy
      większe od prawdziwego nachylenia. Dane z małej próby nie wykluczają
      ani braku związku, ani silnego związku."),

    lc_p("Test nachylenia ma jeszcze jedną własność. W regresji prostej jest
      równoważny testowi korelacji z wykładu 05. Statystyka
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
      label = "Ryc. 1.7", title = "CASchools: od tabeli wyników do interpretacji",
      full_width = TRUE,
      # Każda para zmiennych opisana przy budowaniu (modules/figures.R); odsłanianie w przeglądarce.
      ch1_cas_fig(),
      lc_caption("Wybierz zmienne, obejrzyj wykres i tabelę regresji. Najpierw samodzielnie zdecyduj, czy X istotnie przewiduje Y, a potem pokaż odpowiedź.")
    ),

    lc_p("Dla domyślnej pary, wyniku z czytania i dochodu okręgu, tabela podaje
      \\(b_1 = 1.94\\). Dochód jest zapisany w tysiącach dolarów, a wynik
      testu w punktach. Przed dalszą lekturą warto sprawdzić, które zdanie
      poprawnie odczytuje ten współczynnik."),

    figure_panel(
      label = "Ryc. 1.8",
      title = "Co znaczy b₁ = 1.94?",
      p("Model: wynik z czytania (punkty) ~ dochód okręgu (tys. USD),
        \\(b_1 = 1.94\\). Które zdanie jest poprawne?"),
      tags$div(class = "lc-choices", `data-correct` = "per_1000",
        radioButtons("ch1_b1_meaning", NULL,
          choices = c(
            "Okręg zamożniejszy o 1 dolar ma przeciętnie wynik wyższy o 1.94 pkt." = "per_dollar",
            "Okręg zamożniejszy o 1 tys. USD ma przeciętnie wynik wyższy o 1.94 pkt." = "per_1000",
            "Dochód wyższy o 1% wiąże się z wynikiem wyższym o 1.94%." = "percent",
            "Okręg zamożniejszy o 1 tys. USD ma przeciętnie wynik wyższy o 1940 pkt." = "times_1000"
          ),
          selected = character(0)
        )
      ),
      lc_variants("ch1_b1_meaning", selected = ".none", list(
        per_1000 = lc_status(
          lc_verdict(tags$strong("Tak."), type = "ok"),
          " b₁ podaje zmianę Y w jego jednostkach na jednostkę X: punkty testu
          na 1 tys. USD dochodu, bo w tysiącach dolarów zapisano dochód."
        ),
        per_dollar = lc_status(lc_verdict(tags$strong("Nie."), type = "danger"),
          " Dochód jest zapisany w tysiącach dolarów, więc jednostką X
          jest 1 tys. USD, a nie 1 dolar. Na 1 dolar przypadałoby 0.002 pkt."),
        percent = lc_status(lc_verdict(tags$strong("Nie."), type = "danger"),
          " Obie zmienne są w swoich jednostkach (tys. USD i punkty), a nie
          w procentach. Nachylenie mówi o punktach na 1 tys. USD."),
        times_1000 = lc_status(lc_verdict(tags$strong("Nie."), type = "danger"),
          " Tysiące dotyczą tylko dochodu (X). Wynik testu jest
          w punktach, więc b₁ = 1.94 to 1.94 pkt na 1 tys. USD, a nie 1940 pkt.")
      ))
    ),

    lc_p("Poprawne jest drugie zdanie: okręg zamożniejszy o 1 tys. USD ma
      przeciętnie wynik z czytania wyższy o niecałe 2 punkty.
      \\(SE = 0.10\\), \\(t = 19.9\\), a p-wartość jest mniejsza niż 0.001. Korelacja tych zmiennych wynosi
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

    # Wartość przewidywana
    lc_formula_annotated(
      "⟦1|\\hat{Y}⟧ = ⟦2|b_0⟧ + ⟦3|b_1⟧ ⟦4|X⟧",
      notes  = list("wartość przewidywana", "wyraz wolny", "nachylenie", "predyktor"),
      colors = c("wrzos", "terakota", "indygo", "niebo"),
      above  = c(2, 4)
    ),

    lc_p("Wartość przewidywana szacuje średnie \\(Y\\) wśród obiektów o danym
      \\(X\\), a nie wynik pojedynczego obiektu. Model czytania od dochodu
      przewiduje dla okręgu o dochodzie 20 tys. USD wynik 664.1. Nie znaczy
      to, że każdy taki okręg osiągnie 664 punkty, tylko że okręgi o tym
      dochodzie osiągają przeciętnie około 664, a pojedyncze rozrzucają się
      wokół tej wartości o tyle, ile wynoszą reszty."),

    lc_p("Panel podaje tabelę współczynników dla kilku modeli. Policz
      \\(\\hat{Y}\\) samodzielnie przed odsłonięciem odpowiedzi."),

    figure_panel(
      label = "Ryc. 1.9", title = "Użyj równania regresji do przewidywania",
      full_width = TRUE,
      # Predykcję dla wpisanego X liczy przeglądarka (modules/figures.js, rycina s07_pred).
      ch1_pred_fig(),
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
