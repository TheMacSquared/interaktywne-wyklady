# ============================================================================
# CHAPTER 3: Założenia testu χ², testu Fishera i korelacji
# ============================================================================

ch3_ui <- lecture_chapter(
  id = "ch-chi-fisher",
  num = "03",
  title = "Założenia χ², Fishera i korelacji",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 03 · Założenia testów",
      num    = "03",
      title  = "Założenia χ², Fishera i korelacji.",
      lead   = "Test χ² nie wymaga normalności ani równych wariancji, ale opiera się
                na przybliżeniu, które przy małych liczebnościach może zawieść.
                Test Fishera tego przybliżenia nie potrzebuje, za to płaci ostrożnością.
                Na koniec wracamy do korelacji, której założenia łatwiej sprawdzić
                na wykresie niż testem."
    ),

    lc_p("Dwa poprzednie rozdziały dotyczyły testów porównujących średnie. Ich
      założenia mówiły o kształcie rozkładu i o rozrzucie w grupach. Testy dla
      dwóch zmiennych jakościowych pracują na innych danych: na liczebnościach
      w tabeli. Nie ma tu średnich ani wariancji grup, więc normalność i równość
      wariancji tracą sens. Zostają jednak dwa warunki: niezależne obserwacje
      i dostatecznie duże liczebności, żeby przybliżenie rozkładem χ² było
      wiarygodne."),

    lc_h2("ch3-zalozenia-chi", "Założenia testu χ²"),

    lc_p(gloss("test chi-kwadrat", "Test χ²"), " niezależności poznaliśmy
      w rozdziale 07 wykładu 04. Porównuje on liczebności obserwowane w ",
      gloss("tabela kontyngencji", "tabeli kontyngencji"), " z liczebnościami,
      jakich spodziewalibyśmy się przy niezależności zmiennych. Wynik jest
      wiarygodny, gdy spełnione są trzy warunki."),

    lc_p("Pierwszy to ", gloss("niezależność obserwacji"), ". Każda osoba albo
      każdy obiekt trafia do tabeli dokładnie raz, do jednej komórki, a wynik
      jednej obserwacji nie wpływa na wynik innej. Założenie łamie na przykład
      tabela, w której ta sama osoba występuje dwa razy, przed szkoleniem i po
      nim. Z tego samego powodu w komórkach muszą stać liczebności, a nie
      procenty ani średnie."),

    lc_p("Drugi warunek jest wspólny dla całego wnioskowania: dane powinny
      pochodzić z próby losowej. Jeśli obserwacje zebrano wybiórczo, test
      odpowiada na pytanie o próbę, a nie o populację."),

    lc_p("Trzeci warunek dotyczy wielkości próby. Statystyka χ² ma rozkład χ²
      tylko w przybliżeniu, a przybliżenie jest tym lepsze, im większe są ",
      gloss("liczebność oczekiwana", "liczebności oczekiwane"), ". Przypomnijmy,
      że liczebność oczekiwana komórki to suma jej wiersza pomnożona przez sumę
      jej kolumny i podzielona przez liczbę wszystkich obserwacji:"),

    lc_formula_box(withMathJax(
      "$$E_{ij} = \\frac{n_{i\\cdot} \\cdot n_{\\cdot j}}{n}$$"
    )),

    lc_p("Warunek dotyczy liczebności oczekiwanych, a nie obserwowanych. Zero
      w komórce obserwowanej nie przeszkadza, jeśli przy niezależności
      spodziewalibyśmy się tam wielu obserwacji. W wykładzie 04 podaliśmy
      orientacyjną regułę: co najmniej 5 obserwacji oczekiwanych w każdej
      komórce. Programy statystyczne zwykle same ostrzegają, gdy którakolwiek
      liczebność oczekiwana jest mniejsza od 5, i potrafią pokazać tabelę
      liczebności oczekiwanych. W tabeli 2 × 2
      z 20 obserwacjami i równymi sumami wierszy i kolumn każda liczebność
      oczekiwana wynosi dokładnie 5, więc taka próba leży na granicy reguły."),

    # ========================================================================
    # WIDGET 1: Wizualizacja efektu małych liczebności
    # ========================================================================
    lc_h2("ch3-male-licznosci", "Efekt małych liczebności"),

    lc_p("Reguła mówi, kiedy uważać, ale nie mówi, jak bardzo test się myli,
      gdy liczebności są małe. Można to sprawdzić symulacją. Jeśli zmienne
      są naprawdę niezależne, test na poziomie istotności α = 0.05 powinien
      odrzucać H₀ w 5% prób. Każde odrzucenie jest wtedy fałszywym alarmem,
      czyli ", gloss("błąd pierwszego rodzaju", "błędem I rodzaju"), ". Test,
      którego przybliżenie zawodzi, popełnia go częściej albo rzadziej niż
      w 5% prób."),

    lc_p("Panel losuje 500 tabel 2 × 2 z populacji, w której związku nie ma,
      a obie zmienne mają po dwie równie częste kategorie. Na każdej tabeli
      wykonuje test χ² według wzoru z wykładu 04, bez poprawki Yatesa,
      i test Fishera, a potem liczy, jak często każdy z nich odrzucił H₀.
      Histogram pokazuje rozkład p-wartości z obu testów."),

    figure_panel(
      label = "Ryc. 3.1",
      title = "Symulacja: χ² a Fisher przy małych n",
      lc_toolbar(
        lc_slider("ch3_n", "Wielkość próby", 10, 200, 20, 5),
        lc_segmented("ch3_cat", "Kategorie", c("Równe (50/50)" = "equal",
                                               "Rzadkie (10/90)" = "rare")),
        lc_action("ch3_sim", "Symuluj", variant = "solid"),
        lc_readouts(uiOutput("ch3_sim_results"))
      ),
      lc_plot("ch3_sim_plot", max_height = "250px"),
      lc_caption("500 prób z prawdziwą H₀ (brak związku), α = 0.05. Tabele, w których
        χ² nie da się policzyć (pusty wiersz lub kolumna), są pomijane.")
    ),

    lc_p("Przy domyślnym n = 20 prawie każda wylosowana tabela (97%) ma
      przynajmniej jedną liczebność oczekiwaną mniejszą od 5. Mimo to test χ²
      trzyma poziom α: dokładny rachunek daje 5.1% fałszywych alarmów. Test
      Fishera odrzuca prawdziwą H₀ tylko w 2.1% prób. Pojedyncza symulacja
      z 500 prób odchyla się od tych wartości typowo o jeden punkt procentowy,
      dlatego odczyt Fishera zwykle jest bursztynowy: test nie myli się zbyt
      często, tylko zbyt rzadko. Czerwony kolor oznaczałby więcej fałszywych
      alarmów, niż zakłada α."),

    lc_p("Widać to też na histogramie. Gdy H₀ jest prawdziwa, p-wartości
      powinny rozkładać się mniej więcej równomiernie między 0 a 1. P-wartości
      testu Fishera gromadzą się przy prawym końcu: przy n = 20 około 40%
      z nich trafia do ostatniego przedziału, od 0.95 do 1. Test jest ", em_("konserwatywny"), ": jego
      rzeczywisty poziom istotności jest niższy od deklarowanego. Bierze się
      to stąd, że z małej tabeli da się uzyskać niewiele różnych p-wartości.
      Ceną jest mniejsza ", gloss("moc testu"), ": test, który rzadko odrzuca
      prawdziwą H₀, rzadziej odrzuca też fałszywą. Wraz z próbą różnica
      maleje. Przy n = 100 test χ² daje 5.4% fałszywych alarmów, a Fisher 4.3%."),

    lc_p("Przy ustawieniu „Równe” obie zmienne mają równie częste kategorie, co
      jest dla testu χ² sytuacją najłatwiejszą. Kłopoty zaczynają się przy rzadkich
      kategoriach i wtedy przybliżenie potrafi mylić się w obie strony. Przy
      n = 20, gdy jedna zmienna ma kategorie po 50%, a odpowiedź „Tak” daje
      tylko 10% badanych, test χ² odrzuca prawdziwą H₀ w 2.6% prób. Gdy
      dodatkowo pierwsza zmienna ma kategorie w proporcji 20% do 80%,
      fałszywych alarmów jest już 6.7%, więcej niż zakłada α. Reguła ≥ 5 nie
      wyznacza więc granicy, za którą test przestaje działać. Jest sygnałem,
      że wynik zależy od przybliżenia i warto go potwierdzić metodą, która
      przybliżenia nie potrzebuje. Ustawienie „Rzadkie” daje w obu zmiennych
      jedną kategorię o częstości 10%: przy n = 20 test χ² odrzuca prawdziwą
      H₀ w około 8% prób, a Fisher w około 1%. Od n = 30 χ² wraca w okolice 5%,
      a Fisher pozostaje konserwatywny."),

    # ========================================================================
    # Kiedy który?
    # ========================================================================
    lc_h2("ch3-kiedy", "Kiedy χ², kiedy Fisher?"),

    lc_p("Taką metodą jest ", gloss("test dokładny Fishera"), ". Jak pokazał
      wykład 04, rozważa on wszystkie tabele o tych samych sumach wierszy
      i kolumn co tabela obserwowana i dla każdej liczy dokładne
      prawdopodobieństwo przy H₀. Nie korzysta z rozkładu χ², więc nie ma
      warunku na liczebności oczekiwane. Wymaga niezależności obserwacji,
      tak jak test χ². Sumy brzegowe traktuje jako ustalone, co wraz
      z dyskretnością tabel daje opisaną wyżej ostrożność: rzeczywisty odsetek
      fałszywych alarmów nie przekracza α, ale bywa wyraźnie niższy."),

    lc_p("Wybór wynika z tego, ile kosztuje każdy błąd. Gdy liczebności
      oczekiwane są wyraźnie duże, oba testy dają praktycznie ten sam wynik
      i można zostać przy χ². Gdy część z nich jest mała, bezpieczniej oprzeć
      decyzję na teście Fishera: lepiej stracić trochę mocy, niż ryzykować
      zawyżony błąd I rodzaju. Oba testy liczy się na tej samej tabeli."),

    lc_p("Dwie uwagi praktyczne. Dla tabel 2 × 2 część programów domyślnie
      stosuje w teście χ² poprawkę Yatesa, która zmniejsza statystykę. W warunkach z panelu przy n = 20 test z poprawką
      odrzuca prawdziwą H₀ tylko w 1.3% prób, czyli jest jeszcze ostrożniejszy
      niż Fisher. Przy małych tabelach 2 × 2 prościej więc od razu użyć testu
      Fishera. Dla większych tabel test Fishera też działa, ale przy wielu
      komórkach i dużym n liczy się długo. Wtedy można wyznaczyć p-wartość
      symulacyjnie: program losuje tysiące tabel o tych samych sumach brzegowych i sprawdza, jak często dają χ²
      co najmniej tak duże jak obserwowane. Czasem sensowniejsze jest
      połączenie rzadkich kategorii, jeśli ma to uzasadnienie merytoryczne,
      na przykład zebranie kilku rzadkich odpowiedzi w kategorię „inne”."),

    lc_note("Zasada", rule = TRUE,
      "Sprawdzaj liczebności oczekiwane, nie obserwowane. Gdy część z nich
       jest mała, oprzyj decyzję na teście Fishera."
    ),

    # ========================================================================
    # Założenia korelacji
    # ========================================================================
    lc_h2("ch3-korelacja", "Założenia korelacji"),

    lc_p("Test χ² mierzy związek dwóch zmiennych jakościowych. Dla dwóch
      zmiennych ilościowych jego odpowiednikiem jest test ",
      gloss("korelacja Pearsona", "korelacji Pearsona"), " z rozdziału 06
      wykładu 04. Tam wymieniliśmy jego założenia: pary obserwacji są od siebie
      niezależne, związek jest liniowy, nie ma silnych ",
      gloss("wartość odstająca", "wartości odstających"), ", a obie zmienne
      mają rozkład zbliżony do normalnego. Ściślej, test i przedział ufności
      dla \\(\\rho\\) zakładają, że para zmiennych ma łącznie dwuwymiarowy
      rozkład normalny. W praktyce oznacza to eliptyczną chmurę punktów bez
      wyraźnie skośnych zmiennych."),

    lc_p("Te założenia nie są równie ważne. Normalność dotyczy wnioskowania,
      czyli p-wartości i przedziału ufności, a nie samego współczynnika \\(r\\),
      który zawsze opisuje siłę związku liniowego w próbie. Jak w teście t,
      im większa próba, tym mniej szkodzą umiarkowane odstępstwa od
      normalności. Nieliniowości ani wartości odstających większa próba nie
      naprawia. Dlatego głównym narzędziem kontroli jest wykres rozrzutu,
      a nie test formalny. Kwartet Anscombe’a z wykładu 04 pokazał cztery
      zbiory o tym samym \\(r = 0.82\\) i zupełnie różnych kształtach."),

    lc_p("Gdy założenia Pearsona zawodzą, stosuje się ",
      gloss("korelacja Spearmana", "korelację Spearmana"), ". To ten sam
      współczynnik, policzony nie na wartościach, lecz na ich ",
      gloss("ranga", "rangach"), ", czyli pozycjach w danych uporządkowanych
      rosnąco. Nie wymaga normalności ani liniowości, tylko związku
      monotonicznego: gdy jedna zmienna rośnie, druga rośnie (albo maleje),
      choćby nierównomiernie. Pojedyncza wartość odstająca dostaje po prostu
      najwyższą rangę, więc nie może ciągnąć współczynnika dowolnie daleko.
      W panelu z wykładu 04 jeden dopisany punkt podnosił \\(r\\) Pearsona
      z okolic zera typowo do około 0.52, a korelacja Spearmana na tych samych
      danych zostaje typowo przy 0.06. W zbiorze 3 Anscombe’a, gdzie jeden
      punkt odstaje od idealnej prostej, korelacja Spearmana wynosi 0.99."),

    lc_p("Spearman nie rozwiązuje jednak każdego problemu. W zbiorze 2,
      w którym punkty leżą na łuku, związek nie jest monotoniczny i korelacja
      Spearmana (0.69) opisuje go gorzej niż Pearson. W zbiorze 4, gdzie
      dziesięć punktów ma to samo \\(x\\), wynosi 0.50. Przy wielu powtarzających
      się wartościach i małych próbach stosuje się też ",
      gloss("tau Kendalla"), ", inną korelację rangową. Tabela zbiera
      założenia i alternatywy."),

    lc_table(
      data.frame(
        c1 = c("Pearson", "Spearman"),
        c2 = c(
          "Niezależne pary, związek liniowy, brak silnych wartości
           odstających; dla testu rozkład zbliżony do normalnego",
          "Niezależne pary, związek monotoniczny (słabszy warunek
           niż liniowość)"
        ),
        c3 = c(
          "Spearman (rangi)",
          "Tau Kendalla (małe próby, wiele powtarzających się wartości)"
        )
      ),
      cols = list(
        lc_col("c1", "Test", "row"),
        lc_col("c2", "Założenia", "text"),
        lc_col("c3", "Alternatywa", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_p("Trzy rozdziały przeszły przez założenia najczęściej używanych testów:
      normalność, równość wariancji, liczebności oczekiwane i kształt związku.
      Następny rozdział zbiera je w jednym miejscu: dla każdej metody pokazuje,
      co trzeba sprawdzić i po co sięgnąć, gdy założenie nie jest spełnione."),

    lc_chapter_next(
      num = "04",
      title = "Mapa metod",
      lead = "szybkie przejście od metody do założeń i alternatyw.",
      target_id = "ch-mapa"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  ch3_sim_data <- reactiveVal(NULL)

  observeEvent(input$ch3_sim, {
    n <- input$ch3_n
    n_sims <- 500
    # „Rzadkie”: w obu zmiennych jedna kategoria ma 10% częstości.
    p_cat <- if (identical(input$ch3_cat, "rare")) c(0.1, 0.9) else c(0.5, 0.5)

    results <- sapply(1:n_sims, function(i) {
      # H0 prawdziwa: brak związku
      x <- factor(sample(c("A", "B"), n, replace = TRUE, prob = p_cat), levels = c("A", "B"))
      y <- factor(sample(c("Tak", "Nie"), n, replace = TRUE, prob = p_cat), levels = c("Tak", "Nie"))
      tab <- table(x, y)

      p_chi <- tryCatch(suppressWarnings(chisq.test(tab, correct = FALSE)$p.value),
                        error = function(e) NA)
      p_fisher <- fisher.test(tab)$p.value

      c(p_chi = p_chi, p_fisher = p_fisher)
    })

    results_df <- data.frame(t(results))
    ch3_sim_data(results_df)
  })

  output$ch3_sim_results <- renderUI({
    df <- ch3_sim_data()
    if (is.null(df)) return(NULL)

    fpr_chi <- mean(df$p_chi < 0.05, na.rm = TRUE) * 100
    fpr_fisher <- mean(df$p_fisher < 0.05, na.rm = TRUE) * 100

    # Blisko α: zielony; za dużo fałszywych alarmów: czerwony; za mało
    # (test konserwatywny): bursztynowy.
    fpr_color <- function(fpr) {
      if (abs(fpr - 5) <= 2) col_ok
      else if (fpr > 5) col_fail
      else unname(upwr_cat["bursztyn"])
    }
    chi_color <- fpr_color(fpr_chi)
    fisher_color <- fpr_color(fpr_fisher)

    tagList(
      lc_readout("Fałszywe alarmy χ²", paste0(round(fpr_chi, 1), "%"), color = chi_color),
      lc_readout("Fałszywe alarmy Fisher", paste0(round(fpr_fisher, 1), "%"), color = fisher_color),
      lc_readout("Poziom α", "5%", color = upwr_secondary)
    )
  })

  zoom_plot_server("ch3_sim_plot", reactive({
    df <- ch3_sim_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Symuluj”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      long <- data.frame(
        test = rep(c("χ²", "Fisher"), each = nrow(df)),
        p = c(df$p_chi, df$p_fisher)
      )
      long <- long[!is.na(long$p), ]

      ggplot(long, aes(x = p, fill = test)) +
        geom_histogram(breaks = seq(0, 1, by = 0.05), alpha = 0.6,
                       color = "white", position = "identity") +
        geom_vline(xintercept = 0.05, color = col_fail, linetype = "dashed") +
        scale_fill_manual(values = c(col_test, col_alt), name = NULL) +
        labs(
             x = "p-wartość", y = "Liczba") +
        theme_upwr() +
        theme(legend.position = "top")
    }
  }))
}
