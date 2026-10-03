# ============================================================================
# CHAPTER 7: ANOVA
# ============================================================================

ch7_ui <- list(
  id = "ch-anova", num = "09", title = "ANOVA",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 09 · Testowanie hipotez",
      num    = "09",
      title  = "ANOVA.",
      lead   = "Trzy grupy to trzy pary do porównania, a każde porównanie to kolejna
                okazja do fałszywego alarmu. Analiza wariancji sprawdza jednym testem,
                czy średnie w kilku grupach się różnią, a testy post hoc wskazują,
                które pary za to odpowiadają."
    ),

    lc_p("W poprzednim rozdziale porównywaliśmy średnie w dwóch grupach testem t.
      Wiele pytań z praktyki dotyczy jednak większej liczby grup: pH jogurtu
      fermentowanego w trzech temperaturach, średniej ocen na czterech kierunkach
      studiów, stresu na trzech typach stanowisk pracy. W tym rozdziale zobaczymy,
      dlaczego do takich pytań nie wystarczy powtórzyć testu t dla każdej pary
      grup, i poznamy test, który zastępuje całą serię porównań jednym."),

    # ========================================================================
    # SEKCJA 1: Motywacja — dlaczego nie kilka testów t?
    # ========================================================================
    lc_h2("ch7-motywacja", "Dlaczego nie kilka testów t?"),

    lc_p("Najprostszy pomysł to wykonać ", gloss("test t", "test t"), " dla każdej
      pary grup. Przy trzech grupach A, B i C są to trzy porównania: A z B, A z C
      oraz B z C. Kłopot leży w tym, co oznacza poziom istotności. Każdy test na
      poziomie ", withMathJax("\\(\\alpha = 0.05\\)"), " ma 5% szans na ",
      gloss("błąd pierwszego rodzaju"), ", czyli fałszywy alarm: odrzucenie H₀,
      choć w populacji różnicy nie ma. Te 5% dotyczy jednego testu. Gdy testów
      jest kilka, szansa, że co najmniej jeden z nich da fałszywy alarm, rośnie
      z każdym kolejnym porównaniem. To zjawisko nazywamy problemem ",
      gloss("porównania wielokrotne", "porównań wielokrotnych"), "."),

    lc_p("Przy k grupach liczba par wynosi m = k(k - 1)/2. Jeśli we wszystkich
      grupach średnia w populacji jest taka sama, a testy potraktujemy w uproszczeniu
      jako niezależne, prawdopodobieństwo co najmniej jednego fałszywego alarmu
      w całej serii wynosi:"),

    lc_formula_box(withMathJax(
      "$$m = \\frac{k(k-1)}{2}, \\qquad P(\\text{co najmniej jeden fałszywy alarm}) = 1 - (1 - \\alpha)^m$$"
    )),

    lc_p("Panel rysuje grupy jako wierzchołki, a każdą parę do porównania jako
      odcinek. Obok podaje liczbę testów i ryzyko obliczone z tego wzoru."),

    figure_panel(
      label = "Ryc. 9.1",
      title = "Dodaj grupy i obserwuj inflację błędu I rodzaju",
      fluidRow(
        column(4,
          p(tags$em("Zacznij od 2 grup. Każde kliknięcie dodaje jedną.")),
          lc_action("ch7_motyw_add", "Dodaj grupę +", variant = "solid"),
          br(), br(),
          lc_action("ch7_motyw_reset", icon = "reset", variant = "ghost", aria_label = "Reset"),
          br(), br(),
          uiOutput("ch7_motyw_stats")
        ),
        column(8,
          zoom_plot_ui("ch7_motyw_plot", height = "340px")
        )
      )
    ),

    lc_p("Dla dwóch grup jest jeden test i ryzyko wynosi dokładnie 5%. Przy trzech
      grupach mamy 3 pary i ryzyko 14.3%, przy czterech — 6 par i 26.5%, przy
      pięciu — 10 par i 40.1%. Przy ośmiu grupach, największej liczbie w panelu,
      28 porównań daje 76.2%: w trzech badaniach na cztery co najmniej jedna para
      wyjdzie istotna, choć wszystkie grupy pochodzą z populacji o tej samej
      średniej. Liczba par rośnie mniej więcej z kwadratem liczby grup, więc
      ryzyko szybko wymyka się spod kontroli."),

    lc_p("Potrzebujemy zatem jednego testu, który odpowie na pytanie ogólne, czy
      jakakolwiek średnia odstaje od pozostałych, przy ryzyku fałszywego alarmu
      równym α. Takim testem jest ", gloss("ANOVA"), "."),

    # ========================================================================
    # SEKCJA 2: Wprowadzenie ANOVA
    # ========================================================================
    lc_h2("ch7-intro", "ANOVA jednoczynnikowa"),

    lc_p("ANOVA (ang. analysis of variance, analiza wariancji) porównuje średnie
      w k grupach wyznaczonych przez jedną zmienną jakościową, stąd nazwa
      jednoczynnikowa. Zmienna zależna jest ilościowa, a czynnikiem jest zmienna
      grupująca. Przykład z panelu niżej: pH jogurtu (zmienna zależna) po
      fermentacji w trzech temperaturach, 20, 25 i 30 °C (czynnik o trzech
      poziomach). Hipotezy mają postać:"),

    lc_formula_box(
      p(withMathJax("\\(H_0: \\mu_1 = \\mu_2 = \\ldots = \\mu_k\\) — wszystkie średnie są równe")),
      p(withMathJax("\\(H_a:\\) co najmniej jedna średnia różni się od pozostałych"))
    ),

    lc_p("Hipoteza alternatywna nie twierdzi, że wszystkie średnie są różne.
      Wystarczy, że jedna grupa odstaje od reszty."),

    lc_p("Nazwa „analiza wariancji” bierze się ze sposobu działania testu.
      W wykładzie 01, przy porównaniu wzrostu kobiet i mężczyzn, odchylenie
      standardowe wynosiło 6.0 cm w grupie kobiet i 6.5 cm w grupie mężczyzn,
      a w całej próbie 8.1 cm. Część rozrzutu całej próby brała się z różnicy
      między grupami, a nie ze zmienności wewnątrz nich. ANOVA zamienia tę
      obserwację w test. Całkowitą zmienność danych rozkłada na zmienność ",
      tags$em("między"), " grupami (jak daleko średnie grup leżą od średniej
      ogólnej) i zmienność ", tags$em("wewnątrz"), " grup (jak daleko obserwacje
      leżą od średniej własnej grupy). ", gloss("statystyka F", "Statystyka F"),
      " to stosunek tych dwóch części:"),

    lc_formula_box(withMathJax(
      "$$F = \\frac{\\text{zmienność między grupami}}{\\text{zmienność wewnątrz grup}} = \\frac{\\sum_{j=1}^{k} n_j\\,(\\bar{x}_j - \\bar{x})^2 \\,/\\, (k-1)}{\\sum_{j=1}^{k} \\sum_{i=1}^{n_j} (x_{ij} - \\bar{x}_j)^2 \\,/\\, (n-k)}$$"
    )),

    lc_p("Tu \\(\\bar{x}_j\\) i \\(n_j\\) to średnia i liczebność j-tej grupy,
      \\(\\bar{x}\\) — średnia wszystkich n obserwacji. Licznik i mianownik to ",
      gloss("wariancja", "wariancje"), ": sumy kwadratów odchyleń podzielone przez
      liczby stopni swobody, k - 1 i n − k."),

    lc_p("Gdy H₀ jest prawdziwa, średnie grup różnią się tylko przypadkowo. Licznik
      i mianownik mierzą wtedy ten sam losowy szum, więc F wychodzi w okolicach 1.
      Gdy średnie w populacji się różnią, licznik rośnie i F staje się duże. Przy
      prawdziwej H₀ statystyka F ma rozkład F z k - 1 i n − k stopniami swobody.
      P-wartość to prawdopodobieństwo, że przy prawdziwej H₀ wypadnie wartość F
      co najmniej tak duża jak obserwowana. Liczy się tylko prawy ogon, bo małe F
      oznacza średnie bliższe sobie, niż wynikałoby z szumu, a to nie przemawia
      przeciw H₀. Decyzja jest taka jak w poprzednich rozdziałach: odrzucamy H₀,
      gdy p < α, a poziom α ustalamy przed analizą."),

    lc_p("Odrzucenie H₀ mówi, że co najmniej jedna średnia odstaje, ale nie mówi
      która. Do tego służą ", gloss("test post hoc", "testy post hoc"), " omówione
      niżej. Dla dwóch grup ANOVA daje tę samą p-wartość co test t z założeniem
      równych wariancji (wtedy F = t²), więc jest uogólnieniem testu t
      z poprzedniego rozdziału na dowolną liczbę grup."),

    lc_p("Klasyczna ANOVA zakłada niezależne obserwacje, rozkład zmiennej w każdej
      grupie zbliżony do normalnego i podobne wariancje w grupach. Sprawdzaniem
      tych założeń zajmiemy się w wykładzie 05."),

    # ========================================================================
    # Ćwiczenie: sformułuj hipotezy
    # ========================================================================
    lc_h2("ch7-cwiczenie", "Ćwiczenie: sformułuj hipotezy"),

    lc_p("Niezależnie od liczby grup ANOVA ma jedną parę hipotez. Zapisz H₀ i Hₐ
      dla poniższych sytuacji, zanim odsłonisz odpowiedź."),

    hypothesis_practice("ch7", list(
      list(
        question = "Czy średnia cena posiłku różni się między Spiżem,
                    budką z knyszą a Pasibusem?",
        h0 = "\\(H_0: \\mu_1 = \\mu_2 = \\mu_3\\) (Spiż, knysza, Pasibus mają tę samą średnią cenę)",
        ha = "\\(H_a:\\) co najmniej jedna średnia różni się od pozostałych",
        note = "ANOVA nie ma wersji jednostronnej: wykrywa odstępstwo dowolnej grupy w dowolnym kierunku. Nie mówi, która grupa odstaje — do tego służy test post hoc."
      ),
      list(
        question = "Technolog testuje cztery metody pasteryzacji mleka. Zmienna:
                    liczba kolonii bakterii po 7 dniach.",
        h0 = "\\(H_0: \\mu_1 = \\mu_2 = \\mu_3 = \\mu_4\\)",
        ha = "\\(H_a:\\) co najmniej jedna średnia jest różna",
        note = "Cztery grupy to 6 par. Seria 6 testów t podniosłaby ryzyko co najmniej jednego fałszywego alarmu do około 26%."
      ),
      list(
        question = "Porównujemy średni czas dojazdu do pracy w trzech miastach:
                    Warszawa, Kraków, Wrocław.",
        h0 = "\\(H_0: \\mu_1 = \\mu_2 = \\mu_3\\) (Warszawa, Kraków, Wrocław — ten sam średni czas)",
        ha = "\\(H_a:\\) co najmniej jedna średnia jest różna",
        note = "Jeśli ANOVA odrzuci H₀, test post hoc (Games-Howell) wskaże, które miasta się różnią."
      ),
      list(
        question = "Ergonomista mierzy czas reakcji (ms) operatorów maszyn na trzy
                    zmiany robocze: ranną, popołudniową i nocną.",
        h0 = "\\(H_0: \\mu_R = \\mu_P = \\mu_N\\) — średni czas reakcji jest taki sam na każdej zmianie",
        ha = "\\(H_a:\\) co najmniej jedna zmiana różni się średnim czasem reakcji",
        note = "Czas reakcji przekłada się na ryzyko wypadku. Jeśli ANOVA jest istotna, test post hoc wskaże, która zmiana odstaje."
      )
    )),

    lc_p("We wszystkich przykładach hipotezy mają ten sam kształt, a zmienia się
      tylko liczba średnich w H₀. Pytanie, które grupy się różnią, nie należy do
      ANOVA; przechodzi do testów post hoc."),

    # ========================================================================
    # WIDGET 1: ANOVA jednoczynnikowa
    # ========================================================================
    lc_h2("ch7-akcja", "ANOVA w akcji"),

    lc_p("Panel losuje dane z jednego z trzech scenariuszy, rysuje wykresy pudełkowe
      w grupach i liczy ANOVA. Suwak n ustala liczebność całej próby; każda
      obserwacja trafia do grupy losowo, więc liczebności grup są zbliżone, ale
      nie równe. Panel liczy klasyczną ANOVA."),

    figure_panel(
      label = "Ryc. 9.2",
      title = "ANOVA jednoczynnikowa",
      fluidRow(
        column(4,
          selectInput("ch7_scenario", "Scenariusz:",
            choices = c(
              "Fermentacja jogurtu (TŻ)" = "fermentation",
              "Kierunki studiów" = "students",
              "Stanowisko pracy a wypadki/stres (IB)" = "workplace"
            ),
            selected = "fermentation"
          ),
          uiOutput("ch7_var_ui"),
          lc_slider("ch7_n", "n (ogółem)", 80, 300, 160, 20),
          lc_action("ch7_run_anova", "Generuj i testuj", variant = "solid")
        ),
        column(8,
          zoom_plot_ui("ch7_boxplot", height = "350px"),
          uiOutput("ch7_anova_result")
        )
      )
    ),

    lc_p("W domyślnym scenariuszu dane pochodzą z populacji, w których średnie pH
      wynoszą 4.55 przy 20 °C, 4.30 przy 25 °C i 4.05 przy 30 °C, a odchylenie
      standardowe wewnątrz każdej grupy to 0.14. Różnica między sąsiednimi
      temperaturami jest prawie dwa razy większa niż rozrzut wewnątrz grupy,
      więc pudełka praktycznie się nie nakładają. Przy n = 160 statystyka ma
      rozkład F(2, 157), którego wartość krytyczna dla α = 0.05 wynosi około 3.05.
      W tym scenariuszu F wychodzi zwykle ponad sto, a H₀ jest odrzucana
      w praktycznie każdym losowaniu."),

    lc_p("Scenariusz kierunków studiów pokazuje drugą stronę testu. Średnia ocen
      ma w populacji różne średnie na kierunkach (od 3.4 do 3.8), więc test zwykle
      tę różnicę wykrywa. Wzrost i czas dojazdu generator losuje niezależnie od
      kierunku: dla tych zmiennych H₀ jest prawdziwa. Test odrzuci ją wtedy średnio
      w co dwudziestym losowaniu. To są fałszywe alarmy, na które godzimy się,
      wybierając α = 0.05. Brak podstaw do odrzucenia H₀ w pozostałych losowaniach
      nie dowodzi jednak, że średnie są równe, tylko że dane nie przemawiają
      przeciw tej hipotezie."),

    lc_p("Klasyczna ANOVA zakłada podobne wariancje w grupach. Gdy wariancje wyraźnie
      się różnią, stosuje się wariant Welcha, który tego założenia nie potrzebuje. Daje on
      inną wartość F i niecałkowitą drugą liczbę stopni swobody niż klasyczna
      ANOVA z panelu powyżej."),

    # ========================================================================
    # WIDGET 2: Post-hoc Games-Howell
    # ========================================================================
    lc_h2("ch7-posthoc", "Testy post hoc (Games-Howell)"),

    lc_p("Istotna ANOVA mówi, że grupy się różnią, ale nie mówi które. Wracamy więc
      do porównań parami, tym razem z korektą na porównania wielokrotne: ryzyko
      co najmniej jednego fałszywego alarmu jest kontrolowane na poziomie α dla
      całej rodziny porównań, a nie dla pojedynczego testu. Dlatego p-wartości
      w tabeli post hoc są skorygowane (p.adj) i zwykle większe niż p-wartości
      zwykłych testów t dla tych samych par."),

    lc_p("Panel używa ", gloss("test Games-Howella", "testu Games-Howella"), ".
      Każdą parę porównuje statystyką podobną do testu t, ale nie zakłada
      równych wariancji ani równych liczebności grup, a wartości krytyczne bierze z rozkładu, który uwzględnia
      liczbę porównywanych grup. To bezpieczny wybór domyślny: gdy wariancje są
      podobne, daje wyniki zbliżone do popularnego testu Tukeya, a gdy się różnią,
      nie traci kontroli nad błędem I rodzaju. Panel łączy klasyczną ANOVA,
      która zakłada równe wariancje, z testem post hoc, który tego nie zakłada.
      Przy wyraźnie różnych wariancjach spójniejsza jest para ANOVA Welcha
      i Games-Howell."),

    inline_callout(
      label = "Zasada",
      "Testy post hoc wykonuj po istotnej ANOVA. Gdy ANOVA nie odrzuca H₀,
       porównań parami nie interpretujemy."
    ),

    lc_p("Panel korzysta z danych wylosowanych w Ryc. 9.2. Macierz p-wartości ma
      układ typowej tabeli post hoc, a wykres pokazuje różnicę średnich dla każdej pary
      z 95-procentowym przedziałem ufności."),

    figure_panel(
      label = "Ryc. 9.3",
      title = "Games-Howell",
      helpText("Panel używa danych z Ryc. 9.2. Najpierw uruchom tam ANOVA."),
      lc_action("ch7_run_tukey", "Testuj Games-Howellem", variant = "solid"),
      br(), br(),

      h5("Macierz p-wartości"),
      p(class = "text-muted",
        style = "font-size: 13px; margin-top: -4px;",
        "Odczytaj p-wartość dla każdej pary grup."),
      uiOutput("ch7_tukey_matrix"),

      br(),
      h5("Różnice parowe z 95% CI"),
      lc_plot("ch7_tukey_plot", ratio = "2.4/1", max_height = "260px"),

      uiOutput("ch7_tukey_result")
    ),

    lc_p("Przedział ufności różnicy i skorygowana p-wartość mówią tu to samo, tak
      jak przy związku testu z przedziałem z wykładu 03: para różni się istotnie
      dokładnie wtedy, gdy jej przedział nie obejmuje zera (linia przerywana).
      W scenariuszu fermentacji wszystkie trzy pary różnią się istotnie, bo
      sąsiednie temperatury dzieli w populacji 0.25 pH, a skrajne 0.5.
      Ciekawszy jest scenariusz stanowisk pracy ze zmienną stres. Średnie
      w populacji wynoszą 62 punkty na budowie, 58 w magazynie i 48 w biurze,
      przy odchyleniu standardowym 12. ANOVA jest istotna, ale post hoc zwykle
      pokazuje, że za wynik odpowiada biuro: różnica 4 punktów między budową
      a magazynem przy n = 160 najczęściej nie wychodzi istotna."),

    lc_p("Wynik testu mówi, czy różnice są większe niż szum, ale nie mówi, czy są
      duże. Miarą tego jest wielkość efektu, której poświęcony jest następny
      rozdział."),

    lc_h2("ch7-cas", "Ćwiczenia", "CASchools — ANOVA"),

    lc_p("Na koniec ćwiczenie na prawdziwych danych. W rozdziale 08 porównywaliśmy
      wyniki czytania w dwóch grupach okręgów szkolnych; teraz grup jest trzy,
      wyznaczonych przez dochód. Wykonaj analizę samodzielnie
      i dopiero potem porównaj wynik z rozwiązaniem. Rozwiązanie podaje
      klasyczną ANOVA, która zakłada równe wariancje w grupach."),

    lc_feedback(type = "info",
      p(tags$b("Dane:"), " 420 okręgów szkolnych Kalifornii (1998–1999). Plik: ",
        tags$code("dane/caschools.csv"), "."),
      p("Zmienne w zadaniu: ", tags$code("read"),
        " (wyniki z czytania), ", tags$code("income"),
        " (dochód okręgu, tys. USD).")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 10 — Czy wyniki czytania różnią się między tercylami dochodu?"),
      p("Podziel okręgi na trzy równe grupy dochodowe (tercyle):
        niski / średni / wysoki",
        " (użyj ", tags$em("split into groups"), " w jamovi lub utwórz zmienną
        ręcznie na podstawie kwantyli 0, 1/3, 2/3, 1).
        Wykonaj jednoczynnikową ANOVA dla zmiennej ", tags$code("read"),
        " między grupami. Zapisz: F, df, p.
        Wykonaj test post hoc Games-Howella i wskaż, które pary różnią się istotnie."),
      lc_action("cas_ch7_ans10", "Pokaż rozwiązanie", variant = "solid"),
      uiOutput("cas_ch7_sol10")
    ),

    lc_p("Zadanie przechodzi oba kroki rozdziału: najpierw ANOVA odpowiada, czy
      tercyle dochodu różnią się średnim wynikiem czytania, potem test post hoc
      wskazuje, które pary za to odpowiadają. Brakuje jeszcze odpowiedzi,
      jak duża jest ta różnica w sensie praktycznym. Dla ANOVA miarą siły efektu
      jest η², czyli część całkowitej zmienności wyjaśniona przynależnością
      do grupy. To ta sama dekompozycja zmienności, na której opiera się
      statystyka F. Zajmiemy się nią w następnym rozdziale."),

    lc_chapter_next(
      num       = "10",
      title     = "Siła efektu",
      lead      = "istotny wynik mówi, że różnica istnieje, ale nie mówi, jak jest duża.",
      target_id = "ch-sila-efektu"
    )
  )
)

# ============================================================================
# DANE — CASchools (wczytane raz przy ładowaniu modułu)
# ============================================================================

.ch7_cas <- read.csv(file.path(app_dir, "dane", "caschools.csv"),
                     stringsAsFactors = FALSE)

# ============================================================================
# SERVER
# ============================================================================

# Odmiana: 1 istotna różnica, 2–4 istotne różnice, 5+ istotnych różnic
ch7_plural_diff <- function(n) {
  if (n == 1) "istotna różnica"
  else if (n %% 10 %in% 2:4 && !(n %% 100 %in% 12:14)) "istotne różnice"
  else "istotnych różnic"
}

ch7_server <- function(input, output, session) {

  # --- Widget: inflacja błędu I rodzaju (Ryc. 9.1) ---

  ch7_motyw_k <- reactiveVal(2L)

  observeEvent(input$ch7_motyw_add, {
    ch7_motyw_k(min(ch7_motyw_k() + 1L, 8L))
  })
  observeEvent(input$ch7_motyw_reset, {
    ch7_motyw_k(2L)
  })

  zoom_plot_server("ch7_motyw_plot", reactive({
    k <- ch7_motyw_k()
    angles <- seq(0, 2 * pi, length.out = k + 1)[-(k + 1)]
    nodes <- data.frame(
      x     = cos(angles),
      y     = sin(angles),
      label = LETTERS[1:k]
    )
    pairs_idx <- combn(k, 2)
    edges <- data.frame(
      x1 = nodes$x[pairs_idx[1, ]],
      y1 = nodes$y[pairs_idx[1, ]],
      x2 = nodes$x[pairs_idx[2, ]],
      y2 = nodes$y[pairs_idx[2, ]]
    )
    n_tests <- ncol(pairs_idx)
    fwer    <- 1 - (1 - 0.05)^n_tests
    edge_col <- if (fwer < 0.15) upwr_accent else if (fwer < 0.35) "#D4860A" else "#9B1C1C"

    ggplot() +
      geom_segment(data = edges,
                   aes(x = x1, y = y1, xend = x2, yend = y2),
                   color = edge_col, alpha = 0.55, linewidth = 1.1) +
      geom_point(data = nodes, aes(x = x, y = y),
                 shape = 21, size = 16, fill = upwr_accent,
                 color = "white", stroke = 1.5) +
      geom_text(data = nodes, aes(x = x, y = y, label = label),
                color = "white", fontface = "bold", size = 5.5) +
      coord_equal(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5)) +
      theme_void()
  }))

  output$ch7_motyw_stats <- renderUI({
    k       <- ch7_motyw_k()
    n_tests <- k * (k - 1L) / 2L
    fwer    <- 1 - (1 - 0.05)^n_tests
    fwer_pct <- round(fwer * 100, 1)

    fb_type <- if (fwer < 0.10) "ok" else if (fwer < 0.30) "warning" else "danger"

    lc_feedback(type = fb_type,
      tags$table(class = "lc-table lc-table-sm", style = "margin: 0;",
        tags$tbody(
          tags$tr(tags$td("Grup:"),      tags$td(tags$b(k))),
          tags$tr(tags$td("Testów t:"),  tags$td(tags$b(n_tests))),
          tags$tr(tags$td("Ryzyko ≥ 1 błędu:"),
                  tags$td(tags$b(paste0(format(fwer_pct), "%"))))
        )
      )
    )
  })

  # Shared ANOVA data
  ch7_data_state <- reactiveVal(NULL)
  ch7_data <- reactive({
    state <- ch7_data_state()
    if (is.null(state)) return(NULL)
    req(input$ch7_scenario, input$ch7_n)

    if (!identical(state$scenario, input$ch7_scenario) ||
        !isTRUE(state$n == input$ch7_n)) {
      return(NULL)
    }

    state$data
  })

  # Konfiguracja scenariuszy: jakie zmienne zależne, jaka kolumna grupująca, etykiety
  ch7_scenario_cfg <- function(scenario) {
    if (identical(scenario, "fermentation")) {
      list(
        group_col = "temperatura",
        group_label = "Temperatura fermentacji",
        vars = c("pH jogurtu" = "pH", "Kwasowość miareczkowa (°SH)" = "kwasowosc_SH"),
        var_labels = c(pH = "pH jogurtu", kwasowosc_SH = "Kwasowość (°SH)")
      )
    } else if (identical(scenario, "workplace")) {
      list(
        group_col = "stanowisko",
        group_label = "Stanowisko pracy",
        vars = c("Dni nieobecności (urazy) / rok" = "nieobecnosci",
                 "Poziom stresu (0–100)" = "stres"),
        var_labels = c(nieobecnosci = "Dni nieobecności / rok",
                       stres = "Poziom stresu (0–100)")
      )
    } else {
      list(
        group_col = "kierunek",
        group_label = "Kierunek",
        vars = c("Średnia ocen" = "srednia_ocen", "Wzrost" = "wzrost", "Czas dojazdu" = "czas_dojazdu"),
        var_labels = c(srednia_ocen = "Średnia ocen", wzrost = "Wzrost (cm)", czas_dojazdu = "Czas dojazdu (min)")
      )
    }
  }

  output$ch7_var_ui <- renderUI({
    cfg <- ch7_scenario_cfg(input$ch7_scenario)
    selectInput("ch7_var", "Zmienna zależna:",
                choices = cfg$vars,
                selected = unname(cfg$vars[1]))
  })

  observeEvent(input$ch7_run_anova, {
    req(input$ch7_scenario, input$ch7_n)
    data <- if (identical(input$ch7_scenario, "fermentation")) {
      generate_fermentation_data(input$ch7_n)
    } else if (identical(input$ch7_scenario, "workplace")) {
      generate_workplace_data(input$ch7_n)
    } else {
      generate_student_data(input$ch7_n)
    }

    ch7_data_state(list(
      scenario = input$ch7_scenario,
      n = input$ch7_n,
      data = data
    ))
  }, ignoreInit = TRUE)

  # --- Widget 1: ANOVA ---
  zoom_plot_server("ch7_boxplot", reactive({
    data <- ch7_data()
    if (is.null(data)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Generuj i testuj”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      cfg <- ch7_scenario_cfg(input$ch7_scenario)
      var <- input$ch7_var
      req(var %in% names(data))
      var_label <- cfg$var_labels[[var]]
      group_col <- cfg$group_col

      ggplot(data, aes(x = .data[[group_col]], y = .data[[var]], fill = .data[[group_col]])) +
        geom_boxplot(alpha = 0.6, outlier.alpha = 0.3) +
        geom_jitter(width = 0.15, alpha = 0.2, size = 1) +
        scale_fill_upwr() +
        labs(
             x = cfg$group_label, y = var_label) +
                theme(legend.position = "none")
    }
  }))

  output$ch7_anova_result <- renderUI({
    data <- ch7_data()
    if (is.null(data)) return(NULL)

    cfg <- ch7_scenario_cfg(input$ch7_scenario)
    var <- input$ch7_var
    req(var %in% names(data))
    formula <- as.formula(paste(var, "~", cfg$group_col))

    result <- rstatix::anova_test(data, formula)
    tidy_res <- as.data.frame(result)

    p_val <- tidy_res$p
    res <- format_test_result(p_val)

    lc_feedback(type = "info",
      p(tags$strong("Wynik ANOVA jednoczynnikowej:")),
      p(paste0("F(", tidy_res$DFn, ", ", tidy_res$DFd, ") = ",
               round(tidy_res$F, 3))),
      ui_p_value(p_val),
      p(style = paste0("color:", res$color, "; font-weight: bold;"),
        res$decision)
    )
  })

  # --- Widget 2: Games-Howell post-hoc ---

  # Pomocnicza: licz raz, podawaj do plot/matrix/result
  ch7_gh_data <- reactive({
    req(input$ch7_run_tukey)
    data <- isolate(ch7_data())
    if (is.null(data)) return(NULL)
    cfg <- ch7_scenario_cfg(isolate(input$ch7_scenario))
    var <- isolate(input$ch7_var)
    req(var %in% names(data))
    formula <- as.formula(paste(var, "~", cfg$group_col))
    list(
      gh     = as.data.frame(rstatix::games_howell_test(data, formula)),
      groups = levels(factor(data[[cfg$group_col]]))
    )
  })

  # Macierz p-wartości (format jamovi: dolno-trójkątna, z gwiazdkami istotności)
  output$ch7_tukey_matrix <- renderUI({
    gd <- ch7_gh_data()
    if (is.null(gd)) return(NULL)
    groups <- gd$groups
    gh <- gd$gh
    k <- length(groups)

    # Pretty-print p (z gwiazdką przy istotnym)
    fmt_p <- function(p) {
      stars <- if (p < 0.001) " ***" else if (p < 0.01) " **" else if (p < 0.05) " *" else ""
      txt <- if (p < 0.001) "< 0.001" else sprintf("%.3f", p)
      list(txt = txt, stars = stars, sig = p < 0.05)
    }

    # Budujemy wiersze: pierwsza kolumna to nazwa grupy (wiersz),
    # potem po jednej komórce na każdą grupę wcześniejszą (dolno-trójkątna)
    rows <- lapply(seq_len(k), function(i) {
      cells <- lapply(seq_len(k), function(j) {
        if (j > i) {
          tags$td(style = "background: transparent; border: none;", "")
        } else if (j == i) {
          tags$td(style = "background: var(--upwr-surface-sunken); color: var(--upwr-reference); text-align: center;", "—")
        } else {
          # szukamy pary (groups[j], groups[i]) w gh_df
          idx <- which((gh$group1 == groups[j] & gh$group2 == groups[i]) |
                       (gh$group1 == groups[i] & gh$group2 == groups[j]))
          if (length(idx) == 0) return(tags$td(""))
          p  <- gh$p.adj[idx[1]]
          fp <- fmt_p(p)
          bg <- if (fp$sig) "var(--upwr-accent-tint)" else "var(--upwr-surface)"
          color <- if (fp$sig) "var(--upwr-accent)" else "var(--upwr-ink-soft)"
          weight <- if (fp$sig) "600" else "400"
          tags$td(
            style = paste0("background:", bg, "; color: ", color,
                           "; font-weight: ", weight,
                           "; text-align: right; font-variant-numeric: tabular-nums;"),
            fp$txt, tags$span(style = "color: var(--upwr-accent);", fp$stars)
          )
        }
      })
      tags$tr(
        tags$th(style = "text-align: left;", groups[i]),
        cells
      )
    })

    tags$table(
      class = "lc-table lc-table-bordered",
      style = "font-size: 14px; max-width: 560px;",
      tags$thead(
        tags$tr(
          tags$th(""),
          lapply(groups, function(g) tags$th(style = "text-align: center;", g))
        )
      ),
      tags$tbody(rows),
      tags$caption(style = "caption-side: bottom; font-size: 12px; color: var(--upwr-reference);",
        "p-wartości skorygowane metodą Games-Howella. ",
        tags$b("*"), " p < 0.05    ", tags$b("**"), " p < 0.01    ",
        tags$b("***"), " p < 0.001"
      )
    )
  })

  zoom_plot_server("ch7_tukey_plot", reactive({
    gd <- ch7_gh_data()
    if (is.null(gd)) return(NULL)

    gh_df <- gd$gh
    gh_df$comparison <- paste0(gh_df$group1, " — ", gh_df$group2)
    gh_df$significant <- gh_df$p.adj < 0.05

    ggplot(gh_df, aes(x = estimate, y = comparison, color = significant)) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = upwr_secondary) +
      scale_color_manual(values = c("TRUE" = col_reject, "FALSE" = col_accept),
                         labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                         name = NULL) +
      labs(x = "Różnica średnich", y = NULL) +
      theme(legend.position = "top")
  }))

  output$ch7_tukey_result <- renderUI({
    gd <- ch7_gh_data()
    if (is.null(gd)) {
      return(lc_feedback(type = "warning", "Najpierw uruchom ANOVA."))
    }
    gh_df <- gd$gh
    sig_pairs <- gh_df[gh_df$p.adj < 0.05, ]
    n_sig <- nrow(sig_pairs)

    if (n_sig == 0) {
      lc_feedback(type = "info",
        p(tags$strong("Żadna para nie różni się istotnie"),
          " (po korekcie Games-Howella)."))
    } else {
      lc_feedback(type = "ok",
        p(tags$strong(paste0(n_sig, " ", ch7_plural_diff(n_sig), ":"))),
        tags$ul(
          lapply(1:n_sig, function(i) {
            tags$li(paste0(sig_pairs$group1[i], " — ", sig_pairs$group2[i],
                           ": Δ = ", round(sig_pairs$estimate[i], 2),
                           ", p.adj = ", format_p_value(sig_pairs$p.adj[i])))
          })
        )
      )
    }
  })

  # --- Ćwiczenia CASchools ---

  cas_vis10 <- reactiveVal(FALSE)

  observeEvent(input$cas_ch7_ans10, {
    nowy <- !cas_vis10()
    cas_vis10(nowy)
    updateActionButton(session, "cas_ch7_ans10",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$cas_ch7_sol10 <- renderUI({
    if (!cas_vis10()) return(NULL)
    r <- local({
      edu <- .ch7_cas
      q   <- quantile(edu$income, probs = c(0, 1/3, 2/3, 1))
      edu$income_group <- cut(edu$income, breaks = q,
        labels = c("Niski", "Średni", "Wysoki"), include.lowest = TRUE)
      grp_stats <- tapply(edu$read, edu$income_group, function(x)
        c(n = length(x), m = mean(x), s = sd(x)))
      av <- rstatix::anova_test(edu, read ~ income_group)
      gh <- as.data.frame(rstatix::games_howell_test(edu, read ~ income_group))
      list(grp_stats = grp_stats, F = av$F, df1 = av$DFn, df2 = av$DFd,
           p = av$p, gh = gh)
    })
    lc_feedback(type = "ok", style = "margin-top: 10px;",
      p(tags$b("H₀:"), " μ_niski = μ_średni = μ_wysoki · ",
        tags$b("Hₐ:"), " co najmniej jedna para się różni"),
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        tags$thead(tags$tr(
          tags$th("Tercyl"), tags$th("n"), tags$th("x̄ read"), tags$th("s")
        )),
        tags$tbody(lapply(c("Niski","Średni","Wysoki"), function(g) {
          v <- r$grp_stats[[g]]
          tags$tr(tags$td(g), tags$td(v["n"]),
                  tags$td(round(v["m"], 2)), tags$td(round(v["s"], 2)))
        }))
      ),
      tags$ul(
        tags$li(sprintf("F(%d, %d) = %.3f, %s",
          r$df1, r$df2, r$F, format_p(r$p)))
      ),
      if (r$p < 0.05) tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀")
      else tags$b("Brak podstaw do odrzucenia H₀"),
      p(tags$b("Post hoc Games-Howell:")),
      tags$ul(lapply(seq_len(nrow(r$gh)), function(i) {
        g <- r$gh[i, ]
        tags$li(sprintf("%s − %s: Δ = %.2f pkt, p.adj = %s",
          g$group2, g$group1, g$estimate, format_p_value(g$p.adj)))
      }))
    )
  })
}
