# ============================================================================
# CHAPTER 7: ANOVA
# ============================================================================

# Dane stałe dla scen (Ryc. 9.3, 9.4, 9.6; przyjęte 2026-11-20) „Skąd się bierze rozrzut”,
# „Dwie partie” i „Co odstaje?” (modules/scenes.js). Liczone raz przy starcie.
.ch7_scene_parts <- function(y, g) {
  g <- factor(g)
  gm <- mean(y); means <- tapply(y, g, mean); nj <- tapply(y, g, length)
  ssb <- sum(nj * (means - gm)^2); ssw <- sum((y - means[g])^2)
  k <- nlevels(g); n <- length(y)
  list(gm = gm, means = unname(means), ssb = ssb, ssw = ssw, sst = ssb + ssw,
       msb = ssb / (k - 1), msw = ssw / (n - k), F = (ssb / (k - 1)) / (ssw / (n - k)))
}

.ch7_scenes <- local({
  temps <- c("20 °C", "25 °C", "30 °C")

  # 1. Skąd się bierze rozrzut: 3 komory × 5 słoików, świat rozdziału
  set.seed(19)
  y1 <- round(c(rnorm(5, 4.55, 0.14), rnorm(5, 4.30, 0.14), rnorm(5, 4.05, 0.14)), 2)
  g1 <- rep(1:3, each = 5)
  jars <- c(list(kind = "jars", height = 420, lo = 3.8, hi = 4.8, temps = temps,
                 aria = "Słoiki jogurtu z trzech komór na osi pH",
                 y = unname(split(y1, g1))),
            .ch7_scene_parts(y1, g1))

  # 2. Dwie partie: te same odchylenia od średnich komór, inna skala.
  #    Świat „różnią się”: średnie daleko, słoiki ciasno; „bez znaczenia”: odwrotnie.
  #    Rozrzut całkowity w obu światach prawie ten sam.
  set.seed(3)
  z <- matrix(rnorm(21), 7, 3)
  z <- sweep(z, 2, colMeans(z)); z <- z / sqrt(sum(z^2) / 18)
  g2 <- rep(1:3, each = 7)
  mk <- function(m, s) {
    y <- round(as.vector(sweep(z * s, 2, m, "+")), 2)
    c(list(y = unname(split(y, g2))), .ch7_scene_parts(y, g2))
  }
  worlds <- list(kind = "worlds", height = 432, lo = 3.8, hi = 4.8, temps = temps,
                 aria = "Dwie partie słoików: średnie komór i rozrzut w komorach",
                 world = "diff",
                 worlds = list(diff = mk(c(4.55, 4.30, 4.05), 0.06),
                               same = mk(4.30 + c(0.09, -0.06, -0.03), 0.217)))

  # 3. Co odstaje?: stres na trzech stanowiskach, 12 osób na grupę
  set.seed(156)
  grp <- c("budowa", "magazyn", "biuro")
  d3 <- data.frame(g = factor(rep(grp, each = 12), levels = grp),
                   y = round(c(rnorm(12, 62, 12), rnorm(12, 58, 12), rnorm(12, 48, 12))))
  a3 <- rstatix::anova_test(d3, y ~ g)
  gh <- rstatix::games_howell_test(d3, y ~ g)
  stress <- list(kind = "stress", height = 420, lo = 20, hi = 90, groups = grp,
                 aria = "Stres pracowników na trzech stanowiskach",
                 y = unname(split(d3$y, d3$g)),
                 means = unname(tapply(d3$y, d3$g, mean)),
                 F = a3$F, p = a3$p,
                 pairs = lapply(seq_len(nrow(gh)), function(i) list(
                   a = match(gh$group1[i], grp) - 1, b = match(gh$group2[i], grp) - 1,
                   p = gh$p.adj[i], sig = gh$p.adj[i] < 0.05)))

  set.seed(NULL)
  list(jars = jars, worlds = worlds, stress = stress)
})

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

# Treść stała (dawniej output$cas_ch7_sol10 na serwerze): składana przy budowaniu strony.
cas_ch7_sol10_ui <- function() {
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
  tagList(
    p(tags$b("H₀:"), " μ", tags$sub("niski"), " = μ", tags$sub("średni"), " = μ", tags$sub("wysoki"), " · ",
      tags$b("Hₐ:"), " co najmniej jedna para się różni"),
    local({
      lv <- c("Niski", "Średni", "Wysoki")
      stat <- function(k) vapply(lv, function(g) unname(r$grp_stats[[g]][k]), numeric(1))
      lc_table(
        data.frame(group = lv, n = stat("n"), m = stat("m"), s = stat("s")),
        cols = list(
          lc_col("group", "Tercyl", "row"),
          lc_col("n", "n"),
          lc_col("m", "x̄ read", digits = 2),
          lc_col("s", "s", digits = 2)
        )
      )
    }),
    tags$ul(
      tags$li(sprintf("F(%d, %d) = %.3f, %s",
        r$df1, r$df2, r$F, format_p(r$p)))
    ),
    lc_verdict(tags$strong(if (r$p < 0.05) "Odrzucamy H₀" else "Brak podstaw do odrzucenia H₀"),
               type = if (r$p < 0.05) "danger" else "ok"),
    p(tags$b("Post hoc Games-Howell:")),
    tags$ul(lapply(seq_len(nrow(r$gh)), function(i) {
      g <- r$gh[i, ]
      tags$li(sprintf("%s − %s: Δ = %.2f pkt, p.adj = %s",
        g$group2, g$group1, g$estimate, format_p_value(g$p.adj)))
    }))
  )
}

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

    # Ryc. 9.1: żelki i trądzik, 20 podgrup (modules/proto05.R, rysunek: proto05.js)
    ch7_fig_jelly,

    lc_p("Najprostszy pomysł to wykonać ", gloss("test t", "test t"), " dla każdej
      pary grup. Przy trzech grupach A, B i C są to trzy porównania: A z B, A z C
      oraz B z C. Kłopot leży w tym, co oznacza ", gloss("poziom istotności"), ". Każdy test na
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

    # Liczba par i ryzyko serii testów ze znacznikami
    lc_formula_annotated(
      "⟦1|m⟧ = \\frac{⟦2|k⟧(k-1)}{2}, \\qquad ⟦3|P(\\text{co najmniej jeden fałszywy alarm})⟧ = 1 - (1 - ⟦4|\\alpha⟧)^m",
      notes  = list("liczba par", "liczba grup", "ryzyko w całej serii", "poziom istotności"),
      colors = c("szalwia", "szalwia", "bursztyn", "accent"),
      above  = c(3)
    ),

    lc_p("Panel rysuje grupy jako wierzchołki, a każdą parę do porównania jako
      odcinek. Obok podaje liczbę testów i ryzyko obliczone z tego wzoru."),

    figure_panel(
      label = "Ryc. 9.2",
      title = "Dodaj grupy i obserwuj inflację błędu I rodzaju",
      # Rysunki dla 2–8 grup składa R (modules/figures.R); licznik i odczyty prowadzi przeglądarka.
      ch7_fwer_fig(),
      lc_caption("Start od 2 grup. Każde kliknięcie dodaje jedną.")
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
      w k grupach wyznaczonych przez jedną ", gloss("zmienna jakościowa", "zmienną jakościową"), ", stąd nazwa
      jednoczynnikowa. ", gloss("zmienna zależna", "Zmienna zależna"), " jest ilościowa, a czynnikiem jest zmienna
      grupująca. Przykład z panelu niżej: pH jogurtu (zmienna zależna) po
      fermentacji w trzech temperaturach, 20, 25 i 30 °C (czynnik o trzech
      poziomach). Hipotezy mają postać:"),

    lc_formula_box(
      p(withMathJax("\\(H_0: \\mu_1 = \\mu_2 = \\ldots = \\mu_k\\) — wszystkie średnie są równe")),
      p(withMathJax("\\(H_a:\\) co najmniej jedna średnia różni się od pozostałych"))
    ),

    lc_p(gloss("hipoteza alternatywna", "Hipoteza alternatywna"), " nie twierdzi, że wszystkie średnie są różne.
      Wystarczy, że jedna grupa odstaje od reszty."),

    lc_p("Nazwa „analiza wariancji” bierze się ze sposobu działania testu.
      W wykładzie 01, przy porównaniu wzrostu kobiet i mężczyzn, ", gloss("odchylenie standardowe", "odchylenie
      standardowe"), " wynosiło 6.0 cm w grupie kobiet i 6.5 cm w grupie mężczyzn,
      a w całej próbie 8.1 cm. Część rozrzutu całej próby brała się z różnicy
      między grupami, a nie ze zmienności wewnątrz nich. ANOVA zamienia tę
      obserwację w test. Całkowitą zmienność danych rozkłada na zmienność ",
      tags$em("między"), " grupami (jak daleko średnie grup leżą od średniej
      ogólnej) i zmienność ", tags$em("wewnątrz"), " grup (jak daleko obserwacje
      leżą od średniej własnej grupy). Tak wygląda ten podział na piętnastu
      słoikach jogurtu z trzech komór fermentacyjnych."),

    # Ryc. 9.3: skąd się bierze rozrzut (scena, modules/scenes.js); od 2026-11-20 przed wzorem F
    figure_panel(
      label = "Ryc. 9.3", width_mode = "text",
      scene_widget("ch7_sc_jars",
        title = "Skąd się bierze rozrzut",
        steps = c("Słoiki", "Całość", "Dwie części", "F"),
        config = .ch7_scenes$jars,
        texts = list(
        "Pięć słoików z każdej komory. Przerywana linia to średnie pH wszystkich piętnastu.",
        "Każdy słoik leży w pewnej odległości od średniej ogólnej. Suma kwadratów tych odległości to cały rozrzut.",
        "Drogę do słoika dzielimy w średniej jego komory: bursztynowy odcinek należy do komory, niebieski do słoika.",
        "Każdą część dzielimy przez jej stopnie swobody. F to stosunek górnego paska do dolnego."
        )
      )
    ),

    lc_p(gloss("statystyka F", "Statystyka F"), " zapisuje ostatni kadr sceny wzorem.
      To stosunek zmienności między grupami do zmienności wewnątrz grup, każdej
      podzielonej przez jej stopnie swobody:"),

    # Statystyka F ze znacznikami. Licznik i mianownik w kolorach
    # scen (bursztyn = między, niebo = wewnątrz); opisy zastępują słowny ułamek.
    lc_formula_annotated(
      "⟦1|F⟧ = \\frac{⟦2|\\sum_{j=1}^{k} n_j\\,(\\bar{x}_j - \\bar{x})^2 \\,/\\, (k-1)⟧}{⟦3|\\sum_{j=1}^{k} \\sum_{i=1}^{n_j} (x_{ij} - \\bar{x}_j)^2 \\,/\\, (n-k)⟧}",
      notes  = list("statystyka F", "zmienność między grupami", "zmienność wewnątrz grup"),
      colors = c("indygo", "bursztyn", "niebo"),
      above  = c(2)
    ),

    lc_p("Tu \\(\\bar{x}_j\\) i \\(n_j\\) to średnia i liczebność j-tej grupy,
      \\(\\bar{x}\\) — średnia wszystkich n obserwacji. Licznik i mianownik to ",
      gloss("wariancja", "wariancje"), ": sumy kwadratów odchyleń podzielone przez
      liczby ", gloss("stopnie swobody", "stopni swobody"), ", k - 1 i n − k."),

    # Ryc. 9.4: dwie partie (scena, modules/scenes.js)
    lc_p("Ten sam rozrzut całkowity może się podzielić zupełnie inaczej."),

    figure_panel(
      label = "Ryc. 9.4", width_mode = "text",
      title = "Dwie partie",
      scene_static(.ch7_scenes$worlds, options = list(
        list(name = "world", label = "Partia", selected = "diff",
             values = c("Komory się różnią" = "diff", "Komory bez znaczenia" = "same"))
      ))
    ),

    lc_p("Gdy H₀ jest prawdziwa, średnie grup różnią się tylko przypadkowo. Licznik
      i mianownik mierzą wtedy ten sam losowy szum, więc F wychodzi w okolicach 1.
      Gdy średnie w populacji się różnią, licznik rośnie i F staje się duże. Przy
      prawdziwej H₀ statystyka F ma rozkład F z k - 1 i n − k stopniami swobody.
      ", gloss("p-wartość", "P-wartość"), " to prawdopodobieństwo, że przy prawdziwej H₀ wypadnie wartość F
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
      tych założeń zajmiemy się w wykładzie 06."),

    # ========================================================================
    # Ćwiczenie: sformułuj hipotezy
    # ========================================================================
    lc_h2("ch7-cwiczenie", "Ćwiczenie: sformułuj hipotezy"),

    lc_p("Niezależnie od liczby grup ANOVA ma jedną parę hipotez. Zapisz H₀ i Hₐ
      dla poniższych sytuacji przed odsłonięciem odpowiedzi."),

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

    lc_p("Panel losuje dane z jednego z czterech scenariuszy, rysuje ", gloss("wykres pudełkowy", "wykresy pudełkowe"), "
      w grupach i liczy ANOVA. Suwak n ustala liczebność całej próby; każda
      obserwacja trafia do grupy losowo, więc liczebności grup są zbliżone, ale
      nie równe. Panel liczy klasyczną ANOVA."),

    figure_panel(
      label = "Ryc. 9.5",
      title = "ANOVA jednoczynnikowa",
      # Losowanie, wykres i ANOVA liczy przeglądarka (modules/figures.js, rycina s05_anova).
      lc_sf_figure("s05_anova", config = ch7_anova_config,
        control = list(
          selectInput("ch7_scenario", "Scenariusz", selectize = FALSE,
            choices = c(
              "Fermentacja jogurtu (TŻ)" = "fermentation",
              "Kierunki studiów" = "students",
              "Stanowisko pracy a wypadki/stres (IB)" = "workplace",
              "Pokrycie terenu z satelity (IDSiK)" = "landcover"
            ),
            selected = "fermentation"
          ),
          lc_sf_slider("ch7_n", "n (ogółem)", 80, 300, 160, 20)
        ),
        toolbar = lc_sf_action("gen", "ch7_run_anova", "Generuj i testuj", variant = "solid"),
        ratio = "2.2/1", max_height = "350px",
        after = tagList(
          lc_sf_empty("Kliknij „Generuj i testuj”"),
          tags$div(`data-role` = "result"),
          # Pozycje listy zależą od scenariusza: wymienia je przeglądarka.
          lc_sf_ctl(selectInput("ch7_var", "Zmienna zależna", selectize = FALSE,
            choices = unlist(ch7_anova_config$scenarios$fermentation$vars)))
        ))
    ),

    lc_p("W domyślnym scenariuszu dane pochodzą z populacji, w których średnie pH
      wynoszą 4.55 przy 20 °C, 4.30 przy 25 °C i 4.05 przy 30 °C, a odchylenie
      standardowe wewnątrz każdej grupy to 0.14. Różnica między sąsiednimi
      temperaturami jest prawie dwa razy większa niż rozrzut wewnątrz grupy,
      więc pudełka praktycznie się nie nakładają. Przy n = 160 statystyka ma
      rozkład F(2, 157), którego ", gloss("wartość krytyczna"), " dla α = 0.05 wynosi około 3.05.
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

    lc_p("Scenariusz pokrycia terenu ma ten sam układ na danych satelitarnych.
      Temperatura powierzchni (średnio 22 °C w lesie, 26 °C na polu i 30 °C
      w zabudowie) i wskaźnik zieleni NDVI różnią się między typami terenu.
      Zachmurzenie generator losuje niezależnie od typu terenu, więc dla niego
      H₀ jest prawdziwa."),

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

    # Ryc. 9.6: co odstaje? (scena, modules/scenes.js)
    lc_p("Tak to wygląda na stresie pracowników trzech stanowisk."),

    figure_panel(
      label = "Ryc. 9.6", width_mode = "text",
      scene_widget("ch7_sc_stress",
        title = "Co odstaje?",
        steps = c("Trzy grupy", "ANOVA", "Które pary"),
        config = .ch7_scenes$stress,
        texts = list(
        "Po dwanaście osób z budowy, magazynu i biura. Kreska to średnia grupy.",
        "ANOVA odrzuca H₀: co najmniej jedna średnia odstaje. Nie mówi, która.",
        "Games-Howell porównuje każdą parę ze skorygowaną p-wartością."
        )
      )
    ),

    lc_p("Panel używa ", gloss("test Games-Howella", "testu Games-Howella"), ".
      Każdą parę porównuje statystyką podobną do testu t, ale nie zakłada
      równych wariancji ani równych liczebności grup, a wartości krytyczne bierze z rozkładu, który uwzględnia
      liczbę porównywanych grup. To bezpieczny wybór domyślny: gdy wariancje są
      podobne, daje wyniki zbliżone do popularnego testu Tukeya, a gdy się różnią,
      nie traci kontroli nad błędem I rodzaju. Panel łączy klasyczną ANOVA,
      która zakłada równe wariancje, z testem post hoc, który tego nie zakłada.
      Przy wyraźnie różnych wariancjach spójniejsza jest para ANOVA Welcha
      i Games-Howell."),

    lc_note("Zasada", rule = TRUE,
      "Testy post hoc wykonuj po istotnej ANOVA. Gdy ANOVA nie odrzuca H₀,
       porównań parami nie interpretujemy."
    ),

    lc_p("Panel korzysta z danych wylosowanych w Ryc. 9.5. Macierz p-wartości ma
      układ typowej tabeli post hoc, a wykres pokazuje różnicę średnich dla każdej pary
      z 95-procentowym ", gloss("przedział ufności", "przedziałem ufności"), "."),

    figure_panel(
      label = "Ryc. 9.7",
      title = "Games-Howell",
      # Porównania par liczy przeglądarka na danych z Ryc. 9.5 (shared/lc_stats.js, rycina s05_gh).
      lc_sf_figure("s05_gh", legend = TRUE,
        toolbar = lc_sf_action("test", "ch7_run_tukey", "Testuj Games-Howellem", variant = "solid"),
        body = tagList(
          lc_caption("Panel używa danych z Ryc. 9.5. Najpierw uruchom tam ANOVA."),
          tags$div(`data-when` = "nodata", hidden = NA, lc_caption("Najpierw uruchom ANOVA.")),
          tags$h4(`data-gh-part` = NA, hidden = NA, "Macierz p-wartości"),
          tags$div(`data-role` = "matrix"),
          tags$h4(`data-gh-part` = NA, hidden = NA, "Różnice parowe z 95% CI"),
          lc_sf_chart(0, "2.4/1", "260px"),
          tags$div(`data-role` = "result")
        ))
    ),

    lc_p("Przedział ufności różnicy i skorygowana p-wartość mówią tu to samo, tak
      jak przy związku testu z przedziałem z wykładu 04: para różni się istotnie
      dokładnie wtedy, gdy jej przedział nie obejmuje zera (linia przerywana).
      W scenariuszu fermentacji wszystkie trzy pary różnią się istotnie, bo
      sąsiednie temperatury dzieli w populacji 0.25 pH, a skrajne 0.5.
      Ciekawszy jest scenariusz stanowisk pracy ze zmienną stres. Średnie
      w populacji wynoszą 62 punkty na budowie, 58 w magazynie i 48 w biurze,
      przy odchyleniu standardowym 12. ANOVA jest istotna, ale post hoc zwykle
      pokazuje, że za wynik odpowiada biuro: różnica 4 punktów między budową
      a magazynem przy n = 160 najczęściej nie wychodzi istotna."),

    lc_p("Wynik testu mówi, czy różnice są większe niż szum, ale nie mówi, czy są
      duże. Miarą tego jest ", gloss("wielkość efektu"), ", której poświęcony jest następny
      rozdział."),

    lc_h2("ch7-cas", "Ćwiczenia", "CASchools — ANOVA"),

    lc_p("Na koniec ćwiczenie na prawdziwych danych. W rozdziale 08 porównywaliśmy
      wyniki czytania w dwóch grupach okręgów szkolnych; teraz grup jest trzy,
      wyznaczonych przez dochód. Wykonaj analizę samodzielnie
      i dopiero potem porównaj wynik z rozwiązaniem. Rozwiązanie podaje
      klasyczną ANOVA, która zakłada równe wariancje w grupach."),

    lc_note("Dane",
      p("420 okręgów szkolnych Kalifornii (1998–1999). Plik: ",
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
      lc_more("Rozwiązanie", cas_ch7_sol10_ui())
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
