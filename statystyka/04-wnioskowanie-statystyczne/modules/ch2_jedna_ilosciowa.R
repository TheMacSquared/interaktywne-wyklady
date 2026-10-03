# ============================================================================
# CHAPTER 4: Jedna zmienna ilościowa — test t jednej próby
# ============================================================================

ch2_ui <- list(
  id = "ch-jedna-ilosciowa", num = "04", title = "Test t jednej próby",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 04 · Testowanie hipotez",
      num    = "04",
      title  = "Test t jednej próby.",
      lead   = "Test t jednej próby sprawdza, czy średnia w populacji może być
                równa wartości referencyjnej. Korzysta z tych samych składników
                co przedział ufności dla średniej: średniej z próby, błędu
                standardowego i rozkładu t-Studenta."
    ),

    lc_p("Poprzednie rozdziały opisywały logikę testowania w ogólnej postaci:
      hipotezy, dwa rodzaje błędów, p-wartość i werdykt. Teraz zastosujemy
      ją do pierwszego konkretnego testu. Zaczynamy od najprostszej sytuacji:
      mamy jedną zmienną ilościową i pytamy, czy jej średnia w populacji
      zgadza się ze znaną z góry wartością, na przykład z normą, deklaracją
      producenta albo średnią krajową. Taką wartość oznaczamy \\(\\mu_0\\)."),

    # ========================================================================
    # Case study otwierający
    # ========================================================================
    lc_h2("ch2-pytanie", "Od pytania do testu"),

    lc_p("Analiza nie zaczyna się od wzorów, tylko od pytania. Ktoś przychodzi
      i pyta w języku potocznym:"),

    lc_feedback(type = "info", style = "font-size: 18px; text-align: center;",
      tags$em("„Czy nasi studenci mają typowy poziom koncentracji?
      Bo wydaje mi się, że coś z nimi jest nie tak.”")
    ),

    lc_p("Żeby na to odpowiedzieć, trzeba najpierw ustalić, co znaczy „typowy”.
      Załóżmy, że mamy wartość referencyjną z badania pilotażowego: średni
      wynik testu koncentracji w populacji wynosi 70 pkt. Pytanie dotyczy
      średniej \\(\\mu\\) w populacji naszych studentów, a nie średniej
      z konkretnej próby. Jak w rozdziale 02, zależnie od tego, w którą stronę
      pytamy, otrzymujemy jedną z trzech par hipotez:"),

    lc_formula_box(
      p(tags$b("Dwustronna"), " (czy średnia w ogóle się różni):"),
      p(withMathJax("\\(H_0: \\mu = 70 \\quad\\)"),
        withMathJax("\\(H_a: \\mu \\neq 70\\)"))
    ),
    lc_formula_box(
      p(tags$b("Prawostronna"), " (czy średnia jest ",
        tags$em("wyższa"), " niż norma):"),
      p(withMathJax("\\(H_0: \\mu \\leq 70 \\quad\\)"),
        withMathJax("\\(H_a: \\mu > 70\\)"))
    ),
    lc_formula_box(
      p(tags$b("Lewostronna"), " (czy średnia jest ",
        tags$em("niższa"), " niż norma):"),
      p(withMathJax("\\(H_0: \\mu \\geq 70 \\quad\\)"),
        withMathJax("\\(H_a: \\mu < 70\\)"))
    ),

    lc_p("Wariant wynika z brzmienia ", gloss("pytanie badawcze", "pytania
      badawczego"), " i trzeba go wybrać przed zebraniem danych. Zdanie
      „coś z nimi jest nie tak” nie wskazuje kierunku, więc odpowiada mu
      wariant dwustronny."),

    lc_p("Niezależnie od wariantu liczymy tę samą ",
      gloss("statystyka testowa", "statystykę testową"), ". Mierzy ona, ile ",
      gloss("błąd standardowy", "błędów standardowych"), " dzieli średnią
      z próby od wartości referencyjnej \\(\\mu_0\\). Oba składniki znamy
      z wykładu 03: błąd standardowy średniej \\(SE = s/\\sqrt{n}\\) i rozkład
      t-Studenta z \\(n - 1\\) stopniami swobody. Tak powstaje statystyka ",
      gloss("test t", "testu t"), " jednej próby:"),

    lc_formula_box(
      withMathJax("$$t = \\frac{\\bar{x} - \\mu_0}{s / \\sqrt{n}}, \\quad df = n - 1$$")
    ),

    lc_p("Licznik mówi, jak daleko średnia z próby leży od \\(\\mu_0\\)
      w jednostkach pomiaru. Mianownik przelicza tę odległość na błędy
      standardowe. Dzięki temu ta sama różnica 2 pkt znaczy co innego, gdy
      średnia waha się z próby na próbę o pół punktu, a co innego, gdy waha
      się o pięć punktów. Jeśli H₀ jest prawdziwa, czyli \\(\\mu = \\mu_0\\),
      statystyka t ma rozkład t-Studenta z \\(df = n - 1\\). Ten rozkład
      pokazuje, jakie wartości t pojawiają się z samego przypadku, i na nim
      liczymy ", gloss("p-wartość", "p-wartość"), ". Warianty testu różnią się
      tylko tym, które ogony rozkładu biorą do p-wartości: dwustronny oba,
      jednostronny jeden."),

    # ========================================================================
    # Ćwiczenie: sformułuj hipotezy
    # ========================================================================
    lc_h2("ch2-cwiczenie", "Ćwiczenie: sformułuj hipotezy"),

    lc_p("Zanim zobaczysz test w działaniu, przećwicz jego pierwszy krok,
      czyli przekład pytania na hipotezy. Dla każdego pytania ustal, o jaką
      średnią chodzi, jaka jest wartość referencyjna i czy pytanie wskazuje
      kierunek. Dopiero potem odkryj odpowiedź."),

    hypothesis_practice("ch2", list(
      list(
        question = "Producent deklaruje, że średnia zawartość soli w chlebie
                    wynosi 1,2 g / 100 g. Chcemy sprawdzić, czy jego deklaracja
                    pasuje do rzeczywistości.",
        h0 = "\\(H_0: \\mu = 1{,}2\\) (zgodnie z deklaracją)",
        ha = "\\(H_a: \\mu \\neq 1{,}2\\) (odbiega od deklaracji)",
        note = "Dwustronny — nie wiemy, w którą stronę może odbiegać."
      ),
      list(
        question = "Norma technologiczna przewiduje, że dojrzewanie sera trwa
                    średnio 45 dni. Producent twierdzi, że jego nowa metoda
                    skraca ten czas.",
        h0 = "\\(H_0: \\mu \\geq 45\\) (nie krócej niż norma)",
        ha = "\\(H_a: \\mu < 45\\) (krócej)",
        note = "Jednostronny (lewostronny) — kierunek wynika z treści pytania."
      ),
      list(
        question = "Sprawdzamy, czy średnia waga paczki kawy (deklarowana 250 g)
                    jest zgodna z normą. Dla konsumenta ważne jest wykrycie
                    odchyłek w obie strony.",
        h0 = "\\(H_0: \\mu = 250\\)",
        ha = "\\(H_a: \\mu \\neq 250\\)",
        note = "Dwustronny — interesuje nas każde odchylenie, nie tylko niższa waga."
      )
    )),

    lc_p("We wszystkich trzech pytaniach wartość \\(\\mu_0\\) pochodzi spoza
      danych: z deklaracji, normy albo obietnicy producenta. Kierunek testu
      także wynika z treści pytania, a nie z tego, co pokażą pomiary. Dane
      potrzebne są dopiero w następnym kroku."),

    # ========================================================================
    # WIDGET 1: Krokowy test t jednej próby
    # ========================================================================
    lc_h2("ch2-krok", "Test t jednej próby — krok po kroku"),

    lc_p("Panel przeprowadza test dwustronny w pięciu scenariuszach. W każdym
      dane losowane są z rozkładu normalnego, którego prawdziwa średnia nieco
      różni się od \\(\\mu_0\\), więc H₀ jest w nich fałszywa. Kolejne kroki
      prowadzą od histogramu ", gloss("próba", "próby"), " przez średnią
      i statystykę t do decyzji."),

    figure_panel(
      label = "Ryc. 4.1",
      title = "Test t jednej próby — krok po kroku",
      uiOutput("ch2_hypothesis_panel"),
      lc_step_widget("ch2_test",
        steps = c("Dane", "Statystyki opisowe", "Statystyka testowa",
                  "p-wartość i decyzja"),
        toolbar = lc_toolbar(
          selectInput("ch2_scenario", "Scenariusz",
            choices = c(
              "Koncentracja (μ₀ = 70 pkt)" = "concentration",
              "Zużycie wody (μ₀ = 150 l)" = "water",
              "Plon pszenicy (μ₀ = 5 t/ha)" = "yield",
              "Trwałość jogurtu (μ₀ = 14 dni)" = "yogurt",
              "Hałas w hali (μ₀ = 85 dB, IB)" = "noise"
            ),
            selected = "concentration"
          ),
          lc_slider("ch2_n", "Wielkość próby (n)", 10, 100, 40, 5),
          lc_action("ch2_new_sample", "Losuj próbę", icon = "shuffle", variant = "solid")
        ),
        plot_id = "ch2_step_plot"
      )
    ),

    lc_p("Cztery kroki panelu to cały test. Najpierw oglądamy próbę. Potem
      liczymy jej średnią \\(\\bar{x}\\) i ", gloss("odchylenie standardowe"),
      " \\(s\\), a z nich błąd standardowy. W trzecim kroku przeliczamy
      odległość \\(\\bar{x}\\) od \\(\\mu_0\\) na statystykę t i nanosimy ją
      na rozkład t, jakiego oczekiwalibyśmy przy prawdziwej H₀. W czwartym
      sprawdzamy, czy wynik leży w obszarze odrzucenia, i liczymy
      p-wartość. W teście dwustronnym jest to pole obu ogonów rozkładu
      poza \\(\\pm|t|\\):"),

    lc_formula_box(
      withMathJax("$$p = P\\left(|T| \\geq |t| \\;\\middle|\\; H_0\\right), \\qquad T \\sim t_{n-1}$$")
    ),

    lc_p("Jak w rozdziale 03, p-wartość to prawdopodobieństwo, że przy
      prawdziwej H₀ statystyka wypadnie co najmniej tak daleko od zera jak
      nasza. Nie jest to prawdopodobieństwo, że H₀ jest prawdziwa. Porównujemy
      ją z poziomem istotności \\(\\alpha\\). Wartość 0,05 to konwencja, ale
      tak jak poziom ufności w wykładzie 03 trzeba ją ustalić przed analizą,
      a nie dobierać do wyniku. Gdy \\(p < \\alpha\\), odrzucamy H₀. Gdy
      \\(p \\geq \\alpha\\), nie mamy podstaw do odrzucenia H₀, co nie znaczy,
      że średnia w populacji wynosi dokładnie \\(\\mu_0\\)."),

    lc_p("Scenariusz koncentracji dobrze pokazuje, co znaczy to ostatnie
      zastrzeżenie. Dane losowane są z populacji o średniej 72 pkt
      i odchyleniu standardowym 13 pkt, więc H₀: μ = 70 jest fałszywa. Przy
      n = 40 błąd standardowy wynosi około \\(13/\\sqrt{40} \\approx 2{,}06\\)
      pkt, a różnica 2 pkt to średnio mniej niż jeden błąd standardowy.
      Wartość krytyczna dla df = 39 wynosi 2,02, więc test odrzuca H₀ tylko
      w około 16% prób. W pozostałych popełnia błąd II rodzaju. Przy n = 100
      moc rośnie do około 33%. W scenariuszu hałasu prawdziwa średnia
      (87,5 dB) leży ponad pół odchylenia standardowego (4 dB) od normy
      i już przy n = 40 test odrzuca H₀ w około 97% prób."),

    lc_p("Test t i przedział ufności dla średniej z wykładu 03 są zbudowane
      z tych samych elementów. Przedział to \\(\\bar{x} \\pm t^* \\cdot SE\\),
      a test odrzuca H₀, gdy \\(|t| \\geq t^*\\), czyli gdy \\(\\mu_0\\) leży
      dalej od \\(\\bar{x}\\) niż \\(t^* \\cdot SE\\). To dwa zapisy tego
      samego warunku:"),

    lc_formula_box(
      withMathJax("$$|t| \\geq t^*_{\\alpha/2,\\, n-1} \\iff \\mu_0 \\notin \\bar{x} \\pm t^*_{\\alpha/2,\\, n-1} \\cdot \\frac{s}{\\sqrt{n}}$$")
    ),

    lc_p("Test dwustronny na poziomie \\(\\alpha\\) odrzuca H₀: μ = μ₀
      dokładnie wtedy, gdy ", gloss("przedział ufności"), " na poziomie
      \\(1 - \\alpha\\) nie obejmuje \\(\\mu_0\\). Przy α = 0,05 odpowiada mu
      przedział 95%. Przedział mówi przy tym więcej niż sam werdykt: pokazuje
      wszystkie wartości \\(\\mu_0\\), których test by nie odrzucił, a więc
      także to, jak duża może być różnica."),

    lc_p("Test opiera się na założeniach: obserwacje są niezależne, a średnia
      z próby ma rozkład zbliżony do normalnego. Drugie założenie spełniają
      dane bez silnej skośności albo odpowiednio duża próba; im bardziej
      skośny rozkład, tym większej próby potrzeba. Jak to sprawdzać, omawia
      wykład 05."),

    # ========================================================================
    # WIDGET 2: Test jednostronny — to samo pytanie, ale z kierunkiem
    # ========================================================================
    lc_h2("ch2-jednostronny", "A jeśli znamy kierunek? Test jednostronny"),

    lc_p("Test powyżej pytał, czy średnia różni się od \\(\\mu_0\\) w którąkolwiek
      stronę. Czasem pytanie od początku wskazuje kierunek: czy zużycie wody
      przekracza normę, czy hałas jest wyższy niż dopuszczalny. Wtedy, jak
      w rozdziale 02, Hₐ jest jednostronna. Panel poniżej bierze tę samą
      próbę co panel dwustronny i zadaje jej pytanie kierunkowe."),

    figure_panel(
      label = "Ryc. 4.2",
      title = "Test t jednostronny — krok po kroku",
      uiOutput("ch2b_hypothesis_panel"),
      lc_step_widget("ch2b_test",
        steps = c("Dane", "Statystyki opisowe", "Statystyka testowa",
                  "p-wartość i decyzja"),
        toolbar = lc_toolbar(
          helpText("Dane: te same co w teście dwustronnym powyżej.")
        ),
        plot_id = "ch2b_step_plot"
      )
    ),

    lc_p("Średnia, odchylenie standardowe i statystyka t są takie same jak
      w teście dwustronnym, bo dane się nie zmieniły. Zmienia się obszar
      odrzucenia: całe α = 0,05 leży w jednym ogonie, więc wartość krytyczna
      przy n = 40 (df = 39) spada z 2,02 do 1,68. Zmienia się też p-wartość. Jeśli t
      leży po stronie wskazanej przez Hₐ, p-wartość jednostronna jest
      dokładnie połową dwustronnej. W scenariuszu zużycia wody (prawdziwa
      średnia 158 l przy normie 150 l) test dwustronny przy n = 40 odrzuca
      H₀ w około 51% prób, a prawostronny w około 63%."),

    lc_p("Ta przewaga działa tylko w jednym kierunku. W scenariuszu koncentracji
      pytanie kierunkowe brzmi „Czy studenci mają niższą koncentrację niż
      norma 70 pkt?”,
      a dane pochodzą z populacji o średniej 72 pkt. Średnia z próby zwykle
      wypada więc powyżej 70, t jest dodatnie, a p-wartość lewostronna
      przekracza 0,5. Test odrzuca H₀ w mniej niż 1% prób, choć średnia
      naprawdę różni się od normy. Test jednostronny nie widzi odchylenia
      w przeciwną stronę, niezależnie od jego wielkości."),

    inline_callout(
      label = "Zasada",
      "Kierunek testu ustal przed zebraniem danych, na podstawie pytania.
       Wybór strony po obejrzeniu wyników podwaja rzeczywiste ryzyko błędu
       I rodzaju: przy α = 0,05 faktycznie wynosi ono 10%."
    ),

    # ========================================================================
    # Ćwiczenia — CASchools
    # ========================================================================
    lc_h2("ch2-cas", "Ćwiczenia", "CASchools — test t jednej próby"),

    lc_p("Na koniec dwa zadania na prawdziwych danych. Pierwsze wymaga testu
      dwustronnego, drugie jednostronnego. W obu najpierw zapisz H₀ i Hₐ,
      wykonaj test i sformułuj wniosek w języku pytania,
      a dopiero potem porównaj go z rozwiązaniem."),

    lc_feedback(type = "info",
      p(tags$b("Dane:"), " 420 okręgów szkolnych Kalifornii (1998–1999). Plik: ",
        tags$code("dane/caschools.csv"), "."),
      p("Zmienne w zadaniach: ", tags$code("read"),
        " (średni wynik z czytania, ok. 655 pkt), ",
        tags$code("income"), " (dochód okręgu, tys. USD).")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 1 — Czy wyniki z czytania różnią się od normy 650 pkt?"),
      p("Departament edukacji podaje normę 650 pkt. Przetestuj, czy średni wynik ",
        tags$code("read"), " w okręgach Kalifornii istotnie różni się",
        " od 650. Sformułuj H₀ i Hₐ, wykonaj test t jednej próby (α = 0,05).
        Co raportowałbyś departamentowi?"),
      lc_action("cas_ch2_ans1", "Pokaż rozwiązanie", variant = "solid"),
      uiOutput("cas_ch2_sol1")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 2 — Czy typowy dochód okręgu przekracza 15 tys. USD?"),
      p("Hipoteza dyrekcji: „Nasz stan to stan zamożnych” — typowy okręg ma dochód
        powyżej 15 tys. USD. Przetestuj jednostronnie (prawostronnie)",
        " zmienną ", tags$code("income"),
        ". Sformułuj H₀ i Hₐ dla hipotezy kierunkowej.
        Czy wynik jest istotny statystycznie? A praktycznie?"),
      lc_action("cas_ch2_ans2", "Pokaż rozwiązanie", variant = "solid"),
      uiOutput("cas_ch2_sol2")
    ),

    lc_p("Oba zadania mają ten sam schemat: pytanie, hipotezy, statystyka t,
      p-wartość i powrót do języka pytania. Drugie pytanie z zadania 2,
      o znaczenie praktyczne, wykracza poza sam test. Werdykt mówi, czy
      różnicę da się odróżnić od przypadku, ale nie mówi, czy jest duża.
      Do tego wrócimy w rozdziale 10."),

    lc_chapter_next(
      num       = "05",
      title     = "Test proporcji",
      lead      = "ta sama logika dla odsetka zamiast średniej.",
      target_id = "ch-jedna-jakosciowa"
    )
  )
)

# ============================================================================
# DANE — CASchools (wczytane raz przy ładowaniu modułu)
# ============================================================================

.ch2_cas <- read.csv(file.path(app_dir, "dane", "caschools.csv"),
                     stringsAsFactors = FALSE)


# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  # --- Dane scenariuszy ---
  scenario_params <- list(
    concentration = list(mu0 = 70, mu_true = 72, sd = 13,
                         xlab = "Wynik testu koncentracji (pkt)",
                         title = "Koncentracja studentów",
                         question = "Czy nasi studenci mają typowy poziom koncentracji?",
                         h0_text = "\\(H_0: \\mu = 70\\) (koncentracja jest typowa)",
                         h1_text = "\\(H_a: \\mu \\neq 70\\) (koncentracja odbiega od normy)"),
    water  = list(mu0 = 150, mu_true = 158, sd = 25,
                  xlab = "Zużycie wody (l/osobę/dobę)",
                  title = "Zużycie wody w gminie",
                  question = "Czy zużycie wody w naszej gminie spełnia normę projektową 150 l/osobę?",
                  h0_text = "\\(H_0: \\mu = 150\\) (zużycie zgodne z normą)",
                  h1_text = "\\(H_a: \\mu \\neq 150\\) (zużycie odbiega od normy)"),
    yield  = list(mu0 = 5, mu_true = 5.4, sd = 0.8,
                  xlab = "Plon pszenicy (t/ha)",
                  title = "Plony na poletku doświadczalnym",
                  question = "Czy średni plon pszenicy na naszych poletkach odpowiada średniej krajowej 5 t/ha?",
                  h0_text = "\\(H_0: \\mu = 5\\) (plon typowy dla kraju)",
                  h1_text = "\\(H_a: \\mu \\neq 5\\) (plon odbiega od średniej krajowej)"),
    yogurt = list(mu0 = 14, mu_true = 15.2, sd = 2.5,
                  xlab = "Trwałość (dni do przeterminowania)",
                  title = "Trwałość jogurtu naturalnego",
                  question = "Czy trwałość naszego jogurtu spełnia deklarowane 14 dni?",
                  h0_text = "\\(H_0: \\mu = 14\\) (trwałość zgodna z deklaracją)",
                  h1_text = "\\(H_a: \\mu \\neq 14\\) (trwałość odbiega od deklaracji)"),
    noise  = list(mu0 = 85, mu_true = 87.5, sd = 4,
                  xlab = "Poziom hałasu (dB)",
                  title = "Hałas w hali produkcyjnej",
                  question = "Czy średni poziom hałasu w hali mieści się w normie NDS 85 dB?",
                  h0_text = "\\(H_0: \\mu = 85\\) (hałas zgodny z normą)",
                  h1_text = "\\(H_a: \\mu \\neq 85\\) (hałas odbiega od normy)")
  )

  # Jedna wspólna próba dla widgetu dwustronnego i jednostronnego.
  # Trzymamy metadane próbki, żeby po zmianie scenariusza albo n stara
  # próbka była traktowana jako nieaktualna, a nie jako dane do nowego pytania.
  ch2_sample_state <- reactiveVal(NULL)

  observeEvent(input$ch2_new_sample, {
    req(input$ch2_scenario, input$ch2_n)
    par <- scenario_params[[input$ch2_scenario]]
    req(!is.null(par))
    n <- input$ch2_n

    ch2_sample_state(list(
      scenario = input$ch2_scenario,
      n = n,
      values = rnorm(n, mean = par$mu_true, sd = par$sd)
    ))
  }, ignoreInit = TRUE)

  ch2_sample <- reactive({
    state <- ch2_sample_state()
    if (is.null(state)) return(NULL)
    req(input$ch2_scenario, input$ch2_n)

    if (!identical(state$scenario, input$ch2_scenario) ||
        !isTRUE(state$n == input$ch2_n)) {
      return(NULL)
    }

    state$values
  })

  # Krok widgetu (1..4) żyje w przeglądarce; nowa próba ani zmiana
  # scenariusza nie cofa kroku.
  ch2_step <- lc_step_server("ch2_test", input)$step

  # Histogram danych (kroki 1–2): rama z danych i μ₀, wspólna dla obu kroków.
  ch2_hist_plot <- function(samp, mu0, xlab, step) {
    rng <- range(c(samp, mu0))
    pad <- diff(rng) * 0.06
    breaks <- seq(min(samp), max(samp), length.out = 16)
    y_top <- max(hist(samp, breaks = breaks, plot = FALSE)$counts) * 1.3
    x_bar <- mean(samp)

    p <- ggplot(data.frame(x = samp), aes(x = x)) +
      step_result(geom_histogram, breaks = breaks) +
      labs(x = xlab, y = "Liczba")

    if (step >= 2) {
      # μ₀ znamy z hipotezy; średnia z próby jest nowa w kroku 2.
      mean_right <- x_bar >= mu0
      p <- p +
        step_line("known", xintercept = mu0) +
        step_line(step_role(step, 2), xintercept = x_bar, helper = FALSE) +
        step_symbol_label(mu0, y_top * 0.95, paste0("mu[0] == ", mu0), role = "known",
                          hjust = if (mean_right) 1.1 else -0.1, vjust = 1) +
        step_symbol_label(x_bar, y_top * 0.95, paste0("bar(x) == ", lc_fmt(x_bar, 2)),
                          role = step_role(step, 2),
                          hjust = if (mean_right) -0.1 else 1.1, vjust = 1)
    }
    p + step_frame(xlim = rng + c(-pad, pad), ylim = c(0, y_top))
  }

  # --- Panel hipotezy (zawsze widoczny) ---
  output$ch2_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch2_scenario]]
    samp <- ch2_sample()

    div(class = "ch2-step-panel",
      lc_feedback(type = "info", style = "font-size: 16px;",
        p(tags$b("Pytanie potoczne:")),
        p(tags$em(paste0("„", par$question, "”")))
      ),
      lc_formula_box(
        p(tags$b("Hipoteza formalna (dwustronna):")),
        p(withMathJax(par$h0_text)),
        p(withMathJax(par$h1_text))
      ),
      if (is.null(samp)) {
        div(style = "text-align: center; margin: 10px 0; color: var(--upwr-reference);",
          p(tags$em("Kliknij „Losuj próbę”, żeby zebrać dane"))
        )
      }
    )
  })

  # --- Krokowy wykres ---
  zoom_plot_server("ch2_step_plot", reactive({
    samp <- ch2_sample()
    step <- ch2_step()
    par <- scenario_params[[input$ch2_scenario]]
    mu0 <- par$mu0

    if (is.null(samp)) return(NULL)

    if (step <= 2) {
      ch2_hist_plot(samp, mu0, par$xlab, step)
    } else {
      n <- length(samp)
      t_stat <- (mean(samp) - mu0) / (sd(samp) / sqrt(n))
      step_null_plot(t_stat, df = n - 1, type = "t",
                     phase = if (step == 3) "stat" else "decision")
    }
  }))

  # --- Opis kroku ---
  output$ch2_test_text <- renderUI({
    samp <- ch2_sample()
    step <- ch2_step()
    par <- scenario_params[[input$ch2_scenario]]
    mu0 <- par$mu0

    if (is.null(samp)) return(NULL)

    n <- length(samp)
    x_bar <- mean(samp)
    s <- sd(samp)
    se <- s / sqrt(n)
    t_stat <- (x_bar - mu0) / se
    p_val <- 2 * pt(-abs(t_stat), df = n - 1)

    switch(as.character(step),
      "1" = tagList(
        "Mamy próbę ", step_num(n), " obserwacji. Chcemy sprawdzić, czy średnia
        różni się od μ₀ = ", step_num(mu0), "."
      ),
      "2" = tagList(
        "x̄ = ", step_num(lc_fmt(x_bar, 2)), ", s = ", step_num(lc_fmt(s, 2)),
        ", SE = s/√n = ", step_num(lc_fmt(se, 2)), ". Różnica między x̄ a μ₀: ",
        step_num(lc_fmt(x_bar - mu0, 2)),
        ". Żeby ocenić, czy to dużo, odnosimy ją do SE."
      ),
      "3" = tagList(
        paste0("t = (", lc_fmt(x_bar, 2), " − ", mu0, ") / ", lc_fmt(se, 2), " = "),
        step_num(lc_fmt(t_stat, 3)), ". Statystyka t mówi: średnia z próby jest ",
        step_num(lc_fmt(abs(t_stat), 1)), " błędów standardowych od μ₀."
      ),
      "4" = step_verdict(p_val)
    )
  })

  # --- Widget 2: Test jednostronny (te same dane co Widget 1) ---
  scenario_params_1s <- list(
    concentration = list(alt = "less",
                         question = "Czy studenci mają niższą koncentrację niż norma 70 pkt?",
                         h0_text = "\\(H_0: \\mu \\geq 70\\) (koncentracja nie jest niższa)",
                         h1_text = "\\(H_a: \\mu < 70\\) (koncentracja jest niższa niż norma)"),
    water  = list(alt = "greater",
                  question = "Czy zużycie wody w gminie przekracza normę projektową 150 l/osobę?",
                  h0_text = "\\(H_0: \\mu \\leq 150\\) (zużycie nie przekracza normy)",
                  h1_text = "\\(H_a: \\mu > 150\\) (zużycie przekracza normę)"),
    yield  = list(alt = "greater",
                  question = "Czy nowa odmiana daje wyższy plon niż średnia krajowa 5 t/ha?",
                  h0_text = "\\(H_0: \\mu \\leq 5\\) (plon nie jest wyższy)",
                  h1_text = "\\(H_a: \\mu > 5\\) (plon jest wyższy niż średnia krajowa)"),
    yogurt = list(alt = "greater",
                  question = "Czy trwałość jogurtu jest dłuższa niż deklarowane 14 dni?",
                  h0_text = "\\(H_0: \\mu \\leq 14\\) (trwałość nie przekracza deklaracji)",
                  h1_text = "\\(H_a: \\mu > 14\\) (trwałość jest dłuższa niż deklarowana)"),
    noise  = list(alt = "greater",
                  question = "Czy średni poziom hałasu w hali przekracza normę NDS 85 dB?",
                  h0_text = "\\(H_0: \\mu \\leq 85\\) (hałas w normie)",
                  h1_text = "\\(H_a: \\mu > 85\\) (hałas przekracza normę)")
  )

  # Krok widgetu 2 (1..4); wspólna próba nie cofa kroku.
  ch2b_step <- lc_step_server("ch2b_test", input)$step

  # Panel hipotezy (jednostronny) — zawsze widoczny jako nagłówek
  output$ch2b_hypothesis_panel <- renderUI({
    par1s <- scenario_params_1s[[input$ch2_scenario]]
    samp <- ch2_sample()

    div(class = "ch2-step-panel",
      lc_feedback(type = "info", style = "font-size: 16px;",
        p(tags$b("Pytanie potoczne (kierunkowe):")),
        p(tags$em(paste0("„", par1s$question, "”")))
      ),
      lc_formula_box(
        p(tags$b("Hipoteza formalna (jednostronna):")),
        p(withMathJax(par1s$h0_text)),
        p(withMathJax(par1s$h1_text))
      ),
      if (is.null(samp)) {
        div(style = "text-align: center; margin: 10px 0; color: var(--upwr-reference);",
          p(tags$em("Najpierw wylosuj próbę w teście dwustronnym powyżej"))
        )
      }
    )
  })

  # Krokowy wykres (jednostronny)
  zoom_plot_server("ch2b_step_plot", reactive({
    samp <- ch2_sample()
    step <- ch2b_step()
    par <- scenario_params[[input$ch2_scenario]]
    par1s <- scenario_params_1s[[input$ch2_scenario]]
    mu0 <- par$mu0

    if (is.null(samp)) return(NULL)

    if (step <= 2) {
      ch2_hist_plot(samp, mu0, par$xlab, step)
    } else {
      n <- length(samp)
      t_stat <- (mean(samp) - mu0) / (sd(samp) / sqrt(n))
      step_null_plot(t_stat, df = n - 1, type = "t", alternative = par1s$alt,
                     phase = if (step == 3) "stat" else "decision")
    }
  }))

  # Opis kroku (jednostronny)
  output$ch2b_test_text <- renderUI({
    samp <- ch2_sample()
    step <- ch2b_step()
    par <- scenario_params[[input$ch2_scenario]]
    par1s <- scenario_params_1s[[input$ch2_scenario]]
    mu0 <- par$mu0

    if (is.null(samp)) return(NULL)

    n <- length(samp)
    x_bar <- mean(samp)
    s <- sd(samp)
    se <- s / sqrt(n)
    t_stat <- (x_bar - mu0) / se

    # p-wartość jednostronna
    p_val <- if (par1s$alt == "less") {
      pt(t_stat, df = n - 1)
    } else {
      pt(t_stat, df = n - 1, lower.tail = FALSE)
    }

    dir_label <- if (par1s$alt == "less") "mniejsza" else "większa"

    switch(as.character(step),
      "1" = tagList(
        "n = ", step_num(n), " (te same dane co wyżej). Pytamy, czy średnia jest ",
        dir_label, " niż μ₀ = ", step_num(mu0), "."
      ),
      "2" = tagList(
        "x̄ = ", step_num(lc_fmt(x_bar, 2)), ", s = ", step_num(lc_fmt(s, 2)),
        ", SE = s/√n = ", step_num(lc_fmt(se, 2)), ", t = ",
        step_num(lc_fmt(t_stat, 3)), " — te same wartości co w teście dwustronnym."
      ),
      "3" = tagList(
        "t = ", step_num(lc_fmt(t_stat, 3)), ". W teście jednostronnym patrzymy
        tylko na ", if (par1s$alt == "less") "lewy" else "prawy", " ogon rozkładu."
      ),
      "4" = tagList(
        "Jednostronnie: ", step_verdict(p_val)
      )
    )
  })

  # --- Ćwiczenia CASchools ---

  cas_vis1 <- reactiveVal(FALSE)
  cas_vis2 <- reactiveVal(FALSE)

  observeEvent(input$cas_ch2_ans1, {
    nowy <- !cas_vis1()
    cas_vis1(nowy)
    updateActionButton(session, "cas_ch2_ans1",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$cas_ch2_sol1 <- renderUI({
    if (!cas_vis1()) return(NULL)
    r <- local({
      x <- .ch2_cas$read; mu <- 650
      n <- length(x); m <- mean(x); s <- sd(x); se <- s / sqrt(n)
      t_val <- (m - mu) / se; df <- n - 1
      p_val <- 2 * pt(-abs(t_val), df)
      d <- (m - mu) / s
      list(n = n, m = m, s = s, t = t_val, df = df, p = p_val, d = d)
    })
    div(class = "ch2-step-panel",
      lc_feedback(type = "ok", style = "margin-top: 10px;",
        p(tags$b("H₀:"), " μ_read = 650 · ", tags$b("Hₐ:"), " μ_read ≠ 650"),
        tags$ul(
          tags$li(sprintf("n = %d, x̄ = %.2f, s = %.2f", r$n, r$m, r$s)),
          tags$li(sprintf("t(%s) = %.3f, p %s %s",
            round(r$df, 1), r$t,
            if (r$p < 0.001) "<" else "=",
            if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
        ),
        if (r$p < 0.05) tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀")
        else tags$b("Brak podstaw do odrzucenia H₀"),
        p(tags$b("Interpretacja:"), " ",
          if (r$p < 0.05) {
            sprintf(
              "średni wynik z czytania (%.2f pkt) istotnie statystycznie różni się
               od normy 650 pkt. Różnica wynosi %.2f pkt.",
              r$m, r$m - 650
            )
          } else {
            sprintf(
              "nie mamy podstaw, by twierdzić, że średni wynik z czytania (%.2f pkt)
               różni się od normy 650 pkt.",
              r$m
            )
          })
      )
    )
  })

  observeEvent(input$cas_ch2_ans2, {
    nowy <- !cas_vis2()
    cas_vis2(nowy)
    updateActionButton(session, "cas_ch2_ans2",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$cas_ch2_sol2 <- renderUI({
    if (!cas_vis2()) return(NULL)
    r <- local({
      x <- .ch2_cas$income; mu <- 15
      n <- length(x); m <- mean(x); s <- sd(x); se <- s / sqrt(n)
      t_val <- (m - mu) / se; df <- n - 1
      p_val <- pt(t_val, df, lower.tail = FALSE)
      d <- (m - mu) / s
      list(n = n, m = m, s = s, t = t_val, df = df, p = p_val, d = d)
    })
    div(class = "ch2-step-panel",
      lc_feedback(type = "ok", style = "margin-top: 10px;",
        p(tags$b("H₀:"), " μ_income ≤ 15 · ", tags$b("Hₐ:"), " μ_income > 15"),
        tags$ul(
          tags$li(sprintf("n = %d, x̄ = %.2f, s = %.2f (tys. USD)", r$n, r$m, r$s)),
          tags$li(sprintf("t(%s) = %.3f, p %s %s (jednostronnie)",
            round(r$df, 1), r$t,
            if (r$p < 0.001) "<" else "=",
            if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
        ),
        if (r$p < 0.05) tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀")
        else tags$b("Brak podstaw do odrzucenia H₀"),
        p(tags$b("Interpretacja:"), " ",
          if (r$p < 0.05) {
            sprintf(
              "średni dochód okręgu (%.2f tys. USD) jest istotnie statystycznie wyższy
               od 15 tys. USD. Różnica wynosi %.2f tys. USD.",
              r$m, r$m - 15
            )
          } else {
            sprintf(
              "średni dochód w próbie (%.2f tys. USD) jest wyższy od 15 tys. USD
               o %.2f tys. USD, ale taka nadwyżka mieści się w przypadkowych wahaniach.
               Nie mamy podstaw, by twierdzić, że średni dochód okręgu przekracza
               15 tys. USD. Brak istotności nie dowodzi też, że średnia wynosi
               15 tys. USD lub mniej. Praktycznie różnica też jest niewielka:
               to około %.2f odchylenia standardowego dochodu.",
              r$m, r$m - 15, r$d
            )
          }),
        p("Hipotezę kierunkową formułujemy przed zebraniem danych; wybór kierunku
          po obejrzeniu wyników zawyża ryzyko błędu I rodzaju.")
      )
    )
  })
}
