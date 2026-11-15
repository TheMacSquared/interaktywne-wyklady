# ============================================================================
# CHAPTER 4: Jedna zmienna ilościowa — test t jednej próby
# ============================================================================

# Dane ćwiczeń CASchools: potrzebne już przy składaniu rozwiązań (niżej).
.ch2_cas <- read.csv(file.path(app_dir, "dane", "caschools.csv"),
                     stringsAsFactors = FALSE)

# Treść stała (dawniej output$cas_ch2_sol1 na serwerze): składana przy budowaniu strony.
cas_ch2_sol1_ui <- function() {
  r <- local({
    x <- .ch2_cas$read; mu <- 650
    n <- length(x); m <- mean(x); s <- sd(x); se <- s / sqrt(n)
    t_val <- (m - mu) / se; df <- n - 1
    p_val <- 2 * pt(-abs(t_val), df)
    d <- (m - mu) / s
    list(n = n, m = m, s = s, t = t_val, df = df, p = p_val, d = d)
  })
  div(class = "ch2-step-panel",
    tagList(
      p(tags$b("H₀:"), " μ_read = 650 · ", tags$b("Hₐ:"), " μ_read ≠ 650"),
      tags$ul(
        tags$li(sprintf("n = %d, x̄ = %.2f, s = %.2f", r$n, r$m, r$s)),
        tags$li(sprintf("t(%s) = %.3f, p %s %s",
          round(r$df, 1), r$t,
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
      ),
      lc_verdict(tags$strong(if (r$p < 0.05) "Odrzucamy H₀" else "Brak podstaw do odrzucenia H₀"),
                 type = if (r$p < 0.05) "danger" else "ok"),
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
}

# Treść stała (dawniej output$cas_ch2_sol2 na serwerze): składana przy budowaniu strony.
cas_ch2_sol2_ui <- function() {
  r <- local({
    x <- .ch2_cas$income; mu <- 15
    n <- length(x); m <- mean(x); s <- sd(x); se <- s / sqrt(n)
    t_val <- (m - mu) / se; df <- n - 1
    p_val <- pt(t_val, df, lower.tail = FALSE)
    d <- (m - mu) / s
    list(n = n, m = m, s = s, t = t_val, df = df, p = p_val, d = d)
  })
  div(class = "ch2-step-panel",
    tagList(
      p(tags$b("H₀:"), " μ_income ≤ 15 · ", tags$b("Hₐ:"), " μ_income > 15"),
      tags$ul(
        tags$li(sprintf("n = %d, x̄ = %.2f, s = %.2f (tys. USD)", r$n, r$m, r$s)),
        tags$li(sprintf("t(%s) = %.3f, p %s %s (jednostronnie)",
          round(r$df, 1), r$t,
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
      ),
      lc_verdict(tags$strong(if (r$p < 0.05) "Odrzucamy H₀" else "Brak podstaw do odrzucenia H₀"),
                 type = if (r$p < 0.05) "danger" else "ok"),
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
}

# Scenariusze testu t jednej próby (Ryc. 4.1 i 4.2): parametry losowania i teksty hipotez.
ch2_scenario_params <- list(
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

# Te same scenariusze z pytaniem kierunkowym (test jednostronny).
ch2_scenario_params_1s <- list(
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

# Panel hipotezy nad krokami: wariant wybiera lista „Scenariusz”; podpowiedź widać,
# dopóki nie ma próby (element [data-when = "empty"], przełącza rycina).
ch2_hypothesis_panel <- function(one_sided = FALSE) {
  pars <- if (one_sided) ch2_scenario_params_1s else ch2_scenario_params
  tagList(
    lc_variants("ch2_scenario", lapply(pars, function(par) {
      div(class = "ch2-step-panel",
        lc_status(
          p(tags$b(if (one_sided) "Pytanie potoczne (kierunkowe):" else "Pytanie potoczne:")),
          p(tags$em(paste0("„", par$question, "”")))
        ),
        lc_formula_box(
          p(tags$b(if (one_sided) "Hipoteza formalna (jednostronna):" else "Hipoteza formalna (dwustronna):")),
          p(withMathJax(par$h0_text)),
          p(withMathJax(par$h1_text))
        )
      )
    })),
    tags$div(`data-when` = "empty", lc_empty(if (one_sided) {
      "Najpierw wylosuj próbę w teście dwustronnym powyżej"
    } else {
      "Kliknij „Losuj próbę”, żeby zebrać dane"
    }))
  )
}
# Dane dla rycin s05_t1 i s05_t1b (modules/figures.js).
ch2_t1_config <- list(scenarios = lapply(names(ch2_scenario_params), function(k) {
  c(ch2_scenario_params[[k]][c("mu0", "mu_true", "sd", "xlab")], list(alt = ch2_scenario_params_1s[[k]]$alt))
}) |> stats::setNames(names(ch2_scenario_params)))

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
      mamy jedną ", gloss("zmienna ilościowa", "zmienną ilościową"), " i pytamy, czy jej średnia w populacji
      zgadza się ze znaną z góry wartością, na przykład z normą, deklaracją
      producenta albo średnią krajową. Taką wartość oznaczamy \\(\\mu_0\\)."),

    # ========================================================================
    # Case study otwierający
    # ========================================================================
    lc_h2("ch2-pytanie", "Od pytania do testu"),

    lc_p("Analiza nie zaczyna się od wzorów, tylko od pytania. Ktoś przychodzi
      i pyta w języku potocznym:"),

    lc_note("Pytanie",
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
      z wykładu 04: błąd standardowy średniej \\(SE = s/\\sqrt{n}\\) i ", gloss("rozkład t-Studenta", "rozkład
      t-Studenta"), " z \\(n - 1\\) ", gloss("stopnie swobody", "stopniami swobody"), ". Tak powstaje statystyka ",
      gloss("test t", "testu t"), " jednej próby:"),

    # Statystyka t jednej próby ze znacznikami
    lc_formula_annotated(
      "⟦1|t⟧ = \\frac{⟦2|\\bar{x}⟧ - ⟦3|\\mu_0⟧}{⟦4|s⟧ / \\sqrt{⟦5|n⟧}}, \\quad ⟦6|df⟧ = n - 1",
      notes  = list("statystyka t", "średnia z próby", "wartość referencyjna",
                    "odchylenie standardowe", "liczba obserwacji", "stopnie swobody"),
      colors = c("indygo", "accent", "param", "wrzos", "szalwia", "szalwia"),
      above  = c(2, 3)
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

    lc_p("Przed obejrzeniem testu w działaniu przećwicz jego pierwszy krok,
      czyli zamianę pytania na hipotezy. Dla każdego pytania ustal, o jaką
      średnią chodzi, jaka jest wartość referencyjna i czy pytanie wskazuje
      kierunek. Dopiero potem odkryj odpowiedź."),

    hypothesis_practice("ch2", list(
      list(
        question = "Producent deklaruje, że średnia zawartość soli w chlebie
                    wynosi 1.2 g / 100 g. Chcemy sprawdzić, czy jego deklaracja
                    pasuje do rzeczywistości.",
        h0 = "\\(H_0: \\mu = 1.2\\) (zgodnie z deklaracją)",
        ha = "\\(H_a: \\mu \\neq 1.2\\) (odbiega od deklaracji)",
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

    lc_p("Panel przeprowadza ", gloss("test dwustronny"), " w pięciu scenariuszach. W każdym
      dane losowane są z ", gloss("rozkład normalny", "rozkładu normalnego"), ", którego prawdziwa średnia nieco
      różni się od \\(\\mu_0\\), więc H₀ jest w nich fałszywa. Kolejne kroki
      prowadzą od ", gloss("histogram", "histogramu"), " ", gloss("próba", "próby"), " przez średnią
      i statystykę t do decyzji."),

    figure_panel(
      label = "Ryc. 4.1",
      title = "Test t jednej próby — krok po kroku",
      # Losowanie, wykresy kroków i opisy liczy przeglądarka (modules/figures.js, rycina s05_t1).
      lc_sf_figure("s05_t1", bare = TRUE, config = ch2_t1_config, body = tagList(
        ch2_hypothesis_panel(),
        lc_step_widget("ch2_test",
          steps = c("Dane", "Statystyki opisowe", "Statystyka testowa",
                    "p-wartość i decyzja"),
          toolbar = lc_toolbar(
            lc_sf_ctl(selectInput("ch2_scenario", "Scenariusz", selectize = FALSE,
              choices = c(
                "Koncentracja (μ₀ = 70 pkt)" = "concentration",
                "Zużycie wody (μ₀ = 150 l)" = "water",
                "Plon pszenicy (μ₀ = 5 t/ha)" = "yield",
                "Trwałość jogurtu (μ₀ = 14 dni)" = "yogurt",
                "Hałas w hali (μ₀ = 85 dB, IB)" = "noise"
              ),
              selected = "concentration"
            )),
            lc_sf_slider("ch2_n", "Wielkość próby (n)", 10, 100, 40, 5),
            lc_sf_action("new", "ch2_new_sample", "Losuj próbę", icon = "shuffle", variant = "solid"),
            lc_chart_zoom_button()
          ),
          body = lc_sf_chart(0, "2.5/1", "380px")
        )
      ))
    ),

    lc_p("Cztery kroki panelu to cały test. Najpierw oglądamy próbę. Potem
      liczymy jej średnią \\(\\bar{x}\\) i ", gloss("odchylenie standardowe"),
      " \\(s\\), a z nich błąd standardowy. W trzecim kroku przeliczamy
      odległość \\(\\bar{x}\\) od \\(\\mu_0\\) na statystykę t i nanosimy ją
      na rozkład t, jakiego oczekiwalibyśmy przy prawdziwej H₀. W czwartym
      sprawdzamy, czy wynik leży w ", gloss("obszar odrzucenia", "obszarze odrzucenia"), ", i liczymy
      p-wartość. W teście dwustronnym jest to pole obu ogonów rozkładu
      poza \\(\\pm|t|\\):"),

    # Dwustronna p-wartość testu t ze znacznikami
    lc_formula_annotated(
      "⟦1|p⟧ = P\\left(⟦2|\\left|T\\right|⟧ \\geq ⟦3|\\left|t\\right|⟧ \\;\\middle|\\; H_0\\right), \\qquad T \\sim ⟦4|t_{n-1}⟧",
      notes  = list("p-wartość", "statystyka przy H₀", "statystyka z próby", "rozkład t-Studenta"),
      colors = c("bursztyn", "niebo", "indygo", "indygo"),
      above  = c(2)
    ),

    lc_p("Jak w rozdziale 03, p-wartość to prawdopodobieństwo, że przy
      prawdziwej H₀ statystyka wypadnie co najmniej tak daleko od zera jak
      nasza. Nie jest to prawdopodobieństwo, że H₀ jest prawdziwa. Porównujemy
      ją z ", gloss("poziom istotności", "poziomem istotności"), " \\(\\alpha\\). Wartość 0.05 to konwencja, ale
      tak jak ", gloss("poziom ufności"), " w wykładzie 04 trzeba ją ustalić przed analizą,
      a nie dobierać do wyniku. Gdy \\(p < \\alpha\\), odrzucamy H₀. Gdy
      \\(p \\geq \\alpha\\), nie mamy podstaw do odrzucenia H₀, co nie znaczy,
      że średnia w populacji wynosi dokładnie \\(\\mu_0\\)."),

    lc_p("Scenariusz koncentracji dobrze pokazuje, co znaczy to ostatnie
      zastrzeżenie. Dane losowane są z populacji o średniej 72 pkt
      i odchyleniu standardowym 13 pkt, więc H₀: μ = 70 jest fałszywa. Przy
      n = 40 błąd standardowy wynosi około \\(13/\\sqrt{40} \\approx 2.06\\)
      pkt, a różnica 2 pkt to średnio mniej niż jeden błąd standardowy.
      ", gloss("wartość krytyczna", "Wartość krytyczna"), " dla df = 39 wynosi 2.02, więc test odrzuca H₀ tylko
      w około 16% prób. W pozostałych popełnia błąd II rodzaju. Przy n = 100
      moc rośnie do około 33%. W scenariuszu hałasu prawdziwa średnia
      (87.5 dB) leży ponad pół odchylenia standardowego (4 dB) od normy
      i już przy n = 40 test odrzuca H₀ w około 97% prób."),

    lc_p("Test t i przedział ufności dla średniej z wykładu 04 są zbudowane
      z tych samych elementów. Przedział to \\(\\bar{x} \\pm t^* \\cdot SE\\),
      a test odrzuca H₀, gdy \\(|t| \\geq t^*\\), czyli gdy \\(\\mu_0\\) leży
      dalej od \\(\\bar{x}\\) niż \\(t^* \\cdot SE\\). To dwa zapisy tego
      samego warunku:"),

    # Równoważność testu i przedziału ze znacznikami
    lc_formula_annotated(
      "⟦1|\\left|t\\right|⟧ \\geq ⟦2|t^*_{\\alpha/2,\\, n-1}⟧ \\iff ⟦3|\\mu_0⟧ \\notin ⟦4|\\bar{x} \\pm t^*_{\\alpha/2,\\, n-1} \\cdot \\frac{s}{\\sqrt{n}}⟧",
      notes  = list("statystyka z próby", "wartość krytyczna", "wartość referencyjna", "przedział ufności"),
      colors = c("indygo", "terakota", "param", "accent"),
      above  = c(1, 2)
    ),

    lc_p("Test dwustronny na poziomie \\(\\alpha\\) odrzuca H₀: μ = μ₀
      dokładnie wtedy, gdy ", gloss("przedział ufności"), " na poziomie
      \\(1 - \\alpha\\) nie obejmuje \\(\\mu_0\\). Przy α = 0.05 odpowiada mu
      przedział 95%. Przedział mówi przy tym więcej niż sam werdykt: pokazuje
      wszystkie wartości \\(\\mu_0\\), których test by nie odrzucił, a więc
      także to, jak duża może być różnica."),

    lc_p("Test opiera się na założeniach: obserwacje są niezależne, a średnia
      z próby ma rozkład zbliżony do normalnego. Drugie założenie spełniają
      dane bez silnej skośności albo odpowiednio duża próba; im bardziej
      skośny rozkład, tym większej próby potrzeba. Jak to sprawdzać, omawia
      wykład 06."),

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
      lc_sf_figure("s05_t1b", bare = TRUE, config = ch2_t1_config, body = tagList(
        ch2_hypothesis_panel(one_sided = TRUE),
        lc_step_widget("ch2b_test",
          steps = c("Dane", "Statystyki opisowe", "Statystyka testowa",
                    "p-wartość i decyzja"),
          toolbar = lc_toolbar(
            lc_caption("Dane: te same co w teście dwustronnym powyżej."),
            lc_chart_zoom_button()
          ),
          body = lc_sf_chart(0, "2.5/1", "380px")
        )
      ))
    ),

    lc_p("Średnia, odchylenie standardowe i statystyka t są takie same jak
      w teście dwustronnym, bo dane się nie zmieniły. Zmienia się obszar
      odrzucenia: całe α = 0.05 leży w jednym ogonie, więc wartość krytyczna
      przy n = 40 (df = 39) spada z 2.02 do 1.68. Zmienia się też p-wartość. Jeśli t
      leży po stronie wskazanej przez Hₐ, p-wartość jednostronna jest
      dokładnie połową dwustronnej. W scenariuszu zużycia wody (prawdziwa
      średnia 158 l przy normie 150 l) test dwustronny przy n = 40 odrzuca
      H₀ w około 51% prób, a prawostronny w około 63%."),

    lc_p("Ta przewaga działa tylko w jednym kierunku. W scenariuszu koncentracji
      pytanie kierunkowe brzmi „Czy studenci mają niższą koncentrację niż
      norma 70 pkt?”,
      a dane pochodzą z populacji o średniej 72 pkt. Średnia z próby zwykle
      wypada więc powyżej 70, t jest dodatnie, a p-wartość lewostronna
      przekracza 0.5. Test odrzuca H₀ w mniej niż 1% prób, choć średnia
      naprawdę różni się od normy. ", gloss("test jednostronny", "Test jednostronny"), " nie widzi odchylenia
      w przeciwną stronę, niezależnie od jego wielkości."),

    lc_note("Zasada", rule = TRUE,
      "Kierunek testu ustal przed zebraniem danych, na podstawie pytania.
       Wybór strony po obejrzeniu wyników podwaja rzeczywiste ryzyko błędu
       I rodzaju: przy α = 0.05 faktycznie wynosi ono 10%."
    ),

    # ========================================================================
    # Ćwiczenia — CASchools
    # ========================================================================
    lc_h2("ch2-cas", "Ćwiczenia", "CASchools — test t jednej próby"),

    lc_p("Na koniec dwa zadania na prawdziwych danych. Pierwsze wymaga testu
      dwustronnego, drugie jednostronnego. W obu najpierw zapisz H₀ i Hₐ,
      wykonaj test i sformułuj wniosek w języku pytania,
      a dopiero potem porównaj go z rozwiązaniem."),

    lc_note("Dane",
      p("420 okręgów szkolnych Kalifornii (1998–1999). Plik: ",
        tags$code("dane/caschools.csv"), "."),
      p("Zmienne w zadaniach: ", tags$code("read"),
        " (średni wynik z czytania, ok. 655 pkt), ",
        tags$code("income"), " (dochód okręgu, tys. USD).")
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 1 — Czy wyniki z czytania różnią się od normy 650 pkt?"),
      p("Departament edukacji podaje normę 650 pkt. Przetestuj, czy średni wynik ",
        tags$code("read"), " w okręgach Kalifornii istotnie różni się",
        " od 650. Sformułuj H₀ i Hₐ, wykonaj test t jednej próby (α = 0.05).
        Co należy zaraportować departamentowi?"),
      lc_more("Rozwiązanie", cas_ch2_sol1_ui())
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie 2 — Czy typowy dochód okręgu przekracza 15 tys. USD?"),
      p("Hipoteza dyrekcji: „Nasz stan to stan zamożnych” — typowy okręg ma dochód
        powyżej 15 tys. USD. Przetestuj jednostronnie (prawostronnie)",
        " zmienną ", tags$code("income"),
        ". Sformułuj H₀ i Hₐ dla hipotezy kierunkowej.
        Czy wynik jest istotny statystycznie? A praktycznie?"),
      lc_more("Rozwiązanie", cas_ch2_sol2_ui())
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
