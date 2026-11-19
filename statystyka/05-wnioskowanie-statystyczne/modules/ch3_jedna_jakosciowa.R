# ============================================================================
# CHAPTER 5: Jedna zmienna jakościowa — test dwumianowy
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

# Treść stała (dawniej output$cas_ch3_sol_a na serwerze): składana przy budowaniu strony.
cas_ch3_sol_a_ui <- function() {
  r <- local({
    k <- sum(.ch3_cas$grades == "KK-06")
    n <- nrow(.ch3_cas)
    p_obs <- k / n
    bt <- binom.test(k, n, p = 0.5, alternative = "two.sided")
    list(k = k, n = n, p_obs = p_obs, p_val = bt$p.value,
         ci_lo = bt$conf.int[1], ci_hi = bt$conf.int[2])
  })
  tagList(
    p(tags$b("H₀:"), " p", tags$sub("KK06"), " = 0.5 · ", tags$b("Hₐ:"), " p", tags$sub("KK06"), " ≠ 0.5"),
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
}

# Treść stała (dawniej output$cas_ch3_sol_b na serwerze): składana przy budowaniu strony.
cas_ch3_sol_b_ui <- function() {
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
    p(tags$b("H₀:"), " p", tags$sub("ubóstwo"), " ≤ 0.30 · ",
      tags$b("Hₐ:"), " p", tags$sub("ubóstwo"), " > 0.30"),
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
}

# Scenariusze testu dwumianowego (Ryc. 5.1–5.3): parametry losowania i teksty hipotez.
ch3_scenario_params <- list(
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
    alt_1s = "less"),
  # Scenariusz kierunkowy (IDKiS): scena jest użyteczna, gdy chmury zasłaniają mniej niż 20% obszaru.
  clear_scenes = list(
    p0 = 0.60, p_true = 0.52, n_default = 50,
    success_label = "użyteczna", failure_label = "zachmurzona",
    title = "Zdjęcia satelitarne bez chmur",
    question = "Czy odsetek zdjęć satelitarnych nadających się do użytku różni się od deklarowanych 60%?",
    h0_text = "\\(H_0: p = 0.60\\) (odsetek zgodny z deklaracją)",
    h1_text = "\\(H_a: p \\neq 0.60\\) (odsetek odbiega od deklaracji)",
    question_1s = "Czy odsetek użytecznych zdjęć jest niższy niż deklarowane 60%?",
    h0_text_1s = "\\(H_0: p \\geq 0.60\\)",
    h1_text_1s = "\\(H_a: p < 0.60\\)",
    alt_1s = "less")
)

# Panel hipotezy nad krokami: wariant wybiera lista „Scenariusz”; podpowiedź widać,
# dopóki nie ma próby (element [data-when = "empty"], przełącza rycina).
ch3_hypothesis_panel <- function(one_sided = FALSE) {
  tagList(
    lc_variants("ch3_scenario", lapply(ch3_scenario_params, function(par) {
      tagList(
        lc_status(
          p(tags$b(if (one_sided) "Pytanie potoczne (kierunkowe):" else "Pytanie potoczne:")),
          p(tags$em(paste0("„", if (one_sided) par$question_1s else par$question, "”")))
        ),
        lc_formula_box(
          p(tags$b(if (one_sided) "Hipoteza formalna (jednostronna):" else "Hipoteza formalna (dwustronna):")),
          p(withMathJax(if (one_sided) par$h0_text_1s else par$h0_text)),
          p(withMathJax(if (one_sided) par$h1_text_1s else par$h1_text))
        )
      )
    })),
    tags$div(`data-when` = "empty", lc_empty(if (one_sided) {
      "Najpierw wylosuj próbę w teście dwustronnym powyżej"
    } else {
      "Kliknij „Losuj próbę”"
    }))
  )
}
# Dane dla rycin s05_b1, s05_b1b i s05_bcmp (modules/figures.js).
ch3_b1_config <- list(scenarios = lapply(ch3_scenario_params, `[`,
  c("p0", "p_true", "success_label", "failure_label", "alt_1s")))

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

    lc_p(gloss("test t", "Test t"), " z poprzedniego rozdziału sprawdzał hipotezę o średniej ", gloss("zmienna ilościowa", "zmiennej
      ilościowej"), ". Wiele pytań dotyczy jednak zmiennej o dwóch kategoriach:
      próbka wody spełnia normę albo nie, student zdał albo nie zdał, produkt
      jest wadliwy albo sprawny. Parametrem populacji jest wtedy proporcja
      \\(p\\), czyli odsetek sukcesów, a jej ", gloss("estymator", "estymatorem"), " ",
      gloss("proporcja z próby", "proporcja z próby"), " \\(\\hat{p} = k/n\\),
      gdzie \\(k\\) to liczba sukcesów w próbie liczącej \\(n\\) obserwacji.
      W wykładzie 04 budowaliśmy dla \\(p\\) ", gloss("przedział ufności"), ". Teraz pytanie
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
      liczba sukcesów \\(k\\). Z wykładu 03 wiemy, że jeśli każda z \\(n\\)
      niezależnych obserwacji jest sukcesem z prawdopodobieństwem \\(p_0\\),
      liczba sukcesów \\(K\\) ma ", gloss("rozkład dwumianowy"),
      " \\(B(n, p_0)\\) z wartością oczekiwaną \\(E(K) = np_0\\). Ten rozkład
      znamy dokładnie, więc prawdopodobieństwo każdego możliwego wyniku przy
      prawdziwej H₀ możemy po prostu policzyć."),

    # Rozkład dwumianowy pod H₀ ze znacznikami
    lc_formula_annotated(
      "P(⟦1|K⟧ = ⟦2|j⟧) = \\binom{⟦3|n⟧}{j}\\, ⟦4|p_0⟧^{\\,j} (1-p_0)^{n-j}, \\qquad j = 0, 1, \\ldots, n",
      notes  = list("liczba sukcesów", "możliwy wynik", "liczba obserwacji", "proporcja z H₀"),
      colors = c("indygo", "accent", "szalwia", "param"),
      above  = c(3, 4)
    ),

    lc_p(gloss("p-wartość", "P-wartość"), " to prawdopodobieństwo, że przy
      prawdziwej H₀ próba da wynik co najmniej tak skrajny jak obserwowany.
      W teście dwustronnym za co najmniej tak skrajne uznajemy wszystkie
      wyniki, które przy H₀ są nie bardziej prawdopodobne niż nasze \\(k\\),
      po obu stronach rozkładu. P-wartość jest sumą ich prawdopodobieństw."),

    # Dwustronna p-wartość testu dwumianowego ze znacznikami
    lc_formula_annotated(
      "⟦1|p⟧ = \\sum_{⟦2|j:\\ P(K = j)\\, \\leq\\, P(K = k)⟧} ⟦3|P(K = j)⟧",
      notes  = list("p-wartość", "wyniki nie bardziej prawdopodobne", "prawdopodobieństwo wyniku"),
      colors = c("bursztyn", "accent", "niebo"),
      above  = c(3)
    ),

    lc_p("Tak zbudowany ", gloss("test dwumianowy"), " jest testem dokładnym:
      p-wartość pochodzi wprost z rozkładu dwumianowego, a nie z przybliżenia
      normalnego. Dlatego działa także przy małych próbach i przy proporcjach
      bliskich 0 lub 1, czyli tam, gdzie w wykładzie 04 zawodził przedział
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
      # Losowanie, wykresy kroków i opisy liczy przeglądarka (modules/figures.js, rycina s05_b1).
      lc_sf_figure("s05_b1", bare = TRUE, config = ch3_b1_config, body = tagList(
        ch3_hypothesis_panel(),
        lc_step_widget("ch3_test",
          steps = c("Dane", "Rozkład pod H₀", "p-wartość i decyzja"),
          toolbar = lc_toolbar(
            lc_sf_ctl(selectInput("ch3_scenario", "Scenariusz", selectize = FALSE,
              choices = c(
                "Jakość wody (p₀ = 80%)" = "water_quality",
                "Zdawalność egzaminu (p₀ = 60%)" = "exam_pass",
                "Kiełkowalność nasion (p₀ = 90%)" = "germination",
                "Produkty poza normą (p₀ = 3%)" = "defects",
                "Używanie kasków na budowie (p₀ = 95%, IB)" = "helmets",
                "Zdjęcia satelitarne bez chmur (p₀ = 60%, IDKiS)" = "clear_scenes"
              ),
              selected = "water_quality"
            )),
            lc_sf_slider("ch3_n", "Wielkość próby (n)", 20, 200, 50, 10),
            lc_sf_action("new", "ch3_new_sample", "Losuj próbę", icon = "shuffle", variant = "solid"),
            lc_chart_zoom_button()
          ),
          body = lc_sf_chart(0, "2.5/1", "380px")
        )
      ))
    ),

    lc_p("Przy \\(n = 50\\) i \\(p_0 = 0.8\\) rozkład z kroku 2 ma środek
      w \\(np_0 = 40\\) sukcesach, a jego ", gloss("odchylenie standardowe"), " wynosi
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
      ufności 95% w wykładzie 04."),

    lc_p("Wynik testu warto zestawić z 95-procentowym ", gloss("przedział Cloppera-Pearsona", "przedziałem
      Cloppera-Pearsona"), " z wykładu 04, który dla tej próby wynosi od 0.73
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

    lc_p("Panel używa tej samej próby co ", gloss("test dwustronny"), " powyżej. Zmienia się
      tylko pytanie, a razem z nim zbiór wyników uznanych za skrajne."),

    figure_panel(
      label = "Ryc. 5.2",
      title = "Test dwumianowy jednostronny",
      lc_sf_figure("s05_b1b", bare = TRUE, config = ch3_b1_config, body = tagList(
        ch3_hypothesis_panel(one_sided = TRUE),
        lc_step_widget("ch3b_test",
          steps = c("Dane", "Rozkład pod H₀", "p-wartość i decyzja"),
          toolbar = lc_toolbar(
            lc_caption("Dane: te same co w teście dwustronnym powyżej."),
            lc_chart_zoom_button()
          ),
          body = lc_sf_chart(0, "2.5/1", "380px")
        )
      ))
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
      z tego samego przybliżenia, na którym w wykładzie 04 opierał się
      ", gloss("przedział Walda"), ". Statystyka testowa mierzy, o ile
      ", gloss("błąd standardowy", "błędów standardowych"), " \\(\\hat{p}\\) odbiega od \\(p_0\\)."),

    # Statystyka z testu proporcji ze znacznikami
    lc_formula_annotated(
      "⟦1|z⟧ = \\frac{⟦2|\\hat{p}⟧ - ⟦3|p_0⟧}{\\sqrt{p_0(1-p_0)/⟦4|n⟧}}",
      notes  = list("statystyka z", "proporcja z próby", "proporcja z H₀", "liczba obserwacji"),
      colors = c("indygo", "accent", "param", "szalwia"),
      above  = c(2, 3)
    ),

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
      # Oba testy liczy przeglądarka na próbie z Ryc. 5.1 (rycina s05_bcmp, bez wykresu).
      tags$div(class = "lc-sf", `data-fig` = "s05_bcmp", `data-lc-nochart` = NA,
        `data-config` = as.character(jsonlite::toJSON(ch3_b1_config, auto_unbox = TRUE)),
        lc_toolbar(lc_sf_action("cmp", "ch3_compare", "Porównaj testy", variant = "solid")),
        tags$div(`data-when` = "nodata", hidden = NA, lc_caption("Najpierw wylosuj próbę w widgecie powyżej.")),
        lapply(c("ok", "small"), function(key) tags$div(`data-when` = key, hidden = NA,
          lc_sf_fill_table(lc_table(
            data.frame(
              row = c("Dane", "Statystyka", "p-wartość", "Decyzja"),
              binom = c("@@cmp_b_data@@", "@@cmp_b_stat@@", "@@cmp_b_p@@", "@@cmp_b_dec@@"),
              prop = c("@@cmp_p_data@@", "@@cmp_p_stat@@", "@@cmp_p_p@@", "@@cmp_p_dec@@")
            ),
            cols = list(
              lc_col("row", "", "row"),
              lc_col("binom", "Test dwumianowy", "text"),
              lc_col("prop", "Test proporcji (z)", "text")
            )
          )),
          lc_status(
            p(lc_verdict(tags$b("Oczekiwane przy H₀:"), type = if (key == "ok") "ok" else "danger"), " ",
              tags$i("np"), tags$sub("0"), " = ", lc_sf_value("cmp_np"), " sukcesów i ",
              tags$i("n"), "(1 − ", tags$i("p"), tags$sub("0"), ") = ", lc_sf_value("cmp_nq"), " porażek",
              if (key == "ok") " — obie liczby są duże, przybliżenie normalne działa dobrze."
              else " — jedna z liczb jest mała, więc test proporcji może być niedokładny.")
          )
        ))
      )
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
      ", gloss("niezależność obserwacji"), ", omawiamy w wykładzie 06."),

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
      lc_more("Rozwiązanie", cas_ch3_sol_a_ui())
    ),

    figure_panel(label = "Ćwiczenie",
      h4("Zadanie B — Czy więcej niż 30% okręgów ma wysoki poziom ubóstwa?"),
      p("Przyjmij, że okręg ma wysoki poziom ubóstwa, gdy ", tags$code("lunch > 50"),
        ". Przetestuj jednostronnie (prawostronnie),
        czy odsetek takich okręgów przekracza normę 30%.
        Sformułuj H₀ i Hₐ, wykonaj test dwumianowy. Jaki wniosek?"),
      lc_more("Rozwiązanie", cas_ch3_sol_b_ui())
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
