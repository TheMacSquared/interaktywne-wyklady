# ============================================================================
# CHAPTER 8: Kiedy stosowac?
# ============================================================================

ch8_ui <- lecture_chapter(
  id = "ch-kiedy",
  num = "08",
  title = "Kiedy stosować?",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 08 · Symulacje statystyczne",
      num    = "08",
      title  = "Kiedy stosować?",
      lead   = "Dobieramy bootstrap, permutacje, jackknife, CV i Monte Carlo do pytania badawczego."
    ),

    lc_feedback(type = "info",
      "Poznaliśmy pięć metod: Bootstrap, Jackknife, Permutacje, CV, Monte Carlo.
       Teraz: praktyczny przewodnik decyzyjny."
    ),

    lc_h2("ch8-sec-01", "Mapa decyzji"),

    tagList(
      p("Poniższa tabela podsumowuje, kiedy każda metoda jest właściwym wybórem.")
    ),

    tags$table(class = "decision-table",
      tags$thead(tags$tr(
        tags$th("Sytuacja"),
        tags$th("Metoda"),
        tags$th("Uwagi")
      )),
      tags$tbody(
        tags$tr(
          tags$td("CI dla średniej, duże n, dane normalne"),
          tags$td(tags$b("t-CI (klasyczny)")),
          tags$td("Bootstrap też działa, ale zbyteczny")
        ),
        tags$tr(
          tags$td("CI dla mediany / skośny rozkład"),
          tags$td(tags$b("Bootstrap percentylowy")),
          tags$td("Nie istnieje prosty wzór analityczny")
        ),
        tags$tr(
          tags$td("Estymacja obciążenia statystyki"),
          tags$td(tags$b("Jackknife")),
          tags$td("Szybki, prostszy niż bootstrap dla obciążenia")
        ),
        tags$tr(
          tags$td("Test różnicy grup, brak normalności"),
          tags$td(tags$b("Test permutacyjny")),
          tags$td("Testuje tę samą H₀ co t-test, bez założeń")
        ),
        tags$tr(
          tags$td("Korelacja z obserwacjami odstającymi"),
          tags$td(tags$b("Test perm. korelacji")),
          tags$td("Pearson wymaga normalności dwuwymiarowej")
        ),
        tags$tr(
          tags$td("Ocena jakości modelu predykcyjnego"),
          tags$td(tags$b("K-Fold CV")),
          tags$td("k=5 lub k=10 — dobry kompromis obciążenie/wariancja")
        ),
        tags$tr(
          tags$td("Planowanie badań (ile n?)"),
          tags$td(tags$b("MC symulacja mocy")),
          tags$td("Daje dokładny wynik bez tabel mocy")
        ),
        tags$tr(
          tags$td("Test gdy nie można przetasować etykiet"),
          tags$td(tags$b("MC pod H₀")),
          tags$td("Gdy H₀ określa parametryczny rozkład")
        )
      )
    ),

    # ========================================================================
    # Rozroznienie: Bootstrap vs Jackknife
    # ========================================================================
    lc_h2("ch8-sec-02", "Bootstrap vs Jackknife: kiedy co?"),

    tagList(
      tags$ul(
        tags$li(tags$b("Jackknife"), " daje obciążenie i SE, ale ", tags$b("nie daje CI"),
                ". To nie jest zastępstwo dla bootstrap CI."),
        tags$li(tags$b("Bootstrap"), " daje pełne CI dla dowolnej statystyki.
                Dla obciążenia i SE: również, ale jackknife jest szybszy."),
        tags$li("Jackknife ", tags$b("nie działa dobrze"), " dla mediany i kwantyli
                przy małym n — użyj bootstrap.")
      )
    ),

    lc_feedback(type = "warning",
      tags$strong("Ważne:"),
      " Mann-Whitney U testuje inne H₀ niż test t (przesunięcie rozkładu,
       nie różnica średnich). Bootstrap test różnicy średnich testuje
       dokładnie to samo H₀ co t-test, ale bez założenia o normalności."
    ),

    # ========================================================================
    # Rozroznienie: Permutacje vs MC pod H0
    # ========================================================================
    lc_h2("ch8-sec-03", "Test permutacyjny vs Monte Carlo pod H₀"),

    tagList(
      p(tags$b("Test permutacyjny"), ": przetasowuje etykiety grup.
         Wymaga, żeby H₀ mówiła, że obserwacje są wymienne między grupami
         (np. H₀: brak różnicy między grupami)."),
      p(tags$b("MC pod H₀"), ": losuje z parametrycznego rozkładu.
         Gdy H₀ określa konkretny rozkład (np. H₀: μ = 100),
         nie można „przetasowywać‟ — trzeba symulować.")
    ),

    # ========================================================================
    # Ograniczenia metod
    # ========================================================================
    lc_h2("ch8-sec-04", "Ograniczenia metod resamplingowych"),

    lc_feedback(type = "danger",
      tags$strong("Czego resampling nie naprawi:"),
      tags$ul(
        tags$li(tags$b("Błąd doboru próby (selection bias)"),
                ": bootstrap nie pomoże, jeśli próba nie jest reprezentatywna"),
        tags$li(tags$b("Małe n przy medianie (jackknife)"),
                ": jackknife może być niestabilny"),
        tags$li(tags$b("CV przy zależnych obserwacjach"),
                ": CV wymaga niezależności — nie stosuj dla danych szeregów czasowych"),
        tags$li(tags$b("MC pod H₀"),
                ": wymaga prawidłowego sformułowania rozkładu pod H₀")
      )
    ),

    # ========================================================================
    # WIDGET: Quiz scenariuszy
    # ========================================================================
    lc_h2("ch8-sec-05", "Quiz: która metoda?"),

    figure_panel(label = "Ryc. 8.1", title = "Wybierz odpowiednią metodę dla każdego scenariusza:",
      selectInput("ch8_scenario", "Scenariusz:",
        choices = list(
          "1. CI dla mediany czasu kielkowania (n=12, silna skosnosc)" = "s1",
          "2. Sredni wzrost studentow (n=200, normalny)" = "s2",
          "3. Czy nawoz A daje wyzsze plony niz B? (brak normalnosci)" = "s3",
          "4. Korelacja masa ciala - cisnienie (n=25, outliery)" = "s4",
          "5. Jaki model regresji najlepiej prognozuje plony?" = "s5",
          "6. Ile n potrzeba aby wykryc roznice 5kg? (planowanie)" = "s6",
          "7. Estymacja obciążenia średniej (czy próbka jest reprezentatywna?)" = "s7",
          "8. Test czy proporcja wadliwych != 0.02 (Bernoulli)" = "s8"
        ),
        selected = "s1",
        width = "100%"
      ),
      br(),
      actionButton("ch8_show", "Pokaż odpowiedź",
                   class = "lc-btn-ok-outline"),
      br(), br(),
      uiOutput("ch8_scenario_answer")
    ),

    lc_chapter_next(
      num = "09",
      title = "Ściąga",
      lead = "algorytmy i najważniejsze reguły w jednym miejscu.",
      target_id = "ch-sciaga"
    )

  )
)
# ============================================================================
# SERVER
# ============================================================================

ch8_server <- function(input, output, session) {

  ch8_answers <- list(
    s1 = list(
      type  = "ok",
      title = "Zalecany: Bootstrap CI dla mediany",
      body  = "Nie istnieje prosty wzór analityczny na CI dla mediany.
               Bootstrap działa dla każdej statystyki.
               Jackknife da SE, ale nie pełny CI."
    ),
    s2 = list(
      type  = "info",
      title = "Wystarczy: klasyczny t-CI",
      body  = "Duże n, dane normalne — CTG zapewnia dokładność t-CI.
               Bootstrap również zadziala, ale jest zbyteczny."
    ),
    s3 = list(
      type  = "ok",
      title = "Zalecany: test permutacyjny (lub bootstrap CI różnicy)",
      body  = "Brak normalności: test permutacyjny testuje dokładnie tę samą H₀
               co t-test (różnica średnich), ale bez założeń.
               Mann-Whitney U byłby alternatywą, ale testuje inne H₀."
    ),
    s4 = list(
      type  = "ok",
      title = "Zalecany: Bootstrap CI dla r lub test permutacyjny korelacji",
      body  = "Outliery zaburzają Pearsona. Bootstrap/permutacja są odporne."
    ),
    s5 = list(
      type  = "ok",
      title = "Zalecany: K-Fold CV",
      body  = "CV mierzy błąd predykcji out-of-sample.
               MSE treningowy zawsze preferuje bardziej złożony model —
               CV wybiera optymalny."
    ),
    s6 = list(
      type  = "ok",
      title = "Zalecany: MC symulacja mocy",
      body  = "Podajesz δ = 5 kg i σ szacowane ze wstępnych danych.
               MC daje dokładny wynik n dla osiągnięcia mocy 80%."
    ),
    s7 = list(
      type  = "info",
      title = "Jackknife",
      body  = "Jackknife oblicza obciążenie statystyki — odchylenie estymaty
               od wartości oczekiwanej. Szybszy i prostszy niż bootstrap dla tego celu."
    ),
    s8 = list(
      type  = "info",
      title = "MC pod H₀ lub klasyczny test z (proporcja)",
      body  = "H₀: p = 0.02 określa rozkład Bernoulli — można zasymulować MC.
               Test permutacyjny nie ma tu zastosowania (brak etykiet grup do tasowania)."
    )
  )

  observeEvent(input$ch8_show, {
    scenario <- input$ch8_scenario
    ans      <- ch8_answers[[scenario]]
    output$ch8_scenario_answer <- renderUI({
      lc_feedback(type = ans$type,
        tags$strong(ans$title),
        p(ans$body)
      )
    })
  })

}
