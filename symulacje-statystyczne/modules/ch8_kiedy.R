# ============================================================================
# CHAPTER 8: Kiedy stosowac?
# ============================================================================

ch8_ui <- tabPanel("8. Kiedy stosowa\u0107?",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Poznali\u015bmy pi\u0119\u0107 metod: Bootstrap, Jackknife, Permutacje, CV, Monte Carlo.
       Teraz: praktyczny przewodnik decyzyjny."
    ),

    div(class = "section-title", "Mapa decyzji"),

    div(class = "narrative",
      p("Poni\u017csza tabela podsumowuje, kiedy ka\u017cda metoda jest w\u0142a\u015bciwym wyb\u00f3rem.")
    ),

    tags$table(class = "decision-table",
      tags$thead(tags$tr(
        tags$th("Sytuacja"),
        tags$th("Metoda"),
        tags$th("Uwagi")
      )),
      tags$tbody(
        tags$tr(
          tags$td("CI dla \u015bredniej, du\u017ce n, dane normalne"),
          tags$td(tags$b("t-CI (klasyczny)")),
          tags$td("Bootstrap te\u017c dzia\u0142a, ale zbyteczny")
        ),
        tags$tr(
          tags$td("CI dla mediany / sko\u015bny rozk\u0142ad"),
          tags$td(tags$b("Bootstrap percentylowy")),
          tags$td("Nie istnieje prosty wz\u00f3r analityczny")
        ),
        tags$tr(
          tags$td("Estymacja obci\u0105\u017cenia statystyki"),
          tags$td(tags$b("Jackknife")),
          tags$td("Szybki, prostszy ni\u017c bootstrap dla obci\u0105\u017cenia")
        ),
        tags$tr(
          tags$td("Test r\u00f3\u017cnicy grup, brak normalno\u015bci"),
          tags$td(tags$b("Test permutacyjny")),
          tags$td("Testuje t\u0119 sam\u0105 H\u2080 co t-test, bez za\u0142o\u017ce\u0144")
        ),
        tags$tr(
          tags$td("Korelacja z obserwacjami odstaj\u0105cymi"),
          tags$td(tags$b("Test perm. korelacji")),
          tags$td("Pearson wymaga normalno\u015bci dwuwymiarowej")
        ),
        tags$tr(
          tags$td("Ocena jako\u015bci modelu predykcyjnego"),
          tags$td(tags$b("K-Fold CV")),
          tags$td("k=5 lub k=10 \u2014 dobry kompromis obci\u0105\u017cenie/wariancja")
        ),
        tags$tr(
          tags$td("Planowanie bada\u0144 (ile n?)"),
          tags$td(tags$b("MC symulacja mocy")),
          tags$td("Daje dok\u0142adny wynik bez tabel mocy")
        ),
        tags$tr(
          tags$td("Test gdy nie mo\u017cna przetasowa\u0107 etykiet"),
          tags$td(tags$b("MC pod H\u2080")),
          tags$td("Gdy H\u2080 okre\u015bla parametryczny rozk\u0142ad")
        )
      )
    ),

    # ========================================================================
    # Rozroznienie: Bootstrap vs Jackknife
    # ========================================================================
    div(class = "section-title",
        "Bootstrap vs Jackknife: kiedy co?"),

    div(class = "narrative",
      tags$ul(
        tags$li(tags$b("Jackknife"), " daje obci\u0105\u017cenie i SE, ale ", tags$b("nie daje CI"),
                ". To nie jest zast\u0119pstwo dla bootstrap CI."),
        tags$li(tags$b("Bootstrap"), " daje pe\u0142ne CI dla dowolnej statystyki.
                Dla obci\u0105\u017cenia i SE: r\u00f3wnie\u017c, ale jackknife jest szybszy."),
        tags$li("Jackknife ", tags$b("nie dzia\u0142a dobrze"), " dla mediany i kwantyli
                przy ma\u0142ym n \u2014 u\u017cyj bootstrap.")
      )
    ),

    div(class = "callout-warning",
      tags$strong("Wa\u017cne:"),
      " Mann-Whitney U testuje inne H\u2080 ni\u017c test t (przesuni\u0119cie rozk\u0142adu,
       nie r\u00f3\u017cnica \u015brednich). Bootstrap test r\u00f3\u017cnicy \u015brednich testuje
       dok\u0142adnie to samo H\u2080 co t-test, ale bez za\u0142o\u017cenia o normalno\u015bci."
    ),

    # ========================================================================
    # Rozroznienie: Permutacje vs MC pod H0
    # ========================================================================
    div(class = "section-title",
        "Test permutacyjny vs Monte Carlo pod H\u2080"),

    div(class = "narrative",
      p(tags$b("Test permutacyjny"), ": przetasowuje etykiety grup.
         Wymaga, \u017ceby H\u2080 m\u00f3wi\u0142a, \u017ce obserwacje s\u0105 wymienne mi\u0119dzy grupami
         (np. H\u2080: brak r\u00f3\u017cnicy mi\u0119dzy grupami)."),
      p(tags$b("MC pod H\u2080"), ": losuje z parametrycznego rozk\u0142adu.
         Gdy H\u2080 okre\u015bla konkretny rozk\u0142ad (np. H\u2080: \u03bc = 100),
         nie mo\u017cna \u201eprzetasowywa\u0107\u201f \u2014 trzeba symulowa\u0107.")
    ),

    # ========================================================================
    # Ograniczenia metod
    # ========================================================================
    div(class = "section-title", "Ograniczenia metod resamplingowych"),

    div(class = "callout-danger",
      tags$strong("Czego resampling nie naprawi:"),
      tags$ul(
        tags$li(tags$b("B\u0142\u0105d doboru pr\u00f3by (selection bias)"),
                ": bootstrap nie pomo\u017ce, je\u015bli pr\u00f3ba nie jest reprezentatywna"),
        tags$li(tags$b("Ma\u0142e n przy medianie (jackknife)"),
                ": jackknife mo\u017ce by\u0107 niestabilny"),
        tags$li(tags$b("CV przy zale\u017cnych obserwacjach"),
                ": CV wymaga niezale\u017cno\u015bci \u2014 nie stosuj dla danych szereg\u00f3w czasowych"),
        tags$li(tags$b("MC pod H\u2080"),
                ": wymaga prawid\u0142owego sformu\u0142owania rozk\u0142adu pod H\u2080")
      )
    ),

    # ========================================================================
    # WIDGET: Quiz scenariuszy
    # ========================================================================
    div(class = "section-title", "Quiz: kt\u00f3ra metoda?"),

    div(class = "widget-block",
      h4("Wybierz odpowiedni\u0105 metod\u0119 dla ka\u017cdego scenariusza:"),
      selectInput("ch8_scenario", "Scenariusz:",
        choices = list(
          "1. CI dla mediany czasu kielkowania (n=12, silna skosnosc)" = "s1",
          "2. Sredni wzrost studentow (n=200, normalny)" = "s2",
          "3. Czy nawoz A daje wyzsze plony niz B? (brak normalnosci)" = "s3",
          "4. Korelacja masa ciala - cisnienie (n=25, outliery)" = "s4",
          "5. Jaki model regresji najlepiej prognozuje plony?" = "s5",
          "6. Ile n potrzeba aby wykryc roznice 5kg? (planowanie)" = "s6",
          "7. Estymacja obci\u0105\u017cenia \u015bredniej (czy pr\u00f3bka jest reprezentatywna?)" = "s7",
          "8. Test czy proporcja wadliwych != 0.02 (Bernoulli)" = "s8"
        ),
        selected = "s1",
        width = "100%"
      ),
      br(),
      actionButton("ch8_show", "Poka\u017c odpowied\u017a",
                   class = "btn-outline-success"),
      br(), br(),
      uiOutput("ch8_scenario_answer")
    ),

    div(class = "chapter-transition",
      p("Dalej: \u015bci\u0105ga ze wszystkimi algorytmami"),
      actionButton("ch8_next",
                   "Dalej \u2192 9. \u015aci\u0105ga",
                   class = "btn-primary btn-lg")
    )

  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch8_server <- function(input, output, session) {

  ch8_answers <- list(
    s1 = list(
      cls   = "callout-success",
      title = "Zalecany: Bootstrap CI dla mediany",
      body  = "Nie istnieje prosty wz\u00f3r analityczny na CI dla mediany.
               Bootstrap dzia\u0142a dla ka\u017cdej statystyki.
               Jackknife da SE, ale nie pe\u0142ny CI."
    ),
    s2 = list(
      cls   = "callout-info",
      title = "Wystarczy: klasyczny t-CI",
      body  = "Du\u017ce n, dane normalne \u2014 CTG zapewnia dok\u0142adno\u015b\u0107 t-CI.
               Bootstrap r\u00f3wnie\u017c zadziala, ale jest zbyteczny."
    ),
    s3 = list(
      cls   = "callout-success",
      title = "Zalecany: test permutacyjny (lub bootstrap CI r\u00f3\u017cnicy)",
      body  = "Brak normalno\u015bci: test permutacyjny testuje dok\u0142adnie t\u0119 sam\u0105 H\u2080
               co t-test (r\u00f3\u017cnica \u015brednich), ale bez za\u0142o\u017ce\u0144.
               Mann-Whitney U by\u0142by alternatyw\u0105, ale testuje inne H\u2080."
    ),
    s4 = list(
      cls   = "callout-success",
      title = "Zalecany: Bootstrap CI dla r lub test permutacyjny korelacji",
      body  = "Outliery zaburzaj\u0105 Pearsona. Bootstrap/permutacja s\u0105 odporne."
    ),
    s5 = list(
      cls   = "callout-success",
      title = "Zalecany: K-Fold CV",
      body  = "CV mierzy b\u0142\u0105d predykcji out-of-sample.
               MSE treningowy zawsze preferuje bardziej z\u0142o\u017cony model \u2014
               CV wybiera optymalny."
    ),
    s6 = list(
      cls   = "callout-success",
      title = "Zalecany: MC symulacja mocy",
      body  = "Podajesz \u03b4 = 5 kg i \u03c3 szacowane ze wst\u0119pnych danych.
               MC daje dok\u0142adny wynik n dla osi\u0105gni\u0119cia mocy 80%."
    ),
    s7 = list(
      cls   = "callout-info",
      title = "Jackknife",
      body  = "Jackknife oblicza obci\u0105\u017cenie statystyki \u2014 odchylenie estymaty
               od warto\u015bci oczekiwanej. Szybszy i prostszy ni\u017c bootstrap dla tego celu."
    ),
    s8 = list(
      cls   = "callout-info",
      title = "MC pod H\u2080 lub klasyczny test z (proporcja)",
      body  = "H\u2080: p = 0.02 okre\u015bla rozk\u0142ad Bernoulli \u2014 mo\u017cna zasymulowa\u0107 MC.
               Test permutacyjny nie ma tu zastosowania (brak etykiet grup do tasowania)."
    )
  )

  observeEvent(input$ch8_show, {
    scenario <- input$ch8_scenario
    ans      <- ch8_answers[[scenario]]
    output$ch8_scenario_answer <- renderUI({
      div(class = ans$cls,
        tags$strong(ans$title),
        p(ans$body)
      )
    })
  })

}
