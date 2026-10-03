# ============================================================================
# CHAPTER 5: Ściąga — podsumowanie założeń
# ============================================================================

ch5_ui <- lecture_chapter(
  id = "ch-sciaga",
  num = "05",
  title = "Ściąga",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 05 · Założenia testów",
      num    = "05",
      title  = "Ściąga.",
      lead   = "Kompaktowe podsumowanie: założenia, testy diagnostyczne i alternatywy."
    ),

    # ========================================================================
    lc_h2("ch5-schemat", "Schemat postępowania"),

    tagList(
      p(tags$strong("Krok 1:"), " Wybierz metodę na podstawie typu zmiennych i ",
        gloss("pytanie badawcze", "pytania badawczego"), "."),
      p(tags$strong("Krok 2:"),
        " Obejrzyj wykresy (histogram, Q-Q, wykres rozrzutu); test formalny traktuj pomocniczo."),
      p(tags$strong("Krok 3:"),
        " Oceń, czy naruszenie jest poważne dla tej metody przy tej wielkości próby. Jeśli tak, sięgnij po alternatywę; decyzję podejmij przed testem głównym, a nie po jego wyniku."),
      p(tags$strong("Krok 4:"), " Raportuj wyniki z ",
        gloss("wielkość efektu", "wielkością efektu"), " i ",
        gloss("p-wartość", "p-wartością"), ".")
    ),


    # ========================================================================
    lc_h2("ch5-testy", "Testy diagnostyczne — szybka referencja"),

    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Założenie"), tags$th("Test"), tags$th("H₀ (w populacji)"))
      ),
      tags$tbody(
        tags$tr(
          tags$td("Normalność"),
          tags$td("Shapiro-Wilk"),
          tags$td("Rozkład jest normalny")
        ),
        tags$tr(
          tags$td("Równe wariancje"),
          tags$td("Levene"),
          tags$td("Wariancje w grupach są równe")
        ),
        tags$tr(
          tags$td("Równe wariancje"),
          tags$td("Bartlett"),
          tags$td("Wariancje w grupach są równe")
        ),
        tags$tr(
          tags$td("Stała wariancja reszt"),
          tags$td("Breusch-Pagan"),
          tags$td("Wariancja reszt stała")
        ),
        tags$tr(
          tags$td("Niezależność reszt"),
          tags$td("Durbin-Watson"),
          tags$td("Brak autokorelacji")
        ),
        tags$tr(
          tags$td("Współliniowość"),
          tags$td("VIF (wskaźnik, nie test)"),
          tags$td("— im większy VIF, tym silniejsza współliniowość")
        )
      )
    ),

    lc_p("Brak podstaw do odrzucenia H₀ w teście diagnostycznym nie potwierdza
      założenia: przy małej próbie test ma małą moc, a przy dużej wykrywa
      odchylenia bez praktycznego znaczenia."),

    # ========================================================================
    lc_h2("ch5-alternatywy", "Metoda → alternatywa"),

    tags$table(class = "lc-table lc-table-bordered",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Metoda"), tags$th("→ Alternatywa"))
      ),
      tags$tbody(
        tags$tr(tags$td("Test t jednej próby"), tags$td("Wilcoxon jednej próby — wymaga symetrii")),
        tags$tr(tags$td("Test t Studenta"), tags$td("Test t Welcha — nierówne wariancje (wybór domyślny)")),
        tags$tr(tags$td("Test t dla prób niezależnych"), tags$td("Mann–Whitney: porównanie rang, nie średnich")),
        tags$tr(tags$td("Test t dla par"), tags$td("Wilcoxon dla par — wymaga symetrii różnic")),
        tags$tr(tags$td("ANOVA klasyczna"), tags$td("ANOVA Welcha + post hoc Games-Howella — nierówne wariancje")),
        tags$tr(tags$td("ANOVA"), tags$td("Kruskal-Wallis + post hoc Dunna — porównanie rang, nie średnich")),
        tags$tr(tags$td("Pearson"), tags$td("Spearman")),
        tags$tr(tags$td("χ² (małe n)"), tags$td("Fisher (dokładny)")),
        tags$tr(tags$td("Regresja OLS"), tags$td("Odporne SE / bootstrap / GLM"))
      )
    ),

    # ========================================================================
    lc_h2("ch5-rady", "Praktyczne rady"),

    tagList(
      tags$ul(
        tags$li(tags$b("Wykres:"),
                " najpierw histogram, Q-Q albo wykres rozrzutu; test formalny tylko pomocniczo."),
        tags$li(tags$b("Welch:"), " ", gloss("test t Welcha", "test t Welcha"),
                " i ANOVA Welcha jako wybór domyślny, bez wstępnego testu równości wariancji."),
        tags$li(tags$b("Wielkość próby:"),
                " łagodna ", gloss("skośność"), " zwykle mniej szkodzi w większych próbach,
                  ale silne wartości odstające i bardzo ciężkie ogony nadal wymagają uwagi."),
        tags$li(tags$b("Testy rangowe:"), " ", gloss("test nieparametryczny", "testy nieparametryczne"),
                " to pełnoprawna alternatywa przy silnych naruszeniach lub danych porządkowych,
                  ale odpowiadają na inne pytanie niż pytanie o średnią."),
        tags$li(tags$b("Raport:"),
                " zawsze podawaj wielkość efektu — p-wartość nie mówi, jak duży jest efekt.")
      )
    )

  )
)

ch5_server <- function(input, output, session) {
}
