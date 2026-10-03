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

    lc_table(
      data.frame(
        c1 = c(
          "Normalność",
          "Równe wariancje",
          "Równe wariancje",
          "Stała wariancja reszt",
          "Niezależność reszt",
          "Współliniowość"
        ),
        c2 = c(
          "Shapiro-Wilk",
          "Levene",
          "Bartlett",
          "Breusch-Pagan",
          "Durbin-Watson",
          "VIF (wskaźnik, nie test)"
        ),
        c3 = c(
          "Rozkład jest normalny",
          "Wariancje w grupach są równe",
          "Wariancje w grupach są równe",
          "Wariancja reszt stała",
          "Brak autokorelacji",
          "— im większy VIF, tym silniejsza współliniowość"
        )
      ),
      cols = list(
        lc_col("c1", "Założenie", "row"),
        lc_col("c2", "Test", "text"),
        lc_col("c3", "H₀ (w populacji)", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_p("Brak podstaw do odrzucenia H₀ w teście diagnostycznym nie potwierdza
      założenia: przy małej próbie test ma małą moc, a przy dużej wykrywa
      odchylenia bez praktycznego znaczenia."),

    # ========================================================================
    lc_h2("ch5-alternatywy", "Metoda → alternatywa"),

    lc_table(
      data.frame(
        c1 = c(
          "Test t jednej próby",
          "Test t Studenta",
          "Test t dla prób niezależnych",
          "Test t dla par",
          "ANOVA klasyczna",
          "ANOVA",
          "Pearson",
          "χ² (małe n)",
          "Regresja OLS"
        ),
        c2 = c(
          "Wilcoxon jednej próby — wymaga symetrii",
          "Test t Welcha — nierówne wariancje (wybór domyślny)",
          "Mann–Whitney: porównanie rang, nie średnich",
          "Wilcoxon dla par — wymaga symetrii różnic",
          "ANOVA Welcha + post hoc Games-Howella — nierówne wariancje",
          "Kruskal-Wallis + post hoc Dunna — porównanie rang, nie średnich",
          "Spearman",
          "Fisher (dokładny)",
          "Odporne SE / bootstrap / GLM"
        )
      ),
      cols = list(
        lc_col("c1", "Metoda", "row"),
        lc_col("c2", "→ Alternatywa", "text")
      ),
      prose = TRUE
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
