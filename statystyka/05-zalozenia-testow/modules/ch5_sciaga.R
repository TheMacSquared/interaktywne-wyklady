# ============================================================================
# CHAPTER 5: Sciaga - podsumowanie zalozen
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

    lc_feedback(type = "info",
      tags$strong("Krok 1:"), " Wybierz metodę na podstawie typu zmiennych i ", gloss("pytanie badawcze", "pytania badawczego"), ".",
      br(), br(),
      tags$strong("Krok 2:"), " Sprawdź założenia wizualnie (wykresy) i formalnie (testy).",
      br(), br(),
      tags$strong("Krok 3a:"), " Założenia spełnione → użyj metody parametrycznej.",
      br(),
      tags$strong("Krok 3b:"), " Założenia naruszone → użyj alternatywy.",
      br(), br(),
      tags$strong("Krok 4:"), " Raportuj wyniki z ", gloss("wielkość efektu", "wielkością efektu"), " i ", gloss("p-wartość", "p-wartością"), "."
    ),

    # ========================================================================
    lc_h2("ch5-testy", "Testy diagnostyczne — szybka referencja"),

    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Założenie"), tags$th("Test"), tags$th("H₀"))
      ),
      tags$tbody(
        tags$tr(
          tags$td("Normalność"),
          tags$td("Shapiro-Wilk"),
          tags$td("Dane są normalne")
        ),
        tags$tr(
          tags$td("Równe wariancje"),
          tags$td("Levene"),
          tags$td("Wariancje równe")
        ),
        tags$tr(
          tags$td("Równe wariancje"),
          tags$td("Bartlett"),
          tags$td("Wariancje równe")
        ),
        tags$tr(
          tags$td("Homoscedast. reszt"),
          tags$td("Breusch-Pagan"),
          tags$td("Wariancja reszt stała")
        ),
        tags$tr(
          tags$td("Niezależn. reszt"),
          tags$td("Durbin-Watson"),
          tags$td("Brak autokorelacji")
        ),
        tags$tr(
          tags$td("Współliniowość"),
          tags$td("VIF"),
          tags$td("VIF < 5 (niektórzy < 10)")
        )
      )
    ),

    # ========================================================================
    lc_h2("ch5-alternatywy", "Metoda → alternatywa (quick reference)"),

    tags$table(class = "lc-table lc-table-bordered",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Metoda parametryczna"), tags$th("→ Alternatywa nieparametryczna"))
      ),
      tags$tbody(
        tags$tr(tags$td("Test t jednej próby"), tags$td("Wilcoxon jednej próby — wymaga symetrii")),
        tags$tr(tags$td("Test t niezależny"), tags$td("Mann–Whitney: porównanie rang, nie średnich")),
        tags$tr(tags$td("Test t sparowany"), tags$td("Wilcoxon dla par — wymaga symetrii różnic")),
        tags$tr(tags$td("ANOVA"), tags$td("Kruskal-Wallis")),
        tags$tr(tags$td("Tukey HSD (post-hoc)"), tags$td("Test Dunna")),
        tags$tr(tags$td("Pearson"), tags$td("Spearman")),
        tags$tr(tags$td("χ² (małe n)"), tags$td("Fisher (dokładny)")),
        tags$tr(tags$td("Regresja OLS"), tags$td("Odporne SE / bootstrap / GLM"))
      )
    ),

    # ========================================================================
    lc_h2("ch5-rady", "Praktyczne rady"),

    lc_feedback(type = "ok",
      tags$ul(
        tags$li(tags$b("Wizualizacja > testy formalne."),
                " Wykresy dają intuicję, testy dają liczbę. Używaj obu."),
        tags$li(tags$b(gloss("test t Welcha", "Testy Welcha"), " jako wybór domyślny."),
                " Nie musisz sprawdzać równości wariancji przed testem t."),
        tags$li(tags$b("Duże n łagodzi naruszenia."),
                " Łagodna ", gloss("skośność"), " zwykle jest mniej groźna w większych próbach,
                  ale silne outliery i bardzo ciężkie ogony nadal wymagają uwagi."),
        tags$li(tags$b(gloss("test nieparametryczny", "Testy nieparametryczne"), " nie są \"gorsze\"."),
                " Są praktyczną alternatywą przy silnych naruszeniach lub danych quasi-ilościowych,
                  choć nie zawsze odpowiadają dokładnie na pytanie o średnią."),
        tags$li(tags$b("Raportuj zawsze wielkość efektu"),
                " — p-wartość nie mówi, jak duży jest efekt.")
      )
    )

  )
)

ch5_server <- function(input, output, session) {
}
