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
      tags$strong("Krok 1:"), " Wybierz metodę na podstawie typu zmiennych i pytania badawczego.",
      br(), br(),
      tags$strong("Krok 2:"), " Sprawdź założenia wizualnie (wykresy) i formalnie (testy).",
      br(), br(),
      tags$strong("Krok 3a:"), " Założenia spełnione → użyj metody parametrycznej.",
      br(),
      tags$strong("Krok 3b:"), " Założenia naruszone → użyj alternatywy.",
      br(), br(),
      tags$strong("Krok 4:"), " Raportuj wyniki z wielkością efektu i p-wartością."
    ),

    # ========================================================================
    lc_h2("ch5-testy", "Testy diagnostyczne — szybka referencja"),

    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Założenie"), tags$th("Test"), tags$th("R"), tags$th("H₀"))
      ),
      tags$tbody(
        tags$tr(
          tags$td("Normalność"),
          tags$td("Shapiro-Wilk"),
          tags$td(tags$code("shapiro_test()")),
          tags$td("Dane są normalne")
        ),
        tags$tr(
          tags$td("Równe wariancje"),
          tags$td("Levene"),
          tags$td(tags$code("levene_test()")),
          tags$td("Wariancje równe")
        ),
        tags$tr(
          tags$td("Równe wariancje"),
          tags$td("Bartlett"),
          tags$td(tags$code("bartlett.test()")),
          tags$td("Wariancje równe")
        ),
        tags$tr(
          tags$td("Homoscedast. reszt"),
          tags$td("Breusch-Pagan"),
          tags$td(tags$code("lmtest::bptest()")),
          tags$td("Wariancja reszt stała")
        ),
        tags$tr(
          tags$td("Niezależn. reszt"),
          tags$td("Durbin-Watson"),
          tags$td(tags$code("lmtest::dwtest()")),
          tags$td("Brak autokorelacji")
        ),
        tags$tr(
          tags$td("Współliniowość"),
          tags$td("VIF"),
          tags$td(tags$code("car::vif()")),
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
        tags$tr(tags$td("Test t jednej próby"), tags$td("Wilcoxon jednej próby")),
        tags$tr(tags$td("Test t niezależny"), tags$td("Mann-Whitney U")),
        tags$tr(tags$td("Test t sparowany"), tags$td("Wilcoxon par znakowych")),
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
        tags$li(tags$b("Testy Welcha są domyślne w R."),
                " Nie musisz sprawdzać równości wariancji przed testem t."),
        tags$li(tags$b("Duże n łagodzi naruszenia."),
                " Łagodna skośność zwykle jest mniej groźna w większych próbach,
                  ale silne outliery i bardzo ciężkie ogony nadal wymagają uwagi."),
        tags$li(tags$b("Testy nieparametryczne nie są \"gorsze\"."),
                " Są praktyczną alternatywą przy silnych naruszeniach lub danych quasi-ilościowych,
                  choć nie zawsze odpowiadają dokładnie na pytanie o średnią."),
        tags$li(tags$b("Raportuj zawsze wielkość efektu"),
                " — p-wartość nie mówi, jak duży jest efekt.")
      )
    ),

    # ========================================================================
    lc_h2("ch5-kod", "Kod R — diagnostyka"),

    lc_formula_box(
      tags$pre(style = "background: var(--upwr-surface-sunken); padding: 12px; border-radius: 6px; font-size: 13px;",
        tags$code(
"library(rstatix)
library(lmtest)

# === Normalnosc ===
data %>% shapiro_test(variable)               # per zmienna
data %>% group_by(group) %>% shapiro_test(var) # per grupa

# === Rowne wariancje ===
data %>% levene_test(var ~ group)
bartlett.test(var ~ group, data = data)

# === Diagnostyka regresji ===
model <- lm(y ~ x1 + x2, data = dane)
plot(model)                    # 4 wykresy diagnostyczne
lmtest::bptest(model)          # Breusch-Pagan
lmtest::dwtest(model)          # Durbin-Watson
car::vif(model)                # VIF (wspolliniowosc)

# === Odporne bledy standardowe ===
library(sandwich)
lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = 'HC3'))

# === Welch ANOVA (nierowne wariancje) ===
oneway.test(var ~ group, data = data)"
        )
      )
    )

  )
)

ch5_server <- function(input, output, session) {
}
