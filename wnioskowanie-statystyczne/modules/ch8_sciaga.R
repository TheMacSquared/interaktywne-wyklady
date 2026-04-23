# ============================================================================
# CHAPTER 8: Sciaga - podsumowanie wnioskowania statystycznego
# ============================================================================

ch8_ui <- list(
  id = "ch-sciaga", num = "10", title = "Ściąga",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 10 · Testowanie hipotez",
      num    = "10",
      title  = "Ściąga.",
      lead   = "Kompaktowe podsumowanie wszystkich testów omówionych w wykładzie —
                tabele referencyjne do trzymania pod ręką podczas analiz w Jamovi."
    ),

    # ========================================================================
    h2(id = "ch8-drzewo", class = "section-title", "Drzewo decyzyjne: jaki test?"),

    div(class = "callout-info",
      tags$strong("Krok 1:"), " Ile zmiennych?",
      tags$ul(
        tags$li(tags$b("Jedna zmienna"), " → Krok 2a"),
        tags$li(tags$b("Dwie zmienne"), " → Krok 2b")
      ),

      tags$strong("Krok 2a:"), " Jedna zmienna — jaki typ?",
      tags$ul(
        tags$li(tags$b("Ilościowa"), " → test t jednej próby"),
        tags$li(tags$b("Jakościowa (2 kat.)"), " → test dwumianowy"),
        tags$li(tags$b("Jakościowa (3+ kat.)"), " → χ² zgodności")
      ),

      tags$strong("Krok 2b:"), " Dwie zmienne — jakie typy?",
      tags$ul(
        tags$li(tags$b("Ilościowa + ilościowa"), " → Pearson / Spearman"),
        tags$li(tags$b("Jakościowa + jakościowa"), " → χ² niezależności / Fisher"),
        tags$li(tags$b("Ilościowa + jakościowa (2 grupy)"), " → Krok 3"),
        tags$li(tags$b("Ilościowa + jakościowa (3+ grup)"), " → ANOVA + post-hoc Games-Howell")
      ),

      tags$strong("Krok 3:"), " Próby niezależne czy parowe?",
      tags$ul(
        tags$li(tags$b("Niezależne"), " → test t niezależny"),
        tags$li(tags$b("Parowe"), " → test t parowy")
      )
    ),

    # ========================================================================
    h2(id = "ch8-tabela", class = "section-title", "Tabela testów"),

    tags$table(class = "table table-bordered table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(
          tags$th("Sytuacja"),
          tags$th("Test"),
          tags$th("R (rstatix)")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("1 ilościowa wobec μ₀"),
          tags$td("Test t jednej próby"),
          tags$td(tags$code("t_test(x~1, mu=)"))
        ),
        tags$tr(
          tags$td("1 jakościowa (2 kat.)"),
          tags$td("Test dwumianowy"),
          tags$td(tags$code("binom.test()"))
        ),
        tags$tr(
          tags$td("1 jakościowa (3+ kat.)"),
          tags$td("χ² zgodności"),
          tags$td(tags$code("chisq.test()"))
        ),
        tags$tr(
          tags$td("2 ilościowe"),
          tags$td("Pearson / Spearman"),
          tags$td(tags$code("cor_test(method=)"))
        ),
        tags$tr(
          tags$td("2 jakościowe"),
          tags$td("χ² niezależności / Fisher"),
          tags$td(tags$code("chisq.test()"), br(), tags$code("fisher.test()"))
        ),
        tags$tr(
          tags$td("2 grupy niezależne"),
          tags$td("Test t niezależny"),
          tags$td(tags$code("t_test(y~gr)"))
        ),
        tags$tr(
          tags$td("2 grupy parowe"),
          tags$td("Test t parowy"),
          tags$td(tags$code("t_test(y~t, paired=T)"))
        ),
        tags$tr(
          tags$td("3+ grupy"),
          tags$td("ANOVA"),
          tags$td(tags$code("anova_test(y~gr)"))
        ),
        tags$tr(
          tags$td("Post-hoc (3+ grupy)"),
          tags$td("Games-Howell"),
          tags$td(tags$code("games_howell_test(y~gr)"))
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Uwaga: "),
      "gdy dane mocno naruszają założenia testów parametrycznych (skrajna skośność,
       małe n, dane porządkowe), stosuje się testy nieparametryczne (Mann-Whitney, Wilcoxon,
       Kruskal-Wallis). Omówimy je w osobnym wykładzie."
    ),

    # ========================================================================
    h2(id = "ch8-jamovi", class = "section-title", "Jamovi ↔ testy z tego wykładu"),

    div(class = "narrative",
      p("Liczymy w ", tags$b("jamovi"),
        " — poniżej ścieżka w menu oraz to, co odczytać z wyniku.")
    ),

    tags$table(class = "table table-bordered table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(
          tags$th("Test"),
          tags$th("Kiedy używać (1 zdanie)"),
          tags$th("Ścieżka w jamovi"),
          tags$th("Co odczytać z outputu")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$b("Test dwumianowy")),
          tags$td("Jedna proporcja (np. odsetek złych partii) wobec wartości referencyjnej."),
          tags$td(tags$code("Frequencies → 2 Outcomes Binomial test")),
          tags$td("p-wartość, proporcja, 95% CI")
        ),
        tags$tr(
          tags$td(tags$b("χ² zgodności")),
          tags$td("Zgodność rozkładu 3+ kategorii z oczekiwaniami."),
          tags$td(tags$code("Frequencies → N Outcomes χ² test")),
          tags$td("χ², df, p")
        ),
        tags$tr(
          tags$td(tags$b("χ² niezależności")),
          tags$td("Związek między dwiema zmiennymi jakościowymi."),
          tags$td(tags$code("Frequencies → Independent Samples χ²")),
          tags$td("χ², df, p, Cramér's V, reszty standaryzowane")
        ),
        tags$tr(
          tags$td(tags$b("Fisher exact")),
          tags$td("Jak χ², ale gdy oczekiwane liczebności są < 5."),
          tags$td(tags$code("Frequencies → Independent Samples χ²"), br(),
                  "→ zaznacz ", tags$b("Fisher's exact test")),
          tags$td("p (Fisher)")
        ),
        tags$tr(
          tags$td(tags$b("Pearson / Spearman")),
          tags$td("Siła liniowego / monotonicznego związku dwóch zmiennych ilościowych."),
          tags$td(tags$code("Regression → Correlation Matrix"), br(),
                  "zaznacz ", tags$b("Pearson"), " lub ", tags$b("Spearman")),
          tags$td("r (lub ρ), p, 95% CI")
        ),
        tags$tr(
          tags$td(tags$b("Test t niezależny")),
          tags$td("Porównanie średnich w 2 niezależnych grupach."),
          tags$td(tags$code("T-Tests → Independent Samples T-Test")),
          tags$td("t, df, p, Mean difference, Cohen's d, 95% CI różnicy")
        ),
        tags$tr(
          tags$td(tags$b("Test t parowy")),
          tags$td("Porównanie: ta sama jednostka zmierzona dwukrotnie (przed/po)."),
          tags$td(tags$code("T-Tests → Paired Samples T-Test")),
          tags$td("t, df, p, Cohen's d, średnia różnic")
        ),
        tags$tr(
          tags$td(tags$b("ANOVA (1-czynnikowa)")),
          tags$td("Porównanie średnich w 3+ niezależnych grupach."),
          tags$td(tags$code("ANOVA → One-Way ANOVA")),
          tags$td("F, df₁/df₂, p, η² (w ", tags$em("Effect Size"), ")")
        ),
        tags$tr(
          tags$td(tags$b("Post-hoc: Games-Howell")),
          tags$td("Porównania par grup ", tags$em("po"), " istotnej ANOVA."),
          tags$td(tags$code("ANOVA → One-Way ANOVA"), br(),
                  "→ sekcja ", tags$b("Post-Hoc Tests"), ", zaznacz ", tags$b("Games-Howell")),
          tags$td("Mean difference, p-tukey, 95% CI różnic parowych")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Zasada: "),
      "najpierw ANOVA. Jeśli istotna → post-hoc (Games-Howell).
       Jeśli nieistotna → post-hoc pomijamy."
    ),

    # ========================================================================
    h2(id = "ch8-efekt", class = "section-title", "Miary wielkości efektu"),

    tags$table(class = "table table-bordered",
      style = "font-size: 14px;",
      tags$thead(
        tags$tr(
          tags$th("Miara"), tags$th("Test"),
          tags$th("Mały"), tags$th("Średni"), tags$th("Duży"),
          tags$th("Co to znaczy praktycznie?")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("Cohen's d"), tags$td("Test t (2 grupy)"),
          tags$td("0.2"), tags$td("0.5"), tags$td("0.8"),
          tags$td("d = 0.2 ledwie uchwytne; d = 0.5 wykryje wyszkolony panel sensoryczny; d = 0.8 zauważy konsument w teście ślepym.")
        ),
        tags$tr(
          tags$td("r (korelacja)"), tags$td("Pearson/Spearman"),
          tags$td("0.1"), tags$td("0.3"), tags$td("0.5"),
          tags$td("|r| = 0.3 → związek widoczny na wykresie; |r| = 0.5 → wyraźny trend; |r| > 0.7 → bardzo silny.")
        ),
        tags$tr(
          tags$td("Cramér's V"), tags$td("χ² niezależności"),
          tags$td("0.1"), tags$td("0.3"), tags$td("0.5"),
          tags$td("V = 0.1 odsetki w grupach różnią się o kilka punktów proc.; V = 0.5 różnice rzędu kilkudziesięciu pp.")
        ),
        tags$tr(
          tags$td(withMathJax("\\(\\eta^2\\)")), tags$td("ANOVA"),
          tags$td("0.01"), tags$td("0.06"), tags$td("0.14"),
          tags$td("η² = 0.06 czynnik tłumaczy ~6% zmienności (reszta: inne przyczyny); η² = 0.14 to ~14% — czynnik dominujący.")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Reguła interpretacji: "),
      "progi Cohena to punkt wyjścia, nie wyrocznia. To, czy d = 0.3 jest \"małe\" czy \"ważne\", zależy od dziedziny.
       Dla bezpieczeństwa żywności (toksyny, patogeny) nawet mały efekt bywa krytyczny. Dla sensoryki — liczy się dopiero efekt średni."
    ),

    # ========================================================================
    h2(id = "ch8-pvalue", class = "section-title", "P-wartość — przypomnienie"),

    div(class = "callout-success",
      tags$strong("P-wartość to:"),
      p("Prawdopodobieństwo uzyskania wyniku co najmniej tak skrajnego,
        zakładając że H₀ jest prawdziwa.")
    ),

    div(class = "callout-danger",
      tags$strong("P-wartość NIE jest:"),
      tags$ul(
        tags$li("Prawdopodobieństwem, że H₀ jest prawdziwa"),
        tags$li("Prawdopodobieństwem, że wynik jest przypadkowy"),
        tags$li("Miarą wielkości efektu (p = 0.001 ≠ duży efekt!)")
      )
    ),

    # ========================================================================
    h2(id = "ch8-pulapki", class = "section-title", "Typowe pułapki"),

    div(class = "callout-danger",
      tags$ul(
        tags$li(tags$b("P-hacking:"),
                " próbowanie testu aż wyjdzie p < 0.05 (parametryczny → nieparametryczny → usuwanie \"outlierów\" → zmiana hipotezy).
                 To nie jest analiza — to wyszukiwanie szumu. Analiza powinna być zaplanowana ", tags$em("przed"),
                " patrzeniem na wyniki."),
        tags$li(tags$b("Wielokrotne porównania:"),
                " testujesz 4 metody pasteryzacji mleka → masz 6 par. Bez korekcji ryzyko co najmniej jednego fałszywego alarmu rośnie do ~26% (zamiast 5%).
                 Dlatego po ANOVA stosuje się Games-Howell."),
        tags$li(tags$b("Brak istotności ≠ brak efektu:"),
                " często znaczy po prostu \"za mało danych, żeby to zobaczyć\".
                 Sprawdź wielkość efektu i szerokość przedziału ufności — jeśli CI jest bardzo szeroki, wynik jest niepewny."),
        tags$li(tags$b("Istotność statystyczna ≠ istotność praktyczna:"),
                " przy n = 10 000 nawet różnica 0.01 pH może być istotna — ale technologicznie nic nie znaczy.
                 Zawsze raportuj p ", tags$b("i"), " wielkość efektu (d, η², V).")
      )
    ),

    # ========================================================================
    h2(id = "ch8-kod", class = "section-title", "Kod R — rstatix"),

    div(class = "formula-box",
      tags$pre(style = "background: var(--upwr-surface-sunken); padding: 12px; border-radius: 6px; font-size: 13px;",
        tags$code(
"library(rstatix)
library(broom)

# === Jedna zmienna ===
# Test t jednej proby
data %>% t_test(oceny ~ 1, mu = 3.5)

# === Korelacja ===
data %>% cor_test(wzrost, waga, method = \"pearson\")
data %>% cor_test(wzrost, waga, method = \"spearman\")

# === Dwie grupy ===
# Niezalezne
data %>% t_test(wzrost ~ plec)
data %>% cohens_d(wzrost ~ plec)

# Parowe
data %>% t_test(wynik ~ moment, paired = TRUE)

# === Chi-kwadrat ===
tab <- table(data$plec, data$kierunek)
chisq.test(tab)
fisher.test(tab)

# === ANOVA ===
data %>% anova_test(oceny ~ kierunek)
data %>% games_howell_test(oceny ~ kierunek)"
        )
      )
    )

  )
)

# ============================================================================
# SERVER (brak interaktywnych widgetow)
# ============================================================================

ch8_server <- function(input, output, session) {
  # Sciaga nie wymaga logiki server
}
