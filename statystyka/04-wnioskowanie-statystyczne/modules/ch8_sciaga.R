# ============================================================================
# CHAPTER 8: Sciaga - podsumowanie wnioskowania statystycznego
# ============================================================================

ch8_ui <- list(
  id = "ch-sciaga", num = "12", title = "Ściąga",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 12 · Testowanie hipotez",
      num    = "12",
      title  = "Ściąga.",
      lead   = "Kompaktowe podsumowanie wszystkich testów omówionych w wykładzie —
                tabele referencyjne do trzymania pod ręką podczas analiz w Jamovi."
    ),

    # ========================================================================
    lc_h2("ch8-drzewo", "Drzewo decyzyjne: jaki test?"),

    tagList(
      p(tags$strong("Krok 1:"), " Ile zmiennych?"),
      tags$ul(
        tags$li(tags$b("Jedna zmienna"), " → Krok 2a"),
        tags$li(tags$b("Dwie zmienne"), " → Krok 2b")
      ),
      p(tags$strong("Krok 2a:"), " Jedna zmienna — jaki typ?"),
      tags$ul(
        tags$li(tags$b("Ilościowa"), " → test t jednej próby"),
        tags$li(tags$b("Jakościowa (2 kat.)"), " → test dwumianowy"),
        tags$li(tags$b("Jakościowa (3+ kat.)"), " → χ² zgodności")
      ),
      p(tags$strong("Krok 2b:"), " Dwie zmienne — jakie typy?"),
      tags$ul(
        tags$li(tags$b("Ilościowa + ilościowa"), " → Pearson / Spearman"),
        tags$li(tags$b("Jakościowa + jakościowa"), " → χ² niezależności / Fisher"),
        tags$li(tags$b("Ilościowa + jakościowa (2 grupy)"), " → Krok 3"),
        tags$li(tags$b("Ilościowa + jakościowa (3+ grup)"), " → ANOVA + post-hoc ", gloss("test Games-Howella", "Games-Howell"))
      ),
      p(tags$strong("Krok 3:"), " Próby niezależne czy sparowane?"),
      tags$ul(
        tags$li(tags$b("Niezależne"), " → test t niezależny"),
        tags$li(tags$b("Sparowane"), " → ", gloss("test t dla prób zależnych", "test t dla danych sparowanych"))
      )
    ),

    # ========================================================================
    lc_h2("ch8-tabela", "Tabela testów"),

    lc_table(
      data.frame(
        c1 = c(
          "1 ilościowa wobec μ₀",
          "1 jakościowa (2 kat.)",
          "1 jakościowa (3+ kat.)",
          "2 ilościowe",
          "2 jakościowe",
          "2 grupy niezależne",
          "2 grupy sparowane",
          "3+ grupy",
          "Post-hoc (3+ grupy)"
        ),
        c2 = c(
          "Test t jednej próby",
          "Test dwumianowy",
          "χ² zgodności",
          "Pearson / Spearman",
          "χ² niezależności / Fisher",
          "Test t niezależny",
          "Test t dla danych sparowanych",
          "ANOVA",
          "Games-Howell"
        )
      ),
      cols = list(
        lc_col("c1", "Sytuacja", "row"),
        lc_col("c2", "Test", "text")
      ),
      prose = TRUE
    ),

    lc_note("Uwaga",
      "gdy dane mocno naruszają założenia ",
      gloss("test parametryczny", "testów parametrycznych"),
      " (skrajna skośność,
       małe n, dane porządkowe), stosuje się ",
      gloss("test nieparametryczny", "testy nieparametryczne"),
      " (Mann-Whitney, Wilcoxon,
       Kruskal-Wallis). Omówimy je w osobnym wykładzie."
    ),

    # ========================================================================
    lc_h2("ch8-jamovi", "Jamovi ↔ testy z tego wykładu"),

    tagList(
      p("Liczymy w ", tags$b("jamovi"),
        " — poniżej ścieżka w menu oraz to, co odczytać z wyniku.")
    ),

    lc_table(
      data.frame(
        c1 = c(
          "Test dwumianowy",
          "χ² zgodności",
          "χ² niezależności",
          "Fisher exact",
          "Pearson / Spearman",
          "Test t niezależny",
          "Test t dla danych sparowanych",
          "ANOVA (1-czynnikowa)",
          "Post-hoc: Games-Howell"
        ),
        c2 = I(list(
          "Jedna proporcja (np. odsetek złych partii) wobec wartości referencyjnej.",
          "Zgodność rozkładu 3+ kategorii z oczekiwaniami.",
          "Związek między dwiema zmiennymi jakościowymi.",
          "Jak χ², ale gdy oczekiwane liczebności są < 5.",
          "Siła liniowego / monotonicznego związku dwóch zmiennych ilościowych.",
          "Porównanie średnich w 2 niezależnych grupach.",
          "Porównanie: ta sama jednostka zmierzona dwukrotnie (przed/po).",
          "Porównanie średnich w 3+ niezależnych grupach.",
          tagList("Porównania par grup ", tags$em("po"), " istotnej ANOVA.")
        )),
        c3 = I(list(
          tags$code("Frequencies → 2 Outcomes Binomial test"),
          tags$code("Frequencies → N Outcomes χ² test"),
          tags$code("Frequencies → Independent Samples χ²"),
          tagList(tags$code("Frequencies → Independent Samples χ²"), br(), "→ zaznacz ", tags$b("Fisher's exact test")),
          tagList(tags$code("Regression → Correlation Matrix"), br(), "zaznacz ", tags$b("Pearson"), " lub ", tags$b("Spearman")),
          tags$code("T-Tests → Independent Samples T-Test"),
          tags$code("T-Tests → Paired Samples T-Test"),
          tags$code("ANOVA → One-Way ANOVA"),
          tagList(tags$code("ANOVA → One-Way ANOVA"), br(), "→ sekcja ", tags$b("Post-Hoc Tests"), ", zaznacz ", tags$b("Games-Howell"))
        )),
        c4 = I(list(
          "p-wartość, proporcja, 95% CI",
          "χ², df, p",
          "χ², df, p, Cramér's V, reszty standaryzowane",
          "p (Fisher)",
          "r (lub ρ), p, 95% CI",
          "t, df, p, Mean difference, Cohen's d, 95% CI różnicy",
          "t, df, p, Cohen's d, średnia różnic",
          tagList("F, df₁/df₂, p, η² (w ", tags$em("Effect Size"), ")"),
          "Mean difference, p-tukey, 95% CI różnic parowych"
        ))
      ),
      cols = list(
        lc_col("c1", "Test", "row"),
        lc_col("c2", "Kiedy używać (1 zdanie)", "text"),
        lc_col("c3", "Ścieżka w jamovi", "text"),
        lc_col("c4", "Co odczytać z outputu", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_note("Zasada", rule = TRUE,
      "najpierw ANOVA. Jeśli istotna → post-hoc (Games-Howell).
       Jeśli nieistotna → w tym podstawowym, eksploracyjnym schemacie post-hoc pomijamy.",
      p(tags$em("Wyjątek na później: wcześniej zaplanowane porównanie dwóch konkretnych
        grup odpowiada na węższe pytanie niż ANOVA i może być analizowane osobno.
        Na tych zajęciach trzymamy się prostego schematu powyżej."))
    ),

    # ========================================================================
    lc_h2("ch8-efekt", "Miary wielkości efektu"),

    lc_table(
      data.frame(
        c1 = I(list(
          "Cohen's d",
          "r (korelacja)",
          "Cramér's V",
          withMathJax("\\(\\eta^2\\)")
        )),
        c2 = c("Test t (2 grupy)", "Pearson/Spearman", "χ² niezależności", "ANOVA"),
        c3 = c("0.2", "0.1", "0.1", "0.01"),
        c4 = c("0.5", "0.3", "0.3", "0.06"),
        c5 = c("0.8", "0.5", "0.5", "0.14"),
        c6 = c(
          "d = 0.2 ledwie uchwytne; d = 0.5 wykryje wyszkolony panel sensoryczny; d = 0.8 zauważy konsument w teście ślepym.",
          "|r| = 0.3 → związek widoczny na wykresie; |r| = 0.5 → wyraźny trend; |r| > 0.7 → bardzo silny.",
          "V = 0.1 odsetki w grupach różnią się o kilka punktów proc.; V = 0.5 różnice rzędu kilkudziesięciu pp.",
          "η² = 0.06 czynnik tłumaczy ~6% zmienności (reszta: inne przyczyny); η² = 0.14 to ~14% — czynnik dominujący."
        )
      ),
      cols = list(
        lc_col("c1", "Miara", "row"),
        lc_col("c2", "Test", "text"),
        lc_col("c3", "Mały", "num"),
        lc_col("c4", "Średni", "num"),
        lc_col("c5", "Duży", "num"),
        lc_col("c6", "Co to znaczy praktycznie?", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_note("Zasada", rule = TRUE,
      "progi Cohena to punkt wyjścia, nie wyrocznia. To, czy d = 0.3 jest \"małe\" czy \"ważne\", zależy od dziedziny.
       Dla bezpieczeństwa żywności (toksyny, patogeny) nawet mały efekt bywa krytyczny. Dla sensoryki — liczy się dopiero efekt średni."
    ),

    # ========================================================================
    lc_h2("ch8-pvalue", "P-wartość — przypomnienie"),

    lc_note("Definicja",
      p("Prawdopodobieństwo uzyskania wyniku co najmniej tak skrajnego,
        zakładając że H₀ jest prawdziwa.")
    ),

    lc_warn("Pułapka",
      tags$strong("P-wartość NIE jest:"),
      tags$ul(
        tags$li("Prawdopodobieństwem, że H₀ jest prawdziwa"),
        tags$li("Prawdopodobieństwem, że wynik jest przypadkowy"),
        tags$li("Miarą wielkości efektu (p = 0.001 ≠ duży efekt!)")
      )
    ),

    # ========================================================================
    lc_h2("ch8-pulapki", "Typowe pułapki"),

    lc_warn("Pułapki",
      tags$ul(
        tags$li(tags$b("P-hacking:"),
                " próbowanie testu aż wyjdzie p < 0.05 (parametryczny → nieparametryczny → usuwanie \"outlierów\" → zmiana hipotezy).
                 To nie jest analiza — to wyszukiwanie szumu. Analiza powinna być zaplanowana ", tags$em("przed"),
                " patrzeniem na wyniki."),
        tags$li(tags$b(gloss("porównania wielokrotne", "Wielokrotne porównania"), ":"),
                " testujesz 4 metody pasteryzacji mleka → masz 6 par. Bez korekcji ryzyko co najmniej jednego fałszywego alarmu rośnie do ~26% (zamiast 5%).
                 Dlatego po ANOVA stosuje się Games-Howell."),
        tags$li(tags$b("Brak istotności ≠ brak efektu:"),
                " często znaczy po prostu \"za mało danych, żeby to zobaczyć\".
                 Sprawdź wielkość efektu i szerokość przedziału ufności — jeśli CI jest bardzo szeroki, wynik jest niepewny."),
        tags$li(tags$b("Istotność statystyczna ≠ ", gloss("istotność praktyczna"), ":"),
                " przy n = 10 000 nawet różnica 0.01 pH może być istotna — ale technologicznie nic nie znaczy.
                 Zawsze raportuj p ", tags$b("i"), " wielkość efektu (d, η², V).")
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
