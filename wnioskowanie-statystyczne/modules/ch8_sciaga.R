# ============================================================================
# CHAPTER 8: Sciaga - podsumowanie wnioskowania statystycznego
# ============================================================================

ch8_ui <- tabPanel("9. \u015aci\u0105ga",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Kompaktowe podsumowanie wszystkich test\u00f3w z tego wyk\u0142adu."
    ),

    # ========================================================================
    div(class = "section-title", "Drzewo decyzyjne: jaki test?"),

    div(class = "callout-info",
      tags$strong("Krok 1:"), " Ile zmiennych?",
      tags$ul(
        tags$li(tags$b("Jedna zmienna"), " \u2192 Krok 2a"),
        tags$li(tags$b("Dwie zmienne"), " \u2192 Krok 2b")
      ),

      tags$strong("Krok 2a:"), " Jedna zmienna \u2014 jaki typ?",
      tags$ul(
        tags$li(tags$b("Ilo\u015bciowa"), " \u2192 test t jednej pr\u00f3by / Wilcoxon"),
        tags$li(tags$b("Jako\u015bciowa (2 kat.)"), " \u2192 test dwumianowy"),
        tags$li(tags$b("Jako\u015bciowa (3+ kat.)"), " \u2192 \u03c7\u00b2 zgodno\u015bci")
      ),

      tags$strong("Krok 2b:"), " Dwie zmienne \u2014 jakie typy?",
      tags$ul(
        tags$li(tags$b("Ilo\u015bciowa + ilo\u015bciowa"), " \u2192 Pearson / Spearman"),
        tags$li(tags$b("Jako\u015bciowa + jako\u015bciowa"), " \u2192 \u03c7\u00b2 niezale\u017cno\u015bci / Fisher"),
        tags$li(tags$b("Ilo\u015bciowa + jako\u015bciowa (2 grupy)"), " \u2192 Krok 3"),
        tags$li(tags$b("Ilo\u015bciowa + jako\u015bciowa (3+ grup)"), " \u2192 ANOVA / Kruskal-Wallis")
      ),

      tags$strong("Krok 3:"), " Pr\u00f3by niezale\u017cne czy parowe?",
      tags$ul(
        tags$li(tags$b("Niezale\u017cne"), " \u2192 test t / Mann-Whitney"),
        tags$li(tags$b("Parowe"), " \u2192 test t parowy / Wilcoxon par znakowych")
      )
    ),

    # ========================================================================
    div(class = "section-title", "Tabela test\u00f3w"),

    tags$table(class = "table table-bordered table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(
          tags$th("Sytuacja"),
          tags$th("Parametryczny"),
          tags$th("Nieparametryczny"),
          tags$th("R (rstatix)")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("1 ilo\u015bciowa wobec \u03bc\u2080"),
          tags$td("Test t jednej pr\u00f3by"),
          tags$td("Wilcoxon jednej pr\u00f3by"),
          tags$td(tags$code("t_test(x~1, mu=)"), br(), tags$code("wilcox_test(x~1, mu=)"))
        ),
        tags$tr(
          tags$td("1 jako\u015bciowa (2 kat.)"),
          tags$td(em("\u2014")),
          tags$td("Test dwumianowy"),
          tags$td(tags$code("binom.test()"))
        ),
        tags$tr(
          tags$td("1 jako\u015bciowa (3+ kat.)"),
          tags$td(em("\u2014")),
          tags$td("\u03c7\u00b2 zgodno\u015bci"),
          tags$td(tags$code("chisq.test()"))
        ),
        tags$tr(
          tags$td("2 ilo\u015bciowe"),
          tags$td("Pearson"),
          tags$td("Spearman"),
          tags$td(tags$code("cor_test(method=)"))
        ),
        tags$tr(
          tags$td("2 jako\u015bciowe"),
          tags$td(em("\u2014")),
          tags$td("\u03c7\u00b2 niezale\u017cn. / Fisher"),
          tags$td(tags$code("chisq.test()"), br(), tags$code("fisher.test()"))
        ),
        tags$tr(
          tags$td("2 grupy niezale\u017cne"),
          tags$td("Test t niezale\u017cny"),
          tags$td("Mann-Whitney U"),
          tags$td(tags$code("t_test(y~gr)"), br(), tags$code("wilcox_test(y~gr)"))
        ),
        tags$tr(
          tags$td("2 grupy parowe"),
          tags$td("Test t parowy"),
          tags$td("Wilcoxon par znakowych"),
          tags$td(tags$code("t_test(y~t, paired=T)"), br(), tags$code("wilcox_test(y~t, paired=T)"))
        ),
        tags$tr(
          tags$td("3+ grupy"),
          tags$td("ANOVA"),
          tags$td("Kruskal-Wallis"),
          tags$td(tags$code("anova_test(y~gr)"), br(), tags$code("kruskal_test(y~gr)"))
        ),
        tags$tr(
          tags$td("Post-hoc (3+ grupy)"),
          tags$td("Tukey HSD"),
          tags$td("Test Dunna"),
          tags$td(tags$code("tukey_hsd(y~gr)"), br(), tags$code("dunn_test(y~gr)"))
        )
      )
    ),

    # ========================================================================
    div(class = "section-title", "Jamovi \u2194 testy z tego wyk\u0142adu"),

    div(class = "narrative",
      p("Liczymy w ", tags$b("jamovi"),
        " \u2014 poni\u017cej \u015bcie\u017cka w menu oraz to, co odczyta\u0107 z wyniku.")
    ),

    tags$table(class = "table table-bordered table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(
          tags$th("Test"),
          tags$th("Kiedy u\u017cywa\u0107 (1 zdanie)"),
          tags$th("\u015acie\u017cka w jamovi"),
          tags$th("Co odczyta\u0107 z outputu")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$b("Test dwumianowy")),
          tags$td("Jedna proporcja (np. odsetek z\u0142ych partii) wobec warto\u015bci referencyjnej."),
          tags$td(tags$code("Frequencies \u2192 2 Outcomes Binomial test")),
          tags$td("p-warto\u015b\u0107, proporcja, 95% CI")
        ),
        tags$tr(
          tags$td(tags$b("\u03c7\u00b2 zgodno\u015bci")),
          tags$td("Zgodno\u015b\u0107 rozk\u0142adu 3+ kategorii z oczekiwaniami."),
          tags$td(tags$code("Frequencies \u2192 N Outcomes \u03c7\u00b2 test")),
          tags$td("\u03c7\u00b2, df, p")
        ),
        tags$tr(
          tags$td(tags$b("\u03c7\u00b2 niezale\u017cno\u015bci")),
          tags$td("Zwi\u0105zek mi\u0119dzy dwiema zmiennymi jako\u015bciowymi."),
          tags$td(tags$code("Frequencies \u2192 Independent Samples \u03c7\u00b2")),
          tags$td("\u03c7\u00b2, df, p, Cram\u00e9r's V, reszty standaryzowane")
        ),
        tags$tr(
          tags$td(tags$b("Fisher exact")),
          tags$td("Jak \u03c7\u00b2, ale gdy oczekiwane liczebno\u015bci s\u0105 < 5."),
          tags$td(tags$code("Frequencies \u2192 Independent Samples \u03c7\u00b2"), br(),
                  "\u2192 zaznacz ", tags$b("Fisher's exact test")),
          tags$td("p (Fisher)")
        ),
        tags$tr(
          tags$td(tags$b("Pearson / Spearman")),
          tags$td("Si\u0142a liniowego / monotonicznego zwi\u0105zku dw\u00f3ch zmiennych ilo\u015bciowych."),
          tags$td(tags$code("Regression \u2192 Correlation Matrix"), br(),
                  "zaznacz ", tags$b("Pearson"), " lub ", tags$b("Spearman")),
          tags$td("r (lub \u03c1), p, 95% CI")
        ),
        tags$tr(
          tags$td(tags$b("Test t niezale\u017cny")),
          tags$td("Por\u00f3wnanie \u015brednich w 2 niezale\u017cnych grupach."),
          tags$td(tags$code("T-Tests \u2192 Independent Samples T-Test")),
          tags$td("t, df, p, Mean difference, Cohen's d, 95% CI r\u00f3\u017cnicy")
        ),
        tags$tr(
          tags$td(tags$b("Mann-Whitney U")),
          tags$td("Jak test t niezale\u017cny, ale gdy dane sko\u015bne / odstaj\u0105ce."),
          tags$td(tags$code("T-Tests \u2192 Independent Samples T-Test"), br(),
                  "\u2192 zaznacz ", tags$b("Mann-Whitney U")),
          tags$td("U, p, rank-biserial r (efekt)")
        ),
        tags$tr(
          tags$td(tags$b("Test t parowy")),
          tags$td("Por\u00f3wnanie: ta sama jednostka zmierzona dwukrotnie (przed/po)."),
          tags$td(tags$code("T-Tests \u2192 Paired Samples T-Test")),
          tags$td("t, df, p, Cohen's d, \u015brednia r\u00f3\u017cnic")
        ),
        tags$tr(
          tags$td(tags$b("Wilcoxon par znakowych")),
          tags$td("Jak test t parowy, ale dla danych sko\u015bnych / ma\u0142ych n."),
          tags$td(tags$code("T-Tests \u2192 Paired Samples T-Test"), br(),
                  "\u2192 zaznacz ", tags$b("Wilcoxon rank")),
          tags$td("W, p, rank-biserial r")
        ),
        tags$tr(
          tags$td(tags$b("ANOVA (1-czynnikowa)")),
          tags$td("Por\u00f3wnanie \u015brednich w 3+ niezale\u017cnych grupach."),
          tags$td(tags$code("ANOVA \u2192 One-Way ANOVA")),
          tags$td("F, df\u2081/df\u2082, p, \u03b7\u00b2 (w ", tags$em("Effect Size"), ")")
        ),
        tags$tr(
          tags$td(tags$b("Post-hoc: Tukey HSD")),
          tags$td("Por\u00f3wnania par grup ", tags$em("po"), " istotnej ANOVA."),
          tags$td(tags$code("ANOVA \u2192 One-Way ANOVA"), br(),
                  "\u2192 sekcja ", tags$b("Post-Hoc Tests"), ", zaznacz ", tags$b("Tukey")),
          tags$td("Mean difference, p-tukey, 95% CI r\u00f3\u017cnic parowych")
        ),
        tags$tr(
          tags$td(tags$b("Kruskal-Wallis")),
          tags$td("Jak ANOVA, ale gdy dane sko\u015bne / nier\u00f3wne wariancje."),
          tags$td(tags$code("ANOVA \u2192 One-Way ANOVA Kruskal-Wallis")),
          tags$td("\u03c7\u00b2, df, p, \u03b5\u00b2 (efekt)")
        ),
        tags$tr(
          tags$td(tags$b("Post-hoc: Dunn")),
          tags$td("Por\u00f3wnania par ", tags$em("po"), " istotnym Kruskal-Wallisie."),
          tags$td(tags$code("ANOVA \u2192 One-Way ANOVA Kruskal-Wallis"), br(),
                  "\u2192 zaznacz ", tags$b("DSCF pairwise")),
          tags$td("p-warto\u015bci dla ka\u017cdej pary")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Zasada: "),
      "najpierw test g\u0142\u00f3wny (ANOVA albo Kruskal-Wallis). Je\u015bli istotny \u2192 post-hoc tym samym \"rodzajem\" (Tukey po ANOVA, Dunn po KW).
       Je\u015bli nieistotny \u2192 post-hoc pomijamy."
    ),

    # ========================================================================
    div(class = "section-title", "Miary wielko\u015bci efektu"),

    tags$table(class = "table table-bordered",
      style = "font-size: 14px;",
      tags$thead(
        tags$tr(
          tags$th("Miara"), tags$th("Test"),
          tags$th("Ma\u0142y"), tags$th("\u015aredni"), tags$th("Du\u017cy"),
          tags$th("Co to znaczy praktycznie?")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("Cohen's d"), tags$td("Test t (2 grupy)"),
          tags$td("0.2"), tags$td("0.5"), tags$td("0.8"),
          tags$td("d = 0.2 ledwie uchwytne; d = 0.5 wykryje wyszkolony panel sensoryczny; d = 0.8 zauwa\u017cy konsument w te\u015bcie \u015blepym.")
        ),
        tags$tr(
          tags$td("r (korelacja)"), tags$td("Pearson/Spearman"),
          tags$td("0.1"), tags$td("0.3"), tags$td("0.5"),
          tags$td("|r| = 0.3 \u2192 zwi\u0105zek widoczny na wykresie; |r| = 0.5 \u2192 wyra\u017any trend; |r| > 0.7 \u2192 bardzo silny.")
        ),
        tags$tr(
          tags$td("Cram\u00e9r's V"), tags$td("\u03c7\u00b2 niezale\u017cno\u015bci"),
          tags$td("0.1"), tags$td("0.3"), tags$td("0.5"),
          tags$td("V = 0.1 odsetki w grupach r\u00f3\u017cni\u0105 si\u0119 o kilka punkt\u00f3w proc.; V = 0.5 r\u00f3\u017cnice rz\u0119du kilkudziesi\u0119ciu pp.")
        ),
        tags$tr(
          tags$td(withMathJax("\\(\\eta^2\\)")), tags$td("ANOVA"),
          tags$td("0.01"), tags$td("0.06"), tags$td("0.14"),
          tags$td("\u03b7\u00b2 = 0.06 czynnik t\u0142umaczy ~6% zmienno\u015bci (reszta: inne przyczyny); \u03b7\u00b2 = 0.14 to ~14% \u2014 czynnik dominuj\u0105cy.")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Regu\u0142a interpretacji: "),
      "progi Cohena to punkt wyj\u015bcia, nie wyrocznia. To, czy d = 0.3 jest \"ma\u0142e\" czy \"wa\u017cne\", zale\u017cy od dziedziny.
       Dla bezpiecze\u0144stwa \u017cywno\u015bci (toksyny, patogeny) nawet ma\u0142y efekt bywa krytyczny. Dla sensoryki \u2014 liczy si\u0119 dopiero efekt \u015bredni."
    ),

    # ========================================================================
    div(class = "section-title", "P-warto\u015b\u0107 \u2014 przypomnienie"),

    div(class = "callout-success",
      tags$strong("P-warto\u015b\u0107 to:"),
      p("Prawdopodobie\u0144stwo uzyskania wyniku co najmniej tak skrajnego,
        zak\u0142adaj\u0105c \u017ce H\u2080 jest prawdziwa.")
    ),

    div(class = "callout-danger",
      tags$strong("P-warto\u015b\u0107 NIE jest:"),
      tags$ul(
        tags$li("Prawdopodobie\u0144stwem, \u017ce H\u2080 jest prawdziwa"),
        tags$li("Prawdopodobie\u0144stwem, \u017ce wynik jest przypadkowy"),
        tags$li("Miar\u0105 wielko\u015bci efektu (p = 0.001 \u2260 du\u017cy efekt!)")
      )
    ),

    # ========================================================================
    div(class = "section-title", "Typowe pu\u0142apki"),

    div(class = "callout-danger",
      tags$ul(
        tags$li(tags$b("P-hacking:"),
                " pr\u00f3bowanie testu a\u017c wyjdzie p < 0.05 (parametryczny \u2192 nieparametryczny \u2192 usuwanie \"outlier\u00f3w\" \u2192 zmiana hipotezy).
                 To nie jest analiza \u2014 to wyszukiwanie szumu. Analiza powinna by\u0107 zaplanowana ", tags$em("przed"),
                " patrzeniem na wyniki."),
        tags$li(tags$b("Wielokrotne por\u00f3wnania:"),
                " testujesz 4 metody pasteryzacji mleka \u2192 masz 6 par. Bez korekcji ryzyko co najmniej jednego fa\u0142szywego alarmu ro\u015bnie do ~26% (zamiast 5%).
                 Dlatego po ANOVA stosuje si\u0119 Tukey, a po Kruskal-Wallisie \u2014 Dunna."),
        tags$li(tags$b("Brak istotno\u015bci \u2260 brak efektu:"),
                " cz\u0119sto znaczy po prostu \"za ma\u0142o danych, \u017ceby to zobaczy\u0107\".
                 Sprawd\u017a wielko\u015b\u0107 efektu i szeroko\u015b\u0107 przedzia\u0142u ufno\u015bci \u2014 je\u015bli CI jest bardzo szeroki, wynik jest niepewny."),
        tags$li(tags$b("Istotno\u015b\u0107 statystyczna \u2260 istotno\u015b\u0107 praktyczna:"),
                " przy n = 10 000 nawet r\u00f3\u017cnica 0.01 pH mo\u017ce by\u0107 istotna \u2014 ale technologicznie nic nie znaczy.
                 Zawsze raportuj p ", tags$b("i"), " wielko\u015b\u0107 efektu (d, \u03b7\u00b2, V).")
      )
    ),

    # ========================================================================
    div(class = "section-title", "Kod R \u2014 rstatix"),

    div(class = "formula-box",
      tags$pre(style = "background: #f8f9fa; padding: 12px; border-radius: 6px; font-size: 13px;",
        tags$code(
"library(rstatix)
library(broom)

# === Jedna zmienna ===
# Test t jednej proby
data %>% t_test(oceny ~ 1, mu = 3.5)

# Wilcoxon jednej proby
data %>% wilcox_test(oceny ~ 1, mu = 3.5)

# === Korelacja ===
data %>% cor_test(wzrost, waga, method = \"pearson\")
data %>% cor_test(wzrost, waga, method = \"spearman\")

# === Dwie grupy ===
# Niezalezne
data %>% t_test(wzrost ~ plec)
data %>% wilcox_test(wzrost ~ plec)
data %>% cohens_d(wzrost ~ plec)

# Parowe
data %>% t_test(wynik ~ moment, paired = TRUE)
data %>% wilcox_test(wynik ~ moment, paired = TRUE)

# === Chi-kwadrat ===
tab <- table(data$plec, data$kierunek)
chisq.test(tab)
fisher.test(tab)

# === ANOVA ===
data %>% anova_test(oceny ~ kierunek)
data %>% tukey_hsd(oceny ~ kierunek)

# Kruskal-Wallis
data %>% kruskal_test(oceny ~ kierunek)
data %>% dunn_test(oceny ~ kierunek, p.adjust.method = \"holm\")"
        )
      )
    )

  ))
)

# ============================================================================
# SERVER (brak interaktywnych widgetow)
# ============================================================================

ch8_server <- function(input, output, session) {
  # Sciaga nie wymaga logiki server
}
