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
          tags$td("1 ilo\u015bciowa vs \u03bc\u2080"),
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
    div(class = "section-title", "Miary wielko\u015bci efektu"),

    tags$table(class = "table table-bordered",
      style = "font-size: 14px;",
      tags$thead(
        tags$tr(
          tags$th("Miara"), tags$th("Test"), tags$th("Ma\u0142y"), tags$th("\u015aredni"), tags$th("Du\u017cy")
        )
      ),
      tags$tbody(
        tags$tr(tags$td("Cohen's d"), tags$td("Test t"), tags$td("0.2"), tags$td("0.5"), tags$td("0.8")),
        tags$tr(tags$td("r (korelacja)"), tags$td("Pearson/Spearman"), tags$td("0.1"), tags$td("0.3"), tags$td("0.5")),
        tags$tr(tags$td("Cram\u00e9r's V"), tags$td("\u03c7\u00b2"), tags$td("0.1"), tags$td("0.3"), tags$td("0.5")),
        tags$tr(tags$td(withMathJax("\\(\\eta^2\\)")), tags$td("ANOVA"), tags$td("0.01"), tags$td("0.06"), tags$td("0.14"))
      )
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
        tags$li(tags$b("P-hacking:"), " testowanie wielu por\u00f3wna\u0144 i raportowanie
                 tylko istotnych \u2014 zwi\u0119ksza b\u0142\u0105d I rodzaju"),
        tags$li(tags$b("Wielokrotne por\u00f3wnania:"), " przy k grupach jest k(k\u22121)/2
                 par. Bez korekcji (Bonferroni, Tukey) ryzyko fa\u0142szywych alarm\u00f3w ro\u015bnie"),
        tags$li(tags$b("Brak istotno\u015bci \u2260 brak efektu:"),
                " mo\u017ce brakowa\u0107 mocy (za ma\u0142e n)"),
        tags$li(tags$b("Istotno\u015b\u0107 statystyczna \u2260 istotno\u015b\u0107 praktyczna:"),
                " przy n = 10 000 nawet trywialna r\u00f3\u017cnica mo\u017ce by\u0107 istotna.
                 Zawsze podawaj wielko\u015b\u0107 efektu!")
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
