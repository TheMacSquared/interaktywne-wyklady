# ============================================================================
# CHAPTER 6: Sciaga - podsumowanie zalozen
# ============================================================================

ch6_ui <- tabPanel("6. \u015aci\u0105ga",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Kompaktowe podsumowanie: za\u0142o\u017cenia, testy diagnostyczne i alternatywy."
    ),

    # ========================================================================
    div(class = "section-title", "Schemat post\u0119powania"),

    div(class = "callout-info",
      tags$strong("Krok 1:"), " Wybierz metod\u0119 na podstawie typu zmiennych i pytania badawczego.",
      br(), br(),
      tags$strong("Krok 2:"), " Sprawd\u017a za\u0142o\u017cenia wizualnie (wykresy) i formalnie (testy).",
      br(), br(),
      tags$strong("Krok 3a:"), " Za\u0142o\u017cenia spe\u0142nione \u2192 u\u017cyj metody parametrycznej.",
      br(),
      tags$strong("Krok 3b:"), " Za\u0142o\u017cenia naruszone \u2192 u\u017cyj alternatywy.",
      br(), br(),
      tags$strong("Krok 4:"), " Raportuj wyniki z wielko\u015bci\u0105 efektu i p-warto\u015bci\u0105."
    ),

    # ========================================================================
    div(class = "section-title", "Testy diagnostyczne \u2014 szybka referencja"),

    tags$table(class = "table table-bordered table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Za\u0142o\u017cenie"), tags$th("Test"), tags$th("R"), tags$th("H\u2080"))
      ),
      tags$tbody(
        tags$tr(
          tags$td("Normalno\u015b\u0107"),
          tags$td("Shapiro-Wilk"),
          tags$td(tags$code("shapiro_test()")),
          tags$td("Dane s\u0105 normalne")
        ),
        tags$tr(
          tags$td("Normalno\u015b\u0107"),
          tags$td("Kolmogorov-Smirnov"),
          tags$td(tags$code("ks.test()")),
          tags$td("Dane maj\u0105 podany rozk\u0142ad")
        ),
        tags$tr(
          tags$td("R\u00f3wne wariancje"),
          tags$td("Levene"),
          tags$td(tags$code("levene_test()")),
          tags$td("Wariancje r\u00f3wne")
        ),
        tags$tr(
          tags$td("R\u00f3wne wariancje"),
          tags$td("Bartlett"),
          tags$td(tags$code("bartlett.test()")),
          tags$td("Wariancje r\u00f3wne")
        ),
        tags$tr(
          tags$td("Homoscedast. reszt"),
          tags$td("Breusch-Pagan"),
          tags$td(tags$code("lmtest::bptest()")),
          tags$td("Wariancja reszt sta\u0142a")
        ),
        tags$tr(
          tags$td("Niezale\u017cn. reszt"),
          tags$td("Durbin-Watson"),
          tags$td(tags$code("lmtest::dwtest()")),
          tags$td("Brak autokorelacji")
        ),
        tags$tr(
          tags$td("Wsp\u00f3\u0142liniowo\u015b\u0107"),
          tags$td("VIF"),
          tags$td(tags$code("car::vif()")),
          tags$td("VIF < 5 (niekt\u00f3rzy < 10)")
        )
      )
    ),

    # ========================================================================
    div(class = "section-title", "Metoda \u2192 alternatywa (quick reference)"),

    tags$table(class = "table table-bordered",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Metoda parametryczna"), tags$th("\u2192 Alternatywa nieparametryczna"))
      ),
      tags$tbody(
        tags$tr(tags$td("Test t jednej pr\u00f3by"), tags$td("Wilcoxon jednej pr\u00f3by")),
        tags$tr(tags$td("Test t niezale\u017cny"), tags$td("Mann-Whitney U")),
        tags$tr(tags$td("Test t parowy"), tags$td("Wilcoxon par znakowych")),
        tags$tr(tags$td("ANOVA"), tags$td("Kruskal-Wallis")),
        tags$tr(tags$td("Tukey HSD (post-hoc)"), tags$td("Test Dunna")),
        tags$tr(tags$td("Pearson"), tags$td("Spearman")),
        tags$tr(tags$td("\u03c7\u00b2 (ma\u0142e n)"), tags$td("Fisher (dok\u0142adny)")),
        tags$tr(tags$td("Regresja OLS"), tags$td("Odporne SE / bootstrap / GLM"))
      )
    ),

    # ========================================================================
    div(class = "section-title", "Praktyczne rady"),

    div(class = "callout-success",
      tags$ul(
        tags$li(tags$b("Wizualizacja > testy formalne."),
                " Wykresy daj\u0105 intuicj\u0119, testy daj\u0105 liczb\u0119. U\u017cywaj obu."),
        tags$li(tags$b("Testy Welcha s\u0105 domy\u015blne w R."),
                " Nie musisz sprawdza\u0107 r\u00f3wno\u015bci wariancji przed testem t."),
        tags$li(tags$b("Du\u017ce n \u0142agodzi naruszenia."),
                " Przy n > 30 testy parametryczne s\u0105 odporne na brak normalno\u015bci (CTG)."),
        tags$li(tags$b("Testy nieparametryczne nie s\u0105 \"gorsze\"."),
                " Maj\u0105 mniejsz\u0105 moc przy spe\u0142nionych za\u0142o\u017ceniach, ale s\u0105 bezpieczniejsze og\u00f3lnie."),
        tags$li(tags$b("Raportuj zawsze wielko\u015b\u0107 efektu"),
                " \u2014 p-warto\u015b\u0107 nie m\u00f3wi, jak du\u017cy jest efekt.")
      )
    ),

    # ========================================================================
    div(class = "section-title", "Kod R \u2014 diagnostyka"),

    div(class = "formula-box",
      tags$pre(style = "background: #f8f9fa; padding: 12px; border-radius: 6px; font-size: 13px;",
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

  ))
)

ch6_server <- function(input, output, session) {
}
