# ============================================================================
# CHAPTER 5: Sciaga - podsumowanie regresji
# ============================================================================

ch5_ui <- tabPanel("5. \u015aci\u0105ga",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Kompaktowe podsumowanie regresji liniowej, wielorakiej i logistycznej."
    ),

    # ========================================================================
    div(class = "section-title", "Trzy typy regresji"),

    tags$table(class = "table table-bordered table-striped",
      style = "font-size: 14px;",
      tags$thead(
        tags$tr(tags$th(""), tags$th("Liniowa prosta"), tags$th("Wieloraka"), tags$th("Logistyczna"))
      ),
      tags$tbody(
        tags$tr(tags$td(tags$strong("Y")), tags$td("Ci\u0105g\u0142a"), tags$td("Ci\u0105g\u0142a"), tags$td("Binarna (0/1)")),
        tags$tr(tags$td(tags$strong("X")), tags$td("1 predyktor"), tags$td("k predyktor\u00f3w"), tags$td("k predyktor\u00f3w")),
        tags$tr(tags$td(tags$strong("Wz\u00f3r")),
                tags$td(withMathJax("\\(Y = \\beta_0 + \\beta_1 X + \\varepsilon\\)")),
                tags$td(withMathJax("\\(Y = \\beta_0 + \\sum \\beta_j X_j + \\varepsilon\\)")),
                tags$td(withMathJax("\\(\\ln\\frac{p}{1-p} = \\beta_0 + \\sum \\beta_j X_j\\)"))),
        tags$tr(tags$td(tags$strong("Estymacja")), tags$td("OLS"), tags$td("OLS"), tags$td("MLE")),
        tags$tr(tags$td(tags$strong("Dopasowanie")), tags$td("R\u00b2, RMSE"), tags$td("adj.R\u00b2, AIC, BIC, RMSE"), tags$td("AIC, BIC, dok\u0142adno\u015b\u0107")),
        tags$tr(tags$td(tags$strong("Interpr. \u03b2")),
                tags$td("zmiana Y na 1 jedn. X"),
                tags$td("zmiana Y na 1 jedn. Xj (ceteris paribus)"),
                tags$td(withMathJax("\\(OR = e^\\beta\\)")))
      )
    ),

    # ========================================================================
    div(class = "section-title", "Metryki por\u00f3wnawcze"),

    tags$table(class = "table table-bordered",
      style = "font-size: 14px;",
      tags$thead(
        tags$tr(tags$th("Metryka"), tags$th("Wz\u00f3r"), tags$th("Kierunek"), tags$th("Uwagi"))
      ),
      tags$tbody(
        tags$tr(
          tags$td(withMathJax("\\(R^2\\)")),
          tags$td(withMathJax("\\(1 - SS_{res}/SS_{tot}\\)")),
          tags$td("\u2191 lepiej"),
          tags$td("Zawsze ro\u015bnie z k; nie por\u00f3wnuj modeli o r\u00f3\u017cnej z\u0142o\u017cono\u015bci")
        ),
        tags$tr(
          tags$td(withMathJax("\\(R^2_{adj}\\)")),
          tags$td(withMathJax("\\(1 - \\frac{(1-R^2)(n-1)}{n-k-1}\\)")),
          tags$td("\u2191 lepiej"),
          tags$td("Karze za zb\u0119dne predyktory; bezpieczniejsze ni\u017c R\u00b2")
        ),
        tags$tr(
          tags$td("AIC"),
          tags$td(withMathJax("\\(-2\\ln L + 2k\\)")),
          tags$td("\u2193 lepiej"),
          tags$td("Lepszy do predykcji; \u0142agodniejsza kara")
        ),
        tags$tr(
          tags$td("BIC"),
          tags$td(withMathJax("\\(-2\\ln L + k\\ln n\\)")),
          tags$td("\u2193 lepiej"),
          tags$td("Silniejsza kara za parametry; preferuje prostsze modele")
        ),
        tags$tr(
          tags$td("RMSE"),
          tags$td(withMathJax("\\(\\sqrt{\\frac{1}{n}\\sum e_i^2}\\)")),
          tags$td("\u2193 lepiej"),
          tags$td("W jednostkach Y; intuicyjne")
        )
      )
    ),

    # ========================================================================
    div(class = "section-title", "Interpretacja wsp\u00f3\u0142czynnik\u00f3w"),

    div(class = "callout-info",
      tags$strong("Regresja liniowa:"),
      p(withMathJax("\\(\\beta_1 = 0.5\\)"), " oznacza: wzrost X o 1 powoduje wzrost Y o 0.5 (ceteris paribus)."),

      tags$strong("Regresja logistyczna:"),
      p(withMathJax("\\(\\beta_1 = 0.5 \\Rightarrow OR = e^{0.5} = 1.65\\)"),
        " oznacza: wzrost X o 1 zwi\u0119ksza szanse sukcesu 1.65-krotnie."),

      tags$strong("Istotno\u015b\u0107 wsp\u00f3\u0142czynnik\u00f3w:"),
      p("p < 0.05 dla ", withMathJax("\\(\\beta_j\\)"), " oznacza, \u017ce predyktor ", withMathJax("\\(X_j\\)"),
        " istotnie wp\u0142ywa na Y (przy kontroli pozosta\u0142ych).")
    ),

    # ========================================================================
    div(class = "section-title", "Kiedy kt\u00f3ra regresja?"),

    div(class = "callout-success",
      tags$ul(
        tags$li(tags$b("Y ci\u0105g\u0142a, 1 predyktor"), " \u2192 regresja liniowa prosta"),
        tags$li(tags$b("Y ci\u0105g\u0142a, wiele predyktor\u00f3w"), " \u2192 regresja wieloraka"),
        tags$li(tags$b("Y binarna (0/1)"), " \u2192 regresja logistyczna"),
        tags$li(tags$b("Y porz\u0105dkowa"), " \u2192 regresja porz\u0105dkowa (ordered logit)"),
        tags$li(tags$b("Y licznikowa"), " \u2192 regresja Poissona")
      )
    ),

    # ========================================================================
    div(class = "section-title", "Funkcje R"),

    div(class = "formula-box",
      tags$pre(style = "background: #f8f9fa; padding: 12px; border-radius: 6px; font-size: 13px;",
        tags$code(
"library(broom)

# === Regresja liniowa ===
model <- lm(y ~ x1 + x2, data = dane)
summary(model)         # pelne podsumowanie
tidy(model)            # wspolczynniki jako tabelka
glance(model)          # metryki (R2, AIC, BIC, ...)

# === Regresja logistyczna ===
model_log <- glm(y ~ x1 + x2, data = dane, family = binomial)
tidy(model_log)
exp(coef(model_log))   # odds ratios
tidy(model_log, conf.int = TRUE, exponentiate = TRUE)  # OR z CI

# === Porownanie modeli ===
AIC(model1, model2, model3)
BIC(model1, model2, model3)

# === Predykcja ===
predict(model, newdata = data.frame(x1 = 5, x2 = 3))
predict(model_log, newdata = ..., type = 'response')  # prawdopodobienstwa"
        )
      )
    ),

    # ========================================================================
    div(class = "section-title", "Typowe pu\u0142apki"),

    div(class = "callout-danger",
      tags$ul(
        tags$li(tags$b("Extrapolacja:"), " Model dzia\u0142a w zakresie danych treningowych. Predykcja poza tym zakresem jest ryzykowna."),
        tags$li(tags$b("Korelacja predyktor\u00f3w:"), " Silna korelacja mi\u0119dzy X1 i X2 (wsp\u00f3\u0142liniowo\u015b\u0107) zawy\u017ca SE i utrudnia interpretacj\u0119."),
        tags$li(tags$b("Overfitting:"), " Wi\u0119cej zmiennych = wy\u017csze R\u00b2, ale gorsze uog\u00f3lnianie. Zawsze sprawdzaj adj.R\u00b2 / AIC / BIC."),
        tags$li(tags$b("R\u00b2 w logistycznej:"), " Nie u\u017cywaj R\u00b2 do oceny regresji logistycznej. U\u017cyj AIC, BIC, dok\u0142adno\u015bci, ROC-AUC.")
      )
    )

  ))
)

ch5_server <- function(input, output, session) {
}
