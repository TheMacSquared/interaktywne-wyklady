# ============================================================================
# CHAPTER 9: Sciaga
# ============================================================================

ch9_ui <- tabPanel("9. \u015aci\u0105ga",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Szybkie podsumowanie wszystkich algorytm\u00f3w, tabela decyzyjna i s\u0142ownik."
    ),

    # ========================================================================
    # ALGORYTMY
    # ========================================================================
    div(class = "section-title", "Algorytmy"),

    div(class = "formula-box",
      tags$strong("Bootstrap CI (metoda percentylowa):"),
      tags$pre(style = "margin:8px 0 0 0; font-size:13px;",
"Dane: x = (x\u2081, ..., x\u2099)
Dla b = 1, ..., B:
  x*\u1d47 = losowanie ze zwracaniem z x (n obserwacji)
  \u03b8*\u1d47 = statystyka(x*\u1d47)
SE\u1d47\u1d52\u1d52\u1d57 = sd(\u03b8*\u2081, ..., \u03b8*_B)
95% CI (percentyl) = [Q\u2080.\u2080\u2082\u2085(\u03b8*), Q\u2080.\u2097\u2085(\u03b8*)]")
    ),

    div(class = "formula-box",
      tags$strong("Test permutacyjny (dwie grupy):"),
      tags$pre(style = "margin:8px 0 0 0; font-size:13px;",
"Dane: (x\u2081,...,x\u2099\u2090) z grupy A, (y\u2081,...,y\u2099\u1d47) z grupy B
\u0394_obs = mean(B) - mean(A)
Dla b = 1, ..., B:
  Przetasuj losowo etykiety A/B w polaczonych danych
  \u0394*\u1d47 = mean(nowe B) - mean(nowe A)
p-warto\u015b\u0107 = #{|\u0394*\u1d47| >= |\u0394_obs|} / B")
    ),

    div(class = "formula-box",
      tags$strong("Jackknife:"),
      tags$pre(style = "margin:8px 0 0 0; font-size:13px;",
"Dane: x = (x\u2081, ..., x\u2099)
\u03b8\u0302 = statystyka(x)
Dla i = 1, ..., n:
  \u03b8\u0302\u208b\u1d35 = statystyka(x bez x\u1d35)
\u03b8\u0305\u208b = mean(\u03b8\u0302\u208b\u2081, ..., \u03b8\u0302\u208b\u2099)
Bias = (n-1) * (\u03b8\u0305\u208b - \u03b8\u0302)
SE   = sqrt((n-1)/n * sum((\u03b8\u0302\u208b\u1d35 - \u03b8\u0305\u208b)^2))
BC   = \u03b8\u0302 - Bias")
    ),

    div(class = "formula-box",
      tags$strong("K-Fold Cross-Validation:"),
      tags$pre(style = "margin:8px 0 0 0; font-size:13px;",
"Podziel dane na k fold\u00f3w (k = 5 lub 10 zwykle)
Dla fold\u00f3w f = 1, ..., k:
  Trening  = wszystkie dane poza foldem f
  Test     = fold f
  B\u0142\u0105d_f = MSE(predykcje - obserwacje) na foldem f
CV MSE = mean(B\u0142\u0105d\u2081, ..., B\u0142\u0105d_k)
Najlepszy model: minimalny CV MSE")
    ),

    div(class = "formula-box",
      tags$strong("Monte Carlo \u2014 symulacja mocy:"),
      tags$pre(style = "margin:8px 0 0 0; font-size:13px;",
"Parametry: n, \u03b4 (efekt), \u03b1, B
Dla b = 1, ..., B:
  x\u1d47 = rnorm(n, mean=0,  sd=\u03c3)   # grupa kontrolna
  y\u1d47 = rnorm(n, mean=\u03b4, sd=\u03c3)   # grupa eksperymentalna
  p\u1d47 = t.test(y\u1d47, x\u1d47)$p.value
Moc = #{p\u1d47 < \u03b1} / B")
    ),

    # ========================================================================
    # BOOTSTRAP W REGRESJI (jedyna wzmianka)
    # ========================================================================
    div(class = "section-title", "Bootstrap w regresji"),

    div(class = "narrative",
      p("Bootstrap w regresji liniowej pozwala uzyska\u0107 przedzia\u0142y ufno\u015bci
         dla wsp\u00f3\u0142czynnik\u00f3w bez zak\u0142adania normalno\u015bci reszt."),
      p("Dwa podej\u015bcia:"),
      tags$ol(
        tags$li(tags$b("Bootstrap par (x, y)"),
                ": resampluj wiersze danych, nast\u0119pnie dopasuj model.
                 Dobre gdy chcemy uwzgl\u0119dni\u0107 niepewno\u015b\u0107 w x."),
        tags$li(tags$b("Bootstrap reszt"),
                ": dopasuj model, resampluj reszty, dodaj do dopasowanych warto\u015bci.
                 Zak\u0142ada poprawno\u015b\u0107 struktury modelu.")
      )
    ),

    div(class = "formula-box",
      tags$strong("Bootstrap CI dla \u03b2\u2081 (para-resampling w R):"),
      tags$pre(style = "margin:8px 0 0 0; font-size:13px;",
"library(car)
model <- lm(y ~ x, data = df)
boot_result <- car::Boot(model, R = 1000)
confint(boot_result, level = 0.95, type = 'perc')

# lub r\u0119cznie:
boot_betas <- replicate(1000, {
  idx  <- sample(nrow(df), replace = TRUE)
  coef(lm(y ~ x, data = df[idx, ]))[['x']]
})
quantile(boot_betas, c(0.025, 0.975))   # 95% CI")
    ),

    div(class = "callout-info",
      tags$strong("Kiedy bootstrap w regresji:"),
      tags$ul(
        tags$li("Ma\u0142a pr\u00f3ba w regresji (n < 50)"),
        tags$li("Reszty wyra\u017anie nienormalne"),
        tags$li("Wp\u0142ywowe obserwacje (outliery w x lub y)")
      )
    ),

    # ========================================================================
    # TABELA: ZALECANE B / k
    # ========================================================================
    div(class = "section-title", "Zalecane warto\u015bci B i k"),

    tags$table(class = "decision-table",
      tags$thead(tags$tr(
        tags$th("Zastosowanie"),
        tags$th("Zalecane B lub k")
      )),
      tags$tbody(
        tags$tr(
          tags$td("Bootstrap CI (orientacyjny)"),
          tags$td("B = 500\u20131000")
        ),
        tags$tr(
          tags$td("Bootstrap CI (dok\u0142adny)"),
          tags$td("B = 2000\u20135000")
        ),
        tags$tr(
          tags$td("Test permutacyjny (p-warto\u015b\u0107)"),
          tags$td("B = 2000\u201310\u202f000")
        ),
        tags$tr(
          tags$td("CV (kompromis obci\u0105\u017cenie/wariancja)"),
          tags$td("k = 5 lub k = 10")
        ),
        tags$tr(
          tags$td("LOOCV (niskie obci\u0105\u017cenie, wysoka wariancja)"),
          tags$td("k = n")
        ),
        tags$tr(
          tags$td("MC symulacja mocy (dok\u0142adna)"),
          tags$td("B = 1000\u20135000")
        ),
        tags$tr(
          tags$td("MC pod H\u2080 (precyzyjna p-warto\u015b\u0107)"),
          tags$td("B \u2265 10\u202f000")
        )
      )
    ),

    # ========================================================================
    # SLOWNIK
    # ========================================================================
    div(class = "section-title", "S\u0142ownik polsko-angielski"),

    tags$table(class = "decision-table",
      tags$thead(tags$tr(
        tags$th("Polski"),
        tags$th("English")
      )),
      tags$tbody(
        tags$tr(tags$td("Pr\u00f3ba bootstrapowa"),
                tags$td("Bootstrap sample / resample")),
        tags$tr(tags$td("Losowanie ze zwracaniem"),
                tags$td("Sampling with replacement")),
        tags$tr(tags$td("Rozk\u0142ad bootstrapowy"),
                tags$td("Bootstrap distribution")),
        tags$tr(tags$td("Przedzia\u0142 percentylowy"),
                tags$td("Percentile interval")),
        tags$tr(tags$td("Przedzia\u0142 basic (odbi\u0107)"),
                tags$td("Basic (reflected) interval")),
        tags$tr(tags$td("Test permutacyjny"),
                tags$td("Permutation test / randomization test")),
        tags$tr(tags$td("Przetasowanie etykiet"),
                tags$td("Label shuffling / permutation")),
        tags$tr(tags$td("Rozk\u0142ad permutacyjny"),
                tags$td("Permutation (null) distribution")),
        tags$tr(tags$td("Jackknife (leave-one-out)"),
                tags$td("Jackknife / leave-one-out resampling")),
        tags$tr(tags$td("Obci\u0105\u017cenie estymatora"),
                tags$td("Bias")),
        tags$tr(tags$td("Estymator skorygowany o bias"),
                tags$td("Bias-corrected estimate")),
        tags$tr(tags$td("Walidacja krzy\u017cowa"),
                tags$td("Cross-validation (CV)")),
        tags$tr(tags$td("Prze-uczenie"),
                tags$td("Overfitting")),
        tags$tr(tags$td("Moc testu"),
                tags$td("Power of a test")),
        tags$tr(tags$td("Wielko\u015b\u0107 efektu"),
                tags$td("Effect size"))
      )
    ),

    br(),
    div(class = "chapter-transition",
      p("Dalej: \u0107wiczenia kierunkowe"),
      actionButton("ch9_to_ch10",
                   "Dalej \u2192 10. \u0106wiczenia",
                   class = "btn-primary btn-lg")
    ),
    br()

  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch9_server <- function(input, output, session) {
  # Rozdzial statyczny — brak reaktywnych elementow
}
