# ============================================================================
# CHAPTER 9: Sciaga
# ============================================================================

ch9_ui <- lecture_chapter(
  id = "ch-sciaga",
  num = "09",
  title = "Ściąga",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 09 · Symulacje statystyczne",
      num    = "09",
      title  = "Ściąga",
      lead   = "Podsumowanie algorytmów, parametrów i słownictwa dla metod symulacyjnych."
    ),

    lc_feedback(type = "info",
      "Szybkie podsumowanie wszystkich algorytmów, tabela decyzyjna i słownik."
    ),

    # ========================================================================
    # ALGORYTMY
    # ========================================================================
    lc_h2("ch9-sec-01", "Algorytmy"),

    lc_formula_box(
      tags$strong("Bootstrap CI (metoda percentylowa):"),
      tags$pre(style = "margin:8px 0 0 0; font-size:13px;",
"Dane: x = (x₁, ..., xₙ)
Dla b = 1, ..., B:
  x*ᵇ = losowanie ze zwracaniem z x (n obserwacji)
  θ*ᵇ = statystyka(x*ᵇ)
SEᵇᵒᵒᵗ = sd(θ*₁, ..., θ*_B)
95% CI (percentyl) = [Q₀.₀₂₅(θ*), Q₀.ₗ₅(θ*)]")
    ),

    lc_formula_box(
      tags$strong("Test permutacyjny (dwie grupy):"),
      tags$pre(style = "margin:8px 0 0 0; font-size:13px;",
"Dane: (x₁,...,xₙₐ) z grupy A, (y₁,...,yₙᵇ) z grupy B
Δ_obs = mean(B) - mean(A)
Dla b = 1, ..., B:
  Przetasuj losowo etykiety A/B w polaczonych danych
  Δ*ᵇ = mean(nowe B) - mean(nowe A)
p-wartość = #{|Δ*ᵇ| >= |Δ_obs|} / B")
    ),

    lc_formula_box(
      tags$strong("Jackknife:"),
      tags$pre(style = "margin:8px 0 0 0; font-size:13px;",
"Dane: x = (x₁, ..., xₙ)
θ̂ = statystyka(x)
Dla i = 1, ..., n:
  θ̂₋ᴵ = statystyka(x bez xᴵ)
θ̅₋ = mean(θ̂₋₁, ..., θ̂₋ₙ)
Bias = (n-1) * (θ̅₋ - θ̂)
SE   = sqrt((n-1)/n * sum((θ̂₋ᴵ - θ̅₋)^2))
BC   = θ̂ - Bias")
    ),

    lc_formula_box(
      tags$strong("K-Fold Cross-Validation:"),
      tags$pre(style = "margin:8px 0 0 0; font-size:13px;",
"Podziel dane na k foldów (k = 5 lub 10 zwykle)
Dla foldów f = 1, ..., k:
  Trening  = wszystkie dane poza foldem f
  Test     = fold f
  Błąd_f = MSE(predykcje - obserwacje) na foldem f
CV MSE = mean(Błąd₁, ..., Błąd_k)
Najlepszy model: minimalny CV MSE")
    ),

    lc_formula_box(
      tags$strong("Monte Carlo — symulacja mocy:"),
      tags$pre(style = "margin:8px 0 0 0; font-size:13px;",
"Parametry: n, δ (efekt), α, B
Dla b = 1, ..., B:
  xᵇ = rnorm(n, mean=0,  sd=σ)   # grupa kontrolna
  yᵇ = rnorm(n, mean=δ, sd=σ)   # grupa eksperymentalna
  pᵇ = t.test(yᵇ, xᵇ)$p.value
Moc = #{pᵇ < α} / B")
    ),

    # ========================================================================
    # BOOTSTRAP W REGRESJI (jedyna wzmianka)
    # ========================================================================
    lc_h2("ch9-sec-02", "Bootstrap w regresji"),

    tagList(
      p("Bootstrap w regresji liniowej pozwala uzyskać przedziały ufności
         dla współczynników bez zakładania normalności reszt."),
      p("Dwa podejścia:"),
      tags$ol(
        tags$li(tags$b("Bootstrap par (x, y)"),
                ": resampluj wiersze danych, następnie dopasuj model.
                 Dobre gdy chcemy uwzględnić niepewność w x."),
        tags$li(tags$b("Bootstrap reszt"),
                ": dopasuj model, resampluj reszty, dodaj do dopasowanych wartości.
                 Zakłada poprawność struktury modelu.")
      )
    ),

    lc_formula_box(
      tags$strong("Bootstrap CI dla β₁ (para-resampling w R):"),
      tags$pre(style = "margin:8px 0 0 0; font-size:13px;",
"library(car)
model <- lm(y ~ x, data = df)
boot_result <- car::Boot(model, R = 1000)
confint(boot_result, level = 0.95, type = 'perc')

# lub ręcznie:
boot_betas <- replicate(1000, {
  idx  <- sample(nrow(df), replace = TRUE)
  coef(lm(y ~ x, data = df[idx, ]))[['x']]
})
quantile(boot_betas, c(0.025, 0.975))   # 95% CI")
    ),

    lc_feedback(type = "info",
      tags$strong("Kiedy bootstrap w regresji:"),
      tags$ul(
        tags$li("Mała próba w regresji (n < 50)"),
        tags$li("Reszty wyraźnie nienormalne"),
        tags$li("Wpływowe obserwacje (outliery w x lub y)")
      )
    ),

    # ========================================================================
    # TABELA: ZALECANE B / k
    # ========================================================================
    lc_h2("ch9-sec-03", "Zalecane wartości B i k"),

    tags$table(class = "decision-table",
      tags$thead(tags$tr(
        tags$th("Zastosowanie"),
        tags$th("Zalecane B lub k")
      )),
      tags$tbody(
        tags$tr(
          tags$td("Bootstrap CI (orientacyjny)"),
          tags$td("B = 500–1000")
        ),
        tags$tr(
          tags$td("Bootstrap CI (dokładny)"),
          tags$td("B = 2000–5000")
        ),
        tags$tr(
          tags$td("Test permutacyjny (p-wartość)"),
          tags$td("B = 2000–10 000")
        ),
        tags$tr(
          tags$td("CV (kompromis obciążenie/wariancja)"),
          tags$td("k = 5 lub k = 10")
        ),
        tags$tr(
          tags$td("LOOCV (niskie obciążenie, wysoka wariancja)"),
          tags$td("k = n")
        ),
        tags$tr(
          tags$td("MC symulacja mocy (dokładna)"),
          tags$td("B = 1000–5000")
        ),
        tags$tr(
          tags$td("MC pod H₀ (precyzyjna p-wartość)"),
          tags$td("B ≥ 10 000")
        )
      )
    ),

    # ========================================================================
    # SLOWNIK
    # ========================================================================
    lc_h2("ch9-sec-04", "Słownik polsko-angielski"),

    tags$table(class = "decision-table",
      tags$thead(tags$tr(
        tags$th("Polski"),
        tags$th("English")
      )),
      tags$tbody(
        tags$tr(tags$td("Próba bootstrapowa"),
                tags$td("Bootstrap sample / resample")),
        tags$tr(tags$td("Losowanie ze zwracaniem"),
                tags$td("Sampling with replacement")),
        tags$tr(tags$td("Rozkład bootstrapowy"),
                tags$td("Bootstrap distribution")),
        tags$tr(tags$td("Przedział percentylowy"),
                tags$td("Percentile interval")),
        tags$tr(tags$td("Przedział basic (odbić)"),
                tags$td("Basic (reflected) interval")),
        tags$tr(tags$td("Test permutacyjny"),
                tags$td("Permutation test / randomization test")),
        tags$tr(tags$td("Przetasowanie etykiet"),
                tags$td("Label shuffling / permutation")),
        tags$tr(tags$td("Rozkład permutacyjny"),
                tags$td("Permutation (null) distribution")),
        tags$tr(tags$td("Jackknife (leave-one-out)"),
                tags$td("Jackknife / leave-one-out resampling")),
        tags$tr(tags$td("Obciążenie estymatora"),
                tags$td("Bias")),
        tags$tr(tags$td("Estymator skorygowany o bias"),
                tags$td("Bias-corrected estimate")),
        tags$tr(tags$td("Walidacja krzyżowa"),
                tags$td("Cross-validation (CV)")),
        tags$tr(tags$td("Prze-uczenie"),
                tags$td("Overfitting")),
        tags$tr(tags$td("Moc testu"),
                tags$td("Power of a test")),
        tags$tr(tags$td("Wielkość efektu"),
                tags$td("Effect size"))
      )
    ),

    br(),
    lc_chapter_next(
      num = "10",
      title = "Ćwiczenia",
      lead = "zadania praktyczne do wykonania w Jamovi.",
      target_id = "ch-cwiczenia"
    ),
    br()

  )
)
# ============================================================================
# SERVER
# ============================================================================

ch9_server <- function(input, output, session) {
  # Rozdzial statyczny — brak reaktywnych elementow
}
