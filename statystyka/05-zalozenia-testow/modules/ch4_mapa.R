# ============================================================================
# CHAPTER 4: Mapa metod - kompletna tablica zalozenia -> alternatywa
# ============================================================================

ch4_ui <- lecture_chapter(
  id = "ch-mapa",
  num = "04",
  title = "Mapa metod",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 04 · Założenia testów",
      num    = "04",
      title  = "Mapa metod.",
      lead   = "Zbierzmy wszystko w jedną mapę: każda metoda, jej założenia i co robić, gdy są naruszone."
    ),

    lc_h2("ch4-kompletna-mapa", "Kompletna mapa: metoda → założenia → alternatywa"),

    # ========================================================================
    # Testy parametryczne
    # ========================================================================
    lc_h2("ch4-parametryczne", "Testy parametryczne"),

    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Metoda"), tags$th("Założenia"), tags$th("Jak sprawdzić"),
                tags$th("Gdy naruszone → alternatywa"))
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$strong("Test t jednej pr.")),
          tags$td("Brak silnej skośności i groźnych outlierów"),
          tags$td("Q-Q plot; pomocniczo Shapiro-Wilk"),
          tags$td("Wilcoxon jednej próby")
        ),
        tags$tr(
          tags$td(tags$strong("Test t niezależny")),
          tags$td("Normalność w grupach, równe wariancje"),
          tags$td("Q-Q w grupach + Levene; pomocniczo Shapiro"),
          tags$td("Welch t (nierówne war.), Mann-Whitney U (brak norm.)")
        ),
        tags$tr(
          tags$td(tags$strong("Test t sparowany")),
          tags$td("Normalność różnic"),
          tags$td("Shapiro na różnicach"),
          tags$td("Wilcoxon par znakowych")
        ),
        tags$tr(
          tags$td(tags$strong("ANOVA")),
          tags$td("Normalność reszt, równe wariancje"),
          tags$td("Shapiro + Levene"),
          tags$td("Welch ANOVA (war.), Kruskal-Wallis (norm.)")
        ),
        tags$tr(
          tags$td(tags$strong("Pearson")),
          tags$td("Liniowość, normalność 2D, brak outlierów"),
          tags$td("Scatterplot, Q-Q obu zmiennych"),
          tags$td("Spearman (monotoniczny), Kendall (odporny)")
        )
      )
    ),

    # ========================================================================
    # Testy nieparametryczne
    # ========================================================================
    lc_h2("ch4-nieparametryczne", "Testy nieparametryczne"),

    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Metoda"), tags$th("Założenia"), tags$th("Uwagi"))
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$strong("Wilcoxon / Mann-Whitney")),
          tags$td("Niezależność obserwacji, symetryczne rozkłady (dla mediany)"),
          tags$td("Nie wymaga normalności. Testuje różnicę rozkładów, nie średnich.")
        ),
        tags$tr(
          tags$td(tags$strong("Kruskal-Wallis")),
          tags$td("Niezależność, podobne kształty rozkładów w grupach"),
          tags$td("Post-hoc: test Dunna z korektą")
        ),
        tags$tr(
          tags$td(tags$strong("Spearman")),
          tags$td("Monotoniczność związku"),
          tags$td("Działa na rangach, odporny na outliery")
        )
      )
    ),

    # ========================================================================
    # Testy dla jakosciowych
    # ========================================================================
    lc_h2("ch4-jakosciowe", "Testy dla zmiennych jakościowych"),

    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Metoda"), tags$th("Założenia"), tags$th("Gdy naruszone"))
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$strong("χ² zgodności")),
          tags$td("Oczekiwane ≥ 5 w każdej kategorii"),
          tags$td("Test dwumianowy (2 kat.), symulacja MC")
        ),
        tags$tr(
          tags$td(tags$strong("χ² niezależności")),
          tags$td("Oczekiwane ≥ 5 w każdej komórce"),
          tags$td("Test Fishera (dokładny)")
        ),
        tags$tr(
          tags$td(tags$strong("Test Fishera")),
          tags$td("Niezależność obserwacji"),
          tags$td("Brak — to już metoda dokładna")
        ),
        tags$tr(
          tags$td(tags$strong("Test dwumianowy")),
          tags$td("Niezależność, binarne dane"),
          tags$td("Brak — to metoda dokładna")
        )
      )
    ),

    # ========================================================================
    # Regresja
    # ========================================================================
    lc_h2("ch4-regresja", "Regresja"),

    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Metoda"), tags$th("Założenia"), tags$th("Diagnostyka"), tags$th("Alternatywy"))
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$strong("Regresja liniowa")),
          tags$td("Liniowość, normalność reszt, homoscedastyczność, niezależność reszt, brak współliniowości"),
          tags$td("Reszty vs fitted, Q-Q, Scale-Location, Breusch-Pagan, Durbin-Watson, VIF"),
          tags$td("Transformacje, odporne SE (HC), WLS, GLM, GAM, bootstrap")
        ),
        tags$tr(
          tags$td(tags$strong("Regresja logistyczna")),
          tags$td("Liniowość logitów, niezależność obserwacji, brak współliniowości, wystarczająco dużo zdarzeń na predyktor (reguła EPV ≥ 10)"),
          tags$td("Test Hosmera-Lemeshowa, residuals deviance, VIF"),
          tags$td("Dokładna regresja logistyczna (Firtha), regularyzacja, drzewa decyzyjne")
        )
      )
    ),

    # ========================================================================
    # WIDGET: Interaktywny selektor
    # ========================================================================
    lc_h2("ch4-selektor", "Selektor: mam tę metodę — co sprawdzić?"),

    figure_panel(
      label = "Ryc. 4.1",
      title = "Sprawdzarka założeń",
      fluidRow(
        column(4,
          selectInput("ch4_method", "Metoda:",
            choices = c(
              "Test t jednej próby" = "t_one",
              "Test t niezależny" = "t_ind",
              "Test t sparowany" = "t_paired",
              "ANOVA" = "anova",
              "Korelacja Pearsona" = "pearson",
              "Korelacja Spearmana" = "spearman",
              "Mann-Whitney U" = "mann_whitney",
              "Kruskal-Wallis" = "kruskal",
              "χ² niezależności" = "chi_sq",
              "Test Fishera" = "fisher",
              "Regresja liniowa" = "lm",
              "Regresja logistyczna" = "glm"
            ),
            selected = "t_ind"
          )
        ),
        column(8,
          uiOutput("ch4_method_info")
        )
      )
    ),

    lc_chapter_next(
      num = "05",
      title = "Ściąga",
      lead = "kompaktowa referencja do diagnostyki i alternatyw.",
      target_id = "ch-sciaga"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  method_info <- list(
    t_one = list(
      name = "Test t jednej próby",
      assumptions = c("Dane ilościowe", "Brak silnej skośności i groźnych outlierów"),
      checks = c("Q-Q plot (najpierw)", "shapiro_test() pomocniczo"),
      alternatives = c("Wilcoxon jednej próby: wilcox_test(x ~ 1, mu = ...)"),
      r_code = "rstatix::t_test(data, var ~ 1, mu = wartość)"
    ),
    t_ind = list(
      name = "Test t niezależny",
      assumptions = c("Dane ilościowe", "Brak silnych odchyleń w grupach", "Równe wariancje (lub użyj Welcha)"),
      checks = c("Q-Q per group (najpierw)", "shapiro_test() pomocniczo", "levene_test()"),
      alternatives = c("Welch t (domyślny!): t_test(var.equal = FALSE)", "Mann-Whitney U: wilcox_test()"),
      r_code = "rstatix::t_test(data, var ~ group)"
    ),
    t_paired = list(
      name = "Test t sparowany",
      assumptions = c("Dane ilościowe", "Normalność różnic"),
      checks = c("shapiro_test() na różnicach"),
      alternatives = c("Wilcoxon par znakowych: wilcox_test(paired = TRUE)"),
      r_code = "rstatix::t_test(data, var ~ time, paired = TRUE)"
    ),
    anova = list(
      name = "ANOVA jednoczynnikowa",
      assumptions = c("Normalność reszt w grupach", "Równe wariancje między grupami"),
      checks = c("shapiro_test() per group", "levene_test()"),
      alternatives = c("Welch ANOVA: oneway.test()", "Kruskal-Wallis: kruskal_test()"),
      r_code = "rstatix::anova_test(data, var ~ group)"
    ),
    pearson = list(
      name = "Korelacja Pearsona",
      assumptions = c("Liniowość związku", "Normalność dwuwymiarowa", "Brak ekstremalnych outlierów"),
      checks = c("Scatterplot!", "shapiro_test() na obu zmiennych"),
      alternatives = c("Spearman: cor_test(method='spearman')", "Kendall: cor_test(method='kendall')"),
      r_code = "rstatix::cor_test(data, x, y, method = 'pearson')"
    ),
    spearman = list(
      name = "Korelacja Spearmana",
      assumptions = c("Monotoniczność związku"),
      checks = c("Scatterplot"),
      alternatives = c("Kendall tau (bardziej odporny, wolniejszy)"),
      r_code = "rstatix::cor_test(data, x, y, method = 'spearman')"
    ),
    mann_whitney = list(
      name = "Mann-Whitney U",
      assumptions = c("Niezależność obserwacji", "Dane co najmniej porządkowe"),
      checks = c("Sprawdzenie projektu badawczego"),
      alternatives = c("Test permutacyjny"),
      r_code = "rstatix::wilcox_test(data, var ~ group)"
    ),
    kruskal = list(
      name = "Kruskal-Wallis",
      assumptions = c("Niezależność obserwacji", "Podobne kształty rozkładów w grupach"),
      checks = c("Boxploty, histogramy per grupa"),
      alternatives = c("Test permutacyjny, bootstrap ANOVA"),
      r_code = "rstatix::kruskal_test(data, var ~ group)"
    ),
    chi_sq = list(
      name = "χ² niezależności",
      assumptions = c("Niezależność obserwacji", "Oczekiwane liczności ≥ 5 w każdej komórce"),
      checks = c("chisq.test()$expected — sprawdź wartości"),
      alternatives = c("Test Fishera: fisher.test()", "χ² z MC: chisq.test(simulate.p.value=TRUE)"),
      r_code = "chisq.test(table(var1, var2))"
    ),
    fisher = list(
      name = "Test Fishera",
      assumptions = c("Niezależność obserwacji"),
      checks = c("Brak specjalnych wymagań"),
      alternatives = c("Brak — to metoda dokładna"),
      r_code = "fisher.test(table(var1, var2))"
    ),
    lm = list(
      name = "Regresja liniowa",
      assumptions = c("Liniowość związku", "Normalność reszt", "Homoscedastyczność reszt", "Niezależność reszt", "Brak współliniowości (wieloraka)"),
      checks = c("Reszty vs fitted", "Q-Q reszt", "Scale-Location", "Breusch-Pagan: bptest()", "Durbin-Watson: dwtest()", "VIF: car::vif()"),
      alternatives = c("Transformacja Y lub X", "Odporne SE: sandwich::vcovHC()", "WLS: lm(weights=)", "GLM, GAM, bootstrap"),
      r_code = "model <- lm(y ~ x1 + x2, data = dane); summary(model)"
    ),
    glm = list(
      name = "Regresja logistyczna",
      assumptions = c("Liniowość logitów", "Niezależność obserwacji", "Brak współliniowości", "EPV ≥ 10 (zdarzeń na predyktor)"),
      checks = c("Hosmer-Lemeshow: hoslem.test()", "VIF: car::vif()", "Sprawdź EPV"),
      alternatives = c("Firth logistic: logistf::logistf()", "Regularyzacja: glmnet", "Drzewa decyzyjne"),
      r_code = "model <- glm(y ~ x1 + x2, family = binomial, data = dane)"
    )
  )

  output$ch4_method_info <- renderUI({
    info <- method_info[[input$ch4_method]]
    if (is.null(info)) return(NULL)

    tagList(
      h4(info$name),
      lc_feedback(type = "warning",
        tags$strong("Założenia:"),
        tags$ul(lapply(info$assumptions, tags$li))
      ),
      lc_feedback(type = "info",
        tags$strong("Jak sprawdzić:"),
        tags$ul(lapply(info$checks, function(c) tags$li(tags$code(c))))
      ),
      lc_feedback(type = "ok",
        tags$strong("Alternatywy:"),
        tags$ul(lapply(info$alternatives, tags$li))
      ),
      lc_formula_box(
        tags$strong("Kod R:"),
        tags$pre(style = "margin-top: 5px;", tags$code(info$r_code))
      )
    )
  })
}
