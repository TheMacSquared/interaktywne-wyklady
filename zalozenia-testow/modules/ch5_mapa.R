# ============================================================================
# CHAPTER 5: Mapa metod - kompletna tablica zalozenia -> alternatywa
# ============================================================================

ch5_ui <- tabPanel("5. Mapa metod",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Zbierzmy wszystko w jedn\u0105 map\u0119: ka\u017cda metoda, jej za\u0142o\u017cenia
       i co robi\u0107, gdy s\u0105 naruszone."
    ),

    div(class = "section-title", "Kompletna mapa: metoda \u2192 za\u0142o\u017cenia \u2192 alternatywa"),

    # ========================================================================
    # Testy parametryczne
    # ========================================================================
    div(class = "section-title", "Testy parametryczne"),

    tags$table(class = "table table-bordered table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Metoda"), tags$th("Za\u0142o\u017cenia"), tags$th("Jak sprawdzi\u0107"),
                tags$th("Gdy naruszone \u2192 alternatywa"))
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$strong("Test t jednej pr.")),
          tags$td("Normalno\u015b\u0107 danych (lub n > 30)"),
          tags$td("Q-Q plot, Shapiro-Wilk"),
          tags$td("Wilcoxon jednej pr\u00f3by")
        ),
        tags$tr(
          tags$td(tags$strong("Test t niezale\u017cny")),
          tags$td("Normalno\u015b\u0107 w grupach, r\u00f3wne wariancje"),
          tags$td("Shapiro + Levene"),
          tags$td("Welch t (nier\u00f3wne war.), Mann-Whitney U (brak norm.)")
        ),
        tags$tr(
          tags$td(tags$strong("Test t parowy")),
          tags$td("Normalno\u015b\u0107 r\u00f3\u017cnic"),
          tags$td("Shapiro na r\u00f3\u017cnicach"),
          tags$td("Wilcoxon par znakowych")
        ),
        tags$tr(
          tags$td(tags$strong("ANOVA")),
          tags$td("Normalno\u015b\u0107 reszt, r\u00f3wne wariancje"),
          tags$td("Shapiro + Levene"),
          tags$td("Welch ANOVA (war.), Kruskal-Wallis (norm.)")
        ),
        tags$tr(
          tags$td(tags$strong("Pearson")),
          tags$td("Liniowo\u015b\u0107, normalno\u015b\u0107 2D, brak outlier\u00f3w"),
          tags$td("Scatterplot, Q-Q obu zmiennych"),
          tags$td("Spearman (monotoniczny), Kendall (odporny)")
        )
      )
    ),

    # ========================================================================
    # Testy nieparametryczne
    # ========================================================================
    div(class = "section-title", "Testy nieparametryczne"),

    tags$table(class = "table table-bordered table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Metoda"), tags$th("Za\u0142o\u017cenia"), tags$th("Uwagi"))
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$strong("Wilcoxon / Mann-Whitney")),
          tags$td("Niezale\u017cno\u015b\u0107 obserwacji, symetryczne rozk\u0142ady (dla mediany)"),
          tags$td("Nie wymaga normalno\u015bci. Testuje r\u00f3\u017cnic\u0119 rozk\u0142ad\u00f3w, nie \u015brednich.")
        ),
        tags$tr(
          tags$td(tags$strong("Kruskal-Wallis")),
          tags$td("Niezale\u017cno\u015b\u0107, podobne kszta\u0142ty rozk\u0142ad\u00f3w w grupach"),
          tags$td("Post-hoc: test Dunna z korekt\u0105")
        ),
        tags$tr(
          tags$td(tags$strong("Spearman")),
          tags$td("Monotoniczno\u015b\u0107 zwi\u0105zku"),
          tags$td("Dzia\u0142a na rangach, odporny na outliery")
        )
      )
    ),

    # ========================================================================
    # Testy dla jakosciowych
    # ========================================================================
    div(class = "section-title", "Testy dla zmiennych jako\u015bciowych"),

    tags$table(class = "table table-bordered table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Metoda"), tags$th("Za\u0142o\u017cenia"), tags$th("Gdy naruszone"))
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$strong("\u03c7\u00b2 zgodno\u015bci")),
          tags$td("Oczekiwane \u2265 5 w ka\u017cdej kategorii"),
          tags$td("Test dwumianowy (2 kat.), symulacja MC")
        ),
        tags$tr(
          tags$td(tags$strong("\u03c7\u00b2 niezale\u017cno\u015bci")),
          tags$td("Oczekiwane \u2265 5 w ka\u017cdej kom\u00f3rce"),
          tags$td("Test Fishera (dok\u0142adny)")
        ),
        tags$tr(
          tags$td(tags$strong("Test Fishera")),
          tags$td("Niezale\u017cno\u015b\u0107 obserwacji"),
          tags$td("Brak \u2014 to ju\u017c metoda dok\u0142adna")
        ),
        tags$tr(
          tags$td(tags$strong("Test dwumianowy")),
          tags$td("Niezale\u017cno\u015b\u0107, binarne dane"),
          tags$td("Brak \u2014 to metoda dok\u0142adna")
        )
      )
    ),

    # ========================================================================
    # Regresja
    # ========================================================================
    div(class = "section-title", "Regresja"),

    tags$table(class = "table table-bordered table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Metoda"), tags$th("Za\u0142o\u017cenia"), tags$th("Diagnostyka"), tags$th("Alternatywy"))
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$strong("Regresja liniowa")),
          tags$td("Liniowo\u015b\u0107, normalno\u015b\u0107 reszt, homoscedastyczno\u015b\u0107, niezale\u017cno\u015b\u0107 reszt, brak wsp\u00f3\u0142liniowo\u015bci"),
          tags$td("Reszty vs fitted, Q-Q, Scale-Location, Breusch-Pagan, Durbin-Watson, VIF"),
          tags$td("Transformacje, odporne SE (HC), WLS, GLM, GAM, bootstrap")
        ),
        tags$tr(
          tags$td(tags$strong("Regresja logistyczna")),
          tags$td("Liniowo\u015b\u0107 logit\u00f3w, niezale\u017cno\u015b\u0107 obserwacji, brak wsp\u00f3\u0142liniowo\u015bci, wystarczaj\u0105co du\u017co zdarze\u0144 na predyktor (regu\u0142a EPV \u2265 10)"),
          tags$td("Test Hosmera-Lemeshowa, residuals deviance, VIF"),
          tags$td("Dok\u0142adna regresja logistyczna (Firtha), regularyzacja, drzewa decyzyjne")
        )
      )
    ),

    # ========================================================================
    # WIDGET: Interaktywny selektor
    # ========================================================================
    div(class = "section-title", "Selektor: mam t\u0119 metod\u0119 \u2014 co sprawdzi\u0107?"),

    div(class = "widget-block",
      h4("Sprawdzarka za\u0142o\u017ce\u0144"),
      fluidRow(
        column(4,
          selectInput("ch5_method", "Metoda:",
            choices = c(
              "Test t jednej pr\u00f3by" = "t_one",
              "Test t niezale\u017cny" = "t_ind",
              "Test t parowy" = "t_paired",
              "ANOVA" = "anova",
              "Korelacja Pearsona" = "pearson",
              "Korelacja Spearmana" = "spearman",
              "Mann-Whitney U" = "mann_whitney",
              "Kruskal-Wallis" = "kruskal",
              "\u03c7\u00b2 niezale\u017cno\u015bci" = "chi_sq",
              "Test Fishera" = "fisher",
              "Regresja liniowa" = "lm",
              "Regresja logistyczna" = "glm"
            ),
            selected = "t_ind"
          )
        ),
        column(8,
          uiOutput("ch5_method_info")
        )
      )
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: kompaktowa \u015bci\u0105ga"),
      actionButton("ch5_next", "Dalej \u2192 6. \u015aci\u0105ga",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch5_server <- function(input, output, session) {

  method_info <- list(
    t_one = list(
      name = "Test t jednej pr\u00f3by",
      assumptions = c("Dane ilo\u015bciowe", "Normalno\u015b\u0107 danych (lub n > 30)"),
      checks = c("Q-Q plot", "shapiro_test()"),
      alternatives = c("Wilcoxon jednej pr\u00f3by: wilcox_test(x ~ 1, mu = ...)"),
      r_code = "rstatix::t_test(data, var ~ 1, mu = wartość)"
    ),
    t_ind = list(
      name = "Test t niezale\u017cny",
      assumptions = c("Dane ilo\u015bciowe", "Normalno\u015b\u0107 w ka\u017cdej grupie (lub n > 30)", "R\u00f3wne wariancje (lub u\u017cyj Welcha)"),
      checks = c("Q-Q per group", "shapiro_test() per group", "levene_test()"),
      alternatives = c("Welch t (domy\u015blny!): t_test(var.equal = FALSE)", "Mann-Whitney U: wilcox_test()"),
      r_code = "rstatix::t_test(data, var ~ group)"
    ),
    t_paired = list(
      name = "Test t parowy",
      assumptions = c("Dane ilo\u015bciowe", "Normalno\u015b\u0107 r\u00f3\u017cnic"),
      checks = c("shapiro_test() na r\u00f3\u017cnicach"),
      alternatives = c("Wilcoxon par znakowych: wilcox_test(paired = TRUE)"),
      r_code = "rstatix::t_test(data, var ~ time, paired = TRUE)"
    ),
    anova = list(
      name = "ANOVA jednoczynnikowa",
      assumptions = c("Normalno\u015b\u0107 reszt w grupach", "R\u00f3wne wariancje mi\u0119dzy grupami"),
      checks = c("shapiro_test() per group", "levene_test()"),
      alternatives = c("Welch ANOVA: oneway.test()", "Kruskal-Wallis: kruskal_test()"),
      r_code = "rstatix::anova_test(data, var ~ group)"
    ),
    pearson = list(
      name = "Korelacja Pearsona",
      assumptions = c("Liniowo\u015b\u0107 zwi\u0105zku", "Normalno\u015b\u0107 dwuwymiarowa", "Brak ekstremalnych outlier\u00f3w"),
      checks = c("Scatterplot!", "shapiro_test() na obu zmiennych"),
      alternatives = c("Spearman: cor_test(method='spearman')", "Kendall: cor_test(method='kendall')"),
      r_code = "rstatix::cor_test(data, x, y, method = 'pearson')"
    ),
    spearman = list(
      name = "Korelacja Spearmana",
      assumptions = c("Monotoniczno\u015b\u0107 zwi\u0105zku"),
      checks = c("Scatterplot"),
      alternatives = c("Kendall tau (bardziej odporny, wolniejszy)"),
      r_code = "rstatix::cor_test(data, x, y, method = 'spearman')"
    ),
    mann_whitney = list(
      name = "Mann-Whitney U",
      assumptions = c("Niezale\u017cno\u015b\u0107 obserwacji", "Dane co najmniej porz\u0105dkowe"),
      checks = c("Sprawdzenie projektu badawczego"),
      alternatives = c("Test permutacyjny"),
      r_code = "rstatix::wilcox_test(data, var ~ group)"
    ),
    kruskal = list(
      name = "Kruskal-Wallis",
      assumptions = c("Niezale\u017cno\u015b\u0107 obserwacji", "Podobne kszta\u0142ty rozk\u0142ad\u00f3w w grupach"),
      checks = c("Boxploty, histogramy per grupa"),
      alternatives = c("Test permutacyjny, bootstrap ANOVA"),
      r_code = "rstatix::kruskal_test(data, var ~ group)"
    ),
    chi_sq = list(
      name = "\u03c7\u00b2 niezale\u017cno\u015bci",
      assumptions = c("Niezale\u017cno\u015b\u0107 obserwacji", "Oczekiwane liczno\u015bci \u2265 5 w ka\u017cdej kom\u00f3rce"),
      checks = c("chisq.test()$expected \u2014 sprawd\u017a warto\u015bci"),
      alternatives = c("Test Fishera: fisher.test()", "\u03c7\u00b2 z MC: chisq.test(simulate.p.value=TRUE)"),
      r_code = "chisq.test(table(var1, var2))"
    ),
    fisher = list(
      name = "Test Fishera",
      assumptions = c("Niezale\u017cno\u015b\u0107 obserwacji"),
      checks = c("Brak specjalnych wymaga\u0144"),
      alternatives = c("Brak \u2014 to metoda dok\u0142adna"),
      r_code = "fisher.test(table(var1, var2))"
    ),
    lm = list(
      name = "Regresja liniowa",
      assumptions = c("Liniowo\u015b\u0107 zwi\u0105zku", "Normalno\u015b\u0107 reszt", "Homoscedastyczno\u015b\u0107 reszt", "Niezale\u017cno\u015b\u0107 reszt", "Brak wsp\u00f3\u0142liniowo\u015bci (wieloraka)"),
      checks = c("Reszty vs fitted", "Q-Q reszt", "Scale-Location", "Breusch-Pagan: bptest()", "Durbin-Watson: dwtest()", "VIF: car::vif()"),
      alternatives = c("Transformacja Y lub X", "Odporne SE: sandwich::vcovHC()", "WLS: lm(weights=)", "GLM, GAM, bootstrap"),
      r_code = "model <- lm(y ~ x1 + x2, data = dane); summary(model)"
    ),
    glm = list(
      name = "Regresja logistyczna",
      assumptions = c("Liniowo\u015b\u0107 logit\u00f3w", "Niezale\u017cno\u015b\u0107 obserwacji", "Brak wsp\u00f3\u0142liniowo\u015bci", "EPV \u2265 10 (zdarze\u0144 na predyktor)"),
      checks = c("Hosmer-Lemeshow: hoslem.test()", "VIF: car::vif()", "Sprawd\u017a EPV"),
      alternatives = c("Firth logistic: logistf::logistf()", "Regularyzacja: glmnet", "Drzewa decyzyjne"),
      r_code = "model <- glm(y ~ x1 + x2, family = binomial, data = dane)"
    )
  )

  output$ch5_method_info <- renderUI({
    info <- method_info[[input$ch5_method]]
    if (is.null(info)) return(NULL)

    tagList(
      h4(info$name),
      div(class = "callout-warning",
        tags$strong("Za\u0142o\u017cenia:"),
        tags$ul(lapply(info$assumptions, tags$li))
      ),
      div(class = "callout-info",
        tags$strong("Jak sprawdzi\u0107:"),
        tags$ul(lapply(info$checks, function(c) tags$li(tags$code(c))))
      ),
      div(class = "callout-success",
        tags$strong("Alternatywy:"),
        tags$ul(lapply(info$alternatives, tags$li))
      ),
      div(class = "formula-box",
        tags$strong("Kod R:"),
        tags$pre(style = "margin-top: 5px;", tags$code(info$r_code))
      )
    )
  })
}
