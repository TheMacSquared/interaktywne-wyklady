# ============================================================================
# CHAPTER 7: Cwiczenia praktyczne
# ============================================================================

source(file.path(app_dir, "modules", "ch7_sat.R"), local = TRUE)

ch7_ui <- list(
  id    = "ch-cwiczenia",
  num   = "07",
  title = "Ćwiczenia",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 07 · Regresja",
      num    = "07",
      title  = "Ćwiczenia praktyczne.",
      lead   = "Interpretacja wspolczynnikow, diagnostyka, porownywanie modeli
                i regresja logistyczna na krotkich zadaniach."
    ),

    lc_h2("ch7-instrukcja", "Jak pracować z zadaniami"),

    tagList(
      p(tags$b("Czas trwania:"), " ~ 75-90 minut · ",
        tags$b("Narzędzie:"), " R albo Jamovi"),
      p("Zadania opierają się na danych ", tags$code("dane/caschools.csv"),
        " oraz jednym małym zbiorze symulowanym dla regresji logistycznej.
        Najpierw wykonaj analizę samodzielnie, potem odsłoń rozwiązanie."),
      lc_feedback(type = "info",
        p("Nie chodzi o przepisywanie tabeli. W każdym zadaniu zapisz jedno
          zdanie interpretacji w języku problemu: wynik testu szkolnego,
          dochód okręgu, odsetek uczniów z dotacją do obiadu albo
          prawdopodobieństwo zdania egzaminu.")
      )
    ),

    lc_h2("ch7-liniowa", "Blok 1: regresja liniowa prosta"),

    figure_panel(label = "Ćw. 1", title = "Czy ubóstwo w okręgu przewiduje wynik z czytania?",
      tagList(
        p("Dopasuj model ", tags$code("read ~ lunch"),
          ", gdzie ", tags$code("lunch"), " to procent uczniów z dotacją do obiadu."),
        tags$ol(
          tags$li("Zapisz równanie regresji."),
          tags$li("Zinterpretuj nachylenie."),
          tags$li("Sprawdź R² i oceń, czy model wyjaśnia dużo zmienności.")
        )
      ),
      actionButton("ch7_ans1", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch7_sol1")
    ),

    figure_panel(label = "Ćw. 2", title = "Predykcja i ekstrapolacja",
      tagList(
        p("Użyj modelu z ćwiczenia 1 i przewidź wynik czytania dla okręgu,
          w którym ", tags$code("lunch = 40"), "."),
        p("Następnie policz predykcję dla ", tags$code("lunch = 110"),
          ". Czy druga predykcja ma sens? Uzasadnij.")
      ),
      actionButton("ch7_ans2", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch7_sol2")
    ),

    lc_h2("ch7-jakosc", "Blok 2: jakość i porównanie modeli"),

    figure_panel(label = "Ćw. 3", title = "Który model czytania jest lepszy?",
      tagList(
        p("Porównaj trzy modele dla ", tags$code("read"), ":"),
        tags$ol(
          tags$li(tags$code("read ~ lunch")),
          tags$li(tags$code("read ~ lunch + income")),
          tags$li(tags$code("read ~ lunch + income + english + student_teacher_ratio"))
        ),
        p("Porównaj R², adjusted R², AIC, BIC i RMSE. Który model wybierzesz
          do wyjaśniania, a który do predykcji?")
      ),
      actionButton("ch7_ans3", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch7_sol3")
    ),

    figure_panel(label = "Ćw. 4", title = "Reszty mówią, czy model kłamie",
      tagList(
        p("Dla modelu ", tags$code("read ~ income"),
          " narysuj wykres reszt względem wartości dopasowanych i Q-Q plot."),
        p("Czy widzisz sygnał nieliniowości, obserwacji odstających albo
          problemu z normalnością reszt? Co zrobiłbyś dalej?")
      ),
      actionButton("ch7_ans4", "Pokaż wskazówkę", class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch7_sol4")
    ),

    lc_h2("ch7-wieloraka", "Blok 3: regresja wieloraka"),

    figure_panel(label = "Ćw. 5", title = "Interpretacja ceteris paribus",
      tagList(
        p("Dopasuj model ",
          tags$code("math ~ lunch + income + english + student_teacher_ratio"), "."),
        p("Zinterpretuj współczynnik przy ", tags$code("income"),
          " i porównaj go ze współczynnikiem w modelu prostym ",
          tags$code("math ~ income"), ".")
      ),
      actionButton("ch7_ans5", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch7_sol5")
    ),

    lc_h2("ch7-logistyczna", "Blok 4: regresja logistyczna"),

    figure_panel(label = "Ćw. 6", title = "Od prawdopodobieństwa do decyzji",
      tagList(
        p("Wygeneruj dane jak w kodzie poniżej i dopasuj model logistyczny:"),
        tags$pre(class = "lc-code-block",
          tags$code(
"set.seed(42)
df <- generate_logistic_data(220)
model <- glm(zdal_num ~ godziny_nauki + srednia_ocen,
             data = df, family = binomial)")
        ),
        p("Policz odds ratio, przewidź prawdopodobieństwo zdania dla osoby
          z 22 godzinami nauki i średnią 3.8, a potem porównaj decyzję
          przy progach 0.5 i 0.7.")
      ),
      actionButton("ch7_ans6", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch7_sol6")
    ),

    ch7_sat_ui(),

    lc_h2("ch7-podsumowanie", "Na koniec"),

    lc_feedback(type = "ok",
      tags$ul(
        tags$li("Model najpierw interpretuj w jednostkach danych, dopiero potem przez p-value."),
        tags$li("Nie porównuj modeli tylko po R², gdy różnią się liczbą predyktorów."),
        tags$li("Predykcja poza zakresem danych to ekstrapolacja, nie zwykłe użycie modelu."),
        tags$li("W regresji logistycznej decyzja zależy od progu i kosztu błędów.")
      )
    )
  )
)

ch7_server <- function(input, output, session) {

  ch7_sat_server(input, output, session)

  ch7_show <- function(id) {
    isTruthy(input[[id]]) && input[[id]] > 0
  }

  ch7_metric_row <- function(model, name) {
    g <- broom::glance(model)
    data.frame(
      model = name,
      r2 = g$r.squared,
      adj_r2 = g$adj.r.squared,
      aic = AIC(model),
      bic = BIC(model),
      rmse = sqrt(mean(residuals(model)^2)),
      check.names = FALSE
    )
  }

  ch7_table <- function(df) {
    tags$table(class = "lc-table lc-table-bordered lc-table-striped lc-table-sm",
      tags$thead(
        tags$tr(lapply(names(df), tags$th))
      ),
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          tags$tr(lapply(df[i, ], function(x) tags$td(as.character(x))))
        })
      )
    )
  }

  output$ch7_sol1 <- renderUI({
    if (!ch7_show("ch7_ans1")) return(NULL)

    model <- lm(read ~ lunch, data = .cas_data)
    coefs <- coef(model)
    g <- broom::glance(model)

    lc_feedback(type = "ok",
      p("Równanie: ",
        tags$code(sprintf("read = %.2f %+ .2f * lunch", coefs[1], coefs[2]))),
      p("Interpretacja: wzrost odsetka uczniów z dotacją do obiadu o 1 punkt
        procentowy wiąże się przeciętnie ze zmianą wyniku czytania o ",
        tags$b(sprintf("%.2f", coefs[2])), " punktu."),
      p("R² = ", tags$b(sprintf("%.3f", g$r.squared)),
        ", więc model wyjaśnia około ",
        tags$b(sprintf("%.1f%%", 100 * g$r.squared)),
        " zmienności wyników czytania.")
    )
  })

  output$ch7_sol2 <- renderUI({
    if (!ch7_show("ch7_ans2")) return(NULL)

    model <- lm(read ~ lunch, data = .cas_data)
    pred <- predict(model, newdata = data.frame(lunch = c(40, 110)))
    rng <- range(.cas_data$lunch, na.rm = TRUE)

    lc_feedback(type = "warning",
      p("Dla ", tags$code("lunch = 40"), " predykcja wynosi ",
        tags$b(sprintf("%.1f", pred[1])), " punktu."),
      p("Dla ", tags$code("lunch = 110"), " mechaniczna predykcja wynosi ",
        tags$b(sprintf("%.1f", pred[2])), " punktu, ale to ekstrapolacja."),
      p("W danych ", tags$code("lunch"), " mieści się w zakresie ",
        tags$b(sprintf("%.1f-%.1f", rng[1], rng[2])),
        ". Model nie został nauczony na wartościach powyżej tego zakresu.")
    )
  })

  output$ch7_sol3 <- renderUI({
    if (!ch7_show("ch7_ans3")) return(NULL)

    m1 <- lm(read ~ lunch, data = .cas_data)
    m2 <- lm(read ~ lunch + income, data = .cas_data)
    m3 <- lm(read ~ lunch + income + english + student_teacher_ratio, data = .cas_data)

    metrics <- rbind(
      ch7_metric_row(m1, "lunch"),
      ch7_metric_row(m2, "lunch + income"),
      ch7_metric_row(m3, "lunch + income + english + STR")
    )
    metrics[, -1] <- lapply(metrics[, -1], function(x) round(x, 3))

    best_aic <- metrics$model[which.min(metrics$aic)]
    best_bic <- metrics$model[which.min(metrics$bic)]

    tagList(
      ch7_table(metrics),
      lc_feedback(type = "ok",
        p("Najniższy AIC: ", tags$b(best_aic), ". Najniższy BIC: ",
          tags$b(best_bic), "."),
        p("Jeśli AIC i BIC wybierają ten sam model, decyzja jest prosta.
          Jeśli się rozchodzą, AIC częściej premiuje predykcję, BIC mocniej
          chroni prostotę interpretacji.")
      )
    )
  })

  output$ch7_sol4 <- renderUI({
    if (!ch7_show("ch7_ans4")) return(NULL)

    model <- lm(read ~ income, data = .cas_data)
    g <- broom::glance(model)

    lc_feedback(type = "info",
      p("W R zacznij od:"),
      tags$pre(class = "lc-code-block",
        tags$code(
"model <- lm(read ~ income, data = caschools)
plot(model, which = 1)  # reszty vs dopasowane
plot(model, which = 2)  # Q-Q plot")
      ),
      p("W tym modelu R² = ", tags$b(sprintf("%.3f", g$r.squared)),
        ", ale sama liczba nie wystarczy. Jeśli na wykresie reszt widzisz łuk,
        rozważ transformację dochodu, składnik nieliniowy albo porównanie
        z modelem wielorakim.")
    )
  })

  output$ch7_sol5 <- renderUI({
    if (!ch7_show("ch7_ans5")) return(NULL)

    simple <- lm(math ~ income, data = .cas_data)
    multi <- lm(math ~ lunch + income + english + student_teacher_ratio, data = .cas_data)
    simple_income <- coef(simple)[["income"]]
    multi_income <- coef(multi)[["income"]]
    coefs <- broom::tidy(multi)
    coefs$estimate <- round(coefs$estimate, 3)
    coefs$std.error <- round(coefs$std.error, 3)
    coefs$statistic <- round(coefs$statistic, 2)
    coefs$p.value <- signif(coefs$p.value, 3)

    tagList(
      ch7_table(coefs[, c("term", "estimate", "std.error", "statistic", "p.value")]),
      lc_feedback(type = "ok",
        p("W modelu prostym wzrost dochodu o 1 tys. USD wiąże się ze zmianą
          wyniku matematyki o ", tags$b(sprintf("%.2f", simple_income)),
          " punktu."),
        p("W modelu wielorakim, przy stałych wartościach lunch, english i STR,
          analogiczny współczynnik wynosi ",
          tags$b(sprintf("%.2f", multi_income)), "."),
        p("Różnica między tymi liczbami to właśnie sens kontroli zmiennych:
          część związku dochodu z wynikiem była współdzielona z innymi predyktorami.")
      )
    )
  })

  output$ch7_sol6 <- renderUI({
    if (!ch7_show("ch7_ans6")) return(NULL)

    set.seed(42)
    df <- generate_logistic_data(220)
    model <- glm(zdal_num ~ godziny_nauki + srednia_ocen, data = df, family = binomial)
    ors <- exp(coef(model))
    prob <- predict(model,
      newdata = data.frame(godziny_nauki = 22, srednia_ocen = 3.8),
      type = "response"
    )
    decision_05 <- if (prob >= 0.5) "Tak" else "Nie"
    decision_07 <- if (prob >= 0.7) "Tak" else "Nie"

    or_df <- data.frame(
      parametr = names(ors),
      OR = round(as.numeric(ors), 3),
      check.names = FALSE
    )

    tagList(
      ch7_table(or_df),
      lc_feedback(type = "ok",
        p("Przewidywane prawdopodobieństwo zdania dla 22 godzin nauki i
          średniej 3.8 wynosi ", tags$b(sprintf("%.1f%%", 100 * prob)), "."),
        p("Decyzja przy progu 0.5: ", tags$b(decision_05),
          ". Decyzja przy progu 0.7: ", tags$b(decision_07), "."),
        p("Wyższy próg zmniejsza liczbę fałszywych alarmów pozytywnych,
          ale może zwiększyć liczbę przypadków, w których nie rozpoznamy osoby,
          która faktycznie zda.")
      )
    )
  })
}
