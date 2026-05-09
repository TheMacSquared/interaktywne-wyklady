# ============================================================================
# ROZDZIAŁ 2: Postać klasyczna i standardowa
# ============================================================================

ch2_ui <- lecture_chapter(
  id = "ch-postac",
  num = "02",
  title = "Postać klasyczna i standardowa",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 06 · Optymalizacja",
      num = "02",
      title = "Postać klasyczna i standardowa.",
      lead = "Solver komputerowy (np. simpleks) wymaga, żeby zadanie było zapisane w określonej formie. Postać standardowa zamienia nierówności na równości — to klucz do automatyzacji."
    ),

    lc_h2("ch2-klasyczna", "Postać klasyczna"),
    lc_p("Najprostszy zapis problemu maksymalizacji ma wszystkie ograniczenia jako nierówności typu „nie więcej niż”. Macierzowo:"),
    lc_formula_box(
      withMathJax(helpText("$$\\max\\, c^{T} x \\quad \\text{przy} \\quad Ax \\le b, \\; x \\ge 0$$"))
    ),
    lc_p("To jest naturalny zapis, gdy patrzymy na zasoby: „mam maksymalnie tyle mąki”, „maksymalnie tyle godzin”."),

    lc_h2("ch2-standardowa", "Postać standardowa — dodaj zmienne dopełniające"),
    lc_p("Każdą nierówność typu ax + by ≤ c zamieniamy na równość, dodając zmienną dopełniającą s ≥ 0: ax + by + s = c. Zmienna s reprezentuje niewykorzystany zasób — to, co zostało po produkcji."),
    lc_formula_box(
      withMathJax(helpText("$$0.5x_1 + 0.2x_2 + s_1 = 30$$")),
      withMathJax(helpText("$$0.3x_1 + 0.1x_2 + s_2 = 12$$")),
      withMathJax(helpText("$$x_1, x_2, s_1, s_2 \\ge 0$$"))
    ),
    tags$ul(
      tags$li(tags$strong("s₁:"), " niewykorzystana mąka (kg) — ile mąki zostaje po produkcji."),
      tags$li(tags$strong("s₂:"), " niewykorzystany czas pieca (godziny) — ile godzin pieca pozostaje wolnych.")
    ),

    lc_h2("ch2-przyklad-num", "Mini-przykład numeryczny"),
    lc_p("Załóżmy, że zdecydowaliśmy się produkować 40 chlebów i 50 bułek. Wtedy: zużycie mąki = 0.5·40 + 0.2·50 = 30 kg → s₁ = 0 (mąka wykorzystana w 100%). Zużycie pieca = 0.3·40 + 0.1·50 = 17 godz. > 12 → ROZWIĄZANIE NIEDOPUSZCZALNE."),
    lc_p("Ale gdyby x₁ = 20, x₂ = 30: mąka = 16 kg → s₁ = 14 (14 kg mąki zostaje), piec = 9 godz → s₂ = 3 (3 godz pieca wolne). Rozwiązanie dopuszczalne, zysk = 15·20 + 6·30 = 480 zł."),

    lc_h2("ch2-widget", "Sprawdź dopuszczalność i zysk"),
    figure_panel(
      label = "Ryc. 2.1",
      title = "Twój wybór produkcji",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          numericInput("ch2_x1", "x₁ — liczba chlebów", value = 20, min = 0, max = 100, step = 1),
          numericInput("ch2_x2", "x₂ — liczba bułek",   value = 30, min = 0, max = 200, step = 1)
        ),
        column(
          8,
          uiOutput("ch2_stats"),
          uiOutput("ch2_verdict")
        )
      )
    ),

    inline_callout(
      label = "Wskazówka",
      color = "wskazowka",
      "Zmienne dopełniające s₁, s₂ to niewykorzystane zasoby. W optymalnym rozwiązaniu jedne s będą zerowe (zasób wykorzystany w 100% — „wąskie gardło”), inne dodatnie (zasób ma rezerwę). To, które zasoby są napięte, mówi decydentowi gdzie inwestować."
    ),

    lc_chapter_next(
      num = "03",
      title = "Metoda graficzna",
      lead = "obszar dopuszczalny i wierzchołki",
      target_id = "ch-graf"
    )
  )
)

ch2_server <- function(input, output, session) {
  ch2_check <- reactive({
    x1 <- input$ch2_x1
    x2 <- input$ch2_x2
    if (is.null(x1) || is.null(x2) || is.na(x1) || is.na(x2)) {
      return(NULL)
    }
    flour <- 0.5 * x1 + 0.2 * x2
    oven  <- 0.3 * x1 + 0.1 * x2
    s1    <- 30 - flour
    s2    <- 12 - oven
    profit <- 15 * x1 + 6 * x2
    feasible <- (x1 >= 0) && (x2 >= 0) && (s1 >= -1e-9) && (s2 >= -1e-9)
    list(
      x1 = x1, x2 = x2,
      flour = flour, oven = oven,
      s1 = s1, s2 = s2,
      profit = profit,
      feasible = feasible
    )
  })

  output$ch2_stats <- renderUI({
    r <- ch2_check()
    if (is.null(r)) return(NULL)
    lc_stat_grid(
      lc_stat_box("Mąka",     paste0(eco_fmt(r$flour, 1), " / 30 kg"),
                  caption = paste0("s₁ = ", eco_fmt(max(r$s1, 0), 1), " kg"),
                  color = unname(upwr_cat["niebo"])),
      lc_stat_box("Piec",     paste0(eco_fmt(r$oven, 1), " / 12 godz"),
                  caption = paste0("s₂ = ", eco_fmt(max(r$s2, 0), 1), " godz"),
                  color = unname(upwr_cat["szalwia"])),
      lc_stat_box("Zysk Z",   paste0(eco_fmt(r$profit, 0), " zł"),
                  caption = "15·x₁ + 6·x₂",
                  color = upwr_accent),
      columns = 3
    )
  })

  output$ch2_verdict <- renderUI({
    r <- ch2_check()
    if (is.null(r)) return(NULL)
    if (!r$feasible) {
      breaches <- c()
      if (r$s1 < -1e-9) breaches <- c(breaches, paste0("mąka — przekroczona o ", eco_fmt(-r$s1, 1), " kg"))
      if (r$s2 < -1e-9) breaches <- c(breaches, paste0("piec — przekroczony o ", eco_fmt(-r$s2, 1), " godz"))
      if (r$x1 < 0 || r$x2 < 0) breaches <- c(breaches, "ujemna liczba produktów")
      msg <- paste0("Niedopuszczalne: ", paste(breaches, collapse = "; "), ". Zmniejsz produkcję, żeby zmieścić się w zasobach.")
      return(lc_feedback(type = "warning", msg))
    }
    msg <- paste0(
      "Rozwiązanie dopuszczalne. Produkcja: ", r$x1, " chlebów i ", r$x2, " bułek. ",
      "Zużyto ", eco_fmt(r$flour, 1), " kg mąki (zostaje ", eco_fmt(r$s1, 1),
      " kg), ", eco_fmt(r$oven, 1), " godz pieca (zostaje ", eco_fmt(r$s2, 1),
      " godz). Zysk: ", eco_fmt(r$profit, 0), " zł."
    )
    lc_feedback(type = "ok", msg)
  })
}
