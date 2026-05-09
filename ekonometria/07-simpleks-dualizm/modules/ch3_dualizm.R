# ============================================================================
# ROZDZIAŁ 3: Dualizm
# ============================================================================

ch3_ui <- lecture_chapter(
  id = "ch-dualizm",
  num = "03",
  title = "Dualizm",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 07 · Dualizm",
      num = "03",
      title = "Każde LP ma bliźniaka.",
      lead = "Każde zadanie LP ma swojego bliźniaka — zadanie dualne. Brzmi abstrakcyjnie, ale interpretacja jest bardzo praktyczna: dual to ceny zasobów. Mówią: ile zarobimy więcej, jeśli dostaniemy o 1 więcej kg mąki?"
    ),

    lc_h2("ch3-primal-dual", "Primal i dual"),
    lc_formula_box(
      withMathJax(helpText("$$\\text{Primal:}\\quad \\max\\, c^T x \\quad \\text{przy} \\quad Ax \\le b,\\; x \\ge 0$$")),
      withMathJax(helpText("$$\\text{Dual:}\\quad \\min\\, b^T y \\quad \\text{przy} \\quad A^T y \\ge c,\\; y \\ge 0$$"))
    ),
    lc_p("Każdemu OGRANICZENIU primal odpowiada ZMIENNA dualna. Każdej zmiennej primal — ograniczenie dualne. Twierdzenie o dualności mówi, że optymalna wartość celu primal i dual są równe — a zmienne dualne w optimum dają ceny zasobów."),

    lc_h2("ch3-interpretacja", "Co to znaczy ekonomicznie?"),
    lc_p("Zmienna dualna y_i (cena dualna i-tego ograniczenia) odpowiada na pytanie: o ile wzrośnie maksymalna wartość celu, jeśli prawą stronę ograniczenia b_i zwiększymy o 1 jednostkę? Inaczej: ile jestem skłonny zapłacić za dodatkową jednostkę zasobu? To NIE jest cena rynkowa zasobu — to wewnętrzna, kalkulacyjna wartość, jaką zasób ma w MOIM problemie."),

    lc_h2("ch3-dwa-typy", "Dwa typy zasobów"),
    figure_panel(
      label = "Tabela 3.1",
      title = "Cena dualna a status ograniczenia",
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Status zasobu"),
          tags$th("Luz s_i"),
          tags$th("Cena dualna y_i"),
          tags$th("Interpretacja")
        )),
        tags$tbody(
          tags$tr(tags$td(tags$strong("Wąskie gardło")),
                  tags$td("s_i = 0 (aktywne)"),
                  tags$td("y_i > 0"),
                  tags$td("Każda dodatkowa jednostka zwiększa zysk o y_i.")),
          tags$tr(tags$td(tags$strong("Zasób z rezerwą")),
                  tags$td("s_i > 0 (nieaktywne)"),
                  tags$td("y_i = 0"),
                  tags$td("Dodatkowa jednostka nie pomoże — i tak zostaje rezerwa."))
        )
      )
    ),

    lc_h2("ch3-widget", "Cena dualna w działaniu"),
    lc_p("Wracamy do piekarni: max Z = 30·x₁ + 25·x₂ przy 2x₁ + x₂ ≤ b₁ (mąka) i x₁ + 2x₂ ≤ 90 (piec). Przesuwaj zasób mąki b₁ — zobacz, jak zmienia się maksymalny zysk Z* i lokalne nachylenie krzywej (= cena dualna mąki)."),
    figure_panel(
      label = "Ryc. 3.1",
      title = "Z*(b₁) — zysk jako funkcja zasobu mąki",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch3_b1", "Ile mamy mąki b₁ (kg)",
                      min = 50, max = 200, value = 100, step = 5)
        ),
        column(
          8,
          plotOutput("ch3_plot", height = "340px"),
          uiOutput("ch3_stats"),
          uiOutput("ch3_verdict")
        )
      )
    ),

    lc_h2("ch3-zastosowanie", "Po co nam to w praktyce?"),
    tags$ul(
      tags$li(tags$strong("Wycena ograniczeń:"), " wiemy, ile maksymalnie warto zapłacić za dodatkowy zasób — cena dualna to górna granica racjonalnej ceny zakupu."),
      tags$li(tags$strong("Identyfikacja wąskich gardeł:"), " zmienne dualne y_i > 0 wskazują, gdzie inwestować, żeby podnieść zysk."),
      tags$li(tags$strong("Analiza wrażliwości:"), " cena dualna mówi, jak prognoza zysku zmieni się przy zmianie zasobu — bez ponownego rozwiązywania całego LP.")
    ),

    inline_callout(
      label = "Pułapka",
      color = "uwaga",
      "Cena dualna jest LOKALNA — działa w pewnym zakresie zmiany b. Po przekroczeniu progu zmienia się struktura optimum (inny wierzchołek staje się rozwiązaniem), a cena dualna może spaść do zera albo zmienić wartość. Nie ekstrapoluj liniowo „kupię 100 dodatkowych kg mąki po cenie dualnej 5 zł” — działa to tylko w okolicy aktualnego rozwiązania."
    ),

    lc_chapter_next("04", "Czytanie wyników solvera", "ćwiczenie z analizy raportu", "ch-cwiczenie")
  )
)

ch3_server <- function(input, output, session) {

  # Z*(b1) dla zadania piekarni: max 30 x1 + 25 x2,
  # 2 x1 + x2 <= b1, x1 + 2 x2 <= 90, x1,x2 >= 0
  ch3_zstar <- function(b1) {
    v <- eco_lp_vertices(a1 = 2, a2 = 1, b1 = b1,
                         c1 = 1, c2 = 2, b2 = 90,
                         z1 = 30, z2 = 25)
    max(v$value, na.rm = TRUE)
  }

  ch3_curve <- reactive({
    bs <- seq(50, 200, by = 1)
    zs <- vapply(bs, ch3_zstar, numeric(1))
    data.frame(b1 = bs, z = zs)
  })

  # Lokalne nachylenie ≈ cena dualna mąki przy bieżącym b1
  ch3_dual <- reactive({
    b1 <- input$ch3_b1
    eps <- 0.5
    z_plus  <- ch3_zstar(b1 + eps)
    z_minus <- ch3_zstar(max(b1 - eps, 1))
    (z_plus - z_minus) / (2 * eps)
  })

  output$ch3_plot <- renderPlot({
    df <- ch3_curve()
    b1 <- input$ch3_b1
    z_now <- ch3_zstar(b1)
    ggplot(df, aes(b1, z)) +
      geom_line(color = upwr_accent, linewidth = 1.2) +
      geom_vline(xintercept = b1, linetype = "dashed",
                 color = unname(upwr_cat["grafit"])) +
      geom_point(data = data.frame(b1 = b1, z = z_now),
                 aes(b1, z), color = upwr_accent, size = 4) +
      labs(x = "b₁ — zasób mąki (kg)",
           y = "Z*(b₁) — maksymalny zysk (zł)") +
      theme_upwr()
  })

  output$ch3_stats <- renderUI({
    b1 <- input$ch3_b1
    z_now <- ch3_zstar(b1)
    y1 <- ch3_dual()
    lc_stat_grid(
      lc_stat_box("b₁ (mąka)",      paste0(b1, " kg"),
                  color = unname(upwr_cat["niebo"])),
      lc_stat_box("Z* (zysk max)",  paste0(eco_fmt(z_now, 0), " zł"),
                  color = upwr_accent),
      lc_stat_box("y₁ (cena dualna mąki)", paste0(eco_fmt(y1, 2), " zł/kg"),
                  color = unname(upwr_cat["szalwia"])),
      columns = 3
    )
  })

  output$ch3_verdict <- renderUI({
    b1 <- input$ch3_b1
    z_now <- ch3_zstar(b1)
    y1 <- ch3_dual()
    if (y1 > 0.01) {
      lc_feedback(
        type = "info",
        p("Przy mące b₁ = ", b1, " kg, maksymalny zysk wynosi ",
          tags$strong(paste0(eco_fmt(z_now, 0), " zł")),
          ". Cena dualna mąki: ", tags$strong(paste0(eco_fmt(y1, 2), " zł/kg")), "."),
        p("Każdy dodatkowy 1 kg mąki podniesie zysk o ", eco_fmt(y1, 2),
          " zł — DOPÓKI nie staniemy się ograniczeni innym zasobem (wtedy cena dualna spadnie albo zniknie).")
      )
    } else {
      lc_feedback(
        type = "warning",
        p("Przy mące b₁ = ", b1, " kg, maksymalny zysk wynosi ",
          tags$strong(paste0(eco_fmt(z_now, 0), " zł")),
          ". Cena dualna mąki: ", tags$strong("≈ 0 zł/kg"), "."),
        p("Mąka NIE jest już wąskim gardłem — mamy jej rezerwę. Wąskim gardłem stał się piec. Dokładanie mąki nic nie zmieni; trzeba inwestować w piec.")
      )
    }
  })
}
