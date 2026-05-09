# ============================================================================
# ROZDZIAŁ 4: Metoda graficzna
# ============================================================================

ch4_ui <- lecture_chapter(
  id = "ch-graf",
  num = "04",
  title = "Metoda graficzna",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 06 · Optymalizacja",
      num = "04",
      title = "Metoda graficzna.",
      lead = "Dla dwóch zmiennych można narysować obszar dopuszczalny i znaleźć optimum graficznie. To buduje intuicję: optimum zawsze leży w wierzchołku."
    ),

    lc_h2("ch4-obszar", "Obszar dopuszczalny"),
    lc_p("Każde ograniczenie liniowe wycina półpłaszczyznę. Część wspólna wszystkich półpłaszczyzn (plus warunki nieujemności x₁ ≥ 0, x₂ ≥ 0) to obszar dopuszczalny — wielokąt wszystkich rozwiązań spełniających wszystkie ograniczenia jednocześnie. Każdy punkt wewnątrz tego wielokąta jest wykonalnym planem produkcji."),

    lc_h2("ch4-twierdzenie", "Twierdzenie podstawowe programowania liniowego"),
    inline_callout(
      label = "Zapamiętaj",
      color = "uwaga",
      open = TRUE,
      "Jeśli optimum istnieje (i jest skończone), to znajduje się w jednym z wierzchołków obszaru dopuszczalnego. Nie trzeba sprawdzać wszystkich punktów wewnątrz wielokąta — wystarczy obliczyć wartość funkcji celu w wierzchołkach i wybrać najlepszy."
    ),

    lc_h2("ch4-widget", "Wizualizacja problemu piekarni"),
    lc_p("Poniższy widget pokazuje obszar dopuszczalny dla problemu z dwoma zasobami i dwoma produktami. Przesuwaj suwaki cen i limitów zasobów — zobacz, jak zmienia się optimum i jak „obraca się” linia izozysku."),
    figure_panel(
      label = "Ryc. 4.1",
      title = "Obszar dopuszczalny i wierzchołki",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch4_z1", "Zysk z x₁ (chleba)",  min = 5,  max = 60,  value = 30, step = 5),
          sliderInput("ch4_z2", "Zysk z x₂ (bułki)",   min = 5,  max = 60,  value = 25, step = 5),
          sliderInput("ch4_b1", "Zasób A (b₁): 2x₁ + x₂ ≤ b₁",  min = 40, max = 160, value = 100, step = 10),
          sliderInput("ch4_b2", "Zasób B (b₂): x₁ + 2x₂ ≤ b₂",  min = 40, max = 160, value = 90,  step = 10)
        ),
        column(
          8,
          plotOutput("ch4_plot", height = "380px"),
          uiOutput("ch4_stats"),
          uiOutput("ch4_verdict")
        )
      )
    ),

    lc_h2("ch4-co-jesli", "Co się stanie, gdy zmienisz cenę?"),
    lc_p("Jeśli zwiększymy zysk z chleba (z₁), linia izozysku „obraca się” — staje się bardziej stroma. Optimum może przeskoczyć z jednego wierzchołka na sąsiedni. To pokazuje, że optimum jest WRAŻLIWE na zmiany cen — to ważna informacja dla decydenta. W rozdziale 07 (dualizm) zobaczymy, jak tę wrażliwość zmierzyć liczbowo."),

    inline_callout(
      label = "Pułapka",
      color = "uwaga",
      "Metoda graficzna działa tylko dla 2 zmiennych. Dla 3 zmiennych potrzeba rysować wielościan w 3D, dla 10 zmiennych — niemożliwe. Stąd potrzebujemy simpleksu — algorytmu, który systematycznie przechodzi po wierzchołkach w wyższych wymiarach."
    ),

    lc_chapter_next(
      num = "05",
      title = "Twoja decyzja",
      lead = "ćwiczenie z wyboru produkcji",
      target_id = "ch-cwiczenie"
    )
  )
)

ch4_server <- function(input, output, session) {
  ch4_lp <- reactive({
    eco_lp_vertices(
      a1 = 2, a2 = 1, b1 = input$ch4_b1,
      c1 = 1, c2 = 2, b2 = input$ch4_b2,
      z1 = input$ch4_z1, z2 = input$ch4_z2
    )
  })

  output$ch4_plot <- renderPlot({
    b1 <- input$ch4_b1
    b2 <- input$ch4_b2
    z1 <- input$ch4_z1
    z2 <- input$ch4_z2

    x_grid <- seq(0, 90, length.out = 400)
    boundary <- data.frame(
      x = x_grid,
      y = pmin(pmax(0, (b1 - 2 * x_grid) / 1),
               pmax(0, (b2 - x_grid) / 2))
    )
    boundary <- boundary[boundary$y >= 0, ]

    verts <- ch4_lp()
    best  <- verts[which.max(verts$value), ]

    # Linia izozysku przez optimum: z1*x + z2*y = z_max
    z_max <- best$value
    iso_x <- c(0, z_max / z1)
    iso_y <- c(z_max / z2, 0)
    iso_df <- data.frame(x = iso_x, y = iso_y)

    ggplot(boundary, aes(x, y)) +
      geom_area(fill = upwr_seq_burgundy[2], alpha = 0.7) +
      geom_line(color = upwr_accent, linewidth = 1) +
      geom_line(data = iso_df, aes(x, y),
                color = unname(upwr_cat["grafit"]),
                linetype = "dashed", linewidth = 0.7) +
      geom_point(data = verts, aes(x, y),
                 color = upwr_secondary, size = 3) +
      geom_point(data = best, aes(x, y),
                 color = unname(upwr_cat["szalwia"]), size = 5) +
      coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
      labs(x = "x₁ (chleby)", y = "x₂ (bułki)",
           caption = "Linia przerywana — izozysk przechodzący przez optimum.") +
      theme_upwr()
  })

  output$ch4_stats <- renderUI({
    verts <- ch4_lp()
    best  <- verts[which.max(verts$value), ]
    lc_stat_grid(
      lc_stat_box("x₁*",    eco_fmt(best$x, 1),     color = unname(upwr_cat["niebo"])),
      lc_stat_box("x₂*",    eco_fmt(best$y, 1),     color = unname(upwr_cat["szalwia"])),
      lc_stat_box("Z max",  paste0(eco_fmt(best$value, 1), " zł"),
                  color = upwr_accent),
      columns = 3
    )
  })

  output$ch4_verdict <- renderUI({
    verts <- ch4_lp()
    best  <- verts[which.max(verts$value), ]
    # Sortuj wierzchołki dla czytelności listy.
    verts_sorted <- verts[order(verts$x, verts$y), ]
    pieces <- mapply(function(x, y, v) {
      paste0("(", eco_fmt(x, 1), ", ", eco_fmt(y, 1), ") → ",
             eco_fmt(v, 1), " zł")
    }, verts_sorted$x, verts_sorted$y, verts_sorted$value, SIMPLIFY = TRUE)

    msg <- paste0(
      "Optimum: x₁ = ", eco_fmt(best$x, 1),
      ", x₂ = ", eco_fmt(best$y, 1),
      ", zysk = ", eco_fmt(best$value, 1), " zł. ",
      "Wszystkie wierzchołki: ", paste(pieces, collapse = "; "),
      ". Najwyższa wartość — w zaznaczonym wierzchołku."
    )
    lc_feedback(type = "info", msg)
  })
}
