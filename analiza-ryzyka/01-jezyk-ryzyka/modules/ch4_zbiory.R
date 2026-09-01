# ==========================================================================
# ROZDZIAŁ 4: DZIAŁANIA NA ZDARZENIACH
# ==========================================================================

ch4_ui <- lecture_chapter(
  id = "ch-zbiory",
  num = "04",
  title = "Zdarzenia się łączą",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 04 · Język ryzyka",
      num = "04",
      title = "„Lub”, „i” oraz „nie” zmieniają zdarzenie.",
      lead = "W raportach bezpieczeństwa jedno słowo potrafi zmienić licznik.
              Zobaczymy sumę, część wspólną i dopełnienie na stu kontrolach
              korytarza Bananpolu."
    ),

    margin_callout(
      label = "Dwa zdarzenia",
      tags$div("A — podczas kontroli znaleziono skórkę na przejściu."),
      tags$div("B — podczas kontroli posadzka była mokra."),
      color = "wskazowka"
    ),

    lc_h2("ch4-jezyk", "Przetłumacz zdanie na zbiór"),
    lc_p(
      "Zdarzenie A ∪ B zachodzi, gdy wystąpiło A lub B, włącznie z sytuacją,
       gdy wystąpiły oba. Zdarzenie A ∩ B wymaga obu warunków naraz. Dopełnienie
       Aᶜ obejmuje wszystkie wyniki, w których A nie zaszło."
    ),

    lc_formula_box(
      withMathJax("$$P(A\\cup B)=P(A)+P(B)-P(A\\cap B)$$"),
      tags$p("Część wspólną odejmujemy, ponieważ przy dodawaniu została policzona dwa razy.")
    ),

    figure_panel(
      label = "Demonstracja 4.1",
      title = "Dlaczego nie wystarczy dodać P(A) i P(B)?",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          uiOutput("ch4_venn_explanation"),
          actionButton(
            "ch4_venn_next", "Dodaj P(A) i P(B)",
            class = "lc-btn-primary", width = "100%"
          )
        ),
        column(8, zoom_plot_ui("ch4_venn", height = "430px"))
      )
    ),

    lc_h2("ch4-siatka", "Zbuduj dwa zdarzenia na 100 kontrolach"),
    lc_p(
      "Zmieniaj liczebności A i B oraz ich część wspólną. Aplikacja pilnuje, by
       wybrane zbiory mogły zmieścić się w przestrzeni 100 wyników."
    ),

    figure_panel(
      label = "Interakcja 4",
      title = "Suma, iloczyn i dopełnienie zdarzeń",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch4_n_a", "Liczba kontroli ze zdarzeniem A", 0, 80, 30, 1),
          sliderInput("ch4_n_b", "Liczba kontroli ze zdarzeniem B", 0, 80, 20, 1),
          sliderInput("ch4_overlap", "Liczba kontroli z A i B", 0, 20, 8, 1),
          uiOutput("ch4_stats")
        ),
        column(
          8,
          zoom_plot_ui("ch4_event_grid", height = "480px")
        )
      )
    ),

    lc_h2("ch4-pulapka", "Rozłączne nie znaczy niezależne"),
    lc_p(
      "Zdarzenia rozłączne nie mogą zajść razem, więc ich część wspólna jest
       pusta. Zdarzenia niezależne mogą zajść razem, ale informacja o jednym nie
       zmienia prawdopodobieństwa drugiego. Dwa niezerowe zdarzenia rozłączne
       nie są niezależne: gdy A zaszło, wiemy na pewno, że B nie zaszło."
    ),

    lc_feedback(
      type = "warning",
      tags$strong("Pułapka językowa:"),
      " w rachunku prawdopodobieństwa „A lub B” obejmuje także przypadek
        „A i B”, chyba że wyraźnie mówimy o alternatywie wykluczającej."
    ),

    lc_chapter_next(
      num = "05",
      title = "Od prawdopodobieństwa do decyzji",
      lead = "Dwa zdarzenia o podobnej częstości mogą mieć zupełnie inne skutki.",
      target_id = "ch-decyzja"
    )
  )
)

ch4_server <- function(input, output, session) {
  venn_step <- reactiveVal(1L)

  observeEvent(input$ch4_venn_next, {
    next_step <- if (venn_step() >= 4L) 1L else venn_step() + 1L
    venn_step(next_step)
    updateActionButton(
      session,
      "ch4_venn_next",
      label = switch(
        as.character(next_step),
        "1" = "Dodaj P(A) i P(B)",
        "2" = "Odejmij podwójne naliczenie",
        "3" = "Pokaż zdarzenia rozłączne",
        "4" = "Od początku"
      )
    )
  })

  output$ch4_venn_explanation <- renderUI({
    switch(
      as.character(venn_step()),
      "1" = tagList(
        tags$div(class = "lc-eyebrow", "Krok 1 · Dane"),
        tags$h4("Dwa zachodzące na siebie zdarzenia"),
        tags$p("P(A) = 0,70, P(B) = 0,60, a P(A ∩ B) = 0,40."),
        tags$p("Najpierw zaznaczamy oba zbiory bez wykonywania działania.")
      ),
      "2" = tagList(
        tags$div(class = "lc-eyebrow", "Krok 2 · Naiwna suma"),
        tags$h4("Dodajemy całe A i całe B"),
        lc_formula_box(withMathJax("$$0{,}70+0{,}60=1{,}30$$")),
        tags$p("Wynik 1,30 nie może być prawdopodobieństwem. Ciemna część wspólna dostała dwa kolory — została policzona dwa razy.")
      ),
      "3" = tagList(
        tags$div(class = "lc-eyebrow", "Krok 3 · Korekta"),
        tags$h4("Usuwamy jedną kopię części wspólnej"),
        lc_formula_box(withMathJax("$$0{,}70+0{,}60-0{,}40=0{,}90$$")),
        tags$p("Obszar A ∩ B nadal należy do sumy, ale jest w niej liczony tylko raz.")
      ),
      "4" = tagList(
        tags$div(class = "lc-eyebrow", "Wyjątek · Zdarzenia rozłączne"),
        tags$h4("Kiedy samo dodawanie działa?"),
        lc_formula_box(withMathJax("$$0{,}40+0{,}35=0{,}75$$")),
        tags$p("Koła nie zachodzą na siebie, więc P(A ∩ B) = 0. Niczego nie policzyliśmy dwa razy.")
      )
    )
  })

  venn_plot <- reactive({
    circle_points <- function(center_x, center_y, radius, n = 240L) {
      angle <- seq(0, 2 * pi, length.out = n)
      data.frame(
        x = center_x + radius * cos(angle),
        y = center_y + radius * sin(angle)
      )
    }

    step <- venn_step()
    if (step == 4L) {
      circle_a <- circle_points(3.15, 3.25, 1.55)
      circle_b <- circle_points(6.85, 3.25, 1.55)
    } else {
      center_a <- 4.05
      center_b <- 5.95
      radius <- 2.15
      circle_a <- circle_points(center_a, 3.25, radius)
      circle_b <- circle_points(center_b, 3.25, radius)
      half_angle <- acos((center_b - center_a) / (2 * radius))
      overlap <- rbind(
        data.frame(
          x = center_a + radius * cos(seq(-half_angle, half_angle, length.out = 120L)),
          y = 3.25 + radius * sin(seq(-half_angle, half_angle, length.out = 120L))
        ),
        data.frame(
          x = center_b + radius * cos(seq(pi - half_angle, pi + half_angle, length.out = 120L)),
          y = 3.25 + radius * sin(seq(pi - half_angle, pi + half_angle, length.out = 120L))
        )
      )
    }

    plot <- ggplot() +
      annotate(
        "rect",
        xmin = 0.8, xmax = 9.2, ymin = 0.45, ymax = 6.05,
        fill = upwr_panel, colour = upwr_rule, linewidth = 0.7
      )

    blend_with_panel <- function(colour, fraction = 0.52) {
      grDevices::colorRampPalette(c(upwr_panel, colour))(101L)[round(fraction * 100) + 1L]
    }

    if (step == 1L) {
      plot <- plot +
        geom_polygon(data = circle_a, aes(x = x, y = y), fill = NA,
                     colour = upwr_cat[["terakota"]], linewidth = 1.3) +
        geom_polygon(data = circle_b, aes(x = x, y = y), fill = NA,
                     colour = upwr_cat[["niebo"]], linewidth = 1.3)
    } else if (step == 3L) {
      a_fill <- blend_with_panel(upwr_cat[["terakota"]])
      b_fill <- blend_with_panel(upwr_cat[["niebo"]])
      plot <- plot +
        geom_polygon(data = circle_a, aes(x = x, y = y),
                     fill = a_fill, colour = upwr_cat[["terakota"]], linewidth = 1.1) +
        geom_polygon(data = circle_b, aes(x = x, y = y),
                     fill = b_fill, colour = upwr_cat[["niebo"]], linewidth = 1.1) +
        geom_polygon(data = overlap, aes(x = x, y = y),
                     fill = a_fill, colour = upwr_accent, linewidth = 1)
    } else {
      plot <- plot +
        geom_polygon(data = circle_a, aes(x = x, y = y),
                     fill = upwr_cat[["terakota"]], colour = upwr_cat[["terakota"]],
                     alpha = 0.52, linewidth = 1.1) +
        geom_polygon(data = circle_b, aes(x = x, y = y),
                     fill = upwr_cat[["niebo"]], colour = upwr_cat[["niebo"]],
                     alpha = 0.52, linewidth = 1.1)
    }

    label_x <- if (step == 4L) c(3.15, 6.85) else c(2.7, 7.3)
    label_text <- if (step == 4L) c("A\nP(A) = 0,40", "B\nP(B) = 0,35") else
      c("A\nP(A) = 0,70", "B\nP(B) = 0,60")

    plot <- plot +
      annotate("text", x = label_x, y = 3.35, label = label_text,
               fontface = "bold", size = 5, lineheight = 1.15)

    if (step == 2L) {
      plot <- plot + annotate(
        "label", x = 5, y = 3.25,
        label = "A ∩ B = 0,40\nPOLICZONE 2 RAZY",
        fill = "#ffffff", colour = upwr_accent,
        linewidth = 0.4, fontface = "bold", size = 4.3
      )
    } else if (step == 3L) {
      plot <- plot + annotate(
        "label", x = 5, y = 3.25,
        label = "A ∩ B = 0,40\nJEDNO NALICZENIE",
        fill = "#ffffff", colour = upwr_accent,
        linewidth = 0.4, fontface = "bold", size = 4,
        lineheight = 0.9
      )
    }

    bottom_label <- switch(
      as.character(step),
      "1" = "Najpierw odczytaj dane — jeszcze niczego nie dodajemy",
      "2" = "0,70 + 0,60 = 1,30  →  wynik niemożliwy",
      "3" = "1,30 − 0,40 = 0,90  →  część wspólna pozostaje dokładnie raz",
      "4" = "P(A ∩ B) = 0  →  P(A ∪ B) = P(A) + P(B) = 0,75"
    )

    plot +
      annotate("text", x = 5, y = 0.82, label = bottom_label,
               colour = upwr_ink, size = 4.5) +
      coord_equal(xlim = c(0.5, 9.5), ylim = c(0.3, 6.25), expand = FALSE) +
      labs(
        subtitle = paste("Krok", step, "z 4"),
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        plot.subtitle = element_text(
          family = "Atkinson Hyperlegible", size = 12,
          colour = upwr_ink_soft, lineheight = 1.25,
          margin = margin(b = 10)
        )
      )
  })

  zoom_plot_server(
    "ch4_venn",
    venn_plot,
    alt = paste(
      "Czterostopniowy diagram Venna pokazujący podwójne policzenie części",
      "wspólnej oraz szczególny przypadek zdarzeń rozłącznych."
    )
  )

  observeEvent(list(input$ch4_n_a, input$ch4_n_b), {
    req(input$ch4_n_a, input$ch4_n_b)
    lower <- max(0L, input$ch4_n_a + input$ch4_n_b - 100L)
    upper <- min(input$ch4_n_a, input$ch4_n_b)
    current <- input$ch4_overlap
    if (is.null(current)) current <- lower
    value <- min(max(current, lower), upper)

    updateSliderInput(
      session,
      "ch4_overlap",
      min = lower,
      max = upper,
      value = value
    )
  })

  event_values <- reactive({
    req(input$ch4_n_a, input$ch4_n_b, input$ch4_overlap)
    lower <- max(0L, input$ch4_n_a + input$ch4_n_b - 100L)
    upper <- min(input$ch4_n_a, input$ch4_n_b)
    overlap <- min(max(input$ch4_overlap, lower), upper)
    list(
      n_a = as.integer(input$ch4_n_a),
      n_b = as.integer(input$ch4_n_b),
      overlap = as.integer(overlap)
    )
  })

  output$ch4_stats <- renderUI({
    values <- event_values()
    union <- values$n_a + values$n_b - values$overlap

    lc_stat_grid(
      lc_stat_box("P(A ∩ B)", format_probability_pl(values$overlap / 100),
                  color = upwr_cat[["wrzos"]]),
      lc_stat_box("P(A ∪ B)", format_probability_pl(union / 100),
                  color = upwr_accent),
      lc_stat_box("P(Aᶜ)", format_probability_pl(1 - values$n_a / 100),
                  color = upwr_cat[["szalwia"]]),
      lc_stat_box("Ani A, ani B", format_probability_pl((100 - union) / 100),
                  color = upwr_reference),
      columns = 2
    )
  })

  event_grid_plot <- reactive({
    values <- event_values()
    data <- build_event_grid(
      total = 100L,
      n_a = values$n_a,
      n_b = values$n_b,
      overlap = values$overlap,
      columns = 10L
    )

    ggplot(data, aes(x = column, y = -row, fill = status)) +
      geom_tile(colour = "white", linewidth = 0.7, width = 0.95, height = 0.95) +
      scale_fill_manual(values = c(
        "A i B" = upwr_cat[["wrzos"]],
        "Tylko A" = upwr_cat[["terakota"]],
        "Tylko B" = upwr_cat[["niebo"]],
        "Ani A, ani B" = upwr_reference
      )) +
      coord_equal() +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      labs(
        title = "Sto kontroli korytarza",
        subtitle = "Każdy kwadrat to jeden wynik doświadczenia",
        x = NULL,
        y = NULL,
        fill = NULL
      ) +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom"
      )
  })

  zoom_plot_server(
    "ch4_event_grid",
    event_grid_plot,
    alt = paste(
      "Siatka stu kontroli podzielonych na zdarzenie A, zdarzenie B,",
      "ich część wspólną oraz wyniki nienależące do żadnego zdarzenia."
    )
  )
}
