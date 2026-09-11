# ============================================================================
# ROZDZIAŁ 2: Tablica simpleksowa
# ============================================================================

ch2_ui <- lecture_chapter(
  id = "ch-tablica",
  num = "02",
  title = "Tablica simpleksowa",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 07 · Simpleks",
      num = "02",
      title = "Tablica jako księga rachunkowa.",
      lead = "Tablica simpleksowa to sposób księgowania: w każdym kroku zapisujemy aktualny wierzchołek, wartości zmiennych i wskaźniki, które mówią, dokąd iść dalej."
    ),

    lc_h2("ch2-co-to", "Co zawiera tablica?"),
    tags$ul(
      tags$li(tags$strong("Wiersze zmiennych bazowych"), " (s₁, s₂, …) — aktualnie aktywne ograniczenia, czyli pozycja w obszarze dopuszczalnym = wierzchołek."),
      tags$li(tags$strong("Kolumny zmiennych decyzyjnych"), " (x₁, x₂, …) i ", tags$strong("dopełniających"), " (s₁, s₂, …) — luzów ograniczeń."),
      tags$li(tags$strong("Kolumna RHS"), " — bieżące wartości zmiennych bazowych."),
      tags$li(tags$strong("Wiersz Z"), " — funkcja celu i wskaźniki kierunku poprawy. Najbardziej ujemny wskaźnik wskazuje zmienną wchodzącą.")
    ),

    lc_h2("ch2-pierwsza-tablica", "Pierwsza tablica — start od początku układu"),
    lc_p("Bierzemy zadanie piekarni: max Z = c₁·x₁ + c₂·x₂ przy 2x₁ + x₂ ≤ 100 (mąka) oraz x₁ + 2x₂ ≤ 90 (piec). Startujemy w (0, 0) — zmiennymi bazowymi są dopełnienia s₁ = 100 i s₂ = 90. Zmień współczynniki funkcji celu, żeby zobaczyć, jak zmienia się decyzja simpleksu."),
    figure_panel(
      label = "Tabela 2.1",
      title = "Tablica startowa",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch2_c1", "Współczynnik c₁ (zysk z chleba)", min = 1, max = 12, value = 6, step = 1),
          sliderInput("ch2_c2", "Współczynnik c₂ (zysk z bułek)", min = 1, max = 12, value = 4, step = 1)
        ),
        column(
          8,
          uiOutput("ch2_table"),
          uiOutput("ch2_hint")
        )
      )
    ),

    lc_h2("ch2-iteracja", "Co się dzieje w kolejnym kroku?"),
    lc_p("Po pierwszej iteracji wiersz bazowy wymieniamy: zmienna wchodząca zastępuje zmienną wychodzącą. Wartość funkcji celu rośnie z 0 do c·RHS. Sprawdzamy, czy w nowej tablicy są jeszcze ujemne wskaźniki w wierszu Z. Jeśli tak — kolejna iteracja. Jeśli nie — optimum."),
    figure_panel(
      label = "Tabela 2.2",
      title = "Po pierwszej iteracji",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          actionButton("ch2_iter", "Pokaż kolejną iterację", class = "btn-primary", width = "100%"),
          tags$div(style = "margin-top: 10px; font-size: 0.9em; color: #6b1a26;",
                   "Tablica po wymianie zmiennej bazowej w kolumnie zmiennej wchodzącej.")
        ),
        column(
          8,
          uiOutput("ch2_iter_table"),
          uiOutput("ch2_iter_hint")
        )
      )
    ),

    inline_callout(
      label = "Wskazówka",
      color = "wskazowka",
      "Tablica simpleksowa wygląda księgowo, ale każdy krok ma prostą interpretację geometryczną: skok z jednego wierzchołka na sąsiedni. Excel/Gretl/R robi to za nas — ale zrozumienie, co robi maszyna, jest tym, co odróżnia analityka od użytkownika narzędzia."
    ),

    lc_chapter_next("03", "Dualizm", "ceny zasobów ukryte w problemie", "ch-dualizm")
  )
)

ch2_server <- function(input, output, session) {

  # Pierwsza (startowa) tablica simpleksowa
  output$ch2_table <- renderUI({
    c1 <- input$ch2_c1
    c2 <- input$ch2_c2
    tags$table(
      class = "table",
      tags$thead(tags$tr(
        tags$th("Baza"), tags$th("x₁"), tags$th("x₂"),
        tags$th("s₁"), tags$th("s₂"), tags$th("RHS")
      )),
      tags$tbody(
        tags$tr(tags$td("s₁"), tags$td("2"), tags$td("1"),
                tags$td("1"), tags$td("0"), tags$td("100")),
        tags$tr(tags$td("s₂"), tags$td("1"), tags$td("2"),
                tags$td("0"), tags$td("1"), tags$td("90")),
        tags$tr(tags$td("Z"), tags$td(paste0("−", c1)), tags$td(paste0("−", c2)),
                tags$td("0"), tags$td("0"), tags$td("0"))
      )
    )
  })

  output$ch2_hint <- renderUI({
    c1 <- input$ch2_c1
    c2 <- input$ch2_c2
    if (c1 >= c2) {
      entering <- "x₁"
      coef_in <- c1
      ratio_s1 <- 100 / 2
      ratio_s2 <- 90 / 1
      leaving <- if (ratio_s1 <= ratio_s2) "s₁" else "s₂"
      new_val <- min(ratio_s1, ratio_s2)
      ratio_text <- paste0("dla s₁ to 100/2 = 50, dla s₂ to 90/1 = 90")
    } else {
      entering <- "x₂"
      coef_in <- c2
      ratio_s1 <- 100 / 1
      ratio_s2 <- 90 / 2
      leaving <- if (ratio_s1 <= ratio_s2) "s₁" else "s₂"
      new_val <- min(ratio_s1, ratio_s2)
      ratio_text <- paste0("dla s₁ to 100/1 = 100, dla s₂ to 90/2 = 45")
    }
    z_growth <- coef_in * new_val
    lc_feedback(
      type = "info",
      p("Najbardziej ujemny wskaźnik w wierszu Z to kolumna ", tags$strong(entering),
        " (−", coef_in, "). To jest zmienna wchodząca do bazy."),
      p("Zmienna wychodząca: wybieramy tę z bazowych, dla której iloraz RHS / kolumna ",
        entering, " jest najmniejszy nieujemny — ", ratio_text,
        ". Wybieramy ", tags$strong(leaving), "."),
      p("W kolejnej iteracji ", entering, " wejdzie do bazy z wartością ", new_val,
        ". Funkcja celu wzrośnie z 0 do ", z_growth, " zł.")
    )
  })

  # Druga tablica — pojawia się po kliknięciu „Pokaż kolejną iterację"
  ch2_iter_state <- reactiveVal(FALSE)
  observeEvent(input$ch2_iter, {
    ch2_iter_state(TRUE)
  })

  output$ch2_iter_table <- renderUI({
    if (!isTRUE(ch2_iter_state())) {
      return(tags$div(class = "callout-info",
                      style = "padding: 12px; border: 1px dashed #999; border-radius: 6px;",
                      "Kliknij przycisk po lewej, aby zobaczyć tablicę po jednej iteracji."))
    }
    c1 <- input$ch2_c1
    c2 <- input$ch2_c2
    if (c1 >= c2) {
      # x1 wchodzi, s1 wychodzi (przy domyślnych b: ratio s1 = 50 < s2 = 90)
      # Wiersz x1: dzielimy stary wiersz s1 przez 2 → (1, 0.5, 0.5, 0, 50)
      # Wiersz s2: stary − 1·(nowy x1) → (0, 1.5, −0.5, 1, 40)
      # Wiersz Z:  stary + c1·(nowy x1) → (0, c2_new, c1/2, 0, 50·c1)
      c2_new <- -c2 + c1 * 0.5  # współczynnik przy x2 (ujemny → poprawa)
      z_val <- 50 * c1
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Baza"), tags$th("x₁"), tags$th("x₂"),
          tags$th("s₁"), tags$th("s₂"), tags$th("RHS")
        )),
        tags$tbody(
          tags$tr(tags$td("x₁"), tags$td("1"), tags$td("0.5"),
                  tags$td("0.5"), tags$td("0"), tags$td("50")),
          tags$tr(tags$td("s₂"), tags$td("0"), tags$td("1.5"),
                  tags$td("−0.5"), tags$td("1"), tags$td("40")),
          tags$tr(tags$td("Z"), tags$td("0"), tags$td(eco_fmt(c2_new, 2)),
                  tags$td(eco_fmt(c1 / 2, 2)), tags$td("0"), tags$td(z_val))
        )
      )
    } else {
      c1_new <- -c1 + c2 * 0.5
      z_val <- 45 * c2
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Baza"), tags$th("x₁"), tags$th("x₂"),
          tags$th("s₁"), tags$th("s₂"), tags$th("RHS")
        )),
        tags$tbody(
          tags$tr(tags$td("s₁"), tags$td("1.5"), tags$td("0"),
                  tags$td("1"), tags$td("−0.5"), tags$td("55")),
          tags$tr(tags$td("x₂"), tags$td("0.5"), tags$td("1"),
                  tags$td("0"), tags$td("0.5"), tags$td("45")),
          tags$tr(tags$td("Z"), tags$td(eco_fmt(c1_new, 2)), tags$td("0"),
                  tags$td("0"), tags$td(eco_fmt(c2 / 2, 2)), tags$td(z_val))
        )
      )
    }
  })

  output$ch2_iter_hint <- renderUI({
    if (!isTRUE(ch2_iter_state())) return(NULL)
    c1 <- input$ch2_c1
    c2 <- input$ch2_c2
    if (c1 >= c2) {
      remaining <- -c2 + c1 * 0.5
      z_val <- 50 * c1
      next_var <- "x₂"
    } else {
      remaining <- -c1 + c2 * 0.5
      z_val <- 45 * c2
      next_var <- "x₁"
    }
    if (remaining < 0) {
      lc_feedback(
        type = "info",
        p("Funkcja celu wzrosła z 0 do ", tags$strong(paste0(z_val, " zł")), "."),
        p("W wierszu Z nadal jest ujemny wskaźnik (", eco_fmt(remaining, 2),
          " w kolumnie ", next_var, ") — ", tags$strong("nie jesteśmy jeszcze w optimum."),
          " Kolejna iteracja wprowadzi ", next_var, " do bazy.")
      )
    } else {
      lc_feedback(
        type = "ok",
        p("Funkcja celu wzrosła z 0 do ", tags$strong(paste0(z_val, " zł")), "."),
        p("W wierszu Z nie ma już ujemnych wskaźników — ", tags$strong("to jest optimum."),
          " Algorytm się zatrzymuje.")
      )
    }
  })
}
