# ============================================================================
# CHAPTER 6: Ściąga — podsumowanie regresji
# ============================================================================

ch6_ui <- list(
  id    = "ch-sciaga",
  num   = "06",
  title = "Ściąga",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 06 · Regresja",
      num   = "06",
      title  = "Ściąga.",
      lead   = "Kompaktowe podsumowanie regresji liniowej, wielorakiej
                i logistycznej."
    ),

    lc_h2("ch6-trzy-typy", "Trzy typy regresji"),

    tagList(

    lc_table(
      data.frame(
        c1 = c("Y", "X", "Wzór", "Estymacja", "Dopasowanie", "Interpr. β"),
        c2 = I(list(
          "Ciągła",
          "1 predyktor",
          withMathJax("\\(Y = \\beta_0 + \\beta_1 X + \\varepsilon\\)"),
          "MNK",
          "R², RMSE",
          "zmiana Y na 1 jedn. X"
        )),
        c3 = I(list(
          "Ciągła",
          "k predyktorów",
          withMathJax("\\(Y = \\beta_0 + \\sum \\beta_j X_j + \\varepsilon\\)"),
          "MNK",
          "skorygowane R², AIC, BIC, RMSE",
          "zmiana Y na 1 jedn. Xj (ceteris paribus)"
        )),
        c4 = I(list(
          "Binarna (0/1)",
          "k predyktorów",
          withMathJax("\\(\\ln\\frac{p}{1-p} = \\beta_0 + \\sum \\beta_j X_j\\)"),
          "MNW (największa wiarygodność)",
          "AIC, BIC, dokładność",
          withMathJax("\\(OR = e^\\beta\\)")
        ))
      ),
      cols = list(
        lc_col("c1", "", "row"),
        lc_col("c2", "Liniowa prosta", "text"),
        lc_col("c3", "Wieloraka", "text"),
        lc_col("c4", "Logistyczna", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    ),

    lc_h2("ch6-metryki", "Metryki porównawcze"),

    tagList(

    lc_table(
      data.frame(
        c1 = I(list(
          withMathJax("\\(R^2\\)"),
          withMathJax("\\(R^2_{adj}\\)"),
          "AIC",
          "BIC",
          "RMSE"
        )),
        c2 = I(list(
          withMathJax("\\(1 - SS_{res}/SS_{tot}\\)"),
          withMathJax("\\(1 - \\frac{(1-R^2)(n-1)}{n-k-1}\\)"),
          withMathJax("\\(-2\\ln L + 2(k + 2)\\)"),
          withMathJax("\\(-2\\ln L + (k + 2)\\ln n\\)"),
          withMathJax("\\(\\sqrt{\\frac{1}{n}\\sum e_i^2}\\)")
        )),
        c3 = c("↑ lepiej", "↑ lepiej", "↓ lepiej", "↓ lepiej", "↓ lepiej"),
        c4 = c(
          "Nie maleje po dodaniu predyktora; nie porównuj nim modeli o różnej złożoności",
          "Karze za zbędne predyktory; bezpieczniejsze niż R²",
          "Lepszy do predykcji; łagodniejsza kara",
          "Silniejsza kara za parametry; preferuje prostsze modele",
          "W jednostkach Y; intuicyjne"
        )
      ),
      cols = list(
        lc_col("c1", "Metryka", "row"),
        lc_col("c2", "Wzór", "text"),
        lc_col("c3", "Kierunek", "text"),
        lc_col("c4", "Uwagi", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    ),

    lc_h2("ch6-interpretacja", "Interpretacja współczynników"),

    tagList(

    lc_note("Interpretacja",
      p("Regresja liniowa: ", withMathJax("\\(\\beta_1 = 0.5\\)"),
        " oznacza wzrost Y o 0.5 przy wzroście X o 1 (ceteris paribus)."),
      p("Regresja logistyczna: ", withMathJax("\\(\\beta_1 = 0.5 \\Rightarrow OR = e^{0.5} = 1.65\\)"),
        " oznacza: wzrost X o 1 zwiększa szanse sukcesu 1.65-krotnie."),
      p("Istotność współczynników: p poniżej przyjętego poziomu istotności (zwykle 0.05) dla ", withMathJax("\\(\\beta_j\\)"),
        " oznacza, że predyktor ", withMathJax("\\(X_j\\)"),
        " jest istotnie powiązany z Y przy kontroli pozostałych. W danych obserwacyjnych nie dowodzi to wpływu przyczynowego.")
    ),

    ),

    lc_h2("ch6-kiedy", "Kiedy która regresja?"),

    tagList(

    tagList(
      tags$ul(
        tags$li("Y ciągła, 1 predyktor → ", gloss("regresja prosta", "regresja liniowa prosta")),
        tags$li("Y ciągła, wiele predyktorów → ", gloss("regresja wieloraka")),
        tags$li("Y binarna (0/1) → ", gloss("regresja logistyczna")),
        tags$li("Y porządkowa → regresja porządkowa (ordered logit)"),
        tags$li("Y licznikowa → regresja Poissona")
      )
    ),

    ),

    figure_panel(
      label = "Ryc. 6.1", title = "Mini-drzewko wyboru modelu regresyjnego",
      full_width = TRUE,
      lc_toolbar(
        selectInput("ch6_tree_y", "Jaka jest zmienna zależna Y?",
            choices = c(
              "Ilościowa / ciągła" = "continuous",
              "Binarna 0/1" = "binary",
              "Licznikowa" = "count",
              "Porządkowa" = "ordinal"
            )
          ),
        selectInput("ch6_tree_x", "Ile predyktorów?",
            choices = c("Jeden" = "one", "Wiele" = "many")
          ),
        selectInput("ch6_tree_goal", "Główny cel",
            choices = c(
              "Interpretacja efektów" = "explain",
              "Predykcja nowych obserwacji" = "predict"
            )
          ),
        checkboxInput("ch6_tree_nonlinear", "Podejrzewam nieliniowość / przeuczenie", value = FALSE),
        lc_readouts(uiOutput("ch6_tree_info"))
      ),
      lc_plot("ch6_tree_plot", max_height = "310px"),
      uiOutput("ch6_tree_note")
    ),

    lc_h2("ch6-pulapki", "Typowe pułapki"),

    tagList(

    lc_warn("Pułapki",
      tags$ul(
        tags$li("Ekstrapolacja: model działa w zakresie ", gloss("zbiór treningowy", "danych treningowych"), ". Predykcja poza tym zakresem jest ryzykowna."),
        tags$li("Korelacja predyktorów: silna korelacja między X1 i X2 (", gloss("współliniowość"), ") zawyża SE i utrudnia interpretację."),
        tags$li(gloss("przeuczenie", "Przeuczenie"), ": więcej zmiennych daje wyższe R², ale może pogorszyć uogólnianie na nowe dane. Porównuj modele skorygowanym R², AIC lub BIC."),
        tags$li("R² w logistycznej: nie używaj R² do oceny regresji logistycznej. Użyj AIC, BIC, dokładności, czułości i swoistości.")
      )
    ),

    lc_chapter_next(
      num       = "07",
      title     = "Ćwiczenia",
      lead      = "zadania interpretacyjne i porównywanie modeli",
      target_id = "ch-cwiczenia"
    )

    )
  )
)

ch6_server <- function(input, output, session) {

  ch6_tree_result <- reactive({
    y <- input$ch6_tree_y
    x <- input$ch6_tree_x
    goal <- input$ch6_tree_goal
    nonlinear <- isTRUE(input$ch6_tree_nonlinear)

    if (y == "continuous" && x == "one") {
      model <- "Regresja liniowa prosta"
      note <- "Zacznij od wykresu rozrzutu, linii regresji i diagnostyki reszt."
    } else if (y == "continuous") {
      model <- "Regresja liniowa wieloraka"
      note <- "Interpretuj β przy stałych pozostałych predyktorach; sprawdź współliniowość."
    } else if (y == "binary") {
      model <- "Regresja logistyczna"
      note <- "Model zwraca prawdopodobieństwa; próg klasyfikacji dobierz do kosztu błędów."
    } else if (y == "count") {
      model <- "Regresja Poissona / ujemna dwumianowa"
      note <- "Dla nadmiernej zmienności rozważ model ujemny dwumianowy."
    } else {
      model <- "Regresja porządkowa"
      note <- "Gdy kategorie mają naturalny porządek, nie traktuj ich jak zwykłej skali ciągłej."
    }

    if (goal == "predict") {
      note <- paste(note, "Do predykcji porównuj modele na danych testowych lub przez walidację krzyżową.")
    }
    if (nonlinear) {
      note <- paste(note, "Sprawdź wielomiany, splajny lub prostszy model z lepszą generalizacją.")
    }

    list(model = model, note = note)
  })

  zoom_plot_server("ch6_tree_plot", reactive({
    res <- ch6_tree_result()
    nodes <- data.frame(
      id = 1:5,
      label = c("Y?", "Ilościowa", "Binarna", "Inna", res$model),
      x = c(0, -1.8, 0, 1.8, 0),
      y = c(3, 2, 2, 2, 0.7)
    )
    edges <- data.frame(
      x = c(0, 0, 0, nodes$x[if (input$ch6_tree_y == "continuous") 2 else if (input$ch6_tree_y == "binary") 3 else 4]),
      y = c(3, 3, 3, 2),
      xend = c(-1.8, 0, 1.8, 0),
      yend = c(2, 2, 2, 0.7)
    )
    active_branch <- if (input$ch6_tree_y == "continuous") 2 else if (input$ch6_tree_y == "binary") 3 else 4
    nodes$active <- nodes$id %in% c(1, active_branch, 5)

    ggplot() +
      geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend),
                   color = upwr_rule, linewidth = 1) +
      geom_label(data = nodes, aes(x = x, y = y, label = label, fill = active),
                 color = "white", fontface = "bold", linewidth = 0,
                 label.padding = grid::unit(0.35, "lines"), size = 4) +
      scale_fill_manual(values = c("TRUE" = upwr_secondary, "FALSE" = upwr_reference)) +
      xlim(-2.7, 2.7) +
      ylim(0, 3.5) +
      theme_void() +
      theme(legend.position = "none")
  }))

  output$ch6_tree_info <- renderUI({
    res <- ch6_tree_result()
    lc_readout("Rekomendacja", res$model, color = upwr_secondary)
  })

  output$ch6_tree_note <- renderUI({
    lc_caption(ch6_tree_result()$note, tone = "info")
  })
}
