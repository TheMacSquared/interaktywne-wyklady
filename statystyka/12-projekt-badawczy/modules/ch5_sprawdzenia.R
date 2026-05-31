ch5_ui <- lecture_chapter(id = "ch5", num = "5", title = "Pierwsze sprawdzenia", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 05 · Proste testy",
      num = "05",
      title = "Pierwsze sprawdzenia w danych.",
      lead = "Test statystyczny nie jest finałem. Jest sposobem na sprawdzenie,
              czy każdy trop z wiązki ma kontakt z danymi."
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Przypomnienie celu:"),
      p(tags$em(tr_goal)),
      p("Teraz przepuszczamy całą wiązkę przez dane. Dla każdego tropu dobieramy
        narzędzie do typu zmiennych i zapisujemy wynik do tablicy.")
    ),

    lc_h2("sec-01", "Wiązka spotyka dane — trop po tropie"),

    div(class = "lc-prose",
      p("Każdy trop dostaje wykres i test dopasowany do typu zmiennych:
        korelację dla zmiennych ilościowych, test t lub Mann-Whitneya dla
        porównania dwóch grup. Nie pytamy tylko „czy p < 0,05?\" — pytamy, czy
        wynik wzmacnia trop, osłabia go, czy każe zmienić pytanie.")
    ),

    uiOutput("ch5_bundle_results"),

    lc_h2("sec-02", "Tablica tropów po pierwszych testach"),

    div(class = "lc-prose",
      p("To jest ta sama tablica, którą zaczęliśmy w rozdziale 1 — teraz wypełniona.
        Każdy wiersz to jeden trop: pytanie, narzędzie, wynik i wstępny werdykt.")
    ),

    div(class = "lc-figure-panel",
      h4("Tablica tropów (po pierwszych testach)"),
      tr_board_ui(reveal = tr_trop_order, show_verdict = TRUE)
    ),

    lc_h2("sec-03", "Dlaczego jeden test to za mało: zmienne zakłócające"),

    div(class = "lc-prose",
      p("Pojedynczy test mówi tylko o związku dwóch zmiennych. Ale zmienna Z może
        mieszać w interpretacji, jeśli wiąże się jednocześnie z predyktorem i z
        wynikiem. Pokażmy to na najmocniejszym tropie: `beauty` → `eval`.")
    ),

    div(class = "lc-figure-panel",
      h4("Kandydaci na zmienne zakłócające (dla tropu beauty)"),
      uiOutput("ch5_confounder_table")
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Wniosek pośredni:"),
      p("Jeśli choć jedna zmienna wiąże się i z `beauty`, i z `eval`, to prosty
        test nie wystarczy — trzeba uwzględnić te zmienne jednocześnie. To jest
        dokładnie zadanie dla modelu kontrolnego z rozdziału 8.")
    ),

    lc_chapter_next("06", "Wynik nie kończy badania",
      "Mamy pełną tablicę — czas odczytać, co cała wiązka mówi o celu.",
      "ch6"),
    div(style = "height: 40px;")
  )))
)

ch5_server <- function(input, output, session) {
  # Wykres jednego tropu — korelacja (ilościowe) albo boxplot (grupy).
  .trop_plot <- function(id) {
    tr <- tr_tropy[[id]]
    if (tr$method == "cor") {
      ggplot(tr_data, aes(x = .data[[tr$var]], y = eval)) +
        geom_point(color = proj_col_ref, alpha = 0.45, size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = proj_col_ctrl,
                    fill = proj_col_ctrl, alpha = 0.12) +
        labs(x = unname(tr_labels[tr$var]), y = "Ocena kursu (eval)") +
        theme_upwr()
    } else {
      ggplot(tr_data, aes(x = .data[[tr$var]], y = eval, fill = .data[[tr$var]])) +
        geom_boxplot(alpha = 0.65, outlier.alpha = 0.25) +
        geom_jitter(width = 0.12, alpha = 0.16, size = 1) +
        scale_fill_manual(values = rep(c(proj_col_data, proj_col_hyp, proj_col_ctrl),
                                       length.out = length(unique(tr_data[[tr$var]])))) +
        labs(x = unname(tr_labels[tr$var]), y = "Ocena kursu (eval)") +
        theme_upwr() +
        theme(legend.position = "none")
    }
  }

  # Zarejestruj wykres + render karty wyniku dla każdego tropu.
  lapply(tr_trop_order, function(id) {
    zoom_plot_server(paste0("ch5_plot_", id), reactive(.trop_plot(id)))
  })

  output$ch5_bundle_results <- renderUI({
    blocks <- lapply(tr_trop_order, function(id) {
      tr  <- tr_tropy[[id]]
      row <- tr_board_row(id)
      fb_type <- if (row$supported) "warning" else "ok"
      div(class = "lc-figure-panel",
        h4(tr$short),
        fluidRow(
          column(7, zoom_plot_ui(paste0("ch5_plot_", id), height = "300px")),
          column(5,
            lc_stat_grid(
              lc_stat_box("Pytanie", tr$question, color = proj_col_data),
              lc_stat_box("Test", row$test_name, color = proj_col_ctrl),
              lc_stat_box("Wynik", row$effect, color = proj_col_hyp),
              lc_stat_box("p", row$p_label, color = proj_col_warn),
              columns = 1
            )
          )
        ),
        lc_feedback(
          tags$p(tags$strong("Interpretacja badawcza: "), row$full_verdict),
          type = fb_type
        )
      )
    })
    div(blocks)
  })

  output$ch5_confounder_table <- renderUI({
    rows <- lapply(tr_confounder_vars, function(var) {
      r <- tr_confounder_row(var)
      verdict <- if (r$confounder) {
        tags$span(class = "tropy-verdict tropy-verdict-off", "kandydat na zakłócacz")
      } else {
        tags$span(class = "tropy-muted", "nie zakłóca głównej relacji")
      }
      tags$tr(
        tags$td(tags$strong(r$label)),
        tags$td(r$beauty_label),
        tags$td(r$eval_label),
        tags$td(verdict)
      )
    })
    tagList(
      div(class = "lc-prose",
        p("Zmienna jest kandydatem na zakłócacz, gdy wiąże się i z `beauty`,
          i z `eval` jednocześnie.")
      ),
      tags$table(class = "lc-table lc-table-bordered lc-table-striped",
        tags$thead(tags$tr(
          tags$th("Zmienna"),
          tags$th("Związek z beauty"),
          tags$th("Związek z eval"),
          tags$th("Werdykt")
        )),
        tags$tbody(rows)
      )
    )
  })
}
