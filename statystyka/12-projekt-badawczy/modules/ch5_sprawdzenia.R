ch5_ui <- lecture_chapter(id = "ch5", num = "4", title = "Pierwsze sprawdzenia", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 04 · Proste testy",
      num = "04",
      title = "Pierwsze sprawdzenia w danych.",
      lead = "Test statystyczny sprawdza, czy dany trop ma oparcie w danych.
              Nie rozstrzyga celu — dostarcza przesłanki do jego oceny."
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

    lc_chapter_next("05", "Wynik nie kończy badania",
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

      desc <- tr_desc_table(id)
      first_col <- if (tr$method == "cor") "Zmienna" else "Grupa"
      desc_rows <- lapply(seq_len(nrow(desc)), function(i) {
        tags$tr(
          tags$td(desc$label[i]),
          tags$td(tr_fmt_num(desc$mean[i])),
          tags$td(tr_fmt_num(desc$sd[i])),
          tags$td(tr_fmt_num(desc$median[i])),
          tags$td(paste0(tr_fmt_num(desc$q1[i]), "–", tr_fmt_num(desc$q3[i])))
        )
      })
      desc_tbl <- tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        tags$thead(tags$tr(
          tags$th(first_col), tags$th("Średnia"), tags$th("SD"),
          tags$th("Mediana"), tags$th("Q1–Q3")
        )),
        tags$tbody(desc_rows)
      )

      p_disp <- if (grepl("<", row$p_label)) paste0("p ", row$p_label)
                else paste0("p = ", row$p_label)
      effect_kind <- if (tr$method == "cor") "korelacja" else "różnica średnich"

      div(class = "lc-figure-panel",
        h4(tr$short),
        p(tags$strong("Pytanie: "), tr$question),
        zoom_plot_ui(paste0("ch5_plot_", id), height = "320px"),
        tags$p(style = "margin-top: 12px;", tags$strong("Statystyki opisowe (eval):")),
        desc_tbl,
        tags$p(tags$strong(paste0("Miara efektu (", effect_kind, "): ")),
               row$effect,
               tags$span(style = "margin-left: 16px;", tags$strong("Test: ")),
               tr$test_name, "; ", p_disp),
        lc_feedback(
          tags$p(tags$strong("Interpretacja badawcza: "), row$full_verdict),
          type = fb_type
        )
      )
    })
    div(blocks)
  })
}
