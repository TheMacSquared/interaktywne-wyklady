# ============================================================================
# CHAPTER 3: Inzynieria Bezpieczenstwa
# ============================================================================

ch3_ui <- lecture_chapter(
  id = "ch-bezpieczenstwo",
  num = "03",
  title = "Inżynieria Bezpieczeństwa",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 03 · Materiał kierunkowy",
      num    = "03",
      title  = "Niezawodność i Weibull.",
      lead   = "Czas do awarii ma własny język: przeżycie, hazard, MTTF i krzywa wanny."
    ),

    lc_h2("ch3-survival", "Odsetek działających obiektów"),
    p("Funkcja przeżycia S(t) odpowiada na praktyczne pytanie: jaki odsetek urządzeń nadal działa po czasie t. Rozkład wykładniczy zakłada stałą intensywność uszkodzeń, a Weibull pozwala, żeby hazard malał, był stały albo rósł."),

    figure_panel(
      label = "Ryc. 3.1", title = "Weibull: β steruje typem awarii",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch3_beta", "Parametr kształtu β:", min = 0.4, max = 3.5, value = 1.6, step = 0.1),
          sliderInput("ch3_eta", "Parametr skali η:", min = 200, max = 2000, value = 900, step = 50),
          uiOutput("ch3_weibull_info")
        ),
        column(8, zoom_plot_ui("ch3_weibull_plot", height = "350px"))
      )
    ),

    lc_h2("ch3-fault-tree", "AND/OR w drzewie błędów"),
    p("Drzewo błędów to rachunek prawdopodobieństwa złożonego w języku systemów. Bramka OR zwiększa ryzyko, bo wystarczy jedna przyczyna. Bramka AND zmniejsza ryzyko, bo kilka rzeczy musi zawieść naraz."),
    lc_formula_box(
      p("Dla niezależnych zdarzeń:"),
      p(withMathJax("\\(P(A \\cup B) = 1 - (1-P(A))(1-P(B))\\)")),
      p(withMathJax("\\(P(A \\cap B) = P(A)P(B)\\)"))
    ),

    figure_panel(
      label = "Ryc. 3.2", title = "Bramka w drzewie błędów: ten sam składnik, inne ryzyko systemu",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch3_p_a", "P(A):", min = 0, max = 0.3, value = 0.08, step = 0.01),
          sliderInput("ch3_p_b", "P(B):", min = 0, max = 0.3, value = 0.05, step = 0.01),
          radioButtons("ch3_gate", "Bramka:", choices = c("OR: wystarczy jedna awaria" = "or", "AND: muszą zajść obie" = "and"), selected = "or"),
          uiOutput("ch3_gate_info")
        ),
        column(8, zoom_plot_ui("ch3_gate_plot", height = "300px"))
      )
    )
  )
)

ch3_server <- function(input, output, session) {
  zoom_plot_server("ch3_weibull_plot", reactive({
    beta <- input$ch3_beta
    eta <- input$ch3_eta
    d <- data.frame(t = seq(1, 2200, length.out = 250)) |>
      mutate(
        survival = exp(-(t / eta)^beta),
        hazard = (beta / eta) * (t / eta)^(beta - 1)
      )
    ggplot(d, aes(t)) +
      geom_line(aes(y = survival, color = "S(t): działa"), linewidth = 1.15) +
      geom_line(aes(y = hazard / max(hazard), color = "hazard (skalowany)"), linewidth = 1.15) +
      scale_color_manual(values = c("S(t): działa" = upwr_secondary, "hazard (skalowany)" = upwr_accent), name = NULL) +
      labs(x = "Czas pracy", y = "Wartość względna")
  }))

  output$ch3_weibull_info <- renderUI({
    beta <- input$ch3_beta
    eta <- input$ch3_eta
    mttf <- eta * gamma(1 + 1 / beta)
    type <- if (beta < 0.95) "β < 1: awarie wczesne, hazard maleje."
      else if (beta <= 1.05) "β ≈ 1: awarie losowe, przypadek wykładniczy."
      else "β > 1: starzenie, hazard rośnie."
    lc_stat_grid(
      lc_stat_box("Typ procesu", type, color = upwr_secondary),
      lc_stat_box("MTTF", paste(fmt(mttf, 0), "h"), caption = "średni czas do awarii", color = upwr_accent),
      columns = 1
    )
  })

  zoom_plot_server("ch3_gate_plot", reactive({
    pa <- input$ch3_p_a
    pb <- input$ch3_p_b
    p_or <- 1 - (1 - pa) * (1 - pb)
    p_and <- pa * pb
    d <- data.frame(
      element = c("A", "B", "System OR", "System AND"),
      p = c(pa, pb, p_or, p_and),
      kind = c("Składnik", "Składnik", "System", "System")
    )
    ggplot(d, aes(element, p, fill = kind)) +
      geom_col(width = 0.62) +
      geom_text(aes(label = fmt_pct(p, 1)), vjust = -0.35, size = 4) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max(d$p) * 1.22 + 0.01)) +
      scale_fill_manual(values = c("Składnik" = upwr_reference, "System" = upwr_accent), guide = "none") +
      labs(x = NULL, y = "Prawdopodobieństwo")
  }))

  output$ch3_gate_info <- renderUI({
    pa <- input$ch3_p_a
    pb <- input$ch3_p_b
    p <- if (input$ch3_gate == "or") 1 - (1 - pa) * (1 - pb) else pa * pb
    lc_stat_box("Ryzyko zdarzenia szczytowego", fmt_pct(p, 2),
                caption = if (input$ch3_gate == "or") "OR zwykle powiększa ryzyko względem pojedynczej przyczyny" else "AND działa jak bariera redundantna",
                color = upwr_secondary)
  })
}
