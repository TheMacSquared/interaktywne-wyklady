# ============================================================================
# CHAPTER 10: Sila efektu
# ============================================================================

ch10_ui <- list(
  id = "ch-sila-efektu", num = "10", title = "Sila efektu",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdzial 10 - Testowanie hipotez",
      num    = "10",
      title  = "Sila efektu.",
      lead   = "Istotnosc mowi 'cos tam jest' — ale nie mowi 'jak duze'. Cohen's d, r,
                Cramer's V i eta kwadrat to miary, ktore odpowiadaja na pytanie 'ile?'."
    ),

    # ========================================================================
    # Sekcja 1: Motywacja
    # ========================================================================
    lc_h2("ch10-motywacja", "p-wartosc nie mierzy waznosci"),

    tagList(
      p("Wyobraz sobie, ze badasz skutecznosc nowego szkolenia BHP. Mierzysz czas reakcji
        operatorow przed i po szkoleniu. Wynik: ", tags$b("p = 0.03"), " — istotne!"),
      p("Ale czy szkolenie cokolwiek zmienialo?", tags$b("Tego p nie mowi."),
        " Przy wystarczajaco duzej probie nawet roznica 2 ms (bez zadnego praktycznego znaczenia)
        bedzie 'wysoce istotna statystycznie'."),
      p("I odwrotnie: przy malej probie nawet duzy efekt moze nie osiagnac istotnosci."),
      lc_formula_box(
        p(tags$b("Regula: "), "p informuje o tym, czy efekt istnieje w populacji.
          Sila efektu informuje o tym, jak duzy jest ten efekt.
          Obie liczby sa potrzebne.")
      )
    ),

    figure_panel(
      label = "Ryc. 10.1",
      title = "p kontra d: to nie to samo",
      fluidRow(
        column(4,
          sliderInput("ch10_d", "Cohen's d (wielkosc efektu):",
                      min = 0.1, max = 1.5, value = 0.3, step = 0.05),
          sliderInput("ch10_n", "n na grupe:",
                      min = 20, max = 300, value = 50, step = 10),
          p(tags$em("Zmien n przy stalym d i obserwuj, jak zmienia sie p."))
        ),
        column(8,
          plotOutput("ch10_dist_plot", height = "280px"),
          uiOutput("ch10_dist_stats")
        )
      )
    ),

    # ========================================================================
    # Sekcja 2: Cohen's d
    # ========================================================================
    lc_h2("ch10-cohens-d", "Cohen's d — testy t"),

    tagList(
      p("Cohen's d wyraza roznice srednich w jednostkach odchylenia standardowego.
        Uzywamy go dla wszystkich wariantow testu t:
        jednej proby, dwoch prob niezaleznych i proby sparowanej."),
      lc_formula_box(
        p(tags$b("Dwie grupy: "),
          withMathJax("\\(d = \\dfrac{\\bar{x}_1 - \\bar{x}_2}{s_{\\text{pooled}}}\\)")),
        p(tags$b("Jedna proba: "),
          withMathJax("\\(d = \\dfrac{\\bar{x} - \\mu_0}{s}\\)")),
        p(tags$b("Sparowany: "),
          withMathJax("\\(d = \\dfrac{\\bar{d}}{s_d}\\)"),
          " (srednia roznic przez odchylenie roznic)")
      ),
      tags$table(class = "lc-table lc-table-bordered",
        style = "font-size: 15px; margin: 10px 0;",
        tags$thead(tags$tr(
          tags$th("Wielkosc efektu"), tags$th("|d|"), tags$th("Przyklad (dwie grupy)")
        )),
        tags$tbody(
          tags$tr(tags$td("maly"),   tags$td("0.2"),
                  tags$td("roznica wzrostu 2-3 cm miedzy plciami w tej samej grupie wiekowej")),
          tags$tr(tags$td("sredni"), tags$td("0.5"),
                  tags$td("roznica wynikow miedzy szkolami z roznym finansowaniem")),
          tags$tr(tags$td("duzy"),   tags$td("0.8"),
                  tags$td("roznica wzrostu miedzy 13- a 18-latkami"))
        )
      )
    ),

    # ========================================================================
    # Sekcja 3: r
    # ========================================================================
    lc_h2("ch10-r", "r — korelacja Pearsona"),

    tagList(
      p("Wspolczynnik korelacji Pearsona ", withMathJax("\\(r\\)"),
        " jest jednoczesnie statystyka testowa i miara sily efektu.
        Przyjmuje wartosci od -1 do +1, wiec od razu widac skale zalezenosci."),
      lc_formula_box(
        p(withMathJax("\\(r = \\frac{\\sum (x_i - \\bar{x})(y_i - \\bar{y})}{\\sqrt{\\sum(x_i-\\bar{x})^2 \\cdot \\sum(y_i-\\bar{y})^2}}\\)"))
      ),
      tags$table(class = "lc-table lc-table-bordered",
        style = "font-size: 15px; margin: 10px 0;",
        tags$thead(tags$tr(
          tags$th("Wielkosc efektu"), tags$th("|r|"), tags$th("Interpretacja")
        )),
        tags$tbody(
          tags$tr(tags$td("mala"),   tags$td("0.1"), tags$td("slaby zwiazek liniowy")),
          tags$tr(tags$td("srednia"), tags$td("0.3"), tags$td("umiarkowany zwiazek")),
          tags$tr(tags$td("duza"),   tags$td("0.5"), tags$td("silny zwiazek liniowy"))
        )
      )
    ),

    # ========================================================================
    # Sekcja 4: Cramer's V
    # ========================================================================
    lc_h2("ch10-cramers-v", "Cramer's V — test chi kwadrat"),

    tagList(
      p("Dla zmiennych jakosciowych chi kwadrat mowi, czy sa powiazane,
        ale jego wartosc bezwzgledna rosnie wraz z n i rozmiarem tabeli.
        Cramer's V normalizuje chi kwadrat do przedzialu [0, 1]."),
      lc_formula_box(
        p(withMathJax("\\(V = \\sqrt{\\frac{\\chi^2}{n \\cdot (\\min(r,c)-1)}}\\)")),
        p("gdzie ", withMathJax("\\(r\\)"), " i ", withMathJax("\\(c\\)"),
          " to liczby wierszy i kolumn tabeli kontyngencji.")
      ),
      tags$table(class = "lc-table lc-table-bordered",
        style = "font-size: 15px; margin: 10px 0;",
        tags$thead(tags$tr(
          tags$th("Wielkosc efektu"), tags$th("V (tabela 2x2)"), tags$th("V (tabela 2x3+)")
        )),
        tags$tbody(
          tags$tr(tags$td("maly"),   tags$td("0.10"), tags$td("0.07")),
          tags$tr(tags$td("sredni"), tags$td("0.30"), tags$td("0.21")),
          tags$tr(tags$td("duzy"),   tags$td("0.50"), tags$td("0.35"))
        )
      ),
      p(tags$em("Dla tabeli 2x2 Cramer's V = phi (fi), bezposrednia miara powiazan."))
    ),

    # ========================================================================
    # Sekcja 5: eta kwadrat
    # ========================================================================
    lc_h2("ch10-eta2", "eta kwadrat — ANOVA"),

    tagList(
      p(withMathJax("\\(\\eta^2\\)"),
        " (eta kwadrat) to udzial wariancji calkowitej wyjasniany przez przynaleznosc do grupy.
        Mozna je rozumiec jako 'procent zmiennosci wynikow tlumaczony przez badany czynnik'."),
      lc_formula_box(
        p(withMathJax("\\(\\eta^2 = \\frac{SS_{\\text{miedzy}}}{SS_{\\text{calkowite}}}\\)"))
      ),
      tags$table(class = "lc-table lc-table-bordered",
        style = "font-size: 15px; margin: 10px 0;",
        tags$thead(tags$tr(
          tags$th("Wielkosc efektu"), tags$th(withMathJax("\\(\\eta^2\\)")),
          tags$th("Interpretacja")
        )),
        tags$tbody(
          tags$tr(tags$td("maly"),   tags$td("0.01"),
                  tags$td("czynnik tlumaczy ~1% zmiennosci")),
          tags$tr(tags$td("sredni"), tags$td("0.06"),
                  tags$td("czynnik tlumaczy ~6% zmiennosci")),
          tags$tr(tags$td("duzy"),   tags$td("0.14"),
                  tags$td("czynnik tlumaczy >= 14% zmiennosci"))
        )
      )
    ),

    inline_callout(
      label = "Kontekst",
      tagList(
        "Wartosci Cohena ('maly/sredni/duzy') to konwencje z lat 60. — sluzba jako punkt
         odniesienia, nie bezwzgledny standard. W wielu dziedzinach ",
        tags$b("d = 0.2 moze byc kluczowe"),
        " (np. zmiana smiertelnosci o 2 pp.). Zawsze interpretuj efekt
         w kontekscie stawek i kosztow dziedziny."
      ),
      color = "uwaga"
    ),

    lc_chapter_next(
      num       = "11",
      title     = "Drzewo decyzyjne",
      lead      = "mapa wyboru testu — od typu zmiennych do konkretnego testu.",
      target_id = "ch-drzewo"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch10_server <- function(input, output, session) {

  output$ch10_dist_plot <- renderPlot({
    d <- input$ch10_d
    x_lo <- -4
    x_hi <- d + 4
    x_seq <- seq(x_lo, x_hi, length.out = 600)

    df_g1 <- data.frame(x = x_seq, y = dnorm(x_seq, 0, 1), grupa = "Grupa 1 (srednia = 0)")
    df_g2 <- data.frame(x = x_seq, y = dnorm(x_seq, d, 1), grupa = paste0("Grupa 2 (srednia = d = ", d, ")"))

    df_all <- rbind(df_g1, df_g2)
    df_all$grupa <- factor(df_all$grupa, levels = unique(df_all$grupa))

    ggplot(df_all, aes(x = x, y = y, fill = grupa, color = grupa)) +
      geom_area(alpha = 0.35, position = "identity") +
      geom_line(linewidth = 0.9) +
      geom_vline(xintercept = 0, color = col_h0,     linetype = "dashed", linewidth = 0.8) +
      geom_vline(xintercept = d, color = col_reject,  linetype = "dashed", linewidth = 0.8) +
      annotate("segment",
               x = 0, xend = d, y = 0.45, yend = 0.45,
               arrow = arrow(ends = "both", length = unit(0.08, "inches")),
               color = "grey30", linewidth = 0.7) +
      annotate("text", x = d / 2, y = 0.48, label = paste0("d = ", d),
               color = "grey20", fontface = "bold", size = 4) +
      scale_fill_manual(values  = c(col_h0, col_reject)) +
      scale_color_manual(values = c(col_h0, col_reject)) +
      labs(x = NULL, y = "Gestosc", fill = NULL, color = NULL) +
      theme(legend.position = "bottom")
  })

  output$ch10_dist_stats <- renderUI({
    d     <- input$ch10_d
    n     <- input$ch10_n
    se    <- sqrt(2 / n)
    t_val <- d / se
    df_t  <- 2 * n - 2
    p_val <- 2 * pt(-abs(t_val), df_t)

    res   <- format_test_result(p_val)
    fb_type <- if (p_val < 0.05) "warning" else "ok"

    effect_label <- if (abs(d) < 0.2) "pomijalna" else if (abs(d) < 0.5) "mala" else
                    if (abs(d) < 0.8) "srednia" else "duza"

    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        style = "margin-top: 8px;",
        tags$thead(tags$tr(
          tags$th("n / grupe"), tags$th("Cohen's d"), tags$th("t"), tags$th("p")
        )),
        tags$tbody(tags$tr(
          tags$td(n),
          tags$td(paste0(d, "  (", effect_label, ")")),
          tags$td(round(t_val, 2)),
          tags$td(format.pval(p_val, digits = 3))
        ))
      ),
      lc_feedback(type = fb_type,
        p(style = paste0("color:", res$color, "; font-weight: bold; margin: 0;"),
          res$decision)
      )
    )
  })

}
