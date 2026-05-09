# ============================================================================
# CHAPTER 10: Siła efektu
# ============================================================================

ch10_ui <- list(
  id = "ch-sila-efektu", num = "10", title = "Siła efektu",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 10 — Testowanie hipotez",
      num    = "10",
      title  = "Siła efektu.",
      lead   = "Istotność mówi „coś tam jest” — ale nie mówi „jak duże”. Cohen's d, r,
                Cramér's V i eta kwadrat to miary, które odpowiadają na pytanie „ile?”."
    ),

    # ========================================================================
    # Sekcja 1: Motywacja
    # ========================================================================
    lc_h2("ch10-motywacja", "p-wartość nie mierzy ważności"),

    tagList(
      p("Wyobraź sobie, że badasz skuteczność nowego szkolenia BHP. Mierzysz czas reakcji
        operatorów przed i po szkoleniu. Wynik: ", tags$b("p = 0,03"), " — istotne!"),
      p("Ale czy szkolenie cokolwiek zmieniło? ", tags$b("Tego p nie mówi."),
        " Przy wystarczająco dużej próbie nawet różnica 2 ms (bez żadnego praktycznego znaczenia)
        będzie „wysoce istotna statystycznie”."),
      p("I odwrotnie: przy małej próbie nawet duży efekt może nie osiągnąć istotności."),
      lc_formula_box(
        p(tags$b("Reguła: "), "p informuje o tym, czy efekt istnieje w populacji.
          Siła efektu informuje o tym, jak duży jest ten efekt.
          Obie liczby są potrzebne.")
      )
    ),

    figure_panel(
      label = "Ryc. 10.1",
      title = "p kontra d: to nie to samo",
      fluidRow(
        column(4,
          sliderInput("ch10_d", "Cohen's d (wielkość efektu):",
                      min = 0.1, max = 1.5, value = 0.3, step = 0.05),
          sliderInput("ch10_n", "n na grupę:",
                      min = 20, max = 300, value = 50, step = 10),
          p(tags$em("Zmień n przy stałym d i obserwuj, jak zmienia się p."))
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
      p("Cohen's d wyraża różnicę średnich w jednostkach odchylenia standardowego.
        Używamy go dla wszystkich wariantów testu t:
        jednej próby, dwóch prób niezależnych i próby sparowanej."),
      lc_formula_box(
        p(tags$b("Dwie grupy: "),
          withMathJax("\\(d = \\dfrac{\\bar{x}_1 - \\bar{x}_2}{s_{\\text{pooled}}}\\)")),
        p(tags$b("Jedna próba: "),
          withMathJax("\\(d = \\dfrac{\\bar{x} - \\mu_0}{s}\\)")),
        p(tags$b("Sparowany: "),
          withMathJax("\\(d = \\dfrac{\\bar{d}}{s_d}\\)"),
          " (średnia różnic przez odchylenie różnic)")
      ),
      p("Dlaczego dzielimy przez ", tags$b("s"), "? Żeby różnice z różnych dziedzin
        były porównywalne. Różnica 5 cm wzrostu i różnica 5 punktów na egzaminie
        to zupełnie inne sytuacje — ale przeliczone na ", tags$em("liczbę odchyleń standardowych"),
        " dają wspólną skalę. ", tags$b("d nie zależy od n"),
        " — mówi „jak duża jest różnica”, a nie „jak pewni jej jesteśmy”."),
      tags$table(class = "lc-table lc-table-bordered",
        style = "font-size: 15px; margin: 10px 0;",
        tags$thead(tags$tr(
          tags$th("Wielkość efektu"), tags$th("|d|"), tags$th("Przykład (dwie grupy)")
        )),
        tags$tbody(
          tags$tr(tags$td("mały"),   tags$td("0,2"),
                  tags$td("różnica tętna spoczynkowego ~2 ud./min między grupą kontrolną a po wypiciu herbaty")),
          tags$tr(tags$td("średni"), tags$td("0,5"),
                  tags$td("różnica wyników między szkołami z różnym finansowaniem")),
          tags$tr(tags$td("duży"),   tags$td("0,8"),
                  tags$td("różnica wzrostu między 13- a 18-latkami"))
        )
      )
    ),

    figure_panel(
      label = "Ryc. 10.2",
      title = "Cohen's d w surowych liczbach",
      fluidRow(
        column(4,
          radioButtons("ch10_d_level", "Wielkość efektu:",
            choices = c(
              "d = 0,2 (mały)"      = "0.2",
              "d = 0,5 (średni)"    = "0.5",
              "d = 0,8 (duży)"      = "0.8",
              "d = 1,2 (b. duży)"   = "1.2"
            ),
            selected = "0.5"
          ),
          p(tags$em("Zobacz, jak ta sama wartość d wygląda w konkretnych liczbach."))
        ),
        column(8,
          plotOutput("ch10_d_plot", height = "240px"),
          uiOutput("ch10_d_table")
        )
      )
    ),

    # ========================================================================
    # Sekcja 3: r
    # ========================================================================
    lc_h2("ch10-r", "r — korelacja Pearsona"),

    tagList(
      p("Współczynnik korelacji Pearsona ", withMathJax("\\(r\\)"),
        " jest jednocześnie statystyką testową i miarą siły efektu.
        Przyjmuje wartości od −1 do +1, więc od razu widać skalę zależności."),
      lc_formula_box(
        p(withMathJax("\\(r = \\frac{\\sum (x_i - \\bar{x})(y_i - \\bar{y})}{\\sqrt{\\sum(x_i-\\bar{x})^2 \\cdot \\sum(y_i-\\bar{y})^2}}\\)"))
      ),
      p("Wygodniejsze do interpretacji jest często ", tags$b("r²"),
        " — mówi, ", tags$em("ile procent zmienności y wyjaśnia x"),
        ". Przy r = 0,5 mamy r² = 0,25, czyli 25% zmienności wyjaśnione, a 75%
        zostaje na inne czynniki. Ważne: r mierzy tylko ", tags$b("zależność liniową"),
        " — silna zależność krzywoliniowa może dać r bliskie zeru."),
      tags$table(class = "lc-table lc-table-bordered",
        style = "font-size: 15px; margin: 10px 0;",
        tags$thead(tags$tr(
          tags$th("Wielkość efektu"), tags$th("|r|"), tags$th("Interpretacja")
        )),
        tags$tbody(
          tags$tr(tags$td("mała"),    tags$td("0,1"), tags$td("słaby związek liniowy")),
          tags$tr(tags$td("średnia"), tags$td("0,3"), tags$td("umiarkowany związek")),
          tags$tr(tags$td("duża"),    tags$td("0,5"), tags$td("silny związek liniowy"))
        )
      )
    ),

    figure_panel(
      label = "Ryc. 10.3",
      title = "r w surowych liczbach",
      fluidRow(
        column(4,
          radioButtons("ch10_r_level", "Wielkość korelacji:",
            choices = c(
              "r = 0,1 (znikoma)"    = "0.1",
              "r = 0,3 (mała)"       = "0.3",
              "r = 0,5 (średnia)"    = "0.5",
              "r = 0,7 (duża)"       = "0.7",
              "r = 0,9 (b. duża)"    = "0.9"
            ),
            selected = "0.5"
          ),
          p(tags$em("Te same dane co w korelacji temperatury i pH jogurtu — różny stopień związku."))
        ),
        column(8,
          plotOutput("ch10_r_plot", height = "240px"),
          uiOutput("ch10_r_table")
        )
      )
    ),

    # ========================================================================
    # Sekcja 4: Cramér's V
    # ========================================================================
    lc_h2("ch10-cramers-v", "Cramér's V — test chi kwadrat"),

    tagList(
      p("Dla zmiennych jakościowych χ² mówi, czy są powiązane,
        ale jego wartość bezwzględna rośnie wraz z n i rozmiarem tabeli — sam χ²
        nie jest porównywalny między badaniami. Cramér's V normalizuje χ² do
        przedziału [0, 1] i nie zależy od n."),
      lc_formula_box(
        p(withMathJax("\\(V = \\sqrt{\\frac{\\chi^2}{n \\cdot (\\min(r,c)-1)}}\\)")),
        p("gdzie ", withMathJax("\\(r\\)"), " i ", withMathJax("\\(c\\)"),
          " to liczby wierszy i kolumn tabeli kontyngencji.")
      ),
      p("Dla tabeli 2×2 Cramér's V = φ (fi) — najprostsza miara powiązania
        dwóch zmiennych binarnych. Im większe V, tym wyraźniejsza różnica
        proporcji między grupami."),
      tags$table(class = "lc-table lc-table-bordered",
        style = "font-size: 15px; margin: 10px 0;",
        tags$thead(tags$tr(
          tags$th("Wielkość efektu"), tags$th("V (tabela 2×2)"), tags$th("V (tabela 2×3+)")
        )),
        tags$tbody(
          tags$tr(tags$td("mały"),   tags$td("0,10"), tags$td("0,07")),
          tags$tr(tags$td("średni"), tags$td("0,30"), tags$td("0,21")),
          tags$tr(tags$td("duży"),   tags$td("0,50"), tags$td("0,35"))
        )
      )
    ),

    figure_panel(
      label = "Ryc. 10.4",
      title = "Cramér's V w tabeli 2×2",
      fluidRow(
        column(4,
          radioButtons("ch10_v_level", "Wielkość efektu:",
            choices = c(
              "V = 0,10 (mały)"   = "0.10",
              "V = 0,30 (średni)" = "0.30",
              "V = 0,50 (duży)"   = "0.50",
              "V = 0,70 (b. duży)" = "0.70"
            ),
            selected = "0.30"
          ),
          p(tags$em("Przykład: zaszczepieni vs niezaszczepieni — odsetek infekcji."))
        ),
        column(8,
          plotOutput("ch10_v_plot", height = "240px"),
          uiOutput("ch10_v_table")
        )
      )
    ),

    # ========================================================================
    # Sekcja 5: eta kwadrat
    # ========================================================================
    lc_h2("ch10-eta2", "eta kwadrat — ANOVA"),

    tagList(
      p(withMathJax("\\(\\eta^2\\)"),
        " (eta kwadrat) to udział wariancji całkowitej wyjaśniany przez przynależność do grupy.
        Można je rozumieć jako „procent zmienności wyników tłumaczony przez badany czynnik”."),
      lc_formula_box(
        p(withMathJax("\\(\\eta^2 = \\frac{SS_{\\text{między}}}{SS_{\\text{całkowite}}}\\)"))
      ),
      p("W jamovi i niektórych podręcznikach zobaczysz różne warianty: ",
        tags$b("klasyczne η²"), ", ", tags$b("η² częściowe (partial)"), " i ",
        tags$b("η² uogólnione (ges)"), " — różnią się tym, co dokładnie znajduje
        się w mianowniku. Dla prostej ANOVA jednoczynnikowej wszystkie zwracają
        zbliżone wartości. Progi Cohena (0,01 / 0,06 / 0,14) to ",
        tags$em("dolne piętra"), " — w eksperymentach kontrolowanych często
        oczekujemy efektów znacznie powyżej 0,15."),
      tags$table(class = "lc-table lc-table-bordered",
        style = "font-size: 15px; margin: 10px 0;",
        tags$thead(tags$tr(
          tags$th("Wielkość efektu"), tags$th(withMathJax("\\(\\eta^2\\)")),
          tags$th("Interpretacja")
        )),
        tags$tbody(
          tags$tr(tags$td("mały"),   tags$td("0,01"),
                  tags$td("czynnik tłumaczy ~1% zmienności")),
          tags$tr(tags$td("średni"), tags$td("0,06"),
                  tags$td("czynnik tłumaczy ~6% zmienności")),
          tags$tr(tags$td("duży"),   tags$td("0,14"),
                  tags$td("czynnik tłumaczy ≥ 14% zmienności"))
        )
      )
    ),

    figure_panel(
      label = "Ryc. 10.5",
      title = "η² w ANOVA — trzy grupy",
      fluidRow(
        column(4,
          radioButtons("ch10_eta_level", "Wielkość efektu:",
            choices = c(
              "η² = 0,01 (mały)"    = "0.01",
              "η² = 0,06 (średni)"  = "0.06",
              "η² = 0,14 (duży)"    = "0.14",
              "η² = 0,30 (b. duży)" = "0.30"
            ),
            selected = "0.06"
          ),
          p(tags$em("3 metody pasteryzacji × kolonie bakterii (s = 10 we wszystkich grupach)."))
        ),
        column(8,
          plotOutput("ch10_eta_plot", height = "240px"),
          uiOutput("ch10_eta_table")
        )
      )
    ),

    inline_callout(
      label = "Kontekst",
      tagList(
        "Wartości Cohena („mały/średni/duży”) to konwencje z lat 60. — służą jako punkt
         odniesienia, nie bezwzględny standard. W wielu dziedzinach ",
        tags$b("d = 0,2 może być kluczowe"),
        " (np. zmiana śmiertelności o 2 pp.). Zawsze interpretuj efekt
         w kontekście stawek i kosztów dziedziny."
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

  # --- Ryc. 10.1 (istniejący widget) ---
  output$ch10_dist_plot <- renderPlot({
    d <- input$ch10_d
    x_lo <- -4
    x_hi <- d + 4
    x_seq <- seq(x_lo, x_hi, length.out = 600)

    df_g1 <- data.frame(x = x_seq, y = dnorm(x_seq, 0, 1), grupa = "Grupa 1 (średnia = 0)")
    df_g2 <- data.frame(x = x_seq, y = dnorm(x_seq, d, 1), grupa = paste0("Grupa 2 (średnia = d = ", d, ")"))

    df_all <- rbind(df_g1, df_g2)
    df_all$grupa <- factor(df_all$grupa, levels = unique(df_all$grupa))

    ggplot(df_all, aes(x = x, y = y, fill = grupa, color = grupa)) +
      geom_area(alpha = 0.35, position = "identity") +
      geom_line(linewidth = 0.9) +
      geom_vline(xintercept = 0, color = col_h0,     linetype = "dashed", linewidth = 0.8) +
      geom_vline(xintercept = d, color = col_reject, linetype = "dashed", linewidth = 0.8) +
      annotate("segment",
               x = 0, xend = d, y = 0.45, yend = 0.45,
               arrow = arrow(ends = "both", length = unit(0.08, "inches")),
               color = "grey30", linewidth = 0.7) +
      annotate("text", x = d / 2, y = 0.48, label = paste0("d = ", d),
               color = "grey20", fontface = "bold", size = 4) +
      scale_fill_manual(values  = c(col_h0, col_reject)) +
      scale_color_manual(values = c(col_h0, col_reject)) +
      labs(x = NULL, y = "Gęstość", fill = NULL, color = NULL) +
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

    effect_label <- if (abs(d) < 0.2) "pomijalna" else if (abs(d) < 0.5) "mała" else
                    if (abs(d) < 0.8) "średnia" else "duża"

    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        style = "margin-top: 8px;",
        tags$thead(tags$tr(
          tags$th("n / grupę"), tags$th("Cohen's d"), tags$th("t"), tags$th("p")
        )),
        tags$tbody(tags$tr(
          tags$td(n),
          tags$td(paste0(d, "  (", effect_label, ")")),
          tags$td(round(t_val, 2)),
          tags$td(format_p_value(p_val))
        ))
      ),
      lc_feedback(type = fb_type,
        p(style = paste0("color:", res$color, "; font-weight: bold; margin: 0;"),
          res$decision)
      )
    )
  })

  # --- Ryc. 10.2: Cohen's d w surowych liczbach ---
  ch10_d_examples <- list(
    "0.2" = list(
      x1 = 72, x2 = 74, s = 10,
      kontekst = "Tętno spoczynkowe (ud./min): grupa kontrolna vs po wypiciu herbaty czarnej — różnica 2 ud./min.",
      jednostka = "ud./min",
      etyk1 = "Bez kofeiny", etyk2 = "Po herbacie"
    ),
    "0.5" = list(
      x1 = 60, x2 = 66, s = 12,
      kontekst = "Wynik egzaminu (pkt): grupa kontrolna vs grupa po korepetycjach — różnica 6 pkt.",
      jednostka = "pkt",
      etyk1 = "Bez korepetycji", etyk2 = "Z korepetycjami"
    ),
    "0.8" = list(
      x1 = 12.0, x2 = 14.0, s = 2.5,
      kontekst = "Czas reakcji operatora (s): zmiana ranna vs nocna — różnica 2 s.",
      jednostka = "s",
      etyk1 = "Zmiana ranna", etyk2 = "Zmiana nocna"
    ),
    "1.2" = list(
      x1 = 5.0, x2 = 8.6, s = 3,
      kontekst = "Sen (godz.): okres egzaminacyjny vs ferie — różnica 3,6 h.",
      jednostka = "godz.",
      etyk1 = "Egzaminy", etyk2 = "Ferie"
    )
  )

  output$ch10_d_plot <- renderPlot({
    req(input$ch10_d_level)
    e <- ch10_d_examples[[input$ch10_d_level]]
    x_lo <- min(e$x1, e$x2) - 3 * e$s
    x_hi <- max(e$x1, e$x2) + 3 * e$s
    x_seq <- seq(x_lo, x_hi, length.out = 600)

    df_g1 <- data.frame(x = x_seq, y = dnorm(x_seq, e$x1, e$s), grupa = e$etyk1)
    df_g2 <- data.frame(x = x_seq, y = dnorm(x_seq, e$x2, e$s), grupa = e$etyk2)
    df_all <- rbind(df_g1, df_g2)
    df_all$grupa <- factor(df_all$grupa, levels = c(e$etyk1, e$etyk2))

    ggplot(df_all, aes(x = x, y = y, fill = grupa, color = grupa)) +
      geom_area(alpha = 0.35, position = "identity") +
      geom_line(linewidth = 0.9) +
      geom_vline(xintercept = e$x1, color = col_h0,     linetype = "dashed", linewidth = 0.7) +
      geom_vline(xintercept = e$x2, color = col_reject, linetype = "dashed", linewidth = 0.7) +
      scale_fill_manual(values  = c(col_h0, col_reject)) +
      scale_color_manual(values = c(col_h0, col_reject)) +
      labs(x = paste0("Wartość (", e$jednostka, ")"),
           y = "Gęstość", fill = NULL, color = NULL) +
      theme(legend.position = "bottom")
  })

  output$ch10_d_table <- renderUI({
    req(input$ch10_d_level)
    e <- ch10_d_examples[[input$ch10_d_level]]
    diff <- e$x2 - e$x1
    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        style = "margin-top: 8px;",
        tags$thead(tags$tr(
          tags$th("Grupa"),
          tags$th(HTML("&xbar; &plusmn; s")),
          tags$th("Różnica"),
          tags$th("d")
        )),
        tags$tbody(
          tags$tr(
            tags$td(e$etyk1),
            tags$td(paste0(e$x1, " ± ", e$s, " ", e$jednostka)),
            tags$td(rowspan = 2, paste0(round(diff, 2), " ", e$jednostka)),
            tags$td(rowspan = 2, input$ch10_d_level)
          ),
          tags$tr(
            tags$td(e$etyk2),
            tags$td(paste0(e$x2, " ± ", e$s, " ", e$jednostka))
          )
        )
      ),
      p(style = "margin-top: 8px;", tags$em(e$kontekst))
    )
  })

  # --- Ryc. 10.3: r w surowych liczbach ---
  output$ch10_r_plot <- renderPlot({
    req(input$ch10_r_level)
    r_target <- as.numeric(input$ch10_r_level)
    set.seed(101)
    n_pts <- 50
    z <- rnorm(n_pts)
    e <- rnorm(n_pts)
    x_raw <- z
    y_raw <- r_target * z + sqrt(1 - r_target^2) * e
    # Rescale do realistycznych jednostek: temperatura fermentacji 35-50°C, pH jogurtu 4,2-4,8
    x <- 42 + 5 * x_raw
    y <- 4.5 + 0.2 * y_raw

    df <- data.frame(x = x, y = y)
    r_emp <- cor(df$x, df$y)

    ggplot(df, aes(x = x, y = y)) +
      geom_smooth(method = "lm", se = FALSE, color = upwr_reference,
                  linewidth = 1, formula = y ~ x) +
      geom_point(color = col_effect, alpha = 0.7, size = 2.5) +
      labs(x = "Temperatura fermentacji (°C)",
           y = "pH jogurtu po 24 h",
           subtitle = paste0("r empiryczne = ", round(r_emp, 2),
                             "  (zadane r = ", r_target, ")"))
  })

  output$ch10_r_table <- renderUI({
    req(input$ch10_r_level)
    r_val <- as.numeric(input$ch10_r_level)
    r2 <- r_val^2
    opis <- switch(input$ch10_r_level,
      "0.1" = "Punkty rozproszone, ledwo widoczny trend — czynnik tłumaczy 1% zmienności y.",
      "0.3" = "Trend zauważalny, ale duży rozrzut wokół linii — 9% zmienności wyjaśnione.",
      "0.5" = "Wyraźny trend, około ¼ zmienności y wyjaśniona przez x — reszta na inne czynniki.",
      "0.7" = "Mocny związek liniowy, prawie połowa zmienności y wyjaśniona.",
      "0.9" = "Punkty bardzo blisko linii prostej — 81% zmienności y wyjaśnione przez x."
    )
    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        style = "margin-top: 8px;",
        tags$thead(tags$tr(
          tags$th("r"), tags$th("r²"), tags$th("% wariancji wyjaśnione")
        )),
        tags$tbody(tags$tr(
          tags$td(r_val),
          tags$td(round(r2, 2)),
          tags$td(paste0(round(100 * r2), "%"))
        ))
      ),
      p(style = "margin-top: 8px;", tags$em(opis))
    )
  })

  # --- Ryc. 10.4: Cramér's V w 2×2 ---
  # Dla tabeli 2×2 V = phi = (p_A - p_B) gdy proporcje pomocnicze 50/50 — używamy
  # symetrycznego scenariusza p_A = 0.5 - V/2, p_B = 0.5 + V/2 (przy zbalansowanych grupach n_A = n_B).
  ch10_v_examples <- list(
    "0.10" = list(p_a = 0.45, p_b = 0.55,
                  kontekst = "Słabe powiązanie: zaszczepieni 45% infekcji, niezaszczepieni 55% — różnica 10 pp."),
    "0.30" = list(p_a = 0.35, p_b = 0.65,
                  kontekst = "Wyraźne powiązanie: 35% vs 65% — szczepienie zauważalnie obniża ryzyko (30 pp.)."),
    "0.50" = list(p_a = 0.25, p_b = 0.75,
                  kontekst = "Silne powiązanie: 25% vs 75% — różnica 50 pp., szczepienie chroni połowę grupy."),
    "0.70" = list(p_a = 0.15, p_b = 0.85,
                  kontekst = "Bardzo silne powiązanie: 15% vs 85% — efekt niemal deterministyczny.")
  )

  output$ch10_v_plot <- renderPlot({
    req(input$ch10_v_level)
    e <- ch10_v_examples[[input$ch10_v_level]]
    df <- data.frame(
      grupa = factor(c("Zaszczepieni", "Zaszczepieni", "Niezaszczepieni", "Niezaszczepieni"),
                     levels = c("Zaszczepieni", "Niezaszczepieni")),
      stan  = factor(c("Infekcja", "Brak infekcji", "Infekcja", "Brak infekcji"),
                     levels = c("Brak infekcji", "Infekcja")),
      pct   = c(e$p_a, 1 - e$p_a, e$p_b, 1 - e$p_b)
    )
    ggplot(df, aes(x = grupa, y = pct, fill = stan)) +
      geom_col(width = 0.55, alpha = 0.9) +
      geom_text(aes(label = paste0(round(100 * pct), "%")),
                position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold", size = 5) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = c("Brak infekcji" = col_h0, "Infekcja" = col_reject)) +
      labs(x = NULL, y = "Odsetek osób w grupie", fill = NULL) +
      theme(legend.position = "bottom")
  })

  output$ch10_v_table <- renderUI({
    req(input$ch10_v_level)
    e <- ch10_v_examples[[input$ch10_v_level]]
    diff_pp <- round(100 * (e$p_b - e$p_a))
    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        style = "margin-top: 8px;",
        tags$thead(tags$tr(
          tags$th("Grupa"),
          tags$th("Infekcja (%)"),
          tags$th("Brak infekcji (%)"),
          tags$th("V")
        )),
        tags$tbody(
          tags$tr(
            tags$td("Zaszczepieni"),
            tags$td(paste0(round(100 * e$p_a), "%")),
            tags$td(paste0(round(100 * (1 - e$p_a)), "%")),
            tags$td(rowspan = 2, input$ch10_v_level)
          ),
          tags$tr(
            tags$td("Niezaszczepieni"),
            tags$td(paste0(round(100 * e$p_b), "%")),
            tags$td(paste0(round(100 * (1 - e$p_b)), "%"))
          )
        )
      ),
      p(style = "margin-top: 8px;",
        tags$em(paste0("Różnica między grupami: ", diff_pp, " pp. ", e$kontekst)))
    )
  })

  # --- Ryc. 10.5: η² w ANOVA — trzy grupy ---
  # eta² = SS_between / SS_total. Dla 3 grup równolicznych ze średnimi mu = (mu1, mu2, mu3) i wspólnym sd s,
  # eta² (populacyjne) = var(mu) / (var(mu) + s²). Dobieramy mu tak, żeby var(mu) / (var(mu) + s²) = eta_target.
  # Trzymamy s = 10, mu_centralna = 50, mu1 = 50 - delta, mu3 = 50 + delta:
  # var(mu) = (2 * delta²) / 3, więc delta = sqrt(3 * eta * s² / (2 * (1 - eta)))
  ch10_eta_examples <- list(
    "0.01" = list(delta_round = "1,0", kontekst = "Metody dają niemal identyczne wyniki — czynnik symboliczny."),
    "0.06" = list(delta_round = "3,2", kontekst = "Metody zauważalnie się różnią, ale rozrzut wewnątrz grup nadal dominuje."),
    "0.14" = list(delta_round = "5,1", kontekst = "Czynnik wyraźnie liczy się — 14% zmienności tłumaczy metoda."),
    "0.30" = list(delta_round = "8,0", kontekst = "Dominujący efekt — wybór metody wyjaśnia 30% wszystkich różnic.")
  )

  ch10_eta_means <- function(eta) {
    s <- 10
    delta <- sqrt(3 * eta * s^2 / (2 * (1 - eta)))
    c(50 - delta, 50, 50 + delta)
  }

  output$ch10_eta_plot <- renderPlot({
    req(input$ch10_eta_level)
    eta <- as.numeric(input$ch10_eta_level)
    mus <- ch10_eta_means(eta)
    s <- 10
    set.seed(202)
    n_per <- 30
    df <- data.frame(
      grupa = factor(rep(c("Metoda A", "Metoda B", "Metoda C"), each = n_per),
                     levels = c("Metoda A", "Metoda B", "Metoda C")),
      y = c(rnorm(n_per, mus[1], s),
            rnorm(n_per, mus[2], s),
            rnorm(n_per, mus[3], s))
    )
    ggplot(df, aes(x = grupa, y = y, fill = grupa)) +
      geom_boxplot(alpha = 0.5, outlier.alpha = 0.5) +
      geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
      scale_fill_upwr() +
      labs(x = NULL, y = "Liczba kolonii bakterii (jedn.)", fill = NULL) +
      theme(legend.position = "none")
  })

  output$ch10_eta_table <- renderUI({
    req(input$ch10_eta_level)
    eta <- as.numeric(input$ch10_eta_level)
    e <- ch10_eta_examples[[input$ch10_eta_level]]
    mus <- ch10_eta_means(eta)
    pct <- round(100 * eta)
    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        style = "margin-top: 8px;",
        tags$thead(tags$tr(
          tags$th("Grupa"),
          tags$th(HTML("&xbar;")),
          tags$th("s"),
          tags$th("η²"),
          tags$th("% wariancji wyjaśnione")
        )),
        tags$tbody(
          tags$tr(
            tags$td("Metoda A"), tags$td(round(mus[1], 1)), tags$td(10),
            tags$td(rowspan = 3, input$ch10_eta_level),
            tags$td(rowspan = 3, paste0(pct, "%"))
          ),
          tags$tr(tags$td("Metoda B"), tags$td(round(mus[2], 1)), tags$td(10)),
          tags$tr(tags$td("Metoda C"), tags$td(round(mus[3], 1)), tags$td(10))
        )
      ),
      p(style = "margin-top: 8px;", tags$em(e$kontekst))
    )
  })

}
