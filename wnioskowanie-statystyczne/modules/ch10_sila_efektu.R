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
      lead   = "Istotność mówi 'coś tam jest' — ale nie mówi 'jak duże'. Cohen's d, r,
                Cramér's V i eta kwadrat to miary, które odpowiadają na pytanie 'ile?'."
    ),

    # ========================================================================
    # Sekcja 1: Motywacja
    # ========================================================================
    lc_h2("ch10-motywacja", "p-wartość nie mierzy ważności"),

    tagList(
      p("Wyobraź sobie, że badasz skuteczność nowego szkolenia BHP. Mierzysz czas reakcji
        operatorów przed i po szkoleniu. Wynik: ", tags$b("p = 0,03"), " — istotne!"),
      p("Ale czy szkolenie cokolwiek zmieniło? Tego p nie mówi.",
        " Przy wystarczająco dużej próbie nawet różnica 2 ms (bez żadnego praktycznego znaczenia)
        będzie 'wysoce istotna statystycznie'."),
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
          selectInput("ch10_dist_scenario", "Przykład:",
            choices = c(
              "Enzym (TŻ)"   = "TZ",
              "Ziarno (ROL)" = "ROL",
              "Reakcja (IB)" = "IB"
            ),
            selected = "TZ"
          ),
          sliderInput("ch10_d", "Cohen's d (wielkość efektu):",
                      min = 0.1, max = 1.5, value = 0.3, step = 0.05),
          sliderInput("ch10_n", "n na grupę:",
                      min = 20, max = 300, value = 50, step = 10),
          p(tags$em("Zmień n przy stałym d i obserwuj, jak zmienia się p.")),
          uiOutput("ch10_dist_hint")
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
      p("Dlaczego dzielimy przez s? Żeby różnice z różnych dziedzin
        były porównywalne. Różnica 5 cm wzrostu i różnica 5 punktów na egzaminie
        to zupełnie inne sytuacje — ale przeliczone na ", tags$em("liczbę odchyleń standardowych"),
        " dają wspólną skalę. d nie zależy od n",
        " — mówi 'jak duża jest różnica', a nie 'jak pewni jej jesteśmy'."),
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
          selectInput("ch10_d_scenario", "Przykład:",
            choices = c(
              "Jogurt (TŻ)"    = "TZ",
              "Pszenica (ROL)" = "ROL",
              "BHP (IB)"       = "IB"
            ),
            selected = "TZ"
          ),
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
      p("Wygodniejsze do interpretacji jest często r²",
        " — mówi, ", tags$em("ile procent zmienności y wyjaśnia x"),
        ". Przy r = 0,5 mamy r² = 0,25, czyli 25% zmienności wyjaśnione, a 75%
        zostaje na inne czynniki. Ważne: r mierzy tylko zależność liniową",
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
          selectInput("ch10_r_scenario", "Przykład:",
            choices = c(
              "Jogurt (TŻ)"   = "TZ",
              "Plon (ROL)"    = "ROL",
              "Wypadki (IB)"  = "IB"
            ),
            selected = "TZ"
          ),
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
          uiOutput("ch10_r_hint")
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
          selectInput("ch10_v_scenario", "Przykład:",
            choices = c(
              "Pleśń (TŻ)"      = "TZ",
              "Chwasty (ROL)"   = "ROL",
              "Szczepienie (IB)" = "IB"
            ),
            selected = "TZ"
          ),
          radioButtons("ch10_v_level", "Wielkość efektu:",
            choices = c(
              "V = 0,10 (mały)"   = "0.10",
              "V = 0,30 (średni)" = "0.30",
              "V = 0,50 (duży)"   = "0.50",
              "V = 0,70 (b. duży)" = "0.70"
            ),
            selected = "0.30"
          ),
          uiOutput("ch10_v_hint")
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
        Można je rozumieć jako 'procent zmienności wyników tłumaczony przez badany czynnik'."),
      lc_formula_box(
        p(withMathJax("\\(\\eta^2 = \\frac{SS_{\\text{między}}}{SS_{\\text{całkowite}}}\\)"))
      ),
      p("W jamovi i niektórych podręcznikach zobaczysz różne warianty: ",
        "klasyczne η², η² częściowe (partial) i η² uogólnione (ges)",
        " — różnią się tym, co dokładnie znajduje
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
          selectInput("ch10_eta_scenario", "Przykład:",
            choices = c(
              "Pasteryzacja (TŻ)" = "TZ",
              "Nawozy (ROL)"      = "ROL",
              "Zmiany (IB)"       = "IB"
            ),
            selected = "TZ"
          ),
          radioButtons("ch10_eta_level", "Wielkość efektu:",
            choices = c(
              "η² = 0,01 (mały)"    = "0.01",
              "η² = 0,06 (średni)"  = "0.06",
              "η² = 0,14 (duży)"    = "0.14",
              "η² = 0,30 (b. duży)" = "0.30"
            ),
            selected = "0.06"
          ),
          uiOutput("ch10_eta_hint")
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
        "Wartości Cohena ('mały/średni/duży') to konwencje z lat 60. — służą jako punkt
         odniesienia, nie bezwzględny standard. W wielu dziedzinach ",
        "d = 0,2 może być kluczowe",
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

  # --------------------------------------------------------------------------
  # Dane domenowe
  # --------------------------------------------------------------------------

  ch10_dist_hints <- list(
    TZ  = "Np. czas inaktywacji enzymu (s) w dwóch temperaturach blanszowania.",
    ROL = "Np. masa ziarna (g) z dwóch odmian pszenicy.",
    IB  = "Np. czas reakcji operatora (ms) na zmianie rannej vs nocnej."
  )

  ch10_d_scenarios <- list(
    TZ = list(
      "0.2" = list(
        x1 = 4.50, x2 = 4.56, s = 0.30,
        kontekst  = "pH jogurtu po fermentacji — różnica 0,06 pH między dwoma zakwasami.",
        jednostka = "pH",
        etyk1 = "Zakwas A", etyk2 = "Zakwas B"
      ),
      "0.5" = list(
        x1 = 20.0, x2 = 22.5, s = 5.0,
        kontekst  = "Wilgotność produktu suszonego (%) — bez vs ze stabilizatorem, różnica 2,5 pp.",
        jednostka = "%",
        etyk1 = "Bez stabilizatora", etyk2 = "Ze stabilizatorem"
      ),
      "0.8" = list(
        x1 = 8.0, x2 = 10.0, s = 2.5,
        kontekst  = "Czas inaktywacji enzymów (min) — blanszowanie 80°C vs 95°C, różnica 2 min.",
        jednostka = "min",
        etyk1 = "Blanszowanie 80°C", etyk2 = "Blanszowanie 95°C"
      ),
      "1.2" = list(
        x1 = 5.0, x2 = 8.6, s = 3.0,
        kontekst  = "Liczba drożdży (×10⁶/mL) — dwa szczepy hodowlane, różnica 3,6 × 10⁶/mL.",
        jednostka = "×10⁶/mL",
        etyk1 = "Szczep A", etyk2 = "Szczep B"
      )
    ),
    ROL = list(
      "0.2" = list(
        x1 = 5.0, x2 = 5.4, s = 2.0,
        kontekst  = "Plon pszenicy (t/ha) — kontrola vs nawożenie lekkie, różnica 0,4 t/ha.",
        jednostka = "t/ha",
        etyk1 = "Kontrola", etyk2 = "Nawożenie lekkie"
      ),
      "0.5" = list(
        x1 = 72, x2 = 78, s = 12,
        kontekst  = "Kiełkowalność nasion (%) — nasiona standardowe vs z podkładem, różnica 6 pp.",
        jednostka = "%",
        etyk1 = "Nasiona standardowe", etyk2 = "Nasiona z podkładem"
      ),
      "0.8" = list(
        x1 = 120, x2 = 140, s = 25,
        kontekst  = "Zawartość azotu w glebie (mg/kg) — bez nawozu vs z nawozem azotowym, różnica 20 mg/kg.",
        jednostka = "mg/kg",
        etyk1 = "Bez nawozu azotowego", etyk2 = "Z nawozem azotowym"
      ),
      "1.2" = list(
        x1 = 5.0, x2 = 8.6, s = 3.0,
        kontekst  = "Wzrost sadzonek w 30 dni (cm) — kontrola vs fitohormon, różnica 3,6 cm.",
        jednostka = "cm",
        etyk1 = "Kontrola", etyk2 = "Fitohormon"
      )
    ),
    IB = list(
      "0.2" = list(
        x1 = 72, x2 = 74, s = 10,
        kontekst  = "Tętno spoczynkowe (ud./min) — bez kofeiny vs po wypiciu herbaty czarnej, różnica 2 ud./min.",
        jednostka = "ud./min",
        etyk1 = "Bez kofeiny", etyk2 = "Po herbacie"
      ),
      "0.5" = list(
        x1 = 60, x2 = 66, s = 12,
        kontekst  = "Wynik egzaminu BHP (pkt) — bez korepetycji vs z korepetycjami, różnica 6 pkt.",
        jednostka = "pkt",
        etyk1 = "Bez korepetycji", etyk2 = "Z korepetycjami"
      ),
      "0.8" = list(
        x1 = 12.0, x2 = 14.0, s = 2.5,
        kontekst  = "Czas reakcji operatora (s) — zmiana ranna vs nocna, różnica 2 s.",
        jednostka = "s",
        etyk1 = "Zmiana ranna", etyk2 = "Zmiana nocna"
      ),
      "1.2" = list(
        x1 = 5.0, x2 = 8.6, s = 3.0,
        kontekst  = "Sen (godz.) — okres egzaminacyjny vs ferie, różnica 3,6 h.",
        jednostka = "godz.",
        etyk1 = "Egzaminy", etyk2 = "Ferie"
      )
    )
  )

  ch10_r_scenarios <- list(
    TZ = list(
      x_label   = "Temperatura fermentacji (°C)",
      y_label   = "pH jogurtu po 24 h",
      seed      = 101,
      x_center  = 42, x_scale = 5,
      y_center  = 4.5, y_scale = 0.2,
      r_sign    = 1
    ),
    ROL = list(
      x_label   = "Opad atmosferyczny (mm)",
      y_label   = "Plon pszenicy (t/ha)",
      seed      = 202,
      x_center  = 450, x_scale = 80,
      y_center  = 5.0, y_scale = 0.8,
      r_sign    = 1
    ),
    IB = list(
      x_label   = "Godziny szkolenia BHP",
      y_label   = "Wypadki na 100 pracowników",
      seed      = 303,
      x_center  = 20, x_scale = 8,
      y_center  = 8.0, y_scale = 2.0,
      r_sign    = -1
    )
  )

  ch10_r_hints <- list(
    TZ  = "Korelacja między temperaturą fermentacji a pH jogurtu.",
    ROL = "Korelacja między opadem atmosferycznym a plonem pszenicy.",
    IB  = "Korelacja ujemna: więcej szkoleń BHP → mniej wypadków."
  )

  ch10_v_scenarios <- list(
    TZ = list(
      grp_a = "Opakowanie A", grp_b = "Opakowanie B",
      stan_pos = "Pleśń", stan_neg = "Brak pleśni",
      hint = "Opakowanie a pojawienie się pleśni na produkcie."
    ),
    ROL = list(
      grp_a = "Odmiana A", grp_b = "Odmiana B",
      stan_pos = "Zachwaszczenie powyżej progu", stan_neg = "Zachwaszczenie poniżej progu",
      hint = "Odmiana rośliny a przekroczenie progu zachwaszczenia pola."
    ),
    IB = list(
      grp_a = "Zaszczepieni", grp_b = "Niezaszczepieni",
      stan_pos = "Infekcja", stan_neg = "Brak infekcji",
      hint = "Zaszczepieni vs niezaszczepieni — odsetek infekcji."
    )
  )

  ch10_v_examples <- list(
    "0.10" = list(p_a = 0.45, p_b = 0.55),
    "0.30" = list(p_a = 0.35, p_b = 0.65),
    "0.50" = list(p_a = 0.25, p_b = 0.75),
    "0.70" = list(p_a = 0.15, p_b = 0.85)
  )

  ch10_eta_scenarios <- list(
    TZ = list(
      grp    = c("Metoda A", "Metoda B", "Metoda C"),
      y_lab  = "Liczba kolonii bakterii (jedn.)",
      mu_ctr = 50, s = 10
    ),
    ROL = list(
      grp    = c("Nawóz A", "Nawóz B", "Nawóz C"),
      y_lab  = "Plon pszenicy (t/ha)",
      mu_ctr = 5.0, s = 1.5
    ),
    IB = list(
      grp    = c("Zmiana ranna", "Zmiana popołudniowa", "Zmiana nocna"),
      y_lab  = "Liczba wypadków na 100 pracowników (rocznie)",
      mu_ctr = 12, s = 4
    )
  )

  ch10_eta_examples <- list(
    "0.01" = list(kontekst = "Grupy dają niemal identyczne wyniki — czynnik symboliczny."),
    "0.06" = list(kontekst = "Grupy zauważalnie się różnią, ale rozrzut wewnątrz grup nadal dominuje."),
    "0.14" = list(kontekst = "Czynnik wyraźnie liczy się — 14% zmienności tłumaczy badany czynnik."),
    "0.30" = list(kontekst = "Dominujący efekt — czynnik wyjaśnia 30% wszystkich różnic.")
  )

  # --------------------------------------------------------------------------
  # Ryc. 10.1
  # --------------------------------------------------------------------------

  output$ch10_dist_hint <- renderUI({
    req(input$ch10_dist_scenario)
    p(tags$em(ch10_dist_hints[[input$ch10_dist_scenario]]))
  })

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

  # --------------------------------------------------------------------------
  # Ryc. 10.2: Cohen's d w surowych liczbach
  # --------------------------------------------------------------------------

  output$ch10_d_plot <- renderPlot({
    req(input$ch10_d_level, input$ch10_d_scenario)
    e <- ch10_d_scenarios[[input$ch10_d_scenario]][[input$ch10_d_level]]
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
    req(input$ch10_d_level, input$ch10_d_scenario)
    e <- ch10_d_scenarios[[input$ch10_d_scenario]][[input$ch10_d_level]]
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

  # --------------------------------------------------------------------------
  # Ryc. 10.3: r w surowych liczbach
  # --------------------------------------------------------------------------

  output$ch10_r_hint <- renderUI({
    req(input$ch10_r_scenario)
    p(tags$em(ch10_r_hints[[input$ch10_r_scenario]]))
  })

  output$ch10_r_plot <- renderPlot({
    req(input$ch10_r_level, input$ch10_r_scenario)
    r_target <- as.numeric(input$ch10_r_level)
    sc <- ch10_r_scenarios[[input$ch10_r_scenario]]
    set.seed(sc$seed)
    n_pts <- 50
    z <- rnorm(n_pts)
    e <- rnorm(n_pts)
    x_raw <- z
    y_raw <- sc$r_sign * r_target * z + sqrt(1 - r_target^2) * e
    x <- sc$x_center + sc$x_scale * x_raw
    y <- sc$y_center + sc$y_scale * y_raw

    df <- data.frame(x = x, y = y)
    r_emp <- cor(df$x, df$y)

    ggplot(df, aes(x = x, y = y)) +
      geom_smooth(method = "lm", se = FALSE, color = upwr_reference,
                  linewidth = 1, formula = y ~ x) +
      geom_point(color = col_effect, alpha = 0.7, size = 2.5) +
      labs(x = sc$x_label,
           y = sc$y_label,
           subtitle = paste0("r empiryczne = ", round(r_emp, 2),
                             "  (zadane |r| = ", r_target, ")"))
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

  # --------------------------------------------------------------------------
  # Ryc. 10.4: Cramér's V w 2×2
  # --------------------------------------------------------------------------

  output$ch10_v_hint <- renderUI({
    req(input$ch10_v_scenario)
    p(tags$em(ch10_v_scenarios[[input$ch10_v_scenario]]$hint))
  })

  output$ch10_v_plot <- renderPlot({
    req(input$ch10_v_level, input$ch10_v_scenario)
    e  <- ch10_v_examples[[input$ch10_v_level]]
    sc <- ch10_v_scenarios[[input$ch10_v_scenario]]
    df <- data.frame(
      grupa = factor(c(sc$grp_a, sc$grp_a, sc$grp_b, sc$grp_b),
                     levels = c(sc$grp_a, sc$grp_b)),
      stan  = factor(c(sc$stan_pos, sc$stan_neg, sc$stan_pos, sc$stan_neg),
                     levels = c(sc$stan_neg, sc$stan_pos)),
      pct   = c(e$p_a, 1 - e$p_a, e$p_b, 1 - e$p_b)
    )
    ggplot(df, aes(x = grupa, y = pct, fill = stan)) +
      geom_col(width = 0.55, alpha = 0.9) +
      geom_text(aes(label = paste0(round(100 * pct), "%")),
                position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold", size = 5) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(
        values = setNames(c(col_h0, col_reject), c(sc$stan_neg, sc$stan_pos))
      ) +
      labs(x = NULL, y = "Odsetek osób w grupie", fill = NULL) +
      theme(legend.position = "bottom")
  })

  output$ch10_v_table <- renderUI({
    req(input$ch10_v_level, input$ch10_v_scenario)
    e  <- ch10_v_examples[[input$ch10_v_level]]
    sc <- ch10_v_scenarios[[input$ch10_v_scenario]]
    diff_pp <- round(100 * (e$p_b - e$p_a))
    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        style = "margin-top: 8px;",
        tags$thead(tags$tr(
          tags$th("Grupa"),
          tags$th(paste0(sc$stan_pos, " (%)")),
          tags$th(paste0(sc$stan_neg, " (%)")),
          tags$th("V")
        )),
        tags$tbody(
          tags$tr(
            tags$td(sc$grp_a),
            tags$td(paste0(round(100 * e$p_a), "%")),
            tags$td(paste0(round(100 * (1 - e$p_a)), "%")),
            tags$td(rowspan = 2, input$ch10_v_level)
          ),
          tags$tr(
            tags$td(sc$grp_b),
            tags$td(paste0(round(100 * e$p_b), "%")),
            tags$td(paste0(round(100 * (1 - e$p_b)), "%"))
          )
        )
      ),
      p(style = "margin-top: 8px;",
        tags$em(paste0("Różnica między grupami: ", diff_pp, " pp. — ", sc$hint)))
    )
  })

  # --------------------------------------------------------------------------
  # Ryc. 10.5: η² w ANOVA — trzy grupy
  # --------------------------------------------------------------------------

  output$ch10_eta_hint <- renderUI({
    req(input$ch10_eta_scenario)
    sc <- ch10_eta_scenarios[[input$ch10_eta_scenario]]
    p(tags$em(paste0("3 grupy (", paste(sc$grp, collapse = " / "), ") × ", sc$y_lab, ".")))
  })

  ch10_eta_means <- function(eta, mu_ctr, s) {
    delta <- sqrt(3 * eta * s^2 / (2 * (1 - eta)))
    c(mu_ctr - delta, mu_ctr, mu_ctr + delta)
  }

  output$ch10_eta_plot <- renderPlot({
    req(input$ch10_eta_level, input$ch10_eta_scenario)
    eta <- as.numeric(input$ch10_eta_level)
    sc  <- ch10_eta_scenarios[[input$ch10_eta_scenario]]
    mus <- ch10_eta_means(eta, sc$mu_ctr, sc$s)
    set.seed(202)
    n_per <- 30
    df <- data.frame(
      grupa = factor(rep(sc$grp, each = n_per), levels = sc$grp),
      y = c(rnorm(n_per, mus[1], sc$s),
            rnorm(n_per, mus[2], sc$s),
            rnorm(n_per, mus[3], sc$s))
    )
    ggplot(df, aes(x = grupa, y = y, fill = grupa)) +
      geom_boxplot(alpha = 0.5, outlier.alpha = 0.5) +
      geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
      scale_fill_upwr() +
      labs(x = NULL, y = sc$y_lab, fill = NULL) +
      theme(legend.position = "none")
  })

  output$ch10_eta_table <- renderUI({
    req(input$ch10_eta_level, input$ch10_eta_scenario)
    eta <- as.numeric(input$ch10_eta_level)
    sc  <- ch10_eta_scenarios[[input$ch10_eta_scenario]]
    e   <- ch10_eta_examples[[input$ch10_eta_level]]
    mus <- ch10_eta_means(eta, sc$mu_ctr, sc$s)
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
            tags$td(sc$grp[1]), tags$td(round(mus[1], 1)), tags$td(sc$s),
            tags$td(rowspan = 3, input$ch10_eta_level),
            tags$td(rowspan = 3, paste0(pct, "%"))
          ),
          tags$tr(tags$td(sc$grp[2]), tags$td(round(mus[2], 1)), tags$td(sc$s)),
          tags$tr(tags$td(sc$grp[3]), tags$td(round(mus[3], 1)), tags$td(sc$s))
        )
      ),
      p(style = "margin-top: 8px;", tags$em(e$kontekst))
    )
  })

}
