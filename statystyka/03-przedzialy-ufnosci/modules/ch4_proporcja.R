# ============================================================================
# CHAPTER 4: Przedzial dla proporcji
# ============================================================================

ch4_ui <- list(
  id    = "ch-proporcja",
  num   = "04",
  title = "Przedział dla proporcji",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 04 · Przedziały ufności",
      num    = "04",
      title  = "Przedział dla proporcji.",
      lead   = "Umiemy już budować przedział dla średniej.
                A co, gdy interesuje nas odsetek (proporcja)?"
    ),

    lc_h2("ch4-wzor", "Wzór"),

    tagList(
      p("Często chcemy oszacować odsetek — jaki procent studentów zdał egzamin,
        jaki odsetek wyborców głosuje na partię X, jaki procent produktów jest wadliwy."),
      p("Estymator punktowy to proporcja z próby:"),
      lc_formula_box(
        withMathJax("$$\\hat{p} = \\frac{x}{n}$$")
      ),
      p("Najprostszy przedział ufności dla proporcji to ", tags$b("przedział Walda"), ":"),
      lc_formula_box(
        withMathJax("$$CI = \\hat{p} \\pm z^*_{\\alpha/2} \\cdot \\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}$$")
      ),
      p("Składniki:"),
      tags$ul(
        tags$li(withMathJax("\\(\\hat{p}\\)"),
                " — proporcja z próby (środek przedziału)"),
        tags$li(withMathJax("\\(\\sqrt{\\hat{p}(1-\\hat{p})/n}\\)"),
                " — błąd standardowy proporcji"),
        tags$li(withMathJax("\\(z^*\\)"),
                " — wartość krytyczna z rozkładu normalnego (dla 95% ≈ 1.96)")
      ),
      p(tags$b("Dlaczego z, a nie t?"),
        " Bo proporcja — inaczej niż średnia — nie wymaga osobnego oszacowania
        \"odchylenia standardowego\". Wariancja proporcji to ", withMathJax("\\(p(1-p)\\)"),
        ", więc jest jednoznacznie wyznaczona przez samą ", withMathJax("\\(p\\)"), "."),
      p("Uwaga — Wald nie zawsze działa dobrze. Gdy ",
        withMathJax("\\(n\\)"), " jest małe lub ", withMathJax("\\(\\hat{p}\\)"),
        " bardzo bliskie 0 lub 1, przedział Walda może mieć zaskakująco niskie
        pokrycie. W takich sytuacjach lepiej użyć ", tags$b("przedziału Wilsona"),
        ", który koryguje wzór. W tym wykładzie skupiamy się na Waldzie —
        bo łatwo go zrozumieć, a w przykładach trzymamy się \"bezpiecznych\"
        wartości ", withMathJax("\\(np \\geq 10\\)"), " i ",
        withMathJax("\\(n(1-p) \\geq 10\\)"), ".")
    ),

    inline_callout(label = "W jamovi", color = "wskazowka",
      tagList(
        "Analyses → Frequencies → 2 Outcomes — Binomial test → przeciągnij
         zmienną binarną (np. zdany/niezdany) do pola zmiennych → zaznacz
         Confidence interval (domyślnie 95%, metoda Cloppera-Pearsona
         — bezpieczniejsza niż Wald). W tabeli odczytasz kolumny ",
        tags$code("Proportion"), ", ", tags$code("Lower"), ", ",
        tags$code("Upper"), "."
      )
    ),

    lc_h2("ch4-budowa", "Budowa przedziału — krok po kroku"),

    tagList(
      p("Zobaczmy, jak z konkretnej próby (50 odpowiedzi TAK/NIE) powstaje
        przedział ufności dla proporcji. Pytamy 50 studentów, czy zdali
        egzamin, i estymujemy odsetek zdających w całej populacji.")
    ),

    figure_panel(
      label = "Ryc. 4.1", title = "Konstruowanie przedziału",
      full_width = TRUE,
      div(class = "step-buttons",
        actionButton("ch4_step1", "1. Próba",     class = "lc-btn-outline"),
        actionButton("ch4_step2", "2. p̂",        class = "lc-btn-outline"),
        actionButton("ch4_step3", "3. ± SE",      class = "lc-btn-outline"),
        actionButton("ch4_step4", "4. Przedział", class = "lc-btn-outline")
      ),
      lc_inline_row(gap = "md",
        actionButton("ch4_step_new_sample", "↻ Nowa próba",
                     class = "lc-btn-secondary-outline lc-btn-sm")
      ),
      plotOutput("ch4_step_plot", height = "340px"),
      uiOutput("ch4_step_explanation")
    ),

    lc_h2("ch4-roznica", "Budowa przedziału dla różnicy proporcji"),

    tagList(
      p("CI dla różnicy dwóch proporcji buduje się analogicznie do różnicy
        średnich — trzeba połączyć niepewność z obu prób:"),
      lc_formula_box(
        withMathJax("$$CI = (\\hat{p}_1 - \\hat{p}_2) \\pm z^* \\cdot \\sqrt{\\frac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\frac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}}$$")
      ),
      p("Porównamy odsetek osób zadowolonych z usługi w dwóch grupach —
        po 60 osób w każdej.")
    ),

    figure_panel(
      label = "Ryc. 4.2", title = "Konstruowanie CI dla różnicy",
      full_width = TRUE,
      div(class = "step-buttons",
        actionButton("ch4_dstep1", "1. Dwie próby",   class = "lc-btn-outline"),
        actionButton("ch4_dstep2", "2. Dwie p̂",      class = "lc-btn-outline"),
        actionButton("ch4_dstep3", "3. Różnica",      class = "lc-btn-outline"),
        actionButton("ch4_dstep4", "4. ± SE",         class = "lc-btn-outline"),
        actionButton("ch4_dstep5", "5. Przedział",    class = "lc-btn-outline")
      ),
      lc_inline_row(gap = "md",
        actionButton("ch4_dstep_new_sample", "↻ Nowe próby",
                     class = "lc-btn-secondary-outline lc-btn-sm")
      ),
      plotOutput("ch4_dstep_plot", height = "420px"),
      uiOutput("ch4_dstep_explanation")
    ),

    lc_h2("ch4-case-studies", "Case studies — jak interpretować CI w praktyce"),

    tagList(
      p("Poniżej kilka realistycznych sytuacji. W każdej budujesz CI krok po kroku
        (jak w poprzednich sekcjach), a na końcu weryfikujesz dwie hipotezy:
        jedną, która jest prawdziwa, i jedną, która nie jest. Klikaj nagłówki,
        żeby rozwijać case'y.")
    ),

    lc_h3("A. Przedział dla jednej proporcji"),

    tags$details(class = "case-study", open = NA,
      tags$summary(
        span(class = "case-icon", "\U0001f5f3️"),
        "A1. Sondaż wyborczy — czytanie pojedynczego CI"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Pracownia sondażowa zapytała 400 wyborców, czy poprze partię X.
            212 odpowiedzi TAK (czyli ", withMathJax("\\(\\hat{p} = 0.53\\)"),
            "). Zbudujmy CI dla poparcia w populacji i sprawdźmy dwie hipotezy.")
        ),
        uiOutput("ch4_caseA1_buttons"),
        plotOutput("ch4_caseA1_plot", height = "260px"),
        uiOutput("ch4_caseA1_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f50d"),
        "A2. Ten sam odsetek, trzy różne wielkości próby"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Porównaj trzy badania mierzące odsetek wadliwych produktów
            w fabryce. W każdym ", withMathJax("\\(\\hat{p} = 0.08\\)"), " (8%),
            ale ", tags$b("n różne"), " (50, 200, 1000). Dodawaj CI jeden
            po drugim i patrz, jak się zwężają.")
        ),
        uiOutput("ch4_caseA2_buttons"),
        plotOutput("ch4_caseA2_plot", height = "260px"),
        uiOutput("ch4_caseA2_explain")
      )
    ),

    lc_h3("B. Przedział dla różnicy proporcji"),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f48a"),
        "B1. Lek vs placebo — odsetek wyleczonych"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Badamy nowy lek przeciwbólowy.
            ", tags$b("Lek:"), " 200 pacjentów, 124 zgłosiło ustąpienie bólu (62%).
            ", tags$b("Placebo:"), " 200 pacjentów, 84 zgłosiło ustąpienie bólu (42%).")
        ),
        uiOutput("ch4_caseB1_buttons"),
        plotOutput("ch4_caseB1_plot", height = "380px"),
        uiOutput("ch4_caseB1_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f3ed"),
        "B2. Dwie linie produkcyjne — odsetek braków"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Porównujesz dwie linie produkcyjne pod kątem odsetka wadliwych produktów.
            ", tags$b("Linia A:"), " skontrolowano 250, 22 wadliwych (8.8%).
            ", tags$b("Linia B:"), " skontrolowano 250, 18 wadliwych (7.2%).")
        ),
        uiOutput("ch4_caseB2_buttons"),
        plotOutput("ch4_caseB2_plot", height = "380px"),
        uiOutput("ch4_caseB2_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "⚠️"),
        "B3. Pułapka małej próby"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Pilotaż nowej procedury BHP w dwóch zakładach.
            ", tags$b("Zakład A:"), " 30 pracowników, 6 miało wypadek (20%).
            ", tags$b("Zakład B:"), " 30 pracowników, 9 miało wypadek (30%).
            Różnica wygląda na dużą — ale czy możemy z 95% ufnością
            powiedzieć, że procedura A jest skuteczniejsza?")
        ),
        uiOutput("ch4_caseB3_buttons"),
        plotOutput("ch4_caseB3_plot", height = "380px"),
        uiOutput("ch4_caseB3_explain")
      )
    ),

    lc_h3("C. Wiele grup — forest plot"),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f3e5"),
        "C1. Cztery szpitale — odsetek powikłań pooperacyjnych"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Porównujesz odsetek powikłań po tej samej operacji w czterech szpitalach.
            Dla każdego masz liczbę wykonanych zabiegów i liczbę powikłań.
            Dodawaj CI jeden po drugim i obserwuj.")
        ),
        uiOutput("ch4_caseC1_buttons"),
        plotOutput("ch4_caseC1_plot", height = "320px"),
        uiOutput("ch4_caseC1_explain")
      )
    ),

    lc_chapter_next(
      num       = "05",
      title     = "Co wpływa na szerokość?",
      lead      = "co decyduje o szerokości przedziału",
      target_id = "ch-czynniki"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  # ==========================================================================
  # WIDGET 1: Budowa przedzialu dla proporcji krok po kroku
  # ==========================================================================
  ch4_step <- reactiveVal(0)
  ch4_step_sample <- reactiveVal(NULL)

  # Generuje probke n bernoulli z true_p = 0.6 (z drobna wariancja)
  generate_step_prop_sample <- function() {
    set.seed(sample.int(.Machine$integer.max, 1))
    n <- 50
    rbinom(n, 1, 0.6)  # 50 odpowiedzi TAK/NIE
  }

  observeEvent(input$ch4_step1, {
    if (is.null(ch4_step_sample())) ch4_step_sample(generate_step_prop_sample())
    ch4_step(1)
  })
  observeEvent(input$ch4_step2, {
    if (is.null(ch4_step_sample())) ch4_step_sample(generate_step_prop_sample())
    ch4_step(2)
  })
  observeEvent(input$ch4_step3, {
    if (is.null(ch4_step_sample())) ch4_step_sample(generate_step_prop_sample())
    ch4_step(3)
  })
  observeEvent(input$ch4_step4, {
    if (is.null(ch4_step_sample())) ch4_step_sample(generate_step_prop_sample())
    ch4_step(4)
  })
  observeEvent(input$ch4_step_new_sample, {
    ch4_step_sample(generate_step_prop_sample())
    ch4_step(1)
  })

  output$ch4_step_plot <- renderPlot({
    step <- ch4_step()
    samp <- ch4_step_sample()
    if (step == 0 || is.null(samp)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Kliknij '1. Próba' żeby zacząć",
                   size = 6, color = upwr_reference) +
          theme_void()
      )
    }

    n <- length(samp)
    x <- sum(samp)
    phat <- x / n
    z_star <- qnorm(0.975)
    se <- sqrt(phat * (1 - phat) / n)
    me <- z_star * se

    # ---- LEWY PANEL: slupki TAK / NIE (liczebnosci bezwzgledne) ----
    bar_df <- data.frame(
      val = factor(c("NIE", "TAK"), levels = c("NIE", "TAK")),
      count = c(n - x, x)
    )
    p_left <- ggplot(bar_df, aes(x = val, y = count, fill = val)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = count), vjust = -0.4, fontface = "bold",
                size = 5, color = upwr_secondary) +
      scale_fill_manual(values = c("NIE" = col_miss, "TAK" = col_ci),
                        guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "Liczebność") +
      theme_upwr() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())

    # ---- PRAWY PANEL: os proporcji z p_hat, SE, CI ----
    # Oddzielne poziomy Y - kazdy element na swojej linii
    Y_EST <- 0.30
    Y_SE  <- 0.05
    Y_CI  <- -0.25

    # Wyszarzanie poprzednich elementow
    c_faded <- "#adb5bd"
    c_est <- if (step >= 3) c_faded else col_estimate
    c_se  <- if (step >= 4) c_faded else col_hit

    p_right <- ggplot() +
      xlim(0, 1) +
      ylim(-0.6, 0.6) +
      labs(x = "Proporcja", y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Krok 2+: pionowa linia prowadzaca + punkt p_hat
    if (step >= 2) {
      p_right <- p_right +
        geom_vline(xintercept = phat, color = "#adb5bd",
                   linewidth = 0.8, linetype = "dotted") +
        geom_point(aes(x = phat, y = Y_EST), color = c_est,
                   size = 7, shape = 18) +
        annotate("text", x = phat, y = Y_EST - 0.13,
                 label = "p̂",
                 color = c_est, fontface = "bold", size = 5)
    }

    # Krok 3+: waski przedzial SE
    if (step >= 3) {
      p_right <- p_right +
        geom_errorbarh(aes(xmin = phat - se, xmax = phat + se, y = Y_SE),
                       height = 0.08, color = c_se, linewidth = 1.8) +
        annotate("text", x = phat, y = Y_SE - 0.12,
                 label = "± SE",
                 color = c_se, fontface = "bold", size = 4.2)
    }

    # Krok 4: pelen CI
    if (step >= 4) {
      p_right <- p_right +
        geom_errorbarh(aes(xmin = phat - me, xmax = phat + me, y = Y_CI),
                       height = 0.12, color = col_ci, linewidth = 2.2) +
        annotate("text", x = phat, y = Y_CI - 0.13,
                 label = "95% CI",
                 color = col_ci, fontface = "bold", size = 5)
    }

    library(patchwork)
    p_left + p_right + plot_layout(widths = c(1, 2.5))
  })

  output$ch4_step_explanation <- renderUI({
    step <- ch4_step()
    samp <- ch4_step_sample()
    if (step == 0 || is.null(samp)) return(NULL)

    n <- length(samp)
    x <- sum(samp)
    phat <- x / n
    z_star <- qnorm(0.975)
    se <- sqrt(phat * (1 - phat) / n)
    me <- z_star * se

    switch(as.character(step),
      "1" = lc_feedback(type = "info",
        p(tags$strong("Krok 1:"), " Próba.",
          " Mamy ", tags$b(n), " obserwacji TAK/NIE: ", tags$b(x), " razy TAK, ",
          tags$b(n - x), " razy NIE. Niebieskie punkty (TAK) po prawej, czerwone (NIE)
          po lewej. Sama tabelka liczb — jeszcze nie zaczęliśmy estymować.")
      ),
      "2" = lc_feedback(type = "info",
        p(tags$strong("Krok 2:"), " Estymacja punktowa p̂.",
          " Liczymy proporcję z próby:"),
        p(withMathJax(paste0("\\(\\hat{p} = \\frac{x}{n} = \\frac{", x, "}{", n,
                             "} = ", round(phat, 3), "\\)"))),
        p("To nasza najlepsza pojedyncza wartość — ale potrzebujemy
          jeszcze wiedzieć, jak bardzo niepewna jest ta estymata.")
      ),
      "3" = lc_feedback(type = "info",
        p(tags$strong("Krok 3:"), " Błąd standardowy (± SE).",
          " Niepewność oszacowania proporcji liczymy ze wzoru:"),
        p(withMathJax(paste0(
          "\\(SE = \\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}} = \\sqrt{\\frac{",
          round(phat, 2), " \\cdot ", round(1 - phat, 2), "}{", n, "}} = ",
          round(se, 3), "\\)"))),
        p("Zielony pasek ± SE to zakres \"jednego odchylenia\" wokół
          p̂. Ale 95% CI to około ", tags$b("dwa SE w każdą stronę"),
          " (dokładniej: 1.96).")
      ),
      "4" = {
        lc_feedback(type = "ok",
          p(tags$strong("Krok 4:"), " Przedział ufności."),
          p("Wartość krytyczna z rozkładu normalnego: ",
            withMathJax("\\(z^* = 1.96\\)")),
          p(withMathJax(paste0("\\(ME = z^* \\cdot SE = 1.96 \\cdot ",
                               round(se, 3), " = ", round(me, 3), "\\)"))),
          p(tags$b("95% CI: ["),
            round(phat - me, 3), " ; ", round(phat + me, 3),
            tags$b("]")),
          p(tags$em("Z 95% ufnością prawdziwy odsetek w populacji leży w tym
                   przedziale. Sprawdź, jak zmienia się CI po wylosowaniu nowej próby!"))
        )
      }
    )
  })

  # ==========================================================================
  # WIDGET 2: Budowa CI dla roznicy proporcji
  # ==========================================================================
  ch4_dstep <- reactiveVal(0)
  ch4_dstep_samples <- reactiveVal(NULL)

  generate_dstep_prop_samples <- function() {
    set.seed(sample.int(.Machine$integer.max, 1))
    n1 <- 60; n2 <- 60
    list(
      g1 = rbinom(n1, 1, 0.70),  # grupa 1: 70% zadowolonych
      g2 = rbinom(n2, 1, 0.50)   # grupa 2: 50% zadowolonych
    )
  }

  observeEvent(input$ch4_dstep1, {
    if (is.null(ch4_dstep_samples())) ch4_dstep_samples(generate_dstep_prop_samples())
    ch4_dstep(1)
  })
  observeEvent(input$ch4_dstep2, {
    if (is.null(ch4_dstep_samples())) ch4_dstep_samples(generate_dstep_prop_samples())
    ch4_dstep(2)
  })
  observeEvent(input$ch4_dstep3, {
    if (is.null(ch4_dstep_samples())) ch4_dstep_samples(generate_dstep_prop_samples())
    ch4_dstep(3)
  })
  observeEvent(input$ch4_dstep4, {
    if (is.null(ch4_dstep_samples())) ch4_dstep_samples(generate_dstep_prop_samples())
    ch4_dstep(4)
  })
  observeEvent(input$ch4_dstep5, {
    if (is.null(ch4_dstep_samples())) ch4_dstep_samples(generate_dstep_prop_samples())
    ch4_dstep(5)
  })
  observeEvent(input$ch4_dstep_new_sample, {
    ch4_dstep_samples(generate_dstep_prop_samples())
    ch4_dstep(1)
  })

  output$ch4_dstep_plot <- renderPlot({
    step <- ch4_dstep()
    samples <- ch4_dstep_samples()
    if (step == 0 || is.null(samples)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Kliknij '1. Dwie próby' aby zacząć",
                   size = 6, color = upwr_reference) +
          theme_void()
      )
    }

    g1 <- samples$g1; g2 <- samples$g2
    n1 <- length(g1); n2 <- length(g2)
    x1 <- sum(g1);    x2 <- sum(g2)
    p1 <- x1 / n1;    p2 <- x2 / n2
    diff_val <- p1 - p2
    se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
    z_star <- qnorm(0.975)
    me <- z_star * se

    # ---- LEWY PANEL: slupki TAK/NIE x 2 grupy ----
    bar_df <- data.frame(
      grp = factor(rep(c("Grupa 1", "Grupa 2"), each = 2),
                   levels = c("Grupa 1", "Grupa 2")),
      val = factor(rep(c("NIE", "TAK"), 2), levels = c("NIE", "TAK")),
      count = c(n1 - x1, x1, n2 - x2, x2)
    )
    p_left <- ggplot(bar_df, aes(x = grp, y = count, fill = val)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.65) +
      geom_text(aes(label = count),
                position = position_dodge(width = 0.75),
                vjust = -0.4, fontface = "bold", size = 4.5, color = upwr_secondary) +
      scale_fill_manual(values = c("NIE" = col_miss, "TAK" = col_ci),
                        name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(x = NULL, y = "Liczebność") +
      theme_upwr() +
      theme(legend.position = "top",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())

    # ---- PRAWY GORNY PANEL: dwie p_hat na osi proporcji ----
    p_top <- ggplot() +
      xlim(0, 1) +
      ylim(0.4, 2.6) +
      labs(x = "Proporcja", y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_text(face = "bold", size = 12),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      scale_y_continuous(breaks = c(1, 2), labels = c("Grupa 1", "Grupa 2"),
                         limits = c(0.4, 2.6))

    if (step >= 2) {
      p_top <- p_top +
        geom_point(aes(x = p1, y = 1), color = col_estimate, size = 7, shape = 18) +
        annotate("text", x = p1, y = 1.45, label = paste0("p̂₁ = ", round(p1, 3)),
                 color = col_estimate, fontface = "bold", size = 4.5) +
        geom_point(aes(x = p2, y = 2), color = col_estimate, size = 7, shape = 18) +
        annotate("text", x = p2, y = 2.45, label = paste0("p̂₂ = ", round(p2, 3)),
                 color = col_estimate, fontface = "bold", size = 4.5)
    }

    # ---- PRAWY DOLNY PANEL: roznica + CI ----
    xlims_bot <- range(c(-0.5, 0.5, diff_val - 1.3 * me, diff_val + 1.3 * me))
    pad_bot <- diff(xlims_bot) * 0.08
    xlims_bot <- c(xlims_bot[1] - pad_bot, xlims_bot[2] + pad_bot)

    col_true_local <- "#9b59b6"

    p_bot <- ggplot() +
      xlim(xlims_bot) +
      ylim(-0.55, 0.55) +
      labs(x = "Różnica proporcji  —  Grupa 1 − Grupa 2",
           y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      geom_vline(xintercept = 0, color = col_true_local,
                 linewidth = 1, linetype = "dashed") +
      annotate("text", x = 0, y = 0.45, label = "0 = brak różnicy",
               color = col_true_local, fontface = "bold", size = 4, hjust = -0.1)

    if (step >= 3) {
      p_bot <- p_bot +
        geom_point(aes(x = diff_val, y = 0), color = col_estimate,
                   size = 7, shape = 18) +
        annotate("text", x = diff_val, y = -0.22,
                 label = paste0("p̂₁ − p̂₂ = ", round(diff_val, 3)),
                 color = col_estimate, fontface = "bold", size = 4.5)
    }

    if (step >= 4) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - se, xmax = diff_val + se, y = 0),
                       height = 0.08, color = col_hit, linewidth = 1.8) +
        annotate("text", x = diff_val, y = 0.17,
                 label = paste0("± SE = ±", round(se, 3)),
                 color = col_hit, fontface = "bold", size = 4)
    }

    if (step >= 5) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - me, xmax = diff_val + me, y = 0),
                       height = 0.14, color = col_ci, linewidth = 2.2, alpha = 0.6) +
        annotate("text", x = diff_val, y = -0.42,
                 label = paste0("95% CI: [", round(diff_val - me, 3),
                                " ; ", round(diff_val + me, 3), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    }

    library(patchwork)
    # Layout: lewy slupki | (prawy gora p_hat / prawy dol roznica)
    right_col <- p_top / p_bot + plot_layout(heights = c(1, 1))
    (p_left | right_col) +
      plot_layout(widths = c(1, 2)) +
      plot_annotation(title = paste0("Krok ", step, " z 5"))
  })

  output$ch4_dstep_explanation <- renderUI({
    step <- ch4_dstep()
    samples <- ch4_dstep_samples()
    if (step == 0 || is.null(samples)) return(NULL)

    g1 <- samples$g1; g2 <- samples$g2
    n1 <- length(g1); n2 <- length(g2)
    x1 <- sum(g1);    x2 <- sum(g2)
    p1 <- x1 / n1;    p2 <- x2 / n2
    diff_val <- p1 - p2
    se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
    z_star <- qnorm(0.975)
    me <- z_star * se

    switch(as.character(step),
      "1" = lc_feedback(type = "info",
        p(tags$strong("Krok 1:"), " Dwie próby.",
          " Mamy odpowiedzi TAK/NIE z dwóch grup: ", tags$b(n1),
          " osób w grupie 1 (", x1, " TAK / ", n1 - x1, " NIE) i ",
          tags$b(n2), " w grupie 2 (", x2, " TAK / ", n2 - x2, " NIE).
          Widać już, że w grupie 1 jest więcej TAKów, ale jak duża to
          różnica i czy istotna?")
      ),
      "2" = lc_feedback(type = "info",
        p(tags$strong("Krok 2:"), " Dwie proporcje.",
          " Obliczamy proporcję TAKów w każdej grupie:"),
        p(withMathJax(paste0("\\(\\hat{p}_1 = ", x1, "/", n1, " = ", round(p1, 3), "\\)"))),
        p(withMathJax(paste0("\\(\\hat{p}_2 = ", x2, "/", n2, " = ", round(p2, 3), "\\)"))),
        p("Każda proporcja ma własną niepewność — ale interesuje nas
          ", tags$b("różnica między nimi"), ".")
      ),
      "3" = lc_feedback(type = "info",
        p(tags$strong("Krok 3:"), " Różnica.",
          " Estymator punktowy różnicy:"),
        p(withMathJax(paste0("\\(\\hat{p}_1 - \\hat{p}_2 = ", round(p1, 3),
                             " - ", round(p2, 3), " = ",
                             round(diff_val, 3), "\\)"))),
        p("W dolnym panelu przenosimy się do nowej skali — ", tags$b("skali różnicy"),
          ". Punkt = nasze oszacowanie różnicy. Pionowa linia na 0 oznacza ",
          tags$em("\"gdyby różnicy nie było\""),
          ". Teraz musimy otoczyć naszą różnicę przedziałem niepewności.")
      ),
      "4" = lc_feedback(type = "info",
        p(tags$strong("Krok 4:"), " Błąd standardowy różnicy (± SE).",
          " SE różnicy proporcji łączy niepewności z obu grup:"),
        p(withMathJax(paste0(
          "\\(SE = \\sqrt{\\frac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\frac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = ",
          round(se, 3), "\\)"))),
        p(tags$b("Ważne:"), " wariancje się ", tags$em("dodają"),
          ", nie odchylenia. Dlatego SE różnicy jest mniejszy niż suma SE
          poszczególnych proporcji.")
      ),
      "5" = {
        covers_zero <- (diff_val - me <= 0) & (0 <= diff_val + me)
        lc_feedback(type = if (covers_zero) "warning" else "ok",
          p(tags$strong("Krok 5:"), " Przedział ufności dla różnicy."),
          p("Wartość krytyczna z rozkładu normalnego: ",
            withMathJax("\\(z^* = 1.96\\)")),
          p(withMathJax(paste0("\\(ME = z^* \\cdot SE = 1.96 \\cdot ",
                               round(se, 3), " = ", round(me, 3), "\\)"))),
          p(tags$b("95% CI: ["),
            round(diff_val - me, 3), " ; ", round(diff_val + me, 3),
            tags$b("]")),
          p(tags$em(if (covers_zero)
              "CI obejmuje 0 — nie możemy stwierdzić, że różnica jest istotna."
            else
              paste0("CI nie obejmuje 0 — różnica jest istotna. ",
                     "Możemy stwierdzić z 95% ufnością, że w grupie 1 odsetek TAK ",
                     "jest większy o co najmniej ", round(diff_val - me, 3), ".")))
        )
      }
    )
  })

  # ==========================================================================
  # WIDGET 3: CASE STUDIES (konstruktory krok po kroku + hipotezy)
  # ==========================================================================

  # ---- Helpery statystyczne ----
  ci_prop <- function(x, n, conf = 0.95) {
    phat <- x / n
    z_star <- qnorm(1 - (1 - conf) / 2)
    se <- sqrt(phat * (1 - phat) / n)
    me <- z_star * se
    list(phat = phat, lower = phat - me, upper = phat + me,
         me = me, se = se, z_star = z_star)
  }
  ci_diff_props <- function(x1, n1, x2, n2, conf = 0.95) {
    p1 <- x1 / n1; p2 <- x2 / n2
    se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
    z_star <- qnorm(1 - (1 - conf) / 2)
    diff <- p1 - p2
    me <- z_star * se
    list(diff = diff, lower = diff - me, upper = diff + me,
         me = me, se = se, z_star = z_star, p1 = p1, p2 = p2)
  }

  # ---- Werdykt hipotezy ----
  # dir = "gt" (CI > bound), "lt" (CI < bound)
  hypothesis_verdict <- function(lower, upper, bound, dir) {
    if (dir == "gt") {
      if (lower > bound)      "yes"
      else if (upper < bound) "no"
      else                    "maybe"
    } else {
      if (upper < bound)      "yes"
      else if (lower > bound) "no"
      else                    "maybe"
    }
  }

  verdict_class <- function(v) {
    switch(v, "yes" = "ok", "no" = "danger",
           "maybe" = "warning")
  }
  verdict_label <- function(v) {
    switch(v, "yes" = "TAK", "no" = "NIE", "maybe" = "NIEPEWNE")
  }

  col_hyp <- "#8e44ad"

  # ---- CONFIG case'ow ----
  cases_config <- list(
    A1 = list(
      type = "single_prop",
      data = list(x = 212, n = 400),
      xlab = "Poparcie dla partii X",
      steps = c("1. Próba", "2. p̂", "3. ± SE", "4. Przedział"),
      hypotheses = list(
        list(text = "Poparcie dla partii X przekracza 50% (\"progów większości\")",
             bound = 0.50, dir = "gt",
             explain_yes = "Dolna granica CI leży powyżej 50%. Możemy z 95% ufnością stwierdzić, że większość wyborców popiera partię X.",
             explain_no = "Dolna granica CI leży poniżej 50% — mimo że p̂ = 53%, niepewność sondażu nie pozwala stwierdzić z 95% ufnością że poparcie przekracza 50%."),
        list(text = "Poparcie dla partii X przekracza 60%",
             bound = 0.60, dir = "gt",
             explain_no = "Górna granica CI leży poniżej 60%. Cały CI poza obszarem hipotezy — nie ma podstaw do twierdzenia, że poparcie przekracza 60%.")
      )
    ),
    A2 = list(
      type = "compare_n_prop",
      data = list(phat = 0.08, ns = c(50, 200, 1000)),
      xlab = "Odsetek wadliwych produktów",
      steps = c("1. n = 50", "2. n = 200", "3. n = 1000"),
      hypotheses = list(
        list(text = "Odsetek wadliwych produktów przekracza 5%",
             bound = 0.05, dir = "gt",
             explain_yes = "Dla największej próby (n=1000) dolna granica CI leży powyżej 5% — z 95% ufnością odsetek wadliwych przekracza normę 5%. Zauważ: dla n=50 CI jest tak szeroki, że obejmuje również 5%, więc na małej próbie nie móglbyś nic stwierdzić.",
             explain_no = "Nawet przy n=1000 nie możemy stwierdzić z 95% pewnością, że odsetek przekracza 5%."),
        list(text = "Odsetek wadliwych produktów przekracza 12%",
             bound = 0.12, dir = "gt",
             explain_no = "Górna granica CI nawet dla n=1000 leży poniżej 12%. Dla n=50 CI sięga prawie 16% — mała próba mogłaby błędnie sugerować problem. To pokazuje, dlaczego duże n daje bardziej definitywne odpowiedzi.")
      )
    ),
    B1 = list(
      type = "diff_props",
      data = list(x1 = 124, n1 = 200, x2 = 84, n2 = 200,
                  label1 = "Lek", label2 = "Placebo"),
      xlab = "Odsetek z ustąpieniem bólu",
      steps = c("1. Próby", "2. Dwie p̂", "3. Różnica", "4. ± SE", "5. Przedział"),
      hypotheses = list(
        list(text = "Lek działa skuteczniej niż placebo (różnica > 0)",
             bound = 0, dir = "gt",
             explain_yes = "Cały CI dla różnicy leży powyżej 0. Lek rzeczywiście pomaga skuteczniej niż placebo — różnica jest istotna statystycznie."),
        list(text = "Lek poprawia skuteczność o więcej niż 25 punktów procentowych",
             bound = 0.25, dir = "gt",
             explain_no = "Górna granica CI dla różnicy leży poniżej 0.25. Lek działa, ale poprawa skuteczności względem placebo jest mniejsza niż 25 pkt proc.")
      )
    ),
    B2 = list(
      type = "diff_props",
      data = list(x1 = 22, n1 = 250, x2 = 18, n2 = 250,
                  label1 = "Linia A", label2 = "Linia B"),
      xlab = "Odsetek wadliwych",
      steps = c("1. Próby", "2. Dwie p̂", "3. Różnica", "4. ± SE", "5. Przedział"),
      hypotheses = list(
        list(text = "Linia A produkuje więcej braków niż linia B (różnica > 0)",
             bound = 0, dir = "gt",
             explain_no = "CI dla różnicy obejmuje 0 — mimo że p̂₁ (8.8%) jest wyższe niż p̂₂ (7.2%), nie możemy z 95% ufnością stwierdzić, że linia A jest gorsza. Różnica może być efektem przypadku."),
        list(text = "Różnica w odsetku braków między liniami jest mniejsza niż 5 pkt proc",
             bound = 0.05, dir = "lt",
             explain_yes = "Górna granica CI dla różnicy leży poniżej 0.05. Możemy być pewni, że nawet jeśli któraś linia jest gorsza, to różnica nie przekracza 5 pkt proc.")
      )
    ),
    B3 = list(
      type = "diff_props",
      data = list(x1 = 6, n1 = 30, x2 = 9, n2 = 30,
                  label1 = "Zakład A", label2 = "Zakład B"),
      xlab = "Odsetek wypadków",
      steps = c("1. Próby", "2. Dwie p̂", "3. Różnica", "4. ± SE", "5. Przedział"),
      hypotheses = list(
        list(text = "Zakład A jest bezpieczniejszy niż B (różnica < 0)",
             bound = 0, dir = "lt",
             explain_no = "Mimo że p̂₁ = 20% jest wyraźnie mniejsze od p̂₂ = 30%, CI dla różnicy obejmuje 0. Próba 30 osób w każdym zakładzie to za mało, żeby z 95% ufnością stwierdzić, który jest bezpieczniejszy. To klasyczna pułapka: \"duża\" różnica w punktach procentowych może być statystycznie nieistotna przy małej próbie."),
        list(text = "Różnica wypadkowości między zakładami przekracza 30 pkt proc",
             bound = 0.30, dir = "lt",
             explain_yes = "Górna granica CI leży wyraźnie poniżej 0.30 — możemy wykluczyć aż tak dużą różnicę, ale mała próba nie pozwala nam dokładnie wskazać, jaka ona jest.")
      )
    ),
    C1 = list(
      type = "forest_prop",
      data = list(
        groups = c("Szpital A", "Szpital B", "Szpital C", "Szpital D"),
        x = c(12, 18, 9, 35),
        n = c(150, 180, 160, 170)
      ),
      xlab = "Odsetek powikłań pooperacyjnych",
      steps = c("1. Liczby", "2. Proporcje", "3. CI"),
      hypotheses = list(
        list(kind = "pairwise",
             text = "Które szpitale różnią się istotnie odsetkiem powikłań?",
             unit = "")
      )
    )
  )

  # ---- Reactive state per case ----
  ch4_case_state <- reactiveValues()
  for (cid in names(cases_config)) {
    ch4_case_state[[cid]] <- 0
  }

  # ---- Helper: pasek CI dla pojedynczej proporcji (slupki + panel CI) ----
  plot_single_prop_step <- function(data, step, xlab,
                                     hypothesis = NULL, title = NULL) {
    x <- data$x; n <- data$n
    ci <- ci_prop(x, n)
    phat <- ci$phat; se <- ci$se; me <- ci$me

    # ---- LEWY PANEL: slupki TAK / NIE ----
    bar_df <- data.frame(
      val = factor(c("NIE", "TAK"), levels = c("NIE", "TAK")),
      count = c(n - x, x)
    )
    p_left <- ggplot(bar_df, aes(x = val, y = count, fill = val)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = count), vjust = -0.4, fontface = "bold",
                size = 5, color = upwr_secondary) +
      scale_fill_manual(values = c("NIE" = col_miss, "TAK" = col_ci),
                        guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "Liczebność") +
      theme_upwr() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())

    # ---- PRAWY PANEL: os proporcji ----
    xlims <- c(0, 1)
    if (!is.null(hypothesis)) {
      xlims <- range(c(xlims, hypothesis$bound))
      xlims[1] <- max(0, xlims[1])
      xlims[2] <- min(1, xlims[2])
    }

    p_right <- ggplot() +
      xlim(xlims) +
      ylim(-0.6, 0.6) +
      labs(x = xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Obszar hipotezy
    if (!is.null(hypothesis)) {
      if (hypothesis$dir == "gt") {
        p_right <- p_right + annotate("rect",
                          xmin = hypothesis$bound, xmax = Inf,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      } else {
        p_right <- p_right + annotate("rect",
                          xmin = -Inf, xmax = hypothesis$bound,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      }
      p_right <- p_right +
        geom_vline(xintercept = hypothesis$bound, color = col_hyp,
                   linewidth = 1, linetype = "solid") +
        annotate("text", x = hypothesis$bound, y = 0.5,
                 label = paste0(if (hypothesis$dir == "gt") "≥ " else "≤ ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    }

    # Krok 2+: punkt p_hat
    if (step >= 2) {
      p_right <- p_right +
        geom_point(aes(x = phat, y = 0), color = col_estimate, size = 7, shape = 18) +
        annotate("text", x = phat, y = -0.22,
                 label = paste0("p̂ = ", round(phat, 3)),
                 color = col_estimate, fontface = "bold", size = 4.8)
    }

    # Krok 3+: SE
    if (step >= 3) {
      p_right <- p_right +
        geom_errorbarh(aes(xmin = phat - se, xmax = phat + se, y = 0),
                       height = 0.08, color = col_hit, linewidth = 1.8) +
        annotate("text", x = phat, y = 0.20,
                 label = paste0("± SE = ±", round(se, 3)),
                 color = col_hit, fontface = "bold", size = 4)
    }

    # Krok 4: CI
    if (step >= 4) {
      p_right <- p_right +
        geom_errorbarh(aes(xmin = phat - me, xmax = phat + me, y = 0),
                       height = 0.14, color = col_ci, linewidth = 2.2, alpha = 0.6) +
        annotate("text", x = phat, y = -0.45,
                 label = paste0("95% CI: [", round(phat - me, 3),
                                " ; ", round(phat + me, 3), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    }

    library(patchwork)
    p_left + p_right + plot_layout(widths = c(1, 2.5))
  }

  # ---- Plot dla compare_n_prop (te same dane, rozne n) ----
  plot_compare_n_prop_step <- function(data, step, xlab,
                                        hypothesis = NULL, title = NULL) {
    phat <- data$phat
    ns <- data$ns
    k <- length(ns)

    # CI dla kazdego n
    cis <- lapply(ns, function(n) {
      x <- round(phat * n)
      ci_prop(x, n)
    })

    # Limity X
    xmin <- min(sapply(cis, function(c) c$lower))
    xmax <- max(sapply(cis, function(c) c$upper))
    xlims <- c(max(0, xmin - 0.05), min(1, xmax + 0.05))
    if (!is.null(hypothesis)) {
      xlims <- range(c(xlims, hypothesis$bound))
      xlims[1] <- max(0, xlims[1])
      xlims[2] <- min(1, xlims[2])
    }

    y_positions <- seq_len(k)
    df <- data.frame(
      y = y_positions,
      n = ns,
      phat = sapply(cis, function(c) c$phat),
      lower = sapply(cis, function(c) c$lower),
      upper = sapply(cis, function(c) c$upper),
      label = paste0("n = ", ns)
    )

    p <- ggplot() +
      xlim(xlims) +
      ylim(0.3, k + 0.7) +
      labs(x = xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    p <- p +
      annotate("text", x = xlims[1], y = y_positions,
               label = df$label, hjust = 0, fontface = "bold", size = 4.5,
               color = upwr_secondary)

    if (!is.null(hypothesis)) {
      if (hypothesis$dir == "gt") {
        p <- p + annotate("rect",
                          xmin = hypothesis$bound, xmax = Inf,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      } else {
        p <- p + annotate("rect",
                          xmin = -Inf, xmax = hypothesis$bound,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      }
      p <- p +
        geom_vline(xintercept = hypothesis$bound, color = col_hyp,
                   linewidth = 1, linetype = "solid") +
        annotate("text", x = hypothesis$bound, y = k + 0.45,
                 label = paste0(if (hypothesis$dir == "gt") "≥ " else "≤ ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    }

    # Pokazuj progresywnie: krok i = pierwsze i CI
    rows_df <- df[seq_len(min(step, k)), , drop = FALSE]
    if (nrow(rows_df) > 0) {
      p <- p +
        geom_point(data = rows_df, aes(x = phat, y = y),
                   color = col_estimate, size = 5, shape = 18) +
        geom_errorbarh(data = rows_df,
                       aes(xmin = lower, xmax = upper, y = y),
                       height = 0.18, color = col_ci, linewidth = 1.8, alpha = 0.7) +
        geom_text(data = rows_df,
                  aes(x = (lower + upper) / 2, y = y - 0.22,
                      label = paste0("[", round(lower, 3), " ; ", round(upper, 3), "]")),
                  color = col_ci, size = 3.8, fontface = "bold")
    }

    p
  }

  # ---- Plot dla diff_props (porownanie dwoch grup, slupki + 2 panele CI) ----
  plot_diff_props_step <- function(data, step, xlab,
                                    hypothesis = NULL, title = NULL) {
    x1 <- data$x1; n1 <- data$n1
    x2 <- data$x2; n2 <- data$n2
    label1 <- data$label1; label2 <- data$label2

    cd <- ci_diff_props(x1, n1, x2, n2)
    p1 <- cd$p1; p2 <- cd$p2
    diff_val <- cd$diff
    se <- cd$se; me <- cd$me

    # ---- LEWY PANEL: slupki TAK/NIE x 2 grupy ----
    bar_df <- data.frame(
      grp = factor(rep(c(label1, label2), each = 2),
                   levels = c(label1, label2)),
      val = factor(rep(c("NIE", "TAK"), 2), levels = c("NIE", "TAK")),
      count = c(n1 - x1, x1, n2 - x2, x2)
    )
    p_left <- ggplot(bar_df, aes(x = grp, y = count, fill = val)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.65) +
      geom_text(aes(label = count),
                position = position_dodge(width = 0.75),
                vjust = -0.4, fontface = "bold", size = 4.2, color = upwr_secondary) +
      scale_fill_manual(values = c("NIE" = col_miss, "TAK" = col_ci),
                        name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(x = NULL, y = "Liczebność") +
      theme_upwr() +
      theme(legend.position = "top",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())

    # ---- PRAWY GORNY PANEL: dwie p_hat na osi proporcji ----
    p_top <- ggplot() +
      xlim(0, 1) +
      ylim(0.4, 2.6) +
      labs(x = xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_text(face = "bold", size = 11),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      scale_y_continuous(breaks = c(1, 2), labels = c(label1, label2),
                         limits = c(0.4, 2.6))

    if (step >= 2) {
      p_top <- p_top +
        geom_point(aes(x = p1, y = 1), color = col_estimate, size = 7, shape = 18) +
        annotate("text", x = p1, y = 1.45, label = paste0("p̂₁ = ", round(p1, 3)),
                 color = col_estimate, fontface = "bold", size = 4.2) +
        geom_point(aes(x = p2, y = 2), color = col_estimate, size = 7, shape = 18) +
        annotate("text", x = p2, y = 2.45, label = paste0("p̂₂ = ", round(p2, 3)),
                 color = col_estimate, fontface = "bold", size = 4.2)
    }

    # ---- PRAWY DOLNY PANEL: roznica + CI + obszar hipotezy ----
    xlims_bot <- range(c(-0.3, 0.3, diff_val - 1.3 * me, diff_val + 1.3 * me))
    if (!is.null(hypothesis)) {
      xlims_bot <- range(c(xlims_bot, hypothesis$bound))
    }
    pad_bot <- diff(xlims_bot) * 0.08
    xlims_bot <- c(xlims_bot[1] - pad_bot, xlims_bot[2] + pad_bot)

    col_true_local <- "#9b59b6"

    p_bot <- ggplot() +
      xlim(xlims_bot) +
      ylim(-0.55, 0.55) +
      labs(x = paste0("Różnica proporcji  —  ", label1, " − ", label2),
           y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      geom_vline(xintercept = 0, color = col_true_local,
                 linewidth = 1, linetype = "dashed") +
      annotate("text", x = 0, y = 0.45, label = "0 = brak różnicy",
               color = col_true_local, fontface = "bold", size = 4, hjust = -0.1)

    # Obszar hipotezy
    if (!is.null(hypothesis)) {
      if (hypothesis$dir == "gt") {
        p_bot <- p_bot + annotate("rect",
                                  xmin = hypothesis$bound, xmax = Inf,
                                  ymin = -Inf, ymax = Inf,
                                  fill = col_hyp, alpha = 0.15)
      } else {
        p_bot <- p_bot + annotate("rect",
                                  xmin = -Inf, xmax = hypothesis$bound,
                                  ymin = -Inf, ymax = Inf,
                                  fill = col_hyp, alpha = 0.15)
      }
      p_bot <- p_bot +
        geom_vline(xintercept = hypothesis$bound, color = col_hyp,
                   linewidth = 1, linetype = "solid") +
        annotate("text", x = hypothesis$bound, y = 0.45,
                 label = paste0(if (hypothesis$dir == "gt") "≥ " else "≤ ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4, hjust = -0.1)
    }

    if (step >= 3) {
      p_bot <- p_bot +
        geom_point(aes(x = diff_val, y = 0), color = col_estimate,
                   size = 7, shape = 18) +
        annotate("text", x = diff_val, y = -0.22,
                 label = paste0("p̂₁ − p̂₂ = ", round(diff_val, 3)),
                 color = col_estimate, fontface = "bold", size = 4.5)
    }

    if (step >= 4) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - se, xmax = diff_val + se, y = 0),
                       height = 0.08, color = col_hit, linewidth = 1.8) +
        annotate("text", x = diff_val, y = 0.17,
                 label = paste0("± SE = ±", round(se, 3)),
                 color = col_hit, fontface = "bold", size = 4)
    }

    if (step >= 5) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - me, xmax = diff_val + me, y = 0),
                       height = 0.14, color = col_ci, linewidth = 2.2, alpha = 0.6) +
        annotate("text", x = diff_val, y = -0.42,
                 label = paste0("95% CI: [", round(diff_val - me, 3),
                                " ; ", round(diff_val + me, 3), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    }

    library(patchwork)
    right_col <- p_top / p_bot + plot_layout(heights = c(1, 1))
    (p_left | right_col) +
      plot_layout(widths = c(1, 2)) +
      plot_annotation(title = title)
  }

  # ---- Plot dla forest_prop (wiele grup, proporcje) ----
  plot_forest_prop_step <- function(data, step, xlab, hypothesis = NULL) {
    groups <- data$groups
    xs <- data$x
    ns <- data$n
    k <- length(groups)

    ci_list <- lapply(seq_len(k), function(i) ci_prop(xs[i], ns[i]))
    all_phats <- sapply(ci_list, function(c) c$phat)
    all_lowers <- sapply(ci_list, function(c) c$lower)
    all_uppers <- sapply(ci_list, function(c) c$upper)

    xlims <- range(c(all_lowers, all_uppers))
    xlims[1] <- max(0, xlims[1] - 0.03)
    xlims[2] <- min(1, xlims[2] + 0.03)

    y_positions <- seq_len(k)
    group_df <- data.frame(group = groups, y = y_positions,
                            phat = all_phats, lower = all_lowers,
                            upper = all_uppers,
                            label = paste0(xs, "/", ns))

    p <- ggplot() +
      xlim(xlims) +
      ylim(0.3, k + 0.7) +
      labs(x = xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Etykiety grup
    p <- p +
      annotate("text", x = xlims[1], y = y_positions,
               label = groups, hjust = 0, fontface = "bold", size = 4.5,
               color = upwr_secondary)

    # Krok 1+: surowe liczby x/n
    if (step >= 1) {
      p <- p +
        annotate("text", x = xlims[2], y = y_positions,
                 label = group_df$label, hjust = 1, size = 4,
                 color = upwr_secondary, fontface = "italic")
    }

    # Krok 2+: punkty p_hat
    if (step >= 2) {
      p <- p + geom_point(data = group_df, aes(x = phat, y = y),
                          color = col_estimate, size = 6, shape = 18)
    }
    # Krok 3+: CI
    if (step >= 3) {
      p <- p + geom_errorbarh(data = group_df,
                               aes(xmin = lower, xmax = upper, y = y),
                               height = 0.18, color = col_ci, linewidth = 1.8)
    }

    p
  }

  # ---- Liczba "core" krokow budowy CI (bez hipotez) ----
  n_core_steps <- function(cfg) length(cfg$steps)

  # ---- Dekoder fazy hipotezy ze stanu (jak w ch3) ----
  hyp_phase <- function(step, n_core, n_hyp) {
    if (step <= n_core) return(NULL)
    offset <- step - n_core
    j <- (offset - 1) %/% 2 + 1
    reveal <- (offset - 1) %% 2 == 1
    if (j > n_hyp) return(NULL)
    list(idx = j, reveal = reveal)
  }
  hyp_state <- function(n_core, j, reveal) {
    n_core + (j - 1) * 2 + (if (reveal) 2 else 1)
  }

  # ---- Generator przyciskow dla case'a ----
  case_buttons_ui <- function(case_id) {
    cfg <- cases_config[[case_id]]
    current <- ch4_case_state[[case_id]]
    n_core <- n_core_steps(cfg)
    n_hyp <- length(cfg$hypotheses)
    phase <- hyp_phase(current, n_core, n_hyp)

    core_btns <- lapply(seq_along(cfg$steps), function(i) {
      btn_class <- if (current == i) "lc-btn-primary" else "lc-btn-outline"
      actionButton(paste0("ch4_case", case_id, "_step", i),
                   cfg$steps[i], class = btn_class)
    })

    hyp_btns <- if (current >= n_core) {
      lapply(seq_along(cfg$hypotheses), function(j) {
        is_active <- !is.null(phase) && phase$idx == j
        btn_class <- if (is_active) "lc-btn-warning" else "lc-btn-warning-outline"
        actionButton(paste0("ch4_case", case_id, "_hyp", j),
                     paste0("Hipoteza ", j), class = btn_class)
      })
    } else {
      list(helpText("Wybuduj pełny przedział, żeby sprawdzić hipotezy."))
    }

    reveal_row <- if (!is.null(phase) && !phase$reveal) {
      div(class = "step-buttons lc-mt-xs",
        actionButton(paste0("ch4_case", case_id, "_reveal"),
                     "\U0001f50d Pokaż werdykt", class = "lc-btn-ok"))
    } else {
      NULL
    }

    tagList(
      div(class = "step-buttons", core_btns),
      div(class = "step-buttons lc-mt-xs", hyp_btns),
      reveal_row
    )
  }

  # ---- Glowny render: plot dla case'a ----
  render_case_plot <- function(case_id) {
    cfg <- cases_config[[case_id]]
    step <- ch4_case_state[[case_id]]
    n_core <- n_core_steps(cfg)

    if (step == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Kliknij pierwszy krok, żeby zacząć",
                   size = 5, color = upwr_reference) +
          theme_void()
      )
    }

    n_hyp <- length(cfg$hypotheses)
    phase <- hyp_phase(step, n_core, n_hyp)
    hypothesis <- NULL
    plot_step <- step
    if (!is.null(phase)) {
      hyp_obj <- cfg$hypotheses[[phase$idx]]
      if (is.null(hyp_obj$kind) || hyp_obj$kind != "pairwise") {
        hypothesis <- hyp_obj
      }
      plot_step <- n_core
    }

    switch(cfg$type,
      "single_prop"     = plot_single_prop_step(cfg$data, plot_step, cfg$xlab,
                                                 hypothesis = hypothesis),
      "compare_n_prop"  = plot_compare_n_prop_step(cfg$data, plot_step, cfg$xlab,
                                                    hypothesis = hypothesis),
      "diff_props"      = plot_diff_props_step(cfg$data, plot_step, cfg$xlab,
                                                hypothesis = hypothesis),
      "forest_prop"     = plot_forest_prop_step(cfg$data, plot_step, cfg$xlab,
                                                 hypothesis = hypothesis)
    )
  }

  # ---- Pairwise: macierz nakladania CI dla forest_prop ----
  forest_prop_pairwise_matrix <- function(data) {
    k <- length(data$groups)
    cis <- lapply(seq_len(k), function(i) ci_prop(data$x[i], data$n[i]))
    m <- matrix(FALSE, nrow = k, ncol = k,
                dimnames = list(data$groups, data$groups))
    for (i in seq_len(k)) for (j in seq_len(k)) {
      if (i == j) next
      m[i, j] <- (cis[[i]]$upper < cis[[j]]$lower) ||
                 (cis[[j]]$upper < cis[[i]]$lower)
    }
    m
  }

  render_pairwise_table <- function(mat) {
    groups <- rownames(mat)
    k <- length(groups)
    header <- tags$tr(
      tags$th(""),
      lapply(groups, function(g) tags$th(g, style = "padding: 4px 8px; text-align: center; font-size: 12px;"))
    )
    rows <- lapply(seq_len(k), function(i) {
      tags$tr(
        tags$th(groups[i], style = "padding: 4px 8px; text-align: right; font-size: 12px;"),
        lapply(seq_len(k), function(j) {
          if (i == j) {
            tags$td("—", style = "padding: 4px 8px; text-align: center; color: var(--upwr-reference);")
          } else if (mat[i, j]) {
            tags$td("✓", style = "padding: 4px 8px; text-align: center; color: var(--upwr-sage); font-weight: bold; font-size: 16px;")
          } else {
            tags$td("×", style = "padding: 4px 8px; text-align: center; color: var(--upwr-accent); font-size: 16px;")
          }
        })
      )
    })
    tags$table(
      style = "border-collapse: collapse; margin: 8px auto; border: 1px solid var(--upwr-rule);",
      tags$thead(header),
      tags$tbody(rows)
    )
  }

  # Narracja "jak w raporcie" dla pairwise (proporcje, prezentacja w %)
  pairwise_narrative <- function(data, mat) {
    groups <- data$groups
    phats <- data$x / data$n
    k <- length(groups)

    diff_pairs <- list()
    for (i in seq_len(k - 1)) for (j in seq(i + 1, k)) {
      if (mat[i, j]) {
        if (phats[i] > phats[j]) {
          diff_pairs[[length(diff_pairs) + 1]] <- list(hi = groups[i], lo = groups[j])
        } else {
          diff_pairs[[length(diff_pairs) + 1]] <- list(hi = groups[j], lo = groups[i])
        }
      }
    }
    n_diff <- length(diff_pairs)

    if (n_diff == 0) {
      return(paste0(
        "Żadna para grup nie wykazała istotnej różnicy w odsetkach — ",
        "wszystkie 95% CI nakładają się wzajemnie. Na podstawie tych danych ",
        "nie możemy stwierdzić różnic między grupami."
      ))
    }

    # Czy jedna grupa odstaje od WSZYSTKICH innych?
    standout_idx <- which(sapply(seq_len(k), function(i) all(mat[i, -i])))
    if (length(standout_idx) == 1) {
      i <- standout_idx
      others <- phats[-i]
      direction <- if (phats[i] > max(others)) "wyższy" else "niższy"
      return(paste0(
        "Spośród wszystkich badanych grup wyraźnie odstaje ",
        tags$b(groups[i]), " (odsetek ", round(phats[i] * 100, 1), "%) — ma istotnie ",
        direction, " odsetek niż każda z pozostałych grup ",
        "(jego 95% CI nie nakłada się z żadnym innym). ",
        "Pozostałe grupy mają odsetki w przedziale ",
        round(min(others) * 100, 1), "%–", round(max(others) * 100, 1), "%, ",
        "a ich CI nakładają się — nie możemy stwierdzić między nimi istotnych różnic."
      ))
    }

    pair_strs <- sapply(diff_pairs, function(pp) {
      paste0(tags$b(pp$hi), " > ", tags$b(pp$lo))
    })
    pairs_inline <- if (length(pair_strs) == 1) {
      pair_strs[1]
    } else if (length(pair_strs) == 2) {
      paste(pair_strs, collapse = " oraz ")
    } else {
      paste0(paste(pair_strs[-length(pair_strs)], collapse = ", "),
             " oraz ", pair_strs[length(pair_strs)])
    }

    intro <- if (n_diff == 1) {
      "Spośród wszystkich porównań jedynie jedna para wykazała istotną różnicę: "
    } else {
      paste0("Istotne różnice (CI nie nakładają się) wykazały ",
             n_diff, " pary: ")
    }

    paste0(
      intro, pairs_inline, ". ",
      "Pozostałe pary nie różnią się istotnie — ich 95% CI nakładają się, ",
      "więc na podstawie tych danych nie możemy między nimi rozróżnić."
    )
  }

  # ---- Render: explanation ----
  render_case_explain <- function(case_id) {
    cfg <- cases_config[[case_id]]
    step <- ch4_case_state[[case_id]]
    n_core <- n_core_steps(cfg)

    if (step == 0) return(NULL)

    n_hyp <- length(cfg$hypotheses)
    phase <- hyp_phase(step, n_core, n_hyp)
    if (!is.null(phase)) {
      hyp <- cfg$hypotheses[[phase$idx]]

      # Sub-faza 1: tylko tresc hipotezy
      if (!phase$reveal) {
        return(lc_feedback(type = "info",
          p(tags$strong("Hipoteza ", phase$idx, ": "), hyp$text),
          p(tags$em("Spojrz na wykres: gdzie lezy CI wzgledem obszaru hipotezy?
                    Co o tym sadzicie? Kliknięcie ", tags$b("Pokaż werdykt"),
                    " odsloni odpowiedź."))
        ))
      }

      # Sub-faza 2: werdykt + wyjasnienie
      if (!is.null(hyp$kind) && hyp$kind == "pairwise") {
        mat <- forest_prop_pairwise_matrix(cfg$data)
        narrative <- pairwise_narrative(cfg$data, mat)
        return(lc_feedback(type = "ok",
          p(tags$strong("Hipoteza: "), hyp$text),
          p(tags$strong("Werdykt — macierz par:")),
          p(tags$em("✓ = grupy różnią się istotnie (CI nie nakładają się);  ",
                    "× = nie można stwierdzić różnicy (CI nakładają się)"),
            style = "font-size: 12px; color: var(--upwr-reference);"),
          render_pairwise_table(mat),
          p(tags$strong("Jak to opisać w raporcie:"),
            style = "margin-top: 12px;"),
          p(HTML(narrative), style = "font-style: italic;")
        ))
      }

      verdict <- compute_verdict_for_case(cfg, hyp)
      cls <- verdict_class(verdict)
      label <- verdict_label(verdict)

      body <- if (verdict == "yes" && !is.null(hyp$explain_yes)) {
        p(hyp$explain_yes)
      } else if (verdict == "no" && !is.null(hyp$explain_no)) {
        p(hyp$explain_no)
      } else {
        p("CI przecina granicę hipotezy — nie możemy jednoznacznie
          stwierdzić, czy jest prawdziwa.")
      }

      return(lc_feedback(type = cls,
        p(tags$strong("Hipoteza ", phase$idx, ": "), hyp$text),
        p(tags$strong("Werdykt: ", label)),
        body
      ))
    }

    # Faza budowy CI — wyjasnienie ostatniego kroku
    lc_feedback(type = "info",
      p(tags$strong(cfg$steps[step])),
      p("Krok ", step, " z ", n_core, ".")
    )
  }

  # ---- Werdykt dla case'a ----
  compute_verdict_for_case <- function(cfg, hyp) {
    switch(cfg$type,
      "single_prop" = {
        ci <- ci_prop(cfg$data$x, cfg$data$n)
        hypothesis_verdict(ci$lower, ci$upper, hyp$bound, hyp$dir)
      },
      "compare_n_prop" = {
        # Werdykt na podstawie najwiekszego n (najbardziej precyzyjne CI)
        largest_n <- max(cfg$data$ns)
        x <- round(cfg$data$phat * largest_n)
        ci <- ci_prop(x, largest_n)
        hypothesis_verdict(ci$lower, ci$upper, hyp$bound, hyp$dir)
      },
      "diff_props" = {
        cd <- ci_diff_props(cfg$data$x1, cfg$data$n1, cfg$data$x2, cfg$data$n2)
        hypothesis_verdict(cd$lower, cd$upper, hyp$bound, hyp$dir)
      }
    )
  }

  # ---- Podlaczenie observerow + outputow dla kazdego case'a ----
  register_case <- function(case_id) {
    cfg <- cases_config[[case_id]]
    n_core <- length(cfg$steps)

    lapply(seq_along(cfg$steps), function(i) {
      force(i)
      observeEvent(input[[paste0("ch4_case", case_id, "_step", i)]], {
        ch4_case_state[[case_id]] <- i
      }, ignoreInit = TRUE)
    })

    lapply(seq_along(cfg$hypotheses), function(j) {
      force(j)
      observeEvent(input[[paste0("ch4_case", case_id, "_hyp", j)]], {
        ch4_case_state[[case_id]] <- hyp_state(n_core, j, reveal = FALSE)
      }, ignoreInit = TRUE)
    })

    observeEvent(input[[paste0("ch4_case", case_id, "_reveal")]], {
      current <- ch4_case_state[[case_id]]
      n_hyp <- length(cfg$hypotheses)
      phase <- hyp_phase(current, n_core, n_hyp)
      if (!is.null(phase) && !phase$reveal) {
        ch4_case_state[[case_id]] <- hyp_state(n_core, phase$idx, reveal = TRUE)
      }
    }, ignoreInit = TRUE)

    output[[paste0("ch4_case", case_id, "_buttons")]] <- renderUI({
      ch4_case_state[[case_id]]
      case_buttons_ui(case_id)
    })
    output[[paste0("ch4_case", case_id, "_plot")]] <- renderPlot({
      ch4_case_state[[case_id]]
      render_case_plot(case_id)
    })
    output[[paste0("ch4_case", case_id, "_explain")]] <- renderUI({
      ch4_case_state[[case_id]]
      render_case_explain(case_id)
    })
  }

  for (cid in names(cases_config)) {
    register_case(cid)
  }

}
