# ============================================================================
# CHAPTER 4: Przedzial dla proporcji
# ============================================================================

ch4_ui <- tabPanel("4. Przedzia\u0142 dla proporcji",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Umiemy ju\u017c budowa\u0107 przedzia\u0142 dla \u015bredniej.
       A co, gdy interesuje nas odsetek (proporcja)?"
    ),

    div(class = "section-title", "Wz\u00f3r"),

    div(class = "narrative",
      p("Cz\u0119sto chcemy oszacowa\u0107 odsetek \u2014 jaki procent student\u00f3w zda\u0142 egzamin,
        jaki odsetek wyborc\u00f3w g\u0142osuje na parti\u0119 X, jaki procent produkt\u00f3w jest wadliwy."),
      p("Estymator punktowy to proporcja z pr\u00f3by:"),
      div(class = "formula-box",
        withMathJax("$$\\hat{p} = \\frac{x}{n}$$")
      ),
      p("Najprostszy przedzia\u0142 ufno\u015bci dla proporcji to ", tags$b("przedzia\u0142 Walda"), ":"),
      div(class = "formula-box",
        withMathJax("$$CI = \\hat{p} \\pm z^*_{\\alpha/2} \\cdot \\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}$$")
      ),
      p("Sk\u0142adniki:"),
      tags$ul(
        tags$li(withMathJax("\\(\\hat{p}\\)"),
                " \u2014 proporcja z pr\u00f3by (\u015brodek przedzia\u0142u)"),
        tags$li(withMathJax("\\(\\sqrt{\\hat{p}(1-\\hat{p})/n}\\)"),
                " \u2014 b\u0142\u0105d standardowy proporcji"),
        tags$li(withMathJax("\\(z^*\\)"),
                " \u2014 warto\u015b\u0107 krytyczna z rozk\u0142adu normalnego (dla 95% \u2248 1.96)")
      ),
      p(tags$b("Dlaczego z, a nie t?"),
        " Bo proporcja \u2014 inaczej ni\u017c \u015brednia \u2014 nie wymaga osobnego oszacowania
        \"odchylenia standardowego\". Wariancja proporcji to ", withMathJax("\\(p(1-p)\\)"),
        ", wi\u0119c jest jednoznacznie wyznaczona przez sam\u0105 ", withMathJax("\\(p\\)"), "."),
      p("Uwaga \u2014 Wald nie zawsze dzia\u0142a dobrze. Gdy ",
        withMathJax("\\(n\\)"), " jest ma\u0142e lub ", withMathJax("\\(\\hat{p}\\)"),
        " bardzo bliskie 0 lub 1, przedzia\u0142 Walda mo\u017ce mie\u0107 zaskakuj\u0105co niskie
        pokrycie. W takich sytuacjach lepiej u\u017cy\u0107 ", tags$b("przedzia\u0142u Wilsona"),
        ", kt\u00f3ry koryguje wz\u00f3r. W tym wyk\u0142adzie skupiamy si\u0119 na Waldzie \u2014
        bo \u0142atwo go zrozumie\u0107, a w przyk\u0142adach trzymamy si\u0119 \"bezpiecznych\"
        warto\u015bci ", withMathJax("\\(np \\geq 10\\)"), " i ",
        withMathJax("\\(n(1-p) \\geq 10\\)"), ".")
    ),

    # ========================================================================
    # WIDGET 1: Budowa przedzialu dla proporcji krok po kroku
    # ========================================================================
    div(class = "section-title", "Budowa przedzia\u0142u \u2014 krok po kroku"),

    div(class = "narrative",
      p("Zobaczmy, jak z konkretnej pr\u00f3by (50 odpowiedzi TAK/NIE) powstaje przedzia\u0142
        ufno\u015bci dla proporcji. Pytamy 50 student\u00f3w, czy zdali egzamin, i estymujemy
        odsetek zdaj\u0105cych w ca\u0142ej populacji.")
    ),

    div(class = "widget-block",
      h4("Konstruowanie przedzia\u0142u"),
      div(class = "step-buttons",
        actionButton("ch4_step1", "1. Pr\u00f3ba",     class = "btn-outline-primary"),
        actionButton("ch4_step2", "2. p\u0302",        class = "btn-outline-primary"),
        actionButton("ch4_step3", "3. \u00b1 SE",      class = "btn-outline-primary"),
        actionButton("ch4_step4", "4. Przedzia\u0142", class = "btn-outline-primary")
      ),
      div(style = "display: flex; gap: 8px; margin-top: 8px;",
        actionButton("ch4_step_new_sample", "\u21bb Nowa pr\u00f3ba",
                     class = "btn-outline-secondary btn-sm")
      ),
      plotOutput("ch4_step_plot", height = "340px"),
      uiOutput("ch4_step_explanation")
    ),

    # ========================================================================
    # WIDGET 2: Budowa przedzialu dla roznicy proporcji
    # ========================================================================
    div(class = "section-title", "Budowa przedzia\u0142u dla r\u00f3\u017cnicy proporcji"),

    div(class = "narrative",
      p("CI dla r\u00f3\u017cnicy dw\u00f3ch proporcji buduje si\u0119 analogicznie do r\u00f3\u017cnicy
        \u015brednich \u2014 trzeba po\u0142\u0105czy\u0107 niepewno\u015b\u0107 z obu pr\u00f3b:"),
      div(class = "formula-box",
        withMathJax("$$CI = (\\hat{p}_1 - \\hat{p}_2) \\pm z^* \\cdot \\sqrt{\\frac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\frac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}}$$")
      ),
      p("Por\u00f3wnamy odsetek os\u00f3b zadowolonych z us\u0142ugi w dw\u00f3ch grupach \u2014
        po 60 os\u00f3b w ka\u017cdej.")
    ),

    div(class = "widget-block",
      h4("Konstruowanie CI dla r\u00f3\u017cnicy"),
      div(class = "step-buttons",
        actionButton("ch4_dstep1", "1. Dwie pr\u00f3by",   class = "btn-outline-primary"),
        actionButton("ch4_dstep2", "2. Dwie p\u0302",     class = "btn-outline-primary"),
        actionButton("ch4_dstep3", "3. R\u00f3\u017cnica", class = "btn-outline-primary"),
        actionButton("ch4_dstep4", "4. \u00b1 SE",         class = "btn-outline-primary"),
        actionButton("ch4_dstep5", "5. Przedzia\u0142",   class = "btn-outline-primary")
      ),
      div(style = "display: flex; gap: 8px; margin-top: 8px;",
        actionButton("ch4_dstep_new_sample", "\u21bb Nowe pr\u00f3by",
                     class = "btn-outline-secondary btn-sm")
      ),
      plotOutput("ch4_dstep_plot", height = "420px"),
      uiOutput("ch4_dstep_explanation")
    ),

    # ========================================================================
    # WIDGET 3: CASE STUDIES (konstruktory + hipotezy)
    # ========================================================================
    div(class = "section-title", "Case studies \u2014 jak interpretowa\u0107 CI w praktyce"),

    div(class = "narrative",
      p("Poni\u017cej kilka realistycznych sytuacji. W ka\u017cdej budujesz CI krok po kroku
        (jak w poprzednich sekcjach), a na ko\u0144cu weryfikujesz dwie hipotezy:
        jedn\u0105, kt\u00f3ra jest prawdziwa, i jedn\u0105, kt\u00f3ra nie jest. Klikaj nag\u0142\u00f3wki,
        \u017ceby rozwija\u0107 case'y.")
    ),

    # ----- GRUPA A: JEDNA PROPORCJA -----
    div(class = "section-title", style = "font-size: 18px; margin-top: 25px;",
        "A. Przedzia\u0142 dla jednej proporcji"),

    tags$details(class = "case-study", open = NA,
      tags$summary(
        span(class = "case-icon", "\U0001f5f3\ufe0f"),
        "A1. Sonda\u017c wyborczy \u2014 czytanie pojedynczego CI"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Pracownia sonda\u017cowa zapyta\u0142a 400 wyborc\u00f3w, czy poprze parti\u0119 X.
            212 odpowiedzi TAK (czyli ", withMathJax("\\(\\hat{p} = 0.53\\)"),
            "). Zbudujmy CI dla poparcia w populacji i sprawd\u017amy dwie hipotezy.")
        ),
        uiOutput("ch4_caseA1_buttons"),
        plotOutput("ch4_caseA1_plot", height = "260px"),
        uiOutput("ch4_caseA1_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f50d"),
        "A2. Ten sam odsetek, trzy r\u00f3\u017cne wielko\u015bci pr\u00f3by"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Por\u00f3wnaj trzy badania mierz\u0105ce odsetek wadliwych produkt\u00f3w
            w fabryce. W ka\u017cdym ", withMathJax("\\(\\hat{p} = 0.08\\)"), " (8%),
            ale ", tags$b("n r\u00f3\u017cne"), " (50, 200, 1000). Dodawaj CI jeden
            po drugim i patrz, jak si\u0119 zw\u0119\u017caj\u0105.")
        ),
        uiOutput("ch4_caseA2_buttons"),
        plotOutput("ch4_caseA2_plot", height = "260px"),
        uiOutput("ch4_caseA2_explain")
      )
    ),

    # ----- GRUPA B: ROZNICA PROPORCJI -----
    div(class = "section-title", style = "font-size: 18px; margin-top: 25px;",
        "B. Przedzia\u0142 dla r\u00f3\u017cnicy proporcji"),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f48a"),
        "B1. Lek vs placebo \u2014 odsetek wyleczonych"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Badamy nowy lek przeciwb\u00f3lowy.
            ", tags$b("Lek:"), " 200 pacjent\u00f3w, 124 zg\u0142osi\u0142o ust\u0105pienie b\u00f3lu (62%).
            ", tags$b("Placebo:"), " 200 pacjent\u00f3w, 84 zg\u0142osi\u0142o ust\u0105pienie b\u00f3lu (42%).")
        ),
        uiOutput("ch4_caseB1_buttons"),
        plotOutput("ch4_caseB1_plot", height = "380px"),
        uiOutput("ch4_caseB1_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f3ed"),
        "B2. Dwie linie produkcyjne \u2014 odsetek brak\u00f3w"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Por\u00f3wnujesz dwie linie produkcyjne pod k\u0105tem odsetka wadliwych produkt\u00f3w.
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
        span(class = "case-icon", "\u26a0\ufe0f"),
        "B3. Pu\u0142apka ma\u0142ej pr\u00f3by"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Pilota\u017c nowej procedury BHP w dw\u00f3ch zak\u0142adach.
            ", tags$b("Zak\u0142ad A:"), " 30 pracownik\u00f3w, 6 mia\u0142o wypadek (20%).
            ", tags$b("Zak\u0142ad B:"), " 30 pracownik\u00f3w, 9 mia\u0142o wypadek (30%).
            R\u00f3\u017cnica wygl\u0105da na du\u017c\u0105 \u2014 ale czy mo\u017cemy z 95% ufno\u015bci\u0105
            powiedzie\u0107, \u017ce procedura A jest skuteczniejsza?")
        ),
        uiOutput("ch4_caseB3_buttons"),
        plotOutput("ch4_caseB3_plot", height = "380px"),
        uiOutput("ch4_caseB3_explain")
      )
    ),

    # ----- GRUPA C: WIELE GRUP -----
    div(class = "section-title", style = "font-size: 18px; margin-top: 25px;",
        "C. Wiele grup \u2014 forest plot"),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f3e5"),
        "C1. Cztery szpitale \u2014 odsetek powik\u0142a\u0144 pooperacyjnych"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Por\u00f3wnujesz odsetek powik\u0142a\u0144 po tej samej operacji w czterech szpitalach.
            Dla ka\u017cdego masz liczb\u0119 wykonanych zabieg\u00f3w i liczb\u0119 powik\u0142a\u0144.
            Dodawaj CI jeden po drugim i obserwuj.")
        ),
        uiOutput("ch4_caseC1_buttons"),
        plotOutput("ch4_caseC1_plot", height = "320px"),
        uiOutput("ch4_caseC1_explain")
      )
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: co decyduje o szeroko\u015bci przedzia\u0142u?"),
      actionButton("ch4_next", "Dalej \u2192 5. Co wp\u0142ywa na szeroko\u015b\u0107?",
                   class = "btn-primary btn-lg")
    )
  ))
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
                   label = "Kliknij '1. Pr\u00f3ba' \u017ceby zacz\u0105\u0107",
                   size = 6, color = "#7f8c8d") +
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
                size = 5, color = col_dark) +
      scale_fill_manual(values = c("NIE" = col_secondary, "TAK" = col_primary),
                        guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "Liczebno\u015b\u0107",
           title = paste0("Dane (n = ", n, ")")) +
      theme_educational() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())

    # ---- PRAWY PANEL: os proporcji z p_hat, SE, CI ----
    p_right <- ggplot() +
      xlim(0, 1) +
      ylim(-0.6, 0.6) +
      labs(x = "Proporcja", y = NULL,
           title = paste0("p\u0302 = ", round(phat, 3))) +
      theme_educational() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Krok 2+: punkt p_hat
    if (step >= 2) {
      p_right <- p_right +
        geom_point(aes(x = phat, y = 0), color = col_estimate,
                   size = 7, shape = 18) +
        annotate("text", x = phat, y = -0.22,
                 label = paste0("p\u0302 = ", round(phat, 3)),
                 color = col_estimate, fontface = "bold", size = 5)
    }

    # Krok 3+: waski przedzial SE
    if (step >= 3) {
      p_right <- p_right +
        geom_errorbarh(aes(xmin = phat - se, xmax = phat + se, y = 0),
                       height = 0.08, color = col_success, linewidth = 1.8) +
        annotate("text", x = phat, y = 0.20,
                 label = paste0("\u00b1 SE = \u00b1", round(se, 3)),
                 color = col_success, fontface = "bold", size = 4.2)
    }

    # Krok 4: pelen CI
    if (step >= 4) {
      p_right <- p_right +
        geom_errorbarh(aes(xmin = phat - me, xmax = phat + me, y = 0),
                       height = 0.14, color = col_ci, linewidth = 2.2, alpha = 0.6) +
        annotate("text", x = phat, y = -0.45,
                 label = paste0("95% CI: [", round(phat - me, 3),
                                " ; ", round(phat + me, 3), "]"),
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
      "1" = div(class = "callout-info",
        p(tags$strong("Krok 1:"), " Pr\u00f3ba.",
          " Mamy ", tags$b(n), " obserwacji TAK/NIE: ", tags$b(x), " razy TAK, ",
          tags$b(n - x), " razy NIE. Niebieskie punkty (TAK) po prawej, czerwone (NIE)
          po lewej. Sama tabelka liczb \u2014 jeszcze nie zacz\u0119li\u015bmy estymowa\u0107.")
      ),
      "2" = div(class = "callout-info",
        p(tags$strong("Krok 2:"), " Estymacja punktowa p\u0302.",
          " Liczymy proporcj\u0119 z pr\u00f3by:"),
        p(withMathJax(paste0("\\(\\hat{p} = \\frac{x}{n} = \\frac{", x, "}{", n,
                             "} = ", round(phat, 3), "\\)"))),
        p("To nasza najlepsza pojedyncza warto\u015b\u0107 \u2014 ale potrzebujemy
          jeszcze wiedzie\u0107, jak bardzo niepewna jest ta estymata.")
      ),
      "3" = div(class = "callout-info",
        p(tags$strong("Krok 3:"), " B\u0142\u0105d standardowy (\u00b1 SE).",
          " Niepewno\u015b\u0107 oszacowania proporcji liczymy ze wzoru:"),
        p(withMathJax(paste0(
          "\\(SE = \\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}} = \\sqrt{\\frac{",
          round(phat, 2), " \\cdot ", round(1 - phat, 2), "}{", n, "}} = ",
          round(se, 3), "\\)"))),
        p("Zielony pasek \u00b1 SE to zakres \"jednego odchylenia\" wok\u00f3\u0142
          p\u0302. Ale 95% CI to oko\u0142o ", tags$b("dwa SE w ka\u017cd\u0105 stron\u0119"),
          " (dok\u0142adniej: 1.96).")
      ),
      "4" = {
        div(class = "callout-success",
          p(tags$strong("Krok 4:"), " Przedzia\u0142 ufno\u015bci."),
          p("Warto\u015b\u0107 krytyczna z rozk\u0142adu normalnego: ",
            withMathJax("\\(z^* = 1.96\\)")),
          p(withMathJax(paste0("\\(ME = z^* \\cdot SE = 1.96 \\cdot ",
                               round(se, 3), " = ", round(me, 3), "\\)"))),
          p(tags$b("95% CI: ["),
            round(phat - me, 3), " ; ", round(phat + me, 3),
            tags$b("]")),
          p(tags$em("Z 95% ufno\u015bci\u0105 prawdziwy odsetek w populacji le\u017cy w tym
                   przedziale. Sprawd\u017a, jak zmienia si\u0119 CI po wylosowaniu nowej pr\u00f3by!"))
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
                   label = "Kliknij '1. Dwie pr\u00f3by' aby zacz\u0105\u0107",
                   size = 6, color = "#7f8c8d") +
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
                vjust = -0.4, fontface = "bold", size = 4.5, color = col_dark) +
      scale_fill_manual(values = c("NIE" = col_secondary, "TAK" = col_primary),
                        name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(x = NULL, y = "Liczebno\u015b\u0107", title = "Dane") +
      theme_educational() +
      theme(legend.position = "top",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())

    # ---- PRAWY GORNY PANEL: dwie p_hat na osi proporcji ----
    p_top <- ggplot() +
      xlim(0, 1) +
      ylim(0.4, 2.6) +
      labs(x = "Proporcja", y = NULL, title = "Proporcje w grupach") +
      theme_educational() +
      theme(axis.text.y = element_text(face = "bold", size = 12),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      scale_y_continuous(breaks = c(1, 2), labels = c("Grupa 1", "Grupa 2"),
                         limits = c(0.4, 2.6))

    if (step >= 2) {
      p_top <- p_top +
        geom_point(aes(x = p1, y = 1), color = col_estimate, size = 7, shape = 18) +
        annotate("text", x = p1, y = 1.45, label = paste0("p\u0302\u2081 = ", round(p1, 3)),
                 color = col_estimate, fontface = "bold", size = 4.5) +
        geom_point(aes(x = p2, y = 2), color = col_estimate, size = 7, shape = 18) +
        annotate("text", x = p2, y = 2.45, label = paste0("p\u0302\u2082 = ", round(p2, 3)),
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
      labs(x = "R\u00f3\u017cnica proporcji  \u2014  Grupa 1 \u2212 Grupa 2",
           y = NULL, title = "R\u00f3\u017cnica + CI") +
      theme_educational() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      geom_vline(xintercept = 0, color = col_true_local,
                 linewidth = 1, linetype = "dashed") +
      annotate("text", x = 0, y = 0.45, label = "0 = brak r\u00f3\u017cnicy",
               color = col_true_local, fontface = "bold", size = 4, hjust = -0.1)

    if (step >= 3) {
      p_bot <- p_bot +
        geom_point(aes(x = diff_val, y = 0), color = col_estimate,
                   size = 7, shape = 18) +
        annotate("text", x = diff_val, y = -0.22,
                 label = paste0("p\u0302\u2081 \u2212 p\u0302\u2082 = ", round(diff_val, 3)),
                 color = col_estimate, fontface = "bold", size = 4.5)
    }

    if (step >= 4) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - se, xmax = diff_val + se, y = 0),
                       height = 0.08, color = col_success, linewidth = 1.8) +
        annotate("text", x = diff_val, y = 0.17,
                 label = paste0("\u00b1 SE = \u00b1", round(se, 3)),
                 color = col_success, fontface = "bold", size = 4)
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
      "1" = div(class = "callout-info",
        p(tags$strong("Krok 1:"), " Dwie pr\u00f3by.",
          " Mamy odpowiedzi TAK/NIE z dw\u00f3ch grup: ", tags$b(n1),
          " os\u00f3b w grupie 1 (", x1, " TAK / ", n1 - x1, " NIE) i ",
          tags$b(n2), " w grupie 2 (", x2, " TAK / ", n2 - x2, " NIE).
          Wida\u0107 ju\u017c, \u017ce w grupie 1 jest wi\u0119cej TAK\u00f3w, ale jak du\u017ca to
          r\u00f3\u017cnica i czy istotna?")
      ),
      "2" = div(class = "callout-info",
        p(tags$strong("Krok 2:"), " Dwie proporcje.",
          " Obliczamy proporcj\u0119 TAK\u00f3w w ka\u017cdej grupie:"),
        p(withMathJax(paste0("\\(\\hat{p}_1 = ", x1, "/", n1, " = ", round(p1, 3), "\\)"))),
        p(withMathJax(paste0("\\(\\hat{p}_2 = ", x2, "/", n2, " = ", round(p2, 3), "\\)"))),
        p("Ka\u017cda proporcja ma w\u0142asn\u0105 niepewno\u015b\u0107 \u2014 ale interesuje nas
          ", tags$b("r\u00f3\u017cnica mi\u0119dzy nimi"), ".")
      ),
      "3" = div(class = "callout-info",
        p(tags$strong("Krok 3:"), " R\u00f3\u017cnica.",
          " Estymator punktowy r\u00f3\u017cnicy:"),
        p(withMathJax(paste0("\\(\\hat{p}_1 - \\hat{p}_2 = ", round(p1, 3),
                             " - ", round(p2, 3), " = ",
                             round(diff_val, 3), "\\)"))),
        p("W dolnym panelu przenosimy si\u0119 do nowej skali \u2014 ", tags$b("skali r\u00f3\u017cnicy"),
          ". Punkt = nasze oszacowanie r\u00f3\u017cnicy. Pionowa linia na 0 oznacza ",
          tags$em("\"gdyby r\u00f3\u017cnicy nie by\u0142o\""),
          ". Teraz musimy otoczy\u0107 nasz\u0105 r\u00f3\u017cnic\u0119 przedzia\u0142em niepewno\u015bci.")
      ),
      "4" = div(class = "callout-info",
        p(tags$strong("Krok 4:"), " B\u0142\u0105d standardowy r\u00f3\u017cnicy (\u00b1 SE).",
          " SE r\u00f3\u017cnicy proporcji \u0142\u0105czy niepewno\u015bci z obu grup:"),
        p(withMathJax(paste0(
          "\\(SE = \\sqrt{\\frac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\frac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = ",
          round(se, 3), "\\)"))),
        p(tags$b("Wa\u017cne:"), " wariancje si\u0119 ", tags$em("dodaj\u0105"),
          ", nie odchylenia. Dlatego SE r\u00f3\u017cnicy jest mniejszy ni\u017c suma SE
          poszczeg\u00f3lnych proporcji.")
      ),
      "5" = {
        covers_zero <- (diff_val - me <= 0) & (0 <= diff_val + me)
        div(class = if (covers_zero) "callout-warning" else "callout-success",
          p(tags$strong("Krok 5:"), " Przedzia\u0142 ufno\u015bci dla r\u00f3\u017cnicy."),
          p("Warto\u015b\u0107 krytyczna z rozk\u0142adu normalnego: ",
            withMathJax("\\(z^* = 1.96\\)")),
          p(withMathJax(paste0("\\(ME = z^* \\cdot SE = 1.96 \\cdot ",
                               round(se, 3), " = ", round(me, 3), "\\)"))),
          p(tags$b("95% CI: ["),
            round(diff_val - me, 3), " ; ", round(diff_val + me, 3),
            tags$b("]")),
          p(tags$em(if (covers_zero)
              "CI obejmuje 0 \u2014 nie mo\u017cemy stwierdzi\u0107, \u017ce r\u00f3\u017cnica jest istotna."
            else
              paste0("CI nie obejmuje 0 \u2014 r\u00f3\u017cnica jest istotna. ",
                     "Mo\u017cemy stwierdzi\u0107 z 95% ufno\u015bci\u0105, \u017ce w grupie 1 odsetek TAK ",
                     "jest wi\u0119kszy o co najmniej ", round(diff_val - me, 3), ".")))
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
    switch(v, "yes" = "callout-success", "no" = "callout-danger",
           "maybe" = "callout-warning")
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
      steps = c("1. Pr\u00f3ba", "2. p\u0302", "3. \u00b1 SE", "4. Przedzia\u0142"),
      hypotheses = list(
        list(text = "Poparcie dla partii X przekracza 50% (\"prog\u00f3w wi\u0119kszo\u015bci\")",
             bound = 0.50, dir = "gt",
             explain_yes = "Dolna granica CI le\u017cy powy\u017cej 50%. Mo\u017cemy z 95% ufno\u015bci\u0105 stwierdzi\u0107, \u017ce wi\u0119kszo\u015b\u0107 wyborc\u00f3w popiera parti\u0119 X.",
             explain_no = "Dolna granica CI le\u017cy poni\u017cej 50% \u2014 mimo \u017ce p\u0302 = 53%, niepewno\u015b\u0107 sonda\u017cu nie pozwala stwierdzi\u0107 z 95% ufno\u015bci\u0105 \u017ce poparcie przekracza 50%."),
        list(text = "Poparcie dla partii X przekracza 60%",
             bound = 0.60, dir = "gt",
             explain_no = "G\u00f3rna granica CI le\u017cy poni\u017cej 60%. Ca\u0142y CI poza obszarem hipotezy \u2014 nie ma podstaw do twierdzenia, \u017ce poparcie przekracza 60%.")
      )
    ),
    A2 = list(
      type = "compare_n_prop",
      data = list(phat = 0.08, ns = c(50, 200, 1000)),
      xlab = "Odsetek wadliwych produkt\u00f3w",
      steps = c("1. n = 50", "2. n = 200", "3. n = 1000"),
      hypotheses = list(
        list(text = "Odsetek wadliwych produkt\u00f3w przekracza 5%",
             bound = 0.05, dir = "gt",
             explain_yes = "Dla najwi\u0119kszej pr\u00f3by (n=1000) dolna granica CI le\u017cy powy\u017cej 5% \u2014 z 95% ufno\u015bci\u0105 odsetek wadliwych przekracza norm\u0119 5%. Zauwa\u017c: dla n=50 CI jest tak szeroki, \u017ce obejmuje r\u00f3wnie\u017c 5%, wi\u0119c na ma\u0142ej pr\u00f3bie nie m\u00f3glby\u015b nic stwierdzi\u0107.",
             explain_no = "Nawet przy n=1000 nie mo\u017cemy stwierdzi\u0107 z 95% pewno\u015bci\u0105, \u017ce odsetek przekracza 5%."),
        list(text = "Odsetek wadliwych produkt\u00f3w przekracza 12%",
             bound = 0.12, dir = "gt",
             explain_no = "G\u00f3rna granica CI nawet dla n=1000 le\u017cy poni\u017cej 12%. Dla n=50 CI si\u0119ga prawie 16% \u2014 ma\u0142a pr\u00f3ba mog\u0142aby b\u0142\u0119dnie sugerowa\u0107 problem. To pokazuje, dlaczego du\u017ce n daje bardziej definitywne odpowiedzi.")
      )
    ),
    B1 = list(
      type = "diff_props",
      data = list(x1 = 124, n1 = 200, x2 = 84, n2 = 200,
                  label1 = "Lek", label2 = "Placebo"),
      xlab = "Odsetek z ust\u0105pieniem b\u00f3lu",
      steps = c("1. Pr\u00f3by", "2. Dwie p\u0302", "3. R\u00f3\u017cnica", "4. \u00b1 SE", "5. Przedzia\u0142"),
      hypotheses = list(
        list(text = "Lek dzia\u0142a skuteczniej ni\u017c placebo (r\u00f3\u017cnica > 0)",
             bound = 0, dir = "gt",
             explain_yes = "Ca\u0142y CI dla r\u00f3\u017cnicy le\u017cy powy\u017cej 0. Lek rzeczywi\u015bcie pomaga skuteczniej ni\u017c placebo \u2014 r\u00f3\u017cnica jest istotna statystycznie."),
        list(text = "Lek poprawia skuteczno\u015b\u0107 o wi\u0119cej ni\u017c 25 punkt\u00f3w procentowych",
             bound = 0.25, dir = "gt",
             explain_no = "G\u00f3rna granica CI dla r\u00f3\u017cnicy le\u017cy poni\u017cej 0.25. Lek dzia\u0142a, ale poprawa skuteczno\u015bci wzgl\u0119dem placebo jest mniejsza ni\u017c 25 pkt proc.")
      )
    ),
    B2 = list(
      type = "diff_props",
      data = list(x1 = 22, n1 = 250, x2 = 18, n2 = 250,
                  label1 = "Linia A", label2 = "Linia B"),
      xlab = "Odsetek wadliwych",
      steps = c("1. Pr\u00f3by", "2. Dwie p\u0302", "3. R\u00f3\u017cnica", "4. \u00b1 SE", "5. Przedzia\u0142"),
      hypotheses = list(
        list(text = "Linia A produkuje wi\u0119cej brak\u00f3w ni\u017c linia B (r\u00f3\u017cnica > 0)",
             bound = 0, dir = "gt",
             explain_no = "CI dla r\u00f3\u017cnicy obejmuje 0 \u2014 mimo \u017ce p\u0302\u2081 (8.8%) jest wy\u017csze ni\u017c p\u0302\u2082 (7.2%), nie mo\u017cemy z 95% ufno\u015bci\u0105 stwierdzi\u0107, \u017ce linia A jest gorsza. R\u00f3\u017cnica mo\u017ce by\u0107 efektem przypadku."),
        list(text = "R\u00f3\u017cnica w odsetku brak\u00f3w mi\u0119dzy liniami jest mniejsza ni\u017c 5 pkt proc",
             bound = 0.05, dir = "lt",
             explain_yes = "G\u00f3rna granica CI dla r\u00f3\u017cnicy le\u017cy poni\u017cej 0.05. Mo\u017cemy by\u0107 pewni, \u017ce nawet je\u015bli kt\u00f3ra\u015b linia jest gorsza, to r\u00f3\u017cnica nie przekracza 5 pkt proc.")
      )
    ),
    B3 = list(
      type = "diff_props",
      data = list(x1 = 6, n1 = 30, x2 = 9, n2 = 30,
                  label1 = "Zak\u0142ad A", label2 = "Zak\u0142ad B"),
      xlab = "Odsetek wypadk\u00f3w",
      steps = c("1. Pr\u00f3by", "2. Dwie p\u0302", "3. R\u00f3\u017cnica", "4. \u00b1 SE", "5. Przedzia\u0142"),
      hypotheses = list(
        list(text = "Zak\u0142ad A jest bezpieczniejszy ni\u017c B (r\u00f3\u017cnica < 0)",
             bound = 0, dir = "lt",
             explain_no = "Mimo \u017ce p\u0302\u2081 = 20% jest wyra\u017anie mniejsze od p\u0302\u2082 = 30%, CI dla r\u00f3\u017cnicy obejmuje 0. Pr\u00f3ba 30 os\u00f3b w ka\u017cdym zak\u0142adzie to za ma\u0142o, \u017ceby z 95% ufno\u015bci\u0105 stwierdzi\u0107, kt\u00f3ry jest bezpieczniejszy. To klasyczna pu\u0142apka: \"du\u017ca\" r\u00f3\u017cnica w punktach procentowych mo\u017ce by\u0107 statystycznie nieistotna przy ma\u0142ej pr\u00f3bie."),
        list(text = "R\u00f3\u017cnica wypadkowo\u015bci mi\u0119dzy zak\u0142adami przekracza 30 pkt proc",
             bound = 0.30, dir = "lt",
             explain_yes = "G\u00f3rna granica CI le\u017cy wyra\u017anie poni\u017cej 0.30 \u2014 mo\u017cemy wykluczy\u0107 a\u017c tak du\u017c\u0105 r\u00f3\u017cnic\u0119, ale ma\u0142a pr\u00f3ba nie pozwala nam dok\u0142adnie wskaza\u0107, jaka ona jest.")
      )
    ),
    C1 = list(
      type = "forest_prop",
      data = list(
        groups = c("Szpital A", "Szpital B", "Szpital C", "Szpital D"),
        x = c(12, 18, 9, 35),
        n = c(150, 180, 160, 170)
      ),
      xlab = "Odsetek powik\u0142a\u0144 pooperacyjnych",
      steps = c("1. Liczby", "2. Proporcje", "3. CI"),
      hypotheses = list(
        list(kind = "pairwise",
             text = "Kt\u00f3re szpitale r\u00f3\u017cni\u0105 si\u0119 istotnie odsetkiem powik\u0142a\u0144?",
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
                size = 5, color = col_dark) +
      scale_fill_manual(values = c("NIE" = col_secondary, "TAK" = col_primary),
                        guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "Liczebno\u015b\u0107",
           title = paste0("Dane (n = ", n, ")")) +
      theme_educational() +
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
      labs(x = xlab, y = NULL, title = title) +
      theme_educational() +
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
                 label = paste0(if (hypothesis$dir == "gt") "\u2265 " else "\u2264 ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    }

    # Krok 2+: punkt p_hat
    if (step >= 2) {
      p_right <- p_right +
        geom_point(aes(x = phat, y = 0), color = col_estimate, size = 7, shape = 18) +
        annotate("text", x = phat, y = -0.22,
                 label = paste0("p\u0302 = ", round(phat, 3)),
                 color = col_estimate, fontface = "bold", size = 4.8)
    }

    # Krok 3+: SE
    if (step >= 3) {
      p_right <- p_right +
        geom_errorbarh(aes(xmin = phat - se, xmax = phat + se, y = 0),
                       height = 0.08, color = col_success, linewidth = 1.8) +
        annotate("text", x = phat, y = 0.20,
                 label = paste0("\u00b1 SE = \u00b1", round(se, 3)),
                 color = col_success, fontface = "bold", size = 4)
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
      labs(x = xlab, y = NULL, title = title) +
      theme_educational() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    p <- p +
      annotate("text", x = xlims[1], y = y_positions,
               label = df$label, hjust = 0, fontface = "bold", size = 4.5,
               color = col_dark)

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
                 label = paste0(if (hypothesis$dir == "gt") "\u2265 " else "\u2264 ",
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
                vjust = -0.4, fontface = "bold", size = 4.2, color = col_dark) +
      scale_fill_manual(values = c("NIE" = col_secondary, "TAK" = col_primary),
                        name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(x = NULL, y = "Liczebno\u015b\u0107", title = "Dane") +
      theme_educational() +
      theme(legend.position = "top",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())

    # ---- PRAWY GORNY PANEL: dwie p_hat na osi proporcji ----
    p_top <- ggplot() +
      xlim(0, 1) +
      ylim(0.4, 2.6) +
      labs(x = xlab, y = NULL, title = "Proporcje w grupach") +
      theme_educational() +
      theme(axis.text.y = element_text(face = "bold", size = 11),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      scale_y_continuous(breaks = c(1, 2), labels = c(label1, label2),
                         limits = c(0.4, 2.6))

    if (step >= 2) {
      p_top <- p_top +
        geom_point(aes(x = p1, y = 1), color = col_estimate, size = 7, shape = 18) +
        annotate("text", x = p1, y = 1.45, label = paste0("p\u0302\u2081 = ", round(p1, 3)),
                 color = col_estimate, fontface = "bold", size = 4.2) +
        geom_point(aes(x = p2, y = 2), color = col_estimate, size = 7, shape = 18) +
        annotate("text", x = p2, y = 2.45, label = paste0("p\u0302\u2082 = ", round(p2, 3)),
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
      labs(x = paste0("R\u00f3\u017cnica proporcji  \u2014  ", label1, " \u2212 ", label2),
           y = NULL, title = "R\u00f3\u017cnica + CI") +
      theme_educational() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      geom_vline(xintercept = 0, color = col_true_local,
                 linewidth = 1, linetype = "dashed") +
      annotate("text", x = 0, y = 0.45, label = "0 = brak r\u00f3\u017cnicy",
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
                 label = paste0(if (hypothesis$dir == "gt") "\u2265 " else "\u2264 ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4, hjust = -0.1)
    }

    if (step >= 3) {
      p_bot <- p_bot +
        geom_point(aes(x = diff_val, y = 0), color = col_estimate,
                   size = 7, shape = 18) +
        annotate("text", x = diff_val, y = -0.22,
                 label = paste0("p\u0302\u2081 \u2212 p\u0302\u2082 = ", round(diff_val, 3)),
                 color = col_estimate, fontface = "bold", size = 4.5)
    }

    if (step >= 4) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - se, xmax = diff_val + se, y = 0),
                       height = 0.08, color = col_success, linewidth = 1.8) +
        annotate("text", x = diff_val, y = 0.17,
                 label = paste0("\u00b1 SE = \u00b1", round(se, 3)),
                 color = col_success, fontface = "bold", size = 4)
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
      theme_educational() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Etykiety grup
    p <- p +
      annotate("text", x = xlims[1], y = y_positions,
               label = groups, hjust = 0, fontface = "bold", size = 4.5,
               color = col_dark)

    # Krok 1+: surowe liczby x/n
    if (step >= 1) {
      p <- p +
        annotate("text", x = xlims[2], y = y_positions,
                 label = group_df$label, hjust = 1, size = 4,
                 color = col_dark, fontface = "italic")
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
      btn_class <- if (current == i) "btn-primary" else "btn-outline-primary"
      actionButton(paste0("ch4_case", case_id, "_step", i),
                   cfg$steps[i], class = btn_class)
    })

    hyp_btns <- if (current >= n_core) {
      lapply(seq_along(cfg$hypotheses), function(j) {
        is_active <- !is.null(phase) && phase$idx == j
        btn_class <- if (is_active) "btn-warning" else "btn-outline-warning"
        actionButton(paste0("ch4_case", case_id, "_hyp", j),
                     paste0("Hipoteza ", j), class = btn_class)
      })
    } else {
      list(helpText("Wybuduj pe\u0142ny przedzia\u0142, \u017ceby sprawdzi\u0107 hipotezy."))
    }

    reveal_row <- if (!is.null(phase) && !phase$reveal) {
      div(class = "step-buttons", style = "margin-top: 4px;",
        actionButton(paste0("ch4_case", case_id, "_reveal"),
                     "\U0001f50d Poka\u017c werdykt", class = "btn-success"))
    } else {
      NULL
    }

    tagList(
      div(class = "step-buttons", core_btns),
      div(class = "step-buttons", style = "margin-top: 4px;", hyp_btns),
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
                   label = "Kliknij pierwszy krok, \u017ceby zacz\u0105\u0107",
                   size = 5, color = "#7f8c8d") +
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
            tags$td("\u2014", style = "padding: 4px 8px; text-align: center; color: #95a5a6;")
          } else if (mat[i, j]) {
            tags$td("\u2713", style = "padding: 4px 8px; text-align: center; color: #27ae60; font-weight: bold; font-size: 16px;")
          } else {
            tags$td("\u00d7", style = "padding: 4px 8px; text-align: center; color: #e74c3c; font-size: 16px;")
          }
        })
      )
    })
    tags$table(
      style = "border-collapse: collapse; margin: 8px auto; border: 1px solid #bdc3c7;",
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
        "\u017badna para grup nie wykaza\u0142a istotnej r\u00f3\u017cnicy w odsetkach \u2014 ",
        "wszystkie 95% CI nak\u0142adaj\u0105 si\u0119 wzajemnie. Na podstawie tych danych ",
        "nie mo\u017cemy stwierdzi\u0107 r\u00f3\u017cnic mi\u0119dzy grupami."
      ))
    }

    # Czy jedna grupa odstaje od WSZYSTKICH innych?
    standout_idx <- which(sapply(seq_len(k), function(i) all(mat[i, -i])))
    if (length(standout_idx) == 1) {
      i <- standout_idx
      others <- phats[-i]
      direction <- if (phats[i] > max(others)) "wy\u017cszy" else "ni\u017cszy"
      return(paste0(
        "Spo\u015br\u00f3d wszystkich badanych grup wyra\u017anie odstaje ",
        tags$b(groups[i]), " (odsetek ", round(phats[i] * 100, 1), "%) \u2014 ma istotnie ",
        direction, " odsetek ni\u017c ka\u017cda z pozosta\u0142ych grup ",
        "(jego 95% CI nie nak\u0142ada si\u0119 z \u017cadnym innym). ",
        "Pozosta\u0142e grupy maj\u0105 odsetki w przedziale ",
        round(min(others) * 100, 1), "%\u2013", round(max(others) * 100, 1), "%, ",
        "a ich CI nak\u0142adaj\u0105 si\u0119 \u2014 nie mo\u017cemy stwierdzi\u0107 mi\u0119dzy nimi istotnych r\u00f3\u017cnic."
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
      "Spo\u015br\u00f3d wszystkich por\u00f3wna\u0144 jedynie jedna para wykaza\u0142a istotn\u0105 r\u00f3\u017cnic\u0119: "
    } else {
      paste0("Istotne r\u00f3\u017cnice (CI nie nak\u0142adaj\u0105 si\u0119) wykaza\u0142y ",
             n_diff, " pary: ")
    }

    paste0(
      intro, pairs_inline, ". ",
      "Pozosta\u0142e pary nie r\u00f3\u017cni\u0105 si\u0119 istotnie \u2014 ich 95% CI nak\u0142adaj\u0105 si\u0119, ",
      "wi\u0119c na podstawie tych danych nie mo\u017cemy mi\u0119dzy nimi rozr\u00f3\u017cni\u0107."
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
        return(div(class = "callout-info",
          p(tags$strong("Hipoteza ", phase$idx, ": "), hyp$text),
          p(tags$em("Spojrz na wykres: gdzie lezy CI wzgledem obszaru hipotezy?
                    Co o tym sadzicie? Klikni\u0119cie ", tags$b("Poka\u017c werdykt"),
                    " odsloni odpowied\u017a."))
        ))
      }

      # Sub-faza 2: werdykt + wyjasnienie
      if (!is.null(hyp$kind) && hyp$kind == "pairwise") {
        mat <- forest_prop_pairwise_matrix(cfg$data)
        narrative <- pairwise_narrative(cfg$data, mat)
        return(div(class = "callout-success",
          p(tags$strong("Hipoteza: "), hyp$text),
          p(tags$strong("Werdykt \u2014 macierz par:")),
          p(tags$em("\u2713 = grupy r\u00f3\u017cni\u0105 si\u0119 istotnie (CI nie nak\u0142adaj\u0105 si\u0119);  ",
                    "\u00d7 = nie mo\u017cna stwierdzi\u0107 r\u00f3\u017cnicy (CI nak\u0142adaj\u0105 si\u0119)"),
            style = "font-size: 12px; color: #7f8c8d;"),
          render_pairwise_table(mat),
          p(tags$strong("Jak to opisa\u0107 w raporcie:"),
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
        p("CI przecina granic\u0119 hipotezy \u2014 nie mo\u017cemy jednoznacznie
          stwierdzi\u0107, czy jest prawdziwa.")
      }

      return(div(class = cls,
        p(tags$strong("Hipoteza ", phase$idx, ": "), hyp$text),
        p(tags$strong("Werdykt: ", label)),
        body
      ))
    }

    # Faza budowy CI \u2014 wyjasnienie ostatniego kroku
    div(class = "callout-info",
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
