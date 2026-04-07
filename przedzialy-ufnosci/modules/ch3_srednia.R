# ============================================================================
# CHAPTER 3: Przedzial dla sredniej
# ============================================================================

ch3_ui <- tabPanel("3. Przedzia\u0142 dla \u015bredniej",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Wiemy ju\u017c, czym jest przedzia\u0142 ufno\u015bci i jak go interpretowa\u0107.
       Czas na konkrety: wz\u00f3r i obliczenia."
    ),

    div(class = "section-title", "Wz\u00f3r"),

    div(class = "narrative",
      p("Przedzia\u0142 ufno\u015bci dla \u015bredniej populacji wygl\u0105da tak:"),
      div(class = "formula-box",
        withMathJax("$$CI = \\bar{x} \\pm t^*_{\\alpha/2,\\, n-1} \\cdot \\frac{s}{\\sqrt{n}}$$")
      ),
      p("Trzy sk\u0142adniki:"),
      tags$ul(
        tags$li(withMathJax("\\(\\bar{x}\\)"),
                " \u2014 \u015brednia z pr\u00f3by (\u015brodek przedzia\u0142u)"),
        tags$li(withMathJax("\\(s/\\sqrt{n}\\)"),
                " \u2014 b\u0142\u0105d standardowy \u015bredniej (jak bardzo \u015brednia z pr\u00f3by waha si\u0119 z pr\u00f3by na pr\u00f3b\u0119)"),
        tags$li(withMathJax("\\(t^*\\)"),
                " \u2014 warto\u015b\u0107 krytyczna z rozk\u0142adu t-Studenta zale\u017cna od poziomu ufno\u015bci i ", withMathJax("\\(n-1\\)"), " stopni swobody")
      ),
      p(tags$b("Dlaczego rozk\u0142ad t, a nie normalny (z)?"),
        " Bo ", withMathJax("\\(\\sigma\\)"), " populacji nie znamy \u2014 szacujemy je z pr\u00f3by jako ",
        withMathJax("\\(s\\)"),
        ". To dodaje niepewno\u015bci, dlatego u\u017cywamy szerszego rozk\u0142adu (t ma \"grubsze ogony\" ni\u017c normalny).
        Im wi\u0119ksze ", withMathJax("\\(n\\)"),
        ", tym lepsze oszacowanie ", withMathJax("\\(\\sigma\\)"),
        " i tym bardziej rozk\u0142ad t przypomina normalny."),
      p(tags$b("W praktyce nie musisz si\u0119 tym przejmowa\u0107."),
        " Programy statystyczne (jamovi, SPSS, R) ", tags$em("zawsze"),
        " licz\u0105 CI dla \u015bredniej u\u017cywaj\u0105c rozk\u0142adu t. Nie ma osobnego \"z-przedzia\u0142u\"
        do wyboru. Ten rozdzia\u0142 nauczy Ci\u0119 ", tags$b("interpretowa\u0107"),
        " gotowe przedzia\u0142y \u2014 a nie liczy\u0107 je r\u0119cznie.")
    ),

    # ========================================================================
    # WIDGET 1: Budowa przedzialu krok po kroku
    # ========================================================================
    div(class = "section-title", "Budowa przedzia\u0142u \u2014 krok po kroku"),

    div(class = "narrative",
      p("Zobaczmy, jak z konkretnej pr\u00f3by (25 pomiar\u00f3w wzrostu) powstaje przedzia\u0142 ufno\u015bci.
        Przejd\u017a przez 4 kroki, obserwuj\u0105c co pojawia si\u0119 na wykresie.")
    ),

    div(class = "widget-block",
      h4("Konstruowanie przedzia\u0142u"),
      div(class = "step-buttons",
        actionButton("ch3_step1", "1. Pr\u00f3ba",    class = "btn-outline-primary"),
        actionButton("ch3_step2", "2. \u015arednia",  class = "btn-outline-primary"),
        actionButton("ch3_step3", "3. \u00b1 SE",     class = "btn-outline-primary"),
        actionButton("ch3_step4", "4. Przedzia\u0142", class = "btn-outline-primary")
      ),
      div(style = "display: flex; gap: 8px; margin-top: 8px;",
        actionButton("ch3_step_new_sample", "\u21bb Nowa pr\u00f3ba",
                     class = "btn-outline-secondary btn-sm")
      ),
      plotOutput("ch3_step_plot", height = "340px"),
      uiOutput("ch3_step_explanation")
    ),

    # ========================================================================
    # WIDGET 2: Budowa przedzialu dla roznicy srednich
    # ========================================================================
    div(class = "section-title", "Budowa przedzia\u0142u dla r\u00f3\u017cnicy \u015brednich"),

    div(class = "narrative",
      p("CI dla r\u00f3\u017cnicy dw\u00f3ch \u015brednich buduje si\u0119 analogicznie, ale b\u0142\u0105d standardowy
        jest inny \u2014 trzeba po\u0142\u0105czy\u0107 niepewno\u015b\u0107 z obu pr\u00f3b:"),
      div(class = "formula-box",
        withMathJax("$$CI = (\\bar{x}_1 - \\bar{x}_2) \\pm t^* \\cdot \\sqrt{\\frac{s_1^2}{n_1} + \\frac{s_2^2}{n_2}}$$")
      ),
      p("Por\u00f3wnamy wzrost m\u0119\u017cczyzn i kobiet \u2014 po 25 os\u00f3b w ka\u017cdej grupie.")
    ),

    div(class = "widget-block",
      h4("Konstruowanie CI dla r\u00f3\u017cnicy"),
      div(class = "step-buttons",
        actionButton("ch3_dstep1", "1. Dwie pr\u00f3by",   class = "btn-outline-primary"),
        actionButton("ch3_dstep2", "2. Dwie \u015brednie", class = "btn-outline-primary"),
        actionButton("ch3_dstep3", "3. R\u00f3\u017cnica",     class = "btn-outline-primary"),
        actionButton("ch3_dstep4", "4. \u00b1 SE",          class = "btn-outline-primary"),
        actionButton("ch3_dstep5", "5. Przedzia\u0142",    class = "btn-outline-primary")
      ),
      div(style = "display: flex; gap: 8px; margin-top: 8px;",
        actionButton("ch3_dstep_new_sample", "\u21bb Nowe pr\u00f3by",
                     class = "btn-outline-secondary btn-sm")
      ),
      plotOutput("ch3_dstep_plot", height = "420px"),
      uiOutput("ch3_dstep_explanation")
    ),

    # ========================================================================
    # WIDGET 3: CASE STUDIES (konstruktory + hipotezy)
    # ========================================================================
    div(class = "section-title", "Case studies \u2014 jak interpretowa\u0107 CI w praktyce"),

    div(class = "narrative",
      p("Poni\u017cej kilka realistycznych sytuacji. W ka\u017cdej ", tags$b("budujesz CI krok po kroku"),
        " (jak w poprzednich sekcjach), a na ko\u0144cu weryfikujesz dwie hipotezy:
        jedn\u0105, kt\u00f3ra jest prawdziwa, i jedn\u0105, kt\u00f3ra nie jest. Klikaj nag\u0142\u00f3wki,
        \u017ceby rozwija\u0107 case'y.")
    ),

    # ----- GRUPA A: JEDNA SREDNIA -----
    div(class = "section-title", style = "font-size: 18px; margin-top: 25px;",
        "A. Przedzia\u0142 dla jednej \u015bredniej"),

    tags$details(class = "case-study", open = NA,
      tags$summary(
        span(class = "case-icon", "\U0001f4cf"),
        "A1. Wzrost student\u00f3w \u2014 czytanie pojedynczego CI"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Zmierzy\u0142e\u015b wzrost ", tags$b("30 student\u00f3w"),
            ". \u015arednia z pr\u00f3by ", withMathJax("\\(\\bar{x} = 173.4\\)"), " cm,
            odchylenie standardowe ", withMathJax("\\(s = 8.2\\)"), " cm.
            Zbudujmy CI dla \u015bredniego wzrostu i sprawd\u017amy dwie hipotezy.")
        ),
        uiOutput("ch3_caseA1_buttons"),
        plotOutput("ch3_caseA1_plot", height = "260px"),
        uiOutput("ch3_caseA1_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f50d"),
        "A2. Ten sam pomiar, trzy r\u00f3\u017cne wielko\u015bci pr\u00f3by"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Por\u00f3wnaj trzy badania mierz\u0105ce st\u0119\u017cenie zanieczyszczenia
            (\u00b5g/m\u00b3). \u015arednia = 32.0, s = 8.0, ale ",
            tags$b("n r\u00f3\u017cne"), " (10, 50, 200). Dodawaj CI jeden po drugim
            i patrz, jak si\u0119 zw\u0119\u017caj\u0105.")
        ),
        uiOutput("ch3_caseA2_buttons"),
        plotOutput("ch3_caseA2_plot", height = "260px"),
        uiOutput("ch3_caseA2_explain")
      )
    ),

    # ----- GRUPA B: ROZNICA SREDNICH -----
    div(class = "section-title", style = "font-size: 18px; margin-top: 25px;",
        "B. Przedzia\u0142 dla r\u00f3\u017cnicy \u015brednich"),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f48a"),
        "B1. Test leku na ci\u015bnienie \u2014 CI dla r\u00f3\u017cnicy nie obejmuje 0"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Badamy nowy lek na obni\u017cenie ci\u015bnienia krwi.
            ", tags$b("Lek:"), " n=40, \u015brednie obni\u017cenie 12.3 mmHg, s=4.5.
            ", tags$b("Placebo:"), " n=40, \u015brednie obni\u017cenie 4.1 mmHg, s=4.2.")
        ),
        uiOutput("ch3_caseB1_buttons"),
        plotOutput("ch3_caseB1_plot", height = "380px"),
        uiOutput("ch3_caseB1_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f33f"),
        "B2. Dwa nawozy \u2014 CI dla r\u00f3\u017cnicy obejmuje 0"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Por\u00f3wnujesz plon kukurydzy dla dw\u00f3ch nawoz\u00f3w.
            ", tags$b("Nawoz X:"), " n=25, \u015brednia 8.4 t/ha, s=1.2.
            ", tags$b("Nawoz Y:"), " n=25, \u015brednia 8.1 t/ha, s=1.3.")
        ),
        uiOutput("ch3_caseB2_buttons"),
        plotOutput("ch3_caseB2_plot", height = "380px"),
        uiOutput("ch3_caseB2_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\u26a0\ufe0f"),
        "B3. Pu\u0142apka nak\u0142adaj\u0105cych si\u0119 CI"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Mierzysz czas reakcji w dw\u00f3ch grupach (n=150 ka\u017cda).
            ", tags$b("Grupa A:"), " \u015brednia 350 ms, s=45.
            ", tags$b("Grupa B:"), " \u015brednia 362 ms, s=45.
            CI ka\u017cdej grupy osobno si\u0119 nak\u0142adaj\u0105 \u2014 czy r\u00f3\u017cnica jest istotna?")
        ),
        uiOutput("ch3_caseB3_buttons"),
        plotOutput("ch3_caseB3_plot", height = "380px"),
        uiOutput("ch3_caseB3_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f4ca"),
        "B4. Istotne statystycznie \u2260 wa\u017cne praktycznie"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Bardzo du\u017ce badanie por\u00f3wnuje IQ w dw\u00f3ch wojew\u00f3dztwach.
            ", tags$b("Wojew. A:"), " n=20\u202f000, \u015brednia 100.4, s=15.
            ", tags$b("Wojew. B:"), " n=20\u202f000, \u015brednia 100.0, s=15.
            R\u00f3\u017cnica 0.4 pkt IQ \u2014 du\u017co czy ma\u0142o?")
        ),
        uiOutput("ch3_caseB4_buttons"),
        plotOutput("ch3_caseB4_plot", height = "380px"),
        uiOutput("ch3_caseB4_explain")
      )
    ),

    # ----- GRUPA C: WIELE GRUP -----
    div(class = "section-title", style = "font-size: 18px; margin-top: 25px;",
        "C. Wiele grup \u2014 forest plot"),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f3eb"),
        "C1. Cztery metody nauczania \u2014 czy kt\u00f3ra\u015b wystaje?"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Por\u00f3wnujesz \u015bredni wynik egzaminu (0\u201340 pkt) dla student\u00f3w
            ucz\u0105cych si\u0119 czterema metodami (po 25 student\u00f3w w ka\u017cdej).
            Dodawaj CI jeden po drugim i obserwuj.")
        ),
        uiOutput("ch3_caseC1_buttons"),
        plotOutput("ch3_caseC1_plot", height = "300px"),
        uiOutput("ch3_caseC1_explain")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f3e5"),
        "C2. Pi\u0119\u0107 oddzia\u0142\u00f3w szpitalnych \u2014 czas oczekiwania"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Mierzysz \u015bredni czas oczekiwania na konsultacj\u0119 (minuty) w pi\u0119ciu
            oddzia\u0142ach szpitala. Kt\u00f3ry wymaga interwencji?")
        ),
        uiOutput("ch3_caseC2_buttons"),
        plotOutput("ch3_caseC2_plot", height = "340px"),
        uiOutput("ch3_caseC2_explain")
      )
    ),

    div(class = "callout-info",
      tags$strong("Najwa\u017cniejsze do zapami\u0119tania:"),
      tags$ol(
        tags$li("CI ", tags$b("dla r\u00f3\u017cnicy"),
                " m\u00f3wi czy r\u00f3\u017cnica jest istotna \u2014 sprawd\u017a czy zawiera 0."),
        tags$li("Nie por\u00f3wnuj nak\u0142adania si\u0119 CI poszczeg\u00f3lnych grup \u2014
                to MO\u017bE da\u0107 mylny obraz. Zawsze patrz na CI ",
                tags$b("dla r\u00f3\u017cnicy"), "."),
        tags$li("\"Istotne statystycznie\" \u2260 \"wa\u017cne praktycznie\".
                Przy bardzo du\u017cym n nawet trywialne r\u00f3\u017cnice b\u0119d\u0105 istotne."),
        tags$li("Forest plot to standardowy spos\u00f3b por\u00f3wnania wielu grup.
                Patrz nie tylko na \u015brednie, ale przede wszystkim na ", tags$b("d\u0142ugo\u015b\u0107"),
                " ka\u017cdego CI.")
      )
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: przedzia\u0142 ufno\u015bci dla proporcji"),
      actionButton("ch3_next", "Dalej \u2192 4. Przedzia\u0142 dla proporcji",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  # --- Widget 1: Budowa przedzialu krok po kroku ---
  ch3_step <- reactiveVal(0)
  ch3_step_sample <- reactiveVal(NULL)

  # Generuj probke na starcie (po pierwszym kliknieciu dowolnego kroku)
  generate_step_sample <- function() {
    set.seed(sample.int(.Machine$integer.max, 1))
    generate_population_sample("normal", 25)
  }

  observeEvent(input$ch3_step1, {
    if (is.null(ch3_step_sample())) {
      ch3_step_sample(generate_step_sample())
    }
    ch3_step(1)
  })
  observeEvent(input$ch3_step2, {
    if (is.null(ch3_step_sample())) {
      ch3_step_sample(generate_step_sample())
    }
    ch3_step(2)
  })
  observeEvent(input$ch3_step3, {
    if (is.null(ch3_step_sample())) {
      ch3_step_sample(generate_step_sample())
    }
    ch3_step(3)
  })
  observeEvent(input$ch3_step4, {
    if (is.null(ch3_step_sample())) {
      ch3_step_sample(generate_step_sample())
    }
    ch3_step(4)
  })
  observeEvent(input$ch3_step_new_sample, {
    ch3_step_sample(generate_step_sample())
    # zostawiamy biezacy step, zeby user zobaczyl od razu jak zmienia sie wykres
    if (ch3_step() == 0) ch3_step(1)
  })

  output$ch3_step_plot <- renderPlot({
    step <- ch3_step()
    samp <- ch3_step_sample()

    if (step == 0 || is.null(samp)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Kliknij '1. Pr\u00f3ba' aby zacz\u0105\u0107",
                   size = 6, color = "#7f8c8d") +
          theme_void()
      )
    }

    xbar <- mean(samp)
    s <- sd(samp)
    n <- length(samp)
    se <- s / sqrt(n)
    t_star <- qt(0.975, df = n - 1)
    me <- t_star * se

    # Stala os X dla wszystkich krokow (oparta na surowych danych + CI)
    xlims <- range(c(samp, xbar - 1.2 * me, xbar + 1.2 * me))
    pad <- diff(xlims) * 0.05
    xlims <- c(xlims[1] - pad, xlims[2] + pad)

    # Jitter punktow na Y (deterministyczny na podstawie wartosci)
    set.seed(42)
    jitter_y <- runif(n, min = 0.15, max = 0.55)
    samp_df <- data.frame(x = samp, y = jitter_y)

    p <- ggplot() +
      xlim(xlims) +
      ylim(-0.55, 0.75) +
      labs(x = "Wzrost (cm)", y = NULL) +
      theme_ci() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Krok 1+: surowe punkty z proby
    if (step >= 1) {
      p <- p + geom_point(data = samp_df, aes(x = x, y = y),
                          color = col_primary, size = 3, alpha = 0.7)
    }

    # Krok 2+: linia pionowa i diament srednia
    if (step >= 2) {
      p <- p +
        geom_vline(xintercept = xbar, color = col_estimate,
                   linewidth = 1, linetype = "dotted") +
        geom_point(aes(x = xbar, y = 0), color = col_estimate,
                   size = 7, shape = 18) +
        annotate("text", x = xbar, y = -0.18,
                 label = paste0("x\u0304 = ", round(xbar, 2)),
                 color = col_estimate, fontface = "bold", size = 5)
    }

    # Krok 3: przedzial +/- SE (zielony, wezszy)
    if (step >= 3) {
      p <- p +
        geom_errorbarh(aes(xmin = xbar - se, xmax = xbar + se, y = 0),
                       height = 0.06, color = col_success, linewidth = 1.8) +
        annotate("text", x = xbar, y = 0.14,
                 label = paste0("\u00b1 SE = \u00b1", round(se, 2)),
                 color = col_success, fontface = "bold", size = 4.5)
    }

    # Krok 4: pelny CI (t* * SE, szerszy, niebieski)
    if (step >= 4) {
      p <- p +
        geom_errorbarh(aes(xmin = xbar - me, xmax = xbar + me, y = 0),
                       height = 0.12, color = col_ci, linewidth = 2.2,
                       alpha = 0.6) +
        annotate("text", x = xbar, y = -0.38,
                 label = paste0("95% CI: [", round(xbar - me, 2),
                                " ; ", round(xbar + me, 2), "]"),
                 color = col_ci, fontface = "bold", size = 5)
    }

    p + ggtitle(paste0("Krok ", step, " z 4"))
  })

  output$ch3_step_explanation <- renderUI({
    step <- ch3_step()
    samp <- ch3_step_sample()
    if (step == 0 || is.null(samp)) return(NULL)

    xbar <- mean(samp)
    s <- sd(samp)
    n <- length(samp)
    se <- s / sqrt(n)
    t_star <- qt(0.975, df = n - 1)
    me <- t_star * se

    switch(as.character(step),
      "1" = div(class = "callout-info",
        p(tags$strong("Krok 1: Pr\u00f3ba."),
          " Pobrali\u015bmy ", tags$b(n), " pomiar\u00f3w wzrostu. Ka\u017cda kropka to jedna osoba.
          Zauwa\u017c, jak bardzo surowe obserwacje s\u0105 ", tags$b("rozrzucone"),
          " \u2014 rozrzut indywidualny w populacji jest du\u017cy."),
        p("Statystyki z pr\u00f3by: ",
          withMathJax(paste0("\\(\\bar{x} = ", round(xbar, 2), "\\)")),
          ", ",
          withMathJax(paste0("\\(s = ", round(s, 2), "\\)")),
          ", ",
          withMathJax(paste0("\\(n = ", n, "\\)")), ".")
      ),
      "2" = div(class = "callout-info",
        p(tags$strong("Krok 2: \u015arednia z pr\u00f3by."),
          " Obliczamy ",
          withMathJax(paste0("\\(\\bar{x} = ", round(xbar, 2), "\\)")), " cm.
          To nasz ", tags$b("estymator punktowy"),
          " \u2014 najlepsze pojedyncze oszacowanie prawdziwej \u015bredniej populacji."),
        p("Ale pojedyncza liczba nie wystarczy. Inna pr\u00f3ba da\u0142aby inn\u0105 \u015bredni\u0105.
          Musimy wyrazi\u0107 ", tags$b("niepewno\u015b\u0107"), " tego oszacowania.")
      ),
      "3" = div(class = "callout-info",
        p(tags$strong("Krok 3: B\u0142\u0105d standardowy (\u00b1 SE)."),
          " B\u0142\u0105d standardowy \u015bredniej to:"),
        p(withMathJax(paste0("\\(SE = \\frac{s}{\\sqrt{n}} = \\frac{", round(s, 2),
                             "}{\\sqrt{", n, "}} = ", round(se, 2), "\\)"))),
        p("SE m\u00f3wi, jak bardzo ", withMathJax("\\(\\bar{x}\\)"),
          " waha si\u0119 z pr\u00f3by na pr\u00f3b\u0119. Zauwa\u017c \u2014 jest ",
          tags$b("znacznie mniejszy"),
          " ni\u017c rozrzut surowych danych! To dlatego, \u017ce \u015brednia z pr\u00f3by \"u\u015brednia\"
          losowe odchylenia poszczeg\u00f3lnych obserwacji."),
        p("Ale ", tags$b("\u00b1 1 SE"), " to tylko oko\u0142o 68% ufno\u015bci.
          \u017beby dosta\u0107 95%, trzeba t\u0119 szeroko\u015b\u0107 ", tags$em("powi\u0119kszy\u0107"),
          " przez warto\u015b\u0107 krytyczn\u0105.")
      ),
      "4" = {
        covers <- (xbar - me <= 170) & (170 <= xbar + me)
        div(class = if (covers) "callout-success" else "callout-danger",
          p(tags$strong("Krok 4: Przedzia\u0142 ufno\u015bci (\u00b1 t* \u00b7 SE).")),
          p("Mno\u017cymy SE przez warto\u015b\u0107 krytyczn\u0105 ",
            withMathJax(paste0("\\(t^*_{0.975, ", n - 1, "} = ",
                               round(t_star, 3), "\\)")), ":"),
          p(withMathJax(paste0("\\(ME = t^* \\cdot SE = ", round(t_star, 3),
                               " \\cdot ", round(se, 2), " = ", round(me, 2), "\\)"))),
          p(tags$b("95% CI: ["),
            round(xbar - me, 2), " ; ", round(xbar + me, 2), tags$b("]")),
          p("Zauwa\u017c, jak niebieski (pe\u0142ny) przedzia\u0142 jest ",
            tags$b("szerszy"), " ni\u017c zielony (\u00b1 SE) \u2014 dok\u0142adnie ",
            round(t_star, 2), "\u00d7 szerszy. To dodatkowa niepewno\u015b\u0107 z tego,
            \u017ce szacujemy \u03c3 z pr\u00f3by (a nie znamy go)."),
          p(tags$em(if (covers) "Ten przedzia\u0142 zawiera prawdziw\u0105 \u015bredni\u0105 populacji (\u03bc = 170 cm)."
                    else "Ten przedzia\u0142 NIE zawiera prawdziwej \u015bredniej populacji (\u03bc = 170 cm) \u2014 klikaj 'Nowa pr\u00f3ba', \u017ceby zobaczy\u0107 jak rzadko to si\u0119 zdarza."))
        )
      }
    )
  })

  # --- Widget 2: Budowa przedzialu dla roznicy srednich ---
  ch3_dstep <- reactiveVal(0)
  ch3_dstep_samples <- reactiveVal(NULL)

  generate_diff_samples <- function() {
    list(
      men   = rnorm(25, mean = 178, sd = 7),
      women = rnorm(25, mean = 165, sd = 6)
    )
  }

  observeEvent(input$ch3_dstep1, {
    if (is.null(ch3_dstep_samples())) ch3_dstep_samples(generate_diff_samples())
    ch3_dstep(1)
  })
  observeEvent(input$ch3_dstep2, {
    if (is.null(ch3_dstep_samples())) ch3_dstep_samples(generate_diff_samples())
    ch3_dstep(2)
  })
  observeEvent(input$ch3_dstep3, {
    if (is.null(ch3_dstep_samples())) ch3_dstep_samples(generate_diff_samples())
    ch3_dstep(3)
  })
  observeEvent(input$ch3_dstep4, {
    if (is.null(ch3_dstep_samples())) ch3_dstep_samples(generate_diff_samples())
    ch3_dstep(4)
  })
  observeEvent(input$ch3_dstep5, {
    if (is.null(ch3_dstep_samples())) ch3_dstep_samples(generate_diff_samples())
    ch3_dstep(5)
  })
  observeEvent(input$ch3_dstep_new_sample, {
    ch3_dstep_samples(generate_diff_samples())
    if (ch3_dstep() == 0) ch3_dstep(1)
  })

  output$ch3_dstep_plot <- renderPlot({
    step <- ch3_dstep()
    samples <- ch3_dstep_samples()

    if (step == 0 || is.null(samples)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Kliknij '1. Dwie pr\u00f3by' aby zacz\u0105\u0107",
                   size = 6, color = "#7f8c8d") +
          theme_void()
      )
    }

    men <- samples$men
    women <- samples$women
    n1 <- length(men); n2 <- length(women)
    x1 <- mean(men); x2 <- mean(women)
    s1 <- sd(men);   s2 <- sd(women)
    diff_val <- x1 - x2
    se <- sqrt(s1^2 / n1 + s2^2 / n2)
    df_w <- (s1^2 / n1 + s2^2 / n2)^2 /
            ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
    t_star <- qt(0.975, df = df_w)
    me <- t_star * se

    col_men <- col_primary      # niebieski
    col_women <- col_secondary  # czerwony

    # ---- GORNY PANEL: dwie grupy na skali wzrostu ----
    xlims_top <- range(c(men, women))
    pad_top <- diff(xlims_top) * 0.06
    xlims_top <- c(xlims_top[1] - pad_top, xlims_top[2] + pad_top)

    set.seed(42)
    jitter_men <- runif(n1, min = 1.55, max = 2.05)
    jitter_women <- runif(n2, min = 0.75, max = 1.25)
    men_df <- data.frame(x = men, y = jitter_men)
    women_df <- data.frame(x = women, y = jitter_women)

    p_top <- ggplot() +
      xlim(xlims_top) +
      ylim(0.35, 2.25) +
      labs(x = "Wzrost (cm)", y = NULL) +
      theme_ci() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Etykiety grup po lewej
    p_top <- p_top +
      annotate("text", x = xlims_top[1], y = 1.8, label = "M\u0119\u017cczy\u017ani",
               hjust = 0, fontface = "bold", size = 4.5, color = col_men) +
      annotate("text", x = xlims_top[1], y = 1.0, label = "Kobiety",
               hjust = 0, fontface = "bold", size = 4.5, color = col_women)

    # Krok 1+: punkty
    if (step >= 1) {
      p_top <- p_top +
        geom_point(data = men_df, aes(x = x, y = y),
                   color = col_men, size = 3, alpha = 0.7) +
        geom_point(data = women_df, aes(x = x, y = y),
                   color = col_women, size = 3, alpha = 0.7)
    }

    # Krok 2+: srednie (diamenty + linie)
    if (step >= 2) {
      p_top <- p_top +
        geom_segment(aes(x = x1, xend = x1, y = 0.4, yend = 2.1),
                     color = col_men, linetype = "dotted", linewidth = 0.8) +
        geom_segment(aes(x = x2, xend = x2, y = 0.4, yend = 2.1),
                     color = col_women, linetype = "dotted", linewidth = 0.8) +
        geom_point(aes(x = x1, y = 1.8), color = col_men,
                   size = 7, shape = 18) +
        geom_point(aes(x = x2, y = 1.0), color = col_women,
                   size = 7, shape = 18) +
        annotate("text", x = x1, y = 2.15,
                 label = paste0("x\u0304\u2081 = ", round(x1, 2)),
                 color = col_men, fontface = "bold", size = 4.5) +
        annotate("text", x = x2, y = 0.55,
                 label = paste0("x\u0304\u2082 = ", round(x2, 2)),
                 color = col_women, fontface = "bold", size = 4.5)
    }

    # Dla krokow 1-2 zwracamy tylko gorny panel
    if (step < 3) {
      return(p_top + ggtitle(paste0("Krok ", step, " z 5")))
    }

    # ---- DOLNY PANEL: roznica w skali wycentrowanej na 0 ----
    # Limity: obejmij 0 i CI z marginesem
    xlims_bot <- range(c(0, diff_val - 1.3 * me, diff_val + 1.3 * me))
    pad_bot <- diff(xlims_bot) * 0.08
    xlims_bot <- c(xlims_bot[1] - pad_bot, xlims_bot[2] + pad_bot)

    p_bot <- ggplot() +
      xlim(xlims_bot) +
      ylim(-0.55, 0.55) +
      labs(x = "R\u00f3\u017cnica \u015brednich (cm)  \u2014  M\u0119\u017cczy\u017ani \u2212 Kobiety",
           y = NULL) +
      theme_ci() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      geom_vline(xintercept = 0, color = col_true,
                 linewidth = 1, linetype = "dashed") +
      annotate("text", x = 0, y = 0.45, label = "0 = brak r\u00f3\u017cnicy",
               color = col_true, fontface = "bold", size = 4, hjust = -0.1)

    # Krok 3+: punkt roznicy
    p_bot <- p_bot +
      geom_point(aes(x = diff_val, y = 0), color = col_estimate,
                 size = 7, shape = 18) +
      annotate("text", x = diff_val, y = -0.22,
               label = paste0("x\u0304\u2081 \u2212 x\u0304\u2082 = ", round(diff_val, 2)),
               color = col_estimate, fontface = "bold", size = 4.5)

    # Krok 4+: waski przedzial SE
    if (step >= 4) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - se, xmax = diff_val + se, y = 0),
                       height = 0.08, color = col_success, linewidth = 1.8) +
        annotate("text", x = diff_val, y = 0.17,
                 label = paste0("\u00b1 SE = \u00b1", round(se, 2)),
                 color = col_success, fontface = "bold", size = 4)
    }

    # Krok 5: pelen CI
    if (step >= 5) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - me, xmax = diff_val + me, y = 0),
                       height = 0.14, color = col_ci, linewidth = 2.2, alpha = 0.6) +
        annotate("text", x = diff_val, y = -0.42,
                 label = paste0("95% CI: [", round(diff_val - me, 2),
                                " ; ", round(diff_val + me, 2), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    }

    # Polacz patchworkiem
    library(patchwork)
    (p_top / p_bot) +
      plot_layout(heights = c(2, 1)) +
      plot_annotation(title = paste0("Krok ", step, " z 5"))
  })

  output$ch3_dstep_explanation <- renderUI({
    step <- ch3_dstep()
    samples <- ch3_dstep_samples()
    if (step == 0 || is.null(samples)) return(NULL)

    men <- samples$men
    women <- samples$women
    n1 <- length(men); n2 <- length(women)
    x1 <- mean(men); x2 <- mean(women)
    s1 <- sd(men);   s2 <- sd(women)
    diff_val <- x1 - x2
    se <- sqrt(s1^2 / n1 + s2^2 / n2)
    df_w <- (s1^2 / n1 + s2^2 / n2)^2 /
            ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
    t_star <- qt(0.975, df = df_w)
    me <- t_star * se

    switch(as.character(step),
      "1" = div(class = "callout-info",
        p(tags$strong("Krok 1: Dwie pr\u00f3by."),
          " Mierzymy wzrost w obu grupach: ", tags$b(n1), " m\u0119\u017cczyzn i ",
          tags$b(n2), " kobiet. Ka\u017cdy punkt to jedna osoba.
          Zauwa\u017c \u2014 rozrzut surowych danych jest du\u017cy, ale wyra\u017anie wida\u0107,
          \u017ce \u015brednia \"niebieska\" le\u017cy na prawo od \u015bredniej \"czerwonej\".")
      ),
      "2" = div(class = "callout-info",
        p(tags$strong("Krok 2: Dwie \u015brednie."),
          " Obliczamy \u015bredni\u0105 w ka\u017cdej grupie:"),
        p(withMathJax(paste0("\\(\\bar{x}_1 = ", round(x1, 2), "\\)"))),
        p(withMathJax(paste0("\\(\\bar{x}_2 = ", round(x2, 2), "\\)"))),
        p("Ka\u017cda \u015brednia ma w\u0142asn\u0105 niepewno\u015b\u0107 \u2014 ale interesuje nas
          nie ka\u017cda z osobna, tylko ", tags$b("r\u00f3\u017cnica mi\u0119dzy nimi"), ".")
      ),
      "3" = div(class = "callout-info",
        p(tags$strong("Krok 3: R\u00f3\u017cnica."),
          " Estymator punktowy r\u00f3\u017cnicy: ",
          withMathJax(paste0("\\(\\bar{x}_1 - \\bar{x}_2 = ", round(x1, 2),
                             " - ", round(x2, 2), " = ",
                             round(diff_val, 2), "\\)")), " cm."),
        p("W dolnym panelu przenosimy si\u0119 do nowej skali \u2014 ", tags$b("skali r\u00f3\u017cnicy"),
          ". Punkt = nasze oszacowanie r\u00f3\u017cnicy. Pionowa linia na 0 oznacza ",
          tags$em("\"gdyby r\u00f3\u017cnicy nie by\u0142o\""),
          ". Teraz musimy otoczy\u0107 nasz\u0105 r\u00f3\u017cnic\u0119 przedzia\u0142em niepewno\u015bci.")
      ),
      "4" = div(class = "callout-info",
        p(tags$strong("Krok 4: B\u0142\u0105d standardowy r\u00f3\u017cnicy (\u00b1 SE)."),
          " SE r\u00f3\u017cnicy \u0142\u0105czy niepewno\u015bci z obu pr\u00f3b:"),
        p(withMathJax(paste0(
          "\\(SE_{r\u00f3\u017cnicy} = \\sqrt{\\frac{s_1^2}{n_1} + \\frac{s_2^2}{n_2}} = ",
          "\\sqrt{\\frac{", round(s1, 2), "^2}{", n1, "} + \\frac{",
          round(s2, 2), "^2}{", n2, "}} = ", round(se, 2), "\\)"))),
        p(tags$b("Wa\u017cne:"), " wariancje si\u0119 ", tags$em("dodaj\u0105"),
          ", nie SE. Dlatego SE r\u00f3\u017cnicy jest ", tags$em("mniejszy"),
          " ni\u017c suma SE poszczeg\u00f3lnych \u015brednich \u2014 to w\u0142a\u015bnie \u017ar\u00f3d\u0142o ",
          tags$b("pu\u0142apki nak\u0142adaj\u0105cych si\u0119 CI"),
          " (patrz case B3 poni\u017cej).")
      ),
      "5" = {
        covers_zero <- (diff_val - me <= 0) & (0 <= diff_val + me)
        div(class = if (covers_zero) "callout-warning" else "callout-success",
          p(tags$strong("Krok 5: Przedzia\u0142 ufno\u015bci dla r\u00f3\u017cnicy.")),
          p("Warto\u015b\u0107 krytyczna z rozk\u0142adu t (df Welcha \u2248 ",
            round(df_w, 1), "): ",
            withMathJax(paste0("\\(t^* = ", round(t_star, 3), "\\)"))),
          p(withMathJax(paste0("\\(ME = t^* \\cdot SE = ", round(t_star, 3),
                               " \\cdot ", round(se, 2), " = ",
                               round(me, 2), "\\)"))),
          p(tags$b("95% CI: ["),
            round(diff_val - me, 2), " ; ", round(diff_val + me, 2),
            tags$b("] cm")),
          p(tags$em(if (covers_zero)
              "CI obejmuje 0 \u2014 nie mo\u017cemy stwierdzi\u0107, \u017ce r\u00f3\u017cnica jest istotna."
            else
              paste0("CI nie obejmuje 0 \u2014 r\u00f3\u017cnica jest istotna. ",
                     "Mo\u017cemy stwierdzi\u0107 z 95% ufno\u015bci\u0105, \u017ce m\u0119\u017cczy\u017ani s\u0105 \u015brednio ",
                     "o co najmniej ", round(diff_val - me, 1),
                     " cm wy\u017csi od kobiet.")))
        )
      }
    )
  })

  # ============================================================================
  # WIDGET 3: CASE STUDIES (konstruktory krok po kroku + hipotezy)
  # ============================================================================

  # ---- Helpery statystyczne ----
  ci_mean <- function(xbar, s, n, conf = 0.95) {
    t_star <- qt(1 - (1 - conf) / 2, df = n - 1)
    me <- t_star * s / sqrt(n)
    list(lower = xbar - me, upper = xbar + me, me = me,
         t_star = t_star, se = s / sqrt(n))
  }
  ci_diff_means <- function(x1, s1, n1, x2, s2, n2, conf = 0.95) {
    se <- sqrt(s1^2 / n1 + s2^2 / n2)
    df_w <- (s1^2 / n1 + s2^2 / n2)^2 /
            ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
    t_star <- qt(1 - (1 - conf) / 2, df = df_w)
    diff <- x1 - x2
    me <- t_star * se
    list(diff = diff, lower = diff - me, upper = diff + me,
         me = me, df = df_w, se = se, t_star = t_star)
  }

  # ---- Werdykt hipotezy ----
  # dir = "gt" (CI > bound), "lt" (CI < bound)
  # Zwraca: "yes" / "no" / "maybe"
  hypothesis_verdict <- function(lower, upper, bound, dir) {
    if (dir == "gt") {
      if (lower > bound)      "yes"
      else if (upper < bound) "no"
      else                    "maybe"
    } else {  # lt
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

  # Kolor obszaru hipotezy (fioletowawy)
  col_hyp <- "#8e44ad"

  # ---- CONFIG case'ow ----
  # Kazdy case: type, data, xlab, steps (labele przyciskow), hypotheses (lista 2)
  # hypotheses: list of list(text, bound, dir, interval_fn)
  # Dla "single_mean" / "diff_means" / "compare_n" / "forest" interval_fn
  # wyciaga (lower, upper) z konfiguracji.

  cases_config <- list(
    A1 = list(
      type = "single_mean",
      data = list(xbar = 173.4, s = 8.2, n = 30),
      xlab = "Wzrost (cm)",
      steps = c("1. Pr\u00f3ba", "2. \u015arednia", "3. \u00b1 SE", "4. Przedzia\u0142"),
      hypotheses = list(
        list(text = "\u015aredni wzrost przekracza 168 cm",
             bound = 168, dir = "gt",
             explain_yes = "Dolna granica CI (\u2248 170.3) le\u017cy powy\u017cej 168. Ca\u0142y CI jest w obszarze hipotezy \u2014 z 95% ufno\u015bci\u0105 \u015bredni wzrost w populacji przekracza 168 cm."),
        list(text = "\u015aredni wzrost przekracza 180 cm",
             bound = 180, dir = "gt",
             explain_no = "G\u00f3rna granica CI (\u2248 176.5) le\u017cy poni\u017cej 180. Ca\u0142y CI jest poza obszarem hipotezy \u2014 nie mo\u017cemy stwierdzi\u0107, \u017ce \u015brednia wzrostu przekracza 180 cm.")
      )
    ),
    A2 = list(
      type = "compare_n",
      data = list(xbar = 32.0, s = 8.0, ns = c(10, 50, 200)),
      xlab = "St\u0119\u017cenie (\u00b5g/m\u00b3)",
      steps = c("1. n = 10", "2. n = 50", "3. n = 200"),
      hypotheses = list(
        list(text = "St\u0119\u017cenie przekracza 25 \u00b5g/m\u00b3",
             bound = 25, dir = "gt",
             explain_yes = "Wszystkie trzy CI le\u017c\u0105 powy\u017cej 25 \u2014 nawet najszerszy (n=10) ma doln\u0105 granic\u0119 \u2248 26.3. Ka\u017cde z badan potwierdza hipotez\u0119. Wi\u0119ksze n daje tylko bardziej precyzyjne oszacowanie, ale wniosek jest ten sam."),
        list(text = "St\u0119\u017cenie przekracza 35 \u00b5g/m\u00b3",
             bound = 35, dir = "gt",
             explain_no = "G\u00f3rna granica CI nawet dla n=200 (\u2248 33.1) le\u017cy poni\u017cej 35 \u2014 nawet najdok\u0142adniejsze badanie nie pozwala stwierdzi\u0107, \u017ce st\u0119\u017cenie przekracza 35. Zauwa\u017c: dla n=10 CI si\u0119ga a\u017c do 37.7 i przecina 35, wi\u0119c tam sytuacja by\u0142aby niepewna \u2014 to pokazuje, dlaczego du\u017cy n jest cenny: daje bardziej definitywn\u0105 odpowied\u017a.")
      )
    ),
    B1 = list(
      type = "diff_means",
      data = list(x1 = 12.3, s1 = 4.5, n1 = 40, x2 = 4.1, s2 = 4.2, n2 = 40,
                  label1 = "Lek", label2 = "Placebo",
                  unit = "mmHg", diff_label = "Lek \u2212 placebo"),
      xlab = "Obni\u017cenie ci\u015bnienia (mmHg)",
      steps = c("1. Pr\u00f3by", "2. \u015arednie", "3. R\u00f3\u017cnica", "4. \u00b1 SE", "5. Przedzia\u0142"),
      hypotheses = list(
        list(text = "Lek skuteczniej obni\u017ca ci\u015bnienie ni\u017c placebo (r\u00f3\u017cnica > 0)",
             bound = 0, dir = "gt",
             explain_yes = "Ca\u0142y CI dla r\u00f3\u017cnicy le\u017cy powy\u017cej 0 \u2014 lek rzeczywi\u015bcie obni\u017ca ci\u015bnienie skuteczniej ni\u017c placebo. To ten sam wniosek co \"r\u00f3\u017cnica istotna statystycznie\"."),
        list(text = "Lek dzia\u0142a o wi\u0119cej ni\u017c 12 mmHg lepiej ni\u017c placebo",
             bound = 12, dir = "gt",
             explain_no = "G\u00f3rna granica CI (\u2248 10.1) le\u017cy poni\u017cej 12. Ca\u0142y CI jest w przedziale 6\u201310 mmHg \u2014 efekt leku jest wyra\u017any, ale nie tak du\u017cy jak g\u0142osi hipoteza.")
      )
    ),
    B2 = list(
      type = "diff_means",
      data = list(x1 = 8.4, s1 = 1.2, n1 = 25, x2 = 8.1, s2 = 1.3, n2 = 25,
                  label1 = "Nawoz X", label2 = "Nawoz Y",
                  unit = "t/ha", diff_label = "X \u2212 Y"),
      xlab = "Plon (t/ha)",
      steps = c("1. Pr\u00f3by", "2. \u015arednie", "3. R\u00f3\u017cnica", "4. \u00b1 SE", "5. Przedzia\u0142"),
      hypotheses = list(
        list(text = "R\u00f3\u017cnica plon\u00f3w jest mniejsza ni\u017c 2 t/ha",
             bound = 2, dir = "lt",
             explain_yes = "Ca\u0142y CI dla r\u00f3\u017cnicy le\u017cy poni\u017cej 2 t/ha. Mo\u017cemy by\u0107 pewni, \u017ce nawet je\u015bli kt\u00f3ry\u015b nawoz jest lepszy, to r\u00f3\u017cnica nie jest du\u017ca (mniej ni\u017c 2 t/ha)."),
        list(text = "Nawoz X daje wi\u0119cej ni\u017c 2 t/ha wi\u0119kszy plon ni\u017c Y",
             bound = 2, dir = "gt",
             explain_no = "G\u00f3rna granica CI (\u2248 1.0) le\u017cy poni\u017cej 2 \u2014 nawet najbardziej optymistyczny scenariusz nie przewiduje tak du\u017cej przewagi X nad Y. Uwaga: to nie znaczy, \u017ce X jest lepszy od Y \u2014 CI obejmuje te\u017c warto\u015bci ujemne, wi\u0119c nie wiemy nawet, kt\u00f3ry nawoz jest lepszy.")
      )
    ),
    B3 = list(
      type = "diff_means",
      data = list(x1 = 350, s1 = 45, n1 = 150, x2 = 362, s2 = 45, n2 = 150,
                  label1 = "Grupa A", label2 = "Grupa B",
                  unit = "ms", diff_label = "A \u2212 B"),
      xlab = "Czas reakcji (ms)",
      steps = c("1. Pr\u00f3by", "2. \u015arednie", "3. R\u00f3\u017cnica", "4. \u00b1 SE", "5. Przedzia\u0142"),
      hypotheses = list(
        list(text = "Grupa A reaguje szybciej ni\u017c B (r\u00f3\u017cnica < 0)",
             bound = 0, dir = "lt",
             explain_yes = "Mimo \u017ce CI ka\u017cdej grupy osobno si\u0119 nak\u0142adaj\u0105 (zobacz g\u00f3rny panel!), CI dla r\u00f3\u017cnicy ca\u0142y le\u017cy poni\u017cej 0. To jest w\u0142a\u015bnie pu\u0142apka nak\u0142adaj\u0105cych si\u0119 CI: SE r\u00f3\u017cnicy jest mniejszy ni\u017c suma SE pojedynczych \u015brednich, dlatego CI dla r\u00f3\u017cnicy bywa w\u0119\u017cszy ni\u017c by sugerowa\u0142y nak\u0142adaj\u0105ce si\u0119 CI grup."),
        list(text = "Grupa A jest szybsza o co najmniej 25 ms",
             bound = -25, dir = "lt",
             explain_no = "Dolna granica CI (\u2248 -22) nie dosi\u0119ga -25 \u2014 ca\u0142y CI jest powy\u017cej tej warto\u015bci. Nie mo\u017cemy stwierdzi\u0107, \u017ce r\u00f3\u017cnica wynosi co najmniej 25 ms. Wiemy tylko, \u017ce r\u00f3\u017cnica jest istotna (A szybsza) i mie\u015bci si\u0119 mi\u0119dzy 2 a 22 ms.")
      )
    ),
    B4 = list(
      type = "diff_means",
      data = list(x1 = 100.4, s1 = 15, n1 = 20000, x2 = 100.0, s2 = 15, n2 = 20000,
                  label1 = "Wojew. A", label2 = "Wojew. B",
                  unit = "pkt IQ", diff_label = "A \u2212 B"),
      xlab = "IQ (punkty)",
      steps = c("1. Pr\u00f3by", "2. \u015arednie", "3. R\u00f3\u017cnica", "4. \u00b1 SE", "5. Przedzia\u0142"),
      hypotheses = list(
        list(text = "Wojew\u00f3dztwo A ma wy\u017csze \u015brednie IQ ni\u017c B (r\u00f3\u017cnica > 0)",
             bound = 0, dir = "gt",
             explain_yes = "Dzi\u0119ki ogromnej pr\u00f3bie (n=20000 w ka\u017cdej grupie) CI jest bardzo w\u0105ski i nie obejmuje 0. Formalnie: r\u00f3\u017cnica jest istotna statystycznie."),
        list(text = "R\u00f3\u017cnica wynosi co najmniej 1 punkt IQ",
             bound = 1, dir = "gt",
             explain_no = "Ca\u0142y CI le\u017cy poni\u017cej 1 (g\u00f3rna granica \u2248 0.7). R\u00f3\u017cnica jest statystycznie istotna, ale rozmiarowo trywialna \u2014 0.4 punktu IQ to ~0.03 SD, nic zauwa\u017calnego w \u017cyciu. To klasyczna ilustracja, \u017ce istotno\u015b\u0107 statystyczna \u2260 wa\u017cno\u015b\u0107 praktyczna.")
      )
    ),
    C1 = list(
      type = "forest",
      data = list(
        groups = c("Tradycyjna", "E-learning", "Flipped class", "Tutoring"),
        means  = c(28.5, 30.2, 31.8, 33.4),
        sds    = c(5.2, 5.8, 5.5, 4.9),
        ns     = c(25, 25, 25, 25)
      ),
      xlab = "\u015aredni wynik egzaminu (0\u201340 pkt)",
      steps = c("1. Punkty", "2. \u015arednie", "3. CI"),
      hypotheses = list(
        list(text = "Tutoring daje \u015bredni wynik > 30 pkt",
             bound = 30, dir = "gt", which = "Tutoring",
             explain_yes = "Dolna granica CI dla Tutoringu (\u2248 31.4) le\u017cy powy\u017cej 30. Ca\u0142y CI w obszarze hipotezy \u2192 TAK."),
        list(text = "Tutoring daje \u015bredni wynik > 36 pkt",
             bound = 36, dir = "gt", which = "Tutoring",
             explain_no = "G\u00f3rna granica CI dla Tutoringu (\u2248 35.4) le\u017cy poni\u017cej 36. Ca\u0142y CI poza obszarem hipotezy \u2192 NIE.")
      )
    ),
    C2 = list(
      type = "forest",
      data = list(
        groups = c("Kardiologia", "Neurologia", "Ortopedia", "Pulmonologia", "SOR"),
        means  = c(22, 28, 25, 31, 75),
        sds    = c(8, 10, 9, 11, 25),
        ns     = c(60, 55, 70, 50, 80)
      ),
      xlab = "\u015aredni czas oczekiwania (min)",
      steps = c("1. Punkty", "2. \u015arednie", "3. CI"),
      hypotheses = list(
        list(text = "SOR ma \u015bredni czas oczekiwania > 60 min",
             bound = 60, dir = "gt", which = "SOR",
             explain_yes = "Dolna granica CI dla SOR (\u2248 69.4) le\u017cy wyra\u017anie powy\u017cej 60. Ca\u0142y CI w obszarze hipotezy. Interwencja na SOR ma sens \u2014 czas oczekiwania jest dramatycznie d\u0142u\u017cszy ni\u017c gdziekolwiek indziej."),
        list(text = "SOR ma \u015bredni czas oczekiwania > 85 min",
             bound = 85, dir = "gt", which = "SOR",
             explain_no = "G\u00f3rna granica CI dla SOR (\u2248 80.6) nie si\u0119ga 85. Ca\u0142y CI poza obszarem hipotezy. Sytuacja jest z\u0142a, ale nie a\u017c tak z\u0142a.")
      )
    )
  )

  # ---- Reactive state per case ----
  # Dla kazdego case'a: aktualny step (0 = nic, 1..n = budowa CI,
  #                     n+1 = hipoteza 1, n+2 = hipoteza 2)
  ch3_case_state <- reactiveValues()
  for (cid in names(cases_config)) {
    ch3_case_state[[cid]] <- 0
  }

  # ---- Helper: narysuj pasek CI dla pojedynczej sredniej ----
  # step: 0 = nic, 1 = punkty, 2 = +srednia, 3 = +SE, 4 = +CI
  # hypothesis: NULL lub list(bound, dir)
  plot_single_mean_step <- function(data, step, xlab,
                                     hypothesis = NULL, title = NULL) {
    xbar <- data$xbar; s <- data$s; n <- data$n
    ci <- ci_mean(xbar, s, n)
    se <- ci$se; me <- ci$me
    t_star <- ci$t_star

    # Generujemy "fake" punkty z parametrow (reproducowalnie)
    set.seed(42)
    samp <- rnorm(n, mean = xbar, sd = s)
    samp <- (samp - mean(samp)) / sd(samp) * s + xbar  # wymus dokladnie xbar, s

    # Limity
    xlims <- range(c(samp, xbar - 1.2 * me, xbar + 1.2 * me))
    if (!is.null(hypothesis)) {
      xlims <- range(c(xlims, hypothesis$bound))
    }
    pad <- diff(xlims) * 0.05
    xlims <- c(xlims[1] - pad, xlims[2] + pad)

    set.seed(7)
    jitter_y <- runif(n, min = 0.15, max = 0.55)
    samp_df <- data.frame(x = samp, y = jitter_y)

    p <- ggplot() +
      xlim(xlims) +
      ylim(-0.55, 0.75) +
      labs(x = xlab, y = NULL, title = title) +
      theme_ci() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Obszar hipotezy (pod spodem wszystkiego)
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
        annotate("text", x = hypothesis$bound, y = 0.68,
                 label = paste0(if (hypothesis$dir == "gt") "\u2265 " else "\u2264 ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    }

    if (step >= 1) {
      p <- p + geom_point(data = samp_df, aes(x = x, y = y),
                          color = col_primary, size = 3, alpha = 0.7)
    }
    if (step >= 2) {
      p <- p +
        geom_vline(xintercept = xbar, color = col_estimate,
                   linewidth = 1, linetype = "dotted") +
        geom_point(aes(x = xbar, y = 0), color = col_estimate,
                   size = 7, shape = 18) +
        annotate("text", x = xbar, y = -0.18,
                 label = paste0("x\u0304 = ", round(xbar, 2)),
                 color = col_estimate, fontface = "bold", size = 5)
    }
    if (step >= 3) {
      p <- p +
        geom_errorbarh(aes(xmin = xbar - se, xmax = xbar + se, y = 0),
                       height = 0.06, color = col_success, linewidth = 1.8) +
        annotate("text", x = xbar, y = 0.14,
                 label = paste0("\u00b1 SE = \u00b1", round(se, 2)),
                 color = col_success, fontface = "bold", size = 4)
    }
    if (step >= 4) {
      p <- p +
        geom_errorbarh(aes(xmin = xbar - me, xmax = xbar + me, y = 0),
                       height = 0.12, color = col_ci, linewidth = 2.2,
                       alpha = 0.6) +
        annotate("text", x = xbar, y = -0.38,
                 label = paste0("95% CI: [", round(xbar - me, 2),
                                " ; ", round(xbar + me, 2), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    }

    p
  }

  # ---- Plot dla compare_n ----
  plot_compare_n_step <- function(data, step, xlab, hypothesis = NULL) {
    xbar <- data$xbar; s <- data$s; ns <- data$ns

    # Kazdy step = jeden dodatkowy CI
    ci_list <- lapply(ns, function(n) {
      ci <- ci_mean(xbar, s, n)
      list(n = n, lower = ci$lower, upper = ci$upper, me = ci$me)
    })

    visible_k <- step  # ile CI pokazujemy
    if (visible_k < 1) visible_k <- 0
    if (visible_k > length(ns)) visible_k <- length(ns)

    all_lowers <- sapply(ci_list, function(c) c$lower)
    all_uppers <- sapply(ci_list, function(c) c$upper)
    xlims <- c(min(all_lowers), max(all_uppers))
    if (!is.null(hypothesis)) {
      xlims <- range(c(xlims, hypothesis$bound))
    }
    pad <- diff(xlims) * 0.1
    xlims <- c(xlims[1] - pad, xlims[2] + pad)

    y_positions <- seq_along(ns)

    p <- ggplot() +
      xlim(xlims) +
      ylim(0.3, length(ns) + 0.7) +
      labs(x = xlab, y = NULL) +
      theme_ci() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

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
        annotate("text", x = hypothesis$bound, y = length(ns) + 0.5,
                 label = paste0(if (hypothesis$dir == "gt") "\u2265 " else "\u2264 ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    }

    if (visible_k >= 1) {
      rows_df <- data.frame(
        y = sapply(seq_len(visible_k), function(i) y_positions[i]),
        lower = sapply(seq_len(visible_k), function(i) ci_list[[i]]$lower),
        upper = sapply(seq_len(visible_k), function(i) ci_list[[i]]$upper),
        n = sapply(seq_len(visible_k), function(i) ci_list[[i]]$n),
        xbar_val = xbar
      )
      p <- p +
        geom_errorbarh(data = rows_df,
                       aes(xmin = lower, xmax = upper, y = y),
                       height = 0.12, color = col_ci, linewidth = 1.8) +
        geom_point(data = rows_df,
                   aes(x = xbar_val, y = y),
                   color = col_estimate, size = 5, shape = 18)
      # Labelki n i granic CI dodajemy przez annotate (jeden po drugim)
      for (i in seq_len(visible_k)) {
        ci <- ci_list[[i]]
        y <- y_positions[i]
        p <- p +
          annotate("text", x = xlims[1], y = y,
                   label = paste0("n = ", ci$n),
                   hjust = 0, fontface = "bold", size = 4.5,
                   color = col_dark) +
          annotate("text", x = ci$upper, y = y + 0.22,
                   label = paste0("[", round(ci$lower, 2), " ; ",
                                  round(ci$upper, 2), "]"),
                   hjust = 1, size = 3.8, color = col_ci,
                   fontface = "bold")
      }
    }

    p
  }

  # ---- Plot dla diff_means ----
  # step 1=proby, 2=srednie, 3=roznica, 4=+SE, 5=+CI
  plot_diff_means_step <- function(data, step, xlab, hypothesis = NULL) {
    x1 <- data$x1; s1 <- data$s1; n1 <- data$n1
    x2 <- data$x2; s2 <- data$s2; n2 <- data$n2

    cid <- ci_diff_means(x1, s1, n1, x2, s2, n2)
    diff_val <- cid$diff
    se <- cid$se; me <- cid$me

    col_g1 <- col_primary
    col_g2 <- col_secondary

    # Generuj reprezentatywne probki z parametrow
    set.seed(11)
    samp1 <- rnorm(n1, mean = x1, sd = s1)
    samp1 <- (samp1 - mean(samp1)) / sd(samp1) * s1 + x1
    set.seed(17)
    samp2 <- rnorm(n2, mean = x2, sd = s2)
    samp2 <- (samp2 - mean(samp2)) / sd(samp2) * s2 + x2

    # Gdy n > 80, pokazujemy losowa podproke (dla czytelnosci)
    max_show <- 80
    if (n1 > max_show) samp1 <- sample(samp1, max_show)
    if (n2 > max_show) samp2 <- sample(samp2, max_show)

    # ---- GORNY PANEL ----
    xlims_top <- range(c(samp1, samp2))
    pad_t <- diff(xlims_top) * 0.06
    xlims_top <- c(xlims_top[1] - pad_t, xlims_top[2] + pad_t)

    set.seed(42)
    jit1 <- runif(length(samp1), 1.55, 2.05)
    set.seed(43)
    jit2 <- runif(length(samp2), 0.75, 1.25)

    p_top <- ggplot() +
      xlim(xlims_top) +
      ylim(0.35, 2.25) +
      labs(x = xlab, y = NULL) +
      theme_ci() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      annotate("text", x = xlims_top[1], y = 1.8, label = data$label1,
               hjust = 0, fontface = "bold", size = 4.5, color = col_g1) +
      annotate("text", x = xlims_top[1], y = 1.0, label = data$label2,
               hjust = 0, fontface = "bold", size = 4.5, color = col_g2)

    if (step >= 1) {
      p_top <- p_top +
        geom_point(data = data.frame(x = samp1, y = jit1),
                   aes(x = x, y = y), color = col_g1, size = 3, alpha = 0.7) +
        geom_point(data = data.frame(x = samp2, y = jit2),
                   aes(x = x, y = y), color = col_g2, size = 3, alpha = 0.7)
    }
    if (step >= 2) {
      p_top <- p_top +
        geom_segment(aes(x = x1, xend = x1, y = 0.4, yend = 2.1),
                     color = col_g1, linetype = "dotted", linewidth = 0.8) +
        geom_segment(aes(x = x2, xend = x2, y = 0.4, yend = 2.1),
                     color = col_g2, linetype = "dotted", linewidth = 0.8) +
        geom_point(aes(x = x1, y = 1.8), color = col_g1,
                   size = 7, shape = 18) +
        geom_point(aes(x = x2, y = 1.0), color = col_g2,
                   size = 7, shape = 18) +
        annotate("text", x = x1, y = 2.15,
                 label = paste0("x\u0304\u2081 = ", round(x1, 2)),
                 color = col_g1, fontface = "bold", size = 4.5) +
        annotate("text", x = x2, y = 0.55,
                 label = paste0("x\u0304\u2082 = ", round(x2, 2)),
                 color = col_g2, fontface = "bold", size = 4.5)
    }

    if (step < 3) {
      return(p_top)
    }

    # ---- DOLNY PANEL ----
    xlims_bot <- range(c(0, diff_val - 1.3 * me, diff_val + 1.3 * me))
    if (!is.null(hypothesis)) {
      xlims_bot <- range(c(xlims_bot, hypothesis$bound))
    }
    pad_b <- diff(xlims_bot) * 0.1
    xlims_bot <- c(xlims_bot[1] - pad_b, xlims_bot[2] + pad_b)

    p_bot <- ggplot() +
      xlim(xlims_bot) +
      ylim(-0.55, 0.65) +
      labs(x = paste0("R\u00f3\u017cnica (", data$unit, ")  \u2014  ", data$diff_label),
           y = NULL) +
      theme_ci() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

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
        annotate("text", x = hypothesis$bound, y = 0.55,
                 label = paste0(if (hypothesis$dir == "gt") "\u2265 " else "\u2264 ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    } else {
      # linia zero gdy brak hipotezy
      p_bot <- p_bot +
        geom_vline(xintercept = 0, color = col_true,
                   linewidth = 1, linetype = "dashed") +
        annotate("text", x = 0, y = 0.55, label = "0 = brak r\u00f3\u017cnicy",
                 color = col_true, fontface = "bold", size = 4, hjust = -0.1)
    }

    p_bot <- p_bot +
      geom_point(aes(x = diff_val, y = 0), color = col_estimate,
                 size = 7, shape = 18) +
      annotate("text", x = diff_val, y = -0.22,
               label = paste0("x\u0304\u2081 \u2212 x\u0304\u2082 = ", round(diff_val, 2)),
               color = col_estimate, fontface = "bold", size = 4.5)

    if (step >= 4) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - se, xmax = diff_val + se, y = 0),
                       height = 0.08, color = col_success, linewidth = 1.8) +
        annotate("text", x = diff_val, y = 0.17,
                 label = paste0("\u00b1 SE = \u00b1", round(se, 2)),
                 color = col_success, fontface = "bold", size = 4)
    }
    if (step >= 5) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - me, xmax = diff_val + me, y = 0),
                       height = 0.14, color = col_ci, linewidth = 2.2,
                       alpha = 0.6) +
        annotate("text", x = diff_val, y = -0.42,
                 label = paste0("95% CI: [", round(diff_val - me, 2),
                                " ; ", round(diff_val + me, 2), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    }

    library(patchwork)
    (p_top / p_bot) + plot_layout(heights = c(2, 1))
  }

  # ---- Plot dla forest (wiele grup) ----
  plot_forest_step <- function(data, step, xlab, hypothesis = NULL) {
    groups <- data$groups
    means <- data$means
    sds <- data$sds
    ns <- data$ns
    k <- length(groups)

    ci_list <- lapply(seq_len(k), function(i) {
      ci <- ci_mean(means[i], sds[i], ns[i])
      list(lower = ci$lower, upper = ci$upper)
    })
    all_lowers <- sapply(ci_list, function(c) c$lower)
    all_uppers <- sapply(ci_list, function(c) c$upper)

    # Limity
    xlims <- range(c(all_lowers, all_uppers))
    if (!is.null(hypothesis)) {
      xlims <- range(c(xlims, hypothesis$bound))
    }
    pad <- diff(xlims) * 0.12
    xlims <- c(xlims[1] - pad, xlims[2] + pad)

    # Wygeneruj fake punkty dla kazdej grupy
    points_df <- do.call(rbind, lapply(seq_len(k), function(i) {
      set.seed(50 + i)
      samp <- rnorm(ns[i], mean = means[i], sd = sds[i])
      samp <- (samp - mean(samp)) / sd(samp) * sds[i] + means[i]
      if (ns[i] > 60) samp <- sample(samp, 60)
      set.seed(100 + i)
      jit <- runif(length(samp), min = i - 0.25, max = i + 0.25)
      data.frame(x = samp, y = jit, group = groups[i])
    }))

    y_positions <- seq_len(k)
    group_df <- data.frame(group = groups, y = y_positions,
                            mean = means, lower = all_lowers, upper = all_uppers)

    p <- ggplot() +
      xlim(xlims) +
      ylim(0.3, k + 0.7) +
      labs(x = xlab, y = NULL) +
      theme_ci() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Etykiety grup
    p <- p +
      annotate("text", x = xlims[1], y = y_positions,
               label = groups, hjust = 0, fontface = "bold", size = 4.5,
               color = col_dark)

    # Obszar hipotezy
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

    # Krok 1+: punkty
    if (step >= 1) {
      p <- p + geom_point(data = points_df, aes(x = x, y = y),
                          color = col_primary, size = 2.3, alpha = 0.55)
    }
    # Krok 2+: srednie
    if (step >= 2) {
      p <- p + geom_point(data = group_df, aes(x = mean, y = y),
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

  # ---- Generator przyciskow dla case'a ----
  case_buttons_ui <- function(case_id) {
    cfg <- cases_config[[case_id]]
    current <- ch3_case_state[[case_id]]
    n_core <- n_core_steps(cfg)

    # Przyciski budowy CI
    core_btns <- lapply(seq_along(cfg$steps), function(i) {
      btn_class <- if (current == i) "btn-primary" else "btn-outline-primary"
      actionButton(paste0("ch3_", case_id, "_step", i),
                   cfg$steps[i], class = btn_class)
    })

    # Przyciski hipotez (pojawiaja sie dopiero po wybudowaniu CI)
    hyp_btns <- if (current >= n_core) {
      lapply(seq_along(cfg$hypotheses), function(j) {
        btn_class <- if (current == n_core + j) "btn-warning" else "btn-outline-warning"
        actionButton(paste0("ch3_", case_id, "_hyp", j),
                     paste0("Hipoteza ", j), class = btn_class)
      })
    } else {
      list(helpText("Wybuduj pe\u0142ny przedzia\u0142, \u017ceby sprawdzi\u0107 hipotezy."))
    }

    tagList(
      div(class = "step-buttons", core_btns),
      div(class = "step-buttons", style = "margin-top: 4px;", hyp_btns)
    )
  }

  # ---- Glowny render: plot + explanation dla case'a ----
  render_case_plot <- function(case_id) {
    cfg <- cases_config[[case_id]]
    step <- ch3_case_state[[case_id]]
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

    # Czy jeste\u015bmy w fazie hipotezy?
    hypothesis <- NULL
    plot_step <- step
    if (step > n_core) {
      hyp_idx <- step - n_core
      hypothesis <- cfg$hypotheses[[hyp_idx]]
      plot_step <- n_core  # pe\u0142en CI w tle
    }

    switch(cfg$type,
      "single_mean" = plot_single_mean_step(cfg$data, plot_step, cfg$xlab,
                                             hypothesis = hypothesis),
      "compare_n"   = plot_compare_n_step(cfg$data, plot_step, cfg$xlab,
                                           hypothesis = hypothesis),
      "diff_means"  = plot_diff_means_step(cfg$data, plot_step, cfg$xlab,
                                            hypothesis = hypothesis),
      "forest"      = plot_forest_step(cfg$data, plot_step, cfg$xlab,
                                        hypothesis = hypothesis)
    )
  }

  # ---- Render: explanation ----
  render_case_explain <- function(case_id) {
    cfg <- cases_config[[case_id]]
    step <- ch3_case_state[[case_id]]
    n_core <- n_core_steps(cfg)

    if (step == 0) return(NULL)

    # Faza hipotezy
    if (step > n_core) {
      hyp_idx <- step - n_core
      hyp <- cfg$hypotheses[[hyp_idx]]

      # Oblicz werdykt
      verdict <- compute_verdict_for_case(cfg, hyp)

      cls <- verdict_class(verdict)
      label <- verdict_label(verdict)

      # Buduj tresc wyjasnienia
      body <- if (verdict == "yes" && !is.null(hyp$explain_yes)) {
        p(hyp$explain_yes)
      } else if (verdict == "no" && !is.null(hyp$explain_no)) {
        p(hyp$explain_no)
      } else {
        p("CI przecina granic\u0119 hipotezy \u2014 nie mo\u017cemy jednoznacznie
          stwierdzi\u0107, czy jest prawdziwa.")
      }

      return(div(class = cls,
        p(tags$strong("Hipoteza ", hyp_idx, ": "), hyp$text),
        p(tags$strong("Werdykt: ", label)),
        body
      ))
    }

    # Faza budowy CI \u2014 wyja\u015bnienie ostatniego kroku
    # Dla uproszczenia: callout-info z kr\u00f3tkim opisem
    div(class = "callout-info",
      p(tags$strong(cfg$steps[step])),
      p("Krok ", step, " z ", n_core, ".")
    )
  }

  # ---- Werdykt dla case'a ----
  compute_verdict_for_case <- function(cfg, hyp) {
    switch(cfg$type,
      "single_mean" = {
        ci <- ci_mean(cfg$data$xbar, cfg$data$s, cfg$data$n)
        hypothesis_verdict(ci$lower, ci$upper, hyp$bound, hyp$dir)
      },
      "compare_n" = {
        # Werdykt bazujemy na najwazszym (najwiekszym n) CI
        # (= najbardziej precyzyjnym oszacowaniu)
        largest_n <- max(cfg$data$ns)
        ci <- ci_mean(cfg$data$xbar, cfg$data$s, largest_n)
        hypothesis_verdict(ci$lower, ci$upper, hyp$bound, hyp$dir)
      },
      "diff_means" = {
        cid <- ci_diff_means(cfg$data$x1, cfg$data$s1, cfg$data$n1,
                              cfg$data$x2, cfg$data$s2, cfg$data$n2)
        hypothesis_verdict(cid$lower, cid$upper, hyp$bound, hyp$dir)
      },
      "forest" = {
        # Znajdz odpowiednia grupe
        idx <- which(cfg$data$groups == hyp$which)
        ci <- ci_mean(cfg$data$means[idx], cfg$data$sds[idx], cfg$data$ns[idx])
        hypothesis_verdict(ci$lower, ci$upper, hyp$bound, hyp$dir)
      }
    )
  }

  # ---- Podlaczenie observerow + outputow dla kazdego case'a ----
  for (cid in names(cases_config)) {
    local({
      case_id <- cid
      cfg <- cases_config[[case_id]]
      n_core <- length(cfg$steps)

      # Przyciski core step\u00f3w
      for (i in seq_along(cfg$steps)) {
        local({
          step_i <- i
          observeEvent(input[[paste0("ch3_", case_id, "_step", step_i)]], {
            ch3_case_state[[case_id]] <- step_i
          }, ignoreInit = TRUE)
        })
      }

      # Przyciski hipotez
      for (j in seq_along(cfg$hypotheses)) {
        local({
          hyp_j <- j
          observeEvent(input[[paste0("ch3_", case_id, "_hyp", hyp_j)]], {
            ch3_case_state[[case_id]] <- n_core + hyp_j
          }, ignoreInit = TRUE)
        })
      }

      # Rendery
      output[[paste0("ch3_", case_id, "_buttons")]] <- renderUI({
        ch3_case_state[[case_id]]  # reaktywna zaleznosc
        case_buttons_ui(case_id)
      })
      output[[paste0("ch3_", case_id, "_plot")]] <- renderPlot({
        render_case_plot(case_id)
      })
      output[[paste0("ch3_", case_id, "_explain")]] <- renderUI({
        render_case_explain(case_id)
      })
    })
  }

}
