# ============================================================================
# CHAPTER 6: Dwie zmienne jakosciowe (chi-kwadrat, Fisher)
# ============================================================================

ch5_ui <- tabPanel("6. Dwie zmienne jako\u015bciowe",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Badali\u015bmy korelacj\u0119 mi\u0119dzy zmiennymi ilo\u015bciowymi.
       A co, gdy obie zmienne s\u0105 jako\u015bciowe (kategorialne)?"
    ),

    # ========================================================================
    # Wprowadzenie
    # ========================================================================
    div(class = "section-title", "Tabela kontyngencji i test \u03c7\u00b2"),

    div(class = "narrative",
      p("Gdy mamy dwie zmienne jako\u015bciowe, pytamy: ",
        tags$b("czy s\u0105 ze sob\u0105 powi\u0105zane?"),
        " Narz\u0119dzie: tabela kontyngencji (krzy\u017cowa) + test \u03c7\u00b2 niezale\u017cno\u015bci."),
      p("Idea: por\u00f3wnujemy to, co ", tags$b("zaobserwowali\u015bmy"),
        " z tym, czego ", tags$b("oczekiwaliby\u015bmy, gdyby zmienne by\u0142y niezale\u017cne"), "."),
      div(class = "formula-box",
        p(withMathJax("\\(H_0:\\) zmienne s\u0105 niezale\u017cne \\(\\quad\\text{vs}\\quad H_a:\\) zmienne s\u0105 powi\u0105zane")),
        p("Liczno\u015bci oczekiwane: ", withMathJax("\\(E_{ij} = \\frac{n_{i\\cdot} \\cdot n_{\\cdot j}}{n}\\)")),
        p("Statystyka testowa: ", withMathJax("\\(\\chi^2 = \\sum \\frac{(O_{ij} - E_{ij})^2}{E_{ij}}\\)"))
      )
    ),

    # ========================================================================
    # WIDGET 0: Budowanie intuicji — co to znaczy niezaleznosc?
    # ========================================================================
    div(class = "section-title", "Budowanie intuicji: co to znaczy \u201eniezale\u017cno\u015b\u0107\u201d?"),

    div(class = "narrative",
      p("Zanim przejdziemy do wzor\u00f3w, zbudujmy intuicj\u0119 na przyk\u0142adzie:")
    ),

    div(class = "widget-block",
      h4("Przyk\u0142ad: czy p\u0142e\u0107 wp\u0142ywa na dostawanie mandat\u00f3w?"),

      div(class = "narrative",
        p("Mamy dane z 200 kontroli drogowych. Pytanie: ",
          tags$em("\u201eCzy szansa dostania mandatu jest niezale\u017cna od p\u0142ci?\u201d"))
      ),

      actionButton("ch5_narr_step1", "1. Poka\u017c dane",
                   class = "btn-outline-primary", width = "100%"),
      uiOutput("ch5_narr1"),
      br(),

      conditionalPanel(
        condition = "input.ch5_narr_step1 % 2 == 1",
        actionButton("ch5_narr_step2", "2. Za\u0142\u00f3\u017cmy niezale\u017cno\u015b\u0107 \u2014 co by by\u0142o?",
                     class = "btn-outline-primary", width = "100%"),
        uiOutput("ch5_narr2"),
        br(),

        conditionalPanel(
          condition = "input.ch5_narr_step2 % 2 == 1",
          actionButton("ch5_narr_step3", "3. Por\u00f3wnaj: obserwowane vs oczekiwane",
                       class = "btn-outline-primary", width = "100%"),
          uiOutput("ch5_narr3")
        )
      )
    ),

    # ========================================================================
    # WIDGET 1: Chi-kwadrat krokowy
    # ========================================================================
    div(class = "section-title", "Test \u03c7\u00b2 niezale\u017cno\u015bci \u2014 krok po kroku"),

    div(class = "widget-block",
      fluidRow(
        column(4,
          selectInput("ch5_scenario", "Scenariusz:",
            choices = c(
              "Opakowanie vs ple\u015b\u0144 (T\u017b)" = "packaging",
              "Strefa miasta vs ogrzewanie (GP)" = "heating",
              "Typ gleby vs kategoria plonu (R)" = "soil",
              "Zabezpieczenie vs wypadek (IB)" = "safety"
            ),
            selected = "packaging"
          ),
          sliderInput("ch5_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 50, max = 300, value = 120, step = 10),
          actionButton("ch5_new_sample", "Losuj pr\u00f3b\u0119",
                       class = "btn-primary", width = "100%"),
          hr(),
          h5("Kroki testu:"),
          div(style = "display: flex; flex-direction: column; gap: 6px;",
            actionButton("ch5_step1", "1. Tabela obserwowana",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch5_step2", "2. Procenty \u2014 co widzimy?",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch5_step3", "3. Tabela oczekiwana + \u03c7\u00b2",
                         class = "btn-outline-primary", width = "100%"),
            actionButton("ch5_step4", "4. p-warto\u015b\u0107 i decyzja",
                         class = "btn-outline-primary", width = "100%")
          )
        ),
        column(8,
          uiOutput("ch5_hypothesis_panel"),
          plotOutput("ch5_step_plot", height = "350px"),
          uiOutput("ch5_step_info")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Chi-kwadrat vs Fisher (porownanie)
    # ========================================================================
    div(class = "section-title", "Test \u03c7\u00b2 vs test Fishera"),

    div(class = "narrative",
      p("Test \u03c7\u00b2 opiera si\u0119 na przybli\u017ceniu. Gdy pr\u00f3ba jest ma\u0142a,
        niekt\u00f3re oczekiwane liczno\u015bci mog\u0105 by\u0107 < 5 \u2014 wtedy przybli\u017cenie zawodzi."),
      p("Alternatywa: ", tags$b("test dok\u0142adny Fishera"),
        " \u2014 liczy p-warto\u015b\u0107 dok\u0142adnie, jak test dwumianowy dla proporcji.")
    ),

    div(class = "widget-block",
      h4("Por\u00f3wnanie wynik\u00f3w"),
      actionButton("ch5_compare", "Por\u00f3wnaj \u03c7\u00b2 vs Fisher (na tych samych danych)",
                   class = "btn-primary", width = "100%"),
      br(), br(),
      uiOutput("ch5_compare_result")
    ),

    div(class = "callout-info",
      tags$strong("Kiedy kt\u00f3ry?"),
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th(""), tags$th("Test \u03c7\u00b2"), tags$th("Test Fishera"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$b("Metoda")),
            tags$td("Przybli\u017cony (rozk\u0142ad \u03c7\u00b2)"),
            tags$td("Dok\u0142adny (kombinatoryka)")
          ),
          tags$tr(
            tags$td(tags$b("Warunek")),
            tags$td("Wszystkie E\u2080 \u2265 5"),
            tags$td("Dzia\u0142a zawsze")
          ),
          tags$tr(
            tags$td(tags$b("Du\u017ce n")),
            tags$td(style = "background: #eafaf1;", "Szybki, praktycznie identyczny wynik"),
            tags$td("Dzia\u0142a, ale wolniejszy")
          ),
          tags$tr(
            tags$td(tags$b("Ma\u0142e n")),
            tags$td(style = "background: #fdedec;", "Mo\u017ce by\u0107 niedok\u0142adny"),
            tags$td(style = "background: #eafaf1;", "Bezpieczny wyb\u00f3r")
          ),
          tags$tr(
            tags$td(tags$b("W Jamovi")),
            tags$td("\u03c7\u00b2 (domy\u015blnie)"),
            tags$td("Zaznacz: Fisher's exact test")
          )
        )
      )
    ),

    # ========================================================================
    # Jak interpretowac sile zwiazku
    # ========================================================================
    div(class = "section-title", "Jak du\u017ca jest r\u00f3\u017cnica? Si\u0142a zwi\u0105zku"),

    div(class = "narrative",
      p("P-warto\u015b\u0107 m\u00f3wi ", tags$em("czy"), " zwi\u0105zek istnieje, ale nie ",
        tags$em("jak du\u017cy"), " jest. Zobaczmy to na naszych danych:")
    ),

    div(class = "widget-block",
      h4("Si\u0142a zwi\u0105zku \u2014 na naszych danych"),
      actionButton("ch5_effect", "Poka\u017c si\u0142\u0119 zwi\u0105zku",
                   class = "btn-primary", width = "100%"),
      br(), br(),
      uiOutput("ch5_effect_result")
    ),

    div(class = "callout-info",
      p(tags$b("Jak czyta\u0107 si\u0142\u0119 zwi\u0105zku:")),
      p("1. ", tags$b("Procenty w grupach"), " \u2014 najlepsza intuicja.
        Je\u015bli odsetek to 45% vs 47% \u2014 nawet przy p < 0.05 r\u00f3\u017cnica jest praktycznie \u017cadna.
        Je\u015bli 30% vs 70% \u2014 efekt jest ogromny."),
      p("2. ", tags$b("Cram\u00e9r's V"), " \u2014 wsp\u00f3\u0142czynnik si\u0142y zwi\u0105zku [0\u20131]:"),
      div(class = "formula-box",
        p(withMathJax("\\(V = \\sqrt{\\frac{\\chi^2}{n \\cdot (k - 1)}}\\)"),
          " gdzie k = min(wiersze, kolumny)")
      ),
      p("Interpretacja: < 0.1 pomijalny, 0.1\u20130.3 ma\u0142y, 0.3\u20130.5 \u015bredni, > 0.5 du\u017cy."),
      p("Zawsze ", tags$b("zacznij od procent\u00f3w"),
        " \u2014 to j\u0119zyk zrozumia\u0142y dla ka\u017cdego odbiorcy.")
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: por\u00f3wnywanie dw\u00f3ch grup (zmienna ilo\u015bciowa vs jako\u015bciowa)"),
      actionButton("ch5_next", "Dalej \u2192 7. Ilo\u015bciowa vs jako\u015bciowa",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch5_server <- function(input, output, session) {

  # --- Parametry scenariuszy ---
  scenario_params <- list(
    packaging = list(
      lab1 = "Opakowanie", lab2 = "Ple\u015b\u0144",
      cats1 = c("Szk\u0142o", "Plastik", "Karton"),
      cats2 = c("Tak", "Nie"),
      probs = matrix(c(0.05, 0.95, 0.12, 0.88, 0.20, 0.80), nrow = 3, byrow = TRUE),
      question = "Czy typ opakowania wp\u0142ywa na wyst\u0119powanie ple\u015bni?",
      h0_text = "\\(H_0:\\) typ opakowania i wyst\u0119powanie ple\u015bni s\u0105 niezale\u017cne",
      h1_text = "\\(H_a:\\) typ opakowania i wyst\u0119powanie ple\u015bni s\u0105 powi\u0105zane"),
    heating = list(
      lab1 = "Strefa", lab2 = "Ogrzewanie",
      cats1 = c("Centrum", "Przedmie\u015bcia", "Obrze\u017ca"),
      cats2 = c("Gaz", "W\u0119giel", "Pompa ciep\u0142a"),
      probs = matrix(c(0.50, 0.10, 0.40,
                        0.30, 0.35, 0.35,
                        0.15, 0.55, 0.30), nrow = 3, byrow = TRUE),
      question = "Czy spos\u00f3b ogrzewania zale\u017cy od strefy miasta?",
      h0_text = "\\(H_0:\\) strefa i spos\u00f3b ogrzewania s\u0105 niezale\u017cne",
      h1_text = "\\(H_a:\\) strefa i spos\u00f3b ogrzewania s\u0105 powi\u0105zane"),
    soil = list(
      lab1 = "Typ gleby", lab2 = "Plon",
      cats1 = c("Piaszczysta", "Gliniasta", "Czarnoziemna"),
      cats2 = c("Niski", "Wysoki"),
      probs = matrix(c(0.65, 0.35, 0.45, 0.55, 0.25, 0.75), nrow = 3, byrow = TRUE),
      question = "Czy typ gleby wp\u0142ywa na kategori\u0119 plonu?",
      h0_text = "\\(H_0:\\) typ gleby i kategoria plonu s\u0105 niezale\u017cne",
      h1_text = "\\(H_a:\\) typ gleby i kategoria plonu s\u0105 powi\u0105zane"),
    safety = list(
      lab1 = "Zabezpieczenie", lab2 = "Wypadek",
      cats1 = c("Kask", "Szelki", "Brak"),
      cats2 = c("Tak", "Nie"),
      probs = matrix(c(0.05, 0.95, 0.08, 0.92, 0.22, 0.78), nrow = 3, byrow = TRUE),
      question = "Czy rodzaj zabezpieczenia wp\u0142ywa na wyst\u0119powanie wypadk\u00f3w?",
      h0_text = "\\(H_0:\\) zabezpieczenie i wypadki s\u0105 niezale\u017cne",
      h1_text = "\\(H_a:\\) zabezpieczenie i wypadki s\u0105 powi\u0105zane")
  )

  # --- Wspoldzielone dane ---
  ch5_tab <- reactiveVal(NULL)
  ch5_step <- reactiveVal(0)

  observeEvent(input$ch5_new_sample, {
    par <- scenario_params[[input$ch5_scenario]]
    n <- input$ch5_n
    n_per_cat1 <- rmultinom(1, n, rep(1, length(par$cats1)))

    rows <- list()
    for (i in seq_along(par$cats1)) {
      cats2_draws <- sample(par$cats2, n_per_cat1[i], replace = TRUE, prob = par$probs[i, ])
      rows[[i]] <- data.frame(var1 = par$cats1[i], var2 = cats2_draws)
    }
    df <- do.call(rbind, rows)
    df$var1 <- factor(df$var1, levels = par$cats1)
    df$var2 <- factor(df$var2, levels = par$cats2)

    ch5_tab(table(df$var1, df$var2))
    ch5_step(0)
  })

  # --- Widget 0: Narracja niezaleznosci (mandaty) ---
  # Stale dane do narracji (nie losowane)
  narr_tab <- matrix(c(30, 70, 50, 50), nrow = 2, byrow = TRUE,
    dimnames = list(c("Kobiety", "M\u0119\u017cczy\u017ani"),
                    c("Mandat", "Brak mandatu")))

  output$ch5_narr1 <- renderUI({
    req(input$ch5_narr_step1 %% 2 == 1)

    div(class = "callout-info", style = "margin-top: 10px;",
      p(tags$b("Dane z 200 kontroli:")),
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(tags$tr(tags$th(""), tags$th("Mandat"), tags$th("Brak mandatu"), tags$th("Razem"))),
        tags$tbody(
          tags$tr(tags$td(tags$b("Kobiety")), tags$td("30"), tags$td("70"), tags$td("100")),
          tags$tr(tags$td(tags$b("M\u0119\u017cczy\u017ani")), tags$td("50"), tags$td("50"), tags$td("100")),
          tags$tr(tags$td(tags$b("Razem")), tags$td("80"), tags$td("120"), tags$td("200"))
        )
      ),
      p("Kobiety: 30% dosta\u0142o mandat. M\u0119\u017cczy\u017ani: 50%. Wygl\u0105da na r\u00f3\u017cnic\u0119.
        Ale czy to mo\u017ce by\u0107 przypadek?")
    )
  })

  output$ch5_narr2 <- renderUI({
    req(input$ch5_narr_step2 %% 2 == 1)

    div(class = "callout-warning", style = "margin-top: 10px;",
      p(tags$b("Za\u0142\u00f3\u017cmy, \u017ce p\u0142e\u0107 NIE ma znaczenia (H\u2080).")),
      p("Skoro p\u0142e\u0107 nie wp\u0142ywa na mandaty, to nie musimy dzieli\u0107 danych na kobiety i m\u0119\u017cczyzn.
        Patrzymy na ", tags$b("ca\u0142o\u015b\u0107"), ": 80 mandat\u00f3w na 200 kontroli = ",
        tags$b("40%"), "."),
      p("Je\u015bli p\u0142e\u0107 jest niezale\u017cna, to te 40% powinno by\u0107 ",
        tags$b("takie samo"), " dla kobiet i m\u0119\u017cczyzn:"),
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(tags$tr(tags$th(""), tags$th("Mandat"), tags$th("Brak mandatu"), tags$th("Razem"))),
        tags$tbody(
          tags$tr(tags$td(tags$b("Kobiety")), tags$td(tags$em("40")), tags$td(tags$em("60")), tags$td("100")),
          tags$tr(tags$td(tags$b("M\u0119\u017cczy\u017ani")), tags$td(tags$em("40")), tags$td(tags$em("60")), tags$td("100")),
          tags$tr(tags$td(tags$b("Razem")), tags$td("80"), tags$td("120"), tags$td("200"))
        )
      ),
      p("To jest ", tags$b("tabela oczekiwana"), " \u2014 ile by by\u0142o, gdyby p\u0142e\u0107 nie mia\u0142a wp\u0142ywu.")
    )
  })

  output$ch5_narr3 <- renderUI({
    req(input$ch5_narr_step3 %% 2 == 1)

    div(class = "callout-success", style = "margin-top: 10px;",
      p(tags$b("Por\u00f3wnanie: obserwowane vs oczekiwane")),
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(tags$tr(tags$th(""), tags$th("Mandat (obs.)"), tags$th("Mandat (oczek.)"), tags$th("R\u00f3\u017cnica"))),
        tags$tbody(
          tags$tr(tags$td(tags$b("Kobiety")), tags$td("30"), tags$td("40"), tags$td(tags$b("\u221210"))),
          tags$tr(tags$td(tags$b("M\u0119\u017cczy\u017ani")), tags$td("50"), tags$td("40"), tags$td(tags$b("+10")))
        )
      ),
      p("Kobiety dosta\u0142y ", tags$b("10 mandat\u00f3w mniej"), " ni\u017c oczekiwano,
        m\u0119\u017cczy\u017ani ", tags$b("10 wi\u0119cej"), "."),
      p("Test \u03c7\u00b2 bierze te r\u00f3\u017cnice, podnosi do kwadratu, dzieli przez oczekiwane
        i sumuje po wszystkich kom\u00f3rkach. Im wi\u0119ksza ta suma, tym trudniej
        wyt\u0142umaczy\u0107 r\u00f3\u017cnice przypadkiem."),
      p(tags$em("To w\u0142a\u015bnie robi wz\u00f3r: "),
        withMathJax("\\(\\chi^2 = \\sum \\frac{(O_{ij} - E_{ij})^2}{E_{ij}}\\)"))
    )
  })

  observeEvent(input$ch5_scenario, {
    ch5_tab(NULL)
    ch5_step(0)
  })

  observeEvent(input$ch5_step1, ch5_step(1))
  observeEvent(input$ch5_step2, ch5_step(2))
  observeEvent(input$ch5_step3, ch5_step(3))
  observeEvent(input$ch5_step4, ch5_step(4))

  # --- Panel hipotezy ---
  output$ch5_hypothesis_panel <- renderUI({
    par <- scenario_params[[input$ch5_scenario]]
    tab <- ch5_tab()
    tagList(
      div(class = "callout-info", style = "font-size: 16px;",
        p(tags$b("Pytanie potoczne:")),
        p(tags$em(paste0("\u201e", par$question, "\u201d")))
      ),
      div(class = "formula-box",
        p(tags$b("Hipoteza formalna:")),
        p(withMathJax(par$h0_text)),
        p(withMathJax(par$h1_text))
      ),
      if (is.null(tab)) {
        div(style = "text-align: center; margin: 10px 0; color: #7f8c8d;",
          p(tags$em("Kliknij \u201eLosuj pr\u00f3b\u0119\u201d"))
        )
      }
    )
  })

  # --- Krokowy wykres ---
  output$ch5_step_plot <- renderPlot({
    tab <- ch5_tab()
    step <- ch5_step()
    par <- scenario_params[[input$ch5_scenario]]

    if (is.null(tab) || step == 0) return(NULL)

    if (step <= 2) {
      df <- as.data.frame(tab)
      names(df) <- c("Var1", "Var2", "Freq")

      if (step == 1) {
        # Slupki z liczebnosciami
        ggplot(df, aes(x = Var1, y = Freq, fill = Var2)) +
          geom_col(position = "dodge", alpha = 0.8) +
          geom_text(aes(label = Freq), position = position_dodge(width = 0.9),
                    vjust = -0.3, size = 4) +
          labs(title = paste0(par$lab1, " vs ", par$lab2, " (liczno\u015bci)"),
               x = par$lab1, y = "Liczno\u015b\u0107", fill = par$lab2) +
          scale_fill_brewer(palette = "Set2") +
          theme_educational() +
          theme(legend.position = "top")
      } else {
        # Slupki z procentami (w obr\u0119bie wiersza)
        df_pct <- df %>%
          group_by(Var1) %>%
          mutate(pct = round(Freq / sum(Freq) * 100, 1)) %>%
          ungroup()

        ggplot(df_pct, aes(x = Var1, y = pct, fill = Var2)) +
          geom_col(position = "dodge", alpha = 0.8) +
          geom_text(aes(label = paste0(pct, "%")),
                    position = position_dodge(width = 0.9),
                    vjust = -0.3, size = 4) +
          labs(title = paste0(par$lab1, " vs ", par$lab2, " (% w grupie)"),
               x = par$lab1, y = "Procent", fill = par$lab2) +
          scale_fill_brewer(palette = "Set2") +
          theme_educational() +
          theme(legend.position = "top")
      }
    } else {
      # Krok 3-4: rozklad chi-kwadrat
      test <- chisq.test(tab)
      chi_stat <- as.numeric(test$statistic)
      df_val <- as.numeric(test$parameter)
      plot_test_distribution(chi_stat, df = df_val, test_type = "chisq")
    }
  })

  # --- Krokowe info ---
  output$ch5_step_info <- renderUI({
    tab <- ch5_tab()
    step <- ch5_step()
    par <- scenario_params[[input$ch5_scenario]]

    if (is.null(tab) || step == 0) return(NULL)

    test <- chisq.test(tab)
    n_total <- sum(tab)

    # Buduj HTML tabele krzyzowa
    .html_table <- function(mat, caption = "") {
      header <- tags$tr(tags$th(""),
        lapply(colnames(mat), function(cn) tags$th(cn)))
      rows <- lapply(seq_len(nrow(mat)), function(i) {
        tags$tr(tags$td(tags$b(rownames(mat)[i])),
          lapply(seq_len(ncol(mat)), function(j) tags$td(mat[i, j])))
      })
      div(
        if (nchar(caption) > 0) p(tags$b(caption)),
        tags$table(class = "table table-bordered table-striped",
                   style = "font-size: 14px;",
          tags$thead(header),
          tags$tbody(rows))
      )
    }

    info <- switch(as.character(step),
      "1" = tagList(
        div(class = "stat-box", style = paste0("background:", col_h0, ";"),
            paste0("n = ", n_total)),
        .html_table(tab, paste0("Tabela krzy\u017cowa: ", par$lab1, " \u00d7 ", par$lab2)),
        p("To s\u0105 obserwowane liczno\u015bci. Ale same liczby trudno por\u00f3wna\u0107,
          bo grupy mog\u0105 mie\u0107 r\u00f3\u017cne rozmiary. Kliknij krok 2.")
      ),
      "2" = {
        pct_tab <- round(prop.table(tab, margin = 1) * 100, 1)
        pct_mat <- matrix(paste0(pct_tab, "%"), nrow = nrow(pct_tab),
                          dimnames = dimnames(pct_tab))
        tagList(
          .html_table(pct_mat, "Procenty w ka\u017cdej grupie (wierszu):"),
          p("Gdyby zmienne by\u0142y niezale\u017cne, procenty by\u0142yby ",
            tags$b("takie same"), " w ka\u017cdym wierszu.
            Czy widzisz r\u00f3\u017cnice?")
        )
      },
      "3" = {
        chi_stat <- as.numeric(test$statistic)
        df_val <- as.numeric(test$parameter)
        exp_mat <- round(test$expected, 1)
        low_exp <- any(test$expected < 5)
        tagList(
          .html_table(exp_mat, "Liczno\u015bci oczekiwane (gdyby H\u2080 prawdziwa):"),
          div(class = "stat-box", style = paste0("background:", col_effect, ";"),
              paste0("\u03c7\u00b2(", df_val, ") = ", round(chi_stat, 3))),
          p("Statystyka \u03c7\u00b2 mierzy \u0142\u0105czn\u0105 rozbie\u017cno\u015b\u0107 mi\u0119dzy tabel\u0105 obserwowanc\u0105
            a tabel\u0105 oczekiwan\u0105."),
          if (low_exp) p(style = "color: #e74c3c; font-weight: bold;",
            "\u26a0 Uwaga: niekt\u00f3re oczekiwane liczno\u015bci < 5!")
        )
      },
      "4" = {
        p_val <- test$p.value
        res <- format_test_result(p_val)

        # Cramers V
        k <- min(nrow(tab), ncol(tab))
        v <- sqrt(as.numeric(test$statistic) / (n_total * (k - 1)))

        # Zakres procentow
        pct_tab <- prop.table(tab, margin = 1) * 100
        pct_cols <- apply(pct_tab, 2, function(col) round(range(col), 1))

        tagList(
          div(class = "stat-box", style = paste0("background:", col_pvalue, ";"),
              paste0("p = ", format.pval(p_val, digits = 4))),
          p(style = paste0("color: ", res$color, "; font-weight: bold; font-size: 16px;"),
            res$decision),
          p(res$explanation),
          hr(),
          p(tags$b("Si\u0142a zwi\u0105zku:")),
          p("Cram\u00e9r's V = ", tags$b(round(v, 3)),
            " (", effect_size_label(v), ")"),
          p("Rozrzut procent\u00f3w mi\u0119dzy grupami: ",
            paste(colnames(pct_tab), "od", pct_cols[1, ], "do", pct_cols[2, ], "%",
                  collapse = "; "))
        )
      }
    )
    div(class = "callout-info", info)
  })

  # --- Widget 2: Porownanie chi-kwadrat vs Fisher ---
  output$ch5_compare_result <- renderUI({
    req(input$ch5_compare)
    tab <- isolate(ch5_tab())

    if (is.null(tab)) {
      return(div(class = "callout-warning",
        "Najpierw wylosuj pr\u00f3b\u0119 w widgecie powy\u017cej."))
    }

    test_chi <- chisq.test(tab)
    test_fisher <- tryCatch(
      fisher.test(tab),
      error = function(e) fisher.test(tab, simulate.p.value = TRUE, B = 2000)
    )

    low_exp <- any(test_chi$expected < 5)
    n_low <- sum(test_chi$expected < 5)

    div(
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th(""), tags$th("Test \u03c7\u00b2"), tags$th("Test Fishera"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$b("p-warto\u015b\u0107")),
            tags$td(tags$b(format.pval(test_chi$p.value, digits = 4))),
            tags$td(tags$b(format.pval(test_fisher$p.value, digits = 4)))
          ),
          tags$tr(
            tags$td(tags$b("Decyzja")),
            tags$td(style = paste0("color:", format_test_result(test_chi$p.value)$color),
                    format_test_result(test_chi$p.value)$decision),
            tags$td(style = paste0("color:", format_test_result(test_fisher$p.value)$color),
                    format_test_result(test_fisher$p.value)$decision)
          )
        )
      ),
      div(class = if (low_exp) "callout-danger" else "callout-success",
        p(tags$b("Oczekiwane liczno\u015bci < 5: "),
          if (low_exp) paste0("TAK (", n_low, " kom\u00f3rek) \u2014 \u03c7\u00b2 mo\u017ce by\u0107 niedok\u0142adny, preferuj Fishera!")
          else "NIE \u2014 oba testy daj\u0105 wiarygodne wyniki.")
      )
    )
  })

  # --- Widget 3: Sila zwiazku na danych z Widget 1 ---
  output$ch5_effect_result <- renderUI({
    req(input$ch5_effect)
    tab <- isolate(ch5_tab())
    par <- isolate(scenario_params[[input$ch5_scenario]])

    if (is.null(tab)) {
      return(div(class = "callout-warning",
        "Najpierw wylosuj pr\u00f3b\u0119 w widgecie powy\u017cej."))
    }

    test <- chisq.test(tab)
    n_total <- sum(tab)
    k <- min(nrow(tab), ncol(tab))
    v <- sqrt(as.numeric(test$statistic) / (n_total * (k - 1)))

    # Tabela procentow per wiersz
    pct_tab <- prop.table(tab, margin = 1) * 100

    # Buduj czytelna tabelke procentow
    pct_rows <- lapply(seq_len(nrow(pct_tab)), function(i) {
      tags$tr(
        tags$td(tags$b(rownames(pct_tab)[i])),
        lapply(seq_len(ncol(pct_tab)), function(j) {
          tags$td(paste0(round(pct_tab[i, j], 1), "%"))
        })
      )
    })

    # Zakres procentow per kolumna
    range_info <- sapply(seq_len(ncol(pct_tab)), function(j) {
      vals <- pct_tab[, j]
      paste0(colnames(pct_tab)[j], ": od ", round(min(vals), 1),
             "% do ", round(max(vals), 1), "%",
             " (rozrzut ", round(max(vals) - min(vals), 1), " pp)")
    })

    div(
      p(tags$b("Procenty w ka\u017cdej grupie ", par$lab1, ":")),
      tags$table(class = "table table-bordered table-striped", style = "font-size: 15px;",
        tags$thead(tags$tr(
          tags$th(par$lab1),
          lapply(colnames(pct_tab), function(cn) tags$th(paste0(par$lab2, ": ", cn)))
        )),
        tags$tbody(pct_rows)
      ),
      div(class = "callout-info",
        p(tags$b("Rozrzut procent\u00f3w mi\u0119dzy grupami:")),
        tags$ul(lapply(range_info, function(ri) tags$li(ri))),
        p("Im wi\u0119kszy rozrzut, tym silniejszy zwi\u0105zek praktyczny."),
        hr(),
        p(tags$b("Cram\u00e9r's V = ", round(v, 3)),
          " (", effect_size_label(v), ")")
      )
    )
  })
}
