# ============================================================================
# CASE STUDY 1: CASchools
# Pytanie: Czy zmniejszenie klas poprawi wyniki uczniow w Kalifornii?
# ============================================================================

# Ladowanie i przygotowanie danych
data("CASchools", package = "AER")
ca <- CASchools
ca$score <- (ca$read + ca$math) / 2
ca$str <- ca$students / ca$teachers
ca$comp_per_student <- ca$computer / ca$students
ca$poverty <- ifelse(ca$lunch >= 50, "Wysoki", ifelse(ca$lunch >= 25, "\u015aredni", "Niski"))
ca$poverty <- factor(ca$poverty, levels = c("Niski", "\u015aredni", "Wysoki"))

ch1_ui <- tabPanel("1. CASchools",
  fluidRow(column(8, offset = 2,

    # ========================================================================
    # KONTEKST: Sytuacja decyzyjna
    # ========================================================================
    div(class = "section-title", "Sytuacja wyj\u015bciowa"),

    div(class = "narrative",
      p("Jeste\u015bmy analitykami w kalifornijskim departamencie edukacji.
        Polityk proponuje zmniejszenie liczebno\u015bci klas jako spos\u00f3b na popraw\u0119
        wynik\u00f3w egzaminacyjnych. Program b\u0119dzie kosztowa\u0107 miliardy dolar\u00f3w."),
      p("Naszym zadaniem jest zbada\u0107 na dost\u0119pnych danych:"),
      div(class = "callout-info",
        tags$strong("G\u0142\u00f3wne pytanie badawcze:"),
        p(tags$em("\"Czy zmniejszenie liczby uczni\u00f3w na nauczyciela (STR)
          faktycznie prowadzi do lepszych wynik\u00f3w, czy te\u017c obserwowany
          zwi\u0105zek wynika z innych czynnik\u00f3w?\"")),
        p("Innymi s\u0142owy: czy warto wyda\u0107 te pieni\u0105dze na mniejsze klasy,
          czy mo\u017ce s\u0105 skuteczniejsze interwencje?")
      )
    ),

    div(class = "narrative",
      p(tags$b("Dane:"), " CASchools \u2014 420 dystrykt\u00f3w szkolnych w Kalifornii
        (pakiet AER). Ka\u017cda obserwacja to jeden dystrykt."),
      p(tags$b("Zmienna zale\u017cna:"), " \u015bredni wynik egzaminu = (reading + math) / 2"),
      p(tags$b("Kluczowa zmienna niezale\u017cna:"), " STR = uczniowie / nauczyciele")
    ),

    div(class = "callout-warning",
      tags$strong("Plan analizy:"),
      tags$ol(
        tags$li("Pozna\u0107 dane \u2014 czym dysponujemy i jakie s\u0105 potencjalne zmienne zak\u0142\u00f3caj\u0105ce"),
        tags$li("Sprawdzi\u0107 prosty zwi\u0105zek STR \u2192 wyniki (korelacja, regresja prosta)"),
        tags$li("Zidentyfikowa\u0107 zmienne zak\u0142\u00f3caj\u0105ce \u2014 co jeszcze wp\u0142ywa na wyniki i jest skorelowane z STR?"),
        tags$li("Zbudowa\u0107 model wieloraki \u2014 czy efekt STR przetrwa kontrolowanie zak\u0142\u00f3ce\u0144?"),
        tags$li("Odpowiedzie\u0107 na pytanie decyzyjne")
      )
    ),

    # ========================================================================
    # KROK 1: Poznanie danych
    # ========================================================================
    div(class = "section-title", "Krok 1: Poznanie danych"),

    div(class = "analysis-step",
      span(class = "step-number", "1"),
      "Zanim zaczniemy analizowa\u0107, musimy rozumie\u0107,
                   czym dysponujemy. Jakie zmienne mog\u0105 by\u0107 istotne?"
    ),

    div(class = "widget-block",
      h4("Przegl\u0105d zmiennych"),
      fluidRow(
        column(4,
          selectInput("ch1_eda_var", "Zmienna:",
            choices = c(
              "\u015aredni wynik (score)" = "score",
              "Uczniowie/nauczyciel (STR)" = "str",
              "Wydatki/ucze\u0144" = "expenditure",
              "Doch\u00f3d dystryktu" = "income",
              "% English learners" = "english",
              "% darmowy lunch (proxy biedy)" = "lunch",
              "% CalWorks (zasi\u0142ki)" = "calworks"
            ),
            selected = "score"
          )
        ),
        column(8,
          plotOutput("ch1_eda_plot", height = "280px"),
          uiOutput("ch1_eda_stats")
        )
      )
    ),

    div(class = "narrative",
      p(tags$b("Kluczowa obserwacja:"), " mamy kilka zmiennych opisuj\u0105cych
        status socjoekonomiczny dystryktu (doch\u00f3d, % darmowy lunch, % CalWorks).
        Mog\u0105 by\u0107 zmiennymi zak\u0142\u00f3caj\u0105cymi \u2014 je\u015bli biedniejsze dystrykty
        maj\u0105 jednocze\u015bnie wi\u0119ksze klasy I gorsze wyniki, to obserwowany
        zwi\u0105zek STR\u2192wyniki mo\u017ce by\u0107 pozorny.")
    ),

    div(class = "widget-block",
      h4("Macierz korelacji \u2014 szukamy potencjalnych zak\u0142\u00f3ce\u0144"),
      plotOutput("ch1_corr_plot", height = "400px")
    ),

    div(class = "callout-danger",
      tags$strong("Czerwona flaga!"),
      p("Zmienne spo\u0142eczno-ekonomiczne (lunch, calworks, income) s\u0105:"),
      tags$ul(
        tags$li("silnie skorelowane z wynikami (r \u2248 \u22120.87 dla lunch)"),
        tags$li("skorelowane z STR (biedniejsze dystrykty maj\u0105 wi\u0119ksze klasy)")
      ),
      p("To oznacza, \u017ce prosty zwi\u0105zek STR\u2192wyniki mo\u017ce by\u0107 artefaktem biedy.
        Musimy to rozsupla\u0107.")
    ),

    # ========================================================================
    # KROK 2: Prosty zwiazek STR -> wyniki
    # ========================================================================
    div(class = "section-title", "Krok 2: Prosty zwi\u0105zek STR \u2192 wyniki"),

    div(class = "analysis-step",
      span(class = "step-number", "2"),
      "Najpierw sprawd\u017amy naiwny zwi\u0105zek \u2014 bez kontroli czegokolwiek.
                   To b\u0119dzie nasz punkt wyj\u015bcia."
    ),

    div(class = "narrative",
      div(class = "formula-box",
        p(withMathJax("\\(H_0: \\rho_{\\text{STR, score}} = 0\\)"),
          " \u2014 brak zwi\u0105zku liniowego"),
        p(withMathJax("\\(H_1: \\rho_{\\text{STR, score}} \\neq 0\\)"),
          " \u2014 jest zwi\u0105zek")
      )
    ),

    div(class = "widget-block",
      h4("Korelacja i regresja prosta: score ~ STR"),
      fluidRow(
        column(4,
          checkboxInput("ch1_str_color", "Koloruj wg poziomu biedy", value = FALSE),
          hr(),
          uiOutput("ch1_str_test")
        ),
        column(8,
          plotOutput("ch1_str_plot", height = "380px")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Na tym etapie polityk powiedzia\u0142by:"),
      p(tags$em("\"Widzicie? Mniejsze klasy = lepsze wyniki! Dajcie mi bud\u017cet.\"")),
      p("Ale my wiemy, \u017ce to mo\u017ce by\u0107 pozorna korelacja.
        W\u0142\u0105czmy kolorowanie wg biedy \u2014 wida\u0107, \u017ce biedne dystrykty (czerwone)
        skupiaj\u0105 si\u0119 w prawym dolnym rogu (du\u017ce klasy, niskie wyniki).
        Bieda mo\u017ce t\u0142umaczy\u0107 oba zjawiska.")
    ),

    # ========================================================================
    # KROK 3: Czy bieda to zmienna zaklócajaca?
    # ========================================================================
    div(class = "section-title", "Krok 3: Czy bieda jest zmienn\u0105 zak\u0142\u00f3caj\u0105c\u0105?"),

    div(class = "analysis-step",
      span(class = "step-number", "3"),
      "Aby zmienna Z by\u0142a zak\u0142\u00f3caj\u0105c\u0105, musi spe\u0142nia\u0107 dwa warunki:
                   (a) wp\u0142ywa\u0107 na Y i (b) by\u0107 skorelowana z X.
                   Sprawdzamy oba."
    ),

    div(class = "narrative",
      p("Zmienn\u0105 zak\u0142\u00f3caj\u0105c\u0105 (confoundem) nazwiemy ",
        tags$b("% darmowy lunch"), " \u2014 to standardowe proxy biedy
        w badaniach edukacyjnych."),
      p("Warunek (a): lunch wp\u0142ywa na wyniki?"),
      p("Warunek (b): lunch jest skorelowany z STR?")
    ),

    div(class = "widget-block",
      h4("Sprawdzenie dw\u00f3ch warunk\u00f3w"),
      fluidRow(
        column(6,
          plotOutput("ch1_conf_a", height = "280px")
        ),
        column(6,
          plotOutput("ch1_conf_b", height = "280px")
        )
      ),
      uiOutput("ch1_conf_stats")
    ),

    div(class = "narrative",
      p("Oba warunki spe\u0142nione. Lunch (bieda) jest confoundem.
        Teraz kluczowe pytanie: czy efekt STR przetrwa, gdy skontrolujemy bied\u0119?")
    ),

    div(class = "widget-block",
      h4("Dodatkowe sprawdzenie: wyniki w grupach biedy"),
      fluidRow(
        column(4,
          uiOutput("ch1_anova_result")
        ),
        column(8,
          plotOutput("ch1_anova_plot", height = "320px")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Wynik ANOVA potwierdza:"),
      " bieda ma ogromny wp\u0142yw na wyniki \u2014 r\u00f3\u017cnica mi\u0119dzy grupami
        to ~30 punkt\u00f3w. To wielokrotnie wi\u0119cej ni\u017c ca\u0142y zakres STR."
    ),

    # ========================================================================
    # KROK 4: Kontrolowanie zaklócen (regresja wieloraka)
    # ========================================================================
    div(class = "section-title", "Krok 4: Czy efekt STR przetrwa kontrol\u0119?"),

    div(class = "analysis-step",
      span(class = "step-number", "4"),
      "Budujemy modele regresji, stopniowo dodaj\u0105c zmienne kontrolne.
                   Obserwujemy, co dzieje si\u0119 z wsp\u00f3\u0142czynnikiem STR."
    ),

    div(class = "narrative",
      p("Strategia: zaczynamy od prostego modelu (sam STR) i dodajemy
        zmienne, kt\u00f3re podejrzewamy o zak\u0142\u00f3canie. Je\u015bli \u03b2 przy STR
        znacznie zmaleje lub straci istotno\u015b\u0107, to prosty zwi\u0105zek by\u0142 pozorny.")
    ),

    div(class = "widget-block",
      h4("Seria modeli \u2014 co si\u0119 dzieje z efektem STR?"),
      actionButton("ch1_compare_models", "Buduj 4 modele",
                   class = "btn-primary", width = "250px"),
      br(), br(),
      uiOutput("ch1_model_comparison"),
      plotOutput("ch1_beta_str_plot", height = "250px")
    ),

    div(class = "callout-success",
      tags$strong("Kluczowe odkrycie:"),
      p("Wsp\u00f3\u0142czynnik \u03b2 przy STR maleje po dodaniu zmiennych kontrolnych,
        ale nie zanika ca\u0142kowicie."),
      p("To sugeruje, \u017ce cz\u0119\u015b\u0107 oryginalnego efektu by\u0142a pozorna
        (napedzana bied\u0105), ale mniejsze klasy mog\u0105 mie\u0107 niewielki realny efekt
        \u2014 rz\u0119du ~1 punkt na ka\u017cdego dodatkowego ucznia na nauczyciela.")
    ),

    # ========================================================================
    # KROK 5: Wybrany model — szczegoly
    # ========================================================================
    div(class = "section-title", "Krok 5: Analiza wybranego modelu"),

    div(class = "analysis-step",
      span(class = "step-number", "5"),
      "Zbadajmy szczeg\u00f3\u0142owo najlepszy model.
                   Mo\u017cesz sam wybra\u0107 predyktory."
    ),

    div(class = "widget-block",
      h4("Model wieloraki \u2014 wyb\u00f3r predyktor\u00f3w"),
      fluidRow(
        column(4,
          checkboxGroupInput("ch1_reg_vars", "Predyktory:",
            choices = c(
              "STR" = "str",
              "Doch\u00f3d" = "income",
              "% English learners" = "english",
              "% darmowy lunch" = "lunch",
              "Wydatki/ucze\u0144" = "expenditure"
            ),
            selected = c("str", "income", "english")
          ),
          actionButton("ch1_fit_model", "Dopasuj",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch1_reg_metrics")
        ),
        column(8,
          uiOutput("ch1_reg_coefs"),
          plotOutput("ch1_reg_coef_plot", height = "230px")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Eksperymentuj:"),
      tags$ul(
        tags$li("Dodaj ", tags$b("lunch"), " do modelu z income \u2014 co si\u0119 stanie?
                 (Wsp\u00f3\u0142liniowo\u015b\u0107! Oba mierz\u0105 bied\u0119.)"),
        tags$li("Usu\u0144 income \u2014 jak zmieni si\u0119 \u03b2 przy STR?"),
        tags$li("Dodaj expenditure \u2014 czy wydatki maj\u0105 efekt po kontroli biedy?")
      )
    ),

    # ========================================================================
    # KROK 6: Odpowiedz na pytanie decyzyjne
    # ========================================================================
    div(class = "section-title", "Krok 6: Odpowied\u017a na pytanie decyzyjne"),

    div(class = "analysis-step",
      span(class = "step-number", "6"),
      "Wracamy do oryginalnego pytania: czy zmniejszenie klas to dobra inwestycja?"
    ),

    div(class = "callout-success",
      tags$strong("Co m\u00f3wi\u0105 dane:"),
      tags$ol(
        tags$li(tags$b("Prosty zwi\u0105zek STR\u2192wyniki istnieje"),
                " (r \u2248 \u22120.23), ale jest w du\u017cej mierze nap\u0119dzany bied\u0105
                \u2014 biedniejsze dystrykty maj\u0105 wi\u0119ksze klasy I gorsze wyniki."),
        tags$li(tags$b("Po kontroli biedy i ELL efekt STR maleje"),
                ", ale nie zanika \u2014 oko\u0142o \u22121 punkt na ka\u017cdego dodatkowego
                ucznia na nauczyciela."),
        tags$li(tags$b("Bieda jest wielokrotnie silniejszym predyktorem"),
                " ni\u017c wielko\u015b\u0107 klas. R\u00f3\u017cnica mi\u0119dzy biednym a zamo\u017cnym dystryktem
                to ~30 punkt\u00f3w; ca\u0142y zakres STR to ~5 punkt\u00f3w."),
        tags$li(tags$b("Wydatki na ucznia maj\u0105 zaskakuj\u0105co s\u0142aby efekt"),
                " po kontroli biedy \u2014 wi\u0119cej pieni\u0119dzy samo w sobie nie pomaga.")
      )
    ),

    div(class = "callout-warning",
      tags$strong("Rekomendacja (gdyby to by\u0142 raport):"),
      p("Zmniejszenie klas mo\u017ce mie\u0107 niewielki pozytywny efekt, ale nie jest
        \"silver bullet\". Za miliardy dolar\u00f3w uzyska si\u0119 poprawk\u0119 rz\u0119du kilku punkt\u00f3w."),
      p("Bardziej efektywne mog\u0105 by\u0107 interwencje celowane w przyczyny biedy edukacyjnej:
        wsparcie j\u0119zykowe dla ELL, programy \u017cywieniowe, wsparcie rodzin.")
    ),

    div(class = "callout-danger",
      tags$strong("Ograniczenia naszej analizy:"),
      tags$ul(
        tags$li(tags$b("Dane obserwacyjne, nie eksperymentalne"),
                " \u2014 nie mo\u017cemy orzeka\u0107 o przyczynowo\u015bci.
                  Mo\u017ce istniej\u0105 pomini\u0119te zmienne (np. jako\u015b\u0107 nauczycieli)."),
        tags$li(tags$b("Dane zagregowane na poziomie dystryktu"),
                " \u2014 tracimy zmienno\u015b\u0107 mi\u0119dzy szko\u0142ami wewn\u0105trz dystryktu.
                  B\u0142\u0105d ekologiczny."),
        tags$li(tags$b("Wsp\u00f3\u0142liniowo\u015b\u0107"),
                " \u2014 lunch, income, calworks mierz\u0105 to samo (bied\u0119).
                  Nie powinny by\u0107 w modelu jednocze\u015bnie."),
        tags$li(tags$b("Przekrojowe, nie pod\u0142u\u017cne"),
                " \u2014 widzimy jeden moment, nie zmiany w czasie.
                  Nie wiemy, czy dystrykty kt\u00f3re zmniejszy\u0142y klasy, poprawi\u0142y wyniki.")
      ),
      p(tags$em("Aby naprawd\u0119 odpowiedzie\u0107 na pytanie decyzyjne,
        potrzebowaliby\u015bmy eksperymentu (np. projekt STAR z Tennessee)
        lub danych panelowych z instrumentami."))
    )

  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  # --- Krok 1: EDA ---
  output$ch1_eda_plot <- renderPlot({
    var <- input$ch1_eda_var
    var_label <- switch(var,
      "score" = "\u015aredni wynik", "str" = "Uczniowie/nauczyciel",
      "expenditure" = "Wydatki/ucze\u0144 ($)", "income" = "Doch\u00f3d ($tys.)",
      "english" = "% English learners", "lunch" = "% darmowy lunch",
      "calworks" = "% CalWorks")

    p1 <- ggplot(ca, aes(x = .data[[var]])) +
      geom_histogram(bins = 30, fill = col_explore, alpha = 0.6, color = "white") +
      labs(title = paste0("Rozk\u0142ad: ", var_label), x = var_label, y = "Liczba") +
      theme_educational()

    p2 <- ggplot(ca, aes(y = .data[[var]])) +
      geom_boxplot(fill = col_explore, alpha = 0.4) +
      labs(title = "Boxplot", y = var_label) +
      theme_educational()

    gridExtra::grid.arrange(p1, p2, ncol = 2, widths = c(2, 1))
  })

  output$ch1_eda_stats <- renderUI({
    var <- input$ch1_eda_var
    x <- ca[[var]]
    tagList(
      div(class = "stat-box", style = paste0("background:", col_explore, ";"),
          paste0("n = ", length(x))),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("\u015ar. = ", round(mean(x), 1))),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("SD = ", round(sd(x), 1))),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("Zakres: ", round(min(x), 1), "\u2013", round(max(x), 1)))
    )
  })

  output$ch1_corr_plot <- renderPlot({
    vars <- c("score", "str", "expenditure", "income", "english", "lunch", "calworks")
    cor_mat <- cor(ca[, vars], use = "complete.obs")

    cor_df <- as.data.frame(as.table(cor_mat))
    names(cor_df) <- c("Var1", "Var2", "value")

    labels_pl <- c(
      "score" = "Wynik", "str" = "STR", "expenditure" = "Wydatki",
      "income" = "Doch\u00f3d", "english" = "% ELL", "lunch" = "% lunch",
      "calworks" = "% CalWorks")
    cor_df$Var1 <- labels_pl[as.character(cor_df$Var1)]
    cor_df$Var2 <- labels_pl[as.character(cor_df$Var2)]

    ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(value, 2)), size = 3.5) +
      scale_fill_gradient2(low = col_highlight, mid = "white", high = col_explore,
                           midpoint = 0, limits = c(-1, 1), name = "r") +
      labs(title = "Macierz korelacji \u2014 szukamy powi\u0105za\u0144 i potencjalnych zak\u0142\u00f3ce\u0144",
           x = NULL, y = NULL) +
      theme_educational() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # --- Krok 2: STR vs wyniki ---
  output$ch1_str_plot <- renderPlot({
    p <- ggplot(ca, aes(x = str, y = score))

    if (input$ch1_str_color) {
      p <- p + geom_point(aes(color = poverty), alpha = 0.6, size = 2) +
        scale_color_manual(values = c(col_explore, col_conclude, col_highlight),
                           name = "Bieda")
    } else {
      p <- p + geom_point(color = col_dark, alpha = 0.4, size = 2)
    }

    p + geom_smooth(method = "lm", se = TRUE,
                    color = col_model, fill = col_model, alpha = 0.1) +
      labs(title = "STR vs wyniki egzaminu",
           subtitle = "Ka\u017cdy punkt = jeden dystrykt szkolny",
           x = "Uczniowie na nauczyciela (STR)",
           y = "\u015aredni wynik egzaminu") +
      theme_educational()
  })

  output$ch1_str_test <- renderUI({
    cor_res <- rstatix::cor_test(ca, str, score, method = "pearson")
    tidy_cor <- as.data.frame(cor_res)

    model <- lm(score ~ str, data = ca)
    coefs <- broom::tidy(model)
    g <- broom::glance(model)

    tagList(
      div(class = "callout-info",
        p(tags$strong("Korelacja Pearsona:")),
        p(paste0("r = ", round(tidy_cor$cor, 3),
                 ", p ", if (tidy_cor$p < 0.001) "< 0.001" else paste0("= ", round(tidy_cor$p, 4)))),
        p(style = "color: #e74c3c; font-weight: bold;",
          "Istotna ujemna korelacja")
      ),
      div(class = "callout-info",
        p(tags$strong("Regresja prosta:")),
        p(paste0("score = ", round(coefs$estimate[1], 1), " ",
                 round(coefs$estimate[2], 2), " \u00d7 STR")),
        p(paste0("R\u00b2 = ", round(g$r.squared, 3),
                 " (STR wyja\u015bnia tylko ", round(g$r.squared * 100, 1), "% zmienno\u015bci)")),
        p(tags$em("Interpretacja: ka\u017cdy dodatkowy ucze\u0144/nauczyciela \u2192 wynik ni\u017cszy o ~",
                  abs(round(coefs$estimate[2], 1)), " pkt"))
      )
    )
  })

  # --- Krok 3: Zmienne zaklocajace ---
  output$ch1_conf_a <- renderPlot({
    ggplot(ca, aes(x = lunch, y = score)) +
      geom_point(color = col_dark, alpha = 0.3, size = 1.5) +
      geom_smooth(method = "lm", se = FALSE, color = col_highlight, linewidth = 1.2) +
      labs(title = "Warunek (a): lunch \u2192 wyniki?",
           subtitle = paste0("r = ", round(cor(ca$lunch, ca$score), 3)),
           x = "% darmowy lunch", y = "Wynik") +
      theme_educational()
  })

  output$ch1_conf_b <- renderPlot({
    ggplot(ca, aes(x = lunch, y = str)) +
      geom_point(color = col_dark, alpha = 0.3, size = 1.5) +
      geom_smooth(method = "lm", se = FALSE, color = col_test, linewidth = 1.2) +
      labs(title = "Warunek (b): lunch \u2192 STR?",
           subtitle = paste0("r = ", round(cor(ca$lunch, ca$str), 3)),
           x = "% darmowy lunch", y = "STR") +
      theme_educational()
  })

  output$ch1_conf_stats <- renderUI({
    r_lunch_score <- cor(ca$lunch, ca$score)
    r_lunch_str <- cor(ca$lunch, ca$str)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_highlight, ";"),
          paste0("lunch\u2192wyniki: r = ", round(r_lunch_score, 3))),
      div(class = "stat-box", style = paste0("background:", col_test, ";"),
          paste0("lunch\u2192STR: r = ", round(r_lunch_str, 3))),
      div(class = "stat-box", style = paste0("background:", col_conclude, ";"),
          "Oba istotne \u2192 confound!")
    )
  })

  # ANOVA
  output$ch1_anova_plot <- renderPlot({
    means <- ca %>% group_by(poverty) %>%
      summarise(m = mean(score), .groups = "drop")

    ggplot(ca, aes(x = poverty, y = score, fill = poverty)) +
      geom_boxplot(alpha = 0.6, outlier.alpha = 0.2) +
      geom_jitter(width = 0.15, alpha = 0.1, size = 1) +
      scale_fill_manual(values = c(col_explore, col_conclude, col_highlight)) +
      labs(title = "Wyniki wg poziomu biedy",
           x = "Poziom biedy (na podst. % darmowy lunch)",
           y = "\u015aredni wynik") +
      theme_educational() +
      theme(legend.position = "none")
  })

  output$ch1_anova_result <- renderUI({
    result <- rstatix::anova_test(ca, score ~ poverty)
    tidy_res <- as.data.frame(result)

    tukey <- rstatix::tukey_hsd(ca, score ~ poverty)
    tukey_df <- as.data.frame(tukey)

    means <- ca %>% group_by(poverty) %>%
      summarise(m = round(mean(score), 1), n = n(), .groups = "drop")

    tagList(
      div(class = "callout-info",
        p(tags$strong("\u015arednie w grupach:")),
        lapply(1:nrow(means), function(i) {
          p(paste0(means$poverty[i], ": ", means$m[i], " (n=", means$n[i], ")"))
        }),
        hr(),
        p(tags$strong("ANOVA:")),
        p(paste0("F(", tidy_res$DFn, ",", tidy_res$DFd, ") = ",
                 round(tidy_res$F, 1),
                 ", p < 0.001, \u03b7\u00b2 = ", round(tidy_res$ges, 3))),
        hr(),
        p(tags$strong("Tukey HSD:")),
        tags$ul(lapply(1:nrow(tukey_df), function(i) {
          tags$li(paste0(tukey_df$group1[i], " vs ", tukey_df$group2[i],
                         ": \u0394 = ", round(tukey_df$estimate[i], 1),
                         " pkt, p.adj ", if (tukey_df$p.adj[i] < 0.001) "< 0.001"
                         else paste0("= ", round(tukey_df$p.adj[i], 3))))
        }))
      )
    )
  })

  # --- Krok 4: Seria modeli ---
  ch1_models_data <- reactiveVal(NULL)

  observeEvent(input$ch1_compare_models, {
    m1 <- lm(score ~ str, data = ca)
    m2 <- lm(score ~ str + income, data = ca)
    m3 <- lm(score ~ str + income + english, data = ca)
    m4 <- lm(score ~ str + income + english + lunch, data = ca)

    models <- list(m1, m2, m3, m4)
    labels <- c("1: sam STR", "2: + doch\u00f3d", "3: + doch\u00f3d + ELL", "4: + doch\u00f3d + ELL + lunch")

    results <- lapply(seq_along(models), function(i) {
      m <- models[[i]]
      g <- broom::glance(m)
      coefs <- broom::tidy(m)
      beta_str <- coefs$estimate[coefs$term == "str"]
      p_str <- coefs$p.value[coefs$term == "str"]
      data.frame(
        model = labels[i], r2 = g$r.squared, adj_r2 = g$adj.r.squared,
        aic = g$AIC, rmse = sqrt(mean(residuals(m)^2)),
        beta_str = beta_str, p_str = p_str
      )
    })

    ch1_models_data(do.call(rbind, results))
  })

  output$ch1_model_comparison <- renderUI({
    df <- ch1_models_data()
    if (is.null(df)) return(NULL)

    rows <- lapply(1:nrow(df), function(i) {
      p_str_fmt <- if (df$p_str[i] < 0.001) "< 0.001" else round(df$p_str[i], 3)
      sig_style <- if (df$p_str[i] < 0.05) "font-weight:bold;" else "color: #7f8c8d;"
      tags$tr(
        tags$td(df$model[i]),
        tags$td(style = sig_style, round(df$beta_str[i], 2)),
        tags$td(p_str_fmt),
        tags$td(round(df$adj_r2[i], 3)),
        tags$td(round(df$aic[i], 0))
      )
    })

    tagList(
      tags$table(class = "table table-bordered table-striped",
        style = "font-size: 13px;",
        tags$thead(
          tags$tr(tags$th("Model"), tags$th("\u03b2 STR"), tags$th("p (STR)"),
                  tags$th("adj.R\u00b2"), tags$th("AIC"))
        ),
        tags$tbody(rows)
      ),
      div(class = "callout-info",
        p(tags$strong("Obserwacja:"), " \u03b2 przy STR spada z ~",
          round(df$beta_str[1], 1), " do ~", round(df$beta_str[3], 1),
          " po kontroli biedy i ELL. Efekt zmaleje o ~",
          round((1 - abs(df$beta_str[3]) / abs(df$beta_str[1])) * 100), "%.")
      )
    )
  })

  output$ch1_beta_str_plot <- renderPlot({
    df <- ch1_models_data()
    if (is.null(df)) return(NULL)

    df$model <- factor(df$model, levels = df$model)
    df$sig <- df$p_str < 0.05

    ggplot(df, aes(x = model, y = beta_str, fill = sig)) +
      geom_col(alpha = 0.8, width = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_fill_manual(values = c("TRUE" = col_model, "FALSE" = "#bdc3c7"),
                        labels = c("TRUE" = "p < 0.05", "FALSE" = "nieistotny"),
                        name = NULL) +
      labs(title = "Jak zmienia si\u0119 efekt STR po dodaniu zmiennych kontrolnych?",
           x = NULL, y = "\u03b2 przy STR") +
      theme_educational() +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 20, hjust = 1))
  })

  # --- Krok 5: Model interaktywny ---
  ch1_model <- reactiveVal(NULL)

  observeEvent(input$ch1_fit_model, {
    preds <- input$ch1_reg_vars
    if (length(preds) == 0) preds <- "str"
    formula <- as.formula(paste("score ~", paste(preds, collapse = " + ")))
    ch1_model(lm(formula, data = ca))
  })

  output$ch1_reg_coefs <- renderUI({
    model <- ch1_model()
    if (is.null(model)) return(NULL)

    coefs <- broom::tidy(model)
    labels_pl <- c(
      "(Intercept)" = "Wyraz wolny", "str" = "STR",
      "income" = "Doch\u00f3d", "english" = "% ELL",
      "expenditure" = "Wydatki/ucz.", "lunch" = "% lunch")
    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)

    rows <- lapply(1:nrow(coefs), function(i) {
      sig <- coefs$p.value[i] < 0.05
      tags$tr(style = if (!sig && coefs$term[i] != "(Intercept)") "color: #bdc3c7;" else "",
        tags$td(coefs$term_pl[i]),
        tags$td(round(coefs$estimate[i], 3)),
        tags$td(round(coefs$std.error[i], 3)),
        tags$td(paste0(format.pval(coefs$p.value[i], digits = 3),
                        if (sig) " *" else ""))
      )
    })

    tags$table(class = "table table-bordered",
      style = "font-size: 13px;",
      tags$thead(tags$tr(tags$th("Zmienna"), tags$th("\u03b2"), tags$th("SE"), tags$th("p"))),
      tags$tbody(rows)
    )
  })

  output$ch1_reg_coef_plot <- renderPlot({
    model <- ch1_model()
    if (is.null(model)) return(NULL)

    coefs <- broom::tidy(model, conf.int = TRUE)
    coefs <- coefs[coefs$term != "(Intercept)", ]
    if (nrow(coefs) == 0) return(NULL)

    labels_pl <- c("str" = "STR", "income" = "Doch\u00f3d", "english" = "% ELL",
                    "expenditure" = "Wydatki", "lunch" = "% lunch")
    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)
    coefs$sig <- coefs$p.value < 0.05

    ggplot(coefs, aes(x = estimate, y = term_pl, color = sig)) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = col_dark) +
      scale_color_manual(values = c("TRUE" = col_model, "FALSE" = col_highlight),
                         labels = c("TRUE" = "p < 0.05", "FALSE" = "p \u2265 0.05"),
                         name = NULL) +
      labs(title = "Wsp\u00f3\u0142czynniki z 95% CI", x = "\u03b2", y = NULL) +
      theme_educational() + theme(legend.position = "top")
  })

  output$ch1_reg_metrics <- renderUI({
    model <- ch1_model()
    if (is.null(model)) return(NULL)
    g <- broom::glance(model)
    rmse <- sqrt(mean(residuals(model)^2))
    tagList(
      div(class = "stat-box", style = paste0("background:", col_model, ";"),
          paste0("adj.R\u00b2 = ", round(g$adj.r.squared, 3))),
      div(class = "stat-box", style = paste0("background:", col_conclude, ";"),
          paste0("AIC = ", round(g$AIC, 0))),
      div(class = "stat-box", style = paste0("background:", col_highlight, ";"),
          paste0("RMSE = ", round(rmse, 1)))
    )
  })
}
