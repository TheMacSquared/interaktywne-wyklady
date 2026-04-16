# ============================================================================
# CHAPTER 1: Od proby do populacji
# ============================================================================

ch1_ui <- tabPanel("1. Od pr\u00f3by do populacji",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Wiemy ju\u017c, \u017ce \u015brednia z pr\u00f3by zbiega do rozk\u0142adu normalnego (CTG).
       Teraz wykorzystamy to do szacowania parametr\u00f3w populacji."
    ),

    div(class = "section-title", "Estymacja \u2014 od pr\u00f3by do populacji"),

    div(class = "narrative",
      p("W statystyce rzadko znamy parametry ca\u0142ej populacji.
        Zamiast tego pobieramy ", tags$b("pr\u00f3b\u0119"), " i na jej podstawie
        ", tags$b("szacujemy"), " (estymujemy) nieznany parametr."),
      p("Na przyk\u0142ad: nie znamy \u015bredniego wzrostu wszystkich student\u00f3w
        w Polsce, ale mo\u017cemy zmierzy\u0107 100 os\u00f3b i obliczy\u0107 \u015bredni\u0105 z pr\u00f3by ",
        withMathJax("\\(\\bar{x}\\)"), " jako ", tags$b("estymator"),
        " \u015bredniej populacyjnej ", withMathJax("\\(\\mu\\)"), ".")
    ),

    # ========================================================================
    # WIDGET 1: Estymator w akcji
    # ========================================================================
    div(class = "section-title", "Estymator w akcji"),

    div(class = "narrative",
      p("Zobaczmy, jak dzia\u0142a estymacja. Znamy prawdziwe ",
        withMathJax("\\(\\mu\\)"), " populacji (fioletowa linia).
        Za ka\u017cdym razem losujemy pr\u00f3b\u0119 i obliczamy ",
        withMathJax("\\(\\bar{x}\\)"), ".")
    ),

    div(class = "widget-block",
      h4("Losowanie pr\u00f3b z populacji"),
      fluidRow(
        column(4,
          selectInput("ch1_dist", "Rozk\u0142ad populacji:",
            choices = c(
              "Normalny (wzrost)"           = "normal",
              "Wyk\u0142adniczy (prawosko\u015bny)" = "exponential",
              "Jednostajny"                 = "uniform",
              "Dwumodalny"                  = "bimodal"
            ),
            selected = "normal"
          ),
          sliderInput("ch1_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 5, max = 200, value = 30, step = 5),
          hr(),
          div(style = "display: flex; flex-direction: column; gap: 8px;",
            actionButton("ch1_draw_1", "Pobierz 1 pr\u00f3b\u0119",
                         class = "btn-primary", width = "100%"),
            actionButton("ch1_draw_20", "Pobierz 20 pr\u00f3b",
                         class = "btn-warning", width = "100%"),
            actionButton("ch1_reset", "Reset",
                         class = "btn-outline-secondary", width = "100%")
          ),
          br(),
          uiOutput("ch1_count_info")
        ),
        column(8,
          plotOutput("ch1_estimates_plot", height = "400px"),
          uiOutput("ch1_estimates_stats")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Obserwacja:"),
      " Ka\u017cda pr\u00f3ba daje inny wynik! Ale \u015brednie z pr\u00f3b
        skupiaj\u0105 si\u0119 wok\u00f3\u0142 prawdziwego ", withMathJax("\\(\\mu\\)"),
      ". Im wi\u0119ksze n, tym bli\u017cej."
    ),

    # ========================================================================
    # SEKCJA 2: Trzy w\u0142asno\u015bci dobrego estymatora (tylko tekst)
    # ========================================================================
    div(class = "section-title", "Trzy w\u0142asno\u015bci dobrego estymatora"),

    div(class = "narrative",
      p("Sk\u0105d wiemy, czy dany estymator jest \"dobry\"? Statystycy oceniaj\u0105 estymatory
        wzgl\u0119dem trzech podstawowych w\u0142asno\u015bci: ", tags$b("nieobci\u0105\u017cono\u015bci"), ", ",
        tags$b("efektywno\u015bci"), " i ", tags$b("zgodno\u015bci"), ".")
    ),

    # (1) Nieobciazonosc
    div(class = "widget-block",
      h4("(1) Nieobci\u0105\u017conos\u0107"),
      div(class = "narrative",
        p("Estymator ", withMathJax("\\(\\hat{\\theta}\\)"), " parametru ",
          withMathJax("\\(\\theta\\)"), " jest ", tags$b("nieobci\u0105\u017cony"), ", gdy:"),
        div(class = "formula-box",
          withMathJax("$$E[\\hat{\\theta}] = \\theta$$")
        ),
        p("Czyli: ", tags$em("\u015brednio"),
          " (z bardzo wielu hipotetycznych pr\u00f3b) trafia dok\u0142adnie w prawdziwy parametr.
          Brak systematycznego b\u0142\u0119du w jedn\u0105 stron\u0119."),
        p(tags$b("Przyk\u0142ad:"),
          " \u015brednia z pr\u00f3by ", withMathJax("\\(\\bar{x}\\)"),
          " jest nieobci\u0105\u017conym estymatorem \u015bredniej populacji ",
          withMathJax("\\(\\mu\\)"),
          ". Jakkolwiek pojedynczy ", withMathJax("\\(\\bar{x}\\)"),
          " mo\u017ce by\u0107 wi\u0119kszy lub mniejszy od ", withMathJax("\\(\\mu\\)"),
          ", to \u015brednia ze ", tags$em("wszystkich mo\u017cliwych"), " pr\u00f3b r\u00f3wna si\u0119 dok\u0142adnie ",
          withMathJax("\\(\\mu\\)"), "."),
        p(tags$b("Kontrprzyk\u0142ad:"),
          " wariancja z pr\u00f3by liczona ze wzoru ",
          withMathJax("\\(\\frac{1}{n}\\sum(x_i - \\bar{x})^2\\)"),
          " jest ", tags$em("obci\u0105\u017cona"),
          " (\u015brednio zani\u017ca prawdziw\u0105 wariancj\u0119 populacji). Dlatego
          standardowo dzielimy przez ", withMathJax("\\(n-1\\)"),
          " zamiast przez ", withMathJax("\\(n\\)"),
          " \u2014 to poprawka, kt\u00f3ra czyni estymator nieobci\u0105\u017conym.")
      )
    ),

    # (2) Efektywnosc
    div(class = "widget-block",
      h4("(2) Efektywno\u015b\u0107"),
      div(class = "narrative",
        p("Spo\u015br\u00f3d wszystkich estymator\u00f3w nieobci\u0105\u017conych najlepszy jest ten,
          kt\u00f3ry ma ", tags$b("najmniejsz\u0105 wariancj\u0119"),
          " \u2014 czyli najmniej waha si\u0119 z pr\u00f3by na pr\u00f3b\u0119.
          Taki estymator nazywamy ", tags$b("efektywnym"), "."),
        p("Intuicja: dwa estymatory mog\u0105 by\u0107 ", tags$em("\u015brednio"),
          " r\u00f3wnie celne (oba nieobci\u0105\u017cone), ale jeden mo\u017ce
          regularnie da\u0107 wynik bli\u017cszy prawdy, a drugi cz\u0119sto strzela\u0107
          daleko \u2014 w r\u00f3\u017cne strony, co po u\u015brednieniu si\u0119 znosi.
          Wybieramy ten ", tags$em("ciasny"), "."),
        p(tags$b("Przyk\u0142ad:"),
          " dla rozk\u0142adu normalnego zar\u00f3wno \u015brednia, jak i mediana z pr\u00f3by
          s\u0105 nieobci\u0105\u017cone. Ale \u015brednia ma mniejsz\u0105 wariancj\u0119 \u2014 dok\u0142adnie ",
          tags$b("\u03c0/2 \u2248 1.57 razy mniejsz\u0105"),
          " ni\u017c mediana. Dlatego w fizyce, chemii i ka\u017cdym laboratoryjnym
          pomiarze standardem jest \u015brednia arytmetyczna."),
        p(tags$b("Uwaga:"),
          " efektywno\u015b\u0107 zale\u017cy od rozk\u0142adu danych. Dla danych z outlierami
          mediana mo\u017ce by\u0107 efektywniejsza ni\u017c \u015brednia.")
      )
    ),

    # (3) Zgodnosc
    div(class = "widget-block",
      h4("(3) Zgodno\u015b\u0107"),
      div(class = "narrative",
        p("Estymator jest ", tags$b("zgodny"),
          ", gdy z rosn\u0105c\u0105 wielko\u015bci\u0105 pr\u00f3by zbiega do prawdziwego parametru:"),
        div(class = "formula-box",
          withMathJax("$$\\hat{\\theta}_n \\xrightarrow{p} \\theta \\quad \\text{gdy} \\quad n \\to \\infty$$")
        ),
        p("Innymi s\u0142owy: dla bardzo du\u017cej pr\u00f3by estymator trafia w parametr
          ", tags$em("prawie na pewno"),
          ". Im wi\u0119cej obserwacji, tym mniejszy rozrzut estymatora wok\u00f3\u0142 prawdy."),
        p(tags$b("Przyk\u0142ad:"),
          " \u015brednia z pr\u00f3by jest zgodnym estymatorem \u015bredniej populacji.
          Z prawa wielkich liczb wiemy, \u017ce ", withMathJax("\\(\\bar{x} \\to \\mu\\)"),
          " gdy ", withMathJax("\\(n \\to \\infty\\)"),
          ". Dla \u015bredniej obowi\u0105zuje wz\u00f3r ",
          withMathJax("\\(SD(\\bar{x}) = \\sigma/\\sqrt{n}\\)"),
          " \u2014 odchylenie standardowe maleje proporcjonalnie do ",
          withMathJax("\\(1/\\sqrt{n}\\)"), "."),
        p(tags$b("Praktyczna konsekwencja:"),
          " \u017ceby zmniejszy\u0107 niepewno\u015b\u0107 estymatora dwa razy, musisz ",
          tags$b("czterokrotnie"), " zwi\u0119kszy\u0107 pr\u00f3b\u0119.
          To dlaczego du\u017ce badania s\u0105 takie drogie.")
      )
    ),

    div(class = "callout-info",
      tags$strong("Hierarchia w\u0142asno\u015bci:"),
      " Najpierw chcemy, \u017ceby estymator by\u0142 ", tags$b("nieobci\u0105\u017cony"),
      " (trafia\u0142 \u015brednio w cel). Spo\u015br\u00f3d nieobci\u0105\u017conych wybieramy ten ",
      tags$b("najefektywniejszy"),
      " (najmniej si\u0119 waha). I oczywi\u015bcie chcemy, \u017ceby by\u0142 ",
      tags$b("zgodny"), " \u2014 czyli trafia\u0142 dok\u0142adniej, gdy zbieramy wi\u0119cej danych."
    ),

    # ========================================================================
    # WIDGET 3: Dlaczego punkt nie wystarczy?
    # ========================================================================
    div(class = "section-title", "Dlaczego sam punkt nie wystarczy?"),

    div(class = "narrative",
      p("Nawet najlepszy estymator punktowy zmienia si\u0119 z pr\u00f3by na pr\u00f3b\u0119.
        Podanie samej liczby ", withMathJax("\\(\\bar{x} = 171.3\\)"),
        " nie m\u00f3wi nic o tym, jak bardzo mo\u017cemy si\u0119 myli\u0107."),
      p("Potrzebujemy czego\u015b wi\u0119cej \u2014 ", tags$b("przedzia\u0142u"),
        ", kt\u00f3ry powie: ", tags$em("\"z 95% pewno\u015bci\u0105 prawdziwa warto\u015b\u0107 le\u017cy mi\u0119dzy ... a ...\""),
        ".")
    ),

    div(class = "widget-block",
      h4("Wahania estymatora"),
      fluidRow(
        column(4,
          sliderInput("ch1_fluct_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 5, max = 200, value = 10, step = 5),
          helpText("Ka\u017cde klikni\u0119cie losuje now\u0105 pr\u00f3b\u0119.
                    Obserwuj, jak bardzo skacze estymata."),
          actionButton("ch1_fluct_draw", "Losuj pr\u00f3b\u0119",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch1_fluct_plot", height = "300px")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Wniosek:"),
      " Estymacja punktowa to za ma\u0142o. Potrzebujemy ",
      tags$b("przedzia\u0142u ufno\u015bci"), " \u2014 zakresu warto\u015bci,
      kt\u00f3ry z okre\u015blonym prawdopodobie\u0144stwem zawiera prawdziwy parametr."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: jak skonstruowa\u0107 taki przedzia\u0142?"),
      actionButton("ch1_next", "Dalej \u2192 2. Idea przedzia\u0142\u00f3w",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  # --- Widget 1: Estymator w akcji ---
  ch1_estimates <- reactiveVal(data.frame(
    i = integer(0), xbar = numeric(0)
  ))

  draw_samples <- function(k) {
    dist <- input$ch1_dist
    n <- input$ch1_n
    params <- get_population_params(dist)
    old <- ch1_estimates()
    new_rows <- lapply(seq_len(k), function(j) {
      samp <- generate_population_sample(dist, n)
      data.frame(i = nrow(old) + j, xbar = mean(samp))
    })
    ch1_estimates(rbind(old, do.call(rbind, new_rows)))
  }

  observeEvent(input$ch1_draw_1, draw_samples(1))
  observeEvent(input$ch1_draw_20, draw_samples(20))
  observeEvent(input$ch1_reset, {
    ch1_estimates(data.frame(i = integer(0), xbar = numeric(0)))
  })
  observeEvent(input$ch1_dist, {
    ch1_estimates(data.frame(i = integer(0), xbar = numeric(0)))
  })

  output$ch1_count_info <- renderUI({
    n_est <- nrow(ch1_estimates())
    div(class = "stat-box", style = paste0("background:", col_primary, ";"),
        paste0("Pr\u00f3b: ", n_est))
  })

  output$ch1_estimates_plot <- renderPlot({
    est <- ch1_estimates()
    params <- get_population_params(input$ch1_dist)

    if (nrow(est) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Pobierz pr\u00f3b\u0119'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      ggplot(est, aes(x = xbar)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = col_ci, alpha = 0.6, color = "white") +
        geom_vline(xintercept = params$mu, color = col_true,
                   linewidth = 1.5, linetype = "dashed") +
        annotate("text", x = params$mu, y = Inf, vjust = 2,
                 label = paste0("\u03bc = ", params$mu),
                 color = col_true, fontface = "bold", size = 5) +
        geom_vline(xintercept = mean(est$xbar), color = col_estimate,
                   linewidth = 1.5, linetype = "solid") +
        annotate("text", x = mean(est$xbar), y = Inf, vjust = 4,
                 label = paste0("\u015brednia x\u0304 = ", round(mean(est$xbar), 2)),
                 color = col_estimate, fontface = "bold", size = 5) +
        labs(title = "Rozk\u0142ad estymat \u015bredniej",
             x = expression(bar(x)), y = "G\u0119sto\u015b\u0107") +
        theme_educational()
    }
  })

  output$ch1_estimates_stats <- renderUI({
    est <- ch1_estimates()
    if (nrow(est) == 0) return(NULL)
    params <- get_population_params(input$ch1_dist)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_true, ";"),
          paste0("\u03bc = ", round(params$mu, 2))),
      div(class = "stat-box", style = paste0("background:", col_estimate, ";"),
          paste0("\u015ar. estymat = ", round(mean(est$xbar), 2))),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("SD estymat = ", round(sd(est$xbar), 2)))
    )
  })

  # --- Sekcja 2: tylko tekst, brak server logic ---

  # --- Widget 3: Wahania estymatora ---
  ch1_fluct_history <- reactiveVal(data.frame(
    draw = integer(0), xbar = numeric(0)
  ))

  observeEvent(input$ch1_fluct_draw, {
    samp <- generate_population_sample("normal", input$ch1_fluct_n)
    old <- ch1_fluct_history()
    ch1_fluct_history(rbind(old, data.frame(
      draw = nrow(old) + 1, xbar = mean(samp)
    )))
  })

  observeEvent(input$ch1_fluct_n, {
    ch1_fluct_history(data.frame(draw = integer(0), xbar = numeric(0)))
  })

  output$ch1_fluct_plot <- renderPlot({
    df <- ch1_fluct_history()
    params <- get_population_params("normal")

    if (nrow(df) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Losuj pr\u00f3b\u0119'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      ggplot(df, aes(x = draw, y = xbar)) +
        geom_hline(yintercept = params$mu, color = col_true,
                   linewidth = 1.2, linetype = "dashed") +
        geom_point(color = col_estimate, size = 3) +
        geom_line(color = col_estimate, alpha = 0.5) +
        annotate("text", x = max(df$draw), y = params$mu,
                 label = paste0("\u03bc = ", params$mu),
                 vjust = -1, color = col_true, fontface = "bold") +
        labs(title = paste0("Kolejne estymaty \u015bredniej (n = ", input$ch1_fluct_n, ")"),
             x = "Numer losowania", y = expression(bar(x))) +
        theme_educational()
    }
  })
}
