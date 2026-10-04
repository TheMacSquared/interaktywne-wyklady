# ============================================================================
# CHAPTER 3: Parametr i statystyka
# ============================================================================

ch3_ui <- list(
  id    = "ch-parametr",
  num   = "03",
  title = "Parametr i statystyka",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Dane i populacja",
      num    = "03",
      title  = "Liczba, której nie znamy, i liczba, którą mamy.",
      lead   = "Odsetek pracujących studentów na całym wydziale to jedna,
                konkretna liczba, ale zwykle nikt jej nie zna. Z próby
                liczymy jej odpowiednik i ta liczba za każdym razem wychodzi
                trochę inna. Pierwszą nazywamy parametrem, drugą statystyką."
    ),

    lc_p("Populacja i próba to zbiory jednostek. Statystyka nie zatrzymuje się
      jednak na zbiorach, tylko streszcza je liczbami: ", gloss("średnia", "średnią"), ", odsetkiem,
      rozrzutem. Ta sama formuła, na przykład „odsetek osób, które pracują”,
      daje inną liczbę, gdy liczymy ją dla całej populacji, a inną, gdy dla
      próby. Te dwie liczby mają osobne nazwy i osobne oznaczenia."),

    lc_h2("ch3-definicje", "Dwie liczby, dwa oznaczenia"),

    lc_p(gloss("parametr", "Parametr"), " to liczba opisująca populację,
      na przykład średni czas dojazdu wszystkich studentów wydziału albo
      odsetek pracujących wśród nich. Dla danej populacji parametr ma jedną,
      stałą wartość. Zazwyczaj jej nie znamy, bo nie zbadaliśmy wszystkich.
      ", gloss("statystyka", "Statystyka"), " to liczba policzona z próby
      tą samą formułą, na przykład średni czas dojazdu albo odsetek
      pracujących wśród wylosowanych osób. Statystykę zawsze znamy, bo
      liczymy ją z danych, które mamy w ręku."),

    lc_p("Żeby nie mylić tych liczb, parametry oznaczamy zwykle literami
      greckimi, a statystyki łacińskimi albo symbolem z daszkiem.
      Te same oznaczenia wracają w każdym kolejnym wykładzie."),

    lc_table(
      data.frame(
        what  = c("Średnia", "Odchylenie standardowe", "Odsetek (proporcja)"),
        param = c("μ (mi)", "σ (sigma)", "p"),
        stat  = c("x̄ (x z kreską)", "s", "p̂ (p z daszkiem)"),
        stringsAsFactors = FALSE
      ),
      list(
        lc_col("what", "Miara", "row"),
        lc_col("param", "Parametr (populacja)", "text"),
        lc_col("stat", "Statystyka (próba)", "text")
      ),
      prose = TRUE,
      caption = "Oznaczenia parametrów i odpowiadających im statystyk."
    ),

    lc_p("Średnią i odchylenie standardowe dokładnie zdefiniujemy
      w wykładzie 01. Odsetek jest prosty już teraz: to liczba jednostek
      z daną cechą podzielona przez liczbę wszystkich jednostek. Jeśli
      w próbie 50 osób pracuje 19, to p̂ = 19/50 = 0.38."),

    lc_h2("ch3-zmiennosc", "Każda próba daje inny wynik"),

    lc_p("W prawdziwym badaniu mamy jedną próbę i jedną wartość statystyki.
      W naszym wydziale możemy zrobić coś, czego w praktyce zrobić się
      nie da: losować próbę wiele razy i sprawdzać, jakie wartości p̂
      wychodzą. Panel zaczyna z ukrytym parametrem, tak jak w rzeczywistości.
      Można go odsłonić, gdy uzbiera się kilkanaście prób."),

    figure_panel(
      label = "Ryc. 3.1",
      title = "Odsetek pracujących w kolejnych próbach",
      width_mode = "wide",
      lc_toolbar(
        lc_slider("ch3_n", "Liczebność próby (n)", 10, 400, 50, 10),
        lc_action_group(label = "Losuj próby",
          ch3_draw_1 = "+1", ch3_draw_10 = "+10", ch3_draw_100 = "+100"),
        lc_action("ch3_reset", icon = "reset", variant = "ghost",
                  aria_label = "Wyczyść próby"),
        lc_segmented("ch3_reveal", "Parametr p", choices = c(
          "Ukryty"     = "hide",
          "Odsłonięty" = "show"
        ), selected = "hide"),
        lc_readouts(uiOutput("ch3_reads"))
      ),
      conditionalPanel("!output.ch3_has_draws",
        lc_empty("Wylosuj pierwszą próbę, żeby zobaczyć jej p̂")),
      conditionalPanel("output.ch3_has_draws",
        lc_plot("ch3_draws_plot", ratio = "2.2/1")
      )
    ),

    lc_p("Prawdziwy odsetek pracujących na wydziale wynosi p = ",
      paste0(lc_fmt(pop_p, 3), ". Przy n = 50 kolejne wartości p̂ wypadają typowo
      w przedziale od około 0.24 do 0.52, a mniej więcej co siódma próba
      myli się o więcej niż 0.1. Parametr się nie zmienia: to ta sama
      populacja i ta sama liczba. Zmienia się tylko próba, a razem z nią
      statystyka. To zjawisko nazywamy "),
      gloss("zmienność próbkowa", "zmiennością próbkową"), "."),

    lc_p("Przy n = 200 te same wartości skupiają się ciaśniej, mniej więcej
      między 0.32 a 0.44, a pomyłka większa niż 0.1 praktycznie się
      nie zdarza. Większa próba nie usuwa zmienności próbkowej, ale ją
      zmniejsza. Wiedząc, jak duża jest ta zmienność, można z jednej próby
      powiedzieć, w jakim zakresie prawdopodobnie leży parametr. Na tym
      pomyśle zbudowane są przedziały ufności z wykładu 03."),

    lc_note("Zasada", rule = TRUE,
      "Parametr opisuje populację, jest stały i zwykle nieznany. Statystyka
       opisuje próbę, jest znana i zmienia się od próby do próby."
    ),

    lc_p("Wszystkie próby w tym rozdziale były losowane uczciwie: każda osoba
      z listy miała tę samą szansę. Wartości p̂ rozrzucały się wtedy
      po obu stronach p, bez przewagi jednej strony. Następny rozdział
      pokazuje, co się dzieje, gdy próba powstaje inaczej."),

    lc_chapter_next(
      num       = "04",
      title     = "Dobór próby",
      lead      = "kiedy duża próba nie pomaga",
      target_id = "ch-dobor"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  ch3_draws <- reactiveVal(numeric(0))

  draw_phat <- function(k) {
    n <- input$ch3_n %||% 50
    new <- replicate(k, mean(faculty$praca[sample(pop_N, n)]))
    ch3_draws(c(ch3_draws(), new))
  }

  observeEvent(input$ch3_draw_1, draw_phat(1))
  observeEvent(input$ch3_draw_10, draw_phat(10))
  observeEvent(input$ch3_draw_100, draw_phat(100))
  observeEvent(input$ch3_reset, ch3_draws(numeric(0)))
  observeEvent(input$ch3_n, ch3_draws(numeric(0)), ignoreInit = TRUE)

  output$ch3_has_draws <- reactive(length(ch3_draws()) > 0)
  outputOptions(output, "ch3_has_draws", suspendWhenHidden = FALSE)

  ch3_show <- reactive(identical(input$ch3_reveal, "show"))

  output$ch3_reads <- renderUI({
    d <- ch3_draws()
    tagList(
      lc_readout("prób", length(d)),
      lc_readout("ostatnie p̂", if (length(d)) lc_fmt(tail(d, 1), 3) else "–",
                 color = col_stat, swatch = TRUE),
      lc_readout("p", if (ch3_show()) lc_fmt(pop_p, 3) else "?",
                 color = col_param, swatch = TRUE)
    )
  })

  zoom_plot_server("ch3_draws_plot", reactive({
    d <- ch3_draws()
    req(length(d) > 0)
    df <- data.frame(i = seq_along(d), phat = d)
    last <- df[nrow(df), ]
    p <- ggplot(df, aes(i, phat)) +
      geom_point(color = col_stat, alpha = if (nrow(df) > 60) 0.5 else 0.85,
                 size = 2.2) +
      geom_point(data = last, color = col_stat, size = 4, shape = 21,
                 fill = "white", stroke = 1.5) +
      scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 0.1)) +
      scale_x_continuous(limits = c(0.5, max(20, nrow(df)) + 0.5)) +
      labs(x = "Numer próby", y = "p̂ (odsetek pracujących w próbie)")
    if (ch3_show()) {
      p <- p +
        geom_hline(yintercept = pop_p, color = col_param, linewidth = 1.1,
                   linetype = "dashed") +
        annotate("text", x = 0.5, y = pop_p, label = "p", hjust = -0.3,
                 vjust = -0.6, color = col_param, fontface = "bold", size = 5)
    }
    p
  }), alt = "Wartości p̂ z kolejnych prób na tle parametru p")
}
