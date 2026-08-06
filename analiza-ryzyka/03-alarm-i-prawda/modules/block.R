# Blok 03: Alarm i prawda -------------------------------------------------

alarm_quiz <- list(
  question = "Czujnik ma czułość 95%. Czy po alarmie prawdopodobieństwo awarii wynosi 95%?",
  choices = c(
    "Nie — zależy także od częstości bazowej i fałszywych alarmów" = "no",
    "Tak — czułość jest odpowiedzią na to pytanie" = "yes",
    "Tak, jeśli awarie są rzadkie" = "rare"
  ),
  correct = "no",
  explanation = "Czułość to P(alarm | awaria), a pytanie po alarmie dotyczy P(awaria | alarm)."
)

alarm_exercises <- c(
  "Bananpol: dla 10 000 zmian, częstości awarii 0,01, czułości 0,95 i FPR 0,05 policz, ile alarmów będzie prawdziwych.",
  "Diagnostyka: wyjaśnij, dlaczego dwóch czujników z tym samym zasilaniem nie wolno automatycznie traktować jako niezależnych.",
  "Transfer: zaproponuj naturalne częstości dla testu przesiewowego w medycynie i nazwij właściwy mianownik."
)

alarm_block <- list(
  id = "alarm", title = "Alarm i prawda",
  chapters = list(
    list(
      id = "intuicja", title = "Dobry czujnik, trudne pytanie",
      lead = "Odwrócenie warunku potrafi całkowicie zmienić odpowiedź.",
      sections = list(list(
        id = "pytanie", title = "Alarm na zmianie",
        text = "Czujnik wykrywa większość awarii, ale alarmuje też bez awarii. Najpierw oszacuj wiarygodność alarmu bez rachunku."
      )),
      widget = risk_vote_panel(
        "a3_vote", "a3_vote_feedback",
        "Po alarmie: jak duża jest szansa rzeczywistej awarii?",
        c("Około 95%" = "95", "Około 16%" = "16", "Nie da się określić bez częstości bazowej" = "base")
      )
    ),
    list(
      id = "detektor", title = "Język detektora",
      lead = "Czułość i odsetek fałszywych alarmów opisują dwa różne wiersze tablicy.",
      sections = list(list(
        id = "definicje", title = "Cztery wyniki",
        bullets = c("prawdziwie dodatni: awaria i alarm", "fałszywie dodatni: brak awarii, ale alarm", "fałszywie ujemny: awaria bez alarmu", "prawdziwie ujemny: brak awarii i brak alarmu")
      )),
      widget = figure_panel(
        label = "Tablica 2×2", title = "Zmień parametry detektora",
        fluidRow(
          column(
            4,
            sliderInput("a3_prev", "Częstość awarii", 0.001, 0.10, 0.01, 0.001),
            sliderInput("a3_sens", "Czułość", 0.50, 1, 0.95, 0.01),
            sliderInput("a3_fpr", "Fałszywie dodatnie", 0, 0.30, 0.05, 0.01)
          ),
          column(8, tableOutput("a3_table"))
        ), full_width = TRUE
      ),
      pitfall = "Wysoka czułość nie oznacza, że większość alarmów jest prawdziwa."
    ),
    list(
      id = "czestosci", title = "Naturalne częstości",
      lead = "Zamiast trzech procentów śledzimy konkretne zmiany produkcyjne.",
      sections = list(list(
        id = "mianownik", title = "Wszystkie alarmy",
        text = "Dla pytania po alarmie mianownikiem są prawdziwe i fałszywe alarmy razem."
      )),
      widget = risk_widget_panel("Symulacja", "10 000 zmian Bananpolu", tagList(
        p("Parametry są synchronizowane z tablicą 2×2."), uiOutput("a3_counts")
      ),
      plot_id = "a3_grid", height = "390px"
      )
    ),
    list(
      id = "bayes", title = "Od drzewa do Bayesa",
      lead = "Licznik jest jedną drogą, mianownik sumą wszystkich dróg kończących się alarmem.",
      formula = "P(A\\mid +)=\\frac{P(+\\mid A)P(A)}{P(+\\mid A)P(A)+P(+\\mid \\neg A)P(\\neg A)}",
      sections = list(list(
        id = "drogi", title = "Dwie drogi do alarmu",
        text = "Alarm może powstać po awarii albo bez awarii. Bayes porządkuje ich względne udziały."
      )),
      decision = "Komunikuj posterior wraz z liczebnościami, a nie samą czułość."
    ),
    list(
      id = "baza", title = "Pułapka częstości bazowej",
      lead = "Ten sam czujnik daje inną wiarygodność alarmu w innej populacji.",
      widget = risk_widget_panel(
        "Krzywa", "P(awaria | alarm) a częstość bazowa",
        tagList(
          sliderInput("a3_curve_sens", "Czułość", 0.5, 1, 0.95, 0.01),
          sliderInput("a3_curve_fpr", "FPR", 0.001, 0.20, 0.05, 0.001)
        ),
        "a3_curve", "a3_posterior"
      ),
      pitfall = "Porównywanie czujników bez podania populacji zastosowania bywa pozorne."
    ),
    list(
      id = "druga-informacja", title = "Druga informacja",
      lead = "Drugi alarm pomaga tylko w takim stopniu, w jakim wnosi nową informację.",
      sections = list(list(
        id = "niezaleznosc", title = "Założenie warunkowej niezależności",
        text = "Dwa czujniki mogą reagować na to samo zakłócenie lub utracić wspólne zasilanie."
      )),
      widget = figure_panel(
        label = "Porównanie", title = "Dwa alarmy",
        sliderInput("a3_dependence", "Udział wspólnego trybu fałszywego alarmu", 0, 1, 0, 0.05),
        uiOutput("a3_second"), full_width = TRUE
      ), extension = TRUE
    ),
    list(
      id = "reakcja", title = "Reakcja jest osobnym problemem",
      lead = "Posterior opisuje przekonanie; decyzja wymaga jeszcze konsekwencji.",
      sections = list(list(
        id = "macierz", title = "Macierz konsekwencji",
        bullets = c("alarmuj przy awarii — uniknięta szkoda", "alarmuj bez awarii — koszt postoju", "nie alarmuj przy awarii — możliwa katastrofa", "nie alarmuj bez awarii — brak działania")
      )),
      decision = "Ustal próg reakcji jawnie na podstawie kosztów i wykonalności, nie na podstawie samego posteriora."
    ),
    list(
      id = "sprawdzenie", title = "Ściąga i sprawdzenie",
      lead = "Pytanie → populacja → detektor → posterior → konsekwencje.",
      sections = list(list(
        id = "sciaga", title = "Ściąga",
        bullets = c("Pytanie: co oznacza alarm?", "Model: Bayes lub naturalne częstości", "Założenia: częstość bazowa, stabilne parametry, zależności", "Wynik: P(awaria | alarm)", "Interpretacja: nie jest automatyczną decyzją")
      )),
      widget = risk_assessment_ui("a3", alarm_quiz, alarm_exercises), duration = "15–20 min"
    )
  )
)

alarm_chapters <- risk_block_chapters(alarm_block)

alarm_server <- function(input, output, session) {
  checked <- reactiveVal(FALSE)
  observeEvent(input$a3_vote_check, checked(TRUE))
  output$a3_vote_feedback <- renderUI({
    req(checked())
    lc_feedback(
      type = if (identical(input$a3_vote, "16")) "ok" else "warning",
      tags$strong("Wynik dla danych bazowych:"), " około 16%. Najważniejsze jest jednak wskazanie częstości bazowej."
    )
  })
  detector <- reactive(risk_detector_counts(10000L, input$a3_prev, input$a3_sens, input$a3_fpr))
  output$a3_table <- renderTable(detector(), striped = TRUE, bordered = TRUE)
  output$a3_counts <- renderUI({
    d <- detector()
    positives <- sum(d$alarm)
    posterior <- d$alarm[1] / positives
    lc_stat_grid(lc_stat_box("Prawdziwe alarmy", d$alarm[1]),
      lc_stat_box("Fałszywe alarmy", d$alarm[2]),
      lc_stat_box("P(awaria | alarm)", risk_format_probability(posterior), color = upwr_accent),
      columns = 1
    )
  })
  grid_plot <- reactive({
    d <- detector()
    counts <- c(d$alarm[1], d$alarm[2], d$no_alarm[1], d$no_alarm[2])
    labels <- c("Awaria + alarm", "Brak awarii + alarm", "Awaria bez alarmu", "Brak awarii bez alarmu")
    dat <- data.frame(type = factor(rep(labels, counts), levels = labels))
    dat$id <- seq_len(nrow(dat))
    dat$x <- (dat$id - 1L) %% 100L
    dat$y <- (dat$id - 1L) %/% 100L
    ggplot(dat, aes(x, y, colour = type, shape = type)) +
      geom_point(size = .7) +
      scale_y_reverse() +
      coord_equal() +
      scale_colour_manual(values = upwr_cat_n(4)) +
      labs(title = "Każdy punkt to jedna zmiana", x = NULL, y = NULL, colour = "Wynik", shape = "Wynik") +
      theme_upwr() +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
  })
  zoom_plot_server("a3_grid", grid_plot, alt = "Siatka dziesięciu tysięcy zmian z prawdziwymi i fałszywymi alarmami.")
  curve_plot <- reactive({
    prevalence <- seq(.0001, .2, length.out = 300)
    dat <- data.frame(prevalence, posterior = vapply(prevalence, risk_bayes, numeric(1),
      sensitivity = input$a3_curve_sens, false_positive_rate = input$a3_curve_fpr
    ))
    ggplot(dat, aes(prevalence, posterior)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      geom_vline(xintercept = .01, linetype = 2, colour = upwr_reference) +
      labs(title = "Wiarygodność alarmu zależy od częstości awarii", x = "P(awarii)", y = "P(awarii | alarm)") +
      theme_upwr()
  })
  zoom_plot_server("a3_curve", curve_plot, alt = "Rosnąca krzywa wiarygodności alarmu względem częstości bazowej awarii.")
  output$a3_posterior <- renderUI(lc_stat_grid(lc_stat_box("Dla P(awarii)=0,01",
    risk_format_probability(risk_bayes(.01, input$a3_curve_sens, input$a3_curve_fpr)),
    color = upwr_accent
  ), columns = 1))
  output$a3_second <- renderUI({
    p1 <- risk_bayes(input$a3_prev, input$a3_sens, input$a3_fpr)
    independent <- risk_bayes(p1, input$a3_sens, input$a3_fpr)
    adjusted <- (1 - input$a3_dependence) * independent + input$a3_dependence * p1
    lc_stat_grid(lc_stat_box("Po jednym alarmie", risk_format_probability(p1)),
      lc_stat_box("Po dwóch alarmach", risk_format_probability(adjusted), color = upwr_accent),
      columns = 1
    )
  })
  risk_assessment_server("a3", alarm_quiz, input, output)
}
