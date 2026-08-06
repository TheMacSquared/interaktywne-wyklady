# Blok 08: Niezawodność systemu -----------------------------------------

system_quiz <- list(question = "Dla dwóch niezależnych gałęzi równoległych system zawodzi, gdy…", choices = c("zawiodą obie gałęzie" = "both", "zawiedzie dowolna jedna" = "one", "zawsze po średnim czasie życia" = "mean"), correct = "both", explanation = "W układzie równoległym sukces wymaga co najmniej jednej działającej gałęzi.")
system_exercises <- c("Bananpol: policz R systemu szeregowego dla R1=0,92, R2=0,95 i R3=0,98.", "Diagnostyka: wskaż, dlaczego wspólne zasilanie podważa zwykły rachunek redundancji.", "Transfer: zapisz logikę sukcesu systemu hamulcowego z dwiema niezależnymi gałęziami i wspólnym sterownikiem.")

system_block <- list(id = "system", title = "Niezawodność systemu", chapters = list(
  list(id = "intuicja", title = "Te same elementy, trzy odpowiedzi", lead = "Niezawodność systemu zależy od logiki sukcesu, nie tylko od listy części.", widget = risk_vote_panel("s8_vote", "s8_vote_feedback", "Dwa elementy mają R=0,9. Czy R systemu wynosi 0,81, 0,9 czy 0,99?", c("0,81" = "series", "0,90" = "single", "0,99" = "parallel"))),
  list(id = "definicja", title = "Sukces i wspólny czas misji", lead = "Najpierw definiujemy, co system ma zrobić i przez jak długi czas.", sections = list(list(id = "check", title = "Kontrakt modelu", bullets = c("jednoznaczna funkcja systemu", "ten sam horyzont dla wszystkich R_i", "stany elementów adekwatne do funkcji", "jawne zależności i wspólne zasoby"))), pitfall = "Nie wolno mnożyć niezawodności podanych dla różnych czasów misji."),
  list(id = "szereg", title = "Układ szeregowy", lead = "System działa tylko wtedy, gdy działają wszystkie wymagane elementy.", formula = "R_s=\\prod_i R_i", widget = figure_panel(label = "Stany", title = "Wszystkie muszą działać", checkboxGroupInput("s8_series_states", "Działające elementy", choices = c("Czujnik" = "sensor", "Sterownik" = "controller", "Wentylator" = "fan"), selected = c("sensor", "controller", "fan")), uiOutput("s8_series_state"), full_width = TRUE)),
  list(id = "rownolegle", title = "Układ równoległy", lead = "Liczymy przez zdarzenie przeciwne: awarię wszystkich gałęzi.", formula = "R_p=1-\\prod_i(1-R_i)", widget = figure_panel(label = "Stany", title = "Co najmniej jedna gałąź musi działać", checkboxGroupInput("s8_parallel_states", "Działające wentylatory", choices = c("A" = "a", "B" = "b"), selected = c("a", "b")), uiOutput("s8_parallel_state"), full_width = TRUE)),
  list(id = "przelacznik", title = "Przełącznik architektury", lead = "Dla tych samych elementów zmienia się logika, wzór i wynik.", widget = risk_widget_panel("Architektura", "Te same R, inny system", tagList(selectInput("s8_arch", "Układ", c("Szeregowy" = "series", "Równoległy" = "parallel")), sliderInput("s8_r1", "R₁", .5, 1, .92, .01), sliderInput("s8_r2", "R₂", .5, 1, .95, .01)), "s8_arch_plot", "s8_arch_stats")),
  list(id = "mieszany", title = "Część B — mały układ mieszany", lead = "Redukujemy najpierw gałęzie równoległe, a potem łączymy wynik z elementem szeregowym.", formula = "R_{sys}=R_C[1-(1-R_A)(1-R_B)]", widget = figure_panel(label = "Krok po kroku", title = "Sterownik C oraz wentylatory A/B", actionButton("s8_step", "Pokaż następny krok", class = "lc-btn-primary"), uiOutput("s8_reduction"), full_width = TRUE)),
  list(id = "czas", title = "Systemowa R(t)", lead = "Krzywe elementów i całego systemu muszą używać tego samego czasu.", widget = risk_widget_panel("Czas", "Elementy i system mieszany", sliderInput("s8_mission", "Czas misji (h)", 100, 3000, 1000, 50), "s8_time_plot", "s8_time_stats")),
  list(id = "wspolna", title = "Jawna wspólna przyczyna", lead = "Utrata wspólnego zasilania jest osobnym zdarzeniem w architekturze.", formula = "R=(1-q)R_{bez\\ wspólnej\\ awarii}", widget = risk_widget_panel("Zależność", "Wspólne zasilanie", sliderInput("s8_common", "P(utraty wspólnego zasilania)", 0, .15, .01, .005), "s8_common_plot", "s8_common_stats"), pitfall = "Suwak korelacji nie zastępuje opisu mechanizmu wspólnej przyczyny."),
  list(id = "redundancja", title = "Malejąca korzyść redundancji", lead = "Kolejna gałąź poprawia R, lecz wnosi koszt i coraz mniejszy przyrost.", widget = risk_widget_panel("Trade-off", "Liczba gałęzi i koszt", tagList(sliderInput("s8_branches", "Liczba gałęzi", 1, 6, 2, 1), sliderInput("s8_branch_r", "R jednej gałęzi", .5, .99, .9, .01)), "s8_redundancy", "s8_redundancy_stats")),
  list(id = "poprawa", title = "Który element poprawić?", lead = "Ta sama poprawa elementu może mieć różną wartość systemową.", widget = figure_panel(label = "Porównanie", title = "Spadek ryzyka po poprawie R o 0,02", uiOutput("s8_improvement"), full_width = TRUE), decision = "Porównuj zmianę wyniku systemowego, koszt i wykonalność; nie wybieraj automatycznie najsłabszego elementu."),
  list(id = "sciaga", title = "Ściąga", lead = "Funkcja → misja → architektura → zależności → wynik.", sections = list(list(id = "lista", title = "Pięć kroków", bullets = c("Zdefiniuj sukces systemu", "Ustal wspólny czas misji", "Zredukuj logikę etapami", "Dodaj jawne wspólne przyczyny", "Sprawdź wrażliwość na interwencje")))),
  list(id = "sprawdzenie", title = "Quiz i ćwiczenia", lead = "Rachunek ma odzwierciedlać fizyczną architekturę.", widget = risk_assessment_ui("s8", system_quiz, system_exercises), duration = "15–20 min")
))
system_chapters <- risk_block_chapters(system_block)

system_server <- function(input, output, session) {
  v <- reactiveVal(FALSE)
  observeEvent(input$s8_vote_check, v(TRUE))
  output$s8_vote_feedback <- renderUI({
    req(v())
    lc_feedback(type = "info", tags$strong("Każda odpowiedź może być poprawna:"), " 0,81 dla szeregu, 0,90 dla pojedynczego wymagania i 0,99 dla redundancji równoległej.")
  })
  output$s8_series_state <- renderUI({
    ok <- length(input$s8_series_states) == 3
    lc_feedback(type = if (ok) "ok" else "warning", tags$strong(if (ok) "System działa." else "System nie działa."), " Układ szeregowy wymaga wszystkich elementów.")
  })
  output$s8_parallel_state <- renderUI({
    ok <- length(input$s8_parallel_states) >= 1
    lc_feedback(type = if (ok) "ok" else "warning", tags$strong(if (ok) "System działa." else "System nie działa."), " Wystarcza co najmniej jedna gałąź.")
  })
  arch_value <- reactive(if (input$s8_arch == "series") risk_series_reliability(c(input$s8_r1, input$s8_r2)) else risk_parallel_reliability(c(input$s8_r1, input$s8_r2)))
  arch_plot <- reactive({
    dat <- data.frame(element = c("Element 1", "Element 2", "System"), r = c(input$s8_r1, input$s8_r2, arch_value()))
    ggplot(dat, aes(element, r, fill = element)) +
      geom_col(width = .65) +
      coord_cartesian(ylim = c(0, 1)) +
      scale_fill_manual(values = upwr_cat_n(3), guide = "none") +
      labs(title = paste("Architektura", if (input$s8_arch == "series") "szeregowa" else "równoległa"), x = NULL, y = "Niezawodność") +
      theme_upwr()
  })
  zoom_plot_server("s8_arch_plot", arch_plot, alt = "Słupki niezawodności dwóch elementów i systemu dla wybranej architektury.")
  output$s8_arch_stats <- renderUI(lc_stat_grid(lc_stat_box("R systemu", risk_format_probability(arch_value()), color = upwr_accent), columns = 1))
  step <- reactiveVal(0L)
  observeEvent(input$s8_step, step((step() + 1L) %% 3L))
  output$s8_reduction <- renderUI({
    texts <- c("1. Zdefiniuj sukces: C działa oraz A lub B działa.", "2. Zredukuj A/B: R_AB=1−(1−R_A)(1−R_B).", "3. Połącz szeregowo: R_sys=R_C·R_AB.")
    lc_feedback(type = "info", texts[[step() + 1L]])
  })
  time_plot <- reactive({
    t <- seq(0, 3000, length.out = 400)
    ra <- exp(-t / 1800)
    rb <- exp(-t / 2000)
    rc <- exp(-t / 2500)
    rs <- rc * (1 - (1 - ra) * (1 - rb))
    dat <- rbind(data.frame(t, r = ra, name = "Wentylator A"), data.frame(t, r = rb, name = "Wentylator B"), data.frame(t, r = rc, name = "Sterownik"), data.frame(t, r = rs, name = "System"))
    ggplot(dat, aes(t, r, colour = name)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = input$s8_mission, linetype = 2) +
      scale_colour_manual(values = upwr_cat_n(4)) +
      labs(title = "Wspólny czas dla elementów i systemu", x = "Czas (h)", y = "R(t)", colour = NULL) +
      theme_upwr()
  })
  zoom_plot_server("s8_time_plot", time_plot, alt = "Krzywe niezawodności trzech elementów i systemu mieszanego.")
  output$s8_time_stats <- renderUI({
    t <- input$s8_mission
    rs <- exp(-t / 2500) * (1 - (1 - exp(-t / 1800)) * (1 - exp(-t / 2000)))
    lc_stat_grid(lc_stat_box("R systemu", risk_format_probability(rs), color = upwr_accent), columns = 1)
  })
  common_plot <- reactive({
    q <- seq(0, .15, length.out = 200)
    base <- risk_parallel_reliability(c(.92, .95))
    ggplot(data.frame(q, r = (1 - q) * base), aes(q, r)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      geom_point(data = data.frame(q = input$s8_common, r = (1 - input$s8_common) * base), colour = upwr_secondary, size = 3) +
      labs(title = "Wspólna przyczyna ogranicza redundancję", x = "P(wspólnej awarii)", y = "R systemu") +
      theme_upwr()
  })
  zoom_plot_server("s8_common_plot", common_plot, alt = "Malejąca niezawodność układu redundantnego wraz ze wzrostem wspólnej przyczyny.")
  output$s8_common_stats <- renderUI(lc_stat_grid(lc_stat_box("R z przyczyną wspólną", risk_format_probability(risk_common_cause_reliability(c(.92, .95), input$s8_common)), color = upwr_accent), columns = 1))
  redundancy_plot <- reactive({
    n <- 1:6
    r <- 1 - (1 - input$s8_branch_r)^n
    dat <- data.frame(n, r, cost = n * 100)
    ggplot(dat, aes(n, r)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      geom_point() +
      geom_point(data = dat[dat$n == input$s8_branches, ], colour = upwr_secondary, size = 4) +
      labs(title = "Przyrost niezawodności maleje", x = "Liczba gałęzi", y = "R systemu") +
      theme_upwr()
  })
  zoom_plot_server("s8_redundancy", redundancy_plot, alt = "Krzywa niezawodności równoległej względem liczby gałęzi.")
  output$s8_redundancy_stats <- renderUI({
    r <- 1 - (1 - input$s8_branch_r)^input$s8_branches
    lc_stat_grid(lc_stat_box("R", risk_format_probability(r), color = upwr_accent), lc_stat_box("Koszt demonstracyjny", paste(input$s8_branches * 100, "jedn.")), columns = 1)
  })
  output$s8_improvement <- renderUI({
    base <- c(.92, .95, .98)
    sys0 <- risk_series_reliability(base)
    gains <- vapply(seq_along(base), function(i) {
      x <- base
      x[i] <- min(1, x[i] + .02)
      risk_series_reliability(x) - sys0
    }, numeric(1))
    lc_stat_grid(lc_stat_box("Czujnik", risk_format_probability(gains[1])), lc_stat_box("Sterownik", risk_format_probability(gains[2])), lc_stat_box("Zasilanie", risk_format_probability(gains[3])), columns = 1)
  })
  risk_assessment_server("s8", system_quiz, input, output)
}
