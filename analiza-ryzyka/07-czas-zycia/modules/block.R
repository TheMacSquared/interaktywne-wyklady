# Blok 07: Czas życia elementu ------------------------------------------

zycie_quiz <- list(question = "Co oznacza stały hazard rozkładu wykładniczego?", choices = c("Chwilowe tempo awarii nie zależy od wieku działającego elementu" = "constant", "Każdy element żyje dokładnie tyle samo" = "same", "Ryzyko awarii zawsze rośnie" = "grow"), correct = "constant", explanation = "Brak pamięci dotyczy warunkowego ryzyka dalszego życia, nie identycznych czasów awarii.")
zycie_exercises <- c("Bananpol: dla MTTF=1500 h policz R(1000) w modelu wykładniczym.", "Diagnostyka: wskaż, dlaczego widoczne tylko zakończone awarie zaniżają oszacowany czas życia.", "Transfer: wybierz sensowny kształt Weibulla dla elementu zużywającego się i uzasadnij znak zmiany hazardu.")

zycie_block <- list(id = "zycie", title = "Czas życia elementu", chapters = list(
  list(id = "mttf", title = "Dwa urządzenia z tym samym MTTF", lead = "Ta sama średnia nie gwarantuje tej samej niezawodności w czasie misji.", widget = risk_vote_panel("c7_vote", "c7_vote_feedback", "Czy ten sam MTTF oznacza takie samo R(1000 h)?", c("Tak" = "yes", "Nie — znaczenie ma cały rozkład" = "distribution", "Tylko dla Weibulla" = "weibull"))),
  list(id = "cenzorowanie", title = "Oś obserwacji i cenzorowanie", lead = "Element nadal działający na końcu badania wnosi informację: jego czas życia jest co najmniej tak długi.", widget = risk_widget_panel("Oś czasu", "Awarie i obserwacje ucięte", sliderInput("c7_follow", "Koniec obserwacji (h)", 300, 2500, 1200, 50), "c7_timeline", "c7_timeline_stats"), pitfall = "Usunięcie działających elementów z danych systematycznie skraca obraz czasu życia."),
  list(id = "jezyk", title = "f(t), F(t), R(t) i h(t)", lead = "Cztery funkcje odpowiadają na różne pytania o ten sam czas życia.", formula = "R(t)=1-F(t),\\qquad h(t)=\\frac{f(t)}{R(t)}", widget = risk_widget_panel("Synchronizacja", "Wspólny suwak czasu", sliderInput("c7_time", "Czas t (h)", 0, 4000, 1000, 50), "c7_functions", "c7_functions_stats")),
  list(id = "wykladniczy", title = "Rozkład wykładniczy", lead = "Stały hazard daje model bez pamięci — użyteczny, lecz mechanicznie wymagający.", formula = "R(t)=e^{-\\lambda t},\\qquad MTTF=1/\\lambda", widget = risk_widget_panel("Model", "Stały hazard", sliderInput("c7_mttf", "MTTF (h)", 300, 4000, 1500, 50), "c7_exp", "c7_exp_stats"), pitfall = "Brak pamięci nie pasuje do wyraźnego docierania ani zużycia."),
  list(id = "gamma", title = "Gamma i Erlang", lead = "Czas do k-tego zdarzenia może być sumą kolejnych etapów o wykładniczych czasach.", widget = risk_widget_panel("Rozszerzenie", "Czas do k-tego zdarzenia", sliderInput("c7_k", "Liczba etapów k", 1, 8, 3, 1), "c7_gamma", "c7_gamma_stats"), extension = TRUE),
  list(id = "weibull", title = "Część B — mechanizm Weibulla", lead = "Parametr β opisuje kierunek zmiany hazardu, a η skalę czasu.", formula = "R(t)=\\exp[-(t/\\eta)^\\beta]", widget = risk_widget_panel("Model", "R(t) i h(t) reagują razem", tagList(sliderInput("c7_beta", "β", .4, 4, 2, .1), sliderInput("c7_eta", "η (h)", 300, 4000, 1700, 50)), "c7_weibull", "c7_weibull_stats")),
  list(id = "ten-sam-mttf", title = "Ten sam MTTF, inne R(t)", lead = "Kształt rozkładu wpływa na ryzyko misji, nawet gdy średnie czasy są zgodne.", widget = risk_widget_panel("Porównanie", "Modele skalibrowane do MTTF=1500 h", sliderInput("c7_mission", "Czas misji (h)", 100, 3000, 1000, 50), "c7_same_mean", "c7_same_mean_stats")),
  list(id = "wanna", title = "Krzywa wannowa to złożenie mechanizmów", lead = "Wczesne defekty, okres stabilny i zużycie tworzą trzy składowe hazardu.", widget = risk_widget_panel("Mechanizmy", "Suma trzech składowych", sliderInput("c7_wear", "Nasilenie zużycia", .2, 2, 1, .1), "c7_bathtub", "c7_bathtub_stats"), pitfall = "Pojedynczy Weibull ma hazard monotoniczny; nie tworzy pełnej krzywej wannowej."),
  list(id = "przeglad", title = "Plan przeglądu", lead = "Czas interwencji wynika z wymaganego R(t), kosztów i mechanizmu awarii.", widget = figure_panel(label = "Decyzja", title = "Czy wentylator dotrwa do końca misji?", sliderInput("c7_plan_time", "Czas do przeglądu (h)", 100, 3000, 1000, 50), uiOutput("c7_plan"), full_width = TRUE), decision = "Podaj model, czas misji i prawdopodobieństwo dotrwania; MTTF samo nie wystarcza."),
  list(id = "sciaga", title = "Ściąga", lead = "Czas → cenzorowanie → R(t) i h(t) → mechanizm → plan.", sections = list(list(id = "lista", title = "Pięć pytań", bullets = c("Co rozpoczyna i kończy czas życia?", "Jaki jest wspólny czas misji?", "Czy obserwacje działające są cenzorowane?", "Czy hazard jest stały, rośnie czy maleje?", "Jak wynik zmienia decyzję utrzymaniową?")))),
  list(id = "sprawdzenie", title = "Quiz i ćwiczenia", lead = "Interpretuj funkcje czasu życia bez estymacji parametrów.", widget = risk_assessment_ui("c7", zycie_quiz, zycie_exercises), duration = "15–20 min")
))
zycie_chapters <- risk_block_chapters(zycie_block)

zycie_server <- function(input, output, session) {
  v <- reactiveVal(FALSE)
  observeEvent(input$c7_vote_check, v(TRUE))
  output$c7_vote_feedback <- renderUI({
    req(v())
    lc_feedback(type = if (identical(input$c7_vote, "distribution")) "ok" else "warning", tags$strong("Nie."), " Rozkłady o tym samym MTTF mogą mieć odmienne R(t).")
  })
  times <- c(220, 480, 760, 990, 1350, 1750, 2300, 3100)
  timeline_plot <- reactive({
    obs <- pmin(times, input$c7_follow)
    status <- ifelse(times <= input$c7_follow, "Awaria", "Nadal działa — cenzorowanie")
    dat <- data.frame(id = factor(seq_along(times)), obs, status)
    ggplot(dat, aes(x = 0, xend = obs, y = id, yend = id, colour = status)) +
      geom_segment(linewidth = 2) +
      geom_point(aes(x = obs, shape = status), size = 3) +
      scale_colour_manual(values = c("Awaria" = upwr_accent, "Nadal działa — cenzorowanie" = upwr_secondary)) +
      labs(title = "Każdy element wnosi informację", x = "Czas (h)", y = "Element", colour = NULL, shape = NULL) +
      theme_upwr()
  })
  zoom_plot_server("c7_timeline", timeline_plot, alt = "Osiem linii czasu zakończonych awarią lub znacznikiem cenzorowania.")
  output$c7_timeline_stats <- renderUI(lc_stat_grid(lc_stat_box("Awarie", sum(times <= input$c7_follow)), lc_stat_box("Cenzorowane", sum(times > input$c7_follow)), columns = 1))
  functions_plot <- reactive({
    t <- seq(0, 4000, length.out = 400)
    e <- risk_exponential(t, 1 / 1500)
    dat <- rbind(data.frame(t, value = e$density * 1500, fun = "f(t) × 1500"), data.frame(t, value = e$cdf, fun = "F(t)"), data.frame(t, value = e$reliability, fun = "R(t)"), data.frame(t, value = e$hazard * 1500, fun = "h(t) × 1500"))
    ggplot(dat, aes(t, value, colour = fun)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = input$c7_time, linetype = 2) +
      scale_colour_manual(values = upwr_cat_n(4)) +
      labs(title = "Cztery perspektywy na czas życia", x = "Czas (h)", y = "Wartość przeskalowana", colour = NULL) +
      theme_upwr()
  })
  zoom_plot_server("c7_functions", functions_plot, alt = "Cztery zsynchronizowane funkcje czasu życia ze wspólną linią czasu.")
  output$c7_functions_stats <- renderUI({
    e <- risk_exponential(input$c7_time, 1 / 1500)
    lc_stat_grid(lc_stat_box("F(t)", risk_format_probability(e$cdf)), lc_stat_box("R(t)", risk_format_probability(e$reliability), color = upwr_accent), columns = 1)
  })
  exp_plot <- reactive({
    t <- seq(0, 5000, length.out = 400)
    r <- risk_exponential(t, 1 / input$c7_mttf)$reliability
    ggplot(data.frame(t, r), aes(t, r)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      labs(title = "Niezawodność wykładnicza", x = "Czas (h)", y = "R(t)") +
      theme_upwr()
  })
  zoom_plot_server("c7_exp", exp_plot, alt = "Malejąca wykładnicza krzywa niezawodności.")
  output$c7_exp_stats <- renderUI(lc_stat_grid(lc_stat_box("R(1000 h)", risk_format_probability(exp(-1000 / input$c7_mttf)), color = upwr_accent), columns = 1))
  gamma_plot <- reactive({
    t <- seq(0, 6000, length.out = 400)
    ggplot(data.frame(t, p = dgamma(t, shape = input$c7_k, rate = 1 / 500)), aes(t, p)) +
      geom_line(colour = upwr_secondary, linewidth = 1.1) +
      labs(title = "Czas do k-tego zdarzenia", x = "Czas (h)", y = "Gęstość") +
      theme_upwr()
  })
  zoom_plot_server("c7_gamma", gamma_plot, alt = "Gęstość czasu do k-tego zdarzenia dla rozkładu Erlanga.")
  output$c7_gamma_stats <- renderUI(lc_stat_grid(lc_stat_box("Średni czas", paste(input$c7_k * 500, "h")), columns = 1))
  weib_plot <- reactive({
    t <- seq(1, 5000, length.out = 500)
    w <- risk_weibull(t, input$c7_beta, input$c7_eta)
    dat <- rbind(data.frame(t, value = w$reliability, fun = "R(t)"), data.frame(t, value = w$hazard * input$c7_eta, fun = "h(t) × η"))
    ggplot(dat, aes(t, value, colour = fun)) +
      geom_line(linewidth = 1.05) +
      scale_colour_manual(values = upwr_cat_n(2)) +
      labs(title = "Niezawodność i hazard", x = "Czas (h)", y = "Wartość", colour = NULL) +
      theme_upwr()
  })
  zoom_plot_server("c7_weibull", weib_plot, alt = "Krzywe niezawodności i hazardu Weibulla sterowane parametrami beta i eta.")
  output$c7_weibull_stats <- renderUI(lc_stat_grid(lc_stat_box("Kierunek hazardu", if (input$c7_beta < 1) "maleje" else if (input$c7_beta > 1) "rośnie" else "stały"), lc_stat_box("R(1000 h)", risk_format_probability(risk_weibull(1000, input$c7_beta, input$c7_eta)$reliability), color = upwr_accent), columns = 1))
  same_plot <- reactive({
    t <- seq(0, 3500, length.out = 400)
    shapes <- c(.7, 1, 2.5)
    scales <- 1500 / gamma(1 + 1 / shapes)
    dat <- do.call(rbind, lapply(seq_along(shapes), function(i) data.frame(t, r = exp(-(t / scales[i])^shapes[i]), model = paste0("β=", shapes[i]))))
    ggplot(dat, aes(t, r, colour = model)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = input$c7_mission, linetype = 2) +
      scale_colour_manual(values = upwr_cat_n(3)) +
      labs(title = "Ten sam MTTF, inne R(t)", x = "Czas (h)", y = "R(t)", colour = NULL) +
      theme_upwr()
  })
  zoom_plot_server("c7_same_mean", same_plot, alt = "Trzy krzywe Weibulla o tym samym średnim czasie życia i różnych kształtach.")
  output$c7_same_mean_stats <- renderUI(lc_feedback(type = "info", "Odczytaj trzy różne wartości na pionowej linii czasu misji."))
  bathtub_plot <- reactive({
    t <- seq(1, 4000, length.out = 500)
    early <- 1.2 * exp(-t / 350)
    stable <- rep(.12, length(t))
    wear <- input$c7_wear * (t / 4000)^3
    dat <- data.frame(t, early, stable, wear, total = early + stable + wear)
    long <- reshape(dat, varying = c("early", "stable", "wear", "total"), v.names = "hazard", timevar = "mechanizm", times = c("Wczesne defekty", "Losowe awarie", "Zużycie", "Suma"), direction = "long")
    ggplot(long, aes(t, hazard, colour = mechanizm)) +
      geom_line(aes(linewidth = mechanizm == "Suma")) +
      scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = .7), guide = "none") +
      scale_colour_manual(values = upwr_cat_n(4)) +
      labs(title = "Wanna jako suma mechanizmów", x = "Czas", y = "Względny hazard", colour = NULL) +
      theme_upwr()
  })
  zoom_plot_server("c7_bathtub", bathtub_plot, alt = "Krzywa hazardu w kształcie wanny i jej trzy składowe.")
  output$c7_bathtub_stats <- renderUI(lc_feedback(type = "warning", "Zmiana mechanizmu wymaga innej interwencji utrzymaniowej."))
  output$c7_plan <- renderUI({
    r <- risk_weibull(input$c7_plan_time, 2, 1700)$reliability
    lc_stat_grid(lc_stat_box("R(t)", risk_format_probability(r), color = upwr_accent), lc_stat_box("Ryzyko awarii", risk_format_probability(1 - r)), columns = 1)
  })
  risk_assessment_server("c7", zycie_quiz, input, output)
}
