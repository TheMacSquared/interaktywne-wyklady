# PROTOTYPY PUNKTÓW STOP — rozdział 5 (2026-10-29) ---------------------------
#   s3p_5a_ui — Ryc. 5.7 „Rozkład normalny w sześciu kierunkach” (zaakceptowany 2026-10-29;
#               Chart.js, cases_chart.js; zastąpiła „Batonik 50 g”)
#   Usunięty 2026-10-29: 5b „Czarny poniedziałek 1987”.
#   (5c przeniesiony do stop_proto_ch5_z.R jako trzy warianty 5c-A/B/C)
#   s3p_5d_ui — Ryc. 5.8 „Normalny czy nie?” (zaakceptowany 2026-10-29; dane z ankiety)
# Serwer: s3p_ch5_server(input, output, session).

# ============================================================================
# (5a) GALERIA „ROZKŁAD NORMALNY W SZEŚCIU KIERUNKACH” (Chart.js, cases_chart.js)
# ============================================================================

# Modele orientacyjne; tail: lower = poniżej progu, upper = powyżej, outside = dalej niż |t − μ| od μ.
s3p_5a_cases <- list(
  list(short = "Kosmos", story = "Dron mapuje pole uprawne, a każde zdjęcie dostaje współrzędne z odbiornika satelitarnego, który myli się raz w jedną, raz w drugą stronę.",
       axis = "Błąd pozycji z satelity", unit = "m", mu = 0, sd = 1.5, t = 3, tail = "outside", step = 0.1,
       q = "Jak często pozycja z odbiornika satelitarnego myli się o więcej niż 3 m?"),
  list(short = "Środowisko", story = "Oczyszczalnia codziennie mierzy azotany w wodzie odprowadzanej do rzeki, a pozwolenie wodnoprawne dopuszcza najwyżej 10 mg/l.",
       axis = "Azotany w odpływie z oczyszczalni", unit = "mg/l", mu = 8, sd = 1.5, t = 10, tail = "upper", step = 0.1,
       q = "Jak często oczyszczalnia przekroczy limit 10 mg/l?"),
  list(short = "OZE", story = "Gmina spłaca kredyt na elektrownię wiatrową, a bank założył co najmniej 5 GWh produkcji rocznie, choć wietrzność zmienia się z roku na rok.",
       axis = "Roczna produkcja elektrowni wiatrowej", unit = "GWh", mu = 6, sd = 0.6, t = 5, tail = "lower", step = 0.1,
       q = "Jaka jest szansa, że w danym roku elektrownia wyprodukuje mniej niż 5 GWh?"),
  list(short = "Woda", story = "Zarząd zlewni planuje zbiorniki retencyjne na lata suche, bo rok z opadem poniżej 450 mm oznacza niedobór wody dla rolnictwa.",
       axis = "Roczny opad w zlewni", unit = "mm", mu = 600, sd = 100, t = 450, tail = "lower", step = 5,
       q = "Jak często zdarza się rok suchy, z opadem poniżej 450 mm?"),
  list(short = "Przestrzeń", story = "Gmina dzieli grunt na działki budowlane, ale granice wytycza się w terenie wzdłuż dróg i rowów, więc działki wychodzą różnej wielkości.",
       axis = "Powierzchnia działki po podziale", unit = "m²", mu = 1000, sd = 120, t = 750, tail = "lower", step = 10,
       q = "Jaki odsetek działek nie spełni minimum 750 m² z planu miejscowego?"),
  list(short = "Żywność", story = "Linia pakująca dozuje masę batoników ze średnią ustawioną na 50.5 g, a maszyna nigdy nie trafia idealnie.",
       axis = "Masa batonika", unit = "g", mu = 50.5, sd = 0.6, t = 50, tail = "lower", step = 0.05,
       q = "Jaki odsetek batoników waży mniej niż 50 g z etykiety?")
)

s3p_5a_ui <- s3p_panel("5a", "Ryc. 5.7", "Rozkład normalny w sześciu kierunkach", proto = FALSE,
  tags$div(class = "lc-cases", `data-config` = jsonlite::toJSON(s3p_5a_cases, auto_unbox = TRUE, digits = NA),
    lc_toolbar(
      lc_group("Kierunek", tags$div(class = "lc-seg", role = "group",
        lapply(seq_along(s3p_5a_cases), function(i) tags$button(type = "button", `data-c-case` = i - 1,
          `aria-pressed` = if (i == 1) "true" else "false", s3p_5a_cases[[i]]$short)))),
      lc_readouts(
        lc_readout("Model", tags$span(`data-role` = "model", "")),
        lc_readout("Próg", tags$span(`data-role` = "t", "")),
        lc_readout("P", tags$span(`data-role` = "p", ""), color = "var(--upwr-accent)")
      )
    ),
    tags$div(class = "lc-area-canvas", tags$canvas(`data-role` = "pdf", `aria-label` = "Gęstość rozkładu normalnego z zacienionym ogonem za progiem")),
    lc_caption(tags$span(`data-role` = "story", s3p_5a_cases[[1]]$story), " ",
               tags$span(`data-role` = "q", s3p_5a_cases[[1]]$q))
  )
)

# ============================================================================
# (5d) GALERIA „NORMALNY CZY NIE?”
# ============================================================================

s3p_5d_vars <- local({
  env <- new.env()
  sys.source(file.path(project_root, "R", "ankieta.R"), envir = env)
  s <- env$student_data
  list(
    hk  = list(lab = "Wzrost kobiet (cm)", x = s$wzrost[s$plec == "Kobieta"],
               note = "Dzwon jak z podręcznika: poza μ ± 2σ leży tyle, ile przewiduje model."),
    hall = list(lab = "Wzrost wszystkich (cm)", x = s$wzrost,
               note = "Mieszanka dwóch dzwonów (kobiety i mężczyźni): szczyt jest płaski i szeroki, choć ogony jeszcze pasują."),
    com = list(lab = "Czas dojazdu (min)", x = s$czas_dojazdu,
               note = "Prawy ogon: dojazdy powyżej 70 minut zdarzają się częściej, niż dopuszcza dzwon, a lewego ogona brak."),
    abs = list(lab = "Liczba nieobecności", x = s$liczba_nieobecnosci,
               note = "Zliczenia: wartości całkowite, ograniczone zerem, z prawym ogonem. Lepszym modelem jest Poisson."),
    exp = list(lab = "Wydatki miesięczne (zł)", x = s$wydatki,
               note = "Silny prawy ogon: model normalny przewidywałby ujemne wydatki. To kandydat na rozkład log-normalny."),
    gpa = list(lab = "Średnia ocen", x = s$srednia_ocen,
               note = "Prawie dzwon, ale ucięty przy 5.0: przy górnej granicy skali gromadzą się wyniki.")
  )
})

s3p_5d_ui <- s3p_panel("5d", "Ryc. 5.8", "Normalny czy nie?", proto = FALSE,
  lc_toolbar(
    lc_group("Zmienna z ankiety", lc_select("s3p_5d_var",
      choices = setNames(names(s3p_5d_vars), vapply(s3p_5d_vars, `[[`, "", "lab")))),
    lc_readouts(uiOutput("s3p_5d_read"))
  ),
  lc_plot("s3p_5d_plot", ratio = "2.4/1"),
  uiOutput("s3p_5d_caption")
)

# ============================================================================
# SERWER
# ============================================================================

s3p_ch5_server <- function(input, output, session) {

  # --- 5d -------------------------------------------------------------------
  var_5d <- reactive(s3p_5d_vars[[input$s3p_5d_var %||% "hk"]])

  output$s3p_5d_read <- renderUI({
    v <- var_5d()$x; m <- mean(v); s <- sd(v)
    tagList(lc_readout("Poza μ ± 2σ", s3p_pct(mean(abs(v - m) > 2 * s)), color = upwr_accent),
            lc_readout("Model normalny", "4.6%"),
            lc_readout("Skośność", lc_fmt(mean((v - m)^3) / s^3, 2)))
  })

  zoom_plot_server("s3p_5d_plot", reactive({
    it <- var_5d(); v <- it$x; m <- mean(v); s <- sd(v)
    x <- seq(min(v, m - 3.5 * s), max(v, m + 3.5 * s), length.out = 400)
    ggplot(data.frame(v = v), aes(v)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = upwr_secondary, alpha = 0.75, colour = "white") +
      geom_line(data = data.frame(x = x, y = dnorm(x, m, s)), aes(x, y), colour = upwr_accent, linewidth = 1.2) +
      labs(x = it$lab, y = "Gęstość", subtitle = "Krzywa: rozkład normalny o tej samej średniej i SD") +
      theme_upwr(grid = "y")
  }))

  output$s3p_5d_caption <- renderUI(lc_caption(var_5d()$note))
}
