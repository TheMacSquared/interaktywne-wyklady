warunki_quiz <- list(
  question = "Co zmienia warunek B w prawdopodobieństwie P(A | B)?",
  choices = c(
    "Populację odniesienia" = "den", "Tylko licznik" = "num",
    "Nazwę zdarzenia A" = "name"
  ),
  correct = "den",
  explanation = "Warunek filtruje świat do przypadków spełniających B; w tej populacji liczymy A."
)

warunki_exercises <- c(
  "Bananpol: policz P(incydent) z dwóch trybów pracy i zapisz wynik jako częstość na 1000 zmian.",
  "Diagnostyka: wskaż, dlaczego wspólne zasilanie narusza założenie niezależności dwóch zabezpieczeń.",
  "Transfer: opisz warunek i właściwy mianownik dla ryzyka wypadku podczas pracy nocnej."
)

warunki_vote <- risk_vote_panel(
  "w2_vote", "w2_vote_feedback",
  "Po wykryciu przegrzania: która liczba opisuje możliwość incydentu?",
  c(
    "P(incydent)" = "marginal", "P(incydent | przegrzanie)" = "conditional",
    "P(przegrzanie | incydent)" = "reverse"
  )
)

warunki_filter_widget <- risk_widget_panel(
  title = "Filtrujemy 1000 zmian Bananpolu",
  controls = tagList(
    sliderInput("w2_share", "Udział zmian z przegrzaniem", 0.02, 0.40, 0.10, 0.01),
    sliderInput("w2_risk_hot", "P(incydent | przegrzanie)", 0.01, 0.30, 0.12, 0.01),
    sliderInput("w2_risk_normal", "P(incydent | brak przegrzania)", 0, 0.05, 0.005, 0.001)
  ),
  plot_id = "w2_filter_plot", stats_id = "w2_filter_stats",
  note = "Każdy punkt oznacza jedną porównywalną zmianę. Kolor i kształt rozróżniają grupy."
)

warunki_views_widget <- figure_panel(
  label = "Trzy reprezentacje", title = "Te same liczby: tabela, drzewo i udziały",
  fluidRow(
    column(5, tableOutput("w2_table")),
    column(7, zoom_plot_ui("w2_views_plot", height = "390px"))
  ),
  lc_feedback(type = "info", "Zmiana reprezentacji nie zmienia zdarzenia ani mianownika."),
  full_width = TRUE
)

warunki_total_widget <- risk_widget_panel(
  title = "Dwie drogi do incydentu",
  controls = tagList(
    sliderInput("w2_mode_share", "Udział pracy w przeciążeniu", 0, 1, 0.20, 0.01),
    sliderInput("w2_overload", "P(incydent | przeciążenie)", 0, 0.40, 0.15, 0.01),
    sliderInput("w2_regular", "P(incydent | normalna praca)", 0, 0.10, 0.01, 0.005)
  ),
  plot_id = "w2_total_plot", stats_id = "w2_total_stats",
  note = "Wynik jest ważoną sumą dwóch rozłącznych dróg."
)

warunki_common_widget <- risk_widget_panel(
  title = "Niezależność kontra wspólna przyczyna",
  controls = tagList(
    sliderInput("w2_component_fail", "P awarii pojedynczego zabezpieczenia", 0.001, 0.20, 0.05, 0.001),
    sliderInput("w2_common", "P utraty wspólnego zasilania", 0, 0.10, 0.01, 0.001)
  ),
  plot_id = "w2_common_plot", stats_id = "w2_common_stats",
  note = "Wspólna przyczyna jest osobnym zdarzeniem, a nie nieobjaśnioną korelacją."
)

warunki_block <- list(
  id = "warunki", title = "Warunki zmieniają ocenę",
  chapters = list(
    list(
      id = "pytanie", title = "Która liczba odpowiada na pytanie", lead = "Zanim policzymy, nazywamy warunek i populację odniesienia.",
      sections = list(list(id = "sens", title = "Trzy podobne zapisy", text = "P(A), P(A | B) i P(B | A) odpowiadają na trzy różne pytania. W Bananpolu A oznacza incydent, a B przegrzanie.")), widget = warunki_vote,
      pitfall = "P(A | B) i P(B | A) zwykle nie są równe."
    ),
    list(
      id = "filtr", title = "Filtrujemy świat", lead = "Warunek zmienia mianownik, nie przeszłość.",
      sections = list(list(id = "mianownik", title = "Naturalne częstości", text = "Najpierw dzielimy 1000 zmian na te z przegrzaniem i bez niego, a dopiero potem zliczamy incydenty.")),
      formula = "P(A\\mid B)=\\frac{P(A\\cap B)}{P(B)}", widget = warunki_filter_widget
    ),
    list(
      id = "reprezentacje", title = "Jedna sytuacja, trzy reprezentacje", lead = "Tabela i wykres są różnymi mapami tych samych liczebności.",
      sections = list(list(id = "czytanie", title = "Czytaj od mianownika", bullets = c("Wiersz B wyznacza populację warunkową.", "Komórka A i B jest licznikiem.", "Suma wszystkich dróg prowadzących do A daje P(A)."))), widget = warunki_views_widget
    ),
    list(
      id = "iloczyn", title = "Mnożymy wzdłuż drogi", lead = "Prawdopodobieństwo wspólnej drogi powstaje przez iloczyn kolejnych etapów.",
      sections = list(list(id = "droga", title = "Przegrzanie i incydent", text = "Najpierw losujemy zmianę z przegrzaniem, następnie incydent w obrębie tej grupy.")),
      formula = "P(A\\cap B)=P(B)P(A\\mid B)", decision = "Reguła iloczynu opisuje drogę, ale nie uzasadnia niezależności."
    ),
    list(
      id = "calkowite", title = "Sumujemy rozłączne drogi", lead = "Incydent może powstać podczas pracy normalnej albo przeciążenia.",
      sections = list(list(id = "partycja", title = "Kompletna partycja", text = "Tryby muszą być rozłączne i obejmować wszystkie analizowane zmiany.")),
      formula = "P(A)=\\sum_i P(B_i)P(A\\mid B_i)", widget = warunki_total_widget
    ),
    list(
      id = "niezaleznosc", title = "Niezależność wymaga uzasadnienia", lead = "Dwa urządzenia nie stają się niezależne tylko dlatego, że są dwa.",
      sections = list(list(id = "wspolna", title = "Wspólne zasilanie", text = "Utrata wspólnego zasilania może jednocześnie wyłączyć obie gałęzie i zniwelować redundancję.")),
      widget = warunki_common_widget, pitfall = "P(A ∩ B)=P(A)P(B) wolno użyć dopiero po uzasadnieniu niezależności."
    ),
    list(
      id = "decyzja", title = "Warunek w decyzji", lead = "Działanie kierujemy tam, gdzie warunek istotnie zmienia ocenę.",
      sections = list(list(id = "ranking", title = "Co sprawdzić najpierw", bullets = c("Nazwij zdarzenie i warunek.", "Porównaj P(A) z P(A | B).", "Sprawdź, czy warunek jest wskaźnikiem, czy możliwą przyczyną."))),
      decision = "Przegrzanie uzasadnia dodatkową kontrolę, ale sam związek warunkowy nie dowodzi przyczynowości."
    ),
    list(
      id = "sprawdzenie", title = "Ściąga, quiz i ćwiczenia", lead = "Filtruj mianownik, mnóż wzdłuż drogi i sumuj rozłączne drogi.",
      sections = list(list(id = "sciaga", title = "Checklista", bullets = c("Co jest A i B?", "Jaki jest mianownik?", "Czy drogi są rozłączne?", "Czy niezależność została uzasadniona?"))),
      widget = risk_assessment_ui("w2", warunki_quiz, warunki_exercises), duration = "15–20 min"
    )
  )
)

warunki_chapters <- risk_block_chapters(warunki_block)

warunki_server <- function(input, output, session) {
  vote_checked <- reactiveVal(FALSE)
  observeEvent(input$w2_vote_check, vote_checked(TRUE))
  output$w2_vote_feedback <- renderUI({
    req(vote_checked())
    correct <- identical(input$w2_vote, "conditional")
    lc_feedback(
      type = if (correct) "ok" else "warning",
      tags$strong(if (correct) "Tak." else "Nie."),
      " Po wykryciu przegrzania właściwym mianownikiem są zmiany z przegrzaniem."
    )
  })

  counts <- reactive(risk_conditional_counts(
    1000L, input$w2_share, input$w2_risk_hot, input$w2_risk_normal
  ))
  filter_plot <- reactive({
    d <- counts()
    statuses <- c(rep("Incydent", sum(d$event)), rep("Brak incydentu", sum(d$no_event)))
    groups <- c(
      rep("Przegrzanie", d$event[1]), rep("Brak przegrzania", d$event[2]),
      rep("Przegrzanie", d$no_event[1]), rep("Brak przegrzania", d$no_event[2])
    )
    grid <- data.frame(id = seq_len(1000), status = statuses, group = groups)
    grid$x <- (grid$id - 1L) %% 50L + 1L
    grid$y <- (grid$id - 1L) %/% 50L + 1L
    ggplot(grid, aes(x, y, colour = status, shape = group)) +
      geom_point(size = 1.6) +
      scale_y_reverse() +
      coord_equal() +
      scale_colour_manual(values = c("Incydent" = upwr_accent, "Brak incydentu" = upwr_reference)) +
      labs(title = "1000 porównywalnych zmian", x = NULL, y = NULL, colour = "Wynik", shape = "Warunek") +
      theme_upwr() +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
  })
  zoom_plot_server("w2_filter_plot", filter_plot,
    alt = "Siatka 1000 zmian rozróżniająca incydenty oraz zmiany z przegrzaniem."
  )
  output$w2_filter_stats <- renderUI({
    d <- counts()
    p_all <- sum(d$event) / sum(d$total)
    lc_stat_grid(
      lc_stat_box("P(incydent)", risk_format_probability(p_all)),
      lc_stat_box("P(incydent | przegrzanie)", risk_format_probability(d$event[1] / d$total[1]), color = upwr_accent),
      columns = 1
    )
  })
  output$w2_table <- renderTable(
    {
      counts()
    },
    striped = TRUE,
    bordered = TRUE
  )
  views_plot <- reactive({
    d <- counts()
    long <- data.frame(
      group = rep(d$condition, each = 2), outcome = rep(c("Incydent", "Brak incydentu"), 2),
      count = c(d$event[1], d$no_event[1], d$event[2], d$no_event[2])
    )
    ggplot(long, aes(group, count, fill = outcome)) +
      geom_col(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = c("Incydent" = upwr_accent, "Brak incydentu" = upwr_reference)) +
      labs(title = "Udziały w dwóch mianownikach", x = NULL, y = "Udział", fill = "Wynik") +
      theme_upwr()
  })
  zoom_plot_server("w2_views_plot", views_plot,
    alt = "Dwa słupki pokazujące udział incydentów z warunkiem i bez warunku."
  )

  total_plot <- reactive({
    s <- seq(0, 1, length.out = 201)
    y <- s * input$w2_overload + (1 - s) * input$w2_regular
    ggplot(data.frame(share = s, p = y), aes(share, p)) +
      geom_line(colour = upwr_accent, linewidth = 1) +
      geom_point(
        data = data.frame(
          share = input$w2_mode_share,
          p = risk_total_probability(input$w2_mode_share, input$w2_overload, input$w2_regular)
        ),
        size = 3, colour = upwr_secondary
      ) +
      labs(title = "Suma dwóch dróg", x = "Udział pracy w przeciążeniu", y = "P(incydent)") +
      theme_upwr()
  })
  zoom_plot_server("w2_total_plot", total_plot,
    alt = "Prawdopodobieństwo incydentu rosnące wraz z udziałem pracy w przeciążeniu."
  )
  output$w2_total_stats <- renderUI({
    p <- risk_total_probability(input$w2_mode_share, input$w2_overload, input$w2_regular)
    lc_stat_grid(lc_stat_box("P(incydent)", risk_format_probability(p), color = upwr_accent),
      lc_stat_box("Częstość", risk_natural_frequency(p)),
      columns = 1
    )
  })

  common_plot <- reactive({
    independent <- input$w2_component_fail^2
    with_common <- input$w2_common + (1 - input$w2_common) * independent
    ggplot(data.frame(
      model = c("Tylko niezależne awarie", "Jawna wspólna przyczyna"),
      p = c(independent, with_common)
    ), aes(model, p, fill = model)) +
      geom_col(width = .6) +
      scale_fill_manual(values = upwr_cat_n(2), guide = "none") +
      labs(title = "P jednoczesnej utraty dwóch zabezpieczeń", x = NULL, y = "P(awarii)") +
      theme_upwr()
  })
  zoom_plot_server("w2_common_plot", common_plot,
    alt = "Porównanie prawdopodobieństwa awarii dwóch zabezpieczeń bez i ze wspólną przyczyną."
  )
  output$w2_common_stats <- renderUI({
    independent <- input$w2_component_fail^2
    with_common <- input$w2_common + (1 - input$w2_common) * independent
    lc_stat_grid(lc_stat_box("Model niezależny", risk_format_probability(independent)),
      lc_stat_box("Ze wspólną przyczyną", risk_format_probability(with_common), color = upwr_accent),
      columns = 1
    )
  })
  risk_assessment_server("w2", warunki_quiz, input, output)
}
