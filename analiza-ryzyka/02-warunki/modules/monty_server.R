# Gra Monty Halla, symulacja i wykres wyników.
warunki_monty_server <- function(input, output, session) {
  monty <- reactiveValues(
    prize = sample.int(3L, 1L),
    chosen = NULL,
    opened = NULL,
    final = NULL,
    strategy = NULL
  )

  choose_monty_door <- function(door) {
    monty$chosen <- as.integer(door)
    possible_zonks <- setdiff(seq_len(3L), c(monty$chosen, monty$prize))
    # Indeksowanie zamiast sample(x, 1): dla jednoelementowego x sample() losowałoby z 1:x.
    monty$opened <- possible_zonks[sample.int(length(possible_zonks), 1L)]
    monty$final <- NULL
    monty$strategy <- NULL
  }

  observeEvent(input$w2_monty_door_1, choose_monty_door(1L))
  observeEvent(input$w2_monty_door_2, choose_monty_door(2L))
  observeEvent(input$w2_monty_door_3, choose_monty_door(3L))

  observeEvent(input$w2_monty_stay, {
    req(monty$opened)
    monty$final <- monty$chosen
    monty$strategy <- "pozostanie"
  })

  observeEvent(input$w2_monty_switch, {
    req(monty$opened)
    monty$final <- setdiff(seq_len(3L), c(monty$chosen, monty$opened))
    monty$strategy <- "zmiana"
  })

  observeEvent(input$w2_monty_new, {
    monty$prize <- sample.int(3L, 1L)
    monty$chosen <- NULL
    monty$opened <- NULL
    monty$final <- NULL
    monty$strategy <- NULL
    monty_sim$n <- 0L
    monty_sim$wins_stay <- 0L
    monty_sim$wins_switch <- 0L
  })

  output$w2_monty_controls <- renderUI({
    if (is.null(monty$chosen)) {
      return(tagList(
        tags$div(class = "lc-eyebrow", "Krok 1 z 3"),
        tags$h4("Wybierz jedną bramkę"),
        tags$p("Za jedną jest nagroda, za dwiema pozostałymi — Zonk."),
        lc_toolbar(
          lc_action("w2_monty_door_1", "Bramka 1", variant = "solid"),
          lc_action("w2_monty_door_2", "Bramka 2", variant = "solid"),
          lc_action("w2_monty_door_3", "Bramka 3", variant = "solid")
        )
      ))
    }
    if (is.null(monty$final)) {
      return(tagList(
        tags$div(class = "lc-eyebrow", "Krok 2 z 3"),
        tags$h4(paste("Wybrałeś bramkę", monty$chosen)),
        tags$p(paste("Prowadzący wiedział, gdzie jest nagroda, i odsłonił Zonka za bramką", monty$opened, ".")),
        tags$p("Co robisz z nową informacją?"),
        lc_toolbar(
          lc_action("w2_monty_stay", "Zostaję przy wyborze", variant = "solid"),
          lc_action("w2_monty_switch", "Zmieniam bramkę", variant = "solid")
        )
      ))
    }
    tagList(
      tags$div(class = "lc-eyebrow", "Krok 3 z 3"),
      tags$h4("Sprawdź wynik i zagraj ponownie"),
      lc_action("w2_monty_new", "Nowa gra", variant = "outline")
    )
  })

  output$w2_monty_doors <- renderUI({
    cards <- lapply(seq_len(3L), function(door) {
      if (!is.null(monty$opened) && door == monty$opened) {
        card <- .monty_door_card(
          door, "zonk", "Zonk",
          "Prowadzący odsłonił tę bramkę", upwr_reference
        )
      } else if (!is.null(monty$final)) {
        card <- .monty_door_card(
          door,
          if (door == monty$prize) "car" else "zonk",
          if (door == monty$prize) "Nagroda" else "Zonk",
          if (door == monty$final) "Twój ostateczny wybór" else "Niewybrana bramka",
          if (door == monty$final) upwr_accent else upwr_reference
        )
      } else if (!is.null(monty$chosen) && door == monty$chosen) {
        card <- .monty_door_card(
          door, "chosen", "Twój wybór",
          "Bramka pozostaje zamknięta", upwr_accent
        )
      } else {
        card <- .monty_door_card(
          door, "closed", "Zamknięta",
          "Nagroda albo Zonk", upwr_secondary
        )
      }
      card
    })
    lc_stat_grid(columns = 3, cards)
  })

  output$w2_monty_feedback <- renderUI({
    if (is.null(monty$opened)) {
      return(NULL)
    }
    if (is.null(monty$final)) {
      return(lc_status(
               tags$strong("Nowa informacja:"),
               paste(" bramka", monty$opened, "na pewno nie zawiera nagrody. Zostajesz czy zmieniasz?")
             ))
    }
    won <- identical(monty$final, monty$prize)
    lc_status(
      lc_verdict(tags$strong(if (won) "Nagroda!" else "Zonk."), type = if (won) "ok" else "warning"),
      paste0(
        " Strategia: ", monty$strategy, ". Nagroda była za bramką ", monty$prize,
        ". Jedna gra nie rozstrzyga, która strategia jest lepsza — uruchom symulację."
      )
    )
  })

  monty_sim <- reactiveValues(n = 0L, wins_stay = 0L, wins_switch = 0L)

  output$w2_monty_simulation_panel <- renderUI({
    if (is.null(monty$final)) {
      return(NULL)
    }
    tagList(
      tags$div(class = "lc-eyebrow", "Eksperyment wielokrotny"),
      tags$h4("Czy wynik jednej gry był przypadkiem?"),
      tags$p("Dograj kolejne partie obiema strategiami naraz. Wyniki się sumują, więc zobacz, jak odsetek wygranych stabilizuje się wraz z liczbą gier."),
      lc_toolbar(
        lc_action("w2_monty_sim_1", "+1 gra", variant = "solid"),
        lc_action("w2_monty_sim_10", "+10 gier", variant = "solid"),
        lc_action("w2_monty_sim_100", "+100 gier", variant = "solid"),
        lc_action("w2_monty_sim_1000", "+1000 gier", variant = "solid"),
        lc_readouts(uiOutput("w2_monty_reads"))
      ),
      lc_plot("w2_monty_plot", ratio = "1.6/1", max_height = "390px"),
      uiOutput("w2_monty_note")
    )
  })

  output$w2_monty_reads <- renderUI({
    n <- monty_sim$n
    if (n == 0L) return(NULL)
    tagList(
      lc_readout("Gier", format(n, big.mark = " ")),
      lc_readout("Zostaję", scales::percent(monty_sim$wins_stay / n, accuracy = 0.1),
                 color = upwr_reference),
      lc_readout("Zmieniam", scales::percent(monty_sim$wins_switch / n, accuracy = 0.1),
                 color = upwr_accent)
    )
  })

  output$w2_monty_note <- renderUI({
    n <- monty_sim$n
    if (n == 0L) return(NULL)
    lc_caption(if (n < 30L) {
      "Przy kilku grach przypadek jeszcze rządzi — dograj więcej."
    } else {
      "Linie kropkowane: teoretyczne 1/3 i 2/3."
    })
  })

  add_monty_games <- function(n) {
    req(monty$final)
    prizes <- sample.int(3L, n, replace = TRUE)
    choices <- sample.int(3L, n, replace = TRUE)
    monty_sim$n <- monty_sim$n + n
    monty_sim$wins_stay <- monty_sim$wins_stay + sum(prizes == choices)
    monty_sim$wins_switch <- monty_sim$wins_switch + sum(prizes != choices)
  }

  observeEvent(input$w2_monty_sim_1, add_monty_games(1L))
  observeEvent(input$w2_monty_sim_10, add_monty_games(10L))
  observeEvent(input$w2_monty_sim_100, add_monty_games(100L))
  observeEvent(input$w2_monty_sim_1000, add_monty_games(1000L))

  monty_plot <- reactive({
    n <- monty_sim$n
    if (n == 0L) {
      return(
        ggplot() +
          annotate("text", x = 1, y = 0.55, label = "Dograj partie przyciskami powyżej", colour = upwr_secondary, size = 5) +
          coord_cartesian(xlim = c(0, 2), ylim = c(0, 1)) +
          labs(x = NULL, y = "Odsetek wygranych") +
          theme_upwr() +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      )
    }
    results <- data.frame(
      strategy = c("Zostaję", "Zmieniam"),
      wins = c(monty_sim$wins_stay, monty_sim$wins_switch)
    )
    results$win_rate <- results$wins / n
    results$label <- sprintf(
      "%s\n(%d z %d)", scales::percent(results$win_rate, accuracy = 0.1), results$wins, n
    )
    ggplot(results, aes(strategy, win_rate, fill = strategy)) +
      geom_col(width = 0.62) +
      geom_text(aes(label = label), vjust = -0.35, fontface = "bold", lineheight = 0.9) +
      geom_hline(yintercept = c(1 / 3, 2 / 3), colour = upwr_reference, linetype = "dotted", linewidth = 0.6) +
      scale_fill_manual(values = c("Zostaję" = upwr_reference, "Zmieniam" = upwr_accent), guide = "none") +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1.18), breaks = seq(0, 1, 0.25)) +
      labs(x = NULL, y = "Odsetek wygranych") +
      theme_upwr()
  })

  zoom_plot_server(
    "w2_monty_plot",
    monty_plot,
    alt = "Porównanie odsetka wygranych przy pozostaniu przy pierwszej bramce i przy zmianie bramki."
  )

}
