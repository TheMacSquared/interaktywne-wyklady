# Punkt stop 13 — Przypadek · domknięcie „Serpuchów, policzony”
# Puenta: Pietrow policzył w głowie to, co sala liczy teraz na łańcuchu szans.
# Piaskownica na jawnych założeniach: prawdziwych liczb z 1983 roku nikt nie zna.
#
# Wstawienie: a3_stop_serpuchow_case_ui jako body sekcji (rozdział „sprawdzenie”),
# a3_stop_serpuchow_case_server(input, output, session) w alarm_server().

# Rachunek ---------------------------------------------------------------------

# Łańcuch szans według wzoru (3.8): szanse a priori 10^-k, alarm satelity mnoży
# je przez 10^m, milczenie radaru przez LR₋ = (1 − s) / 1. Liczymy w log10, żeby
# skrajne ustawienia (10⁻⁸, 10⁶) nie traciły precyzji.
a3_sp_chain <- function(k, m, s) {
  s <- min(max(s, 0), 0.99)
  log_odds <- c(-k, -k + m, -k + m + log10(1 - s))
  data.frame(
    stage = c("przed alarmem", "po satelicie", "po radarach"),
    x = 1:3,
    log_odds = log_odds,
    p = stats::plogis(log_odds * log(10))
  )
}

# Mnożnik do etykiety strzałki: „× 1000”, „× 3200”, „× 0.1”, „× 1”.
a3_sp_fmt_factor <- function(x) {
  if (x >= 1) {
    paste0("× ", format(signif(x, 2), big.mark = " ", scientific = FALSE, trim = TRUE))
  } else {
    paste0("× ", format(signif(x, 2), scientific = FALSE, trim = TRUE))
  }
}

# Etykiety osi y jako prawdopodobieństwa dla log10 szans (plotmath: 10^-6).
a3_sp_axis_labels <- function(log_odds) {
  txt <- vapply(log_odds, function(lo) {
    if (lo <= -3) paste0("10^", lo)
    else if (lo == -2) "'0.01'"
    else if (lo == 0) "'0.5'"
    else if (lo == 2) "'0.99'"
    else if (lo == 4) "'0.9999'"
    else paste0("'", format(signif(stats::plogis(lo * log(10)), 2), scientific = FALSE), "'")
  }, character(1))
  parse(text = txt)
}

# Wykres -------------------------------------------------------------------------

a3_sp_plot <- function(k = 5, m = 3, s = 0.9) {
  d <- a3_sp_chain(k, m, s)
  family <- ggplot2::theme_get()$text$family %||% ""

  gap <- 0.11
  arrows <- data.frame(
    x = d$x[1:2] + gap, xend = d$x[2:3] - gap,
    y = d$log_odds[1:2] + gap * diff(d$log_odds),
    yend = d$log_odds[2:3] - gap * diff(d$log_odds),
    label = c(a3_sp_fmt_factor(10^m), a3_sp_fmt_factor(1 - min(max(s, 0), 0.99)))
  )
  # Etykieta nad strzałką, po stronie, gdzie linia jest niżej: przy strzałce
  # w górę na lewo od środka, w dół na prawo, poziomej pośrodku.
  slope <- diff(d$log_odds)
  arrows$hjust <- ifelse(abs(slope) < 0.25, 0.5, ifelse(slope > 0, 1, 0))
  arrows$lx <- (d$x[1:2] + d$x[2:3]) / 2 - 0.03 * sign(slope) * (abs(slope) >= 0.25)
  arrows$ly <- (d$log_odds[1:2] + d$log_odds[2:3]) / 2 + 0.35

  # Skala stała od 10⁻⁸ do 0.9999, rozszerzana w dół tylko przy skrajnym
  # ustawieniu (k = 8 i czuły radar daje szanse do 10⁻¹⁰).
  y_min <- min(-8.6, min(d$log_odds) - 0.6)
  breaks <- seq(2 * floor(y_min / 2) + 2, 4, by = 2)

  ggplot(d, aes(x, log_odds)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = upwr_reference,
               linewidth = 0.7) +
    geom_segment(data = arrows, aes(x = x, xend = xend, y = y, yend = yend),
                 inherit.aes = FALSE, colour = upwr_secondary, linewidth = 1.1,
                 arrow = grid::arrow(length = grid::unit(0.16, "inches"), type = "closed")) +
    geom_text(data = arrows, aes(x = lx, y = ly, label = label, hjust = hjust),
              inherit.aes = FALSE, vjust = 0, family = family, size = 6.2,
              colour = upwr_ink) +
    geom_point(aes(colour = x == 3), size = 6) +
    scale_colour_manual(values = c(`FALSE` = upwr_secondary, `TRUE` = upwr_accent),
                        guide = "none") +
    scale_x_continuous(breaks = 1:3, labels = d$stage, limits = c(0.7, 3.3),
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = breaks, labels = a3_sp_axis_labels(breaks),
                       limits = c(y_min, 5.3), expand = c(0, 0)) +
    labs(x = NULL, y = "P(atak)") +
    theme_upwr(grid = "y") +
    theme(
      axis.text.x = element_text(size = rel(1.05), colour = upwr_ink,
                                 margin = margin(t = 6)),
      axis.text.y = element_text(size = rel(0.95)),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# UI -------------------------------------------------------------------------------

a3_stop_serpuchow_case_ui <- tagList(
  lc_p("Wracamy do nocy z 26 września 1983 roku. System Oko zgłasza pięć startów, Pietrow ma kilka minut i nie wierzy. Teraz mamy narzędzie, którego on nie miał: łańcuch szans ze wzoru (3.8). Liczby na suwakach to założenia do dyskusji, nie dane historyczne — prawdziwych wartości nikt nie zna."),
  lc_p("Pierwszy suwak to szanse a priori 1 : 10ᵏ, że akurat tej nocy trwa pierwszy atak, i to w postaci kilku rakiet, a nie setek. Drugi to iloraz wiarygodności alarmu satelity 10ᵐ: ile razy częściej taki alarm pojawia się przy ataku niż bez niego. Trzeci to czułość radarów naziemnych s. Radar widzi rakietę dopiero, gdy ta wzniesie się nad horyzont, więc milczenie nabierało wagi z każdą minutą. Radary milczały, a milczenie ma iloraz LR₋ = (1 − s) / 1, bo radar nie widzi rakiet, których nie ma."),
  risk_formula("\\frac{P(\\text{atak}\\mid \\text{alarm},\\,\\text{cisza})}{P(\\text{brak}\\mid \\text{alarm},\\,\\text{cisza})}=10^{-k}\\cdot 10^{m}\\cdot(1-s)"),
  risk_try("odczytaj posterior po samym satelicie przy ustawieniach domyślnych. Potem, bez patrzenia na radary, znajdź najmniejsze m, przy którym alarm przekracza przerywaną linię 0.5 — jak to m zależy od k? Na koniec zmniejszaj czułość radaru s do zera: kiedy milczenie radarów przestaje uspokajać?"),
  risk_widget_panel(
    "Przypadek", "Serpuchów, policzony: łańcuch szans",
    tagList(
      lc_slider("a3_sp_prior", "Szanse a priori 1 : 10ᵏ, k", 2, 8, 5, 0.5),
      lc_slider("a3_sp_lr", "LR satelity 10ᵐ, m", 0, 6, 3, 0.5),
      lc_slider("a3_sp_radar", "Czułość radaru s", 0, 0.99, 0.9, 0.01)
    ),
    plot_id = "a3_sp_plot", stats_id = "a3_sp_read",
    note = "Oś pionowa w skali log szans; przerywana linia to P(atak) = 0.5. Wszystkie liczby są założeniami.",
    ratio = "1.9/1"
  ),
  lc_p("Przy ustawieniach domyślnych szanse a priori wynoszą 1 : 100 000. Alarm satelity mnoży je przez 1000: 10⁻⁵ · 10³ = 10⁻², czyli P(atak | alarm) = 0.01 / 1.01 ≈ 0.0099. Milczenie radarów przy czułości 0.9 mnoży szanse jeszcze przez 0.1: 10⁻³, czyli posterior ≈ 0.001. Nawet bardzo dobry satelita nie wystarczy, gdy zdarzenie jest tak rzadkie: żeby przekroczyć 0.5 bez radarów, LR satelity musiałby przekroczyć odwrotność szans a priori, tu 100 000. Przy s = 0 radar nic nie wnosi (LR₋ = 1) i zostaje sam satelita."),
  lc_p("Mnożenie ilorazów jest tu uprawnione, bo dwa źródła mylą się z różnych powodów. Fałszywy alarm wywołało odbicie słońca od wysokich chmur, które czujnik podczerwieni satelity wziął za płomienie silników. Radar widzi obiekty w inny sposób i tego samego odblasku nie zobaczy, więc warunkowa niezależność z definicji 3.7 jest wiarygodna. Inaczej jest z pięcioma sygnałami satelity: powstały z jednej przyczyny, więc liczymy je jako jeden alarm, a nie pięć niezależnych."),
  lc_p("Zostaje próg reakcji. Obie pomyłki są katastrofalne: przeoczony atak i wojna rozpętana przez błąd czujnika. Przy posteriorze rzędu 0.001 odwet byłby prawie na pewno błędem, a meldunek o fałszywym alarmie — prawie na pewno słuszny. Pietrow nie liczył, ale ważył te same trzy rzeczy: jak rzadki jest taki atak, ile wart jest nowy system i co mówi drugie, niezależne źródło.")
)

# Serwer -----------------------------------------------------------------------------

a3_stop_serpuchow_case_server <- function(input, output, session) {
  params <- reactive(list(
    k = input$a3_sp_prior %||% 5,
    m = input$a3_sp_lr %||% 3,
    s = input$a3_sp_radar %||% 0.9
  ))
  chain <- reactive(do.call(a3_sp_chain, params()))

  zoom_plot_server("a3_sp_plot", reactive(do.call(a3_sp_plot, params())),
    alt = "Łańcuch szans w trzech etapach: przed alarmem, po satelicie i po radarach, na osi prawdopodobieństwa w skali logarytmicznej szans, ze strzałkami opisanymi mnożnikami.")

  output$a3_sp_read <- renderUI({
    d <- chain()
    tagList(
      lc_readout("Po satelicie", risk_fmt_p(d$p[2]), color = upwr_secondary),
      lc_readout("Po radarach", risk_fmt_p(d$p[3]), color = upwr_accent)
    )
  })
}
