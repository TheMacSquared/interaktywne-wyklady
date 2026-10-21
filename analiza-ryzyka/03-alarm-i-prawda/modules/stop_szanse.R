# Punkty stop rozdziału „Częstość bazowa”:
#   - galeria „Ten sam mnożnik, różne miejsca” (szanse przed i po alarmie),
#   - wykres podwójnych alarmów do piaskownicy „Dwa alarmy”.
# Wymaga: palette.R, theme_upwr.R, shared.R, lecture_layout.R, risk_math.R.

# ============================================================================
# GALERIA: TEN SAM MNOŻNIK, RÓŻNE MIEJSCA
# ============================================================================

# Każdy wariant ma swój powód: A i B — ten sam czujnik, inna hala;
# test przesiewowy — ten sam start co hala A, inny mnożnik.
a3_odds_gallery_data <- function() {
  dat <- data.frame(
    variant = c("Hala A", "Hala B", "Test przesiewowy"),
    prevalence = c(.01, .05, .01),
    sensitivity = c(.95, .95, .90),
    false_positive_rate = c(.05, .05, .09)
  )
  dat$lr <- dat$sensitivity / dat$false_positive_rate
  dat$odds_before <- dat$prevalence / (1 - dat$prevalence)
  dat$odds_after <- dat$odds_before * dat$lr
  dat$p_after <- dat$odds_after / (1 + dat$odds_after)
  dat$variant <- factor(dat$variant, levels = rev(dat$variant))
  dat
}

a3_odds_gallery_plot <- function() {
  dat <- a3_odds_gallery_data()
  dat$mid <- sqrt(dat$odds_before * dat$odds_after)
  dat$lr_label <- paste0("×", format(round(dat$lr), trim = TRUE))
  dat$p_label <- sprintf("%.2f", dat$p_after)
  breaks <- c(1 / 99, 1 / 19, 1 / 9, 1 / 3, 1, 3)
  labels <- c("1:99", "1:19", "1:9", "1:3", "1:1", "3:1")

  ggplot(dat, aes(y = variant)) +
    geom_vline(xintercept = 1, linetype = 2, colour = upwr_reference, linewidth = 0.5) +
    geom_segment(
      aes(x = odds_before, xend = odds_after, yend = variant),
      colour = upwr_accent, linewidth = 1.6, lineend = "round", linejoin = "mitre",
      arrow = grid::arrow(length = grid::unit(0.16, "inches"), type = "closed")
    ) +
    geom_point(aes(x = odds_before), shape = 21, size = 4.2, stroke = 1.3,
               colour = upwr_secondary, fill = "#ffffff") +
    geom_text(aes(x = mid, label = lr_label), vjust = -1.1, size = 5,
              colour = upwr_accent, family = lc_mono_family) +
    geom_text(aes(x = odds_after, label = p_label), hjust = -0.45, size = 5.2,
              colour = upwr_ink, family = lc_mono_family) +
    scale_x_log10(breaks = breaks, labels = labels, limits = c(1 / 140, 3.2)) +
    labs(x = "Szanse zdarzenia (skala logarytmiczna)", y = NULL) +
    theme_upwr(grid = "x") +
    theme(
      axis.text.y = element_text(size = rel(1.05), colour = upwr_ink),
      axis.line.y = element_blank(), axis.ticks.y = element_blank()
    )
}

a3_stop_odds_gallery_ui <- figure_panel(
  label = "Galeria", title = "Ten sam mnożnik, różne miejsca",
  lc_plot("a3_odds_gallery", ratio = "2.2/1", ratio_narrow = "1.2/1", max_height = "400px"),
  full_width = TRUE
)

# ============================================================================
# DWA ALARMY: PODWÓJNE ALARMY NA 10 000 ZMIAN
# ============================================================================

# Model jak w risk_two_alarm_posterior(): P(oba | stan) = d·p + (1 − d)·p².
a3_double_counts <- function(prevalence, sensitivity, false_positive_rate,
                             dependence = 0, population = 10000) {
  joint <- function(p, d) d * p + (1 - d) * p^2
  data.frame(
    type = factor(c("Prawdziwe", "Fałszywe"), levels = c("Prawdziwe", "Fałszywe")),
    count = c(
      population * prevalence * joint(sensitivity, dependence),
      population * (1 - prevalence) * joint(false_positive_rate, dependence)
    ),
    independent = c(
      population * prevalence * sensitivity^2,
      population * (1 - prevalence) * false_positive_rate^2
    ),
    ceiling = c(
      population * prevalence * sensitivity,
      population * (1 - prevalence) * false_positive_rate
    )
  )
}

# Para słupków w każdej grupie: przerywany kontur — czujniki niezależne (d = 0),
# pełny słupek — bieżące kopiowanie. Liczba tylko przy pełnym słupku.
# Oś y stała dla danych parametrów detektora (górna granica to d = 1),
# żeby suwak zmieniał wysokość słupków, a nie skalę.
a3_double_plot <- function(prevalence = .01, sensitivity = .95,
                           false_positive_rate = .05, dependence = 0) {
  dat <- a3_double_counts(prevalence, sensitivity, false_positive_rate, dependence)
  dat$label <- format(round(dat$count), big.mark = " ", trim = TRUE)
  dat$x <- as.numeric(dat$type)
  top <- max(dat$ceiling, 1)
  half <- 0.17
  w <- 0.3

  ggplot(dat) +
    geom_rect(aes(xmin = x - half - w / 2, xmax = x - half + w / 2,
                  ymin = 0, ymax = independent),
              fill = "#ffffff", colour = upwr_reference, linetype = 2, linewidth = 0.7) +
    geom_rect(aes(xmin = x + half - w / 2, xmax = x + half + w / 2,
                  ymin = 0, ymax = count, fill = type)) +
    geom_text(aes(x = x + half, y = count, label = label), vjust = -0.6, size = 5.4,
              colour = upwr_ink, family = lc_mono_family) +
    scale_fill_manual(values = c(Prawdziwe = upwr_accent, Fałszywe = upwr_single_alt),
                      guide = "none") +
    scale_x_continuous(breaks = dat$x, labels = levels(dat$type),
                       limits = c(0.45, 2.55), expand = expansion(0)) +
    scale_y_continuous(limits = c(0, top * 1.12), expand = expansion(mult = c(0, 0)),
                       labels = function(x) format(x, big.mark = " ", trim = TRUE)) +
    labs(x = NULL, y = "Podwójne alarmy na 10 000 zmian") +
    theme_upwr(grid = "y") +
    theme(
      axis.text.x = element_text(size = rel(1.05), colour = upwr_ink),
      axis.ticks.x = element_blank()
    )
}

a3_stop_double_plot_ui <- lc_plot("a3_double", ratio = "1.8/1", ratio_narrow = "1.1/1",
                                  max_height = "380px")

# ============================================================================
# SERWER
# ============================================================================

a3_stop_szanse_server <- function(input, output, session, detector_parameters) {
  zoom_plot_server("a3_odds_gallery", function() a3_odds_gallery_plot(),
    alt = paste(
      "Trzy strzałki na logarytmicznej osi szans: hala A od 1:99 razy 19 do 0.16,",
      "hala B od 1:19 razy 19 do 1:1, czyli 0.50, test przesiewowy od 1:99 razy 10 do 0.09."
    ))

  double_plot <- reactive({
    p <- detector_parameters()
    a3_double_plot(p$prevalence, p$sensitivity, p$false_positive_rate,
                   dependence = input$a3_dependence %||% 0)
  })
  zoom_plot_server("a3_double", double_plot,
    alt = paste(
      "Słupki podwójnych alarmów prawdziwych i fałszywych na 10 000 zmian;",
      "przerywany kontur pokazuje czujniki niezależne. Kopiowanie najbardziej",
      "podnosi słupek podwójnych fałszywych alarmów."
    ))
}
