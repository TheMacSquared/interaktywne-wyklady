# Punkt stop rozdziału „Częstości”:
#   - galeria „Trzy detektory w tej samej hali” (alarmy prawdziwe i fałszywe,
#     wiarygodność alarmu i przeoczone awarie na 10 000 zmian).
# Wymaga: palette.R, theme_upwr.R, shared.R, lecture_layout.R, risk_math.R.

# ============================================================================
# GALERIA: TRZY DETEKTORY W TEJ SAMEJ HALI
# ============================================================================

# Każdy wariant ma swój powód: A — obecny detektor; B — mniej czuły, ale rzadziej
# się myli na sprawnych zmianach; C — niczego nie przeocza, ale FPR jak w A.
a3_detectors_data <- function(prevalence = .01, population = 10000) {
  spec <- data.frame(
    id = c("A", "B", "C"),
    sensitivity = c(.95, .80, 1.00),
    false_positive_rate = c(.05, .01, .05)
  )
  rows <- lapply(seq_len(nrow(spec)), function(i) {
    counts <- risk_detector_counts(population, prevalence,
                                   spec$sensitivity[i], spec$false_positive_rate[i])
    data.frame(
      true_alarms = counts$alarm[counts$state == "Awaria"],
      false_alarms = counts$alarm[counts$state == "Brak awarii"],
      missed = counts$no_alarm[counts$state == "Awaria"]
    )
  })
  dat <- cbind(spec, do.call(rbind, rows))
  dat$alarms <- dat$true_alarms + dat$false_alarms
  dat$ppv <- dat$true_alarms / dat$alarms
  dat$label <- sprintf("%s · czułość %.2f · FPR %.2f",
                       dat$id, dat$sensitivity, dat$false_positive_rate)
  dat$label <- factor(dat$label, levels = rev(dat$label))
  dat
}

a3_detectors_plot <- function() {
  dat <- a3_detectors_data()
  dat$y <- as.numeric(dat$label)
  h <- 0.27
  x_ppv <- 700
  x_missed <- 820
  head_y <- max(dat$y) + 0.62

  ggplot(dat) +
    annotate("segment", x = 0, xend = 600, y = 0.55, yend = 0.55,
             colour = upwr_reference, linewidth = 0.4) +
    geom_rect(aes(xmin = 0, xmax = true_alarms, ymin = y - h, ymax = y + h),
              fill = upwr_accent) +
    geom_rect(aes(xmin = true_alarms, xmax = alarms, ymin = y - h, ymax = y + h),
              fill = upwr_single_alt) +
    geom_text(aes(x = true_alarms / 2, y = y, label = true_alarms), size = 4.6,
              colour = "#ffffff", family = lc_mono_family) +
    geom_text(aes(x = (true_alarms + alarms) / 2, y = y, label = false_alarms),
              size = 4.6, colour = upwr_ink, family = lc_mono_family) +
    geom_text(aes(x = x_ppv, y = y, label = sprintf("%.2f", ppv)), size = 6.6,
              colour = upwr_accent, family = lc_mono_family, fontface = "bold") +
    geom_text(aes(x = x_missed, y = y, label = missed), size = 5,
              colour = upwr_reference, family = lc_mono_family) +
    annotate("text", x = c(x_ppv, x_missed), y = head_y,
             label = c("wiarygodność\nalarmu", "przeoczone"), size = 4,
             colour = upwr_ink_soft) +
    scale_x_continuous(breaks = seq(0, 600, 200), limits = c(0, 880),
                       expand = expansion(mult = c(0, .02))) +
    scale_y_continuous(breaks = dat$y, labels = as.character(dat$label),
                       limits = c(0.55, head_y + 0.2), expand = expansion(0)) +
    labs(x = "Alarmy na 10 000 zmian", y = NULL) +
    theme_upwr(grid = "x") +
    theme(
      axis.text.y = element_text(size = rel(1.05), colour = upwr_ink),
      axis.line = element_blank(), axis.ticks.y = element_blank()
    )
}

a3_stop_detectors_ui <- figure_panel(
  label = "Galeria", title = "Trzy detektory w tej samej hali", full_width = TRUE,
  lc_plot("a3_detectors", ratio = "2.4/1", ratio_narrow = "1.3/1", max_height = "400px"),
  lc_caption(
    "Hala z awarią na 1 zmianie na 100, 10 000 zmian. Burgund: alarmy prawdziwe,",
    "złoto: fałszywe. Wiarygodność alarmu: P(awaria | 🚨). Przeoczone: awarie z wynikiem 🔕."
  )
)

# ============================================================================
# SERWER
# ============================================================================

a3_stop_detektory_server <- function(input, output, session) {
  zoom_plot_server("a3_detectors", function() a3_detectors_plot(),
    alt = paste(
      "Trzy poziome słupki alarmów na 10 000 zmian. Detektor A: 95 prawdziwych",
      "i 495 fałszywych, wiarygodność alarmu 0.16, przeoczone 5. Detektor B: 80 i 99,",
      "wiarygodność 0.45, przeoczone 20. Detektor C: 100 i 495, wiarygodność 0.17, przeoczone 0."
    ))
}
