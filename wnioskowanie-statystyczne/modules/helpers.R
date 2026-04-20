# ============================================================================
# FUNKCJE POMOCNICZE - Wnioskowanie statystyczne
# ============================================================================

# Generowanie danych studenckich (n=200)
generate_student_data <- function(n = 200) {
  set.seed(NULL)
  plec <- sample(c("Kobieta", "M\u0119\u017cczyzna"), n, replace = TRUE, prob = c(0.55, 0.45))
  kierunek <- sample(c("Informatyka", "Ekonomia", "Psychologia", "Biologia"),
                     n, replace = TRUE, prob = c(0.3, 0.25, 0.25, 0.2))

  wzrost <- ifelse(plec == "Kobieta",
                   rnorm(n, mean = 166, sd = 6),
                   rnorm(n, mean = 178, sd = 7))

  waga <- ifelse(plec == "Kobieta",
                 rnorm(n, mean = 62, sd = 8),
                 rnorm(n, mean = 78, sd = 10))

  # Srednia ocen zalezna od kierunku
  base_gpa <- switch_kierunek_gpa(kierunek)
  srednia_ocen <- pmin(pmax(base_gpa + rnorm(n, 0, 0.4), 2.0), 5.0)

  czas_dojazdu <- rgamma(n, shape = 3, scale = 10)

  zdal_egzamin <- rbinom(n, 1, prob = 0.7 + 0.05 * (srednia_ocen - 3.5))

  data.frame(
    plec = plec,
    kierunek = kierunek,
    wzrost = round(wzrost, 1),
    waga = round(waga, 1),
    srednia_ocen = round(srednia_ocen, 2),
    czas_dojazdu = round(czas_dojazdu, 1),
    zdal_egzamin = factor(ifelse(zdal_egzamin == 1, "Tak", "Nie")),
    stringsAsFactors = FALSE
  )
}

# Pomocnicza: srednia bazowa per kierunek
switch_kierunek_gpa <- function(kierunek) {
  sapply(kierunek, function(k) {
    switch(k,
      "Informatyka" = 3.6,
      "Ekonomia" = 3.4,
      "Psychologia" = 3.8,
      "Biologia" = 3.5,
      3.5
    )
  })
}

# Generowanie danych parowych (przed/po interwencji)
generate_paired_data <- function(n = 30, effect = 5) {
  set.seed(NULL)
  wynik_przed <- rnorm(n, mean = 50, sd = 12)
  wynik_po <- wynik_przed + rnorm(n, mean = effect, sd = 8)
  data.frame(
    student = 1:n,
    wynik_przed = round(wynik_przed, 1),
    wynik_po = round(wynik_po, 1)
  )
}

# Generowanie danych do korelacji
generate_correlation_data <- function(n = 50, r = 0.7, type = "linear") {
  set.seed(NULL)
  if (type == "linear") {
    x <- rnorm(n, mean = 170, sd = 10)
    y <- r * scale(x) * 12 + sqrt(1 - r^2) * rnorm(n) * 12 + 70
    data.frame(x = round(x, 1), y = round(y, 1))
  } else if (type == "monotonic") {
    x <- runif(n, 1, 10)
    y <- log(x) * 5 + rnorm(n, 0, 1)
    data.frame(x = round(x, 1), y = round(y, 2))
  } else {
    x <- rnorm(n, mean = 0, sd = 3)
    y <- rnorm(n, mean = 0, sd = 3)
    data.frame(x = round(x, 2), y = round(y, 2))
  }
}

# Rysowanie rozkladu pod H0 z zaznaczeniem statystyki testowej
plot_test_distribution <- function(stat_value, df = NULL, test_type = "t",
                                    alternative = "two.sided") {
  if (test_type == "t") {
    x <- seq(-4, 4, length.out = 500)
    y <- if (!is.null(df)) dt(x, df) else dnorm(x)
    label <- if (!is.null(df)) paste0("t(", df, ")") else "N(0,1)"
  } else if (test_type == "chisq") {
    x <- seq(0, max(stat_value * 2, 15), length.out = 500)
    y <- dchisq(x, df)
    label <- paste0("\u03c7\u00b2(", df, ")")
  } else if (test_type == "f") {
    x <- seq(0, max(stat_value * 2, 8), length.out = 500)
    df1 <- df[1]; df2 <- df[2]
    y <- df(x, df1, df2)
    label <- paste0("F(", df1, ",", df2, ")")
  } else {
    x <- seq(-4, 4, length.out = 500)
    y <- dnorm(x)
    label <- "N(0,1)"
  }

  plot_df <- data.frame(x = x, y = y)

  # Obszar p-wartosci
  if (test_type %in% c("chisq", "f")) {
    shade_df <- plot_df[plot_df$x >= stat_value, ]
  } else if (alternative == "two.sided") {
    shade_df <- plot_df[abs(plot_df$x) >= abs(stat_value), ]
  } else if (alternative == "greater") {
    shade_df <- plot_df[plot_df$x >= stat_value, ]
  } else {
    shade_df <- plot_df[plot_df$x <= stat_value, ]
  }

  # Punkty krytyczne i strefy
  alpha <- 0.05

  if (test_type %in% c("chisq", "f")) {
    if (test_type == "chisq") {
      crit_right <- qchisq(1 - alpha, df)
    } else {
      crit_right <- qf(1 - alpha, df[1], df[2])
    }
    shade_h0 <- plot_df[plot_df$x <= crit_right, ]
    shade_h1 <- plot_df[plot_df$x >= crit_right, ]

    p <- ggplot(plot_df, aes(x = x, y = y)) +
      geom_area(data = shade_h0, fill = "#27ae60", alpha = 0.15) +
      geom_area(data = shade_h1, fill = "#e74c3c", alpha = 0.25) +
      geom_line(color = "#3498db", linewidth = 1.2) +
      geom_vline(xintercept = crit_right, color = "#2c3e50",
                 linewidth = 0.8, linetype = "dashed") +
      geom_vline(xintercept = stat_value, color = "#e74c3c",
                 linewidth = 1.2) +
      annotate("text", x = stat_value, y = max(y) * 0.85,
               label = paste0("stat = ", round(stat_value, 3)),
               hjust = -0.1, color = "#e74c3c", fontface = "bold") +
      labs(title = paste0("Rozk\u0142ad pod H\u2080: ", label),
           x = "Statystyka testowa", y = "G\u0119sto\u015b\u0107") +
      theme_educational()

  } else if (alternative == "two.sided") {
    crit <- qt(1 - alpha / 2, df)
    shade_h0 <- plot_df[plot_df$x >= -crit & plot_df$x <= crit, ]
    shade_left <- plot_df[plot_df$x <= -crit, ]
    shade_right <- plot_df[plot_df$x >= crit, ]

    p <- ggplot(plot_df, aes(x = x, y = y)) +
      geom_area(data = shade_h0, fill = "#27ae60", alpha = 0.15) +
      geom_area(data = shade_left, fill = "#e74c3c", alpha = 0.25) +
      geom_area(data = shade_right, fill = "#e74c3c", alpha = 0.25) +
      geom_line(color = "#3498db", linewidth = 1.2) +
      geom_vline(xintercept = c(-crit, crit), color = "#2c3e50",
                 linewidth = 0.8, linetype = "dashed") +
      geom_vline(xintercept = stat_value, color = "#e74c3c",
                 linewidth = 1.2) +
      annotate("text", x = 0, y = max(y) * 0.45,
               label = "nie odrzucamy H\u2080", color = "#27ae60",
               fontface = "bold", size = 4) +
      annotate("text", x = -3.3, y = max(y) * 0.25,
               label = "Ha", color = "#e74c3c",
               fontface = "bold", size = 4) +
      annotate("text", x = 3.3, y = max(y) * 0.25,
               label = "Ha", color = "#e74c3c",
               fontface = "bold", size = 4) +
      annotate("text", x = stat_value, y = max(y) * 0.85,
               label = paste0("t = ", round(stat_value, 3)),
               hjust = if (stat_value > 0) -0.1 else 1.1,
               color = "#e74c3c", fontface = "bold") +
      labs(title = paste0("Rozk\u0142ad pod H\u2080: ", label),
           x = "Statystyka testowa", y = "G\u0119sto\u015b\u0107") +
      theme_educational()

  } else {
    # Jednostronny
    if (alternative == "greater") {
      crit <- qt(1 - alpha, df)
      shade_h0 <- plot_df[plot_df$x <= crit, ]
      shade_h1 <- plot_df[plot_df$x >= crit, ]
    } else {
      crit <- qt(alpha, df)
      shade_h0 <- plot_df[plot_df$x >= crit, ]
      shade_h1 <- plot_df[plot_df$x <= crit, ]
    }
    p <- ggplot(plot_df, aes(x = x, y = y)) +
      geom_area(data = shade_h0, fill = "#27ae60", alpha = 0.15) +
      geom_area(data = shade_h1, fill = "#e74c3c", alpha = 0.25) +
      geom_line(color = "#3498db", linewidth = 1.2) +
      geom_vline(xintercept = crit, color = "#2c3e50",
                 linewidth = 0.8, linetype = "dashed") +
      geom_vline(xintercept = stat_value, color = "#e74c3c",
                 linewidth = 1.2) +
      annotate("text", x = stat_value, y = max(y) * 0.85,
               label = paste0("t = ", round(stat_value, 3)),
               hjust = if (stat_value > 0) -0.1 else 1.1,
               color = "#e74c3c", fontface = "bold") +
      labs(title = paste0("Rozk\u0142ad pod H\u2080: ", label),
           x = "Statystyka testowa", y = "G\u0119sto\u015b\u0107") +
      theme_educational()
  }

  p
}

# Formatowanie wyniku testu jako tekst PL
format_test_result <- function(p_value, alpha = 0.05) {
  if (p_value < alpha) {
    list(
      decision = "Odrzucamy H\u2080",
      color = "#e74c3c",
      explanation = paste0("p = ", format.pval(p_value, digits = 4),
                           " < \u03b1 = ", alpha,
                           " \u2014 wynik istotny statystycznie")
    )
  } else {
    list(
      decision = "Brak podstaw do odrzucenia H\u2080",
      color = "#27ae60",
      explanation = paste0("p = ", format.pval(p_value, digits = 4),
                           " \u2265 \u03b1 = ", alpha,
                           " \u2014 wynik nieistotny statystycznie")
    )
  }
}

# Etykieta wielkosci efektu
effect_size_label <- function(d) {
  d <- abs(d)
  if (d < 0.2) "pomijalny"
  else if (d < 0.5) "ma\u0142y"
  else if (d < 0.8) "\u015bredni"
  else "du\u017cy"
}

# Praktyczna interpretacja Cohen's d (sensoryka / konsumenci)
interpret_cohens_d <- function(d) {
  ad <- abs(d)
  if (ad < 0.2) {
    "Efekt pomijalny: r\u00f3\u017cnica praktycznie nieuchwytna, nawet wyszkolony panel sensoryczny mia\u0142by problem j\u0105 wykry\u0107."
  } else if (ad < 0.5) {
    "Efekt ma\u0142y: r\u00f3\u017cnica ledwie uchwytna; konsument w te\u015bcie \u015blepym prawdopodobnie nie rozr\u00f3\u017cni, wyszkolony panel \u2014 czasem."
  } else if (ad < 0.8) {
    "Efekt \u015bredni: r\u00f3\u017cnica, kt\u00f3r\u0105 dobrze wytrenowany panel sensoryczny rozr\u00f3\u017cni; konsument \u2014 bywa \u017ce zauwa\u017cy."
  } else {
    "Efekt du\u017cy: r\u00f3\u017cnica wyra\u017ana, rozpozna j\u0105 nawet konsument w te\u015bcie \u015blepym."
  }
}

# Generowanie danych: fermentacja jogurtu w 3 temperaturach (ANOVA ch7)
# Zmienna: pH po 6h fermentacji; grupy: 20/25/30 stopni C
generate_fermentation_data <- function(n = 160) {
  set.seed(NULL)
  groups <- c("20\u00b0C", "25\u00b0C", "30\u00b0C")
  temp <- sample(groups, n, replace = TRUE)
  # \u015arednie pH: wy\u017csza temperatura -> szybsze zakwaszenie -> ni\u017csze pH
  base_pH <- sapply(temp, function(t) switch(t, "20\u00b0C" = 4.55, "25\u00b0C" = 4.30, "30\u00b0C" = 4.05))
  pH <- round(base_pH + rnorm(n, 0, 0.14), 2)

  # Druga zmienna: kwasowo\u015b\u0107 miareczkowa (\u00b0SH) \u2014 alternatywa do pH
  base_sh <- sapply(temp, function(t) switch(t, "20\u00b0C" = 28, "25\u00b0C" = 33, "30\u00b0C" = 38))
  kwasowosc_SH <- round(base_sh + rnorm(n, 0, 3.5), 1)

  data.frame(
    temperatura = factor(temp, levels = groups),
    pH = pH,
    kwasowosc_SH = kwasowosc_SH,
    stringsAsFactors = FALSE
  )
}

# Generowanie danych: telefon vs koncentracja (case study ch1)
# Inspirowane Ward et al. (2017) "Brain Drain"
generate_phone_data <- function(n_per_group = 40) {
  set.seed(NULL)
  plecak <- rnorm(n_per_group, mean = 72, sd = 12)
  biurko <- rnorm(n_per_group, mean = 65, sd = 14)
  data.frame(
    grupa = factor(rep(c("Telefon w plecaku", "Telefon na biurku"),
                       each = n_per_group),
                   levels = c("Telefon w plecaku", "Telefon na biurku")),
    koncentracja = round(c(plecak, biurko), 1)
  )
}

