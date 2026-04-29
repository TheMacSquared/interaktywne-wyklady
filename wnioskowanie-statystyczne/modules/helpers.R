# ============================================================================
# FUNKCJE POMOCNICZE - Testowanie hipotez
# ============================================================================

# ----------------------------------------------------------------------------
# Kolory domenowe dla testowania hipotez, mapowane na paletę UPWr.
# Używane w modułach zamiast ad-hoc hex-ów; role semantyczne zachowane
# dla czytelności kodu i spójności wykresów w całym wykładzie.
# ----------------------------------------------------------------------------

test_colors <- c(
  h0     = unname(upwr_cat["niebo"]),      # rozkład pod H0
  h1     = upwr_accent,                     # obszar odrzucenia / H1
  pvalue = unname(upwr_cat["bursztyn"]),    # p-wartość
  accept = unname(upwr_cat["szalwia"]),     # brak podstaw do odrzucenia
  reject = upwr_accent,                     # odrzucenie H0 (= h1)
  effect = unname(upwr_cat["wrzos"]),       # wielkość efektu
  paired = unname(upwr_cat["kurkuma"])      # dane parowe
)

# Aliasy: nazwy domenowe używane w modułach wykładu (col_h0 = rozkład pod H0,
# col_reject = obszar odrzucenia itd.). Dają czytelniejszy kod w wykresach
# niż test_colors["..."]. Same wartości pochodzą z test_colors.
col_h0     <- unname(test_colors["h0"])
col_h1     <- unname(test_colors["h1"])
col_pvalue <- unname(test_colors["pvalue"])
col_accept <- unname(test_colors["accept"])
col_reject <- unname(test_colors["reject"])
col_effect <- unname(test_colors["effect"])
col_paired <- unname(test_colors["paired"])


# ----------------------------------------------------------------------------
# hypothesis_practice() — widget „Sformułuj hipotezy”.
# Wyświetla listę pytań potocznych. Dla każdego przycisk „Pokaż odpowiedź”
# odkrywa poprawną parę H0/Ha. Wbudowane do użycia w rozdziałach konkretnych
# testów (po definicji testu, przed widgetem krok-po-kroku) oraz w ch2h
# (jako galeria pytanie → hipoteza w języku naturalnym).
#
# Argumenty:
#   prefix    — unikalny prefiks inputów (np. "ch2h"), żeby ID nie kolidowały
#   questions — lista list(question=..., h0=..., ha=..., note=...)
#               question — potoczne pytanie (str lub tagi shiny)
#               h0, ha   — hipotezy; mogą być:
#                          • stringiem (prosty tekst, bez MathJax)
#                          • stringiem z MathJax \\(...\\) — auto-render
#                          • tagami shiny (gdy chcesz mieszać formatowanie)
#               note     — opcjonalny komentarz metodyczny
# ----------------------------------------------------------------------------

hypothesis_practice <- function(prefix, questions) {
  items <- lapply(seq_along(questions), function(i) {
    q <- questions[[i]]
    btn_id <- paste0(prefix, "_hp_btn_", i)

    # h0/ha: jeśli string z MathJax (\\(...\\)) — wrap w withMathJax,
    # jeśli zwykły tekst lub tagi — przekaż jak jest
    render_hyp <- function(x) {
      if (is.character(x) && length(x) == 1 && grepl("\\\\\\(", x)) {
        withMathJax(x)
      } else {
        x
      }
    }

    tags$div(
      class = "hp-item",
      tags$div(class = "hp-question",
        tags$span(class = "hp-num", paste0(i, ".")),
        if (inherits(q$question, "html") || inherits(q$question, "shiny.tag") ||
            inherits(q$question, "shiny.tag.list")) q$question
        else tags$span(q$question)
      ),
      tags$div(class = "hp-controls",
        actionButton(btn_id, "Pokaż odpowiedź",
                     class = "lc-btn-secondary-outline lc-btn-sm")
      ),
      conditionalPanel(
        condition = paste0("input['", btn_id, "'] % 2 === 1"),
        tags$div(class = "hp-answer",
          tags$div(class = "hp-hypotheses",
            tags$div(tags$b("H₀: "), render_hyp(q$h0)),
            tags$div(tags$b("Hₐ: "), render_hyp(q$ha))
          ),
          if (!is.null(q$note))
            tags$div(class = "hp-note", tags$em(q$note))
        )
      )
    )
  })

  tags$div(class = "hypothesis-practice", items)
}

exercise_solution_toggle_server <- function(input, output, session, btn_id, sol_fn,
                                            visible = reactiveVal(FALSE),
                                            output_id = sub("_ans", "_sol", btn_id),
                                            feedback_type = "ok") {
  observeEvent(input[[btn_id]], {
    nowy <- !visible()
    visible(nowy)
    updateActionButton(session, btn_id,
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output[[output_id]] <- renderUI({
    if (!visible()) return(NULL)
    lc_feedback(type = feedback_type, style = "margin-top: 10px;", sol_fn())
  })

  visible
}


# Generowanie danych studenckich (n=200)
generate_student_data <- function(n = 200) {
  set.seed(NULL)
  plec <- sample(c("Kobieta", "Mężczyzna"), n, replace = TRUE, prob = c(0.55, 0.45))
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
generate_correlation_data <- function(n = 50, r = 0.7, type = "linear",
                                      x_mean = 170, x_sd = 10,
                                      y_mean = 70, y_sd = 12) {
  set.seed(NULL)
  if (type == "linear") {
    x <- rnorm(n, mean = x_mean, sd = x_sd)
    y <- r * scale(x) * y_sd + sqrt(1 - r^2) * rnorm(n) * y_sd + y_mean
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
    label <- paste0("χ²(", df, ")")
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
      geom_area(data = shade_h0, fill = unname(upwr_cat["szalwia"]), alpha = 0.15) +
      geom_area(data = shade_h1, fill = upwr_accent, alpha = 0.25) +
      geom_line(color = unname(upwr_cat["niebo"]), linewidth = 1.2) +
      geom_vline(xintercept = crit_right, color = upwr_secondary,
                 linewidth = 0.8, linetype = "dashed") +
      geom_vline(xintercept = stat_value, color = upwr_accent,
                 linewidth = 1.2) +
      annotate("text", x = stat_value, y = max(y) * 0.85,
               label = paste0("stat = ", round(stat_value, 3)),
               hjust = -0.1, color = upwr_accent, fontface = "bold") +
      labs(title = paste0("Rozkład pod H0: ", label),
           x = "Statystyka testowa", y = "Gęstość") +
      theme()

  } else if (alternative == "two.sided") {
    crit <- qt(1 - alpha / 2, df)
    shade_h0 <- plot_df[plot_df$x >= -crit & plot_df$x <= crit, ]
    shade_left <- plot_df[plot_df$x <= -crit, ]
    shade_right <- plot_df[plot_df$x >= crit, ]

    p <- ggplot(plot_df, aes(x = x, y = y)) +
      geom_area(data = shade_h0, fill = unname(upwr_cat["szalwia"]), alpha = 0.15) +
      geom_area(data = shade_left, fill = upwr_accent, alpha = 0.25) +
      geom_area(data = shade_right, fill = upwr_accent, alpha = 0.25) +
      geom_line(color = unname(upwr_cat["niebo"]), linewidth = 1.2) +
      geom_vline(xintercept = c(-crit, crit), color = upwr_secondary,
                 linewidth = 0.8, linetype = "dashed") +
      geom_vline(xintercept = stat_value, color = upwr_accent,
                 linewidth = 1.2) +
      annotate("text", x = 0, y = max(y) * 0.45,
               label = "nie odrzucamy H0", color = unname(upwr_cat["szalwia"]),
               fontface = "bold", size = 4) +
      annotate("text", x = -3.3, y = max(y) * 0.25,
               label = "Ha", color = upwr_accent,
               fontface = "bold", size = 4) +
      annotate("text", x = 3.3, y = max(y) * 0.25,
               label = "Ha", color = upwr_accent,
               fontface = "bold", size = 4) +
      annotate("text", x = stat_value, y = max(y) * 0.85,
               label = paste0("t = ", round(stat_value, 3)),
               hjust = if (stat_value > 0) -0.1 else 1.1,
               color = upwr_accent, fontface = "bold") +
      labs(title = paste0("Rozkład pod H0: ", label),
           x = "Statystyka testowa", y = "Gęstość") +
      theme()

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
      geom_area(data = shade_h0, fill = unname(upwr_cat["szalwia"]), alpha = 0.15) +
      geom_area(data = shade_h1, fill = upwr_accent, alpha = 0.25) +
      geom_line(color = unname(upwr_cat["niebo"]), linewidth = 1.2) +
      geom_vline(xintercept = crit, color = upwr_secondary,
                 linewidth = 0.8, linetype = "dashed") +
      geom_vline(xintercept = stat_value, color = upwr_accent,
                 linewidth = 1.2) +
      annotate("text", x = stat_value, y = max(y) * 0.85,
               label = paste0("t = ", round(stat_value, 3)),
               hjust = if (stat_value > 0) -0.1 else 1.1,
               color = upwr_accent, fontface = "bold") +
      labs(title = paste0("Rozkład pod H0: ", label),
           x = "Statystyka testowa", y = "Gęstość") +
      theme()
  }

  p
}

# Formatowanie wyniku testu jako tekst PL
format_test_result <- function(p_value, alpha = 0.05) {
  if (p_value < alpha) {
    list(
      decision = "Odrzucamy H₀",
      color = upwr_accent,
      explanation = paste0("p = ", format.pval(p_value, digits = 4),
                           " < α = ", alpha,
                           " — wynik istotny statystycznie")
    )
  } else {
    list(
      decision = "Brak podstaw do odrzucenia H₀",
      color = unname(upwr_cat["szalwia"]),
      explanation = paste0("p = ", format.pval(p_value, digits = 4),
                           " ≥ α = ", alpha,
                           " — wynik nieistotny statystycznie")
    )
  }
}

# Etykieta wielkosci efektu
effect_size_label <- function(d) {
  d <- abs(d)
  if (d < 0.2) "pomijalny"
  else if (d < 0.5) "mały"
  else if (d < 0.8) "średni"
  else "duży"
}

# Praktyczna interpretacja Cohen's d (sensoryka / konsumenci)
interpret_cohens_d <- function(d) {
  ad <- abs(d)
  if (ad < 0.2) {
    "Efekt pomijalny: różnica praktycznie nieuchwytna, nawet wyszkolony panel sensoryczny miałby problem ją wykryć."
  } else if (ad < 0.5) {
    "Efekt mały: różnica ledwie uchwytna; konsument w teście ślepym prawdopodobnie nie rozróżni, wyszkolony panel — czasem."
  } else if (ad < 0.8) {
    "Efekt średni: różnica, którą dobrze wytrenowany panel sensoryczny rozróżni; konsument — bywa że zauważy."
  } else {
    "Efekt duży: różnica wyraźna, rozpozna ją nawet konsument w teście ślepym."
  }
}

# Generowanie danych: fermentacja jogurtu w 3 temperaturach (ANOVA ch7)
# Zmienna: pH po 6h fermentacji; grupy: 20/25/30 stopni C
generate_fermentation_data <- function(n = 160) {
  set.seed(NULL)
  groups <- c("20°C", "25°C", "30°C")
  temp <- sample(groups, n, replace = TRUE)
  # Średnie pH: wyższa temperatura -> szybsze zakwaszenie -> niższe pH
  base_pH <- sapply(temp, function(t) switch(t, "20°C" = 4.55, "25°C" = 4.30, "30°C" = 4.05))
  pH <- round(base_pH + rnorm(n, 0, 0.14), 2)

  # Druga zmienna: kwasowość miareczkowa (°SH) — alternatywa do pH
  base_sh <- sapply(temp, function(t) switch(t, "20°C" = 28, "25°C" = 33, "30°C" = 38))
  kwasowosc_SH <- round(base_sh + rnorm(n, 0, 3.5), 1)

  data.frame(
    temperatura = factor(temp, levels = groups),
    pH = pH,
    kwasowosc_SH = kwasowosc_SH,
    stringsAsFactors = FALSE
  )
}

# Generowanie danych: stanowiska pracy a wypadki / stres (IB)
# Trzy stanowiska (budowa / magazyn / biuro) × dwie zmienne zależne:
#   nieobecnosci — liczba dni nieobecności w roku z powodu urazu
#   stres        — poziom stresu w pracy (skala 0–100)
generate_workplace_data <- function(n = 160) {
  set.seed(NULL)
  groups <- c("Budowa", "Magazyn", "Biuro")
  stanowisko <- sample(groups, n, replace = TRUE)

  # Dni nieobecności: budowa > magazyn > biuro
  base_abs <- sapply(stanowisko, function(s)
    switch(s, "Budowa" = 9, "Magazyn" = 5, "Biuro" = 2))
  nieobecnosci <- pmax(0, round(base_abs + rnorm(n, 0, 3)))

  # Stres: magazyn i budowa wyższy, biuro niższy
  base_stres <- sapply(stanowisko, function(s)
    switch(s, "Budowa" = 62, "Magazyn" = 58, "Biuro" = 48))
  stres <- pmin(100, pmax(0, round(base_stres + rnorm(n, 0, 12))))

  data.frame(
    stanowisko = factor(stanowisko, levels = groups),
    nieobecnosci = nieobecnosci,
    stres = stres,
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
