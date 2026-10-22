# ============================================================================
# Wykład 02 — pomocnicze: dane scen, widget kadrowy
# ============================================================================

# --- Scena 1: kontrola drogowa (rozdz. 2) -----------------------------------
# Jeden dzień patrolu: 22 kobiety i 48 mężczyzn, mandat albo pouczenie.
# Kolejność zatrzymań jest wymieszana; liczby dobrane tak, żeby w liczebnościach
# mężczyźni mieli prawie cztery razy więcej mandatów, a w odsetkach różnica była
# wyraźnie mniejsza.
stop_data <- local({
  set.seed(5)
  sex  <- c(rep("K", 22), rep("M", 48))
  fine <- c(rep(1L, 4), rep(0L, 18), rep(1L, 15), rep(0L, 33))
  ord  <- sample(length(sex))
  data.frame(plec = factor(sex[ord], levels = c("K", "M")), mandat = fine[ord])
})
stop_tab <- table(stop_data$plec, stop_data$mandat)   # wiersze K/M, kolumny 0/1
stop_pct <- round(100 * stop_tab[, "1"] / rowSums(stop_tab))

# --- Scena 2: zarobki absolwentów w trzech branżach (rozdz. 3) --------------
# Pensja brutto rok po studiach (tys. zł), po 30 osób w branży, rozkłady
# prawoskośne (log-normalne).
branch_levels <- c("rolnicza", "biotech", "kosmiczna")
pay_data <- local({
  set.seed(11)
  spec <- list(rolnicza = c(6.2, 0.22), biotech = c(7.6, 0.25), kosmiczna = c(9.8, 0.28))
  do.call(rbind, lapply(names(spec), function(b) {
    s <- spec[[b]]
    data.frame(branza = b, pensja = round(rlnorm(30, log(s[1]), s[2]), 1))
  }))
})
pay_data$branza <- factor(pay_data$branza, levels = branch_levels)
pay_stats <- do.call(rbind, lapply(split(pay_data$pensja, pay_data$branza), function(x) {
  q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
  iqr <- q[3] - q[1]
  data.frame(n = length(x), mean = mean(x), q1 = q[1], med = q[2], q3 = q[3],
             lo = min(x[x >= q[1] - 1.5 * iqr]), hi = max(x[x <= q[3] + 1.5 * iqr]))
}))

# --- Scena 3 i paradoks Simpsona: staż a pensja (rozdz. 4 i 5) ---------------
# Pracownicy trzech branż, po 40 osób. W każdej branży pensja rośnie ze stażem,
# ale branża kosmiczna jest młoda i dobrze płatna, a rolnicza ma długie staże
# i niższe pensje, więc w danych połączonych związek jest ujemny.
tenure_data <- local({
  set.seed(7)
  spec <- list(kosmiczna = c(4, 2.2, 14), biotech = c(9, 3.5, 10.5), rolnicza = c(16, 5, 7.5))
  do.call(rbind, lapply(names(spec), function(b) {
    s <- spec[[b]]
    st <- pmin(30, pmax(0, rnorm(40, s[1], s[2])))
    pay <- s[3] + 0.35 * (st - mean(st)) + rnorm(40, 0, 1.4)
    data.frame(branza = b, staz = round(st, 1), pensja = round(pay, 1))
  }))
})
tenure_data$branza <- factor(tenure_data$branza, levels = branch_levels)
tenure_bio <- tenure_data[tenure_data$branza == "biotech", ]

# Kolory branż: te same w scenach (scenes.css) i na wykresach ggplot.
branch_colors <- c(
  rolnicza  = unname(upwr_cat["bursztyn"]),
  biotech   = unname(upwr_cat["wrzos"]),
  kosmiczna = unname(upwr_cat["indygo"])
)

# ============================================================================
# Widget kadrowy: kroki zmienia pasek kroków i „Dalej”, scena (scenes.js)
# animuje przejście między kadrami. Bez przycisków powtórzeń.
# ============================================================================

frames_widget <- function(id, title, steps, config) {
  lc_step_widget(id,
    title = title,
    steps = steps,
    body = tags$div(class = "lc-sc",
      `data-config` = jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA))
  )
}

# Teksty kroków: lista fragmentów HTML, po jednym na krok.
scene_texts <- function(input, output, id, texts) {
  step <- lc_step_server(id, input)$step
  output[[paste0(id, "_text")]] <- renderUI(texts[[step()]])
}

# ============================================================================
# Pułapki korelacji: dane bez związku (wartość odstająca, rozdz. 5)
# ============================================================================

# Dwie niezależne zmienne (ρ = 0), SD = 3; wartość odstająca leży o 15 dalej
# niż największe x i y, więc jej wpływ na r jest taki jak opisany w tekście.
generate_uncorrelated <- function(n) {
  data.frame(x = round(rnorm(n, 0, 3), 2), y = round(rnorm(n, 0, 3), 2))
}
