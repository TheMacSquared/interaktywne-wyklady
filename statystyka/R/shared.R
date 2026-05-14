# ============================================================================
# WSPOLNE FUNKCJE POMOCNICZE
# Zrodlowane przez kazdy app.R po ustaleniu app_dir i project_root
# ============================================================================

# Paleta kolorów UPWr + motyw ggplot2 są sourcowane z R/palette.R i
# R/theme_upwr.R. Role semantyczne (upwr_accent/single/secondary/reference),
# paleta kategoryczna (upwr_cat), skale ciągłe (upwr_seq_*, upwr_div,
# upwr_ord*), theme_upwr(). Sourcowanie odbywa się w app.R po ustaleniu
# project_root — patrz typy-danych/app.R.

#' Ustaw globalny motyw i defaulty geom-ów dla całej apki.
#' Wywołać raz w app.R po sourcowaniu palette.R + theme_upwr.R.
lc_apply_ggplot_defaults <- function() {
  # Zarejestruj Atkinson Hyperlegible w ggplot przez showtext (jeśli dostępne).
  # Bez showtext ggplot nie wyrenderuje fontu webowego — spadnie na systemowy sans.
  base_family <- ""
  if (requireNamespace("showtext", quietly = TRUE) &&
      requireNamespace("sysfonts", quietly = TRUE)) {
    if (!"Atkinson Hyperlegible" %in% sysfonts::font_families()) {
      try(sysfonts::font_add_google("Atkinson Hyperlegible", "Atkinson Hyperlegible"),
          silent = TRUE)
    }
    if ("Atkinson Hyperlegible" %in% sysfonts::font_families()) {
      showtext::showtext_auto()
      showtext::showtext_opts(dpi = 96)
      base_family <- "Atkinson Hyperlegible"
    }
  }
  ggplot2::theme_set(theme_upwr(base_family = base_family))
  ggplot2::update_geom_defaults("point",   list(colour = upwr_single))
  ggplot2::update_geom_defaults("line",    list(colour = upwr_single))
  ggplot2::update_geom_defaults("bar",     list(fill   = upwr_single))
  ggplot2::update_geom_defaults("col",     list(fill   = upwr_single))
  ggplot2::update_geom_defaults("density", list(colour = upwr_single, fill = NA))
  ggplot2::update_geom_defaults("boxplot", list(fill   = upwr_panel,  colour = upwr_single))
  ggplot2::update_geom_defaults("smooth",  list(colour = upwr_accent, fill = upwr_seq_burgundy[3]))
  ggplot2::update_geom_defaults("vline",   list(colour = upwr_reference, linetype = "dashed"))
  ggplot2::update_geom_defaults("hline",   list(colour = upwr_reference, linetype = "dashed"))
  invisible(NULL)
}

# ============================================================================
# WSPOLNE FUNKCJE GENEROWANIA DANYCH
# Uzywane przez: przedzialy-ufnosci, rozklady-prawdopodobienstwa
# ============================================================================

# Generowanie proby z wybranego rozkladu (superset wszystkich aplikacji)
generate_population_sample <- function(dist_type, n) {
  switch(dist_type,
    "normal"      = rnorm(n, mean = 170, sd = 10),
    "exponential" = rexp(n, rate = 0.5),
    "uniform"     = runif(n, min = 0, max = 10),
    "bimodal"     = {
      k <- rbinom(n, 1, 0.5)
      ifelse(k == 1, rnorm(n, mean = 3, sd = 0.8), rnorm(n, mean = 7, sd = 0.8))
    },
    "skewed"      = rgamma(n, shape = 2, scale = 1.5),
    "u_shape"     = rbeta(n, 0.5, 0.5) * 10,
    "skewed_left" = 10 - rgamma(n, shape = 2, scale = 1.5),
    "die"         = sample(1:6, n, replace = TRUE),
    rnorm(n)
  )
}

# Parametry populacji dla wybranego rozkladu
get_population_params <- function(dist_type) {
  switch(dist_type,
    "normal"      = list(mu = 170, sigma = 10),
    "exponential" = list(mu = 2, sigma = 2),
    "uniform"     = list(mu = 5, sigma = sqrt(100/12)),
    "bimodal"     = list(mu = 5, sigma = sqrt(0.8^2 + 4)),
    "skewed"      = list(mu = 3, sigma = sqrt(2) * 1.5),
    "u_shape"     = list(mu = 5, sigma = sqrt(10^2 / 4)),
    "skewed_left" = list(mu = 10 - 2*1.5, sigma = sqrt(2) * 1.5),
    "die"         = list(mu = 3.5, sigma = sqrt(35/12)),
    list(mu = 0, sigma = 1)
  )
}

# Nazwy rozkladow po polsku (superset wszystkich aplikacji)
dist_names_pl <- c(
  "normal"      = "Normalny (wzrost)",
  "exponential" = "Wykładniczy (prawoskośny)",
  "uniform"     = "Jednostajny",
  "bimodal"     = "Dwumodalny",
  "skewed"      = "Prawoskosńny (Gamma)",
  "u_shape"     = "U-kształtny (Beta)",
  "skewed_left" = "Lewoskośny",
  "die"         = "Kostka (dyskretny)"
)

# ============================================================================
# FORMATOWANIE P-WARTOSCI (styl PL — przecinek jako separator dziesiętny)
# Uzywane we wszystkich wykladach z testami statystycznymi.
# ============================================================================

# Formatuje p-wartość: "0,023", "<0,0001", ">0,99". Bez prefiksu "p =".
format_p_value <- function(p_value) {
  if (is.na(p_value)) return("NA")
  if (p_value < 0.0001) return("<0,0001")
  rounded <- signif(p_value, 2)
  if (rounded >= 1) return(">0,99")
  s <- formatC(rounded, format = "fg", digits = 2)
  if (!grepl("\\.", s)) s <- paste0(s, ".0")
  gsub("\\.", ",", s)
}

# Wersja z prefiksem: "p = 0,023" lub "p < 0,0001".
format_p <- function(p_value) {
  v <- format_p_value(p_value)
  if (startsWith(v, "<") || startsWith(v, ">")) {
    paste0("p ", substr(v, 1, 1), " ", substr(v, 2, nchar(v)))
  } else {
    paste0("p = ", v)
  }
}

# UI: linia "p = 0,023" w werdyktach — liczba pogrubiona i powiększona.
# Zwraca tag <p> gotowy do wstawienia w lc_feedback / tagList.
ui_p_value <- function(p_value) {
  v <- format_p_value(p_value)
  is_bound <- startsWith(v, "<") || startsWith(v, ">")
  prefix   <- if (is_bound) paste0("p ", substr(v, 1, 1), " ") else "p = "
  number   <- if (is_bound) substr(v, 2, nchar(v)) else v
  tags$p(
    prefix,
    tags$strong(
      style = "font-size: 1.25em;",
      number
    )
  )
}

# ============================================================================
# FORMATOWANIE WYNIKOW STATYSTYCZNYCH
# Helpery uzywane w rozwiazaniach cwiczen — zapewniaja ze wartosci w UI
# sa liczone z danych/parametrow, a nie wpisane na staie.
# ============================================================================

# Formatuje prawdopodobienstwo jako "0.2392 (~23.9%)"
.fmt_p <- function(p) sprintf("%.4f (~%.1f%%)", p, 100 * p)

# CI dla sredniej — zwraca named list: mean, sd, n, lo, hi, me
.ci_mean <- function(x, level = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x); s <- sd(x)
  se <- s / sqrt(n)
  me <- qt((1 + level) / 2, df = n - 1) * se
  list(n = n, mean = m, sd = s, se = se, me = me, lo = m - me, hi = m + me)
}

# CI dla proporcji — Wald. Zwraca named list: p, n, k, lo, hi, me
.ci_prop <- function(x, level = 0.95) {
  x <- x[!is.na(x)]
  if (is.logical(x)) x <- as.integer(x)
  n <- length(x); k <- sum(x); p <- k / n
  se <- sqrt(p * (1 - p) / n)
  me <- qnorm((1 + level) / 2) * se
  list(n = n, k = k, p = p, se = se, me = me, lo = p - me, hi = p + me)
}

# Skrotowe formattery — wywolania inline w tagach shiny
.fmt_mean <- function(ci, digits = 2) sprintf("%.*f", digits, ci$mean)
.fmt_sd   <- function(ci, digits = 2) sprintf("%.*f", digits, ci$sd)
.fmt_me   <- function(ci, digits = 2) sprintf("%.*f", digits, ci$me)
.fmt_ci   <- function(ci, digits = 2) sprintf("[%.*f, %.*f]", digits, ci$lo, digits, ci$hi)
.fmt_prop <- function(ci, digits = 3) sprintf("%.*f", digits, ci$p)

# ============================================================================
# SLOWNIK TERMINOW (gloss)
# Sourcujemy glossary.R jeśli istnieje — udostępnia gloss() i .GLOSSARY.
# ============================================================================
local({
  gf <- file.path(project_root, "R", "glossary.R")
  if (file.exists(gf)) source(gf, local = parent.env(environment()))
})

# ============================================================================
# MODUŁ: zoom_plot — przycisk powiększ + showModal dla każdego wykresu
# UI:     zoom_plot_ui("id", height = "300px")
# Server: zoom_plot_server("id", reactive({ ggplot(...) }))
# ============================================================================

zoom_plot_ui <- function(id, height = "300px", width = "100%", ...) {
  ns <- NS(id)
  div(class = "lc-zoom-plot-wrap",
    plotOutput(ns("plot"), height = height, width = width, ...),
    actionButton(ns("zoom"), HTML("&#x2922;"),
                 class = "lc-zoom-btn", title = "Powiększ wykres")
  )
}

zoom_plot_server <- function(id, plot_fn) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot(plot_fn())

    observeEvent(input$zoom, {
      showModal(modalDialog(
        plotOutput(session$ns("plot_modal"), height = "65vh"),
        footer    = NULL,
        easyClose = TRUE,
        size      = "l"
      ))
    }, ignoreInit = TRUE)

    output$plot_modal <- renderPlot(plot_fn())
  })
}
