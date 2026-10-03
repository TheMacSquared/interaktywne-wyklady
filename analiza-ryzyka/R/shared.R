# ============================================================================
# WSPOLNE FUNKCJE POMOCNICZE
# Zrodlowane przez kazdy app.R po ustaleniu app_dir i project_root
# ============================================================================

# Paleta kolorów UPWr + motyw ggplot2 są sourcowane z R/palette.R i
# R/theme_upwr.R. Role semantyczne (upwr_accent/single/secondary/reference),
# paleta kategoryczna (upwr_cat), skale ciągłe (upwr_seq_*, upwr_div,
# upwr_ord*), theme_upwr(). Sourcowanie odbywa się w app.R po ustaleniu
# project_root — patrz typy-danych/app.R.

# Krój cyfr w etykietach wykresów (geom_text(family = lc_mono_family)).
# lc_apply_ggplot_defaults() przestawia go na IBM Plex Mono, gdy krój jest dostępny.
lc_mono_family <- "mono"

#' Ustaw globalny motyw i defaulty geom-ów dla całej apki.
#' Wywołać raz w app.R po sourcowaniu palette.R + theme_upwr.R.
lc_apply_ggplot_defaults <- function() {
  # IBM Plex Sans i IBM Plex Mono z R/fonts/ (OFL), te same co na stronie,
  # rejestrowane w systemfonts i rysowane przez ragg. Plex Sans ma grekę,
  # indeksy dolne i znaki łączące; Plex Mono tylko cyfry, łacinę i μ.
  # Glify spoza kroju ragg bierze z kroju systemowego (showtext tego nie potrafi).
  base_family <- ""
  font_dir <- file.path(project_root, "R", "fonts")
  if (requireNamespace("ragg", quietly = TRUE) &&
      requireNamespace("systemfonts", quietly = TRUE) &&
      file.exists(file.path(font_dir, "IBMPlexSans-Regular.ttf"))) {
    registered <- systemfonts::registry_fonts()$family
    for (fam in c("IBM Plex Sans", "IBM Plex Mono")) {
      if (fam %in% registered) next
      ttf <- function(style) file.path(font_dir, paste0(gsub(" ", "", fam), "-", style, ".ttf"))
      systemfonts::register_font(fam,
        plain = ttf("Regular"), bold = ttf("Bold"),
        italic = ttf("Italic"), bolditalic = ttf("BoldItalic"))
    }
    options(shiny.useragg = TRUE)
    base_family <- "IBM Plex Sans"
    lc_mono_family <<- "IBM Plex Mono"
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

# Formatuje p-wartość: "0.023", "<0.0001", ">0.99". Bez prefiksu "p =".
format_p_value <- function(p_value) {
  if (is.na(p_value)) return("NA")
  if (p_value < 0.0001) return("<0.0001")
  rounded <- signif(p_value, 2)
  if (rounded >= 1) return(">0.99")
  s <- formatC(rounded, format = "fg", digits = 2)
  if (!grepl("\\.", s)) s <- paste0(s, ".0")
  s
}

# Wersja z prefiksem: "p = 0.023" lub "p < 0.0001".
format_p <- function(p_value) {
  v <- format_p_value(p_value)
  if (startsWith(v, "<") || startsWith(v, ">")) {
    paste0("p ", substr(v, 1, 1), " ", substr(v, 2, nchar(v)))
  } else {
    paste0("p = ", v)
  }
}

# UI: linia "p = 0.023" w werdyktach — liczba pogrubiona i powiększona.
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
                 class = "lc-zoom-btn", title = "Powiększ wykres",
                 `aria-label` = "Powiększ wykres")
  )
}

# Czcionki wykresu: w tekście 1.1x, w oknie powiększenia 1.5x (osie i podpisy
# czytelne z sali). Wykresy renderujemy z res = 96, zgodnie z showtext_opts(dpi = 96)
# w lc_apply_ggplot_defaults(); przy domyślnym res = 72 cały tekst wychodził
# o jedną czwartą mniejszy, niż wynika ze skali.
zoom_plot_font_scale_inline <- 1.1
zoom_plot_font_scale_modal  <- 1.5
zoom_plot_res               <- 96

# Tytuł, podtytuł i podpis łamiemy tak, żeby mieściły się w szerokości wykresu
# (ggplot sam ich nie łamie, a w wąskiej kolumnie widżetu byłyby ucięte).
# Przybliżenie: średnia szerokość znaku to ok. 0,55 wysokości czcionki.
wrap_plot_labels <- function(p, width_px, k) {
  if (is.null(width_px) || !is.finite(width_px) || width_px <= 0) return(p)
  labs_now <- ggplot2::get_labs(p)
  wrap_one <- function(txt, rel_size) {
    if (!is.character(txt) || length(txt) != 1L || is.na(txt)) return(txt)
    px_per_char <- 0.55 * 11 * k * rel_size * 96 / 72
    n <- max(12L, floor((width_px - 16) / px_per_char))
    paste(strwrap(txt, width = n), collapse = "\n")
  }
  p <- p + ggplot2::labs(
    title    = wrap_one(labs_now$title, 1.3),
    subtitle = wrap_one(labs_now$subtitle, 1.0),
    caption  = wrap_one(labs_now$caption, 0.8)
  )
  # W wąskim wykresie legenda z boku zabiera połowę szerokości, a etykiety osi
  # nachodzą na siebie — legenda idzie pod wykres, kolidujące etykiety znikają.
  if (width_px < 480) {
    p <- p + ggplot2::theme(legend.position = "bottom", legend.direction = "vertical") +
      ggplot2::guides(x = ggplot2::guide_axis(check.overlap = TRUE))
  }
  p
}

zoom_plot_server <- function(id, plot_fn,
                             alt = "Wykres ilustrujący omawiane zagadnienie") {
  moduleServer(id, function(input, output, session) {
    # showtext liczy rozmiar czcionek względem swojego dpi, a Shiny renderuje
    # obraz z res * pixelratio (na ekranach retina 2x) — bez dopasowania tekst
    # wychodzi o połowę mniejszy. Ustawiamy dpi zanim wykresy się narysują.
    observe({
      ratio <- session$clientData$pixelratio
      if (!is.null(ratio) && requireNamespace("showtext", quietly = TRUE)) {
        showtext::showtext_opts(dpi = zoom_plot_res * ratio)
      }
    }, priority = 100)

    scaled_plot <- function(k, out_id) {
      p <- plot_fn()
      if (inherits(p, "ggplot")) {
        p <- wrap_plot_labels(
          p, session$clientData[[paste0("output_", session$ns(out_id), "_width")]], k
        )
        # Legenda w theme_upwr ma rel(0.8–0.85) — na sali za małe,
        # więc w wykresach wykładu rysujemy ją w pełnym rozmiarze tekstu.
        p <- p + ggplot2::theme(
          text         = ggplot2::element_text(size = 11 * k),
          legend.text  = ggplot2::element_text(size = ggplot2::rel(1)),
          legend.title = ggplot2::element_text(size = ggplot2::rel(1))
        )
      }
      p
    }

    output$plot <- renderPlot(scaled_plot(zoom_plot_font_scale_inline, "plot"), res = zoom_plot_res, alt = alt)

    observeEvent(input$zoom, {
      showModal(modalDialog(
        plotOutput(session$ns("plot_modal"), height = "65vh"),
        footer    = NULL,
        easyClose = TRUE,
        size      = "l"
      ))
    }, ignoreInit = TRUE)

    output$plot_modal <- renderPlot(scaled_plot(zoom_plot_font_scale_modal, "plot_modal"), res = zoom_plot_res, alt = alt)
  })
}
