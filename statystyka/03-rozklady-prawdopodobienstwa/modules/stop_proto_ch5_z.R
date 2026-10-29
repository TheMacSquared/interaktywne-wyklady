# RYC. 5.5 „KTO BARDZIEJ WYJĄTKOWY?” (prototyp 5c-C, zaakceptowany 2026-10-29) ------
# Chart.js (duel_chart.js): cztery pary, widok „dwie skale” z przeciąganiem wartości
# albo „wspólna skala z”. Bez serwera. Usunięte warianty: 5c-A „Dwie skale → jedna”,
# 5c-B „Tylko wspólna oś”.

s3p_z_pairs <- list(
  list(short = "Wzrost", q = "Kto jest bardziej wyjątkowo wysoki w swojej grupie?",
       a = list(lab = "Kobieta", grp = "Kobiety", x = 180, mu = 166, sd = 6, unit = "cm", step = 0.5),
       b = list(lab = "Mężczyzna", grp = "Mężczyźni", x = 190, mu = 178, sd = 7, unit = "cm", step = 0.5)),
  list(short = "Egzaminy", q = "Który wynik jest lepszy na tle swojego egzaminu?",
       a = list(lab = "Egzamin A", grp = "Egzamin A", x = 90, mu = 60, sd = 15, unit = "pkt", step = 1),
       b = list(lab = "Test B", grp = "Test B", x = 680, mu = 500, sd = 100, unit = "pkt", step = 5)),
  list(short = "Gorączka i IQ", q = "Co jest rzadsze?",
       a = list(lab = "Temperatura", grp = "Temperatura ciała", x = 37.4, mu = 36.6, sd = 0.4, unit = "°C", step = 0.1),
       b = list(lab = "IQ", grp = "Iloraz inteligencji", x = 130, mu = 100, sd = 15, unit = "pkt", step = 1)),
  list(short = "Koszykarz", q = "Jak wyjątkowy jest koszykarz o wzroście 213 cm?",
       a = list(lab = "Wśród mężczyzn", grp = "Wszyscy mężczyźni", x = 213, mu = 178, sd = 7, unit = "cm", step = 0.5),
       b = list(lab = "W NBA", grp = "Zawodnicy NBA", x = 213, mu = 200, sd = 9, unit = "cm", step = 0.5))
)
s3p_z_cols <- c(a = upwr_accent, b = unname(upwr_cat["indygo"]))
s3p_z_of <- function(o) (o$x - o$mu) / o$sd

# ============================================================================
# (5c-C) POJEDYNEK — Chart.js, przełącznik widoku
# ============================================================================

s3p_5cc_ui <- s3p_panel("5c-C", "Ryc. 5.5", "Kto bardziej wyjątkowy?", proto = FALSE,
  tags$div(class = "lc-duel",
    `data-config` = jsonlite::toJSON(s3p_z_pairs, auto_unbox = TRUE, digits = NA),
    lc_toolbar(
      lc_group("Para", tags$div(class = "lc-seg", role = "group",
        lapply(seq_along(s3p_z_pairs), function(i) tags$button(type = "button", `data-d-pair` = i - 1,
          `aria-pressed` = if (i == 1) "true" else "false", s3p_z_pairs[[i]]$short)))),
      lc_group("Widok", tags$div(class = "lc-seg", role = "group",
        tags$button(type = "button", `data-d-view` = "raw", `aria-pressed` = "true", "Dwie skale"),
        tags$button(type = "button", `data-d-view` = "z", `aria-pressed` = "false", "Wspólna skala z"))),
      lc_readouts(
        lc_readout(tags$span(`data-role` = "la", "Kobieta"), tags$span(`data-role` = "za", "180.0 cm"),
                   color = "var(--upwr-accent)", swatch = TRUE),
        lc_readout(tags$span(`data-role` = "lb", "Mężczyzna"), tags$span(`data-role` = "zb", "190.0 cm"),
                   color = "var(--upwr-cat-indygo)", swatch = TRUE)
      )
    ),
    tags$div(class = "lc-duel-top",
      tags$div(class = "lc-duel-small", tags$canvas(`data-role` = "a")),
      tags$div(class = "lc-duel-small", tags$canvas(`data-role` = "b"))),
    tags$div(class = "lc-duel-z", tags$canvas(`data-role` = "z")),
    lc_caption(tags$span(`data-role` = "q", s3p_z_pairs[[1]]$q))
  )
)
