# PUNKTY STOP — rozdział 1 -----------------------------------------------------
#   s3p_11a_ui — Ryc. 1.1 „Stos kostek”
#   s3p_11c_ui — Ryc. 1.2 „Lejek”
#   s3p_15b_ui — Ryc. 1.5 „Rozkład czy nie?”
# Serwer: s3p_ch1_server(input, output, session).

# ============================================================================
# RYC. 1.1 I 1.2 — częstości z danych dążą do rozkładu (dawna Ryc. 1.1 w dwóch krokach)
# ============================================================================

# (1.1-A) „Stos kostek”: każdy rzut to kafelek w kolumnie swojej ścianki.
# Rzuty, kafelki i słupki liczy przeglądarka (stops12_charts.js, d03-dice).
s3p_11a_tiles_max <- 60

s3p_11a_ui <- s3p_panel("Ryc. 1.1", "Stos kostek",
  lc_sf_figure("d03-dice", ratio = "2.2/1", max_height = "440px", legend = TRUE,
    config = list(tiles_max = s3p_11a_tiles_max, captions = list(
      few = "Kilka rzutów: kolumny są nierówne, niektórych ścianek jeszcze nie ma. Z tak małej próby nie odgadniemy modelu.",
      tiles = "Kolumny rosną w różnym tempie, ale wszystkie trzymają się blisko linii n/6. Bezwzględne różnice wysokości nie znikają, za to rosną wolniej niż same kolumny.",
      bars = "W skali odsetków różnice topnieją: słupki przylegają do 1/6. Częstości z danych zbliżają się do rozkładu prawdopodobieństwa.")),
    toolbar = tagList(
      lc_sf_action_group("Rzuć kostką", r1 = "+1", r6 = "+6", r60 = "+60", r600 = "+600"),
      lc_sf_action("reset", "s3p_11a_reset", icon = "reset", variant = "ghost", aria_label = "Wyzeruj rzuty"),
      lc_readouts(
        lc_readout("Rzuty", lc_sf_value("n", "0")),
        lc_readout("Ostatni", lc_sf_value("last"), color = upwr_accent))),
    after = tags$div(`data-role` = "caption", hidden = NA, lc_caption(lc_sf_value("cap", ""))))
)

# (1.1-C) „Lejek”: częstość szóstki w wielu seriach na tle 1/6 ± 2·SE.
s3p_11c_len <- 1000
# Serie losuje i rysuje przeglądarka (lejek_chart.js): rzuty, odczyty i podpis.

s3p_11c_ui <- s3p_panel("Ryc. 1.2", "Lejek",
  tags$div(class = "lc-lejek",
    lc_toolbar(
      lc_action_group(label = "Seria 1000 rzutów", s3p_11c_s1 = "+1", s3p_11c_s10 = "+10"),
      lc_action("s3p_11c_reset", icon = "reset", variant = "ghost", aria_label = "Wyzeruj serie"),
      lc_readouts(
        lc_readout("Serie", tags$span(`data-lejek` = "count", "0")),
        lc_readout("Rozrzut przy n = 10", tags$span(`data-lejek` = "sd10", "–")),
        lc_readout("n = 100", tags$span(`data-lejek` = "sd100", "–")),
        lc_readout("n = 1000", tags$span(`data-lejek` = "sd1000", "–"), color = upwr_accent)
      ),
      lc_chart_zoom_button()
    ),
    tags$div(class = "lc-lejek-canvas",
      tags$div(class = "lc-ec", role = "img", `data-role` = "lejek",
                  `aria-label` = "Lejek: częstość szóstek w seriach rzutów na tle pasa 1/6")),
    tags$div(`data-lejek-caption` = "few", lc_caption(
      "Każda seria to 1000 rzutów i linia pokazująca, jak zmienia się w niej częstość szóstek. Dodaj kilka serii.")),
    tags$div(`data-lejek-caption` = "many", hidden = NA, lc_caption("Każda seria błądzi inaczej, ale prawie wszystkie mieszczą się w lejku. Lejek zwęża się jak 1/√n: stukrotnie więcej rzutów, dziesięciokrotnie węższy pas. Rozrzut między seriami (w pasku) wynosi teoretycznie 0.118, 0.037 i 0.012."))
  )
)

# ============================================================================
# RYC. 1.5 „ROZKŁAD CZY NIE?” — galeria bez suwaków
# ============================================================================

s3p_15b_cards <- list(
  list(title = "Prognoza pogody na jutro", out = c("Słońce", "Deszcz", "Śnieg"), p = c(0.5, 0.3, 0.3),
       ok = FALSE, why = "Suma wynosi 1.1. Któryś z wyników musi się zdarzyć, więc szanse nie mogą razem przekroczyć 1."),
  list(title = "Uczeń przed egzaminem", out = c("Zdam", "Nie zdam"), p = c(1.2, -0.2),
       ok = FALSE, why = "Suma wynosi 1, ale prawdopodobieństwo nie może być ujemne ani większe niż 1."),
  list(title = "Moneta, która może stanąć na kancie", out = c("Orzeł", "Reszka", "Kant"), p = c(0.5, 0.5, 0),
       ok = TRUE, why = "Liczby są nieujemne i sumują się do 1. Zero jest dozwolone: wynik możliwy do nazwania, ale niemożliwy."),
  list(title = "Obciążona kostka", out = c("1–5 (każda)", "6"), p = c(0.1, 0.5), mult = c(5, 1),
       ok = TRUE, why = "Pięć ścianek po 0.1 i szóstka 0.5: razem 5 · 0.1 + 0.5 = 1. Wyniki nie muszą być równo prawdopodobne.")
)

# Karty Ryc. 1.5; show = TRUE dodaje sumę i werdykt.
s3p_15b_grid <- function(show) {
  tags$div(class = "s3p-15b-grid", lapply(seq_along(s3p_15b_cards), function(i) {
    cd <- s3p_15b_cards[[i]]; mult <- cd$mult %||% rep(1, length(cd$p))
    rows <- lapply(seq_along(cd$p), function(j) tags$div(class = "s3p-15b-row",
      tags$span(cd$out[j]),
      tags$span(class = paste("s3p-15b-val", if (cd$p[j] < 0 || cd$p[j] > 1) "is-bad"), lc_fmt(cd$p[j], 2)),
      tags$div(class = "s3p-15b-bar", style = sprintf("width:%s%%;", 100 * min(max(cd$p[j], 0), 1)))))
    tags$div(class = "s3p-15b-card",
      tags$h4(paste0(LETTERS[i], ". ", cd$title)), rows,
      if (show) tagList(
        tags$div(class = "s3p-15b-sum", sprintf("Suma = %s", lc_fmt(sum(cd$p * mult), 2))),
        tags$div(class = paste("s3p-15b-verdict", if (cd$ok) "is-ok" else "is-bad"),
          tags$b(if (cd$ok) "Rozkład." else "Nie rozkład."), " ", cd$why)))
  }))
}

s3p_15b_ui <- s3p_panel("Ryc. 1.5", "Rozkład czy nie?",
  tags$style(HTML("
    .s3p-15b-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 1em; margin: .6em 0; }
    @media (max-width: 640px) { .s3p-15b-grid { grid-template-columns: minmax(0, 1fr); } }
    .s3p-15b-card { border: 1px solid var(--upwr-rule); border-radius: 8px; background: var(--upwr-surface); padding: 14px 18px; }
    .s3p-15b-card h4 { margin: 0 0 .5em; font-size: 1.15em; }
    .s3p-15b-row { display: grid; grid-template-columns: 8.5em 4.2em 1fr; align-items: center; gap: .6em; margin: .3em 0; font-size: 1.08em; }
    .s3p-15b-val { font-family: var(--upwr-mono); font-size: 1.15em; font-weight: 700; text-align: right; }
    .s3p-15b-val.is-bad { color: var(--upwr-accent); }
    .s3p-15b-bar { height: .8em; background: var(--upwr-secondary); border-radius: 2px; }
    .s3p-15b-sum { margin-top: .6em; padding-top: .45em; border-top: 1px solid var(--upwr-rule); font-family: var(--upwr-mono); }
    .s3p-15b-verdict { margin-top: .5em; padding: .5em .7em; border-radius: 6px; }
    .s3p-15b-verdict.is-ok { background: var(--upwr-sage-tint); }
    .s3p-15b-verdict.is-bad { background: var(--upwr-accent-tint); }
    .s3p-15b-verdict b { font-size: 1.1em; }
  ")),
  lc_toolbar(lc_segmented("s3p_15b_show", "Werdykty", choices = c("Ukryj" = "hide", "Pokaż" = "show"))),
  # Karty bez werdyktów i z werdyktami są w dokumencie; przełącza je segment.
  lc_variants("s3p_15b_show", selected = "hide",
    list(hide = s3p_15b_grid(FALSE), show = s3p_15b_grid(TRUE)))
)
