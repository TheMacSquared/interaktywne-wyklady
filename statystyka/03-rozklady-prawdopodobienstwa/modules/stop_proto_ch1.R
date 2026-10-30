# PROTOTYPY PUNKTÓW STOP — rozdział 1 (2026-10-29) ---------------------------
#   s3p_11a_ui — Ryc. 1.1 „Stos kostek” (zaakceptowany 2026-10-29)
#   s3p_11c_ui — Ryc. 1.2 „Lejek” (zaakceptowany 2026-10-29)
#   s3p_15b_ui — Ryc. 1.5 „Rozkład czy nie?” (zaakceptowany 2026-10-29; zastąpił „Zbuduj własny rozkład”)
#   Oba zastąpiły dawną Ryc. 1.1 (dwa wykresy w jednym panelu). Usunięte:
#   1a „Kto wejdzie następny?” (dublował Ryc. 1.1), 1.1-B „Cała sala rzuca”,
#   1b „Kursy bukmachera”, 1c „Kostka uczciwa i obciążona” (prawie to samo co Ryc. 1.1–1.2).
# Serwer: s3p_ch1_server(input, output, session).

# ============================================================================
# RYC. 1.1 I 1.2 — częstości z danych dążą do rozkładu (dawna Ryc. 1.1 w dwóch krokach)
# ============================================================================

# (1.1-A) „Stos kostek”: każdy rzut to kafelek w kolumnie swojej ścianki.
s3p_11a_tiles_max <- 60

s3p_11a_ui <- s3p_panel("1.1-A", "Ryc. 1.1", "Stos kostek", proto = FALSE,
  lc_toolbar(
    lc_action_group(label = "Rzuć kostką", s3p_11a_r1 = "+1", s3p_11a_r6 = "+6",
                    s3p_11a_r60 = "+60", s3p_11a_r600 = "+600"),
    lc_action("s3p_11a_reset", icon = "reset", variant = "ghost", aria_label = "Wyzeruj rzuty"),
    lc_readouts(uiOutput("s3p_11a_read"))
  ),
  lc_plot("s3p_11a_plot", ratio = "2.2/1"),
  uiOutput("s3p_11a_caption")
)

# (1.1-C) „Lejek”: częstość szóstki w wielu seriach na tle 1/6 ± 2·SE.
s3p_11c_len <- 1000
# Punkty na skali log, w których rysujemy serie (Chart.js, lejek_chart.js).
s3p_11c_nn <- unique(round(exp(seq(0, log(s3p_11c_len), length.out = 200))))

s3p_11c_ui <- s3p_panel("1.1-C", "Ryc. 1.2", "Lejek", proto = FALSE,
  lc_toolbar(
    lc_action_group(label = "Seria 1000 rzutów", s3p_11c_s1 = "+1", s3p_11c_s10 = "+10"),
    lc_action("s3p_11c_reset", icon = "reset", variant = "ghost", aria_label = "Wyzeruj serie"),
    lc_readouts(uiOutput("s3p_11c_read"))
  ),
  tags$div(class = "lc-lejek", style = "position:relative; aspect-ratio:2.2/1;",
    tags$canvas(`data-role` = "lejek",
                `aria-label` = "Lejek: częstość szóstek w seriach rzutów na tle pasa 1/6")),
  uiOutput("s3p_11c_caption")
)

# ============================================================================
# RYC. 1.5 „ROZKŁAD CZY NIE?” — galeria bez suwaków (prototyp 1.5-B)
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

s3p_15b_ui <- s3p_panel("1.5-B", "Ryc. 1.5", "Rozkład czy nie?", proto = FALSE,
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
  uiOutput("s3p_15b_cards")
)

# ============================================================================
# SERWER
# ============================================================================

s3p_ch1_server <- function(input, output, session) {

  # --- 1.1-A ----------------------------------------------------------------
  rolls_a <- reactiveVal(integer(0))
  add_a <- function(n) rolls_a(c(rolls_a(), sample.int(6, n, replace = TRUE)))
  observeEvent(input$s3p_11a_r1, add_a(1))
  observeEvent(input$s3p_11a_r6, add_a(6))
  observeEvent(input$s3p_11a_r60, add_a(60))
  observeEvent(input$s3p_11a_r600, add_a(600))
  observeEvent(input$s3p_11a_reset, rolls_a(integer(0)))

  output$s3p_11a_read <- renderUI({
    r <- rolls_a()
    tagList(lc_readout("Rzuty", length(r)),
            lc_readout("Ostatni", if (length(r)) tail(r, 1) else "–", color = upwr_accent))
  })

  zoom_plot_server("s3p_11a_plot", reactive({
    r <- rolls_a(); n <- length(r)
    if (n == 0) {
      return(ggplot() + annotate("text", 3.5, 0, label = "Rzuć kostką: każdy rzut położy kafelek nad swoją ścianką",
        colour = upwr_reference, size = 5) + theme_void())
    }
    if (n <= s3p_11a_tiles_max) {
      d <- data.frame(face = r, h = ave(r, r, FUN = seq_along), last = seq_len(n) == n)
      ymax <- max(10, max(d$h) + 1)
      return(ggplot(d, aes(face, h)) +
        geom_tile(aes(fill = last), width = 0.8, height = 0.86, colour = "white") +
        geom_text(aes(label = face), colour = "white", fontface = "bold", size = 3.6) +
        geom_hline(yintercept = n / 6 + 0.5, linetype = "22", colour = upwr_accent) +
        annotate("text", x = 6.55, y = n / 6 + 0.5, label = "n/6", colour = upwr_accent, vjust = -0.4,
                 hjust = 0, fontface = "bold") +
        scale_fill_manual(values = c(`FALSE` = upwr_secondary, `TRUE` = upwr_accent), guide = "none") +
        scale_x_continuous(breaks = 1:6, limits = c(0.5, 7)) +
        scale_y_continuous(breaks = function(l) pretty(c(0, l[2]))) +
        coord_cartesian(ylim = c(0.5, ymax)) +
        labs(x = "Liczba oczek", y = "Liczba rzutów",
             subtitle = "Każdy kafelek to jeden rzut · linia: tyle, ile każdej ścianki „należałoby się”") +
        theme_upwr(grid = "y"))
    }
    d <- data.frame(face = 1:6, f = tabulate(r, 6) / n)
    ggplot(d, aes(face, f)) +
      geom_col(fill = upwr_secondary, alpha = 0.85, width = 0.8) +
      geom_text(aes(label = lc_fmt(f, 3)), vjust = -0.5) +
      geom_hline(yintercept = 1/6, linetype = "22", colour = upwr_accent) +
      annotate("text", x = 6.55, y = 1/6, label = "1/6", colour = upwr_accent, vjust = -0.4, hjust = 0, fontface = "bold") +
      scale_x_continuous(breaks = 1:6, limits = c(0.5, 7)) +
      coord_cartesian(ylim = c(0, 0.3)) +
      labs(x = "Liczba oczek", y = "Częstość względna",
           subtitle = sprintf("%d rzutów: kafelków za dużo, słupki pokazują odsetki", n)) +
      theme_upwr(grid = "y")
  }))

  output$s3p_11a_caption <- renderUI({
    n <- length(rolls_a())
    if (n == 0) return(NULL)
    lc_caption(if (n <= 12) "Kilka rzutów: kolumny są nierówne, niektórych ścianek jeszcze nie ma. Z tak małej próby nie odgadniemy modelu."
      else if (n <= s3p_11a_tiles_max) "Kolumny rosną w różnym tempie, ale wszystkie trzymają się blisko linii n/6. Bezwzględne różnice wysokości nie znikają, za to rosną wolniej niż same kolumny."
      else "W skali odsetków różnice topnieją: słupki przylegają do 1/6. Częstości z danych zbliżają się do rozkładu prawdopodobieństwa.")
  })

  # --- 1.1-C ----------------------------------------------------------------
  series_c <- reactiveVal(list())
  add_c <- function(k) series_c(c(series_c(), lapply(seq_len(k), function(i)
    cumsum(sample.int(6, s3p_11c_len, replace = TRUE) == 6) / seq_len(s3p_11c_len))))
  observeEvent(input$s3p_11c_s1, add_c(1))
  observeEvent(input$s3p_11c_s10, add_c(10))
  observeEvent(input$s3p_11c_reset, series_c(list()))

  output$s3p_11c_read <- renderUI({
    s <- series_c()
    sd_at <- function(n) if (length(s) >= 5) lc_fmt(sd(vapply(s, `[`, 0, n)), 3) else "–"
    tagList(lc_readout("Serie", length(s)),
            lc_readout("Rozrzut przy n = 10", sd_at(10)),
            lc_readout("n = 100", sd_at(100)),
            lc_readout("n = 1000", sd_at(1000), color = upwr_accent))
  })

  # Wykres rysuje lejek_chart.js; serwer wysyła punkty n i częstości każdej serii.
  observe({
    s <- series_c()
    session$sendCustomMessage("lejek_series", list(
      n      = s3p_11c_nn,
      series = lapply(s, function(f) unname(f[s3p_11c_nn]))
    ))
  })

  output$s3p_11c_caption <- renderUI({
    lc_caption(if (length(series_c()) < 5)
      "Każda seria to 1000 rzutów i linia pokazująca, jak zmienia się w niej częstość szóstek. Dodaj kilka serii."
    else "Każda seria błądzi inaczej, ale prawie wszystkie mieszczą się w lejku. Lejek zwęża się jak 1/√n: stukrotnie więcej rzutów, dziesięciokrotnie węższy pas. Rozrzut między seriami (w pasku) wynosi teoretycznie 0.118, 0.037 i 0.012.")
  })

  # --- 1.5-B ----------------------------------------------------------------
  output$s3p_15b_cards <- renderUI({
    show <- identical(input$s3p_15b_show, "show")
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
  })
}
