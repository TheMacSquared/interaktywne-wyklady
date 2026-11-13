# PUNKTY STOP WYKŁADU 03 — wspólne elementy ----------------------------------
#
# Ryciny-punkty stop są w plikach stops_ch1.R … stops_ch6.R (rozdziały)
# i stops_ec.R (ryciny ECharts z danymi liczonymi w R). Prefiks s3p_ w nazwach.
# Wymaga: palette.R, theme_upwr.R, shared.R, lecture_layout.R, helpers.R.

# Panel punktu stop: plakietka „Ryc. X.Y”, tytuł, treść.
s3p_panel <- function(kind, title, ..., width_mode = "wide") {
  figure_panel(label = kind, title = title, width_mode = width_mode, ...)
}

# Tekst kroku widgetu krokowego: lista napisów (albo funkcji bez argumentów).
s3p_step_text <- function(output, id, step, texts) {
  output[[paste0(id, "_text")]] <- renderUI({
    t <- texts[[step()]]
    if (is.function(t)) t() else t
  })
}

# Karta przykładu: sytuacja, liczby, wynik, decyzja (rodzaj „Przykłady”).
s3p_card <- function(title, situation, numbers, result, decision) {
  tags$div(class = "s3p-card",
    tags$h4(title),
    tags$p(situation),
    tags$div(class = "s3p-calc", numbers),
    tags$p(tags$strong("Wynik:"), " ", result),
    tags$p(class = "s3p-verdict", tags$strong("Decyzja:"), " ", decision)
  )
}

s3p_pct <- function(x, digits = 1) paste0(lc_fmt(100 * x, digits), "%")

s3p_css <- tags$style(HTML("
  .s3p-card { border: 1px solid var(--upwr-rule); border-left: 4px solid var(--upwr-single-alt, #b48a2a);
    background: var(--upwr-surface); padding: 12px 16px; border-radius: 6px; margin: .2em 0 .6em; }
  .s3p-card h4 { margin: 0 0 .3em; font-size: 1.08em; }
  .s3p-card p { margin: .25em 0; }
  .s3p-calc { font-family: var(--upwr-mono); font-size: .98em; margin: .4em 0; line-height: 1.5; }
  .s3p-verdict { color: var(--upwr-accent); }
  .s3p-dedic { display: inline-block; margin-top: .5em; font-size: .92em; color: var(--upwr-ink-soft); }
  .s3p-svg { display: block; width: 100%; height: auto; margin: .3em auto; }
  .s3p-svg text { font-family: var(--upwr-sans); fill: var(--upwr-ink); }
  .s3p-node rect { fill: var(--upwr-surface); stroke-width: 2.2; }
  .s3p-node text { font-size: 14px; font-weight: 600; text-anchor: middle; }
  .s3p-edge { fill: none; stroke: var(--upwr-ink-subtle); stroke-width: 1.6; }
  .s3p-node .s3p-node-sub { font-size: 11.5px; font-weight: 400; fill: var(--upwr-ink-soft) !important; }
  .s3p-link { stroke-dasharray: 5 4; }
  .s3p-edge-lab { font-size: 11.5px; fill: var(--upwr-ink-soft) !important; text-anchor: middle; }
  .s3p-group { font-size: 12px; font-weight: 700; letter-spacing: .06em; fill: var(--upwr-ink-subtle) !important; }
"))
