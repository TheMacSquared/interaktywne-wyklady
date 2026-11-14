# PROTOTYPY PUNKTÓW STOP (2026-11-14) — wspólne narzędzia wykładu 10 -------------
#
# Numery jak w PUNKTY-STOP-PROPOZYCJA.md. Prototypy działają w przeglądarce, bez
# serwera R (także offline):
#   - sceny z kadrami: R rysuje SVG każdego kadru z góry (i10p_frames_ui),
#     modules/sceny.js tylko przełącza kadry;
#   - sceny interaktywne: rysunek i stan w modules/proto_*.js (window.lcI10.register),
#     R podaje dane i teksty w data-config (i10p_scene_ui);
#   - wykresy: ryciny lcSF (R/lc_stat_figures.js, ECharts) rejestrowane w
#     modules/proto_*.js, kontener lc_sf_figure() w R.
# Każdy plik stop_proto_r*.R definiuje obiekty UI i10p_<nr>_ui; wpięcie w block.R
# przez i10p_get("i10p_<nr>_ui") — brakujący obiekt daje panel „w budowie”.
#
# PARAMETRY MISJI stoją w jednym miejscu: i10p_cfg (niżej), liczone z
# bananpol$integration, bananpol$lifetime i bananpol$interventions. Do JS trafiają
# w data-config; pliki proto_*.js nie mają wpisanych liczb modelu. Rachunek misji
# w R: i10p_mission() (opakowanie risk_mission_analysis()), w JS: lcI10.util.mission().
# Po unifikacji misji z danymi Bananpolu (TODO.md, „10 — od modelu do decyzji”)
# wystarczy zmienić i10p_cfg i jednostkę i10p_cfg$unit.
#
# Wspólny język wizualny: stała oś P(TOP) na misję z czarną przerywaną kreską
# limitu i szarą kreską stanu bazowego; drzewo I – (D ∪ S) z liczbami przy liściach;
# 10 000 misji jako częstości naturalne; kartki z biurka (P1, P18B).

i10p_cfg <- list(
  unit = "h",                                        # jednostka czasu misji
  time = 1000,                                       # misja bazowa
  base_time = 1000,                                  # czas, dla którego podano R zasilania i sterownika
  time_max = 3000,                                   # zakres suwaków czasu i osi czasu
  mttf = bananpol$lifetime$exponential_mttf,         # model wykładniczy wentylatora
  beta = bananpol$lifetime$weibull_shape,            # Weibull: kształt
  eta = bananpol$lifetime$weibull_scale,             # Weibull: skala
  power_r = bananpol$integration$power_r1000,
  controller_r = bananpol$integration$controller_r1000,
  init = bananpol$integration$initiation,
  sens = bananpol$integration$sensitivity,
  fpr = bananpol$integration$false_positive_rate,
  efficacy = bananpol$integration$intervention_efficacy,
  missions = 3,                                      # misje w roku (wzór 10.9)
  limit = .002,                                      # demonstracyjny limit P(TOP) na misję
  budget = 2,                                        # budżet bazowy
  u = .2,                                            # bazowy zakres niepewności
  u_max = .5,
  top_axis = .004,                                   # stała oś P(TOP) w rycinach
  models = list(exp = "wykładniczy", weibull = "Weibull"),
  interventions = lapply(seq_len(nrow(bananpol$interventions)), function(i) list(
    id = bananpol$interventions$id[i], label = bananpol$interventions$label[i],
    cost = bananpol$interventions$cost_index[i]))
)

# Rachunek misji na parametrach i10p_cfg (te same domyślne co widgety wykładu).
i10p_mission <- function(intervention = "none", stress = 1, model = "exp", time = i10p_cfg$time,
                         sens = i10p_cfg$sens, init = i10p_cfg$init) {
  risk_mission_analysis(time, model, i10p_cfg$power_r, i10p_cfg$controller_r, init, sens, intervention, stress)
}
i10p_top <- function(...) i10p_mission(...)$top
# Niezawodność wentylatora w obu modelach (wzór 10.4) z parametrów i10p_cfg.
i10p_fan_r <- function(t, model = "exp") {
  if (model == "exp") exp(-t / i10p_cfg$mttf) else exp(-(t / i10p_cfg$eta)^i10p_cfg$beta)
}
# risk_mission_analysis() ma parametry wentylatora wpisane na sztywno; pilnujemy,
# żeby i10p_cfg opisywał ten sam model.
stopifnot(
  abs(i10p_fan_r(i10p_cfg$time, "exp") - i10p_mission()$fan_r) < 1e-12,
  abs(i10p_fan_r(i10p_cfg$time, "weibull") - i10p_mission(model = "weibull")$fan_r) < 1e-12
)

# Panel prototypu: złota plakietka „Prototyp P4 · piaskownica”. Po akceptacji
# proto = FALSE daje zwykłą figurę z etykietą rodzaju.
i10p_panel <- function(num, kind, title, ..., width_mode = "wide", proto = TRUE) {
  body <- tags$div(`data-proto` = num, ...)
  if (!proto) return(figure_panel(label = kind, title = title, width_mode = width_mode, body))
  figure_panel(label = paste0("Prototyp ", num, " · ", kind), title = title,
    color = upwr_single_alt, width_mode = width_mode, body)
}

# Obiekt prototypu po nazwie; gdy pliku jeszcze nie ma, panel zastępczy.
i10p_get <- function(name) {
  obj <- get0(name, inherits = TRUE)
  if (!is.null(obj)) return(obj)
  figure_panel(label = "Prototyp · w budowie", title = name, color = upwr_single_alt,
    width_mode = "text", lc_caption("Ten punkt stop nie jest jeszcze zbudowany."))
}

# Podpis wariantu nad panelem (np. to samo miejsce w dwóch wersjach do porównania).
i10p_variant <- function(text) lc_caption(tags$strong("Wariant:"), " ", text, tone = "info")

i10p_css <- tags$style(HTML("
  .i10p-svg { display: block; width: 100%; height: auto; margin: .2em auto .4em; }
  .i10p-svg text { font-family: var(--upwr-sans); }
  .i10p-svg text:not([fill]):not([style*='fill']) { fill: var(--upwr-ink); }
  .i10p-mono { font-family: var(--upwr-mono) !important; }
  .i10p-subtle { fill: var(--upwr-ink-subtle) !important; }
  .i10p-axis { stroke: var(--upwr-ink-subtle); stroke-width: 1.2; fill: none; }
  .i10p-grid { stroke: var(--upwr-rule); stroke-width: 1; fill: none; }
  .i10p-tick { font-family: var(--upwr-mono) !important; font-size: 12px; fill: var(--upwr-ink-subtle) !important; text-anchor: middle; }
  .i10p-limit { stroke: var(--upwr-ink); stroke-width: 1.6; stroke-dasharray: 6 4; fill: none; }
  .i10p-base { stroke: var(--upwr-ink-subtle); stroke-width: 1.4; stroke-dasharray: 3 3; fill: none; }
  .i10p-link { stroke: var(--upwr-ink-subtle); stroke-width: 1.6; fill: none; }
  .i10p-paper { fill: var(--upwr-surface); stroke: var(--upwr-rule); stroke-width: 1.2; }
  .i10p-box { fill: var(--upwr-surface); stroke: var(--upwr-ink-subtle); stroke-width: 1.4; }
  .i10p-box-hit { fill: color-mix(in srgb, var(--upwr-accent) 14%, var(--upwr-surface)); stroke: var(--upwr-accent); stroke-width: 2.2; }
  .i10p-box-ok { fill: color-mix(in srgb, var(--upwr-sage) 18%, var(--upwr-surface)); stroke: var(--upwr-sage); stroke-width: 1.8; }
  .i10p-box-dim { fill: var(--upwr-surface-sunken); stroke: var(--upwr-rule); stroke-width: 1.2; }
  .i10p-fill-bad { fill: var(--upwr-accent); stroke: none; }
  .i10p-fill-ok { fill: var(--upwr-sage); stroke: none; }
  .i10p-fill-warn { fill: var(--upwr-cat-kurkuma); stroke: none; }
  .i10p-fill-idle { fill: var(--upwr-surface-sunken); stroke: var(--upwr-rule); stroke-width: 1; }
  .i10p-fill-sec { fill: var(--upwr-secondary); stroke: none; }
  /* !important: reguła .i10p-svg text:not([fill]) ma wyższą specyficzność niż sama klasa. */
  .i10p-ok { fill: var(--upwr-sage) !important; }
  .i10p-bad { fill: var(--upwr-accent) !important; }
  .i10p-ink { fill: var(--upwr-ink) !important; }
  .i10p-inv { fill: var(--upwr-surface) !important; }
  .i10p-head { display: flex; justify-content: space-between; align-items: baseline; gap: 1em; margin-bottom: .3em; }
  .i10p-name { font-weight: 700; font-size: 1.05em; }
  .i10p-step { font-family: var(--upwr-mono); font-size: .9em; color: var(--upwr-ink-subtle); }
  .i10p-caption { min-height: 3em; margin: .4em 0; }
  .i10p-read { font-family: var(--upwr-mono); font-weight: 600; margin: .2em 0 .6em; }
  .i10p-nav { display: flex; gap: .6em; flex-wrap: wrap; align-items: center; }
  .i10p-nav .lc-seg button[aria-pressed='true'] { background: var(--upwr-accent); color: var(--upwr-surface); }
  .i10p-slider { min-width: min(100%, 14em); flex: 1 1 14em; }
  .i10p-slider input[type='range'] { width: 100%; accent-color: var(--upwr-accent); cursor: pointer; }
  .i10p-slider output { display: inline !important; font-family: var(--upwr-mono); color: var(--upwr-ink); }
"))

# Owija fragmenty SVG w <svg> o szerokości w (viewBox w × h).
i10p_svg <- function(w, h, aria, ...) {
  HTML(paste0(sprintf('<svg class="i10p-svg" style="max-width:%dpx" viewBox="0 0 %d %d" role="img" aria-label="%s">', w, w, h, aria),
    paste(unlist(list(...)), collapse = ""), "</svg>"))
}

# Liczba z kropką dziesiętną, bez końcowych zer (do tekstów i SVG).
i10p_num <- function(x, digits = 3) format(round(x, digits), nsmall = 0, scientific = FALSE, trim = TRUE)
# Prawdopodobieństwo jak w odczytach wykładu (risk_fmt_p): 0.513, 0.00162.
i10p_p <- function(x, digits = 3L) vapply(x, risk_fmt_p, character(1), digits = digits)

# Pozioma oś z podziałką: zwraca fragment SVG. x_of: funkcja wartość → piksel.
i10p_axis <- function(x_of, y, ticks, label = NULL, labels = format(ticks)) {
  paste0(
    sprintf('<line class="i10p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', x_of(min(ticks)), x_of(max(ticks)), y, y),
    paste(sprintf('<line class="i10p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="i10p-tick" x="%g" y="%g">%s</text>',
      x_of(ticks), x_of(ticks), y, y + 5, x_of(ticks), y + 18, labels), collapse = ""),
    if (!is.null(label)) sprintf('<text class="i10p-subtle" x="%g" y="%g" font-size="12" text-anchor="end">%s</text>',
      x_of(max(ticks)), y + 34, label) else ""
  )
}

# Odczyt ryciny lcSF wypełniany przez JS: S.setValue(root, key, tekst).
i10p_read <- function(key, label, color = NULL, text = "–") {
  lc_readout(label, lc_sf_value(key, text), color = color, swatch = !is.null(color))
}

# ============================================================================
# SCENY W PRZEGLĄDARCE (modules/sceny.js, window.lcI10)
# ============================================================================

# Przycisk sceny: wygląd lc_action(), bez wejścia Shiny — klik obsługuje scena
# po atrybucie data-i10p (scene.on[act]).
i10p_button <- function(act, label = NULL, icon = NULL, variant = "outline", aria_label = NULL) {
  b <- lc_action(paste0("i10p-", act), label, icon = icon, variant = variant, aria_label = aria_label)
  b$attribs$id <- NULL
  b$attribs$class <- sub("action-button ", "", b$attribs$class, fixed = TRUE)
  b$attribs[["data-i10p"]] <- act
  b
}

i10p_prev_next <- function() tagList(i10p_button("prev", "Wstecz", icon = "prev"),
  i10p_button("next", "Dalej", icon = "next", variant = "solid"))

# Przełącznik sceny: przyciski data-i10p = act, data-value = wartość; scena dostaje
# kliknięty przycisk w scene.on[act](btn) i sama ustawia aria-pressed
# (lcI10.util.press(btn)). choices: c("Etykieta" = "wartość").
i10p_seg <- function(act, label, choices, selected = choices[[1]]) {
  lc_group(label, tags$div(class = "lc-seg", role = "group", `aria-label` = label,
    lapply(seq_along(choices), function(i) tags$button(type = "button", `data-i10p` = act,
      `data-value` = unname(choices[[i]]),
      `aria-pressed` = if (identical(unname(choices[[i]]), unname(selected))) "true" else "false",
      names(choices)[[i]]))))
}

# Suwak sceny (natywny, bez Shiny): scena czyta go przez lcI10.util.sliderValue(root, id);
# odczyt wartości przy etykiecie uzupełnia silnik scen.
i10p_slider <- function(id, label, min, max, value, step, suffix = NULL) {
  tags$div(class = "lc-grp i10p-slider",
    tags$label(`for` = id, class = "lc-grp-l", label, " ", tags$output(`data-value` = id, value), suffix),
    tags$input(id = id, type = "range", min = min, max = max, value = value, step = step))
}

# Kontener sceny: ... to treść w kolejności (sloty i10p_out() i zwykłe tagi).
i10p_scene_ui <- function(num, kind, title, scene, config, ..., width_mode = "wide") {
  i10p_panel(num, kind, title, width_mode = width_mode, i10p_css,
    tags$div(class = "i10p-scene", `data-scene` = scene,
      `data-config` = jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA), ...))
}
i10p_out <- function(name, ...) tags$div(`data-i10p-out` = name, ...)

# Scena z kadrami policzonymi w R. frames: lista list(name, text, read); svg(i)
# zwraca rysunek kadru i (i10p_svg).
i10p_frames_ui <- function(num, kind, title, frames, svg, body = i10p_frame_body, width_mode = "wide") {
  cfg <- list(frames = lapply(seq_along(frames), function(i) list(
    name = frames[[i]]$name,
    svg = as.character(svg(i)),
    body = as.character(body(frames[[i]]))
  )))
  i10p_scene_ui(num, kind, title, "kadry", cfg,
    i10p_out("head"), i10p_out("stage"), i10p_out("body"),
    tags$div(class = "i10p-nav", i10p_prev_next()), width_mode = width_mode)
}

# Treść kadru: opis i (gdy jest) odczyt.
i10p_frame_body <- function(f) {
  tagList(tags$p(class = "i10p-caption", f$text),
    if (!is.null(f$read) && !(is.character(f$read) && !nzchar(f$read))) tags$p(class = "i10p-read", f$read))
}
