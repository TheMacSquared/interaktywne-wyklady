# PROTOTYPY PUNKTÓW STOP (2026-11-14) — P17, P18A, P18B: domknięcia wykładu 10 ----------
#
#   i10p_17_ui  — Notatka na jednej stronie (P17): pięć punktów notatki decyzyjnej,
#                 każdy z odsyłaczem do wzoru lub definicji, oraz przedział P(TOP) po
#                 działaniu na stałej osi z kreską stanu bazowego i limitu (SVG z R).
#   i10p_18a_ui — Od danych do decyzji (P18A): scena z sześcioma kadrami, po jednym na
#                 ogniwo łańcucha dane → parametr → model → decyzja; wyniki liczone w R.
#   i10p_18b_ui — Cztery liczby wracają (P18B): cztery kartki z biurka (scena P1) ze
#                 zdaniami i warunkami; statyczna siatka HTML.
# Wymaga: stop_proto_common.R (i10p_cfg, i10p_mission, i10p_p, i10p_svg, i10p_axis,
# i10p_css, i10p_frames_ui) oraz stop_proto_ra.R (i10p1_notes, i10p1_post — dane sceny P1).
# Liczby tylko z i10p_mission() i i10p_cfg, format i10p_p(); w tekstach przez sprintf.

# ============================================================================
# WSPÓLNE LICZBY DOMKNIĘCIA
# ============================================================================

i10pf_base <- i10p_mission()
i10pf_top <- i10pf_base$top
# Roczne ryzyko: 1 − (1 − P(TOP))^(liczba misji w roku), wzór 10.9.
i10pf_year <- 1 - (1 - i10pf_top)^i10p_cfg$missions

# Ograniczenie źródła ciepła w trzech scenariuszach: m = 1 − u, 1, 1 + u.
i10pf_scen <- c(optymistyczny = 1 - i10p_cfg$u, bazowy = 1, ostrożny = 1 + i10p_cfg$u)
i10pf_prev <- vapply(i10pf_scen, function(m) i10p_top("prevention", stress = m), numeric(1))
i10pf_worst <- max(i10pf_prev)
i10pf_met <- i10pf_worst <= i10p_cfg$limit
i10pf_status <- if (i10pf_met) "spełniony" else "niespełniony"

i10pf_iv <- function(id) Filter(function(x) identical(x$id, id), i10p_cfg$interventions)[[1]]
i10pf_rec <- i10pf_iv("prevention")
i10pf_in_budget <- i10pf_rec$cost <= i10p_cfg$budget

# Liczba słowna dla „z trzech misji” (odmiana dopasowana do tekstu).
i10pf_word <- function(n) c("1" = "jednej", "2" = "dwóch", "3" = "trzech", "4" = "czterech")[[as.character(n)]]

# ============================================================================
# P17 — NOTATKA NA JEDNEJ STRONIE
# ============================================================================

i10p17_css <- tags$style(HTML("
  .i10p17-card { background: var(--upwr-surface); border: 1px solid var(--upwr-rule);
    border-top: 3px solid var(--upwr-ink); border-radius: 6px; padding: 1em 1.2em; margin: .4em 0; }
  .i10p17-head { font-family: var(--upwr-mono); font-size: .75em; letter-spacing: .06em;
    color: var(--upwr-ink-subtle); border-bottom: 1px solid var(--upwr-rule); padding-bottom: .5em; margin-bottom: .2em; }
  .i10p17-rows { list-style: none; margin: 0; padding: 0; }
  .i10p17-row { display: grid; grid-template-columns: 1.6em 9.2em minmax(0, 1fr) auto; gap: .2em .8em;
    align-items: start; padding: .7em 0; border-bottom: 1px solid var(--upwr-rule); }
  .i10p17-row:last-child { border-bottom: none; }
  .i10p17-num { font-family: var(--upwr-mono); font-weight: 700; color: var(--upwr-ink-subtle); }
  .i10p17-lbl { font-size: .95em; }
  .i10p17-text { font-size: .95em; line-height: 1.45; }
  .i10p17-src { font-family: var(--upwr-mono); font-size: .72em; color: var(--upwr-ink-subtle); white-space: nowrap;
    border: 1px solid var(--upwr-rule); border-radius: 999px; padding: .15em .6em; justify-self: start; margin-top: .15em; }
  .i10p17-fig { margin: .8em 0 0; }
  @media (max-width: 720px) {
    .i10p17-row { grid-template-columns: 1.6em minmax(0, 1fr); }
    .i10p17-num { grid-row: span 3; }
    .i10p17-lbl, .i10p17-text, .i10p17-src { grid-column: 2; }
  }
"))

i10p17_rows <- list(
  list(lbl = "Wynik",
    text = sprintf("P(TOP) ≈ %s na misję, czyli około %s na 10 000 misji; około %s rocznie przy %s misjach.",
      i10p_p(i10pf_top), i10p_num(1e4 * i10pf_top, 0), i10p_p(i10pf_year), i10pf_word(i10p_cfg$missions)),
    src = "wzory 10.8, 10.9"),
  list(lbl = "Rekomendacja",
    text = sprintf("%s (koszt %s, %s). Ryzyko po działaniu: %s / %s / %s w trzech scenariuszach.",
      i10pf_rec$label, i10p_num(i10pf_rec$cost, 0),
      if (i10pf_in_budget) paste0("w budżecie ", i10p_num(i10p_cfg$budget, 0))
        else paste0("ponad budżet ", i10p_num(i10p_cfg$budget, 0)),
      i10p_p(i10pf_prev[[1]]), i10p_p(i10pf_prev[[2]]), i10p_p(i10pf_prev[[3]])),
    src = "wzory 10.10, 10.11"),
  list(lbl = "Kryterium",
    text = if (i10pf_met)
        sprintf("Limit demonstracyjny %s na misję jest spełniony we wszystkich scenariuszach; najgorszy wynik %s.",
          i10p_p(i10p_cfg$limit), i10p_p(i10pf_worst))
      else
        sprintf("Limit demonstracyjny %s na misję nie jest spełniony we wszystkich scenariuszach; najgorszy wynik %s.",
          i10p_p(i10p_cfg$limit), i10p_p(i10pf_worst)),
    src = "definicja 10.4"),
  list(lbl = "Założenia i granice",
    text = sprintf("Brak napraw w misji, elementy nowe, D i S niezależne przy I; skuteczność działania %s%% to hipoteza bez pilotażu.",
      i10p_num(100 * i10p_cfg$efficacy, 0)),
    src = "definicje 10.2, 10.3"),
  list(lbl = "Właściciel i kontrola",
    text = "Kierownik utrzymania; pomiar częstości zapotrzebowania po pierwszych misjach, przegląd po każdej zmianie instalacji.",
    src = "definicja 10.6")
)

# Oś stała: 0 do 5 działek; działka = limit / 4, więc limit leży na czwartej kresce.
i10p17_svg <- function() {
  step <- i10p_cfg$limit / 4
  axis_max <- 5 * step
  x_of <- function(p) 50 + p / axis_max * 680
  ticks <- (0:5) * step
  lo <- min(i10pf_prev); hi <- max(i10pf_prev); mid <- i10pf_prev[["bazowy"]]
  i10p_svg(800, 120,
    "Przedział P(TOP) po ograniczeniu źródła ciepła, z kreską stanu bazowego i limitem na tej samej osi.",
    i10p_axis(x_of, 92, ticks, labels = format(ticks, nsmall = 4, scientific = FALSE)),
    sprintf('<line style="stroke:var(--upwr-sage)" stroke-width="8" stroke-linecap="round" x1="%g" x2="%g" y1="56" y2="56"/>',
      x_of(lo), x_of(hi)),
    sprintf('<circle cx="%g" cy="56" r="5" style="fill:var(--upwr-sage);stroke:var(--upwr-surface);stroke-width:2"/>', x_of(mid)),
    sprintf('<line class="i10p-base" x1="%g" x2="%g" y1="30" y2="92"/>', x_of(i10pf_base$top), x_of(i10pf_base$top)),
    sprintf('<text class="i10p-mono i10p-subtle" x="%g" y="22" font-size="12" text-anchor="end">bez działania</text>',
      x_of(i10pf_base$top) - 6),
    sprintf('<line class="i10p-limit" x1="%g" x2="%g" y1="30" y2="92"/>', x_of(i10p_cfg$limit), x_of(i10p_cfg$limit)),
    sprintf('<text class="i10p-mono" x="%g" y="22" font-size="12" text-anchor="start">limit</text>',
      x_of(i10p_cfg$limit) + 6),
    sprintf('<text class="i10p-mono i10p-subtle" x="%g" y="82" font-size="12" text-anchor="middle">po działaniu: %s–%s</text>',
      (x_of(lo) + x_of(hi)) / 2, i10p_p(lo), i10p_p(hi))
  )
}

i10p_17_ui <- i10p_panel("P17", "domknięcie", "Notatka na jednej stronie", i10p_css, i10p17_css,
  tags$div(class = "i10p17-card",
    tags$div(class = "i10p17-head",
      sprintf("NOTATKA DECYZYJNA · ochrona termiczna komory · misja %s %s",
        i10p_num(i10p_cfg$time, 0), i10p_cfg$unit)),
    tags$ol(class = "i10p17-rows",
      lapply(seq_along(i10p17_rows), function(i) {
        r <- i10p17_rows[[i]]
        tags$li(class = "i10p17-row",
          tags$span(class = "i10p17-num", i),
          tags$strong(class = "i10p17-lbl", r$lbl),
          tags$span(class = "i10p17-text", r$text),
          tags$span(class = "i10p17-src", r$src))
      })),
    tags$div(class = "i10p17-fig", i10p17_svg())
  )
)

# ============================================================================
# P18A — SCENA „OD DANYCH DO DECYZJI” (KADRY)
# ============================================================================

i10p18a_names <- c("Teczka", "Karty", "Układ", "Drzewo", "Scenariusze", "Notatka")
i10p18a_src <- c("wykłady 01–02", "wykłady 03, 07", "wykład 08", "wykład 09", "wzory 10.10–10.11", "definicja 10.6")
i10p18a_stage <- c("dane", "parametr", "model", "model", "decyzja", "decyzja")

# Wyniki ogniw: wiersze tekstu w ogniwie i klasa każdego wiersza.
i10p18a_res <- list(
  list(lines = c(sprintf("misja %s %s", i10p_num(i10p_cfg$time, 0), i10p_cfg$unit),
      c("P(I) =", i10p_p(i10p_cfg$init))),
    cls = c("", "", "")),
  list(lines = c("P(D | I) =", i10p_p(1 - i10p_cfg$sens),
      sprintf("R(%s) =", i10p_num(i10p_cfg$time, 0)), i10p_p(i10p_fan_r(i10p_cfg$time, "exp"))),
    cls = c("", "", "", "")),
  list(lines = sprintf("R_sys = %s", i10p_p(i10pf_base$system_r)), cls = ""),
  list(lines = c("P(TOP) =", i10p_p(i10pf_top)), cls = c("", "")),
  list(lines = c("po działaniu", paste0(i10p_p(min(i10pf_prev)), "–"), i10p_p(max(i10pf_prev))),
    cls = c("", "", "")),
  list(lines = c(paste("limit", i10p_p(i10p_cfg$limit)), i10pf_status),
    cls = c("", if (i10pf_met) "i10p-ok" else "i10p-bad"))
)

i10p18a_frames <- list(
  list(name = "Teczka",
    text = "Teczka ustala jednostkę i warunki: jedna misja, zapotrzebowanie na początku, elementy nowe.",
    read = NULL),
  list(name = "Karty",
    text = "Karty dają liczby cząstkowe: przeoczenia detektora i niezawodność wentylatora.",
    read = NULL),
  list(name = "Układ",
    text = "Schemat układu sprowadza cztery elementy do jednej liczby dla wspólnego czasu misji.",
    read = NULL),
  list(name = "Drzewo",
    text = sprintf("Drzewo łączy zapotrzebowanie, detekcję i chłodzenie: około %s misji na 10 000 bez ochrony.",
      i10p_num(1e4 * i10pf_top, 0)),
    read = NULL),
  list(name = "Scenariusze",
    text = "Ograniczenie źródła ciepła obniża ryzyko; wynik podajemy jako zakres w trzech scenariuszach.",
    read = NULL),
  list(name = "Notatka",
    text = "Notatka porównuje najgorszy scenariusz z kryterium i zapisuje założenia dla kogoś, kto nie widział rachunku.",
    read = sprintf("%s ≤ %s", i10p_p(i10pf_worst), i10p_p(i10p_cfg$limit)))
)

i10p18a_left <- function(j) 16 + (j - 1) * 130
i10p18a_top <- 76
i10p18a_w <- 118
i10p18a_h <- 150

# Ogniwo j w kadrze k: wcześniejsze zwykłe z wynikiem, bieżące wyróżnione, późniejsze przygaszone.
i10p18a_box <- function(j, k) {
  x <- i10p18a_left(j); cx <- x + i10p18a_w / 2; y <- i10p18a_top
  state <- if (j < k) "done" else if (j == k) "now" else "todo"
  rect_cls <- c(done = "i10p-box", now = "i10p-box-ok", todo = "i10p-box-dim")[[state]]
  name_cls <- if (state == "todo") "i10p-subtle" else ""
  res <- i10p18a_res[[j]]
  paste0(
    sprintf('<rect class="%s" x="%g" y="%g" width="%g" height="%g" rx="6"/>', rect_cls, x, y, i10p18a_w, i10p18a_h),
    sprintf('<text class="%s" x="%g" y="%g" font-size="14" font-weight="700" text-anchor="middle">%s</text>',
      name_cls, cx, y + 30, i10p18a_names[j]),
    if (state != "todo") paste0(
      paste(vapply(seq_along(res$lines), function(i) sprintf(
        '<text class="i10p-mono %s" x="%g" y="%g" font-size="12" text-anchor="middle">%s</text>',
        res$cls[i], cx, y + 64 + 18 * (i - 1), res$lines[i]), character(1)), collapse = ""),
      sprintf('<text class="i10p-subtle" x="%g" y="%g" font-size="11" text-anchor="middle">%s</text>',
        cx, y + 138, i10p18a_src[j])) else ""
  )
}

# Pasy etapów nad ogniwami: dane, parametr, model (2 ogniwa), decyzja (2 ogniwa).
i10p18a_stages <- function() {
  paste(vapply(unique(i10p18a_stage), function(s) {
    idx <- which(i10p18a_stage == s)
    x1 <- i10p18a_left(min(idx)) + 4
    x2 <- i10p18a_left(max(idx)) + i10p18a_w - 4
    paste0(
      sprintf('<line class="i10p-axis" x1="%g" x2="%g" y1="58" y2="58"/>', x1, x2),
      sprintf('<line class="i10p-axis" x1="%g" x2="%g" y1="52" y2="58"/>', x1, x1),
      sprintf('<line class="i10p-axis" x1="%g" x2="%g" y1="52" y2="58"/>', x2, x2),
      sprintf('<text class="i10p-mono i10p-subtle" x="%g" y="46" font-size="12" text-anchor="middle">%s</text>',
        (x1 + x2) / 2, s))
  }, character(1)), collapse = "")
}

# Strzałki między ogniwami.
i10p18a_arrows <- function() {
  mid <- i10p18a_top + i10p18a_h / 2
  paste(vapply(1:5, function(j) {
    xr <- i10p18a_left(j) + i10p18a_w; xl <- i10p18a_left(j + 1)
    paste0(
      sprintf('<line class="i10p-link" x1="%g" x2="%g" y1="%g" y2="%g"/>', xr + 2, xl - 2, mid, mid),
      sprintf('<polyline class="i10p-link" points="%g,%g %g,%g %g,%g"/>', xl - 6, mid - 4, xl - 2, mid, xl - 6, mid + 4))
  }, character(1)), collapse = "")
}

i10p18a_svg <- function(k) {
  aria <- sprintf("Łańcuch dane, parametr, model, decyzja: kadr %d z %d, ogniwo „%s”.",
    k, length(i10p18a_names), i10p18a_names[k])
  i10p_svg(800, 240, aria,
    i10p18a_stages(),
    paste(vapply(seq_along(i10p18a_names), i10p18a_box, character(1), k = k), collapse = ""),
    i10p18a_arrows())
}

i10p_18a_ui <- i10p_frames_ui("P18A", "domknięcie kursu (kadry)", "Od danych do decyzji",
  i10p18a_frames, i10p18a_svg)

# ============================================================================
# P18B — CZTERY LICZBY WRACAJĄ (KARTKI Z SCENY P1)
# ============================================================================

i10p18b_css <- tags$style(HTML("
  .i10p18b-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: .8em; margin: .4em 0; }
  .i10p18b-card { background: var(--upwr-surface); border: 1px solid var(--upwr-rule); border-radius: 6px;
    padding: .9em 1em 1em; display: flex; flex-direction: column; gap: .4em; }
  .i10p18b-card.is-analyst { border: 2px solid var(--upwr-sage); }
  .i10p18b-who { font-family: var(--upwr-mono); font-size: .75em; letter-spacing: .06em; color: var(--upwr-ink-subtle); }
  .i10p18b-num { font-family: var(--upwr-mono); font-size: 2em; font-weight: 700; line-height: 1.1; }
  .i10p18b-say { margin: 0; line-height: 1.45; }
  .i10p18b-f { white-space: nowrap; }
  .i10p18b-pill { align-self: flex-start; margin-top: auto; font-family: var(--upwr-mono); font-size: .72em;
    color: var(--upwr-ink-subtle); border: 1px solid var(--upwr-rule); border-radius: 999px; padding: .15em .6em; }
  @media (max-width: 640px) { .i10p18b-grid { grid-template-columns: minmax(0, 1fr); } }
"))

i10p18b_say <- c(
  sprintf("Wśród alarmów %s jest fałszywych: <span class='i10p18b-f'>P(I | alarm) = %s</span>. To miara zaufania do alarmu; do drzewa idzie <span class='i10p18b-f'>1 − czułość = %s</span>.",
    i10p1_notes[[1]]$num, i10p_p(i10p1_post), i10p_p(1 - i10p_cfg$sens)),
  sprintf("Gdy jest zapotrzebowanie, układ nie wytrzymuje misji w %s przypadków: <span class='i10p18b-f'>P(S | I) = %s</span>.",
    i10p1_notes[[2]]$num, i10p_p(i10pf_base$cooling_failure)),
  sprintf("Gdyby układ musiał pracować w każdej z %s misji, zawiódłby w %s lat. Roczne ryzyko utraty ochrony to %s.",
    i10pf_word(i10p_cfg$missions), i10p1_notes[[3]]$num, i10p_p(i10pf_year)),
  sprintf("Wśród wszystkich misji %s na 10 000 kończy się bez ochrony: P(TOP) = %s.",
    i10p1_notes[[4]]$num, i10p_p(i10pf_top))
)
i10p18b_pills <- c("karta 1 · poza drzewem", "liść drzewa", "pułapka: zgubiony warunek I", "punkt 1 notatki")

i10p_18b_ui <- i10p_panel("P18B", "domknięcie kursu", "Cztery liczby wracają", i10p_css, i10p18b_css,
  tags$div(class = "i10p18b-grid",
    lapply(seq_along(i10p1_notes), function(j) {
      n <- i10p1_notes[[j]]
      tags$article(class = paste("i10p18b-card", if (j == length(i10p1_notes)) "is-analyst"),
        tags$span(class = "i10p18b-who", n$who),
        tags$span(class = "i10p18b-num", n$num),
        tags$p(class = "i10p18b-say", HTML(i10p18b_say[j])),
        tags$span(class = "i10p18b-pill", i10p18b_pills[j]))
    })),
  lc_caption("Liczba jest tak wiarygodna jak zdanie, które ją opisuje.")
)
