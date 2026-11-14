# PROTOTYPY PUNKTÓW STOP (2026-11-14) — P2A, P2B, P5A, P5B, P21, P22 ---------------
#
#   s8p_2a_ui — Galeria „Sto misji, trzy logiki” (P2A): kratki 81 / 9 / 9 / 1
#               z przełącznikiem logiki; 19, 10 i 1 zawiedziona misja (przykład 8.1).
#   s8p_2b_ui — Symulacja „Misja za misją” (P2B): scena „misje” (modules/proto_rc.js).
#   s8p_5a_ui — Piaskownica „Zepsuj blok, szukaj drogi” (P5A): klikalny schemat,
#               trzy architektury pod przełącznikiem.
#   s8p_5b_ui — to samo, trzy schematy obok siebie (P5B).
#   s8p_21_ui — Scena „Trzy kopie, jedna serwerownia” (P21): układ kopii × zdarzenie.
#   s8p_22_ui — Galeria „Kiedy element rozstrzyga?” (P22): istotność Birnbauma
#               na schemacie układu mieszanego (przykład 8.11).
# Wymaga: stop_proto_common.R.

# ============================================================================
# P2A — STO MISJI, TRZY LOGIKI
# ============================================================================

s8p2_r <- 0.9
# Sto misji według stanu dwóch elementów: oba działają, tylko E₁, tylko E₂, żaden.
s8p2_n <- round(100 * c(both = s8p2_r^2, e1 = s8p2_r * (1 - s8p2_r), e2 = (1 - s8p2_r) * s8p2_r, none = (1 - s8p2_r)^2))
s8p2_logic <- list(
  series = list(stages = list(1, 2), fail = c("e1", "e2", "none"), text = "System wymaga obu elementów. Zawiedziona jest każda misja, w której zawiódł choć jeden."),
  single = list(stages = list(1), fail = c("e2", "none"), text = "System wymaga tylko E₁. Zawiedzione są misje, w których E₁ stał, bez względu na E₂."),
  parallel = list(stages = list(c(1, 2)), fail = "none", text = "Wystarczy którykolwiek element. Zawiedziona jest tylko misja, w której stały oba.")
)
s8p2_failed <- function(key) sum(s8p2_n[s8p2_logic[[key]]$fail])

s8p2_svg <- function(key) {
  lg <- s8p2_logic[[key]]
  cls <- function(g) if (g %in% lg$fail) "s8p-cell-bad" else "s8p-cell-ok"
  cols <- c(both = 9, e1 = 1, e2 = 1, none = 1)
  xs <- c(both = 30, e1 = 256, e2 = 326, none = 396)
  heads <- c(both = "oba działają", e1 = "tylko E₁", e2 = "tylko E₂", none = "żaden")
  groups <- paste(vapply(names(s8p2_n), function(g) paste0(
    s8p_text(xs[[g]] + cols[[g]] * 10 - 2, 28, heads[[g]], 13, "s8p-subtle", "middle"),
    s8p_cells(stats::setNames(s8p2_n[[g]], cls(g)), cols[[g]], xs[[g]], 42, size = 16, gap = 4),
    s8p_text(xs[[g]] + cols[[g]] * 10 - 2, 244, s8p2_n[[g]], 15, "s8p-mono", "middle", 700)
  ), character(1)), collapse = "")
  s8p_svg(800, 256, sprintf("Sto misji w czterech grupach: 81, 9, 9 i 1. W tej logice zawiedzionych jest %d.", s8p2_failed(key)),
    groups,
    s8p_rbd(lg$stages, c("E₁", "E₂"), x = 480, y = 80, scale = .85, lamp = NULL),
    s8p_text(480, 190, s8p2_failed(key), 54, "s8p-mono s8p-bad", weight = 700),
    s8p_text(480 + if (s8p2_failed(key) > 9) 76 else 44, 190, "na 100 misji zawiedzionych", 15, "s8p-subtle"))
}
s8p2_views <- lapply(names(s8p2_logic), function(key) list(svg = s8p2_svg(key), text = s8p2_logic[[key]]$text,
  read = sprintf("R = %s", s8p_fix(1 - s8p2_failed(key) / 100, 2))))
names(s8p2_views) <- names(s8p2_logic)

s8p_2a_ui <- s8p_views_ui("P2A", "galeria", "Sto misji, trzy logiki",
  s8p_seg("view", "Logika sukcesu", c("Oba wymagane" = "series", "Tylko E₁" = "single", "Wystarczy jeden" = "parallel")),
  s8p2_views,
  lc_caption("Kratka to jedna misja dwóch elementów o R = 0.9. Kratki stoją w miejscu; przełącznik zmienia tylko to, które z nich liczą się jako porażka."))

# ============================================================================
# P2B — MISJA ZA MISJĄ (symulacja w przeglądarce)
# ============================================================================

s8p_2b_ui <- s8p_scene_ui("P2B", "symulacja", "Misja za misją", "misje",
  list(r = s8p2_r, seed = 81, exact = list(series = 0.19, single = 0.10, parallel = 0.01)),
  tags$div(class = "s8p-nav",
    lc_group("Przeprowadź misje", tags$div(class = "lc-seg", role = "group", `aria-label` = "Przeprowadź misje",
      tags$button(type = "button", `data-s8p` = "add", `data-value` = "1", "+1"),
      tags$button(type = "button", `data-s8p` = "add", `data-value` = "10", "+10"),
      tags$button(type = "button", `data-s8p` = "add", `data-value` = "100", "+100"),
      tags$button(type = "button", `data-s8p` = "add", `data-value` = "1000", "+1000"))),
    s8p_button("clear", "Od nowa", icon = "reset")),
  s8p_out("stage"), s8p_out("body"),
  lc_caption("Każda kolumna kratek to jedna misja: te same stany E₁ i E₂ ocenione w trzech logikach. Pasek pokazuje ostatnie 100 misji; przerywane kreski to 19, 10 i 1 na 100 z przykładu 8.1."))

# ============================================================================
# P5A i P5B — ZEPSUJ BLOK, SZUKAJ DROGI
# ============================================================================

s8p5_archs <- list(
  series = list(title = "Szeregowy", labels = c("Czujnik", "Sterownik", "Wentylator"), stages = list(1, 2, 3)),
  parallel = list(title = "Równoległy", labels = c("Wentylator A", "Wentylator B"), stages = list(c(1, 2))),
  mixed = list(title = "Mieszany", labels = c("Sterownik C", "Wentylator A", "Wentylator B"), stages = list(1, c(2, 3)))
)
s8p5_arch <- function(key, h = 190) {
  a <- s8p5_archs[[key]]
  list(n = length(a$labels),
    draw = function(up) s8p_svg(680, h, sprintf("Schemat blokowy, układ %s. %s", tolower(a$title),
        if (s8p_rbd_works(a$stages, up)) "Lampka świeci." else "Lampka zgaszona."),
      s8p_rbd_center(340, h / 2 - 8, stages = a$stages, labels = a$labels, up = up, click = TRUE, bw = 132)),
    status = function(up) {
      ok <- s8p_rbd_works(a$stages, up); down <- sum(!up)
      tags$p(class = "s8p-read", sprintf("zepsute bloki: %d · %s", down,
        if (ok) "droga istnieje, lampka świeci" else "nie ma drogi, lampka zgaszona"))
    })
}
s8p5_keys <- stats::setNames(names(s8p5_archs), vapply(s8p5_archs, function(a) a$title, ""))

s8p_5a_ui <- s8p_panel("P5A", "piaskownica", "Zepsuj blok, szukaj drogi", s8p_css,
  s8p_rbd_scene_multi(lapply(stats::setNames(nm = names(s8p5_archs)), s8p5_arch), s8p_seg("arch", "Układ", s8p5_keys)),
  lc_caption("Kliknij blok, żeby go zepsuć; drugi klik go naprawia. Linia świeci tam, dokąd da się dojść od wejścia przez działające bloki."))

s8p_5b_ui <- s8p_panel("P5B", "galeria", "Trzy schematy obok siebie", s8p_css,
  tags$div(class = "s8p-stack", lapply(names(s8p5_archs), function(key) {
    a <- s8p5_arch(key, h = if (key == "series") 110 else 150)
    tags$div(tags$h4(s8p5_archs[[key]]$title), s8p_rbd_scene(a$n, a$draw, a$status))
  })),
  lc_caption("Kliknij blok, żeby go zepsuć; drugi klik go naprawia. Każdy schemat ma własny stan."))

# ============================================================================
# P21 — TRZY KOPIE, JEDNA SERWEROWNIA
# ============================================================================

# Kopie: nośnik podłączony do sieci (online) i miejsce (site = w serwerowni).
s8p21_setups <- list(
  same = list(sub = rep("dysk · serwerownia", 3), online = c(TRUE, TRUE, TRUE), site = c(TRUE, TRUE, TRUE)),
  rule = list(sub = c("dysk · serwerownia", "taśma · serwerownia", "dysk · inne miasto"), online = c(TRUE, FALSE, TRUE), site = c(TRUE, TRUE, FALSE))
)
# Które kopie przeżywają zdarzenie.
s8p21_up <- function(setup, event) {
  s <- s8p21_setups[[setup]]
  switch(event, none = rep(TRUE, 3), disk = c(FALSE, TRUE, TRUE), fire = !s$site, ransom = !s$online)
}
s8p21_text <- list(
  same = c(none = "Trzy kopie na trzech dyskach w jednej serwerowni. Na schemacie to trzy gałęzie równoległe.",
    disk = "Pada dysk z kopią 1. Pozostałe dwie kopie tego nie odczuwają: to awaria niezależna.",
    fire = "Pożar serwerowni niszczy wszystkie trzy kopie naraz. Trzy gałęzie miały jedną wspólną przyczynę.",
    ransom = "Ransomware szyfruje wszystko, co jest podłączone do sieci: trzy dyski naraz."),
  rule = c(none = "Trzy kopie, dwa rodzaje nośnika, jedna poza serwerownią. Liczba gałęzi ta sama.",
    disk = "Pada dysk z kopią 1. Pozostałe dwie kopie tego nie odczuwają: to awaria niezależna.",
    fire = "Pożar niszczy obie kopie w serwerowni. Kopia w innym mieście zostaje.",
    ransom = "Ransomware szyfruje oba dyski podłączone do sieci. Taśma leży odłączona na półce i zostaje.")
)
s8p21_svg <- function(setup, event) {
  s <- s8p21_setups[[setup]]; up <- s8p21_up(setup, event)
  ok <- any(up)
  # geometria jak w s8p_rbd(): blok j na wysokości y + (j − 2)·(bh + vgap), lewa krawędź x + lead + pad
  x <- 190; y <- 122; bw <- 190; bh <- 48; vgap <- 16
  inside <- which(s$site); top <- y + (min(inside) - 2) * (bh + vgap) - bh / 2 - 14
  bottom <- y + (max(inside) - 2) * (bh + vgap) + bh / 2 + 14
  s8p_svg(800, 250, sprintf("Trzy kopie zapasowe jako układ równoległy. %s", if (ok) "Dane ocalały." else "Dane utracone."),
    sprintf('<rect x="%g" y="%g" width="%g" height="%g" rx="10" fill="none" stroke="var(--upwr-ink-subtle)" stroke-width="1.4" stroke-dasharray="6 5"/>',
      x + 30 + 24 - 16, top, bw + 32, bottom - top),
    s8p_text(x + 30 + 24 - 16, top - 6, "serwerownia", 12, "s8p-subtle"),
    s8p_rbd(list(c(1, 2, 3)), paste("Kopia", 1:3), sub = s$sub, up = up, x = x, y = y, bw = bw, lamp = "dane"),
    s8p_chip(590, 128, if (ok) "dane ocalały" else "dane utracone", if (ok) "ok" else "bad", w = 170))
}
s8p21_views <- list()
for (.setup in names(s8p21_setups)) for (.event in c("none", "disk", "fire", "ransom")) {
  s8p21_views[[paste(.setup, .event, sep = "|")]] <- list(svg = s8p21_svg(.setup, .event),
    text = s8p21_text[[.setup]][[.event]],
    read = sprintf("ocalałe kopie: %d z 3", sum(s8p21_up(.setup, .event))))
}

s8p_21_ui <- s8p_views_ui("P21", "scena", "Trzy kopie, jedna serwerownia",
  tagList(s8p_seg("view", "Gdzie leżą kopie", c("Trzy w serwerowni" = "same", "Reguła 3-2-1" = "rule")),
    s8p_seg("view", "Zdarzenie", c("Spokój" = "none", "Awaria dysku" = "disk", "Pożar serwerowni" = "fire", "Ransomware" = "ransom"))),
  s8p21_views)

# ============================================================================
# P22 — KIEDY ELEMENT ROZSTRZYGA? (istotność Birnbauma, przykład 8.11)
# ============================================================================

# Bloki: 1 = C, 2 = A, 3 = B.
s8p22_lab <- c("Sterownik C", "Wentylator A", "Wentylator B")
s8p22_sub <- s8p_fix(c(s8p_r_c, s8p_r_a, s8p_r_b), 2)
s8p22_stages <- list(1, c(2, 3))
s8p22_ib <- c(C = s8p_r_ab, A = s8p_r_c * (1 - s8p_r_b), B = s8p_r_c * (1 - s8p_r_a))   # 0.996, 0.049, 0.0784
# Stan pozostałych, w którym wybrany element rozstrzyga (dla C: najczęstszy z trzech).
s8p22_others <- list(C = c(NA, TRUE, TRUE), A = c(TRUE, NA, FALSE), B = c(TRUE, FALSE, NA))
s8p22_text <- c(
  C = "Sterownik leży na każdej drodze. Rozstrzyga zawsze, gdy działa choć jeden wentylator: I_B(C) = R_AB = 0.996.",
  A = "Wentylator A rozstrzyga tylko wtedy, gdy sterownik działa, a wentylator B już stoi: I_B(A) = 0.98 · 0.05 = 0.049.",
  B = "Wentylator B rozstrzyga tylko wtedy, gdy sterownik działa, a wentylator A już stoi: I_B(B) = 0.98 · 0.08 = 0.0784."
)
s8p22_svg <- function(el) {
  i <- match(el, c("C", "A", "B")); base <- s8p22_others[[el]]
  one <- function(cx, state, head) {
    up <- base; up[i] <- state
    paste0(s8p_text(cx, 22, head, 13, "s8p-subtle", "middle"),
      s8p_rbd_center(cx, 100, stages = s8p22_stages, labels = s8p22_lab, sub = s8p22_sub, up = up, mark = i,
        scale = .78, lamp = NULL, bw = 128))
  }
  bars <- paste(vapply(seq_along(s8p22_ib), function(j) {
    y <- 196 + (j - 1) * 30; sel <- j == i; w <- 440 * s8p22_ib[[j]]
    paste0(
      s8p_text(150, y + 5, s8p22_lab[c(1, 2, 3)][j], 14, if (!sel) "s8p-subtle", "end", if (sel) 700),
      sprintf('<rect class="s8p-sunken" x="164" y="%g" width="440" height="16" rx="3"/>', y - 8),
      sprintf('<rect x="164" y="%g" width="%g" height="16" rx="3" fill="%s"/>', y - 8, max(w, 2),
        if (sel) "var(--upwr-single-alt)" else "var(--upwr-ink-subtle)"),
      s8p_text(616, y + 5, s8p_num(s8p22_ib[[j]], 4), 14, "s8p-mono", weight = if (sel) 700))
  }, character(1)), collapse = "")
  s8p_svg(800, 290, sprintf("Układ mieszany z zaznaczonym elementem %s w stanie, w którym rozstrzyga o systemie. Istotność Birnbauma %s.", el, s8p_num(s8p22_ib[[el]], 4)),
    one(210, TRUE, "element działa: lampka świeci"), one(590, FALSE, "element stoi: lampka gaśnie"),
    '<line class="s8p-rule" x1="400" x2="400" y1="14" y2="160"/>',
    s8p_text(164, 180, "istotność Birnbauma: jak często ten element rozstrzyga", 12, "s8p-subtle"),
    bars)
}
s8p22_views <- lapply(c(C = "C", A = "A", B = "B"), function(el) list(svg = s8p22_svg(el), text = s8p22_text[[el]],
  read = sprintf("zysk z poprawy tego elementu o 0.01: %s", s8p_fix(0.01 * s8p22_ib[[el]], 5))))

s8p_22_ui <- s8p_views_ui("P22", "galeria", "Kiedy element rozstrzyga?",
  s8p_seg("view", "Element w złotej ramce", c("Sterownik C" = "C", "Wentylator A" = "A", "Wentylator B" = "B")),
  s8p22_views)
