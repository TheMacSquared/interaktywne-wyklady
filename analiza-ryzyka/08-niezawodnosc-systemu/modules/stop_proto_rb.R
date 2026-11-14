# PROTOTYPY PUNKTÓW STOP (2026-11-14) — kafle stanów: P6, P8, P12, P13, P14, P20 ---
#
# Jeden obraz w sześciu rolach: wszystkie stany elementów jako małe schematy
# blokowe. Kafel świeci, gdy w tym stanie system działa (lampka na wyjściu).
#   s8p_6_ui  — „Osiem stanów, jedna wygrana” (szereg trzech elementów)
#   s8p_8_ui  — „Cztery stany, jedna przegrana” (dwa wentylatory; przełącznik na szereg)
#   s8p_12_ui — „Tabela stanów jako osiem schematów” (układ mieszany, przykład 8.7)
#   s8p_13_ui — „Między szeregiem a układem równoległym” (oszacowanie 8.9)
#   s8p_14_ui — „Naprawa, która gasi system” (układ niekoherentny)
#   s8p_20_ui — „k z n na kaflach” (trzy czujniki, r = 0.9)
# Wszystko liczone w R; przełączniki obsługuje scena „widoki” (modules/sceny.js).
# Wymaga: stop_proto_common.R.

# Mały schemat do kafla: krótkie nazwy bloków, bez podpisu lampki.
s8pb_mini <- function(cx, y, stages, labels, up, k = NULL, scale = .72) {
  s8p_rbd_center(cx, y, stages = stages, labels = labels, up = up, k = k, scale = scale,
    lamp = NULL, bw = 48, bh = 30, vgap = 8, lead = 14, gap = 14, pad = 12)
}

# Siatka kafli. states: macierz stanów (wiersz = stan); lit: czy system działa;
# mini(cx, y, up): rysunek w kaflu; probs: prawdopodobieństwa stanów (NULL = bez);
# order: kolejność współrzędnych w napisie stanu (np. c(1, 2, 3)).
s8pb_tiles <- function(states, lit, mini, probs = NULL, ncol = 4, tw = 188, th = 150, gap = 12,
                       x0 = 6, y0 = 6, digits = 5) {
  paste(vapply(seq_len(nrow(states)), function(i) {
    x <- x0 + ((i - 1) %% ncol) * (tw + gap); y <- y0 + ((i - 1) %/% ncol) * (th + gap)
    up <- states[i, ]
    paste0(
      sprintf('<rect class="s8p-tile %s" x="%g" y="%g" width="%g" height="%g" rx="8"/>', if (lit[i]) "is-lit" else "is-out", x, y, tw, th),
      s8p_text(x + 12, y + 22, sprintf("(%s)", paste(as.integer(up), collapse = ", ")), 13, "s8p-mono s8p-subtle"),
      s8p_text(x + tw - 12, y + 22, if (lit[i]) "φ = 1" else "φ = 0", 13, paste("s8p-mono", if (lit[i]) "s8p-ok" else "s8p-bad"), "end", 700),
      mini(x + tw / 2, y + th / 2 + 4, up),
      if (!is.null(probs)) s8p_text(x + tw / 2, y + th - 12, s8p_fix(probs[i], digits), 15,
        paste("s8p-mono", if (lit[i]) "s8p-ok" else "s8p-subtle"), "middle", if (lit[i]) 700 else 400) else ""
    )
  }, character(1)), collapse = "")
}
# Wysokość siatki kafli.
s8pb_height <- function(n, ncol = 4, th = 150, gap = 12) 12 + ceiling(n / ncol) * (th + gap) - gap

# Prawdopodobieństwo każdego stanu przy niezależnych elementach (wzór 8.8).
s8pb_probs <- function(states, r) apply(states, 1, function(up) prod(ifelse(up, r, 1 - r)))

s8pb_st3 <- s8p_states(3)
s8pb_st2 <- s8p_states(2)

# ============================================================================
# P6 — OSIEM STANÓW, JEDNA WYGRANA (szereg)
# ============================================================================

s8p6_stages <- list(1, 2, 3)
s8p6_lab <- c("Cz", "St", "W")
s8p6_lit <- apply(s8pb_st3, 1, function(up) s8p_rbd_works(s8p6_stages, up))

s8p_6_ui <- s8p_panel("P6", "galeria", "Osiem stanów, jedna wygrana", s8p_css,
  s8p_svg(800, s8pb_height(8), "Osiem stanów szeregu czujnik, sterownik, wentylator jako osiem małych schematów. Lampka świeci tylko w stanie, w którym działają wszystkie trzy elementy.",
    s8pb_tiles(s8pb_st3, s8p6_lit, function(cx, y, up) s8pb_mini(cx, y, s8p6_stages, s8p6_lab, up))),
  tags$p(class = "s8p-caption", "Trzy elementy mogą być w ośmiu stanach. Nawias podaje stany czujnika (Cz), sterownika (St) i wentylatora (W): 1 działa, 0 zawiódł. W szeregu lampka świeci w jednym kaflu."),
  tags$p(class = "s8p-read", sprintf("świeci %d z %d kafli", sum(s8p6_lit), length(s8p6_lit)))
)

# ============================================================================
# P8 — CZTERY STANY, JEDNA PRZEGRANA (dwa wentylatory, przykład 8.4)
# ============================================================================

s8p8_r <- c(s8p_r_a, s8p_r_b)
s8p8_lab <- c("A", "B")
s8p8_prob <- s8pb_probs(s8pb_st2, s8p8_r)
s8p8_view <- function(stages, text, read) {
  lit <- apply(s8pb_st2, 1, function(up) s8p_rbd_works(stages, up))
  list(svg = s8p_svg(800, s8pb_height(4),
      sprintf("Cztery stany dwóch wentylatorów; lampka świeci w %d z nich.", sum(lit)),
      s8pb_tiles(s8pb_st2, lit, function(cx, y, up) s8pb_mini(cx, y, stages, s8p8_lab, up, scale = .85), probs = s8p8_prob, digits = 3)),
    text = text, read = read, r = sum(s8p8_prob[lit]))
}
s8p8_views <- list(
  parallel = s8p8_view(list(c(1, 2)),
    "Wentylatory A (0.92) i B (0.95) równolegle. Pod każdym kaflem stoi prawdopodobieństwo tego stanu. Gaśnie jeden kafel, więc łatwiej policzyć jego niż trzy pozostałe.",
    "zgaszony: 0.08 · 0.05 = 0.004 · R = 1 − 0.004 = 0.996"),
  series = s8p8_view(list(1, 2),
    "Te same wentylatory w szeregu. Teraz świeci jeden kafel i to jego prawdopodobieństwo jest niezawodnością systemu.",
    "świeci: 0.92 · 0.95 = 0.874")
)

s8p_8_ui <- s8p_views_ui("P8", "galeria", "Cztery stany, jedna przegrana",
  s8p_seg("view", "Układ", c("Równoległy" = "parallel", "Szeregowy" = "series")), s8p8_views)

# ============================================================================
# P12 i P13 — UKŁAD MIESZANY NA KAFLACH (przykład 8.7, oszacowanie 8.9)
# ============================================================================

# Bloki: 1 = A, 2 = B, 3 = C; napis stanu w kolejności (x_A, x_B, x_C) jak w przykładzie 8.7.
s8p12_r <- c(s8p_r_a, s8p_r_b, s8p_r_c)
s8p12_lab <- c("A", "B", "C")
s8p12_prob <- s8pb_probs(s8pb_st3, s8p12_r)
s8p12_structs <- list(
  series = list(stages = list(3, 1, 2), name = "szeregowy"),
  mixed = list(stages = list(3, c(1, 2)), name = "mieszany"),
  parallel = list(stages = list(c(3, 1, 2)), name = "równoległy")
)
s8p12_lit <- function(key) apply(s8pb_st3, 1, function(up) s8p_rbd_works(s8p12_structs[[key]]$stages, up))
s8p12_r_of <- function(key) sum(s8p12_prob[s8p12_lit(key)])
s8p12_svg <- function(key) {
  st <- s8p12_structs[[key]]; lit <- s8p12_lit(key)
  s8p_svg(800, s8pb_height(8),
    sprintf("Osiem stanów elementów A, B, C dla układu: %s. Lampka świeci w %d kaflach, suma ich prawdopodobieństw to %s.",
      st$name, sum(lit), s8p_num(sum(s8p12_prob[lit]), 5)),
    s8pb_tiles(s8pb_st3, lit, function(cx, y, up) s8pb_mini(cx, y, st$stages, s8p12_lab, up,
      scale = if (key == "parallel") .7 else .72), probs = s8p12_prob))
}

s8p_12_ui <- s8p_panel("P12", "pojęcie", "Tabela stanów jako osiem schematów", s8p_css,
  s8p12_svg("mixed"),
  tags$p(class = "s8p-caption", "Sterownik C (0.98) w szeregu z wentylatorami A (0.92) i B (0.95). Nawias to (x_A, x_B, x_C), liczba pod schematem to prawdopodobieństwo stanu. Niezawodność systemu jest sumą liczb ze świecących kafli."),
  tags$p(class = "s8p-read", sprintf("%s = %s", paste(s8p_fix(s8p12_prob[s8p12_lit("mixed")], 5), collapse = " + "), s8p_fix(s8p12_r_of("mixed"), 5)))
)

s8p13_views <- lapply(names(s8p12_structs), function(key) {
  n <- sum(s8p12_lit(key))
  list(svg = s8p12_svg(key),
    text = c(
      series = "Szereg tych samych trzech elementów: świeci jeden kafel. To dolna granica dla każdego układu koherentnego.",
      mixed = "Układ mieszany: świecą trzy kafle. Do kafla szeregu doszły dwa stany, w których jeden wentylator zastępuje drugi.",
      parallel = "Układ równoległy: gaśnie tylko kafel, w którym zawiodło wszystko. To górna granica."
    )[[key]],
    read = sprintf("świeci %d z 8 · R = %s", n, s8p_num(s8p12_r_of(key), 5)))
})
names(s8p13_views) <- names(s8p12_structs)

s8p_13_ui <- s8p_views_ui("P13", "galeria", "Między szeregiem a układem równoległym",
  s8p_seg("view", "Układ tych samych elementów", c("Szeregowy" = "series", "Mieszany" = "mixed", "Równoległy" = "parallel"), selected = "series"),
  s8p13_views)

# ============================================================================
# P14 — NAPRAWA, KTÓRA GASI SYSTEM (φ = x_A · (1 − x_B))
# ============================================================================

# Stany w kolejności, w której naprawa B stoi obok: (1, 0) → (1, 1).
s8p14_st <- rbind(c(FALSE, FALSE), c(FALSE, TRUE), c(TRUE, FALSE), c(TRUE, TRUE))
s8p14_lit <- s8p14_st[, 1] & !s8p14_st[, 2]
# Dwa bloki obok siebie bez przewodów (takiego układu nie ma jak połączyć) i lampka.
s8p14_mini <- function(cx, y, up) {
  blk <- function(x, lab, ok) paste0(
    sprintf('<g class="s8p-blk%s"><rect x="%g" y="%g" width="44" height="30" rx="6"/>', if (ok) "" else " is-down", x, y - 15),
    sprintf('<text class="s8p-blk-name" x="%g" y="%g" font-size="14">%s</text></g>', x + 22, y + 5, lab))
  on <- up[1] && !up[2]
  paste0(blk(cx - 74, "A", up[1]), blk(cx - 22, "B", up[2]),
    if (on) sprintf('<circle class="s8p-halo" cx="%g" cy="%g" r="15"/>', cx + 56, y) else "",
    sprintf('<circle class="s8p-lamp%s" cx="%g" cy="%g" r="9"/>', if (on) " is-on" else "", cx + 56, y))
}

s8p_14_ui <- s8p_panel("P14", "przypadek brzegowy", "Naprawa, która gasi system", s8p_css,
  s8p_svg(800, 196, "Cztery stany elementów A i B dla funkcji fi równej x_A razy jeden minus x_B. Lampka świeci tylko w stanie A działa, B zawiódł; po naprawie B gaśnie.",
    s8pb_tiles(s8p14_st, s8p14_lit, s8p14_mini, th = 130),
    # strzałka „naprawa B” od kafla (1, 0) do kafla (1, 1)
    '<path d="M500,160 C540,186 660,186 700,160" fill="none" stroke="var(--upwr-accent)" stroke-width="2.2"/>',
    '<path d="M700,160 l-12,1 l7,9 z" fill="var(--upwr-accent)"/>',
    s8p_text(600, 192, "naprawa B", 13, "s8p-bad", "middle", 700)),
  tags$p(class = "s8p-caption", "φ(x_A, x_B) = x_A · (1 − x_B). W stanie (1, 0) lampka świeci; naprawa B przenosi układ do stanu (1, 1) i ją gasi. Bloków nie łączy żaden przewód, bo takiej logiki nie da się narysować schematem blokowym."),
  tags$p(class = "s8p-read", "φ(1, 0) = 1 · φ(1, 1) = 0")
)

# ============================================================================
# P20 — k Z n NA KAFLACH (trzy czujniki, r = 0.9)
# ============================================================================

s8p20_r <- 0.9
s8p20_lab <- c("Cz₁", "Cz₂", "Cz₃")
s8p20_prob <- s8pb_probs(s8pb_st3, rep(s8p20_r, 3))
s8p20_views <- lapply(1:3, function(k) {
  lit <- rowSums(s8pb_st3) >= k
  r <- sum(s8p20_prob[lit])
  list(svg = s8p_svg(800, s8pb_height(8, th = 170),
      sprintf("Osiem stanów trzech czujników dla układu %d z 3. Lampka świeci w %d kaflach, suma prawdopodobieństw %s.", k, sum(lit), s8p_num(r, 3)),
      s8pb_tiles(s8pb_st3, lit, function(cx, y, up) s8pb_mini(cx, y, list(c(1, 2, 3)), s8p20_lab, up, k = k, scale = .8),
        probs = s8p20_prob, th = 170, digits = 3)),
    text = c("Wystarcza jeden czujnik z trzech: to układ równoległy. Gaśnie tylko kafel, w którym zawiodły wszystkie.",
      "Głosowanie 2 z 3: alarm uznajemy, gdy zgłoszą go co najmniej dwa czujniki. Świecą cztery kafle.",
      "Potrzebne wszystkie trzy: to szereg. Świeci jeden kafel.")[[k]],
    read = sprintf("świeci %d z 8 · R = %s", sum(lit), s8p_fix(r, 3)), r = r)
})
names(s8p20_views) <- c("1", "2", "3")

s8p_20_ui <- s8p_views_ui("P20", "galeria", "k z n: od układu równoległego do szeregu",
  s8p_seg("view", "Ile czujników musi działać", c("1 z 3" = "1", "2 z 3" = "2", "3 z 3" = "3"), selected = "2"),
  s8p20_views)
