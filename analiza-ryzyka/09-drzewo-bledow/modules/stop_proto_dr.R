# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa „dr”: kadry i widoki drzewa ---------
#
# Wszystkie obiekty to sceny „drzewo” (modules/sceny.js): R podaje opis drzewa
# i kadrów, rysunek SVG powstaje w przeglądarce (lcF9.util.tree).
#   f9p_3_ui   — P3  pojęcie (kadry): trzy piętra drzewa i to, co zostaje poza nim
#   f9p_6b_ui  — P6B galeria: osiem kombinacji liści obok siebie
#   f9p_7_ui   — P7  pojęcie: schemat blokowy i drzewo błędów tego samego systemu
#   f9p_8_ui   — P8  pojęcie (kadry): rachunek od liści do korzenia i skala 10 000
#   f9p_11a_ui — P11A galeria: przekroje w przełączniku
#   f9p_11b_ui — P11B galeria: dwa minimalne przekroje obok siebie
#   f9p_13_ui  — P13 pojęcie (kadry): dwa kanały detekcji, jedno zasilanie
#   f9p_15_ui  — P15 pojęcie: dwa światy jednego liścia (istotność Birnbauma)
#   f9p_17_ui  — P17 przypadek brzegowy (kadry): zapomniany zawór
#   f9p_19a_ui — P19A domknięcie: jedno drzewo, pięć warstw
#   f9p_19b_ui — P19B domknięcie: karta z pięcioma wierszami
# Wymaga: stop_proto_common.R (f9p_*), R/risk_math.R, R/bananpol.R.

# ============================================================================
# P3 — TRZY PIĘTRA DRZEWA
# ============================================================================

# Czynniki poza granicą systemu (akapit po definicji 9.2) jako blade kartki obok drzewa.
f9p3_outside <- f9p_svg(230, 300, "Poza granicą systemu: zachowanie ludzi, dostęp straży, rozmieszczenie towaru.",
  '<rect class="f9p-line-dash" x="6" y="6" width="218" height="288" rx="8"/>',
  '<text class="f9p-subtle" x="115" y="34" font-size="13" text-anchor="middle" font-weight="700">POZA GRANICĄ SYSTEMU</text>',
  paste(sprintf('<rect class="f9p-paper" x="28" y="%d" width="174" height="52" rx="5"/><text class="f9p-subtle" x="115" y="%d" font-size="14" text-anchor="middle">%s</text>',
    c(56, 130, 204), c(56, 130, 204) + 31, c("zachowanie ludzi", "dostęp straży", "rozmieszczenie towaru")), collapse = ""))

f9p_3_ui <- f9p_tree_ui("P3", "pojęcie", "Trzy piętra drzewa", list(
  f9p_frame("Szczyt", "Na szczycie stoi jeden niepożądany stan: nieopanowany pożar magazynu w ciągu roku.",
    f9p_view(hide = c("I", "M", "D", "S")), read = "zdarzenie szczytowe (TOP)"),
  f9p_frame("Mechanizm", "Piętro niżej: co musiało się stać? Zapłon oraz zawiedzione zabezpieczenia. „Zabezpieczenia zawiodły” to zdarzenie pośrednie, wynik bramki.",
    f9p_view(hide = c("D", "S"), mark = "M"), read = "zdarzenie pośrednie = wynik bramki"),
  f9p_frame("Liście", "Na dole zdarzenia bazowe: nie rozwijamy ich dalej i przypisujemy im prawdopodobieństwa.",
    f9p_view(mark = c("I", "D", "S"), p = f9p_p_bananpol), read = "zdarzenia bazowe: I, D, S"),
  f9p_frame("Granica", "Tych czynników w drzewie nie ma. Nie są nieistotne, tylko zostały poza granicą systemu z definicji 9.1.",
    f9p_view(p = f9p_p_bananpol, grow = 2), f9p_view(svg = f9p3_outside), read = "gdzie kończy się drzewo, decyduje analityk")
))

# ============================================================================
# P8 — LICZBY WCHODZĄ OD DOŁU
# ============================================================================

# Siatka 100 × 100 magazyno-lat: n_init kratek z inicjacją, z nich n_top nieopanowanych.
# Zaznaczone kratki stoją w lewym górnym rogu w bloku o szerokości 10 kratek.
f9p_grid_svg <- function(n_init, n_top, cell = 3, aria = NULL) {
  side <- 100 * cell
  idx <- seq_len(n_init) - 1L
  rects <- function(i, cls) if (!length(i)) "" else paste(sprintf('<rect class="%s" x="%g" y="%g" width="%g" height="%g"/>',
    cls, (i %% 10) * cell + 1, (i %/% 10) * cell + 1, cell, cell), collapse = "")
  f9p_svg(side + 2, side + 2, aria %||% sprintf("Siatka 10 000 magazyno-lat: %d z inicjacją, %d z nieopanowanym pożarem.", n_init, n_top),
    sprintf('<rect class="f9p-grid-bg" x="1" y="1" width="%d" height="%d"/>', side, side),
    paste(sprintf('<path class="f9p-grid-line" d="M%g,1 V%d M1,%g H%d"/>', 1 + (1:9) * 10 * cell, side + 1, 1 + (1:9) * 10 * cell, side + 1), collapse = ""),
    rects(idx[idx >= n_top], "f9p-grid-init"), rects(idx[idx < n_top], "f9p-grid-top"))
}
f9p_grid_css <- tags$style(HTML("
  .f9p-grid-bg { fill: var(--upwr-surface-sunken); stroke: var(--upwr-rule); stroke-width: 1; }
  .f9p-grid-line { stroke: var(--upwr-rule); stroke-width: .6; fill: none; }
  .f9p-grid-init { fill: var(--upwr-cat-kurkuma); }
  .f9p-grid-top { fill: var(--upwr-accent); }
"))

f9p_8_ui <- tagList(f9p_grid_css, f9p_tree_ui("P8", "pojęcie", "Liczby wchodzą od dołu", list(
  f9p_frame("Liście", "Struktura jest gotowa, więc liczby wchodzą od dołu: każdy liść dostaje swoje prawdopodobieństwo.",
    f9p_view(p = f9p_p_bananpol), read = "P(I) = 0.005 · d = 0.05 · s = 0.08"),
  f9p_frame("Bramka OR", "Najniższa bramka zamienia się w jedną liczbę: bariery zawodzą przy inicjacji, gdy zawiedzie co najmniej jedna.",
    f9p_view(p = f9p_p_bananpol, mark = "M", values = c(M = "0.126")), read = "1 − 0.95 · 0.92 = 0.126"),
  f9p_frame("Bramka AND", "Wynik bramki OR jest wejściem bramki AND piętro wyżej, razem z inicjacją.",
    f9p_view(p = f9p_p_bananpol, mark = "TOP", values = c(M = "0.126", TOP = "0.00063")), read = "0.005 · 0.126 = 0.00063"),
  f9p_frame("Skala", "Kratka to jeden magazyno-rok. Inicjacja zdarza się w 50 z 10 000, a bariery zawodzą w 12.6% z nich.",
    f9p_view(p = f9p_p_bananpol, values = c(M = "0.126", TOP = "0.00063"), grow = 2),
    f9p_view(svg = f9p_grid_svg(50, 6), title = "10 000 magazyno-lat", note = "50 z inicjacją · około 6 nieopanowanych"),
    read = "50 · 0.126 = 6.3 na 10 000 magazyno-lat")
)))

# ============================================================================
# P6B — OSIEM KOMBINACJI LIŚCI
# ============================================================================

f9p6_sets <- list(character(0), "D", "S", c("D", "S"), "I", c("I", "D"), c("I", "S"), c("I", "D", "S"))
f9p6_name <- function(s) if (!length(s)) "∅" else paste0("{", paste(s, collapse = ", "), "}")
f9p6_hit <- function(s) "I" %in% s && any(c("D", "S") %in% s)

f9p_6b_ui <- f9p_tree_ui("P6B", "galeria", "Osiem kombinacji, trzy zapalają szczyt", nav = "none", list(
  list(name = "Osiem kombinacji", panels = lapply(f9p6_sets, function(s)
      f9p_view(title = f9p6_name(s), lit = s, mini = TRUE, hit = f9p6_hit(s))),
    body = as.character(f9p_frame_body(list(
      text = "Każde drzewko to jedna kombinacja liści (I po lewej, D i S pod bramką OR). Szczyt zapala się w trzech z ośmiu i w każdej z nich zachodzi inicjacja.",
      read = "TOP aktywne: {I, D} · {I, S} · {I, D, S}"))))
))

# ============================================================================
# P7 — SCHEMAT BLOKOWY I DRZEWO BŁĘDÓW
# ============================================================================

# Schemat blokowy dwóch elementów: szeregowy albo równoległy (język sukcesu, wykład 08).
f9p7_rbd <- function(type) {
  box <- function(x, y, name, r) paste0(
    sprintf('<rect class="f9p-paper" x="%g" y="%g" width="130" height="54" rx="5"/>', x, y),
    sprintf('<text x="%g" y="%g" font-size="14" text-anchor="middle" font-weight="600">%s</text>', x + 65, y + 23, name),
    sprintf('<text class="f9p-mono f9p-ok" x="%g" y="%g" font-size="13" text-anchor="middle">działa: %s</text>', x + 65, y + 42, r))
  ends <- '<circle class="f9p-dot" cx="14" cy="110" r="5"/><circle class="f9p-dot" cx="406" cy="110" r="5"/>'
  if (type == "series") {
    f9p_svg(420, 220, "Schemat blokowy: detekcja i tłumienie w szeregu.",
      '<path class="f9p-line" d="M14,110 H406"/>', ends, box(55, 83, "Detekcja", "0.95"), box(235, 83, "Tłumienie", "0.92"))
  } else {
    f9p_svg(420, 220, "Schemat blokowy: dwie samodzielne bariery równolegle.",
      '<path class="f9p-line" d="M14,110 H70 M70,50 V170 M70,50 H350 M70,170 H350 M350,50 V170 M350,110 H406"/>', ends,
      box(145, 23, "Bariera 1", "0.95"), box(145, 143, "Bariera 2", "0.92"))
  }
}
f9p7_css <- tags$style(HTML(".f9p-dot { fill: var(--upwr-ink-soft); }"))
f9p7_tree <- function(gate) f9p_gate("M", c("Ochrona zawodzi", "przy inicjacji"), gate,
  f9p_leaf("D", c("Pierwsza", "zawodzi")), f9p_leaf("S", c("Druga", "zawodzi")))

f9p_7_ui <- tagList(f9p7_css, f9p_tree_ui("P7", "pojęcie", "Ten sam system w dwóch językach", nav = "seg", seg_label = "Architektura",
  trees = list(or = f9p7_tree("or"), and = f9p7_tree("and")), list(
  f9p_frame("Obie funkcje wymagane", "Układ szeregowy działa, gdy działają oba elementy. Zawodzi, gdy zawiedzie którykolwiek: to bramka OR awarii.",
    f9p_view(svg = f9p7_rbd("series"), title = "Schemat blokowy: kiedy działa", note = "R = 0.95 · 0.92 = 0.874"),
    f9p_view(tree = "or", title = "Drzewo błędów: kiedy zawodzi", p = c(D = "0.05", S = "0.08"), values = c(M = "0.126"), note = "1 − 0.874 = 0.126"),
    read = "szereg ↔ OR awarii: 0.126"),
  f9p_frame("Dwie samodzielne bariery", "Układ równoległy działa, gdy działa choć jedna gałąź. Zawodzi dopiero, gdy zawiodą obie: to bramka AND awarii.",
    f9p_view(svg = f9p7_rbd("parallel"), title = "Schemat blokowy: kiedy działa", note = "R = 1 − 0.05 · 0.08 = 0.996"),
    f9p_view(tree = "and", title = "Drzewo błędów: kiedy zawodzi", p = c(D = "0.05", S = "0.08"), values = c(M = "0.004"), note = "0.05 · 0.08 = 0.004"),
    read = "równoległy ↔ AND awarii: 0.004")
)))

# ============================================================================
# P11A, P11B — PRZEKRÓJ JAKO ZAPALONA ŚCIEŻKA
# ============================================================================

f9p_11a_ui <- f9p_tree_ui("P11A", "galeria", "Najkrótsze drogi do szczytu", nav = "seg", seg_label = "Zbiór liści", list(
  f9p_frame("{I, D}", "Inicjacja i brak detekcji wystarczają, żeby zapalić szczyt. Zgaś którykolwiek z tych liści, a szczyt gaśnie: przekrój jest minimalny.",
    f9p_view(lit = c("I", "D"), p = f9p_p_bananpol), read = "P(K₁) = 0.005 · 0.05 = 0.00025"),
  f9p_frame("{I, S}", "Druga droga: inicjacja i brak tłumienia. Też minimalna, i też przechodzi przez inicjację.",
    f9p_view(lit = c("I", "S"), p = f9p_p_bananpol), read = "P(K₂) = 0.005 · 0.08 = 0.0004"),
  f9p_frame("{I, D, S}", "Trzy liście też zapalają szczyt, ale jeden jest zbędny: po zgaszeniu S albo D szczyt świeci dalej. To przekrój, lecz nie minimalny.",
    f9p_view(lit = c("I", "D", "S"), p = f9p_p_bananpol), read = "część wspólna K₁ ∩ K₂: 0.005 · 0.05 · 0.08 = 0.00002")
))

f9p_11b_ui <- f9p_tree_ui("P11B", "galeria", "Dwa minimalne przekroje", nav = "none", list(
  list(name = "Dwa przekroje", panels = list(
      f9p_view(title = "K₁ = {I, D}", lit = c("I", "D"), p = f9p_p_bananpol, note = "0.005 · 0.05 = 0.00025", hit = TRUE),
      f9p_view(title = "K₂ = {I, S}", lit = c("I", "S"), p = f9p_p_bananpol, note = "0.005 · 0.08 = 0.0004", hit = TRUE)),
    body = as.character(f9p_frame_body(list(
      text = "Dwie zapalone ścieżki, z których nie da się zgasić żadnego liścia. Obie przechodzą przez inicjację: jej usunięcie wyłącza oba scenariusze naraz.",
      read = "0.00025 + 0.0004 − 0.00002 = 0.00063"))))
))

# ============================================================================
# P13 — DWA KANAŁY DETEKCJI, JEDNO ZASILANIE (przykład 9.6)
# ============================================================================

f9p13_naive <- f9p_gate("ND", c("Brak", "detekcji"), "and",
  f9p_gate("K1", c("Kanał 1", "zawodzi"), "or", f9p_leaf("Z1", "Zasilanie", sym = "Z", ev = "Z"), f9p_leaf("B1", "Czujka 1", sym = "B₁")),
  f9p_gate("K2", c("Kanał 2", "zawodzi"), "or", f9p_leaf("Z2", "Zasilanie", sym = "Z", ev = "Z"), f9p_leaf("B2", "Czujka 2", sym = "B₂")))
f9p13_reduced <- f9p_gate("ND", c("Brak", "detekcji"), "or",
  f9p_leaf("Z", "Zasilanie"),
  f9p_gate("G", c("Obie czujki", "uszkodzone"), "and", f9p_leaf("B1", "Czujka 1", sym = "B₁"), f9p_leaf("B2", "Czujka 2", sym = "B₂")))
f9p13_p <- c(Z = "0.02", B1 = "0.10", B2 = "0.10")
# Liczby z przykładu 9.6 policzone, nie przepisane.
f9p13_channel <- risk_gate_or(c(.02, .10))                  # 0.118
f9p13_wrong <- f9p13_channel^2                              # 0.0139
f9p13_right <- risk_gate_or(c(.02, risk_gate_and(c(.10, .10))))  # 0.0298

f9p_13_ui <- f9p_tree_ui("P13", "pojęcie", "Dwa kanały, jedno zasilanie",
  trees = list(naive = f9p13_naive, red = f9p13_reduced), list(
  f9p_frame("Rysunek", "Dwa kanały czujek, każdy sam wystarcza. Kanał zawodzi, gdy uszkodzi się jego czujka albo zabraknie zasilania. Zasilanie Z jest na rysunku dwa razy.",
    f9p_view(tree = "naive", p = f9p13_p), read = "(Z ∪ B₁) ∩ (Z ∪ B₂)"),
  f9p_frame("Naiwnie", "Rachunek bramka po bramce traktuje dwa koła Z jak dwa niezależne zdarzenia.",
    f9p_view(tree = "naive", p = f9p13_p, mark = c("Z1", "Z2"), values = c(K1 = "0.118", K2 = "0.118", ND = sprintf("%.4f", f9p13_wrong))),
    read = sprintf("0.118 · 0.118 = %.4f", f9p13_wrong)),
  f9p_frame("Jedno zdarzenie", "Zasilanie jest jedno. Gdy go zabraknie, zapalają się oba koła Z naraz, oba kanały i szczyt.",
    f9p_view(tree = "naive", p = f9p13_p, lit = "Z"), read = "Z samo wystarcza: pojedynczy punkt awarii"),
  f9p_frame("Po redukcji", "To samo drzewo po uporządkowaniu: Z występuje raz i prowadzi wprost do szczytu, obok pary czujek.",
    f9p_view(tree = "red", p = f9p13_p, values = c(G = "0.01", ND = sprintf("%.4f", f9p13_right))),
    read = sprintf("0.02 + 0.01 − 0.02 · 0.01 = %.4f", f9p13_right)),
  f9p_frame("Porównanie", "Rysunek z dwoma kanałami obiecywał redundancję. Wynik naiwny jest ponad dwa razy za mały.",
    f9p_view(tree = "naive", title = "Naiwnie", p = f9p13_p, values = c(K1 = "0.118", K2 = "0.118", ND = sprintf("%.4f", f9p13_wrong)), note = sprintf("%.4f", f9p13_wrong), grow = 4),
    f9p_view(tree = "red", title = "Po redukcji", p = f9p13_p, lit = "Z", values = c(G = "0.01", ND = sprintf("%.4f", f9p13_right)), note = sprintf("%.4f", f9p13_right), hit = TRUE, grow = 3),
    read = sprintf("%.4f / %.4f ≈ %.1f", f9p13_right, f9p13_wrong, f9p13_right / f9p13_wrong))
))

# ============================================================================
# P15 — DWA ŚWIATY JEDNEGO LIŚCIA (przykład 9.8)
# ============================================================================

# P(TOP) w świecie, w którym liść na pewno zachodzi (1) albo na pewno nie (0).
f9p15_world <- function(leaf, state) {
  p <- c(I = f9p_pi, D = f9p_d, S = f9p_s); p[leaf] <- state
  risk_fta_top(p[["I"]], p[["D"]], p[["S"]])
}
f9p15_frame <- function(leaf, name, text) {
  a <- f9p15_world(leaf, 1); b <- f9p15_world(leaf, 0)
  p_a <- f9p_p_bananpol; p_a[leaf] <- "1"; p_b <- f9p_p_bananpol; p_b[leaf] <- "0"
  f9p_frame(name, text,
    f9p_view(title = paste(leaf, "na pewno zachodzi"), lit = leaf, p = p_a, values = c(TOP = f9p_num(a)), note = paste("P(TOP) =", f9p_num(a))),
    f9p_view(title = paste(leaf, "na pewno nie zachodzi"), no = leaf, p = p_b, values = c(TOP = f9p_num(b)), note = paste("P(TOP) =", f9p_num(b))),
    read = sprintf("I_B(%s) = %s − %s = %s", leaf, f9p_num(a), f9p_num(b), f9p_num(a - b)))
}

f9p_15_ui <- f9p_tree_ui("P15", "pojęcie", "Dwa światy jednego liścia", nav = "seg", seg_label = "Liść", list(
  f9p15_frame("I", "Inicjacja I", "Z inicjacją szczyt zależy już tylko od barier. Bez inicjacji nie zapali się nigdy."),
  f9p15_frame("D", "Brak detekcji D", "Gdy detekcja na pewno zawodzi, każda inicjacja kończy się pożarem. Gdy na pewno działa, zostaje droga przez tłumienie."),
  f9p15_frame("S", "Brak tłumienia S", "Gdy tłumienie na pewno zawodzi, każda inicjacja kończy się pożarem. Gdy na pewno działa, zostaje droga przez detekcję.")
))

# ============================================================================
# P17 — ZAPOMNIANY ZAWÓR (przykład 9.9)
# ============================================================================

f9p17_tree <- f9p_gate("TOP", c("Nieopanowany pożar", "magazynu w roku"), "and",
  f9p_leaf("I", "Inicjacja"),
  f9p_gate("M", c("Zabezpieczenia", "zawiodły"), "or",
    f9p_leaf("D", c("Brak", "detekcji")), f9p_leaf("S", c("Brak", "tłumienia")), f9p_leaf("V", c("Zawór", "zamknięty"))))
f9p17_p <- c(f9p_p_bananpol, V = "0.02")
f9p17_or <- risk_gate_or(c(f9p_d, f9p_s, .02))   # 0.14348
f9p17_top <- f9p_pi * f9p17_or                   # 0.000717

f9p_17_ui <- f9p_tree_ui("P17", "przypadek brzegowy", "Zapomniany zawór", trees = list(main = f9p17_tree), list(
  f9p_frame("Przed przeglądem", "Drzewo, które zna zespół. Rachunek jest bezbłędny dla drzewa, które dostał.",
    f9p_view(hide = "V", p = f9p_p_bananpol, values = c(M = "0.126", TOP = "0.00063")), read = "P(TOP) = 0.00063"),
  f9p_frame("Przegląd", "Ekspert spoza zespołu pyta o zawór instalacji tłumienia: po konserwacji bywa zostawiany zamknięty. Tego liścia w drzewie nie było, więc w rachunku miał prawdopodobieństwo zero.",
    f9p_view(ghost = "V", p = f9p_p_bananpol, values = c(M = "0.126", TOP = "0.00063")), read = "brakujący liść = prawdopodobieństwo zero"),
  f9p_frame("Dopisany", "Zamknięty zawór wystarcza, żeby tłumienie zawiodło, więc dołącza do bramki OR. Obie liczby rosną.",
    f9p_view(mark = "V", p = f9p17_p, values = c(M = f9p_num(f9p17_or), TOP = sprintf("%.6f", f9p17_top))),
    read = sprintf("0.005 · %s = %.6f (+%.0f%%)", f9p_num(f9p17_or), f9p17_top, 100 * (f9p17_top / f9p_top - 1))),
  f9p_frame("Nowa droga", "Lista minimalnych przekrojów zyskuje {I, V}. Niekompletne drzewo zawsze zaniża ryzyko.",
    f9p_view(lit = c("I", "V"), p = f9p17_p, values = c(M = f9p_num(f9p17_or), TOP = sprintf("%.6f", f9p17_top))),
    read = "przekroje: {I, D} · {I, S} · {I, V}")
))

# ============================================================================
# P19A, P19B — DOMKNIĘCIE: JEDNO DRZEWO, PIĘĆ PYTAŃ
# ============================================================================

f9p19_rows <- list(
  list(q = "Co?", name = "Zdarzenie szczytowe", res = "Nieopanowany pożar magazynu w ciągu roku", ref = "definicja 9.1"),
  list(q = "Jak?", name = "Bramki", res = "TOP = I ∩ (D ∪ S); 3 z 8 kombinacji zapalają szczyt", ref = "definicja 9.3"),
  list(q = "Ile?", name = "Rachunek", res = "0.126, potem 0.00063: około 6 na 10 000 magazyno-lat", ref = "wzór (9.4)"),
  list(q = "Którędy?", name = "Minimalne przekroje", res = "{I, D} i {I, S}; żadnego pojedynczego punktu awarii", ref = "wzór (9.6)"),
  list(q = "Co najpierw?", name = "Istotność", res = "inicjacja 50%, tłumienie 30%, detekcja 18% przy tej samej redukcji o połowę", ref = "wzór (9.10)")
)
f9p19_vals <- c(M = "0.126", TOP = "0.00063")

f9p_19a_ui <- f9p_tree_ui("P19A", "domknięcie", "Jedno drzewo, pięć pytań", nav = "seg", seg_label = "Pytanie", list(
  f9p_frame("Co?", "Najpierw zdanie: stan, granice, kryterium, horyzont. Bez niego drzewo nie ma szczytu.",
    f9p_view(hide = c("I", "M", "D", "S")), read = "zdarzenie szczytowe · definicja 9.1"),
  f9p_frame("Jak?", "Potem logika bez liczb: inicjacja oraz co najmniej jedna zawiedziona bariera. Trzy z ośmiu kombinacji zapalają szczyt.",
    f9p_view(lit = c("I", "D")), read = "TOP = I ∩ (D ∪ S) · definicja 9.3"),
  f9p_frame("Ile?", "Liczby wchodzą od dołu, bramka po bramce.",
    f9p_view(p = f9p_p_bananpol, values = f9p19_vals), read = "0.005 · 0.126 = 0.00063 · wzór (9.4)"),
  f9p_frame("Którędy?", "Dwie najkrótsze drogi do szczytu, obie przez inicjację.",
    f9p_view(title = "{I, D}", lit = c("I", "D"), p = f9p_p_bananpol, note = "0.00025"),
    f9p_view(title = "{I, S}", lit = c("I", "S"), p = f9p_p_bananpol, note = "0.0004"), read = "minimalne przekroje · wzór (9.6)"),
  f9p_frame("Co najpierw?", "Ta sama redukcja o połowę: inicjacja zabiera 50% ryzyka, tłumienie 30%, detekcja 18%. Ranking otwiera rozmowę o kosztach.",
    f9p_view(mark = "I", p = c(I = "−50%", S = "−30%", D = "−18%"), values = f9p19_vals), read = "istotność · wzór (9.10)")
))

f9p19_css <- tags$style(HTML("
  .f9p19-card { display: grid; grid-template-columns: max-content max-content 1fr max-content; gap: .5em 1.2em; align-items: baseline; }
  .f9p19-card > div { padding: .45em 0; border-bottom: 1px solid var(--upwr-rule); }
  .f9p19-q { font-weight: 700; color: var(--upwr-accent); }
  .f9p19-ref { font-family: var(--upwr-mono); font-size: .88em; color: var(--upwr-ink-subtle); }
  @media (max-width: 640px) { .f9p19-card { grid-template-columns: max-content 1fr; } .f9p19-ref { grid-column: 2; } }
"))
f9p_19b_ui <- f9p_panel("P19B", "domknięcie", "Jedno drzewo, pięć pytań", f9p19_css,
  tags$div(class = "f9p19-card", lapply(f9p19_rows, function(r) tagList(
    tags$div(class = "f9p19-q", r$q), tags$div(r$name), tags$div(r$res), tags$div(class = "f9p19-ref", r$ref)))),
  lc_caption("Kolejność jest częścią metody: logika przed liczbami, przekroje przed rankingiem. Wynik jest tak dobry, jak kompletność drzewa i dane w liściach."))
