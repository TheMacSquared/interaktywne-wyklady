# PUNKTY STOP (2026-11-03) — oddziały i taśma przecięta na n, wykład 05 ----------
#
# Obie sceny rysuje i losuje modules/sceny.js (sceny „oddzialy” i „ciecie”);
# tu zostają ziarna, dane startowe, teksty i CSS.
#   d5p_branches_ui — Scena „Dwadzieścia oddziałów” (historia z powtórzeniem) (P8)
#   d5p_cut_ui      — Pojęcie „Taśma przecięta na n” (wzór 5.7) (P9)
# Wymaga: stop_proto_common.R (d5p_scene_ui, d5p_out, d5p_button, d5p_series)
#   oraz lc_sf_slider z R/lc_stat_figures.R.

d5p_br_budget <- 30L      # budżet kontroli na oddział (średnia r / p = 3 / 0.1)
d5p_br_rows <- 20L        # liczba oddziałów
d5p_br_seed <- 2L         # ziarno pierwszych 20 audytów: dokładnie 8 przekroczeń
d5p_br_model_over <- 1 - pnbinom(27, 3, 0.1)  # P(X > 30) = 0.411 (wzór 5.7)

# Pierwsze 20 audytów z ustalonym ziarnem (taśmy do trzeciej wady).
d5p_br_seed_tapes <- local({
  set.seed(d5p_br_seed)
  lapply(seq_len(d5p_br_rows), function(i) d5p_series(3, 0.1))
})

# Startowa taśma P9 z ziarnem 43: trzecia wada w 24. kontroli.
d5p_cut_seed <- 43L
d5p_cut_len <- 80L
d5p_cut_start <- local({
  set.seed(d5p_cut_seed)
  runif(d5p_cut_len) < 0.1
})

# ============================================================================
# (P8) DWADZIEŚCIE ODDZIAŁÓW — taśmy do 20 audytów, potem histogram końców X
# ============================================================================

# Kafelki oddziałów (do 20 audytów), potem histogram końców X na osi 0–120:
# modules/sceny.js (branchesSvg). Pierwsze 20 audytów to taśmy z ziarnem 2;
# „Sto sezonów” dolosowuje 2000 audytów w przeglądarce.
d5p_branches_ui <- d5p_scene_ui("Scena", "Dwadzieścia oddziałów", "oddzialy",
  list(seedX = vapply(d5p_br_seed_tapes, length, integer(1)), model = sprintf("%.2f", d5p_br_model_over),
    captions = c(
      "Dyrektor jakości rozdaje każdemu z 20 oddziałów budżet po 30 kontroli. Wada co dziesiątą paletę, więc trzy wady to średnio 30 kontroli.",
      "Jedne oddziały kończą szybko, inne wychodzą daleko poza budżet.",
      "Budżet „średni” zawiódł w {k} z 20 oddziałów.",
      "Kolejne audyty dokładają słupki; odsetek przekroczeń jeszcze się ustala.",
      "Po sezonach ok. 4 audyty na 10 przekraczają budżet równy średniej.")),
  d5p_out("head"), d5p_out("stage"), d5p_out("read"), d5p_out("caption"),
  tags$div(class = "d5p-nav",
    d5p_button("one", "Audyt w kolejnym oddziale", variant = "solid"),
    d5p_button("all", "Wszystkie oddziały"),
    d5p_button("sez", "Sto sezonów"),
    d5p_button("reset", icon = "reset", variant = "ghost", aria_label = "Od nowa")))

# ============================================================================
# (P9) TAŚMA PRZECIĘTA NA n — wzór (5.7): P(X ≤ n) = P(Y_n ≥ r)
# ============================================================================

# Kolory jak we wzorze (5.7): X i reguła „do trzeciej wady” w kolorze akcentu,
# Y_n i reguła „n prób” w kolorze nieba (modules/sceny.js: cutSvg, cutRead).
d5p_cut_css <- tags$style(HTML("
  .d5p-cut-two { display: grid; grid-template-columns: 1fr 1fr; gap: .9em 1.6em; margin: .4em 0 .4em; }
  .d5p-cut-eq { font-size: 1.15em; margin: .2em 0 .2em; }
  @media (max-width: 640px) { .d5p-cut-two { grid-template-columns: 1fr; } }
  .d5p-cut-col { padding-left: .8em; border-left: 3px solid var(--c); }
  .d5p-cut-rule { font-family: var(--upwr-mono); font-size: .8em; color: var(--upwr-ink-subtle); }
  .d5p-cut-val { font-family: var(--upwr-mono); font-weight: 700; font-size: 1.2em; color: var(--c); margin: .1em 0 .3em; }
  .d5p-cut-verdict { font-weight: 600; margin: .2em 0 .6em; }
"))

# Taśma 80 kontroli (rysowana także za trzecią wadą), cięcie za n-tą kratką;
# nad taśmą klamra X, pod taśmą klamra Y_n. Rysunek i odczyt: modules/sceny.js.
d5p_cut_ui <- d5p_scene_ui("Pojęcie", "Taśma przecięta na n", "ciecie",
  list(start = d5p_cut_start, len = d5p_cut_len),
  d5p_cut_css,
  d5p_out("head"), d5p_out("stage"),
  lc_sf_slider("d5p_cut_n", "Cięcie po kontroli n", 5, 80, 30, 1),
  d5p_out("read"),
  tags$p(class = "d5p-caption",
    "Dwa opisy tego samego zdarzenia: trzecia wada pojawia się do n-tej kontroli włącznie, czyli w pierwszych n kontrolach są co najmniej trzy wady. Lewy odczyt liczy kontrole do trzeciej wady, prawy liczy wady w pierwszych n kontrolach; dlatego oba prawdopodobieństwa są równe."),
  tags$div(class = "d5p-nav",
    d5p_button("shuffle", icon = "shuffle", variant = "ghost", aria_label = "Inna taśma")))
