# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa „sc”: sceny i piaskownice ------------
#
#   f9p_1_ui  — P1  scena (kadry SVG z R): protokół po pożarze czytany od końca
#   f9p_2_ui  — P2  pojęcie (kadry HTML z R): cztery składniki zdarzenia szczytowego
#   f9p_4a_ui — P4A scena „lancuch” (proto_sc.js): budzik przed egzaminem
#   f9p_4b_ui — P4B ta sama scena na instalacji gaśniczej Bananpolu
#   f9p_5_ui  — P5  piaskownica „konstruktor”: bramka, wejścia, klikane liście
#   f9p_6a_ui — P6A piaskownica „stany”: klikane drzewo i tabela ośmiu kombinacji
#   f9p_9_ui  — P9  piaskownica „parametry”: suwaki, liczby na drzewie, siatka 10 000
#   f9p_14_ui — P14 piaskownica: drzewo ze wspólną przyczyną + rycina lcSF f9_wspolna
# Wymaga: stop_proto_common.R (f9p_*), R/lc_stat_figures.R, modules/proto_sc.js.

# ============================================================================
# P1 — SCENA „PROTOKÓŁ PO POŻARZE”
# ============================================================================

f9p1_card <- function(x, y, w, title, sub, cls = "f9p-paper", dim = FALSE) {
  paste0(sprintf('<g%s>', if (dim) ' opacity=".3"' else ""),
    sprintf('<rect class="%s" x="%g" y="%g" width="%g" height="62" rx="6"/>', cls, x, y, w),
    sprintf('<text x="%g" y="%g" font-size="16" font-weight="700" text-anchor="middle">%s</text>', x + w / 2, y + 26, title),
    sprintf('<text class="f9p-subtle" x="%g" y="%g" font-size="12.5" text-anchor="middle">%s</text>', x + w / 2, y + 46, sub), "</g>")
}
f9p1_link <- function(d, label = NULL, lx = 0, ly = 0, dim = FALSE) paste0(
  sprintf('<path class="f9p-line" d="%s"%s/>', d, if (dim) ' opacity=".3"' else ""),
  if (!is.null(label)) sprintf('<text class="f9p-bad" x="%g" y="%g" font-size="13" font-weight="700" text-anchor="middle">%s</text>', lx, ly, label) else "")

f9p1_svg <- function(step) {
  second <- step == 4
  top <- f9p1_card(250, 14, 300, "Hala spłonęła", if (second) "protokół nr 2 · rok wcześniej, inny magazyn" else if (step == 5) "protokoły nr 1 i nr 2" else "protokół nr 1 · godz. 03:40", "f9p1-top")
  mid <- paste0(
    f9p1_link("M400,76 V108 M210,108 H590 M210,108 V140 M590,108 V140", if (step == 5) "potrzebne oba naraz", 482, 100),
    f9p1_card(90, 140, 240, "Był ogień", "zapłon przy ładowarce wózków"),
    f9p1_card(470, 140, 240, "Nikt go nie zatrzymał", "ogień rozwijał się bez reakcji"))
  a <- function(dim = FALSE) paste0(f9p1_link("M590,202 V234 M470,234 V266 M470,234 H590", dim = dim),
    f9p1_card(365, 266, 210, "Czujka dymu milczała", "tryskacze nie dostały sygnału", dim = dim))
  b <- paste0(f9p1_link("M590,202 V234 M710,234 V266 M590,234 H710"),
    f9p1_card(605, 266, 190, "Zawór był zamknięty", "po przeglądzie instalacji"))
  aria <- c("Protokół po pożarze: hala spłonęła.",
    "Hala spłonęła, bo był ogień i nikt go nie zatrzymał.",
    "Nikt nie zatrzymał ognia, bo czujka dymu milczała.",
    "Drugi protokół: ten sam skutek, ale zawiódł zamknięty zawór.",
    "Dwa protokoły razem: zapłon w obu, czujka albo zawór.")[step]
  f9p_svg(800, 340, aria, top, if (step >= 2) mid,
    if (step == 3) a(), if (step == 4) paste0(a(dim = TRUE), b),
    if (step == 5) paste0(a(), b, '<text class="f9p-bad" x="664" y="226" font-size="13" font-weight="700" text-anchor="middle">wystarczyło jedno</text>'))
}

f9p1_frames <- list(
  list(name = "Skutek", text = "Protokół po pożarze zaczyna się od końca: hala spłonęła. Pytanie nie brzmi „co się zapaliło”, tylko „co musiało się stać, żeby do tego doszło”."),
  list(name = "Krok wstecz", text = "Był ogień i nikt go nie zatrzymał. Sam zapłon to jeszcze nie spalona hala: potrzebne było jedno i drugie."),
  list(name = "Jeszcze krok", text = "Dlaczego nikt nie zatrzymał ognia? Czujka dymu nie wysłała sygnału, więc tryskacze nie ruszyły."),
  list(name = "Inna noc", text = "Drugi protokół, rok wcześniej. Czujka zadziałała, ale zawór instalacji został zamknięty po przeglądzie. Skutek ten sam."),
  list(name = "Dwa protokoły", text = "Zapłon był w obu protokołach. Bariera za każdym razem zawiodła inna, i jedna wystarczyła.",
    read = "hala spłonęła ← zapłon oraz (czujka albo zawór)")
)
f9p1_css <- tags$style(HTML("
  .f9p1-top { fill: var(--upwr-accent-tint); stroke: var(--upwr-accent); stroke-width: 2; }
"))
f9p_1_ui <- tagList(f9p1_css,
  f9p_frames_ui("P1", "scena", "Protokół po pożarze", f9p1_frames, f9p1_svg))

# ============================================================================
# P2 — CZTERY SKŁADNIKI ZDARZENIA SZCZYTOWEGO (przykład 9.1)
# ============================================================================

f9p2_parts <- list(
  list(key = "stan", label = "Stan", text = "Temperatura przekracza próg"),
  list(key = "granice", label = "Granice systemu", text = "w komorze dojrzewalni nr 2"),
  list(key = "kryterium", label = "Kryterium", text = "18 °C przez ponad 2 godziny"),
  list(key = "horyzont", label = "Horyzont", text = "co najmniej raz w roku eksploatacji")
)
# Kadr k: zdanie złożone z k pierwszych składników; pozostałe to puste pola.
f9p2_stage <- function(k) {
  as.character(tags$div(class = "f9p2",
    tags$p(class = "f9p2-old", if (k == 0) "„Awaria chłodni”" else tags$s("„Awaria chłodni”")),
    tags$div(class = "f9p2-slots", lapply(seq_along(f9p2_parts), function(i) {
      part <- f9p2_parts[[i]]
      tags$div(class = paste0("f9p2-slot is-", part$key, if (i > k) " is-empty"),
        tags$span(class = "f9p2-label", part$label),
        tags$span(class = "f9p2-text", if (i <= k) part$text else "?"))
    }))))
}
f9p2_frames <- list(
  list(name = "Propozycja", text = "Kierownik logistyki proponuje zdarzenie szczytowe: „Awaria chłodni”. Dwie osoby nie rozstrzygną zgodnie, czy dane zdarzenie się w nim mieści."),
  list(name = "Stan", text = "Co się stało? Nie „awaria”, tylko skutek dla towaru: temperatura przekracza dopuszczalny próg."),
  list(name = "Granice", text = "Gdzie? Jedna komora wraz z zasilaniem i sterowaniem, a nie „chłodnia”."),
  list(name = "Kryterium", text = "Po czym poznamy, że stan zaszedł? Próg i czas, odczytane z rejestratora komory."),
  list(name = "Horyzont", text = "Kiedy? Jeden rok eksploatacji w trybie normalnej pracy. Dopiero teraz można zapytać o prawdopodobieństwo.",
    read = "stan · granice · kryterium · horyzont")
)
f9p2_css <- tags$style(HTML("
  .f9p2 { margin: .4em 0 .6em; }
  .f9p2-old { font-size: 1.25em; font-weight: 600; margin: 0 0 .6em; color: var(--upwr-ink-soft); }
  .f9p2-slots { display: grid; grid-template-columns: repeat(auto-fit, minmax(11em, 1fr)); gap: .6em; }
  .f9p2-slot { border: 1.5px solid var(--f9p2-c); border-top-width: 5px; border-radius: 5px; padding: .5em .7em; background: var(--upwr-surface); min-height: 5.2em; }
  .f9p2-slot.is-empty { border-style: dashed; border-color: var(--upwr-rule); border-top-width: 1.5px; background: transparent; }
  .f9p2-label { display: block; font-family: var(--upwr-mono); font-size: .8em; text-transform: uppercase; letter-spacing: .05em; color: var(--f9p2-c); }
  .f9p2-slot.is-empty .f9p2-label, .f9p2-slot.is-empty .f9p2-text { color: var(--upwr-ink-subtle); }
  .f9p2-text { display: block; font-size: 1.08em; font-weight: 600; margin-top: .25em; }
  .f9p2-slot.is-stan { --f9p2-c: var(--upwr-accent); }
  .f9p2-slot.is-granice { --f9p2-c: var(--upwr-cat-niebo); }
  .f9p2-slot.is-kryterium { --f9p2-c: var(--upwr-cat-bursztyn); }
  .f9p2-slot.is-horyzont { --f9p2-c: var(--upwr-cat-szalwia); }
"))
f9p_2_ui <- tagList(f9p2_css,
  f9p_frames_ui("P2", "pojęcie", "Od „awarii chłodni” do zdarzenia szczytowego", f9p2_frames, function(i) f9p2_stage(i - 1)))

# ============================================================================
# P4A, P4B — SCENA „ŁAŃCUCH A ZAPAS”
# ============================================================================

f9p4_ui <- function(num, title, worlds, hint) {
  f9p_scene_ui(num, "scena", title, "lancuch", list(worlds = worlds),
    f9p_out("stage"), lc_caption(hint))
}
f9p_4a_ui <- f9p4_ui("P4A", "Jutro egzamin o ósmej", hint = "Kliknij element, żeby go zepsuć albo naprawić.", worlds = list(
  list(title = "Jeden budzik w telefonie", type = "chain", start = "wieczór", end_ok = "dzwoni 6:30", end_bad = "cisza",
    parts = list(list(name = "Bateria", ok = "naładowana", bad = "rozładowana"), list(name = "Dźwięk", ok = "włączony", bad = "wyciszony")),
    ok = "Budzik dzwoni. Wstajesz na egzamin.", bad = "Budzik milczy. Zaspałeś.", count = "Zaspałeś"),
  list(title = "Telefon i budzik na szafce", type = "par", start = "wieczór", end_ok = "dzwoni 6:30", end_bad = "cisza",
    parts = list(list(name = "Telefon", ok = "zadzwoni", bad = "milczy"), list(name = "Budzik", ok = "zadzwoni", bad = "stanął")),
    ok = "Coś dzwoni. Wstajesz na egzamin.", bad = "Oba milczą. Zaspałeś.", count = "Zaspałeś")
))
f9p_4b_ui <- f9p4_ui("P4B", "Ta sama para urządzeń, dwie instalacje", hint = "Kliknij element, żeby go zepsuć albo naprawić.", worlds = list(
  list(title = "Łańcuch: czujka uruchamia tłumienie", type = "chain", start = "zapłon", end_ok = "ugaszony", end_bad = "pożar",
    parts = list(list(name = "Detekcja", ok = "wykrywa", bad = "milczy"), list(name = "Tłumienie", ok = "gasi", bad = "nie rusza")),
    ok = "Sygnał dociera, instalacja gasi ogień.", bad = "Łańcuch przerwany. Ogień wymyka się spod kontroli.", count = "Pożar"),
  list(title = "Dwie samodzielne instalacje", type = "par", start = "zapłon", end_ok = "ugaszony", end_bad = "pożar",
    parts = list(list(name = "Instalacja A", ok = "gasi", bad = "zawodzi"), list(name = "Instalacja B", ok = "gasi", bad = "zawodzi")),
    ok = "Co najmniej jedna instalacja gasi ogień.", bad = "Zawiodły obie. Ogień wymyka się spod kontroli.", count = "Pożar")
))

# ============================================================================
# P5 — KONSTRUKTOR BRAMKI
# ============================================================================

f9p_5_ui <- f9p_scene_ui("P5", "piaskownica", "Utrata kontroli nad zapłonem", "konstruktor",
  list(label = c("Utrata kontroli", "nad zapłonem"),
    causes = list(list(id = "D", label = c("Brak", "detekcji")), list(id = "S", label = c("Brak", "tłumienia")),
      list(id = "Z", label = c("Utrata", "zasilania"))),
    chosen = c("D", "S"), gate = "or"),
  lc_toolbar(
    f9p_seg("gate", "Logika", c("Wystarczy jedna przyczyna — OR" = "or", "Potrzebna kombinacja — AND" = "and")),
    lc_group("Przyczyny", tags$div(class = "lc-seg f9p-seg", role = "group", `aria-label` = "Przyczyny",
      lapply(list(c("D", "Brak detekcji"), c("S", "Brak tłumienia"), c("Z", "Utrata zasilania")), function(ch)
        tags$button(type = "button", `data-f9p` = "cause", `data-value` = ch[1],
          `aria-pressed` = if (ch[1] %in% c("D", "S")) "true" else "false", ch[2])))),
    f9p_button("clear", "Zgaś wszystko")),
  f9p_out("stage"), f9p_out("body"),
  lc_caption("Kliknij koło, żeby zdarzenie zaszło. Konstruktor nie ocenia, która bramka jest właściwa; pokazuje, co wybrana bramka znaczy."))

# ============================================================================
# P6A — DRZEWO BANANPOLU I TABELA STANÓW
# ============================================================================

f9p_6a_ui <- f9p_scene_ui("P6A", "piaskownica", "Zapal liście, sprawdź szczyt", "stany",
  list(tree = f9p_tree_bananpol),
  f9p_out("stage"), f9p_out("body"),
  tags$div(class = "f9p-nav", f9p_button("clear", "Zgaś wszystko"), f9p_button("reset", "Wyczyść tabelę")),
  lc_caption("Kliknij koło, żeby zdarzenie bazowe zaszło. Każda sprawdzona kombinacja zostaje w tabeli."))

# ============================================================================
# P9 — PARAMETRY MAŁEGO DRZEWA
# ============================================================================

f9p_9_ui <- f9p_scene_ui("P9", "piaskownica", "Parametry małego drzewa", "parametry",
  list(tree = f9p_tree_bananpol, base = list(i = f9p_pi, d = f9p_d, s = f9p_s)),
  lc_toolbar(
    f9p_slider("f9p9_i", "P(inicjacji)", 0, .03, f9p_pi, .001),
    f9p_slider("f9p9_d", "P(braku detekcji | I)", 0, .3, f9p_d, .01),
    f9p_slider("f9p9_s", "P(niepowodzenia tłumienia | I)", 0, .3, f9p_s, .01),
    f9p_button("base", "Stan bazowy")),
  f9p_out("stage"), f9p_out("body"),
  lc_caption("Siatka ma zawsze 10 000 kratek: jedna kratka to jeden magazyno-rok. Kratki z inicjacją są w kolorze kurkumy, z nieopanowanym pożarem w kolorze akcentu."))

# ============================================================================
# P14 — WSPÓLNA PRZYCZYNA (przykład 9.7)
# ============================================================================

f9p14_top <- function(...) f9p_gate("TOP", c("Nieopanowany pożar", "magazynu w roku"), "and", f9p_leaf("I", "Inicjacja"), ...)
f9p14_c <- f9p_leaf("C", c("Wspólna", "przyczyna"))
f9p14_d0 <- f9p_leaf("D0", c("Detekcja", "lokalnie"), sym = "D₀")
f9p14_s0 <- f9p_leaf("S0", c("Tłumienie", "lokalnie"), sym = "S₀")
f9p14_trees <- list(
  chain = f9p14_top(f9p_gate("M", c("Zabezpieczenia", "zawiodły"), "or", f9p14_c, f9p14_d0, f9p14_s0)),
  par = f9p14_top(f9p_gate("M", c("Zabezpieczenia", "zawiodły"), "or", f9p14_c,
    f9p_gate("G", c("Obie bariery", "lokalnie"), "and", f9p14_d0, f9p14_s0))))
f9p14_p <- c(I = "0.005", C = "q", D0 = "0.05", S0 = "0.08")
# Liczby przykładu 9.7 przy q = 0.01.
f9p14_q <- 0.01
f9p14_chain <- f9p_pi * (f9p14_q + (1 - f9p14_q) * f9p_or)                  # 0.000674
f9p14_par <- f9p_pi * (f9p14_q + (1 - f9p14_q) * risk_gate_and(c(f9p_d, f9p_s)))  # 0.0000698

f9p_14_ui <- f9p_tree_ui("P14", "piaskownica", "Wspólna przyczyna jako most", nav = "seg", seg_label = "Architektura",
  trees = f9p14_trees, list(
    f9p_frame("Łańcuch", "Wspólna przyczyna C dostaje własny liść. Zapalona droga {I, C} nie przechodzi przez żadne urządzenie: nie usunie jej lepsza czujka ani lepszy zawór.",
      f9p_view(tree = "chain", lit = c("I", "C"), p = f9p14_p),
      read = sprintf("przekroje: {I, C} · {I, D₀} · {I, S₀} · przy q = 0.01: %.6f zamiast 0.00063 (+%.0f%%)", f9p14_chain, 100 * (f9p14_chain / f9p_top - 1))),
    f9p_frame("Dwie samodzielne bariery", "Przy redundancji droga przez urządzenia wymaga trzech liści, a droga przez C nadal dwóch. Wspólna przyczyna skraca najkrótszą drogę do szczytu.",
      f9p_view(tree = "par", lit = c("I", "C"), p = f9p14_p),
      read = sprintf("przekroje: {I, C} · {I, D₀, S₀} · przy q = 0.01: %.7f zamiast 0.00002 (×%.2f)", f9p14_par, f9p14_par / (f9p_pi * f9p_d * f9p_s)))),
  after = lc_sf_figure("f9_wspolna",
    control = list(lc_sf_slider("f9p14_q", "q = P(C | I)", 0, .2, f9p14_q, .005)),
    toolbar = lc_readouts(f9p_read("chain", "Łańcuch z C", upwr_secondary), f9p_read("par", "Dwie bariery z C", unname(upwr_cat["niebo"])),
      f9p_read("floor", "Sam przekrój {I, C}", upwr_accent)),
    ratio = "2.4/1", max_height = "320px", legend = TRUE,
    config = list(pi = f9p_pi, d = f9p_d, s = f9p_s),
    after = lc_caption("Własne, stałe parametry: P(I) = 0.005, d₀ = 0.05, s₀ = 0.08. Przerywane linie to wyniki bez wspólnej przyczyny. Żadna poprawa urządzeń nie sprowadzi wyniku poniżej linii samego przekroju {I, C}.")))

# Style scen tej grupy (dołączane raz, przy pierwszym panelu grupy).
f9p_sc_css <- tags$style(HTML("
  .f9p4-part { cursor: pointer; }
  .f9p4-part rect { fill: var(--upwr-sage-tint); stroke: var(--upwr-sage); stroke-width: 2; }
  .f9p4-part.is-bad rect { fill: var(--upwr-accent-tint); stroke: var(--upwr-accent); stroke-dasharray: 6 4; }
  .f9p4-part:hover rect { stroke-width: 3.2; }
  .f9p4-part .f9p4-state { fill: var(--upwr-sage) !important; font-weight: 600; }
  .f9p4-part.is-bad .f9p4-state { fill: var(--upwr-accent) !important; }
  .f9p4-wire { stroke: var(--upwr-ink-subtle); stroke-width: 2; stroke-dasharray: 4 5; fill: none; opacity: .6; }
  .f9p4-wire.is-live { stroke: var(--upwr-sage); stroke-width: 3.4; stroke-dasharray: none; opacity: 1; }
  .f9p4-start { fill: var(--upwr-sage); }
  .f9p4-end { fill: var(--upwr-sage); }
  .f9p4-end.is-bad { fill: var(--upwr-accent); }
  .f9p4-verdict { font-weight: 600; color: var(--upwr-sage); margin: .1em 0 .5em; min-height: 2.6em; }
  .f9p4-verdict.is-bad { color: var(--upwr-accent); }
  .f9p4-chips { display: flex; flex-wrap: wrap; gap: .35em; }
  .f9p4-chip { font-family: var(--upwr-mono); font-size: .82em; padding: .15em .55em; border-radius: 3px;
    border: 1.5px solid transparent; background: var(--upwr-sage-tint); color: var(--upwr-ink); }
  .f9p4-chip.is-bad { background: var(--upwr-accent-tint); }
  .f9p4-chip.is-now { border-color: var(--upwr-ink); font-weight: 700; }
  .f9p-cell.f9p6-side { flex: 0 1 13em; max-width: 14em; }
  .f9p6-tbl { width: 100%; }
  .f9p6-tbl td, .f9p6-tbl th { text-align: center; padding: .25em .6em; }
  .f9p6-tbl tr.is-hit td { background: var(--upwr-accent-tint); }
  .f9p6-tbl tr.is-hit .f9p6-top { color: var(--upwr-accent); font-weight: 700; }
  .f9p6-tbl tr.is-now td { box-shadow: inset 0 2px 0 var(--upwr-ink), inset 0 -2px 0 var(--upwr-ink); }
  .f9p-grid-bg { fill: var(--upwr-surface-sunken); stroke: var(--upwr-rule); stroke-width: 1; }
  .f9p-grid-line { stroke: var(--upwr-rule); stroke-width: .6; fill: none; }
  .f9p-grid-init { fill: var(--upwr-cat-kurkuma); }
  .f9p-grid-top { fill: var(--upwr-accent); }
"))
f9p_4a_ui <- tagList(f9p_sc_css, f9p_4a_ui)
