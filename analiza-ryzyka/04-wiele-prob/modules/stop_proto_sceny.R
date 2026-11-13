# SCENY WYKŁADU 04 — dane i teksty; rysunek w przeglądarce ----------------
#
# Sceny rysuje modules/sceny.js (SVG w JS, bez serwera R), więc działają też
# w wersji offline bez zapisanych stanów. Tu zostają dane, teksty kadrów i CSS.
#   p4_proto_bananas_ui   — Scena „Pająk w kiści” (kadry historii) (1a): wynik 0/1 wprost
#   p4_proto_threshold_ui — Scena „Kontroler i próg” (kadry historii) (1b): 0/1 z liczby i normy
#   p4_proto_pallet_ui    — Scena „Partia na palecie” (historia z powtórzeniem) (2)
#   p4_proto_drivers_ui   — Scena „Hala, 100 kierowców” (historia z powtórzeniem) (9)
#   p4_proto_receipt_ui   — Scena „Odbiór dostawy” (historia z powtórzeniem) (11)
# Scena „Tabliczka magazyniera” jest w stop_proto_tabliczka.R.

p4_scene_css <- tags$style(HTML("
  .p4s-head { display: flex; justify-content: space-between; align-items: baseline; gap: 1em; margin-bottom: .3em; }
  .p4s-name { font-weight: 700; font-size: 1.05em; }
  .p4s-step { font-family: var(--upwr-mono); font-size: .9em; color: var(--upwr-ink-subtle); }
  .p4s-caption { min-height: 3em; margin: .4em 0; }
  .p4s-read { font-family: var(--upwr-mono); font-weight: 600; margin: .2em 0 .6em; }
  .p4s-nav { display: flex; gap: .6em; flex-wrap: wrap; }
  .p4s-svg { display: block; width: 100%; height: auto; margin: 0 auto; }
  /* kolor domyślny tylko dla napisów bez własnego fill (atrybutu albo stylu) */
  .p4s-svg text { font-family: var(--upwr-sans); }
  .p4s-svg text:not([fill]):not([style*='fill']) { fill: var(--upwr-ink); }
  .p4s-mono { font-family: var(--upwr-mono) !important; }
  .p4s-subtle { fill: var(--upwr-ink-subtle) !important; }
  .p4s-ok { fill: color-mix(in srgb, var(--upwr-sage) 30%, var(--upwr-surface)); stroke: var(--upwr-sage); stroke-width: 1.2; }
  .p4s-bad { fill: var(--upwr-accent); stroke: var(--upwr-accent); }
  .p4s-idle { fill: var(--upwr-surface-sunken); stroke: var(--upwr-rule); stroke-width: 1.2; }
  .p4s-band { fill: var(--upwr-sage); fill-opacity: .15; }
  .p4s-band-edge { stroke: var(--upwr-sage); stroke-width: 1.4; }
  .p4s-band-late { fill: var(--upwr-single-alt); fill-opacity: .18; }
  .p4s-band-late-edge { stroke: var(--upwr-single-alt); stroke-width: 1.4; stroke-dasharray: 5 4; }
  .p4s-axis { stroke: var(--upwr-ink-subtle); stroke-width: 1.2; }
  .p4s-stamp { font-weight: 800 !important; font-size: 26px; letter-spacing: .08em; }
  .p4s-stamp-ok { fill: var(--upwr-sage) !important; }
  .p4s-stamp-bad { fill: var(--upwr-accent) !important; }
  .p4s-stamp-box-ok { fill: none; stroke: var(--upwr-sage); stroke-width: 3; }
  .p4s-stamp-box-bad { fill: none; stroke: var(--upwr-accent); stroke-width: 3; }
  .p4s-phobia-slot { margin-left: auto; }
  .p4s-layer { opacity: 0; animation: p4s-show 1ms linear both; }
  @keyframes p4s-show { to { opacity: 1; } }
  .p4s-fly { animation-name: p4s-fly; animation-timing-function: cubic-bezier(.3, .1, .3, 1); animation-fill-mode: both; }
  .p4s-fly.is-out { animation-name: p4s-fly-out; }
  @keyframes p4s-fly { 0% { transform: translate(var(--dx), var(--dy)); opacity: 0; }
    15% { transform: translate(var(--dx), var(--dy)); opacity: 1; } 100% { transform: none; opacity: .85; } }
  @keyframes p4s-fly-out { 0% { transform: translate(var(--dx), var(--dy)); opacity: 0; }
    15% { transform: translate(var(--dx), var(--dy)); opacity: 1; } 90% { transform: none; opacity: 1; } 100% { transform: none; opacity: 0; } }
  @media (prefers-reduced-motion: reduce) {
    .p4s-layer, .p4s-fly { animation: none !important; opacity: 1; }
    .p4s-fly.is-out { opacity: 0; }
  }
  .p4s-calc { margin-top: .9em; padding: .7em 1em; border-top: 1px solid var(--upwr-rule); font-size: .95em; }
  .p4s-calc-row { display: flex; gap: .8em; flex-wrap: wrap; padding: .12em 0; }
  .p4s-calc-l { min-width: 15em; color: var(--upwr-ink-soft); }
  .p4s-calc-v { font-family: var(--upwr-mono); }
  .p4s-tally { border-collapse: collapse; margin: .4em 0; font-family: var(--upwr-mono); }
  .p4s-tally th, .p4s-tally td { border: 1px solid var(--upwr-rule); padding: 4px 12px; text-align: center; }
  .p4s-tally th { font-family: var(--upwr-sans); font-weight: 600; }
  .p4s-tally .is-err { color: var(--upwr-accent); font-weight: 700; }
"))

# Przycisk sceny: wygląd lc_action(), ale bez wejścia Shiny — klik obsługuje
# sceny.js po atrybucie data-p4s.
p4s_button <- function(act, label = NULL, icon = NULL, variant = "outline", aria_label = NULL) {
  b <- lc_action(paste0("p4s-", act), label, icon = icon, variant = variant, aria_label = aria_label)
  b$attribs$id <- NULL
  b$attribs$class <- sub("action-button ", "", b$attribs$class, fixed = TRUE)
  b$attribs[["data-p4s"]] <- act
  b
}

p4s_prev_next <- function() tagList(p4s_button("prev", "Wstecz", icon = "prev"),
  p4s_button("next", "Dalej", icon = "next", variant = "solid"))

p4_scene_shell <- function(scene, title, buttons, config, after = NULL, extra = NULL) {
  figure_panel(label = "Scena", title = title, width_mode = "wide",
    p4_scene_css, extra,
    tags$div(class = "p4s-scene", `data-scene` = scene,
      `data-config` = jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA),
      tags$div(`data-p4s-out` = "head"),
      tags$div(`data-p4s-out` = "stage"),
      tags$div(`data-p4s-out` = "caption"),
      tags$div(class = "p4s-nav", buttons),
      after
    )
  )
}

# ============================================================================
# (1a) PAJĄK W KIŚCI — kadry historii; doświadczenie samo daje wynik 0/1
# ============================================================================

# Dwanaście kiści, pająki w kiściach 4 i 9 (stałe, żeby narracja się zgadzała).
p4_banana_spider <- seq_len(12) %in% c(4, 9)

p4_banana_frames <- list(
  list(name = "Dostawa",
    text = "Do dojrzewalni Bananpolu przyjechała skrzynia bananów z Ekwadoru. Z zewnątrz każda kiść wygląda tak samo."),
  list(name = "Otwieramy",
    text = "Kontroler rozchyla kiść po kiści. Albo jest czysta, albo siedzi w niej pająk. Trzeciej możliwości nie ma."),
  list(name = "Zero albo jeden",
    text = "Wynik każdej kontroli to tak albo nie. Nikt nie musi niczego mierzyć ani ustalać granicy: doświadczenie samo odpowiada zerem albo jedynką.")
)

# Żart z fobiami: phobia = 1 pająk, 2 klaun (po „arachnofobii”), 3 znak ostrzegawczy
# (po „koulrofobii”). Zmienia się tylko rysunek lokatora kiści, wynik 0/1 zostaje.
p4_banana_phobia <- list(
  list(button = "arachnofobia", who = "z pająkiem"),
  list(button = "koulrofobia", who = "z klaunem"),
  list(button = NULL, who = "z zagrożeniem")
)

p4_proto_bananas_ui <- p4_scene_shell("banany", "Pająk w kiści",
  tagList(p4s_prev_next(), tags$div(class = "p4s-phobia-slot", `data-p4s-out` = "phobia")),
  list(frames = p4_banana_frames, phobia = p4_banana_phobia, spider = p4_banana_spider))

# ============================================================================
# (1b) KONTROLER I PRÓG — kadry historii
# ============================================================================

p4_valve_pressure <- c(3.05, 2.93, 3.12, 2.75, 3.01, 2.88, 3.24, 2.97, 3.08, 2.95)
p4_tol <- c(2.8, 3.2)
p4_tol_late <- c(2.7, 3.3)

p4_threshold_frames <- list(
  list(name = "Pomiary",
    text = "Z zaworami do chłodni jest trudniej niż z bananami. Kontroler mierzy ciśnienie otwarcia dziesięciu zaworów i dostaje liczby w barach, a nie odpowiedź tak albo nie."),
  list(name = "Kryterium na piśmie",
    text = "Zanim zmierzył pierwszy zawór, w protokole stała tolerancja producenta: od 2.8 do 3.2 bara."),
  list(name = "Zero albo jeden",
    text = "Każdy pomiar staje się jednym z dwóch wyników: w tolerancji albo poza nią. Z dziesięciu liczb zostaje dziesięć zer i jedynek."),
  list(name = "Granica po fakcie",
    text = "Kierownik dostawy proponuje „nie przesadzać” i poszerzyć tolerancję do 2.7–3.3 bara. Wady znikają z protokołu, ale zawory są te same.")
)

p4_proto_threshold_ui <- p4_scene_shell("prog", "Kontroler i próg", p4s_prev_next(),
  list(frames = p4_threshold_frames, pressure = p4_valve_pressure, tol = p4_tol, tolLate = p4_tol_late))

# ============================================================================
# (2) PARTIA NA PALECIE — historia z powtórzeniem
# ============================================================================

p4_proto_pallet_ui <- p4_scene_shell("paleta", "Partia na palecie",
  tagList(p4s_button("one", "Skontroluj partię", variant = "solid"),
    p4s_button("ten", "+10 partii"), p4s_button("hund", "+100 partii"),
    p4s_button("reset", icon = "reset", variant = "ghost", aria_label = "Nowa paleta od zera")),
  list(title = "Kontrola przyjęcia", size = 100L, p = 0.02, captions = c(
    "Na rampie stoi paleta stu zaworów. Kontroler sprawdza każdy i odkłada niesprawne na bok.",
    "Krzyżyki za każdym razem leżą gdzie indziej: czasem parami, czasem wcale.",
    "Pojedyncza paleta nie daje się przewidzieć, ale stos partii za każdym razem rośnie w ten sam kształt.")))

# ============================================================================
# (9) HALA, 100 KIEROWCÓW — historia z powtórzeniem
# ============================================================================

# Ziarno 31: po 10 latach 22 kierowców z kolizją, zgodnie z modelem (22.1).
# Pod sceną rachunek ryzyka skumulowanego jednego kierowcy, wzór (4.6).
p4_drivers_first <- local({ set.seed(31); rgeom(100, 1e-4) + 1 })

p4_proto_drivers_ui <- p4_scene_shell("kierowcy", "Hala, 100 kierowców",
  tagList(p4s_button("next", "Kolejny rok", variant = "solid"),
    p4s_button("reset", icon = "reset", variant = "ghost", aria_label = "Od początku")),
  list(first = p4_drivers_first, captions = c(
    "Sto kierowców wózków w hali Bananpolu. Każdy raz dziennie przejeżdża przez skrzyżowanie, na którym kolizja zdarza się raz na 10 000 przejazdów.",
    "Każdy przejazd jest prawie bezpieczny. Kolejny rok — kolejne 250 przejazdów na kierowcę.",
    "Po dziesięciu latach kolizję ma za sobą {k} kierowców na 100. Rzadko na przejazd nie znaczy rzadko w karierze.")),
  after = tags$div(`data-p4s-out` = "calc"))

# ============================================================================
# (11) ODBIÓR DOSTAWY — historia z powtórzeniem
# ============================================================================

p4_receipt_n <- 50L; p4_receipt_c <- 1L
p4_receipt_quality <- c(good = 0.02, weak = 0.08)
p4_receipt_share_good <- 0.7

p4_proto_receipt_ui <- p4_scene_shell("odbior", "Odbiór dostawy: plan (50, 1)",
  tagList(p4s_button("one", "Kolejna dostawa", variant = "solid"),
    p4s_button("ten", "+20 dostaw"), p4s_button("hund", "+100 dostaw"),
    p4s_button("reset", icon = "reset", variant = "ghost", aria_label = "Wyzeruj dostawy")),
  list(title = "Magazyn Bananpolu", n = p4_receipt_n, c = p4_receipt_c, quality = as.list(p4_receipt_quality),
    shareGood = p4_receipt_share_good, captions = c(
      "Magazynier losuje 50 zaworów z każdej dostawy i przyjmuje ją, gdy znajdzie najwyżej jeden niesprawny. Nie wie, który dostawca miał dobry dzień.",
      "Burgund: decyzje błędne. Magazynier widzi tylko próbkę, nie wie, w którym wierszu stoi dostawa.")))
