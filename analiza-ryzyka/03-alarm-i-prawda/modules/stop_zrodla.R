# Punkt stop: „Kiedy drugi alarm coś wnosi” (galeria, przypadek brzegowy) ----
#
# Stoi po piaskownicy z kopiowaniem w rozdziale „Założenie warunkowej
# niezależności”. Trzy pary źródeł, każda z jednym mechanizmem pomyłki:
# różne przyczyny (niezależne), wspólne drgania (częściowo), wspólny przewód
# (pełna zależność). Liczby z risk_two_alarm_posterior() przy parametrach
# Bananpolu. Obraz statyczny: SVG generowane w R, kolory tokenami --upwr-*.
# Fałszywa przyczyna = złoto (jak fałszywe alarmy), niezależność = szałwia.

a3_stop_sources_css <- tags$style(HTML("
  .a3-zr-row { display: flex; flex-wrap: wrap; gap: 1.2em; justify-content: center; }
  .a3-zr-card { flex: 1 1 250px; max-width: 360px; min-width: 0; display: flex; flex-direction: column;
    background: var(--upwr-surface); border: 1px solid var(--upwr-rule); border-radius: 10px; padding: .9em 1em 1em; }
  .a3-zr-head { font: 600 1.05em var(--upwr-sans); color: var(--upwr-ink); margin: 0; }
  .a3-zr-tag { align-self: flex-start; font: 600 .78em var(--upwr-mono); letter-spacing: .02em; margin: .35em 0 .2em;
    padding: .12em .5em; border-radius: 999px; border: 1px solid var(--upwr-rule); color: var(--upwr-ink-soft); }
  .a3-zr-tag.is-indep { border-color: var(--upwr-cat-szalwia); color: var(--upwr-cat-szalwia); }
  .a3-zr-svg { display: block; width: 100%; height: auto; margin: .3em 0 .2em; }
  .a3-zr-svg .a3-zr-obj { fill: var(--upwr-surface-sunken); stroke: var(--upwr-ink-soft); stroke-width: 2; stroke-linejoin: round; }
  .a3-zr-svg .a3-zr-line { fill: none; stroke: var(--upwr-ink-soft); stroke-width: 2; stroke-linecap: round; }
  .a3-zr-svg .a3-zr-ground { stroke: var(--upwr-rule); stroke-width: 2; }
  .a3-zr-svg .a3-zr-cause { fill: none; stroke: var(--upwr-single-alt); stroke-width: 3; stroke-linecap: round; stroke-linejoin: round; }
  .a3-zr-svg .a3-zr-cause-fill { fill: var(--upwr-single-alt); }
  .a3-zr-svg .a3-zr-cloud { fill: var(--upwr-surface); stroke: var(--upwr-ink-subtle); stroke-width: 1.5; }
  .a3-zr-svg .a3-zr-calm { fill: none; stroke: var(--upwr-cat-szalwia); stroke-width: 2.5; stroke-linecap: round; }
  .a3-zr-svg .a3-zr-calm-fill { fill: var(--upwr-cat-szalwia); }
  .a3-zr-svg text { font: 13px var(--upwr-mono); fill: var(--upwr-ink-subtle); }
  .a3-zr-mech { font: .95em/1.4 var(--upwr-sans); color: var(--upwr-ink-soft); margin: .2em 0 .8em; flex: 1 0 auto; }
  .a3-zr-meter-lab { display: flex; flex-wrap: wrap; justify-content: space-between; gap: 0 .6em; font: 12px var(--upwr-mono); color: var(--upwr-ink-subtle); margin-bottom: 4px; }
  .a3-zr-meter { position: relative; height: 12px; border-radius: 6px; background: var(--upwr-surface-sunken); border: 1px solid var(--upwr-rule); }
  .a3-zr-meter i { position: absolute; left: 0; top: 0; bottom: 0; border-radius: 6px 0 0 6px; background: var(--upwr-cat-szalwia); }
  .a3-zr-meter b { position: absolute; top: -5px; width: 4px; height: 20px; margin-left: -2px; border-radius: 2px; background: var(--upwr-ink); }
  .a3-zr-gain { font: 12px var(--upwr-mono); color: var(--upwr-ink-soft); margin-top: 6px; }
  .a3-zr-big { display: flex; flex-direction: column; align-items: flex-start; gap: .2em; margin-top: .6em; }
  .a3-zr-big strong { font: 700 2.2em/1 var(--upwr-mono); color: var(--upwr-accent); }
  .a3-zr-big span { font: 12px var(--upwr-mono); color: var(--upwr-ink-subtle); }
"))

# Rysunki par źródeł (viewBox 240 × 130). Napisy tylko jako krótkie etykiety obiektów.
.a3_zr_drawing <- function(kind) {
  body <- switch(kind,
    # Satelita (podczerwień) nad chmurą ze słońcem; radar na ziemi. Odblask trafia tylko satelitę.
    satellite = paste0(
      '<line class="a3-zr-ground" x1="10" y1="118" x2="230" y2="118"/>',
      '<circle class="a3-zr-cause-fill" cx="206" cy="24" r="10"/>',
      '<path class="a3-zr-cloud" d="M118 74 a14 14 0 0 1 22 -14 a18 18 0 0 1 32 4 a13 13 0 0 1 4 26 h-54 a10 10 0 0 1 -4 -16 z"/>',
      '<path class="a3-zr-cause" d="M198 32 L158 64"/>',
      '<path class="a3-zr-cause" d="M138 70 L72 38" stroke-dasharray="6 5"/>',
      '<path class="a3-zr-cause" d="M78 46 l-8 -8 l11 -2"/>',
      '<rect class="a3-zr-obj" x="38" y="18" width="20" height="16" rx="2"/>',
      '<rect class="a3-zr-obj" x="12" y="21" width="22" height="10"/>',
      '<rect class="a3-zr-obj" x="62" y="21" width="22" height="10"/>',
      '<text x="48" y="54" text-anchor="middle">IR</text>',
      '<path class="a3-zr-obj" d="M42 116 l8 -18 l8 18 z"/>',
      '<path class="a3-zr-obj" d="M30 92 a22 22 0 0 0 38 -16 z"/>',
      '<path class="a3-zr-calm" d="M74 70 a10 10 0 0 1 6 10 M80 62 a18 18 0 0 1 9 20"/>',
      '<text x="94" y="108">radar</text>'
    ),
    # Dwa czujniki na jednym wsporniku; drgania wspornika dochodzą do obu.
    bracket = paste0(
      '<line class="a3-zr-ground" x1="10" y1="118" x2="230" y2="118"/>',
      '<rect class="a3-zr-obj" x="112" y="40" width="16" height="78"/>',
      '<rect class="a3-zr-obj" x="50" y="34" width="140" height="12" rx="3"/>',
      '<rect class="a3-zr-obj" x="56" y="46" width="30" height="24" rx="4"/>',
      '<rect class="a3-zr-obj" x="154" y="46" width="30" height="24" rx="4"/>',
      '<text x="71" y="88" text-anchor="middle">A</text>',
      '<text x="169" y="88" text-anchor="middle">B</text>',
      '<path class="a3-zr-cause" d="M96 112 q4 -6 8 0 t8 0"/>',
      '<path class="a3-zr-cause" d="M128 112 q4 -6 8 0 t8 0"/>',
      '<path class="a3-zr-cause" d="M60 22 q5 -7 10 0 t10 0"/>',
      '<path class="a3-zr-cause" d="M160 22 q5 -7 10 0 t10 0"/>'
    ),
    # Czujnik A i „czujnik” B, który tylko przekazuje sygnał z tego samego przewodu.
    wire = paste0(
      '<line class="a3-zr-ground" x1="10" y1="118" x2="230" y2="118"/>',
      '<rect class="a3-zr-obj" x="24" y="48" width="40" height="32" rx="5"/>',
      '<text x="44" y="100" text-anchor="middle">A</text>',
      '<path class="a3-zr-cause" d="M30 40 l6 -10 l6 8 l6 -12"/>',
      '<path class="a3-zr-line" d="M64 64 H120"/>',
      '<circle class="a3-zr-obj" cx="124" cy="64" r="4"/>',
      '<path class="a3-zr-line" d="M128 64 H176"/>',
      '<path class="a3-zr-line" d="M124 68 V104 H150"/>',
      '<rect class="a3-zr-obj" x="176" y="48" width="40" height="32" rx="5" stroke-dasharray="5 4"/>',
      '<text x="196" y="100" text-anchor="middle">B</text>',
      '<path class="a3-zr-cause" d="M140 58 l8 6 l-8 6"/>',
      '<rect class="a3-zr-obj" x="150" y="96" width="22" height="16" rx="3"/>'
    )
  )
  body
}

a3_stop_sources_ui <- local({
  p_one <- risk_two_alarm_posterior(0.01, 0.95, 0.05, 1)
  p_ind <- risk_two_alarm_posterior(0.01, 0.95, 0.05, 0)
  fmt <- function(p) formatC(p, format = "f", digits = 3)

  card <- function(head, tag, indep, kind, aria, mech, dependence) {
    post <- risk_two_alarm_posterior(0.01, 0.95, 0.05, dependence)
    gain <- (post - p_one) / (p_ind - p_one)
    pct <- sprintf("%.1f%%", 100 * gain)
    tags$div(class = "a3-zr-card",
      tags$p(class = "a3-zr-head", head),
      tags$span(class = paste("a3-zr-tag", if (indep) "is-indep"), tag),
      HTML(sprintf('<svg class="a3-zr-svg" viewBox="0 0 240 130" role="img" aria-label="%s">%s</svg>',
        aria, .a3_zr_drawing(kind))),
      tags$p(class = "a3-zr-mech", mech),
      tags$div(class = "a3-zr-meter-lab",
        tags$span(paste("jeden:", fmt(p_one))), tags$span(paste("niezależne:", fmt(p_ind)))),
      tags$div(class = "a3-zr-meter", `aria-hidden` = "true",
        tags$i(style = sprintf("width:%s", pct)), tags$b(style = sprintf("left:%s", pct))),
      tags$div(class = "a3-zr-gain", sprintf("zysk z drugiego alarmu: %.0f%%", 100 * gain)),
      tags$div(class = "a3-zr-big",
        tags$span("P(A | 🚨🚨)"), tags$strong(fmt(post)))
    )
  }

  figure_panel(
    label = "Galeria",
    title = "Kiedy drugi alarm coś wnosi",
    full_width = TRUE,
    a3_stop_sources_css,
    tags$div(class = "a3-zr-row",
      card("Satelita i radar", "niezależne", TRUE, "satellite",
        "Odblask słońca od chmury trafia w satelitę na podczerwień; radar na ziemi go nie widzi.",
        "Odblask słońca od chmur myli czujnik podczerwieni satelity, ale nie radar: oba mylą się z różnych powodów.",
        0),
      card("Dwa czujniki na jednym wsporniku", "częściowa zależność", FALSE, "bracket",
        "Dwa czujniki A i B na wspólnym wsporniku; drgania wspornika dochodzą do obu.",
        "Wspólne drgania wspornika czasem wywołują alarm w obu naraz; w modelu w połowie zmian B powtarza wynik A (kopiowanie 0.5).",
        0.5),
      card("Czujnik i jego kopia", "pełna zależność", FALSE, "wire",
        "Czujnik A i element B na tym samym przewodzie; B przekazuje dalej sygnał z A.",
        "Drugi tylko powtarza sygnał pierwszego: każda pomyłka A jest też pomyłką B.",
        1)
    ),
    lc_caption(sprintf(paste0(
      "Hala Bananpolu: częstość awarii 0.01, czułość 0.95, FPR 0.05 dla każdego źródła. ",
      "Pasek biegnie od posterioru po jednym alarmie (%s) do posterioru po dwóch niezależnych (%s)."),
      fmt(p_one), fmt(p_ind)))
  )
})
