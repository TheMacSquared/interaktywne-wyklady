# Warianty do burzy mózgów — celowo stoją tuż po obecnych rycinach.
# Dane i kwantyle liczone w R; Chart.js odpowiada za płynną eksplorację.

bb_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
          else if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  force(expr)
}

bb_slider <- function(id, key, label, min, max, value, step = 1) {
  input_id <- paste0("bb_", id, "_", key)
  tags$div(class = "lc-grp bb-slider",
    tags$label(`for` = input_id, label, tags$output(`data-value` = key, value)),
    tags$input(id = input_id, type = "range", min = min, max = max,
               value = value, step = step, `data-bb-input` = key)
  )
}
bb_action <- function(key, label) tags$button(type = "button", class = "lc-action is-outline",
                                             `data-bb-action` = key, label)
bb_actions <- function(...) tags$div(class = "lc-action-group", ...)
bb_canvas <- function(label, ratio = "2 / 1") tags$div(class = "bb-chart",
  style = paste0("aspect-ratio:", ratio),
  tags$canvas(role = "img", `aria-label` = label, label))
bb_panel <- function(id, title, controls, config, caption, ..., ratio = "2 / 1") {
  tags$div(class = "bb-widget", id = paste0("bb_", id), `data-kind` = id,
    `data-config` = jsonlite::toJSON(config, auto_unbox = TRUE, digits = 10, dataframe = "rows"),
    figure_panel(label = paste0("Wariant ", bb_variants[[id]]),
      title = title, width_mode = "wide",
      lc_toolbar(controls, lc_chart_zoom_button()),
      ...,
      bb_canvas(title, ratio),
      tags$div(class = "bb-read", role = "status", `aria-live` = "polite"),
      lc_caption(caption)
    )
  )
}
# Numery wariantów zgodne z BRAINSTORM.md; B1 i B2 (rozdz. 1) odrzucone 2026-11-09.
bb_variants <- c(coverage = "B3", people = "B4", difference = "B5",
                 useful = "B6", defects = "B7", publish = "B8")

bb_configs <- bb_seed(431026, {
  groups <- lapply(1:8, function(i) s4p_2_draw(100))
  # Stałe 95% CI z 4000 niezależnych, poprawnie raportowanych badań.
  x <- matrix(rnorm(4000 * 40, 26, 8), nrow = 40)
  means <- colMeans(x); se <- apply(x, 2, sd) / sqrt(40)
  publication <- data.frame(mean = means, lo = means - qt(.975, 39) * se,
                            hi = means + qt(.975, 39) * se)
  z <- qnorm(.975)
  defect_intervals <- lapply(0:50, function(k) {
    list(wald = unname(s4p_ch4_wald(k, 50, z)),
         wilson = unname(s4p_ch4_wilson(k, 50, z)),
         exact = c(if (k == 0) 0 else qbeta(.025, k, 51-k),
                   if (k == 50) 1 else qbeta(.975, k+1, 50-k)))
  })
  st <- s4p_3a_stats(.34)
  list(coverage = list(groups = groups, tq = unname(qt((1 + (80:99)/100)/2, 24))),
       people = list(z = z), difference = list(base = 9.08,
         margins = c((st$ci1[2]-st$ci1[1])/2, (st$ci2[2]-st$ci2[1])/2),
         diff_margin = st$d_ist),
       useful = list(tq = unname(qt(.975, 2*(20:2000)-2))),
       defects = list(intervals = defect_intervals), publish = list(rows = publication, xlim = c(floor(min(publication$lo)), ceiling(max(publication$hi)))))
})

bb_ui <- list(
  coverage = bb_panel("coverage", "Sto badań: stos i mapa trafień",
    tagList(bb_slider("coverage", "level", "Ufność (%)", 80, 99, 95),
            bb_action("new", "Następne 100 badań")), bb_configs$coverage,
    "Na mapie są wszystkie 100 badań, na wykresie ostatnie 30; zmiana poziomu zachowuje te same próby.",
    tags$div(class = "bb-map", role = "img", `aria-label` = "Mapa trafień stu przedziałów"),
    ratio = "1.7 / 1"),
  people = bb_panel("people", "Dwa różne znaczenia 95%",
    tagList(bb_slider("people", "n", "Liczba osób", 5, 1600, 25),
            bb_actions(bb_action("n5", "5"), bb_action("n25", "25"), bb_action("n100", "100"), bb_action("n400", "400"))),
    bb_configs$people,
    "Porównanie modelowe: stałe σ = 10 cm i środek 170 cm; górny pas obejmuje 95% wzrostów, dolny ilustruje szerokość 95% CI średniej.",
    ratio = "1.9 / 1"),
  difference = bb_panel("difference", "Rozsuń grupy i znajdź pułapkę",
    tagList(bb_slider("difference", "d", "Różnica (g / 100 g)", 0, .8, .34, .01),
            bb_actions(bb_action("near", "Blisko"), bb_action("trap", "Pułapka"),
                       bb_action("far", "Daleko"))), bb_configs$difference,
    "Przesuwasz średnią linii 1; szerokości CI wynikają z tych samych n i s. Górne wiersze są przesunięte o stałe 9.08 g / 100 g; dolny pokazuje różnicę i jej CI.",
    ratio = "1.8 / 1"),
  useful = bb_panel("useful", "Czy oszczędność czasu jest użyteczna?",
    tagList(bb_slider("useful", "effect", "Oszczędność (min)", -4, 8, .4, .1),
            bb_slider("useful", "n", "Pomiarów w każdej grupie", 20, 2000, 1000),
            bb_actions(bb_action("tiny", "Mała, precyzyjna"), bb_action("uncertain", "Duża, niepewna"),
                       bb_action("useful", "Wyraźnie użyteczna"))), bb_configs$useful,
    "Założony przed badaniem próg użytecznej zmiany: 2 min; s = 10 min w obu niezależnych grupach, 95% CI różnicy.",
    ratio = "2.5 / 1"),
  defects = bb_panel("defects", "Zaznacz wadliwe sztuki",
    bb_actions(bb_action("zero", "0 wad"), bb_action("one", "1 wada"), bb_action("four", "4 wady")),
    bb_configs$defects,
    "Każdy kwadrat to jedna z 50 sprawdzonych sztuk. Wald, Wilson i dokładny Clopper–Pearson dla tej samej próby, ufność 95%.",
    tags$div(class = "bb-products", role = "group", `aria-label` = "Próba 50 produktów"),
    ratio = "2.3 / 1"),
  publish = bb_panel("publish", "Co zostaje po selekcji wyników?",
    bb_actions(bb_action("one", "+1 badanie"), bb_action("hundred", "+100"),
               bb_action("thousand", "+1000"), bb_action("filter", "Tylko dolna granica > 26"),
               bb_action("reset", "Od nowa")), bb_configs$publish,
    "Prawda modelu: μ = 26 min. Każde badanie ma stałe 95% CI; filtr wybiera wyłącznie wyniki sugerujące μ > 26. Pokazujemy ostatnie 40 spośród widocznych.",
    ratio = "1.7 / 1")
)

bb <- function(id) bb_ui[[id]]
