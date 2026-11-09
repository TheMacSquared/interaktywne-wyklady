# Eksploratory CI w ECharts: Ryc. 2.2, 2.4, 3.4 i 3.5.
# Dane prób i kwantyle t liczy R przy starcie aplikacji; JS (ci_explorers.js)
# tylko rysuje i przelicza marginesy, więc wykresy działają też bez serwera.

ex_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
          else if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  force(expr)
}

ex_slider <- function(id, key, label, min, max, value, step = 1) {
  input_id <- paste0("ex_", id, "_", key)
  tags$div(class = "lc-grp ci-explorer-slider",
    tags$label(`for` = input_id, label, tags$output(`data-value` = key, value)),
    tags$input(id = input_id, type = "range", min = min, max = max,
               value = value, step = step, `data-ex-input` = key)
  )
}
ex_action <- function(key, label) tags$button(type = "button", class = "lc-action is-outline",
                                              `data-ex-action` = key, label)
ex_actions <- function(...) tags$div(class = "lc-action-group", ...)

ex_panel <- function(id, label, title, controls, config, caption, ..., ratio = "2 / 1") {
  tags$div(class = "ci-explorer", id = paste0("ex_", id), `data-kind` = id,
    `data-config` = jsonlite::toJSON(config, auto_unbox = TRUE, digits = 10, dataframe = "rows"),
    figure_panel(label = label, title = title, width_mode = "wide",
      lc_toolbar(controls, lc_chart_zoom_button()),
      ...,
      tags$div(class = "ci-explorer-chart", style = paste0("aspect-ratio:", ratio),
        tags$div(class = "lc-ec", role = "img", `aria-label` = title)),
      tags$div(class = "ci-explorer-read", role = "status", `aria-live` = "polite"),
      lc_caption(caption)
    )
  )
}

ex_configs <- ex_seed(431026, {
  groups <- lapply(1:8, function(i) fig_2_draw(100))
  st <- fig_3a_stats(.34)
  list(coverage = list(groups = groups, tq = unname(qt((1 + (80:99)/100)/2, 24))),
       people = list(z = qnorm(.975)),
       difference = list(base = 9.08,
         margins = c((st$ci1[2]-st$ci1[1])/2, (st$ci2[2]-st$ci2[1])/2),
         diff_margin = st$d_ist),
       useful = list(tq = unname(qt(.975, 2*(20:2000)-2))))
})

ex_ui <- list(
  coverage = ex_panel("coverage", "Ryc. 2.2", "Sto badań: stos i mapa trafień",
    tagList(ex_slider("coverage", "level", "Ufność (%)", 80, 99, 95),
            ex_action("new", "Następne 100 badań")), ex_configs$coverage,
    "Na mapie są wszystkie 100 badań, na wykresie ostatnie 30; zmiana poziomu zachowuje te same próby.",
    tags$div(class = "ci-explorer-map", role = "img", `aria-label` = "Mapa trafień stu przedziałów"),
    ratio = "1.7 / 1"),
  people = ex_panel("people", "Ryc. 2.4", "Dwa różne znaczenia 95%",
    tagList(ex_slider("people", "n", "Liczba osób", 5, 1600, 25),
            ex_actions(ex_action("n5", "5"), ex_action("n25", "25"), ex_action("n100", "100"), ex_action("n400", "400"))),
    ex_configs$people,
    "Porównanie modelowe: stałe σ = 10 cm i środek 170 cm; górny pas obejmuje 95% wzrostów, dolny ilustruje szerokość 95% CI średniej.",
    ratio = "1.9 / 1"),
  difference = ex_panel("difference", "Ryc. 3.4", "Rozsuń grupy i znajdź pułapkę",
    tagList(ex_slider("difference", "d", "Różnica (g / 100 g)", 0, .8, .34, .01),
            ex_actions(ex_action("near", "Blisko"), ex_action("trap", "Pułapka"),
                       ex_action("far", "Daleko"))), ex_configs$difference,
    "Przesuwasz średnią linii 1; szerokości CI wynikają z tych samych n i s. Górne wiersze są przesunięte o stałe 9.08 g / 100 g; dolny pokazuje różnicę i jej CI.",
    ratio = "1.8 / 1"),
  useful = ex_panel("useful", "Ryc. 3.5", "Czy oszczędność czasu jest użyteczna?",
    tagList(ex_slider("useful", "effect", "Oszczędność (min)", -4, 8, .4, .1),
            ex_slider("useful", "n", "Pomiarów w każdej grupie", 20, 2000, 1000),
            ex_actions(ex_action("tiny", "Mała, precyzyjna"), ex_action("uncertain", "Duża, niepewna"),
                       ex_action("useful", "Wyraźnie użyteczna"))), ex_configs$useful,
    "Założony przed badaniem próg użytecznej zmiany: 2 min; s = 10 min w obu niezależnych grupach, 95% CI różnicy.",
    ratio = "2.5 / 1")
)

ci_explorer <- function(id) ex_ui[[id]]
