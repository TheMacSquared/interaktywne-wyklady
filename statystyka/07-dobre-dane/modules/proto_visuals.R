# PROTOTYP wizualne-stopy (07-dobre-dane): diagramy-punkty zatrzymania.
# Cały plik jest prototypem; wywołania oznaczone „# PROTOTYP <id>” w modułach.
#   schemat — dd_schema(): struktura zbioru (jednostka obserwacji, n, typy kolumn)
#   katalog — dd_problem_map(): siedem problemów w dwóch grupach (rozdz. 1)
#   mapa    — dd_dataset_map(): dziesięć zbiorów × siedem problemów (rozdz. 11)
#   braki   — dd_missing_matrix_plot(): mapa braków w wierszach (rozdz. 1, § 5)

# ============================================================================
# Wspólne: typy zmiennych i problemy z katalogu
# ============================================================================

# Typy kolumn: kolor jak type_colors (R/palette.R), przez zmienne CSS (ciemny motyw).
.dd_types <- list(
  id         = list(label = "identyfikator", color = "var(--upwr-reference)"),
  nominalna  = list(label = "nominalna",     color = "var(--upwr-cat-terakota)"),
  porzadkowa = list(label = "porządkowa",    color = "var(--upwr-cat-bursztyn)"),
  dyskretna  = list(label = "dyskretna",     color = "var(--upwr-cat-niebo)"),
  ciagla     = list(label = "ciągła",        color = "var(--upwr-cat-szalwia)"),
  data       = list(label = "data",          color = "var(--upwr-reference)"),
  tekst      = list(label = "tekst",         color = "var(--upwr-cat-grafit)")
)

# Siedem problemów z rozdziału 1. group: "crit" (dyskwalifikuje) / "fix" (naprawialny).
.dd_problems <- list(
  list(k = 1, name = "Za mało danych",            short = "mało danych",   group = "crit", sec = "sec-p1"),
  list(k = 2, name = "Brak zmienności",           short = "zmienność",     group = "crit", sec = "sec-p2"),
  list(k = 3, name = "Błędy i literówki",         short = "błędy",         group = "fix",  sec = "sec-p3"),
  list(k = 4, name = "Źle zdefiniowane zmienne",  short = "definicje",     group = "fix",  sec = "sec-p4"),
  list(k = 5, name = "Braki danych",              short = "braki",         group = "fix",  sec = "sec-p5"),
  list(k = 6, name = "Brak niezależności",        short = "niezależność",  group = "crit", sec = "sec-p6"),
  list(k = 7, name = "Zła struktura",             short = "struktura",     group = "crit", sec = "sec-p7")
)

# Miniaturowe glify problemów (SVG 64×40, kolor z currentColor).
dd_problem_glyph <- function(k) {
  dot <- function(x, y, r = 3.2, fill = "currentColor", op = 1) {
    tags$circle(cx = x, cy = y, r = r, fill = fill, `fill-opacity` = op)
  }
  body <- switch(as.character(k),
    # 1: trzy samotne punkty
    "1" = list(dot(16, 26), dot(32, 14), dot(47, 24)),
    # 2: wszystko w jednym słupku
    "2" = list(
      tags$line(x1 = 4, y1 = 36, x2 = 60, y2 = 36, stroke = "currentColor", `stroke-opacity` = .45),
      tags$rect(x = 8,  y = 33, width = 9, height = 3,  fill = "currentColor", `fill-opacity` = .35),
      tags$rect(x = 20, y = 34, width = 9, height = 2,  fill = "currentColor", `fill-opacity` = .35),
      tags$rect(x = 32, y = 33, width = 9, height = 3,  fill = "currentColor", `fill-opacity` = .35),
      tags$rect(x = 44, y = 5,  width = 9, height = 31, fill = "currentColor")),
    # 3: skupisko i jeden punkt daleko, przekreślony
    "3" = list(dot(8, 30, 2.6), dot(14, 27, 2.6), dot(12, 33, 2.6), dot(19, 30, 2.6), dot(16, 24, 2.6),
      tags$path(d = "M48 6 L58 16 M58 6 L48 16", stroke = "currentColor", `stroke-width` = 2.4,
                `stroke-linecap` = "round")),
    # 4: komórki z różnym zapisem
    "4" = list(
      tags$rect(x = 4,  y = 6,  width = 26, height = 10, rx = 2, fill = "none", stroke = "currentColor", `stroke-width` = 1.6),
      tags$rect(x = 4,  y = 22, width = 14, height = 10, rx = 2, fill = "none", stroke = "currentColor", `stroke-width` = 1.6),
      tags$rect(x = 34, y = 14, width = 22, height = 10, rx = 2, fill = "none", stroke = "currentColor", `stroke-width` = 1.6),
      tags$text(x = 52, y = 38, `font-size` = 14, `font-weight` = 700, fill = "currentColor",
                `text-anchor` = "middle", "?")),
    # 5: siatka komórek z dziurami
    "5" = {
      holes <- c(2, 7, 9)
      lapply(0:11, function(i) {
        col <- i %% 4; row <- i %/% 4
        tags$rect(x = 6 + col * 14, y = 4 + row * 11, width = 11, height = 8, rx = 1.5,
          fill = if (i %in% holes) "none" else "currentColor",
          stroke = "currentColor", `stroke-width` = 1.2,
          `fill-opacity` = if (i %in% holes) 0 else .75,
          `stroke-dasharray` = if (i %in% holes) "2 1.5")
      })
    },
    # 6: punkty połączone w szereg
    "6" = {
      xs <- seq(5, 59, length.out = 9)
      ys <- c(30, 27, 22, 17, 14, 15, 19, 24, 27)
      c(list(tags$polyline(points = paste(xs, ys, sep = ",", collapse = " "),
                           fill = "none", stroke = "currentColor", `stroke-width` = 1.6)),
        lapply(seq_along(xs), function(i) dot(xs[i], ys[i], 2.4)))
    },
    # 7: wiele wierszy zbiega się w jeden
    "7" = list(
      lapply(0:4, function(i) tags$line(x1 = 4, y1 = 6 + i * 7, x2 = 22, y2 = 6 + i * 7,
        stroke = "currentColor", `stroke-width` = 3, `stroke-opacity` = .55)),
      tags$path(d = "M27 6 Q35 6 37 20 Q35 34 27 34", fill = "none", stroke = "currentColor", `stroke-width` = 1.6),
      tags$line(x1 = 42, y1 = 20, x2 = 60, y2 = 20, stroke = "currentColor", `stroke-width` = 5))
  )
  tags$svg(class = "dd-glyph", viewBox = "0 0 64 40", width = 64, height = 40,
           `aria-hidden` = "true", body)
}

dd_group_color <- function(group) {
  if (group == "crit") "var(--upwr-accent)" else "var(--upwr-cat-bursztyn)"
}

# ============================================================================
# PROTOTYP schemat: struktura zbioru
# ============================================================================
# unit: czym jest jeden wiersz (np. "okręg szkolny"); n: liczba wierszy;
# cols: nazwany wektor kolumna → klucz typu z .dd_types.
dd_schema <- function(unit, n, cols) {
  keys <- unname(cols)
  stopifnot(all(keys %in% names(.dd_types)))
  k <- length(cols)
  cell <- function(key) tags$div(class = "dds-cell",
    style = paste0("--c:", .dd_types[[key]]$color, ";"))
  n_fmt <- as.character(n)

  tags$div(class = "dds", role = "img",
    `aria-label` = sprintf("Struktura zbioru: %s wierszy, jeden wiersz to %s; kolumny: %s.",
      n_fmt, unit, paste(sprintf("%s (%s)", names(cols),
        vapply(keys, function(x) .dd_types[[x]]$label, "")), collapse = ", ")),
    tags$div(class = "dds-grid", style = paste0("--k:", k, ";"),
      # nagłówki kolumn
      tags$div(class = "dds-gutter"),
      lapply(names(cols), function(nm) tags$div(class = "dds-head", tags$span(nm))),
      # pierwszy wiersz = jednostka obserwacji
      tags$div(class = "dds-gutter dds-unit",
        tags$span(class = "dds-unit-k", "1 wiersz"),
        tags$span(class = "dds-unit-v", unit)),
      lapply(keys, function(key) tags$div(class = "dds-cell is-first",
        style = paste0("--c:", .dd_types[[key]]$color, ";"))),
      # kolejne wiersze (wyblakłe)
      lapply(1:3, function(r) tagList(
        tags$div(class = paste("dds-gutter", if (r == 2) "dds-n"),
          if (r == 2) tags$span(paste0("n = ", n_fmt))),
        lapply(keys, function(key) tags$div(class = paste0("dds-cell is-ghost g", r),
          style = paste0("--c:", .dd_types[[key]]$color, ";")))
      ))
    ),
    tags$div(class = "dds-legend",
      lapply(intersect(names(.dd_types), keys), function(key) tags$span(class = "dds-leg",
        tags$i(style = paste0("--c:", .dd_types[[key]]$color, ";")),
        .dd_types[[key]]$label))
    )
  )
}

# ============================================================================
# PROTOTYP katalog: siedem problemów w dwóch grupach
# ============================================================================
dd_problem_map <- function() {
  tile <- function(p) {
    tags$a(class = "ddp-tile", href = paste0("#", p$sec),
      style = paste0("--c:", dd_group_color(p$group), ";"),
      tags$span(class = "ddp-k", p$k),
      dd_problem_glyph(p$k),
      tags$span(class = "ddp-name", p$name))
  }
  group <- function(g, label) {
    ps <- Filter(function(p) p$group == g, .dd_problems)
    tags$div(class = "ddp-group", style = paste0("--c:", dd_group_color(g), ";"),
      tags$div(class = "ddp-glabel", label),
      tags$div(class = "ddp-tiles", lapply(ps, tile)))
  }
  tags$div(class = "ddp", role = "img",
    `aria-label` = "Siedem problemów w danych. Dyskwalifikują zbiór: za mało danych, brak zmienności, brak niezależności, zła struktura. Do naprawy: błędy i literówki, źle zdefiniowane zmienne, braki danych.",
    group("crit", "Dyskwalifikują zbiór"),
    group("fix", "Do naprawy"))
}

# ============================================================================
# PROTOTYP mapa: dziesięć zbiorów × siedem problemów
# ============================================================================
# Poziom problemu: 2 = poważny, 1 = ślad (jedna podgrupa, łagodna postać).
# Werdykt: good / clean (dobry po czyszczeniu) / bad.
.dd_datasets <- list(
  list(ch = "02", name = "Szkoły w Kalifornii",   p = c(),                verdict = "good"),
  list(ch = "03", name = "Ankieta na grupie",     p = c(`1` = 2),         verdict = "bad"),
  list(ch = "04", name = "Pingwiny",              p = c(`5` = 1),         verdict = "good"),
  list(ch = "05", name = "Filmy Tarantino",       p = c(`1` = 2, `6` = 2, `7` = 2), verdict = "bad"),
  list(ch = "06", name = "Hotel",                 p = c(`2` = 2),         verdict = "bad"),
  list(ch = "07", name = "Wynagrodzenia",         p = c(`2` = 1),         verdict = "good"),
  list(ch = "08", name = "Formularz",             p = c(`4` = 2, `5` = 2), verdict = "bad"),
  list(ch = "09", name = "Laboratorium",          p = c(`3` = 2),         verdict = "clean"),
  list(ch = "10", name = "Ankieta studencka",     p = c(`1` = 1),         verdict = "good"),
  list(ch = "11", name = "Kawiarnia",             p = c(`5` = 2, `6` = 2), verdict = "bad")
)
.dd_verdicts <- list(
  good  = list(label = "dobry",           color = "var(--upwr-sage)"),
  clean = list(label = "po czyszczeniu",  color = "var(--upwr-cat-bursztyn)"),
  bad   = list(label = "do odrzucenia",   color = "var(--upwr-accent)")
)

dd_dataset_map <- function() {
  head <- tags$div(class = "ddm-row ddm-head",
    tags$div(class = "ddm-name"),
    lapply(.dd_problems, function(p) tags$div(class = "ddm-ph",
      style = paste0("--c:", dd_group_color(p$group), ";"),
      tags$span(class = "ddm-pk", p$k), dd_problem_glyph(p$k),
      tags$span(class = "ddm-ps", p$short))),
    tags$div(class = "ddm-verdict"))
  rows <- lapply(.dd_datasets, function(d) {
    tags$div(class = "ddm-row",
      tags$div(class = "ddm-name", tags$span(class = "ddm-ch", d$ch), d$name),
      lapply(.dd_problems, function(p) {
        lvl <- d$p[as.character(p$k)]
        lvl <- if (length(lvl) == 0 || is.na(lvl)) 0 else lvl
        tags$div(class = "ddm-cell", style = paste0("--c:", dd_group_color(p$group), ";"),
          if (lvl > 0) tags$i(class = if (lvl == 2) "ddm-dot" else "ddm-ring"))
      }),
      tags$div(class = "ddm-verdict",
        tags$span(class = "ddm-chip",
          style = paste0("--c:", .dd_verdicts[[d$verdict]]$color, ";"),
          .dd_verdicts[[d$verdict]]$label)))
  })
  tags$div(class = "ddm", role = "img",
    `aria-label` = paste("Mapa problemów:", paste(vapply(.dd_datasets, function(d)
      sprintf("%s — %s", d$name, .dd_verdicts[[d$verdict]]$label), ""), collapse = "; ")),
    head, rows,
    tags$div(class = "ddm-legend",
      tags$span(tags$i(class = "ddm-dot"), "poważny"),
      tags$span(tags$i(class = "ddm-ring"), "ślad")))
}

# ============================================================================
# PROTOTYP braki: mapa braków (wiersz × zmienna)
# ============================================================================
dd_missing_matrix_plot <- function(df, id_col = "id") {
  vars <- setdiff(names(df), id_col)
  complete <- stats::complete.cases(df[vars])
  long <- do.call(rbind, lapply(vars, function(v) data.frame(
    id = df[[id_col]], var = v, state = ifelse(is.na(df[[v]]), "brak", "jest"))))
  long <- rbind(long, data.frame(id = df[[id_col]], var = "komplet",
    state = ifelse(complete, "komplet", "niekomplet")))
  long$var <- factor(long$var, levels = c(vars, "komplet"))
  long$id  <- factor(long$id, levels = rev(df[[id_col]]))
  long <- long[long$state != "niekomplet", ]

  ggplot(long, aes(x = var, y = id, fill = state)) +
    geom_tile(aes(alpha = state), color = NA, width = 0.92, height = 0.82) +
    geom_vline(xintercept = length(vars) + 0.5, color = upwr_reference, linewidth = 0.6) +
    scale_fill_manual(values = c(jest = unname(upwr_cat["niebo"]),
                                 brak = data_bad,
                                 komplet = data_good), guide = "none") +
    scale_alpha_manual(values = c(jest = 0.45, brak = 1, komplet = 1), guide = "none") +
    scale_x_discrete(position = "top", drop = FALSE) +
    labs(x = NULL, y = "Osoba (id)") +
    theme_upwr(base_size = 14) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.ticks = element_blank())
}

# ============================================================================
# CSS prototypów (wstrzykiwany w app.R przez header_extras)
# ============================================================================
dd_proto_css <- "
/* PROTOTYP schemat */
.dds { font-family: var(--upwr-sans); color: var(--upwr-ink); padding: .4em 0 .2em; }
.dds-grid {
  display: grid; grid-template-columns: 9.5em repeat(var(--k), minmax(2.4em, 5.2em));
  column-gap: 4px; row-gap: 4px; align-items: end; padding-right: 5.5em;
  overflow-x: auto;
}
.dds-head { position: relative; height: 6.2em; }
.dds-head span {
  position: absolute; left: 50%; bottom: .35em; transform-origin: left bottom;
  transform: rotate(-40deg); white-space: nowrap;
  font: 500 .82em/1 var(--upwr-mono); color: var(--upwr-ink-soft);
}
.dds-cell { height: 1.7em; border-radius: 3px; background: var(--c); }
.dds-cell.is-ghost { background: color-mix(in srgb, var(--c) 28%, var(--upwr-surface)); }
.dds-cell.g2 { opacity: .7; } .dds-cell.g3 { opacity: .4; }
.dds-gutter { align-self: center; padding-right: .8em; text-align: right; }
.dds-unit { display: flex; flex-direction: column; line-height: 1.15; }
.dds-unit-k { font: 600 .72em/1.2 var(--upwr-mono); letter-spacing: .06em;
  text-transform: uppercase; color: var(--upwr-ink-subtle); }
.dds-unit-v { font-weight: 600; font-size: .98em; color: var(--upwr-accent); }
.dds-n span { font: 600 .95em var(--upwr-mono); color: var(--upwr-ink); }
.dds-legend { display: flex; flex-wrap: wrap; gap: .3em 1.1em; margin: .9em 0 0 9.5em;
  font-size: .82em; color: var(--upwr-ink-soft); }
.dds-leg i { display: inline-block; width: .9em; height: .9em; border-radius: 2px;
  background: var(--c); margin-right: .35em; vertical-align: -.1em; }
@container (max-width: 560px) {
  .dds-grid { grid-template-columns: 6.5em repeat(var(--k), minmax(1.6em, 1fr)); }
  .dds-legend { margin-left: 0; }
}

/* PROTOTYP katalog */
.ddp { display: grid; grid-template-columns: 1fr; gap: 1.1em; padding: .3em 0; }
.ddp-glabel { font: 600 .74em/1.2 var(--upwr-mono); letter-spacing: .07em;
  text-transform: uppercase; color: var(--c); border-bottom: 2px solid var(--c);
  padding-bottom: .35em; margin-bottom: .7em; }
.ddp-tiles { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: .6em; }
.ddp-tile {
  display: flex; flex-direction: column; align-items: center; gap: .3em;
  padding: .7em .4em .6em; border-radius: 6px; text-decoration: none !important;
  background: color-mix(in srgb, var(--c) 9%, var(--upwr-surface));
  border: 1px solid color-mix(in srgb, var(--c) 35%, var(--upwr-surface));
  color: var(--c); transition: background .15s;
}
.ddp-tile:hover { background: color-mix(in srgb, var(--c) 18%, var(--upwr-surface)); }
.ddp-k { font: 700 1.05em/1 var(--upwr-mono); align-self: flex-start; margin: -.2em 0 -.6em .1em; }
.ddp-name { font-size: .86em; line-height: 1.2; text-align: center; color: var(--upwr-ink); }
@container (max-width: 520px) { .ddp-tiles { grid-template-columns: repeat(2, minmax(0, 1fr)); } }

/* PROTOTYP mapa */
.ddm { font-family: var(--upwr-sans); color: var(--upwr-ink); overflow-x: auto; }
.ddm-row { display: grid; grid-template-columns: minmax(11em, 1.6fr) repeat(7, minmax(3.6em, 1fr)) 8.2em;
  align-items: center; min-width: 40em; border-bottom: 1px solid var(--upwr-rule-soft); }
.ddm-row:not(.ddm-head) { height: 2.3em; }
.ddm-head { border-bottom: 1.5px solid var(--upwr-rule); padding-bottom: .4em; align-items: end; }
.ddm-ph { display: flex; flex-direction: column; align-items: center; color: var(--c); gap: .1em; }
.ddm-ph .dd-glyph { width: 2.6em; height: 1.65em; }
.ddm-pk { font: 700 .8em/1 var(--upwr-mono); }
.ddm-ps { font-size: .7em; color: var(--upwr-ink-soft); text-align: center; line-height: 1.1; }
.ddm-name { font-size: .92em; }
.ddm-ch { font: 600 .8em var(--upwr-mono); color: var(--upwr-ink-subtle); margin-right: .7em; }
.ddm-cell { display: flex; justify-content: center; }
.ddm-dot, .ddm-ring { display: inline-block; width: .95em; height: .95em; border-radius: 50%; }
.ddm-dot { background: var(--c, var(--upwr-ink-soft)); }
.ddm-ring { border: 2px solid var(--c, var(--upwr-ink-soft)); width: .75em; height: .75em; }
.ddm-verdict { text-align: right; }
.ddm-chip { display: inline-block; font-size: .78em; font-weight: 600; padding: .15em .6em;
  border-radius: 999px; color: var(--c);
  background: color-mix(in srgb, var(--c) 14%, var(--upwr-surface));
  border: 1px solid color-mix(in srgb, var(--c) 45%, var(--upwr-surface)); }
.ddm-legend { display: flex; gap: 1.2em; justify-content: flex-end; margin-top: .6em;
  font-size: .8em; color: var(--upwr-ink-soft); }
.ddm-legend i { margin-right: .35em; vertical-align: -.12em; }
"
