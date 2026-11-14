# Stałe rysunki wykładu 01 jako SVG (dawniej ggplot na serwerze): skalują się
# z szerokością, działają w trybie ciemnym (tokeny motywu) i offline.
#   jezyk_ops_svg()      — Rycina 1.1 „Cztery działania na zdarzeniach”
#   jezyk_venn_widget()  — Demonstracja 1.1: lc_step_widget("ch4_venn") z czterema kadrami
#                          diagramu Venna; kadr i opis kroku przełącza modules/jezyk.js
# Wymaga: lecture_layout.R (lc_step_widget).

.j1_col <- function(token) sprintf("var(--upwr-%s)", token)
.j1_mix <- function(token, pct) sprintf("color-mix(in srgb, var(--upwr-%s) %d%%, var(--upwr-panel))", token, pct)

# ---- Rycina 1.1: suma, iloczyn, dopełnienie, różnica ----------------------------
# Jak dawny facet ggplot: prostokąt Ω 10 × 6, koła A (3.9, 3) i B (6.1, 3) o promieniu 2,
# zaznaczony obszar w kolorze akcentu zmieszanym z tłem panelu (45%).
jezyk_ops_svg <- function() {
  s <- 30; pw <- 10 * s; ph <- 6 * s; gx <- 28; top <- 34
  fill <- .j1_mix("accent", 45)
  panel <- function(i, title, region) {
    ox <- 10 + ((i - 1) %% 2) * (pw + gx); oy <- top + ((i - 1) %/% 2) * (ph + top + 14)
    X <- function(x) ox + x * s; Y <- function(y) oy + (6 - y) * s
    circle <- function(cx, attrs = "") sprintf('<circle cx="%g" cy="%g" r="%g"%s/>', X(cx), Y(3), 2 * s, attrs)
    shape <- switch(region,
      union = paste0(circle(3.9, sprintf(' style="fill:%s"', fill)), circle(6.1, sprintf(' style="fill:%s"', fill))),
      inter = sprintf('<clipPath id="j1ops-b%d">%s</clipPath>%s', i, circle(6.1),
        circle(3.9, sprintf(' clip-path="url(#j1ops-b%d)" style="fill:%s"', i, fill))),
      comp = sprintf('<path fill-rule="evenodd" style="fill:%s" d="M%g %g h%g v%g h%g Z M%g %g a%g %g 0 1 0 %g 0 a%g %g 0 1 0 %g 0 Z"/>',
        fill, X(0), Y(6), pw, ph, -pw, X(3.9 - 2), Y(3), 2 * s, 2 * s, 4 * s, 2 * s, 2 * s, -4 * s),
      diff = sprintf('<mask id="j1ops-m%d"><rect x="%g" y="%g" width="%g" height="%g" fill="white"/>%s</mask>%s',
        i, X(0), Y(6), pw, ph, circle(6.1, ' fill="black"'), circle(3.9, sprintf(' mask="url(#j1ops-m%d)" style="fill:%s"', i, fill)))
    )
    paste0(
      sprintf('<text x="%g" y="%g" text-anchor="middle" font-size="19" font-weight="700" style="fill:%s">%s</text>',
        ox + pw / 2, oy - 10, .j1_col("ink"), title),
      sprintf('<rect x="%g" y="%g" width="%g" height="%g" style="fill:%s"/>', X(0), Y(6), pw, ph, .j1_col("panel")),
      shape,
      circle(3.9, sprintf(' style="fill:none;stroke:%s" stroke-width="1.6"', .j1_col("ink"))),
      circle(6.1, sprintf(' style="fill:none;stroke:%s" stroke-width="1.6"', .j1_col("ink"))),
      sprintf('<text x="%g" y="%g" text-anchor="middle" font-size="18" font-weight="700" style="fill:%s">A</text>', X(3.2), Y(3) + 6, .j1_col("ink")),
      sprintf('<text x="%g" y="%g" text-anchor="middle" font-size="18" font-weight="700" style="fill:%s">B</text>', X(6.8), Y(3) + 6, .j1_col("ink")),
      sprintf('<rect x="%g" y="%g" width="%g" height="%g" style="fill:none;stroke:%s" stroke-width="1.3"/>', X(0), Y(6), pw, ph, .j1_col("ink"))
    )
  }
  W <- 2 * pw + gx + 20; H <- 2 * (ph + top) + 14 + 6
  HTML(paste0(
    sprintf('<svg viewBox="0 0 %g %g" style="display:block;width:100%%;max-width:760px;height:auto;margin:0 auto" role="img" ', W, H),
    'aria-label="Cztery diagramy Venna: suma, iloczyn, dopełnienie A oraz różnica A bez B, z zaznaczonym obszarem odpowiadającym każdemu działaniu.">',
    panel(1, "A ∪ B", "union"), panel(2, "A ∩ B", "inter"), panel(3, "Aᶜ", "comp"), panel(4, "A \\ B", "diff"),
    "</svg>"))
}

# ---- Demonstracja 1.1: dlaczego nie wystarczy dodać P(A) i P(B) -----------------
# Kadry jak dawny ggplot (coord_equal, x 0.5–9.5, y 0.6–6.25): A = niebo, B = bursztyn.
jezyk_venn_texts <- c(
  "Dwa zachodzące na siebie zdarzenia. P(A) = 0.70, P(B) = 0.60, a P(A ∩ B) = 0.40. Najpierw zaznaczamy oba zbiory bez wykonywania działania.",
  "Dodajemy całe A i całe B: <span class=\"j1-math\">0.70 + 0.60 = 1.30</span>. Wynik 1.30 nie może być prawdopodobieństwem. Ciemna część wspólna dostała dwa kolory — została policzona dwa razy.",
  "Usuwamy jedną kopię części wspólnej: <span class=\"j1-math\">0.70 + 0.60 − 0.40 = 0.90</span>. Obszar A ∩ B nadal należy do sumy, ale jest w niej liczony tylko raz.",
  "Kiedy samo dodawanie działa? <span class=\"j1-math\">0.40 + 0.35 = 0.75</span>. Koła nie zachodzą na siebie, więc P(A ∩ B) = 0. Niczego nie policzyliśmy dwa razy."
)

jezyk_venn_svg <- function(step) {
  s <- 80
  X <- function(x) (x - 0.5) * s; Y <- function(y) (6.25 - y) * s
  W <- 9 * s; H <- (6.25 - 0.6) * s
  a <- "cat-niebo"; b <- "cat-bursztyn"
  circle <- function(cx, r, style) sprintf('<circle cx="%g" cy="%g" r="%g" style="%s"/>', X(cx), Y(3.25), r * s, style)
  edge <- sprintf("stroke:%s;stroke-width:0.9", .j1_col("ink"))
  frame <- sprintf('<rect x="%g" y="%g" width="%g" height="%g" style="fill:%s;stroke:%s" stroke-width="1.8"/>',
    X(0.8), Y(6.05), 8.4 * s, (6.05 - 0.45) * s, .j1_col("panel"), .j1_col("rule"))
  label <- function(x, name, p) sprintf(
    '<text x="%g" y="%g" text-anchor="middle" font-size="22" font-weight="700" style="fill:%s"><tspan x="%g" dy="0">%s</tspan><tspan x="%g" dy="28">P(%s) = %s</tspan></text>',
    X(x), Y(3.35) - 4, .j1_col("ink"), X(x), name, X(x), name, p)
  badge <- function(line2) sprintf(paste0(
    '<rect x="%g" y="%g" width="230" height="64" rx="6" style="fill:%s;stroke:%s" stroke-width="1.2"/>',
    '<text x="%g" y="%g" text-anchor="middle" font-size="18" font-weight="700" style="fill:%s"><tspan x="%g" dy="0">A ∩ B = 0.40</tspan><tspan x="%g" dy="24">%s</tspan></text>'),
    X(5) - 115, Y(3.25) - 32, .j1_col("surface"), .j1_col("accent"), X(5), Y(3.25) - 6, .j1_col("accent"), X(5), X(5), line2)
  if (step == 4L) {
    body <- paste0(circle(3.15, 1.55, sprintf("fill:%s;fill-opacity:.7;%s", .j1_col(a), edge)),
      circle(6.85, 1.55, sprintf("fill:%s;fill-opacity:.7;%s", .j1_col(b), edge)),
      label(3.15, "A", "0.40"), label(6.85, "B", "0.35"))
  } else {
    ca <- 4.05; cb <- 5.95; r <- 2.15; h <- acos((cb - ca) / (2 * r))
    top <- c(ca + r * cos(h), 3.25 + r * sin(h)); bottom <- c(top[1], 3.25 - r * sin(h))
    lens <- sprintf('M%g %g A%g %g 0 0 1 %g %g A%g %g 0 0 1 %g %g Z',
      X(top[1]), Y(top[2]), r * s, r * s, X(bottom[1]), Y(bottom[2]), r * s, r * s, X(top[1]), Y(top[2]))
    body <- switch(step,
      paste0(circle(ca, r, sprintf("fill:none;stroke:%s;stroke-opacity:.7;stroke-width:2.8", .j1_col(a))),
        circle(cb, r, sprintf("fill:none;stroke:%s;stroke-opacity:.7;stroke-width:2.8", .j1_col(b)))),
      paste0(circle(ca, r, sprintf("fill:%s;fill-opacity:.7;%s", .j1_col(a), edge)),
        circle(cb, r, sprintf("fill:%s;fill-opacity:.7;%s", .j1_col(b), edge))),
      paste0(circle(ca, r, sprintf("fill:%s;%s", .j1_mix(a, 70), edge)),
        circle(cb, r, sprintf("fill:%s;%s", .j1_mix(b, 70), edge)),
        sprintf('<path d="%s" style="fill:%s;stroke:%s;stroke-width:2.4"/>', lens, .j1_mix(a, 70), .j1_col("accent")))
    )
    body <- paste0(body, label(2.7, "A", "0.70"), label(7.3, "B", "0.60"),
      if (step == 2L) badge("POLICZONE 2 RAZY") else if (step == 3L) badge("JEDNO NALICZENIE") else "")
  }
  aria <- c("Dwa zachodzące na siebie koła A i B.", "Koła A i B wypełnione; część wspólna ma dwa kolory.",
    "Część wspólna policzona raz.", "Dwa rozłączne koła A i B.")[step]
  sprintf('<svg viewBox="0 0 %g %g" style="display:block;width:100%%;height:auto" role="img" aria-label="%s">%s%s</svg>',
    W, H, aria, frame, body)
}

# Ciało lc_step_widget: kadry SVG (widoczny ten z bieżącego kroku) i opisy kroków w config.
jezyk_venn_frames <- function() {
  tags$div(class = "j1-venn",
    `data-config` = jsonlite::toJSON(list(texts = jezyk_venn_texts), auto_unbox = TRUE),
    style = "max-width:720px;margin:0 auto;",
    lapply(1:4, function(i) tags$div(`data-venn` = i, hidden = if (i > 1) NA, HTML(jezyk_venn_svg(i)))))
}

# Demonstracja 1.1 bez serwera: opis kroku (#ch4_venn_text) wypełnia JS, więc pole
# nie może być wyjściem Shiny — niepodpięte wyjście zostaje „recalculating” (wyszarzone).
jezyk_venn_widget <- function() {
  w <- lc_step_widget("ch4_venn",
    title = "Dlaczego nie wystarczy dodać P(A) i P(B)?",
    steps = c("Dane", "Naiwna suma", "Korekta", "Zdarzenia rozłączne"),
    body = jezyk_venn_frames())
  htmltools::tagQuery(w)$find("#ch4_venn_text")$removeClass("shiny-html-output")$allTags()
}
