# Scena „Tabliczka magazyniera” do rozdziału 3 (prototyp zaakceptowany 2026-10-28)
#
# Kadry historii przed definicją 4.3: Ω (osiem pojemników) → X(ω) (tabliczka
# z liczbą) → przegródki C(3, k) → wagi p^k (1 - p)^(3-k) i rozkład X → ta sama
# mechanika w innych historiach (przełącznik w kadrze 5).
# Zastąpiła prototyp „Ciągi trzech kontroli” (usunięty).
#   p4_proto_tally_ui; serwer: p4_proto_tally_server(input, output, session).
# Wymaga: stop_proto_sceny.R (p4_scene_shell, p4_svg, p4_scene_head, p4_banana_svg_one).

p4_tally_stories <- list(
  banany = list(label = "Banany", p = 0.1, item = "banana", plaque = "pająki", unit = c("skrzynka", "skrzynki"),
    frames = c(
      "Skrzynka ma trzy kiście, a każda jest czysta albo z pająkiem. Możliwych skrzynek jest więc 2³ = 8 i to jest cała przestrzeń wyników Ω.",
      "Magazynier nie opisuje skrzynki. Na tabliczce zapisuje jedną liczbę: w ilu kiściach siedzi pająk. Każda skrzynka dostaje swoją liczbę.",
      "Różne skrzynki dostają tę samą liczbę. Pod tabliczkami zbiera się 1, 3, 3 i 1 skrzynka: tyle jest sposobów, żeby pająki trafiły do k z trzech kiści.",
      "Skrzynki nie są jednakowo częste: pająk siedzi w kiści z prawdopodobieństwem 0.1. Przegródka waży tyle, ile liczba skrzynek razy szansa jednej z nich."
    )),
  zawory = list(label = "Zawory", p = 0.02, item = "valve", plaque = "niesprawne", unit = c("wynik", "wyniki"),
    frames = c(
      "Kontrolujemy trzy zawory, a każdy jest sprawny albo niesprawny. Możliwych wyników kontroli jest 2³ = 8 i to jest cała przestrzeń wyników Ω.",
      "Kontroler nie przepisuje całego wyniku. W protokole zapisuje jedną liczbę: ile zaworów jest niesprawnych. Każdy wynik dostaje swoją liczbę.",
      "Różne wyniki dostają tę samą liczbę. Pod tabliczkami zbiera się 1, 3, 3 i 1 wynik: tyle jest sposobów, żeby wady trafiły do k z trzech zaworów.",
      "Wyniki nie są jednakowo częste: zawór jest niesprawny z prawdopodobieństwem 0.02. Przegródka waży tyle, ile liczba wyników razy szansa jednego z nich."
    )),
  test = list(label = "Test ABCD", p = 0.25, item = "answer", plaque = "punkty", unit = c("karta", "karty"),
    frames = c(
      "Student zgaduje odpowiedzi na trzy pytania ABCD, a każda jest trafiona albo nie. Możliwych kart odpowiedzi jest 2³ = 8 i to jest cała przestrzeń wyników Ω.",
      "Prowadzący nie przepisuje karty. Do protokołu wpisuje jedną liczbę: ile punktów zdobył student. Każda karta dostaje swoją liczbę.",
      "Różne karty dostają tę samą liczbę punktów. Pod tabliczkami zbiera się 1, 3, 3 i 1 karta: tyle jest sposobów, żeby trafienia padły w k z trzech pytań.",
      "Karty nie są jednakowo częste: zgadując, student trafia z prawdopodobieństwem 0.25. Przegródka waży tyle, ile liczba kart razy szansa jednej z nich."
    ))
)

p4_tally_frame_names <- c("Wszystkie wyniki", "Tabliczka", "Przegródki", "Szanse", "Inne historie")
p4_tally_frame5 <- "Ta sama tabliczka działa w każdej z tych historii. Przegródki 1, 3, 3, 1 zostają, bo zależą tylko od liczby prób; prawdopodobieństwo p zmienia jedynie wysokości słupków."

# Osiem wyników w kolejności: X = 0, potem X = 1, … (żeby strzałki się nie plątały).
p4_tally_outcomes <- local({
  g <- as.matrix(expand.grid(a = 0:1, b = 0:1, c = 0:1)[, 3:1])
  g[order(rowSums(g), -g[, 1], -g[, 2]), , drop = FALSE]
})

p4_tally_item <- function(kind, x, y, hit, phobia = 1L) {
  switch(kind,
    banana = sprintf('<g transform="translate(%g %g) scale(.38)">%s</g>', x, y + 1, p4_banana_svg_one(0, 0, TRUE, hit, phobia)),
    valve = sprintf('<circle class="%s" cx="%g" cy="%g" r="10"/>', if (hit) "p4s-bad" else "p4s-ok", x, y),
    answer = sprintf('<rect class="%s" x="%g" y="%g" width="18" height="18" rx="3"/><text x="%g" y="%g" text-anchor="middle" font-size="13" font-weight="700" fill="%s">%s</text>',
      if (hit) "p4s-bad" else "p4s-idle", x - 9, y - 9, x, y + 5, if (hit) "#fff" else "var(--upwr-ink-subtle)", if (hit) "✓" else "✗")
  )
}

p4_tally_box <- function(kind, x, y, row, phobia = 1L) {
  paste0(sprintf('<rect x="%g" y="%g" width="86" height="40" rx="5" fill="var(--upwr-surface)" stroke="var(--upwr-rule)" stroke-width="1.3"/>', x, y),
    paste(vapply(1:3, function(j) p4_tally_item(kind, x + 16 + (j - 1) * 27, y + 20, row[j] == 1, phobia), character(1)), collapse = ""))
}

p4_tally_svg <- function(step, story, phobia = 1L) {
  st <- p4_tally_stories[[story]]; p <- st$p
  up <- if (step >= 3) 110 else 0
  W <- 760; H <- (if (step >= 4) 500 else if (step == 3) 340 else 190) - up
  row_x <- function(i) 8 + (i - 1) * 93
  col_x <- c(130, 300, 470, 640)
  k_of <- rowSums(p4_tally_outcomes)
  rank_in <- ave(seq_len(8), k_of, FUN = seq_along)
  boxes <- vapply(seq_len(8), function(i) {
    k <- k_of[i]
    if (step < 3) {
      x <- row_x(i); y <- 36
      paste0(p4_tally_box(st$item, x, y, p4_tally_outcomes[i, ], phobia),
        sprintf('<text class="p4s-mono p4s-subtle" x="%g" y="%g" text-anchor="middle" font-size="15">ω%s</text>',
          x + 43, y - 6, chartr("12345678", "₁₂₃₄₅₆₇₈", i)))
    } else {
      x <- col_x[k + 1] - 43; y <- 200 + (rank_in[i] - 1) * 46
      anim <- if (step == 3) sprintf(' class="p4s-slide" style="--dx:%gpx;--dy:%gpx;animation-delay:%dms"', row_x(i) - x, 36 + up - y, (i - 1) * 90) else ""
      sprintf('<g%s>%s</g>', anim, p4_tally_box(st$item, x, y, p4_tally_outcomes[i, ], phobia))
    }
  }, character(1))
  arrows <- if (step == 2) paste(vapply(seq_len(8), function(i) sprintf(
    '<line x1="%g" y1="78" x2="%g" y2="136" stroke="var(--upwr-accent)" stroke-width="1.4" marker-end="url(#p4t-arrow)"/>',
    row_x(i) + 43, col_x[k_of[i] + 1]), character(1)), collapse = "") else ""
  plaques <- if (step >= 2) paste(vapply(0:3, function(k) paste0(
    sprintf('<rect x="%g" y="132" width="124" height="46" rx="5" fill="#8a6a3a"/><rect x="%g" y="136" width="116" height="38" rx="4" fill="#fffaf0"/>', col_x[k + 1] - 62, col_x[k + 1] - 58),
    sprintf('<text class="p4s-mono" x="%g" y="163" text-anchor="middle" font-size="24" font-weight="700">X = %d</text>', col_x[k + 1], k),
    if (step >= 3) sprintf('<text class="p4s-subtle" x="%g" y="192" text-anchor="middle" font-size="15">%d %s</text>',
      col_x[k + 1], choose(3, k), st$unit[if (choose(3, k) == 1) 1 else 2]) else ""
  ), character(1)), collapse = "") else ""
  bars <- if (step >= 4) {
    probs <- dbinom(0:3, 3, p)
    paste0(sprintf('<line class="p4s-axis" x1="40" x2="720" y1="490" y2="490"/>'),
      paste(vapply(0:3, function(k) {
        h <- max(1.5, probs[k + 1] * 110)
        paste0(
          sprintf('<text class="p4s-mono p4s-subtle" x="%g" y="%g" text-anchor="middle" font-size="16">%d · %s</text>',
            col_x[k + 1], 490 - 110 - 22, choose(3, k), format(signif(p^k * (1 - p)^(3 - k), 3), scientific = FALSE)),
          sprintf('<rect class="p4s-bad%s" x="%g" y="%g" width="56" height="%g" rx="2"/>', if (step == 4) " p4s-grow" else "",
            col_x[k + 1] - 28, 490 - h, h),
          sprintf('<text class="p4s-mono" x="%g" y="%g" text-anchor="middle" font-size="20" font-weight="700">%s</text>',
            col_x[k + 1], 490 - h - 6, sprintf(if (probs[k + 1] < 0.001) "%.6f" else "%.3f", probs[k + 1]))
        )
      }, character(1)), collapse = ""))
  } else ""
  p4_svg(W, H, sprintf("Osiem możliwych wyników trzech prób, liczba X przypisana każdemu wynikowi i rozkład X przy p = %g.", p),
    '<defs><marker id="p4t-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" markerHeight="6" orient="auto-start-reverse"><path d="M0 0 L10 5 L0 10 z" fill="var(--upwr-accent)"/></marker></defs>',
    sprintf('<text x="8" y="12" font-size="13" font-weight="600">%s</text>', if (step < 3) "Ω: wszystkie możliwe wyniki" else ""),
    arrows, sprintf('<g transform="translate(0 %d)">', -up), plaques, paste(boxes, collapse = ""), bars, '</g>')
}

p4_tally_css <- tags$style(HTML("
  .p4s-slide { animation: p4s-slide 900ms cubic-bezier(.3, .1, .3, 1) both; }
  @keyframes p4s-slide { from { transform: translate(var(--dx), var(--dy)); } to { transform: none; } }
  .p4s-grow { transform-box: fill-box; transform-origin: bottom; animation: p4s-grow 900ms ease-out both; }
  @keyframes p4s-grow { from { transform: scaleY(0); } to { transform: none; } }
  @media (prefers-reduced-motion: reduce) { .p4s-slide, .p4s-grow { animation: none !important; } }
"))

p4_proto_tally_ui <- tagList(p4_tally_css,
  p4_scene_shell("p4s_tal", "Scena", "Tabliczka magazyniera", proto = FALSE,
    tagList(lc_action("p4s_tal_prev", "Wstecz", icon = "prev"), lc_action("p4s_tal_next", "Dalej", icon = "next", variant = "solid"),
      tags$div(class = "p4s-phobia-slot", style = "display:flex;gap:.6em;align-items:center;",
        uiOutput("p4s_tal_phobia_btn"),
        conditionalPanel("output.p4s_tal_last",
          lc_segmented("p4s_tal_story", NULL, choices = setNames(names(p4_tally_stories),
            vapply(p4_tally_stories, `[[`, "", "label"))))))))

p4_proto_tally_server <- function(input, output, session) {
  step <- reactiveVal(1L)
  observeEvent(input$p4s_tal_next, step(min(5L, step() + 1L)))
  observeEvent(input$p4s_tal_prev, step(max(1L, step() - 1L)))
  story <- reactive(input$p4s_tal_story %||% "banany")
  output$p4s_tal_last <- reactive(step() == 5L)
  outputOptions(output, "p4s_tal_last", suspendWhenHidden = FALSE)
  output$p4s_tal_head <- renderUI(p4_scene_head(p4_tally_frame_names[step()], sprintf("Kadr %d z 5", step())))
  phobia <- reactiveVal(1L)
  observeEvent(input$p4s_tal_phobia, phobia(min(3L, phobia() + 1L)))
  output$p4s_tal_stage <- renderUI(p4_tally_svg(step(), story(), phobia()))
  output$p4s_tal_phobia_btn <- renderUI({
    label <- p4_banana_phobia[[phobia()]]$button
    # zwykły przycisk HTML, jak w „Pająku w kiści”
    if (identical(story(), "banany") && !is.null(label)) tags$button(type = "button", class = "lc-action is-ghost",
      onclick = "Shiny.setInputValue('p4s_tal_phobia', Date.now(), {priority: 'event'})", tags$span(label))
  })
  output$p4s_tal_caption <- renderUI(tags$p(class = "p4s-caption",
    if (step() == 5L) p4_tally_frame5 else p4_tally_stories[[story()]]$frames[step()]))
}
