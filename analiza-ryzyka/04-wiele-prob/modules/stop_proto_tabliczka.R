# Scena „Tabliczka magazyniera” do rozdziału 3 (prototyp zaakceptowany 2026-10-28)
#
# Kadry historii przed definicją 4.3: Ω (osiem pojemników) → X(ω) (tabliczka
# z liczbą) → przegródki C(3, k) → wagi p^k (1 - p)^(3-k) i rozkład X → ta sama
# mechanika w innych historiach (przełącznik w kadrze 5).
# Zastąpiła prototyp „Ciągi trzech kontroli” (usunięty). Rysuje modules/sceny.js.
#   p4_proto_tally_ui
# Wymaga: stop_proto_sceny.R (p4_scene_shell, p4s_prev_next, p4_banana_phobia).

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

p4_tally_css <- tags$style(HTML("
  .p4s-slide { animation: p4s-slide 900ms cubic-bezier(.3, .1, .3, 1) both; }
  @keyframes p4s-slide { from { transform: translate(var(--dx), var(--dy)); } to { transform: none; } }
  .p4s-grow { transform-box: fill-box; transform-origin: bottom; animation: p4s-grow 900ms ease-out both; }
  @keyframes p4s-grow { from { transform: scaleY(0); } to { transform: none; } }
  @media (prefers-reduced-motion: reduce) { .p4s-slide, .p4s-grow { animation: none !important; } }
"))

p4_proto_tally_ui <- p4_scene_shell("tabliczka", "Tabliczka magazyniera",
  tagList(p4s_prev_next(),
    tags$div(class = "p4s-phobia-slot", style = "display:flex;gap:.6em;align-items:center;",
      tags$div(`data-p4s-out` = "phobia"), tags$div(`data-p4s-out` = "story"))),
  list(stories = p4_tally_stories, storyOrder = names(p4_tally_stories), frameNames = p4_tally_frame_names,
    frame5 = p4_tally_frame5, outcomes = unname(p4_tally_outcomes), phobia = p4_banana_phobia),
  extra = p4_tally_css)
