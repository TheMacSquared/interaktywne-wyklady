# PANELE ROZDZIAŁU 03 „Błędy, p-wartość i decyzja” --------------------------------
#
# Panele bez liczenia i bez własnego JS. Style: modules/panels.css.
#   ch1d_fig_report  Ryc. 3.8  Od p-wartości do zdania w raporcie  kadry (dawny prototyp 5W)
# Pozostałe ryciny rozdziału: czujka dymu (Ryc. 3.1) w proto05.R i proto05.js,
# ryciny liczone w przeglądarce w figures.js, Ryc. 3.7 w figures.R.

# --- Ryc. 3.8: od p-wartości do zdania w raporcie -----------------------------------
# Dwa wyniki z tekstu (różnica 7 pkt: p ≈ 0.016, różnica 4 pkt: p ≈ 0.17) prowadzone
# obok siebie; klasy wierszy i odsłanianie jak w Ryc. 1.3 (panels.css).
.p5w_row <- function(from, label, a, b, class = NULL) {
  tags$div(class = paste(c("p5t-row", if (from > 1) paste0("p5q-f", from), class), collapse = " "),
    tags$span(class = "p5t-l", label),
    tags$div(class = "p5t-c is-a", a), tags$div(class = "p5t-c is-b", b))
}

ch1d_fig_report <- figure_panel(label = "Ryc. 3.8", width_mode = "text", title = "Od p-wartości do zdania w raporcie",
  lc_step_widget("p5_report",
    steps = c("p-wartość", "Porównanie", "Werdykt", "Raport"),
    body = tags$div(class = "p5t p5w",
      tags$div(class = "p5t-row is-head", tags$span(class = "p5t-l"),
        tags$div(class = "p5t-h", "Różnica 7 pkt"), tags$div(class = "p5t-h", "Różnica 4 pkt")),
      .p5w_row(1, "Z danych", tags$b(class = "p5w-num", "p = 0.016"), tags$b(class = "p5w-num", "p = 0.17")),
      .p5w_row(2, "Poziom istotności 0.05", tags$b(class = "p5w-num", "p < α"), tags$b(class = "p5w-num", "p ≥ α")),
      .p5w_row(3, "Werdykt formalny",
        tags$b("Odrzucamy H₀ na rzecz Hₐ"), tags$b("Brak podstaw do odrzucenia H₀")),
      .p5w_row(4, "Zdanie w raporcie",
        tagList(tags$b("Średnia koncentracja z telefonem na biurku istotnie różni się od średniej z telefonem w plecaku"),
                tags$small("p = 0.016, α = 0.05")),
        tagList(tags$b("Nie wykazaliśmy, że średnia koncentracja w obu warunkach się różni"),
                tags$small("p = 0.17, α = 0.05"),
                tags$span(class = "p5t-not", tags$s("Telefon nie wpływa na koncentrację"))))
    ),
    texts = list(
      "Test daje jedną liczbę. Dwa możliwe wyniki eksperymentu z telefonem.",
      "Porównujemy ją z poziomem istotności ustalonym przed badaniem.",
      "Werdykt ma ustaloną formę. Drugi jest celowo ostrożny.",
      "W raporcie werdykt wraca do języka pytania, razem z p-wartością."
    )
  )
)
