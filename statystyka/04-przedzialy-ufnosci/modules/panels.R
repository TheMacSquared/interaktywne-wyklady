# PANELE DO ZATRZYMANIA (przyjęte 2026-11-20) -------------------------------------
#
# Trzy panele w miejscach, gdzie wykład miał długi ciąg tekstu albo pojęcie bez obrazu.
# Bez liczenia i bez własnego JS: kadry prowadzi lc_step_widget(), rysunki SVG składa R.
# Style: panels.css. Jak sceny tego wykładu (plakietka „Scena”), nie mają numerów rycin:
# plakietki „Schemat” i „Zestawienie”. Wymaga: lecture_layout.R.
#
#   ch1_panel_dict    Schemat      Słownik estymacji na jednym obrazie    rozdz. 01
#   ch2_panel_random  Zestawienie  Co jest losowe: przed próbą i po niej  rozdz. 02
#   ch5_panel_levels  Zestawienie  Trzy poziomy ufności, trzy ceny        rozdz. 05
# Odrzucony 2026-11-20: trójkąt błędów standardowych (SE różnicy jako przeciwprostokątna).

# --- Schemat: słownik estymacji --------------------------------------------------------
# Populacja (parametr μ nieznany) → próba 100 osób → przepis „policz średnią” (estymator)
# → liczba 171.3 cm (estymata) → wniosek o μ (estymacja). Kropki: stały układ.
.pp_dots <- function(n, seed, w = 150, h = 96, r = 3.2, pick = 0) {
  set.seed(seed)
  x <- runif(n, r + 2, w - r - 2); y <- runif(n, r + 2, h - r - 2)
  cls <- ifelse(seq_len(n) <= pick, "pp-dot is-in", "pp-dot")
  HTML(paste0('<svg class="pp-dots" viewBox="0 0 ', w, ' ', h, '" aria-hidden="true">',
    paste(sprintf('<circle class="%s" cx="%.1f" cy="%.1f" r="%.1f"/>', cls, x, y, r), collapse = ""), "</svg>"))
}

ch1_panel_dict <- figure_panel(label = "Schemat", width_mode = "text", title = "Słownik estymacji na jednym obrazie",
  lc_step_widget("pp_dict",
    steps = c("Populacja", "Próba", "Estymator", "Estymacja"),
    body = tags$div(class = "pp-flow",
      tags$div(class = "pp-card is-pop",
        tags$span(class = "pp-term", "populacja"),
        .pp_dots(150, 4, pick = 0),
        tags$span(class = "pp-desc", "wszyscy studenci w Polsce"),
        tags$span(class = "pp-tag is-param", tags$b("parametr"), HTML("&mu; = ?"))),
      tags$div(class = "pp-arrow pp-f2", tags$span("losujemy"), HTML("&rarr;")),
      tags$div(class = "pp-card is-sample pp-f2",
        tags$span(class = "pp-term", "próba"),
        .pp_dots(26, 9, pick = 26),
        tags$span(class = "pp-desc", "100 zmierzonych osób")),
      tags$div(class = "pp-arrow pp-f3", tags$span("liczymy"), HTML("&rarr;")),
      tags$div(class = "pp-card is-est pp-f3",
        tags$span(class = "pp-term", "statystyka"),
        tags$span(class = "pp-big", HTML("x&#772; = 171.3 cm")),
        tags$span(class = "pp-tag", tags$b("estymator"), "przepis: policz średnią z próby"),
        tags$span(class = "pp-tag", tags$b("estymata"), "wynik przepisu: 171.3 cm")),
      tags$div(class = "pp-back pp-f4",
        HTML("&larr;"), tags$span(tags$b("estymacja:"), " z jednej próby szacujemy nieznane μ"))
    ),
    texts = list(
      "Parametr opisuje całą populację. Ma jedną stałą wartość, ale jej nie znamy.",
      "Zamiast wszystkich mierzymy próbę: tu 100 osób.",
      "Estymator to przepis, estymata to liczba, którą przepis dał w tej próbie.",
      "Estymacja to droga powrotna: z liczby policzonej w próbie do wniosku o populacji."
    )
  )
)

# --- Zestawienie: co jest losowe przed próbą i po niej --------------------------------------
.pp_row <- function(from, label, before, after, class = NULL) {
  tags$div(class = paste(c("pp-row", if (from > 1) paste0("pp-f", from), class), collapse = " "),
    tags$span(class = "pp-row-l", label),
    tags$div(class = "pp-cell is-before", before), tags$div(class = "pp-cell is-after", after))
}

ch2_panel_random <- figure_panel(label = "Zestawienie", width_mode = "text", title = "Co jest losowe: przed próbą i po niej",
  lc_step_widget("pp_random",
    steps = c("Średnia", "Przedział", "Parametr", "Co znaczy 95%"),
    body = tags$div(class = "pp-tbl",
      tags$div(class = "pp-row is-head", tags$span(class = "pp-row-l"),
        tags$div(class = "pp-head", "Przed losowaniem próby"),
        tags$div(class = "pp-head", "Po losowaniu próby")),
      .pp_row(1, "Średnia z próby",
        tagList(tags$b(HTML("X&#772;")), tags$small("losowa: każda próba da inną")),
        tagList(tags$b(HTML("x&#772; = 170 cm")), tags$small("jedna ustalona liczba"))),
      .pp_row(2, "Przedział",
        tagList(tags$b(HTML("X&#772; &plusmn; 1.96 &middot; SE")), tags$small("losowy: jego końce jeżdżą razem ze średnią")),
        tagList(tags$b("[165, 175]"), tags$small("dwa ustalone końce"))),
      .pp_row(3, "Parametr",
        tagList(tags$b("stały, nieznany"), tags$small("nie zależy od próby")),
        tagList(tags$b("stały, nieznany"), tags$small("leży w [165, 175] albo nie")), class = "is-same"),
      .pp_row(4, "Liczba 95%",
        tagList(tags$b("prawdopodobieństwo"), tags$small("że przedział, który dopiero powstanie, obejmie μ")),
        tagList(tags$b("ufność"), tags$small("metoda trafia w 95% prób; ten przedział trafił albo nie"),
                tags$span(class = "pp-not", tags$s("95% szans, że μ leży w [165, 175]"))), class = "is-key")
    ),
    texts = list(
      "Zanim wylosujemy próbę, średnia jest zmienną losową. Po losowaniu to zwykła liczba.",
      "Przedział budujemy wokół średniej, więc przed losowaniem jest losowy razem z nią.",
      "Parametr się nie zmienia. Losowanie próby niczego w nim nie rusza.",
      "Po losowaniu nie zostało nic losowego, czemu można by przypisać 95%. Dlatego mówimy o ufności."
    )
  )
)

# --- Zestawienie: trzy poziomy ufności, trzy ceny ---------------------------------------------
# Szerokość paska ∝ wartość krytyczna z* (1.645, 1.960, 2.576): ten sam wynik, inny zapas.
.pp_level <- function(level, z, miss, when, class = NULL) {
  tags$div(class = paste(c("pp-level", class), collapse = " "),
    tags$span(class = "pp-level-n", paste0(level, "%")),
    tags$div(class = "pp-level-bar", tags$i(style = sprintf("width:%.1f%%;", 100 * z / 2.576))),
    tags$span(class = "pp-level-z", HTML(sprintf("z* = %.3f", z))),
    tags$span(class = "pp-level-miss", miss),
    tags$span(class = "pp-level-when", when))
}

ch5_panel_levels <- figure_panel(label = "Zestawienie", width_mode = "text", title = "Trzy poziomy ufności, trzy ceny",
  tags$div(class = "pp-levels",
    .pp_level(90, 1.645, "chybia 1 przedział na 10", "wstępna eksploracja, gdy pomyłka niewiele kosztuje"),
    .pp_level(95, 1.960, "chybia 1 przedział na 20", "zwyczajowy wybór w raportach i publikacjach", class = "is-default"),
    .pp_level(99, 2.576, "chybia 1 przedział na 100", "badania medyczne, kontrola jakości, bezpieczeństwo")),
  lc_caption("Pasek pokazuje szerokość przedziału z tych samych danych. Większa pewność zawsze kosztuje szerszy przedział.
    Poziom wybieramy przed obejrzeniem wyników.")
)
