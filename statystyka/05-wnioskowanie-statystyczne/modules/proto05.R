# PROTOTYPY PUNKTÓW STOP 5A–5E (2026-11-09) ----------------------------------
#
# Każdy prototyp ma złotą plakietkę „Prototyp <nr> · <rodzaj>”, jak w wykładzie 04.
# Rysunek i obliczenia: modules/proto05.js (+ proto05.css), działają bez serwera R.
# Lista propozycji: PUNKTY-STOP-PROPOZYCJA.md, decyzje: KOLEJKA-ZATWIERDZANIA.md.
# 5B (dubel Ryc. 3.1) i 5C (rozkład r z powtórzonych grupek) odrzucone 2026-11-09.
# Wstawki w tekście: p5("5A"), p5("5D"), p5("5E"); teksty kroków: p5_server().
# Wymaga: lecture_layout.R (lc_step_widget, lc_toolbar, figure_panel), helpers.R.

p5_panel <- function(num, kind, title, ..., width_mode = "text") {
  figure_panel(label = paste0("Prototyp ", num, " · ", kind), title = title,
    color = upwr_single_alt, width_mode = width_mode, ...)
}

p5 <- function(num) get(paste0("p5_", num, "_ui"), inherits = TRUE)

.p5_body <- function(config) {
  tags$div(class = "p5-sc",
    `data-config` = jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA))
}

# Scena krokowa z przyciskiem czynności i opcjonalnie +10/+100/+1000.
p5_scene_widget <- function(id, title, steps, config, action = NULL, options = NULL,
                            more_from = 2, more = c("+10" = "m10", "+100" = "m100", "+1000" = "m1000")) {
  lc_step_widget(id,
    title = title,
    steps = steps,
    toolbar = if (length(options) || !is.null(action)) lc_toolbar(
      .scene_options(options),
      if (!is.null(action)) tags$button(type = "button", class = "lc-action is-solid",
        `data-sc-act` = "go", lc_icon("shuffle"), tags$span(action)),
      if (!is.null(action) && !is.null(more)) lc_step_from(more_from,
        tags$div(class = "lc-seg", role = "group", `aria-label` = "Więcej powtórzeń",
          lapply(seq_along(more), function(i) tags$button(type = "button",
            `data-sc-act` = unname(more[[i]]), names(more)[[i]]))))
    ),
    body = .p5_body(config)
  )
}

# Jeden obraz z suwakiem (bez kroków). Suwak to zwykły input range: działa offline.
p5_static <- function(id, config, name, label, min, max, value, step = 1) {
  tags$div(id = id, class = "p5-host",
    lc_toolbar(tags$label(class = "p5-range",
      tags$span(label),
      tags$input(type = "range", min = min, max = max, value = value, step = step,
        `data-p5-range` = name, `aria-label` = label),
      tags$output(`data-p5-out` = name, value))),
    .p5_body(config),
    tags$div(class = "p5-status", `aria-live` = "polite")
  )
}

# --- 5A: czujka dymu (rozdz. 03, przed tabelą błędów) -------------------------
p5_5A_ui <- p5_panel("5A", "scena", "Czujka dymu w akademiku",
  p5_static("p5_smoke",
    config = list(kind = "smoke", m0 = 20, s0 = 6, m1 = 45, s1 = 8, xmax = 80, t = 32,
      height = 410,
      aria = "Dwa rozkłady odczytu czujki dymu: zwykłe dni z tostami i prysznicem oraz pożar. Przesuwany próg czułości zmienia odsetek fałszywych alarmów i przeoczonych pożarów"),
    name = "t", label = "Próg czułości czujki", min = 10, max = 60, value = 32, step = 1)
)

# --- 5D: żelki i trądzik (rozdz. 09, przed problemem porównań wielokrotnych) -----
# Kolory żelków to kolory materiału (SCENY.md: własne kolory dla materiałów).
p5_jelly_colors <- c("#c43d2f", "#2aa65e", "#2e6fd1", "#f0c419", "#e5832a", "#8b47ab", "#f78fb3",
                     "#8d5524", "#f5f0e6", "#2d2d2d", "#1abc9c", "#a3d65c", "#7b1e3a", "#e8d3a9",
                     "#97a3a4", "#98e0c5", "#fa8072", "#808000", "#1f2f6b", "#d4a017")
p5_jelly_names <- c("czerwone", "zielone", "niebieskie", "żółte", "pomarańczowe", "fioletowe",
                    "różowe", "brązowe", "białe", "czarne", "turkusowe", "limonkowe", "bordowe",
                    "beżowe", "szare", "miętowe", "łososiowe", "oliwkowe", "granatowe", "złote")

# Stała seria: jeden kolor (zielone) poniżej α, pozostałe p powyżej; badanie ogólne p = 0.58.
p5_jelly_p <- c(0.62, 0.031, 0.48, 0.91, 0.27, 0.74, 0.39, 0.083, 0.56, 0.19,
                0.85, 0.33, 0.67, 0.12, 0.95, 0.44, 0.71, 0.24, 0.58, 0.36)

p5_5D_ui <- p5_panel("5D", "scena", "Żelki i trądzik",
  p5_scene_widget("p5_jelly", "Badacz sprawdza, czy żelki wywołują trądzik",
    steps = c("Pytanie", "Kolor po kolorze", "Gazeta", "Cała seria"),
    config = list(kind = "jelly", colors = p5_jelly_colors, names = p5_jelly_names,
      p = p5_jelly_p, hit = 1L, p_all = 0.58, alpha = 0.05, height = 350,
      aria = "Badanie żelków: najpierw wszystkie razem bez związku, potem 20 kolorów po kolei z jednym alarmem przy zielonych, potem gazeta pokazuje tylko zielone żelki, na koniec cała seria: 1 alarm na 20 testów"))
)

# --- 5E: mandaty i świat bez związku (rozdz. 07, przed wzorami E i χ²) -----------
p5_5E_ui <- local({
  obs <- matrix(c(30, 70, 50, 50), nrow = 2, byrow = TRUE)
  ex <- outer(rowSums(obs), colSums(obs)) / sum(obs)
  p5_panel("5E", "scena", "Mandaty: jak by było, gdyby płeć nie miała znaczenia?",
    p5_scene_widget("p5_tickets", "200 kontroli drogowych, 100 kobiet i 100 mężczyzn",
      steps = c("Kontrole", "Świat bez związku", "Porównaj", "Jedna liczba"),
      config = list(kind = "tickets", obs = obs, exp = ex, rows = c("Kobiety", "Mężczyźni"),
        n_row = 100, height = 450,
        aria = "Sto kobiet i stu mężczyzn po kontroli drogowej, z zaznaczonymi mandatami; kolejne kadry pokazują liczby oczekiwane przy braku związku, różnice i ich wkład do statystyki chi-kwadrat")))
})

# --- teksty kroków ------------------------------------------------------------
p5_server <- function(input, output, session) {
  scene_texts(input, output, "p5_jelly", list(
    tagList("Badacz sprawdza, czy osoby jedzące żelki mają więcej trądziku. Wynik:
      brak związku. Może więc któryś kolor?"),
    tagList("Bada każdy z 20 kolorów osobno, testem przy α = 0.05. Lampka zapala się
      na czerwono, gdy p < 0.05. Przy zielonych żelkach się zapaliła."),
    tagList("Tak wynik widzi czytelnik gazety: jeden kolor, jedno badanie i p poniżej
      0.05. Wygląda przekonująco."),
    tagList("Cała seria: 19 kolorów bez alarmu i jeden z alarmem. Przy 20 testach
      i α = 0.05 przypadek daje średnio jeden fałszywy alarm, a tyle właśnie wyszło.")
  ))
  scene_texts(input, output, "p5_tickets", list(
    tagList("Policja skontrolowała 100 kobiet i 100 mężczyzn. Mandat dostało 30 kobiet
      i 50 mężczyzn."),
    tagList("Gdyby płeć nie miała znaczenia, w obu grupach mandat dostawałby ten sam
      odsetek kierowców, co w całej próbie. To liczebności oczekiwane."),
    tagList("Obrysy pokazują różnice: u kobiet brakuje 10 mandatów, u mężczyzn jest
      10 nadmiarowych. Tyle samo różnią się komórki bez mandatu."),
    tagList("Każdą różnicę podnosimy do kwadratu i dzielimy przez liczbę oczekiwaną.
      Suma czterech wkładów to statystyka χ².")
  ))
}
