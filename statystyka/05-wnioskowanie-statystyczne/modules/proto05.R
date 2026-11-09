# PROTOTYPY PUNKTÓW STOP 5A–5E (2026-11-09) ----------------------------------
#
# Każdy prototyp ma złotą plakietkę „Prototyp <nr> · <rodzaj>”, jak w wykładzie 04.
# Rysunek i obliczenia: modules/proto05.js (+ proto05.css), działają bez serwera R.
# Lista propozycji: PUNKTY-STOP-PROPOZYCJA.md, decyzje: KOLEJKA-ZATWIERDZANIA.md.
# Wstawki w tekście: p5("5A") … p5("5E"); teksty kroków: p5_server().
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

# --- 5B: planowanie powtórki badania z telefonem (rozdz. 03, przy Ryc. 3.1) -----
p5_5B_ui <- p5_panel("5B", "piaskownica", "Ile osób zaprosić do powtórki badania?",
  p5_static("p5_power",
    config = list(kind = "power", delta = 7, sd_a = 12, sd_b = 14, n = 20, height = 380,
      aria = "Rozkład różnicy średnich koncentracji, gdy telefon nie ma znaczenia i gdy różnica wynosi 7 punktów; suwak liczby osób w grupie zwęża oba rozkłady i zmienia moc testu"),
    name = "n", label = "Osób w każdej grupie", min = 5, max = 150, value = 20, step = 1)
)

# --- 5C: sen i ostatnia cyfra telefonu (rozdz. 06, przed testem korelacji) --------
p5_5C_ui <- p5_panel("5C", "scena", "Sen i ostatnia cyfra telefonu",
  p5_scene_widget("p5_corr", "Studentka pyta grupki znajomych o sen i numer telefonu",
    steps = c("Grupka", "Kolejne grupki", "Zakres"),
    action = "Zapytaj grupkę",
    options = list(list(name = "n", label = "Znajomych w grupce",
      values = c("10" = "10", "40" = "40", "200" = "200"), selected = "40")),
    config = list(kind = "corr", n = 40, height = 500,
      aria = "Wykres rozrzutu godzin snu i ostatniej cyfry numeru telefonu w grupce znajomych z wartością r, a pod nim stos wartości r z kolejnych grupek"))
)

# --- 5D: żelki i trądzik (rozdz. 09, przed problemem porównań wielokrotnych) -----
# Kolory żelków to kolory materiału (SCENY.md: własne kolory dla materiałów).
p5_jelly_colors <- c("#c43d2f", "#2aa65e", "#2e6fd1", "#f0c419", "#e5832a", "#8b47ab", "#f78fb3",
                     "#8d5524", "#f5f0e6", "#2d2d2d", "#1abc9c", "#a3d65c", "#7b1e3a", "#e8d3a9",
                     "#97a3a4", "#98e0c5", "#fa8072", "#808000", "#1f2f6b", "#d4a017")
p5_jelly_names <- c("czerwone", "zielone", "niebieskie", "żółte", "pomarańczowe", "fioletowe",
                    "różowe", "brązowe", "białe", "czarne", "turkusowe", "limonkowe", "bordowe",
                    "beżowe", "szare", "miętowe", "łososiowe", "oliwkowe", "granatowe", "złote")

p5_5D_ui <- p5_panel("5D", "scena", "Żelki i trądzik",
  p5_scene_widget("p5_jelly", "Badacz sprawdza, czy żelki któregoś koloru wywołują trądzik",
    steps = c("Seria kolorów", "Powtarzamy serie", "Ile to jest"),
    action = "Zbadaj 20 kolorów",
    config = list(kind = "jelly", colors = p5_jelly_colors, names = p5_jelly_names,
      alpha = 0.05, height = 410,
      aria = "Dwadzieścia żelków w różnych kolorach, pod każdym lampka testu przy alfa 0.05; licznik zbiera odsetek serii, w których zapaliła się co najmniej jedna lampka"))
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
  scene_texts(input, output, "p5_corr", list(
    tagList("Studentka pyta grupkę znajomych, ile śpią i jaka jest ostatnia cyfra ich
      numeru telefonu. Te dwie rzeczy nie mają ze sobą nic wspólnego, a r i tak nie wychodzi zero."),
    tagList("Każda kolejna grupka daje inne r, które odkłada się na stos. Zmień liczbę
      znajomych w grupce: stos zaczyna się od nowa."),
    tagList("Zaznaczony pas to środkowe 95% grupek. Tyle może wyjść samo z przypadku,
      gdy związku nie ma (ρ = 0). Przy 10 osobach pas jest szeroki, przy 200 wąski.")
  ))
  scene_texts(input, output, "p5_jelly", list(
    tagList("Ogólnie żelki nie mają związku z trądzikiem. Badacz sprawdza więc każdy
      kolor osobno, testem przy α = 0.05. Lampka zapala się, gdy p < 0.05."),
    tagList("Żaden kolor naprawdę nie działa. Powtarzaj całe serie po 20 kolorów
      i patrz, jak często choć jedna lampka się zapala."),
    tagList("Jeden test myli się w 5% przypadków. Seria 20 testów daje choć jeden
      fałszywy alarm w około 64% serii. Tytuł w gazecie pochodzi z tej serii.")
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
