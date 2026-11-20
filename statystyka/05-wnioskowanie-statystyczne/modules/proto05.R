# SCENY RYSOWANE W proto05.js: Ryc. 3.1, 7.1 i 9.1 ---------------------------------
#
# Powstały jako prototypy punktów stop 5A, 5E i 5D (2026-11-09) i są stałymi rycinami;
# nazwy plików proto05.* zostały z tamtego etapu. Rysunek i obliczenia: modules/proto05.js
# (+ proto05.css), bez serwera R. Historia decyzji: KOLEJKA-ZATWIERDZANIA.md.
#   ch1d_fig_smoke   Ryc. 3.1  Czujka dymu w akademiku            (kind "smoke")
#   ch5_fig_tickets  Ryc. 7.1  Mandaty: czy płeć ma znaczenie?    (kind "tickets")
#   ch7_fig_jelly    Ryc. 9.1  Żelki i trądzik                    (kind "jelly")
# Wymaga: lecture_layout.R (lc_step_widget, lc_toolbar, figure_panel), helpers.R.

.p5_body <- function(config) {
  tags$div(class = "p5-sc",
    `data-config` = jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA))
}

# Scena krokowa z przyciskiem czynności i opcjonalnie +10/+100/+1000.
p5_scene_widget <- function(id, title, steps, config, action = NULL, options = NULL,
                            more_from = 2, more = c("+10" = "m10", "+100" = "m100", "+1000" = "m1000"),
                         texts = NULL) {
  lc_step_widget(id,
    title = title,
    steps = steps,
    texts = texts,
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

# --- Ryc. 3.1 (dawny prototyp 5A, wdrożony 2026-11-19): czujka dymu, przed tabelą błędów ---
ch1d_fig_smoke <- figure_panel(label = "Ryc. 3.1", width_mode = "text", title = "Czujka dymu w akademiku",
  p5_static("p5_smoke",
    config = list(kind = "smoke", m0 = 20, s0 = 6, m1 = 45, s1 = 8, xmax = 80, t = 32,
      height = 460,
      aria = "Dwa rozkłady odczytu czujki dymu: zwykłe dni z tostami i prysznicem oraz pożar. Przesuwany próg czułości zmienia odsetek fałszywych alarmów i przeoczonych pożarów"),
    name = "t", label = "Próg czułości czujki", min = 10, max = 60, value = 32, step = 1)
)

# --- Ryc. 9.1 (dawny prototyp 5D, wdrożony 2026-11-20): żelki i trądzik -----------
# Kolory żelków to kolory materiału (SCENY.md: własne kolory dla materiałów).
p5_jelly_colors <- c("#c43d2f", "#2aa65e", "#2e6fd1", "#f0c419", "#e5832a", "#8b47ab", "#f78fb3",
                     "#8d5524", "#f5f0e6", "#2d2d2d", "#1abc9c", "#a3d65c", "#7b1e3a", "#e8d3a9",
                     "#97a3a4", "#98e0c5", "#fa8072", "#808000", "#1f2f6b", "#d4a017")
p5_jelly_names <- c("czerwone", "zielone", "niebieskie", "żółte", "pomarańczowe", "fioletowe",
                    "różowe", "brązowe", "białe", "czarne", "turkusowe", "limonkowe", "bordowe",
                    "beżowe", "szare", "miętowe", "łososiowe", "oliwkowe", "granatowe", "złote")

# Stałe dane: 100 osób niejedzących i 400 jedzących żelki (po 20 na ulubiony kolor),
# ta sama populacja N(12, 4) wyprysków. Pas „zakresu przypadku” = średnia niejedzących
# ± 1.96·s·√(1/100 + 1/n); alarm, gdy średnia podgrupy wypada poza pas (test z).
# Ziarno 833 dobrane tak, że poza pasem jest dokładnie jedna podgrupa: zielone, ze średnio
# większą liczbą wyprysków (+2.1, p = 0.031); jedzący razem nie różnią się od niejedzących (p = 0.61).
p5_jelly_data <- function(seed = 833, n_ctrl = 100, k = 20, m = 20, mu = 12, sd = 4) {
  set.seed(seed)
  ctrl <- pmax(0, round(rnorm(n_ctrl, mu, sd)))
  eat <- pmax(0, round(rnorm(k * m, mu, sd)))
  grp <- rep(seq_len(k), each = m)
  s <- sd(c(ctrl, eat))
  se_row <- s * sqrt(1 / n_ctrl + 1 / m)
  se_all <- s * sqrt(1 / n_ctrl + 1 / (k * m))
  rm <- as.numeric(tapply(eat, grp, mean))
  list(ctrl = ctrl, eat = eat, grp = grp, rm = rm,
       p = 2 * pnorm(-abs((rm - mean(ctrl)) / se_row)),
       p_all = 2 * pnorm(-abs((mean(eat) - mean(ctrl)) / se_all)),
       half_row = qnorm(0.975) * se_row, half_all = qnorm(0.975) * se_all,
       m_ctrl = mean(ctrl), m_eat = mean(eat))
}

ch7_fig_jelly <- local({
  d <- p5_jelly_data()
  hit <- which(d$p < 0.05)
  stopifnot(length(hit) == 1, d$rm[hit] > d$m_ctrl)
  figure_panel(label = "Ryc. 9.1", width_mode = "text", title = "Żelki i trądzik",
    p5_scene_widget("p5_jelly", "Badacz dzieli jedzących żelki według ulubionego koloru",
      steps = c("Pytanie", "Podział na kolory", "Gazeta", "Cała seria"),
      config = c(list(kind = "jelly", colors = p5_jelly_colors, names = p5_jelly_names,
        hit = hit, alpha = 0.05, height = 640), d,
        list(aria = "Wyniki 400 osób jedzących żelki i 100 niejedzących; jedzący zostają podzieleni na 20 podgrup według ulubionego koloru, średnie podgrup rozrzuca przypadek i jedna, zielona, wypada poza zakres przypadku; gazeta pokazuje tylko ją")),
      texts = list(
      tagList("Każda kropka to jedna osoba i jej liczba wyprysków. Średnia jedzących
      żelki leży w szarym pasie: tyle może się różnić od niejedzących przez sam
      przypadek. Związku nie ma."),
      tagList("Badacz dzieli tych samych 400 jedzących na 20 grup według ulubionego
      koloru. W grupie 20 osób średnia skacze bardziej, więc pas jest szerszy.
      Średnie rozrzucają się wokół wspólnej, a zielone wypadają poza pas:
      mają średnio więcej wyprysków."),
      tagList("Tak wynik widzi czytelnik gazety: jeden kolor, jedno badanie i p poniżej
      0.05. Wygląda przekonująco."),
      tagList("Wszystkie 20 grup pochodzi z tej samej chmury wyników. 19 średnich jest
      w pasie, jedna poza nim. Przy 20 podgrupach i α = 0.05 tyle właśnie daje
      przypadek.")
      )
    ),
    lc_caption("Na podstawie komiksu: Randall Munroe, „Significant”, xkcd nr 882 (",
      tags$a(href = "https://xkcd.com/882/", target = "_blank", rel = "noopener", "xkcd.com/882"),
      "), licencja CC BY-NC 2.5. Grafika i dane własne."))
})

# --- Ryc. 7.1 (dawny prototyp 5E, wdrożony 2026-11-20): mandaty, od pytania o niezależność do różnic ---
# 5E-2 (ten sam scenariusz w ludzikach z wykładu 02) odrzucony 2026-11-09: za dużo szczegółów.
p5_tickets_config <- local({
  obs <- matrix(c(30, 70, 50, 50), nrow = 2, byrow = TRUE)
  list(kind = "tickets", obs = obs, exp = outer(rowSums(obs), colSums(obs)) / sum(obs),
       rows = c("Kobiety", "Mężczyźni"), n_row = 100,
       aria = "Dwustu kierowców po kontroli drogowej, osiemdziesięciu z mandatem; kolejne kadry dzielą ich na sto kobiet i stu mężczyzn, pokazują liczby oczekiwane przy braku związku, liczby z archiwum i różnice między nimi")
})
p5_tickets_steps <- c("Kontrole", "Pytanie", "Bez wpływu", "Archiwum", "Porównaj")

# Kolejność kadrów od 2026-11-20: scena zaczyna od całej grupy i pytania o wpływ płci,
# liczebności oczekiwane stoją przed danymi z podziałem. Wcześniej dane były pierwsze,
# co utrudniało myślenie o niezależności jako o punkcie odniesienia. Scena jest czysto
# narracyjna: obliczenia (E, O − E, wkłady, χ²) pokazuje Ryc. 7.2 na tabeli krzyżowej.
ch5_fig_tickets <- figure_panel(label = "Ryc. 7.1", width_mode = "text", title = "Mandaty: czy płeć ma znaczenie?",
  p5_scene_widget("p5_tickets", "200 kontroli drogowych",
    steps = p5_tickets_steps, config = c(p5_tickets_config, list(height = 330)),
    texts = list(
    tagList("Policja skontrolowała 200 kierowców. 80 kontroli skończyło się mandatem."),
    tagList("Wśród kontrolowanych było 100 kobiet i 100 mężczyzn. Czy płeć ma wpływ na to,
    kto dostaje mandat?"),
    tagList("Gdyby płeć nie miała znaczenia, w obu grupach mandat dostawałby ten sam
    odsetek kierowców: 80 z 200, czyli 40 na 100. To liczebności oczekiwane."),
    tagList("Sprawdzamy w archiwum, kto naprawdę dostał mandat: 30 kobiet i 50 mężczyzn.
    Czy to nadal wygląda na niezależność?"),
    tagList("Obrysy pokazują różnice: u kobiet brakuje 10 mandatów, u mężczyzn jest
    10 nadmiarowych. Tyle samo różnią się komórki bez mandatu.")
    )
  ))
