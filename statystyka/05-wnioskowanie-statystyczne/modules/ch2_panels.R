# PANELE ROZDZIAŁU 02 „Od pytania do hipotezy” (Ryc. 2.1–2.6) --------------------
#
# Panele przerywające tekst rozdziału. Bez liczenia i bez własnego JS: kroki,
# przełączniki i odsłanianie robią klocki layoutu (lc_step_widget, lc_segmented +
# lc_variants, lc_toggle_*), rysunki SVG składa R. Style: modules/panels_ch12.css.
# Powstały jako prototypy 5P, 5L, 5Q, 5M, 5R, 5O, przyjęte 2026-11-19.
#
#   ch2h_fig_ask     Ryc. 2.1  O co naprawdę pyta pytanie?        scena (trzy pytania z tekstu)
#   ch2h_fig_parse   Ryc. 2.2  Rozbiór pytania                    kadry
#   ch2h_fig_recipe  Ryc. 2.3  Przepis na parę hipotez            pojęcie, statyczny
#   ch2h_fig_axis    Ryc. 2.4  Dwie hipotezy dzielą oś parametru  galeria
#   ch2h_fig_side    Ryc. 2.5  Jedno słowo w pytaniu, inny test   galeria, bez α
#   ch2h_fig_errors  Ryc. 2.6  Popraw hipotezę                    galeria błędów
# Dawna Ryc. 2.4 (obszar odrzucenia, α) jest w rozdziale 03 jako Ryc. 3.5.

.p5_sub <- function(sym, idx) HTML(sprintf("%s<sub>%s</sub>", sym, idx))

# --- Ryc. 2.1: „wyżsi” znaczy „przeciętnie wyżsi” ------------------------------------
# Stałe wzrosty 10 kobiet i 10 mężczyzn (cm). Jedna kobieta jest wyższa od większości
# mężczyzn, jeden mężczyzna niższy od większości kobiet. Kolory płci jak w wykładzie 02.
p5_height_data <- list(
  K = c(158, 161, 163, 165, 166, 167, 169, 171, 174, 183),
  M = c(166, 172, 175, 177, 179, 180, 182, 184, 187, 191)
)

.p5_person <- function(x, h_cm, sex, base = 190, mark = FALSE) {
  hp <- (h_cm - 120) * 2.2
  top <- base - hp
  sprintf(paste0(
    '<g class="p5h-person is-%s%s"><circle cx="%.1f" cy="%.1f" r="8"/>',
    '<rect x="%.1f" y="%.1f" width="16" height="%.1f" rx="7"/></g>'),
    tolower(sex), if (mark) " is-mark" else "", x, top + 8, x - 8, top + 19, hp - 19)
}

p5_height_svg <- function(grouped) {
  d <- p5_height_data
  base <- 190
  y_of <- function(h) base - (h - 120) * 2.2
  parts <- character()
  if (!grouped) {
    # Sala: osoby wymieszane, stała kolejność.
    set.seed(52)
    all <- data.frame(h = c(d$K, d$M), sex = rep(c("K", "M"), each = 10))[sample(20), ]
    xs <- 46 + (seq_len(20) - 1) * 28.8
    parts <- c(parts, mapply(.p5_person, xs, all$h, all$sex, MoreArgs = list(base = base)))
  } else {
    xk <- 40 + (seq_len(10) - 1) * 26
    xm <- 366 + (seq_len(10) - 1) * 26
    mk <- mean(d$K); mm <- mean(d$M)
    parts <- c(parts,
      mapply(.p5_person, xk, d$K, "K", MoreArgs = list(base = base)),
      mapply(.p5_person, xm, d$M, "M", MoreArgs = list(base = base)),
      sprintf('<line class="p5h-mean is-k" x1="24" x2="292" y1="%.1f" y2="%.1f"/>', y_of(mk), y_of(mk)),
      sprintf('<line class="p5h-mean is-m" x1="350" x2="618" y1="%.1f" y2="%.1f"/>', y_of(mm), y_of(mm)),
      sprintf('<text class="p5h-lab is-k" x="24" y="%.1f">kobiety · średnio %.0f cm</text>', y_of(mk) - 44, mk),
      sprintf('<text class="p5h-lab is-m" x="350" y="%.1f">mężczyźni · średnio %.0f cm</text>', y_of(mm) - 34, mm),
      sprintf('<text class="p5h-n" x="%.1f" y="%.1f">%d</text>', xk[10], y_of(d$K[10]) - 7, d$K[10]),
      sprintf('<text class="p5h-n" x="%.1f" y="%.1f">%d</text>', xm[1], y_of(d$M[1]) - 7, d$M[1]))
  }
  HTML(paste0(
    '<svg class="p5h-svg" viewBox="0 0 640 214" role="img" aria-label="',
    if (grouped) "Dziesięć kobiet i dziesięciu mężczyzn ustawionych osobno według wzrostu, z liniami średnich; najwyższa kobieta jest wyższa od większości mężczyzn"
    else "Dwadzieścia osób różnego wzrostu, kobiety i mężczyźni wymieszani",
    '"><line class="p5h-floor" x1="16" x2="624" y1="', base, '" y2="', base, '"/>',
    paste(parts, collapse = ""), "</svg>"))
}

# Korepetycje: wyniki egzaminu (pkt) 10 osób z korepetycjami i 10 bez. Grupy się nakładają.
p5_tutor_data <- list(
  yes = c(52, 58, 61, 64, 66, 69, 72, 75, 79, 86),
  no  = c(44, 49, 53, 56, 58, 60, 62, 65, 70, 81)
)

p5_tutor_svg <- function(grouped) {
  d <- p5_tutor_data
  y_of <- function(v) 190 - (v - 35) * 2.8
  dot <- function(x, v, g) sprintf('<circle class="p5h-dot is-%s" cx="%.1f" cy="%.1f" r="9"/>', g, x, y_of(v))
  axis <- paste0('<line class="p5h-axis" x1="30" x2="30" y1="30" y2="190"/>',
    paste(sprintf('<text class="p5h-tick" x="24" y="%.1f">%d</text>', y_of(c(40, 60, 80)) + 4, c(40, 60, 80)), collapse = ""),
    '<text class="p5h-tick" x="24" y="22">pkt</text>')
  if (!grouped) {
    set.seed(7)
    all <- data.frame(v = c(d$yes, d$no), g = rep(c("yes", "no"), each = 10))[sample(20), ]
    parts <- mapply(dot, 62 + (seq_len(20) - 1) * 28, all$v, all$g)
  } else {
    xy <- 62 + (seq_len(10) - 1) * 25; xn <- 366 + (seq_len(10) - 1) * 25
    my <- mean(d$yes); mn <- mean(d$no)
    parts <- c(mapply(dot, xy, d$yes, "yes"), mapply(dot, xn, d$no, "no"),
      sprintf('<line class="p5h-mean is-yes" x1="46" x2="304" y1="%.1f" y2="%.1f"/>', y_of(my), y_of(my)),
      sprintf('<line class="p5h-mean is-no" x1="350" x2="608" y1="%.1f" y2="%.1f"/>', y_of(mn), y_of(mn)),
      sprintf('<text class="p5h-lab is-yes" x="46" y="%.1f">z korepetycjami · średnio %.0f pkt</text>', y_of(my) - 62, my),
      sprintf('<text class="p5h-lab is-no" x="350" y="%.1f">bez korepetycji · średnio %.0f pkt</text>', y_of(mn) - 76, mn),
      sprintf('<text class="p5h-n" x="%.1f" y="%.1f">%d</text>', xy[1], y_of(d$yes[1]) + 24, d$yes[1]),
      sprintf('<text class="p5h-n" x="%.1f" y="%.1f">%d</text>', xn[10], y_of(d$no[10]) - 14, d$no[10]))
  }
  HTML(paste0('<svg class="p5h-svg" viewBox="0 0 640 214" role="img" aria-label="',
    if (grouped) "Wyniki egzaminu osobno dla osób z korepetycjami i bez, z liniami średnich; grupy się nakładają"
    else "Wyniki egzaminu dwudziestu osób, z korepetycjami i bez, wymieszane",
    '">', axis, '<line class="p5h-floor" x1="30" x2="624" y1="190" y2="190"/>', paste(parts, collapse = ""), "</svg>"))
}

# Lody: 20 dni, temperatura (°C) i liczba sprzedanych porcji. Dzień ciepły: co najmniej 22°C.
p5_ice_data <- data.frame(
  temp  = c(12, 14, 15, 16, 17, 18, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31),
  sales = c(70, 95, 60, 110, 90, 130, 85, 120, 150, 105, 140, 170, 125, 180, 160, 210, 175, 230, 200, 240)
)

p5_ice_svg <- function(grouped) {
  d <- p5_ice_data
  d$g <- ifelse(d$temp >= 22, "warm", "cool")
  y_of <- function(v) 190 - v * 0.62
  if (!grouped) {
    set.seed(11)
    d <- d[sample(nrow(d)), ]
    xs <- 46 + (seq_len(20) - 1) * 28.8
    parts <- sprintf('<rect class="p5h-bar is-%s" x="%.1f" y="%.1f" width="16" height="%.1f" rx="4"/>',
                     d$g, xs - 8, y_of(d$sales), 190 - y_of(d$sales))
    floor_y <- 190; extra <- ""
  } else {
    x_of <- function(t) 60 + (t - 10) * 25
    fit <- coef(lm(sales ~ temp, d))
    ends <- c(11, 32)
    parts <- c(
      sprintf('<line class="p5h-mean is-trend" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>',
              x_of(ends[1]), y_of(fit[1] + fit[2] * ends[1]), x_of(ends[2]), y_of(fit[1] + fit[2] * ends[2])),
      sprintf('<circle class="p5h-dot is-%s" cx="%.1f" cy="%.1f" r="8"/>', d$g, x_of(d$temp), y_of(d$sales)),
      sprintf('<text class="p5h-tick is-x" x="%.1f" y="207">%d°C</text>', x_of(c(15, 20, 25, 30)), c(15L, 20L, 25L, 30L)),
      sprintf('<text class="p5h-lab is-trend" x="46" y="30">im cieplej, tym więcej · korelacja r = %.2f</text>', cor(d$temp, d$sales)))
  }
  HTML(paste0('<svg class="p5h-svg" viewBox="0 0 640 214" role="img" aria-label="',
    if (grouped) "Dni ułożone według temperatury: sprzedaż lodów rośnie z temperaturą, choć punkty są rozrzucone wokół linii"
    else "Sprzedaż lodów w dwudziestu kolejnych dniach, dni ciepłe i chłodne wymieszane",
    '"><line class="p5h-floor" x1="16" x2="624" y1="190" y2="190"/>', paste(parts, collapse = ""), "</svg>"))
}

# Jedno z trzech potocznych pytań z tekstu: obraz „surowy” i obraz po uporządkowaniu,
# na którym widać, o jaki parametr pytanie naprawdę pyta.
.p5h_btn <- function(key) paste0("p5_ask_", key)

# Przycisk-ikona w pasku (prawy górny róg): porządkuje obraz bieżącego pytania.
.p5h_toggle <- function(key, label) {
  htmltools::tagAppendAttributes(
    lc_action(.p5h_btn(key), icon = "eye", variant = "outline", aria_label = label),
    `data-lc-toggle-button` = "true", `aria-pressed` = "false")
}

.p5h_view <- function(key, question, off_svg, off_cap, on_svg, on_cap) {
  tags$div(class = "p5h",
    tags$p(class = "p5q-question", question),
    lc_toggle_content(.p5h_btn(key),
      off = tagList(off_svg, lc_caption(off_cap)),
      on = tagList(on_svg, lc_caption(on_cap))))
}

ch2h_fig_ask <- figure_panel(label = "Ryc. 2.1", width_mode = "text", title = "O co naprawdę pyta pytanie?",
  lc_toolbar(
    lc_segmented("p5_ask", "Pytanie",
      choices = c("Wzrost" = "height", "Korepetycje" = "tutor", "Lody" = "ice"), selected = "height"),
    # Każde pytanie ma własny przycisk i własny stan; widoczny jest przycisk bieżącego pytania.
    lc_variants("p5_ask", class = "lc-push", list(
      height = .p5h_toggle("height", "Ustaw osobno i pokaż średnie"),
      tutor  = .p5h_toggle("tutor", "Rozdziel grupy i pokaż średnie"),
      ice    = .p5h_toggle("ice", "Ułóż dni według temperatury")))),
  lc_variants("p5_ask", list(
    height = .p5h_view("height", "„Czy mężczyźni są wyżsi od kobiet?”",
      p5_height_svg(FALSE), "Dwadzieścia osób z sali. Niebieskie sylwetki to kobiety, bursztynowe to mężczyźni.",
      p5_height_svg(TRUE), "Nie każdy mężczyzna jest wyższy od każdej kobiety. Pytanie dotyczy średnich."),
    tutor = .p5h_view("tutor", "„Czy korepetycje pomagają?”",
      p5_tutor_svg(FALSE), "Wyniki egzaminu dwudziestu osób. Zielone kropki to osoby po korepetycjach.",
      p5_tutor_svg(TRUE), "„Pomagają” trzeba najpierw zmierzyć: tu wynikiem egzaminu. Pytanie dotyczy średnich, nie każdej osoby."),
    ice = .p5h_view("ice", "„Czy lodów sprzedaje się więcej w ciepłe dni?”",
      p5_ice_svg(FALSE), "Sprzedaż lodów w dwudziestu kolejnych dniach. Bursztynowe słupki to dni ciepłe.",
      p5_ice_svg(TRUE), "Nie każdy ciepły dzień bije każdy chłodny. Pytanie dotyczy związku temperatury ze sprzedażą.")
  ))
)

# --- Ryc. 2.2: rozbiór pytania (kadry) -----------------------------------------------
# Bez kadru z symbolami (decyzja 19.11: za wcześnie; zapis formalny jest w następnej sekcji).
# Jeden układ, kolejne kadry odsłaniają elementy (klasy p5q-f2 … p5q-f4 i atrybut
# data-lc-step widgetu krokowego; panels_ch12.css).
ch2h_fig_parse <- figure_panel(label = "Ryc. 2.2", width_mode = "text", title = "Rozbiór pytania: telefon a koncentracja",
  lc_step_widget("p5_parse",
    steps = c("Pytanie", "Parametr", "Relacja", "Hipotezy"),
    body = tags$div(class = "p5q",
      # Jeden ciąg HTML: htmltools wstawiłby odstęp przed „?”.
      tags$p(class = "p5q-question", HTML(paste0(
        "„Czy <span class=\"p5q-w is-par\">telefon na biurku</span> ",
        "<span class=\"p5q-w is-rel\">wpływa</span> na ",
        "<span class=\"p5q-w is-par\">koncentrację</span>?”"))),
      tags$div(class = "p5q-slots",
        tags$div(class = "p5q-slot is-par",
          tags$span(class = "p5q-slot-l", "1 · Co porównujemy?"),
          tags$span(class = "p5q-slot-v p5q-f2", "średnią koncentrację"),
          tags$span(class = "p5q-slot-s p5q-f2", "w dwóch warunkach: biurko i plecak")),
        tags$div(class = "p5q-slot is-rel",
          tags$span(class = "p5q-slot-l", "2 · Jaka relacja?"),
          tags$span(class = "p5q-slot-v p5q-f3", "różni się", tags$span(class = "p5q-sign", "≠")),
          tags$span(class = "p5q-slot-s p5q-f3", "pytanie nie wskazuje kierunku"))),
      tags$div(class = "p5q-hyps p5q-f4",
        tags$div(class = "p5q-hyp is-h0",
          tags$span(class = "p5q-hyp-l", "H₀", tags$small("stan domyślny")),
          tags$div(
            tags$p(class = "p5q-words",
              tags$span(class = "p5q-w is-par is-on", "średnia koncentracja „biurko”"), " ",
              tags$span(class = "p5q-w is-rel is-on", "jest równa"), " ",
              tags$span(class = "p5q-w is-par is-on", "średniej koncentracji „plecak”")))),
        tags$div(class = "p5q-hyp is-ha",
          tags$span(class = "p5q-hyp-l", "Hₐ", tags$small("to, co chcemy wykazać")),
          tags$div(
            tags$p(class = "p5q-words",
              tags$span(class = "p5q-w is-par is-on", "średnia koncentracja „biurko”"), " ",
              tags$span(class = "p5q-w is-rel is-on", "różni się od"), " ",
              tags$span(class = "p5q-w is-par is-on", "średniej koncentracji „plecak”")))))
    ),
    texts = list(
      "Pytanie zadane potocznie. Danymi nie da się go sprawdzić wprost.",
      "Koncentrację mierzy wynik testu, a porównujemy dwa warunki. Pytamy o średnią w populacji, osobno dla biurka i dla plecaka.",
      "„Wpływa” nie mówi, w którą stronę. Relacją jest więc „różni się”.",
      "H₀ opisuje stan domyślny, czyli brak różnicy. Hₐ jest jej dopełnieniem."
    )
  )
)

# --- Ryc. 2.4: oś parametru podzielona między H₀ i Hₐ ---------------------------
# kind: "two" (= i ≠), "gt" (≤ i >), "lt" (≥ i <). Punkt równości zawsze należy do H₀.
p5_axis_svg <- function(kind, ref = "wartość odniesienia",
                        left = "mniejsze wartości parametru", right = "większe wartości parametru") {
  x0 <- 40; x1 <- 600; xm <- 320; yb <- 62; gap <- 14
  seg <- function(cls, a, b) sprintf('<line class="p5x-bar %s" x1="%d" x2="%d" y1="%d" y2="%d"/>', cls, a, b, yb, yb)
  lab <- function(cls, x, txt) sprintf('<text class="p5x-lab %s" x="%d" y="34">%s</text>', cls, x, txt)
  dot <- sprintf('<circle class="p5x-dot" cx="%d" cy="%d" r="9"/>', xm, yb)
  body <- switch(kind,
    two = c(seg("is-ha", x0, xm - gap), seg("is-ha", xm + gap, x1), dot,
            lab("is-ha", (x0 + xm) %/% 2, "Hₐ: ≠"), lab("is-h0", xm, "H₀: ="), lab("is-ha", (xm + x1) %/% 2, "Hₐ: ≠")),
    gt  = c(seg("is-h0", x0, xm), seg("is-ha", xm + gap, x1), dot,
            lab("is-h0", (x0 + xm) %/% 2, "H₀: ≤"), lab("is-ha", (xm + x1) %/% 2, "Hₐ: >")),
    lt  = c(seg("is-ha", x0, xm - gap), seg("is-h0", xm, x1), dot,
            lab("is-ha", (x0 + xm) %/% 2, "Hₐ: <"), lab("is-h0", (xm + x1) %/% 2, "H₀: ≥")))
  aria <- switch(kind,
    two = "Oś parametru: H₀ to jeden punkt, wartość odniesienia; Hₐ zajmuje całą resztę osi po obu stronach",
    gt  = "Oś parametru: H₀ zajmuje lewą część osi razem z wartością odniesienia, Hₐ prawą",
    lt  = "Oś parametru: Hₐ zajmuje lewą część osi, H₀ prawą razem z wartością odniesienia")
  HTML(paste0(
    '<svg class="p5x-svg" viewBox="0 0 640 128" role="img" aria-label="', aria, '">',
    paste(body, collapse = ""),
    sprintf('<line class="p5x-axis" x1="%d" x2="%d" y1="92" y2="92"/>', x0 - 14, x1 + 14),
    sprintf('<path class="p5x-arrow" d="M%d 87 l-8 5 l8 5 M%d 87 l8 5 l-8 5"/>', x0 - 14, x1 + 14),
    sprintf('<line class="p5x-axis" x1="%d" x2="%d" y1="86" y2="98"/>', xm, xm),
    sprintf('<text class="p5x-ref" x="%d" y="118">%s</text>', xm, ref),
    sprintf('<text class="p5x-end" x="%d" y="118" text-anchor="start">%s</text>', x0 - 14, left),
    sprintf('<text class="p5x-end" x="%d" y="118" text-anchor="end">%s</text>', x1 + 14, right),
    "</svg>"))
}

.p5_axis_variant <- function(kind, question, when) {
  tags$div(class = "p5x",
    p5_axis_svg(kind),
    tags$p(class = "p5x-ex", tags$em(question), tags$span(when)))
}

ch2h_fig_axis <- figure_panel(label = "Ryc. 2.4", width_mode = "text", title = "Dwie hipotezy dzielą oś parametru",
  lc_toolbar(lc_segmented("p5_axis_pair", "Para znaków",
    choices = c("= i ≠" = "two", "≤ i >" = "gt", "≥ i <" = "lt"), selected = "two")),
  lc_variants("p5_axis_pair", list(
    two = .p5_axis_variant("two", "„Czy grupy się różnią?”", "pytanie bez kierunku"),
    gt  = .p5_axis_variant("gt", "„Czy nawóz daje wyższe plony?”", "pytanie tylko o wzrost"),
    lt  = .p5_axis_variant("lt", "„Czy nowa metoda skraca czas pracy?”", "pytanie tylko o spadek")
  )),
  lc_caption("Kropka, czyli równość, zawsze należy do H₀. Obie hipotezy razem pokrywają całą oś.")
)

# --- Ryc. 2.6: popraw hipotezę (galeria błędów) --------------------------------------
# Każdy zapis: pytanie, błędna para hipotez, a po kliknięciu zaznaczony błąd,
# diagnoza i poprawna para. bad() oznacza błędny fragment (widoczny po odsłonięciu).
.p5e_bad <- function(...) tags$span(class = "p5e-bad", ...)

.p5e_pair <- function(h0, ha, class = NULL) {
  tags$div(class = paste(c("p5e-pair", class), collapse = " "),
    tags$p(tags$b("H₀:"), " ", h0), tags$p(tags$b("Hₐ:"), " ", ha))
}

.p5e_card <- function(i, question, wrong, diag, fix, wrong_head = "Tak zapisano") {
  btn <- paste0("p5_fix_", i)
  card <- function(marked) tags$div(class = paste("p5e-card is-wrong", if (marked) "is-marked"),
    tags$span(class = "p5e-head", wrong_head), wrong)
  tags$div(class = "p5e",
    tags$p(class = "p5e-q", tags$em(question)),
    lc_toggle_content(btn,
      off = card(FALSE),
      on = tags$div(class = "p5e-cols",
        card(TRUE),
        tags$div(class = "p5e-card is-right", tags$span(class = "p5e-head", "Poprawnie"), fix),
        tags$p(class = "p5e-diag", tags$strong("Błąd:"), " ", diag))),
    tags$div(class = "p5e-actions", lc_toggle_button(btn, "Pokaż błąd i poprawkę", "Ukryj", variant = "outline")))
}

ch2h_fig_errors <- figure_panel(label = "Ryc. 2.6", width_mode = "text", title = "Popraw hipotezę",
  lc_step_widget("p5_errors",
    title = "Co jest nie tak z tym zapisem?",
    steps = paste("Zapis", 1:5),
    body = lc_step_views(list(
      .p5e_card(1, "„Czy średni wzrost kobiet i mężczyzn się różni?”",
        .p5e_pair(tagList(.p5_sub("μ", "K"), " ", .p5e_bad("≠"), " ", .p5_sub("μ", "M")),
                  tagList(.p5_sub("μ", "K"), " ", .p5e_bad("="), " ", .p5_sub("μ", "M"))),
        "H₀ nie zawiera równości. Tylko przy konkretnej wartości parametru wiadomo, jakich wyników oczekiwać, gdy H₀ jest prawdziwa.",
        .p5e_pair(tagList(.p5_sub("μ", "K"), " = ", .p5_sub("μ", "M")),
                  tagList(.p5_sub("μ", "K"), " ≠ ", .p5_sub("μ", "M")))),
      .p5e_card(2, "„Czy średni wzrost studentów wynosi 170 cm?”",
        .p5e_pair(tagList(.p5e_bad("średnia w próbie"), " = 170 cm"),
                  tagList(.p5e_bad("średnia w próbie"), " ≠ 170 cm")),
        "Hipoteza mówi o próbie. Średnią z próby znamy dokładnie, więc nie ma czego testować.",
        .p5e_pair("μ = 170 cm", "μ ≠ 170 cm")),
      .p5e_card(3, "„Czy nowa linia dobrze pakuje kawę do paczek po 500 g?”",
        .p5e_pair(tagList("linia pakuje ", .p5e_bad("dobrze")),
                  tagList("linia pakuje ", .p5e_bad("źle"))),
        "Brakuje parametru i wartości odniesienia. Takiej hipotezy nie da się sprawdzić danymi.",
        .p5e_pair("średnia masa paczki = 500 g", "średnia masa paczki ≠ 500 g")),
      .p5e_card(4, "„Czy nawóz (N) zmienia plon w porównaniu z kontrolą (K)?”",
        tags$div(class = "p5e-pair",
          tags$p(tags$small("przed badaniem"), tags$b("Hₐ:"), " ", .p5_sub("μ", "N"), " ≠ ", .p5_sub("μ", "K")),
          tags$p(tags$small("po obejrzeniu danych"), tags$b("Hₐ:"), " ", .p5_sub("μ", "N"), " ", .p5e_bad(">"), " ", .p5_sub("μ", "K"))),
        "Kierunek dopisano po zobaczeniu wyniku. Zostaje para ustalona przed analizą, inaczej ryzyko fałszywego alarmu jest większe od deklarowanego.",
        .p5e_pair(tagList(.p5_sub("μ", "N"), " = ", .p5_sub("μ", "K")),
                  tagList(.p5_sub("μ", "N"), " ≠ ", .p5_sub("μ", "K"))),
        wrong_head = "Tak zmieniono zapis"),
      .p5e_card(5, "„Producent chce wykazać, że suplement poprawia pamięć.”",
        .p5e_pair(tagList(.p5e_bad("suplement poprawia pamięć")),
                  tagList(.p5e_bad("suplement nie poprawia pamięci"))),
        "Role są zamienione. To, co chcemy wykazać, trafia do Hₐ; inaczej brak podstaw do odrzucenia H₀ wyglądałby jak potwierdzenie efektu.",
        .p5e_pair(tagList(.p5_sub("μ", "suplement"), " ≤ ", .p5_sub("μ", "placebo")),
                  tagList(.p5_sub("μ", "suplement"), " > ", .p5_sub("μ", "placebo"))))
    )),
    texts = as.list(rep("Najpierw wskaż błąd, potem odsłoń poprawkę.", 5))
  )
)

# --- Ryc. 2.3: przepis na parę hipotez (panel statyczny) ------------------------------
# Wyróżnia to, co w tekście jest rozproszone: kolejność budowy pary, równość zawsze
# w H₀, rozłączność i dopełnianie się hipotez.
.p5r_step <- function(n, head, ...) {
  tags$li(tags$span(class = "p5r-n", n),
    tags$div(tags$span(class = "p5r-step-h", head), tags$span(class = "p5r-step-t", ...)))
}
.p5r_pair <- function(question, ha, h0) {
  tags$div(class = "p5r-pair",
    tags$span(class = "p5r-pair-q", question),
    tags$span(class = "p5r-sign is-ha", ha),
    tags$span(class = "p5r-sign is-h0", h0))
}

ch2h_fig_recipe <- figure_panel(label = "Ryc. 2.3", width_mode = "text", title = "Przepis na parę hipotez",
  tags$div(class = "p5r",
    tags$ol(class = "p5r-steps",
      .p5r_step(1, "Nazwij parametr", "średnia, proporcja, różnica średnich, korelacja"),
      .p5r_step(2, "Zapisz Hₐ", "to, co chcesz wykazać: ≠, > albo <"),
      .p5r_step(3, "H₀ to cała reszta", "dlatego zawsze zawiera równość: =, ≤ albo ≥")),
    tags$div(class = "p5r-pairs", role = "group", `aria-label` = "Pary znaków w Hₐ i H₀",
      tags$div(class = "p5r-pair is-head",
        tags$span(class = "p5r-pair-q", "Pytanie"),
        tags$span(class = "p5r-rowlab is-ha", "Hₐ"),
        tags$span(class = "p5r-rowlab is-h0", "H₀")),
      .p5r_pair("czy się różni?", "≠", "="),
      .p5r_pair("czy jest większe?", ">", "≤"),
      .p5r_pair("czy jest mniejsze?", "<", "≥")),
    tags$div(class = "p5r-rules",
      tags$div(class = "p5r-rule",
        tags$strong("H₀ zawsze ze znakiem równości."),
        tags$span("Nigdy ≠, > ani <.")),
      tags$div(class = "p5r-rule",
        tags$strong("H₀ i Hₐ są rozłączne."),
        tags$span("Nie mogą być prawdziwe jednocześnie, a razem obejmują wszystkie możliwości.")))
  )
)

# --- Ryc. 2.5: jedno słowo w pytaniu, inny test (galeria, bez α i bez rozkładu) ---------
# Słowo w pytaniu wybiera parę hipotez (słownie) i typ testu.
# Oś wyniku z próby (różnica średnich plonów), pięć możliwych wyników badania i pas,
# w którym test zgłasza efekt. Bez liczb: granice pasów są umowne, dokładne wyznaczy
# rozdział 03. Pas testu jednostronnego zaczyna się bliżej środka niż dwustronnego.
p5_side_svg <- function(kind) {
  xs <- c(100, 195, 320, 445, 540); yd <- 74
  zone <- switch(kind, two = list(c(40, 150), c(490, 600)), gt = list(c(400, 600)), lt = list(c(40, 240)))
  sig <- switch(kind, two = c(TRUE, FALSE, FALSE, FALSE, TRUE),
                      gt = c(FALSE, FALSE, FALSE, TRUE, TRUE), lt = c(TRUE, TRUE, FALSE, FALSE, FALSE))
  aria <- switch(kind,
    two = "Test dwustronny: efekt zgłaszają tylko wyniki daleko od zera, po obu stronach osi",
    gt  = "Test prawostronny: efekt zgłaszają wyniki po prawej stronie, także bliższe zera; wyniki po lewej nie dają sygnału, nawet bardzo odległe",
    lt  = "Test lewostronny: efekt zgłaszają wyniki po lewej stronie, także bliższe zera; wyniki po prawej nie dają sygnału, nawet bardzo odległe")
  HTML(paste0(
    '<svg class="p5x-svg" viewBox="0 0 640 156" role="img" aria-label="', aria, '">',
    paste(vapply(zone, function(z) sprintf('<rect class="p5s-zone" x="%d" y="46" width="%d" height="56" rx="8"/>', z[1], z[2] - z[1]), ""), collapse = ""),
    paste(sprintf('<text class="p5s-lab %s" x="%d" y="%d">%s</text>', ifelse(sig, "is-sig", "is-none"), xs,
                  rep(c(24, 38), length.out = 5), ifelse(sig, "sygnał", "brak sygnału")), collapse = ""),
    paste(sprintf('<circle class="p5s-dot %s" cx="%d" cy="%d" r="10"/>', ifelse(sig, "is-sig", "is-none"), xs, yd), collapse = ""),
    '<line class="p5x-axis" x1="26" x2="614" y1="116" y2="116"/>',
    '<path class="p5x-arrow" d="M26 111 l-8 5 l8 5 M614 111 l8 5 l-8 5"/>',
    '<line class="p5x-axis" x1="320" x2="320" y1="110" y2="122"/>',
    '<text class="p5x-ref" x="320" y="142">bez różnicy</text>',
    '<text class="p5x-end" x="26" y="142" text-anchor="start">niższy plon z nawozem</text>',
    '<text class="p5x-end" x="614" y="142" text-anchor="end">wyższy plon z nawozem</text>',
    "</svg>"))
}

.p5_side_variant <- function(kind, word, test, h0, ha, note) {
  tags$div(class = "p5x",
    tags$p(class = "p5q-question", HTML(sprintf(
      "„Czy nawóz <span class=\"p5q-w is-rel is-on\">%s</span> plon?”", word))),
    tags$div(class = "p5w-hyps",
      tags$p(tags$b(class = "p5w-h0", "H₀:"), " średni plon z nawozem ", h0, " bez nawozu."),
      tags$p(tags$b(class = "p5w-ha", "Hₐ:"), " średni plon z nawozem ", ha, " bez nawozu.")),
    tags$p(class = "p5w-test", test),
    p5_side_svg(kind),
    tags$p(class = "p5x-ex", tags$span(note)))
}

ch2h_fig_side <- figure_panel(label = "Ryc. 2.5", width_mode = "text", title = "Jedno słowo w pytaniu, inny test",
  lc_toolbar(lc_segmented("p5_side", "Słowo w pytaniu",
    choices = c("zmienia" = "two", "zwiększa" = "gt", "zmniejsza" = "lt"), selected = "two")),
  lc_variants("p5_side", list(
    two = .p5_side_variant("two", "zmienia", "Test dwustronny",
      "jest taki sam jak", "różni się od plonu",
      "Sygnałem jest duża różnica w dowolną stronę."),
    gt  = .p5_side_variant("gt", "zwiększa", "Test prawostronny",
      "jest nie wyższy niż", "jest wyższy niż",
      "Sygnałem jest tylko wyższy plon. Nawet bardzo niski plon z nawozem nie daje sygnału."),
    lt  = .p5_side_variant("lt", "zmniejsza", "Test lewostronny",
      "jest nie niższy niż", "jest niższy niż",
      "Sygnałem jest tylko niższy plon. Nawet bardzo wysoki plon z nawozem nie daje sygnału.")
  )),
  lc_caption("Kropki to pięć możliwych wyników badania: różnica średnich plonów w próbie. Test jednostronny
    reaguje wcześniej w zapowiedzianą stronę, ale nie widzi przeciwnej.")
)
