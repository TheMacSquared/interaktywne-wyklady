# PANELE ROZDZIAŁU 01 „Logika testowania” (Ryc. 1.1 i 1.3) ----------------------
#
# Bez liczenia i bez własnego JS: kadry prowadzi lc_step_widget(), a ruch i odsłanianie
# robi CSS z atrybutu data-lc-step (klasy p5q-fK: element widoczny od kroku K).
# Style: modules/panels.css. Powstały jako prototypy 5S i 5T, przyjęte 2026-11-19.
#
#   ch1_fig_case   Ryc. 1.1  Eksperyment: telefon a koncentracja   kadry (case)
#   ch1_fig_court  Ryc. 1.3  Sąd i test statystyczny               kadry; zastąpił tabelę analogii

# --- Ryc. 1.1: eksperyment z telefonem w kadrach ---------------------------------------
# 80 osób w siatce 16 × 5; losowanie koloruje je grupami (stałe ziarno), potem każda
# osoba przesuwa się do bloku swojej grupy (8 × 5). Przesunięcie zapisane w zmiennych
# CSS --dx, --dy; kolory grup jak w odczytach Ryc. 1.2 (plecak: szałwia, biurko: bursztyn).
p5_case_svg <- function() {
  set.seed(105)
  grp <- rep("b", 80); grp[sample(80, 40)] <- "a"
  col <- (seq_len(80) - 1) %% 16; row <- (seq_len(80) - 1) %/% 16
  x1 <- 65 + col * 34; y1 <- 92 + row * 36
  slot <- ave(seq_len(80), grp, FUN = seq_along) - 1
  x3 <- ifelse(grp == "a", 62, 362) + (slot %% 8) * 31
  y3 <- 104 + (slot %/% 8) * 34
  people <- sprintf(paste0(
    '<g class="p5c-p is-%s" style="--dx:%.0fpx;--dy:%.0fpx"><circle cx="%.0f" cy="%.0f" r="5"/>',
    '<rect x="%.0f" y="%.0f" width="10" height="15" rx="4.5"/></g>'),
    grp, x3 - x1, y3 - y1, x1, y1, x1 - 5, y1 + 7)
  HTML(paste0(
    '<svg class="p5c-svg" viewBox="0 0 640 300" role="img" aria-label="Osiemdziesięciu studentów losowo dzielonych na dwie grupy po czterdzieści osób: telefon w plecaku i telefon na biurku; obie grupy piszą ten sam test koncentracji">',
    '<text class="p5c-all" x="320" y="50">80 studentów</text>',
    # Grupa A: plecak
    '<g class="p5q-f3"><g class="p5c-ico is-a" transform="translate(62 30)">',
    '<path d="M6 12 q0 -10 10 -10 q10 0 10 10"/><rect x="0" y="10" width="32" height="34" rx="8"/><rect x="8" y="24" width="16" height="12" rx="3"/></g>',
    '<text class="p5c-head is-a" x="106" y="50">Grupa A · telefon w plecaku</text>',
    '<text class="p5c-sub" x="106" y="70">40 osób</text></g>',
    # Grupa B: biurko
    '<g class="p5q-f3"><g class="p5c-ico is-b" transform="translate(362 30)">',
    '<rect x="6" y="18" width="20" height="10" rx="2.5"/><line x1="-2" y1="30" x2="34" y2="30"/><line x1="2" y1="30" x2="2" y2="44"/><line x1="30" y1="30" x2="30" y2="44"/></g>',
    '<text class="p5c-head is-b" x="406" y="50">Grupa B · telefon na biurku</text>',
    '<text class="p5c-sub" x="406" y="70">40 osób</text></g>',
    paste(people, collapse = ""),
    '<g class="p5q-f4"><text class="p5c-q" x="170" y="290">średni wynik testu: ?</text>',
    '<text class="p5c-q" x="470" y="290">średni wynik testu: ?</text></g>',
    "</svg>"))
}

ch1_fig_case <- figure_panel(label = "Ryc. 1.1", width_mode = "text", title = "Eksperyment: telefon a koncentracja",
  lc_step_widget("p5_case",
    steps = c("Studenci", "Losowanie", "Dwie grupy", "Test"),
    body = tags$div(class = "p5c", p5_case_svg()),
    texts = list(
      "Do eksperymentu zgłasza się 80 studentów.",
      "Każdego losujemy do jednej z dwóch grup. Los nie patrzy na oceny, sen ani motywację.",
      "Grupy różni tylko to, gdzie leży telefon. Na biurku jest wyciszony i odwrócony ekranem w dół, nikt go nie używa.",
      "Wszyscy piszą ten sam test koncentracji, od 0 do 100 punktów. Porównamy średnie wyniki obu grup."
    )
  )
)

# --- Ryc. 1.3: sąd i test (kadry zamiast tabeli analogii) -------------------------------
.p5t_row <- function(from, label, court, test, class = NULL) {
  tags$div(class = paste(c("p5t-row", paste0("p5q-f", from), class), collapse = " "),
    tags$span(class = "p5t-l", label),
    tags$div(class = "p5t-c", court),
    tags$div(class = "p5t-c", test))
}
.p5t_not <- function(...) tags$span(class = "p5t-not", tags$s(...))

ch1_fig_court <- figure_panel(label = "Ryc. 1.3", width_mode = "text", title = "Sąd i test statystyczny",
  lc_step_widget("p5_court",
    steps = c("Założenie", "Dowody", "Mocne dowody", "Słabe dowody"),
    body = tags$div(class = "p5t",
      tags$div(class = "p5t-row is-head",
        tags$span(class = "p5t-l"),
        tags$div(class = "p5t-h", "Sąd"),
        tags$div(class = "p5t-h", "Test: telefon a koncentracja")),
      tags$div(class = "p5t-row is-h0",
        tags$span(class = "p5t-l", "Na start zakładamy"),
        tags$div(class = "p5t-c", tags$b("Oskarżony jest niewinny"), tags$small("domniemanie niewinności")),
        tags$div(class = "p5t-c", tags$b("Telefon nie wpływa na koncentrację"), tags$small("hipoteza zerowa H₀"))),
      .p5t_row(2, "Co może to podważyć",
        tags$b("Dowody złożone w sądzie"), tags$b("Wyniki testu 80 studentów")),
      .p5t_row(3, "Gdy dowody są mocne",
        tags$b("Winny"), tagList(tags$b("Odrzucamy H₀"), tags$small("telefon wpływa na koncentrację")),
        class = "is-reject"),
      .p5t_row(4, "Gdy dowody są słabe",
        tagList(tags$b("Wina nie została wykazana"), .p5t_not("Niewinny")),
        tagList(tags$b("Brak podstaw do odrzucenia H₀"), .p5t_not("Telefon nie wpływa")),
        class = "is-keep")
    ),
    texts = list(
      "Sąd i test zaczynają tak samo: od założenia, że nic się nie stało.",
      "Założenie mogą podważyć tylko dowody. W teście dowodami są dane.",
      "Jeśli dowody wyraźnie przeczą założeniu, porzucamy je.",
      "Jeśli dowody są za słabe, założenie zostaje. Nie zostało jednak udowodnione."
    )
  )
)
