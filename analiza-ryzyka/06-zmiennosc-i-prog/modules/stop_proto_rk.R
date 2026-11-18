# WIDGETY (2026-11-17, przyjęte) — grupa rk: P20A–P20C, rozdział 7 „Komunikat o ryzyku”
#
#   z6p_20a_ui — piaskownica „Redaktor komunikatu” (P20A): suwaki μ, σ, próg, liczba
#                pomiarów na zmianę, kształt rozkładu i działanie; trzy zdania komunikatu
#                i ostrzeżenia składa JS (modules/proto_rk.js, scena „redaktor”).
#   z6p_20b_ui — galeria „Popraw raport” (P20B): pięć raportów, w każdym jedno błędne
#                zdanie (pułapki z rozdziałów 1–6); scena „raporty”.
#   z6p_20c_ui — „Decyzja z budżetem” (P20C): pakiet działań z kosztem, cel ≤ 10
#                przekroczeń na 1000 przy budżecie 20 tys. zł; scena „budzet”.
# Koszty i efekty działań są ilustracyjne (jak liczby Bananpolu). σ łączonych działań:
# w tym modelu bierzemy najsilniejsze ograniczenie, nie iloczyn.
# Wymaga: stop_proto_common.R (z6p_scene_ui, z6p_out, z6p_button, z6p_seg),
# R/lc_stat_figures.R (lc_sf_slider).

z6p20_css <- tags$style(HTML("
  .z6p20-msg { list-style: none; padding: 0; margin: .4em 0; display: grid; gap: .35em; }
  .z6p20-row { display: grid; grid-template-columns: 12em minmax(0, 1fr); gap: .2em 1em; align-items: baseline;
    border-left: 4px solid var(--upwr-ink); background: var(--upwr-surface); border-radius: 4px; padding: .45em .8em; }
  .z6p20-row.is-warn { border-left-color: var(--upwr-accent); }
  .z6p20-k { font-weight: 700; }
  .z6p20-warn { margin: .3em 0 .2em; padding-left: 1.2em; color: var(--upwr-accent); }
  .z6p20-report { border: 1px solid var(--upwr-rule); border-radius: 6px; background: var(--upwr-surface); padding: .6em .9em; }
  .z6p20-rhead { font-family: var(--upwr-mono); font-size: .75em; letter-spacing: .06em; text-transform: uppercase;
    color: var(--upwr-ink-subtle); border-bottom: 1px solid var(--upwr-rule); padding-bottom: .35em; margin-bottom: .4em; }
  .z6p20-sents { margin: 0; padding-left: 1.4em; display: grid; gap: .25em; }
  .z6p20-sent { all: unset; cursor: pointer; display: block; padding: .3em .5em; border-radius: 4px; line-height: 1.45; }
  .z6p20-sent:hover, .z6p20-sent:focus-visible { background: var(--upwr-surface-sunken); outline: 1px dashed var(--upwr-rule); }
  .z6p20-sent.is-fine { background: color-mix(in srgb, var(--upwr-sage) 18%, transparent); }
  .z6p20-sent.is-bad { background: color-mix(in srgb, var(--upwr-accent) 14%, transparent); text-decoration: line-through;
    text-decoration-color: var(--upwr-accent); }
  .z6p20-sent:disabled { cursor: default; }
  .z6p20-diag { border-left: 4px solid var(--upwr-accent); padding: .3em .8em; margin: .5em 0; }
  .z6p20-diag p { margin: .2em 0; }
  .z6p20-status { font-weight: 700; margin: .2em 0; }
  .z6p20-status.is-ok { color: var(--upwr-sage); }
  .z6p20-status.is-bad { color: var(--upwr-accent); }
  .z6p20-joke { font-style: italic; border-left: 4px solid var(--upwr-accent); padding: .25em .8em; margin: .4em 0; }
  .z6p20-acts { display: flex; flex-wrap: wrap; gap: .4em; }
  .z6p20-acts .lc-action[aria-pressed='true'] { background: var(--upwr-ink); color: var(--upwr-surface); border-color: var(--upwr-ink); }
  .z6p20-cost { font-family: var(--upwr-mono); font-size: .85em; opacity: .8; }
  @media (max-width: 640px) { .z6p20-row { grid-template-columns: minmax(0, 1fr); } }
"))

# ============================================================================
# P20A — PIASKOWNICA „REDAKTOR KOMUNIKATU”
# ============================================================================

z6p20a_cfg <- list(mu = z6p_mu, sd = z6p_sd, c = z6p_c, n = 8)

z6p_20a_ui <- tagList(z6p20_css,
  z6p_scene_ui("P20A", "piaskownica", "Redaktor komunikatu", "redaktor", z6p20a_cfg, proto = FALSE,
    tags$div(class = "z6p-nav",
      lc_sf_slider("z6p20a_mu", "Średnia μ", 76, 90, z6p_mu, 0.5, suffix = "°C"),
      lc_sf_slider("z6p20a_sd", "Odchylenie σ", 1, 6, z6p_sd, 0.25, suffix = "°C"),
      lc_sf_slider("z6p20a_c", "Próg c", 78, 95, z6p_c, 0.5, suffix = "°C"),
      lc_sf_slider("z6p20a_n", "Pomiarów na zmianę", 1, 24, 8, 1)),
    tags$div(class = "z6p-nav",
      z6p_seg("shape", "Kształt rozkładu", c("normalny" = "normal", "skośny" = "skew")),
      z6p_seg("act", "Działanie", c("brak" = "none", "chłodzenie −2°C" = "cool",
        "stabilizacja σ −1°C" = "stab", "próg +2°C" = "thr")),
      z6p_button("base", icon = "reset", variant = "ghost", aria_label = "Dane łożyska Bananpolu")),
    z6p_out("stage"),
    z6p_out("body"),
    tags$p(class = "z6p-caption",
      "Przesuń suwaki tak, żeby opisać inne łożysko, i sprawdź, jak zmieniają się trzy zdania. Ostrzeżenia pod komunikatem przypominają pułapki z wykładu.")))

# ============================================================================
# P20B — GALERIA „POPRAW RAPORT”
# ============================================================================

# Inne dane niż raport P21 (17.11.2026): te same pięć błędów w nowych sytuacjach,
# żeby galeria była sprawdzianem, a nie powtórką. Liczby: N(56, 2) i próg 58 bar;
# 1 − 0.95¹⁶ ≈ 0.56; dach N(1.2, 0.4) i N(2.5, 0.25): pole wspólne 0.044, P(L > S) 0.0029;
# dojazd jak w scenie P19: N(30, 6) daje 6, skośny 21 + Gamma(2.25, 4) daje 25 na 1000.
z6p20b_cfg <- list(reports = list(
  list(head = "Raport tygodniowy · ciśnienie w instalacji chłodniczej",
    sentences = c("Średnie ciśnienie w tym tygodniu: 56 bar.",
                  "Próg ostrzegawczy zaworu bezpieczeństwa: 58 bar.",
                  "Zapas 2 bar: instalacja w normie, ryzyko przekroczeń pomijalne."),
    wrong = 2L,
    hint = "Szukaj zdania, które wyciąga wniosek z samych dwóch liczb.",
    diag = "Średnia i próg nie wystarczą; bez rozrzutu nie wiadomo, jak często odczyty przekraczają próg.",
    fix = "Przy σ = 2 bar próg leży jedno odchylenie nad średnią: około 159 na 1000 odczytów przekracza 58 bar."),
  list(head = "Raport utrzymania ruchu · czujnik drgań wentylatora",
    sentences = c("Czujnik zapisuje drgania co 30 minut, 16 razy na zmianę.",
                  "Ponad progiem alarmowym leży 5% odczytów.",
                  "Zatem alarm drgań pojawi się w około 5% zmian."),
    wrong = 2L,
    hint = "Które zdanie zmienia jednostkę obserwacji?",
    diag = "Odsetek odczytów to nie odsetek zmian; zmiana ma alarm, gdy przekroczy choć jeden z 16 odczytów.",
    fix = "Przy niezależnych odczytach alarm pojawi się w około 1 − 0.95¹⁶ ≈ 56% zmian; przy skorelowanych rzadziej."),
  list(head = "Raport magazynu · regały w chłodni",
    sentences = c("Dopuszczalne obciążenie miejsca paletowego podnieśliśmy w instrukcji z 1000 do 1100 kg.",
                  "Dzięki temu „za ciężkich” palet jest trzy razy mniej, więc regały są bezpieczniejsze.",
                  "Zmiana weszła w życie od poniedziałku."),
    wrong = 1L,
    hint = "Co fizycznie zmieniło się w regale?",
    diag = "Zmieniono definicję „za ciężkiej” palety, nie nośność regału: ubyło przekroczeń w raporcie, nie w magazynie.",
    fix = "Nowy limit jest uprawniony tylko po ocenie nośności regału; bez niej ryzyko przeciążenia zostaje takie jak przedtem."),
  list(head = "Ocena dachu magazynu przed zimą",
    sentences = c("Obciążenie śniegiem w najgorszym dniu zimy: N(1.2, 0.4) kN/m²; nośność dachu: N(2.5, 0.25) kN/m².",
                  "Krzywe obu rozkładów nakładają się na polu 0.044, więc ryzyko zawalenia wynosi około 4% na zimę.",
                  "Rekomendujemy przegląd konstrukcji przed sezonem."),
    wrong = 1L,
    hint = "Które zdanie liczy ryzyko z rysunku zamiast z par L i S?",
    diag = "Pole nakładania gęstości nie jest prawdopodobieństwem awarii; tu zawyża ryzyko piętnastokrotnie.",
    fix = "Ryzyko to P(L > S) = Φ(−2.76) ≈ 0.003, czyli około 3 zimy na 1000."),
  list(head = "Ocena dostaw · czas dojazdu dostawcy",
    sentences = c("Czas dojazdu dostawcy: średnio 30 min, odchylenie standardowe 6 min, 500 dostaw.",
                  "Model standardowy: dojazd dłuższy niż 45 min zdarza się 6 razy na 1000 dostaw.",
                  "Histogram ma ostrą lewą krawędź przy 21 min i długi prawy ogon."),
    wrong = 1L,
    hint = "Co mówi kształt histogramu o prawym ogonie?",
    diag = "„Model standardowy” przyjęto bez sprawdzenia; histogram i wykres kwantylowy pokazują skośność prawostronną.",
    fix = "Model skośny (jak w scenie z dwiema trasami) daje około 25 spóźnień na 1000 dostaw, cztery razy więcej.")
))

z6p_20b_ui <- tagList(z6p20_css,
  z6p_scene_ui("P20B", "galeria", "Popraw raport", "raporty", z6p20b_cfg, proto = FALSE,
    tags$div(class = "z6p-head", tags$span(class = "z6p-name", "Znajdź błędne zdanie"), z6p_out("step", class = "z6p-step")),
    z6p_out("stage"),
    z6p_out("body"),
    tags$div(class = "z6p-nav",
      z6p_button("prevr", "Poprzedni", icon = "prev"),
      z6p_button("nextr", "Następny", variant = "solid"))))

# ============================================================================
# P20C — DECYZJA Z BUDŻETEM
# ============================================================================

z6p20c_cfg <- list(mu = z6p_mu, sd = z6p_sd, c = z6p_c, budget = 20, target = 10,
  actions = list(
    list(key = "fan", label = "Wentylator obudowy", short = "wentylator obudowy", cost = 12, dmu = -2),
    list(key = "lube", label = "Smar o mniejszym tarciu", short = "smar", cost = 2, dmu = -1),
    list(key = "vfd", label = "Falownik", short = "falownik", cost = 8, sd = 2),
    list(key = "load", label = "Rozłożony załadunek", short = "rozłożony załadunek", cost = 1, sd = 2.5),
    list(key = "auto", label = "Automatyczny smarownik", short = "automatyczny smarownik", cost = 5, dmu = -0.5, sd = 2.5),
    list(key = "thr_ok", label = "Próg 87°C po badaniu materiałowym", short = "próg 87°C po badaniu", cost = 6, dc = 2, proof = TRUE, group = "thr"),
    list(key = "thr_no", label = "Próg 87°C bez badania", short = "próg 87°C bez badania", cost = 0, dc = 2, proof = FALSE, group = "thr")
  ))

z6p20c_button <- function(a) {
  b <- tags$button(type = "button", class = "lc-action is-outline", `data-z6p` = "toggle", `data-value` = a$key,
    `aria-pressed` = "false",
    tags$span(a$label), tags$span(class = "z6p20-cost", sprintf(" %d tys.", a$cost)))
  if (!is.null(a$group)) b <- htmltools::tagAppendAttributes(b, `data-group` = a$group)
  b
}

z6p_20c_ui <- tagList(z6p20_css,
  z6p_scene_ui("P20C", "piaskownica", "Decyzja z budżetem", "budzet", z6p20c_cfg, proto = FALSE,
    tags$p(class = "z6p-caption",
      "Masz 20 tys. zł. Cel: najwyżej 10 przekroczeń 85°C na 1000 pomiarów. Wybierz pakiet działań."),
    tags$div(class = "z6p20-acts", lapply(z6p20c_cfg$actions, z6p20c_button),
      z6p_button("clear", icon = "reset", variant = "ghost", aria_label = "Wyczyść pakiet")),
    z6p_out("stage"),
    z6p_out("body")))
