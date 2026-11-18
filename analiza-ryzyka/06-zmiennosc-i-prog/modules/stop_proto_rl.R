# OŚ WYKŁADU „OD ZŁEGO RAPORTU” (przyjęta 17.11.2026) — grupa rl: P21 i domknięcie P16A
#
# Jeden szablon raportu z oceny ryzyka (nazwy wierszy jak w PN-ISO 31000 / ISO Guide 73:
# zakres i kryteria → poziom ryzyka → okres odniesienia → źródło ryzyka →
# założenia i niepewność → postępowanie z ryzykiem → monitorowanie i przegląd), w dwóch wersjach:
#   z6p_21_ui      — wersja zła na otwarcie rozdziału 1 (pole body rozdziału).
#   z6p_21_fix(k)  — notka „Poprawka do raportu” na końcu rozdziału k (pole widget):
#                    wiersze poprawiane w tym rozdziale, stare skreślone, nowe pod nimi.
#                    Rozdz. 1: poziom ryzyka; 3: okres odniesienia i źródło ryzyka;
#                    4: postępowanie z ryzykiem; 6: założenia i niepewność.
#   z6p_21_lina_ui — karta liny dźwigu w wersji złej na otwarcie rozdziału 5 (pole body).
#   z6p_21_lina    — ta sama karta na końcu rozdziału 5, przed i po (pole widget).
#   z6p_16a_ui     — domknięcie P16A (rozdz. 7): wersja poprawiona, z przełącznikiem na
#                    wersję z rozdziału 1; tu dochodzą wiersze „zakres i kryteria” oraz „monitorowanie i przegląd”.
# Wygląd: klasy z6p16a-* (stop_proto_rg.R). Liczby z wykładu: przykłady 6.1, 6.7, 6.9,
# 6.10, wzory (6.4), (6.5), (6.7). Statyczne, bez JS.

z6p21_rows <- list(
  list(key = "pytanie", lbl = "Zakres i kryteria", ch = 7,
    bad = "Czy łożyska wentylatorów są w normie?",
    good = "Jak często temperatura łożyska przekracza próg 85°C? Cel: najwyżej 10 przekroczeń na 1000 pomiarów.",
    note = "Pytanie bez kryterium nie prowadzi do decyzji: liczba przekroczeń nic nie mówi, dopóki nie wiadomo, ile to za dużo.",
    why = "pytanie bez kryterium", src = "rozdz. 7"),
  list(key = "wynik", lbl = "Poziom ryzyka", ch = 1,
    bad = "Średnia 82°C przy progu 85°C: w normie, ryzyko przekroczeń pomijalne.",
    good = "Średnia temperatura 82°C, odchylenie standardowe 3°C. Próg 85°C przekracza około 159 na 1000 pomiarów.",
    note = "Sama średnia i próg nie wystarczą do oceny ryzyka: bez rozrzutu nie da się policzyć, jak często wyniki przekraczają próg.",
    why = "brakowało rozrzutu", src = "przykład 6.1"),
  list(key = "horyzont", lbl = "Okres odniesienia", ch = 3,
    bad = "Ponad progiem leży 16% pomiarów, więc alarm pojawi się w około 16% zmian.",
    good = "Przy ośmiu pomiarach na zmianę alarm pojawia się w około 75% zmian (przy niezależnych pomiarach).",
    note = "Odsetek pomiarów to nie odsetek zmian: pytanie o zmianę dotyczy co najmniej jednego z ośmiu pomiarów. Przy skorelowanych pomiarach zmian z alarmem jest mniej.",
    why = "pomylono pomiar ze zmianą", src = "wzór (6.7)"),
  list(key = "mechanizm", lbl = "Źródło ryzyka", ch = 3,
    bad = "Przyczyna: pojedyncze, przypadkowe gorące odczyty.",
    good = "Średnia leży jedno odchylenie standardowe pod progiem; przekroczenia wynikają z położenia i rozrzutu temperatury.",
    note = "Gorące odczyty nie są przypadkiem: przy z = 1 mniej więcej co szósty pomiar musi wypaść ponad progiem.",
    why = "przypadek zamiast mechanizmu", src = "wzór (6.4)"),
  list(key = "zalozenia", lbl = "Założenia i niepewność", ch = 6,
    bad = "Model standardowy.",
    good = "Wykres kwantylowy pokazuje skośność prawostronną. Model skośny: przy progu 91°C około 12 przekroczeń na 1000 (model normalny: 1.3).",
    note = "„Model standardowy” to założenie, nie fakt. Kształt ogona trzeba sprawdzić, bo daleko w ogonie modele różnią się kilkukrotnie.",
    why = "„standard” bez sprawdzenia", src = "przykład 6.10"),
  list(key = "dzialanie", lbl = "Postępowanie z ryzykiem", ch = 4,
    bad = "Podnosimy próg do 87°C: przekroczeń będzie 48 na 1000 zamiast 159.",
    good = "Rekomendujemy chłodzenie o 2°C i ograniczenie σ do 2°C: przewidywana częstość około 6 na 1000. Próg bez zmian do czasu badania materiałowego.",
    note = "Podniesienie progu zmienia definicję przekroczenia, nie temperaturę łożyska. Rewizja progu wymaga dowodu konstrukcyjnego, nie liczby alarmów.",
    why = "zmiana definicji udawała poprawę", src = "przykład 6.7"),
  list(key = "kontrola", lbl = "Monitorowanie i przegląd", ch = 7,
    bad = NULL,
    good = "Po wdrożeniu: miesiąc pomiarów, powtórzony rachunek i wykres kwantylowy.",
    note = "Bez kontroli nie wiadomo, czy działanie przyniosło przewidywany skutek.",
    why = "nie sprawdzono skutku", src = "rozdz. 7")
)

z6p21_lina <- list(
  list(key = "wynik", lbl = "Poziom ryzyka",
    bad = "Ryzyko 50%. Zerwie się albo się nie zerwie.",
    good = "Ryzyko zerwania przy pojedynczym podniesieniu: P(L > S) ≈ 0.173, czyli około 173 na 1000.",
    note = "Dwa możliwe wyniki to nie dwie równe szanse. Ryzyko to P(L > S), liczone z rozkładów obciążenia i wytrzymałości przez margines D = S − L.",
    why = "dwa wyniki wzięto za 50 na 50", src = "przykład 6.8"),
  list(key = "zalozenia", lbl = "Założenia i niepewność",
    bad = NULL,
    good = "Obciążenie N(85, 8) kN, wytrzymałość liny N(95, 7) kN, zmienne niezależne.",
    note = "Bez modelu obu zmiennych nie da się policzyć ryzyka zerwania.",
    why = "brak modelu", src = "wzór (6.9)"),
  list(key = "dzialanie", lbl = "Postępowanie z ryzykiem",
    bad = "Wymieniamy wszystkie liny.",
    good = "Rekomendujemy linę o średniej wytrzymałości około 118 kN albo ważenie ładunków.",
    note = "Wymiana na taką samą linę nie zmienia rozkładu wytrzymałości, więc nie zmienia ryzyka.",
    why = "działanie bez rachunku", src = "przykład 6.9")
)

z6p21_css <- tags$style(HTML("
  .z6p21-bad { border-top-color: var(--upwr-accent); }
  .z6p21-none { color: var(--upwr-ink-subtle); font-style: italic; }
  .z6p21-sign { margin: .5em 0 0; font-size: .9em; font-style: italic; color: var(--upwr-ink-subtle); }
  .z6p21-old { color: var(--upwr-ink-subtle); text-decoration: line-through; text-decoration-color: var(--upwr-accent); }
  .z6p21-new { display: block; margin-top: .3em; border-left: 4px solid var(--upwr-sage); padding-left: .6em; }
  .z6p21-rowfix { grid-template-columns: 1.6em 12em minmax(0, 1fr); }
  .z6p21-lblfix { display: flex; flex-direction: column; gap: .35em; align-items: flex-start; }
  @media (max-width: 720px) { .z6p21-rowfix { grid-template-columns: 1.6em minmax(0, 1fr); } }
  .z6p21-notes { margin-top: .7em; padding-top: .5em; border-top: 1px dashed var(--upwr-rule); font-size: .88em; color: var(--upwr-ink-subtle); }
  .z6p21-notes p { margin: .25em 0; }
  .z6p21-mark { font-family: var(--upwr-mono); font-size: .9em; font-weight: 700; color: var(--upwr-ink); }
  .z6p21-why { white-space: normal !important; font-family: var(--upwr-mono); font-size: .72em; color: var(--upwr-accent); white-space: nowrap;
    border: 1px solid color-mix(in srgb, var(--upwr-accent) 40%, transparent); border-radius: 999px; padding: .15em .6em;
    justify-self: start; margin-top: .15em; }
"))

# Karta raportu. version: "bad" (zła), "good" (poprawiona), "fix" (stare skreślone + nowe).
z6p21_card <- function(rows, version, head, sign = NULL, nums = seq_along(rows)) {
  tags$div(class = paste("z6p16a-card z6p16a-after", if (version == "bad") "z6p21-bad"),
    tags$div(class = "z6p16a-head", head),
    tags$ol(class = "z6p16a-rows",
      lapply(seq_along(rows), function(i) {
        r <- rows[[i]]
        text <- switch(version,
          bad = if (is.null(r$bad)) tags$span(class = "z6p16a-text z6p21-none", "brak")
                else tags$span(class = "z6p16a-text", r$bad),
          good = tags$span(class = "z6p16a-text", risk_subscripts(r$good)),
          fix = tags$span(class = "z6p16a-text",
            if (is.null(r$bad)) tags$span(class = "z6p21-none", "brak") else tags$span(class = "z6p21-old", r$bad),
            tags$span(class = "z6p21-new", risk_subscripts(r$good))))
        # W poprawce diagnoza stoi pod nazwą wiersza, żeby tekst miał pełną szerokość.
        pill <- if (version == "good") tags$span(class = "z6p16a-src", r$src)
        lbl <- if (version == "fix") tags$span(class = "z6p16a-lbl z6p21-lblfix", tags$strong(r$lbl), tags$span(class = "z6p21-why", r$why))
               else tags$strong(class = "z6p16a-lbl", r$lbl)
        tags$li(class = paste("z6p16a-row", if (version == "fix") "z6p21-rowfix"),
          tags$span(class = "z6p16a-num", nums[[i]]),
          lbl, text, pill)
      })),
    if (!is.null(sign)) tags$p(class = "z6p21-sign", sign),
    # Komentarz dydaktyczny osobno od zdań raportu: przypisy pod kartą (tylko w poprawce).
    if (version == "fix") tags$div(class = "z6p21-notes",
      lapply(seq_along(rows), function(i) tags$p(
        tags$span(class = "z6p21-mark", paste0(nums[[i]], " · ", rows[[i]]$lbl)), " ", risk_subscripts(rows[[i]]$note)))))
}

z6p21_head <- "Raport z oceny ryzyka · wentylator 2 · maj"

# ---- Otwarcie (rozdz. 1) --------------------------------------------------------
z6p_21_ui <- z6p_panel("P21", "scena", "Raport, który przejdzie przez cały wykład", proto = FALSE, z6p_css, z6p16a_css, z6p21_css,
  tags$div(class = "z6p16a",
    z6p21_card(z6p21_rows, "bad", z6p21_head, sign = "Wniosek działu: sytuacja pod kontrolą, bez inwestycji.")),
  lc_caption("Każdy wiersz brzmi rozsądnie i każdy jest błędny albo pusty. Kolejne rozdziały poprawiają je po kolei; poprawiony raport składamy w rozdziale 7."))

# ---- Poprawka na końcu rozdziału --------------------------------------------------
z6p_21_fix <- function(ch) {
  idx <- which(vapply(z6p21_rows, function(r) r$ch == ch, logical(1)))
  z6p_panel(paste0("P21-", ch), "poprawka", "Poprawka do raportu", width_mode = "text", proto = FALSE,
    z6p_css, z6p16a_css, z6p21_css,
    tags$div(class = "z6p16a",
      z6p21_card(z6p21_rows[idx], "fix", nums = idx,
        paste0(z6p21_head, " · ", if (length(idx) > 1) "wiersze " else "wiersz ",
          paste(tolower(vapply(z6p21_rows[idx], `[[`, "", "lbl")), collapse = " i ")))))
}

# ---- Lina dźwigu (rozdz. 5) -------------------------------------------------------
z6p21_lina_head <- "Raport z oceny ryzyka · lina dźwigu w magazynie"

z6p_21_lina_ui <- z6p_panel("P21-5a", "scena", "Raport o linie dźwigu", width_mode = "text", proto = FALSE,
  z6p_css, z6p16a_css, z6p21_css,
  tags$div(class = "z6p16a", z6p21_card(z6p21_lina, "bad", z6p21_lina_head)),
  lc_caption("Ten sam szablon co raport o łożysku i te same pułapki: liczba, której nie da się obronić, brak modelu i działanie bez rachunku. Poprawiamy go na końcu rozdziału."))

z6p_21_lina <- z6p_panel("P21-5", "poprawka", "Raport o linie dźwigu, przed i po", width_mode = "text", proto = FALSE,
  z6p_css, z6p16a_css, z6p21_css,
  tags$div(class = "z6p16a", z6p21_card(z6p21_lina, "fix", z6p21_lina_head)))

# ---- Domknięcie (rozdz. 7): wersja poprawiona z przełącznikiem ----------------------
z6p_16a_ui <- z6p_panel("P16A", "domknięcie", "Raport, wersja poprawiona", proto = FALSE, z6p_css, z6p16a_css, z6p21_css,
  lc_toggle_button("z6p16a_toggle", "Pokaż raport z rozdziału 1", "Pokaż raport poprawiony", variant = "outline"),
  lc_toggle_content("z6p16a_toggle",
    off = tags$div(class = "z6p16a",
      tags$div(class = "z6p16a-main",
        z6p21_card(z6p21_rows, "good", z6p21_head,
          sign = "Wniosek: cel nie jest spełniony; rekomendujemy chłodzenie i stabilizację, progu nie zmieniamy."),
        tags$figure(class = "z6p16a-fig", z6p16a_svg()))),
    on = tags$div(class = "z6p16a",
      z6p21_card(z6p21_rows, "bad", z6p21_head, sign = "Wniosek działu: sytuacja pod kontrolą, bez inwestycji."))))
