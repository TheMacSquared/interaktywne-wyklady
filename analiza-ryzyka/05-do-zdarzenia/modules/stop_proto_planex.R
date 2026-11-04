# PUNKTY STOP (2026-11-04) — warianty przykładu „plan audytu” w sekcji „Ile kontroli zaplanować?”
#
# Karta planu kontroli dostawy bananów (pająk w kiści, r = 3, p = 0.10): forma dokumentu do podpisu
# (dawny wariant B) i źródło każdej rubryki (kolumna „skąd” z wariantu A).
# Warianty C (dwa plany) i D (zadanie z listy 5) odrzucone przy przeglądzie.
#   d5p_planex_ui
# Bez serwera. Wymaga: stop_proto_common.R (d5p_panel, d5p_css).

d5p_planex_css <- tags$style(HTML("
  .d5x-table { width: 100%; border-collapse: collapse; margin: .3em 0 .6em; font-size: .95em; }
  .d5x-table th, .d5x-table td { text-align: left; vertical-align: top; padding: .45em .6em; border-bottom: 1px solid var(--upwr-rule); }
  .d5x-table th { font-family: var(--upwr-mono); font-size: .75em; letter-spacing: .06em; text-transform: uppercase; color: var(--upwr-ink-subtle); font-weight: 700; }
  .d5x-table td:first-child { font-weight: 700; color: var(--upwr-ink); width: 30%; }
  .d5x-table td:last-child { color: var(--upwr-ink-subtle); font-size: .9em; width: 22%; }
  .d5x-form { border: 1px solid var(--upwr-ink-subtle); border-radius: 4px; padding: 1em 1.2em; background: var(--upwr-surface); }
  .d5x-form-h { display: flex; justify-content: space-between; gap: 1em; border-bottom: 2px solid var(--upwr-ink); padding-bottom: .4em; margin-bottom: .6em;
    font-family: var(--upwr-mono); font-size: .8em; letter-spacing: .06em; text-transform: uppercase; }
  .d5x-form dl { display: grid; grid-template-columns: minmax(9em, 30%) 1fr; gap: .45em 1em; margin: 0; }
  .d5x-form dt { font-weight: 700; color: var(--upwr-ink); }
  .d5x-form dd { margin: 0; border-bottom: 1px dotted var(--upwr-rule); padding-bottom: .3em; }
  .d5x-form dl.d5x-form-src { grid-template-columns: minmax(9em, 24%) 1fr minmax(10em, 30%); }
  .d5x-form dd.d5x-src { font-family: var(--upwr-mono); font-size: .78em; color: var(--upwr-ink-subtle); }
  @media (max-width: 640px) { .d5x-form dl.d5x-form-src { grid-template-columns: 1fr; } }
  .d5x-form-f { margin-top: .8em; font-family: var(--upwr-mono); font-size: .8em; color: var(--upwr-ink-subtle); }
  .d5x-two { display: grid; grid-template-columns: 1fr 1fr; gap: 1em; }
  .d5x-plan { border: 1px solid var(--upwr-rule); border-top: 4px solid var(--c); border-radius: 4px; padding: .8em 1em; background: var(--upwr-surface); }
  .d5x-plan h5 { margin: 0 0 .5em; font-size: 1em; }
  .d5x-plan dl { margin: 0; display: grid; grid-template-columns: auto 1fr; gap: .3em .8em; font-size: .95em; }
  .d5x-plan dt { color: var(--upwr-ink-subtle); }
  .d5x-plan dd { margin: 0; font-weight: 600; }
  .d5x-miss { color: var(--upwr-accent); }
  .d5x-steps { margin: .3em 0 .6em; padding-left: 1.4em; }
  .d5x-steps li { margin: .35em 0; }
  @media (max-width: 640px) { .d5x-two { grid-template-columns: 1fr; } }
"))

# Karta planu (forma z wariantu B) ze źródłem każdej liczby (kolumna „skąd” z wariantu A).
d5p_planex_rows <- list(
  list("1. Cel", "Znaleźć 3 kiście z pająkiem do zgłoszenia fitosanitarnego. Liczy się pająk żywy albo martwy, widoczny w kiści.",
    "zlecenie kontroli"),
  list("2. Częstość", "p = 0.10 na kiść, z protokołów poprzednich dostaw.",
    "dane Bananpolu, wykład 04"),
  list("3. Limit", "61 kiści (średnio 30, rezerwa 31).",
    "kwantyl 95% rozkładu ujemnego dwumianowego (def. 5.4)"),
  list("4. Szansa ukończenia", "95%: w 19 kontrolach na 20 trzy kiście z pająkiem znajdą się w limicie.",
    "wzór (5.7): P(X ≤ 61) = P(Y₆₁ ≥ 3) ≈ 0.951"),
  list("5. Gdy limit się skończy", "Kontrola staje. Raport podaje, ile kiści z pająkiem znaleziono. Kierownik decyduje, czy kontynuować, i sprawdza, czy p nie spadło.",
    "decyzja, nie rachunek")
)

d5p_planex_ui <- d5p_panel("Przykład 5.7", "Plan kontroli dostawy bananów jako dokument",
  d5p_css, d5p_planex_css,
  tags$p("Tak wygląda plan kontroli dostawy bananów położony na biurku kierownika do podpisu. Po prawej przy każdej rubryce stoi, skąd pochodzi jej treść."),
  tags$div(class = "d5x-form",
    tags$div(class = "d5x-form-h", tags$span("Plan kontroli · Bananpol"), tags$span("Pająki w kiściach")),
    tags$dl(class = "d5x-form-src",
      lapply(d5p_planex_rows, function(x) tagList(
        tags$dt(x[[1]]), tags$dd(x[[2]]), tags$dd(class = "d5x-src", x[[3]])))),
    tags$div(class = "d5x-form-f", "Akceptowane ryzyko niedokończenia: 5% · podpis kierownika: ____________"))
)
