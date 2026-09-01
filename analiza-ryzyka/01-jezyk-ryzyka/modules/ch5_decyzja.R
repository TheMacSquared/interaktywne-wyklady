# ==========================================================================
# ROZDZIAŁ 5: OD PRAWDOPODOBIEŃSTWA DO DECYZJI
# ==========================================================================

ch5_ui <- lecture_chapter(
  id = "ch-decyzja",
  num = "05",
  title = "Od liczby do decyzji",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 05 · Język ryzyka",
      num = "05",
      title = "Który problem powinien być pierwszy?",
      lead = "Dyrektor Bananpolu dostał dwa wyniki dotyczące bezpieczeństwa.
              Obejrzyj dwa przypadki i wybierz najlepiej uzasadniony wniosek.
              Dopiero potem porównamy możliwe sposoby rozumowania."
    ),

    lc_h2("ch5-porownanie", "Dwa problemy dyrektora Bananpolu"),
    lc_p(
      "Poniższe liczby są fikcyjne i służą wyłącznie temu ćwiczeniu. Porównaj
       oba przypadki, zwracając uwagę na dokładne brzmienie każdej odpowiedzi."
    ),

    lc_stat_grid(
      lc_stat_box(
        "A · Poślizgnięcie",
        "P = 0,08 na zmianę",
        caption = "Możliwy skutek: od braku urazu do złamania",
        color = upwr_cat[["bursztyn"]]
      ),
      lc_stat_box(
        "B · Kolizja z wózkiem",
        "P = 0,002 na zmianę",
        caption = "Możliwy skutek: ciężki lub śmiertelny uraz",
        color = upwr_cat[["terakota"]]
      ),
      columns = 2
    ),

    figure_panel(
      label = "Decyzja",
      title = "Jaki priorytet można teraz uzasadnić?",
      radioButtons(
        "ch5_priority",
        "Wybierz najlepiej uzasadnione stwierdzenie",
        choices = c(
          "Najpierw A, bo ma większe prawdopodobieństwo" = "a",
          "Najpierw B, bo może mieć cięższy skutek" = "b",
          "Oba problemy mają takie samo ryzyko" = "equal",
          "Same prawdopodobieństwa nie wystarczają do ustalenia priorytetu" = "insufficient"
        ),
        selected = character(0)
      ),
      actionButton("ch5_check", "Sprawdź rozumowanie", class = "lc-btn-primary"),
      uiOutput("ch5_feedback")
    ),

    margin_callout(
      label = "Granica wykładu",
      "Ten kurs buduje przede wszystkim składową probabilistyczną analizy.
       Skutków nie zamieniamy automatycznie w pieniądze ani punkty.",
      color = "uwaga"
    ),

    lc_h2("ch5-profil", "Profil dwóch problemów"),
    lc_p(
      "Dla obu problemów warto zapisać profil: definicję zdarzenia,
       prawdopodobieństwo wraz z horyzontem, możliwe skutki, liczbę osób
       eksponowanych, niepewność danych, działające bariery oraz dostępne
       działania. Dopiero wtedy można zastosować jawne kryteria priorytetu."
    ),

    figure_panel(
      label = "Profil ryzyka",
      full_width = TRUE,
      tags$table(
        class = "lc-table lc-table-striped lc-table-bordered",
        tags$thead(tags$tr(
          tags$th("Pytanie"), tags$th("Poślizgnięcie"), tags$th("Kolizja z wózkiem")
        )),
        tags$tbody(
          tags$tr(tags$td("Co może się zdarzyć?"), tags$td("Upadek w korytarzu"), tags$td("Potrącenie pieszego")),
          tags$tr(tags$td("Kto jest eksponowany?"), tags$td("Osoby korzystające z przejścia"), tags$td("Piesi w strefie transportu")),
          tags$tr(tags$td("Jakie są skutki?"), tags$td("Różna dotkliwość urazu"), tags$td("Możliwy uraz ciężki")),
          tags$tr(tags$td("Jakie bariery działają?"), tags$td("Sprzątanie i oznakowanie"), tags$td("Separacja ruchu i ograniczenie prędkości")),
          tags$tr(tags$td("Czego nie wiemy?"), tags$td("Kompletność rejestru"), tags$td("Ruch pieszych i zdarzenia bliskie wypadku"))
        )
      )
    ),

    lc_h2("ch5-macierz", "Jak czytać macierz ryzyka"),
    lc_p(
      "Kategorie „rzadkie”, „możliwe”, „poważne” i „katastrofalne” pomagają
       porządkować dyskusję, ale są skalami porządkowymi. Iloczyn numerów pól
       1–5 nie staje się automatycznie ilościową miarą ryzyka. Granice kategorii
       i reguły decyzji muszą być jawne."
    ),

    lc_feedback(
      type = "ok",
      tags$strong("Dobra praktyka:"),
      " wynik probabilistyczny kończ zdaniem: co ten wynik zmienia, jakiego
        skutku dotyczy i które założenie jest najważniejsze."
    ),

    lc_chapter_next(
      num = "06",
      title = "Ściąga",
      lead = "Zbierzemy cały wykład w jedną mapę pojęć i pytań.",
      target_id = "ch-sciaga"
    )
  )
)

ch5_server <- function(input, output, session) {
  check_count <- reactiveVal(0L)

  observeEvent(input$ch5_check, {
    check_count(check_count() + 1L)
  })

  output$ch5_feedback <- renderUI({
    req(check_count() > 0)
    answer <- input$ch5_priority
    if (is.null(answer)) answer <- ""
    is_correct <- identical(answer, "insufficient")

    lc_feedback(
      type = if (is_correct) "ok" else "warning",
      tags$strong(if (is_correct) "Dobrze." else "To zbyt szybki ranking."),
      if (is_correct) {
        paste(
          "Prawdopodobieństwa odpowiadają tylko na część pytania.",
          "Priorytet wymaga jawnych kryteriów skutku, ekspozycji, barier i wykonalności działań."
        )
      } else {
        paste(
          "Wybrany argument może być ważny, ale sam nie wystarcza.",
          "Najpierw uzupełnij profil obu problemów i kryteria decyzji."
        )
      }
    )
  })
}
