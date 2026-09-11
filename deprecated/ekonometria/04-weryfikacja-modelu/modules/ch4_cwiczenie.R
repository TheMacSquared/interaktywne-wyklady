# ============================================================================
# ROZDZIAŁ 4: Diagnoza krok po kroku — ćwiczenie
# ============================================================================

ch4_ui <- lecture_chapter(
  id = "ch-cwiczenie",
  num = "04",
  title = "Diagnoza krok po kroku",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 04 · Weryfikacja",
      num = "04",
      title = "Postaw diagnozę modelowi.",
      lead = "Sprawdź się: postaw diagnozę modelowi po raporcie z konsoli."
    ),

    lc_h2("ch4-pomysl", "Sytuacja"),
    lc_p("Doradca rolniczy zbudował model: plon pszenicy (Y, dt/ha) ~ ilość nawozu (X, kg/ha) na próbie 45 gospodarstw. Wyniki:"),

    figure_panel(
      label = "Tabela 4.1",
      title = "Raport z modelu",
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Współczynnik"),
          tags$th("Estymata"),
          tags$th("SE"),
          tags$th("t"),
          tags$th("p")
        )),
        tags$tbody(
          tags$tr(tags$td("Wyraz wolny"), tags$td("25.0"), tags$td("3.50"), tags$td("7.14"), tags$td("<0.001")),
          tags$tr(tags$td("Nawóz"),       tags$td("0.18"), tags$td("0.04"), tags$td("4.50"), tags$td("<0.001"))
        )
      ),
      p(tags$em("R² = 0.69, SE_e = 4.2 dt/ha, n = 45"))
    ),

    lc_p("Wykres reszt vs fitted pokazuje WYRAŹNY ŁUK — reszty są ujemne dla małych i dużych wartości fitted, dodatnie pośrodku. Dodatkowo: kierunek β₁ jest dodatni (więcej nawozu → wyższy plon), zgodny z intuicją; jednostki są poprawne; wszystkie gospodarstwa są w tym samym regionie, więc klimat nie wymaga uwzględnienia."),

    lc_h2("ch4-checklist", "Postaw diagnozę"),
    lc_p("Zaznacz, które warunki dobrego modelu są tu spełnione:"),
    figure_panel(
      label = "Ćwiczenie 4.1",
      title = "Kryteria oceny modelu",
      checkboxGroupInput(
        "ch4_diag", NULL,
        choiceNames = list(
          "Znak β₁ poprawny (zgodny z teorią)",
          "Skala efektu sensowna",
          "p-wartość bardzo niska",
          "R² wysoki",
          "Założenie liniowości spełnione",
          "Brak pominiętych zmiennych zakłócających"
        ),
        choiceValues = list("znak", "skala", "p", "r2", "linio", "pominiete")
      ),
      uiOutput("ch4_feedback")
    ),

    lc_h2("ch4-co-dalej", "Co zrobić dalej?"),
    figure_panel(
      label = "Ćwiczenie 4.2",
      title = "Wybierz kolejny krok",
      radioButtons(
        "ch4_action", NULL,
        choices = c(
          "Dodać X² do modelu (pozwala na krzywoliniowość)" = "a",
          "Zwiększyć próbę"                                 = "b",
          "Wybrać nieparametryczny test"                    = "c",
          "Zostawić — i tak p < 0.001"                      = "d"
        ),
        selected = character(0)
      ),
      uiOutput("ch4_feedback_action")
    ),

    inline_callout(
      label = "Wskazówka",
      color = "wskazowka",
      open = TRUE,
      "W diagnostyce pierwszy krok to zawsze rzut oka na wykres reszt. Wzorzec w resztach = sygnał, że model coś pomija. Wzór nie zawsze polega na p-wartości."
    )
  )
)

ch4_server <- function(input, output, session) {
  output$ch4_feedback <- renderUI({
    sel <- input$ch4_diag
    if (is.null(sel) || length(sel) == 0) {
      return(lc_feedback("Zaznacz wybrane kryteria, aby zobaczyć ocenę.", type = "info"))
    }

    has_linio <- "linio" %in% sel
    stat_set <- all(c("znak", "skala", "p", "r2") %in% sel)

    if (stat_set && !has_linio) {
      lc_feedback(
        paste0("Dobra diagnoza! Model statystycznie wygląda dobrze — znak i skala β₁ są sensowne, ",
               "p < 0.001, R² = 0.69. Ale założenie liniowości jest złamane — łuk w resztach to sygnał alarmowy. ",
               "Mimo świetnych liczb model opisuje relację niewłaściwą postacią funkcyjną."),
        type = "ok"
      )
    } else if (has_linio) {
      lc_feedback(
        paste0("Uważaj: reszty mają wyraźny łuk. To bezpośredni sygnał, że założenie liniowości NIE jest spełnione. ",
               "Wykres reszt vs fitted z systematycznym wzorcem oznacza, że prosta jest zbyt prostym opisem zjawiska."),
        type = "warning"
      )
    } else {
      lc_feedback(
        paste0("Brakuje pełnego obrazu. Sprawdź wszystkie kryteria — zwłaszcza znak, skalę, p-wartość, R² ",
               "i (kluczowe dla tego przykładu) założenie liniowości w resztach."),
        type = "info"
      )
    }
  })

  output$ch4_feedback_action <- renderUI({
    a <- input$ch4_action
    if (is.null(a) || length(a) == 0) {
      return(lc_feedback("Wybierz jedną z opcji.", type = "info"))
    }
    switch(a,
      a = lc_feedback(
        paste0("Dokładnie. Łuk w resztach to wzorzec krzywoliniowy — dodanie X² (modelu kwadratowego) ",
               "pozwala dopasować parabolę i usunąć systematyczny wzorzec z reszt. To minimalna modyfikacja, ",
               "która adresuje konkretny problem."),
        type = "ok"
      ),
      b = lc_feedback(
        paste0("Nie. Próba n = 45 daje już wyraźną istotność (p < 0.001), więc problem nie jest w mocy testu. ",
               "Większa próba nie naprawi nieliniowości — będziemy mieć ten sam wzorzec łuku, tylko na większej liczbie punktów."),
        type = "warning"
      ),
      c = lc_feedback(
        paste0("Nie do końca. Test nieparametryczny rezygnuje z założenia rozkładu, ale tu problem jest inny: ",
               "źle określona postać funkcyjna. Trzeba poprawić model, a nie zmienić test."),
        type = "warning"
      ),
      d = lc_feedback(
        paste0("Nie. p-wartość mówi tylko, że b₁ jest różne od zera — ale jeśli model jest źle wyspecyfikowany, ",
               "to b₁ szacuje średnie nachylenie nieliniowej zależności, nie prawdziwą strukturę. Zostawienie modelu ",
               "z łukiem w resztach to ignorowanie kluczowej diagnozy."),
        type = "warning"
      )
    )
  })
}
