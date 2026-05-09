# ============================================================================
# ROZDZIAŁ 4: Czytanie tabeli wyników (ćwiczenie)
# ============================================================================

ch4_ui <- lecture_chapter(
  id = "ch-cwiczenie",
  num = "04",
  title = "Czytanie tabeli wyników",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 03 · Estymacja",
      num = "04",
      title = "Czytanie tabeli wyników.",
      lead = "Sprawdź się: czy potrafisz odczytać tabelę wyników modelu regresji? Trzy pytania o tę samą tabelę — odpowiedzi pokazują, czy umiesz przejść z liczb do wniosku."
    ),

    lc_h2("ch4-pomysl", "Sytuacja"),
    lc_p("Analityk HR z firmy produkcyjnej zbadał, jak staż pracy (X, w latach) wiąże się z miesięcznym wynagrodzeniem (Y, w tys. zł) na próbie 80 pracowników. Dopasował prosty model regresji liniowej KMNK. Wyniki:"),

    figure_panel(
      label = "Tabela 4.1",
      title = "Model: wynagrodzenie ~ staż (n = 80)",
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
          tags$tr(
            tags$td("Wyraz wolny"),
            tags$td("3.50"),
            tags$td("0.42"),
            tags$td("8.33"),
            tags$td("< 0.001")
          ),
          tags$tr(
            tags$td("Staż (lata)"),
            tags$td("0.18"),
            tags$td("0.04"),
            tags$td("4.50"),
            tags$td("< 0.001")
          )
        )
      ),
      tags$p(
        style = "margin-top:8px; font-style:italic; color:var(--upwr-reference);",
        "R² = 0.205, n = 80, df = 78, t z tabeli (α = 0.05, dwustronnie): t₀.₀₂₅,₇₈ ≈ 1.99"
      )
    ),

    lc_h2("ch4-pytanie1", "Pytanie 1: Co znaczy SE = 0.04 dla stażu?"),
    radioButtons(
      "ch4_q1",
      label = NULL,
      choices = c(
        "Dane są dokładne do 0.04 tys. zł"                          = "a",
        "b₁ wahałoby się o ±0.04 między różnymi próbami z tej populacji" = "b",
        "p-wartość to 0.04"                                         = "c",
        "4% pracowników odstaje od trendu"                          = "d"
      ),
      selected = character(0)
    ),
    uiOutput("ch4_feedback1"),

    lc_h2("ch4-pytanie2", "Pytanie 2: Jaki jest 95% przedział ufności dla stażu?"),
    radioButtons(
      "ch4_q2",
      label = NULL,
      choices = c(
        "[0.10, 0.26]"                  = "a",
        "[-0.04, 0.40]"                 = "b",
        "[0.14, 0.22]"                  = "c",
        "Nie da się policzyć z tabeli"  = "d"
      ),
      selected = character(0)
    ),
    uiOutput("ch4_feedback2"),

    lc_h2("ch4-pytanie3", "Pytanie 3: Czy staż istotnie wpływa na wynagrodzenie?"),
    radioButtons(
      "ch4_q3",
      label = NULL,
      choices = c(
        "Tak (p < 0.001), wzrost stażu o rok = średnio +180 zł miesięcznie" = "a",
        "Nie, R² jest niski"                                                = "b",
        "Tylko dla pracowników > 5 lat"                                     = "c",
        "Nie da się stwierdzić z tych danych"                               = "d"
      ),
      selected = character(0)
    ),
    uiOutput("ch4_feedback3"),

    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      open = TRUE,
      "Tabela KMNK to sześć kluczowych liczb na zmienną: estymata, SE, t, p oraz R² i SE reszt globalnie. Nawet bez liczenia ręcznego potrafisz z nich opisać model słowami — kierunek efektu (znak estymaty), jego siłę (wartość estymaty), niepewność (SE i przedział), istotność (p) i jakość dopasowania (R²)."
    )
  )
)

ch4_server <- function(input, output, session) {
  output$ch4_feedback1 <- renderUI({
    if (is.null(input$ch4_q1) || !nzchar(input$ch4_q1)) return(NULL)
    if (input$ch4_q1 == "b") {
      lc_feedback(
        type = "ok",
        tags$p(strong("Dokładnie!"),
               " SE to odchylenie standardowe rozkładu estymatora b₁ — czyli właśnie miara tego, jak bardzo nachylenie wahałoby się między różnymi próbami z tej samej populacji.")
      )
    } else {
      msg <- switch(input$ch4_q1,
        a = "Odpowiedź a) myli SE z dokładnością pomiaru danych. SE nie opisuje precyzji liczb wpisanych do tabeli, tylko niepewność oszacowania b₁.",
        c = "Odpowiedź c) myli SE z p-wartością. To dwie różne liczby: SE jest w jednostkach Y na X, p to prawdopodobieństwo.",
        d = "Odpowiedź d) myli SE z udziałem nietypowych obserwacji. SE nie zlicza outlierów — opisuje rozrzut estymatora między próbami."
      )
      lc_feedback(
        type = "warning",
        tags$p(strong("Nie do końca."), " ", msg,
               " Poprawna odpowiedź: ", strong("b)"),
               " — SE = 0.04 mówi, że b₁ wahałoby się o ±0.04 między różnymi próbami.")
      )
    }
  })

  output$ch4_feedback2 <- renderUI({
    if (is.null(input$ch4_q2) || !nzchar(input$ch4_q2)) return(NULL)
    if (input$ch4_q2 == "a") {
      lc_feedback(
        type = "ok",
        tags$p(strong("Dokładnie!"),
               " 0.18 ± 1.99 · 0.04 = 0.18 ± 0.0796 ≈ [0.10, 0.26]. ",
               "Procedura budowania takich przedziałów trafiałaby w prawdziwe nachylenie populacyjne β₁ w około 95% powtarzanych prób.")
      )
    } else {
      msg <- switch(input$ch4_q2,
        b = "Odpowiedź b) używa zbyt szerokiego marginesu (jakby t krytyczne wynosiło 5.5) — to przeszacowanie. Dla df = 78 t₀.₀₂₅ ≈ 1.99.",
        c = "Odpowiedź c) używa za wąskiego marginesu (±0.04 zamiast ±1.99·0.04). Sam SE to za mało — trzeba pomnożyć przez t krytyczne.",
        d = "Odpowiedź d) jest zbyt ostrożna. Przedział ufności da się policzyć: estymata, SE i t krytyczne wystarczą."
      )
      lc_feedback(
        type = "warning",
        tags$p(strong("Nie do końca."), " ", msg,
               " Poprawna odpowiedź: ", strong("a) [0.10, 0.26]"),
               " — wzór: 0.18 ± 1.99 · 0.04.")
      )
    }
  })

  output$ch4_feedback3 <- renderUI({
    if (is.null(input$ch4_q3) || !nzchar(input$ch4_q3)) return(NULL)
    if (input$ch4_q3 == "a") {
      lc_feedback(
        type = "ok",
        tags$p(strong("Dokładnie!"),
               " p < 0.001 oznacza, że efekt stażu jest statystycznie istotny. Estymata 0.18 (tys. zł na rok stażu) przekłada się na ok. 180 zł miesięcznie za każdy dodatkowy rok pracy — to interpretacja nachylenia ‚jak w raporcie’.")
      )
    } else {
      msg <- switch(input$ch4_q3,
        b = "Odpowiedź b) myli istotność z dopasowaniem. R² = 0.205 mówi, że staż wyjaśnia 20.5% zmienności płac — to rzeczywiście niedużo, ale nie unieważnia istotnego, dodatniego efektu stażu. R² i p-wartość odpowiadają na różne pytania.",
        c = "Odpowiedź c) wprowadza warunek, którego w modelu nie ma. Model liniowy zakłada stały efekt na całej skali stażu, więc nie możemy stwierdzić, że ‚tylko powyżej 5 lat’.",
        d = "Odpowiedź d) jest zbyt ostrożna. Tabela podaje wprost p < 0.001 i estymatę dodatnią — to wystarczy, żeby wyciągnąć wniosek o istotnym, dodatnim efekcie stażu."
      )
      lc_feedback(
        type = "warning",
        tags$p(strong("Nie do końca."), " ", msg,
               " Poprawna odpowiedź: ", strong("a)"),
               " — p < 0.001, dodatnia estymata, +180 zł miesięcznie na rok stażu.")
      )
    }
  })
}
