# ============================================================================
# ROZDZIAŁ 4: Czytanie wyników solvera
# ============================================================================

ch4_ui <- lecture_chapter(
  id = "ch-cwiczenie",
  num = "04",
  title = "Czytanie wyników solvera",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 07 · Ćwiczenie",
      num = "04",
      title = "Raport solvera w praktyce.",
      lead = "Sprawdź się: czy potrafisz przeczytać raport solvera (Excel/Gretl/R)? Trzy pytania, trzy decyzje biznesowe."
    ),

    lc_h2("ch4-sytuacja", "Sytuacja"),
    lc_p("Stolarz produkuje krzesła (x₁) i stoły (x₂). Zasoby: drewno (240 m), praca (200 godz). Zysk jednostkowy: 80 zł za krzesło, 60 zł za stół. Krzesło zużywa 4 m drewna i 3 godz pracy; stół 6 m drewna i 5 godz pracy. Oddał problem do solvera i otrzymał następujący raport (skrócony):"),

    figure_panel(
      label = "Raport",
      title = "Wynik solvera dla problemu stolarza",
      tags$div(
        tags$h4("Rozwiązanie optymalne", style = "margin-top: 0;"),
        tags$table(
          class = "table",
          tags$thead(tags$tr(
            tags$th("Zmienna"),
            tags$th("Wartość"),
            tags$th("Koszt redukowany")
          )),
          tags$tbody(
            tags$tr(tags$td("x₁ (krzesła)"), tags$td("60"),
                    tags$td("0")),
            tags$tr(tags$td("x₂ (stoły)"),  tags$td("0"),
                    tags$td("−16 (zysk niewystarczający, żeby produkować)"))
          )
        ),
        tags$h4("Ograniczenia"),
        tags$table(
          class = "table",
          tags$thead(tags$tr(
            tags$th("Ograniczenie"),
            tags$th("LHS"),
            tags$th("RHS"),
            tags$th("Cena dualna y_i")
          )),
          tags$tbody(
            tags$tr(tags$td("Drewno"), tags$td("240"), tags$td("240"),
                    tags$td("20 (wąskie gardło)")),
            tags$tr(tags$td("Praca"),  tags$td("180"), tags$td("200"),
                    tags$td("0 (rezerwa 20 godz)"))
          )
        ),
        tags$div(style = "margin-top: 12px; font-weight: 600;",
                 "Funkcja celu: Z = 4800 zł")
      )
    ),

    lc_h2("ch4-pytania", "Pytania do raportu"),

    figure_panel(
      label = "Pytanie 1",
      title = "Cena dualna drewna",
      radioButtons(
        "ch4_q1",
        "Co to znaczy, że cena dualna drewna y₁ = 20?",
        choices = list(
          "20 zł na metr drewna to koszt jego zakupu" = "a",
          "Każdy dodatkowy metr drewna podniesie zysk o 20 zł (do pewnego progu)" = "b",
          "Drewno warto zwiększyć tylko o 20 metrów" = "c",
          "20 to liczba krzeseł" = "d"
        ),
        selected = character(0)
      ),
      uiOutput("ch4_feedback_q1")
    ),

    figure_panel(
      label = "Pytanie 2",
      title = "Co zrobić, żeby zwiększyć zysk?",
      radioButtons(
        "ch4_q2",
        "Stolarz chce zwiększyć zysk. Co poradzisz?",
        choices = list(
          "Kupić więcej drewna" = "a",
          "Zatrudnić dodatkowego pracownika" = "b",
          "Obniżyć zużycie drewna na krzesło" = "c",
          "To i tak nic nie zmieni" = "d"
        ),
        selected = character(0)
      ),
      uiOutput("ch4_feedback_q2")
    ),

    figure_panel(
      label = "Pytanie 3",
      title = "Dlaczego x₂ = 0?",
      radioButtons(
        "ch4_q3",
        "Dlaczego solver postawił produkcję stołów na zero?",
        choices = list(
          "Stół jest zakazany przepisami" = "a",
          "Producent nie ma narzędzi" = "b",
          "Zysk ze stołu jest niewystarczający w stosunku do zużycia drewna — koszt redukowany ujemny" = "c",
          "To pomyłka solvera" = "d"
        ),
        selected = character(0)
      ),
      uiOutput("ch4_feedback_q3")
    ),

    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      "Trzy liczby z raportu solvera, które musisz umieć odczytać: wartości zmiennych decyzyjnych (x*) — co i ile produkować; ceny dualne ograniczeń (y_i) — gdzie inwestować, żeby podnieść zysk; koszty redukowane — dlaczego niektóre zmienne są zerowe. To jest mapa decyzyjna dla biznesu, nie tylko liczba w komórce Excela."
    )
  )
)

ch4_server <- function(input, output, session) {

  output$ch4_feedback_q1 <- renderUI({
    ans <- input$ch4_q1
    if (is.null(ans) || identical(ans, "")) return(NULL)
    if (identical(ans, "b")) {
      lc_feedback(
        type = "ok",
        p(tags$strong("Dokładnie!"),
          " Cena dualna y₁ = 20 zł/m to wartość krańcowa zasobu w problemie. ",
          "Każdy dodatkowy metr drewna zwiększa optymalny zysk o 20 zł — ale tylko lokalnie, ",
          "do progu, w którym wąskim gardłem zostanie inny zasób (tu: praca).")
      )
    } else {
      lc_feedback(
        type = "warning",
        p(tags$strong("Nie do końca."),
          " Cena dualna NIE jest ceną rynkową ani ilością — to wewnętrzna wartość zasobu w problemie. ",
          "Mówi: o ile wzrośnie zysk Z, gdy podniesiemy RHS ograniczenia o 1 jednostkę. Poprawna odpowiedź: B.")
      )
    }
  })

  output$ch4_feedback_q2 <- renderUI({
    ans <- input$ch4_q2
    if (is.null(ans) || identical(ans, "")) return(NULL)
    if (identical(ans, "a")) {
      lc_feedback(
        type = "ok",
        p(tags$strong("Dobrze!"),
          " Drewno jest wąskim gardłem (cena dualna 20 > 0). Dokładanie drewna podnosi zysk — ",
          "każdy dodatkowy metr to +20 zł, dopóki praca nie stanie się ograniczeniem aktywnym.")
      )
    } else if (identical(ans, "b")) {
      lc_feedback(
        type = "warning",
        p(tags$strong("Niezupełnie."),
          " Praca ma cenę dualną 0 — jest rezerwa 20 godz. Dodatkowy pracownik nic nie zmieni, ",
          "bo i tak nie wykorzystujemy całej dostępnej pracy. Inwestycja w drewno (a) podnosi zysk; ",
          "inwestycja w pracę — nie.")
      )
    } else {
      lc_feedback(
        type = "warning",
        p(tags$strong("Nie do końca."),
          " W raporcie wąskim gardłem jest drewno (y₁ = 20). To tu inwestycja przekłada się na zysk. ",
          "Poprawna odpowiedź: A.")
      )
    }
  })

  output$ch4_feedback_q3 <- renderUI({
    ans <- input$ch4_q3
    if (is.null(ans) || identical(ans, "")) return(NULL)
    if (identical(ans, "c")) {
      lc_feedback(
        type = "ok",
        p(tags$strong("Dokładnie!"),
          " Koszt redukowany x₂ = −16 mówi: zysk jednostkowy ze stołu (60 zł) jest o 16 zł za mały ",
          "w stosunku do tego, co stół „kosztuje” w cenach dualnych zasobów. ",
          "Stół zużywa 6 m drewna · 20 zł/m = 120 zł wartości dualnej drewna; to więcej niż 60 zł zysku, ",
          "więc opłaca się produkować tylko krzesła.")
      )
    } else {
      lc_feedback(
        type = "warning",
        p(tags$strong("Nie."),
          " Solver się nie myli i nie zna przepisów. x₂ = 0, bo koszt redukowany jest ujemny: ",
          "zysk ze stołu nie pokrywa wartości dualnych zużywanych zasobów. Poprawna odpowiedź: C.")
      )
    }
  })
}
