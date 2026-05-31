ch7_ui <- lecture_chapter(id = "ch7", num = "7", title = "Checklist projektu grupowego", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 07 · Ściąga projektowa",
      num = "07",
      title = "Checklist projektu grupowego.",
      lead = "Podsumowanie wykładu. Dobry projekt opiera się na celu, zestawie
              hipotez i jasnym planie interpretacji, a nie na wyborze testu."
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Cel, który ciągnęliśmy przez cały wykład:"),
      p(tags$em(tr_goal))
    ),

    lc_h2("sec-01", "Od ciekawości do iteracji"),

    div(class = "lc-feedback lc-feedback-ok",
      tags$strong("Minimalny szkielet dobrego projektu:"),
      tags$ol(
        tags$li(tags$b("Ciekawość:"), " umiem powiedzieć, dlaczego temat mnie interesuje."),
        tags$li(tags$b("Pytanie:"), " jedno główne pytanie badawcze, nie tylko metoda."),
        tags$li(tags$b("Hipotezy:"), " mam trop główny i konkurencyjne wyjaśnienia."),
        tags$li(tags$b("Pomiar:"), " wiem, jakie pojęcie mierzy każda ważna zmienna."),
        tags$li(tags$b("Dane:"), " jednostka obserwacji, źródło danych, braki, jakość próby."),
        tags$li(tags$b("Analiza:"), " wykres/opis/test pasuje do typu pytania i zmiennych."),
        tags$li(tags$b("Model:"), " sprawdziłem(am) tropy razem, nie tylko pojedynczo — czy efekt trzyma się pod kontrolą innych zmiennych."),
        tags$li(tags$b("Interpretacja:"), " wynik zmienia, wzmacnia albo osłabia hipotezę."),
        tags$li(tags$b("Iteracja:"), " wiem, jakie pytanie lub dane byłyby następnym krokiem.")
      )
    ),

    div(class = "lc-figure-panel",
      h4("Autodiagnoza projektu"),
      checkboxGroupInput("ch7_checks", "Co macie już dopracowane?",
        choices = c(
          "Umiemy powiedzieć, skąd wzięła się ciekawość" = "curiosity",
          "Pytanie badawcze jest konkretne" = "q",
          "Wiemy, co jest jednostką obserwacji" = "unit",
          "Nazwaliśmy pojęcie i ograniczenia pomiaru" = "measure",
          "Mamy hipotezę i alternatywne wyjaśnienia" = "h",
          "Plan testu/analizy pasuje do typu zmiennych" = "a",
          "Sprawdziliśmy tropy razem w jednym modelu" = "model",
          "Wiemy, jak wynik może zmienić hipotezę" = "iter",
          "Wniosek będzie ostrożny przy danych obserwacyjnych" = "w"
        )
      ),
      uiOutput("ch7_score")
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Zdanie, które warto mieć w raporcie:"),
      p(tags$em("\"Nasze dane wspierają interpretację X, ale nie pozwalają
        rozstrzygnąć przyczynowości, ponieważ...\"")),
      p("To nie osłabia pracy. To pokazuje, że autorzy rozumieją granice własnego badania.")
    ),

    div(class = "lc-prose",
      p("Właśnie taki standard chcemy przenieść do projektów grupowych: nie metoda
        dla metody, tylko badawcza historia, w której statystyka jest narzędziem
        do ostrożnego dochodzenia do odpowiedzi.")
    ),

    div(class = "lc-figure-panel",
      h4("Tak wygląda domknięty projekt: cel + wiązka + werdykty"),
      div(class = "lc-prose",
        p("Cała droga w jednym kadrze — od celu, przez tropy, po wstępne werdykty.
          Wasz projekt grupowy ma mieć taką samą spójność.")
      ),
      tr_board_ui(reveal = tr_trop_order, show_verdict = TRUE)
    ),

    div(style = "height: 40px;")
  )))
)

ch7_server <- function(input, output, session) {
  output$ch7_score <- renderUI({
    done <- length(input$ch7_checks)
    total <- 9
    pct <- round(done / total * 100)
    color <- if (done <= 3) proj_col_risk else if (done <= 5) proj_col_warn else proj_col_ctrl
    label <- if (done <= 3) {
      "Jeszcze za wcześnie na testy - dopracujcie pytanie i pomiar."
    } else if (done <= 5) {
      "Dobry szkic, ale są luki interpretacyjne."
    } else {
      "Projekt ma kompletną strukturę badawczą."
    }
    tagList(
      div(style = "background: var(--upwr-rule); border-radius: 10px; height: 28px;",
        div(style = paste0("background:", color, "; width:", pct, "%; height: 28px;",
                           "border-radius: 10px; text-align:center; color:white;",
                           "line-height:28px; font-weight:700; transition: width .25s;"),
            paste0(done, "/", total))
      ),
      p(style = paste0("margin-top: 10px; font-weight: 700; color:", color, ";"), label)
    )
  })
}
