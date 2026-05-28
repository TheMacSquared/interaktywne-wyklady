ch3_ui <- lecture_chapter(id = "ch3", num = "3", title = "Hipotezy jako tropy", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 03 · Hipotezy",
      num = "03",
      title = "Hipotezy jako tropy.",
      lead = "Hipoteza nie jest zakładem honorowym. To roboczy trop, który wolno
              poprawić, zawęzić albo porzucić po kontakcie z danymi."
    ),

    lc_h2("sec-01", "Budujemy kilka wersji historii"),

    div(class = "lc-figure-panel",
      h4("Karta hipotezy"),
      selectInput("ch3_hyp", "Wybierz trop:",
        choices = c(
          "Atrakcyjność wiąże się z oceną kursu" = "beauty",
          "Płeć prowadzącego wiąże się z oceną kursu" = "gender",
          "Native speakerzy są oceniani inaczej" = "native",
          "Kursy jednopunktowe dostają inne oceny" = "credits",
          "Wyższy response rate zmienia interpretację oceny" = "response"
        ),
        selected = "beauty"
      ),
      uiOutput("ch3_hyp_card")
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Zasada warsztatu:"),
      p("Do każdej hipotezy dopisujemy przynajmniej jedno alternatywne wyjaśnienie.
        Jeśli nie umiemy tego zrobić, to zwykle jeszcze nie rozumiemy problemu.")
    ),

    div(class = "lc-figure-panel",
      h4("Mapa alternatywnych wyjaśnień"),
      uiOutput("ch3_alt_map")
    ),

    lc_chapter_next("04", "Co właściwie mierzymy?",
      "Sprawdzamy, czy nasze pojęcia naprawdę mają odpowiedniki w danych.",
      "ch4"),
    div(style = "height: 40px;")
  )))
)

ch3_server <- function(input, output, session) {
  hyp <- reactive({
    list(
      beauty = list(
        q = "Czy prowadzący oceniani jako atrakcyjniejsi dostają wyższe oceny kursu?",
        h = "Wyższe `beauty` współwystępuje z wyższym `eval`.",
        alt = c("Atrakcyjność może być powiązana z wiekiem lub płcią.",
                "Studenci mogą wyżej oceniać osoby bardziej pewne siebie, a nie wygląd sam w sobie.",
                "Efekt może zależeć od typu kursu.")
      ),
      gender = list(
        q = "Czy oceny kursu różnią się między kobietami i mężczyznami prowadzącymi?",
        h = "Średnie `eval` różni się między grupami `gender`.",
        alt = c("Kobiety i mężczyźni mogą prowadzić inne typy kursów.",
                "Różnice mogą wynikać z oczekiwań studentów wobec stylu prowadzenia.",
                "Nierówny response rate może zmieniać obraz.")
      ),
      native = list(
        q = "Czy native speaker status wiąże się z oceną kursu?",
        h = "Średnie `eval` różni się między `native = tak` i `native = nie`.",
        alt = c("Status native może mieszać się z typem kursu.",
                "Studenci mogą oceniać zrozumiałość języka, nie jakość dydaktyczną.",
                "Grupy mogą mieć różną liczebność.")
      ),
      credits = list(
        q = "Czy kursy jednopunktowe są oceniane inaczej niż większe kursy?",
        h = "Średnie `eval` różni się między kategoriami `credits`.",
        alt = c("Mniejsze kursy mogą być łatwiejsze albo mniej obciążające.",
                "Studenci mogą mieć inne oczekiwania wobec kursów pobocznych.",
                "Liczebność i poziom kursu mogą działać razem.")
      ),
      response = list(
        q = "Czy przy niskim odsetku odpowiedzi ocena kursu znaczy to samo?",
        h = "`response.rate` współwystępuje z `eval`.",
        alt = c("Odpowiadają głównie osoby skrajnie zadowolone lub niezadowolone.",
                "Duże kursy mogą mieć niższy response rate.",
                "Response rate może mówić o zaangażowaniu grupy, nie o jakości kursu.")
      )
    )[[input$ch3_hyp]]
  })

  output$ch3_hyp_card <- renderUI({
    x <- hyp()
    div(class = "question-card",
      p(tags$strong("Pytanie: "), x$q),
      p(tags$strong("Robocza hipoteza: "), HTML(gsub("`([^`]+)`", "<code>\\1</code>", x$h))),
      p(tags$strong("Co sprawdzimy najpierw: "),
        "wykres, opis grup lub prosty test dopasowany do typu zmiennych.")
    )
  })

  output$ch3_alt_map <- renderUI({
    tags$ul(lapply(hyp()$alt, tags$li))
  })
}
