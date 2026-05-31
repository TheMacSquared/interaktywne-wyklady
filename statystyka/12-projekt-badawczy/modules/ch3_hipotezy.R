ch3_ui <- lecture_chapter(id = "ch3", num = "3", title = "Hipotezy jako tropy", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 03 · Hipotezy",
      num = "03",
      title = "Hipotezy jako tropy.",
      lead = "Hipoteza nie jest zakładem honorowym. To roboczy trop, który wolno
              poprawić, zawęzić albo porzucić po kontakcie z danymi."
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Przypomnienie celu:"),
      p(tags$em(tr_goal))
    ),

    lc_h2("sec-01", "Cała wiązka hipotez naraz"),

    div(class = "lc-prose",
      p("Projekt badawczy rzadko stoi na jednej hipotezie. Pod jednym celem
        rozkładamy kilka konkurujących tropów — każdy z własnym pytaniem,
        roboczą hipotezą i alternatywnymi wyjaśnieniami. To ta sama wiązka,
        którą wprowadziliśmy w rozdziale 1; tutaj nadajemy jej kształt hipotez.")
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Zasada warsztatu:"),
      p("Do każdej hipotezy dopisujemy przynajmniej jedno alternatywne wyjaśnienie.
        Jeśli nie umiemy tego zrobić, to zwykle jeszcze nie rozumiemy problemu.")
    ),

    uiOutput("ch3_bundle"),

    lc_chapter_next("04", "Co właściwie mierzymy?",
      "Sprawdzamy, czy nasze pojęcia naprawdę mają odpowiedniki w danych.",
      "ch4"),
    div(style = "height: 40px;")
  )))
)

ch3_server <- function(input, output, session) {
  output$ch3_bundle <- renderUI({
    cards <- lapply(tr_trop_order, function(id) {
      tr <- tr_tropy[[id]]
      div(class = "trop-card",
        h4(tr$short),
        p(tags$strong("Pytanie: "), tr$question),
        p(tags$strong("Robocza hipoteza: "),
          HTML(gsub("`([^`]+)`", "<code>\\1</code>", tr$hypothesis))),
        p(tags$strong("Alternatywne wyjaśnienia:")),
        tags$ul(class = "trop-alt", lapply(tr$alt, tags$li))
      )
    })
    div(class = "trop-stack", cards)
  })
}
