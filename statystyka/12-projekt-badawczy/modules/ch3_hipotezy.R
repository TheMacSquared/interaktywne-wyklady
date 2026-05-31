ch3_ui <- lecture_chapter(id = "ch3", num = "2", title = "Hipotezy jako tropy", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 02 · Hipotezy",
      num = "02",
      title = "Hipotezy jako tropy.",
      lead = "Hipoteza to robocze przypuszczenie o związku między zmiennymi.
              Wolno je zawęzić lub odrzucić, gdy dane go nie potwierdzają."
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Przypomnienie celu:"),
      p(tags$em(tr_goal))
    ),

    lc_h2("sec-01", "Wszystkie hipotezy naraz"),

    div(class = "lc-prose",
      p("Projekt badawczy zwykle obejmuje kilka hipotez pod wspólnym celem.
        Każda ma własne pytanie, roboczą hipotezę i alternatywne wyjaśnienia.
        To ta sama wiązka, którą wprowadzono w rozdziale 1 — tutaj zapisana
        jako formalne hipotezy.")
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Zasada:"),
      p("Do każdej hipotezy podajemy co najmniej jedno alternatywne wyjaśnienie
        obserwacji. Brak takiego wyjaśnienia zwykle oznacza, że problem nie jest
        jeszcze dobrze rozpoznany.")
    ),

    uiOutput("ch3_bundle"),

    lc_chapter_next("03", "Co właściwie mierzymy?",
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
