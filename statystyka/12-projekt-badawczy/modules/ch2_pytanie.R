ch2_ui <- lecture_chapter(id = "ch2", num = "2", title = "Jak obracać pytanie", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 02 · Rama badawcza",
      num = "02",
      title = "Jak obracać pytanie?",
      lead = "Jedne dane mogą prowadzić do kilku różnych projektów. Wybór ramy
              zmienia hipotezy, analizę i końcowy wniosek."
    ),

    lc_h2("sec-01", "Cztery ramy tej samej historii"),

    div(class = "lc-figure-panel",
      h4("Wybierz ramę pytania"),
      radioButtons("ch2_frame", NULL,
        choices = c(
          "Obciążenie: czy oceny są zniekształcone przez cechy prowadzącego?" = "bias",
          "Trafność: czy ewaluacje mierzą jakość nauczania?" = "validity",
          "Kontekst: czy typ kursu zmienia oceny?" = "context",
          "Sprawiedliwość: czy grupy prowadzących są oceniane podobnie?" = "fairness"
        ),
        selected = "bias"
      ),
      uiOutput("ch2_frame_card")
    ),

    div(class = "research-ladder",
      div(tags$strong("Luźny temat"), "Studenci różnie oceniają prowadzących."),
      div(tags$strong("Pytanie badawcze"), "Czy ta różnica mówi o jakości, czy o czymś pobocznym?"),
      div(tags$strong("Plan analizy"), "Najpierw szukamy prostych tropów, potem alternatywnych wyjaśnień.")
    ),

    tr_discussion_box("Ćwiczenie:",
      tags$li("Przepiszcie wybraną ramę jako jedno zdanie pytające."),
      tags$li("Dopiszcie, dla kogo odpowiedź byłaby ważna."),
      tags$li("Zaznaczcie, czego te dane nie pozwolą rozstrzygnąć.")
    ),

    lc_chapter_next("03", "Hipotezy jako tropy",
      "Nie przywiązujemy się do jednej hipotezy. Budujemy kilka konkurencyjnych opowieści.",
      "ch3"),
    div(style = "height: 40px;")
  )))
)

ch2_server <- function(input, output, session) {
  output$ch2_frame_card <- renderUI({
    cards <- list(
      bias = list(
        title = "Rama: obciążenie w ewaluacjach",
        question = "Czy oceny studenckie zależą od cech prowadzącego, które nie powinny decydować o jakości zajęć?",
        use = "Dobra rama, jeśli interesuje nas etyka używania ewaluacji w decyzjach kadrowych.",
        danger = "Łatwo pomylić obciążenie ocen z realnymi różnicami w stylu prowadzenia zajęć."
      ),
      validity = list(
        title = "Rama: trafność pomiaru",
        question = "Czy `eval` jest sensownym wskaźnikiem jakości nauczania, czy raczej satysfakcji studentów?",
        use = "Dobra rama, jeśli projekt ma dotyczyć jakości danych i pomiaru.",
        danger = "Nie mamy bezpośredniego pomiaru efektów uczenia się."
      ),
      context = list(
        title = "Rama: kontekst kursu",
        question = "Czy oceny zależą od poziomu kursu, liczebności albo typu zajęć?",
        use = "Dobra rama, jeśli chcemy myśleć o porównywalności ocen między kursami.",
        danger = "Brakuje wielu cech kursu: obowiązkowości, trudności, pory zajęć."
      ),
      fairness = list(
        title = "Rama: sprawiedliwość ocen",
        question = "Czy grupy prowadzących, np. według płci albo native speaker statusu, są oceniane podobnie?",
        use = "Dobra rama, jeśli interesuje nas równość i możliwe nierówne traktowanie.",
        danger = "Różnice grupowe wymagają ostrożnej interpretacji i kontroli kontekstu."
      )
    )
    x <- cards[[input$ch2_frame]]
    div(class = "question-card",
      h4(x$title),
      p(tags$strong("Pytanie: "), x$question),
      p(tags$strong("Dlaczego warto: "), x$use),
      p(tags$strong("Ryzyko interpretacji: "), x$danger)
    )
  })
}
