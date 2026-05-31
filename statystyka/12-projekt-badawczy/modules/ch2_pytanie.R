ch2_ui <- lecture_chapter(id = "ch2", num = "2", title = "Jak obracać pytanie", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 02 · Rama badawcza",
      num = "02",
      title = "Jak obracać pytanie?",
      lead = "Cel zostaje ten sam, ale można go ująć w różnych ramach. Rama
              decyduje, dla kogo odpowiedź jest ważna i co wolno z niej wyczytać."
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Przypomnienie celu:"),
      p(tags$em(tr_goal))
    ),

    lc_h2("sec-01", "Cztery ramy tej samej historii"),

    div(class = "lc-prose",
      p("Rama nie zmienia celu — zmienia perspektywę, z której na niego patrzymy.
        Ta sama wiązka tropów wygląda inaczej, gdy myślimy o etyce ewaluacji,
        o jakości pomiaru, o kontekście kursu albo o sprawiedliwości ocen.
        Poniżej wszystkie cztery ramy naraz.")
    ),

    uiOutput("ch2_frames"),

    div(class = "research-ladder",
      div(tags$strong("Luźny temat"), "Studenci różnie oceniają prowadzących."),
      div(tags$strong("Cel badawczy"), "Czy ta różnica mówi o jakości, czy o czymś pobocznym?"),
      div(tags$strong("Plan analizy"), "Najpierw szukamy prostych tropów, potem alternatywnych wyjaśnień.")
    ),

    tr_discussion_box("Ćwiczenie:",
      tags$li("Wybierzcie ramę, która najlepiej pasuje do waszego celu, i przepiszcie ją jako jedno zdanie."),
      tags$li("Dopiszcie, dla kogo odpowiedź byłaby ważna."),
      tags$li("Zaznaczcie, czego te dane nie pozwolą rozstrzygnąć.")
    ),

    lc_chapter_next("03", "Hipotezy jako tropy",
      "Nie przywiązujemy się do jednej hipotezy. Cała wiązka jedzie razem.",
      "ch3"),
    div(style = "height: 40px;")
  )))
)

ch2_server <- function(input, output, session) {
  output$ch2_frames <- renderUI({
    cards <- list(
      list(
        title = "Rama: obciążenie w ewaluacjach",
        question = "Czy oceny studenckie zależą od cech prowadzącego, które nie powinny decydować o jakości zajęć?",
        use = "Dobra rama, jeśli interesuje nas etyka używania ewaluacji w decyzjach kadrowych.",
        danger = "Łatwo pomylić obciążenie ocen z realnymi różnicami w stylu prowadzenia zajęć."
      ),
      list(
        title = "Rama: trafność pomiaru",
        question = "Czy `eval` jest sensownym wskaźnikiem jakości nauczania, czy raczej satysfakcji studentów?",
        use = "Dobra rama, jeśli projekt ma dotyczyć jakości danych i pomiaru.",
        danger = "Nie mamy bezpośredniego pomiaru efektów uczenia się."
      ),
      list(
        title = "Rama: kontekst kursu",
        question = "Czy oceny zależą od poziomu kursu, liczebności albo typu zajęć?",
        use = "Dobra rama, jeśli chcemy myśleć o porównywalności ocen między kursami.",
        danger = "Brakuje wielu cech kursu: obowiązkowości, trudności, pory zajęć."
      ),
      list(
        title = "Rama: sprawiedliwość ocen",
        question = "Czy grupy prowadzących, np. według płci albo native speaker statusu, są oceniane podobnie?",
        use = "Dobra rama, jeśli interesuje nas równość i możliwe nierówne traktowanie.",
        danger = "Różnice grupowe wymagają ostrożnej interpretacji i kontroli kontekstu."
      )
    )
    items <- lapply(cards, function(x) {
      div(class = "question-card",
        h4(x$title),
        p(tags$strong("Pytanie: "), HTML(gsub("`([^`]+)`", "<code>\\1</code>", x$question))),
        p(tags$strong("Dlaczego warto: "), x$use),
        p(tags$strong("Ryzyko interpretacji: "), x$danger)
      )
    })
    div(class = "trop-stack", items)
  })
}
