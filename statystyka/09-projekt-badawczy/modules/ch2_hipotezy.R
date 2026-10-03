ch2_ui <- lecture_chapter(id = "ch2", num = "2", title = "Hipotezy jako tropy", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 02 · Hipotezy",
      num = "02",
      title = "Hipotezy jako tropy.",
      lead = "Hipoteza badawcza to robocze przypuszczenie o związku między
              zmiennymi. Można ją zawęzić, przeformułować albo porzucić, gdy
              dane jej nie wspierają, a każda ma konkurentów, którzy
              wyjaśniliby ten sam wynik inaczej."
    ),

    lc_h2("sec-01", "Od pytania do hipotezy badawczej"),

    lc_p("W rozdziale 1 postawiliśmy cel: ", tags$em(tr_goal), " Do celu
      dobraliśmy pięć tropów, każdy w postaci ",
      gloss("pytanie badawcze", "pytania badawczego"), ". Pytanie wskazuje
      kierunek, ale nie mówi, jaki wynik byłby dla tropu korzystny, a jaki
      nie. Potrzebna jest więc ",
      gloss("hipoteza badawcza", "hipoteza badawcza"), ": zdanie twierdzące
      o związku między konkretnymi zmiennymi, które dane mogą wesprzeć albo
      osłabić. Z pytania „czy atrakcyjniejsi prowadzący dostają wyższe
      oceny?” robi się hipoteza „wyższe beauty współwystępuje z wyższym
      eval”."),

    lc_p("Hipoteza badawcza jest sformułowana słowami i dotyczy zjawiska.
      W wykładzie 04 (rozdział 2) zamienialiśmy takie zdania na parę hipotez
      statystycznych: zerową, która mówi o braku związku, i alternatywną.
      Ten krok wykonamy w rozdziale 5, przy wyborze testu. Na razie
      porządkujemy projekt, a nie liczymy."),

    lc_h2("sec-02", "Alternatywne wyjaśnienia"),

    lc_p("Nawet jeśli dane wesprą hipotezę, nie musi to oznaczać, że trop jest
      prawdziwy w tej postaci, w jakiej go zapisaliśmy. Ten sam wynik może
      dawać inny mechanizm. Takie konkurencyjne mechanizmy nazywamy
      alternatywnymi wyjaśnieniami. Jeśli na przykład młodsi prowadzący są
      oceniani jako atrakcyjniejsi i jednocześnie dostają wyższe oceny,
      związek wyglądu z oceną może być w części związkiem wieku z oceną.
      Zmienną, która w ten sposób wiąże się jednocześnie z badaną cechą
      i z wynikiem, nazywaliśmy w wykładzie 06 ",
      gloss("zmienna zakłócająca", "zmienną zakłócającą"), "."),

    lc_p("Każde alternatywne wyjaśnienie rodzi pytanie praktyczne: czy mamy
      dane, które pozwolą je odróżnić od głównego tropu? Wiek prowadzącego
      jest w tabeli, więc tę alternatywę da się sprawdzić. Pewności siebie
      prowadzącego w tabeli nie ma, więc tej alternatywy nie wykluczymy
      żadnym rachunkiem. Brak danych nie przekreśla projektu, ale musi
      trafić do konspektu jako ograniczenie."),

    inline_callout(label = "Zasada",
      "Do każdej hipotezy zapisz co najmniej jedno alternatywne wyjaśnienie
       i sprawdź, czy dane pozwalają je odróżnić od głównego tropu. Jeśli
       nie pozwalają, zapisz to jako ograniczenie."
    ),

    lc_h2("sec-03", "Cała wiązka jako hipotezy"),

    lc_p("Poniżej wszystkie pięć tropów zapisanych w tym samym układzie:
      pytanie, hipoteza robocza, alternatywne wyjaśnienia, dostępne dane
      i to, co trzeba będzie uwzględnić w analizie. Dwa ostatnie pola są
      zalążkiem konspektu, który złożymy w rozdziale 4."),

    figure_panel(
      label = "Ryc. 2.1",
      title = "Wiązka tropów zapisana jako hipotezy",
      uiOutput("ch2_bundle")
    ),

    lc_p("W każdym tropie brakuje w danych czegoś, co pomogłoby rozstrzygnąć
      między hipotezą a alternatywą: miary stylu prowadzenia, oczekiwań
      studentów, języka prowadzenia zajęć, treści komentarzy albo informacji
      o tym, kto nie wypełnił ankiety. Te braki nie znikną po żadnym teście
      i trafią do wniosku jako ograniczenia."),

    lc_p("Widać też, że alternatywy się powtarzają. Kurs, czyli jego typ,
      poziom albo wielkość, pojawia się jako możliwe wyjaśnienie we wszystkich
      pięciu tropach, a płeć jest jednocześnie osobnym tropem i alternatywą
      dla tropu atrakcyjności. Tropy nie są więc od siebie niezależne. Na
      razie sprawdzimy je osobno, ale to nakładanie się wróci w rozdziale 6,
      a w rozdziale 7 doprowadzi do jednego modelu, w którym wszystkie zmienne
      występują razem."),

    lc_chapter_next("03", "Pomiar",
      "Sprawdzamy, czy pojęcia z hipotez naprawdę mają odpowiedniki w danych.",
      "ch3"),
    div(style = "height: 40px;")
  )))
)

ch2_server <- function(input, output, session) {
  output$ch2_bundle <- renderUI({
    cards <- lapply(tr_trop_order, function(id) {
      tr <- tr_tropy[[id]]
      div(class = "trop-card",
        h4(tr$short),
        p(tags$strong("Pytanie:"), " ", tr$question),
        p(tags$strong("Robocza hipoteza:"), " ",
          HTML(gsub("`([^`]+)`", "<code>\\1</code>", tr$hypothesis))),
        p(tags$strong("Alternatywne wyjaśnienia:")),
        tags$ul(class = "trop-alt", lapply(tr$alt, tags$li)),
        div(class = "trop-plan-grid",
          div(class = "trop-plan-box",
            tags$strong("Dostępne dane i braki:"),
            p(HTML(gsub("`([^`]+)`", "<code>\\1</code>", tr$data_check)))
          ),
          div(class = "trop-plan-box",
            tags$strong("Co uwzględnić w analizie:"),
            p(HTML(gsub("`([^`]+)`", "<code>\\1</code>", tr$plan_check)))
          )
        )
      )
    })
    div(class = "trop-stack", cards)
  })
}
