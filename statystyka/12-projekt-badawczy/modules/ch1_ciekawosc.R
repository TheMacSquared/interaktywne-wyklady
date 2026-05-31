ch1_ui <- lecture_chapter(id = "ch1", num = "1", title = "Od ciekawości do celu", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 01 · Start badania",
      num = "01",
      title = "Od ciekawości do celu.",
      lead = "Projekt badawczy zaczyna się przed testem: od celu i zestawu
              konkurujących tropów, które ten cel pomogą wyjaśnić."
    ),

    lc_h2("sec-01", "Zaczynamy od sytuacji, nie od metody"),

    div(class = "lc-prose",
      p("Mamy dane o 463 kursach: oceny studenckie, cechy prowadzących i kilka
        informacji o kontekście zajęć. To nie jest jeszcze projekt badawczy.
        To dopiero materiał, z którego można zbudować kilka różnych historii."),
      p("Dzisiejsze ćwiczenie polega na tym, żeby zobaczyć, jak z luźnej ciekawości
        powstaje jeden cel badawczy, z celu wiązka hipotez, a z hipotez plan analizy.
        Ten sam cel i tę samą wiązkę będziemy ciągnąć przez cały wykład.")
    ),

    div(class = "lc-figure-panel",
      h4("Podgląd danych"),
      div(class = "lc-prose",
        p("Zanim zaczniemy formułować hipotezy, zobaczmy samą tabelę:
          co jest jedną obserwacją, jakie są typy zmiennych i czego w danych
          nie widać.")
      ),
      fluidRow(
        column(4,
          numericInput("ch1_row_from", "Pokaż od wiersza:", value = 1,
                       min = 1, max = nrow(tr_data), step = 10)
        ),
        column(4,
          selectInput("ch1_col_view", "Zakres kolumn:",
            choices = c("Kluczowe zmienne" = "key", "Wszystkie zmienne" = "all"),
            selected = "key"
          )
        ),
        column(4,
          selectInput("ch1_sort_var", "Sortuj wg:",
            choices = setNames(
              c("none", "eval", "beauty", "response.rate", "age"),
              c("bez sortowania", tr_labels[c("eval", "beauty", "response.rate", "age")])
            ),
            selected = "none"
          )
        )
      ),
      div(style = "overflow-x: auto;", tableOutput("ch1_data_view")),
      uiOutput("ch1_data_legend")
    ),

    lc_h2("sec-02", "Nasz cel na cały wykład"),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Cel badawczy:"),
      p(tags$em(tr_goal)),
      p("Tego pytania nie rozstrzyga pojedynczy test. To cel, do którego dochodzimy
        przez zestaw konkurujących hipotez (tropów). Każdy trop dotyczy innego
        możliwego składnika oceny z ankiety; dopiero razem pozwalają ocenić cel.")
    ),

    div(class = "lc-prose",
      p("Cel powstaje przez zawężanie: od ogólnego tematu, przez konkretne pytanie,
        do planu analizy.")
    ),

    div(class = "research-ladder",
      div(tags$strong("Luźny temat"), "Studenci różnie oceniają prowadzących."),
      div(tags$strong("Cel badawczy"), "Czy ta różnica mówi o jakości, czy o czymś pobocznym?"),
      div(tags$strong("Plan analizy"), "Najpierw szukamy prostych tropów, potem alternatywnych wyjaśnień.")
    ),

    lc_h2("sec-03", "Wiązka tropów, którą będziemy śledzić"),

    div(class = "lc-prose",
      p("Rozważamy wszystkie tropy naraz, a nie pojedynczo. Tworzą one wiązkę
        hipotez projektu. Każdy trop dotyczy innego możliwego składnika oceny;
        żaden nie odpowiada na cel samodzielnie, ale razem dają pełniejszy obraz.")
    ),

    uiOutput("ch1_tropy_bundle"),

    div(class = "research-step",
      span(class = "step-number", "1"),
      "Kolejność pracy: najpierw cel i tropy, potem dobór metody statystycznej.
       Test jest narzędziem do sprawdzenia tropu, nie punktem wyjścia."
    ),

    lc_h2("sec-04", "Tablica tropów — tu będziemy zbierać wyniki"),

    div(class = "lc-prose",
      p("Przez cały wykład będziemy wracać do jednej tablicy. Na razie jest pusta:
        mamy pytania, ale jeszcze żadnego kontaktu z danymi. W rozdziale 4 zaczniemy
        ją wypełniać, a w rozdziale 5 odczytamy, co cała wiązka mówi o celu.")
    ),

    div(class = "lc-figure-panel",
      h4("Tablica tropów (stan początkowy)"),
      tr_board_ui(reveal = character(0), show_verdict = TRUE)
    ),

    lc_h2("sec-05", "Tropy poza naszą wiązką"),

    div(class = "lc-prose",
      p("Pięć tropów to nasz wybór na dziś, nie pełna lista. Ten sam cel można
        badać wieloma innymi pytaniami — poniżej kilka przykładów. Zasadą przy
        własnym projekcie jest dążenie do wyczerpania tematu, a nie zatrzymanie
        się na pierwszych tropach.")
    ),

    uiOutput("ch1_extra_tropy"),

    lc_chapter_next("02", "Hipotezy jako tropy",
      "Mamy cel i wiązkę. Teraz nadajemy każdemu tropowi kształt hipotezy z alternatywnymi wyjaśnieniami.",
      "ch3"),
    div(style = "height: 40px;")
  )))
)

ch1_server <- function(input, output, session) {
  output$ch1_tropy_bundle <- renderUI({
    cards <- lapply(tr_trop_order, function(id) {
      tr <- tr_tropy[[id]]
      div(class = "trop-card",
        h4(tr$short),
        p(tags$strong("Pytanie: "), tr$question)
      )
    })
    div(class = "trop-stack", cards)
  })

  output$ch1_extra_tropy <- renderUI({
    extra <- list(
      c("Wielkość kursu", "Czy bardzo duże grupy są oceniane inaczej niż kameralne?"),
      c("Pora i dzień zajęć", "Czy zajęcia o poranku albo w piątek dostają niższe oceny?"),
      c("Trudność i obciążenie", "Czy łatwiejsze kursy dostają wyższe oceny niezależnie od jakości?"),
      c("Dyscyplina / wydział", "Czy kursy ścisłe są oceniane surowiej niż humanistyczne?"),
      c("Powtarzalność prowadzącego", "Czy ten sam prowadzący dostaje podobne oceny na różnych kursach?")
    )
    rows <- lapply(extra, function(x) {
      tags$tr(
        tags$td(tags$strong(x[[1]])),
        tags$td(x[[2]])
      )
    })
    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      tags$thead(tags$tr(tags$th("Trop"), tags$th("Przykładowe pytanie"))),
      tags$tbody(rows)
    )
  })

  output$ch1_data_view <- renderTable({
    key_cols <- c(
      "eval", "beauty", "gender", "age", "minority", "native",
      "division", "credits", "students", "allstudents",
      "response.rate"
    )
    cols <- if (identical(input$ch1_col_view, "all")) names(tr_data) else key_cols
    show <- tr_data[, cols, drop = FALSE]

    if (!is.null(input$ch1_sort_var) && !identical(input$ch1_sort_var, "none")) {
      show <- show[order(show[[input$ch1_sort_var]], decreasing = TRUE), , drop = FALSE]
    }

    start <- min(max(1, input$ch1_row_from), nrow(show))
    idx <- start:min(nrow(show), start + 9)
    out <- show[idx, , drop = FALSE]
    num_cols <- vapply(out, is.numeric, logical(1))
    out[num_cols] <- lapply(out[num_cols], round, 2)
    out
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%", rownames = TRUE)

  output$ch1_data_legend <- renderUI({
    items <- list(
      c("eval", "ogólna ocena kursu w ewaluacji studenckiej; główny wynik, ale nie idealna miara jakości"),
      c("beauty", "ocena atrakcyjności prowadzącego; główny trop dotyczący możliwego obciążenia ocen"),
      c("gender, age", "cechy prowadzącego, które mogą współwystępować z ocenami"),
      c("minority", "czy prowadzący należy do grupy mniejszościowej; kluczowa zmienna dla pytań o obciążenie i sprawiedliwość ocen"),
      c("native, tenure", "status językowy i zawodowy prowadzącego; potencjalne alternatywne wyjaśnienia"),
      c("division, credits", "cechy kursu: poziom i liczba punktów; kontekst, który może zmieniać oczekiwania"),
      c("students, allstudents", "liczba odpowiedzi i liczba zapisanych studentów"),
      c("response.rate", "odsetek studentów, którzy wypełnili ankietę; ważny dla reprezentatywności opinii"),
      c("prof", "identyfikator prowadzącego; jedna osoba może pojawiać się przy więcej niż jednym kursie")
    )
    tagList(
      lc_feedback(
        tags$p(tags$strong("Jak czytać tabelę?")),
        tags$p("Jedna obserwacja to kurs/ewaluacja. Już na tym etapie pytamy:
          które kolumny są wynikiem, które są tropem, a które mogą zmieniać interpretację?"),
        type = "info"
      ),
      div(class = "data-legend",
        lapply(items, function(x) {
          div(class = "data-legend-item",
            tags$code(x[[1]]),
            tags$br(),
            x[[2]]
          )
        })
      )
    )
  })
}
