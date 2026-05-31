ch1_ui <- lecture_chapter(id = "ch1", num = "1", title = "Od ciekawości do celu", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 01 · Start badania",
      num = "01",
      title = "Od ciekawości do celu.",
      lead = "Na początku nie potrzebujemy testu. Potrzebujemy ciekawości,
              jednego dobrego celu i wiązki tropów, którymi da się go oświetlić."
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
      h4("Pierwsze obserwacje"),
      uiOutput("ch1_snapshot")
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
      p("Tego pytania nie rozstrzygniemy jednym testem. Ono jest celem, do którego
        zbliżamy się wiązką tropów — kilkoma konkurującymi hipotezami, które razem
        oświetlają, co tak naprawdę siedzi w ocenie z ankiety.")
    ),

    lc_h2("sec-03", "Wiązka tropów, którą będziemy śledzić"),

    div(class = "lc-prose",
      p("Zamiast wybierać jeden trop, kładziemy na stół wszystkie naraz. To jest
        wiązka hipotez naszego projektu. Każdy trop pyta o inny możliwy składnik
        oceny; żaden sam nie odpowiada na cel, ale razem rysują obraz.")
    ),

    uiOutput("ch1_tropy_bundle"),

    div(class = "research-step",
      span(class = "step-number", "1"),
      "Dobra analiza zaczyna się od zdania: 'ciekawe, czy...', a potem od decyzji,
       który cel chcemy oświetlić i jakimi tropami. Statystyka przychodzi później."
    ),

    lc_h2("sec-04", "Tablica tropów — tu będziemy zbierać wyniki"),

    div(class = "lc-prose",
      p("Przez cały wykład będziemy wracać do jednej tablicy. Na razie jest pusta:
        mamy pytania, ale jeszcze żadnego kontaktu z danymi. W rozdziale 5 zaczniemy
        ją wypełniać, a w rozdziale 6 odczytamy, co cała wiązka mówi o celu.")
    ),

    div(class = "lc-figure-panel",
      h4("Tablica tropów (stan początkowy)"),
      tr_board_ui(reveal = character(0), show_verdict = TRUE)
    ),

    tr_discussion_box("Rozmowa na start:",
      tags$li("Który trop wydaje się studentom najbardziej przekonujący — i dlaczego?"),
      tags$li("Który byłby najważniejszy dla władz uczelni przy decyzjach kadrowych?"),
      tags$li("Który da się sprawdzić na tych danych, a który wymagałby nowych danych?")
    ),

    lc_chapter_next("02", "Jak obracać pytanie badawcze",
      "Ten sam cel można ująć w różnych ramach: obciążenie, trafność pomiaru, kontekst albo sprawiedliwość.",
      "ch2"),
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

  output$ch1_snapshot <- renderUI({
    lc_stat_grid(
      lc_stat_box("Kursów", nrow(tr_data), color = proj_col_data),
      lc_stat_box("Prowadzących", length(unique(tr_data$prof)), color = proj_col_hyp),
      lc_stat_box("Śr. ocena", round(mean(tr_data$eval), 2), color = proj_col_ctrl),
      lc_stat_box("Śr. response rate", paste0(round(mean(tr_data$response.rate), 1), "%"),
                  color = proj_col_warn),
      columns = 4
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
