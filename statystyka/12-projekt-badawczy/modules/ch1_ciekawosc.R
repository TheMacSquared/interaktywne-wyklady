ch1_ui <- lecture_chapter(id = "ch1", num = "1", title = "Od ciekawości do pytania", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 01 · Start badania",
      num = "01",
      title = "Od ciekawości do pytania.",
      lead = "Na początku nie potrzebujemy testu. Potrzebujemy ciekawości,
              dobrych podejrzeń i języka, którym da się opisać problem."
    ),

    lc_h2("sec-01", "Zaczynamy od sytuacji, nie od metody"),

    div(class = "lc-prose",
      p("Mamy dane o 463 kursach: oceny studenckie, cechy prowadzących i kilka
        informacji o kontekście zajęć. To nie jest jeszcze projekt badawczy.
        To dopiero materiał, z którego można zbudować kilka różnych historii."),
      p("Dzisiejsze ćwiczenie polega na tym, żeby zobaczyć, jak z luźnej ciekawości
        powstaje pytanie, z pytania hipoteza, a z hipotezy plan analizy.")
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

    div(class = "lc-figure-panel",
      h4("Co nas zaciekawiło?"),
      checkboxGroupInput("ch1_curiosity", NULL,
        choices = c(
          "Czy ewaluacje mierzą jakość, czy sympatię?" = "quality",
          "Czy wygląd prowadzącego może wpływać na ocenę?" = "beauty",
          "Czy niektóre grupy prowadzących są oceniane inaczej?" = "fairness",
          "Czy typ kursu zmienia sposób oceniania?" = "context",
          "Czy odpowiedzieli reprezentatywni studenci?" = "response"
        ),
        selected = c("quality", "beauty")
      ),
      uiOutput("ch1_curiosity_prompt"),
      actionButton("ch1_show_main_question", "Pokaż główne pytanie",
                   class = "lc-btn-secondary"),
      uiOutput("ch1_main_question")
    ),

    div(class = "research-step",
      span(class = "step-number", "1"),
      "Dobra analiza często zaczyna się od zdania: 'ciekawe, czy...'.
       Dopiero później pytamy, jaką statystyką da się to sprawdzić."
    ),

    tr_discussion_box("Rozmowa na start:",
      tags$li("Które z tych pytań jest najbardziej interesujące dla studentów?"),
      tags$li("Które byłoby ważne dla władz uczelni?"),
      tags$li("Które jest najłatwiej sprawdzić na tych danych, a które wymagałoby nowych danych?")
    ),

    lc_chapter_next("02", "Jak obracać pytanie badawcze",
      "To samo zjawisko można opowiedzieć jako obciążenie, trafność pomiaru, kontekst albo sprawiedliwość.",
      "ch2"),
    div(style = "height: 40px;")
  )))
)

ch1_server <- function(input, output, session) {
  output$ch1_main_question <- renderUI({
    if (is.null(input$ch1_show_main_question) || input$ch1_show_main_question == 0) {
      return(NULL)
    }
    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Główne pytanie / główna teza:"),
      p(tags$em("Czy ocena z ankiety naprawdę mówi, kto dobrze uczy, czy raczej
        pokazuje mieszankę jakości zajęć, sympatii, stereotypów i okoliczności kursu?"))
    )
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

  output$ch1_curiosity_prompt <- renderUI({
    n <- length(input$ch1_curiosity)
    if (n == 0) {
      return(div(class = "lc-feedback lc-feedback-warning",
        "Wybierz przynajmniej jeden trop. Bez ciekawości nie ma projektu."
      ))
    }
    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Teraz zamieńcie ciekawość w pytanie:"),
      p("Nie: \"zrobię test\". Tak: \"chcę zrozumieć, czy i dlaczego...\""),
      p("Wybrane tropy: ", tags$strong(n), ". Spróbujcie ułożyć z nich jedno główne pytanie.")
    )
  })
}
