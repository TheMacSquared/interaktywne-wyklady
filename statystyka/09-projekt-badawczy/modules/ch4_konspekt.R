ch4_ui <- lecture_chapter(id = "ch4", num = "4", title = "Konspekt pracy badawczej", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 04 · Konspekt",
      num = "04",
      title = "Konspekt pracy badawczej.",
      lead = "Po celu, tropach i operacjonalizacji możemy zapisać pełny plan
              badania: zmienne, hipotezy, alternatywne wyjaśnienia i sposób interpretacji."
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Przypomnienie celu:"),
      p(tags$em(tr_goal))
    ),

    lc_h2("sec-01", "Co musi znaleźć się w konspekcie?"),

    div(class = "lc-prose",
      p("Konspekt nie jest jeszcze raportem i nie jest listą testów. To roboczy
        plan badania. Powinien być na tyle konkretny, żeby po jego przeczytaniu
        było wiadomo, co dokładnie będziemy sprawdzać w danych.")
    ),

    div(class = "proposal-skeleton",
      div(class = "proposal-step",
        span(class = "proposal-step-num", "1"),
        div(
          h4("Cel badania"),
          p("Jedno główne pytanie, które porządkuje cały projekt."),
          div(class = "proposal-example",
            p(tags$strong("U nas: "), tags$em(tr_goal))
          )
        )
      ),
      div(class = "proposal-step",
        span(class = "proposal-step-num", "2"),
        div(
          h4("Zmienne, dane i pomiar"),
          p("Źródło danych, jednostka obserwacji, zmienna wynikowa, zmienne główne,
            zmienne kontekstowe oraz ograniczenia pomiaru."),
          div(class = "proposal-example",
            p(tags$strong("U nas: "), "jedna obserwacja to kurs/ewaluacja; mamy oceny,
              cechy prowadzących i cechy kursów. ", tags$code("eval"), " jest oceną z ankiety,
              ale nie jest czystą miarą jakości nauczania.")
          )
        )
      ),
      div(class = "proposal-step",
        span(class = "proposal-step-num", "3"),
        div(
          h4("Tropy i hipotezy"),
          p("Każdy trop zapisujemy w tym samym porządku: pytanie, hipoteza,
            zmienne do użycia, alternatywne wyjaśnienia i plan interpretacji."),
          div(class = "proposal-example",
            p(tags$strong("U nas: "), "atrakcyjność, płeć, status native speaker,
              status mniejszościowy i response rate jako różne tropy interpretacji ", tags$code("eval"), ".")
          )
        )
      ),
      div(class = "proposal-step",
        span(class = "proposal-step-num", "4"),
        div(
          h4("Plan interpretacji"),
          p("Co opiszemy, co porównamy, które zmienne uwzględnimy jako kontekst
            i jak ostrożnie połączymy wyniki z celem badania.")
        )
      )
    ),

    lc_h2("sec-02", "Wypełniony konspekt dla naszych danych"),

    div(class = "lc-figure-panel",
      h4("Konspekt roboczy"),
      uiOutput("ch4_full_proposal")
    ),

    lc_h2("sec-03", "Konspekt własnej pracy"),

    div(class = "lc-figure-panel",
      h4("Wypełnij konspekt"),
      div(class = "lc-prose",
        p("Wypełnijcie pola roboczo. To nie musi być piękny tekst, ale musi być
          konkretne: cel, zmienne, tropy i plan powinny pasować do siebie.")
      ),
      div(class = "proposal-draft-grid",
        textAreaInput("ch4_goal", "Cel badania", height = "120px",
          placeholder = "Chcemy sprawdzić, czy..."),
        textAreaInput("ch4_variables", "Zmienne, dane i pomiar", height = "150px",
          placeholder = "Jednostką obserwacji jest... Zmienna wynikowa to... Zmienne główne to... Ograniczenia pomiaru..."),
        textAreaInput("ch4_hypotheses", "Tropy / hipotezy", height = "150px",
          placeholder = "Trop 1: ... Hipoteza: ... Alternatywne wyjaśnienia: ..."),
        textAreaInput("ch4_plan", "Plan interpretacji", height = "150px",
          placeholder = "Najpierw opiszemy... Następnie porównamy... Uwzględnimy... Wynik zinterpretujemy ostrożnie, bo...")
      ),
      uiOutput("ch4_proposal_preview")
    ),

    lc_chapter_next("05", "Pierwsze sprawdzenia w danych",
      "Dopiero teraz wybieramy testy i wykresy, bo mamy pełny konspekt badania.",
      "ch5"),
    div(style = "height: 40px;")
  )))
)

ch4_server <- function(input, output, session) {
  code_html <- function(x) {
    HTML(gsub("`([^`]+)`", "<code>\\1</code>", x))
  }

  output$ch4_full_proposal <- renderUI({
    variable_rows <- list(
      list(
        role = "Zmienna wynikowa",
        vars = "`eval`",
        meaning = "Ogólna ocena kursu w ankiecie studenckiej; główny wynik, który interpretujemy.",
        caveat = "Nie jest bezpośrednim pomiarem jakości nauczania. Może mieszać satysfakcję, sympatię, łatwość kursu i oczekiwania studentów."
      ),
      list(
        role = "Główne tropy",
        vars = "`beauty`, `gender`, `native`, `minority`, `response.rate`",
        meaning = "Zmienne, które pozwalają sprawdzić różne możliwe źródła ocen z ankiety.",
        caveat = "Każda z nich jest tylko wskaźnikiem szerszego zjawiska, więc wymaga alternatywnych wyjaśnień."
      ),
      list(
        role = "Kontekst kursu",
        vars = "`division`, `credits`, `students`, `allstudents`",
        meaning = "Informacje o poziomie kursu, liczbie punktów i wielkości grupy.",
        caveat = "Mogą zmieniać interpretację ocen i response rate; nie są pełnym opisem trudności lub organizacji zajęć."
      ),
      list(
        role = "Cechy prowadzącego",
        vars = "`age`, `tenure`, `prof`",
        meaning = "Dodatkowe informacje o prowadzącym, przydatne jako kontekst albo możliwe wyjaśnienia poboczne.",
        caveat = "Nie mierzą stylu prowadzenia, przygotowania dydaktycznego ani relacji ze studentami."
      )
    )

    variable_table <- tags$table(class = "lc-table lc-table-bordered proposal-variable-table",
      tags$thead(tags$tr(
        tags$th("Rola w konspekcie"),
        tags$th("Zmienne"),
        tags$th("Co opisują"),
        tags$th("Ograniczenie pomiaru")
      )),
      tags$tbody(lapply(variable_rows, function(row) {
        tags$tr(
          tags$td(tags$strong(row$role)),
          tags$td(code_html(row$vars)),
          tags$td(row$meaning),
          tags$td(row$caveat)
        )
      }))
    )

    trop_cards <- lapply(tr_trop_order, function(id) {
      tr <- tr_tropy[[id]]
      div(class = "proposal-trop",
        h5(paste0("Trop: ", tr$short)),
        p(tags$strong("Pytanie badawcze: "), tr$question),
        p(tags$strong("Hipoteza robocza: "), code_html(tr$hypothesis)),
        p(tags$strong("Zmienne do użycia: "),
          "wynik: ", tags$code("eval"), "; trop: ", tags$code(tr$var), "."),
        p(tags$strong("Dostępne dane i braki: "), code_html(tr$data_check)),
        tags$strong("Alternatywne wyjaśnienia:"),
        tags$ul(lapply(tr$alt, tags$li)),
        p(tags$strong("Co uwzględnić w analizie: "), code_html(tr$plan_check))
      )
    })

    tagList(
      div(class = "proposal-preview",
        h4("1. Cel badania"),
        p("Sprawdzić, czy ocena z ankiety ", tags$code("eval"),
          " mierzy jakość nauczania, czy raczej miesza jakość zajęć, sympatię,
          stereotypy i okoliczności kursu.")
      ),
      div(class = "proposal-preview",
        h4("2. Zmienne, dane i pomiar"),
        p("Jednostką obserwacji jest kurs/ewaluacja. Dane zawierają oceny studenckie,
          cechy prowadzących i kilka informacji o kontekście kursu."),
        variable_table
      ),
      div(class = "proposal-preview",
        h4("3. Tropy i hipotezy"),
        p("Poniższe pola są częścią konspektu. Każdy trop dotyczy innego możliwego
          składnika oceny z ankiety; dopiero razem tworzą plan badania."),
        div(class = "proposal-trop-list", trop_cards)
      ),
      div(class = "proposal-preview",
        h4("4. Plan interpretacji"),
        tags$ol(
          tags$li("Najpierw opiszemy zmienną wynikową i najważniejsze zmienne z tropów."),
          tags$li("Następnie sprawdzimy każdy trop osobno: czy sugeruje związek z ", tags$code("eval"), "."),
          tags$li("Przy każdym tropie zapiszemy alternatywne wyjaśnienia i sprawdzimy, czy mamy dane, żeby je uwzględnić."),
          tags$li("Na końcu zestawimy tropy razem, żeby ocenić, co cała wiązka mówi o celu."),
          tags$li("Wniosek sformułujemy ostrożnie: dane obserwacyjne wspierają interpretację, ale nie dowodzą przyczynowości.")
        )
      )
    )
  })

  output$ch4_proposal_preview <- renderUI({
    clean <- function(x) {
      if (is.null(x)) "" else trimws(x)
    }
    fields <- list(
      "Cel" = input$ch4_goal,
      "Zmienne, dane i pomiar" = input$ch4_variables,
      "Tropy" = input$ch4_hypotheses,
      "Plan interpretacji" = input$ch4_plan
    )
    filled <- Filter(function(x) nzchar(clean(x)), fields)

    if (length(filled) == 0) {
      return(div(class = "proposal-preview",
        h4("Podgląd konspektu"),
        p("Wpiszcie roboczą wersję każdej części. Tu pojawi się konspekt projektu.")
      ))
    }

    div(class = "proposal-preview",
      h4("Podgląd konspektu"),
      tags$ol(lapply(names(fields), function(label) {
        value <- clean(fields[[label]])
        tags$li(tags$strong(paste0(label, ": ")),
          if (nzchar(value)) value else tags$span(class = "tropy-muted", "do uzupełnienia"))
      }))
    )
  })
}
