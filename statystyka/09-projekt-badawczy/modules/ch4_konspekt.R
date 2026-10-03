ch4_ui <- lecture_chapter(id = "ch4", num = "4", title = "Konspekt pracy badawczej", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 04 · Konspekt",
      num = "04",
      title = "Konspekt pracy badawczej.",
      lead = "Po celu, tropach i pomiarze możemy zapisać pełny plan
              badania: zmienne, hipotezy, alternatywne wyjaśnienia i sposób interpretacji."
    ),

    lc_h2("sec-01", "Cztery części konspektu"),

    lc_p("Rozdziały 1–3 dały trzy elementy projektu: cel badania, wiązkę
      tropów z alternatywnymi wyjaśnieniami i opis tego, co naprawdę mierzą
      zmienne. Konspekt składa je w jeden dokument, który powstaje przed
      analizą. Nie jest jeszcze raportem ani listą testów. To plan, po którego
      przeczytaniu wiadomo, co dokładnie będzie sprawdzane w danych i jak
      zostaną odczytane wyniki."),

    lc_p("Kolejność ma znaczenie. Gdy pytania i sposób interpretacji zapisuje
      się przed obejrzeniem wyników, trudniej ulec pokusie, żeby po fakcie
      wybrać te porównania, które akurat wyszły. Przy wielu tropach
      sprawdzanych naraz łatwo bowiem o przypadkowo istotny wynik: to problem ",
      gloss("porównania wielokrotne", "porównań wielokrotnych"), " znany
      z wykładu 04 (rozdział 9). Konspekt ma cztery części; przy każdej podajemy, jak
      wygląda ona w naszym projekcie."),

    div(class = "proposal-skeleton",
      div(class = "proposal-step",
        span(class = "proposal-step-num", "1"),
        div(
          h4("Cel badania"),
          p("Jedno główne ", gloss("pytanie badawcze", "pytanie"), ", które porządkuje cały projekt."),
          div(class = "proposal-example",
            p(tags$strong("U nas:"), " ", tags$em(tr_goal))
          )
        )
      ),
      div(class = "proposal-step",
        span(class = "proposal-step-num", "2"),
        div(
          h4("Zmienne, dane i pomiar"),
          p("Źródło danych, ", gloss("jednostka obserwacji"), ", ", gloss("zmienna zależna", "zmienna wynikowa"), ", zmienne główne,
            zmienne kontekstowe oraz ograniczenia pomiaru."),
          div(class = "proposal-example",
            p(tags$strong("U nas:"), " ", "jedna obserwacja to kurs; mamy oceny,
              cechy prowadzących i cechy kursów. ", tags$code("eval"), " jest oceną z ankiety,
              ale nie jest czystą miarą jakości nauczania.")
          )
        )
      ),
      div(class = "proposal-step",
        span(class = "proposal-step-num", "3"),
        div(
          h4("Tropy i hipotezy"),
          p("Każdy trop zapisujemy w tym samym porządku: pytanie, ", gloss("hipoteza badawcza", "hipoteza"), ",
            zmienne do użycia, alternatywne wyjaśnienia i plan interpretacji."),
          div(class = "proposal-example",
            p(tags$strong("U nas:"), " ", "atrakcyjność, płeć, status native speaker,
              status mniejszościowy i odsetek odpowiedzi (response rate) jako
              różne tropy interpretacji ", tags$code("eval"), ".")
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

    lc_p("Pierwsze trzy części opisują, co badamy. Czwarta mówi, jak będziemy
      czytać wyniki, i jest najczęściej pomijana, choć to ona chroni przed
      wnioskami mocniejszymi, niż pozwalają dane."),

    lc_h2("sec-02", "Wypełniony konspekt dla naszych danych"),

    lc_p("Tak wygląda kompletny konspekt naszego projektu. Część o zmiennych
      porządkuje kolumny tabeli według ról: zmienna wynikowa, zmienne
      tropów, kontekst kursu i pozostałe cechy prowadzącego. Przy każdej
      roli zapisano ograniczenie pomiaru z rozdziału 3. Karty tropów
      powtarzają układ z rozdziału 2 i dodają listę zmiennych do użycia."),

    figure_panel(
      label = "Ryc. 4.1",
      title = "Konspekt roboczy",
      uiOutput("ch4_full_proposal")
    ),

    lc_p("W konspekcie nie ma nazw testów ani żadnych wyników. Nie ma ich
      celowo: test dobiera się do typu zmiennych i do pytania, a to
      zrobimy dopiero w rozdziale 5. Jest za to plan interpretacji, który
      z góry ustala kolejność pracy: najpierw opis, potem każdy trop osobno,
      potem alternatywne wyjaśnienia, a na końcu zestawienie wszystkich
      tropów. Ostatni punkt planu przesądza też o formie wniosku. Mamy ",
      gloss("dane obserwacyjne", "dane obserwacyjne"), ", więc niezależnie
      od wyników nie pozwolą one stwierdzić,
      że któraś cecha prowadzącego powoduje wyższe oceny."),

    lc_h2("sec-03", "Konspekt własnej pracy"),

    lc_p("Ten sam schemat służy do planowania własnego projektu. W panelu
      są cztery pola odpowiadające czterem częściom konspektu. Wystarczy
      wersja robocza, ale konkretna: cel, zmienne, tropy i plan powinny do
      siebie pasować. Pod polami pojawia się podgląd całości. Wpisany tekst
      nie jest nigdzie zapisywany, więc przed zamknięciem strony warto
      go skopiować."),

    figure_panel(
      label = "Ćwiczenie",
      title = "Wypełnij konspekt",
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

    lc_p("Gotowy konspekt warto przeczytać tak, jakby napisał go ktoś inny.
      Czy da się z niego odtworzyć, jakie zmienne zostaną porównane? Czy
      przy każdej hipotezie jest alternatywne wyjaśnienie? Czy plan
      interpretacji mówi, co zrobimy, jeśli wynik wyjdzie inaczej, niż
      zakładamy? Konspekt wróci w rozdziale 8, gdzie dopiszemy do niego
      wyniki."),

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
        meaning = "Poziom kursu, to, czy jest jednopunktowym kursem fakultatywnym, oraz liczba odpowiedzi i zapisanych.",
        caveat = "Mogą zmieniać interpretację ocen i odsetka odpowiedzi; nie są pełnym opisem trudności lub organizacji zajęć."
      ),
      list(
        role = "Cechy prowadzącego",
        vars = "`age`, `tenure`, `prof`",
        meaning = "Dodatkowe informacje o prowadzącym, przydatne jako kontekst albo możliwe wyjaśnienia poboczne.",
        caveat = "Nie mierzą stylu prowadzenia, przygotowania dydaktycznego ani relacji ze studentami."
      )
    )

    variable_df <- data.frame(
      role = vapply(variable_rows, `[[`, character(1), "role"),
      meaning = vapply(variable_rows, `[[`, character(1), "meaning"),
      caveat = vapply(variable_rows, `[[`, character(1), "caveat"),
      stringsAsFactors = FALSE
    )
    variable_df$vars <- I(lapply(variable_rows, function(row) code_html(row$vars)))
    variable_table <- lc_table(variable_df,
      cols = list(
        lc_col("role", "Rola w konspekcie", "row"),
        lc_col("vars", "Zmienne", "text"),
        lc_col("meaning", "Co opisują", "text"),
        lc_col("caveat", "Ograniczenie pomiaru", "text")
      ),
      narrow = "cards"
    )

    trop_cards <- lapply(tr_trop_order, function(id) {
      tr <- tr_tropy[[id]]
      div(class = "proposal-trop",
        h5(paste0("Trop: ", tr$short)),
        p(tags$strong("Pytanie badawcze:"), " ", tr$question),
        p(tags$strong("Hipoteza robocza:"), " ", code_html(tr$hypothesis)),
        p(tags$strong("Zmienne do użycia:"), " ",
          "wynik: ", tags$code("eval"), "; trop: ", tags$code(tr$var), "."),
        p(tags$strong("Dostępne dane i braki:"), " ", code_html(tr$data_check)),
        tags$strong("Alternatywne wyjaśnienia:"),
        tags$ul(lapply(tr$alt, tags$li)),
        p(tags$strong("Co uwzględnić w analizie:"), " ", code_html(tr$plan_check))
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
        p("Jednostką obserwacji jest kurs (463 kursy, 94 prowadzących). Dane zawierają oceny studenckie,
          cechy prowadzących i kilka informacji o kontekście kursu."),
        variable_table
      ),
      div(class = "proposal-preview",
        h4("3. Tropy i hipotezy"),
        p("Pięć tropów; każdy dotyczy innego możliwego składnika oceny z ankiety."),
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
        p("Wpisz roboczą wersję każdej części. Tu pojawi się konspekt projektu.")
      ))
    }

    div(class = "proposal-preview",
      h4("Podgląd konspektu"),
      tags$ol(lapply(names(fields), function(label) {
        value <- clean(fields[[label]])
        tags$li(tags$strong(paste0(label, ":")), " ",
          if (nzchar(value)) value else tags$span(class = "tropy-muted", "do uzupełnienia"))
      }))
    )
  })
}
