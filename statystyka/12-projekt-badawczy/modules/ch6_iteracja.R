ch6_ui <- lecture_chapter(id = "ch6", num = "5", title = "Wynik nie kończy badania", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 05 · Iteracja",
      num = "05",
      title = "Wynik nie kończy badania.",
      lead = "Tablica tropów jest kompletna. Pozostaje odczytać, co cały zestaw
              wyników mówi o celu i jakie pytania wynikają z niego dalej."
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Przypomnienie celu:"),
      p(tags$em(tr_goal))
    ),

    lc_h2("sec-01", "Cała wiązka naraz: co wiemy o celu?"),

    div(class = "lc-prose",
      p("Pojedynczy wynik nie odpowiada na cel. Dopiero zebrane razem tropy
        zaczynają coś mówić: część się wzmocniła, część osłabła. To jest moment,
        w którym z osobnych testów składamy jeden obraz.")
    ),

    div(class = "lc-figure-panel",
      h4("Tablica tropów — pełny obraz"),
      tr_board_ui(reveal = tr_trop_order, show_verdict = TRUE)
    ),

    uiOutput("ch6_goal_readout"),

    lc_h2("sec-02", "Co robimy z każdym tropem?"),

    div(class = "lc-prose",
      p("Werdykt „wzmocniony\" nie znaczy „udowodniony\", a „osłabiony\" nie znaczy
        „temat zamknięty\". Każdy trop generuje następne pytania — poniżej dla
        całej wiązki naraz.")
    ),

    uiOutput("ch6_next_steps"),

    lc_h2("sec-03", "Zanim zaufamy wynikowi: zmienne zakłócające"),

    div(class = "lc-prose",
      p("Pierwsze pytanie po każdym wyniku brzmi: czy to nie zasługa czegoś
        innego? Zmienna Z może mieszać w interpretacji, jeśli wiąże się
        jednocześnie z predyktorem i z wynikiem. Prześledźmy to na jednym
        przykładzie, a potem zbiorczo dla pozostałych zmiennych.")
    ),

    lc_h3("Przykład: czy płeć zakłóca relację beauty → eval?"),

    div(class = "lc-prose",
      p("Płeć jest kandydatem na zakłócacz tylko wtedy, gdy wiąże się i z
        atrakcyjnością (predyktorem), i z oceną kursu (wynikiem). Sprawdźmy oba
        związki naraz.")
    ),

    div(class = "lc-figure-panel",
      h4("Płeć a obie zmienne relacji"),
      div(class = "two-plot-grid",
        zoom_plot_ui("ch6_conf_beauty", height = "300px"),
        zoom_plot_ui("ch6_conf_eval", height = "300px")
      ),
      uiOutput("ch6_conf_example_verdict")
    ),

    lc_h3("Pozostałe zmienne — tabela zbiorcza"),

    div(class = "lc-figure-panel",
      h4("Kandydaci na zmienne zakłócające (dla tropu beauty)"),
      uiOutput("ch6_confounder_table")
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Brak zakłócenia to nie brak znaczenia:"),
      p("Zmienna może „nie zakłócać\" relacji beauty → eval, a mimo to być ważna.
        Wiek wiąże się silnie z atrakcyjnością (|r| = 0,30), ale słabo z oceną
        (|r| = 0,05) — według reguły „nie zakłóca\". A jednak to podpowiada coś
        istotnego: ocena atrakcyjności może częściowo mierzyć wiek, czyli być
        jego ", tags$em("proxy"), ". Pytanie „czy uroda wpływa na ocenę, czy jest
        tylko zasłoną dla wieku?\" zostaje otwarte — i jest dobrym tematem na
        model kontrolny.")
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Wniosek pośredni:"),
      p("Jeśli choć jedna zmienna wiąże się i z `beauty`, i z `eval`, to prosty
        test nie wystarczy — trzeba uwzględnić te zmienne jednocześnie. To jest
        dokładnie zadanie dla modelu kontrolnego z rozdziału 7.")
    ),

    lc_h2("sec-04", "Czego brakuje w danych?"),

    div(class = "lc-prose",
      p("Wiązka pokazała też, czego nie ma w danych. To nie porażka — to lista
        zakupów do lepszego badania. Tu analiza danych przechodzi w projektowanie
        badania.")
    ),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Brakujące elementy, które najbardziej zmieniłyby interpretację:"),
      tags$ul(
        tags$li("Efekty uczenia się przed i po kursie — czy studenci faktycznie się nauczyli."),
        tags$li("Trudność kursu i obciążenie pracą."),
        tags$li("Oczekiwana ocena / łatwość zaliczenia."),
        tags$li("Obowiązkowość kursu."),
        tags$li("Styl prowadzenia i jakość materiałów."),
        tags$li("Powody braku odpowiedzi w ankiecie.")
      ),
      p("Zadanie dla grupy: wybierzcie jeden brak, który najmocniej podważyłby
        wasz obecny odczyt celu.")
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Wniosek:"),
      p("Wynik nie zamyka tematu. Wskazuje, które pytanie warto postawić jako następne.")
    ),

    lc_chapter_next("06", "Model kontrolny",
      "Sprawdziliśmy tropy pojedynczo — czas sprawdzić je wszystkie naraz, w jednym modelu.",
      "ch8"),
    div(style = "height: 40px;")
  )))
)

ch6_server <- function(input, output, session) {
  output$ch6_goal_readout <- renderUI({
    supported <- tr_board_summary$short[tr_board_summary$supported]
    weak      <- tr_board_summary$short[!tr_board_summary$supported]
    fmt <- function(x) if (length(x) == 0) "—" else paste(x, collapse = ", ")
    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Co tablica mówi o celu:"),
      p("Tropy wzmocnione przez dane: ", tags$strong(fmt(supported)), "."),
      p("Tropy osłabione: ", tags$strong(fmt(weak)), "."),
      p("Żaden pojedynczy trop nie rozstrzyga, czy `eval` mierzy jakość. Ale wiązka
        pokazuje, że w ocenie z ankiety siedzi więcej niż sama jakość nauczania —
        i że trzeba sprawdzić te tropy jednocześnie, w jednym modelu.")
    )
  })

  output$ch6_next_steps <- renderUI({
    cases <- list(
      beauty = c(
        "Jak zaprojektować badanie, które oddzieli wygląd od jakości materiałów?",
        "Czy efekt jest jednakowy dla różnych typów kursów i poziomów?"
      ),
      gender = c(
        "Czy kobiety i mężczyźni prowadzą podobne typy kursów?",
        "Czy różnica wygląda tak samo dla niższych i wyższych kursów?",
        "Czy response rate różni się między tymi grupami?"
      ),
      native = c(
        "Czy native speakerzy prowadzą inne kursy niż pozostali?",
        "Czy studenci oceniają jakość nauczania, czy łatwość komunikacji?",
        "Jak zebrać dane o języku prowadzenia, jasności wyjaśnień i typie zajęć?"
      ),
      minority = c(
        "Czy grupa minority jest wystarczająco liczna na stabilny wynik?",
        "Czy różnice ujawniają się tylko w wybranych typach kursów?",
        "Czy potrzebujemy lepszego pomiaru doświadczeń prowadzących i studentów?"
      ),
      response = c(
        "Czy kursy z niskim response rate są większe albo trudniejsze?",
        "Czy bardziej niezadowoleni studenci chętniej odpowiadają?",
        "Jak w projekcie zadbać o reprezentatywność ankiety?"
      )
    )
    cards <- lapply(tr_trop_order, function(id) {
      tr  <- tr_tropy[[id]]
      row <- tr_board_row(id)
      badge_cls <- if (row$supported) "tropy-verdict tropy-verdict-on"
                   else "tropy-verdict tropy-verdict-off"
      div(class = "trop-card",
        h4(tr$short, " ", tags$span(class = badge_cls, row$verdict)),
        p(tags$strong("Następne pytania:")),
        tags$ul(class = "trop-alt", lapply(cases[[id]], tags$li))
      )
    })
    div(class = "trop-stack", cards)
  })

  # Zmienne zakłócające — przykład rozpisany (płeć) + tabela zbiorcza.
  .conf_box <- function(y_var, y_label) {
    ggplot(tr_data, aes(x = gender, y = .data[[y_var]], fill = gender)) +
      geom_boxplot(alpha = 0.65, outlier.alpha = 0.25) +
      geom_jitter(width = 0.12, alpha = 0.16, size = 1) +
      scale_fill_manual(values = c(proj_col_data, proj_col_hyp)) +
      labs(x = "Płeć prowadzącego", y = y_label) +
      theme_upwr() +
      theme(legend.position = "none")
  }
  zoom_plot_server("ch6_conf_beauty",
                   reactive(.conf_box("beauty", "Ocena atrakcyjności (beauty)")))
  zoom_plot_server("ch6_conf_eval",
                   reactive(.conf_box("eval", "Ocena kursu (eval)")))

  output$ch6_conf_example_verdict <- renderUI({
    r <- tr_confounder_row("gender")
    lc_feedback(
      tags$p(tags$strong("Co widać: "),
        "płeć wiąże się z atrakcyjnością (", tags$code(r$beauty_label),
        ") i z oceną kursu (", tags$code(r$eval_label), ")."),
      tags$p(tags$strong("Werdykt: "),
        "oba związki są wyraźne, więc płeć jest kandydatem na zmienną zakłócającą.
         Relacji beauty → eval nie można czytać bez uwzględnienia płci."),
      type = "warning"
    )
  })

  output$ch6_confounder_table <- renderUI({
    rows <- lapply(tr_confounder_vars, function(var) {
      r <- tr_confounder_row(var)
      verdict <- if (r$confounder) {
        tags$span(class = "tropy-verdict tropy-verdict-off", "kandydat na zakłócacz")
      } else {
        tags$span(class = "tropy-muted", "nie zakłóca głównej relacji")
      }
      tags$tr(
        tags$td(tags$strong(r$label)),
        tags$td(r$beauty_label),
        tags$td(r$eval_label),
        tags$td(verdict)
      )
    })
    tagList(
      div(class = "lc-prose",
        p("Zmienna jest kandydatem na zakłócacz, gdy wiąże się i z `beauty`,
          i z `eval` jednocześnie.")
      ),
      tags$table(class = "lc-table lc-table-bordered lc-table-striped",
        tags$thead(tags$tr(
          tags$th("Zmienna"),
          tags$th("Związek z beauty"),
          tags$th("Związek z eval"),
          tags$th("Werdykt")
        )),
        tags$tbody(rows)
      )
    )
  })
}
