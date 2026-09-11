ch6_ui <- lecture_chapter(id = "ch6", num = "6", title = "Wynik nie kończy badania", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 06 · Iteracja",
      num = "06",
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

    lc_h2("sec-02", "Co robimy po wstępnych wynikach?"),

    div(class = "lc-prose",
      p("Werdykt „wzmocniony\" nie znaczy „udowodniony\", a „osłabiony\" nie znaczy
        „temat zamknięty\". Na tym etapie mamy już pierwsze wyniki, więc nie
        układamy planu od zera. Sprawdzamy alternatywne wyjaśnienia zapisane
        wcześniej i dopisujemy nowe hipotezy, które pojawiły się po obliczeniach.")
    ),

    uiOutput("ch6_next_steps"),

    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Dlaczego te punkty czasem się nakładają?"),
      p("Na poziomie konspektu tropy zapisujemy osobno, żeby nie zgubić pytań.
        Na poziomie analizy te same zmienne mogą się spotkać: ", tags$code("gender"),
        " może być osobnym tropem, ale też alternatywnym wyjaśnieniem dla relacji ",
        tags$code("beauty"), " → ", tags$code("eval"), "."),
      p("To właśnie prowadzi do analizy łączonej: po weryfikacji alternatyw
        budujemy jeden model, który pozwala zobaczyć tropy razem.")
    ),

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

    lc_chapter_next("07", "Model kontrolny",
      "Sprawdziliśmy tropy pojedynczo — czas sprawdzić je wszystkie naraz, w jednym modelu.",
      "ch7"),
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
      beauty = list(
        narrative = "Wstępny wynik sugeruje, że atrakcyjność wiąże się z oceną kursu. Teraz pytamy, czy ten związek nie wynika z innych cech prowadzącego albo kursu.",
        checks = c(
          "Sprawdzić alternatywy z konspektu: wiek, płeć i typ kursu.",
          "Zobaczyć, czy `beauty` współwystępuje z tymi zmiennymi.",
          "Zobaczyć, czy związek `beauty` z `eval` pozostaje widoczny, gdy te zmienne analizujemy razem."
        )
      ),
      gender = list(
        narrative = "Jeżeli płeć różnicuje oceny, trzeba sprawdzić, czy jest samodzielnym tropem, czy raczej miesza się z innymi cechami kursu i prowadzącego.",
        checks = c(
          "Sprawdzić, z czym współwystępuje `gender`: typ kursu, response rate, wiek, atrakcyjność.",
          "Ocenić, czy wynik dla płci może być alternatywnym wyjaśnieniem dla innych tropów.",
          "Zobaczyć, czy efekt płci pozostaje widoczny, gdy inne zmienne analizujemy razem."
        )
      ),
      native = list(
        narrative = "Jeżeli status językowy różnicuje oceny, trzeba ustalić, czy chodzi o sam odbiór prowadzącego, czy o kontekst kursów, które prowadzi dana grupa.",
        checks = c(
          "Sprawdzić, czy `native` współwystępuje z poziomem kursu, credits albo liczebnością grup.",
          "Ocenić, czy różnice między grupami mogą wynikać z nierównych liczebności albo rodzaju prowadzonych zajęć.",
          "Zobaczyć, czy `native` wnosi informację, gdy uwzględniamy inne cechy kursu i prowadzącego."
        )
      ),
      minority = list(
        narrative = "Jeżeli status mniejszościowy wiąże się z oceną, wynik trzeba traktować jako ostrożny sygnał i sprawdzić, czy nie nakładają się tu inne różnice między grupami.",
        checks = c(
          "Opisać liczebności grup, zanim interpretujemy różnice.",
          "Sprawdzić, czy `minority` współwystępuje z płcią, statusem `native`, typem kursu lub innymi cechami.",
          "Dopisać nowe hipotezy ostrożnie: wynik może wskazywać problem, ale nie dowodzi mechanizmu."
        )
      ),
      response = list(
        narrative = "Jeżeli response rate wiąże się z oceną, trzeba sprawdzić, czy ankieta opisuje doświadczenie całej grupy, czy raczej głos wybranej części studentów.",
        checks = c(
          "Sprawdzić, czy `response.rate` wiąże się z wielkością kursu (`students`, `allstudents`) albo typem kursu.",
          "Zobaczyć, czy niska odpowiedź osłabia zaufanie do pozostałych wyników.",
          "Dopisać hipotezę o selekcji odpowiedzi, jeśli response rate zmienia interpretację innych tropów."
        )
      )
    )
    cards <- lapply(tr_trop_order, function(id) {
      tr  <- tr_tropy[[id]]
      row <- tr_board_row(id)
      badge_cls <- if (row$supported) "tropy-verdict tropy-verdict-on"
                   else "tropy-verdict tropy-verdict-off"
      div(class = "trop-card",
        h4(tr$short, " ", tags$span(class = badge_cls, row$verdict)),
        p(tags$strong("Hipoteza po wstępnym wyniku: "), cases[[id]]$narrative),
        p(tags$strong("Co sprawdzamy dalej:")),
        tags$ul(class = "trop-alt",
          lapply(cases[[id]]$checks, function(x) {
            tags$li(HTML(gsub("`([^`]+)`", "<code>\\1</code>", x)))
          })
        )
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
