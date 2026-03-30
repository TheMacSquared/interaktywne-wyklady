# ============================================================================
# CHAPTER 2: Formulowanie hipotez statystycznych
# ============================================================================

ch2h_ui <- tabPanel("2. Formu\u0142owanie hipotez",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Znamy logik\u0119 testowania: H\u2080, H\u2081, p-warto\u015b\u0107, decyzja.
       Ale jak przej\u015b\u0107 od pytania badawczego do formalnej hipotezy?"
    ),

    div(class = "section-title", "Od pytania do hipotezy"),

    div(class = "narrative",
      p("W badaniach pytania s\u0105 formu\u0142owane ", tags$b("swobodnym j\u0119zykiem"), ":"),
      tags$ul(
        tags$li(em("\"Czy m\u0119\u017cczy\u017ani s\u0105 wy\u017csi od kobiet?\"")),
        tags$li(em("\"Czy korepetycje pomagaj\u0105?\"")),
        tags$li(em("\"Czy lod\u00f3w sprzedaje si\u0119 wi\u0119cej w ciep\u0142e dni?\""))
      ),
      p("Ale test statystyczny wymaga ", tags$b("formalnych hipotez"), ":
        precyzyjnych stwierdze\u0144 o parametrach populacji, kt\u00f3re mo\u017cna
        zweryfikowa\u0107 danymi."),
      p("Najwa\u017cniejsza zasada: ", tags$b("H\u2080 zawsze zawiera znak r\u00f3wno\u015bci"),
        " (=, \u2264, \u2265). H\u2081 zawiera to, co chcemy wykaza\u0107 (\u2260, >, <).")
    ),

    div(class = "callout-info",
      tags$strong("Szablon:"),
      p(withMathJax("\\(H_0\\)"), ": brak efektu / brak r\u00f3\u017cnicy / brak zwi\u0105zku"),
      p(withMathJax("\\(H_1\\)"), ": jest efekt / jest r\u00f3\u017cnica / jest zwi\u0105zek")
    ),

    # ========================================================================
    # WIDGET 1: Pytanie badawcze -> hipoteza (galeria przykladow)
    # ========================================================================
    div(class = "section-title", "Galeria przyk\u0142ad\u00f3w: pytanie \u2192 hipoteza"),

    div(class = "narrative",
      p("Przegl\u0105daj przyk\u0142ady \u2014 ka\u017cdy pokazuje, jak przej\u015b\u0107
        od potocznego pytania do formalnych hipotez.")
    ),

    div(class = "widget-block",
      h4("Przyk\u0142ady formu\u0142owania hipotez"),
      fluidRow(
        column(4,
          selectInput("ch2h_example", "Wybierz przyk\u0142ad:",
            choices = c(
              "Wzrost m\u0119\u017cczyzn vs kobiet" = "ex1",
              "Czy korepetycje pomagaj\u0105?" = "ex2",
              "Skuteczno\u015b\u0107 leku" = "ex3",
              "Czy kostka jest uczciwa?" = "ex4",
              "Zwi\u0105zek nauki z ocenami" = "ex5",
              "P\u0142e\u0107 a wyb\u00f3r kierunku" = "ex6",
              "R\u00f3\u017cnice mi\u0119dzy 4 grupami" = "ex7",
              "\u015aredni czas dojazdu = 30 min?" = "ex8",
              "Odsetek zda\u0144 > 70%?" = "ex9",
              "Przed vs po szkoleniu" = "ex10"
            ),
            selected = "ex1"
          )
        ),
        column(8,
          uiOutput("ch2h_example_display")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Cwiczenie - sformuluj hipoteze (potoczne -> formalne)
    # ========================================================================
    div(class = "section-title", "\u0106wiczenie: sformu\u0142uj hipotez\u0119"),

    div(class = "narrative",
      p("Dostajesz pytanie badawcze w j\u0119zyku potocznym.
        Tw\u00f3j cel: wybra\u0107 poprawn\u0105 par\u0119 H\u2080/H\u2081.")
    ),

    div(class = "widget-block",
      h4("Quiz: pytanie \u2192 hipoteza"),
      uiOutput("ch2h_quiz_question"),
      radioButtons("ch2h_quiz_answer", "Wybierz poprawn\u0105 par\u0119 hipotez:",
        choices = c("A" = "A", "B" = "B", "C" = "C"),
        selected = character(0)
      ),
      div(style = "display: flex; gap: 8px;",
        actionButton("ch2h_quiz_check", "Sprawd\u017a", class = "btn-primary"),
        actionButton("ch2h_quiz_next", "Nast\u0119pne pytanie", class = "btn-outline-secondary")
      ),
      br(), br(),
      uiOutput("ch2h_quiz_feedback")
    ),

    # ========================================================================
    # WIDGET 3: Hipoteza -> co badamy? (odwrotny kierunek)
    # ========================================================================
    div(class = "section-title", "Odwrotnie: hipoteza \u2192 co badamy?"),

    div(class = "narrative",
      p("Teraz odwrotnie: widzisz formaln\u0105 hipotez\u0119 statystyczn\u0105.
        Co w\u0142a\u015bciwie badamy? Jak opisa\u0107 to przyst\u0119pnym j\u0119zykiem?")
    ),

    div(class = "widget-block",
      h4("Quiz: hipoteza \u2192 interpretacja"),
      uiOutput("ch2h_rev_question"),
      radioButtons("ch2h_rev_answer", "Co badamy?",
        choices = c("A" = "A", "B" = "B", "C" = "C"),
        selected = character(0)
      ),
      div(style = "display: flex; gap: 8px;",
        actionButton("ch2h_rev_check", "Sprawd\u017a", class = "btn-primary"),
        actionButton("ch2h_rev_next", "Nast\u0119pne pytanie", class = "btn-outline-secondary")
      ),
      br(), br(),
      uiOutput("ch2h_rev_feedback")
    ),

    # ========================================================================
    # WIDGET 4: Jednostronny vs dwustronny
    # ========================================================================
    div(class = "section-title", "Jednostronny vs dwustronny test"),

    div(class = "narrative",
      p("Sformu\u0142owanie H\u2081 decyduje, czy test jest jedno- czy dwustronny:"),
      tags$table(class = "table table-bordered", style = "font-size: 15px;",
        tags$thead(
          tags$tr(tags$th("Typ"), tags$th("H\u2081"), tags$th("Przyk\u0142ad"), tags$th("Kiedy?"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("Dwustronny")),
            tags$td(withMathJax("\\(\\mu_1 \\neq \\mu_2\\)")),
            tags$td("\"Czy grupy si\u0119 r\u00f3\u017cni\u0105?\""),
            tags$td("Gdy nie wiesz, w kt\u00f3r\u0105 stron\u0119 \u2014 ", tags$b("domy\u015blny wyb\u00f3r"))
          ),
          tags$tr(
            tags$td(tags$strong("Prawostronny")),
            tags$td(withMathJax("\\(\\mu_1 > \\mu_2\\)")),
            tags$td("\"Czy lek jest lepszy od placebo?\""),
            tags$td("Gdy masz silne podstawy teoretyczne dla kierunku")
          ),
          tags$tr(
            tags$td(tags$strong("Lewostronny")),
            tags$td(withMathJax("\\(\\mu_1 < \\mu_2\\)")),
            tags$td("\"Czy nowa metoda jest szybsza?\""),
            tags$td("Gdy oczekujesz ni\u017cszej warto\u015bci")
          )
        )
      )
    ),

    div(class = "widget-block",
      h4("Wizualizacja: jedno- vs dwustronny"),
      fluidRow(
        column(4,
          radioButtons("ch2h_sided", "Typ testu:",
            choices = c(
              "Dwustronny (\u2260)" = "two.sided",
              "Prawostronny (>)" = "greater",
              "Lewostronny (<)" = "less"
            ),
            selected = "two.sided"
          ),
          sliderInput("ch2h_alpha", "\u03b1:",
                      min = 0.01, max = 0.10, value = 0.05, step = 0.01)
        ),
        column(8,
          plotOutput("ch2h_sided_plot", height = "300px")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("W w\u0105tpliwo\u015bci:"),
      " U\u017cywaj testu ", tags$b("dwustronnego"),
      ". Test jednostronny jest mocniejszy (wi\u0119ksza moc), ale ryzykowny:
        je\u015bli efekt jest w przeciwnym kierunku, nie mo\u017cesz go wykry\u0107.
        Test jednostronny powinien by\u0107 zaplanowany ", tags$em("przed"),
      " zbieraniem danych."
    ),

    # ========================================================================
    # Typowe bledy
    # ========================================================================
    div(class = "section-title", "Typowe b\u0142\u0119dy przy formu\u0142owaniu hipotez"),

    div(class = "callout-danger",
      tags$ol(
        tags$li(
          tags$b("H\u2080 z nier\u00f3wno\u015bci\u0105:"),
          " \u017ale: ", withMathJax("\\(H_0: \\mu_1 \\neq \\mu_2\\)"),
          ". H\u2080 ", tags$b("zawsze"), " zawiera = (ewentualnie \u2264 lub \u2265)."
        ),
        tags$li(
          tags$b("Hipoteza o pr\u00f3bie zamiast populacji:"),
          " \u017ale: \"H\u2080: \u015brednia w pr\u00f3bie = 170\". Hipotezy dotycz\u0105 ",
          tags$b("parametr\u00f3w populacji"), ", nie statystyk z pr\u00f3by."
        ),
        tags$li(
          tags$b("Brak precyzji:"),
          " \u017ale: \"H\u2080: dane s\u0105 dobre\". Hipoteza musi precyzyjnie okre\u015bla\u0107 parametr i warto\u015b\u0107."
        ),
        tags$li(
          tags$b("Zmiana hipotezy po zobaczeniu danych (HARKing):"),
          " Hipotezy formu\u0142ujemy PRZED analiz\u0105, nie po!"
        ),
        tags$li(
          tags$b("Myl\u0105ce H\u2080 i H\u2081:"),
          " H\u2081 to to, co chcesz ", tags$b("wykaza\u0107"),
          ". H\u2080 to \"stan domy\u015blny\" (brak efektu). Nie odwracaj ich."
        )
      )
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: pierwszy test \u2014 jedna zmienna ilo\u015bciowa"),
      actionButton("ch2h_next", "Dalej \u2192 3. Jedna zmienna ilo\u015bciowa",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch2h_server <- function(input, output, session) {

  # --- Baza przykaldow ---
  examples <- list(
    ex1 = list(
      question = "\"Czy m\u0119\u017cczy\u017ani s\u0105 wy\u017csi od kobiet?\"",
      context = "Badanie wzrostu w populacji student\u00f3w. Zmienna: wzrost (cm). Grupy: p\u0142e\u0107.",
      h0 = "\\(H_0: \\mu_{M} = \\mu_{K}\\)",
      h1 = "\\(H_1: \\mu_{M} \\neq \\mu_{K}\\)",
      h1_alt = "lub jednostronnie: \\(H_1: \\mu_{M} > \\mu_{K}\\)",
      test = "Test t niezale\u017cny (lub Mann-Whitney)",
      tip = "Pytanie sugeruje kierunek (\"wy\u017csi\"), ale bezpieczniej u\u017cy\u0107 testu dwustronnego,
             chyba \u017ce mamy mocne podstawy teoretyczne."
    ),
    ex2 = list(
      question = "\"Czy korepetycje pomagaj\u0105 studentom?\"",
      context = "Mierzymy wyniki tego samego studenta PRZED i PO korepetycjach.",
      h0 = "\\(H_0: \\mu_d = 0\\) (\\(d\\) = wynik po \u2212 wynik przed)",
      h1 = "\\(H_1: \\mu_d \\neq 0\\)",
      h1_alt = "lub jednostronnie: \\(H_1: \\mu_d > 0\\)",
      test = "Test t parowy (dane zale\u017cne, te same osoby)",
      tip = "Kluczowe: te same osoby mierzone dwa razy \u2192 test parowy, nie niezale\u017cny!"
    ),
    ex3 = list(
      question = "\"Czy nowy lek obni\u017ca ci\u015bnienie skuteczniej ni\u017c placebo?\"",
      context = "Randomizowane badanie: grupa lekowa vs grupa placebo. Zmienna: zmiana ci\u015bnienia.",
      h0 = "\\(H_0: \\mu_{lek} = \\mu_{placebo}\\)",
      h1 = "\\(H_1: \\mu_{lek} < \\mu_{placebo}\\)",
      h1_alt = "(jednostronnie, bo obni\u017cenie = ni\u017csze ci\u015bnienie)",
      test = "Test t niezale\u017cny (pr\u00f3by niezale\u017cne, r\u00f3\u017cne osoby)",
      tip = "Tu test jednostronny ma sens \u2014 interesuje nas tylko, czy lek OBNI\u017bA ci\u015bnienie."
    ),
    ex4 = list(
      question = "\"Czy ta kostka jest uczciwa?\"",
      context = "Rzucamy kostk\u0105 n razy. Zliczamy, ile razy wypad\u0142o ka\u017cde oczko.",
      h0 = "\\(H_0: p_1 = p_2 = \\ldots = p_6 = 1/6\\)",
      h1 = "\\(H_1:\\) co najmniej jedno \\(p_i \\neq 1/6\\)",
      h1_alt = "",
      test = "Test \u03c7\u00b2 zgodno\u015bci",
      tip = "H\u2080 precyzyjnie okre\u015bla oczekiwany rozk\u0142ad. Jedna zmienna jako\u015bciowa (wynik rzutu)."
    ),
    ex5 = list(
      question = "\"Czy wi\u0119cej nauki = lepsze oceny?\"",
      context = "Zmienna X: godziny nauki/tydzie\u0144. Zmienna Y: \u015brednia ocen. Obie ilo\u015bciowe.",
      h0 = "\\(H_0: \\rho = 0\\) (brak korelacji)",
      h1 = "\\(H_1: \\rho \\neq 0\\)",
      h1_alt = "lub jednostronnie: \\(H_1: \\rho > 0\\)",
      test = "Korelacja Pearsona (lub Spearman)",
      tip = "Dwie zmienne ilo\u015bciowe \u2192 korelacja. \"Wi\u0119cej = lepsze\" sugeruje kierunek dodatni."
    ),
    ex6 = list(
      question = "\"Czy wyb\u00f3r kierunku studi\u00f3w zale\u017cy od p\u0142ci?\"",
      context = "Dwie zmienne jako\u015bciowe: p\u0142e\u0107 (K/M) i kierunek (Informatyka/Ekonomia/Psychologia/Biologia).",
      h0 = "\\(H_0:\\) p\u0142e\u0107 i kierunek s\u0105 niezale\u017cne",
      h1 = "\\(H_1:\\) p\u0142e\u0107 i kierunek s\u0105 powi\u0105zane",
      h1_alt = "",
      test = "Test \u03c7\u00b2 niezale\u017cno\u015bci (lub Fisher przy ma\u0142ych n)",
      tip = "Dwie jako\u015bciowe \u2192 tabela kontyngencji \u2192 \u03c7\u00b2 niezale\u017cno\u015bci."
    ),
    ex7 = list(
      question = "\"Czy \u015brednie oceny r\u00f3\u017cni\u0105 si\u0119 mi\u0119dzy kierunkami?\"",
      context = "Zmienna Y: \u015brednia ocen. Grupy: 4 kierunki studi\u00f3w. Jedna zmienna ilo\u015bciowa, jedna jako\u015bciowa (3+ grup).",
      h0 = "\\(H_0: \\mu_1 = \\mu_2 = \\mu_3 = \\mu_4\\)",
      h1 = "\\(H_1:\\) co najmniej jedna para \u015brednich si\u0119 r\u00f3\u017cni",
      h1_alt = "",
      test = "ANOVA jednoczynnikowa (lub Kruskal-Wallis)",
      tip = "Wi\u0119cej ni\u017c 2 grupy \u2192 ANOVA. NIE wykonuj wielu test\u00f3w t parami!"
    ),
    ex8 = list(
      question = "\"Czy \u015bredni czas dojazdu to 30 minut?\"",
      context = "Zmienna: czas dojazdu (min). Jedna pr\u00f3ba, por\u00f3wnanie z warto\u015bci\u0105 referencyjn\u0105.",
      h0 = "\\(H_0: \\mu = 30\\)",
      h1 = "\\(H_1: \\mu \\neq 30\\)",
      h1_alt = "",
      test = "Test t jednej pr\u00f3by",
      tip = "Jedna zmienna ilo\u015bciowa, pytamy czy \u015brednia r\u00f3\u017cni si\u0119 od konkretnej warto\u015bci."
    ),
    ex9 = list(
      question = "\"Czy wi\u0119cej ni\u017c 70% student\u00f3w zdaje egzamin?\"",
      context = "Zmienna: zdany/niezdany (binarna). Pytanie o proporcj\u0119 w populacji.",
      h0 = "\\(H_0: p \\leq 0.7\\)",
      h1 = "\\(H_1: p > 0.7\\)",
      h1_alt = "",
      test = "Test dwumianowy (lub test proporcji)",
      tip = "Zmienna binarna, pytanie o proporcj\u0119 \u2192 test dwumianowy. Jednostronny, bo \"wi\u0119cej ni\u017c\"."
    ),
    ex10 = list(
      question = "\"Czy szkolenie BHP zmniejszy\u0142o liczb\u0119 wypadk\u00f3w?\"",
      context = "Te same zak\u0142ady, mierzone PRZED i PO szkoleniu. Zmienna: liczba wypadk\u00f3w/miesi\u0105c.",
      h0 = "\\(H_0: \\mu_d = 0\\) (d = po \u2212 przed)",
      h1 = "\\(H_1: \\mu_d < 0\\) (zmniejszenie)",
      h1_alt = "",
      test = "Test t parowy (lub Wilcoxon parowy)",
      tip = "Te same jednostki mierzone dwa razy \u2192 parowy. Jednostronny, bo \"zmniejszy\u0142o\"."
    )
  )

  # --- Widget 1: Galeria ---
  output$ch2h_example_display <- renderUI({
    ex <- examples[[input$ch2h_example]]
    if (is.null(ex)) return(NULL)

    tagList(
      div(class = "callout-info",
        p(tags$strong("Pytanie badawcze: "), ex$question),
        p(tags$em("Kontekst: "), ex$context)
      ),
      div(class = "formula-box",
        p(tags$strong("Hipoteza zerowa: "), withMathJax(ex$h0)),
        p(tags$strong("Hipoteza alternatywna: "), withMathJax(ex$h1)),
        if (nchar(ex$h1_alt) > 0) p(tags$em(withMathJax(ex$h1_alt)))
      ),
      div(class = "callout-success",
        p(tags$strong("Test: "), ex$test),
        p(tags$strong("Wskaz\u00f3wka: "), ex$tip)
      )
    )
  })

  # --- Widget 2: Quiz pytanie -> hipoteza ---
  quiz_bank <- list(
    list(
      question = "Badacz chce sprawdzi\u0107, czy studenci informatyki \u015bpi\u0105 mniej ni\u017c \u015brednio 7 godzin.",
      options = c(
        "A" = "H\u2080: \u03bc = 7, H\u2081: \u03bc \u2260 7",
        "B" = "H\u2080: \u03bc \u2265 7, H\u2081: \u03bc < 7",
        "C" = "H\u2080: \u03bc < 7, H\u2081: \u03bc \u2265 7"
      ),
      correct = "B",
      explanation = "\"Mniej ni\u017c 7\" to hipoteza alternatywna (H\u2081: \u03bc < 7). H\u2080 zawiera r\u00f3wno\u015b\u0107 (\u2265)."
    ),
    list(
      question = "Firma farmaceutyczna bada, czy nowy lek r\u00f3\u017cni si\u0119 skuteczno\u015bci\u0105 od istniej\u0105cego.",
      options = c(
        "A" = "H\u2080: \u03bc_nowy = \u03bc_stary, H\u2081: \u03bc_nowy \u2260 \u03bc_stary",
        "B" = "H\u2080: \u03bc_nowy \u2260 \u03bc_stary, H\u2081: \u03bc_nowy = \u03bc_stary",
        "C" = "H\u2080: \u03bc_nowy = \u03bc_stary, H\u2081: \u03bc_nowy > \u03bc_stary"
      ),
      correct = "A",
      explanation = "\"R\u00f3\u017cni si\u0119\" (bez kierunku) = test dwustronny. H\u2080: brak r\u00f3\u017cnicy, H\u2081: jest r\u00f3\u017cnica."
    ),
    list(
      question = "Nauczyciel chce zbada\u0107, czy rozk\u0142ad ocen na egzaminie odpowiada\u0142 krzywej normalnej: 10% niedostatecznych, 20% dostatecznych, 40% dobrych, 20% bardzo dobrych, 10% celuj\u0105cych.",
      options = c(
        "A" = "H\u2080: rozk\u0142ad ocen jest normalny, H\u2081: nie jest normalny",
        "B" = "H\u2080: p\u2081=0.1, p\u2082=0.2, p\u2083=0.4, p\u2084=0.2, p\u2085=0.1; H\u2081: co najmniej jedno pi r\u00f3\u017cne",
        "C" = "H\u2080: \u03bc = 3.5, H\u2081: \u03bc \u2260 3.5"
      ),
      correct = "B",
      explanation = "To test \u03c7\u00b2 zgodno\u015bci \u2014 H\u2080 okre\u015bla konkretne proporcje, H\u2081: rozk\u0142ad si\u0119 r\u00f3\u017cni."
    ),
    list(
      question = "Dietetyk bada, czy p\u0142e\u0107 wp\u0142ywa na preferencje dietetyczne (wege/mi\u0119so/r\u00f3\u017cne).",
      options = c(
        "A" = "H\u2080: \u03bc_K = \u03bc_M, H\u2081: \u03bc_K \u2260 \u03bc_M",
        "B" = "H\u2080: dieta i p\u0142e\u0107 s\u0105 niezale\u017cne, H\u2081: dieta i p\u0142e\u0107 s\u0105 powi\u0105zane",
        "C" = "H\u2080: proporcje diet s\u0105 r\u00f3wne, H\u2081: nie s\u0105 r\u00f3wne"
      ),
      correct = "B",
      explanation = "Dwie zmienne jako\u015bciowe \u2192 test niezale\u017cno\u015bci. H\u2080: niezale\u017cno\u015b\u0107, H\u2081: powi\u0105zanie."
    ),
    list(
      question = "Producent twierdzi, \u017ce co najmniej 95% produkt\u00f3w spe\u0142nia normy. Kontrola chce to zweryfikowa\u0107.",
      options = c(
        "A" = "H\u2080: p = 0.95, H\u2081: p \u2260 0.95",
        "B" = "H\u2080: p \u2265 0.95, H\u2081: p < 0.95",
        "C" = "H\u2080: p < 0.95, H\u2081: p \u2265 0.95"
      ),
      correct = "B",
      explanation = "Kontrola chce sprawdzi\u0107, czy odsetek jest ni\u017cszy ni\u017c deklarowane 95%. H\u2081: p < 0.95."
    )
  )

  ch2h_quiz_idx <- reactiveVal(1)
  observe({ ch2h_quiz_idx() })  # initialize
  observeEvent(input$ch2h_quiz_next, {
    ch2h_quiz_idx(sample(length(quiz_bank), 1))
  })

  output$ch2h_quiz_question <- renderUI({
    q <- quiz_bank[[ch2h_quiz_idx()]]
    div(class = "callout-info",
      p(tags$strong("Pytanie:"), q$question),
      tags$ul(
        tags$li(tags$strong("A) "), q$options["A"]),
        tags$li(tags$strong("B) "), q$options["B"]),
        tags$li(tags$strong("C) "), q$options["C"])
      )
    )
  })

  output$ch2h_quiz_feedback <- renderUI({
    req(input$ch2h_quiz_check)
    isolate({
      q <- quiz_bank[[ch2h_quiz_idx()]]
      answer <- input$ch2h_quiz_answer
      if (is.null(answer) || answer == "") {
        return(div(class = "callout-warning", "Wybierz odpowied\u017a!"))
      }
      if (answer == q$correct) {
        div(class = "callout-success",
          tags$strong("Poprawnie!"),
          p(q$explanation))
      } else {
        div(class = "callout-danger",
          tags$strong("Nie! "),
          p("Poprawna odpowied\u017a: ", q$correct, "."),
          p(q$explanation))
      }
    })
  })

  # --- Widget 3: Quiz hipoteza -> interpretacja ---
  rev_bank <- list(
    list(
      hypothesis = "H\u2080: \u03bc = 36.6, H\u2081: \u03bc \u2260 36.6",
      context = "Badanie grupy pacjent\u00f3w. Zmienna: temperatura cia\u0142a (\u00b0C).",
      options = c(
        "A" = "Czy \u015brednia temperatura pacjent\u00f3w r\u00f3\u017cni si\u0119 od normy 36.6\u00b0C?",
        "B" = "Czy temperatura ka\u017cdego pacjenta wynosi 36.6\u00b0C?",
        "C" = "Czy rozk\u0142ad temperatury jest normalny?"
      ),
      correct = "A",
      explanation = "Test t jednej pr\u00f3by: por\u00f3wnanie \u015bredniej populacyjnej z warto\u015bci\u0105 referencyjn\u0105."
    ),
    list(
      hypothesis = "H\u2080: \u03c1 = 0, H\u2081: \u03c1 \u2260 0",
      context = "Dane: 200 student\u00f3w. Zmienne: godziny nauki i wyniki egzaminu.",
      options = c(
        "A" = "Czy godziny nauki powoduj\u0105 lepsze wyniki?",
        "B" = "Czy istnieje zwi\u0105zek liniowy mi\u0119dzy godzinami nauki a wynikami?",
        "C" = "Czy studenci ucz\u0105 si\u0119 wystarczaj\u0105co du\u017co?"
      ),
      correct = "B",
      explanation = "\u03c1 = 0 to brak korelacji. Testujemy zwi\u0105zek liniowy (Pearson). Uwaga: korelacja \u2260 przyczynowo\u015b\u0107!"
    ),
    list(
      hypothesis = "H\u2080: p\u0142e\u0107 i preferencje s\u0105 niezale\u017cne, H\u2081: s\u0105 powi\u0105zane",
      context = "Ankieta: 300 os\u00f3b. Zmienne: p\u0142e\u0107 (K/M) i ulubiony gatunek filmu.",
      options = c(
        "A" = "Czy kobiety ogl\u0105daj\u0105 wi\u0119cej film\u00f3w?",
        "B" = "Czy p\u0142e\u0107 wp\u0142ywa na preferencje filmowe?",
        "C" = "Czy rozk\u0142ad p\u0142ci jest r\u00f3wnomierny?"
      ),
      correct = "B",
      explanation = "Test \u03c7\u00b2 niezale\u017cno\u015bci: czy istnieje zwi\u0105zek mi\u0119dzy dwiema zmiennymi jako\u015bciowymi."
    ),
    list(
      hypothesis = "H\u2080: \u03bc\u2081 = \u03bc\u2082 = \u03bc\u2083, H\u2081: co najmniej jedna \u015brednia r\u00f3\u017cna",
      context = "Badanie wynik\u00f3w egzaminu w trzech grupach \u0107wiczeniowych.",
      options = c(
        "A" = "Czy wyniki egzaminu s\u0105 normalne?",
        "B" = "Czy prowadz\u0105cy wp\u0142ywaj\u0105 na wyniki? (ANOVA)",
        "C" = "Czy trzecia grupa jest najlepsza?"
      ),
      correct = "B",
      explanation = "ANOVA: por\u00f3wnanie \u015brednich w 3+ grupach. H\u2081 nie m\u00f3wi, kt\u00f3ra grupa jest najlepsza \u2014 to rola post-hoc."
    ),
    list(
      hypothesis = "H\u2080: \u03bc_d = 0, H\u2081: \u03bc_d > 0 (d = po \u2212 przed)",
      context = "20 uczni\u00f3w, mierzeni przed i po kursie szybkiego czytania. Zmienna: s\u0142owa/min.",
      options = c(
        "A" = "Czy kurs poprawi\u0142 szybko\u015b\u0107 czytania?",
        "B" = "Czy uczniowie czytaj\u0105 szybciej ni\u017c \u015brednia populacyjna?",
        "C" = "Czy r\u00f3\u017cnica mi\u0119dzy uczniami jest istotna?"
      ),
      correct = "A",
      explanation = "Test t parowy (jednostronny): te same osoby przed/po. \u03bc_d > 0 oznacza poprawa (po > przed)."
    )
  )

  ch2h_rev_idx <- reactiveVal(1)
  observeEvent(input$ch2h_rev_next, {
    ch2h_rev_idx(sample(length(rev_bank), 1))
  })

  output$ch2h_rev_question <- renderUI({
    q <- rev_bank[[ch2h_rev_idx()]]
    div(class = "callout-info",
      p(tags$strong("Kontekst: "), q$context),
      div(class = "formula-box",
        p(tags$strong("Hipotezy: "), q$hypothesis)
      ),
      tags$ul(
        tags$li(tags$strong("A) "), q$options["A"]),
        tags$li(tags$strong("B) "), q$options["B"]),
        tags$li(tags$strong("C) "), q$options["C"])
      )
    )
  })

  output$ch2h_rev_feedback <- renderUI({
    req(input$ch2h_rev_check)
    isolate({
      q <- rev_bank[[ch2h_rev_idx()]]
      answer <- input$ch2h_rev_answer
      if (is.null(answer) || answer == "") {
        return(div(class = "callout-warning", "Wybierz odpowied\u017a!"))
      }
      if (answer == q$correct) {
        div(class = "callout-success",
          tags$strong("Poprawnie!"),
          p(q$explanation))
      } else {
        div(class = "callout-danger",
          tags$strong("Nie! "),
          p("Poprawna odpowied\u017a: ", q$correct, "."),
          p(q$explanation))
      }
    })
  })

  # --- Widget 4: Jednostronny vs dwustronny ---
  output$ch2h_sided_plot <- renderPlot({
    alpha <- input$ch2h_alpha
    sided <- input$ch2h_sided
    x <- seq(-4, 4, length.out = 500)
    y <- dnorm(x)
    df <- data.frame(x = x, y = y)

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_line(color = col_h0, linewidth = 1.2)

    if (sided == "two.sided") {
      crit <- qnorm(1 - alpha / 2)
      shade_left <- df[df$x <= -crit, ]
      shade_right <- df[df$x >= crit, ]
      p <- p +
        geom_area(data = shade_left, fill = col_reject, alpha = 0.4) +
        geom_area(data = shade_right, fill = col_reject, alpha = 0.4) +
        geom_vline(xintercept = c(-crit, crit), linetype = "dashed", color = col_reject) +
        labs(title = paste0("Dwustronny: \u03b1/2 = ", alpha/2, " na ka\u017cdym ogonie"))
    } else if (sided == "greater") {
      crit <- qnorm(1 - alpha)
      shade <- df[df$x >= crit, ]
      p <- p +
        geom_area(data = shade, fill = col_reject, alpha = 0.4) +
        geom_vline(xintercept = crit, linetype = "dashed", color = col_reject) +
        labs(title = paste0("Prawostronny: ca\u0142e \u03b1 = ", alpha, " na prawym ogonie"))
    } else {
      crit <- qnorm(alpha)
      shade <- df[df$x <= crit, ]
      p <- p +
        geom_area(data = shade, fill = col_reject, alpha = 0.4) +
        geom_vline(xintercept = crit, linetype = "dashed", color = col_reject) +
        labs(title = paste0("Lewostronny: ca\u0142e \u03b1 = ", alpha, " na lewym ogonie"))
    }

    p +
      labs(x = "Statystyka testowa (z)", y = "G\u0119sto\u015b\u0107") +
      annotate("text", x = 0, y = max(y) * 0.5, label = "Nie odrzucamy H\u2080",
               color = col_accept, fontface = "bold", size = 5) +
      theme_test()
  })
}
