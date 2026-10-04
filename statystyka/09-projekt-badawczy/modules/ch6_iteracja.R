ch6_ui <- lecture_chapter(id = "ch6", num = "6", title = "Wynik nie kończy badania", content = tagList(
  lc_chapter_hero(
    kicker = "Rozdział 06 · Iteracja",
    num = "06",
    title = "Wynik nie kończy badania.",
    lead = "Pełna tablica tropów to dopiero połowa pracy. Każdy wynik
            z rozdziału 5 może być odbiciem innej zmiennej, a lista tego,
            czego w danych brakuje, jest równie ważna jak lista wyników."
  ),

  lc_p("Rozdział 5 sprawdził każdy trop osobno i wypełnił tablicę. Cel
    badania był jednak sformułowany dla całej wiązki naraz: ",
    tags$em(tr_goal), " Ten rozdział składa wyniki w jeden obraz, sprawdza,
    które z nich mogą się nawzajem tłumaczyć, i zapisuje, czego dane nie
    pozwolą rozstrzygnąć."),

  lc_h2("sec-01", "Cała wiązka naraz"),

  lc_p("Pojedynczy test odpowiada na pytanie o jeden trop. Na cel odpowiada
    dopiero zestawienie: które tropy dane wzmocniły, które osłabiły i co
    z tego wynika dla oceny z ankiety jako miary jakości nauczania.
    Zestawienie to tablica tropów z końca rozdziału 5."),

  lc_p("Wzmocnione są cztery tropy: atrakcyjność, płeć, status native
    speaker i odsetek odpowiedzi. Osłabiony jest jeden: status
    mniejszościowy. Żaden z tych wyników osobno nie rozstrzyga, czy ocena
    z ankiety mierzy jakość nauczania. Razem pokazują jednak, że z oceną
    wiążą się cechy, które z jakością zajęć nie mają oczywistego związku:
    to, jak studenci oceniają wygląd prowadzącego, jego płeć i to, jaka
    część grupy wypełniła ankietę. Ocena z ankiety zawiera więc coś więcej
    niż samą jakość nauczania. Ile więcej i czego dokładnie, tego tablica
    nie mówi, bo każdy jej wiersz powstał bez oglądania się na pozostałe."),

  lc_h2("sec-02", "Po wstępnych wynikach: alternatywne wyjaśnienia"),

  lc_p("Werdykt „wzmocniony” nie znaczy „udowodniony”, a „osłabiony” nie
    znaczy „temat zamknięty”. Na tym etapie nie układamy planu od nowa.
    Wracamy do alternatywnych wyjaśnień, które konspekt z rozdziału 4
    zapisał przy każdym tropie, i dopisujemy nowe ",
    gloss("hipoteza badawcza", "hipotezy"), ", które podsunęły wyniki.
    Karty poniżej zestawiają dla każdego tropu wstępny wynik i plan
    dalszych sprawdzeń."),

  uiOutput("ch6_next_steps"),

  lc_p("Listy sprawdzeń przy różnych tropach powtarzają te same zmienne.
    Płeć jest osobnym tropem, ale pojawia się też jako alternatywne
    wyjaśnienie przy atrakcyjności i przy mniejszości. Typ kursu wraca
    przy płci, statusie native speaker i odsetku odpowiedzi. W konspekcie
    tropy zapisuje się osobno, żeby nie zgubić żadnego pytania. W analizie
    te same zmienne się spotykają, a to oznacza, że trzeba je sprawdzić
    razem, a nie po kolei."),

  lc_h2("sec-03", "Zanim zaufamy wynikowi: zmienne zakłócające"),

  lc_p("Pierwsze pytanie po każdym wyniku brzmi: czy to nie zasługa czegoś
    innego? Z rozdziału 06 wykładu 04 i rozdziału 03 wykładu 06 znamy ",
    gloss("zmienna zakłócająca", "zmienną zakłócającą"), ": to zmienna Z,
    która wiąże się jednocześnie z ", gloss("predyktor", "predyktorem"),
    " X i z wynikiem Y. Wtedy związek X z Y może częściowo, a nawet
    całkowicie, wynikać z tego, że obie zmienne zależą od Z. Jeśli Z wiąże
    się tylko z jedną z nich, nie może wytworzyć związku między nimi."),

  lc_p("Prześledźmy to dla głównego tropu, czyli związku atrakcyjności
    z oceną kursu. Zaczniemy od jednego kandydata, a potem przejrzymy
    pozostałe zmienne zbiorczo."),

  lc_h3("Przykład: czy płeć zakłóca związek atrakcyjności z oceną?"),

  lc_p("Płeć jest kandydatem na zmienną zakłócającą tylko wtedy, gdy wiąże
    się i z atrakcyjnością, i z oceną kursu. Panel pokazuje oba związki
    obok siebie: rozkład oceny atrakcyjności i rozkład oceny kursu
    w grupach kobiet i mężczyzn."),

  figure_panel(label = "Ryc. 6.1", title = "Płeć a obie zmienne relacji",
    lc_plots(
      lc_plot("ch6_conf_beauty", max_height = "300px"),
      lc_plot("ch6_conf_eval", max_height = "300px")
    ),
    uiOutput("ch6_conf_example_verdict")
  ),

  lc_p("Oba związki istnieją, ale idą w przeciwne strony. Kobiety dostają
    wyższe oceny atrakcyjności (", gloss("mediana"), " -0.06 wobec -0.24 u mężczyzn),
    a niższe oceny kursu (mediana 3.90 wobec 4.15). Płeć spełnia więc
    warunek zmiennej zakłócającej i związku atrakcyjności z oceną nie
    można czytać bez niej. Kierunek ma jednak znaczenie. Gdyby płeć
    wytwarzała ten związek, kobiety musiałyby mieć jednocześnie wyższą
    atrakcyjność i wyższą ocenę. Jest odwrotnie, więc uwzględnienie płci
    nie powinno osłabić związku, a raczej nieco go wzmocnić."),

  lc_h3("Pozostałe zmienne — tabela zbiorcza"),

  lc_p("Ten sam test przeprowadzamy dla wszystkich cech prowadzącego
    i kursu. Dla ", gloss("zmienna ilościowa", "zmiennych ilościowych"), " (wiek) siłę związku mierzy wartość
    bezwzględna ", gloss("korelacja", "korelacji"), " |r|, dla zmiennych grupujących — różnica median
    między grupami, wyrażona w jednostkach beauty albo w punktach oceny.
    Tabela nie wyznacza granicy, od której związek jest „wyraźny”. Szukamy
    zmiennych, których związek jest wyraźnie większy niż u pozostałych,
    i to zarówno z beauty, jak i z oceną kursu."),

  figure_panel(label = "Ryc. 6.2", title = "Związki cech prowadzącego i kursu z beauty i z oceną kursu",
    uiOutput("ch6_confounder_table")
  ),

  lc_p("Oprócz płci kandydatami są status native speaker i liczba punktów
    kursu. Ten drugi przypadek jest najwyraźniejszy, choć dotyczy tylko
    27 kursów jednopunktowych. Mają one średnią ocenę 4.52 wobec 3.97
    w pozostałych kursach, a medianę oceny atrakcyjności prowadzących
    -0.53 wobec -0.07. Podobnie jak przy płci, kierunki są przeciwne:
    kursy jednopunktowe łączą niższą atrakcyjność z wyższą oceną. Wiek,
    tenure, status mniejszościowy i poziom kursu mają wyraźny związek
    najwyżej z jedną ze zmiennych."),

  lc_p("To, że zmienna nie zakłóca związku, nie znaczy, że jest bez
    znaczenia. Wiek wiąże się z atrakcyjnością wyraźnie (|r| = 0.30),
    a z oceną kursu słabo (|r| = 0.05), więc nie jest kandydatem. Mówi jednak coś o samym pomiarze: ocena atrakcyjności
    w części odzwierciedla wiek prowadzącego. Pytanie, czy na ocenę kursu
    wpływa atrakcyjność, czy wiek, który się za nią kryje, warto sprawdzić
    w modelu, który uwzględni obie zmienne naraz."),

  lc_note("Zasada", rule = TRUE,
    "Zmienna może zakłócać związek X z Y tylko wtedy, gdy wiąże się i z X,
     i z Y. Gdy takich zmiennych jest kilka, prosty test nie wystarczy:
     trzeba je uwzględnić jednocześnie."),

  lc_p("Mamy już trzech kandydatów na zmienne zakłócające i kilka tropów,
    które na siebie zachodzą. Sprawdzanie ich parami nie da odpowiedzi,
    bo każda para pomija pozostałe zmienne. To zadanie dla modelu
    kontrolnego z rozdziału 7."),

  lc_h2("sec-04", "Czego brakuje w danych"),

  lc_p("Wiązka tropów pokazała też, czego w danych nie ma. Przy każdym
    tropie konspekt z rozdziału 4 zapisał w polu „Dostępne dane i braki”
    zmienne, których brakuje. Zebrane razem tworzą listę rzeczy, które
    najbardziej zmieniłyby interpretację oceny z ankiety:"),

  tags$ul(
    tags$li("Efekty uczenia się, na przykład wynik testu przed kursem
      i po nim. Bez nich nie wiemy, czy studenci faktycznie się nauczyli."),
    tags$li("Trudność kursu i obciążenie pracą."),
    tags$li("Oczekiwana ocena końcowa i łatwość zaliczenia."),
    tags$li("Informacja, czy kurs jest obowiązkowy."),
    tags$li("Styl prowadzenia i jakość materiałów."),
    tags$li("Powody, dla których część studentów nie wypełniła ankiety.")
  ),

  lc_p("Ta lista nie jest porażką analizy. To miejsce, w którym analiza
    danych przechodzi w projektowanie następnego badania. Pierwszy punkt
    jest najważniejszy: tylko on mierzy to, o co pyta cel, czyli jakość
    nauczania rozumianą jako to, czego studenci się nauczyli. Wszystkie
    zmienne w naszym zbiorze mówią o czymś innym. Przy własnym projekcie
    warto wybrać jeden brak, który najmocniej podważa obecny odczyt celu,
    i zapisać go w konspekcie jako ograniczenie."),

  lc_p("Wynik nie zamyka więc tematu, tylko wskazuje następne pytanie.
    Dla naszej wiązki są dwa: czy tropy utrzymają się, gdy uwzględnimy je
    razem, oraz czego nie da się sprawdzić na tych danych w ogóle."),

  lc_chapter_next("07", "Model kontrolny",
    "Sprawdziliśmy tropy pojedynczo — czas sprawdzić je wszystkie naraz, w jednym modelu.",
    "ch7")
  )
)

ch6_server <- function(input, output, session) {
  output$ch6_next_steps <- renderUI({
    cases <- list(
      beauty = list(
        narrative = "Atrakcyjność wiąże się z oceną kursu (r = 0.19). Teraz pytamy, czy ten związek nie wynika z innych cech prowadzącego albo kursu.",
        checks = c(
          "Sprawdzić alternatywy z konspektu: wiek, płeć i typ kursu.",
          "Zobaczyć, czy `beauty` współwystępuje z tymi zmiennymi.",
          "Zobaczyć, czy związek `beauty` z `eval` pozostaje widoczny, gdy te zmienne analizujemy razem."
        )
      ),
      gender = list(
        narrative = "Kursy prowadzone przez kobiety mają średnio o 0.17 punktu niższe oceny. Trzeba ustalić, czy płeć jest samodzielnym tropem, czy miesza się z innymi cechami kursu i prowadzącego.",
        checks = c(
          "Sprawdzić, z czym współwystępuje `gender`: typ kursu, response rate, wiek, atrakcyjność.",
          "Ocenić, czy wynik dla płci może być alternatywnym wyjaśnieniem dla innych tropów.",
          "Zobaczyć, czy efekt płci pozostaje widoczny, gdy inne zmienne analizujemy razem."
        )
      ),
      native = list(
        narrative = "Kursy prowadzone przez osoby, dla których angielski nie jest językiem ojczystym, mają średnio o 0.33 punktu niższe oceny, ale jest ich tylko 28. Trzeba ustalić, czy chodzi o odbiór prowadzącego, czy o kontekst kursów, które prowadzi ta grupa.",
        checks = c(
          "Sprawdzić, czy `native` współwystępuje z poziomem kursu, credits albo liczebnością grup.",
          "Ocenić, czy różnice między grupami mogą wynikać z nierównych liczebności albo rodzaju prowadzonych zajęć.",
          "Zobaczyć, czy `native` wnosi informację, gdy uwzględniamy inne cechy kursu i prowadzącego."
        )
      ),
      minority = list(
        narrative = "Różnica 0.12 punktu na niekorzyść prowadzących z grup mniejszościowych nie jest istotna, ale grupa liczy 64 kursy. Brak istotności przy takiej liczebności nie wyklucza różnicy, więc trop zostaje jako ostrożny sygnał.",
        checks = c(
          "Opisać liczebności grup, zanim interpretujemy różnice.",
          "Sprawdzić, czy `minority` współwystępuje z płcią, statusem `native`, typem kursu lub innymi cechami.",
          "Sprawdzić, czy różnica nie zmienia się po uwzględnieniu innych cech. Nową hipotezę formułować ostrożnie: wynik może wskazywać problem, ale nie dowodzi mechanizmu."
        )
      ),
      response = list(
        narrative = "Kursy z wyższym odsetkiem odpowiedzi mają wyższe oceny (r = 0.22). Trzeba sprawdzić, czy ankieta opisuje doświadczenie całej grupy, czy raczej głos wybranej części studentów.",
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
      tagList(
        lc_h3(tagList(tr$short, " · ", lc_verdict(row$verdict, type = if (row$supported) "ok" else "danger"))),
        p(tags$strong("Po wstępnym wyniku:"), " ", cases[[id]]$narrative),
        p(tags$strong("Co sprawdzamy dalej:")),
        tags$ul(
          lapply(cases[[id]]$checks, function(x) {
            tags$li(HTML(gsub("`([^`]+)`", "<code>\\1</code>", x)))
          })
        )
      )
    })
    tagList(cards)
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
    lc_status(
      tags$p(tags$strong("Płeć a atrakcyjność:"), paste0(" ", r$beauty_label, ". "),
        tags$strong("Płeć a ocena kursu:"), paste0(" ", r$eval_label, "."))
    )
  })

  output$ch6_confounder_table <- renderUI({
    rows <- lapply(tr_confounder_vars, tr_confounder_row)
    df <- data.frame(
      label = vapply(rows, function(r) r$label, character(1)),
      beauty = vapply(rows, function(r) r$beauty_label, character(1)),
      eval = vapply(rows, function(r) r$eval_label, character(1)),
      stringsAsFactors = FALSE
    )
    lc_table(df,
      cols = list(
        lc_col("label", "Zmienna", "row"),
        lc_col("beauty", "Związek z beauty", "text"),
        lc_col("eval", "Związek z eval", "text")
      ),
      narrow = "cards"
    )
  })
}
