# Tab 3: Grupa — za mało danych (n=8), zły zbiór

ch3_ui <- lecture_chapter(id = "ch3", num = "3", title = "Grupa", content = tagList(

  lc_chapter_hero(
    kicker = "Rozdział 03 · Co czyni dobry zbiór danych?",
    num    = "03",
    title  = "Ankieta na grupie.",
    lead   = "Ten zbiór wygląda jak typowy projekt studencki, ale ma problem,
              którego nie da się naprawić kosmetyką: zbyt małe n."
  ),

  lc_h2("sec-01", "Opis"),

  lc_p("Drugie studium przypadku to typowy projekt zaliczeniowy. Dzień przed
    terminem oddania student pyta 8 znajomych o płeć, kierunek studiów,
    liczbę godzin nauki w tygodniu, poziom stresu w skali od 1 do 10
    i średnią ocen. Powstaje zbiór z 8 wierszami i 5 zmiennymi. Pytania,
    które chciałby na nim zadać, brzmią rozsądnie: czy osoby uczące się
    dłużej mają wyższą średnią, czy kobiety i mężczyźni różnią się poziomem
    stresu, czy kierunki różnią się ocenami. Zanim przeczytasz dalej, oceń
    sam, czy te dane pozwolą na nie odpowiedzieć."),

  lc_h2("sec-02", "Podgląd danych"),

  lc_p("W podglądzie policz, ile osób trafia do każdej grupy, którą
    chciałbyś porównać."),

  figure_panel(
    label = "Tab. 3.1",
    title = "Ankieta: 8 odpowiedzi",
    uiOutput("tab2_table")
  ),

  lc_p("Same zmienne nie budzą zastrzeżeń: dwie jakościowe (płeć, kierunek)
    i trzy ilościowe, bez braków i bez wartości spoza skali. Kłopot widać
    dopiero po policzeniu osób w grupach. Kobiet i mężczyzn jest po 4,
    a na kierunkach od 1 osoby (biologia) do 3 osób (psychologia).
    Porównanie kierunków opierałoby się więc na pojedynczych odpowiedziach,
    a porównanie płci na czterech osobach z każdej strony."),

  lc_h2("sec-03", "Ile obserwacji naprawdę potrzebujesz?"),

  lc_p("Z wykładu 03 wiemy, że precyzję oszacowania średniej opisuje
    szerokość ", gloss("przedział ufności", "przedziału ufności"), ".
    Zależy ona od rozrzutu danych i od liczby obserwacji:"),

  lc_formula_box(withMathJax(
    "$$\\text{szerokość} = 2 \\cdot t^*_{\\alpha/2,\\, n-1} \\cdot \\frac{s}{\\sqrt{n}}$$"
  )),

  lc_p("Z wykładu 04 wiemy z kolei, że ", gloss("moc testu"), " to
    prawdopodobieństwo, że test wykryje różnicę, która naprawdę istnieje.
    Panel pokazuje, jak od liczby obserwacji zależą trzy rzeczy: kształt
    histogramu, szerokość 95% przedziału ufności dla średniej ocen (przy
    odchyleniu standardowym 0.6) i moc testu t porównującego dwie
    równoliczne grupy, gdy różnica średnich wynosi pół odchylenia
    standardowego (d = 0.5)."),

  figure_panel(
    label = "Ryc. 3.1",
    title = "Liczba obserwacji a precyzja i moc",
    lc_slider("tab2_n", "Liczba obserwacji", 5, 200, 8, 1),
    fluidRow(
      column(6, zoom_plot_ui("tab2_hist", height = "280px")),
      column(6, zoom_plot_ui("tab2_ci", height = "280px"))
    ),
    zoom_plot_ui("tab2_power", height = "280px")
  ),

  lc_p("Wszystkie trzy wykresy prowadzą do tego samego wniosku. Przy n = 8
    histogram ma tylko kilka słupków i nie da się z niego odczytać kształtu
    rozkładu. W naszej ankiecie średnia ocen wynosi 3.71, odchylenie
    standardowe 0.62, a 95% przedział ufności rozciąga się od 3.20 do 4.23.
    Ma ponad 1 punkt szerokości, czyli nie odróżnia grupy ze średnią
    na poziomie 3.2 od grupy ze średnią 4.2. Szerokość
    maleje proporcjonalnie do 1/√n, więc każde kolejne zawężenie kosztuje
    coraz więcej obserwacji: przy 30 osobach przedział ma około 0.45 pkt
    szerokości, a przy 100 osobach około 0.24 pkt."),

  lc_p("Jeszcze gorzej wygląda moc. Przy 8 osobach, po 4 w każdej grupie,
    test t wykrywa różnicę d = 0.5 tylko w około 9% przypadków. W dziewięciu
    badaniach na dziesięć wynik byłby nieistotny, choć różnica istnieje.
    Moc 80% wymaga około 64 osób w każdej grupie, czyli ponad 120 łącznie.
    To problem „Za mało danych” z katalogu z rozdziału 1, i w tym zbiorze
    dotyka on każdej analizy naraz."),

  lc_h2("sec-04", "Werdykt"),

  lc_p("Tego zbioru nie da się uratować czyszczeniem ani przekodowaniem, bo
    brakuje w nim nie jakości, lecz informacji. Jedyną naprawą jest zebranie
    większej liczby odpowiedzi, najlepiej po wcześniejszym oszacowaniu, ile
    osób potrzeba do wykrycia różnicy, która nas interesuje. Przy tym
    szacowaniu liczy się liczebność w każdej porównywanej grupie, a nie
    łączna liczba wierszy: 30 osób podzielonych na trzy kierunki to tylko
    po 10 osób na kierunek."),

  lc_p("Z 8 odpowiedzi można co najwyżej opisać te konkretne osoby, na
    przykład tabelą lub wykresem punktowym. Testy, przedziały ufności
    i regresja wymagają uogólnienia na populację studentów, a ta
    próba jest za mała i w dodatku dobrana spośród znajomych."),

  lc_note("Werdykt",
    "Zbiór do odrzucenia: przy 8 osobach żadna analiza wnioskująca nie da
    wiarygodnego wyniku, a jedyną naprawą jest zebranie nowych danych."),

  lc_chapter_next(
    num = "04",
    title = "Pingwiny",
    lead = "Kolejny zbiór ma kilkaset obserwacji i drobne braki, które
            da się uczciwie obsłużyć.",
    target_id = "ch4"
  ),

  div(style = "height: 40px;")
))

ch3_server <- function(input, output, session) {

  output$tab2_table <- renderUI({
    dd_data_table(round_df(small_data), n = 10)
  })

  # Slider simulations
  sim_data <- reactive({
    n <- input$tab2_n
    set.seed(42)
    data.frame(
      godziny = rnorm(n, 15, 5),
      oceny = rnorm(n, 3.8, 0.6)
    )
  })

  zoom_plot_server("tab2_hist", reactive({
    d <- sim_data()
    ggplot(d, aes(x = oceny)) +
      geom_histogram(bins = max(5L, round(input$tab2_n / 5)), fill = data_primary, color = "white", alpha = 0.8) +
      labs(x = "Średnia ocen", y = "Liczebność") +
      theme_upwr(base_size = 14)
  }))

  zoom_plot_server("tab2_ci", reactive({
    ns <- seq(5, 200, by = 5)
    ci_widths <- 2 * qt(0.975, ns - 1) * 0.6 / sqrt(ns)  # assuming SD = 0.6
    df_ci <- data.frame(n = ns, ci_width = ci_widths)

    ggplot(df_ci, aes(x = n, y = ci_width)) +
      geom_line(color = data_bad, linewidth = 1.2) +
      geom_point(data = df_ci[df_ci$n == max(ns[ns <= input$tab2_n]), ],
                 color = data_bad, size = 4) +
      labs(x = "Liczba obserwacji (n)", y = "Szerokość przedziału ufności") +
      theme_upwr(base_size = 14)
  }))

  zoom_plot_server("tab2_power", reactive({
    ns <- seq(5, 200, by = 5)
    # Power simulation: detect effect size d=0.5
    powers <- sapply(ns, function(n) {
      set.seed(123)
      rejections <- replicate(500, {
        x <- rnorm(n / 2, 0, 1)
        y <- rnorm(n / 2, 0.5, 1)  # effect size d = 0.5
        t.test(x, y)$p.value < 0.05
      })
      mean(rejections)
    })
    df_pow <- data.frame(n = ns, power = powers)

    ggplot(df_pow, aes(x = n, y = power)) +
      geom_line(color = data_primary, linewidth = 1.2) +
      geom_point(data = df_pow[df_pow$n == max(ns[ns <= input$tab2_n]), ],
                 color = data_primary, size = 4) +
      geom_hline(yintercept = 0.8, linetype = "dashed", color = data_reference) +
      annotate("text", x = 150, y = 0.83, label = "Moc 80%", color = data_reference, size = 4) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(x = "Liczba obserwacji (n)", y = "Moc testu") +
      theme_upwr(base_size = 14)
  }))

}
