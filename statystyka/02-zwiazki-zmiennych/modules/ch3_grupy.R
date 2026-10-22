# ============================================================================
# CHAPTER 3: Zmienna ilościowa w grupach zmiennej jakościowej
# ============================================================================

ch3_num_choices <- c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga",
                     "Średnia ocen" = "srednia_ocen",
                     "Czas dojazdu (min)" = "czas_dojazdu",
                     "Wydatki miesięczne (zł)" = "wydatki")
ch3_grp_choices <- c("Płeć" = "plec", "Kierunek" = "kierunek", "Rok studiów" = "rok_studiow")

ch3_ui <- list(
  id = "ch-grupy", num = "03", title = "Ilościowa i jakościowa",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Związki dwóch zmiennych",
      num    = "03",
      title  = "Liczby w grupach.",
      lead   = "Pensja to liczba, branża to kategoria. Czy branża mówi coś
                o pensji, sprawdzamy, porównując rozkłady pensji w kolejnych
                branżach: ich środki, rozrzut i to, jak bardzo na siebie
                zachodzą."
    ),

    lc_p("Gdy jedna zmienna jest ilościowa, a druga jakościowa, kategorie
      drugiej dzielą obserwacje na grupy. W każdej grupie zmienna ilościowa
      ma swój rozkład, a do jego opisu mamy już wszystkie narzędzia
      z wykładu 01: średnią, medianę, odchylenie standardowe, wykres
      pudełkowy. Związek polega na tym, że te rozkłady się różnią."),

    lc_h2("ch3-branze", "Pensje w trzech branżach"),

    figure_panel(
      label = "Ryc. 3.1",
      width_mode = "text",
      frames_widget("ch3_pay", "Ile zarabia absolwent?",
        steps = c("Pensje", "Według branży", "Typowa pensja", "Pudełka"),
        config = list(kind = "branze", names = branch_levels,
                      b = as.integer(pay_data$branza) - 1L, x = pay_data$pensja,
                      xmin = 3, xmax = 17,
                      stats = lapply(seq_len(nrow(pay_stats)), function(k) as.list(pay_stats[k, ])),
                      aria = "Pensje 90 absolwentów rozdzielają się na trzy branże; przy każdej pojawia się mediana i wykres pudełkowy"))
    ),

    lc_p("W kadrze 2 pensje rozdzieliły się na trzy ", gloss("rozkład warunkowy",
      "rozkłady warunkowe"), ", po jednym na branżę. Najprościej porównać je
      przez wartość typową: mediana pensji wynosi 5.8 tys. zł w branży rolniczej,
      7.7 tys. zł w biotechnologii i 9.5 tys. zł w branży kosmicznej. Różnica
      median między branżą kosmiczną a rolniczą, 3.7 tys. zł, to liczba, która
      opisuje siłę związku w tych danych. Można też porównać średnie: w branży
      kosmicznej średnia wynosi 10.0 tys. zł i jest wyższa od mediany, bo kilka
      bardzo wysokich pensji ciągnie ją w górę. Przy prawoskośnych zarobkach
      mediana lepiej opisuje typowego absolwenta."),

    lc_p(gloss("wykres pudełkowy", "Pudełka"), " z kadru 4 pokazują więcej niż
      środek. Pudełko branży rolniczej jest wąskie: połowa absolwentów zarabia
      tam od 5.2 do 6.2 tys. zł. W branży kosmicznej środkowa połowa rozciąga się
      od 7.8 do 11.1 tys. zł, więc pensje różnią się tam znacznie bardziej.
      Rozkłady branż się przy tym zachodzą: 11 z 30 absolwentów branży kosmicznej
      zarabia mniej niż najlepiej opłacany absolwent branży rolniczej
      (8.3 tys. zł)."),

    lc_warn("Pułapka",
      "Różnica median opisuje typowe osoby w grupach, nie każdą osobę. Zdanie
       „w branży kosmicznej zarabia się więcej” nie znaczy, że każdy
       absolwent tej branży zarabia więcej niż każdy absolwent rolnictwa."
    ),

    lc_h2("ch3-ankieta", "Grupy w ankiecie"),

    lc_p("Panel poniżej porównuje grupy w ankiecie studentów. Wybierz zmienną
      ilościową i zmienną, która dzieli studentów na grupy. Pudełka stoją
      obok siebie na wspólnej osi, a tabela pod wykresem podaje liczebność,
      średnią, medianę i odchylenie standardowe w każdej grupie."),

    figure_panel(
      label = "Ryc. 3.2",
      title = "Zmienna ilościowa w grupach",
      width_mode = "text",
      lc_toolbar(
        lc_group("Zmienna",
          lc_select("ch3_num", ch3_num_choices, selected = "wzrost",
                    aria_label = "Zmienna ilościowa")),
        lc_group("Grupy",
          lc_select("ch3_grp", ch3_grp_choices, selected = "plec",
                    aria_label = "Zmienna dzieląca na grupy"))
      ),
      lc_plot("ch3_box_plot", ratio = "2/1"),
      uiOutput("ch3_box_table"),
      uiOutput("ch3_box_caption")
    ),

    lc_p("Wzrost i płeć są wyraźnie związane: mediana wzrostu wynosi 166.0 cm
      u kobiet i 177.1 cm u mężczyzn, a pudełka prawie na siebie nie
      zachodzą. Tylko 3 z 99 kobiet są wyższe od mediany mężczyzn. Średnia ocen
      i kierunek studiów to przeciwny przypadek: mediany leżą między 3.75
      a 3.95, a pudełka stoją niemal w jednej linii. Znajomość kierunku prawie
      nic nie mówi o średniej ocen. W grupach osób niebinarnych i tych, które
      nie podały płci, jest tylko 13 i 6 osób, więc ich mediany mogą łatwo
      wypaść daleko od pozostałych bez żadnego związku z płcią."),

    lc_chapter_next(
      num       = "04",
      title     = "Dwie zmienne ilościowe",
      lead      = "staż, pensja i współczynnik korelacji.",
      target_id = "ch-korelacja"
    )
  )
)

ch3_server <- function(input, output, session) {

  scene_texts(input, output, "ch3_pay", list(
    tagList("Rok po studiach 90 absolwentów podaje swoją pensję brutto. Każda kropka
      to jedna osoba; połowa zarabia mniej niż 7.3 tys. zł."),
    tagList("Ci sami absolwenci pracują w trzech branżach: rolniczej,
      biotechnologicznej i kosmicznej. Kropki przesuwają się do swoich rzędów,
      a każda zostaje przy swojej pensji."),
    tagList("W każdej branży zaznaczamy medianę: pensję, którą połowa absolwentów
      przekracza, a połowa nie."),
    tagList("Pudełka obejmują środkową połowę pensji w każdej branży. Rozkłady
      różnią się położeniem i szerokością, ale wyraźnie na siebie zachodzą.")
  ))

  ch3_df <- reactive({
    v <- input$ch3_num
    g <- input$ch3_grp
    req(v, g)
    data.frame(y = student_data[[v]], grupa = droplevels(factor(student_data[[g]])))
  })

  ch3_stats <- reactive({
    df <- ch3_df()
    do.call(rbind, lapply(split(df$y, df$grupa), function(x)
      data.frame(n = length(x), mean = mean(x), med = median(x), sd = sd(x))))
  })

  zoom_plot_server("ch3_box_plot", reactive({
    df <- ch3_df()
    k <- nlevels(df$grupa)
    v_label <- names(ch3_num_choices)[ch3_num_choices == input$ch3_num]
    g_label <- names(ch3_grp_choices)[ch3_grp_choices == input$ch3_grp]
    set.seed(1)
    ggplot(df, aes(x = y, y = grupa, color = grupa)) +
      geom_boxplot(outlier.shape = NA, fill = NA, color = upwr_secondary, width = 0.55) +
      geom_jitter(height = 0.18, width = 0, size = 1.8, alpha = 0.6) +
      scale_color_manual(values = upwr_cat_n(k), guide = "none") +
      scale_y_discrete(limits = rev, labels = scales::label_wrap(12)) +
      labs(x = v_label, y = g_label)
  }))

  output$ch3_box_table <- renderUI({
    st <- ch3_stats()
    digits <- if (input$ch3_num %in% c("srednia_ocen")) 2 else 1
    df <- data.frame(grupa = rownames(st), n = st$n, mean = st$mean, med = st$med, sd = st$sd,
                     stringsAsFactors = FALSE)
    lc_table(df, list(
      lc_col("grupa", names(ch3_grp_choices)[ch3_grp_choices == input$ch3_grp], "row"),
      lc_col("n", "n"),
      lc_col("mean", "Średnia", digits = digits, short = "x̄", desc = "średnia"),
      lc_col("med", "Mediana", digits = digits, short = "Me", desc = "mediana"),
      lc_col("sd", "Odchylenie standardowe", digits = digits, short = "SD", desc = "odchylenie standardowe")
    ))
  })

  output$ch3_box_caption <- renderUI({
    # Małe grupy (np. 6 osób, które nie podały płci) mają przypadkowe mediany,
    # więc podpis porównuje tylko grupy liczące co najmniej 20 osób.
    st <- ch3_stats()
    st <- st[st$n >= 20, , drop = FALSE]
    digits <- if (input$ch3_num %in% c("srednia_ocen")) 2 else 1
    hi <- which.max(st$med)
    lo <- which.min(st$med)
    lc_caption(tone = "info",
      paste0("Największa różnica median w grupach od 20 osób: ", rownames(st)[hi],
             " − ", rownames(st)[lo], " = "),
      tags$b(lc_fmt(st$med[hi] - st$med[lo], digits)), ".")
  })
}
