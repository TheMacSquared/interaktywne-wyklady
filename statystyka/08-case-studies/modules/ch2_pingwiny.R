# ============================================================================
# CASE STUDY 2: Pingwiny Palmera
# Pytanie: Czy trzy gatunki pingwinów różnią się masą ciała?
# ============================================================================

pg <- palmerpenguins::penguins
pg <- pg[!is.na(pg$body_mass_g), ]
pg$species <- factor(pg$species, levels = c("Adelie", "Chinstrap", "Gentoo"))
pg_sex <- pg[!is.na(pg$sex), ]
pg_sex$plec <- factor(pg_sex$sex, levels = c("female", "male"),
                      labels = c("Samice", "Samce"))

pg_cols <- c(Adelie = case_explore, Chinstrap = case_conclude, Gentoo = case_model)

ch2_ui <- lecture_chapter(id = "ch2", num = "2", title = "Pingwiny", content = tagList(

  lc_chapter_hero(
    kicker = "Rozdział 02 · Studium przypadku",
    num    = "02",
    title  = "Trzy gatunki, jedna waga.",
    lead   = "Gentoo ważą o prawie półtora kilograma więcej niż dwa pozostałe
              gatunki, i to widać od razu. Dużo trudniej powiedzieć coś
              o dwóch gatunkach, które na wykresie wyglądają tak samo."
  ),

  # ========================================================================
  # KONTEKST
  # ========================================================================
  lc_h2("ch2-sytuacja", "Sytuacja wyjściowa"),

  lc_p("Drugie studium dotyczy porównania kilku grup. Narzędzia pochodzą
    z wykładów 01, 04 i 05: opis grup, sprawdzenie założeń, ",
    gloss("ANOVA"), " i porównania par. W rozdziale 01 wystarczyła klasyczna
    ANOVA z testem Tukeya. Tu dane same podpowiedzą, że lepiej sięgnąć po
    jej odporną wersję."),

  lc_p("Dane zebrano w latach 2007–2009 w stacji badawczej Palmer na
    Antarktydzie. Opisują 344 pingwiny trzech gatunków (Adelie, Chinstrap
    i Gentoo) z trzech wysp. Znamy je z rozdziału 03B wykładu 06, gdzie
    służyły do pokazania paradoksu Simpsona na wymiarach dzioba. Tym razem
    zajmiemy się masą ciała. Biolog planujący dalsze badania chce wiedzieć,
    czy gatunki różnią się przeciętną masą, o ile i czy wniosek nie zależy
    od tego, jak wielu samców i samic jest w każdej grupie."),

  lc_p("Analizę przeprowadzimy w czterech krokach:"),

  tags$ol(
    tags$li("Opis grup: rozkład masy w każdym gatunku (wykład 01)."),
    tags$li("Założenia: normalność i jednorodność wariancji w grupach
      (wykład 05), a na ich podstawie wybór testu."),
    tags$li("Test dla trzech grup i porównania par (wykład 04)."),
    tags$li("Sprawdzenie, czy wniosek zmienia się po podziale na płeć.")
  ),

  # ========================================================================
  # KROK 1: Opis grup
  # ========================================================================
  lc_h2("ch2-opis", "Krok 1: Opis grup"),

  lc_p("Dla dwóch pingwinów brakuje pomiaru masy, więc analiza obejmuje 342
    ptaki. Panel pokazuje rozkład masy w każdym gatunku: ",
    gloss("wykres pudełkowy", "wykres pudełkowy"), " z naniesionymi
    pojedynczymi ptakami. Odczyty podają średnią masę gatunku."),

  figure_panel(
    label = "Ryc. 2.1",
    title = "Masa ciała w trzech gatunkach",
    lc_toolbar(lc_readouts(uiOutput("ch2_desc_reads"))),
    lc_plot("ch2_desc_plot", ratio = "1.6/1", max_height = "340px")
  ),

  lc_p("Gentoo są wyraźnie cięższe: średnio 5076 g (",
    gloss("odchylenie standardowe"), " 504 g, n = 123). Adelie (3701 g,
    SD 459 g, n = 151) i Chinstrap (3733 g, SD 384 g, n = 68) ważą prawie
    tyle samo, a ich pudełka niemal się pokrywają. Grupy nie są równe
    liczebnie: Chinstrap jest ponad dwa razy mniej niż Adelie. Gatunki
    różnią się też miejscem występowania. Gentoo pochodzą tylko z wyspy
    Biscoe, Chinstrap tylko z Dream, a Adelie ze wszystkich trzech wysp,
    więc różnicy między gatunkami nie da się tu oddzielić od różnicy między
    wyspami."),

  # ========================================================================
  # KROK 2: Założenia
  # ========================================================================
  lc_h2("ch2-zalozenia", "Krok 2: Założenia i wybór testu"),

  lc_p("Klasyczna ANOVA zakłada, że w każdej grupie rozkład jest w przybliżeniu
    normalny, a wariancje grup są równe. Jak w wykładzie 05, normalność
    oceniamy na ", gloss("wykres kwantyl-kwantyl", "wykresach Q-Q"), "
    w grupach, a równość wariancji porównując odchylenia standardowe
    i ", gloss("test Levene'a", "testem Levene'a"), "."),

  figure_panel(
    label = "Ryc. 2.2",
    title = "Wykresy Q-Q masy ciała w gatunkach",
    lc_plot("ch2_qq_plot", ratio = "2.4/1", max_height = "300px"),
    uiOutput("ch2_assump_table")
  ),

  lc_p("Punkty na wykresach Q-Q leżą blisko prostej we wszystkich gatunkach.
    ", gloss("test Shapiro-Wilka", "Test Shapiro-Wilka"), " daje p = 0.56
    dla Chinstrap i p = 0.23 dla Gentoo, a dla Adelie p = 0.03. Przy 151
    ptakach tak małe odchylenie nie zagraża testowi, bo średnie grup mają
    rozkład bliski normalnemu niezależnie od kształtu danych. Inaczej
    z wariancjami: test Levene'a odrzuca ich równość (F(2, 339) = 5.12,
    p = 0.006). Największe odchylenie standardowe (504 g) jest o jedną
    trzecią większe niż najmniejsze (384 g), a grupy mają bardzo różne
    liczebności."),

  case_quiz_ui("ch2_quiz_test",
    title = "Który test wybrać?",
    question = "Trzy grupy o różnej liczebności, rozkłady bliskie normalnym,
      wariancje różne (test Levene'a p = 0.006). Który test porówna średnie
      najlepiej?",
    choices = c(
      "Klasyczna ANOVA, bo rozkłady są normalne." = "classic",
      "ANOVA Welcha, a po niej porównania par Games-Howella." = "welch",
      "Test Kruskala-Wallisa, bo jedno z założeń nie jest spełnione." = "kw",
      "Trzy testy t dla par, bez żadnej poprawki." = "ttests"
    ),
    correct = "welch"
  ),

  lc_p("Rozdział 02 wykładu 05 zalecał w takiej sytuacji parę ANOVA Welcha
    i ", gloss("test Games-Howella", "test Games-Howella"), ". Oba nie
    zakładają równych wariancji, a przy grupach o różnej liczebności
    klasyczna ANOVA może wtedy dawać za dużo albo za mało fałszywych
    alarmów. Test Kruskala-Wallisa odpowiadałby na inne pytanie, o całe
    rozkłady, a nie o średnie, i nie jest potrzebny, skoro rozkłady są
    bliskie normalnym."),

  # ========================================================================
  # KROK 3: Test i porównania par
  # ========================================================================
  lc_h2("ch2-test", "Krok 3: Test dla trzech grup i porównania par"),

  lc_p("Sprawdzamy ", gloss("hipoteza zerowa", "hipotezę zerową"), ", że
    średnia masa jest taka sama we wszystkich trzech gatunkach. Jeśli ją
    odrzucimy, test Games-Howella wskaże, które pary się różnią, i poda
    przedział ufności dla każdej różnicy."),

  figure_panel(
    label = "Ryc. 2.3",
    title = "Różnice średnich masy między parami gatunków",
    lc_toolbar(lc_readouts(uiOutput("ch2_welch_reads"))),
    lc_plot("ch2_gh_plot", ratio = "2.2/1", max_height = "260px"),
    uiOutput("ch2_gh_table")
  ),

  lc_p("ANOVA Welcha odrzuca hipotezę o równych średnich (F(2, 189) = 318,
    p < 0.001). Różnice dotyczą jednak tylko Gentoo. Są one cięższe od
    Adelie o 1375 g (95% przedział ufności od 1237 do 1514 g) i od
    Chinstrap o 1343 g (od 1189 do 1497 g). Podział na gatunki wyjaśnia
    67% zmienności masy (", gloss("eta kwadrat", "η²"), " = 0.67). Między
    Adelie a Chinstrap różnica wynosi 32 g, a przedział ufności sięga od
    -109 do 174 g (p = 0.85)."),

  case_quiz_ui("ch2_quiz_ns",
    title = "Co znaczy p = 0.85 dla Adelie i Chinstrap?",
    question = "Różnica średnich masy: 32 g, 95% przedział ufności od -109
      do 174 g, p = 0.85. Które zdanie jest poprawne?",
    choices = c(
      "Adelie i Chinstrap ważą przeciętnie tyle samo." = "equal",
      "Chinstrap są cięższe od Adelie o 32 g." = "heavier",
      "Dane nie pozwalają stwierdzić różnicy; mieści się w nich zarówno brak różnicy, jak i różnica rzędu 150 g w każdą stronę." = "ci",
      "Test był źle dobrany, bo nie wykrył różnicy." = "wrong_test"
    ),
    correct = "ci"
  ),

  lc_p("Brak podstaw do odrzucenia hipotezy zerowej nie dowodzi, że średnie
    są równe. Przedział ufności mówi więcej niż sama p-wartość: zgodne
    z danymi są różnice od około 110 g na korzyść Adelie do około 170 g na
    korzyść Chinstrap. Biolog może więc powiedzieć, że te gatunki nie
    różnią się wyraźnie, ale nie, że ważą tyle samo."),

  # ========================================================================
  # KROK 4: Płeć
  # ========================================================================
  lc_h2("ch2-plec", "Krok 4: Czy wniosek zależy od płci?"),

  lc_p("Samce pingwinów są cięższe od samic, więc różnica między gatunkami
    mogłaby częściowo wynikać z tego, ile samców i samic trafiło do każdej
    grupy. Tak jak ubóstwo w rozdziale 01, płeć jest kandydatem na ",
    gloss("zmienna zakłócająca", "zmienną zakłócającą"), ". Dla 9 ptaków
    płeć nie jest znana, więc ten krok obejmuje 333 pingwiny. Panel
    porównuje gatunki osobno wśród samic i wśród samców."),

  figure_panel(
    label = "Ryc. 2.4",
    title = "Masa ciała gatunków osobno dla samic i samców",
    lc_toolbar(
      lc_segmented("ch2_sex_group", "Grupa",
        choices = c("Samice" = "Samice", "Samce" = "Samce")),
      lc_readouts(uiOutput("ch2_sex_reads"))
    ),
    lc_plot("ch2_sex_plot", ratio = "1.6/1", max_height = "340px")
  ),

  lc_p("W każdym gatunku jest prawie tyle samo samców co samic (73 i 73
    Adelie, 34 i 34 Chinstrap, 61 i 58 Gentoo), więc proporcje płci nie
    tłumaczą różnic między gatunkami. Gentoo pozostają najcięższe w obu
    płciach. Samce są cięższe od samic o 674 g u Adelie, 412 g u Chinstrap
    i 805 g u Gentoo."),

  lc_p("Podział na płeć zmienia jednak obraz Adelie i Chinstrap. Wśród samic
    Chinstrap są cięższe o 158 g (przedział od 18 do 298 g, p = 0.02),
    a wśród samców lżejsze o 105 g (od -283 do 74 g, p = 0.34). W danych
    łącznych te dwie różnice o przeciwnych znakach się znoszą. Wniosek
    „Adelie i Chinstrap ważą podobnie” jest więc prawdziwy dla gatunków
    jako całości, ale nie dla każdej płci osobno."),

  # ========================================================================
  # WNIOSKI
  # ========================================================================
  lc_h2("ch2-wnioski", "Odpowiedź i ograniczenia"),

  lc_p("Gatunki różnią się masą ciała, ale ta różnica dotyczy głównie Gentoo,
    które ważą średnio o około 1.4 kg więcej niż Adelie i Chinstrap. Wynik
    jest wyraźny i utrzymuje się w obu płciach. Między Adelie i Chinstrap
    dane nie pokazują wyraźnej różnicy dla gatunków jako całości. Po
    podziale na płeć pojawia się niewielka różnica wśród samic, w przeciwną
    stronę niż wśród samców."),

  tags$ul(
    tags$li("Gatunek pokrywa się z wyspą: Gentoo i Chinstrap pochodzą każdy
      z jednej wyspy. Różnica między gatunkami może częściowo być różnicą
      między miejscami żerowania."),
    tags$li("Ptaki ważono w określonym okresie sezonu lęgowego. Masa
      pingwinów zmienia się w ciągu roku, więc wynik dotyczy tego okresu."),
    tags$li("Różnica wśród samic (p = 0.02) to jedno z kilku porównań
      wykonanych po obejrzeniu danych łącznych. Przy wielu porównaniach
      pojedynczy wynik bliski 0.05 łatwo okazuje się przypadkiem.")
  ),

  lc_note("Zasada", rule = TRUE,
    "Wybór testu wynika z danych: najpierw opis grup i założenia, potem test.
     Po teście warto sprawdzić, czy wniosek dla całości zgadza się z wnioskiem
     w podgrupach.")
))

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  fmt0 <- function(v) formatC(v, format = "f", digits = 0)

  # --- Krok 1 ---
  output$ch2_desc_reads <- renderUI({
    m <- tapply(pg$body_mass_g, pg$species, mean)
    tagList(lapply(names(m), function(s)
      lc_readout(s, paste0(fmt0(m[[s]]), " g"), color = pg_cols[[s]], swatch = TRUE)))
  })

  zoom_plot_server("ch2_desc_plot", reactive({
    ggplot(pg, aes(species, body_mass_g, fill = species, colour = species)) +
      geom_boxplot(alpha = 0.25, outlier.shape = NA, width = 0.55) +
      geom_jitter(width = 0.15, height = 0, alpha = 0.45, size = 1.6) +
      scale_fill_manual(values = pg_cols, guide = "none") +
      scale_colour_manual(values = pg_cols, guide = "none") +
      labs(x = NULL, y = "Masa ciała (g)")
  }), alt = "Wykres pudełkowy masy ciała trzech gatunków pingwinów.")

  # --- Krok 2 ---
  zoom_plot_server("ch2_qq_plot", reactive({
    ggplot(pg, aes(sample = body_mass_g, colour = species)) +
      stat_qq_line(colour = case_reference, linetype = "dashed") +
      stat_qq(alpha = 0.6, size = 1.4) +
      facet_wrap(~species, nrow = 1, scales = "free_y") +
      scale_colour_manual(values = pg_cols, guide = "none") +
      labs(x = "Kwantyle teoretyczne", y = "Masa ciała (g)")
  }), alt = "Wykresy kwantyl-kwantyl masy ciała w trzech gatunkach.")

  output$ch2_assump_table <- renderUI({
    sw <- as.data.frame(rstatix::shapiro_test(dplyr::group_by(pg, species), body_mass_g))
    sds <- tapply(pg$body_mass_g, pg$species, sd)
    ns <- table(pg$species)
    lev <- as.data.frame(rstatix::levene_test(pg, body_mass_g ~ species))
    tagList(
      lc_table(
        data.frame(gat = levels(pg$species), n = as.integer(ns),
                   sd = as.numeric(sds), p = lc_pval(sw$p)),
        cols = list(
          lc_col("gat", "Gatunek", "row"),
          lc_col("n", "n"),
          lc_col("sd", "SD", digits = 0, suffix = " g"),
          lc_col("p", "p (Shapiro-Wilk)")
        )
      ),
      lc_caption(HTML(paste0("Test Levene'a: F(", lev$df1, ", ", lev$df2, ") = ",
                             formatC(lev$statistic, format = "f", digits = 2),
                             ", p = ", lc_pval(lev$p), ".")))
    )
  })

  case_quiz_server(input, output, "ch2_quiz_test", "welch", list(
    classic = "Normalność wystarcza tylko połowicznie: klasyczna ANOVA zakłada
      też równe wariancje, a test Levene'a je odrzuca, i to przy grupach
      o różnej liczebności.",
    welch = "ANOVA Welcha nie zakłada równych wariancji, a Games-Howell to
      dopasowane do niej porównania par.",
    kw = "Test Kruskala-Wallisa porównuje całe rozkłady, nie średnie. Przy
      rozkładach bliskich normalnym wystarczy poprawić założenie
      o wariancjach, nie trzeba porzucać średnich.",
    ttests = "Trzy osobne testy t bez poprawki zawyżają ryzyko fałszywego
      alarmu. Do tego służą porównania post hoc."
  ))

  # --- Krok 3 ---
  ch2_gh <- as.data.frame(rstatix::games_howell_test(pg, body_mass_g ~ species))
  ch2_gh$para <- paste(ch2_gh$group2, "−", ch2_gh$group1)

  output$ch2_welch_reads <- renderUI({
    w <- as.data.frame(rstatix::welch_anova_test(pg, body_mass_g ~ species))
    a <- aov(body_mass_g ~ species, pg)
    ss <- summary(a)[[1]][["Sum Sq"]]
    tagList(
      lc_readout("F Welcha", formatC(w$statistic, format = "f", digits = 0)),
      lc_readout("p", HTML(lc_pval(w$p))),
      lc_readout("η²", formatC(ss[1] / sum(ss), format = "f", digits = 2))
    )
  })

  zoom_plot_server("ch2_gh_plot", reactive({
    d <- ch2_gh
    d$para <- factor(d$para, levels = rev(d$para))
    d$sig <- d$p.adj < 0.05
    ggplot(d, aes(estimate, para, colour = sig)) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = case_reference) +
      geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2,
                    orientation = "y", linewidth = 0.9) +
      geom_point(size = 3) +
      scale_colour_manual(values = c(`TRUE` = case_model, `FALSE` = case_highlight),
                          guide = "none") +
      labs(x = "Różnica średnich masy (g) z 95% przedziałem ufności", y = NULL)
  }), alt = "Różnice średnich masy między parami gatunków z przedziałami ufności.")

  output$ch2_gh_table <- renderUI({
    d <- ch2_gh
    lc_table(
      data.frame(para = d$para, est = d$estimate,
                 ci = paste0(fmt0(d$conf.low), " do ", fmt0(d$conf.high)),
                 p = lc_pval(d$p.adj)),
      cols = list(
        lc_col("para", "Para", "row"),
        lc_col("est", "Różnica", digits = 0, suffix = " g"),
        lc_col("ci", "95% przedział (g)", "text"),
        lc_col("p", "p (Games-Howell)")
      )
    )
  })

  case_quiz_server(input, output, "ch2_quiz_ns", "ci", list(
    equal = "Brak istotnej różnicy nie dowodzi równości. Przedział ufności
      obejmuje różnice rzędu 100–170 g w obie strony.",
    heavier = "32 g to różnica w tej próbie. Przedział ufności obejmuje zero
      i wartości ujemne, więc kierunek różnicy w populacji jest nieznany.",
    ci = "Przedział od -109 do 174 g obejmuje zero, ale też różnice, które
      biolog mógłby uznać za istotne praktycznie.",
    wrong_test = "Test był dobrany do danych. To, że nie wykrył różnicy,
      wynika z danych, a nie z wyboru testu."
  ))

  # --- Krok 4 ---
  ch2_sex_gh <- reactive({
    d <- pg_sex[pg_sex$plec == (input$ch2_sex_group %||% "Samice"), ]
    list(d = d, gh = as.data.frame(rstatix::games_howell_test(d, body_mass_g ~ species)))
  })

  output$ch2_sex_reads <- renderUI({
    x <- ch2_sex_gh()
    m <- tapply(x$d$body_mass_g, x$d$species, mean)
    ac <- x$gh[x$gh$group1 == "Adelie" & x$gh$group2 == "Chinstrap", ]
    tagList(
      lapply(names(m), function(s)
        lc_readout(s, paste0(fmt0(m[[s]]), " g"), color = pg_cols[[s]], swatch = TRUE)),
      lc_readout("Chinstrap − Adelie",
                 HTML(paste0(fmt0(ac$estimate), " g, p = ", lc_pval(ac$p.adj))))
    )
  })

  zoom_plot_server("ch2_sex_plot", reactive({
    d <- ch2_sex_gh()$d
    ggplot(d, aes(species, body_mass_g, fill = species, colour = species)) +
      geom_boxplot(alpha = 0.25, outlier.shape = NA, width = 0.55) +
      geom_jitter(width = 0.15, height = 0, alpha = 0.45, size = 1.6) +
      scale_fill_manual(values = pg_cols, guide = "none") +
      scale_colour_manual(values = pg_cols, guide = "none") +
      coord_cartesian(ylim = c(2600, 6400)) +
      labs(x = NULL, y = "Masa ciała (g)")
  }), alt = "Masa ciała trzech gatunków osobno dla samic albo samców.")
}
