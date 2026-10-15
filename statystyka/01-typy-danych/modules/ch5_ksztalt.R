# ============================================================================
# CHAPTER 5: Kształt rozkładu
# ============================================================================

# PROTOTYP mapa-narzedzi: siatka statystyk (HTML/CSS, style w app.R)
ch5_tools_map <- function() {
  chip <- function(txt, color = NULL) {
    tags$span(class = "tmap-chip",
      style = if (!is.null(color)) paste0("--tmap-c:", color, ";"), txt)
  }
  cell <- function(...) tags$div(class = "tmap-cell", ...)
  tags$div(class = "tmap", role = "img",
    `aria-label` = "Mapa narzędzi: położenie — średnia (wrażliwa), mediana i dominanta (odporne); rozrzut — odchylenie standardowe, wariancja, rozstęp (wrażliwe), IQR (odporny).",
    tags$div(class = "tmap-corner"),
    tags$div(class = "tmap-head", "Położenie"),
    tags$div(class = "tmap-head", "Rozrzut"),
    tags$div(class = "tmap-row", "Wrażliwe na wartości skrajne"),
    cell(chip("średnia", "var(--upwr-accent)")),
    cell(chip("SD"), chip("wariancja"), chip("rozstęp")),
    tags$div(class = "tmap-row", "Odporne"),
    cell(chip("mediana", stat_colors[["Mediana"]]), chip("dominanta")),
    cell(chip("IQR"))
  )
}

ch5_ui <- list(
  id = "ch-ksztalt", num = "05", title = "Kształt rozkładu",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 05 · Statystyka opisowa",
      num    = "05",
      title  = "Kształt rozkładu.",
      lead   = "Dwa rozkłady mogą mieć tę samą średnią i to samo odchylenie
                standardowe, a wyglądać zupełnie inaczej. Położenie i rozrzut
                to nie wszystko: liczy się też to, czy rozkład jest symetryczny
                i jak często trafiają się w nim wartości skrajne."
    ),

    uiOutput("tracker_ch5"),

    lc_p("W rozdziale 3 opisywaliśmy, gdzie leży środek danych, a w rozdziale 4,
      jak bardzo dane są wokół niego rozproszone. Te dwie informacje nie mówią
      jeszcze, jak wygląda histogram. Rozkład może mieć długi ogon po jednej
      stronie albo częściej niż zwykle wyrzucać wartości daleko od środka.
      Do opisu kształtu służą dwie kolejne miary: skośność i kurtoza."),

    # --- Widget 1: Skewness ---
    lc_h2("ch5-skosnosc", "Skośność (asymetria)"),

    lc_p("W rozdziale 3 zauważyliśmy, że dla czasu dojazdu ", gloss("średnia"), " (35.7 min)
      jest wyraźnie większa od ", gloss("mediana", "mediany"), " (32.9 min). Przyczyną był długi prawy
      ogon: kilka bardzo długich dojazdów podnosiło średnią, a mediana na nie
      nie reagowała. Taką asymetrię rozkładu mierzy ",
      gloss("skośność"), ". Liczymy ją, standaryzując każdą ", gloss("obserwacja", "obserwację"), ", czyli
      wyrażając jej odległość od średniej w ", gloss("odchylenie standardowe", "odchyleniach standardowych"), ",
      a następnie uśredniając trzecie potęgi tych odległości."),

    lc_formula_annotated(
      "⟦1|g_1⟧ = \\frac{1}{⟦2|n⟧} ⟦3|\\sum_{i=1}^{n}⟧ \\left( ⟦4|\\frac{x_i - \\bar{x}}{s}⟧ \\right)^{⟦5|3⟧}",
      notes = list(
        "skośność",
        "liczba obserwacji",
        "suma",
        "obserwacja standaryzowana",
        "trzecia potęga"
      ),
      colors = c("indygo", "szalwia", "bursztyn", "niebo", "terakota"),
      above = c(3, 4, 5)
    ),

    # PROTOTYP skosnosc-kroki: krokowa budowa skośności (standaryzacja → z³ → bilans)
    lc_p("Panel przeprowadza to obliczenie na danych z ankiety. Najpierw
      zamieniamy każdą obserwację na odległość od średniej w odchyleniach
      standardowych, potem podnosimy te odległości do trzeciej potęgi,
      a na końcu sumujemy osobno wkłady z lewej i z prawej strony."),

    figure_panel(
      label = "Ryc. 5.0a",
      lc_step_widget("ch5_sk",
        title = "Obliczanie skośności",
        steps = c("Dane", "Standaryzacja", "Trzecie potęgi", "Bilans"),
        toolbar = lc_toolbar(
          lc_segmented("ch5_sk_var", "Zmienna",
            choices = c("Czas dojazdu" = "czas_dojazdu", "Wzrost" = "wzrost"))
        ),
        plot_id = "ch5_sk_plot",
        ratio = "2.2/1"
      )
    ),

    lc_p("Trzecia potęga zachowuje znak, więc odchylenia w prawo i w lewo
      mogą się znosić. W rozkładzie symetrycznym znoszą się całkowicie
      i skośność wynosi około zera. Potęgowanie wzmacnia duże odległości,
      więc o wyniku decydują głównie obserwacje w ogonach. Długi ogon
      w prawo daje skośność dodatnią (rozkład prawostronnie skośny), długi
      ogon w lewo — ujemną (rozkład lewostronnie skośny). Poniżej dwa
      rozkłady skośne, będące swoimi lustrzanymi odbiciami, i między nimi
      rozkład symetryczny."),

    figure_panel(
      label = "Ryc. 5.1",
      title = "Porównanie trzech typów skośności",
      lc_plot("ch5_skew_comparison", max_height = "300px"),
      uiOutput("ch5_skew_comparison_caption")
    ),

    lc_p("Rozkład prawostronnie skośny ma skośność około 0.99, jego lustrzane
      odbicie około -0.99, a rozkład symetryczny wartość bliską zera. Znak
      mówi, po której stronie leży dłuższy ogon, a wartość bezwzględna,
      jak silna jest asymetria. Jako orientacyjną skalę przyjmuje się
      często: poniżej 0.5 rozkład jest w przybliżeniu symetryczny, od 0.5
      do 1 umiarkowanie skośny, powyżej 1 silnie skośny."),

    lc_p("Skośność idzie w parze z relacją średniej i mediany. Średnia
      przesuwa się w stronę ogona, a mediana zostaje bliżej szczytu, dlatego
      przy skośności dodatniej zwykle średnia jest większa od mediany,
      a przy ujemnej mniejsza. Sprawdźmy to na danych z ankiety."),

    figure_panel(
      label = "Ryc. 5.2",
      title = "Sprawdź skośność w naszych danych",
      selectInput("ch5_skew_var", "Wybierz zmienną:",
        choices = c(
          "Wzrost" = "wzrost",
          "Czas dojazdu" = "czas_dojazdu",
          "Średnia ocen" = "srednia_ocen",
          "Liczba nieobecności" = "liczba_nieobecnosci"
        ),
        selected = "czas_dojazdu"
      ),
      lc_plot("ch5_skew_plot", ratio = "1.8/1", max_height = "350px"),
      uiOutput("ch5_skew_info")
    ),

    lc_p("Czas dojazdu ma skośność 1.02, czyli silną asymetrię prawostronną,
      i średnia leży na prawo od mediany. Liczba nieobecności (0.69) jest
      skośna umiarkowanie: nikt nie może mieć mniej niż zero nieobecności,
      ale kilka osób ma ich wyraźnie więcej niż reszta. Wzrost (0.11)
      i średnia ocen (-0.17) są w przybliżeniu symetryczne, a ich średnie
      i mediany niemal się pokrywają."),

    lc_note("Zasada", rule = TRUE,
      "Gdy skośność jest wyraźnie różna od zera, a średnia i mediana się
       rozjeżdżają, podawaj obie albo opisuj dane medianą."
    ),

    # --- Widget 2: Kurtosis ---
    lc_h2("ch5-kurtoza", "Kurtoza (ciężkość ogonów)"),

    lc_p("Rozkład może być idealnie symetryczny, a mimo to różnić się od
      innych tym, jak często daje wartości daleko od środka. Skośność tego
      nie wychwyci, bo ogony po obu stronach znoszą się w trzeciej potędze.
      Tę cechę mierzy ", gloss("kurtoza"), ". Wzór ma tę samą budowę co
      skośność, ale z czwartą potęgą. Od wyniku odejmujemy 3, czyli wartość
      dla ", gloss("rozkład normalny", "rozkładu normalnego"), ", i otrzymujemy kurtozę nadwyżkową."),

    lc_formula_annotated(
      "⟦1|g_2⟧ = \\frac{1}{n} \\sum_{i=1}^{n} \\left( ⟦2|\\frac{x_i - \\bar{x}}{s}⟧ \\right)^{⟦3|4⟧} ⟦4|- 3⟧",
      notes = list(
        "kurtoza nadwyżkowa",
        "obserwacja standaryzowana",
        "czwarta potęga",
        "kurtoza rozkładu normalnego"
      ),
      colors = c("indygo", "niebo", "terakota", "accent"),
      above = 3
    ),

    lc_p("Czwarta potęga jest zawsze dodatnia i bardzo silnie wzmacnia duże
      odległości: obserwacja odległa o 3 odchylenia standardowe wnosi 81,
      a odległa o 1 — tylko 1. Kurtoza mówi więc przede wszystkim o ogonach,
      a nie o tym, czy szczyt jest ostry czy płaski. Względem rozkładu
      normalnego (kurtoza nadwyżkowa 0, rozkład mezokurtyczny) wyróżniamy
      rozkłady leptokurtyczne (dodatnia: ciężkie ogony, częstsze wartości
      skrajne) i platykurtyczne (ujemna: lekkie ogony, wartości skupione
      w ograniczonym zakresie)."),

    # PROTOTYP potegi: wkład obserwacji przy drugiej i czwartej potędze
    figure_panel(
      label = "Ryc. 5.2a",
      title = "Wkład obserwacji: kwadrat i czwarta potęga",
      width_mode = "text",
      tags$div(class = "proto-potegi",
        lc_plot("ch5_proto_potegi", ratio = "2.2/1", max_height = "280px")
      )
    ),

    lc_p("W panelu poniżej porównujemy z rozkładem normalnym rozkład o tej
      samej średniej (0) i tym samym odchyleniu standardowym (1), ale
      o wybranej kurtozie. Dolny wykres powiększa prawy ogon."),

    figure_panel(
      label = "Ryc. 5.3",
      title = "Porównaj rozkłady o różnej kurtozie",
      lc_toolbar(
        lc_slider("ch5_kurt_val", "Nadwyżkowa kurtoza", -1.2, 6, 0, 0.2),
        lc_action("ch5_kurt_platy", "Platykurtyczny", variant = "outline"),
        lc_action("ch5_kurt_mezo", "Mezokurtyczny", variant = "outline"),
        lc_action("ch5_kurt_lepto", "Leptokurtyczny", variant = "outline")
      ),
      lc_plot("ch5_kurt_plot", ratio = "1.8/1", max_height = "350px"),
      tags$h4("Powiększenie prawego ogona (x > 2.5)"),
      lc_plot("ch5_kurt_tails", ratio = "2.8/1", max_height = "220px"),
      uiOutput("ch5_kurt_text")
    ),

    lc_p("Choć oba rozkłady mają to samo odchylenie standardowe, przy kurtozie
      4 prawdopodobieństwo wartości większej niż 2.5 odchylenia standardowego
      wynosi około 1.1%, a w rozkładzie normalnym 0.6%. Dla wartości powyżej
      3 odchyleń różnica jest już czterokrotna. Rozkład leptokurtyczny ma
      przy tym wyższy, węższy szczyt: część obserwacji przesuwa się do
      środka, a część daleko w ogony. Rozkład platykurtyczny przy kurtozie
      -1 w ogóle nie sięga poza 2 odchylenia standardowe, więc w powiększeniu
      ogona jego krzywa leży na zerze."),

    lc_p("W praktyce dodatnia kurtoza jest sygnałem ostrzegawczym. Oznacza,
      że wartości skrajne zdarzają się częściej, niż sugerowałby rozkład
      normalny, a reguła 68–95–99.7 zaniża ich częstość. W finansach tak
      wyglądają rozkłady stóp zwrotu: duże straty, rzadkie w modelu
      normalnym, w rzeczywistości zdarzają się zaskakująco często."),

    # --- Widget 3: Full picture ---
    lc_h2("ch5-pelny-obraz", "Pełny obraz"),

    lc_p("Mamy już trzy grupy narzędzi. Statystyki położenia mówią, gdzie leży
      środek danych, statystyki rozrzutu — jak szeroko dane się rozkładają,
      a skośność i kurtoza — jaki mają kształt. Razem dają pełny opis
      rozkładu ", gloss("zmienna ilościowa", "zmiennej ilościowej"), ".
      Panel poniżej zestawia ", gloss("histogram"), ", ", gloss("wykres pudełkowy"), " i tabelę wszystkich
      omówionych statystyk dla wybranej zmiennej."),

    # PROTOTYP mapa-narzedzi: położenie / rozrzut × wrażliwe / odporne
    figure_panel(
      label = "Ryc. 5.3a",
      title = "Mapa narzędzi",
      width_mode = "text",
      ch5_tools_map()
    ),

    figure_panel(
      label = "Ryc. 5.4",
      title = "Pełna charakterystyka rozkładu",
      lc_toolbar(
        lc_segmented("ch5_full_var", "Zmienna",
          choices = c("Wzrost" = "wzrost", "Średnia ocen" = "srednia_ocen",
                      "Czas dojazdu" = "czas_dojazdu", "Waga" = "waga"))
      ),
      lc_plot("ch5_full_hist", ratio = "1.8/1", max_height = "350px"),
      lc_plot("ch5_full_box", ratio = "5.2/1", max_height = "120px"),
      uiOutput("ch5_full_table"),
      uiOutput("ch5_full_interpretation")
    ),

    lc_p("Różne miary opowiadają o tej samej zmiennej spójną historię
      i warto czytać je razem. Czas dojazdu ma dodatnią skośność (1.02),
      średnią większą od mediany, a wykres pudełkowy pokazuje 7 wartości
      odstających, wszystkie po prawej stronie. Dodatnia kurtoza (0.75)
      potwierdza, że długie dojazdy zdarzają się częściej niż w rozkładzie
      normalnym."),

    lc_p("Waga jest prawie symetryczna (skośność 0.30), ale ma wyraźnie ujemną
      kurtozę (-0.72): rozkład jest płaski i szeroki, bez wyraźnych ogonów.
      To skutek zjawiska z rozdziału 3, omawianego przy modalności. W danych
      są pomieszane dwie grupy, kobiety i mężczyźni, o różnych średnich wagach.
      Każda grupa osobno ma rozkład zbliżony do normalnego, ale razem
      wypełniają szeroki przedział, a środek rozkładu się spłaszcza. Ujemna
      kurtoza bywa więc wskazówką, żeby obejrzeć histogram i sprawdzić,
      czy w danych nie kryje się kilka grup."),

    lc_chapter_next(
      num       = "06",
      title     = "Quiz",
      lead      = "sprawdź się — rozpoznawanie typów zmiennych w praktyce.",
      target_id = "ch-quiz"
    ),

    br(), br()
  )
)

# --------------------------------------------------------------------------
# Chapter 5 Server
# --------------------------------------------------------------------------

ch5_server <- function(input, output, session) {

  # --------------------------------------------------------------------------
  # Widget 1: Skewness
  # --------------------------------------------------------------------------

  ch5_skew_data <- reactive({
    req(input$ch5_skew_var)
    vals <- student_data[[input$ch5_skew_var]]
    vals <- vals[!is.na(vals)]
    list(
      values = vals,
      label = variable_meta[[input$ch5_skew_var]]$label,
      var_name = input$ch5_skew_var
    )
  })

  ch5_skew_cmp <- reactive({
    set.seed(42)
    n_pts <- 5000
    left_skew  <- -rgamma(n_pts, shape = 4, scale = 1)
    symmetric  <- rnorm(n_pts, mean = 0, sd = 2)
    right_skew <- rgamma(n_pts, shape = 4, scale = 1)

    types <- c("Lewostronnie skośny", "Symetryczny", "Prawostronnie skośny")
    df <- rbind(
      data.frame(x = left_skew,  typ = types[1]),
      data.frame(x = symmetric,  typ = types[2]),
      data.frame(x = right_skew, typ = types[3])
    )
    df$typ <- factor(df$typ, levels = types)
    skew <- setNames(
      c(e1071::skewness(left_skew), e1071::skewness(symmetric), e1071::skewness(right_skew)),
      types)
    list(df = df, skew = skew)
  })

  output$ch5_skew_comparison_caption <- renderUI({
    sk <- ch5_skew_cmp()$skew
    lc_caption(paste0("Skośność: ",
      paste(sprintf("%s %s", tolower(names(sk)), lc_fmt(sk, 2)), collapse = ", "), "."))
  })

  zoom_plot_server("ch5_skew_comparison", reactive({
    df_cmp <- ch5_skew_cmp()$df

    ggplot(df_cmp, aes(x = x)) +
      geom_density(aes(fill = typ), alpha = 0.5, color = upwr_secondary, linewidth = 0.8) +
      facet_wrap(~typ, scales = "free_x") +
      scale_fill_manual(values = c(
        "Lewostronnie skośny" = upwr_accent,
        "Symetryczny" = upwr_col("szalwia"),
        "Prawostronnie skośny" = upwr_col("niebo")
      )) +
      labs(x = "Wartość", y = "Gęstość") +
      theme(legend.position = "none",
            strip.text = element_text(face = "bold", size = 12))
  }))

  zoom_plot_server("ch5_skew_plot", reactive({
    d <- ch5_skew_data()
    vals <- d$values
    m <- mean(vals)
    med <- median(vals)
    sk <- e1071::skewness(vals)

    df <- data.frame(x = vals)

    ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)),
        bins = 20, fill = upwr_cat["niebo"], color = "white", alpha = 0.6) +
      geom_density(color = upwr_secondary, linewidth = 1) +
      geom_vline(aes(xintercept = m, color = "Średnia"), linewidth = 1.2, linetype = "solid") +
      geom_vline(aes(xintercept = med, color = "Mediana"), linewidth = 1.2, linetype = "dashed") +
      scale_color_manual(name = NULL,
        breaks = c("Średnia", "Mediana"),
        values = stat_colors[c("Średnia", "Mediana")]) +
      labs(x = d$label, y = "Gęstość") +
      theme(legend.position = "top")
  }))

  output$ch5_skew_info <- renderUI({
    d <- ch5_skew_data()
    vals <- d$values
    sk <- e1071::skewness(vals)
    m <- mean(vals)
    med <- median(vals)

    p(paste0("Skośność = ", round(sk, 3)),
      " | Średnia = ", round(m, 2),
      ", Mediana = ", round(med, 2))
  })

  # PROTOTYP skosnosc-kroki: serwer widgetu krokowego
  ch5_sk_step <- lc_step_server("ch5_sk", input)$step

  ch5_sk_data <- reactive({
    var <- input$ch5_sk_var %||% "czas_dojazdu"
    x <- student_data[[var]]
    m <- mean(x); s <- sd(x)
    z <- (x - m) / s
    set.seed(7)
    list(x = x, z = z, m = m, s = s, z3 = z^3, n = length(x),
         jit = runif(length(x), -0.35, 0.35),
         label = variable_meta[[var]]$label)
  })

  zoom_plot_server("ch5_sk_plot", reactive({
    step <- ch5_sk_step()
    d <- ch5_sk_data()
    col_l <- upwr_col("niebo")
    col_r <- upwr_col("terakota")
    side <- ifelse(d$z < 0, "l", "r")
    side_cols <- c(l = col_l, r = col_r)

    if (step <= 2) {
      xv <- if (step == 1) d$x else d$z
      ctr <- if (step == 1) d$m else 0
      df <- data.frame(x = xv, y = d$jit, side = side)
      p <- ggplot(df, aes(x = x, y = y))
      if (step == 2) {
        p <- p + geom_vline(xintercept = -3:3, colour = upwr_reference,
                            linewidth = 0.3, linetype = "22")
      }
      p +
        geom_point(aes(colour = if (step == 2) side else "d"),
                   size = 2.4, alpha = 0.7) +
        scale_colour_manual(values = c(side_cols, d = STEP_ROLES$data$colour)) +
        step_line(if (step == 1) "new" else "known", xintercept = ctr, helper = FALSE) +
        step_label(ctr + diff(range(xv)) * 0.01, 0.5,
                   if (step == 1) sprintf("x̄ = %.1f", d$m) else "x̄",
                   role = if (step == 1) "new" else "known", hjust = 0, vjust = 0) +
        scale_x_continuous(breaks = if (step == 2) -3:4 else waiver()) +
        labs(x = if (step == 1) d$label else "Odległość od średniej (w SD)", y = NULL) +
        step_frame(xlim = range(xv) + c(-1, 1) * diff(range(xv)) * 0.05,
                   ylim = c(-0.5, 0.62), y_axis = FALSE)

    } else if (step == 3) {
      df <- data.frame(z = d$z, z3 = d$z3, side = side)
      ggplot(df, aes(x = z, y = z3, colour = side)) +
        geom_hline(yintercept = 0, colour = upwr_secondary, linewidth = 0.5) +
        geom_segment(aes(xend = z, y = 0, yend = z3), linewidth = 0.6, alpha = 0.7) +
        geom_point(size = 1.8) +
        scale_colour_manual(values = side_cols) +
        labs(x = "Odległość od średniej (w SD)", y = "z³") +
        theme(legend.position = "none")

    } else {
      sums <- c(l = sum(d$z3[d$z3 < 0]), r = sum(d$z3[d$z3 > 0]))
      tot <- sum(sums)
      df <- data.frame(
        what = factor(c("Lewa strona", "Prawa strona", "Suma"),
                      levels = c("Suma", "Prawa strona", "Lewa strona")),
        val = c(sums[["l"]], sums[["r"]], tot),
        fill = c(col_l, col_r, upwr_col("indygo"))
      )
      lim <- max(abs(df$val)) * 1.25
      ggplot(df, aes(x = val, y = what)) +
        geom_vline(xintercept = 0, colour = upwr_secondary, linewidth = 0.5) +
        geom_col(aes(fill = I(fill)), width = 0.55, alpha = 0.9) +
        geom_text(aes(label = formatC(val, format = "f", digits = 1), hjust = ifelse(val < 0, 1.15, -0.15)),
                  family = lc_mono_family, colour = upwr_secondary, size = 4) +
        scale_x_continuous(limits = c(-lim, lim)) +
        labs(x = "Suma z³", y = NULL) +
        theme(panel.grid.major.y = element_blank())
    }
  }))

  output$ch5_sk_text <- renderUI({
    step <- ch5_sk_step()
    d <- ch5_sk_data()
    switch(as.character(step),
      "1" = paste0(d$n, " obserwacji i ich średnia."),
      "2" = paste0("Ta sama oś w jednostkach SD: średnia to 0, odchylenie standardowe to 1."),
      "3" = "Trzecia potęga zachowuje znak i wydłuża dalekie obserwacje.",
      "4" = {
        tot <- sum(d$z3)
        paste0("Suma ", lc_fmt(tot, 1), " podzielona przez ", d$n,
               " daje skośność ", lc_fmt(tot / d$n, 2), ".")
      })
  })

  # PROTOTYP potegi: słupki z² i z⁴ dla odległości 1, 2, 3 SD
  zoom_plot_server("ch5_proto_potegi", reactive({
    z <- 1:3
    df <- data.frame(
      z = rep(z, 2),
      pot = factor(rep(c("z²", "z⁴"), each = 3), levels = c("z²", "z⁴")),
      val = c(z^2, z^4)
    )
    ggplot(df, aes(x = factor(z), y = val, fill = pot)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.62) +
      geom_text(aes(label = val), position = position_dodge(width = 0.7),
                vjust = -0.4, family = lc_mono_family, colour = upwr_secondary,
                size = 3.8) +
      scale_fill_manual(values = c("z²" = upwr_reference, "z⁴" = upwr_col("terakota")),
                        name = NULL) +
      scale_x_discrete(labels = paste(z, "SD")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Odległość od średniej", y = "Wkład do sumy") +
      theme(legend.position = "top", legend.text = element_text(size = 13),
            panel.grid.major.x = element_blank())
  }))

  # --------------------------------------------------------------------------
  # Widget 2: Kurtosis (suwak kurtozy, bez t-rozkładu)
  # --------------------------------------------------------------------------

  observeEvent(input$ch5_kurt_platy, { updateSliderInput(session, "ch5_kurt_val", value = -1.0) })
  observeEvent(input$ch5_kurt_mezo,  { updateSliderInput(session, "ch5_kurt_val", value = 0) })
  observeEvent(input$ch5_kurt_lepto, { updateSliderInput(session, "ch5_kurt_val", value = 4) })

  # Generate density with target excess kurtosis
  # Leptokurtic: t-distribution scaled to sd=1 (higher peak, heavier tails)
  # Platykurtic: beta(a,a) scaled to sd=1 (flatter peak, no tails)
  ch5_kurt_density <- reactive({
    ek <- input$ch5_kurt_val
    req(!is.null(ek))
    x_seq <- seq(-5, 5, length.out = 500)

    if (ek < -0.01) {
      # Platykurtyczny: beta(a,a) scaled to sd=1
      # excess_kurt of beta(a,a) = -6/(2a+3)
      # So a = -(6/ek + 3) / 2, but ek is negative
      # ek = -6/(2a+3) -> 2a+3 = -6/ek -> a = (-6/ek - 3)/2
      a <- max(1.01, (-6 / ek - 3) / 2)
      scale_b <- sqrt(2 * a + 1)
      # beta(a,a) on [0,1] mapped to [-scale_b, scale_b] for sd=1
      dens <- dbeta((x_seq / scale_b + 1) / 2, a, a) / (2 * scale_b)
      # Zero out beyond support
      dens[abs(x_seq) > scale_b] <- 0
    } else if (ek <= 0.01) {
      dens <- dnorm(x_seq)
    } else {
      # Leptokurtyczny: t-distribution scaled to sd=1
      # excess_kurt = 6 / (df - 4) -> df = 6/ek + 4
      df_mapped <- max(4.5, 6 / ek + 4)
      sd_t <- sqrt(df_mapped / (df_mapped - 2))
      dens <- dt(x_seq * sd_t, df = df_mapped) * sd_t
    }
    data.frame(x = x_seq, dens = dens, norm = dnorm(x_seq))
  })

  zoom_plot_server("ch5_kurt_plot", reactive({
    df <- ch5_kurt_density()
    ek <- input$ch5_kurt_val

    type_name <- if (ek < -0.1) "Platykurtyczny" else if (ek > 0.1) "Leptokurtyczny" else "Mezokurtyczny"
    type_color <- if (ek < -0.1) upwr_cat["bursztyn"] else if (ek > 0.1) upwr_accent else upwr_cat["szalwia"]

    ggplot(df, aes(x = x)) +
      geom_line(aes(y = norm, linetype = "Rozkład normalny"),
                color = upwr_reference, linewidth = 1) +
      geom_area(aes(y = dens), fill = type_color, alpha = 0.35) +
      geom_line(aes(y = dens, linetype = type_name), color = type_color, linewidth = 1.2) +
      scale_linetype_manual(name = NULL,
        values = setNames(c("dashed", "solid"), c("Rozkład normalny", type_name))) +
      labs(x = "x", y = "Gęstość") +
      theme(legend.position = "top")
  }))

  zoom_plot_server("ch5_kurt_tails", reactive({
    df <- ch5_kurt_density()
    ek <- input$ch5_kurt_val
    type_color <- if (ek < -0.1) upwr_cat["bursztyn"] else if (ek > 0.1) upwr_accent else upwr_cat["szalwia"]

    tail_df <- df[df$x >= 2.5, ]

    ggplot(tail_df, aes(x = x)) +
      geom_area(aes(y = norm), fill = upwr_reference, alpha = 0.15) +
      geom_line(aes(y = norm), color = upwr_reference, linewidth = 1, linetype = "dashed") +
      geom_area(aes(y = dens), fill = type_color, alpha = 0.3) +
      geom_line(aes(y = dens), color = type_color, linewidth = 1.2) +
      labs(x = "x", y = "Gęstość") +
      theme()
  }))

  output$ch5_kurt_text <- renderUI({
    ek <- input$ch5_kurt_val
    req(!is.null(ek))

    if (ek < -0.5) {
      type_class <- "warning"
      type_name <- "Platykurtyczny"
      desc <- "Ogony lżejsze niż w rozkładzie normalnym — wartości skrajne
               są rzadsze."
    } else if (ek < 0.5) {
      type_class <- "info"
      type_name <- "Mezokurtyczny"
      desc <- "Ogony zbliżone do rozkładu normalnego."
    } else {
      type_class <- "danger"
      type_name <- "Leptokurtyczny"
      desc <- "Ogony cięższe niż w rozkładzie normalnym — wartości skrajne
               są częstsze."
    }

    lc_status(
      lc_verdict(tags$strong(paste0(type_name, " (nadwyżkowa kurtoza = ",
                         format(round(ek, 1)), "):")), type = type_class),
      " ",
      desc
    )
  })

  # --------------------------------------------------------------------------
  # Widget 3: Full picture (Capstone)
  # --------------------------------------------------------------------------

  ch5_full_data <- reactive({
    req(input$ch5_full_var)
    vals <- student_data[[input$ch5_full_var]]
    vals <- vals[!is.na(vals)]
    list(
      values = vals,
      label = variable_meta[[input$ch5_full_var]]$label,
      var_name = input$ch5_full_var
    )
  })

  zoom_plot_server("ch5_full_hist", reactive({
    d <- ch5_full_data()
    vals <- d$values
    m <- mean(vals)
    med <- median(vals)
    df <- data.frame(x = vals)

    ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)),
        bins = 20, fill = upwr_cat["niebo"], color = "white", alpha = 0.5) +
      geom_density(color = upwr_secondary, linewidth = 1) +
      geom_rug(color = upwr_secondary, alpha = 0.5) +
      geom_vline(aes(xintercept = m, color = "Średnia"), linewidth = 1.1) +
      geom_vline(aes(xintercept = med, color = "Mediana"), linewidth = 1.1, linetype = "dashed") +
      scale_color_manual(name = NULL,
        breaks = c("Średnia", "Mediana"),
        values = stat_colors[c("Średnia", "Mediana")]) +
      labs(x = d$label, y = "Gęstość") +
      theme(legend.position = "top")
  }))

  zoom_plot_server("ch5_full_box", reactive({
    d <- ch5_full_data()
    df <- data.frame(x = d$values)

    ggplot(df, aes(x = x)) +
      geom_boxplot(fill = upwr_cat["niebo"], alpha = 0.4, color = upwr_secondary,
        outlier.color = upwr_accent, outlier.size = 3) +
      labs(x = d$label) +
            theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  }))

  output$ch5_full_table <- renderUI({
    d <- ch5_full_data()
    vals <- d$values

    n <- length(vals)
    m <- mean(vals)
    med <- median(vals)
    s <- sd(vals)
    v <- var(vals)
    rng <- diff(range(vals))
    q1 <- quantile(vals, 0.25, names = FALSE)
    q3 <- quantile(vals, 0.75, names = FALSE)
    iqr_val <- IQR(vals)
    cv <- (s / m) * 100
    sk <- e1071::skewness(vals)
    ku <- e1071::kurtosis(vals)
    trimmed <- mean(vals, trim = 0.1)

    # Mode: bin with highest frequency
    h <- hist(vals, breaks = 20, plot = FALSE)
    mode_bin_idx <- which.max(h$counts)
    mode_val <- (h$breaks[mode_bin_idx] + h$breaks[mode_bin_idx + 1]) / 2

    stats_df <- data.frame(
      Statystyka = c(
        "n", "Średnia", "Mediana", "Dominanta (środek przedziałowy)",
        "Śr. ucięta 10%",
        "Odch. std.", "Wariancja", "Rozstęp", "IQR", "CV (%)",
        "Minimum", "Q1", "Q3", "Maksimum",
        "Skośność", "Kurtoza (nadwyżkowa)"
      ),
      Wartość = c(
        as.character(n),
        formatC(m, format = "f", digits = 2),
        formatC(med, format = "f", digits = 2),
        formatC(mode_val, format = "f", digits = 2),
        formatC(trimmed, format = "f", digits = 2),
        formatC(s, format = "f", digits = 2),
        formatC(v, format = "f", digits = 2),
        formatC(rng, format = "f", digits = 2),
        formatC(iqr_val, format = "f", digits = 2),
        formatC(cv, format = "f", digits = 1),
        formatC(min(vals), format = "f", digits = 2),
        formatC(q1, format = "f", digits = 2),
        formatC(q3, format = "f", digits = 2),
        formatC(max(vals), format = "f", digits = 2),
        formatC(sk, format = "f", digits = 3),
        formatC(ku, format = "f", digits = 3)
      ),
      stringsAsFactors = FALSE
        )
        lc_table(stats_df,
          cols = list(lc_col("Statystyka", "Statystyka", "row"), lc_col("Wartość", "Wartość")),
          label = "Pełna charakterystyka rozkładu")
      })

  output$ch5_full_interpretation <- renderUI({
    d <- ch5_full_data()
    vals <- d$values

    m <- mean(vals)
    med <- median(vals)
    s <- sd(vals)
    q1 <- quantile(vals, 0.25, names = FALSE)
    q3 <- quantile(vals, 0.75, names = FALSE)
    iqr_val <- IQR(vals)
    sk <- e1071::skewness(vals)

    # Outliers
    lower_fence <- q1 - 1.5 * iqr_val
    upper_fence <- q3 + 1.5 * iqr_val
    outliers_low <- vals[vals < lower_fence]
    outliers_high <- vals[vals > upper_fence]
    n_outliers <- length(outliers_low) + length(outliers_high)

    if (n_outliers > 0) {
      outlier_text <- paste0(
        "Wykryto ", n_outliers, " wartości odstających ",
        "(poza przedziałem [", round(lower_fence, 2), ", ",
        round(upper_fence, 2), "])."
      )
    } else {
      outlier_text <- "Brak wartości odstających (wg kryterium 1.5 · IQR)."
    }

    lc_status(
      p(tags$strong("Podsumowanie:")),
      tags$ul(
        tags$li(paste0("Średnia = ", round(m, 2), ", Mediana = ", round(med, 2))),
        tags$li(paste0("Środkowe 50% obserwacji leży między ",
          round(q1, 2), " a ", round(q3, 2), " (Q1–Q3).")),
        tags$li(paste0("Skośność = ", round(sk, 3), ", Kurtoza = ", round(e1071::kurtosis(vals), 3))),
        tags$li(outlier_text)
      )
    )
  })
}
