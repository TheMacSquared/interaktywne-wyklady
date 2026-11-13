# ============================================================================
# CHAPTER 5: Kształt rozkładu
# ============================================================================

# Siatka statystyk (HTML/CSS, style w app.R)
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

# Ryc. 5.2, 5.3, 5.4 i histogram Ryc. 5.7 rysuje stat_charts.js; opisy dla
# danych ankiety liczymy tu z góry (dla każdej zmiennej).
ch5_skew_choices <- c("Wzrost" = "wzrost", "Czas dojazdu" = "czas_dojazdu",
                      "Średnia ocen" = "srednia_ocen", "Liczba nieobecności" = "liczba_nieobecnosci")

ch5_skew_notes <- function() {
  lapply(stats::setNames(nm = unname(ch5_skew_choices)), function(v) {
    vals <- student_data[[v]]; vals <- vals[!is.na(vals)]
    p(paste0("Skośność = ", round(e1071::skewness(vals), 3)),
      " | Średnia = ", round(mean(vals), 2), ", Mediana = ", round(median(vals), 2))
  })
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

    # Krokowa budowa skośności (standaryzacja → z³ → bilans)
    lc_p("Panel przeprowadza to obliczenie na danych z ankiety. Najpierw
      zamieniamy każdą obserwację na odległość od średniej w odchyleniach
      standardowych, potem podnosimy te odległości do trzeciej potęgi,
      a na końcu sumujemy osobno wkłady z lewej i z prawej strony."),

    figure_panel(
      label = "Ryc. 5.1",
      lc_sf_figure("sk", bare = TRUE, body = lc_step_widget("ch5_sk",
        title = "Obliczanie skośności",
        steps = c("Dane", "Standaryzacja", "Trzecie potęgi", "Bilans"),
        toolbar = lc_toolbar(
          lc_sf_ctl(lc_segmented("ch5_sk_var", "Zmienna",
            choices = c("Czas dojazdu" = "czas_dojazdu", "Wzrost" = "wzrost"))),
          lc_chart_zoom_button()
        ),
        body = lc_sf_chart(0, ratio = "2.2/1", max_height = "400px")
      ))
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
      label = "Ryc. 5.2",
      title = "Porównanie trzech typów skośności",
      lc_sf_figure("skewcmp", charts = 3, ratio = "1.1/1", max_height = "300px",
        headings = list("Lewostronnie skośny", "Symetryczny", "Prawostronnie skośny"),
        after = htmltools::tagAppendAttributes(lc_caption("Skośność: …"), `data-role` = "caption"))
    ),

    lc_p("Rozkład prawostronnie skośny ma skośność około 0.95, jego lustrzane
      odbicie około -0.95, a rozkład symetryczny wartość bliską zera. Znak
      mówi, po której stronie leży dłuższy ogon, a wartość bezwzględna,
      jak silna jest asymetria. Jako orientacyjną skalę przyjmuje się
      często: poniżej 0.5 rozkład jest w przybliżeniu symetryczny, od 0.5
      do 1 umiarkowanie skośny, powyżej 1 silnie skośny."),

    lc_p("Skośność idzie w parze z relacją średniej i mediany. Średnia
      przesuwa się w stronę ogona, a mediana zostaje bliżej szczytu, dlatego
      przy skośności dodatniej zwykle średnia jest większa od mediany,
      a przy ujemnej mniejsza. Sprawdźmy to na danych z ankiety."),

    figure_panel(
      label = "Ryc. 5.3",
      title = "Sprawdź skośność w naszych danych",
      lc_sf_figure("skew", ratio = "1.8/1", max_height = "350px", legend = TRUE,
        control = lc_group("Zmienna", lc_select("ch5_skew_var", ch5_skew_choices,
                                                selected = "czas_dojazdu", aria_label = "Zmienna")),
        selected = "czas_dojazdu", notes = ch5_skew_notes())
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

    # Wkład obserwacji przy drugiej i czwartej potędze
    figure_panel(
      label = "Ryc. 5.4",
      title = "Wkład obserwacji: kwadrat i czwarta potęga",
      width_mode = "text",
      lc_sf_figure("powers", ratio = "2.2/1", max_height = "300px", legend = TRUE)
    ),

    lc_p("W panelu poniżej porównujemy z rozkładem normalnym rozkład o tej
      samej średniej (0) i tym samym odchyleniu standardowym (1), ale
      o wybranej kurtozie. Dolny wykres powiększa prawy ogon."),

    figure_panel(
      label = "Ryc. 5.5",
      title = "Porównaj rozkłady o różnej kurtozie",
      tags$div(class = "lc-kurt", `data-ek` = "0",
        lc_toolbar(
          lc_group("Przykłady", tags$div(class = "lc-seg", role = "group", `aria-label` = "Przykłady",
            tags$button(type = "button", `data-k-set` = "-1", `aria-pressed` = "false", "Platykurtyczny"),
            tags$button(type = "button", `data-k-set` = "0", `aria-pressed` = "true", "Mezokurtyczny"),
            tags$button(type = "button", `data-k-set` = "4", `aria-pressed` = "false", "Leptokurtyczny"))),
          lc_readouts(
            lc_readout(tags$span(`data-role` = "type", "Mezokurtyczny"),
                       tags$span(`data-role` = "ek", "0.0"),
                       color = "var(--upwr-cat-szalwia)", swatch = TRUE),
            lc_readout(tagList(tags$i(class = "lc-sf-line-sw is-dashed", `aria-hidden` = "true",
                                       style = "--lc-sw:var(--upwr-reference);"), "Rozkład normalny"), "0.0",
                       color = "var(--upwr-reference)")
          ),
          lc_chart_zoom_button()
        ),
        tags$div(class = "lc-kurt-main",
          tags$div(class = "lc-ec", role = "img", `data-role` = "main",
            `aria-label` = "Gęstość rozkładu o wybranej kurtozie na tle rozkładu normalnego")),
        lc_caption("Przeciągnij szczyt lub wybierz go Tabem i użyj strzałek góra/dół, aby zmienić kurtozę nadwyżkową (od -1.2 do 6)."),
        tags$h4("Powiększenie prawego ogona (x > 2.5)"),
        tags$div(class = "lc-kurt-tail",
          tags$div(class = "lc-ec", role = "img", `data-role` = "tail",
            `aria-label` = "Powiększenie prawego ogona obu gęstości dla x od 2.5 do 5")),
        lc_status(tags$p(`data-role` = "text", ""))
      )
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

    # Położenie / rozrzut × wrażliwe / odporne
    figure_panel(
      label = "Ryc. 5.6",
      title = "Mapa narzędzi",
      width_mode = "text",
      ch5_tools_map()
    ),

    figure_panel(
      label = "Ryc. 5.7",
      title = "Pełna charakterystyka rozkładu",
      # Histogram i pudełko rysuje przeglądarka; tabele zostają w R (te same wejścia).
      lc_sf_figure("full", legend = TRUE,
        control = lc_segmented("ch5_full_var", "Zmienna",
          choices = c("Wzrost" = "wzrost", "Średnia ocen" = "srednia_ocen",
                      "Czas dojazdu" = "czas_dojazdu", "Waga" = "waga")),
        # Zestaw minimum albo pełny (dwie tabele obok siebie)
        toolbar = lc_segmented("ch5_full_set", "Zestaw",
          choices = c("Minimum" = "min", "Pełny" = "full"), selected = "min"),
        body = tagList(lc_sf_chart(0, ratio = "1.8/1", max_height = "350px"),
                       lc_sf_chart(1, ratio = "5.2/1", max_height = "130px"))),
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

  # Ryc. 5.2 i 5.3 rysuje stat_charts.js; opisy w UI (ch5_skew_notes).

  # Ryc. 5.1 (skośność krok po kroku) rysuje stat_charts.js.

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

    # Minimum (n, średnia, SD, mediana, kwartyle) albo
    # pełny zestaw w dwóch tabelach obok siebie.
    f2 <- function(x) formatC(x, format = "f", digits = 2)
    stat_table <- function(names, values, label) {
      lc_table(data.frame(Statystyka = names, Wartość = values, stringsAsFactors = FALSE),
        cols = list(lc_col("Statystyka", "Statystyka", "row"), lc_col("Wartość", "Wartość")),
        label = label)
    }

    if (!identical(input$ch5_full_set, "full")) {
      return(stat_table(
        c("n", "Średnia", "Odch. std.", "Mediana", "Q1", "Q3"),
        c(as.character(n), f2(m), f2(s), f2(med), f2(q1), f2(q3)),
        "Charakterystyka rozkładu — zestaw minimum"))
    }

    tags$div(class = "ch5-full-pair",
      tags$div(
        tags$h4("Położenie"),
        stat_table(
          c("n", "Średnia", "Mediana", "Dominanta", "Śr. ucięta 10%",
            "Minimum", "Q1", "Q3", "Maksimum"),
          c(as.character(n), f2(m), f2(med), f2(mode_val), f2(trimmed),
            f2(min(vals)), f2(q1), f2(q3), f2(max(vals))),
          "Statystyki położenia")
      ),
      tags$div(
        tags$h4("Rozrzut i kształt"),
        stat_table(
          c("Odch. std.", "Wariancja", "Rozstęp", "IQR", "CV (%)",
            "Skośność", "Kurtoza"),
          c(f2(s), f2(v), f2(rng), f2(iqr_val), formatC(cv, format = "f", digits = 1),
            formatC(sk, format = "f", digits = 3), formatC(ku, format = "f", digits = 3)),
          "Statystyki rozrzutu i kształtu")
      )
    )
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
