# ============================================================================
# CHAPTER 3: Statystyki położenia
# ============================================================================

ch3_ui <- list(
  id = "ch-polozenie", num = "03", title = "Statystyki położenia",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 03 · Statystyka opisowa",
      num    = "03",
      title  = "Statystyki położenia.",
      lead   = "Dwustu wyników nikt nie przeczyta jeden po drugim. Potrzebujemy jednej
                liczby, która powie, gdzie leży typowa wartość. Średnia, mediana
                i percentyle robią to na różne sposoby i każda z nich ma słabe miejsca."
    ),

    uiOutput("tracker_ch3"),

    lc_p("W poprzednim rozdziale opisywaliśmy zmienne jakościowe: liczyliśmy
      kategorie i szukaliśmy najczęstszej z nich. ",
      gloss("zmienna ilościowa", "Zmienne ilościowe"), ", takie jak wzrost czy
      czas dojazdu, wymagają innych narzędzi, bo ich wartości są liczbami,
      a nie etykietami. Zaczniemy od tego, żeby takie dane zobaczyć, a potem
      streścimy je jedną liczbą."),

    # ========================================================================
    # WIDGET: Histogram krok po kroku
    # ========================================================================
    lc_h2("ch3-histogram", "Histogram — krok po kroku"),

    lc_p(gloss("histogram", "Histogram"), " to podstawowy wykres dla ",
      gloss("zmienna ciągła", "zmiennych ciągłych"), ". Zakres wartości dzielimy
      na przedziały równej szerokości (biny), a wysokość słupka nad przedziałem
      pokazuje, ile obserwacji do niego wpadło. Poniżej budujemy histogram
      krok po kroku na danych z ankiety 200 studentów."),

    figure_panel(
      label = "Ryc. 3.1",
      lc_step_widget("ch3_hist",
        title = "Budowa histogramu",
        steps = c("Surowe dane", "Posortuj dane", "Podziel na przedziały",
                  "Przypisz do binów", "Zlicz obserwacje", "Zbuduj słupki",
                  "Wpływ szerokości binu"),
        toolbar = lc_toolbar(
          selectInput("ch3_hist_var", "Zmienna",
            choices = c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga",
                        "Czas dojazdu (min)" = "czas_dojazdu",
                        "Średnia ocen" = "srednia_ocen"),
            selected = "wzrost"
          ),
          lc_step_from(3, uiOutput("ch3_hist_bin_slider"))
        ),
        plot_id = "ch3_hist_plot",
        extra = uiOutput("ch3_hist_table")
      )
    ),

    lc_p("Ostatni krok pokazuje rzecz, o której łatwo zapomnieć: kształt
      histogramu zależy od szerokości binu. Zbyt wąskie przedziały dają
      poszarpany wykres, w którym przypadkowe wahania przesłaniają kształt
      rozkładu. Zbyt szerokie zlewają dane w kilka słupków i ukrywają
      szczegóły. Domyślne ustawienie programu jest tylko punktem wyjścia."),

    lc_p("Histogram pokazuje cały rozkład, ale do porównań i raportów potrzebujemy
      czegoś krótszego: liczby, która mówi, gdzie leży środek danych. Takie
      liczby nazywamy statystykami położenia."),

    # ========================================================================
    # WIDGET 0a: Mean introduction
    # ========================================================================
    lc_h2("ch3-srednia", "Średnia arytmetyczna"),

    lc_p("Najbardziej znaną statystyką położenia jest ",
      gloss("średnia", "średnia arytmetyczna"), ": sumujemy wszystkie wartości
      i dzielimy przez ich liczbę."),

    lc_formula_box(withMathJax(
      "$$\\bar{x} = \\frac{1}{n} \\sum_{i=1}^{n} x_i = \\frac{x_1 + x_2 + \\ldots + x_n}{n}$$"
    )),

    lc_p("Średnią można rozumieć jako punkt równowagi. Gdyby każdą obserwację
      położyć jako jednakowy ciężarek na linijce, linijka balansowałaby
      dokładnie w punkcie średniej."),

    figure_panel(
      label = "Ryc. 3.2",
      title = "Średnia jako punkt równowagi",
      selectInput("ch3_mean_var", "Zmienna:",
        choices = c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga",
                    "Średnia ocen" = "srednia_ocen"),
        selected = "wzrost"
      ),
      lc_plot("ch3_mean_plot", max_height = "300px"),
      uiOutput("ch3_mean_text")
    ),

    lc_p("Średni wzrost w naszej ankiecie wynosi 171.1 cm. Do sumy trafia każda
      wartość, więc każda ciągnie średnią w swoją stronę, także wartości skrajne.
      Obserwacja odległa od reszty przesuwa punkt równowagi bardziej niż kilka
      obserwacji leżących blisko środka. To zaleta, gdy chcemy uwzględnić
      wszystkie dane, i słabość, gdy w danych trafiają się wartości nietypowe.
      Dlatego potrzebujemy drugiej miary, która na skrajności nie reaguje."),

    # ========================================================================
    # WIDGET 0b: Median introduction
    # ========================================================================
    lc_h2("ch3-mediana", "Mediana"),

    lc_p(gloss("mediana", "Mediana"), " to wartość środkowa: po posortowaniu danych
      połowa obserwacji leży poniżej niej, a połowa powyżej. Przy nieparzystej
      liczbie obserwacji jest to środkowy element, przy parzystej — średnia
      z dwóch środkowych. Zapis \\(x_{(i)}\\) oznacza i-tą wartość po posortowaniu."),

    lc_formula_box(withMathJax(
      "$$\\text{Me} = \\begin{cases} x_{((n+1)/2)} & n \\text{ nieparzyste} \\\\[4pt] \\dfrac{x_{(n/2)} + x_{(n/2+1)}}{2} & n \\text{ parzyste} \\end{cases}$$"
    )),

    lc_p("Mediana zależy tylko od kolejności obserwacji. Nie ma znaczenia, jak
      daleko od środka leżą wartości skrajne, liczy się tylko to, po której
      stronie się znajdują."),

    figure_panel(
      label = "Ryc. 3.3",
      title = "Mediana dzieli dane na pół",
      selectInput("ch3_median_var", "Zmienna:",
        choices = c("Wzrost (cm)" = "wzrost", "Czas dojazdu (min)" = "czas_dojazdu",
                    "Średnia ocen" = "srednia_ocen"),
        selected = "czas_dojazdu"
      ),
      lc_plot("ch3_median_plot", max_height = "300px"),
      uiOutput("ch3_median_text")
    ),

    lc_p("Porównaj obie miary dla dwóch zmiennych. Dla wzrostu mediana (170.7 cm)
      i średnia (171.1 cm) prawie się pokrywają, bo rozkład jest w przybliżeniu
      symetryczny. Dla czasu dojazdu różnica jest wyraźniejsza: mediana wynosi
      32.9 min, a średnia 35.7 min. Rozkład czasu dojazdu ma długi prawy ogon:
      19 osób dojeżdża dłużej niż godzinę i to one podnoszą średnią. Mediana
      tylko odnotowuje, że leżą powyżej środka."),

    # ========================================================================
    # WIDGET 1: Mean vs Median -- comparison
    # ========================================================================
    lc_h2("ch3-srednia-vs-mediana", "Średnia vs mediana — kiedy się różnią?"),

    lc_p("Różnica między średnią a medianą mówi więc coś o kształcie rozkładu.
      Mechanizm najlepiej widać na danych, w których skrajności są normą:
      na zarobkach. W typowej firmie większość pracowników zarabia umiarkowanie,
      a nieliczni bardzo dużo. W panelu poniżej możesz dopisywać kolejne pensje
      do wylosowanej listy 30 wynagrodzeń, w tym ",
      gloss("wartość odstająca", "wartość odstającą"), " — pensję prezesa."),

    figure_panel(
      label = "Ryc. 3.4",
      title = "Zarobki w firmie: średnia vs mediana",

      lc_toolbar(
        lc_slider("ch3_svm_new_value", "Nowa wartość", 2000, 25000, 5000, 500, suffix = " zł"),
        lc_action("ch3_svm_add", "Dodaj wartość", variant = "solid"),
        lc_action("ch3_svm_outlier", "Dodaj pensję prezesa", variant = "solid"),
        lc_action("ch3_svm_reset", icon = "reset", variant = "ghost", aria_label = "Reset"),
        lc_readouts(uiOutput("ch3_svm_stats"))
      ),

      lc_plot("ch3_svm_hist", max_height = "280px"),
      lc_plot("ch3_svm_strip", ratio = "5.2/1", max_height = "120px")
    ),

    lc_p("Gdy dopisujesz pensje podobne do pozostałych, średnia i mediana
      przesuwają się nieznacznie i trzymają się blisko siebie. Jedna pensja
      prezesa zmienia obraz: przy 30 pensjach średnia skacze o ponad tysiąc złotych, a mediana
      przesuwa się najwyżej o pół pozycji w posortowanej liście."),

    lc_p("Obie liczby odpowiadają na inne pytania. Średnia mówi, ile przypadłoby
      na osobę, gdyby sumę wszystkich pensji podzielić po równo. Mediana mówi,
      ile zarabia osoba stojąca w środku kolejki. Przy zarobkach to drugie pytanie
      zwykle lepiej opisuje typowego pracownika, dlatego w raportach
      o wynagrodzeniach obok średniej podaje się medianę."),

    # ========================================================================
    # WIDGET 2: Robustness mini-demo
    # ========================================================================
    lc_h2("ch3-odpornosc", "Odporność miar na wartości odstające"),

    lc_p("Własność, którą właśnie zaobserwowaliśmy, ma nazwę: ",
      gloss("odporność"), ". Statystyka jest odporna, jeśli pojedyncze skrajne
      obserwacje nie zmieniają jej znacząco. Średnia nie jest odporna,
      mediana jest."),

    lc_p("Między nimi leży ", gloss("średnia ucinana"), ". Odrzucamy ustalony
      odsetek najmniejszych i największych wartości, a ze środka liczymy zwykłą
      średnią. W panelu poniżej ucinamy po 10% z każdej strony: przy 40 pensjach
      odpadają cztery najniższe i cztery najwyższe."),

    figure_panel(
      label = "Ryc. 3.5",
      title = "Odporność: średnia vs mediana vs średnia ucinana",

      lc_toolbar(
        lc_action("ch3_rob_add1", "Dodaj wartość odstającą (ok. 50 000 zł)", variant = "solid"),
        lc_action("ch3_rob_add5", "Dodaj 5 wartości odstających", variant = "solid"),
        lc_action("ch3_rob_reset", icon = "reset", variant = "ghost", aria_label = "Reset"),
        lc_readouts(uiOutput("ch3_rob_outliers_count"))
      ),

      lc_plot("ch3_rob_plot", ratio = "1.9/1", max_height = "320px"),

      uiOutput("ch3_rob_table")
    ),

    lc_p("Pierwsza wartość odstająca wyraźnie podnosi średnią, a średnia ucinana
      i mediana prawie stoją w miejscu. Średnia ucinana chroni jednak tylko
      do pewnej granicy. Gdy wartości odstających jest więcej, niż wynosi
      obcięty odsetek, część z nich trafia do obliczeń. Przy pięciu dodanych
      pensjach obcinamy cztery największe, więc piąta już podnosi wynik.
      Mediana wytrzymuje znacznie więcej: zmienia się wyraźnie dopiero wtedy,
      gdy skrajne wartości stanowią blisko połowę danych."),

    lc_note("Zasada", rule = TRUE,
      "Przy rozkładach skośnych, takich jak zarobki, ceny czy czasy oczekiwania,
       podawaj medianę obok średniej albo zamiast niej."
    ),

    # ========================================================================
    # WIDGET 2b: Discrete variables
    # ========================================================================
    lc_h2("ch3-dyskretna", "Zmienne dyskretne — te same statystyki, inne wykresy"),

    lc_p("Dotąd pracowaliśmy na zmiennych ciągłych: wzroście i zarobkach. ",
      gloss("zmienna dyskretna", "Zmienne dyskretne"), ", takie jak liczba kursów
      czy liczba nieobecności, przyjmują tylko wartości całkowite. Średnią
      i medianę liczymy dla nich tak samo, ale wykres trzeba wybrać ostrożniej."),

    figure_panel(
      label = "Ryc. 3.6",
      title = "Dyskretna vs ciągła — porównanie wizualizacji",
      lc_toolbar(
        lc_segmented("ch3_disc_var", "Zmienna dyskretna",
          choices = c("Liczba nieobecności" = "liczba_nieobecnosci",
                      "Liczba kursów" = "liczba_kursow")),
        lc_readouts(uiOutput("ch3_disc_stats"))
      ),
      lc_plots(
        tags$div(
          tags$h4("Wykres słupkowy (poprawny)"),
          lc_plot("ch3_disc_bar", max_height = "300px")
        ),
        tags$div(
          tags$h4("Histogram (problematyczny)"),
          lc_plot("ch3_disc_hist", max_height = "300px")
        )
      )
    ),

    lc_p("Wykres słupkowy rysuje osobny słupek dla każdej wartości: zero, jednej,
      dwóch nieobecności i tak dalej, więc pokazuje liczebności dokładnie.
      Histogram dzieli oś na przedziały, których granice nie pokrywają się
      z liczbami całkowitymi. Dwie sąsiednie wartości mogą trafić do jednego
      słupka, a niektóre przedziały zostają puste, choć w danych nie ma
      żadnej luki."),

    lc_p("Zwróć też uwagę na średnią. Średnia liczba nieobecności wynosi 2.87,
      choć nikt nie opuścił 2.87 zajęć. Średnia nie musi być wartością, którą
      ktokolwiek faktycznie przyjmuje. Mediana zmiennej dyskretnej zwykle jest
      jedną z jej wartości, tutaj wynosi 3."),

    # ========================================================================
    # WIDGET 2c: Multimodality in continuous distributions
    # ========================================================================
    lc_h2("ch3-modalnosc", "Modalność rozkładu — ile „górek” ma histogram?"),

    lc_p("W rozdziale o ", gloss("zmienna jakościowa", "zmiennych jakościowych"),
      " dominantą nazywaliśmy najczęstszą kategorię. Dla zmiennej ciągłej to
      pojęcie trzeba przerobić: prawie każda wartość występuje tylko raz, więc
      zamiast najczęstszej wartości szukamy najwyższego miejsca histogramu,
      czyli szczytu rozkładu. Szczyt nazywamy ", gloss("moda", "modą"), "."),

    lc_p("Ważniejsze od położenia szczytu bywa to, ile ich jest. Rozkład może
      mieć jeden szczyt (unimodalny), dwa (bimodalny) albo więcej (wielomodalny)."),

    figure_panel(
      label = "Ryc. 3.7",
      title = "Unimodalny vs bimodalny vs wielomodalny",
      lc_toolbar(
        lc_segmented("ch3_modal_scenario", "Scenariusz",
          choices = c("Unimodalny" = "unimodal", "Bimodalny" = "bimodal",
                      "Wielomodalny" = "multimodal"))
      ),
      lc_plot("ch3_modal_plot", ratio = "1.8/1", max_height = "350px"),
      uiOutput("ch3_modal_text")
    ),

    lc_p("Kilka szczytów to zwykle znak, że w danych są pomieszane różne grupy.
      Wtedy jedna statystyka położenia może opisywać wartość, której prawie nikt
      nie ma: średnia wzrostu kobiet i mężczyzn razem wypada pomiędzy szczytami.
      Zanim policzysz średnią, obejrzyj histogram. Jeśli widać kilka szczytów,
      opisz grupy osobno."),

    # ========================================================================
    # WIDGET 3: Percentile explorer
    # ========================================================================
    lc_h2("ch3-percentyle", "Percentyle i kwantyle"),

    lc_p("Mediana dzieli dane na dwie połowy. Ten sam pomysł można uogólnić na
      dowolny podział. ", gloss("percentyl", "Percentyl"), " rzędu p to wartość,
      poniżej której leży p% obserwacji. Na przykład 75. percentyl wzrostu to
      wzrost, którego nie przekracza 75% studentów. Ogólniej mówimy
      o kwantylach rzędu q, gdzie q jest ułamkiem od 0 do 1."),

    lc_p("Najczęściej używamy trzech ", gloss("kwartyl", "kwartyli"),
      ", które dzielą dane na cztery równe części:"),
    tags$ul(
      tags$li(tags$strong("Q1 (25. percentyl)"), " — poniżej leży pierwsza ćwiartka danych"),
      tags$li(tags$strong("Q2 (50. percentyl)"), " — mediana"),
      tags$li(tags$strong("Q3 (75. percentyl)"), " — powyżej leży ostatnia ćwiartka danych")
    ),

    figure_panel(
      label = "Ryc. 3.8",
      title = "Percentyle wzrostu studentów",

      lc_toolbar(
        lc_slider("ch3_q_pct", "Percentyl", 0, 100, 50, 1, suffix = "%"),
        lc_action("ch3_q_q1", "Q1 (25%)", variant = "outline"),
        lc_action("ch3_q_med", "Mediana (50%)", variant = "outline"),
        lc_action("ch3_q_q3", "Q3 (75%)", variant = "outline")
      ),

      lc_plot("ch3_q_hist", max_height = "280px"),
      lc_plot("ch3_q_box", ratio = "5.2/1", max_height = "120px"),
      uiOutput("ch3_q_text")
    ),

    lc_p("W naszej ankiecie Q1 wzrostu wynosi 165.5 cm, a Q3 177.0 cm, więc
      połowa studentów ma wzrost między tymi wartościami. Odległość Q3 − Q1,
      tutaj 11.5 cm, to ",
      gloss("rozstęp międzykwartylowy"), " (IQR). Jest to miara rozrzutu
      odporna na wartości odstające, z tego samego powodu co mediana. Korzysta
      z niej wykres pudełkowy pod histogramem; wrócimy do niego w następnym
      rozdziale."),

    # ====================================================================
    # WIDGET 4: Guess the statistic game
    # ====================================================================
    lc_h2("ch3-gra", "Gra: zgadnij średnią i medianę"),

    lc_p("Na koniec sprawdź, czy potrafisz odczytać obie miary z samego
      histogramu. Kliknij na wykres dwa razy: pierwszy punkt to Twój typ
      średniej, drugi — mediany. Zanim klikniesz, ustal, w którą stronę
      ciągnie ogon rozkładu i po której stronie mediany powinna wtedy
      leżeć średnia."),

    figure_panel(
      label = "Ryc. 3.9",
      title = "Kliknij na wykres, aby umieścić średnią i medianę",
      lc_toolbar(
        lc_action("ch3_game_new", "Nowa runda", variant = "solid"),
        lc_action("ch3_game_reveal", "Pokaż odpowiedź", variant = "outline")
      ),
      uiOutput("ch3_game_status_banner"),
      lc_plot("ch3_game_plot", ratio = "1.8/1", max_height = "350px"),
      uiOutput("ch3_game_feedback")
    ),
    lc_chapter_next(
      num       = "04",
      title     = "Statystyki rozrzutu",
      lead      = "dwie grupy z tą samą średnią mogą wyglądać zupełnie inaczej — różni je rozrzut.",
      target_id = "ch-rozrzut"
    ),

    # Spacer at bottom
    lc_spacer("lg")

  )
)

# --------------------------------------------------------------------------
# Chapter 3 Server
# --------------------------------------------------------------------------

ch3_server <- function(input, output, session) {

  # --------------------------------------------------------------------------
  # Widget: Histogram krok po kroku
  # --------------------------------------------------------------------------

  # Krok widgetu (1..8) żyje w przeglądarce; zmiana zmiennej nie cofa kroku.
  ch3_hist_step <- lc_step_server("ch3_hist", input)$step

  # Default bin widths per variable
  ch3_hist_defaults <- list(
    wzrost = list(min = 1, max = 15, value = 3, step = 1, unit = "cm"),
    waga = list(min = 2, max = 20, value = 5, step = 1, unit = "kg"),
    czas_dojazdu = list(min = 2, max = 20, value = 5, step = 1, unit = "min"),
    srednia_ocen = list(min = 0.1, max = 1, value = 0.3, step = 0.05, unit = "pkt")
  )

  output$ch3_hist_bin_slider <- renderUI({
    d <- ch3_hist_defaults[[input$ch3_hist_var]]
    lc_slider("ch3_hist_bin_width", "Szerokość binu", d$min, d$max, d$value, d$step)
  })

  # Compute bin breaks
  ch3_hist_breaks <- reactive({
    req(input$ch3_hist_bin_width)
    x <- student_data[[input$ch3_hist_var]]
    w <- input$ch3_hist_bin_width
    start <- floor(min(x) / w) * w
    end <- ceiling(max(x) / w) * w + w
    seq(start, end, by = w)
  })

  # Data with bin assignments
  ch3_hist_binned <- reactive({
    req(input$ch3_hist_bin_width)
    x <- student_data[[input$ch3_hist_var]]
    breaks <- ch3_hist_breaks()
    df <- data.frame(value = x)
    df$bin <- cut(df$value, breaks = breaks, include.lowest = TRUE, right = FALSE)
    df$bin_num <- as.numeric(df$bin)
    df
  })

  # Bin statistics
  ch3_hist_stats <- reactive({
    df <- ch3_hist_binned()
    breaks <- ch3_hist_breaks()
    all_bins <- data.frame(
      bin_start = breaks[-length(breaks)],
      bin_end = breaks[-1]
    )
    all_bins$bin_mid <- (all_bins$bin_start + all_bins$bin_end) / 2
    all_bins$bin_num <- seq_len(nrow(all_bins))

    counts <- df %>%
      filter(!is.na(bin)) %>%
      group_by(bin_num) %>%
      summarise(count = n(), .groups = "drop")
    all_bins <- all_bins %>% left_join(counts, by = "bin_num")
    all_bins$count[is.na(all_bins$count)] <- 0

    # Trim to relevant range
    min_d <- min(all_bins$bin_num[all_bins$count > 0])
    max_d <- max(all_bins$bin_num[all_bins$count > 0])
    all_bins %>%
      filter(bin_num >= max(1, min_d - 1),
             bin_num <= min(nrow(all_bins), max_d + 1))
  })

  # Variable labels
  ch3_hist_var_labels <- c(
    "wzrost" = "Wzrost (cm)", "waga" = "Waga (kg)",
    "czas_dojazdu" = "Czas dojazdu (min)",
    "srednia_ocen" = "Średnia ocen"
  )

  zoom_plot_server("ch3_hist_plot", reactive({
    step <- ch3_hist_step()
    var_name <- input$ch3_hist_var
    req(var_name)
    x <- student_data[[var_name]]
    x_label <- ch3_hist_var_labels[var_name]
    n <- length(x)

    x_lo <- min(x) - diff(range(x)) * 0.05
    x_hi <- max(x) + diff(range(x)) * 0.05

    strip_theme <- theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
    # Dwie grupy przynależności do binów: naprzemiennie dane / grupa.
    bin_role_colour <- function(bin_num) {
      ifelse(bin_num %% 2 == 1, STEP_ROLES$data$colour, STEP_ROLES$group$colour)
    }

    if (step == 1) {
      df <- data.frame(value = x)
      ggplot(df, aes(x = value, y = 0)) +
        step_layer(geom_jitter, "data", height = 0.3, size = 3) +
        labs(x = x_label, y = "") + strip_theme +
        step_frame(xlim = c(x_lo, x_hi), ylim = c(-0.5, 0.5))

    } else if (step == 2) {
      df <- data.frame(value = sort(x))
      ggplot(df, aes(x = value, y = 0)) +
        step_layer(geom_point, "data", size = 3) +
        labs(x = x_label, y = "") + strip_theme +
        step_frame(xlim = c(x_lo, x_hi), ylim = c(-0.5, 0.5))

    } else if (step == 3) {
      breaks <- ch3_hist_breaks()
      df <- data.frame(value = sort(x))
      bin_rects <- data.frame(
        xmin = breaks[-length(breaks)], xmax = breaks[-1]
      ) %>% filter(xmax > x_lo, xmin < x_hi)

      ggplot() +
        step_layer(geom_rect, "new", data = bin_rects,
                   mapping = aes(xmin = xmin, xmax = xmax, ymin = -0.35, ymax = 0.35),
                   fill = NA, linetype = "22") +
        step_layer(geom_point, "data", data = df, mapping = aes(x = value, y = 0), size = 2.5) +
        geom_text(data = bin_rects,
                  aes(x = (xmin + xmax) / 2, y = -0.45,
                      label = paste0("[", xmin, ", ", xmax, ")")),
                  size = 2.8, colour = STEP_ROLES$new$colour) +
        labs(x = x_label, y = "") + strip_theme +
        step_frame(xlim = c(x_lo, x_hi), ylim = c(-0.55, 0.5))

    } else if (step == 4) {
      df <- ch3_hist_binned()
      breaks <- ch3_hist_breaks()
      bin_rects <- data.frame(
        xmin = breaks[-length(breaks)], xmax = breaks[-1],
        bin_num = seq_len(length(breaks) - 1)
      ) %>% filter(xmax > x_lo, xmin < x_hi)
      df <- df %>% filter(!is.na(bin))

      ggplot() +
        step_layer(geom_rect, "known", data = bin_rects,
                   mapping = aes(xmin = xmin, xmax = xmax, ymin = -0.35, ymax = 0.35),
                   fill = NA, linetype = "22") +
        geom_jitter(data = df, aes(x = value, y = 0), colour = bin_role_colour(df$bin_num),
                    height = 0.2, size = 3, alpha = STEP_ROLES$data$alpha) +
        labs(x = x_label, y = "") + strip_theme +
        step_frame(xlim = c(x_lo, x_hi), ylim = c(-0.5, 0.5))

    } else if (step == 5) {
      df <- ch3_hist_binned() %>% filter(!is.na(bin))
      stats <- ch3_hist_stats()

      ggplot() +
        step_layer(geom_rect, "known", data = stats,
                   mapping = aes(xmin = bin_start, xmax = bin_end, ymin = -0.35, ymax = 0.35),
                   fill = NA, linetype = "22") +
        geom_jitter(data = df, aes(x = value, y = 0), colour = bin_role_colour(df$bin_num),
                    height = 0.2, size = 2, alpha = STEP_ROLES$data$alpha) +
        geom_text(data = stats,
                  aes(x = bin_mid, y = 0.45,
                      label = ifelse(count > 0, paste0("n=", count), "")),
                  size = 4, fontface = "bold", family = lc_mono_family, colour = STEP_ROLES$new$colour) +
        labs(x = x_label, y = "") + strip_theme +
        step_frame(xlim = c(x_lo, x_hi), ylim = c(-0.5, 0.6))

    } else if (step == 6) {
      stats <- ch3_hist_stats()
      w <- input$ch3_hist_bin_width

      ggplot(stats, aes(x = bin_mid, y = count)) +
        step_result(geom_col, width = w * 0.95, linewidth = 0.9) +
        geom_text(aes(label = count), vjust = -0.5, size = 4, fontface = "bold",
                  family = lc_mono_family, colour = STEP_ROLES$known$colour) +
        labs(x = x_label, y = "Liczba obserwacji") +
        step_frame(xlim = c(x_lo, x_hi))

    } else if (step == 7) {
      df <- data.frame(value = x)
      w <- input$ch3_hist_bin_width
      widths <- c(w / 2, w, w * 2)
      unit <- ch3_hist_defaults[[var_name]]$unit
      labels <- paste0("Bin = ", widths, " ", unit)

      plots <- lapply(seq_along(widths), function(i) {
        ggplot(df, aes(x = value)) +
          # Granice binów od tej samej dolnej krawędzi co w krokach 3–6.
          geom_histogram(binwidth = widths[i], boundary = floor(min(x) / w) * w,
                         fill = c(upwr_accent, upwr_cat["niebo"], upwr_cat["szalwia"])[i],
                         alpha = 0.7, color = upwr_secondary, linewidth = 0.3) +
          labs(x = if (i == 2) x_label else "", y = if (i == 1) "Liczba obs." else "") +
                    theme(plot.title = element_text(
            size = 12, face = "bold",
            color = c(upwr_accent, upwr_cat["niebo"], upwr_cat["szalwia"])[i]))
      })
      gridExtra::arrangeGrob(grobs = plots, ncol = 3)
    }
  }))

  output$ch3_hist_text <- renderUI({
    step <- ch3_hist_step()
    var_name <- input$ch3_hist_var
    req(var_name)
    x <- student_data[[var_name]]
    n <- length(x)
    unit <- ch3_hist_defaults[[var_name]]$unit

    txt <- switch(as.character(step),
      "1" = paste0("Mamy ", n, " obserwacji — każdy punkt to jedna wartość. ",
                   "Trudno z tego odczytać rozkład, prawda?"),
      "2" = paste0("Sortujemy od min = ", round(min(x), 1),
                   " do max = ", round(max(x), 1), " ", unit,
                   ". Widać zagęszczenia, ale wciąż nieczytelne."),
      "3" = paste0("Dzielimy oś na równe przedziały (biny) o szerokości ",
                   input$ch3_hist_bin_width, " ", unit,
                   ". Każdy bin to 'koszyk' na obserwacje."),
      "4" = "Każda obserwacja trafia do swojego binu — kolor = przynależność.",
      "5" = "Liczymy obserwacje w każdym binie. Te liczby staną się wysokością słupków.",
      "6" = "Zamieniamy punkty na słupki — wysokość = liczba obserwacji. To już jest histogram.",
      "7" = paste0("Te same dane z trzema szerokościami binu. ",
                   "Za wąskie → szum. Za szerokie → utrata szczegółów.")
    )
    txt
  })

  output$ch3_hist_table <- renderUI({
    if (ch3_hist_step() < 5) return(NULL)
    stats <- ch3_hist_stats()
    n <- length(student_data[[input$ch3_hist_var]])

    result <- stats %>% filter(count > 0) %>%
      mutate(pct = round(count / n * 100, 1))
    out <- data.frame(
      bin = paste0("[", result$bin_start, ", ", result$bin_end, ")"),
      count = result$count,
      pct = result$pct
    )
    lc_table(out, cols = list(
      lc_col("bin", "Przedział", "row"),
      lc_col("count", "Liczba obs."),
      lc_col("pct", "Procent (%)", digits = 1)
    ))
  })

  # --------------------------------------------------------------------------
  # Widget 0a: Mean introduction
  # --------------------------------------------------------------------------

  zoom_plot_server("ch3_mean_plot", reactive({
    var_name <- input$ch3_mean_var
    req(var_name)
    x <- student_data[[var_name]]
    m <- mean(x)
    var_labels <- c("wzrost" = "Wzrost (cm)", "waga" = "Waga (kg)",
                    "srednia_ocen" = "Średnia ocen")
    df <- data.frame(val = x)

    ggplot(df, aes(x = val)) +
      geom_histogram(bins = 25, fill = upwr_rule, color = "white", alpha = 0.8) +
      geom_vline(xintercept = m, color = upwr_accent, linewidth = 1.5, linetype = "solid") +
      annotate("text", x = m, y = Inf, label = paste0("Średnia = ", round(m, 2)),
               vjust = 2, hjust = -0.1, color = upwr_accent, size = 5, fontface = "bold") +
      annotate("segment", x = min(x), xend = m, y = -0.5, yend = -0.5,
               color = upwr_cat["niebo"], linewidth = 2,
               arrow = arrow(length = unit(0.2, "cm"), ends = "last")) +
      annotate("segment", x = max(x), xend = m, y = -0.5, yend = -0.5,
               color = upwr_cat["niebo"], linewidth = 2,
               arrow = arrow(length = unit(0.2, "cm"), ends = "last")) +
      labs(x = var_labels[var_name], y = "Liczebność") +
      theme()
  }))

  output$ch3_mean_text <- renderUI({
    var_name <- input$ch3_mean_var
    req(var_name)
    x <- student_data[[var_name]]
    m <- mean(x)
    s <- sum(x)
    n <- length(x)
    lc_status(
      withMathJax(paste0(
        "$$\\bar{x} = \\frac{", round(s, 1), "}{", n, "} = ", round(m, 2), "$$"
      ))
    )
  })

  # --------------------------------------------------------------------------
  # Widget 0b: Median introduction
  # --------------------------------------------------------------------------

  zoom_plot_server("ch3_median_plot", reactive({
    var_name <- input$ch3_median_var
    req(var_name)
    x <- student_data[[var_name]]
    med <- median(x)
    x_sorted <- sort(x)
    n <- length(x_sorted)
    n_below <- sum(x_sorted < med)
    n_above <- sum(x_sorted > med)
    var_labels <- c("wzrost" = "Wzrost (cm)", "czas_dojazdu" = "Czas dojazdu (min)",
                    "srednia_ocen" = "Średnia ocen")
    df <- data.frame(val = x)

    ggplot(df, aes(x = val)) +
      geom_histogram(bins = 25, fill = upwr_rule, color = "white", alpha = 0.8) +
      geom_vline(xintercept = med, color = upwr_cat["indygo"], linewidth = 1.5) +
      annotate("rect", xmin = min(x) - 1, xmax = med, ymin = -Inf, ymax = Inf,
               fill = upwr_cat["niebo"], alpha = 0.08) +
      annotate("rect", xmin = med, xmax = max(x) + 1, ymin = -Inf, ymax = Inf,
               fill = upwr_accent, alpha = 0.08) +
      annotate("text", x = (min(x) + med) / 2, y = Inf,
               label = paste0("50% (", n_below, " obs.)"),
               vjust = 2, color = upwr_secondary, size = 5, fontface = "bold") +
      annotate("text", x = (max(x) + med) / 2, y = Inf,
               label = paste0("50% (", n_above, " obs.)"),
               vjust = 2, color = upwr_secondary, size = 5, fontface = "bold") +
      annotate("text", x = med, y = Inf, label = paste0("Me = ", round(med, 1)),
               vjust = 4, hjust = -0.1, color = upwr_cat["indygo"], size = 5, fontface = "bold") +
      geom_histogram(bins = 25, fill = upwr_rule, color = "white", alpha = 0.8) +
      geom_vline(xintercept = med, color = upwr_cat["indygo"], linewidth = 1.5) +
      labs(x = var_labels[var_name], y = "Liczebność") +
      theme()
  }))

  output$ch3_median_text <- renderUI({
    var_name <- input$ch3_median_var
    req(var_name)
    x <- student_data[[var_name]]
    med <- median(x)
    m <- mean(x)
    diff <- abs(m - med)

    lc_status(
      paste0("Mediana = ", round(med, 1)),
      " | Średnia = ",
      round(m, 2),
      " | Różnica = ",
      round(diff, 2)
    )
  })

  # --------------------------------------------------------------------------
  # Widget 1: Mean vs Median comparison
  # --------------------------------------------------------------------------

  ch3_svm_generate <- function() {
    round(rgamma(30, shape = 3, scale = 1500) + 2000)
  }

  ch3_svm_data <- reactiveVal(NULL)

  observe({
    if (is.null(ch3_svm_data())) {
      set.seed(NULL)
      ch3_svm_data(ch3_svm_generate())
    }
  })

  observeEvent(input$ch3_svm_add, {
    ch3_svm_data(c(ch3_svm_data(), input$ch3_svm_new_value))
  })

  observeEvent(input$ch3_svm_outlier, {
    ch3_svm_data(c(ch3_svm_data(), 50000))
  })

  observeEvent(input$ch3_svm_reset, {
    set.seed(NULL)
    ch3_svm_data(ch3_svm_generate())
  })

  zoom_plot_server("ch3_svm_hist", reactive({
    req(ch3_svm_data())
    d <- data.frame(x = ch3_svm_data())
    m <- mean(d$x)
    med <- median(d$x)

    ggplot(d, aes(x = x)) +
      geom_histogram(fill = upwr_reference, color = "white", bins = 25) +
      geom_vline(aes(xintercept = m, color = "Średnia"),
                 linewidth = 1.2, linetype = "solid") +
      geom_vline(aes(xintercept = med, color = "Mediana"),
                 linewidth = 1.2, linetype = "dashed") +
      scale_color_manual(
        name = NULL,
        breaks = c("Średnia", "Mediana"),
        values = c("Średnia" = upwr_accent, "Mediana" = upwr_cat["niebo"])
      ) +
      scale_x_continuous(labels = function(x) format(x, big.mark = " ")) +
      labs(x = "Zarobki (zł)", y = "Liczba osób") +
      theme(legend.position = "top")
  }))

  zoom_plot_server("ch3_svm_strip", reactive({
    req(ch3_svm_data())
    d <- data.frame(x = ch3_svm_data())
    m <- mean(d$x)
    med <- median(d$x)

    ggplot(d, aes(x = x, y = 0)) +
      geom_jitter(height = 0.3, width = 0, size = 2.5,
                  alpha = 0.6, color = upwr_secondary) +
      geom_point(aes(x = m), y = 0, color = upwr_accent,
                 size = 5, shape = 18) +
      geom_point(aes(x = med), y = 0, color = upwr_cat["niebo"],
                 size = 5, shape = 18) +
      scale_x_continuous(labels = function(x) format(x, big.mark = " ")) +
      labs(x = "Zarobki (zł)", y = NULL) +
            theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  }))

  output$ch3_svm_stats <- renderUI({
    req(ch3_svm_data())
    d <- ch3_svm_data()
    m <- mean(d)
    med <- median(d)
    diff_val <- m - med

    diff_color <- if (abs(diff_val) < 500) upwr_cat["szalwia"] else upwr_cat["bursztyn"]

    tagList(
      lc_readout("Średnia", paste0(lc_fmt(m, 0, big_mark = " "), " zł"), color = upwr_accent),
      lc_readout("Mediana", paste0(lc_fmt(med, 0, big_mark = " "), " zł"), color = upwr_cat[["niebo"]]),
      lc_readout("Różnica", paste0(lc_fmt(diff_val, 0, big_mark = " "), " zł"), color = unname(diff_color))
    )
  })

  # --------------------------------------------------------------------------
  # Widget 2: Robustness mini-demo
  # --------------------------------------------------------------------------

  ch3_rob_generate <- function() {
    round(rgamma(40, shape = 3, scale = 1500) + 2000)
  }

  ch3_rob_base <- reactiveVal(NULL)
  ch3_rob_outliers <- reactiveVal(numeric(0))

  observe({
    if (is.null(ch3_rob_base())) {
      set.seed(NULL)
      ch3_rob_base(ch3_rob_generate())
    }
  })

  ch3_rob_all <- reactive({
    c(ch3_rob_base(), ch3_rob_outliers())
  })

  # Store baseline stats for comparison
  ch3_rob_base_stats <- reactive({
    req(ch3_rob_base())
    d <- ch3_rob_base()
    list(
      mean = mean(d),
      median = median(d),
      trimmed = mean(d, trim = 0.1)
    )
  })

  observeEvent(input$ch3_rob_add1, {
    new_outlier <- 50000 + runif(1, -5000, 5000)
    ch3_rob_outliers(c(ch3_rob_outliers(), round(new_outlier)))
  })

  observeEvent(input$ch3_rob_add5, {
    new_outliers <- round(50000 + runif(5, -5000, 5000))
    ch3_rob_outliers(c(ch3_rob_outliers(), new_outliers))
  })

  observeEvent(input$ch3_rob_reset, {
    set.seed(NULL)
    ch3_rob_base(ch3_rob_generate())
    ch3_rob_outliers(numeric(0))
  })

  zoom_plot_server("ch3_rob_plot", reactive({
    req(ch3_rob_all())
    d <- data.frame(x = ch3_rob_all())
    m <- mean(d$x)
    med <- median(d$x)
    tr <- mean(d$x, trim = 0.1)

    line_data <- data.frame(
      xval = c(m, med, tr),
      Statystyka = factor(
        c("Średnia", "Mediana", "Śr. ucinana (10%)"),
        levels = c("Średnia", "Mediana", "Śr. ucinana (10%)")
      ),
      ltype = c("solid", "dashed", "dotted")
    )

    ggplot(d, aes(x = x)) +
      geom_histogram(fill = upwr_reference, color = "white", bins = 30) +
      geom_vline(data = line_data,
                 aes(xintercept = xval, color = Statystyka,
                     linetype = Statystyka),
                 linewidth = 1.2) +
      scale_color_manual(
        name = NULL,
        breaks = c("Średnia", "Mediana", "Śr. ucinana (10%)"),
        values = c("Średnia" = upwr_accent,
                   "Mediana" = upwr_cat["niebo"],
                   "Śr. ucinana (10%)" = upwr_cat["szalwia"])
      ) +
      scale_linetype_manual(
        name = NULL,
        breaks = c("Średnia", "Mediana", "Śr. ucinana (10%)"),
        values = c("Średnia" = "solid",
                   "Mediana" = "dashed",
                   "Śr. ucinana (10%)" = "dotted")
      ) +
      scale_x_continuous(labels = function(x) format(x, big.mark = " ")) +
      labs(x = "Zarobki (zł)", y = "Liczba osób") +
      theme(legend.position = "top")
  }))

  output$ch3_rob_outliers_count <- renderUI({
    n_outliers <- length(ch3_rob_outliers())
    lc_readout("Dodane wartości odstające", n_outliers, color = upwr_secondary)
  })

  output$ch3_rob_table <- renderUI({
    req(ch3_rob_all())
    req(ch3_rob_base_stats())

    d <- ch3_rob_all()
    base <- ch3_rob_base_stats()

    current_mean <- mean(d)
    current_med <- median(d)
    current_tr <- mean(d, trim = 0.1)

    lc_table(
      data.frame(
        stat = c("Średnia", "Mediana", "Średnia ucinana (10%)"),
        value = c(current_mean, current_med, current_tr),
        change = c(current_mean - base$mean, current_med - base$median,
                   current_tr - base$trimmed)
      ),
      cols = list(
        lc_col("stat", "Statystyka", "row"),
        lc_col("value", "Wartość (zł)", digits = 0),
        lc_col("change", "Zmiana (zł)", digits = 0,
           short = "Zmiana", desc = "różnica względem danych bez dodanych wartości odstających")
      ),
      label = "Statystyki położenia po dodaniu wartości odstających"
    )
    })

  # --------------------------------------------------------------------------
  # Widget 2b: Discrete variables

  zoom_plot_server("ch3_disc_bar", reactive({
    var_name <- input$ch3_disc_var
    req(var_name)
    vals <- student_data[[var_name]]
    df <- data.frame(x = factor(vals))

    ggplot(df, aes(x = x)) +
      geom_bar(fill = type_colors["ilosciowa_dyskretna"], color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = variable_meta[[var_name]]$label, y = "Liczebność") +
      theme()
  }))

  zoom_plot_server("ch3_disc_hist", reactive({
    var_name <- input$ch3_disc_var
    req(var_name)
    vals <- student_data[[var_name]]

    ggplot(data.frame(x = vals), aes(x = x)) +
      geom_histogram(bins = 15, fill = upwr_accent, color = "white", alpha = 0.6) +
      labs(x = variable_meta[[var_name]]$label, y = "Liczebność") +
      theme()
  }))

  output$ch3_disc_stats <- renderUI({
    var_name <- input$ch3_disc_var
    req(var_name)
    vals <- student_data[[var_name]]

    mode_val <- as.numeric(names(sort(table(vals), decreasing = TRUE))[1])

    tagList(
      lc_readout("Średnia", lc_fmt(mean(vals), 2)),
      lc_readout("Mediana", median(vals)),
      lc_readout("Moda", mode_val),
      lc_readout("SD", lc_fmt(sd(vals), 2)),
      lc_readout("Zakres", paste0(min(vals), "–", max(vals)))
    )
    })

  # Widget 2c: Multimodality in continuous distributions
  # --------------------------------------------------------------------------

  zoom_plot_server("ch3_modal_plot", reactive({
    scenario <- input$ch3_modal_scenario
    req(scenario)

    set.seed(42)
    if (scenario == "unimodal") {
      x <- rnorm(500, mean = 165, sd = 6)
      df <- data.frame(val = x)
      ggplot(df, aes(x = val)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = upwr_cat["niebo"], color = "white", alpha = 0.7) +
        geom_density(linewidth = 1.2, color = upwr_secondary) +
        geom_vline(xintercept = mean(x), color = upwr_accent, linewidth = 1, linetype = "dashed") +
        annotate("text", x = mean(x) + 1, y = Inf, label = "moda ≈ średnia ≈ mediana",
                 hjust = 0, vjust = 2, color = upwr_accent, size = 4.5, fontface = "bold") +
        labs(x = "Wzrost kobiet (cm)", y = "Gęstość") +
        theme()

    } else if (scenario == "bimodal") {
      x_k <- rnorm(250, mean = 162, sd = 5)
      x_m <- rnorm(250, mean = 182, sd = 5)
      df <- data.frame(val = c(x_k, x_m),
                       grupa = rep(c("Kobiety", "Mężczyźni"), each = 250))
      ggplot(df, aes(x = val)) +
        geom_histogram(aes(y = after_stat(density)), bins = 35,
                       fill = upwr_reference, color = "white", alpha = 0.5) +
        geom_density(linewidth = 1.2, color = upwr_secondary) +
        geom_density(aes(color = grupa), linewidth = 0.8, linetype = "dashed") +
        scale_color_manual(values = c("Kobiety" = upwr_accent, "Mężczyźni" = upwr_cat["niebo"])) +
        labs(x = "Wzrost (cm)", y = "Gęstość", color = NULL) +
                theme(legend.position = "top")

    } else {
      x1 <- rnorm(150, mean = 12, sd = 3)
      x2 <- rnorm(120, mean = 25, sd = 4)
      x3 <- rnorm(130, mean = 40, sd = 5)
      df <- data.frame(val = c(x1, x2, x3),
                       grupa = c(rep("Rower", 150), rep("Autobus", 120), rep("Auto", 130)))
      ggplot(df, aes(x = val)) +
        geom_histogram(aes(y = after_stat(density)), bins = 40,
                       fill = upwr_reference, color = "white", alpha = 0.5) +
        geom_density(linewidth = 1.2, color = upwr_secondary) +
        geom_density(aes(color = grupa), linewidth = 0.8, linetype = "dashed") +
        scale_color_manual(values = c("Rower" = upwr_cat["szalwia"], "Autobus" = upwr_cat["bursztyn"], "Auto" = upwr_accent)) +
        labs(x = "Czas dojazdu (min)", y = "Gęstość", color = NULL) +
                theme(legend.position = "top")
    }
  }))

  output$ch3_modal_text <- renderUI({
    scenario <- input$ch3_modal_scenario
    req(scenario)

    if (scenario == "unimodal") {
      lc_status(
        tags$b("Rozkład unimodalny: "),
        "jeden szczyt. ",
        "Dla rozkładu symetrycznego moda ≈ średnia ≈ mediana. ",
        "Większość statystyk opisowych zakłada właśnie taki rozkład."
      )
    } else if (scenario == "bimodal") {
      lc_status(
        lc_verdict(tags$b("Rozkład bimodalny: "), type = "warning"),
        "dwa szczyty, osobno dla kobiet ",
        "i mężczyzn. Średnia całości wypada między szczytami, ",
        "gdzie obserwacji jest niewiele."
      )
    } else {
      lc_status(
        lc_verdict(tags$b("Rozkład wielomodalny: "), type = "warning"),
        "trzy szczyty = trzy podgrupy. ",
        "Każda podgrupa (rowerzyści, pasażerowie autobusów, kierowcy) ",
        "ma własną typową wartość."
      )
    }
  })

  # Widget 3: Percentile explorer
  # --------------------------------------------------------------------------

  # Quick-select buttons
  observeEvent(input$ch3_q_q1, {
    updateSliderInput(session, "ch3_q_pct", value = 25)
  })

  observeEvent(input$ch3_q_med, {
    updateSliderInput(session, "ch3_q_pct", value = 50)
  })

  observeEvent(input$ch3_q_q3, {
    updateSliderInput(session, "ch3_q_pct", value = 75)
  })

  zoom_plot_server("ch3_q_hist", reactive({
    pct <- input$ch3_q_pct / 100
    wzrost <- student_data$wzrost
    q_val <- quantile(wzrost, probs = pct)

    d <- data.frame(x = wzrost)
    d$below <- d$x <= q_val

    ggplot(d, aes(x = x)) +
      geom_histogram(aes(fill = below), color = "white", bins = 25,
                     boundary = q_val, show.legend = FALSE) +
      geom_vline(xintercept = q_val, color = upwr_secondary,
                 linewidth = 1.2, linetype = "solid") +
      annotate("text", x = q_val, y = Inf,
               label = paste0(round(q_val, 1), " cm"),
               vjust = -0.5, hjust = -0.1,
               fontface = "bold", size = 5, color = upwr_secondary) +
      scale_fill_manual(values = c("TRUE" = upwr_cat["niebo"], "FALSE" = upwr_reference)) +
      labs(x = "Wzrost (cm)", y = "Liczba studentów") +
      theme()
  }))

  zoom_plot_server("ch3_q_box", reactive({
    pct <- input$ch3_q_pct / 100
    wzrost <- student_data$wzrost
    q_val <- quantile(wzrost, probs = pct)

    d <- data.frame(x = wzrost)

    ggplot(d, aes(x = x, y = 0)) +
      geom_boxplot(fill = upwr_rule, color = upwr_secondary,
                   width = 0.5, outlier.alpha = 0.4) +
      geom_point(aes(x = q_val), y = 0,
                 color = upwr_accent, size = 5, shape = 18) +
      annotate("text", x = q_val, y = 0.35,
               label = paste0("P", input$ch3_q_pct),
               fontface = "bold", size = 4.5, color = upwr_accent) +
      labs(x = "Wzrost (cm)", y = NULL) +
            theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  }))

  output$ch3_q_text <- renderUI({
    pct <- input$ch3_q_pct / 100
    wzrost <- student_data$wzrost
    q_val <- round(quantile(wzrost, probs = pct), 1)

    actual_pct <- round(100 * mean(wzrost <= q_val), 1)

    lc_status(
      p(paste0(input$ch3_q_pct, "% studentów ma wzrost poniżej ", q_val, " cm.")),
      lc_caption(paste0("Dokładnie ", actual_pct, "% obserwacji ≤ ", q_val, " cm."))
    )
  })

  # ========================================================================
  # Widget 4: Guess the statistic game
  # ========================================================================

  ch3_game_data <- reactiveVal(NULL)
  ch3_game_guesses <- reactiveVal(list(mean = NULL, median = NULL))
  ch3_game_revealed <- reactiveVal(FALSE)
  ch3_game_round <- reactiveVal(0)
  ch3_game_score <- reactiveVal(list(total = 0, good = 0))

  generate_game_distribution <- function() {
    type <- sample(c("symmetric", "right_skew", "left_skew"), 1)
    n <- 200
    if (type == "symmetric") {
      vals <- rnorm(n, mean = sample(40:80, 1), sd = sample(8:15, 1))
    } else if (type == "right_skew") {
      vals <- rgamma(n, shape = sample(2:4, 1), scale = sample(5:12, 1)) + sample(10:30, 1)
    } else {
      vals <- 100 - rgamma(n, shape = sample(2:4, 1), scale = sample(5:12, 1))
    }
    round(vals, 1)
  }

  observe({
    if (is.null(ch3_game_data())) {
      ch3_game_data(generate_game_distribution())
    }
  })

  observeEvent(input$ch3_game_new, {
    ch3_game_data(generate_game_distribution())
    ch3_game_guesses(list(mean = NULL, median = NULL))
    ch3_game_revealed(FALSE)
    ch3_game_round(ch3_game_round() + 1)
  })

  observeEvent(input$ch3_game_click, {
    if (ch3_game_revealed()) return()
    g <- ch3_game_guesses()
    if (is.null(g$mean)) {
      g$mean <- input$ch3_game_click$x
    } else if (is.null(g$median)) {
      g$median <- input$ch3_game_click$x
    }
    ch3_game_guesses(g)
  })

  observeEvent(input$ch3_game_reveal, {
    g <- ch3_game_guesses()
    req(g$mean, g$median)
    ch3_game_revealed(TRUE)
    vals <- ch3_game_data()
    real_mean <- mean(vals)
    real_med <- median(vals)
    rng <- diff(range(vals))
    mean_err <- abs(g$mean - real_mean) / rng
    med_err <- abs(g$median - real_med) / rng
    sc <- ch3_game_score()
    sc$total <- sc$total + 1
    if (mean_err < 0.08 && med_err < 0.08) sc$good <- sc$good + 1
    ch3_game_score(sc)
  })

  output$ch3_game_status_banner <- renderUI({
    g <- ch3_game_guesses()
    if (is.null(g$mean)) {
      lc_caption("Kliknij na wykresie, gdzie Twoim zdaniem leży średnia.", tone = "info")
    } else if (is.null(g$median)) {
      lc_caption("Teraz kliknij, gdzie leży mediana.", tone = "info")
    } else if (!ch3_game_revealed()) {
      lc_caption("Gotowe. Kliknij „Pokaż odpowiedź”.", tone = "ok")
    } else {
      sc <- ch3_game_score()
      lc_status(p(paste0("Trafione rundy: ", sc$good, " z ", sc$total, ".")))
    }
  })

  zoom_plot_server("ch3_game_plot", reactive({
    vals <- ch3_game_data()
    req(vals)
    g <- ch3_game_guesses()
    revealed <- ch3_game_revealed()

    p <- ggplot(data.frame(x = vals), aes(x = x)) +
      geom_histogram(bins = 25, fill = "grey70", color = "white", alpha = 0.7) +
      labs(x = "Wartość", y = "Liczebność")

    if (!is.null(g$mean)) {
      p <- p + geom_vline(xintercept = g$mean, color = upwr_accent,
                          linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = g$mean, y = Inf, label = "Twoja\nśrednia",
                 vjust = 2, color = upwr_accent, fontface = "bold", size = 3.5)
    }
    if (!is.null(g$median)) {
      p <- p + geom_vline(xintercept = g$median, color = upwr_cat["niebo"],
                          linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = g$median, y = Inf, label = "Twoja\nmediana",
                 vjust = 3.5, color = upwr_cat["niebo"], fontface = "bold", size = 3.5)
    }

    if (revealed) {
      real_mean <- mean(vals)
      real_med <- median(vals)
      p <- p +
        geom_vline(xintercept = real_mean, color = upwr_accent, linewidth = 1.5) +
        annotate("text", x = real_mean, y = Inf, label = paste0("Średnia\n", round(real_mean, 1)),
                 vjust = 1, color = upwr_accent, fontface = "bold", size = 4) +
        geom_vline(xintercept = real_med, color = upwr_cat["niebo"], linewidth = 1.5) +
        annotate("text", x = real_med, y = Inf, label = paste0("Mediana\n", round(real_med, 1)),
                 vjust = 2.5, color = upwr_cat["niebo"], fontface = "bold", size = 4)
    }

    p
  }))

  output$ch3_game_feedback <- renderUI({
    if (!ch3_game_revealed()) return(NULL)
    vals <- ch3_game_data()
    g <- ch3_game_guesses()
    real_mean <- mean(vals)
    real_med <- median(vals)

    mean_err <- round(abs(g$mean - real_mean), 1)
    med_err <- round(abs(g$median - real_med), 1)
    rng <- diff(range(vals))

    overall_err <- (abs(g$mean - real_mean) + abs(g$median - real_med)) / rng
    if (overall_err < 0.08) {
      grade <- "Doskonale!"
      cls <- "info"
    } else if (overall_err < 0.15) {
      grade <- "Nieźle!"
      cls <- "warning"
    } else {
      grade <- "Można lepiej!"
      cls <- "danger"
    }

    lc_status(
      lc_verdict(tags$strong(paste0(grade, " ")), type = cls),
      paste0("Błąd średniej: ", mean_err, ", błąd mediany: ", med_err, ".")
    )
  })

}
