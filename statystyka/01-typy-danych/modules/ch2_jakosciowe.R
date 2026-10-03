# ============================================================================
# CHAPTER 2: Zmienne jakościowe
# ============================================================================

ch2_ui <- list(
  id = "ch-jakosciowe", num = "02", title = "Zmienne jakościowe",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 02 · Statystyka opisowa",
      num    = "02",
      title  = "Zmienne jakościowe.",
      lead   = "Kierunku studiów ani grupy krwi nie da się dodać ani uśrednić.
                Takie dane można tylko policzyć, ale z samego liczenia da się
                wyciągnąć zaskakująco dużo: tabelę, wykres, porównanie grup
                i wartość typową."
    ),

    lc_p("W poprzednim rozdziale podzieliliśmy zmienne na typy. Zaczynamy od ",
      gloss("zmienna jakościowa", "zmiennych jakościowych"), ", czyli takich,
      których wartościami są kategorie: kierunek studiów, płeć, grupa krwi,
      poziom zadowolenia. Kategorii nie da się dodawać ani mnożyć, więc
      podstawowe, co możemy z nimi zrobić, to policzyć, ile razy każda z nich
      występuje. Wszystkie narzędzia w tym rozdziale wyrastają z tego liczenia."),

    # ========================================================================
    # WIDGET 1: Frequency table step-by-step
    # ========================================================================
    lc_h2("ch2-tabela-czestosci", "Tabela częstości"),

    lc_p("Wynik liczenia zapisujemy w ", gloss("tabela częstości", "tabeli częstości"),
      ". Dla każdej kategorii podaje ona ", gloss("liczebność"), " \\(n_i\\),
      czyli liczbę obserwacji w tej kategorii, oraz ",
      gloss("częstość względna", "częstość względną"), " \\(f_i\\), czyli udział
      kategorii w całej próbie liczącej \\(n\\) obserwacji. Częstość względną
      podajemy jako ułamek albo jako procent."),

    lc_formula_box(withMathJax(
      "$$f_i = \\frac{n_i}{n}, \\qquad p_i = f_i \\cdot 100\\%$$"
    )),

    lc_p("Tabelę uzupełnia ", gloss("częstość skumulowana"), ": suma częstości
      od pierwszej kategorii do bieżącej, podawana zwykle w procentach. Panel poniżej buduje tabelę krok
      po kroku dla dwóch zmiennych z ankiety 200 studentów: kierunku studiów
      i zadowolenia ze studiów."),

    figure_panel(
      label = "Ryc. 2.1",
      width_mode = "text",
      lc_step_widget("ch2_freq",
        title = "Tabela częstości — krok po kroku",
        steps = c("Surowe dane", "Zliczanie", "Częstości względne", "Skumulowane"),
        toolbar = lc_toolbar(
          lc_segmented("ch2_freq_var", "Zmienna",
            choices = c(
              "Kierunek (nominalna)" = "kierunek",
              "Zadowolenie (porządkowa)" = "zadowolenie"
            ),
            selected = "kierunek"
          )
        ),
        body = uiOutput("ch2_freq_table")
      )
    ),

    lc_p("Najliczniejszym kierunkiem w ankiecie jest Informatyka: 60 osób, czyli
      30% próby. Dalej są Biologia (51 osób, 25.5%), Ekonomia (49 osób, 24.5%)
      i Psychologia (40 osób, 20%). Częstości względne sumują się do 1,
      a procenty do 100, co jest prostym sprawdzianem poprawności tabeli."),

    lc_p("Ostatni krok wygląda dla obu zmiennych tak samo, ale znaczy co innego.
      Dla zadowolenia procent skumulowany odpowiada na sensowne pytanie: 18.5%
      studentów jest niezadowolonych lub bardzo niezadowolonych, a 53.5% ocenia
      studia co najwyżej neutralnie. Dla kierunku ta sama kolumna sumuje
      kategorie w kolejności alfabetycznej, więc liczba 80% przy Informatyce
      niczego nie opisuje. O tym, czy liczenie narastająco ma sens, decyduje
      rodzaj zmiennej."),

    # ========================================================================
    # WIDGET 1b: Nominal vs Ordinal comparison
    # ========================================================================
    lc_h2("ch2-nominalna-vs-porzadkowa", "Nominalna vs porządkowa — czy kolejność ma znaczenie?"),

    lc_p("Zmienne jakościowe dzielimy na dwa rodzaje. ",
      gloss("zmienna nominalna", "Zmienne nominalne"), " mają kategorie bez
      naturalnej kolejności, jak kierunek studiów czy grupa krwi. ",
      gloss("zmienna porządkowa", "Zmienne porządkowe"), " mają kategorie,
      które da się uszeregować, jak zadowolenie od „bardzo niezadowolony” do
      „bardzo zadowolony”. Od tego podziału zależy, które operacje mają sens:
      kategorie porządkowe można sumować narastająco i porównywać jako „wyżej”
      i „niżej”, nominalnych nie. Panel pokazuje obie zmienne obok siebie,
      a przełącznik ustawia słupki w losowej kolejności."),

    figure_panel(
      label = "Ryc. 2.2",
      title = "Czy kolejność kategorii ma znaczenie?",

      checkboxInput("ch2_ord_shuffle", "Losowa kolejność kategorii", value = FALSE),

      fluidRow(
        column(6,
          h5(style = paste0("text-align: center; color: ", type_colors["nominalna"], ";"), "Nominalna: Kierunek studiów"),
          zoom_plot_ui("ch2_ord_nom_plot", height = "300px")
        ),
        column(6,
          h5(style = paste0("text-align: center; color: ", type_colors["porzadkowa"], ";"), "Porządkowa: Zadowolenie"),
          zoom_plot_ui("ch2_ord_ord_plot", height = "300px")
        )
      ),

      uiOutput("ch2_ord_explanation")
    ),

    lc_p("Po przetasowaniu wykres kierunków mówi dokładnie to samo co wcześniej:
      wysokości słupków się nie zmieniają, a porządek alfabetyczny i tak był
      umowny. Wykres zadowolenia traci natomiast informację. W naturalnej
      kolejności widać, że odpowiedzi skupiają się w środku i po dodatniej
      stronie skali (70 osób neutralnych, 65 zadowolonych) i że ocen pozytywnych
      jest wyraźnie więcej niż negatywnych (93 wobec 37). Po przetasowaniu
      tego kształtu już nie widać. Dlatego kategorie zmiennej porządkowej
      zawsze pokazujemy w ich naturalnej kolejności, a dla nominalnej wybieramy
      kolejność, która ułatwia porównanie, na przykład od najliczniejszej."),

    # ========================================================================
    # WIDGET 2: Pie vs Bar — scenario comparison
    # ========================================================================
    lc_h2("ch2-kolowy-vs-slupkowy", "Wykres kołowy vs słupkowy"),

    lc_p("Tabela podaje dokładne liczby, ale różnice między kategoriami szybciej
      widać na wykresie. Dla zmiennych jakościowych używa się zwykle dwóch
      wykresów. Wykres kołowy przedstawia udział kategorii jako kąt wycinka, a ",
      gloss("wykres słupkowy"), " jako długość słupka odmierzaną na wspólnej
      osi. Oba niosą tę samą informację, ale oko nie odczytuje ich równie dobrze.
      Panel zestawia oba wykresy w trzech scenariuszach z fikcyjnymi udziałami
      pięciu produktów."),

    figure_panel(
      label = "Ryc. 2.3",
      title = "Wykres kołowy a słupkowy — trzy scenariusze",
      div(style = "display: flex; gap: 8px; margin-bottom: 15px; flex-wrap: wrap;",
        lc_action("ch2_sc1", "1. Duże różnice", variant = "outline"),
        lc_action("ch2_sc2", "2. Podobne wartości", variant = "outline"),
        lc_action("ch2_sc3", "3. Podobne + złe kolory", variant = "outline")
      ),
      fluidRow(
        column(6,
          h5(style = "text-align: center; color: var(--upwr-reference);", "Wykres kołowy"),
          div(style = "position: relative; width: 100%; height: 320px;",
            tags$canvas(id = "ch2_pie_canvas")
          ),
          uiOutput("ch2_scenario_pie_verdict")
        ),
        column(6,
          h5(style = "text-align: center; color: var(--upwr-reference);", "Wykres słupkowy — te same dane"),
          div(style = "position: relative; width: 100%; height: 320px;",
            tags$canvas(id = "ch2_bar_canvas")
          ),
          uiOutput("ch2_scenario_bar_verdict")
        )
      ),
      div(style = "display: flex; flex-wrap: wrap; gap: 14px; font-size: 12px; color: var(--upwr-reference); margin-top: 8px;",
        id = "ch2_legend",
        uiOutput("ch2_scenario_legend")
      )
    ),

    lc_p("Przy dużych różnicach (45%, 25%, 15%, 10% i 5%) oba wykresy prowadzą
      do tego samego wniosku, choć na kołowym trudniej ocenić, o ile jeden
      wycinek jest większy od drugiego. Przy udziałach 22%, 21%, 20%, 19% i 18%
      wycinki wyglądają na równe i z samego koła nie da się ustalić, który
      produkt prowadzi. Słupki na wspólnej osi pokazują tę kolejność od razu.
      Trzeci scenariusz dokłada zbliżone odcienie jednego koloru: na kołowym
      kategorie zlewają się ze sobą, a na słupkowym nadal decyduje pozycja
      na osi."),

    lc_p("Źródłem tej różnicy jest percepcja. Długości odmierzone od wspólnej
      linii porównujemy znacznie dokładniej niż kąty i pola, więc wykres
      słupkowy jest co najmniej tak samo czytelny jak kołowy, a przy
      zbliżonych udziałach wyraźnie czytelniejszy."),

    inline_callout(
      label = "Zasada",
      "Do porównywania kategorii używaj wykresu słupkowego. Wykres kołowy
       sprawdza się tylko przy kilku kategoriach o wyraźnie różnych udziałach."
    ),

    # ========================================================================
    # WIDGET 4: Color manipulation demo
    # ========================================================================
    lc_h2("ch2-kolory", "Manipulacja kolorami"),

    lc_p("Nawet poprawnie dobrany wykres słupkowy można odczytać na różne
      sposoby, zależnie od kolorów. Kolor nie zmienia wysokości słupków, ale
      decyduje, na który z nich najpierw padnie wzrok. Panel pokazuje liczebności
      kierunków z ankiety w kilku paletach: neutralnej, trzech wyróżniających
      wybrane kategorie i czterech standardowych paletach."),

    figure_panel(
      label = "Ryc. 2.4",
      title = "Jak kolory zmieniają percepcję danych",
      fluidRow(
        column(4,
          selectInput("ch2_color_palette", "Paleta kolorów:",
            choices = c(
              "Neutralna (szara)" = "neutral",
              "Ciepła (podkreśla Informatykę)" = "warm",
              "Zimna (podkreśla Biologię)" = "cool",
              "Stronnicza" = "biased",
              "— Palety standardowe —" = "sep1",
              "Viridis" = "viridis",
              "Set2 (ColorBrewer)" = "set2",
              "Okabe-Ito (colorblind-safe)" = "okabe_ito",
              "Tableau 10" = "tableau"
            ),
            selected = "neutral"
          ),
          lc_action("ch2_color_random", "Losowe kolory", variant = "outline")
        ),
        column(8, zoom_plot_ui("ch2_color_plot", height = "380px"))
      )
    ),

    lc_p("Dane są za każdym razem te same: 60 osób na Informatyce, 51 na Biologii,
      49 na Ekonomii i 40 na Psychologii. Przy palecie neutralnej wszystkie
      słupki mają jednakową wagę i porównujemy tylko ich wysokość. Paleta ciepła
      maluje Informatykę intensywnym burgundem, a pozostałe kierunki jasnym
      beżem, więc wykres zaczyna opowiadać o Informatyce. Paleta zimna robi to
      samo z Biologią. Paleta stronnicza nadaje własne kolory tylko
      najliczniejszej i najmniej licznej kategorii, a resztę zostawia szarą,
      więc wzrok od razu porównuje skrajności. Intensywne barwy przyciągają
      uwagę, a jasne i szare spychają kategorie na margines. Wybór kolorów nie
      jest więc neutralny i powinien wynikać z tego, co wykres ma pokazać."),

    lc_p("Gdy wszystkie kategorie mają być równorzędne, warto sięgnąć po palety
      zaprojektowane z myślą o czytelności. Viridis jest percepcyjnie
      równomierna (równe różnice wartości dają równe różnice w odbiorze
      koloru), pozostaje czytelna w skali szarości i dla osób z zaburzeniami
      widzenia barw; w wielu programach statystycznych jest domyślna.
      Okabe-Ito zaprojektowano specjalnie z myślą o daltonistach, którzy
      stanowią około 8% mężczyzn, i jest częstym wyborem w publikacjach
      naukowych. Palety ColorBrewer (Set2, Set3, Paired i inne) opracowała
      kartografka Cynthia Brewer. Tableau 10 to
      standard w narzędziach analityki biznesowej, z wyrównaną jasnością
      i kontrastem kolorów."),

    # ========================================================================
    # WIDGET 4b: Cross-tabulation
    # ========================================================================
    lc_h2("ch2-krzyzowa", "Tabela krzyżowa — dwie zmienne jednocześnie"),

    lc_p("Dotąd każdą zmienną opisywaliśmy osobno. Często jednak pytamy
      o związek dwóch zmiennych jakościowych, na przykład o to, czy kobiety
      i mężczyźni wybierają te same kierunki. Do tego służy tabela krzyżowa,
      zwana też ", gloss("tabela kontyngencji", "tabelą kontyngencji"), ".
      Jej wiersze to kategorie jednej zmiennej, kolumny to kategorie drugiej,
      a w każdej komórce stoi liczba osób, które mają obie cechy jednocześnie.
      Pod tabelą widać odczyt zaznaczonej komórki; kliknięcie innej komórki
      go zmienia."),

    figure_panel(
      label = "Ryc. 2.5",
      title = "Tabela krzyżowa",
      width_mode = "text",
      lc_toolbar(
        lc_segmented("ch2_cross_row", "Wiersze",
          choices = c("Płeć" = "plec", "Kierunek" = "kierunek",
                      "Grupa krwi" = "grupa_krwi"),
          selected = "plec", exclusive_with = "ch2_cross_col"
        ),
        lc_segmented("ch2_cross_col", "Kolumny",
          choices = c("Płeć" = "plec", "Kierunek" = "kierunek",
                      "Grupa krwi" = "grupa_krwi"),
          selected = "kierunek", exclusive_with = "ch2_cross_row"
        ),
        lc_segmented("ch2_cross_type", "Miara",
          choices = c("Liczebności" = "counts",
                      "% wierszowe" = "row_pct",
                      "% kolumnowe" = "col_pct"),
          selected = "counts"
        )
      ),
      uiOutput("ch2_cross_table"),
      uiOutput("ch2_cross_caption"),
      lc_toolbar(
        lc_segmented("ch2_cross_chart", "Wykres",
          choices = c("Słupkowy" = "bar", "Heatmapa" = "heatmap"),
          selected = "bar"
        )
      ),
      lc_plot("ch2_cross_plot")
    ),

    lc_p("Same liczebności łatwo źle odczytać. W ankiecie jest 109 kobiet
      i 91 mężczyzn, więc kobiet jest więcej na prawie każdym kierunku
      (na Informatyce 33 wobec 27) przede wszystkim dlatego, że jest ich więcej
      w całej próbie. Żeby porównać grupy różnej wielkości, przechodzimy
      na procenty."),

    lc_p("Procenty wierszowe pokazują, jak rozkładają się kierunki w obrębie
      każdej płci: Informatykę studiuje 30.3% kobiet i 29.7% mężczyzn,
      Psychologię 18.3% kobiet i 22.0% mężczyzn. Procenty kolumnowe pokazują
      skład płci na każdym kierunku: kobiety stanowią od 50.0% studentów
      Psychologii do 57.1% studentów Ekonomii. Rozkłady kierunków u kobiet
      i mężczyzn są do siebie podobne, więc w tej próbie wybór kierunku
      niewiele zależy od płci. To, które procenty policzyć, zależy od pytania:
      zmienna, której grupy porównujemy, wyznacza kierunek procentowania."),

    # ========================================================================
    # WIDGET 5: Mode (dominanta)
    # ========================================================================
    lc_h2("ch2-dominanta", "Dominanta (moda)"),

    lc_p("Tabele i wykresy pokazują cały rozkład. Czasem potrzebujemy jednej
      wartości, która powie, co w danych jest typowe; taką wartość nazywamy ",
      gloss("miara tendencji centralnej", "miarą tendencji centralnej"), ".
      Dla zmiennych jakościowych jest nią ", gloss("dominanta"), " (inaczej
      moda): kategoria, która występuje w danych najczęściej. Na wykresie
      słupkowym to po prostu najwyższy słupek. Panel startuje od danych
      z ankiety, a przycisk losuje 200 nowych obserwacji z przypadkowymi
      proporcjami kierunków."),

    figure_panel(
      label = "Ryc. 2.6",
      title = "Dominanta — najczęściej występująca kategoria",
      lc_action("ch2_mode_resample", "Losuj nowe proporcje", icon = "shuffle", variant = "solid"),
      lc_plot("ch2_mode_plot", ratio = "1.8/1", max_height = "350px"),
      uiOutput("ch2_mode_text")
    ),

    lc_p("W ankiecie dominantą kierunku jest Informatyka: 60 z 200 osób, czyli
      30%. Sama dominanta nie mówi jednak, jak wyraźnie kategoria przeważa.
      Tutaj Informatyka wyprzedza Biologię tylko o 9 osób i obejmuje mniej niż
      jedną trzecią próby, dlatego dominantę podajemy razem z jej liczebnością
      albo procentem. Po wylosowaniu nowych proporcji najwyższy słupek może
      przypaść dowolnemu kierunkowi, a gdy dwa słupki są prawie równe, nawet
      niewielka zmiana w danych przenosi dominantę na inną kategorię."),

    lc_p("Dla zmiennych nominalnych dominanta jest jedyną sensowną miarą
      tendencji centralnej. Średniej nie da się obliczyć z nazw kategorii,
      bo „Biologii” nie można dodać do „Ekonomii”, a bez naturalnej kolejności
      nie istnieje też kategoria środkowa. Dla zmiennych porządkowych,
      takich jak zadowolenie, kategorię środkową już można wskazać; tę miarę,
      medianę, poznamy w następnym rozdziale."),

    lc_chapter_next(
      num       = "03",
      title     = "Statystyki położenia",
      lead      = "zmienne jakościowe mamy za sobą — pora na narzędzia dla ilościowych.",
      target_id = "ch-polozenie"
    ),

    # Bottom spacer
    lc_spacer("lg")

  )
)


# --------------------------------------------------------------------------
# Chapter 2 Server
# --------------------------------------------------------------------------

ch2_server <- function(input, output, session) {

  ch2_freq_step <- lc_step_server("ch2_freq", input)$step
  ch2_scenario_idx <- reactiveVal(1)
  ch2_random_colors <- reactiveVal(NULL)
  ch2_mode_data <- reactiveVal(NULL)

  # --- Initialise reactive values that need data ---
  observe({
    if (is.null(ch2_mode_data())) {
      ch2_mode_data(student_data$kierunek)
    }
  })

  # ========================================================================
  # Widget 1: Frequency table step-by-step
  # ========================================================================

  output$ch2_freq_text <- renderUI({
    step <- ch2_freq_step()
    var_name <- input$ch2_freq_var
    is_ord <- (!is.null(var_name) && var_name == "zadowolenie")
    var_label <- if (is_ord) "zadowolenie" else "kierunek"

    if (step == 1) {
      tagList(
          "Pierwsze obserwacje zmiennej ",
          tags$code(var_label, .noWS = "outside"), ". Każdy wiersz to odpowiedź jednego studenta.",
          if (is_ord) " Kategorie mają naturalną kolejność: od „Bardzo niezadowolony” do „Bardzo zadowolony”."
      )
    } else if (step == 2) {
      tagList(
          "Liczymy, ile razy występuje każda kategoria. To są liczebności (częstości bezwzględne).",
          if (is_ord) " Kolejność wierszy wynika z porządku kategorii."
      )
    } else if (step == 3) {
      tagList(
          paste0("Dzielimy każdą liczebność przez liczbę obserwacji (n = ",
                 nrow(student_data), ") i wynik podajemy jako ułamek lub procent."))
    } else if (step == 4) {
      if (is_ord) {
        cum_pct <- cumsum(prop.table(table(student_data$zadowolenie))) * 100
        low_pct <- cum_pct[["Niezadowolony"]]
        tagList(
          "Sumujemy częstości narastająco. Dla zmiennej porządkowej wynik da się
           odczytać: ", lc_fmt(low_pct, 1), "% studentów jest niezadowolonych
           lub bardzo niezadowolonych, a ", lc_fmt(100 - low_pct, 1),
          "% neutralnych lub bardziej zadowolonych.")
      } else {
        cum_pct <- cumsum(prop.table(table(student_data$kierunek))) * 100
        tagList(
          "Sumujemy częstości narastająco. Dla zmiennej nominalnej kolejność
           kategorii jest umowna, więc wynik nic nie znaczy: „",
          lc_fmt(cum_pct[["Informatyka"]], 1), "% studentów studiuje kierunki
           do Informatyki włącznie” to tylko skutek porządku alfabetycznego.")
      }
    }
  })

  output$ch2_freq_table <- renderUI({
    step <- ch2_freq_step()

    var_name <- input$ch2_freq_var
    is_ord <- (!is.null(var_name) && var_name == "zadowolenie")
    x <- if (is_ord) student_data$zadowolenie else student_data$kierunek
    col_label <- if (is_ord) "Zadowolenie" else "Kierunek"

    if (step == 1) {
      raw <- data.frame(V = as.character(head(x, 20)))
      return(lc_table_preview(raw, n = 20, total = length(x),
                              cols = list(lc_col("V", col_label, "text"))))
    }

    counts <- table(x)
    df <- data.frame(
      kat = names(counts),
      n = as.integer(counts)
    )
    df$f <- round(df$n / sum(df$n), 3)
    df$p <- round(df$f * 100, 1)
    df$cn <- cumsum(df$n)
    df$cp <- round(cumsum(df$f) * 100, 1)
    foot <- list(kat = "Razem", n = sum(df$n), f = 1, p = 100, cn = "", cp = "")

    with_class <- function(col, class) { col$class <- class; col }
    cols <- list(
      kat = lc_col("kat", col_label, "row"),
      n   = lc_col("n", "Liczebność", short = "n", desc = "liczebność"),
      f   = lc_col("f", "Częstość względna", digits = 3, short = "f",
                   desc = "częstość względna (n / N)"),
      p   = lc_col("p", "Procent", digits = 1, short = "%",
                   desc = "procent (f · 100)"),
      cn  = lc_col("cn", "Liczebność skumulowana", short = "N skum.",
                   desc = "liczebność skumulowana"),
      cp  = lc_col("cp", "Procent skumulowany", digits = 1, short = "% skum.",
                   desc = "procent skumulowany")
    )

    if (step == 2) {
      return(lc_table(df, list(cols$kat, with_class(cols$n, "is-new")), foot = foot))
    }
    if (step == 3) {
      return(lc_table(df, list(cols$kat, cols$n, with_class(cols$f, "is-new"),
                               with_class(cols$p, "is-new")),
                      foot = foot, scroll = TRUE, label = "Tabela częstości"))
    }
    # Krok 4: dla zmiennej nominalnej skumulowane wartości są wyszarzone.
    cum_class <- if (is_ord) "is-new" else "is-new is-dim"
    lc_table_split(df,
      list(cols$kat, cols$n, cols$f, cols$p,
           with_class(cols$cn, cum_class), with_class(cols$cp, cum_class)),
      groups = list(c("n", "f", "p"), c("cn", "cp")),
      foot = foot, label = "Tabela częstości")
  })


  # ========================================================================
  # Widget 1b: Nominal vs Ordinal comparison
  # ========================================================================

  zoom_plot_server("ch2_ord_nom_plot", reactive({
    df <- data.frame(kierunek = student_data$kierunek)
    lvls <- levels(df$kierunek)
    if (isTRUE(input$ch2_ord_shuffle)) {
      lvls <- sample(lvls)
    }
    df$kierunek <- factor(df$kierunek, levels = lvls)
    ggplot(df, aes(x = kierunek)) +
      geom_bar(fill = type_colors["nominalna"], color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)),
                vjust = -0.5, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Kierunek", y = "Liczebność") +
      theme()
  }))

  zoom_plot_server("ch2_ord_ord_plot", reactive({
    df <- data.frame(zadowolenie = student_data$zadowolenie)
    lvls <- levels(df$zadowolenie)
    if (isTRUE(input$ch2_ord_shuffle)) {
      lvls <- sample(lvls)
    }
    df$zadowolenie <- factor(df$zadowolenie, levels = lvls)
    short_labels <- c(
      "Bardzo niezadowolony" = "B. niezad.",
      "Niezadowolony"        = "Niezad.",
      "Neutralny"            = "Neutr.",
      "Zadowolony"           = "Zad.",
      "Bardzo zadowolony"    = "B. zad."
    )
    ggplot(df, aes(x = zadowolenie)) +
      geom_bar(fill = type_colors["porzadkowa"], color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)),
                vjust = -0.5, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      scale_x_discrete(labels = function(x) short_labels[x]) +
      labs(x = "Zadowolenie", y = "Liczebność") +
      theme()
  }))

  output$ch2_ord_explanation <- renderUI({
    if (isTRUE(input$ch2_ord_shuffle)) {
      lc_feedback(type = "warning",
        tags$strong("Losowa kolejność:"),
        " wykres kierunków znaczy to samo co wcześniej, a wykres zadowolenia
         gubi porządek od „bardzo niezadowolony” do „bardzo zadowolony”."
      )
    } else {
      lc_feedback(type = "info",
        tags$strong("Domyślna kolejność:"),
        " kierunki alfabetycznie (umownie), zadowolenie od „bardzo niezadowolony”
         do „bardzo zadowolony”."
      )
    }
  })

  # ========================================================================
  # Widget 2: Pie vs Bar — scenario comparison (Chart.js)
  # ========================================================================

  observeEvent(input$ch2_sc1, { ch2_scenario_idx(1) })
  observeEvent(input$ch2_sc2, { ch2_scenario_idx(2) })
  observeEvent(input$ch2_sc3, { ch2_scenario_idx(3) })

  ch2_current_scenario <- reactive({
    pie_vs_bar_scenarios[[ch2_scenario_idx()]]
  })

  # Send scenario data to Chart.js via custom message
  observe({
    s <- ch2_current_scenario()
    session$sendCustomMessage("render_scenario", list(
      labels = as.list(s$labels),
      data   = as.list(s$data),
      colors = as.list(s$colors)
    ))
  })

  output$ch2_scenario_pie_verdict <- renderUI({
    s <- ch2_current_scenario()
    badge_style <- if (s$pie_ok) "background: var(--upwr-sage-tint); color: var(--upwr-sage);" else
                                 "background: var(--upwr-accent-tint); color: var(--upwr-accent);"
    badge_text  <- if (s$pie_ok) "OK" else "Problem"
    div(style = "text-align: center; font-size: 13px; color: var(--upwr-reference); margin-top: 6px;",
      tags$span(style = paste0("display: inline-block; font-size: 11px; padding: 2px 8px;
                                 border-radius: 6px; font-weight: 500; margin-right: 4px; ",
                                badge_style), badge_text),
      s$pie_verdict
    )
  })

  output$ch2_scenario_bar_verdict <- renderUI({
    s <- ch2_current_scenario()
    div(style = "text-align: center; font-size: 13px; color: var(--upwr-reference); margin-top: 6px;",
      tags$span(style = "display: inline-block; font-size: 11px; padding: 2px 8px;
                         border-radius: 6px; font-weight: 500; margin-right: 4px;
                         background: var(--upwr-sage-tint); color: var(--upwr-sage);", "OK"),
      s$bar_verdict
    )
  })

  output$ch2_scenario_legend <- renderUI({
    s <- ch2_current_scenario()
    legend_items <- mapply(function(label, color, value) {
      tags$span(style = "display: flex; align-items: center; gap: 4px;",
        tags$span(style = paste0("width: 10px; height: 10px; border-radius: 2px;
                                   flex-shrink: 0; background: ", color, ";")),
        paste0(label, " ", value, "%")
      )
    }, s$labels, s$colors, s$data, SIMPLIFY = FALSE)
    tagList(legend_items)
  })

  # ========================================================================
  # Widget 4: Color manipulation demo
  # ========================================================================

  observeEvent(input$ch2_color_random, {
    # Paleta o gwarantowanym kontraście na białym tle
    safe_colors <- c(
      "#e6194B", "#3cb44b", "#4363d8", "#f58231", "#911eb4",
      "#42d4f4", "#f032e6", "#bfef45", "#fabed4", "#469990",
      "#dcbeff", "#9A6324", "#800000", "#aaffc3", "#808000",
      "#000075", "#a9a9a9", "#e6beff", "#ffd8b1", "#fffac8"
    )
    ch2_random_colors(sample(safe_colors, 4))
  })

  # Reset random colors when palette selector changes
  observeEvent(input$ch2_color_palette, {
    ch2_random_colors(NULL)
  })

  zoom_plot_server("ch2_color_plot", reactive({
    df <- data.frame(kierunek = student_data$kierunek)
    df_counts <- as.data.frame(table(df$kierunek))
    names(df_counts) <- c("Kierunek", "n")

    levels_order <- levels(student_data$kierunek)
    if (is.null(levels_order)) levels_order <- unique(as.character(student_data$kierunek))

    rand_cols <- ch2_random_colors()
    palette_choice <- input$ch2_color_palette

    if (!is.null(rand_cols)) {
      fill_colors <- setNames(rand_cols, levels_order)
      subtitle <- "Losowa paleta kolorów"
    } else if (palette_choice == "neutral") {
      fill_colors <- setNames(rep(upwr_reference, 4), levels_order)
      subtitle <- "Neutralna — wszystkie kategorie równe"
    } else if (palette_choice == "warm") {
      fill_colors <- setNames(
        ifelse(levels_order == "Informatyka", upwr_accent, upwr_rule),
        levels_order
      )
      subtitle <- "Ciepła paleta — uwaga przyciągana do Informatyki"
    } else if (palette_choice == "cool") {
      fill_colors <- setNames(
        ifelse(levels_order == "Biologia", upwr_cat["indygo"], upwr_rule),
        levels_order
      )
      subtitle <- "Zimna paleta — uwaga przyciągana do Biologii"
    } else if (palette_choice == "biased") {
      biggest <- df_counts$Kierunek[which.max(df_counts$n)]
      smallest <- df_counts$Kierunek[which.min(df_counts$n)]
      cols <- setNames(rep(upwr_reference, 4), levels_order)
      cols[as.character(biggest)]  <- upwr_accent
      cols[as.character(smallest)] <- upwr_secondary
      fill_colors <- cols
      subtitle <- paste0("Stronnicza — wyróżnione skrajności: ", biggest,
                         " i ", smallest)
    } else if (palette_choice == "viridis") {
      fill_colors <- setNames(
        c("#440154", "#31688e", "#35b779", "#fde725")[1:length(levels_order)],
        levels_order)
      subtitle <- "Viridis — percepcyjnie równomierna, bezpieczna dla daltonistów"
    } else if (palette_choice == "set2") {
      fill_colors <- setNames(
        c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")[1:length(levels_order)],
        levels_order)
      subtitle <- "Set2 (ColorBrewer) — popularny domyślny wybór"
    } else if (palette_choice == "okabe_ito") {
      fill_colors <- setNames(
        c("#E69F00", "#56B4E9", "#009E73", "#CC79A7")[1:length(levels_order)],
        levels_order)
      subtitle <- "Okabe-Ito — zaprojektowana specjalnie dla daltonistów"
    } else if (palette_choice == "tableau") {
      fill_colors <- setNames(
        c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2")[1:length(levels_order)],
        levels_order)
      subtitle <- "Tableau 10 — standard w wizualizacji danych"
    } else {
      fill_colors <- setNames(rep(upwr_reference, length(levels_order)), levels_order)
      subtitle <- ""
    }

    ggplot(df_counts, aes(x = Kierunek, y = n, fill = Kierunek)) +
      geom_col(color = "white", width = 0.7) +
      geom_text(aes(label = n), vjust = -0.5, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      scale_fill_manual(values = fill_colors, guide = "none") +
      labs(x = "Kierunek", y = "Liczebność")
  }))


  # ========================================================================
  # Widget 4b: Cross-tabulation

  cross_labels <- c("plec" = "Płeć", "kierunek" = "Kierunek", "grupa_krwi" = "Grupa krwi")
  cross_short <- list(kierunek = c("Biologia" = "Biol.", "Ekonomia" = "Ekon.",
                                   "Informatyka" = "Inf.", "Psychologia" = "Psych."))
  cross_measure <- c("counts" = "n", "row_pct" = "row", "col_pct" = "col")
  cross_target <- reactiveVal(c(1L, 1L))
  observeEvent(input$ch2_cross_cell, cross_target(as.integer(input$ch2_cross_cell)))

  cross_tab <- reactive({
    row_var <- input$ch2_cross_row
    col_var <- input$ch2_cross_col
    req(row_var, col_var, row_var != col_var)
    table(student_data[[row_var]], student_data[[col_var]])
  })

  cross_cell <- reactive({
    tbl <- cross_tab()
    target <- cross_target()
    if (target[1] > nrow(tbl) || target[2] > ncol(tbl)) target <- c(1L, 1L)
    target
  })

  output$ch2_cross_table <- renderUI({
    tbl <- cross_tab()
    col_levels <- colnames(tbl)
    short <- cross_short[[input$ch2_cross_col]]
    lc_crosstab(tbl,
      measure = cross_measure[[input$ch2_cross_type]],
      target = cross_cell(),
      row_name = cross_labels[[input$ch2_cross_row]],
      col_name = cross_labels[[input$ch2_cross_col]],
      short_labels = if (!is.null(short)) unname(short[col_levels]),
      input_id = "ch2_cross_cell"
    )
  })

  output$ch2_cross_caption <- renderUI({
    tbl <- cross_tab()
    target <- cross_cell()
    i <- target[1]
    j <- target[2]
    count <- tbl[i, j]
    row_name <- rownames(tbl)[i]
    col_name <- colnames(tbl)[j]
    q <- function(x) paste0("„", x, "”")
    text <- switch(input$ch2_cross_type,
      counts = tagList(paste0(q(row_name), " i ", q(col_name), ": "), tags$b(count),
        paste0(" osób, czyli ", lc_fmt(count / sum(tbl) * 100, 1), "% całej próby.")),
      row_pct = tagList(paste0("W wierszu ", q(row_name), " "),
        tags$b(paste0(lc_fmt(count / sum(tbl[i, ]) * 100, 1), "%")),
        paste0(" osób to ", q(col_name), " (", count, " z ", sum(tbl[i, ]), ").")),
      col_pct = tagList(paste0("W kolumnie ", q(col_name), " "),
        tags$b(paste0(lc_fmt(count / sum(tbl[, j]) * 100, 1), "%")),
        paste0(" osób to ", q(row_name), " (", count, " z ", sum(tbl[, j]), ")."))
    )
    lc_caption(tone = "info", text)
  })

  zoom_plot_server("ch2_cross_plot", reactive({
    row_var <- input$ch2_cross_row
    col_var <- input$ch2_cross_col
    chart_type <- input$ch2_cross_chart
    req(row_var, col_var, row_var != col_var)

    df <- data.frame(
      row = student_data[[row_var]],
      col = student_data[[col_var]]
    )

    row_label <- c("plec" = "Płeć", "kierunek" = "Kierunek", "grupa_krwi" = "Grupa krwi")
    col_label <- row_label

    if (!is.null(chart_type) && chart_type == "heatmap") {
      # Heatmap (geom_tile)
      tbl <- table(df$row, df$col)
      if (input$ch2_cross_type == "row_pct") {
        tbl <- prop.table(tbl, margin = 1) * 100
        fill_label <- "% wierszowy"
        fmt <- function(x) paste0(round(x, 1), "%")
      } else if (input$ch2_cross_type == "col_pct") {
        tbl <- prop.table(tbl, margin = 2) * 100
        fill_label <- "% kolumnowy"
        fmt <- function(x) paste0(round(x, 1), "%")
      } else {
        fill_label <- "Liczebność"
        fmt <- function(x) as.character(x)
      }
      heat_df <- as.data.frame(as.table(tbl))
      names(heat_df) <- c("Wiersz", "Kolumna", "Wartosc")

      ggplot(heat_df, aes(x = Kolumna, y = Wiersz, fill = Wartosc)) +
        geom_tile(color = "white", linewidth = 1.5) +
        scale_fill_upwr_seq(variant = "burgundy", name = fill_label) +
        labs(x = col_label[col_var], y = row_label[row_var]) +
                theme(
          panel.grid = element_blank(),
          axis.text = element_text(size = 12)
        )
    } else {
      # Grouped bar chart
      ggplot(df, aes(x = row, fill = col)) +
        geom_bar(position = "dodge", alpha = 0.85, color = "white") +
        scale_fill_upwr() +
        labs(x = row_label[row_var], y = "Liczebność", fill = col_label[col_var]) +
                theme(legend.position = "top")
    }
  }))

  # Widget 5: Mode (dominanta)
  # ========================================================================

  observeEvent(input$ch2_mode_resample, {
    probs <- runif(4)
    probs <- probs / sum(probs)
    new_data <- sample(
      c("Informatyka", "Biologia", "Psychologia", "Ekonomia"),
      200, replace = TRUE, prob = probs
    )
    ch2_mode_data(factor(new_data,
      levels = c("Informatyka", "Biologia", "Psychologia", "Ekonomia")))
  })

  zoom_plot_server("ch2_mode_plot", reactive({
    req(ch2_mode_data())
    x <- ch2_mode_data()
    df_counts <- as.data.frame(table(x))
    names(df_counts) <- c("Kierunek", "n")
    mode_cat <- df_counts$Kierunek[which.max(df_counts$n)]

    df_counts$is_mode <- ifelse(df_counts$Kierunek == mode_cat,
                                "Dominanta", "Inne")

    ggplot(df_counts, aes(x = Kierunek, y = n, fill = is_mode)) +
      geom_col(color = "white", width = 0.7, alpha = 0.9) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      scale_fill_manual(
        values = c("Dominanta" = type_colors["nominalna"], "Inne" = upwr_rule),
        guide = "none"
      ) +
      labs(x = "Kierunek", y = "Liczebność")
  }))

  output$ch2_mode_text <- renderUI({
    req(ch2_mode_data())
    x <- ch2_mode_data()
    counts <- table(x)
    mode_cat <- names(counts)[which.max(counts)]
    mode_n   <- max(counts)
    total_n  <- sum(counts)
    mode_pct <- round(mode_n / total_n * 100, 1)

    lc_feedback(type = "info",
      tags$b("Dominanta:"), " ", mode_cat,
      tags$br(),
      paste0("Występuje ", mode_n, " razy (", mode_pct, "% z ", total_n,
             " obserwacji).")
    )
  })

}
