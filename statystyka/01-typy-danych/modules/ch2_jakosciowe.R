# ============================================================================
# CHAPTER 2: Zmienne jakościowe
# ============================================================================

cross_choices <- c("Płeć" = "plec", "Kierunek" = "kierunek", "Grupa krwi" = "grupa_krwi")

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
      czyli liczbę ", gloss("obserwacja", "obserwacji"), " w tej kategorii, oraz ",
      gloss("częstość względna", "częstość względną"), " \\(f_i\\), czyli udział
      kategorii w całej ", gloss("próba", "próbie"), " liczącej \\(n\\) obserwacji. Częstość względną
      podajemy jako ułamek albo jako procent."),

    lc_formula_box(withMathJax(
      "$$f_i = \\frac{n_i}{n}, \\qquad p_i = f_i \\cdot 100\\%$$"
    )),

    lc_p("Tabelę uzupełnia ", gloss("częstość skumulowana"), ": suma częstości
      od pierwszej kategorii do bieżącej, podawana zwykle w procentach. Panel poniżej buduje tabelę krok
      po kroku dla dwóch ", gloss("zmienna", "zmiennych"), " z ankiety 200 studentów: kierunku studiów
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

      lc_toolbar(
        checkboxInput("ch2_ord_shuffle", "Losowa kolejność kategorii", value = FALSE)
      ),
      lc_plots(
        tags$div(
          tags$h4("Nominalna: kierunek studiów"),
          lc_plot("ch2_ord_nom_plot", max_height = "300px")
        ),
        tags$div(
          tags$h4("Porządkowa: zadowolenie"),
          lc_plot("ch2_ord_ord_plot", max_height = "300px")
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
      lc_toolbar(
        lc_segmented("ch2_scenario", "Scenariusz",
          choices = c("Duże różnice" = "1", "Podobne" = "2", "Złe kolory" = "3"))
      ),
      lc_plots(pair = TRUE,
        tags$div(
          tags$h4("Wykres kołowy"),
          div(style = "position: relative; width: 100%; height: 320px;",
            tags$canvas(id = "ch2_pie_canvas")
          ),
          uiOutput("ch2_scenario_pie_verdict")
        ),
        tags$div(
          tags$h4("Wykres słupkowy, te same dane"),
          div(style = "position: relative; width: 100%; height: 320px;",
            tags$canvas(id = "ch2_bar_canvas"),
            uiOutput("ch2_bar_cover")
          ),
          uiOutput("ch2_scenario_bar_verdict")
        )
      ),
      tags$div(style = "margin-top: 1.6em; font-size: 1.15em;",
        lc_readouts(uiOutput("ch2_scenario_legend"))
      ),
      tags$h4("Ułóżcie produkty od najmniejszego udziału do największego"),
      uiOutput("ch2_order_widget"),
      uiOutput("ch2_order_result")
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

    lc_note("Zasada", rule = TRUE,
      "Do porównywania kategorii używaj wykresu słupkowego. Wykres kołowy
       sprawdza się tylko przy kilku kategoriach o wyraźnie różnych udziałach."
    ),

    # ========================================================================
    # WIDGET 4: Color manipulation demo
    # ========================================================================
    lc_h2("ch2-kolory", "Manipulacja kolorami"),

    lc_p("Nawet poprawnie dobrany wykres można odczytać na różne sposoby,
      zależnie od kolorów. Kolor nie zmienia danych, ale decyduje, na co
      najpierw padnie wzrok. Panel pokazuje te same dane w kilku paletach na
      dwóch wykresach: słupkowym dla kategorii (kierunki studiów) i mapie
      gęstości dla danych ciągłych (wzrost i waga). Dobra paleta musi
      sprawdzić się w obu rolach, a to dwa różne zadania."),

    figure_panel(
      label = "Ryc. 2.4",
      title = "Jak kolory zmieniają percepcję danych",
      lc_toolbar(
        selectInput("ch2_color_palette", "Paleta kolorów",
          choices = list(
            "Kolor jako komunikat" = c(
              "Neutralna (szara)" = "neutral",
              "Wyróżnienie jednej kategorii" = "highlight"
            ),
            "Palety standardowe" = c(
              "Viridis" = "viridis",
              "ColorBrewer (Set2 / YlGnBu)" = "brewer",
              "Okabe-Ito (dla daltonistów)" = "okabe_ito",
              "jUPWR (jamovi)" = "jupwr"
            )
          ),
          selected = "neutral"
        ),
        lc_action("ch2_color_random", "Losowe kolory", icon = "shuffle", variant = "outline")
      ),
      lc_plots(pair = TRUE,
        tags$div(
          tags$h4("Kategorie: kierunek studiów"),
          lc_plot("ch2_color_plot")
        ),
        tags$div(
          tags$h4("Dane ciągłe: wzrost × waga"),
          lc_plot("ch2_color_heat")
        )
      ),
      uiOutput("ch2_color_caption")
    ),

    lc_p("Dane są za każdym razem te same: 60 osób na Informatyce, 51 na Biologii,
      49 na Ekonomii i 40 na Psychologii. Przy palecie neutralnej wszystkie
      słupki mają jednakową wagę i porównujemy tylko ich wysokość. Wyróżnienie
      maluje Informatykę burgundem, a pozostałe kierunki jasnym beżem, więc
      wykres zaczyna opowiadać o Informatyce. Na mapie gęstości ten sam zabieg
      wyciąga na pierwszy plan tylko najgęstszy obszar, a resztę rozkładu
      spycha w tło. Wybór kolorów nie jest więc neutralny i powinien wynikać
      z tego, co wykres ma pokazać."),

    lc_p("Kategorie i dane ciągłe potrzebują innych palet. Kategorie wymagają
      kolorów wyraźnie różnych, ale bez porządku, bo Biologia nie jest
      „większa” od Ekonomii. Dane ciągłe wymagają skali, w której jasność
      rośnie razem z wartością, żeby od razu było widać, gdzie jest więcej.
      Przycisk losowych kolorów psuje obie zasady: słupki dostają barwy
      o przypadkowej wadze, a mapa skalę, w której nie da się odczytać,
      co jest wysoko, a co nisko."),

    lc_p("Dlatego standardowe palety mają zwykle dwie wersje. Viridis jest
      percepcyjnie równomierna (równe różnice wartości dają równe różnice
      w odbiorze koloru), pozostaje czytelna w skali szarości i dla osób
      z zaburzeniami widzenia barw; w wielu programach statystycznych jest
      domyślna. Palety ColorBrewer opracowała kartografka Cynthia Brewer:
      Set2 to zestaw dla kategorii, a YlGnBu skala od żółci do granatu dla
      wartości ciągłych. Okabe-Ito zaprojektowano z myślą o daltonistach,
      którzy stanowią około 8% mężczyzn; ma tylko wersję dla kategorii, więc
      mapę rysujemy odcieniami jej niebieskiego. Paleta jUPWR pochodzi
      z jamovi, którego używamy na zajęciach: kategorie rozdziela jasnością
      i osią żółć–błękit zamiast pary czerwień–zieleń, a dla wartości
      ciągłych ma ciepłą skalę od jasnego różu do ciemnego burgundu."),

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
        lc_group("Wiersze × kolumny",
          tags$div(class = "lc-xt-pair",
            lc_select("ch2_cross_row", cross_choices, selected = "plec",
                      aria_label = "Zmienna w wierszach"),
            tags$span(class = "lc-xt-times", `aria-hidden` = "true", "×"),
            lc_select("ch2_cross_col", cross_choices, selected = "kierunek",
                      aria_label = "Zmienna w kolumnach"),
            lc_action("ch2_cross_swap", icon = "shuffle", variant = "ghost",
                      aria_label = "Zamień wiersze z kolumnami")
          )
        ),
        tags$div(class = "lc-push",
          lc_segmented("ch2_cross_type", NULL,
            choices = c("Liczebności" = "counts",
                        "% wierszowe" = "row_pct",
                        "% kolumnowe" = "col_pct"),
            selected = "counts"
          )
        )
      ),
      uiOutput("ch2_cross_table"),
      uiOutput("ch2_cross_caption"),
      tags$div(class = "lc-xt-bar is-plain",
        uiOutput("ch2_cross_legend", inline = TRUE),
        lc_segmented("ch2_cross_chart", NULL,
          choices = c("Słupki" = "bar", "Mapa ciepła" = "heatmap"),
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
      tendencji centralnej. ", gloss("średnia", "Średniej"), " nie da się obliczyć z nazw kategorii,
      bo „Biologii” nie można dodać do „Ekonomii”, a bez naturalnej kolejności
      nie istnieje też kategoria środkowa. Dla zmiennych porządkowych,
      takich jak zadowolenie, kategorię środkową już można wskazać; tę miarę,
      ", gloss("mediana", "medianę"), ", poznamy w następnym rozdziale."),

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
      lc_status(
        lc_verdict(tags$strong("Losowa kolejność:"), type = "warning"),
        " wykres kierunków znaczy to samo co wcześniej, a wykres zadowolenia
         gubi porządek od „bardzo niezadowolony” do „bardzo zadowolony”."
      )
    } else {
      lc_status(
        tags$strong("Domyślna kolejność:"),
        " kierunki alfabetycznie (umownie), zadowolenie od „bardzo niezadowolony”
         do „bardzo zadowolony”."
      )
    }
  })

  # ========================================================================
  # Widget 2: Pie vs Bar — scenario comparison (Chart.js)
  # ========================================================================

  # Zmiana scenariusza zaczyna rundę od nowa: nowy identyfikator widgetu
  # ustawień (bez starych przeciągnięć) i schowane wartości.
  ch2_round <- reactiveVal(0)
  ch2_revealed <- reactiveVal(FALSE)

  observeEvent(input$ch2_scenario, {
    ch2_scenario_idx(as.integer(input$ch2_scenario))
    ch2_round(ch2_round() + 1)
    ch2_revealed(FALSE)
  })

  observeEvent(input$ch2_reveal, ch2_revealed(TRUE))

  ch2_current_scenario <- reactive({
    pie_vs_bar_scenarios[[ch2_scenario_idx()]]
  })

  ch2_widget_id <- reactive(paste0("ch2_order_", ch2_round()))

  # Send scenario data to Chart.js via custom message
  observe({
    s <- ch2_current_scenario()
    session$sendCustomMessage("render_scenario", list(
      labels = as.list(s$labels),
      data   = as.list(s$data),
      colors = as.list(s$colors),
      reveal = ch2_revealed()
    ))
  })

  output$ch2_scenario_pie_verdict <- renderUI({
    if (!ch2_revealed()) return(NULL)
    s <- ch2_current_scenario()
    lc_caption(lc_verdict(b_(if (s$pie_ok) "OK." else "Problem."),
                          type = if (s$pie_ok) "ok" else "danger"),
               " ", s$pie_verdict)
  })

  output$ch2_scenario_bar_verdict <- renderUI({
    if (!ch2_revealed()) return(NULL)
    s <- ch2_current_scenario()
    lc_caption(lc_verdict(b_("OK."), type = "ok"), " ", s$bar_verdict)
  })

  output$ch2_bar_cover <- renderUI({
    if (ch2_revealed()) return(NULL)
    tags$div(
      style = "position: absolute; inset: 0; display: flex; align-items: center;
        justify-content: center; padding: 1em; text-align: center;
        background: var(--upwr-surface); color: var(--upwr-ink-soft);",
      "Słupki i wartości odsłonimy po ułożeniu produktów"
    )
  })

  output$ch2_scenario_legend <- renderUI({
    s <- ch2_current_scenario()
    # Odczyty w kolorach wycinków pełnią rolę legendy obu wykresów.
    # Przed odsłonięciem zamiast wartości pokazują kreskę.
    tagList(mapply(function(label, color, value) {
      shown <- if (ch2_revealed()) paste0(value, "%") else "—"
      lc_readout(label, shown, color = color, swatch = TRUE)
    }, s$labels, s$colors, s$data, SIMPLIFY = FALSE))
  })

  # Karty w puli nie są posortowane (kolejność stała dla każdego scenariusza).
  ch2_pool_order <- c(4, 1, 5, 2, 3)
  ch2_position_zones <- c(
    pos1 = "1. najmniejszy", pos2 = "2.", pos3 = "3.",
    pos4 = "4.", pos5 = "5. największy"
  )

  output$ch2_order_widget <- renderUI({
    s <- ch2_current_scenario()
    ids <- LETTERS[seq_along(s$labels)]
    lc_drop_match(
      input_id = ch2_widget_id(),
      items = data.frame(
        id   = ids[ch2_pool_order],
        text = s$labels[ch2_pool_order],
        stringsAsFactors = FALSE
      ),
      zones = ch2_position_zones,
      colors = rep(upwr_reference, length(ch2_position_zones)),
      hint = "Przeciągnijcie produkty na pozycje od najmniejszego udziału do największego.",
      actions = lc_action("ch2_reveal", "Odsłoń odpowiedź", variant = "solid")
    )
  })

  # Porównanie ułożenia z prawidłową kolejnością. Ułożenie zamrażamy w chwili
  # odsłonięcia, żeby kolejne przeciągnięcia nie zmieniały wyniku.
  output$ch2_order_result <- renderUI({
    if (!ch2_revealed()) return(NULL)
    s <- ch2_current_scenario()
    assignment <- isolate(input[[ch2_widget_id()]])
    ids <- LETTERS[seq_along(s$labels)]
    correct_idx <- order(s$data)
    correct_ids <- ids[correct_idx]
    chosen <- vapply(names(ch2_position_zones), function(pos) {
      value <- assignment[[pos]]
      if (is.null(value)) NA_character_ else as.character(value)
    }, character(1))
    hits <- sum(chosen == correct_ids, na.rm = TRUE)
    lc_caption(
      paste0("Trafionych pozycji: ", hits, " z 5. Od najmniejszego udziału: ",
             paste0(s$labels[correct_idx], " (", s$data[correct_idx], "%)",
                    collapse = ", "), ".")
    )
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

  # Każda paleta ma wersję dla kategorii (cat, 4 kolory w kolejności
  # poziomów kierunku) i dla danych ciągłych (seq, kotwice od niskich
  # do wysokich wartości; opcjonalnie seq_at — położenie kotwic 0..1).
  ch2_palette <- reactive({
    levels_order <- levels(student_data$kierunek)
    rand_cols <- ch2_random_colors()

    if (!is.null(rand_cols)) {
      return(list(
        cat = rand_cols, seq = rand_cols[1:3],
        note = "Losowe kolory: słupki dostają przypadkowe wagi, a mapa skalę
          bez porządku jasności."
      ))
    }

    switch(input$ch2_color_palette,
      neutral = list(
        cat = rep(upwr_reference, 4),
        seq = c("#f6f3ee", upwr_rule, upwr_reference, "#3d3833"),
        note = "Neutralna: wszystkie kategorie równe, gęstość tylko jasnością szarości."
      ),
      highlight = list(
        cat = ifelse(levels_order == "Informatyka", upwr_accent, upwr_rule),
        seq = c("#f6f3ee", upwr_rule, upwr_rule, upwr_accent),
        seq_at = c(0, 0.5, 0.8, 1),
        note = "Wyróżnienie: uwaga przyciągana do Informatyki i do najgęstszego
          obszaru mapy."
      ),
      viridis = list(
        cat = c("#440154", "#31688e", "#35b779", "#fde725"),
        seq = c("#440154", "#3b528b", "#21918c", "#5ec962", "#fde725"),
        note = "Viridis: percepcyjnie równomierna, czytelna w skali szarości
          i dla daltonistów."
      ),
      brewer = list(
        cat = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"),
        seq = c("#ffffd9", "#c7e9b4", "#41b6c4", "#225ea8", "#081d58"),
        note = "ColorBrewer: Set2 dla kategorii, YlGnBu dla wartości ciągłych."
      ),
      okabe_ito = list(
        cat = c("#E69F00", "#56B4E9", "#009E73", "#CC79A7"),
        seq = c("#eef5fa", "#0072B2"),
        note = "Okabe-Ito: paleta tylko dla kategorii; mapa w odcieniach jej
          niebieskiego."
      ),
      # Motyw „jUPWR jasny” z jamovi-upwr (jmvcore/R/themes.R): main dla
      # kategorii, ciepla (odwrócona: od jasnych do ciemnych) dla gęstości.
      jupwr = list(
        cat = c("#9c3b4a", "#d99a5b", "#3f6f9e", "#7a9b8e"),
        seq = rev(c("#6e2632", "#9c3b4a", "#c85264", "#d99a5b", "#eec79a", "#fbe3e5")),
        note = "jUPWR: paleta wykresów z jamovi używanego na zajęciach."
      )
    )
  })

  output$ch2_color_caption <- renderUI({
    lc_caption(ch2_palette()$note)
  })

  zoom_plot_server("ch2_color_plot", reactive({
    df_counts <- as.data.frame(table(student_data$kierunek))
    names(df_counts) <- c("Kierunek", "n")
    fill_colors <- setNames(ch2_palette()$cat, levels(student_data$kierunek))

    ggplot(df_counts, aes(x = Kierunek, y = n, fill = Kierunek)) +
      geom_col(color = "white", width = 0.7) +
      geom_text(aes(label = n), vjust = -0.5, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      scale_fill_manual(values = fill_colors, guide = "none") +
      scale_x_discrete(labels = c("Biologia" = "Biol.", "Ekonomia" = "Ekon.",
                                  "Informatyka" = "Inf.", "Psychologia" = "Psych.")) +
      labs(x = "Kierunek", y = "Liczebność")
  }))

  zoom_plot_server("ch2_color_heat", reactive({
    pal <- ch2_palette()

    ggplot(student_data, aes(x = wzrost, y = waga)) +
      stat_density_2d(aes(fill = after_stat(density)), geom = "raster",
                      contour = FALSE, n = 120) +
      scale_fill_gradientn(
        colours = pal$seq, values = pal$seq_at,
        breaks = function(lim) lim, labels = c("mało", "dużo"),
        name = "Liczba osób",
        guide = guide_colourbar(title.position = "top", title.hjust = 0.5,
                                barwidth = unit(9, "lines"), barheight = unit(0.6, "lines"))
      ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = "Wzrost (cm)", y = "Waga (kg)") +
      theme(legend.position = "bottom")
  }))


  # ========================================================================
  # Widget 4b: Cross-tabulation

  cross_labels <- c("plec" = "Płeć", "kierunek" = "Kierunek", "grupa_krwi" = "Grupa krwi")
  cross_short <- list(kierunek = c("Biologia" = "Biol.", "Ekonomia" = "Ekon.",
                                   "Informatyka" = "Inf.", "Psychologia" = "Psych."))
  cross_measure <- c("counts" = "n", "row_pct" = "row", "col_pct" = "col")
  cross_target <- reactiveVal(c(1L, 1L))
  cross_prev <- reactiveVal(c(row = "plec", col = "kierunek"))

  # Ta sama zmienna w wierszach i kolumnach: druga lista dostaje poprzednią
  # wartość pierwszej (zmiana wybranej zmiennej działa jak zamiana).
  observeEvent(list(input$ch2_cross_row, input$ch2_cross_col), {
    r <- input$ch2_cross_row
    k <- input$ch2_cross_col
    req(r, k)
    p <- cross_prev()
    if (identical(r, k)) {
      if (!identical(r, p[["row"]])) {
        k <- p[["row"]]
        updateSelectInput(session, "ch2_cross_col", selected = k)
      } else {
        r <- p[["col"]]
        updateSelectInput(session, "ch2_cross_row", selected = r)
      }
    }
    cross_prev(c(row = r, col = k))
  })

  observeEvent(input$ch2_cross_swap, {
    updateSelectInput(session, "ch2_cross_row", selected = input$ch2_cross_col)
  })

  output$ch2_cross_legend <- renderUI({
    tbl <- cross_tab()
    if (identical(input$ch2_cross_chart, "heatmap")) return(NULL)
    lc_legend(cross_labels[[input$ch2_cross_col]], colnames(tbl), upwr_cat_n(ncol(tbl)))
  })
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
      input_id = "ch2_cross_cell",
      lead = FALSE
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
        theme(legend.position = "none")
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

    lc_status(
      tags$b("Dominanta:"),
      " ",
      mode_cat,
      tags$br(),
      paste0("Występuje ", mode_n, " razy (", mode_pct, "% z ", total_n,
             " obserwacji).")
    )
  })

}
