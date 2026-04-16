# ============================================================================
# CHAPTER 6: Ściąga
# ============================================================================

ch6_ui <- tabPanel("6. \u015aci\u0105ga",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Podsumowanie -- narz\u0119dzia statystyki opisowej"),

    div(class = "narrative",
      p("Poni\u017cej kompletna \u015bci\u0105ga ze wszystkimi poj\u0119ciami i narz\u0119dziami
        om\u00f3wionymi w trakcie wyk\u0142adu. Warto j\u0105 mie\u0107 pod r\u0119k\u0105 podczas analiz.")
    ),

    # --- Cheat sheet 1: Tools by variable type ---
    div(class = "section-title", "Narz\u0119dzia wg typu zmiennej"),

    div(class = "widget-block",
      tableOutput("ch6_ref_table")
    ),

    div(class = "callout-danger",
      tags$strong("Najcz\u0119stszy b\u0142\u0105d:"),
      " Obliczanie \u015bredniej z danych nominalnych lub porz\u0105dkowych
      (np. \u015brednia z kod\u00f3w kierunk\u00f3w). Wynik b\u0119dzie liczb\u0105, ale
      nie b\u0119dzie mia\u0142 \u017cadnego sensu!"
    ),

    # --- Cheat sheet 2: Measures summary ---
    div(class = "section-title", "Miary statystyczne -- kiedy co stosowa\u0107"),

    div(class = "widget-block",
      h4("Miary po\u0142o\u017cenia (rozdzia\u0142 3)"),
      tableOutput("ch6_location_table"),
      hr(),
      h4("Miary rozrzutu (rozdzia\u0142 4)"),
      tableOutput("ch6_spread_table"),
      hr(),
      h4("Miary kszta\u0142tu (rozdzia\u0142 5)"),
      tableOutput("ch6_shape_table")
    ),

    # --- Cheat sheet 3: Visualization guide ---
    div(class = "section-title", "Przewodnik po wykresach"),

    div(class = "widget-block",
      tableOutput("ch6_plots_table")
    ),

    # --- Cheat sheet 4: Common mistakes ---
    div(class = "section-title", "Typowe b\u0142\u0119dy"),

    div(class = "widget-block",
      tableOutput("ch6_mistakes_table")
    ),

    # --- Formulas ---
    div(class = "section-title", "Wzory"),

    div(class = "widget-block",
      withMathJax(
        h4("Miary po\u0142o\u017cenia"),
        helpText("$$\\bar{x} = \\frac{1}{n} \\sum_{i=1}^{n} x_i$$"),
        p("Mediana: warto\u015b\u0107 \u015brodkowa po posortowaniu danych"),
        hr(),
        h4("Miary rozrzutu"),
        helpText("$$s^2 = \\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2$$"),
        helpText("$$s = \\sqrt{s^2}$$"),
        helpText("$$CV = \\frac{s}{\\bar{x}} \\cdot 100\\%$$"),
        helpText("$$IQR = Q_3 - Q_1$$"),
        hr(),
        h4("Miary kszta\u0142tu"),
        helpText("$$\\text{Sko\u015bno\u015b\u0107} = \\frac{n}{(n-1)(n-2)} \\sum \\left(\\frac{x_i - \\bar{x}}{s}\\right)^3$$"),
        helpText("$$\\text{Kurtoza (excess)} = \\frac{n(n+1)}{(n-1)(n-2)(n-3)} \\sum \\left(\\frac{x_i - \\bar{x}}{s}\\right)^4 - \\frac{3(n-1)^2}{(n-2)(n-3)}$$")
      )
    ),

    # --- Przejscie do quizu ---
    div(class = "chapter-transition",
      p("Czas sprawdzi\u0107 si\u0119! Przejd\u017a do quizu z typ\u00f3w zmiennych."),
      actionButton("ch6_to_ch7", "Dalej: Quiz \u2192", class = "btn-primary")
    ),

    br(), br()
  ))
) # end ch6 tabPanel

# --------------------------------------------------------------------------
# Chapter 6 Server
# --------------------------------------------------------------------------

ch6_server <- function(input, output, session) {

  output$ch6_ref_table <- renderTable({
    df <- data.frame(
      a = c("Jako\u015bciowa nominalna", "Jako\u015bciowa porz\u0105dkowa",
            "Ilo\u015bciowa dyskretna", "Ilo\u015bciowa ci\u0105g\u0142a"),
      b = c("Moda, cz\u0119sto\u015bci, proporcje",
            "Moda, cz\u0119sto\u015bci, proporcje, cz\u0119sto\u015bci skumulowane",
            "\u015arednia, mediana, odch. std., kwartyle",
            "\u015arednia, mediana, odch. std., kwartyle, sko\u015bno\u015b\u0107, kurtoza"),
      c = c("S\u0142upkowy, ko\u0142owy (ostro\u017cnie!)",
            "S\u0142upkowy (z zachowaniem kolejno\u015bci)",
            "S\u0142upkowy, punktowy",
            "Histogram, g\u0119sto\u015bci, pude\u0142kowy, skrzypcowy"),
      d = c("Nie obliczaj \u015bredniej ani mediany",
            "Nie obliczaj \u015bredniej (sporne!), nie rysuj histogramu",
            "Nie rysuj wykresu g\u0119sto\u015bci (sko\u0144czona l. warto\u015bci)",
            "Nie rysuj wykresu s\u0142upkowego (zbyt wiele warto\u015bci)"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Typ zmiennej", "Odpowiednie statystyki",
                   "Odpowiednie wykresy", "Czego NIE robi\u0107")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch6_location_table <- renderTable({
    df <- data.frame(
      a = c("\u015arednia arytmetyczna", "Mediana",
            "\u015arednia ucinana", "Dominanta (moda)"),
      b = c("Dane symetryczne, bez outlier\u00f3w",
            "Dane sko\u015bne lub z outlierami",
            "Kompromis mi\u0119dzy \u015bredni\u0105 a median\u0105",
            "Zmienne nominalne; szukanie najcz\u0119stszej warto\u015bci"),
      c = c("Niska -- jeden outlier mo\u017ce silnie przesun\u0105\u0107",
            "Wysoka -- zale\u017cy tylko od rang",
            "\u015arednia -- ucina skrajne obserwacje",
            "Nie dotyczy (kategorie)"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Miara", "Kiedy stosowa\u0107", "Odporno\u015b\u0107 na outliery")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch6_spread_table <- renderTable({
    df <- data.frame(
      a = c("Odchylenie standardowe (s)", "Wariancja (s\u00b2)",
            "Rozst\u0119p (Range)", "IQR", "Wsp\u00f3\u0142czynnik zmienno\u015bci (CV)"),
      b = c("Przeci\u0119tne odchylenie od \u015bredniej",
            "Kwadrat odchylenia -- w jednostkach\u00b2",
            "Max - Min (wra\u017cliwy na outliery)",
            "Rozrzut \u015brodkowych 50% danych (Q3 - Q1)",
            "Rozrzut wzgl\u0119dem \u015bredniej (%), pozwala por\u00f3wnywa\u0107 zmienne"),
      c = c("Dane symetryczne, zbli\u017cone do normalnego",
            "We wzorach (rzadko raportowana wprost)",
            "Szybka orientacja, ma\u0142e zbiory",
            "Dane sko\u015bne, z outlierami, razem z boxplotem",
            "Por\u00f3wnanie rozrzutu zmiennych o r\u00f3\u017cnych jednostkach"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Miara", "Co mierzy", "Kiedy stosowa\u0107")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch6_shape_table <- renderTable({
    df <- data.frame(
      a = c("Sko\u015bno\u015b\u0107", "Kurtoza (excess)"),
      b = c("0 = symetryczny, >0 prawostronny, <0 lewostronny",
            "0 = normalny (mezokurtyczny), >0 ci\u0119\u017ckie ogony, <0 lekkie ogony"),
      c = c("|\u015bko\u015bno\u015b\u0107| > 1: silna asymetria, rozwa\u017c median\u0119 zamiast \u015bredniej",
            "Kurtoza > 2: cz\u0119ste warto\u015bci ekstremalne, uwa\u017caj na outliery"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Miara", "Warto\u015b\u0107 referencyjna", "Praktyczna regu\u0142a")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch6_plots_table <- renderTable({
    df <- data.frame(
      a = c("S\u0142upkowy (bar)", "Ko\u0142owy (pie)", "Histogram",
            "G\u0119sto\u015bci (density)", "Pude\u0142kowy (boxplot)",
            "Skrzypcowy (violin)", "Heatmapa"),
      b = c("Jako\u015bciowe (nominalne, porz\u0105dkowe)",
            "Jako\u015bciowe (tylko du\u017ce r\u00f3\u017cnice!)",
            "Ilo\u015bciowe ci\u0105g\u0142e",
            "Ilo\u015bciowe ci\u0105g\u0142e (g\u0142adka wersja histogramu)",
            "Ilo\u015bciowe (por\u00f3wnanie grup)",
            "Ilo\u015bciowe (pe\u0142ny kszta\u0142t rozk\u0142adu + por\u00f3wnanie)",
            "Tabela krzy\u017cowa (dwie zmienne jako\u015bciowe)"),
      c = c("Liczebno\u015bci / proporcje kategorii",
            "Proporcje (tylko gdy kategorie bardzo si\u0119 r\u00f3\u017cni\u0105)",
            "Kszta\u0142t rozk\u0142adu, sko\u015bno\u015b\u0107, modalno\u015b\u0107",
            "Kszta\u0142t rozk\u0142adu (bez zale\u017cno\u015bci od bin\u00f3w)",
            "Median\u0119, IQR, outliery -- kompaktowo",
            "Pe\u0142ny kszta\u0142t + median\u0119/IQR",
            "Zale\u017cno\u015bci mi\u0119dzy zmiennymi jako\u015bciowymi"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Wykres", "Typ danych", "Pokazuje")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch6_mistakes_table <- renderTable({
    df <- data.frame(
      a = c("\u015arednia z danych nominalnych",
            "Histogram dla zmiennej dyskretnej (ma\u0142o warto\u015bci)",
            "Tylko \u015brednia bez miary rozrzutu",
            "Pie chart dla podobnych warto\u015bci",
            "Ignorowanie outlier\u00f3w",
            "Dominanta dla danych ci\u0105g\u0142ych",
            "SD bez sprawdzenia symetrii"),
      b = c("Kategorie nie maj\u0105 warto\u015bci liczbowej -- wynik jest bezsensowny",
            "Biny \u0142\u0105cz\u0105 warto\u015bci, kt\u00f3re powinny by\u0107 osobno -- u\u017cyj wykresu s\u0142upkowego",
            "Dwie grupy z t\u0105 sam\u0105 \u015bredni\u0105 mog\u0105 mie\u0107 zupe\u0142nie r\u00f3\u017cny rozrzut",
            "Ludzkie oko nie odr\u00f3\u017cnia k\u0105t\u00f3w r\u00f3\u017cni\u0105cych si\u0119 o 2-3%",
            "Jeden outlier mo\u017ce przesun\u0105\u0107 \u015bredni\u0105 i zwi\u0119kszy\u0107 SD",
            "Prawie ka\u017cda warto\u015b\u0107 wyst\u0119puje 1-2 razy -- moda jest przypadkowa",
            "Przy silnej sko\u015bno\u015bci SD s\u0142abo opisuje rozrzut -- lepsza jest IQR"),
      c = c("U\u017cyj dominanty (mody) i tabeli cz\u0119sto\u015bci",
            "U\u017cyj wykresu s\u0142upkowego (geom_col / geom_bar)",
            "Zawsze raportuj \u015bredni\u0105 + SD lub median\u0119 + IQR",
            "U\u017cyj wykresu s\u0142upkowego",
            "Raportuj median\u0119 + IQR obok \u015bredniej + SD",
            "U\u017cyj \u015bredniej i mediany",
            "Sprawd\u017a sko\u015bno\u015b\u0107; je\u015bli |skew| > 1, raportuj median\u0119 + IQR"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("B\u0142\u0105d", "Dlaczego to b\u0142\u0105d", "Co zrobi\u0107 zamiast")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

}
