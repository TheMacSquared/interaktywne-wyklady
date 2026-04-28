# ============================================================================
# CHAPTER 6: Ściąga
# ============================================================================

ch6_ui <- list(
  id = "ch-sciaga", num = "06", title = "Ściąga",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 06 · Statystyka opisowa",
      num    = "06",
      title  = "Ściąga.",
      lead   = "Kompletne podsumowanie — wszystkie pojęcia i narzędzia
                omówione w trakcie wykładu w jednym miejscu. Warto ją mieć
                pod ręką podczas analiz."
    ),

    # --- Cheat sheet 1: Tools by variable type ---
    lc_h2("ch6-narzedzia", "Narzędzia wg typu zmiennej"),

    inline_callout(
      label = "Najczęstszy błąd",
      "Obliczanie średniej z danych nominalnych lub porządkowych
       (np. średnia z kodów kierunków). Wynik będzie liczbą, ale
       nie będzie miał żadnego sensu!",
      color = "uwaga"
    ),

    figure_panel(
      label = "Ryc. 6.1",
      title = "Narzędzia wg typu zmiennej",
      tableOutput("ch6_ref_table")
    ),

    # --- Cheat sheet 2: Measures summary ---
    lc_h2("ch6-miary", "Miary statystyczne — kiedy co stosować"),

    figure_panel(
      label = "Ryc. 6.2",
      title = "Miary statystyczne",
      h4("Miary położenia (rozdział 3)"),
      tableOutput("ch6_location_table"),
      hr(),
      h4("Miary rozrzutu (rozdział 4)"),
      tableOutput("ch6_spread_table"),
      hr(),
      h4("Miary kształtu (rozdział 5)"),
      tableOutput("ch6_shape_table")
    ),

    # --- Cheat sheet 3: Visualization guide ---
    lc_h2("ch6-wykresy", "Przewodnik po wykresach"),

    figure_panel(
      label = "Ryc. 6.3",
      title = "Przewodnik po wykresach",
      tableOutput("ch6_plots_table")
    ),

    # --- Cheat sheet 4: Common mistakes ---
    lc_h2("ch6-bledy", "Typowe błędy"),

    figure_panel(
      label = "Ryc. 6.4",
      title = "Typowe błędy",
      tableOutput("ch6_mistakes_table")
    ),

    # --- Formulas ---
    lc_h2("ch6-wzory", "Wzory"),

    figure_panel(
      label = "Ryc. 6.5",
      title = "Wzory",
      withMathJax(
        h4("Miary położenia"),
        helpText("$$\\bar{x} = \\frac{1}{n} \\sum_{i=1}^{n} x_i$$"),
        p("Mediana: wartość środkowa po posortowaniu danych"),
        hr(),
        h4("Miary rozrzutu"),
        helpText("$$s^2 = \\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2$$"),
        helpText("$$s = \\sqrt{s^2}$$"),
        helpText("$$CV = \\frac{s}{\\bar{x}} \\cdot 100\\%$$"),
        helpText("$$IQR = Q_3 - Q_1$$"),
        hr(),
        h4("Miary kształtu"),
        helpText("$$\\text{Skośność} = \\frac{n}{(n-1)(n-2)} \\sum \\left(\\frac{x_i - \\bar{x}}{s}\\right)^3$$"),
        helpText("$$\\text{Kurtoza (excess)} = \\frac{n(n+1)}{(n-1)(n-2)(n-3)} \\sum \\left(\\frac{x_i - \\bar{x}}{s}\\right)^4 - \\frac{3(n-1)^2}{(n-2)(n-3)}$$")
      )
    ),

    lc_chapter_next(
      num       = "07",
      title     = "Quiz",
      lead      = "sprawdź się — rozpoznawanie typów zmiennych w praktyce.",
      target_id = "ch-quiz"
    ),

    br(), br()
  )
)

# --------------------------------------------------------------------------
# Chapter 6 Server
# --------------------------------------------------------------------------

ch6_server <- function(input, output, session) {

  output$ch6_ref_table <- renderTable({
    df <- data.frame(
      a = c("Jakościowa nominalna", "Jakościowa porządkowa",
            "Ilościowa dyskretna", "Ilościowa ciągła"),
      b = c("Moda, częstości, proporcje",
            "Moda, częstości, proporcje, częstości skumulowane",
            "Średnia, mediana, odch. std., kwartyle",
            "Średnia, mediana, odch. std., kwartyle, skośność, kurtoza"),
      c = c("Słupkowy, kołowy (ostrożnie!)",
            "Słupkowy (z zachowaniem kolejności)",
            "Słupkowy, punktowy",
            "Histogram, gęstości, pudełkowy, skrzypcowy"),
      d = c("Nie obliczaj średniej ani mediany",
            "Nie obliczaj średniej (sporne!), nie rysuj histogramu",
            "Nie rysuj wykresu gęstości (skończona l. wartości)",
            "Nie rysuj wykresu słupkowego (zbyt wiele wartości)"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Typ zmiennej", "Odpowiednie statystyki",
                   "Odpowiednie wykresy", "Czego NIE robić")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch6_location_table <- renderTable({
    df <- data.frame(
      a = c("Średnia arytmetyczna", "Mediana",
            "Średnia ucinana", "Dominanta (moda)"),
      b = c("Dane symetryczne, bez outlierów",
            "Dane skośne lub z outlierami",
            "Kompromis między średnią a medianą",
            "Zmienne nominalne; szukanie najczęstszej wartości"),
      c = c("Niska -- jeden outlier może silnie przesunąć",
            "Wysoka -- zależy tylko od rang",
            "Średnia -- ucina skrajne obserwacje",
            "Nie dotyczy (kategorie)"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Miara", "Kiedy stosować", "Odporność na outliery")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch6_spread_table <- renderTable({
    df <- data.frame(
      a = c("Odchylenie standardowe (s)", "Wariancja (s²)",
            "Rozstęp (Range)", "IQR", "Współczynnik zmienności (CV)"),
      b = c("Przeciętne odchylenie od średniej",
            "Kwadrat odchylenia -- w jednostkach²",
            "Max - Min (wrażliwy na outliery)",
            "Rozrzut środkowych 50% danych (Q3 - Q1)",
            "Rozrzut względem średniej (%), pozwala porównywać zmienne"),
      c = c("Dane symetryczne, zbliżone do normalnego",
            "We wzorach (rzadko raportowana wprost)",
            "Szybka orientacja, małe zbiory",
            "Dane skośne, z outlierami, razem z boxplotem",
            "Porównanie rozrzutu zmiennych o różnych jednostkach"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Miara", "Co mierzy", "Kiedy stosować")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch6_shape_table <- renderTable({
    df <- data.frame(
      a = c("Skośność", "Kurtoza (excess)"),
      b = c("0 = symetryczny, >0 prawostronny, <0 lewostronny",
            "0 = normalny (mezokurtyczny), >0 ciężkie ogony, <0 lekkie ogony"),
      c = c("|śkośność| > 1: silna asymetria, rozważ medianę zamiast średniej",
            "Kurtoza > 2: częste wartości ekstremalne, uważaj na outliery"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Miara", "Wartość referencyjna", "Praktyczna reguła")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch6_plots_table <- renderTable({
    df <- data.frame(
      a = c("Słupkowy (bar)", "Kołowy (pie)", "Histogram",
            "Gęstości (density)", "Pudełkowy (boxplot)",
            "Skrzypcowy (violin)", "Heatmapa"),
      b = c("Jakościowe (nominalne, porządkowe)",
            "Jakościowe (tylko duże różnice!)",
            "Ilościowe ciągłe",
            "Ilościowe ciągłe (gładka wersja histogramu)",
            "Ilościowe (porównanie grup)",
            "Ilościowe (pełny kształt rozkładu + porównanie)",
            "Tabela krzyżowa (dwie zmienne jakościowe)"),
      c = c("Liczebności / proporcje kategorii",
            "Proporcje (tylko gdy kategorie bardzo się różnią)",
            "Kształt rozkładu, skośność, modalność",
            "Kształt rozkładu (bez zależności od binów)",
            "Medianę, IQR, outliery -- kompaktowo",
            "Pełny kształt + medianę/IQR",
            "Zależności między zmiennymi jakościowymi"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Wykres", "Typ danych", "Pokazuje")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch6_mistakes_table <- renderTable({
    df <- data.frame(
      a = c("Średnia z danych nominalnych",
            "Histogram dla zmiennej dyskretnej (mało wartości)",
            "Tylko średnia bez miary rozrzutu",
            "Pie chart dla podobnych wartości",
            "Ignorowanie outlierów",
            "Dominanta dla danych ciągłych",
            "SD bez sprawdzenia symetrii"),
      b = c("Kategorie nie mają wartości liczbowej -- wynik jest bezsensowny",
            "Biny łączą wartości, które powinny być osobno -- użyj wykresu słupkowego",
            "Dwie grupy z tą samą średnią mogą mieć zupełnie różny rozrzut",
            "Ludzkie oko nie odróżnia kątów różniących się o 2-3%",
            "Jeden outlier może przesunąć średnią i zwiększyć SD",
            "Prawie każda wartość występuje 1-2 razy -- moda jest przypadkowa",
            "Przy silnej skośności SD słabo opisuje rozrzut -- lepsza jest IQR"),
      c = c("Użyj dominanty (mody) i tabeli częstości",
            "Użyj wykresu słupkowego (geom_col / geom_bar)",
            "Zawsze raportuj średnią + SD lub medianę + IQR",
            "Użyj wykresu słupkowego",
            "Raportuj medianę + IQR obok średniej + SD",
            "Użyj średniej i mediany",
            "Sprawdź skośność; jeśli |skew| > 1, raportuj medianę + IQR"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Błąd", "Dlaczego to błąd", "Co zrobić zamiast")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

}
