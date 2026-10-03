# Tab 11: Kawiarnia — sprzedaż dzienna, braki danych + szereg czasowy

ch11_ui <- lecture_chapter(id = "ch11", num = "11", title = "Kawiarnia", content = tagList(

  lc_chapter_hero(
    kicker = "Rozdział 11 · Co czyni dobry zbiór danych?",
    num    = "11",
    title  = "Kawiarnia studencka.",
    lead   = "Dane dzienne mogą wyglądać jak zwykła tabela, ale ukrywać
              strukturę szeregu czasowego i naruszenie niezależności obserwacji."
  ),

  lc_h2("sec-01", "Opis"),

  lc_p("Kawiarnia na terenie kampusu zapisywała dzienną sprzedaż przez cały
    rok akademicki: 245 kolejnych dni od 1 października do 1 czerwca. Dla
    każdego dnia mamy jego numer, datę, dzień tygodnia, liczbę sprzedanych kaw
    i temperaturę na zewnątrz. Właściciel chce wiedzieć, czy temperatura
    wpływa na sprzedaż, czyli czy w chłodniejsze dni studenci kupują więcej
    kawy."),

  lc_h2("sec-02", "Podgląd danych"),

  lc_p("Przeglądając tabelę, zwróć uwagę na puste komórki i na to, czym jest
    jeden wiersz."),

  figure_panel(
    label = "Ryc. 11.1",
    title = "Sprzedaż kawy dzień po dniu",
    uiOutput("tab10_table")
  ),

  lc_p("Każdy wiersz to jeden dzień. W tabeli 245 dni wygląda tak samo jak
    150 studentów z poprzedniego rozdziału: jak zbiór osobnych, równorzędnych
    pomiarów. W kolumnie kaw co jakiś czas pojawia się kreska oznaczająca
    brak danych."),

  lc_h2("sec-03", "Braki danych"),

  lc_p("Wykres pokazuje odsetek brakujących wartości w obu zmiennych."),

  figure_panel(
    label = "Ryc. 11.2",
    title = "Odsetek braków w zmiennych",
    lc_plot("tab10_missing", max_height = "300px"),
    uiOutput("tab10_missing_info")
  ),

  lc_p("Liczba kaw nie jest znana dla 32 dni (13.1%), temperatura dla 7 dni
    (2.9%). Do analizy obu zmiennych naraz zostaje 207 kompletnych dni.
    Ważniejsze od samego odsetka jest pytanie, dlaczego danych brakuje.
    Według właściciela sprzedaż nie została zapisana w dni, w które kawiarnia
    była zamknięta, na przykład w święta, i w dni awarii kasy. Jeśli tak, braki
    nie są losowe: święta wypadają w określonych tygodniach roku, a w okolicy
    zamknięcia ruch zwykle jest nietypowy. To problem braków danych z katalogu
    z rozdziału 1, i to taki, którego nie wolno z góry uznać za przypadkowy."),

  lc_h2("sec-04", "Dane w kolejności dni"),

  lc_p("Tabela nie pokazuje, że wiersze mają ustaloną kolejność. Wykres
    sprzedaży dzień po dniu ujawnia, czy kolejne obserwacje są do siebie
    podobne."),

  figure_panel(
    label = "Ryc. 11.3",
    title = "Sprzedaż w kolejnych dniach roku akademickiego",
    lc_action("tab10_reveal", "Pokaż dane w kolejności", variant = "solid"),
    conditionalPanel("input.tab10_reveal > 0",
      lc_plot("tab10_lineplot", ratio = "1.8/1", max_height = "350px"),
      lc_caption("Sprzedaż powtarza tygodniowy rytm: wysoka w dni robocze,
                  niska w weekendy.")
    )
  ),

  lc_p("Linia nie jest przypadkowym szumem. Widać na niej regularny,
    tygodniowy rytm: średnio od 134 do 147 kaw od poniedziałku do czwartku,
    124 w piątek, 98 w sobotę i 87 w niedzielę. Na ten rytm nakłada się
    wolniejsza fala pór roku: w styczniu i lutym kawiarnia sprzedawała
    średnio około 135 kaw dziennie, a w maju 107. Znając dzień tygodnia
    i porę roku, można więc
    całkiem dobrze przewidzieć sprzedaż, zanim spojrzymy na temperaturę."),

  lc_p("Podobieństwo kolejnych obserwacji można zmierzyć. ",
    gloss("autokorelacja", "Autokorelacja"), " to korelacja zmiennej z nią
    samą przesuniętą w czasie. Przy przesunięciu o jeden dzień zestawiamy
    sprzedaż każdego dnia ze sprzedażą dnia następnego; pomijamy pary,
    w których brakuje jednej z wartości. Gdyby dni były
    niezależne, punkty na wykresie tworzyłyby bezkształtną chmurę,
    a korelacja byłaby bliska zera."),

  lc_formula_box(withMathJax(
    "$$r_1 = \\text{cor}(x_t,\\; x_{t+1})$$"
  )),

  conditionalPanel("input.tab10_reveal > 0",
    figure_panel(
      label = "Ryc. 11.4",
      title = "Sprzedaż danego dnia i dnia następnego",
      lc_plot("tab10_lag", max_height = "300px"),
      uiOutput("tab10_autocorr_info")
    )
  ),

  lc_p("Korelacja kolejnych dni wynosi 0.37: dzień z dużą sprzedażą częściej
    sąsiaduje z innym dniem dużej sprzedaży. To zależność umiarkowana, ale
    najsilniejsza więź łączy dni odległe o tydzień. Sprzedaż w dany dzień
    i w ten sam dzień tygodnia tydzień później koreluje na poziomie 0.84."),

  lc_p("To problem braku niezależności obserwacji z katalogu z rozdziału 1.
    Dane zbierane dzień po dniu tworzą ", gloss("szereg czasowy"), ", a testy
    z wykładów 04–06 zakładają ", gloss("niezależność obserwacji"), ".
    Gdy kolejne obserwacje są do siebie podobne, 245 dni niesie mniej
    informacji niż 245 niezależnych pomiarów. Test, który tego nie
    uwzględnia, zaniża błędy standardowe i daje zbyt małe wartości p."),

  lc_h2("sec-05", "Werdykt"),

  lc_p("Zbiór ma dwa problemy. Braki danych dałoby się opanować, gdybyśmy
    wiedzieli, które dni były zamknięciami, a które awariami. Drugi problem
    przekreśla prostą analizę. ", gloss("korelacja Pearsona", "Korelacja Pearsona"),
    " między temperaturą a sprzedażą wynosi -0.28 i sugerowałaby, że
    w chłodniejsze dni kawy sprzedaje się więcej. Obie zmienne zmieniają się
    jednak w rytmie pór roku: zimą jest zimno, a kawiarnia ma więcej klientów,
    wiosną robi się cieplej, a ruch słabnie. Taka
    korelacja może odzwierciedlać wyłącznie wspólny przebieg w czasie: czas
    działa tu jak ", gloss("zmienna zakłócająca"), ", a wynik byłby ",
    gloss("korelacja pozorna", "korelacją pozorną"), "."),

  lc_p(gloss("agregacja", "Agregacja"), " do tygodni usuwa rytm tygodniowy:
    średnia sprzedaż z całego tygodnia staje się jedną obserwacją, a wszystkie
    dni tygodnia ważą w niej tyle samo. Zostaje 35 tygodni, czyli skromna
    próba o małej mocy. Wolna fala w ciągu roku nie znika, więc kolejne
    tygodnie wciąż są do siebie podobne. Ich autokorelacja wynosi 0.78.
    Agregacja jest tu wyborem, a nie darmową naprawą: zmniejsza zależność
    między dniami tego samego tygodnia, ale nie oddziela temperatury od
    pory roku. Do tego potrzebne byłyby dane z kilku lat akademickich,
    w których ", gloss("sezonowość", "sezonowość"), " powtarza się, a temperatura
    w tych samych tygodniach bywa różna, oraz metody szeregów czasowych,
    których ten kurs nie obejmuje."),

  lc_note("Werdykt",
    "Zbiór zły do prostych testów: obserwacje dzienne są zależne, a związek
     temperatury ze sprzedażą miesza się z porą roku."
  ),

  lc_p("Dziesięć zbiorów dało dziesięć różnych werdyktów, a o żadnym nie
    zdecydowała jedna liczba. Niektórych usterek nie naprawi żadna obróbka:
    zbyt małej próby w ankiecie na grupie, braku zmienności w ocenach hotelu
    ani pytań, które od początku zbierały nieporównywalne odpowiedzi, jak
    w formularzu rejestracyjnym. Inne, jak błędy przepisywania w badaniach
    laboratoryjnych czy niewielkie braki danych, wymagają pracy, ale
    zostawiają użyteczny zbiór. Najtrudniej zauważyć problemy struktury:
    zdarzenia zamiast jednostek w filmach Tarantino czy zależne od siebie
    dni w kawiarni w tabeli wyglądają jak zwykłe wiersze. Dlatego ocenę
    każdego zbioru warto zaczynać od pytania, co jest jednostką obserwacji
    i czy jednostki są od siebie niezależne, a dopiero potem cokolwiek
    liczyć."),

  lc_chapter_next(
    num = "12",
    title = "Ściąga",
    lead = "Ściąga zbiera kryteria z dziesięciu przypadków w jedną listę
            kontrolną do oceny własnych danych.",
    target_id = "ch12"
  )
))

# Pary (dzień t, dzień t + lag) w kolejności kalendarza; pomija pary z brakiem.
# Usunięcie braków przed przesunięciem łączyłoby dni, które nie sąsiadują.
cafe_lag_pairs <- function(x, lag = 1) {
  n  <- length(x)
  df <- data.frame(x = x[seq_len(n - lag)], y = x[(lag + 1):n])
  df[stats::complete.cases(df), ]
}

ch11_server <- function(input, output, session) {

  output$tab10_table <- renderUI({
    dd_data_table(round_df(cafe_data), page_size = 10, page = input$tab10_table_page, page_input = "tab10_table_page")
  })

  zoom_plot_server("tab10_missing", reactive({
    miss_pct <- sapply(cafe_data[, c("kawy", "temperatura")], function(x) mean(is.na(x)) * 100)
    df_miss  <- data.frame(variable = names(miss_pct), pct = miss_pct)
    df_miss  <- df_miss[df_miss$pct > 0, ]

    ggplot(df_miss, aes(x = reorder(variable, -pct), y = pct)) +
      geom_col(fill = data_primary) +
      geom_text(aes(label = paste0(round(pct, 1), "%")),
                vjust = -0.4, size = 5, fontface = "bold") +
      scale_y_continuous(limits = c(0, 30)) +
      labs(
           x = NULL, y = "% braków") +
      theme_upwr(base_size = 14)
  }))

  output$tab10_missing_info <- renderUI({
    kawy_na <- sum(is.na(cafe_data$kawy))
    temp_na <- sum(is.na(cafe_data$temperatura))
    n_comp  <- sum(complete.cases(cafe_data[, c("kawy", "temperatura")]))
    lc_status(
      paste0("Brakujące wartości: kawy ", kawy_na, " (",
             round(kawy_na / nrow(cafe_data) * 100, 1), "%), temperatura ", temp_na, " (",
             round(temp_na / nrow(cafe_data) * 100, 1), "%). ",
             "Kompletne dni: ", n_comp, " z ", nrow(cafe_data), ".")
    )
  })

  zoom_plot_server("tab10_lineplot", reactive({
    df <- cafe_data[!is.na(cafe_data$kawy), ]
    ggplot(df, aes(x = dzien, y = kawy)) +
      geom_line(color = data_primary, alpha = 0.6) +
      geom_point(color = data_primary, size = 1.2, alpha = 0.4) +
      labs(
           
           x = "Numer dnia (= kolejność w roku akademickim)", y = "Liczba sprzedanych kaw") +
      theme_upwr(base_size = 14)
  }))

  zoom_plot_server("tab10_lag", reactive({
    lag_df <- cafe_lag_pairs(cafe_data$kawy)

    ggplot(lag_df, aes(x = x, y = y)) +
      geom_point(alpha = 0.4, color = data_reference) +
      geom_smooth(method = "lm", color = data_bad, se = TRUE) +
      labs(
           
           x = "Kawy(t)", y = "Kawy(t+1)") +
      theme_upwr(base_size = 14)
  }))

  output$tab10_autocorr_info <- renderUI({
    lag_df <- cafe_lag_pairs(cafe_data$kawy)
    r      <- cor(lag_df$x, lag_df$y)
    lc_caption(paste0("Autokorelacja przy przesunięciu o jeden dzień: r = ", round(r, 3),
                      " (", nrow(lag_df), " par sąsiednich dni z obiema wartościami)."))
  })

}
