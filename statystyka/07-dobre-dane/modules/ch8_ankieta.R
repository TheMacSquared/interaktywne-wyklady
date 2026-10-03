# Tab 8: Formularz rejestracyjny — mix dobrych i złych zmiennych

ch8_ui <- lecture_chapter(id = "ch8", num = "8", title = "Formularz", content = tagList(

  lc_chapter_hero(
    kicker = "Rozdział 08 · Co czyni dobry zbiór danych?",
    num    = "08",
    title  = "Formularz rejestracyjny.",
    lead   = "Źle zaprojektowane pytania tworzą dane, których nie da się
              po prostu naprawić jednym kliknięciem w analizie."
  ),

  lc_h2("sec-01", "Opis"),

  lc_p("Organizatorzy wakacyjnego kursu zbierali zapisy przez formularz online
    i otrzymali 90 zgłoszeń. Formularz miał pięć pól: wiek, wykształcenie,
    doświadczenie, dostępność i samoocenę umiejętności. Część pól była listą
    do wyboru albo polem liczbowym, a część zwykłym polem tekstowym, w które
    każdy wpisywał, co chciał. Naturalne pytanie dla takiego zbioru brzmi:
    czy doświadczenie i wykształcenie uczestników wiążą się z tym, jak
    oceniają swoje umiejętności?"),

  lc_h2("sec-02", "Podgląd danych"),

  lc_p("Przeglądając tabelę, zwróć uwagę, które kolumny zawierają liczby albo
    powtarzające się kategorie, a które swobodny tekst."),

  figure_panel(
    label = "Ryc. 8.1",
    title = "Zgłoszenia na kurs (90 osób)",
    uiOutput("tab7_table")
  ),

  lc_p("Już pierwsze wiersze pokazują dwa rodzaje kolumn. Wiek to liczba
    całkowita od 19 do 36 lat, a wykształcenie przyjmuje tylko cztery wartości:
    technikum, licencjat, inżynier i magister. W polu doświadczenia obok „3”
    i „5 lat” stoją „trochę”, „tak mam” i „licencjat znam”, dostępność to
    opisy w rodzaju „nie w piątki” czy „kiedy trzeba”, a samoocena miesza
    liczby z ocenami słownymi i szkolnymi („dobry”, „B+”, „8/10”)."),

  lc_h2("sec-03", "Próba policzenia średniej"),

  lc_p("Najprostszy sprawdzian, czy kolumna nadaje się do analizy ilościowej,
    to próba policzenia jej średniej. Panel liczy średnią wybranej zmiennej
    i pokazuje, ile wartości program w ogóle rozpoznał jako liczby."),

  figure_panel(
    label = "Ryc. 8.2",
    title = "Średnia wybranej zmiennej",
    selectInput("tab7_var", "Wybierz zmienną:",
      choices = c("wiek", "wyksztalcenie", "doswiadczenie", "dostepnosc", "ocena_umiejetnosci")),
    lc_action("tab7_mean", "Policz średnią", variant = "solid"),
    uiOutput("tab7_mean_result")
  ),

  lc_p("Średni wiek uczestników wynosi 27.4 roku i nie wymaga żadnej obróbki.
    Wykształcenie jest ", gloss("zmienna jakościowa", "zmienną jakościową"),
    ", więc średniej się dla niego nie liczy, ale kategorie są spójne
    i wystarczy je zliczyć: 33 osoby po licencjacie, 27 po magisterium,
    17 inżynierów i 13 absolwentów technikum. Trzy pozostałe kolumny
    zawodzą. W doświadczeniu tylko 8 z 90 wpisów to liczby, więc 91.1%
    wartości nie da się odczytać. W dostępności nie da się odczytać żadnej,
    a w samoocenie liczbami jest 38 wpisów, czyli 57.8% wartości przepada."),

  lc_p("To problem, który w katalogu z rozdziału 1 nazwaliśmy źle
    zdefiniowanymi zmiennymi. Pytanie otwarte zamiast skali sprawia, że ta sama
    cecha jest zapisana na kilkanaście sposobów, a program statystyczny nie
    ma jak przeliczyć „trochę” albo „elastycznie” na liczbę."),

  lc_h2("sec-04", "Czyszczenie danych"),

  lc_p("Część wpisów da się przetłumaczyć na liczby, jeśli przyjmiemy reguły
    przekodowania, na przykład „ponad rok” = 1 rok doświadczenia, „brak” = 0,
    „dobry” = 7 na skali od 1 do 10. Porównując wersję surową z oczyszczoną,
    zwróć uwagę, ile braków powstało i ile decyzji podjęliśmy za respondentów."),

  figure_panel(
    label = "Ryc. 8.3",
    title = "Dane surowe i po przekodowaniu",
    lc_segmented("tab7_toggle", "Widok danych", choices = c("Surowe", "Oczyszczone")),
    uiOutput("tab7_clean_table"),
    uiOutput("tab7_clean_info")
  ),

  lc_p("Po przekodowaniu wiek i wykształcenie zostają bez zmian. W kolumnie
    doświadczenia udało się przypisać liczbę lat 47 osobom, a 43 zostały
    z brakiem danych, i to przy założeniu, że „ponad rok” znaczy dokładnie rok.
    Samoocenę przekodowaliśmy w całości, ale tylko 38 wpisów było liczbami od
    początku. 11 odpowiedzi „8/10” dało się przeliczyć wprost, a 41 ocen
    słownych zamieniliśmy na liczby według reguły, którą sami wymyśliliśmy.
    Dostępności nie da się sprowadzić ani do liczby, ani do krótkiej listy
    kategorii, więc kolumna wypada z analizy. Każda taka reguła to decyzja
    analityka, która nie wynika z danych, i trzeba ją opisać w raporcie."),

  lc_p("Tych kłopotów można było uniknąć na etapie projektowania formularza.
    Zmienne, które chcemy analizować, powinny mieć zamknięte odpowiedzi: listę
    kategorii albo pole liczbowe z podaną jednostką, np. „liczba lat
    doświadczenia”. Pola czysto informacyjne, których nie zamierzamy liczyć,
    mogą zostać otwarte. Przed uruchomieniem formularza warto dać go do
    wypełnienia kilku osobom i sprawdzić, czy odpowiedzi od razu dają się
    wczytać jako dane."),

  lc_note("Zasada", rule = TRUE,
    "Pytania o zmienne, które będziesz analizować, zamykaj: lista kategorii
     albo liczba z podaną jednostką."
  ),

  lc_h2("sec-05", "Werdykt"),

  lc_p("Formularz daje dwie zmienne gotowe do analizy: wiek i wykształcenie.
    Wystarczą do opisu uczestników, a nawet do porównania średniego wieku
    w grupach wykształcenia, choć technikum reprezentuje tylko 13 osób.
    Pytanie postawione na początku wymaga jednak doświadczenia i samooceny,
    a te istnieją tylko w wersji przekodowanej: doświadczenie z brakami
    u prawie połowy osób, samoocena z liczbami, które w dużej części sami
    przypisaliśmy. Wynik takiej analizy mówiłby więcej o naszych regułach
    przekodowania niż o uczestnikach kursu."),

  lc_p("Braki w doświadczeniu nie są przy tym losowe. Powstają tam, gdzie ktoś
    odpowiedział opisowo („trochę”, „dużo”, „tak mam”), czyli zależą od samej
    odpowiedzi. Usunięcie tych osób zmieniłoby skład próby w sposób, którego
    nie potrafimy opisać."),

  lc_note("Werdykt",
    "Zbiór zły do postawionego pytania: kluczowe zmienne są źle zdefiniowane
     i nie da się ich naprawić bez arbitralnych decyzji."
  ),

  lc_h2("sec-06", "Drugi przykład: dane do uratowania"),

  lc_p("Nie każdy formularz z polami tekstowymi jest stracony. W innym
    formularzu tego samego kursu respondenci też wpisywali odpowiedzi po
    swojemu, ale prawie każdą z nich da się jednoznacznie przypisać do jednej
    z kilku kategorii. Porównując obie wersje tabeli, zwróć uwagę, które wpisy
    nie mają odpowiednika po standaryzacji."),

  div(class = "toggle-pills",
    actionButton("tab7b_raw", "Surowe", class = "pill-btn active"),
    actionButton("tab7b_cat", "Po standaryzacji", class = "pill-btn")
  ),

  figure_panel(
    label = "Ryc. 8.4",
    title = "12 zgłoszeń przed i po standaryzacji",
    uiOutput("tab7b_table")
  ),

  lc_p("Standaryzacja sprowadza różne zapisy tej samej odpowiedzi do jednej
    etykiety. „podst.”, „PODSTAWOWY” i „podstawowy” to ten sam poziom, a
    „przel.”, „przelew bankowy” i „PRZELEW” to ten sam sposób płatności.
    Umowna jest tylko decyzja, by „paypal” zaliczyć do kart. Godziny nauki
    zamieniamy na trzy przedziały: „ok. 5”, „4-6h” i „5h” trafiają do
    kategorii średniej (4–6 h). Po standaryzacji kompletnych jest 10 z 12
    wierszy (83%). Wpisów „dużo” i „mało” nie da się przypisać do żadnego
    przedziału, więc zostają brakami danych."),

  lc_p("Ceną jest utrata dokładności: zamiast liczby godzin mamy ",
    gloss("zmienna porządkowa", "zmienną porządkową"), " z trzema poziomami.
    W zamian zmienna staje się użyteczna. Różnica wobec pierwszego formularza
    polega na tym, że tu reguły przypisania wynikają z samych odpowiedzi,
    a nie z pomysłu analityka."),

  lc_chapter_next(
    num = "09",
    title = "Badania laboratoryjne",
    lead = "Formularz psuł dane już na etapie pytań. W badaniach laboratoryjnych
            pytania są dobre, a błędy pojawiają się przy przepisywaniu wyników.",
    target_id = "ch9"
  ),

  div(style = "height: 40px;")
))

ch8_server <- function(input, output, session) {

  # Reguły przekodowania (wspólne dla tabeli i opisu zmian)
  dosw_map  <- c("3" = 3, "5 lat" = 5, "ponad rok" = 1, "nie mam" = 0, "brak" = 0)
  ocena_map <- c("7" = 7, "6" = 6, "4" = 4, "9" = 9, "7.5" = 7.5,
                 "dobry" = 7, "bardzo dobry" = 9, "średni" = 5, "B+" = 7, "8/10" = 8)

  output$tab7_table <- renderUI({
    dd_data_table(round_df(reg_data), page_size = 10, page = input$tab7_table_page, page_input = "tab7_table_page")
  })

  output$tab7_mean_result <- renderUI({
    req(input$tab7_mean > 0)
    isolate({
      var  <- input$tab7_var
      vals <- reg_data[[var]]

      if (var == "wiek") {
        lc_status(
          lc_verdict(type = "ok", "Średnia wieku (lata):"),
          paste0(" ", round(mean(vals, na.rm = TRUE), 1), ". Wszystkie wartości są liczbami.")
        )
      } else if (var == "wyksztalcenie") {
        lc_status(
          lc_verdict(type = "info", "Średniej nie da się policzyć:"),
          " to zmienna jakościowa z czterema spójnymi kategoriami."
        )
      } else {
        nums  <- safe_numeric(vals)
        n_na  <- sum(is.na(nums))
        pct_na <- round(n_na / length(nums) * 100, 1)

        if (n_na == 0) {
          lc_status(paste0("Średnia: ", round(mean(nums, na.rm = TRUE), 2)))
        } else {
          lc_status(
            lc_verdict(type = "danger",
              paste0(n_na, " z ", length(nums), " wartości (", pct_na, "%) nie da się odczytać jako liczby.")),
            tags$br(),
            "Przykłady: ",
            paste(head(vals[is.na(nums)], 5), collapse = ", ")
          )
        }
      }
    })
  })

  output$tab7_clean_table <- renderUI({
    if (input$tab7_toggle == "Surowe") {
      dd_data_table(round_df(reg_data), page_size = 8, page = input$tab7_clean_table_page, page_input = "tab7_clean_table_page")
    } else {
      clean <- data.frame(
        wiek             = reg_data$wiek,
        wyksztalcenie    = reg_data$wyksztalcenie,
        doswiadczenie_lat = as.numeric(dosw_map[reg_data$doswiadczenie]),
        ocena_1_10       = as.numeric(ocena_map[reg_data$ocena_umiejetnosci]),
        stringsAsFactors = FALSE
      )
      dd_data_table(round_df(clean), page_size = 8, page = input$tab7_clean_table_page, page_input = "tab7_clean_table_page")
    }
  })

  output$tab7_clean_info <- renderUI({
    if (input$tab7_toggle == "Oczyszczone") {
      n       <- nrow(reg_data)
      dosw_ok <- sum(!is.na(dosw_map[reg_data$doswiadczenie]))
      oc_num  <- sum(!is.na(safe_numeric(reg_data$ocena_umiejetnosci)))
      oc_frac <- sum(reg_data$ocena_umiejetnosci == "8/10")
      oc_word <- n - oc_num - oc_frac
      lc_status(
        tags$strong("Zmiany:"),
        tags$br(), "wiek, wyksztalcenie: bez zmian",
        tags$br(), paste0("doswiadczenie_lat: ", dosw_ok, " z ", n,
                          " wpisów przekodowanych, braki: ", n - dosw_ok),
        tags$br(), paste0("ocena_1_10: ", oc_num, " liczb bez zmian, ", oc_frac,
                          " × „8/10” → 8, ", oc_word,
                          " ocen słownych przypisanych umownie („dobry” → 7, „B+” → 7)"),
        tags$br(), "dostepnosc: usunięta, nie da się zakodować"
      )
    }
  })



  tab7b_view <- reactiveVal("raw")
  observeEvent(input$tab7b_raw, {
    tab7b_view("raw")
    session$sendCustomMessage(type = "shinyjs-runjs", message = list(code =
      "$('#tab7b_raw').addClass('active'); $('#tab7b_cat').removeClass('active');"))
  })
  observeEvent(input$tab7b_cat, {
    tab7b_view("cat")
    session$sendCustomMessage(type = "shinyjs-runjs", message = list(code =
      "$('#tab7b_cat').addClass('active'); $('#tab7b_raw').removeClass('active');"))
  })

  output$tab7b_table <- renderUI({
    if (tab7b_view() == "raw") {
      dd_data_table(fixable_data, n = 12)
    } else {
      # Wiersze bez kategorii (NA) podświetlone jak w tekście pod tabelą.
      dd_data_table(fixable_data_cat, n = 12,
        cell_class = list(nauka_kat = ifelse(is.na(fixable_data_cat$nauka_kat),
                                             "is-target", NA)))
    }
  })
}
