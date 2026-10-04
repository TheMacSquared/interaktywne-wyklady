# ============================================================================
# CHAPTER 1: Typy danych
# ============================================================================

ch1_ui <- list(
  id = "ch-typy", num = "01", title = "Typy danych",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 01 · Statystyka opisowa",
      num    = "01",
      title  = "Typy danych.",
      lead   = "Kody pocztowe są liczbami, a mimo to średnia z nich nic nie znaczy.
               Komputer policzy ją bez protestu, więc to my musimy wiedzieć,
               jakiego rodzaju informację niesie zmienna. Od tego zależy, co wolno
               z nią zrobić: jakie liczby obliczyć i jaki wykres narysować."
    ),

    lc_p("Kod 50-375 nie jest „większy” od kodu 00-950 w żadnym sensownym
      znaczeniu. Cyfry służą tu tylko za etykiety, które odróżniają jeden obszar
      od drugiego. Gdybyśmy policzyli ", gloss("średnia", "średnią"), " kodów
      pocztowych wszystkich studentów, dostalibyśmy liczbę, która nie wskazuje
      żadnego miejsca na mapie."),

    lc_p("Ten sam problem pojawia się w każdym zbiorze danych, w którym kategorie
      zakodowano liczbami. W ankiecie, którą będziemy analizować przez cały kurs,
      jest pytanie o kierunek studiów. Gdyby Biologię zapisać jako 1, Ekonomię
      jako 2, Informatykę jako 3, a Psychologię jako 4, „średni kierunek” 200
      ankietowanych wyniósłby 2.45 — coś między Ekonomią a Informatyką, czyli nic.
      Zanim więc cokolwiek policzymy, musimy rozpoznać typ każdej ", gloss("zmienna", "zmiennej"), "."),

    # --- Widget 1: Taxonomy tree ---
    lc_h2("ch1-taksonomia", "Taksonomia typów danych"),

    lc_p("Pierwszy podział dotyczy tego, czym są wartości zmiennej. ",
      gloss("zmienna ilościowa", "Zmienne ilościowe"), " przyjmują liczby, na
      których działania arytmetyczne mają sens: wzrost 180 cm to o 10 cm więcej
      niż 170 cm. ", gloss("zmienna jakościowa", "Zmienne jakościowe"), "
      przyjmują kategorie, takie jak płeć czy kierunek studiów, nawet jeśli
      w pliku zapisano je cyframi."),

    lc_p("Każda z tych grup dzieli się jeszcze na dwie. Wśród zmiennych
      ilościowych ", gloss("zmienna dyskretna", "zmienne dyskretne"), " przyjmują
      wartości policzalne, zwykle całkowite (liczba kursów, liczba nieobecności),
      a ", gloss("zmienna ciągła", "zmienne ciągłe"), " mogą przyjąć dowolną
      wartość z przedziału, także ułamkową (wzrost, czas dojazdu). Wśród
      zmiennych jakościowych ", gloss("zmienna porządkowa", "zmienne porządkowe"), "
      mają kategorie ułożone w naturalnej kolejności (zadowolenie od „bardzo
      niezadowolony” do „bardzo zadowolony”), a ",
      gloss("zmienna nominalna", "zmienne nominalne"), " takiej kolejności nie mają
      (płeć, grupa krwi). Diagram zbiera ten podział w jedno drzewo; liście
      drzewa pokazują przykłady z naszej ankiety."),

    figure_panel(
      label = "Ryc. 1.1",
      title = "Taksonomia typów danych",
      div(class = "taxonomy-tree",
        tags$ul(
          tags$li(
            div(class = "tax-node", "Dane"),
            tags$ul(
              tags$li(
                div(class = "tax-node", HTML("Ilościowe<br><small>(liczbowe)</small>")),
                tags$ul(
                  tags$li(
                    div(class = "tax-leaf", id = "ch1_leaf_ciagla",
                      style = paste0("background:", type_colors["ilosciowa_ciagla"], ";"),
                      onclick = "Shiny.setInputValue('ch1_leaf_click', 'ciagla', {priority:'event'})",
                      "Ciągłe"
                    )
                  ),
                  tags$li(
                    div(class = "tax-leaf", id = "ch1_leaf_dyskretna",
                      style = paste0("background:", type_colors["ilosciowa_dyskretna"], ";"),
                      onclick = "Shiny.setInputValue('ch1_leaf_click', 'dyskretna', {priority:'event'})",
                      "Dyskretne"
                    )
                  )
                )
              ),
              tags$li(
                div(class = "tax-node", HTML("Jakościowe<br><small>(kategoryczne)</small>")),
                tags$ul(
                  tags$li(
                    div(class = "tax-leaf", id = "ch1_leaf_porzadkowa",
                      style = paste0("background:", type_colors["porzadkowa"], ";"),
                      onclick = "Shiny.setInputValue('ch1_leaf_click', 'porzadkowa', {priority:'event'})",
                      "Porządkowe"
                    )
                  ),
                  tags$li(
                    div(class = "tax-leaf", id = "ch1_leaf_nominalna",
                      style = paste0("background:", type_colors["nominalna"], ";"),
                      onclick = "Shiny.setInputValue('ch1_leaf_click', 'nominalna', {priority:'event'})",
                      "Nominalne"
                    )
                  )
                )
              )
            )
          )
        )
      ),
      lc_caption("Kliknij kolorowy liść drzewa, aby zobaczyć przykłady."),
      uiOutput("ch1_leaf_detail")
    ),

    lc_p("Typy różnią się tym, ile operacji na danych ma sens. Kategorie nominalne
      możemy tylko policzyć: ile osób ma grupę krwi A, a ile B. Porządkowe możemy
      dodatkowo uporządkować, więc ma sens pytanie, kto jest bardziej zadowolony.
      Dopiero na zmiennych ilościowych wolno dodawać i odejmować, a więc także
      liczyć średnią. Kody pocztowe i zakodowany cyframi kierunek studiów są
      zmiennymi nominalnymi, dlatego średnia z nich nie miała sensu."),

    lc_p("Granica między typami nie zawsze jest ostra. Ocena wykładowcy w skali
      1–10 jest formalnie zmienną porządkową: nie wiemy, czy różnica między
      oceną 9 a 10 jest taka sama jak między 2 a 3. Przy dziesięciu stopniach
      skali często traktuje się ją jednak jak zmienną dyskretną i liczy średnią.
      To decyzja analityka, którą trzeba podjąć świadomie i uzasadnić."),

    lc_note("Zasada", rule = TRUE,
      "Przed rozpoczęciem analizy określ typ każdej zmiennej."
    ),

    # --- Widget 2: Examples gallery ---
    lc_h2("ch1-przyklady", "Przykłady typów zmiennych"),

    lc_p("Typ zmiennej decyduje nie tylko o tym, co wolno policzyć, ale też o tym,
      jak dane pokazać. Poniżej każdy z czterech typów reprezentuje jedna
      zmienna z ankiety, narysowana na wykresie, który do niej pasuje.
      Przełącznik nad wykresami zamienia je na wykresy źle dobrane do typu."),

    figure_panel(
      label = "Ryc. 1.2",
      title = "Cztery typy zmiennych — jeden wykres na typ",
      lc_toolbar(
        checkboxInput("ch1_show_bad", "Pokaż źle dobrane wykresy", value = FALSE),
        lc_step_nav("ch1_ex", c("Płeć", "Zadowolenie", "Liczba kursów", "Wzrost"), start = 1L)
      ),
      # Jeden typ zmiennej na slajd; kropki przełączają slajdy.
      conditionalPanel("input.ch1_ex == 1",
        tags$h4("Płeć · jakościowa nominalna"),
        tags$p(
          "Kategorie bez naturalnego porządku. Możemy liczyć, ile jest
          obserwacji w każdej kategorii, ale nie możemy ich uporządkować
          ani uśredniać."
        ),
        lc_plot("ch1_ex1_plot", ratio = "2/1", max_height = "340px")
      ),
      conditionalPanel("input.ch1_ex == 2",
        tags$h4("Zadowolenie ze studiów · jakościowa porządkowa"),
        tags$p(
          "Kategorie z naturalnym porządkiem. Wiemy, że „Bardzo zadowolony”
          jest wyżej niż „Zadowolony”, ale nie znamy dokładnych odległości
          między kategoriami."
        ),
        lc_plot("ch1_ex2_plot", ratio = "2/1", max_height = "340px")
      ),
      conditionalPanel("input.ch1_ex == 3",
        tags$h4("Liczba kursów · ilościowa dyskretna"),
        tags$p(
          "Wartości liczbowe, ale tylko całkowite. Możemy obliczać średnią
          i ", gloss("odchylenie standardowe"), ". ", gloss("wykres słupkowy", "Wykres słupkowy"), " jest tu odpowiedni,
          bo mamy skończoną liczbę wartości."
        ),
        lc_plot("ch1_ex3_plot", ratio = "2/1", max_height = "340px")
      ),
      conditionalPanel("input.ch1_ex == 4",
        tags$h4("Wzrost (cm) · ilościowa ciągła"),
        tags$p(
          "Wartości liczbowe, które mogą przyjmować dowolne wartości
          z pewnego przedziału (także ułamkowe). ", gloss("histogram", "Histogram"), " grupuje
          wartości w przedziały, gęstość wygładza rozkład."
        ),
        lc_plot("ch1_ex4_plot", ratio = "2/1", max_height = "340px")
      )
    ),

    lc_p("Dla obu zmiennych jakościowych właściwy jest wykres słupkowy z liczebnością
      każdej kategorii. W ankiecie jest 109 kobiet i 91 mężczyzn; najczęstsza
      odpowiedź o zadowolenie to „Neutralny” (70 osób), a skrajne
      „Bardzo niezadowolony” wybrało tylko 7. Przy zmiennej porządkowej słupki
      stoją w kolejności kategorii, przy nominalnej kolejność jest umowna.
      Liczba kursów przyjmuje tylko siedem wartości, od 3 do 9, więc również
      tu każda wartość dostaje własny słupek. Wzrost to inna sytuacja: wśród
      200 pomiarów jest 148 różnych wartości, od 150 do 191.2 cm. Słupek dla
      każdej z nich miałby wysokość od 1 do 4, dlatego histogram łączy wartości
      w przedziały i dopiero wtedy widać kształt rozkładu."),

    lc_p("Źle dobrane wykresy pokazują oba kierunki błędu. Płeć i zadowolenie
      zakodowane jako liczby trafiają na histogram, który sugeruje ciągłą oś
      wartości, choć między kategoriami nie ma nic pośrodku, a etykiety
      kategorii znikają. Liczba kursów pokazana jako gładka krzywa gęstości
      sugeruje, że ktoś może mieć 4.5 kursu, choć zmienna przyjmuje tylko
      wartości całkowite. Wzrost na wykresie słupkowym rozpada się na 148 cienkich
      kresek, z których nie da się odczytać kształtu rozkładu. Wykres nie naprawi
      złego rozpoznania typu, tylko je utrwali."),

    # --- Widget 4: Dataset preview ---
    lc_h2("ch1-dane", "Nasze dane — ankieta studencka"),

    lc_p("Wszystkie przykłady w tym wykładzie pochodzą z jednego zbioru: ankiety,
      w której 200 studentów odpowiedziało na 12 pytań. Każdy wiersz tabeli to
      jeden student, każda kolumna to jedna zmienna. Poniżej pierwsze 10
      wierszy."),

    figure_panel(
      label = "Ryc. 1.3",
      title = "Pierwsze 10 obserwacji — nasz zbiór danych",
      uiOutput("ch1_data_preview")
    ),

    lc_p("W tabeli są zmienne wszystkich czterech typów. Płeć, kierunek i grupa
      krwi są nominalne. Rok studiów, zadowolenie i ocena wykładowcy są
      porządkowe. Liczba kursów i liczba nieobecności są dyskretne, a wzrost,
      waga, czas dojazdu i średnia ocen — ciągłe. Warto zwrócić uwagę na rok
      studiów i ocenę wykładowcy: w tabeli wyglądają jak liczby, ale opisują
      uporządkowane kategorie. To ta sama pułapka co z kodami pocztowymi."),

    lc_p("Kolejne rozdziały idą według typów. Najpierw podsumujemy zmienne
      jakościowe, potem ilościowe: ich położenie, rozrzut i kształt
      rozkładu. Panel poniżej pozwala wybrać jedną zmienną ilościową, która
      będzie towarzyszyć tym rozdziałom."),

    # --- Variable tracker selector ---
    figure_panel(
      label = "Narzędzie",
      title = "Śledź zmienną przez cały kurs",
      color = upwr_single_alt,
      p(
        "Wybierz jedną zmienną ilościową. Każdy kolejny rozdział pokaże,
         jakie nowe informacje dają kolejne narzędzia statystyczne zastosowane
         do tej samej zmiennej."),
      lc_toolbar(selectInput("tracked_var", "Zmienna do śledzenia",
        choices = c("Wzrost (cm)" = "wzrost",
                    "Waga (kg)" = "waga",
                    "Czas dojazdu (min)" = "czas_dojazdu",
                    "Średnia ocen" = "srednia_ocen"),
        selected = "wzrost"
          ))
    ),

    lc_chapter_next(
      num       = "02",
      title     = "Zmienne jakościowe",
      lead      = "Jak podsumować kategorie — są prostsze i stanowią naturalny punkt wyjścia.",
      target_id = "ch-jakosciowe"
    ),

    # Bottom spacing
    lc_spacer("md")

  )
)

# --------------------------------------------------------------------------
# Chapter 1 Server
# --------------------------------------------------------------------------

ch1_server <- function(input, output, session) {

  ch1_selected_leaf <- reactiveVal(NULL)

  # --- Widget 1: Taxonomy tree (HTML) ---

  .leaf_info <- list(
    ciagla = list(
      label = "Ciągłe", color = type_colors["ilosciowa_ciagla"],
      desc = "Wartości liczbowe, które mogą przyjmować dowolną wartość z przedziału (także ułamkowe).",
      examples = "wzrost (cm), waga (kg), czas dojazdu (min), średnia ocen"
    ),
    dyskretna = list(
      label = "Dyskretne", color = type_colors["ilosciowa_dyskretna"],
      desc = "Wartości liczbowe, ale tylko całkowite — można je policzyć.",
      examples = "liczba kursów, liczba nieobecności"
    ),
    porzadkowa = list(
      label = "Porządkowe", color = type_colors["porzadkowa"],
      desc = "Kategorie z naturalnym porządkiem, ale odległości między nimi nie są znane.",
      examples = "rok studiów (1 < 2 < 3 < ...), zadowolenie ze studiów, ocena wykładowcy"
    ),
    nominalna = list(
      label = "Nominalne", color = type_colors["nominalna"],
      desc = "Kategorie bez naturalnego porządku — można je tylko liczyć.",
      examples = "płeć, kierunek studiów, grupa krwi"
    )
  )

  observeEvent(input$ch1_leaf_click, {
    leaf_id <- input$ch1_leaf_click
    if (identical(ch1_selected_leaf(), leaf_id)) {
      ch1_selected_leaf(NULL)
    } else {
      ch1_selected_leaf(leaf_id)
    }
  })

  output$ch1_leaf_detail <- renderUI({
    sel <- ch1_selected_leaf()
    if (is.null(sel)) return(NULL)
    info <- .leaf_info[[sel]]
    # Znacznik w kolorze typu, jak liść drzewa.
    lc_status(
      p(tags$i(class = "lc-th-swatch", style = paste0("--lc-sw:", info$color)),
        b_(info$label), " ", info$desc),
      lc_caption(tagList(tags$em("W naszych danych: "), info$examples))
    )
  })

  # --- Widget 2: Examples gallery ---

  zoom_plot_server("ch1_ex1_plot", reactive({
    if (input$ch1_show_bad) {
      render_bad_plot(student_data$plec, "Płeć", "nominalna")
    } else {
      render_good_plot(student_data$plec, "Płeć", "nominalna")
    }
  }))

  zoom_plot_server("ch1_ex2_plot", reactive({
    if (input$ch1_show_bad) {
      render_bad_plot(student_data$zadowolenie, "Zadowolenie", "porzadkowa")
    } else {
      render_good_plot(student_data$zadowolenie, "Zadowolenie", "porzadkowa")
    }
  }))

  zoom_plot_server("ch1_ex3_plot", reactive({
    if (input$ch1_show_bad) {
      render_bad_plot(student_data$liczba_kursow, "Liczba kursów", "ilosciowa_dyskretna")
    } else {
      render_good_plot(student_data$liczba_kursow, "Liczba kursów", "ilosciowa_dyskretna")
    }
  }))

  zoom_plot_server("ch1_ex4_plot", reactive({
    if (input$ch1_show_bad) {
      render_bad_plot(student_data$wzrost, "Wzrost (cm)", "ilosciowa_ciagla")
    } else {
      render_good_plot(student_data$wzrost, "Wzrost (cm)", "ilosciowa_ciagla")
    }
  }))

  # --- Widget 4: Dataset preview ---

  output$ch1_data_preview <- renderUI({
    lc_table(head(student_data, 10), scroll = TRUE, sticky_first = TRUE,
             label = "Pierwsze 10 obserwacji ankiety")
  })

}
