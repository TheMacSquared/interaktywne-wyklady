# ============================================================================
# ROZDZIAŁ 4: Założenia KMNK
# ============================================================================

ch4_ui <- lecture_chapter(
  id = "ch-zalozenia",
  num = "04",
  title = "Założenia KMNK",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · KMNK",
      num = "04",
      title = "Założenia klasycznej MNK.",
      lead = "KMNK daje porządne, wiarygodne wyniki tylko wtedy, gdy spełnione są pewne warunki. Niżej lista kontrolna, do której będziemy wracać przy każdym kolejnym modelu."
    ),

    lc_h2("ch4-po-co-zalozenia", "Po co w ogóle założenia?"),
    lc_p("KMNK to procedura matematyczna — ona zawsze coś policzy, nawet jeśli dane są kompletnie pokręcone. Pytanie nie brzmi ‚czy się policzy‘, tylko ‚czy wynikom można ufać‘. Część założeń dotyczy samej linii regresji: czy nie pomijamy czegoś, co systematycznie przesuwa wynik. Inne dotyczą głównie niepewności: czy standardowe błędy, testy i przedziały ufności są liczone uczciwie."),

    lc_h2("ch4-lista", "Sześć założeń klasycznych"),
    figure_panel(
      label = "Lista kontrolna",
      title = "Założenia KMNK z przykładami łamania",
      tags$ol(
        tags$li(
          strong("1. Liniowość względem parametrów."),
          " Parametry β występują w równaniu liniowo (mnożone przez X, dodawane). Zmienne X mogą być dowolnie przekształcone — log, kwadrat, interakcje. ",
          em("Łamanie:"), " model Y = β₀ + β₁² · X jest nieliniowy względem β i KMNK go nie unie­sie."
        ),
        tags$li(
          strong("2. Błąd nie zależy systematycznie od X."),
          " Dla małych i dużych wartości X reszty powinny średnio krążyć wokół zera. ",
          em("Łamanie:"), " gdy pominęliśmy ważną zmienną powiązaną z X — jej wpływ wpada do ε i linia może przypisać X efekt, który naprawdę pochodzi z czegoś innego."
        ),
        tags$li(
          strong("3. Stała wariancja ε (homoskedastyczność)."),
          " Rozrzut składnika losowego jest taki sam dla wszystkich poziomów X. ",
          em("Łamanie:"), " typowe w danych firmowych — błąd przy małych firmach jest mały, przy korporacjach duży. To heteroskedastyczność."
        ),
        tags$li(
          strong("4. Brak autokorelacji ε."),
          " Składniki losowe różnych obserwacji są niezależne. ",
          em("Łamanie:"), " w szeregach czasowych dzisiejsze ε zależy od wczorajszego (np. zaszokowana sprzedaż wraca do normy stopniowo, a nie skokowo)."
        ),
        tags$li(
          strong("5. Brak ścisłej współliniowości X (w modelach wielorakich)."),
          " Żadna zmienna objaśniająca nie jest dokładną kombinacją liniową pozostałych. ",
          em("Łamanie:"), " jedna X jest sumą dwóch innych (np. wydatki = wydatki_marketing + wydatki_produkcja w jednym modelu) — model się nie policzy."
        ),
        tags$li(
          strong("6. Normalność reszt."),
          " Składnik losowy ma rozkład normalny. ",
          em("Ważne tylko przy małej próbie"), " (n < 30) i przy klasycznych testach istotności. Przy większych próbach centralne twierdzenie graniczne ratuje większość wniosków."
        )
      )
    ),

    inline_callout(
      label = "Wskazówka",
      color = "ok",
      "Najważniejszy warunek dla samej interpretacji współczynnika brzmi: reszty nie mogą mieć ukrytego wzorca powiązanego z X. Jeśli taki wzorzec jest, nachylenie może opisywać nie tylko wpływ X, ale też wpływ pominiętego czynnika. Homoskedastyczność, brak autokorelacji i normalność są szczególnie ważne dla niepewności: standardowych błędów, testów i przedziałów ufności."
    ),

    # ------------------------------------------------------------------------
    # Główny widget: suwak naruszenia
    # ------------------------------------------------------------------------
    lc_h2("ch4-suwak", "Suwak naruszenia — od idealnych danych do wyraźnego problemu"),
    lc_p("Książkowy przykład pokazuje albo stan idealny, albo karykaturę naruszenia. W realnych danych jest między tym wszystko, co możliwe. Ten widget pozwala płynnie przejść od ‚OK‘ przez ‚delikatne ostrzeżenie‘ do ‚wyraźnego problemu‘. Wybierz, które założenie testujemy, i przesuwaj suwak."),
    figure_panel(
      label = "Ryc. 4.1",
      title = "Diagnostyka graficzna z suwakiem naruszenia",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          radioButtons(
            "ch4_zalozenie", "Które założenie testujemy?",
            choices = c(
              "Homoskedastyczność (stała wariancja)" = "hetero",
              "Liniowość (X→Y prosta)"               = "nieliniowosc",
              "Brak obserwacji odstających"          = "outliery"
            ),
            selected = "hetero"
          ),
          sliderInput("ch4_naruszenie", "Poziom naruszenia (%)",
                      min = 0, max = 100, value = 0, step = 5),
          tags$br(),
          uiOutput("ch4_legenda")
        ),
        column(
          8,
          plotOutput("ch4_suwak_plot", height = "380px"),
          uiOutput("ch4_suwak_verdict")
        )
      )
    ),
    inline_callout(
      label = "Wskazówka",
      color = "ok",
      "Granice nie są ostre. ‚Lekkie ostrzeżenie‘ przy 30% to subiektywna ocena — różni statystycy mogą się tu różnić. Liczy się to, czy potrafisz powiedzieć: w jakim kierunku idzie problem, jak duży jest, i co z tym dalej zrobić."
    ),

    # ------------------------------------------------------------------------
    # Drugi widget: szybkie cztery scenariusze (zachowane)
    # ------------------------------------------------------------------------
    lc_h2("ch4-wizualnie", "Cztery skrajne scenariusze — szybkie porównanie"),
    lc_p("Po prześlizgnięciu się suwakiem warto zobaczyć też cztery klasyczne sytuacje obok siebie — w tym taką, która mieści ", em("dwa"),  " problemy naraz (obserwacje odstające)."),
    figure_panel(
      label = "Ryc. 4.2",
      title = "Diagnostyka graficzna — cztery scenariusze",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          selectInput(
            "ch4_kind", "Scenariusz danych:",
            choices = c(
              "OK — wszystko gra"            = "ok",
              "Heteroskedastyczność"         = "hetero",
              "Nieliniowość"                 = "nonlinear",
              "Obserwacje odstające"         = "outliers"
            ),
            selected = "ok"
          ),
          lc_p("Lewy panel: dane z dopasowaną prostą KMNK. Prawy panel: reszty wobec wartości dopasowanych — to tu najszybciej widać kłopoty.")
        ),
        column(
          8,
          plotOutput("ch4_plot", height = "320px"),
          uiOutput("ch4_verdict")
        )
      )
    ),

    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      "Łamanie założeń ", strong("nie unieważnia"), " od razu modelu — najczęściej znaczy tylko, że trzeba go poprawić. Heteroskedastyczność leczymy odpornymi błędami (HC), nieliniowość — transformacją X albo dodatkowym członem, autokorelację — modelem dynamicznym. Diagnostyka jest zaproszeniem do pracy, nie wyrokiem."
    ),

    lc_chapter_next(
      num = "05",
      title = "Czytanie wyników",
      lead = "ćwiczenie z interpretacji modelu",
      target_id = "ch-cwiczenie"
    )
  )
)

ch4_server <- function(input, output, session) {

  # --- Główny widget: suwak naruszenia -------------------------------------

  ch4_suwak_data <- reactive({
    eco02_naruszenie_data(
      zalozenie = input$ch4_zalozenie,
      level     = input$ch4_naruszenie,
      n         = 100,
      seed      = 11
    )
  })

  ch4_suwak_fit <- reactive(lm(y ~ x, data = ch4_suwak_data()))

  output$ch4_legenda <- renderUI({
    nazwa <- switch(input$ch4_zalozenie,
      hetero       = "Homoskedastyczność: rozrzut reszt nie zależy od X",
      nieliniowosc = "Liniowość: zależność Y od X jest prosta, nie zakrzywiona",
      outliery     = "Brak outlierów: wszystkie obserwacje pochodzą z tej samej populacji"
    )
    co_widac <- switch(input$ch4_zalozenie,
      hetero       = "Przy 0% rozrzut jest stały. Z każdym przesunięciem suwaka rośnie ‚lejek‘ — wariancja reszt staje się coraz silniejszą funkcją X.",
      nieliniowosc = "Przy 0% relacja jest dokładnie prosta. Wraz z suwakiem dane zaczynają się wyginać w łuk — model liniowy przestaje być adekwatny.",
      outliery     = "Przy 0% punkty leżą równomiernie wokół linii. Suwak dodaje rosnącą liczbę skrajnych obserwacji odbiegających od reszty."
    )
    tagList(
      tags$p(strong("Założenie: "), nazwa),
      tags$p(co_widac)
    )
  })

  output$ch4_suwak_plot <- renderPlot({
    d <- ch4_suwak_data()
    d$fitted <- fitted(ch4_suwak_fit())
    d$resid  <- resid(ch4_suwak_fit())

    p1 <- ggplot(d, aes(x, y)) +
      geom_point(color = unname(upwr_cat["grafit"]), alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = upwr_accent, linewidth = 1) +
      labs(x = "X", y = "Y", title = "Rozrzut z prostą KMNK") +
      theme_upwr()

    p2 <- ggplot(d, aes(fitted, resid)) +
      geom_hline(yintercept = 0, color = unname(upwr_cat["grafit"]), linetype = "dashed") +
      geom_point(color = unname(upwr_cat["terakota"]), alpha = 0.75, size = 2) +
      geom_smooth(method = "loess", se = FALSE, color = unname(upwr_cat["niebo"]),
                  linewidth = 0.9) +
      labs(x = "Wartości dopasowane ŷ", y = "Reszty e",
           title = "Reszty vs ŷ — detektor naruszeń") +
      theme_upwr()

    if (requireNamespace("patchwork", quietly = TRUE)) {
      patchwork::wrap_plots(p1, p2, ncol = 2)
    } else {
      p2
    }
  })

  output$ch4_suwak_verdict <- renderUI({
    werdykt <- eco02_werdykt_naruszenie(input$ch4_zalozenie, input$ch4_naruszenie)
    nazwa_status <- switch(werdykt$type,
      ok      = "Założenie spełnione",
      warning = "Lekkie ostrzeżenie",
      danger  = "Wyraźny problem"
    )

    tagList(
      lc_stat_grid(
        lc_stat_box(label = "Poziom naruszenia",
                    value = paste0(input$ch4_naruszenie, "%"),
                    caption = "ile na suwaku",
                    color = upwr_secondary),
        lc_stat_box(label = "Status",
                    value = nazwa_status,
                    caption = paste0("typ: ", werdykt$type),
                    color = switch(werdykt$type,
                                   ok      = unname(upwr_cat["szalwia"]),
                                   warning = unname(upwr_cat["terakota"]),
                                   danger  = upwr_accent)),
        columns = 2
      ),
      lc_feedback(
        type = werdykt$type,
        tags$p(werdykt$opis),
        if (werdykt$type != "ok")
          tags$p(strong("Co dalej: "), werdykt$rekomendacja)
        else NULL
      )
    )
  })

  # --- Drugi widget: cztery skrajne scenariusze (zachowane) ----------------

  ch4_df <- reactive({
    eco_diagnostic_data(kind = input$ch4_kind, n = 120, seed = 44)
  })
  ch4_fit <- reactive(lm(y ~ x, data = ch4_df()))

  output$ch4_plot <- renderPlot({
    d <- ch4_df()
    d$fitted <- fitted(ch4_fit())
    d$resid  <- resid(ch4_fit())

    p1 <- ggplot(d, aes(x, y)) +
      geom_point(color = unname(upwr_cat["grafit"]), alpha = 0.65, size = 1.8) +
      geom_smooth(method = "lm", se = FALSE, color = upwr_accent, linewidth = 1) +
      labs(x = "X", y = "Y", title = "Rozrzut z prostą KMNK") +
      theme_upwr()

    p2 <- ggplot(d, aes(fitted, resid)) +
      geom_hline(yintercept = 0, color = unname(upwr_cat["grafit"]), linetype = "dashed") +
      geom_point(color = unname(upwr_cat["terakota"]), alpha = 0.7, size = 1.8) +
      labs(x = "Wartości dopasowane ŷ", y = "Reszty e",
           title = "Reszty vs ŷ — diagnostyka") +
      theme_upwr()

    if (requireNamespace("patchwork", quietly = TRUE)) {
      patchwork::wrap_plots(p1, p2, ncol = 2)
    } else {
      p2
    }
  })

  output$ch4_verdict <- renderUI({
    kind <- input$ch4_kind
    if (kind == "ok") {
      lc_feedback(
        type = "ok",
        strong("Wygląda dobrze. "),
        "Reszty rozkładają się losowo wokół zera, bez wyraźnego wzoru. Wariancja reszt nie zmienia się z ŷ, nie ma łuku ani lejka. KMNK na takich danych daje wiarygodne oszacowania i sensowne testy istotności."
      )
    } else if (kind == "hetero") {
      lc_feedback(
        type = "warning",
        strong("Heteroskedastyczność. "),
        "Rozrzut reszt rozszerza się wraz ze wzrostem wartości dopasowanych — typowy ‚lejek‘. Wariancja składnika losowego nie jest stała. Skutek: oszacowania b₀ i b₁ pozostają nieobciążone, ale standardowe błędy są nietrafne, więc testy istotności mogą zwodzić. Lekarstwo: odporne błędy standardowe (HC0, HC1) albo ważona MNK."
      )
    } else if (kind == "nonlinear") {
      lc_feedback(
        type = "warning",
        strong("Nieliniowość relacji. "),
        "Reszty układają się w łuk — model liniowy systematycznie myli się w okolicach środka i krańców. Prawdziwa relacja nie jest prostą. Lekarstwo: dodać człon kwadratowy, zlogarytmować zmienną albo zastosować inny model funkcyjny."
      )
    } else {
      lc_feedback(
        type = "warning",
        strong("Obserwacje odstające. "),
        "W chmurze widać kilka punktów oderwanych od reszty — i to one ciągną prostą za sobą. Sprawdź, czy nie są to błędy w danych albo wyjątkowe okoliczności. Można je usunąć (z uzasadnieniem!), zastosować regresję odporną albo modelować je osobno."
      )
    }
  })
}
