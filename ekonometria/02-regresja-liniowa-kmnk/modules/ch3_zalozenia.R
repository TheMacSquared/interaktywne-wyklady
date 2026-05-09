# ============================================================================
# ROZDZIAŁ 3: Założenia KMNK
# ============================================================================

ch3_ui <- lecture_chapter(
  id = "ch-zalozenia",
  num = "03",
  title = "Założenia KMNK",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · KMNK",
      num = "03",
      title = "Założenia klasycznej MNK.",
      lead = "KMNK daje porządne, wiarygodne wyniki tylko wtedy, gdy spełnione są pewne warunki. Niżej lista kontrolna, do której będziemy wracać przy każdym kolejnym modelu."
    ),

    lc_h2("ch3-po-co-zalozenia", "Po co w ogóle założenia?"),
    lc_p("KMNK to procedura matematyczna — ona zawsze coś policzy, nawet jeśli dane są kompletnie pokręcone. Pytanie nie brzmi ‚czy się policzy‘, tylko ‚czy wynikom można ufać‘. Założenia klasyczne to warunki, przy których wzory na błędy standardowe, testy istotności i przedziały ufności mają sens. Gdy są łamane, oszacowania mogą być obciążone albo testy mogą prowadzić w pole."),

    lc_h2("ch3-lista", "Sześć założeń klasycznych"),
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
          strong("2. Średnia ε równa zero."),
          " Składnik losowy nie ma systematycznego biasu w żadnym kierunku. ",
          em("Łamanie:"), " gdy pominęliśmy ważną zmienną dodatnio skorelowaną z X — wpadnie do ε i przesunie jego średnią."
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
      "Pierwsze cztery założenia (liniowość, zerowa średnia ε, homoskedastyczność, brak autokorelacji) wpływają na to, czy oszacowania są ", strong("nieobciążone i efektywne"), ". Założenie o normalności wpływa głównie na wnioskowanie statystyczne (testy, przedziały ufności). To rozróżnienie wraca w każdym podręczniku ekonometrii."
    ),

    lc_h2("ch3-wizualnie", "Jak rozpoznać złamanie założeń?"),
    lc_p("Najprostsze diagnostyki to dwa wykresy: rozrzut Y vs X z dopasowaną prostą oraz reszty vs wartości dopasowane. Drugi z nich jest najczulszym detektorem heteroskedastyczności i nieliniowości — jeśli na nim widać wzór (lejek, łuk, klastry), coś jest nie tak."),
    figure_panel(
      label = "Ryc. 3.1",
      title = "Diagnostyka graficzna — cztery scenariusze",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          selectInput(
            "ch3_kind", "Scenariusz danych:",
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
          plotOutput("ch3_plot", height = "360px"),
          uiOutput("ch3_verdict")
        )
      )
    ),

    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      "Łamanie założeń ", strong("nie unieważnia"), " od razu modelu — najczęściej znaczy tylko, że trzeba go poprawić. Heteroskedastyczność leczymy odpornymi błędami (HC), nieliniowość — transformacją X albo dodatkowym członem, autokorelację — modelem dynamicznym. Diagnostyka jest zaproszeniem do pracy, nie wyrokiem."
    ),

    lc_chapter_next(
      num = "04",
      title = "Czytanie wyników",
      lead = "ćwiczenie z interpretacji modelu",
      target_id = "ch-cwiczenie"
    )
  )
)

ch3_server <- function(input, output, session) {
  ch3_df <- reactive({
    eco_diagnostic_data(kind = input$ch3_kind, n = 120, seed = 44)
  })
  ch3_fit <- reactive(lm(y ~ x, data = ch3_df()))

  output$ch3_plot <- renderPlot({
    d <- ch3_df()
    d$fitted <- fitted(ch3_fit())
    d$resid  <- resid(ch3_fit())

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

  output$ch3_verdict <- renderUI({
    kind <- input$ch3_kind
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
