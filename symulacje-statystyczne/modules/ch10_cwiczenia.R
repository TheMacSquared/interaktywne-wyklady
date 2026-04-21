# ============================================================================
# CHAPTER 10: Cwiczenia praktyczne
# ============================================================================

# ============================================================================
# UI
# ============================================================================

ch10_ui <- tabPanel("10. Ćwiczenia",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Czas zastosować metody resamplingowe na danych z Twojego kierunku."
    ),

    div(class = "section-title", "Ćwiczenia — metody resamplingowe"),

    div(class = "narrative",
      p(tags$b("Czas trwania:"), " ~ 90 minut · ",
        tags$b("Narzędzie:"), " Jamovi (z pakietem bootstrap/permutacje)"),
      p("Trzy bloki zadań na kierunek: bootstrap CI, test permutacyjny
         i myślenie krytyczne (kiedy/dlaczego). Każde zadanie ma ",
        "ukryte rozwiązanie.")
    ),

    div(class = "callout-info",
      selectInput("ch10_kierunek", tags$b("Wybierz wariant dla kierunku:"),
        choices = list(
          "Rolnictwo"                        = "rol",
          "Technologia Żywności"             = "zyw",
          "Inżynieria Bezpieczeństwa (BHP)" = "bhp",
          "Edukacja / Nauki Społeczne"          = "edu"
        ),
        selected = "rol",
        width = "100%"
      )
    ),

    uiOutput("ch10_content"),

    br(), br(), br()

  ))
)

# ============================================================================
# TRESC ZADAN — per kierunek
# ============================================================================

.ch10_rol <- function() tagList(

  div(class = "callout-info",
    p(tags$b("Dane: "), tags$code("plony_nawoz"), " — wygeneruj w R lub Jamovi:"),
    tags$pre(style = "font-size:12px;",
"set.seed(42)
df <- data.frame(
  nawoz      = sample(c('A','B'), 28, replace=TRUE),
  plon_dt_ha = round(rgamma(28, 2.5, scale=18), 1),
  odmiana    = sample(c('Typ1','Typ2','Typ3'), 28, replace=TRUE)
)"),
    p("Dane: n = 28, plony w dt/ha (silnie prawoskętne).")
  ),

  div(class = "section-title", "Blok 1: Bootstrap CI"),

  div(class = "widget-block",
    h4("Zadanie 1 — Mediana plonu"),
    div(class = "narrative",
      p("Wyznacz 95% bootstrap CI dla mediany plonu (zmienna ",
        tags$code("plon_dt_ha"), "). Dlaczego mediana, a nie średnia?"),
      p("Zanim klikniesz rozwiązanie: ",
        tags$em("zinterpretuj CI jednym zdaniem —
          co oznacza ten przedział w kontekście agrotechnicznym?"))
    ),
    actionButton("ch10_rol_ans1", "Pokaż rozwiązanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_rol_sol1")
  ),

  div(class = "section-title", "Blok 2: Test permutacyjny"),

  div(class = "widget-block",
    h4("Zadanie 2 — Nawóz A vs B"),
    div(class = "narrative",
      p("Czy nawóz A daje wyższe plony niż B?
         Wykonaj permutacyjny test różnicy średnich (B = 5000)."),
      p("Porównaj p-wartość z testu permutacyjnego z p-wartością z testu t.
         Jeśli się różnią, co to oznacza?")
    ),
    actionButton("ch10_rol_ans2", "Pokaż rozwiązanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_rol_sol2")
  ),

  div(class = "section-title", "Blok 3: Myślenie krytyczne"),

  div(class = "widget-block",
    h4("Zadanie 3 — Mała próba"),
    div(class = "narrative",
      p("Przeprowadzono 5 dodatkowych prób na mniejszych poletkach (n = 5)."),
      p(tags$em("Czy bootstrap CI pomoże z tak małą próbą?
                 Jakie są ograniczenia? Czy zaproponowany CI będzie wiarygodny?"))
    ),
    actionButton("ch10_rol_ans3", "Pokaż odpowiedź",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_rol_sol3")
  )

)

.ch10_zyw <- function() tagList(

  div(class = "callout-info",
    p(tags$b("Dane: "), tags$code("analiza_sensoryczna"), " — wygeneruj w R lub Jamovi:"),
    tags$pre(style = "font-size:12px;",
"set.seed(42)
df <- data.frame(
  produkt             = sample(c('A','B'), 35, replace=TRUE),
  ocena_tekstury      = pmin(7, pmax(1, round(rgamma(35, 2, 0.4)+1))),
  zawartosc_bialka    = round(rnorm(35, 18, 3.5), 1),
  czas_przechowywania = round(runif(35, 1, 90))
)"),
    p("Dane: n = 35, ocena tekstury na skali 1–7 (skośna), białko w g/100g.")
  ),

  div(class = "section-title", "Blok 1: Bootstrap CI"),

  div(class = "widget-block",
    h4("Zadanie 1 — Mediana oceny tekstury"),
    div(class = "narrative",
      p("Wyznacz 95% bootstrap CI dla mediany oceny tekstury produktu A.
         Porównaj z t-CI."),
      p(tags$em("Który CI jest właściwszy dla skali 1–7 przy n ≈ 18?
                 Uzasadnij."))
    ),
    actionButton("ch10_zyw_ans1", "Pokaż rozwiązanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_zyw_sol1")
  ),

  div(class = "section-title", "Blok 2: Test permutacyjny"),

  div(class = "widget-block",
    h4("Zadanie 2 — Produkt A vs B"),
    div(class = "narrative",
      p("Permutacyjny test różnicy ocen tekstury między produktem A i B.
         Porównaj z Mann-Whitney U."),
      p(tags$em("Dlaczego Mann-Whitney U i test permutacyjny mogą dać inne p-wartości?
                 Który testuje tę samą H₀ co t-test?"))
    ),
    actionButton("ch10_zyw_ans2", "Pokaż rozwiązanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_zyw_sol2")
  ),

  div(class = "section-title", "Blok 3: Myślenie krytyczne"),

  div(class = "widget-block",
    h4("Zadanie 3 — Korelacja białko – tekstura"),
    div(class = "narrative",
      p("Czy istnieje związek między zawartością białka a oceną tekstury?"),
      p(tags$em("Oblicz bootstrap CI dla współczynnika korelacji Pearsona.
                 Dlaczego tu wolimy bootstrap, a nie klasyczny test Pearsona?"))
    ),
    actionButton("ch10_zyw_ans3", "Pokaż odpowiedź",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_zyw_sol3")
  )

)

.ch10_bhp <- function() tagList(

  div(class = "callout-info",
    p(tags$b("Dane: "), tags$code("czas_reakcji_bhp"), " — wygeneruj w R lub Jamovi:"),
    tags$pre(style = "font-size:12px;",
"set.seed(42)
df <- data.frame(
  warunki         = sample(c('normalny','stres'), 22, replace=TRUE),
  czas_reakcji_ms = round(rlnorm(22, meanlog=5.6, sdlog=0.4)),
  bledy_count     = rpois(22, lambda=2.5)
)"),
    p("Dane: n = 22, czas reakcji w ms (log-normalny, silnie prawoskętny).")
  ),

  div(class = "section-title", "Blok 1: Bootstrap CI"),

  div(class = "widget-block",
    h4("Zadanie 1 — Mediana czasu reakcji"),
    div(class = "narrative",
      p("Wyznacz 95% bootstrap CI dla mediany czasu reakcji pracowników."),
      p(tags$em("Dlaczego bootstrap CI dla mediany — a nie t-CI dla średniej?
                 Jakie jest znaczenie praktyczne tego CI w kontekście BHP?"))
    ),
    actionButton("ch10_bhp_ans1", "Pokaż rozwiązanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_bhp_sol1")
  ),

  div(class = "section-title", "Blok 2: Test permutacyjny"),

  div(class = "widget-block",
    h4("Zadanie 2 — Stres vs normalne warunki"),
    div(class = "narrative",
      p("Czy stres istotnie wydłuża czas reakcji?
         Wykonaj permutacyjny test różnicy średnich (B = 5000)."),
      p(tags$em("Porównaj p-wartość z t-testem i Wilcoxonem.
                 Który test jest tu najodpowiedniejszy i dlaczego?"))
    ),
    actionButton("ch10_bhp_ans2", "Pokaż rozwiązanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_bhp_sol2")
  ),

  div(class = "section-title", "Blok 3: Myślenie krytyczne"),

  div(class = "widget-block",
    h4("Zadanie 3 — Norma bezpieczeństwa"),
    div(class = "narrative",
      p("Norma bezpieczeństwa: mediana czasu reakcji nie powinna przekraczać 250ms."),
      p(tags$em("Oblicz bootstrap CI dla mediany. Jeśli 250ms leży poza CI —
                 co wnioskujesz? Sformułuj wniosek „jak w raporcie BHP‟."))
    ),
    actionButton("ch10_bhp_ans3", "Pokaż odpowiedź",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_bhp_sol3")
  )

)

.ch10_edu <- function() tagList(

  div(class = "callout-info",
    p(tags$b("Dane: "), tags$code("wyniki_programu"), " — wygeneruj w R lub Jamovi:"),
    tags$pre(style = "font-size:12px;",
"set.seed(42)
df <- data.frame(
  program       = sample(c('tradycyjny','nowy'), 45, replace=TRUE),
  wynik_test    = pmin(100, pmax(0, round(rnorm(45, 68, 15)))),
  frekwencja_pct = round(pmin(100, pmax(50, 100-rexp(45, rate=0.04))),1),
  klasa         = sample(c('A','B','C'), 45, replace=TRUE)
)"),
    p("Dane: n = 45, wynik testu 0–100, frekwencja % (heavy tail).")
  ),

  div(class = "section-title", "Blok 1: Bootstrap CI"),

  div(class = "widget-block",
    h4("Zadanie 1 — Różnica wyników"),
    div(class = "narrative",
      p("Wyznacz 95% bootstrap CI dla różnicy średnich wyników
         (nowy program vs tradycyjny). Porównaj z t-CI Welcha."),
      p(tags$em("Czy oba CI są podobne? Co to mówi o rozkładzie danych?"))
    ),
    actionButton("ch10_edu_ans1", "Pokaż rozwiązanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_edu_sol1")
  ),

  div(class = "section-title", "Blok 2: Test permutacyjny"),

  div(class = "widget-block",
    h4("Zadanie 2 — Skuteczność nowego programu"),
    div(class = "narrative",
      p("Permutacyjny test różnicy wyników między programami.
         Porównaj p-wartość z testem t i Mann-Whitney U."),
      p(tags$em("Który test testuje H₀: brak różnicy średnich — test permutacyjny czy Mann-Whitney?
                 Uzasadnij."))
    ),
    actionButton("ch10_edu_ans2", "Pokaż rozwiązanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_edu_sol2")
  ),

  div(class = "section-title", "Blok 3: Myślenie krytyczne"),

  div(class = "widget-block",
    h4("Zadanie 3 — Frekwencja w klasie A"),
    div(class = "narrative",
      p("Oblicz bootstrap CI dla mediany frekwencji w klasie A (heavy tail, n ≈ 15)."),
      p(tags$em("Czy klasyczny t-CI byłby wiarygodny? Uzasadnij odwołując się
                 do rozkładu danych i rozmiaru próby."))
    ),
    actionButton("ch10_edu_ans3", "Pokaż odpowiedź",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_edu_sol3")
  )

)

# ============================================================================
# SERVER
# ============================================================================

ch10_server <- function(input, output, session) {

  output$ch10_content <- renderUI({
    switch(input$ch10_kierunek,
      "rol" = .ch10_rol(),
      "zyw" = .ch10_zyw(),
      "bhp" = .ch10_bhp(),
      "edu" = .ch10_edu()
    )
  })

  # ---- Rozwiazania ROLNICTWO ----
  observeEvent(input$ch10_rol_ans1, {
    output$ch10_rol_sol1 <- renderUI({
      div(class = "callout-success",
        tags$strong("Rozwiązanie:"),
        p("Plony są silnie prawoskętne (Gamma) — średnia jest wrażliwa na outliery.
           Mediana lepiej opisuje „typowy‟ plon. Dla mediany nie istnieje prosty wzór
           analityczny — bootstrap jest konieczny."),
        p(tags$b("Interpretacja: "), "\"Z 95% ufnością typowy plon poletek
           wynosi od [dolna granica] do [górna granica] dt/ha.\"")
      )
    })
  })

  observeEvent(input$ch10_rol_ans2, {
    output$ch10_rol_sol2 <- renderUI({
      div(class = "callout-success",
        tags$strong("Rozwiązanie:"),
        p("Test permutacyjny testuje H₀: brak różnicy średnich, bez założenia normalności.
           T-test zakłada normalność lub duże n."),
        p("Jeśli p-wartości są podobne: dane są wystarczająco symetryczne.
           Jeśli się różnią: t-test jest zawodny (silna skośność, małe n).")
      )
    })
  })

  observeEvent(input$ch10_rol_ans3, {
    output$ch10_rol_sol3 <- renderUI({
      div(class = "callout-warning",
        tags$strong("Odpowiedź:"),
        p("Bootstrap pomoże technicznie (obliczy CI), ale przy n = 5 wynik będzie
           mało precyzyjny i wrażliwy na poszczególne obserwacje."),
        p("Bootstrap nie może dodać informacji, której nie ma w danych.
           Nie naprawia problemu małej próby — jedynie uczciwie pokazuje
           jak duża jest niepewność przy n = 5.")
      )
    })
  })

  # ---- Rozwiazania TZ ----
  observeEvent(input$ch10_zyw_ans1, {
    output$ch10_zyw_sol1 <- renderUI({
      div(class = "callout-success",
        tags$strong("Rozwiązanie:"),
        p("Dane na skali 1–7 są skośne i dyskretne — t-CI zakłada ciągłość i normalność.
           Bootstrap CI jest asymetryczny i lepiej odzwierciedla skośność skali."),
        p("Przy n ≈ 18 (część produktu A), CTG jest wątpliwe dla danych na skali.
           Bootstrap jest tutaj właściwszy.")
      )
    })
  })

  observeEvent(input$ch10_zyw_ans2, {
    output$ch10_zyw_sol2 <- renderUI({
      div(class = "callout-success",
        tags$strong("Rozwiązanie:"),
        p("Test permutacyjny testuje H₀: średnia A = średnia B (ta sama H₀ co t-test).
           Mann-Whitney U testuje H₀: rozkłady są identyczne (przesunięcie lokalizacji)."),
        p("Są to różne hipotezy! P-wartości mogą się różnić.
           Do oceny róŻnicy średnich użyj testu permutacyjnego lub bootstrapu.")
      )
    })
  })

  observeEvent(input$ch10_zyw_ans3, {
    output$ch10_zyw_sol3 <- renderUI({
      div(class = "callout-success",
        tags$strong("Odpowiedź:"),
        p("Korelacja Pearson zakłada normalność dwuwymiarową.
           Dane sensoryczne rzadko spełniają to założenie."),
        p("Bootstrap CI dla r oblicza się bez założeń: wystarczy
           resamplować pary (białko, tekstura) i za każdym razem liczyć r.")
      )
    })
  })

  # ---- Rozwiazania BHP ----
  observeEvent(input$ch10_bhp_ans1, {
    output$ch10_bhp_sol1 <- renderUI({
      div(class = "callout-success",
        tags$strong("Rozwiązanie:"),
        p("Czas reakcji ma log-normalny rozkład (silna prawoskętność).
           T-CI dla średniej jest zawodny przy n=22 i silnej skośności.
           Dla mediany nie istnieje klasyczny wzor CI — bootstrap jest konieczny."),
        p(tags$b("Interpretacja: "), "\"Mediana czasu reakcji w populacji z 95% ufnością
           mieści się w przedziale [d, g] ms.\"")
      )
    })
  })

  observeEvent(input$ch10_bhp_ans2, {
    output$ch10_bhp_sol2 <- renderUI({
      div(class = "callout-success",
        tags$strong("Rozwiązanie:"),
        p("Test permutacyjny: brak założeń o normalności, testuje różnicę średnich.
           T-test: wrażliwy na skośność przy małej próbie.
           Wilcoxon: testuje inne H₀ (przesunięcie mediany)."),
        p(tags$b("Rekomendacja BHP:"), " test permutacyjny jest najodpowiedniejszy
           — testuje dokładnie to, czy stres wpływa na średni czas reakcji.")
      )
    })
  })

  observeEvent(input$ch10_bhp_ans3, {
    output$ch10_bhp_sol3 <- renderUI({
      div(class = "callout-success",
        tags$strong("Odpowiedź:"),
        p("Jeśli bootstrap CI dla mediany = [280, 350] ms, a norma to 250 ms:"),
        p(tags$b("Wniosek (jak w raporcie BHP): "),
          "\"Mediana czasu reakcji pracowników wynosi [obs] ms.
           Bootstrap 95% CI [280, 350] ms nie zawiera wartości normowej 250 ms,
           co wskazuje na istotne przekroczenie normy.
           Zaleca się wdrożenie działań redukcji stresu.\"")
      )
    })
  })

  # ---- Rozwiazania EDU ----
  observeEvent(input$ch10_edu_ans1, {
    output$ch10_edu_sol1 <- renderUI({
      div(class = "callout-success",
        tags$strong("Rozwiązanie:"),
        p("Bootstrap CI dla różnicy średnich oblicza się przez:
           (1) resampling par (wynik, program), (2) obliczanie różnicy średnich
           w każdej próbie, (3) percentyle 2.5% i 97.5%."),
        p("Jeśli bootstrap CI i t-CI Welcha są podobne: dane są wystarczająco normalne.
           Jeśli się różnią: bootstrap lepiej radzi sobie z asymetrią.")
      )
    })
  })

  observeEvent(input$ch10_edu_ans2, {
    output$ch10_edu_sol2 <- renderUI({
      div(class = "callout-success",
        tags$strong("Rozwiązanie:"),
        p("Test permutacyjny i t-test testują H₀: średnia A = średnia B.
           Mann-Whitney U testuje H₀: rozkłady są identyczne (równość pośred.
           odpowiada równości średnich tylko przy symetrycznych rozkładach)."),
        p(tags$b("Test permutacyjny"), " jest dokładnie równoważny t-testowi co do H₀,
           ale bez założenia normalności. Mann-Whitney testuje co innego.")
      )
    })
  })

  observeEvent(input$ch10_edu_ans3, {
    output$ch10_edu_sol3 <- renderUI({
      div(class = "callout-warning",
        tags$strong("Odpowiedź:"),
        p("Frekwencja ma heavy tail (wiele wartości bliskich 100%, kilka bardzo niskich).
           T-CI dla mediany wymaga normalności. Przy n ≈ 15 i heavy-tail: CTG jeszcze
           nie działa wystarczająco dobrze."),
        p("Bootstrap CI dla mediany: nie wymaga założeń, lepiej oddaje niepewność
           przy takim rozkładzie. T-CI byłby tu zawądny.")
      )
    })
  })

}
