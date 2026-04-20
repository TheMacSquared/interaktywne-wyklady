# ============================================================================
# CHAPTER 10: Cwiczenia praktyczne
# ============================================================================

# ============================================================================
# UI
# ============================================================================

ch10_ui <- tabPanel("10. \u0106wiczenia",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Czas zastosowa\u0107 metody resamplingowe na danych z Twojego kierunku."
    ),

    div(class = "section-title", "\u0106wiczenia \u2014 metody resamplingowe"),

    div(class = "narrative",
      p(tags$b("Czas trwania:"), " ~ 90 minut \u00b7 ",
        tags$b("Narz\u0119dzie:"), " Jamovi (z pakietem bootstrap/permutacje)"),
      p("Trzy bloki zada\u0144 na kierunek: bootstrap CI, test permutacyjny
         i my\u015blenie krytyczne (kiedy/dlaczego). Ka\u017cde zadanie ma ",
        "ukryte rozwi\u0105zanie.")
    ),

    div(class = "callout-info",
      selectInput("ch10_kierunek", tags$b("Wybierz wariant dla kierunku:"),
        choices = list(
          "Rolnictwo"                        = "rol",
          "Technologia \u017bywno\u015bci"             = "zyw",
          "In\u017cynieria Bezpiecze\u0144stwa (BHP)" = "bhp",
          "Edukacja / Nauki Spo\u0142eczne"          = "edu"
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
    p(tags$b("Dane: "), tags$code("plony_nawoz"), " \u2014 wygeneruj w R lub Jamovi:"),
    tags$pre(style = "font-size:12px;",
"set.seed(42)
df <- data.frame(
  nawoz      = sample(c('A','B'), 28, replace=TRUE),
  plon_dt_ha = round(rgamma(28, 2.5, scale=18), 1),
  odmiana    = sample(c('Typ1','Typ2','Typ3'), 28, replace=TRUE)
)"),
    p("Dane: n = 28, plony w dt/ha (silnie prawosk\u0119tne).")
  ),

  div(class = "section-title", "Blok 1: Bootstrap CI"),

  div(class = "widget-block",
    h4("Zadanie 1 \u2014 Mediana plonu"),
    div(class = "narrative",
      p("Wyznacz 95% bootstrap CI dla mediany plonu (zmienna ",
        tags$code("plon_dt_ha"), "). Dlaczego mediana, a nie \u015brednia?"),
      p("Zanim klikniesz rozwi\u0105zanie: ",
        tags$em("zinterpretuj CI jednym zdaniem \u2014
          co oznacza ten przedzia\u0142 w kontek\u015bcie agrotechnicznym?"))
    ),
    actionButton("ch10_rol_ans1", "Poka\u017c rozwi\u0105zanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_rol_sol1")
  ),

  div(class = "section-title", "Blok 2: Test permutacyjny"),

  div(class = "widget-block",
    h4("Zadanie 2 \u2014 Naw\u00f3z A vs B"),
    div(class = "narrative",
      p("Czy naw\u00f3z A daje wy\u017csze plony ni\u017c B?
         Wykonaj permutacyjny test r\u00f3\u017cnicy \u015brednich (B = 5000)."),
      p("Por\u00f3wnaj p-warto\u015b\u0107 z testu permutacyjnego z p-warto\u015bci\u0105 z testu t.
         Je\u015bli si\u0119 r\u00f3\u017cni\u0105, co to oznacza?")
    ),
    actionButton("ch10_rol_ans2", "Poka\u017c rozwi\u0105zanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_rol_sol2")
  ),

  div(class = "section-title", "Blok 3: My\u015blenie krytyczne"),

  div(class = "widget-block",
    h4("Zadanie 3 \u2014 Ma\u0142a pr\u00f3ba"),
    div(class = "narrative",
      p("Przeprowadzono 5 dodatkowych pr\u00f3b na mniejszych poletkach (n = 5)."),
      p(tags$em("Czy bootstrap CI pomo\u017ce z tak ma\u0142\u0105 pr\u00f3b\u0105?
                 Jakie s\u0105 ograniczenia? Czy zaproponowany CI b\u0119dzie wiarygodny?"))
    ),
    actionButton("ch10_rol_ans3", "Poka\u017c odpowied\u017a",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_rol_sol3")
  )

)

.ch10_zyw <- function() tagList(

  div(class = "callout-info",
    p(tags$b("Dane: "), tags$code("analiza_sensoryczna"), " \u2014 wygeneruj w R lub Jamovi:"),
    tags$pre(style = "font-size:12px;",
"set.seed(42)
df <- data.frame(
  produkt             = sample(c('A','B'), 35, replace=TRUE),
  ocena_tekstury      = pmin(7, pmax(1, round(rgamma(35, 2, 0.4)+1))),
  zawartosc_bialka    = round(rnorm(35, 18, 3.5), 1),
  czas_przechowywania = round(runif(35, 1, 90))
)"),
    p("Dane: n = 35, ocena tekstury na skali 1\u20137 (sko\u015bna), bia\u0142ko w g/100g.")
  ),

  div(class = "section-title", "Blok 1: Bootstrap CI"),

  div(class = "widget-block",
    h4("Zadanie 1 \u2014 Mediana oceny tekstury"),
    div(class = "narrative",
      p("Wyznacz 95% bootstrap CI dla mediany oceny tekstury produktu A.
         Por\u00f3wnaj z t-CI."),
      p(tags$em("Kt\u00f3ry CI jest w\u0142a\u015bciwszy dla skali 1\u20137 przy n \u2248 18?
                 Uzasadnij."))
    ),
    actionButton("ch10_zyw_ans1", "Poka\u017c rozwi\u0105zanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_zyw_sol1")
  ),

  div(class = "section-title", "Blok 2: Test permutacyjny"),

  div(class = "widget-block",
    h4("Zadanie 2 \u2014 Produkt A vs B"),
    div(class = "narrative",
      p("Permutacyjny test r\u00f3\u017cnicy ocen tekstury mi\u0119dzy produktem A i B.
         Por\u00f3wnaj z Mann-Whitney U."),
      p(tags$em("Dlaczego Mann-Whitney U i test permutacyjny mog\u0105 da\u0107 inne p-warto\u015bci?
                 Kt\u00f3ry testuje t\u0119 sam\u0105 H\u2080 co t-test?"))
    ),
    actionButton("ch10_zyw_ans2", "Poka\u017c rozwi\u0105zanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_zyw_sol2")
  ),

  div(class = "section-title", "Blok 3: My\u015blenie krytyczne"),

  div(class = "widget-block",
    h4("Zadanie 3 \u2014 Korelacja bia\u0142ko \u2013 tekstura"),
    div(class = "narrative",
      p("Czy istnieje zwi\u0105zek mi\u0119dzy zawarto\u015bci\u0105 bia\u0142ka a ocen\u0105 tekstury?"),
      p(tags$em("Oblicz bootstrap CI dla wsp\u00f3\u0142czynnika korelacji Pearsona.
                 Dlaczego tu wolimy bootstrap, a nie klasyczny test Pearsona?"))
    ),
    actionButton("ch10_zyw_ans3", "Poka\u017c odpowied\u017a",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_zyw_sol3")
  )

)

.ch10_bhp <- function() tagList(

  div(class = "callout-info",
    p(tags$b("Dane: "), tags$code("czas_reakcji_bhp"), " \u2014 wygeneruj w R lub Jamovi:"),
    tags$pre(style = "font-size:12px;",
"set.seed(42)
df <- data.frame(
  warunki         = sample(c('normalny','stres'), 22, replace=TRUE),
  czas_reakcji_ms = round(rlnorm(22, meanlog=5.6, sdlog=0.4)),
  bledy_count     = rpois(22, lambda=2.5)
)"),
    p("Dane: n = 22, czas reakcji w ms (log-normalny, silnie prawosk\u0119tny).")
  ),

  div(class = "section-title", "Blok 1: Bootstrap CI"),

  div(class = "widget-block",
    h4("Zadanie 1 \u2014 Mediana czasu reakcji"),
    div(class = "narrative",
      p("Wyznacz 95% bootstrap CI dla mediany czasu reakcji pracownik\u00f3w."),
      p(tags$em("Dlaczego bootstrap CI dla mediany \u2014 a nie t-CI dla \u015bredniej?
                 Jakie jest znaczenie praktyczne tego CI w kontek\u015bcie BHP?"))
    ),
    actionButton("ch10_bhp_ans1", "Poka\u017c rozwi\u0105zanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_bhp_sol1")
  ),

  div(class = "section-title", "Blok 2: Test permutacyjny"),

  div(class = "widget-block",
    h4("Zadanie 2 \u2014 Stres vs normalne warunki"),
    div(class = "narrative",
      p("Czy stres istotnie wyd\u0142u\u017ca czas reakcji?
         Wykonaj permutacyjny test r\u00f3\u017cnicy \u015brednich (B = 5000)."),
      p(tags$em("Por\u00f3wnaj p-warto\u015b\u0107 z t-testem i Wilcoxonem.
                 Kt\u00f3ry test jest tu najodpowiedniejszy i dlaczego?"))
    ),
    actionButton("ch10_bhp_ans2", "Poka\u017c rozwi\u0105zanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_bhp_sol2")
  ),

  div(class = "section-title", "Blok 3: My\u015blenie krytyczne"),

  div(class = "widget-block",
    h4("Zadanie 3 \u2014 Norma bezpiecze\u0144stwa"),
    div(class = "narrative",
      p("Norma bezpiecze\u0144stwa: mediana czasu reakcji nie powinna przekracza\u0107 250ms."),
      p(tags$em("Oblicz bootstrap CI dla mediany. Je\u015bli 250ms le\u017cy poza CI \u2014
                 co wnioskujesz? Sformu\u0142uj wniosek \u201ejak w raporcie BHP\u201f."))
    ),
    actionButton("ch10_bhp_ans3", "Poka\u017c odpowied\u017a",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_bhp_sol3")
  )

)

.ch10_edu <- function() tagList(

  div(class = "callout-info",
    p(tags$b("Dane: "), tags$code("wyniki_programu"), " \u2014 wygeneruj w R lub Jamovi:"),
    tags$pre(style = "font-size:12px;",
"set.seed(42)
df <- data.frame(
  program       = sample(c('tradycyjny','nowy'), 45, replace=TRUE),
  wynik_test    = pmin(100, pmax(0, round(rnorm(45, 68, 15)))),
  frekwencja_pct = round(pmin(100, pmax(50, 100-rexp(45, rate=0.04))),1),
  klasa         = sample(c('A','B','C'), 45, replace=TRUE)
)"),
    p("Dane: n = 45, wynik testu 0\u2013100, frekwencja % (heavy tail).")
  ),

  div(class = "section-title", "Blok 1: Bootstrap CI"),

  div(class = "widget-block",
    h4("Zadanie 1 \u2014 R\u00f3\u017cnica wynik\u00f3w"),
    div(class = "narrative",
      p("Wyznacz 95% bootstrap CI dla r\u00f3\u017cnicy \u015brednich wynik\u00f3w
         (nowy program vs tradycyjny). Por\u00f3wnaj z t-CI Welcha."),
      p(tags$em("Czy oba CI s\u0105 podobne? Co to m\u00f3wi o rozk\u0142adzie danych?"))
    ),
    actionButton("ch10_edu_ans1", "Poka\u017c rozwi\u0105zanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_edu_sol1")
  ),

  div(class = "section-title", "Blok 2: Test permutacyjny"),

  div(class = "widget-block",
    h4("Zadanie 2 \u2014 Skuteczno\u015b\u0107 nowego programu"),
    div(class = "narrative",
      p("Permutacyjny test r\u00f3\u017cnicy wynik\u00f3w mi\u0119dzy programami.
         Por\u00f3wnaj p-warto\u015b\u0107 z testem t i Mann-Whitney U."),
      p(tags$em("Kt\u00f3ry test testuje H\u2080: brak r\u00f3\u017cnicy \u015brednich \u2014 test permutacyjny czy Mann-Whitney?
                 Uzasadnij."))
    ),
    actionButton("ch10_edu_ans2", "Poka\u017c rozwi\u0105zanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput("ch10_edu_sol2")
  ),

  div(class = "section-title", "Blok 3: My\u015blenie krytyczne"),

  div(class = "widget-block",
    h4("Zadanie 3 \u2014 Frekwencja w klasie A"),
    div(class = "narrative",
      p("Oblicz bootstrap CI dla mediany frekwencji w klasie A (heavy tail, n \u2248 15)."),
      p(tags$em("Czy klasyczny t-CI by\u0142by wiarygodny? Uzasadnij odwo\u0142uj\u0105c si\u0119
                 do rozk\u0142adu danych i rozmiaru pr\u00f3by."))
    ),
    actionButton("ch10_edu_ans3", "Poka\u017c odpowied\u017a",
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
        tags$strong("Rozwi\u0105zanie:"),
        p("Plony s\u0105 silnie prawosk\u0119tne (Gamma) \u2014 \u015brednia jest wra\u017cliwa na outliery.
           Mediana lepiej opisuje \u201etypowy\u201f plon. Dla mediany nie istnieje prosty wz\u00f3r
           analityczny \u2014 bootstrap jest konieczny."),
        p(tags$b("Interpretacja: "), "\"Z 95% ufno\u015bci\u0105 typowy plon poletek
           wynosi od [dolna granica] do [g\u00f3rna granica] dt/ha.\"")
      )
    })
  })

  observeEvent(input$ch10_rol_ans2, {
    output$ch10_rol_sol2 <- renderUI({
      div(class = "callout-success",
        tags$strong("Rozwi\u0105zanie:"),
        p("Test permutacyjny testuje H\u2080: brak r\u00f3\u017cnicy \u015brednich, bez za\u0142o\u017cenia normalno\u015bci.
           T-test zak\u0142ada normalno\u015b\u0107 lub du\u017ce n."),
        p("Je\u015bli p-warto\u015bci s\u0105 podobne: dane s\u0105 wystarczaj\u0105co symetryczne.
           Je\u015bli si\u0119 r\u00f3\u017cni\u0105: t-test jest zawodny (silna sko\u015bno\u015b\u0107, ma\u0142e n).")
      )
    })
  })

  observeEvent(input$ch10_rol_ans3, {
    output$ch10_rol_sol3 <- renderUI({
      div(class = "callout-warning",
        tags$strong("Odpowied\u017a:"),
        p("Bootstrap pomo\u017ce technicznie (obliczy CI), ale przy n = 5 wynik b\u0119dzie
           ma\u0142o precyzyjny i wra\u017cliwy na poszczeg\u00f3lne obserwacje."),
        p("Bootstrap nie mo\u017ce doda\u0107 informacji, kt\u00f3rej nie ma w danych.
           Nie naprawia problemu ma\u0142ej pr\u00f3by \u2014 jedynie uczciwie pokazuje
           jak du\u017ca jest niepewno\u015b\u0107 przy n = 5.")
      )
    })
  })

  # ---- Rozwiazania TZ ----
  observeEvent(input$ch10_zyw_ans1, {
    output$ch10_zyw_sol1 <- renderUI({
      div(class = "callout-success",
        tags$strong("Rozwi\u0105zanie:"),
        p("Dane na skali 1\u20137 s\u0105 sko\u015bne i dyskretne \u2014 t-CI zak\u0142ada ci\u0105g\u0142o\u015b\u0107 i normalno\u015b\u0107.
           Bootstrap CI jest asymetryczny i lepiej odzwierciedla sko\u015bno\u015b\u0107 skali."),
        p("Przy n \u2248 18 (cz\u0119\u015b\u0107 produktu A), CTG jest w\u0105tpliwe dla danych na skali.
           Bootstrap jest tutaj w\u0142a\u015bciwszy.")
      )
    })
  })

  observeEvent(input$ch10_zyw_ans2, {
    output$ch10_zyw_sol2 <- renderUI({
      div(class = "callout-success",
        tags$strong("Rozwi\u0105zanie:"),
        p("Test permutacyjny testuje H\u2080: \u015brednia A = \u015brednia B (ta sama H\u2080 co t-test).
           Mann-Whitney U testuje H\u2080: rozk\u0142ady s\u0105 identyczne (przesuni\u0119cie lokalizacji)."),
        p("S\u0105 to r\u00f3\u017cne hipotezy! P-warto\u015bci mog\u0105 si\u0119 r\u00f3\u017cni\u0107.
           Do oceny r\u00f3\u017bnicy \u015brednich u\u017cyj testu permutacyjnego lub bootstrapu.")
      )
    })
  })

  observeEvent(input$ch10_zyw_ans3, {
    output$ch10_zyw_sol3 <- renderUI({
      div(class = "callout-success",
        tags$strong("Odpowied\u017a:"),
        p("Korelacja Pearson zak\u0142ada normalno\u015b\u0107 dwuwymiarow\u0105.
           Dane sensoryczne rzadko spe\u0142niaj\u0105 to za\u0142o\u017cenie."),
        p("Bootstrap CI dla r oblicza si\u0119 bez za\u0142o\u017ce\u0144: wystarczy
           resamplowa\u0107 pary (bia\u0142ko, tekstura) i za ka\u017cdym razem liczy\u0107 r.")
      )
    })
  })

  # ---- Rozwiazania BHP ----
  observeEvent(input$ch10_bhp_ans1, {
    output$ch10_bhp_sol1 <- renderUI({
      div(class = "callout-success",
        tags$strong("Rozwi\u0105zanie:"),
        p("Czas reakcji ma log-normalny rozk\u0142ad (silna prawosk\u0119tno\u015b\u0107).
           T-CI dla \u015bredniej jest zawodny przy n=22 i silnej sko\u015bno\u015bci.
           Dla mediany nie istnieje klasyczny wzor CI \u2014 bootstrap jest konieczny."),
        p(tags$b("Interpretacja: "), "\"Mediana czasu reakcji w populacji z 95% ufno\u015bci\u0105
           mie\u015bci si\u0119 w przedziale [d, g] ms.\"")
      )
    })
  })

  observeEvent(input$ch10_bhp_ans2, {
    output$ch10_bhp_sol2 <- renderUI({
      div(class = "callout-success",
        tags$strong("Rozwi\u0105zanie:"),
        p("Test permutacyjny: brak za\u0142o\u017ce\u0144 o normalno\u015bci, testuje r\u00f3\u017cnic\u0119 \u015brednich.
           T-test: wra\u017cliwy na sko\u015bno\u015b\u0107 przy ma\u0142ej pr\u00f3bie.
           Wilcoxon: testuje inne H\u2080 (przesuni\u0119cie mediany)."),
        p(tags$b("Rekomendacja BHP:"), " test permutacyjny jest najodpowiedniejszy
           \u2014 testuje dok\u0142adnie to, czy stres wp\u0142ywa na \u015bredni czas reakcji.")
      )
    })
  })

  observeEvent(input$ch10_bhp_ans3, {
    output$ch10_bhp_sol3 <- renderUI({
      div(class = "callout-success",
        tags$strong("Odpowied\u017a:"),
        p("Je\u015bli bootstrap CI dla mediany = [280, 350] ms, a norma to 250 ms:"),
        p(tags$b("Wniosek (jak w raporcie BHP): "),
          "\"Mediana czasu reakcji pracownik\u00f3w wynosi [obs] ms.
           Bootstrap 95% CI [280, 350] ms nie zawiera warto\u015bci normowej 250 ms,
           co wskazuje na istotne przekroczenie normy.
           Zaleca si\u0119 wdro\u017cenie dzia\u0142a\u0144 redukcji stresu.\"")
      )
    })
  })

  # ---- Rozwiazania EDU ----
  observeEvent(input$ch10_edu_ans1, {
    output$ch10_edu_sol1 <- renderUI({
      div(class = "callout-success",
        tags$strong("Rozwi\u0105zanie:"),
        p("Bootstrap CI dla r\u00f3\u017cnicy \u015brednich oblicza si\u0119 przez:
           (1) resampling par (wynik, program), (2) obliczanie r\u00f3\u017cnicy \u015brednich
           w ka\u017cdej pr\u00f3bie, (3) percentyle 2.5% i 97.5%."),
        p("Je\u015bli bootstrap CI i t-CI Welcha s\u0105 podobne: dane s\u0105 wystarczaj\u0105co normalne.
           Je\u015bli si\u0119 r\u00f3\u017cni\u0105: bootstrap lepiej radzi sobie z asymetri\u0105.")
      )
    })
  })

  observeEvent(input$ch10_edu_ans2, {
    output$ch10_edu_sol2 <- renderUI({
      div(class = "callout-success",
        tags$strong("Rozwi\u0105zanie:"),
        p("Test permutacyjny i t-test testuj\u0105 H\u2080: \u015brednia A = \u015brednia B.
           Mann-Whitney U testuje H\u2080: rozk\u0142ady s\u0105 identyczne (r\u00f3wno\u015b\u0107 po\u015bred.
           odpowiada r\u00f3wno\u015bci \u015brednich tylko przy symetrycznych rozk\u0142adach)."),
        p(tags$b("Test permutacyjny"), " jest dok\u0142adnie r\u00f3wnowa\u017cny t-testowi co do H\u2080,
           ale bez za\u0142o\u017cenia normalno\u015bci. Mann-Whitney testuje co innego.")
      )
    })
  })

  observeEvent(input$ch10_edu_ans3, {
    output$ch10_edu_sol3 <- renderUI({
      div(class = "callout-warning",
        tags$strong("Odpowied\u017a:"),
        p("Frekwencja ma heavy tail (wiele warto\u015bci bliskich 100%, kilka bardzo niskich).
           T-CI dla mediany wymaga normalno\u015bci. Przy n \u2248 15 i heavy-tail: CTG jeszcze
           nie dzia\u0142a wystarczaj\u0105co dobrze."),
        p("Bootstrap CI dla mediany: nie wymaga za\u0142o\u017ce\u0144, lepiej oddaje niepewno\u015b\u0107
           przy takim rozk\u0142adzie. T-CI by\u0142by tu zaw\u0105dny.")
      )
    })
  })

}
