# ============================================================================
# CHAPTER 12: Cwiczenia kierunkowe (Rolnictwo / TZ / BHP / Edukacja)
# ============================================================================

ch12_ui <- tabPanel("12. \u0106wiczenia",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Praktyczne zadania bayesowskie w kontek\u015bcie Twojego kierunku."
    ),

    div(class = "section-title", "Wybierz kierunek"),

    selectInput("ch12_kierunek", NULL,
      choices = list(
        "Rolnictwo"                      = "rol",
        "Technologia \u017cywno\u015bci" = "zyw",
        "BHP"                            = "bhp",
        "Edukacja"                       = "edu"
      ),
      selected = "rol",
      width = "300px"),

    uiOutput("ch12_content")

  ))
)

ch12_server <- function(input, output, session) {

  output$ch12_content <- renderUI({
    switch(input$ch12_kierunek,
      "rol" = .ch12_rol(),
      "zyw" = .ch12_zyw(),
      "bhp" = .ch12_bhp(),
      "edu" = .ch12_edu()
    )
  })

  # ==========================================================================
  # ROLNICTWO
  # ==========================================================================

  .ch12_rol <- function() {
    tagList(
      div(class = "section-title", "Rolnictwo: por\u00f3wnanie nawoz\u00f3w"),

      div(class = "widget-block",
        h4("Zadanie 1 \u2014 Por\u00f3wnanie plon\u00f3w (BF vs p)"),
        div(class = "narrative",
          p("Rolnik por\u00f3wnuje dwa nawozy (A i B) na identycznych poletkach.
             Plony w dt/ha dla 15 poletek ka\u017cdy nawoz."),
          p(tags$b("Pytanie: "), "Czy naw\u00f3z B daje wy\u017csze plony ni\u017c A?")
        ),
        actionButton("ch12_rol_ans1", "Poka\u017c rozwi\u0105zanie",
                      class = "btn-outline-success btn-sm"),
        uiOutput("ch12_rol_sol1")
      ),

      div(class = "widget-block",
        h4("Zadanie 2 \u2014 Skuteczno\u015b\u0107 oprysku (tabela 2\u00d72)"),
        div(class = "narrative",
          p("Dla 80 poletek z opryskiem: 60 zdrowych, 20 chorych.
             Dla 80 bez oprysku: 40 zdrowych, 40 chorych."),
          p(tags$b("Pytanie: "), "Jak silny jest dow\u00f3d na skuteczno\u015b\u0107 oprysku?
             Jakie OR wynika z posteriora?")
        ),
        actionButton("ch12_rol_ans2", "Poka\u017c rozwi\u0105zanie",
                      class = "btn-outline-success btn-sm"),
        uiOutput("ch12_rol_sol2")
      ),

      div(class = "widget-block",
        h4("Zadanie 3 \u2014 My\u015blenie krytyczne"),
        div(class = "narrative",
          p("Eksperyment z 2 poletkami. p = 0.04 (istotne), BF\u2081\u2080 = 1.8 (anekdotyczne).
             Jak pogodzi\u0107 te wyniki? Kt\u00f3ry paradygmat jest tu wiarygodniejszy i dlaczego?")
        ),
        actionButton("ch12_rol_ans3", "Poka\u017c rozwi\u0105zanie",
                      class = "btn-outline-success btn-sm"),
        uiOutput("ch12_rol_sol3")
      )
    )
  }

  observeEvent(input$ch12_rol_ans1, {
    output$ch12_rol_sol1 <- renderUI({
      div(class = "callout-success",
        tags$b("Rozwi\u0105zanie:"), tags$br(),
        tags$code("BayesFactor::ttestBF(plon_B, plon_A)"),
        " \u2013 BF\u2081\u2080 > 3 to dow\u00f3d umiarkowany ", tags$em("za"), " r\u00f3\u017cnic\u0105.",
        tags$br(),
        "Dodatkowo ", tags$code("posterior(bf_obj, iterations=4000)"),
        " \u2192 wyci\u0105gamy median\u0119 r\u00f3\u017cnicy i 95% HDI.",
        tags$br(),
        tags$b("Raport: "),
        "\u201eJest umiarkowany dow\u00f3d (BF\u2081\u2080 = 5.3), \u017ce naw\u00f3z B daje wy\u017csze plony ni\u017c A
         (mediana r\u00f3\u017cnicy = 4.2 dt/ha, 95% HDI: [0.8, 7.5]).\u201f"
      )
    })
  })

  observeEvent(input$ch12_rol_ans2, {
    output$ch12_rol_sol2 <- renderUI({
      div(class = "callout-success",
        tags$b("Rozwi\u0105zanie:"), tags$br(),
        tags$code("contingencyTableBF(matrix(c(60,20,40,40), 2, byrow=TRUE), sampleType=\"indepMulti\", fixedMargin=\"rows\")"),
        tags$br(),
        "Posterior dla OR:", tags$code("posterior_2x2_or(tab)"),
        tags$br(),
        tags$b("Raport: "),
        "\u201eOR obserwowane = 3.0. Silny bayesowski dow\u00f3d (BF\u2081\u2080 \u2248 50) za skuteczno\u015bci\u0105 oprysku.
         Mediana posterior OR \u2248 2.9, 95% HDI: [1.5, 5.6]; P(OR > 1 | dane) > 99%.\u201f"
      )
    })
  })

  observeEvent(input$ch12_rol_ans3, {
    output$ch12_rol_sol3 <- renderUI({
      div(class = "callout-success",
        tags$b("Rozwi\u0105zanie:"), tags$br(),
        "To paradoks Lindleya w miniaturze: cz\u0119sto\u015bciowa istotno\u015b\u0107 (p=0.04) przy
         bayesowsko s\u0142abym dowodzie (BF=1.8) zazwyczaj sygnalizuje ma\u0142y efekt przy du\u017cej pr\u00f3bie
         \u2014 ale tutaj pr\u00f3ba jest bardzo ma\u0142a (2 poletka!).",
        tags$br(),
        tags$b("Wniosek: "), "p = 0.04 przy n = 2 to praktycznie artefakt.
         BF s\u0142aby poprawnie m\u00f3wi: \u201emasz za ma\u0142o danych, \u017ceby cokolwiek stwierdzi\u0107\u201f.
         Tutaj bayesowski werdykt jest uczciwszy.",
        tags$br(),
        tags$em("Lekcja: p-warto\u015b\u0107 w ma\u0142ej pr\u00f3bie nie daje nam w\u0142a\u015bciwej miary niepewno\u015bci.")
      )
    })
  })

  # ==========================================================================
  # TECHNOLOGIA ZYWNOSCI
  # ==========================================================================

  .ch12_zyw <- function() {
    tagList(
      div(class = "section-title", "Technologia \u017cywno\u015bci: receptury i trwa\u0142o\u015b\u0107"),

      div(class = "widget-block",
        h4("Zadanie 1 \u2014 Nowa vs stara receptura (dwie grupy)"),
        div(class = "narrative",
          p("20 degustator\u00f3w ocenia star\u0105 receptur\u0119 (skala 1\u201310), drugie 20 \u2014 now\u0105."),
          p(tags$b("Pytanie: "), "Czy nowa receptura jest oceniana lepiej?
             Wykonaj test bayesowski i podaj P(nowa > stara | dane).")
        ),
        actionButton("ch12_zyw_ans1", "Poka\u017c rozwi\u0105zanie",
                      class = "btn-outline-success btn-sm"),
        uiOutput("ch12_zyw_sol1")
      ),

      div(class = "widget-block",
        h4("Zadanie 2 \u2014 Trwa\u0142o\u015b\u0107 vs temperatura (regresja)"),
        div(class = "narrative",
          p("Dla 40 porcji produktu zapisano temperatur\u0119 przechowywania (\u00b0C)
             i czas do utraty walor\u00f3w (dni)."),
          p(tags$b("Pytanie: "), "O ile dni skraca si\u0119 trwa\u0142o\u015b\u0107 na ka\u017cdy dodatkowy \u00b0C?
             (regresja bayesowska, 95% HDI dla slope)")
        ),
        actionButton("ch12_zyw_ans2", "Poka\u017c rozwi\u0105zanie",
                      class = "btn-outline-success btn-sm"),
        uiOutput("ch12_zyw_sol2")
      ),

      div(class = "widget-block",
        h4("Zadanie 3 \u2014 Aha-moment"),
        div(class = "narrative",
          p("Recenzent pisze: \u201ep = 0.08, wi\u0119c nowa receptura nie jest lepsza\u201f.
             Jak przeformu\u0142owa\u0107 to stwierdzenie w j\u0119zyku bayesowskim,
             \u017ceby lepiej odzwierciedli\u0142o stan wiedzy?")
        ),
        actionButton("ch12_zyw_ans3", "Poka\u017c rozwi\u0105zanie",
                      class = "btn-outline-success btn-sm"),
        uiOutput("ch12_zyw_sol3")
      )
    )
  }

  observeEvent(input$ch12_zyw_ans1, {
    output$ch12_zyw_sol1 <- renderUI({
      div(class = "callout-success",
        tags$code("ttestBF(formula = ocena ~ receptura, data = d)"),
        " zwraca BF\u2081\u2080.",
        tags$br(),
        tags$code("post <- posterior(bf_obj, 4000)"), " \u2192 ",
        tags$code("mean(post[,\"beta\"] > 0)"),
        " \u2013 bezpo\u015brednio prawdopodobie\u0144stwo, \u017ce nowa > stara.",
        tags$br(),
        tags$b("Raport: "), "\u201eBF\u2081\u2080 = 4.1 (umiarkowany dow\u00f3d za r\u00f3\u017cnic\u0105).
         P(nowa > stara | dane) = 93%. Mediana r\u00f3\u017cnicy = 0.7 pkt, 95% HDI: [0.1, 1.3].\u201f"
      )
    })
  })

  observeEvent(input$ch12_zyw_ans2, {
    output$ch12_zyw_sol2 <- renderUI({
      div(class = "callout-success",
        tags$code("stan_glm(trwalosc ~ temp, data = d, family=gaussian, iter=1000)"),
        tags$br(),
        "Z posteriora: mediana \u03b2\u2081 + 95% HDI.",
        tags$br(),
        tags$b("Raport: "), "\u201eKa\u017cdy dodatkowy \u00b0C skraca trwa\u0142o\u015b\u0107 o mediana 1.8 dnia
         (95% HDI: [1.1, 2.5]). P(efekt ujemny | dane) > 99%.\u201f"
      )
    })
  })

  observeEvent(input$ch12_zyw_ans3, {
    output$ch12_zyw_sol3 <- renderUI({
      div(class = "callout-success",
        tags$b("Problem ze stwierdzeniem recenzenta: "),
        "p = 0.08 nie znaczy \u201ebrak efektu\u201f \u2014 znaczy tylko \u201enie mo\u017cemy odrzuci\u0107 H\u2080 przy \u03b1 = 0.05\u201f.",
        tags$br(),
        tags$b("Bayesowska przeformulacja: "),
        "\u201eObliczyli\u015bmy BF\u2081\u2080 = 1.4 \u2014 anekdotyczny dow\u00f3d za r\u00f3\u017cnic\u0105.
         Mediana r\u00f3\u017cnicy = 0.4 pkt, 95% HDI: [-0.05, 0.9]. Mamy ", tags$em("za ma\u0142o danych"),
         ", \u017ceby odpowiedzie\u0107; wynik zgodny zar\u00f3wno z brakiem efektu, jak i z umiarkowanym efektem.\u201f",
        tags$br(),
        tags$em("To jest uczciwsze: widz\u0119, jak du\u017c\u0105 niepewno\u015b\u0107 mam.")
      )
    })
  })

  # ==========================================================================
  # BHP
  # ==========================================================================

  .ch12_bhp <- function() {
    tagList(
      div(class = "section-title", "BHP: szkolenia i wypadki"),

      div(class = "widget-block",
        h4("Zadanie 1 \u2014 Szkolenie vs incydenty (tabela 2\u00d72)"),
        div(class = "narrative",
          p("Przed szkoleniem: 12 incydent\u00f3w w 80 zmianach. Po szkoleniu: 5 w 80 zmianach."),
          p(tags$b("Pytanie: "), "Jak silny jest dow\u00f3d na spadek liczby incydent\u00f3w?
             Podaj OR i jego 95% HDI.")
        ),
        actionButton("ch12_bhp_ans1", "Poka\u017c rozwi\u0105zanie",
                      class = "btn-outline-success btn-sm"),
        uiOutput("ch12_bhp_sol1")
      ),

      div(class = "widget-block",
        h4("Zadanie 2 \u2014 Czas reakcji (regresja logistyczna)"),
        div(class = "narrative",
          p("Dla 100 pracownik\u00f3w mierzono czas reakcji (ms) i czy pope\u0142nili b\u0142\u0105d (0/1).
             Czy d\u0142u\u017cszy czas reakcji zwi\u0119ksza szans\u0119 b\u0142\u0119du?"),
          p(tags$b("Pytanie: "), "Wykonaj ", tags$code("stan_glm(blad ~ czas, family=binomial)"),
             " \u2014 podaj OR na wzrost czasu o 100 ms.")
        ),
        actionButton("ch12_bhp_ans2", "Poka\u017c rozwi\u0105zanie",
                      class = "btn-outline-success btn-sm"),
        uiOutput("ch12_bhp_sol2")
      ),

      div(class = "widget-block",
        h4("Zadanie 3 \u2014 Decyzja zarz\u0105dcza"),
        div(class = "narrative",
          p("Posterior dla zmniejszenia wypadk\u00f3w po wdro\u017ceniu nowej procedury:
             mediana 30% spadku, 95% HDI: [5%, 52%].
             Koszt wdro\u017cenia: 100k z\u0142. \u017badnych innych danych.
             Jak argumentowa\u0107 \u201ewdra\u017camy\u201f / \u201enie wdra\u017camy\u201f na podstawie HDI?")
        ),
        actionButton("ch12_bhp_ans3", "Poka\u017c rozwi\u0105zanie",
                      class = "btn-outline-success btn-sm"),
        uiOutput("ch12_bhp_sol3")
      )
    )
  }

  observeEvent(input$ch12_bhp_ans1, {
    output$ch12_bhp_sol1 <- renderUI({
      div(class = "callout-success",
        tags$code("tab <- matrix(c(12, 68, 5, 75), 2, byrow=TRUE)"),
        tags$br(),
        tags$code("contingencyTableBF(tab, sampleType=\"indepMulti\", fixedMargin=\"rows\")"),
        tags$br(),
        tags$code("posterior_2x2_or(tab, alpha_prior=1, beta_prior=1)"),
        tags$br(),
        tags$b("Raport: "),
        "\u201eOR wzrostu incydentu przed/po = 2.6 (mediana posterior).
         BF\u2081\u2080 \u2248 3.1 \u2014 umiarkowany dow\u00f3d za r\u00f3\u017cnic\u0105.
         95% HDI OR: [0.9, 7.5]. P(OR > 1 | dane) \u2248 93%.\u201f",
        tags$br(),
        tags$em("HDI zbli\u017ca si\u0119 do 1 \u2014 umiarkowana pewno\u015b\u0107, warto zebra\u0107 wi\u0119cej danych.")
      )
    })
  })

  observeEvent(input$ch12_bhp_ans2, {
    output$ch12_bhp_sol2 <- renderUI({
      div(class = "callout-success",
        tags$code("fit <- stan_glm(blad ~ I(czas/100), data=d, family=binomial, iter=1000)"),
        tags$br(),
        tags$code("post <- as.matrix(fit); OR_100 <- exp(post[,\"I(czas/100)\"])"),
        tags$br(),
        tags$b("Raport: "), "\u201eOR na 100 ms wzrostu czasu reakcji = 1.4 (95% HDI: [1.1, 1.8]).
         P(efekt > 0 | dane) > 99%. Ka\u017cde dodatkowe 100 ms zwi\u0119ksza szans\u0119 b\u0142\u0119du
         \u015brednio 1.4\u00d7.\u201f"
      )
    })
  })

  observeEvent(input$ch12_bhp_ans3, {
    output$ch12_bhp_sol3 <- renderUI({
      div(class = "callout-success",
        tags$b("Za wdro\u017ceniem: "),
        "HDI nie obejmuje 0% (dolny kraniec = 5%). Mamy wiarygodny spadek minimum
         o 5%, mediana 30%. Je\u015bli nawet dolna granica HDI (5% spadku) jest ekonomicznie op\u0142acalna
         \u2014 warto wdra\u017ca\u0107.",
        tags$br(),
        tags$b("Przeciw: "),
        "HDI jest szerokie (5-52%). Je\u015bli op\u0142acalno\u015b\u0107 100k z\u0142 wymaga spadku > 20%,
         ryzyko niepowodzenia jest realne \u2014 mo\u017cna zbiera\u0107 dane pilota\u017cowo przed pe\u0142nym wdro\u017ceniem.",
        tags$br(),
        tags$em("Zaleta HDI: pokazuje spektrum scenariuszy, a nie tylko \u201eistotny/nieistotny\u201f.
                 Pozwala w\u0142\u0105czy\u0107 rachunek ekonomiczny.")
      )
    })
  })

  # ==========================================================================
  # EDUKACJA
  # ==========================================================================

  .ch12_edu <- function() {
    tagList(
      div(class = "section-title", "Edukacja: metody i wyniki"),

      div(class = "widget-block",
        h4("Zadanie 1 \u2014 Klasyczna vs aktywna (ANOVA)"),
        div(class = "narrative",
          p("Trzy klasy (A, B, C) \u2014 ka\u017cda ucz\u0119 inn\u0105 metod\u0105.
             n = 25 na klas\u0119, \u015brednie wyniki: 68, 72, 70."),
          p(tags$b("Pytanie: "), "BF\u2081\u2080 dla modelu z r\u00f3\u017cnicami \u2014
             czy mamy przes\u0142anki, \u017ce metoda ma znaczenie?")
        ),
        actionButton("ch12_edu_ans1", "Poka\u017c rozwi\u0105zanie",
                      class = "btn-outline-success btn-sm"),
        uiOutput("ch12_edu_sol1")
      ),

      div(class = "widget-block",
        h4("Zadanie 2 \u2014 Godziny nauki \u2192 zaliczenie (reg. log.)"),
        div(class = "narrative",
          p("Dla 60 student\u00f3w: godziny nauki w tygodniu (1\u201310) i czy zaliczyli (0/1)."),
          p(tags$b("Pytanie: "), "OR na dodatkow\u0105 godzin\u0119 + P(OR > 1.5 | dane).
             Co to oznacza praktycznie?")
        ),
        actionButton("ch12_edu_ans2", "Poka\u017c rozwi\u0105zanie",
                      class = "btn-outline-success btn-sm"),
        uiOutput("ch12_edu_sol2")
      ),

      div(class = "widget-block",
        h4("Zadanie 3 \u2014 Bayes vs cz\u0119sto\u015bciowo (dyskusja)"),
        div(class = "narrative",
          p("W Twojej pracy magisterskiej: por\u00f3wnujesz dwa podr\u0119czniki (n = 18 + 18).
             Test t: p = 0.06. ttestBF: BF\u2081\u2080 = 2.1.
             Jak opisa\u0107 wynik w sekcji \u201eWyniki\u201f tak, \u017ceby nie nadinterpretowa\u0107?")
        ),
        actionButton("ch12_edu_ans3", "Poka\u017c rozwi\u0105zanie",
                      class = "btn-outline-success btn-sm"),
        uiOutput("ch12_edu_sol3")
      )
    )
  }

  observeEvent(input$ch12_edu_ans1, {
    output$ch12_edu_sol1 <- renderUI({
      div(class = "callout-success",
        tags$code("anovaBF(wynik ~ klasa, data = d)"),
        tags$br(),
        tags$b("Interpretacja: "),
        "Przy takiej wielko\u015bci efektu (4 pkt r\u00f3\u017cnicy mi\u0119dzy najni\u017csz\u0105 a najwy\u017csz\u0105 grup\u0105,
         n = 25 na grup\u0119) najcz\u0119\u015bciej BF\u2081\u2080 \u2248 1.5\u20133 \u2014 dow\u00f3d anekdotyczny / s\u0142aby umiarkowany.",
        tags$br(),
        tags$b("Raport: "),
        "\u201eBF\u2081\u2080 = 2.2 \u2014 niewystarczaj\u0105cy, \u017ceby rozstrzygn\u0105\u0107. Z naszych danych nie wynika jednoznacznie,
         czy metoda nauczania ma wp\u0142yw na wyniki.\u201f"
      )
    })
  })

  observeEvent(input$ch12_edu_ans2, {
    output$ch12_edu_sol2 <- renderUI({
      div(class = "callout-success",
        tags$code("fit <- stan_glm(zaliczenie ~ godziny, family=binomial, iter=1000)"),
        tags$br(),
        tags$code("post <- as.matrix(fit); OR <- exp(post[,\"godziny\"])"),
        tags$br(),
        tags$code("mean(OR > 1.5)"), " \u2192 P(OR > 1.5 | dane)",
        tags$br(),
        tags$b("Raport: "), "\u201eOR na godzin\u0119 = 1.35 (95% HDI: [1.15, 1.6]).
         P(OR > 1.5 | dane) = 22%. Praktycznie: ka\u017cda dodatkowa godzina podnosi szanse
         o oko\u0142o 35%, ale efekt >50% jest nieprawdopodobny.\u201f"
      )
    })
  })

  observeEvent(input$ch12_edu_ans3, {
    output$ch12_edu_sol3 <- renderUI({
      div(class = "callout-success",
        tags$b("Propozycja opisu: "),
        tags$em("\u201eTest t-Studenta nie wykaza\u0142 istotnej statystycznie r\u00f3\u017cnicy (t = 1.95, p = 0.06).
                 Analiza bayesowska da\u0142a BF\u2081\u2080 = 2.1 \u2014 anekdotyczny dow\u00f3d za r\u00f3\u017cnic\u0105.
                 Mediana r\u00f3\u017cnicy \u015brednich = 2.3 pkt (95% HDI: [-0.2, 4.8]).
                 Wielko\u015b\u0107 pr\u00f3by (n = 36) jest zbyt ma\u0142a, aby wiarygodnie rozstrzygn\u0105\u0107 kierunek efektu.\u201f"),
        tags$br(), tags$br(),
        tags$b("Dlaczego to lepiej: "),
        "Unikamy b\u0142\u0119dnego wniosku \u201ebrak r\u00f3\u017cnicy\u201f i jednocze\u015bnie nie sprzedajemy
         anekdotycznego dowodu jako \u201etrendu\u201f. Uczciwa komunikacja niepewno\u015bci."
      )
    })
  })
}
