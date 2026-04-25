# ============================================================================
# CHAPTER 12: Cwiczenia kierunkowe (Rolnictwo / TZ / BHP / Edukacja)
# ============================================================================

ch12_ui <- lecture_chapter(
  id = "ch-cwiczenia",
  num = "12",
  title = "Ćwiczenia",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 12 · Metody bayesowskie",
      num    = "12",
      title  = "Ćwiczenia",
      lead   = "Praktyczne zadania bayesowskie w kontekście kierunkowym."
    ),

    lc_feedback(type = "info",
      "Praktyczne zadania bayesowskie w kontekście Twojego kierunku."
    ),

    lc_h2("ch12-sec-01", "Wybierz kierunek"),

    selectInput("ch12_kierunek", NULL,
      choices = list(
        "Rolnictwo"                      = "rol",
        "Technologia żywności" = "zyw",
        "BHP"                            = "bhp",
        "Edukacja"                       = "edu"
      ),
      selected = "rol",
      width = "300px"),

    uiOutput("ch12_content")

  )
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
      lc_h2("ch12-sec-02", "Rolnictwo: porównanie nawozów"),

      figure_panel(label = "Ćw. 1", title = "Zadanie 1 — Porównanie plonów (BF vs p)",
        tagList(
          p("Rolnik porównuje dwa nawozy (A i B) na identycznych poletkach.
             Plony w dt/ha dla 15 poletek każdy nawoz."),
          p(tags$b("Pytanie: "), "Czy nawóz B daje wyższe plony niż A?")
        ),
        actionButton("ch12_rol_ans1", "Pokaż rozwiązanie",
                      class = "lc-btn-ok-outline lc-btn-sm"),
        uiOutput("ch12_rol_sol1")
      ),

      figure_panel(label = "Ćw. 2", title = "Zadanie 2 — Skuteczność oprysku (tabela 2×2)",
        tagList(
          p("Dla 80 poletek z opryskiem: 60 zdrowych, 20 chorych.
             Dla 80 bez oprysku: 40 zdrowych, 40 chorych."),
          p(tags$b("Pytanie: "), "Jak silny jest dowód na skuteczność oprysku?
             Jakie OR wynika z posteriora?")
        ),
        actionButton("ch12_rol_ans2", "Pokaż rozwiązanie",
                      class = "lc-btn-ok-outline lc-btn-sm"),
        uiOutput("ch12_rol_sol2")
      ),

      figure_panel(label = "Ćw. 3", title = "Zadanie 3 — Myślenie krytyczne",
        tagList(
          p("Eksperyment z 2 poletkami. p = 0.04 (istotne), BF₁₀ = 1.8 (anekdotyczne).
             Jak pogodzić te wyniki? Który paradygmat jest tu wiarygodniejszy i dlaczego?")
        ),
        actionButton("ch12_rol_ans3", "Pokaż rozwiązanie",
                      class = "lc-btn-ok-outline lc-btn-sm"),
        uiOutput("ch12_rol_sol3")
      )
    )
  }

  observeEvent(input$ch12_rol_ans1, {
    output$ch12_rol_sol1 <- renderUI({
      lc_feedback(type = "ok",
        tags$b("Rozwiązanie:"), tags$br(),
        tags$code("BayesFactor::ttestBF(plon_B, plon_A)"),
        " – BF₁₀ > 3 to dowód umiarkowany ", tags$em("za"), " różnicą.",
        tags$br(),
        "Dodatkowo ", tags$code("posterior(bf_obj, iterations=4000)"),
        " → wyciągamy medianę różnicy i 95% HDI.",
        tags$br(),
        tags$b("Raport: "),
        "„Jest umiarkowany dowód (BF₁₀ = 5.3), że nawóz B daje wyższe plony niż A
         (mediana różnicy = 4.2 dt/ha, 95% HDI: [0.8, 7.5]).‟"
      )
    })
  })

  observeEvent(input$ch12_rol_ans2, {
    output$ch12_rol_sol2 <- renderUI({
      lc_feedback(type = "ok",
        tags$b("Rozwiązanie:"), tags$br(),
        tags$code("contingencyTableBF(matrix(c(60,20,40,40), 2, byrow=TRUE), sampleType=\"indepMulti\", fixedMargin=\"rows\")"),
        tags$br(),
        "Posterior dla OR:", tags$code("posterior_2x2_or(tab)"),
        tags$br(),
        tags$b("Raport: "),
        "„OR obserwowane = 3.0. Silny bayesowski dowód (BF₁₀ ≈ 50) za skutecznością oprysku.
         Mediana posterior OR ≈ 2.9, 95% HDI: [1.5, 5.6]; P(OR > 1 | dane) > 99%.‟"
      )
    })
  })

  observeEvent(input$ch12_rol_ans3, {
    output$ch12_rol_sol3 <- renderUI({
      lc_feedback(type = "ok",
        tags$b("Rozwiązanie:"), tags$br(),
        "To paradoks Lindleya w miniaturze: częstościowa istotność (p=0.04) przy
         bayesowsko słabym dowodzie (BF=1.8) zazwyczaj sygnalizuje mały efekt przy dużej próbie
         — ale tutaj próba jest bardzo mała (2 poletka!).",
        tags$br(),
        tags$b("Wniosek: "), "p = 0.04 przy n = 2 to praktycznie artefakt.
         BF słaby poprawnie mówi: „masz za mało danych, żeby cokolwiek stwierdzić‟.
         Tutaj bayesowski werdykt jest uczciwszy.",
        tags$br(),
        tags$em("Lekcja: p-wartość w małej próbie nie daje nam właściwej miary niepewności.")
      )
    })
  })

  # ==========================================================================
  # TECHNOLOGIA ZYWNOSCI
  # ==========================================================================

  .ch12_zyw <- function() {
    tagList(
      lc_h2("ch12-sec-03", "Technologia żywności: receptury i trwałość"),

      figure_panel(label = "Ćw. 4", title = "Zadanie 1 — Nowa vs stara receptura (dwie grupy)",
        tagList(
          p("20 degustatorów ocenia starą recepturę (skala 1–10), drugie 20 — nową."),
          p(tags$b("Pytanie: "), "Czy nowa receptura jest oceniana lepiej?
             Wykonaj test bayesowski i podaj P(nowa > stara | dane).")
        ),
        actionButton("ch12_zyw_ans1", "Pokaż rozwiązanie",
                      class = "lc-btn-ok-outline lc-btn-sm"),
        uiOutput("ch12_zyw_sol1")
      ),

      figure_panel(label = "Ćw. 5", title = "Zadanie 2 — Trwałość vs temperatura (regresja)",
        tagList(
          p("Dla 40 porcji produktu zapisano temperaturę przechowywania (°C)
             i czas do utraty walorów (dni)."),
          p(tags$b("Pytanie: "), "O ile dni skraca się trwałość na każdy dodatkowy °C?
             (regresja bayesowska, 95% HDI dla slope)")
        ),
        actionButton("ch12_zyw_ans2", "Pokaż rozwiązanie",
                      class = "lc-btn-ok-outline lc-btn-sm"),
        uiOutput("ch12_zyw_sol2")
      ),

      figure_panel(label = "Ćw. 6", title = "Zadanie 3 — Aha-moment",
        tagList(
          p("Recenzent pisze: „p = 0.08, więc nowa receptura nie jest lepsza‟.
             Jak przeformułować to stwierdzenie w języku bayesowskim,
             żeby lepiej odzwierciedliło stan wiedzy?")
        ),
        actionButton("ch12_zyw_ans3", "Pokaż rozwiązanie",
                      class = "lc-btn-ok-outline lc-btn-sm"),
        uiOutput("ch12_zyw_sol3")
      )
    )
  }

  observeEvent(input$ch12_zyw_ans1, {
    output$ch12_zyw_sol1 <- renderUI({
      lc_feedback(type = "ok",
        tags$code("ttestBF(formula = ocena ~ receptura, data = d)"),
        " zwraca BF₁₀.",
        tags$br(),
        tags$code("post <- posterior(bf_obj, 4000)"), " → ",
        tags$code("mean(post[,\"beta\"] > 0)"),
        " – bezpośrednio prawdopodobieństwo, że nowa > stara.",
        tags$br(),
        tags$b("Raport: "), "„BF₁₀ = 4.1 (umiarkowany dowód za różnicą).
         P(nowa > stara | dane) = 93%. Mediana różnicy = 0.7 pkt, 95% HDI: [0.1, 1.3].‟"
      )
    })
  })

  observeEvent(input$ch12_zyw_ans2, {
    output$ch12_zyw_sol2 <- renderUI({
      lc_feedback(type = "ok",
        tags$code("stan_glm(trwalosc ~ temp, data = d, family=gaussian, iter=1000)"),
        tags$br(),
        "Z posteriora: mediana β₁ + 95% HDI.",
        tags$br(),
        tags$b("Raport: "), "„Każdy dodatkowy °C skraca trwałość o mediana 1.8 dnia
         (95% HDI: [1.1, 2.5]). P(efekt ujemny | dane) > 99%.‟"
      )
    })
  })

  observeEvent(input$ch12_zyw_ans3, {
    output$ch12_zyw_sol3 <- renderUI({
      lc_feedback(type = "ok",
        tags$b("Problem ze stwierdzeniem recenzenta: "),
        "p = 0.08 nie znaczy „brak efektu‟ — znaczy tylko „nie możemy odrzucić H₀ przy α = 0.05‟.",
        tags$br(),
        tags$b("Bayesowska przeformulacja: "),
        "„Obliczyliśmy BF₁₀ = 1.4 — anekdotyczny dowód za różnicą.
         Mediana różnicy = 0.4 pkt, 95% HDI: [-0.05, 0.9]. Mamy ", tags$em("za mało danych"),
         ", żeby odpowiedzieć; wynik zgodny zarówno z brakiem efektu, jak i z umiarkowanym efektem.‟",
        tags$br(),
        tags$em("To jest uczciwsze: widzę, jak dużą niepewność mam.")
      )
    })
  })

  # ==========================================================================
  # BHP
  # ==========================================================================

  .ch12_bhp <- function() {
    tagList(
      lc_h2("ch12-sec-04", "BHP: szkolenia i wypadki"),

      figure_panel(label = "Ćw. 7", title = "Zadanie 1 — Szkolenie vs incydenty (tabela 2×2)",
        tagList(
          p("Przed szkoleniem: 12 incydentów w 80 zmianach. Po szkoleniu: 5 w 80 zmianach."),
          p(tags$b("Pytanie: "), "Jak silny jest dowód na spadek liczby incydentów?
             Podaj OR i jego 95% HDI.")
        ),
        actionButton("ch12_bhp_ans1", "Pokaż rozwiązanie",
                      class = "lc-btn-ok-outline lc-btn-sm"),
        uiOutput("ch12_bhp_sol1")
      ),

      figure_panel(label = "Ćw. 8", title = "Zadanie 2 — Czas reakcji (regresja logistyczna)",
        tagList(
          p("Dla 100 pracowników mierzono czas reakcji (ms) i czy popełnili błąd (0/1).
             Czy dłuższy czas reakcji zwiększa szansę błędu?"),
          p(tags$b("Pytanie: "), "Wykonaj ", tags$code("stan_glm(blad ~ czas, family=binomial)"),
             " — podaj OR na wzrost czasu o 100 ms.")
        ),
        actionButton("ch12_bhp_ans2", "Pokaż rozwiązanie",
                      class = "lc-btn-ok-outline lc-btn-sm"),
        uiOutput("ch12_bhp_sol2")
      ),

      figure_panel(label = "Ćw. 9", title = "Zadanie 3 — Decyzja zarządcza",
        tagList(
          p("Posterior dla zmniejszenia wypadków po wdrożeniu nowej procedury:
             mediana 30% spadku, 95% HDI: [5%, 52%].
             Koszt wdrożenia: 100k zł. Żadnych innych danych.
             Jak argumentować „wdrażamy‟ / „nie wdrażamy‟ na podstawie HDI?")
        ),
        actionButton("ch12_bhp_ans3", "Pokaż rozwiązanie",
                      class = "lc-btn-ok-outline lc-btn-sm"),
        uiOutput("ch12_bhp_sol3")
      )
    )
  }

  observeEvent(input$ch12_bhp_ans1, {
    output$ch12_bhp_sol1 <- renderUI({
      lc_feedback(type = "ok",
        tags$code("tab <- matrix(c(12, 68, 5, 75), 2, byrow=TRUE)"),
        tags$br(),
        tags$code("contingencyTableBF(tab, sampleType=\"indepMulti\", fixedMargin=\"rows\")"),
        tags$br(),
        tags$code("posterior_2x2_or(tab, alpha_prior=1, beta_prior=1)"),
        tags$br(),
        tags$b("Raport: "),
        "„OR wzrostu incydentu przed/po = 2.6 (mediana posterior).
         BF₁₀ ≈ 3.1 — umiarkowany dowód za różnicą.
         95% HDI OR: [0.9, 7.5]. P(OR > 1 | dane) ≈ 93%.‟",
        tags$br(),
        tags$em("HDI zbliża się do 1 — umiarkowana pewność, warto zebrać więcej danych.")
      )
    })
  })

  observeEvent(input$ch12_bhp_ans2, {
    output$ch12_bhp_sol2 <- renderUI({
      lc_feedback(type = "ok",
        tags$code("fit <- stan_glm(blad ~ I(czas/100), data=d, family=binomial, iter=1000)"),
        tags$br(),
        tags$code("post <- as.matrix(fit); OR_100 <- exp(post[,\"I(czas/100)\"])"),
        tags$br(),
        tags$b("Raport: "), "„OR na 100 ms wzrostu czasu reakcji = 1.4 (95% HDI: [1.1, 1.8]).
         P(efekt > 0 | dane) > 99%. Każde dodatkowe 100 ms zwiększa szansę błędu
         średnio 1.4×.‟"
      )
    })
  })

  observeEvent(input$ch12_bhp_ans3, {
    output$ch12_bhp_sol3 <- renderUI({
      lc_feedback(type = "ok",
        tags$b("Za wdrożeniem: "),
        "HDI nie obejmuje 0% (dolny kraniec = 5%). Mamy wiarygodny spadek minimum
         o 5%, mediana 30%. Jeśli nawet dolna granica HDI (5% spadku) jest ekonomicznie opłacalna
         — warto wdrażać.",
        tags$br(),
        tags$b("Przeciw: "),
        "HDI jest szerokie (5-52%). Jeśli opłacalność 100k zł wymaga spadku > 20%,
         ryzyko niepowodzenia jest realne — można zbierać dane pilotażowo przed pełnym wdrożeniem.",
        tags$br(),
        tags$em("Zaleta HDI: pokazuje spektrum scenariuszy, a nie tylko „istotny/nieistotny‟.
                 Pozwala włączyć rachunek ekonomiczny.")
      )
    })
  })

  # ==========================================================================
  # EDUKACJA
  # ==========================================================================

  .ch12_edu <- function() {
    tagList(
      lc_h2("ch12-sec-05", "Edukacja: metody i wyniki"),

      figure_panel(label = "Ćw. 10", title = "Zadanie 1 — Klasyczna vs aktywna (ANOVA)",
        tagList(
          p("Trzy klasy (A, B, C) — każda uczę inną metodą.
             n = 25 na klasę, średnie wyniki: 68, 72, 70."),
          p(tags$b("Pytanie: "), "BF₁₀ dla modelu z różnicami —
             czy mamy przesłanki, że metoda ma znaczenie?")
        ),
        actionButton("ch12_edu_ans1", "Pokaż rozwiązanie",
                      class = "lc-btn-ok-outline lc-btn-sm"),
        uiOutput("ch12_edu_sol1")
      ),

      figure_panel(label = "Ćw. 11", title = "Zadanie 2 — Godziny nauki → zaliczenie (reg. log.)",
        tagList(
          p("Dla 60 studentów: godziny nauki w tygodniu (1–10) i czy zaliczyli (0/1)."),
          p(tags$b("Pytanie: "), "OR na dodatkową godzinę + P(OR > 1.5 | dane).
             Co to oznacza praktycznie?")
        ),
        actionButton("ch12_edu_ans2", "Pokaż rozwiązanie",
                      class = "lc-btn-ok-outline lc-btn-sm"),
        uiOutput("ch12_edu_sol2")
      ),

      figure_panel(label = "Ćw. 12", title = "Zadanie 3 — Bayes vs częstościowo (dyskusja)",
        tagList(
          p("W Twojej pracy magisterskiej: porównujesz dwa podręczniki (n = 18 + 18).
             Test t: p = 0.06. ttestBF: BF₁₀ = 2.1.
             Jak opisać wynik w sekcji „Wyniki‟ tak, żeby nie nadinterpretować?")
        ),
        actionButton("ch12_edu_ans3", "Pokaż rozwiązanie",
                      class = "lc-btn-ok-outline lc-btn-sm"),
        uiOutput("ch12_edu_sol3")
      )
    )
  }

  observeEvent(input$ch12_edu_ans1, {
    output$ch12_edu_sol1 <- renderUI({
      lc_feedback(type = "ok",
        tags$code("anovaBF(wynik ~ klasa, data = d)"),
        tags$br(),
        tags$b("Interpretacja: "),
        "Przy takiej wielkości efektu (4 pkt różnicy między najniższą a najwyższą grupą,
         n = 25 na grupę) najczęściej BF₁₀ ≈ 1.5–3 — dowód anekdotyczny / słaby umiarkowany.",
        tags$br(),
        tags$b("Raport: "),
        "„BF₁₀ = 2.2 — niewystarczający, żeby rozstrzygnąć. Z naszych danych nie wynika jednoznacznie,
         czy metoda nauczania ma wpływ na wyniki.‟"
      )
    })
  })

  observeEvent(input$ch12_edu_ans2, {
    output$ch12_edu_sol2 <- renderUI({
      lc_feedback(type = "ok",
        tags$code("fit <- stan_glm(zaliczenie ~ godziny, family=binomial, iter=1000)"),
        tags$br(),
        tags$code("post <- as.matrix(fit); OR <- exp(post[,\"godziny\"])"),
        tags$br(),
        tags$code("mean(OR > 1.5)"), " → P(OR > 1.5 | dane)",
        tags$br(),
        tags$b("Raport: "), "„OR na godzinę = 1.35 (95% HDI: [1.15, 1.6]).
         P(OR > 1.5 | dane) = 22%. Praktycznie: każda dodatkowa godzina podnosi szanse
         o około 35%, ale efekt >50% jest nieprawdopodobny.‟"
      )
    })
  })

  observeEvent(input$ch12_edu_ans3, {
    output$ch12_edu_sol3 <- renderUI({
      lc_feedback(type = "ok",
        tags$b("Propozycja opisu: "),
        tags$em("„Test t-Studenta nie wykazał istotnej statystycznie różnicy (t = 1.95, p = 0.06).
                 Analiza bayesowska dała BF₁₀ = 2.1 — anekdotyczny dowód za różnicą.
                 Mediana różnicy średnich = 2.3 pkt (95% HDI: [-0.2, 4.8]).
                 Wielkość próby (n = 36) jest zbyt mała, aby wiarygodnie rozstrzygnąć kierunek efektu.‟"),
        tags$br(), tags$br(),
        tags$b("Dlaczego to lepiej: "),
        "Unikamy błędnego wniosku „brak różnicy‟ i jednocześnie nie sprzedajemy
         anekdotycznego dowodu jako „trendu‟. Uczciwa komunikacja niepewności."
      )
    })
  })
}
