# ============================================================================
# ROZDZIAŁ 5: Twoja decyzja (ćwiczenie)
# ============================================================================

ch5_ui <- lecture_chapter(
  id = "ch-cwiczenie",
  num = "05",
  title = "Twoja decyzja",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 06 · Optymalizacja",
      num = "05",
      title = "Twoja decyzja.",
      lead = "Sprawdź się: jaką produkcję wybierzesz, żeby zarobić jak najwięcej? A potem — co zrobić, żeby zarobić jeszcze więcej?"
    ),

    lc_h2("ch5-sytuacja", "Sytuacja"),
    lc_p("Stolarz produkuje krzesła i stoły. Każde krzesło wymaga 4 m drewna i 3 godzin pracy. Każdy stół — 6 m drewna i 4 godziny pracy. Tygodniowo dysponuje 240 m drewna i 200 godzinami pracy. Krzesło sprzedaje za 80 zł zysku, stół za 100 zł zysku. Cel: maksymalny tygodniowy zysk."),
    figure_panel(
      label = "Tabela 5.1",
      title = "Dane zadania",
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Zasób"),
          tags$th("Krzesło (x₁)"),
          tags$th("Stół (x₂)"),
          tags$th("Limit")
        )),
        tags$tbody(
          tags$tr(tags$td("Drewno"), tags$td("4 m"),    tags$td("6 m"),    tags$td("240 m")),
          tags$tr(tags$td("Praca"),  tags$td("3 godz"), tags$td("4 godz"), tags$td("200 godz")),
          tags$tr(tags$td(tags$strong("Zysk")),
                  tags$td(tags$strong("80 zł")),
                  tags$td(tags$strong("100 zł")),
                  tags$td("—"))
        )
      )
    ),

    lc_h2("ch5-wybor", "Twój wybór produkcji"),
    figure_panel(
      label = "Ćwiczenie 4.1",
      title = "Wpisz liczbę krzeseł i stołów",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          numericInput("ch5_x1", "Krzesła (x₁)", value = 30, min = 0, max = 80, step = 1),
          numericInput("ch5_x2", "Stoły (x₂)",   value = 20, min = 0, max = 60, step = 1)
        ),
        column(
          8,
          uiOutput("ch5_stats"),
          uiOutput("ch5_feedback")
        )
      )
    ),

    lc_h2("ch5-pytanie", "Co zrobić, żeby jeszcze więcej zarobić?"),
    lc_p("Załóżmy, że jesteś już w optimum. Stolarz może zainwestować — ale w co? Wybierz odpowiedź:"),
    figure_panel(
      label = "Quiz",
      title = "Gdzie zainwestować?",
      radioButtons(
        "ch5_action", NULL,
        choices = c(
          "Kupić więcej drewna"             = "drewno",
          "Zatrudnić dodatkowego pracownika" = "praca",
          "Podnieść cenę krzesła"            = "cena",
          "Zmniejszyć produkcję stołów"      = "mniej_stolow"
        ),
        selected = character(0)
      ),
      uiOutput("ch5_action_feedback")
    ),

    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      open = TRUE,
      "W optymalizacji liniowej rezerwę zasobu (zmienna dopełniająca > 0) widzimy bezpośrednio. Zwiększenie zasobu wąskiego gardła zwiększa zysk; zwiększenie zasobu z rezerwą — nie. To są „ceny dualne” — wracają w rozdziale 07."
    )
  )
)

ch5_server <- function(input, output, session) {
  # Parametry zadania (na sztywno — to jest ćwiczenie z konkretnymi liczbami).
  WOOD_LIMIT  <- 240
  LABOR_LIMIT <- 200
  WOOD_CHAIR  <- 4;  WOOD_TABLE  <- 6
  LAB_CHAIR   <- 3;  LAB_TABLE   <- 4
  PRICE_CHAIR <- 80; PRICE_TABLE <- 100
  OPT_X1 <- 60; OPT_X2 <- 0; OPT_Z <- 4800

  ch5_check <- reactive({
    x1 <- input$ch5_x1
    x2 <- input$ch5_x2
    if (is.null(x1) || is.null(x2) || is.na(x1) || is.na(x2)) {
      return(NULL)
    }
    wood  <- WOOD_CHAIR * x1 + WOOD_TABLE * x2
    labor <- LAB_CHAIR  * x1 + LAB_TABLE  * x2
    profit <- PRICE_CHAIR * x1 + PRICE_TABLE * x2
    feasible <- (x1 >= 0) && (x2 >= 0) &&
                (wood <= WOOD_LIMIT + 1e-9) &&
                (labor <= LABOR_LIMIT + 1e-9)
    list(
      x1 = x1, x2 = x2,
      wood = wood, labor = labor,
      profit = profit,
      feasible = feasible
    )
  })

  output$ch5_stats <- renderUI({
    r <- ch5_check()
    if (is.null(r)) return(NULL)
    lc_stat_grid(
      lc_stat_box("Drewno", paste0(eco_fmt(r$wood, 0), " / 240 m"),
                  color = unname(upwr_cat["niebo"])),
      lc_stat_box("Praca",  paste0(eco_fmt(r$labor, 0), " / 200 godz"),
                  color = unname(upwr_cat["szalwia"])),
      lc_stat_box("Zysk",   paste0(eco_fmt(r$profit, 0), " zł"),
                  color = upwr_accent),
      columns = 3
    )
  })

  output$ch5_feedback <- renderUI({
    r <- ch5_check()
    if (is.null(r)) return(NULL)

    if (!r$feasible) {
      breaches <- c()
      if (r$wood  > WOOD_LIMIT)  breaches <- c(breaches,
        paste0("drewno przekroczone o ", eco_fmt(r$wood - WOOD_LIMIT, 0), " m"))
      if (r$labor > LABOR_LIMIT) breaches <- c(breaches,
        paste0("praca przekroczona o ", eco_fmt(r$labor - LABOR_LIMIT, 0), " godz"))
      if (r$x1 < 0 || r$x2 < 0)  breaches <- c(breaches, "ujemna produkcja")
      msg <- paste0("Niedopuszczalne: ", paste(breaches, collapse = "; "), ".")
      return(lc_feedback(type = "warning", msg))
    }

    if (abs(r$profit - OPT_Z) < 1e-6 && r$x1 == OPT_X1 && r$x2 == OPT_X2) {
      return(lc_feedback(
        type = "ok",
        paste0("Brawo — to optimum! Produkcja: ", OPT_X1, " krzeseł i ",
               OPT_X2, " stołów, zysk = ", OPT_Z, " zł. ",
               "Wąskie gardło: drewno (", WOOD_LIMIT, "/", WOOD_LIMIT,
               " — wykorzystane w 100%). Praca: ", LAB_CHAIR * OPT_X1,
               "/", LABOR_LIMIT, " godz, zostaje rezerwa ",
               LABOR_LIMIT - LAB_CHAIR * OPT_X1, " godz.")
      ))
    }

    msg <- paste0(
      "Dopuszczalne, ale nie optymalne. Twój wybór: ", r$x1, " krzeseł i ",
      r$x2, " stołów, zysk = ", eco_fmt(r$profit, 0), " zł. ",
      "Optimum: x₁ = ", OPT_X1, ", x₂ = ", OPT_X2,
      ", zysk = ", OPT_Z, " zł. ",
      "Wąskie gardło w optimum: drewno (240/240). Praca ma rezerwę 20 godz."
    )
    lc_feedback(type = "info", msg)
  })

  output$ch5_action_feedback <- renderUI({
    a <- input$ch5_action
    if (is.null(a) || !nzchar(a)) return(NULL)
    switch(a,
      drewno = lc_feedback(
        type = "ok",
        "Tak — drewno jest wąskim gardłem (w optimum 240/240, wykorzystane w 100%). Każdy dodatkowy metr drewna pozwala wyprodukować więcej krzeseł, a więc zwiększa zysk."
      ),
      praca = lc_feedback(
        type = "warning",
        "Praca ma rezerwę 20 godz w optimum (180/200). Dodatkowy pracownik nie pomoże — i tak nie wykorzystujemy tego, co już mamy."
      ),
      cena = lc_feedback(
        type = "info",
        "Podniesienie ceny zwiększa zysk przy tej samej produkcji, ale nie usuwa wąskiego gardła. To rozwiązanie marketingowe, nie operacyjne — i nie zawsze możliwe (rynek dyktuje cenę)."
      ),
      mniej_stolow = lc_feedback(
        type = "warning",
        "W optimum i tak produkujemy 0 stołów. Zmniejszenie produkcji stołów nie zmienia nic — limit już jest osiągnięty."
      )
    )
  })
}
