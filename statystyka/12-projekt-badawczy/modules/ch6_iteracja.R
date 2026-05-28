ch6_ui <- lecture_chapter(id = "ch6", num = "6", title = "Wynik nie kończy badania", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 06 · Iteracja",
      num = "06",
      title = "Wynik nie kończy badania.",
      lead = "Po pierwszym wyniku nie zamykamy projektu. Modyfikujemy hipotezę,
              dokładamy pytanie albo projektujemy lepszy pomiar."
    ),

    lc_h2("sec-01", "Co robimy po wyniku?"),

    div(class = "lc-figure-panel",
      h4("Co robimy z konkretnym wynikiem?"),
      selectInput("ch6_result", "Wybierz wynik z naszych danych:",
        choices = c(
          "beauty wiąże się z eval" = "beauty",
          "kobiety i mężczyźni mają różne średnie oceny" = "gender",
          "native speaker status wiąże się z oceną" = "native",
          "minority daje słabszy / niejednoznaczny trop" = "minority",
          "grupy wieku nie różnią się wyraźnie" = "age",
          "response rate może zmieniać interpretację" = "response"
        ),
        selected = "beauty"
      ),
      uiOutput("ch6_next_step")
    ),

    div(class = "lc-figure-panel",
      h4("Czego brakuje w danych?"),
      checkboxGroupInput("ch6_missing", NULL,
        choices = c(
          "Efekty uczenia się przed i po kursie" = "learning",
          "Trudność kursu i obciążenie pracą" = "difficulty",
          "Oczekiwana ocena/łatwość zaliczenia" = "grade",
          "Obowiązkowość kursu" = "required",
          "Styl prowadzenia i materiały" = "style",
          "Powody braku odpowiedzi w ankiecie" = "nonresponse"
        ),
        selected = c("learning", "difficulty")
      ),
      uiOutput("ch6_missing_summary")
    ),

    div(class = "lc-feedback lc-feedback-warning",
      tags$strong("Zdanie badacza:"),
      p(tags$em("\"Ten wynik nie kończy tematu. On mówi nam, które kolejne pytanie
        jest teraz najbardziej sensowne.\""))
    ),

    lc_chapter_next("07", "Jak zaprojektować lepsze badanie?",
      "Skoro dane obserwacyjne mają ograniczenia, projektujemy mocniejszy kolejny krok.",
      "ch_projekt"),
    div(style = "height: 40px;")
  )))
)

ch6_server <- function(input, output, session) {
  output$ch6_next_step <- renderUI({
    beauty_r <- cor.test(tr_data$beauty, tr_data$eval)
    gender_test <- tr_group_test("gender", "t")
    gender_diff <- tr_mean_diff("gender")
    native_test <- tr_group_test("native", "wilcox")
    native_diff <- tr_mean_diff("native")
    minority_test <- tr_group_test("minority", "wilcox")
    minority_diff <- tr_mean_diff("minority")
    age_test <- tr_multi_group_test("age_group", "anova")
    response_r <- cor.test(tr_data$response.rate, tr_data$eval)

    cases <- list(
      beauty = list(
        title = "Trop beauty-eval",
        result = paste0("Korelacja beauty z eval: r = ",
                        round(unname(beauty_r$estimate), 3),
                        ", p = ", tr_fmt_p(beauty_r$p.value), "."),
        rethink = "Nie mówimy jeszcze: atrakcyjność powoduje wyższe oceny. Mówimy: wygląd jest tropem, który współwystępuje z oceną.",
        next_steps = c(
          "Czy ten związek zostaje po uwzględnieniu płci, wieku i typu kursu?",
          "Czy beauty jest proxy czegoś innego: wieku, pewności siebie, stylu prowadzenia?",
          "Jak zaprojektować badanie, które oddzieli wygląd od jakości materiałów?"
        ),
        type = "warning"
      ),
      gender = list(
        title = "Różnica według płci prowadzącego",
        result = paste0("Różnica średnich (mężczyzna minus kobieta): ",
                        round(gender_diff$diff, 3),
                        ", p = ", tr_fmt_p(gender_test$p), "."),
        rethink = "To nie jest gotowy dowód nierównego traktowania. To sygnał, że oceny mogą zależeć od cech osoby, nie tylko od kursu.",
        next_steps = c(
          "Czy kobiety i mężczyźni prowadzą podobne typy kursów?",
          "Czy różnica wygląda tak samo dla niższych i wyższych kursów?",
          "Czy response rate różni się między tymi grupami?"
        ),
        type = "warning"
      ),
      native = list(
        title = "Różnica według native speaker statusu",
        result = paste0("Różnica średnich (native tak minus native nie): ",
                        round(native_diff$diff, 3),
                        ", p = ", tr_fmt_p(native_test$p), "."),
        rethink = "Ten wynik może dotyczyć języka, zrozumiałości, typu kursów albo oczekiwań studentów wobec prowadzącego.",
        next_steps = c(
          "Czy native speakerzy prowadzą inne kursy niż pozostali?",
          "Czy studenci oceniają jakość nauczania, czy łatwość komunikacji?",
          "Jak zebrać dane o języku prowadzenia, jasności wyjaśnień i typie zajęć?"
        ),
        type = "warning"
      ),
      minority = list(
        title = "Słabszy trop: minority",
        result = paste0("Różnica średnich (minority tak minus nie): ",
                        round(minority_diff$diff, 3),
                        ", p = ", tr_fmt_p(minority_test$p), "."),
        rethink = "Brak silnego wyniku nie oznacza, że pytanie o sprawiedliwość znika. Może efekt jest mały, zależny od kontekstu albo źle mierzony.",
        next_steps = c(
          "Czy grupa minority jest wystarczająco liczna?",
          "Czy różnice ujawniają się tylko w wybranych typach kursów?",
          "Czy potrzebujemy lepszego pomiaru doświadczeń prowadzących i studentów?"
        ),
        type = "info"
      ),
      age = list(
        title = "Brak wyraźnej różnicy między grupami wieku",
        result = paste0("ANOVA dla grup wieku: p = ", tr_fmt_p(age_test$p), "."),
        rethink = "To przykład sensownego pytania, które nie musi dać efektu. Możemy osłabić hipotezę wieku albo ją doprecyzować.",
        next_steps = c(
          "Czy wiek działa liniowo, czy tylko w połączeniu z tenure albo typem kursu?",
          "Czy grupy wieku są dobrym pomiarem doświadczenia?",
          "Czy studenci reagują na wiek, czy na styl prowadzenia zajęć?"
        ),
        type = "ok"
      ),
      response = list(
        title = "Response rate jako problem interpretacji",
        result = paste0("Korelacja response rate z eval: r = ",
                        round(unname(response_r$estimate), 3),
                        ", p = ", tr_fmt_p(response_r$p.value), "."),
        rethink = "Nawet gdy średnia ocena wygląda jasno, niska odpowiedź może oznaczać, że słyszymy tylko część grupy.",
        next_steps = c(
          "Czy kursy z niskim response rate są większe albo trudniejsze?",
          "Czy bardziej niezadowoleni studenci chętniej odpowiadają?",
          "Jak w projekcie zadbać o reprezentatywność ankiety?"
        ),
        type = "info"
      )
    )
    x <- cases[[input$ch6_result]]
    div(class = "question-card",
      h4(x$title),
      lc_feedback(
        tags$p(tags$strong("Co widzimy w danych? "), x$result),
        tags$p(tags$strong("Jak modyfikujemy hipotezę? "), x$rethink),
        type = x$type
      ),
      tags$p(tags$strong("Następne pytania:")),
      tags$ol(lapply(x$next_steps, tags$li))
    )
  })

  output$ch6_missing_summary <- renderUI({
    n <- length(input$ch6_missing)
    div(class = "lc-feedback lc-feedback-info",
      tags$strong("Projekt lepszego badania:"),
      p("Wybraliście ", tags$strong(n), " brakujące elementy. To jest dokładnie
        moment, w którym analiza danych przechodzi w projektowanie badania."),
      p("Zadanie dla grupy: z tych braków wybierzcie jeden, który najbardziej
        zmieniłby interpretację wyników.")
    )
  })
}
