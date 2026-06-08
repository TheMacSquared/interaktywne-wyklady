# ============================================================================
# CHAPTER 1: Regresja liniowa prosta
# ============================================================================

# Dane CASchools są wczytywane w helpers.R jako .cas_data / .cas_labels
# (wspólne dla ch1 i ch2).

.ch1_pred_specs <- list(
  read_students = list(
    label = "Czytanie ~ liczba uczniów",
    x = "students", y = "read", default = 2000, step = 100,
    unit = "uczniów",
    question = "Jaki będzie przewidywany średni wynik z czytania w okręgu o takiej liczbie uczniów?"
  ),
  math_income = list(
    label = "Matematyka ~ dochód okręgu",
    x = "income", y = "math", default = 15, step = 1,
    unit = "tys. USD dochodu",
    question = "Jaki będzie przewidywany średni wynik z matematyki przy takim dochodzie okręgu?"
  ),
  read_str = list(
    label = "Czytanie ~ uczniowie na nauczyciela",
    x = "student_teacher_ratio", y = "read", default = 20, step = 0.5,
    unit = "uczniów na nauczyciela",
    question = "Jaki będzie przewidywany średni wynik z czytania przy takim STR?"
  ),
  math_expenditure = list(
    label = "Matematyka ~ wydatki na ucznia",
    x = "expenditure", y = "math", default = 6000, step = 100,
    unit = "wydatków na ucznia",
    question = "Jaki będzie przewidywany średni wynik z matematyki przy takich wydatkach na ucznia?"
  )
)

.ch1_pred_choices <- setNames(names(.ch1_pred_specs), vapply(.ch1_pred_specs, `[[`, character(1), "label"))

ch1_ui <- list(
  id    = "ch-liniowa",
  num   = "01",
  title = "Regresja liniowa prosta",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 01 · Regresja",
      num    = "01",
      title  = "Regresja liniowa prosta.",
      lead   = "Korelacja mówiła, czy dwie zmienne są powiązane.
                Regresja idzie dalej: modeluje ten związek i pozwala predykować."
    ),

    lc_h2("ch1-od-korelacji", "Od korelacji do regresji"),

    tagList(
      p("W poprzednim wykładzie pytaliśmy, ", tags$em("czy"),
        " dwie zmienne idą razem — i mierzyliśmy to korelacją. Teraz pytanie się
        przesuwa: ", tags$em("o ile"), " zmienia się Y, kiedy X rośnie o jedną
        jednostkę? Korelacja sama z siebie tego nie odpowie; potrzebujemy modelu,
        który da konkretne liczby — i pozwoli przewidywać Y dla nowych X."),
      p("Najprostszy taki model to linia prosta. Zanim ją jednak narysujemy,
        zawsze warto najpierw rzucić okiem na wykres rozrzutu: regresja liniowa
        ma sens dopiero wtedy, gdy chmura punktów układa się w przybliżeniu
        wzdłuż prostej. Jeśli widać krzywiznę albo dwie chmury, prosta będzie
        kłamać niezależnie od tego, jak ładnie policzą się współczynniki."),
      p("Formalnie regresja liniowa prosta zapisuje związek X → Y tak:"),
      lc_formula_box(
        withMathJax(helpText("$$Y = \\beta_0 + \\beta_1 X + \\varepsilon$$")),
        p(withMathJax("\\(\\beta_0\\)"), " — wyraz wolny (intercept): wartość Y gdy X = 0"),
        p(withMathJax("\\(\\beta_1\\)"), " — nachylenie (slope): o ile zmieni się Y, gdy X wzrośnie o 1"),
        p(withMathJax("\\(\\varepsilon\\)"), " — błąd losowy (reszty)")
      ),
      p("Greckie litery ", withMathJax("\\(\\beta_0, \\beta_1\\)"),
        " to prawdziwe, populacyjne parametry — nieznane.
        Z próby liczymy ich estymatory, oznaczane małymi literami ",
        withMathJax("\\(b_0, b_1\\)"),
        ". Zaraz zobaczysz, jak każdy z tych elementów wpływa na kształt linii.")
    ),

    figure_panel(
      label = "Ryc. 1.0", title = "Co robią β₀, β₁ i szum?",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch1_beta_b0", "β₀ (punkt startu):",
                      min = -10, max = 20, value = 5, step = 1),
          sliderInput("ch1_beta_b1", "β₁ (nachylenie):",
                      min = -3, max = 3, value = 1, step = 0.25),
          sliderInput("ch1_beta_sigma", "Szum σ:",
                      min = 0, max = 8, value = 2, step = 0.5)
        ),
        column(8,
          zoom_plot_ui("ch1_beta_plot", height = "320px"),
          uiOutput("ch1_beta_info")
        )
      )
    ),

    tagList(
      p("Suwakami sterowałeś trzema wielkościami: ",
        withMathJax("\\(\\beta_0\\)"), " podnosił całą linię w górę i w dół,
        ", withMathJax("\\(\\beta_1\\)"), " ją przekręcał, a ",
        withMathJax("\\(\\sigma\\)"),
        " rozsypywał punkty wokół niej. Ale to była ręczna animacja: my
        ustalaliśmy parametry i patrzyliśmy, co z nich wynika."),
      p("W praktyce mamy odwrotny problem: widzimy ", tags$em("chmurę punktów"),
        " i potrzebujemy z niej wyłuskać ", withMathJax("\\(b_0\\)"), " i ",
        withMathJax("\\(b_1\\)"),
        ". W rozdziale o korelacji policzyliśmy już r i odchylenia standardowe —
        okaże się, że to wystarczy, żeby od razu napisać równanie prostej.")
    ),

    lc_h2("ch1-korelacja-regresja", "Regresja z korelacji"),

    figure_panel(
      label = "Ryc. 1.1", title = "Jak policzyć regresję z korelacji?",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Dla jednej zmiennej X i jednej Y nachylenie regresji można policzyć bez optymalizacji: z korelacji i odchyleń standardowych."),
          actionButton("ch1_corr_new", "Nowa próba",
                       class = "lc-btn-primary", width = "100%"),
          hr(),
          h5("Kroki:"),
          actionButton("ch1_corr_step1", "1. Średnie X i Y",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_corr_step2", "2. Odchylenia standardowe",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_corr_step3", "3. Korelacja r",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_corr_step4", "4. Nachylenie b₁",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_corr_step5", "5. Wyraz wolny b₀",
                       class = "lc-btn-outline", width = "100%")
        ),
        column(8,
          zoom_plot_ui("ch1_corr_plot", height = "360px"),
          uiOutput("ch1_corr_info")
        )
      )
    ),

    tagList(
      p("Recepta jest więc prosta: jedno r, dwa odchylenia standardowe i dwie
        średnie wystarczą, żeby wyznaczyć linię. W rzeczywistej pracy nikt nie
        robi tego ręcznie — wpisujemy do R jedną komendę i dostajemy gotową
        tabelę regresji: kolumny z estymatorami, błędami standardowymi, statystykami t i
        p-value. Cały dalszy rozdział będzie ćwiczeniem w odczytywaniu właśnie
        takich tabel."),
      p("Zacznijmy od najprostszego ruchu: dostajesz tabelę z dwiema liczbami
        (", withMathJax("\\(b_0\\)"), " i ", withMathJax("\\(b_1\\)"),
        ") i twoim zadaniem jest narysować prostą, którą ta tabela opisuje.")
    ),

    lc_h2("ch1-rysuj-z-tabeli", "Ćwiczenie: narysuj prostą z tabeli"),

    figure_panel(
      label = "Ćwiczenie", title = "Kliknij dwa punkty, przez które przechodzi prosta",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Przeczytaj tabelę współczynników. Potem kliknij na wykresie dwa punkty, które wyznaczają prostą regresji."),
          uiOutput("ch1_draw_table"),
          actionButton("ch1_draw_reset", "Wyczyść punkty",
                       class = "lc-btn-secondary-outline", width = "100%"),
          actionButton("ch1_draw_reveal", "Pokaż odpowiedź",
                       class = "lc-btn-ok", width = "100%"),
          actionButton("ch1_draw_new", "Nowe ćwiczenie",
                       class = "lc-btn-primary", width = "100%"),
          uiOutput("ch1_draw_feedback")
        ),
        column(8,
          zoom_plot_ui("ch1_draw_plot", height = "360px",
                     click = "ch1_draw_plot_click"),
          uiOutput("ch1_draw_stats")
        )
      )
    ),

    tagList(
      p("Mając gotowe ", withMathJax("\\(b_0\\)"), " i ",
        withMathJax("\\(b_1\\)"),
        " z tabeli, narysowanie prostej jest mechaniczne. Ale przewińmy
        pytanie o krok wstecz: skąd komputer wziął te dwie liczby?
        Spośród nieskończenie wielu prostych, które dałoby się przeciągnąć
        przez chmurę punktów, musi wybrać jedną. Według jakiego kryterium?"),
      p("Zasada nazywa się ", tags$em("metodą najmniejszych kwadratów (MNK / OLS)"),
        ": wybieramy taką prostą, która minimalizuje sumę kwadratów pionowych
        odległości między punktami a linią. Następny widget rozkłada ten pomysł
        na sześć kroków.")
    ),

    lc_h2("ch1-ols-krok", "Najmniejsze kwadraty — krok po kroku"),

    figure_panel(
      label = "Ryc. 1.1b", title = "Jak linia staje się modelem",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Ta sama próba, kolejne warstwy interpretacji."),
          actionButton("ch1_ols_new", "Nowa próba",
                       class = "lc-btn-primary", width = "100%"),
          hr(),
          h5("Kroki:"),
          actionButton("ch1_ols_step1", "1. Dane",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_ols_step2", "2. Średnia Y",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_ols_step3", "3. Linia regresji",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_ols_step4", "4. Reszty",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_ols_step5", "5. Wynik modelu",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_ols_step6", "6. Inna prosta?",
                       class = "lc-btn-outline", width = "100%")
        ),
        column(8,
          zoom_plot_ui("ch1_ols_plot", height = "360px"),
          uiOutput("ch1_ols_info")
        )
      )
    ),

    lc_h2("ch1-reszty", "Reszty i dlaczego kwadraty"),

    tagList(
      p("Te pionowe odcinki, które pojawiły się w kroku 4, mają swoją nazwę:
        to reszty. Każda obserwacja ma własną resztę — różnicę między tym, co
        zobaczyliśmy, a tym, co przewiduje model:"),
      lc_formula_box(
        withMathJax(helpText("$$e_i = y_i - \\hat{y}_i$$"))
      ),
      p("Reszta ze znakiem mówi nam, czy konkretny punkt leży nad linią
        (", withMathJax("\\(e_i > 0\\)"), "), czy pod nią (",
        withMathJax("\\(e_i < 0\\)"),
        "). MNK nie dba o znak — sumuje kwadraty. Dlaczego nie sumy wartości
        bezwzględnych?"),
      p("Powody są dwa, jeden praktyczny i jeden matematyczny. Po pierwsze,
        kwadraty ", tags$em("karzą większe błędy nieproporcjonalnie mocno"),
        ": jeden punkt oddalony o 4 jednostki przeszkadza tak, jak szesnaście
        punktów oddalonych o 1. To zmusza prostą, by raczej trochę odsunąć się
        od każdej dużej obserwacji, niż zignorować skrajne odchylenia."),
      p("Po drugie, kwadraty są ", tags$em("różniczkowalne"),
        " — dzięki temu zadanie optymalizacji ma jedno, jawne rozwiązanie. To
        właśnie ten wzór, który widzieliśmy wcześniej: ",
        withMathJax("\\(b_1 = r \\cdot s_Y / s_X\\)"),
        ". Gdybyśmy minimalizowali wartości bezwzględne, dostalibyśmy regresję
        ", tags$em("medianową"),
        " — sensowną, ale bez wzoru zamkniętego i trudniejszą obliczeniowo."),
      inline_callout(label = "Zapamiętaj", color = "wskazowka",
        "Diagnostyka modelu polega głównie na patrzeniu w reszty. Jeśli układają
         się w wachlarz albo w łuk, znaczy, że linia kłamie — wrócimy do tego
         w rozdziale o jakości modelu."
      )
    ),

    lc_h2("ch1-pvalue", "p-value dla nachylenia"),

    tagList(
      p("Mamy linię, mamy reszty, mamy wzór. Ale ", withMathJax("\\(b_1\\)"),
        " policzone z jednej próby to nie to samo, co prawdziwe nachylenie
        w populacji. Gdybyśmy wzięli inną grupę 65 obserwacji, dostalibyśmy
        trochę inne ", withMathJax("\\(b_1\\)"),
        ". Pytanie brzmi: czy to, co widzimy, jest naprawdę różne od zera, czy
        równie dobrze mogłoby się zdarzyć, gdyby X i Y w populacji były od siebie
        niezależne?"),
      p("Wzór na ", withMathJax("\\(b_1\\)"),
        " ma swój brat-cień: błąd standardowy ",
        withMathJax("\\(SE(b_1)\\)"),
        ", który mierzy, jak bardzo nasza estymata mogłaby się chwiać między
        próbami. Statystyka testowa jest właściwie ilorazem — ",
        withMathJax("\\(t = b_1 / SE(b_1)\\)"),
        " — i mówi, ", tags$em("ile błędów standardowych"),
        " dzieli nasze nachylenie od zera. Im dalej, tym mniej prawdopodobne,
        że to przypadek."),
      p("Sformalizowane:"),
      lc_formula_box(
        withMathJax(helpText("$$H_0: \\beta_1 = 0 \\quad\\text{brak liniowego wpływu X na Y}$$")),
        withMathJax(helpText("$$H_a: \\beta_1 \\neq 0 \\quad\\text{nachylenie jest różne od zera}$$"))
      ),
      p("Małe p-value mówi: gdyby prawdziwe ", withMathJax("\\(\\beta_1\\)"),
        " wynosiło zero, zobaczenie tak skrajnego ", withMathJax("\\(b_1\\)"),
        " byłoby mało prawdopodobne. To dokładnie ten sam mechanizm, który
        widziałeś w teście t — kolumny ", tags$em("Estimate, SE, t, p"),
        " w tabeli regresji to jego standardowy raport.")
    ),

    figure_panel(
      label = "Ryc. 1.2", title = "Kiedy nachylenie jest istotne?",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch1_pval_scenario", "Scenariusz:",
            choices = c(
              "Wyraźny dodatni wpływ" = "strong_positive",
              "Brak wpływu" = "none",
              "Wyraźny ujemny wpływ" = "strong_negative",
              "Ten sam trend, mała próba" = "small_sample"
            ),
            selected = "strong_positive"
          ),
          uiOutput("ch1_pval_table"),
          uiOutput("ch1_pval_verdict")
        ),
        column(8,
          zoom_plot_ui("ch1_pval_plot", height = "360px"),
          uiOutput("ch1_pval_stats")
        )
      )
    ),

    tagList(
      p("Symulacja jest wygodna, bo my znamy prawdziwe ",
        withMathJax("\\(\\beta_1\\)"),
        " — sami je ustawiliśmy. W rzeczywistych danych jesteśmy ślepi: widzimy
        tylko próbę. Spróbujmy więc tej samej procedury na realnym zbiorze."),
      p("CASchools to dane o około 420 okręgach szkolnych w Kalifornii z lat 90.
        Każdy wiersz to jeden okręg, każda kolumna — jeden mierzony parametr:
        dochód w tysiącach dolarów, wydatki na ucznia, stosunek liczby uczniów
        do nauczycieli (STR), procent dzieci z angielskim jako drugim językiem,
        średnie wyniki z czytania i matematyki. To dane, na których ekonomiści
        edukacji testowali hipotezę: czy mniejsze klasy poprawiają wyniki?"),
      p("Wybierz parę zmiennych i zanim klikniesz „Pokaż odpowiedź”,
        popatrz na chmurę i na tabelę: czy znak ", withMathJax("\\(b_1\\)"),
        " pasuje do intuicji? Czy ", withMathJax("\\(p\\)"),
        " jest dość małe, żeby odrzucić H₀? Dopiero potem porównaj swoją diagnozę
        z werdyktem widgetu.")
    ),

    lc_h2("ch1-caschool", "Regresja na danych CASchools"),

    figure_panel(
      label = "Ryc. 1.4", title = "CASchools: od outputu do interpretacji",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Wybierz zmienne, obejrzyj wykres i tabelę regresji. Najpierw samodzielnie zdecyduj, czy X istotnie przewiduje Y, a potem pokaż odpowiedź."),
          selectInput("ch1_cas_x", "Zmienna X:",
            choices = c(
              "Dochód okręgu (income)" = "income",
              "Uczniowie na nauczyciela (STR)" = "student_teacher_ratio",
              "Wydatki na ucznia (expenditure)" = "expenditure",
              "Udział uczniów z angielskim jako drugim językiem (english)" = "english",
              "Udział lunch subsydiowany (lunch)" = "lunch",
              "Komputery" = "computer",
              "Zakres klas (grades)" = "grades"
            ),
            selected = "income"
          ),
          selectInput("ch1_cas_y", "Zmienna Y:",
            choices = c(
              "Czytanie (read)" = "read",
              "Matematyka (math)" = "math",
              "Dochód okręgu (income)" = "income",
              "Wydatki na ucznia (expenditure)" = "expenditure",
              "Uczniowie na nauczyciela (STR)" = "student_teacher_ratio"
            ),
            selected = "read"
          ),
          actionButton("ch1_cas_reveal", "Pokaż odpowiedź",
                       class = "lc-btn-ok", width = "100%"),
          uiOutput("ch1_cas_answer")
        ),
        column(8,
          zoom_plot_ui("ch1_cas_plot", height = "360px"),
          uiOutput("ch1_cas_table"),
          uiOutput("ch1_cas_summary")
        )
      )
    ),

    tagList(
      p("Do tej pory traktowaliśmy regresję jako narzędzie do opisu zależności:
        czy istnieje, jaki ma znak, czy jest istotna. Ale model regresji ma drugie
        zastosowanie, równie ważne: przewidywanie. Skoro mamy równanie ",
        withMathJax("\\(\\hat{Y} = b_0 + b_1 X\\)"),
        ", możemy podstawić dowolne X i odczytać oczekiwane Y."),
      p("Trzeba tylko pamiętać, co ta liczba znaczy: predykcja to średnia warunkowa
        — najlepszy strzał w Y dla okręgów o danym X, ", tags$em("nie"),
        " obietnica konkretnej wartości. Jeśli dla okręgu o dochodzie 20 tys.
        USD model daje ", withMathJax("\\(\\hat{Y} = 658\\)"),
        ", to nie znaczy, że ", tags$em("każdy"),
        " taki okręg dostanie 658 — znaczy, że średnio okręgi o tym dochodzie
        kręcą się wokół 658."),
      p("Sam rachunek jest banalny: podstaw X do równania. Spróbuj.")
    ),

    lc_h2("ch1-predykcja", "Predykcja z modelu"),

    figure_panel(
      label = "Ryc. 1.5", title = "Użyj równania regresji do przewidywania",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Wybierz gotowy model, ustaw wartość X i spróbuj policzyć przewidywane Y z równania regresji."),
          selectInput("ch1_pred_case", "Model:",
            choices = .ch1_pred_choices,
            selected = "read_students"
          ),
          uiOutput("ch1_pred_x_input"),
          uiOutput("ch1_pred_question"),
          actionButton("ch1_pred_reveal", "Pokaż odpowiedź",
                       class = "lc-btn-ok", width = "100%"),
          uiOutput("ch1_pred_answer")
        ),
        column(8,
          zoom_plot_ui("ch1_pred_plot", height = "360px"),
          uiOutput("ch1_pred_table"),
          uiOutput("ch1_pred_stats")
        )
      )
    ),

    lc_h2("ch1-co-dalej", "Co zostawiamy na potem"),

    tagList(
      p("W jednym rozdziale przeszliśmy od chmury punktów do równania prostej,
        nauczyliśmy się czytać tabelę regresji i przewidywać Y dla nowego X.
        Świadomie jednak pominęliśmy kilka rzeczy, do których wrócimy."),
      tags$ul(
        tags$li("Co czyni model dobrym: kiedy wolno ufać prostej? Reszty zdradzają, czy model się
                 nadaje; R² i RMSE mówią, ile wyjaśnia i jak duże robi
                 pomyłki — to temat rozdziału 2."),
        tags$li("Wiele predyktorów: jak dołączyć drugą i trzecią zmienną X, kiedy STR ", tags$em("i"),
                " wydatki ", tags$em("i"),
                " dochód wpływają na wyniki naraz — rozdział 3."),
        tags$li("Porównywanie modeli: kiedy bogatszy model jest lepszy, a kiedy tylko przepasowany —
                 rozdział 4. Tam dochodzą R²adj, AIC, BIC i train/test.")
      ),
      p("Linia regresji jest w wykresach od ponad stu lat. Reszta tego wykładu
        pokaże, dlaczego mimo prostoty ciągle bywa nadużywana — i jak tego nie
        robić.")
    ),

    lc_chapter_next(
      num       = "02",
      title     = "Co czyni model dobrym?",
      lead      = "reszty, R², RMSE — diagnostyka pojedynczego modelu",
      target_id = "ch-jakosc"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  zoom_plot_server("ch1_beta_plot", reactive({
    set.seed(101)
    x <- seq(0, 10, length.out = 80)
    y_true <- input$ch1_beta_b0 + input$ch1_beta_b1 * x
    y <- y_true + rnorm(length(x), 0, input$ch1_beta_sigma)
    df <- data.frame(x = x, y = y, y_true = y_true)

    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = upwr_secondary, alpha = 0.45) +
      geom_line(aes(y = y_true), color = unname(upwr_cat["niebo"]), linewidth = 1.3) +
      annotate("segment", x = 0, xend = 0, y = 0, yend = input$ch1_beta_b0,
               color = unname(upwr_cat["bursztyn"]), linewidth = 1.1) +
      annotate("text", x = 0.6, y = input$ch1_beta_b0,
               label = paste0("β₀ = ", input$ch1_beta_b0),
               hjust = 0, color = unname(upwr_cat["bursztyn"]), fontface = "bold") +
      labs(x = "X", y = "Y") +
      theme_upwr()
  }))

  output$ch1_beta_info <- renderUI({
    direction <- if (input$ch1_beta_b1 > 0) "rośnie" else if (input$ch1_beta_b1 < 0) "maleje" else "nie zmienia się"
    lc_feedback(type = "info",
      p(tags$strong("Interpretacja:"),
        paste0(" gdy X wzrasta o 1, oczekiwane Y ", direction,
               " o ", abs(input$ch1_beta_b1), ". Szum σ = ",
               input$ch1_beta_sigma, " rozprasza punkty wokół linii."))
    )
  })

  # --- Widget: regresja z korelacji ---
  ch1_corr_data <- reactiveVal(generate_regression_data(n = 65, beta0 = 8, beta1 = 1.6, sigma = 4))
  ch1_corr_step <- reactiveVal(0)

  observeEvent(input$ch1_corr_new, {
    ch1_corr_data(generate_regression_data(n = 65, beta0 = 8, beta1 = 1.6, sigma = 4))
    ch1_corr_step(0)
  })
  observeEvent(input$ch1_corr_step1, ch1_corr_step(1))
  observeEvent(input$ch1_corr_step2, ch1_corr_step(2))
  observeEvent(input$ch1_corr_step3, ch1_corr_step(3))
  observeEvent(input$ch1_corr_step4, ch1_corr_step(4))
  observeEvent(input$ch1_corr_step5, ch1_corr_step(5))

  zoom_plot_server("ch1_corr_plot", reactive({
    df <- ch1_corr_data()
    step <- ch1_corr_step()
    x_bar <- mean(df$x)
    y_bar <- mean(df$y)
    r <- cor(df$x, df$y)
    b1 <- r * sd(df$y) / sd(df$x)
    b0 <- y_bar - b1 * x_bar
    slope_x0 <- x_bar + 0.4
    slope_x1 <- slope_x0 + 1
    slope_y0 <- b0 + b1 * slope_x0
    slope_y1 <- b0 + b1 * slope_x1
    slope_y_mid <- (slope_y0 + slope_y1) / 2
    slope_y_pad <- max(1.8, abs(b1) * 1.4)

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_point(color = upwr_secondary,
                 alpha = if (step == 4) 0.24 else 0.55,
                 size = if (step == 4) 1.8 else 2.2) +
      labs(x = "X", y = "Y") +
      theme_upwr()

    if (step >= 1 && !(step %in% c(4, 5))) {
      p <- p +
        geom_vline(xintercept = x_bar, linetype = "dashed",
                   color = unname(upwr_cat["bursztyn"]), linewidth = 0.9) +
        geom_hline(yintercept = y_bar, linetype = "dashed",
                   color = unname(upwr_cat["bursztyn"]), linewidth = 0.9)
    }
    if (step == 2) {
      p <- p +
        geom_segment(aes(xend = x_bar, yend = y),
                     color = unname(upwr_cat["niebo"]),
                     linetype = "dashed", alpha = 0.35, linewidth = 0.5) +
        geom_segment(aes(xend = x, yend = y_bar),
                     color = unname(upwr_cat["terakota"]),
                     linetype = "dashed", alpha = 0.35, linewidth = 0.5) +
        annotate("text", x = x_bar, y = max(df$y),
                 label = "odchylenia X", hjust = -0.05, vjust = 1,
                 color = unname(upwr_cat["niebo"]), fontface = "bold") +
        annotate("text", x = min(df$x), y = y_bar,
                 label = "odchylenia Y", hjust = 0, vjust = -0.7,
                 color = unname(upwr_cat["terakota"]), fontface = "bold")
    }
    if (step == 3) {
      p <- p + geom_abline(intercept = b0, slope = b1,
                           color = unname(upwr_cat["niebo"]), linewidth = 1.3)
    }
    if (step == 4) {
      p <- p +
        geom_abline(intercept = b0, slope = b1,
                    color = unname(upwr_cat["niebo"]), linewidth = 1.8) +
        geom_segment(aes(x = slope_x0, xend = slope_x1,
                         y = slope_y0, yend = slope_y0),
                     inherit.aes = FALSE,
                     color = unname(upwr_cat["bursztyn"]),
                     linewidth = 1.2,
                     arrow = arrow(length = grid::unit(0.12, "inches"))) +
        geom_segment(aes(x = slope_x1, xend = slope_x1,
                         y = slope_y0, yend = slope_y1),
                     inherit.aes = FALSE,
                     color = unname(upwr_cat["terakota"]),
                     linewidth = 1.2,
                     arrow = arrow(length = grid::unit(0.12, "inches"))) +
        geom_point(
          data = data.frame(
            x = c(slope_x0, slope_x1, slope_x1),
            y = c(slope_y0, slope_y0, slope_y1)
          ),
          aes(x = x, y = y),
          inherit.aes = FALSE,
          color = upwr_secondary,
          fill = "white",
          shape = 21,
          stroke = 1.1,
          size = 3.2
        ) +
        annotate("text", x = (slope_x0 + slope_x1) / 2, y = slope_y0,
                 label = "ΔX = 1", vjust = 1.6,
                 color = unname(upwr_cat["bursztyn"]), fontface = "bold") +
        annotate("text", x = slope_x1, y = (slope_y0 + slope_y1) / 2,
                 label = paste0("ΔY = b₁ = ", round(b1, 2)),
                 hjust = -0.08,
                 color = unname(upwr_cat["terakota"]), fontface = "bold") +
        coord_cartesian(
          xlim = c(slope_x0 - 1.1, slope_x1 + 1.45),
          ylim = c(slope_y_mid - slope_y_pad, slope_y_mid + slope_y_pad)
        )
    }
    if (step == 5) {
      p <- p +
        geom_abline(intercept = 0, slope = b1,
                    color = unname(upwr_cat["bursztyn"]),
                    linewidth = 1.2, linetype = "longdash") +
        geom_abline(intercept = b0, slope = b1,
                    color = unname(upwr_cat["niebo"]), linewidth = 1.5) +
        geom_segment(aes(x = 0, xend = 0,
                         y = 0, yend = b0),
                     inherit.aes = FALSE,
                     color = unname(upwr_cat["terakota"]),
                     linewidth = 1.2,
                     arrow = arrow(length = grid::unit(0.12, "inches"),
                                   ends = "both")) +
        geom_point(
          data = data.frame(x = 0, y = c(0, b0)),
          aes(x = x, y = y),
          inherit.aes = FALSE,
          color = upwr_secondary,
          fill = "white",
          shape = 21,
          stroke = 1.1,
          size = 3
        ) +
        annotate("text", x = min(df$x), y = b1 * min(df$x),
                 label = "b[0] == 0", parse = TRUE,
                 hjust = 0, vjust = -0.6,
                 color = unname(upwr_cat["bursztyn"]), fontface = "bold") +
        annotate("text", x = 0.15, y = b0 / 2,
                 label = paste0("b[0] == ", round(b0, 2)), parse = TRUE,
                 hjust = 0,
                 color = unname(upwr_cat["terakota"]), fontface = "bold")
    }
    if (step == 0) {
      p <- p + annotate("text", x = mean(df$x), y = mean(df$y),
                        label = "Klikaj kroki po lewej", color = upwr_reference, size = 5)
    }
    p
  }))

  output$ch1_corr_info <- renderUI({
    df <- ch1_corr_data()
    step <- ch1_corr_step()
    if (step == 0) return(NULL)

    x_bar <- mean(df$x)
    y_bar <- mean(df$y)
    sx <- sd(df$x)
    sy <- sd(df$y)
    r <- cor(df$x, df$y)
    b1 <- r * sy / sx
    b0 <- y_bar - b1 * x_bar

    tagList(
      lc_stat_grid(
        if (step >= 1) lc_stat_box("x̄", round(x_bar, 2), color = unname(upwr_cat["bursztyn"])),
        if (step >= 1) lc_stat_box("ȳ", round(y_bar, 2), color = unname(upwr_cat["bursztyn"])),
        if (step >= 2) lc_stat_box("sX", round(sx, 2), color = upwr_secondary),
        if (step >= 2) lc_stat_box("sY", round(sy, 2), color = upwr_secondary),
        if (step >= 3) lc_stat_box("r", round(r, 3), color = unname(upwr_cat["szalwia"])),
        columns = if (step >= 3) 5 else 4
      ),
      if (step >= 4) lc_formula_box(
        withMathJax(helpText(sprintf("$$b_1 = r \\cdot \\frac{s_Y}{s_X} = %.3f \\cdot \\frac{%.2f}{%.2f} = %.3f$$",
                                     r, sy, sx, b1)))
      ),
      if (step >= 5) lc_formula_box(
        withMathJax(helpText(sprintf("$$b_0 = \\bar{y} - b_1\\bar{x} = %.2f - %.3f \\cdot %.2f = %.2f$$",
                                     y_bar, b1, x_bar, b0)))
      ),
      if (step >= 5) lc_feedback(type = "ok",
        tags$div(style = "font-weight: 700; margin-bottom: 6px;", "Końcowy model:"),
        withMathJax(tags$div(
          style = "font-size: 1.35rem; font-weight: 700; text-align: center;",
          sprintf("$$\\hat{Y} = %.2f + %.3fX$$", b0, b1)
        ))
      ),
      if (step >= 5) lc_feedback(type = "info",
        p("To jest ta sama prosta, którą zwraca klasyczna regresja liniowa dla jednego predyktora. Korelacja ustala kierunek i siłę związku, a iloraz odchyleń standardowych przelicza ją na jednostki X i Y.")
      )
    )
  })

  # --- Cwiczenie: narysuj prosta z outputu regresji ---
  ch1_draw_model <- reactiveVal(NULL)
  ch1_draw_points <- reactiveVal(data.frame(x = numeric(), y = numeric()))
  ch1_draw_revealed <- reactiveVal(FALSE)

  .ch1_new_draw_model <- function() {
    beta0 <- runif(1, 2.5, 8.5)
    beta1 <- sample(c(-1.6, -1.2, -0.8, 0.8, 1.2, 1.6), 1)
    x <- runif(35, -4.5, 4.5)
    y <- beta0 + beta1 * x + rnorm(length(x), 0, 1.2)
    list(
      beta0 = beta0,
      beta1 = beta1,
      data = data.frame(x = x, y = y)
    )
  }

  ch1_draw_model(.ch1_new_draw_model())

  observeEvent(input$ch1_draw_new, {
    ch1_draw_model(.ch1_new_draw_model())
    ch1_draw_points(data.frame(x = numeric(), y = numeric()))
    ch1_draw_revealed(FALSE)
  })

  observeEvent(input$ch1_draw_reset, {
    ch1_draw_points(data.frame(x = numeric(), y = numeric()))
    ch1_draw_revealed(FALSE)
  })

  observeEvent(input$ch1_draw_reveal, {
    req(nrow(ch1_draw_points()) == 2)
    ch1_draw_revealed(TRUE)
  })

  observeEvent(input$ch1_draw_plot_click, {
    if (ch1_draw_revealed()) return()
    click <- input$ch1_draw_plot_click
    pts <- ch1_draw_points()
    new_pt <- data.frame(x = click$x, y = click$y)
    if (nrow(pts) >= 2) {
      pts <- new_pt
    } else {
      pts <- rbind(pts, new_pt)
    }
    ch1_draw_points(pts)
  })

  output$ch1_draw_table <- renderUI({
    model <- ch1_draw_model()
    tags$table(class = "lc-table lc-table-bordered lc-table-sm",
      tags$thead(
        tags$tr(
          tags$th("Term"),
          tags$th("Estimate")
        )
      ),
      tags$tbody(
        tags$tr(tags$td("wyraz wolny"), tags$td(sprintf("%.2f", model$beta0))),
        tags$tr(tags$td("X"), tags$td(sprintf("%.2f", model$beta1)))
      )
    )
  })

  zoom_plot_server("ch1_draw_plot", reactive({
    model <- ch1_draw_model()
    pts <- ch1_draw_points()
    revealed <- ch1_draw_revealed()
    x_min <- -5
    x_max <- 5
    y_min <- -4
    y_max <- 17
    grid_df <- data.frame(x = c(x_min, x_max), y = c(y_min, y_max))

    p <- ggplot(grid_df, aes(x = x, y = y)) +
      geom_blank() +
      geom_hline(yintercept = 0, color = upwr_rule, linewidth = 0.6) +
      geom_vline(xintercept = 0, color = upwr_rule, linewidth = 0.6) +
      coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE) +
      scale_x_continuous(breaks = seq(x_min, x_max, by = 1)) +
      scale_y_continuous(breaks = seq(y_min, y_max, by = 1)) +
      labs(x = "X", y = "Y") +
      theme_upwr()

    if (revealed) {
      p <- p +
        geom_point(data = model$data, aes(x = x, y = y),
                   inherit.aes = FALSE,
                   color = upwr_secondary, alpha = 0.45, size = 2)
    }

    if (nrow(pts) > 0) {
      p <- p +
        geom_point(data = pts, aes(x = x, y = y),
                   inherit.aes = FALSE,
                   color = unname(upwr_cat["terakota"]),
                   fill = "white", shape = 21, stroke = 1.2, size = 3.6) +
        geom_text(data = pts, aes(x = x, y = y, label = seq_len(nrow(pts))),
                  inherit.aes = FALSE,
                  color = unname(upwr_cat["terakota"]),
                  fontface = "bold", vjust = -1)
    }

    if (nrow(pts) == 2 && abs(diff(pts$x)) >= 0.05) {
      user_b1 <- diff(pts$y) / diff(pts$x)
      user_b0 <- pts$y[1] - user_b1 * pts$x[1]
      p <- p +
        geom_abline(intercept = user_b0, slope = user_b1,
                    color = unname(upwr_cat["terakota"]),
                    linewidth = 1.4, linetype = "longdash")
      if (revealed) {
        p <- p +
        geom_abline(intercept = model$beta0, slope = model$beta1,
                    color = unname(upwr_cat["niebo"]), linewidth = 1.5) +
        annotate("text", x = x_min + 0.25, y = y_max - 0.8,
                 label = "poprawna prosta", hjust = 0,
                 color = unname(upwr_cat["niebo"]), fontface = "bold") +
        annotate("text", x = x_min + 0.25, y = y_max - 1.8,
                 label = "Twoja prosta", hjust = 0,
                 color = unname(upwr_cat["terakota"]), fontface = "bold")
      } else {
        p <- p +
          annotate("text", x = x_min + 0.25, y = y_max - 0.8,
                   label = "Twoja prosta", hjust = 0,
                   color = unname(upwr_cat["terakota"]), fontface = "bold") +
          annotate("text", x = 0, y = y_max - 1,
                   label = "Kliknij „Pokaż odpowiedź”",
                   color = upwr_reference, size = 5)
      }
    } else if (nrow(pts) == 2) {
      p <- p + annotate("text", x = 0, y = y_max - 1,
                        label = "Wybierz punkty bardziej oddalone poziomo",
                        color = unname(upwr_cat["terakota"]), size = 5)
    } else {
      p <- p + annotate("text", x = 0, y = y_max - 1,
                        label = "Kliknij dwa punkty na wykresie",
                        color = upwr_reference, size = 5)
    }

    p
  }))

  output$ch1_draw_feedback <- renderUI({
    pts <- ch1_draw_points()
    if (nrow(pts) < 2) {
      return(lc_feedback(type = "info", style = "margin-top: 12px;",
        p(if (nrow(pts) == 0) {
          "Kliknij pierwszy punkt prostej."
        } else {
          "Kliknij drugi punkt prostej."
        })
      ))
    }
    if (!ch1_draw_revealed()) {
      return(lc_feedback(type = "warning", style = "margin-top: 12px;",
        p("Gotowe. Kliknij „Pokaż odpowiedź”, żeby porównać z modelem.")
      ))
    }

    lc_feedback(type = "ok", style = "margin-top: 12px;",
      p("Porównaj czerwoną przerywaną prostą z niebieską poprawną prostą.")
    )
  })

  output$ch1_draw_stats <- renderUI({
    model <- ch1_draw_model()
    pts <- ch1_draw_points()
    if (nrow(pts) < 2 || !ch1_draw_revealed()) return(NULL)

    if (abs(diff(pts$x)) < 0.05) {
      return(lc_feedback(type = "warning",
        p("Punkty mają prawie ten sam X. Wybierz dwa punkty bardziej oddalone poziomo.")
      ))
    }

    user_b1 <- diff(pts$y) / diff(pts$x)
    user_b0 <- pts$y[1] - user_b1 * pts$x[1]
    tagList(
      lc_stat_grid(
        lc_stat_box("Twoje b₀", round(user_b0, 2), color = unname(upwr_cat["terakota"])),
        lc_stat_box("Poprawne b₀", round(model$beta0, 2), color = unname(upwr_cat["niebo"])),
        lc_stat_box("Twoje b₁", round(user_b1, 2), color = unname(upwr_cat["terakota"])),
        lc_stat_box("Poprawne b₁", round(model$beta1, 2), color = unname(upwr_cat["niebo"])),
        columns = 4
      )
    )
  })

  # --- Widget: OLS krok po kroku ---
  ch1_ols_data <- reactiveVal(generate_regression_data(n = 70, beta0 = 4, beta1 = 1.4, sigma = 4))
  ch1_ols_step <- reactiveVal(0)

  observeEvent(input$ch1_ols_new, {
    ch1_ols_data(generate_regression_data(n = 70, beta0 = 4, beta1 = 1.4, sigma = 4))
    ch1_ols_step(0)
  })
  observeEvent(input$ch1_ols_step1, ch1_ols_step(1))
  observeEvent(input$ch1_ols_step2, ch1_ols_step(2))
  observeEvent(input$ch1_ols_step3, ch1_ols_step(3))
  observeEvent(input$ch1_ols_step4, ch1_ols_step(4))
  observeEvent(input$ch1_ols_step5, ch1_ols_step(5))
  observeEvent(input$ch1_ols_step6, ch1_ols_step(6))

  zoom_plot_server("ch1_ols_plot", reactive({
    df <- ch1_ols_data()
    step <- ch1_ols_step()
    model <- lm(y ~ x, data = df)
    coefs <- coef(model)
    df$fitted <- fitted(model)
    df$resid <- residuals(model)
    mean_y <- mean(df$y)
    alt_b1 <- coefs[2] * 0.45
    alt_b0 <- mean_y - alt_b1 * mean(df$x)
    df$alt_fitted <- alt_b0 + alt_b1 * df$x

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_point(color = upwr_secondary, alpha = 0.55, size = 2) +
      labs(x = "X", y = "Y") +
      theme_upwr()

    if (step >= 2) {
      p <- p + geom_hline(yintercept = mean_y, linetype = "dashed",
                          color = unname(upwr_cat["bursztyn"]), linewidth = 1)
    }
    if (step >= 3) {
      p <- p + geom_smooth(method = "lm", se = FALSE,
                           color = unname(upwr_cat["niebo"]), linewidth = 1.2)
    }
    if (step >= 4) {
      p <- p + geom_segment(aes(xend = x, yend = fitted),
                            color = unname(upwr_cat["terakota"]), alpha = 0.35)
    }
    if (step >= 6) {
      p <- p +
        geom_abline(intercept = alt_b0, slope = alt_b1,
                    color = unname(upwr_cat["bursztyn"]), linewidth = 1.1,
                    linetype = "longdash") +
        geom_segment(aes(xend = x, yend = alt_fitted),
                     color = unname(upwr_cat["bursztyn"]), alpha = 0.22) +
        annotate("text", x = min(df$x), y = max(df$y),
                 label = "inna prosta", hjust = 0, vjust = 1,
                 color = unname(upwr_cat["bursztyn"]), fontface = "bold") +
        annotate("text", x = min(df$x), y = max(df$y) - 0.1 * diff(range(df$y)),
                 label = "OLS", hjust = 0, vjust = 1,
                 color = unname(upwr_cat["niebo"]), fontface = "bold")
    }
    if (step == 0) {
      p <- p + annotate("text", x = mean(df$x), y = mean(df$y),
                        label = "Klikaj kroki po lewej", color = upwr_reference, size = 5)
    }
    p
  }))

  output$ch1_ols_info <- renderUI({
    df <- ch1_ols_data()
    step <- ch1_ols_step()
    if (step == 0) return(NULL)
    model <- lm(y ~ x, data = df)
    coefs <- coef(model)
    sse <- sum(residuals(model)^2)
    alt_b1 <- coefs[2] * 0.45
    alt_b0 <- mean(df$y) - alt_b1 * mean(df$x)
    alt_sse <- sum((df$y - (alt_b0 + alt_b1 * df$x))^2)
    if (step == 6) {
      return(tagList(
        lc_stat_grid(
          lc_stat_box("SSE OLS", round(sse, 1), color = unname(upwr_cat["niebo"])),
          lc_stat_box("SSE innej prostej", round(alt_sse, 1),
                      caption = paste0("+", round((alt_sse / sse - 1) * 100, 1), "%"),
                      color = unname(upwr_cat["bursztyn"])),
          columns = 2
        ),
        lc_feedback(type = "warning",
          p("Ta przerywana linia też jest prostym modelem regresyjnym: dla każdego X daje przewidywane Ŷ. Nie jest jednak linią OLS, bo ma większą sumę kwadratów reszt. OLS wygrywa nie dlatego, że jest jedyną prostą, tylko dlatego, że minimalizuje SSE.")
        )
      ))
    }
    info <- switch(as.character(step),
      "1" = "Najpierw mamy tylko punkty: pary obserwacji X i Y.",
      "2" = "Pozioma linia to średnia Y. To najprostszy model bez predyktora.",
      "3" = "Linia regresji przechodzi tak, aby suma kwadratów pionowych błędów była możliwie mała.",
      "4" = "Każdy odcinek to reszta: obserwacja minus predykcja.",
      "5" = paste0("Model: Ŷ = ", round(coefs[1], 2), " + ",
                   round(coefs[2], 2), "X; SSE = ", round(sse, 1), ".")
    )
    lc_feedback(type = "info", p(info))
  })

  # --- Widget: p-value dla nachylenia ---
  ch1_pval_data <- reactive({
    scenario <- input$ch1_pval_scenario
    if (is.null(scenario)) scenario <- "strong_positive"
    seed <- switch(scenario,
      "strong_positive" = 3101,
      "none" = 3102,
      "strong_negative" = 3103,
      "small_sample" = 3104
    )
    set.seed(seed)

    params <- switch(scenario,
      "strong_positive" = list(n = 70, beta0 = 6, beta1 = 1.25, sigma = 3.0,
                               title = "Duży efekt i umiarkowany szum"),
      "none" = list(n = 70, beta0 = 6, beta1 = 0.00, sigma = 4.2,
                    title = "Brak systematycznego trendu"),
      "strong_negative" = list(n = 70, beta0 = 8, beta1 = -1.15, sigma = 3.0,
                               title = "Ujemne nachylenie"),
      "small_sample" = list(n = 14, beta0 = 6, beta1 = 1.25, sigma = 5.0,
                            title = "Trend podobny, ale mniej danych")
    )

    x <- runif(params$n, -4, 4)
    y <- params$beta0 + params$beta1 * x + rnorm(params$n, 0, params$sigma)
    data.frame(x = x, y = y, title = params$title)
  })

  ch1_pval_model <- reactive({
    lm(y ~ x, data = ch1_pval_data())
  })

  output$ch1_pval_table <- renderUI({
    model <- ch1_pval_model()
    coefs <- broom::tidy(model)
    coefs$term <- ifelse(coefs$term == "(Intercept)", "wyraz wolny", "X")

    fmt_p <- function(p) {
      ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))
    }

    tags$table(class = "lc-table lc-table-bordered lc-table-striped lc-table-sm",
      tags$thead(
        tags$tr(
          tags$th("Term"),
          tags$th("Estimate"),
          tags$th("t"),
          tags$th("p-value")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(coefs)), function(i) {
          tags$tr(
            tags$td(coefs$term[i]),
            tags$td(sprintf("%.2f", coefs$estimate[i])),
            tags$td(sprintf("%.2f", coefs$statistic[i])),
            tags$td(fmt_p(coefs$p.value[i]))
          )
        })
      )
    )
  })

  zoom_plot_server("ch1_pval_plot", reactive({
    df <- ch1_pval_data()
    model <- ch1_pval_model()
    p_val <- broom::tidy(model)$p.value[2]
    is_sig <- p_val < 0.05
    line_color <- if (is_sig) unname(upwr_cat["niebo"]) else upwr_reference

    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = upwr_secondary, alpha = 0.55, size = 2.1) +
      geom_hline(yintercept = mean(df$y), color = unname(upwr_cat["bursztyn"]),
                 linetype = "dashed", linewidth = 0.9) +
      geom_smooth(method = "lm", se = TRUE,
                  color = line_color, fill = line_color,
                  linewidth = 1.4, alpha = 0.16) +
      annotate("label", x = min(df$x), y = max(df$y),
               hjust = 0, vjust = 1,
               label = if (is_sig) "p < 0.05: nachylenie istotne" else "p ≥ 0.05: brak istotności",
               color = line_color, fill = "white", linewidth = 0) +
      labs(x = "X", y = "Y", subtitle = df$title[1]) +
      theme_upwr()
  }))

  output$ch1_pval_verdict <- renderUI({
    model <- ch1_pval_model()
    coefs <- broom::tidy(model)
    p_val <- coefs$p.value[2]
    b1 <- coefs$estimate[2]
    is_sig <- p_val < 0.05

    if (is_sig) {
      lc_feedback(type = "ok", style = "margin-top: 12px;",
        tags$strong("Wniosek: "),
        sprintf("odrzucamy H0. Nachylenie b1 = %.2f jest istotnie różne od zera.", b1)
      )
    } else {
      lc_feedback(type = "warning", style = "margin-top: 12px;",
        tags$strong("Wniosek: "),
        sprintf("nie odrzucamy H0. Dane nie dają mocnych podstaw, by uznać nachylenie b1 = %.2f za różne od zera.", b1)
      )
    }
  })

  output$ch1_pval_stats <- renderUI({
    model <- ch1_pval_model()
    coefs <- broom::tidy(model)
    p_val <- coefs$p.value[2]

    tagList(
      lc_stat_grid(
        lc_stat_box("b₁", round(coefs$estimate[2], 2), color = unname(upwr_cat["szalwia"])),
        lc_stat_box("SE(b₁)", round(coefs$std.error[2], 2), color = upwr_secondary),
        lc_stat_box("t", round(coefs$statistic[2], 2), color = unname(upwr_cat["bursztyn"])),
        lc_stat_box("p-value", if (p_val < 0.001) "< 0.001" else round(p_val, 3),
                    color = if (p_val < 0.05) unname(upwr_cat["niebo"]) else upwr_reference),
        columns = 4
      ),
      lc_feedback(type = "info",
        p("p-value dotyczy testu dla współczynnika przy X, czyli pytania,
          czy prawdziwe nachylenie prostej w populacji może wynosić zero.")
      )
    )
  })

  # --- CASchools: output + quiz interpretacyjny ---
  ch1_cas_revealed <- reactiveVal(FALSE)

  observeEvent(input$ch1_cas_reveal, {
    ch1_cas_revealed(TRUE)
  })

  observeEvent(list(input$ch1_cas_x, input$ch1_cas_y), {
    ch1_cas_revealed(FALSE)
  }, ignoreInit = TRUE)

  ch1_cas_model <- reactive({
    req(input$ch1_cas_x, input$ch1_cas_y)
    validate(need(input$ch1_cas_x != input$ch1_cas_y, "Wybierz dwie różne zmienne."))
    if (identical(input$ch1_cas_x, "grades")) {
      df <- .cas_data
      df$grades01 <- ifelse(df$grades == "KK-08", 1, 0)
      lm(as.formula(paste(input$ch1_cas_y, "~ grades01")), data = df)
    } else {
      form <- as.formula(paste(input$ch1_cas_y, "~", input$ch1_cas_x))
      lm(form, data = .cas_data)
    }
  })

  zoom_plot_server("ch1_cas_plot", reactive({
    req(input$ch1_cas_x, input$ch1_cas_y)
    validate(need(input$ch1_cas_x != input$ch1_cas_y, "Wybierz dwie różne zmienne."))

    if (identical(input$ch1_cas_x, "grades")) {
      df <- .cas_data
      df$grades01 <- ifelse(df$grades == "KK-08", 1, 0)
      model <- ch1_cas_model()
      pred_df <- data.frame(
        grades = c("KK-06", "KK-08"),
        grades01 = c(0, 1)
      )
      pred_df$pred <- predict(model, newdata = pred_df)

      ggplot(df, aes(x = grades, y = .data[[input$ch1_cas_y]])) +
        geom_jitter(width = 0.12, height = 0, color = upwr_secondary,
                    alpha = 0.42, size = 1.8) +
        stat_summary(fun = mean, geom = "point",
                     color = unname(upwr_cat["terakota"]), size = 3.4) +
        geom_crossbar(data = pred_df,
                      aes(x = grades, y = pred, ymin = pred, ymax = pred),
                      inherit.aes = FALSE,
                      color = unname(upwr_cat["niebo"]), fill = NA,
                      linewidth = 0.8, width = 0.55) +
        labs(
          x = unname(.cas_labels[input$ch1_cas_x]),
          y = unname(.cas_labels[input$ch1_cas_y])
        ) +
        theme_upwr()
    } else {
      ggplot(.cas_data, aes(x = .data[[input$ch1_cas_x]], y = .data[[input$ch1_cas_y]])) +
        geom_point(color = upwr_secondary, alpha = 0.45, size = 1.8) +
        geom_smooth(method = "lm", se = TRUE,
                    color = unname(upwr_cat["niebo"]),
                    fill = unname(upwr_cat["niebo"]), alpha = 0.15) +
        labs(
          x = unname(.cas_labels[input$ch1_cas_x]),
          y = unname(.cas_labels[input$ch1_cas_y])
        ) +
        theme_upwr()
    }
  }))

  output$ch1_cas_table <- renderUI({
    req(input$ch1_cas_x, input$ch1_cas_y)
    if (input$ch1_cas_x == input$ch1_cas_y) {
      return(lc_feedback(type = "warning", p("Wybierz dwie różne zmienne.")))
    }

    model <- ch1_cas_model()
    coefs <- broom::tidy(model)
    coefs$term <- ifelse(coefs$term == "(Intercept)", "wyraz wolny",
                         ifelse(coefs$term == "grades01", "grades: KK-08 vs KK-06", input$ch1_cas_x))

    fmt_p <- function(p) {
      ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))
    }

    tags$table(class = "lc-table lc-table-bordered lc-table-striped lc-table-sm",
      tags$thead(
        tags$tr(
          tags$th("Term"),
          tags$th("Estimate"),
          tags$th("SE"),
          tags$th("t"),
          tags$th("p-value")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(coefs)), function(i) {
          tags$tr(
            tags$td(coefs$term[i]),
            tags$td(sprintf("%.3f", coefs$estimate[i])),
            tags$td(sprintf("%.3f", coefs$std.error[i])),
            tags$td(sprintf("%.2f", coefs$statistic[i])),
            tags$td(fmt_p(coefs$p.value[i]))
          )
        })
      )
    )
  })

  output$ch1_cas_answer <- renderUI({
    req(input$ch1_cas_x, input$ch1_cas_y)
    if (input$ch1_cas_x == input$ch1_cas_y) return(NULL)

    if (!ch1_cas_revealed()) {
      return(lc_feedback(type = "warning", style = "margin-top: 12px;",
        p("Zanim klikniesz: sprawdź znak b₁ i p-value w tabeli. Czy wpływ X jest istotny?")
      ))
    }

    model <- ch1_cas_model()
    coefs <- broom::tidy(model)
    p_val <- coefs$p.value[2]
    b1 <- coefs$estimate[2]
    x_label <- unname(.cas_labels[input$ch1_cas_x])
    y_label <- unname(.cas_labels[input$ch1_cas_y])
    relation <- if (b1 > 0) "dodatni" else "ujemny"

    if (p_val < 0.05) {
      lc_feedback(type = "ok", style = "margin-top: 12px;",
        tags$strong("Odpowiedź: "),
        if (identical(input$ch1_cas_x, "grades")) {
          sprintf("tak, %s istotnie przewiduje %s. Okręgi KK-08 różnią się od KK-06 średnio o %.3f punktu, p = %.3g.",
                  x_label, y_label, b1, p_val)
        } else {
          sprintf("tak, %s istotnie przewiduje %s. Efekt jest %s: b1 = %.3f, p = %.3g.",
                  x_label, y_label, relation, b1, p_val)
        }
      )
    } else {
      lc_feedback(type = "warning", style = "margin-top: 12px;",
        tags$strong("Odpowiedź: "),
        if (identical(input$ch1_cas_x, "grades")) {
          sprintf("nie mamy podstaw, by uznać różnicę między KK-08 i KK-06 w %s za istotną: b1 = %.3f, p = %.3g.",
                  y_label, b1, p_val)
        } else {
          sprintf("nie mamy podstaw, by uznać wpływ %s na %s za istotny: b1 = %.3f, p = %.3g.",
                  x_label, y_label, b1, p_val)
        }
      )
    }
  })

  output$ch1_cas_summary <- renderUI({
    req(input$ch1_cas_x, input$ch1_cas_y)
    if (input$ch1_cas_x == input$ch1_cas_y || !ch1_cas_revealed()) return(NULL)

    model <- ch1_cas_model()
    coefs <- broom::tidy(model)
    x_label <- unname(.cas_labels[input$ch1_cas_x])
    y_label <- unname(.cas_labels[input$ch1_cas_y])

    if (identical(input$ch1_cas_x, "grades")) {
      b0 <- coefs$estimate[1]
      b1 <- coefs$estimate[2]
      y0 <- b0
      y1 <- b0 + b1

      return(tagList(
        lc_stat_grid(
          lc_stat_box("b₀", round(b0, 2), color = upwr_secondary),
          lc_stat_box("b₁", round(b1, 3), color = unname(upwr_cat["szalwia"])),
          lc_stat_box("p dla b₁", signif(coefs$p.value[2], 3), color = unname(upwr_cat["bursztyn"])),
          columns = 3
        ),
        lc_formula_box(
          withMathJax(helpText(sprintf(
            "$$\\hat{Y} = %.2f %+ .2f \\cdot X_{\\text{KK-08}}$$",
            b0, b1
          ))),
          p(tags$strong("Kodowanie: "), "KK-06 = 0, KK-08 = 1."),
          withMathJax(helpText(sprintf(
            "$$\\text{KK-06: } \\hat{Y} = %.2f %+ .2f \\cdot 0 = %.2f$$",
            b0, b1, y0
          ))),
          withMathJax(helpText(sprintf(
            "$$\\text{KK-08: } \\hat{Y} = %.2f %+ .2f \\cdot 1 = %.2f$$",
            b0, b1, y1
          )))
        ),
        lc_feedback(type = "info", style = "margin-top: 10px;",
          p(tags$strong("Interpretacja: "),
            paste0("w tym kodowaniu b₀ to średni przewidywany ", y_label,
                   " dla KK-06, a b₁ to różnica KK-08 minus KK-06."))
        )
      ))
    }

    tagList(
      lc_stat_grid(
        lc_stat_box("b₀", round(coefs$estimate[1], 2), color = upwr_secondary),
        lc_stat_box("b₁", round(coefs$estimate[2], 3), color = unname(upwr_cat["szalwia"])),
        lc_stat_box("p dla b₁", signif(coefs$p.value[2], 3), color = unname(upwr_cat["bursztyn"])),
        columns = 3
      ),
      lc_feedback(type = "info", style = "margin-top: 10px;",
        p(tags$strong("Interpretacja: "),
          paste0("gdy ", x_label, " rośnie o 1, przewidywane ", y_label,
                 " zmienia się średnio o ", round(coefs$estimate[2], 3), "."))
      )
    )
  })

  # --- CASchools: pierwsza predykcja z modelu ---
  ch1_pred_revealed <- reactiveVal(FALSE)

  ch1_pred_spec <- reactive({
    case <- input$ch1_pred_case
    if (is.null(case)) case <- "read_students"
    .ch1_pred_specs[[case]]
  })

  ch1_pred_model <- reactive({
    spec <- ch1_pred_spec()
    form <- as.formula(paste(spec$y, "~", spec$x))
    lm(form, data = .cas_data)
  })

  observeEvent(input$ch1_pred_reveal, {
    req(input$ch1_pred_x)
    ch1_pred_revealed(TRUE)
  })

  observeEvent(list(input$ch1_pred_case, input$ch1_pred_x), {
    ch1_pred_revealed(FALSE)
  }, ignoreInit = TRUE)

  output$ch1_pred_x_input <- renderUI({
    spec <- ch1_pred_spec()
    x_vals <- .cas_data[[spec$x]]
    numericInput(
      "ch1_pred_x",
      label = paste0("Wartość X (", spec$unit, "):"),
      value = spec$default,
      min = floor(min(x_vals, na.rm = TRUE)),
      max = ceiling(max(x_vals, na.rm = TRUE)),
      step = spec$step
    )
  })

  output$ch1_pred_question <- renderUI({
    spec <- ch1_pred_spec()
    req(input$ch1_pred_x)
    lc_feedback(type = "info", style = "margin-top: 12px;",
      p(tags$strong("Pytanie: "), spec$question),
      p("Podstaw do równania wartość X = ",
        tags$strong(input$ch1_pred_x), " i spróbuj policzyć przewidywane Y.")
    )
  })

  output$ch1_pred_table <- renderUI({
    spec <- ch1_pred_spec()
    model <- ch1_pred_model()
    coefs <- broom::tidy(model)
    coefs$term <- ifelse(coefs$term == "(Intercept)", "wyraz wolny", spec$x)

    tags$table(class = "lc-table lc-table-bordered lc-table-striped lc-table-sm",
      tags$thead(
        tags$tr(
          tags$th("Term"),
          tags$th("Estimate")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(coefs)), function(i) {
          tags$tr(
            tags$td(coefs$term[i]),
            tags$td(sprintf("%.3f", coefs$estimate[i]))
          )
        })
      )
    )
  })

  zoom_plot_server("ch1_pred_plot", reactive({
    spec <- ch1_pred_spec()
    model <- ch1_pred_model()
    coefs <- coef(model)
    x0 <- input$ch1_pred_x
    if (is.null(x0)) x0 <- spec$default
    y_hat <- unname(coefs[1] + coefs[2] * x0)

    p <- ggplot(.cas_data, aes(x = .data[[spec$x]], y = .data[[spec$y]])) +
      geom_point(color = upwr_secondary, alpha = 0.42, size = 1.8) +
      geom_smooth(method = "lm", se = TRUE,
                  color = unname(upwr_cat["niebo"]),
                  fill = unname(upwr_cat["niebo"]), alpha = 0.15) +
      labs(
        x = unname(.cas_labels[spec$x]),
        y = unname(.cas_labels[spec$y])
      ) +
      theme_upwr()

    if (ch1_pred_revealed()) {
      p <- p +
        geom_vline(xintercept = x0, color = unname(upwr_cat["bursztyn"]),
                   linetype = "dashed", linewidth = 0.9) +
        geom_hline(yintercept = y_hat, color = unname(upwr_cat["terakota"]),
                   linetype = "dashed", linewidth = 0.9) +
        annotate("point", x = x0, y = y_hat,
                 color = unname(upwr_cat["terakota"]),
                 fill = "white", shape = 21, stroke = 1.2, size = 4) +
        annotate("text", x = x0, y = y_hat,
                 label = paste0("Ŷ = ", round(y_hat, 1)),
                 hjust = -0.1, vjust = -0.8,
                 color = unname(upwr_cat["terakota"]), fontface = "bold")
    }

    p
  }))

  output$ch1_pred_answer <- renderUI({
    spec <- ch1_pred_spec()
    req(input$ch1_pred_x)

    if (!ch1_pred_revealed()) {
      return(lc_feedback(type = "warning", style = "margin-top: 12px;",
        p("Odpowiedź jest ukryta. Najpierw policz predykcję z tabeli współczynników.")
      ))
    }

    coefs <- coef(ch1_pred_model())
    x0 <- input$ch1_pred_x
    y_hat <- unname(coefs[1] + coefs[2] * x0)
    x_label <- unname(.cas_labels[spec$x])
    y_label <- unname(.cas_labels[spec$y])

    tagList(
      lc_feedback(type = "ok", style = "margin-top: 12px;",
        tags$div(style = "font-weight: 700; margin-bottom: 6px;", "Odpowiedź:"),
        withMathJax(tags$div(
          style = "font-size: 1.25rem; font-weight: 700; text-align: center;",
          sprintf("$$\\hat{Y} = %.2f + %.3f \\cdot %.2f = %.2f$$",
                  coefs[1], coefs[2], x0, y_hat)
        )),
        p(sprintf("Dla %s = %.2f przewidywane %s wynosi %.2f.",
                  x_label, x0, y_label, y_hat))
      )
    )
  })

  output$ch1_pred_stats <- renderUI({
    if (!ch1_pred_revealed()) return(NULL)
    spec <- ch1_pred_spec()
    model <- ch1_pred_model()
    coefs <- coef(model)
    x0 <- input$ch1_pred_x
    y_hat <- unname(coefs[1] + coefs[2] * x0)

    lc_stat_grid(
      lc_stat_box("b₀", round(coefs[1], 2), color = upwr_secondary),
      lc_stat_box("b₁", round(coefs[2], 3), color = unname(upwr_cat["szalwia"])),
      lc_stat_box("X", round(x0, 2), caption = unname(.cas_labels[spec$x]),
                  color = unname(upwr_cat["bursztyn"])),
      lc_stat_box("Ŷ", round(y_hat, 2), caption = unname(.cas_labels[spec$y]),
                  color = unname(upwr_cat["terakota"])),
      columns = 4
    )
  })
}
