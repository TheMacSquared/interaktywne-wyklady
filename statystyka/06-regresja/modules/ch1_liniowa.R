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
        zawsze warto najpierw rzucić okiem na wykres rozrzutu: ", gloss("regresja liniowa"), "
        ma sens dopiero wtedy, gdy chmura punktów układa się w przybliżeniu
        wzdłuż prostej. Jeśli widać krzywiznę albo dwie chmury, prosta będzie
        kłamać niezależnie od tego, jak ładnie policzą się współczynniki."),
      p("Formalnie ", gloss("regresja prosta", "regresja liniowa prosta"), " zapisuje związek X → Y tak:"),
      lc_formula_box(
        withMathJax(helpText("$$Y = \\beta_0 + \\beta_1 X + \\varepsilon$$")),
        p(withMathJax("\\(\\beta_0\\)"), " — ", gloss("wyraz wolny"), " (intercept): wartość Y gdy X = 0"),
        p(withMathJax("\\(\\beta_1\\)"), " — ", gloss("współczynnik regresji", "nachylenie"), " (slope): o ile zmieni się Y, gdy X wzrośnie o 1"),
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
          lc_slider("ch1_beta_b0", "β₀ (punkt startu)", -10, 20, 5, 1),
          lc_slider("ch1_beta_b1", "β₁ (nachylenie)", -3, 3, 1, 0.25),
          lc_slider("ch1_beta_sigma", "Szum σ", 0, 8, 2, 0.5)
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

    p("Dla jednej zmiennej X i jednej Y nachylenie regresji można policzyć bez optymalizacji: z korelacji i odchyleń standardowych."),

    figure_panel(
      label = "Ryc. 1.1",
      full_width = TRUE,
      lc_step_widget("ch1_corr",
        title = "Jak policzyć regresję z korelacji?",
        steps = c("Dane", "Średnie X i Y", "Odchylenia standardowe", "Korelacja r",
                  "Nachylenie b₁", "Wyraz wolny b₀"),
        toolbar = lc_toolbar(
          lc_action("ch1_corr_new", "Nowa próba", icon = "shuffle", variant = "outline")
        ),
        plot_id = "ch1_corr_plot",
        ratio = "2/1",
        extra = uiOutput("ch1_corr_info")
      )
    ),

    tagList(
      p("Recepta jest więc prosta: jedno r, dwa odchylenia standardowe i dwie
        średnie wystarczą, żeby wyznaczyć linię. W rzeczywistej pracy nikt nie
        robi tego ręcznie — wpisujemy do R jedną komendę i dostajemy gotową
        tabelę regresji: kolumny z estymatorami, ", gloss("błąd standardowy", "błędami standardowymi"), ", statystykami t i
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
          lc_action("ch1_draw_reset", "Wyczyść punkty", variant = "outline"),
          lc_action("ch1_draw_reveal", "Pokaż odpowiedź", variant = "solid"),
          lc_action("ch1_draw_new", "Nowe ćwiczenie", variant = "solid"),
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
      p("Zasada nazywa się ", tags$em(gloss("metoda najmniejszych kwadratów", "metodą najmniejszych kwadratów"), " (MNK / OLS)"),
        ": wybieramy taką prostą, która minimalizuje sumę kwadratów pionowych
        odległości między punktami a linią. Następny widget rozkłada ten pomysł
        na sześć kroków.")
    ),

    lc_h2("ch1-ols-krok", "Najmniejsze kwadraty — krok po kroku"),

    p("Ta sama próba, kolejne warstwy interpretacji."),

    figure_panel(
      label = "Ryc. 1.1b",
      full_width = TRUE,
      lc_step_widget("ch1_ols",
        title = "Jak linia staje się modelem",
        steps = c("Dane", "Średnia Y", "Linia regresji", "Reszty",
                  "Wynik modelu", "Inna prosta?"),
        toolbar = lc_toolbar(
          lc_action("ch1_ols_new", "Nowa próba", icon = "shuffle", variant = "outline")
        ),
        plot_id = "ch1_ols_plot",
        ratio = "2/1"
      )
    ),

    lc_h2("ch1-reszty", "Reszty i dlaczego kwadraty"),

    tagList(
      p("Te pionowe odcinki, które pojawiły się w kroku 4, mają swoją nazwę:
        to ", gloss("reszta", "reszty"), ". Każda obserwacja ma własną resztę — różnicę między tym, co
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
        próbami. ", gloss("statystyka testowa", "Statystyka testowa"), " jest właściwie ilorazem — ",
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
          lc_action("ch1_cas_reveal", "Pokaż odpowiedź", variant = "solid"),
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
      p("Trzeba tylko pamiętać, co ta liczba znaczy: ", gloss("wartość przewidywana", "predykcja"), " to średnia warunkowa
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
          lc_action("ch1_pred_reveal", "Pokaż odpowiedź", variant = "solid"),
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
  # Krok widgetu (1..5) żyje w przeglądarce; nowa próba nie zmienia kroku.
  ch1_corr_data <- reactiveVal(generate_regression_data(n = 65, beta0 = 8, beta1 = 1.6, sigma = 4))
  # Pierwszy krok paska („Dane”) to stan 0 rysunku: sama chmura punktów.
  ch1_corr_pos <- lc_step_server("ch1_corr", input)$step
  ch1_corr_step <- reactive(ch1_corr_pos() - 1L)

  observeEvent(input$ch1_corr_new, {
    ch1_corr_data(generate_regression_data(n = 65, beta0 = 8, beta1 = 1.6, sigma = 4))
  })

  # Etykieta w kolorze roli (krój wykresu, jak dotąd).
  ch1_role_text <- function(x, y, label, role, ...) {
    annotate("text", x = x, y = y, label = label,
             colour = STEP_ROLES[[role]]$colour, fontface = "bold", ...)
  }

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

    # Stała rama z pełnych danych (z punktem x = 0 i b₀ z kroku 5).
    pad <- function(v, m = 0.05) v + c(-1, 1) * diff(v) * m
    frame <- step_frame(
      xlim = pad(range(c(df$x, 0))),
      ylim = pad(range(c(df$y, 0, b0, b1 * min(df$x))))
    )
    marker_df <- function(x, y) data.frame(x = x, y = y)

    # W kroku 4 dane ustępują konstrukcji nachylenia (tło).
    p <- ggplot(df, aes(x = x, y = y)) +
      step_layer(geom_point, if (step == 4) "background" else "data",
                 size = if (step == 4) 1.8 else 2.2) +
      labs(x = "X", y = "Y")

    if (step >= 1 && !(step %in% c(4, 5))) {
      role <- step_role(step, 1)
      p <- p +
        step_line(role, xintercept = x_bar) +
        step_line(role, yintercept = y_bar)
    }
    if (step == 2) {
      p <- p +
        step_layer(geom_segment, "new", mapping = aes(xend = x_bar, yend = y),
                   linetype = "22", alpha = 0.35, linewidth = 0.5) +
        step_layer(geom_segment, "new", mapping = aes(xend = x, yend = y_bar),
                   linetype = "22", alpha = 0.35, linewidth = 0.5) +
        ch1_role_text(x_bar, max(df$y), "odchylenia X", "new",
                      hjust = -0.05, vjust = 1) +
        ch1_role_text(min(df$x), y_bar, "odchylenia Y", "new",
                      hjust = 0, vjust = -0.7)
    }
    if (step == 3) {
      p <- p + step_layer(geom_abline, "new", intercept = b0, slope = b1,
                          linewidth = 1.3)
    }
    if (step == 4) {
      p <- p +
        step_layer(geom_abline, "known", intercept = b0, slope = b1,
                   linewidth = 1.8) +
        step_layer(geom_segment, "new",
                   data = data.frame(x = slope_x0, xend = slope_x1,
                                     y = slope_y0, yend = slope_y0),
                   mapping = aes(x = x, xend = xend, y = y, yend = yend),
                   linewidth = 1.2,
                   arrow = arrow(length = grid::unit(0.12, "inches"))) +
        step_layer(geom_segment, "new",
                   data = data.frame(x = slope_x1, xend = slope_x1,
                                     y = slope_y0, yend = slope_y1),
                   mapping = aes(x = x, xend = xend, y = y, yend = yend),
                   linewidth = 1.2,
                   arrow = arrow(length = grid::unit(0.12, "inches"))) +
        geom_point(
          data = marker_df(c(slope_x0, slope_x1, slope_x1),
                           c(slope_y0, slope_y0, slope_y1)),
          colour = STEP_ROLES$known$colour, fill = "white",
          shape = 21, stroke = 1.1, size = 3.2
        ) +
        ch1_role_text((slope_x0 + slope_x1) / 2, slope_y0, "ΔX = 1", "new",
                      vjust = 1.6) +
        ch1_role_text(slope_x1, (slope_y0 + slope_y1) / 2,
                      paste0("ΔY = b₁ = ", round(b1, 2)), "new", hjust = -0.08)
    }
    if (step == 5) {
      p <- p +
        step_layer(geom_abline, "known", intercept = 0, slope = b1,
                   linewidth = 1.2, linetype = "22") +
        step_layer(geom_abline, "known", intercept = b0, slope = b1,
                   linewidth = 1.5) +
        step_layer(geom_segment, "new",
                   data = data.frame(x = 0, xend = 0, y = 0, yend = b0),
                   mapping = aes(x = x, xend = xend, y = y, yend = yend),
                   linewidth = 1.2,
                   arrow = arrow(length = grid::unit(0.12, "inches"),
                                 ends = "both")) +
        geom_point(
          data = marker_df(0, c(0, b0)),
          colour = STEP_ROLES$known$colour, fill = "white",
          shape = 21, stroke = 1.1, size = 3
        ) +
        ch1_role_text(min(df$x), b1 * min(df$x), "b[0] == 0", "known",
                      parse = TRUE, hjust = 0, vjust = -0.6) +
        ch1_role_text(0.15, b0 / 2, paste0("b[0] == ", round(b0, 2)), "new",
                      parse = TRUE, hjust = 0)
    }

    # Krok 4 przybliża trójkąt nachylenia; pozostałe kroki mają wspólną ramę.
    if (step == 4) {
      p + coord_cartesian(
            xlim = c(slope_x0 - 1.1, slope_x1 + 1.45),
            ylim = c(slope_y_mid - slope_y_pad, slope_y_mid + slope_y_pad)
          ) +
        theme(legend.position = "none")
    } else {
      p + frame
    }
  }))

  # Opis kroku: statystyki z dotychczasowych kroków (dawniej kafelki).
  output$ch1_corr_text <- renderUI({
    df <- ch1_corr_data()
    step <- ch1_corr_step()

    x_bar <- mean(df$x)
    y_bar <- mean(df$y)
    sx <- sd(df$x)
    sy <- sd(df$y)
    r <- cor(df$x, df$y)
    b1 <- r * sy / sx
    b0 <- y_bar - b1 * x_bar

    if (step == 0) {
      return(paste0("Chmura ", nrow(df), " punktów: każdy punkt to jedna obserwacja (X, Y)."))
    }

    stat <- function(label, value) tagList(label, " = ", tags$b(value, .noWS = "outside"))
    stats <- list(
      if (step >= 1) stat("x̄", round(x_bar, 2)),
      if (step >= 1) stat("ȳ", round(y_bar, 2)),
      if (step >= 2) stat("sX", round(sx, 2)),
      if (step >= 2) stat("sY", round(sy, 2)),
      if (step >= 3) stat("r", round(r, 3))
    )
    stats <- Filter(Negate(is.null), stats)
    parts <- list(stats[[1]])
    for (s in stats[-1]) parts <- c(parts, list(", ", s))

    tagList(
      parts, HTML("."),
      if (step >= 5) tagList(" ", "To jest ta sama prosta, którą zwraca klasyczna regresja liniowa dla jednego predyktora. Korelacja ustala kierunek i siłę związku, a iloraz odchyleń standardowych przelicza ją na jednostki X i Y.")
    )
  })

  # Wzory nachylenia i wyrazu wolnego pod opisem kroku (kroki 4–5).
  output$ch1_corr_info <- renderUI({
    df <- ch1_corr_data()
    step <- ch1_corr_step()
    if (step < 4) return(NULL)

    x_bar <- mean(df$x)
    y_bar <- mean(df$y)
    sx <- sd(df$x)
    sy <- sd(df$y)
    r <- cor(df$x, df$y)
    b1 <- r * sy / sx
    b0 <- y_bar - b1 * x_bar

    tagList(
      lc_formula_box(
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
  # Krok widgetu (1..6) żyje w przeglądarce; nowa próba nie zmienia kroku.
  ch1_ols_data <- reactiveVal(generate_regression_data(n = 70, beta0 = 4, beta1 = 1.4, sigma = 4))
  ch1_ols_step <- lc_step_server("ch1_ols", input)$step

  observeEvent(input$ch1_ols_new, {
    ch1_ols_data(generate_regression_data(n = 70, beta0 = 4, beta1 = 1.4, sigma = 4))
  })

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

    # Stała rama z pełnych danych (z prostą z kroku 6).
    pad <- function(v, m = 0.05) v + c(-1, 1) * diff(v) * m
    frame <- step_frame(xlim = pad(range(df$x)),
                        ylim = pad(range(c(df$y, df$fitted, df$alt_fitted))))

    p <- ggplot(df, aes(x = x, y = y)) +
      step_layer(geom_point, "data", size = 2) +
      labs(x = "X", y = "Y")

    if (step >= 2) {
      p <- p + step_line(step_role(step, 2), yintercept = mean_y)
    }
    if (step >= 3) {
      p <- p + step_layer(geom_smooth, step_role(step, 3), method = "lm",
                          formula = y ~ x, se = FALSE, linewidth = 1.2)
    }
    if (step >= 4) {
      p <- p + step_layer(geom_segment, step_role(step, 4),
                          mapping = aes(xend = x, yend = fitted), alpha = 0.35)
    }
    if (step >= 6) {
      p <- p +
        step_layer(geom_abline, "new", intercept = alt_b0, slope = alt_b1,
                   linewidth = 1.1, linetype = "longdash") +
        step_layer(geom_segment, "new", mapping = aes(xend = x, yend = alt_fitted),
                   alpha = 0.22) +
        annotate("text", x = min(df$x), y = max(df$y),
                 label = "inna prosta", hjust = 0, vjust = 1,
                 colour = STEP_ROLES$new$colour, fontface = "bold") +
        annotate("text", x = min(df$x), y = max(df$y) - 0.1 * diff(range(df$y)),
                 label = "OLS", hjust = 0, vjust = 1,
                 colour = STEP_ROLES$known$colour, fontface = "bold")
    }
    p + frame
  }))

  output$ch1_ols_text <- renderUI({
    df <- ch1_ols_data()
    step <- ch1_ols_step()
    model <- lm(y ~ x, data = df)
    coefs <- coef(model)
    sse <- sum(residuals(model)^2)
    alt_b1 <- coefs[2] * 0.45
    alt_b0 <- mean(df$y) - alt_b1 * mean(df$x)
    alt_sse <- sum((df$y - (alt_b0 + alt_b1 * df$x))^2)
    if (step == 6) {
      return(tagList(
        "SSE OLS = ", tags$b(round(sse, 1)), ", SSE innej prostej = ",
        tags$b(round(alt_sse, 1)), " (+", round((alt_sse / sse - 1) * 100, 1), "%). ",
        "Ta przerywana linia też jest prostym modelem regresyjnym: dla każdego X daje przewidywane Ŷ. Nie jest jednak linią OLS, bo ma większą sumę kwadratów reszt. OLS wygrywa nie dlatego, że jest jedyną prostą, tylko dlatego, że minimalizuje SSE."
      ))
    }
    switch(as.character(step),
      "1" = "Najpierw mamy tylko punkty: pary obserwacji X i Y.",
      "2" = "Pozioma linia to średnia Y. To najprostszy model bez predyktora.",
      "3" = "Linia regresji przechodzi tak, aby suma kwadratów pionowych błędów była możliwie mała.",
      "4" = "Każdy odcinek to reszta: obserwacja minus predykcja.",
      "5" = tagList("Model: Ŷ = ", tags$b(round(coefs[1], 2)), " + ",
                    tags$b(round(coefs[2], 2)), "X; SSE = ", tags$b(round(sse, 1)), ".")
    )
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
      labs(x = "X", y = "Y") +
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
