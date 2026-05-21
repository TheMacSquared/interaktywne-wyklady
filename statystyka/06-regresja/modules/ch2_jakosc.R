# ============================================================================
# CHAPTER 2: Co czyni model dobrym?
# ============================================================================

# Scenariusze widgetu reszt vs fitted — dobrane tak, by pokazać różne wzorce.
.ch2_resid_specs <- list(
  read_lunch = list(
    label = "Czytanie ~ lunch (model dobrze działa)",
    x = "lunch", y = "read",
    verdict = "ok",
    title = "Wzorzec OK: linia ma sens",
    comment = "Reszty rozsypane wokół zera, bez wyraźnego wzorca.
              Model liniowy jest tu uzasadniony."
  ),
  read_income = list(
    label = "Czytanie ~ dochód (krzywizna)",
    x = "income", y = "read",
    verdict = "warning",
    title = "Łuk w resztach: zależność jest nieliniowa",
    comment = "Reszty układają się w łuk — zależność czytanie ~ dochód
              jest krzywa, nie liniowa. Prosta systematycznie zaniża
              przewidywania w środku zakresu X."
  ),
  read_str = list(
    label = "Czytanie ~ STR (słaby model, ale bez wzorca)",
    x = "student_teacher_ratio", y = "read",
    verdict = "info",
    title = "Słaby model, ale uczciwy",
    comment = "Reszty są duże, bo R² jest niskie — STR słabo przewiduje
              czytanie. Ale wzorzec sam w sobie jest losowy. To rzetelny
              model: po prostu niewiele tłumaczy."
  ),
  math_english = list(
    label = "Matematyka ~ angielski jako 2. język (przyzwoity)",
    x = "english", y = "math",
    verdict = "ok",
    title = "Reszty wyglądają OK",
    comment = "Wzorzec nie krzyczy. Można patrzeć dalej — na R² i RMSE."
  )
)

.ch2_resid_choices <- setNames(
  names(.ch2_resid_specs),
  vapply(.ch2_resid_specs, `[[`, character(1), "label")
)

# Scenariusze widgetu RMSE — modele różnej jakości, ten sam zbiór.
.ch2_rmse_specs <- list(
  read_lunch = list(
    label = "Czytanie ~ lunch (najlepsze dopasowanie)",
    x = "lunch", y = "read"
  ),
  read_income = list(
    label = "Czytanie ~ dochód okręgu",
    x = "income", y = "read"
  ),
  math_english = list(
    label = "Matematyka ~ angielski jako 2. język",
    x = "english", y = "math"
  ),
  read_str = list(
    label = "Czytanie ~ uczniowie na nauczyciela",
    x = "student_teacher_ratio", y = "read"
  )
)

.ch2_rmse_choices <- setNames(
  names(.ch2_rmse_specs),
  vapply(.ch2_rmse_specs, `[[`, character(1), "label")
)

ch2_ui <- list(
  id    = "ch-jakosc",
  num   = "02",
  title = "Co czyni model dobrym?",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 02 · Regresja",
      num    = "02",
      title  = "Co czyni model dobrym?",
      lead   = "W ch1 dopasowaliśmy linię. Ale czy ona w ogóle ma sens?
                Reszty mówią prawdę o modelu."
    ),

    tagList(
      p("W rozdziale 1 mieliśmy wszystko, czego potrzeba do policzenia
        regresji: chmurę punktów, MNK, p-value, predykcję. Każde z tych
        narzędzi mówiło jednak: ", tags$em("jeśli model jest sensowny, to..."),
        ". Pytanie, które dotąd omijaliśmy, brzmi: czy nasz model jest sensowny?"),
      p("To pytanie rozkłada się na trzy konkretne podpytania, a każdemu
        z nich odpowiada inne narzędzie:"),
      tags$ol(
        tags$li("Czy linia nie kłamie systematycznie? — ",
                tags$em("wzorzec reszt")),
        tags$li("Ile zmienności Y model wyjaśnia? — ",
                withMathJax("\\(R^2\\)")),
        tags$li("Jak duże są typowe pomyłki w predykcji? — ",
                tags$em("RMSE"))
      ),
      p("Wszystko to są miary jakości pojedynczego modelu.
        Porównywaniem różnych modeli — który lepszy, który gorszy
        — zajmiemy się w rozdziale 4. Tu pytamy tylko: czy ", tags$em("ten"),
        " model jest wart zaufania?")
    ),

    lc_h2("ch2-reszty", "Wzorzec reszt: kiedy linia kłamie"),

    tagList(
      p("W rozdziale 1 reszty pojawiły się jako pojęcie pomocnicze:
        coś, co MNK ", tags$em("kwadratuje"),
        ", żeby znaleźć najlepszą prostą. Teraz reszty stają się głównym
        bohaterem. Patrzymy w nie, żeby zobaczyć, ", tags$em("czego model
        nie złapał"), "."),
      p("Idealny model ma reszty rozsypane jak chmura wokół zera —
        bez żadnego wzorca, bez trendu, bez wachlarza. Każde odchylenie
        od tego ideału ma swoją wymowę:"),
      tags$ul(
        tags$li("Łuk w resztach: zależność jest tak naprawdę krzywa, a my dopasowaliśmy prostą.
                 Linia systematycznie zaniża przewidywania w jednym zakresie
                 X i zawyża w innym."),
        tags$li("Wachlarz (lejek): wariancja Y zmienia się z X. Tam, gdzie X duże, punkty są
                 bardziej rozproszone niż tam, gdzie X małe. Łamie to założenie
                 stałej wariancji (homoskedastyczności)."),
        tags$li("Pojedynczy odstający: kropka na wykresie reszt daleko od reszty chmury — to obserwacja,
                 która ", tags$em("ciągnie"), " linię na siebie.")
      ),
      p("Standardowe narzędzie to wykres reszt vs dopasowanych
        wartości: na osi X kładziemy ", withMathJax("\\(\\hat{Y}\\)"), ", na osi Y ",
        withMathJax("\\(e_i = y_i - \\hat{y}_i\\)"),
        ". Jeśli chmura nie ma struktury — model się nadaje. Jeśli ma —
        sygnał, że trzeba coś poprawić."),
      p("Uzupełnieniem jest wykres Q-Q reszt: porównuje kwantyle reszt z kwantylami rozkładu normalnego.
         Punkty biegnące wzdłuż linii prostej — reszty są w przybliżeniu normalne.
         Łuk lub grube ogony — sygnał problemów.")
    ),

    figure_panel(
      label = "Ryc. 2.1", title = "Reszty na danych CASchools",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Cztery scenariusze na tych samych okręgach szkolnych z Kalifornii. Zobacz, jak różny model zachowuje się na tych samych danych."),
          selectInput("ch2_resid_case", "Model:",
            choices = .ch2_resid_choices,
            selected = "read_income"
          ),
          uiOutput("ch2_resid_verdict")
        ),
        column(8,
          zoom_plot_ui("ch2_resid_plot", height = "340px"),
          uiOutput("ch2_resid_stats")
        )
      )
    ),

    inline_callout(label = "Zapamiętaj", color = "wskazowka",
      "Wykres reszt vs dopasowanych i Q-Q reszt to standardowa pierwsza diagnoza modelu
       liniowego. Jeśli chmura nie ma struktury i Q-Q biegnie wzdłuż linii — model
       jest OK. Pełna diagnostyka (leverage, wpływowe obserwacje) — w kolejnych wykładach."
    ),

    lc_h2("ch2-zalozenia", "Założenia, które widać w resztach"),

    tagList(
      p("W praktyce nie zaczynamy diagnostyki regresji od listy testów,
        tylko od pytania: ", tags$em("czy model zostawił po sobie losowy szum?"),
        " Dlatego większość klasycznych założeń modelu liniowego czytamy
        właśnie z wykresów reszt."),
      tags$table(class = "lc-table lc-table-bordered lc-table-striped",
        style = "font-size: 14px;",
        tags$thead(
          tags$tr(
            tags$th("Założenie"),
            tags$th("Co sprawdzić"),
            tags$th("Sygnał problemu"),
            tags$th("Co wtedy")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("Liniowość")),
            tags$td("reszty vs dopasowane"),
            tags$td("łuk, fala, systematyczny wzorzec"),
            tags$td("transformacja, składnik kwadratowy, model nieliniowy")
          ),
          tags$tr(
            tags$td(tags$strong("Stała wariancja")),
            tags$td("reszty vs dopasowane / Scale-Location"),
            tags$td("wachlarz, rosnący lub malejący rozrzut"),
            tags$td("transformacja Y, odporne błędy standardowe, WLS")
          ),
          tags$tr(
            tags$td(tags$strong("Normalność reszt")),
            tags$td("Q-Q reszt"),
            tags$td("grube ogony, łuk, odstające punkty"),
            tags$td("sprawdź outliery, bootstrap CI, inny model dla Y")
          ),
          tags$tr(
            tags$td(tags$strong("Brak obserwacji wpływowych")),
            tags$td("reszty standaryzowane, leverage, Cook's distance"),
            tags$td("pojedynczy punkt zmienia nachylenie"),
            tags$td("zweryfikuj pomiar, pokaż analizę z/bez punktu")
          )
        )
      ),
      p("Testy formalne — np. Shapiro-Wilk dla reszt albo Breusch-Pagan
        dla heteroscedastyczności — są dodatkiem do wykresu. Przy dużych
        próbach łatwo wykrywają drobiazgi, a przy małych często nie mają
        mocy. W raporcie najpierw pokaż wzorzec reszt, dopiero potem
        ewentualnie podaj test.")
    ),

    lc_h2("ch2-r2", "R² — ile model wyjaśnia?"),

    tagList(
      p("Wzorzec reszt mówił o jakości ", tags$em("jakościowej"),
        ": czy linia nie kłamie. Teraz pytanie ilościowe: ile zmienności Y rzeczywiście wyjaśnia model?"),
      p("Współczynnik determinacji ", withMathJax("\\(R^2\\)"),
        " mówi, jaki odsetek całej zmienności Y jest „zaopiekowany\" przez X.
        Liczy się prosto:"),
      lc_formula_box(
        withMathJax(helpText("$$R^2 = 1 - \\frac{SS_{res}}{SS_{tot}} = 1 - \\frac{\\sum(y_i - \\hat{y}_i)^2}{\\sum(y_i - \\bar{y})^2}$$"))
      ),
      p("Licznik to suma kwadratów reszt — wariancja, której model nie wyjaśnił.
        Mianownik to całkowita wariancja Y. Iloraz mierzy, ", tags$em("ile zmienności
        został niewytłumaczonej"),
        ", a 1 minus to jest dopełnieniem: ", tags$em("ile zmienności wyjaśniono"),
        ". Zakres [0, 1]: 0 = model nie wyjaśnia nic, 1 = idealne dopasowanie.")
    ),

    figure_panel(
      label = "Ryc. 2.2", title = "To samo X i Y, różna siła wyjaśniania",
      full_width = TRUE,
      helpText("Trzy stałe przykłady: niskie, średnie i wysokie R². Im ciaśniej punkty leżą przy linii, tym większa część zmienności Y jest wyjaśniona przez X."),
      zoom_plot_ui("ch2_r2_compare_plot", height = "360px")
    ),

    tagList(
      p("Tu pojawia się pierwsza ważna pułapka: wysokie ",
        withMathJax("\\(R^2\\)"),
        " nie oznacza automatycznie dobrego modelu. Model może tak mocno
        dopasować się do przypadkowych szczegółów próby, że świetnie wygląda
        na danych treningowych, ale słabo przewiduje nowe obserwacje. To jest
        ", tags$em("przeuczenie"), " (overfitting)."),
      p("Niskie ", withMathJax("\\(R^2\\)"),
        " też nie przekreśla modelu. W naukach społecznych, edukacyjnych
        czy bezpieczeństwie pracy procesy są głośne i wieloczynnikowe, więc
        ", withMathJax("\\(R^2 = 0.3\\)"),
        " bywa bardzo dobrą informacją. R² mówi o sile związku w tych
        konkretnych danych, a nie o jakości modelu w ogóle."),
      lc_h3("Jak wygląda przeuczenie?"),
      p("Kilka typowych sytuacji:"),
      tags$ul(
        tags$li(tags$b("Za dużo predyktorów przy małej próbie: "),
                "model z 20 zmiennymi dla 40 obserwacji może przypadkiem
                „wyjaśnić” szum, a nie zjawisko."),
        tags$li(tags$b("Zbyt elastyczna krzywa: "),
                "wielomian wysokiego stopnia przechodzi blisko każdego punktu,
                ale między punktami faluje bez sensu."),
        tags$li(tags$b("Powtarzane dobieranie modelu pod tę samą próbę: "),
                "sprawdzamy wiele wariantów i wybieramy ten, który wygląda
                najlepiej, choć wygrał przypadkiem."),
        tags$li(tags$b("Wyciek informacji: "),
                "w predyktorach znajduje się zmienna, której w praktycznej
                predykcji jeszcze byśmy nie znali, np. wynik po egzaminie
                użyty do przewidywania zdania egzaminu.")
      ),
      figure_panel(
        label = "Ryc. 2.2b", title = "Przeuczenie: dopasowanie kontra generalizacja",
        full_width = TRUE,
        helpText("Te same dane treningowe i testowe, trzy poziomy elastyczności modelu.
                  Model przeuczony potrafi mocno falować między punktami treningowymi,
                  mimo że nie poprawia przewidywania nowych obserwacji."),
        zoom_plot_ui("ch2_overfit_plot", height = "380px"),
        uiOutput("ch2_overfit_stats")
      ),
      figure_panel(
        label = "Miniściąga", title = "Jak ograniczać przeuczenie?",
        full_width = TRUE,
        tags$table(class = "lc-table lc-table-bordered lc-table-striped lc-table-sm",
          tags$thead(
            tags$tr(tags$th("Problem"), tags$th("Objaw"), tags$th("Co zrobić"))
          ),
          tags$tbody(
            tags$tr(
              tags$td("Model za złożony"),
              tags$td("R² wysokie, ale interpretacja chaotyczna"),
              tags$td("Uprościć model; usuwać predyktory bez uzasadnienia teoretycznego")
            ),
            tags$tr(
              tags$td("Dopasowanie do szumu"),
              tags$td("Błąd na danych treningowych mały, na nowych duży"),
              tags$td("Użyć train/test albo walidacji krzyżowej")
            ),
            tags$tr(
              tags$td("Dodawanie kolejnych X tylko pod R²"),
              tags$td("R² rośnie po każdym dodatku"),
              tags$td("Patrzeć na adjusted R², AIC, BIC i sens merytoryczny")
            ),
            tags$tr(
              tags$td("Niestabilne współczynniki"),
              tags$td("Mała zmiana danych mocno zmienia tabelę regresji"),
              tags$td("Zebrać więcej danych, ograniczyć liczbę zmiennych, sprawdzić współliniowość")
            )
          )
        )
      ),
      p("R² ma też siostrę używaną w porównaniach modeli — ",
        withMathJax("\\(R^2_{adj}\\)"),
        ", która karze za zbędne predyktory. Spotkamy ją w rozdziale 4,
        kiedy będziemy wybierać między kilkoma modelami. Tam pokażemy też
        train/test na przykładzie wielomianów: model może mieć świetne
        dopasowanie do treningu i jednocześnie gorszą predykcję na danych
        testowych.")
    ),

    lc_h2("ch2-rmse", "RMSE — jak duże są typowe pomyłki?"),

    tagList(
      p("R² jest miarą względną — daje wartość między 0 a 1, ale nie mówi
        nic o tym, ", tags$em("jak duże w jednostkach Y"),
        " są pomyłki modelu. Dla praktyka często to jest pytanie ważniejsze:
        jeśli model przewiduje wynik testu, czy myli się o 5 punktów czy o 50?"),
      p("Odpowiada na to RMSE — Root Mean Squared Error:
        pierwiastek ze średniej kwadratów reszt."),
      lc_formula_box(
        withMathJax(helpText("$$RMSE = \\sqrt{\\frac{1}{n}\\sum_{i=1}^{n}(y_i - \\hat{y}_i)^2}$$"))
      ),
      p("Liczone w jednostkach Y. Jeśli Y to wynik testu czytania w skali
        600–700, a RMSE wyszło 15, znaczy: typowa pomyłka modelu to ±15
        punktów. To dużo czy mało? Zależy od skali."),
      p(tags$strong("Złota zasada: "),
        "RMSE zawsze porównuj z rozrzutem Y. RMSE = 15 dla zmiennej w skali
        600–700 (zakres ~80 punktów) to nie najgorzej. RMSE = 15 dla zmiennej
        w skali 0–50 to katastrofa.")
    ),

    figure_panel(
      label = "Ryc. 2.3", title = "RMSE i zakres Y na danych CASchools",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Wybierz model i porównaj RMSE z zakresem Y. Liczbowo to różne wyniki, ale dopiero stosunek RMSE do zakresu daje intuicję jakości."),
          selectInput("ch2_rmse_case", "Model:",
            choices = .ch2_rmse_choices,
            selected = "read_lunch"
          ),
          uiOutput("ch2_rmse_interpretation")
        ),
        column(8,
          zoom_plot_ui("ch2_rmse_plot", height = "320px"),
          uiOutput("ch2_rmse_stats")
        )
      )
    ),

    lc_h2("ch2-ekstrapolacja", "Ekstrapolacja: poza zakresem danych"),

    tagList(
      p("Model regresji uczy się z danych, które mamy. Poza ich zakresem —
        nie ma podstaw, żeby mu ufać. Wciąż daje liczbę, ale ta liczba
        jest ekstrapolacją: predykcją za granicę,
        gdzie model nigdy nie był."),
      p("Ekstrapolacja jest niebezpieczna, bo linia wygląda pewnie
        nawet daleko od danych. Ale każdy punkt poza zakresem X to
        predykcja bez pokrycia — model nie wie, czy zależność tam
        jest nadal liniowa."),
      p("Przykład: model przewiduje wyniki testu czytania na podstawie
        dochodów okręgu. Dane obejmują dochody 5–55 tys. USD. Co jeśli
        zapytamy o dochód 80 tys.? Prosta obliczy wynik — ale nie ma
        żadnych danych w tym przedziale, żeby to potwierdzić.")
    ),

    figure_panel(
      label = "Ryc. 2.4", title = "Ekstrapolacja poza zakres danych",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Przesuń suwak poza zakres danych (szary pas) i obserwuj, jak predykcja traci grunt pod nogami."),
          sliderInput("ch2_extrap_x", "Dochód okręgu (tys. USD):",
            min = 1, max = 80, value = 20, step = 1),
          uiOutput("ch2_extrap_verdict")
        ),
        column(8,
          zoom_plot_ui("ch2_extrap_plot", height = "320px"),
          uiOutput("ch2_extrap_stats")
        )
      )
    ),

    inline_callout(label = "Zasada", color = "wskazowka",
      "Nigdy nie ufaj predykcji poza zakresem X, na którym model był uczony.
       Im dalej od danych, tym bardziej ryzykowna ekstrapolacja."
    ),

    lc_h2("ch2-co-dalej", "Co dalej"),

    tagList(
      p("Mamy trzy narzędzia do oceny pojedynczego modelu: wzorzec reszt
        (czy linia kłamie), ",
        withMathJax("\\(R^2\\)"), " (ile wyjaśnia), ",
        "RMSE (jak duże pomyłki). To wystarczy, żeby powiedzieć, czy ",
        tags$em("ten"), " model jest wart zaufania."),
      p("Czego jeszcze nie umiemy:"),
      tags$ul(
        tags$li("Porównać dwa modele i wybrać lepszy — rozdział 4 wprowadzi R²adj, AIC, BIC
                 i train/test."),
        tags$li("Modelować zależności od wielu X-ów naraz — rozdział 3 rozszerzy regresję prostą na wieloraką."),
        tags$li("Modelować Y binarne (zdał/nie zdał, kliknął/nie kliknął) — rozdział 5
                 wprowadzi regresję logistyczną.")
      ),
      p("Następnie wracamy do regresji wielorakiej — bo realne dane prawie
        nigdy nie mają tylko jednego X.")
    ),

    lc_chapter_next(
      num       = "03",
      title     = "Regresja wieloraka",
      lead      = "wiele zmiennych objaśniających naraz",
      target_id = "ch-wieloraka"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  # --- Widget: Reszty vs fitted na CASchools ---
  ch2_resid_spec <- reactive({
    case <- input$ch2_resid_case
    if (is.null(case)) case <- "read_income"
    .ch2_resid_specs[[case]]
  })

  ch2_resid_model <- reactive({
    spec <- ch2_resid_spec()
    form <- as.formula(paste(spec$y, "~", spec$x))
    lm(form, data = .cas_data)
  })

  zoom_plot_server("ch2_resid_plot", reactive({
    spec <- ch2_resid_spec()
    model <- ch2_resid_model()

    df_scatter <- data.frame(
      x = .cas_data[[spec$x]],
      y = .cas_data[[spec$y]]
    )
    df_resid <- data.frame(
      fitted = fitted(model),
      resid  = residuals(model)
    )

    p_left <- ggplot(df_scatter, aes(x = x, y = y)) +
      geom_point(color = upwr_secondary, alpha = 0.4, size = 1.7) +
      geom_smooth(method = "lm", se = FALSE,
                  color = unname(upwr_cat["niebo"]), linewidth = 1.2) +
      labs(
        title = "Dane + linia regresji",
        x = unname(.cas_labels[spec$x]),
        y = unname(.cas_labels[spec$y])
      ) +
      theme_upwr()

    p_right <- ggplot(df_resid, aes(x = fitted, y = resid)) +
      geom_point(color = upwr_secondary, alpha = 0.4, size = 1.7) +
      geom_hline(yintercept = 0, color = upwr_reference,
                 linetype = "dashed", linewidth = 0.8) +
      geom_smooth(method = "loess", se = FALSE,
                  color = unname(upwr_cat["terakota"]), linewidth = 1.2) +
      labs(
        title = "Reszty vs dopasowane",
        x = expression(hat(Y)),
        y = expression(e[i] == y[i] - hat(y)[i])
      ) +
      theme_upwr()

    p_qq <- ggplot(df_resid, aes(sample = resid)) +
      stat_qq(color = upwr_secondary, alpha = 0.4, size = 1.7) +
      stat_qq_line(color = unname(upwr_cat["niebo"]), linewidth = 1.2) +
      labs(
        title = "Q-Q reszt",
        x = "Kwantyle teoretyczne",
        y = "Kwantyle próbki"
      ) +
      theme_upwr()

    if (requireNamespace("patchwork", quietly = TRUE)) {
      patchwork::wrap_plots(p_left, p_right, p_qq, ncol = 3)
    } else if (requireNamespace("gridExtra", quietly = TRUE)) {
      gridExtra::grid.arrange(p_left, p_right, p_qq, ncol = 3)
    } else {
      df_combined <- rbind(
        data.frame(panel = "Dane + linia regresji",
                   x = df_scatter$x, y = df_scatter$y),
        data.frame(panel = "Reszty vs dopasowane",
                   x = df_resid$fitted, y = df_resid$resid)
      )
      ggplot(df_combined, aes(x = x, y = y)) +
        geom_point(color = upwr_secondary, alpha = 0.4, size = 1.7) +
        facet_wrap(~ panel, scales = "free", ncol = 2) +
        theme_upwr()
    }
  }))

  output$ch2_resid_verdict <- renderUI({
    spec <- ch2_resid_spec()
    lc_feedback(type = spec$verdict, style = "margin-top: 12px;",
      tags$strong(spec$title),
      p(spec$comment)
    )
  })

  output$ch2_resid_stats <- renderUI({
    spec <- ch2_resid_spec()
    model <- ch2_resid_model()
    g <- broom::glance(model)

    lc_stat_grid(
      lc_stat_box("R²", round(g$r.squared, 3),
                  color = unname(upwr_cat["niebo"])),
      lc_stat_box("RMSE", round(sqrt(mean(residuals(model)^2)), 2),
                  color = unname(upwr_cat["bursztyn"])),
      lc_stat_box("n", nrow(.cas_data),
                  color = upwr_secondary),
      columns = 3
    )
  })

  # --- Widget: R² compare (przeniesiony z ch4) ---
  zoom_plot_server("ch2_r2_compare_plot", reactive({
    set.seed(103)
    make_panel <- function(label, sigma) {
      x <- seq(-3, 3, length.out = 70)
      y <- 10 + 2.2 * x + rnorm(length(x), 0, sigma)
      data.frame(wariant = label, x = x, y = y)
    }
    df <- rbind(
      make_panel("Niskie R²", 12.0),
      make_panel("Średnie R²", 4.0),
      make_panel("Wysokie R²", 0.9)
    )

    r2_levels <- c("Niskie R²", "Średnie R²", "Wysokie R²")
    stats <- df %>%
      group_by(wariant) %>%
      summarise(r2 = summary(lm(y ~ x))$r.squared, .groups = "drop")
    df$wariant <- factor(df$wariant, levels = r2_levels)
    stats$wariant <- factor(stats$wariant, levels = r2_levels)

    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = upwr_secondary, alpha = 0.5, size = 1.9) +
      geom_smooth(method = "lm", se = FALSE,
                  color = unname(upwr_cat["niebo"]), linewidth = 1.1) +
      geom_text(
        data = stats,
        aes(x = -2.8, y = Inf, label = paste0("R² = ", round(r2, 2))),
        inherit.aes = FALSE, hjust = 0, vjust = 1.6,
        color = upwr_secondary, fontface = "bold"
      ) +
      facet_wrap(~ wariant, nrow = 1) +
      labs(x = "X", y = "Y") +
      theme_upwr()
  }))

  # --- Widget: intuicja przeuczenia ---
  ch2_overfit_sets <- local({
    set.seed(26)
    f <- function(x) 4.8 * sin(x)
    train_x <- sort(runif(30, 0, 10))
    test_x <- sort(runif(180, 0, 10))
    list(
      train = data.frame(
        set = "Trening",
        x = train_x,
        y = f(train_x) + rnorm(length(train_x), 0, 0.9)
      ),
      test = data.frame(
        set = "Test",
        x = test_x,
        y = f(test_x) + rnorm(length(test_x), 0, 0.9)
      )
    )
  })

  ch2_overfit_metrics <- reactive({
    train <- ch2_overfit_sets$train
    test <- ch2_overfit_sets$test
    degrees <- c(1, 4, 12)
    do.call(rbind, lapply(degrees, function(degree) {
      model <- lm(y ~ poly(x, degree), data = train)
      data.frame(
        degree = degree,
        train_rmse = sqrt(mean((train$y - predict(model, train))^2)),
        test_rmse = sqrt(mean((test$y - predict(model, test))^2))
      )
    }))
  })

  zoom_plot_server("ch2_overfit_plot", reactive({
    train <- ch2_overfit_sets$train
    test <- ch2_overfit_sets$test
    degrees <- c(1, 4, 12)
    labels <- c(
      "1" = "Zbyt prosty",
      "4" = "Rozsądnie elastyczny",
      "12" = "Przeuczony"
    )

    grid <- do.call(rbind, lapply(degrees, function(degree) {
      model <- lm(y ~ poly(x, degree), data = train)
      x_grid <- seq(0, 10, length.out = 260)
      data.frame(
        degree = factor(degree, levels = degrees, labels = labels[as.character(degrees)]),
        x = x_grid,
        y = predict(model, newdata = data.frame(x = x_grid))
      )
    }))

    train_plot <- train
    test_plot <- test
    train_plot$set <- "Trening"
    test_plot$set <- "Test"
    points <- do.call(rbind, lapply(labels[as.character(degrees)], function(lab) {
      tmp <- rbind(train_plot, test_plot)
      tmp$degree <- factor(lab, levels = labels[as.character(degrees)])
      tmp
    }))

    ggplot() +
      geom_point(data = points[points$set == "Test", ],
                 aes(x = x, y = y), color = unname(upwr_cat["bursztyn"]),
                 alpha = 0.22, size = 1.6) +
      geom_point(data = points[points$set == "Trening", ],
                 aes(x = x, y = y), color = upwr_secondary,
                 alpha = 0.72, size = 2.1) +
      geom_line(data = grid, aes(x = x, y = y),
                color = unname(upwr_cat["niebo"]), linewidth = 1.05) +
      facet_wrap(~ degree, nrow = 1) +
      labs(x = "X", y = "Y", caption = "Ciemne punkty = trening; jasne bursztynowe = nowe dane testowe") +
      coord_cartesian(ylim = c(-8, 8)) +
      theme_upwr()
  }))

  output$ch2_overfit_stats <- renderUI({
    metrics <- ch2_overfit_metrics()
    labels <- c(
      "1" = "Zbyt prosty",
      "4" = "Rozsądnie elastyczny",
      "12" = "Przeuczony"
    )
    metrics$model <- labels[as.character(metrics$degree)]
    rows <- lapply(seq_len(nrow(metrics)), function(i) {
      tags$tr(
        tags$td(metrics$model[i]),
        tags$td(metrics$degree[i]),
        tags$td(round(metrics$train_rmse[i], 2)),
        tags$td(round(metrics$test_rmse[i], 2))
      )
    })

    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-striped lc-table-sm",
        tags$thead(
          tags$tr(tags$th("Model"), tags$th("Stopień"), tags$th("RMSE trening"), tags$th("RMSE test"))
        ),
        tags$tbody(rows)
      ),
      lc_feedback(type = "info",
        p("Przeuczenie rozpoznajemy po rozjechaniu błędu treningowego i testowego:
          model dobrze pamięta punkty, które widział, ale gorzej działa na nowych danych."))
    )
  })

  # --- Widget: RMSE i zakres Y na CASchools ---
  ch2_rmse_spec <- reactive({
    case <- input$ch2_rmse_case
    if (is.null(case)) case <- "read_lunch"
    .ch2_rmse_specs[[case]]
  })

  ch2_rmse_model <- reactive({
    spec <- ch2_rmse_spec()
    form <- as.formula(paste(spec$y, "~", spec$x))
    lm(form, data = .cas_data)
  })

  zoom_plot_server("ch2_rmse_plot", reactive({
    spec <- ch2_rmse_spec()
    model <- ch2_rmse_model()
    rmse <- sqrt(mean(residuals(model)^2))
    y_vals <- .cas_data[[spec$y]]
    y_mean <- mean(y_vals)

    df <- data.frame(
      x = .cas_data[[spec$x]],
      y = y_vals,
      fitted = fitted(model)
    )

    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = upwr_secondary, alpha = 0.42, size = 1.8) +
      geom_smooth(method = "lm", se = FALSE,
                  color = unname(upwr_cat["niebo"]), linewidth = 1.2) +
      geom_ribbon(
        data = local({
          ord <- order(df$x)
          data.frame(x = df$x[ord], ymin = df$fitted[ord] - rmse, ymax = df$fitted[ord] + rmse)
        }),
        aes(x = x, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE,
        fill = unname(upwr_cat["bursztyn"]), alpha = 0.18
      ) +
      annotate("label", x = min(df$x), y = max(df$y),
               hjust = 0, vjust = 1,
               label = paste0("Pasmo ±RMSE = ±", round(rmse, 1)),
               color = unname(upwr_cat["bursztyn"]),
               fill = "white", linewidth = 0) +
      labs(
        x = unname(.cas_labels[spec$x]),
        y = unname(.cas_labels[spec$y])
      ) +
      theme_upwr()
  }))

  output$ch2_rmse_stats <- renderUI({
    spec <- ch2_rmse_spec()
    model <- ch2_rmse_model()
    g <- broom::glance(model)
    rmse <- sqrt(mean(residuals(model)^2))
    y_vals <- .cas_data[[spec$y]]
    y_range <- diff(range(y_vals))
    rmse_ratio <- rmse / y_range

    lc_stat_grid(
      lc_stat_box("R²", round(g$r.squared, 3),
                  color = unname(upwr_cat["niebo"])),
      lc_stat_box("RMSE", round(rmse, 2),
                  caption = paste("jednostek", unname(.cas_labels[spec$y])),
                  color = unname(upwr_cat["bursztyn"])),
      lc_stat_box("Zakres Y", round(y_range, 1),
                  caption = "max − min",
                  color = upwr_secondary),
      lc_stat_box("RMSE / zakres", paste0(round(rmse_ratio * 100, 1), "%"),
                  color = unname(upwr_cat["terakota"])),
      columns = 4
    )
  })

  # --- Widget: Ekstrapolacja ---
  .ch2_extrap_model <- lm(read ~ income, data = .cas_data)
  .ch2_extrap_x_range <- range(.cas_data$income)

  zoom_plot_server("ch2_extrap_plot", reactive({
    x_val <- input$ch2_extrap_x
    if (is.null(x_val)) x_val <- 20
    x_obs <- .cas_data$income
    y_obs <- .cas_data$read
    x_range <- .ch2_extrap_x_range
    x_grid <- seq(min(1, x_val - 2), max(80, x_val + 2), length.out = 300)
    df_line <- data.frame(
      income = x_grid,
      read   = predict(.ch2_extrap_model, newdata = data.frame(income = x_grid)),
      outside = x_grid < x_range[1] | x_grid > x_range[2]
    )
    y_pred <- predict(.ch2_extrap_model, newdata = data.frame(income = x_val))
    in_range <- x_val >= x_range[1] & x_val <= x_range[2]
    point_color <- if (in_range) unname(upwr_cat["niebo"]) else unname(upwr_cat["terakota"])

    ggplot() +
      annotate("rect",
        xmin = x_range[1], xmax = x_range[2],
        ymin = -Inf, ymax = Inf,
        fill = upwr_secondary, alpha = 0.08) +
      geom_point(data = data.frame(x = x_obs, y = y_obs),
                 aes(x = x, y = y),
                 color = upwr_secondary, alpha = 0.35, size = 1.6) +
      geom_line(data = df_line[!df_line$outside, ],
                aes(x = income, y = read),
                color = unname(upwr_cat["niebo"]), linewidth = 1.1) +
      geom_line(data = df_line[df_line$outside, ],
                aes(x = income, y = read),
                color = unname(upwr_cat["niebo"]), linewidth = 1.1,
                linetype = "dashed") +
      geom_vline(xintercept = x_val, color = point_color,
                 linetype = "dotted", linewidth = 0.9) +
      geom_point(data = data.frame(x = x_val, y = y_pred),
                 aes(x = x, y = y),
                 color = point_color, size = 4, shape = 18) +
      labs(
        x = "Dochód okręgu (tys. USD)",
        y = "Wynik testu czytania",
        caption = "Szary pas = zakres danych treningowych"
      ) +
      theme_upwr()
  }))

  output$ch2_extrap_verdict <- renderUI({
    x_val <- input$ch2_extrap_x
    if (is.null(x_val)) x_val <- 20
    x_range <- .ch2_extrap_x_range
    y_pred <- predict(.ch2_extrap_model, newdata = data.frame(income = x_val))
    in_range <- x_val >= x_range[1] & x_val <= x_range[2]
    dist_pct <- min(abs(x_val - x_range[1]), abs(x_val - x_range[2])) /
                diff(x_range) * 100

    if (in_range) {
      lc_feedback(type = "ok", style = "margin-top: 12px;",
        tags$strong("W zakresie danych"),
        p(sprintf("Predykcja: %.1f pkt. Jesteśmy wewnątrz zakresu danych — predykcja ma sens.", y_pred))
      )
    } else {
      lc_feedback(type = "warning", style = "margin-top: 12px;",
        tags$strong("Ekstrapolacja!"),
        p(sprintf("Predykcja: %.1f pkt. Jesteśmy %.0f%% zakresu danych poza granicą — brak gwarancji.", y_pred, dist_pct))
      )
    }
  })

  output$ch2_extrap_stats <- renderUI({
    x_val <- input$ch2_extrap_x
    if (is.null(x_val)) x_val <- 20
    x_range <- .ch2_extrap_x_range
    y_pred <- predict(.ch2_extrap_model, newdata = data.frame(income = x_val))

    lc_stat_grid(
      lc_stat_box("X podany", x_val,
                  caption = "tys. USD",
                  color = unname(upwr_cat["niebo"])),
      lc_stat_box("Predykcja", round(y_pred, 1),
                  caption = "pkt czytania",
                  color = unname(upwr_cat["bursztyn"])),
      lc_stat_box("Zakres X danych",
                  paste0(round(x_range[1], 0), "–", round(x_range[2], 0)),
                  caption = "tys. USD",
                  color = upwr_secondary),
      columns = 3
    )
  })

  output$ch2_rmse_interpretation <- renderUI({
    spec <- ch2_rmse_spec()
    model <- ch2_rmse_model()
    rmse <- sqrt(mean(residuals(model)^2))
    y_vals <- .cas_data[[spec$y]]
    y_range <- diff(range(y_vals))
    rmse_ratio <- rmse / y_range
    y_label <- unname(.cas_labels[spec$y])

    verdict_type <- if (rmse_ratio < 0.05) "ok"
                    else if (rmse_ratio < 0.12) "info"
                    else "warning"

    verdict_text <- if (rmse_ratio < 0.05) {
      "Typowa pomyłka jest mała w stosunku do zakresu Y — model robi co trzeba."
    } else if (rmse_ratio < 0.12) {
      "Typowa pomyłka jest umiarkowana w stosunku do zakresu Y. Można dyskutować, czy to dość."
    } else {
      "Typowa pomyłka jest duża w stosunku do zakresu Y. Model ma ograniczoną wartość praktyczną."
    }

    lc_feedback(type = verdict_type, style = "margin-top: 12px;",
      p(sprintf("Typowa pomyłka modelu to ±%.1f w skali „%s\" (zakres %.0f).",
                rmse, y_label, y_range)),
      p(verdict_text)
    )
  })
}
