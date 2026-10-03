# ============================================================================
# CHAPTER 3: Regresja wieloraka
# ============================================================================

ch3_ui <- list(
  id    = "ch-wieloraka",
  num   = "03",
  title = "Regresja wieloraka",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Regresja",
      num   = "03",
      title  = "Regresja wieloraka.",
      lead   = "Drugi predyktor w modelu zmienia znaczenie pierwszego. Współczynnik
                dochodu w danych o szkołach z Kalifornii maleje prawie czterokrotnie,
                gdy obok dochodu pojawia się odsetek uczniów z dotacją do obiadu."
    ),

    lc_p("W rozdziale 01 opisywaliśmy wynik jedną zmienną, a w rozdziale 02
      ocenialiśmy, czy taki model jest wart zaufania: patrzyliśmy na reszty,
      \\(R^2\\) i RMSE. Na wynik zwykle działa jednak wiele czynników naraz,
      a te czynniki są ze sobą powiązane. Model z jednym ",
      gloss("predyktor", "predyktorem"), " przypisuje wtedy jednej zmiennej
      także to, co należy się innym."),

    lc_p("Tę sytuację znamy z ćwiczeń w wykładzie 04. Wyniki czytania w okręgach
      szkolnych Kalifornii korelowały ujemnie z liczbą uczniów na nauczyciela
      (\\(r = -0.25\\)), ale okręgi z mniejszymi klasami bywają też
      zamożniejsze, więc część tej korelacji mógł tłumaczyć dochód. Korelacja
      nie pozwalała tego sprawdzić, bo zawsze dotyczy tylko dwóch zmiennych.
      Potrzebny jest model, który uwzględnia kilka zmiennych naraz."),

    lc_h2("ch3-wiele-predyktorow", "Wiele predyktorów naraz"),

    lc_p(gloss("regresja wieloraka", "Regresja wieloraka"), " rozszerza równanie
      prostej o kolejne predyktory. Przy \\(k\\) predyktorach model ma postać:"),

    lc_formula_box(withMathJax(
      "$$Y = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\ldots + \\beta_k X_k + \\varepsilon$$"
    )),

    lc_p("Współczynniki szacuje się tak samo jak w ",
      gloss("regresja prosta", "regresji prostej"), ": metodą najmniejszych
      kwadratów, czyli tak, by suma kwadratów reszt była jak najmniejsza.
      Zmienia się natomiast interpretacja. Współczynnik \\(\\beta_j\\) mówi,
      o ile średnio zmienia się \\(Y\\), gdy \\(X_j\\) rośnie o jednostkę,
      a wszystkie pozostałe predyktory mają te same wartości."),

    lc_p("To zastrzeżenie jest sednem regresji wielorakiej. W regresji prostej
      współczynnik zbiera cały związek \\(X\\) z \\(Y\\), także ten, który
      przechodzi przez zmienne pominięte w modelu. W regresji wielorakiej
      zostaje tylko ta część związku, której nie da się przypisać pozostałym
      predyktorom. Dlatego ten sam predyktor może mieć w obu modelach zupełnie
      inny współczynnik, czasem nawet o przeciwnym znaku."),

    lc_h2("ch3-budowanie", "Budowanie modelu wielorakiego"),

    lc_p("Zobaczmy, jak to wygląda na danych. Zbiór CASchools opisuje 420
      okręgów szkolnych w Kalifornii: średnie wyniki testów z czytania
      i matematyki oraz cechy okręgu, takie jak dochód mieszkańców, odsetek
      uczniów z dotacją do obiadu czy liczba uczniów na nauczyciela. To ",
      gloss("dane obserwacyjne", "dane obserwacyjne"), ": nikt nie przydzielał
      okręgom dochodów ani wielkości klas, więc wszystkie te cechy są ze sobą
      splecione."),

    lc_p("Panel poniżej zawsze zaczyna od dochodu okręgu i pozwala dokładać
      kolejne predyktory do tego samego równania. Tabela pokazuje ",
      gloss("współczynnik regresji", "współczynniki"), " pełnego modelu,
      wykres u góry rozbija dane na grupy według dodanych zmiennych,
      a wykres u dołu porównuje linię regresji prostej z linią modelu,
      w którym pozostałe predyktory ustawiono na ich średnich."),

    figure_panel(
      label = "Ryc. 3.1", title = "CASchools: model z wieloma predyktorami",
      full_width = TRUE,
      lc_toolbar(
        selectInput("ch3_outcome", "Zmienna zależna Y",
          choices = c(
            "Wynik: czytanie" = "read",
            "Wynik: matematyka" = "math"
          ),
          selected = "read"
        ),
        checkboxGroupInput("ch3_predictors", "Predyktory dodane do dochodu okręgu",
          choices = c(
            "Dotacje do obiadów (%)" = "lunch",
            "Angielski jako drugi język (%)" = "english",
            "Uczniowie / nauczyciel" = "student_teacher_ratio",
            "Wydatki na ucznia" = "expenditure",
            "Komputery" = "computer",
            "CalWORKs (%)" = "calworks"
          ),
          selected = "lunch", inline = TRUE
        ),
        lc_readouts(uiOutput("ch3_model_stats"))
      ),
      lc_caption("Dane: 420 okręgów szkolnych w Kalifornii. Model zawsze zaczyna od
        dochodu okręgu (tys. USD); zaznaczone predyktory dochodzą do tego samego
        równania."),
      lc_note("Jak czytać",
        p("Tabela pokazuje współczynniki pełnego modelu addytywnego,
          bez interakcji. Gwiazdka przy p-wartości oznacza p < 0.05.")
      ),
      uiOutput("ch3_model_coefs"),
      uiOutput("ch3_prediction_plot_ui")
    ),

    lc_p("W ustawieniu startowym model wyjaśnia wynik z czytania dochodem
      i odsetkiem uczniów z dotacją do obiadu. Sam dochód miał w regresji
      prostej współczynnik 1.94: okręg zamożniejszy o tysiąc dolarów miał
      średnio o 1.94 punktu wyższy wynik. Po dodaniu dotacji współczynnik
      dochodu spada do 0.50. Dochód i odsetek dotacji są silnie powiązane
      (\\(r = -0.68\\)), bo w biedniejszych okręgach więcej dzieci
      kwalifikuje się do dotacji. W regresji prostej dochód zbierał więc także
      związek, który teraz przejmuje odsetek dotacji (współczynnik -0.56
      na punkt procentowy). \\(R^2\\) rośnie z 0.49 do 0.79."),

    lc_p("Każdy dodany predyktor wnosi do równania własny składnik, a jego
      współczynnik nie zależy od wartości pozostałych zmiennych. Taki model
      nazywa się addytywnym. Bywa, że wpływ jednej zmiennej zależy od drugiej,
      na przykład nachylenie jest inne w każdej grupie. Opisuje to ",
      gloss("interakcja", "interakcja"), ", której poświęcony jest rozdział 03B.
      Diagnostyka z rozdziału 02 obowiązuje w modelu wielorakim bez zmian:
      reszty ocenia się tak samo, niezależnie od liczby predyktorów."),

    lc_h2("ch3-kontrola", "Co znaczy „przy stałych pozostałych zmiennych”?"),

    lc_p("Spadek współczynnika dochodu z 1.94 do 0.50 wymaga wyjaśnienia,
      bo dane się nie zmieniły. Zmieniło się pytanie, na które odpowiada
      współczynnik. W regresji prostej porównujemy wszystkie okręgi bogatsze
      ze wszystkimi biedniejszymi. W regresji wielorakiej porównujemy okręgi,
      które różnią się dochodem, ale mają taki sam odsetek dotacji do obiadu.
      Mówimy wtedy, że odsetek dotacji jest ",
      gloss("zmienna kontrolna", "zmienną kontrolną"), "."),

    lc_p("Kontrolowanie zmiennej oznacza, że z predyktora usuwa się informację,
      którą niesie już inny predyktor. Zostaje część unikalna: to, czym
      okręgi o tym samym odsetku dotacji nadal różnią się dochodem. Właśnie
      tę część model wiąże z wynikiem. Panel poniżej pokazuje to w trzech
      krokach: dwie regresje proste, a potem model z dochodem, odsetkiem
      dotacji i odsetkiem uczniów uczących się angielskiego jako drugiego
      języka."),

    figure_panel(
      label = "Ryc. 3.2",
      full_width = TRUE,
      lc_step_widget("ch3_control",
        title = "Efekt pozorny i kontrola zmiennych",
        steps = c("Czytanie ~ dochód", "Czytanie ~ lunch", "Model z kontrolą"),
        plot_id = "ch3_control_plot",
        ratio = "2/1",
        extra = uiOutput("ch3_control_table")
      )
    ),

    lc_p("W modelu z trzema predyktorami współczynnik dochodu wynosi 0.70.
      Wśród okręgów o tym samym odsetku dotacji i tym samym odsetku uczniów
      uczących się angielskiego okręg zamożniejszy o tysiąc dolarów ma średnio
      wynik wyższy o 0.70 punktu. To prawie trzy razy mniej niż w regresji
      prostej. Dotacje (-0.40) i odsetek uczniów uczących się angielskiego
      (-0.29) mają ujemne współczynniki, a przedziały ufności wszystkich
      trzech leżą daleko od zera."),

    lc_p("Teraz można wrócić do zadania z wykładu 04. Tam pytaliśmy, czy dochód
      jest ", gloss("zmienna zakłócająca", "zmienną zakłócającą"), " związku
      między liczbą uczniów na nauczyciela a wynikiem z czytania. W regresji
      prostej każdy dodatkowy uczeń na nauczyciela wiąże się z wynikiem
      niższym średnio o 2.62 punktu. Po dodaniu dochodu współczynnik spada
      do -0.95, a więc dochód tłumaczy większą część pierwotnego związku.
      Związek nie znika jednak: po dodaniu jeszcze odsetka dotacji i odsetka
      uczniów uczących się angielskiego współczynnik wynosi -0.78, a p-wartość
      jest mniejsza niż 0.001. Ten model można zbudować w panelu Ryc. 3.1."),

    lc_p("Kontrola zmiennych ma jednak granice. Model uwzględnia tylko te
      zmienne, które do niego włożyliśmy. Jeśli istnieje zmienna pominięta,
      związana i z predyktorem, i z wynikiem, współczynnik nadal ją zawiera.
      Dlatego współczynnik z danych obserwacyjnych opisuje związek po
      uwzględnieniu wybranych zmiennych, a nie dowodzi przyczyny. Dodanie
      zmiennej może też odwrócić znak współczynnika. To ",
      gloss("paradoks Simpsona", "paradoks Simpsona"), " znany z wykładu 04.
      W CASchools do odwrócenia nie dochodzi, ale rozdział 03B pokazuje je
      na danych o pingwinach."),

    lc_h2("ch3-wspolliniowosc", "Współliniowość"),

    lc_p("Kontrola działa dlatego, że dochód i odsetek dotacji niosą częściowo
      różną informację. Przy korelacji -0.68 okręgi o podobnym dochodzie wciąż
      wyraźnie różnią się odsetkiem dotacji, więc model ma z czego oszacować
      osobny związek każdej zmiennej. Problem pojawia się, gdy dwa predyktory mówią
      prawie to samo. ", gloss("współliniowość", "Współliniowość"), " to
      silna korelacja między predyktorami. Model widzi wtedy niewiele okręgów,
      w których jedna zmienna rośnie, a druga nie, więc trudno mu rozdzielić
      wspólny związek między obie zmienne."),

    lc_p("Siłę tego problemu mierzy ", gloss("VIF"), ", czyli współczynnik
      inflacji wariancji. Dla predyktora \\(X_j\\) liczy się go z
      \\(R_j^2\\) modelu, w którym \\(X_j\\) przewiduje się pozostałymi
      predyktorami:"),

    lc_formula_box(withMathJax(
      "$$\\text{VIF}_j = \\frac{1}{1 - R_j^2}$$"
    )),

    lc_p("Gdy predyktor nie jest związany z pozostałymi, \\(R_j^2 = 0\\)
      i VIF wynosi 1. Im lepiej pozostałe predyktory odtwarzają \\(X_j\\),
      tym większy VIF. Pierwiastek z VIF mówi, ile razy błąd standardowy
      współczynnika jest większy niż przy predyktorach nieskorelowanych:
      VIF równy 4 oznacza błąd standardowy dwa razy większy. Nie ma jednej
      granicy, od której VIF jest za duży. Wartości bliskie 1 nie budzą
      wątpliwości, a im dalej od 1, tym ostrożniej interpretuje się
      pojedyncze współczynniki."),

    lc_p("Panel poniżej losuje 140 obserwacji z modelu, w którym oba predyktory
      mają prawdziwy współczynnik 1.1, i dopasowuje do nich regresję
      wieloraką. Suwak ustala korelację między \\(X_1\\) i \\(X_2\\)."),

    figure_panel(
      label = "Ryc. 3.3", title = "Gdy predyktory mówią prawie to samo",
      full_width = TRUE,
      lc_toolbar(
        lc_slider("ch3_collin_rho", "Korelacja X₁–X₂", 0, 0.98, 0.8, 0.02),
        lc_action("ch3_collin_new", "Generuj i dopasuj", variant = "solid"),
        lc_readouts(uiOutput("ch3_collin_info"))
      ),
      lc_plot("ch3_collin_plot", max_height = "300px"),
      uiOutput("ch3_collin_table")
    ),

    lc_p("Przy korelacji 0.8 VIF wynosi około 2.8, a błąd standardowy każdego
      współczynnika jest około 1.7 razy większy niż przy predyktorach
      nieskorelowanych. Przy korelacji 0.98 VIF wynosi około 25, a błąd
      standardowy rośnie mniej więcej pięciokrotnie. Kolejne losowania przy
      takiej korelacji dają współczynniki wyraźnie różne od siebie i od 1.1,
      choć \\(R^2\\) modelu pozostaje wysokie. Model jako całość dobrze
      przewiduje \\(Y\\). Niepewny jest tylko podział tego przewidywania
      między \\(X_1\\) i \\(X_2\\)."),

    lc_p("W CASchools współliniowość jest umiarkowana. W modelu ze wszystkimi
      siedmioma predyktorami z Ryc. 3.1 największy VIF, około 5.7, ma odsetek
      uczniów z dotacją do obiadu. Ta zmienna jest silnie związana zarówno
      z dochodem (\\(r = -0.68\\)), jak i z odsetkiem uczniów z rodzin
      objętych pomocą socjalną CalWORKs (\\(r = 0.74\\)). Gdy współczynniki dwóch predyktorów są
      niestabilne, można zostawić w modelu jeden z nich albo połączyć je
      w jedną miarę."),

    inline_callout(label = "Zasada",
      "Współliniowość nie psuje przewidywań modelu. Osłabia interpretację
       pojedynczych współczynników, dlatego VIF sprawdza się, zanim zacznie
       się je interpretować."
    ),

    lc_h2("ch3-co-dalej", "Co dalej"),

    lc_p("Regresja wieloraka pozwala opisać wynik wieloma zmiennymi naraz,
      a każdy jej współczynnik odpowiada na pytanie o związek przy stałych
      pozostałych predyktorach. Dotąd wszystkie predyktory były ilościowe,
      a ich związek z wynikiem był taki sam w każdej grupie. Rozdział 03B
      pokazuje na danych o pingwinach, co się dzieje, gdy w danych są naturalne
      grupy: pominięta zmienna może odwrócić kierunek związku (paradoks
      Simpsona), grupę można wpisać do równania jako predyktor jakościowy,
      a interakcja pozwala, by nachylenie różniło się między grupami."),

    lc_p("Zostaje pytanie, który model wybrać. Najwyższe \\(R^2\\) nie jest
      dobrym kryterium: w CASchools rośnie z 0.49 dla samego dochodu do 0.79
      po dodaniu dotacji i do 0.83 po dodaniu odsetka uczniów uczących się
      angielskiego, ale rosłoby też po dodaniu predyktora zupełnie losowego.
      Miary, które karzą model za złożoność, wprowadza rozdział 04."),

    lc_chapter_next(
      num       = "03B",
      title     = "Kontekst i interakcje",
      lead      = "pominięta zmienna, predyktor jakościowy i interakcje",
      target_id = "ch-3b"
    )
  )
)


# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  ch3_labels_pl <- c(
    "read" = "Wynik: czytanie",
    "math" = "Wynik: matematyka",
    "lunch" = "Dotacje do obiadów (%)",
    "income" = "Dochód okręgu (tys. USD)",
    "english" = "Angielski jako drugi język (%)",
    "student_teacher_ratio" = "Uczniowie / nauczyciel",
    "expenditure" = "Wydatki na ucznia",
    "computer" = "Komputery",
    "calworks" = "CalWORKs (%)"
  )

  ch3_base_x <- "income"

  ch3_selected_predictors <- reactive({
    unique(c(ch3_base_x, input$ch3_predictors))
  })

  # Model jako reactive: zależy od danych i wyboru predyktorów.
  # Dzięki temu przełączanie checkboxów porównuje modele na TYCH SAMYCH danych.
  ch3_model <- reactive({
    df <- .cas_data
    outcome <- input$ch3_outcome
    if (is.null(outcome)) outcome <- "read"
    preds <- ch3_selected_predictors()
    formula <- as.formula(paste(outcome, "~", paste(preds, collapse = " + ")))
    lm(formula, data = df)
  })

  output$ch3_model_coefs <- renderUI({
    model <- ch3_model()
    if (is.null(model)) return(NULL)

    coefs <- broom::tidy(model)

    labels_pl <- c("(Intercept)" = "Wyraz wolny", ch3_labels_pl)

    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)

    df <- data.frame(
      term = unname(coefs$term_pl),
      estimate = coefs$estimate,
      se = coefs$std.error,
      t = coefs$statistic,
      p = paste0(lc_pval(coefs$p.value), ifelse(coefs$p.value < 0.05, " *", ""))
    )

    lc_table_split(df,
      cols = list(
        lc_col("term", "Zmienna", "row"),
        lc_col("estimate", "Estymata", digits = 4),
        lc_col("se", "SE", digits = 4),
        lc_col("t", "t", digits = 3),
        lc_col("p", "p")
      ),
      groups = list(c("estimate", "se"), c("t", "p")),
      label = "Współczynniki modelu"
    )
  })

  ch3_make_bins <- function(x, labels) {
    probs <- seq(0, 1, length.out = length(labels) + 1)
    breaks <- unique(as.numeric(quantile(x, probs = probs, na.rm = TRUE)))
    if (length(breaks) <= 2) {
      cut(x, breaks = 2, include.lowest = TRUE, labels = labels[seq_len(2)])
    } else {
      cut(x, breaks = breaks, include.lowest = TRUE, labels = labels[seq_len(length(breaks) - 1)])
    }
  }

  ch3_group_reps <- function(df, var, labels) {
    group <- ch3_make_bins(df[[var]], labels)
    reps <- tapply(df[[var]], group, median, na.rm = TRUE)
    list(group = group, reps = reps)
  }

  ch3_prediction_grid <- function(df, predictors, x_var) {
    extra <- setdiff(predictors, x_var)
    x_grid <- seq(min(df[[x_var]], na.rm = TRUE), max(df[[x_var]], na.rm = TRUE), length.out = 120)
    grid <- data.frame(x = x_grid)
    names(grid) <- x_var

    if (length(extra) >= 1) {
      color_info <- ch3_group_reps(df, extra[1], c("niskie", "średnie", "wysokie"))
      grid <- merge(
        grid,
        data.frame(color_group = names(color_info$reps), stringsAsFactors = FALSE),
        all = TRUE
      )
      grid[[extra[1]]] <- as.numeric(color_info$reps[grid$color_group])
    }

    facet_vars <- extra[-1]
    if (length(facet_vars) >= 1) {
      levels_1 <- if (length(facet_vars) == 1) c("niskie", "średnie", "wysokie") else c("niższe", "wyższe")
      facet_info_1 <- ch3_group_reps(df, facet_vars[1], levels_1)
      grid <- merge(
        grid,
        data.frame(facet_1 = names(facet_info_1$reps), stringsAsFactors = FALSE),
        all = TRUE
      )
      grid[[facet_vars[1]]] <- as.numeric(facet_info_1$reps[grid$facet_1])
    }

    if (length(facet_vars) >= 2) {
      facet_info_2 <- ch3_group_reps(df, facet_vars[2], c("niższe", "wyższe"))
      grid <- merge(
        grid,
        data.frame(facet_2 = names(facet_info_2$reps), stringsAsFactors = FALSE),
        all = TRUE
      )
      grid[[facet_vars[2]]] <- as.numeric(facet_info_2$reps[grid$facet_2])
    }

    other_vars <- setdiff(predictors, names(grid))
    for (var in other_vars) {
      grid[[var]] <- mean(df[[var]], na.rm = TRUE)
    }

    grid
  }

  output$ch3_prediction_plot_ui <- renderUI({
    if (length(ch3_selected_predictors()) > 4) return(NULL)
    tagList(
      lc_plot("ch3_coef_plot", max_height = "320px"),
      if (length(ch3_selected_predictors()) > 1) {
        lc_plot("ch3_compare_plot", max_height = "230px")
      }
    )
  })

  zoom_plot_server("ch3_coef_plot", reactive({
    model <- ch3_model()
    if (is.null(model)) return(NULL)

    df <- .cas_data
    outcome <- input$ch3_outcome
    if (is.null(outcome)) outcome <- "read"
    predictors <- ch3_selected_predictors()
    if (length(predictors) > 4) return(NULL)
    x_var <- ch3_base_x

    plot_df <- df
    extra <- setdiff(predictors, x_var)
    color_var <- extra[1]
    if (!is.na(color_var)) {
      plot_df$color_group <- ch3_group_reps(plot_df, color_var, c("niskie", "średnie", "wysokie"))$group
    }

    grid <- ch3_prediction_grid(df, predictors, x_var)
    grid$pred <- predict(model, newdata = grid)

    if (length(predictors) >= 3) {
      facet_vars <- extra[-1]
      levels_1 <- if (length(facet_vars) == 1) c("niskie", "średnie", "wysokie") else c("niższe", "wyższe")
      plot_df$facet_1 <- ch3_group_reps(plot_df, facet_vars[1], levels_1)$group

      if (length(facet_vars) >= 2) {
        plot_df$facet_2 <- ch3_group_reps(plot_df, facet_vars[2], c("niższe", "wyższe"))$group
      }
    }

    p <- ggplot(plot_df, aes(x = .data[[x_var]], y = .data[[outcome]])) +
      labs(
        x = ch3_labels_pl[[x_var]],
        y = ch3_labels_pl[[outcome]]
      ) +
      theme_upwr() +
      theme(legend.position = "top")

    if (length(predictors) == 1) {
      p <- p +
        geom_point(color = upwr_secondary, alpha = 0.45, size = 1.8) +
        geom_line(data = grid, aes(x = .data[[x_var]], y = pred),
                  color = unname(upwr_cat["niebo"]), linewidth = 1.05) +
        guides(color = "none", linetype = "none")
    } else {
      p <- p +
        geom_point(aes(color = color_group), alpha = 0.68, size = 1.9) +
        geom_line(data = grid, aes(x = .data[[x_var]], y = pred, color = color_group),
                  linewidth = 1.05) +
        scale_color_manual(
          values = c(
            "niskie" = unname(upwr_cat["szalwia"]),
            "średnie" = unname(upwr_cat["bursztyn"]),
            "wysokie" = unname(upwr_cat["terakota"])
          ),
          name = ch3_labels_pl[[color_var]]
        )
    }

    if (length(predictors) == 1) {
      p <- p
    } else if (length(predictors) == 3) {
      facet_label <- ch3_labels_pl[[extra[-1][1]]]
      p <- p + facet_grid(
        cols = vars(facet_1),
        labeller = labeller(facet_1 = function(x) paste(facet_label, x))
      )
    } else if (length(predictors) == 4) {
      facet_vars <- extra[-1]
      facet_label_1 <- ch3_labels_pl[[facet_vars[1]]]
      facet_label_2 <- ch3_labels_pl[[facet_vars[2]]]
      p <- p + facet_grid(
        rows = vars(facet_1),
        cols = vars(facet_2),
        labeller = labeller(
          facet_1 = function(x) paste(facet_label_1, x),
          facet_2 = function(x) paste(facet_label_2, x)
        )
      )
    }

    p
  }))

  zoom_plot_server("ch3_compare_plot", reactive({
    model <- ch3_model()
    if (is.null(model)) return(NULL)

    df <- .cas_data
    outcome <- input$ch3_outcome
    if (is.null(outcome)) outcome <- "read"
    predictors <- ch3_selected_predictors()
    if (length(predictors) <= 1 || length(predictors) > 4) return(NULL)
    x_var <- ch3_base_x

    x_grid <- seq(min(df[[x_var]], na.rm = TRUE), max(df[[x_var]], na.rm = TRUE), length.out = 120)
    simple_model <- lm(as.formula(paste(outcome, "~", x_var)), data = df)

    simple_grid <- data.frame(x = x_grid)
    names(simple_grid) <- x_var
    simple_grid$pred <- predict(simple_model, newdata = simple_grid)
    simple_grid$model <- "Regresja prosta"

    model_grid <- data.frame(x = x_grid)
    names(model_grid) <- x_var
    for (var in setdiff(predictors, x_var)) {
      model_grid[[var]] <- mean(df[[var]], na.rm = TRUE)
    }
    model_grid$pred <- predict(model, newdata = model_grid)
    model_grid$model <- "Aktualny model"

    line_cols <- c(x_var, "pred", "model")
    line_df <- rbind(simple_grid[line_cols], model_grid[line_cols])

    ggplot(df, aes(x = .data[[x_var]], y = .data[[outcome]])) +
      geom_point(color = upwr_secondary, alpha = 0.28, size = 1.5) +
      geom_line(data = line_df, aes(x = .data[[x_var]], y = pred, color = model, linetype = model),
                linewidth = 1.05) +
      scale_color_manual(
        values = c("Regresja prosta" = unname(upwr_cat["terakota"]),
                   "Aktualny model" = unname(upwr_cat["niebo"])),
        name = NULL
      ) +
      scale_linetype_manual(
        values = c("Regresja prosta" = "dashed", "Aktualny model" = "solid"),
        name = NULL
      ) +
      labs(x = ch3_labels_pl[[x_var]], y = ch3_labels_pl[[outcome]]) +
      theme_upwr() +
      theme(legend.position = "top")
  }), alt = "Linia regresji prostej i linia aktualnego modelu wielorakiego przy średnich wartościach pozostałych predyktorów.")

  output$ch3_model_stats <- renderUI({
    model <- ch3_model()
    if (is.null(model)) return(NULL)
    metrics <- compute_model_metrics(model)
    tagList(
      lc_readout("R²", round(metrics$r_squared, 3), color = unname(upwr_cat["niebo"])),
      lc_readout("adj.R²", round(metrics$adj_r_squared, 3), color = unname(upwr_cat["szalwia"])),
      lc_readout("AIC", round(metrics$aic, 1), color = unname(upwr_cat["bursztyn"])),
      lc_readout("RMSE", round(metrics$rmse, 3), color = unname(upwr_cat["terakota"]))
    )
  })

  # --- Widget: kontrola zmiennych na CASchools ---
  # Krok widgetu (1..3) żyje w przeglądarce.
  ch3_control_step <- lc_step_server("ch3_control", input)$step

  zoom_plot_server("ch3_control_plot", reactive({
    df <- .cas_data
    step <- ch3_control_step()
    pad <- function(v, m = 0.05) v + c(-1, 1) * diff(v) * m
    if (step == 1) {
      ggplot(df, aes(x = income, y = read)) +
        step_layer(geom_point, "data") +
        step_layer(geom_smooth, "new", method = "lm", formula = y ~ x, se = FALSE) +
        labs(x = "Dochód okręgu (tys. USD)", y = "Wynik: czytanie") +
        step_frame(xlim = pad(range(df$income)), ylim = pad(range(df$read)))
    } else if (step == 2) {
      ggplot(df, aes(x = lunch, y = read)) +
        step_layer(geom_point, "data") +
        step_layer(geom_smooth, "new", method = "lm", formula = y ~ x, se = FALSE) +
        labs(x = "Dotacje do obiadów (%)", y = "Wynik: czytanie") +
        step_frame(xlim = pad(range(df$lunch)), ylim = pad(range(df$read)))
    } else {
      model <- lm(read ~ income + lunch + english, data = df)
      coefs <- broom::tidy(model)
      coefs <- coefs[coefs$term != "(Intercept)", ]
      labels <- c(
        income = "Dochód okręgu",
        lunch = "Dotacje do obiadów",
        english = "Angielski jako drugi język"
      )
      coefs$term <- labels[coefs$term]
      coefs$lower <- coefs$estimate - 1.96 * coefs$std.error
      coefs$upper <- coefs$estimate + 1.96 * coefs$std.error
      ggplot(coefs, aes(x = estimate, y = term)) +
        step_line("known", xintercept = 0) +
        step_layer(geom_point, "new", size = 3) +
        step_layer(geom_errorbar, "new", mapping = aes(xmin = lower, xmax = upper),
                   width = 0.2, orientation = "y") +
        labs(x = "β w modelu wielorakim", y = NULL) +
        step_frame(xlim = pad(range(c(0, coefs$lower, coefs$upper))),
                   ylim = c(0.5, nrow(coefs) + 0.5))
    }
  }), alt = "Wykres pokazujący zależności proste i współczynniki po kontroli pozostałych zmiennych.")

  output$ch3_control_text <- renderUI({
    df <- .cas_data
    step <- ch3_control_step()
    if (step == 1) {
      ti <- broom::tidy(lm(read ~ income, data = df))
      tagList(
        "β dochód = ", tags$b(round(ti$estimate[2], 3)), ", p = ",
        tags$b(HTML(lc_pval(ti$p.value[2]))), ". ",
        "W modelu prostym bogatsze okręgi mają wyższe wyniki czytania."
      )
    } else if (step == 2) {
      tl <- broom::tidy(lm(read ~ lunch, data = df))
      tagList(
        "β lunch = ", tags$b(round(tl$estimate[2], 3)), ", p = ",
        tags$b(HTML(lc_pval(tl$p.value[2]))), ". ",
        "Odsetek uczniów z dotacją do obiadu jest silnie ujemnie
         powiązany z wynikiem czytania."
      )
    } else {
      ti <- broom::tidy(lm(read ~ income, data = df))
      tm <- broom::tidy(lm(read ~ income + lunch + english, data = df))
      tagList(
        "β dochód: ", tags$b(round(ti$estimate[2], 3)), " w modelu prostym, ",
        tags$b(round(tm$estimate[tm$term == "income"], 3)),
        " po kontroli dotacji do obiadów i angielskiego jako drugiego języka."
      )
    }
  })

  # Tabela współczynników modelu z kontrolą (krok 3).
  output$ch3_control_table <- renderUI({
    if (ch3_control_step() < 3) return(NULL)
    tb <- broom::tidy(lm(read ~ income + lunch + english, data = .cas_data))[-1, ]
    out <- data.frame(
      term = unname(ch3_labels_pl[tb$term]),
      estimate = tb$estimate,
      se = tb$std.error,
      p = lc_pval(tb$p.value)
    )
    lc_table(out, cols = list(
      lc_col("term", "Zmienna", "row"),
      lc_col("estimate", "β", digits = 4),
      lc_col("se", "SE", digits = 4),
      lc_col("p", "p")
    ))
  })

  # Widget "Efekt dodawania zmiennych" został przeniesiony do ch4
  # (Jak porównywać modele) — tam pasuje merytorycznie.

  # --- Widget: współliniowość ---
  ch3_collin_data <- reactiveVal(NULL)

  observeEvent(input$ch3_collin_new, {
    ch3_collin_data(generate_collinearity_data(140, input$ch3_collin_rho))
  })

  zoom_plot_server("ch3_collin_plot", reactive({
    df <- ch3_collin_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Generuj i dopasuj”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      ggplot(df, aes(x = x1, y = x2)) +
        geom_point(color = upwr_secondary, alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["niebo"])) +
        labs(x = "X₁", y = "X₂") +
        theme_upwr()
    }
  }), alt = "Wykres punktowy dwóch coraz silniej współliniowych predyktorów.")

  output$ch3_collin_info <- renderUI({
    df <- ch3_collin_data()
    if (is.null(df)) return(NULL)
    model <- lm(y ~ x1 + x2, data = df)
    tagList(
      lc_readout("corr(X₁,X₂)", round(cor(df$x1, df$x2), 2), color = unname(upwr_cat["niebo"])),
      lc_readout("R² modelu", round(summary(model)$r.squared, 3), color = unname(upwr_cat["szalwia"]))
    )
  })

  output$ch3_collin_table <- renderUI({
    df <- ch3_collin_data()
    if (is.null(df)) return(NULL)
    model <- lm(y ~ x1 + x2, data = df)
    coefs <- as.data.frame(broom::tidy(model))[-1, ]
    vifs <- compute_vif_simple(df, c("x1", "x2"))
    coefs$p_txt <- lc_pval(coefs$p.value)
    coefs$vif <- unname(vifs[coefs$term])
    lc_table(coefs,
      cols = list(
        lc_col("term", "Zmienna", "row"),
        lc_col("estimate", "β", digits = 3),
        lc_col("std.error", "SE", digits = 3),
        lc_col("p_txt", "p"),
        lc_col("vif", "VIF", digits = 2)
      )
    )
  })
}
