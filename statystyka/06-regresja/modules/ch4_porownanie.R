# ============================================================================
# CHAPTER 4: Porównanie modeli
# ============================================================================

ch4_ui <- list(
  id    = "ch-porownanie",
  num   = "04",
  title = "Porównanie modeli",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 04 · Regresja",
      num    = "04",
      title  = "Lepsze dopasowanie to jeszcze nie lepszy model.",
      lead   = "Każda dodatkowa zmienna poprawia wynik na danych, które model już
                widział. Porównanie modeli polega na tym, żeby odróżnić poprawę
                prawdziwą od dopasowania do przypadku."
    ),

    lc_p("W rozdziale 02 ocenialiśmy jeden model: oglądaliśmy reszty, liczyliśmy
      R² i RMSE. W rozdziałach 03 i 03B modele zaczęły rosnąć: dochodziły kolejne
      predyktory, zmienne jakościowe i interakcje. Każde takie rozszerzenie to
      nowy kandydat, więc zwykle mamy do wyboru kilka modeli dla tej samej
      zmiennej zależnej. Ten rozdział pokazuje, czym je porównywać i dlaczego
      najprostsza odpowiedź, czyli wybór modelu o najwyższym R², prowadzi na
      manowce."),

    lc_h2("ch4-problem", "Dlaczego sam R² nie wystarczy"),

    lc_p("R² ma przy porównaniach zdradliwą własność: po dodaniu predyktora nigdy
      nie spada. Metoda najmniejszych kwadratów może nowemu predyktorowi nadać
      współczynnik zero i wtedy suma kwadratów reszt pozostaje taka jak
      wcześniej. Każdy inny współczynnik wybierze tylko wtedy, gdy zmniejsza
      tę sumę. \\(SS_{res}\\) nie rośnie, \\(SS_{tot}\\) się nie zmienia, więc
      R² może tylko wzrosnąć albo zostać w miejscu, nawet gdy nowa zmienna
      to czysty szum."),

    lc_p("Szum zawsze trochę koreluje z resztami w konkretnej próbie, a model
      tę przypadkową zgodność wykorzystuje. To ten sam mechanizm, który
      w wykładzie 04 opisywaliśmy przy ",
      gloss("porównania wielokrotne", "porównaniach wielokrotnych"), ": im więcej
      kandydatów sprawdzamy, tym większa szansa, że któryś pasuje do danych
      wyłącznie przez przypadek. Model zbudowany przez dokładanie zmiennych
      dopóty, dopóki rośnie R², dopasowuje się coraz lepiej do tej jednej
      próby, w tym do jej przypadkowych wahań. Takie zjawisko nazywamy ",
      gloss("przeuczenie", "przeuczeniem"), ". Do porównywania modeli
      o różnej liczbie parametrów potrzebujemy więc miar, które za każdy
      dodatkowy parametr pobierają opłatę."),

    lc_h2("ch4-efekt-dodawania", "Efekt dodawania zmiennych"),

    lc_p("Panel losuje dane 150 studentów: ocenę oraz cztery predyktory — godziny
      nauki, frekwencję, poziom stresu i liczbę godzin snu. Ocena powstaje
      z kombinacji wszystkich czterech zmiennych i losowego szumu. Panel
      dopasowuje pięć modeli, dokładając predyktory po kolei. W ostatnim kroku
      dokłada zmienną „szum”: liczbę losowaną niezależnie od oceny, która
      z oceną nie ma żadnego związku. Panel pokazuje R²
      oraz ", gloss("skorygowany R²", "R² skorygowany"), ", a w tabeli także ",
      gloss("AIC"), ", ", gloss("BIC"), " i RMSE. Te miary omawiamy
      w następnej sekcji."),

    figure_panel(
      label = "Ryc. 4.1", title = "Krok po kroku",
      full_width = TRUE,
      lc_toolbar(
        lc_action("ch4_stepwise", "Buduj modele krok po kroku", variant = "solid")
      ),
      lc_plot("ch4_step_plot", max_height = "300px"),
      uiOutput("ch4_step_table"),
      lc_caption("Modele z 1–4 predyktorami i model z dodanym szumem.")
    ),

    lc_p("W pierwszych czterech krokach R² rośnie, przeciętnie od około 0.41 dla
      samych godzin nauki do około 0.71 dla wszystkich czterech predyktorów.
      R² skorygowany idzie tuż pod nim, AIC i BIC maleją, a RMSE się zmniejsza.
      Wszystkie miary są tu zgodne, bo dane wygenerowano tak, że każda z czterech
      zmiennych naprawdę wpływa na ocenę i każdy dodatek wnosi prawdziwą
      informację."),

    lc_p("Piąty krok jest inny. Szum nie ma z oceną nic wspólnego, a R² mimo to
      rośnie, zwykle tylko w trzecim miejscu po kropce (mediana przyrostu około
      0.001). R² nie umie więc odróżnić prawdziwego predyktora od przypadkowego.
      Miary z karą za złożoność reagują inaczej: w symulacji 4000 losowań
      R² skorygowany wzrósł po dodaniu szumu w około 31% przypadków, AIC wskazał
      większy model w około 16%, a BIC tylko w około 3%. Dlatego przy porównaniach
      patrzymy na nie, a nie na R²."),

    lc_h2("ch4-metryki", "Metryki porównawcze"),

    lc_p("Najprostsza poprawka dotyczy samego R². R² skorygowany liczy
      dopasowanie z uwzględnieniem liczby predyktorów k:"),

    lc_formula_box(withMathJax(
      "$$R^2_{adj} = 1 - \\frac{(1 - R^2)(n - 1)}{n - k - 1}$$"
    )),

    lc_p("Czynnik \\((n-1)/(n-k-1)\\) rośnie z każdym predyktorem, więc nowa
      zmienna musi zmniejszyć \\(1 - R^2\\) na tyle, żeby to zrównoważyć.
      Jeśli wnosi mniej, R² skorygowany spada. Może nawet wyjść ujemny, gdy
      model nie wyjaśnia prawie nic."),

    lc_p("Drugą rodzinę tworzą kryteria informacyjne. Oba zaczynają od tego,
      jak dobrze model odtwarza dane: \\(\\hat{L}\\) oznacza wiarygodność,
      czyli prawdopodobieństwo (dokładniej: gęstość) zaobserwowanych danych
      przy najlepiej dopasowanych parametrach. Do tego dochodzi kara za liczbę
      parametrów. W regresji liniowej z k predyktorami szacujemy k + 1
      współczynników i wariancję reszt, razem k + 2 parametry:"),

    lc_formula_box(withMathJax(
      "$$\\text{AIC} = -2\\ln\\hat{L} + 2(k + 2), \\qquad
        \\text{BIC} = -2\\ln\\hat{L} + (k + 2)\\ln n$$",
      "$$-2\\ln\\hat{L} = n\\ln\\left(\\text{RMSE}^2\\right) + n\\left(1 + \\ln 2\\pi\\right)$$"
    )),

    lc_p("Drugi wiersz pokazuje, co kryje się pod wiarygodnością w regresji
      liniowej: to RMSE w skali logarytmicznej, pomnożone przez liczbę
      obserwacji. AIC i BIC są więc sumą tego samego składnika dopasowania
      i kary za złożoność. Mniejsza wartość oznacza lepszy kompromis.
      Kryteria różnią się tylko karą: AIC dolicza 2 za każdy parametr,
      BIC — \\(\\ln n\\). Już przy ośmiu obserwacjach \\(\\ln n\\) przekracza 2,
      a przy n = 150 wynosi około 5, więc BIC mocniej karze złożoność
      i częściej wybiera prostszy model."),

    lc_p("Pojedyncza wartość AIC czy BIC nic nie mówi. Liczba 250 nie jest ani
      dobra, ani zła, bo zależy od skali Y i liczby obserwacji. Sens mają tylko
      różnice między modelami dopasowanymi do tych samych obserwacji tej samej
      zmiennej zależnej. Nie ma też uniwersalnej granicy, od której różnica
      rozstrzyga sprawę. Mała różnica oznacza, że dane słabo odróżniają modele,
      i wtedy rozsądnie jest wybrać prostszy albo ten lepiej uzasadniony
      merytorycznie."),

    lc_p("Kary mają też interpretację w języku testów. Gdy do modelu dokładamy
      jedną zmienną bez żadnego związku z Y, przy n = 150 R² skorygowany
      wzrośnie w około 32% przypadków, AIC uzna większy model za lepszy
      w około 16%, a BIC w około 2.5%. To odpowiednik poziomu istotności:
      każda miara przepuszcza część fałszywych alarmów. Jeśli sprawdzimy
      dziesięć takich bezużytecznych kandydatów, każdego osobno, szansa,
      że przynajmniej jeden przejdzie przez AIC, wynosi około 80%. Problem
      porównań wielokrotnych wraca więc przy wyborze zmiennych, a kary go
      łagodzą, ale nie usuwają."),

    lc_h2("ch4-arena", "Arena modeli liniowych"),

    lc_p("Panel losuje dane z tego samego mechanizmu co poprzedni, ale pozwala
      zmienić liczbę obserwacji. Dla każdego z czterech modeli pokazuje
      R² skorygowany, AIC, BIC i RMSE, a w tabeli wyróżnia najlepszą wartość
      każdej miary."),

    figure_panel(
      label = "Ryc. 4.2", title = "Porównanie modeli regresji",
      full_width = TRUE,
      lc_toolbar(
        lc_slider("ch4_n", "n", 50, 300, 150, 25),
        lc_action("ch4_compare", "Buduj i porównaj modele", variant = "solid")
      ),
      lc_plot("ch4_metrics_plot", max_height = "350px"),
      uiOutput("ch4_metrics_table"),
      lc_caption("Generujemy dane i budujemy 4 modele z różną
                    liczbą predyktorów.")
    ),

    lc_p("Przy n = 150 wszystkie kryteria praktycznie zawsze wskazują pełny model.
      Przy n = 50 zaczynają się rozchodzić. AIC nadal wybiera model z czterema
      predyktorami w około 86% losowań, BIC tylko w około 69%, a w co czwartym
      losowaniu woli model bez snu. Słaby efekt snu jest prawdziwy, ale
      w małej próbie trudno go odróżnić od szumu, a kara BIC jest na tyle
      surowa, że często go odrzuca."),

    lc_p("Kolumna RMSE zawsze wyróżnia model czwarty. RMSE liczone na tych samych
      danych, na których model dopasowano, zachowuje się jak R²: po dodaniu
      predyktora nie rośnie, więc nie nadaje się do porównywania modeli
      o różnej złożoności. Uczciwe RMSE trzeba policzyć na nowych danych.
      Tym zajmiemy się za chwilę."),

    lc_p("Gdy AIC i BIC się nie zgadzają, nie oznacza to, że jedno z nich się
      myli. AIC szuka modelu, który najlepiej przewiduje nowe obserwacje,
      i godzi się na dodatkowy parametr, jeśli ten choć trochę poprawia
      predykcję. BIC szuka modelu prostego, który zawiera tylko wyraźne efekty.
      Do przewidywania częściej sięga się po AIC, do wyjaśniania, które
      zmienne naprawdę mają znaczenie, po BIC."),

    lc_h2("ch4-overfitting", "Przeuczenie: model uczy się szumu"),

    lc_p("Dotąd porównywaliśmy modele z jedną, dwiema, trzema i czterema
      zmiennymi. Ten sam problem pojawia się, gdy złożoność rośnie
      w inny sposób, na przykład gdy jedną zmienną X wprowadzamy do modelu
      jako wielomian coraz wyższego stopnia. Wielomian stopnia d ma d + 1
      współczynników i każdy kolejny stopień pozwala krzywej mocniej się zgiąć.
      Wielomiany kolejnych stopni to ",
      gloss("modele zagnieżdżone", "modele zagnieżdżone"), ": niższy powstaje
      z wyższego przez usunięcie najwyższej potęgi. AIC i BIC nie wymagają
      jednak zagnieżdżenia. Wystarczy, że modele opisują tę samą zmienną
      zależną na tych samych obserwacjach."),

    lc_p("W rozdziale 02 widzieliśmy trzy wielomiany: zbyt prosty, rozsądny
      i przeuczony. Panel poniżej pozwala przejść przez wszystkie stopnie
      od 1 do 15. Dane powstają jako fala sinusoidalna z szumem
      o odchyleniu standardowym 1, a pod wykresem widać R², R² skorygowany,
      AIC i BIC dla wybranego stopnia."),

    figure_panel(
      label = "Ryc. 4.3", title = "Wielomian: dopasowanie a uogólnianie",
      full_width = TRUE,
      lc_toolbar(
        lc_slider("ch4_poly_degree", "Stopień wielomianu", 1, 15, 1, 1),
        lc_slider("ch4_poly_n", "n (punktów)", 15, 100, 30, 5),
        lc_action("ch4_poly_gen", "Generuj", variant = "solid"),
        lc_readouts(uiOutput("ch4_poly_stats"))
      ),
      lc_plot("ch4_poly_plot", max_height = "300px")
    ),

    lc_p("Przy 30 punktach prosta (stopień 1) nie łapie fali, a R² jest bliski
      zera. Od stopnia 4 krzywa zaczyna podążać za danymi i R² skacze
      do około 0.78. Dalej R² rośnie powoli aż do około 0.91 przy stopniu 15,
      ale krzywa coraz bardziej faluje między punktami, a przy brzegach
      odlatuje. R² skorygowany zatrzymuje się w okolicach 0.80 od stopnia 6.
      BIC jest najmniejszy zwykle przy stopniach 4–6 i potem rośnie."),

    lc_p("AIC przy tak małej próbie zawodzi częściej. Jego kara jest łagodna,
      więc w mniej więcej co trzecim losowaniu najmniejszą wartość osiąga
      dla stopnia 12–15, czyli dla krzywej wyraźnie przeuczonej. Kryteria
      informacyjne same są oszacowaniami z tej samej próby i w małych
      próbach mogą się mylić. Dlatego potrzebny jest sprawdzian, który
      nie korzysta z danych użytych do dopasowania."),

    lc_h2("ch4-train-test", "Zbiór uczący i testowy"),

    lc_p("Najbardziej bezpośredni sprawdzian polega na rozdzieleniu danych.
      Model dopasowujemy na ",
      gloss("zbiór treningowy", "zbiorze uczącym"), " (inaczej treningowym),
      a jego błąd mierzymy na ",
      gloss("zbiór testowy", "zbiorze testowym"), ", czyli na obserwacjach,
      których przy dopasowaniu nie widział. RMSE na zbiorze testowym liczymy
      tym samym wzorem co w rozdziale 02, tylko z resztami
      \\(y_j - \\hat{y}_j\\) dla obserwacji testowych. Ta miara odpowiada
      wprost na pytanie, które naprawdę nas interesuje: jak bardzo model
      pomyli się na nowych danych."),

    lc_p("W panelu ciemne punkty to 35 obserwacji uczących, a jasne to 180
      obserwacji testowych z tego samego mechanizmu (fala z szumem
      o odchyleniu standardowym 1). Pod wykresem widać RMSE na obu zbiorach
      dla wybranego stopnia i stopień, który na zbiorze testowym wypada
      najlepiej."),

    figure_panel(
      label = "Ryc. 4.4", title = "Zbiór uczący i testowy: kiedy model przestaje uogólniać",
      full_width = TRUE,
      lc_toolbar(
        lc_slider("ch4_tt_degree", "Stopień wielomianu", 1, 15, 1, 1),
        lc_action("ch4_tt_new", "Nowy podział danych", variant = "solid"),
        lc_readouts(uiOutput("ch4_tt_info"))
      ),
      lc_plot("ch4_tt_plot", max_height = "330px"),
      uiOutput("ch4_tt_note")
    ),

    lc_p("Błąd na zbiorze uczącym maleje z każdym stopniem: typowo od około 2.2
      przy prostej do około 0.7 przy stopniu 15. Błąd testowy najpierw też
      spada, osiąga minimum w okolicach stopnia 6 (typowo około 1.1), a potem
      rośnie. Przy stopniu 15 w mniej więcej trzech podziałach na cztery
      jest większy niż dla zwykłej prostej, często kilkukrotnie. Żaden model
      nie zejdzie na zbiorze testowym wyraźnie poniżej 1, bo tyle wynosi
      odchylenie standardowe szumu, którego nie da się przewidzieć. RMSE
      uczące wyraźnie poniżej tej wartości to znak, że model dopasował się
      także do szumu."),

    lc_p("Rozjazd obu krzywych to rozpoznawalny objaw przeuczenia: model
      coraz lepiej pamięta punkty, które widział, i coraz gorzej radzi sobie
      z nowymi. Przy kolejnych podziałach najlepszy stopień się zmienia,
      zwykle mieści się między 4 a 9. Sam wynik testu też jest więc
      oszacowaniem z próby, tym pewniejszym, im większy zbiór testowy.
      Zbioru testowego nie wolno też używać wielokrotnie do dobierania
      modelu. Gdy sprawdzimy na nim kilkadziesiąt wariantów i wybierzemy
      najlepszy, wracamy do problemu porównań wielokrotnych, a zbiór
      testowy przestaje być dla modelu nowy."),

    inline_callout(
      label = "Zasada",
      "Model ocenia się po tym, jak przewiduje dane, których nie widział, i po
       tym, czy ma sens merytoryczny. R² na danych użytych do dopasowania
       przy dodawaniu zmiennych tylko rośnie."
    ),

    lc_h2("ch4-co-dalej", "Co dalej"),

    lc_p("Mamy komplet narzędzi do porównywania modeli: R² skorygowany, AIC
      i BIC, które karzą za liczbę parametrów, oraz błąd na zbiorze testowym,
      który mierzy przewidywanie wprost. Wszystkie modele w tym rozdziale
      miały jednak ciągłą zmienną zależną."),

    lc_p("Często Y przyjmuje tylko dwie wartości: student zdał albo nie zdał,
      klient kupił albo nie kupił. Regresja liniowa daje wtedy przewidywania
      spoza przedziału [0, 1], których nie da się czytać jako
      prawdopodobieństw. Następny rozdział wprowadza regresję logistyczną,
      zbudowaną dla takich zmiennych. AIC i BIC przydadzą się w niej
      bez zmian."),

    lc_chapter_next(
      num       = "05",
      title     = "Regresja logistyczna",
      lead      = "gdy zmienna zależna jest binarna",
      target_id = "ch-logistyczna"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  # --- Widget: Efekt dodawania zmiennych (przeniesiony z ch3 wielorakiej) ---
  ch4_step_data <- reactiveVal(NULL)

  observeEvent(input$ch4_stepwise, {
    df <- generate_multi_data(150)
    df$szum <- rnorm(nrow(df))  # zmienna bez związku z oceną

    pred_sets <- list(
      c("godziny_nauki"),
      c("godziny_nauki", "frekwencja"),
      c("godziny_nauki", "frekwencja", "stres"),
      c("godziny_nauki", "frekwencja", "stres", "sen_h"),
      c("godziny_nauki", "frekwencja", "stres", "sen_h", "szum")
    )

    results <- lapply(seq_along(pred_sets), function(i) {
      formula <- as.formula(paste("ocena ~", paste(pred_sets[[i]], collapse = " + ")))
      model <- lm(formula, data = df)
      metrics <- compute_model_metrics(model)
      data.frame(
        k = i,
        predictors = paste(pred_sets[[i]], collapse = " + "),
        r_squared = metrics$r_squared,
        adj_r_squared = metrics$adj_r_squared,
        aic = metrics$aic,
        bic = metrics$bic,
        rmse = metrics$rmse
      )
    })

    ch4_step_data(do.call(rbind, results))
  })

  zoom_plot_server("ch4_step_plot", reactive({
    df <- ch4_step_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Buduj modele krok po kroku”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      long <- df %>%
        select(k, r_squared, adj_r_squared) %>%
        tidyr::pivot_longer(cols = c(r_squared, adj_r_squared),
                            names_to = "metric", values_to = "value") %>%
        mutate(metric = ifelse(metric == "r_squared", "R²", "R² skorygowany"))

      ggplot(long, aes(x = k, y = value, color = metric)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 3) +
        scale_x_continuous(breaks = 1:5,
                           labels = c(paste0(1:4, " pred."), "+ szum")) +
        scale_color_manual(values = c(unname(upwr_cat["niebo"]), unname(upwr_cat["szalwia"])), name = NULL) +
        labs(
             x = "Liczba predyktorów", y = "Wartość") +
        theme_upwr() +
        theme(legend.position = "top")
    }
  }))

  output$ch4_step_table <- renderUI({
    df <- ch4_step_data()
    if (is.null(df)) return(NULL)


    lc_table(df,
      cols = list(
        lc_col("predictors", "Predyktory", "row"),
        lc_col("r_squared", "R²", digits = 3),
        lc_col("adj_r_squared", "R² skor.", digits = 3),
        lc_col("aic", "AIC", digits = 1),
        lc_col("bic", "BIC", digits = 1),
        lc_col("rmse", "RMSE", digits = 3)
      )
    )
  })

  # --- Widget: Arena modeli liniowych ---
  ch4_models <- reactiveVal(NULL)

  observeEvent(input$ch4_compare, {
    df <- generate_multi_data(input$ch4_n)

    models <- list(
      "1: nauka" = lm(ocena ~ godziny_nauki, data = df),
      "2: nauka + frekw." = lm(ocena ~ godziny_nauki + frekwencja, data = df),
      "3: nauka + frekw. + stres" = lm(ocena ~ godziny_nauki + frekwencja + stres, data = df),
      "4: wszystkie" = lm(ocena ~ godziny_nauki + frekwencja + stres + sen_h, data = df)
    )

    results <- lapply(names(models), function(name) {
      m <- compute_model_metrics(models[[name]])
      data.frame(
        model = name,
        r_squared = m$r_squared,
        adj_r_squared = m$adj_r_squared,
        aic = m$aic,
        bic = m$bic,
        rmse = m$rmse,
        n_params = m$n_params
      )
    })

    ch4_models(do.call(rbind, results))
  })

  zoom_plot_server("ch4_metrics_plot", reactive({
    df <- ch4_models()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Buduj i porównaj modele”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      long <- df %>%
        select(model, adj_r_squared, aic, bic, rmse) %>%
        tidyr::pivot_longer(-model, names_to = "metric", values_to = "value") %>%
        mutate(metric = factor(metric,
          levels = c("adj_r_squared", "aic", "bic", "rmse"),
          labels = c("R² skorygowany", "AIC", "BIC", "RMSE")))

      ggplot(long, aes(x = model, y = value, fill = model)) +
        geom_col(alpha = 0.8) +
        facet_wrap(~metric, scales = "free_y", ncol = 4) +
        scale_fill_manual(values = c(unname(upwr_cat["niebo"]), unname(upwr_cat["szalwia"]), unname(upwr_cat["bursztyn"]), unname(upwr_cat["wrzos"]))) +
        labs(x = NULL, y = "Wartość") +
        theme_upwr() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
    }
  }))

  output$ch4_metrics_table <- renderUI({
    df <- ch4_models()
    if (is.null(df)) return(NULL)

    best_adj_r2 <- which.max(df$adj_r_squared)
    best_aic <- which.min(df$aic)
    best_bic <- which.min(df$bic)
    best_rmse <- which.min(df$rmse)


    best <- function(i) ifelse(seq_len(nrow(df)) == i, "is-best", NA)
    lc_table(df,
      cols = list(
        lc_col("model", "Model", "row"),
        lc_col("r_squared", "R²", digits = 3),
        lc_col("adj_r_squared", "R² skor.", digits = 3),
        lc_col("aic", "AIC", digits = 1),
        lc_col("bic", "BIC", digits = 1),
        lc_col("rmse", "RMSE", digits = 3)
      ),
      cell_class = list(adj_r_squared = best(best_adj_r2), aic = best(best_aic),
                        bic = best(best_bic), rmse = best(best_rmse))
    )
  })

  # --- Widget: Przeuczenie (wielomian) ---
  ch4_poly_data <- reactiveVal(NULL)

  observeEvent(input$ch4_poly_gen, {
    n <- input$ch4_poly_n
    x <- sort(runif(n, 0, 10))
    y <- sin(x) * 3 + rnorm(n, 0, 1)
    ch4_poly_data(data.frame(x = x, y = y))
  })

  zoom_plot_server("ch4_poly_plot", reactive({
    df <- ch4_poly_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Generuj”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      degree <- input$ch4_poly_degree
      model <- lm(y ~ poly(x, degree), data = df)

      x_pred <- seq(min(df$x), max(df$x), length.out = 200)
      y_pred <- predict(model, newdata = data.frame(x = x_pred))

      pred_df <- data.frame(x = x_pred, y = y_pred)

      ggplot() +
        geom_point(data = df, aes(x = x, y = y), color = upwr_secondary, alpha = 0.5) +
        geom_line(data = pred_df, aes(x = x, y = y), color = unname(upwr_cat["niebo"]), linewidth = 1.2) +
        labs(
             x = "X", y = "Y") +
        theme_upwr()
    }
  }))

  output$ch4_poly_stats <- renderUI({
    df <- ch4_poly_data()
    if (is.null(df)) return(NULL)
    degree <- input$ch4_poly_degree
    model <- lm(y ~ poly(x, degree), data = df)
    metrics <- compute_model_metrics(model)

    tagList(
      lc_readout("R²", round(metrics$r_squared, 3), color = unname(upwr_cat["niebo"])),
      lc_readout("R² skor.", round(metrics$adj_r_squared, 3), color = unname(upwr_cat["szalwia"])),
      lc_readout("AIC", round(metrics$aic, 1), color = unname(upwr_cat["bursztyn"])),
      lc_readout("BIC", round(metrics$bic, 1), color = upwr_secondary)
    )
  })

  # --- Widget: zbiór uczący / testowy ---
  ch4_tt_data <- reactiveVal(generate_train_test_poly())

  observeEvent(input$ch4_tt_new, {
    ch4_tt_data(generate_train_test_poly())
  })

  zoom_plot_server("ch4_tt_plot", reactive({
    sets <- ch4_tt_data()
    train <- sets$train
    test <- sets$test
    degree <- input$ch4_tt_degree
    model <- lm(y ~ poly(x, degree), data = train)
    grid <- data.frame(x = seq(0, 10, length.out = 300))
    grid$y <- predict(model, newdata = grid)

    ggplot() +
      geom_point(data = test, aes(x = x, y = y), color = unname(upwr_cat["bursztyn"]),
                 alpha = 0.25, size = 1.8) +
      geom_point(data = train, aes(x = x, y = y), color = upwr_secondary,
                 alpha = 0.75, size = 2.2) +
      geom_line(data = grid, aes(x = x, y = y), color = unname(upwr_cat["niebo"]),
                linewidth = 1.2) +
      labs(x = "X", y = "Y") +
      theme_upwr()
  }))

  ch4_tt_state <- reactive({
    sets <- ch4_tt_data()
    train <- sets$train
    test <- sets$test
    degree <- input$ch4_tt_degree

    degrees <- 1:15
    metrics <- lapply(degrees, function(d) {
      model <- lm(y ~ poly(x, d), data = train)
      train_rmse <- sqrt(mean((train$y - predict(model, train))^2))
      test_rmse <- sqrt(mean((test$y - predict(model, test))^2))
      data.frame(degree = d, train_rmse = train_rmse, test_rmse = test_rmse)
    })
    metrics <- do.call(rbind, metrics)
    current <- metrics[metrics$degree == degree, ]
    best <- metrics[which.min(metrics$test_rmse), ]

    list(current = current, best = best)
  })

  output$ch4_tt_info <- renderUI({
    st <- ch4_tt_state()
    current <- st$current
    best <- st$best
    tagList(
      lc_readout("RMSE uczący", round(current$train_rmse, 2), color = unname(upwr_cat["niebo"])),
      lc_readout("RMSE testowy", round(current$test_rmse, 2), color = unname(upwr_cat["bursztyn"])),
      lc_readout("Najlepszy na teście", paste0("stopień ", best$degree, " · RMSE ", round(best$test_rmse, 2)), color = unname(upwr_cat["szalwia"]))
    )
  })

  output$ch4_tt_note <- renderUI({
    st <- ch4_tt_state()
    current <- st$current
    best <- st$best
    lc_caption(
        if (current$test_rmse > best$test_rmse * 1.25)
            "Błąd testowy wyraźnie wyższy niż przy najlepszym stopniu."
          else
            "Błąd testowy bliski najmniejszego."
      )
  })
}
