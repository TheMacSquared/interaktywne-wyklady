# ============================================================================
# CHAPTER 3B: KONTEKST, ZMIENNE JAKOŚCIOWE I INTERAKCJE
# ============================================================================

.ch3b_species_colors <- c(
  Adelie = unname(upwr_cat["szalwia"]),
  Chinstrap = unname(upwr_cat["bursztyn"]),
  Gentoo = unname(upwr_cat["terakota"])
)

.ch3b_term_labels <- c(
  "(Intercept)" = "Stała: Adelie",
  flipper_length_mm = "Długość płetwy",
  speciesChinstrap = "Gatunek: Chinstrap",
  speciesGentoo = "Gatunek: Gentoo",
  `flipper_length_mm:speciesChinstrap` = "Płetwa × Chinstrap",
  `flipper_length_mm:speciesGentoo` = "Płetwa × Gentoo"
)

ch3b_ui <- list(
  id = "ch-3b",
  num = "03B",
  title = "Kontekst i interakcje",
  duration = "30–45 min",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 03B · Regresja",
      num = "03B",
      title = "Jedna linia może ukrywać trzy różne historie.",
      lead = paste(
        "Pingwiny trzech gatunków różnią się budową ciała. Linia dopasowana",
        "do wszystkich naraz potrafi wskazać kierunek, którego nie ma",
        "w żadnym gatunku z osobna."
      )
    ),

    lc_p("W rozdziale 03 zobaczyliśmy, że współczynnik w modelu wielorakim
      opisuje związek przy stałych pozostałych zmiennych i że po dodaniu
      zmiennej kontrolnej może zmienić wartość, a nawet znak. Wszystkie
      tamte predyktory były liczbami. Ten rozdział dokłada predyktor, który
      liczbą nie jest: przynależność do grupy. Zamiast danych CASchools
      użyjemy danych o 333 pingwinach z Antarktydy (146 Adelie, 68 Chinstrap
      i 119 Gentoo). Trzy gatunki tworzą naturalne grupy, w których dobrze
      widać, co grupa robi z linią regresji."),

    lc_h2("ch3b-simpson", "Pominięta zmienna i paradoks Simpsona"),

    lc_p("W wykładzie 04 przy korelacji widzieliśmy ",
      gloss("paradoks Simpsona", "paradoks Simpsona"), " na danych uczniów
      z trzech szkół. W danych połączonych korelacja czasu nauki z wynikiem
      wynosiła -0.49, a w każdej szkole osobno była dodatnia. Odwrócenie
      brało się z różnic między szkołami. Regresja dziedziczy ten problem:
      jeśli w modelu brakuje zmiennej, która różnicuje grupy, nachylenie
      prostej miesza dwie rzeczy. Jedna to różnice między grupami, druga to
      związek wewnątrz grupy. Taką brakującą zmienną nazywamy ",
      gloss("zmienna pominięta", "zmienną pominiętą"), "."),

    lc_p("Panel pokazuje długość i wysokość dzioba pingwinów w trzech krokach:
      jedna prosta dla wszystkich, te same punkty z podziałem na gatunki
      i model, który uwzględnia gatunek."),

    figure_panel(
      label = "Ryc. 3B.1",
      title = "Paradoks Simpsona krok po kroku",
      full_width = TRUE,
      lc_toolbar(
        lc_action("ch3b_simpson_all", "1. Jedna linia dla wszystkich", variant = "outline"),
        lc_action("ch3b_simpson_groups", "2. Pokaż gatunki", variant = "outline"),
        lc_action("ch3b_simpson_control", "3. Kontroluj gatunek", variant = "solid")
      ),
      lc_plot("ch3b_simpson_plot", max_height = "430px"),
      uiOutput("ch3b_simpson_explanation"),
      uiOutput("ch3b_simpson_stats")
    ),

    lc_p("W danych połączonych prosta lekko opada: każdy dodatkowy milimetr
      długości dzioba wiąże się średnio z wysokością mniejszą o 0.08 mm,
      a korelacja wynosi -0.23. Po pokolorowaniu punktów widać trzy chmury
      i w każdej z nich związek jest dodatni, z korelacją 0.39 u Adelie
      i 0.65 u Chinstrap i Gentoo. Ujemna linia ogólna powstaje z ułożenia
      grup. Adelie mają dzioby krótkie i wysokie (średnio 38.8 mm na 18.3 mm),
      a Gentoo długie i niskie (47.6 mm na 15.0 mm). Prosta poprowadzona
      przez wszystkie punkty łączy głównie te dwa skupiska."),

    lc_p("Model z gatunkiem porównuje pingwiny tego samego gatunku. Nachylenie
      dla długości dzioba wynosi w nim 0.20 mm na milimetr, czyli ma przeciwny
      znak niż w modelu prostym. Oba ", gloss("współczynnik regresji",
      "współczynniki"), " są policzone poprawnie, ale odpowiadają na różne
      pytania. Pierwszy opisuje, jak zmienia się wysokość dzioba w całej
      mieszaninie gatunków. Drugi opisuje, czy pingwin o dłuższym dziobie ma
      wyższy dziób niż pingwin tego samego gatunku z krótszym. Zwykle
      interesuje nas to drugie pytanie. Większa próba tego nie naprawi: przy
      tysiącu pingwinów w tych samych proporcjach model bez gatunku dawałby
      tę samą ujemną linię, tylko z mniejszym błędem standardowym."),

    lc_h2("ch3b-kategoria", "Predyktor jakościowy w równaniu"),

    lc_p("Żeby kontrolować gatunek, trzeba go wpisać do równania, a gatunek
      jest ", gloss("zmienna jakościowa", "zmienną jakościową"), ": nie ma
      jednostki, w której Gentoo byłoby „o jeden” większe od Chinstrap.
      Przypisanie gatunkom liczb 1, 2 i 3 narzuciłoby kolejność i równe
      odstępy, których w danych nie ma. Zamiast tego jeden gatunek wybiera
      się jako kategorię odniesienia, a dla każdego z pozostałych tworzy
      się osobną ", gloss("zmienna wskaźnikowa", "zmienną wskaźnikową"),
      ": przyjmuje ona wartość 1 dla pingwinów tego gatunku i 0 dla
      wszystkich innych. Przy trzech gatunkach potrzebne są więc dwie takie
      zmienne. Tutaj kategorią odniesienia jest Adelie."),

    lc_formula_box(withMathJax(
      "$$\\text{masa} = \\beta_0 + \\beta_1\\,\\text{płetwa} + \\beta_2\\,\\text{Chinstrap} + \\beta_3\\,\\text{Gentoo} + \\varepsilon$$"
    )),

    lc_p("Dla pingwina Adelie obie zmienne wskaźnikowe są równe 0, więc
      zostaje prosta \\(\\beta_0 + \\beta_1\\,\\text{płetwa}\\). Dla Chinstrap
      dochodzi stała \\(\\beta_2\\), dla Gentoo stała \\(\\beta_3\\).
      Współczynnik \\(\\beta_1\\) jest wspólnym nachyleniem dla wszystkich
      gatunków, a \\(\\beta_2\\) i \\(\\beta_3\\) przesuwają prostą w górę
      lub w dół względem Adelie. Taki model nazywamy addytywnym: daje trzy
      równoległe proste. Predyktor o dwóch kategoriach pojawił się już
      w rozdziale 01 przy danych CASchools (okręgi KK-06 i KK-08). Wtedy
      wystarczała jedna zmienna 0/1, a jej współczynnik był różnicą średnich
      między kategoriami."),

    lc_p("W danych pingwinów model addytywny daje wspólne nachylenie 40.6 g
      na milimetr płetwy. Chinstrap jest przy tej samej długości płetwy
      lżejszy od Adelie średnio o 205 g, a Gentoo cięższy o 285 g. Surowe
      średnie mówią co innego: Gentoo waży średnio 5092 g, a Adelie 3706 g,
      czyli o prawie 1400 g więcej. Większość tej różnicy bierze się z tego,
      że Gentoo mają dłuższe płetwy (średnio 217 mm wobec 190 mm u Adelie).
      Współczynnik przy zmiennej wskaźnikowej, tak jak każdy współczynnik
      w modelu wielorakim, opisuje różnicę przy stałych pozostałych
      zmiennych."),

    lc_h2("ch3b-interakcja", "Czy nachylenie zależy od gatunku?"),

    lc_p("Model addytywny zakłada, że dodatkowy milimetr płetwy wiąże się
      z takim samym przyrostem masy u każdego gatunku. To założenie, a nie
      fakt. Jeśli u jednego gatunku masa rośnie z długością płetwy szybciej
      niż u innych, proste powinny mieć różne nachylenia. Takie zjawisko
      nazywamy ", gloss("interakcja", "interakcją"), ": związek jednego
      predyktora z Y zależy od wartości innego predyktora. W równaniu
      interakcję zapisuje się jako iloczyn długości płetwy i zmiennej
      wskaźnikowej."),

    lc_formula_box(withMathJax(
      "$$\\text{masa} = \\beta_0 + \\beta_1\\,\\text{płetwa} + \\beta_2\\,\\text{Chinstrap} + \\beta_3\\,\\text{Gentoo} + \\beta_4\\,\\text{płetwa}\\cdot\\text{Chinstrap} + \\beta_5\\,\\text{płetwa}\\cdot\\text{Gentoo} + \\varepsilon$$"
    )),

    lc_p("Dla Adelie oba iloczyny są równe 0 i nachylenie wynosi
      \\(\\beta_1\\). Dla Gentoo nachylenie wynosi \\(\\beta_1 + \\beta_5\\),
      więc \\(\\beta_5\\) mówi, o ile nachylenie u Gentoo różni się od
      nachylenia u Adelie. Analogicznie \\(\\beta_4\\) dla Chinstrap. Panel
      pozwala przełączać się między modelem addytywnym a modelem z interakcją
      na tych samych danych."),

    figure_panel(
      label = "Ryc. 3B.2",
      title = "Model addytywny kontra model z interakcją",
      full_width = TRUE,
      lc_toolbar(
        lc_segmented(
            "ch3b_interaction_model",
            "Model",
            choices = c(
              "Addytywny: płetwa + gatunek" = "add",
              "Z interakcją: płetwa × gatunek" = "interaction"
            ),
            selected = "add"
          ),
        lc_readouts(uiOutput("ch3b_interaction_metrics"))
      ),
      lc_plot("ch3b_interaction_plot", max_height = "400px"),
      uiOutput("ch3b_interaction_table")
    ),

    lc_p("Zacznij od wykresu, a dopiero potem czytaj tabelę. W modelu
      z interakcją proste dla Adelie i Chinstrap pozostają prawie
      równoległe: nachylenia wynoszą 32.7 i 34.6 g na milimetr, a składnik
      interakcji dla Chinstrap (1.9 g na mm) jest bardzo niepewny
      (p = 0.81). Prosta dla Gentoo jest wyraźnie bardziej stroma: 54.2 g
      na milimetr, o 21.5 g więcej niż u Adelie (p = 0.002). Wspólne
      nachylenie 40.6 g z modelu addytywnego było więc kompromisem między
      gatunkami, który żadnego z nich nie opisuje dokładnie."),

    lc_p("Po dodaniu interakcji zmienia się też znaczenie współczynników przy
      gatunkach. Współczynnik dla Gentoo (-4166 g) to różnica między Gentoo
      a Adelie przy długości płetwy 0 mm, czyli poza zakresem danych.
      Różnica między gatunkami zależy teraz od długości płetwy: przy 210 mm,
      gdzie oba gatunki mają obserwacje, wynosi około 344 g na korzyść
      Gentoo, a przy 220 mm około 559 g. W modelu z interakcją sensowniej
      jest czytać przewidywane proste niż pojedyncze współczynniki przy
      gatunkach."),

    lc_p("To samo pytanie można zadać w danych CASchools: czy związek dochodu
      okręgu z wynikami uczniów jest taki sam w okręgach o różnym kontekście
      społecznym. Pingwiny pokazują mechanizm wyraźniej, bo grupy są
      naturalne i dobrze rozdzielone. W danych obserwacyjnych o szkołach
      granice grup trzeba zwykle zdefiniować samemu, a interpretacja wymaga
      większej ostrożności."),

    lc_h2("ch3b-decyzja", "Jak zdecydować, czy dodać interakcję?"),

    lc_p("Interakcja kosztuje parametry: przy trzech gatunkach model z interakcją
      ma 6 współczynników zamiast 4. Dodawanie jej wszędzie, gdzie się da,
      prowadzi do modeli trudnych do odczytania i dopasowanych do przypadku.
      Decyzja ma kilka elementów."),

    lc_p("Pierwszym jest pytanie badawcze. Interakcja jest odpowiedzią na
      pytanie, czy związek X z Y może zależeć od grupy, i najlepiej, gdy to
      pytanie pada przed analizą, na podstawie wiedzy o zjawisku. U pingwinów
      jest to sensowne: gatunki różnią się budową ciała, więc ta sama długość
      płetwy może u nich oznaczać różną masę."),

    lc_p("Drugim jest wielkość różnicy. Na wykresie widać, czy nachylenia różnią
      się na tyle, by miało to znaczenie praktyczne. Płetwy Gentoo mają od 203
      do 231 mm, więc różnica nachyleń o 21.5 g na milimetr daje na tym
      zakresie około 600 g różnicy w przyroście przewidywanej masy. To więcej
      niż typowy błąd predykcji modelu, który wynosi około 365 g."),

    lc_p("Trzecim jest dopasowanie z uwzględnieniem złożoności. Model z interakcją
      zawsze dopasuje się co najmniej tak dobrze jak addytywny, bo ma więcej
      parametrów. Tutaj ", gloss("RMSE"), " spada z 371.0 do 365.1 g, a R²
      rośnie z 0.787 do 0.794. Uczciwsze porównanie daje kryterium ",
      gloss("AIC"), ", które karze za dodatkowe parametry: spada z 4895.3
      do 4888.6, a przy porównaniu modeli na tych samych danych niższa wartość
      jest lepsza. Test F porównujący oba modele daje F = 5.35 i p = 0.005.
      AIC i test F wskazują na model z interakcją, ale poprawa dopasowania
      jest niewielka. Za interakcją przemawia przede wszystkim to, że jedna z prostych
      ma wyraźnie inne nachylenie. Kryteria takie jak AIC omawia szerzej
      rozdział 04."),

    lc_p("Czwarty element dotyczy budowy modelu. Jeśli w modelu jest interakcja
      płetwy z gatunkiem, zostają w nim także oba składniki osobno: długość
      płetwy i zmienne wskaźnikowe gatunku. Bez zmiennych wskaźnikowych
      wszystkie proste musiałyby przecinać się w jednym punkcie przy długości
      płetwy 0 mm, a to zniekształca oszacowanie różnicy nachyleń."),

    lc_note("Zasada", rule = TRUE,
      "Interakcję dodawaj wtedy, gdy masz powód sądzić, że związek zależy od
       grupy, i oceniaj ją na wykresie przewidywanych prostych, a nie tylko
       po p-wartości. Przy interakcji zachowaj w modelu oba składniki osobno."
    ),

    lc_chapter_next(
      num = "04",
      title = "Porównywanie modeli",
      lead = "Różnica w dopasowaniu musi uzasadnić dodatkową złożoność.",
      target_id = "ch-porownanie"
    )
  )
)

ch3b_server <- function(input, output, session) {
  penguins <- .penguins_data
  simpson_step <- reactiveVal("all")

  observeEvent(input$ch3b_simpson_all, simpson_step("all"))
  observeEvent(input$ch3b_simpson_groups, simpson_step("groups"))
  observeEvent(input$ch3b_simpson_control, simpson_step("control"))

  simple_model <- lm(bill_depth_mm ~ bill_length_mm, data = penguins)
  controlled_model <- lm(bill_depth_mm ~ bill_length_mm + species, data = penguins)

  zoom_plot_server("ch3b_simpson_plot", reactive({
    step <- simpson_step()
    plot <- ggplot(penguins, aes(bill_length_mm, bill_depth_mm))

    if (identical(step, "all")) {
      plot <- plot +
        geom_point(color = upwr_secondary, alpha = 0.55) +
        geom_smooth(method = "lm", se = FALSE, color = upwr_accent, linewidth = 1.2)
    } else {
      plot <- plot +
        geom_point(aes(color = species, shape = species), alpha = 0.62) +
        geom_smooth(aes(color = species), method = "lm", se = FALSE, linewidth = 1.05) +
        scale_color_manual(values = .ch3b_species_colors, name = "Gatunek") +
        labs(shape = "Gatunek")
    }

    plot +
      labs(
        x = "Długość dzioba (mm)",
        y = "Wysokość dzioba (mm)"
      ) +
      theme_upwr()
  }), alt = paste(
    "Punkty długości i wysokości dzioba pingwinów. Po pokazaniu gatunków",
    "widoczne są trzy grupy i odmienne linie regresji."
  ))

  output$ch3b_simpson_stats <- renderUI({
    b_simple <- coef(simple_model)[["bill_length_mm"]]
    b_control <- coef(controlled_model)[["bill_length_mm"]]
    tagList(
      lc_readout("Nachylenie bez gatunku", round(b_simple, 3)),
      lc_readout("Nachylenie po kontroli gatunku", round(b_control, 3), color = upwr_accent)
    )
  })

  output$ch3b_simpson_explanation <- renderUI({
    step <- simpson_step()
    if (identical(step, "all")) {
      return(lc_caption(
               "Jedna prosta dla 333 pingwinów. Model nie wie, że punkty pochodzą z trzech gatunków."
             ))
    }
    if (identical(step, "groups")) {
      return(lc_caption(
               "Kolor ujawnia trzy skupiska. W każdym gatunku prosta rośnie.",
               tone = "info"
             ))
    }
    lc_caption(
      "Model z gatunkiem: nachylenie zmienia znak z ujemnego na dodatni.",
      tone = "ok"
    )
  })

  interaction_model <- reactive({
    if (identical(input$ch3b_interaction_model, "interaction")) {
      lm(body_mass_g ~ flipper_length_mm * species, data = penguins)
    } else {
      lm(body_mass_g ~ flipper_length_mm + species, data = penguins)
    }
  })

  prediction_grid <- reactive({
    grid <- expand.grid(
      flipper_length_mm = seq(
        min(penguins$flipper_length_mm),
        max(penguins$flipper_length_mm),
        length.out = 160
      ),
      species = levels(penguins$species)
    )
    grid$species <- factor(grid$species, levels = levels(penguins$species))
    grid$prediction <- predict(interaction_model(), newdata = grid)
    grid
  })

  zoom_plot_server("ch3b_interaction_plot", reactive({
    ggplot(penguins, aes(flipper_length_mm, body_mass_g, color = species, shape = species)) +
      geom_point(alpha = 0.42) +
      geom_line(
        data = prediction_grid(),
        aes(
          x = flipper_length_mm, y = prediction,
          color = species, group = species
        ),
        linewidth = 1.2,
        inherit.aes = FALSE
      ) +
      scale_color_manual(values = .ch3b_species_colors, name = "Gatunek") +
      labs(
        x = "Długość płetwy (mm)",
        y = "Masa ciała (g)",
        shape = "Gatunek"
      ) +
      theme_upwr()
  }), alt = paste(
    "Masa ciała względem długości płetwy dla trzech gatunków pingwinów.",
    "Model addytywny pokazuje równoległe linie, a model interakcyjny różne nachylenia."
  ))

  output$ch3b_interaction_metrics <- renderUI({
    model <- interaction_model()
    rmse <- sqrt(mean(residuals(model)^2))
    tagList(
      lc_readout("AIC", round(AIC(model), 1), color = unname(upwr_cat["wrzos"])),
      lc_readout("RMSE", round(rmse, 1), color = upwr_secondary),
      lc_readout("Parametry", length(coef(model)), color = unname(upwr_cat["bursztyn"]))
    )
  })

  output$ch3b_interaction_table <- renderUI({
    table <- broom::tidy(interaction_model())
    table$label <- vapply(table$term, function(term) {
      if (term %in% names(.ch3b_term_labels)) {
        unname(.ch3b_term_labels[[term]])
      } else {
        term
      }
    }, character(1))

    lc_table(
      data.frame(
        label = table$label,
        estimate = table$estimate,
        se = table$std.error,
        p = lc_pval(table$p.value)
      ),
      cols = list(
        lc_col("label", "Składnik", "row"),
        lc_col("estimate", "Współczynnik", digits = 3, short = "b",
               desc = "współczynnik"),
        lc_col("se", "Błąd stand.", digits = 3, short = "SE",
               desc = "błąd standardowy"),
        lc_col("p", "p-value")
      )
    )
  })
}
