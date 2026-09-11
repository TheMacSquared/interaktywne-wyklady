# ============================================================================
# ROZDZIAŁ 2: Dopasowanie KMNK
# ============================================================================

ch2_ui <- lecture_chapter(
  id = "ch-dopasowanie",
  num = "02",
  title = "Dopasowanie KMNK",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · KMNK",
      num = "02",
      title = "Dopasowanie metodą najmniejszych kwadratów.",
      lead = "Mając dane, jak wybrać konkretną prostą? KMNK mówi: tak, żeby suma kwadratów odchyleń od prostej była najmniejsza. Brzmi technicznie — ale intuicja jest prosta i można ją zobaczyć krok po kroku."
    ),

    lc_h2("ch2-idea", "Idea metody"),
    lc_p("Wyobraź sobie wykres rozrzutu: na osi poziomej X (np. metraż sklepu), na pionowej Y (miesięczna sprzedaż). Punkty układają się w chmurę — z grubsza wznoszącą się, ale nieidealną. Możemy przez tę chmurę narysować dowolnie wiele prostych. Która jest ‚najlepsza‘?"),
    lc_p("Klasyczna metoda najmniejszych kwadratów (KMNK, ang. OLS) odpowiada konkretnie: dla każdej kandydującej prostej liczymy ", strong("reszty"), " — pionowe odległości od punktów do prostej. Te reszty podnosimy do kwadratu i sumujemy. Wybieramy tę prostą, dla której ta suma jest najmniejsza. Stąd nazwa."),
    lc_formula_box(
      withMathJax(helpText("$$\\min_{b_0,\\, b_1} \\sum_{i=1}^{n} \\bigl(Y_i - b_0 - b_1 X_i\\bigr)^2$$")),
      p("Pod znakiem sumy: ", withMathJax("\\(Y_i - b_0 - b_1 X_i\\)"), " to reszta dla obserwacji ", withMathJax("\\(i\\)"), " — różnica między rzeczywistą wartością Y a tym, co przewiduje prosta."),
      p("Litery ", withMathJax("\\(b_0, b_1\\)"), " (zamiast ", withMathJax("\\(\\beta_0, \\beta_1\\)"), ") oznaczają ", strong("oszacowania"), " parametrów liczone z konkretnej próby. Prawdziwe ", withMathJax("\\(\\beta\\)"), " są nieznane — szukamy ich.")
    ),

    # ------------------------------------------------------------------------
    # Główny widget: KMNK krok-po-kroku (4 stany + reset)
    # ------------------------------------------------------------------------
    lc_h2("ch2-widget-kroki", "KMNK krok po kroku — spróbuj sam"),
    lc_p("Teraz najlepsza część: zamiast pokazać ci od razu gotowy wynik, przejdziemy przez metodę kawałkiem. Klikaj kolejne przyciski i obserwuj, co się dzieje na wykresie i w komentarzu."),
    lc_p("Scenariusz: 50 sklepów sieci handlowej. X to metraż (m²), Y to miesięczna sprzedaż (tys. zł)."),
    figure_panel(
      label = "Ryc. 2.1",
      title = "KMNK krok po kroku",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          tags$strong("Sterowanie próbą"),
          sliderInput("ch2_n", "Liczba sklepów", min = 20, max = 120, value = 50, step = 10),
          sliderInput("ch2_sigma", "Szum w danych σ", min = 5, max = 50, value = 25, step = 5),
          actionButton("ch2_resample", "Wylosuj nową próbę", class = "btn-outline-secondary", width = "100%"),
          tags$hr(),
          tags$strong("Kroki KMNK"),
          actionButton("ch2_step1", "Krok 1: zobacz dane", class = "btn-primary", width = "100%"),
          tags$br(), tags$br(),
          actionButton("ch2_step2", "Krok 2: narysuj swoją prostą", width = "100%"),
          tags$br(), tags$br(),
          actionButton("ch2_step3", "Krok 3: porównaj z KMNK", width = "100%"),
          tags$br(), tags$br(),
          actionButton("ch2_step4", "Krok 4: wynik liczbowy", width = "100%"),
          tags$hr(),
          conditionalPanel(
            condition = "input.ch2_step2 > 0 || input.ch2_step3 > 0",
            tags$strong("Twoja prosta (krok 2 i 3)"),
            sliderInput("ch2_user_b0", "Twój b₀ (wyraz wolny)",
                        min = -50, max = 200, value = 50, step = 5),
            sliderInput("ch2_user_b1", "Twój b₁ (nachylenie)",
                        min = -1, max = 4, value = 1.0, step = 0.1)
          )
        ),
        column(
          8,
          plotOutput("ch2_kmnk_plot", height = "440px"),
          uiOutput("ch2_kmnk_status"),
          uiOutput("ch2_kmnk_verdict")
        )
      )
    ),
    inline_callout(
      label = "Wskazówka",
      color = "ok",
      "Spróbuj w kroku 2 ustawić swoją prostą tak, żeby SSR (suma kwadratów reszt) było jak najmniejsze — bez patrzenia na rozwiązanie KMNK. W kroku 3 zobaczysz, jak blisko byłeś. To buduje intuicję, że ‚dopasować prostą‘ to nie sztuka — to konkretne zadanie optymalizacyjne."
    ),

    # ------------------------------------------------------------------------
    # Drugi widget: „Dlaczego kwadraty?” — porównanie 3 metod
    # ------------------------------------------------------------------------
    lc_h2("ch2-dlaczego-kwadraty", "Dlaczego kwadraty, a nie inna funkcja straty?"),
    lc_p("KMNK to nie jedyny sposób dopasowania prostej. Można na przykład minimalizować sumę wartości bezwzględnych reszt (LAD — Least Absolute Deviations) albo największą resztę (minimax). Każda z tych metod daje nieco inną prostą. Poniżej porównanie."),
    figure_panel(
      label = "Ryc. 2.2",
      title = "Trzy metody dopasowania na tej samej próbie",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          radioButtons("ch2_metoda", "Metoda dopasowania:",
                       choices = c(
                         "KMNK (kwadraty reszt)" = "kmnk",
                         "LAD (wartości bezwzględne)" = "lad",
                         "Minimax (najgorszy punkt)" = "minimax"
                       ),
                       selected = "kmnk"),
          checkboxInput("ch2_outlier", "Dodaj jeden punkt odstający", value = FALSE),
          tags$br(),
          uiOutput("ch2_metoda_opis")
        ),
        column(
          8,
          plotOutput("ch2_metoda_plot", height = "360px"),
          uiOutput("ch2_metoda_verdict")
        )
      )
    ),
    tags$ol(
      tags$li(strong("Kara rośnie szybciej dla dużych pomyłek (KMNK)."), " Dwie reszty po 5 dają w sumie kwadratów 50; jedna reszta 10 daje 100. KMNK woli rozkładać błąd równomiernie. LAD traktuje wszystkie reszty równo (suma |reszt|)."),
      tags$li(strong("Jednoznaczne rozwiązanie matematyczne (KMNK)."), " Suma kwadratów jest funkcją gładką — pochodne się ładnie zerują, dostajemy wzory zamknięte na b₀ i b₁. LAD i minimax wymagają numerycznych algorytmów."),
      tags$li(strong("Kompatybilność z normalnością (KMNK)."), " Przy normalnym składniku losowym KMNK pokrywa się z metodą największej wiarygodności. To nie przypadek — kwadrat odpowiada logarytmowi gęstości normalnej.")
    ),

    inline_callout(
      label = "Pułapka",
      color = "uwaga",
      "Druga strona medalu: KMNK jest wrażliwy na obserwacje odstające. Jeden punkt daleko od chmury — np. miesiąc z nietypową promocją — może wyraźnie przekrzywić prostą, bo jego reszta podniesiona do kwadratu dominuje w sumie. Zaznacz checkbox ‚Dodaj punkt odstający‘ i zobacz, jak każda z trzech metod reaguje."
    ),

    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      "R² mierzy, jaką część zmienności Y wyjaśnia model. R² = 0 oznacza, że X w ogóle nie pomaga; R² = 1 — że model trafia idealnie. Wartości pomiędzy interpretujemy procentowo: R² = 0,72 to ‚72% zmienności sprzedaży tłumaczą wydatki na reklamę‘."
    ),

    lc_chapter_next(
      num = "03",
      title = "Estymatory parametrów",
      lead = "skąd dokładnie biorą się b₀ i b₁",
      target_id = "ch-estymatory"
    )
  )
)

ch2_server <- function(input, output, session) {

  # --- Stan widgetu „krok po kroku” ----------------------------------------

  ch2_step <- reactiveVal(0L)
  ch2_seed <- reactiveVal(2024L)

  observeEvent(input$ch2_step1, ch2_step(1L))
  observeEvent(input$ch2_step2, ch2_step(2L))
  observeEvent(input$ch2_step3, ch2_step(3L))
  observeEvent(input$ch2_step4, ch2_step(4L))

  observeEvent(input$ch2_resample, {
    ch2_seed(sample.int(1e6, 1))
    ch2_step(1L)
  })

  # Reset stepu jeśli zmieniono parametry próby (pokaż znów dane).
  observeEvent(list(input$ch2_n, input$ch2_sigma), {
    if (ch2_step() > 0) ch2_step(1L)
  }, ignoreInit = TRUE)

  ch2_data <- reactive({
    eco02_kmnk_data(n = input$ch2_n, beta0 = 20, beta1 = 1.5,
                    sigma = input$ch2_sigma, seed = ch2_seed())
  })

  ch2_fit <- reactive(lm(y ~ x, data = ch2_data()))

  output$ch2_kmnk_plot <- renderPlot({
    step <- ch2_step()
    df <- ch2_data()

    if (step == 0) {
      return(
        ggplot() +
          annotate("text", x = 0, y = 0,
                   label = "Kliknij ‚Krok 1: zobacz dane‘, żeby zacząć.",
                   size = 5, color = unname(upwr_cat["grafit"])) +
          theme_void()
      )
    }

    p <- ggplot(df, aes(x, y)) +
      geom_point(color = unname(upwr_cat["grafit"]), alpha = 0.7, size = 2.4) +
      coord_cartesian(xlim = c(20, 210), ylim = c(0, 400)) +
      labs(x = "X — metraż sklepu (m²)", y = "Y — miesięczna sprzedaż (tys. zł)") +
      theme_upwr()

    if (step >= 2) {
      b0 <- input$ch2_user_b0
      b1 <- input$ch2_user_b1
      df_user <- df
      df_user$pred_user <- b0 + b1 * df_user$x
      p <- p +
        geom_segment(data = df_user,
                     aes(xend = x, yend = pred_user),
                     color = unname(upwr_cat["terakota"]), alpha = 0.45) +
        geom_abline(intercept = b0, slope = b1,
                    color = unname(upwr_cat["niebo"]),
                    linewidth = 1.3)
    }

    if (step >= 3) {
      coefs <- coef(ch2_fit())
      p <- p +
        geom_abline(intercept = coefs[1], slope = coefs[2],
                    color = upwr_accent, linewidth = 1.4, linetype = "solid")
    }

    p
  })

  output$ch2_kmnk_status <- renderUI({
    step <- ch2_step()
    if (step == 0) return(NULL)
    df <- ch2_data()
    n <- nrow(df)

    if (step == 1) {
      lc_stat_grid(
        lc_stat_box(label = "Liczba sklepów (n)", value = n,
                    caption = "ile obserwacji w próbie",
                    color = upwr_secondary),
        lc_stat_box(label = "Zakres X", value = paste0(round(min(df$x)), "–", round(max(df$x)), " m²"),
                    caption = "metraż w próbie",
                    color = unname(upwr_cat["szalwia"])),
        lc_stat_box(label = "Zakres Y", value = paste0(round(min(df$y)), "–", round(max(df$y)), " tys. zł"),
                    caption = "sprzedaż w próbie",
                    color = unname(upwr_cat["niebo"])),
        columns = 3
      )
    } else if (step == 2) {
      ssr_user <- eco02_user_line_ssr(df, input$ch2_user_b0, input$ch2_user_b1)
      lc_stat_grid(
        lc_stat_box(label = "Twój b₀", value = eco_fmt(input$ch2_user_b0, 1),
                    caption = "wyraz wolny", color = unname(upwr_cat["niebo"])),
        lc_stat_box(label = "Twój b₁", value = eco_fmt(input$ch2_user_b1, 2),
                    caption = "nachylenie", color = unname(upwr_cat["niebo"])),
        lc_stat_box(label = "SSR (Twoja)", value = format(round(ssr_user), big.mark = " "),
                    caption = "suma kwadratów reszt", color = unname(upwr_cat["terakota"])),
        columns = 3
      )
    } else {
      ssr_user <- eco02_user_line_ssr(df, input$ch2_user_b0, input$ch2_user_b1)
      ssr_kmnk <- sum(resid(ch2_fit())^2)
      coefs <- coef(ch2_fit())
      lc_stat_grid(
        lc_stat_box(label = "SSR (Twoja)", value = format(round(ssr_user), big.mark = " "),
                    caption = "suma kwadratów dla Twojej prostej",
                    color = unname(upwr_cat["niebo"])),
        lc_stat_box(label = "SSR (KMNK)", value = format(round(ssr_kmnk), big.mark = " "),
                    caption = "minimum możliwe na tej próbie",
                    color = upwr_accent),
        lc_stat_box(label = "Różnica", value = format(round(ssr_user - ssr_kmnk), big.mark = " "),
                    caption = "ile gorzej dopasowała Twoja prosta",
                    color = unname(upwr_cat["terakota"])),
        columns = 3
      )
    }
  })

  output$ch2_kmnk_verdict <- renderUI({
    step <- ch2_step()
    if (step == 0) return(NULL)
    df <- ch2_data()

    if (step == 1) {
      lc_feedback(
        type = "info",
        strong("Krok 1 — Dane. "),
        "Widzisz chmurę punktów: każdy punkt to jeden sklep. Mocna pozytywna zależność jest gołym okiem widoczna — większy sklep = większa sprzedaż. Pytanie: jaką ",
        em("dokładnie"),
        " prostą przez tę chmurę narysować? Przejdź do kroku 2."
      )
    } else if (step == 2) {
      ssr_user <- eco02_user_line_ssr(df, input$ch2_user_b0, input$ch2_user_b1)
      lc_feedback(
        type = "info",
        strong("Krok 2 — Twoja prosta. "),
        "Manipuluj suwakami b₀ i b₁ tak, żeby SSR było jak najmniejsze. Pomarańczowe odcinki to reszty — pionowe odległości od punktów do Twojej prostej. SSR jest sumą kwadratów tych odcinków. ",
        "Twoje aktualne SSR = ", strong(format(round(ssr_user), big.mark = " ")),
        ". Przejdź do kroku 3, żeby zobaczyć, ile lepiej zrobiłaby sama metoda."
      )
    } else if (step == 3) {
      coefs <- coef(ch2_fit())
      ssr_user <- eco02_user_line_ssr(df, input$ch2_user_b0, input$ch2_user_b1)
      ssr_kmnk <- sum(resid(ch2_fit())^2)
      blisko <- ssr_user / ssr_kmnk

      ocena <- if (blisko < 1.05) "Brawo — to praktycznie KMNK." else
               if (blisko < 1.30) "Bardzo blisko — różnica niewielka." else
               if (blisko < 2.00) "Blisko, ale jest jeszcze trochę zapasu." else
               "Daleko — KMNK by tu wyraźnie pomogła."
      lc_feedback(
        type = "info",
        strong("Krok 3 — KMNK. "),
        "Czerwona prosta to dopasowanie metodą najmniejszych kwadratów: ",
        strong(paste0("ŷ = ", eco_fmt(coefs[1], 2), " + ", eco_fmt(coefs[2], 3), " · x")),
        ". Jej SSR = ", strong(format(round(ssr_kmnk), big.mark = " ")),
        " — to jest minimum, którego nie da się przebić żadną inną prostą na tej próbie. ",
        ocena, " Przejdź do kroku 4, żeby zobaczyć pełne wyniki."
      )
    } else {
      g <- broom::glance(ch2_fit())
      b <- broom::tidy(ch2_fit())
      r2 <- g$r.squared
      sila <- if (r2 >= 0.7) "silne" else if (r2 >= 0.4) "umiarkowane" else if (r2 >= 0.15) "słabe" else "bardzo słabe"
      tagList(
        lc_stat_grid(
          lc_stat_box(label = "b₀", value = eco_fmt(b$estimate[1], 2),
                      caption = "wyraz wolny", color = upwr_secondary),
          lc_stat_box(label = "b₁", value = eco_fmt(b$estimate[2], 3),
                      caption = "nachylenie", color = unname(upwr_cat["szalwia"])),
          lc_stat_box(label = "R²", value = eco_fmt(r2, 3),
                      caption = "udział wyjaśnionej zmienności",
                      color = unname(upwr_cat["niebo"])),
          lc_stat_box(label = "SE reszt", value = eco_fmt(g$sigma, 1),
                      caption = "typowa wielkość pomyłki",
                      color = unname(upwr_cat["terakota"])),
          columns = 4
        ),
        lc_feedback(
          type = "ok",
          strong("Krok 4 — Model gotowy. "),
          "Z tej próby KMNK wyciągnęła równanie ",
          strong(paste0("ŷ = ", eco_fmt(b$estimate[1], 2), " + ", eco_fmt(b$estimate[2], 3), " · x")),
          ". Każdy dodatkowy m² powierzchni sklepu wiąże się ze średnim wzrostem sprzedaży o ",
          strong(eco_fmt(b$estimate[2], 3)), " tys. zł. ",
          "Metraż wyjaśnia ", strong(paste0(eco_fmt(100 * r2, 1), "%")),
          " zmienności sprzedaży — to dopasowanie ", strong(sila), ". ",
          "Typowa pomyłka modelu (SE reszt) to ±", strong(eco_fmt(g$sigma, 1)),
          " tys. zł na sklep."
        )
      )
    }
  })

  # --- Drugi widget: „Dlaczego kwadraty?” ----------------------------------

  ch2_metoda_data <- reactive({
    set.seed(2024)
    n <- 30
    x <- runif(n, 0, 50)
    y <- 10 + 1.2 * x + rnorm(n, 0, 6)
    if (isTRUE(input$ch2_outlier)) {
      x <- c(x, 45)
      y <- c(y, 15)  # punkt mocno odstający w dół
    }
    data.frame(x = x, y = y)
  })

  output$ch2_metoda_plot <- renderPlot({
    df <- ch2_metoda_data()
    metoda <- input$ch2_metoda

    # Prosta KMNK
    coef_kmnk <- coef(lm(y ~ x, data = df))

    # Prosta LAD (numerycznie — minimum sumy |reszt|)
    lad_loss <- function(par) sum(abs(df$y - par[1] - par[2] * df$x))
    coef_lad <- optim(coef_kmnk, lad_loss, method = "Nelder-Mead")$par

    # Prosta minimax
    minimax_loss <- function(par) max(abs(df$y - par[1] - par[2] * df$x))
    coef_mm <- optim(coef_kmnk, minimax_loss, method = "Nelder-Mead")$par

    coefs <- switch(metoda,
                    kmnk    = coef_kmnk,
                    lad     = coef_lad,
                    minimax = coef_mm)

    label_metody <- switch(metoda,
                           kmnk    = "KMNK (kwadraty reszt)",
                           lad     = "LAD (wartości bezwzględne)",
                           minimax = "Minimax (najgorszy punkt)")

    color_metody <- switch(metoda,
                           kmnk    = upwr_accent,
                           lad     = unname(upwr_cat["szalwia"]),
                           minimax = unname(upwr_cat["niebo"]))

    df$fitted <- coefs[1] + coefs[2] * df$x

    ggplot(df, aes(x, y)) +
      geom_segment(aes(xend = x, yend = fitted),
                   color = color_metody, alpha = 0.4) +
      geom_point(color = unname(upwr_cat["grafit"]), alpha = 0.85, size = 2.3) +
      geom_abline(intercept = coefs[1], slope = coefs[2],
                  color = color_metody, linewidth = 1.3) +
      labs(x = "X", y = "Y", title = label_metody) +
      coord_cartesian(xlim = c(0, 52), ylim = c(0, 80)) +
      theme_upwr()
  })

  output$ch2_metoda_opis <- renderUI({
    opis <- switch(input$ch2_metoda,
      kmnk = list(
        nazwa = "KMNK",
        formula = "min Σ (Yᵢ − b₀ − b₁Xᵢ)²",
        komentarz = "Minimalizujemy SUMĘ KWADRATÓW reszt. Karze mocno duże pomyłki, daje wzory zamknięte."
      ),
      lad = list(
        nazwa = "LAD",
        formula = "min Σ |Yᵢ − b₀ − b₁Xᵢ|",
        komentarz = "Minimalizujemy SUMĘ WARTOŚCI BEZWZGLĘDNYCH. Mniej wrażliwa na outliery, ale brak wzoru zamkniętego."
      ),
      minimax = list(
        nazwa = "Minimax",
        formula = "min max |Yᵢ − b₀ − b₁Xᵢ|",
        komentarz = "Minimalizujemy NAJWIĘKSZĄ resztę. Bardzo ostrożna metoda — chce, by nawet najgorszy punkt nie był strasznie daleko."
      )
    )
    tagList(
      tags$p(strong(opis$nazwa, "— funkcja celu:")),
      tags$pre(opis$formula),
      tags$p(opis$komentarz)
    )
  })

  output$ch2_metoda_verdict <- renderUI({
    metoda <- input$ch2_metoda
    out <- isTRUE(input$ch2_outlier)
    if (!out) {
      lc_feedback(
        type = "info",
        strong("Bez outliera. "),
        "Przy dobrze zachowanych danych wszystkie trzy metody dają podobne proste — różnice są minimalne. Dlatego w praktyce w 99% przypadków używa się KMNK: ma najlepsze własności statystyczne i działa równie dobrze."
      )
    } else {
      jak <- switch(metoda,
        kmnk = "KMNK silnie reaguje na outliera — prosta wyraźnie się przesuwa, bo kwadrat dużej reszty dominuje w sumie.",
        lad = "LAD jest mniej wrażliwa — wartość bezwzględna nie wzmacnia dużych reszt. Outlier nie deformuje prostej tak mocno.",
        minimax = "Minimax jest najbardziej wrażliwa — dosłownie chce, żeby najgorszy punkt nie był ‚za bardzo‘ daleko, więc outlier ją prowadzi."
      )
      lc_feedback(
        type = "warning",
        strong("Z outlierem. "),
        jak,
        " To pokazuje, dlaczego diagnostyka graficzna jest ważna — KMNK milczy, jeśli źle ją karmimy."
      )
    }
  })
}
