# ============================================================================
# ROZDZIAŁ 1: Równanie regresji liniowej
# ============================================================================

ch1_ui <- lecture_chapter(
  id = "ch-rownanie",
  num = "01",
  title = "Równanie regresji",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · KMNK",
      num = "01",
      title = "Równanie regresji liniowej.",
      lead = "Korelacja powiedziała tylko, że dwie zmienne są powiązane. Regresja idzie dalej: modeluje ten związek liczbowo i pozwala odpowiadać na pytania ‚o ile’ — a nie tylko ‚czy’."
    ),

    lc_h2("ch1-po-co", "Po co regresja?"),
    lc_p("Wyobraź sobie, że prowadzisz lodziarnię i zauważyłeś, że w cieplejsze dni sprzedajesz więcej lodów. To jest korelacja — wiesz, że temperatura i sprzedaż się trzymają razem. Ale jeśli jutro prognoza pokazuje 26°C zamiast 22°C, to o ile więcej lodów zamówić u dostawcy? Korelacja na to pytanie nie odpowie. Regresja owszem."),
    lc_p("Albo inny obraz: rolnik patrzy na ilość nawozu (X) i plon z hektara (Y). Wie, że zależność jest dodatnia, ale chce konkretnej liczby — o ile kilogramów więcej da każdy dodatkowy worek nawozu? Regresja liniowa daje mu tę liczbę i pokazuje, jak bardzo można jej wierzyć."),
    lc_p("Krótko: regresja liniowa to narzędzie do mierzenia wpływu jednej zmiennej na drugą. Wpływu wyrażonego liczbą, którą można wstawić do prognozy, do umowy, do biznesplanu."),

    lc_h2("ch1-formula", "Równanie regresji liniowej z jedną zmienną"),
    lc_formula_box(
      withMathJax(helpText("$$Y_i = \\beta_0 + \\beta_1 X_i + \\varepsilon_i$$")),
      p(withMathJax("\\(Y_i\\)"), " — zmienna objaśniana dla obserwacji ", withMathJax("\\(i\\)"), " (np. sprzedaż w danym miesiącu)."),
      p(withMathJax("\\(X_i\\)"), " — zmienna objaśniająca (np. wydatki na reklamę w tym samym miesiącu)."),
      p(withMathJax("\\(\\beta_0\\)"), " — wyraz wolny: wartość Y, gdy X = 0."),
      p(withMathJax("\\(\\beta_1\\)"), " — nachylenie: o ile średnio zmienia się Y, gdy X rośnie o jedną jednostkę."),
      p(withMathJax("\\(\\varepsilon_i\\)"), " — składnik losowy: wszystko, czego model nie opisał (inne czynniki, błędy pomiaru, przypadkowość).")
    ),
    lc_p("To jest ten sam zapis, z którym spotkamy się w Excelu, Gretlu, R-ze i każdym podręczniku ekonometrii. Litery z indeksem ", withMathJax("\\(i\\)"), " to konkretne obserwacje (firma 1, firma 2, miesiąc 1, miesiąc 2…), a parametry ", withMathJax("\\(\\beta_0\\)"), " i ", withMathJax("\\(\\beta_1\\)"), " są dla wszystkich obserwacji wspólne — opisują regułę, którą próbujemy wykryć."),

    lc_h2("ch1-interpretacja", "Co znaczą β₀ i β₁ w praktyce?"),
    lc_p("Załóżmy, że dla pewnej firmy oszacowano model:"),
    lc_formula_box(
      withMathJax(helpText("$$\\widehat{Y_i} = 5{,}0 + 1{,}4 \\cdot X_i$$")),
      p("gdzie Y to miesięczna sprzedaż w tysiącach złotych, a X to wydatki na reklamę w tysiącach złotych.")
    ),
    lc_stat_grid(
      lc_stat_box("β₀ = 5,0", caption = "w miesiącu bez reklamy przewidujemy 5 tys. zł sprzedaży", color = upwr_secondary),
      lc_stat_box("β₁ = 1,4", caption = "każdy dodatkowy 1 tys. zł reklamy podnosi sprzedaż średnio o 1,4 tys. zł", color = unname(upwr_cat["szalwia"])),
      lc_stat_box("ε", caption = "konkretny miesiąc może odbiegać od tej reguły w obie strony", color = unname(upwr_cat["terakota"])),
      columns = 3
    ),
    lc_p("Dwie liczby — i już można rozmawiać o decyzji marketingowej. Jeśli wzrost wydatków o 1 tys. zł daje średnio 1,4 tys. zł sprzedaży, to ekonomicznie się to opłaca, póki marża pokrywa różnicę. Bez modelu mielibyśmy tylko poczucie, że „chyba reklama działa”."),

    # ------------------------------------------------------------------------
    # Widget 1 — „Co znaczy β₁?” Manipulacja parametrami bez danych.
    # ------------------------------------------------------------------------
    lc_h2("ch1-widget-bety", "Pobaw się parametrami: jak wygląda prosta?"),
    lc_p("Zanim wejdziemy w realne dane, popatrzmy na samo równanie. Suwaki niżej zmieniają β₀ (wyraz wolny) i β₁ (nachylenie). Wykres pokazuje samą prostą — bez chmury punktów, bez szumu — w realistycznych jednostkach lodziarni: X to temperatura (°C), Y to sprzedaż (tys. zł)."),
    figure_panel(
      label = "Ryc. 1.1",
      title = "Prosta regresji jako funkcja β₀ i β₁",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch1_beta0", "Wyraz wolny β₀", min = -5, max = 15, value = 2, step = 0.5),
          sliderInput("ch1_beta1", "Nachylenie β₁",  min = -1, max = 3,  value = 0.4, step = 0.1)
        ),
        column(
          8,
          plotOutput("ch1_line_plot", height = "320px"),
          uiOutput("ch1_line_verdict")
        )
      )
    ),
    inline_callout(
      label = "Wskazówka",
      color = "ok",
      "Zauważ, że β₀ przesuwa linię w górę i w dół (przecięcie z osią Y), a β₁ obraca ją wokół tego punktu. To są dwa niezależne parametry — każdy odpowiada za inną cechę prostej."
    ),

    # ------------------------------------------------------------------------
    # Widget 2 — „Po co składnik losowy?” Przyciski +szum.
    # ------------------------------------------------------------------------
    lc_h2("ch1-widget-szum", "Po co potrzebujemy ε? Dodaj szum krok po kroku."),
    lc_p("Idealna prosta jest dobra w podręczniku. W realnych danych obserwacje nie leżą dokładnie na linii — są chmurą wokół niej. To nie jest błąd, tylko skutek tego, że na sprzedaż wpływa wiele innych czynników (pogoda, promocje konkurencji, dzień tygodnia). Wszystko, co nie jest β₀+β₁X, ląduje w ε."),
    lc_p("Kliknij przycisk niżej kilka razy. Każde kliknięcie zwiększa wariancję składnika losowego — zacznij od idealnych danych i zobacz, jak rosnący szum stopniowo „rozmywa” prostą."),
    figure_panel(
      label = "Ryc. 1.2",
      title = "Mechanizm β₀+β₁X jest stały — tylko ε rośnie",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          actionButton("ch1_szum_plus", "Zwiększ szum (+1)", class = "btn-primary", width = "100%"),
          tags$br(), tags$br(),
          actionButton("ch1_szum_reset", "Wyzeruj szum", class = "btn-outline-secondary", width = "100%"),
          tags$br(), tags$br(),
          uiOutput("ch1_szum_status")
        ),
        column(
          8,
          plotOutput("ch1_szum_plot", height = "340px"),
          uiOutput("ch1_szum_verdict")
        )
      )
    ),
    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      "Mechanizm generujący dane (prawdziwa prosta β₀+β₁X) cały czas jest ten sam. Zmienia się tylko, jak bardzo poszczególne obserwacje od niego odbiegają. Zadaniem KMNK będzie odzyskać tę prawdziwą prostą z zaszumionych danych — i to jest dokładnie temat następnego rozdziału."
    ),

    inline_callout(
      label = "Pułapka",
      color = "uwaga",
      "Wyraz wolny β₀ ma sens ekonomiczny tylko wtedy, gdy X = 0 leży w zakresie zaobserwowanych danych. Jeśli nasze firmy wydają na reklamę od 10 do 50 tys. zł, β₀ to ekstrapolacja — formalnie poprawna, ale nie traktuj jej jak realnego scenariusza."
    ),

    lc_chapter_next(
      num = "02",
      title = "Dopasowanie KMNK",
      lead = "metoda najmniejszych kwadratów na danych",
      target_id = "ch-dopasowanie"
    )
  )
)

ch1_server <- function(input, output, session) {

  # --- Widget 1: prosta jako funkcja β₀, β₁ -------------------------------

  output$ch1_line_plot <- renderPlot({
    x <- seq(0, 35, length.out = 100)
    y <- input$ch1_beta0 + input$ch1_beta1 * x
    df <- data.frame(x = x, y = y)

    ggplot(df, aes(x, y)) +
      geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
      geom_vline(xintercept = 0, color = "gray70", linewidth = 0.3) +
      geom_line(color = upwr_accent, linewidth = 1.4) +
      geom_point(data = data.frame(x = 0, y = input$ch1_beta0),
                 aes(x, y), size = 4, color = upwr_secondary) +
      annotate("label", x = 0, y = input$ch1_beta0, label = "β₀", hjust = -0.4,
               fill = "white", color = upwr_secondary, size = 5) +
      coord_cartesian(xlim = c(-2, 35), ylim = c(-15, 35)) +
      labs(x = "X — temperatura (°C)", y = "Y — sprzedaż (tys. zł)") +
      theme_upwr()
  })

  output$ch1_line_verdict <- renderUI({
    b0 <- input$ch1_beta0
    b1 <- input$ch1_beta1
    znak_b1 <- if (b1 > 0) "rośnie" else if (b1 < 0) "maleje" else "się nie zmienia"

    przy_25 <- b0 + b1 * 25
    przy_30 <- b0 + b1 * 30
    diff5 <- przy_30 - przy_25

    lc_feedback(
      type = "info",
      tags$p(
        strong("Przy aktualnych parametrach: "),
        "Y = ", eco_fmt(b0, 2), " + ", eco_fmt(b1, 2), " · X."
      ),
      tags$p(
        "Przy 0°C przewidujemy ", strong(eco_fmt(b0, 2), " tys. zł"),
        " sprzedaży (czyli β₀). Każdy dodatkowy stopień Celsjusza sprawia, że sprzedaż ",
        znak_b1, " średnio o ", strong(eco_fmt(abs(b1), 2), " tys. zł"),
        " (czyli β₁ = ", eco_fmt(b1, 2), ")."
      ),
      tags$p(
        "Konkretnie: w dzień 25°C model przewiduje ",
        strong(eco_fmt(przy_25, 2), " tys. zł"),
        ", a w dzień 30°C — ", strong(eco_fmt(przy_30, 2), " tys. zł"),
        ". Różnica 5 stopni daje różnicę ", strong(eco_fmt(diff5, 2), " tys. zł"), "."
      )
    )
  })

  # --- Widget 2: szum krok po kroku ---------------------------------------

  ch1_szum_level <- reactiveVal(0)

  observeEvent(input$ch1_szum_plus, {
    ch1_szum_level(min(ch1_szum_level() + 1, 6))
  })
  observeEvent(input$ch1_szum_reset, {
    ch1_szum_level(0)
  })

  ch1_szum_data <- reactive({
    set.seed(2024)
    n <- 30
    x <- seq(15, 30, length.out = n)
    sigma <- ch1_szum_level() * 1.2
    eps <- if (sigma > 0) rnorm(n, 0, sigma) else rep(0, n)
    y <- 2 + 0.4 * x + eps
    data.frame(x = x, y = y, fit = 2 + 0.4 * x)
  })

  output$ch1_szum_status <- renderUI({
    lvl <- ch1_szum_level()
    sigma <- lvl * 1.2
    label <- if (lvl == 0) "idealne dane" else
             if (lvl <= 2) "lekki szum"   else
             if (lvl <= 4) "umiarkowany szum" else "duży szum"
    lc_stat_grid(
      lc_stat_box(
        label = paste0("Poziom szumu: ", lvl, "/6"),
        caption = paste0("σ ≈ ", eco_fmt(sigma, 1), "  ·  ", label),
        color = upwr_accent
      ),
      columns = 1
    )
  })

  output$ch1_szum_plot <- renderPlot({
    df <- ch1_szum_data()
    ggplot(df, aes(x, y)) +
      geom_line(aes(y = fit), color = unname(upwr_cat["niebo"]),
                linewidth = 1.2, linetype = "dashed") +
      geom_point(color = unname(upwr_cat["grafit"]), alpha = 0.8, size = 2.6) +
      coord_cartesian(xlim = c(14, 31), ylim = c(0, 25)) +
      labs(
        x = "X — temperatura (°C)",
        y = "Y — sprzedaż (tys. zł)",
        caption = "Niebieska przerywana: prawdziwa prosta β₀+β₁X. Punkty: realizacje z szumem ε."
      ) +
      theme_upwr()
  })

  output$ch1_szum_verdict <- renderUI({
    lvl <- ch1_szum_level()
    if (lvl == 0) {
      lc_feedback(
        type = "ok",
        strong("Szum = 0. "),
        "Wszystkie punkty leżą dokładnie na prostej Y = 2 + 0,4 X. To jest stan idealny, którego w rzeczywistości nie zobaczysz — dane realne zawsze mają jakąś niepewność."
      )
    } else if (lvl <= 2) {
      lc_feedback(
        type = "info",
        strong("Lekki szum. "),
        "Punkty rozsypują się wokół prostej, ale liniowy trend jest wyraźny gołym okiem. Dla takich danych KMNK znajdzie prostą bardzo blisko prawdziwej."
      )
    } else if (lvl <= 4) {
      lc_feedback(
        type = "info",
        strong("Umiarkowany szum. "),
        "Trend liniowy jest cały czas widoczny, ale poszczególne punkty potrafią leżeć daleko od linii. Pojedynczej obserwacji nie da się przewidzieć — model mówi jedynie o ‚średnim‘ zachowaniu."
      )
    } else {
      lc_feedback(
        type = "warning",
        strong("Duży szum. "),
        "Mechanizm β₀+β₁X cały czas tam jest — ale gołym okiem trudno go już wyłowić. To jest typowa sytuacja realnych danych ekonomicznych. Bez statystyki łatwo o błędne wnioski."
      )
    }
  })
}
