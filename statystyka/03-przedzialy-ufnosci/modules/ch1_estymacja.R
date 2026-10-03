# ============================================================================
# CHAPTER 1: Od próby do populacji
# ============================================================================

ch1_ui <- list(
  id    = "ch-estymacja",
  num   = "01",
  title = "Od próby do populacji",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 01 · Przedziały ufności",
      num    = "01",
      title  = "Od próby do populacji.",
      lead   = "Średniego wzrostu wszystkich studentów nikt nie zmierzy. Mierzymy
                kilkadziesiąt osób i liczymy z nich jedną liczbę, choć następna grupa
                dałaby trochę inną. Ten wykład pokazuje, jak z jednej próby powiedzieć,
                gdzie leży wartość dla wszystkich i z jakim zapasem."
    ),

    lc_p("Wykład 02 zakończyliśmy centralnym twierdzeniem granicznym. Pokazało ono,
      że średnia z próby jest zmienną losową: każda nowa próba daje inną średnią,
      a średnie z wielu prób układają się w ", gloss("rozkład próbkowy"), "
      o środku μ i odchyleniu standardowym σ/√n. Tam patrzyliśmy na to od strony
      populacji: znaliśmy μ i σ i pytaliśmy, jakie średnie z prób mogą wyjść.
      W praktyce sytuacja jest odwrotna. Mamy jedną próbę, a μ nie znamy.
      W tym rozdziale nazwiemy to zadanie i zobaczymy, czego wymagamy od liczby,
      którą szacujemy nieznany parametr."),

    lc_h2("ch1-estymacja", "Estymacja — od próby do populacji"),

    lc_p("Liczbę opisującą całą ", gloss("populacja", "populację"), ", na przykład
      średni wzrost wszystkich studentów w Polsce, nazywamy ", gloss("parametr", "parametrem"),
      ". Parametr ma jedną, stałą wartość, ale zwykle jej nie znamy, bo nie da się
      zmierzyć wszystkich. Zamiast tego pobieramy ", gloss("próba", "próbę"), ",
      na przykład 100 osób, i liczymy z niej ", gloss("statystyka", "statystykę"),
      ". Szacowanie nieznanego parametru na podstawie próby nazywamy ",
      gloss("estymacja", "estymacją"), "."),

    lc_p("Regułę, według której z danych liczymy oszacowanie, nazywamy ",
      gloss("estymator", "estymatorem"), ", a liczbę otrzymaną z konkretnej próby — ",
      gloss("estymata", "estymatą"), ". ", gloss("średnia", "Średnia"), " z próby ",
      withMathJax("\\(\\bar{x}\\)"), " jest estymatorem średniej populacji ",
      withMathJax("\\(\\mu\\)"), ". Jeśli w naszej próbie wyszło ",
      withMathJax("\\(\\bar{x} = 171.3\\)"), " cm, to 171.3 cm jest estymatą.
      Estymator to przepis, estymata to wynik zastosowania go do jednej próby."),

    lc_h2("ch1-estymator", "Estymator w akcji"),

    lc_p("Żeby ocenić, jak działa estymator, trzeba odwrócić typową sytuację:
      wybrać populację o znanym μ, losować z niej wiele prób i sprawdzać,
      gdzie lądują kolejne estymaty. Panel poniżej robi to dla czterech
      rozkładów populacji. Wrzosowa przerywana linia to prawdziwe μ,
      bursztynowa — średnia ze wszystkich dotąd uzyskanych estymat ",
      withMathJax("\\(\\bar{x}\\)"), "."),

    figure_panel(
      label = "Ryc. 1.1", title = "Losowanie prób z populacji",
      full_width = TRUE,
      lc_toolbar(
        selectInput("ch1_dist", "Rozkład populacji",
            choices = c(
              "Normalny (wzrost)"         = "normal",
              "Wykładniczy (prawoskośny)" = "exponential",
              "Jednostajny"               = "uniform",
              "Dwumodalny"                = "bimodal"
            ),
            selected = "normal"
          ),
        lc_slider("ch1_n", "Wielkość próby (n)", 5, 200, 30, 5),
        lc_action("ch1_draw_1", "Pobierz 1 próbę", variant = "solid"),
        lc_action("ch1_draw_20", "Pobierz 20 prób", variant = "solid"),
        lc_action("ch1_reset", icon = "reset", variant = "ghost", aria_label = "Reset"),
        lc_readouts(uiOutput("ch1_estimates_stats"), uiOutput("ch1_count_info"))
      ),
      lc_plot("ch1_estimates_plot", max_height = "400px")
    ),

    lc_p("Populacja „wzrostu” ma rozkład normalny ze średnią μ = 170 cm
      i odchyleniem standardowym σ = 10 cm. Przy n = 30 średnie z prób rozkładają
      się wokół 170 cm z błędem standardowym σ/√n = 10/√30 ≈ 1.83 cm, więc około
      95% z nich wypada między 166.4 a 173.6 cm. Po kilkudziesięciu losowaniach
      dwie rzeczy są wyraźne. Pojedyncze estymaty rozrzucają się po obu stronach μ,
      ale ich średnia leży tuż przy μ. Wartość „SD estymat” w panelu jest bliska
      1.83 cm, czyli błędowi standardowemu z wykładu 02. Po wyczyszczeniu panelu
      i zwiększeniu n histogram estymat jest węższy, a zmiana rozkładu populacji na wykładniczy, jednostajny czy
      dwumodalny nie zmienia tego obrazu: estymaty dalej skupiają się wokół μ."),

    lc_h2("ch1-wlasnosci", "Trzy własności dobrego estymatora"),

    lc_p("Średnia z próby nie jest jedynym możliwym estymatorem środka populacji.
      Równie dobrze można by użyć mediany z próby, średniej z najmniejszej
      i największej obserwacji albo średniej ucinanej. Żeby wybrać między nimi,
      potrzebujemy kryteriów. Statystyka ocenia estymatory według trzech
      podstawowych własności: ",
      gloss("nieobciążoność", "nieobciążoności"), ", ",
      gloss("efektywność estymatora", "efektywności"), " i ",
      gloss("zgodność estymatora", "zgodności"), ". Poniżej ",
      withMathJax("\\(\\hat{\\theta}\\)"), " oznacza estymator parametru ",
      withMathJax("\\(\\theta\\)"), "."),

    lc_h3("(1) Nieobciążoność"),

    lc_p("Estymator jest nieobciążony, gdy jego wartość oczekiwana jest równa
      szacowanemu parametrowi:"),

    lc_formula_box(withMathJax(
      "$$E(\\hat{\\theta}) = \\theta$$"
    )),

    lc_p("Pojedyncza estymata może wypaść za wysoko albo za nisko, ale średnio,
      w bardzo wielu hipotetycznych próbach, estymator trafia w parametr.
      Nie ma błędu systematycznego w jedną stronę."),

    lc_p(strong("Przykład:"), " średnia z próby jest nieobciążonym estymatorem μ.
      To wzór E(X̄) = μ z wykładu 02 i to właśnie widać na Ryc. 1.1:
      bursztynowa linia średniej z estymat leży tuż przy wrzosowej linii μ."),

    lc_p(strong("Kontrprzykład:"), " ", gloss("wariancja"), " z próby liczona
      z dzieleniem przez n, ",
      withMathJax("\\(\\frac{1}{n}\\sum(x_i - \\bar{x})^2\\)"), ", jest obciążona.
      Jej wartość oczekiwana wynosi (n - 1)/n · σ², więc średnio zaniża wariancję
      populacji. Dla n = 10 i σ² = 100 daje średnio 90 zamiast 100. Dlatego
      wariancję z próby liczy się z dzieleniem przez n - 1: ta poprawka usuwa
      obciążenie."),

    lc_h3("(2) Efektywność"),

    lc_p("Nieobciążoność mówi tylko, że estymator trafia średnio. Dwa estymatory
      nieobciążone mogą jednak różnić się rozrzutem: jeden daje estymaty
      skupione blisko parametru, drugi często myli się mocno w jedną lub drugą
      stronę, a błędy znoszą się dopiero po uśrednieniu wielu prób. Spośród
      estymatorów nieobciążonych lepszy jest ten o mniejszej wariancji,
      bo w pojedynczej próbie, a tylko taką zwykle mamy, częściej wypada blisko
      prawdy. Taki estymator nazywamy efektywniejszym."),

    lc_p(strong("Przykład:"), " gdy populacja ma rozkład normalny, zarówno średnia,
      jak i ", gloss("mediana"), " z próby są nieobciążonymi estymatorami μ.
      Przy dużych próbach wariancja mediany jest jednak około π/2 ≈ 1.57 raza
      większa niż wariancja średniej. Mediana z próby liczącej 157 obserwacji
      jest więc mniej więcej tak dokładna jak średnia ze 100 obserwacji.
      Dlatego przy pomiarach o rozkładzie zbliżonym do normalnego standardem
      jest średnia arytmetyczna."),

    lc_p(strong("Uwaga:"), " efektywność zależy od rozkładu populacji. Gdy w danych
      zdarzają się ", gloss("wartość odstająca", "wartości odstające"), ",
      średnia mocno na nie reaguje i mediana może okazać się efektywniejsza."),

    lc_h3("(3) Zgodność"),

    lc_p("Trzecia własność dotyczy tego, co dzieje się, gdy zbieramy więcej danych.
      Estymator jest zgodny, gdy wraz ze wzrostem wielkości próby zbiega
      (według prawdopodobieństwa) do prawdziwego parametru:"),

    lc_formula_box(withMathJax(
      "$$\\hat{\\theta}_n \\xrightarrow{p} \\theta \\quad \\text{gdy} \\quad n \\to \\infty$$"
    )),

    lc_p("Oznacza to, że dla dowolnie małego marginesu prawdopodobieństwo,
      że estymata odbiegnie od parametru o więcej niż ten margines, maleje
      do zera, gdy n rośnie. W dużej próbie estymator praktycznie nie może
      trafić daleko od prawdy."),

    lc_p(strong("Przykład:"), " średnia z próby jest zgodnym estymatorem μ.
      Wynika to z ", gloss("prawo wielkich liczb", "prawa wielkich liczb"),
      ", a widać to też we wzorze na błąd standardowy: ",
      gloss("odchylenie standardowe"), " średniej, SE = σ/√n, maleje do zera
      wraz ze wzrostem n. Wariancja z próby jest zgodna zarówno w wersji
      z n - 1, jak i z n: obciążenie (n - 1)/n znika, gdy n rośnie."),

    lc_p("Z trzech własności wynika praktyczna kolejność wyboru. Najpierw szukamy
      estymatorów nieobciążonych, spośród nich wybieramy najefektywniejszy,
      a zgodność gwarantuje, że więcej danych daje dokładniejszy wynik.
      Średnia z próby spełnia wszystkie trzy warunki dla μ i dlatego jest
      punktem wyjścia dla przedziałów ufności w tym wykładzie. Zgodność ma
      jednak swoją cenę: SE maleje jak 1/√n, a nie jak 1/n."),

    inline_callout(
      label = "Zasada",
      "Żeby zmniejszyć błąd standardowy średniej o połowę, trzeba czterokrotnie
       zwiększyć próbę."
    ),

    lc_h2("ch1-punkt-nie-wystarczy", "Sam punkt nie wystarczy"),

    lc_p("Nawet najlepszy estymator daje w każdej próbie inną estymatę.
      Liczba ", withMathJax("\\(\\bar{x} = 171.3\\)"), " cm podana bez komentarza
      nie mówi, czy prawdziwe μ może wynosić 171 cm, czy równie dobrze 165 cm.
      O tym decyduje rozrzut estymatora, a więc błąd standardowy. Poniższy panel
      losuje kolejne próby z populacji wzrostu (μ = 170 cm, σ = 10 cm) i zapisuje
      ich średnie jedna po drugiej."),

    figure_panel(
      label = "Ryc. 1.2", title = "Wahania estymatora",
      full_width = TRUE,
      lc_toolbar(
        lc_slider("ch1_fluct_n", "Wielkość próby (n)", 5, 200, 10, 5),
        lc_action("ch1_fluct_draw", "Losuj próbę", icon = "shuffle", variant = "solid")
      ),
      lc_plot("ch1_fluct_plot", max_height = "300px"),
      lc_caption("Każde kliknięcie losuje nową próbę.")
    ),

    lc_p("Przy n = 10 błąd standardowy wynosi 10/√10 ≈ 3.16 cm, więc około 95%
      średnich z prób wypada między 163.8 a 176.2 cm. Kolejne punkty skaczą
      o kilka centymetrów w górę i w dół od linii μ. Przy n = 40 SE spada
      do 1.58 cm i skoki są o połowę mniejsze, ale nie znikają. Dowolna
      pojedyncza estymata może więc leżeć kilka centymetrów od μ, a sama
      liczba nie zdradza, jak daleko."),

    lc_p("Dlatego oprócz estymaty podaje się zakres wartości, który uwzględnia
      tę niepewność: ", gloss("przedział ufności"), ". Punktem wyjścia jest
      zdanie z wykładu 02: w około 95% prób średnia leży nie dalej niż 1.96·SE
      od μ. Jeśli tak jest, to również μ leży nie dalej niż 1.96·SE od średniej
      z próby. Następny rozdział zamienia to odwrócenie w konstrukcję przedziału
      i wyjaśnia, co dokładnie oznacza jego poziom ufności."),

    lc_chapter_next(
      num       = "02",
      title     = "Idea przedziałów",
      lead      = "jak skonstruować przedział ufności i co on naprawdę mówi",
      target_id = "ch-idea"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  # --- Widget 1: Estymator w akcji ---
  ch1_estimates <- reactiveVal(data.frame(
    i = integer(0), xbar = numeric(0)
  ))

  draw_samples <- function(k) {
    dist <- input$ch1_dist
    n <- input$ch1_n
    params <- get_population_params(dist)
    old <- ch1_estimates()
    new_rows <- lapply(seq_len(k), function(j) {
      samp <- generate_population_sample(dist, n)
      data.frame(i = nrow(old) + j, xbar = mean(samp))
    })
    ch1_estimates(rbind(old, do.call(rbind, new_rows)))
  }

  observeEvent(input$ch1_draw_1, draw_samples(1))
  observeEvent(input$ch1_draw_20, draw_samples(20))
  observeEvent(input$ch1_reset, {
    ch1_estimates(data.frame(i = integer(0), xbar = numeric(0)))
  })
  observeEvent(input$ch1_dist, {
    ch1_estimates(data.frame(i = integer(0), xbar = numeric(0)))
  })

  output$ch1_count_info <- renderUI({
    n_est <- nrow(ch1_estimates())
    lc_readout("Prób", n_est, color = col_ci)
  })

  zoom_plot_server("ch1_estimates_plot", reactive({
    est <- ch1_estimates()
    params <- get_population_params(input$ch1_dist)

    if (nrow(est) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Pobierz 1 próbę”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      ggplot(est, aes(x = xbar)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = col_ci, alpha = 0.6, color = "white") +
        geom_vline(xintercept = params$mu, color = col_true,
                   linewidth = 1.5, linetype = "dashed") +
        annotate("text", x = params$mu, y = Inf, vjust = 2,
                 label = "μ",
                 color = col_true, fontface = "bold", size = 5) +
        geom_vline(xintercept = mean(est$xbar), color = col_estimate,
                   linewidth = 1.5, linetype = "solid") +
        annotate("text", x = mean(est$xbar), y = Inf, vjust = 4,
                 label = "średnia x̄",
                 color = col_estimate, fontface = "bold", size = 5) +
        labs(
             x = expression(bar(x)), y = "Gęstość") +
        theme_upwr()
    }
  }))

  output$ch1_estimates_stats <- renderUI({
    est <- ch1_estimates()
    if (nrow(est) == 0) return(NULL)
    params <- get_population_params(input$ch1_dist)
    tagList(
      lc_readout("μ", round(params$mu, 2), color = col_true),
      lc_readout("Śr. estymat", round(mean(est$xbar), 2), color = col_estimate),
      lc_readout("SD estymat", round(sd(est$xbar), 2), color = upwr_secondary)
    )
  })

  # --- Sekcja 2: tylko tekst, brak server logic ---

  # --- Widget 3: Wahania estymatora ---
  ch1_fluct_history <- reactiveVal(data.frame(
    draw = integer(0), xbar = numeric(0)
  ))

  observeEvent(input$ch1_fluct_draw, {
    samp <- generate_population_sample("normal", input$ch1_fluct_n)
    old <- ch1_fluct_history()
    ch1_fluct_history(rbind(old, data.frame(
      draw = nrow(old) + 1, xbar = mean(samp)
    )))
  })

  observeEvent(input$ch1_fluct_n, {
    ch1_fluct_history(data.frame(draw = integer(0), xbar = numeric(0)))
  })

  zoom_plot_server("ch1_fluct_plot", reactive({
    df <- ch1_fluct_history()
    params <- get_population_params("normal")

    if (nrow(df) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij „Losuj próbę”",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      ggplot(df, aes(x = draw, y = xbar)) +
        geom_hline(yintercept = params$mu, color = col_true,
                   linewidth = 1.2, linetype = "dashed") +
        geom_point(color = col_estimate, size = 3) +
        geom_line(color = col_estimate, alpha = 0.5) +
        annotate("text", x = max(df$draw), y = params$mu,
                 label = "μ",
                 vjust = -1, color = col_true, fontface = "bold") +
        labs(
             x = "Numer losowania", y = expression(bar(x))) +
        theme_upwr()
    }
  }))
}
