# ============================================================================
# CHAPTER 4: Przedział dla proporcji
# ============================================================================

ch4_ui <- list(
  id    = "ch-proporcja",
  num   = "04",
  title = "Przedział dla proporcji",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 04 · Przedziały ufności",
      num    = "04",
      title  = "Przedział dla proporcji.",
      lead   = "Odsetek zdających, poparcie w sondażu, udział wadliwych sztuk:
                wiele pytań dotyczy proporcji, a nie średniej. Przedział buduje
                się tak samo jak dla średniej, inny jest tylko wzór na błąd
                standardowy."
    ),

    lc_h2("ch4-wzor", "Wzór"),

    lc_p("W poprzednim rozdziale przedział dla średniej składał się z trzech
      elementów: estymatora \\(\\bar{x}\\), błędu standardowego \\(s/\\sqrt{n}\\)
      i wartości krytycznej \\(t^*\\). Wiele pytań badawczych dotyczy jednak
      odsetka: jaka część studentów zdała egzamin, jaka część wyborców popiera
      partię, ile procent produktów jest wadliwych. Każda pojedyncza odpowiedź
      ma tu dwa warianty, TAK albo NIE, a parametrem populacji jest proporcja
      \\(p\\), czyli odsetek odpowiedzi TAK."),

    lc_p("Estymatorem \\(p\\) jest ", gloss("proporcja z próby"), " \\(\\hat{p}\\):
      liczba odpowiedzi TAK \\(x\\) podzielona przez liczebność próby \\(n\\)."),

    lc_formula_box(
      withMathJax("$$\\hat{p} = \\frac{x}{n}$$")
    ),

    lc_p("Jak bardzo \\(\\hat{p}\\) waha się z próby na próbę, wynika z wykładu 02.
      Liczba odpowiedzi TAK wśród \\(n\\) niezależnych odpowiedzi ma ",
      gloss("rozkład dwumianowy"), " B(n, p), z wartością oczekiwaną
      \\(E(X) = np\\) i ", gloss("wariancja", "wariancją"), " \\(Var(X) = np(1-p)\\). Proporcja z próby to
      \\(X\\) podzielone przez \\(n\\), więc \\(E(\\hat{p}) = p\\), a wariancja
      dzieli się przez \\(n^2\\) i wynosi \\(p(1-p)/n\\). Pierwiastek z niej to ",
      gloss("błąd standardowy"), " proporcji. CTG z wykładu 02 dodaje drugą
      informację: \\(\\hat{p}\\) jest średnią z \\(n\\) zer i jedynek, więc przy
      dużej próbie ma w przybliżeniu rozkład normalny."),

    lc_formula_box(
      withMathJax("$$E(\\hat{p}) = p \\qquad SE(\\hat{p}) = \\sqrt{\\frac{p(1-p)}{n}}$$")
    ),

    lc_p("Prawdziwego \\(p\\) nie znamy, więc we wzorze na SE zastępujemy je przez
      \\(\\hat{p}\\). Dalej konstrukcja jest taka sama jak w rozdziale 3:
      estymator ± wartość krytyczna · SE. Ponieważ opieramy się na przybliżeniu
      normalnym, ", gloss("wartość krytyczna"), " pochodzi z ",
      gloss("rozkład normalny", "rozkładu normalnego"), ".
      Dla poziomu 95% to \\(z^* = 1.96\\),
      jak w wykładzie 02. Tak zbudowany ", gloss("przedział ufności"),
      " nazywa się ", gloss("przedział Walda", "przedziałem Walda"), "."),

    lc_formula_box(
      withMathJax("$$CI = \\hat{p} \\pm z^*_{\\alpha/2} \\cdot \\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}$$")
    ),

    lc_p("W odróżnieniu od rozdziału 3 nie sięgamy po rozkład t-Studenta.
      Rozkład t opisuje średnią z danych o rozkładzie normalnym, gdy \\(\\sigma\\)
      szacujemy z próby niezależnie od średniej. Dane zero-jedynkowe nie mają
      rozkładu normalnego, a ich odchylenie standardowe \\(\\sqrt{p(1-p)}\\)
      wynika wprost z \\(p\\). Uzasadnieniem przedziału jest tu przybliżenie
      normalne rozkładu \\(\\hat{p}\\), więc i wartość krytyczna pochodzi
      z rozkładu normalnego."),

    lc_p("To przybliżenie zawodzi, gdy próba jest mała albo \\(\\hat{p}\\) leży
      blisko 0 lub 1. Rozkład dwumianowy jest wtedy wyraźnie skośny, jak
      B(50, 0.1) w wykładzie 02, a 95-procentowy przedział Walda obejmuje
      prawdziwe \\(p\\) rzadziej, niż obiecuje. Dla \\(p = 0.08\\) i \\(n = 50\\)
      jego rzeczywiste ", gloss("pokrycie"), " wynosi około 91%. Przy 4 wadliwych
      sztukach na 50 Wald daje przedział od 0.5% do 15.5%, a ",
      gloss("przedział Wilsona"), ", który poprawia wzór Walda, od 3.2% do 18.8%.
      Im bliżej
      0 lub 1 leży proporcja i im mniejsza jest próba, tym gorzej działa
      przybliżenie Walda. W tym rozdziale liczymy przedziały Walda, bo ich wzór
      najprościej pokazuje konstrukcję. W części przykładów poniżej sukcesów
      albo porażek jest niewiele i tam przedziały są tylko przybliżone."),

    lc_p("Programy statystyczne często nie podają przedziału Walda, tylko ",
      gloss("przedział Cloppera-Pearsona", "przedział Cloppera-Pearsona"),
      ", oparty bezpośrednio na rozkładzie dwumianowym i bezpieczniejszy przy
      małych próbach. Jego granice mogą się więc nieco różnić od przedziałów
      Walda liczonych w tym rozdziale."),

    lc_h2("ch4-budowa", "Budowa przedziału — krok po kroku"),

    lc_p("Panel przeprowadza tę konstrukcję na jednej próbie. Symulujemy
      50 odpowiedzi TAK/NIE, na przykład pytając 50 studentów, czy zdali
      egzamin, z populacji, w której prawdziwy odsetek TAK wynosi 60%.
      Przycisk „Nowa próba” losuje kolejnych 50 odpowiedzi."),

    figure_panel(
      label = "Ryc. 4.1",
      full_width = TRUE,
      lc_step_widget("ch4_step",
        title = "Konstruowanie przedziału",
        steps = c("Próba", "p̂", "± SE", "Przedział"),
        toolbar = lc_toolbar(
          lc_action("ch4_step_new_sample", "Nowa próba", icon = "shuffle",
                    variant = "outline")
        ),
        plot_id = "ch4_step_plot"
      )
    ),

    lc_p("Przy \\(p = 0.6\\) i \\(n = 50\\) błąd standardowy wynosi około
      \\(\\sqrt{0.6 \\cdot 0.4 / 50} \\approx 0.069\\), a margines błędu
      \\(1.96 \\cdot 0.069 \\approx 0.14\\). Przedział ma więc szerokość
      około 27 punktów procentowych: dla \\(\\hat{p} = 0.60\\) sięga od 46%
      do 74%. Kolejne próby przesuwają \\(\\hat{p}\\), a razem z nim cały
      przedział. Szerokość zmienia się przy tym niewiele, bo zależy od
      \\(\\hat{p}\\) tylko przez iloczyn \\(\\hat{p}(1-\\hat{p})\\), który w okolicy
      0.5 prawie się nie zmienia."),

    lc_p("Tak jak w rozdziale 2, poziom 95% opisuje metodę, a nie pojedynczy
      przedział. Konkretny przedział albo obejmuje 0.6, albo nie. Przy tych
      parametrach przedział Walda trafia w prawdziwe \\(p\\) w 94.1% prób,
      czyli niemal tak często, jak obiecuje. Przybliżenie normalne działa
      tu dobrze, bo w próbie jest typowo około 30 odpowiedzi TAK i 20 NIE."),

    lc_h2("ch4-roznica", "Budowa przedziału dla różnicy proporcji"),

    lc_p("Częściej niż jeden odsetek porównujemy dwa: zdawalność w dwóch grupach,
      skuteczność leku i placebo, odsetek braków na dwóch liniach produkcyjnych.
      Parametrem jest wtedy różnica \\(p_1 - p_2\\), a jej estymatorem różnica
      proporcji z prób \\(\\hat{p}_1 - \\hat{p}_2\\). Gdy próby są niezależne,
      wariancja różnicy jest sumą wariancji obu proporcji, tak samo jak przy
      różnicy średnich w rozdziale 3. Błąd standardowy różnicy to pierwiastek
      z tej sumy:"),

    lc_formula_box(
      withMathJax("$$CI = (\\hat{p}_1 - \\hat{p}_2) \\pm z^* \\cdot \\sqrt{\\frac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\frac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}}$$")
    ),

    lc_p("Dodają się wariancje, a nie błędy standardowe. Dlatego SE różnicy
      jest większy od SE każdej z proporcji, ale mniejszy od ich sumy. Panel
      losuje dwie niezależne próby po 60 osób z populacji, w których odsetek
      osób zadowolonych z usługi wynosi 70% i 50%."),

    figure_panel(
      label = "Ryc. 4.2",
      full_width = TRUE,
      lc_step_widget("ch4_dstep",
        title = "Konstruowanie CI dla różnicy",
        steps = c("Dwie próby", "Dwie p̂", "Różnica", "± SE", "Przedział"),
        toolbar = lc_toolbar(
          lc_action("ch4_dstep_new_sample", "Nowe próby", icon = "shuffle",
                    variant = "outline")
        ),
        plot_id = "ch4_dstep_plot",
        ratio = "2/1"
      )
    ),

    lc_p("Dla tych parametrów SE obu proporcji wynosi około 0.059 i 0.065,
      a SE różnicy około 0.088, czyli mniej niż suma 0.124. Margines błędu to
      \\(1.96 \\cdot 0.088 \\approx 0.17\\), więc przedział dla różnicy ma
      szerokość około 34 punktów procentowych. To więcej niż przedział dla
      jednej proporcji na Ryc. 4.1, choć każda z prób jest większa, bo przedział
      różnicy zbiera niepewność z obu prób."),

    lc_p("Pionowa linia w zerze oznacza brak różnicy. Przedział, który jej nie
      obejmuje, wskazuje z 95% ufnością, która grupa ma wyższy odsetek.
      Prawdziwa różnica wynosi tu 20 punktów procentowych, a mimo to przedział
      leży w całości powyżej zera tylko w około 62% par prób. W pozostałych
      obejmuje zero i nie pozwala rozstrzygnąć, która grupa jest bardziej
      zadowolona. Dwie próby po 60 osób to za mało, żeby tak dużą różnicę
      wykrywać niezawodnie."),

    lc_h2("ch4-case-studies", "Case studies — jak interpretować CI w praktyce"),

    lc_p("Poniższe sytuacje mają ustalone dane, więc ich przedziały się nie
      zmieniają. W każdej budujemy przedział tymi samymi krokami co wyżej,
      a potem sprawdzamy dwie hipotezy. Werdykt zależy od położenia całego
      przedziału względem granicy hipotezy. Przedział w całości po stronie
      hipotezy oznacza TAK, w całości po drugiej stronie NIE, a przedział
      przecinający granicę nie pozwala rozstrzygnąć. Każdy przypadek rozwija
      się po kliknięciu nagłówka."),

    lc_h3("A. Przedział dla jednej proporcji"),

    tags$details(class = "case-study", open = NA,
      tags$summary(
        span(class = "case-icon", "\U0001f5f3️"),
        "A1. Sondaż wyborczy — czytanie pojedynczego CI"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Pracownia sondażowa zapytała 400 wyborców, czy poprą partię X.
            212 odpowiedziało TAK, czyli ", withMathJax("\\(\\hat{p} = 0.53\\)"),
            ". Budujemy przedział dla poparcia w populacji i sprawdzamy dwie hipotezy.")
        ),
        uiOutput("ch4_caseA1_widget")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f50d"),
        "A2. Ten sam odsetek, trzy różne wielkości próby"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Trzy badania mierzą odsetek wadliwych produktów w fabryce.
            W każdym ", withMathJax("\\(\\hat{p} = 0.08\\)"), " (8%), ale próby
            mają różną liczebność: 50, 200 i 1000 sztuk. Kolejne kroki dokładają
            przedziały od najmniejszej próby do największej.")
        ),
        uiOutput("ch4_caseA2_widget")
      )
    ),

    lc_h3("B. Przedział dla różnicy proporcji"),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f48a"),
        "B1. Lek a placebo — odsetek wyleczonych"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Badamy nowy lek przeciwbólowy.
            ", tags$b("Lek:"), " 200 pacjentów, 124 zgłosiło ustąpienie bólu (62%).
            ", tags$b("Placebo:"), " 200 pacjentów, 84 zgłosiło ustąpienie bólu (42%).")
        ),
        uiOutput("ch4_caseB1_widget")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f3ed"),
        "B2. Dwie linie produkcyjne — odsetek braków"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Porównujemy dwie linie produkcyjne pod względem odsetka wadliwych produktów.
            ", tags$b("Linia A:"), " skontrolowano 250 sztuk, 22 wadliwe (8.8%).
            ", tags$b("Linia B:"), " skontrolowano 250 sztuk, 18 wadliwych (7.2%).")
        ),
        uiOutput("ch4_caseB2_widget")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "⚠️"),
        "B3. Pułapka małej próby"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Pilotaż nowej procedury BHP w dwóch zakładach.
            ", tags$b("Zakład A:"), " 30 pracowników, 6 miało wypadek (20%).
            ", tags$b("Zakład B:"), " 30 pracowników, 9 miało wypadek (30%).
            Różnica wygląda na dużą, ale czy z 95% ufnością możemy
            powiedzieć, że w zakładzie A jest bezpieczniej?")
        ),
        uiOutput("ch4_caseB3_widget")
      )
    ),

    lc_h3("C. Wiele grup — forest plot"),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f3e5"),
        "C1. Cztery szpitale — odsetek powikłań pooperacyjnych"
      ),
      div(class = "case-body",
        div(class = "case-scenario",
          p("Porównujemy odsetek powikłań po tej samej operacji w czterech szpitalach.
            Dla każdego znamy liczbę wykonanych zabiegów i liczbę powikłań.
            Kolejne kroki pokazują liczby, proporcje i przedziały.")
        ),
        uiOutput("ch4_caseC1_widget")
      )
    ),

    lc_p("Przypadki powtarzają kilka lekcji. W A2 ta sama proporcja 8% daje
      przedział o szerokości 15 punktów procentowych przy 50 sztukach i tylko
      3.4 punktu przy 1000 sztukach. Dwudziestokrotnie większa próba zwęża
      przedział około 4.5 raza, bo \\(n\\) stoi we wzorze pod pierwiastkiem.
      W B2 i B3 obserwowana różnica nie wystarcza do wniosku: w B3 dziesięć
      punktów procentowych różnicy przy 30 osobach w grupie daje przedział od
      -32 do +12 punktów, który obejmuje zero i różnice w obu kierunkach.
      W C1 szpital D (20.6% powikłań) odstaje od pozostałych trzech (od 5.6%
      do 10%), bo jego przedział nie nakłada się z żadnym innym. Porównywanie
      nakładania się osobnych przedziałów jest jednak kryterium ostrożnym.
      Jak pokazał rozdział 3, o różnicy dwóch grup rozstrzyga przedział
      dla różnicy."),

    lc_p("We wszystkich przykładach szerokość przedziału zależała od liczebności
      próby, a pośrednio także od samej proporcji i od przyjętego poziomu
      ufności. Tym czynnikom przyjrzymy się w następnym rozdziale."),

    lc_chapter_next(
      num       = "05",
      title     = "Co wpływa na szerokość?",
      lead      = "co decyduje o szerokości przedziału",
      target_id = "ch-czynniki"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  # ==========================================================================
  # WIDGET 1: Budowa przedziału dla proporcji krok po kroku
  # ==========================================================================
  # Krok widgetu (1..4) żyje w przeglądarce; nowa próba nie zmienia kroku.
  ch4_step <- lc_step_server("ch4_step", input)$step

  # Generuje próbkę n prób Bernoulliego z true_p = 0.6
  generate_step_prop_sample <- function() {
    set.seed(sample.int(.Machine$integer.max, 1))
    n <- 50
    rbinom(n, 1, 0.6)  # 50 odpowiedzi TAK/NIE
  }
  ch4_step_sample <- reactiveVal(generate_step_prop_sample())

  observeEvent(input$ch4_step_new_sample, {
    ch4_step_sample(generate_step_prop_sample())
  })

  # TAK: dane (niebo), NIE: druga grupa (bursztyn)
  ch4_yes_no_fill <- c("NIE" = STEP_ROLES$group$colour, "TAK" = STEP_ROLES$data$colour)

  # Grubość paska przedziału: element wprowadzany w kroku grubszy niż znany.
  ch4_bar_lw <- function(role) if (role == "new") 1.8 else 1.1

  # Etykieta w kolorze roli, krojem wykresu.
  ch4_role_text <- function(x, y, label, role, size, hjust = 0.5) {
    annotate("text", x = x, y = y, label = label, hjust = hjust,
             colour = STEP_ROLES[[role]]$colour, fontface = "bold", size = size)
  }

  zoom_plot_server("ch4_step_plot", reactive({
    step <- ch4_step()
    samp <- ch4_step_sample()

    n <- length(samp)
    x <- sum(samp)
    phat <- x / n
    z_star <- qnorm(0.975)
    se <- sqrt(phat * (1 - phat) / n)
    me <- z_star * se

    # ---- LEWY PANEL: słupki TAK / NIE (liczebności bezwzględne) ----
    bar_df <- data.frame(
      val = factor(c("NIE", "TAK"), levels = c("NIE", "TAK")),
      count = c(n - x, x)
    )
    p_left <- ggplot(bar_df, aes(x = val, y = count)) +
      step_result(geom_col, width = 0.6, fill = ch4_yes_no_fill[as.character(bar_df$val)]) +
      geom_text(aes(label = count), vjust = -0.4, fontface = "bold",
                family = "mono", size = 5, colour = STEP_ROLES$known$colour) +
      labs(x = NULL, y = "Liczebność") +
      step_frame(xlim = c(0.4, 2.6), ylim = c(0, max(bar_df$count) * 1.15)) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())

    # ---- PRAWY PANEL: oś proporcji z p_hat, SE, CI ----
    # Oddzielne poziomy Y — każdy element na swojej linii
    Y_EST <- 0.30
    Y_SE  <- 0.05
    Y_CI  <- -0.25

    p_right <- ggplot() +
      labs(x = "Proporcja", y = NULL) +
      step_frame(xlim = c(0, 1), ylim = c(-0.6, 0.6), y_axis = FALSE)

    # Krok 2+: pionowa linia prowadząca + punkt p_hat
    if (step >= 2) {
      role <- step_role(step, 2)
      p_right <- p_right +
        step_line("known", xintercept = phat) +
        step_layer(geom_point, role, data = data.frame(x = phat, y = Y_EST),
                   mapping = aes(x = x, y = y), size = 7, shape = 18) +
        ch4_role_text(phat, Y_EST - 0.13, "p̂", role = role,
                   size = 5)
    }

    # Krok 3+: wąski przedział SE
    if (step >= 3) {
      role <- step_role(step, 3)
      p_right <- p_right +
        step_layer(geom_errorbar, role,
                   data = data.frame(xmin = phat - se, xmax = phat + se, y = Y_SE),
                   mapping = aes(xmin = xmin, xmax = xmax, y = y),
                   width = 0.08, linewidth = ch4_bar_lw(role)) +
        ch4_role_text(phat, Y_SE - 0.12, "± SE", role = role,
                   size = 4.2)
    }

    # Krok 4: pełen CI
    if (step >= 4) {
      p_right <- p_right +
        step_layer(geom_errorbar, "new",
                   data = data.frame(xmin = phat - me, xmax = phat + me, y = Y_CI),
                   mapping = aes(xmin = xmin, xmax = xmax, y = y),
                   width = 0.12, linewidth = 2.2) +
        ch4_role_text(phat, Y_CI - 0.13, "95% CI", role = "new",
                   size = 5)
    }

    library(patchwork)
    p_left + p_right + plot_layout(widths = c(1, 2.5))
  }))

  output$ch4_step_text <- renderUI({
    step <- ch4_step()
    samp <- ch4_step_sample()

    n <- length(samp)
    x <- sum(samp)
    phat <- x / n
    z_star <- qnorm(0.975)
    se <- sqrt(phat * (1 - phat) / n)
    me <- z_star * se

    switch(as.character(step),
      "1" = tagList(
        p(n, " odpowiedzi: ", x, " razy TAK, ", n - x, " razy NIE.")
      ),
      "2" = tagList(
        p(withMathJax(paste0("\\(\\hat{p} = \\frac{x}{n} = \\frac{", x, "}{", n,
                             "} = ", round(phat, 3), "\\)")))
      ),
      "3" = tagList(
        p(withMathJax(paste0(
          "\\(SE = \\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}} = \\sqrt{\\frac{",
          round(phat, 2), " \\cdot ", round(1 - phat, 2), "}{", n, "}} = ",
          round(se, 3), "\\)"))),
        p("Pasek ± SE to jeden błąd standardowy w każdą stronę. Przedział 95%
          sięga 1.96 SE.")
      ),
      "4" = tagList(
        p(withMathJax(paste0("\\(ME = z^* \\cdot SE = 1.96 \\cdot ",
                             round(se, 3), " = ", round(me, 3), "\\)"))),
        p("95% CI: ",
          tags$b(paste0("[", round(phat - me, 3), " ; ", round(phat + me, 3), "]")))
      )
    )
  })

  # ==========================================================================
  # WIDGET 2: Budowa CI dla różnicy proporcji
  # ==========================================================================
  # Krok widgetu (1..5) żyje w przeglądarce; nowe próby nie zmieniają kroku.
  ch4_dstep <- lc_step_server("ch4_dstep", input)$step

  generate_dstep_prop_samples <- function() {
    set.seed(sample.int(.Machine$integer.max, 1))
    n1 <- 60; n2 <- 60
    list(
      g1 = rbinom(n1, 1, 0.70),  # grupa 1: 70% zadowolonych
      g2 = rbinom(n2, 1, 0.50)   # grupa 2: 50% zadowolonych
    )
  }
  ch4_dstep_samples <- reactiveVal(generate_dstep_prop_samples())

  observeEvent(input$ch4_dstep_new_sample, {
    ch4_dstep_samples(generate_dstep_prop_samples())
  })

  zoom_plot_server("ch4_dstep_plot", reactive({
    step <- ch4_dstep()
    samples <- ch4_dstep_samples()

    g1 <- samples$g1; g2 <- samples$g2
    n1 <- length(g1); n2 <- length(g2)
    x1 <- sum(g1);    x2 <- sum(g2)
    p1 <- x1 / n1;    p2 <- x2 / n2
    diff_val <- p1 - p2
    se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
    z_star <- qnorm(0.975)
    me <- z_star * se

    # ---- LEWY PANEL: słupki TAK/NIE × 2 grupy ----
    # Bez legendy: kategorie TAK/NIE na osi X, grupy w panelach.
    bar_df <- data.frame(
      grp = factor(rep(c("Grupa 1", "Grupa 2"), each = 2),
                   levels = c("Grupa 1", "Grupa 2")),
      val = factor(rep(c("NIE", "TAK"), 2), levels = c("NIE", "TAK")),
      count = c(n1 - x1, x1, n2 - x2, x2)
    )
    p_left <- ggplot(bar_df, aes(x = val, y = count)) +
      step_result(geom_col, width = 0.65,
                  fill = ch4_yes_no_fill[as.character(bar_df$val)]) +
      geom_text(aes(label = count), vjust = -0.4, fontface = "bold",
                family = "mono", size = 4.5, colour = STEP_ROLES$known$colour) +
      facet_wrap(~grp, nrow = 1) +
      labs(x = NULL, y = "Liczebność") +
      step_frame(xlim = c(0.4, 2.6), ylim = c(0, max(bar_df$count) * 1.2)) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())

    # ---- PRAWY GÓRNY PANEL: dwie p_hat na osi proporcji ----
    p_top <- ggplot() +
      scale_y_continuous(breaks = c(1, 2), labels = c("Grupa 1", "Grupa 2")) +
      labs(x = "Proporcja", y = NULL) +
      step_frame(xlim = c(0, 1), ylim = c(0.4, 2.6)) +
      theme(axis.text.y = element_text(face = "bold", size = 12),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    if (step >= 2) {
      role <- step_role(step, 2)
      p_top <- p_top +
        step_layer(geom_point, role, data = data.frame(x = c(p1, p2), y = c(1, 2)),
                   mapping = aes(x = x, y = y), size = 7, shape = 18) +
        ch4_role_text(p1, 1.45, paste0("p̂₁ = ", round(p1, 3)), role = role,
                   size = 4.5) +
        ch4_role_text(p2, 2.45, paste0("p̂₂ = ", round(p2, 3)), role = role,
                   size = 4.5)
    }

    # ---- PRAWY DOLNY PANEL: różnica + CI ----
    xlims_bot <- range(c(-0.5, 0.5, diff_val - 1.3 * me, diff_val + 1.3 * me))
    pad_bot <- diff(xlims_bot) * 0.08
    xlims_bot <- c(xlims_bot[1] - pad_bot, xlims_bot[2] + pad_bot)

    p_bot <- ggplot() +
      step_line("known", xintercept = 0) +
      ch4_role_text(0, 0.45, "0 = brak różnicy", role = "known",
                 hjust = -0.1, size = 4) +
      labs(x = "Różnica proporcji  —  Grupa 1 − Grupa 2",
           y = NULL) +
      step_frame(xlim = xlims_bot, ylim = c(-0.55, 0.55), y_axis = FALSE)

    if (step >= 3) {
      role <- step_role(step, 3)
      p_bot <- p_bot +
        step_layer(geom_point, role, data = data.frame(x = diff_val, y = 0),
                   mapping = aes(x = x, y = y), size = 7, shape = 18) +
        ch4_role_text(diff_val, -0.22, paste0("p̂₁ − p̂₂ = ", round(diff_val, 3)),
                   role = role, size = 4.5)
    }

    if (step >= 4) {
      role <- step_role(step, 4)
      p_bot <- p_bot +
        step_layer(geom_errorbar, role,
                   data = data.frame(xmin = diff_val - se, xmax = diff_val + se, y = 0),
                   mapping = aes(xmin = xmin, xmax = xmax, y = y),
                   width = 0.08, linewidth = ch4_bar_lw(role)) +
        ch4_role_text(diff_val, 0.17, paste0("± SE = ±", round(se, 3)), role = role,
                   size = 4)
    }

    if (step >= 5) {
      p_bot <- p_bot +
        step_layer(geom_errorbar, "new",
                   data = data.frame(xmin = diff_val - me, xmax = diff_val + me, y = 0),
                   mapping = aes(xmin = xmin, xmax = xmax, y = y),
                   width = 0.14, linewidth = 2.2, alpha = 0.6) +
        ch4_role_text(diff_val, -0.42,
                   paste0("95% CI: [", round(diff_val - me, 3),
                          " ; ", round(diff_val + me, 3), "]"),
                   role = "new", size = 4.8)
    }

    library(patchwork)
    # Layout: lewy słupki | (prawy góra p_hat / prawy dół różnica)
    right_col <- p_top / p_bot + plot_layout(heights = c(1, 1))
    (p_left | right_col) + plot_layout(widths = c(1, 2))
  }))

  output$ch4_dstep_text <- renderUI({
    step <- ch4_dstep()
    samples <- ch4_dstep_samples()

    g1 <- samples$g1; g2 <- samples$g2
    n1 <- length(g1); n2 <- length(g2)
    x1 <- sum(g1);    x2 <- sum(g2)
    p1 <- x1 / n1;    p2 <- x2 / n2
    diff_val <- p1 - p2
    se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
    z_star <- qnorm(0.975)
    me <- z_star * se

    switch(as.character(step),
      "1" = tagList(
        p("Grupa 1: ", x1, " TAK i ", n1 - x1, " NIE. Grupa 2: ",
          x2, " TAK i ", n2 - x2, " NIE.")
      ),
      "2" = tagList(
        p(withMathJax(paste0("\\(\\hat{p}_1 = ", x1, "/", n1, " = ", round(p1, 3),
                             " \\qquad \\hat{p}_2 = ", x2, "/", n2, " = ",
                             round(p2, 3), "\\)")))
      ),
      "3" = tagList(
        p(withMathJax(paste0("\\(\\hat{p}_1 - \\hat{p}_2 = ", round(p1, 3),
                             " - ", round(p2, 3), " = ",
                             round(diff_val, 3), "\\)"))),
        p("Dolny panel ma skalę różnicy. Linia w zerze oznacza brak różnicy.")
      ),
      "4" = tagList(
        p(withMathJax(paste0(
          "\\(SE = \\sqrt{\\frac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\frac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = ",
          round(se, 3), "\\)")))
      ),
      "5" = {
        lower <- diff_val - me
        upper <- diff_val + me
        tagList(
          p(withMathJax(paste0("\\(ME = z^* \\cdot SE = 1.96 \\cdot ",
                               round(se, 3), " = ", round(me, 3), "\\)"))),
          p("95% CI: ",
            tags$b(paste0("[", round(lower, 3), " ; ", round(upper, 3), "]"))),
          p(if (lower > 0)
              paste0("Przedział nie obejmuje 0: w grupie 1 odsetek TAK jest ",
                     "wyższy, z 95% ufnością o co najmniej ", round(lower, 3), ".")
            else if (upper < 0)
              paste0("Przedział nie obejmuje 0: w grupie 2 odsetek TAK jest ",
                     "wyższy, z 95% ufnością o co najmniej ", round(-upper, 3), ".")
            else
              "Przedział obejmuje 0: te próby nie rozstrzygają, która grupa ma wyższy odsetek.")
        )
      }
    )
  })

  # ==========================================================================
  # WIDGET 3: CASE STUDIES (konstruktory krok po kroku + hipotezy)
  # ==========================================================================

  # ---- Helpery statystyczne ----
  ci_prop <- function(x, n, conf = 0.95) {
    phat <- x / n
    z_star <- qnorm(1 - (1 - conf) / 2)
    se <- sqrt(phat * (1 - phat) / n)
    me <- z_star * se
    list(phat = phat, lower = phat - me, upper = phat + me,
         me = me, se = se, z_star = z_star)
  }
  ci_diff_props <- function(x1, n1, x2, n2, conf = 0.95) {
    p1 <- x1 / n1; p2 <- x2 / n2
    se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
    z_star <- qnorm(1 - (1 - conf) / 2)
    diff <- p1 - p2
    me <- z_star * se
    list(diff = diff, lower = diff - me, upper = diff + me,
         me = me, se = se, z_star = z_star, p1 = p1, p2 = p2)
  }

  # ---- Werdykt hipotezy ----
  # dir = "gt" (CI > bound), "lt" (CI < bound)
  hypothesis_verdict <- function(lower, upper, bound, dir) {
    if (dir == "gt") {
      if (lower > bound)      "yes"
      else if (upper < bound) "no"
      else                    "maybe"
    } else {
      if (upper < bound)      "yes"
      else if (lower > bound) "no"
      else                    "maybe"
    }
  }

  verdict_class <- function(v) {
    switch(v, "yes" = "ok", "no" = "danger",
           "maybe" = "warning")
  }
  verdict_label <- function(v) {
    switch(v, "yes" = "TAK", "no" = "NIE", "maybe" = "NIEPEWNE")
  }

  col_hyp <- "#8e44ad"

  # ---- CONFIG case'ów ----
  cases_config <- list(
    A1 = list(
      type = "single_prop",
      data = list(x = 212, n = 400),
      xlab = "Poparcie dla partii X",
      steps = c("1. Próba", "2. p̂", "3. ± SE", "4. Przedział"),
      hypotheses = list(
        list(text = "Poparcie dla partii X przekracza 50% (próg większości)",
             bound = 0.50, dir = "gt",
             explain_maybe = "CI (ok. 48–58%) obejmuje 50%. Mimo że p̂ = 53%, niepewność sondażu nie pozwala stwierdzić z 95% ufnością, że poparcie przekracza 50%."),
        list(text = "Poparcie dla partii X przekracza 60%",
             bound = 0.60, dir = "gt",
             explain_no = "Górna granica CI leży poniżej 60%. Cały CI leży poza obszarem hipotezy, więc nie ma podstaw do twierdzenia, że poparcie przekracza 60%.")
      )
    ),
    A2 = list(
      type = "compare_n_prop",
      data = list(phat = 0.08, ns = c(50, 200, 1000)),
      xlab = "Odsetek wadliwych produktów",
      steps = c("1. n = 50", "2. n = 200", "3. n = 1000"),
      hypotheses = list(
        list(text = "Odsetek wadliwych produktów przekracza 5%",
             bound = 0.05, dir = "gt",
             explain_yes = "Dla największej próby (n = 1000) dolna granica CI leży powyżej 5%, więc z 95% ufnością odsetek wadliwych przekracza normę 5%. Dla n = 50 i n = 200 CI obejmuje 5%, więc na mniejszej próbie nie dałoby się tego stwierdzić."),
        list(text = "Odsetek wadliwych produktów przekracza 12%",
             bound = 0.12, dir = "gt",
             explain_no = "Dla n = 1000 górna granica CI leży poniżej 12%. Dla n = 50 CI sięga prawie 16%, więc na małej próbie tej hipotezy nie dałoby się wykluczyć. Duże n daje bardziej jednoznaczne odpowiedzi.")
      )
    ),
    B1 = list(
      type = "diff_props",
      data = list(x1 = 124, n1 = 200, x2 = 84, n2 = 200,
                  label1 = "Lek", label2 = "Placebo"),
      xlab = "Odsetek z ustąpieniem bólu",
      steps = c("1. Próby", "2. Dwie p̂", "3. Różnica", "4. ± SE", "5. Przedział"),
      hypotheses = list(
        list(text = "Lek działa skuteczniej niż placebo (różnica > 0)",
             bound = 0, dir = "gt",
             explain_yes = "Cały CI dla różnicy leży powyżej 0. Lek pomaga skuteczniej niż placebo, różnica jest istotna statystycznie."),
        list(text = "Lek poprawia skuteczność o więcej niż 25 punktów procentowych",
             bound = 0.25, dir = "gt",
             explain_maybe = "CI dla różnicy (ok. 10–30 punktów procentowych) obejmuje 25 punktów. Lek działa, ale dane nie rozstrzygają, czy poprawa względem placebo przekracza 25 punktów procentowych.")
      )
    ),
    B2 = list(
      type = "diff_props",
      data = list(x1 = 22, n1 = 250, x2 = 18, n2 = 250,
                  label1 = "Linia A", label2 = "Linia B"),
      xlab = "Odsetek wadliwych",
      steps = c("1. Próby", "2. Dwie p̂", "3. Różnica", "4. ± SE", "5. Przedział"),
      hypotheses = list(
        list(text = "Linia A produkuje więcej braków niż linia B (różnica > 0)",
             bound = 0, dir = "gt",
             explain_maybe = "CI dla różnicy obejmuje 0. Mimo że p̂₁ (8.8%) jest wyższe niż p̂₂ (7.2%), nie możemy z 95% ufnością stwierdzić, że linia A jest gorsza. Różnica może być efektem przypadku."),
        list(text = "Linia A ma najwyżej o 5 punktów procentowych więcej braków niż B (różnica < 0.05)",
             bound = 0.05, dir = "lt",
             explain_maybe = "Górna granica CI (ok. 6.4 punktu procentowego) przekracza 5 punktów, więc nie możemy wykluczyć, że linia A jest gorsza o więcej niż 5 punktów procentowych. Żeby to rozstrzygnąć, potrzebna byłaby większa próba.")
      )
    ),
    B3 = list(
      type = "diff_props",
      data = list(x1 = 6, n1 = 30, x2 = 9, n2 = 30,
                  label1 = "Zakład A", label2 = "Zakład B"),
      xlab = "Odsetek wypadków",
      steps = c("1. Próby", "2. Dwie p̂", "3. Różnica", "4. ± SE", "5. Przedział"),
      hypotheses = list(
        list(text = "Zakład A jest bezpieczniejszy niż B (różnica < 0)",
             bound = 0, dir = "lt",
             explain_maybe = "Mimo że p̂₁ = 20% jest wyraźnie mniejsze od p̂₂ = 30%, CI dla różnicy obejmuje 0. Próba 30 osób w każdym zakładzie to za mało, żeby z 95% ufnością stwierdzić, który jest bezpieczniejszy. To klasyczna pułapka: „duża” różnica w punktach procentowych może być statystycznie nieistotna przy małej próbie."),
        list(text = "Zakład A ma wypadkowość wyższą o ponad 30 punktów procentowych (różnica > 0.30)",
             bound = 0.30, dir = "gt",
             explain_no = "Górna granica CI (ok. 12 punktów procentowych) leży poniżej 30, więc dane wykluczają, że A jest aż o 30 punktów gorszy od B. W drugą stronę przedział sięga ok. -32 punktów: dużej przewagi B nad A wykluczyć nie można. Mała próba daje przedział zbyt szeroki, żeby wskazać, jaka jest różnica.")
      )
    ),
    C1 = list(
      type = "forest_prop",
      data = list(
        groups = c("Szpital A", "Szpital B", "Szpital C", "Szpital D"),
        x = c(12, 18, 9, 35),
        n = c(150, 180, 160, 170)
      ),
      xlab = "Odsetek powikłań pooperacyjnych",
      steps = c("1. Liczby", "2. Proporcje", "3. CI"),
      hypotheses = list(
        list(kind = "pairwise",
             text = "Które szpitale różnią się istotnie odsetkiem powikłań?",
             unit = "")
      )
    )
  )


  # ---- Helper: pasek CI dla pojedynczej proporcji (słupki + panel CI) ----
  plot_single_prop_step <- function(data, step, xlab,
                                     hypothesis = NULL, title = NULL) {
    x <- data$x; n <- data$n
    ci <- ci_prop(x, n)
    phat <- ci$phat; se <- ci$se; me <- ci$me

    # ---- LEWY PANEL: słupki TAK / NIE ----
    bar_df <- data.frame(
      val = factor(c("NIE", "TAK"), levels = c("NIE", "TAK")),
      count = c(n - x, x)
    )
    p_left <- ggplot(bar_df, aes(x = val, y = count, fill = val)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = count), vjust = -0.4, fontface = "bold",
                size = 5, color = upwr_secondary) +
      scale_fill_manual(values = c("NIE" = col_miss, "TAK" = col_ci),
                        guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "Liczebność") +
      theme_upwr() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())

    # ---- PRAWY PANEL: oś proporcji ----
    xlims <- c(0, 1)
    if (!is.null(hypothesis)) {
      xlims <- range(c(xlims, hypothesis$bound))
      xlims[1] <- max(0, xlims[1])
      xlims[2] <- min(1, xlims[2])
    }

    p_right <- ggplot() +
      xlim(xlims) +
      ylim(-0.6, 0.6) +
      labs(x = xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Obszar hipotezy
    if (!is.null(hypothesis)) {
      if (hypothesis$dir == "gt") {
        p_right <- p_right + annotate("rect",
                          xmin = hypothesis$bound, xmax = Inf,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      } else {
        p_right <- p_right + annotate("rect",
                          xmin = -Inf, xmax = hypothesis$bound,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      }
      p_right <- p_right +
        geom_vline(xintercept = hypothesis$bound, color = col_hyp,
                   linewidth = 1, linetype = "solid") +
        annotate("text", x = hypothesis$bound, y = 0.5,
                 label = paste0(if (hypothesis$dir == "gt") "≥ " else "≤ ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    }

    # Krok 2+: punkt p_hat
    if (step >= 2) {
      p_right <- p_right +
        geom_point(aes(x = phat, y = 0), color = col_estimate, size = 7, shape = 18) +
        annotate("text", x = phat, y = -0.22,
                 label = paste0("p̂ = ", round(phat, 3)),
                 color = col_estimate, fontface = "bold", size = 4.8)
    }

    # Krok 3+: SE
    if (step >= 3) {
      p_right <- p_right +
        geom_errorbarh(aes(xmin = phat - se, xmax = phat + se, y = 0),
                       height = 0.08, color = col_hit, linewidth = 1.8) +
        annotate("text", x = phat, y = 0.20,
                 label = paste0("± SE = ±", round(se, 3)),
                 color = col_hit, fontface = "bold", size = 4)
    }

    # Krok 4: CI
    if (step >= 4) {
      p_right <- p_right +
        geom_errorbarh(aes(xmin = phat - me, xmax = phat + me, y = 0),
                       height = 0.14, color = col_ci, linewidth = 2.2, alpha = 0.6) +
        annotate("text", x = phat, y = -0.45,
                 label = paste0("95% CI: [", round(phat - me, 3),
                                " ; ", round(phat + me, 3), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    }

    library(patchwork)
    p_left + p_right + plot_layout(widths = c(1, 2.5))
  }

  # ---- Plot dla compare_n_prop (te same dane, różne n) ----
  plot_compare_n_prop_step <- function(data, step, xlab,
                                        hypothesis = NULL, title = NULL) {
    phat <- data$phat
    ns <- data$ns
    k <- length(ns)

    # CI dla każdego n
    cis <- lapply(ns, function(n) {
      x <- round(phat * n)
      ci_prop(x, n)
    })

    # Limity X
    xmin <- min(sapply(cis, function(c) c$lower))
    xmax <- max(sapply(cis, function(c) c$upper))
    xlims <- c(max(0, xmin - 0.05), min(1, xmax + 0.05))
    if (!is.null(hypothesis)) {
      xlims <- range(c(xlims, hypothesis$bound))
      xlims[1] <- max(0, xlims[1])
      xlims[2] <- min(1, xlims[2])
    }

    y_positions <- seq_len(k)
    df <- data.frame(
      y = y_positions,
      n = ns,
      phat = sapply(cis, function(c) c$phat),
      lower = sapply(cis, function(c) c$lower),
      upper = sapply(cis, function(c) c$upper),
      label = paste0("n = ", ns)
    )

    p <- ggplot() +
      xlim(xlims) +
      ylim(0.3, k + 0.7) +
      labs(x = xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    p <- p +
      annotate("text", x = xlims[1], y = y_positions,
               label = df$label, hjust = 0, fontface = "bold", size = 4.5,
               color = upwr_secondary)

    if (!is.null(hypothesis)) {
      if (hypothesis$dir == "gt") {
        p <- p + annotate("rect",
                          xmin = hypothesis$bound, xmax = Inf,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      } else {
        p <- p + annotate("rect",
                          xmin = -Inf, xmax = hypothesis$bound,
                          ymin = -Inf, ymax = Inf,
                          fill = col_hyp, alpha = 0.15)
      }
      p <- p +
        geom_vline(xintercept = hypothesis$bound, color = col_hyp,
                   linewidth = 1, linetype = "solid") +
        annotate("text", x = hypothesis$bound, y = k + 0.45,
                 label = paste0(if (hypothesis$dir == "gt") "≥ " else "≤ ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4.5, hjust = -0.1)
    }

    # Pokazuj progresywnie: krok i = pierwsze i CI
    rows_df <- df[seq_len(min(step, k)), , drop = FALSE]
    if (nrow(rows_df) > 0) {
      p <- p +
        geom_point(data = rows_df, aes(x = phat, y = y),
                   color = col_estimate, size = 5, shape = 18) +
        geom_errorbarh(data = rows_df,
                       aes(xmin = lower, xmax = upper, y = y),
                       height = 0.18, color = col_ci, linewidth = 1.8, alpha = 0.7) +
        geom_text(data = rows_df,
                  aes(x = (lower + upper) / 2, y = y - 0.22,
                      label = paste0("[", round(lower, 3), " ; ", round(upper, 3), "]")),
                  color = col_ci, size = 3.8, fontface = "bold")
    }

    p
  }

  # ---- Plot dla diff_props (porównanie dwóch grup, słupki + 2 panele CI) ----
  plot_diff_props_step <- function(data, step, xlab,
                                    hypothesis = NULL, title = NULL) {
    x1 <- data$x1; n1 <- data$n1
    x2 <- data$x2; n2 <- data$n2
    label1 <- data$label1; label2 <- data$label2

    cd <- ci_diff_props(x1, n1, x2, n2)
    p1 <- cd$p1; p2 <- cd$p2
    diff_val <- cd$diff
    se <- cd$se; me <- cd$me

    # ---- LEWY PANEL: słupki TAK/NIE × 2 grupy ----
    bar_df <- data.frame(
      grp = factor(rep(c(label1, label2), each = 2),
                   levels = c(label1, label2)),
      val = factor(rep(c("NIE", "TAK"), 2), levels = c("NIE", "TAK")),
      count = c(n1 - x1, x1, n2 - x2, x2)
    )
    p_left <- ggplot(bar_df, aes(x = grp, y = count, fill = val)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.65) +
      geom_text(aes(label = count),
                position = position_dodge(width = 0.75),
                vjust = -0.4, fontface = "bold", size = 4.2, color = upwr_secondary) +
      scale_fill_manual(values = c("NIE" = col_miss, "TAK" = col_ci),
                        name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(x = NULL, y = "Liczebność") +
      theme_upwr() +
      theme(legend.position = "top",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())

    # ---- PRAWY GÓRNY PANEL: dwie p_hat na osi proporcji ----
    p_top <- ggplot() +
      xlim(0, 1) +
      ylim(0.4, 2.6) +
      labs(x = xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_text(face = "bold", size = 11),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      scale_y_continuous(breaks = c(1, 2), labels = c(label1, label2),
                         limits = c(0.4, 2.6))

    if (step >= 2) {
      p_top <- p_top +
        geom_point(aes(x = p1, y = 1), color = col_estimate, size = 7, shape = 18) +
        annotate("text", x = p1, y = 1.45, label = paste0("p̂₁ = ", round(p1, 3)),
                 color = col_estimate, fontface = "bold", size = 4.2) +
        geom_point(aes(x = p2, y = 2), color = col_estimate, size = 7, shape = 18) +
        annotate("text", x = p2, y = 2.45, label = paste0("p̂₂ = ", round(p2, 3)),
                 color = col_estimate, fontface = "bold", size = 4.2)
    }

    # ---- PRAWY DOLNY PANEL: różnica + CI + obszar hipotezy ----
    xlims_bot <- range(c(-0.3, 0.3, diff_val - 1.3 * me, diff_val + 1.3 * me))
    if (!is.null(hypothesis)) {
      xlims_bot <- range(c(xlims_bot, hypothesis$bound))
    }
    pad_bot <- diff(xlims_bot) * 0.08
    xlims_bot <- c(xlims_bot[1] - pad_bot, xlims_bot[2] + pad_bot)

    col_true_local <- "#9b59b6"

    p_bot <- ggplot() +
      xlim(xlims_bot) +
      ylim(-0.55, 0.55) +
      labs(x = paste0("Różnica proporcji  —  ", label1, " − ", label2),
           y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      geom_vline(xintercept = 0, color = col_true_local,
                 linewidth = 1, linetype = "dashed") +
      annotate("text", x = 0, y = 0.45, label = "0 = brak różnicy",
               color = col_true_local, fontface = "bold", size = 4, hjust = -0.1)

    # Obszar hipotezy
    if (!is.null(hypothesis)) {
      if (hypothesis$dir == "gt") {
        p_bot <- p_bot + annotate("rect",
                                  xmin = hypothesis$bound, xmax = Inf,
                                  ymin = -Inf, ymax = Inf,
                                  fill = col_hyp, alpha = 0.15)
      } else {
        p_bot <- p_bot + annotate("rect",
                                  xmin = -Inf, xmax = hypothesis$bound,
                                  ymin = -Inf, ymax = Inf,
                                  fill = col_hyp, alpha = 0.15)
      }
      p_bot <- p_bot +
        geom_vline(xintercept = hypothesis$bound, color = col_hyp,
                   linewidth = 1, linetype = "solid") +
        annotate("text", x = hypothesis$bound, y = 0.45,
                 label = paste0(if (hypothesis$dir == "gt") "≥ " else "≤ ",
                                hypothesis$bound),
                 color = col_hyp, fontface = "bold", size = 4, hjust = -0.1)
    }

    if (step >= 3) {
      p_bot <- p_bot +
        geom_point(aes(x = diff_val, y = 0), color = col_estimate,
                   size = 7, shape = 18) +
        annotate("text", x = diff_val, y = -0.22,
                 label = paste0("p̂₁ − p̂₂ = ", round(diff_val, 3)),
                 color = col_estimate, fontface = "bold", size = 4.5)
    }

    if (step >= 4) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - se, xmax = diff_val + se, y = 0),
                       height = 0.08, color = col_hit, linewidth = 1.8) +
        annotate("text", x = diff_val, y = 0.17,
                 label = paste0("± SE = ±", round(se, 3)),
                 color = col_hit, fontface = "bold", size = 4)
    }

    if (step >= 5) {
      p_bot <- p_bot +
        geom_errorbarh(aes(xmin = diff_val - me, xmax = diff_val + me, y = 0),
                       height = 0.14, color = col_ci, linewidth = 2.2, alpha = 0.6) +
        annotate("text", x = diff_val, y = -0.42,
                 label = paste0("95% CI: [", round(diff_val - me, 3),
                                " ; ", round(diff_val + me, 3), "]"),
                 color = col_ci, fontface = "bold", size = 4.8)
    }

    library(patchwork)
    right_col <- p_top / p_bot + plot_layout(heights = c(1, 1))
    (p_left | right_col) +
      plot_layout(widths = c(1, 2))
  }

  # ---- Plot dla forest_prop (wiele grup, proporcje) ----
  plot_forest_prop_step <- function(data, step, xlab, hypothesis = NULL) {
    groups <- data$groups
    xs <- data$x
    ns <- data$n
    k <- length(groups)

    ci_list <- lapply(seq_len(k), function(i) ci_prop(xs[i], ns[i]))
    all_phats <- sapply(ci_list, function(c) c$phat)
    all_lowers <- sapply(ci_list, function(c) c$lower)
    all_uppers <- sapply(ci_list, function(c) c$upper)

    xlims <- range(c(all_lowers, all_uppers))
    xlims[1] <- max(0, xlims[1] - 0.03)
    xlims[2] <- min(1, xlims[2] + 0.03)

    y_positions <- seq_len(k)
    group_df <- data.frame(group = groups, y = y_positions,
                            phat = all_phats, lower = all_lowers,
                            upper = all_uppers,
                            label = paste0(xs, "/", ns))

    p <- ggplot() +
      xlim(xlims) +
      ylim(0.3, k + 0.7) +
      labs(x = xlab, y = NULL) +
      theme_upwr() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    # Etykiety grup
    p <- p +
      annotate("text", x = xlims[1], y = y_positions,
               label = groups, hjust = 0, fontface = "bold", size = 4.5,
               color = upwr_secondary)

    # Krok 1+: surowe liczby x/n
    if (step >= 1) {
      p <- p +
        annotate("text", x = xlims[2], y = y_positions,
                 label = group_df$label, hjust = 1, size = 4,
                 color = upwr_secondary, fontface = "italic")
    }

    # Krok 2+: punkty p_hat
    if (step >= 2) {
      p <- p + geom_point(data = group_df, aes(x = phat, y = y),
                          color = col_estimate, size = 6, shape = 18)
    }
    # Krok 3+: CI
    if (step >= 3) {
      p <- p + geom_errorbarh(data = group_df,
                               aes(xmin = lower, xmax = upper, y = y),
                               height = 0.18, color = col_ci, linewidth = 1.8)
    }

    p
  }

  # ---- Liczba "core" kroków budowy CI (bez hipotez) ----
  n_core_steps <- function(cfg) length(cfg$steps)
  # ---- Wykres case'a: krok budowy CI i (na ostatnim kroku) obszar hipotezy ----
  render_case_plot <- function(cfg, step, hyp_idx) {
    hypothesis <- NULL
    plot_step <- step
    if (!is.null(hyp_idx)) {
      hyp_obj <- cfg$hypotheses[[hyp_idx]]
      # Hipoteza pairwise (forest) nie ma bound/dir — nie rysujemy obszaru.
      if (is.null(hyp_obj$kind) || hyp_obj$kind != "pairwise") {
        hypothesis <- hyp_obj
      }
    }

    switch(cfg$type,
      "single_prop"     = plot_single_prop_step(cfg$data, plot_step, cfg$xlab,
                                                 hypothesis = hypothesis),
      "compare_n_prop"  = plot_compare_n_prop_step(cfg$data, plot_step, cfg$xlab,
                                                    hypothesis = hypothesis),
      "diff_props"      = plot_diff_props_step(cfg$data, plot_step, cfg$xlab,
                                                hypothesis = hypothesis),
      "forest_prop"     = plot_forest_prop_step(cfg$data, plot_step, cfg$xlab,
                                                 hypothesis = hypothesis)
    )
  }

  # ---- Pairwise: macierz nakładania CI dla forest_prop ----
  forest_prop_pairwise_matrix <- function(data) {
    k <- length(data$groups)
    cis <- lapply(seq_len(k), function(i) ci_prop(data$x[i], data$n[i]))
    m <- matrix(FALSE, nrow = k, ncol = k,
                dimnames = list(data$groups, data$groups))
    for (i in seq_len(k)) for (j in seq_len(k)) {
      if (i == j) next
      m[i, j] <- (cis[[i]]$upper < cis[[j]]$lower) ||
                 (cis[[j]]$upper < cis[[i]]$lower)
    }
    m
  }

  render_pairwise_table <- function(mat) {
    groups <- rownames(mat)
    k <- length(groups)
    header <- tags$tr(
      tags$th(""),
      lapply(groups, function(g) tags$th(g, style = "padding: 4px 8px; text-align: center; font-size: 12px;"))
    )
    rows <- lapply(seq_len(k), function(i) {
      tags$tr(
        tags$th(groups[i], style = "padding: 4px 8px; text-align: right; font-size: 12px;"),
        lapply(seq_len(k), function(j) {
          if (i == j) {
            tags$td("—", style = "padding: 4px 8px; text-align: center; color: var(--upwr-reference);")
          } else if (mat[i, j]) {
            tags$td("✓", style = "padding: 4px 8px; text-align: center; color: var(--upwr-sage); font-weight: bold; font-size: 16px;")
          } else {
            tags$td("×", style = "padding: 4px 8px; text-align: center; color: var(--upwr-accent); font-size: 16px;")
          }
        })
      )
    })
    tags$table(
      style = "border-collapse: collapse; margin: 8px auto; border: 1px solid var(--upwr-rule);",
      tags$thead(header),
      tags$tbody(rows)
    )
  }

  # Narracja "jak w raporcie" dla pairwise (proporcje, prezentacja w %)
  pairwise_narrative <- function(data, mat) {
    groups <- data$groups
    phats <- data$x / data$n
    k <- length(groups)

    diff_pairs <- list()
    for (i in seq_len(k - 1)) for (j in seq(i + 1, k)) {
      if (mat[i, j]) {
        if (phats[i] > phats[j]) {
          diff_pairs[[length(diff_pairs) + 1]] <- list(hi = groups[i], lo = groups[j])
        } else {
          diff_pairs[[length(diff_pairs) + 1]] <- list(hi = groups[j], lo = groups[i])
        }
      }
    }
    n_diff <- length(diff_pairs)

    if (n_diff == 0) {
      return(paste0(
        "Żadna para grup nie wykazała istotnej różnicy w odsetkach — ",
        "wszystkie 95% CI nakładają się wzajemnie. Na podstawie tych danych ",
        "nie możemy stwierdzić różnic między grupami."
      ))
    }

    # Czy jedna grupa odstaje od WSZYSTKICH innych?
    standout_idx <- which(sapply(seq_len(k), function(i) all(mat[i, -i])))
    if (length(standout_idx) == 1) {
      i <- standout_idx
      others <- phats[-i]
      direction <- if (phats[i] > max(others)) "wyższy" else "niższy"
      return(paste0(
        "Spośród wszystkich badanych grup wyraźnie odstaje ",
        tags$b(groups[i]), " (odsetek ", round(phats[i] * 100, 1), "%) — ma istotnie ",
        direction, " odsetek niż każda z pozostałych grup ",
        "(jego 95% CI nie nakłada się z żadnym innym). ",
        "Pozostałe grupy mają odsetki w przedziale ",
        round(min(others) * 100, 1), "%–", round(max(others) * 100, 1), "%, ",
        "a ich CI nakładają się — nie możemy stwierdzić między nimi istotnych różnic."
      ))
    }

    pair_strs <- sapply(diff_pairs, function(pp) {
      paste0(tags$b(pp$hi), " > ", tags$b(pp$lo))
    })
    pairs_inline <- if (length(pair_strs) == 1) {
      pair_strs[1]
    } else if (length(pair_strs) == 2) {
      paste(pair_strs, collapse = " oraz ")
    } else {
      paste0(paste(pair_strs[-length(pair_strs)], collapse = ", "),
             " oraz ", pair_strs[length(pair_strs)])
    }

    intro <- if (n_diff == 1) {
      "Spośród wszystkich porównań jedynie jedna para wykazała istotną różnicę: "
    } else {
      paste0("Istotne różnice (CI nie nakładają się) wykazały ",
             n_diff, " pary: ")
    }

    paste0(
      intro, pairs_inline, ". ",
      "Pozostałe pary nie różnią się istotnie — ich 95% CI nakładają się, ",
      "więc na podstawie tych danych nie możemy między nimi rozróżnić."
    )
  }

  # ---- Opis hipotezy i werdykt pod widgetem case'a ----
  render_case_explain <- function(cfg, hyp_idx, revealed, widget_id) {
    if (is.null(hyp_idx)) return(NULL)
    hyp <- cfg$hypotheses[[hyp_idx]]

    # Najpierw sama treść hipotezy (czas na dyskusję), werdykt po kliknięciu.
    if (!revealed) {
      return(lc_status(
        p(tags$strong(paste0("Hipoteza ", hyp_idx, ":")), " ", hyp$text),
        p("Spójrz na wykres: gdzie leży CI względem obszaru hipotezy?
          Co o tym sądzicie?"),
        lc_action(paste0(widget_id, "_reveal"), "Pokaż werdykt", variant = "solid")
      ))
    }

    if (!is.null(hyp$kind) && hyp$kind == "pairwise") {
      mat <- forest_prop_pairwise_matrix(cfg$data)
      narrative <- pairwise_narrative(cfg$data, mat)
      return(lc_status(
        p(tags$strong("Hipoteza:"), " ", hyp$text),
        p(tags$strong("Werdykt — macierz par:")),
        p(tags$em("✓ = grupy różnią się istotnie (CI nie nakładają się);  ",
                  "× = nie można stwierdzić różnicy (CI nakładają się)"),
          style = "font-size: 12px; color: var(--upwr-reference);"),
        render_pairwise_table(mat),
        p(tags$strong("Jak to opisać w raporcie:"),
          style = "margin-top: 12px;"),
        p(HTML(narrative), style = "font-style: italic;")
      ))
    }

    verdict <- compute_verdict_for_case(cfg, hyp)
    label <- verdict_label(verdict)

    body <- if (verdict == "yes" && !is.null(hyp$explain_yes)) {
      p(hyp$explain_yes)
    } else if (verdict == "no" && !is.null(hyp$explain_no)) {
      p(hyp$explain_no)
    } else if (verdict == "maybe" && !is.null(hyp$explain_maybe)) {
      p(hyp$explain_maybe)
    } else {
      p("CI przecina granicę hipotezy — nie możemy jednoznacznie
        stwierdzić, czy jest prawdziwa.")
    }

    lc_status(
      p(tags$strong(paste0("Hipoteza ", hyp_idx, ":")), " ", hyp$text),
      p(tags$strong("Werdykt:"), " ",
        if (verdict %in% c("yes", "no")) lc_verdict(label, type = if (verdict == "yes") "ok" else "danger") else label),
      body
    )
  }

  # ---- Werdykt dla case'a ----
  compute_verdict_for_case <- function(cfg, hyp) {
    switch(cfg$type,
      "single_prop" = {
        ci <- ci_prop(cfg$data$x, cfg$data$n)
        hypothesis_verdict(ci$lower, ci$upper, hyp$bound, hyp$dir)
      },
      "compare_n_prop" = {
        # Werdykt na podstawie największego n (najbardziej precyzyjne CI)
        largest_n <- max(cfg$data$ns)
        x <- round(cfg$data$phat * largest_n)
        ci <- ci_prop(x, largest_n)
        hypothesis_verdict(ci$lower, ci$upper, hyp$bound, hyp$dir)
      },
      "diff_props" = {
        cd <- ci_diff_props(cfg$data$x1, cfg$data$n1, cfg$data$x2, cfg$data$n2)
        hypothesis_verdict(cd$lower, cd$upper, hyp$bound, hyp$dir)
      }
    )
  }

  # ---- Widget krokowy case'a: pasek budowy CI, hipotezy jako przełączniki ----
  register_case <- function(case_id) {
    cfg <- cases_config[[case_id]]
    n_core <- length(cfg$steps)
    widget_id <- paste0("ch4_case", case_id)
    step <- lc_step_server(widget_id, input)$step
    # Hipoteza liczy się tylko na ostatnim kroku (pełny przedział).
    hyp_idx <- reactive({
      h <- input[[paste0(widget_id, "_hyp")]]
      if (is.null(h) || step() < n_core) NULL else as.integer(h)
    })
    revealed <- reactiveVal(FALSE)
    observeEvent(input[[paste0(widget_id, "_hyp")]], revealed(FALSE), ignoreNULL = FALSE)
    observeEvent(input[[paste0(widget_id, "_reveal")]], revealed(TRUE))

    hyp_choices <- stats::setNames(as.character(seq_along(cfg$hypotheses)),
                                   paste("Hipoteza", seq_along(cfg$hypotheses)))
    output[[paste0(widget_id, "_widget")]] <- renderUI({
      lc_step_widget(widget_id,
        steps = sub("^[0-9]+\\.\\s*", "", cfg$steps),
        plot_id = paste0(widget_id, "_plot"),
        ratio = if (cfg$type %in% c("single_prop", "compare_n_prop")) "2.4/1" else "1.6/1",
        toolbar = lc_toolbar(
          lc_step_from(n_core, lc_chips(paste0(widget_id, "_hyp"), hyp_choices, label = "Sprawdź"))
        ),
        extra = uiOutput(paste0(widget_id, "_explain"))
      )
    })
    output[[paste0(widget_id, "_text")]] <- renderUI({
      if (step() == n_core) "Przedział gotowy. Wybierz hipotezę, żeby sprawdzić ją na wykresie."
    })
    zoom_plot_server(paste0(widget_id, "_plot"), reactive(
      render_case_plot(cfg, step(), hyp_idx())
    ))
    output[[paste0(widget_id, "_explain")]] <- renderUI(
      render_case_explain(cfg, hyp_idx(), revealed(), widget_id)
    )
  }

  for (cid in names(cases_config)) {
    register_case(cid)
  }

}
