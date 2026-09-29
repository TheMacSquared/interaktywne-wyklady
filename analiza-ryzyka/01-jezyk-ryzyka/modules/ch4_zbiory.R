# ==========================================================================
# ROZDZIAŁ 4: DZIAŁANIA NA ZDARZENIACH
# ==========================================================================

ch4_ui <- lecture_chapter(
  id = "ch-zbiory",
  num = "04",
  title = "Działania na zdarzeniach",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 04 · Język ryzyka",
      num = "04",
      title = "Skórka lub mokro to nie skórka i mokro.",
      lead = "W raportach bezpieczeństwa słowa „lub”, „i” oraz „nie” zmieniają
              to, co liczymy. Na stu kontrolach korytarza Bananpolu zobaczymy
              działania na zdarzeniach: sumę, część wspólną i dopełnienie."
    ),

    margin_callout(
      label = "Dwa zdarzenia",
      tags$div("A — podczas kontroli znaleziono skórkę na przejściu."),
      tags$div("B — podczas kontroli posadzka była mokra."),
      color = "wskazowka"
    ),

    lc_p(
      "Kierownik zmiany pyta inspektora: „Jak często przejście jest
       niebezpieczne?”. Inspektor ma w notesie dwie osobne kolumny — skórka na
       przejściu i mokra posadzka — z wynikami stu kontroli. Żadna z nich nie
       odpowiada na pytanie wprost. „Niebezpieczne” może znaczyć „skórka lub
       mokro”, a może „skórka i mokro jednocześnie”; każde z tych odczytań daje
       inną liczbę. Zanim cokolwiek policzymy, musimy przetłumaczyć zdanie na
       działanie na zdarzeniach."
    ),

    lc_h2("ch4-jezyk", "Przetłumacz zdanie na zbiór"),
    lc_p(
      "Zdarzenie A ∪ B zachodzi, gdy wystąpiło A lub B, włącznie z sytuacją,
       gdy wystąpiły oba. Zdarzenie A ∩ B wymaga obu warunków naraz. Dopełnienie
       Aᶜ obejmuje wszystkie wyniki, w których A nie zaszło."
    ),
    risk_definition("1.6", "Działania na zdarzeniach", c(
      "Niech A i B będą zdarzeniami w tej samej przestrzeni Ω. Sumą zdarzeń
       A ∪ B nazywamy zdarzenie złożone z wyników należących do A lub do B (lub
       do obu). Iloczynem zdarzeń A ∩ B nazywamy zdarzenie złożone z wyników
       należących jednocześnie do A i do B. Dopełnieniem (zdarzeniem
       przeciwnym) Aᶜ nazywamy zdarzenie złożone z tych wyników Ω, które nie
       należą do A.",
      "Różnicą A \\ B nazywamy zdarzenie złożone z wyników należących do A, ale
       nie do B; zachodzi A \\ B = A ∩ Bᶜ."
    )),
    risk_definition("1.7", "Zdarzenia rozłączne", c(
      "Zdarzenia A i B są rozłączne (wykluczające się), jeśli A ∩ B = ∅, czyli
       nie mogą zajść jednocześnie. Przykład: zdarzenie A i jego dopełnienie Aᶜ
       są zawsze rozłączne, a ich suma to cała przestrzeń: A ∪ Aᶜ = Ω."
    )),
    lc_p(
      "Z ostatniej uwagi wynika najczęściej używany wzór tego kursu. Każdy wynik
       należy albo do A, albo do Aᶜ — nigdy do obu i zawsze do któregoś. W
       definicji klasycznej oznacza to |A| + |Aᶜ| = |Ω|; po podzieleniu przez |Ω|
       dostajemy wzór (1.4). Obserwowaliśmy to już na siatce palet w rozdziale 03:
       P(A) i P(Aᶜ) zawsze sumowały się do 1."
    ),
    risk_formula("P(A^{c})=1-P(A)", num = "1.4",
      legend = c("A^{c}" = "zdarzenie przeciwne do A: A nie zaszło")),
    lc_p(
      "Wzór (1.4) jest szczególnie wygodny, gdy zdarzenie ma postać „co
       najmniej jeden”. Bezpośrednie liczenie takich zdarzeń wymaga zebrania
       wielu przypadków, a jego dopełnienie — „ani jeden” — jest zwykle jednym,
       prostym przypadkiem. Ten trik wróci wielokrotnie, zwłaszcza w wykładach 04
       i 08."
    ),
    lc_p(
      "Suma zdarzeń jest trudniejsza. Kusi, żeby dodać P(A) i P(B), ale wyniki
       należące do obu zdarzeń zostałyby wtedy policzone dwa razy — raz w A i raz
       w B. W definicji klasycznej |A ∪ B| = |A| + |B| − |A ∩ B|, bo część
       wspólną trzeba odjąć raz. Po podzieleniu przez |Ω| dostajemy wzór (1.5)."
    ),

    risk_formula("P(A\\cup B)=P(A)+P(B)-P(A\\cap B)", num = "1.5",
      legend = c(
        "A\\cup B" = "zaszło A lub B (lub oba)",
        "A\\cap B" = "zaszły jednocześnie A i B"
      )),
    lc_p("Część wspólną odejmujemy, ponieważ przy dodawaniu została policzona dwa razy."),

    risk_try("klikaj przycisk pod opisem i obserwuj diagram krok po kroku. Na
      kroku 2 zwróć uwagę, który obszar dostał dwa kolory; na kroku 4 sprawdź,
      dlaczego samo dodawanie przestaje być błędem."),

    figure_panel(
      label = "Demonstracja 1.1",
      title = "Dlaczego nie wystarczy dodać P(A) i P(B)?",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          uiOutput("ch4_venn_explanation"),
          actionButton(
            "ch4_venn_next", "Dodaj P(A) i P(B)",
            class = "lc-btn-primary", width = "100%"
          )
        ),
        column(8, zoom_plot_ui("ch4_venn", height = "430px"))
      )
    ),

    lc_p(
      "Naiwna suma 0,70 + 0,60 = 1,30 łamie własność 0 ≤ P ≤ 1 ze wzoru (1.3) —
       to sygnał, że coś policzono podwójnie. Po odjęciu części wspólnej wynik
       0,90 jest poprawny: to prawdopodobieństwo, że zaszło przynajmniej jedno z
       dwóch zdarzeń. W ostatnim kroku koła się nie stykają, P(A ∩ B) = 0 i
       wzór (1.5) upraszcza się do zwykłego dodawania. Dodawanie prawdopodobieństw
       bez poprawki jest więc poprawne tylko dla zdarzeń rozłącznych
       (definicja 1.7)."
    ),
    risk_example("1.4", "Nocna zmiana lub piątek",
      problem = c(
        "Wróć do losowania zmiany z przykładu 1.3 (|Ω| = 15, A — zmiana nocna,
         B — piątek). Oblicz P(A ∩ B), P(A ∪ B), P(Aᶜ) oraz prawdopodobieństwo,
         że wylosowana zmiana nie jest ani nocna, ani piątkowa."
      ),
      steps = c(
        "A ∩ B = {(pt, nocna)} — jeden wynik, więc P(A ∩ B) = 1/15 ≈ 0,067.",
        "Ze wzoru (1.5): P(A ∪ B) = 5/15 + 3/15 − 1/15 = 7/15 ≈ 0,467. Bez
         odjęcia części wspólnej wyszłoby 8/15 — piątkowa nocka zostałaby
         policzona dwa razy.",
        "Ze wzoru (1.4): P(Aᶜ) = 1 − 5/15 = 10/15 ≈ 0,667.",
        "„Ani A, ani B” to dopełnienie sumy: P((A ∪ B)ᶜ) = 1 − 7/15 = 8/15 ≈ 0,533.
         Sprawdzenie przez wyliczenie: 4 dni pon–czw × 2 zmiany dzienne = 8 wyników."
      ),
      answer = "P(A ∩ B) = 1/15, P(A ∪ B) = 7/15, P(Aᶜ) = 2/3, P(ani A, ani B) = 8/15."
    ),

    lc_h2("ch4-siatka", "Zbuduj dwa zdarzenia na 100 kontrolach"),
    lc_p(
      "Zmieniaj liczebności A i B oraz ich część wspólną. Aplikacja pilnuje, by
       wybrane zbiory mogły zmieścić się w przestrzeni 100 wyników."
    ),
    lc_p(
      "Tym razem przestrzeń to sto kontroli korytarza, a prawdopodobieństwa są
       częstościami z definicji 1.2: P(A) = |A|/100. Każdy kwadrat to jedna
       kontrola i należy do dokładnie jednej z czterech grup — tylko A, tylko B,
       A i B, ani A, ani B. Cztery grupy są parami rozłączne i razem wypełniają
       Ω, dlatego wszystkie wzory tego rozdziału można sprawdzić zwykłym
       liczeniem kwadratów."
    ),
    risk_try("zostaw ustawienia startowe (A = 30, B = 20, część wspólna 8) i
      policz kwadraty w każdym kolorze. Potem zwiększ część wspólną do 20 i
      zmniejsz ją do 0. Na koniec ustaw A = 80 i B = 40 i sprawdź, dlaczego
      suwak części wspólnej nie pozwala zejść poniżej 20."),

    figure_panel(
      label = "Ćwiczenie 4",
      title = "Suma, iloczyn i dopełnienie zdarzeń",
      full_width = TRUE,
      fluidRow(
        column(
          4,
          sliderInput("ch4_n_a", "Liczba kontroli ze zdarzeniem A", 0, 80, 30, 1),
          sliderInput("ch4_n_b", "Liczba kontroli ze zdarzeniem B", 0, 80, 20, 1),
          sliderInput("ch4_overlap", "Liczba kontroli z A i B", 0, 20, 8, 1),
          uiOutput("ch4_stats")
        ),
        column(
          8,
          zoom_plot_ui("ch4_event_grid", height = "480px")
        )
      )
    ),

    lc_p(
      "Przy ustawieniach startowych panel pokazuje P(A ∩ B) = 0,08, P(A ∪ B) =
       0,42, P(Aᶜ) = 0,70 i „ani A, ani B” = 0,58. Sprawdzenie wzorem (1.5):
       0,30 + 0,20 − 0,08 = 0,42. Ostatnia wartość to 1 − 0,42, bo kontrola,
       w której nie było ani skórki, ani mokrej posadzki, jest dokładnie
       dopełnieniem sumy. Tę równoważność zapisują prawa de Morgana."
    ),
    risk_formula("(A\\cup B)^{c}=A^{c}\\cap B^{c},\\qquad (A\\cap B)^{c}=A^{c}\\cup B^{c}", num = "1.6"),
    lc_p(
      "Pierwsze prawo czytamy: „nie zaszło ani A, ani B” to to samo co „nie
       zaszło A i nie zaszło B”. Drugie: „nie zaszły oba naraz” to to samo co
       „nie zaszło A lub nie zaszło B”. W raportach bezpieczeństwa przydaje się
       szczególnie pierwsze — kontrola „bez żadnych uwag” jest dopełnieniem sumy
       wszystkich rodzajów uwag. Suwak części wspólnej ma też ograniczenia: przy
       A = 80 i B = 40 część wspólna musi mieć co najmniej 20 kontroli, bo
       inaczej suma przekroczyłaby 100."
    ),
    risk_check("j1_chk_suma",
      "W 100 kontrolach P(A) = 0,30, P(B) = 0,20, a P(A ∪ B) = 0,50. Co można powiedzieć o zdarzeniach A i B?",
      c(
        "Są rozłączne — w żadnej kontroli nie wystąpiły razem" = "disjoint",
        "Wystąpiły razem w 50 kontrolach" = "fifty",
        "Nie da się nic powiedzieć bez P(A ∩ B)" = "unknown"
      ),
      correct = "disjoint",
      explanation = "Ze wzoru (1.5): P(A ∩ B) = P(A) + P(B) − P(A ∪ B) = 0,30 + 0,20 − 0,50 = 0. Część wspólna jest pusta, więc zdarzenia są rozłączne (definicja 1.7).",
      hints = c(
        fifty = "0,50 to prawdopodobieństwo sumy, nie części wspólnej. Przekształć wzór (1.5).",
        unknown = "Wzór (1.5) łączy cztery wielkości. Znasz trzy z nich — wylicz czwartą."
      )
    ),

    lc_h2("ch4-aksjomaty", "Jedna definicja dla symetrii i dla danych"),
    lc_p(
      "Mamy już dwa sposoby przypisywania liczb zdarzeniom: definicję klasyczną
       dla symetrycznych losowań i częstość dla rejestrów. Wzory (1.3)–(1.5)
       wyprowadziliśmy z liczenia elementów, ale działają one w obu przypadkach —
       a także w modelach z kolejnych wykładów, gdzie wyników jest nieskończenie
       wiele (czas do awarii, stężenie gazu). Współczesny rachunek
       prawdopodobieństwa odwraca więc kierunek: nie mówi, skąd bierze się
       liczba, tylko jakie reguły musi spełniać każde sensowne przypisanie."
    ),
    risk_definition("1.8", "Aksjomatyczna definicja prawdopodobieństwa (Kołmogorow)", c(
      "Prawdopodobieństwem nazywamy funkcję P, która każdemu zdarzeniu A ⊆ Ω
       przypisuje liczbę P(A) i spełnia trzy aksjomaty (1.7): nieujemność,
       unormowanie oraz addytywność dla zdarzeń parami rozłącznych.",
      "Addytywność zapisuje się w wersji dla przeliczalnie wielu zdarzeń; w tym
       wykładzie wystarczy wersja dla dwóch: jeśli A ∩ B = ∅, to P(A ∪ B) =
       P(A) + P(B)."
    )),
    risk_formula(
      "P(A)\\ge 0,\\qquad P(\\Omega)=1,\\qquad P\\Big(\\bigcup_{i} A_i\\Big)=\\sum_{i} P(A_i)\\ \\text{dla parami rozłącznych } A_i",
      num = "1.7",
      legend = c(
        "A_i" = "kolejne zdarzenia, z których żadne dwa nie mogą zajść razem",
        "\\bigcup_{i} A_i" = "zaszło któreś z nich"
      )
    ),
    risk_derivation("wzory (1.3)–(1.5) z aksjomatów", c(
      "Definicja klasyczna i częstość spełniają aksjomaty (1.7) — łatwo to
       sprawdzić, licząc elementy. Ważniejsze jest odwrócenie: każda własność,
       którą wyprowadzimy z samych aksjomatów, obowiązuje w każdym modelu, a nie
       tylko przy symetrii.",
      "Dopełnienie: A i Aᶜ są rozłączne, a ich suma to Ω. Zdarzenie niemożliwe:
       ∅ = Ωᶜ. Suma dowolnych zdarzeń: A ∪ B rozkładamy na rozłączne kawałki A
       oraz B \\ A, a B na rozłączne kawałki A ∩ B oraz B \\ A. Ograniczenie z
       góry: skoro P(Aᶜ) ≥ 0, to P(A) = 1 − P(Aᶜ) ≤ 1."
    ), lines = c(
      "1 = P(Ω) = P(A ∪ Aᶜ) = P(A) + P(Aᶜ)       ⇒  P(Aᶜ) = 1 − P(A)          (1.4)",
      "P(∅) = P(Ωᶜ) = 1 − P(Ω) = 0                                           (1.3)",
      "P(A ∪ B) = P(A) + P(B \\ A)",
      "P(B)     = P(A ∩ B) + P(B \\ A)            ⇒  P(A ∪ B) = P(A) + P(B) − P(A ∩ B)   (1.5)"
    )),
    lc_p(
      "Aksjomaty mają też praktyczną funkcję kontrolną. Jeśli w arkuszu oceny
       ryzyka trzy wykluczające się scenariusze awarii mają prawdopodobieństwa
       0,5, 0,4 i 0,3, to arkusz jest wewnętrznie sprzeczny: ich suma 1,2
       przekracza P(Ω) = 1. Nie trzeba znać żadnych danych, żeby wykryć taki błąd."
    ),
    risk_example("1.5", "Co najmniej jedna uszkodzona paleta",
      problem = c(
        "Z dostawy 24 palet, w której 6 ma uszkodzone zabezpieczenie, inspektor
         losuje jednocześnie dwie różne palety; każda para ma tę samą szansę.
         Oblicz prawdopodobieństwo, że co najmniej jedna z wylosowanych palet
         ma uszkodzone zabezpieczenie."
      ),
      steps = c(
        "Zdarzeniem elementarnym jest nieuporządkowana para palet. Liczba par:
         |Ω| = C(24, 2) = 24 · 23 / 2 = 276.",
        "Zdarzenie „co najmniej jedna uszkodzona” obejmuje pary z jedną albo dwiema
         uszkodzonymi paletami. Łatwiej policzyć dopełnienie: „obie nieuszkodzone”.
         Takich par jest C(18, 2) = 18 · 17 / 2 = 153.",
        "Ze wzoru (1.2): P(obie nieuszkodzone) = 153/276 ≈ 0,554.",
        "Ze wzoru (1.4): P(co najmniej jedna uszkodzona) = 1 − 153/276 = 123/276 ≈ 0,446.",
        "Sprawdzenie wprost: dokładnie jedna uszkodzona to 6 · 18 = 108 par, obie
         uszkodzone to C(6, 2) = 15 par; razem 123 pary, jak wyżej."
      ),
      answer = "Około 0,446. Dopełnienie sprowadziło rachunek do jednego przypadku
        zamiast dwóch."
    ),

    lc_h2("ch4-pulapka", "Rozłączne nie znaczy niezależne"),
    lc_p(
      "Zdarzenia rozłączne nie mogą zajść razem, więc ich część wspólna jest
       pusta. Zdarzenia niezależne mogą zajść razem, ale informacja o jednym nie
       zmienia prawdopodobieństwa drugiego. Dwa niezerowe zdarzenia rozłączne
       nie są niezależne: gdy A zaszło, wiemy na pewno, że B nie zaszło."
    ),

    lc_feedback(
      type = "warning",
      tags$strong("Pułapka językowa:"),
      " w rachunku prawdopodobieństwa „A lub B” obejmuje także przypadek
        „A i B”, chyba że wyraźnie mówimy o alternatywie wykluczającej."
    ),

    lc_chapter_next(
      num = "05",
      title = "Macierz ryzyka",
      lead = "Dwa zdarzenia o podobnej częstości mogą mieć zupełnie inne skutki.",
      target_id = "ch-decyzja"
    )
  )
)

ch4_server <- function(input, output, session) {
  venn_step <- reactiveVal(1L)

  observeEvent(input$ch4_venn_next, {
    next_step <- if (venn_step() >= 4L) 1L else venn_step() + 1L
    venn_step(next_step)
    updateActionButton(
      session,
      "ch4_venn_next",
      label = switch(
        as.character(next_step),
        "1" = "Dodaj P(A) i P(B)",
        "2" = "Odejmij podwójne naliczenie",
        "3" = "Pokaż zdarzenia rozłączne",
        "4" = "Od początku"
      )
    )
  })

  output$ch4_venn_explanation <- renderUI({
    switch(
      as.character(venn_step()),
      "1" = tagList(
        tags$div(class = "lc-eyebrow", "Krok 1 · Dane"),
        tags$h4("Dwa zachodzące na siebie zdarzenia"),
        tags$p("P(A) = 0,70, P(B) = 0,60, a P(A ∩ B) = 0,40."),
        tags$p("Najpierw zaznaczamy oba zbiory bez wykonywania działania.")
      ),
      "2" = tagList(
        tags$div(class = "lc-eyebrow", "Krok 2 · Naiwna suma"),
        tags$h4("Dodajemy całe A i całe B"),
        lc_formula_box(withMathJax("$$0{,}70+0{,}60=1{,}30$$")),
        tags$p("Wynik 1,30 nie może być prawdopodobieństwem. Ciemna część wspólna dostała dwa kolory — została policzona dwa razy.")
      ),
      "3" = tagList(
        tags$div(class = "lc-eyebrow", "Krok 3 · Korekta"),
        tags$h4("Usuwamy jedną kopię części wspólnej"),
        lc_formula_box(withMathJax("$$0{,}70+0{,}60-0{,}40=0{,}90$$")),
        tags$p("Obszar A ∩ B nadal należy do sumy, ale jest w niej liczony tylko raz.")
      ),
      "4" = tagList(
        tags$div(class = "lc-eyebrow", "Wyjątek · Zdarzenia rozłączne"),
        tags$h4("Kiedy samo dodawanie działa?"),
        lc_formula_box(withMathJax("$$0{,}40+0{,}35=0{,}75$$")),
        tags$p("Koła nie zachodzą na siebie, więc P(A ∩ B) = 0. Niczego nie policzyliśmy dwa razy.")
      )
    )
  })

  venn_plot <- reactive({
    circle_points <- function(center_x, center_y, radius, n = 240L) {
      angle <- seq(0, 2 * pi, length.out = n)
      data.frame(
        x = center_x + radius * cos(angle),
        y = center_y + radius * sin(angle)
      )
    }

    step <- venn_step()
    if (step == 4L) {
      circle_a <- circle_points(3.15, 3.25, 1.55)
      circle_b <- circle_points(6.85, 3.25, 1.55)
    } else {
      center_a <- 4.05
      center_b <- 5.95
      radius <- 2.15
      circle_a <- circle_points(center_a, 3.25, radius)
      circle_b <- circle_points(center_b, 3.25, radius)
      half_angle <- acos((center_b - center_a) / (2 * radius))
      overlap <- rbind(
        data.frame(
          x = center_a + radius * cos(seq(-half_angle, half_angle, length.out = 120L)),
          y = 3.25 + radius * sin(seq(-half_angle, half_angle, length.out = 120L))
        ),
        data.frame(
          x = center_b + radius * cos(seq(pi - half_angle, pi + half_angle, length.out = 120L)),
          y = 3.25 + radius * sin(seq(pi - half_angle, pi + half_angle, length.out = 120L))
        )
      )
    }

    plot <- ggplot() +
      annotate(
        "rect",
        xmin = 0.8, xmax = 9.2, ymin = 0.45, ymax = 6.05,
        fill = upwr_panel, colour = upwr_rule, linewidth = 0.7
      )

    blend_with_panel <- function(colour, fraction = 0.52) {
      grDevices::colorRampPalette(c(upwr_panel, colour))(101L)[round(fraction * 100) + 1L]
    }

    if (step == 1L) {
      plot <- plot +
        geom_polygon(data = circle_a, aes(x = x, y = y), fill = NA,
                     colour = upwr_cat[["terakota"]], linewidth = 1.3) +
        geom_polygon(data = circle_b, aes(x = x, y = y), fill = NA,
                     colour = upwr_cat[["niebo"]], linewidth = 1.3)
    } else if (step == 3L) {
      a_fill <- blend_with_panel(upwr_cat[["terakota"]])
      b_fill <- blend_with_panel(upwr_cat[["niebo"]])
      plot <- plot +
        geom_polygon(data = circle_a, aes(x = x, y = y),
                     fill = a_fill, colour = upwr_cat[["terakota"]], linewidth = 1.1) +
        geom_polygon(data = circle_b, aes(x = x, y = y),
                     fill = b_fill, colour = upwr_cat[["niebo"]], linewidth = 1.1) +
        geom_polygon(data = overlap, aes(x = x, y = y),
                     fill = a_fill, colour = upwr_accent, linewidth = 1)
    } else {
      plot <- plot +
        geom_polygon(data = circle_a, aes(x = x, y = y),
                     fill = upwr_cat[["terakota"]], colour = upwr_cat[["terakota"]],
                     alpha = 0.52, linewidth = 1.1) +
        geom_polygon(data = circle_b, aes(x = x, y = y),
                     fill = upwr_cat[["niebo"]], colour = upwr_cat[["niebo"]],
                     alpha = 0.52, linewidth = 1.1)
    }

    label_x <- if (step == 4L) c(3.15, 6.85) else c(2.7, 7.3)
    label_text <- if (step == 4L) c("A\nP(A) = 0,40", "B\nP(B) = 0,35") else
      c("A\nP(A) = 0,70", "B\nP(B) = 0,60")

    plot <- plot +
      annotate("text", x = label_x, y = 3.35, label = label_text,
               fontface = "bold", size = 5, lineheight = 1.15)

    if (step == 2L) {
      plot <- plot + annotate(
        "label", x = 5, y = 3.25,
        label = "A ∩ B = 0,40\nPOLICZONE 2 RAZY",
        fill = "#ffffff", colour = upwr_accent,
        linewidth = 0.4, fontface = "bold", size = 4.3
      )
    } else if (step == 3L) {
      plot <- plot + annotate(
        "label", x = 5, y = 3.25,
        label = "A ∩ B = 0,40\nJEDNO NALICZENIE",
        fill = "#ffffff", colour = upwr_accent,
        linewidth = 0.4, fontface = "bold", size = 4,
        lineheight = 0.9
      )
    }

    bottom_label <- switch(
      as.character(step),
      "1" = "Najpierw odczytaj dane — jeszcze niczego nie dodajemy",
      "2" = "0,70 + 0,60 = 1,30  →  wynik niemożliwy",
      "3" = "1,30 − 0,40 = 0,90  →  część wspólna pozostaje dokładnie raz",
      "4" = "P(A ∩ B) = 0  →  P(A ∪ B) = P(A) + P(B) = 0,75"
    )

    plot +
      annotate("text", x = 5, y = 0.82, label = bottom_label,
               colour = upwr_ink, size = 4.5) +
      coord_equal(xlim = c(0.5, 9.5), ylim = c(0.3, 6.25), expand = FALSE) +
      labs(
        subtitle = paste("Krok", step, "z 4"),
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        plot.subtitle = element_text(
          family = "Atkinson Hyperlegible", size = 12,
          colour = upwr_ink_soft, lineheight = 1.25,
          margin = margin(b = 10)
        )
      )
  })

  zoom_plot_server(
    "ch4_venn",
    venn_plot,
    alt = paste(
      "Czterostopniowy diagram Venna pokazujący podwójne policzenie części",
      "wspólnej oraz szczególny przypadek zdarzeń rozłącznych."
    )
  )

  observeEvent(list(input$ch4_n_a, input$ch4_n_b), {
    req(input$ch4_n_a, input$ch4_n_b)
    lower <- max(0L, input$ch4_n_a + input$ch4_n_b - 100L)
    upper <- min(input$ch4_n_a, input$ch4_n_b)
    current <- input$ch4_overlap
    if (is.null(current)) current <- lower
    value <- min(max(current, lower), upper)

    updateSliderInput(
      session,
      "ch4_overlap",
      min = lower,
      max = upper,
      value = value
    )
  })

  event_values <- reactive({
    req(input$ch4_n_a, input$ch4_n_b, input$ch4_overlap)
    lower <- max(0L, input$ch4_n_a + input$ch4_n_b - 100L)
    upper <- min(input$ch4_n_a, input$ch4_n_b)
    overlap <- min(max(input$ch4_overlap, lower), upper)
    list(
      n_a = as.integer(input$ch4_n_a),
      n_b = as.integer(input$ch4_n_b),
      overlap = as.integer(overlap)
    )
  })

  output$ch4_stats <- renderUI({
    values <- event_values()
    union <- values$n_a + values$n_b - values$overlap

    lc_stat_grid(
      lc_stat_box("P(A ∩ B)", format_probability_pl(values$overlap / 100),
                  color = upwr_cat[["wrzos"]]),
      lc_stat_box("P(A ∪ B)", format_probability_pl(union / 100),
                  color = upwr_accent),
      lc_stat_box("P(Aᶜ)", format_probability_pl(1 - values$n_a / 100),
                  color = upwr_cat[["szalwia"]]),
      lc_stat_box("Ani A, ani B", format_probability_pl((100 - union) / 100),
                  color = upwr_reference),
      columns = 2
    )
  })

  event_grid_plot <- reactive({
    values <- event_values()
    data <- build_event_grid(
      total = 100L,
      n_a = values$n_a,
      n_b = values$n_b,
      overlap = values$overlap,
      columns = 10L
    )

    ggplot(data, aes(x = column, y = -row, fill = status)) +
      geom_tile(colour = "white", linewidth = 0.7, width = 0.95, height = 0.95) +
      scale_fill_manual(values = c(
        "A i B" = upwr_cat[["wrzos"]],
        "Tylko A" = upwr_cat[["terakota"]],
        "Tylko B" = upwr_cat[["niebo"]],
        "Ani A, ani B" = upwr_reference
      )) +
      coord_equal() +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      labs(
        title = "Sto kontroli korytarza",
        subtitle = "Każdy kwadrat to jeden wynik doświadczenia",
        x = NULL,
        y = NULL,
        fill = NULL
      ) +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom"
      )
  })

  zoom_plot_server(
    "ch4_event_grid",
    event_grid_plot,
    alt = paste(
      "Siatka stu kontroli podzielonych na zdarzenie A, zdarzenie B,",
      "ich część wspólną oraz wyniki nienależące do żadnego zdarzenia."
    )
  )
}
