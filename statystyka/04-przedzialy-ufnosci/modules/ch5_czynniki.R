# ============================================================================
# CHAPTER 5: Co wpływa na szerokość przedziału?
# ============================================================================
# Ryciny rozdziału liczy przeglądarka (sf_ch5.js, silnik lcSF): Ryc. 5.2–5.4
# i Przykłady 1–3. R przygotowuje tylko dane stałe (config).

# Ryc. 5.4: statystyki prób (średnia, s, n) dla każdego zbioru danych.
# Próby syntetyczne z set.seed(42), dane kierunkowe z plików w dane/.
ch5_cmp_samples <- function() {
  csv <- function(f, col) read.csv(file.path(app_dir, "dane", f))[[col]]
  gen <- list(
    height     = function() rnorm(30, mean = 170, sd = 10),
    commute    = function() rgamma(50, shape = 4, scale = 7.5),
    grades     = function() pmin(pmax(rnorm(40, mean = 3.5, sd = 0.7), 2), 5),
    ib_wypadki = function() csv("bhp_zaklady.csv", "wskaznik_wypadkow"),
    rol_plon   = function() csv("rolnictwo_pola.csv", "plon_pszenicy"),
    tz_bialko  = function() csv("zywnosc_partie.csv", "zawartosc_bialka")
  )
  lapply(gen, function(f) {
    set.seed(42)
    x <- f()
    list(xbar = mean(x), s = sd(x), n = length(x))
  })
}

# Przykłady 1–3: przedział na tle obszaru hipotezy (sf_ch5.js, c5-edge).
ch5_edge_cases <- list(
  edge1 = list(kind = "mean", xbar = 28.5, s = 8, n = 40,
               text = "Średni czas dojazdu przekracza 26 min", bound = 26, dir = "gt",
               xlab = "Średni czas dojazdu (min)"),
  edge2 = list(kind = "prop", x = 540, n = 1000,
               text = "Poparcie dla partii X przekracza 50%", bound = 0.50, dir = "gt",
               xlab = "Poparcie dla partii X"),
  edge3 = list(kind = "mean", xbar = 68, s = 10, n = 20,
               text = "Średni wynik szkolenia przekracza 65 pkt", bound = 65, dir = "gt",
               xlab = "Średni wynik (pkt)")
)

# Przyciski poziomu ufności i „Pokaż werdykt” (widoczny po wyborze poziomu),
# wykres i werdykt; stan przycisków i tekst ustawia przeglądarka.
ch5_edge_ui <- function(case_id) {
  id <- function(s) paste0("ch5_", case_id, "_", s)
  lc_sf_figure("c5-edge", ratio = "2.6/1", max_height = "240px",
    config = ch5_edge_cases[[case_id]],
    toolbar = tagList(
      lc_group("Poziom ufności",
        lc_sf_action("c90", id("conf90"), "90%", variant = "outline"),
        lc_sf_action("c95", id("conf95"), "95%", variant = "outline"),
        lc_sf_action("c99", id("conf99"), "99%", variant = "outline")),
      htmltools::tagAppendAttributes(
        lc_sf_action("reveal", id("reveal"), "Pokaż werdykt", variant = "solid"),
        hidden = NA, style = "display:none")),
    after = tags$div(`data-role` = "edge_explain"))
}

ch5_ui <- list(
  id    = "ch-czynniki",
  num   = "05",
  title = "Co wpływa na szerokość?",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 05 · Przedziały ufności",
      num    = "05",
      title  = "Co wpływa na szerokość przedziału?",
      lead   = "Szerokość przedziału zależy od trzech rzeczy: liczby obserwacji,
                zmienności danych i tego, jak dużej pewności żądamy. Realny wpływ
                mamy głównie na pierwszą z nich, a każde zawężenie przedziału
                o połowę kosztuje czterokrotnie więcej danych."
    ),

    lc_p("W dwóch poprzednich rozdziałach budowaliśmy przedziały dla średniej
      i dla proporcji. Za każdym razem wynik miał tę samą postać: ", gloss("estymata"), "
      punktowa plus minus pewna odległość. Od tej odległości zależy, czy
      przedział jest użyteczny. Przedział „wzrost studentów leży między 150
      a 190 cm” jest poprawny, ale nic nie mówi. W tym rozdziale sprawdzimy,
      co tę odległość wydłuża i skraca oraz jak zaplanować badanie, żeby
      wynik był wystarczająco precyzyjny."),

    lc_p("Zacznijmy od sytuacji, w której precyzję widać w złotówkach. Pracownia
      sondażowa ma oszacować poparcie dla partii i płaci 20 zł za każdą ankietę.
      Na początku ma 100 ankiet, a przy poparciu bliskim 50% wynik obarczony
      jest marginesem prawie ±10 punktów procentowych. Dokupuj ankiety albo
      podwajaj próbę i obserwuj, jak zwęża się przedział i rośnie rachunek.
      Wykres pokazuje planowaną precyzję przy p = 0.5, a nie wynik zebranych
      już odpowiedzi."),

    fig("5a"),

    lc_p("Pierwsze dokupione setki ankiet wyraźnie zwężają przedział, kolejne
      coraz mniej. Żeby zmniejszyć margines o połowę, trzeba czterokrotnie
      zwiększyć próbę, więc także koszt. Z ±10 do ±5 punktów pracownia przechodzi
      za 6 000 zł, z ±5 do ±2.5 płaci już 24 000 zł. Ten rozdział wyjaśnia,
      skąd bierze się ta zależność i od czego jeszcze zależy szerokość przedziału."),

    lc_h2("ch5-czynniki", "Trzy czynniki szerokości przedziału"),

    lc_p("Odległość od estymaty do każdej z granic przedziału nazywamy ",
      gloss("margines błędu", "marginesem błędu"), " (ME, od ang. margin of
      error). Cały przedział ma szerokość 2·ME. Dla średniej margines błędu
      to iloczyn ", gloss("wartość krytyczna", "wartości krytycznej"), " rozkładu t i błędu standardowego:"),

    lc_formula_annotated(
      "⟦1|ME⟧ = ⟦2|t^*⟧ \\cdot \\frac{⟦3|s⟧}{\\sqrt{⟦4|n⟧}}",
      notes  = list("margines błędu", "wartość krytyczna", "odchylenie standardowe",
                    "liczba obserwacji"),
      colors = c("wrzos", "terakota", "wrzos", "szalwia"),
      above  = c(2, 3)
    ),

    lc_p("We wzorze widać trzy czynniki. Pierwszy to ",
      gloss("wielkość próby", "wielkość próby"), " n: stoi w mianowniku pod
      pierwiastkiem, więc im więcej obserwacji, tym węższy przedział. Drugi to ",
      gloss("poziom ufności", "poziom ufności"), ", który wyznacza wartość
      krytyczną t*: im większej pewności żądamy, tym większe t* i szerszy
      przedział. Trzeci to zmienność danych mierzona ",
      gloss("odchylenie standardowe", "odchyleniem standardowym"), " s:
      im bardziej rozproszone obserwacje, tym mniej precyzyjna średnia."),

    lc_p("Ta sama logika obowiązuje dla proporcji. Wzór ma postać
      z*·√(p̂(1 − p̂)/n), a rolę s pełni √(p̂(1 − p̂)), które jest największe
      przy p̂ = 0.5. Wszystko, co dalej powiemy o średniej, przenosi się
      więc na proporcje."),

    lc_h2("ch5-eksploracja", "Jak szybko maleje margines błędu"),

    lc_p("Panel poniżej pozwala zmieniać każdy z trzech czynników osobno.
      Górny wykres pokazuje, jak margines błędu maleje wraz z n przy
      ustalonym poziomie ufności i ustalonym s, a punkt zaznacza bieżące n.
      Dolny pokazuje odpowiadający mu przedział na stałej osi, więc łatwo
      porównać jego szerokość między ustawieniami."),

    figure_panel(
      label = "Ryc. 5.2", title = "Jak zmienia się szerokość przedziału?",
      full_width = TRUE,
      # W przeglądarce: sf_ch5.js (c5-factors) — krzywa ME(n) i pasek przedziału.
      lc_sf_figure("c5-factors",
        control = list(
          lc_sf_slider("ch5_n", "Wielkość próby (n)", 5, 100, 30, 1),
          lc_sf_slider("ch5_conf", "Poziom ufności", 0.80, 0.99, 0.95, 0.01),
          lc_sf_slider("ch5_s", "Odchylenie std. (s)", 1, 12, 8, 1)),
        toolbar = lc_readouts(
          lc_readout("ME", lc_sf_value("f_me", "2.99"), color = col_ci),
          lc_readout("Szer.", lc_sf_value("f_width", "5.97"), color = upwr_secondary),
          lc_readout("t*", lc_sf_value("f_t", "2.045"), color = col_estimate)),
        body = tagList(
          lc_sf_chart(0, ratio = "2.4/1", max_height = "320px"),
          lc_sf_chart(1, ratio = "6/1", max_height = "150px")))
    ),

    lc_p("Przy ustawieniach początkowych (n = 30, s = 8, poziom ufności 95%)
      wartość krytyczna wynosi t* = 2.045, a margines błędu 2.99, więc przedział
      ma szerokość około 6. Każdy z suwaków działa na tę liczbę inaczej."),

    lc_p("Odchylenie standardowe działa proporcjonalnie: przy s = 4 margines
      spada dokładnie o połowę, do 1.49. Poziom ufności działa przez t*:
      przy 90% margines wynosi 2.48, przy 99% już 4.03. Najciekawsza jest
      krzywa dla n. Na początku opada stromo: przy n = 5 margines wynosi
      9.93, przy n = 30 już tylko 2.99. Dalej spłaszcza się i kolejne
      obserwacje dają coraz mniej."),

    lc_p("To ten sam mechanizm, który poznaliśmy w wykładzie 03: ", gloss("błąd standardowy"), "
      maleje jak 1/√n, więc żeby zmniejszyć go o połowę, trzeba czterokrotnie
      większej próby. Margines błędu dziedziczy tę zależność. Przejście z n = 25
      do n = 100 skraca go z 3.30 do 1.59, czyli nieco ponad dwukrotnie,
      bo przy większym n maleje też t*. Kolejne czterokrotne powiększenie
      próby, do 400 obserwacji, znowu skróci przedział mniej więcej o połowę.
      Każde następne zawężenie przedziału jest więc droższe od poprzedniego."),

    lc_h2("ch5-planowanie", "Planowanie wielkości próby"),

    lc_p("Skoro wiemy, jak margines błędu zależy od n, możemy odwrócić pytanie.
      Zamiast liczyć, jak precyzyjny wynik dała próba, którą już mamy,
      ustalamy z góry, jakiej precyzji potrzebujemy, i liczymy, ile obserwacji
      trzeba zebrać. Wystarczy rozwiązać wzór na margines błędu względem n:"),

    lc_formula_annotated(
      "⟦1|n⟧ = \\left(\\frac{⟦2|z^*⟧ \\cdot ⟦3|s⟧}{⟦4|ME_{\\text{max}}⟧}\\right)^2",
      notes  = list("potrzebna liczebność", "wartość krytyczna", "zakładane odchylenie",
                    "dopuszczalny margines"),
      colors = c("szalwia", "terakota", "wrzos", "wrzos"),
      above  = c(2, 3)
    ),

    lc_p("Wynik zaokrąglamy zawsze w górę. Do planowania bierzemy z* = 1.96, bo n
      dopiero wyznaczamy. Najtrudniejsze jest s: przed badaniem nie mamy danych, więc odchylenie
      standardowe trzeba założyć na podstawie badania pilotażowego, wcześniejszych
      publikacji albo rozsądnego szacunku zakresu wartości."),

    figure_panel(
      label = "Ryc. 5.3", title = "Kalkulator wielkości próby",
      full_width = TRUE,
      # W przeglądarce: sf_ch5.js (c5-plan) — wzór na n, krzywa ME(n), osiągnięte ME.
      lc_sf_figure("c5-plan",
        control = list(
          numericInput("ch5_plan_me", "Pożądany margines błędu",
                         value = 2, min = 0.1, step = 0.1),
          numericInput("ch5_plan_s", "Spodziewane s",
                         value = 10, min = 0.1, step = 0.5),
          lc_sf_slider("ch5_plan_conf", "Poziom ufności", 0.80, 0.99, 0.95, 0.01)),
        body = tagList(
          tags$div(`data-role` = "plan_result"),
          lc_sf_chart(0, ratio = "2.6/1", max_height = "300px"),
          lc_sf_chart(1, ratio = "6/1", max_height = "150px")))
    ),

    lc_p("Przy domyślnych ustawieniach (margines 2, s = 10, poziom ufności 95%)
      wzór daje (1.96 · 10 / 2)² = 96.04, czyli potrzeba 97 obserwacji. Żądanie
      dwa razy większej precyzji, czyli marginesu 1, podnosi wymaganą próbę
      do 385 osób, prawie czterokrotnie. Poziom ufności też kosztuje: przy 90%
      wystarczy 68 obserwacji, przy 99% potrzeba 166."),

    lc_p("Dla proporcji rachunek jest analogiczny, z p(1 − p) w miejscu s².
      Gdy nie wiemy nic o spodziewanej proporcji, przyjmujemy najgorszy
      przypadek p = 0.5. Margines 3 punktów procentowych przy poziomie ufności
      95% wymaga wtedy 1068 respondentów. Stąd biorą się typowe sondaże
      na około tysiącu osób."),

    lc_p("To ten sam rachunek co w pracowni z Ryc. 5.1: margines ±3 punkty
      wymaga około tysiąca ankiet, a każde kolejne zmniejszenie go o połowę
      kosztuje cztery razy więcej."),

    lc_h2("ch5-porownanie", "Ten sam zbiór, trzy poziomy ufności"),

    lc_p("W kalkulatorze wybór poziomu ufności był częścią planu badania.
      Teraz spójrzmy na niego od strony gotowych danych. Panel poniżej liczy
      z jednej próby trzy przedziały: 90%, 95% i 99%. Dane i ich środek
      są za każdym razem te same, zmienia się tylko t*."),

    figure_panel(
      label = "Ryc. 5.4", title = "Trzy poziomy ufności",
      full_width = TRUE,
      # W przeglądarce: sf_ch5.js (c5-cmp); statystyki prób liczy R przy budowie UI.
      lc_sf_figure("c5-cmp", max_height = "250px", ratio = "3/1",
        config = list(data = ch5_cmp_samples()),
        control = list(
          selectInput("ch5_cmp_data", "Dane", selectize = FALSE,
              choices = list(
                "Przykłady ogólne" = c(
                  "Wzrost studentów (n=30)" = "height",
                  "Czas dojazdu (n=50)" = "commute",
                  "Oceny z egzaminu (n=40)" = "grades"
                ),
                "Dane kierunkowe" = c(
                  "IB: wskaźnik wypadków (n=320)" = "ib_wypadki",
                  "ROL: plon pszenicy (n=280)" = "rol_plon",
                  "TZ: zawartość białka (n=350)" = "tz_bialko"
                )
              ),
              selected = "height"
            )),
        toolbar = tagList(
          lc_sf_action("calc", "ch5_cmp_calc", "Oblicz 3 przedziały", variant = "solid"),
          tags$div(class = "lc-reads lc-push", `data-role` = "cmp_stats")))
    ),

    lc_p("Dla próby wzrostu 30 studentów (średnia 170.69 cm, s = 12.55 cm)
      przedział 90% to [166.79, 174.58], 95% to [166.00, 175.37], a 99% to
      [164.37, 177.00]. Przedział 99% jest o ponad 60% szerszy niż 90%.
      Przy danych kierunkowych, gdzie próby liczą około 300 obserwacji,
      wszystkie trzy przedziały są wąskie i różnice między nimi stają się
      niewielkie w porównaniu ze skalą zmiennej."),

    lc_p("Wybór poziomu ufności to więc kompromis między pewnością a precyzją.
      Przedział 99% rzadziej mija prawdziwą wartość, ale jest szeroki.
      Przedział 90% jest węższy, za to częściej się myli. Poziom 95% przyjął się
      jako rozsądny środek i jest domyślny w większości programów, ale to umowa,
      a nie prawo statystyki."),

    lc_h2("ch5-edge-case", "Przypadek graniczny: poziom ufności zmienia wniosek"),

    lc_p("Kompromis między pewnością a precyzją ma praktyczne konsekwencje,
      gdy pytamy, czy parametr przekracza jakiś próg. Jeśli cały przedział leży
      powyżej progu, wszystkie wiarygodne wartości parametru go przekraczają
      i możemy to stwierdzić. Jeśli przedział przecina próg, dane są zgodne
      zarówno z wartościami powyżej, jak i poniżej. Nie wystarczy więc, że
      sama średnia czy proporcja z próby leży nad granicą. Liczy się położenie
      całego przedziału, a jego szerokość zależy od wybranego poziomu ufności."),

    lc_p("W trzech przykładach poniżej ta sama próba jest oceniana przy
      poziomach 90%, 95% i 99%. Po wybraniu poziomu wykres pokazuje przedział
      na tle obszaru hipotezy. Przed odsłonięciem werdyktu warto samodzielnie
      ocenić, czy przedział pozwala ją przyjąć."),

    figure_panel(
      label = "Przykład 1",
      title = "Czas dojazdu — czy średni czas przekracza 26 min?",
      p("Zmierzono czas dojazdu 40 pracowników. Średnia z próby wynosi ",
        withMathJax("\\(\\bar{x} = 28.5\\)"), " min, odchylenie standardowe ", withMathJax("\\(s = 8\\)"), " min.
        Hipoteza: średni czas dojazdu w populacji przekracza 26 min."),
      ch5_edge_ui("edge1")
    ),

    figure_panel(
      label = "Przykład 2",
      title = "Sondaż — czy poparcie przekracza 50%?",
      p("Pracownia sondażowa zapytała 1000 wyborców, czy poprą partię X.
        Odpowiedzi TAK udzieliło 540 osób (", withMathJax("\\(\\hat{p} = 0.54\\)"), ").
        Hipoteza: poparcie w populacji przekracza próg 50%."),
      ch5_edge_ui("edge2")
    ),

    figure_panel(
      label = "Przykład 3",
      title = "Wynik szkolenia — czy średnia przekracza 65 pkt?",
      p("Po szkoleniu BHP 20 pracowników uzyskało średni wynik ",
        withMathJax("\\(\\bar{x} = 68\\)"), " pkt
        (na 100), ", withMathJax("\\(s = 10\\)"), " pkt.
        Hipoteza: średni wynik w populacji przekracza próg 65 pkt."),
      ch5_edge_ui("edge3")
    ),

    lc_p("Gdy przedział leży blisko progu, sama zmiana poziomu ufności może
      przesunąć jego granicę na drugą stronę i zmienić wniosek. Bywa też
      odwrotnie: próba jest tak mała, że przedział obejmuje próg przy każdym
      rozsądnym poziomie ufności. Wtedy zmiana poziomu nie pomoże, a
      rozstrzygnięcie wymaga większej próby, czyli powrotu do planowania
      z poprzednich sekcji."),

    lc_p("Takie wyniki często wydają się podejrzane: skoro przy 90% wniosek
      jest pozytywny, a przy 95% już nie, to jak jest naprawdę? Obie odpowiedzi
      są poprawne, bo odpowiadają na różne pytania. Zdanie „nie możemy tego
      stwierdzić przy ufności 95%, ale możemy przy 90%” nie jest sprzecznością,
      tylko precyzyjnym opisem siły dowodów. Potoczne myślenie zna tylko
      „pewne”, „prawdopodobne” i „wątpliwe”, a statystyka pozwala tę pewność
      zmierzyć."),

    lc_p("Wybór poziomu ufności powinien wynikać z kosztu pomyłki. Przy wstępnej
      eksploracji, gdy błąd niewiele kosztuje, można przyjąć 90%. W badaniach
      medycznych czy kontroli jakości uzasadnione bywa 99%. Ten swobodny wybór
      ma jedno ograniczenie: poziom trzeba ustalić przed obejrzeniem wyników.
      Kto najpierw patrzy na przedziały, a potem wybiera poziom, przy którym
      wniosek wychodzi „po jego myśli”, przestaje mierzyć siłę dowodów."),


    lc_note("Zasada", rule = TRUE,
      "Poziom ufności ustal przed analizą danych i zawsze podawaj go w raporcie
       razem z przedziałem."
    ),

    fig("D1"),

    lc_p("Ten rozdział zamyka wykład o przedziałach ufności. Zaczęliśmy od
      pytania, jak z jednej próby powiedzieć coś o populacji, a skończyliśmy
      na przedziale, którego szerokość umiemy wyjaśnić i zaplanować.
      Przykłady z ostatniej sekcji zadawały jednak pytanie innego rodzaju:
      nie „gdzie leży parametr?”, tylko „czy przekracza konkretną wartość?”.
      Takie pytania są tematem wykładu 05 o testowaniu hipotez. Zobaczymy tam,
      że sprawdzenie, czy przedział obejmuje wartość progową, jest blisko
      spokrewnione z testem statystycznym, a rolę poziomu ufności przejmie ",
      gloss("poziom istotności", "poziom istotności"), "."),

    lc_chapter_next(
      num       = "06",
      title     = "Ćwiczenia",
      lead      = "czas zastosować tę wiedzę na rzeczywistym zbiorze danych",
      target_id = "ch-cwiczenia"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch5_server <- function(input, output, session) {
  # Ryc. 5.2–5.4 i Przykłady 1–3 liczy przeglądarka (sf_ch5.js): suwaki, kalkulator,
  # trzy przedziały, przyciski poziomu ufności i werdykty. Serwer nie ma tu nic do roboty.
  invisible(NULL)
}
