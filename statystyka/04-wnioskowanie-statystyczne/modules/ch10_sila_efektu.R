# ============================================================================
# CHAPTER 10: Siła efektu
# ============================================================================

ch10_ui <- list(
  id = "ch-sila-efektu", num = "10", title = "Siła efektu",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 10 · Testowanie hipotez",
      num    = "10",
      title  = "Siła efektu.",
      lead   = "Wynik może być istotny statystycznie i jednocześnie zbyt mały, żeby
                cokolwiek znaczył w praktyce. Miary siły efektu mówią, jak duża jest
                różnica albo zależność, a ta informacja nie zmienia się wraz
                z liczebnością próby."
    ),

    lc_p("Każdy test z rozdziałów 04–09 kończył się jedną z dwóch decyzji:
      odrzucamy H₀ albo nie mamy podstaw, żeby ją odrzucić. Ta decyzja mówi,
      czy dane dają się pogodzić z brakiem efektu. Nie mówi natomiast, jak duży
      jest efekt, który wykryliśmy. Do tego służą miary ",
      gloss("wielkość efektu", "siły efektu"), ". W tym rozdziale każdemu
      poznanemu testowi przypiszemy jego miarę: testom t odpowiada d Cohena,
      korelacji współczynnik r, testowi χ² V Cramera, a ANOVA η²."),

    # ========================================================================
    # Sekcja 1: Motywacja
    # ========================================================================
    lc_h2("ch10-motywacja", "p-wartość nie mierzy ważności"),

    lc_p("Wróćmy do przykładu B4 z wykładu 03. Badanie porównywało IQ w dwóch
      województwach, po 20 000 osób w każdym. Średnie wyniosły 100,4 i 100,0
      punktu przy odchyleniu standardowym 15. Przedział ufności dla różnicy,
      [0,11; 0,69] pkt, nie obejmował zera. Test t daje ten sam werdykt:
      t ≈ 2,67, p ≈ 0,008, więc na poziomie α = 0,05 odrzucamy H₀ o równości
      średnich. Mimo to różnica 0,4 punktu to około 0,03 odchylenia
      standardowego IQ. Wynik jest istotny statystycznie, ale nie ma ",
      gloss("istotność praktyczna", "istotności praktycznej"), "."),

    lc_p("Rozbieżność bierze się stąd, że statystyka testowa dzieli różnicę średnich
      przez ", gloss("błąd standardowy", "błąd standardowy"), ", a błąd
      standardowy maleje jak \\(1/\\sqrt{n}\\). Dla dwóch równolicznych grup
      o tym samym odchyleniu standardowym s statystykę t można zapisać przez
      różnicę wyrażoną w odchyleniach standardowych, którą oznaczymy d:"),

    lc_formula_box(withMathJax(
      "$$t = \\frac{\\bar{x}_1 - \\bar{x}_2}{s\\sqrt{2/n}} = d \\cdot \\sqrt{\\frac{n}{2}}, \\qquad d = \\frac{\\bar{x}_1 - \\bar{x}_2}{s}$$"
    )),

    lc_p("Ten sam efekt d daje tym większe t i tym mniejszą p-wartość, im
      większa jest próba. Przy dostatecznie dużym n każda niezerowa różnica
      stanie się istotna, a przy małym n nawet duża różnica może nie przekroczyć
      progu. Panel pokazuje dwie populacje oddalone o d odchyleń standardowych
      oraz wynik testu t dla prób po n obserwacji, w których różnica średnich
      wynosi dokładnie d."),

    figure_panel(
      label = "Ryc. 10.1",
      title = "p kontra d: to nie to samo",
      fluidRow(
        column(4,
          selectInput("ch10_dist_scenario", "Przykład:",
            choices = c(
              "Enzym (TŻ)"   = "TZ",
              "Ziarno (ROL)" = "ROL",
              "Reakcja (IB)" = "IB"
            ),
            selected = "TZ"
          ),
          lc_slider("ch10_d", "Cohen's d (wielkość efektu)", 0.1, 1.5, 0.3, 0.05),
          lc_slider("ch10_n", "n na grupę", 20, 300, 50, 10),
          uiOutput("ch10_dist_hint")
        ),
        column(8,
          zoom_plot_ui("ch10_dist_plot", height = "280px"),
          uiOutput("ch10_dist_stats")
        )
      )
    ),

    lc_p("Przy ustawieniach startowych (d = 0,3, po 50 obserwacji w grupie)
      t = 1,50 i p ≈ 0,14, więc nie mamy podstaw do odrzucenia H₀. Ta sama
      różnica przy 90 obserwacjach w grupie daje p ≈ 0,046, a przy 300
      p < 0,001. Krzywe na wykresie przez cały czas wyglądają tak samo, bo
      efekt się nie zmienia. Zmienia się tylko precyzja, z jaką go mierzymy.
      Działa to także w drugą stronę: przy 20 obserwacjach w grupie d = 0,8
      daje p ≈ 0,016, ale d = 0,5 już tylko p ≈ 0,12. Brak istotności przy
      małej próbie nie dowodzi, że efektu nie ma. Oznacza jedynie, że próba
      była za mała, żeby odróżnić go od przypadku."),

    lc_p("Dlatego p-wartość i siła efektu odpowiadają na dwa różne pytania.
      p-wartość mówi, jak trudno pogodzić dane z H₀. Miara siły efektu mówi,
      jak duża jest różnica albo zależność. W raporcie potrzebne są obie,
      a najlepiej razem z przedziałem ufności z wykładu 03, który pokazuje
      jednocześnie, gdzie leży efekt i jak dokładnie go znamy."),

    # ========================================================================
    # Sekcja 2: Cohen's d
    # ========================================================================
    lc_h2("ch10-cohens-d", "Cohen's d — testy t"),

    lc_p("Wielkość d z poprzedniej sekcji ma swoją nazwę: ",
      gloss("d Cohena", "d Cohena"), " (ang. Cohen's d). Wyraża różnicę średnich
      w jednostkach ", gloss("odchylenie standardowe", "odchylenia standardowego"),
      ". Używamy go przy wszystkich wariantach testu t: dla dwóch grup
      niezależnych z rozdziału 08 w mianowniku stoi odchylenie standardowe
      połączone z obu grup."),

    lc_formula_box(withMathJax(
      "$$d = \\frac{\\bar{x}_1 - \\bar{x}_2}{s_p}, \\qquad s_p = \\sqrt{\\frac{(n_1 - 1)\\,s_1^2 + (n_2 - 1)\\,s_2^2}{n_1 + n_2 - 2}}$$"
    )),

    lc_p("W teście t jednej próby z rozdziału 04 porównujemy średnią z wartością
      odniesienia \\(\\mu_0\\), a w teście dla prób zależnych z rozdziału 08
      liczymy średnią różnic w parach \\(\\bar{d}\\) i dzielimy ją przez
      odchylenie standardowe tych różnic \\(s_d\\)."),

    lc_formula_box(withMathJax(
      "$$d = \\frac{\\bar{x} - \\mu_0}{s} \\quad \\text{(jedna próba)}, \\qquad d = \\frac{\\bar{d}}{s_d} \\quad \\text{(próby zależne)}$$"
    )),

    lc_p("Dzielenie przez odchylenie standardowe sprowadza różnice z różnych
      dziedzin do wspólnej skali. Różnica 5 cm wzrostu i różnica 5 punktów na
      egzaminie to zupełnie inne sytuacje, ale przeliczone na liczbę odchyleń
      standardowych dają się porównać. W przeciwieństwie do t, d nie rośnie
      wraz z n. Większa próba pozwala oszacować d dokładniej, ale nie robi go
      większym. W R d dla dwóch grup liczy ",
      tags$code("cohens_d(wynik ~ grupa)"), " z pakietu rstatix. Domyślnie
      w mianowniku używa pierwiastka ze średniej z dwóch wariancji, co przy
      równolicznych grupach daje dokładnie \\(s_p\\)."),

    lc_p("Cohen zaproponował orientacyjne progi: 0,2 to efekt mały, 0,5 średni,
      a 0,8 duży. Tabela pokazuje, jak wyglądają one w przykładach z panelu
      poniżej. Różnica IQ z przykładu B4 (d ≈ 0,03) leży daleko poniżej
      progu efektu małego."),

    tags$table(class = "lc-table lc-table-bordered",
      style = "font-size: 15px; margin: 10px 0;",
      tags$thead(tags$tr(
        tags$th("Wielkość efektu"), tags$th("|d|"), tags$th("Przykład")
      )),
      tags$tbody(
        tags$tr(tags$td("mały"),   tags$td("0,2"),
                tags$td("pH jogurtu 4,50 i 4,56 przy SD 0,30")),
        tags$tr(tags$td("średni"), tags$td("0,5"),
                tags$td("wilgotność suszu 20,0% i 22,5% przy SD 5")),
        tags$tr(tags$td("duży"),   tags$td("0,8"),
                tags$td("czas inaktywacji enzymów 8 i 10 min przy SD 2,5"))
      )
    ),

    lc_p("Panel zamienia wybraną wartość d na konkretne średnie i odchylenia
      standardowe w trzech dziedzinach."),

    figure_panel(
      label = "Ryc. 10.2",
      title = "Cohen's d w surowych liczbach",
      fluidRow(
        column(4,
          selectInput("ch10_d_scenario", "Przykład:",
            choices = c(
              "Jogurt (TŻ)"    = "TZ",
              "Pszenica (ROL)" = "ROL",
              "BHP (IB)"       = "IB"
            ),
            selected = "TZ"
          ),
          lc_segmented("ch10_d_level", "Wielkość efektu", choices = c(
              "d = 0,2 (mały)"      = "0.2",
              "d = 0,5 (średni)"    = "0.5",
              "d = 0,8 (duży)"      = "0.8",
              "d = 1,2 (b. duży)"   = "1.2"
            ), selected = "0.5")
        ),
        column(8,
          zoom_plot_ui("ch10_d_plot", height = "240px"),
          uiOutput("ch10_d_table")
        )
      )
    ),

    lc_p("Nawet efekt średni oznacza silnie zachodzące na siebie rozkłady.
      Przy d = 0,5 krzywe dzielą około 80% powierzchni, a losowo wybrana
      obserwacja z grupy o wyższej średniej przewyższa losowo wybraną
      obserwację z drugiej grupy w około 64% przypadków. Przy d = 0,2 to
      tylko 56%, czyli niewiele więcej niż rzut monetą, a przy d = 0,8 około
      71%. Dopiero przy d = 1,2 wspólna część rozkładów spada do mniej więcej
      połowy. Efekt duży w sensie Cohena nie oznacza więc, że grupy się
      nie pokrywają."),

    # ========================================================================
    # Sekcja 3: r
    # ========================================================================
    lc_h2("ch10-r", "r — korelacja Pearsona"),

    lc_p("Dla korelacji z rozdziału 06 nie trzeba liczyć osobnej miary.
      Współczynnik ", gloss("korelacja Pearsona", "korelacji Pearsona"),
      " \\(r\\) sam jest miarą siły efektu: nie zależy od n i ma stałą skalę
      od −1 do +1. Test korelacji sprawdzał tylko, czy r z próby różni się od
      zera bardziej, niż wynikałoby z przypadku."),

    lc_formula_box(withMathJax(
      "$$r = \\frac{\\sum (x_i - \\bar{x})(y_i - \\bar{y})}{\\sqrt{\\sum (x_i - \\bar{x})^2 \\cdot \\sum (y_i - \\bar{y})^2}}$$"
    )),

    lc_p("Łatwiejszy do interpretacji jest często ",
      gloss("współczynnik determinacji", "kwadrat korelacji r²"), ". Mówi on,
      jaką część zmienności y wyjaśnia liniowa zależność od x. Przy r = 0,5
      mamy r² = 0,25: x wyjaśnia 25% zmienności y, a 75% zostaje na inne
      czynniki. Pamiętaj, że r mierzy tylko zależność liniową. Silna zależność
      krzywoliniowa może dać r bliskie zeru. Orientacyjne progi Cohena dla |r|
      to 0,1, 0,3 i 0,5."),

    tags$table(class = "lc-table lc-table-bordered",
      style = "font-size: 15px; margin: 10px 0;",
      tags$thead(tags$tr(
        tags$th("Wielkość efektu"), tags$th("|r|"), tags$th("r²")
      )),
      tags$tbody(
        tags$tr(tags$td("mała"),    tags$td("0,1"), tags$td("1% zmienności wyjaśnione")),
        tags$tr(tags$td("średnia"), tags$td("0,3"), tags$td("9% zmienności wyjaśnione")),
        tags$tr(tags$td("duża"),    tags$td("0,5"), tags$td("25% zmienności wyjaśnione"))
      )
    ),

    lc_p("Panel pokazuje 50 punktów wylosowanych z populacji o zadanej
      korelacji. Podtytuł wykresu podaje r policzone z tych punktów."),

    figure_panel(
      label = "Ryc. 10.3",
      title = "r w surowych liczbach",
      fluidRow(
        column(4,
          selectInput("ch10_r_scenario", "Przykład:",
            choices = c(
              "Jogurt (TŻ)"   = "TZ",
              "Plon (ROL)"    = "ROL",
              "Wypadki (IB)"  = "IB"
            ),
            selected = "TZ"
          ),
          radioButtons("ch10_r_level", "Wielkość korelacji:",
            choices = c(
              "r = 0,1 (mała)"       = "0.1",
              "r = 0,3 (średnia)"    = "0.3",
              "r = 0,5 (duża)"       = "0.5",
              "r = 0,7 (b. duża)"    = "0.7",
              "r = 0,9 (b. duża)"    = "0.9"
            ),
            selected = "0.5"
          ),
          uiOutput("ch10_r_hint")
        ),
        column(8,
          zoom_plot_ui("ch10_r_plot", height = "240px"),
          uiOutput("ch10_r_table")
        )
      )
    ),

    lc_p("Przy ustawieniach startowych (zadane r = 0,5) z wylosowanych punktów
      wychodzi r = 0,55. Trend widać wyraźnie, ale punkty leżą daleko od
      prostej: x wyjaśnia około jednej czwartej zmienności y. Przy r = 0,3
      zależność da się jeszcze dostrzec, ale wyjaśnia tylko 9% zmienności,
      a przy r = 0,1 trudno ją zauważyć na wykresie. Różnica między r zadanym
      a policzonym z 50 punktów przypomina, że r z próby jest
      estymatorem i ma własny rozrzut."),

    # ========================================================================
    # Sekcja 4: Cramér's V
    # ========================================================================
    lc_h2("ch10-cramers-v", "Cramér's V — test chi kwadrat"),

    lc_p("Test χ² z rozdziału 07 ma ten sam problem co test t. Przy tych samych
      proporcjach w ", gloss("tabela kontyngencji", "tabeli kontyngencji"),
      " statystyka χ² rośnie proporcjonalnie do n, a do tego zależy od rozmiaru
      tabeli. Sama wartość χ² nie mówi więc, jak silny jest związek dwóch ",
      gloss("zmienna jakościowa", "zmiennych jakościowych"), ". ",
      gloss("V Cramera", "V Cramera"), " (ang. Cramér's V) dzieli χ² przez n
      i przez rozmiar tabeli, dzięki czemu przyjmuje wartości od 0 (brak
      związku) do 1 (pełna zależność)."),

    lc_formula_box(withMathJax(
      "$$V = \\sqrt{\\frac{\\chi^2}{n \\cdot (\\min(r, c) - 1)}}$$"
    )),

    lc_p("We wzorze \\(r\\) i \\(c\\) oznaczają liczbę wierszy i kolumn tabeli.
      Dla tabeli 2×2 V jest równe współczynnikowi φ (fi). Gdy obie grupy są
      równoliczne, a ogólny odsetek wynosi 50%, φ jest po prostu różnicą
      odsetków w grupach: V = 0,30 odpowiada na przykład 35% i 65%. Progi
      Cohena zależą od mniejszego wymiaru tabeli. W R V liczy ",
      tags$code("cramer_v(tab)"), " z pakietu rstatix."),

    tags$table(class = "lc-table lc-table-bordered",
      style = "font-size: 15px; margin: 10px 0;",
      tags$thead(tags$tr(
        tags$th("Wielkość efektu"),
        tags$th("V (min(r, c) = 2, np. 2×2, 2×3)"),
        tags$th("V (min(r, c) = 3, np. 3×3)")
      )),
      tags$tbody(
        tags$tr(tags$td("mały"),   tags$td("0,10"), tags$td("0,07")),
        tags$tr(tags$td("średni"), tags$td("0,30"), tags$td("0,21")),
        tags$tr(tags$td("duży"),   tags$td("0,50"), tags$td("0,35"))
      )
    ),

    lc_p("Panel pokazuje odsetki w dwóch równolicznych grupach dla wybranej
      wartości V w tabeli 2×2."),

    figure_panel(
      label = "Ryc. 10.4",
      title = "Cramér's V w tabeli 2×2",
      fluidRow(
        column(4,
          selectInput("ch10_v_scenario", "Przykład:",
            choices = c(
              "Pleśń (TŻ)"      = "TZ",
              "Chwasty (ROL)"   = "ROL",
              "Szczepienie (IB)" = "IB"
            ),
            selected = "TZ"
          ),
          lc_segmented("ch10_v_level", "Wielkość efektu", choices = c(
              "V = 0,10 (mały)"   = "0.10",
              "V = 0,30 (średni)" = "0.30",
              "V = 0,50 (duży)"   = "0.50",
              "V = 0,70 (b. duży)" = "0.70"
            ), selected = "0.30"),
          uiOutput("ch10_v_hint")
        ),
        column(8,
          zoom_plot_ui("ch10_v_plot", height = "240px"),
          uiOutput("ch10_v_table")
        )
      )
    ),

    lc_p("Przy ustawieniach startowych pleśń pojawia się na 35% produktów
      w opakowaniu A i na 65% w opakowaniu B. To różnica 30 punktów
      procentowych, a mimo to według progów Cohena jest to dopiero efekt
      średni. Efekt mały (V = 0,10) oznacza w tym układzie odsetki 45% i 55%,
      a duży (V = 0,50) 25% i 75%. Przy innych proporcjach grup albo innym
      ogólnym odsetku ta sama wartość V odpowiada nieco innej różnicy."),

    # ========================================================================
    # Sekcja 5: eta kwadrat
    # ========================================================================
    lc_h2("ch10-eta2", "eta kwadrat — ANOVA"),

    lc_p("ANOVA z rozdziału 09 dzieliła całkowitą zmienność wyników na część
      między grupami i część wewnątrz grup. Statystyka F porównywała te części,
      ale podobnie jak t rośnie wraz z n. ",
      gloss("eta kwadrat", "Eta kwadrat"), " (\\(\\eta^2\\)) mówi, jaki udział
      całkowitej zmienności przypada na różnice między grupami, czyli jaką część
      zmienności wyników tłumaczy badany czynnik. To ta sama idea co r²
      w korelacji."),

    lc_formula_box(withMathJax(
      "$$\\eta^2 = \\frac{SS_{\\text{między}}}{SS_{\\text{całkowita}}}$$"
    )),

    lc_p("Programy podają kilka wariantów tej miary: η² częściowe (ang. partial)
      i η² uogólnione. Funkcja ", tags$code("anova_test()"), " z pakietu rstatix
      zwraca η² uogólnione w kolumnie ges. W jednoczynnikowej ANOVA dla grup
      niezależnych wszystkie trzy warianty są równe zwykłemu η². Różnią się
      dopiero w modelach z kilkoma czynnikami. Orientacyjne progi Cohena to
      0,01, 0,06 i 0,14."),

    tags$table(class = "lc-table lc-table-bordered",
      style = "font-size: 15px; margin: 10px 0;",
      tags$thead(tags$tr(
        tags$th("Wielkość efektu"), tags$th(withMathJax("\\(\\eta^2\\)")),
        tags$th("Interpretacja")
      )),
      tags$tbody(
        tags$tr(tags$td("mały"),   tags$td("0,01"),
                tags$td("czynnik tłumaczy około 1% zmienności")),
        tags$tr(tags$td("średni"), tags$td("0,06"),
                tags$td("czynnik tłumaczy około 6% zmienności")),
        tags$tr(tags$td("duży"),   tags$td("0,14"),
                tags$td("czynnik tłumaczy co najmniej 14% zmienności"))
      )
    ),

    lc_p("Panel losuje po 30 obserwacji w trzech grupach z populacji o zadanym
      η². Tabela pod wykresem podaje średnie i η² tej populacji."),

    figure_panel(
      label = "Ryc. 10.5",
      title = "η² w ANOVA — trzy grupy",
      fluidRow(
        column(4,
          selectInput("ch10_eta_scenario", "Przykład:",
            choices = c(
              "Pasteryzacja (TŻ)" = "TZ",
              "Nawozy (ROL)"      = "ROL",
              "Zmiany (IB)"       = "IB"
            ),
            selected = "TZ"
          ),
          lc_segmented("ch10_eta_level", "Wielkość efektu", choices = c(
              "η² = 0,01 (mały)"    = "0.01",
              "η² = 0,06 (średni)"  = "0.06",
              "η² = 0,14 (duży)"    = "0.14",
              "η² = 0,30 (b. duży)" = "0.30"
            ), selected = "0.06"),
          uiOutput("ch10_eta_hint")
        ),
        column(8,
          zoom_plot_ui("ch10_eta_plot", height = "240px"),
          uiOutput("ch10_eta_table")
        )
      )
    ),

    lc_p("Przy ustawieniach startowych (η² = 0,06) średnie grup wynoszą 46,9,
      50 i 53,1 przy odchyleniu standardowym 10 w każdej grupie. Różnice
      średnich o około 3 jednostki giną w rozrzucie wewnątrz grup i pudełka
      niemal całkowicie na siebie zachodzą. Dopiero przy η² = 0,30 (średnie
      42,0, 50 i 58,0) grupy wyraźnie się rozsuwają, choć nadal częściowo
      się pokrywają. η² policzone z wylosowanych 90 punktów nie musi równać
      się wartości w populacji. Dla ustawień startowych wynosi 0,02, bo przy
      30 obserwacjach w grupie średnie z próby mocno się wahają."),

    # ========================================================================
    # Domknięcie
    # ========================================================================
    lc_p("Progi Cohena (mały, średni, duży) to konwencja zaproponowana
      w latach 60. jako punkt odniesienia na wypadek, gdy nic lepszego nie
      jest dostępne. Nie są bezwzględnym standardem. To, czy efekt jest ważny,
      zależy od dziedziny i od stawki. Lek, który obniża śmiertelność
      z 10% do 8%, ma według progów Cohena efekt poniżej małego (φ ≈ 0,035),
      a w dużej populacji może uratować wiele osób. Z kolei w ocenie sensorycznej żywności efekt mały bywa
      niezauważalny dla konsumenta. Najlepiej oceniać efekt także w jego
      naturalnych jednostkach: punktach IQ, minutach, tonach z hektara."),

    lc_p("Dla testu proporcji z rozdziału 05 nie wprowadzaliśmy osobnej miary.
      Tam efektem jest sama różnica między odsetkiem w próbie a wartością
      odniesienia, wyrażona w punktach procentowych, najlepiej razem
      z przedziałem ufności."),

    inline_callout(label = "Zasada",
      "Raportuj p-wartość razem z miarą siły efektu i oceniaj efekt w kontekście
       dziedziny, a nie tylko według progów Cohena."
    ),

    lc_p("Ten rozdział zamyka część narracyjną wykładu. Zaczęliśmy od logiki
      testu: zakładamy H₀ i sprawdzamy, jak zaskakujące byłyby przy niej
      nasze dane. Poziom istotności α ustalamy przed analizą, a p-wartość
      to prawdopodobieństwo wyniku co najmniej tak skrajnego jak obserwowany,
      gdy H₀ jest prawdziwa. Nie jest to prawdopodobieństwo, że H₀ jest
      prawdziwa, a brak podstaw do odrzucenia H₀ nie dowodzi, że jest ona
      prawdziwa. Potem poznaliśmy sześć testów dla różnych typów zmiennych,
      a na koniec miary, które mówią, jak duży jest wykryty efekt."),

    lc_p("Pozostaje praktyczne pytanie: który test wybrać do konkretnych danych.
      Odpowiada na nie drzewo decyzyjne w następnym rozdziale. Prowadzi ono
      od typu zmiennych i liczby grup do testu, a ściąga zbiera w jednym
      miejscu wzory, wywołania R i miary siły efektu."),

    lc_chapter_next(
      num       = "11",
      title     = "Drzewo decyzyjne",
      lead      = "mapa wyboru testu — od typu zmiennych do konkretnego testu.",
      target_id = "ch-drzewo"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch10_server <- function(input, output, session) {

  # --------------------------------------------------------------------------
  # Dane domenowe
  # --------------------------------------------------------------------------

  ch10_dist_hints <- list(
    TZ  = "Np. czas inaktywacji enzymu (s) w dwóch temperaturach blanszowania.",
    ROL = "Np. masa ziarna (g) z dwóch odmian pszenicy.",
    IB  = "Np. czas reakcji operatora (ms) na zmianie rannej vs nocnej."
  )

  ch10_d_scenarios <- list(
    TZ = list(
      "0.2" = list(
        x1 = 4.50, x2 = 4.56, s = 0.30,
        kontekst  = "pH jogurtu po fermentacji — różnica 0,06 pH między dwoma zakwasami.",
        jednostka = "pH",
        etyk1 = "Zakwas A", etyk2 = "Zakwas B"
      ),
      "0.5" = list(
        x1 = 20.0, x2 = 22.5, s = 5.0,
        kontekst  = "Wilgotność produktu suszonego (%) — bez vs ze stabilizatorem, różnica 2,5 pp.",
        jednostka = "%",
        etyk1 = "Bez stabilizatora", etyk2 = "Ze stabilizatorem"
      ),
      "0.8" = list(
        x1 = 8.0, x2 = 10.0, s = 2.5,
        kontekst  = "Czas inaktywacji enzymów (min) — blanszowanie 80°C vs 95°C, różnica 2 min.",
        jednostka = "min",
        etyk1 = "Blanszowanie 80°C", etyk2 = "Blanszowanie 95°C"
      ),
      "1.2" = list(
        x1 = 5.0, x2 = 8.6, s = 3.0,
        kontekst  = "Liczba drożdży (×10⁶/mL) — dwa szczepy hodowlane, różnica 3,6 × 10⁶/mL.",
        jednostka = "×10⁶/mL",
        etyk1 = "Szczep A", etyk2 = "Szczep B"
      )
    ),
    ROL = list(
      "0.2" = list(
        x1 = 5.0, x2 = 5.4, s = 2.0,
        kontekst  = "Plon pszenicy (t/ha) — kontrola vs nawożenie lekkie, różnica 0,4 t/ha.",
        jednostka = "t/ha",
        etyk1 = "Kontrola", etyk2 = "Nawożenie lekkie"
      ),
      "0.5" = list(
        x1 = 72, x2 = 78, s = 12,
        kontekst  = "Kiełkowalność nasion (%) — nasiona standardowe vs z podkładem, różnica 6 pp.",
        jednostka = "%",
        etyk1 = "Nasiona standardowe", etyk2 = "Nasiona z podkładem"
      ),
      "0.8" = list(
        x1 = 120, x2 = 140, s = 25,
        kontekst  = "Zawartość azotu w glebie (mg/kg) — bez nawozu vs z nawozem azotowym, różnica 20 mg/kg.",
        jednostka = "mg/kg",
        etyk1 = "Bez nawozu azotowego", etyk2 = "Z nawozem azotowym"
      ),
      "1.2" = list(
        x1 = 5.0, x2 = 8.6, s = 3.0,
        kontekst  = "Wzrost sadzonek w 30 dni (cm) — kontrola vs fitohormon, różnica 3,6 cm.",
        jednostka = "cm",
        etyk1 = "Kontrola", etyk2 = "Fitohormon"
      )
    ),
    IB = list(
      "0.2" = list(
        x1 = 72, x2 = 74, s = 10,
        kontekst  = "Tętno spoczynkowe (ud./min) — bez kofeiny vs po wypiciu herbaty czarnej, różnica 2 ud./min.",
        jednostka = "ud./min",
        etyk1 = "Bez kofeiny", etyk2 = "Po herbacie"
      ),
      "0.5" = list(
        x1 = 60, x2 = 66, s = 12,
        kontekst  = "Wynik egzaminu BHP (pkt) — bez korepetycji vs z korepetycjami, różnica 6 pkt.",
        jednostka = "pkt",
        etyk1 = "Bez korepetycji", etyk2 = "Z korepetycjami"
      ),
      "0.8" = list(
        x1 = 12.0, x2 = 14.0, s = 2.5,
        kontekst  = "Czas reakcji operatora (s) — zmiana ranna vs nocna, różnica 2 s.",
        jednostka = "s",
        etyk1 = "Zmiana ranna", etyk2 = "Zmiana nocna"
      ),
      "1.2" = list(
        x1 = 5.0, x2 = 8.6, s = 3.0,
        kontekst  = "Sen (godz.) — okres egzaminacyjny vs ferie, różnica 3,6 h.",
        jednostka = "godz.",
        etyk1 = "Egzaminy", etyk2 = "Ferie"
      )
    )
  )

  ch10_r_scenarios <- list(
    TZ = list(
      x_label   = "Temperatura fermentacji (°C)",
      y_label   = "pH jogurtu po 24 h",
      seed      = 101,
      x_center  = 42, x_scale = 5,
      y_center  = 4.5, y_scale = 0.2,
      r_sign    = 1
    ),
    ROL = list(
      x_label   = "Opad atmosferyczny (mm)",
      y_label   = "Plon pszenicy (t/ha)",
      seed      = 202,
      x_center  = 450, x_scale = 80,
      y_center  = 5.0, y_scale = 0.8,
      r_sign    = 1
    ),
    IB = list(
      x_label   = "Godziny szkolenia BHP",
      y_label   = "Wypadki na 100 pracowników",
      seed      = 303,
      x_center  = 20, x_scale = 8,
      y_center  = 8.0, y_scale = 2.0,
      r_sign    = -1
    )
  )

  ch10_r_hints <- list(
    TZ  = "Korelacja między temperaturą fermentacji a pH jogurtu.",
    ROL = "Korelacja między opadem atmosferycznym a plonem pszenicy.",
    IB  = "Korelacja ujemna: im więcej godzin szkolenia BHP, tym mniej wypadków."
  )

  ch10_v_scenarios <- list(
    TZ = list(
      grp_a = "Opakowanie A", grp_b = "Opakowanie B",
      stan_pos = "Pleśń", stan_neg = "Brak pleśni",
      hint = "Opakowanie a pojawienie się pleśni na produkcie."
    ),
    ROL = list(
      grp_a = "Odmiana A", grp_b = "Odmiana B",
      stan_pos = "Zachwaszczenie powyżej progu", stan_neg = "Zachwaszczenie poniżej progu",
      hint = "Odmiana rośliny a przekroczenie progu zachwaszczenia pola."
    ),
    IB = list(
      grp_a = "Zaszczepieni", grp_b = "Niezaszczepieni",
      stan_pos = "Infekcja", stan_neg = "Brak infekcji",
      hint = "Zaszczepieni vs niezaszczepieni — odsetek infekcji."
    )
  )

  ch10_v_examples <- list(
    "0.10" = list(p_a = 0.45, p_b = 0.55),
    "0.30" = list(p_a = 0.35, p_b = 0.65),
    "0.50" = list(p_a = 0.25, p_b = 0.75),
    "0.70" = list(p_a = 0.15, p_b = 0.85)
  )

  ch10_eta_scenarios <- list(
    TZ = list(
      grp    = c("Metoda A", "Metoda B", "Metoda C"),
      y_lab  = "Liczba kolonii bakterii (jedn.)",
      mu_ctr = 50, s = 10
    ),
    ROL = list(
      grp    = c("Nawóz A", "Nawóz B", "Nawóz C"),
      y_lab  = "Plon pszenicy (t/ha)",
      mu_ctr = 5.0, s = 1.5
    ),
    IB = list(
      grp    = c("Zmiana ranna", "Zmiana popołudniowa", "Zmiana nocna"),
      y_lab  = "Liczba wypadków na 100 pracowników (rocznie)",
      mu_ctr = 12, s = 4
    )
  )

  ch10_eta_examples <- list(
    "0.01" = list(kontekst = "Grupy dają niemal identyczne wyniki — czynnik symboliczny."),
    "0.06" = list(kontekst = "Grupy zauważalnie się różnią, ale rozrzut wewnątrz grup nadal dominuje."),
    "0.14" = list(kontekst = "Czynnik wyraźnie liczy się — 14% zmienności tłumaczy badany czynnik."),
    "0.30" = list(kontekst = "Dominujący efekt — czynnik wyjaśnia 30% wszystkich różnic.")
  )

  # --------------------------------------------------------------------------
  # Ryc. 10.1
  # --------------------------------------------------------------------------

  output$ch10_dist_hint <- renderUI({
    req(input$ch10_dist_scenario)
    p(tags$em(ch10_dist_hints[[input$ch10_dist_scenario]]))
  })

  zoom_plot_server("ch10_dist_plot", reactive({
    d <- input$ch10_d
    x_lo <- -4
    x_hi <- d + 4
    x_seq <- seq(x_lo, x_hi, length.out = 600)

    df_g1 <- data.frame(x = x_seq, y = dnorm(x_seq, 0, 1), grupa = "Grupa 1 (średnia = 0)")
    df_g2 <- data.frame(x = x_seq, y = dnorm(x_seq, d, 1), grupa = paste0("Grupa 2 (średnia = d = ", d, ")"))

    df_all <- rbind(df_g1, df_g2)
    df_all$grupa <- factor(df_all$grupa, levels = unique(df_all$grupa))

    ggplot(df_all, aes(x = x, y = y, fill = grupa, color = grupa)) +
      geom_area(alpha = 0.35, position = "identity") +
      geom_line(linewidth = 0.9) +
      geom_vline(xintercept = 0, color = col_h0,     linetype = "dashed", linewidth = 0.8) +
      geom_vline(xintercept = d, color = col_reject, linetype = "dashed", linewidth = 0.8) +
      annotate("segment",
               x = 0, xend = d, y = 0.45, yend = 0.45,
               arrow = arrow(ends = "both", length = unit(0.08, "inches")),
               color = "grey30", linewidth = 0.7) +
      annotate("text", x = d / 2, y = 0.48, label = paste0("d = ", d),
               color = "grey20", fontface = "bold", size = 4) +
      scale_fill_manual(values  = c(col_h0, col_reject)) +
      scale_color_manual(values = c(col_h0, col_reject)) +
      labs(x = NULL, y = "Gęstość", fill = NULL, color = NULL) +
      theme(legend.position = "bottom")
  }))

  output$ch10_dist_stats <- renderUI({
    d     <- input$ch10_d
    n     <- input$ch10_n
    se    <- sqrt(2 / n)
    t_val <- d / se
    df_t  <- 2 * n - 2
    p_val <- 2 * pt(-abs(t_val), df_t)

    res   <- format_test_result(p_val)
    fb_type <- if (p_val < 0.05) "warning" else "ok"

    effect_label <- if (abs(d) < 0.2) "pomijalna" else if (abs(d) < 0.5) "mała" else
                    if (abs(d) < 0.8) "średnia" else "duża"

    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        style = "margin-top: 8px;",
        tags$thead(tags$tr(
          tags$th("n / grupę"), tags$th("Cohen's d"), tags$th("t"), tags$th("p")
        )),
        tags$tbody(tags$tr(
          tags$td(n),
          tags$td(paste0(d, "  (", effect_label, ")")),
          tags$td(round(t_val, 2)),
          tags$td(format_p_value(p_val))
        ))
      ),
      lc_feedback(type = fb_type,
        p(style = paste0("color:", res$color, "; font-weight: bold; margin: 0;"),
          res$decision)
      )
    )
  })

  # --------------------------------------------------------------------------
  # Ryc. 10.2: Cohen's d w surowych liczbach
  # --------------------------------------------------------------------------

  zoom_plot_server("ch10_d_plot", reactive({
    req(input$ch10_d_level, input$ch10_d_scenario)
    e <- ch10_d_scenarios[[input$ch10_d_scenario]][[input$ch10_d_level]]
    x_lo <- min(e$x1, e$x2) - 3 * e$s
    x_hi <- max(e$x1, e$x2) + 3 * e$s
    x_seq <- seq(x_lo, x_hi, length.out = 600)

    df_g1 <- data.frame(x = x_seq, y = dnorm(x_seq, e$x1, e$s), grupa = e$etyk1)
    df_g2 <- data.frame(x = x_seq, y = dnorm(x_seq, e$x2, e$s), grupa = e$etyk2)
    df_all <- rbind(df_g1, df_g2)
    df_all$grupa <- factor(df_all$grupa, levels = c(e$etyk1, e$etyk2))

    ggplot(df_all, aes(x = x, y = y, fill = grupa, color = grupa)) +
      geom_area(alpha = 0.35, position = "identity") +
      geom_line(linewidth = 0.9) +
      geom_vline(xintercept = e$x1, color = col_h0,     linetype = "dashed", linewidth = 0.7) +
      geom_vline(xintercept = e$x2, color = col_reject, linetype = "dashed", linewidth = 0.7) +
      scale_fill_manual(values  = c(col_h0, col_reject)) +
      scale_color_manual(values = c(col_h0, col_reject)) +
      labs(x = paste0("Wartość (", e$jednostka, ")"),
           y = "Gęstość", fill = NULL, color = NULL) +
      theme(legend.position = "bottom")
  }))

  output$ch10_d_table <- renderUI({
    req(input$ch10_d_level, input$ch10_d_scenario)
    e <- ch10_d_scenarios[[input$ch10_d_scenario]][[input$ch10_d_level]]
    diff <- e$x2 - e$x1
    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        style = "margin-top: 8px;",
        tags$thead(tags$tr(
          tags$th("Grupa"),
          tags$th(HTML("&xbar; &plusmn; s")),
          tags$th("Różnica"),
          tags$th("d")
        )),
        tags$tbody(
          tags$tr(
            tags$td(e$etyk1),
            tags$td(paste0(e$x1, " ± ", e$s, " ", e$jednostka)),
            tags$td(rowspan = 2, paste0(round(diff, 2), " ", e$jednostka)),
            tags$td(rowspan = 2, input$ch10_d_level)
          ),
          tags$tr(
            tags$td(e$etyk2),
            tags$td(paste0(e$x2, " ± ", e$s, " ", e$jednostka))
          )
        )
      ),
      p(style = "margin-top: 8px;", tags$em(e$kontekst))
    )
  })

  # --------------------------------------------------------------------------
  # Ryc. 10.3: r w surowych liczbach
  # --------------------------------------------------------------------------

  output$ch10_r_hint <- renderUI({
    req(input$ch10_r_scenario)
    p(tags$em(ch10_r_hints[[input$ch10_r_scenario]]))
  })

  zoom_plot_server("ch10_r_plot", reactive({
    req(input$ch10_r_level, input$ch10_r_scenario)
    r_target <- as.numeric(input$ch10_r_level)
    sc <- ch10_r_scenarios[[input$ch10_r_scenario]]
    set.seed(sc$seed)
    n_pts <- 50
    z <- rnorm(n_pts)
    e <- rnorm(n_pts)
    x_raw <- z
    y_raw <- sc$r_sign * r_target * z + sqrt(1 - r_target^2) * e
    x <- sc$x_center + sc$x_scale * x_raw
    y <- sc$y_center + sc$y_scale * y_raw

    df <- data.frame(x = x, y = y)
    r_emp <- cor(df$x, df$y)

    ggplot(df, aes(x = x, y = y)) +
      geom_smooth(method = "lm", se = FALSE, color = upwr_reference,
                  linewidth = 1, formula = y ~ x) +
      geom_point(color = col_effect, alpha = 0.7, size = 2.5) +
      labs(x = sc$x_label,
           y = sc$y_label,
           subtitle = paste0("r empiryczne = ", round(r_emp, 2),
                             "  (zadane |r| = ", r_target, ")"))
  }))

  output$ch10_r_table <- renderUI({
    req(input$ch10_r_level)
    r_val <- as.numeric(input$ch10_r_level)
    r2 <- r_val^2
    opis <- switch(input$ch10_r_level,
      "0.1" = "Punkty rozproszone, ledwo widoczny trend — czynnik tłumaczy 1% zmienności y.",
      "0.3" = "Trend zauważalny, ale duży rozrzut wokół linii — 9% zmienności wyjaśnione.",
      "0.5" = "Wyraźny trend, około ¼ zmienności y wyjaśniona przez x — reszta na inne czynniki.",
      "0.7" = "Mocny związek liniowy, prawie połowa zmienności y wyjaśniona.",
      "0.9" = "Punkty bardzo blisko linii prostej — 81% zmienności y wyjaśnione przez x."
    )
    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        style = "margin-top: 8px;",
        tags$thead(tags$tr(
          tags$th("r"), tags$th("r²"), tags$th("% wariancji wyjaśnione")
        )),
        tags$tbody(tags$tr(
          tags$td(r_val),
          tags$td(round(r2, 2)),
          tags$td(paste0(round(100 * r2), "%"))
        ))
      ),
      p(style = "margin-top: 8px;", tags$em(opis))
    )
  })

  # --------------------------------------------------------------------------
  # Ryc. 10.4: Cramér's V w 2×2
  # --------------------------------------------------------------------------

  output$ch10_v_hint <- renderUI({
    req(input$ch10_v_scenario)
    p(tags$em(ch10_v_scenarios[[input$ch10_v_scenario]]$hint))
  })

  zoom_plot_server("ch10_v_plot", reactive({
    req(input$ch10_v_level, input$ch10_v_scenario)
    e  <- ch10_v_examples[[input$ch10_v_level]]
    sc <- ch10_v_scenarios[[input$ch10_v_scenario]]
    df <- data.frame(
      grupa = factor(c(sc$grp_a, sc$grp_a, sc$grp_b, sc$grp_b),
                     levels = c(sc$grp_a, sc$grp_b)),
      stan  = factor(c(sc$stan_pos, sc$stan_neg, sc$stan_pos, sc$stan_neg),
                     levels = c(sc$stan_neg, sc$stan_pos)),
      pct   = c(e$p_a, 1 - e$p_a, e$p_b, 1 - e$p_b)
    )
    ggplot(df, aes(x = grupa, y = pct, fill = stan)) +
      geom_col(width = 0.55, alpha = 0.9) +
      geom_text(aes(label = paste0(round(100 * pct), "%")),
                position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold", size = 5) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(
        values = setNames(c(col_h0, col_reject), c(sc$stan_neg, sc$stan_pos))
      ) +
      labs(x = NULL, y = "Odsetek w grupie", fill = NULL) +
      theme(legend.position = "bottom")
  }))

  output$ch10_v_table <- renderUI({
    req(input$ch10_v_level, input$ch10_v_scenario)
    e  <- ch10_v_examples[[input$ch10_v_level]]
    sc <- ch10_v_scenarios[[input$ch10_v_scenario]]
    diff_pp <- round(100 * (e$p_b - e$p_a))
    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        style = "margin-top: 8px;",
        tags$thead(tags$tr(
          tags$th("Grupa"),
          tags$th(paste0(sc$stan_pos, " (%)")),
          tags$th(paste0(sc$stan_neg, " (%)")),
          tags$th("V")
        )),
        tags$tbody(
          tags$tr(
            tags$td(sc$grp_a),
            tags$td(paste0(round(100 * e$p_a), "%")),
            tags$td(paste0(round(100 * (1 - e$p_a)), "%")),
            tags$td(rowspan = 2, input$ch10_v_level)
          ),
          tags$tr(
            tags$td(sc$grp_b),
            tags$td(paste0(round(100 * e$p_b), "%")),
            tags$td(paste0(round(100 * (1 - e$p_b)), "%"))
          )
        )
      ),
      p(style = "margin-top: 8px;",
        tags$em(paste0("Różnica między grupami: ", diff_pp, " pp.")))
    )
  })

  # --------------------------------------------------------------------------
  # Ryc. 10.5: η² w ANOVA — trzy grupy
  # --------------------------------------------------------------------------

  output$ch10_eta_hint <- renderUI({
    req(input$ch10_eta_scenario)
    sc <- ch10_eta_scenarios[[input$ch10_eta_scenario]]
    p(tags$em(paste0("3 grupy (", paste(sc$grp, collapse = " / "), ") × ", sc$y_lab, ".")))
  })

  ch10_eta_means <- function(eta, mu_ctr, s) {
    delta <- sqrt(3 * eta * s^2 / (2 * (1 - eta)))
    c(mu_ctr - delta, mu_ctr, mu_ctr + delta)
  }

  zoom_plot_server("ch10_eta_plot", reactive({
    req(input$ch10_eta_level, input$ch10_eta_scenario)
    eta <- as.numeric(input$ch10_eta_level)
    sc  <- ch10_eta_scenarios[[input$ch10_eta_scenario]]
    mus <- ch10_eta_means(eta, sc$mu_ctr, sc$s)
    set.seed(202)
    n_per <- 30
    df <- data.frame(
      grupa = factor(rep(sc$grp, each = n_per), levels = sc$grp),
      y = c(rnorm(n_per, mus[1], sc$s),
            rnorm(n_per, mus[2], sc$s),
            rnorm(n_per, mus[3], sc$s))
    )
    ggplot(df, aes(x = grupa, y = y, fill = grupa)) +
      geom_boxplot(alpha = 0.5, outlier.alpha = 0.5) +
      geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
      scale_fill_upwr() +
      labs(x = NULL, y = sc$y_lab, fill = NULL) +
      theme(legend.position = "none")
  }))

  output$ch10_eta_table <- renderUI({
    req(input$ch10_eta_level, input$ch10_eta_scenario)
    eta <- as.numeric(input$ch10_eta_level)
    sc  <- ch10_eta_scenarios[[input$ch10_eta_scenario]]
    e   <- ch10_eta_examples[[input$ch10_eta_level]]
    mus <- ch10_eta_means(eta, sc$mu_ctr, sc$s)
    pct <- round(100 * eta)
    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-sm",
        style = "margin-top: 8px;",
        tags$thead(tags$tr(
          tags$th("Grupa"),
          tags$th(HTML("&xbar;")),
          tags$th("s"),
          tags$th("η²"),
          tags$th("% wariancji wyjaśnione")
        )),
        tags$tbody(
          tags$tr(
            tags$td(sc$grp[1]), tags$td(round(mus[1], 1)), tags$td(sc$s),
            tags$td(rowspan = 3, input$ch10_eta_level),
            tags$td(rowspan = 3, paste0(pct, "%"))
          ),
          tags$tr(tags$td(sc$grp[2]), tags$td(round(mus[2], 1)), tags$td(sc$s)),
          tags$tr(tags$td(sc$grp[3]), tags$td(round(mus[3], 1)), tags$td(sc$s))
        )
      ),
      p(style = "margin-top: 8px;", tags$em(e$kontekst))
    )
  })

}
