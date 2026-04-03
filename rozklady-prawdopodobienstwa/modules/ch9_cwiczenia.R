# ============================================================================
# CHAPTER 9: Cwiczenia - Rozklady prawdopodobienstwa w BHP
# ============================================================================

# ============================================================================
# UI
# ============================================================================

ch9_ui <- tabPanel("9. \u0106wiczenia",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Poprzednio: quiz z rozpoznawania rozk\u0142ad\u00f3w"
    ),

    div(class = "section-title", "\u0106wiczenia: Rozk\u0142ady prawdopodobie\u0144stwa w BHP"),

    div(class = "narrative",
      p(tags$b("Czas:"), " 90 minut | ",
        tags$b("Narz\u0119dzie:"), " Jamovi | ",
        tags$b("Kierunek:"), " In\u017cynieria Bezpiecze\u0144stwa"),
      p("Trzy bloki zada\u0144 \u2014 od oblicze\u0144 przez rozpoznawanie rozk\u0142ad\u00f3w
        po analiz\u0119 prawdziwych danych. Ka\u017cde zadanie ma ",
        tags$b("ukryte rozwi\u0105zanie"), " \u2014 kliknij przycisk, aby je zobaczy\u0107.")
    ),

    # ======================================================================
    # BLOK 1: Kalkulator rozkladow
    # ======================================================================

    div(class = "section-title", "Blok 1: Kalkulator rozk\u0142ad\u00f3w (25 min)"),

    div(class = "callout-info",
      p("W Jamovi: ", tags$b("Analyses \u2192 Exploration \u2192 Distribution"),
        " (lub modu\u0142 ", tags$code("distrACTION"), ")."),
      p("Dla ka\u017cdego zadania: wybierz odpowiedni rozk\u0142ad, ustaw parametry,
        odczytaj prawdopodobie\u0144stwo.")
    ),

    # --- Zadanie 1 ---
    div(class = "widget-block",
      h4("Zadanie 1 \u2014 Szkolenie BHP"),
      div(class = "narrative",
        p("Test BHP sk\u0142ada si\u0119 z 25 pyta\u0144 prawda/fa\u0142sz. Zaliczenie wymaga ",
          tags$b("minimum 20 poprawnych"), " odpowiedzi. Pracownik nie uczy\u0142 si\u0119
          i odpowiada losowo."),
        tags$ol(
          tags$li("Jaki rozk\u0142ad opisuje liczb\u0119 poprawnych odpowiedzi? Podaj parametry."),
          tags$li("Jakie jest prawdopodobie\u0144stwo, \u017ce pracownik zaliczy test?"),
          tags$li("Jaka jest oczekiwana liczba poprawnych odpowiedzi?"),
          tags$li("Gdyby pr\u00f3g zaliczenia obni\u017cono do 15 \u2014 jak zmieni\u0142oby si\u0119 prawdopodobie\u0144stwo?")
        )
      ),
      actionButton("ch9_ans1", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch9_sol1")
    ),

    # --- Zadanie 2 ---
    div(class = "widget-block",
      h4("Zadanie 2 \u2014 Wypadki przy pracy"),
      div(class = "narrative",
        p("W zak\u0142adzie produkcyjnym dochodzi \u015brednio do ",
          tags$b("2.5 wypadku przy pracy miesi\u0119cznie"), "."),
        tags$ol(
          tags$li("Jaki rozk\u0142ad opisuje liczb\u0119 wypadk\u00f3w w miesi\u0105cu? Podaj parametr."),
          tags$li("Jakie jest prawdopodobie\u0144stwo ", tags$b("dok\u0142adnie 5"), " wypadk\u00f3w w miesi\u0105cu?"),
          tags$li("Jakie jest prawdopodobie\u0144stwo ", tags$b("\u017cadnego"), " wypadku w miesi\u0105cu?"),
          tags$li("Jakie jest prawdopodobie\u0144stwo ", tags$b("wi\u0119cej ni\u017c 4"), " wypadk\u00f3w?"),
          tags$li(tags$em("Trudniejsze:"), " Je\u015bli w kwartale (3 miesi\u0105ce) \u2014 jaki rozk\u0142ad i jakie P(\u226510)?")
        )
      ),
      actionButton("ch9_ans2", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch9_sol2")
    ),

    # --- Zadanie 3 ---
    div(class = "widget-block",
      h4("Zadanie 3 \u2014 Poziom ha\u0142asu"),
      div(class = "narrative",
        p("Pomiary ha\u0142asu na stanowisku w hali monta\u017cowej maj\u0105 rozk\u0142ad ",
          tags$b("N(82, 4)"), " dB (\u015brednia 82 dB, odchylenie standardowe 4 dB).
          Norma BHP: ", tags$b("85 dB"), "."),
        tags$ol(
          tags$li("Jaki procent pomiar\u00f3w przekracza norm\u0119 85 dB?"),
          tags$li("Jaki procent pomiar\u00f3w mie\u015bci si\u0119 w przedziale 78\u201386 dB?"),
          tags$li("Poni\u017cej jakiej warto\u015bci znajduje si\u0119 95% pomiar\u00f3w?"),
          tags$li(tags$em("Trudniejsze:"), " Pracodawca twierdzi, \u017ce \u201eprawie nigdy\u201d
            nie przekracza 90 dB. Zweryfikuj \u2014 jaki procent pomiar\u00f3w > 90 dB?")
        )
      ),
      actionButton("ch9_ans3", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch9_sol3")
    ),

    # --- Zadanie 4 ---
    div(class = "widget-block",
      h4("Zadanie 4 \u2014 Niezawodno\u015b\u0107 czujnika dymu"),
      div(class = "narrative",
        p("Czujnik dymu ma \u015bredni czas bezawaryjnej pracy (",
          tags$b("MTBF"), ") wynosz\u0105cy ", tags$b("365 dni"),
          ". Czas do awarii ma rozk\u0142ad wyk\u0142adniczy."),
        tags$ol(
          tags$li("Jaki jest parametr \u03bb (rate) tego rozk\u0142adu?"),
          tags$li("Jakie jest prawdopodobie\u0144stwo awarii w ci\u0105gu pierwszych 180 dni?"),
          tags$li("Jakie jest prawdopodobie\u0144stwo, \u017ce czujnik przetrwa d\u0142u\u017cej ni\u017c 2 lata (730 dni)?"),
          tags$li(tags$em("Trudniejsze:"), " Czujnik pracuje ju\u017c 200 dni bez awarii.
            Czy to zmienia prawdopodobie\u0144stwo awarii w nast\u0119pnych 180 dniach? Uzasadnij.")
        )
      ),
      actionButton("ch9_ans4", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch9_sol4")
    ),

    # ======================================================================
    # BLOK 2: Rozpoznawanie rozkladow
    # ======================================================================

    div(class = "section-title", "Blok 2: Rozpoznawanie rozk\u0142ad\u00f3w (25 min)"),

    # --- Zadanie 5 ---
    div(class = "widget-block",
      h4("Zadanie 5 \u2014 Kt\u00f3ry to rozk\u0142ad?"),
      div(class = "narrative",
        p("Dla ka\u017cdej sytuacji: ", tags$b("nazwij rozk\u0142ad"), " i ",
          tags$b("podaj parametry"), ". Pracujcie w parach, potem dyskusja.")
      ),
      tags$table(class = "table table-striped table-bordered",
        tags$thead(tags$tr(
          tags$th(""), tags$th("Sytuacja"), tags$th("Rozk\u0142ad"), tags$th("Parametry")
        )),
        tags$tbody(
          tags$tr(tags$td("a)"), tags$td("Inspektor BHP sprawdza 20 stanowisk. Ka\u017cde ma 10% szans na naruszenie przepis\u00f3w. Ile narusze\u0144 znajdzie?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td("b)"), tags$td("\u015arednio 3 alarmy przeciwpo\u017carowe na tydzie\u0144 w galerii handlowej. Ile alarm\u00f3w w nast\u0119pnym tygodniu?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td("c)"), tags$td("Czas oczekiwania na karetk\u0119 pogotowia \u2014 \u015brednia 8 min, odch. std. 2 min, rozk\u0142ad symetryczny"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td("d)"), tags$td("Awaria w fabryce mo\u017ce wyst\u0105pi\u0107 w losowym momencie 8-godzinnej zmiany (ka\u017cdy moment tak samo prawdopodobny)"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td("e)"), tags$td("Z 50 ga\u015bnic w magazynie, 4% jest przeterminowanych. Ile przeterminowanych w losowej kontroli?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td("f)"), tags$td("Inspektor sprawdza kolejne budynki a\u017c do znalezienia pierwszego z naruszeniem przepis\u00f3w ppo\u017c. (szansa naruszenia: 15%). Ile budynk\u00f3w sprawdzi?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td("g)"), tags$td("\u015arednio 1 powa\u017cny wypadek co 20 dni roboczych. Ile dni do nast\u0119pnego wypadku?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td("h)"), tags$td("Waga \u0142adunku na palecie \u2014 \u015brednia 500 kg, odch. std. 30 kg"), tags$td("?"), tags$td("?"))
        )
      ),
      actionButton("ch9_ans5", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch9_sol5")
    ),

    # --- Zadanie 6 ---
    div(class = "widget-block",
      h4("Zadanie 6 \u2014 Trudniejsze: powi\u0105zania mi\u0119dzy rozk\u0142adami"),

      div(class = "narrative",
        tags$b("6a)"), " W firmie kurierskiej \u015brednio dochodzi do ",
          tags$b("4 kolizji drogowych miesi\u0119cznie"), ".",
        tags$ul(
          tags$li("Jaki rozk\u0142ad opisuje ", tags$b("liczb\u0119"), " kolizji w miesi\u0105cu?"),
          tags$li("Jaki rozk\u0142ad opisuje ", tags$b("czas"), " (w dniach) mi\u0119dzy kolejnymi kolizjami?"),
          tags$li("Podaj parametry obu rozk\u0142ad\u00f3w. Jaki jest zwi\u0105zek mi\u0119dzy nimi?")
        ),

        tags$b("6b)"), " Partia 100 \u015brodk\u00f3w ochrony indywidualnej (r\u0119kawice). Wadliwo\u015b\u0107 wynosi 3%.",
        tags$ul(
          tags$li("Jaki rozk\u0142ad opisuje liczb\u0119 wadliwych r\u0119kawic w partii? Podaj parametry."),
          tags$li("Kontroler jako\u015bci sprawdza r\u0119kawice po kolei. Jaki rozk\u0142ad opisuje numer r\u0119kawicy,
            przy kt\u00f3rej natrafi na pierwsz\u0105 wadliw\u0105?"),
          tags$li("Oblicz: P(\u22655 wadliwych w partii) oraz E(numer pierwszej wadliwej).")
        ),

        tags$b("6c)"), " St\u0119\u017cenie py\u0142u na stanowisku ma rozk\u0142ad N(4.2, 0.8) mg/m\u00b3.
          Norma BHP wynosi ", tags$b("5.0 mg/m\u00b3"), ".",
        tags$ul(
          tags$li("Jaki procent pomiar\u00f3w przekracza norm\u0119?"),
          tags$li("Pracodawca musi zapewni\u0107, \u017ce ", tags$b("mniej ni\u017c 5% pomiar\u00f3w"),
            " przekracza norm\u0119. Do jakiej warto\u015bci musia\u0142by obni\u017cy\u0107 \u015brednie st\u0119\u017cenie (przy tym samym \u03c3)?")
        )
      ),
      actionButton("ch9_ans6", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch9_sol6")
    ),

    # ======================================================================
    # BLOK 3: Analiza danych w Jamovi
    # ======================================================================

    div(class = "section-title", "Blok 3: Analiza danych w Jamovi (40 min)"),

    div(class = "callout-info",
      p("Otw\u00f3rz pliki CSV z folderu ", tags$code("dane/"), " w Jamovi.")
    ),

    # --- Zadanie 7 ---
    div(class = "widget-block",
      h4("Zadanie 7 \u2014 Wypadki miesi\u0119cznie"),
      p(class = "text-muted", tags$code("wypadki_miesiecznie.csv")),
      div(class = "narrative",
        tags$ol(
          tags$li("Otw\u00f3rz plik. Zr\u00f3b ", tags$b("histogram"), " zmiennej ",
            tags$code("liczba_wypadkow"),
            " (Exploration \u2192 Descriptives \u2192 Plots \u2192 Histogram)."),
          tags$li("Oblicz ", tags$b("\u015bredni\u0105"), " i ", tags$b("wariancj\u0119"),
            ". Czy s\u0105 zbli\u017cone do siebie?"),
          tags$li("Jaki to sugeruje rozk\u0142ad? Podaj parametr(y)."),
          tags$li("U\u017cywaj\u0105c kalkulatora rozk\u0142ad\u00f3w z parametrem \u03bb = \u015brednia z danych:",
            tags$ul(
              tags$li("Oblicz P(X \u2265 5)"),
              tags$li("Oblicz P(X = 0)")
            )),
          tags$li("Por\u00f3wnaj teoretyczne prawdopodobie\u0144stwa z empirycznymi cz\u0119sto\u015bciami
            w danych (ile miesi\u0119cy mia\u0142o \u22655 wypadk\u00f3w? ile mia\u0142o 0?).")
        )
      ),
      actionButton("ch9_ans7", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch9_sol7")
    ),

    # --- Zadanie 8 ---
    div(class = "widget-block",
      h4("Zadanie 8 \u2014 Ha\u0142as na stanowiskach"),
      p(class = "text-muted", tags$code("halas_stanowiska.csv")),
      div(class = "narrative",
        tags$ol(
          tags$li("Rozdziel dane na dwa stanowiska."),
          tags$li("Dla ka\u017cdego stanowiska zr\u00f3b ", tags$b("histogram"),
            " i oblicz ", tags$b("statystyki opisowe"),
            " (\u015brednia, mediana, odch. std., kwartyle)."),
          tags$li("Kt\u00f3re stanowisko ma rozk\u0142ad bli\u017cszy normalnemu? Po czym to poznajesz?"),
          tags$li("Dla stanowiska o rozk\u0142adzie normalnym: jaki % pomiar\u00f3w przekracza
            norm\u0119 85 dB? (u\u017cyj kalkulatora z parametrami z danych)"),
          tags$li(tags$em("Trudniejsze:"), " Dlaczego stanowisko B mog\u0142oby mie\u0107
            rozk\u0142ad sko\u015bny? Podaj hipotez\u0119 techniczn\u0105.")
        )
      ),
      actionButton("ch9_ans8", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch9_sol8")
    ),

    # --- Zadanie 9 ---
    div(class = "widget-block",
      h4("Zadanie 9 \u2014 Kontrola kask\u00f3w"),
      p(class = "text-muted", tags$code("kontrola_kaskow.csv")),
      div(class = "narrative",
        tags$ol(
          tags$li("Zr\u00f3b ", tags$b("histogram"), " zmiennej ", tags$code("liczba_wadliwych"), "."),
          tags$li("Oblicz \u015bredni\u0105 liczb\u0119 wadliwych kask\u00f3w na parti\u0119."),
          tags$li("Wiedz\u0105c, \u017ce partia liczy 30 kask\u00f3w \u2014 oszacuj prawdopodobie\u0144stwo
            wadliwo\u015bci p (p = \u015brednia / 30)."),
          tags$li("U\u017cywaj\u0105c B(30, p): oblicz P(\u2265 3 wadliwych w jednej partii)."),
          tags$li("Kierownik jako\u015bci chce odrzuca\u0107 parti\u0119, je\u015bli jest \u2265 4 wadliwych.
            Jak cz\u0119sto partia zostanie odrzucona?")
        )
      ),
      actionButton("ch9_ans9", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch9_sol9")
    ),

    # --- Zadanie 10 ---
    div(class = "widget-block",
      h4("Zadanie 10 \u2014 Czas mi\u0119dzy incydentami"),
      p(class = "text-muted", tags$code("czas_miedzy_incydentami.csv")),
      div(class = "narrative",
        tags$ol(
          tags$li("Zr\u00f3b ", tags$b("histogram"), " zmiennej ",
            tags$code("dni_od_poprzedniego"), ". Jaki kszta\u0142t ma rozk\u0142ad?"),
          tags$li("Oblicz ", tags$b("\u015bredni\u0105"), " i ", tags$b("odchylenie standardowe"),
            ". Czy s\u0105 do siebie zbli\u017cone? Co to mo\u017ce sugerowa\u0107?"),
          tags$li("Jaki rozk\u0142ad pasuje do tych danych? Podaj parametr(y)."),
          tags$li("U\u017cywaj\u0105c dopasowanego rozk\u0142adu: oblicz P(nast\u0119pny incydent w ci\u0105gu 7 dni)."),
          tags$li(tags$em("Trudniejsze:"), " Je\u015bli \u015bredni czas mi\u0119dzy
            incydentami wynosi X dni, to ile incydent\u00f3w oczekujemy w ci\u0105gu 30 dni?
            Jaki rozk\u0142ad to opisuje? Oblicz P(\u2265 3 incydenty w miesi\u0105cu).")
        )
      ),
      actionButton("ch9_ans10", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch9_sol10")
    ),

    # ======================================================================
    # PODSUMOWANIE
    # ======================================================================

    div(class = "section-title", "Podsumowanie"),

    div(class = "callout-warning",
      p(tags$b("Po zako\u0144czeniu \u0107wicze\u0144 odpowiedz na pytania:")),
      tags$ol(
        tags$li("Kt\u00f3ry rozk\u0142ad najcz\u0119\u015bciej widzisz w kontek\u015bcie BHP i dlaczego?"),
        tags$li("Jak wygl\u0105da histogram danych z rozk\u0142adu wyk\u0142adniczego?
          Czym r\u00f3\u017cni si\u0119 od normalnego?"),
        tags$li("Jaki jest praktyczny sens \u201ebezpami\u0119ciowo\u015bci\u201d rozk\u0142adu wyk\u0142adniczego
          dla bezpiecze\u0144stwa? (Podpowied\u017a: czy stary czujnik jest mniej niezawodny ni\u017c nowy?)")
      )
    ),

    actionButton("ch9_ans_summary", "Poka\u017c odpowiedzi", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol_summary"),

    br(), br(), br()
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch9_server <- function(input, output, session) {

  # Helper: toggle visibility of solution on button click
  .solution_toggle <- function(btn_id, output_id, content_ui) {
    vis <- reactiveVal(FALSE)
    observeEvent(input[[btn_id]], {
      vis(!vis())
      updateActionButton(session, btn_id,
        label = if (vis()) "Ukryj rozwi\u0105zanie" else "Poka\u017c rozwi\u0105zanie")
    })
    output[[output_id]] <- renderUI({
      if (vis()) div(class = "callout-success", style = "margin-top: 10px;", content_ui)
    })
  }

  # --- Zadanie 1 ---
  .solution_toggle("ch9_ans1", "ch9_sol1", withMathJax(tagList(
    tags$b("a)"), " B(25, 0.5) \u2014 25 pr\u00f3b Bernoulliego, p = 0.5 (losowe T/F)", tags$br(),
    tags$b("b)"), " P(X \u2265 20) = 0.0020 (~0.2%) \u2014 praktycznie niemo\u017cliwe zda\u0107 zgaduj\u0105c", tags$br(),
    tags$b("c)"), " E(X) = np = 25 \u00d7 0.5 = 12.5 odpowiedzi poprawnych", tags$br(),
    tags$b("d)"), " P(X \u2265 15) = 0.2122 (~21%) \u2014 du\u017co \u0142atwiej, ale nadal ma\u0142o prawdopodobne"
  )))

  # --- Zadanie 2 ---
  .solution_toggle("ch9_ans2", "ch9_sol2", withMathJax(tagList(
    tags$b("a)"), " Pois(\u03bb = 2.5) \u2014 zliczamy zdarzenia w ustalonym czasie", tags$br(),
    tags$b("b)"), " P(X = 5) = 0.0668 (~6.7%)", tags$br(),
    tags$b("c)"), " P(X = 0) = 0.0821 (~8.2%)", tags$br(),
    tags$b("d)"), " P(X > 4) = 0.1088 (~10.9%)", tags$br(),
    tags$b("e)"), " W kwartale: Pois(\u03bb = 7.5) (addytywno\u015b\u0107 Poissona). P(X \u2265 10) = 0.2236 (~22.4%)"
  )))

  # --- Zadanie 3 ---
  .solution_toggle("ch9_ans3", "ch9_sol3", withMathJax(tagList(
    tags$b("a)"), " P(X > 85) = 0.2266 (~22.7%) \u2014 ponad 1/5 pomiar\u00f3w przekracza norm\u0119!", tags$br(),
    tags$b("b)"), " P(78 < X < 86) = 0.6827 (~68.3%) \u2014 to dok\u0142adnie regu\u0142a \u03bc\u00b1\u03c3", tags$br(),
    tags$b("c)"), " Kwantyl 95%: 88.6 dB", tags$br(),
    tags$b("d)"), " P(X > 90) = 0.0228 (~2.3%) \u2014 \u201eprawie nigdy\u201d to ~2%, wi\u0119c pracodawca ma racj\u0119, ale nadal to nie jest 0"
  )))

  # --- Zadanie 4 ---
  .solution_toggle("ch9_ans4", "ch9_sol4", withMathJax(tagList(
    tags$b("a)"), " \u03bb = 1/365 \u2248 0.00274 (awarii na dzie\u0144)", tags$br(),
    tags$b("b)"), " P(X < 180) = 0.3893 (~38.9%) \u2014 prawie 2 na 5 czujnik\u00f3w padnie przed p\u00f3\u0142 rokiem", tags$br(),
    tags$b("c)"), " P(X > 730) = 0.1353 (~13.5%)", tags$br(),
    tags$b("d)"), " ", tags$b("Nie zmienia"), " \u2014 rozk\u0142ad wyk\u0142adniczy jest ",
      tags$b("bezpami\u0119ciowy"), ". P(X > 200+180 | X > 200) = P(X > 180) = 0.3893. ",
      "Czas ju\u017c przepracowany nie wp\u0142ywa na przysz\u0142o\u015b\u0107."
  )))

  # --- Zadanie 5 ---
  .solution_toggle("ch9_ans5", "ch9_sol5", tagList(
    tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(
        tags$th(""), tags$th("Rozk\u0142ad"), tags$th("Parametry")
      )),
      tags$tbody(
        tags$tr(tags$td("a)"), tags$td("Dwumianowy"), tags$td("B(20, 0.1)")),
        tags$tr(tags$td("b)"), tags$td("Poissona"), tags$td("Pois(3)")),
        tags$tr(tags$td("c)"), tags$td("Normalny"), tags$td("N(8, 2)")),
        tags$tr(tags$td("d)"), tags$td("Jednostajny ci\u0105g\u0142y"), tags$td("U(0, 8)")),
        tags$tr(tags$td("e)"), tags$td("Dwumianowy"), tags$td("B(50, 0.04)")),
        tags$tr(tags$td("f)"), tags$td("Geometryczny"), tags$td("Geom(0.15)")),
        tags$tr(tags$td("g)"), tags$td("Wyk\u0142adniczy"), tags$td("Exp(\u03bb = 1/20)")),
        tags$tr(tags$td("h)"), tags$td("Normalny"), tags$td("N(500, 30)"))
      )
    )
  ))

  # --- Zadanie 6 ---
  .solution_toggle("ch9_ans6", "ch9_sol6", withMathJax(tagList(
    tags$b("6a) Kolizje drogowe"), tags$br(),
    "Liczba kolizji w miesi\u0105cu: ", tags$b("Pois(\u03bb = 4)"), tags$br(),
    "Czas mi\u0119dzy kolizjami: ", tags$b("Exp(rate = 4/30)"),
      " \u2192 \u015brednio co 7.5 dnia", tags$br(),
    tags$em("Zwi\u0105zek: Poisson zlicza zdarzenia, wyk\u0142adniczy mierzy odst\u0119py mi\u0119dzy nimi."),
    tags$br(), tags$br(),

    tags$b("6b) R\u0119kawice ochronne"), tags$br(),
    "Liczba wadliwych: ", tags$b("B(100, 0.03)"), ", E(X) = 3", tags$br(),
    "P(X \u2265 5) = 0.1821 (~18.2%)", tags$br(),
    "Numer pierwszej wadliwej: ", tags$b("Geom(p = 0.03)"),
      ", E(X) = 1/0.03 \u2248 33.3", tags$br(), tags$br(),

    tags$b("6c) St\u0119\u017cenie py\u0142u"), tags$br(),
    "P(X > 5.0) przy N(4.2, 0.8) = 0.1587 (~15.9%) \u2014 za du\u017co!", tags$br(),
    "Aby P(X > 5.0) < 0.05: potrzebne 5.0 = \u03bc + 1.645 \u00d7 0.8 \u2192 ",
      tags$b("\u03bc \u2264 3.68 mg/m\u00b3"), tags$br(),
    "Trzeba obni\u017cy\u0107 \u015brednie st\u0119\u017cenie z 4.2 do 3.68 mg/m\u00b3 (redukcja o 0.52)."
  )))

  # --- Zadanie 7 ---
  .solution_toggle("ch9_ans7", "ch9_sol7", withMathJax(tagList(
    p(tags$em("Warto\u015bci empiryczne zale\u017c\u0105 od generatora. Poni\u017cej warto\u015bci orientacyjne.")),
    tags$b("a)"), " Histogram: dyskretny, warto\u015bci 0\u20137, skupione wok\u00f3\u0142 2-3", tags$br(),
    tags$b("b)"), " \u015arednia \u2248 2.1, wariancja \u2248 2.4 \u2014 zbli\u017cone do siebie", tags$br(),
    tags$b("c)"), " Poisson z \u03bb \u2248 \u015brednia z danych (~2.1)", tags$br(),
    tags$b("d)"), " Przy \u03bb = 2.1: P(X \u2265 5) \u2248 0.05, P(X = 0) \u2248 0.12", tags$br(),
    tags$b("e)"), " Empiryczne cz\u0119sto\u015bci powinny by\u0107 zbli\u017cone do teoretycznych (\u00b1kilka pp)"
  )))

  # --- Zadanie 8 ---
  .solution_toggle("ch9_ans8", "ch9_sol8", withMathJax(tagList(
    tags$b("Stanowisko A:"), " \u015brednia \u2248 82, mediana \u2248 82, sd \u2248 3.4 \u2014 symetryczne (\u015brednia \u2248 mediana)", tags$br(),
    tags$b("Stanowisko B:"), " \u015brednia \u2248 79, mediana < \u015bredniej, sd \u2248 11 \u2014 sko\u015bne prawo (Q3\u2013mediana > mediana\u2013Q1)", tags$br(), tags$br(),
    tags$b("c)"), " Stanowisko A jest bli\u017csze normalnemu \u2014 histogram dzwonowaty i symetryczny, \u015brednia \u2248 mediana, kwartyle mniej wi\u0119cej r\u00f3wnoodleg\u0142e. Histogram jest najwa\u017cniejszy \u2014 statystyki liczbowe nie wykryj\u0105 np. wielomodalno\u015bci.", tags$br(),
    tags$b("d)"), " Przy N(82, 3.4): P(X > 85) \u2248 19%", tags$br(),
    tags$b("e)"), " Hipoteza: spawalnia generuje sporadyczne szczyty ha\u0142asu (uruchomienie spawarki,
      szlifowanie) \u2014 wi\u0119kszo\u015b\u0107 czasu cicho, ale z ostrymi pikami \u2192 rozk\u0142ad sko\u015bny"
  )))

  # --- Zadanie 9 ---
  .solution_toggle("ch9_ans9", "ch9_sol9", withMathJax(tagList(
    tags$b("a)"), " Histogram: warto\u015bci 0\u20135, najcz\u0119\u015bciej 0\u20132", tags$br(),
    tags$b("b)"), " \u015arednia \u2248 1.6 wadliwych na parti\u0119", tags$br(),
    tags$b("c)"), " p = 1.6/30 \u2248 0.053 (~5.3%)", tags$br(),
    tags$b("d)"), " Przy B(30, 0.053): P(X \u2265 3) \u2248 0.20 (~20%)", tags$br(),
    tags$b("e)"), " P(X \u2265 4) \u2248 0.08 (~8%) \u2014 co ~12-13 partia by\u0142aby odrzucona"
  )))

  # --- Zadanie 10 ---
  .solution_toggle("ch9_ans10", "ch9_sol10", withMathJax(tagList(
    tags$b("a)"), " Histogram: silnie sko\u015bny prawo \u2014 du\u017co kr\u00f3tkich odst\u0119p\u00f3w, ma\u0142o d\u0142ugich", tags$br(),
    tags$b("b)"), " \u015arednia \u2248 14 dni, sd \u2248 13 dni \u2014 \u015brednia \u2248 sd (cecha rozk\u0142adu wyk\u0142adniczego)", tags$br(),
    tags$b("c)"), " \u03bb = 1/14 \u2248 0.071 incydentu/dzie\u0144", tags$br(),
    tags$b("d)"), " P(X < 7) = 1 \u2212 e^(\u22127/14) \u2248 0.39 (~39%)", tags$br(),
    tags$b("e)"), " W 30 dniach: 30/14 \u2248 2.14 incydentu \u2192 Pois(\u03bb = 2.14). P(X \u2265 3) \u2248 0.33 (~33%)"
  )))

  # --- Podsumowanie ---
  .solution_toggle("ch9_ans_summary", "ch9_sol_summary", tagList(
    tags$b("1."), " Poisson i wyk\u0142adniczy \u2014 najcz\u0119stsze w BHP, bo wypadki/incydenty to rzadkie
      zdarzenia w czasie. Normalny \u2014 przy pomiarach \u015brodowiskowych (ha\u0142as, zanieczyszczenia).",
    tags$br(), tags$br(),
    tags$b("2."), " Histogram wyk\u0142adniczy: silnie sko\u015bny prawo, zaczyna si\u0119 wysoko i opada.
      Normalny: symetryczny dzwon.",
    tags$br(), tags$br(),
    tags$b("3."), " Bezpami\u0119ciowo\u015b\u0107: w modelu wyk\u0142adniczym czujnik, kt\u00f3ry pracuje 2 lata,
      ma tak\u0105 sam\u0105 szans\u0119 awarii jutro jak nowy. W praktyce to uproszczenie \u2014 rzeczywiste
      urz\u0105dzenia si\u0119 zu\u017cywaj\u0105 (\u2192 rozk\u0142ad Weibulla). Ale model wyk\u0142adniczy sprawdza si\u0119
      dla awarii losowych (przepi\u0119cia, uderzenia pioruna), nie mechanicznego zu\u017cycia."
  ))

}
