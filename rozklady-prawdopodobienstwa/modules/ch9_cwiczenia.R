# ============================================================================
# CHAPTER 9: Cwiczenia - Rozklady prawdopodobienstwa
# Trzy warianty kierunkowe: BHP, Rolnictwo, Technologia Zywnosci
# ============================================================================

# ============================================================================
# UI
# ============================================================================

ch9_ui <- tabPanel("9. \u0106wiczenia",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Poprzednio: quiz z rozpoznawania rozk\u0142ad\u00f3w"
    ),

    div(class = "section-title", "\u0106wiczenia: Rozk\u0142ady prawdopodobie\u0144stwa"),

    div(class = "narrative",
      p(tags$b("Czas:"), " 90 minut | ", tags$b("Narz\u0119dzie:"), " Jamovi"),
      p("Trzy bloki zada\u0144 \u2014 od oblicze\u0144 przez rozpoznawanie rozk\u0142ad\u00f3w
        po analiz\u0119 prawdziwych danych. Ka\u017cde zadanie ma ",
        tags$b("ukryte rozwi\u0105zanie"), " \u2014 kliknij przycisk, aby je zobaczy\u0107.")
    ),

    div(class = "callout-info",
      selectInput("ch9_kierunek", tags$b("Wybierz wariant dla kierunku:"),
        choices = list(
          "In\u017cynieria Bezpiecze\u0144stwa (BHP)" = "bhp",
          "Rolnictwo"                                = "rol",
          "Technologia \u017cywno\u015bci"            = "zyw"
        ),
        selected = "bhp",
        width = "100%"
      )
    ),

    uiOutput("ch9_content"),

    br(), br(), br()
  ))
)

# ============================================================================
# TRESC ZADAN — funkcje zwracajace tagList per kierunek
# ============================================================================

# Helper: wiersz tabeli zadania 5
.z5row <- function(lit, sytuacja) {
  tags$tr(tags$td(lit), tags$td(sytuacja), tags$td("?"), tags$td("?"))
}

# --------------------------------------------------------------------------
# BHP
# --------------------------------------------------------------------------

.ch9_content_bhp <- function() tagList(

  div(class = "section-title", "Blok 1: Kalkulator rozk\u0142ad\u00f3w (25 min)"),
  div(class = "callout-info",
    p("W Jamovi: ", tags$b("Analyses \u2192 Exploration \u2192 Distribution"),
      " (lub modu\u0142 ", tags$code("distrACTION"), ")."),
    p("Dla ka\u017cdego zadania: wybierz odpowiedni rozk\u0142ad, ustaw parametry, odczytaj prawdopodobie\u0144stwo.")
  ),

  div(class = "widget-block",
    h4("Zadanie 1 \u2014 Szkolenie BHP"),
    div(class = "narrative",
      p("Test BHP sk\u0142ada si\u0119 z 25 pyta\u0144 prawda/fa\u0142sz. Zaliczenie wymaga ",
        tags$b("minimum 20 poprawnych"), " odpowiedzi. Pracownik nie uczy\u0142 si\u0119 i odpowiada losowo."),
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
        tags$li(tags$em("Trudniejsze:"), " Je\u015bli rozpatrujemy ca\u0142y kwarta\u0142 (3 miesi\u0105ce) \u2014 jaki rozk\u0142ad opisuje liczb\u0119 wypadk\u00f3w i jaki ma parametr? Oblicz P(\u226510 wypadk\u00f3w w kwartale).")
      )
    ),
    actionButton("ch9_ans2", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol2")
  ),

  div(class = "widget-block",
    h4("Zadanie 3 \u2014 Poziom ha\u0142asu"),
    div(class = "narrative",
      p("Pomiary ha\u0142asu na stanowisku w hali monta\u017cowej maj\u0105 rozk\u0142ad ",
        tags$b("N(82, 4)"), " dB (\u015brednia 82 dB, odchylenie standardowe 4 dB). Norma BHP: ", tags$b("85 dB"), "."),
      tags$ol(
        tags$li("Jaki procent pomiar\u00f3w przekracza norm\u0119 85 dB?"),
        tags$li("Jaki procent pomiar\u00f3w mie\u015bci si\u0119 w przedziale 78\u201386 dB?"),
        tags$li("Poni\u017cej jakiej warto\u015bci znajduje si\u0119 95% pomiar\u00f3w?"),
        tags$li("Pracodawca twierdzi, \u017ce \u201eprawie nigdy\u201d nie przekracza 90 dB. Zweryfikuj \u2014 jaki procent pomiar\u00f3w > 90 dB?"),
        tags$li(tags$em("Trudniejsze:"), " Pracodawca planuje wymieni\u0107 wentylacj\u0119. Przy jakim poziomie \u015bredniej (przy tym samym \u03c3 = 4 dB) dok\u0142adnie 5% pomiar\u00f3w przekracza\u0142oby norm\u0119 85 dB?")
      )
    ),
    actionButton("ch9_ans3", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 \u2014 Niezawodno\u015b\u0107 czujnika dymu"),
    div(class = "narrative",
      p("Czujnik dymu ma \u015bredni czas bezawaryjnej pracy (", tags$b("MTBF"), ") wynosz\u0105cy ",
        tags$b("365 dni"), ". Czas do awarii ma rozk\u0142ad wyk\u0142adniczy."),
      tags$ol(
        tags$li("Jaki jest parametr \u03bb (rate) tego rozk\u0142adu?"),
        tags$li("Jakie jest prawdopodobie\u0144stwo awarii w ci\u0105gu pierwszych 180 dni?"),
        tags$li("Jakie jest prawdopodobie\u0144stwo, \u017ce czujnik przetrwa d\u0142u\u017cej ni\u017c 2 lata (730 dni)?"),
        tags$li(tags$em("Trudniejsze:"), " Czujnik pracuje ju\u017c 200 dni bez awarii. Czy to zmienia prawdopodobie\u0144stwo awarii w nast\u0119pnych 180 dniach? Uzasadnij (bezpami\u0119ciowo\u015b\u0107).")
      )
    ),
    actionButton("ch9_ans4", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol4")
  ),

  div(class = "section-title", "Blok 2: Rozpoznawanie rozk\u0142ad\u00f3w (25 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 \u2014 Kt\u00f3ry to rozk\u0142ad?"),
    div(class = "narrative",
      p("Dla ka\u017cdej sytuacji: ", tags$b("nazwij rozk\u0142ad"), " i ", tags$b("podaj parametry"),
        ". Pracujcie w parach, potem dyskusja.")
    ),
    tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(tags$th(""), tags$th("Sytuacja"), tags$th("Rozk\u0142ad"), tags$th("Parametry"))),
      tags$tbody(
        .z5row("a)", "Inspektor BHP sprawdza 20 stanowisk. Ka\u017cde ma 10% szans na naruszenie przepis\u00f3w. Ile narusze\u0144 znajdzie?"),
        .z5row("b)", "\u015arednio 3 alarmy przeciwpo\u017carowe na tydzie\u0144 w galerii handlowej. Ile alarm\u00f3w w nast\u0119pnym tygodniu?"),
        .z5row("c)", "Czas oczekiwania na karetk\u0119 pogotowia \u2014 \u015brednia 8 min, odch. std. 2 min, rozk\u0142ad symetryczny"),
        .z5row("d)", "Awaria w fabryce mo\u017ce wyst\u0105pi\u0107 w losowym momencie 8-godzinnej zmiany (ka\u017cdy moment tak samo prawdopodobny)"),
        .z5row("e)", "Z 50 ga\u015bnic w magazynie, 4% jest przeterminowanych. Ile przeterminowanych w losowej kontroli?"),
        .z5row("f)", "Inspektor sprawdza kolejne budynki a\u017c do znalezienia pierwszego z naruszeniem przepis\u00f3w ppo\u017c. (szansa: 15%). Ile budynk\u00f3w sprawdzi?"),
        .z5row("g)", "\u015arednio 1 powa\u017cny wypadek co 20 dni roboczych. Ile dni do nast\u0119pnego wypadku?"),
        .z5row("h)", "Waga \u0142adunku na palecie \u2014 \u015brednia 500 kg, odch. std. 30 kg")
      )
    ),
    actionButton("ch9_ans5", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 \u2014 Trudniejsze: powi\u0105zania mi\u0119dzy rozk\u0142adami"),
    div(class = "narrative",
      tags$b("6a)"), " W firmie kurierskiej \u015brednio dochodzi do ", tags$b("4 kolizji drogowych miesi\u0119cznie"), ".",
      tags$ul(
        tags$li("Jaki rozk\u0142ad opisuje ", tags$b("liczb\u0119"), " kolizji w miesi\u0105cu?"),
        tags$li("Jaki rozk\u0142ad opisuje ", tags$b("czas"), " (w dniach) mi\u0119dzy kolejnymi kolizjami?"),
        tags$li("Podaj parametry obu rozk\u0142ad\u00f3w. Jaki jest zwi\u0105zek mi\u0119dzy nimi?")
      ),
      tags$b("6b)"), " Partia 100 \u015brodk\u00f3w ochrony indywidualnej (r\u0119kawice). Wadliwo\u015b\u0107 wynosi 3%.",
      tags$ul(
        tags$li("Jaki rozk\u0142ad opisuje liczb\u0119 wadliwych r\u0119kawic w partii? Podaj parametry."),
        tags$li("Kontroler jako\u015bci sprawdza r\u0119kawice po kolei. Jaki rozk\u0142ad opisuje numer r\u0119kawicy, przy kt\u00f3rej natrafi na pierwsz\u0105 wadliw\u0105?"),
        tags$li("Oblicz: P(\u22655 wadliwych w partii) oraz E(numer pierwszej wadliwej).")
      ),
      tags$b("6c)"), " St\u0119\u017cenie py\u0142u na stanowisku ma rozk\u0142ad N(4.2, 0.8) mg/m\u00b3. Norma BHP wynosi ", tags$b("5.0 mg/m\u00b3"), ".",
      tags$ul(
        tags$li("Jaki procent pomiar\u00f3w przekracza norm\u0119?"),
        tags$li("Pracodawca musi zapewni\u0107, \u017ce ", tags$b("mniej ni\u017c 5% pomiar\u00f3w"), " przekracza norm\u0119. Do jakiej warto\u015bci musia\u0142by obni\u017cy\u0107 \u015brednie st\u0119\u017cenie (przy tym samym \u03c3)?")
      )
    ),
    actionButton("ch9_ans6", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol6")
  ),

  div(class = "section-title", "Blok 3: Analiza danych w Jamowi (40 min)"),
  div(class = "callout-info",
    p("Otw\u00f3rz pliki CSV z folderu ", tags$code("cwiczenia/dane/"), " w Jamovi.")
  ),

  div(class = "widget-block",
    h4("Zadanie 7 \u2014 Wypadki miesi\u0119cznie"),
    p(class = "text-muted", tags$code("wypadki_miesiecznie.csv")),
    div(class = "narrative",
      tags$ol(
        tags$li("Otw\u00f3rz plik. Zr\u00f3b ", tags$b("histogram"), " zmiennej ", tags$code("liczba_wypadkow"), " (Exploration \u2192 Descriptives \u2192 Plots \u2192 Histogram)."),
        tags$li("Oblicz ", tags$b("\u015bredni\u0105"), " i ", tags$b("wariancj\u0119"), ". Czy s\u0105 zbli\u017cone do siebie?"),
        tags$li("Jaki to sugeruje rozk\u0142ad? Podaj parametr(y)."),
        tags$li("U\u017cywaj\u0105c kalkulatora rozk\u0142ad\u00f3w z \u03bb = \u015brednia z danych: Oblicz P(X \u2265 5) i P(X = 0)."),
        tags$li("Por\u00f3wnaj teoretyczne prawdopodobie\u0144stwa z empirycznymi cz\u0119sto\u015bciami w danych.")
      )
    ),
    actionButton("ch9_ans7", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol7")
  ),

  div(class = "widget-block",
    h4("Zadanie 8 \u2014 Ha\u0142as na stanowiskach"),
    p(class = "text-muted", tags$code("halas_stanowiska.csv")),
    div(class = "narrative",
      tags$ol(
        tags$li("Rozdziel dane na dwa stanowiska (Data \u2192 Filters: ", tags$code("stanowisko == \"A_montaz\""), ")."),
        tags$li("Dla ka\u017cdego stanowiska zr\u00f3b ", tags$b("histogram"), " i oblicz ", tags$b("statystyki opisowe"), " (\u015brednia, mediana, odch. std., skosno\u015b\u0107)."),
        tags$li("Kt\u00f3re stanowisko ma rozk\u0142ad bli\u017cszy normalnemu? Po czym to poznajesz?"),
        tags$li("Dla stanowiska o rozk\u0142adzie normalnym: jaki % pomiar\u00f3w przekracza norm\u0119 85 dB? (u\u017cyj kalkulatora z parametrami z danych)"),
        tags$li(tags$em("Trudniejsze:"), " Dlaczego stanowisko B mog\u0142oby mie\u0107 rozk\u0142ad sko\u015bny? Podaj hipotez\u0119 techniczn\u0105.")
      )
    ),
    actionButton("ch9_ans8", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol8")
  ),

  div(class = "widget-block",
    h4("Zadanie 9 \u2014 Kontrola kask\u00f3w"),
    p(class = "text-muted", tags$code("kontrola_kaskow.csv")),
    div(class = "narrative",
      tags$ol(
        tags$li("Zr\u00f3b ", tags$b("histogram"), " zmiennej ", tags$code("liczba_wadliwych"), "."),
        tags$li("Oblicz \u015bredni\u0105 liczb\u0119 wadliwych kask\u00f3w na parti\u0119."),
        tags$li("Wiedz\u0105c, \u017ce partia liczy 30 kask\u00f3w \u2014 oszacuj p = \u015brednia / 30."),
        tags$li("U\u017cywaj\u0105c B(30, p): oblicz P(\u2265 3 wadliwych w jednej partii)."),
        tags$li("Kierownik jako\u015bci chce odrzuca\u0107 parti\u0119, je\u015bli jest \u2265 4 wadliwych. Jak cz\u0119sto partia zostanie odrzucona?")
      )
    ),
    actionButton("ch9_ans9", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol9")
  ),

  div(class = "widget-block",
    h4("Zadanie 10 \u2014 Czas mi\u0119dzy incydentami"),
    p(class = "text-muted", tags$code("czas_miedzy_incydentami.csv")),
    div(class = "narrative",
      tags$ol(
        tags$li("Zr\u00f3b ", tags$b("histogram"), " zmiennej ", tags$code("dni_od_poprzedniego"), ". Jaki kszta\u0142t ma rozk\u0142ad?"),
        tags$li("Oblicz ", tags$b("\u015bredni\u0105"), " i ", tags$b("odchylenie standardowe"), ". Czy s\u0105 do siebie zbli\u017cone?"),
        tags$li("Jaki rozk\u0142ad pasuje do tych danych? Podaj parametr(y)."),
        tags$li("U\u017cywaj\u0105c dopasowanego rozk\u0142adu: oblicz P(nast\u0119pny incydent w ci\u0105gu 7 dni)."),
        tags$li(tags$em("Trudniejsze:"), " Je\u015bli \u015bredni czas mi\u0119dzy incydentami wynosi X dni, ile incydent\u00f3w oczekujemy w ci\u0105gu 30 dni? Jaki rozk\u0142ad to opisuje? Oblicz P(\u2265 3 incydenty w miesi\u0105cu).")
      )
    ),
    actionButton("ch9_ans10", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol10")
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zako\u0144czeniu \u0107wicze\u0144 odpowiedz na pytania:")),
    tags$ol(
      tags$li("Kt\u00f3ry rozk\u0142ad najcz\u0119\u015bciej widzisz w kontek\u015bcie BHP i dlaczego?"),
      tags$li("Jak wygl\u0105da histogram danych z rozk\u0142adu wyk\u0142adniczego? Czym r\u00f3\u017cni si\u0119 od normalnego?"),
      tags$li("Jaki jest praktyczny sens \u201ebezpami\u0119ciowo\u015bci\u201d rozk\u0142adu wyk\u0142adniczego dla bezpiecze\u0144stwa?")
    )
  ),
  actionButton("ch9_ans_summary", "Poka\u017c odpowiedzi", class = "btn-outline-success btn-sm"),
  uiOutput("ch9_sol_summary")
)

# --------------------------------------------------------------------------
# ROLNICTWO
# --------------------------------------------------------------------------

.ch9_content_rol <- function() tagList(

  div(class = "section-title", "Blok 1: Kalkulator rozk\u0142ad\u00f3w (25 min)"),
  div(class = "callout-info",
    p("W Jamovi: ", tags$b("Analyses \u2192 Exploration \u2192 Distribution"),
      " (lub modu\u0142 ", tags$code("distrACTION"), ")."),
    p("Dla ka\u017cdego zadania: wybierz odpowiedni rozk\u0142ad, ustaw parametry, odczytaj prawdopodobie\u0144stwo.")
  ),

  div(class = "widget-block",
    h4("Zadanie 1 \u2014 Zaraza ziemniaka"),
    div(class = "narrative",
      p("Losowo wybrano ", tags$b("30 ro\u015blin ziemniaka"), " z du\u017cego pola. Na podstawie wieloletnich obserwacji wiadomo, \u017ce ",
        tags$b("20% ro\u015blin"), " na tym polu jest zainfekowanych zar az\u0105."),
      tags$ol(
        tags$li("Jaki rozk\u0142ad opisuje liczb\u0119 zainfekowanych ro\u015blin w pr\u00f3bce? Podaj parametry."),
        tags$li("Jakie jest prawdopodobie\u0144stwo, \u017ce w pr\u00f3bce znajdziemy ", tags$b("co najmniej 8"), " chorych ro\u015blin?"),
        tags$li("Jaka jest oczekiwana liczba chorych ro\u015blin w pr\u00f3bce?"),
        tags$li("Gdyby poziom infekcji wzr\u00f3s\u0142 do 40% \u2014 jak zmieni si\u0119 prawdopodobie\u0144stwo z punktu 2?")
      )
    ),
    actionButton("ch9_ans1", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 \u2014 Po\u0142amania ga\u0142\u0119zi po burzy"),
    div(class = "narrative",
      p("Po sierpniowych burzach na plantacji jab\u0142oni notuje si\u0119 \u015brednio ",
        tags$b("2 po\u0142amania ga\u0142\u0119zi na drzewo"), "."),
      tags$ol(
        tags$li("Jaki rozk\u0142ad opisuje liczb\u0119 po\u0142ama\u0144 na drzewo? Podaj parametr."),
        tags$li("Jakie jest prawdopodobie\u0144stwo, \u017ce losowo wybrane drzewo ma ", tags$b("zero"), " po\u0142ama\u0144?"),
        tags$li("Jakie jest prawdopodobie\u0144stwo ", tags$b("wi\u0119cej ni\u017c 4"), " po\u0142ama\u0144 na jednym drzewie?"),
        tags$li("Jakie jest prawdopodobie\u0144stwo ", tags$b("dok\u0142adnie 2"), " po\u0142ama\u0144?"),
        tags$li(tags$em("Trudniejsze:"), " Sad z\u0142o\u017cony z 3 kwater (ka\u017cda jak osobne drzewo) \u2014 jaki rozk\u0142ad i jakie P(\u226510 po\u0142ama\u0144 \u0142\u0105cznie)?")
      )
    ),
    actionButton("ch9_ans2", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol2")
  ),

  div(class = "widget-block",
    h4("Zadanie 3 \u2014 Plon pszenicy"),
    div(class = "narrative",
      p("Plony pszenicy w regionie maj\u0105 rozk\u0142ad ", tags$b("N(6.2, 0.8)"), " t/ha. Minimalna norma skupu: ", tags$b("5 t/ha"), "."),
      tags$ol(
        tags$li("Jaki procent p\u00f3l osi\u0105ga plon poni\u017cej normy skupu 5 t/ha?"),
        tags$li("Jaki procent p\u00f3l mie\u015bci si\u0119 w przedziale 5.4\u20137.0 t/ha?"),
        tags$li("Poni\u017cej jakiego plonu znajduje si\u0119 najgorsze 5% p\u00f3l?"),
        tags$li("Pracodawca twierdzi, \u017ce \u201eprawie zawsze\u201d plon przekracza 7.5 t/ha. Zweryfikuj \u2014 jaki procent p\u00f3l przekracza 7.5 t/ha?"),
        tags$li(tags$em("Trudniejsze:"), " Przy tym samym \u03c3 = 0.8 t/ha \u2014 do jakiej \u015bredniej nale\u017ca\u0142oby d\u0105\u017cy\u0107, \u017ceby tylko 1% p\u00f3l by\u0142o poni\u017cej normy 5 t/ha?")
      )
    ),
    actionButton("ch9_ans3", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 \u2014 Czas mi\u0119dzy opadami deszczu"),
    div(class = "narrative",
      p("W lipcu w regionie rolniczym \u015bredni czas mi\u0119dzy opadami wynosi ", tags$b("10 dni"), ". Rozk\u0142ad wyk\u0142adniczy."),
      tags$ol(
        tags$li("Jaki jest parametr \u03bb (rate) tego rozk\u0142adu?"),
        tags$li("Jakie jest prawdopodobie\u0144stwo suszy trwaj\u0105cej ", tags$b("d\u0142u\u017cej ni\u017c 15 dni"), "?"),
        tags$li("Jakie jest prawdopodobie\u0144stwo, \u017ce deszcz spadnie w ci\u0105gu ", tags$b("pierwszych 5 dni"), "?"),
        tags$li(tags$em("Trudniejsze:"), " Min\u0119\u0142o ju\u017c 12 dni bez deszczu. Czy to zmienia prawdopodobie\u0144stwo suszy w nast\u0119pnych 10 dniach? Uzasadnij (bezpami\u0119ciowo\u015b\u0107).")
      )
    ),
    actionButton("ch9_ans4", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol4")
  ),

  div(class = "section-title", "Blok 2: Rozpoznawanie rozk\u0142ad\u00f3w (25 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 \u2014 Kt\u00f3ry to rozk\u0142ad?"),
    div(class = "narrative",
      p("Dla ka\u017cdej sytuacji: ", tags$b("nazwij rozk\u0142ad"), " i ", tags$b("podaj parametry"), ". Pracujcie w parach, potem dyskusja.")
    ),
    tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(tags$th(""), tags$th("Sytuacja"), tags$th("Rozk\u0142ad"), tags$th("Parametry"))),
      tags$tbody(
        .z5row("a)", "Agrotechnik sprawdza 15 pr\u00f3bek gleby. Ka\u017cda ma 25% szans na zakwaszenie poni\u017cej normy. Ile pr\u00f3bek b\u0119dzie zakwaszonych?"),
        .z5row("b)", "\u015arednio 4 ataki mszyc na tydzie\u0144 na polu rzepaku. Ile atak\u00f3w w nast\u0119pnym tygodniu?"),
        .z5row("c)", "Masa tysi\u0105ca ziaren pszenicy \u2014 \u015brednia 42 g, odch. std. 3 g, rozk\u0142ad symetryczny"),
        .z5row("d)", "Moment wschodz\u00f3w ro\u015blin w ci\u0105gu 14-dniowego okienka \u2014 ka\u017cdy dzie\u0144 tak samo prawdopodobny"),
        .z5row("e)", "Z 200 nasion w worku 3% jest niekielkuj\u0105cych. Ile takich nasion w losowej pr\u00f3bce 50 sztuk?"),
        .z5row("f)", "Agronom sprawdza kolejne dzia\u0142ki a\u017c do znalezienia pierwszej z erozj\u0105 gleby (szansa: 10%). Ile dzia\u0142ek sprawdzi?"),
        .z5row("g)", "\u015arednio 1 wyst\u0105pienie szkodnik\u00f3w co 7 dni na polu. Ile dni do nast\u0119pnego wyst\u0105pienia?"),
        .z5row("h)", "Wilgotno\u015b\u0107 gleby na polu \u2014 \u015brednia 35%, odch. std. 5%")
      )
    ),
    actionButton("ch9_ans5", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 \u2014 Trudniejsze: powi\u0105zania mi\u0119dzy rozk\u0142adami"),
    div(class = "narrative",
      tags$b("6a)"), " Na du\u017cym polu kukurydzy \u015brednio wyst\u0119puje ", tags$b("5 ognisk chwast\u00f3w tygodniowo"), ".",
      tags$ul(
        tags$li("Jaki rozk\u0142ad opisuje ", tags$b("liczb\u0119"), " ognisk w tygodniu?"),
        tags$li("Jaki rozk\u0142ad opisuje ", tags$b("czas"), " (w dniach) mi\u0119dzy kolejnymi ogniskami?"),
        tags$li("Podaj parametry obu rozk\u0142ad\u00f3w. Jaki jest zwi\u0105zek mi\u0119dzy nimi?")
      ),
      tags$b("6b)"), " Partia 500 sadzonek pomidor\u00f3w. Wadliwo\u015b\u0107 (uszkodzone korzenie) wynosi 4%.",
      tags$ul(
        tags$li("Jaki rozk\u0142ad opisuje liczb\u0119 wadliwych sadzonek w 100 sztukach? Podaj parametry."),
        tags$li("Ogrodnik sadzi sadzonki po kolei. Jaki rozk\u0142ad opisuje numer sadzonki, przy kt\u00f3rej natrafi na pierwsz\u0105 wadliw\u0105?"),
        tags$li("Oblicz: P(\u22655 wadliwych w partii 100 sztuk) oraz E(numer pierwszej wadliwej).")
      ),
      tags$b("6c)"), " Zawarto\u015b\u0107 azotu w glebie ma rozk\u0142ad N(1.8, 0.4) % s.m. Norma minimalna dla pszenicy: ", tags$b("1.0% s.m."), ".",
      tags$ul(
        tags$li("Jaki procent p\u00f3l spe\u0142nia norm\u0119 minimaln\u0105?"),
        tags$li("Agrotechnik chce, \u017ceby ", tags$b("co najmniej 99% p\u00f3l"), " spe\u0142nia\u0142o norm\u0119. Do jakiej warto\u015bci musi wzrosn\u0105\u0107 \u015brednia (przy tym samym \u03c3 = 0.4)?")
      )
    ),
    actionButton("ch9_ans6", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol6")
  ),

  div(class = "section-title", "Blok 3: Analiza danych w Jamovi (40 min)"),
  div(class = "callout-info",
    p("Otw\u00f3rz pliki CSV z folderu ", tags$code("cwiczenia/rolnictwo/dane/"), " w Jamovi.")
  ),

  div(class = "widget-block",
    h4("Zadanie 7 \u2014 Wyst\u0105pienia szkodnik\u00f3w"),
    p(class = "text-muted", tags$code("szkodniki_tygodniowo.csv")),
    div(class = "narrative",
      tags$ol(
        tags$li("Otw\u00f3rz plik. Zr\u00f3b ", tags$b("histogram"), " zmiennej ", tags$code("liczba_wystapien"), " (Exploration \u2192 Descriptives \u2192 Plots \u2192 Histogram)."),
        tags$li("Oblicz ", tags$b("\u015bredni\u0105"), " i ", tags$b("wariancj\u0119"), ". Czy s\u0105 zbli\u017cone do siebie?"),
        tags$li("Jaki to sugeruje rozk\u0142ad? Podaj parametr \u03bb."),
        tags$li("U\u017cywaj\u0105c kalkulatora z \u03bb = \u015brednia z danych: oblicz P(X \u2265 5) i P(X = 0)."),
        tags$li("Por\u00f3wnaj teoretyczne prawdopodobie\u0144stwa z empirycznymi cz\u0119sto\u015bciami w danych.")
      )
    ),
    actionButton("ch9_ans7", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol7")
  ),

  div(class = "widget-block",
    h4("Zadanie 8 \u2014 Plony na dw\u00f3ch odmianach"),
    p(class = "text-muted", tags$code("plony_odmiany.csv")),
    div(class = "narrative",
      tags$ol(
        tags$li("Rozdziel dane na dwie odmiany pszenicy."),
        tags$li("Dla ka\u017cdej odmiany zr\u00f3b ", tags$b("histogram"), " i oblicz ", tags$b("statystyki opisowe"), " (\u015brednia, mediana, odch. std., kwartyle)."),
        tags$li("Kt\u00f3ra odmiana ma rozk\u0142ad bli\u017cszy normalnemu? Po czym to poznajesz?"),
        tags$li("Dla odmiany o rozk\u0142adzie normalnym: jaki % p\u00f3l jest poni\u017cej normy skupu 5 t/ha? (u\u017cyj kalkulatora z parametrami z danych)"),
        tags$li(tags$em("Trudniejsze:"), " Dlaczego druga odmiana mog\u0142aby mie\u0107 rozk\u0142ad sko\u015bny? Podaj hipotez\u0119 agronomiczn\u0105.")
      )
    ),
    actionButton("ch9_ans8", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol8")
  ),

  div(class = "widget-block",
    h4("Zadanie 9 \u2014 Kontrola jako\u015bci nasion"),
    p(class = "text-muted", tags$code("kontrola_nasion.csv")),
    div(class = "narrative",
      tags$ol(
        tags$li("Zr\u00f3b ", tags$b("histogram"), " zmiennej ", tags$code("liczba_wadliwych"), "."),
        tags$li("Oblicz \u015bredni\u0105 liczb\u0119 wadliwych nasion na parti\u0119."),
        tags$li("Wiedz\u0105c, \u017ce partia liczy 50 nasion \u2014 oszacuj p = \u015brednia / 50."),
        tags$li("U\u017cywaj\u0105c B(50, p): oblicz P(\u2265 4 wadliwych w jednej partii)."),
        tags$li("Magazynier odrzuca parti\u0119, je\u015bli jest \u2265 5 wadliwych nasion. Jak cz\u0119sto partia zostanie odrzucona?")
      )
    ),
    actionButton("ch9_ans9", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol9")
  ),

  div(class = "widget-block",
    h4("Zadanie 10 \u2014 Czas mi\u0119dzy deszczami"),
    p(class = "text-muted", tags$code("czas_miedzy_deszczami.csv")),
    div(class = "narrative",
      tags$ol(
        tags$li("Zr\u00f3b ", tags$b("histogram"), " zmiennej ", tags$code("dni_od_poprzedniego"), ". Jaki kszta\u0142t ma rozk\u0142ad?"),
        tags$li("Oblicz ", tags$b("\u015bredni\u0105"), " i ", tags$b("odchylenie standardowe"), ". Czy s\u0105 zbli\u017cone do siebie?"),
        tags$li("Jaki rozk\u0142ad pasuje do tych danych? Podaj parametr \u03bb."),
        tags$li("Oblicz P(nast\u0119pny deszcz za wi\u0119cej ni\u017c 14 dni)."),
        tags$li(tags$em("Trudniejsze:"), " Ile opad\u00f3w oczekujemy w sezonie 90-dniowym? Jaki rozk\u0142ad i jaki \u03bb? Oblicz P(\u2265 15 opad\u00f3w w sezonie).")
      )
    ),
    actionButton("ch9_ans10", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol10")
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zako\u0144czeniu \u0107wicze\u0144 odpowiedz na pytania:")),
    tags$ol(
      tags$li("Kt\u00f3ry rozk\u0142ad najcz\u0119\u015bciej pojawia si\u0119 w kontek\u015bcie rolniczym i dlaczego?"),
      tags$li("Co m\u00f3wi nam \u015brednia i wariancja, gdy s\u0105 do siebie zbli\u017cone?"),
      tags$li("Dlaczego bezpami\u0119ciowo\u015b\u0107 rozk\u0142adu wyk\u0142adniczego jest zaskakuj\u0105ca w kontek\u015bcie suszy?")
    )
  ),
  actionButton("ch9_ans_summary", "Poka\u017c odpowiedzi", class = "btn-outline-success btn-sm"),
  uiOutput("ch9_sol_summary")
)

# --------------------------------------------------------------------------
# TECHNOLOGIA ZYWNOSCI
# --------------------------------------------------------------------------

.ch9_content_zyw <- function() tagList(

  div(class = "section-title", "Blok 1: Kalkulator rozk\u0142ad\u00f3w (25 min)"),
  div(class = "callout-info",
    p("W Jamovi: ", tags$b("Analyses \u2192 Exploration \u2192 Distribution"),
      " (lub modu\u0142 ", tags$code("distrACTION"), ")."),
    p("Dla ka\u017cdego zadania: wybierz odpowiedni rozk\u0142ad, ustaw parametry, odczytaj prawdopodobie\u0144stwo.")
  ),

  div(class = "widget-block",
    h4("Zadanie 1 \u2014 Kontrola szczelno\u015bci opakowa\u0144"),
    div(class = "narrative",
      p("W linii produkcyjnej d\u017cem\u00f3w losowo pobierana jest pr\u00f3bka ", tags$b("40 s\u0142oik\u00f3w"), ". Wiadomo, \u017ce \u015brednio ",
        tags$b("8% s\u0142oik\u00f3w"), " ma nieszczeln\u0105 zakr\u0119tk\u0119."),
      tags$ol(
        tags$li("Jaki rozk\u0142ad opisuje liczb\u0119 nieszczelnych s\u0142oik\u00f3w w pr\u00f3bce? Podaj parametry."),
        tags$li("Jakie jest prawdopodobie\u0144stwo, \u017ce w pr\u00f3bce znajdziemy ", tags$b("co najmniej 5"), " nieszczelnych s\u0142oik\u00f3w?"),
        tags$li("Jaka jest oczekiwana liczba wadliwych opakowa\u0144 w pr\u00f3bce?"),
        tags$li("Gdyby wadliwo\u015b\u0107 spad\u0142a do 3% po naprawie maszyny \u2014 jak zmieni si\u0119 prawdopodobie\u0144stwo z punktu 2?")
      )
    ),
    actionButton("ch9_ans1", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 \u2014 Reklamacje konsument\u00f3w"),
    div(class = "narrative",
      p("Producent napoj\u00f3w energetycznych otrzymuje \u015brednio ", tags$b("4 reklamacje tygodniowo"), " dotycz\u0105ce jako\u015bci smaku."),
      tags$ol(
        tags$li("Jaki rozk\u0142ad opisuje liczb\u0119 reklamacji w tygodniu? Podaj parametr."),
        tags$li("Jakie jest prawdopodobie\u0144stwo ", tags$b("dok\u0142adnie 0"), " reklamacji w tygodniu?"),
        tags$li("Jakie jest prawdopodobie\u0144stwo ", tags$b("wi\u0119cej ni\u017c 6"), " reklamacji?"),
        tags$li("Jakie jest prawdopodobie\u0144stwo ", tags$b("dok\u0142adnie 4"), " reklamacji?"),
        tags$li(tags$em("Trudniejsze:"), " Je\u015bli badamy ca\u0142y miesi\u0105c (4 tygodnie) \u2014 jaki rozk\u0142ad i jakie P(\u226520 reklamacji)?")
      )
    ),
    actionButton("ch9_ans2", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol2")
  ),

  div(class = "widget-block",
    h4("Zadanie 3 \u2014 Zawarto\u015b\u0107 soli w w\u0119dlinie"),
    div(class = "narrative",
      p("Pomiary zawarto\u015bci soli w partiach szynki maj\u0105 rozk\u0142ad ", tags$b("N(2.1, 0.3)"), " g/100g. Norma maksymalna: ", tags$b("2.5 g/100g"), "."),
      tags$ol(
        tags$li("Jaki procent partii przekracza norm\u0119 maksymaln\u0105 2.5 g/100g?"),
        tags$li("Jaki procent partii mie\u015bci si\u0119 w przedziale 1.8\u20132.4 g/100g?"),
        tags$li("Poni\u017cej jakiej warto\u015bci znajduje si\u0119 95% partii?"),
        tags$li("Producent twierdzi, \u017ce \u201eprawie nigdy\u201d nie przekracza 2.8 g/100g. Zweryfikuj \u2014 jaki procent partii > 2.8?"),
        tags$li(tags$em("Trudniejsze:"), " Inspektor sanitarny wymaga, \u017ceby ", tags$b("mniej ni\u017c 5% partii"), " przekracza\u0142o norm\u0119 2.5 g/100g. Do jakiej warto\u015bci musia\u0142aby spa\u015b\u0107 \u015brednia (przy tym samym \u03c3 = 0.3)?")
      )
    ),
    actionButton("ch9_ans3", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 \u2014 Trwa\u0142o\u015b\u0107 jogurtu po otwarciu"),
    div(class = "narrative",
      p("Czas do zepsucia jogurtu po otwarciu (w warunkach lodowki) ma rozk\u0142ad wyk\u0142adniczy ze \u015bredni\u0105 ", tags$b("8 dni"), "."),
      tags$ol(
        tags$li("Jaki jest parametr \u03bb (rate) tego rozk\u0142adu?"),
        tags$li("Jakie jest prawdopodobie\u0144stwo, \u017ce jogurt zepsuje si\u0119 ", tags$b("w ci\u0105gu pierwszych 3 dni"), "?"),
        tags$li("Jakie jest prawdopodobie\u0144stwo, \u017ce jogurt ", tags$b("przetrwa d\u0142u\u017cej ni\u017c 10 dni"), " po otwarciu?"),
        tags$li(tags$em("Trudniejsze:"), " Konsument otworzy\u0142 jogurt 5 dni temu i nadal nie jest zepsuty. Czy zmienia to prawdopodobie\u0144stwo zepsucia w ci\u0105gu kolejnych 3 dni? Uzasadnij (bezpami\u0119ciowo\u015b\u0107).")
      )
    ),
    actionButton("ch9_ans4", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol4")
  ),

  div(class = "section-title", "Blok 2: Rozpoznawanie rozk\u0142ad\u00f3w (25 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 \u2014 Kt\u00f3ry to rozk\u0142ad?"),
    div(class = "narrative",
      p("Dla ka\u017cdej sytuacji: ", tags$b("nazwij rozk\u0142ad"), " i ", tags$b("podaj parametry"), ". Pracujcie w parach, potem dyskusja.")
    ),
    tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(tags$th(""), tags$th("Sytuacja"), tags$th("Rozk\u0142ad"), tags$th("Parametry"))),
      tags$tbody(
        .z5row("a)", "Kontroler sprawdza 25 butelek soku. Ka\u017cda ma 5% szans na b\u0142\u0105d nape\u0142nienia. Ile b\u0142\u0119dnych?"),
        .z5row("b)", "\u015arednio 3 awarie linii produkcyjnej dziennie w zak\u0142adzie. Ile awarii jutro?"),
        .z5row("c)", "Masa netto opakowania cukru \u2014 \u015brednia 1000 g, odch. std. 5 g, rozk\u0142ad symetryczny"),
        .z5row("d)", "Moment pobrania pr\u00f3bki z ta\u015bmy produkcyjnej w ci\u0105gu 60-minutowej zmiany (ka\u017cda minuta tak samo prawdopodobna)"),
        .z5row("e)", "Z partii 200 tabliczek czekolady, 2% ma wady powlekania. Ile wadliwych w losowej pr\u00f3bce 30 sztuk?"),
        .z5row("f)", "Inspektor sprawdza kolejne partie jogurt\u00f3w a\u017c do znalezienia pierwszej przeterminowanej (szansa: 8%). Ile partii sprawdzi?"),
        .z5row("g)", "\u015arednio 1 usterka linii pakuj\u0105cej co 4 godziny. Ile godzin do nast\u0119pnej usterki?"),
        .z5row("h)", "Zawarto\u015b\u0107 bia\u0142ka w proszku mlecznym \u2014 \u015brednia 26%, odch. std. 1.5%")
      )
    ),
    actionButton("ch9_ans5", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 \u2014 Trudniejsze: powi\u0105zania mi\u0119dzy rozk\u0142adami"),
    div(class = "narrative",
      tags$b("6a)"), " Na linii produkcyjnej serka topionego \u015brednio wyst\u0119puje ", tags$b("6 usterek opakowania dziennie"), ".",
      tags$ul(
        tags$li("Jaki rozk\u0142ad opisuje ", tags$b("liczb\u0119"), " usterek dziennie?"),
        tags$li("Jaki rozk\u0142ad opisuje ", tags$b("czas"), " (w godzinach) mi\u0119dzy kolejnymi usterkami?"),
        tags$li("Podaj parametry obu rozk\u0142ad\u00f3w. Jaki jest zwi\u0105zek mi\u0119dzy nimi?")
      ),
      tags$b("6b)"), " Partia 300 puszek konserw. Wadliwo\u015b\u0107 (z\u0142e zamkni\u0119cie) wynosi 2%.",
      tags$ul(
        tags$li("Jaki rozk\u0142ad opisuje liczb\u0119 wadliwych puszek w 200 sztukach? Podaj parametry."),
        tags$li("Kontroler sprawdza puszki po kolei. Jaki rozk\u0142ad opisuje numer puszki, przy kt\u00f3rej natrafi na pierwsz\u0105 wadliw\u0105?"),
        tags$li("Oblicz: P(\u22655 wadliwych w partii 200 sztuk) oraz E(numer pierwszej wadliwej).")
      ),
      tags$b("6c)"), " Zawarto\u015b\u0107 t\u0142uszczu w mleku pe\u0142nym ma rozk\u0142ad N(3.6, 0.2) %. Norma minimalna: ", tags$b("co najmniej 3.2%"), ".",
      tags$ul(
        tags$li("Jaki procent partii spe\u0142nia norm\u0119 minimaln\u0105?"),
        tags$li("Producent chce zapewni\u0107, \u017ce ", tags$b("mniej ni\u017c 1% partii"), " nie spe\u0142nia normy. Jaka minimalna \u015brednia jest potrzebna (przy tym samym \u03c3 = 0.2)?")
      )
    ),
    actionButton("ch9_ans6", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol6")
  ),

  div(class = "section-title", "Blok 3: Analiza danych w Jamovi (40 min)"),
  div(class = "callout-info",
    p("Otw\u00f3rz pliki CSV z folderu ", tags$code("cwiczenia/zywnosc/dane/"), " w Jamovi.")
  ),

  div(class = "widget-block",
    h4("Zadanie 7 \u2014 Reklamacje tygodniowo"),
    p(class = "text-muted", tags$code("reklamacje_tygodniowo.csv")),
    div(class = "narrative",
      tags$ol(
        tags$li("Otw\u00f3rz plik. Zr\u00f3b ", tags$b("histogram"), " zmiennej ", tags$code("liczba_reklamacji"), " (Exploration \u2192 Descriptives \u2192 Plots \u2192 Histogram)."),
        tags$li("Oblicz ", tags$b("\u015bredni\u0105"), " i ", tags$b("wariancj\u0119"), ". Czy s\u0105 zbli\u017cone do siebie?"),
        tags$li("Jaki to sugeruje rozk\u0142ad? Podaj parametr \u03bb."),
        tags$li("U\u017cywaj\u0105c kalkulatora z \u03bb = \u015brednia z danych: oblicz P(X \u2265 7) i P(X = 0)."),
        tags$li("Por\u00f3wnaj teoretyczne prawdopodobie\u0144stwa z empirycznymi cz\u0119sto\u015bciami w danych.")
      )
    ),
    actionButton("ch9_ans7", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol7")
  ),

  div(class = "widget-block",
    h4("Zadanie 8 \u2014 Masa netto dw\u00f3ch linii produkcyjnych"),
    p(class = "text-muted", tags$code("masa_netto_linie.csv")),
    div(class = "narrative",
      tags$ol(
        tags$li("Rozdziel dane na dwie linie produkcyjne."),
        tags$li("Dla ka\u017cdej linii zr\u00f3b ", tags$b("histogram"), " i oblicz ", tags$b("statystyki opisowe"), " (\u015brednia, mediana, odch. std., kwartyle)."),
        tags$li("Kt\u00f3ra linia ma rozk\u0142ad bli\u017cszy normalnemu? Po czym to poznajesz?"),
        tags$li("Dla linii o rozk\u0142adzie normalnym: jaki % opakowa\u0144 jest poni\u017cej deklarowanej masy 995 g? (u\u017cyj kalkulatora z parametrami z danych)"),
        tags$li(tags$em("Trudniejsze:"), " Dlaczego druga linia mog\u0142aby mie\u0107 rozk\u0142ad sko\u015bny? Podaj hipotez\u0119 technologiczn\u0105.")
      )
    ),
    actionButton("ch9_ans8", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol8")
  ),

  div(class = "widget-block",
    h4("Zadanie 9 \u2014 Kontrola wadliwo\u015bci opakowa\u0144"),
    p(class = "text-muted", tags$code("kontrola_opakowania.csv")),
    div(class = "narrative",
      tags$ol(
        tags$li("Zr\u00f3b ", tags$b("histogram"), " zmiennej ", tags$code("liczba_wadliwych"), "."),
        tags$li("Oblicz \u015bredni\u0105 liczb\u0119 wadliwych opakowa\u0144 na parti\u0119."),
        tags$li("Wiedz\u0105c, \u017ce partia liczy 40 opakowa\u0144 \u2014 oszacuj p = \u015brednia / 40."),
        tags$li("U\u017cywaj\u0105c B(40, p): oblicz P(\u2265 4 wadliwych w jednej partii)."),
        tags$li("Kierownik jako\u015bci odrzuca parti\u0119, je\u015bli jest \u2265 5 wadliwych. Jak cz\u0119sto partia zostanie odrzucona?")
      )
    ),
    actionButton("ch9_ans9", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol9")
  ),

  div(class = "widget-block",
    h4("Zadanie 10 \u2014 Czas mi\u0119dzy awariami linii"),
    p(class = "text-muted", tags$code("czas_miedzy_awariami.csv")),
    div(class = "narrative",
      tags$ol(
        tags$li("Zr\u00f3b ", tags$b("histogram"), " zmiennej ", tags$code("godziny_od_poprzedniej"), ". Jaki kszta\u0142t ma rozk\u0142ad?"),
        tags$li("Oblicz ", tags$b("\u015bredni\u0105"), " i ", tags$b("odchylenie standardowe"), ". Czy s\u0105 zbli\u017cone do siebie?"),
        tags$li("Jaki rozk\u0142ad pasuje do tych danych? Podaj parametr \u03bb."),
        tags$li("Oblicz P(nast\u0119pna awaria w ci\u0105gu 2 godzin)."),
        tags$li(tags$em("Trudniejsze:"), " Ile awarii oczekujemy w ci\u0105gu tygodnia pracy (40 godzin)? Jaki rozk\u0142ad i jaki \u03bb? Oblicz P(\u2265 5 awarii w tygodniu).")
      )
    ),
    actionButton("ch9_ans10", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch9_sol10")
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zako\u0144czeniu \u0107wicze\u0144 odpowiedz na pytania:")),
    tags$ol(
      tags$li("Kt\u00f3ry rozk\u0142ad najcz\u0119\u015bciej pojawia si\u0119 w kontroli jako\u015bci \u017cywno\u015bci i dlaczego?"),
      tags$li("Co oznacza w praktyce, gdy \u015brednia i wariancja liczby reklamacji s\u0105 zbli\u017cone?"),
      tags$li("Jaki jest praktyczny sens bezpami\u0119ciowo\u015bci rozk\u0142adu wyk\u0142adniczego dla trwa\u0142o\u015bci produkt\u00f3w?")
    )
  ),
  actionButton("ch9_ans_summary", "Poka\u017c odpowiedzi", class = "btn-outline-success btn-sm"),
  uiOutput("ch9_sol_summary")
)

# ============================================================================
# ROZWIAZANIA — listy per kierunek
# ============================================================================

.ch9_solutions <- list(

  bhp = list(
    sol1 = withMathJax(tagList(
      tags$b("a)"), " B(25, 0.5) \u2014 25 pr\u00f3b Bernoulliego, p = 0.5 (losowe T/F)", tags$br(),
      tags$b("b)"), " P(X \u2265 20) = 0.0020 (~0.2%) \u2014 praktycznie niemo\u017cliwe zda\u0107 zgaduj\u0105c", tags$br(),
      tags$b("c)"), " E(X) = np = 25 \u00d7 0.5 = 12.5 odpowiedzi poprawnych", tags$br(),
      tags$b("d)"), " P(X \u2265 15) = 0.2122 (~21%) \u2014 du\u017co \u0142atwiej, ale nadal niezbyt prawdopodobne"
    )),
    sol2 = withMathJax(tagList(
      tags$b("a)"), " Pois(\u03bb = 2.5) \u2014 zliczamy zdarzenia w ustalonym czasie", tags$br(),
      tags$b("b)"), " P(X = 5) = 0.0668 (~6.7%)", tags$br(),
      tags$b("c)"), " P(X = 0) = 0.0821 (~8.2%)", tags$br(),
      tags$b("d)"), " P(X > 4) = 0.1088 (~10.9%)", tags$br(),
      tags$b("e)"), " W kwartale: Pois(\u03bb = 7.5). P(X \u2265 10) = 0.2236 (~22.4%)"
    )),
    sol3 = withMathJax(tagList(
      tags$b("a)"), " P(X > 85) = 0.2266 (~22.7%) \u2014 ponad 1/5 pomiar\u00f3w przekracza norm\u0119!", tags$br(),
      tags$b("b)"), " P(78 < X < 86) = 0.6827 (~68.3%) \u2014 regu\u0142a \u03bc\u00b1\u03c3", tags$br(),
      tags$b("c)"), " Kwantyl 95%: 88.6 dB", tags$br(),
      tags$b("d)"), " P(X > 90) = 0.0228 (~2.3%) \u2014 prawie nigdy, ale to nie jest 0", tags$br(),
      tags$b("e)"), " 85 = \u03bc + 1.645 \u00d7 4 \u2192 ", tags$b("\u03bc \u2264 78.4 dB"), ". Trzeba obni\u017cy\u0107 \u015bredni\u0105 z 82 do 78.4 dB."
    )),
    sol4 = withMathJax(tagList(
      tags$b("a)"), " \u03bb = 1/365 \u2248 0.00274 (awarii na dzie\u0144)", tags$br(),
      tags$b("b)"), " P(X < 180) = 0.3893 (~38.9%)", tags$br(),
      tags$b("c)"), " P(X > 730) = 0.1353 (~13.5%)", tags$br(),
      tags$b("d)"), " ", tags$b("Nie zmienia"), " \u2014 rozk\u0142ad wyk\u0142adniczy jest ", tags$b("bezpami\u0119ciowy"),
        ". P(X > 200+180 | X > 200) = P(X > 180) = 0.3893."
    )),
    sol5 = tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(tags$th(""), tags$th("Rozk\u0142ad"), tags$th("Parametry"))),
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
    ),
    sol6 = withMathJax(tagList(
      tags$b("6a)"), " Liczba kolizji: Pois(\u03bb = 4). Czas mi\u0119dzy kolizjami: Exp(rate = 4/30) \u2192 \u015brednio co 7.5 dnia.", tags$br(),
      tags$em("Zwi\u0105zek: Poisson zlicza zdarzenia, wyk\u0142adniczy mierzy odst\u0119py."), tags$br(), tags$br(),
      tags$b("6b)"), " B(100, 0.03), E(X) = 3. P(X \u2265 5) = 0.1821. Numer pierwszej wadliwej: Geom(0.03), E = 33.3.", tags$br(), tags$br(),
      tags$b("6c)"), " P(X > 5.0) = 0.1587 (~15.9%). Aby < 5%: \u03bc \u2264 5.0 \u2212 1.645 \u00d7 0.8 = ", tags$b("3.68 mg/m\u00b3"), "."
    )),
    sol7 = withMathJax(tagList(
      p(tags$em("Warto\u015bci empiryczne zale\u017c\u0105 od generatora.")),
      tags$b("b)"), " \u015arednia \u2248 2.1, wariancja \u2248 2.4 \u2014 zbli\u017cone (cecha Poissona)", tags$br(),
      tags$b("c)"), " Poisson z \u03bb \u2248 \u015brednia z danych", tags$br(),
      tags$b("d)"), " Przy \u03bb = 2.1: P(X \u2265 5) \u2248 0.05, P(X = 0) \u2248 0.12"
    )),
    sol8 = withMathJax(tagList(
      tags$b("Stanowisko A:"), " \u015brednia \u2248 82, sd \u2248 3.4 \u2014 symetryczne, bliskie normalnemu", tags$br(),
      tags$b("Stanowisko B:"), " sko\u015bne prawo, \u015brednia > mediana \u2014 sporadyczne szczyty ha\u0142asu", tags$br(),
      tags$b("d)"), " Przy N(82, 3.4): P(X > 85) \u2248 19%", tags$br(),
      tags$b("e)"), " Hipoteza: spawalnia generuje sporadyczne szczyty (uruchomienie spawarki, szlifowanie)."
    )),
    sol9 = withMathJax(tagList(
      tags$b("b)"), " \u015arednia \u2248 1.6 wadliwych na parti\u0119", tags$br(),
      tags$b("c)"), " p = 1.6/30 \u2248 0.053 (~5.3%)", tags$br(),
      tags$b("d)"), " B(30, 0.053): P(X \u2265 3) \u2248 0.20 (~20%)", tags$br(),
      tags$b("e)"), " P(X \u2265 4) \u2248 0.08 (~8%) \u2014 co ~12-13 partia odrzucana"
    )),
    sol10 = withMathJax(tagList(
      tags$b("a)"), " Silnie sko\u015bny prawo", tags$br(),
      tags$b("b)"), " \u015arednia \u2248 sd (cecha rozk\u0142adu wyk\u0142adniczego)", tags$br(),
      tags$b("c)"), " \u03bb = 1/14 \u2248 0.071", tags$br(),
      tags$b("d)"), " P(X < 7) \u2248 0.39 (~39%)", tags$br(),
      tags$b("e)"), " 30/14 \u2248 2.14 incydentu \u2192 Pois(\u03bb = 2.14). P(X \u2265 3) \u2248 0.33."
    )),
    sol_summary = tagList(
      tags$b("1."), " Poisson i wyk\u0142adniczy \u2014 najcz\u0119stsze w BHP. Normalny \u2014 przy pomiarach \u015brodowiskowych.", tags$br(), tags$br(),
      tags$b("2."), " Histogram wyk\u0142adniczy: silnie sko\u015bny prawo. Normalny: symetryczny dzwon.", tags$br(), tags$br(),
      tags$b("3."), " Bezpami\u0119ciowo\u015b\u0107: stary czujnik ma tak\u0105 sam\u0105 szans\u0119 awarii jak nowy. Model sprawdza si\u0119 dla awarii losowych (przepi\u0119cia), nie mechanicznego zu\u017cycia."
    )
  ),

  rol = list(
    sol1 = withMathJax(tagList(
      tags$b("a)"), " B(30, 0.2) \u2014 30 pr\u00f3b Bernoulliego, p = 0.2", tags$br(),
      tags$b("b)"), " P(X \u2265 8) = 0.1107 (~11.1%)", tags$br(),
      tags$b("c)"), " E(X) = 30 \u00d7 0.2 = 6 ro\u015blin", tags$br(),
      tags$b("d)"), " Przy p = 0.4: P(X \u2265 8) = 0.6394 (~63.9%) \u2014 drastyczny wzrost ryzyka"
    )),
    sol2 = withMathJax(tagList(
      tags$b("a)"), " Pois(\u03bb = 2)", tags$br(),
      tags$b("b)"), " P(X = 0) = 0.1353 (~13.5%)", tags$br(),
      tags$b("c)"), " P(X > 4) = 0.0527 (~5.3%)", tags$br(),
      tags$b("d)"), " P(X = 2) = 0.2707 (~27.1%) \u2014 najcz\u0119stszy wynik", tags$br(),
      tags$b("e)"), " \u0141\u0105cznie 3 kwatery: Pois(\u03bb = 6). P(X \u2265 10) = 0.0839 (~8.4%)"
    )),
    sol3 = withMathJax(tagList(
      tags$b("a)"), " P(X < 5) = 0.0668 (~6.7%) \u2014 co 15. pole poni\u017cej normy skupu", tags$br(),
      tags$b("b)"), " P(5.4 < X < 7.0) = 0.6731 (~67.3%)", tags$br(),
      tags$b("c)"), " Kwantyl 5%: 4.88 t/ha", tags$br(),
      tags$b("d)"), " P(X > 7.5) = 0.0668 (~6.7%) \u2014 \u201eprawie zawsze\u201d to grubo przesadzone", tags$br(),
      tags$b("e)"), " 5.0 = \u03bc \u2212 2.326 \u00d7 0.8 \u2192 ", tags$b("\u03bc \u2265 6.86 t/ha")
    )),
    sol4 = withMathJax(tagList(
      tags$b("a)"), " \u03bb = 1/10 = 0.1 (opadu na dzie\u0144)", tags$br(),
      tags$b("b)"), " P(X > 15) = e^(\u221215/10) = 0.2231 (~22.3%)", tags$br(),
      tags$b("c)"), " P(X \u2264 5) = 1 \u2212 e^(\u22125/10) = 0.3935 (~39.4%)", tags$br(),
      tags$b("d)"), " ", tags$b("Nie zmienia"), " \u2014 P(X > 12+10 | X > 12) = P(X > 10) = 0.3679."
    )),
    sol5 = tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(tags$th(""), tags$th("Rozk\u0142ad"), tags$th("Parametry"))),
      tags$tbody(
        tags$tr(tags$td("a)"), tags$td("Dwumianowy"), tags$td("B(15, 0.25)")),
        tags$tr(tags$td("b)"), tags$td("Poissona"), tags$td("Pois(4)")),
        tags$tr(tags$td("c)"), tags$td("Normalny"), tags$td("N(42, 3)")),
        tags$tr(tags$td("d)"), tags$td("Jednostajny ci\u0105g\u0142y"), tags$td("U(1, 14)")),
        tags$tr(tags$td("e)"), tags$td("Dwumianowy"), tags$td("B(50, 0.03)")),
        tags$tr(tags$td("f)"), tags$td("Geometryczny"), tags$td("Geom(0.10)")),
        tags$tr(tags$td("g)"), tags$td("Wyk\u0142adniczy"), tags$td("Exp(\u03bb = 1/7)")),
        tags$tr(tags$td("h)"), tags$td("Normalny"), tags$td("N(35, 5)"))
      )
    ),
    sol6 = withMathJax(tagList(
      tags$b("6a)"), " Ogniska chwast\u00f3w: Pois(\u03bb = 5). Czas mi\u0119dzy ogniskami: Exp(rate = 5/7) \u2192 \u015brednio co 1.4 dnia.", tags$br(),
      tags$em("Zwi\u0105zek: Poisson zlicza zdarzenia, wyk\u0142adniczy mierzy odst\u0119py."), tags$br(), tags$br(),
      tags$b("6b)"), " B(100, 0.04), E(X) = 4. P(X \u2265 5 | n=100) = 0.3711. Geom(0.04), E = 25.", tags$br(), tags$br(),
      tags$b("6c)"), " P(X \u2265 1.0) = 0.9772 (~97.7%). Aby \u2265 99%: \u03bc \u2265 1.0 + 2.326 \u00d7 0.4 = ", tags$b("1.93% s.m.")
    )),
    sol7 = withMathJax(tagList(
      p(tags$em("Warto\u015bci empiryczne zale\u017c\u0105 od generatora.")),
      tags$b("b)"), " \u015arednia \u2248 2.8, wariancja \u2248 2.9 \u2014 zbli\u017cone (cecha Poissona)", tags$br(),
      tags$b("c)"), " Poisson z \u03bb \u2248 \u015brednia z danych", tags$br(),
      tags$b("d)"), " Przy \u03bb = 2.8: P(X \u2265 5) \u2248 0.11, P(X = 0) \u2248 0.06"
    )),
    sol8 = withMathJax(tagList(
      tags$b("Odmiana A:"), " \u015brednia \u2248 6.2, sd \u2248 0.8 \u2014 symetryczna, bliskie normalnemu", tags$br(),
      tags$b("Odmiana B:"), " sko\u015bna, \u015brednia > mediana \u2014 wra\u017cliwa na susz\u0119", tags$br(),
      tags$b("d)"), " Przy N(6.2, 0.8): P(X < 5.0) \u2248 6.7%", tags$br(),
      tags$b("e)"), " Hipoteza: odmiana B przy suszy daje dramatycznie ni\u017csze plony \u2192 sko\u015bno\u015b\u0107 lewa."
    )),
    sol9 = withMathJax(tagList(
      tags$b("b)"), " \u015arednia \u2248 1.5 wadliwych na parti\u0119", tags$br(),
      tags$b("c)"), " p = 1.5/50 = 0.03 (~3%)", tags$br(),
      tags$b("d)"), " B(50, 0.03): P(X \u2265 4) \u2248 0.07 (~7%)", tags$br(),
      tags$b("e)"), " P(X \u2265 5) \u2248 0.03 (~3%) \u2014 co ~33. partia odrzucana"
    )),
    sol10 = withMathJax(tagList(
      tags$b("a)"), " Silnie sko\u015bny prawo", tags$br(),
      tags$b("b)"), " \u015arednia \u2248 sd (cecha wyk\u0142adniczego)", tags$br(),
      tags$b("c)"), " \u03bb = 1/\u015brednia", tags$br(),
      tags$b("d)"), " P(X > 14) = e^(\u221214\u03bb) \u2014 podstaw \u03bb z danych", tags$br(),
      tags$b("e)"), " 90/\u015brednia opad\u00f3w \u2192 Pois(\u03bb = 90/\u015brednia). Oblicz P(X \u2265 15)."
    )),
    sol_summary = tagList(
      tags$b("1."), " Poisson (szkodniki, po\u0142amania), normalny (plony, sk\u0142ad gleby), wyk\u0142adniczy (czasy mi\u0119dzy zdarzeniami).", tags$br(), tags$br(),
      tags$b("2."), " \u015arednia \u2248 wariancja \u2014 charakterystyczna cecha Poissona.", tags$br(), tags$br(),
      tags$b("3."), " Bezpami\u0119ciowo\u015b\u0107: nieistotne, ile dni trwa susza \u2014 szansa deszczu w ci\u0105gu nast\u0119pnych 10 dni zawsze taka sama."
    )
  ),

  zyw = list(
    sol1 = withMathJax(tagList(
      tags$b("a)"), " B(40, 0.08)", tags$br(),
      tags$b("b)"), " P(X \u2265 5) = 0.2894 (~28.9%)", tags$br(),
      tags$b("c)"), " E(X) = 40 \u00d7 0.08 = 3.2 s\u0142oika", tags$br(),
      tags$b("d)"), " Przy p = 0.03: P(X \u2265 5) = 0.0214 (~2.1%) \u2014 naprawa maszyny drastycznie redukuje ryzyko"
    )),
    sol2 = withMathJax(tagList(
      tags$b("a)"), " Pois(\u03bb = 4)", tags$br(),
      tags$b("b)"), " P(X = 0) = 0.0183 (~1.8%)", tags$br(),
      tags$b("c)"), " P(X > 6) = 0.1107 (~11.1%)", tags$br(),
      tags$b("d)"), " P(X = 4) = 0.1954 (~19.5%) \u2014 najcz\u0119stszy wynik", tags$br(),
      tags$b("e)"), " W miesi\u0105cu: Pois(\u03bb = 16). P(X \u2265 20) = 0.2050 (~20.5%)"
    )),
    sol3 = withMathJax(tagList(
      tags$b("a)"), " P(X > 2.5) = 0.0912 (~9.1%) \u2014 co 11. partia przekracza norm\u0119", tags$br(),
      tags$b("b)"), " P(1.8 < X < 2.4) = 0.8186 (~81.9%)", tags$br(),
      tags$b("c)"), " Kwantyl 95%: 2.59 g/100g", tags$br(),
      tags$b("d)"), " P(X > 2.8) = 0.0013 (~0.13%) \u2014 tu producent ma racj\u0119, ale 2.5 g to problem", tags$br(),
      tags$b("e)"), " 2.5 = \u03bc + 1.645 \u00d7 0.3 \u2192 ", tags$b("\u03bc \u2264 2.01 g/100g")
    )),
    sol4 = withMathJax(tagList(
      tags$b("a)"), " \u03bb = 1/8 = 0.125 (zepsu\u0107 na dzie\u0144)", tags$br(),
      tags$b("b)"), " P(X \u2264 3) = 1 \u2212 e^(\u22123/8) = 0.3127 (~31.3%)", tags$br(),
      tags$b("c)"), " P(X > 10) = e^(\u221210/8) = 0.2865 (~28.7%)", tags$br(),
      tags$b("d)"), " ", tags$b("Nie zmienia"), " \u2014 P(X > 5+3 | X > 5) = P(X > 3) = 0.6873."
    )),
    sol5 = tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(tags$th(""), tags$th("Rozk\u0142ad"), tags$th("Parametry"))),
      tags$tbody(
        tags$tr(tags$td("a)"), tags$td("Dwumianowy"), tags$td("B(25, 0.05)")),
        tags$tr(tags$td("b)"), tags$td("Poissona"), tags$td("Pois(3)")),
        tags$tr(tags$td("c)"), tags$td("Normalny"), tags$td("N(1000, 5)")),
        tags$tr(tags$td("d)"), tags$td("Jednostajny ci\u0105g\u0142y"), tags$td("U(0, 60)")),
        tags$tr(tags$td("e)"), tags$td("Dwumianowy"), tags$td("B(30, 0.02)")),
        tags$tr(tags$td("f)"), tags$td("Geometryczny"), tags$td("Geom(0.08)")),
        tags$tr(tags$td("g)"), tags$td("Wyk\u0142adniczy"), tags$td("Exp(\u03bb = 1/4)")),
        tags$tr(tags$td("h)"), tags$td("Normalny"), tags$td("N(26, 1.5)"))
      )
    ),
    sol6 = withMathJax(tagList(
      tags$b("6a)"), " Usterki: Pois(\u03bb = 6). Czas mi\u0119dzy usterkami: Exp(rate = 6/8) \u2192 \u015brednio co 1.33 h.", tags$br(),
      tags$em("Zwi\u0105zek: Poisson zlicza zdarzenia, wyk\u0142adniczy mierzy odst\u0119py."), tags$br(), tags$br(),
      tags$b("6b)"), " B(200, 0.02), E(X) = 4. P(X \u2265 5) = 0.3712. Geom(0.02), E = 50.", tags$br(), tags$br(),
      tags$b("6c)"), " P(X \u2265 3.2) = 0.9772 (~97.7%). Aby < 1% nie spe\u0142nia\u0142o: \u03bc \u2265 3.2 + 2.326 \u00d7 0.2 = ", tags$b("3.67%")
    )),
    sol7 = withMathJax(tagList(
      p(tags$em("Warto\u015bci empiryczne zale\u017c\u0105 od generatora.")),
      tags$b("b)"), " \u015arednia \u2248 4.1, wariancja \u2248 4.3 \u2014 zbli\u017cone (cecha Poissona)", tags$br(),
      tags$b("c)"), " Poisson z \u03bb \u2248 \u015brednia z danych", tags$br(),
      tags$b("d)"), " Przy \u03bb = 4.1: P(X \u2265 7) \u2248 0.15, P(X = 0) \u2248 0.017"
    )),
    sol8 = withMathJax(tagList(
      tags$b("Linia A:"), " \u015brednia \u2248 1000, sd \u2248 5 \u2014 symetryczna, bliskie normalnemu", tags$br(),
      tags$b("Linia B:"), " sko\u015bna prawo, sd wi\u0119ksze", tags$br(),
      tags$b("d)"), " Przy N(1000, 5): P(X < 995) \u2248 15.9%", tags$br(),
      tags$b("e)"), " Hipoteza: stara g\u0142owica nape\u0142niaj\u0105ca sporadycznie dozuje za du\u017co \u2192 asymetria prawo."
    )),
    sol9 = withMathJax(tagList(
      tags$b("b)"), " \u015arednia \u2248 1.8 wadliwych na parti\u0119", tags$br(),
      tags$b("c)"), " p = 1.8/40 = 0.045 (~4.5%)", tags$br(),
      tags$b("d)"), " B(40, 0.045): P(X \u2265 4) \u2248 0.14 (~14%)", tags$br(),
      tags$b("e)"), " P(X \u2265 5) \u2248 0.05 (~5%) \u2014 co 20. partia odrzucana"
    )),
    sol10 = withMathJax(tagList(
      tags$b("a)"), " Silnie sko\u015bny prawo", tags$br(),
      tags$b("b)"), " \u015arednia \u2248 sd (cecha wyk\u0142adniczego)", tags$br(),
      tags$b("c)"), " \u03bb = 1/\u015brednia (awarii na godzin\u0119)", tags$br(),
      tags$b("d)"), " P(X \u2264 2) = 1 \u2212 e^(\u22122\u03bb) \u2014 podstaw \u03bb z danych", tags$br(),
      tags$b("e)"), " 40 \u00d7 \u03bb awarii \u2192 Pois(\u03bb\u2019 = 40/\u015brednia). Oblicz P(X \u2265 5)."
    )),
    sol_summary = tagList(
      tags$b("1."), " Dwumianowy (kontrola jako\u015bci parti\u0105), Poisson (reklamacje, awarie), normalny (sk\u0142ad, masa).", tags$br(), tags$br(),
      tags$b("2."), " \u015arednia \u2248 wariancja reklamacji \u2014 cecha Poissona: zdarzenia niezale\u017cne i losowe.", tags$br(), tags$br(),
      tags$b("3."), " Bezpami\u0119ciowo\u015b\u0107: jogurt, kt\u00f3ry przetrwa\u0142 5 dni, ma tak\u0105 sam\u0105 szans\u0119 zepsucia jak \u015bwie\u017co otwarty. Uproszczenie \u2014 rzeczywista degradacja jest monotonalna."
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch9_server <- function(input, output, session) {

  sol_ids <- c("sol1", "sol2", "sol3", "sol4", "sol5", "sol6",
               "sol7", "sol8", "sol9", "sol10", "sol_summary")
  btn_ids <- c("ans1", "ans2", "ans3", "ans4", "ans5", "ans6",
               "ans7", "ans8", "ans9", "ans10", "ans_summary")

  # Stan widocznosci (TRUE = otwarte)
  vis <- lapply(sol_ids, function(x) reactiveVal(FALSE))
  names(vis) <- sol_ids

  # Render tresci po zmianie kierunku + reset stanow
  observeEvent(input$ch9_kierunek, {
    k <- input$ch9_kierunek

    # Reset wszystkich stanow
    for (sid in sol_ids) vis[[sid]](FALSE)
    for (bid in btn_ids) {
      updateActionButton(session, paste0("ch9_", bid), label = "Poka\u017c rozwi\u0105zanie")
    }

    # Render tresci
    output$ch9_content <- renderUI({
      switch(k,
        bhp = .ch9_content_bhp(),
        rol = .ch9_content_rol(),
        zyw = .ch9_content_zyw()
      )
    })
  }, ignoreNULL = FALSE)

  # Helper toggle dla kazdego zadania
  .make_toggle <- function(sol_id, btn_id) {
    observeEvent(input[[btn_id]], {
      nowy_stan <- !vis[[sol_id]]()
      vis[[sol_id]](nowy_stan)
      updateActionButton(session, btn_id,
        label = if (nowy_stan) "Ukryj rozwi\u0105zanie" else "Poka\u017c rozwi\u0105zanie")
    }, ignoreInit = TRUE)

    output[[sol_id]] <- renderUI({
      if (!vis[[sol_id]]()) return(NULL)
      k <- isolate(input$ch9_kierunek)
      sol <- .ch9_solutions[[k]][[sol_id]]
      div(class = "callout-success", style = "margin-top: 10px;", sol)
    })
  }

  mapply(.make_toggle,
    sol_id = paste0("ch9_", sol_ids),
    btn_id = paste0("ch9_", btn_ids)
  )
}
