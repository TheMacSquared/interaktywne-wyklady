# ============================================================================
# CHAPTER 9: Cwiczenia - Rozklady prawdopodobienstwa
# Cztery warianty kierunkowe, w tym dane satelitarne i kosmiczne
# ============================================================================

# ============================================================================
# UI
# ============================================================================

ch9_ui <- list(
  id = "ch-cwiczenia", num = "09", title = "Ćwiczenia",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 09 · Rozkłady prawdopodobieństwa",
      num    = "09",
      title  = "Ćwiczenia.",
      lead   = "Trzy bloki zadań — od obliczeń przez rozpoznawanie rozkładów
                po analizę prawdziwych danych. Narzędzie: Jamovi (90 min)."
    ),

    lc_h2("ch9-cwiczenia", "Ćwiczenia: Rozkłady prawdopodobieństwa"),

    tagList(
      p(tags$b("Czas:"), " 90 minut | ", tags$b("Narzędzie:"), " Jamovi"),
      p("Każde zadanie ma ", tags$b("ukryte rozwiązanie"),
        " — kliknij przycisk, aby je zobaczyć.")
    ),

    figure_panel(label = "Ćwiczenie",
      selectInput("ch9_kierunek", tags$b("Wybierz wariant dla kierunku:"),
        choices = list(
          "Inżynieria Bezpieczeństwa (BHP)" = "bhp",
          "Rolnictwo"                       = "rol",
          "Technologia żywności"            = "zyw",
          "Inżynieria danych satelitarnych i kosmicznych" = "sat"
        ),
        selected = "bhp",
        width = "100%"
      )
    ),

    uiOutput("ch9_content")
  )
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

  lc_h3("Blok 1: Kalkulator rozkładów (25 min)"),
  lc_feedback(type = "info",
    p("W Jamovi: ", tags$b("Analyses → Exploration → Distribution"),
      " (lub moduł ", tags$code("distrACTION"), ")."),
    p("Dla każdego zadania: wybierz odpowiedni rozkład, ustaw parametry, odczytaj prawdopodobieństwo.")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 1 — Szkolenie BHP"),
    tagList(
      p("Test BHP składa się z 25 pytań prawda/fałsz. Zaliczenie wymaga ",
        tags$b("minimum 20 poprawnych"), " odpowiedzi. Pracownik nie uczył się i odpowiada losowo."),
      tags$ol(
        tags$li("Jaki rozkład opisuje liczbę poprawnych odpowiedzi? Podaj parametry."),
        tags$li("Jakie jest prawdopodobieństwo, że pracownik zaliczy test?"),
        tags$li("Jaka jest oczekiwana liczba poprawnych odpowiedzi?"),
        tags$li("Gdyby próg zaliczenia obniżono do 15 — jak zmieniłoby się prawdopodobieństwo?")
      )
    ),
    actionButton("ch9_ans1", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol1")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 2 — Wypadki przy pracy"),
    tagList(
      p("W zakładzie produkcyjnym dochodzi średnio do ",
        tags$b("2.5 wypadku przy pracy miesięcznie"), "."),
      tags$ol(
        tags$li("Jaki rozkład opisuje liczbę wypadków w miesiącu? Podaj parametr."),
        tags$li("Jakie jest prawdopodobieństwo ", tags$b("dokładnie 5"), " wypadków w miesiącu?"),
        tags$li("Jakie jest prawdopodobieństwo ", tags$b("żadnego"), " wypadku w miesiącu?"),
        tags$li("Jakie jest prawdopodobieństwo ", tags$b("więcej niż 4"), " wypadków?"),
        tags$li(tags$em("Trudniejsze:"), " Jeśli rozpatrujemy cały kwartał (3 miesiące) — jaki rozkład opisuje liczbę wypadków i jaki ma parametr? Oblicz P(≥10 wypadków w kwartale).")
      )
    ),
    actionButton("ch9_ans2", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol2")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 3 — Poziom hałasu"),
    tagList(
      p("Pomiary hałasu na stanowisku w hali montażowej mają rozkład ",
        tags$b("N(82, 4)"), " dB (średnia 82 dB, odchylenie standardowe 4 dB). Norma BHP: ", tags$b("85 dB"), "."),
      tags$ol(
        tags$li("Jaki procent pomiarów przekracza normę 85 dB?"),
        tags$li("Jaki procent pomiarów mieści się w przedziale 78–86 dB?"),
        tags$li("Poniżej jakiej wartości znajduje się 95% pomiarów?"),
        tags$li("Pracodawca twierdzi, że „prawie nigdy” nie przekracza 90 dB. Zweryfikuj — jaki procent pomiarów > 90 dB?"),
        tags$li(tags$em("Trudniejsze:"), " Pracodawca planuje wymienić wentylację. Przy jakim poziomie średniej (przy tym samym σ = 4 dB) dokładnie 5% pomiarów przekraczałoby normę 85 dB?")
      )
    ),
    actionButton("ch9_ans3", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol3")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 4 — Niezawodność czujnika dymu"),
    tagList(
      p("Czujnik dymu ma średni czas bezawaryjnej pracy (", tags$b("MTBF"), ") wynoszący ",
        tags$b("365 dni"), ". Czas do awarii ma rozkład wykładniczy."),
      tags$ol(
        tags$li("Jaki jest parametr λ (rate) tego rozkładu?"),
        tags$li("Jakie jest prawdopodobieństwo awarii w ciągu pierwszych 180 dni?"),
        tags$li("Jakie jest prawdopodobieństwo, że czujnik przetrwa dłużej niż 2 lata (730 dni)?"),
        tags$li(tags$em("Trudniejsze:"), " Czujnik pracuje już 200 dni bez awarii. Czy to zmienia prawdopodobieństwo awarii w następnych 180 dniach? Uzasadnij (bezpamięciowość).")
      )
    ),
    actionButton("ch9_ans4", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol4")
  ),

  lc_h3("Blok 2: Rozpoznawanie rozkładów (25 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 5 — Który to rozkład?"),
    tagList(
      p("Dla każdej sytuacji: ", tags$b("nazwij rozkład"), " i ", tags$b("podaj parametry"),
        ". Pracujcie w parach, potem dyskusja.")
    ),
    tags$table(class = "lc-table lc-table-striped lc-table-bordered",
      tags$thead(tags$tr(tags$th(""), tags$th("Sytuacja"), tags$th("Rozkład"), tags$th("Parametry"))),
      tags$tbody(
        .z5row("a)", "Inspektor BHP sprawdza 20 stanowisk. Każde ma 10% szans na naruszenie przepisów. Ile naruszeń znajdzie?"),
        .z5row("b)", "Średnio 3 alarmy przeciwpożarowe na tydzień w galerii handlowej. Ile alarmów w następnym tygodniu?"),
        .z5row("c)", "Czas oczekiwania na karetkę pogotowia — średnia 8 min, odch. std. 2 min, rozkład symetryczny"),
        .z5row("d)", "Awaria w fabryce może wystąpić w losowym momencie 8-godzinnej zmiany (każdy moment tak samo prawdopodobny)"),
        .z5row("e)", "Z 50 gaśnic w magazynie, 4% jest przeterminowanych. Ile przeterminowanych w losowej kontroli?"),
        .z5row("f)", "Inspektor sprawdza kolejne budynki aż do znalezienia pierwszego z naruszeniem przepisów ppoż. (szansa: 15%). Ile budynków sprawdzi?"),
        .z5row("g)", "Średnio 1 poważny wypadek co 20 dni roboczych. Ile dni do następnego wypadku?"),
        .z5row("h)", "Waga ładunku na palecie — średnia 500 kg, odch. std. 30 kg")
      )
    ),
    actionButton("ch9_ans5", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol5")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 6 — Trudniejsze: powiązania między rozkładami"),
    tagList(
      tags$b("6a)"), " W firmie kurierskiej średnio dochodzi do ", tags$b("4 kolizji drogowych miesięcznie"), ".",
      tags$ul(
        tags$li("Jaki rozkład opisuje ", tags$b("liczbę"), " kolizji w miesiącu?"),
        tags$li("Jaki rozkład opisuje ", tags$b("czas"), " (w dniach) między kolejnymi kolizjami?"),
        tags$li("Podaj parametry obu rozkładów. Jaki jest związek między nimi?")
      ),
      tags$b("6b)"), " Partia 100 środków ochrony indywidualnej (rękawice). Wadliwość wynosi 3%.",
      tags$ul(
        tags$li("Jaki rozkład opisuje liczbę wadliwych rękawic w partii? Podaj parametry."),
        tags$li("Kontroler jakości sprawdza rękawice po kolei. Jaki rozkład opisuje numer rękawicy, przy której natrafi na pierwszą wadliwą?"),
        tags$li("Oblicz: P(≥5 wadliwych w partii) oraz E(numer pierwszej wadliwej).")
      ),
      tags$b("6c)"), " Stężenie pyłu na stanowisku ma rozkład N(4.2, 0.8) mg/m³. Norma BHP wynosi ", tags$b("5.0 mg/m³"), ".",
      tags$ul(
        tags$li("Jaki procent pomiarów przekracza normę?"),
        tags$li("Pracodawca musi zapewnić, że ", tags$b("mniej niż 5% pomiarów"), " przekracza normę. Do jakiej wartości musiałby obniżyć średnie stężenie (przy tym samym σ)?")
      )
    ),
    actionButton("ch9_ans6", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol6")
  ),

  lc_h3("Blok 3: Analiza danych w Jamowi (40 min)"),
  lc_feedback(type = "info",
    p("Otwórz pliki CSV z folderu ", tags$code("cwiczenia/dane/"), " w Jamovi.")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 7 — Wypadki miesięcznie"),
    p(class = "text-muted", tags$code("wypadki_miesiecznie.csv")),
    tagList(
      tags$ol(
        tags$li("Otwórz plik. Zrób ", tags$b("histogram"), " zmiennej ", tags$code("liczba_wypadkow"), " (Exploration → Descriptives → Plots → Histogram)."),
        tags$li("Oblicz ", tags$b("średnią"), " i ", tags$b("wariancję"), ". Czy są zbliżone do siebie?"),
        tags$li("Jaki to sugeruje rozkład? Podaj parametr(y)."),
        tags$li("Używając kalkulatora rozkładów z λ = średnia z danych: Oblicz P(X ≥ 5) i P(X = 0)."),
        tags$li("Porównaj teoretyczne prawdopodobieństwa z empirycznymi częstościami w danych.")
      )
    ),
    actionButton("ch9_ans7", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol7")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 8 — Hałas na stanowiskach"),
    p(class = "text-muted", tags$code("halas_stanowiska.csv")),
    tagList(
      tags$ol(
        tags$li("Rozdziel dane na dwa stanowiska (Data → Filters: ", tags$code("stanowisko == \"A_montaz\""), ")."),
        tags$li("Dla każdego stanowiska zrób ", tags$b("histogram"), " i oblicz ", tags$b("statystyki opisowe"), " (średnia, mediana, odch. std., skosność)."),
        tags$li("Które stanowisko ma rozkład bliższy normalnemu? Po czym to poznajesz?"),
        tags$li("Dla stanowiska o rozkładzie normalnym: jaki % pomiarów przekracza normę 85 dB? (użyj kalkulatora z parametrami z danych)"),
        tags$li(tags$em("Trudniejsze:"), " Dlaczego stanowisko B mogłoby mieć rozkład skośny? Podaj hipotezę techniczną.")
      )
    ),
    actionButton("ch9_ans8", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol8")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 9 — Kontrola kasków"),
    p(class = "text-muted", tags$code("kontrola_kaskow.csv")),
    tagList(
      tags$ol(
        tags$li("Zrób ", tags$b("histogram"), " zmiennej ", tags$code("liczba_wadliwych"), "."),
        tags$li("Oblicz średnią liczbę wadliwych kasków na partię."),
        tags$li("Wiedząc, że partia liczy 30 kasków — oszacuj p = średnia / 30."),
        tags$li("Używając B(30, p): oblicz P(≥ 3 wadliwych w jednej partii)."),
        tags$li("Kierownik jakości chce odrzucać partię, jeśli jest ≥ 4 wadliwych. Jak często partia zostanie odrzucona?")
      )
    ),
    actionButton("ch9_ans9", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol9")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 10 — Czas między incydentami"),
    p(class = "text-muted", tags$code("czas_miedzy_incydentami.csv")),
    tagList(
      tags$ol(
        tags$li("Zrób ", tags$b("histogram"), " zmiennej ", tags$code("dni_od_poprzedniego"), ". Jaki kształt ma rozkład?"),
        tags$li("Oblicz ", tags$b("średnią"), " i ", tags$b("odchylenie standardowe"), ". Czy są do siebie zbliżone?"),
        tags$li("Jaki rozkład pasuje do tych danych? Podaj parametr(y)."),
        tags$li("Używając dopasowanego rozkładu: oblicz P(następny incydent w ciągu 7 dni)."),
        tags$li(tags$em("Trudniejsze:"), " Jeśli średni czas między incydentami wynosi X dni, ile incydentów oczekujemy w ciągu 30 dni? Jaki rozkład to opisuje? Oblicz P(≥ 3 incydenty w miesiącu).")
      )
    ),
    actionButton("ch9_ans10", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol10")
  ),

  lc_h3("Podsumowanie"),
  lc_feedback(type = "warning",
    p(tags$b("Po zakończeniu ćwiczeń odpowiedz na pytania:")),
    tags$ol(
      tags$li("Który rozkład najczęściej widzisz w kontekście BHP i dlaczego?"),
      tags$li("Jak wygląda histogram danych z rozkładu wykładniczego? Czym różni się od normalnego?"),
      tags$li("Jaki jest praktyczny sens „bezpamięciowości” rozkładu wykładniczego dla bezpieczeństwa?")
    )
  ),
  actionButton("ch9_ans_summary", "Pokaż odpowiedzi", class = "lc-btn-ok-outline lc-btn-sm"),
  uiOutput("ch9_sol_summary")
)

# --------------------------------------------------------------------------
# ROLNICTWO
# --------------------------------------------------------------------------

.ch9_content_rol <- function() tagList(

  lc_h3("Blok 1: Kalkulator rozkładów (25 min)"),
  lc_feedback(type = "info",
    p("W Jamovi: ", tags$b("Analyses → Exploration → Distribution"),
      " (lub moduł ", tags$code("distrACTION"), ")."),
    p("Dla każdego zadania: wybierz odpowiedni rozkład, ustaw parametry, odczytaj prawdopodobieństwo.")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 1 — Zaraza ziemniaka"),
    tagList(
      p("Losowo wybrano ", tags$b("30 roślin ziemniaka"), " z dużego pola. Na podstawie wieloletnich obserwacji wiadomo, że ",
        tags$b("20% roślin"), " na tym polu jest zainfekowanych zar azą."),
      tags$ol(
        tags$li("Jaki rozkład opisuje liczbę zainfekowanych roślin w próbce? Podaj parametry."),
        tags$li("Jakie jest prawdopodobieństwo, że w próbce znajdziemy ", tags$b("co najmniej 8"), " chorych roślin?"),
        tags$li("Jaka jest oczekiwana liczba chorych roślin w próbce?"),
        tags$li("Gdyby poziom infekcji wzrósł do 40% — jak zmieni się prawdopodobieństwo z punktu 2?")
      )
    ),
    actionButton("ch9_ans1", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol1")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 2 — Połamania gałęzi po burzy"),
    tagList(
      p("Po sierpniowych burzach na plantacji jabłoni notuje się średnio ",
        tags$b("2 połamania gałęzi na drzewo"), "."),
      tags$ol(
        tags$li("Jaki rozkład opisuje liczbę połamań na drzewo? Podaj parametr."),
        tags$li("Jakie jest prawdopodobieństwo, że losowo wybrane drzewo ma ", tags$b("zero"), " połamań?"),
        tags$li("Jakie jest prawdopodobieństwo ", tags$b("więcej niż 4"), " połamań na jednym drzewie?"),
        tags$li("Jakie jest prawdopodobieństwo ", tags$b("dokładnie 2"), " połamań?"),
        tags$li(tags$em("Trudniejsze:"), " Sad złożony z 3 kwater (każda jak osobne drzewo) — jaki rozkład i jakie P(≥10 połamań łącznie)?")
      )
    ),
    actionButton("ch9_ans2", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol2")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 3 — Plon pszenicy"),
    tagList(
      p("Plony pszenicy w regionie mają rozkład ", tags$b("N(6.2, 0.8)"), " t/ha. Minimalna norma skupu: ", tags$b("5 t/ha"), "."),
      tags$ol(
        tags$li("Jaki procent pól osiąga plon poniżej normy skupu 5 t/ha?"),
        tags$li("Jaki procent pól mieści się w przedziale 5.4–7.0 t/ha?"),
        tags$li("Poniżej jakiego plonu znajduje się najgorsze 5% pól?"),
        tags$li("Pracodawca twierdzi, że „prawie zawsze” plon przekracza 7.5 t/ha. Zweryfikuj — jaki procent pól przekracza 7.5 t/ha?"),
        tags$li(tags$em("Trudniejsze:"), " Przy tym samym σ = 0.8 t/ha — do jakiej średniej należałoby dążyć, żeby tylko 1% pól było poniżej normy 5 t/ha?")
      )
    ),
    actionButton("ch9_ans3", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol3")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 4 — Czas między opadami deszczu"),
    tagList(
      p("W lipcu w regionie rolniczym średni czas między opadami wynosi ", tags$b("10 dni"), ". Rozkład wykładniczy."),
      tags$ol(
        tags$li("Jaki jest parametr λ (rate) tego rozkładu?"),
        tags$li("Jakie jest prawdopodobieństwo suszy trwającej ", tags$b("dłużej niż 15 dni"), "?"),
        tags$li("Jakie jest prawdopodobieństwo, że deszcz spadnie w ciągu ", tags$b("pierwszych 5 dni"), "?"),
        tags$li(tags$em("Trudniejsze:"), " Minęło już 12 dni bez deszczu. Czy to zmienia prawdopodobieństwo suszy w następnych 10 dniach? Uzasadnij (bezpamięciowość).")
      )
    ),
    actionButton("ch9_ans4", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol4")
  ),

  lc_h3("Blok 2: Rozpoznawanie rozkładów (25 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 5 — Który to rozkład?"),
    tagList(
      p("Dla każdej sytuacji: ", tags$b("nazwij rozkład"), " i ", tags$b("podaj parametry"), ". Pracujcie w parach, potem dyskusja.")
    ),
    tags$table(class = "lc-table lc-table-striped lc-table-bordered",
      tags$thead(tags$tr(tags$th(""), tags$th("Sytuacja"), tags$th("Rozkład"), tags$th("Parametry"))),
      tags$tbody(
        .z5row("a)", "Agrotechnik sprawdza 15 próbek gleby. Każda ma 25% szans na zakwaszenie poniżej normy. Ile próbek będzie zakwaszonych?"),
        .z5row("b)", "Średnio 4 ataki mszyc na tydzień na polu rzepaku. Ile ataków w następnym tygodniu?"),
        .z5row("c)", "Masa tysiąca ziaren pszenicy — średnia 42 g, odch. std. 3 g, rozkład symetryczny"),
        .z5row("d)", "Moment wschodzów roślin w ciągu 14-dniowego okienka — każdy dzień tak samo prawdopodobny"),
        .z5row("e)", "Z 200 nasion w worku 3% jest niekielkujących. Ile takich nasion w losowej próbce 50 sztuk?"),
        .z5row("f)", "Agronom sprawdza kolejne działki aż do znalezienia pierwszej z erozją gleby (szansa: 10%). Ile działek sprawdzi?"),
        .z5row("g)", "Średnio 1 wystąpienie szkodników co 7 dni na polu. Ile dni do następnego wystąpienia?"),
        .z5row("h)", "Wilgotność gleby na polu — średnia 35%, odch. std. 5%")
      )
    ),
    actionButton("ch9_ans5", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol5")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 6 — Trudniejsze: powiązania między rozkładami"),
    tagList(
      tags$b("6a)"), " Na dużym polu kukurydzy średnio występuje ", tags$b("5 ognisk chwastów tygodniowo"), ".",
      tags$ul(
        tags$li("Jaki rozkład opisuje ", tags$b("liczbę"), " ognisk w tygodniu?"),
        tags$li("Jaki rozkład opisuje ", tags$b("czas"), " (w dniach) między kolejnymi ogniskami?"),
        tags$li("Podaj parametry obu rozkładów. Jaki jest związek między nimi?")
      ),
      tags$b("6b)"), " Partia 500 sadzonek pomidorów. Wadliwość (uszkodzone korzenie) wynosi 4%.",
      tags$ul(
        tags$li("Jaki rozkład opisuje liczbę wadliwych sadzonek w 100 sztukach? Podaj parametry."),
        tags$li("Ogrodnik sadzi sadzonki po kolei. Jaki rozkład opisuje numer sadzonki, przy której natrafi na pierwszą wadliwą?"),
        tags$li("Oblicz: P(≥5 wadliwych w partii 100 sztuk) oraz E(numer pierwszej wadliwej).")
      ),
      tags$b("6c)"), " Zawartość azotu w glebie ma rozkład N(1.8, 0.4) % s.m. Norma minimalna dla pszenicy: ", tags$b("1.0% s.m."), ".",
      tags$ul(
        tags$li("Jaki procent pól spełnia normę minimalną?"),
        tags$li("Agrotechnik chce, żeby ", tags$b("co najmniej 99% pól"), " spełniało normę. Do jakiej wartości musi wzrosnąć średnia (przy tym samym σ = 0.4)?")
      )
    ),
    actionButton("ch9_ans6", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol6")
  ),

  lc_h3("Blok 3: Analiza danych w Jamovi (40 min)"),
  lc_feedback(type = "info",
    p("Otwórz pliki CSV z folderu ", tags$code("cwiczenia/rolnictwo/dane/"), " w Jamovi.")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 7 — Wystąpienia szkodników"),
    p(class = "text-muted", tags$code("szkodniki_tygodniowo.csv")),
    tagList(
      tags$ol(
        tags$li("Otwórz plik. Zrób ", tags$b("histogram"), " zmiennej ", tags$code("liczba_wystapien"), " (Exploration → Descriptives → Plots → Histogram)."),
        tags$li("Oblicz ", tags$b("średnią"), " i ", tags$b("wariancję"), ". Czy są zbliżone do siebie?"),
        tags$li("Jaki to sugeruje rozkład? Podaj parametr λ."),
        tags$li("Używając kalkulatora z λ = średnia z danych: oblicz P(X ≥ 5) i P(X = 0)."),
        tags$li("Porównaj teoretyczne prawdopodobieństwa z empirycznymi częstościami w danych.")
      )
    ),
    actionButton("ch9_ans7", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol7")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 8 — Plony na dwóch odmianach"),
    p(class = "text-muted", tags$code("plony_odmiany.csv")),
    tagList(
      tags$ol(
        tags$li("Rozdziel dane na dwie odmiany pszenicy."),
        tags$li("Dla każdej odmiany zrób ", tags$b("histogram"), " i oblicz ", tags$b("statystyki opisowe"), " (średnia, mediana, odch. std., kwartyle)."),
        tags$li("Która odmiana ma rozkład bliższy normalnemu? Po czym to poznajesz?"),
        tags$li("Dla odmiany o rozkładzie normalnym: jaki % pól jest poniżej normy skupu 5 t/ha? (użyj kalkulatora z parametrami z danych)"),
        tags$li(tags$em("Trudniejsze:"), " Dlaczego druga odmiana mogłaby mieć rozkład skośny? Podaj hipotezę agronomiczną.")
      )
    ),
    actionButton("ch9_ans8", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol8")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 9 — Kontrola jakości nasion"),
    p(class = "text-muted", tags$code("kontrola_nasion.csv")),
    tagList(
      tags$ol(
        tags$li("Zrób ", tags$b("histogram"), " zmiennej ", tags$code("liczba_wadliwych"), "."),
        tags$li("Oblicz średnią liczbę wadliwych nasion na partię."),
        tags$li("Wiedząc, że partia liczy 50 nasion — oszacuj p = średnia / 50."),
        tags$li("Używając B(50, p): oblicz P(≥ 4 wadliwych w jednej partii)."),
        tags$li("Magazynier odrzuca partię, jeśli jest ≥ 5 wadliwych nasion. Jak często partia zostanie odrzucona?")
      )
    ),
    actionButton("ch9_ans9", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol9")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 10 — Czas między deszczami"),
    p(class = "text-muted", tags$code("czas_miedzy_deszczami.csv")),
    tagList(
      tags$ol(
        tags$li("Zrób ", tags$b("histogram"), " zmiennej ", tags$code("dni_od_poprzedniego"), ". Jaki kształt ma rozkład?"),
        tags$li("Oblicz ", tags$b("średnią"), " i ", tags$b("odchylenie standardowe"), ". Czy są zbliżone do siebie?"),
        tags$li("Jaki rozkład pasuje do tych danych? Podaj parametr λ."),
        tags$li("Oblicz P(następny deszcz za więcej niż 14 dni)."),
        tags$li(tags$em("Trudniejsze:"), " Ile opadów oczekujemy w sezonie 90-dniowym? Jaki rozkład i jaki λ? Oblicz P(≥ 15 opadów w sezonie).")
      )
    ),
    actionButton("ch9_ans10", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol10")
  ),

  lc_h3("Podsumowanie"),
  lc_feedback(type = "warning",
    p(tags$b("Po zakończeniu ćwiczeń odpowiedz na pytania:")),
    tags$ol(
      tags$li("Który rozkład najczęściej pojawia się w kontekście rolniczym i dlaczego?"),
      tags$li("Co mówi nam średnia i wariancja, gdy są do siebie zbliżone?"),
      tags$li("Dlaczego bezpamięciowość rozkładu wykładniczego jest zaskakująca w kontekście suszy?")
    )
  ),
  actionButton("ch9_ans_summary", "Pokaż odpowiedzi", class = "lc-btn-ok-outline lc-btn-sm"),
  uiOutput("ch9_sol_summary")
)

# --------------------------------------------------------------------------
# TECHNOLOGIA ZYWNOSCI
# --------------------------------------------------------------------------

.ch9_content_zyw <- function() tagList(

  lc_h3("Blok 1: Kalkulator rozkładów (25 min)"),
  lc_feedback(type = "info",
    p("W Jamovi: ", tags$b("Analyses → Exploration → Distribution"),
      " (lub moduł ", tags$code("distrACTION"), ")."),
    p("Dla każdego zadania: wybierz odpowiedni rozkład, ustaw parametry, odczytaj prawdopodobieństwo.")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 1 — Kontrola szczelności opakowań"),
    tagList(
      p("W linii produkcyjnej dżemów losowo pobierana jest próbka ", tags$b("40 słoików"), ". Wiadomo, że średnio ",
        tags$b("8% słoików"), " ma nieszczelną zakrętkę."),
      tags$ol(
        tags$li("Jaki rozkład opisuje liczbę nieszczelnych słoików w próbce? Podaj parametry."),
        tags$li("Jakie jest prawdopodobieństwo, że w próbce znajdziemy ", tags$b("co najmniej 5"), " nieszczelnych słoików?"),
        tags$li("Jaka jest oczekiwana liczba wadliwych opakowań w próbce?"),
        tags$li("Gdyby wadliwość spadła do 3% po naprawie maszyny — jak zmieni się prawdopodobieństwo z punktu 2?")
      )
    ),
    actionButton("ch9_ans1", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol1")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 2 — Reklamacje konsumentów"),
    tagList(
      p("Producent napojów energetycznych otrzymuje średnio ", tags$b("4 reklamacje tygodniowo"), " dotyczące jakości smaku."),
      tags$ol(
        tags$li("Jaki rozkład opisuje liczbę reklamacji w tygodniu? Podaj parametr."),
        tags$li("Jakie jest prawdopodobieństwo ", tags$b("dokładnie 0"), " reklamacji w tygodniu?"),
        tags$li("Jakie jest prawdopodobieństwo ", tags$b("więcej niż 6"), " reklamacji?"),
        tags$li("Jakie jest prawdopodobieństwo ", tags$b("dokładnie 4"), " reklamacji?"),
        tags$li(tags$em("Trudniejsze:"), " Jeśli badamy cały miesiąc (4 tygodnie) — jaki rozkład i jakie P(≥20 reklamacji)?")
      )
    ),
    actionButton("ch9_ans2", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol2")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 3 — Zawartość soli w wędlinie"),
    tagList(
      p("Pomiary zawartości soli w partiach szynki mają rozkład ", tags$b("N(2.1, 0.3)"), " g/100g. Norma maksymalna: ", tags$b("2.5 g/100g"), "."),
      tags$ol(
        tags$li("Jaki procent partii przekracza normę maksymalną 2.5 g/100g?"),
        tags$li("Jaki procent partii mieści się w przedziale 1.8–2.4 g/100g?"),
        tags$li("Poniżej jakiej wartości znajduje się 95% partii?"),
        tags$li("Producent twierdzi, że „prawie nigdy” nie przekracza 2.8 g/100g. Zweryfikuj — jaki procent partii > 2.8?"),
        tags$li(tags$em("Trudniejsze:"), " Inspektor sanitarny wymaga, żeby ", tags$b("mniej niż 5% partii"), " przekraczało normę 2.5 g/100g. Do jakiej wartości musiałaby spaść średnia (przy tym samym σ = 0.3)?")
      )
    ),
    actionButton("ch9_ans3", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol3")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 4 — Trwałość jogurtu po otwarciu"),
    tagList(
      p("Czas do zepsucia jogurtu po otwarciu (w warunkach lodowki) ma rozkład wykładniczy ze średnią ", tags$b("8 dni"), "."),
      tags$ol(
        tags$li("Jaki jest parametr λ (rate) tego rozkładu?"),
        tags$li("Jakie jest prawdopodobieństwo, że jogurt zepsuje się ", tags$b("w ciągu pierwszych 3 dni"), "?"),
        tags$li("Jakie jest prawdopodobieństwo, że jogurt ", tags$b("przetrwa dłużej niż 10 dni"), " po otwarciu?"),
        tags$li(tags$em("Trudniejsze:"), " Konsument otworzył jogurt 5 dni temu i nadal nie jest zepsuty. Czy zmienia to prawdopodobieństwo zepsucia w ciągu kolejnych 3 dni? Uzasadnij (bezpamięciowość).")
      )
    ),
    actionButton("ch9_ans4", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol4")
  ),

  lc_h3("Blok 2: Rozpoznawanie rozkładów (25 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 5 — Który to rozkład?"),
    tagList(
      p("Dla każdej sytuacji: ", tags$b("nazwij rozkład"), " i ", tags$b("podaj parametry"), ". Pracujcie w parach, potem dyskusja.")
    ),
    tags$table(class = "lc-table lc-table-striped lc-table-bordered",
      tags$thead(tags$tr(tags$th(""), tags$th("Sytuacja"), tags$th("Rozkład"), tags$th("Parametry"))),
      tags$tbody(
        .z5row("a)", "Kontroler sprawdza 25 butelek soku. Każda ma 5% szans na błąd napełnienia. Ile błędnych?"),
        .z5row("b)", "Średnio 3 awarie linii produkcyjnej dziennie w zakładzie. Ile awarii jutro?"),
        .z5row("c)", "Masa netto opakowania cukru — średnia 1000 g, odch. std. 5 g, rozkład symetryczny"),
        .z5row("d)", "Moment pobrania próbki z taśmy produkcyjnej w ciągu 60-minutowej zmiany (każda minuta tak samo prawdopodobna)"),
        .z5row("e)", "Z partii 200 tabliczek czekolady, 2% ma wady powlekania. Ile wadliwych w losowej próbce 30 sztuk?"),
        .z5row("f)", "Inspektor sprawdza kolejne partie jogurtów aż do znalezienia pierwszej przeterminowanej (szansa: 8%). Ile partii sprawdzi?"),
        .z5row("g)", "Średnio 1 usterka linii pakującej co 4 godziny. Ile godzin do następnej usterki?"),
        .z5row("h)", "Zawartość białka w proszku mlecznym — średnia 26%, odch. std. 1.5%")
      )
    ),
    actionButton("ch9_ans5", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol5")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 6 — Trudniejsze: powiązania między rozkładami"),
    tagList(
      tags$b("6a)"), " Na linii produkcyjnej serka topionego średnio występuje ", tags$b("6 usterek opakowania dziennie"), ".",
      tags$ul(
        tags$li("Jaki rozkład opisuje ", tags$b("liczbę"), " usterek dziennie?"),
        tags$li("Jaki rozkład opisuje ", tags$b("czas"), " (w godzinach) między kolejnymi usterkami?"),
        tags$li("Podaj parametry obu rozkładów. Jaki jest związek między nimi?")
      ),
      tags$b("6b)"), " Partia 300 puszek konserw. Wadliwość (złe zamknięcie) wynosi 2%.",
      tags$ul(
        tags$li("Jaki rozkład opisuje liczbę wadliwych puszek w 200 sztukach? Podaj parametry."),
        tags$li("Kontroler sprawdza puszki po kolei. Jaki rozkład opisuje numer puszki, przy której natrafi na pierwszą wadliwą?"),
        tags$li("Oblicz: P(≥5 wadliwych w partii 200 sztuk) oraz E(numer pierwszej wadliwej).")
      ),
      tags$b("6c)"), " Zawartość tłuszczu w mleku pełnym ma rozkład N(3.6, 0.2) %. Norma minimalna: ", tags$b("co najmniej 3.2%"), ".",
      tags$ul(
        tags$li("Jaki procent partii spełnia normę minimalną?"),
        tags$li("Producent chce zapewnić, że ", tags$b("mniej niż 1% partii"), " nie spełnia normy. Jaka minimalna średnia jest potrzebna (przy tym samym σ = 0.2)?")
      )
    ),
    actionButton("ch9_ans6", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol6")
  ),

  lc_h3("Blok 3: Analiza danych w Jamovi (40 min)"),
  lc_feedback(type = "info",
    p("Otwórz pliki CSV z folderu ", tags$code("cwiczenia/zywnosc/dane/"), " w Jamovi.")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 7 — Reklamacje tygodniowo"),
    p(class = "text-muted", tags$code("reklamacje_tygodniowo.csv")),
    tagList(
      tags$ol(
        tags$li("Otwórz plik. Zrób ", tags$b("histogram"), " zmiennej ", tags$code("liczba_reklamacji"), " (Exploration → Descriptives → Plots → Histogram)."),
        tags$li("Oblicz ", tags$b("średnią"), " i ", tags$b("wariancję"), ". Czy są zbliżone do siebie?"),
        tags$li("Jaki to sugeruje rozkład? Podaj parametr λ."),
        tags$li("Używając kalkulatora z λ = średnia z danych: oblicz P(X ≥ 7) i P(X = 0)."),
        tags$li("Porównaj teoretyczne prawdopodobieństwa z empirycznymi częstościami w danych.")
      )
    ),
    actionButton("ch9_ans7", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol7")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 8 — Masa netto dwóch linii produkcyjnych"),
    p(class = "text-muted", tags$code("masa_netto_linie.csv")),
    tagList(
      tags$ol(
        tags$li("Rozdziel dane na dwie linie produkcyjne."),
        tags$li("Dla każdej linii zrób ", tags$b("histogram"), " i oblicz ", tags$b("statystyki opisowe"), " (średnia, mediana, odch. std., kwartyle)."),
        tags$li("Która linia ma rozkład bliższy normalnemu? Po czym to poznajesz?"),
        tags$li("Dla linii o rozkładzie normalnym: jaki % opakowań jest poniżej deklarowanej masy 995 g? (użyj kalkulatora z parametrami z danych)"),
        tags$li(tags$em("Trudniejsze:"), " Dlaczego druga linia mogłaby mieć rozkład skośny? Podaj hipotezę technologiczną.")
      )
    ),
    actionButton("ch9_ans8", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol8")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 9 — Kontrola wadliwości opakowań"),
    p(class = "text-muted", tags$code("kontrola_opakowania.csv")),
    tagList(
      tags$ol(
        tags$li("Zrób ", tags$b("histogram"), " zmiennej ", tags$code("liczba_wadliwych"), "."),
        tags$li("Oblicz średnią liczbę wadliwych opakowań na partię."),
        tags$li("Wiedząc, że partia liczy 40 opakowań — oszacuj p = średnia / 40."),
        tags$li("Używając B(40, p): oblicz P(≥ 4 wadliwych w jednej partii)."),
        tags$li("Kierownik jakości odrzuca partię, jeśli jest ≥ 5 wadliwych. Jak często partia zostanie odrzucona?")
      )
    ),
    actionButton("ch9_ans9", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol9")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 10 — Czas między awariami linii"),
    p(class = "text-muted", tags$code("czas_miedzy_awariami.csv")),
    tagList(
      tags$ol(
        tags$li("Zrób ", tags$b("histogram"), " zmiennej ", tags$code("godziny_od_poprzedniej"), ". Jaki kształt ma rozkład?"),
        tags$li("Oblicz ", tags$b("średnią"), " i ", tags$b("odchylenie standardowe"), ". Czy są zbliżone do siebie?"),
        tags$li("Jaki rozkład pasuje do tych danych? Podaj parametr λ."),
        tags$li("Oblicz P(następna awaria w ciągu 2 godzin)."),
        tags$li(tags$em("Trudniejsze:"), " Ile awarii oczekujemy w ciągu tygodnia pracy (40 godzin)? Jaki rozkład i jaki λ? Oblicz P(≥ 5 awarii w tygodniu).")
      )
    ),
    actionButton("ch9_ans10", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch9_sol10")
  ),

  lc_h3("Podsumowanie"),
  lc_feedback(type = "warning",
    p(tags$b("Po zakończeniu ćwiczeń odpowiedz na pytania:")),
    tags$ol(
      tags$li("Który rozkład najczęściej pojawia się w kontroli jakości żywności i dlaczego?"),
      tags$li("Co oznacza w praktyce, gdy średnia i wariancja liczby reklamacji są zbliżone?"),
      tags$li("Jaki jest praktyczny sens bezpamięciowości rozkładu wykładniczego dla trwałości produktów?")
    )
  ),
  actionButton("ch9_ans_summary", "Pokaż odpowiedzi", class = "lc-btn-ok-outline lc-btn-sm"),
  uiOutput("ch9_sol_summary")
)

# ============================================================================
# ROZWIAZANIA — listy per kierunek
# ============================================================================

# Helper: formatuje prawdopodobieństwo jako "0.2392 (~23.9%)"
.fmt_p <- function(p) sprintf("%.4f (~%.1f%%)", p, 100 * p)

source(file.path(app_dir, "modules", "ch9_sat.R"), local = TRUE)

.ch9_solutions <- list(

  bhp = list(
    sol1 = withMathJax(tagList(
      tags$b("a)"), " B(25, 0.5) — 25 prób Bernoulliego, p = 0.5 (losowe T/F)", tags$br(),
      tags$b("b)"), " P(X ≥ 20) = ", .fmt_p(1 - pbinom(19, 25, 0.5)), " — praktycznie niemożliwe zdać zgadując", tags$br(),
      tags$b("c)"), " E(X) = np = 25 × 0.5 = 12.5 odpowiedzi poprawnych", tags$br(),
      tags$b("d)"), " P(X ≥ 15) = ", .fmt_p(1 - pbinom(14, 25, 0.5)), " — dużo łatwiej, ale nadal niezbyt prawdopodobne"
    )),
    sol2 = withMathJax(tagList(
      tags$b("a)"), " Pois(λ = 2.5) — zliczamy zdarzenia w ustalonym czasie", tags$br(),
      tags$b("b)"), " P(X = 5) = ", .fmt_p(dpois(5, 2.5)), tags$br(),
      tags$b("c)"), " P(X = 0) = ", .fmt_p(dpois(0, 2.5)), tags$br(),
      tags$b("d)"), " P(X > 4) = ", .fmt_p(1 - ppois(4, 2.5)), tags$br(),
      tags$b("e)"), " W kwartale: Pois(λ = 7.5). P(X ≥ 10) = ", .fmt_p(1 - ppois(9, 7.5))
    )),
    sol3 = withMathJax(tagList(
      tags$b("a)"), " P(X > 85) = ", .fmt_p(1 - pnorm(85, 82, 4)), " — ponad 1/5 pomiarów przekracza normę!", tags$br(),
      tags$b("b)"), " P(78 < X < 86) = ", .fmt_p(pnorm(86, 82, 4) - pnorm(78, 82, 4)), " — reguła μ±σ", tags$br(),
      tags$b("c)"), sprintf(" Kwantyl 95%%: %.2f dB", qnorm(0.95, 82, 4)), tags$br(),
      tags$b("d)"), " P(X > 90) = ", .fmt_p(1 - pnorm(90, 82, 4)), " — prawie nigdy, ale to nie jest 0", tags$br(),
      tags$b("e)"), sprintf(" 85 = μ + 1.645 × 4 → "), tags$b(sprintf("μ ≤ %.1f dB", 85 - qnorm(0.95) * 4)),
        sprintf(". Trzeba obniżyć średnią z 82 do %.1f dB.", 85 - qnorm(0.95) * 4)
    )),
    sol4 = withMathJax(tagList(
      tags$b("a)"), sprintf(" λ = 1/365 ≈ %.5f (awarii na dzień)", 1/365), tags$br(),
      tags$b("b)"), " P(X < 180) = ", .fmt_p(pexp(180, 1/365)), tags$br(),
      tags$b("c)"), " P(X > 730) = ", .fmt_p(1 - pexp(730, 1/365)), tags$br(),
      tags$b("d)"), " ", tags$b("Nie zmienia"), " — rozkład wykładniczy jest ", tags$b("bezpamięciowy"),
        sprintf(". P(X > 200+180 | X > 200) = P(X > 180) = %.4f.", 1 - pexp(180, 1/365))
    )),
    sol5 = tags$table(class = "lc-table lc-table-striped lc-table-bordered",
      tags$thead(tags$tr(tags$th(""), tags$th("Rozkład"), tags$th("Parametry"))),
      tags$tbody(
        tags$tr(tags$td("a)"), tags$td("Dwumianowy"), tags$td("B(20, 0.1)")),
        tags$tr(tags$td("b)"), tags$td("Poissona"), tags$td("Pois(3)")),
        tags$tr(tags$td("c)"), tags$td("Normalny"), tags$td("N(8, 2)")),
        tags$tr(tags$td("d)"), tags$td("Jednostajny ciągły"), tags$td("U(0, 8)")),
        tags$tr(tags$td("e)"), tags$td("Dwumianowy"), tags$td("B(50, 0.04)")),
        tags$tr(tags$td("f)"), tags$td("Geometryczny"), tags$td("Geom(0.15)")),
        tags$tr(tags$td("g)"), tags$td("Wykładniczy"), tags$td("Exp(λ = 1/20)")),
        tags$tr(tags$td("h)"), tags$td("Normalny"), tags$td("N(500, 30)"))
      )
    ),
    sol6 = withMathJax(tagList(
      tags$b("6a)"), " Liczba kolizji: Pois(λ = 4). Czas między kolizjami: Exp(rate = 4/30) → średnio co 7.5 dnia.", tags$br(),
      tags$em("Związek: Poisson zlicza zdarzenia, wykładniczy mierzy odstępy."), tags$br(), tags$br(),
      tags$b("6b)"), sprintf(" B(100, 0.03), E(X) = 3. P(X ≥ 5) = %.4f. Numer pierwszej wadliwej: Geom(0.03), E = %.1f.",
                              1 - pbinom(4, 100, 0.03), 1/0.03), tags$br(), tags$br(),
      tags$b("6c)"), " P(X > 5.0) = ", .fmt_p(1 - pnorm(5.0, 4.2, 0.8)),
        sprintf(". Aby < 5%%: μ ≤ 5.0 − 1.645 × 0.8 = "), tags$b(sprintf("%.2f mg/m³", 5.0 - qnorm(0.95) * 0.8)), "."
    )),
    sol7 = withMathJax(tagList(
      p(tags$em("Wartości empiryczne zależą od generatora.")),
      tags$b("b)"), " Średnia ≈ 2.1, wariancja ≈ 2.4 — zbliżone (cecha Poissona)", tags$br(),
      tags$b("c)"), " Poisson z λ ≈ średnia z danych", tags$br(),
      tags$b("d)"), " Przy λ = 2.1: P(X ≥ 5) ≈ 0.05, P(X = 0) ≈ 0.12"
    )),
    sol8 = withMathJax(tagList(
      tags$b("Stanowisko A:"), " średnia ≈ 82, sd ≈ 3.4 — symetryczne, bliskie normalnemu", tags$br(),
      tags$b("Stanowisko B:"), " skośne prawo, średnia > mediana — sporadyczne szczyty hałasu", tags$br(),
      tags$b("d)"), " Przy N(82, 3.4): P(X > 85) ≈ 19%", tags$br(),
      tags$b("e)"), " Hipoteza: spawalnia generuje sporadyczne szczyty (uruchomienie spawarki, szlifowanie)."
    )),
    sol9 = withMathJax(tagList(
      tags$b("b)"), " Średnia ≈ 1.6 wadliwych na partię", tags$br(),
      tags$b("c)"), " p = 1.6/30 ≈ 0.053 (~5.3%)", tags$br(),
      tags$b("d)"), " B(30, 0.053): P(X ≥ 3) ≈ 0.20 (~20%)", tags$br(),
      tags$b("e)"), " P(X ≥ 4) ≈ 0.08 (~8%) — co ~12-13 partia odrzucana"
    )),
    sol10 = withMathJax(tagList(
      tags$b("a)"), " Silnie skośny prawo", tags$br(),
      tags$b("b)"), " Średnia ≈ sd (cecha rozkładu wykładniczego)", tags$br(),
      tags$b("c)"), " λ = 1/14 ≈ 0.071", tags$br(),
      tags$b("d)"), " P(X < 7) ≈ 0.39 (~39%)", tags$br(),
      tags$b("e)"), " 30/14 ≈ 2.14 incydentu → Pois(λ = 2.14). P(X ≥ 3) ≈ 0.33."
    )),
    sol_summary = tagList(
      tags$b("1."), " Poisson i wykładniczy — najczęstsze w BHP. Normalny — przy pomiarach środowiskowych.", tags$br(), tags$br(),
      tags$b("2."), " Histogram wykładniczy: silnie skośny prawo. Normalny: symetryczny dzwon.", tags$br(), tags$br(),
      tags$b("3."), " Bezpamięciowość: stary czujnik ma taką samą szansę awarii jak nowy. Model sprawdza się dla awarii losowych (przepięcia), nie mechanicznego zużycia."
    )
  ),

  rol = list(
    sol1 = withMathJax(tagList(
      tags$b("a)"), " B(30, 0.2) — 30 prób Bernoulliego, p = 0.2", tags$br(),
      tags$b("b)"), " P(X ≥ 8) = ", .fmt_p(1 - pbinom(7, 30, 0.2)), tags$br(),
      tags$b("c)"), " E(X) = 30 × 0.2 = 6 roślin", tags$br(),
      tags$b("d)"), " Przy p = 0.4: P(X ≥ 8) = ", .fmt_p(1 - pbinom(7, 30, 0.4)), " — drastyczny wzrost ryzyka"
    )),
    sol2 = withMathJax(tagList(
      tags$b("a)"), " Pois(λ = 2)", tags$br(),
      tags$b("b)"), " P(X = 0) = ", .fmt_p(dpois(0, 2)), tags$br(),
      tags$b("c)"), " P(X > 4) = ", .fmt_p(1 - ppois(4, 2)), tags$br(),
      tags$b("d)"), " P(X = 2) = ", .fmt_p(dpois(2, 2)), " — najczęstszy wynik", tags$br(),
      tags$b("e)"), " Łącznie 3 kwatery: Pois(λ = 6). P(X ≥ 10) = ", .fmt_p(1 - ppois(9, 6))
    )),
    sol3 = withMathJax(tagList(
      tags$b("a)"), " P(X < 5) = ", .fmt_p(pnorm(5, 6.2, 0.8)), " — co 15. pole poniżej normy skupu", tags$br(),
      tags$b("b)"), " P(5.4 < X < 7.0) = ", .fmt_p(pnorm(7.0, 6.2, 0.8) - pnorm(5.4, 6.2, 0.8)), tags$br(),
      tags$b("c)"), sprintf(" Kwantyl 5%%: %.2f t/ha", qnorm(0.05, 6.2, 0.8)), tags$br(),
      tags$b("d)"), " P(X > 7.5) = ", .fmt_p(1 - pnorm(7.5, 6.2, 0.8)), " — „prawie zawsze” to grubo przesadzone", tags$br(),
      tags$b("e)"), " 5.0 = μ − 2.326 × 0.8 → ", tags$b(sprintf("μ ≥ %.2f t/ha", 5.0 + qnorm(0.99) * 0.8))
    )),
    sol4 = withMathJax(tagList(
      tags$b("a)"), " λ = 1/10 = 0.1 (opadu na dzień)", tags$br(),
      tags$b("b)"), " P(X > 15) = e^(−15/10) = ", .fmt_p(1 - pexp(15, 0.1)), tags$br(),
      tags$b("c)"), " P(X ≤ 5) = 1 − e^(−5/10) = ", .fmt_p(pexp(5, 0.1)), tags$br(),
      tags$b("d)"), " ", tags$b("Nie zmienia"), sprintf(" — P(X > 12+10 | X > 12) = P(X > 10) = %.4f.", 1 - pexp(10, 0.1))
    )),
    sol5 = tags$table(class = "lc-table lc-table-striped lc-table-bordered",
      tags$thead(tags$tr(tags$th(""), tags$th("Rozkład"), tags$th("Parametry"))),
      tags$tbody(
        tags$tr(tags$td("a)"), tags$td("Dwumianowy"), tags$td("B(15, 0.25)")),
        tags$tr(tags$td("b)"), tags$td("Poissona"), tags$td("Pois(4)")),
        tags$tr(tags$td("c)"), tags$td("Normalny"), tags$td("N(42, 3)")),
        tags$tr(tags$td("d)"), tags$td("Jednostajny ciągły"), tags$td("U(1, 14)")),
        tags$tr(tags$td("e)"), tags$td("Dwumianowy"), tags$td("B(50, 0.03)")),
        tags$tr(tags$td("f)"), tags$td("Geometryczny"), tags$td("Geom(0.10)")),
        tags$tr(tags$td("g)"), tags$td("Wykładniczy"), tags$td("Exp(λ = 1/7)")),
        tags$tr(tags$td("h)"), tags$td("Normalny"), tags$td("N(35, 5)"))
      )
    ),
    sol6 = withMathJax(tagList(
      tags$b("6a)"), " Ogniska chwastów: Pois(λ = 5). Czas między ogniskami: Exp(rate = 5/7) → średnio co 1.4 dnia.", tags$br(),
      tags$em("Związek: Poisson zlicza zdarzenia, wykładniczy mierzy odstępy."), tags$br(), tags$br(),
      tags$b("6b)"), sprintf(" B(100, 0.04), E(X) = 4. P(X ≥ 5 | n=100) = %.4f. Geom(0.04), E = %.1f.",
                              1 - pbinom(4, 100, 0.04), 1/0.04), tags$br(), tags$br(),
      tags$b("6c)"), " P(X ≥ 1.0) = ", .fmt_p(1 - pnorm(1.0, 1.8, 0.4)),
        sprintf(". Aby ≥ 99%%: μ ≥ 1.0 + 2.326 × 0.4 = "), tags$b(sprintf("%.2f%% s.m.", 1.0 + qnorm(0.99) * 0.4))
    )),
    sol7 = withMathJax(tagList(
      p(tags$em("Wartości empiryczne zależą od generatora.")),
      tags$b("b)"), " Średnia ≈ 2.8, wariancja ≈ 2.9 — zbliżone (cecha Poissona)", tags$br(),
      tags$b("c)"), " Poisson z λ ≈ średnia z danych", tags$br(),
      tags$b("d)"), " Przy λ = 2.8: P(X ≥ 5) ≈ 0.11, P(X = 0) ≈ 0.06"
    )),
    sol8 = withMathJax(tagList(
      tags$b("Odmiana A:"), " średnia ≈ 6.2, sd ≈ 0.8 — symetryczna, bliskie normalnemu", tags$br(),
      tags$b("Odmiana B:"), " skośna, średnia > mediana — wrażliwa na suszę", tags$br(),
      tags$b("d)"), " Przy N(6.2, 0.8): P(X < 5.0) ≈ 6.7%", tags$br(),
      tags$b("e)"), " Hipoteza: odmiana B przy suszy daje dramatycznie niższe plony → skośność lewa."
    )),
    sol9 = withMathJax(tagList(
      tags$b("b)"), " Średnia ≈ 1.5 wadliwych na partię", tags$br(),
      tags$b("c)"), " p = 1.5/50 = 0.03 (~3%)", tags$br(),
      tags$b("d)"), " B(50, 0.03): P(X ≥ 4) ≈ 0.07 (~7%)", tags$br(),
      tags$b("e)"), " P(X ≥ 5) ≈ 0.03 (~3%) — co ~33. partia odrzucana"
    )),
    sol10 = withMathJax(tagList(
      tags$b("a)"), " Silnie skośny prawo", tags$br(),
      tags$b("b)"), " Średnia ≈ sd (cecha wykładniczego)", tags$br(),
      tags$b("c)"), " λ = 1/średnia", tags$br(),
      tags$b("d)"), " P(X > 14) = e^(−14λ) — podstaw λ z danych", tags$br(),
      tags$b("e)"), " 90/średnia opadów → Pois(λ = 90/średnia). Oblicz P(X ≥ 15)."
    )),
    sol_summary = tagList(
      tags$b("1."), " Poisson (szkodniki, połamania), normalny (plony, skład gleby), wykładniczy (czasy między zdarzeniami).", tags$br(), tags$br(),
      tags$b("2."), " Średnia ≈ wariancja — charakterystyczna cecha Poissona.", tags$br(), tags$br(),
      tags$b("3."), " Bezpamięciowość: nieistotne, ile dni trwa susza — szansa deszczu w ciągu następnych 10 dni zawsze taka sama."
    )
  ),

  zyw = list(
    sol1 = withMathJax(tagList(
      tags$b("a)"), " B(40, 0.08)", tags$br(),
      tags$b("b)"), " P(X ≥ 5) = ", .fmt_p(1 - pbinom(4, 40, 0.08)), tags$br(),
      tags$b("c)"), " E(X) = 40 × 0.08 = 3.2 słoika", tags$br(),
      tags$b("d)"), " Przy p = 0.03: P(X ≥ 5) = ", .fmt_p(1 - pbinom(4, 40, 0.03)), " — naprawa maszyny drastycznie redukuje ryzyko"
    )),
    sol2 = withMathJax(tagList(
      tags$b("a)"), " Pois(λ = 4)", tags$br(),
      tags$b("b)"), " P(X = 0) = ", .fmt_p(dpois(0, 4)), tags$br(),
      tags$b("c)"), " P(X > 6) = ", .fmt_p(1 - ppois(6, 4)), tags$br(),
      tags$b("d)"), " P(X = 4) = ", .fmt_p(dpois(4, 4)), " — najczęstszy wynik", tags$br(),
      tags$b("e)"), " W miesiącu: Pois(λ = 16). P(X ≥ 20) = ", .fmt_p(1 - ppois(19, 16))
    )),
    sol3 = withMathJax(tagList(
      tags$b("a)"), " P(X > 2.5) = ", .fmt_p(1 - pnorm(2.5, 2.1, 0.3)), " — co 11. partia przekracza normę", tags$br(),
      tags$b("b)"), " P(1.8 < X < 2.4) = ", .fmt_p(pnorm(2.4, 2.1, 0.3) - pnorm(1.8, 2.1, 0.3)), tags$br(),
      tags$b("c)"), sprintf(" Kwantyl 95%%: %.2f g/100g", qnorm(0.95, 2.1, 0.3)), tags$br(),
      tags$b("d)"), " P(X > 2.8) = ", .fmt_p(1 - pnorm(2.8, 2.1, 0.3)), " — tu producent ma rację, ale 2.5 g to problem", tags$br(),
      tags$b("e)"), " 2.5 = μ + 1.645 × 0.3 → ", tags$b(sprintf("μ ≤ %.2f g/100g", 2.5 - qnorm(0.95) * 0.3))
    )),
    sol4 = withMathJax(tagList(
      tags$b("a)"), " λ = 1/8 = 0.125 (zepsuć na dzień)", tags$br(),
      tags$b("b)"), " P(X ≤ 3) = 1 − e^(−3/8) = ", .fmt_p(pexp(3, 1/8)), tags$br(),
      tags$b("c)"), " P(X > 10) = e^(−10/8) = ", .fmt_p(1 - pexp(10, 1/8)), tags$br(),
      tags$b("d)"), " ", tags$b("Nie zmienia"), sprintf(" — P(X > 5+3 | X > 5) = P(X > 3) = %.4f.", 1 - pexp(3, 1/8))
    )),
    sol5 = tags$table(class = "lc-table lc-table-striped lc-table-bordered",
      tags$thead(tags$tr(tags$th(""), tags$th("Rozkład"), tags$th("Parametry"))),
      tags$tbody(
        tags$tr(tags$td("a)"), tags$td("Dwumianowy"), tags$td("B(25, 0.05)")),
        tags$tr(tags$td("b)"), tags$td("Poissona"), tags$td("Pois(3)")),
        tags$tr(tags$td("c)"), tags$td("Normalny"), tags$td("N(1000, 5)")),
        tags$tr(tags$td("d)"), tags$td("Jednostajny ciągły"), tags$td("U(0, 60)")),
        tags$tr(tags$td("e)"), tags$td("Dwumianowy"), tags$td("B(30, 0.02)")),
        tags$tr(tags$td("f)"), tags$td("Geometryczny"), tags$td("Geom(0.08)")),
        tags$tr(tags$td("g)"), tags$td("Wykładniczy"), tags$td("Exp(λ = 1/4)")),
        tags$tr(tags$td("h)"), tags$td("Normalny"), tags$td("N(26, 1.5)"))
      )
    ),
    sol6 = withMathJax(tagList(
      tags$b("6a)"), " Usterki: Pois(λ = 6). Czas między usterkami: Exp(rate = 6/24) → średnio co 4 h.", tags$br(),
      tags$em("Związek: Poisson zlicza zdarzenia, wykładniczy mierzy odstępy."), tags$br(), tags$br(),
      tags$b("6b)"), sprintf(" B(200, 0.02), E(X) = 4. P(X ≥ 5) = %.4f. Geom(0.02), E = %.0f.",
                              1 - pbinom(4, 200, 0.02), 1/0.02), tags$br(), tags$br(),
      tags$b("6c)"), " P(X ≥ 3.2) = ", .fmt_p(1 - pnorm(3.2, 3.6, 0.2)),
        sprintf(". Aby < 1%% nie spełniało: μ ≥ 3.2 + 2.326 × 0.2 = "), tags$b(sprintf("%.2f%%", 3.2 + qnorm(0.99) * 0.2))
    )),
    sol7 = withMathJax(tagList(
      p(tags$em("Wartości empiryczne zależą od generatora.")),
      tags$b("b)"), " Średnia ≈ 4.1, wariancja ≈ 4.3 — zbliżone (cecha Poissona)", tags$br(),
      tags$b("c)"), " Poisson z λ ≈ średnia z danych", tags$br(),
      tags$b("d)"), " Przy λ = 4.1: P(X ≥ 7) ≈ 0.15, P(X = 0) ≈ 0.017"
    )),
    sol8 = withMathJax(tagList(
      tags$b("Linia A:"), " średnia ≈ 1000, sd ≈ 5 — symetryczna, bliskie normalnemu", tags$br(),
      tags$b("Linia B:"), " skośna prawo, sd większe", tags$br(),
      tags$b("d)"), " Przy N(1000, 5): P(X < 995) ≈ 15.9%", tags$br(),
      tags$b("e)"), " Hipoteza: stara głowica napełniająca sporadycznie dozuje za dużo → asymetria prawo."
    )),
    sol9 = withMathJax(tagList(
      tags$b("b)"), " Średnia ≈ 1.8 wadliwych na partię", tags$br(),
      tags$b("c)"), " p = 1.8/40 = 0.045 (~4.5%)", tags$br(),
      tags$b("d)"), " B(40, 0.045): P(X ≥ 4) ≈ 0.14 (~14%)", tags$br(),
      tags$b("e)"), " P(X ≥ 5) ≈ 0.05 (~5%) — co 20. partia odrzucana"
    )),
    sol10 = withMathJax(tagList(
      tags$b("a)"), " Silnie skośny prawo", tags$br(),
      tags$b("b)"), " Średnia ≈ sd (cecha wykładniczego)", tags$br(),
      tags$b("c)"), " λ = 1/średnia (awarii na godzinę)", tags$br(),
      tags$b("d)"), " P(X ≤ 2) = 1 − e^(−2λ) — podstaw λ z danych", tags$br(),
      tags$b("e)"), " 40 × λ awarii → Pois(λ’ = 40/średnia). Oblicz P(X ≥ 5)."
    )),
    sol_summary = tagList(
      tags$b("1."), " Dwumianowy (kontrola jakości partią), Poisson (reklamacje, awarie), normalny (skład, masa).", tags$br(), tags$br(),
      tags$b("2."), " Średnia ≈ wariancja reklamacji — cecha Poissona: zdarzenia niezależne i losowe.", tags$br(), tags$br(),
      tags$b("3."), " Bezpamięciowość: jogurt, który przetrwał 5 dni, ma taką samą szansę zepsucia jak świeżo otwarty. Uproszczenie — rzeczywista degradacja jest monotonalna."
    )
  )
)

.ch9_solutions$sat <- .ch9_sat_solutions

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
      updateActionButton(session, paste0("ch9_", bid), label = "Pokaż rozwiązanie")
    }

    # Render tresci
    output$ch9_content <- renderUI({
      switch(k,
        bhp = .ch9_content_bhp(),
        rol = .ch9_content_rol(),
        zyw = .ch9_content_zyw(),
        sat = .ch9_content_sat()
      )
    })
  }, ignoreNULL = FALSE)

  # Helper toggle dla kazdego zadania
  # sol_id_bare: klucz w vis i .ch9_solutions (np. "sol1")
  # sol_id_full: nazwa output i btn w UI (np. "ch9_sol1")
  # btn_id_full: nazwa input przycisku (np. "ch9_ans1")
  .make_toggle <- function(sol_id_bare, sol_id_full, btn_id_full) {
    observeEvent(input[[btn_id_full]], {
      nowy_stan <- !vis[[sol_id_bare]]()
      vis[[sol_id_bare]](nowy_stan)
      updateActionButton(session, btn_id_full,
        label = if (nowy_stan) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
    }, ignoreInit = TRUE)

    output[[sol_id_full]] <- renderUI({
      if (!vis[[sol_id_bare]]()) return(NULL)
      k <- isolate(input$ch9_kierunek)
      sol <- .ch9_solutions[[k]][[sol_id_bare]]
      lc_feedback(type = "ok", style = "margin-top: 10px;", sol)
    })
  }

  mapply(.make_toggle,
    sol_id_bare = sol_ids,
    sol_id_full = paste0("ch9_", sol_ids),
    btn_id_full = paste0("ch9_", btn_ids)
  )
}
