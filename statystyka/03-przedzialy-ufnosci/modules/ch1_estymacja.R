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

    lc_p("Wykład 02 zakończyliśmy ", gloss("centralne twierdzenie graniczne", "centralnym twierdzeniem granicznym"), ". Pokazało ono,
      że średnia z próby jest ", gloss("zmienna losowa", "zmienną losową"), ": każda nowa próba daje inną średnią,
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
      gdzie lądują kolejne estymaty. W scenie poniżej kolejne grupki po 25 osób
      wychodzą z sali, a ich średnie wzrostu spadają jedna po drugiej na stos.
      Wzrost w populacji ma ", gloss("rozkład normalny"), " ze średnią μ = 170 cm
      i odchyleniem standardowym σ = 10 cm, ale scena zdradza μ dopiero w ostatnim kroku."),

    # PROTOTYP SCENY (2026-10-08): Zmierz grupkę — estymator się waha, μ stoi
    figure_panel(
      label = "Prototyp sceny",
      width_mode = "text",
      scene_widget("ch1_grupka", "Zmierz grupkę: x̄ się waha",
        steps = c("Grupka", "Powtarzamy", "μ"),
        labels = c("Zmierz grupkę", "Zmierz grupkę", "Zmierz grupkę"),
        more_from = 2,
        config = list(kind = "net", mode = "mean", mu = net_world$mu, sigma = net_world$sigma,
                      n = 25L, xmin = 140, xmax = 200, height = 480,
                      aria = "Student z miarką mierzy grupkę osób wychodzących z sali; średnia grupki x̄ spada żetonem na stos pod osią wzrostu, a w ostatnim kroku widać μ i odchylenie standardowe średnich"))
    ),

    lc_p("Pojedyncze estymaty rozrzucają się po obu stronach μ, ale stos układa
      się wokół μ. Przy n = 25 ", gloss("błąd standardowy"), " wynosi
      σ/√n = 10/√25 = 2 cm, więc około 95% średnich grupek wypada między 166.1
      a 173.9 cm, a SD(x̄) w odczycie zbliża się do 2 cm."),

    lc_h2("ch1-wlasnosci", "Trzy własności dobrego estymatora"),

    lc_p("Średnia z próby nie jest jedynym możliwym estymatorem środka populacji.
      Równie dobrze można by użyć mediany z próby, średniej z najmniejszej
      i największej obserwacji albo ", gloss("średnia ucięta", "średniej uciętej"), ". Żeby wybrać między nimi,
      potrzebujemy kryteriów. Statystyka ocenia estymatory według trzech
      podstawowych własności: ",
      gloss("nieobciążoność", "nieobciążoności"), ", ",
      gloss("efektywność estymatora", "efektywności"), " i ",
      gloss("zgodność estymatora", "zgodności"), ". Poniżej ",
      withMathJax("\\(\\hat{\\theta}\\)"), " oznacza estymator parametru ",
      withMathJax("\\(\\theta\\)"), "."),

    lc_h3("Nieobciążoność", num = "1"),

    lc_p("Estymator jest nieobciążony, gdy jego ", gloss("wartość oczekiwana"), " jest równa
      szacowanemu parametrowi:"),

    lc_formula_box(withMathJax(
      "$$E(\\hat{\\theta}) = \\theta$$"
    )),

    lc_p("Pojedyncza estymata może wypaść za wysoko albo za nisko, ale średnio,
      w bardzo wielu hipotetycznych próbach, estymator trafia w parametr.
      Nie ma błędu systematycznego w jedną stronę."),

    lc_note("Przykład", tags$p("Średnia z próby jest nieobciążonym estymatorem μ.
      To wzór E(X̄) = μ z wykładu 02 i to właśnie widać w scenie powyżej:
      stos średnich układa się wokół μ.")),

    lc_note("Kontrprzykład", tags$p(gloss("wariancja", "Wariancja"), " z próby liczona
      z dzieleniem przez n, ",
      withMathJax("\\(\\frac{1}{n}\\sum(x_i - \\bar{x})^2\\)"), ", jest ", gloss("obciążenie", "obciążona"), ".
      Jej wartość oczekiwana wynosi (n - 1)/n · σ², więc średnio zaniża wariancję
      populacji. Dla n = 10 i σ² = 100 daje średnio 90 zamiast 100. Dlatego
      wariancję z próby liczy się z dzieleniem przez n - 1: ta poprawka usuwa
      obciążenie.")),

    lc_h3("Efektywność", num = "2"),

    lc_p("Nieobciążoność mówi tylko, że estymator trafia średnio. Dwa estymatory
      nieobciążone mogą jednak różnić się rozrzutem: jeden daje estymaty
      skupione blisko parametru, drugi często myli się mocno w jedną lub drugą
      stronę, a błędy znoszą się dopiero po uśrednieniu wielu prób. Spośród
      estymatorów nieobciążonych lepszy jest ten o mniejszej wariancji,
      bo w pojedynczej próbie, a tylko taką zwykle mamy, częściej wypada blisko
      prawdy. Taki estymator nazywamy efektywniejszym."),

    lc_note("Przykład", tags$p("Gdy populacja ma rozkład normalny, zarówno średnia,
      jak i ", gloss("mediana"), " z próby są nieobciążonymi estymatorami μ.
      Przy dużych próbach wariancja mediany jest jednak około π/2 ≈ 1.57 raza
      większa niż wariancja średniej. Mediana z próby liczącej 157 obserwacji
      jest więc mniej więcej tak dokładna jak średnia ze 100 obserwacji.
      Dlatego przy pomiarach o rozkładzie zbliżonym do normalnego standardem
      jest średnia arytmetyczna.")),

    lc_note("Uwaga", tags$p("Efektywność zależy od rozkładu populacji. Gdy w danych
      zdarzają się ", gloss("wartość odstająca", "wartości odstające"), ",
      średnia mocno na nie reaguje i mediana może okazać się efektywniejsza.")),

    lc_h3("Zgodność", num = "3"),

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

    lc_note("Przykład", tags$p("Średnia z próby jest zgodnym estymatorem μ.
      Wynika to z ", gloss("prawo wielkich liczb", "prawa wielkich liczb"),
      ", a widać to też we wzorze na błąd standardowy: ",
      gloss("odchylenie standardowe"), " średniej, SE = σ/√n, maleje do zera
      wraz ze wzrostem n. Wariancja z próby jest zgodna zarówno w wersji
      z n - 1, jak i z n: obciążenie (n - 1)/n znika, gdy n rośnie.")),

    lc_p("Z trzech własności wynika praktyczna kolejność wyboru. Najpierw szukamy
      estymatorów nieobciążonych, spośród nich wybieramy najefektywniejszy,
      a zgodność gwarantuje, że więcej danych daje dokładniejszy wynik.
      Średnia z próby spełnia wszystkie trzy warunki dla μ i dlatego jest
      punktem wyjścia dla przedziałów ufności w tym wykładzie. Zgodność ma
      jednak swoją cenę: SE maleje jak 1/√n, a nie jak 1/n."),

    lc_note("Zasada", rule = TRUE,
      "Żeby zmniejszyć błąd standardowy średniej o połowę, trzeba czterokrotnie
       zwiększyć próbę."
    ),

    lc_h2("ch1-punkt-nie-wystarczy", "Sam punkt nie wystarczy"),

    lc_p("Nawet najlepszy estymator daje w każdej próbie inną estymatę.
      Liczba ", withMathJax("\\(\\bar{x} = 171.3\\)"), " cm podana bez komentarza
      nie mówi, czy prawdziwe μ może wynosić 171 cm, czy równie dobrze 165 cm.
      O tym decyduje rozrzut estymatora, a więc błąd standardowy. Stos ze sceny
      pokazał, że przy n = 25 kolejne średnie lądują o kilka centymetrów w górę
      i w dół od μ. Czterokrotnie większa grupka zmniejszyłaby SE o połowę, ale
      skoki by nie zniknęły. Dowolna pojedyncza estymata może więc leżeć kilka
      centymetrów od μ, a sama liczba nie zdradza, jak daleko."),

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

  # --- PROTOTYP SCENY (2026-10-08): Zmierz grupkę ---
  scene_texts(input, output, "ch1_grupka", list(
    tagList("Zmierz kilka grupek. Pionowa kreska na osi to średnia grupki ",
      tags$code("x̄", .noWS = "outside"), "."),
    tagList("Każda średnia spada na stos. Dorzuć +100 i +1000 grupek."),
    tagList("Przerywana linia to ", tags$code("μ", .noWS = "outside"),
      ". Estymator się waha, μ stoi w miejscu.")
  ))
}
