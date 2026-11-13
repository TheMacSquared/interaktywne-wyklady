# ============================================================================
# CHAPTER 2: Idea przedziałów ufności
# ============================================================================

# Ryc. 2.3: quiz z kafelkami. Jedna odpowiedź: po wyborze kafelki znikają, a pojawia
# się komentarz do wybranej litery. Obsługa w przeglądarce (sf_ch12.js, .c04-quiz),
# więc quiz działa także bez serwera.
ch2_quiz_choices <- list(
  list(value = "A", text = "Jest 95% prawdopodobieństwa, że μ leży w [165, 175]",
       feedback = "μ jest stałe, a nie losowe. Losowy jest przedział, zanim wylosujemy próbę."),
  list(value = "B", text = "95% danych z populacji leży w [165, 175]",
       feedback = "Przedział dotyczy parametru (średniej), a nie pojedynczych obserwacji."),
  list(value = "C", text = "Gdybyśmy powtarzali badanie, 95% tak skonstruowanych przedziałów zawierałoby μ",
       feedback = "Poziom ufności opisuje metodę, nie konkretny wynik."),
  list(value = "D", text = "Jesteśmy w 95% pewni, że średnia z próby leży w [165, 175]",
       feedback = "Średnia z próby jest środkiem przedziału, więc leży w nim zawsze.")
)
ch2_quiz_correct <- "C"

ch2_quiz_ui <- function() {
  tags$div(class = "c04-quiz", `data-correct` = ch2_quiz_correct,
    # Siatka kafelków ma display: grid, które przebiłoby atrybut hidden.
    tags$style(HTML(".c04-quiz [hidden] { display: none !important; }")),
    div(class = "quiz-tiles quiz-cols-4",
      lapply(ch2_quiz_choices, function(opt) {
        tags$button(type = "button", class = "btn btn-default quiz-tile", `data-quiz-choice` = opt$value,
          div(class = "tile-letter", opt$value),
          div(class = "tile-text", opt$text)
        )
      })
    ),
    lapply(ch2_quiz_choices, function(opt) {
      tags$div(`data-quiz-feedback` = opt$value, hidden = NA,
        if (identical(opt$value, ch2_quiz_correct)) {
          lc_status(lc_verdict(tags$strong("Poprawnie."), type = "ok"), p(opt$feedback))
        } else {
          lc_status(lc_verdict(tags$strong("Nie do końca."), type = "danger"), p(opt$feedback),
                    p(paste0("Poprawna odpowiedź to ", ch2_quiz_correct, ".")))
        })
    })
  )
}

ch2_ui <- list(
  id    = "ch-idea",
  num   = "02",
  title = "Idea przedziałów",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 02 · Przedziały ufności",
      num    = "02",
      title  = "Idea przedziałów.",
      lead   = "Estymata punktowa zmienia się z próby na próbę. Przedział ufności
                dokłada do niej zakres niepewności, a poziom ufności mówi, jak często
                metoda, która go wyznacza, trafia w prawdziwą wartość."
    ),

    lc_p("Poprzedni rozdział skończył się odwróceniem zdania z wykładu 03: skoro
      w około 95% prób średnia leży nie dalej niż 1.96·SE od μ, to w tych samych
      próbach μ leży nie dalej niż 1.96·SE od średniej. W tym rozdziale zapiszemy
      to odwrócenie wzorem, nazwiemy powstały zakres i sprawdzimy, czego
      dokładnie dotyczy liczba 95%."),

    lc_h2("ch2-czym-jest", "Czym jest przedział ufności?"),

    lc_p("Z ", gloss("centralne twierdzenie graniczne", "centralnego twierdzenia
      granicznego"), " wiemy, że średnia z próby X̄ ma w przybliżeniu ", gloss("rozkład normalny"), "
      o średniej μ i ", gloss("odchylenie standardowe", "odchyleniu standardowym"), " SE = σ/√n, czyli ",
      gloss("błąd standardowy", "błędzie standardowym"), ". W rozkładzie normalnym
      95% wartości leży nie dalej niż 1.96 odchylenia standardowego od środka.
      Dla średniej oznacza to, że w około 95% prób X̄ leży nie dalej niż 1.96·SE
      od μ:"),

    lc_formula_annotated(
      "P\\left(⟦1|\\mu⟧ - ⟦2|1.96⟧ \\cdot ⟦3|SE⟧ \\le ⟦4|\\bar{X}⟧ \\le \\mu + 1.96 \\cdot SE\\right) = ⟦5|0.95⟧",
      notes  = list("średnia populacji", "wartość krytyczna", "błąd standardowy",
                    "średnia z próby", "poziom ufności"),
      colors = c("param", "terakota", "wrzos", "accent", "niebo"),
      above  = c(2, 4)
    ),

    lc_p("To zdanie opisuje średnią, a nas interesuje μ. Odległość działa jednak
      w obie strony, więc wystarczy przekształcić obie nierówności tak, żeby μ
      znalazło się w środku. Zdarzenie jest to samo, zmienia się tylko zapis,
      dlatego prawdopodobieństwo pozostaje równe 0.95:"),

    lc_formula_annotated(
      "P\\left(⟦1|\\bar{X}⟧ - 1.96 \\cdot SE \\le ⟦2|\\mu⟧ \\le \\bar{X} + 1.96 \\cdot SE\\right) = 0.95",
      notes  = list("losowy środek", "stały parametr"),
      colors = c("accent", "param")
    ),

    # Pełne wyprowadzenie: od standaryzacji przez algebrę nierówności do t*.
    lc_more("Wyprowadzenie krok po kroku",

      lc_p("Zapiszmy to przejście dla dowolnego poziomu ufności. Poziom ufności
        oznaczamy 1 − α, więc dla 95% α = 0.05. Liczba \\(z_{\\alpha/2}\\) to
        wartość, powyżej której leży α/2 rozkładu normalnego standardowego.
        Dla 95% powyżej leży 0.025, a poniżej minus tej wartości drugie 0.025,
        stąd \\(z_{0.025} = 1.96\\)."),

      lc_p("Krok 1. Standaryzujemy średnią: odejmujemy jej środek μ i dzielimy
        przez błąd standardowy. Z centralnego twierdzenia granicznego wynik ma
        w przybliżeniu rozkład normalny standardowy:"),
      lc_formula_box("$$Z = \\frac{\\bar{X} - \\mu}{\\sigma/\\sqrt{n}} \\sim N(0,\\,1)$$"),

      lc_p("Krok 2. W rozkładzie N(0, 1) środkowy pas od \\(-z_{\\alpha/2}\\)
        do \\(z_{\\alpha/2}\\) ma prawdopodobieństwo 1 − α:"),
      lc_formula_box("$$P\\left(-z_{\\alpha/2} \\le Z \\le z_{\\alpha/2}\\right) = 1 - \\alpha$$"),

      lc_p("Krok 3. W miejsce Z wstawiamy jego definicję:"),
      lc_formula_box("$$P\\left(-z_{\\alpha/2} \\le \\frac{\\bar{X} - \\mu}{\\sigma/\\sqrt{n}} \\le z_{\\alpha/2}\\right) = 1 - \\alpha$$"),

      lc_p("Krok 4. Mnożymy wszystkie trzy części przez \\(\\sigma/\\sqrt{n}\\).
        Ta liczba jest dodatnia, więc kierunek nierówności się nie zmienia:"),
      lc_formula_box("$$P\\left(-z_{\\alpha/2} \\cdot \\frac{\\sigma}{\\sqrt{n}} \\le \\bar{X} - \\mu \\le z_{\\alpha/2} \\cdot \\frac{\\sigma}{\\sqrt{n}}\\right) = 1 - \\alpha$$"),

      lc_p("Krok 5. Odejmujemy \\(\\bar{X}\\) od wszystkich trzech części,
        żeby w środku zostało samo μ, na razie ze znakiem minus:"),
      lc_formula_box("$$P\\left(-\\bar{X} - z_{\\alpha/2} \\cdot \\frac{\\sigma}{\\sqrt{n}} \\le -\\mu \\le -\\bar{X} + z_{\\alpha/2} \\cdot \\frac{\\sigma}{\\sqrt{n}}\\right) = 1 - \\alpha$$"),

      lc_p("Krok 6. Mnożymy przez −1. Mnożenie przez liczbę ujemną odwraca
        nierówności, więc lewy i prawy koniec zamieniają się miejscami:"),
      lc_formula_box("$$P\\left(\\bar{X} - z_{\\alpha/2} \\cdot \\frac{\\sigma}{\\sqrt{n}} \\le \\mu \\le \\bar{X} + z_{\\alpha/2} \\cdot \\frac{\\sigma}{\\sqrt{n}}\\right) = 1 - \\alpha$$"),

      lc_p("Na każdym kroku zmieniał się tylko zapis, a zdarzenie pozostawało to
        samo, dlatego prawdopodobieństwo wciąż wynosi 1 − α. Po wylosowaniu
        próby wstawiamy zaobserwowaną średnią \\(\\bar{x}\\) i dostajemy
        przedział \\(\\bar{x} \\pm z_{\\alpha/2} \\cdot \\sigma/\\sqrt{n}\\).
        Dla α = 0.05 jest to wzór z 1.96 zapisany powyżej."),

      lc_p("Krok 7. Gdy σ nie znamy, w kroku 1 wstawiamy odchylenie standardowe
        z próby s. Taka statystyka nie ma już rozkładu normalnego, tylko rozkład
        t-Studenta z n − 1 stopniami swobody. Dokładnie tak jest, gdy wartości
        w populacji mają rozkład normalny, a w przybliżeniu także w innych
        przypadkach, jeśli próba jest duża:"),
      lc_formula_box("$$T = \\frac{\\bar{X} - \\mu}{s/\\sqrt{n}} \\sim t_{n-1}$$"),

      lc_p("Kroki 2–6 przebiegają tak samo, tylko w miejsce \\(z_{\\alpha/2}\\)
        wchodzi kwantyl rozkładu t, \\(t^* = t_{\\alpha/2,\\,n-1}\\):"),
      lc_formula_box("$$\\bar{x} \\pm t_{\\alpha/2,\\,n-1} \\cdot \\frac{s}{\\sqrt{n}}$$"),

      lc_p("Przy n = 25 i 95% ufności \\(t_{0.025,\\,24} = 2.06\\). Z tego wzoru
        korzystają scena z siatkami w następnej sekcji i rozdział 3.")
    ),

    # Ta sama odległość czytana od μ i od x̄ (Ryc. 2.1, fig_2a_ui w figures_ch2.R)
    fig("2a"),

    lc_p("Przedział od x̄ - 1.96·SE do x̄ + 1.96·SE to 95% ",
      gloss("przedział ufności"), " dla μ (CI, od ang. ",
      tags$em("confidence interval"), "), a 95% to jego ",
      gloss("poziom ufności"), ". Warto zauważyć, co w tym wzorze jest losowe.
      Nieznany ", gloss("parametr"), " μ stoi w miejscu. Z próby na próbę zmienia
      się średnia, a razem z nią oba końce przedziału. Prawdopodobieństwo 0.95
      opisuje więc metodę: zanim wylosujemy próbę, wiemy, że przedział zbudowany
      w ten sposób obejmie μ z prawdopodobieństwem 0.95."),

    lc_p("W praktyce σ nie znamy, więc SE liczymy z odchylenia standardowego
      próby s, a zamiast 1.96 bierzemy nieco większy mnożnik t* z ",
      gloss("rozkład t-Studenta", "rozkładu t-Studenta"), " (przy 25 osobach 2.06).
      Tak liczy też scena poniżej."),

    lc_h2("ch2-wiele-ci", "Wiele przedziałów ufności"),

    lc_p("Definicja mówi o tym, co dzieje się w wielu próbach, a w prawdziwym
      badaniu mamy jedną. Scena pozwala powtórzyć badanie dowolnie wiele razy
      na ", gloss("populacja", "populacji"), ", w której znamy μ: wzrost o rozkładzie
      normalnym z μ = 170 cm i σ = 10 cm. Każda grupka 25 osób daje jeden
      przedział x̄ ± t·s/√n, czyli siatkę zarzuconą na oś wzrostu. W ostatnim
      kroku odczyt pod stosem podaje ", gloss("pokrycie"), ", czyli odsetek
      siatek, które złapały μ."),

    # Scena: Zarzuć siatkę — co znaczy 95%
    figure_panel(
      label = "Scena",
      width_mode = "wide",
      scene_widget("ch2_siatka", "Zarzuć siatkę: co znaczy 95%",
        steps = c("Siatka", "Powtarzamy", "Gdzie μ?"),
        labels = c("Zarzuć siatkę", "Zarzuć siatkę", "Zarzuć siatkę"),
        more_from = 2,
        config = list(kind = "net", mode = "net", style = "editorial", mu = net_world$mu, sigma = net_world$sigma,
                      n = 25L, mult = "t", tq = net_tq, z = 1.96,
                      xmin = 140, xmax = 200, height = 600,
                      aria = "Student z miarką mierzy grupkę osób wychodzących z sali, zarzuca przedział x̄ ± margines na oś wzrostu; kolejne przedziały układają się pod osią, a w ostatnim kroku widać μ i odsetek trafień"))
    ),

    lc_p("Przy n = 25 i poziomie 95% każda siatka sięga około 4.1 cm w każdą
      stronę od swojej średniej (t = 2.06, σ/√n = 2 cm), a średnie rozrzucają
      się wokół 170 cm. Siatki różnią się położeniem i nieco szerokością, bo s
      też zmienia się z próby na próbę. Większość z nich łapie μ, ale co jakiś
      czas trafia się chybiona."),

    lc_p("Przy małej liczbie przedziałów pokrycie mocno skacze. Wszystkie 10
      przedziałów trafia w około 60% serii (0.95¹⁰ ≈ 0.60), a w około 9% serii
      chybiają co najmniej dwa, co daje pokrycie 80% lub mniej. Im więcej
      przedziałów, tym bliżej 95%: przy 200 przedziałach odchylenie standardowe
      pokrycia wynosi około 1.5 punktu procentowego, a wynik prawie na pewno
      mieści się między 90% a 100%. Poziom ufności jest więc długookresową
      częstością trafień metody, a nie gwarancją dla żadnej pojedynczej serii."),

    lc_p("Chybione przedziały nie mają żadnej wady konstrukcyjnej. Powstały tą samą
      metodą co trafione, tylko z próby, której średnia wypadła daleko od μ.
      W prawdziwym badaniu nie wiemy, gdzie leży μ, więc nie da się też
      rozpoznać, czy nasz jedyny przedział jest jednym z trafionych."),

    lc_p("Pewność ma swoją cenę. Przy 99% kwantyl t rośnie do 2.80, przedziały
      są wyraźnie szersze i chybia średnio jeden na sto. Przy 80% kwantyl spada
      do 1.32, przedziały się zwężają, a chybia średnio co piąty. Na Ryc. 2.2
      zmiana poziomu ufności przelicza te same sto badań, więc widać, ile
      trafień kosztuje węższy przedział."),

    ci_explorer("coverage"),

    lc_h2("ch2-jak-interpretowac", "Jak (nie) interpretować przedział ufności"),

    lc_p("Scena pokazuje, co znaczy 95%, gdy przedziałów jest wiele.
      W raporcie mamy jednak jeden przedział i trzeba o nim powiedzieć coś
      prawdziwego. Załóżmy, że z jednej próby otrzymaliśmy 95% przedział ufności
      [165, 175] dla średniego wzrostu w populacji. Z czterech zdań poniżej tylko
      jedno poprawnie opisuje ten wynik."),

    figure_panel(
      label = "Ryc. 2.3", title = "Quiz: interpretacja CI",
      full_width = TRUE,
      p("Wybierz poprawną interpretację:"),
      ch2_quiz_ui()
    ),

    lc_p("Poprawne jest zdanie C, bo mówi o metodzie. Zdanie A brzmi podobnie
      i jest najczęstszym błędem. Po wylosowaniu próby zarówno μ, jak i końce
      przedziału, 165 i 175, są ustalonymi liczbami. μ albo leży w tym przedziale,
      albo nie, tylko my nie wiemy, który przypadek zachodzi. W ujęciu, którego
      używamy, nie ma tu już niczego losowego, czemu można by przypisać
      prawdopodobieństwo 95%. Dlatego mówimy o ufności, a nie
      o prawdopodobieństwie: ufamy przedziałowi, bo powstał metodą, która
      trafia w 95% prób."),

    lc_p("Zdanie B myli parametr z pojedynczymi obserwacjami. Przedział ufności
      szacuje średnią populacji, a nie zakres, w którym mieszczą się ludzie. Przy
      σ = 10 cm 95% wzrostów w populacji leży w pasie μ ± 19.6 cm, prawie czterokrotnie
      szerszym niż przedział [165, 175]. Co więcej, przedział ufności zwęża się
      wraz ze wzrostem próby, a rozrzut wzrostu w populacji nie zależy od tego,
      ilu ludzi zmierzyliśmy. Zdanie D nie mówi nic: średnia z próby, 170 cm,
      jest środkiem przedziału, więc leży w nim zawsze."),

    lc_p("Ryc. 2.4 zestawia oba pasy na jednej osi. Gdy zwiększamy liczbę osób,
      zwęża się tylko przedział ufności średniej, a pas, w którym mieści się
      95% wzrostów, zostaje taki sam."),

    ci_explorer("people"),

    lc_note("Zasada", rule = TRUE,
      tagList(
        "95% to własność metody, nie konkretnego przedziału. O jednym przedziale
         mówimy: z 95% ufnością μ leży między 165 a 175 cm. Rozumiemy przez to,
         że przedział wyznaczono metodą, która obejmuje μ w 95% prób."
      )
    ),

    lc_p("Wiemy już, skąd bierze się przedział ufności i co mówi jego poziom.
      W następnym rozdziale policzymy go dla średniej, gdy σ trzeba oszacować
      z danych, i przyjrzymy się każdemu składnikowi wzoru."),

    lc_chapter_next(
      num       = "03",
      title     = "Przedział dla średniej",
      lead      = "wzór x̄ ± t·s/√n i jak go liczyć",
      target_id = "ch-srednia"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  # --- Scena: Zarzuć siatkę ---
  scene_texts(input, output, "ch2_siatka", list(
    tagList("Zmierz grupkę. Siatka to ", tags$code("x̄", .noWS = "outside"),
      " ± margines, czyli 95% przedział ufności dla ", tags$code("μ", .noWS = "outside"), "."),
    tagList("Siatki spadają na stos, najnowsza na górze. ", tags$code("μ", .noWS = "outside"),
      " jest niewidoczne, jak w prawdziwym badaniu. Dorzuć +100 i +1000 siatek."),
    tagList("Przerywana linia to ", tags$code("μ", .noWS = "outside"), ". W długiej serii łapie je około 95% siatek,
      a pojedyncza siatka albo złapała, albo nie.")
  ))

  # Ryc. 2.3 (quiz) obsługuje przeglądarka: ch2_quiz_ui() i sf_ch12.js.
}
