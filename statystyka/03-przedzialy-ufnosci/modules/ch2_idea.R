# ============================================================================
# CHAPTER 2: Idea przedziałów ufności
# ============================================================================

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

    lc_p("Poprzedni rozdział skończył się odwróceniem zdania z wykładu 02: skoro
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

    lc_formula_box(withMathJax(
      "$$P\\left(\\mu - 1.96 \\cdot SE \\le \\bar{X} \\le \\mu + 1.96 \\cdot SE\\right) = 0.95$$"
    )),

    lc_p("To zdanie opisuje średnią, a nas interesuje μ. Odległość działa jednak
      w obie strony, więc wystarczy przekształcić obie nierówności tak, żeby μ
      znalazło się w środku. Zdarzenie jest to samo, zmienia się tylko zapis,
      dlatego prawdopodobieństwo pozostaje równe 0.95:"),

    lc_formula_box(withMathJax(
      "$$P\\left(\\bar{X} - 1.96 \\cdot SE \\le \\mu \\le \\bar{X} + 1.96 \\cdot SE\\right) = 0.95$$"
    )),

    lc_p("Przedział od x̄ - 1.96·SE do x̄ + 1.96·SE to 95% ",
      gloss("przedział ufności"), " dla μ (CI, od ang. ",
      tags$em("confidence interval"), "), a 95% to jego ",
      gloss("poziom ufności"), ". Warto zauważyć, co w tym wzorze jest losowe.
      Nieznany ", gloss("parametr"), " μ stoi w miejscu. Z próby na próbę zmienia
      się średnia, a razem z nią oba końce przedziału. Prawdopodobieństwo 0.95
      opisuje więc metodę: zanim wylosujemy próbę, wiemy, że przedział zbudowany
      w ten sposób obejmie μ z prawdopodobieństwem 0.95."),

    lc_p("Ten wzór wymaga dwóch uzupełnień. Po pierwsze, liczba 1.96 odpowiada
      poziomowi 95%. Dla 90% w jej miejsce wchodzi 1.645, a dla 99% — 2.576,
      czyli ", gloss("kwantyl", "kwantyle"), " rozkładu N(0, 1), które odcinają odpowiednio po 5% i po 0.5%
      w każdym ogonie. Po drugie, SE zawiera σ, którego zwykle nie znamy. W praktyce
      zastępujemy je odchyleniem standardowym z próby s, a 1.96 — kwantylem
      ", gloss("rozkład t-Studenta", "rozkładu t-Studenta"), " z wykładu 02 (dla n = 30 jest to 2.05). Szczegółami tej
      wersji zajmiemy się w rozdziale 3, ale scena poniżej już jej używa."),

    lc_h2("ch2-wiele-ci", "Wiele przedziałów ufności"),

    lc_p("Definicja mówi o tym, co dzieje się w wielu próbach, a w prawdziwym
      badaniu mamy jedną. Scena pozwala powtórzyć badanie dowolnie wiele razy
      na ", gloss("populacja", "populacji"), ", w której znamy μ: wzrost o rozkładzie
      normalnym z μ = 170 cm i σ = 10 cm. Każda grupka 25 osób daje jeden
      przedział x̄ ± t·s/√n, czyli siatkę zarzuconą na oś wzrostu. W ostatnim
      kroku odczyt pod stosem podaje ", gloss("pokrycie"), ", czyli odsetek
      siatek, które złapały μ."),

    # PROTOTYP SCENY (2026-10-08): Zarzuć siatkę — co znaczy 95%
    figure_panel(
      label = "Prototyp sceny",
      width_mode = "text",
      scene_widget("ch2_siatka", "Zarzuć siatkę: co znaczy 95%",
        steps = c("Siatka", "Powtarzamy", "Gdzie μ?"),
        labels = c("Zarzuć siatkę", "Zarzuć siatkę", "Zarzuć siatkę"),
        more_from = 2,
        config = list(kind = "net", mode = "net", mu = net_world$mu, sigma = net_world$sigma,
                      n = 25L, mult = "t", tq = net_tq, z = 1.96,
                      xmin = 140, xmax = 200, height = 550,
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
      do 1.32, przedziały się zwężają, a chybia średnio co piąty."),

    lc_h2("ch2-jak-interpretowac", "Jak (nie) interpretować przedział ufności"),

    lc_p("Scena pokazuje, co znaczy 95%, gdy przedziałów jest wiele.
      W raporcie mamy jednak jeden przedział i trzeba o nim powiedzieć coś
      prawdziwego. Załóżmy, że z jednej próby otrzymaliśmy 95% przedział ufności
      [165, 175] dla średniego wzrostu w populacji. Z czterech zdań poniżej tylko
      jedno poprawnie opisuje ten wynik."),

    figure_panel(
      label = "Ryc. 2.1", title = "Quiz: interpretacja CI",
      full_width = TRUE,
      p("Wybierz poprawną interpretację:"),
      uiOutput("ch2_quiz_options"),
      uiOutput("ch2_quiz_feedback")
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

  # --- PROTOTYP SCENY (2026-10-08): Zarzuć siatkę ---
  scene_texts(input, output, "ch2_siatka", list(
    tagList("Zmierz grupkę. Siatka to ", tags$code("x̄", .noWS = "outside"),
      " ± margines, czyli 95% przedział ufności dla ", tags$code("μ", .noWS = "outside"), "."),
    tagList("Siatki spadają na stos, najnowsza na górze. ", tags$code("μ", .noWS = "outside"),
      " jest niewidoczne, jak w prawdziwym badaniu. Dorzuć +100 i +1000 siatek."),
    tagList("Przerywana linia to ", tags$code("μ", .noWS = "outside"), ". W długiej serii łapie je około 95% siatek,
      a pojedyncza siatka albo złapała, albo nie.")
  ))

  # --- Widget 1: Quiz (tiles) ---
  ch2_quiz_answered <- reactiveVal(FALSE)
  ch2_quiz_selected <- reactiveVal(NULL)

  ch2_quiz_choices <- list(
    list(letter = "A", value = "A",
         text = "Jest 95% prawdopodobieństwa, że μ leży w [165, 175]"),
    list(letter = "B", value = "B",
         text = "95% danych z populacji leży w [165, 175]"),
    list(letter = "C", value = "C",
         text = "Gdybyśmy powtarzali badanie, 95% tak skonstruowanych przedziałów zawierałoby μ"),
    list(letter = "D", value = "D",
         text = "Jesteśmy w 95% pewni, że średnia z próby leży w [165, 175]")
  )

  output$ch2_quiz_options <- renderUI({
    if (ch2_quiz_answered()) return(NULL)
    div(class = "quiz-tiles quiz-cols-4",
      lapply(ch2_quiz_choices, function(opt) {
        actionButton(paste0("ch2_tile_", opt$value),
          tagList(
            div(class = "tile-letter", opt$letter),
            div(class = "tile-text", opt$text)
          ),
          class = "quiz-tile"
        )
      })
    )
  })

  observe({
    for (opt in ch2_quiz_choices) {
      local({
        val <- opt$value
        observeEvent(input[[paste0("ch2_tile_", val)]], {
          if (ch2_quiz_answered()) return()
          ch2_quiz_selected(val)
          ch2_quiz_answered(TRUE)
        }, ignoreInit = TRUE)
      })
    }
  })

  output$ch2_quiz_feedback <- renderUI({
    req(ch2_quiz_answered())
    answer <- ch2_quiz_selected()
    if (answer == "C") {
      lc_status(
        lc_verdict(tags$strong("Poprawnie."), type = "ok"),
        p("Poziom ufności opisuje metodę, nie konkretny wynik.")
      )
    } else {
      feedback <- switch(answer,
        "A" = "μ jest stałe, a nie losowe. Losowy jest przedział, zanim wylosujemy próbę.",
        "B" = "Przedział dotyczy parametru (średniej), a nie pojedynczych obserwacji.",
        "D" = "Średnia z próby jest środkiem przedziału, więc leży w nim zawsze."
      )
      lc_status(
        lc_verdict(tags$strong("Nie do końca."), type = "danger"),
        p(feedback),
        p("Poprawna odpowiedź to C.")
      )
    }
  })
}
