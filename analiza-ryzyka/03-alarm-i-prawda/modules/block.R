# Blok 03: Alarm i prawda -------------------------------------------------

alarm_quiz <- list(
  question = "Czujnik ma czułość 95%. Czy po alarmie prawdopodobieństwo awarii wynosi 95%?",
  choices = c(
    "Nie — zależy także od częstości bazowej i fałszywych alarmów" = "no",
    "Tak — czułość jest odpowiedzią na to pytanie" = "yes",
    "Tak, jeśli awarie są rzadkie" = "rare"
  ),
  correct = "no",
  explanation = "Czułość to P(alarm | awaria), a pytanie po alarmie dotyczy P(awaria | alarm)."
)

alarm_exercises <- c(
  "Bananpol: dla 10 000 zmian, częstości awarii 0,01, czułości 0,95 i FPR 0,05 policz, ile alarmów będzie prawdziwych.",
  "Diagnostyka: wyjaśnij, dlaczego dwóch czujników z tym samym zasilaniem nie wolno automatycznie traktować jako niezależnych.",
  "Transfer: zaproponuj naturalne częstości dla testu przesiewowego w medycynie i nazwij właściwy mianownik."
)

alarm_terms_table <- figure_panel(
  label = "Słownik",
  title = "Cztery liczby opisujące detektor",
  full_width = TRUE,
  tags$table(
    class = "lc-table lc-table-striped lc-table-bordered",
    tags$thead(tags$tr(
      tags$th("Nazwa"), tags$th("Zapis warunkowy"), tags$th("Mianownik"), tags$th("W Bananpolu")
    )),
    tags$tbody(
      tags$tr(tags$td("Częstość bazowa"), tags$td("P(awaria)"), tags$td("wszystkie zmiany"), tags$td("0,01")),
      tags$tr(tags$td("Czułość"), tags$td("P(alarm | awaria)"), tags$td("zmiany z awarią"), tags$td("0,95")),
      tags$tr(tags$td("Odsetek fałszywych alarmów"), tags$td("P(alarm | brak awarii)"), tags$td("zmiany bez awarii"), tags$td("0,05")),
      tags$tr(tags$td("Wiarygodność alarmu"), tags$td("P(awaria | alarm)"), tags$td("wszystkie alarmy"), tags$td("wynik tego wykładu"))
    )
  )
)

alarm_paths_widget <- figure_panel(
  label = "Przykład liczbowy",
  title = "Dwie drogi do alarmu na 10 000 zmian",
  full_width = TRUE,
  lc_stat_grid(
    lc_stat_box("Krok 1 · Awarie", "100 na 10 000 zmian", caption = "P(awaria) = 0,01", color = upwr_cat[["terakota"]]),
    lc_stat_box("Krok 2a · Prawdziwe alarmy", "95 na 100 awarii", caption = "czułość 0,95", color = upwr_cat[["niebo"]]),
    lc_stat_box("Krok 2b · Fałszywe alarmy", "495 na 9900 zmian bez awarii", caption = "FPR 0,05", color = upwr_cat[["bursztyn"]]),
    columns = 3
  ),
  lc_formula_box(withMathJax("$$P(\\text{awaria}\\mid\\text{alarm})=\\frac{95}{95+495}\\approx 0{,}16$$")),
  lc_feedback(
    type = "info",
    tags$strong("Czytaj mianownik:"),
    " licznik to jedna droga (awaria i alarm), a mianownik to wszystkie zmiany
      kończące się alarmem — z awarią i bez niej. Fałszywych alarmów jest pięć
      razy więcej niż prawdziwych, bo zmian bez awarii jest aż 99 razy więcej."
  )
)

alarm_sciaga_widget <- tagList(
  figure_panel(
    label = "Ściąga 3.1",
    title = "Audyt alarmu w pięciu krokach",
    full_width = TRUE,
    tags$table(
      class = "lc-table lc-table-striped lc-table-bordered",
      tags$thead(tags$tr(tags$th("Krok"), tags$th("Pytanie"), tags$th("Typowy błąd"))),
      tags$tbody(
        tags$tr(tags$td("Pytanie"), tags$td("Czy pytamy o P(alarm | awaria), czy o P(awaria | alarm)?"), tags$td("utożsamienie obu kierunków")),
        tags$tr(tags$td("Populacja"), tags$td("W jakiej populacji zmian działa detektor?"), tags$td("pominięcie częstości bazowej")),
        tags$tr(tags$td("Detektor"), tags$td("Skąd znamy czułość i FPR i czy są stabilne?"), tags$td("dane z innej populacji lub innych warunków")),
        tags$tr(tags$td("Posterior"), tags$td("Ile alarmów jest prawdziwych na 10 000 zmian?"), tags$td("raportowanie samego procentu bez liczebności")),
        tags$tr(tags$td("Konsekwencje"), tags$td("Co kosztuje reakcja, a co jej brak?"), tags$td("automatyczne utożsamienie posterioru z decyzją"))
      )
    )
  ),
  lc_formula_box(
    withMathJax("$$P(A\\mid +)=\\frac{P(+\\mid A)\\,P(A)}{P(+\\mid A)\\,P(A)+P(+\\mid \\neg A)\\,P(\\neg A)}$$"),
    tags$p("Licznik jest drogą przez awarię; mianownik sumą obu dróg kończących się alarmem.")
  ),
  risk_assessment_ui("a3", alarm_quiz, alarm_exercises)
)

alarm_block <- list(
  id = "alarm", title = "Alarm i prawda",
  chapters = list(
    list(
      id = "intuicja", title = "Dobry czujnik, trudne pytanie",
      lead = "Odwrócenie warunku potrafi całkowicie zmienić odpowiedź.",
      intro = c(
        "Trzecia w nocy, telefon z chłodni Bananpolu: czujnik przegrzania znowu alarmuje. Wysyłać ekipę? Odpowiedź zależy od liczby, o którą mało kto pyta w środku nocy — od tego, jak często awaria zdarza się w ogóle.",
        "W poprzednim wykładzie nauczyliśmy się filtrować mianownik warunkiem. Dziś wykonamy najtrudniejszy manewr tego kursu: odwrócenie kierunku warunku. Producent czujnika podaje P(alarm | awaria); dyżurny przy telefonie potrzebuje P(awaria | alarm). To nie są te same liczby — a różnica między nimi bywa dziesięciokrotna."
      ),
      callout = list(
        label = "Dane Bananpolu",
        text = "Detektor przegrzania w dojrzewalni: częstość awarii 0,01 na zmianę, czułość 0,95, fałszywe alarmy w 0,05 zmian bez awarii. Jednostka: zmiana pracy dojrzewalni; horyzont: 10 000 porównywalnych zmian. Liczby są fikcyjne.",
        color = "uwaga"
      ),
      sections = list(list(
        id = "pytanie", title = "Alarm na zmianie",
        text = c(
          "Awaria zdarza się średnio na 1 zmianie na 100. Czujnik wykrywa 95% awarii, ale w 5% zmian bez awarii alarmuje fałszywie. Najpierw oszacuj wiarygodność alarmu bez rachunku — zapisz swoją liczbę, zanim klikniesz dalej.",
          "To pytanie ma długą historię błędnych odpowiedzi: w badaniach z udziałem lekarzy interpretujących wyniki testów przesiewowych większość odpowiadała „około 95%”, myląc czułość testu z jego wiarygodnością. Zaraz sprawdzisz, po której stronie tej statystyki jesteś."
        )
      )),
      widget = risk_vote_panel(
        "a3_vote", "a3_vote_feedback",
        "Po alarmie: jak duża jest szansa rzeczywistej awarii?",
        c("Około 95%" = "95", "Około 16%" = "16", "Nie da się określić bez częstości bazowej" = "base")
      )
    ),
    list(
      id = "detektor", title = "Język detektora",
      lead = "Czułość i odsetek fałszywych alarmów opisują dwa różne wiersze tablicy.",
      intro = c(
        "Zanim policzymy cokolwiek, uporządkujmy słownik. Każda zmiana pracy dojrzewalni kończy się jednym z czterech wyników: awaria z alarmem albo bez, brak awarii z alarmem albo bez. Cała wiedza o detektorze mieści się w tym, jak często trafia do każdej z czterech komórek.",
        "Zwróć uwagę, że czułość i odsetek fałszywych alarmów mają różne mianowniki: pierwsza jest liczona wśród zmian z awarią, drugi wśród zmian bez awarii. Właśnie dlatego nie można ich dodawać, odejmować ani porównywać wprost — to częstości z dwóch różnych światów."
      ),
      sections = list(list(
        id = "definicje", title = "Cztery wyniki",
        bullets = c("prawdziwie dodatni: awaria i alarm", "fałszywie dodatni: brak awarii, ale alarm", "fałszywie ujemny: awaria bez alarmu", "prawdziwie ujemny: brak awarii i brak alarmu")
      )),
      widget = tagList(
        alarm_terms_table,
        figure_panel(
          label = "Tablica 2×2", title = "Zmień parametry detektora",
          fluidRow(
            column(
              4,
              sliderInput("a3_prev", "Częstość awarii", 0.001, 0.10, 0.01, 0.001),
              sliderInput("a3_sens", "Czułość", 0.50, 1, 0.95, 0.01),
              sliderInput("a3_fpr", "Fałszywie dodatnie", 0, 0.30, 0.05, 0.01)
            ),
            column(8, tableOutput("a3_table"))
          ), full_width = TRUE
        )
      ),
      pitfall = "Wysoka czułość nie oznacza, że większość alarmów jest prawdziwa."
    ),
    list(
      id = "czestosci", title = "Naturalne częstości",
      lead = "Zamiast trzech procentów śledzimy konkretne zmiany produkcyjne.",
      intro = c(
        "Trzy procenty naraz — częstość bazowa, czułość, FPR — przeciążają intuicję, bo każdy odnosi się do innego mianownika. Naturalne częstości rozbrajają problem: zamiast ułamków wyobrażamy sobie 10 000 konkretnych zmian i śledzimy, ile z nich trafia do każdej grupy.",
        "Na siatce poniżej każdy punkt to jedna zmiana. Widać od razu to, co ukrywają procenty: zmian bez awarii jest tak dużo, że nawet rzadkie fałszywe alarmy tworzą tłum liczniejszy niż wszystkie prawdziwe alarmy razem wzięte."
      ),
      sections = list(list(
        id = "mianownik", title = "Wszystkie alarmy",
        text = "Dla pytania po alarmie mianownikiem są prawdziwe i fałszywe alarmy razem. Dyżurny nie wie, z której grupy pochodzi jego telefon — wie tylko, że alarm jest. Dlatego wiarygodność alarmu to udział prawdziwych alarmów wśród wszystkich alarmów, a nie wśród awarii."
      )),
      widget = risk_widget_panel("Symulacja", "10 000 zmian Bananpolu", tagList(
        p("Parametry są synchronizowane z tablicą 2×2."), uiOutput("a3_counts")
      ),
      plot_id = "a3_grid", height = "390px"
      )
    ),
    list(
      id = "bayes", title = "Od drzewa do Bayesa",
      lead = "Licznik jest jedną drogą, mianownik sumą wszystkich dróg kończących się alarmem.",
      intro = "Wzór przyjdzie na końcu — najpierw jeszcze raz przejdźmy drogę na konkretnych zmianach. Alarm może powstać na dwóch rozłącznych drogach: po awarii (droga przez czułość) albo bez awarii (droga przez fałszywe alarmy). Wiarygodność alarmu to udział pierwszej drogi w sumie obu — policzmy go krok po kroku na 10 000 zmian.",
      sections = list(list(
        id = "drogi", title = "Dwie drogi do alarmu",
        text = "Alarm może powstać po awarii albo bez awarii. O wyniku decydują względne szerokości obu dróg: jeśli droga fałszywa jest szersza od prawdziwej, większość alarmów jest fałszywa — niezależnie od tego, jak dobra jest czułość."
      )),
      widget = tagList(
        alarm_paths_widget,
        lc_p("Iloraz, który właśnie policzyliśmy — jedna droga podzielona przez sumę wszystkich dróg kończących się alarmem — ma swoją nazwę i ogólny zapis:"),
        lc_formula_box(
          withMathJax("$$P(A\\mid +)=\\frac{P(+\\mid A)P(A)}{P(+\\mid A)P(A)+P(+\\mid \\neg A)P(\\neg A)}$$"),
          tags$p("Licznik jest drogą przez awarię; mianownik sumą obu dróg kończących się alarmem.")
        ),
        lc_p(
          "Wzór Bayesa nie wnosi nowej matematyki — porządkuje rachunek, który
           wykonaliśmy na zmianach. Warto rozpoznać w mianowniku starego znajomego:
           to wzór na prawdopodobieństwo całkowite z poprzedniego wykładu,
           zastosowany do zdarzenia „alarm”. Nowa jest tylko nazwa."
        )
      ),
      decision = "Komunikuj posterior wraz z liczebnościami, a nie samą czułość."
    ),
    list(
      id = "baza", title = "Pułapka częstości bazowej",
      lead = "Ten sam czujnik daje inną wiarygodność alarmu w innej populacji.",
      intro = c(
        "Wiarygodność alarmu nie jest cechą czujnika — jest cechą pary: czujnik plus populacja, w której pracuje. Ten sam model detektora zamontowany w hali o rzadkich awariach będzie „krzyczał wilk” znacznie częściej niż w hali, gdzie awarie są powszechne.",
        "Krzywa poniżej pokazuje tę zależność w całym zakresie. Zauważ, jak stromo rośnie na początku: przy bardzo rzadkich awariach niewielka zmiana częstości bazowej silnie zmienia sens alarmu. To dlatego przenoszenie parametrów detektora między instalacjami bez sprawdzenia częstości bazowej jest błędem, a nie oszczędnością."
      ),
      sections = list(list(
        id = "transfer", title = "Przykład transferowy: test przesiewowy",
        text = "Identyczny mechanizm działa w medycynie. Test przesiewowy o czułości 90% i FPR 9% stosowany w populacji, w której choroba dotyka 1% badanych, daje wynik dodatni, który potwierdza się w mniej więcej jednym przypadku na dziesięć. Dlatego po badaniu przesiewowym wykonuje się test potwierdzający — i dlatego programy przesiewowe kieruje się do grup o podwyższonej częstości bazowej."
      )),
      widget = risk_widget_panel(
        "Krzywa", "P(awaria | alarm) a częstość bazowa",
        tagList(
          sliderInput("a3_curve_sens", "Czułość", 0.5, 1, 0.95, 0.01),
          sliderInput("a3_curve_fpr", "FPR", 0.001, 0.20, 0.05, 0.001)
        ),
        "a3_curve", "a3_posterior"
      ),
      pitfall = "Porównywanie czujników bez podania populacji zastosowania bywa pozorne."
    ),
    list(
      id = "druga-informacja", title = "Druga informacja",
      lead = "Drugi alarm pomaga tylko w takim stopniu, w jakim wnosi nową informację.",
      intro = c(
        "Naturalny odruch po niepewnym alarmie to sięgnięcie po drugie źródło: drugi czujnik, odczyt ręczny, telefon do operatora. Rachunek jest optymistyczny — jeśli druga informacja jest warunkowo niezależna od pierwszej, posterior po pierwszym alarmie staje się częstością bazową dla drugiego i wiarygodność szybko rośnie.",
        "Cały zysk wisi jednak na słowie „niezależna”. Dwa identyczne czujniki obok siebie mogą reagować na to samo zakłócenie elektromagnetyczne, ten sam kurz i tę samą wilgoć. Suwak poniżej pokazuje, jak zysk z drugiego alarmu topnieje, gdy rośnie udział wspólnego trybu fałszywego alarmu."
      ),
      sections = list(list(
        id = "niezaleznosc", title = "Założenie warunkowej niezależności",
        text = "Dwa czujniki mogą reagować na to samo zakłócenie lub utracić wspólne zasilanie. Warunkowa niezależność oznacza, że przy ustalonym stanie instalacji (awaria albo jej brak) wynik jednego czujnika nie zmienia prawdopodobieństwa wyniku drugiego — i to założenie trzeba uzasadnić mechanizmem, tak jak w poprzednim wykładzie."
      )),
      widget = figure_panel(
        label = "Porównanie", title = "Dwa alarmy",
        sliderInput("a3_dependence", "Udział wspólnego trybu fałszywego alarmu", 0, 1, 0, 0.05),
        uiOutput("a3_second"), full_width = TRUE
      ), extension = TRUE
    ),
    list(
      id = "reakcja", title = "Reakcja jest osobnym problemem",
      lead = "Posterior opisuje przekonanie; decyzja wymaga jeszcze konsekwencji.",
      intro = c(
        "Policzyliśmy: po alarmie szansa awarii wynosi około 16%. Czy wysłać ekipę? Sama liczba nie odpowiada, bo decyzja zależy również od tego, co jest na szali. Wyjazd do fałszywego alarmu kosztuje godzinę pracy ekipy; zignorowanie prawdziwej awarii może kosztować całą partię owoców albo pożar instalacji.",
        "Gdy konsekwencje są tak asymetryczne, niski posterior może w pełni uzasadniać reakcję. Regułę reakcji ustala się przed nocnym telefonem, na chłodno: przy jakim poziomie wiarygodności i jakich kosztach jedziemy zawsze, a kiedy wystarczy zdalna weryfikacja."
      ),
      sections = list(list(
        id = "macierz", title = "Macierz konsekwencji",
        bullets = c("alarmuj przy awarii — uniknięta szkoda", "alarmuj bez awarii — koszt postoju", "nie alarmuj przy awarii — możliwa katastrofa", "nie alarmuj bez awarii — brak działania")
      )),
      decision = "Ustal próg reakcji jawnie na podstawie kosztów i wykonalności, nie na podstawie samego posteriora."
    ),
    list(
      id = "sprawdzenie", title = "Ściąga i sprawdzenie",
      lead = "Pytanie → populacja → detektor → posterior → konsekwencje.",
      intro = "Największym ryzykiem tego wykładu nie jest błąd rachunkowy, lecz odpowiedź na niewłaściwe pytanie. Ściąga porządkuje audyt alarmu od pytania do decyzji; quiz i ćwiczenia sprawdzają, czy odróżniasz kierunki warunkowania bez podpowiedzi.",
      sections = list(list(
        id = "sciaga", title = "Ściąga",
        bullets = c("Pytanie: co oznacza alarm?", "Model: Bayes lub naturalne częstości", "Założenia: częstość bazowa, stabilne parametry, zależności", "Wynik: P(awaria | alarm)", "Interpretacja: nie jest automatyczną decyzją")
      ), list(
        id = "most", title = "Co dalej",
        text = "Alarm dotyczył pojedynczej zmiany. W następnym wykładzie zmienimy skalę: policzymy, ile zdarzeń pojawi się w całej serii wielu porównywalnych prób."
      )),
      widget = alarm_sciaga_widget
    )
  )
)

alarm_chapters <- risk_block_chapters(alarm_block)

alarm_server <- function(input, output, session) {
  checked <- reactiveVal(FALSE)
  observeEvent(input$a3_vote_check, checked(TRUE))
  output$a3_vote_feedback <- renderUI({
    req(checked())
    if (is.null(input$a3_vote)) {
      return(lc_feedback(type = "info", "Najpierw zaznacz jedną z odpowiedzi."))
    }
    if (identical(input$a3_vote, "16")) {
      lc_feedback(
        type = "ok",
        tags$strong("Około 16%."),
        " Na 1000 zmian przypada około 10 awarii i 9–10 prawdziwych alarmów, ale też około 50 fałszywych alarmów z 990 zmian bez awarii. Większość alarmów jest fałszywa."
      )
    } else if (identical(input$a3_vote, "base")) {
      lc_feedback(
        type = "info",
        tags$strong("Dobry odruch, ale częstość bazowa jest podana:"),
        " 1 awaria na 100 zmian. Z nią wynik da się policzyć — około 16%. Bez częstości bazowej odpowiedź rzeczywiście byłaby niemożliwa."
      )
    } else {
      lc_feedback(
        type = "warning",
        tags$strong("95% to czułość, czyli P(alarm | awaria)."),
        " Pytanie po alarmie dotyczy P(awaria | alarm). Przy rzadkich awariach większość alarmów pochodzi ze zmian bez awarii i wynik spada do około 16%."
      )
    }
  })
  detector <- reactive(risk_detector_counts(10000L, input$a3_prev, input$a3_sens, input$a3_fpr))
  output$a3_table <- renderTable(detector(), striped = TRUE, bordered = TRUE)
  output$a3_counts <- renderUI({
    d <- detector()
    positives <- sum(d$alarm)
    posterior <- d$alarm[1] / positives
    lc_stat_grid(lc_stat_box("Prawdziwe alarmy", d$alarm[1]),
      lc_stat_box("Fałszywe alarmy", d$alarm[2]),
      lc_stat_box("P(awaria | alarm)", risk_format_probability(posterior), color = upwr_accent),
      columns = 1
    )
  })
  grid_plot <- reactive({
    d <- detector()
    counts <- c(d$alarm[1], d$alarm[2], d$no_alarm[1], d$no_alarm[2])
    labels <- c("Awaria + alarm", "Brak awarii + alarm", "Awaria bez alarmu", "Brak awarii bez alarmu")
    dat <- data.frame(type = factor(rep(labels, counts), levels = labels))
    dat$id <- seq_len(nrow(dat))
    dat$x <- (dat$id - 1L) %% 100L
    dat$y <- (dat$id - 1L) %/% 100L
    ggplot(dat, aes(x, y, colour = type, shape = type)) +
      geom_point(size = .7) +
      scale_y_reverse() +
      coord_equal() +
      scale_colour_manual(values = upwr_cat_n(4)) +
      labs(title = "Każdy punkt to jedna zmiana", x = NULL, y = NULL, colour = "Wynik", shape = "Wynik") +
      theme_upwr() +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
  })
  zoom_plot_server("a3_grid", grid_plot, alt = "Siatka dziesięciu tysięcy zmian z prawdziwymi i fałszywymi alarmami.")
  curve_plot <- reactive({
    prevalence <- seq(.0001, .2, length.out = 300)
    dat <- data.frame(prevalence, posterior = vapply(prevalence, risk_bayes, numeric(1),
      sensitivity = input$a3_curve_sens, false_positive_rate = input$a3_curve_fpr
    ))
    ggplot(dat, aes(prevalence, posterior)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      geom_vline(xintercept = .01, linetype = 2, colour = upwr_reference) +
      labs(title = "Wiarygodność alarmu zależy od częstości awarii", x = "P(awarii)", y = "P(awarii | alarm)") +
      theme_upwr()
  })
  zoom_plot_server("a3_curve", curve_plot, alt = "Rosnąca krzywa wiarygodności alarmu względem częstości bazowej awarii.")
  output$a3_posterior <- renderUI(lc_stat_grid(lc_stat_box("Dla P(awarii)=0,01",
    risk_format_probability(risk_bayes(.01, input$a3_curve_sens, input$a3_curve_fpr)),
    color = upwr_accent
  ), columns = 1))
  output$a3_second <- renderUI({
    p1 <- risk_bayes(input$a3_prev, input$a3_sens, input$a3_fpr)
    independent <- risk_bayes(p1, input$a3_sens, input$a3_fpr)
    adjusted <- (1 - input$a3_dependence) * independent + input$a3_dependence * p1
    lc_stat_grid(lc_stat_box("Po jednym alarmie", risk_format_probability(p1)),
      lc_stat_box("Po dwóch alarmach", risk_format_probability(adjusted), color = upwr_accent),
      columns = 1
    )
  })
  risk_assessment_server("a3", alarm_quiz, input, output)
}
