# Blok 08: Niezawodność systemu -----------------------------------------

system_quiz <- list(questions = list(
  list(question = "Dla dwóch niezależnych gałęzi równoległych system zawodzi, gdy…", choices = c("zawiodą obie gałęzie" = "both", "zawiedzie dowolna jedna" = "one", "zawsze po średnim czasie życia" = "mean"), correct = "both", explanation = "W układzie równoległym sukces wymaga co najmniej jednej działającej gałęzi."),
  list(question = "Niezależne elementy w szeregu mają R=0,9 i 0,8. Jakie jest R systemu?",
    choices = c("0,98" = "a", "0,85" = "b", "0,72" = "c"), correct = "c",
    explanation = "System wymaga obu elementów: 0,9×0,8=0,72."),
  list(question = "Dwie gałęzie mają R=0,9 i wspólne zasilanie o R=0,99. Jakie jest R układu?",
    choices = c("0,81" = "a", "0,9801" = "b", "0,99" = "c"), correct = "b",
    explanation = "Warunkowo bez utraty zasilania redundancja daje 0,99; całość 0,99×0,99."),
  list(question = "φ(x_A,x_B,x_C)=x_A. Czy ten trzyczęściowy model jest koherentny?",
    choices = c("Nie, B i C są nieistotne" = "a", "Tak, bo φ jest niemalejąca" = "b", "Nie, bo φ czasami wynosi zero" = "c"), correct = "a",
    explanation = "Koherentność wymaga monotoniczności i istotności każdego uwzględnionego elementu."),
  list(question = "Czy zwykły wzór na równoległe R wystarcza dla rezerwy uruchamianej po awarii?",
    choices = c("Tak, zawsze są dwa elementy" = "a", "Tak, jeśli mają identyczny MTTF" = "b", "Nie, trzeba określić oczekiwanie i przełączenie" = "c"), correct = "c",
    explanation = "Rezerwa oczekująca ma inny czas pracy i dodatkowy mechanizm przełączenia.")
))
system_exercises <- list(
  list(
    task = "Struktura: zapisz tabelę ośmiu stanów dla φ=x_C·[1−(1−x_A)(1−x_B)]. Sprawdź monotoniczność i istotność każdego elementu; porównaj z φ=x_A, gdy w modelu pozostają także B i C.",
    answer = c(
      "Stany zapisujemy jako (x_A, x_B, x_C). φ = 1 tylko dla (1, 0, 1), (0, 1, 1) i (1, 1, 1); w pozostałych pięciu stanach — (0, 0, 0), (1, 0, 0), (0, 1, 0), (1, 1, 0), (0, 0, 1) — φ = 0.",
      "Monotoniczność: zmiana dowolnego x_i z 0 na 1 nigdy nie zamienia jedynki w zero, bo φ jest iloczynem x_C i funkcji niemalejącej względem x_A i x_B. Istotność: A rozstrzyga przy (x_B, x_C) = (0, 1), bo φ(0, 0, 1) = 0 i φ(1, 0, 1) = 1; B symetrycznie przy (x_A, x_C) = (0, 1); C rozstrzyga przy (x_A, x_B) = (1, 1). Funkcja jest koherentna.",
      "Dla φ = x_A stany B i C nigdy nie zmieniają wyniku, więc są nieistotne i model trzyelementowy nie jest koherentny — albo B i C są w nim zbędne, albo pominięto drogę sukcesu, na której mają znaczenie."
    )
  ),
  list(
    task = "Bananpol: policz R systemu szeregowego dla R1=0,92, R2=0,95 i R3=0,98.",
    answer = "Ze wzoru (8.2): R_s = 0,92 · 0,95 · 0,98 = 0,85652 ≈ 0,857. Prawdopodobieństwo awarii systemu w czasie misji to około 0,143 — więcej niż awaria najsłabszego elementu (0,08). Wynik jest poprawny tylko przy wspólnym czasie misji i niezależności elementów."
  ),
  list(
    task = "Diagnostyka: wskaż, dlaczego wspólne zasilanie podważa zwykły rachunek redundancji.",
    answer = c(
      "Wzór (8.3) mnoży prawdopodobieństwa awarii gałęzi, a to wymaga niezależności. Wspólne zasilanie jest zdarzeniem, które samo wyłącza obie gałęzie naraz, więc awarie gałęzi są dodatnio zależne: P(obie zawodzą) jest większe od iloczynu P(A zawodzi) · P(B zawodzi).",
      "Poprawny rachunek wydziela wspólne zasilanie jako osobny blok szeregowy albo jawne zdarzenie wspólne (wzór 8.10). Przy q = 0,01 i wentylatorach 0,92 i 0,95 ryzyko rośnie z 0,004 do około 0,014, a ponad 70% tego ryzyka pochodzi z jednej wspólnej przyczyny."
    )
  ),
  list(
    task = "Transfer: zapisz logikę sukcesu systemu hamulcowego z dwiema niezależnymi gałęziami i wspólnym sterownikiem.",
    answer = c(
      "Sukces: sterownik S działa oraz działa co najmniej jedna gałąź hamulcowa. Funkcja struktury: φ = x_S · [1 − (1 − x_1)(1 − x_2)]. Niezawodność: R = R_S · [1 − (1 − R_1)(1 − R_2)] — ta sama postać co wzór (8.4).",
      "Na przykład przy R_S = 0,99 i R_1 = R_2 = 0,95 dostajemy 0,99 · (1 − 0,05²) = 0,99 · 0,9975 ≈ 0,988. Sterownik jest wąskim gardłem: jego istotność Birnbauma wynosi 0,9975, a każdej gałęzi tylko 0,99 · 0,05 ≈ 0,05."
    )
  ),
  list(
    task = "Beta-factor: dwa jednakowe wentylatory mają niezawodność 0,92 na 1000 h (model wykładniczy), a udział awarii ze wspólnej przyczyny wynosi β = 0,05. Oblicz R układu równoległego i porównaj z rachunkiem przy pełnej niezależności.",
    answer = c(
      "Niezależnie: 1 − 0,08² = 0,9936, czyli ryzyko 0,0064.",
      "Ze wzoru (8.12): λ = −ln 0,92 / 1000 ≈ 8,34 · 10⁻⁵ na godzinę. Część wspólna: e^(−0,05·λ·1000) ≈ 0,99584; część niezależna: (1 − e^(−0,95·λ·1000))² ≈ 0,00580. R ≈ 0,99584 · (1 − 0,00580) ≈ 0,9901, czyli ryzyko około 0,0099.",
      "Pięcioprocentowy udział wspólnej przyczyny zwiększa ryzyko o około 55% (0,0099 / 0,0064 ≈ 1,55)."
    )
  ),
  list(
    task = "Ile gałęzi: jedna gałąź ma R = 0,8. Ile niezależnych gałęzi równoległych potrzeba do R ≥ 0,999? Co się zmieni, jeśli wszystkie gałęzie dzielą zasilanie o q = 0,01?",
    answer = c(
      "Ze wzoru (8.14): n ≥ ln(0,001) / ln(0,2) ≈ 4,29, więc n = 5. Sprawdzenie: 1 − 0,2⁴ = 0,9984 < 0,999, a 1 − 0,2⁵ = 0,99968.",
      "Przy wspólnym zasilaniu R = 0,99 · [1 − 0,2ⁿ] < 0,99 dla każdego n. Cel 0,999 jest nieosiągalny samą liczbą gałęzi; trzeba poprawić zasilanie albo je zdublować."
    )
  )
)

system_sciaga_widget <- figure_panel(
  label = "Ściąga 8.1",
  title = "Architektury, funkcje struktury i wzory",
  full_width = TRUE,
  tags$table(
    class = "lc-table lc-table-striped lc-table-bordered",
    tags$thead(tags$tr(
      tags$th("Układ"), tags$th("Sukces, gdy…"), tags$th("φ(x)"), tags$th("R systemu (niezależne elementy)")
    )),
    tags$tbody(
      tags$tr(tags$td("Szeregowy"), tags$td("działają wszystkie elementy"), tags$td("x₁x₂⋯xₙ = min xᵢ"), tags$td("∏ Rᵢ — wzór (8.2)")),
      tags$tr(tags$td("Równoległy"), tags$td("działa co najmniej jeden"), tags$td("1 − ∏(1 − xᵢ) = max xᵢ"), tags$td("1 − ∏(1 − Rᵢ) — wzór (8.3)")),
      tags$tr(tags$td("Mieszany C + A/B"), tags$td("C oraz (A lub B)"), tags$td("x_C·[1 − (1 − x_A)(1 − x_B)]"), tags$td("R_C·[1 − (1 − R_A)(1 − R_B)] — wzór (8.4)")),
      tags$tr(tags$td("Wspólna przyczyna"), tags$td("brak zdarzenia wspólnego i sukces układu"), tags$td("x_Z̄ · φ(x)"), tags$td("(1 − q)·R_niez — wzór (8.10)")),
      tags$tr(tags$td("n jednakowych gałęzi"), tags$td("działa co najmniej jedna"), tags$td("max xᵢ"), tags$td("1 − (1 − r)ⁿ — wzór (8.13)"))
    )
  )
)

system_block <- list(id = "system", title = "Niezawodność systemu", chapters = list(
  list(
    id = "intuicja", title = "Logika sukcesu", hook = "Te same części, trzy różne systemy",
    lead = "Niezawodność systemu zależy od logiki sukcesu, nie tylko od listy części.",
    intro = c(
      "Nocna awaria chłodzenia dojrzewalni. Rano trzy osoby podają trzy różne wartości niezawodności instalacji — i każda potrafi obronić swoją liczbę. To nie są trzy odpowiedzi dla tego samego systemu, lecz odpowiedzi dla trzech różnych definicji sukcesu.",
      "Do tej pory każdy element — wentylator, czujnik, zasilanie — miał własną niezawodność R(t). Ten wykład skleja te liczby w jedną: niezawodność systemu. Zaskoczenie polega na tym, że wynik zależy od architektury co najmniej tak mocno jak od jakości części."
    ),
    callout = list(
      label = "Dane Bananpolu",
      text = "Instalacja chłodzenia dojrzewalni: R wentylatora 0,92, R czujnika 0,95, R zasilania 0,98 — wszystkie dla wspólnego czasu misji 1000 h. P utraty wspólnego zasilania: 0,01. Liczby są fikcyjne.",
      color = "uwaga"
    ),
    body = list(
      c(
        "W wykładzie o czasie życia niezawodność R(t) była własnością jednego urządzenia: prawdopodobieństwem, że przetrwa do chwili t. Dojrzewalnia bananów nie jest jednak jednym urządzeniem. Chłodzenie to czujnik temperatury, sterownik, wentylatory i zasilanie, a kierownika zmiany interesuje tylko jedno pytanie: czy przez najbliższe tysiąc godzin w komorze utrzyma się właściwa temperatura.",
        "Zanim przejdziemy do wzorów, zatrzymaj się przy najprostszej sytuacji: dwa elementy, każdy o niezawodności 0,9. Ile wynosi niezawodność systemu? Zaznacz odpowiedź, zanim zaczniesz liczyć."
      ),
      risk_vote_panel("s8_vote", "s8_vote_feedback", "Dwa elementy mają R=0,9. Czy R systemu wynosi 0,81, 0,9 czy 0,99?", c("0,81" = "series", "0,90" = "single", "0,99" = "parallel")),
      "Pytanie było celowo niedookreślone. Nie powiedzieliśmy, czy system potrzebuje obu elementów, jednego konkretnego, czy któregokolwiek z nich. Każda z trzech liczb jest poprawną odpowiedzią na inne pytanie — i właśnie dlatego lista części wraz z ich niezawodnościami nie wystarcza do obliczenia niezawodności systemu.",
      risk_example("8.1", "Trzy logiki sukcesu",
        problem = list(
          "Dwa niezależne elementy E₁ i E₂ mają niezawodność 0,9 w tym samym czasie misji. Oblicz R systemu, gdy:",
          risk_parts(
            "System wymaga obu elementów.",
            "System wymaga tylko E₁, a E₂ jest dla funkcji obojętny.",
            "Wystarczy, że działa którykolwiek z nich."
          )
        ),
        steps = c(
          "Sukces = {E₁ działa} ∩ {E₂ działa}. Z niezależności P = 0,9 · 0,9 = 0,81.",
          "Sukces = {E₁ działa}. P = 0,9; stan E₂ nie ma znaczenia.",
          "Porażka = {E₁ zawodzi} ∩ {E₂ zawodzi}, P(porażki) = 0,1 · 0,1 = 0,01, więc P(sukcesu) = 0,99."
        ),
        steps_type = "a",
        answer = "0,81, 0,90 i 0,99. Te same części dają trzy różne systemy; o wyniku rozstrzyga definicja sukcesu, a nie katalog elementów."
      ),
      c(
        "Różnica między skrajnymi odpowiedziami jest ogromna, jeśli spojrzeć na ryzyko zamiast na niezawodność: w wariancie (a) system zawodzi w 19 misjach na 100, w wariancie (c) w jednej. Dziewiętnastokrotna różnica ryzyka przy identycznych częściach to najlepszy argument, że architektura jest pełnoprawną zmienną decyzyjną.",
        "Wykład prowadzi przez kolejne kroki tego rachunku: definicję sukcesu i czasu misji, układy szeregowe i równoległe, układy mieszane i ich zapis funkcją struktury, niezawodność w czasie, wspólne przyczyny awarii oraz wybór redundancji i elementu do poprawy."
      ),
      risk_check("s8_chk_logika",
        "Ktoś twierdzi, że dwa wentylatory po R = 0,9 dają system o R = 0,99. Jakie założenia przyjął, choć ich nie wypowiedział?",
        c("Że wystarcza jeden wentylator, a awarie są niezależne" = "both", "Że wentylatory są nowe" = "new", "Że oba muszą działać naraz" = "series"),
        correct = "both",
        explanation = "0,99 = 1 − 0,1 · 0,1 wymaga dwóch rzeczy: jeden wentylator wystarcza do funkcji (logika równoległa) i awarie nie mają wspólnej przyczyny (mnożenie prawdopodobieństw awarii).",
        hints = c(new = "Wiek wentylatora jest już zawarty w R = 0,9. Jakie założenie o logice i zależności ukrywa liczba 0,99?", series = "Gdyby oba musiały działać, wynik wynosiłby 0,9 · 0,9 = 0,81.")
      )
    )
  ),
  list(
    id = "definicja", title = "Schemat blokowy", hook = "Najpierw ustal, co znaczy „działa”",
    lead = "Najpierw definiujemy, co system ma zrobić i przez jak długi czas, a potem rysujemy drogi sukcesu.",
    intro = c(
      "„System działa” to zdanie bez treści, dopóki nie powiemy, co dokładnie ma robić: utrzymywać temperaturę poniżej progu? podnieść alarm w ciągu minuty? pracować bez przerwy przez tysiąc godzin? Każda definicja sukcesu wyznacza inny zbiór wymaganych elementów — i inną liczbę na końcu rachunku.",
      "Drugi filar to wspólny czas misji. Niezawodność 0,92 „na tysiąc godzin” i 0,95 „na rok” nie są liczbami z tej samej analizy; ich iloczyn nie znaczy nic. Zanim pomnożysz cokolwiek, sprowadź wszystkie R_i do jednego horyzontu."
    ),
    sections = list(
      list(
        id = "sukces", title = "Niezawodność systemu na czas misji",
        body = list(
          c(
            "Definicja sukcesu jest decyzją, a nie faktem technicznym. W dojrzewalni można przyjąć, że chłodzenie działa, gdy temperatura w komorze nie przekracza 14,5 °C; ale można też wymagać, żeby dodatkowo działał alarm przekroczenia, bo bez niego awaria wyjdzie na jaw dopiero rano. Druga definicja dołącza do systemu czujnik i kanał alarmowy — i obniża niezawodność, bo wymaga więcej.",
            "Drugą decyzją jest horyzont. Z wykładu o czasie życia wiemy, że R(t) maleje z czasem, więc bez t liczba nie ma sensu. Dla systemu obowiązuje ta sama zasada: niezawodność systemu to funkcja czasu, a do jednego rachunku wchodzą tylko niezawodności elementów liczone dla tego samego t."
          ),
          risk_definition("8.1", "Niezawodność systemu", c(
            "Niech T_s oznacza czas od uruchomienia do pierwszej chwili, w której system przestaje realizować zdefiniowaną funkcję. Niezawodność systemu na czas misji t to prawdopodobieństwo, że system realizuje tę funkcję nieprzerwanie przez cały przedział [0, t].",
            "Definicja ma trzy obowiązkowe składniki: opis funkcji (co jest sukcesem), czas misji t oraz warunki pracy, w których podane są niezawodności elementów."
          )),
          risk_formula("R_s(t)=P(T_s>t)", num = "8.1",
            legend = c("T_s" = "czas życia systemu — chwila pierwszej utraty funkcji", "t" = "czas misji, wspólny dla wszystkich elementów", "R_s(t)" = "niezawodność systemu na czas misji t")),
          "Wzór (8.1) jest dosłownie tym samym, co definicja niezawodności elementu z wykładu 07, zastosowanym do czasu życia całego systemu. Cała trudność polega na tym, jak T_s zależy od czasów życia elementów — to wyznacza architektura. Zanim ją opiszemy, zobaczmy, co się dzieje, gdy czasy misji się nie zgadzają.",
          risk_example("8.2", "Dwa horyzonty w jednej karcie",
            problem = "W dokumentacji wentylator ma R = 0,92 na 1000 h, a czujnik R = 0,95 „na rok pracy ciągłej” (8760 h). System wymaga obu elementów. Oblicz R systemu na misję 1000 h, zakładając wykładnicze czasy życia, i porównaj z naiwnym iloczynem 0,92 · 0,95.",
            steps = c(
              "W modelu wykładniczym R(t) = e^(−λt), więc R(t₂) = R(t₁)^(t₂/t₁) — niezawodność na krótszą misję to potęga niezawodności na dłuższą.",
              "Czujnik na 1000 h: 0,95^(1000/8760) = 0,95^0,114 ≈ 0,9942.",
              "System: 0,92 · 0,9942 ≈ 0,915.",
              "Naiwny iloczyn: 0,92 · 0,95 = 0,874 — to niezawodność na misję, której nie ma: wentylator liczony na 1000 h, czujnik na rok."
            ),
            answer = "Około 0,915, a nie 0,874. Pomieszanie horyzontów zawyżyło ryzyko prawie półtora raza (0,126 zamiast 0,085); przy odwrotnym pomieszaniu równie łatwo je zaniżyć. Przeliczenie wymaga założenia o kształcie R(t) — tu wykładniczego."
          ),
          risk_check("s8_chk_misja",
            "Element A ma R = 0,90 na 500 h, element B ma R = 0,90 na 2000 h. Który jest bardziej niezawodny na misję 1000 h?",
            c("B, bo na dłuższym horyzoncie ma tę samą niezawodność" = "b", "A i B są jednakowe, bo oba mają 0,90" = "same", "A, bo jego liczba dotyczy krótszego czasu" = "a"),
            correct = "b",
            explanation = "Dla każdego rozsądnego modelu R(t) maleje z czasem. B osiąga 0,90 dopiero po 2000 h, więc na 1000 h ma R > 0,90, a A po 1000 h ma R < 0,90. Liczby bez horyzontu nie da się porównać.",
            hints = c(same = "Liczba 0,90 dotyczy różnych czasów. Co się dzieje z R(t) elementu A po przekroczeniu 500 h?", a = "Krótszy horyzont przy tej samej wartości R oznacza szybsze starzenie, a nie lepszy element.")
          )
        )
      ),
      list(
        id = "check", title = "Kontrakt modelu",
        text = "Z powyższych rozważań wynika krótka lista warunków, które musi spełnić każda analiza niezawodności systemu, zanim pojawi się pierwsze mnożenie. Warto ją traktować jak kontrakt: jeśli któryś punkt jest niespełniony, liczba na końcu nie ma jasnej interpretacji.",
        bullets = c("jednoznaczna funkcja systemu", "ten sam horyzont dla wszystkich R_i", "stany elementów adekwatne do funkcji", "jawne zależności i wspólne zasoby"),
        body = "Trzeci punkt bywa pomijany. Stan „działa / nie działa” musi odnosić się do funkcji: wentylator, który się kręci, ale daje połowę wydajności, dla definicji „utrzymać 14,5 °C w pełnym załadunku” jest w stanie awarii. Czwarty punkt rozwiniemy w rozdziale o wspólnej przyczynie.",
        pitfall = "Nie wolno mnożyć niezawodności podanych dla różnych czasów misji."
      ),
      list(
        id = "schemat", title = "Schemat blokowy systemu",
        text = c(
          "Zanim pojawią się wzory, narysujmy logikę. Schemat blokowy niezawodności (RBD) czyta się jednym pytaniem: czy da się przejść od lewej do prawej krawędzi wyłącznie przez działające bloki? Bloki ustawione w szereg muszą działać wszystkie; bloki w równoległych gałęziach zastępują się nawzajem.",
          "Przełącz trzy architektury zbudowane z tych samych elementów. Fizycznie nic się nie zmienia — te same urządzenia stoją w tej samej hali. Zmienia się wyłącznie to, które z nich są wymagane naraz, a które mogą się zastąpić."
        ),
        body = list(
          risk_definition("8.2", "Schemat blokowy niezawodności", c(
            "Schemat blokowy niezawodności (RBD, reliability block diagram) to graf, w którym każdy element systemu jest blokiem, a linie łączą wejście z wyjściem schematu. System działa wtedy i tylko wtedy, gdy istnieje droga od wejścia do wyjścia przechodząca wyłącznie przez działające bloki.",
            "Schemat zapisuje logikę sukcesu dla ustalonej funkcji, a nie fizyczne połączenia elektryczne czy przepływ powietrza."
          )),
          risk_try("przełącz kolejno „Szeregowy”, „Równoległy” i „Mieszany”. Dla każdego schematu wskaż, ile bloków musi zawieść, żeby zniknęła ostatnia droga od lewej do prawej."),
          risk_widget_panel("Schemat", "Trzy architektury tych samych elementów", selectInput("s8_diagram", "Układ", c("Szeregowy" = "series", "Równoległy" = "parallel", "Mieszany" = "mixed")), plot_id = "s8_diagram_plot", note = "Blok to element, linia to wymaganie drogi sukcesu; schemat opisuje logikę niezawodności, nie fizyczne połączenia."),
          c(
            "W układzie szeregowym jest jedna droga i przechodzi przez wszystkie bloki, więc wystarczy awaria dowolnego z nich. W układzie równoległym każda gałąź jest osobną drogą; system traci ostatnią drogę dopiero przy awarii obu wentylatorów. W układzie mieszanym sterownik C leży na każdej drodze — jego awaria wystarczy — natomiast wentylatory trzeba „wyłączyć” oba.",
            "Ta liczba — najmniejszy zbiór elementów, których awaria przerywa wszystkie drogi — wróci w wykładzie o drzewie błędów pod nazwą minimalnego zbioru przekrojów. Już teraz widać, że element leżący samotnie na wszystkich drogach jest kandydatem na wąskie gardło systemu."
          )
        ),
        pitfall = "Schemat niezawodnościowy nie musi pokrywać się ze schematem instalacji: dwa fizycznie odległe urządzenia mogą tworzyć jeden szereg logiczny."
      )
    )
  ),
  list(
    id = "szereg", title = "Układ szeregowy i równoległy", hook = "Jeden słaby element psuje wszystko albo nic",
    lead = "Szereg działa tylko wtedy, gdy działają wszystkie elementy; redundancję liczymy przez awarię wszystkich gałęzi.",
    intro = "Czujnik wykrywa przegrzanie, sterownik przetwarza sygnał, wentylator chłodzi. Wystarczy, że zawiedzie jedno ogniwo, a funkcja chłodzenia znika — to definicja układu szeregowego. Zanim pojawi się wzór, sprawdź w przełączniku stanów, które kombinacje utrzymują system przy życiu.",
    sections = list(
      list(
        id = "szereg", title = "Wszystkie muszą działać",
        body = list(
          risk_definition("8.3", "Układ szeregowy", c(
            "Układ n elementów jest szeregowy, jeśli działa wtedy i tylko wtedy, gdy działają wszystkie elementy. Równoważnie: awaria dowolnego elementu jest awarią systemu, a czas życia systemu to T_s = min(T₁, …, Tₙ)."
          )),
          risk_try("odznaczaj po jednym elemencie, potem po dwa. Policz, ile z ośmiu możliwych kombinacji stanów daje „System działa”."),
          figure_panel(label = "Stany", title = "Wszystkie muszą działać", checkboxGroupInput("s8_series_states", "Działające elementy", choices = c("Czujnik" = "sensor", "Sterownik" = "controller", "Wentylator" = "fan"), selected = c("sensor", "controller", "fan")), uiOutput("s8_series_state"), full_width = TRUE),
          "Kombinacja wygrywająca jest dokładnie jedna: wszyscy działają. Skoro system wymaga wszystkich elementów naraz, a awarie są niezależne, prawdopodobieństwa działania mnożą się wzdłuż łańcucha:",
          risk_formula("R_s=\\prod_{i=1}^{n} R_i", num = "8.2",
            legend = c("R_i" = "niezawodność i-tego elementu na wspólny czas misji", "n" = "liczba elementów w szeregu", "R_s" = "niezawodność układu szeregowego")),
          risk_derivation("iloczyn dla szeregu", c(
            "Sukces systemu to zdarzenie „działa E₁ i działa E₂ i … i działa Eₙ”, czyli przecięcie n zdarzeń. Z wykładu 02 wiemy, że dla zdarzeń niezależnych prawdopodobieństwo przecięcia jest iloczynem prawdopodobieństw. Bez niezależności trzeba by mnożyć prawdopodobieństwa warunkowe: P(E₁) · P(E₂ | E₁) · …, i to one niosą informację o wspólnych zasobach."
          ), lines = c("R_s = P(E₁ ∩ E₂ ∩ … ∩ Eₙ)", "    = P(E₁) · P(E₂) · … · P(Eₙ)     (niezależność)", "    = R₁ · R₂ · … · Rₙ")),
          "Iloczyn liczb mniejszych od jedności maleje z każdym czynnikiem, więc długie szeregi są bezlitosne: dziesięć elementów po R = 0,95 daje systemowe R ≈ 0,60. W układzie szeregowym system jest co najwyżej tak niezawodny jak najsłabszy element — a dotkliwość tej straty rośnie z długością łańcucha.",
          risk_example("8.3", "Ile ogniw zmieści się w łańcuchu?",
            problem = "Linia sterowania chłodzeniem składa się z jednakowych, niezależnych modułów, każdy o R = 0,99 na misję 1000 h. System wymaga wszystkich modułów. Ilu modułów można użyć, żeby R systemu nie spadło poniżej 0,90? Porównaj wynik dokładny z przybliżeniem R_s ≈ 1 − Σ(1 − R_i).",
            steps = c(
              "Ze wzoru (8.2): R_s = 0,99ⁿ ≥ 0,90, czyli n ≤ ln 0,90 / ln 0,99 ≈ 10,48.",
              "Sprawdzenie: 0,99¹⁰ ≈ 0,904, a 0,99¹¹ ≈ 0,895 — jedenasty moduł przekracza próg.",
              "Przybliżenie: przy małych prawdopodobieństwach awarii ryzyko szeregu to w przybliżeniu suma ryzyk, 10 · 0,01 = 0,10, czyli R_s ≈ 0,90 — nieco zaniżone względem 0,904, bo pomija sytuacje, w których zawodzą dwa moduły naraz."
            ),
            answer = "Najwyżej 10 modułów. Każdy moduł dokłada około jednego punktu procentowego ryzyka, więc budżet ryzyka szeregu rozdziela się mniej więcej addytywnie między elementy."
          ),
          risk_check("s8_chk_szereg",
            "Do szeregu wentylator (0,92) i czujnik (0,95) dokładamy bardzo dobry przekaźnik o R = 0,999. Co się stanie z R systemu?",
            c("Wzrośnie, bo przekaźnik jest lepszy od pozostałych" = "up", "Spadnie, choć nieznacznie" = "down", "Nie zmieni się" = "same"),
            correct = "down",
            explanation = "Każdy element szeregu mnoży R przez liczbę mniejszą od 1: 0,92 · 0,95 = 0,874, a po dodaniu przekaźnika 0,874 · 0,999 ≈ 0,873. W szeregu nie ma elementów, które „podnoszą średnią”.",
            hints = c(up = "Niezawodności w szeregu się nie uśredniają, tylko mnożą. Pomnóż 0,874 przez 0,999.", same = "Zmiana jest mała, ale czy iloczyn przez 0,999 może pozostać bez zmian?")
          )
        ),
        pitfall = "Iloczyn R_i zakłada niezależność elementów; wspólne zasoby i zależności trzeba dodać do modelu jawnie — wrócimy do tego przy wspólnej przyczynie."
      ),
      list(
        id = "rownolegle", title = "Układ równoległy",
        text = "Dwa wentylatory pracują równocześnie, ale jeden wystarcza do wymaganej wydajności. Zakładamy, że awaria jednego nie zmienia obciążenia ani charakterystyki drugiego. Rezerwa oczekująca na uruchomienie to inny model: trzeba uwzględnić stan podczas oczekiwania i niezawodność przełącznika. System równoległy działa, dopóki działa co najmniej jedna gałąź, więc zawodzi tylko wtedy, gdy zawiodą wszystkie naraz. Przełącz stany gałęzi i znajdź jedyną kombinację, która kładzie system.",
        body = list(
          risk_definition("8.4", "Układ równoległy (redundancja czynna)", c(
            "Układ n elementów jest równoległy, jeśli działa wtedy i tylko wtedy, gdy działa co najmniej jeden element. Wszystkie gałęzie pracują od początku misji (redundancja czynna, gorąca), a czas życia systemu to T_s = max(T₁, …, Tₙ)."
          )),
          figure_panel(label = "Stany", title = "Co najmniej jedna gałąź musi działać", checkboxGroupInput("s8_parallel_states", "Działające wentylatory", choices = c("A" = "a", "B" = "b"), selected = c("a", "b")), uiOutput("s8_parallel_state"), full_width = TRUE),
          "Tym razem przegrywająca kombinacja jest jedna — i to jest zaproszenie do triku z dopełnieniem, znanego z wykładu o wielu próbach: zamiast wielu scenariuszy sukcesu liczymy jeden scenariusz porażki.",
          risk_formula("R_p=1-\\prod_{i=1}^{n}(1-R_i)", num = "8.3",
            legend = c("1-R_i" = "prawdopodobieństwo awarii i-tej gałęzi w czasie misji", "\\prod_{i}(1-R_i)" = "prawdopodobieństwo, że zawiodą wszystkie gałęzie", "R_p" = "niezawodność układu równoległego")),
          "Przy niezależnych gałęziach prawdopodobieństwa awarii się mnożą — dwie gałęzie po R = 0,9 dają awarię systemu 0,1 · 0,1 = 0,01, czyli R = 0,99. Wzór (8.3) jest lustrzanym odbiciem wzoru (8.2): w szeregu mnożymy niezawodności, w układzie równoległym — zawodności. Dlatego dobrze jest zapamiętać regułę „szereg: iloczyn R; równolegle: iloczyn 1 − R”.",
          risk_example("8.4", "Dwa wentylatory w dojrzewalni",
            problem = "Wentylator A ma R = 0,92 na 1000 h, a wentylator B, nowszego typu, R = 0,95. Jeden wentylator wystarcza do utrzymania temperatury. Oblicz R układu dwóch wentylatorów i sprawdź wynik drugą metodą.",
            steps = c(
              "Ze wzoru (8.3): P(obie gałęzie zawodzą) = 0,08 · 0,05 = 0,004, więc R_p = 1 − 0,004 = 0,996.",
              "Sprawdzenie wzorem na sumę zdarzeń: P(A ∪ B) = P(A) + P(B) − P(A ∩ B) = 0,92 + 0,95 − 0,92 · 0,95 = 1,87 − 0,874 = 0,996.",
              "Porównanie: sam wentylator A zawodzi z prawdopodobieństwem 0,08, sam B — 0,05; układ — 0,004."
            ),
            answer = "R = 0,996. Ryzyko spada dwudziestokrotnie względem samego wentylatora A (0,08 → 0,004), o ile awarie wentylatorów są naprawdę niezależne."
          ),
          risk_check("s8_chk_rown",
            "Dlaczego R układu równoległego nie wynosi 0,92 + 0,95 = 1,87?",
            c("Bo suma liczy dwa razy sytuację, w której działają oba wentylatory" = "overlap", "Bo wentylatory są zależne" = "dep", "Bo wzór na sumę działa tylko dla trzech elementów" = "three"),
            correct = "overlap",
            explanation = "Zdarzenia „działa A” i „działa B” nie wykluczają się. Suma P(A) + P(B) liczy dwukrotnie ich część wspólną; po odjęciu P(A ∩ B) = 0,874 otrzymujemy 0,996, tak jak z dopełnienia.",
            hints = c(dep = "Nawet przy pełnej niezależności suma przekracza 1. Czego nie odjęliśmy?", three = "Wzór na sumę dwóch zdarzeń ma trzy składniki. Którego brakuje w 0,92 + 0,95?")
          )
        ),
        pitfall = "Wzór 1−∏(1−R_i) zakłada niezależność awarii gałęzi; wspólna przyczyna potrafi zniweczyć redundancję."
      ),
      list(
        id = "przelacznik", title = "Przełącznik architektury",
        text = "Ten widget to eksperyment kontrolowany: dwa elementy o ustalonych niezawodnościach i jeden przełącznik logiki. Zanim klikniesz, oszacuj: o ile system równoległy będzie lepszy od szeregowego przy R₁ = 0,92 i R₂ = 0,95? Potem sprawdź, jak różnica reaguje na pogorszenie jednego z elementów.",
        body = list(
          risk_try("przy domyślnych suwakach przełącz układ z szeregowego na równoległy. Potem obniż R₂ do 0,60 i znów porównaj oba układy."),
          risk_widget_panel("Architektura", "Te same R, inny system", tagList(selectInput("s8_arch", "Układ", c("Szeregowy" = "series", "Równoległy" = "parallel")), sliderInput("s8_r1", "R₁", .5, 1, .92, .01), sliderInput("s8_r2", "R₂", .5, 1, .95, .01)), "s8_arch_plot", "s8_arch_stats"),
          c(
            "Przy domyślnych wartościach układ szeregowy daje 0,874 — mniej niż którykolwiek element — a równoległy 0,996 — więcej niż którykolwiek element. Słupek systemu zawsze leży poniżej najniższego słupka elementu w szeregu i powyżej najwyższego w układzie równoległym.",
            "Po obniżeniu R₂ do 0,60 szereg spada do 0,92 · 0,60 = 0,552, a układ równoległy tylko do 1 − 0,08 · 0,40 = 0,968. Szereg jest wrażliwy na każdy słaby element, redundancja maskuje słabą gałąź, dopóki druga jest dobra. Ta asymetria zadecyduje w ostatnim rozdziale o tym, który element opłaca się poprawiać."
          )
        ),
        takeaway = "Suwaki się nie zmieniły — zmieniła się tylko logika sukcesu. To najważniejsza obserwacja tego wykładu: lista części nie wyznacza niezawodności, dopóki nie powiemy, które z nich naprawdę muszą działać razem."
      )
    )
  ),
  list(
    id = "mieszany", title = "Funkcja struktury", hook = "Każdy układ da się rozebrać na proste kawałki",
    lead = "Redukujemy najpierw gałęzie równoległe, potem łączymy wynik z elementem szeregowym, a całą logikę sukcesu zapisujemy jedną funkcją.",
    intro = c(
      "Prawdziwe instalacje rzadko są czystym szeregiem albo czystą redundancją. Chłodzenie dojrzewalni to sterownik (wymagany zawsze) i dwa wentylatory (zastępowalne). Takie układy liczy się przez redukcję: zwiń każdą grupę równoległą do jednego zastępczego bloku, a potem pomnóż powstały szereg.",
      "Redukcja działa także w czasie: podstawiając R_i(t) z wykładu o czasie życia, otrzymujemy krzywą niezawodności całego systemu. Zwróć uwagę na jej położenie — system nigdy nie jest lepszy od najsłabszego wymaganego szeregu, a względna przewaga redundancji topnieje z czasem misji."
    ),
    sections = list(
      list(
        id = "redukcja", title = "Redukcja krok po kroku",
        body = list(
          c(
            "Redukcja wykorzystuje prostą własność wzorów (8.2) i (8.3): grupa niezależnych elementów połączonych równolegle zachowuje się jak jeden blok o niezawodności R_p, a grupa połączona szeregowo — jak jeden blok o niezawodności R_s. Schemat zwija się więc od środka, aż zostanie jeden blok. Warunek: grupy nie mogą dzielić elementów, bo wtedy ich zastępcze bloki nie są niezależne."
          ),
          risk_try("klikaj „Pokaż następny krok” i przy każdym kroku zapisz, który fragment schematu z poprzedniego rozdziału został właśnie zastąpiony jednym blokiem."),
          figure_panel(label = "Krok po kroku", title = "Sterownik C oraz wentylatory A/B", actionButton("s8_step", "Pokaż następny krok", class = "lc-btn-primary"), uiOutput("s8_reduction"), full_width = TRUE),
          "Trzy kroki redukcji, które właśnie przeszliśmy, składają się w gotowy wzór całego układu:",
          risk_formula("R_{sys}=R_C\\,[1-(1-R_A)(1-R_B)]", num = "8.4",
            legend = c("R_C" = "niezawodność sterownika (blok szeregowy)", "R_A, R_B" = "niezawodności wentylatorów", "[1-(1-R_A)(1-R_B)]" = "zastępczy blok równoległy wentylatorów")),
          "Nawias kwadratowy to zredukowany blok równoległy wentylatorów; sterownik mnoży go szeregowo.",
          risk_example("8.5", "Chłodzenie ze sterownikiem",
            problem = "Przyjmijmy, że sterownik C ma R = 0,98, a wentylatory — jak w przykładzie 8.4 — 0,92 i 0,95. Oblicz R systemu oraz porównaj je z niezawodnością samego sterownika i z układem, w którym jest tylko wentylator A.",
            steps = c(
              "Krok 1 — blok równoległy: R_AB = 1 − 0,08 · 0,05 = 0,996.",
              "Krok 2 — szereg z C: R_sys = 0,98 · 0,996 = 0,97608.",
              "Wariant bez redundancji: 0,98 · 0,92 = 0,9016.",
              "Ryzyko: 1 − 0,97608 ≈ 0,024 wobec 1 − 0,98 = 0,020 dla samego sterownika."
            ),
            answer = "R_sys ≈ 0,976. Redundancja wentylatorów obniża ryzyko z około 0,098 do 0,024, ale ponad 80% pozostałego ryzyka (0,020 z 0,024) to sterownik — wąskie gardło, którego redundancja nie dotyczy."
          )
        )
      ),
      list(
        id = "czas", title = "Systemowa R(t)",
        body = list(
          c(
            "Dotąd każde R_i było jedną liczbą dla ustalonej misji. Z wykładu 07 wiemy jednak, że R_i(t) to cała funkcja czasu. Wzory (8.2)–(8.4) obowiązują dla każdego t osobno, więc wystarczy podstawić w nich R_i(t), żeby dostać R_s(t) — krzywą niezawodności całego systemu.",
            "Najprostszy przypadek to elementy o wykładniczych czasach życia, R_i(t) = e^(−λᵢt). Dla szeregu iloczyn wykładników daje znowu wykładnik: szereg elementów wykładniczych ma wykładniczy czas życia z intensywnością równą sumie intensywności. Układ równoległy już tej własności nie ma."
          ),
          risk_formula("R_s(t)=e^{-(\\lambda_1+\\cdots+\\lambda_n)t},\\qquad MTTF_s=\\frac{1}{\\lambda_1+\\cdots+\\lambda_n}", num = "8.5",
            legend = c("\\lambda_i" = "intensywność awarii i-tego elementu (1/MTTF_i)", "MTTF_s" = "średni czas do awarii układu szeregowego")),
          risk_formula("R_p(t)=e^{-\\lambda_A t}+e^{-\\lambda_B t}-e^{-(\\lambda_A+\\lambda_B)t}", num = "8.6",
            legend = c("\\lambda_A, \\lambda_B" = "intensywności awarii dwóch gałęzi równoległych")),
          risk_derivation("MTTF układu równoległego", c(
            "Średni czas życia to pole pod krzywą niezawodności: MTTF = ∫₀^∞ R(t) dt (wykład 07). Całkując każdy składnik wzoru (8.6) osobno, dostajemy MTTF_p = 1/λ_A + 1/λ_B − 1/(λ_A + λ_B).",
            "Dla dwóch jednakowych gałęzi (λ_A = λ_B = λ) to 2/λ − 1/(2λ) = 3/(2λ): druga gałąź wydłuża średni czas życia o połowę, a nie dwukrotnie. Hazard układu równoległego nie jest stały — na początku jest bliski zeru, a z czasem rośnie do λ, gdy zostaje jedna gałąź."
          ), lines = c("MTTF_p = ∫ [e^(−λ_A t) + e^(−λ_B t) − e^(−(λ_A+λ_B)t)] dt", "       = 1/λ_A + 1/λ_B − 1/(λ_A + λ_B)", "λ_A = λ_B = λ:  MTTF_p = 3/(2λ)")),
          risk_example("8.6", "R systemu na 1000 h z MTTF elementów",
            problem = "W widgecie poniżej elementy mają wykładnicze czasy życia: wentylator A — MTTF 1800 h, wentylator B — 2000 h, sterownik — 2500 h. Oblicz R systemu mieszanego na misję 1000 h oraz średni czas do awarii systemu.",
            steps = c(
              "R_A(1000) = e^(−1000/1800) ≈ 0,574; R_B(1000) = e^(−0,5) ≈ 0,607; R_C(1000) = e^(−0,4) ≈ 0,670.",
              "Blok równoległy: R_AB = 1 − 0,426 · 0,393 ≈ 0,832.",
              "System ze wzoru (8.4): R_sys ≈ 0,670 · 0,832 ≈ 0,558.",
              "MTTF: całka z R_C(t) · R_AB(t) = 1/(λ_C + λ_A) + 1/(λ_C + λ_B) − 1/(λ_A + λ_B + λ_C) ≈ 1470,6 h."
            ),
            answer = "R_sys(1000) ≈ 0,558, MTTF systemu ≈ 1470 h. System ma niższą niezawodność niż sam wentylator A (0,574), bo sterownik o R = 0,670 jest wymagany na każdej drodze sukcesu. Dla porównania szereg wszystkich trzech elementów miałby MTTF = 1/(1/1800 + 1/2000 + 1/2500) ≈ 687 h."
          ),
          risk_try("ustaw czas misji na 1000 h i odczytaj R systemu. Potem przesuń suwak na 100 h i na 3000 h; za każdym razem porównaj krzywą systemu z krzywą sterownika i wentylatora A."),
          risk_widget_panel("Czas", "Elementy i system mieszany", sliderInput("s8_mission", "Czas misji (h)", 100, 3000, 1000, 50), "s8_time_plot", "s8_time_stats"),
          c(
            "Ta sama redukcja działa w czasie: krzywe elementów i całego systemu muszą używać wspólnego czasu misji, a krzywa systemu zawsze leży poniżej najsłabszego wymaganego szeregu — tutaj poniżej krzywej sterownika. Przy 1000 h panel pokazuje R systemu 0,558.",
            "Warto porównać, ile redundancja wentylatorów zmniejsza ryzyko w zależności od t. Prawdopodobieństwo awarii bloku A/B to prawdopodobieństwo awarii A pomnożone przez 1 − R_B(t). Przy 100 h ten mnożnik wynosi około 0,05 — redundancja zmniejsza ryzyko dwudziestokrotnie. Przy 1000 h mnożnik to około 0,39, a przy 3000 h już około 0,78. W długiej misji druga gałąź sama jest prawdopodobnie zepsuta, zanim będzie potrzebna."
          ),
          risk_check("s8_chk_czas",
            "Dwa elementy o wykładniczych czasach życia z intensywnościami λ₁ i λ₂ połączono szeregowo. Jaki rozkład ma czas życia systemu?",
            c("Wykładniczy z intensywnością λ₁ + λ₂" = "exp", "Wykładniczy z intensywnością λ₁ · λ₂" = "prod", "Nie jest wykładniczy, bo hazard rośnie" = "notexp"),
            correct = "exp",
            explanation = "R_s(t) = e^(−λ₁t) · e^(−λ₂t) = e^(−(λ₁+λ₂)t), wzór (8.5). Intensywności awarii szeregu się sumują. Rosnący hazard ma natomiast układ równoległy.",
            hints = c(prod = "Przy mnożeniu potęg o tej samej podstawie wykładniki się dodają.", notexp = "Rosnący hazard dotyczy układu równoległego. Pomnóż e^(−λ₁t) przez e^(−λ₂t).")
          )
        )
      ),
      list(
        id = "struktura", title = "Stany elementów i stan systemu",
        text = c(
          "Checkboksy z poprzednich sekcji wykonywały w tle prostą matematykę: brały wektor stanów elementów i zwracały stan systemu. Ta operacja ma nazwę — funkcja struktury φ — i zapisuje architekturę bez ani jednego prawdopodobieństwa. Rozdzielenie logiki (φ) od liczb (R_i) to porządek, który za wykład wróci w drzewach błędów.",
          "Stan elementu i zapisujemy jako x_i: 1 gdy działa, 0 gdy zawiódł. Funkcja struktury φ przypisuje wektorowi stanów elementów stan całego systemu. Przełączniki stanów w rozdziale o układach robiły dokładnie to: dla szeregu φ(x)=x₁x₂⋯xₙ, dla układu równoległego φ(x)=1−(1−x₁)⋯(1−xₙ), a dla naszego układu mieszanego φ(x)=x_C·[1−(1−x_A)(1−x_B)]."
        ),
        body = list(
          risk_definition("8.5", "Wektor stanów i funkcja struktury", c(
            "Wektor stanów systemu złożonego z n elementów to x = (x₁, …, xₙ), gdzie xᵢ = 1, gdy i-ty element działa, i xᵢ = 0, gdy zawiódł. Wektorów stanów jest 2ⁿ.",
            "Funkcja struktury to funkcja φ: {0, 1}ⁿ → {0, 1}, która każdemu wektorowi stanów przypisuje stan systemu: φ(x) = 1, gdy system realizuje funkcję, i φ(x) = 0 w przeciwnym razie."
          )),
          risk_formula("\\varphi(x_1,\\ldots,x_n)\\in\\{0,1\\},\\qquad x_i\\in\\{0,1\\}"),
          "Dla stanów zero-jedynkowych iloczyn to minimum, a „jedynka minus iloczyn dopełnień” to maksimum. Pozwala to zapisać dwa podstawowe układy bardzo zwięźle:",
          risk_formula("\\varphi_{szer}(x)=\\prod_{i=1}^{n}x_i=\\min_i x_i,\\qquad \\varphi_{rów}(x)=1-\\prod_{i=1}^{n}(1-x_i)=\\max_i x_i", num = "8.7"),
          c(
            "Teraz dołączamy prawdopodobieństwa. Stany elementów są losowe: Xᵢ = 1 z prawdopodobieństwem Rᵢ. Stan systemu φ(X) jest więc zmienną zero-jedynkową, a jej wartość oczekiwana to prawdopodobieństwo, że wynosi 1. Ta prosta obserwacja łączy logikę z liczbami."
          ),
          risk_formula("R_s=P\\big(\\varphi(X)=1\\big)=E\\big[\\varphi(X)\\big]=\\sum_{x:\\,\\varphi(x)=1}\\;\\prod_{i=1}^{n}R_i^{x_i}(1-R_i)^{1-x_i}", num = "8.8",
            legend = c("X" = "losowy wektor stanów elementów", "R_i^{x_i}(1-R_i)^{1-x_i}" = "prawdopodobieństwo, że element i jest w stanie x_i (przy niezależności)")),
          risk_example("8.7", "Niezawodność z tabeli stanów",
            problem = "Dla układu mieszanego z przykładu 8.5 (R_C = 0,98, R_A = 0,92, R_B = 0,95) wypisz stany, w których φ = 1, i oblicz R_s ze wzoru (8.8). Porównaj z wynikiem redukcji.",
            steps = c(
              "φ(x) = x_C · [1 − (1 − x_A)(1 − x_B)] = 1 dla trzech wektorów (x_A, x_B, x_C): (1, 1, 1), (1, 0, 1), (0, 1, 1).",
              "(1, 1, 1): 0,92 · 0,95 · 0,98 = 0,85652.",
              "(1, 0, 1): 0,92 · 0,05 · 0,98 = 0,04508.",
              "(0, 1, 1): 0,08 · 0,95 · 0,98 = 0,07448.",
              "Suma: 0,85652 + 0,04508 + 0,07448 = 0,97608."
            ),
            answer = "0,97608 — dokładnie tyle co z redukcji w przykładzie 8.5. Tabela stanów jest wolniejsza, ale działa dla każdej struktury, także takiej, której nie da się zredukować do szeregów i równoległości."
          )
        )
      ),
      list(
        id = "koherentnosc", title = "System koherentny",
        text = "Nie każda funkcja zero-jedynkowa opisuje rozsądny system. Wszystkie układy, które dotąd liczyliśmy, mają dwie własności, które inżynier przyjmuje za oczywiste — i które warto nazwać, bo ich naruszenie jest sygnałem błędu w modelu.",
        bullets = c("naprawa elementu nigdy nie pogarsza stanu systemu — φ jest niemalejąca względem każdego x_i", "każdy element jest istotny — istnieje układ stanów pozostałych elementów, w którym jego stan rozstrzyga o wyniku", "wszystkie układy z tego wykładu: szeregowy, równoległy i mieszany, są koherentne"),
        body = list(
          risk_definition("8.6", "System koherentny", c(
            "Dla wektorów stanów piszemy x ≤ y, gdy xᵢ ≤ yᵢ dla każdego i. Przez (1ᵢ, x) i (0ᵢ, x) oznaczamy wektor x, w którym i-tą współrzędną zastąpiono odpowiednio jedynką i zerem.",
            "Funkcja struktury φ jest monotoniczna, jeśli x ≤ y pociąga φ(x) ≤ φ(y). Element i jest istotny, jeśli istnieje wektor x, dla którego φ(1ᵢ, x) = 1 i φ(0ᵢ, x) = 0. System jest koherentny, jeśli jego funkcja struktury jest monotoniczna i wszystkie elementy są istotne."
          )),
          "Z definicji wynika użyteczne oszacowanie. W systemie koherentnym awaria wszystkich elementów oznacza awarię systemu, a działanie wszystkich — działanie systemu. Stąd każdy system koherentny leży między szeregiem a układem równoległym tych samych elementów:",
          risk_formula("\\prod_{i=1}^{n}R_i\\;\\le\\;R_s\\;\\le\\;1-\\prod_{i=1}^{n}(1-R_i)", num = "8.9"),
          risk_derivation("oszacowanie (8.9)", c(
            "Ponieważ każdy element jest istotny, istnieje stan, w którym φ = 1; z monotoniczności φ(1, …, 1) = 1. Analogicznie istnieje stan z φ = 0, więc φ(0, …, 0) = 0. Jeśli wszystkie xᵢ = 1, to φ(x) = 1 = min xᵢ; w pozostałych przypadkach min xᵢ = 0 ≤ φ(x). Jeśli wszystkie xᵢ = 0, to φ(x) = 0 = max xᵢ; w pozostałych przypadkach max xᵢ = 1 ≥ φ(x).",
            "Mamy więc min xᵢ ≤ φ(x) ≤ max xᵢ dla każdego x. Biorąc wartości oczekiwane i korzystając z (8.7)–(8.8), otrzymujemy (8.9). Dla układu z przykładu 8.7: 0,85652 ≤ 0,97608 ≤ 1 − 0,08 · 0,05 · 0,02 = 0,99992."
          )),
          risk_check("s8_chk_koh",
            "System dwóch elementów ma funkcję struktury φ(x_A, x_B) = x_A · (1 − x_B). Czy jest koherentny?",
            c("Nie — naprawa B wyłącza system, więc φ nie jest monotoniczna" = "nonmono", "Tak — oba elementy są istotne" = "yes", "Nie — element A jest nieistotny" = "irrel"),
            correct = "nonmono",
            explanation = "φ(1, 0) = 1, ale φ(1, 1) = 0: przejście B ze stanu awarii do stanu działania psuje system. Oba elementy są istotne, lecz brak monotoniczności wystarcza, by system nie był koherentny. Taki zapis zwykle oznacza, że „stan B” opisuje coś innego niż działanie, np. błędne zadziałanie zabezpieczenia.",
            hints = c(yes = "Istotność to tylko połowa definicji. Porównaj φ(1, 0) i φ(1, 1).", irrel = "Sprawdź, czy przy x_B = 0 stan A zmienia wynik.")
          )
        ),
        pitfall = "Element, którego stan nigdy nie wpływa na φ, łamie koherentność i zwykle sygnalizuje błąd modelu: albo element jest zbędny w schemacie, albo pominęliśmy drogę, na której ma znaczenie."
      )
    )
  ),
  list(
    id = "wspolna", title = "Wspólna przyczyna", hook = "Zapas nie pomoże, gdy padnie zasilanie",
    lead = "Utrata wspólnego zasilania jest osobnym zdarzeniem w architekturze.",
    intro = c(
      "Obietnica z wykładu drugiego zostaje spełniona: wspólne zasilanie wraca w pełnej skali. Dwa wentylatory na papierze dają R = 0,996 — ale oba wpięte są w tę samą rozdzielnicę. Utrata zasilania wyłącza obie gałęzie naraz, więc nie jest szumem w danych, lecz osobnym zdarzeniem, które trzeba dopisać do architektury.",
      "Model jest prosty: z prawdopodobieństwem q pada wspólny zasób i system nie działa niezależnie od stanu gałęzi; z prawdopodobieństwem 1−q obowiązuje zwykły rachunek redundancji. Krzywa poniżej pokazuje, jak szybko nawet małe q zjada obiecany zysk z drugiego wentylatora."
    ),
    sections = list(
      list(
        id = "zdarzenie", title = "Wspólna przyczyna jako zdarzenie",
        body = list(
          c(
            "Wzór (8.3) mnoży prawdopodobieństwa awarii gałęzi, a to wolno zrobić tylko wtedy, gdy awarie są niezależne. W dojrzewalni istnieje co najmniej jeden mechanizm, który tę niezależność łamie: obie gałęzie czerpią prąd z jednej rozdzielnicy. Gdy rozdzielnica pada, oba wentylatory stają w tej samej sekundzie — nie jest to zbieg dwóch awarii, lecz jedna awaria o dwóch skutkach.",
            "Najczystszym sposobem ujęcia takiej zależności jest wydzielenie jej przyczyny jako osobnego zdarzenia. Zamiast mówić „awarie wentylatorów są skorelowane”, mówimy: istnieje zdarzenie Z (utrata zasilania), które wyłącza obie gałęzie; poza nim gałęzie zawodzą niezależnie."
          ),
          risk_definition("8.7", "Awaria ze wspólnej przyczyny", c(
            "Awaria ze wspólnej przyczyny (CCF, common cause failure) to zdarzenie, które w tym samym czasie powoduje utratę funkcji kilku elementów — zwykle elementów redundantnych. Jej źródłem jest wspólny zasób (zasilanie, sterowanie, chłodzenie), wspólne środowisko (pożar, zalanie, wibracje) albo wspólny błąd ludzki lub projektowy (ta sama pomyłka serwisanta, wada serii)."
          )),
          "Jeśli Z zachodzi z prawdopodobieństwem q i poza nim gałęzie są niezależne, to ze wzoru na prawdopodobieństwo całkowite (wykład 02) dostajemy: R = P(Z̄) · R_niez + P(Z) · 0. Drugi składnik znika, bo po utracie zasilania system nie działa niezależnie od stanu gałęzi.",
          risk_formula("R=(1-q)R_{bez\\ wspólnej\\ awarii}", num = "8.10",
            legend = c("q" = "prawdopodobieństwo zdarzenia wspólnego w czasie misji", "R_{bez\\ wspólnej\\ awarii}" = "niezawodność układu obliczona przy niezależnych gałęziach")),
          "W języku schematu blokowego wzór (8.10) to nic innego jak dopisanie bloku „zasilanie” szeregowo przed układem równoległym. Wspólna przyczyna przestaje być ukrytą korelacją, a staje się elementem architektury, który da się policzyć, ocenić i poprawić.",
          risk_example("8.8", "Rozdzielnica w dojrzewalni",
            problem = "Wentylatory 0,92 i 0,95 pracują równolegle, a prawdopodobieństwo utraty wspólnego zasilania w czasie misji wynosi q = 0,01. Oblicz R układu i ustal, jaka część ryzyka pochodzi ze wspólnej przyczyny.",
            steps = c(
              "Bez wspólnej przyczyny: R_niez = 0,996 (przykład 8.4).",
              "Ze wzoru (8.10): R = 0,99 · 0,996 = 0,98604; ryzyko 1 − 0,98604 = 0,01396.",
              "Ryzyko przed uwzględnieniem zasilania: 0,004. Wzrost: 0,01396 / 0,004 ≈ 3,5 raza.",
              "Udział wspólnej przyczyny: 0,01 / 0,01396 ≈ 0,72."
            ),
            answer = "R ≈ 0,986. Około 72% ryzyka to jedno zdarzenie — utrata zasilania — którego redundancja wentylatorów w ogóle nie dotyka. Następny wentylator nic tu nie da; pomoże tylko poprawa lub zdublowanie zasilania."
          ),
          risk_try("zacznij od q = 0 i odczytaj R. Następnie ustaw q = 0,01 (dane Bananpolu) i q = 0,05; porównaj spadek R z ryzykiem 0,004, które obiecywał sam układ równoległy."),
          risk_widget_panel("Zależność", "Wspólne zasilanie", sliderInput("s8_common", "P(utraty wspólnego zasilania)", 0, .15, .01, .005), "s8_common_plot", "s8_common_stats"),
          c(
            "Linia jest prosta, bo R zależy od q liniowo: każdy punkt procentowy q odbiera prawie cały punkt procentowy niezawodności. Przy q = 0,01 panel pokazuje 0,986, przy q = 0,05 R spada do 0,95 · 0,996 ≈ 0,946 — ryzyko 0,054, trzynaście i pół raza większe niż obiecane 0,004. Przy q = 0,15 zostaje około 0,847.",
            "Porównaj to z wkładem wentylatorów: poprawa jednego z nich o kilka punktów procentowych zmienia ryzyko bloku równoległego o tysięczne części. Gdy q jest rzędu ryzyka pojedynczej gałęzi, wspólna przyczyna dominuje całą analizę."
          ),
          risk_check("s8_chk_wsp",
            "Przy q = 0,01 dokładamy trzeci, niezależny wentylator o R = 0,90 (wciąż na tej samej rozdzielnicy). Jak zmieni się ryzyko układu?",
            c("Spadnie około dziesięciokrotnie" = "ten", "Spadnie nieznacznie, bo dominuje wspólne zasilanie" = "slight", "Nie zmieni się wcale" = "none"),
            correct = "slight",
            explanation = "Blok wentylatorów zmieni ryzyko z 0,004 na 0,0004, ale całość to 1 − 0,99 · (1 − 0,0004) ≈ 0,0104 zamiast 0,01396. Ryzyko nie spadnie poniżej q = 0,01, ile by nie było wentylatorów.",
            hints = c(ten = "Dziesięciokrotnie spada tylko ryzyko bloku wentylatorów. Co z członem (1 − q)?", none = "Blok wentylatorów się poprawia, więc ryzyko nieco spada. Ile najwyżej?")
          )
        )
      ),
      list(
        id = "beta", title = "Model beta-factor",
        body = list(
          c(
            "Jawne zdarzenie wspólne wymaga, żeby przyczynę dało się nazwać i oszacować jej q. Często znamy tylko całkowitą intensywność awarii elementu, a z danych branżowych wiemy, jaka część awarii elementów redundantnych zdarza się „parami”. Tę informację wykorzystuje najpopularniejszy model zależności w analizach niezawodności — model beta-factor.",
            "Intensywność awarii każdego elementu λ dzielimy na dwie części: niezależną, dotyczącą tylko tego elementu, i wspólną, która w tej samej chwili wyłącza wszystkie elementy grupy. Parametr β to udział części wspólnej; w praktyce przyjmuje się zwykle wartości od około 0,01 do 0,1."
          ),
          risk_formula("\\lambda=\\lambda_N+\\lambda_W,\\qquad \\lambda_W=\\beta\\lambda,\\qquad \\lambda_N=(1-\\beta)\\lambda", num = "8.11",
            legend = c("\\lambda" = "całkowita intensywność awarii jednego elementu", "\\lambda_N" = "część niezależna", "\\lambda_W" = "część wspólna, wyłączająca całą grupę", "\\beta" = "udział awarii ze wspólnej przyczyny")),
          "Dla dwóch jednakowych gałęzi równoległych wspólna część działa jak blok szeregowy o niezawodności e^(−βλt), a części niezależne tworzą zwykły układ równoległy. Łącząc wzory (8.3) i (8.10):",
          risk_formula("R_p(t)=e^{-\\beta\\lambda t}\\Big[1-\\big(1-e^{-(1-\\beta)\\lambda t}\\big)^{2}\\Big]", num = "8.12"),
          risk_example("8.9", "Ile kosztuje β = 0,1?",
            problem = "Dwa jednakowe wentylatory mają niezawodność 0,92 na 1000 h (model wykładniczy). Oblicz R układu równoległego na 1000 h przy β = 0 i przy β = 0,1.",
            steps = c(
              "λ = −ln 0,92 / 1000 ≈ 8,34 · 10⁻⁵ na godzinę; λt ≈ 0,0834.",
              "β = 0: R = 1 − 0,08² = 0,9936, ryzyko 0,0064.",
              "β = 0,1: część wspólna e^(−0,1 · 0,0834) ≈ 0,9917, czyli ryzyko wspólne ≈ 0,0083; część niezależna gałęzi e^(−0,9 · 0,0834) ≈ 0,9277, więc obie zawodzą niezależnie z prawdopodobieństwem 0,0723² ≈ 0,0052.",
              "Ze wzoru (8.12): R ≈ 0,9917 · (1 − 0,0052) ≈ 0,9865, ryzyko ≈ 0,0135."
            ),
            answer = "Ryzyko rośnie z 0,0064 do 0,0135 — ponad dwukrotnie — choć każdy wentylator z osobna ma nadal R = 0,92. Dziesięcioprocentowy udział awarii wspólnych wystarcza, by to one stały się głównym składnikiem ryzyka układu."
          ),
          "Model beta-factor jest przybliżeniem: zakłada, że zdarzenie wspólne wyłącza zawsze wszystkie gałęzie i że β nie zależy od liczby gałęzi. Jego wartość dydaktyczna jest jednak duża — pokazuje, że niezawodności elementów nie wystarczą do policzenia redundancji; potrzebny jest jeszcze jeden parametr opisujący zależność."
        )
      )
    ),
    pitfall = "Suwak korelacji nie zastępuje opisu mechanizmu wspólnej przyczyny."
  ),
  list(
    id = "redundancja", title = "Istotność Birnbauma", hook = "Kolejny zapas daje coraz mniej",
    lead = "Kolejna gałąź poprawia R, lecz wnosi koszt i coraz mniejszy przyrost; ta sama poprawa elementu ma różną wartość w różnych miejscach architektury.",
    intro = c(
      "Skoro drugi wentylator tak pomaga, czemu nie zamontować czterech? Rachunek odpowiada krzywą nasycenia: pierwsza dodatkowa gałąź redukuje ryzyko dziesięciokrotnie, następna znowu dziesięciokrotnie — ale to już redukcja z 0,01 do 0,001, podczas gdy koszt każdej gałęzi jest taki sam.",
      "W praktyce granicę opłacalności wyznaczają dwa czynniki, których krzywa nie pokazuje: wspólne przyczyny (od pewnego momentu to one dominują ryzyko i kolejne gałęzie nie pomagają wcale) oraz koszty pośrednie — miejsce, obsługa, dodatkowe punkty awarii."
    ),
    sections = list(
      list(
        id = "nasycenie", title = "Malejąca korzyść redundancji",
        body = list(
          "Dla n jednakowych, niezależnych gałęzi o niezawodności r wzór (8.3) przyjmuje prostą postać, a odwracając go, dostajemy liczbę gałęzi potrzebną do osiągnięcia celu R*:",
          risk_formula("R_n=1-(1-r)^{n}", num = "8.13",
            legend = c("r" = "niezawodność jednej gałęzi", "n" = "liczba gałęzi równoległych")),
          risk_formula("n\\ \\ge\\ \\frac{\\ln(1-R^{*})}{\\ln(1-r)}", num = "8.14",
            legend = c("R^{*}" = "docelowa niezawodność układu")),
          "Każda gałąź mnoży ryzyko przez ten sam czynnik 1 − r. Względna korzyść jest więc stała, ale bezwzględna maleje geometrycznie: przy r = 0,9 kolejne gałęzie podnoszą R o 0,09, 0,009, 0,0009 i tak dalej. Jeśli każda gałąź kosztuje tyle samo, koszt jednostki zmniejszonego ryzyka rośnie z każdą gałęzią dziesięciokrotnie.",
          risk_example("8.10", "Cel 0,9999",
            problem = "Gałąź chłodzenia ma r = 0,9. Ile gałęzi potrzeba, żeby układ osiągnął R ≥ 0,9999? Jak zmienia się odpowiedź, jeśli wszystkie gałęzie dzielą zasilanie o q = 0,01?",
            steps = c(
              "Ze wzoru (8.14): n ≥ ln(0,0001) / ln(0,1) = 4.",
              "Kontrola ze wzoru (8.13): n = 1, 2, 3, 4 daje R = 0,9; 0,99; 0,999; 0,9999.",
              "Ze wspólnym zasilaniem (wzór 8.10): R = 0,99 · (1 − 0,1ⁿ), czyli 0,891; 0,9801; 0,98901; 0,989901 — i nigdy nie przekroczy 0,99."
            ),
            answer = "Bez wspólnej przyczyny wystarczą 4 gałęzie. Ze wspólnym zasilaniem cel jest nieosiągalny dla dowolnego n; już trzecia gałąź daje tylko około 0,009, a czwarta niecałą jedną tysięczną."
          ),
          risk_try("przy R jednej gałęzi 0,9 przesuwaj liczbę gałęzi od 1 do 6 i zapisuj przyrost R po każdym kroku. Potem zmień R gałęzi na 0,6 i powtórz."),
          risk_widget_panel("Trade-off", "Liczba gałęzi i koszt", tagList(sliderInput("s8_branches", "Liczba gałęzi", 1, 6, 2, 1), sliderInput("s8_branch_r", "R jednej gałęzi", .5, .99, .9, .01)), "s8_redundancy", "s8_redundancy_stats"),
          c(
            "Przy r = 0,9 krzywa po drugiej gałęzi jest praktycznie pozioma: panel pokazuje R = 0,990 dla dwóch gałęzi i koszt 200 jednostek, a każda następna gałąź dokłada 100 jednostek kosztu za coraz mniej. Przy słabej gałęzi (r = 0,6) krzywa rośnie dłużej, bo pojedyncza gałąź zostawia dużo ryzyka do usunięcia — redundancja najwięcej daje tam, gdzie elementy są słabe.",
            "Widget zakłada pełną niezależność gałęzi. Po lekturze poprzedniego rozdziału wiemy, że realna krzywa ma sufit na poziomie 1 − q: od chwili, gdy ryzyko bloku równoległego spadnie poniżej ryzyka wspólnej przyczyny, dokładanie gałęzi przestaje mieć sens."
          ),
          risk_check("s8_chk_nas",
            "Przy r = 0,9 przejście z 2 do 3 gałęzi podnosi R z 0,99 do 0,999. O ile podniesie je przejście z 3 do 4 gałęzi?",
            c("O 0,0009" = "small", "O 0,009, tyle samo co poprzednio" = "same", "O 0,09" = "big"),
            correct = "small",
            explanation = "Ryzyko spada z 0,001 do 0,0001, więc R rośnie o 0,0009. Każda gałąź zmniejsza ryzyko dziesięciokrotnie, ale bezwzględny przyrost też jest dziesięciokrotnie mniejszy niż poprzednio.",
            hints = c(same = "Stały jest czynnik, przez który mnożymy ryzyko, a nie przyrost R. Policz 1 − 0,1⁴.", big = "0,09 to przyrost przy przejściu z jednej do dwóch gałęzi.")
          )
        ),
        takeaway = "Przy niezależnych, jednakowych gałęziach każda kolejna redukuje stały ułamek pozostałego ryzyka, lecz coraz mniejszą wartość bezwzględną, a koszt rośnie liniowo. Wielkość korzyści zależy jednak od niezawodności gałęzi i od wspólnych przyczyn: zależne zasilanie potrafi odebrać redundancji większość obiecanego zysku."
      ),
      list(
        id = "transfer", title = "Przykład transferowy: kopie zapasowe",
        text = "Ta sama krzywa opisuje kopie zapasowe danych. Druga kopia radykalnie zmniejsza ryzyko utraty, trzecia już umiarkowanie — a wszystkie trzy trzymane w tej samej serwerowni dzielą wspólną przyczynę: pożar, zalanie, ransomware. Reguła 3-2-1 (trzy kopie, dwa nośniki, jedna poza lokalizacją) to inżynieria wspólnych przyczyn, nie mnożenie gałęzi."
      ),
      list(
        id = "poprawa", title = "Który element poprawić?",
        text = c(
          "Budżet pozwala poprawić jeden element o dwie setne niezawodności. Który wybrać? W szeregu niezależnych elementów o dodatnich R identyczny dopuszczalny przyrost bezwzględny daje największy zysk dla najsłabszego elementu. W układzie mieszanym decyduje także położenie gałęzi. Wartość poprawy zależy od miejsca elementu w architekturze: wzmacnianie gałęzi równoległej, którą i tak ktoś zastępuje, daje ułamek tego, co wzmocnienie wąskiego gardła w szeregu.",
          "Porównanie poniżej liczy dokładnie to: systemowy zysk z identycznej poprawy w trzech różnych miejscach. To pierwsza wersja analizy wrażliwości, która w wykładzie o drzewach błędów stanie się rankingiem interwencji."
        ),
        body = list(
          c(
            "Pytanie „o ile wzrośnie R systemu, gdy R_i wzrośnie o Δ?” ma dokładną odpowiedź dzięki jednej obserwacji. Rozważmy dwa scenariusze: element i na pewno działa albo na pewno nie działa. Ze wzoru na prawdopodobieństwo całkowite R_s = R_i · R_s(1ᵢ) + (1 − R_i) · R_s(0ᵢ), gdzie R_s(1ᵢ) i R_s(0ᵢ) to niezawodność systemu w tych dwóch scenariuszach. Przy niezależnych elementach żadna z tych dwóch liczb nie zależy od R_i — R_s jest funkcją liniową R_i."
          ),
          risk_definition("8.8", "Istotność Birnbauma", c(
            "Istotność Birnbauma elementu i to różnica I_B(i) = R_s(1ᵢ) − R_s(0ᵢ): o ile wzrasta niezawodność systemu, gdy element i zmienia się z pewnie zepsutego w pewnie działający. Równoważnie jest to prawdopodobieństwo, że element i jest krytyczny — że pozostałe elementy są w stanie, w którym jego stan rozstrzyga o systemie."
          )),
          risk_formula("I_B(i)=\\frac{\\partial R_s}{\\partial R_i}=R_s(1_i)-R_s(0_i),\\qquad \\Delta R_s=I_B(i)\\,\\Delta R_i", num = "8.15",
            legend = c("R_s(1_i)" = "niezawodność systemu, gdy element i na pewno działa", "R_s(0_i)" = "niezawodność systemu, gdy element i na pewno nie działa", "\\Delta R_i" = "poprawa niezawodności elementu i")),
          "Dla szeregu istotność elementu to iloczyn niezawodności pozostałych elementów, więc największa jest dla elementu najsłabszego — stąd reguła z początku sekcji. Dla gałęzi równoległej istotność zawiera czynnik 1 − R innej gałęzi, bo element jest krytyczny tylko wtedy, gdy jego zastępca już zawiódł.",
          risk_example("8.11", "Wąskie gardło czy gałąź redundantna?",
            problem = list(
              risk_parts(
                "Szereg Bananpolu: wentylator 0,92, czujnik 0,95, zasilanie 0,98. Oblicz zysk z poprawy każdego elementu o 0,02.",
                "Układ mieszany z przykładu 8.5 (R_C = 0,98, R_A = 0,92, R_B = 0,95): oblicz istotności Birnbauma i zysk z poprawy każdego elementu o 0,01."
              )
            ),
            steps = c(
              "I_B(wentylator) = 0,95 · 0,98 = 0,931; I_B(czujnik) = 0,92 · 0,98 = 0,9016; I_B(zasilanie) = 0,92 · 0,95 = 0,874. Zyski przy Δ = 0,02: 0,01862; 0,01803; 0,01748 — różnice są niewielkie, bo wszystkie elementy są dość dobre.",
              "I_B(C) = R_AB = 0,996; I_B(A) = R_C · (1 − R_B) = 0,98 · 0,05 = 0,049; I_B(B) = R_C · (1 − R_A) = 0,98 · 0,08 = 0,0784. Zyski przy Δ = 0,01: sterownik 0,00996; wentylator A 0,00049; wentylator B 0,00078."
            ),
            steps_type = "a",
            answer = "W szeregu najwięcej daje poprawa najsłabszego wentylatora, choć przewaga jest mała. W układzie mieszanym poprawa sterownika daje około dwudziestokrotnie więcej niż poprawa wentylatora A — tej samej wielkości poprawa ma zupełnie inną wartość w zależności od miejsca w architekturze."
          ),
          risk_try("odczytaj trzy zyski w panelu i porównaj je z krokiem (a) przykładu 8.11. Zastanów się, jak zmieniłaby się kolejność, gdyby zasilanie miało R = 0,90."),
          figure_panel(label = "Porównanie", title = "Spadek ryzyka po poprawie R o 0,02", uiOutput("s8_improvement"), full_width = TRUE),
          c(
            "Panel pokazuje zyski 0,019, 0,018 i 0,017 — kolejność zgodna z istotnością Birnbauma, bo każdy zysk to 0,02 · I_B. Gdyby zasilanie spadło do 0,90, jego istotność wynosiłaby wciąż 0,874, ale wentylatora i czujnika spadłyby do 0,95 · 0,90 = 0,855 i 0,92 · 0,90 = 0,828 — najcenniejsza stałaby się poprawa zasilania, czyli nowego najsłabszego ogniwa.",
            "Istotność mówi, ile daje poprawa, ale nie ile kosztuje. Poprawa sterownika z 0,98 do 0,99 może wymagać wymiany na model przemysłowy, a poprawa wentylatora — tylko częstszego serwisu. Dlatego ranking istotności jest punktem wyjścia do decyzji, a nie decyzją."
          ),
          risk_check("s8_chk_poprawa",
            "W układzie mieszanym (C = 0,98, A = 0,92, B = 0,95) możesz poprawić R jednego elementu o 0,01. Który wybór daje największy wzrost R systemu?",
            c("Sterownik C" = "c", "Wentylator A, bo jest najsłabszy" = "a", "Wentylator B" = "b"),
            correct = "c",
            explanation = "I_B(C) = 0,996, a I_B(A) = 0,049 i I_B(B) = 0,0784. Sterownik jest na każdej drodze sukcesu, a wentylator A jest krytyczny tylko wtedy, gdy B już zawiódł.",
            hints = c(a = "Reguła „popraw najsłabszy” działa w czystym szeregu. Kiedy wentylator A rozstrzyga o systemie?", b = "Wentylator B jest krytyczny tylko wtedy, gdy A już zawiódł. Porównaj z elementem, który jest na każdej drodze.")
          )
        ),
        decision = "Porównuj zmianę wyniku systemowego, koszt i wykonalność; nie wybieraj automatycznie najsłabszego elementu."
      )
    )
  ),
  list(
    id = "sciaga", title = "Ściąga i sprawdzenie", hook = "Najpierw architektura, potem rachunek",
    lead = "Funkcja → misja → architektura → zależności → wynik; rachunek ma odzwierciedlać fizyczną architekturę.",
    intro = c(
      "Rachunek systemowy sprowadza się do dwóch wzorów i jednej dyscypliny: iloczyn dla szeregu, dopełnienie iloczynu dla redundancji, i bezwzględny wymóg wspólnego czasu misji oraz jawnych wspólnych przyczyn. Pięć kroków poniżej wystarcza do audytu każdej analizy — własnej i cudzej.",
      "Quiz sprawdza logikę sukcesu i porażki w obu układach; ćwiczenia prowadzą od rachunku szeregowego przez diagnozę wspólnego zasilania po zapis logiki systemu hamulcowego — czyli transfer całego warsztatu poza chłodnię."
    ),
    sections = list(
      list(
        id = "podsumowanie", title = "Podsumowanie",
        text = c(
          "Wykład zaczął się od obserwacji, że te same części mogą tworzyć systemy o bardzo różnej niezawodności. Dlatego rachunek zaczyna się od definicji: funkcji systemu i wspólnego czasu misji (8.1). Logikę sukcesu zapisujemy schematem blokowym, a dwa podstawowe układy mają lustrzane wzory — iloczyn niezawodności dla szeregu (8.2) i iloczyn zawodności dla układu równoległego (8.3). Układy mieszane redukujemy etapami (8.4), a w czasie podstawiamy R_i(t): szereg elementów wykładniczych jest wykładniczy (8.5), układ równoległy już nie (8.6).",
          "Funkcja struktury oddziela logikę od liczb (8.7), a niezawodność systemu jest jej wartością oczekiwaną (8.8). Systemy koherentne — monotoniczne i bez zbędnych elementów — leżą zawsze między szeregiem a układem równoległym (8.9). Wszystkie te wzory zakładają niezależność; wspólną przyczynę dopisujemy do architektury jako jawne zdarzenie (8.10) albo przez model beta-factor (8.11)–(8.12). Nawet małe q lub β potrafi zdominować ryzyko układu redundantnego.",
          "Decyzje projektowe wynikają z dwóch ostatnich narzędzi. Liczba gałęzi (8.13)–(8.14) daje malejącą bezwzględną korzyść i ma sufit wyznaczony przez wspólne przyczyny. Istotność Birnbauma (8.15) mówi, ile daje poprawa elementu w konkretnym miejscu architektury — wąskie gardło szeregowe jest zwykle warte więcej niż gałąź redundantna. Ostateczny wybór łączy ten zysk z kosztem i wykonalnością."
        )
      ),
      list(id = "tabela", title = "Ściąga wzorów", widget = system_sciaga_widget),
      list(id = "lista", title = "Pięć kroków", bullets = c("Zdefiniuj sukces systemu", "Ustal wspólny czas misji", "Zredukuj logikę etapami", "Dodaj jawne wspólne przyczyny", "Sprawdź wrażliwość na interwencje"), widget = risk_assessment_ui("s8", system_quiz, system_exercises)),
      list(id = "most", title = "Co dalej", text = "Opisaliśmy logikę sukcesu: kiedy system działa. Następny wykład odwróci perspektywę i zapyta, jakie kombinacje przyczyn prowadzą do awarii — to ta sama algebra, ale czytana od strony zdarzenia szczytowego.")
    )
  )
))
system_chapters <- risk_block_chapters(system_block)

system_server <- function(input, output, session) {
  v <- reactiveVal(FALSE)
  observeEvent(input$s8_vote_check, v(TRUE))
  output$s8_vote_feedback <- renderUI({
    req(v())
    if (is.null(input$s8_vote)) {
      return(lc_feedback(type = "info", "Najpierw zaznacz jedną z odpowiedzi."))
    }
    lc_feedback(type = "info", tags$strong("Każda odpowiedź może być poprawna:"), " 0,81 dla szeregu, 0,90 dla pojedynczego wymagania i 0,99 dla redundancji równoległej.")
  })
  output$s8_series_state <- renderUI({
    ok <- length(input$s8_series_states) == 3
    lc_feedback(type = if (ok) "ok" else "warning", tags$strong(if (ok) "System działa." else "System nie działa."), " Układ szeregowy wymaga wszystkich elementów.")
  })
  output$s8_parallel_state <- renderUI({
    ok <- length(input$s8_parallel_states) >= 1
    lc_feedback(type = if (ok) "ok" else "warning", tags$strong(if (ok) "System działa." else "System nie działa."), " Wystarcza co najmniej jedna gałąź.")
  })
  diagram_plot <- reactive({
    if (input$s8_diagram == "series") {
      boxes <- data.frame(x = c(2, 4.5, 7), y = 0, w = 1, label = c("Czujnik", "Sterownik", "Wentylator"))
      lines <- data.frame(xs = c(.3, 3, 5.5, 8), xe = c(1, 3.5, 6, 8.7), ys = 0, ye = 0)
      title <- "Układ szeregowy: jedna droga przez wszystkie bloki"
      limits <- list(x = c(0, 9), y = c(-1.5, 1.5))
    } else if (input$s8_diagram == "parallel") {
      boxes <- data.frame(x = 4.5, y = c(.9, -.9), w = 1.4, label = c("Wentylator A", "Wentylator B"))
      lines <- data.frame(
        xs = c(.5, 2, 2, 2, 2, 5.9, 5.9, 7, 7, 7),
        xe = c(2, 2, 2, 3.1, 3.1, 7, 7, 7, 7, 8.5),
        ys = c(0, 0, 0, .9, -.9, .9, -.9, .9, -.9, 0),
        ye = c(0, .9, -.9, .9, -.9, .9, -.9, 0, 0, 0)
      )
      title <- "Układ równoległy: wystarczy jedna droga"
      limits <- list(x = c(0, 9), y = c(-1.8, 1.8))
    } else {
      boxes <- data.frame(
        x = c(1.9, 6.4, 6.4), y = c(0, .9, -.9), w = c(1.2, 1.4, 1.4),
        label = c("Sterownik C", "Wentylator A", "Wentylator B")
      )
      lines <- data.frame(
        xs = c(0, 3.1, 4.2, 4.2, 4.2, 4.2, 7.8, 7.8, 8.6, 8.6, 8.6),
        xe = c(.7, 4.2, 4.2, 4.2, 5, 5, 8.6, 8.6, 8.6, 8.6, 9.5),
        ys = c(0, 0, 0, 0, .9, -.9, .9, -.9, .9, -.9, 0),
        ye = c(0, 0, .9, -.9, .9, -.9, .9, -.9, 0, 0, 0)
      )
      title <- "Układ mieszany: szereg C z redundancją A/B"
      limits <- list(x = c(-.2, 9.7), y = c(-1.8, 1.8))
    }
    ggplot() +
      geom_segment(data = lines, aes(x = xs, xend = xe, y = ys, yend = ye), colour = upwr_reference, linewidth = 1) +
      geom_rect(data = boxes, aes(xmin = x - w, xmax = x + w, ymin = y - .45, ymax = y + .45), fill = upwr_secondary, colour = "white") +
      geom_text(data = boxes, aes(x = x, y = y, label = label), colour = "white", fontface = "bold", size = 3.6) +
      coord_equal(xlim = limits$x, ylim = limits$y) +
      labs(title = title, x = NULL, y = NULL) +
      theme_upwr() +
      theme(
        axis.text = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  zoom_plot_server("s8_diagram_plot", diagram_plot, alt = "Schemat blokowy niezawodności: bloki elementów połączone liniami dróg sukcesu dla wybranej architektury.")
  arch_value <- reactive(if (input$s8_arch == "series") risk_series_reliability(c(input$s8_r1, input$s8_r2)) else risk_parallel_reliability(c(input$s8_r1, input$s8_r2)))
  arch_plot <- reactive({
    dat <- data.frame(element = c("Element 1", "Element 2", "System"), r = c(input$s8_r1, input$s8_r2, arch_value()))
    ggplot(dat, aes(element, r, fill = element)) +
      geom_col(width = .65) +
      coord_cartesian(ylim = c(0, 1)) +
      scale_fill_manual(values = upwr_cat_n(3), guide = "none") +
      labs(title = paste("Architektura", if (input$s8_arch == "series") "szeregowa" else "równoległa"), x = NULL, y = "Niezawodność") +
      theme_upwr()
  })
  zoom_plot_server("s8_arch_plot", arch_plot, alt = "Słupki niezawodności dwóch elementów i systemu dla wybranej architektury.")
  output$s8_arch_stats <- renderUI(lc_stat_grid(lc_stat_box("R systemu", risk_format_probability(arch_value()), color = upwr_accent), columns = 1))
  step <- reactiveVal(0L)
  observeEvent(input$s8_step, step((step() + 1L) %% 3L))
  output$s8_reduction <- renderUI({
    texts <- c("1. Zdefiniuj sukces: C działa oraz A lub B działa.", "2. Zredukuj A/B: R_AB=1−(1−R_A)(1−R_B).", "3. Połącz szeregowo: R_sys=R_C·R_AB.")
    lc_feedback(type = "info", texts[[step() + 1L]])
  })
  time_plot <- reactive({
    t <- seq(0, 3000, length.out = 400)
    ra <- exp(-t / 1800)
    rb <- exp(-t / 2000)
    rc <- exp(-t / 2500)
    rs <- rc * (1 - (1 - ra) * (1 - rb))
    dat <- rbind(data.frame(t, r = ra, name = "Wentylator A"), data.frame(t, r = rb, name = "Wentylator B"), data.frame(t, r = rc, name = "Sterownik"), data.frame(t, r = rs, name = "System"))
    ggplot(dat, aes(t, r, colour = name)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = input$s8_mission, linetype = 2) +
      scale_colour_manual(values = upwr_cat_n(4)) +
      labs(title = "Wspólny czas dla elementów i systemu", x = "Czas (h)", y = "R(t)", colour = NULL) +
      theme_upwr()
  })
  zoom_plot_server("s8_time_plot", time_plot, alt = "Krzywe niezawodności trzech elementów i systemu mieszanego.")
  output$s8_time_stats <- renderUI({
    t <- input$s8_mission
    rs <- exp(-t / 2500) * (1 - (1 - exp(-t / 1800)) * (1 - exp(-t / 2000)))
    lc_stat_grid(lc_stat_box("R systemu", risk_format_probability(rs), color = upwr_accent), columns = 1)
  })
  common_plot <- reactive({
    q <- seq(0, .15, length.out = 200)
    base <- risk_parallel_reliability(c(.92, .95))
    ggplot(data.frame(q, r = (1 - q) * base), aes(q, r)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      geom_point(data = data.frame(q = input$s8_common, r = (1 - input$s8_common) * base), colour = upwr_secondary, size = 3) +
      labs(title = "Wspólna przyczyna ogranicza redundancję", x = "P(wspólnej awarii)", y = "R systemu") +
      theme_upwr()
  })
  zoom_plot_server("s8_common_plot", common_plot, alt = "Malejąca niezawodność układu redundantnego wraz ze wzrostem wspólnej przyczyny.")
  output$s8_common_stats <- renderUI(lc_stat_grid(lc_stat_box("R z przyczyną wspólną", risk_format_probability(risk_common_cause_reliability(risk_parallel_reliability(c(.92, .95)), input$s8_common)), color = upwr_accent), columns = 1))
  redundancy_plot <- reactive({
    n <- 1:6
    r <- 1 - (1 - input$s8_branch_r)^n
    dat <- data.frame(n, r, cost = n * 100)
    ggplot(dat, aes(n, r)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      geom_point() +
      geom_point(data = dat[dat$n == input$s8_branches, ], colour = upwr_secondary, size = 4) +
      labs(title = "Przyrost niezawodności maleje", x = "Liczba gałęzi", y = "R systemu") +
      theme_upwr()
  })
  zoom_plot_server("s8_redundancy", redundancy_plot, alt = "Krzywa niezawodności równoległej względem liczby gałęzi.")
  output$s8_redundancy_stats <- renderUI({
    r <- 1 - (1 - input$s8_branch_r)^input$s8_branches
    lc_stat_grid(lc_stat_box("R", risk_format_probability(r), color = upwr_accent), lc_stat_box("Koszt demonstracyjny", paste(input$s8_branches * 100, "jedn.")), columns = 1)
  })
  output$s8_improvement <- renderUI({
    base <- c(.92, .95, .98)
    sys0 <- risk_series_reliability(base)
    gains <- vapply(seq_along(base), function(i) {
      x <- base
      x[i] <- min(1, x[i] + .02)
      risk_series_reliability(x) - sys0
    }, numeric(1))
    lc_stat_grid(lc_stat_box("Wentylator", risk_format_probability(gains[1])), lc_stat_box("Czujnik", risk_format_probability(gains[2])), lc_stat_box("Zasilanie", risk_format_probability(gains[3])), columns = 1)
  })
  risk_assessment_server("s8", system_quiz, input, output)
}
