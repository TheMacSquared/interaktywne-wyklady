# Blok 07: Czas życia elementu ------------------------------------------

zycie_quiz <- list(questions = list(
  list(question = "Co oznacza stały hazard rozkładu wykładniczego?", choices = c("Chwilowe tempo awarii nie zależy od wieku działającego elementu" = "constant", "Każdy element żyje dokładnie tyle samo" = "same", "Ryzyko awarii zawsze rośnie" = "grow"), correct = "constant", explanation = "Brak pamięci dotyczy warunkowego ryzyka dalszego życia, nie identycznych czasów awarii."),
  list(question = "MTTF=1500 h w modelu wykładniczym. Ile wynosi R(1000)?",
    choices = c("Około 0,667" = "a", "1500" = "b", "Około 0,513" = "c"), correct = "c",
    explanation = "R(1000)=exp(−1000/1500)."),
  list(question = "Element działa na końcu obserwacji po 1200 h. Co zapisujemy?",
    choices = c("Usuwamy element z danych" = "a", "Czas 1200 h i znacznik cenzorowania" = "b", "Awarię dokładnie po 1200 h" = "c"), correct = "b",
    explanation = "Wiemy, że T>1200 h; nie znamy dokładnego czasu przyszłej awarii."),
  list(question = "Hazard wynosi 0,002 na godzinę. Co przybliża 0,002×0,1?",
    choices = c("Szansę awarii w najbliższej 0,1 h wśród działających" = "a", "Szansę awarii od uruchomienia do teraz" = "b", "Niezawodność 0,1 h każdego modelu" = "c"), correct = "a",
    explanation = "Dla małego Δt hazard razy Δt przybliża warunkowe prawdopodobieństwo zdarzenia."),
  list(question = "Co opisuje czas do trzeciego zdarzenia jednorodnego procesu Poissona?",
    choices = c("Zawsze Weibull z β=3" = "a", "Rozkład dwumianowy" = "b", "Erlang, czyli gamma z k=3" = "c"), correct = "c",
    explanation = "Sumujemy trzy niezależne wykładnicze czasy o tej samej intensywności.")
))
zycie_exercises <- list(
  list(
    task = "Bananpol: dla MTTF=1500 h policz R(1000) w modelu wykładniczym.",
    answer = c(
      "Ze wzoru (7.8): λ = 1/MTTF = 1/1500 na godzinę, więc R(1000) = e^(−1000/1500) = e^(−0,667) ≈ 0,513.",
      "Mimo że średni czas życia wynosi 1500 h, około 49% wentylatorów zawiedzie przed upływem 1000 h. W modelu wykładniczym przed upływem samego MTTF psuje się 1 − e⁻¹ ≈ 63% egzemplarzy."
    )
  ),
  list(
    task = "Diagnostyka: wskaż, dlaczego widoczne tylko zakończone awarie zaniżają oszacowany czas życia.",
    answer = c(
      "Do zbioru zakończonych awarii trafiają wyłącznie czasy krótsze od długości badania — długie życia są z konstrukcji ucięte. Średnia z takiego zbioru jest średnią warunkową E(T | T ≤ c), a ta zawsze leży poniżej E(T).",
      "W danych z rozdziału o cenzorowaniu po 1200 h naiwna średnia wynosi 612,5 h, podczas gdy średnia wszystkich ośmiu czasów to 1368,75 h. Egzemplarze działające wnoszą informację T > 1200 h; wzór (7.2) wlicza ich czas pracy do mianownika łącznego czasu obserwacji."
    )
  ),
  list(
    task = "Transfer: wybierz sensowny kształt Weibulla dla elementu zużywającego się i uzasadnij znak zmiany hazardu.",
    answer = c(
      "Zużycie oznacza hazard rosnący, więc β > 1 — typowo od około 2 do 4 dla łożysk, pasków czy uszczelek. Ze wzoru (7.13) hazard jest proporcjonalny do t^(β−1); dodatni wykładnik β − 1 sprawia, że h(t) rośnie z wiekiem.",
      "Przy β = 2 hazard rośnie liniowo: dwa razy starszy element ma dwa razy większe chwilowe tempo awarii. Wybór β jest hipotezą o mechanizmie, którą trzeba potwierdzić danymi i rozmową z utrzymaniem ruchu."
    )
  ),
  list(
    task = "Plan wymiany: wentylator ma rozkład Weibulla z β = 2 i η = 1700 h. (a) Po ilu godzinach niezawodność spada do 0,80? (b) Wentylator przepracował już 800 h bez awarii. Jakie jest prawdopodobieństwo, że przetrwa kolejne 300 h? Porównaj z nowym wentylatorem.",
    answer = c(
      "(a) Ze wzoru (7.16): t = 1700 · (−ln 0,80)^(1/2) = 1700 · √0,2231 ≈ 1700 · 0,472 ≈ 803 h.",
      "(b) Ze wzoru (7.17): R(1100)/R(800) = exp[−(1100/1700)² + (800/1700)²] ≈ 0,821. Nowy wentylator przetrwa 300 h z prawdopodobieństwem R(300) = exp[−(300/1700)²] ≈ 0,969. Przy hazardzie rosnącym wiek ma znaczenie: używany egzemplarz jest wyraźnie bardziej ryzykowny."
    )
  ),
  list(
    task = "Cenzorowanie w liczbach: badano 10 wentylatorów przez 1000 h. Trzy zawiodły po 200, 500 i 900 h, siedem działało do końca badania. Oszacuj MTTF wzorem (7.2) przy założeniu modelu wykładniczego i porównaj z naiwną średnią z samych awarii.",
    answer = c(
      "Łączny czas obserwacji: 200 + 500 + 900 + 7 · 1000 = 8600 h. Liczba awarii: d = 3. Oszacowanie: MTTF ≈ 8600/3 ≈ 2867 h.",
      "Naiwna średnia z trzech awarii: (200 + 500 + 900)/3 ≈ 533 h — ponad pięciokrotnie mniej. Przy zaledwie trzech awariach oszacowanie jest bardzo niepewne i zależy od założenia stałego hazardu, ale nie jest systematycznie zaniżone przez pominięcie działających egzemplarzy."
    )
  )
)

zycie_functions_table <- figure_panel(
  label = "Słownik",
  title = "Cztery funkcje, cztery pytania",
  full_width = TRUE,
  tags$table(
    class = "lc-table lc-table-striped lc-table-bordered",
    tags$thead(tags$tr(
      tags$th("Funkcja"), tags$th("Definicja"), tags$th("Pytanie inspektora")
    )),
    tags$tbody(
      tags$tr(tags$td("Gęstość f(t)"), tags$td("rozkład momentów awarii"), tags$td("Kiedy awarie są najgęstsze?")),
      tags$tr(tags$td("Dystrybuanta F(t)"), tags$td("P(T ≤ t)"), tags$td("Jaka część elementów zawiedzie do chwili t?")),
      tags$tr(tags$td("Niezawodność R(t)"), tags$td("P(T > t) = 1 − F(t)"), tags$td("Jaka część dotrwa poza t?")),
      tags$tr(tags$td("Hazard h(t)"), tags$td("f(t) / R(t)"), tags$td("Jak ryzykowna jest najbliższa chwila dla elementu, który wciąż działa?"))
    )
  )
)

zycie_block <- list(id = "zycie", title = "Czas życia elementu", chapters = list(
  list(
    id = "mttf", title = "Dwa urządzenia z tym samym MTTF",
    lead = "Ta sama średnia nie gwarantuje tej samej niezawodności w czasie misji.",
    intro = c(
      "Dwa wentylatory z kart katalogowych mają identyczny średni czas życia. Po roku eksploatacji jeden park maszyn wygląda wyraźnie lepiej od drugiego. Średnia nie mówi, jak awarie rozkładają się w czasie — a właśnie od tego zależy, czy element dotrwa do końca misji.",
      "Czas do awarii jest zmienną ciągłą, więc cały warsztat poprzedniego wykładu — gęstość, pola, ogony — działa również tutaj. Dochodzi jedno nowe pojęcie, które zmieni sposób myślenia o starzeniu: hazard, czyli chwilowe tempo awarii elementu, który wciąż działa."
    ),
    callout = list(
      label = "Dane Bananpolu",
      text = "Wentylatory dojrzewalni: model wykładniczy o MTTF 1500 h oraz model Weibulla o kształcie β = 2 i skali η = 1700 h. Jednostka: godzina pracy; horyzont: czas do awarii wentylatora. Liczby są fikcyjne.",
      color = "uwaga"
    ),
    sections = list(list(
      id = "srednia", title = "Czym jest MTTF",
      text = "MTTF (mean time to failure) to wartość oczekiwana czasu życia E(T) — jedna liczba z całego rozkładu, dokładnie tak jak E(X) = np w wykładzie o partiach. Dwa rozkłady o tej samej średniej mogą mieć zupełnie różne udziały wczesnych awarii, a dla planu utrzymania liczą się właśnie one.",
      body = list(
        c(
          "W dojrzewalni Bananpolu wentylator pracuje bez przerwy, a jego awaria przerywa kontrolę temperatury w komorze. Dział zakupów porównuje dwie oferty. Obie karty katalogowe podają ten sam MTTF, około 1500 godzin. Kierownik utrzymania ruchu pyta jednak o coś innego: jaka część wentylatorów dotrwa do planowego przeglądu po 1000 godzinach? Na to pytanie średnia nie odpowiada, bo nie mówi, jak awarie rozkładają się wokół niej.",
          "Żeby odpowiedzieć, musimy nazwać wielkość losową, o której mówimy. W wykładzie 05 liczyliśmy dyskretne próby do zdarzenia; tu zegar biegnie w sposób ciągły, a zdarzeniem jest awaria."
        ),
        risk_definition("7.1", "Czas życia", c(
          "Czas życia T to nieujemna zmienna losowa mierząca czas od ustalonego początku (uruchomienia, montażu, ostatniej wymiany) do pierwszej awarii elementu. Definicja wymaga trzech ustaleń: co rozpoczyna zegar, w jakiej jednostce go mierzymy (godziny pracy, cykle, kilometry) i co uznajemy za awarię."
        )),
        risk_definition("7.2", "MTTF", c(
          "Średni czas do awarii (MTTF, mean time to failure) to wartość oczekiwana czasu życia: MTTF = E(T). Dla zmiennej ciągłej o gęstości f(t) oblicza się ją wzorem (7.1)."
        )),
        risk_formula("MTTF=E(T)=\\int_{0}^{\\infty} t\\,f(t)\\,dt", num = "7.1",
          legend = c("T" = "czas życia elementu", "f(t)" = "gęstość czasu życia", "t" = "czas pracy (h)")),
        "Wzór (7.1) jest ciągłą wersją średniej ważonej: każdy możliwy moment awarii t mnożymy przez jego „wagę” f(t) dt i sumujemy. Wynik jest jedną liczbą. Dwie gęstości o zupełnie różnych kształtach mogą dać tę samą całkę — tak jak dwie klasy o różnym rozkładzie ocen mogą mieć tę samą średnią. Zanim policzymy, dlaczego tak jest, zagłosuj.",
        risk_vote_panel("c7_vote", "c7_vote_feedback", "Czy ten sam MTTF oznacza takie samo R(1000 h)?", c("Tak" = "yes", "Nie — znaczenie ma cały rozkład" = "distribution", "Tylko dla Weibulla" = "weibull")),
        risk_example("7.1", "Dwie oferty, jedna średnia",
          problem = c(
            "Oferta A: czas życia ma rozkład wykładniczy z MTTF = 1500 h, czyli R(t) = e^(−t/1500). Oferta B: rozkład Weibulla z β = 2 i η = 1700 h, czyli R(t) = exp[−(t/1700)²]; jego MTTF wynosi około 1507 h.",
            "Oblicz dla obu ofert prawdopodobieństwo, że wentylator przepracuje bez awarii 500 h, 1000 h i 3000 h. Wzory na R(t) wyprowadzimy w dalszych rozdziałach; tu potraktuj je jako dane z kart katalogowych."
          ),
          steps = c(
            "Oferta A: R(500) = e^(−1/3) ≈ 0,717; R(1000) = e^(−2/3) ≈ 0,513; R(3000) = e^(−2) ≈ 0,135.",
            "Oferta B: R(500) = exp[−(500/1700)²] = exp(−0,0865) ≈ 0,917; R(1000) = exp(−0,346) ≈ 0,707; R(3000) = exp(−3,114) ≈ 0,044.",
            "Do 1000 h oferta B jest wyraźnie lepsza: przetrwa ją 71% zamiast 51% wentylatorów. Przy 3000 h kolejność się odwraca: B przetrwa 4%, A — 14%."
          ),
          answer = "Przy niemal równym MTTF niezawodność w horyzoncie 1000 h różni się o 19 punktów procentowych, a w horyzoncie 3000 h przewaga przechodzi na drugą ofertę. O wyborze decyduje czas misji, nie średnia."
        ),
        "Skąd bierze się ta różnica? Oferta A psuje się „równomiernie w czasie”: część egzemplarzy zawodzi bardzo wcześnie, a część żyje kilka razy dłużej niż średnia. Oferta B się zużywa: wczesne awarie są rzadkie, ale awarie skupiają się wokół 1000–2500 godzin. Obie średnie wychodzą podobnie, bo w ofercie A krótkie życia równoważą się bardzo długimi.",
        risk_check("c7_chk_mttf",
          "Dwa typy wentylatorów mają MTTF = 1500 h. Które stwierdzenie jest uprawnione bez znajomości rozkładów?",
          c("Oba mają taką samą niezawodność po 1000 h" = "same_r", "W obu połowa egzemplarzy psuje się przed 1500 h" = "half", "Średnio żyją tyle samo, ale R(t) w konkretnym horyzoncie może się różnić" = "mean_only"),
          correct = "mean_only",
          explanation = "MTTF to tylko wartość oczekiwana. Przykład 7.1 pokazuje dwa rozkłady o niemal tej samej średniej i R(1000) równym 0,513 oraz 0,707.",
          hints = c(same_r = "Porównaj wartości R(1000) w przykładzie 7.1.", half = "Połowa egzemplarzy psuje się przed medianą, nie przed średnią. W modelu wykładniczym przed upływem MTTF psuje się aż około 63% egzemplarzy.")
        )
      )
    ))
  ),
  list(
    id = "cenzorowanie", title = "Oś obserwacji i cenzorowanie",
    lead = "Element nadal działający na końcu badania wnosi informację: jego czas życia jest co najmniej tak długi.",
    intro = c(
      "Badanie trwałości wentylatorów zakończyło się po 1200 godzinach, a spora część egzemplarzy wciąż działała. Usunięcie ich z danych jest poważnym błędem: ich czas życia nie jest całkiem nieznany — wiemy, że przekroczył moment zakończenia obserwacji, i ta informacja musi zostać w analizie.",
      "Przesuń koniec obserwacji na osi czasu i zobacz, jak zmienia się bilans awarii i obserwacji uciętych. Gdyby policzyć „średni czas życia” wyłącznie z zakończonych awarii, każdy wcześniejszy koniec badania dawałby krótszy wynik — nie dlatego, że wentylatory są gorsze, lecz dlatego, że najdłużej żyjące egzemplarze jeszcze nie zdążyły się zepsuć."
    ),
    body = list(
      c(
        "Dane o czasie życia rzadko przychodzą kompletne. Badanie ma budżet i termin, egzemplarze montuje się w różnych tygodniach, część z nich zostaje zdemontowana z przyczyn niezwiązanych z awarią. W każdym z tych przypadków dla części elementów znamy nie czas życia, lecz tylko dolne ograniczenie: „działał co najmniej tyle”. Taką obserwację nazywamy cenzorowaną.",
        "W badaniu Bananpolu osiem wentylatorów ma rzeczywiste czasy życia 220, 480, 760, 990, 1350, 1750, 2300 i 3100 h. W prawdziwym badaniu tych liczb nie znamy — widzimy je tylko do końca obserwacji. Symulacja pozwala nam spojrzeć na nie z góry i porównać, co zobaczyłby analityk przy różnych terminach zakończenia badania."
      ),
      risk_definition("7.3", "Obserwacja cenzorowana prawostronnie", c(
        "Obserwacja czasu życia jest cenzorowana prawostronnie w chwili c, jeśli do chwili c element nie uległ awarii, a potem przestaliśmy go obserwować. Zapisujemy wtedy parę (c, cenzorowana) i wiemy tylko, że T > c. Obserwację zakończoną awarią zapisujemy jako (t, awaria)."
      ))
    ),
    sections = list(
      list(
        id = "os-czasu", title = "Co widzi analityk",
        body = list(
          risk_try("ustaw koniec obserwacji na 1200 h i policz awarie oraz obserwacje ucięte. Potem przesuń suwak do 800 h i do 2500 h. Dla każdego ustawienia zapisz, które czasy byłyby widoczne jako awarie."),
          risk_widget_panel("Oś czasu", "Awarie i obserwacje ucięte", sliderInput("c7_follow", "Koniec obserwacji (h)", 300, 2500, 1200, 50), "c7_timeline", "c7_timeline_stats"),
          c(
            "Przy końcu obserwacji 1200 h widzimy cztery awarie (220, 480, 760, 990 h) i cztery obserwacje ucięte. Średnia z samych awarii wynosi 612,5 h. Przy 800 h awarie są trzy, a ich średnia spada do około 487 h; przy 2500 h awarii jest siedem, a średnia rośnie do około 1121 h. Tymczasem średnia wszystkich ośmiu rzeczywistych czasów to 1368,75 h.",
            "Naiwna średnia rośnie wraz z długością badania, choć wentylatory się nie zmieniają. Powód jest prosty: dłuższe badanie dopuszcza do zbioru awarii coraz dłuższe czasy życia. Średnia z zakończonych awarii jest w istocie średnią warunkową — liczoną tylko wśród egzemplarzy, które zdążyły się zepsuć — i zawsze leży poniżej prawdziwego MTTF."
          )
        )
      ),
      list(
        id = "szacunek", title = "Jak wykorzystać obserwacje ucięte",
        body = list(
          c(
            "Jeżeli przyjmiemy model wykładniczy, informację „co najmniej c godzin” da się wykorzystać bardzo prosto. W tym modelu hazard jest stały, więc każda godzina pracy — zakończona awarią czy nie — jest jednakowo „narażona”. Naturalny szacunek intensywności awarii to liczba awarii podzielona przez łączny czas pracy wszystkich egzemplarzy, także tych, które przetrwały badanie."
          ),
          risk_formula("\\hat\\lambda=\\frac{d}{\\sum_{i=1}^{n} t_i},\\qquad \\widehat{MTTF}=\\frac{\\sum_{i=1}^{n} t_i}{d}", num = "7.2",
            legend = c("d" = "liczba zaobserwowanych awarii", "t_i" = "czas obserwacji i-tego egzemplarza: do awarii albo do końca badania", "n" = "liczba badanych egzemplarzy", "\\hat\\lambda" = "oszacowana intensywność awarii (1/h)")),
          risk_example("7.2", "Badanie przerwane po 1200 h",
            problem = "Badanie ośmiu wentylatorów przerwano po 1200 h. Awarie wystąpiły po 220, 480, 760 i 990 h; cztery egzemplarze działały do końca. Oszacuj MTTF wzorem (7.2) i porównaj z naiwną średnią z samych awarii.",
            steps = c(
              "Czas pracy egzemplarzy z awarią: 220 + 480 + 760 + 990 = 2450 h.",
              "Czas pracy egzemplarzy cenzorowanych: 4 · 1200 = 4800 h. Łącznie Σ tᵢ = 7250 h.",
              "Liczba awarii d = 4, więc λ̂ = 4/7250 ≈ 0,00055 na godzinę, a MTTF ≈ 7250/4 = 1812,5 h.",
              "Naiwna średnia z samych awarii: 2450/4 = 612,5 h."
            ),
            answer = "Szacunek z wykorzystaniem cenzorowania to około 1813 h, naiwna średnia — 612,5 h. Szacunek (7.2) nie trafia dokładnie w średnią ośmiu rzeczywistych czasów (1368,75 h): opiera się na czterech awariach i na założeniu stałego hazardu. W przeciwieństwie do naiwnej średniej nie jest jednak zaniżony z samej konstrukcji."
          ),
          "Wzór (7.2) jest najprostszym przykładem ogólnej zasady: obserwacja cenzorowana wchodzi do analizy z informacją, którą rzeczywiście niesie. W modelach innych niż wykładniczy robi się to przez funkcję wiarygodności albo estymator Kaplana–Meiera; na tym kursie nie estymujemy parametrów, ale musimy umieć rozpoznać, kiedy ktoś zrobił to źle.",
          risk_check("c7_chk_cenzor",
            "Dlaczego średnia z samych zakończonych awarii rośnie, gdy przedłużamy badanie tych samych wentylatorów?",
            c("Bo wentylatory z czasem pracują coraz lepiej" = "better", "Bo do zbioru awarii dochodzą dłuższe czasy życia, wcześniej ucięte" = "longer", "To efekt przypadku, który zniknie przy większej próbie" = "chance"),
            correct = "longer",
            explanation = "Krótkie badanie widzi tylko krótkie życia. Każde przedłużenie dopuszcza dłuższe czasy, więc średnia z awarii rośnie, choć rozkład czasu życia się nie zmienia. Ten efekt jest systematyczny, a nie losowy.",
            hints = c(better = "Rzeczywiste czasy życia w symulacji są stałe; zmienia się tylko to, co widzimy.", chance = "Większa próba nie pomoże, jeśli nadal ucinamy długie czasy życia. To błąd systematyczny.")
          )
        )
      ),
      list(
        id = "transfer", title = "Przykład transferowy: badania przeżycia",
        text = "Cenzorowanie to codzienność badań klinicznych: pacjenci, u których zdarzenie nie wystąpiło do końca obserwacji, wnoszą informację „co najmniej tyle”. Metody analizy przeżycia — z krzywą Kaplana–Meiera na czele — powstały właśnie po to, żeby tej informacji nie wyrzucać. Inżynieria niezawodności i medycyna używają tu tego samego aparatu."
      )
    ),
    pitfall = "Usunięcie działających elementów z danych systematycznie skraca obraz czasu życia."
  ),
  list(
    id = "jezyk", title = "f(t), F(t), R(t) i h(t)",
    lead = "Cztery funkcje odpowiadają na różne pytania o ten sam czas życia.",
    intro = "Cztery funkcje brzmią groźnie, ale to cztery spojrzenia na jeden rozkład — znając jedną, można wyprowadzić pozostałe. Nowością jest hazard: dzieli gęstość przez niezawodność, więc pyta o ryzyko najbliższej chwili wśród elementów, które dożyły do t. To warunkowe spojrzenie — mianownik R(t) robi tu dokładnie to, co warunek B w wykładzie drugim.",
    sections = list(
      list(
        id = "zmienna", title = "Zmienna losowa T",
        text = "Czas życia elementu opisujemy zmienną losową T: czasem od uruchomienia do awarii. Wszystkie cztery funkcje mówią o tym samym T — dystrybuanta F(t)=P(T≤t) to prawdopodobieństwo awarii do chwili t, a niezawodność R(t)=P(T>t) to prawdopodobieństwo przetrwania poza t. O tę zmienną pytaliśmy już w głosowaniu o MTTF: średnia E(T) jest tylko jedną liczbą z całego rozkładu.",
        body = list(
          risk_definition("7.4", "Dystrybuanta, niezawodność i gęstość", c(
            "Dystrybuanta czasu życia F(t) = P(T ≤ t) to prawdopodobieństwo, że element zawiedzie najpóźniej w chwili t. Funkcja niezawodności R(t) = P(T > t) to prawdopodobieństwo, że element przetrwa chwilę t. Gęstość f(t) opisuje, jak gęsto rozłożone są momenty awarii: prawdopodobieństwo awarii w krótkim przedziale (t, t + Δt] wynosi w przybliżeniu f(t)Δt."
          )),
          risk_formula("R(t)=1-F(t),\\qquad f(t)=F'(t)=-R'(t)", num = "7.3",
            legend = c("F(t)" = "prawdopodobieństwo awarii do chwili t", "R(t)" = "prawdopodobieństwo przetrwania chwili t", "f(t)" = "gęstość momentów awarii (1/h)")),
          c(
            "Z definicji wynika kilka własności, które warto umieć sprawdzić na każdym wykresie. R(0) = 1, bo nowy element działa. R(t) nigdy nie rośnie: kto przetrwał 2000 h, przetrwał też 1000 h, więc zdarzenie {T > 2000} zawiera się w {T > 1000}. Gdy t rośnie bez końca, R(t) dąży do zera — każdy element kiedyś zawiedzie. Gęstość jest spadkiem niezawodności w jednostce czasu: im szybciej ubywa działających egzemplarzy, tym większa f(t).",
            "Te trzy funkcje opisują jednak całą populację nowych elementów. Utrzymanie ruchu zadaje inne pytanie: ten konkretny wentylator działa już od 1000 godzin — jak ryzykowna jest dla niego najbliższa godzina? Odpowiedź wymaga warunkowania na przetrwanie."
          )
        )
      ),
      list(
        id = "hazard", title = "Hazard: ryzyko najbliższej chwili",
        body = list(
          risk_definition("7.5", "Hazard", c(
            "Hazard (funkcja intensywności awarii) h(t) to granica ilorazu warunkowego prawdopodobieństwa awarii w przedziale (t, t + Δt], pod warunkiem przetrwania do chwili t, przez długość przedziału Δt, gdy Δt dąży do zera. Hazard mierzy chwilowe tempo awarii wśród elementów, które wciąż działają; ma jednostkę 1/h."
          )),
          risk_formula("R(t)=1-F(t),\\qquad h(t)=\\frac{f(t)}{R(t)}", num = "7.4",
            legend = c("h(t)" = "hazard w chwili t (1/h)", "f(t)" = "gęstość czasu życia", "R(t)" = "niezawodność, czyli udział elementów działających w chwili t")),
          risk_derivation("h(t) = f(t)/R(t)", c(
            "Zaczynamy od prawdopodobieństwa warunkowego z wykładu 02. Zdarzenie „awaria w (t, t + Δt]” zawiera się w zdarzeniu „przetrwanie do t”, więc iloczyn obu zdarzeń to po prostu awaria w tym przedziale.",
            "Licznik to przyrost dystrybuanty, który dla małego Δt jest w przybliżeniu równy f(t)Δt. Po podzieleniu przez Δt i przejściu do granicy zostaje iloraz gęstości i niezawodności."
          ), lines = c(
            "P(t < T ≤ t + Δt | T > t) = P(t < T ≤ t + Δt) / P(T > t)",
            "                           = [F(t + Δt) − F(t)] / R(t)",
            "                           ≈ f(t) · Δt / R(t)",
            "h(t) = lim (Δt → 0) P(t < T ≤ t + Δt | T > t) / Δt = f(t) / R(t)"
          )),
          "Wzór (7.4) mówi, że gęstość i hazard różnią się tylko mianownikiem. Gęstość odpowiada na pytanie „jaka część wszystkich nowych elementów zawiedzie w okolicy chwili t”, hazard — „jaka część tych, które dożyły t”. Gdy działających zostaje mało, R(t) w mianowniku jest małe i hazard może być duży, choć gęstość jest już niewielka.",
          zycie_functions_table,
          lc_p("F(t) i R(t) są bezwymiarowymi prawdopodobieństwami. Gęstość f(t) i hazard h(t) mają jednostkę 1/h, gdy czas mierzymy w godzinach. Hazard nie jest prawdopodobieństwem i może być większy od 1/h: dopiero h(t)Δt przybliża prawdopodobieństwo awarii w krótkim przedziale, warunkowo dla działającego elementu."),
          risk_formula("P(t<T\\le t+\\Delta t\\mid T>t)\\approx h(t)\\Delta t", num = "7.5",
            legend = c("\\Delta t" = "krótki przedział czasu, w którym hazard jest prawie stały")),
          lc_p("Niezawodność R(t) pyta o przetrwanie całej misji bez awarii. Gotowość pyta, czy funkcja jest dostępna w danej chwili, także po naprawach. Liczba kolejnych awarii na godzinę w systemie naprawialnym opisuje proces zliczający; nie jest automatycznie hazardem czasu do pierwszej awarii. MTTF dotyczy pierwszej awarii, MTBF odstępów między awariami; nie mieszaj tych wielkości."),
          risk_example("7.3", "Cztery funkcje w jednej chwili",
            problem = "Wentylator ma wykładniczy czas życia z MTTF = 1500 h, czyli R(t) = e^(−t/1500). Oblicz F(1000), R(1000), f(1000) i h(1000). Następnie oszacuj prawdopodobieństwo, że wentylator, który działa po 1000 h, zawiedzie w ciągu najbliższych 10 h.",
            steps = c(
              "R(1000) = e^(−1000/1500) ≈ 0,513, więc F(1000) = 1 − 0,513 ≈ 0,487.",
              "Ze wzoru (7.3): f(t) = −R'(t) = (1/1500) · e^(−t/1500), więc f(1000) ≈ 0,513/1500 ≈ 0,000342 na godzinę.",
              "Ze wzoru (7.4): h(1000) = f(1000)/R(1000) = 1/1500 ≈ 0,000667 na godzinę.",
              "Ze wzoru (7.5): P(awarii w ciągu 10 h | działa po 1000 h) ≈ 0,000667 · 10 ≈ 0,0067. Dokładnie: 1 − e^(−10/1500) ≈ 0,0066 — przybliżenie jest bardzo dobre, bo przedział jest krótki."
            ),
            answer = "F ≈ 0,487, R ≈ 0,513, f ≈ 0,00034/h, h ≈ 0,00067/h. Hazard jest prawie dwa razy większy od gęstości, bo odnosi się tylko do połowy populacji, która dożyła 1000 h."
          ),
          risk_try("zostaw model wykładniczy i przesuwaj wspólną linię czasu od 0 do 4000 h. Porównaj, jak zmieniają się F(t) i R(t) w panelu oraz jak przebiegają przeskalowane krzywe f(t) i h(t)."),
          risk_widget_panel("Synchronizacja", "Wspólny suwak czasu", sliderInput("c7_time", "Czas t (h)", 0, 4000, 1000, 50), "c7_functions", "c7_functions_stats", note = "Dla rozkładu wykładniczego f(t) jest proporcjonalna do R(t), dlatego obie krzywe mają ten sam kształt, a przeskalowany hazard jest poziomą linią. To cecha tego modelu, nie ogólna reguła."),
          c(
            "Dla t = 1000 h panel pokazuje F ≈ 0,487 i R ≈ 0,513, jak w przykładzie 7.3. Suma obu wartości zawsze wynosi 1. Na wykresie gęstość przemnożono przez 3000, a hazard przez 1500, żeby wszystkie cztery krzywe zmieściły się na jednej osi. Przy takim skalowaniu przeskalowana gęstość jest równa 2R(t), a przeskalowany hazard wynosi stale 1.",
            "Najważniejsza obserwacja: gęstość maleje, choć hazard stoi w miejscu. Mniej awarii w okolicy 3000 h nie oznacza, że stare wentylatory są bezpieczniejsze — po prostu mało który dożył tego wieku. O ryzyku dla działającego egzemplarza mówi hazard, nie gęstość."
          ),
          risk_check("c7_chk_gestosc",
            "W modelu wykładniczym gęstość f(t) maleje z czasem. Co to mówi o wentylatorze, który przepracował już 3000 h?",
            c("Jest bezpieczniejszy niż nowy, bo awarie są rzadsze" = "safer", "Ma takie samo chwilowe ryzyko awarii jak nowy" = "same", "Jest bardziej ryzykowny, bo przekroczył MTTF" = "riskier"),
            correct = "same",
            explanation = "Gęstość maleje, bo maleje liczba działających egzemplarzy. Hazard h(t) = f(t)/R(t) jest w tym modelu stały i równy 1/1500 na godzinę, więc dla działającego egzemplarza wiek nie ma znaczenia.",
            hints = c(safer = "Gęstość dotyczy wszystkich nowych egzemplarzy, a nie tych, które przetrwały. Podziel f(t) przez R(t).", riskier = "Przekroczenie średniej nie zmienia hazardu w modelu wykładniczym. Sprawdź wzór (7.4).")
          )
        )
      ),
      list(
        id = "od-hazardu", title = "Od hazardu do niezawodności",
        body = list(
          c(
            "Wzór (7.4) można odwrócić. Inżynier często zna nie gęstość, lecz mechanizm: wie, że hazard jest stały, rośnie liniowo z wiekiem albo maleje po docieraniu. Z takiej hipotezy o hazardzie da się jednoznacznie odtworzyć niezawodność, a z niej — gęstość i średnią. Dzięki temu cztery funkcje naprawdę są czterema widokami jednego obiektu.",
            "Kluczowa jest skumulowana funkcja hazardu H(t) — pole pod krzywą hazardu od zera do t. Mierzy ona łączne „narażenie” elementu na awarię od chwili uruchomienia."
          ),
          risk_formula("H(t)=\\int_{0}^{t} h(u)\\,du,\\qquad R(t)=e^{-H(t)}", num = "7.6",
            legend = c("H(t)" = "skumulowany hazard do chwili t (bezwymiarowy)", "h(u)" = "hazard w chwili u")),
          risk_derivation("R(t) = e^(−H(t))", c(
            "Ze wzorów (7.3) i (7.4) hazard to −R'(t)/R(t), czyli pochodna funkcji −ln R(t). Całkujemy od 0 do t i korzystamy z tego, że R(0) = 1, więc ln R(0) = 0."
          ), lines = c(
            "h(t) = f(t)/R(t) = −R'(t)/R(t) = −[ln R(t)]'",
            "∫₀ᵗ h(u) du = −ln R(t) + ln R(0) = −ln R(t)",
            "R(t) = exp(−H(t))"
          )),
          "Ta sama logika daje drugi, często wygodniejszy wzór na MTTF. Zamiast całkować t · f(t), wystarczy zsumować pole pod krzywą niezawodności.",
          risk_formula("MTTF=\\int_{0}^{\\infty} R(t)\\,dt", num = "7.7"),
          risk_derivation("MTTF jako pole pod R(t)", c(
            "Czas życia T można zapisać jako „sumę” jedynek: dla każdej chwili t < T element jeszcze działa. Formalnie T = ∫₀^∞ 1{T > t} dt, gdzie 1{T > t} równa się 1, gdy element działa w chwili t, i 0 w przeciwnym razie.",
            "Wartość oczekiwana wskaźnika to prawdopodobieństwo zdarzenia, więc E(1{T > t}) = R(t). Zamiana kolejności wartości oczekiwanej i całki daje wzór (7.7)."
          )),
          risk_example("7.4", "Hazard rośnie liniowo",
            problem = "Utrzymanie ruchu zakłada, że hazard wentylatora rośnie proporcjonalnie do wieku: h(t) = 2t/1700² na godzinę. Wyznacz R(t) i oblicz R(1000) oraz h(1000).",
            steps = c(
              "Ze wzoru (7.6): H(t) = ∫₀ᵗ 2u/1700² du = t²/1700² = (t/1700)².",
              "R(t) = exp[−(t/1700)²]. To dokładnie model oferty B z przykładu 7.1.",
              "H(1000) = (1000/1700)² ≈ 0,346, więc R(1000) = e^(−0,346) ≈ 0,707.",
              "h(1000) = 2 · 1000/1700² ≈ 0,000692 na godzinę — niemal tyle samo co stały hazard oferty A (0,000667)."
            ),
            answer = "R(1000) ≈ 0,707. W chwili 1000 h oba modele mają prawie równy hazard, ale oferta B dochodzi do niego od zera, więc do tego momentu zgromadziła mniejszy skumulowany hazard (0,346 wobec 0,667) i ma wyższą niezawodność."
          )
        )
      )
    )
  ),
  list(
    id = "wykladniczy", title = "Rozkład wykładniczy i gamma",
    lead = "Stały hazard daje model bez pamięci; suma k takich etapów daje rozkład gamma, a dla całkowitego k — Erlanga.",
    intro = c(
      "Najprostsza hipoteza o hazardzie brzmi: jest stały. Element nie dociera się i nie zużywa — psuje się od losowych zaburzeń, które w każdej godzinie są tak samo prawdopodobne. Ta hipoteza wyznacza dokładnie jeden rozkład: wykładniczy, ciągły odpowiednik geometrycznego z wykładu piątego.",
      "Konsekwencją stałego hazardu jest brak pamięci: wentylator pracujący od 1000 godzin ma przed sobą dokładnie taki sam rozkład dalszego życia jak fabrycznie nowy. Jeśli dane pokazują, że stare egzemplarze psują się częściej niż nowe, model wykładniczy jest z góry wykluczony — żaden dobór λ tego nie naprawi."
    ),
    sections = list(
      list(
        id = "staly-hazard", title = "Stały hazard",
        body = list(
          c(
            "Ze wzoru (7.6) hipoteza „hazard równy stałej λ” od razu wyznacza niezawodność: skumulowany hazard rośnie liniowo, H(t) = λt, więc R(t) = e^(−λt). Gęstość jest pochodną dystrybuanty, a MTTF — polem pod krzywą niezawodności ze wzoru (7.7), czyli ∫₀^∞ e^(−λt) dt = 1/λ. Cały rozkład wyznacza jedna liczba."
          ),
          risk_definition("7.6", "Rozkład wykładniczy", c(
            "Czas życia T ma rozkład wykładniczy z parametrem λ > 0 (intensywnością awarii), jeśli jego hazard jest stały i równy λ. Gęstość, niezawodność i średnią podaje wzór (7.8)."
          )),
          risk_formula("f(t)=\\lambda e^{-\\lambda t},\\qquad R(t)=e^{-\\lambda t},\\qquad h(t)=\\lambda,\\qquad MTTF=1/\\lambda", num = "7.8",
            legend = c("\\lambda" = "stała intensywność awarii (1/h)", "t" = "czas pracy (h)")),
          risk_derivation("wykładniczy jako granica geometrycznego", c(
            "Podzielmy czas pracy na krótkie kroki długości Δ, na przykład jednej godziny. Jeśli w każdym kroku awaria zdarza się niezależnie z prawdopodobieństwem p = λΔ, liczba kroków do awarii ma rozkład geometryczny z wykładu 05, a ze wzoru (5.2) P(X > n) = (1 − p)ⁿ.",
            "Chwila t odpowiada n = t/Δ krokom. Gdy kroki stają się coraz krótsze, (1 − λΔ)^(t/Δ) dąży do e^(−λt). Dla MTTF = 1500 h i kroków godzinnych (1 − 1/1500)¹⁰⁰⁰ ≈ 0,5133, a dokładne e^(−1000/1500) ≈ 0,5134."
          ), lines = c(
            "P(T > t) ≈ (1 − λΔ)^(t/Δ)",
            "ln P(T > t) ≈ (t/Δ) · ln(1 − λΔ) ≈ (t/Δ) · (−λΔ) = −λt",
            "P(T > t) → e^(−λt)   gdy Δ → 0"
          )),
          c(
            "Wykładniczy dziedziczy po geometrycznym najważniejszą własność. We wzorze (5.4) pokazaliśmy, że seria kontroli bez wykrycia nie przybliża wykrycia. W czasie ciągłym brzmi to tak: jeśli element przetrwał s godzin, prawdopodobieństwo, że przetrwa jeszcze t godzin, jest takie samo jak dla elementu nowego."
          ),
          risk_formula("P(T>s+t\\mid T>s)=P(T>t)=e^{-\\lambda t}", num = "7.9",
            legend = c("s" = "czas, który element już przepracował", "t" = "dodatkowy czas pracy")),
          "Dowód jest przepisaniem dowodu wzoru (5.4): P(T > s + t | T > s) = R(s + t)/R(s) = e^(−λ(s+t)) / e^(−λs) = e^(−λt). W języku hazardu brak pamięci jest oczywisty — skoro ryzyko najbliższej chwili nie zależy od wieku, to dalsza przyszłość elementu też od niego nie zależy.",
          risk_example("7.5", "Wentylator po 1000 godzinach",
            problem = "Wentylator o wykładniczym czasie życia z MTTF = 1500 h przepracował bez awarii 1000 h. (a) Jakie jest prawdopodobieństwo, że przetrwa kolejne 500 h? (b) Porównaj z nowym wentylatorem. (c) Jaka część nowych wentylatorów zawodzi przed upływem MTTF i ile wynosi mediana czasu życia?",
            steps = c(
              "(a) Ze wzoru (7.9): P(T > 1500 | T > 1000) = e^(−500/1500) = e^(−1/3) ≈ 0,717.",
              "(b) Nowy wentylator: R(500) = e^(−1/3) ≈ 0,717 — dokładnie to samo.",
              "(c) F(MTTF) = 1 − e^(−λ · 1/λ) = 1 − e⁻¹ ≈ 0,632. Mediana m spełnia e^(−m/1500) = 0,5, więc m = 1500 · ln 2 ≈ 1040 h."
            ),
            answer = "(a) i (b) około 0,717; (c) około 63% wentylatorów zawodzi przed upływem średniej, a połowa — przed 1040 h. Średnią 1500 h podnoszą rzadkie, bardzo długie życia z prawego ogona, jak w rozkładzie geometrycznym."
          ),
          risk_try("zmieniaj MTTF od 300 do 4000 h i obserwuj wartość R(1000 h) w panelu. Sprawdź, dla jakiego MTTF niezawodność w horyzoncie 1000 h przekracza 0,7."),
          risk_widget_panel("Model", "Stały hazard", sliderInput("c7_mttf", "MTTF (h)", 300, 4000, 1500, 50), "c7_exp", "c7_exp_stats"),
          c(
            "Przy MTTF = 1500 h panel pokazuje R(1000 h) ≈ 0,513. Podwojenie MTTF do 3000 h podnosi tę wartość do około 0,717, a MTTF = 4000 h daje około 0,779. Przy MTTF = 500 h niezawodność w horyzoncie 1000 h spada do 0,135. Kształt krzywej zawsze jest ten sam — zmienia się tylko skala osi czasu, a w chwili t = MTTF krzywa przechodzi przez e⁻¹ ≈ 0,368.",
            "Ta sztywność jest zaletą i wadą zarazem. Zaletą, bo jeden parametr łatwo oszacować ze wzoru (7.2). Wadą, bo model nie ma czym opisać docierania ani zużycia. Jeśli dane pokazują starzenie, trzeba sięgnąć po rodzinę z dodatkowym parametrem kształtu."
          ),
          risk_check("c7_chk_pamiec",
            "Wentylator z wykładniczym czasem życia (MTTF = 1500 h) przepracował 2000 h. Ile wynosi oczekiwany dalszy czas jego pracy?",
            c("1500 h" = "full", "0 h — przekroczył już średnią" = "zero", "Około 500 h" = "rest"),
            correct = "full",
            explanation = "Z braku pamięci (7.9) dalszy czas życia działającego elementu ma ten sam rozkład wykładniczy co czas życia nowego, więc jego średnia wynosi nadal 1500 h.",
            hints = c(zero = "MTTF nie jest terminem ważności. W modelu wykładniczym wiek nie zmienia rozkładu dalszego życia.", rest = "Odejmowanie przepracowanego czasu od średniej zakłada zużycie. W modelu wykładniczym zużycia nie ma — wzór (7.9).")
          )
        ),
        pitfall = "Brak pamięci nie pasuje do wyraźnego docierania ani zużycia."
      ),
      list(
        id = "gamma", title = "Rozkład gamma i przypadek Erlanga",
        text = c(
          "W wykładzie piątym czekaliśmy na r-te wykrycie, licząc dyskretne próby; gamma robi to samo w czasie ciągłym. W jednorodnym procesie Poissona o intensywności λ czas do k-tego zdarzenia jest sumą k niezależnych czasów wykładniczych o tej samej intensywności — tak jak ujemny dwumianowy był sumą k oczekiwań geometrycznych. Ta paralela to nie przypadek, lecz ta sama konstrukcja w dwóch skalach czasu.",
          "Erlang jest rozkładem gamma o całkowitym parametrze kształtu k: sumą k niezależnych etapów o wykładniczych czasach — na przykład czasem do k-tej awarii w jednorodnym procesie Poissona. Ogólny rozkład gamma dopuszcza dowolne k>0. Kształt niecałkowity traci interpretację etapów, ale pozwala modelować hazard rosnący (k>1) albo malejący (k<1) i dopasowywać rozkład do danych bez sztucznego zaokrąglania."
        ),
        body = list(
          risk_definition("7.7", "Rozkład gamma", c(
            "Czas T ma rozkład gamma z parametrem kształtu k > 0 i intensywnością λ > 0, jeśli jego gęstość dana jest wzorem (7.10). Dla całkowitego k rozkład nazywamy rozkładem Erlanga; opisuje on wtedy sumę k niezależnych czasów wykładniczych z parametrem λ. Dla k = 1 otrzymujemy rozkład wykładniczy."
          )),
          risk_formula("f(t)=\\frac{\\lambda^{k}t^{k-1}e^{-\\lambda t}}{\\Gamma(k)},\\qquad E(T)=k/\\lambda,\\qquad \\operatorname{Var}(T)=k/\\lambda^{2}", num = "7.10",
            legend = c("k" = "parametr kształtu (dla Erlanga: liczba etapów)", "\\lambda" = "intensywność pojedynczego etapu (1/h)", "\\Gamma(k)" = "funkcja gamma; dla całkowitego k równa (k − 1)!")),
          "Średnia i wariancja wynikają z tego samego rachunku co wzór (5.5): średnia sumy to suma k średnich 1/λ, a wariancja sumy niezależnych składników to suma k wariancji 1/λ². Do planowania potrzebujemy jednak prawdopodobieństwa, że k-te zdarzenie nastąpi dopiero po chwili t. Tu działa ta sama sztuczka, która w wykładzie 05 dała wzór (5.7).",
          risk_formula("P(T_k>t)=P(N(t)\\le k-1)=\\sum_{j=0}^{k-1} e^{-\\lambda t}\\frac{(\\lambda t)^{j}}{j!}", num = "7.11",
            legend = c("T_k" = "czas do k-tego zdarzenia", "N(t)" = "liczba zdarzeń w przedziale [0, t], rozkład Poissona o średniej λt")),
          risk_derivation("wzór (7.11)", c(
            "k-te zdarzenie nastąpi po chwili t wtedy i tylko wtedy, gdy do chwili t zdarzeń było mniej niż k. To dwa opisy tego samego zdarzenia — jeden w języku czasu oczekiwania, drugi w języku liczby zdarzeń. Dokładnie tak we wzorze (5.7) zamienialiśmy „r-te wykrycie najpóźniej w n-tej kontroli” na „co najmniej r wykryć w n kontrolach”.",
            "W jednorodnym procesie Poissona N(t) ma rozkład Poissona o średniej λt, więc wystarczy zsumować k pierwszych prawdopodobieństw tego rozkładu."
          )),
          risk_example("7.6", "Czy zapas wentylatorów wystarczy?",
            problem = "W całej hali dojrzewalni awarie wentylatorów (z natychmiastową wymianą) pojawiają się jak jednorodny proces Poissona — średnio jedna na 500 h. Magazyn ma trzy wentylatory zapasowe; zapas wyczerpuje się w chwili trzeciej awarii. Jaki jest średni czas do wyczerpania zapasu i jakie jest prawdopodobieństwo, że zapas nie wyczerpie się przed upływem 1000 h?",
            steps = c(
              "Czas do trzeciej awarii T₃ ma rozkład Erlanga z k = 3 i λ = 1/500 na godzinę. Ze wzoru (7.10): E(T₃) = 3 · 500 = 1500 h, a odchylenie standardowe √3 · 500 ≈ 866 h.",
              "Ze wzoru (7.11) z λt = 1000/500 = 2: P(T₃ > 1000) = e⁻² · (1 + 2 + 2²/2) = 5e⁻² ≈ 0,677.",
              "Dla porównania: gdyby zapas był jeden, P(T₁ > 1000) = e⁻² ≈ 0,135."
            ),
            answer = "Średnio 1500 h; z prawdopodobieństwem około 0,68 zapas wystarczy na 1000 h. Mniej więcej w jednym okresie na trzy trzeba będzie zamówić wentylatory wcześniej — średnia sama nie wystarcza do planu, tak jak w wykładzie 05."
          ),
          risk_try("zacznij od k = 1 i porównaj kształt z krzywą wykładniczą. Potem ustaw k = 3 (przykład 7.6) i k = 8. Obserwuj, gdzie leży szczyt gęstości i jak zmienia się jej symetria."),
          risk_widget_panel("Model", "Czas oczekiwania o kształcie k", sliderInput("c7_k", "Parametr kształtu k", .5, 8, 3, .5), "c7_gamma", "c7_gamma_stats", note = "Dla całkowitego k suwak pokazuje rozkłady Erlanga; wartości pośrednie należą do ogólnej rodziny gamma."),
          c(
            "Wykres używa skali 500 h, czyli λ = 1/500, jak w przykładzie 7.6. Dla k = 1 gęstość jest najwyższa w zerze i opada wykładniczo. Dla k = 3 szczyt przesuwa się do (k − 1) · 500 = 1000 h, a średnia wynosi 1500 h. Dla k = 8 średnia to 4000 h, szczyt leży przy 3500 h, a kształt jest wyraźnie bardziej symetryczny — ten sam efekt, który w wykładzie 05 widzieliśmy dla ujemnego dwumianowego przy rosnącym r.",
            "Dla k = 0,5 gęstość w pobliżu zera jest bardzo duża: ogólna gamma z k < 1 opisuje sytuację, w której wiele awarii zdarza się tuż po uruchomieniu, a hazard maleje. Takiej wartości nie da się czytać jako „pół etapu” — to już tylko parametr kształtu."
          ),
          risk_check("c7_chk_gamma",
            "Awarie w hali pojawiają się średnio raz na 500 h (proces Poissona). Ile wynosi średni czas do czwartej awarii?",
            c("500 h" = "one", "2000 h" = "four", "125 h" = "quarter"),
            correct = "four",
            explanation = "Czas do czwartej awarii jest sumą czterech niezależnych czasów wykładniczych o średniej 500 h, więc ze wzoru (7.10) E(T₄) = 4 · 500 = 2000 h.",
            hints = c(one = "500 h to średni odstęp między kolejnymi awariami. Ile takich odstępów trzeba zsumować?", quarter = "Czekanie na więcej zdarzeń trwa dłużej, nie krócej. Wzór (7.10): E(T) = k/λ.")
          )
        ),
        takeaway = "Most przez Poissona: przy stałej intensywności, niezależnych przyrostach i pojedynczych zdarzeniach liczba zdarzeń N(t) ma rozkład Poissona o średniej λt. Czas do pierwszego jest wykładniczy, do k-tego — Erlanga. Stała średnia liczba zgłoszeń nie wystarcza, jeśli zgłoszenia przychodzą grupami albo zależą od wcześniejszych. To krótki kontekst dla gamma, nie dodatkowy rozbudowany dział."
      )
    )
  ),
  list(
    id = "weibull", title = "Część B — mechanizm Weibulla",
    lead = "Parametr β opisuje kierunek zmiany hazardu, η skalę czasu, a ten sam MTTF może kryć różne R(t).",
    intro = c(
      "Weibull jest domyślnym językiem inżynierii niezawodności, bo jednym parametrem odpowiada na najważniejsze pytanie diagnostyczne: co dzieje się z hazardem. β < 1 oznacza hazard malejący (wczesne defekty odsiewają się z parku), β = 1 odtwarza rozkład wykładniczy, a β > 1 — hazard rosnący, charakterystyczny dla zużycia.",
      "Drugi parametr, η, jest czystą skalą czasu: mówi, kiedy rzeczy się dzieją, a nie jak. Przy każdym β niezawodność w chwili t = η wynosi e⁻¹ ≈ 0,37 — to punkt orientacyjny, po którym łatwo czytać wykresy."
    ),
    body = c(
      "Tu zaczyna się druga część wykładu. Z części pierwszej potrzebujemy trzech narzędzi: wzoru (7.4), który definiuje hazard jako ryzyko najbliższej chwili dla działającego elementu, wzoru (7.6), który z hipotezy o hazardzie odtwarza niezawodność, oraz wzoru (7.7), który z niezawodności daje MTTF. Rozkład wykładniczy okazał się modelem jednej hipotezy — hazardu stałego — i dlatego nie potrafi opisać ani docierania, ani zużycia.",
      "Druga część odpowiada na pytanie, które w dojrzewalni jest najważniejsze: co zrobić, gdy hazard zmienia się z wiekiem? Zbudujemy model z parametrem kształtu, porównamy urządzenia o tym samym MTTF, rozłożymy krzywą wannową na mechanizmy i zamienimy wszystko w plan przeglądu."
    ),
    sections = list(
      list(
        id = "parametry", title = "β i η",
        body = list(
          c(
            "W przykładzie 7.4 hazard rósł liniowo i otrzymaliśmy R(t) = exp[−(t/1700)²]. Uogólnijmy ten pomysł: niech skumulowany hazard będzie potęgą czasu, H(t) = (t/η)^β. Wykładnik β decyduje o tym, czy narażenie narasta coraz szybciej, równomiernie czy coraz wolniej, a η ustala, w jakiej skali czasu to się dzieje."
          ),
          risk_definition("7.8", "Rozkład Weibulla", c(
            "Czas życia T ma rozkład Weibulla z parametrem kształtu β > 0 i parametrem skali η > 0, jeśli jego skumulowany hazard wynosi H(t) = (t/η)^β, a więc niezawodność dana jest wzorem (7.12)."
          )),
          risk_formula("R(t)=\\exp[-(t/\\eta)^\\beta]", num = "7.12",
            legend = c("\\beta" = "parametr kształtu (bez jednostki)", "\\eta" = "parametr skali, czyli charakterystyczny czas życia (h)")),
          "Hazard otrzymujemy, różniczkując skumulowany hazard. To wzór, w którym widać cały mechanizm Weibulla:",
          risk_formula("h(t)=\\frac{\\beta}{\\eta}\\left(\\frac{t}{\\eta}\\right)^{\\beta-1},\\qquad f(t)=h(t)\\,R(t)", num = "7.13"),
          c(
            "Czas występuje w hazardzie w potędze β − 1. Gdy β > 1, wykładnik jest dodatni i hazard rośnie z wiekiem — model zużycia. Gdy β = 1, wykładnik jest zerowy, hazard jest stały i równy 1/η — to rozkład wykładniczy z MTTF = η. Gdy β < 1, wykładnik jest ujemny i hazard maleje — model wczesnych defektów. Dla żadnego β hazard nie może najpierw maleć, a potem rosnąć: kierunek zmiany jest ustalony raz na zawsze.",
            "Średni czas życia wynika ze wzoru (7.7). Całka pola pod krzywą exp[−(t/η)^β] prowadzi do funkcji gamma, tej samej, która pojawiła się w gęstości (7.10)."
          ),
          risk_formula("MTTF=\\eta\\,\\Gamma\\!\\left(1+\\frac{1}{\\beta}\\right)", num = "7.14",
            legend = c("\\Gamma" = "funkcja gamma; Γ(2) = 1, Γ(1,5) = √π/2 ≈ 0,886")),
          risk_derivation("wzór (7.14)", c(
            "Podstawiamy u = (t/η)^β, czyli t = η · u^(1/β) i dt = (η/β) · u^(1/β − 1) du. Całka z wzoru (7.7) przechodzi w definicję funkcji gamma."
          ), lines = c(
            "MTTF = ∫₀^∞ exp[−(t/η)^β] dt",
            "     = (η/β) · ∫₀^∞ u^(1/β − 1) · e^(−u) du",
            "     = (η/β) · Γ(1/β) = η · Γ(1 + 1/β)"
          )),
          risk_example("7.7", "Wentylator zużywający się",
            problem = "Wentylator oferty B ma rozkład Weibulla z β = 2 i η = 1700 h. Oblicz R(1000), h(500), h(1000), h(2000), R(1700) oraz MTTF.",
            steps = c(
              "Ze wzoru (7.12): R(1000) = exp[−(1000/1700)²] = exp(−0,346) ≈ 0,707.",
              "Ze wzoru (7.13) z β = 2: h(t) = 2t/1700². Stąd h(500) ≈ 0,000346, h(1000) ≈ 0,000692 i h(2000) ≈ 0,001384 na godzinę — hazard rośnie proporcjonalnie do wieku.",
              "R(1700) = exp(−1) ≈ 0,368 — w chwili t = η zawsze pozostaje około 37% działających egzemplarzy.",
              "Ze wzoru (7.14): MTTF = 1700 · Γ(1,5) ≈ 1700 · 0,886 ≈ 1507 h."
            ),
            answer = "R(1000) ≈ 0,707; hazard podwaja się przy podwojeniu wieku (0,00035 → 0,00069 → 0,00138 na godzinę); R(1700) ≈ 0,368; MTTF ≈ 1507 h."
          ),
          risk_try("zacznij od β = 2 i η = 1700 h. Następnie ustaw β = 1 i η = 1500 h, a potem β = 0,5. Za każdym razem odczytaj kierunek hazardu i R(1000 h) w panelu; na dolnym wykresie zwróć uwagę na przebieg h(t) tuż po uruchomieniu."),
          risk_widget_panel("Model", "R(t) i h(t) reagują razem", tagList(sliderInput("c7_beta", "β", .4, 4, 2, .1), sliderInput("c7_eta", "η (h)", 300, 4000, 1700, 50)), "c7_weibull", "c7_weibull_stats"),
          c(
            "Dla β = 2 i η = 1700 h panel pokazuje hazard rosnący i R(1000 h) ≈ 0,707, jak w przykładzie 7.7; dolny wykres to prosta linia wychodząca z zera. Dla β = 1 i η = 1500 h hazard jest poziomy, a R(1000 h) ≈ 0,513 — odtworzyliśmy ofertę A. Dla β = 0,5 (przy η = 1700 h) hazard startuje bardzo wysoko i szybko opada: w chwili 100 h wynosi około 0,0012, a w chwili 1000 h około 0,0004 na godzinę.",
            "Zmiana η przy stałym β nie zmienia kształtu żadnej krzywej, tylko rozciąga lub ściska oś czasu. To dlatego η nazywa się parametrem skali: dwa parki maszyn o tym samym mechanizmie awarii, ale różnej jakości wykonania, różnią się η, a nie β."
          ),
          risk_check("c7_chk_beta",
            "Element ma rozkład Weibulla z β = 3. Ile razy wzrośnie jego hazard, gdy wiek wzrośnie dwukrotnie?",
            c("2 razy" = "two", "4 razy" = "four", "8 razy" = "eight"),
            correct = "four",
            explanation = "Ze wzoru (7.13) hazard jest proporcjonalny do t^(β−1) = t². Podwojenie wieku mnoży hazard przez 2² = 4. Czynnik 8 = 2³ dotyczy skumulowanego hazardu H(t) = (t/η)³.",
            hints = c(two = "Hazard rośnie liniowo tylko dla β = 2. Jaki jest wykładnik β − 1 dla β = 3?", eight = "2³ = 8 to wzrost skumulowanego hazardu H(t). Hazard ma wykładnik β − 1.")
          )
        ),
        takeaway = "Dobór β nie jest kosmetyką statystyczną, lecz hipotezą o mechanizmie awarii. Zanim dopasujesz parametry do danych, zapytaj inżyniera utrzymania: czy ten element się dociera, zużywa, czy psuje losowo?"
      ),
      list(
        id = "same-mttf", title = "Ten sam MTTF, inne R(t)",
        text = c(
          "Wracamy do głosowania z początku wykładu, tym razem z rachunkiem. Trzy modele Weibulla — o hazardzie malejącym, stałym i rosnącym — skalibrowano tak, żeby wszystkie miały MTTF równy dokładnie 1500 godzin. Karta katalogowa nie odróżni ich od siebie.",
          "Przesuń czas misji i odczytaj trzy wartości R(t) na pionowej linii. Dla krótkich misji najlepszy jest model zużyciowy (β > 1): awarie przychodzą późno, ale zbiorowo. Dla długich misji przewaga się odwraca. Wniosek praktyczny: porównywanie urządzeń po MTTF bez czasu misji jest porównywaniem nieporównywalnego."
        ),
        body = list(
          risk_example("7.8", "Kalibracja do wspólnego MTTF",
            problem = "Wyznacz η dla trzech modeli Weibulla o MTTF = 1500 h i kształtach β = 0,7, β = 1 i β = 2,5. Następnie oblicz R(1000) dla każdego z nich.",
            steps = c(
              "Ze wzoru (7.14): η = MTTF / Γ(1 + 1/β).",
              "β = 0,7: Γ(1 + 1/0,7) ≈ 1,266, więc η ≈ 1185 h. β = 1: Γ(2) = 1, więc η = 1500 h. β = 2,5: Γ(1,4) ≈ 0,887, więc η ≈ 1691 h.",
              "Ze wzoru (7.12): R(1000) ≈ exp[−(1000/1185)^0,7] ≈ 0,41; R(1000) = e^(−1000/1500) ≈ 0,51; R(1000) ≈ exp[−(1000/1691)^2,5] ≈ 0,76."
            ),
            answer = "η ≈ 1185, 1500 i 1691 h; R(1000) ≈ 0,41, 0,51 i 0,76. Przy tej samej średniej różnica w niezawodności misji 1000 h sięga 35 punktów procentowych."
          ),
          risk_try("zacznij od misji 1000 h i odczytaj trzy wartości R(t) na pionowej linii. Potem przesuwaj czas misji w prawo i znajdź moment, w którym krzywa β = 2,5 spada poniżej pozostałych."),
          risk_widget_panel("Porównanie", "Modele skalibrowane do MTTF=1500 h", sliderInput("c7_mission", "Czas misji (h)", 100, 3000, 1000, 50), "c7_same_mean", "c7_same_mean_stats"),
          c(
            "Dla misji 1000 h pionowa linia przecina krzywe przy wartościach 0,41 (β = 0,7), 0,51 (β = 1) i 0,76 (β = 2,5), zgodnie z przykładem 7.8. Model zużyciowy przestaje być najlepszy około 1830 h, kiedy jego krzywa przecina krzywą wykładniczą, a około 1940 h spada także poniżej krzywej β = 0,7. Około 2600 h krzywa wykładnicza przecina krzywą β = 0,7 i od tej chwili model z malejącym hazardem jest najlepszy. Przy 3000 h wartości R wynoszą 0,147, 0,135 i zaledwie 0,015.",
            "Mechanizm jest ten sam co w przykładzie 7.1. Model β = 0,7 traci wiele egzemplarzy wcześnie, ale te, które przetrwały, żyją bardzo długo. Model β = 2,5 prawie nie traci egzemplarzy na początku, ale później zużycie dopada wszystkie niemal jednocześnie. Średnie się wyrównują, a niezawodności misji — nie."
          ),
          risk_check("c7_chk_misja",
            "Wentylator ma pracować bez przeglądu przez 3000 h. Który z trzech modeli o MTTF = 1500 h daje najwyższą niezawodność misji?",
            c("β = 2,5, bo nie ma wczesnych awarii" = "wear", "β = 0,7" = "early", "Wszystkie jednakowo, bo mają ten sam MTTF" = "equal"),
            correct = "early",
            explanation = "R(3000) wynosi około 0,147 dla β = 0,7, 0,135 dla β = 1 i tylko 0,015 dla β = 2,5. Przy długiej misji zużycie eliminuje prawie cały park, a egzemplarze modelu z malejącym hazardem, które przetrwały start, żyją długo.",
            hints = c(wear = "Brak wczesnych awarii pomaga w krótkiej misji. Co dzieje się z hazardem β = 2,5 po 2000 h?", equal = "Wróć do przykładu 7.1: ta sama średnia nie oznacza tego samego R(t).")
          )
        ),
        decision = "Wybieraj urządzenie pod konkretny czas misji: porównuj R(t) w horyzoncie eksploatacji, nie sam MTTF z katalogu."
      )
    )
  ),
  list(
    id = "wanna", title = "Krzywa wannowa to złożenie mechanizmów",
    lead = "Wczesne defekty, okres stabilny i zużycie tworzą trzy składowe hazardu.",
    intro = "Podręcznikowa krzywa wannowa — wysoki hazard na początku, płaski środek, wznoszący koniec — bywa błędnie przedstawiana jako „kształt rozkładu Weibulla”. Tymczasem pojedynczy Weibull ma hazard monotoniczny: może odtworzyć jedno ramię wanny, nigdy całą. Wanna powstaje z nałożenia trzech mechanizmów, z których każdy ma własny przebieg i własne lekarstwo.",
    sections = list(
      list(
        id = "mechanizmy", title = "Trzy mechanizmy, trzy interwencje",
        bullets = c(
          "wczesne defekty (hazard malejący) — może pomagać docieranie i kontrola odbiorcza; skuteczność wymaga sprawdzenia mechanizmu;",
          "awarie losowe (hazard stały) — pomaga redundancja i ochrona przed zaburzeniami zewnętrznymi;",
          "zużycie (hazard rosnący) — może pomagać wymiana profilaktyczna we właściwym momencie."
        ),
        body = list(
          c(
            "Wentylator w dojrzewalni może zawieść z kilku niezależnych powodów. Wadliwy lut w sterowniku ujawni się zwykle w pierwszych setkach godzin. Przepięcie w sieci może przyjść w dowolnej chwili, niezależnie od wieku. Łożysko wyciera się powoli i jego ryzyko rośnie z każdą godziną. Wentylator działa, dopóki nie zadziała żaden z tych mechanizmów — jego czas życia to najkrótszy z trzech czasów.",
            "Jak z trzech hazardów zrobić jeden? Odpowiedź daje reguła mnożenia dla zdarzeń niezależnych z wykładu 02 i wzór (7.6)."
          ),
          risk_formula("R(t)=R_1(t)\\,R_2(t)\\,R_3(t),\\qquad h(t)=h_1(t)+h_2(t)+h_3(t)", num = "7.15",
            legend = c("R_i(t)" = "niezawodność względem i-tego mechanizmu", "h_i(t)" = "hazard i-tego mechanizmu")),
          risk_derivation("sumowanie hazardów", c(
            "Czas życia to T = min(T₁, T₂, T₃). Element działa w chwili t wtedy i tylko wtedy, gdy żaden mechanizm jeszcze nie zadziałał, więc przy niezależnych mechanizmach niezawodności się mnożą.",
            "Logarytm iloczynu jest sumą logarytmów, więc ze wzoru (7.6) skumulowane hazardy się sumują. Różniczkując, dostajemy sumę hazardów."
          ), lines = c(
            "R(t) = P(T₁ > t, T₂ > t, T₃ > t) = R₁(t) · R₂(t) · R₃(t)",
            "H(t) = −ln R(t) = H₁(t) + H₂(t) + H₃(t)",
            "h(t) = H'(t) = h₁(t) + h₂(t) + h₃(t)"
          )),
          "Wzór (7.15) tłumaczy wannę bez żadnej dodatkowej teorii. Wczesne defekty dają składnik malejący, awarie losowe — stały, zużycie — rosnący. Suma najpierw maleje, bo dominuje pierwszy składnik, potem jest prawie płaska, a na końcu rośnie. Każdy składnik z osobna może być Weibullem; suma już nie.",
          risk_example("7.9", "Który mechanizm dominuje?",
            problem = "W modelu z widgetu hazardy (w jednostkach względnych) wynoszą: wczesne defekty h₁(t) = 1,2 · e^(−t/350), awarie losowe h₂(t) = 0,12, zużycie h₃(t) = (t/4000)³ przy nasileniu zużycia równym 1. Oblicz trzy składowe i hazard całkowity dla t = 100, 1000 i 3000.",
            steps = c(
              "t = 100: h₁ = 1,2 · e^(−0,286) ≈ 0,902; h₂ = 0,12; h₃ = (0,025)³ ≈ 0,00002. Suma ≈ 1,022.",
              "t = 1000: h₁ = 1,2 · e^(−2,857) ≈ 0,069; h₂ = 0,12; h₃ = 0,25³ ≈ 0,016. Suma ≈ 0,205.",
              "t = 3000: h₁ ≈ 0,0002; h₂ = 0,12; h₃ = 0,75³ ≈ 0,422. Suma ≈ 0,542."
            ),
            answer = "Na początku dominują wczesne defekty (88% hazardu), w środku — awarie losowe (około 59%), a pod koniec — zużycie (około 78%). Minimum hazardu całkowitego przypada w tym modelu około t ≈ 1310."
          ),
          risk_try("zacznij od nasilenia zużycia 1 i znajdź na wykresie dno wanny. Następnie ustaw nasilenie 2 i 0,2. Obserwuj, jak zmienia się położenie dna i które ramię wanny reaguje na suwak."),
          risk_widget_panel("Mechanizmy", "Suma trzech składowych", sliderInput("c7_wear", "Nasilenie zużycia", .2, 2, 1, .1), "c7_bathtub", "c7_bathtub_stats"),
          c(
            "Suwak zmienia wyłącznie składową zużycia, więc lewe ramię wanny się nie rusza. Przy nasileniu 1 dno leży około t ≈ 1310, przy nasileniu 2 przesuwa się wcześniej, do około 1160, a przy nasileniu 0,2 — później, do około 1700. Mocniejsze zużycie skraca okres stabilny z prawej strony i podnosi całą prawą część krzywej.",
            "To obraz, który warto przenieść na decyzje. Środkowy, płaski odcinek wanny jest okresem, w którym element zachowuje się prawie jak wykładniczy — wymiana profilaktyczna niewiele tu daje. Na lewym ramieniu pomaga kontrola odbiorcza i docieranie, na prawym — wymiana przed wejściem w strefę zużycia."
          ),
          risk_check("c7_chk_wanna",
            "Dlaczego pojedynczy rozkład Weibulla nie może opisać pełnej krzywej wannowej?",
            c("Bo jego hazard jest monotoniczny: dla danego β tylko rośnie, tylko maleje albo jest stały" = "monotone", "Bo ma tylko dwa parametry, a wanna ma trzy odcinki" = "params", "Bo Weibull nie dopuszcza hazardu malejącego" = "no_decrease"),
            correct = "monotone",
            explanation = "Ze wzoru (7.13) hazard Weibulla jest proporcjonalny do t^(β−1), więc zmienia się zawsze w jednym kierunku. Wanna wymaga zmiany kierunku, a tę daje dopiero suma hazardów (7.15).",
            hints = c(params = "Liczba parametrów to nie wszystko. Przyjrzyj się wykładnikowi β − 1 we wzorze (7.13).", no_decrease = "Weibull z β < 1 ma hazard malejący. Problem leży w czymś innym.")
          )
        )
      )
    ),
    takeaway = "Wanna jest kształtem hazardu, który można uzyskać przez nałożenie trzech mechanizmów: wczesnych defektów, awarii losowych i zużycia. Dlatego plan przeglądów oparty na jednym dopasowanym modelu może być trafny w środku życia elementu, a mylny na jego początku i końcu.",
    pitfall = "Pojedynczy Weibull ma hazard monotoniczny; nie tworzy pełnej krzywej wannowej."
  ),
  list(
    id = "przeglad", title = "Plan przeglądu",
    lead = "Czas interwencji wynika z wymaganego R(t), kosztów i mechanizmu awarii.",
    intro = c(
      "Pytanie utrzymaniowe brzmi konkretnie: po ilu godzinach zaplanować przegląd wentylatora, żeby ryzyko awarii przed przeglądem pozostało akceptowalne? Suwak poniżej liczy R(t) dla modelu zużyciowego Weibulla (β = 2, η = 1700 h) — przesuwaj czas przeglądu i obserwuj, jak rośnie ryzyko.",
      "Zauważ, że sensowność wymiany profilaktycznej zależy od mechanizmu: przy hazardzie rosnącym wcześniejsza wymiana naprawdę redukuje ryzyko, ale przy stałym hazardzie wymiana sprawnego elementu na nowy niczego nie zmienia — nowy ma dokładnie ten sam hazard co stary. Plan przeglądów bez hipotezy o hazardzie jest strzałem w ciemno."
    ),
    sections = list(
      list(
        id = "wymagana", title = "Od wymaganej niezawodności do terminu",
        body = list(
          c(
            "Kierownik utrzymania ruchu formułuje wymaganie: do chwili przeglądu awarii może doznać co najwyżej 10% wentylatorów. W języku funkcji czasu życia to warunek R(t) ≥ 0,90. Zamiast szukać terminu na wykresie metodą prób, możemy odwrócić wzór na niezawodność. W przemyśle taki czas ma własną nazwę."
          ),
          risk_definition("7.9", "Czas życia Bq", c(
            "Czas życia Bq to chwila, do której zawodzi q procent elementów, czyli rozwiązanie równania F(t) = q/100 albo równoważnie R(t) = 1 − q/100. Najczęściej podaje się B10 — czas, do którego przetrwa 90% egzemplarzy."
          )),
          risk_formula("t_{R^*}=\\eta\\,\\left(-\\ln R^*\\right)^{1/\\beta}", num = "7.16",
            legend = c("R^*" = "wymagana niezawodność do chwili przeglądu", "t_{R^*}" = "najpóźniejszy termin przeglądu spełniający wymaganie")),
          "Wzór (7.16) otrzymujemy, rozwiązując równanie exp[−(t/η)^β] = R* ze wzoru (7.12): logarytmujemy obie strony, mnożymy przez −1 i podnosimy do potęgi 1/β. Dla β = 1 dostajemy t = −MTTF · ln R*, czyli termin w modelu wykładniczym.",
          risk_example("7.10", "Termin B10 dla wentylatora",
            problem = "Wentylator ma rozkład Weibulla z β = 2 i η = 1700 h. Wyznacz termin przeglądu, do którego zawiedzie co najwyżej 10% wentylatorów (B10). Powtórz dla 5% oraz dla modelu wykładniczego z MTTF = 1500 h.",
            steps = c(
              "Ze wzoru (7.16): t = 1700 · (−ln 0,90)^(1/2) = 1700 · √0,1054 ≈ 1700 · 0,325 ≈ 552 h.",
              "Dla R* = 0,95: t = 1700 · √0,0513 ≈ 385 h.",
              "Model wykładniczy: t = −1500 · ln 0,90 ≈ 1500 · 0,1054 ≈ 158 h."
            ),
            answer = "B10 ≈ 552 h, B5 ≈ 385 h. W modelu wykładniczym o podobnym MTTF B10 wynosi tylko 158 h — ale tam, jak zobaczymy, przegląd z wymianą niczego nie poprawia."
          ),
          risk_try("przesuwaj czas do przeglądu i znajdź największą wartość, dla której R(t) nie spada poniżej 0,90. Porównaj ją z wynikiem przykładu 7.10. Potem ustaw 1000 h i odczytaj ryzyko awarii."),
          figure_panel(label = "Decyzja", title = "Czy wentylator dotrwa do końca misji?", sliderInput("c7_plan_time", "Czas do przeglądu (h)", 100, 3000, 1000, 50), uiOutput("c7_plan"), full_width = TRUE),
          c(
            "Suwak ma krok 50 h, więc najbliższa wartość to 550 h: R ≈ 0,901, ryzyko awarii ≈ 0,099. Przy 600 h wymaganie jest już złamane. Przy domyślnych 1000 h ryzyko awarii przed przeglądem wynosi około 0,293 — prawie trzy razy więcej, niż dopuszcza kierownik. Przy 1500 h przekracza połowę (0,541).",
            "Zwróć uwagę, jak szybko rośnie ryzyko przy β = 2. Między 500 a 1000 h ryzyko wzrasta z około 0,083 do 0,293, czyli ponad trzykrotnie przy dwukrotnie dłuższym okresie. To bezpośrednia konsekwencja rosnącego hazardu: każda kolejna godzina jest groźniejsza od poprzedniej."
          )
        )
      ),
      list(
        id = "wymiana", title = "Czy wymiana profilaktyczna pomaga?",
        body = list(
          c(
            "Przegląd ma sens tylko wtedy, gdy coś zmienia: wykrywa zużycie i prowadzi do wymiany albo naprawy. Żeby ocenić, czy wymiana się opłaca, trzeba porównać przyszłość używanego egzemplarza z przyszłością nowego. Potrzebna jest niezawodność warunkowa — to samo pytanie, które we wzorze (7.9) zadaliśmy dla modelu wykładniczego, ale teraz dla dowolnego rozkładu."
          ),
          risk_formula("R(t\\mid s)=P(T>s+t\\mid T>s)=\\frac{R(s+t)}{R(s)}=e^{-[H(s+t)-H(s)]}", num = "7.17",
            legend = c("s" = "wiek elementu, który wciąż działa", "t" = "planowany dalszy czas pracy")),
          risk_example("7.11", "Stary czy nowy wentylator?",
            problem = "Wentylator przepracował 1000 h bez awarii. Oblicz prawdopodobieństwo, że przetrwa kolejne 500 h, i porównaj z nowym wentylatorem. Zrób to dla modelu Weibulla (β = 2, η = 1700 h) i dla modelu wykładniczego (MTTF = 1500 h).",
            steps = c(
              "Weibull, egzemplarz używany: ze wzoru (7.17) R(1500)/R(1000) = exp[−(1500/1700)² + (1000/1700)²] = exp(−0,779 + 0,346) = exp(−0,433) ≈ 0,649.",
              "Weibull, egzemplarz nowy: R(500) = exp[−(500/1700)²] ≈ 0,917.",
              "Model wykładniczy: z braku pamięci (7.9) oba prawdopodobieństwa są równe e^(−500/1500) ≈ 0,717."
            ),
            answer = "Przy zużyciu wymiana podnosi szansę przetrwania kolejnych 500 h z 0,649 do 0,917. Przy stałym hazardzie wymiana nie zmienia niczego: 0,717 przed i po."
          ),
          risk_check("c7_chk_wymiana",
            "Element ma stały hazard. Co da wymiana sprawnego egzemplarza na nowy po 1000 h pracy?",
            c("Znacząco zmniejszy ryzyko awarii w kolejnym okresie" = "helps", "Nie zmieni ryzyka awarii w kolejnym okresie" = "nothing", "Zwiększy ryzyko, bo nowy egzemplarz ma wczesne defekty" = "worse"),
            correct = "nothing",
            explanation = "Przy stałym hazardzie niezawodność warunkowa (7.17) nie zależy od wieku s: używany i nowy egzemplarz mają ten sam rozkład dalszego życia. Wymiana kosztuje, a ryzyka nie zmienia.",
            hints = c(helps = "Porównaj wyniki dla modelu wykładniczego w przykładzie 7.11.", worse = "W modelu wykładniczym nie ma wczesnych defektów — hazard jest stały od pierwszej godziny. Ten argument wymagałby hazardu malejącego.")
          ),
          "Rachunek pokazuje, dlaczego hipoteza o hazardzie musi poprzedzać plan. Jeśli dominującym mechanizmem jest zużycie, wymiana w okolicy B10 realnie obniża ryzyko. Jeśli dominują awarie losowe, lepiej wydać pieniądze na redundancję lub ochronę przed zaburzeniami. Jeśli dominują wczesne defekty, wymiana może wręcz zaszkodzić, bo wprowadza do parku nowe, jeszcze niesprawdzone egzemplarze."
        )
      )
    ),
    decision = "Podaj model, czas misji i prawdopodobieństwo dotrwania. Przegląd sam nie odnawia elementu: trzeba określić, co wykrywa i czy prowadzi do wymiany lub naprawy. MTTF samo nie wyznacza harmonogramu."
  ),
  list(
    id = "sciaga", title = "Ściąga i sprawdzenie",
    lead = "Czas → cenzorowanie → R(t) i h(t) → mechanizm → plan; interpretuj funkcje czasu życia bez estymacji parametrów.",
    intro = c(
      "Zanim przejdziesz do quizu, sprawdź, czy umiesz odpowiedzieć na pięć pytań poniżej dla dowolnego elementu ze swojego otoczenia — od baterii w laptopie po pasek rozrządu. To one, a nie wzory, są szkieletem analizy czasu życia.",
      "Quiz pyta o interpretacje — zwłaszcza o to, co naprawdę znaczy stały hazard — a ćwiczenia prowadzą od rachunku R(t) przez diagnozę cenzorowania po dobór kształtu Weibulla do mechanizmu."
    ),
    sections = list(
      list(
        id = "podsumowanie", title = "Podsumowanie",
        text = c(
          "Wykład zaczął się od dwóch wentylatorów o tym samym MTTF (7.1) i różnej niezawodności w horyzoncie 1000 h. Żeby zrozumieć tę różnicę, opisaliśmy czas życia czterema funkcjami: dystrybuantą, niezawodnością i gęstością (7.3) oraz hazardem (7.4), czyli ryzykiem najbliższej chwili dla elementu, który wciąż działa. Skumulowany hazard pozwala przejść od hipotezy o mechanizmie do niezawodności (7.6), a pole pod krzywą niezawodności daje MTTF (7.7). Dane o czasie życia są zwykle cenzorowane i elementów działających nie wolno z nich usuwać; najprostszy szacunek (7.2) wlicza ich czas pracy.",
          "Stały hazard wyznacza rozkład wykładniczy (7.8) — ciągłą wersję geometrycznego z wykładu 05 — i jego brak pamięci (7.9), odpowiednik wzoru (5.4). Suma k wykładniczych etapów daje rozkład gamma i Erlanga (7.10), a tożsamość (7.11) łączy czas do k-tego zdarzenia z liczbą zdarzeń Poissona, tak jak wzór (5.7) łączył ujemny dwumianowy z dwumianowym.",
          "Część B dodała parametr kształtu. W modelu Weibulla (7.12) kierunek zmiany hazardu (7.13) wyznacza β, a MTTF (7.14) zależy od obu parametrów — dlatego modele o tej samej średniej różnią się niezawodnością misji. Pełna krzywa wannowa wymaga sumy hazardów kilku mechanizmów (7.15). Plan przeglądu wynika z wymaganej niezawodności (7.16), a o sensie wymiany decyduje niezawodność warunkowa (7.17): przy zużyciu wymiana pomaga, przy stałym hazardzie nic nie zmienia."
        )
      ),
      list(id = "lista", title = "Pięć pytań", bullets = c("Co rozpoczyna i kończy czas życia?", "Jaki jest wspólny czas misji?", "Czy obserwacje działające są cenzorowane?", "Czy hazard jest stały, rośnie czy maleje?", "Jak wynik zmienia decyzję utrzymaniową?"), widget = risk_assessment_ui("c7", zycie_quiz, zycie_exercises)),
      list(id = "most", title = "Co dalej", text = "Dotąd badaliśmy pojedynczy element. W następnym wykładzie połączymy funkcje niezawodności R_i(t) kilku elementów w niezawodność całego systemu — i okaże się, że wynik zależy nie tylko od elementów, ale i od architektury.")
    )
  )
))
zycie_chapters <- risk_block_chapters(zycie_block)

zycie_server <- function(input, output, session) {
  v <- reactiveVal(FALSE)
  observeEvent(input$c7_vote_check, v(TRUE))
  output$c7_vote_feedback <- renderUI({
    req(v())
    if (is.null(input$c7_vote)) {
      return(lc_feedback(type = "info", "Najpierw zaznacz jedną z odpowiedzi."))
    }
    lc_feedback(type = if (identical(input$c7_vote, "distribution")) "ok" else "warning", tags$strong("Nie."), " Rozkłady o tym samym MTTF mogą mieć odmienne R(t).")
  })
  times <- c(220, 480, 760, 990, 1350, 1750, 2300, 3100)
  timeline_plot <- reactive({
    obs <- pmin(times, input$c7_follow)
    status <- ifelse(times <= input$c7_follow, "Awaria", "Nadal działa — cenzorowanie")
    dat <- data.frame(id = factor(seq_along(times)), obs, status)
    ggplot(dat, aes(x = 0, xend = obs, y = id, yend = id, colour = status)) +
      geom_segment(linewidth = 2) +
      geom_point(aes(x = obs, shape = status), size = 3) +
      scale_colour_manual(values = c("Awaria" = upwr_accent, "Nadal działa — cenzorowanie" = upwr_secondary)) +
      labs(title = "Każdy element wnosi informację", x = "Czas (h)", y = "Element", colour = NULL, shape = NULL) +
      theme_upwr()
  })
  zoom_plot_server("c7_timeline", timeline_plot, alt = "Osiem linii czasu zakończonych awarią lub znacznikiem cenzorowania.")
  output$c7_timeline_stats <- renderUI(lc_stat_grid(lc_stat_box("Awarie", sum(times <= input$c7_follow)), lc_stat_box("Cenzorowane", sum(times > input$c7_follow)), columns = 1))
  functions_plot <- reactive({
    t <- seq(0, 4000, length.out = 400)
    e <- risk_exponential(t, 1 / 1500)
    dat <- rbind(data.frame(t, value = e$density * 3000, fun = "f(t) × 3000"), data.frame(t, value = e$cdf, fun = "F(t)"), data.frame(t, value = e$reliability, fun = "R(t)"), data.frame(t, value = e$hazard * 1500, fun = "h(t) × 1500"))
    ggplot(dat, aes(t, value, colour = fun)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = input$c7_time, linetype = 2) +
      scale_colour_manual(values = upwr_cat_n(4)) +
      labs(title = "Cztery perspektywy na czas życia", x = "Czas (h)", y = "Wartość przeskalowana", colour = NULL) +
      theme_upwr()
  })
  zoom_plot_server("c7_functions", functions_plot, alt = "Cztery zsynchronizowane funkcje czasu życia ze wspólną linią czasu.")
  output$c7_functions_stats <- renderUI({
    e <- risk_exponential(input$c7_time, 1 / 1500)
    lc_stat_grid(lc_stat_box("F(t)", risk_format_probability(e$cdf)), lc_stat_box("R(t)", risk_format_probability(e$reliability), color = upwr_accent), columns = 1)
  })
  exp_plot <- reactive({
    t <- seq(0, 5000, length.out = 400)
    r <- risk_exponential(t, 1 / input$c7_mttf)$reliability
    ggplot(data.frame(t, r), aes(t, r)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      labs(title = "Niezawodność wykładnicza", x = "Czas (h)", y = "R(t)") +
      theme_upwr()
  })
  zoom_plot_server("c7_exp", exp_plot, alt = "Malejąca wykładnicza krzywa niezawodności.")
  output$c7_exp_stats <- renderUI(lc_stat_grid(lc_stat_box("R(1000 h)", risk_format_probability(exp(-1000 / input$c7_mttf)), color = upwr_accent), columns = 1))
  gamma_plot <- reactive({
    t <- seq(0, 6000, length.out = 400)
    ggplot(data.frame(t, p = dgamma(t, shape = input$c7_k, rate = 1 / 500)), aes(t, p)) +
      geom_line(colour = upwr_secondary, linewidth = 1.1) +
      labs(title = "Gęstość gamma: kształt k, skala 500 h", x = "Czas (h)", y = "Gęstość") +
      theme_upwr()
  })
  zoom_plot_server("c7_gamma", gamma_plot, alt = "Gęstość rozkładu gamma dla wybranego parametru kształtu.")
  output$c7_gamma_stats <- renderUI(lc_stat_grid(lc_stat_box("Średni czas E(T)", paste(input$c7_k * 500, "h")), lc_stat_box("Interpretacja kształtu", if (input$c7_k %% 1 == 0) paste0("Erlang: czas do ", input$c7_k, ". zdarzenia") else "ogólna gamma (bez etapów)"), columns = 1))
  weib_plot <- reactive({
    t <- seq(1, 5000, length.out = 500)
    w <- risk_weibull(t, input$c7_beta, input$c7_eta)
    dat <- rbind(data.frame(t, value = w$reliability, fun = "R(t)"), data.frame(t, value = w$hazard, fun = "Hazard h(t) [1/h]"))
    ggplot(dat, aes(t, value, colour = fun)) +
      geom_line(linewidth = 1.05) +
      scale_colour_manual(values = upwr_cat_n(2)) +
      facet_wrap(~fun, ncol = 1, scales = "free_y") +
      labs(title = "Niezawodność i hazard — osobne skale", x = "Czas (h)", y = NULL, colour = NULL) +
      theme_upwr()
  })
  zoom_plot_server("c7_weibull", weib_plot, alt = "Krzywe niezawodności i hazardu Weibulla na osobnych skalach pionowych, sterowane parametrami beta i eta.")
  output$c7_weibull_stats <- renderUI(lc_stat_grid(lc_stat_box("Kierunek hazardu", if (input$c7_beta < 1) "maleje" else if (input$c7_beta > 1) "rośnie" else "stały"), lc_stat_box("R(1000 h)", risk_format_probability(risk_weibull(1000, input$c7_beta, input$c7_eta)$reliability), color = upwr_accent), columns = 1))
  same_plot <- reactive({
    t <- seq(0, 3500, length.out = 400)
    shapes <- c(.7, 1, 2.5)
    scales <- 1500 / gamma(1 + 1 / shapes)
    dat <- do.call(rbind, lapply(seq_along(shapes), function(i) data.frame(t, r = exp(-(t / scales[i])^shapes[i]), model = paste0("β=", shapes[i]))))
    ggplot(dat, aes(t, r, colour = model)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = input$c7_mission, linetype = 2) +
      scale_colour_manual(values = upwr_cat_n(3)) +
      labs(title = "Ten sam MTTF, inne R(t)", x = "Czas (h)", y = "R(t)", colour = NULL) +
      theme_upwr()
  })
  zoom_plot_server("c7_same_mean", same_plot, alt = "Trzy krzywe Weibulla o tym samym średnim czasie życia i różnych kształtach.")
  output$c7_same_mean_stats <- renderUI(lc_feedback(type = "info", "Odczytaj trzy różne wartości na pionowej linii czasu misji."))
  bathtub_plot <- reactive({
    t <- seq(1, 4000, length.out = 500)
    early <- 1.2 * exp(-t / 350)
    stable <- rep(.12, length(t))
    wear <- input$c7_wear * (t / 4000)^3
    dat <- data.frame(t, early, stable, wear, total = early + stable + wear)
    long <- reshape(dat, varying = c("early", "stable", "wear", "total"), v.names = "hazard", timevar = "mechanizm", times = c("Wczesne defekty", "Losowe awarie", "Zużycie", "Suma"), direction = "long")
    ggplot(long, aes(t, hazard, colour = mechanizm)) +
      geom_line(aes(linewidth = mechanizm == "Suma")) +
      scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = .7), guide = "none") +
      scale_colour_manual(values = upwr_cat_n(4)) +
      labs(title = "Wanna jako suma mechanizmów", x = "Czas", y = "Względny hazard", colour = NULL) +
      theme_upwr()
  })
  zoom_plot_server("c7_bathtub", bathtub_plot, alt = "Krzywa hazardu w kształcie wanny i jej trzy składowe.")
  output$c7_bathtub_stats <- renderUI(lc_feedback(type = "warning", "Zmiana mechanizmu wymaga innej interwencji utrzymaniowej."))
  output$c7_plan <- renderUI({
    r <- risk_weibull(input$c7_plan_time, 2, 1700)$reliability
    lc_stat_grid(lc_stat_box("R(t)", risk_format_probability(r), color = upwr_accent), lc_stat_box("Ryzyko awarii", risk_format_probability(1 - r)), columns = 1)
  })
  risk_assessment_server("c7", zycie_quiz, input, output)
}
