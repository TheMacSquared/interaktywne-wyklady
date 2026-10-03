# Blok 03: Alarm i prawda -------------------------------------------------

alarm_quiz <- list(questions = list(
  list(
  question = "Czujnik ma czułość 95%. Czy po alarmie prawdopodobieństwo awarii wynosi 95%?",
  choices = c(
    "Nie — zależy także od częstości bazowej i fałszywych alarmów" = "no",
    "Tak — czułość jest odpowiedzią na to pytanie" = "yes",
    "Tak, jeśli awarie są rzadkie" = "rare"
  ),
  correct = "no",
  explanation = "Czułość to P(alarm | awaria), a pytanie po alarmie dotyczy P(awaria | alarm)."
),
  list(question = "Na 10 000 zmian: 100 awarii, czułość 0,95, FPR 0,05. Ile alarmów jest prawdziwych?",
    choices = c("95 z 100" = "a", "9500 z 10 000" = "b", "95 z 590" = "c"), correct = "c",
    explanation = "Jest 95 prawdziwych alarmów i 495 fałszywych, więc posterior wynosi 95/590≈0,161."),
  list(question = "Przy stałej czułości i FPR maleje częstość awarii. Co dzieje się z P(awaria | alarm)?",
    choices = c("Zawsze rośnie" = "a", "Maleje, jeśli FPR>0" = "b", "Nie zmienia się" = "c"), correct = "b",
    explanation = "Maleje udział prawdziwych alarmów wśród wszystkich alarmów."),
  list(question = "Kiedy wolno ponownie użyć Bayesa z tymi samymi parametrami drugiego czujnika?",
    choices = c("Gdy wyniki są niezależne warunkowo przy awarii i przy jej braku" = "a", "Gdy czujniki mają różne numery seryjne" = "b", "Gdy oba alarmy wystąpiły jednocześnie" = "c"), correct = "a",
    explanation = "Potrzebna jest niezależność warunkowa w obu stanach, a nie tylko niezależność bezwarunkowa."),
  list(question = "Reakcja kosztuje 100 zł i zapobiega stracie 2000 zł. Kiedy minimalizuje oczekiwany koszt?",
    choices = c("Dopiero powyżej 0,5" = "a", "Przy każdym dodatnim posteriorze" = "b", "Gdy posterior przekracza 0,05" = "c"), correct = "c",
    explanation = "Porównujemy 100 z 2000q; przy q=0,05 koszty są równe.")
))
alarm_exercises <- list(
  list(
    task = "Bananpol: dla 10 000 zmian, częstości awarii 0,01, czułości 0,95 i FPR 0,05 policz, ile alarmów będzie prawdziwych.",
    answer = c(
      "Awarie: 0,01 · 10 000 = 100 zmian; zmiany bez awarii: 9900.",
      "Prawdziwe alarmy: 0,95 · 100 = 95. Fałszywe alarmy: 0,05 · 9900 = 495. Wszystkich alarmów jest 95 + 495 = 590.",
      "Prawdziwych jest 95 z 590 alarmów, czyli P(awaria | alarm) = 95/590 ≈ 0,161 — zgodnie ze wzorem (3.4). Pięć na sześć alarmów jest fałszywych."
    )
  ),
  list(
    task = "Diagnostyka: wyjaśnij, dlaczego dwóch czujników z tym samym zasilaniem nie wolno automatycznie traktować jako niezależnych.",
    answer = c(
      "Wspólne zasilanie jest wspólną przyczyną wyników obu czujników. Spadek napięcia może wywołać fałszywy alarm na obu naraz, a utrata zasilania wycisza oba jednocześnie — także wtedy, gdy awaria trwa. Przy ustalonym stanie instalacji P(oba alarmy | stan) jest wtedy większe niż iloczyn P(alarm | stan) · P(alarm | stan), więc warunek z definicji 3.6 nie zachodzi.",
      "Skutek rachunkowy: wzór (3.7) mnoży iloraz wiarygodności przez siebie i zawyża posterior. W skrajnym przypadku, gdy drugi czujnik tylko powtarza pierwszy, drugi alarm nie wnosi żadnej informacji i posterior zostaje na poziomie 0,161 zamiast 0,785."
    )
  ),
  list(
    task = "Transfer: zaproponuj naturalne częstości dla testu przesiewowego w medycynie i nazwij właściwy mianownik.",
    answer = c(
      "Przykład: 1000 badanych, choroba u 1% (10 osób), czułość 0,90, FPR 0,09. Wynik dodatni: 9 chorych i około 89 zdrowych, razem około 98 osób.",
      "Pacjent z wynikiem dodatnim pyta o P(choroba | wynik dodatni), więc mianownikiem są wszystkie osoby z wynikiem dodatnim (około 98), a nie wszyscy chorzy (10) ani wszyscy badani (1000). Odpowiedź: 9/98 ≈ 0,09."
    )
  ),
  list(
    task = "Druga informacja o innej jakości: po alarmie czujnika (czułość 0,95, FPR 0,05, częstość awarii 0,01) dyżurny prosi operatora o odczyt ręczny termometru. Odczyt wskazuje przegrzanie w 80% zmian z awarią i w 10% zmian bez awarii. Przyjmij warunkową niezależność obu źródeł i oblicz posterior po alarmie i dodatnim odczycie ręcznym.",
    answer = c(
      "Szanse a priori: 0,01/0,99 = 1/99. Iloraz wiarygodności czujnika: 0,95/0,05 = 19; odczytu ręcznego: 0,80/0,10 = 8.",
      "Ze wzoru (3.7): szanse a posteriori = (1/99) · 19 · 8 = 152/99 ≈ 1,535.",
      "Prawdopodobieństwo: 1,535/(1 + 1,535) ≈ 0,606. Słabsze źródło (LR = 8) podnosi posterior z 0,161 do około 0,61 — mniej niż drugi taki sam czujnik (0,785), ale wciąż wyraźnie."
    )
  ),
  list(
    task = "Sprzeczne sygnały: pierwszy czujnik alarmuje, a drugi, identyczny i warunkowo niezależny, milczy. Jakie jest P(awaria) po obu wynikach? Zinterpretuj.",
    answer = c(
      "Iloraz wiarygodności braku alarmu: P(brak alarmu | awaria) / P(brak alarmu | brak awarii) = 0,05/0,95 = 1/19.",
      "Szanse a posteriori: (1/99) · 19 · (1/19) = 1/99, czyli prawdopodobieństwo 0,01 — dokładnie częstość bazowa.",
      "Przy identycznych czujnikach alarm i milczenie znoszą się: wracamy do stanu wiedzy sprzed obu odczytów. To nie znaczy, że awarii na pewno nie ma; znaczy, że sytuacja wymaga trzeciej, niezależnej informacji."
    )
  )
)

alarm_terms_table <- figure_panel(
  label = "Słownik",
  title = "Cztery liczby opisujące detektor",
  full_width = TRUE,
  lc_table(
    data.frame(
      name = c("Częstość bazowa", "Czułość", "Odsetek fałszywych alarmów", "Wiarygodność alarmu"),
      notation = c("P(awaria)", "P(alarm | awaria)", "P(alarm | brak awarii)", "P(awaria | alarm)"),
      denominator = c("wszystkie zmiany", "zmiany z awarią", "zmiany bez awarii", "wszystkie alarmy"),
      value = c("0,01", "0,95", "0,05", "wynik tego wykładu")
    ),
    cols = list(
      lc_col("name", "Nazwa", "row"),
      lc_col("notation", "Zapis warunkowy", "text"),
      lc_col("denominator", "Mianownik", "text"),
      lc_col("value", "W Bananpolu", "text")
    ),
    narrow = "cards"
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
  lc_status(
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
    lc_table(
      data.frame(
        krok = c("Pytanie", "Populacja", "Detektor", "Posterior", "Konsekwencje"),
        pytanie = c("Czy pytamy o P(alarm | awaria), czy o P(awaria | alarm)?", "W jakiej populacji zmian działa detektor?", "Skąd znamy czułość i FPR i czy są stabilne?", "Ile alarmów jest prawdziwych na 10 000 zmian?", "Co kosztuje reakcja, a co jej brak?"),
        typowy_blad = c("utożsamienie obu kierunków", "pominięcie częstości bazowej", "dane z innej populacji lub innych warunków", "raportowanie samego procentu bez liczebności", "automatyczne utożsamienie posterioru z decyzją")
      ),
      cols = list(
        lc_col("krok", "Krok", "row"),
        lc_col("pytanie", "Pytanie", "text"),
        lc_col("typowy_blad", "Typowy błąd", "text")
      ),
      narrow = "cards", prose = TRUE
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
      id = "intuicja", title = "Kierunek warunku", hook = "Dobry czujnik nie znaczy pewny alarm",
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
      sections = list(
        list(
          id = "pytanie", title = "Alarm na zmianie",
          body = list(
            c(
              "Awaria zdarza się średnio na 1 zmianie na 100. Czujnik wykrywa 95% awarii, ale w 5% zmian bez awarii alarmuje fałszywie. Najpierw oszacuj wiarygodność alarmu bez rachunku — zapisz swoją liczbę, zanim klikniesz dalej.",
              "To pytanie ma długą historię błędnych odpowiedzi: w badaniach z udziałem lekarzy interpretujących wyniki testów przesiewowych większość podawała wartości bliskie „około 95%”, choć poprawna odpowiedź wynosiła kilka procent — liczbę opisującą test brano za wiarygodność wyniku dodatniego. Zaraz sprawdzisz, po której stronie tej statystyki jesteś."
            ),
            risk_try("wybierz jedną odpowiedź, zanim zaczniesz liczyć, i kliknij „Sprawdź intuicję”. Zapamiętaj swój wybór — wrócimy do niego w rozdziale o naturalnych częstościach."),
            risk_vote_panel(
              "a3_vote", "a3_vote_feedback",
              "Po alarmie: jak duża jest szansa rzeczywistej awarii?",
              c("Około 95%" = "95", "Około 16%" = "16", "Nie da się określić bez częstości bazowej" = "base")
            ),
            c(
              "Odpowiedź „około 95%” jest kusząca, bo 95 to jedyna duża liczba w opisie czujnika i brzmi jak jego „skuteczność”. Ale 95% opisuje zachowanie czujnika w świecie, w którym awaria już nastąpiła. Dyżurny nie wie, w którym świecie jest — wie tylko, że zadzwonił telefon. Liczba, której potrzebuje, musi uwzględniać oba światy naraz: ten z awarią i ten, w którym awarii nie ma, a czujnik i tak alarmuje.",
              "Poprawny wynik — około 16% — zaskakuje niemal każdego, kto widzi go pierwszy raz. W tym wykładzie wyprowadzimy go trzema drogami: z tablicy 2×2, z naturalnych częstości i ze wzoru Bayesa. Wszystkie trzy dają tę samą liczbę, bo opisują ten sam rachunek w różnym zapisie."
            )
          )
        ),
        list(
          id = "kierunek", title = "Dwa kierunki jednego warunku",
          body = list(
            c(
              "Zapis P(A | B) z wykładu 02 (wzór 2.1) czytamy: prawdopodobieństwo A wśród przypadków, w których zaszło B. Warunek wskazuje mianownik. Jeśli zamienimy miejscami A i B, zmieni się mianownik — a wraz z nim cała liczba. P(alarm | awaria) liczymy wśród zmian z awarią; P(awaria | alarm) liczymy wśród zmian z alarmem. Obie liczby mają ten sam licznik: zmiany, na których wystąpiły jednocześnie awaria i alarm.",
              "Różnica jest łatwa do zobaczenia na przykładzie spoza przemysłu. Prawie każdy zawodowy koszykarz jest wysoki, ale wśród wysokich ludzi koszykarzy jest znikomy odsetek. P(wysoki | koszykarz) jest bliskie 1, a P(koszykarz | wysoki) bliskie 0. Nikt nie pomyli tych liczb w przypadku koszykarzy; przy czujnikach i testach mylimy je nagminnie, bo obie brzmią jak „skuteczność”."
            ),
            risk_example("3.1", "Ten sam licznik, dwa mianowniki",
              problem = "W dzienniku starszej dojrzewalni Bananpolu z 1000 zmian zanotowano 20 awarii. Czujnik alarmował w 19 z nich oraz w 49 z 980 zmian bez awarii. Oblicz P(alarm | awaria) i P(awaria | alarm).",
              steps = c(
                "P(alarm | awaria): mianownikiem są zmiany z awarią (20), licznikiem zmiany z awarią i alarmem (19). Wynik: 19/20 = 0,95.",
                "P(awaria | alarm): mianownikiem są wszystkie zmiany z alarmem, czyli 19 + 49 = 68. Licznik jest ten sam (19). Wynik: 19/68 ≈ 0,279.",
                "Licznik się nie zmienił; zmienił się mianownik — z 20 na 68, bo do alarmów dochodzą 49 fałszywych alarmów ze zmian bez awarii."
              ),
              answer = "0,95 i około 0,28. Te same dane dają dwie bardzo różne liczby, zależnie od tego, który warunek stoi za kreską."
            ),
            risk_check("d3_chk_kierunek",
              "Raport serwisu mówi: „w 95% awarii czujnik zaalarmował”. Którą wielkość podaje?",
              c("P(alarm | awaria)" = "sens", "P(awaria | alarm)" = "ppv", "P(awaria i alarm)" = "joint"),
              correct = "sens",
              explanation = "Mianownikiem są awarie („w 95% awarii”), a pytamy o alarm. To P(alarm | awaria) — czułość, o której będzie mowa w następnym rozdziale.",
              hints = c(
                ppv = "Sprawdź, wśród jakich przypadków liczono 95%. Czy wśród alarmów, czy wśród awarii?",
                joint = "Prawdopodobieństwo łączne ma za mianownik wszystkie zmiany. Tu mianownikiem są tylko zmiany z awarią."
              )
            ),
            "Od tej chwili przy każdej liczbie opisującej detektor będziemy pytać: jaki jest jej mianownik? To jedno pytanie wystarcza, żeby uniknąć najczęstszego błędu tego wykładu."
          )
        )
      )
    ),
    list(
      id = "detektor", title = "Czułość i swoistość", hook = "Alarm może się pomylić na dwa sposoby",
      lead = "Czułość i odsetek fałszywych alarmów opisują dwa różne wiersze tablicy.",
      intro = c(
        "Zanim policzymy cokolwiek, uporządkujmy słownik. Każda zmiana pracy dojrzewalni kończy się jednym z czterech wyników: awaria z alarmem albo bez, brak awarii z alarmem albo bez. Cała wiedza o detektorze mieści się w tym, jak często trafia do każdej z czterech komórek.",
        "Zwróć uwagę, że czułość i odsetek fałszywych alarmów mają różne mianowniki: pierwsza jest liczona wśród zmian z awarią, drugi wśród zmian bez awarii. Właśnie dlatego nie można ich dodawać, odejmować ani porównywać wprost — to częstości z dwóch różnych światów."
      ),
      sections = list(
        list(
          id = "definicje", title = "Cztery wyniki",
          body = list(
            risk_confusion_matrix(),
            "Cztery wyniki układają się w tablicę o dwóch wierszach (stan instalacji) i dwóch kolumnach (wynik detektora). W analizie ryzyka i w diagnostyce medycznej przyjęła się ta sama konwencja zapisu: A oznacza zdarzenie, którego szukamy (tu: awarię), ¬A jego brak, „+” wynik dodatni detektora (alarm), a „−” wynik ujemny (brak alarmu).",
            risk_definition("3.1", "Tablica wyników detektora", c(
              "Tablica wyników detektora to tablica 2×2, której wiersze odpowiadają stanowi rzeczywistemu (A albo ¬A), a kolumny wynikowi detektora (+ albo −). Jej komórki to liczby lub prawdopodobieństwa wyników: prawdziwie dodatnich TP (A i +), fałszywie ujemnych FN (A i −), fałszywie dodatnich FP (¬A i +) oraz prawdziwie ujemnych TN (¬A i −).",
              "Suma wiersza A to liczba zdarzeń, suma wiersza ¬A — liczba przypadków bez zdarzenia, a suma kolumny + — liczba wszystkich alarmów."
            )),
            "Detektor opisujemy, dzieląc komórki przez sumy wierszy, bo producent testuje czujnik w warunkach, w których wie, czy awaria wystąpiła. Dwa takie ilorazy wystarczają, żeby opisać zachowanie detektora w obu stanach instalacji.",
            risk_definition("3.2", "Czułość i swoistość", c(
              "Czułość (ang. sensitivity) to P(+ | A): prawdopodobieństwo alarmu, gdy zdarzenie rzeczywiście zachodzi. Swoistość (ang. specificity) to P(− | ¬A): prawdopodobieństwo braku alarmu, gdy zdarzenia nie ma.",
              "Odsetek fałszywych alarmów FPR (ang. false positive rate) to P(+ | ¬A) = 1 − swoistość. Obie wielkości opisują detektor, a nie populację, w której pracuje."
            )),
            risk_formula("\\begin{aligned}\\text{czułość} &= P(+\\mid A)=\\frac{TP}{TP+FN},\\\\[0.6em] \\text{swoistość} &= P(-\\mid \\neg A)=\\frac{TN}{TN+FP}\\end{aligned}", num = "3.1",
              legend = c("TP" = "liczba prawdziwie dodatnich", "FN" = "liczba fałszywie ujemnych", "TN" = "liczba prawdziwie ujemnych", "FP" = "liczba fałszywie dodatnich")),
            risk_formula("\\text{FPR}=P(+\\mid \\neg A)=\\frac{FP}{TN+FP}=1-\\text{swoistość}", num = "3.2"),
            c(
              "Trzecia potrzebna liczba nie dotyczy detektora, lecz miejsca, w którym go zamontowano. Ten sam czujnik w hali z nowymi wentylatorami i w hali ze sprzętem po dwudziestu latach eksploatacji będzie miał identyczną czułość i swoistość, ale zupełnie inną liczbę awarii do wykrycia."
            ),
            risk_definition("3.3", "Częstość bazowa", c(
              "Częstość bazowa (prawdopodobieństwo a priori) to P(A): udział przypadków ze zdarzeniem w populacji, w której działa detektor, oceniony przed poznaniem wyniku detektora. W Bananpolu jest to udział zmian z awarią wśród wszystkich zmian danej hali."
            ))
          )
        ),
        list(
          id = "kampania", title = "Skąd znamy czułość i FPR",
          body = list(
            c(
              "Czułość i FPR szacuje się w kampanii testowej: badamy detektor na przypadkach, o których wiemy, czy zdarzenie zaszło. Ponieważ prawdziwe awarie są rzadkie, w teście celowo wywołuje się wiele awarii — inaczej trzeba by czekać latami na kilka przypadków. To rozsądne przy szacowaniu czułości, ale ma skutek uboczny: proporcja awarii w kampanii testowej jest ustalona przez planującego test, a nie przez świat.",
              "Dlatego z tablicy kampanii testowej wolno liczyć ilorazy w wierszach (czułość, swoistość, FPR), ale nie wolno liczyć ilorazów w kolumnach. Udział prawdziwych alarmów wśród alarmów zależy od częstości bazowej, a ta w kampanii jest sztuczna."
            ),
            risk_example("3.2", "Kampania testowa czujnika",
              problem = list(
                "Przed montażem dział utrzymania ruchu przetestował czujnik. W 60 zmianach z celowo wywołanym przegrzaniem alarm wystąpił 57 razy. W 400 zmianach normalnej pracy alarm wystąpił 20 razy.",
                risk_parts(
                  "Oblicz czułość, swoistość i FPR.",
                  "Producent pisze w ulotce: „77 alarmów, z czego 57 prawdziwych — 74% trafności”. Czy dyżurny w hali z częstością awarii 0,01 może przyjąć, że alarm jest prawdziwy z prawdopodobieństwem 0,74?"
                )
              ),
              steps = c(
                "Ze wzoru (3.1): czułość = 57/60 = 0,95; swoistość = (400 − 20)/400 = 380/400 = 0,95. Ze wzoru (3.2): FPR = 20/400 = 0,05.",
                "Iloraz 57/77 ≈ 0,74 jest liczony w kolumnie „alarm”. Jego wartość zależy od tego, ile awarii było w teście: 60 na 460 zmian, czyli około 0,13. W hali Bananpolu awarie zdarzają się na 0,01 zmian — trzynaście razy rzadziej niż w teście. Fałszywych alarmów będzie więc proporcjonalnie znacznie więcej; w rozdziale 3 policzymy, że wiarygodność alarmu wynosi tam około 0,16."
              ),
              steps_type = "a",
              answer = "(a) Czułość 0,95, swoistość 0,95, FPR 0,05. (b) Nie. Liczba 0,74 opisuje kampanię testową, w której awarie wywołano sztucznie często; w hali trzeba ją przeliczyć z częstością bazową 0,01."
            ),
            risk_check("d3_chk_mianownik",
              "Serwis chce oszacować FPR czujnika z danych eksploatacyjnych. Który iloraz jest właściwy?",
              c(
                "fałszywe alarmy / wszystkie zmiany bez awarii" = "fpr",
                "fałszywe alarmy / wszystkie alarmy" = "fdr",
                "fałszywe alarmy / wszystkie zmiany" = "all"
              ),
              correct = "fpr",
              explanation = "FPR = P(+ | ¬A), więc mianownikiem są zmiany bez awarii — wiersz ¬A tablicy (wzór 3.2). Iloraz fałszywych alarmów do wszystkich alarmów to zupełnie inna wielkość, zależna od częstości bazowej.",
              hints = c(
                fdr = "To udział fałszywych wśród alarmów, czyli kolumna tablicy. FPR jest liczony w wierszu.",
                all = "Wszystkie zmiany obejmują też zmiany z awarią, na których fałszywy alarm nie może wystąpić."
              )
            )
          )
        ),
        list(
          id = "tablica", title = "Tablica 2×2 dla 10 000 zmian",
          body = list(
            "Znając czułość, FPR i częstość bazową, możemy odtworzyć oczekiwaną tablicę wyników dla dowolnej liczby zmian. Najpierw dzielimy zmiany na wiersze według częstości bazowej, potem każdy wiersz na kolumny według właściwego parametru detektora: wiersz A według czułości, wiersz ¬A według FPR.",
            risk_try("zacznij od ustawień domyślnych (częstość 0,01, czułość 0,95, FPR 0,05) i odczytaj kolumnę alarmów. Następnie zwiększ częstość awarii do 0,05 i porównaj liczbę prawdziwych i fałszywych alarmów. Na koniec wróć do 0,01 i zmniejszaj FPR."),
            alarm_terms_table,
            figure_panel(
              label = "Tablica 2×2", title = "Zmień parametry detektora",
              lc_p("Liczebności są zaokrągloną ilustracją dla 10 000 zmian. Prawdopodobieństwa obliczamy bezpośrednio z parametrów modelu."),
              lc_toolbar(
                lc_slider("a3_prev", "Częstość awarii", 0.001, 0.10, 0.01, 0.001),
                lc_slider("a3_sens", "Czułość", 0.50, 1, 0.95, 0.01),
                lc_slider("a3_fpr", "Fałszywie dodatnie", 0, 0.30, 0.05, 0.01)
              ),
              uiOutput("a3_table"),
              full_width = TRUE
            ),
            c(
              "Przy ustawieniach domyślnych wiersz awarii ma 100 zmian: 95 z alarmem i 5 bez. Wiersz bez awarii ma 9900 zmian: 495 z alarmem i 9405 bez. Kolumna alarmów zawiera więc 95 + 495 = 590 zmian, z których prawdziwych jest mniej niż jedna szósta. Czujnik jest dobry w obu wierszach — myli się w 5% przypadków — a mimo to w kolumnie alarmów przeważają pomyłki, bo wiersz bez awarii jest 99 razy liczniejszy.",
              "Przy częstości awarii 0,05 tablica zmienia się zasadniczo: 475 prawdziwych i 475 fałszywych alarmów, czyli dokładnie pół na pół. Detektor się nie zmienił; zmieniła się proporcja wierszy. Zmniejszanie FPR działa w tym samym kierunku: każdy punkt procentowy FPR to w tej hali 99 fałszywych alarmów na 10 000 zmian."
            )
          )
        )
      ),
      pitfall = "Wysoka czułość nie oznacza, że większość alarmów jest prawdziwa."
    ),
    list(
      id = "czestosci", title = "Wzór Bayesa", hook = "Dziesięć tysięcy zmian mówi więcej niż procenty",
      lead = "Zamiast trzech procentów śledzimy konkretne zmiany produkcyjne; wzór porządkuje ten rachunek na końcu.",
      intro = c(
        "Trzy procenty naraz — częstość bazowa, czułość, FPR — przeciążają intuicję, bo każdy odnosi się do innego mianownika. Naturalne częstości rozbrajają problem: zamiast ułamków wyobrażamy sobie 10 000 konkretnych zmian i śledzimy, ile z nich trafia do każdej grupy.",
        "Na siatce poniżej każde pole to jeden alarm z 10 000 zmian. Widać od razu to, co ukrywają procenty: zmian bez awarii jest tak dużo, że nawet rzadkie fałszywe alarmy tworzą tłum liczniejszy niż wszystkie prawdziwe alarmy razem wzięte."
      ),
      sections = list(
        list(
          id = "mianownik", title = "Wszystkie alarmy",
          text = "Dla pytania po alarmie mianownikiem są prawdziwe i fałszywe alarmy razem. Dyżurny nie wie, z której grupy pochodzi jego telefon — wie tylko, że alarm jest. Dlatego wiarygodność alarmu to udział prawdziwych alarmów wśród wszystkich alarmów, a nie wśród awarii.",
          body = list(
            risk_definition("3.4", "Wartości predykcyjne", c(
              "Wartość predykcyjna dodatnia PPV (ang. positive predictive value) to P(A | +): prawdopodobieństwo, że zdarzenie zachodzi, gdy detektor alarmuje. W tym wykładzie nazywamy ją też wiarygodnością alarmu lub posteriorem po alarmie.",
              "Wartość predykcyjna ujemna NPV (ang. negative predictive value) to P(¬A | −): prawdopodobieństwo, że zdarzenia nie ma, gdy detektor milczy. W przeciwieństwie do czułości i swoistości obie wartości predykcyjne zależą od częstości bazowej."
            )),
            "W tablicy 2×2 wartości predykcyjne to ilorazy w kolumnach: PPV = TP/(TP + FP), NPV = TN/(TN + FN). Czułość i swoistość czytamy wierszami, wartości predykcyjne — kolumnami. Siatka poniżej pokazuje, dlaczego kolumna alarmów wygląda tak niekorzystnie.",
            risk_try("odczytaj liczbę prawdziwych i fałszywych alarmów przy ustawieniach domyślnych i znajdź je na siatce. Potem w tablicy 2×2 z poprzedniego rozdziału zmniejsz FPR do 0,01 i wróć tutaj — parametry obu widoków są wspólne."),
            risk_widget_panel("Symulacja", "10 000 zmian Bananpolu", tagList(
              p("Parametry są synchronizowane z tablicą 2×2."), uiOutput("a3_counts")
            ),
            plot_id = "a3_grid", ratio = "1.9/1", max_height = "470px"
            ),
            c(
              "Przy ustawieniach domyślnych panel pokazuje 95 prawdziwych i 495 fałszywych alarmów, a P(awaria | alarm) = 0,161. Na siatce prawdziwe alarmy to niewielka grupa pól, a fałszywe — obszar pięć razy większy. Pozostałe zmiany, bez alarmu, leżą poza siatką. Przy FPR = 0,01 fałszywych alarmów jest 99, prawdziwych nadal 95, a wiarygodność alarmu rośnie do 0,490.",
              "Wniosek jest praktyczny: przy rzadkich awariach o wiarygodności alarmu decyduje przede wszystkim FPR, a nie czułość. Podniesienie czułości z 0,95 do 1 dodałoby pięć prawdziwych alarmów; obniżenie FPR o jeden punkt procentowy usuwa 99 fałszywych."
            ),
            risk_check("d3_chk_ppv",
              "W hali o częstości awarii 0,01 porównujemy detektor A (czułość 0,95, FPR 0,05) z detektorem B (czułość 0,80, FPR 0,01). Który ma wyższą wiarygodność alarmu P(awaria | alarm)?",
              c("Detektor A, bo ma wyższą czułość" = "a", "Detektor B" = "b", "Oba jednakową, bo działają w tej samej hali" = "same"),
              correct = "b",
              explanation = "Na 10 000 zmian: A daje 95 prawdziwych i 495 fałszywych alarmów (PPV ≈ 0,161), B daje 80 prawdziwych i 99 fałszywych (PPV = 80/179 ≈ 0,447). Ceną jest 20 przeoczonych awarii na 100 zamiast 5 — wybór detektora to kompromis, nie ranking.",
              hints = c(
                a = "Policz fałszywe alarmy obu detektorów wśród 9900 zmian bez awarii.",
                same = "Ta sama hala oznacza tę samą częstość bazową, ale detektory mają różne FPR."
              )
            )
          )
        ),
        list(
          id = "drogi", title = "Od drzewa do Bayesa",
          text = c(
            "Wzór przyjdzie na końcu — najpierw jeszcze raz przejdźmy drogę na konkretnych zmianach. Alarm może powstać na dwóch rozłącznych drogach: po awarii (droga przez czułość) albo bez awarii (droga przez fałszywe alarmy). Wiarygodność alarmu to udział pierwszej drogi w sumie obu — policzmy go krok po kroku na 10 000 zmian.",
            "O wyniku decydują względne szerokości obu dróg: jeśli droga fałszywa jest szersza od prawdziwej, większość alarmów jest fałszywa — niezależnie od tego, jak dobra jest czułość."
          ),
          body = list(
            alarm_paths_widget,
            lc_p("Iloraz, który właśnie policzyliśmy — jedna droga podzielona przez sumę wszystkich dróg kończących się alarmem — ma swoją nazwę i ogólny zapis. Wyprowadzimy go z trzech narzędzi wykładu 02: definicji prawdopodobieństwa warunkowego (2.1), reguły mnożenia (2.2) i wzoru na prawdopodobieństwo całkowite (2.4)."),
            c(
              "Krok pierwszy: z definicji prawdopodobieństwa warunkowego P(A | +) = P(A ∩ +) / P(+). Licznik to prawdopodobieństwo, że zmiana leży na drodze „awaria i alarm”; mianownik — że kończy się alarmem.",
              "Krok drugi: licznika nie znamy wprost, ale znamy czułość. Reguła mnożenia daje P(A ∩ +) = P(A) · P(+ | A): idziemy po drzewie najpierw gałęzią „awaria”, potem gałęzią „alarm”. W Bananpolu: 0,01 · 0,95 = 0,0095.",
              "Krok trzeci: mianownik rozkładamy na dwie rozłączne drogi. Zdarzenia A i ¬A tworzą podział wszystkich zmian, więc wzór na prawdopodobieństwo całkowite daje:"
            ),
            risk_formula("P(+)=P(+\\mid A)\\,P(A)+P(+\\mid \\neg A)\\,P(\\neg A)", num = "3.3",
              legend = c("P(+)" = "prawdopodobieństwo alarmu na losowej zmianie", "P(+\\mid A)" = "czułość", "P(+\\mid \\neg A)" = "FPR", "P(A)" = "częstość bazowa")),
            "W Bananpolu P(+) = 0,95 · 0,01 + 0,05 · 0,99 = 0,0095 + 0,0495 = 0,059, czyli 590 alarmów na 10 000 zmian. Podstawiając licznik z kroku drugiego i mianownik (3.3) do definicji, dostajemy wzór Bayesa.",
            risk_formula("P(A\\mid +)=\\frac{P(+\\mid A)\\,P(A)}{P(+\\mid A)\\,P(A)+P(+\\mid \\neg A)\\,P(\\neg A)}", num = "3.4",
              legend = c("P(A\\mid +)" = "wiarygodność alarmu (PPV, posterior)", "P(+\\mid A)\\,P(A)" = "droga przez awarię — licznik", "P(+\\mid \\neg A)\\,P(\\neg A)" = "droga przez fałszywe alarmy")),
            lc_p("Licznik jest drogą przez awarię; mianownik sumą obu dróg kończących się alarmem."),
            lc_p(
              "Wzór Bayesa nie wnosi nowej matematyki — porządkuje rachunek, który
               wykonaliśmy na zmianach. Warto rozpoznać w mianowniku starego znajomego:
               to wzór na prawdopodobieństwo całkowite z poprzedniego wykładu,
               zastosowany do zdarzenia „alarm”. Nowa jest tylko nazwa."
            ),
            risk_derivation("wzór Bayesa w trzech linijkach", c(
              "Całe wyprowadzenie to połączenie definicji z wykładu 02. Każda linijka odpowiada jednemu krokowi z tekstu."
            ), lines = c(
              "P(A | +) = P(A ∩ +) / P(+)                          (definicja warunku)",
              "P(A ∩ +) = P(+ | A) · P(A)                          (reguła mnożenia)",
              "P(+) = P(+ | A) · P(A) + P(+ | ¬A) · P(¬A)          (prawdopodobieństwo całkowite, 3.3)",
              "P(A | +) = 0,0095 / (0,0095 + 0,0495) = 0,0095 / 0,059 ≈ 0,161"
            )),
            c(
              "Ten sam schemat daje wartość predykcyjną ujemną. Dyżurny, który przez całą zmianę nie dostał alarmu, też ma pytanie: czy mogę spokojnie spać? Zamieniamy w rachunku „+” na „−”, czułość na 1 − czułość, a FPR na swoistość."
            ),
            risk_formula("P(\\neg A\\mid -)=\\frac{P(-\\mid \\neg A)\\,P(\\neg A)}{P(-\\mid \\neg A)\\,P(\\neg A)+P(-\\mid A)\\,P(A)}", num = "3.5",
              legend = c("P(-\\mid \\neg A)" = "swoistość", "P(-\\mid A)" = "1 − czułość, czyli odsetek przeoczonych awarii")),
            risk_example("3.3", "Czy cisza uspokaja?",
              problem = "Przy częstości awarii 0,01, czułości 0,95 i FPR 0,05 oblicz NPV oraz prawdopodobieństwo awarii na zmianie, na której czujnik milczał. Porównaj je z częstością bazową.",
              steps = c(
                "Naturalne częstości: z 10 000 zmian bez alarmu jest 5 zmian z awarią (przeoczonych) i 9405 zmian bez awarii, razem 9410.",
                "Ze wzoru (3.5): NPV = 9405/9410 = (0,95 · 0,99)/(0,95 · 0,99 + 0,05 · 0,01) ≈ 0,9995.",
                "P(awaria | brak alarmu) = 1 − NPV = 5/9410 ≈ 0,00053.",
                "Częstość bazowa wynosi 0,01, czyli prawie dziewiętnaście razy więcej: 0,01/0,00053 ≈ 18,8."
              ),
              answer = "NPV ≈ 0,9995. Brak alarmu obniża szansę awarii z 0,01 do około 0,0005 — cisza jest bardzo wiarygodna, alarm znacznie mniej. Asymetria wynika z częstości bazowej, a nie z tego, że czujnik „lepiej milczy, niż alarmuje”: czułość i swoistość są tu równe."
            )
          ),
          decision = "Komunikuj posterior wraz z liczebnościami, a nie samą czułość."
        )
      )
    ),
    list(
      id = "baza", title = "Częstość bazowa", hook = "Ten sam alarm znaczy co innego w innej hali",
      lead = "Ten sam czujnik daje inną wiarygodność alarmu w innej populacji, a drugi alarm pomaga tylko o tyle, o ile wnosi nową informację.",
      intro = c(
        "Wiarygodność alarmu nie jest cechą czujnika — jest cechą pary: czujnik plus populacja, w której pracuje. Ten sam model detektora zamontowany w hali o rzadkich awariach będzie „krzyczał wilk” znacznie częściej niż w hali, gdzie awarie są powszechne.",
        "Krzywa poniżej pokazuje tę zależność w całym zakresie. Zauważ, jak stromo rośnie na początku: przy bardzo rzadkich awariach niewielka zmiana częstości bazowej silnie zmienia sens alarmu. To dlatego przenoszenie parametrów detektora między instalacjami bez sprawdzenia częstości bazowej jest błędem, a nie oszczędnością."
      ),
      sections = list(
        list(
          id = "krzywa", title = "Pułapka częstości bazowej",
          body = list(
            risk_try("odczytaj wartość w panelu dla częstości 0,01 (przerywana linia na wykresie). Potem zmniejsz FPR do 0,01 i sprawdź, jak zmienia się ta wartość i kształt krzywej przy lewej krawędzi. Na koniec przywróć FPR 0,05 i zmniejsz czułość do 0,80."),
            risk_widget_panel(
              "Krzywa", "P(awaria | alarm) a częstość bazowa",
              tagList(
                lc_slider("a3_curve_sens", "Czułość", 0.5, 1, 0.95, 0.01),
                lc_slider("a3_curve_fpr", "FPR", 0.001, 0.20, 0.05, 0.001)
              ),
              "a3_curve", "a3_posterior"
            ),
            c(
              "Przy czułości 0,95 i FPR 0,05 krzywa przechodzi przez około 0,019 dla częstości 0,001, przez 0,161 dla 0,01, przez 0,5 dla 0,05 i przez około 0,83 dla 0,2. Między częstością 0,001 a 0,01 posterior rośnie ponad ośmiokrotnie; między 0,1 a 0,2 — już tylko z 0,68 do 0,83. Przy FPR 0,01 wartość dla częstości 0,01 skacze do 0,490, a przy czułości 0,80 (i FPR 0,05) spada tylko do 0,139. Lewa część krzywej jest wrażliwa na FPR, prawie wcale na czułość.",
              "Tę stromość najłatwiej zrozumieć, gdy zamiast prawdopodobieństw użyjemy szans. Szanse zdarzenia to iloraz P(A)/P(¬A): częstość 0,01 odpowiada szansom 1 : 99, częstość 0,05 — szansom 1 : 19. Dzieląc wzór Bayesa (3.4) dla A przez ten sam wzór dla ¬A, skracamy wspólny mianownik P(+) i dostajemy bardzo prostą zależność."
            ),
            risk_definition("3.5", "Iloraz wiarygodności", c(
              "Iloraz wiarygodności wyniku dodatniego to LR₊ = P(+ | A) / P(+ | ¬A) = czułość / FPR. Mówi, ile razy częściej alarm pojawia się przy awarii niż bez niej.",
              "Analogicznie iloraz wiarygodności wyniku ujemnego to LR₋ = P(− | A) / P(− | ¬A) = (1 − czułość) / swoistość. LR₊ > 1 podnosi przekonanie o zdarzeniu, LR₋ < 1 je obniża."
            )),
            risk_formula("\\frac{P(A\\mid +)}{P(\\neg A\\mid +)}=\\frac{P(+\\mid A)}{P(+\\mid \\neg A)}\\cdot\\frac{P(A)}{P(\\neg A)}", num = "3.6",
              legend = c("\\frac{P(A)}{P(\\neg A)}" = "szanse a priori", "\\frac{P(+\\mid A)}{P(+\\mid \\neg A)}" = "iloraz wiarygodności LR₊", "\\frac{P(A\\mid +)}{P(\\neg A\\mid +)}" = "szanse a posteriori")),
            "Wzór (3.6) mówi: szanse po alarmie = szanse przed alarmem · LR₊. Czujnik Bananpolu ma LR₊ = 0,95/0,05 = 19 — każdy alarm mnoży szanse awarii przez 19, niezależnie od hali. Jeśli szanse wyjściowe są maleńkie, nawet dziewiętnastokrotny wzrost daje mało; stąd stromość krzywej po lewej stronie.",
            risk_example("3.4", "Ten sam czujnik w dwóch halach",
              problem = "Czujnik (czułość 0,95, FPR 0,05) pracuje w hali A, gdzie awaria zdarza się na 0,01 zmian, i w hali B ze starszym sprzętem, gdzie awaria zdarza się na 0,05 zmian. Oblicz wiarygodność alarmu w obu halach metodą szans.",
              steps = c(
                "LR₊ = 0,95/0,05 = 19 w obu halach — to cecha czujnika.",
                "Hala A: szanse a priori 0,01/0,99 = 1/99. Ze wzoru (3.6) szanse a posteriori = 19/99. Prawdopodobieństwo: 19/(19 + 99) = 19/118 ≈ 0,161.",
                "Hala B: szanse a priori 0,05/0,95 = 1/19. Szanse a posteriori = 19 · 1/19 = 1, czyli 1 : 1. Prawdopodobieństwo: 1/(1 + 1) = 0,5.",
                "Kontrola wzorem (3.4) dla hali B: 0,95 · 0,05/(0,95 · 0,05 + 0,05 · 0,95) = 0,0475/0,095 = 0,5."
              ),
              answer = "Hala A: około 0,16; hala B: 0,5. Pięciokrotnie wyższa częstość bazowa daje ponad trzykrotnie wyższą wiarygodność alarmu przy identycznym czujniku."
            ),
            risk_check("d3_chk_baza",
              "Ten sam czujnik (LR₊ = 19) zamontowano w hali, w której awaria zdarza się na 0,1 zmian. Jaka jest wiarygodność alarmu?",
              c("Około 0,95 — jak czułość" = "a", "Około 0,68" = "b", "Około 0,16 — jak w hali Bananpolu" = "c"),
              correct = "b",
              explanation = "Szanse a priori 0,1/0,9 = 1/9; po alarmie 19/9. Prawdopodobieństwo 19/(19 + 9) = 19/28 ≈ 0,68. Ten sam czujnik, inna hala, inna wiarygodność alarmu.",
              hints = c(
                a = "Czułość to P(alarm | awaria). Pytamy o kierunek odwrotny — użyj wzoru (3.6).",
                c = "0,16 odpowiada częstości 0,01. Tu częstość jest dziesięć razy wyższa."
              )
            )
          ),
          pitfall = "Porównywanie czujników bez podania populacji zastosowania bywa pozorne."
        ),
        list(
          id = "transfer", title = "Przykład transferowy: test przesiewowy",
          text = "Identyczny mechanizm działa w medycynie. Test przesiewowy o czułości 90% i FPR 9% stosowany w populacji, w której choroba dotyka 1% badanych, daje wynik dodatni, który potwierdza się w mniej więcej jednym przypadku na dziesięć. Dlatego po badaniu przesiewowym wykonuje się test potwierdzający — i dlatego programy przesiewowe kieruje się do grup o podwyższonej częstości bazowej.",
          body = list(
            risk_example("3.5", "Naturalne częstości w badaniu przesiewowym",
              problem = "Test przesiewowy ma czułość 0,90 i FPR 0,09; choroba występuje u 1% badanych. Przedstaw wynik dla 1000 osób w naturalnych częstościach i oblicz, jaka część wyników dodatnich jest prawdziwa.",
              steps = c(
                "Chorzy: 1% z 1000 = 10 osób. Wynik dodatni ma 90% z nich: 9 osób.",
                "Zdrowi: 990 osób. Wynik fałszywie dodatni ma 9% z nich: 0,09 · 990 = 89,1, czyli około 89 osób.",
                "Wszystkich wyników dodatnich: 9 + 89 = 98. Prawdziwych: 9/98 ≈ 0,092.",
                "Wzorem (3.4): 0,9 · 0,01/(0,9 · 0,01 + 0,09 · 0,99) = 0,009/0,0981 ≈ 0,092. Metodą szans (3.6): LR₊ = 0,9/0,09 = 10, szanse 1/99 · 10 = 10/99, prawdopodobieństwo 10/109 ≈ 0,092."
              ),
              answer = "Około 9 na 98 wyników dodatnich jest prawdziwych, czyli mniej więcej jeden na jedenaście. Opis „jeden na dziesięć” w tekście to zaokrąglenie tej samej liczby."
            ),
            "Naturalne częstości mają jeszcze jedną zaletę dydaktyczną: pacjentowi łatwiej zrozumieć „9 z 98 osób z takim wynikiem jest chorych” niż „wartość predykcyjna dodatnia wynosi 9,2%”. Ta sama zasada dotyczy raportu dla kierownika zmiany w Bananpolu."
          )
        ),
        list(
          id = "druga-informacja", title = "Druga informacja",
          text = c(
            "Naturalny odruch po niepewnym alarmie to sięgnięcie po drugie źródło: drugi czujnik, odczyt ręczny, telefon do operatora. Rachunek jest optymistyczny — jeśli druga informacja jest warunkowo niezależna od pierwszej, posterior po pierwszym alarmie staje się częstością bazową dla drugiego i wiarygodność szybko rośnie.",
            "Cały zysk wisi jednak na słowie „niezależna”. Dwa identyczne czujniki obok siebie mogą reagować na to samo zakłócenie elektromagnetyczne, ten sam kurz i tę samą wilgoć. W modelu poniżej drugi czujnik z prawdopodobieństwem zadanym suwakiem kopiuje wynik pierwszego; w pozostałych przypadkach działa niezależnie warunkowo przy ustalonym stanie instalacji. Kopiowanie dotyczy zarówno awarii, jak i jej braku. Oba czujniki zachowują tę samą czułość i FPR."
          ),
          body = list(
            risk_definition("3.6", "Warunkowa niezależność dwóch detektorów", c(
              "Wyniki dwóch detektorów są warunkowo niezależne przy danym stanie, jeśli zarówno przy zdarzeniu, jak i przy jego braku prawdopodobieństwo wspólnego wyniku jest iloczynem prawdopodobieństw pojedynczych wyników: P(+₁ ∩ +₂ | A) = P(+₁ | A) · P(+₂ | A) oraz P(+₁ ∩ +₂ | ¬A) = P(+₁ | ¬A) · P(+₂ | ¬A)."
            )),
            "Przy warunkowej niezależności ilorazy wiarygodności się mnożą, a wzór (3.6) stosujemy dwa razy z rzędu. Posterior po pierwszej informacji staje się szansami a priori dla drugiej.",
            risk_formula("\\frac{P(A\\mid +_1,+_2)}{P(\\neg A\\mid +_1,+_2)}=\\mathrm{LR}_1\\cdot \\mathrm{LR}_2\\cdot\\frac{P(A)}{P(\\neg A)}", num = "3.7",
              legend = c("\\mathrm{LR}_1,\\ \\mathrm{LR}_2" = "ilorazy wiarygodności obu informacji", "+_1,+_2" = "alarm pierwszego i drugiego detektora")),
            risk_example("3.6", "Dwa niezależne alarmy",
              problem = "W hali Bananpolu (częstość awarii 0,01) alarmują dwa identyczne czujniki (czułość 0,95, FPR 0,05), których wyniki są warunkowo niezależne. Oblicz posterior po dwóch alarmach dwiema metodami.",
              steps = c(
                "Metoda szans (3.7): szanse a priori 1/99; po dwóch alarmach 1/99 · 19 · 19 = 361/99 ≈ 3,65. Prawdopodobieństwo: 361/(361 + 99) = 361/460 ≈ 0,785.",
                "Metoda sekwencyjna (3.4): po pierwszym alarmie posterior 0,161 staje się nową częstością bazową. 0,95 · 0,161/(0,95 · 0,161 + 0,05 · 0,839) ≈ 0,785.",
                "Naturalne częstości dla 10 000 zmian: 100 awarii · 0,95² ≈ 90 podwójnych alarmów prawdziwych; 9900 zmian bez awarii · 0,05² ≈ 25 podwójnych fałszywych. 90/(90 + 25) ≈ 0,78."
              ),
              answer = "Około 0,785. Drugi niezależny alarm podnosi wiarygodność z 0,16 do niemal 0,79, bo podwójny fałszywy alarm jest rzadki: zdarza się tylko w 0,25% zmian bez awarii."
            )
          )
        ),
        list(
          id = "niezaleznosc", title = "Założenie warunkowej niezależności",
          text = "Dwa czujniki mogą reagować na to samo zakłócenie lub utracić wspólne zasilanie. Warunkowa niezależność oznacza, że przy ustalonym stanie instalacji (awaria albo jej brak) wynik jednego czujnika nie zmienia prawdopodobieństwa wyniku drugiego — i to założenie trzeba uzasadnić mechanizmem, tak jak w poprzednim wykładzie.",
          body = list(
            risk_try("zacznij od prawdopodobieństwa skopiowania 0 i porównaj wynik z przykładem 3.6. Następnie ustaw 0,25, 0,5 i 1. Czujniki mają parametry ustawione w tablicy 2×2 w rozdziale o języku detektora."),
            figure_panel(
              label = "Porównanie", title = "Dwa alarmy",
              lc_slider("a3_dependence", "Prawdopodobieństwo skopiowania pierwszego alarmu", 0, 1, 0, 0.05),
              uiOutput("a3_second"), full_width = TRUE
            ),
            c(
              "Przy parametrach domyślnych i braku kopiowania panel pokazuje 0,785 — dokładnie wynik przykładu 3.6. Przy kopiowaniu 0,25 posterior po dwóch alarmach spada do 0,391, przy 0,5 do 0,263, a przy pełnym kopiowaniu wraca do 0,161, czyli do wartości po jednym alarmie. Nawet umiarkowana zależność zjada większość zysku z drugiego czujnika.",
              "Mechanizm widać w naturalnych częstościach: kopiowanie najbardziej zwiększa liczbę podwójnych fałszywych alarmów. Bez kopiowania podwójny fałszywy alarm wymaga dwóch niezależnych pomyłek (0,05² = 0,0025); przy kopiowaniu wystarczy jedna. Założenie (3.7) jest więc najbardziej optymistycznym wariantem — jeśli nie ma za nim mechanizmu, raport powinien pokazać też wariant z zależnością."
            ),
            risk_check("d3_chk_niezal",
              "Dwa czujniki wiszą na wspólnym wsporniku i reagują na te same drgania. Oba alarmują. Co wiemy o posteriorze po dwóch alarmach?",
              c(
                "Jest równy 0,785, bo są dwa alarmy" = "indep",
                "Leży między 0,161 a 0,785, zależnie od siły wspólnej przyczyny" = "between",
                "Jest niższy niż 0,161, bo czujniki są zależne" = "lower"
              ),
              correct = "between",
              explanation = "0,785 wymaga warunkowej niezależności (definicja 3.6). Przy pełnym kopiowaniu drugi alarm nie wnosi informacji i posterior zostaje na 0,161. Częściowa zależność daje w tym modelu wynik pomiędzy: 0,391 przy kopiowaniu 0,25 i 0,263 przy 0,5. Zależność zmniejsza zysk z drugiego alarmu, ale nie zamienia go w dowód przeciwko awarii.",
              hints = c(
                indep = "Wzór (3.7) zakłada warunkową niezależność. Czy wspólne drgania ją naruszają?",
                lower = "Skrajny przypadek zależności to pełne kopiowanie. Jaki posterior daje wtedy drugi alarm?"
              )
            )
          ),
          extension = TRUE
        )
      )
    ),
    list(
      id = "reakcja", title = "Próg reakcji", hook = "Wiedzieć to jeszcze nie działać",
      lead = "Posterior opisuje przekonanie; decyzja wymaga jeszcze konsekwencji.",
      intro = c(
        "Policzyliśmy: po alarmie szansa awarii wynosi około 16%. Czy wysłać ekipę? Sama liczba nie odpowiada, bo decyzja zależy również od tego, co jest na szali. Wyjazd do fałszywego alarmu kosztuje godzinę pracy ekipy; zignorowanie prawdziwej awarii może kosztować całą partię owoców albo pożar instalacji.",
        "Gdy konsekwencje są tak asymetryczne, niski posterior może w pełni uzasadniać reakcję. Regułę reakcji ustala się przed nocnym telefonem, na chłodno: przy jakim poziomie wiarygodności i jakich kosztach jedziemy zawsze, a kiedy wystarczy zdalna weryfikacja."
      ),
      sections = list(
        list(
          id = "macierz", title = "Macierz konsekwencji",
          bullets = c("alarmuj przy awarii — uniknięta szkoda", "alarmuj bez awarii — koszt postoju", "nie alarmuj przy awarii — możliwa katastrofa", "nie alarmuj bez awarii — brak działania"),
          body = c(
            "Macierz konsekwencji ma ten sam kształt co tablica wyników detektora, ale zamiast liczebności zawiera koszty. Wiersze to znów stan instalacji, kolumny — tym razem nie wynik czujnika, lecz decyzja dyżurnego: reagować albo nie. Posterior mówi, jak prawdopodobny jest każdy wiersz; macierz mówi, ile kosztuje każda komórka. Decyzja wymaga obu.",
            "Oddzielenie tych dwóch warstw jest ważne organizacyjnie. Posterior powinien policzyć analityk na podstawie danych o detektorze i hali. Koszty i akceptowalne ryzyko ustala kierownictwo i służby bezpieczeństwa. Gdy obie rzeczy mieszają się w jednej liczbie („alarm jest ważny w 16%, więc go ignorujemy”), nikt nie wie, czy spór dotyczy faktów, czy wartości."
          )
        ),
        list(
          id = "rachunek", title = "Koszt reakcji a koszt zaniechania",
          body = list(
            figure_panel(label = "Decyzja", title = "Jeden rachunek kosztów", full_width = TRUE,
              lc_p("Rozważamy wyłącznie szkodę materialną. Reakcja kosztuje 100 zł niezależnie od stanu i całkowicie zapobiega stracie; brak reakcji przy awarii kosztuje 2000 zł. Przy posteriorze q oczekiwany koszt braku reakcji to 2000q. Reagujemy, gdy q>0,05. Przy q≈0,161 koszt braku reakcji wynosi około 322 zł, więc reakcja jest uzasadniona mimo przewagi fałszywych alarmów."),
              lc_formula_box(withMathJax("$$L(\\text{reakcja})=100,\\qquad L(\\text{brak})=2000q$$")),
              lc_p("Jeśli reakcja ogranicza stratę tylko o połowę, jej koszt oczekiwany to 100+1000q; próg rośnie do q>0,10. Skuteczność działania jest osobnym założeniem. Urazów i pełnej oceny bezpieczeństwa nie sprowadzamy w tym przykładzie do jednej kwoty.")
            ),
            c(
              "Rachunek z ramki da się zapisać ogólnie. Niech c oznacza koszt reakcji, a L stratę, której reakcja w pełni zapobiega. Oczekiwany koszt reakcji to c, oczekiwany koszt jej braku to q · L. Reagujemy, gdy q · L > c, czyli gdy posterior przekracza iloraz kosztów."
            ),
            risk_definition("3.7", "Próg reakcji", c(
              "Próg reakcji q* to wartość posterioru, przy której oczekiwany koszt reakcji i jej braku są równe. Przy posteriorze powyżej progu reakcja minimalizuje oczekiwany koszt; poniżej — nie. Próg zależy od kosztów i skuteczności reakcji, a nie od detektora."
            )),
            risk_formula("q^{*}=\\frac{c}{L},\\qquad \\text{reaguj, gdy } P(A\\mid +)>q^{*}", num = "3.8",
              legend = c("c" = "koszt reakcji", "L" = "strata, której reakcja zapobiega", "q^{*}" = "próg reakcji")),
            "W Bananpolu q* = 100/2000 = 0,05. Łącząc wzór (3.8) z wzorem (3.6), można przełożyć próg na posteriorze na próg na częstości bazowej: reagujemy na alarm, gdy szanse a priori · 19 > 0,05/0,95 = 1/19, czyli gdy szanse a priori przekraczają 1/361. Odpowiada to częstości awarii około 0,0028. Poniżej tej częstości pojedynczy alarm tego czujnika nie uzasadnia wyjazdu przy tych kosztach.",
            risk_example("3.7", "Jedna reguła, trzy sytuacje",
              problem = list(
                "Reakcja kosztuje 100 zł, brak reakcji przy awarii 2000 zł, a reakcja w pełni zapobiega stracie. Czujnik ma czułość 0,95 i FPR 0,05. Rozstrzygnij, czy reagować:",
                risk_parts(
                  "Na alarm w hali o częstości awarii 0,01.",
                  "Na alarm w hali o częstości awarii 0,002.",
                  "Na zmianie bez alarmu w hali o częstości awarii 0,01."
                )
              ),
              steps = c(
                "Próg ze wzoru (3.8): q* = 100/2000 = 0,05. Posterior 0,161 > 0,05. Oczekiwany koszt braku reakcji: 2000 · 0,161 ≈ 322 zł > 100 zł. Reagujemy.",
                "Ze wzoru (3.4): 0,95 · 0,002/(0,95 · 0,002 + 0,05 · 0,998) ≈ 0,037 < 0,05. Oczekiwany koszt braku reakcji: 2000 · 0,037 ≈ 73 zł < 100 zł. Sam alarm nie uzasadnia wyjazdu; opłaca się tania druga informacja. Jeśli drugi, warunkowo niezależny czujnik też alarmuje, ze wzoru (3.7): szanse 0,002/0,998 · 361 ≈ 0,72, posterior ≈ 0,42 > 0,05. Reagujemy.",
                "Z przykładu 3.3: P(awaria | brak alarmu) ≈ 0,00053. Oczekiwany koszt braku reakcji: około 1,06 zł. Nie reagujemy."
              ),
              steps_type = "a",
              answer = "(a) reagować; (b) najpierw zweryfikować drugim źródłem, reagować po potwierdzeniu; (c) nie reagować. Ta sama reguła kosztowa daje różne decyzje, bo posterior zależy od hali i od wyniku detektora."
            ),
            risk_check("d3_chk_prog",
              "Wyjazd ekipy podrożał do 300 zł; strata przy zignorowanej awarii nadal wynosi 2000 zł, a reakcja w pełni jej zapobiega. Jaki jest próg reakcji?",
              c("0,05" = "old", "0,15" = "new", "0,50" = "half"),
              correct = "new",
              explanation = "Ze wzoru (3.8): q* = 300/2000 = 0,15. Posterior 0,161 w hali Bananpolu nadal przekracza próg, ale już tylko nieznacznie — niewielka zmiana kosztów lub częstości bazowej może odwrócić decyzję.",
              hints = c(
                old = "0,05 odpowiadało kosztowi reakcji 100 zł. Przelicz iloraz c/L dla nowego kosztu.",
                half = "Próg 0,5 odpowiadałby równym kosztom obu pomyłek. Tu strata jest znacznie większa od kosztu reakcji."
              )
            ),
            "Reguła progu ma jeszcze jedną zaletę: można ją zapisać w procedurze przed nocnym telefonem. Dyżurny nie musi liczyć Bayesa o trzeciej w nocy; wystarczy, że wie, w której hali pracuje czujnik i czy wymagane jest potwierdzenie drugim źródłem."
          )
        )
      ),
      decision = "Ustal próg reakcji jawnie na podstawie kosztów i wykonalności, nie na podstawie samego posteriora."
    ),
    list(
      id = "sprawdzenie", title = "Ściąga i sprawdzenie", hook = "Alarm to dopiero początek pytania",
      lead = "Pytanie → populacja → detektor → posterior → konsekwencje.",
      intro = "Największym ryzykiem tego wykładu nie jest błąd rachunkowy, lecz odpowiedź na niewłaściwe pytanie. Ściąga porządkuje audyt alarmu od pytania do decyzji; quiz i ćwiczenia sprawdzają, czy odróżniasz kierunki warunkowania bez podpowiedzi.",
      sections = list(list(
        id = "podsumowanie", title = "Podsumowanie",
        text = c(
          "Wykład zaczął się od pytania dyżurnego: co oznacza alarm? Odpowiedź wymaga odwrócenia warunku. Detektor opisujemy liczbami liczonymi w wierszach tablicy 2×2 — czułością i swoistością (3.1) oraz odsetkiem fałszywych alarmów (3.2) — natomiast dyżurnego interesuje liczba z kolumny: wartość predykcyjna dodatnia P(awaria | alarm). Most między nimi buduje wzór Bayesa (3.4), który jest tylko definicją prawdopodobieństwa warunkowego (2.1) z mianownikiem rozpisanym wzorem na prawdopodobieństwo całkowite (3.3).",
          "Wiarygodność alarmu nie jest cechą czujnika, lecz pary czujnik–populacja. W zapisie szans (3.6) czujnik wnosi iloraz wiarygodności, a hala — szanse a priori; przy rzadkich awariach nawet dobry czujnik daje przewagę fałszywych alarmów, za to cisza jest bardzo wiarygodna (3.5). Druga informacja mnoży ilorazy wiarygodności (3.7), ale tylko wtedy, gdy jest warunkowo niezależna od pierwszej; wspólna przyczyna zjada zysk.",
          "Posterior nie jest decyzją. Próg reakcji (3.8) wynika z kosztów i skuteczności działania, a nie z detektora; niski posterior może uzasadniać reakcję, jeśli strata jest duża. Rachunek i wartości warto rozdzielić: analityk liczy posterior, organizacja ustala próg, a procedura łączy oba przed nocnym telefonem."
        )
      ), list(
        id = "sciaga", title = "Ściąga",
        bullets = c("Pytanie: co oznacza alarm?", "Model: Bayes lub naturalne częstości", "Założenia: częstość bazowa, stabilne parametry, zależności", "Wynik: P(awaria | alarm)", "Interpretacja: nie jest automatyczną decyzją"),
        widget = alarm_sciaga_widget
      ), list(
        id = "most", title = "Co dalej",
        text = "Alarm dotyczył pojedynczej zmiany. W następnym wykładzie zmienimy skalę: policzymy, ile zdarzeń pojawi się w całej serii wielu porównywalnych prób."
      ))
    )
  )
)

alarm_chapters <- risk_block_chapters(alarm_block)

alarm_server <- function(input, output, session) {
  detector_parameters <- reactive(list(
    prevalence = input$a3_prev %||% .01,
    sensitivity = input$a3_sens %||% .95,
    false_positive_rate = input$a3_fpr %||% .05
  ))
  posterior <- reactive(do.call(risk_bayes, detector_parameters()))
  checked <- reactiveVal(FALSE)
  observeEvent(input$a3_vote_check, checked(TRUE))
  output$a3_vote_feedback <- renderUI({
    req(checked())
    if (is.null(input$a3_vote)) {
      return(lc_caption(
               "Najpierw zaznacz jedną z odpowiedzi.",
               tone = "info"
             ))
    }
    if (identical(input$a3_vote, "16")) {
      lc_status(
        lc_verdict(tags$strong("Około 16%."), type = "ok"),
        " Na 1000 zmian przypada około 10 awarii i 9–10 prawdziwych alarmów, ale też około 50 fałszywych alarmów z 990 zmian bez awarii. Większość alarmów jest fałszywa."
      )
    } else if (identical(input$a3_vote, "base")) {
      lc_status(
        tags$strong("Dobry odruch, ale częstość bazowa jest podana:"),
        " 1 awaria na 100 zmian. Z nią wynik da się policzyć — około 16%. Bez częstości bazowej odpowiedź rzeczywiście byłaby niemożliwa."
      )
    } else {
      lc_status(
        lc_verdict(tags$strong("95% to czułość, czyli P(alarm | awaria)."), type = "warning"),
        " Pytanie po alarmie dotyczy P(awaria | alarm). Przy rzadkich awariach większość alarmów pochodzi ze zmian bez awarii i wynik spada do około 16%."
      )
    }
  })
  detector <- reactive(do.call(risk_detector_counts, c(list(population = 10000L), detector_parameters())))
  output$a3_table <- renderUI({
    d <- detector()
    counts <- matrix(c(d$alarm, d$no_alarm), nrow = 2,
                     dimnames = list(d$state, c("Alarm", "Brak alarmu")))
    lc_crosstab(counts, measure = "n", row_name = "Stan", col_name = "Odczyt detektora",
                lead = FALSE, label = "Tablica 2×2 dla 10 000 zmian")
  })
  output$a3_counts <- renderUI({
    d <- detector()
    lc_stat_grid(lc_stat_box("Prawdziwe alarmy", d$alarm[1]),
      lc_stat_box("Fałszywe alarmy", d$alarm[2]),
      lc_stat_box("P(awaria | alarm)", risk_format_probability(posterior()), color = upwr_accent),
      columns = 1
    )
  })
  # Siatka pokazuje tylko alarmy (prawdziwe i fałszywe): 10 000 pól byłoby
  # nieczytelne, a proporcja alarmów to właśnie wiarygodność alarmu.
  grid_plot <- reactive({
    d <- detector()
    tp <- d$alarm[1]
    fp <- d$alarm[2]
    outside <- paste0(
      "Poza siatką: ", format(d$no_alarm[1], big.mark = " ", trim = TRUE),
      " awarii bez alarmu i ", format(d$no_alarm[2], big.mark = " ", trim = TRUE),
      " zmian bez awarii i bez alarmu."
    )
    n <- tp + fp
    if (n == 0) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "Brak alarmów") +
        labs(caption = outside) + theme_void())
    }
    labels <- c("Prawdziwy alarm", "Fałszywy alarm")
    dat <- data.frame(type = factor(rep(labels, c(tp, fp)), levels = labels))
    ncol <- max(5L, round(sqrt(n) * 1.2))
    dat$x <- (seq_len(n) - 1L) %% ncol
    dat$y <- (seq_len(n) - 1L) %/% ncol
    ggplot(dat, aes(x, y, fill = type)) +
      geom_tile(colour = if (n <= 1500) "white" else NA, linewidth = 0.3) +
      scale_y_reverse() +
      coord_equal(expand = FALSE) +
      scale_fill_manual(
        values = c(upwr_accent, upwr_single_alt),
        labels = paste0(labels, " (", c(tp, fp), ")"), drop = FALSE
      ) +
      guides(fill = guide_legend(ncol = 1)) +
      labs(
        title = paste0("Każde pole to jeden alarm (", format(n, big.mark = " ", trim = TRUE), ")"),
        caption = outside, x = NULL, y = NULL, fill = NULL
      ) +
      theme_upwr() +
      theme(
        axis.text = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(), panel.grid = element_blank(),
        legend.position = "bottom", legend.key.size = grid::unit(1.1, "lines")
      )
  })
  zoom_plot_server("a3_grid", grid_plot, alt = "Siatka wszystkich alarmów w dziesięciu tysiącach zmian: prawdziwe i fałszywe alarmy jako pola dwóch kolorów.")
  curve_plot <- reactive({
    prevalence <- seq(.0001, .2, length.out = 300)
    dat <- data.frame(prevalence, posterior = vapply(prevalence, risk_bayes, numeric(1),
      sensitivity = input$a3_curve_sens, false_positive_rate = input$a3_curve_fpr
    ))
    ggplot(dat, aes(prevalence, posterior)) +
      geom_line(colour = upwr_accent, linewidth = 1.1) +
      geom_vline(xintercept = .01, linetype = 2, colour = upwr_reference) +
      labs(x = "P(awarii)", y = "P(awarii | alarm)") +
      theme_upwr()
  })
  zoom_plot_server("a3_curve", curve_plot, alt = "Rosnąca krzywa wiarygodności alarmu względem częstości bazowej awarii.")
  output$a3_posterior <- renderUI(lc_stat_grid(lc_stat_box("Dla P(awarii)=0,01",
    risk_format_probability(risk_bayes(.01, input$a3_curve_sens, input$a3_curve_fpr)),
    color = upwr_accent
  ), columns = 1))
  output$a3_second <- renderUI({
    p1 <- posterior()
    adjusted <- do.call(risk_two_alarm_posterior, c(detector_parameters(),
      list(dependence = input$a3_dependence %||% 0)))
    lc_stat_grid(lc_stat_box("Po jednym alarmie", risk_format_probability(p1)),
      lc_stat_box("Po dwóch alarmach", risk_format_probability(adjusted), color = upwr_accent),
      columns = 1
    )
  })
  risk_assessment_server("a3", alarm_quiz, input, output)
}
