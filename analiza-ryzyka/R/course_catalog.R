# ==========================================================================
# KATALOG WYKŁADÓW 02–10
# ==========================================================================

.rc <- risk_chapter

risk_course_catalog <- list(
  "warunki" = list(
    folder = "02-warunki", lecture_id = "warunki", num = "02",
    title = "Warunki zmieniają ocenę",
    chapters = list(
      .rc("filtr", "Zawężamy świat", "Warunek zmienia populację odniesienia.",
          c("P(A|B) liczymy wyłącznie wśród wyników spełniających B.", "Licznik i mianownik zawsze nazywamy słowami."),
          "P(A\\mid B)=\\frac{P(A\\cap B)}{P(B)}", "Porównujemy poślizgnięcia we wszystkich zmianach i tylko podczas zmian z mokrą posadzką."),
      .rc("iloczyn", "Mnożymy wzdłuż drogi", "Reguła iloczynu składa kolejne etapy.",
          c("Najpierw zachodzi warunek B, potem zdarzenie A w obrębie B.", "Kolejność opisu nie zmienia części wspólnej."),
          "P(A\\cap B)=P(B)P(A\\mid B)", widget = TRUE),
      .rc("drzewo", "Drzewo prawdopodobieństwa", "Gałęzie porządkują rozłączne drogi.",
          c("Prawdopodobieństwa na gałęziach wychodzących sumują się do jedności.", "Liść otrzymujemy przez mnożenie wzdłuż gałęzi."),
          case = "Zdarzenie może wystąpić przy suchej albo mokrej posadzce; drzewo zachowuje oba tryby."),
      .rc("calkowite", "Sumujemy drogi", "To samo zdarzenie może mieć kilka rozłącznych przyczyn.",
          c("Dzielimy przestrzeń na kompletne, rozłączne warunki.", "Sumujemy prawdopodobieństwa wszystkich dróg do A."),
          "P(A)=\\sum_i P(B_i)P(A\\mid B_i)"),
      .rc("niezaleznosc", "Niezależność wymaga uzasadnienia", "Brak związku nie jest ustawieniem domyślnym.",
          c("Przy niezależności P(A|B)=P(A).", "Wspólne zasilanie, środowisko i procedura tworzą zależność."),
          pitfall = "P(A∩B)=P(A)P(B) wolno użyć dopiero po uzasadnieniu niezależności."),
      .rc("decyzja", "Warunek w decyzji", "Pytamy, który warunek najbardziej zmienia wynik.",
          c("Wynik warunkowy wskazuje grupę lub tryb wymagający działania.", "Nie zamieniamy związku warunkowego automatycznie w przyczynę."),
          case = "Dyrektor porównuje sprzątanie co 30 i 60 minut, zachowując ten sam mianownik ekspozycji."),
      .rc("sciaga", "Ściąga i ćwiczenia", "Od filtra do drzewa i decyzji.",
          c("Filtruj mianownik, mnóż wzdłuż drogi, sumuj rozłączne drogi.", "Na końcu nazwij założenie o zależności."))
    ),
    widget = list(title="Reguła iloczynu", input_label="P(A | B)", min=.01,max=.50,value=.20,step=.01,
      input_short="P(A | B)",output_label="P(A ∩ B)",compute=function(x) .20*x,format="probability",
      plot_title="Część wspólna przy P(B)=0,20",x_label="P(A | B)",y_label="P(A ∩ B)",ylim=c(0,.11),
      note="P(B) jest stałe i wynosi 0,20.",alt="Wykres reguły iloczynu dla stałego prawdopodobieństwa warunku."),
    quiz=list(question="Co zmienia warunek B?",choices=c("Tylko licznik"="num","Populację odniesienia"="den","Nazwę zdarzenia"="name"),correct="den",explanation="Warunek zawęża mianownik do wyników spełniających B."),
    exercises=c("Narysuj drzewo dla suchej i mokrej posadzki.","Wskaż wspólną przyczynę dwóch pozornie niezależnych zdarzeń.")
  ),
  "alarm-i-prawda" = list(
    folder="03-alarm-i-prawda",lecture_id="alarm-i-prawda",num="03",title="Alarm nie zawsze oznacza awarię",
    chapters=list(
      .rc("alarm", "Alarm odwraca pytanie", "Obserwujemy sygnał, ale pytamy o ukrytą przyczynę.", c("P(alarm|wyciek) nie jest P(wyciek|alarm).", "Częstość bazowa wpływa na interpretację alarmu."), case="Czujnik w dojrzewalni sygnalizuje nieprawidłowe stężenie gazu."),
      .rc("parametry", "Czułość i fałszywe alarmy", "Opisujemy wszystkie wyniki detektora.", c("Czułość dotyczy prawdziwych zdarzeń.", "Fałszywie dodatni alarm występuje bez zdarzenia.")),
      .rc("czestosci", "Naturalne częstości", "Najpierw liczymy na 10 000 zmian.", c("Oddzielamy prawdziwe i fałszywe alarmy.", "Mianownikiem pytania po alarmie są wszystkie alarmy.")),
      .rc("bayes", "Wzór Bayesa", "Skracamy rozumowanie z drzewa.", c("Licznik to wspólna droga zdarzenie i alarm.", "Mianownik obejmuje każdą drogę kończącą się alarmem."), "P(A\\mid +)=\\frac{P(+\\mid A)P(A)}{P(+)}", widget=TRUE),
      .rc("bazowa", "Pułapka częstości bazowej", "Dobry czujnik może mieć niski dodatni wynik predykcyjny.", c("Rzadkie zdarzenia dostarczają mało prawdziwych alarmów.", "Niewielki odsetek fałszywych alarmów działa na dużej bazie."), pitfall="Parametr czujnika nie jest prawdopodobieństwem zdarzenia po alarmie."),
      .rc("drugi-test", "Druga informacja", "Aktualizację można powtórzyć.", c("Drugi test zmienia ocenę po pierwszym.", "Mnożenie dowodów wymaga warunkowej niezależności.")),
      .rc("decyzja", "Reakcja na alarm", "Prawdopodobieństwo i koszt decyzji to osobne warstwy.", c("Próg reakcji zależy od skutków przeoczenia i fałszywego alarmu.", "Raport podaje posterior oraz przyjętą regułę działania.")),
      .rc("sciaga", "Ściąga i ćwiczenia", "Drzewo, częstości, Bayes, decyzja.", c("Najpierw częstości naturalne, potem wzór.", "Zawsze sprawdź prior i zależność testów."))
    ),
    widget=list(title="Częstość bazowa zmienia wiarygodność alarmu",input_label="Prawdziwe zdarzenia w zmianach",min=.001,max=.10,value=.01,step=.001,input_short="P(A)",output_label="P(A | alarm)",compute=function(x) .95*x/(.95*x+.05*(1-x)),format="probability",plot_title="Prawdopodobieństwo zdarzenia po alarmie",x_label="Częstość bazowa P(A)",y_label="P(A | alarm)",ylim=c(0,.7),note="Czułość 0,95; odsetek fałszywych alarmów 0,05.",alt="Wpływ częstości bazowej na prawdopodobieństwo zdarzenia po alarmie."),
    quiz=list(question="Mianownik P(zdarzenie | alarm) obejmuje:",choices=c("Wszystkie zdarzenia"="events","Wszystkie alarmy"="alarms","Tylko fałszywe alarmy"="false"),correct="alarms",explanation="Po warunku alarm filtrujemy świat do wszystkich alarmów."),
    exercises=c("Policz wynik na 10 000 zmian przed użyciem wzoru.","Wskaż koszt fałszywego alarmu i koszt przeoczenia.")
  ),
  "wiele-prob" = list(
    folder="04-wiele-prob",lecture_id="wiele-prob",num="04",title="Wiele prób, jedna szansa",
    chapters=list(
      .rc("zmienna", "Od zdarzenia do zmiennej losowej", "Zliczamy zdarzenia w ustalonej liczbie prób.", c("X przypisuje wynikowi liczbę zdarzeń.", "Rozkład opisuje możliwe wartości X i ich szanse.")),
      .rc("bernoulli", "Próba Bernoulliego", "Jedna próba ma dwa wyniki.", c("Wyróżnione zdarzenie może być awarią.", "Parametr p odnosi się do jednej jawnej jednostki próby."), case="Próbą jest kontrola jednego zaworu, nie nieokreślony kwartał pracy."),
      .rc("zalozenia", "Cztery założenia", "Stałe n, dwa wyniki, stałe p i niezależność.", c("Zmiana ekspozycji może zmieniać p.", "Wspólna partia produkcyjna może zależnić próby."), pitfall="Pracownicy × dni wymagają jawnej jednostki, np. pracownikodnia."),
      .rc("dwumian", "Rozkład dwumianowy", "Pytamy o liczbę zdarzeń w n próbach.", c("Współczynnik kombinacyjny liczy układy zdarzeń.", "Rozróżniamy dokładnie, najwyżej i co najmniej."), "P(X=k)={n\\choose k}p^k(1-p)^{n-k}"),
      .rc("srednia", "Wartość oczekiwana i zmienność", "Średnia wielu serii nie jest gwarancją jednej serii.", c("E(X)=np.", "Wariancja wynosi np(1-p).")),
      .rc("jedna", "Co najmniej jedno", "Dopełnienie upraszcza sumę wielu możliwości.", c("Najpierw liczymy brak zdarzeń.", "Duże n potrafi zneutralizować małe p."), "P(X\\ge1)=1-(1-p)^n", widget=TRUE),
      .rc("sciaga", "Ściąga i ćwiczenia", "Dobór modelu zaczyna się od sprawdzenia prób.", c("Nazwij próbę, n i p.", "Oczekiwana liczba nie jest prognozą bez błędu."))
    ),
    widget=list(title="Małe p w wielu próbach",input_label="Liczba niezależnych prób n",min=1,max=500,value=100,step=1,input_short="n",output_label="P(co najmniej 1)",compute=function(x) 1-(1-.01)^x,format="probability",plot_title="Co najmniej jedno zdarzenie przy p=0,01",x_label="Liczba prób",y_label="P(X ≥ 1)",ylim=c(0,1),note="Prawdopodobieństwo jednej próby jest stałe: p=0,01.",alt="Wzrost prawdopodobieństwa co najmniej jednego zdarzenia wraz z liczbą prób."),
    quiz=list(question="W rozkładzie dwumianowym ustalone jest:",choices=c("n"="n","k sukcesów"="k","czas do awarii"="time"),correct="n",explanation="Ustalamy liczbę prób n i pytamy o liczbę zdarzeń X."),
    exercises=c("Zdefiniuj próbę dla kontroli partii zaworów.","Policz P(X≥1) przez dopełnienie.")
  ),
  "do-zdarzenia" = list(
    folder="05-do-zdarzenia",lecture_id="do-zdarzenia",num="05",title="Ile prób do zdarzenia",
    chapters=list(
      .rc("zamiana", "Zmieniamy to, co jest stałe", "Mechanizm Bernoulliego zostaje, pytanie się odwraca.", c("Dwumianowy: stałe n.", "Ujemny dwumianowy: stałe r zdarzeń.")),
      .rc("geometryczny", "Do pierwszego zdarzenia", "Rozkład geometryczny jest przypadkiem r=1.", c("X liczy wszystkie próby do pierwszego zdarzenia.", "Długi prawy ogon oznacza możliwość długiego oczekiwania.")),
      .rc("ujemny", "Do r-tego zdarzenia", "Ustalamy r i obserwujemy zmienną liczbę prób.", c("Ostatnia próba kończy się wyróżnionym zdarzeniem.", "E(X)=r/p w przyjętej parametryzacji."), widget=TRUE),
      .rc("parametryzacja", "Dwie konwencje liczenia", "Program i podręcznik mogą różnić się o r.", c("My liczymy wszystkie próby.", "R dnbinom liczy porażki przed r-tym sukcesem."), pitfall="Zawsze zapisz słowami, co liczy X."),
      .rc("limit", "Plan zasobów", "Średnia nie wystarcza do ustalenia limitu kontroli.", c("Pytamy o ukończenie przed limitem.", "Kwantyl jest użyteczniejszy niż sama średnia."), case="Inspektor planuje liczbę palet potrzebnych do wykrycia trzech wadliwych."),
      .rc("ograniczenia", "Kiedy model zawodzi", "Stałe p i niezależność są wymagające.", c("Uczenie, zmęczenie i partie zmieniają p.", "Próby mogą być zależne w obrębie dostawy.")),
      .rc("sciaga", "Ściąga i ćwiczenia", "Stałe n czy stałe r?", c("Najpierw określ, co zatrzymuje eksperyment.", "Nie nazywaj liczby prób czasem ciągłym."))
    ),
    widget=list(title="Oczekiwana liczba prób do trzeciego wykrycia",input_label="Prawdopodobieństwo wykrycia p",min=.02,max=.50,value=.10,step=.01,input_short="p",output_label="E(X)=3/p",compute=function(x) 3/x,format="count",plot_title="Średnia liczba prób",x_label="p w jednej próbie",y_label="Oczekiwana liczba prób",ylim=c(0,150),note="Liczymy wszystkie próby do trzeciego zdarzenia.",alt="Spadek oczekiwanej liczby prób wraz ze wzrostem prawdopodobieństwa wykrycia."),
    quiz=list(question="Co jest stałe w ujemnym dwumianowym?",choices=c("Liczba prób"="n","Liczba zdarzeń r"="r","Czas kalendarzowy"="time"),correct="r",explanation="Doświadczenie kończymy po ustalonej liczbie r zdarzeń."),
    exercises=c("Porównaj dwie parametryzacje dla r=3.","Wyznacz średnią liczbę kontroli przy p=0,1.")
  ),
  "zmiennosc-i-prog" = list(
    folder="06-zmiennosc-i-prog",lecture_id="zmiennosc-i-prog",num="06",title="Zmienność i próg bezpieczeństwa",
    chapters=list(
      .rc("ciagla", "Zmienna ciągła", "Prawdopodobieństwo jest polem dla przedziału.", c("P(X=x)=0 dla idealnego modelu ciągłego.", "Gęstość nie jest prawdopodobieństwem w punkcie.")),
      .rc("normalny", "Rozkład normalny", "μ przesuwa, a σ rozszerza rozkład.", c("Średnia opisuje położenie.", "Odchylenie standardowe opisuje typowy rozrzut.")),
      .rc("standaryzacja", "Wspólna linijka z", "Odległość od średniej mierzymy w jednostkach σ.", c("z=(x-μ)/σ.", "Znak wskazuje stronę średniej."), "z=\\frac{x-\\mu}{\\sigma}"),
      .rc("prog", "Przekroczenie progu", "Dla bezpieczeństwa często liczy się ogon.", c("Akceptowalna średnia nie gwarantuje małej liczby przekroczeń.", "Zmniejszenie σ może być równie ważne jak zmiana μ."), widget=TRUE),
      .rc("wytrzymalosc", "Obciążenie i wytrzymałość", "Awaria zachodzi, gdy L>S.", c("Analizujemy różnicę D=S-L albo symulujemy pary.", "Zależność L i S wpływa na wariancję różnicy."), "P(\\text{awarii})=P(S-L<0)", pitfall="Pole nakładania dwóch gęstości nie jest P(L>S)."),
      .rc("granice", "Kiedy normalny zawodzi", "Ogon wymaga szczególnej ostrożności.", c("Skośność, ograniczenie do wartości dodatnich i ciężkie ogony zmieniają wynik.", "Dopasowanie środka nie gwarantuje poprawnego ekstremum.")),
      .rc("sciaga", "Ściąga i ćwiczenia", "Średnia, rozrzut, próg, decyzja.", c("Nazwij mierzoną wielkość i okres.", "Nie ekstrapoluj rzadkich zdarzeń bez diagnostyki."))
    ),
    widget=list(title="Ogon powyżej progu 85",input_label="Odchylenie standardowe σ",min=.5,max=8,value=3,step=.1,input_short="σ",output_label="P(X>85)",compute=function(x) 1-pnorm((85-82)/x),format="probability",plot_title="Przekroczenie progu przy μ=82",x_label="Odchylenie standardowe σ",y_label="P(X>85)",ylim=c(0,.4),note="Średnia wynosi 82, a próg 85 w tej samej umownej jednostce.",alt="Wpływ zmienności na prawdopodobieństwo przekroczenia progu."),
    quiz=list(question="Co opisuje awarię w modelu obciążenie–wytrzymałość?",choices=c("Nakładanie gęstości"="overlap","L>S"="difference","Równe średnie"="means"),correct="difference",explanation="Zdarzenie awarii definiujemy jako obciążenie większe od wytrzymałości."),
    exercises=c("Policz z-score progu.","Zaproponuj redukcję średniej albo zmienności i porównaj skutki.")
  ),
  "czas-zycia" = list(
    folder="07-czas-zycia",lecture_id="czas-zycia",num="07",title="Czas życia elementu",
    chapters=list(
      .rc("czas", "Czas do awarii", "Dodatnia zmienna losowa opisuje długość działania.", c("Czas misji musi być jawny.", "Elementy działające na końcu obserwacji też niosą informację.")),
      .rc("funkcje", "F, R i hazard", "Trzy widoki odpowiadają na różne pytania.", c("F(t)=P(T≤t).", "R(t)=P(T>t); hazard opisuje chwilową intensywność warunkową."), "R(t)=1-F(t)"),
      .rc("wykladniczy", "Rozkład wykładniczy", "Stały hazard daje brak pamięci.", c("R(t)=exp(-λt).", "Wiek nie zmienia rozkładu dalszego czasu w idealnym modelu."), widget=TRUE),
      .rc("gamma", "Gamma i etapy", "Interpretacja czasu do k-tego zdarzenia wymaga procesu Poissona.", c("Dla całkowitego k otrzymujemy Erlanga.", "Nie każda gamma oznacza fizyczne etapy awarii.")),
      .rc("weibull", "Weibull i mechanizm", "Kształt hazardu zależy od β.", c("β<1: awarie wczesne; β=1: stały hazard; β>1: zużycie.", "η ustala skalę czasu.")),
      .rc("wanna", "Krzywa wannowa", "Trzy fazy są syntezą mechanizmów.", c("Nie tworzy jej pojedynczy Weibull o stałym β.", "Model złożony może łączyć różne przyczyny."), pitfall="Ten sam MTTF nie oznacza tej samej niezawodności w czasie misji."),
      .rc("utrzymanie", "Decyzja utrzymaniowa", "Model ma wspierać termin przeglądu.", c("Porównujemy R(t) w planowanym czasie misji.", "Wymiana prewencyjna zależy też od kosztów i ukrytych uszkodzeń."), case="Oceniamy, czy wentylator chłodni dotrwa do kolejnego przeglądu."),
      .rc("sciaga", "Ściąga i ćwiczenia", "Mechanizm przed nazwą rozkładu.", c("Najpierw narysuj hazard jakościowo.", "Oddziel estymację parametrów od interpretacji modelu."))
    ),
    widget=list(title="Niezawodność w czasie misji",input_label="Czas misji t [h]",min=0,max=5000,value=1000,step=50,input_short="t [h]",output_label="R(t)",compute=function(x) exp(-x/1500),format="probability",plot_title="Model wykładniczy przy MTTF=1500 h",x_label="Czas misji [h]",y_label="R(t)",ylim=c(0,1),note="Idealny model ze stałym hazardem λ=1/1500 h⁻¹.",alt="Spadek niezawodności wykładniczej wraz z czasem misji."),
    quiz=list(question="Co oznacza R(t)?",choices=c("P(T≤t)"="cdf","P(T>t)"="survival","Średni czas"="mean"),correct="survival",explanation="R(t) jest prawdopodobieństwem działania dłużej niż t."),
    exercises=c("Porównaj dwa modele o podobnym MTTF.","Dopasuj jakościowy hazard do fazy eksploatacji.")
  ),
  "niezawodnosc-systemu" = list(
    folder="08-niezawodnosc-systemu",lecture_id="niezawodnosc-systemu",num="08",title="Niezawodność systemu",
    chapters=list(
      .rc("misja", "Najpierw czas misji", "R_i(t) musi odnosić się do tego samego t.", c("Niezawodność nie jest bezczasową etykietą.", "System naprawialny wymaga odróżnienia gotowości.")),
      .rc("szereg", "System szeregowy", "Wszystkie elementy muszą działać.", c("Awaria jednego zatrzymuje system.", "Przy niezależności mnożymy R_i(t)."), "R_s(t)=\\prod_i R_i(t)"),
      .rc("rownolegle", "System równoległy", "Wystarczy co najmniej jedna działająca gałąź.", c("Najłatwiej policzyć awarię wszystkich gałęzi.", "Redundancja daje malejące przyrosty."), "R_p(t)=1-\\prod_i[1-R_i(t)]", widget=TRUE),
      .rc("mieszany", "Układ mieszany", "Redukujemy czytelne bloki krok po kroku.", c("Najpierw definiujemy sukces systemu.", "Nie uśredniamy niezawodności elementów.")),
      .rc("wspolna", "Wspólna przyczyna", "Redundancja pomaga tylko przy rzeczywistej niezależności.", c("Wspólne zasilanie i środowisko tworzą pojedynczy punkt awarii.", "Dodajemy jawne zdarzenie wspólne."), pitfall="Nie zastępuj modelu wspólnej przyczyny nieobjaśnionym suwakiem korelacji."),
      .rc("priorytet", "Który element poprawić", "Wpływ zależy od architektury.", c("Porównujemy zmianę R_systemu po tej samej interwencji.", "Koszt i wykonalność pozostają osobną warstwą."), case="Dwie pompy instalacji Bananpolu mają osobne gałęzie, ale początkowo wspólne zasilanie."),
      .rc("sciaga", "Ściąga i ćwiczenia", "Logika działania przed wzorem.", c("Nazwij czas misji i definicję sukcesu.", "Sprawdź niezależność i wspólne przyczyny."))
    ),
    widget=list(title="Dwie identyczne gałęzie równoległe",input_label="Niezawodność jednej gałęzi",min=.80,max=.999,value=.95,step=.001,input_short="R elementu",output_label="R systemu",compute=function(x) 1-(1-x)^2,format="probability",plot_title="Korzyść z redundancji przy niezależności",x_label="R jednej gałęzi",y_label="R dwóch gałęzi",ylim=c(.95,1),note="Obie gałęzie mają ten sam czas misji i są niezależne.",alt="Niezawodność dwóch równoległych gałęzi zależnie od niezawodności elementu."),
    quiz=list(question="Kiedy pada system równoległy?",choices=c("Gdy padnie dowolny element"="one","Gdy padną wszystkie gałęzie"="all","Po czasie średnim"="mean"),correct="all",explanation="System równoległy traci funkcję dopiero po awarii wszystkich wymaganych gałęzi."),
    exercises=c("Policz system 2×równoległy w szeregu z czujnikiem.","Dodaj wspólne zdarzenie utraty zasilania.")
  ),
  "drzewo-bledow" = list(
    folder="09-drzewo-bledow",lecture_id="drzewo-bledow",num="09",title="Analiza drzewa błędów",
    chapters=list(
      .rc("top", "Zdarzenie szczytowe", "Precyzujemy system, skutek i okres.", c("Zdarzenie musi być obserwowalne i jednoznaczne.", "FTA zaczyna od skutku i pyta o przyczyny."), case="Top event: nieopanowany pożar magazynu Bananpolu w ciągu roku."),
      .rc("dekompozycja", "Od skutku do przyczyn", "Rozbijamy zdarzenia aż do liści z danymi.", c("Oddzielamy inicjację od wykrycia i tłumienia.", "Drzewo wymaga przeglądu eksperckiego.")),
      .rc("bramki", "Bramki OR i AND", "Logika poprzedza liczby.", c("OR: wystarczy jedna przyczyna.", "AND: potrzebna jest kombinacja.")),
      .rc("rachunek", "Liczenie od liści", "Wzory zależą od niezależności wejść.", c("Dla niezależnego OR używamy dopełnienia.", "Dla niezależnego AND używamy iloczynu."), widget=TRUE),
      .rc("przekroje", "Minimalne przekroje", "Szukamy najmniejszych kombinacji wystarczających.", c("Przekrój opisuje logikę, nie ranking ważności.", "Powtarzający się basic event liczymy jako to samo zdarzenie.")),
      .rc("waznosc", "Ważność i wspólna przyczyna", "Najczęstszy liść nie zawsze jest najlepszą interwencją.", c("Miara ważności musi być nazwana.", "Wspólne zasilanie zmienia strukturę drzewa.")),
      .rc("granice", "Granice FTA", "Drzewo nie dowodzi kompletności przyczyn.", c("Wynik zależy od zakresu, danych i zależności.", "Model podlega aktualizacji po nowych informacjach."), pitfall="Proste propagowanie może być błędne, gdy ten sam liść występuje w kilku gałęziach."),
      .rc("sciaga", "Ściąga i ćwiczenia", "Top event, logika, dane, przegląd.", c("Najpierw przełączaj stany bez liczb.", "Potem licz i sprawdzaj założenia."))
    ),
    widget=list(title="Bramka OR z trzema niezależnymi wejściami",input_label="P jednego zdarzenia bazowego",min=.0001,max=.05,value=.01,step=.0001,input_short="p liścia",output_label="P(OR)",compute=function(x) 1-(1-x)^3,format="probability",plot_title="Dokładne prawdopodobieństwo bramki OR",x_label="p identycznego wejścia",y_label="P(co najmniej jednego)",ylim=c(0,.15),note="Trzy różne, niezależne zdarzenia o tym samym p.",alt="Prawdopodobieństwo bramki OR dla trzech niezależnych wejść."),
    quiz=list(question="Czy dla OR zawsze dodajemy p_i?",choices=c("Tak"="yes","Nie; suma bywa tylko przybliżeniem"="no"),correct="no",explanation="Dokładny rachunek uwzględnia części wspólne albo używa dopełnienia przy niezależności."),
    exercises=c("Zbuduj małe drzewo inicjacja AND nieskuteczne tłumienie.","Wskaż powtórzony liść i wspólną przyczynę.")
  ),
  "model-do-decyzji" = list(
    folder="10-model-do-decyzji",lecture_id="model-do-decyzji",num="10",title="Od modelu do decyzji",
    chapters=list(
      .rc("definicja", "Zdefiniuj problem", "Zanim wybierzesz rozkład, nazwij zdarzenie i horyzont.", c("Oddziel zagrożenie, inicjator, bariery i skutek.", "Zapisz populację i jednostkę ekspozycji.")),
      .rc("alarm", "Czy alarm oznacza zdarzenie", "Aktualizujemy ocenę po informacji.", c("Używamy częstości bazowej i parametrów detektora.", "Reguła reakcji jest osobną decyzją.")),
      .rc("kontrola", "Ile wadliwych elementów", "Dobieramy dwumianowy po sprawdzeniu prób.", c("Definiujemy partię, próbę i p.", "Raportujemy prawdopodobieństwo przekroczenia limitu.")),
      .rc("czas", "Czy element dotrwa do przeglądu", "Wybieramy mechanizm czasu życia.", c("Porównujemy wykładniczy i Weibulla przez hazard.", "Odczytujemy R(t) dla czasu misji.")),
      .rc("system", "Czy zabezpieczenia zadziałają", "Redukujemy architekturę i dodajemy wspólną przyczynę.", c("Model niezależny jest punktem odniesienia.", "Wspólne zasilanie ma osobną gałąź.")),
      .rc("fta", "Złóż końcowe FTA", "Łączymy inicjację, wykrycie, tłumienie i ekspozycję.", c("Liście wskazują źródło parametrów z wcześniejszych wykładów.", "Sprawdzamy powtórzenia i kompletność.")),
      .rc("interwencja", "Jedna poprawa w budżecie", "Porównujemy wpływ możliwych działań.", c("Efekt modelowy nie wystarcza bez kosztu i wykonalności.", "Analiza wrażliwości wskazuje informację wartą zebrania."), widget=TRUE),
      .rc("notatka", "Notatka dla dyrektora", "Wynik kończy się rekomendacją i ograniczeniami.", c("Cztery zdania: wynik, częstość naturalna, założenie, decyzja.", "Niepewność i brakujące dane są częścią raportu."))
    ),
    widget=list(title="Skutek redukcji wybranego prawdopodobieństwa",input_label="Skuteczność interwencji",min=0,max=.90,value=.40,step=.05,input_short="Redukcja",output_label="Pozostałe P",compute=function(x) .02*(1-x),format="probability",plot_title="Prawdopodobieństwo po interwencji",x_label="Względna skuteczność działania",y_label="Pozostałe prawdopodobieństwo",ylim=c(0,.021),note="Punkt wyjścia P=0,02; to demonstracja wrażliwości, nie pełny ranking kosztów.",alt="Spadek prawdopodobieństwa wraz ze skutecznością interwencji."),
    quiz=list(question="Co powinno kończyć analizę?",choices=c("Sam procent"="percent","Rekomendacja z założeniami"="decision","Nazwa rozkładu"="model"),correct="decision",explanation="Wynik ma wspierać przejrzystą decyzję i wskazywać ograniczenia."),
    exercises=c("Napisz czterozdaniową notatkę dla dyrektora.","Wskaż parametr, którego lepszy pomiar najbardziej zmieni decyzję.")
  )
)
