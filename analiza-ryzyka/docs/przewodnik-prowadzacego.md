# Analiza ryzyka — przewodnik prowadzącego i klucz ćwiczeń

Wersja po korektach spójności, 7 września 2026 r. Wszystkie wartości Bananpolu
są fikcyjne; limity i koszty służą ćwiczeniom, nie są normami ani cenami.

## Zakres i rytm zajęć

Sylabus obejmuje probabilistyczne podstawy ryzyka i niezawodności. Kurs dodaje
krótki kontekst identyfikacji scenariuszy, niepewności danych i decyzji.
Wymiar planowany to 15 spotkań po 90 minut: bloki 01–05 po jednym, 06–10 po
dwa. To 30 godzin dydaktycznych po 45 minut; należy porównać z pełną kartą
przedmiotu. Efekty formalne i zasady zaliczenia nie są zawarte w lokalnym
wyciągu sylabusa.

Propozycja pojedynczego spotkania: 10 minut na problem i głosowanie, 20 na
intuicję i eksperyment, 25 na model i rachunek, 20 na zadanie w parach,
10 na omówienie błędów, 5 na podsumowanie. W dłuższych blokach rozdziały
„Część B” wyznaczają drugi etap. Rozbudowany Monty Hall w 02 jest dygresją do
skrócenia, gdy brakuje czasu na warunkowe i całkowite. Q–Q w 06 można skrócić;
nie pomijać definicji misji, wspólnej przyczyny ani oceny niepewności w finale.

Po kursie student powinien umieć samodzielnie nazwać zdarzenie i ekspozycję,
wybrać model, policzyć wynik, wskazać założenie i obronić działanie. Quiz
wspiera diagnozę tych umiejętności; samo rozpoznanie odpowiedzi nie zastępuje
samodzielnego zadania. Klucze pięciu pytań każdego bloku 02–10 i objaśnienia
znajdują się także w aplikacji po wysłaniu odpowiedzi.

## 01 — Język ryzyka

- Komunikat o trzech wypadkach: brakuje definicji zdarzenia, populacji
  narażonej, ekspozycji, okresu i skutków. Z liczników 3 i 5 nie wynika ranking
  bezpieczeństwa. Częstość na zmianę i liczba wypadków na osobogodzinę mają
  różne mianowniki; nie można ich bezpośrednio porównywać.
- Sto kontroli rampy: `|A∩B|=6`, `|A∪B|=28+17−6=39`, poza sumą 61.
  Zdarzenia nie są rozłączne. Samo niezerowe przecięcie nie dowodzi zależności.
- Dobór opisu: losowanie jednej z 30 palet, z których 4 są wadliwe —
  klasyczne `4/30`, o ile losowanie jest jednostajne; 8 zdarzeń na 100 zmian —
  częstość empiryczna `0,08`; prognoza przy nowej procedurze i pogodzie wymaga
  modelu oraz danych o nowych warunkach.
- Laboratorium: zagrożenie to możliwość niebezpiecznego uwolnienia gazu,
  ekspozycja dotyczy obecnych osób; zdarzeniem może być przekroczenie
  ustalonego stężenia w zdefiniowanym okresie, skutkiem np. zatrucie, barierą
  detekcja i procedura reakcji. Alarm jest obserwacją, nie dowodem faktycznej
  ekspozycji. Akceptuj inne jednoznaczne definicje z poprawnym mianownikiem.
- Nowe zadanie scenariuszowe: student wskazuje źródło, inicjację, narażonych,
  barierę zapobiegającą, barierę ograniczającą skutki, rolę człowieka i brak
  danych. Nie wymagaj jednego identycznego scenariusza od wszystkich par.

## 02 — Warunki zmieniają ocenę

1. `P(A)=0,1×0,12+0,9×0,005=0,0165`. Na 1000 porównywalnych zmian oczekujemy
   16,5 zmian z incydentem, nie dokładnie tylu w realizacji. Zaokrąglona siatka
   pokazuje 12+4=16; udziały z ilustracji nie są dokładnymi parametrami modelu.
2. Wspólne zasilanie może wyłączyć obie funkcje. Rozwiązanie wymaga osobnego
   zdarzenia C i danych o lokalnych awariach poza C. Nie mnożyć bezwarunkowych
   prawdopodobieństw, które obejmują już wspólne awarie.
3. Transfer: `P(wypadek na zmianie | zmiana nocna)` ma w mianowniku
   porównywalne zmiany nocne. `P(noc | wypadek)` ma w mianowniku zmiany
   z wypadkiem. Pytaj też, czy noc jest przyczyną, czy wskaźnikiem innych
   warunków: zmęczenia, obsady, rodzaju prac.

Typowa pomyłka: dodawanie samych ryzyk warunkowych bez wag grup.

## 03 — Alarm i prawda

1. Na 10 000 zmian: 100 awarii, 9900 bez awarii; TP=95, FN=5, FP=495,
   TN=9405. Alarmów 590, posterior `95/590≈0,1610`.
2. Do łączenia dwóch czujników potrzeba niezależności warunkowej przy awarii
   i przy jej braku. Osobne obudowy nie wykluczają wspólnego zakłócenia.
3. Transfer: np. fikcyjna populacja 10 000 testowanych, 100 ze stanem
   wykrywanym, 90 prawdziwie dodatnich oraz 198 fałszywie dodatnich daje
   `90/288=31,25%`. Mianownikiem są wszystkie dodatnie wyniki, a nie wszyscy
   badani. To przykład rachunkowy, nie parametry konkretnego testu.

Nowy przykład kosztowy: pełna skuteczność reakcji daje koszty 100 zł vs 2000q,
więc próg q=0,05. Przy skuteczności 50% porównujemy 100+1000q z 2000q,
więc próg q=0,10. Przy równości decyzje mają ten sam koszt oczekiwany.
Nie przenosić przykładu straty materialnej wprost na ocenę ryzyka urazu.

## 04 — Wiele prób

1. Dla Bin(100; 0,02): `P(X=0)=0,1326196`, `P(X=2)=0,2734139`,
   `P(X≥1)=0,8673804`, `E(X)=2`. Wynik jednej partii nie musi wynieść 2.
2. Dwie dostawy: zagrożona jest stałość p; wspólna jakość partii może również
   tworzyć zależność. Przy próbkowaniu dużej części skończonej partii bez
   zwracania trzeba dodatkowo rozważyć model hipergeometryczny; wystarczy
   nazwać to ograniczenie, nie trzeba rozwijać nowego działu.
3. Mocowania: próba to kontrola jednego mocowania, sukces statystyczny to
   jednoznacznie określona wada, n=30. „Sukces” jest kodem 1, nie oceną
   pożądanego wyniku. Trzeba uzasadnić porównywalność i niezależność kontroli.

Nowy przykład niepewności: dla zera wad w n=100 jednostronna górna granica
95% wynosi `1−0,05^(1/100)=0,02951305`. To granica z procedury pokrycia przy
ustalonym n, nie posterior parametru. Podstawienie tej granicy do prognozy
następnej partii 100 daje `P(X≥1)=0,95`; nie oznacza to, że prawdopodobieństwo
wad w nowej partii zostało ustalone na 95%.

## 05 — Ile prób do zdarzenia

1. Przy p=0,1 i r=3: średnia 30 prób, `P(X≤40)=0,7771919`;
   kwantyl 95% to 61. W R: `pnbinom(40−3, size=3, prob=.1)` oraz
   `qnbinom(.95, size=3, prob=.1)+3`.
2. Przy losowym p między seriami `E(X)=rE(1/p)`, więc ta sama średnia p nie
   utrzymuje średniej X. Zmienność wydłuża przeciętne oczekiwanie i może
   zwiększać ogon. Symulacja ogranicza p do [0,005; 0,95], co dodatkowo
   zmienia jego rozkład; nie nazywaj tego dokładnym porównaniem przy równych
   średnich p.
3. Dla dwóch naruszeń `E(X)=2/p`; plan na poziomie 95% to
   `qnbinom(.95, size=2, prob=p)+2`. Student powinien podać p, regułę
   zatrzymania oraz reakcję po wyczerpaniu limitu. Bez podanego p dopuszczalna
   jest odpowiedź symboliczna.

Typowa pomyłka: pominięcie +r, gdy oprogramowanie liczy niepowodzenia.

## 06 — Zmienność i próg

1. `z=(85−82)/3=1`, `P(T>85)=0,1586553`, czyli około 159 na 1000
   porównywalnych pomiarów. Nie jest to automatycznie 159 zmian z choć jednym
   przekroczeniem.
2. Model normalny podważają systematyczna skośność i odchylenia ogonów na
   Q–Q; przypadkowy rozrzut punktów nie jest sam w sobie dowodem błędu.
   Dobry opis centrum nie wystarcza do prognozy rzadkich przekroczeń.
3. Dla niezależnych normalnych L~N(85;8), S~N(95;7), gdzie drugi parametr to
   odchylenie: `D=S−L~N(10; sqrt(113))`, `P(D<0)≈0,173424`.
   Inne liczby z suwaków dają inny wynik. Przy zależności potrzebna jest
   kowariancja i uzasadnienie wspólnego rozkładu.

Zmniejszenie σ zmniejsza prawy ogon tylko przy c>μ; przy c<μ go zwiększa,
przy c=μ pozostawia 0,5. Zmiana samego progu zmienia kryterium, nie proces.

## 07 — Czas życia

1. `R(1000)=exp(−1000/1500)=0,5134171`. MTTF nie jest terminem gwarantowanej
   awarii ani zalecanym okresem przeglądu.
2. Działające egzemplarze są prawostronnie cenzorowane; wnoszą informację
   `T>t_obserwacji`. Ich usunięcie pozostawia głównie krótsze czasy. Samo
   policzenie średniej obserwowanych czasów, traktując cenzorowanie jak
   awarię, też jest błędem.
3. Zużycie sugeruje β>1, hazard rosnący. β=1 oznacza stały hazard,
   β<1 malejący. Kształt rozkładu wspiera hipotezę o mechanizmie, nie dowodzi
   go samodzielnie. Pojedynczy Weibull nie ma pełnego hazardu wannowego.

Uzupełnienia: h ma jednostkę 1/h; hΔt przybliża warunkowe prawdopodobieństwo
w krótkim przedziale. W jednorodnym procesie Poissona licznik N(t) ma średnią
λt, odstępy są wykładnicze, czas do k-tego zdarzenia ma Erlanga. Ogólna gamma
ma także niecałkowity kształt bez dosłownej interpretacji k etapów.
Przegląd nie oznacza odnowienia: dopiero określona naprawa lub wymiana zmienia
stan urządzenia. Gotowość po naprawach różni się od niezawodności misji.

## 08 — Niezawodność systemu

1. Dla φ=C·[1−(1−A)(1−B)] tabela stanów:

   | A | B | C | φ |
   |---|---|---|---|
   | 0 | 0 | 0 | 0 |
   | 0 | 0 | 1 | 0 |
   | 0 | 1 | 0 | 0 |
   | 0 | 1 | 1 | 1 |
   | 1 | 0 | 0 | 0 |
   | 1 | 0 | 1 | 1 |
   | 1 | 1 | 0 | 0 |
   | 1 | 1 | 1 | 1 |

   Naprawa dowolnego elementu nie zmienia 1 na 0. A jest istotny przy B=0,
   C=1; B analogicznie, C przy A=1. Model jest koherentny. φ=A dla modelu
   trzech elementów pomija wpływ B i C, więc nie spełnia istotności wszystkich.
2. Szereg: `0,92×0,95×0,98=0,85652` przy niezależności i wspólnym czasie.
3. Wspólne zasilanie: warunkowo bez jego awarii redundancja daje
   `1−(1−R_A)(1−R_B)`. Pomnożyć przez `1−q`, jeśli lokalne R nie zawierają
   już wspólnej przyczyny. Rezerwa oczekująca wymaga innego opisu niż dwie
   równocześnie pracujące gałęzie.
4. Hamowanie: `φ=C·[1−(1−A)(1−B)]` tylko jeśli jedna gałąź rzeczywiście
   wystarcza do wymaganej funkcji. Dla niezależnych elementów
   `R=R_C[1−(1−R_A)(1−R_B)]`. Warunki skuteczności hamowania trzeba nazwać;
   dwa fizyczne przewody nie dowodzą tej architektury.

## 09 — Drzewo błędów

1. Łańcuch wymagający detekcji i modułu wykonawczego:
   `0,005[1−0,95×0,92]=0,00063`. Dwie samodzielne bariery:
   `0,005×0,05×0,08=0,00002`. Różnica 31,5 raza wynika z architektury.
   Liczby barier są warunkowe przy inicjacji i niezależne w tym warunku.
   Po dodaniu C do łańcucha, przy q=0,01 i lokalnych d₀=0,05, s₀=0,08:
   `P(TOP)=0,005[0,01+0,99(1−0,95×0,92)]=0,0006737`.
   Przekroje: {I,C}, {I,D₀}, {I,S₀}. Żaden nie musi być krótszy od poprzednich.
2. Rachunek Bananpolu to ten sam wariant łańcucha: 0,00063, czyli 0,063%
   na rok w przyjętym modelu co najwyżej jednej inicjacji.
3. Powtórzony liść C: `C∩C=C∪C=C`, więc wynik to q. Dla q=0,05 błędne
   AND niezależnych kopii daje 0,0025, a OR 0,0975. Minimalność przekroju
   oznacza brak zbędnego elementu, a nie najmniejszą liczbę liści globalnie.
4. Przykład transferowy: utrata zasilania urządzenia
   `TOP=C ∪ (A∩B)`, gdzie C jest wspólną awarią rozdziału, a A i B awariami
   lokalnych źródeł bez C. Przy niezależności wynik
   `q+(1−q)p_Ap_B`, przekroje {C}, {A,B}. Przełączenie UPS, czas podtrzymania
   i obsługa człowieka wymagają dodatkowych założeń; nie są automatycznie
   zawarte w tym prostym drzewie.

## 10 — Studium misji ochrony termicznej

To nowy spójny scenariusz, a nie roczne drzewo pożaru z 09. I oznacza potrzebę
chłodzenia na początku misji; D — jej przeoczenie; S — brak ciągłego chłodzenia
w misji. TOP=I∩(D∪S). Stan początkowy jest nowy i sprawny, brak napraw;
parametry pracy układu są warunkowe przy I. Detekcja ma osobne zasilanie,
więc w modelu D i S są niezależne przy I. Sam TOP nie oznacza jeszcze urazu
lub pożaru. Posterior karty alarmu nie jest wejściem do drzewa.

1. Misja 1000 h, P(I)=0,005, czułość 0,95, R_P=0,98, R_C=0,95:

   | Model wentylatora | R wentylatora | R systemu | P(TOP) |
   |---|---:|---:|---:|
   | Wykładniczy MTTF=1500 h | 0,513417 | 0,710574 | 0,001624775 |
   | Weibull β=2, η=1700 h | 0,707498 | 0,851346 | 0,000956107 |

   `R_sys=R_P R_C[1−(1−R_fan)²]`; `P(TOP)=0,005[1−0,95 R_sys]`.
   Na 10 000 misji odpowiada to około 16 i 10 utratom ochrony.
2. Weibull: `R(3000)=0,0444146`, `R(1000)³=0,3541402`, `R(500)⁶=0,5950968`.
   Pierwszy wynik dotyczy jednego starzejącego się urządzenia, drugi trzech
   misji nowych urządzeń, trzeci sześciu. Bez odnowy warunkowe iloczyny
   teleskopują do R(3000). Dla wykładniczego brak pamięci daje równość tych
   trzech wyrażeń, ale nie uprawnia do przyjęcia jej dla Weibulla.
3. Wariant bazowy wykładniczy, u=0,2, budżet 2. Dostępne są czujnik (koszt 2)
   i ograniczenie źródła ciepła (koszt 1):

   | Działanie | Optymistyczny | Bazowy | Ostrożny |
   |---|---:|---:|---:|
   | Lepszy czujnik | 0,000917934 | 0,001535953 | 0,002301275 |
   | Ograniczenie źródła ciepła | 0,000397242 | 0,000812387 | 0,001436016 |

   Źródło ciepła ma niższe P(TOP) w każdym scenariuszu i spełnia limit 0,002.
   Czujnik przekracza limit w ostrożnym. Po zmianie limitu na 0,0001 żadne
   z tych działań nie wystarcza. Dla Weibulla wartości się zmienią; oba
   mieszczą się w limicie 0,002, ale źródło ciepła nadal wygrywa w tym zestawie.
   Mnożnik scenariusza działa na inicjację, przeoczenia i skumulowane hazardy;
   skuteczność działań zmienia się z 60% przez 50% do 40%. To zadane scenariusze,
   nie przedziały ufności i nie estymacje skuteczności.
4. Transfer oceniaj według poniższej rubryki. Właściciel działania i termin
   sprawdzenia muszą być jawne. Akceptuj rekomendację „potrzebujemy dodatkowych
   danych lub zmiany projektu”, jeżeli wynika z poprawnej analizy.

## Rubryka samodzielnego zadania końcowego

Propozycja do wykorzystania dydaktycznego, nie ustalenie formalnego zaliczenia:
po 0–2 pkt za każdy element (maksymalnie 12).

| Element | Pełne 2 punkty |
|---|---|
| Scenariusz | Zdarzenie, narażenie, skutek i rola barier są jednoznaczne. |
| Dane | Jednostki, horyzonty, warunki i źródła parametrów są nazwane. |
| Model | Architektura odpowiada funkcji, zależności są ujawnione. |
| Rachunek | Właściwy model i wynik; średnia nie jest traktowana jak gwarancja. |
| Niepewność | Sprawdzono zmianę wyniku i rekomendacji przy innych założeniach. |
| Decyzja | Koszt, wykonalność, pozostałe ryzyko, kryterium i kontrola są jawne. |

Nie dawaj pełnych punktów za poprawne mnożenie przy błędnie opisanych
zdarzeniach. Przy otwartych zadaniach dopuszczaj kilka architektur, jeśli
student potrafi wskazać warunki ich poprawności.

## Źródła do przygotowania prowadzącego

- [NIST — Assessing Product Reliability](https://www.itl.nist.gov/div898/handbook/apr/apr_d.htm):
  funkcje czasu życia, cenzorowanie, modele niezawodności i estymacja.
- [NASA — Fault Tree Handbook with Aerospace Applications](https://s3vi.ndc.nasa.gov/ssri-kb/static/resources/Fault%20Tree%20Handbook_NASA.pdf):
  rozdziały 6–8 o logice, wejściach ilościowych i rezerwie; źródło do
  rozróżniania danych na żądanie i awarii w czasie.
- [ISO 31000 — publiczny opis zakresu](https://www.iso.org/standard/65694.html):
  kontekst identyfikacji, analizy, oceny, działania i monitorowania ryzyka.
  Kurs nie wymaga przerobienia pełnego tekstu normy.
