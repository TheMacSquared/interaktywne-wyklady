# Rozdziały wprowadzające do wykładów 03–06 — propozycja

Wzór: rozdział 1 wykładu 07 „Pytania o wentylator” (`07-czas-zycia/modules/block.R`, `zycie_map_table`).

Szkielet, który powtarzamy w każdym wykładzie:

1. **Scena** — jedno miejsce w Bananpolu, jedna osoba, „ma dziś na biurku N spraw”, wszystkie o tym samym obiekcie.
2. **Lista spraw** (`tags$ul`) — konkretne, z liczbami z wykładu.
3. **Pytanie przewodnie** — z `plan-kanoniczny.md` („Mapa serii”), sformułowane jako rozwidlenie: „jeśli A, to…; jeśli B, to…”.
4. **Mapa wykładu** (`lc_table`): pytanie osoby → rozdział → narzędzie. Kolumna „Część” tylko tam, gdzie wykład ma części A/B (w 03–06 nie ma).
5. **Co już umiemy** — co wraca z wcześniejszych wykładów i jedno nowe pojęcie.
6. **Dane Bananpolu** — przeniesiony callout z dotychczasowego rozdziału 1.
7. Kolejne rozdziały zaczynają się od „Pierwsze / Drugie … pytanie <osoby>: …”.

---

## Wykład 03 — Alarm i prawda

- **id / title / hook:** `pytania`, „Pytania o alarm”, „Jeden czujnik, cztery decyzje”
- **lead:** „Każda decyzja o czujniku zależy od tego, jak rzadko zdarza się to, przed czym ostrzega.”

**Scena.** Rano po nocnej zmianie kierownik utrzymania ruchu dojrzewalni ma na biurku cztery sprawy dotyczące tego samego czujnika przegrzania.

- Dyżurny trzeci raz w tym miesiącu wysłał w nocy ekipę po alarmie i trzeci raz awarii nie było. Pyta, czy czujnik jest zepsuty.
- Karta katalogowa czujnika podaje: wykrywa 95% awarii, fałszywie alarmuje w 5% zmian bez awarii.
- Dział techniczny chce zamontować ten sam model w nowej hali, w której awarie zdarzają się częściej — albo dołożyć drugi czujnik „dla pewności”.
- Do zatwierdzenia czeka instrukcja dyżurnego: po jakim sygnale wysyłać ekipę, a kiedy wystarczy sprawdzenie zdalne.

**Pytanie przewodnie.** Żadnej z tych spraw nie rozstrzygnie liczba 95% z karty. Wszystkie zależą od jednego pytania: czy alarm oznacza awarię? Odpowiedź zależy mniej od czujnika, a bardziej od tego, jak rzadko zdarza się awaria. Jeśli rzadko — większość alarmów jest fałszywa nawet przy dobrym czujniku i trzy puste wyjazdy niczego nie dowodzą. Jeśli często — alarmowi można ufać.

**Mapa wykładu.**

| Pytanie kierownika | Rozdział | Narzędzie |
|---|---|---|
| Czy czujnik jest zepsuty, skoro wykrywa 95% awarii, a wyjazdy są puste? | Kierunek warunku | P(alarm \| awaria) a P(awaria \| alarm), mianownik |
| Co dokładnie mówią liczby z karty katalogowej? | Czułość i swoistość | tablica 2×2, czułość, FPR |
| Jaka jest szansa awarii, gdy dzwoni telefon? | Wzór Bayesa | naturalne częstości, wzór Bayesa |
| Czy ten sam czujnik sprawdzi się w nowej hali? Czy drugi czujnik pomoże? | Częstość bazowa | częstość bazowa, szanse, warunkowa niezależność |
| Kiedy wysyłać ekipę? | Próg reakcji | macierz konsekwencji, próg reakcji |

**Co już umiemy.** Z wykładu 02: prawdopodobieństwo warunkowe, drzewo i prawdopodobieństwo całkowite oraz pierwsza obserwacja, że P(incydent | przegrzanie) = 0.12 to inna liczba niż P(przegrzanie | incydent) ≈ 0.71. Dochodzi jeden nowy manewr: odwrócenie warunku — wnioskowanie od sygnału, który widać, do stanu, którego nie widać.

**Dane Bananpolu.** Obecny callout z sekcji „Alarm na zmianie” (częstość awarii 0.01 na zmianę, czułość 0.95, FPR 0.05, 10 000 zmian).

**Co trzeba przestawić.**
- Serpuchów: obecnie otwiera wykład. Propozycja: zostaje pierwszą sekcją rozdziału „Kierunek warunku” (zaraz po wstępie); scena biurka jest krótka, więc hak historyczny nadal pada w pierwszych minutach. Alternatywa: Serpuchów jako hak *przed* biurkiem w rozdziale wstępnym — odchodzi od wzoru 07, ale zachowuje obecne otwarcie.
- Intro „Kierunku warunku” → „Pierwsze pytanie kierownika: czy czujnik jest zepsuty…”. Głosowanie i dziennik dyżurnego zostają w tym rozdziale.

---

## Wykład 04 — Wiele prób

- **id / title / hook:** `pytania`, „Pytania o dostawę”, „Jedna rampa, cztery decyzje”
- **lead:** „Każda decyzja o dostawie zależy od tego, czy kontrole są powtórzeniami tej samej próby.”

**Scena.** Inspektor na rampie rozładunkowej ma dziś na biurku cztery sprawy dotyczące przyjęcia dostaw.

- Nowy dostawca przysłał partię stu zaworów do instalacji chłodniczej; w umowie stoi „wadliwość 2%”.
- Na rampie stoi skrzynia 12 kiści z Ekwadoru; w pojedynczej kiści pająk siedzi z prawdopodobieństwem 0.1. Ile kiści trzeba będzie odłożyć?
- Magazyn planuje zapas: „średnio 2 wadliwe zawory na partię, więc 2 zapasowe wystarczą”.
- Dostawca proponuje plan odbioru: sprawdzamy część partii i przyjmujemy ją, jeśli wad będzie nie więcej niż ustalony limit.

**Pytanie przewodnie.** Wszystkie cztery sprawy pytają o to samo: ile zdarzeń wystąpi w ustalonej liczbie prób? Zanim padnie wzór, trzeba odpowiedzieć na pytanie wcześniejsze: czy kontrole są naprawdę powtórzeniami tej samej próby — z tym samym p i niezależne od siebie? Jeśli tak, liczba wad ma rozkład dwumianowy i każdą z czterech decyzji da się policzyć. Jeśli nie — wady przychodzą seriami albo p zmienia się między partiami — model obiecuje więcej, niż daje.

**Mapa wykładu.**

| Pytanie inspektora | Rozdział | Narzędzie |
|---|---|---|
| Co jest pojedynczą próbą: zawór, kiść, skrzynia? | Próba Bernoulliego | jednostka ekspozycji, wynik 0/1 |
| Dlaczego jedna partia zaskakuje, a sto partii już nie? | Schemat Bernoulliego | stałe p, niezależność |
| Jaka jest szansa, że pająki siedzą w więcej niż dwóch kiściach? | Rozkład dwumianowy | zmienna losowa, B(n, p) |
| Czy 2 zapasowe zawory wystarczą? | Wartość oczekiwana | np, „co najmniej jeden” przez zdarzenie przeciwne |
| Czy przyjąć plan odbioru dostawcy? | Plan odbioru | reguła (n, c), dwa ryzyka planu |

**Co już umiemy.** Z wykładu 01: zdarzenie, przestrzeń wyników, częstość. Z wykładu 02: niezależność i reguła iloczynu — teraz zastosujemy ją do całej serii naraz. Dochodzi jedno nowe pojęcie: zmienna losowa, czyli pomost od zdarzeń do liczb, które można dodawać i uśredniać.

**Dane Bananpolu.** Obecny callout (skrzynia 12 kiści, p = 0.1) uzupełniony o zawory: partia 100 sztuk, p = 0.02.

**Co trzeba przestawić.**
- Akapit „Nowy dostawca przysłał…” z intro „Próby Bernoulliego” przechodzi do sceny; intro zaczyna się od „Pierwsze pytanie inspektora: co jest pojedynczą próbą…”.
- Zdanie „Dotychczas pytaliśmy o pojedynczą zmianę albo pojedynczy alarm…” → do „Co już umiemy”.
- Do sprawdzenia: czy plan odbioru w rozdziale 5 ma konkretne (n, c) — jeśli tak, wpisać je do czwartej sprawy zamiast „ustalonego limitu”.

---

## Wykład 05 — Ile prób do zdarzenia

- **id / title / hook:** `pytania`, „Pytania o audyt”, „Jeden audyt, cztery decyzje”
- **lead:** „Każda decyzja o audycie zależy od tego, kiedy kończymy kontrolę.”

**Scena.** Kierownik działu jakości ma dziś na biurku cztery sprawy dotyczące audytu zabezpieczeń ładunku na paletach.

- Do analizy przyczyn potrzebne są trzy palety z wadliwym zabezpieczeniem; wadliwa jest mniej więcej co dziesiąta paleta.
- Audytor pyta, ile kontroli potrwa, zanim znajdzie pierwszą wadę — i czy brak wady po dwudziestu kontrolach to już podejrzane.
- Planista zarezerwował na audyt 30 kontroli, „bo średnio tyle wystarczy”.
- Palety pochodzą od dwóch dostawców o różnej jakości. Czy jeden plan audytu wystarczy dla obu?

**Pytanie przewodnie.** Wszystkie sprawy pytają: ile prób potrzeba do kolejnego wykrycia? Kluczowe jest jedno rozwidlenie: kiedy kończymy kontrolę? Jeśli po ustalonej liczbie palet — losowa jest liczba wad i wracamy do wykładu 04. Jeśli po ustalonej liczbie wad — losowa staje się liczba kontroli, a jej rozkład ma długi ogon, więc plan trzeba oprzeć na ogonie, nie na średniej.

**Mapa wykładu.**

| Pytanie kierownika | Rozdział | Narzędzie |
|---|---|---|
| Co jest losowe: liczba wad czy liczba kontroli? | Reguła zatrzymania | reguła zatrzymania |
| Ile kontroli do pierwszej wady? | Rozkład geometryczny | Geo(p), długi ogon |
| Ile kontroli do trzech wad? | Rozkład ujemny dwumianowy | suma czasów oczekiwania, NB(r, p) |
| Czy 30 zaplanowanych kontroli wystarczy? | Limit planistyczny | kwantyl zamiast średniej |
| Czy jeden plan wystarczy dla dwóch dostawców? | Założenia modelu | zmienne p między partiami |

**Co już umiemy.** Z wykładu 04: schemat Bernoulliego, rozkład dwumianowy i plan odbioru. Pojedyncze kontrole są dokładnie te same; zmienia się tylko reguła zatrzymania. Dochodzi jedno nowe pojęcie: czas oczekiwania mierzony liczbą prób — w wykładzie 07 zmierzymy go w godzinach.

**Dane Bananpolu.** Obecny callout (p = 0.10, r = 3, kontrola jednej palety, jedna seria audytowa).

**Co trzeba przestawić.**
- Akapity 1–2 intro „Reguły zatrzymania” (dwa powody audytu, szukanie na siłę) są już prawie sceną — pierwszy przechodzi do wstępu, drugi zostaje w rozdziale; akapit 3 („Pojedyncze próby są dokładnie te same…”) → „Co już umiemy”.
- Liczba „po dwudziestu kontrolach” — do sprawdzenia z przykładem w rozdziale geometrycznym (w intro: „co dziesiąta seria przekroczy 22 kontrole”); można wpisać 22 dla spójności.

---

## Wykład 06 — Przekroczenie progu (wdrożone 25.11.2026)

Ten wykład ma już oś („raport do poprawienia”, P21) — wstęp ją tylko wydziela i dokłada mapę.

- **id / title / hook:** `pytania`, „Raport o łożysku”, „Siedem wierszy, każdy do poprawki”
- **lead:** „Każda poprawka raportu zależy od tego, czy przekroczenia biorą się ze średniej, czy z rozrzutu.”

**Scena.** Obecny pierwszy akapit intro („Na biurko kierownika dojrzewalni trafia raport…”) i pod nim raport P21 w złej wersji (`z6p_get("z6p_21_ui")`). Raport pełni rolę listy spraw — zamiast `tags$ul`.

**Pytanie przewodnie.** Jak często przekraczamy granicę? Raport odpowiada „w normie”, bo patrzy tylko na średnią. Rozwidlenie: czy przekroczenia to wyjątki, czy zwykła praca? Jeśli rozrzut jest mały, próg leży daleko w ogonie i przekroczenia są rzadkie. Jeśli duży — przy tej samej średniej co szósty pomiar przekracza próg, a o tym, co robić (chłodzić, stabilizować, ruszać próg), decyduje źródło: średnia czy rozrzut.

**Mapa wykładu.**

| Wiersz raportu | Rozdział | Narzędzie |
|---|---|---|
| 2. Poziom ryzyka | Zmienność a próg | odchylenie standardowe σ |
| — (skąd liczba 159 na 1000) | Rozkład normalny | gęstość, pole pod krzywą, N(μ, σ) |
| 3. Częstość alarmu, 4. Źródło ryzyka | Prawdopodobieństwo przekroczenia | standaryzacja z, „co najmniej raz na zmianę” |
| 6. Postępowanie z ryzykiem | Hierarchia interwencji | przesunąć μ, zwęzić σ, zmienić próg |
| drugi raport: lina dźwigu | Obciążenie–wytrzymałość | P(L > S), margines D = S − L |
| 5. Założenia i niepewność | Wykres kwantylowy | wykres kwantylowy, ogon |
| 1. Zakres i kryteria, 7. Monitorowanie | Komunikat o ryzyku | komunikat w trzech zdaniach |

**Co już umiemy.** Z wykładów 04–05: zmienna losowa, wartość oczekiwana i ogon rozkładu — ale zmienne zliczały zdarzenia. Z wykładu 01: częstość stabilizuje się w długiej serii. Dochodzi zmienna ciągła: temperatura nie zlicza niczego, więc prawdopodobieństwa czytamy z pól pod gęstością.

**Dane Bananpolu.** Obecny callout (82°C, σ = 3°C, próg 85°C).

**Co trzeba przestawić.**
- P21 przenosi się z rozdziału „Zmienność a próg” do wstępu; poprawka `z6p_21_fix(1)` zostaje na końcu „Zmienności a próg”.
- Akapit 2 obecnego intro („Ten wykład wprowadza zmienne ciągłe…”) → pytanie przewodnie + „Co już umiemy”.
- Intro „Zmienności a próg” → „Pierwszy wiersz do poprawki: poziom ryzyka. …”.
- Numeracja rozdziałów przesuwa się o 1: w P21 caption „poprawiony raport składamy w rozdziale 7”, pola `ch =` w `z6p21_rows`, odwołania „rozdz. 7”, „wyprzedza rachunek z rozdziału 3”, „wiersz 2 raportu poprawiliśmy” itp. trzeba przenumerować (w 07 zrobiliśmy to samo).
