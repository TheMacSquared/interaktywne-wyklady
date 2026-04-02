# Ćwiczenia: Rozkłady prawdopodobieństwa w BHP

**Czas:** 90 minut | **Narzędzie:** Jamovi | **Kierunek:** Inżynieria Bezpieczeństwa

---

## Blok 1: Kalkulator rozkładów (25 min)

W Jamovi: **Analyses → Exploration → Distribution** (lub moduł `distrACTION`).
Dla każdego zadania: wybierz odpowiedni rozkład, ustaw parametry, odczytaj prawdopodobieństwo.

### Zadanie 1 — Szkolenie BHP (rozkład dwumianowy)

Test BHP składa się z 25 pytań prawda/fałsz. Zaliczenie wymaga **minimum 20 poprawnych** odpowiedzi. Pracownik nie uczył się i odpowiada losowo.

a) Jaki rozkład opisuje liczbę poprawnych odpowiedzi? Podaj parametry.
b) Jakie jest prawdopodobieństwo, że pracownik zaliczy test?
c) Jaka jest oczekiwana liczba poprawnych odpowiedzi?
d) Gdyby próg zaliczenia obniżono do 15 — jak zmieniłoby się prawdopodobieństwo?

### Zadanie 2 — Wypadki przy pracy (rozkład Poissona)

W zakładzie produkcyjnym dochodzi średnio do **2.5 wypadku przy pracy miesięcznie**.

a) Jaki rozkład opisuje liczbę wypadków w miesiącu? Podaj parametr.
b) Jakie jest prawdopodobieństwo **dokładnie 5** wypadków w miesiącu?
c) Jakie jest prawdopodobieństwo **żadnego** wypadku w miesiącu?
d) Jakie jest prawdopodobieństwo **więcej niż 4** wypadków?
e) *Trudniejsze:* Jeśli w kwartale (3 miesiące) — jaki rozkład i jakie P(≥10)?

### Zadanie 3 — Poziom hałasu (rozkład normalny)

Pomiary hałasu na stanowisku w hali montażowej mają rozkład **N(82, 4)** dB (średnia 82 dB, odchylenie standardowe 4 dB). Norma BHP: **85 dB**.

a) Jaki procent pomiarów przekracza normę 85 dB?
b) Jaki procent pomiarów mieści się w przedziale 78–86 dB?
c) Poniżej jakiej wartości znajduje się 95% pomiarów?
d) *Trudniejsze:* Pracodawca twierdzi, że „prawie nigdy" nie przekracza 90 dB. Zweryfikuj — jaki procent pomiarów > 90 dB?

### Zadanie 4 — Niezawodność czujnika dymu (rozkład wykładniczy)

Czujnik dymu ma średni czas bezawaryjnej pracy (**MTBF**) wynoszący **365 dni**. Czas do awarii ma rozkład wykładniczy.

a) Jaki jest parametr λ (rate) tego rozkładu?
b) Jakie jest prawdopodobieństwo awarii w ciągu pierwszych 180 dni?
c) Jakie jest prawdopodobieństwo, że czujnik przetrwa dłużej niż 2 lata (730 dni)?
d) *Trudniejsze:* Czujnik pracuje już 200 dni bez awarii. Czy to zmienia prawdopodobieństwo awarii w następnych 180 dniach? Uzasadnij (bezpamięciowość).

---

## Blok 2: Rozpoznawanie rozkładów (25 min)

### Zadanie 5 — Który to rozkład?

Dla każdej sytuacji: **nazwij rozkład** i **podaj parametry**. Pracujcie w parach, potem dyskusja.

| | Sytuacja | Rozkład | Parametry |
|---|---|---|---|
| a) | Inspektor BHP sprawdza 20 stanowisk. Każde ma 10% szans na naruszenie przepisów. Ile naruszeń znajdzie? | | |
| b) | Średnio 3 alarmy przeciwpożarowe na tydzień w galerii handlowej. Ile alarmów w następnym tygodniu? | | |
| c) | Czas oczekiwania na karetkę pogotowia — średnia 8 min, odch. std. 2 min, rozkład symetryczny | | |
| d) | Awaria w fabryce może wystąpić w losowym momencie 8-godzinnej zmiany (każdy moment tak samo prawdopodobny) | | |
| e) | Z 50 gaśnic w magazynie, 4% jest przeterminowanych. Ile przeterminowanych w losowej kontroli? | | |
| f) | Inspektor sprawdza kolejne budynki aż do znalezienia pierwszego z naruszeniem przepisów ppoż. (szansa naruszenia: 15%). Ile budynków sprawdzi? | | |
| g) | Średnio 1 poważny wypadek co 20 dni roboczych. Ile dni do następnego wypadku? | | |
| h) | Waga ładunku na palecie — średnia 500 kg, odch. std. 30 kg | | |

### Zadanie 6 — Trudniejsze: powiązania między rozkładami

**6a)** W firmie kurierskiej średnio dochodzi do **4 kolizji drogowych miesięcznie**.

- Jaki rozkład opisuje **liczbę** kolizji w miesiącu?
- Jaki rozkład opisuje **czas** (w dniach) między kolejnymi kolizjami?
- Podaj parametry obu rozkładów. Jaki jest związek między nimi?

**6b)** Partia 100 środków ochrony indywidualnej (rękawice). Wadliwość wynosi 3%.

- Jaki rozkład opisuje liczbę wadliwych rękawic w partii? Podaj parametry.
- Kontroler jakości sprawdza rękawice po kolei. Jaki rozkład opisuje numer rękawicy, przy której natrafi na pierwszą wadliwą?
- Oblicz: P(≥5 wadliwych w partii) oraz E(numer pierwszej wadliwej).

**6c)** Stężenie pyłu na stanowisku ma rozkład N(4.2, 0.8) mg/m³. Norma BHP wynosi **5.0 mg/m³**.

- Jaki procent pomiarów przekracza normę?
- Pracodawca musi zapewnić, że **mniej niż 5% pomiarów** przekracza normę. Do jakiej wartości musiałby obniżyć średnie stężenie (przy tym samym σ)?

---

## Blok 3: Analiza danych w Jamovi (40 min)

Otwórz pliki CSV z folderu `dane/` w Jamovi.

### Zadanie 7 — Wypadki miesięcznie (`wypadki_miesiecznie.csv`)

a) Otwórz plik. Zrób **histogram** zmiennej `liczba_wypadkow` (Exploration → Descriptives → Plots → Histogram).
b) Oblicz **średnią** i **wariancję**. Czy są zbliżone do siebie?
c) Jaki to sugeruje rozkład? Podaj parametr λ.
d) Używając kalkulatora rozkładów z parametrem λ = średnia z danych:
   - Oblicz P(X ≥ 5)
   - Oblicz P(X = 0)
e) Porównaj teoretyczne prawdopodobieństwa z empirycznymi częstościami w danych (ile miesięcy miało ≥5 wypadków? ile miało 0?).

### Zadanie 8 — Hałas na stanowiskach (`halas_stanowiska.csv`)

a) Rozdziel dane na dwa stanowiska (Data → Filters: `stanowisko == "A_montaz"`).
b) Dla każdego stanowiska zrób **histogram** i oblicz **statystyki opisowe** (średnia, mediana, odch. std., skośność).
c) Które stanowisko ma rozkład bliższy normalnemu? Po czym to poznajesz?
d) Dla stanowiska o rozkładzie normalnym: jaki % pomiarów przekracza normę 85 dB? (użyj kalkulatora z parametrami z danych)
e) *Trudniejsze:* Dlaczego stanowisko B mogłoby mieć rozkład skośny? Podaj hipotezę techniczną.

### Zadanie 9 — Kontrola kasków (`kontrola_kaskow.csv`)

a) Zrób **histogram** zmiennej `liczba_wadliwych`.
b) Oblicz średnią liczbę wadliwych kasków na partię.
c) Wiedząc, że partia liczy 30 kasków — oszacuj prawdopodobieństwo wadliwości p (p = średnia / 30).
d) Używając B(30, p): oblicz P(≥ 3 wadliwych w jednej partii).
e) Kierownik jakości chce odrzucać partię, jeśli jest ≥ 4 wadliwych. Jak często partia zostanie odrzucona?

### Zadanie 10 — Czas między incydentami (`czas_miedzy_incydentami.csv`)

a) Zrób **histogram** zmiennej `dni_od_poprzedniego`. Jaki kształt ma rozkład?
b) Oblicz **średnią** i **odchylenie standardowe**. Czy są do siebie zbliżone? (To cecha rozkładu wykładniczego!)
c) Jaki parametr λ (rate) sugerują dane? (λ = 1/średnia)
d) Używając Exp(λ): oblicz P(następny incydent w ciągu 7 dni).
e) *Trudniejsze:* Przelicz na Poissona — jeśli średni czas między incydentami wynosi X dni, to ile incydentów oczekujemy w ciągu 30 dni? Jaki λ dla rozkładu Poissona? Sprawdź: P(≥ 3 incydenty w miesiącu).

---

## Podsumowanie

Po zakończeniu ćwiczeń odpowiedz na pytania:

1. Który rozkład najczęściej widzisz w kontekście BHP i dlaczego?
2. Jak wygląda histogram danych z rozkładu wykładniczego? Czym różni się od normalnego?
3. Jaki jest praktyczny sens „bezpamięciowości" rozkładu wykładniczego dla bezpieczeństwa? (Podpowiedź: czy stary czujnik jest mniej niezawodny niż nowy?)
