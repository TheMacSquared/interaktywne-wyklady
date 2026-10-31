# Klawiatura, powiększenie i czytelność widgetów 00–04

Ocena z 2026-10-31, uzupełniona po wdrożeniu trzech kolejek klawiatury,
czytelności i małej lupy. To lista kandydatów do kolejnych zmian, a nie decyzja o migracji
wszystkich wykresów do Chart.js.

Przegląd obejmuje panele rzeczywiście podłączone do `.chapters` wykładów
00–04, ich kontrolki i skrypty obsługujące interakcję. Uwzględnia również
podłączone prototypy rozdziału 6 wykładu 03; nie obejmuje usuniętych wariantów.
Ocena wynika z kodu i celu demonstracji. Wdrożone kolejki sprawdzono w Chromium:
klawiaturę, lupy, ukrywanie odpowiedzi, S/M/L, telefon i powroty do rozdziału.
Czytelność na rzutniku oraz pozostałych kandydatów trzeba jeszcze potwierdzić
na zajęciach i w przeglądarce.

## Wniosek i kolejność

Najpierw uzupełnić działania, które bez kursora są niedostępne. Następnie
uporządkować skalowanie napisów i powiększanie istniejących wykresów.
Nie dodawać pól liczbowych do demonstracji pojęć. Zachować je w kalkulatorach,
w których konkretny wynik i dokładnie zadane parametry są częścią zadania.

W aktywnych panelach jest 18 widgetów Chart.js: 6 w wykładzie 01, 4 w 02
i 8 w 03. Wszystkie mają wspólną lupę i skalowanie napisów. Trzynaście ma
uchwyty klawiaturowe: Ryc. 4.3 wykładu 03 oraz dwanaście widgetów z pierwszej
i drugiej kolejki. W pozostałych pięciu zachowano istniejące przyciski i pola;
trzecia kolejka objęła ich czytelność.

Pierwsza kolejka — wdrożona:

1. 03 / Ryc. 4.4 — dystrybuanta.
2. 03 / Ryc. 4.5 — kwantyl.
3. 01 / Ryc. 3.9 — percentyle.
4. 01 / Ryc. 3.10 — zgadywanie średniej i mediany.
5. 02 / Ryc. 5.3 — wartość odstająca i korelacja.

Pierwsze trzy najłatwiej wykorzystają doświadczenia z uchwytem granicy.
Ostatnie dwa są szczególnie wartościowe dydaktycznie, ale wymagają innej
semantyki sterowania: niezależnych odpowiedzi albo ruchu w dwóch wymiarach.

## Pierwsza kolejka: uzasadnienie wdrożonych zmian

Tabela opisuje problemy rozpoznane w przeglądzie i przyjęte rozwiązania.
Wszystkie pięć widgetów ma już klawiaturę, wspólną małą lupę i skalowanie
napisów. Zgadywanie średniej/mediany nie wymusza kolejności typów; odpowiedzi
pozostają ukryte do odsłonięcia. Nie dodano pól liczbowych.

| Wykład / rycina | Co wymagało kursora | Przyjęta zmiana | Powiększenie i napisy |
|---|---|---|---|
| 03 / 4.4 — Pole na lewo od x to wartość dystrybuanty | Położenie x ustala ruch lub kliknięcie na jednym z dwóch wykresów. | Jeden uchwyt x dostępny Tabem, strzałki i większy krok z Shiftem. Oba wykresy reagują na ten sam stan. Zwykłe najechanie kursora nie powinno nadpisywać wyboru podczas obsługi klawiaturą. | Duża korzyść: powiększać wspólnie gęstość i dystrybuantę. Na wąskim ekranie zachować układ pionowy. |
| 03 / 4.5 — Kwantyl: odwrotne pytanie | W trybie „znam x” granicę można przesuwać tylko kursorem. Tryb „znam p” ma już przyciski. | Uchwyt klawiaturowy tylko w trybie x. Pozostawić przyciski p i „Odsłoń”; zmiana wartości nadal chowa odpowiedź. | Duża korzyść. Skalować osie oraz duże podpisy x i p rysowane bezpośrednio na canvas. |
| 01 / 3.9 — Percentyle wzrostu studentów | Płynny wybór percentyla wymaga przeciągania; przyciski udostępniają tylko wybrane wartości. | Jeden uchwyt sterujący rzędem percentyla p, np. strzałki o 1 punkt procentowy. Nie sterować arbitralnym krokiem wzrostu. | Duża korzyść: histogram i pudełko powiększane razem, z nadal wyrównanymi osiami. |
| 01 / 3.10 — Kliknij na wykres, aby umieścić średnią i medianę | Bez kliknięcia nie da się umieścić obu odpowiedzi, więc sama dostępność przycisku odsłonięcia nie wystarcza. | Dwa niezależne znaczniki odpowiedzi obsługiwane klawiaturą. Ich początkowe położenie nie może zdradzać wyniku. Nie narzucać kolejności średnia ≤ mediana. Zachować jawne odsłonięcie odpowiedzi. | Przydatne do odczytania kształtu i ustawienia typów. Nie pokazywać prawdziwych miar w podpowiedzi ani nazwie dostępności przed odsłonięciem. |
| 02 / 5.3 — Wpływ wartości odstającej na r | Przyciski dodają punkt, lecz jego swobodne przesuwanie wymaga myszy. | Tab wybiera tylko dopisane punkty odstające; ←/→ zmieniają x, ↑/↓ zmieniają y. Nie dodawać 50 podstawowych punktów do kolejności Tab. | Duża korzyść. Lupa zachowuje wszystkie punkty, bieżące r i ustawioną scenę; krok klawiatury zależy od zakresów osi. |

Źródła: [cdf_chart.js](03-rozklady-prawdopodobienstwa/modules/cdf_chart.js),
[quantile_chart.js](03-rozklady-prawdopodobienstwa/modules/quantile_chart.js),
[percentile_chart.js](01-typy-danych/modules/percentile_chart.js),
[guess_chart.js](01-typy-danych/modules/guess_chart.js),
[scatter_charts.js](02-zwiazki-zmiennych/modules/scatter_charts.js).

## Druga kolejka: wdrożona

Wszystkie siedem paneli ma klawiaturę, lupę i skalowanie napisów. Pensja
prezesa staje się dostępna Tabem po jej dodaniu, a po nowym losowaniu uchwyt
znika. Kurtoza zmienia się co 0.2 (z Shiftem co 1), rozrzut i nachylenie co
0.05 (z Shiftem co 0.5); klawiatura przerywa animację gotowego przypadku.
Dwie krzywe mają niezależne uchwyty 2D: ←/→ zmieniają μ, ↑ zmniejsza σ,
↓ zwiększa σ. Porównanie obserwacji i galeria używają kroków oraz jednostek
z konfiguracji. W widoku z uchwyty oryginalnych osi są ukryte. Lupa zachowuje
parametry i widok; długie tytuły osi zawijają się przy większej czcionce.
Dodawanie dowolnej pensji kliknięciem pozostaje osobnym działaniem myszą.

Testy: 22 scenariusze JS (w tym siedem nowych), pełne kontrole R oraz Chromium:
klawiatura, lupy bez zmiany stanu, ukrywanie uchwytów, ciemny motyw i telefon.
Tabela zachowuje uzasadnienie wyboru mechanizmów.

| Wykład / rycina | Ocena klawiatury | Ocena lupy i skalowania |
|---|---|---|
| 01 / 3.4 — Zarobki w firmie: średnia vs mediana | Uchwyt pensji prezesa ma duży sens: pokazuje ciągłą reakcję średniej i odporność mediany. Dodawanie pracowników kliknięciem jest osobnym działaniem; sam uchwyt nie zastąpi całej obsługi widgetu. | Wspólne powiększenie histogramu i paska obserwacji. Trzeba uwzględnić zmianę zakresu po dodaniu prezesa. |
| 01 / 5.5 — Porównaj rozkłady o różnej kurtozie | Przyciski dają już gotowe stany. ↑/↓ zmieniające kurtozę uzupełnią przeciąganie szczytu, bez nowego suwaka ani pola. | Duża korzyść dla panelu ogona. Powiększać krzywą główną i ogon razem; zachować skalę umożliwiającą porównanie z normalnym. |
| 02 / 4.3 — Siła korelacji | Przypadki „Słaba / Średnia / Silna” mają przyciski. Uchwyt ↑/↓ jest przydatnym rozszerzeniem do badania stanów pośrednich. | Przydatne do oglądania punktów i pasa. Nie zmieniać rozrzutu danych przy powiększeniu. |
| 02 / 4.4 — r nie zależy od nachylenia | Gotowe nachylenia są już dostępne przyciskami. Uchwyt ↑/↓ umożliwi płynną eksplorację, zachowując stałe r. | Przydatne; stałe zakresy osi pomagają zachować sens porównania. |
| 03 / 5.3 — Dwie krzywe normalne | Duża korzyść, lecz to sterowanie dwuwymiarowe: przesunięcie szczytu zmienia μ, jego wysokość zmienia σ. Dwa uchwyty, z jasno opisanym znaczeniem kierunków. Nie kopiować bezpośrednio obsługi granicy a/b. | Przydatne dla porównania nakładających się rozkładów; powiększenie nie może zmieniać parametrów. |
| 03 / 5.5 — Kto bardziej wyjątkowy? | Uchwyt każdej obserwacji na jej własnej osi. Krok brać z konfiguracji pary: cm, punkty i °C nie mogą mieć jednego wspólnego kroku. | Duża korzyść: dwa małe wykresy i wspólna skala z. Powiększać cały widget, zachowując widok i wartości. Ukryte uchwyty nie powinny zatrzymywać Tab. |
| 03 / 5.7 — Rozkład normalny w sześciu kierunkach | Przyciski zmieniają przykład, lecz próg jest przeciągany. Jeden uchwyt progu ma sens. Krok i jednostkę brać z przykładu, z zachowaniem jednostronnego lub dwustronnego pytania. | Przydatne przy długich nazwach osi i małych prawdopodobieństwach. Pytanie i odczyty pozostają razem z wykresem. |

Źródła: [salary_chart.js](01-typy-danych/modules/salary_chart.js),
[kurtosis_chart.js](01-typy-danych/modules/kurtosis_chart.js),
[normals_chart.js](03-rozklady-prawdopodobienstwa/modules/normals_chart.js),
[duel_chart.js](03-rozklady-prawdopodobienstwa/modules/duel_chart.js),
[cases_chart.js](03-rozklady-prawdopodobienstwa/modules/cases_chart.js).

## Trzecia kolejka: czytelność — wdrożona

Wspólna lupa i S/M/L zmieniają wygląd bez wymiany instancji wykresu,
ponownego losowania danych ani odsłaniania odpowiedzi. Pięć dodatkowych testów
JS chroni te zachowania, w tym trwającą animację lejka i niepoprawne σ.

| Wykład / rycina | Zrealizowane rozwiązanie |
|---|---|
| 01 / 2.3 — Wykres kołowy a słupkowy | Wspólna lupa całego panelu z legendą. W powiększeniu wykresy są pod sobą, etykiety słupków rosną. Zmiana wyglądu zachowuje scenariusz i stan odsłonięcia; celowo zbliżone kolory pozostają częścią zadania. |
| 01 / 3.6 — Odporność miar | Histogram i pasek powiększane razem. Osie skalują się z S/M/L, bez zmiany pensji ani trzech miar. Dotychczasowe przyciski dodania wartości skrajnych pozostają podstawą demonstracji. |
| 02 / 5.4 — Paradoks Simpsona | Lupa i skalowane napisy zachowują widok „Razem / Osobno”, punkty i proste. Nie dodano uchwytów do punktów. |
| 03 / 1.2 — Lejek | Lupa i skalowane osie zachowują serie i ich liczbę. Skala pozostaje logarytmiczna; długi tytuł osi zawija się. Zmiana wyglądu nie kończy trwającej animacji. |
| 03 / 5.4 — Kalkulator z-score | Powiększenie obu wykresów i skalowanie osi bez zmiany μ, σ, x i wyniku. Widoczne etykiety są powiązane z polami przez label/for; pola pozostają właściwą obsługą dokładnych parametrów. |

## Wykłady 00 i 04 oraz pozostałe elementy 01–03

### 00 — Dane i populacja

Sceny 1.1, 2.1, 3.1, 3.2 i 4.1 są sterowane przyciskami i krokami;
nie mają analogicznego problemu jak przeciągana granica pola. Nie dodawać
uchwytów do pojedynczych osób, filiżanek czy punktów populacji.

Najlepszymi kandydatami do sprawdzenia czytelności są:

- 3.1 — grupka z egzaminu i rozkład p̂: drobne podziałki i wyniki wielu prób;
- 3.2 — telefon i biblioteka: porównanie dwóch sposobów zbierania próby;
- 4.1 — herbata z mlekiem: podpisy histogramu trafień;
- 1.1 — student i tabela: tekst w tabeli, jeśli okaże się za drobny na rzutniku.

To SVG, które skaluje się przez `viewBox`, a nie Chart.js. W pierwszej kolejności
sprawdzić większy kontener lub powiększenie całej sceny. `styleAxes()` i
`axisFontSize()` nie obsłużą jej automatycznie. Ryc. 1.2, 5.1 i quizy nie
potrzebują lupy wykresu; warto zachować naturalne skalowanie tekstu.

Źródła: [scenes.js](00-dane-i-populacja/modules/scenes.js),
[scenes.css](00-dane-i-populacja/modules/scenes.css),
[helpers.R](00-dane-i-populacja/modules/helpers.R).

### 01 — Pozostałe demonstracje i tabele

Wykresy R mają już przycisk powiększenia przez `lc_plot()` / `zoom_plot_ui()`.
Nie dodawać im obok drugiej lupy Chart.js. Warto sprawdzić i ewentualnie
ujednolicić istniejący symbol oraz rozmiar napisów w powiększeniu, szczególnie:

- 3.1 — budowa histogramu: stosy i porównania kilku histogramów;
- 4.2 i 4.5 — obliczanie SD i budowa pudełka: drobne etykiety kolejnych warstw;
- 4.8 — porównanie SD i CV: czytelność obu obrazów;
- 5.1 i 5.4 — skośność oraz kwadrat/czwarta potęga: podpisy wkładów obserwacji;
- 5.7 — pełna charakterystyka: histogram i cienki pasek pudełka powinny
  pozostać możliwe do porównania po powiększeniu.

Nie dodawać uchwytów do wzorów, mapy narzędzi, tabel i quizów. Taksonomia
typów danych (1.2) ma osobny problem: liście z `onclick` wymagają kontroli
obsługi klawiaturą. To zadanie dla semantyki przycisków, a nie lupy lub
uchwytów na wykresie.

### 02 — Sceny, obrazy i tabela krzyżowa

Sceny 2.1, 3.1, 4.1 mają nawigację krokową. Nowe uchwyty nie są potrzebne.
W tabeli krzyżowej 2.2 oraz pudełkach 3.2 sprawdzić istniejące kontrolki
i powiększenie R. W razie kłopotów z tekstem tabeli użyć jej mechanizmów
responsywności, zamiast powiększać ją jak bitmapę.

Kierunek korelacji 4.2, kwartet Anscombe’a 5.1 i nieliniowość 5.2 są obrazami.
Mają już obsługę powiększenia; wspólny skrypt obsługuje też Enter, spację
i Escape. Nie przenosić ich do Chart.js tylko po to, żeby dodać lupę.

### 03 — Pozostałe rozkłady i CTG

Eksploratory rozkładów dyskretnych i ciągłych oraz 5.2 i 5.6 mają już kontrolki
i wykresy R. Najpierw sprawdzić klawiaturę istniejących suwaków i obecne
powiększenie. Decyzję o Chart.js podejmować osobno, gdy potrzeba płynnej
interakcji rzeczywiście uzasadnia migrację.

Na rzutniku szczególnie warto obejrzeć 3.12 (cztery rozkłady), 4.10 (paradoks
autobusu), 6.2–6.4 (porównania rozkładu populacji i średnich) oraz podłączone
prototypy: Galton, grupka przyjaciół, średnia Cauchy’ego i galeria średnich.
Mają już wykresy R z powiększeniem. Podłączone „Drzewo rodu rozkładów” jest
osobnym kandydatem do większego widoku SVG, po rozstrzygnięciu jego treści.

### 04 — Przedziały ufności

Największa potencjalna korzyść dotyczy czytelności, nie nowych uchwytów.
Sceny „Zmierz grupkę” i „Zarzuć siatkę” mają przyciski i SVG, jak w wykładzie 00.

Priorytety sprawdzenia istniejącego powiększenia R:

- 2.1 — odwrócenie μ ↔ x̄: relacja dwóch perspektyw;
- 3.2–3.3 i 4.1–4.2 — konstruowanie przedziału: widoczność SE, końców CI i zera;
- przykłady porównawcze 3 / A–C oraz dynamiczne przypadki A1–C2 i A1–C1:
  wspólny widok grup, przedziału różnicy i hipotezy;
- 5.1 — wpływ n, poziomu ufności i s na szerokość;
- 5.3 — trzy poziomy ufności na wspólnej osi.

Nie dodawać swobodnego przesuwania końców CI w demonstracji jego konstrukcji:
końce wynikają z danych i metody. Przesuwanie ich niezależnie może sugerować,
że wybieramy je uznaniowo. Zmiana n lub poziomu ufności przez istniejącą
kontrolkę lepiej zachowuje sens dydaktyczny.

W kalkulatorze wielkości próby 5.2 zachować pola liczbowe: zadany margines
błędu i spodziewane s są istotą obliczenia. Nie dodawać tam uchwytów zamiast
parametrów. Dynamiczne przypadki też korzystają z widgetów krokowych
i `zoom_plot_server()`, mimo że ich wykresy nie są widoczne w samym statycznym UI.

## Zasady wspólne dla wdrożenia

1. Jedna lupa na logiczny widget. Jeśli dwa obrazy coś porównują, powiększać
   je wspólnie, zachowując stan, odczyty i odsłonięcie odpowiedzi.
2. Skalować nie tylko osie, ale też podpisy rysowane wtyczkami canvas.
   W `chart_utils.js` i `percentile_chart.js` są podpisy o stałym rozmiarze,
   a w `quantile_chart.js` osobne rozmiary x i p. Samo `styleAxes()` ich nie zmieni.
3. Przy większym kroju przeliczyć marginesy i szerokość osi, żeby nie zepsuć
   wyrównania histogramu z paskiem obserwacji lub pudełkiem.
4. Używać wspólnego cyklu życia `lcCharts`, stylu focus i symbolu lupy.
   Reguły ruchu, kroki, jednostki i ograniczenia pozostawić właściwe dla zadania.
5. Nie kopiować ograniczenia a ≤ b do niezależnych typów średniej i mediany
   ani jednego kroku 0.5 cm do wszystkich widgetów.
6. W widgetach krokowych nie dopuścić, by strzałka poruszająca uchwyt
   jednocześnie przełączała krok. Obecny `lc_widgets.js` obsługuje strzałki
   na poziomie `.lc-stepper`; nowe sterowanie wymaga uwzględnienia tego konfliktu.
7. Nie dopisywać instrukcji obsługi do każdej narracji. Zwięzły istniejący
   podpis, focus i podpowiedź dostępności zwykle wystarczą.

Powiększenie R i obrazów jest w [shared.R](R/shared.R) oraz
[shared_toc.js](R/shared_toc.js). Wzorzec wdrożony w 03 / 4.3 jest w
[area_chart.js](03-rozklady-prawdopodobienstwa/modules/area_chart.js);
wspólne mechanizmy w [lc_charts.js](R/lc_charts.js).
