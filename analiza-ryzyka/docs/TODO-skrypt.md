# Archiwum TODO — decyzje po przeróbce wykładów na skrypt

Aktualny, nadrzędny backlog projektu znajduje się w [`../../TODO.md`](../../TODO.md).
Ten plik zachowuje pełny kontekst historyczny decyzji; nowych zadań nie należy
już dopisywać tutaj.

Otwarte pytania po commicie `fb4737d` (2026-09-28). Każdy punkt wymaga decyzji prowadzącego.

## Kolejki pracy

Nadrzędna kolejność prac, także nad responsywnością i wspólnymi komponentami,
jest w [głównym TODO](../../TODO.md). Poniższa lista zachowuje pierwotny,
szczegółowy kontekst decyzji kursowych.

### A. Eksperymenty wymagające wyboru przed dalszym rozwojem

- [ ] **Wykład 01, ćwiczenie 2:** porównać dotychczasowy widget i prototypy
  A/B/C, wybrać jeden wariant, a następnie usunąć pozostały kod serwera i CSS
  `.lc-proto-*`. Nie migrować prototypów do wspólnych komponentów przed
  wyborem.
- [ ] **Wykład 01, łańcuch pojęć:** ocenić interaktywny schemat jako element
  treści tego wykładu. Ewentualne uogólnienie traktować jako osobne zadanie.
- [ ] **Wykład 07:** rozdzielić ocenę lokalnych ról tekstu `.life-*` od testu
  responsywności paneli. Lokalne klasy pozostają pilotażem do czasu decyzji,
  czy wzorzec ma być używany w innych wykładach.

### B. Decyzje merytoryczne niezależne od layoutu

Punkty 1–5 poniżej rozpatrywać osobnymi zmianami dla wykładów 04, 07, 08, 09
i 10. Nie łączyć ich z migracją responsywności. W punkcie 2 decyzję o wzorze
λ̂ warto podjąć przed ostatecznym zatwierdzeniem treści wykładu 07.

## 1. [04] Które założenie łamie kontroler „po wykryciu sprawdzam dokładniej”?

- **Gdzie:** `04-wiele-prob/modules/block.R`, pytanie kontrolne `p4_chk_zalozenia` (ok. l. 248).
- **Stan:** jako poprawną odpowiedź przyjęto „stałość p”. Wyjaśnienie wspomina też, że wynik zależy od historii serii. Dotychczasowy tekst opisuje ten przypadek jako „zmianę definicji próby”.
- **Decyzja:**
  - [ ] zostaje „stałość p”
  - [ ] „niezależność”
  - [ ] „definicja próby” — wtedy przeredagować pytanie i wyjaśnienie

## 2. [07] Szacunek λ̂ = d/Σtᵢ jako wzór numerowany

- **Gdzie:** `07-czas-zycia/modules/block.R`, wzór (7.2) (ok. l. 162) i przykład 7.2.
- **Stan:** lead ostatniego rozdziału zapowiada kurs „bez estymacji parametrów”, a wzór (7.2) jest widocznym, numerowanym szacunkiem przy modelu wykładniczym. Pokazuje, jak wykorzystać obserwacje cenzorowane. W przykładzie 7.2 szacunek (1812,5 h) różni się od średniej z symulacji (1368,75 h); tekst to wyjaśnia.
- **Decyzja:**
  - [ ] zostaje
  - [ ] przenieść do zwijanego `risk_derivation` („Skąd to się bierze”)
  - [ ] usunąć i poprawić numerację (7.3)–(7.17)

## 3. [10] Horyzont roczny

- **Gdzie:** `10-model-do-decyzji/modules/block.R`, sekcja „Horyzont roczny” (`id = "rok"`, ok. l. 393), wzór (10.9) i ćwiczenie „Horyzont roczny”.
- **Stan:** case liczy jedną misję, zgodnie z commitem 81f8e67 i pytaniem 1 quizu. Ryzyko roczne to P_rok = 1 − (1 − P(TOP))³ ≈ 0,005. Wartość 1 − R_sys³ ≈ 0,641 jest pokazana jako pułapka: to niezawodność sprzętu bez warunku I.
- **Decyzja:**
  - [ ] zostaje jak jest
  - [ ] horyzont roczny ma być wynikiem głównym — wymaga zmiany serwera i `risk_mission_analysis`

## 4. [09] Widget rankingu potencjalnej redukcji

- **Gdzie:** `09-drzewo-bledow/modules/block.R`, serwer `f9_rank_plot` (ok. l. 607).
- **Stan:** ranking liczy na stałych wartościach bazowych (0,005; 0,05; 0,08), a nie na suwakach z rozdziału 3. Ze wzoru (9.10) wynika, że suwak redukcji zmienia tylko skalę słupków, a nie ich kolejność. Tekst mówi o tym wprost. Dydaktycznie widget niewiele daje.
- **Decyzja:**
  - [ ] zostaje
  - [ ] podpiąć suwaki z rozdziału 3, żeby kolejność mogła się zmieniać
  - [ ] pokazywać redukcję względną albo istotność krytyczną obok Birnbauma

## 5. [08] Parametry spoza danych Bananpolu

- **Gdzie:** `08-niezawodnosc-systemu/modules/block.R`:
  - widget czasu w serwerze (`exp(-t / 1800)` itd., ok. l. 679), przykład 8.6;
  - przykłady 8.5, 8.7, 8.11 i pytanie o poprawę (ok. l. 370, 562).
- **Stan:**
  - Widget czasu używa MTTF 1800 / 2000 / 2500 h, które nie wynikają z danych w ramce Bananpolu. Przykład 8.6 przedstawia je jako osobny zestaw parametrów.
  - Sterownik C w przykładach ma R = 0,98 — tyle samo, co zasilanie w danych Bananpolu, co może mylić.
  - Próg 14,5 °C w definicji sukcesu jest fikcyjny.
- **Decyzja:**
  - [ ] zostaje
  - [ ] ujednolicić MTTF w widgecie z danymi Bananpolu
  - [ ] zmienić R_C na wartość niekolidującą (np. 0,97) i przeliczyć przykłady 8.5, 8.7, 8.11

## Responsywność paneli, tabel i widgetów

Wspólna koncepcja i lista kolejnych kroków dla obu kursów jest w
[głównym TODO](../../TODO.md#responsywne-panele-tabele-i-widgety).
Pilotaż analizy ryzyka obejmuje wykład 07, rozdziały 2 i 4. Dalsza migracja
czeka na dopracowanie tabeli krokowej i weryfikację pośrednich szerokości.
