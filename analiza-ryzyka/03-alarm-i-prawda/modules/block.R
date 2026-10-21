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
  list(question = "Na 10 000 zmian: 100 awarii, czułość 0.95, FPR 0.05. Ile alarmów jest prawdziwych?",
    choices = c("95 z 100" = "a", "9500 z 10 000" = "b", "95 z 590" = "c"), correct = "c",
    explanation = "Jest 95 prawdziwych alarmów i 495 fałszywych, więc posterior wynosi 95/590≈0.161."),
  list(question = "Przy stałej czułości i FPR maleje częstość awarii. Co dzieje się z P(awaria | alarm)?",
    choices = c("Zawsze rośnie" = "a", "Maleje, jeśli FPR>0" = "b", "Nie zmienia się" = "c"), correct = "b",
    explanation = "Maleje udział prawdziwych alarmów wśród wszystkich alarmów."),
  list(question = "Kiedy wolno ponownie użyć Bayesa z tymi samymi parametrami drugiego czujnika?",
    choices = c("Gdy wyniki są niezależne warunkowo przy awarii i przy jej braku" = "a", "Gdy czujniki mają różne numery seryjne" = "b", "Gdy oba alarmy wystąpiły jednocześnie" = "c"), correct = "a",
    explanation = "Potrzebna jest niezależność warunkowa w obu stanach, a nie tylko niezależność bezwarunkowa."),
  list(question = "Reakcja kosztuje 100 zł i zapobiega stracie 2000 zł. Kiedy minimalizuje oczekiwany koszt?",
    choices = c("Dopiero powyżej 0.5" = "a", "Przy każdym dodatnim posteriorze" = "b", "Gdy posterior przekracza 0.05" = "c"), correct = "c",
    explanation = "Porównujemy 100 z 2000q; przy q=0.05 koszty są równe.")
))
alarm_exercises <- list(
  list(
    task = "Bananpol: dla 10 000 zmian, częstości awarii 0.01, czułości 0.95 i FPR 0.05 policz, ile alarmów będzie prawdziwych.",
    answer = c(
      "Awarie: 0.01 · 10 000 = 100 zmian; zmiany bez awarii: 9900.",
      "Prawdziwe alarmy: 0.95 · 100 = 95. Fałszywe alarmy: 0.05 · 9900 = 495. Wszystkich alarmów jest 95 + 495 = 590.",
      "Prawdziwych jest 95 z 590 alarmów, czyli P(awaria | alarm) = 95/590 ≈ 0.161 — zgodnie ze wzorem (3.4). Pięć na sześć alarmów jest fałszywych."
    )
  ),
  list(
    task = "Diagnostyka: wyjaśnij, dlaczego dwóch czujników z tym samym zasilaniem nie wolno automatycznie traktować jako niezależnych.",
    answer = c(
      "Wspólne zasilanie jest wspólną przyczyną wyników obu czujników. Spadek napięcia może wywołać fałszywy alarm na obu naraz, a utrata zasilania wycisza oba jednocześnie — także wtedy, gdy awaria trwa. Przy ustalonym stanie instalacji P(oba alarmy | stan) jest wtedy większe niż iloczyn P(alarm | stan) · P(alarm | stan), więc warunek z definicji 3.7 nie zachodzi.",
      "Skutek rachunkowy: wzór (3.8) mnoży iloraz wiarygodności przez siebie i zawyża posterior. W skrajnym przypadku, gdy drugi czujnik tylko powtarza pierwszy, drugi alarm nie wnosi żadnej informacji i posterior zostaje na poziomie 0.161 zamiast 0.785."
    )
  ),
  list(
    task = "Transfer: zaproponuj naturalne częstości dla testu przesiewowego w medycynie i nazwij właściwy mianownik.",
    answer = c(
      "Przykład: 1000 badanych, choroba u 1% (10 osób), czułość 0.90, FPR 0.09. Wynik dodatni: 9 chorych i około 89 zdrowych, razem około 98 osób.",
      "Pacjent z wynikiem dodatnim pyta o P(choroba | wynik dodatni), więc mianownikiem są wszystkie osoby z wynikiem dodatnim (około 98), a nie wszyscy chorzy (10) ani wszyscy badani (1000). Odpowiedź: 9/98 ≈ 0.09."
    )
  ),
  list(
    task = "Druga informacja o innej jakości: po alarmie czujnika (czułość 0.95, FPR 0.05, częstość awarii 0.01) dyżurny prosi operatora o odczyt ręczny termometru. Odczyt wskazuje przegrzanie w 80% zmian z awarią i w 10% zmian bez awarii. Przyjmij warunkową niezależność obu źródeł i oblicz posterior po alarmie i dodatnim odczycie ręcznym.",
    answer = c(
      "Szanse a priori: 0.01/0.99 = 1/99. Iloraz wiarygodności czujnika: 0.95/0.05 = 19; odczytu ręcznego: 0.80/0.10 = 8.",
      "Ze wzoru (3.8): szanse a posteriori = (1/99) · 19 · 8 = 152/99 ≈ 1.535.",
      "Prawdopodobieństwo: 1.535/(1 + 1.535) ≈ 0.606. Słabsze źródło (LR = 8) podnosi posterior z 0.161 do około 0.61 — mniej niż drugi taki sam czujnik (0.785), ale wciąż wyraźnie."
    )
  ),
  list(
    task = "Sprzeczne sygnały: pierwszy czujnik alarmuje, a drugi, identyczny i warunkowo niezależny, milczy. Jakie jest P(awaria) po obu wynikach? Zinterpretuj.",
    answer = c(
      "Iloraz wiarygodności braku alarmu: P(brak alarmu | awaria) / P(brak alarmu | brak awarii) = 0.05/0.95 = 1/19.",
      "Szanse a posteriori: (1/99) · 19 · (1/19) = 1/99, czyli prawdopodobieństwo 0.01 — dokładnie częstość bazowa.",
      "Przy identycznych czujnikach alarm i milczenie znoszą się: wracamy do stanu wiedzy sprzed obu odczytów. To nie znaczy, że awarii na pewno nie ma; znaczy, że sytuacja wymaga trzeciej, niezależnej informacji."
    )
  ),
  list(
    task = "Odwrócenie warunku: w Bananpolu 20% zmian przebiega w przeciążeniu, P(incydent | przeciążenie) = 0.15 i P(incydent | normalna praca) = 0.01. Doszło do incydentu. Jakie jest prawdopodobieństwo, że zmiana była przeciążona? Porównaj wynik z udziałem przeciążenia wśród wszystkich zmian.",
    answer = c(
      "Mianownik ze wzoru na prawdopodobieństwo całkowite: P(incydent) = 0.20 · 0.15 + 0.80 · 0.01 = 0.038. Ze wzoru Bayesa (3.6): P(przeciążenie | incydent) = 0.030 / 0.038 ≈ 0.789.",
      "Przeciążenie to tylko 20% zmian, ale około 79% incydentów. Informacja o incydencie prawie czterokrotnie podnosi ocenę, że zmiana była przeciążona."
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
      value = c("0.01", "0.95", "0.05", "wynik tego wykładu")
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

# Widget „Wzory na tablicy 2×2”: kolejne wzory rozdziału z zaznaczonymi komórkami
# licznika (niebieskie) i mianownika (żółte). Działa w przeglądarce (modules/wzory.js).
alarm_wzory_cells <- list(
  list(key = "tp", abbr = "TP", n = 95L, desc = "awaria i alarm"),
  list(key = "fn", abbr = "FN", n = 5L, desc = "awaria bez alarmu"),
  list(key = "fp", abbr = "FP", n = 495L, desc = "alarm bez awarii"),
  list(key = "tn", abbr = "TN", n = 9405L, desc = "spokój i cisza")
)
alarm_wzory_steps <- list(
  list(name = "Częstość bazowa", symbol = "P(A)", num = list("tp", "fn"), den = list("tp", "fn", "fp", "tn"),
    value = "0.01", note = "Wiersz awarii wśród wszystkich zmian. Opisuje halę, nie czujnik."),
  list(name = "Czułość", symbol = "P(🚨 | A)", num = list("tp"), den = list("tp", "fn"),
    value = "0.95", note = "Liczona w wierszu awarii: ile awarii czujnik złapał."),
  list(name = "Swoistość", symbol = "P(🔕 | Aᶜ)", num = list("tn"), den = list("fp", "tn"),
    value = "0.95", note = "Liczona w wierszu bez awarii: ile spokojnych zmian przeszło w ciszy."),
  list(name = "Odsetek fałszywych alarmów", symbol = "P(🚨 | Aᶜ)", num = list("fp"), den = list("fp", "tn"),
    value = "0.05", note = "Ten sam wiersz co swoistość, drugi licznik: FPR = 1 - swoistość."),
  list(name = "Zapowiedź: wiarygodność alarmu", symbol = "P(A | 🚨)", num = list("tp"), den = list("tp", "fp"),
    value = "0.16", note = "Tym razem kolumna, nie wiersz. Tej liczby potrzebuje dyżurny; wyprowadzimy ją w rozdziale „Wzór Bayesa”.")
)
# Panel z tablicą i krokami; ten sam widget podsumowuje rozdział „Czułość
# i swoistość” i pokazuje wartości predykcyjne w rozdziale „Wzór Bayesa”.
alarm_wzory_panel <- function(steps, title, caption) {
  cell <- function(c) tags$div(class = "lc-wz-cell", `data-wz-cell` = c$key,
    tags$div(class = "lc-wz-abbr", c$abbr),
    tags$div(class = "lc-wz-n", format(c$n, big.mark = " ")),
    tags$div(class = "lc-wz-desc", c$desc))
  cl <- alarm_wzory_cells
  figure_panel(
    label = "Pojęcie",
    title = title,
    full_width = TRUE,
    tags$div(class = "lc-wz",
      `data-config` = jsonlite::toJSON(list(cells = cl, steps = steps), auto_unbox = TRUE),
      tags$div(class = "lc-sp-head",
        tags$span(class = "lc-sp-name", `data-wz-out` = "name", steps[[1]]$name),
        tags$span(class = "lc-sp-step", `data-wz-out` = "step", sprintf("Wzór 1 z %d", length(steps)))
      ),
      tags$div(class = "lc-wz-body",
        tags$div(class = "lc-wz-grid",
          tags$div(), tags$div(class = "lc-wz-head", "Alarm 🚨"), tags$div(class = "lc-wz-head", "Brak alarmu 🔕"),
          tags$div(class = "lc-wz-side", "Awaria A"), cell(cl[[1]]), cell(cl[[2]]),
          tags$div(class = "lc-wz-side", "Brak awarii Aᶜ"), cell(cl[[3]]), cell(cl[[4]])
        ),
        tags$div(class = "lc-wz-formula", `aria-live` = "polite")
      ),
      tags$div(class = "lc-sp-nav",
        tags$button(type = "button", class = "lc-action is-outline", `data-wz` = "prev", lc_icon("prev"), tags$span("Wstecz")),
        tags$button(type = "button", class = "lc-action is-solid", `data-wz` = "next", tags$span("Dalej"), lc_icon("next"))
      )
    ),
    lc_caption(caption)
  )
}
alarm_wzory_widget <- alarm_wzory_panel(alarm_wzory_steps,
  "Wzory na tablicy 2×2: licznik i mianownik",
  "Niebieskie: licznik. Żółte: mianownik. Liczby to oczekiwane zmiany na 10 000 w hali Bananpolu.")

# Wartości predykcyjne obok czułości i swoistości: ten sam licznik, inny mianownik.
alarm_ppv_steps <- list(
  list(name = "Czułość: wiersz awarii", symbol = "P(🚨 | A)", num = list("tp"), den = list("tp", "fn"),
    value = "0.95", note = "Przypomnienie z rozdziału 2: liczymy wśród awarii."),
  list(name = "PPV: kolumna alarmów", symbol = "P(A | 🚨)", num = list("tp"), den = list("tp", "fp"),
    value = "0.161", note = "Ten sam licznik TP, ale mianownik to wszystkie alarmy. Tego potrzebuje dyżurny po telefonie."),
  list(name = "Swoistość: wiersz bez awarii", symbol = "P(🔕 | Aᶜ)", num = list("tn"), den = list("fp", "tn"),
    value = "0.95", note = "Przypomnienie z rozdziału 2: liczymy wśród spokojnych zmian."),
  list(name = "NPV: kolumna ciszy", symbol = "P(Aᶜ | 🔕)", num = list("tn"), den = list("fn", "tn"),
    value = "0.9995", note = "Ten sam licznik TN, mianownik to wszystkie ciche zmiany. Cisza jest bardzo wiarygodna, alarm znacznie mniej.")
)
alarm_ppv_widget <- alarm_wzory_panel(alarm_ppv_steps,
  "Wartości predykcyjne na tablicy 2×2: wiersze i kolumny",
  "Niebieskie: licznik. Żółte: mianownik. Czułość i swoistość czytamy wierszami, PPV i NPV kolumnami.")

# Scena „Telefon o trzeciej”: panel czujnika w dojrzewalni i karty z założeniami
# liczbowymi, bliźniacza z „Serpuchów, 1983”. Rysunek i kadry działają
# w przeglądarce (modules/telefon.js).
alarm_telefon_cards <- list(
  list(key = "base", tone = "base", num = "1", unit = "na 100", text = "zmian kończy się awarią", term = "częstość bazowa"),
  list(key = "sens", tone = "sens", num = "95", unit = "na 100", text = "awarii czujnik wykrywa", term = "czułość"),
  list(key = "fpr", tone = "fpr", num = "5", unit = "na 100", text = "spokojnych zmian alarmuje", term = "odsetek fałszywych alarmów"),
  list(key = "ask", tone = "ask", num = "?", unit = "na 100", text = "alarmów jest prawdziwych", term = "o to pyta dyżurny")
)
alarm_telefon_frames <- list(
  list(name = "Telefon", cards = list(),
    text = "Trzecia w nocy. Panel dojrzewalni 3 pokazuje przegrzanie i dzwoni do dyżurnego. Wysyłać ekipę?"),
  list(name = "Hala", cards = list("base"),
    text = "Dyżurny zna swoją halę: awaria zdarza się średnio na jednej zmianie na sto."),
  list(name = "Czujnik przy awarii", cards = list("base", "sens"),
    text = "Zna też czujnik. Producent podaje, że wykrywa 95 awarii na 100."),
  list(name = "Czujnik bez awarii", cards = list("base", "sens", "fpr"),
    text = "Czujnik myli się także w drugą stronę: alarmuje na 5 spokojnych zmianach na 100."),
  list(name = "Pytanie", cards = list("base", "sens", "fpr", "ask"),
    text = "Dyżurny ma trzy liczby, ale potrzebuje czwartej: ile alarmów takich jak ten kończy się prawdziwą awarią?")
)
alarm_telefon_widget <- figure_panel(
  label = "Scena",
  title = "Telefon o trzeciej: trzy liczby i jedno pytanie",
  full_width = TRUE,
  tags$div(class = "lc-tf",
    `data-config` = jsonlite::toJSON(list(cards = alarm_telefon_cards, frames = alarm_telefon_frames), auto_unbox = TRUE),
    tags$div(class = "lc-sp-head",
      tags$span(class = "lc-sp-name", `data-tf-out` = "name", "Telefon"),
      tags$span(class = "lc-sp-step", `data-tf-out` = "step", "Kadr 1 z 5")
    ),
    tags$div(class = "lc-tf-stage", role = "img"),
    tags$p(class = "lc-sp-caption lc-tf-caption", `aria-live` = "polite"),
    tags$div(class = "lc-sp-nav",
      tags$button(type = "button", class = "lc-action is-outline", `data-tf` = "prev", lc_icon("prev"), tags$span("Wstecz")),
      tags$button(type = "button", class = "lc-action is-solid", `data-tf` = "next", tags$span("Dalej"), lc_icon("next"))
    )
  )
)

# Scena „Dziennik dyżurnego”: 2000 zmian z czterech lat, w nich awarie, alarmy
# przy awarii i fałszywe alarmy; w ostatnim kadrze telefony zjeżdżają w dwa stosy.
# Rysunek i kadry działają w przeglądarce (modules/dziennik.js).
alarm_dziennik_frames <- list(
  list(name = "Dziennik", step = 1L, read = "Zmian: 2000",
    text = "Dyżurny przegląda dziennik z czterech lat pracy dojrzewalni: 2000 zmian, każde pole to jedna zmiana."),
  list(name = "Awarie", step = 2L, read = "Awarie: 20",
    text = "Awaria zdarza się na jednej zmianie na sto. W czterech latach to 20 zmian."),
  list(name = "Alarm przy awarii", step = 3L, read = "Prawdziwe alarmy: 19 · przeoczona awaria: 1",
    text = "Czujnik wykrywa 95% awarii: alarmuje na 19 z tych 20 zmian. Jedna awaria przechodzi po cichu."),
  list(name = "Alarm bez awarii", step = 4L, read = "Fałszywe alarmy: 99",
    text = "Na 5% spokojnych zmian czujnik też alarmuje. Spokojnych zmian jest 1980, więc fałszywych telefonów jest 99."),
  list(name = "Telefony", step = 5L, read = "P(awaria | alarm) = 19/118 ≈ 0.16",
    text = "Dyżurny nie widzi awarii, widzi tylko telefony. Ze 118 telefonów prawdziwych było 19.")
)
alarm_dziennik_widget <- figure_panel(
  label = "Scena",
  title = "Dziennik dyżurnego: 2000 zmian, 118 telefonów",
  full_width = TRUE,
  tags$div(class = "lc-dz",
    `data-config` = jsonlite::toJSON(list(cols = 50L, rows = 40L, events = 20L, hits = 19L, false = 99L,
      frames = alarm_dziennik_frames), auto_unbox = TRUE),
    tags$div(class = "lc-sp-head",
      tags$span(class = "lc-sp-name", `data-dz-out` = "name", "Dziennik"),
      tags$span(class = "lc-sp-step", `data-dz-out` = "step", "Kadr 1 z 5")
    ),
    tags$div(class = "lc-dz-stage", role = "img"),
    tags$p(class = "lc-sp-caption lc-dz-caption", `aria-live` = "polite"),
    tags$p(class = "lc-dz-readout"),
    tags$div(class = "lc-sp-nav",
      tags$button(type = "button", class = "lc-action is-outline", `data-dz` = "prev", lc_icon("prev"), tags$span("Wstecz")),
      tags$button(type = "button", class = "lc-action is-solid", `data-dz` = "next", tags$span("Dalej"), lc_icon("next"))
    )
  )
)

# Scena „Serpuchów, 1983”: ekran systemu Oko i karty z tym, co wie dyżurny.
# Rysunek i kadry działają w przeglądarce (modules/serpuchow.js).
alarm_serpuchow_frames <- list(
  list(name = "Alarm", missiles = 1L, status = "START", tone = "alarm", cards = list(),
    text = "26 września 1983, tuż po północy, bunkier dowodzenia pod Moskwą. Satelitarny system ostrzegania Oko zgłasza start rakiety z bazy w USA."),
  list(name = "Pięć rakiet", missiles = 5L, status = "START", tone = "alarm", cards = list("proc"),
    text = "Po chwili system zgłasza kolejne starty, razem pięć, i ocenia wiarygodność jako wysoką. Procedura każe zameldować atak. Co robicie?"),
  list(name = "Co wie dyżurny", missiles = 5L, status = "START", tone = "alarm", cards = list("proc", "nowy", "setki"),
    text = "Dyżurny oficer Stanisław Pietrow wie, że system działa od niedawna. Wie też, że prawdziwy atak to setki rakiet naraz, nie pięć."),
  list(name = "Radary milczą", missiles = 5L, status = "START", tone = "alarm", cards = list("proc", "nowy", "setki", "radar"),
    text = "Radary naziemne patrzą inną metodą niż satelita. Mijają minuty, a żaden z nich nie potwierdza startu."),
  list(name = "Decyzja", missiles = 5L, status = "FAŁSZYWY ALARM", tone = "calm", reveal = TRUE,
    cards = list("proc", "nowy", "setki", "radar", "decyzja"), dim = list("proc"),
    text = "Pietrow melduje awarię systemu. Rakiety nie nadlatują. Później okazuje się, że satelita wziął odbicie słońca od wysokich chmur za starty.")
)
alarm_serpuchow_widget <- figure_panel(
  label = "Scena",
  title = "Serpuchów, 1983: pięć rakiet na ekranie",
  full_width = TRUE,
  tags$div(class = "lc-sp",
    `data-config` = jsonlite::toJSON(list(frames = alarm_serpuchow_frames), auto_unbox = TRUE),
    tags$div(class = "lc-sp-head",
      tags$span(class = "lc-sp-name", `data-sp-out` = "name", "Alarm"),
      tags$span(class = "lc-sp-step", `data-sp-out` = "step", "Kadr 1 z 5")
    ),
    tags$div(class = "lc-sp-stage", role = "img"),
    tags$p(class = "lc-sp-caption", `aria-live` = "polite"),
    tags$div(class = "lc-sp-nav",
      tags$button(type = "button", class = "lc-action is-outline", `data-sp` = "prev", lc_icon("prev"), tags$span("Wstecz")),
      tags$button(type = "button", class = "lc-action is-solid", `data-sp` = "next", tags$span("Dalej"), lc_icon("next"))
    )
  )
)

# Drzewo dwóch dróg do alarmu na 10 000 zmian. Grubość gałęzi kończących się
# alarmem jest proporcjonalna do liczby zmian; pozostałe gałęzie są przygaszone.
alarm_tree_svg <- function(d, prevalence, sensitivity, false_positive_rate) {
  tp <- d$alarm[1]; fp <- d$alarm[2]
  width <- function(n) 3 + 21 * n / max(1, tp, fp)
  num <- function(n) format(n, big.mark = " ", trim = TRUE)
  # data-k: klucz elementu, który podświetla krokowy widget (modules/drzewo.js).
  node <- function(k, x, y, label, n, color = NULL, dim = FALSE) sprintf(
    '<g class="a3-tree-node%s" data-k="%s"><rect x="%g" y="%g" width="150" height="36" rx="18"%s/><text x="%g" y="%g" text-anchor="middle"%s>%s · %s</text></g>',
    if (dim) " is-dim" else "", k, x - 75, y - 18,
    if (!is.null(color)) sprintf(' style="stroke:%s;stroke-width:2.5"', color) else "",
    x, y + 5, if (!is.null(color)) sprintf(' style="fill:%s"', color) else "", label, num(n))
  edge <- function(k, x1, y1, x2, y2, w = 2, color = NULL, dim = FALSE) sprintf(
    '<path class="a3-tree-edge%s" data-k="%s" d="M%g %g C%g %g %g %g %g %g" style="stroke-width:%.1f%s"/>',
    if (dim) " is-dim" else "", k, x1, y1, (x1 + x2) / 2, y1, (x1 + x2) / 2, y2, x2, y2, w,
    if (!is.null(color)) paste0(";stroke:", color) else "")
  prob <- function(k, x, y, p) sprintf('<text class="a3-tree-p" data-k="%s" x="%g" y="%g" text-anchor="middle">%s</text>', k, x, y, risk_fmt_p(p, 2L))
  HTML(paste0(
    sprintf('<svg class="a3-tree-svg" viewBox="0 0 760 300" role="img" data-prev="%s" data-sens="%s" data-fpr="%s" data-tp="%d" data-fn="%d" data-fp="%d" data-tn="%d" aria-label="Drzewo: ',
      prevalence, sensitivity, false_positive_rate, tp, d$no_alarm[1], fp, d$no_alarm[2]),
    num(sum(d$total)), ' zmian dzieli się na awarie i brak awarii, a każda grupa na alarm i brak alarmu. Prawdziwych alarmów jest ',
    num(tp), ', fałszywych ', num(fp), '.">',
    edge("e-a", 150, 150, 245, 70), edge("e-n", 150, 150, 245, 230),
    edge("e-hit", 395, 70, 485, 30, width(tp), upwr_accent), edge("e-miss", 395, 70, 485, 110, dim = TRUE),
    edge("e-fa", 395, 230, 485, 190, width(fp), upwr_single_alt), edge("e-tn", 395, 230, 485, 270, dim = TRUE),
    prob("p-a", 186, 96, prevalence), prob("p-n", 186, 218, 1 - prevalence),
    prob("p-hit", 440, 26, sensitivity), prob("p-miss", 440, 118, 1 - sensitivity),
    prob("p-fa", 440, 174, false_positive_rate), prob("p-tn", 440, 278, 1 - false_positive_rate),
    node("n-root", 75, 150, "Zmiany", sum(d$total)),
    node("n-a", 320, 70, "Awaria", d$total[1]), node("n-n", 320, 230, "Brak awarii", d$total[2]),
    node("n-hit", 560, 30, "Alarm", tp, upwr_accent), node("n-miss", 560, 110, "Bez alarmu", d$no_alarm[1], dim = TRUE),
    node("n-fa", 560, 190, "Alarm", fp, upwr_single_alt), node("n-tn", 560, 270, "Bez alarmu", d$no_alarm[2], dim = TRUE),
    '<g data-k="b-alarm"><path class="a3-tree-brace" d="M642 30 h14 v160 h-14 M656 110 h10"/>',
    sprintf('<text class="a3-tree-sum" x="672" y="106">%s</text><text class="a3-tree-sum" x="672" y="126">alarmów</text></g>', num(tp + fp)),
    sprintf('<g data-k="b-quiet"><path class="a3-tree-brace" d="M642 110 h14 v160 h-14 M656 190 h10"/><text class="a3-tree-sum" x="672" y="186">%s</text><text class="a3-tree-sum" x="672" y="206">cichych</text></g>',
      num(d$no_alarm[1] + d$no_alarm[2])),
    '</svg>'
  ))
}

# Drzewo z podświetlonymi obiema drogami do alarmu (dla przykładu do wzoru 3.3).
# quiet = TRUE: podświetlone drogi kończące się ciszą (wzór 3.5).
alarm_tree_lit <- function(prevalence, sensitivity, false_positive_rate, quiet = FALSE) {
  d <- risk_detector_counts(10000L, prevalence, sensitivity, false_positive_rate)
  svg <- as.character(alarm_tree_svg(d, prevalence, sensitivity, false_positive_rate))
  lit <- c("n-root", "e-a", "p-a", "n-a", "e-n", "p-n", "n-n",
    if (quiet) c("e-miss", "p-miss", "n-miss", "e-tn", "p-tn", "n-tn")
    else c("e-hit", "p-hit", "n-hit", "e-fa", "p-fa", "n-fa"))
  for (k in lit) svg <- sub(sprintf('class="([^"]*)" data-k="%s"', k),
    sprintf('class="\\1 is-lit" data-k="%s"', k), svg)
  brace <- if (quiet) "b-quiet" else "b-alarm"
  svg <- sub(sprintf('<g data-k="%s">', brace), sprintf('<g class="is-lit" data-k="%s">', brace), svg, fixed = TRUE)
  HTML(sub('class="a3-tree-svg"',
    paste0('class="a3-tree-svg is-stepped', if (quiet) " is-quiet", '"'), svg, fixed = TRUE))
}
a3_stop_total_ui <- a3_stop_total_panel(lapply(a3_total_raw, function(s)
  alarm_tree_lit(s$prevalence, s$sensitivity, s$fpr)))
a3_stop_ppv_ui <- a3_stop_bayes_panel("ppv", lapply(a3_total_raw, function(s)
  alarm_tree_lit(s$prevalence, s$sensitivity, s$fpr)))
a3_stop_npv_ui <- a3_stop_bayes_panel("npv", lapply(a3_total_raw, function(s)
  alarm_tree_lit(s$prevalence, s$sensitivity, s$fpr, quiet = TRUE)))

# Krokowe drzewo: wzór Bayesa składany z trzech narzędzi wykładu 02, a na koniec
# ta sama konstrukcja dla ciszy (NPV). Drzewo rysuje serwer (reaguje na suwaki
# tablicy 2×2), kroki i linijki wzoru dokłada modules/drzewo.js.
alarm_paths_widget <- figure_panel(
  label = "Pojęcie",
  title = "Wzór Bayesa na drzewie: dwie drogi do alarmu",
  full_width = TRUE,
  tags$div(class = "lc-tr",
    tags$div(class = "lc-sp-head",
      tags$span(class = "lc-sp-name", `data-tr-out` = "name", "Drzewo"),
      tags$span(class = "lc-sp-step", `data-tr-out` = "step", "Krok 1 z 6")
    ),
    lc_toolbar(lc_readouts(uiOutput("a3_counts"))),
    uiOutput("a3_tree"),
    tags$div(class = "lc-tr-lines", `aria-live` = "polite"),
    tags$div(class = "lc-sp-nav",
      tags$button(type = "button", class = "lc-action is-outline", `data-tr` = "prev", lc_icon("prev"), tags$span("Wstecz")),
      tags$button(type = "button", class = "lc-action is-solid", `data-tr` = "next", tags$span("Dalej"), lc_icon("next"))
    )
  ),
  lc_caption("Parametry są wspólne z tablicą 2×2. Liczby w drzewie to zmiany na 10 000; prawdopodobieństwa liczymy z parametrów.")
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
    withMathJax("$$P(A\\mid \\text{🚨})=\\frac{P(\\text{🚨}\\mid A)\\,P(A)}{P(\\text{🚨}\\mid A)\\,P(A)+P(\\text{🚨}\\mid A^{c})\\,P(A^{c})}$$"),
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
      intro = "Każdy system ostrzegawczy kończy się na człowieku, który dostaje sygnał i musi zdecydować, czy mu wierzyć. Zaczniemy od najsłynniejszego takiego człowieka, a potem wrócimy do chłodni Bananpolu.",
      sections = list(
        list(
          id = "serpuchow", title = "Noc w Serpuchowie",
          body = list(
            alarm_serpuchow_widget,
            c(
              "Pietrow nie miał wzoru ani tablicy. Kierował się trzema pytaniami, które w tym wykładzie dostaną nazwy: jak często taki atak w ogóle się zdarza, jak bardzo można ufać nowemu systemowi i co mówi drugie, niezależne źródło.",
              "Zanim pójdziesz dalej, odpowiedz sobie: dlaczego nie uwierzył? Do tej nocy wrócimy na końcu wykładu i policzymy to, co Pietrow wyczuł."
            )
          )
        ),
        list(
          id = "pytanie", title = "Alarm na zmianie",
          text = c(
        "Ta sama sytuacja, mniejsza stawka. Trzecia w nocy, telefon z chłodni Bananpolu: czujnik przegrzania znowu alarmuje. Wysyłać ekipę? Odpowiedź zależy od liczby, o którą mało kto pyta w środku nocy — od tego, jak często awaria zdarza się w ogóle.",
        "W poprzednim wykładzie liczyliśmy w przód: znając okoliczność zmiany, pytaliśmy o ryzyko skutku. Dyżurny jest w odwrotnej sytuacji — stanu urządzenia nie widzi, widzi tylko sygnał, i z niego musi wnioskować o tym, czego nie widać. To najtrudniejszy manewr tego kursu: odwrócenie kierunku warunku. Producent czujnika podaje P(alarm | awaria); dyżurny przy telefonie potrzebuje P(awaria | alarm). To nie są te same liczby — a różnica między nimi bywa dziesięciokrotna."
          ),
          callout = list(
            label = "Dane Bananpolu",
            text = "Detektor przegrzania w dojrzewalni: częstość awarii 0.01 na zmianę, czułość 0.95, fałszywe alarmy w 0.05 zmian bez awarii. Jednostka: zmiana pracy dojrzewalni; horyzont: 10 000 porównywalnych zmian. Liczby są fikcyjne.",
            color = "uwaga"
          ),
          body = list(
            alarm_telefon_widget,
            c(
              "Awaria zdarza się średnio na 1 zmianie na 100. Czujnik wykrywa 95% awarii, ale w 5% zmian bez awarii alarmuje fałszywie. Najpierw oszacuj wiarygodność alarmu bez rachunku — zapisz swoją liczbę, zanim klikniesz dalej.",
              "To pytanie ma długą historię błędnych odpowiedzi: w badaniach z udziałem lekarzy interpretujących wyniki testów przesiewowych większość podawała wartości bliskie „około 95%”, choć poprawna odpowiedź wynosiła kilka procent — liczbę opisującą test brano za wiarygodność wyniku dodatniego. Zaraz sprawdzisz, po której stronie tej statystyki jesteś."
            ),
            risk_try("wybierz jedną odpowiedź, zanim zaczniesz liczyć, i kliknij „Sprawdź intuicję”. Zapamiętaj swój wybór — wrócimy do niego w rozdziale „Wzór Bayesa”."),
            risk_vote_panel(
              "a3_vote", "a3_vote_feedback",
              "Po alarmie: jak duża jest szansa rzeczywistej awarii?",
              c("Około 95%" = "95", "Około 16%" = "16", "Nie da się określić bez częstości bazowej" = "base"),
              correct = "16"
            ),
            "Sprawdźmy zakład na dzienniku dyżurnego. Przejdź kadry i policz telefony, zanim zobaczysz jakikolwiek wzór.",
            alarm_dziennik_widget,
            c(
              "Odpowiedź „około 95%” jest kusząca, bo 95 to jedyna duża liczba w opisie czujnika i brzmi jak jego „skuteczność”. Ale 95% opisuje zachowanie czujnika w świecie, w którym awaria już nastąpiła. Dyżurny nie wie, w którym świecie jest — wie tylko, że zadzwonił telefon. Liczba, której potrzebuje, musi uwzględniać oba światy naraz: ten z awarią i ten, w którym awarii nie ma, a czujnik i tak alarmuje.",
              "Poprawny wynik — około 16% — zaskakuje niemal każdego, kto widzi go pierwszy raz. Scena pokazała go na 2000 zmian dziennika. W tym wykładzie wyprowadzimy go jeszcze trzema drogami: z tablicy 2×2, z naturalnych częstości i ze wzoru Bayesa. Wszystkie trzy dają tę samą liczbę, bo opisują ten sam rachunek w różnym zapisie."
            )
          )
        ),
        list(
          id = "kierunek", title = "Dwa kierunki jednego warunku",
          body = list(
            c(
              "W wykładzie 02 warunek był okolicznością, którą znamy — przegrzanie, lato, tryb pracy — a liczby przychodziły w kierunku drzewa: od okoliczności do skutku. Tam też pierwszy raz zobaczyliśmy, że odwrócenie kreski zmienia liczbę: P(incydent | przegrzanie) = 0.12, a P(przegrzanie | incydent) ≈ 0.71 (scena „Odprawa”). Zapis P(A | B) czytamy: prawdopodobieństwo A wśród przypadków, w których zaszło B. Warunek wskazuje mianownik. Jeśli zamienimy miejscami A i B, zmieni się mianownik — a wraz z nim cała liczba. P(alarm | awaria) liczymy wśród zmian z awarią; P(awaria | alarm) liczymy wśród zmian z alarmem. Obie liczby mają ten sam licznik: zmiany, na których wystąpiły jednocześnie awaria i alarm.",
              "Różnica jest łatwa do zobaczenia na przykładzie spoza przemysłu. Prawie każdy zawodowy koszykarz jest wysoki, ale wśród wysokich ludzi koszykarzy jest znikomy odsetek. P(wysoki | koszykarz) jest bliskie 1, a P(koszykarz | wysoki) bliskie 0. Nikt nie pomyli tych liczb w przypadku koszykarzy; przy czujnikach i testach mylimy je nagminnie, bo obie brzmią jak „skuteczność”."
            ),
            a3_stop_denominators_ui,
            "Oba panele pokazują te same 1000 zmian. Licznik jest wspólny: 19 zmian z awarią i alarmem. Zmienia się tylko obrys, czyli mianownik: wiersz 20 zmian z awarią albo kolumna 68 zmian z alarmem. Warunek wskazuje mianownik: 19/20 to nie to samo co 19/68.",
            risk_example("3.1", "Ten sam licznik, dwa mianowniki",
              problem = "W dzienniku starszej dojrzewalni Bananpolu z 1000 zmian zanotowano 20 awarii. Czujnik alarmował w 19 z nich oraz w 49 z 980 zmian bez awarii. Oblicz P(alarm | awaria) i P(awaria | alarm).",
              steps = c(
                "P(alarm | awaria): mianownikiem są zmiany z awarią (20), licznikiem zmiany z awarią i alarmem (19). Wynik: 19/20 = 0.95.",
                "P(awaria | alarm): mianownikiem są wszystkie zmiany z alarmem, czyli 19 + 49 = 68. Licznik jest ten sam (19). Wynik: 19/68 ≈ 0.279.",
                "Licznik się nie zmienił; zmienił się mianownik — z 20 na 68, bo do alarmów dochodzą 49 fałszywych alarmów ze zmian bez awarii."
              ),
              answer = "0.95 i około 0.28. Te same dane dają dwie bardzo różne liczby, zależnie od tego, który warunek stoi za kreską."
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
            risk_confusion_matrix(alarm = "Alarm 🚨", no_alarm = "Brak alarmu 🔕"),
            "Cztery wyniki układają się w tablicę o dwóch wierszach (stan instalacji) i dwóch kolumnach (wynik detektora). W analizie ryzyka i w diagnostyce medycznej przyjęła się ta sama konwencja zapisu: A oznacza zdarzenie, którego szukamy (tu: awarię), Aᶜ jego brak, 🚨 alarm detektora, a 🔕 brak alarmu. W literaturze zamiast 🚨 i 🔕 spotkasz znaki „+” (wynik dodatni) i „-” (wynik ujemny).",
            risk_definition("3.1", "Tablica wyników detektora", c(
              "Tablica wyników detektora to tablica 2×2, której wiersze odpowiadają stanowi rzeczywistemu (A albo Aᶜ), a kolumny wynikowi detektora (🚨 albo 🔕). Jej komórki to liczby lub prawdopodobieństwa wyników: prawdziwie dodatnich TP (A i 🚨), fałszywie ujemnych FN (A i 🔕), fałszywie dodatnich FP (Aᶜ i 🚨) oraz prawdziwie ujemnych TN (Aᶜ i 🔕).",
              "Suma wiersza A to liczba zdarzeń, suma wiersza Aᶜ — liczba przypadków bez zdarzenia, a suma kolumny 🚨 — liczba wszystkich alarmów."
            )),
            "Detektor opisujemy, dzieląc komórki przez sumy wierszy, bo producent testuje czujnik w warunkach, w których wie, czy awaria wystąpiła. Dwa takie ilorazy wystarczają, żeby opisać zachowanie detektora w obu stanach instalacji.",
            risk_definition("3.2", "Czułość i swoistość", c(
              "Czułość (ang. sensitivity) to P(🚨 | A): prawdopodobieństwo alarmu, gdy zdarzenie rzeczywiście zachodzi. Swoistość (ang. specificity) to P(🔕 | Aᶜ): prawdopodobieństwo braku alarmu, gdy zdarzenia nie ma.",
              "Odsetek fałszywych alarmów FPR (ang. false positive rate) to P(🚨 | Aᶜ) = 1 - swoistość. Obie wielkości opisują detektor, a nie populację, w której pracuje."
            )),
            risk_formula("\\begin{aligned}\\text{czułość} &= P(\\text{🚨}\\mid A)=\\frac{TP}{TP+FN},\\\\[0.6em] \\text{swoistość} &= P(\\text{🔕}\\mid A^{c})=\\frac{TN}{TN+FP}\\end{aligned}", num = "3.1",
              legend = c("TP" = "liczba prawdziwie dodatnich", "FN" = "liczba fałszywie ujemnych", "TN" = "liczba prawdziwie ujemnych", "FP" = "liczba fałszywie dodatnich")),
            risk_formula("\\text{FPR}=P(\\text{🚨}\\mid A^{c})=\\frac{FP}{TN+FP}=1-\\text{swoistość}", num = "3.2"),
            c(
              "Trzecia potrzebna liczba nie dotyczy detektora, lecz miejsca, w którym go zamontowano. Ten sam czujnik w hali z nowymi wentylatorami i w hali ze sprzętem po dwudziestu latach eksploatacji będzie miał identyczną czułość i swoistość, ale zupełnie inną liczbę awarii do wykrycia."
            ),
            risk_definition("3.3", "Częstość bazowa", c(
              "Częstość bazowa (prawdopodobieństwo a priori) to P(A): jak często zdarzenie występuje w danej populacji, zanim poznamy wynik detektora. W Bananpolu jest to udział zmian z awarią wśród wszystkich zmian danej hali.",
              "Ten sam fakt można zapisać na trzy sposoby: jako prawdopodobieństwo (0.01), jako naturalną częstość (1 zmiana na 100) albo jako szanse a priori (1 : 99, czyli 1 zmiana z awarią na 99 spokojnych). Częstość bazowa opisuje miejsce — halę, populację badanych — a nie detektor."
            )),
            "Wszystkie cztery liczby to ilorazy komórek tej samej tablicy. Różnią się tylko tym, które komórki trafiają do licznika, a które do mianownika.",
            alarm_wzory_widget
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
                  "Producent pisze w ulotce: „77 alarmów, z czego 57 prawdziwych — 74% trafności”. Czy dyżurny w hali z częstością awarii 0.01 może przyjąć, że alarm jest prawdziwy z prawdopodobieństwem 0.74?"
                )
              ),
              steps = c(
                "Ze wzoru (3.1): czułość = 57/60 = 0.95; swoistość = (400 - 20)/400 = 380/400 = 0.95. Ze wzoru (3.2): FPR = 20/400 = 0.05.",
                "Iloraz 57/77 ≈ 0.74 jest liczony w kolumnie „alarm”. Jego wartość zależy od tego, ile awarii było w teście: 60 na 460 zmian, czyli około 0.13. W hali Bananpolu awarie zdarzają się na 0.01 zmian — trzynaście razy rzadziej niż w teście. Fałszywych alarmów będzie więc proporcjonalnie znacznie więcej; w sekcji o tablicy 2×2 dla 10 000 zmian porównamy obie tablice obok siebie: wiarygodność alarmu wynosi w hali około 0.16."
              ),
              steps_type = "a",
              answer = "(a) Czułość 0.95, swoistość 0.95, FPR 0.05. (b) Nie. Liczba 0.74 opisuje kampanię testową, w której awarie wywołano sztucznie często; w hali trzeba ją przeliczyć z częstością bazową 0.01."
            ),
            risk_check("d3_chk_mianownik",
              "Serwis chce oszacować FPR czujnika z danych eksploatacyjnych. Który iloraz jest właściwy?",
              c(
                "fałszywe alarmy / wszystkie zmiany bez awarii" = "fpr",
                "fałszywe alarmy / wszystkie alarmy" = "fdr",
                "fałszywe alarmy / wszystkie zmiany" = "all"
              ),
              correct = "fpr",
              explanation = "FPR = P(🚨 | Aᶜ), więc mianownikiem są zmiany bez awarii — wiersz Aᶜ tablicy (wzór 3.2). Iloraz fałszywych alarmów do wszystkich alarmów to zupełnie inna wielkość, zależna od częstości bazowej.",
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
            "Znając czułość, FPR i częstość bazową, możemy odtworzyć oczekiwaną tablicę wyników dla dowolnej liczby zmian. Najpierw dzielimy zmiany na wiersze według częstości bazowej, potem każdy wiersz na kolumny według właściwego parametru detektora: wiersz A według czułości, wiersz Aᶜ według FPR.",
            risk_try("zacznij od ustawień domyślnych (częstość 0.01, czułość 0.95, FPR 0.05) i odczytaj kolumnę alarmów. Następnie zwiększ częstość awarii do 0.05 i porównaj liczbę prawdziwych i fałszywych alarmów. Na koniec wróć do 0.01 i zmniejszaj FPR."),
            alarm_terms_table,
            figure_panel(
              label = "Tablica 2×2", title = "Zmień parametry detektora",
              lc_p("Liczebności są zaokrągloną ilustracją dla 10 000 zmian. Prawdopodobieństwa obliczamy bezpośrednio z parametrów modelu."),
              lc_toolbar(
                lc_slider("a3_prev", "Częstość awarii", 0.001, 0.10, 0.01, 0.001),
                lc_slider("a3_sens", "Czułość", 0.50, 1, 0.95, 0.01),
                lc_slider("a3_fpr", "FPR (fałszywe alarmy)", 0, 0.30, 0.05, 0.01)
              ),
              uiOutput("a3_table"),
              full_width = TRUE
            ),
            c(
              "Przy ustawieniach domyślnych wiersz awarii ma 100 zmian: 95 z alarmem i 5 bez. Wiersz bez awarii ma 9900 zmian: 495 z alarmem i 9405 bez. Kolumna alarmów zawiera więc 95 + 495 = 590 zmian, z których prawdziwych jest mniej niż jedna szósta. Czujnik jest dobry w obu wierszach — myli się w 5% przypadków — a mimo to w kolumnie alarmów przeważają pomyłki, bo wiersz bez awarii jest 99 razy liczniejszy.",
              "Przy częstości awarii 0.05 tablica zmienia się zasadniczo: 475 prawdziwych i 475 fałszywych alarmów, czyli dokładnie pół na pół. Detektor się nie zmienił; zmieniła się proporcja wierszy. Zmniejszanie FPR działa w tym samym kierunku: każdy punkt procentowy FPR to w tej hali 99 fałszywych alarmów na 10 000 zmian."
            ),
            "Wróćmy do ulotki producenta z przykładu 3.2. Jego „74% trafności” to w rzeczywistości wiarygodność alarmu w kampanii testowej: udział prawdziwych alarmów w kolumnie alarmów. Policzmy tę samą liczbę dla hali, przy ustawieniach domyślnych tablicy, i postawmy obie tablice obok siebie.",
            a3_stop_campaign_ui,
            "Ten sam czujnik: wiarygodność alarmu w kampanii wynosi 0.74, w hali 0.16. Ilorazy w wierszach są identyczne, 0.95 i 0.05, bo opisują czujnik. Różni się tylko pasek nad tablicami: w kampanii awarie to 13% zmian, w hali 1%. Od tego udziału zależy kolumna alarmów. Wiersze te same, kolumny inne — dlatego liczby z ulotki nie przenosimy do hali."
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
        "Dziennik dyżurnego pokazał to na 2000 zmian: 19 prawdziwych i 99 fałszywych telefonów. Zmian bez awarii jest tak dużo, że nawet rzadkie fałszywe alarmy tworzą tłum liczniejszy niż wszystkie prawdziwe alarmy razem wzięte. Teraz ten sam rachunek zapiszemy dla 10 000 zmian i dowolnych parametrów detektora."
      ),
      sections = list(
        list(
          id = "mianownik", title = "Wszystkie alarmy",
          text = "Dla pytania po alarmie mianownikiem są prawdziwe i fałszywe alarmy razem. Dyżurny nie wie, z której grupy pochodzi jego telefon — wie tylko, że alarm jest. Dlatego wiarygodność alarmu to udział prawdziwych alarmów wśród wszystkich alarmów, a nie wśród awarii.",
          body = list(
            risk_definition("3.4", "Wartości predykcyjne", c(
              "Wartość predykcyjna dodatnia PPV (ang. positive predictive value) to P(A | 🚨): prawdopodobieństwo, że zdarzenie zachodzi, gdy detektor alarmuje. W tym wykładzie nazywamy ją też wiarygodnością alarmu lub posteriorem po alarmie.",
              "Wartość predykcyjna ujemna NPV (ang. negative predictive value) to P(Aᶜ | 🔕): prawdopodobieństwo, że zdarzenia nie ma, gdy detektor milczy. W przeciwieństwie do czułości i swoistości obie wartości predykcyjne zależą od częstości bazowej."
            )),
            "W tablicy 2×2 wartości predykcyjne to ilorazy w kolumnach: PPV = TP/(TP + FP), NPV = TN/(TN + FN). Czułość i swoistość czytamy wierszami, wartości predykcyjne — kolumnami. W dzienniku dyżurnego kolumna alarmów to 118 telefonów, z których prawdziwych było 19.",
            risk_formula("\\begin{aligned}\\text{PPV} &= P(A\\mid \\text{🚨})=\\frac{TP}{TP+FP},\\\\[0.6em] \\text{NPV} &= P(A^{c}\\mid \\text{🔕})=\\frac{TN}{TN+FN}\\end{aligned}",
              legend = c("TP + FP" = "wszystkie alarmy (kolumna 🚨)", "TN + FN" = "wszystkie zmiany bez alarmu (kolumna 🔕)")),
            alarm_ppv_widget,
            risk_try("w tablicy 2×2 z poprzedniego rozdziału odczytaj kolumnę alarmów przy ustawieniach domyślnych. Potem zmniejsz FPR do 0.01 i porównaj liczbę prawdziwych i fałszywych alarmów. Drzewo dróg w następnej sekcji korzysta z tych samych parametrów."),
            c(
              "Przy ustawieniach domyślnych kolumna alarmów ma 95 prawdziwych i 495 fałszywych alarmów, a P(awaria | alarm) = 0.161 — dokładnie ta sama proporcja co 19 do 99 w dzienniku dyżurnego. Przy FPR = 0.01 fałszywych alarmów jest 99, prawdziwych nadal 95, a wiarygodność alarmu rośnie do 0.490.",
              "Wniosek jest praktyczny: przy rzadkich awariach o wiarygodności alarmu decyduje przede wszystkim FPR, a nie czułość. Podniesienie czułości z 0.95 do 1 dodałoby pięć prawdziwych alarmów; obniżenie FPR o jeden punkt procentowy usuwa 99 fałszywych."
            ),
            risk_check("d3_chk_ppv",
              "W hali o częstości awarii 0.01 porównujemy detektor A (czułość 0.95, FPR 0.05) z detektorem B (czułość 0.80, FPR 0.01). Który ma wyższą wiarygodność alarmu P(awaria | alarm)?",
              c("Detektor A, bo ma wyższą czułość" = "a", "Detektor B" = "b", "Oba jednakową, bo działają w tej samej hali" = "same"),
              correct = "b",
              explanation = "Na 10 000 zmian: A daje 95 prawdziwych i 495 fałszywych alarmów (PPV ≈ 0.161), B daje 80 prawdziwych i 99 fałszywych (PPV = 80/179 ≈ 0.447). Ceną jest 20 przeoczonych awarii na 100 zamiast 5 — wybór detektora to kompromis, nie ranking.",
              hints = c(
                a = "Policz fałszywe alarmy obu detektorów wśród 9900 zmian bez awarii.",
                same = "Ta sama hala oznacza tę samą częstość bazową, ale detektory mają różne FPR."
              )
            ),
            a3_stop_detectors_ui,
            "Detektor C łapie każdą awarię, a mimo to jego P(awaria | 🚨) wynosi tylko 0.17 — złoty stos jest taki sam jak u A. Przy 9900 sprawnych zmianach nawet 5% fałszywych alarmów daje 495 pomyłek, więc P(awaria | 🚨) podnosi dopiero niższy FPR detektora B — za cenę 20 przeoczonych awarii zamiast 5. Wybór detektora to kompromis między fałszywymi alarmami a przeoczeniami, a nie ranking: który jest lepszy, zależy od tego, co w tej hali kosztuje więcej."
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
              "Krok pierwszy: z definicji prawdopodobieństwa warunkowego P(A | 🚨) = P(A ∩ 🚨) / P(🚨). Licznik to prawdopodobieństwo, że zmiana leży na drodze „awaria i alarm”; mianownik — że kończy się alarmem.",
              "Krok drugi: licznika nie znamy wprost, ale znamy czułość. Reguła mnożenia daje P(A ∩ 🚨) = P(A) · P(🚨 | A): idziemy po drzewie najpierw gałęzią „awaria”, potem gałęzią „alarm”. W Bananpolu: 0.01 · 0.95 = 0.0095.",
              "Krok trzeci: mianownik rozkładamy na dwie rozłączne drogi. Zdarzenia A i Aᶜ tworzą podział wszystkich zmian, więc wzór na prawdopodobieństwo całkowite daje:"
            ),
            risk_formula("P(\\text{🚨})=P(\\text{🚨}\\mid A)\\,P(A)+P(\\text{🚨}\\mid A^{c})\\,P(A^{c})", num = "3.3",
              legend = c("P(\\text{🚨})" = "prawdopodobieństwo alarmu na losowej zmianie", "P(\\text{🚨}\\mid A)" = "czułość", "P(\\text{🚨}\\mid A^{c})" = "FPR", "P(A)" = "częstość bazowa")),
            "W Bananpolu P(🚨) = 0.95 · 0.01 + 0.05 · 0.99 = 0.0095 + 0.0495 = 0.059, czyli 590 alarmów na 10 000 zmian. Podstawiając licznik z kroku drugiego i mianownik (3.3) do definicji, dostajemy wzór Bayesa.",
            a3_stop_total_ui,
            "Obie drogi do alarmu widać na drzewie: w hali B awarii jest pięć razy więcej, więc droga przez awarię zrównuje się z drogą przez fałszywe alarmy, a inny czujnik zwęża przede wszystkim drogę fałszywą, więc z 590 alarmów zostaje 179.",
            risk_formula("P(A\\mid \\text{🚨})=\\frac{P(\\text{🚨}\\mid A)\\,P(A)}{P(\\text{🚨}\\mid A)\\,P(A)+P(\\text{🚨}\\mid A^{c})\\,P(A^{c})}", num = "3.4",
              legend = c("P(A\\mid \\text{🚨})" = "wiarygodność alarmu (PPV, posterior)", "P(\\text{🚨}\\mid A)\\,P(A)" = "droga przez awarię — licznik", "P(\\text{🚨}\\mid A^{c})\\,P(A^{c})" = "droga przez fałszywe alarmy")),
            lc_p("Licznik jest drogą przez awarię; mianownik sumą obu dróg kończących się alarmem."),
            a3_stop_ppv_ui,
            lc_p(
              "Wzór Bayesa nie wnosi nowej matematyki — porządkuje rachunek, który
               wykonaliśmy na zmianach. Warto rozpoznać w mianowniku starego znajomego:
               to wzór na prawdopodobieństwo całkowite z poprzedniego wykładu,
               zastosowany do zdarzenia „alarm”. Nowa jest tylko nazwa."
            ),
            risk_derivation("wzór Bayesa w trzech linijkach", c(
              "Całe wyprowadzenie to połączenie definicji z wykładu 02. Każda linijka odpowiada jednemu krokowi z tekstu."
            ), lines = c(
              "P(A | 🚨) = P(A ∩ 🚨) / P(🚨)                          (definicja warunku)",
              "P(A ∩ 🚨) = P(🚨 | A) · P(A)                          (reguła mnożenia)",
              "P(🚨) = P(🚨 | A) · P(A) + P(🚨 | Aᶜ) · P(Aᶜ)          (prawdopodobieństwo całkowite, 3.3)",
              "P(A | 🚨) = 0.0095 / (0.0095 + 0.0495) = 0.0095 / 0.059 ≈ 0.161"
            )),
            c(
              "Ten sam schemat daje wartość predykcyjną ujemną. Dyżurny, który przez całą zmianę nie dostał alarmu, też ma pytanie: czy mogę spokojnie spać? Zamieniamy w rachunku 🚨 na 🔕, czułość na 1 - czułość, a FPR na swoistość."
            ),
            risk_formula("P(A^{c}\\mid \\text{🔕})=\\frac{P(\\text{🔕}\\mid A^{c})\\,P(A^{c})}{P(\\text{🔕}\\mid A^{c})\\,P(A^{c})+P(\\text{🔕}\\mid A)\\,P(A)}", num = "3.5",
              legend = c("P(\\text{🔕}\\mid A^{c})" = "swoistość", "P(\\text{🔕}\\mid A)" = "1 - czułość, czyli odsetek przeoczonych awarii")),
            a3_stop_npv_ui,
            risk_example("3.3", "Czy cisza uspokaja?",
              problem = "Przy częstości awarii 0.01, czułości 0.95 i FPR 0.05 oblicz NPV oraz prawdopodobieństwo awarii na zmianie, na której czujnik milczał. Porównaj je z częstością bazową.",
              steps = c(
                "Naturalne częstości: z 10 000 zmian bez alarmu jest 5 zmian z awarią (przeoczonych) i 9405 zmian bez awarii, razem 9410.",
                "Ze wzoru (3.5): NPV = 9405/9410 = (0.95 · 0.99)/(0.95 · 0.99 + 0.05 · 0.01) ≈ 0.9995.",
                "P(awaria | brak alarmu) = 1 - NPV = 5/9410 ≈ 0.00053.",
                "Częstość bazowa wynosi 0.01, czyli prawie dziewiętnaście razy więcej: 0.01/0.00053 ≈ 18.8."
              ),
              answer = "NPV ≈ 0.9995. Brak alarmu obniża szansę awarii z 0.01 do około 0.0005 — cisza jest bardzo wiarygodna, alarm znacznie mniej. Asymetria wynika z częstości bazowej, a nie z tego, że czujnik „lepiej milczy, niż alarmuje”: czułość i swoistość są tu równe."
            )
          ),
          decision = "Komunikuj posterior wraz z liczebnościami, a nie samą czułość."
        ),
        list(
          id = "przyczyny", title = "Więcej niż dwa stany",
          body = list(
            c(
              "Alarm rozdzielał świat na dwa stany: awaria albo jej brak. Często stanów jest więcej. Po incydencie w Bananpolu inspektor pyta, w którym trybie pracy przebiegała zmiana; po alarmie serwis pyta, który z kilku podzespołów zawiódł. Schemat rachunku się nie zmienia: licznik to jedna droga drzewa, mianownik — suma wszystkich dróg kończących się tą samą obserwacją."
            ),
            a3_stop_service_ui,
            "Serwisant nie liczył niczego nowego: każdy słupek po obserwacji to udział podzespołu w rejestrze razy częstość objawu przy jego awarii, a mianownik 35 to wszystkie awarie z tym objawem. Obserwacja przesuwa ocenę w stronę przyczyny, przy której jest ona częstsza — nawet jeśli ta przyczyna jest rzadka. Ogólnie zapisuje to wzór (3.6).",
            "Zapiszmy to ogólnie. Ukryte stany A₁, …, Aₖ tworzą układ zupełny (definicja 2.2 z wykładu 02) — tu sterownik, sprężarka i wentylator — a O oznacza obserwację: alarm, incydent, objaw. Mianownik to wzór na prawdopodobieństwo całkowite (2.4) zastosowany do O.",
            risk_formula("P(A_j\\mid O)=\\frac{P(O\\mid A_j)\\,P(A_j)}{\\sum_{i=1}^{k} P(O\\mid A_i)\\,P(A_i)}", num = "3.6",
              legend = c("A_j" = "stan, o który pytamy", "P(A_j)" = "udział stanu przed obserwacją", "P(O\\mid A_j)" = "prawdopodobieństwo obserwacji w tym stanie", "\\sum_i" = "prawdopodobieństwo całkowite obserwacji, wzór (2.4)")),
            "Wzór (3.4) jest przypadkiem k = 2: stanami są A i Aᶜ, a obserwacją alarm 🚨. Ogólna postać przydaje się wszędzie tam, gdzie po fakcie pytamy o źródło.",
            risk_example("3.4", "Po incydencie: który tryb?",
              problem = list(
                "Na zmianie w Bananpolu doszło do incydentu.",
                risk_parts(
                  "Jakie jest prawdopodobieństwo, że czujnik wykrył na niej przegrzanie? Dane z drzewa 1000 zmian z wykładu 02: P(przegrzanie) = 0.10, P(incydent | przegrzanie) = 0.12, P(incydent | brak przegrzania) = 5/900.",
                  "Zmiany dzielą się na przeciążone (udział 0.20, P(incydent | przeciążenie) = 0.15) i normalne (P(incydent | normalna praca) = 0.01). Jakie jest prawdopodobieństwo, że zmiana z incydentem była przeciążona?"
                )
              ),
              steps = c(
                "Licznik: 0.10 · 0.12 = 0.012. Mianownik ze wzoru (2.4): 0.012 + 0.90 · 5/900 = 0.012 + 0.005 = 0.017. Ze wzoru (3.6): P(przegrzanie | incydent) = 0.012 / 0.017 ≈ 0.706.",
                "Licznik: 0.20 · 0.15 = 0.030. Mianownik: 0.030 + 0.80 · 0.01 = 0.038. Stąd P(przeciążenie | incydent) = 0.030 / 0.038 ≈ 0.789."
              ),
              steps_type = "a",
              answer = "(a) Około 0.71: przegrzanie dotyczy tylko 10% zmian, ale towarzyszy siedmiu na dziesięć incydentów. (b) Około 0.79: przeciążenie to 20% zmian, ale prawie cztery piąte incydentów. Obserwacja przesuwa ocenę w stronę stanu, w którym jest ona częstsza."
            ),
            "Wzór (3.6) wskazuje najbardziej prawdopodobne źródło incydentu. W przykładzie 3.4(a) P(incydent | przegrzanie) = 0.12, a P(przegrzanie | incydent) ≈ 0.71 — dwa kierunki tego samego warunku różnią się prawie sześciokrotnie.",
            risk_check("d3_chk_przyczyny",
              "W Bananpolu P(incydent | przegrzanie) = 0.12, a P(przegrzanie | incydent) ≈ 0.71. Która informacja najbardziej odpowiada za to, że druga liczba jest dużo większa od pierwszej?",
              c("Incydenty są rzadkie: jest ich 17, a zmian z przegrzaniem 100" = "base", "Czujnik przegrzania jest mało dokładny" = "sensor", "Wzór Bayesa zawsze zwiększa prawdopodobieństwo" = "always"),
              correct = "base",
              explanation = "Z reguły iloczynu (2.2) stosunek obu liczb to P(przegrzanie) / P(incydent) = 0.10 / 0.017 ≈ 5.9. Decyduje stosunek wielkości grup, do których się odnoszą: 100 zmian z przegrzaniem wobec 17 incydentów.",
              hints = c(sensor = "Obie liczby dotyczą tych samych 12 zmian z oboma zdarzeniami. Co różni mianowniki 100 i 17?", always = "Wzór (3.6) może też obniżać prawdopodobieństwo — np. P(normalna praca | incydent) ≈ 0.21 < 0.80.")
            )
          )
        ),
        list(
          id = "zonk", title = "I tu był Zonk",
          body = list(
            c(
              "Pamiętasz studio „Idź na całość” z początku wykładu 02? Zmiana bramki wygrywała w dwóch grach na trzy, a klasyczne „50 na 50” zawiodło. W wykładzie 02 rozwiązaliśmy zagadkę, licząc możliwe światy gry. Teraz wystarczy jeden rachunek.",
              "To dokładnie sytuacja z tego wykładu: położenie nagrody jest ukrytym stanem, a gest prowadzącego — obserwacją. Prowadzący pełni rolę czujnika, który „alarmuje” z różnym prawdopodobieństwem zależnie od tego, gdzie stoi nagroda."
            ),
            "Wybrałeś bramkę 2. Niech H₁, H₂, H₃ oznaczają położenie nagrody — to układ zupełny, każdy stan z prawdopodobieństwem 1/3. Obserwacja O: prowadzący otworzył bramkę 1. Z zasad gry P(O | H₁) = 0 (nie odsłoni nagrody), P(O | H₂) = 1/2 (rzuca monetą), P(O | H₃) = 1 (nie ma wyboru). Mianownik to wzór (2.4), licznik — droga przez H₃:",
            tags$pre(class = "lc-derivation-lines", paste(c("P(O) = 1/3 · 0 + 1/3 · 1/2 + 1/3 · 1 = 1/2", "P(H₃ | O) = (1/3 · 1) / (1/2) = 2/3", "P(H₂ | O) = (1/3 · 1/2) / (1/2) = 1/3"), collapse = "\n")),
            "Ten sam wynik co w symulacji z wykładu 02: 2/3 dla zmiany, 1/3 dla pozostania. Prowadzący „nie ma wyboru”, gdy nagroda stoi za bramką 3, a rzuca monetą, gdy stoi za Twoją — i ta asymetria jest całą informacją, którą niesie jego gest."
          )
        )
      )
    ),
    list(
      id = "baza", title = "Częstość bazowa", hook = "Ten sam alarm znaczy co innego w innej hali",
      lead = "Ten sam czujnik daje inną wiarygodność alarmu w innej populacji, a drugi alarm pomaga tylko o tyle, o ile wnosi nową informację.",
      intro = c(
        "Częstość bazowa z definicji 3.3 to P(A): jak często awaria zdarza się w danej hali, zanim zadzwoni telefon — 0.01, czyli 1 zmiana na 100, czyli szanse 1 : 99. Wiarygodność alarmu nie jest cechą czujnika — jest cechą pary: czujnik plus populacja, w której pracuje. Ten sam model detektora zamontowany w hali o rzadkich awariach będzie „krzyczał wilk” znacznie częściej niż w hali, gdzie awarie są powszechne.",
        "Krzywa poniżej pokazuje tę zależność w całym zakresie. Zauważ, jak stromo rośnie na początku: przy bardzo rzadkich awariach niewielka zmiana częstości bazowej silnie zmienia sens alarmu. To dlatego przenoszenie parametrów detektora między instalacjami bez sprawdzenia częstości bazowej jest błędem, a nie oszczędnością."
      ),
      sections = list(
        list(
          id = "krzywa", title = "Pułapka częstości bazowej",
          body = list(
            a3_stop_halls_ui,
            "Czujnik w obu halach jest ten sam, zmieniła się tylko częstość awarii: z 1% na 5%. To wystarczyło, by wiarygodność alarmu wzrosła z 0.16 do 0.50. Krzywa poniżej pokazuje tę zależność dla każdej częstości bazowej; dwie hale to na niej dwa punkty, przy 0.01 i przy 0.05.",
            risk_definition("3.5", "Pułapka częstości bazowej", c(
              "Pułapka częstości bazowej (ang. base rate neglect) to ocena wyniku detektora bez uwzględnienia, jak często zdarzenie występuje w danej populacji. Najczęściej polega na utożsamieniu czułości P(🚨 | A) z wiarygodnością alarmu P(A | 🚨).",
              "Pułapka jest najgroźniejsza, gdy zdarzenie jest rzadkie: wtedy nawet niewielki odsetek fałszywych alarmów daje ich więcej niż prawdziwych."
            )),
            risk_try("przy obecnym czujniku odczytaj P(awaria | 🚨) dla trzech hal. Potem włącz tańszy czujnik: o ile zmienia się wynik w nowej hali? Na koniec włącz ciche tło i porównaj."),
            a3_stop_curve_ui,
            c(
              "Przy obecnym czujniku w nowej hali prawdziwy jest mniej więcej co szósty alarm (0.16), w starej co drugi (0.50), w lipcowy upał pięć na sześć (0.83). Ten sam czujnik w lipcu jest wiarygodniejszy niż w styczniu, bo w lipcu awarie są częstsze — tak jak test przesiewowy wykonany w grupie ryzyka daje wiarygodniejszy wynik dodatni niż ten sam test w całej populacji. Alarm to mieszanka dwóch strumieni: prawdziwe alarmy rosną razem z awaryjnością hali, a fałszywe są stałym tłem — około 5 na 100 spokojnych zmian. W hali, która rzadko się psuje, tło zagłusza sygnał.",
              "Tańszy czujnik prawie nie zmienia P(awaria | 🚨) w nowej hali: 0.14 zamiast 0.16, bo fałszywych alarmów jest tyle samo, a prawdziwych ubyło niewiele. Jego cena jest gdzie indziej: przegapia 20 awarii na 100 zamiast 5. Ciche tło (FPR 0.01) podnosi wynik w nowej hali do 0.49 — prawie trzy razy. Przy rzadkich awariach o wiarygodności alarmu decyduje FPR, a nie czułość; dlatego badania przesiewowe kieruje się do grup ryzyka, czyli tam, gdzie częstość bazowa jest wyższa.",
              "Tę stromość najłatwiej zrozumieć, gdy zamiast prawdopodobieństw użyjemy szans. Szanse zdarzenia to iloraz P(A)/P(Aᶜ): częstość 0.01 odpowiada szansom 1 : 99, częstość 0.05 — szansom 1 : 19. Dzieląc wzór Bayesa (3.4) dla A przez ten sam wzór dla Aᶜ, skracamy wspólny mianownik P(🚨) i dostajemy bardzo prostą zależność."
            ),
            risk_definition("3.6", "Iloraz wiarygodności", c(
              "Iloraz wiarygodności wyniku dodatniego to LR₊ = P(🚨 | A) / P(🚨 | Aᶜ) = czułość / FPR. Mówi, ile razy częściej alarm pojawia się przy awarii niż bez niej.",
              "Analogicznie iloraz wiarygodności wyniku ujemnego to LR₋ = P(🔕 | A) / P(🔕 | Aᶜ) = (1 - czułość) / swoistość. LR₊ > 1 podnosi przekonanie o zdarzeniu, LR₋ < 1 je obniża."
            )),
            risk_formula("\\frac{P(A\\mid \\text{🚨})}{P(A^{c}\\mid \\text{🚨})}=\\frac{P(\\text{🚨}\\mid A)}{P(\\text{🚨}\\mid A^{c})}\\cdot\\frac{P(A)}{P(A^{c})}", num = "3.7",
              legend = c("\\frac{P(A)}{P(A^{c})}" = "szanse a priori", "\\frac{P(\\text{🚨}\\mid A)}{P(\\text{🚨}\\mid A^{c})}" = "iloraz wiarygodności LR₊", "\\frac{P(A\\mid \\text{🚨})}{P(A^{c}\\mid \\text{🚨})}" = "szanse a posteriori")),
            "Wzór (3.7) mówi: szanse po alarmie = szanse przed alarmem · LR₊. Czujnik Bananpolu ma LR₊ = 0.95/0.05 = 19 — każdy alarm mnoży szanse awarii przez 19, niezależnie od hali. Jeśli szanse wyjściowe są maleńkie, nawet dziewiętnastokrotny wzrost daje mało; stąd stromość krzywej po lewej stronie.",
            "Wzór (3.7) działa tak samo dla każdej informacji: bierzemy szanse hali, 1 : 99, i mnożymy je przez iloraz wiarygodności tego, co właśnie zobaczyliśmy. Poniżej trzy różne sygnały z tej samej hali Bananpolu.",
            a3_stop_signals_ui,
            "Alarm mnoży szanse przez 19, dodatni odczyt termometru przez 8, a cisza czujnika dzieli je przez 19. Na osi logarytmicznej to trzy strzałki z tego samego punktu, o długości log LR. Ile sygnał zmienia ocenę, zależy od jego LR, a skąd startujemy, zależy od hali. Dlatego w przykładzie 3.5 ten sam czujnik da w dwóch halach różne wyniki.",
            risk_example("3.5", "Ten sam czujnik w dwóch halach",
              problem = "Czujnik (czułość 0.95, FPR 0.05) pracuje w hali A, gdzie awaria zdarza się na 0.01 zmian, i w hali B ze starszym sprzętem, gdzie awaria zdarza się na 0.05 zmian. Oblicz wiarygodność alarmu w obu halach metodą szans.",
              steps = c(
                "LR₊ = 0.95/0.05 = 19 w obu halach — to cecha czujnika.",
                "Hala A: szanse a priori 0.01/0.99 = 1/99. Ze wzoru (3.7) szanse a posteriori = 19/99. Prawdopodobieństwo: 19/(19 + 99) = 19/118 ≈ 0.161.",
                "Hala B: szanse a priori 0.05/0.95 = 1/19. Szanse a posteriori = 19 · 1/19 = 1, czyli 1 : 1. Prawdopodobieństwo: 1/(1 + 1) = 0.5.",
                "Kontrola wzorem (3.4) dla hali B: 0.95 · 0.05/(0.95 · 0.05 + 0.05 · 0.95) = 0.0475/0.095 = 0.5."
              ),
              answer = "Hala A: około 0.16; hala B: 0.5. Pięciokrotnie wyższa częstość bazowa daje ponad trzykrotnie wyższą wiarygodność alarmu przy identycznym czujniku."
            ),
            risk_check("d3_chk_baza",
              "Ten sam czujnik (LR₊ = 19) zamontowano w hali, w której awaria zdarza się na 0.1 zmian. Jaka jest wiarygodność alarmu?",
              c("Około 0.95 — jak czułość" = "a", "Około 0.68" = "b", "Około 0.16 — jak w hali Bananpolu" = "c"),
              correct = "b",
              explanation = "Szanse a priori 0.1/0.9 = 1/9; po alarmie 19/9. Prawdopodobieństwo 19/(19 + 9) = 19/28 ≈ 0.68. Ten sam czujnik, inna hala, inna wiarygodność alarmu.",
              hints = c(
                a = "Czułość to P(alarm | awaria). Pytamy o kierunek odwrotny — użyj wzoru (3.7).",
                c = "0.16 odpowiada częstości 0.01. Tu częstość jest dziesięć razy wyższa."
              )
            )
          ),
          pitfall = "Porównywanie czujników bez podania populacji zastosowania bywa pozorne."
        ),
        list(
          id = "transfer", title = "Przykład transferowy: test przesiewowy",
          text = "Identyczny mechanizm działa w medycynie. Test przesiewowy o czułości 90% i FPR 9% stosowany w populacji, w której choroba dotyka 1% badanych, daje wynik dodatni, który potwierdza się w mniej więcej jednym przypadku na dziesięć. Dlatego po badaniu przesiewowym wykonuje się test potwierdzający — i dlatego programy przesiewowe kieruje się do grup o podwyższonej częstości bazowej.",
          body = list(
            risk_example("3.6", "Naturalne częstości w badaniu przesiewowym",
              problem = "Test przesiewowy ma czułość 0.90 i FPR 0.09; choroba występuje u 1% badanych. Przedstaw wynik dla 1000 osób w naturalnych częstościach i oblicz, jaka część wyników dodatnich jest prawdziwa.",
              steps = c(
                "Chorzy: 1% z 1000 = 10 osób. Wynik dodatni ma 90% z nich: 9 osób.",
                "Zdrowi: 990 osób. Wynik fałszywie dodatni ma 9% z nich: 0.09 · 990 = 89.1, czyli około 89 osób.",
                "Wszystkich wyników dodatnich: 9 + 89 = 98. Prawdziwych: 9/98 ≈ 0.092.",
                "Wzorem (3.4): 0.9 · 0.01/(0.9 · 0.01 + 0.09 · 0.99) = 0.009/0.0981 ≈ 0.092. Metodą szans (3.7): LR₊ = 0.9/0.09 = 10, szanse 1/99 · 10 = 10/99, prawdopodobieństwo 10/109 ≈ 0.092."
              ),
              answer = "Około 9 na 98 wyników dodatnich jest prawdziwych, czyli mniej więcej jeden na jedenaście. Opis „jeden na dziesięć” w tekście to zaokrąglenie tej samej liczby."
            ),
            "Naturalne częstości mają jeszcze jedną zaletę dydaktyczną: pacjentowi łatwiej zrozumieć „9 z 98 osób z takim wynikiem jest chorych” niż „wartość predykcyjna dodatnia wynosi 9.2%”. Ta sama zasada dotyczy raportu dla kierownika zmiany w Bananpolu."
          )
        ),
        list(
          id = "druga-informacja", title = "Druga informacja",
          text = c(
            "Naturalny odruch po niepewnym alarmie to sięgnięcie po drugie źródło: drugi czujnik, odczyt ręczny, telefon do operatora. Rachunek jest optymistyczny — jeśli druga informacja jest warunkowo niezależna od pierwszej, posterior po pierwszym alarmie staje się częstością bazową dla drugiego i wiarygodność szybko rośnie.",
            "Cały zysk wisi jednak na słowie „niezależna”. Dwa identyczne czujniki obok siebie mogą reagować na to samo zakłócenie elektromagnetyczne, ten sam kurz i tę samą wilgoć. W modelu poniżej drugi czujnik z prawdopodobieństwem zadanym suwakiem kopiuje wynik pierwszego; w pozostałych przypadkach działa niezależnie warunkowo przy ustalonym stanie instalacji. Kopiowanie dotyczy zarówno awarii, jak i jej braku. Oba czujniki zachowują tę samą czułość i FPR."
          ),
          body = list(
            a3_stop_second_ui,
            "Drugi czujnik przepuszcza prawie wszystkie prawdziwe alarmy i tylko co dwudziesty fałszywy, dlatego wiarygodność alarmu rośnie z 0.16 do 0.78. Rachunek działa tylko wtedy, gdy przy ustalonym stanie hali drugi czujnik nie wie, co pokazał pierwszy: tę własność nazywa definicja 3.7, a wzór 3.8 zapisuje ten sam przesiew w szansach.",
            risk_definition("3.7", "Warunkowa niezależność dwóch detektorów", c(
              "Wyniki dwóch detektorów są warunkowo niezależne przy danym stanie, jeśli zarówno przy zdarzeniu, jak i przy jego braku prawdopodobieństwo wspólnego wyniku jest iloczynem prawdopodobieństw pojedynczych wyników: P(🚨₁ ∩ 🚨₂ | A) = P(🚨₁ | A) · P(🚨₂ | A) oraz P(🚨₁ ∩ 🚨₂ | Aᶜ) = P(🚨₁ | Aᶜ) · P(🚨₂ | Aᶜ)."
            )),
            "Przy warunkowej niezależności ilorazy wiarygodności się mnożą, a wzór (3.7) stosujemy dwa razy z rzędu. Posterior po pierwszej informacji staje się szansami a priori dla drugiej.",
            risk_formula("\\frac{P(A\\mid \\text{🚨}_1,\\text{🚨}_2)}{P(A^{c}\\mid \\text{🚨}_1,\\text{🚨}_2)}=\\mathrm{LR}_1\\cdot \\mathrm{LR}_2\\cdot\\frac{P(A)}{P(A^{c})}", num = "3.8",
              legend = c("\\mathrm{LR}_1,\\ \\mathrm{LR}_2" = "ilorazy wiarygodności obu informacji", "\\text{🚨}_1,\\text{🚨}_2" = "alarm pierwszego i drugiego detektora")),
            risk_example("3.7", "Dwa niezależne alarmy",
              problem = "W hali Bananpolu (częstość awarii 0.01) alarmują dwa identyczne czujniki (czułość 0.95, FPR 0.05), których wyniki są warunkowo niezależne. Oblicz posterior po dwóch alarmach dwiema metodami.",
              steps = c(
                "Metoda szans (3.8): szanse a priori 1/99; po dwóch alarmach 1/99 · 19 · 19 = 361/99 ≈ 3.65. Prawdopodobieństwo: 361/(361 + 99) = 361/460 ≈ 0.785.",
                "Metoda sekwencyjna (3.4): po pierwszym alarmie posterior 0.161 staje się nową częstością bazową. 0.95 · 0.161/(0.95 · 0.161 + 0.05 · 0.839) ≈ 0.785.",
                "Naturalne częstości dla 10 000 zmian: 100 awarii · 0.95² ≈ 90 podwójnych alarmów prawdziwych; 9900 zmian bez awarii · 0.05² ≈ 25 podwójnych fałszywych. 90/(90 + 25) ≈ 0.78."
              ),
              answer = "Około 0.785. Drugi niezależny alarm podnosi wiarygodność z 0.16 do niemal 0.79, bo podwójny fałszywy alarm jest rzadki: zdarza się tylko w 0.25% zmian bez awarii."
            )
          )
        ),
        list(
          id = "niezaleznosc", title = "Założenie warunkowej niezależności",
          text = "Dwa czujniki mogą reagować na to samo zakłócenie lub utracić wspólne zasilanie. Warunkowa niezależność oznacza, że przy ustalonym stanie instalacji (awaria albo jej brak) wynik jednego czujnika nie zmienia prawdopodobieństwa wyniku drugiego — i to założenie trzeba uzasadnić mechanizmem, tak jak w poprzednim wykładzie.",
          body = list(
            risk_try("zacznij od prawdopodobieństwa skopiowania 0 i porównaj wynik z przykładem 3.7. Następnie ustaw 0.25, 0.5 i 1. Czujniki mają parametry ustawione w tablicy 2×2 w rozdziale „Czułość i swoistość”. Obserwuj, który pełny słupek rośnie."),
            figure_panel(
              label = "Porównanie", title = "Dwa alarmy",
              lc_toolbar(
                lc_slider("a3_dependence", "Prawdopodobieństwo skopiowania pierwszego alarmu", 0, 1, 0, 0.05),
                lc_readouts(uiOutput("a3_second"))
              ),
              a3_stop_double_plot_ui,
              lc_caption("Przerywany kontur: czujniki niezależne. Pełny słupek: bieżące kopiowanie."),
              full_width = TRUE
            ),
            c(
              "Przy parametrach domyślnych i braku kopiowania panel pokazuje 0.785 — dokładnie wynik przykładu 3.7. Przy kopiowaniu 0.25 posterior po dwóch alarmach spada do 0.391, przy 0.5 do 0.263, a przy pełnym kopiowaniu wraca do 0.161, czyli do wartości po jednym alarmie. Nawet umiarkowana zależność zjada większość zysku z drugiego czujnika.",
              "Mechanizm widać na wykresie pod suwakiem. Słupek prawdziwych podwójnych alarmów prawie się nie zmienia (od 90 do 95 na 10 000 zmian), bo przy awarii oba czujniki i tak alarmują niemal zawsze. Słupek fałszywych rośnie od 25 do 495. Bez kopiowania podwójny fałszywy alarm wymaga dwóch niezależnych pomyłek (0.05² = 0.0025); przy kopiowaniu wystarczy jedna. Założenie (3.8) jest więc najbardziej optymistycznym wariantem — jeśli nie ma za nim mechanizmu, raport powinien pokazać też wariant z zależnością."
            ),
            a3_stop_sources_ui,
            "Liczba źródeł niczego jeszcze nie przesądza: o wartości drugiego źródła decyduje mechanizm, czyli to, czy oba mylą się z tego samego powodu. Pietrow ufał milczeniu radarów właśnie dlatego, że odblask słońca od chmur mógł oszukać satelitę, ale nie radar; dwa czujniki na jednym wsporniku albo na jednym przewodzie takiej pewności nie dają."
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
          body = list(
            a3_stop_matrix_ui,
            "Macierz konsekwencji ma ten sam kształt co tablica wyników detektora, ale zamiast liczebności zawiera koszty. Wiersze to znów stan instalacji, kolumny — tym razem nie wynik czujnika, lecz decyzja dyżurnego: reagować albo nie. Posterior mówi, jak prawdopodobny jest każdy wiersz; macierz mówi, ile kosztuje każda komórka. Decyzja wymaga obu.",
            "Oddzielenie tych dwóch warstw jest ważne organizacyjnie. Posterior powinien policzyć analityk na podstawie danych o detektorze i hali. Koszty i akceptowalne ryzyko ustala kierownictwo i służby bezpieczeństwa. Gdy obie rzeczy mieszają się w jednej liczbie („alarm jest ważny w 16%, więc go ignorujemy”), nikt nie wie, czy spór dotyczy faktów, czy wartości."
          )
        ),
        list(
          id = "rachunek", title = "Koszt reakcji a koszt zaniechania",
          body = list(
            "Do rachunku potrzebujemy trzech liczb. Oznaczmy: q = P(awaria | 🚨) — posterior po alarmie, policzony w poprzednich rozdziałach (w Bananpolu 0.161); c — koszt reakcji, czyli wyjazdu ekipy (100 zł); L — strata, której reakcja zapobiega, czyli partia owoców (2000 zł). Rozważamy wyłącznie szkodę materialną i zakładamy, że reakcja kosztuje c niezależnie od stanu i całkowicie zapobiega stracie.",
            "Jeśli reagujemy, płacimy c zawsze — przy awarii i bez niej. Jeśli nie reagujemy, płacimy L, ale tylko wtedy, gdy awaria rzeczywiście jest, czyli z prawdopodobieństwem q. Średnio, na wielu takich alarmach:",
            lc_formula_box(withMathJax("$$\\begin{aligned}\\text{oczekiwany koszt reakcji} &= c = 100\\text{ zł}\\\\[0.4em] \\text{oczekiwany koszt braku reakcji} &= q\\cdot L = 2000\\,q\\end{aligned}$$")),
            "Reagujemy, gdy q · L > c, czyli w Bananpolu gdy q > 0.05. Przy q = 0.161 brak reakcji kosztuje średnio około 322 zł, więc wyjazd za 100 zł jest uzasadniony, mimo że większość alarmów jest fałszywa.",
            risk_try("ustaw koszt reakcji c na 350 zł i obserwuj punkty na osi. Która z trzech sytuacji zmienia decyzję i dlaczego właśnie ta? Potem wróć do 100 zł i zwiększaj stratę L."),
            a3_stop_cost_ui,
            c(
              "Na wykresie linia ciągła to c, a przerywana to q · L. Przecinają się tam, gdzie oba koszty są równe: q · L = c, czyli q = c / L. Na prawo od tego punktu reakcja jest tańsza."
            ),
            risk_definition("3.8", "Próg reakcji", c(
              "Próg reakcji q* to wartość posterioru, przy której oczekiwany koszt reakcji i jej braku są równe. Przy posteriorze powyżej progu reakcja minimalizuje oczekiwany koszt; poniżej — nie. Próg zależy od kosztów i skuteczności reakcji, a nie od detektora."
            )),
            risk_formula("q^{*}=\\frac{c}{L},\\qquad \\text{reaguj, gdy } q>q^{*}", num = "3.9",
              legend = c("q" = "P(A | 🚨), posterior po alarmie", "c" = "koszt reakcji", "L" = "strata, której reakcja zapobiega", "q^{*}" = "próg reakcji")),
            "Jeśli reakcja ogranicza stratę tylko o połowę, oczekiwany koszt reakcji to c + q · L/2 = 100 + 1000q; próg rośnie do q > 0.10. Skuteczność działania jest osobnym założeniem. Urazów i pełnej oceny bezpieczeństwa nie sprowadzamy w tym przykładzie do jednej kwoty.",
            "W Bananpolu q* = 100/2000 = 0.05. Łącząc wzór (3.9) z wzorem (3.7), można przełożyć próg na posteriorze na próg na częstości bazowej: reagujemy na alarm, gdy szanse a priori · 19 > 0.05/0.95 = 1/19, czyli gdy szanse a priori przekraczają 1/361. Odpowiada to częstości awarii około 0.0028. Poniżej tej częstości pojedynczy alarm tego czujnika nie uzasadnia wyjazdu przy tych kosztach.",
            "Zanim policzymy przykład 3.8, zobaczmy tę samą regułę w trzech firmach, z tym samym rodzajem alarmu, ale innymi kosztami.",
            a3_stop_examples_ui,
            "Posterior opisuje czujnik i halę, próg — pieniądze i skutki. Dopiero ich porównanie jest decyzją.",
            risk_example("3.8", "Jedna reguła, trzy sytuacje",
              problem = list(
                "Reakcja kosztuje 100 zł, brak reakcji przy awarii 2000 zł, a reakcja w pełni zapobiega stracie. Czujnik ma czułość 0.95 i FPR 0.05. Rozstrzygnij, czy reagować:",
                risk_parts(
                  "Na alarm w hali o częstości awarii 0.01.",
                  "Na alarm w hali o częstości awarii 0.002.",
                  "Na zmianie bez alarmu w hali o częstości awarii 0.01."
                )
              ),
              steps = c(
                "Próg ze wzoru (3.9): q* = 100/2000 = 0.05. Posterior 0.161 > 0.05. Oczekiwany koszt braku reakcji: 2000 · 0.161 ≈ 322 zł > 100 zł. Reagujemy.",
                "Ze wzoru (3.4): 0.95 · 0.002/(0.95 · 0.002 + 0.05 · 0.998) ≈ 0.037 < 0.05. Oczekiwany koszt braku reakcji: 2000 · 0.037 ≈ 73 zł < 100 zł. Sam alarm nie uzasadnia wyjazdu; opłaca się tania druga informacja. Jeśli drugi, warunkowo niezależny czujnik też alarmuje, ze wzoru (3.8): szanse 0.002/0.998 · 361 ≈ 0.72, posterior ≈ 0.42 > 0.05. Reagujemy.",
                "Z przykładu 3.3: P(awaria | brak alarmu) ≈ 0.00053. Oczekiwany koszt braku reakcji: około 1.06 zł. Nie reagujemy."
              ),
              steps_type = "a",
              answer = "(a) reagować; (b) najpierw zweryfikować drugim źródłem, reagować po potwierdzeniu; (c) nie reagować. Ta sama reguła kosztowa daje różne decyzje, bo posterior zależy od hali i od wyniku detektora."
            ),
            risk_check("d3_chk_prog",
              "Wyjazd ekipy podrożał do 300 zł; strata przy zignorowanej awarii nadal wynosi 2000 zł, a reakcja w pełni jej zapobiega. Jaki jest próg reakcji?",
              c("0.05" = "old", "0.15" = "new", "0.50" = "half"),
              correct = "new",
              explanation = "Ze wzoru (3.9): q* = 300/2000 = 0.15. Posterior 0.161 w hali Bananpolu nadal przekracza próg, ale już tylko nieznacznie — niewielka zmiana kosztów lub częstości bazowej może odwrócić decyzję.",
              hints = c(
                old = "0.05 odpowiadało kosztowi reakcji 100 zł. Przelicz iloraz c/L dla nowego kosztu.",
                half = "Próg 0.5 odpowiadałby równym kosztom obu pomyłek. Tu strata jest znacznie większa od kosztu reakcji."
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
        id = "serpuchow-policzony", title = "Serpuchów, policzony",
        body = list(a3_stop_serpuchow_case_ui)
      ), list(
        id = "podsumowanie", title = "Podsumowanie",
        text = c(
          "Wykład zaczął się od pytania dyżurnego: co oznacza alarm? Odpowiedź wymaga odwrócenia warunku. Detektor opisujemy liczbami liczonymi w wierszach tablicy 2×2 — czułością i swoistością (3.1) oraz odsetkiem fałszywych alarmów (3.2) — natomiast dyżurnego interesuje liczba z kolumny: wartość predykcyjna dodatnia P(awaria | alarm). Most między nimi buduje wzór Bayesa (3.4), który jest tylko definicją prawdopodobieństwa warunkowego (2.1) z mianownikiem rozpisanym wzorem na prawdopodobieństwo całkowite (3.3). Ta sama konstrukcja dla wielu stanów (3.6) wskazuje najbardziej prawdopodobne źródło incydentu — i rozstrzyga zagadkę Monty’ego Halla z wykładu 02.",
          "Wiarygodność alarmu nie jest cechą czujnika, lecz pary czujnik–populacja. W zapisie szans (3.7) czujnik wnosi iloraz wiarygodności, a hala — szanse a priori; przy rzadkich awariach nawet dobry czujnik daje przewagę fałszywych alarmów, za to cisza jest bardzo wiarygodna (3.5). Druga informacja mnoży ilorazy wiarygodności (3.8), ale tylko wtedy, gdy jest warunkowo niezależna od pierwszej; wspólna przyczyna zjada zysk.",
          "Posterior nie jest decyzją. Próg reakcji (3.9) wynika z kosztów i skuteczności działania, a nie z detektora; niski posterior może uzasadniać reakcję, jeśli strata jest duża. Rachunek i wartości warto rozdzielić: analityk liczy posterior, organizacja ustala próg, a procedura łączy oba przed nocnym telefonem."
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
    lc_crosstab(counts, measure = "n", row_name = "Stan", col_name = "Odczyt detektora", big_mark = " ",
                lead = FALSE, label = "Tablica 2×2 dla 10 000 zmian")
  })
  output$a3_counts <- renderUI({
    d <- detector()
    # Kolory jak gałęzie drzewa: odczyty pełnią rolę legendy.
    tagList(
      lc_readout("Prawdziwe alarmy", d$alarm[1], color = upwr_accent, swatch = TRUE),
      lc_readout("Fałszywe alarmy", d$alarm[2], color = upwr_single_alt, swatch = TRUE),
      lc_readout("P(awaria | alarm)", risk_fmt_p(posterior()), color = upwr_secondary)
    )
  })
  output$a3_second <- renderUI({
    p1 <- posterior()
    adjusted <- do.call(risk_two_alarm_posterior, c(detector_parameters(),
      list(dependence = input$a3_dependence %||% 0)))
    tagList(
      lc_readout("Po jednym alarmie", risk_fmt_p(p1), color = upwr_secondary),
      lc_readout("Po dwóch alarmach", risk_fmt_p(adjusted), color = upwr_accent)
    )
  })
  output$a3_tree <- renderUI({
    p <- detector_parameters()
    d <- detector()
    alarm_tree_svg(d, p$prevalence, p$sensitivity, p$false_positive_rate)
  })
  a3_stop_szanse_server(input, output, session, detector_parameters)
  a3_stop_prog_server(input, output, session)
  a3_stop_serpuchow_case_server(input, output, session)
  risk_assessment_server("a3", alarm_quiz, input, output)
}
