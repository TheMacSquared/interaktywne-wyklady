warunki_quiz <- list(questions = list(
  list(
  question = "Co zmienia warunek B w prawdopodobieństwie P(A | B)?",
  choices = c(
    "Populację odniesienia" = "den", "Tylko licznik" = "num",
    "Nazwę zdarzenia A" = "name"
  ),
  correct = "den",
  explanation = "Warunek filtruje świat do przypadków spełniających B; w tej populacji liczymy A."
),
  list(question = "P(B)=0,1 i P(A | B)=0,12. Ile wynosi P(A ∩ B)?",
    choices = c("0,12" = "a", "0,22" = "b", "0,012" = "c"), correct = "c",
    explanation = "Reguła iloczynu: 0,1 × 0,12 = 0,012."),
  list(question = "Dodatkowo P(A | brak B)=0,005. Ile wynosi P(A)?",
    choices = c("0,012" = "a", "0,0165" = "b", "0,125" = "c"), correct = "b",
    explanation = "Sumujemy ważone drogi: 0,1×0,12 + 0,9×0,005 = 0,0165."),
  list(question = "Dwa dodatnio prawdopodobne zdarzenia są rozłączne. Czy są niezależne?",
    choices = c("Nie" = "a", "Tak" = "b", "Tylko gdy oba p są małe" = "c"), correct = "a",
    explanation = "Przecięcie ma prawdopodobieństwo zero, a iloczyn ich dodatnich prawdopodobieństw jest dodatni."),
  list(question = "P(A | B)>P(A). Czy usunięcie B na pewno zmniejszy P(A)?",
    choices = c("Tak, wynika to z definicji" = "a", "Tak, jeśli próba jest duża" = "b", "Nie, związek może wynikać ze wspólnej przyczyny" = "c"), correct = "c",
    explanation = "Warunkowanie na obserwacji nie jest tym samym co interwencja w proces.")
))
warunki_exercises <- c(
  "Bananpol: policz P(incydent) z dwóch trybów pracy i zapisz wynik jako częstość na 1000 zmian.",
  "Diagnostyka: wskaż, dlaczego wspólne zasilanie narusza założenie niezależności dwóch zabezpieczeń.",
  "Transfer: opisz warunek i właściwy mianownik dla ryzyka wypadku podczas pracy nocnej."
)

warunki_vote <- risk_vote_panel(
  "w2_vote", "w2_vote_feedback",
  "Po wykryciu przegrzania: która liczba opisuje możliwość incydentu?",
  c(
    "P(incydent)" = "marginal", "P(incydent | przegrzanie)" = "conditional",
    "P(przegrzanie | incydent)" = "reverse"
  )
)

warunki_filter_widget <- risk_widget_panel(
  title = "Filtrujemy 1000 zmian Bananpolu",
  controls = tagList(
    sliderInput("w2_share", "Udział zmian z przegrzaniem", 0.02, 0.40, 0.10, 0.01),
    sliderInput("w2_risk_hot", "P(incydent | przegrzanie)", 0.01, 0.30, 0.12, 0.01),
    sliderInput("w2_risk_normal", "P(incydent | brak przegrzania)", 0, 0.05, 0.005, 0.001)
  ),
  plot_id = "w2_filter_plot", stats_id = "w2_filter_stats",
  note = "Każdy punkt oznacza jedną porównywalną zmianę. Kolor i kształt rozróżniają grupy."
)

warunki_monty_widget <- figure_panel(
  label = "Idź na całość",
  title = "Zostać przy bramce czy zmienić wybór?",
  full_width = TRUE,
  uiOutput("w2_monty_controls"),
  uiOutput("w2_monty_doors"),
  uiOutput("w2_monty_feedback"),
  uiOutput("w2_monty_simulation_panel")
)

# --- Rysunki SVG bramek Monty'ego Halla (paleta UPWr, bez plików binarnych) ---

.monty_svg_door_closed <- function(door, highlight = FALSE) {
  frame <- if (highlight) upwr_accent else upwr_secondary
  sprintf(
    '<svg viewBox="0 0 120 150" width="100%%" height="140" role="img" aria-label="Bramka %d: zamknięta">
       <rect x="8" y="6" width="104" height="138" rx="10" fill="%s"/>
       <rect x="18" y="16" width="84" height="118" rx="6" fill="none"
             stroke="#ffffff" stroke-opacity="0.35" stroke-width="3"/>
       <circle cx="96" cy="78" r="5" fill="%s"/>
       <text x="58" y="90" text-anchor="middle" font-size="46" font-weight="bold"
             fill="#ffffff">%d</text>
     </svg>',
    door, frame, unname(upwr_cat[["bursztyn"]]), door
  )
}

.monty_svg_doorway <- function(content, label) {
  sprintf(
    '<svg viewBox="0 0 120 150" width="100%%" height="140" role="img" aria-label="%s">
       <rect x="8" y="6" width="104" height="138" rx="10" fill="%s"/>
       <rect x="16" y="14" width="88" height="122" rx="6" fill="%s"/>
       %s
     </svg>',
    label, upwr_secondary, upwr_panel, content
  )
}

.monty_svg_car <- function() {
  body_col <- unname(upwr_cat[["bursztyn"]])
  wheel_col <- upwr_secondary
  paste0(
    '<path d="M36 86 Q41 64 58 64 L72 64 Q86 64 91 86 Z" fill="', body_col, '"/>',
    '<rect x="24" y="84" width="72" height="24" rx="9" fill="', body_col, '"/>',
    '<rect x="47" y="70" width="21" height="13" rx="3" fill="#ffffff" fill-opacity="0.85"/>',
    '<circle cx="41" cy="110" r="9" fill="', wheel_col, '"/>',
    '<circle cx="41" cy="110" r="3.5" fill="#ffffff"/>',
    '<circle cx="79" cy="110" r="9" fill="', wheel_col, '"/>',
    '<circle cx="79" cy="110" r="3.5" fill="#ffffff"/>',
    '<circle cx="93" cy="92" r="2.5" fill="#ffffff"/>'
  )
}

.monty_svg_goat <- function() {
  goat_col <- upwr_reference
  line_col <- upwr_secondary
  paste0(
    '<ellipse cx="54" cy="93" rx="24" ry="15" fill="', goat_col, '"/>',
    '<rect x="38" y="103" width="5" height="21" rx="2" fill="', goat_col, '"/>',
    '<rect x="48" y="105" width="5" height="19" rx="2" fill="', goat_col, '"/>',
    '<rect x="60" y="105" width="5" height="19" rx="2" fill="', goat_col, '"/>',
    '<rect x="70" y="103" width="5" height="21" rx="2" fill="', goat_col, '"/>',
    '<path d="M32 88 Q25 84 28 76" stroke="', goat_col, '" stroke-width="5" fill="none" stroke-linecap="round"/>',
    '<circle cx="84" cy="74" r="10" fill="', goat_col, '"/>',
    '<ellipse cx="75" cy="69" rx="5" ry="3" fill="', goat_col, '" transform="rotate(-35 75 69)"/>',
    '<path d="M88 66 Q92 57 99 55" stroke="', line_col, '" stroke-width="3" fill="none" stroke-linecap="round"/>',
    '<path d="M83 64 Q84 55 90 51" stroke="', line_col, '" stroke-width="3" fill="none" stroke-linecap="round"/>',
    '<path d="M85 84 L83 93 L90 85 Z" fill="', line_col, '"/>',
    '<circle cx="87" cy="72" r="1.8" fill="', line_col, '"/>'
  )
}

.monty_door_card <- function(door, state, title_text, caption, border) {
  svg <- switch(state,
    closed = .monty_svg_door_closed(door, highlight = FALSE),
    chosen = .monty_svg_door_closed(door, highlight = TRUE),
    zonk = .monty_svg_doorway(
      .monty_svg_goat(), sprintf("Bramka %d: Zonk", door)
    ),
    car = .monty_svg_doorway(
      .monty_svg_car(), sprintf("Bramka %d: nagroda", door)
    )
  )
  tags$div(
    style = paste0(
      "text-align:center; padding:0.6rem 0.5rem; border:2px solid ", border,
      "; border-radius:12px; background:#ffffff; height:100%;"
    ),
    HTML(svg),
    tags$div(
      style = "font-weight:700; margin-top:0.35rem;",
      paste0("Bramka ", door, " · ", title_text)
    ),
    tags$div(
      style = paste0("font-size:0.85rem; color:", upwr_ink_soft, ";"),
      caption
    )
  )
}

warunki_views_widget <- figure_panel(
  label = "Trzy reprezentacje", title = "Te same liczby: tabela, drzewo i udziały",
  fluidRow(
    column(
      5, tableOutput("w2_table"),
      radioButtons("w2_view", "Widok wykresu", c("Drzewo dróg" = "tree", "Udziały" = "shares"), inline = TRUE)
    ),
    column(7, zoom_plot_ui("w2_views_plot", height = "390px"))
  ),
  lc_feedback(type = "info", "Zmiana reprezentacji nie zmienia zdarzenia ani mianownika."),
  full_width = TRUE
)

warunki_total_widget <- risk_widget_panel(
  title = "Dwie drogi do incydentu",
  controls = tagList(
    sliderInput("w2_mode_share", "Udział pracy w przeciążeniu", 0, 1, 0.20, 0.01),
    sliderInput("w2_overload", "P(incydent | przeciążenie)", 0, 0.40, 0.15, 0.01),
    sliderInput("w2_regular", "P(incydent | normalna praca)", 0, 0.10, 0.01, 0.005)
  ),
  plot_id = "w2_total_plot", stats_id = "w2_total_stats",
  note = "Wynik jest ważoną sumą dwóch rozłącznych dróg."
)

warunki_common_widget <- risk_widget_panel(
  title = "Niezależność kontra wspólna przyczyna",
  controls = tagList(
    sliderInput("w2_component_fail", "P awarii pojedynczego zabezpieczenia", 0.001, 0.20, 0.05, 0.001),
    sliderInput("w2_common", "P utraty wspólnego zasilania", 0, 0.10, 0.01, 0.001)
  ),
  plot_id = "w2_common_plot", stats_id = "w2_common_stats",
  note = "Wspólna przyczyna jest osobnym zdarzeniem, a nie nieobjaśnioną korelacją."
)

warunki_path_widget <- figure_panel(
  label = "Przykład liczbowy",
  title = "Od warunku do wspólnej drogi",
  full_width = TRUE,
  lc_stat_grid(
    lc_stat_box("Krok 1 · Przegrzanie", "100 na 1000 zmian", caption = "P(B) = 0,10", color = upwr_cat[["bursztyn"]]),
    lc_stat_box("Krok 2 · Incydent w B", "12 na 100 zmian", caption = "P(A | B) = 0,12", color = upwr_cat[["niebo"]]),
    lc_stat_box("Cała droga · A i B", "12 na 1000 zmian", caption = "P(A ∩ B) = 0,012", color = upwr_accent),
    columns = 3
  ),
  lc_formula_box(withMathJax("$$0{,}10\\times 0{,}12=0{,}012$$")),
  lc_feedback(
    type = "info",
    tags$strong("Czytaj mianowniki:"),
    " drugie 12 odnosi się do 100 zmian z przegrzaniem. Po przemnożeniu wracamy do mianownika 1000 wszystkich zmian."
  )
)

warunki_signal_panel <- figure_panel(
  label = "Interpretacja",
  title = "Co mówi wzrost prawdopodobieństwa warunkowego?",
  full_width = TRUE,
  fluidRow(
    column(
      4,
      lc_stat_box("Bez warunku", "P(A) = 0,017", caption = "17 incydentów na 1000 zmian")
    ),
    column(
      4,
      lc_stat_box("Po przegrzaniu", "P(A | B) = 0,120", caption = "12 incydentów na 100 zmian", color = upwr_accent)
    ),
    column(
      4,
      lc_stat_box("Porównanie", "około 7× więcej", caption = "silny sygnał do dalszego sprawdzenia", color = upwr_cat[["bursztyn"]])
    )
  ),
  tags$div(
    class = "lc-table-wrap",
    tags$table(
      class = "lc-table lc-table-striped lc-table-bordered",
      tags$thead(tags$tr(tags$th("Wniosek"), tags$th("Czy wynika z danych?"), tags$th("Co dalej?"))),
      tags$tbody(
        tags$tr(tags$td("Przegrzanie identyfikuje grupę o wyższej częstości"), tags$td("Tak"), tags$td("Sprawdź stabilność wyniku i jakość rejestru")),
        tags$tr(tags$td("Przegrzanie powoduje incydenty"), tags$td("Jeszcze nie"), tags$td("Poszukaj mechanizmu i zmiennych wspólnych")),
        tags$tr(tags$td("Warto skierować kontrolę na zmiany z przegrzaniem"), tags$td("Możliwa decyzja operacyjna"), tags$td("Określ koszt i skutek fałszywych alarmów"))
      )
    )
  )
)

warunki_sciaga_widget <- tagList(
  figure_panel(
    label = "Ściąga 2.1",
    title = "Trzy zapisy, trzy pytania",
    full_width = TRUE,
    tags$table(
      class = "lc-table lc-table-striped lc-table-bordered",
      tags$thead(tags$tr(
        tags$th("Zapis"), tags$th("Pytanie"), tags$th("Mianownik")
      )),
      tags$tbody(
        tags$tr(tags$td("P(A)"), tags$td("Jak często zachodzi incydent?"), tags$td("wszystkie porównywalne zmiany")),
        tags$tr(tags$td("P(A | B)"), tags$td("Jak często incydent zachodzi w grupie z warunkiem?"), tags$td("tylko zmiany spełniające B")),
        tags$tr(tags$td("P(B | A)"), tags$td("Jak często warunek towarzyszył incydentowi?"), tags$td("tylko zmiany ze zdarzeniem A")),
        tags$tr(tags$td("P(A ∩ B)"), tags$td("Jak często oba naraz?"), tags$td("wszystkie porównywalne zmiany"))
      )
    )
  ),
  lc_formula_box(
    withMathJax("$$P(A\\mid B)=\\frac{P(A\\cap B)}{P(B)}$$"),
    tags$p("Warunek filtruje mianownik: liczymy A wyłącznie wśród przypadków, w których zaszło B.")
  ),
  lc_formula_box(
    withMathJax("$$P(A\\cap B)=P(B)\\,P(A\\mid B)$$"),
    tags$p("Wspólną drogę mnożymy etapami: najpierw wejście do grupy B, potem A wewnątrz tej grupy.")
  ),
  lc_formula_box(
    withMathJax("$$P(A)=\\sum_i P(B_i)\\,P(A\\mid B_i)$$"),
    tags$p("Wynik ogólny jest ważoną sumą rozłącznych dróg — wagi są udziałami trybów pracy.")
  ),
  risk_assessment_ui("w2", warunki_quiz, warunki_exercises)
)

warunki_block <- list(
  id = "warunki", title = "Warunki zmieniają ocenę",
  chapters = list(
    list(
      id = "pytanie", title = "Która liczba odpowiada na pytanie", lead = "Zanim policzymy, nazywamy warunek i populację odniesienia.",
      intro = c(
        "W poprzednim wykładzie ustaliliśmy język: zdarzenie, mianownik, jednostkę i okres. Dziś do tego języka dochodzi jedno słowo, które potrafi zmienić każdą liczbę w raporcie: warunek. Czujnik w dojrzewalni Bananpolu zgłosił przegrzanie łożyska wentylatora — i od tej chwili pytanie „jak często zdarza się incydent?” przestaje mieć jedną odpowiedź.",
        "To samo zdarzenie może mieć inne prawdopodobieństwo w całym zakładzie i inne w wybranej grupie zmian. Kluczowe jest nie tylko to, co liczymy, lecz także spośród jakich przypadków liczymy. Ten wykład uczy zadawać pytanie tak precyzyjnie, żeby wskazywało właściwy mianownik."
      ),
      callout = list(
        label = "Dane Bananpolu",
        text = "Zdarzenie A: incydent przegrzania łożyska podczas zmiany. Warunek B: czujnik wykrył przegrzanie. Jednostka: 8-godzinna zmiana robocza; horyzont: 1000 porównywalnych zmian. Liczby są fikcyjne.",
        color = "uwaga"
      ),
      sections = list(
        list(id = "sens", title = "Trzy podobne zapisy", text = c(
          "P(A), P(A | B) i P(B | A) wyglądają niemal identycznie, ale odpowiadają na trzy różne pytania. W Bananpolu A oznacza incydent, a B przegrzanie. Pierwszy zapis dotyczy wszystkich zmian, drugi wyłącznie zmian z przegrzaniem, a trzeci — wyłącznie zmian, na których doszło do incydentu.",
          "Pomyłka między tymi zapisami nie jest błędem rachunkowym, tylko błędem pytania. Dyrektor, który słyszy „12% zmian z przegrzaniem kończy się incydentem”, a zapamiętuje „12% wszystkich zmian kończy się incydentem”, zawyża problem siedmiokrotnie — mimo że nikt nie policzył niczego źle."
        )),
        list(id = "pytania", title = "Najpierw zdanie, potem symbol", bullets = c("Jak często dochodzi do incydentu? — P(A).", "Jak często dochodzi do incydentu po wykryciu przegrzania? — P(A | B).", "W ilu incydentach wcześniej wykryto przegrzanie? — P(B | A)."))
      ), widget = warunki_vote,
      pitfall = "P(A | B) i P(B | A) zwykle nie są równe."
    ),
    list(
      id = "filtr", title = "Filtrujemy świat", lead = "Zaczynamy w studiu teleturnieju: jedna odsłonięta bramka zmienia całą ocenę.",
      intro = c(
        "Zanim wrócimy do hali Bananpolu, przenieśmy się do studia „Idź na całość”. Przed Tobą trzy bramki: za jedną nagroda, za dwiema Zonk. Wybierasz jedną. Prowadzący — który wie, gdzie stoi nagroda — otwiera jedną z pozostałych bramek i pokazuje Zonka. I pada pytanie, od którego zaczęły się dekady sporów: zostajesz przy swojej bramce czy zmieniasz?",
        "Zagraj kilka rund, zanim przeczytasz cokolwiek dalej, i uruchom symulację tysiąca gier. Po drodze zapisz w głowie odpowiedź na jedno pytanie: czy ruch prowadzącego czegoś Cię nauczył, czy niczego nie zmienił?"
      ),
      widget = tagList(
        warunki_monty_widget,
        lc_h2("warunki-filtr-lekcja", "Co właściwie zrobił prowadzący?"),
        lc_p(
          "Symulacja jest bezlitosna dla intuicji „50 na 50”: zmiana bramki wygrywa
           mniej więcej dwa razy częściej. Żeby zobaczyć dlaczego, policz światy,
           w których możesz się znaleźć. W dwóch grach na trzy Twój pierwszy wybór
           trafia w Zonka — i wtedy prowadzący nie ma żadnej swobody: musi odsłonić
           jedynego pozostałego Zonka, więc nagroda stoi za bramką, na którą się
           przełączysz. Tylko w jednej grze na trzy pierwszy strzał trafia w nagrodę
           i zmiana przegrywa."
        ),
        lc_p(
          "Kluczem nie jest samo otwarcie bramki, lecz to, że ruch prowadzącego zależy
           od tego, co jest ukryte. Jego gest odfiltrowuje część możliwych światów:
           po odsłonięciu Zonka za bramką 1 zostają tylko te scenariusze, które są
           zgodne z tym, co widzisz. Prawdopodobieństwa liczone w tym przefiltrowanym
           świecie różnią się od tych sprzed filtracji — i właśnie ta operacja
           dostanie za chwilę nazwę i wzór."
        ),
        lc_h2("warunki-filtr-definicja", "Od opowieści do definicji"),
        lc_p(
          "Poznanie warunku nie zmienia tego, co się wydarzyło — w studiu ani
           w zakładzie. Zmienia zbiór przypadków, do którego odnosimy licznik.
           Prawdopodobieństwo zdarzenia A pod warunkiem B to udział A liczony
           wyłącznie wśród przypadków, w których zaszło B: filtrujemy mianownik,
           a potem liczymy jak zwykle."
        ),
        lc_formula_box(
          withMathJax("$$P(A\\mid B)=\\frac{P(A\\cap B)}{P(B)}$$"),
          tags$p("Mianownikiem przestaje być cała przestrzeń — zostaje tylko część
                 spełniająca warunek B. Licznik zbiera przypadki, w których zaszły
                 oba zdarzenia naraz.")
        ),
        lc_p(
          "W tym języku gest prowadzącego jest warunkiem B: „za bramką 1 jest Zonk,
           a odsłonił ją prowadzący znający układ”. Pytanie o zmianę bramki to
           pytanie o P(nagroda za bramką 3 | B) — i rachunek na przefiltrowanych
           światach daje 2/3, dokładnie tyle, ile pokazała symulacja."
        ),
        lc_h2("warunki-filtr-mianownik", "Naturalne częstości w Bananpolu"),
        lc_p(
          "Wracamy do hali. Wykryte przegrzanie robi z tysiącem zmian dokładnie to,
           co prowadzący z bramkami: filtruje świat. Najpierw dzielimy 1000 zmian na
           te z przegrzaniem i bez niego, a dopiero potem zliczamy incydenty. Jeśli
           100 zmian spełnia B, to mianownikiem P(A | B) jest 100, a nie 1000."
        ),
        lc_p(
          "Ten sposób liczenia — na konkretnych zmianach zamiast na ułamkach —
           nazywamy naturalnymi częstościami. Wróci on w następnym wykładzie jako
           główne narzędzie do rozbrajania pozornie paradoksalnych wyników. Przy
           każdym prawdopodobieństwie warunkowym zadawaj dwa pytania kontrolne:
           ile przypadków spełnia warunek B i w ilu spośród nich zaszło także A?"
        ),
        lc_p(
          "Suwaki poniżej sterują trzema parametrami naraz: jak częsty jest warunek
           oraz jak ryzykowna jest praca z warunkiem i bez niego. Zwróć uwagę, że
           P(incydent) w całym zakładzie zawsze leży pomiędzy dwiema wartościami
           warunkowymi — bliżej tej grupy, która jest liczniejsza."
        ),
        warunki_filter_widget
      ),
      takeaway = "Warunek zmienia mianownik, nie przeszłość. Prowadzący w studiu i czujnik przegrzania w hali wykonują tę samą operację: zawężają świat, w którym liczymy."
    ),
    list(
      id = "reprezentacje", title = "Jedna sytuacja, trzy reprezentacje", lead = "Tabela, drzewo dróg i udziały są różnymi mapami tych samych liczebności.",
      intro = c(
        "Reprezentacja powinna ułatwiać odpowiedź, a nie zmieniać problem. Tabela dobrze pilnuje liczebności, drzewo pokazuje kolejność warunków, a słupki pomagają porównać częstości w grupach.",
        "W praktyce inspektora wybór reprezentacji to wybór narzędzia komunikacji: tabela przekonuje audytora, który chce sprawdzić sumy, drzewo tłumaczy mechanizm zarządowi, a wykres udziałów najlepiej pokazuje kontrast między grupami na slajdzie. Umiejętność przejścia między nimi bez zmiany liczb jest testem zrozumienia."
      ),
      sections = list(
        list(id = "czytanie", title = "Czytaj od mianownika", bullets = c("Wiersz B wyznacza populację warunkową.", "Komórka A i B jest licznikiem.", "Suma wszystkich dróg prowadzących do A daje P(A).")),
        list(id = "kontrola", title = "Test zgodności", text = "Po zmianie widoku liczba incydentów i liczebność grup pozostają takie same. Siatka i tabela zaokrąglają oczekiwane liczby do całych zmian, więc udziały z ilustracji mogą różnić się od parametrów modelu. Jeśli wynik zmienia się wraz z rodzajem wykresu, zmieniliśmy definicję albo mianownik, a nie tylko reprezentację.")
      ), widget = warunki_views_widget
    ),
    list(
      id = "iloczyn", title = "Mnożymy wzdłuż drogi", lead = "Prawdopodobieństwo wspólnej drogi powstaje przez iloczyn kolejnych etapów.",
      intro = "Iloczyn nie pojawia się jako sztuczka algebraiczna. Odpowiada przejściu przez dwa kolejne filtry: najpierw trafiamy do grupy B, a następnie szukamy A wewnątrz tej grupy. Zanim zapiszemy regułę ogólnie, przejdźmy tę drogę na konkretnych zmianach Bananpolu.",
      sections = list(
        list(id = "droga", title = "Przegrzanie i incydent", text = "Najpierw losujemy zmianę z przegrzaniem, następnie incydent w obrębie tej grupy. Pierwszy czynnik odnosi się do wszystkich zmian, drugi tylko do zmian spełniających warunek."),
        list(id = "jednostki", title = "Sprawdzenie na liczebnościach", text = "Jeśli przegrzanie dotyczy 10% z 1000 zmian, otrzymujemy 100 zmian. Jeżeli incydent występuje w 12% tej grupy, zostaje 12 zmian, czyli 1,2% całej obserwowanej populacji.")
      ),
      widget = tagList(
        warunki_path_widget,
        lc_p("Ten rachunek nie korzystał z niczego szczególnego w liczbach 0,10 i 0,12 — działa dla dowolnych wartości, więc uogólniamy go w jedną regułę:"),
        lc_formula_box(
          withMathJax("$$P(A\\cap B)=P(B)\\,P(A\\mid B)$$"),
          tags$p("Pierwszy czynnik wprowadza do grupy spełniającej warunek, drugi liczy zdarzenie wewnątrz tej grupy.")
        ),
        lc_p(
          "Warto czytać każdy czynnik razem z jego mianownikiem. W zapisie
           P(B)·P(A | B) pierwsza liczba mówi, jaka część wszystkich zmian wchodzi
           do grupy, a druga — jaka część tej grupy kończy się incydentem. Iloczyn
           wraca do wspólnego mianownika wszystkich zmian."
        )
      ),
      decision = "Reguła iloczynu opisuje drogę, ale nie uzasadnia niezależności."
    ),
    list(
      id = "calkowite", title = "Wzór na prawdopodobieństwo całkowite", lead = "Sumujemy rozłączne drogi: incydent może powstać podczas pracy normalnej albo przeciążenia.",
      intro = c(
        "Wynik ogólny jest średnią ważoną wyników w grupach. Wysokie prawdopodobieństwo w rzadkim trybie może mieć mały wkład do całości, natomiast niewielka zmiana w dominującym trybie może silnie przesunąć wynik.",
        "To tłumaczy częste zaskoczenie w raportach bezpieczeństwa: tryb pracy, o którym wszyscy mówią, bo jest spektakularnie ryzykowny, może odpowiadać za mniejszość incydentów — jeśli występuje rzadko. Zanim wskażesz głównego winowajcę, pomnóż ryzyko warunkowe przez udział trybu."
      ),
      sections = list(
        list(id = "partycja", title = "Kompletna partycja", text = "Dzielimy przestrzeń na rozłączne tryby B_i, które razem obejmują wszystkie analizowane zmiany, i sumujemy wkład każdej drogi. Żadna zmiana nie może zniknąć ani należeć do dwóch trybów naraz."),
        list(id = "wagi", title = "Nie sumujemy samych ryzyk warunkowych", text = "P(A | B₁) i P(A | B₂) mają różne mianowniki. Zanim je dodamy, ważymy każde prawdopodobieństwo udziałem odpowiadającego mu trybu pracy. Sprawdź to na suwakach: przesuwaj udział przeciążenia i obserwuj, jak wynik ogólny wędruje między dwiema wartościami warunkowymi.")
      ),
      widget = tagList(
        warunki_total_widget,
        lc_p("Prosta, po której porusza się punkt na wykresie, jest wykresem jednej reguły — ważonej sumy rozłącznych dróg:"),
        lc_formula_box(
          withMathJax("$$P(A)=\\sum_i P(B_i)\\,P(A\\mid B_i)$$"),
          tags$p("Wagi P(B_i) są udziałami trybów pracy i sumują się do jedności.")
        ),
        lc_h2("warunki-calkowite-transfer", "Przykład transferowy: droga do pracy"),
        lc_p(
          "Ten sam wzór działa poza zakładem. Ryzyko kolizji rowerzysty w mieście
           jest ważoną sumą ryzyka na drogach dla rowerów i na jezdni: nawet gdy
           jezdnia jest kilkukrotnie bardziej ryzykowna na kilometr, o łącznym
           wyniku decyduje również to, jaką część trasy stanowi. Zmiana trasy to
           zmiana wag — bez zmiany żadnego ryzyka warunkowego."
        )
      )
    ),
    list(
      id = "niezaleznosc", title = "Niezależność wymaga uzasadnienia", lead = "Dwa urządzenia nie stają się niezależne tylko dlatego, że są dwa.",
      intro = c(
        "Niezależność jest twierdzeniem o mechanizmie i informacji: wiedza o jednym zdarzeniu nie zmienia prawdopodobieństwa drugiego. Nie wynika z osobnych nazw elementów ani z narysowania ich w dwóch gałęziach.",
        "Formalny test jest prosty: A i B są niezależne, gdy P(A | B) = P(A) — warunek niczego nie wnosi. W praktyce rzadko mamy dane, by ten warunek sprawdzić wprost, dlatego uzasadnienie niezależności jest zwykle argumentem o mechanizmie: co fizycznie łączy oba zdarzenia, a co je rozdziela."
      ),
      callout = list(
        label = "Test niezależności",
        text = "Jeśli P(A | B) = P(A), informacja o B nie zmienia oceny A. Każda różnica między tymi liczbami jest miarą zależności.",
        color = "wskazowka"
      ),
      sections = list(
        list(id = "wspolna", title = "Wspólne zasilanie", text = "Utrata wspólnego zasilania może jednocześnie wyłączyć obie gałęzie i zniwelować redundancję. Ten przykład celowo wybiega naprzód: wróci w pełnej skali w wykładach o niezawodności systemu i drzewie błędów."),
        list(id = "audyt", title = "Zanim pomnożysz", bullets = c("Czy elementy mają wspólne zasilanie, otoczenie lub obsługę?", "Czy jedna awaria może obciążyć drugi element?", "Czy oba wyniki pochodzą z tego samego procesu rejestracji?"))
      ),
      widget = warunki_common_widget, pitfall = "P(A ∩ B)=P(A)P(B) wolno użyć dopiero po uzasadnieniu niezależności."
    ),
    list(
      id = "decyzja", title = "Warunek w decyzji", lead = "Działanie kierujemy tam, gdzie warunek istotnie zmienia ocenę.",
      intro = c(
        "Duża różnica między P(A | B) i P(A) może być użyteczna operacyjnie, nawet zanim poznamy pełny mechanizm. Może wskazać grupę do kontroli, ale sama nie rozstrzyga, czy usunięcie B zmniejszy częstość A.",
        "W Bananpolu przegrzanie podnosi ryzyko incydentu z 1,7% do 12% — to sygnał zbyt silny, żeby go zignorować, i zbyt słaby, żeby od razu wymieniać wentylatory. Rozsądna kolejność: skierować kontrolę tam, gdzie warunek wskazuje, i równolegle szukać mechanizmu."
      ),
      sections = list(
        list(id = "ranking", title = "Co sprawdzić najpierw", bullets = c("Nazwij zdarzenie i warunek.", "Porównaj P(A) z P(A | B) na tym samym horyzoncie.", "Sprawdź liczebność grupy B i niepewność wyniku.", "Ustal, czy warunek jest wskaźnikiem, czy możliwą przyczyną.")),
        list(id = "przyczynowosc", title = "Predykcja nie jest interwencją", text = c(
          "Warunek może dobrze przewidywać incydent, ponieważ oba zjawiska mają wspólną przyczynę. Decyzja o kontroli może wtedy nadal być rozsądna, lecz decyzja o usunięciu przyczyny wymaga mocniejszego uzasadnienia.",
          "Klasyczny przykład spoza zakładu: nocne zmiany wiążą się z wyższą częstością wypadków. Czy winna jest pora, zmęczenie, obsada, czy rodzaj zadań zlecanych nocą? Skierowanie dodatkowego nadzoru na noc jest zasadne od razu; przestawienie całej produkcji na dzień — dopiero po zrozumieniu mechanizmu."
        ))
      ),
      widget = warunki_signal_panel,
      decision = "Przegrzanie uzasadnia dodatkową kontrolę, ale sam związek warunkowy nie dowodzi przyczynowości."
    ),
    list(
      id = "sprawdzenie", title = "Ściąga, quiz i ćwiczenia", lead = "Filtruj mianownik, mnóż wzdłuż drogi i sumuj rozłączne drogi.",
      intro = "Ostatni rozdział łączy rachunek z audytem założeń. Poprawny symbol i poprawne działanie nie wystarczą, jeśli zdarzenie, warunek albo populacja odniesienia są niejasne.",
      sections = list(
        list(id = "sciaga", title = "Checklista", bullets = c("Co dokładnie oznaczają A i B?", "Jaki jest mianownik każdego prawdopodobieństwa?", "Czy grupy tworzą kompletną i rozłączną partycję?", "Czy niezależność została uzasadniona mechanizmem?", "Czy związek warunkowy nie został nazwany przyczyną bez dowodu?")),
        list(id = "raport", title = "Jedno zdanie do raportu", text = c(
          "Podaj wynik razem z warunkiem i horyzontem, porównaj go z wynikiem ogólnym, a następnie oddziel obserwowany związek od interpretacji przyczynowej i decyzji operacyjnej.",
          "Wzorzec: „W grupie zmian z wykrytym przegrzaniem incydent wystąpił w 12 na 100 zmian, wobec 17 na 1000 wśród wszystkich zmian. Związek uzasadnia ukierunkowaną kontrolę; nie przesądza o przyczynie.”"
        )),
        list(id = "most", title = "Co dalej", text = "Umiemy już przejść od P(A) do P(A | B). W następnym wykładzie odwrócimy kierunek: czujnik alarmuje, a my pytamy o P(awaria | alarm) — i okaże się, że odwrócenie warunku bez częstości bazowej jest najczęstszym błędem w interpretacji alarmów.")
      ),
      widget = warunki_sciaga_widget
    )
  )
)

warunki_chapters <- risk_block_chapters(warunki_block)

warunki_server <- function(input, output, session) {
  vote_checked <- reactiveVal(FALSE)
  observeEvent(input$w2_vote_check, vote_checked(TRUE))
  output$w2_vote_feedback <- renderUI({
    req(vote_checked())
    if (is.null(input$w2_vote)) {
      return(lc_feedback(type = "info", "Najpierw zaznacz jedną z odpowiedzi."))
    }
    correct <- identical(input$w2_vote, "conditional")
    lc_feedback(
      type = if (correct) "ok" else "warning",
      tags$strong(if (correct) "Tak." else "Nie."),
      " Po wykryciu przegrzania właściwym mianownikiem są zmiany z przegrzaniem."
    )
  })

  monty <- reactiveValues(
    prize = sample.int(3L, 1L),
    chosen = NULL,
    opened = NULL,
    final = NULL,
    strategy = NULL
  )

  choose_monty_door <- function(door) {
    monty$chosen <- as.integer(door)
    possible_zonks <- setdiff(seq_len(3L), c(monty$chosen, monty$prize))
    monty$opened <- sample(possible_zonks, 1L)
    monty$final <- NULL
    monty$strategy <- NULL
  }

  observeEvent(input$w2_monty_door_1, choose_monty_door(1L))
  observeEvent(input$w2_monty_door_2, choose_monty_door(2L))
  observeEvent(input$w2_monty_door_3, choose_monty_door(3L))

  observeEvent(input$w2_monty_stay, {
    req(monty$opened)
    monty$final <- monty$chosen
    monty$strategy <- "pozostanie"
  })

  observeEvent(input$w2_monty_switch, {
    req(monty$opened)
    monty$final <- setdiff(seq_len(3L), c(monty$chosen, monty$opened))
    monty$strategy <- "zmiana"
  })

  observeEvent(input$w2_monty_new, {
    monty$prize <- sample.int(3L, 1L)
    monty$chosen <- NULL
    monty$opened <- NULL
    monty$final <- NULL
    monty$strategy <- NULL
    monty_simulation(NULL)
  })

  output$w2_monty_controls <- renderUI({
    if (is.null(monty$chosen)) {
      return(tagList(
        tags$div(class = "lc-eyebrow", "Krok 1 z 3"),
        tags$h4("Wybierz jedną bramkę"),
        tags$p("Za jedną jest nagroda, za dwiema pozostałymi — Zonk."),
        fluidRow(
          column(4, actionButton("w2_monty_door_1", "Bramka 1", class = "lc-btn-primary", width = "100%")),
          column(4, actionButton("w2_monty_door_2", "Bramka 2", class = "lc-btn-primary", width = "100%")),
          column(4, actionButton("w2_monty_door_3", "Bramka 3", class = "lc-btn-primary", width = "100%"))
        )
      ))
    }
    if (is.null(monty$final)) {
      return(tagList(
        tags$div(class = "lc-eyebrow", "Krok 2 z 3"),
        tags$h4(paste("Wybrałeś bramkę", monty$chosen)),
        tags$p(paste("Prowadzący wiedział, gdzie jest nagroda, i odsłonił Zonka za bramką", monty$opened, ".")),
        tags$p("Co robisz z nową informacją?"),
        fluidRow(
          column(6, actionButton("w2_monty_stay", "Zostaję przy wyborze", class = "lc-btn-primary", width = "100%")),
          column(6, actionButton("w2_monty_switch", "Zmieniam bramkę", class = "lc-btn-primary", width = "100%"))
        )
      ))
    }
    tagList(
      tags$div(class = "lc-eyebrow", "Krok 3 z 3"),
      tags$h4("Sprawdź wynik i zagraj ponownie"),
      actionButton("w2_monty_new", "Nowa gra", class = "lc-btn-secondary-outline", width = "100%")
    )
  })

  output$w2_monty_doors <- renderUI({
    cards <- lapply(seq_len(3L), function(door) {
      if (!is.null(monty$opened) && door == monty$opened) {
        card <- .monty_door_card(
          door, "zonk", "Zonk",
          "Prowadzący odsłonił tę bramkę", upwr_reference
        )
      } else if (!is.null(monty$final)) {
        card <- .monty_door_card(
          door,
          if (door == monty$prize) "car" else "zonk",
          if (door == monty$prize) "Nagroda" else "Zonk",
          if (door == monty$final) "Twój ostateczny wybór" else "Niewybrana bramka",
          if (door == monty$final) upwr_accent else upwr_reference
        )
      } else if (!is.null(monty$chosen) && door == monty$chosen) {
        card <- .monty_door_card(
          door, "chosen", "Twój wybór",
          "Bramka pozostaje zamknięta", upwr_accent
        )
      } else {
        card <- .monty_door_card(
          door, "closed", "Zamknięta",
          "Nagroda albo Zonk", upwr_secondary
        )
      }
      column(4, card)
    })
    tags$div(
      style = "margin:0.75rem 0;",
      do.call(fluidRow, cards)
    )
  })

  output$w2_monty_feedback <- renderUI({
    if (is.null(monty$opened)) {
      return(NULL)
    }
    if (is.null(monty$final)) {
      return(lc_feedback(
        type = "info",
        tags$strong("Nowa informacja:"),
        paste(" bramka", monty$opened, "na pewno nie zawiera nagrody. Zostajesz czy zmieniasz?")
      ))
    }
    won <- identical(monty$final, monty$prize)
    lc_feedback(
      type = if (won) "ok" else "warning",
      tags$strong(if (won) "Nagroda!" else "Zonk."),
      paste0(
        " Strategia: ", monty$strategy, ". Nagroda była za bramką ", monty$prize,
        ". Jedna gra nie rozstrzyga, która strategia jest lepsza — uruchom symulację."
      )
    )
  })

  monty_simulation <- reactiveVal(NULL)

  output$w2_monty_simulation_panel <- renderUI({
    if (is.null(monty$final)) {
      return(NULL)
    }
    tagList(
      tags$div(class = "lc-eyebrow", "Eksperyment wielokrotny"),
      tags$h4("Czy wynik jednej gry był przypadkiem?"),
      actionButton(
        "w2_monty_simulate", "Porównaj strategie w 1000 gier",
        class = "lc-btn-primary", width = "100%"
      ),
      zoom_plot_ui("w2_monty_plot", height = "390px")
    )
  })

  observeEvent(input$w2_monty_simulate, {
    req(monty$final)
    n <- 1000L
    prizes <- sample.int(3L, n, replace = TRUE)
    choices <- sample.int(3L, n, replace = TRUE)
    monty_simulation(data.frame(
      strategy = c("Zostaję", "Zmieniam"),
      win_rate = c(mean(prizes == choices), mean(prizes != choices))
    ))
  })

  monty_plot <- reactive({
    results <- monty_simulation()
    if (is.null(results)) {
      return(
        ggplot() +
          annotate("text", x = 1, y = 0.55, label = "Uruchom 1000 gier", colour = upwr_secondary, size = 5) +
          coord_cartesian(xlim = c(0, 2), ylim = c(0, 1)) +
          labs(title = "Która strategia wygrywa częściej?", x = NULL, y = "Odsetek wygranych") +
          theme_upwr() +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      )
    }
    ggplot(results, aes(strategy, win_rate, fill = strategy)) +
      geom_col(width = 0.62) +
      geom_text(aes(label = scales::percent(win_rate, accuracy = 0.1)), vjust = -0.5, fontface = "bold") +
      geom_hline(yintercept = c(1 / 3, 2 / 3), colour = upwr_reference, linetype = "dotted", linewidth = 0.6) +
      scale_fill_manual(values = c("Zostaję" = upwr_reference, "Zmieniam" = upwr_accent), guide = "none") +
      scale_y_continuous(labels = scales::percent, limits = c(0, 0.78)) +
      labs(
        title = "Wyniki 1000 gier",
        subtitle = "Zmiana wygrywa około dwa razy częściej",
        x = NULL,
        y = "Odsetek wygranych"
      ) +
      theme_upwr()
  })

  zoom_plot_server(
    "w2_monty_plot",
    monty_plot,
    alt = "Porównanie odsetka wygranych przy pozostaniu przy pierwszej bramce i przy zmianie bramki."
  )

  counts <- reactive(risk_conditional_counts(
    1000L, input$w2_share, input$w2_risk_hot, input$w2_risk_normal
  ))
  filter_plot <- reactive({
    d <- counts()
    statuses <- c(rep("Incydent", sum(d$event)), rep("Brak incydentu", sum(d$no_event)))
    groups <- c(
      rep("Przegrzanie", d$event[1]), rep("Brak przegrzania", d$event[2]),
      rep("Przegrzanie", d$no_event[1]), rep("Brak przegrzania", d$no_event[2])
    )
    grid <- data.frame(id = seq_len(1000), status = statuses, group = groups)
    grid$x <- (grid$id - 1L) %% 50L + 1L
    grid$y <- (grid$id - 1L) %/% 50L + 1L
    ggplot(grid, aes(x, y, colour = status, shape = group)) +
      geom_point(size = 1.6) +
      scale_y_reverse() +
      coord_equal() +
      scale_colour_manual(values = c("Incydent" = upwr_accent, "Brak incydentu" = upwr_reference)) +
      labs(title = "1000 porównywalnych zmian", x = NULL, y = NULL, colour = "Wynik", shape = "Warunek") +
      theme_upwr() +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
  })
  zoom_plot_server("w2_filter_plot", filter_plot,
    alt = "Siatka 1000 zmian rozróżniająca incydenty oraz zmiany z przegrzaniem."
  )
  output$w2_filter_stats <- renderUI({
    d <- counts()
    p_all <- sum(d$event) / sum(d$total)
    lc_stat_grid(
      lc_stat_box("Udział incydentów w zaokrąglonej ilustracji", risk_format_probability(p_all)),
      lc_stat_box("Udział przy przegrzaniu — ilustracja", risk_format_probability(d$event[1] / d$total[1]), color = upwr_accent),
      lc_stat_box("P(incydent) w modelu", risk_format_probability(risk_total_probability(input$w2_share, input$w2_risk_hot, input$w2_risk_normal))),
      columns = 1
    )
  })
  output$w2_table <- renderTable(
    {
      counts()
    },
    striped = TRUE,
    bordered = TRUE
  )
  views_plot <- reactive({
    d <- counts()
    if (identical(input$w2_view, "tree")) {
      fmt <- function(p) gsub("\\.", ",", sprintf("%.3f", p))
      nodes <- data.frame(
        x = c(0, 2.6, 2.6, 5.2, 5.2, 5.2, 5.2),
        y = c(0, 1.4, -1.4, 2, .8, -.8, -2),
        label = c(
          "1000 zmian", "Przegrzanie", "Brak przegrzania",
          paste0("Incydent: ", d$event[1]), paste0("Brak: ", d$no_event[1]),
          paste0("Incydent: ", d$event[2]), paste0("Brak: ", d$no_event[2])
        )
      )
      edges <- data.frame(
        xs = c(0, 0, 2.6, 2.6, 2.6, 2.6),
        ys = c(0, 0, 1.4, 1.4, -1.4, -1.4),
        xe = c(2.6, 2.6, 5.2, 5.2, 5.2, 5.2),
        ye = c(1.4, -1.4, 2, .8, -.8, -2),
        p = c(
          fmt(input$w2_share), fmt(1 - input$w2_share),
          fmt(input$w2_risk_hot), fmt(1 - input$w2_risk_hot),
          fmt(input$w2_risk_normal), fmt(1 - input$w2_risk_normal)
        )
      )
      ggplot() +
        geom_segment(data = edges, aes(x = xs, y = ys, xend = xe, yend = ye), colour = upwr_reference, linewidth = .8) +
        geom_label(data = edges, aes((xs + xe) / 2, (ys + ye) / 2, label = p), size = 3.1, colour = upwr_secondary, linewidth = 0) +
        geom_label(data = nodes, aes(x, y, label = label), size = 3.4, fill = upwr_secondary, colour = "white", fontface = "bold", linewidth = 0) +
        coord_cartesian(xlim = c(-.7, 6.3), ylim = c(-2.5, 2.5)) +
        labs(title = "Drzewo dróg: mnożymy wzdłuż gałęzi", subtitle = "Parametry na gałęziach; liczebności na końcach zaokrąglono", x = NULL, y = NULL) +
        theme_upwr() +
        theme(
          axis.text = element_blank(), axis.ticks = element_blank(),
          axis.line = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
    } else {
      long <- data.frame(
        group = rep(d$condition, each = 2), outcome = rep(c("Incydent", "Brak incydentu"), 2),
        count = c(d$event[1], d$no_event[1], d$event[2], d$no_event[2])
      )
      ggplot(long, aes(group, count, fill = outcome)) +
        geom_col(position = "fill") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = c("Incydent" = upwr_accent, "Brak incydentu" = upwr_reference)) +
        labs(title = "Udziały w dwóch mianownikach", x = NULL, y = "Udział", fill = "Wynik") +
        theme_upwr()
    }
  })
  zoom_plot_server("w2_views_plot", views_plot,
    alt = "Drzewo dróg z prawdopodobieństwami gałęzi albo słupki udziału incydentów z warunkiem i bez warunku."
  )

  total_plot <- reactive({
    s <- seq(0, 1, length.out = 201)
    y <- s * input$w2_overload + (1 - s) * input$w2_regular
    ggplot(data.frame(share = s, p = y), aes(share, p)) +
      geom_line(colour = upwr_accent, linewidth = 1) +
      geom_point(
        data = data.frame(
          share = input$w2_mode_share,
          p = risk_total_probability(input$w2_mode_share, input$w2_overload, input$w2_regular)
        ),
        size = 3, colour = upwr_secondary
      ) +
      labs(title = "Suma dwóch dróg", x = "Udział pracy w przeciążeniu", y = "P(incydent)") +
      theme_upwr()
  })
  zoom_plot_server("w2_total_plot", total_plot,
    alt = "Prawdopodobieństwo incydentu rosnące wraz z udziałem pracy w przeciążeniu."
  )
  output$w2_total_stats <- renderUI({
    p <- risk_total_probability(input$w2_mode_share, input$w2_overload, input$w2_regular)
    lc_stat_grid(lc_stat_box("P(incydent)", risk_format_probability(p), color = upwr_accent),
      lc_stat_box("Częstość", risk_natural_frequency(p)),
      columns = 1
    )
  })

  common_plot <- reactive({
    independent <- input$w2_component_fail^2
    with_common <- input$w2_common + (1 - input$w2_common) * independent
    ggplot(data.frame(
      model = c("Tylko niezależne awarie", "Jawna wspólna przyczyna"),
      p = c(independent, with_common)
    ), aes(model, p, fill = model)) +
      geom_col(width = .6) +
      scale_fill_manual(values = upwr_cat_n(2), guide = "none") +
      labs(title = "P jednoczesnej utraty dwóch zabezpieczeń", x = NULL, y = "P(awarii)") +
      theme_upwr()
  })
  zoom_plot_server("w2_common_plot", common_plot,
    alt = "Porównanie prawdopodobieństwa awarii dwóch zabezpieczeń bez i ze wspólną przyczyną."
  )
  output$w2_common_stats <- renderUI({
    independent <- input$w2_component_fail^2
    with_common <- input$w2_common + (1 - input$w2_common) * independent
    lc_stat_grid(lc_stat_box("Model niezależny", risk_format_probability(independent)),
      lc_stat_box("Ze wspólną przyczyną", risk_format_probability(with_common), color = upwr_accent),
      columns = 1
    )
  })
  risk_assessment_server("w2", warunki_quiz, input, output)
}
