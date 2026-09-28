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

warunki_filter_widget <- risk_widget_panel(
  title = "Filtrujemy 500 zmian Bananpolu",
  controls = tagList(
    sliderInput("w2_share", "Udział zmian z przegrzaniem", 0.02, 0.40, 0.10, 0.01),
    sliderInput("w2_risk_hot", "P(incydent | przegrzanie)", 0.01, 0.30, 0.12, 0.01),
    sliderInput("w2_risk_normal", "P(incydent | brak przegrzania)", 0, 0.05, 0.005, 0.001)
  ),
  plot_id = "w2_filter_plot", stats_id = "w2_filter_stats",
  note = "Każdy znak oznacza jedną porównywalną zmianę. Trójkąty to zmiany z przegrzaniem, kółka bez; wypełniony znak to incydent.",
  height = "560px"
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

.monty_svg_cat <- function() {
  cat_col <- upwr_reference
  line_col <- upwr_secondary
  paste0(
    # ogon
    '<path d="M78 112 Q98 110 96 90 Q95 80 88 84" stroke="', cat_col, '" stroke-width="6" fill="none" stroke-linecap="round"/>',
    # siedzący tułów
    '<ellipse cx="60" cy="104" rx="22" ry="20" fill="', cat_col, '"/>',
    # przednie łapy
    '<rect x="46" y="110" width="9" height="16" rx="4" fill="', cat_col, '"/>',
    '<rect x="63" y="110" width="9" height="16" rx="4" fill="', cat_col, '"/>',
    # uszy
    '<path d="M42 66 L46 46 L58 60 Z" fill="', cat_col, '"/>',
    '<path d="M78 66 L74 46 L62 60 Z" fill="', cat_col, '"/>',
    # głowa
    '<circle cx="60" cy="72" r="18" fill="', cat_col, '"/>',
    # oczy
    '<ellipse cx="53" cy="69" rx="2.4" ry="3.4" fill="', line_col, '"/>',
    '<ellipse cx="67" cy="69" rx="2.4" ry="3.4" fill="', line_col, '"/>',
    # nos
    '<path d="M57.5 76 L62.5 76 L60 79 Z" fill="', line_col, '"/>',
    # wąsy
    '<path d="M50 77 L34 74" stroke="', line_col, '" stroke-width="1.6" stroke-linecap="round"/>',
    '<path d="M50 80 L35 83" stroke="', line_col, '" stroke-width="1.6" stroke-linecap="round"/>',
    '<path d="M70 77 L86 74" stroke="', line_col, '" stroke-width="1.6" stroke-linecap="round"/>',
    '<path d="M70 80 L85 83" stroke="', line_col, '" stroke-width="1.6" stroke-linecap="round"/>'
  )
}

.monty_door_card <- function(door, state, title_text, caption, border) {
  svg <- switch(state,
    closed = .monty_svg_door_closed(door, highlight = FALSE),
    chosen = .monty_svg_door_closed(door, highlight = TRUE),
    zonk = .monty_svg_doorway(
      .monty_svg_cat(), sprintf("Bramka %d: Zonk", door)
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

# Widget używa stałych liczb Bananpolu z narracji (1000 zmian, 100 z przegrzaniem,
# 12 i 5 incydentów), więc działa niezależnie od suwaków z poprzedniego rozdziału.
warunki_views_counts <- data.frame(
  condition = c("Przegrzanie", "Brak przegrzania"),
  event = c(12L, 5L),
  no_event = c(88L, 895L),
  total = c(100L, 900L),
  stringsAsFactors = FALSE
)
warunki_views_params <- c(share = 0.10, hot = 0.12, normal = 5 / 900)

# Wielkości, które można zaznaczyć na drzewie. Węzły licznika są bordowe, węzły
# mianownika bursztynowe; przy prawdopodobieństwach warunkowych licznik leży
# wewnątrz mianownika, więc jego węzeł jest liczony w obu.
warunki_views_targets <- list(
  a = list(
    label = "P(A) — incydent wśród wszystkich zmian",
    tex = "P(A)", num_nodes = c(4L, 6L), den_nodes = 1L, num_edges = c(1L, 3L, 2L, 5L), den_edges = integer(0),
    num = 17L, den = 1000L, num_caption = "incydenty (obie drogi)", den_caption = "wszystkie zmiany"
  ),
  b = list(
    label = "P(B) — przegrzanie wśród wszystkich zmian",
    tex = "P(B)", num_nodes = 2L, den_nodes = 1L, num_edges = 1L, den_edges = integer(0),
    num = 100L, den = 1000L, num_caption = "zmiany z przegrzaniem", den_caption = "wszystkie zmiany"
  ),
  ab = list(
    label = "P(A ∩ B) — incydent i przegrzanie naraz",
    tex = "P(A\\cap B)", num_nodes = 4L, den_nodes = 1L, num_edges = c(1L, 3L), den_edges = integer(0),
    num = 12L, den = 1000L, num_caption = "incydent i przegrzanie", den_caption = "wszystkie zmiany"
  ),
  a_b = list(
    label = "P(A | B) — incydent wśród zmian z przegrzaniem",
    tex = "P(A\\mid B)", num_nodes = 4L, den_nodes = c(2L, 5L), num_edges = 3L, den_edges = c(1L, 4L),
    num = 12L, den = 100L, num_caption = "incydent i przegrzanie", den_caption = "zmiany z przegrzaniem"
  ),
  b_a = list(
    label = "P(B | A) — przegrzanie wśród zmian z incydentem",
    tex = "P(B\\mid A)", num_nodes = 4L, den_nodes = 6L, num_edges = c(1L, 3L), den_edges = c(2L, 5L),
    num = 12L, den = 17L, num_caption = "incydent i przegrzanie", den_caption = "wszystkie incydenty"
  )
)

warunki_views_widget <- figure_panel(
  label = "Trzy widoki", title = "Te same liczby: tabela, drzewo i udziały",
  zoom_plot_ui("w2_views_tree", height = "380px"),
  fluidRow(
    column(5, tags$div(class = "lc-table-wrap", tableOutput("w2_table"))),
    column(7, zoom_plot_ui("w2_views_shares", height = "320px"))
  ),
  lc_feedback(type = "info", "Zmiana widoku nie zmienia zdarzenia ani mianownika."),
  full_width = TRUE
)

warunki_tree_read_widget <- figure_panel(
  label = "Czytaj od mianownika", title = "Licznik i mianownik na drzewie",
  radioButtons(
    "w2_target", "Zaznacz na drzewie",
    choices = setNames(names(warunki_views_targets), vapply(warunki_views_targets, `[[`, "", "label")),
    selected = "a"
  ),
  zoom_plot_ui("w2_target_plot", height = "440px"),
  uiOutput("w2_target_result"),
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
        list(id = "sens", title = "Trzy podobne pytania", text = c(
          "O incydent w Bananpolu można zapytać na trzy sposoby, które brzmią niemal tak samo, a odpowiadają na zupełnie różne pytania. Można pytać, jak często incydent zdarza się w ogóle — wtedy liczymy go wśród wszystkich zmian. Można pytać, jak często zdarza się po wykryciu przegrzania — wtedy liczymy go wyłącznie wśród zmian, na których czujnik zgłosił przegrzanie. Można wreszcie pytać, jak często przegrzanie poprzedzało incydent — wtedy patrzymy wyłącznie na zmiany, na których incydent rzeczywiście zaszedł.",
          "Różnica między tymi pytaniami nie leży w zdarzeniu, o które pytamy, lecz w grupie przypadków, spośród których liczymy. Warunek „po wykryciu przegrzania” zawęża tę grupę: odrzucamy zmiany bez przegrzania i dopiero w tym, co zostało, sprawdzamy, jak często doszło do incydentu. To samo zdarzenie, inny mianownik — i inna liczba.",
          "Pomyłka między tymi pytaniami nie jest błędem rachunkowym, tylko błędem pytania. Dyrektor, który słyszy „12% zmian z przegrzaniem kończy się incydentem”, a zapamiętuje „12% wszystkich zmian kończy się incydentem”, zawyża problem siedmiokrotnie — mimo że nikt nie policzył niczego źle.",
          "Dlatego każde pytanie o częstość formułujemy pełnym zdaniem, które nazywa zarówno zdarzenie, jak i grupę odniesienia:"
        ), bullets = c(
          "Jak często dochodzi do incydentu? — liczymy wśród wszystkich porównywalnych zmian.",
          "Jak często dochodzi do incydentu po wykryciu przegrzania? — liczymy tylko wśród zmian z przegrzaniem.",
          "W ilu incydentach wcześniej wykryto przegrzanie? — liczymy tylko wśród zmian, na których zaszedł incydent."
        ))
      ),
      pitfall = "Częstość incydentu wśród zmian z przegrzaniem i częstość przegrzania wśród zmian z incydentem to dwie różne liczby — zwykle nie są równe."
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
          tags$p("Tę wielkość nazywamy prawdopodobieństwem warunkowym, a zapis
                 P(A | B) czytamy: prawdopodobieństwo A pod warunkiem B.")
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
      id = "reprezentacje", title = "Jedna sytuacja, trzy widoki", lead = "Tabela, drzewo dróg i udziały są różnymi mapami tych samych liczebności, a drzewo podpowiada regułę mnożenia.",
      intro = c(
        "Sposób prezentacji powinien ułatwiać odpowiedź, a nie zmieniać problem. Tabela dobrze pilnuje liczebności, drzewo pokazuje kolejność warunków, a słupki pomagają porównać częstości w grupach.",
        "W praktyce inspektora wybór widoku to wybór narzędzia komunikacji: tabela przekonuje audytora, który chce sprawdzić sumy, drzewo tłumaczy mechanizm zarządowi, a wykres udziałów najlepiej pokazuje kontrast między grupami na slajdzie. Umiejętność przejścia między nimi bez zmiany liczb jest testem zrozumienia. Poniżej wszystkie trzy widoki tych samych 1000 zmian Bananpolu obok siebie — sprawdź, czy w każdym znajdujesz te same liczebności."
      ),
      widget = tagList(
        warunki_views_widget,
        lc_p(
          "W każdym widoku liczba incydentów i liczebność grup są takie same.
           Jeśli wynik zmienia się wraz z rodzajem wykresu, zmieniliśmy definicję
           albo mianownik, a nie tylko sposób prezentacji."
        ),
        lc_h2("warunki-reprezentacje-iloczyn", "Mnożymy wzdłuż drogi"),
        lc_p(
          "Drzewo pokazuje coś więcej niż tylko liczebności. Na gałęziach stoją
           prawdopodobieństwa, a na końcach liczby zmian — i między jednymi a drugimi
           jest prosty związek. Żeby dojść do liścia „Incydent” w górnej części
           drzewa, trzeba przejść dwie gałęzie: najpierw trafić do grupy zmian
           z przegrzaniem, a potem, już wewnątrz tej grupy, trafić na incydent.
           Iloczyn nie pojawia się więc jako sztuczka algebraiczna — odpowiada
           przejściu przez dwa kolejne filtry."
        ),
        lc_p(
          "Pierwszy czynnik odnosi się do wszystkich zmian: przegrzanie dotyczy
           10% z 1000, czyli 100 zmian. Drugi czynnik odnosi się już tylko do tej
           setki: incydent występuje w 12% z nich, czyli w 12 zmianach. Te 12 zmian
           to 1,2% całej obserwowanej populacji — i dokładnie tyle daje pomnożenie
           0,10 przez 0,12. Prześledź tę drogę na liczbach poniżej."
        ),
        warunki_path_widget,
        lc_p(
          "Ten rachunek nie korzystał z niczego szczególnego w liczbach 0,10
           i 0,12 — działa dla dowolnych wartości, więc uogólniamy go w jedną
           regułę:"
        ),
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
        lc_p(
          "Wróćmy do drzewa z 1000 zmian Bananpolu. Wybierz prawdopodobieństwo,
           a drzewo pokaże, które węzły tworzą licznik, a które mianownik. Zacznij
           od P(A): incydent może powstać na dwóch rozłącznych drogach, więc jego
           licznik to suma dwóch liści. Przy prawdopodobieństwach warunkowych
           licznik leży wewnątrz mianownika — te same zmiany liczymy raz na górze
           i raz na dole ułamka."
        ),
        warunki_tree_read_widget,
        lc_p(
          "Drzewo pokazuje ważoną sumę w konkretnych zmianach: 12 incydentów
           z gałęzi przegrzania i 5 z gałęzi bez przegrzania. Suwaki poniżej
           robią to samo w ułamkach, dla dwóch trybów pracy."
        ),
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
    # Indeksowanie zamiast sample(x, 1): dla jednoelementowego x sample() losowałoby z 1:x.
    monty$opened <- possible_zonks[sample.int(length(possible_zonks), 1L)]
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
    monty_sim$n <- 0L
    monty_sim$wins_stay <- 0L
    monty_sim$wins_switch <- 0L
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

  monty_sim <- reactiveValues(n = 0L, wins_stay = 0L, wins_switch = 0L)

  output$w2_monty_simulation_panel <- renderUI({
    if (is.null(monty$final)) {
      return(NULL)
    }
    tagList(
      tags$div(class = "lc-eyebrow", "Eksperyment wielokrotny"),
      tags$h4("Czy wynik jednej gry był przypadkiem?"),
      tags$p("Dograj kolejne partie obiema strategiami naraz. Wyniki się sumują, więc zobacz, jak odsetek wygranych stabilizuje się wraz z liczbą gier."),
      fluidRow(
        column(3, actionButton("w2_monty_sim_1", "+1 gra", class = "lc-btn-primary", width = "100%")),
        column(3, actionButton("w2_monty_sim_10", "+10 gier", class = "lc-btn-primary", width = "100%")),
        column(3, actionButton("w2_monty_sim_100", "+100 gier", class = "lc-btn-primary", width = "100%")),
        column(3, actionButton("w2_monty_sim_1000", "+1000 gier", class = "lc-btn-primary", width = "100%"))
      ),
      zoom_plot_ui("w2_monty_plot", height = "390px")
    )
  })

  add_monty_games <- function(n) {
    req(monty$final)
    prizes <- sample.int(3L, n, replace = TRUE)
    choices <- sample.int(3L, n, replace = TRUE)
    monty_sim$n <- monty_sim$n + n
    monty_sim$wins_stay <- monty_sim$wins_stay + sum(prizes == choices)
    monty_sim$wins_switch <- monty_sim$wins_switch + sum(prizes != choices)
  }

  observeEvent(input$w2_monty_sim_1, add_monty_games(1L))
  observeEvent(input$w2_monty_sim_10, add_monty_games(10L))
  observeEvent(input$w2_monty_sim_100, add_monty_games(100L))
  observeEvent(input$w2_monty_sim_1000, add_monty_games(1000L))

  monty_plot <- reactive({
    n <- monty_sim$n
    if (n == 0L) {
      return(
        ggplot() +
          annotate("text", x = 1, y = 0.55, label = "Dograj partie przyciskami powyżej", colour = upwr_secondary, size = 5) +
          coord_cartesian(xlim = c(0, 2), ylim = c(0, 1)) +
          labs(title = "Która strategia wygrywa częściej?", x = NULL, y = "Odsetek wygranych") +
          theme_upwr() +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      )
    }
    results <- data.frame(
      strategy = c("Zostaję", "Zmieniam"),
      wins = c(monty_sim$wins_stay, monty_sim$wins_switch)
    )
    results$win_rate <- results$wins / n
    results$label <- sprintf(
      "%s\n(%d z %d)", scales::percent(results$win_rate, accuracy = 0.1), results$wins, n
    )
    subtitle <- if (n < 30L) {
      "Przy kilku grach przypadek jeszcze rządzi — dograj więcej"
    } else {
      "Linie kropkowane: teoretyczne 1/3 i 2/3"
    }
    ggplot(results, aes(strategy, win_rate, fill = strategy)) +
      geom_col(width = 0.62) +
      geom_text(aes(label = label), vjust = -0.35, fontface = "bold", lineheight = 0.9) +
      geom_hline(yintercept = c(1 / 3, 2 / 3), colour = upwr_reference, linetype = "dotted", linewidth = 0.6) +
      scale_fill_manual(values = c("Zostaję" = upwr_reference, "Zmieniam" = upwr_accent), guide = "none") +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1.18), breaks = seq(0, 1, 0.25)) +
      labs(
        title = sprintf("Wyniki po %d %s", n, if (n == 1L) "grze" else "grach"),
        subtitle = subtitle,
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

  # Siatka ilustracyjna: 500 zmian w 20 kolumnach, żeby wykres był pionowy i czytelny.
  dot_counts <- reactive(risk_conditional_counts(
    500L, input$w2_share, input$w2_risk_hot, input$w2_risk_normal
  ))
  filter_plot <- reactive({
    d <- dot_counts()
    n_cols <- 20L
    # Kolejność: najpierw blok zmian z przegrzaniem (filtr), w każdym bloku incydenty na początku.
    groups <- c(
      rep("Przegrzanie", d$event[1] + d$no_event[1]),
      rep("Brak przegrzania", d$event[2] + d$no_event[2])
    )
    statuses <- c(
      rep("Incydent", d$event[1]), rep("Brak incydentu", d$no_event[1]),
      rep("Incydent", d$event[2]), rep("Brak incydentu", d$no_event[2])
    )
    grid <- data.frame(id = seq_along(groups), status = statuses, group = groups)
    grid$x <- (grid$id - 1L) %% n_cols + 1L
    grid$y <- (grid$id - 1L) %/% n_cols + 1L
    ggplot(grid, aes(x, y, shape = group, fill = status)) +
      geom_point(size = 2.6, colour = upwr_secondary, stroke = 0.6) +
      scale_y_reverse() +
      coord_equal() +
      scale_shape_manual(values = c("Przegrzanie" = 24, "Brak przegrzania" = 21)) +
      scale_fill_manual(values = c("Incydent" = upwr_secondary, "Brak incydentu" = "white")) +
      guides(
        shape = guide_legend(order = 1, override.aes = list(fill = "white")),
        fill = guide_legend(order = 2, override.aes = list(shape = 21))
      ) +
      labs(title = "500 porównywalnych zmian", x = NULL, y = NULL, shape = "Warunek", fill = "Wynik") +
      theme_upwr() +
      theme(
        axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
      )
  })
  zoom_plot_server("w2_filter_plot", filter_plot,
    alt = "Siatka 500 zmian: trójkąty to zmiany z przegrzaniem, kółka bez; wypełnione znaki to incydenty."
  )
  output$w2_filter_stats <- renderUI({
    d <- dot_counts()
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
      d <- warunki_views_counts
      data.frame(
        Grupa = c(d$condition, "Razem"),
        Incydent = c(d$event, sum(d$event)),
        `Brak incydentu` = c(d$no_event, sum(d$no_event)),
        Razem = c(d$total, sum(d$total)),
        check.names = FALSE
      )
    },
    striped = TRUE,
    bordered = TRUE,
    digits = 0
  )
  output$w2_target_result <- renderUI({
    tg <- warunki_views_targets[[input$w2_target %||% "a"]]
    value <- risk_format_probability(tg$num / tg$den)
    tagList(
      lc_stat_grid(
        lc_stat_box("Licznik", format(tg$num), caption = tg$num_caption, color = upwr_accent),
        lc_stat_box("Mianownik", format(tg$den), caption = tg$den_caption, color = upwr_cat[["bursztyn"]]),
        lc_stat_box("Wynik", value, caption = tg$label, color = upwr_secondary),
        columns = 3
      ),
      lc_formula_box(withMathJax(sprintf(
        "$$%s=\\frac{%d}{%d}=%s$$", tg$tex, tg$num, tg$den, gsub("\\.", "{,}", sprintf("%.3f", tg$num / tg$den))
      ))),
      if (tg$den < 1000L) lc_p(
        "Licznik jest częścią mianownika: ", format(tg$num), " zmian z licznika należy jednocześnie do bursztynowej grupy w mianowniku. Ich węzeł jest bordowy, bo liczymy je dwa razy — raz na górze, raz na dole ułamka."
      )
    )
  })
  # Drzewo dróg; target = NULL rysuje drzewo bez podświetleń.
  tree_plot <- function(target = NULL) {
    d <- warunki_views_counts
    prm <- warunki_views_params
    fmt <- function(p) gsub("\\.", ",", sprintf("%.3f", p))
    amber <- unname(upwr_cat[["bursztyn"]])
    num_nodes <- target$num_nodes %||% integer(0)
    den_nodes <- target$den_nodes %||% integer(0)
    num_edges <- target$num_edges %||% integer(0)
    den_edges <- target$den_edges %||% integer(0)
    nodes <- data.frame(
      x = c(0, 5, 5, 10.5, 10.5, 10.5, 10.5),
      y = c(0, 1.6, -1.6, 2.5, .7, -.7, -2.5),
      label = c(
        "1000 zmian", paste0("Przegrzanie: ", d$total[1]), paste0("Brak przegrzania: ", d$total[2]),
        paste0("Incydent: ", d$event[1]), paste0("Brak: ", d$no_event[1]),
        paste0("Incydent: ", d$event[2]), paste0("Brak: ", d$no_event[2])
      )
    )
    node_idx <- seq_len(nrow(nodes))
    nodes$fill <- ifelse(node_idx %in% num_nodes, upwr_accent,
      ifelse(node_idx %in% den_nodes, amber, upwr_secondary))
    edges <- data.frame(
      xs = c(0, 0, 5, 5, 5, 5),
      ys = c(0, 0, 1.6, 1.6, -1.6, -1.6),
      xe = c(5, 5, 10.5, 10.5, 10.5, 10.5),
      ye = c(1.6, -1.6, 2.5, .7, -.7, -2.5),
      p = c(
        fmt(prm[["share"]]), fmt(1 - prm[["share"]]),
        fmt(prm[["hot"]]), fmt(1 - prm[["hot"]]),
        fmt(prm[["normal"]]), fmt(1 - prm[["normal"]])
      )
    )
    edge_idx <- seq_len(nrow(edges))
    edges$colour <- ifelse(edge_idx %in% num_edges, upwr_accent,
      ifelse(edge_idx %in% den_edges, amber, upwr_reference))
    edges$width <- ifelse(edge_idx %in% c(num_edges, den_edges), 1.8, .8)
    ggplot() +
      geom_segment(data = edges, aes(x = xs, y = ys, xend = xe, yend = ye, colour = colour, linewidth = width)) +
      scale_colour_identity() +
      scale_linewidth_identity() +
      geom_label(
        data = edges, aes((xs + xe) / 2, (ys + ye) / 2, label = p),
        size = 5, colour = upwr_secondary, fontface = "bold", linewidth = 0,
        label.padding = unit(0.3, "lines")
      ) +
      geom_label(
        data = nodes, aes(x, y, label = label, fill = fill),
        size = 5.6, colour = "white", fontface = "bold", linewidth = 0,
        label.padding = unit(0.5, "lines"), label.r = unit(0.25, "lines")
      ) +
      scale_fill_identity() +
      coord_cartesian(xlim = c(-1.3, 12), ylim = c(-3.1, 3.1)) +
      labs(title = "Drzewo dróg: mnożymy wzdłuż gałęzi", subtitle = "Prawdopodobieństwa na gałęziach, liczebności w węzłach", x = NULL, y = NULL) +
      theme_upwr() +
      theme(
        axis.text = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  }
  shares_plot <- function() {
    d <- warunki_views_counts
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
  zoom_plot_server("w2_views_tree", reactive(tree_plot()),
    alt = "Drzewo dróg z prawdopodobieństwami na gałęziach i liczebnościami w węzłach."
  )
  zoom_plot_server("w2_views_shares", reactive(shares_plot()),
    alt = "Słupki udziału incydentów wśród zmian z przegrzaniem i bez przegrzania."
  )
  zoom_plot_server("w2_target_plot",
    reactive(tree_plot(warunki_views_targets[[input$w2_target %||% "a"]])),
    alt = "Drzewo dróg z podświetlonym licznikiem (bordowy) i mianownikiem (bursztynowy) wybranego prawdopodobieństwa."
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
