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
warunki_exercises <- list(
  list(
    task = "Bananpol: policz P(incydent) z dwóch trybów pracy i zapisz wynik jako częstość na 1000 zmian. Przyjmij udział pracy w przeciążeniu 0,20, P(incydent | przeciążenie) = 0,15 i P(incydent | normalna praca) = 0,01.",
    answer = c(
      "Tryby „przeciążenie” i „normalna praca” tworzą układ zupełny, więc stosujemy wzór (2.4): P(incydent) = 0,20 · 0,15 + 0,80 · 0,01 = 0,030 + 0,008 = 0,038.",
      "W naturalnych częstościach: około 38 incydentów na 1000 zmian, z czego 30 na 200 zmianach w przeciążeniu i 8 na 800 zmianach normalnych."
    )
  ),
  list(
    task = "Diagnostyka: wskaż, dlaczego wspólne zasilanie narusza założenie niezależności dwóch zabezpieczeń.",
    answer = c(
      "Utrata wspólnego zasilania wyłącza oba zabezpieczenia naraz. Informacja, że jedno z nich nie zadziałało, zwiększa więc prawdopodobieństwo, że zasilanie padło, a tym samym — że nie zadziałało również drugie. P(awaria 2 | awaria 1) > P(awaria 2), co z definicji 2.3 oznacza zależność.",
      "Liczbowo, ze wzoru (2.7) przy q = 0,05 i c = 0,01: P(obie) = 0,01 + 0,99 · 0,0025 ≈ 0,0125, czyli mniej więcej pięć razy więcej niż 0,0025 z iloczynu (2.6)."
    )
  ),
  list(
    task = "Transfer: opisz warunek i właściwy mianownik dla ryzyka wypadku podczas pracy nocnej.",
    answer = c(
      "Zdarzenie A: wypadek przy pracy podczas zmiany. Warunek B: zmiana nocna. P(A | B) liczymy wśród wszystkich zmian nocnych w ustalonym okresie (albo wśród przepracowanych godzin nocnych, jeśli zmiany mają różną długość), a nie wśród wszystkich zmian ani wśród wszystkich wypadków.",
      "Udział wypadków nocnych wśród wszystkich wypadków to P(B | A) — inna liczba, która zależy także od tego, ile pracy w ogóle wykonuje się nocą."
    )
  ),
  list(
    task = "Odwrócenie warunku: w sytuacji z ćwiczenia 1 doszło do incydentu. Jakie jest prawdopodobieństwo, że zmiana przebiegała w trybie przeciążenia? Porównaj wynik z udziałem przeciążenia wśród wszystkich zmian.",
    answer = c(
      "Ze wzoru Bayesa (2.5): P(przeciążenie | incydent) = 0,20 · 0,15 / 0,038 = 0,030 / 0,038 ≈ 0,789.",
      "Przeciążenie to tylko 20% zmian, ale około 79% incydentów. Informacja o incydencie prawie czterokrotnie podnosi ocenę, że zmiana była przeciążona."
    )
  ),
  list(
    task = "Test niezależności z tabeli: w 1000 zmian było 400 zmian nocnych i 20 wypadków, z czego 10 nocą. Czy wypadek i zmiana nocna są niezależne?",
    answer = c(
      "P(W) = 20/1000 = 0,020, P(N) = 400/1000 = 0,40, P(W ∩ N) = 10/1000 = 0,010. Iloczyn P(W) · P(N) = 0,008 ≠ 0,010, więc zdarzenia są zależne.",
      "Równoważnie: P(W | N) = 10/400 = 0,025 > 0,020 = P(W), a P(N | W) = 10/20 = 0,50 > 0,40 = P(N). Związek jest słaby, a przy 20 wypadkach może być przypadkowy — rachunek na próbie nie zastępuje oceny niepewności."
    )
  )
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
  ratio = "1.6/1", max_height = "560px"
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
    column(5, uiOutput("w2_table")),
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
  risk_annotated_formula(
    items = list(
      list(
        symbol = "P(A ∩ B)", value = "0,012", color = upwr_accent,
        note = "12 na 1000 zmian. Mianownik wraca do wszystkich zmian."
      ),
      list(
        symbol = "P(B)", value = "0,10", color = upwr_cat[["bursztyn"]],
        note = "100 na 1000 zmian: przegrzanie."
      ),
      list(
        symbol = "P(A | B)", value = "0,12", color = upwr_cat[["niebo"]],
        note = "12 na 100 zmian z przegrzaniem. Mianownik to 100."
      )
    ),
    ops = c("=", "×")
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
    tags$p("Wzór (2.1). Warunek filtruje mianownik: liczymy A wyłącznie wśród przypadków, w których zaszło B.")
  ),
  lc_formula_box(
    withMathJax("$$P(A\\cap B)=P(B)\\,P(A\\mid B)$$"),
    tags$p("Wzór (2.2). Wspólną drogę mnożymy etapami: najpierw wejście do grupy B, potem A wewnątrz tej grupy.")
  ),
  lc_formula_box(
    withMathJax("$$P(A)=\\sum_i P(B_i)\\,P(A\\mid B_i)$$"),
    tags$p("Wzór (2.4). Wynik ogólny jest ważoną sumą rozłącznych dróg — wagi są udziałami trybów pracy.")
  ),
  risk_assessment_ui("w2", warunki_quiz, warunki_exercises)
)

warunki_block <- list(
  id = "warunki", title = "Warunki zmieniają ocenę",
  chapters = list(
    list(
      id = "pytanie", title = "Populacja odniesienia", hook = "Ten sam incydent, trzy różne liczby", lead = "Zanim policzymy, nazywamy warunek i populację odniesienia.",
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
        )),
        list(
          id = "zapis", title = "Zapis, który pilnuje mianownika",
          body = list(
            c(
              "Pełne zdania są dokładne, ale długie. Rachunek prawdopodobieństwa ma dla nich skrócony zapis, który zachowuje całą informację o mianowniku. Niech A oznacza zdarzenie „na zmianie doszło do incydentu”, a B — „czujnik wykrył przegrzanie”. Wtedy P(A) to prawdopodobieństwo incydentu liczone wśród wszystkich zmian, P(A ∩ B) — prawdopodobieństwo, że na zmianie zaszły oba zdarzenia naraz, także liczone wśród wszystkich zmian, a P(A | B) — prawdopodobieństwo incydentu liczone wyłącznie wśród zmian z przegrzaniem.",
              "Pionowa kreska w zapisie P(A | B) jest najważniejszym znakiem tego wykładu. To, co stoi na prawo od niej, opisuje grupę odniesienia; to, co stoi na lewo — zdarzenie, którego udział w tej grupie liczymy. Zamiana stron kreski zamienia pytanie: P(B | A) to udział zmian z przegrzaniem wśród zmian z incydentem. Symbol ∩ oznacza natomiast „oba naraz” i niczego nie filtruje, dlatego P(A ∩ B) nigdy nie przekracza ani P(A), ani P(B).",
              "W Bananpolu zarejestrowano 1000 porównywalnych zmian. Przegrzanie wykryto na 100 z nich, incydentów było łącznie 17, a 12 z nich wystąpiło na zmianach z przegrzaniem. Te liczby wystarczą, by odpowiedzieć na wszystkie trzy pytania z poprzedniej sekcji — pod warunkiem, że do każdego dobierzemy właściwy mianownik."
            ),
            risk_example("2.1", "Trzy pytania, trzy mianowniki",
              problem = "Na podstawie danych Bananpolu (1000 zmian, 100 z przegrzaniem, 17 incydentów, w tym 12 na zmianach z przegrzaniem) oblicz P(A), P(A ∩ B), P(A | B) i P(B | A). Przy każdej liczbie zapisz, spośród jakich zmian ją liczysz.",
              steps = c(
                "P(A): incydenty wśród wszystkich zmian, 17/1000 = 0,017.",
                "P(A ∩ B): zmiany z incydentem i przegrzaniem wśród wszystkich zmian, 12/1000 = 0,012.",
                "P(A | B): incydenty wśród 100 zmian z przegrzaniem, 12/100 = 0,12.",
                "P(B | A): przegrzania wśród 17 zmian z incydentem, 12/17 ≈ 0,706."
              ),
              answer = "Licznik 12 pojawia się w trzech ostatnich wynikach, ale za każdym razem dzielimy go przez inny mianownik: 1000, 100 albo 17. Stąd trzy różne liczby: 0,012, 0,12 i około 0,71. Żadna z nich nie jest „prawdziwszym” ryzykiem — każda odpowiada na inne pytanie."
            ),
            "Przykład 2.1 pokazuje, jak łatwo o pomyłkę w raporcie. Zdanie „siedem na dziesięć incydentów poprzedziło przegrzanie” dotyczy P(B | A) ≈ 0,71 i jest prawdziwe. Zdanie „siedem na dziesięć przegrzań kończy się incydentem” dotyczyłoby P(A | B) i jest fałszywe — w rzeczywistości incydentem kończy się 12 na 100 przegrzań. Obie wersje brzmią podobnie; różnią się tylko stroną kreski. W kolejnych rozdziałach nadamy temu zapisowi formalną definicję i nauczymy się przechodzić od jednej liczby do drugiej.",
            risk_check("w2_chk_zapis",
              "Kierownik zmiany pisze: „Na zmianach z incydentem przegrzanie wykryto w 12 przypadkach na 17”. Który zapis opisuje tę liczbę?",
              c("P(A | B)" = "ab", "P(B | A)" = "ba", "P(A ∩ B)" = "and"),
              correct = "ba",
              explanation = "Grupą odniesienia są zmiany z incydentem (A), a liczymy w niej udział przegrzań (B). To P(B | A) = 12/17 ≈ 0,71.",
              hints = c(ab = "P(A | B) liczy incydenty wśród zmian z przegrzaniem — mianownikiem byłoby 100, nie 17.", and = "P(A ∩ B) ma mianownik 1000 wszystkich zmian. Tu mianownikiem jest 17.")
            )
          )
        )
      ),
      pitfall = "Częstość incydentu wśród zmian z przegrzaniem i częstość przegrzania wśród zmian z incydentem to dwie różne liczby — zwykle nie są równe."
    ),
    list(
      id = "filtr", title = "Prawdopodobieństwo warunkowe", hook = "Jedna otwarta bramka zmienia wszystko", lead = "Zaczynamy w studiu teleturnieju: jedna odsłonięta bramka zmienia całą ocenę.",
      intro = c(
        "Zanim wrócimy do hali Bananpolu, przenieśmy się do studia „Idź na całość”. Przed Tobą trzy bramki: za jedną nagroda, za dwiema Zonk. Wybierasz jedną. Prowadzący — który wie, gdzie stoi nagroda — otwiera jedną z pozostałych bramek i pokazuje Zonka. I pada pytanie, od którego zaczęły się dekady sporów: zostajesz przy swojej bramce czy zmieniasz?",
        "Zagraj kilka rund, zanim przeczytasz cokolwiek dalej, i uruchom symulację tysiąca gier. Po drodze zapisz w głowie odpowiedź na jedno pytanie: czy ruch prowadzącego czegoś Cię nauczył, czy niczego nie zmienił?"
      ),
      body = list(
        risk_try("wybierz bramkę, zdecyduj, czy zostajesz, czy zmieniasz, i sprawdź wynik. Po pierwszej grze dograj „+1000 gier” i zapisz odsetek wygranych przy pozostaniu i przy zmianie."),
        warunki_monty_widget
      ),
      sections = list(
        list(
          id = "lekcja", title = "Co właściwie zrobił prowadzący?",
          body = list(
            c(
              "Symulacja jest bezlitosna dla intuicji „50 na 50”: zmiana bramki wygrywa mniej więcej dwa razy częściej. Żeby zobaczyć dlaczego, policz światy, w których możesz się znaleźć. W dwóch grach na trzy Twój pierwszy wybór trafia w Zonka — i wtedy prowadzący nie ma żadnej swobody: musi odsłonić jedynego pozostałego Zonka, więc nagroda stoi za bramką, na którą się przełączysz. Tylko w jednej grze na trzy pierwszy strzał trafia w nagrodę i zmiana przegrywa.",
              "Kluczem nie jest samo otwarcie bramki, lecz to, że ruch prowadzącego zależy od tego, co jest ukryte. Jego gest odfiltrowuje część możliwych światów: po odsłonięciu Zonka za bramką 1 zostają tylko te scenariusze, które są zgodne z tym, co widzisz. Prawdopodobieństwa liczone w tym przefiltrowanym świecie różnią się od tych sprzed filtracji — i właśnie ta operacja dostanie za chwilę nazwę i wzór.",
              "Filtrowanie da się przeprowadzić dosłownie, na liczbach gier. Zamiast pytać o prawdopodobieństwa, wyobraźmy sobie wiele rozegranych partii i zapytajmy, ile z nich wygląda dokładnie tak, jak to, co widzimy w studiu."
            ),
            risk_example("2.2", "Trzysta gier w studiu",
              problem = "Rozegrano 300 gier. W każdej wybierasz bramkę 2, a nagroda stoi z jednakowym prawdopodobieństwem za każdą bramką. Prowadzący zawsze odsłania Zonka spośród bramek, których nie wybrałeś; gdy ma dwie możliwości, rzuca monetą. W ilu grach prowadzący odsłoni bramkę 1 i w ilu z nich zmiana na bramkę 3 da nagrodę?",
              steps = c(
                "Nagroda za bramką 1 (100 gier): prowadzący nie może otworzyć ani bramki 1, ani Twojej bramki 2, więc otwiera bramkę 3. Bramka 1 zostaje otwarta w 0 z tych gier.",
                "Nagroda za bramką 2 (100 gier): prowadzący wybiera losowo między bramkami 1 i 3 — bramkę 1 otwiera w około 50 grach.",
                "Nagroda za bramką 3 (100 gier): prowadzący musi otworzyć bramkę 1 — we wszystkich 100 grach.",
                "Filtr „prowadzący otworzył bramkę 1” zostawia 0 + 50 + 100 = 150 gier. Nagroda stoi za bramką 3 w 100 z nich, za Twoją bramką 2 — w 50."
              ),
              answer = "Po odsłonięciu bramki 1 zmiana wygrywa w 100 grach na 150, czyli z prawdopodobieństwem 2/3; pozostanie — w 50 na 150, czyli 1/3. Nowym mianownikiem jest 150 gier zgodnych z obserwacją, a nie 300 wszystkich."
            ),
            "Zauważ, co się stało z mianownikiem. Przed ruchem prowadzącego każda z 300 gier była możliwa. Po jego ruchu połowę gier odrzucamy jako niezgodne z tym, co widzimy, a pozostałe 150 staje się nowym „całym światem”. Nierówność 100 do 50 bierze się stąd, że gdy nagroda stoi za bramką 3, prowadzący musi otworzyć bramkę 1, a gdy stoi za Twoją bramką — robi to tylko w połowie przypadków. Wiedza prowadzącego przenosi informację na jego gest.",
            risk_check("w2_chk_monty",
              "Zmieńmy zasady: prowadzący nie wie, gdzie jest nagroda, i otwiera losowo jedną z dwóch pozostałych bramek. Tym razem przypadkiem pokazał Zonka. Jakie jest teraz prawdopodobieństwo wygranej po zmianie?",
              c("2/3, tak jak poprzednio" = "two_thirds", "1/2" = "half", "1/3" = "third"),
              correct = "half",
              explanation = "W 300 grach z wyborem bramki 2 niewiedzący prowadzący otwiera bramkę 1 w około 150 grach, ale w 50 z nich odsłania nagrodę. Filtr „otworzył bramkę 1 i był tam Zonk” zostawia 100 gier: 50 z nagrodą za bramką 2 i 50 za bramką 3. Gest przestał zależeć od położenia nagrody, więc nie niesie przewagi dla zmiany.",
              hints = c(two_thirds = "Policz jak w przykładzie 2.2, ale pamiętaj, że niewiedzący prowadzący czasem odsłoniłby nagrodę — te gry odpadają przy filtrze.", third = "1/3 to szansa pozostania przy wiedzącym prowadzącym. Co się zmienia, gdy jego ruch nie zależy od położenia nagrody?")
            )
          )
        ),
        list(
          id = "definicja", title = "Od opowieści do definicji",
          body = list(
            "Poznanie warunku nie zmienia tego, co się wydarzyło — w studiu ani w zakładzie. Zmienia zbiór przypadków, do którego odnosimy licznik. Prawdopodobieństwo zdarzenia A pod warunkiem B to udział A liczony wyłącznie wśród przypadków, w których zaszło B: filtrujemy mianownik, a potem liczymy jak zwykle.",
            risk_definition("2.1", "Prawdopodobieństwo warunkowe", c(
              "Niech A i B będą zdarzeniami i niech P(B) > 0. Prawdopodobieństwem warunkowym zdarzenia A pod warunkiem B nazywamy liczbę P(A | B) określoną wzorem (2.1).",
              "Zdarzenie B nazywamy warunkiem. Dla warunku o zerowym prawdopodobieństwie wzór (2.1) nie ma sensu — nie można filtrować do pustej grupy przypadków."
            )),
            risk_formula("P(A\\mid B)=\\frac{P(A\\cap B)}{P(B)},\\qquad P(B)>0", num = "2.1",
              legend = c("P(A\\cap B)" = "prawdopodobieństwo, że zaszły oba zdarzenia naraz", "P(B)" = "prawdopodobieństwo warunku, czyli wielkość przefiltrowanej grupy", "P(A\\mid B)" = "udział A wewnątrz tej grupy")),
            "Tę wielkość nazywamy prawdopodobieństwem warunkowym, a zapis P(A | B) czytamy: prawdopodobieństwo A pod warunkiem B.",
            risk_derivation("dlaczego iloraz?", c(
              "Wzór (2.1) jest wprost przepisaniem liczenia na przypadkach. Jeśli w n porównywalnych przypadkach warunek B zaszedł n_B razy, a oba zdarzenia naraz — n_AB razy, to udział A wśród przypadków z B wynosi n_AB / n_B.",
              "Dzieląc licznik i mianownik przez n, nie zmieniamy ilorazu, a dostajemy częstości odniesione do całej populacji:"
            ), lines = c("n_AB / n_B = (n_AB / n) / (n_B / n)", "           ≈ P(A ∩ B) / P(B)", "", "Bananpol: 12 / 100 = (12/1000) / (100/1000) = 0,012 / 0,10 = 0,12")),
            "W tym języku gest prowadzącego jest warunkiem B. Załóżmy, jak w przykładzie 2.2, że wybrałeś bramkę 2, a B oznacza: „za bramką 1 jest Zonk, a odsłonił ją prowadzący znający układ”. Pytanie o zmianę bramki to pytanie o P(nagroda za bramką 3 | B) — i rachunek na przefiltrowanych światach daje 2/3, dokładnie tyle, ile pokazała symulacja. We wzorze (2.1): P(B) = 150/300 = 1/2, P(nagroda za 3 ∩ B) = 100/300 = 1/3, a iloraz to (1/3) / (1/2) = 2/3.",
            "Prawdopodobieństwo warunkowe zachowuje się jak zwykłe prawdopodobieństwo, tylko w mniejszym świecie. W szczególności P(B | B) = 1, bo w przefiltrowanej grupie warunek zachodzi zawsze, a P(nie A | B) = 1 − P(A | B): w grupie 100 zmian z przegrzaniem 12 kończy się incydentem, więc 88 — bez incydentu. Z tej własności korzystają gałęzie drzewa, które w kolejnym rozdziale zawsze sumują się do jedynki na każdym rozwidleniu."
          )
        ),
        list(
          id = "mianownik", title = "Naturalne częstości w Bananpolu",
          body = list(
            c(
              "Wracamy do hali. Wykryte przegrzanie robi z tysiącem zmian dokładnie to, co prowadzący z bramkami: filtruje świat. Najpierw dzielimy 1000 zmian na te z przegrzaniem i bez niego, a dopiero potem zliczamy incydenty. Jeśli 100 zmian spełnia B, to mianownikiem P(A | B) jest 100, a nie 1000.",
              "Ten sposób liczenia — na konkretnych zmianach zamiast na ułamkach — nazywamy naturalnymi częstościami. Wróci on w następnym wykładzie jako główne narzędzie do rozbrajania pozornie paradoksalnych wyników. Przy każdym prawdopodobieństwie warunkowym zadawaj dwa pytania kontrolne: ile przypadków spełnia warunek B i w ilu spośród nich zaszło także A?",
              "Suwaki poniżej sterują trzema parametrami naraz: jak częsty jest warunek oraz jak ryzykowna jest praca z warunkiem i bez niego. Zwróć uwagę, że P(incydent) w całym zakładzie zawsze leży pomiędzy dwiema wartościami warunkowymi — bliżej tej grupy, która jest liczniejsza."
            ),
            risk_try("zostaw ustawienia startowe i policz na siatce trójkąty oraz wypełnione trójkąty. Potem zwiększ udział zmian z przegrzaniem do 0,40 i obserwuj, jak P(incydent) w modelu przesuwa się w stronę P(incydent | przegrzanie)."),
            warunki_filter_widget,
            "Przy ustawieniach startowych siatka ma 50 trójkątów, z których 6 jest wypełnionych, i 450 kółek, z których wypełnione są 2. Udział incydentów wśród trójkątów to 6/50 = 0,12 — dokładnie P(A | B). Udział incydentów na całej siatce to 8/500 = 0,016, a model podaje 0,0165. Różnica bierze się z zaokrąglenia: 0,005 · 450 = 2,25 incydentu, a na siatce można narysować tylko całe zmiany. Właśnie dlatego panel pokazuje obok siebie ilustrację i wynik modelu.",
            risk_example("2.3", "Ile zmian zostaje po filtrze?",
              problem = "Na siatce 500 zmian ustawiono udział przegrzania 0,40, P(incydent | przegrzanie) = 0,12 i P(incydent | brak przegrzania) = 0,005. Ile zmian spełnia warunek, ile incydentów wypada w każdej grupie i ile wynosi P(incydent) w modelu?",
              steps = c(
                "Warunek spełnia 0,40 · 500 = 200 zmian; bez przegrzania zostaje 300.",
                "Incydenty wśród zmian z przegrzaniem: 0,12 · 200 = 24. Wśród pozostałych: 0,005 · 300 = 1,5, czyli na siatce 2 zmiany po zaokrągleniu.",
                "Model: P(incydent) = 0,40 · 0,12 + 0,60 · 0,005 = 0,048 + 0,003 = 0,051."
              ),
              answer = "Po filtrze zostaje 200 zmian i 24 incydenty, więc P(A | B) nadal wynosi 0,12. Zmienił się natomiast wynik ogólny: 0,051 zamiast 0,0165, bo liczniejsza grupa z przegrzaniem ciągnie średnią w swoją stronę."
            ),
            risk_check("w2_chk_mianownik",
              "Na siatce 500 zmian jest 50 zmian z przegrzaniem, w tym 6 z incydentem, oraz 2 incydenty na pozostałych zmianach. Ile wynosi udział incydentów wśród zmian z przegrzaniem?",
              c("6/500 = 0,012" = "all", "6/50 = 0,12" = "cond", "6/8 = 0,75" = "inverse"),
              correct = "cond",
              explanation = "Warunek „przegrzanie” zawęża mianownik do 50 zmian. 6/500 to P(A ∩ B), a 6/8 to P(B | A) — udział przegrzań wśród zmian z incydentem.",
              hints = c(all = "To udział zmian z incydentem i przegrzaniem wśród wszystkich 500 — czyli P(A ∩ B). Gdzie jest filtr?", inverse = "Mianownik 8 to wszystkie incydenty. Takie pytanie brzmi: jak często incydentowi towarzyszyło przegrzanie?")
            )
          )
        )
      ),
      takeaway = "Warunek zmienia mianownik, nie przeszłość. Prowadzący w studiu i czujnik przegrzania w hali wykonują tę samą operację: zawężają świat, w którym liczymy."
    ),
    list(
      id = "reprezentacje", title = "Reguła mnożenia", hook = "Drogę do incydentu da się narysować", lead = "Tabela, drzewo dróg i udziały są różnymi mapami tych samych liczebności, a drzewo podpowiada regułę mnożenia.",
      intro = c(
        "Sposób prezentacji powinien ułatwiać odpowiedź, a nie zmieniać problem. Tabela dobrze pilnuje liczebności, drzewo pokazuje kolejność warunków, a słupki pomagają porównać częstości w grupach.",
        "W praktyce inspektora wybór widoku to wybór narzędzia komunikacji: tabela przekonuje audytora, który chce sprawdzić sumy, drzewo tłumaczy mechanizm zarządowi, a wykres udziałów najlepiej pokazuje kontrast między grupami na slajdzie. Umiejętność przejścia między nimi bez zmiany liczb jest testem zrozumienia. Poniżej wszystkie trzy widoki tych samych 1000 zmian Bananpolu obok siebie — sprawdź, czy w każdym znajdujesz te same liczebności."
      ),
      body = list(
        risk_try("znajdź liczbę 12 w tabeli, na drzewie i na wykresie udziałów. Potem odszukaj w każdym widoku mianownik 100 i mianownik 1000."),
        warunki_views_widget,
        "W każdym widoku liczba incydentów i liczebność grup są takie same. Jeśli wynik zmienia się wraz z rodzajem wykresu, zmieniliśmy definicję albo mianownik, a nie tylko sposób prezentacji.",
        "Każdy widok eksponuje co innego. W tabeli liczby 12 i 17 stoją w tej samej kolumnie, więc łatwo przeczytać P(B | A) = 12/17. Na drzewie 12 stoi na końcu gałęzi wychodzącej z węzła „Przegrzanie: 100”, więc naturalnie czytamy P(A | B) = 12/100. Wykres udziałów w ogóle nie pokazuje liczebności grup: słupek zmian z przegrzaniem ma ten sam rozmiar co słupek 900 zmian bez przegrzania. Porównuje więc wyłącznie P(A | B) = 0,12 z P(A | nie B) = 5/900 ≈ 0,006 i ukrywa, że pierwsza grupa jest dziewięć razy mniejsza."
      ),
      sections = list(
        list(
          id = "iloczyn", title = "Mnożymy wzdłuż drogi",
          body = list(
            c(
              "Drzewo pokazuje coś więcej niż tylko liczebności. Na gałęziach stoją prawdopodobieństwa, a na końcach liczby zmian — i między jednymi a drugimi jest prosty związek. Żeby dojść do liścia „Incydent” w górnej części drzewa, trzeba przejść dwie gałęzie: najpierw trafić do grupy zmian z przegrzaniem, a potem, już wewnątrz tej grupy, trafić na incydent. Iloczyn nie pojawia się więc jako sztuczka algebraiczna — odpowiada przejściu przez dwa kolejne filtry.",
              "Pierwszy czynnik odnosi się do wszystkich zmian: przegrzanie dotyczy 10% z 1000, czyli 100 zmian. Drugi czynnik odnosi się już tylko do tej setki: incydent występuje w 12% z nich, czyli w 12 zmianach. Te 12 zmian to 1,2% całej obserwowanej populacji — i dokładnie tyle daje pomnożenie 0,10 przez 0,12. Prześledź tę drogę na liczbach poniżej."
            ),
            warunki_path_widget,
            "Ten rachunek nie korzystał z niczego szczególnego w liczbach 0,10 i 0,12 — działa dla dowolnych wartości, więc uogólniamy go w jedną regułę, nazywaną regułą iloczynu (wzorem na prawdopodobieństwo iloczynu zdarzeń):",
            risk_formula("P(A\\cap B)=P(B)\\,P(A\\mid B)=P(A)\\,P(B\\mid A)", num = "2.2",
              legend = c("P(B)" = "udział wszystkich zmian, które wchodzą do grupy B", "P(A\\mid B)" = "udział A wewnątrz grupy B", "P(A)\\,P(B\\mid A)" = "ta sama droga przebyta w odwrotnej kolejności")),
            "Pierwszy czynnik wprowadza do grupy spełniającej warunek, drugi liczy zdarzenie wewnątrz tej grupy.",
            risk_derivation("reguła iloczynu", c(
              "Wzór (2.2) nie jest nowym założeniem, tylko definicją 2.1 przepisaną inaczej. Mnożymy obie strony wzoru (2.1) przez P(B). Ponieważ w definicji role A i B są symetryczne w przecięciu (A ∩ B = B ∩ A), tę samą operację można wykonać z warunkiem A, o ile P(A) > 0."
            ), lines = c("P(A | B) = P(A ∩ B) / P(B)    | · P(B)", "P(A ∩ B) = P(B) · P(A | B)", "", "P(B | A) = P(A ∩ B) / P(A)    | · P(A)", "P(A ∩ B) = P(A) · P(B | A)")),
            "Warto czytać każdy czynnik razem z jego mianownikiem. W zapisie P(B)·P(A | B) pierwsza liczba mówi, jaka część wszystkich zmian wchodzi do grupy, a druga — jaka część tej grupy kończy się incydentem. Iloczyn wraca do wspólnego mianownika wszystkich zmian.",
            "Druga postać wzoru (2.2) jest równie prawdziwa, choć na drzewie Bananpolu jej nie widać: drzewo zaczyna od przegrzania, a nie od incydentu. Gdybyśmy narysowali drzewo w odwrotnej kolejności, pierwsza gałąź miałaby wagę P(A) = 0,017, druga P(B | A) = 12/17, a ich iloczyn znów dałby 12/1000 = 0,012. Ta symetria jest podstawą wzoru Bayesa, do którego dojdziemy w następnym rozdziale.",
            risk_check("w2_chk_droga",
              "Na drzewie dróg gałąź „Brak przegrzania” ma wagę 0,9, a następująca po niej gałąź „Incydent” — 5/900 ≈ 0,006. Ile wynosi prawdopodobieństwo drogi „brak przegrzania i incydent” liczone wśród wszystkich zmian?",
              c("0,005" = "path", "0,006" = "branch", "0,9" = "first"),
              correct = "path",
              explanation = "Ze wzoru (2.2): 0,9 · 5/900 = 5/1000 = 0,005. To 5 zmian z 1000 — dolny liść „Incydent: 5” na drzewie.",
              hints = c(branch = "0,006 to P(A | nie B) — udział wewnątrz 900 zmian bez przegrzania. Wróć do wspólnego mianownika 1000.", first = "0,9 to dopiero wejście do grupy bez przegrzania. Droga ma dwie gałęzie.")
            )
          )
        ),
        list(
          id = "lancuch", title = "Dłuższe drogi",
          body = list(
            "Drzewo może mieć więcej niż dwa poziomy. Wtedy reguła iloczynu działa krok po kroku: każda kolejna gałąź jest prawdopodobieństwem warunkowym względem wszystkiego, co zaszło wcześniej na tej drodze. Dla trzech zdarzeń dostajemy regułę łańcuchową:",
            risk_formula("P(A\\cap B\\cap C)=P(C)\\,P(B\\mid C)\\,P(A\\mid B\\cap C)", num = "2.3",
              legend = c("P(C)" = "pierwsza gałąź, liczona wśród wszystkich przypadków", "P(B\\mid C)" = "druga gałąź, liczona wśród przypadków z C", "P(A\\mid B\\cap C)" = "trzecia gałąź, liczona wśród przypadków z B i C naraz")),
            "Wzór (2.3) wynika z dwukrotnego zastosowania (2.2): najpierw P(A ∩ (B ∩ C)) = P(B ∩ C) · P(A | B ∩ C), a potem P(B ∩ C) = P(C) · P(B | C). Każdy czynnik ma własny, coraz węższy mianownik — i dlatego najczęstszym błędem przy dłuższych drogach jest wstawienie w środek łańcucha prawdopodobieństwa liczonego w całej populacji zamiast w grupie, do której doszliśmy.",
            risk_example("2.4", "Przeciążenie, przegrzanie, incydent",
              problem = "W Bananpolu 20% zmian przebiega w trybie przeciążenia (C). Na zmianach w przeciążeniu czujnik wykrywa przegrzanie (B) w 30% przypadków. Na zmianach w przeciążeniu z wykrytym przegrzaniem incydent (A) zdarza się w 15% przypadków. Oblicz prawdopodobieństwo, że losowa zmiana jest przeciążona, z przegrzaniem i z incydentem, oraz zapisz wynik w naturalnych częstościach.",
              steps = c(
                "Ze wzoru (2.3): P(A ∩ B ∩ C) = 0,20 · 0,30 · 0,15.",
                "0,20 · 0,30 = 0,06; 0,06 · 0,15 = 0,009.",
                "Naturalne częstości: z 1000 zmian 200 jest przeciążonych, z nich 60 ma przegrzanie, z tych 60 incydentem kończy się 9."
              ),
              answer = "0,009, czyli około 9 zmian na 1000. Liczba 0,15 dotyczy tylko 60 zmian na końcu łańcucha — pomnożona przez 1000 dałaby absurdalne 150 incydentów."
            )
          )
        )
      ),
      decision = "Reguła iloczynu opisuje drogę, ale nie uzasadnia niezależności."
    ),
    list(
      id = "calkowite", title = "Prawdopodobieństwo całkowite", hook = "Do incydentu prowadzą dwie drogi", lead = "Sumujemy rozłączne drogi: incydent może powstać podczas pracy normalnej albo przeciążenia.",
      intro = c(
        "Wynik ogólny jest średnią ważoną wyników w grupach. Wysokie prawdopodobieństwo w rzadkim trybie może mieć mały wkład do całości, natomiast niewielka zmiana w dominującym trybie może silnie przesunąć wynik.",
        "To tłumaczy częste zaskoczenie w raportach bezpieczeństwa: tryb pracy, o którym wszyscy mówią, bo jest spektakularnie ryzykowny, może odpowiadać za mniejszość incydentów — jeśli występuje rzadko. Zanim wskażesz głównego winowajcę, pomnóż ryzyko warunkowe przez udział trybu."
      ),
      sections = list(
        list(
          id = "partycja", title = "Kompletna partycja",
          text = "Dzielimy przestrzeń na rozłączne tryby B_i, które razem obejmują wszystkie analizowane zmiany, i sumujemy wkład każdej drogi. Żadna zmiana nie może zniknąć ani należeć do dwóch trybów naraz.",
          body = list(
            risk_definition("2.2", "Układ zupełny zdarzeń", c(
              "Zdarzenia B₁, B₂, …, B_k tworzą układ zupełny (partycję), jeśli są parami rozłączne — żadne dwa nie mogą zajść jednocześnie — a ich suma obejmuje wszystkie możliwe przypadki, czyli dokładnie jedno z nich zawsze zachodzi. Zakładamy ponadto, że P(B_i) > 0 dla każdego i.",
              "Najprostszym układem zupełnym jest para: zdarzenie B i jego dopełnienie „nie B”."
            )),
            "Przegrzanie i brak przegrzania tworzą układ zupełny: każda zmiana należy do dokładnie jednej z tych grup. Podobnie tryby „przeciążenie” i „normalna praca”, jeśli każdą zmianę zakwalifikowano do jednego z nich. Nie tworzą natomiast układu zupełnego kategorie „zmiana nocna” i „zmiana w przeciążeniu”, bo zmiana nocna może być przeciążona, a zmiana dzienna normalna nie należy do żadnej z nich.",
            risk_formula("P(A)=\\sum_{i=1}^{k} P(B_i)\\,P(A\\mid B_i)", num = "2.4",
              legend = c("B_1,\\ldots,B_k" = "układ zupełny zdarzeń (tryby pracy)", "P(B_i)" = "waga drogi: udział trybu i", "P(A\\mid B_i)" = "prawdopodobieństwo A wewnątrz trybu i")),
            "Wagi P(B_i) są udziałami trybów pracy i sumują się do jedności.",
            risk_derivation("wzór na prawdopodobieństwo całkowite", c(
              "Ponieważ tryby B_i pokrywają wszystkie przypadki, każde wystąpienie A leży dokładnie w jednym z nich. Zdarzenie A rozpada się więc na rozłączne kawałki A ∩ B₁, …, A ∩ B_k — jeden kawałek na każdą drogę drzewa.",
              "Prawdopodobieństwa rozłącznych zdarzeń się dodają, a każdy kawałek liczymy regułą iloczynu (2.2):"
            ), lines = c("A = (A ∩ B₁) ∪ (A ∩ B₂) ∪ … ∪ (A ∩ B_k)     (kawałki rozłączne)", "P(A) = P(A ∩ B₁) + … + P(A ∩ B_k)", "     = P(B₁)·P(A | B₁) + … + P(B_k)·P(A | B_k)", "", "Bananpol: 0,10 · 0,12 + 0,90 · 5/900 = 0,012 + 0,005 = 0,017")),
            "Warunki układu zupełnego nie są formalnością. Jeśli tryby się nakładają, zmiany ze wspólnej części zostaną policzone dwa razy i wynik będzie zawyżony. Jeśli tryby czegoś nie obejmują — na przykład pominięto zmiany serwisowe — ich incydenty znikną z sumy i wynik będzie zaniżony."
          )
        ),
        list(
          id = "drzewo", title = "Licznik i mianownik na drzewie",
          body = list(
            "Wróćmy do drzewa z 1000 zmian Bananpolu. Wybierz prawdopodobieństwo, a drzewo pokaże, które węzły tworzą licznik, a które mianownik. Zacznij od P(A): incydent może powstać na dwóch rozłącznych drogach, więc jego licznik to suma dwóch liści. Przy prawdopodobieństwach warunkowych licznik leży wewnątrz mianownika — te same zmiany liczymy raz na górze i raz na dole ułamka.",
            risk_try("przejdź kolejno przez wszystkie pięć opcji. Przy każdej zapisz, czy mianownikiem jest korzeń drzewa (1000), węzeł pośredni (100) czy suma liści (17)."),
            warunki_tree_read_widget,
            "Drzewo pokazuje ważoną sumę w konkretnych zmianach: 12 incydentów z gałęzi przegrzania i 5 z gałęzi bez przegrzania. Suwaki w następnej sekcji robią to samo w ułamkach, dla dwóch trybów pracy.",
            "Najciekawsza jest ostatnia opcja, P(B | A). Jej mianownikiem nie jest żaden pojedynczy węzeł drzewa, lecz suma dwóch liści „Incydent” z różnych gałęzi — czyli dokładnie licznik z opcji P(A). Drzewo zbudowane w kolejności „najpierw warunek, potem zdarzenie” pozwala więc odwrócić pytanie, ale wymaga do tego wzoru na prawdopodobieństwo całkowite w mianowniku. Wrócimy do tego w sekcji o odwracaniu warunku."
          )
        ),
        list(
          id = "wagi", title = "Nie sumujemy samych ryzyk warunkowych",
          text = "P(A | B₁) i P(A | B₂) mają różne mianowniki. Zanim je dodamy, ważymy każde prawdopodobieństwo udziałem odpowiadającego mu trybu pracy. Sprawdź to na suwakach: przesuwaj udział przeciążenia i obserwuj, jak wynik ogólny wędruje między dwiema wartościami warunkowymi.",
          body = list(
            risk_try("ustaw udział przeciążenia na 0, potem na 1. Odczytaj, ile wynosi P(incydent) na obu krańcach, i porównaj z suwakami ryzyk warunkowych."),
            warunki_total_widget,
            "Prosta, po której porusza się punkt na wykresie, jest wykresem jednej reguły — ważonej sumy rozłącznych dróg, czyli wzoru (2.4) dla dwóch trybów. Przy udziale przeciążenia s ma on postać P(A) = s · P(A | przeciążenie) + (1 − s) · P(A | normalna praca). To funkcja liniowa zmiennej s: na lewym krańcu wykresu (s = 0) równa się ryzyku pracy normalnej, na prawym (s = 1) — ryzyku przeciążenia, a pomiędzy nimi rośnie jednostajnie.",
            risk_example("2.5", "Skąd biorą się incydenty?",
              problem = "Przyjmij ustawienia startowe suwaków: udział przeciążenia 0,20, P(incydent | przeciążenie) = 0,15, P(incydent | normalna praca) = 0,01. Oblicz P(incydent) i udział każdej drogi w incydentach. Następnie powtórz rachunek dla udziału przeciążenia 0,05.",
              steps = c(
                "Ze wzoru (2.4): P(A) = 0,20 · 0,15 + 0,80 · 0,01 = 0,030 + 0,008 = 0,038.",
                "Udział drogi przez przeciążenie: 0,030 / 0,038 ≈ 0,79; przez pracę normalną: 0,008 / 0,038 ≈ 0,21.",
                "Dla udziału 0,05: P(A) = 0,05 · 0,15 + 0,95 · 0,01 = 0,0075 + 0,0095 = 0,017.",
                "Teraz droga przez przeciążenie daje 0,0075 / 0,017 ≈ 0,44 incydentów, a praca normalna ≈ 0,56.",
                "Kontrola błędu: naiwna średnia (0,15 + 0,01) / 2 = 0,08 nie odpowiada żadnemu z tych scenariuszy, bo pomija wagi."
              ),
              answer = "Przy 20% przeciążeń odpowiada ono za około 79% incydentów (38 na 1000 zmian). Gdy przeciążenia stanowią tylko 5% zmian, większość incydentów — około 56% — pochodzi z pracy normalnej, choć jest ona piętnaście razy mniej ryzykowna."
            ),
            risk_check("w2_chk_wagi",
              "P(incydent | przeciążenie) = 0,15, a P(incydent | normalna praca) = 0,01. Czy dla jakiegoś udziału przeciążenia P(incydent) może wynieść 0,20?",
              c("Tak, jeśli przeciążenie jest bardzo częste" = "yes", "Nie, wynik zawsze leży między 0,01 a 0,15" = "no", "Tak, bo sumujemy dwa ryzyka: 0,15 + 0,01" = "sum"),
              correct = "no",
              explanation = "Wzór (2.4) to średnia ważona z wagami sumującymi się do jedności, więc wynik leży między najmniejszym a największym ryzykiem warunkowym. Nawet gdy wszystkie zmiany są przeciążone, P(incydent) = 0,15.",
              hints = c(yes = "Przesuń suwak udziału do 1. Ile wynosi wtedy P(incydent)?", sum = "Dodawanie ryzyk bez wag to dokładnie błąd z tytułu sekcji. Różne mianowniki, różne grupy.")
            )
          )
        ),
        list(
          id = "bayes", title = "Odwracamy warunek",
          body = list(
            c(
              "Wzór na prawdopodobieństwo całkowite idzie w kierunku drzewa: od trybów do incydentu. Po incydencie inspektor pyta w przeciwną stronę: skoro incydent już się zdarzył, na której drodze najprawdopodobniej powstał? To pytanie o P(B_j | A), a nie o P(A | B_j).",
              "Odpowiedź daje połączenie trzech narzędzi tego wykładu. Definicja 2.1 zapisuje P(B_j | A) jako iloraz P(A ∩ B_j) / P(A). Licznik rozpisujemy regułą iloczynu (2.2) w kierunku drzewa, a mianownik — wzorem (2.4) jako sumę wszystkich dróg. Wynik nosi nazwę wzoru Bayesa."
            ),
            risk_formula("P(B_j\\mid A)=\\frac{P(B_j)\\,P(A\\mid B_j)}{\\sum_{i=1}^{k} P(B_i)\\,P(A\\mid B_i)}", num = "2.5",
              legend = c("B_j" = "tryb, o który pytamy", "P(B_j)" = "udział trybu przed obserwacją", "P(A\\mid B_j)" = "prawdopodobieństwo obserwacji A w tym trybie", "\\sum_i" = "prawdopodobieństwo całkowite A, wzór (2.4)")),
            "Licznik to jedna droga drzewa, mianownik — wszystkie drogi prowadzące do A. Wzór Bayesa odpowiada więc na pytanie: jaką część liści „Incydent” stanowi liść na końcu drogi przez B_j? Na drzewie Bananpolu jest to dokładnie opcja P(B | A) z poprzedniej sekcji: 12 z 17 liści, czyli około 0,71.",
            risk_example("2.6", "Po incydencie: który tryb?",
              problem = list(
                "Na zmianie w Bananpolu doszło do incydentu.",
                risk_parts(
                  "Jakie jest prawdopodobieństwo, że czujnik wykrył na niej przegrzanie? Użyj danych z drzewa: P(B) = 0,10, P(A | B) = 0,12, P(A | nie B) = 5/900.",
                  "W sytuacji z przykładu 2.5 (udział przeciążenia 0,20, ryzyka 0,15 i 0,01) doszło do incydentu. Jakie jest prawdopodobieństwo, że zmiana była przeciążona?"
                )
              ),
              steps = c(
                "Licznik: P(B) · P(A | B) = 0,10 · 0,12 = 0,012. Mianownik ze wzoru (2.4): 0,012 + 0,90 · 5/900 = 0,012 + 0,005 = 0,017. Ze wzoru (2.5): P(B | A) = 0,012 / 0,017 ≈ 0,706.",
                "Licznik: 0,20 · 0,15 = 0,030. Mianownik: 0,038 z przykładu 2.5. Stąd P(przeciążenie | incydent) = 0,030 / 0,038 ≈ 0,789."
              ),
              steps_type = "a",
              answer = "(a) Około 0,71: przegrzanie dotyczy tylko 10% zmian, ale towarzyszy siedmiu na dziesięć incydentów. (b) Około 0,79: przeciążenie to 20% zmian, ale prawie cztery piąte incydentów. Obserwacja incydentu silnie przesuwa ocenę w stronę trybu, w którym incydenty są częstsze."
            ),
            risk_derivation("Monty Hall według wzoru (2.5)", c(
              "Wybrałeś bramkę 2. Niech H₁, H₂, H₃ oznaczają położenie nagrody — to układ zupełny, każde z prawdopodobieństwem 1/3. Obserwacja O: prowadzący otworzył bramkę 1. Z zasad gry P(O | H₁) = 0 (nie odsłoni nagrody), P(O | H₂) = 1/2 (rzuca monetą), P(O | H₃) = 1 (nie ma wyboru).",
              "Mianownik to wzór (2.4), licznik — droga przez H₃:"
            ), lines = c("P(O) = 1/3 · 0 + 1/3 · 1/2 + 1/3 · 1 = 1/2", "P(H₃ | O) = (1/3 · 1) / (1/2) = 2/3", "P(H₂ | O) = (1/3 · 1/2) / (1/2) = 1/3")),
            "Ten sam rachunek, który w studiu daje przewagę zmianie bramki, w zakładzie pozwala wskazać najbardziej prawdopodobne źródło incydentu. Jest też źródłem najczęstszego błędu w interpretacji alarmów: utożsamiania P(A | B) z P(B | A). W przykładzie 2.6(a) te dwie liczby to 0,12 i 0,71 — różnią się prawie sześciokrotnie. W następnym wykładzie wzór (2.5) stanie się głównym narzędziem do oceny, co naprawdę znaczy sygnał alarmu.",
            risk_check("w2_chk_bayes",
              "W Bananpolu P(A | B) = 0,12, a P(B | A) ≈ 0,71. Która informacja najbardziej odpowiada za to, że druga liczba jest dużo większa od pierwszej?",
              c("Incydenty są rzadkie: jest ich 17, a zmian z przegrzaniem 100" = "base", "Czujnik przegrzania jest mało dokładny" = "sensor", "Wzór Bayesa zawsze zwiększa prawdopodobieństwo" = "always"),
              correct = "base",
              explanation = "Z reguły iloczynu (2.2) P(B | A) / P(A | B) = P(B) / P(A) = 0,10 / 0,017 ≈ 5,9. O stosunku obu liczb decyduje stosunek wielkości grup, do których się odnoszą: 100 zmian z przegrzaniem wobec 17 incydentów.",
              hints = c(sensor = "Wszystkie liczby dotyczą tych samych 12 zmian z oboma zdarzeniami. Co różni mianowniki 100 i 17?", always = "Wzór (2.5) może też obniżać prawdopodobieństwo — np. P(normalna praca | incydent) ≈ 0,21 < 0,80.")
            )
          )
        ),
        list(
          id = "transfer", title = "Przykład transferowy: droga do pracy",
          text = "Ten sam wzór działa poza zakładem. Ryzyko kolizji rowerzysty w mieście jest ważoną sumą ryzyka na drogach dla rowerów i na jezdni: nawet gdy jezdnia jest kilkukrotnie bardziej ryzykowna na kilometr, o łącznym wyniku decyduje również to, jaką część trasy stanowi. Zmiana trasy to zmiana wag — bez zmiany żadnego ryzyka warunkowego."
        )
      )
    ),
    list(
      id = "niezaleznosc", title = "Niezależność zdarzeń", hook = "Dwa urządzenia to nie zawsze dwie szanse", lead = "Dwa urządzenia nie stają się niezależne tylko dlatego, że są dwa.",
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
        list(
          id = "definicja", title = "Definicja i trzy równoważne testy",
          body = list(
            "Warunek P(A | B) = P(A) jest intuicyjny, ale ma dwie wady: wymaga P(B) > 0 i wygląda na niesymetryczny, jakby B wpływało na A, a nie odwrotnie. Dlatego formalna definicja korzysta z reguły iloczynu (2.2): jeśli P(A | B) = P(A), to P(A ∩ B) = P(B) · P(A). Tę równość przyjmujemy za definicję.",
            risk_definition("2.3", "Niezależność zdarzeń", c(
              "Zdarzenia A i B nazywamy niezależnymi, jeśli spełniają wzór (2.6). W przeciwnym razie nazywamy je zależnymi.",
              "Dla P(B) > 0 niezależność jest równoważna warunkowi P(A | B) = P(A), a dla P(A) > 0 — warunkowi P(B | A) = P(B)."
            )),
            risk_formula("P(A\\cap B)=P(A)\\,P(B)", num = "2.6",
              legend = c("P(A\\cap B)" = "prawdopodobieństwo, że oba zdarzenia zachodzą naraz", "P(A)\\,P(B)" = "iloczyn prawdopodobieństw bezwarunkowych")),
            risk_derivation("równoważność trzech testów", c(
              "Załóżmy, że P(A) > 0 i P(B) > 0. Dzielimy wzór (2.6) przez P(B) albo przez P(A) i korzystamy z definicji 2.1:"
            ), lines = c("P(A ∩ B) = P(A) · P(B)", "⇔ P(A ∩ B) / P(B) = P(A)   ⇔   P(A | B) = P(A)", "⇔ P(A ∩ B) / P(A) = P(B)   ⇔   P(B | A) = P(B)")),
            "Równoważność ma praktyczną konsekwencję: niezależność jest symetryczna. Jeśli przegrzanie niczego nie mówi o incydencie, to i incydent niczego nie mówi o przegrzaniu. Podobnie działa zależność: jeśli B podnosi prawdopodobieństwo A, to A podnosi prawdopodobieństwo B.",
            risk_example("2.7", "Czy incydent i przegrzanie są niezależne?",
              problem = "Sprawdź trzema testami, czy w danych Bananpolu (1000 zmian, 100 z przegrzaniem, 17 incydentów, 12 z obu zdarzeniami) incydent A i przegrzanie B są niezależne.",
              steps = c(
                "Test iloczynu (2.6): P(A) · P(B) = 0,017 · 0,10 = 0,0017, a P(A ∩ B) = 0,012 — ponad siedem razy więcej.",
                "Test warunkowy: P(A | B) = 0,12, a P(A) = 0,017.",
                "Test odwrotny: P(B | A) = 12/17 ≈ 0,71, a P(B) = 0,10.",
                "Gdyby zdarzenia były niezależne, spodziewalibyśmy się 0,0017 · 1000 = 1,7 zmiany z oboma zdarzeniami, a nie 12."
              ),
              answer = "Wszystkie trzy testy dają ten sam werdykt: zdarzenia są wyraźnie zależne. We wszystkich trzech pojawia się ten sam współczynnik około 7 — to nie przypadek, tylko konsekwencja równoważności testów."
            ),
            "Niezależności nie należy mylić z rozłącznością. Zdarzenia rozłączne nie mogą zajść razem, więc P(A ∩ B) = 0. Jeśli oba mają dodatnie prawdopodobieństwa, to P(A) · P(B) > 0 i wzór (2.6) nie zachodzi. Rozłączność jest więc skrajną zależnością: wiedza, że zaszło B, mówi z pewnością, że nie zaszło A.",
            risk_check("w2_chk_niez",
              "W rejestrze: P(awaria czujnika) = 0,30, P(zmiana nocna) = 0,50, P(awaria czujnika na zmianie nocnej) = 0,15 (liczone wśród wszystkich zmian). Czy awaria czujnika i zmiana nocna są niezależne?",
              c("Tak, bo 0,30 · 0,50 = 0,15" = "yes", "Nie, bo 0,15 jest mniejsze od 0,30" = "smaller", "Nie da się tego ocenić bez P(A | B)" = "cannot"),
              correct = "yes",
              explanation = "Wzór (2.6) jest spełniony: P(A) · P(B) = 0,15 = P(A ∩ B). Równoważnie P(awaria | noc) = 0,15 / 0,50 = 0,30 = P(awaria).",
              hints = c(smaller = "P(A ∩ B) zawsze jest nie większe niż P(A). Porównaj je z iloczynem P(A) · P(B).", cannot = "P(A | B) da się policzyć z definicji 2.1: 0,15 / 0,50.")
            )
          )
        ),
        list(
          id = "wspolna", title = "Wspólne zasilanie",
          text = "Utrata wspólnego zasilania może jednocześnie wyłączyć obie gałęzie i zniwelować redundancję. Ten przykład celowo wybiega naprzód: wróci w pełnej skali w wykładach o niezawodności systemu i drzewie błędów.",
          body = list(
            "Dwa niezależne zabezpieczenia, z których każde zawodzi z prawdopodobieństwem q, zawodzą razem z prawdopodobieństwem q² — to wzór (2.6). Przy q = 0,05 daje to 0,0025, czyli dwadzieścia razy mniej niż dla jednego zabezpieczenia. Właśnie ta obietnica czyni redundancję atrakcyjną. Jeśli jednak oba zabezpieczenia mają wspólne zasilanie, pojawia się trzecia droga do jednoczesnej awarii, która omija mnożenie.",
            "Najprostszy model rozdziela dwie sytuacje, które tworzą układ zupełny: zasilanie padło (prawdopodobieństwo c) albo działa (1 − c). Gdy padło, oba zabezpieczenia są wyłączone na pewno. Gdy działa, zawodzą niezależnie, każde z prawdopodobieństwem q. Wzór (2.4) daje wtedy:",
            risk_formula("P(\\text{obie awarie})=c\\cdot 1+(1-c)\\,q^{2}", num = "2.7",
              legend = c("c" = "prawdopodobieństwo utraty wspólnego zasilania", "q" = "prawdopodobieństwo awarii pojedynczego zabezpieczenia przy działającym zasilaniu", "q^{2}" = "jednoczesna awaria obu przy działającym zasilaniu, z niezależności")),
            "Zwróć uwagę, że niezależność nie znika z modelu — obowiązuje nadal, ale tylko wewnątrz grupy „zasilanie działa”. Taką sytuację nazywamy niezależnością warunkową: zdarzenia są niezależne przy ustalonej wspólnej przyczynie, a zależne, gdy tę przyczynę pominiemy.",
            risk_example("2.8", "Ile kosztuje wspólne zasilanie?",
              problem = "Każde z dwóch zabezpieczeń zawodzi z prawdopodobieństwem q = 0,05 przy działającym zasilaniu. Wspólne zasilanie pada z prawdopodobieństwem c = 0,01. Porównaj prawdopodobieństwo jednoczesnej awarii obu zabezpieczeń w modelu niezależnym i w modelu ze wspólną przyczyną.",
              steps = c(
                "Model niezależny, wzór (2.6): q² = 0,05² = 0,0025.",
                "Model ze wspólną przyczyną, wzór (2.7): 0,01 + 0,99 · 0,0025 = 0,01 + 0,002475 = 0,012475.",
                "Stosunek: 0,012475 / 0,0025 ≈ 5,0."
              ),
              answer = "Około 0,0125 zamiast 0,0025 — pięć razy więcej. Rzadka wspólna przyczyna (1%) dominuje wynik, bo jej droga nie jest mnożona przez drugie małe q. Im lepsze pojedyncze zabezpieczenia, tym większy względny udział wspólnej przyczyny."
            ),
            risk_try("zacznij od P utraty wspólnego zasilania równego 0 i sprawdź, że oba słupki są równe. Potem ustaw 0,01 i zmniejszaj P awarii pojedynczego zabezpieczenia do 0,01."),
            warunki_common_widget,
            "Przy c = 0 oba modele pokrywają się, bo pozostaje tylko droga niezależna. Już przy c = 0,01 słupek „Jawna wspólna przyczyna” jest około pięć razy wyższy. Gdy przy c = 0,01 zmniejszamy q do 0,01, model niezależny obiecuje 0,0001, a model ze wspólną przyczyną podaje około 0,0101 — ponad sto razy więcej. Poprawa pojedynczych zabezpieczeń prawie nie zmienia wyniku, bo prawie całe ryzyko jednoczesnej awarii pochodzi teraz od zasilania. Skuteczniejszą inwestycją jest wtedy rozdzielenie zasilania niż wymiana czujników.",
            risk_check("w2_chk_wspolna",
              "W modelu (2.7) q = 0,01 i c = 0,001. Które działanie najbardziej zmniejszy prawdopodobieństwo jednoczesnej awarii?",
              c("Zmniejszenie q do 0,005" = "q", "Rozdzielenie zasilania, czyli c ≈ 0" = "c", "Oba działania dają podobny efekt" = "same"),
              correct = "c",
              explanation = "Przy c = 0,001 wynik to około 0,0011, z czego 0,001 pochodzi od wspólnej przyczyny. Po usunięciu jej zostaje q² = 0,0001. Zmniejszenie q do 0,005 obniżyłoby tylko składnik 0,0001 do 0,000025 — wynik nadal przekraczałby 0,001.",
              hints = c(q = "Policz oba składniki wzoru (2.7). Który z nich dominuje?", same = "Porównaj wkład c · 1 z wkładem (1 − c) · q².")
            )
          )
        ),
        list(
          id = "audyt", title = "Zanim pomnożysz",
          bullets = c("Czy elementy mają wspólne zasilanie, otoczenie lub obsługę?", "Czy jedna awaria może obciążyć drugi element?", "Czy oba wyniki pochodzą z tego samego procesu rejestracji?"),
          body = "Każde pytanie z listy odpowiada innemu mechanizmowi zależności: wspólnej przyczynie, przeciążeniu jednego elementu po awarii drugiego i wspólnemu źródłowi błędu w danych. Jeśli na którekolwiek odpowiedź brzmi „tak” albo „nie wiadomo”, iloczyn P(A) · P(B) jest optymistycznym przybliżeniem, a nie wynikiem. Wtedy trzeba albo jawnie zamodelować wspólną przyczynę, jak we wzorze (2.7), albo uczciwie podać wynik jako dolne oszacowanie ryzyka."
        )
      ),
      pitfall = "P(A ∩ B)=P(A)P(B) wolno użyć dopiero po uzasadnieniu niezależności."
    ),
    list(
      id = "decyzja", title = "Różnica i iloraz ryzyk", hook = "Silny sygnał to jeszcze nie przyczyna", lead = "Działanie kierujemy tam, gdzie warunek istotnie zmienia ocenę.",
      intro = c(
        "Duża różnica między P(A | B) i P(A) może być użyteczna operacyjnie, nawet zanim poznamy pełny mechanizm. Może wskazać grupę do kontroli, ale sama nie rozstrzyga, czy usunięcie B zmniejszy częstość A.",
        "W Bananpolu przegrzanie podnosi ryzyko incydentu z 1,7% do 12% — to sygnał zbyt silny, żeby go zignorować, i zbyt słaby, żeby od razu wymieniać wentylatory. Rozsądna kolejność: skierować kontrolę tam, gdzie warunek wskazuje, i równolegle szukać mechanizmu."
      ),
      sections = list(
        list(id = "ranking", title = "Co sprawdzić najpierw", bullets = c("Nazwij zdarzenie i warunek.", "Porównaj P(A) z P(A | B) na tym samym horyzoncie.", "Sprawdź liczebność grupy B i niepewność wyniku.", "Ustal, czy warunek jest wskaźnikiem, czy możliwą przyczyną.")),
        list(
          id = "miary", title = "Jak mocno warunek zmienia ocenę?",
          body = list(
            "Porównanie P(A | B) z P(A) mówi, czy warunek coś wnosi. Do decyzji potrzebna jest jeszcze miara tego, jak dużo wnosi. W analizie ryzyka używa się dwóch podstawowych miar: różnicy ryzyk, która mówi, o ile zdarzeń więcej przypada na każdą zmianę z warunkiem, i ilorazu ryzyk (ryzyka względnego), który mówi, ile razy częściej zdarzenie występuje w grupie z warunkiem niż bez niego.",
            risk_formula("RD=P(A\\mid B)-P(A\\mid \\bar B),\\qquad RR=\\frac{P(A\\mid B)}{P(A\\mid \\bar B)}", num = "2.8",
              legend = c("\\bar B" = "dopełnienie warunku: brak przegrzania", "RD" = "różnica ryzyk, w jednostkach prawdopodobieństwa", "RR" = "iloraz ryzyk (ryzyko względne), bez jednostki")),
            "W Bananpolu RD = 0,12 − 5/900 ≈ 0,114, a RR = 0,12 / (5/900) = 21,6. Iloraz 21,6 jest większy niż „około 7×” z porównania z P(A), bo porównujemy teraz dwie rozłączne grupy, a nie grupę z całością, która sama zawiera zmiany z przegrzaniem. Obie liczby są poprawne, ale odpowiadają na różne pytania — w raporcie trzeba nazwać, z czym porównujemy.",
            risk_example("2.9", "Gdzie skierować kontrolę?",
              problem = "Dział utrzymania ruchu może wdrożyć dodatkową kontrolę, która zmniejsza o połowę ryzyko incydentu w grupie, do której zostanie skierowana. Wariant 1: kontrola na 100 zmianach z przegrzaniem. Wariant 2: kontrola na 900 zmianach bez przegrzania. Ile incydentów na 1000 zmian pozostanie w każdym wariancie i ile incydentów zapobiega jedna skontrolowana zmiana?",
              steps = c(
                "Wariant 1: P(A | B) spada z 0,12 do 0,06, czyli z 12 do 6 incydentów. Łącznie 6 + 5 = 11 incydentów, P(A) = 0,011.",
                "Wariant 2: w grupie bez przegrzania 5 incydentów spada do 2,5. Łącznie 12 + 2,5 = 14,5 incydentu, P(A) = 0,0145.",
                "Efekt na jedną skontrolowaną zmianę: wariant 1 — 6 / 100 = 0,06 incydentu; wariant 2 — 2,5 / 900 ≈ 0,0028 incydentu.",
                "Kontrola w grupie z przegrzaniem jest około 22 razy wydajniejsza na jedną zmianę — to ten sam iloraz co RR we wzorze (2.8)."
              ),
              answer = "Wariant 1 zostawia 11 incydentów na 1000 zmian, wariant 2 — 14,5. Kontrola ukierunkowana warunkiem zapobiega większej liczbie incydentów przy dziewięciokrotnie mniejszym nakładzie. Rachunek zakłada jednak, że kontrola rzeczywiście działa na mechanizm incydentu — o tym mówi sekcja o przyczynowości."
            ),
            risk_check("w2_chk_symetria",
              "Wiadomo, że P(A | B) > P(A). Co z tego wynika dla P(B | A)?",
              c("P(B | A) > P(B)" = "greater", "P(B | A) = P(A | B)" = "equal", "Nic — trzeba to osobno zmierzyć" = "nothing"),
              correct = "greater",
              explanation = "Z reguły iloczynu (2.2): P(A | B) > P(A) ⇔ P(A ∩ B) > P(A) · P(B) ⇔ P(B | A) > P(B). Dodatni związek działa w obie strony: w Bananpolu 0,12 > 0,017 i jednocześnie 0,71 > 0,10.",
              hints = c(equal = "Przykład 2.1: P(A | B) = 0,12, a P(B | A) ≈ 0,71. Te liczby zwykle się różnią.", nothing = "Pomnóż nierówność P(A | B) > P(A) przez P(B) i podziel przez P(A).")
            )
          )
        ),
        list(
          id = "sygnal", title = "Od liczby do wniosku",
          body = list(
            "Panel poniżej zestawia trzy liczby z tego wykładu i trzy wnioski, które kusi, żeby z nich wyciągnąć. Czytaj tabelę wierszami: każdy wiersz to inne zdanie do raportu i inny poziom pewności, na jaki pozwalają dane.",
            warunki_signal_panel,
            "Tylko pierwszy wniosek wynika wprost z obliczeń wykonanych w tym wykładzie: w grupie zmian z przegrzaniem incydenty są częstsze. Drugi wniosek jest twierdzeniem o mechanizmie i wymaga danych, których w tabeli 2×2 nie ma. Trzeci wniosek jest decyzją — wynika z danych w połączeniu z kosztami, jak w przykładzie 2.9, i nie potrzebuje rozstrzygnięcia przyczynowego, żeby był rozsądny."
          )
        ),
        list(id = "przyczynowosc", title = "Predykcja nie jest interwencją", text = c(
          "Warunek może dobrze przewidywać incydent, ponieważ oba zjawiska mają wspólną przyczynę. Decyzja o kontroli może wtedy nadal być rozsądna, lecz decyzja o usunięciu przyczyny wymaga mocniejszego uzasadnienia.",
          "Klasyczny przykład spoza zakładu: nocne zmiany wiążą się z wyższą częstością wypadków. Czy winna jest pora, zmęczenie, obsada, czy rodzaj zadań zlecanych nocą? Skierowanie dodatkowego nadzoru na noc jest zasadne od razu; przestawienie całej produkcji na dzień — dopiero po zrozumieniu mechanizmu."
        ),
        body = "W Bananpolu wspólną przyczyną mogłoby być przeciążenie z przykładu 2.4: zmiany w przeciążeniu częściej mają przegrzanie i częściej kończą się incydentem, nawet jeśli samo przegrzanie incydentu nie powoduje. Wtedy wymiana wentylatorów usunęłaby sygnał przegrzania, ale nie incydenty. Rozróżnienie, czy warunek jest przyczyną, czy tylko wskaźnikiem, wymaga porównania grup przy ustalonej wspólnej przyczynie — tej samej niezależności warunkowej, którą poznaliśmy przy wspólnym zasilaniu.")
      ),
      decision = "Przegrzanie uzasadnia dodatkową kontrolę, ale sam związek warunkowy nie dowodzi przyczynowości."
    ),
    list(
      id = "sprawdzenie", title = "Ściąga i sprawdzenie", hook = "Najpierw warunek, potem liczba", lead = "Filtruj mianownik, mnóż wzdłuż drogi i sumuj rozłączne drogi.",
      intro = "Ostatni rozdział łączy rachunek z audytem założeń. Poprawny symbol i poprawne działanie nie wystarczą, jeśli zdarzenie, warunek albo populacja odniesienia są niejasne.",
      sections = list(
        list(
          id = "podsumowanie", title = "Podsumowanie",
          text = c(
            "Wykład zaczął się od obserwacji, że to samo zdarzenie ma różne prawdopodobieństwa w różnych grupach odniesienia. Prawdopodobieństwo warunkowe (definicja 2.1, wzór 2.1) formalizuje filtrowanie: liczymy A wyłącznie wśród przypadków, w których zaszło B. Prowadzący w studiu i czujnik przegrzania w hali robią to samo — zawężają świat, a ruch prowadzącego niesie informację, bo zależy od tego, co ukryte.",
            "Z definicji wynikają trzy narzędzia rachunkowe. Reguła iloczynu (2.2) i jej wersja łańcuchowa (2.3) opisują drogi na drzewie: mnożymy wzdłuż gałęzi, pilnując mianownika każdego czynnika. Wzór na prawdopodobieństwo całkowite (2.4) sumuje rozłączne drogi układu zupełnego (definicja 2.2) i tłumaczy, dlaczego rzadki, ale ryzykowny tryb może odpowiadać za mniejszość incydentów. Wzór Bayesa (2.5) odwraca kierunek warunku i pokazuje, że P(A | B) i P(B | A) różnią się w stosunku P(B) do P(A).",
            "Niezależność (definicja 2.3, wzór 2.6) jest szczególnym przypadkiem, w którym warunek niczego nie zmienia, i wymaga uzasadnienia mechanizmem. Wspólna przyczyna (2.7) potrafi wielokrotnie podnieść ryzyko jednoczesnej awarii. Wreszcie miary (2.8) przekładają różnicę między grupami na decyzję — ale związek warunkowy wskazuje, gdzie działać, a nie dowodzi, co jest przyczyną."
          )
        ),
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
  warunki_monty_server(input, output, session)

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
  output$w2_table <- renderUI({
    d <- warunki_views_counts
    lc_table(
      data.frame(group = d$condition, event = d$event, no_event = d$no_event, total = d$total),
      cols = list(
        lc_col("group", "Grupa", "row"),
        lc_col("event", "Incydent", "num"),
        lc_col("no_event", "Brak incydentu", "num"),
        lc_col("total", "Razem", "num")
      ),
      foot = list(group = "Razem", event = sum(d$event), no_event = sum(d$no_event),
                  total = sum(d$total))
    )
  })
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
      scale_x_discrete(labels = scales::label_wrap(12)) +
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
      scale_x_discrete(labels = scales::label_wrap(14)) +
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
