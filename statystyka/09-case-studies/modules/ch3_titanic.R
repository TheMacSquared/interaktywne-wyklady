# ============================================================================
# CASE STUDY 3: Titanic
# Pytanie: Kto miał większe szanse przeżycia i czy liczyła się klasa?
# ============================================================================

tt_tab <- as.data.frame(datasets::Titanic)
tt <- tt_tab[rep(seq_len(nrow(tt_tab)), tt_tab$Freq), c("Class", "Sex", "Age", "Survived")]
rownames(tt) <- NULL
tt$klasa <- factor(tt$Class, levels = c("1st", "2nd", "3rd", "Crew"),
                   labels = c("1. klasa", "2. klasa", "3. klasa", "Załoga"))
tt$plec <- factor(tt$Sex, levels = c("Male", "Female"), labels = c("Mężczyźni", "Kobiety"))
tt$wiek <- factor(tt$Age, levels = c("Adult", "Child"), labels = c("Dorośli", "Dzieci"))
tt$przezyl <- as.integer(tt$Survived == "Yes")

tt_cols <- c(`1. klasa` = case_explore, `2. klasa` = case_test,
             `3. klasa` = case_conclude, `Załoga` = case_muted)

# Treść stała (dawniej output$ch3_crosstab na serwerze): składana przy budowaniu strony.
ch3_crosstab_ui <- function() {
  tab <- table(tt$klasa, factor(tt$przezyl, levels = c(1, 0),
                                labels = c("Przeżyli", "Zginęli")))
  lc_crosstab(tab, measure = "row", row_name = "Klasa", col_name = "Przeżycie",
              big_mark = " ")
}

# Treść stała (dawniej output$ch3_chi2 na serwerze): składana przy budowaniu strony.
ch3_chi2_ui <- function() {
  tab <- table(tt$klasa, tt$przezyl)
  ch <- suppressWarnings(chisq.test(tab))
  v <- rstatix::cramer_v(tab)
  lc_caption(HTML(paste0("Test χ² niezależności: χ²(", ch$parameter, ") = ",
                         formatC(ch$statistic, format = "f", digits = 1),
                         ", p ", lc_pval(ch$p.value), ", V Cramera = ",
                         formatC(v, format = "f", digits = 2), ".")))
}

ch3_ui <- lecture_chapter(id = "ch3", num = "3", title = "Titanic", content = tagList(

  lc_chapter_hero(
    kicker = "Rozdział 03 · Studium przypadku",
    num    = "03",
    title  = "Kto wsiadł do szalup.",
    lead   = "Z załogi przeżył co czwarty, z trzeciej klasy też co czwarty.
              Te same odsetki kryją jednak zupełnie różne historie, bo na
              pokładzie liczyło się przede wszystkim to, kim się było."
  ),

  # ========================================================================
  # KONTEKST
  # ========================================================================
  lc_h2("ch3-sytuacja", "Sytuacja wyjściowa"),

  lc_p("Trzecie studium dotyczy wyniku, który przyjmuje tylko dwie wartości:
    przeżył albo nie. Narzędzia pochodzą z wykładów 01, 05 i 07: ",
    gloss("tabela kontyngencji"), ", ", gloss("test chi-kwadrat", "test χ²"),
    " niezależności i ", gloss("regresja logistyczna"), "."),

  lc_p("W nocy z 14 na 15 kwietnia 1912 roku Titanic zatonął po zderzeniu
    z górą lodową. Na pokładzie było 2201 osób, a szalupy mogły pomieścić
    niewiele ponad połowę z nich. Przeżyło 711 osób, czyli 32.3%. Dane
    opisują każdą osobę czterema cechami: klasą (pierwsza, druga, trzecia
    albo załoga), płcią, wiekiem (dorosły albo dziecko) i tym, czy przeżyła.
    Pytanie brzmi: czy szanse przeżycia zależały od klasy, a jeśli tak, to
    czy dlatego, że pasażerowie wyższych klas mieli lepszy dostęp do szalup,
    czy dlatego, że w różnych klasach podróżowali inni ludzie."),

  lc_p("Analizę przeprowadzimy w czterech krokach:"),

  # Mapa kroków zamiast listy numerowanej
  figure_panel(
    label = "Ryc. 3.1",
    title = "Przebieg analizy",
    case_pipeline(case_steps_ch3)
  ),

  # ========================================================================
  # KROK 1: Opis
  # ========================================================================
  lc_h2("ch3-opis", "Krok 1: Kto przeżył"),

  lc_p("Wynik 0/1 najprościej opisać odsetkiem: jaka część osób w danej
    grupie przeżyła. Panel pokazuje ten odsetek dla grup wyznaczonych przez
    wybraną cechę. Liczba pod słupkiem to liczebność grupy."),

  figure_panel(
    label = "Ryc. 3.2",
    title = "Odsetek ocalałych w grupach",
    ch3_desc_fig()
  ),

  lc_p("Odsetek ocalałych maleje z klasą: 62.5% w pierwszej, 41.4% w drugiej,
    25.2% w trzeciej i 24.0% wśród załogi. Jeszcze większa jest różnica
    między płciami: przeżyło 73.2% kobiet i 21.2% mężczyzn. Dzieci
    przeżywały częściej niż dorośli (52.3% wobec 31.3%), ale było ich tylko
    109."),

  # ========================================================================
  # KROK 2: Test χ²
  # ========================================================================
  lc_h2("ch3-chi2", "Krok 2: Test χ² niezależności"),

  lc_p("Odsetki opisują tę konkretną katastrofę. Test χ² z rozdziału 07
    wykładu 05 sprawdza, czy tak duże różnice między klasami mogłyby się
    pojawić, gdyby przeżycie nie zależało od klasy. Tabela pokazuje odsetki
    w wierszach, czyli rozkład przeżycia w każdej klasie."),

  figure_panel(
    label = "Ryc. 3.3",
    title = "Klasa a przeżycie",
    ch3_crosstab_ui(),
    ch3_chi2_ui()
  ),

  lc_p("Test odrzuca hipotezę o niezależności (χ²(3) = 190.4, p < 0.001).
    ", gloss("V Cramera"), " wynosi 0.29, co oznacza umiarkowany związek.
    Dla płci i przeżycia związek jest silniejszy (V = 0.45)."),

  case_quiz_ui("ch3_quiz_chi2",
    title = "Co mówi istotny wynik testu χ²?",
    question = "χ²(3) = 190.4, p < 0.001 dla klasy i przeżycia. Które zdanie
      jest uprawnione?",
    choices = c(
      "Podróż wyższą klasą zwiększała szanse przeżycia." = "causal",
      "Przeżycie było powiązane z klasą; test nie mówi, dlaczego." = "assoc",
      "Każda klasa różniła się od każdej innej odsetkiem ocalałych." = "all_pairs",
      "Gdyby powtórzyć rejs, w 1. klasie znów przeżyłoby 62.5%." = "repeat"
    ),
    correct = "assoc",
    feedback = list(
    causal = "Test stwierdza związek, nie przyczynę. Klasa mogła się wiązać
    z przeżyciem także przez to, kto podróżował w danej klasie.",
    assoc = "Test χ² mówi tylko, że klasa i przeżycie nie są niezależne.
    Dlaczego, trzeba sprawdzić osobno.",
    all_pairs = "Istotny wynik oznacza, że nie wszystkie klasy są takie same.
    Załoga i trzecia klasa mają prawie ten sam odsetek ocalałych.",
    `repeat` = "62.5% to wynik tej jednej katastrofy. Test nie przewiduje
    wyniku innego zdarzenia."
    )
  ),

  lc_p("Test χ² stwierdza związek, ale nie jego przyczynę. Co więcej, nie
    każda para klas się różni: załoga (24.0%) i trzecia klasa (25.2%) mają
    prawie ten sam odsetek. Zanim uznamy, że różnice wynikają z samej klasy,
    trzeba zapytać, czym jeszcze różniły się osoby w poszczególnych
    klasach."),

  # ========================================================================
  # KROK 3: Płeć jako zmienna zakłócająca
  # ========================================================================
  lc_h2("ch3-plec", "Krok 3: Kto podróżował w każdej klasie"),

  lc_p("Zasada „kobiety i dzieci najpierw” sprawia, że płeć jest dobrym
    kandydatem na ", gloss("zmienna zakłócająca", "zmienną zakłócającą"),
    ". Musi być powiązana z obiema zmiennymi: z przeżyciem jest
    powiązana bardzo silnie, a z klasą? Kobiety stanowiły 44.6% pierwszej
    klasy, 37.2% drugiej, 27.8% trzeciej i tylko 2.6% załogi. Panel
    pokazuje odsetki ocalałych w klasach osobno dla mężczyzn i kobiet."),

  # Płeć jako wspólne tło klasy i przeżycia (siła: V Cramera)
  figure_panel(
    label = "Ryc. 3.4",
    title = "Płeć jako wspólne tło",
    width_mode = "text",
    case_confounder(
      nodes    = c("Płeć", "Klasa", "Przeżycie"),
      edges    = c("V = 0.40", "V = 0.45", "V = 0.29"),
      strength = c(0.40, 0.45, 0.29)
    )
  ),

  figure_panel(
    label = "Ryc. 3.5",
    title = "Odsetek ocalałych w klasach, osobno dla płci",
    ch3_sex_fig()
  ),

  lc_p("Załoga to prawie sami mężczyźni (862 z 885), a mężczyźni przeżywali
    rzadko w każdej klasie. Niski odsetek ocalałych w załodze wynika więc
    głównie z jej składu. Wśród mężczyzn członkowie załogi przeżywali
    częściej (22.3%) niż pasażerowie drugiej (14.0%) i trzeciej klasy
    (17.3%). Wśród kobiet różnice między klasami są ogromne: z pierwszej
    klasy przeżyło 97.2% kobiet, z drugiej 87.7%, a z trzeciej tylko 45.9%.
    Równe odsetki załogi i trzeciej klasy w danych łącznych kryją więc
    zupełnie różne sytuacje."),

  # ========================================================================
  # KROK 4: Regresja logistyczna
  # ========================================================================
  lc_h2("ch3-model", "Krok 4: Klasa, płeć i wiek naraz"),

  lc_p("Podział na płeć to dopiero początek: w każdej grupie płci klasy
    różnią się jeszcze odsetkiem dzieci. Żeby porównać osoby podobne pod
    wszystkimi trzema względami jednocześnie, używamy regresji logistycznej
    z rozdziału 05 wykładu 07. Jej wyniki podaje się jako ",
    gloss("iloraz szans", "ilorazy szans"), ": ile razy szanse przeżycia
    w danej grupie są większe niż w grupie odniesienia przy tych samych
    wartościach pozostałych zmiennych. Grupą odniesienia jest dorosły
    mężczyzna z pierwszej klasy. Iloraz poniżej 1 oznacza mniejsze szanse."),

  figure_panel(
    label = "Ryc. 3.6",
    title = "Ilorazy szans przeżycia",
    ch3_or_fig()
  ),

  lc_p("W modelu z samą klasą załoga i trzecia klasa mają prawie ten sam
    iloraz szans względem pierwszej klasy (0.19 i 0.20). Po dodaniu płci
    i wieku drogi się rozchodzą: dla załogi iloraz wynosi 0.42 (przedział
    ufności od 0.31 do 0.58), a dla trzeciej klasy 0.17 (od 0.12 do 0.24).
    Mężczyzna z załogi miał więc ponad dwa razy większe szanse niż
    mężczyzna w tym samym wieku z trzeciej klasy. Najsilniejszym
    predyktorem jest płeć: kobieta miała około 11 razy większe szanse
    przeżycia niż mężczyzna z tej samej klasy i w tym samym wieku."),

  case_quiz_ui("ch3_quiz_or",
    title = "Co znaczy iloraz szans 11.25 dla kobiet?",
    question = "W modelu z klasą, płcią i wiekiem iloraz szans dla kobiet
      (względem mężczyzn) wynosi 11.25. Które zdanie jest poprawne?",
    choices = c(
      "Kobieta miała 11.25 razy większe prawdopodobieństwo przeżycia niż mężczyzna." = "prob",
      "Szanse przeżycia kobiety były 11.25 razy większe niż mężczyzny z tej samej klasy i w tym samym wieku." = "odds",
      "Przeżyło o 11.25% więcej kobiet niż mężczyzn." = "pct",
      "W każdej klasie przeżyło 11.25 razy więcej kobiet niż mężczyzn." = "count"
    ),
    correct = "odds",
    feedback = list(
    prob = "Iloraz szans to nie stosunek prawdopodobieństw. Dla dorosłych
    z trzeciej klasy prawdopodobieństwa wynoszą 0.57 i 0.10: ich stosunek
    to 5.7, a stosunek szans około 11.",
    odds = "Iloraz szans porównuje szanse, czyli p/(1 - p), przy tych samych
    wartościach pozostałych zmiennych w modelu.",
    pct = "11.25 to krotność szans, a nie różnica w punktach procentowych.",
    count = "Model nie mówi o liczbie ocalałych, tylko o szansach jednej
    osoby. Do tego zakłada ten sam iloraz we wszystkich klasach, a w danych
    różnice między płciami nie są w każdej klasie takie same."
    )
  ),

  # Iloraz szans jako stosunek dwóch szans
  lc_formula_annotated(
    "⟦1|\\text{OR}⟧ = \\frac{⟦2|p_K⟧ \\, / \\, (1 - p_K)}{⟦3|p_M⟧ \\, / \\, (1 - p_M)}",
    notes  = list("iloraz szans", "prawdopodobieństwo: kobieta", "prawdopodobieństwo: mężczyzna"),
    colors = c("indygo", "bursztyn", "niebo"),
    above  = 2
  ),

  lc_p("Szanse to nie prawdopodobieństwo. Model przewiduje dla dorosłej
    kobiety z trzeciej klasy prawdopodobieństwo przeżycia 0.57, a dla
    dorosłego mężczyzny z trzeciej klasy 0.10. Stosunek prawdopodobieństw
    wynosi 5.7, a stosunek szans (0.57/0.43 do 0.10/0.90) około 11. Przy
    prawdopodobieństwach bliskich 0 lub 1 te dwie liczby rozjeżdżają się
    najbardziej."),

  # ========================================================================
  # WNIOSKI
  # ========================================================================
  lc_h2("ch3-wnioski", "Odpowiedź i ograniczenia"),

  lc_p("Szanse przeżycia zależały od klasy, ale jeszcze silniej od płci
    i wieku. Pierwsza klasa miała największe szanse w każdej grupie,
    a trzecia najmniejsze. Załoga, która w danych łącznych wyglądała jak
    trzecia klasa, po uwzględnieniu płci wypada wyraźnie lepiej. Jej niski
    odsetek ocalałych wynikał z tego, że prawie cała była męska."),

  tags$ul(
    tags$li("To jedno zdarzenie, a nie próba z populacji. Testy i przedziały
      ufności opisują raczej, czy różnice są większe niż te, które mógłby
      wytworzyć przypadek, niż jak przeniosą się na inne katastrofy."),
    tags$li("Dane mają tylko cztery cechy. Nie znamy na przykład położenia
      kabin, które w trzeciej klasie były daleko od pokładu z szalupami,
      ani narodowości i znajomości języka."),
    tags$li("Wiek jest podzielony tylko na dorosłych i dzieci, bez granicy
      wieku w danych."),
    tags$li("Model zakłada, że różnice między klasami są takie same dla
      kobiet i mężczyzn. Ryc. 3.5 pokazuje, że tak nie jest: wśród kobiet
      klasa liczyła się dużo bardziej. Uchwyciłaby to interakcja z rozdziału
      03B wykładu 07.")
  ),

  lc_note("Zasada", rule = TRUE,
    "Równe odsetki w dwóch grupach nie oznaczają równych szans, jeśli grupy
     różnią się składem. Porównuje się grupy podobne pod względem tego, co
     wpływa na wynik.")
))

# ============================================================================
# SERVER
# ============================================================================

# Rozdział nie ma logiki serwera: dane są stałe, więc tabele, odczyty i opisy rycin liczy R
# przy budowaniu strony (modules/figures.R), a przeglądarka rysuje wybrany wariant.
ch3_server <- function(input, output, session) {}
