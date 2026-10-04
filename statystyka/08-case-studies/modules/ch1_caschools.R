# ============================================================================
# CASE STUDY 1: CASchools
# Pytanie: Czy zmniejszenie klas poprawi wyniki uczniów w Kalifornii?
# ============================================================================

# Ładowanie i przygotowanie danych
data("CASchools", package = "AER")
ca <- CASchools
ca$score <- (ca$read + ca$math) / 2
ca$str <- ca$students / ca$teachers
ca$comp_per_student <- ca$computer / ca$students
ca$poverty <- ifelse(ca$lunch >= 50, "Wysoki", ifelse(ca$lunch >= 25, "Średni", "Niski"))
ca$poverty <- factor(ca$poverty, levels = c("Niski", "Średni", "Wysoki"))

ch1_ui <- lecture_chapter(id = "ch1", num = "1", title = "CASchools", content = tagList(

  lc_chapter_hero(
    kicker = "Rozdział 01 · Studium przypadku",
    num    = "01",
    title  = "Mniejsze klasy w Kalifornii.",
    lead   = "Pierwszy wykres obiecuje 2.3 punktu więcej za każdego ucznia mniej
              na nauczyciela. Gdy porównamy okręgi o podobnej zamożności,
              z tej obietnicy zostaje mniej więcej jedna czwarta, i to z dużą
              niepewnością."
  ),

  # ========================================================================
  # KONTEKST: Sytuacja decyzyjna
  # ========================================================================
  lc_h2("sec-01", "Sytuacja wyjściowa"),

  lc_p("Ten wykład nie wprowadza nowych metod. Przechodzi jedną analizę od
    pytania do decyzji i po kolei sięga po narzędzia z wykładów 01–06:
    opis danych, korelację, ANOVA, regresję prostą i wieloraką, ocenę
    reszt i porównanie modeli. Celem jest zobaczyć, jak te narzędzia
    składają się w całość i jak każde z nich zmienia odpowiedź."),

  lc_p("Wyobraźmy sobie, że pracujemy jako analitycy w kalifornijskim
    departamencie edukacji. Polityk proponuje zmniejszenie liczebności
    klas jako sposób na poprawę wyników egzaminacyjnych. Program kosztowałby
    miliardy dolarów, bo mniejsze klasy oznaczają więcej nauczycieli
    i więcej sal. Zanim ktokolwiek wyda te pieniądze, mamy sprawdzić na
    dostępnych danych, czego można się po nim spodziewać. Nasze ",
    gloss("pytanie badawcze"), " brzmi: czy okręgi, w których na jednego
    nauczyciela przypada mniej uczniów, mają lepsze wyniki, a jeśli tak,
    to czy ta różnica wynika z liczby uczniów, czy z czegoś innego, co
    takie okręgi wyróżnia."),

  lc_p("Danymi jest zbiór CASchools, który znamy z ćwiczeń w wykładach 04
    i 06 i który w rozdziale 02 wykładu 07 oceniliśmy jako bardzo dobry.
    Opisuje 420 okręgów szkolnych w Kalifornii w roku szkolnym 1998–1999.
    ", gloss("jednostka obserwacji", "Jednostką obserwacji"), " jest okręg,
    a nie uczeń ani klasa. ", gloss("zmienna zależna", "Zmienną zależną"),
    " będzie średni wynik okręgu, czyli średnia z wyników testu z czytania
    i z matematyki. W wykładzie 06 modelowaliśmy sam wynik z czytania,
    dlatego liczby w tym wykładzie będą podobne, ale nie identyczne."),

  lc_p("Główną zmienną objaśniającą jest STR (ang. ", em_("student–teacher
    ratio"), "): liczba uczniów w okręgu podzielona przez liczbę nauczycieli.
    To przybliżenie warunków nauki, a nie pomiar wielkości konkretnej klasy.
    Okręg ze STR równym 20 może mieć klasy po 25 uczniów i nauczycieli
    wspomagających, którzy nie prowadzą własnej klasy."),

  lc_p("Analizę przeprowadzimy w sześciu krokach:"),

  tags$ol(
    tags$li("Poznanie danych: rozkłady zmiennych i ich wzajemne korelacje (wykłady 01 i 07)."),
    tags$li("Prosty związek STR z wynikami: test korelacji i regresja prosta (wykłady 04 i 06)."),
    tags$li("Sprawdzenie, czy ubóstwo jest ", gloss("zmienna zakłócająca", "zmienną zakłócającą"),
            " (wykład 04, w tym ANOVA)."),
    tags$li("Regresja wieloraka: czy związek STR z wynikami przetrwa kontrolę innych zmiennych (wykład 06)."),
    tags$li("Analiza wybranego modelu: współczynniki, przedziały ufności, reszty (wykłady 05 i 06)."),
    tags$li("Odpowiedź na pytanie decyzyjne i ograniczenia analizy.")
  ),

  # ========================================================================
  # KROK 1: Poznanie danych
  # ========================================================================
  lc_h2("sec-02", "Krok 1: Poznanie danych"),

  lc_p(
    "Zanim policzymy cokolwiek o związkach, sprawdzamy, jakie wartości
     przyjmują zmienne i czy nie kryją niespodzianek."
  ),

  lc_p("Tak jak w wykładzie 01, każdą zmienną oglądamy najpierw osobno:
    histogram pokazuje kształt rozkładu, a wykres pudełkowy medianę,
    rozstęp międzykwartylowy i wartości odstające. Pod wykresem są
    podstawowe statystyki opisowe wybranej zmiennej."),

  figure_panel(
    label = "Ryc. 1.1",
    title = "Przegląd zmiennych",
    lc_toolbar(
      selectInput("ch1_eda_var", "Zmienna",
          choices = c(
            "Średni wynik testu (pkt)" = "score",
            "Uczniowie na nauczyciela (STR)" = "str",
            "Wydatki na ucznia (USD)" = "expenditure",
            "Dochód okręgu (tys. USD)" = "income",
            "Uczniowie uczący się angielskiego (%)" = "english",
            "Uczniowie z dotacją do obiadu (%)" = "lunch",
            "Uczniowie z rodzin na zasiłku CalWORKs (%)" = "calworks"
          ),
          selected = "score"
          ),
          lc_readouts(uiOutput("ch1_eda_stats"))
          ),
          lc_plot("ch1_eda_plot", max_height = "280px")
  ),

  lc_p("Średni wynik okręgu wynosi 654.2 pkt przy odchyleniu standardowym
    19.1 pkt, a rozkład jest zbliżony do symetrycznego (od 605.6 do 706.8
    pkt). STR waha się od 14.0 do 25.8 ucznia na nauczyciela, ale większość
    okręgów mieści się w wąskim pasie: połowa ma STR między 18.6 a 20.9.
    Ta wąskość ma znaczenie dla całej analizy. Dane mówią o różnicach
    rzędu dwóch, trzech uczniów na nauczyciela, a nie o zmniejszeniu klas
    o połowę."),

  lc_p("W zbiorze są też trzy zmienne opisujące zamożność rodzin: średni
    dochód w okręgu, odsetek uczniów z dotacją do obiadu i odsetek uczniów
    z rodzin na zasiłku CalWORKs. Do dotacji kwalifikują się dzieci z rodzin
    o niskich dochodach, więc wysoki odsetek dotacji oznacza biedniejszy
    okręg. Odsetek uczniów uczących się angielskiego mówi, ile dzieci
    dopiero poznaje język, w którym pisze test. Jeśli te cechy wiążą się
    jednocześnie z wynikami i z liczbą uczniów na nauczyciela, mogą
    wytworzyć związek STR z wynikami, którego w rzeczywistości nie ma.
    Żeby to ocenić, potrzebujemy korelacji wszystkich par zmiennych."),

  figure_panel(
    label = "Ryc. 1.2",
    title = "Macierz korelacji",
    lc_plot("ch1_corr_plot", max_height = "400px")
  ),

  lc_p("Wiersz wyniku pokazuje, że zmienne opisujące zamożność są z nim
    związane bardzo silnie: odsetek uczniów z dotacją do obiadu
    \\(r = -0.87\\), dochód \\(r = 0.71\\), odsetek uczniów uczących się
    angielskiego \\(r = -0.64\\), odsetek rodzin na zasiłku \\(r = -0.63\\).
    STR koreluje z wynikiem znacznie słabiej (\\(r = -0.23\\)). Wiersz STR
    jest jednak ważniejszy dla naszego pytania. Ze STR najsilniej wiążą się
    wydatki na ucznia (\\(r = -0.62\\)), co łatwo zrozumieć: więcej
    nauczycieli na tę samą liczbę uczniów to więcej pensji. Z dochodem
    (\\(r = -0.23\\)), odsetkiem uczniów uczących się angielskiego (0.19)
    i odsetkiem dotacji (0.14) STR wiąże się słabo, ale w kierunku,
    w którym zakłócenie jest możliwe: okręgi biedniejsze mają przeciętnie
    trochę więcej uczniów na nauczyciela."),

  # ========================================================================
  # KROK 2: Prosty związek STR -> wyniki
  # ========================================================================
  lc_h2("sec-03", "Krok 2: Prosty związek STR z wynikami"),

  lc_p(
    "Sprawdzamy związek, który zobaczyłby polityk, gdyby spojrzał tylko na
     dwie zmienne. To punkt odniesienia dla dalszych kroków."
  ),

  lc_p("Macierz korelacji opisuje próbę. Żeby powiedzieć coś o zależności
    ogólnej, potrzebny jest test. Jak w rozdziale 06 wykładu 04 sprawdzamy
    hipotezę, że w populacji współczynnik ", gloss("korelacja", "korelacji"),
    " \\(\\rho\\) między STR a wynikiem wynosi zero:"),

  lc_formula_box(withMathJax(
    "$$H_0: \\rho = 0 \\qquad H_a: \\rho \\neq 0$$"
  )),

  lc_p("Siłę związku w jednostkach wyniku opisuje ",
    gloss("regresja prosta", "regresja prosta"), " z wykładu 06. Jej
    nachylenie mówi, o ile punktów średnio różnią się wyniki okręgów, w których
    STR różni się o jednego ucznia na nauczyciela. Panel pokazuje chmurę
    okręgów z prostą regresji, a obok wynik testu korelacji i równanie
    prostej."),

  figure_panel(
    label = "Ryc. 1.3",
    title = "Wynik a liczba uczniów na nauczyciela",
    lc_toolbar(
      checkboxInput("ch1_str_color", "Koloruj wg poziomu ubóstwa", value = FALSE)
    ),
    lc_plot("ch1_str_plot", max_height = "380px"),
    uiOutput("ch1_str_test")
  ),

  lc_p("Korelacja wynosi \\(r = -0.23\\) (95% przedział ufności od -0.32
    do -0.13, p < 0.001), więc hipotezę zerową odrzucamy. Prosta regresji
    ma nachylenie -2.28: okręg, w którym na nauczyciela przypada o jednego
    ucznia więcej, ma średnio wynik niższy o 2.28 pkt. To ta sama
    zależność, którą w zadaniu 5 z wykładu 04 i w rozdziale 03 wykładu 06
    widzieliśmy dla samego czytania (\\(r = -0.25\\), nachylenie -2.62).
    Dla matematyki nachylenie jest łagodniejsze (-1.94), więc dla średniej
    z obu testów wychodzi wartość pośrednia."),

  lc_p("Na tym etapie polityk mógłby powiedzieć, że mniejsze klasy dają
    lepsze wyniki, i poprosić o budżet. Zauważmy jednak, że STR wyjaśnia
    tylko 5.1% zmienności wyników, a związek jest ",
    gloss("dane obserwacyjne", "obserwacyjny"), ": okręgi same ustalały,
    ilu zatrudnić nauczycieli. Po włączeniu kolorowania według poziomu
    ubóstwa widać, że bordowe punkty, czyli okręgi, w których co najmniej
    połowa uczniów ma dotację do obiadu, leżą przeważnie w dolnej części
    wykresu. Zanim uznamy nachylenie -2.28 za efekt liczby uczniów, trzeba
    sprawdzić, czy nie jest to ",
    gloss("korelacja pozorna", "korelacja pozorna"), " wytworzona przez
    ubóstwo."),

  # ========================================================================
  # KROK 3: Ubóstwo jako zmienna zakłócająca
  # ========================================================================
  lc_h2("sec-04", "Krok 3: Ubóstwo jako zmienna zakłócająca"),

  lc_p(
    "Sprawdzamy, czy za liczbą uczniów na nauczyciela i za wynikami nie
     kryje się wspólne tło: sytuacja materialna rodzin."
  ),

  lc_p("Z wykładu 04 wiemy, że zmienna zakłócająca musi być powiązana z obiema
    zmiennymi, których związek badamy. Ubóstwo mierzymy odsetkiem uczniów
    z dotacją do obiadu. Ten wskaźnik wymaga ostrożnej lektury. Więcej
    dotacji idzie w parze z niższymi wynikami, ale nie dlatego, że obiady
    szkodzą nauce. Dotacje trafiają tam, gdzie rodzinom jest trudniej, więc
    odsetek dotacji pokazuje, kto potrzebuje wsparcia. Panel zestawia oba
    potrzebne związki: odsetka dotacji z wynikiem i odsetka dotacji ze STR."),

  figure_panel(
    label = "Ryc. 1.4",
    title = "Dotacje do obiadów, wyniki i liczba uczniów na nauczyciela",
    lc_readouts(uiOutput("ch1_conf_stats")),
    lc_plots(
      lc_plot("ch1_conf_a", max_height = "280px"),
      lc_plot("ch1_conf_b", max_height = "280px")
    )
  ),

  lc_p("Pierwszy związek jest bardzo silny (\\(r = -0.87\\)): odsetek dotacji
    sam przewiduje wynik lepiej niż jakakolwiek inna zmienna w zbiorze.
    Drugi jest słaby (\\(r = 0.14\\)): okręgi biedniejsze mają tylko trochę
    więcej uczniów na nauczyciela. Ubóstwo spełnia więc oba warunki
    zmiennej zakłócającej, ale drugi w niewielkim stopniu. Podobnie
    wyszło w zadaniu 9 z rozdziału 07 wykładu 04: po podziale obu zmiennych
    na dwie kategorie test χ² nie wykazał istotnego związku STR
    z ubóstwem.
    Ubóstwo może zatem tłumaczyć część korelacji STR z wynikami, ale
    raczej nie całą."),

  lc_p("Skalę różnic między okręgami biednymi i zamożnymi najłatwiej zobaczyć,
    dzieląc okręgi na trzy grupy według odsetka dotacji: poniżej 25%
    (niski poziom ubóstwa), od 25% do 50% (średni) i co najmniej 50%
    (wysoki). Średnie wyniki trzech grup porównuje ",
    gloss("ANOVA"), " z rozdziału 09 wykładu 04, a ",
    gloss("test post hoc", "test post hoc"), " Tukeya wskazuje, które pary
    grup się różnią."),

  figure_panel(
    label = "Ryc. 1.5",
    title = "Wyniki w grupach ubóstwa",
    lc_plot("ch1_anova_plot", max_height = "320px"),
    uiOutput("ch1_anova_result")
  ),

  lc_p("Średnie wynoszą 674.4 pkt w okręgach o niskim ubóstwie, 658.3 pkt
    w okręgach o średnim i 638.4 pkt w okręgach o wysokim. Wszystkie trzy
    pary różnią się istotnie, a różnica między skrajnymi grupami to 36.1 pkt,
    prawie dwa odchylenia standardowe wyniku. ",
    gloss("eta kwadrat", "η²"), " = 0.61: sam podział na trzy grupy
    ubóstwa wyjaśnia 61% zmienności wyników, podczas gdy STR w regresji
    prostej wyjaśniał 5%. Dla porównania: nachylenie z kroku 2 przewiduje
    między okręgiem z 10% najniższych STR (17.3) a okręgiem z 10%
    najwyższych (21.9) różnicę około 10 pkt."),

  lc_p("Średni STR w trzech grupach jest prawie taki sam (19.2, 19.9 i 19.8),
    a wewnątrz grup związek STR z wynikiem nadal jest ujemny w okręgach
    o niskim ubóstwie (nachylenie -2.71) i o wysokim (-1.39), choć
    w grupie środkowej prawie znika (-0.43, nieistotne). Podział na trzy
    grupy jest jednak zgrubny: w każdej z nich okręgi wciąż różnią się
    dochodem i odsetkiem uczniów uczących się angielskiego. Żeby porównać
    okręgi podobne pod wieloma względami jednocześnie, potrzebna jest
    regresja wieloraka."),

  lc_note("Zasada", rule = TRUE,
    "Zanim przypiszesz różnicę w wynikach jednej zmiennej, zapytaj, czym
     jeszcze różnią się porównywane grupy."),

  # ========================================================================
  # KROK 4: Kontrolowanie zakłóceń (regresja wieloraka)
  # ========================================================================
  lc_h2("sec-05", "Krok 4: Związek STR z wynikami po kontroli innych zmiennych"),

  lc_p(
    "Dokładamy do modelu kolejne ", gloss("zmienna kontrolna", "zmienne kontrolne"),
    " i patrzymy, co dzieje się ze ", gloss("współczynnik regresji", "współczynnikiem"),
    " przy STR."
  ),

  lc_p("W ", gloss("regresja wieloraka", "regresji wielorakiej"), " z rozdziału
    03 wykładu 06 współczynnik przy STR odpowiada na inne pytanie niż
    w regresji prostej: o ile różnią się średnio wyniki okręgów, które mają
    różny STR, ale te same wartości pozostałych ",
    gloss("predyktor", "predyktorów"), ". W najszerszym modelu tego kroku:"),

  lc_formula_box(withMathJax(
    "$$\\text{wynik} = \\beta_0 + \\beta_1 \\, \\text{STR} + \\beta_2 \\, \\text{dochód} + \\beta_3 \\, \\text{angielski} + \\beta_4 \\, \\text{dotacje} + \\varepsilon$$"
  )),

  lc_p("Panel buduje cztery modele, za każdym razem dokładając jedną zmienną:
    sam STR, potem dochód, odsetek uczniów uczących się angielskiego
    i odsetek dotacji do obiadu. Tabela podaje współczynnik przy STR,
    jego p-wartość oraz dwie miary porównawcze z rozdziału 04 wykładu 06:
    ", gloss("skorygowany R²"), " i ", gloss("AIC"), ". Wykres pod tabelą
    pokazuje współczynnik STR w kolejnych modelach."),

  figure_panel(
    label = "Ryc. 1.6",
    title = "Seria modeli i współczynnik przy STR",
    lc_toolbar(lc_action("ch1_compare_models", "Buduj 4 modele", variant = "solid")),
    uiOutput("ch1_model_comparison"),
    lc_plot("ch1_beta_str_plot", max_height = "250px")
  ),

  lc_p("Po dodaniu dochodu współczynnik przy STR spada z -2.28 do -0.65
    i przestaje być istotny (p = 0.07). To ten sam ruch, który rozdział 03
    wykładu 06 pokazał dla czytania, gdzie współczynnik spadł z -2.62 do
    -0.95. Dochód tłumaczy większą część prostego związku: zamożniejsze
    okręgi mają i nieco mniej uczniów na nauczyciela, i wyraźnie lepsze
    wyniki. Po dodaniu odsetka uczniów uczących się angielskiego
    współczynnik przy STR prawie znika (-0.07, p = 0.80)."),

  lc_p("Czwarty model przynosi niespodziankę. Po dodaniu odsetka dotacji
    współczynnik przy STR wraca do -0.56 i znów jest istotny (p = 0.015).
    Dzieje się tak, bo w modelach 2 i 3 dochód wchodzi do równania liniowo,
    choć jego związek z wynikiem jest zakrzywiony: przy wyższych dochodach
    wyniki rosną wolniej. W rozdziale 02 wykładu 06 było to widać jako łuk
    w resztach. Odsetek dotacji dokłada informację o ubóstwie, której
    liniowy dochód nie oddaje, i dopiero wtedy porównanie okręgów
    o podobnym STR staje się porównaniem okręgów o naprawdę podobnej
    sytuacji. W wykładzie 06 ten sam czteroczynnikowy model dla czytania
    dawał współczynnik -0.78. Dla średniej z czytania i matematyki wychodzi
    -0.56, bo dla samej matematyki jest on mniejszy i nieistotny (-0.34)."),

  lc_p("Który z tych modeli wybrać? Miary z rozdziału 04 wykładu 06 wskazują
    jednoznacznie model 4: skorygowany R² rośnie od 0.049 przez 0.509
    i 0.705 do 0.803, a AIC spada od 3650 przez 3374 i 3161 do 2991.
    Różnica AIC między modelami 3 i 4 wynosi 170, więc dane wyraźnie
    odróżniają te modele. Seria pokazuje też coś ważniejszego niż sam
    wybór: współczynnik przy STR zmienia się od -0.07 do -0.65 w zależności
    od tego, jakie zmienne kontrolujemy. Po uwzględnieniu zamożności
    okręgu jest kilka razy mniejszy niż w regresji prostej, a jego dokładna
    wartość zależy od specyfikacji modelu."),

  # ========================================================================
  # KROK 5: Wybrany model — szczegóły
  # ========================================================================
  lc_h2("sec-06", "Krok 5: Analiza wybranego modelu"),

  lc_p(
    "Przyglądamy się wybranemu modelowi w całości: wszystkim współczynnikom,
     ich przedziałom ufności i jakości dopasowania."
  ),

  lc_p("Seria z kroku 4 pokazywała tylko współczynnik przy STR. Teraz
    oglądamy pełną tabelę: dla każdego predyktora współczynnik, ",
    gloss("błąd standardowy"), " i p-wartość, a na wykresie 95% ",
    gloss("przedział ufności", "przedziały ufności"), ". Panel startuje od
    modelu 4 z kroku 4 (STR, dochód, odsetek uczniów uczących się
    angielskiego i odsetek dotacji do obiadu), najlepszego według
    skorygowanego R² i AIC, i pozwala dodawać lub usuwać predyktory. Pod listą
    predyktorów są skorygowany R², AIC i ", gloss("RMSE"), " dopasowanego
    modelu."),

  figure_panel(
    label = "Ryc. 1.7",
    title = "Model wieloraki — wybór predyktorów",
    lc_toolbar(
      checkboxGroupInput("ch1_reg_vars", "Predyktory", inline = TRUE,
          choices = c(
            "Uczniowie na nauczyciela (STR)" = "str",
            "Dochód okręgu (tys. USD)" = "income",
            "Uczniowie uczący się angielskiego (%)" = "english",
            "Uczniowie z dotacją do obiadu (%)" = "lunch",
            "Wydatki na ucznia (USD)" = "expenditure"
          ),
          selected = c("str", "income", "english", "lunch")
          ),
          lc_action("ch1_fit_model", "Dopasuj", variant = "solid"),
          lc_readouts(uiOutput("ch1_reg_metrics"))
          ),
          uiOutput("ch1_reg_coefs"),
          lc_plot("ch1_reg_coef_plot", max_height = "230px")
  ),

  lc_p("W modelu startowym przedział ufności dla STR rozciąga się od -1.01
    do -0.11, a RMSE wynosi 8.4 pkt. Odsetek dotacji ma współczynnik -0.40
    pkt na punkt procentowy, dochód 0.68 pkt na tysiąc dolarów, a odsetek
    uczniów uczących się angielskiego -0.19 pkt na punkt procentowy. Po
    usunięciu dotacji (model 3) przedział dla STR rozszerza się na od -0.61
    do 0.48 i obejmuje zero, RMSE rośnie do 10.3 pkt, a współczynniki
    pozostałych zmiennych rosną: dochodu do 1.49, angielskiego do -0.49. Dochód
    i dotacje mierzą częściowo to samo (\\(r = -0.68\\)), ale ",
    gloss("współliniowość"), " jest umiarkowana: największy ",
    gloss("VIF"), " w modelu 4 ma odsetek dotacji (3.2). Oba współczynniki
    mają wąskie przedziały ufności, więc model potrafi rozdzielić ich
    udziały."),

  lc_p("Ciekawym sprawdzianem jest dodanie wydatków na ucznia do modelu 4.
    Współczynnik przy wydatkach wynosi wtedy około 1.6 pkt na każde
    1000 USD i jest nieistotny (p = 0.07), a współczynnik przy STR maleje
    do -0.26 i też traci istotność. Nie znaczy to, że oba czynniki nie
    mają znaczenia. Wydatki i STR są silnie powiązane (\\(r = -0.62\\)),
    bo mniej uczniów na nauczyciela oznacza więcej pensji na ucznia. Model
    z obiema zmiennymi porównuje okręgi o różnym STR, ale tych samych
    wydatkach, a to inne pytanie niż to, które zadaje polityk. Jego program
    zmienia oba czynniki naraz."),

  lc_p("Zanim zaufamy przedziałom ufności, sprawdzamy reszty, tak jak
    w wykładzie 05 i w rozdziale 02 wykładu 06. Panel ich nie pokazuje,
    ale w modelu 4 nie układają się w łuk względem dochodu, jak w modelu 3,
    a ich rozrzut jest podobny dla okręgów o niskich i wysokich
    przewidywanych wynikach. Test Shapiro-Wilka wykrywa niewielkie
    odchylenie od rozkładu normalnego (p = 0.03), ale przy 420 okręgach
    tak małe odchylenie nie zagraża wnioskom o współczynnikach."),

  # ========================================================================
  # KROK 6: Odpowiedź na pytanie decyzyjne
  # ========================================================================
  lc_h2("sec-07", "Krok 6: Odpowiedź na pytanie decyzyjne"),

  lc_p(
    "Wracamy do pytania polityka: czego można się spodziewać po zmniejszeniu
     liczby uczniów na nauczyciela?"
  ),

  lc_p("Prosty związek istnieje: okręgi z mniejszą liczbą uczniów na
    nauczyciela mają lepsze wyniki (\\(r = -0.23\\), nachylenie -2.28 pkt
    na ucznia). Większa część tego związku wynika jednak z tego, że okręgi
    z mniejszą liczbą uczniów na nauczyciela są przeciętnie zamożniejsze
    i mają mniej dzieci uczących się angielskiego. W najlepszym z modeli,
    po kontroli dochodu, odsetka uczniów uczących się angielskiego i odsetka
    dotacji, współczynnik przy STR wynosi -0.56 pkt, z 95% przedziałem
    ufności od -1.01 do -0.11. To mniej więcej jedna czwarta nachylenia
    z regresji prostej."),

  lc_p("W jednostkach praktycznych: zmniejszenie liczby uczniów na nauczyciela
    o dwóch, na przykład z 20 do 18, oznacza zatrudnienie około 11% więcej
    nauczycieli. Model 4 wiąże taką różnicę z wynikiem wyższym średnio
    o 1.1 pkt (przedział od 0.2 do 2.0 pkt). To około 0.06 odchylenia
    standardowego wyniku i ułamek różnicy między okręgami o niskim
    i wysokim ubóstwie (36.1 pkt). Ponadto sama wartość współczynnika
    zależy od wyboru zmiennych kontrolnych. W modelach, które można zbudować
    w panelu Ryc. 1.7 i które uwzględniają dochód albo odsetek dotacji,
    a pomijają wydatki, współczynnik przy STR leży między -0.07 a -1.12."),

  lc_p("Decydentowi można więc uczciwie powiedzieć tyle: pierwszy wykres
    przecenia korzyść z mniejszych klas kilkakrotnie. Po uwzględnieniu
    sytuacji uczniów zostaje mały związek, raczej ujemny, ale za słabo
    określony, żeby obiecać konkretny wzrost wyników za konkretną kwotę.
    Znacznie silniej z wynikami wiąże się sytuacja materialna rodzin i to,
    czy dzieci znają język testu. Z tego, że więcej dotacji idzie w parze
    z gorszymi wynikami, nie wynika, że dotacje szkodzą, a z małego
    współczynnika przy wydatkach nie wynika, że pieniądze nie pomagają.
    W obu przypadkach dane obserwacyjne pokazują, gdzie trafia pomoc,
    a nie, co ona daje."),

  lc_p("Analiza ma kilka ograniczeń, które trzeba wymienić razem z wynikiem."),

  tags$ul(
    tags$li(gloss("dane obserwacyjne", "Dane obserwacyjne"), ", nie ",
      gloss("dane eksperymentalne", "eksperymentalne"), ". Nie możemy orzekać
      o ", gloss("przyczynowość", "przyczynowości"), ". Mogą istnieć ",
      gloss("zmienna pominięta", "zmienne pominięte"), ", na przykład
      doświadczenie nauczycieli, związane i ze STR, i z wynikami."),
    tags$li("Jednostką obserwacji jest okręg. Wnioski dotyczą okręgów, a nie
      pojedynczych klas ani uczniów, a STR to średnia dla całego okręgu,
      a nie liczebność konkretnej klasy."),
    tags$li("Specyfikacja modelu ma znaczenie. Współczynnik przy STR zmienia
      się w zależności od zestawu zmiennych kontrolnych i od tego, czy
      uwzględnimy zakrzywiony związek z dochodem."),
    tags$li(gloss("dane przekrojowe", "Dane przekrojowe"), ", nie ",
      gloss("dane podłużne", "podłużne"), ". Widzimy jeden rok szkolny,
      a nie zmiany w czasie, więc nie wiemy, czy okręgi, które zmniejszyły
      klasy, poprawiły potem wyniki."),
    tags$li("Zakres danych jest wąski. To Kalifornia z lat 1998–1999, a większość
      okręgów ma STR między 17 a 22. O dużo mniejszych klasach ani o innych
      systemach szkolnych dane nic nie mówią.")
  ),

  lc_p("Żeby odpowiedzieć na pytanie przyczynowe, potrzebny byłby eksperyment,
    w którym uczniów losowo przydziela się do klas mniejszych i większych.
    Takie badanie przeprowadzono w Tennessee w latach 80. (projekt STAR).
    Analiza danych obserwacyjnych, nawet staranna, odpowiada na skromniejsze
    pytanie: jak duża różnica w wynikach zostaje po uwzględnieniu tego, co
    udało się zmierzyć.")


  )
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  # --- Krok 1: EDA ---
  zoom_plot_server("ch1_eda_plot", reactive({
    var <- input$ch1_eda_var
    var_label <- switch(var,
      "score" = "Średni wynik testu (pkt)", "str" = "Uczniowie na nauczyciela",
      "expenditure" = "Wydatki na ucznia (USD)", "income" = "Dochód okręgu (tys. USD)",
      "english" = "Uczący się angielskiego (%)", "lunch" = "Z dotacją do obiadu (%)",
      "calworks" = "Z rodzin na zasiłku (%)")

    p1 <- ggplot(ca, aes(x = .data[[var]])) +
      geom_histogram(bins = 30, fill = case_explore, alpha = 0.6, color = "white") +
      labs( x = var_label, y = "Liczba okręgów") +
      theme_upwr()

    p2 <- ggplot(ca, aes(y = .data[[var]])) +
      geom_boxplot(fill = case_explore, alpha = 0.4) +
      labs(y = var_label) +
      theme_upwr()

    gridExtra::arrangeGrob(p1, p2, ncol = 2, widths = c(2, 1))
  }))

  output$ch1_eda_stats <- renderUI({
    var <- input$ch1_eda_var
    x <- ca[[var]]
    tagList(
      lc_readout("n", length(x), color = case_explore),
      lc_readout("Średnia", lc_fmt(mean(x), 1), color = case_reference),
      lc_readout("SD", lc_fmt(sd(x), 1), color = case_reference),
      lc_readout("Zakres", paste0(lc_fmt(min(x), 1), "–", lc_fmt(max(x), 1)), color = case_reference)
    )
  })

  zoom_plot_server("ch1_corr_plot", reactive({
    vars <- c("score", "str", "expenditure", "income", "english", "lunch", "calworks")
    cor_mat <- cor(ca[, vars], use = "complete.obs")

    cor_df <- as.data.frame(as.table(cor_mat))
    names(cor_df) <- c("Var1", "Var2", "value")

    labels_pl <- c(
      "score" = "Wynik", "str" = "STR", "expenditure" = "Wydatki",
      "income" = "Dochód", "english" = "Angielski (%)", "lunch" = "Dotacje (%)",
      "calworks" = "CalWORKs (%)")
    cor_df$Var1 <- labels_pl[as.character(cor_df$Var1)]
    cor_df$Var2 <- labels_pl[as.character(cor_df$Var2)]

    ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(value, 2)), size = 3.5) +
      scale_fill_gradient2(low = case_highlight, mid = "white", high = case_explore,
                           midpoint = 0, limits = c(-1, 1), name = "r") +
      labs(
           x = NULL, y = NULL) +
      theme_upwr() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }))

  # --- Krok 2: STR vs wyniki ---
  zoom_plot_server("ch1_str_plot", reactive({
    p <- ggplot(ca, aes(x = str, y = score))

    if (input$ch1_str_color) {
      p <- p + geom_point(aes(color = poverty), alpha = 0.6, size = 2) +
        scale_color_manual(values = c(case_explore, case_conclude, case_highlight),
                           name = "Poziom ubóstwa")
    } else {
      p <- p + geom_point(color = case_reference, alpha = 0.4, size = 2)
    }

    p + geom_smooth(method = "lm", se = TRUE,
                    color = case_model, fill = case_model, alpha = 0.1) +
      labs(
           
           x = "Uczniowie na nauczyciela (STR)",
           y = "Średni wynik testu (pkt)") +
      theme_upwr()
  }))

  output$ch1_str_test <- renderUI({
    cor_res <- rstatix::cor_test(ca, str, score, method = "pearson")
    tidy_cor <- as.data.frame(cor_res)

    model <- lm(score ~ str, data = ca)
    coefs <- broom::tidy(model)
    g <- broom::glance(model)

    lc_status(
      p(tags$strong("Korelacja Pearsona:"), paste0(" r = ", round(tidy_cor$cor, 3),
                 ", p ", if (tidy_cor$p < 0.001) "< 0.001" else paste0("= ", round(tidy_cor$p, 4)))),
                   p(tags$strong("Regresja prosta:"), paste0(" wynik = ", round(coefs$estimate[1], 1),
                 if (coefs$estimate[2] < 0) " - " else " + ",
                 abs(round(coefs$estimate[2], 2)), " × STR")),
        p(paste0("R² = ", round(g$r.squared, 3),
                 " (STR wyjaśnia ", round(g$r.squared * 100, 1), "% zmienności wyników)")),
        p(paste0("Uczeń więcej na nauczyciela: wynik niższy średnio o ",
                 abs(round(coefs$estimate[2], 2)), " pkt"))
                 )
  })

  # --- Krok 3: Zmienne zakłócające ---
  zoom_plot_server("ch1_conf_a", reactive({
    ggplot(ca, aes(x = lunch, y = score)) +
      geom_point(color = case_reference, alpha = 0.3, size = 1.5) +
      geom_smooth(method = "lm", se = FALSE, color = case_highlight, linewidth = 1.2) +
      labs(
           
           x = "Uczniowie z dotacją do obiadu (%)", y = "Średni wynik testu (pkt)") +
      theme_upwr()
  }))

  zoom_plot_server("ch1_conf_b", reactive({
    ggplot(ca, aes(x = lunch, y = str)) +
      geom_point(color = case_reference, alpha = 0.3, size = 1.5) +
      geom_smooth(method = "lm", se = FALSE, color = case_test, linewidth = 1.2) +
      labs(
           
           x = "Uczniowie z dotacją do obiadu (%)", y = "Uczniowie na nauczyciela (STR)") +
      theme_upwr()
  }))

  output$ch1_conf_stats <- renderUI({
    r_lunch_score <- cor(ca$lunch, ca$score)
    r_lunch_str <- cor(ca$lunch, ca$str)
    tagList(
      lc_readout("r: dotacje a wynik", lc_fmt(r_lunch_score, 3), color = case_highlight),
      lc_readout("r: dotacje a STR", lc_fmt(r_lunch_str, 3), color = case_test)
    )
  })

  # ANOVA
  zoom_plot_server("ch1_anova_plot", reactive({
    means <- ca %>% group_by(poverty) %>%
      summarise(m = mean(score), .groups = "drop")

    ggplot(ca, aes(x = poverty, y = score, fill = poverty)) +
      geom_boxplot(alpha = 0.6, outlier.alpha = 0.2) +
      geom_jitter(width = 0.15, alpha = 0.1, size = 1) +
      scale_fill_manual(values = c(case_explore, case_conclude, case_highlight)) +
      labs(
           x = "Poziom ubóstwa (odsetek uczniów z dotacją do obiadu)",
           y = "Średni wynik testu (pkt)") +
      theme_upwr() +
      theme(legend.position = "none")
  }))

  output$ch1_anova_result <- renderUI({
    result <- rstatix::anova_test(ca, score ~ poverty)
    tidy_res <- as.data.frame(result)

    tukey <- rstatix::tukey_hsd(ca, score ~ poverty)
    tukey_df <- as.data.frame(tukey)

    means <- ca %>% group_by(poverty) %>%
      summarise(m = round(mean(score), 1), n = n(), .groups = "drop")

    lc_status(
      p(tags$strong("Średnie w grupach:")),
        lapply(1:nrow(means), function(i) {
          p(paste0(means$poverty[i], ": ", means$m[i], " pkt (n = ", means$n[i], ")"))
        }),
          p(tags$strong("ANOVA:")),
        p(paste0("F(", tidy_res$DFn, ",", tidy_res$DFd, ") = ",
                 round(tidy_res$F, 1),
                 ", p < 0.001, η² = ", round(tidy_res$ges, 3))),
                 ", p < 0.001, η² = ",   p(tags$strong("Tukey HSD:")),
        tags$ul(lapply(1:nrow(tukey_df), function(i) {
          tags$li(paste0(tukey_df$group1[i], " vs ", tukey_df$group2[i],
                         ": Δ = ", round(tukey_df$estimate[i], 1),
                         " pkt, p.adj ", if (tukey_df$p.adj[i] < 0.001) "< 0.001"
                         else paste0("= ", round(tukey_df$p.adj[i], 3))))
                           }))
                         )
  })

  # --- Krok 4: Seria modeli ---
  ch1_models_data <- reactiveVal(NULL)

  observeEvent(input$ch1_compare_models, {
    m1 <- lm(score ~ str, data = ca)
    m2 <- lm(score ~ str + income, data = ca)
    m3 <- lm(score ~ str + income + english, data = ca)
    m4 <- lm(score ~ str + income + english + lunch, data = ca)

    models <- list(m1, m2, m3, m4)
    labels <- c("1: sam STR", "2: + dochód", "3: + dochód + angielski",
                "4: + dochód + angielski + dotacje")

    results <- lapply(seq_along(models), function(i) {
      m <- models[[i]]
      g <- broom::glance(m)
      coefs <- broom::tidy(m)
      beta_str <- coefs$estimate[coefs$term == "str"]
      p_str <- coefs$p.value[coefs$term == "str"]
      data.frame(
        model = labels[i], r2 = g$r.squared, adj_r2 = g$adj.r.squared,
        aic = g$AIC, rmse = sqrt(mean(residuals(m)^2)),
        beta_str = beta_str, p_str = p_str
      )
    })

    ch1_models_data(do.call(rbind, results))
  })

  output$ch1_model_comparison <- renderUI({
    df <- ch1_models_data()
    if (is.null(df)) return(NULL)

    tagList(
      # Model z nieistotnym β przy STR jest wyszarzony.
      lc_table(
        data.frame(model = df$model, beta = df$beta_str, p = lc_pval(df$p_str),
                   r2 = df$adj_r2, aic = df$aic),
        cols = list(
          lc_col("model", "Model", "row"),
          lc_col("beta", "β STR", digits = 2),
          lc_col("p", "p (STR)"),
          lc_col("r2", "R² skor.", digits = 3),
          lc_col("aic", "AIC", digits = 0)
        ),
        row_class = ifelse(df$p_str < 0.05, NA_character_, "is-dim"),
        label = "Seria modeli"
      ),
      lc_caption(paste0(
        "β przy STR: ", round(df$beta_str[1], 2), " w modelu 1, ",
        round(df$beta_str[4], 2), " w modelu 4, czyli ",
        round(abs(df$beta_str[4]) / abs(df$beta_str[1]) * 100), "% pierwotnej wartości."
      ))
    )
  })

  zoom_plot_server("ch1_beta_str_plot", reactive({
    df <- ch1_models_data()
    if (is.null(df)) return(NULL)

    df$model <- factor(df$model, levels = df$model)
    df$sig <- df$p_str < 0.05

    ggplot(df, aes(x = model, y = beta_str, fill = sig)) +
      geom_col(alpha = 0.8, width = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_fill_manual(values = c("TRUE" = case_model, "FALSE" = case_muted),
                        labels = c("TRUE" = "p < 0.05", "FALSE" = "nieistotny"),
                        name = NULL) +
      labs(
           x = NULL, y = "β przy STR") +
      theme_upwr() +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 20, hjust = 1))
  }))

  # --- Krok 5: Model interaktywny ---
  ch1_model <- reactiveVal(NULL)

  observeEvent(input$ch1_fit_model, {
    preds <- input$ch1_reg_vars
    if (length(preds) == 0) preds <- "str"
    formula <- as.formula(paste("score ~", paste(preds, collapse = " + ")))
    ch1_model(lm(formula, data = ca))
  })

  output$ch1_reg_coefs <- renderUI({
    model <- ch1_model()
    if (is.null(model)) return(NULL)

    coefs <- broom::tidy(model)
    labels_pl <- c(
      "(Intercept)" = "Wyraz wolny", "str" = "STR",
      "income" = "Dochód (tys. USD)", "english" = "Angielski (%)",
      "expenditure" = "Wydatki (USD)", "lunch" = "Dotacje (%)")
    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)

    sig <- coefs$p.value < 0.05
    # Nieistotne predyktory są wyszarzone; gwiazdka oznacza p < 0.05.
    lc_table(
      data.frame(term = unname(coefs$term_pl), estimate = coefs$estimate,
                 se = coefs$std.error,
                 p = paste0(lc_pval(coefs$p.value), ifelse(sig, " *", ""))),
      cols = list(
        lc_col("term", "Zmienna", "row"),
        lc_col("estimate", "β", digits = 3),
        lc_col("se", "SE", digits = 3),
        lc_col("p", "p")
      ),
      row_class = ifelse(!sig & coefs$term != "(Intercept)", "is-dim", NA_character_),
      label = "Współczynniki modelu"
    )
  })

  zoom_plot_server("ch1_reg_coef_plot", reactive({
    model <- ch1_model()
    if (is.null(model)) return(NULL)

    coefs <- broom::tidy(model, conf.int = TRUE)
    coefs <- coefs[coefs$term != "(Intercept)", ]
    if (nrow(coefs) == 0) return(NULL)

    labels_pl <- c("str" = "STR", "income" = "Dochód", "english" = "Angielski (%)",
                    "expenditure" = "Wydatki", "lunch" = "Dotacje (%)")
    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)
    coefs$sig <- coefs$p.value < 0.05

    ggplot(coefs, aes(x = estimate, y = term_pl, color = sig)) +
      geom_point(size = 3) +
      geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, orientation = "y") +
      geom_vline(xintercept = 0, linetype = "dashed", color = case_reference) +
      scale_color_manual(values = c("TRUE" = case_model, "FALSE" = case_highlight),
                         labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                         name = NULL) +
      labs(x = "β", y = NULL) +
      theme_upwr() + theme(legend.position = "top")
  }))

  output$ch1_reg_metrics <- renderUI({
    model <- ch1_model()
    if (is.null(model)) return(NULL)
    g <- broom::glance(model)
    rmse <- sqrt(mean(residuals(model)^2))
    tagList(
      lc_readout("R² skor.", lc_fmt(g$adj.r.squared, 3), color = case_model),
      lc_readout("AIC", lc_fmt(g$AIC, 0), color = case_conclude),
      lc_readout("RMSE", lc_fmt(rmse, 1), color = case_highlight)
    )
  })
}
