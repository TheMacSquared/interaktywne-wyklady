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

# Treść stała (dawniej output$ch1_str_test na serwerze): składana przy budowaniu strony.
ch1_str_test_ui <- function() {
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
}

# Treść stała (dawniej output$ch1_conf_stats na serwerze): składana przy budowaniu strony.
ch1_conf_stats_ui <- function() {
  r_lunch_score <- cor(ca$lunch, ca$score)
  r_lunch_str <- cor(ca$lunch, ca$str)
  tagList(
    lc_readout("r: dotacje a wynik", lc_fmt(r_lunch_score, 3), color = case_highlight),
    lc_readout("r: dotacje a STR", lc_fmt(r_lunch_str, 3), color = case_test)
  )
}

# Treść stała (dawniej output$ch1_anova_result na serwerze): składana przy budowaniu strony.
ch1_anova_result_ui <- function() {
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
               round(tidy_res$F, 1), ", p ",
               if (tidy_res$p < 0.001) "< 0.001" else paste0("= ", round(tidy_res$p, 3)),
               ", η² = ", round(tidy_res$ges, 3))),
      p(tags$strong("Tukey HSD:")),
      tags$ul(lapply(1:nrow(tukey_df), function(i) {
        tags$li(paste0(tukey_df$group1[i], " vs ", tukey_df$group2[i],
                       ": Δ = ", round(tukey_df$estimate[i], 1),
                       " pkt, p.adj ", if (tukey_df$p.adj[i] < 0.001) "< 0.001"
                       else paste0("= ", round(tukey_df$p.adj[i], 3))))
                         }))
                       )
}

# Treść stała (dawniej output$ch1_model_comparison na serwerze): składana przy budowaniu strony.
ch1_model_comparison_ui <- function() {
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
}

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

  lc_p("Ten wykład nie wprowadza nowych metod. Każdy jego rozdział przechodzi
    jedną analizę od pytania do decyzji. Pierwsza sięga po kolei po narzędzia
    z wykładów 01–07:
    opis danych, korelację, ANOVA, regresję prostą i wieloraką, ocenę
    ", gloss("reszta", "reszt"), " i porównanie modeli. Celem jest zobaczyć, jak te narzędzia
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

  lc_p("Danymi jest zbiór CASchools, który znamy z ćwiczeń w wykładach 05
    i 07 i który w rozdziale 02 wykładu 08 oceniliśmy jako bardzo dobry.
    Opisuje 420 okręgów szkolnych w Kalifornii w roku szkolnym 1998–1999.
    ", gloss("jednostka obserwacji", "Jednostką obserwacji"), " jest okręg,
    a nie uczeń ani klasa. ", gloss("zmienna zależna", "Zmienną zależną"),
    " będzie średni wynik okręgu, czyli średnia z wyników testu z czytania
    i z matematyki. W wykładzie 07 modelowaliśmy sam wynik z czytania,
    dlatego liczby w tym wykładzie będą podobne, ale nie identyczne."),

  lc_p("Główną zmienną objaśniającą jest STR (ang. ", em_("student–teacher
    ratio"), "): liczba uczniów w okręgu podzielona przez liczbę nauczycieli.
    To przybliżenie warunków nauki, a nie pomiar wielkości konkretnej klasy.
    Okręg ze STR równym 20 może mieć klasy po 25 uczniów i nauczycieli
    wspomagających, którzy nie prowadzą własnej klasy."),

  lc_p("Analizę przeprowadzimy w sześciu krokach:"),

  # Mapa sześciu kroków zamiast listy numerowanej
  # (dawna lista tags$ol z odwołaniami do wykładów — w historii gita).
  figure_panel(
    label = "Ryc. 1.1",
    title = "Przebieg analizy",
    case_pipeline(case_steps_ch1)
  ),

  # ========================================================================
  # KROK 1: Poznanie danych
  # ========================================================================
  lc_h2("sec-02", "Krok 1: Poznanie danych"),
  case_pipeline(case_steps_ch1, current = 1),  # Pasek postępu

  lc_p(
    "Zanim policzymy cokolwiek o związkach, sprawdzamy, jakie wartości
     przyjmują zmienne i czy nie kryją niespodzianek."
  ),

  lc_p("Tak jak w wykładzie 01, każdą zmienną oglądamy najpierw osobno:
    ", gloss("histogram"), " pokazuje kształt rozkładu, a ", gloss("wykres pudełkowy"), " ", gloss("mediana", "medianę"), ",
    ", gloss("rozstęp międzykwartylowy"), " i wartości odstające. Pod wykresem są
    podstawowe ", gloss("statystyka opisowa", "statystyki opisowe"), " wybranej zmiennej."),

  figure_panel(
    label = "Ryc. 1.2",
    title = "Przegląd zmiennych",
    ch1_eda_fig()
  ),

  lc_p("Średni wynik okręgu wynosi 654.2 pkt przy ", gloss("odchylenie standardowe", "odchyleniu standardowym"), "
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
    label = "Ryc. 1.3",
    title = "Macierz korelacji",
    ch1_corr_fig()
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
  case_pipeline(case_steps_ch1, current = 2),  # Pasek postępu

  lc_p(
    "Sprawdzamy związek, który zobaczyłby polityk, gdyby spojrzał tylko na
     dwie zmienne. To punkt odniesienia dla dalszych kroków."
  ),

  lc_p("Macierz korelacji opisuje ", gloss("próba", "próbę"), ". Żeby powiedzieć coś o zależności
    ogólnej, potrzebny jest test. Jak w rozdziale 06 wykładu 05 sprawdzamy
    hipotezę, że w ", gloss("populacja", "populacji"), " współczynnik ", gloss("korelacja", "korelacji"),
    " \\(\\rho\\) między STR a wynikiem wynosi zero:"),

  # Hipotezy testu korelacji ze znacznikami
  lc_formula_annotated(
    "⟦1|H_0⟧: ⟦2|\\rho⟧ = 0 \\qquad ⟦3|H_a⟧: \\rho \\neq 0",
    notes  = list("hipoteza zerowa", "korelacja w populacji", "hipoteza alternatywna"),
    colors = c("grafit", "param", "wrzos")
  ),

  lc_p("Siłę związku w jednostkach wyniku opisuje ",
    gloss("regresja prosta", "regresja prosta"), " z wykładu 07. Jej
    nachylenie mówi, o ile punktów średnio różnią się wyniki okręgów, w których
    STR różni się o jednego ucznia na nauczyciela. Panel pokazuje chmurę
    okręgów z prostą regresji, a obok wynik testu korelacji i równanie
    prostej."),

  figure_panel(
    label = "Ryc. 1.4",
    title = "Wynik a liczba uczniów na nauczyciela",
    ch1_str_fig(),
    ch1_str_test_ui()
  ),

  lc_p("Korelacja wynosi \\(r = -0.23\\) (95% przedział ufności od -0.32
    do -0.13, p < 0.001), więc ", gloss("hipoteza zerowa", "hipotezę zerową"), " odrzucamy. Prosta regresji
    ma nachylenie -2.28: okręg, w którym na nauczyciela przypada o jednego
    ucznia więcej, ma średnio wynik niższy o 2.28 pkt. To ta sama
    zależność, którą w zadaniu 5 z wykładu 05 i w rozdziale 03 wykładu 07
    widzieliśmy dla samego czytania (\\(r = -0.25\\), nachylenie -2.62).
    Dla matematyki nachylenie jest łagodniejsze (-1.94), więc dla średniej
    z obu testów wychodzi wartość pośrednia."),

  case_quiz_ui("ch1_quiz_simple",
    title = "Co można powiedzieć po kroku 2?",
    question = "Korelacja r = -0.23 (p < 0.001), nachylenie -2.28 pkt na
      ucznia, R² = 0.05. Które zdanie można uczciwie przekazać politykowi?",
    choices = c(
      "Zmniejszenie STR o jednego ucznia podniesie wyniki o 2.28 pkt." = "causal",
      "Okręgi z mniejszym STR mają przeciętnie lepsze wyniki; nie wiadomo jeszcze, czy to skutek liczby uczniów." = "assoc",
      "STR wyjaśnia większość różnic w wynikach między okręgami." = "most",
      "Związek jest nieistotny, bo korelacja jest słaba." = "ns"
    ),
    correct = "assoc",
    feedback = list(
    causal = "Nachylenie z regresji prostej na danych obserwacyjnych opisuje
    różnice między okręgami, a nie skutek zmiany STR w jednym okręgu.",
    assoc = "Związek jest istotny, ale obserwacyjny. Czy wynika z liczby
    uczniów, pokażą dopiero kolejne kroki.",
    most = "R² = 0.05: STR wyjaśnia około 5% zmienności wyników.",
    ns = "Słaby związek może być istotny: przy 420 okręgach p < 0.001."
    )
  ),

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
  case_pipeline(case_steps_ch1, current = 3),  # Pasek postępu

  lc_p(
    "Sprawdzamy, czy za liczbą uczniów na nauczyciela i za wynikami nie
     kryje się wspólne tło: sytuacja materialna rodzin."
  ),

  lc_p("Z wykładu 05 wiemy, że zmienna zakłócająca musi być powiązana z obiema
    zmiennymi, których związek badamy. Ubóstwo mierzymy odsetkiem uczniów
    z dotacją do obiadu. Ten wskaźnik wymaga ostrożnej lektury. Więcej
    dotacji idzie w parze z niższymi wynikami, ale nie dlatego, że obiady
    szkodzą nauce. Dotacje trafiają tam, gdzie rodzinom jest trudniej, więc
    odsetek dotacji pokazuje, kto potrzebuje wsparcia. Panel zestawia oba
    potrzebne związki: odsetka dotacji z wynikiem i odsetka dotacji ze STR."),

  figure_panel(
    label = "Ryc. 1.5",
    title = "Dotacje do obiadów, wyniki i liczba uczniów na nauczyciela",
    lc_readouts(ch1_conf_stats_ui()),
    lc_plots(
      ch1_conf_fig("score", "Średni wynik testu (pkt)", "@accent"),
      ch1_conf_fig("str", "Uczniowie na nauczyciela (STR)", "@wrzos")
    )
  ),

  lc_p("Pierwszy związek jest bardzo silny (\\(r = -0.87\\)): odsetek dotacji
    sam przewiduje wynik lepiej niż jakakolwiek inna zmienna w zbiorze.
    Drugi jest słaby (\\(r = 0.14\\)): okręgi biedniejsze mają tylko trochę
    więcej uczniów na nauczyciela. Ubóstwo spełnia więc oba warunki
    zmiennej zakłócającej, ale drugi w niewielkim stopniu. Podobnie
    wyszło w zadaniu 9 z rozdziału 07 wykładu 05: po podziale obu zmiennych
    na dwie kategorie test χ² nie wykazał istotnego związku STR
    z ubóstwem.
    Ubóstwo może zatem tłumaczyć część korelacji STR z wynikami, ale
    raczej nie całą."),

  # Schemat zmiennej zakłócającej z siłą obu związków
  figure_panel(
    label = "Ryc. 1.6",
    title = "Ubóstwo jako wspólne tło",
    width_mode = "text",
    case_confounder(
      nodes    = c("Ubóstwo (dotacje)", "STR", "Wynik"),
      edges    = c("r = 0.14", "r = -0.87", "r = -0.23"),
      strength = c(0.14, 0.87, 0.23)
    )
  ),

  lc_p("Skalę różnic między okręgami biednymi i zamożnymi najłatwiej zobaczyć,
    dzieląc okręgi na trzy grupy według odsetka dotacji: poniżej 25%
    (niski poziom ubóstwa), od 25% do 50% (średni) i co najmniej 50%
    (wysoki). Średnie wyniki trzech grup porównuje ",
    gloss("ANOVA"), " z rozdziału 09 wykładu 05, a ",
    gloss("test post hoc", "test post hoc"), " Tukeya wskazuje, które pary
    grup się różnią."),

  figure_panel(
    label = "Ryc. 1.7",
    title = "Wyniki w grupach ubóstwa",
    ch1_anova_fig(),
    ch1_anova_result_ui()
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
    "Przed przypisaniem różnicy w wynikach jednej zmiennej zapytaj, czym
     jeszcze różnią się porównywane grupy."),

  # ========================================================================
  # KROK 4: Kontrolowanie zakłóceń (regresja wieloraka)
  # ========================================================================
  lc_h2("sec-05", "Krok 4: Związek STR z wynikami po kontroli innych zmiennych"),
  case_pipeline(case_steps_ch1, current = 4),  # Pasek postępu

  lc_p(
    "Dokładamy do modelu kolejne ", gloss("zmienna kontrolna", "zmienne kontrolne"),
    " i patrzymy, co dzieje się ze ", gloss("współczynnik regresji", "współczynnikiem"),
    " przy STR."
  ),

  lc_p("W ", gloss("regresja wieloraka", "regresji wielorakiej"), " z rozdziału
    03 wykładu 07 współczynnik przy STR odpowiada na inne pytanie niż
    w regresji prostej: o ile różnią się średnio wyniki okręgów, które mają
    różny STR, ale te same wartości pozostałych ",
    gloss("predyktor", "predyktorów"), ". W najszerszym modelu tego kroku:"),

  # Model wieloraki ze znacznikami (STR vs zmienne kontrolne)
  lc_formula_annotated(
    "⟦1|\\text{wynik}⟧ = \\beta_0 + ⟦2|\\beta_1⟧ \\, ⟦3|\\text{STR}⟧ + ⟦4|\\beta_2 \\, \\text{dochód} + \\beta_3 \\, \\text{angielski} + \\beta_4 \\, \\text{dotacje}⟧ + ⟦5|\\varepsilon⟧",
    notes  = list("zmienna zależna", "współczynnik przy STR", "uczniowie na nauczyciela",
                  "zmienne kontrolne", "reszta"),
    colors = c("niebo", "accent", "niebo", "szalwia", "bursztyn"),
    above  = c(2, 4)
  ),

  lc_p("Panel buduje cztery modele, za każdym razem dokładając jedną zmienną:
    sam STR, potem dochód, odsetek uczniów uczących się angielskiego
    i odsetek dotacji do obiadu. Tabela podaje współczynnik przy STR,
    jego ", gloss("p-wartość"), " oraz dwie miary porównawcze z rozdziału 04 wykładu 07:
    ", gloss("skorygowany R²"), " i ", gloss("AIC"), ". Wykres pod tabelą
    pokazuje współczynnik STR w kolejnych modelach."),

  figure_panel(
    label = "Ryc. 1.8",
    title = "Seria modeli i współczynnik przy STR",
    ch1_model_comparison_ui(),
    ch1_beta_str_fig()
  ),

  case_quiz_ui("ch1_quiz_drop",
    title = "Dlaczego współczynnik przy STR zmalał?",
    question = "Po dodaniu dochodu do modelu współczynnik przy STR spada
      z -2.28 do -0.65. Które wyjaśnienie jest poprawne?",
    choices = c(
      "Dochód zmniejsza liczbę uczniów w klasach." = "mechanism",
      "Część prostego związku wynikała z tego, że okręgi z mniejszym STR są zamożniejsze." = "confound",
      "Model z dochodem jest gorzej dopasowany, więc współczynnik jest mniej wiarygodny." = "worse",
      "STR i dochód mierzą to samo, więc model nie umie ich rozdzielić." = "collinear"
    ),
    correct = "confound",
    feedback = list(
    mechanism = "Model nie mówi, że dochód zmienia STR. Współczynnik maleje,
    bo porównujemy teraz okręgi o tym samym dochodzie.",
    confound = "Zamożniejsze okręgi mają i nieco mniejszy STR, i lepsze
    wyniki. Po wyrównaniu dochodu zostaje mniejsza różnica.",
    worse = "Model z dochodem jest lepiej dopasowany: skorygowany R² rośnie
    z 0.049 do 0.509.",
    collinear = "STR i dochód są skorelowane słabo (r = -0.23), więc model
    bez trudu je rozdziela."
    )
  ),

  lc_p("Po dodaniu dochodu współczynnik przy STR spada z -2.28 do -0.65
    i przestaje być istotny (p = 0.07). To ten sam ruch, który rozdział 03
    wykładu 07 pokazał dla czytania, gdzie współczynnik spadł z -2.62 do
    -0.95. Dochód tłumaczy większą część prostego związku: zamożniejsze
    okręgi mają i nieco mniej uczniów na nauczyciela, i wyraźnie lepsze
    wyniki. Po dodaniu odsetka uczniów uczących się angielskiego
    współczynnik przy STR prawie znika (-0.07, p = 0.80)."),

  lc_p("Czwarty model przynosi niespodziankę. Po dodaniu odsetka dotacji
    współczynnik przy STR wraca do -0.56 i znów jest istotny (p = 0.015).
    Dzieje się tak, bo w modelach 2 i 3 dochód wchodzi do równania liniowo,
    choć jego związek z wynikiem jest zakrzywiony: przy wyższych dochodach
    wyniki rosną wolniej. W rozdziale 02 wykładu 07 było to widać jako łuk
    w resztach. Odsetek dotacji dokłada informację o ubóstwie, której
    liniowy dochód nie oddaje, i dopiero wtedy porównanie okręgów
    o podobnym STR staje się porównaniem okręgów o naprawdę podobnej
    sytuacji. W wykładzie 07 ten sam czteroczynnikowy model dla czytania
    dawał współczynnik -0.78. Dla średniej z czytania i matematyki wychodzi
    -0.56, bo dla samej matematyki jest on mniejszy i nieistotny (-0.34)."),

  lc_p("Który z tych modeli wybrać? Miary z rozdziału 04 wykładu 07 wskazują
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
  case_pipeline(case_steps_ch1, current = 5),  # Pasek postępu

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
    label = "Ryc. 1.9",
    title = "Model wieloraki — wybór predyktorów",
    # Model każdego podzbioru predyktorów jest policzony w R; pola wyboru przełączają wariant.
    ch1_reg_fig()
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
    w wykładzie 06 i w rozdziale 02 wykładu 07. Panel ich nie pokazuje,
    ale w modelu 4 nie układają się w łuk względem dochodu, jak w modelu 3,
    a ich rozrzut jest podobny dla okręgów o niskich i wysokich
    przewidywanych wynikach. ", gloss("test Shapiro-Wilka", "Test Shapiro-Wilka"), " wykrywa niewielkie
    odchylenie od ", gloss("rozkład normalny", "rozkładu normalnego"), " (p = 0.03), ale przy 420 okręgach
    tak małe odchylenie nie zagraża wnioskom o współczynnikach."),

  # ========================================================================
  # KROK 6: Odpowiedź na pytanie decyzyjne
  # ========================================================================
  lc_h2("sec-07", "Krok 6: Odpowiedź na pytanie decyzyjne"),
  case_pipeline(case_steps_ch1, current = 6),  # Pasek postępu

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
    w panelu Ryc. 1.9 i które uwzględniają dochód albo odsetek dotacji,
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
    tags$li("Dane obserwacyjne, nie ",
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
