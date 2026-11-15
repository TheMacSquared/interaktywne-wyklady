# ============================================================================
# CHAPTER 3: Regresja wieloraka
# ============================================================================

ch3_ui <- list(
  id    = "ch-wieloraka",
  num   = "03",
  title = "Regresja wieloraka",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Regresja",
      num   = "03",
      title  = "Regresja wieloraka.",
      lead   = "Drugi predyktor w modelu zmienia znaczenie pierwszego. Współczynnik
                dochodu w danych o szkołach z Kalifornii maleje prawie czterokrotnie,
                gdy obok dochodu pojawia się odsetek uczniów z dotacją do obiadu."
    ),

    lc_p("W rozdziale 01 opisywaliśmy wynik jedną zmienną, a w rozdziale 02
      ocenialiśmy, czy taki model jest wart zaufania: patrzyliśmy na reszty,
      \\(R^2\\) i RMSE. Na wynik zwykle działa jednak wiele czynników naraz,
      a te czynniki są ze sobą powiązane. Model z jednym ",
      gloss("predyktor", "predyktorem"), " przypisuje wtedy jednej zmiennej
      także to, co należy się innym."),

    lc_p("Tę sytuację znamy z ćwiczeń w wykładzie 05. Wyniki czytania w okręgach
      szkolnych Kalifornii korelowały ujemnie z liczbą uczniów na nauczyciela
      (\\(r = -0.25\\)), ale okręgi z mniejszymi klasami bywają też
      zamożniejsze, więc część tej ", gloss("korelacja", "korelacji"), " mógł tłumaczyć dochód. Korelacja
      nie pozwalała tego sprawdzić, bo zawsze dotyczy tylko dwóch zmiennych.
      Potrzebny jest model, który uwzględnia kilka zmiennych naraz."),

    lc_h2("ch3-wiele-predyktorow", "Wiele predyktorów naraz"),

    lc_p(gloss("regresja wieloraka", "Regresja wieloraka"), " rozszerza równanie
      prostej o kolejne predyktory. Przy \\(k\\) predyktorach model ma postać:"),

    # Model wieloraki ze znacznikami symboli
    lc_formula_annotated(
      "Y = ⟦1|\\beta_0⟧ + ⟦2|\\beta_1⟧ X_1 + \\beta_2 X_2 + \\ldots + \\beta_k X_{⟦3|k⟧} + ⟦4|\\varepsilon⟧",
      notes  = list("wyraz wolny", "nachylenie przy X₁", "liczba predyktorów",
                    "błąd losowy"),
      colors = c("terakota", "indygo", "szalwia", "bursztyn"),
      above  = c(2, 3)
    ),

    lc_p("Współczynniki szacuje się tak samo jak w ",
      gloss("regresja prosta", "regresji prostej"), ": ", gloss("metoda najmniejszych kwadratów", "metodą najmniejszych kwadratów"), ",
      czyli tak, by suma kwadratów reszt była jak najmniejsza.
      Zmienia się natomiast interpretacja. Współczynnik \\(\\beta_j\\) mówi,
      o ile średnio zmienia się \\(Y\\), gdy \\(X_j\\) rośnie o jednostkę,
      a wszystkie pozostałe predyktory mają te same wartości."),

    lc_p("To zastrzeżenie jest sednem regresji wielorakiej. W regresji prostej
      współczynnik zbiera cały związek \\(X\\) z \\(Y\\), także ten, który
      przechodzi przez ", gloss("zmienna pominięta", "zmienne pominięte"), " w modelu. W regresji wielorakiej
      zostaje tylko ta część związku, której nie da się przypisać pozostałym
      predyktorom. Dlatego ten sam predyktor może mieć w obu modelach zupełnie
      inny współczynnik, czasem nawet o przeciwnym znaku."),

    lc_h2("ch3-budowanie", "Budowanie modelu wielorakiego"),

    lc_p("Zobaczmy, jak to wygląda na danych. Zbiór CASchools opisuje 420
      okręgów szkolnych w Kalifornii: średnie wyniki testów z czytania
      i matematyki oraz cechy okręgu, takie jak dochód mieszkańców, odsetek
      uczniów z dotacją do obiadu czy liczba uczniów na nauczyciela. To ",
      gloss("dane obserwacyjne", "dane obserwacyjne"), ": nikt nie przydzielał
      okręgom dochodów ani wielkości klas, więc wszystkie te cechy są ze sobą
      splecione."),

    lc_p("Panel poniżej zawsze zaczyna od dochodu okręgu i pozwala dokładać
      kolejne predyktory do tego samego równania. Tabela pokazuje ",
      gloss("współczynnik regresji", "współczynniki"), " pełnego modelu,
      wykres u góry rozbija dane na grupy według dodanych zmiennych,
      a wykres u dołu porównuje linię regresji prostej z linią modelu,
      w którym pozostałe predyktory ustawiono na ich średnich."),

    figure_panel(
      label = "Ryc. 3.1", title = "CASchools: model z wieloma predyktorami",
      full_width = TRUE,
      # Tabele i miary z R przy budowaniu, wykresy w przeglądarce (modules/figures.*, rycina s07_multi).
      ch3_multi_fig(),
      lc_caption("Dane: 420 okręgów szkolnych w Kalifornii. Model zawsze zaczyna od
        dochodu okręgu (tys. USD); zaznaczone predyktory dochodzą do tego samego
        równania."),
      lc_note("Jak czytać",
        p("Tabela pokazuje współczynniki pełnego modelu addytywnego,
          bez interakcji. Gwiazdka przy ", gloss("p-wartość", "p-wartości"), " oznacza p < 0.05.")
      )
    ),

    lc_p("W ustawieniu startowym model wyjaśnia wynik z czytania dochodem
      i odsetkiem uczniów z dotacją do obiadu. Sam dochód miał w regresji
      prostej współczynnik 1.94: okręg zamożniejszy o tysiąc dolarów miał
      średnio o 1.94 punktu wyższy wynik. Po dodaniu dotacji współczynnik
      dochodu spada do 0.50. Dochód i odsetek dotacji są silnie powiązane
      (\\(r = -0.68\\)), bo w biedniejszych okręgach więcej dzieci
      kwalifikuje się do dotacji. W regresji prostej dochód zbierał więc także
      związek, który teraz przejmuje odsetek dotacji (współczynnik -0.56
      na punkt procentowy). \\(R^2\\) rośnie z 0.49 do 0.79."),

    lc_p("Każdy dodany predyktor wnosi do równania własny składnik, a jego
      współczynnik nie zależy od wartości pozostałych zmiennych. Taki model
      nazywa się addytywnym. Bywa, że wpływ jednej zmiennej zależy od drugiej,
      na przykład nachylenie jest inne w każdej grupie. Opisuje to ",
      gloss("interakcja", "interakcja"), ", której poświęcony jest rozdział 03B.
      Diagnostyka z rozdziału 02 obowiązuje w modelu wielorakim bez zmian:
      reszty ocenia się tak samo, niezależnie od liczby predyktorów."),

    lc_h2("ch3-kontrola", "Co znaczy „przy stałych pozostałych zmiennych”?"),

    lc_p("Spadek współczynnika dochodu z 1.94 do 0.50 wymaga wyjaśnienia,
      bo dane się nie zmieniły. Zmieniło się pytanie, na które odpowiada
      współczynnik. W regresji prostej porównujemy wszystkie okręgi bogatsze
      ze wszystkimi biedniejszymi. W regresji wielorakiej porównujemy okręgi,
      które różnią się dochodem, ale mają taki sam odsetek dotacji do obiadu.
      Mówimy wtedy, że odsetek dotacji jest ",
      gloss("zmienna kontrolna", "zmienną kontrolną"), "."),

    lc_p("Kontrolowanie zmiennej oznacza, że z predyktora usuwa się informację,
      którą niesie już inny predyktor. Zostaje część unikalna: to, czym
      okręgi o tym samym odsetku dotacji nadal różnią się dochodem. Właśnie
      tę część model wiąże z wynikiem. Panel poniżej pokazuje to w trzech
      krokach: dwie regresje proste, a potem model z dochodem, odsetkiem
      dotacji i odsetkiem uczniów uczących się angielskiego jako drugiego
      języka."),

    figure_panel(
      label = "Ryc. 3.2",
      full_width = TRUE,
      ch3_control_fig()
    ),

    lc_p("W modelu z trzema predyktorami współczynnik dochodu wynosi 0.70.
      Wśród okręgów o tym samym odsetku dotacji i tym samym odsetku uczniów
      uczących się angielskiego okręg zamożniejszy o tysiąc dolarów ma średnio
      wynik wyższy o 0.70 punktu. To prawie trzy razy mniej niż w regresji
      prostej. Dotacje (-0.40) i odsetek uczniów uczących się angielskiego
      (-0.29) mają ujemne współczynniki, a ", gloss("przedział ufności", "przedziały ufności"), " wszystkich
      trzech leżą daleko od zera."),

    lc_p("Teraz można wrócić do zadania z wykładu 05. Tam pytaliśmy, czy dochód
      jest ", gloss("zmienna zakłócająca", "zmienną zakłócającą"), " związku
      między liczbą uczniów na nauczyciela a wynikiem z czytania. W regresji
      prostej każdy dodatkowy uczeń na nauczyciela wiąże się z wynikiem
      niższym średnio o 2.62 punktu. Po dodaniu dochodu współczynnik spada
      do -0.95, a więc dochód tłumaczy większą część pierwotnego związku.
      Związek nie znika jednak: po dodaniu jeszcze odsetka dotacji i odsetka
      uczniów uczących się angielskiego współczynnik wynosi -0.78, a p-wartość
      jest mniejsza niż 0.001. Ten model można zbudować w panelu Ryc. 3.1."),

    lc_p("Kontrola zmiennych ma jednak granice. Model uwzględnia tylko te
      zmienne, które do niego włożyliśmy. Jeśli istnieje zmienna pominięta,
      związana i z predyktorem, i z wynikiem, współczynnik nadal ją zawiera.
      Dlatego współczynnik z danych obserwacyjnych opisuje związek po
      uwzględnieniu wybranych zmiennych, a nie dowodzi przyczyny. Dodanie
      zmiennej może też odwrócić znak współczynnika. To ",
      gloss("paradoks Simpsona", "paradoks Simpsona"), " znany z wykładu 05.
      W CASchools do odwrócenia nie dochodzi, ale rozdział 03B pokazuje je
      na danych o pingwinach."),

    lc_h2("ch3-wspolliniowosc", "Współliniowość"),

    lc_p("Kontrola działa dlatego, że dochód i odsetek dotacji niosą częściowo
      różną informację. Przy korelacji -0.68 okręgi o podobnym dochodzie wciąż
      wyraźnie różnią się odsetkiem dotacji, więc model ma z czego oszacować
      osobny związek każdej zmiennej. Problem pojawia się, gdy dwa predyktory mówią
      prawie to samo. ", gloss("współliniowość", "Współliniowość"), " to
      silna korelacja między predyktorami. Model widzi wtedy niewiele okręgów,
      w których jedna zmienna rośnie, a druga nie, więc trudno mu rozdzielić
      wspólny związek między obie zmienne."),

    lc_p("Siłę tego problemu mierzy ", gloss("VIF"), ", czyli współczynnik
      inflacji wariancji. Dla predyktora \\(X_j\\) liczy się go z
      \\(R_j^2\\) modelu, w którym \\(X_j\\) przewiduje się pozostałymi
      predyktorami:"),

    # VIF ze znacznikami symboli
    lc_formula_annotated(
      "⟦1|\\text{VIF}_j⟧ = \\frac{1}{1 - ⟦2|R_j^2⟧}",
      notes  = list("inflacja wariancji", "R² dla Xⱼ z pozostałych"),
      colors = c("accent", "indygo")
    ),

    lc_p("Gdy predyktor nie jest związany z pozostałymi, \\(R_j^2 = 0\\)
      i VIF wynosi 1. Im lepiej pozostałe predyktory odtwarzają \\(X_j\\),
      tym większy VIF. Pierwiastek z VIF mówi, ile razy ", gloss("błąd standardowy"), "
      współczynnika jest większy niż przy predyktorach nieskorelowanych:
      VIF równy 4 oznacza błąd standardowy dwa razy większy. Nie ma jednej
      granicy, od której VIF jest za duży. Wartości bliskie 1 nie budzą
      wątpliwości, a im dalej od 1, tym ostrożniej interpretuje się
      pojedyncze współczynniki."),

    lc_p("Panel pokazuje trzy ustawione przykłady. W każdym jest 30 studentów
      i ten sam model: wynik egzaminu w punktach wyjaśniany godzinami nauki
      i liczbą rozwiązanych zadań. Obie zmienne mierzą w dużej mierze to samo,
      wysiłek włożony w naukę, ale w kolejnych przykładach są ze sobą
      powiązane coraz silniej. Tabela zestawia trzy modele: z każdym
      predyktorem osobno i z oboma naraz."),

    figure_panel(
      label = "Ryc. 3.3", title = "Gdy predyktory mówią prawie to samo",
      full_width = TRUE,
      ch3_case_fig()
    ),

    lc_p("Gdy predyktory są słabo powiązane (\\(r = 0.31\\), VIF 1.1), model
      z oboma niewiele zmienia. Każdy współczynnik zachowuje kierunek
      i istotność, a jego błąd standardowy nie rośnie."),

    lc_p("W drugim przykładzie godziny i zadania są skorelowane na poziomie
      0.96, a VIF wynosi około 14. Każdy predyktor osobno jest wyraźnie
      istotny (p < 0.001). W modelu z oboma żaden nie jest istotny
      (p = 0.71 i p = 0.33), choć model jako całość jest (p < 0.001)
      i przewiduje wynik tak samo dobrze jak każdy predyktor osobno.
      Błąd standardowy współczynnika godzin wzrósł z 0.34 do 1.27, prawie
      czterokrotnie, tyle, ile wynosi pierwiastek z VIF. Wniosek, że ani
      godziny, ani zadania nie mają związku z wynikiem, byłby błędny. Związek
      jest, tylko model nie potrafi przypisać go jednej zmiennej."),

    lc_p("W trzecim przykładzie (\\(r = 0.95\\)) skutek jest bardziej mylący.
      Osobno każde rozwiązane zadanie wiąże się z wynikiem wyższym o 0.74 pkt.
      W modelu z godzinami współczynnik zadań staje się ujemny: -0.82 pkt na
      zadanie przy stałych godzinach nauki, i to przy p = 0.021. Jednocześnie
      współczynnik godzin rośnie z 1.89 do 3.60. Nie znaczy to, że
      rozwiązywanie zadań szkodzi. Wśród studentów, którzy uczyli się tyle
      samo godzin, liczba zadań różni się niewiele, więc model, rozdzielając
      wspólny związek, przerzuca go na jedną zmienną. Przy silnej
      współliniowości znak i wielkość pojedynczego współczynnika mogą być
      dziełem przypadku."),

    lc_p("W CASchools współliniowość jest umiarkowana. W modelu ze wszystkimi
      siedmioma predyktorami z Ryc. 3.1 największy VIF, około 5.7, ma odsetek
      uczniów z dotacją do obiadu. Ta zmienna jest silnie związana zarówno
      z dochodem (\\(r = -0.68\\)), jak i z odsetkiem uczniów z rodzin
      objętych pomocą socjalną CalWORKs (\\(r = 0.74\\)). Gdy współczynniki dwóch predyktorów są
      niestabilne, można zostawić w modelu jeden z nich albo połączyć je
      w jedną miarę."),

    lc_note("Zasada", rule = TRUE,
      "Współliniowość nie psuje przewidywań modelu. Osłabia interpretację
       pojedynczych współczynników, dlatego VIF sprawdza się, zanim zacznie
       się je interpretować."
    ),

    lc_h2("ch3-co-dalej", "Co dalej"),

    lc_p("Regresja wieloraka pozwala opisać wynik wieloma zmiennymi naraz,
      a każdy jej współczynnik odpowiada na pytanie o związek przy stałych
      pozostałych predyktorach. Dotąd wszystkie predyktory były ilościowe,
      a ich związek z wynikiem był taki sam w każdej grupie. Rozdział 03B
      pokazuje na danych o pingwinach, co się dzieje, gdy w danych są naturalne
      grupy: pominięta zmienna może odwrócić kierunek związku (paradoks
      Simpsona), grupę można wpisać do równania jako predyktor jakościowy,
      a interakcja pozwala, by nachylenie różniło się między grupami."),

    lc_p("Zostaje pytanie, który model wybrać. Najwyższe \\(R^2\\) nie jest
      dobrym kryterium: w CASchools rośnie z 0.49 dla samego dochodu do 0.79
      po dodaniu dotacji i do 0.83 po dodaniu odsetka uczniów uczących się
      angielskiego, ale rosłoby też po dodaniu predyktora zupełnie losowego.
      Miary, które karzą model za złożoność, wprowadza rozdział 04."),

    lc_chapter_next(
      num       = "03B",
      title     = "Kontekst i interakcje",
      lead      = "pominięta zmienna, predyktor jakościowy i interakcje",
      target_id = "ch-3b"
    )
  )
)


# ============================================================================
# SERVER
# ============================================================================

# Wszystkie ryciny rozdziału liczy przeglądarka albo R przy budowaniu strony.
ch3_server <- function(input, output, session) {}
