# ============================================================================
# CHAPTER 1: Logika testowania hipotez
# ============================================================================

ch1_ui <- list(
  id = "ch-logika", num = "01", title = "Logika testowania",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 01 · Testowanie hipotez",
      num    = "01",
      title  = "Logika testowania.",
      lead   = "Studenci z telefonem na biurku wypadli w teście koncentracji słabiej
                niż studenci z telefonem w plecaku. Różnice między grupami powstają
                jednak także przypadkiem. Test statystyczny sprawdza, czy obserwowana
                różnica jest większa, niż przypadek zwykle wytwarza."
    ),

    lc_p("Wykład 03 skończył się na pytaniach innego rodzaju niż „gdzie leży
      parametr?”. Pytaliśmy, czy średni czas dojazdu przekracza 26 minut albo
      czy poparcie przekracza 50%, i rozstrzygaliśmy to, sprawdzając, czy
      ", gloss("przedział ufności"), " obejmuje wartość progową. Testowanie hipotez zajmuje się
      właśnie takimi pytaniami: czy parametr ma konkretną wartość, czy dwie grupy
      się różnią, czy dwie zmienne są ze sobą powiązane. Odpowiedzią nie jest
      zakres wartości, tylko decyzja. Zaczniemy od przykładu, w którym taka
      decyzja jest potrzebna."),

    # ========================================================================
    # SEKCJA 0: Case study otwierający
    # ========================================================================
    lc_h2("ch1-case", "Case study: telefon a koncentracja"),

    lc_p("Wyobraź sobie eksperyment przeprowadzony na uczelni, inspirowany
      badaniem Ward i in. (2017):"),
    tags$ul(
      tags$li("80 studentów losowo przydzielonych do dwóch grup po 40 osób"),
      tags$li(tags$strong("Grupa A:"), " telefon schowany w plecaku"),
      tags$li(tags$strong("Grupa B:"), " telefon leży na biurku (ekranem w dół, wyciszony)"),
      tags$li("Wszyscy rozwiązują ten sam test koncentracji (0–100 punktów)")
    ),

    lc_p("Nikt nie używa telefonu w trakcie testu. Grupy różnią się tylko tym,
      czy telefon leży w zasięgu wzroku. O przydziale do grupy decydowało
      losowanie, więc wszystkie inne cechy studentów rozkładają się między
      grupami przypadkowo. Jeśli wyniki grup się różnią, zostają dwa
      wyjaśnienia: wpływ telefonu albo przypadek. Panel poniżej losuje wyniki
      takiego eksperymentu, a każde kliknięcie to nowa grupa 80 studentów."),

    figure_panel(
      label = "Ryc. 1.1",
      title = "Wyniki eksperymentu",
      lc_toolbar(
        lc_action("ch1_case_generate", "Przeprowadź eksperyment", variant = "solid"),
        lc_readouts(uiOutput("ch1_case_stats"))
      ),
      lc_plot("ch1_case_plot", max_height = "350px")
    ),

    lc_p("W typowym losowaniu średnia w grupie „biurko” wypada o kilka punktów
      niżej niż w grupie „plecak”. Wyniki obu grup mocno się jednak nakładają:
      ", gloss("odchylenie standardowe"), " w każdej z nich wynosi kilkanaście punktów, więc
      wielu studentów z telefonem na biurku wypada lepiej niż przeciętny student
      z telefonem w plecaku. Kolejne kliknięcia pokazują też, że sama różnica
      średnich zmienia się od eksperymentu do eksperymentu."),

    lc_p("Stąd pytanie, na które odpowiada ten wykład. Gdyby telefon nie miał
      żadnego wpływu, średnie dwóch losowych grup i tak by się różniły, bo każda
      grupa to inna próba. To ta sama ", gloss("zmienność próbkowa"), ", którą w wykładzie 03
      mierzył błąd standardowy. Czy zaobserwowana różnica jest na tyle duża,
      że trudno ją wytłumaczyć samą zmiennością próbkową? Taką decyzję
      podejmuje test statystyczny."),

    # ========================================================================
    # SEKCJA 1: Logika testowania z odniesieniem do case study
    # ========================================================================
    lc_h2("ch1-logika", "Testowanie hipotez — logika rozumowania"),

    lc_p("Test statystyczny rozumuje podobnie jak sąd w procesie karnym. Sąd
      zaczyna od domniemania niewinności: oskarżonego uważa się za niewinnego,
      dopóki dowody wyraźnie nie przemawiają przeciw temu. Test zaczyna od
      założenia, że efektu nie ma. To założenie nazywamy ",
      gloss("hipoteza zerowa", "hipotezą zerową"), " i oznaczamy H₀. Jego
      zaprzeczenie, czyli twierdzenie, że efekt istnieje, to ",
      gloss("hipoteza alternatywna"), " Hₐ."),

    lc_table(
      data.frame(
        element = c("H₀", "Hₐ", "Dane", "Możliwy werdykt"),
        court = c("Oskarżony jest niewinny", "Oskarżony jest winny",
                  "Dowody złożone w sądzie",
                  "Wina nie została wykazana — albo dowody wystarczają, by uznać oskarżonego za winnego"),
        phone = c("Telefon nie wpływa na koncentrację (różnica = 0)",
                  "Telefon wpływa na koncentrację (różnica ≠ 0)",
                  "Wyniki testu 80 studentów",
                  "Nie mamy podstaw, by twierdzić, że telefon wpływa — albo odrzucamy hipotezę o braku wpływu")
      ),
      cols = list(
        lc_col("element", "Element", "row"),
        lc_col("court", "Sąd", "text"),
        lc_col("phone", "Nasz eksperyment z telefonem", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_p("Z tej analogii wynikają dwie własności testu. Pierwsza: dane oceniamy
      z perspektywy H₀. Pytamy, czy wyniki takie jak nasze byłyby czymś
      zwyczajnym w świecie, w którym telefon nie ma wpływu. Jeśli byłyby bardzo
      rzadkie, uznajemy, że dane przemawiają przeciw H₀, i ją odrzucamy. Druga:
      oba werdykty nie są symetryczne. Sąd, który uniewinnia z braku dowodów,
      nie stwierdza, że oskarżony jest niewinny, tylko że wina nie została
      wykazana. Test, który nie odrzuca H₀, również nie dowodzi braku efektu.
      Mówi tylko, że dane nie dają podstaw, by efekt stwierdzić."),

    lc_p("Ta sama logika stała za przedziałami ufności z wykładu 03. Gdy 95%
      przedział ufności dla różnicy średnich nie obejmuje zera, zero nie należy
      do wartości, z którymi dane są zgodne. Test robi to samo, ale zamiast
      zakresu wartości daje decyzję i liczbę mierzącą, jak bardzo wynik odstaje
      od H₀: ", gloss("p-wartość"), "."),

    lc_p("Zanim cokolwiek policzymy, trzeba uporządkować samo pytanie: jaki stan
      uznajemy za domyślny i co byłoby sygnałem efektu. Następny rozdział zapisuje
      to jako parę hipotez H₀ i Hₐ. Błędy, p-wartość i formalną decyzję omawia
      rozdział 03."),

    lc_chapter_next(
      num       = "02",
      title     = "Od pytania do hipotezy",
      lead      = "najpierw nazywamy H₀ i Hₐ, a dopiero potem podejmujemy decyzję.",
      target_id = "ch-hipotezy"
    )

  )
)

# ============================================================================
# CHAPTER 3: Błędy, p-wartość i decyzja
# ============================================================================

ch1d_ui <- list(
  id = "ch-decyzja", num = "03", title = "Błędy, p-wartość i decyzja",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Testowanie hipotez",
      num    = "03",
      title  = "Błędy, p-wartość i decyzja.",
      lead   = "Test kończy się decyzją podjętą na podstawie jednej próby, a taka
                decyzja może się mylić na dwa sposoby. Częstość obu pomyłek da się
                kontrolować, a p-wartość prowadzi od danych do werdyktu."
    ),

    lc_p("W poprzednim rozdziale zapisaliśmy pytanie o telefon jako parę hipotez:
      H₀ mówi, że średnia koncentracja w obu warunkach jest taka sama, Hₐ — że
      się różni. Test kończy się jedną z dwóch decyzji: odrzucamy H₀ albo nie mamy
      podstaw, by ją odrzucić. Zanim zobaczymy, jak tę decyzję podjąć, ustalmy,
      na czym polega jej pomyłka."),

    # ========================================================================
    # SEKCJA 1: Błędy I i II rodzaju
    # ========================================================================
    lc_h2("ch1-bledy", "Błędy I i II rodzaju"),

    lc_p("Decyzja testu i stan rzeczywistości to dwie różne rzeczy. Dobrą analogią
      jest czujnik dymu. W świecie są dwa możliwe stany: pożar albo jego brak.
      Czujnik podejmuje jedną z dwóch decyzji: włącza alarm albo milczy. Dwie
      z czterech kombinacji są trafne, dwie to błędy. Fałszywy alarm, gdy nic się
      nie pali, jest uciążliwy, ale niegroźny. Przegapiony pożar jest groźny.
      Obu błędów nie da się wyeliminować jednocześnie: czulszy czujnik rzadziej
      przegapia pożar, ale częściej włącza się bez powodu, a mniej czuły odwrotnie."),

    lc_p("Test statystyczny ma tę samą strukturę. Stany świata to „H₀ prawdziwa”
      i „H₀ fałszywa”, decyzje to „nie odrzucamy H₀” i „odrzucamy H₀”."),

    lc_table(
      data.frame(
        decision = c("Nie odrzucamy H₀", "Odrzucamy H₀"),
        h0_true = c("Decyzja trafna", "Błąd I rodzaju (α)"),
        h0_false = c("Błąd II rodzaju (β)", "Decyzja trafna (moc, 1 − β)")
      ),
      cols = list(
        lc_col("decision", "", "row"),
        lc_col("h0_true", "H₀ prawdziwa", "text"),
        lc_col("h0_false", "H₀ fałszywa", "text")
      ),
      cell_class = list(h0_true = c("is-best", "is-base"),
                        h0_false = c("is-base", "is-best")),
      prose = TRUE
    ),

    lc_p(gloss("błąd pierwszego rodzaju", "Błąd I rodzaju"), " to odrzucenie H₀,
      która jest prawdziwa, czyli fałszywy alarm. W analogii sądowej oznacza
      skazanie niewinnego, w nauce ogłoszenie efektu, którego nie ma.
      Prawdopodobieństwo tego błędu oznaczamy \\(\\alpha\\) i ustalamy je sami,
      wybierając ", gloss("poziom istotności"), ". Najczęściej przyjmuje się
      \\(\\alpha = 0.05\\)."),

    lc_p(gloss("błąd drugiego rodzaju", "Błąd II rodzaju"), " to nieodrzucenie H₀,
      która jest fałszywa, czyli przegapiony efekt. W analogii sądowej oznacza
      uniewinnienie winnego, w nauce przeoczenie zależności, która istnieje.
      Jego prawdopodobieństwo oznaczamy \\(\\beta\\). Tej wartości nie wybieramy
      bezpośrednio: zależy od tego, jak duży jest prawdziwy efekt, jak duży jest
      rozrzut danych i jak liczna jest próba. Dopełnienie \\(\\beta\\), czyli
      prawdopodobieństwo wykrycia efektu, który naprawdę istnieje, nazywamy ",
      gloss("moc testu", "mocą testu"), "."),

    lc_formula_box(withMathJax(
      "$$\\begin{aligned}
      \\alpha &= P(\\text{odrzucamy } H_0 \\mid H_0 \\text{ prawdziwa}) \\\\
      \\beta &= P(\\text{nie odrzucamy } H_0 \\mid H_0 \\text{ fałszywa}) \\\\
      \\text{moc} = 1 - \\beta &= P(\\text{odrzucamy } H_0 \\mid H_0 \\text{ fałszywa})
      \\end{aligned}$$"
    )),

    lc_p("Oba prawdopodobieństwa są warunkowe i opisują procedurę, a nie pojedynczy
      wynik, podobnie jak ", gloss("poziom ufności"), " w wykładzie 03. \\(\\alpha = 0.05\\)
      znaczy, że gdyby H₀ była prawdziwa, test zastosowany do wielu prób
      odrzucałby ją średnio w 5 przypadkach na 100. Nie znaczy, że konkretna
      decyzja jest błędna z prawdopodobieństwem 5%."),

    lc_p("Poziom istotności, tak jak poziom ufności, jest umową. Konwencja
      \\(\\alpha = 0.05\\) nie wynika z żadnego twierdzenia. Ustala się ją przed
      analizą danych i dobiera do kosztów pomyłki. Przy planowaniu badań
      przyjmuje się zwykle także, że moc ma wynosić co najmniej 0.80, czyli
      \\(\\beta \\leq 0.20\\). Schemat poniżej zestawia oba błędy w jednym
      obrazie."),

    div(style = "text-align: center; margin: 15px 0;",
      tags$img(src = "assets/type-error.jpg",
               alt = "Schemat błędu pierwszego i drugiego rodzaju zestawiony z decyzją testu i stanem rzeczywistym.",
               style = "width: 100%; border-radius: 8px;")
    ),

    lc_h2("ch1-moc", "Wizualizacja α, β i mocy testu"),

    lc_p("Definicje \\(\\alpha\\) i \\(\\beta\\) łatwiej zrozumieć na wykresie.
      Panel poniżej rysuje dwa ", gloss("rozkład próbkowy", "rozkłady próbkowe"),
      " średniej koncentracji. Niebieska krzywa to rozkład średniej z próby, gdy
      prawdziwa jest H₀ i średnia w populacji wynosi 70 pkt. Burgundowa krzywa
      to rozkład średniej, gdy prawdziwa jest Hₐ i średnia jest większa o różnicę
      ustawioną suwakiem. Odchylenie standardowe wyników w populacji wynosi
      w panelu 13 pkt, więc szerokość obu krzywych wyznacza ",
      gloss("błąd standardowy"), " \\(13/\\sqrt{n}\\)."),

    lc_p("Przerywane pionowe linie to ", gloss("wartość krytyczna", "wartości krytyczne"),
      ". Wyznacza je samo \\(\\alpha\\): w ", gloss("test dwustronny", "teście dwustronnym"),
      " odcinają po \\(\\alpha/2\\) w każdym ogonie niebieskiej krzywej. Średnia
      z próby, która wypadnie poza nimi, trafia do ",
      gloss("obszar odrzucenia", "obszaru odrzucenia"), " i prowadzi do odrzucenia
      H₀. Burgundowe pole pod niebieską krzywą w obu ogonach to \\(\\alpha\\).
      Zielone pole pod burgundową krzywą poza liniami krytycznymi to moc.
      Niezacieniowana część burgundowej krzywej między liniami to \\(\\beta\\):
      prawdopodobieństwo, że mimo prawdziwego efektu średnia z próby nie wyjdzie
      poza wartości krytyczne."),

    figure_panel(
      label = "Ryc. 3.1",
      title = "Moc testu i błędy",
      lc_toolbar(
        lc_slider("ch1_alpha", "α (poziom istotności)", 0.01, 0.20, 0.05, 0.01),
        lc_slider("ch1_effect", "Odległość μ od μ₀ (pkt)", 0, 15, 7, 1),
        lc_slider("ch1_power_n", "n (liczebność próby)", 10, 200, 40, 5),
        lc_readouts(uiOutput("ch1_power_stats"))
      ),
      lc_plot("ch1_power_plot", ratio = "1.6/1", max_height = "380px")
    ),

    lc_p("Przy ustawieniach startowych (\\(\\alpha = 0.05\\), różnica 7 pkt,
      n = 40) błąd standardowy wynosi 2.06 pkt, a wartości krytyczne leżą przy
      65.97 i 74.03 pkt. Moc wynosi 92.6%, a \\(\\beta\\) 7.4%. Trzy suwaki
      pokazują trzy mechanizmy. Zmniejszenie \\(\\alpha\\) do 0.01 odsuwa wartości
      krytyczne od środka: fałszywych alarmów jest mniej, ale moc spada do 79.7%.
      Mniejsza różnica zbliża krzywe do siebie: przy 3 pkt moc wynosi tylko 30.9%.
      Większa próba zwęża obie krzywe, bo błąd standardowy maleje jak
      \\(1/\\sqrt{n}\\): przy n = 10 moc to 39.9%, przy n = 100 ponad 99.9%."),

    lc_p("Wynika z tego ten sam kompromis co przy czujniku dymu. Przy ustalonej
      próbie zmniejszenie \\(\\alpha\\) zwiększa \\(\\beta\\). Oba błędy naraz
      zmniejsza tylko większa próba. Dlatego liczebność próby planuje się przed
      badaniem, tak żeby moc dla efektu, który uznajemy za praktycznie ważny,
      była wystarczająca."),

    lc_p("Panel opisuje najprostszy test: średnią jednej próby porównujemy ze
      znaną wartością \\(\\mu_0 = 70\\) pkt (takim testem zajmie się rozdział 04).
      Eksperyment z telefonem porównuje jednak dwie grupy. Przy dwóch grupach po n osób błąd
      standardowy różnicy średnich jest około \\(\\sqrt{2}\\) razy większy, więc
      moc jest niższa. W eksperymencie z rozdziału 01 wyniki losujemy z populacji,
      których średnie różnią się o 7 pkt, a odchylenia standardowe wynoszą 12
      i 14 pkt. Test porównujący dwie grupy po 40 osób ma dla takich danych moc
      około 67%: mniej więcej co trzecie powtórzenie eksperymentu nie wykryje
      różnicy, która naprawdę istnieje."),

    # ========================================================================
    # SEKCJA 2: P-wartość (po błędach — bo α jest już zdefiniowane)
    # ========================================================================
    lc_h2("ch1-pvalue", "Co to jest p-wartość?"),

    lc_p("\\(\\alpha\\) i \\(\\beta\\) opisują test, zanim zobaczymy dane.
      Po eksperymencie mamy jednak jedną konkretną różnicę średnich i trzeba
      zdecydować, co z nią zrobić. Potrzebujemy liczby, która powie, jak bardzo
      ten wynik odstaje od tego, czego spodziewalibyśmy się przy prawdziwej H₀.
      Tą liczbą jest ", "p-wartość", "."),

    lc_p("p-wartość to prawdopodobieństwo, że gdyby H₀ była prawdziwa,
      otrzymalibyśmy wynik co najmniej tak skrajny jak zaobserwowany. Dla różnicy
      średnich w eksperymencie z telefonem:"),

    lc_formula_box(withMathJax(
      "$$p = P\\left(|\\bar{X}_A - \\bar{X}_B| \\geq |d_{\\text{obs}}| \\;\\middle|\\; H_0\\right)$$"
    )),

    lc_p("gdzie \\(d_{\\text{obs}}\\) to różnica średnich zaobserwowana w naszej
      próbie. Wartość bezwzględna oznacza, że „co najmniej tak skrajny” liczymy
      w obie strony. Hₐ mówi tylko, że telefon wpływa na koncentrację, bez
      wskazania kierunku, więc równie mocnym sygnałem byłaby różnica tej samej
      wielkości na korzyść grupy „biurko”. Wariant jednostronny, w którym liczy się
      tylko jeden ogon, omówiliśmy w poprzednim rozdziale."),

    lc_p("Definicję najłatwiej zrozumieć przez symulację. Wyobraźmy sobie, że
      eksperyment powtarzamy wiele razy w świecie, w którym telefon nie ma wpływu.
      Każde powtórzenie da inną różnicę średnich, bo każda grupa to inna losowa
      próba. Rozkład tych różnic pokazuje, jakie wyniki wytwarza sam przypadek,
      a p-wartość to odsetek powtórzeń, w których różnica wyszła co najmniej tak
      daleko od zera jak nasza."),

    lc_p("Panel poniżej losuje takie eksperymenty: obie grupy pochodzą z tej samej
      populacji o średniej 70 pkt i odchyleniu standardowym 13 pkt. Każdy słupek
      histogramu zlicza różnice średnich z symulowanych eksperymentów. Burgundowa
      linia ciągła to różnica z eksperymentu z rozdziału 01, przerywana to jej
      lustrzane odbicie po drugiej stronie zera. Bursztynowe słupki to różnice
      co najmniej tak skrajne jak nasza."),

    figure_panel(
      label = "Ryc. 3.2",
      title = "Powtórzone eksperymenty pod H₀",
      lc_toolbar(
        lc_action_group(ch1_sim_10 = "10×", ch1_sim_200 = "200×", label = "Powtórz eksperyment"),
        lc_action("ch1_sim_reset", icon = "reset", variant = "ghost", aria_label = "Reset"),
        lc_readouts(uiOutput("ch1_sim_info"))
      ),
      lc_plot("ch1_sim_plot", max_height = "350px"),
      lc_caption("Każdy eksperyment: dwie grupy po 40 osób z tej samej populacji, jak w rozdziale 01.")
    ),

    lc_p("Przy 40 osobach w grupie losowe różnice mają odchylenie standardowe
      około 2.9 pkt, a w 95% eksperymentów bez efektu mieszczą się między -5.7
      a 5.7 pkt. Różnica 7 pkt zdarza się bez efektu rzadko, jej p-wartość wynosi
      około 0.016. Różnica 4 pkt dałaby p ≈ 0.17, czyli wynik zupełnie zwyczajny
      w świecie bez efektu. Przy kilkuset symulacjach odsetek bursztynowych
      słupków zbliża się do teoretycznej p-wartości. Przy dziesięciu mocno skacze,
      tak jak pokrycie przedziałów ufności w wykładzie 03."),

    lc_p("W praktyce nikt nie symuluje tysięcy eksperymentów. Wynik przelicza się
      na ", gloss("statystyka testowa", "statystykę testową"), ", czyli liczbę,
      której rozkład przy prawdziwej H₀ znamy z teorii. Kolejne rozdziały wprowadzą
      takie statystyki po kolei: t, χ² i F. p-wartość jest wtedy polem pod krzywą
      tego rozkładu w ogonach, za wartością statystyki obliczoną z próby. Wykres
      poniżej pokazuje to dla statystyki o ", gloss("standardowy rozkład normalny", "standardowym rozkładzie normalnym"), "
      i wyniku 2.17."),

    figure_panel(
      label = "Ryc. 3.3",
      title = "p-wartość jako pole ogonów",
      div(class = "ws-chart-wrap",
        tags$canvas(id = "ch1_pvalue_chart")
      )
    ),

    lc_p("Zacieniowane pole w obu ogonach, na lewo od -2.17 i na prawo od 2.17,
      wynosi 0.030. Tyle wynosi p-wartość tego wyniku w teście dwustronnym. Im
      dalej od zera leży statystyka, tym mniejsze pole w ogonach i tym mniejsza
      p-wartość."),

    lc_p("Definicja p-wartości jest krótka, ale łatwo ją przekręcić. Sprawdź,
      która z trzech interpretacji wyniku p = 0.03 jest poprawna."),

    figure_panel(
      label = "Ryc. 3.4",
      title = "Co naprawdę oznacza p-wartość?",
      p("Załóżmy, że w badaniu wyszło p = 0.03. Które zdanie jest poprawną
        interpretacją?"),
      tags$div(class = "lc-choices", `data-correct` = "tail_prob",
        radioButtons("ch1_pvalue_meaning", NULL,
          choices = c(
            "Jest 3% szans, że H₀ jest prawdziwa." = "h0_prob",
            "Jest 3% szans, że wynik jest przypadkowy." = "random_prob",
            "Gdyby H₀ była prawdziwa, taki lub bardziej skrajny wynik pojawiłby się w 3% powtórzeń." = "tail_prob"
          ),
          selected = character(0)
        )
      ),
      uiOutput("ch1_pvalue_meaning_feedback")
    ),

    lc_p("Najczęstszy błąd polega na odwróceniu warunku. p-wartość liczymy przy
      założeniu, że H₀ jest prawdziwa, więc nie może jednocześnie mówić, z jakim
      prawdopodobieństwem H₀ jest prawdziwa. P(wynik co najmniej tak skrajny | H₀)
      to inna wielkość niż P(H₀ | wynik), tak jak prawdopodobieństwo, że zawodowy
      koszykarz jest wysoki, to coś innego niż prawdopodobieństwo, że wysoki
      człowiek jest zawodowym koszykarzem. Z tego samego powodu p = 0.03 nie
      oznacza 3% szans, że wynik jest dziełem przypadku: „wynik jest przypadkowy”
      to tylko inne sformułowanie H₀."),

    lc_p("Mała p-wartość mówi więc tyle, że dane byłyby rzadkie, gdyby H₀ była
      prawdziwa. Nie mówi też, jak duży jest efekt: przy bardzo licznej próbie
      nawet znikoma różnica daje małą p-wartość. Wielkością efektu zajmuje się
      rozdział 10."),

    # ========================================================================
    # SEKCJA 3: Quiz — decyzja
    # ========================================================================
    lc_h2("ch1-decyzja", "Decyzja w praktyce"),

    lc_p("Pozostaje połączyć p-wartość z poziomem istotności. Reguła decyzyjna
      jest prosta: jeśli \\(p < \\alpha\\), odrzucamy H₀; jeśli
      \\(p \\geq \\alpha\\), nie mamy podstaw do jej odrzucenia. p-wartość spada
      poniżej \\(\\alpha\\) dokładnie wtedy, gdy wynik trafia do obszaru
      odrzucenia, więc ta reguła odrzuca prawdziwą H₀ z prawdopodobieństwem
      \\(\\alpha\\). Werdykt formułuje się w ustalony sposób. Gdy
      \\(p < \\alpha\\):"),

    lc_formula_box(
      tags$em("„Na przyjętym poziomie istotności α odrzucamy hipotezę zerową
              na rzecz hipotezy alternatywnej.”")
    ),

    lc_p("Gdy \\(p \\geq \\alpha\\):"),

    lc_formula_box(
      tags$em("„Na przyjętym poziomie istotności α nie mamy podstaw do
              odrzucenia hipotezy zerowej.”")
    ),

    lc_p("Drugi werdykt jest celowo ostrożny. Nie mówimy „H₀ jest prawdziwa”, bo
      brak dowodu efektu nie jest dowodem jego braku. Efekt może istnieć, a próba
      mogła być za mała, by go wykryć. To właśnie błąd II rodzaju, a przy małej
      mocy jego prawdopodobieństwo jest duże."),

    lc_p("Formalny werdykt trzeba jeszcze przetłumaczyć z powrotem na język ", gloss("pytanie badawcze", "pytania
      badawczego"), ". W eksperymencie z telefonem po odrzuceniu H₀ piszemy: „średnia
      koncentracja studentów z telefonem na biurku istotnie statystycznie różni się
      od średniej koncentracji studentów z telefonem w plecaku”, a nie
      „odrzuciliśmy hipotezę zerową”. Raport podaje też samą p-wartość i poziom
      istotności, a nie tylko werdykt."),

    lc_p("Decyzja testu łączy się bezpośrednio z przedziałami ufności z wykładu 03.
      Poziomowi istotności \\(\\alpha\\) odpowiada poziom ufności
      \\(1 - \\alpha\\). Test dwustronny na poziomie 0.05 odrzuca H₀ o braku
      różnicy średnich dokładnie wtedy, gdy odpowiadający mu 95% przedział
      ufności dla różnicy nie obejmuje zera. Przedział mówi przy tym więcej niż
      sam werdykt, bo pokazuje także, jak duża może być różnica."),

    lc_note("Zasada", rule = TRUE,
      "Poziom istotności ustal przed analizą danych. W raporcie podawaj p-wartość
       obok werdyktu, a werdykt formułuj w języku pytania badawczego."
    ),

    lc_p("Na koniec poćwicz samą decyzję. Każdy scenariusz podaje p-wartość
      i przyjęty poziom istotności."),

    figure_panel(
      label = "Ryc. 3.5",
      title = "Quiz: odrzucić czy nie?",
      uiOutput("ch1_quiz_scenario"),
      p("Decyzja:"),
      uiOutput("ch1_quiz_options"),
      uiOutput("ch1_quiz_feedback"),
      lc_action("ch1_quiz_next", "Nowy scenariusz", variant = "outline")
    ),

    lc_p("Reguła \\(p < \\alpha\\) wyznacza ostrą granicę, choć siła dowodów
      zmienia się płynnie. Wyniki p = 0.048 i p = 0.052 prowadzą przy
      \\(\\alpha = 0.05\\) do przeciwnych werdyktów, a przemawiają przeciw H₀
      niemal tak samo mocno. To kolejny powód, by w raporcie podawać samą
      p-wartość. W kolejnych rozdziałach ten sam schemat — hipotezy, statystyka
      testowa, p-wartość, decyzja — zastosujemy do konkretnych testów, zaczynając
      od ", gloss("test t", "testu t"), " dla jednej średniej."),

    lc_chapter_next(
      num       = "04",
      title     = "Test t jednej próby",
      lead      = "pierwszy konkretny test — średnia wobec wartości referencyjnej.",
      target_id = "ch-jedna-ilosciowa"
    )
  )
)


# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  observe({
    session$sendCustomMessage("ws_pvalue_chart", list(
      id = "ch1_pvalue_chart",
      stat = 2.17
    ))
  })

  # --- Sekcja 0: Case study ---
  ch1_case_data <- reactiveVal(NULL)

  observeEvent(input$ch1_case_generate, {
    ch1_case_data(generate_phone_data(40))
  })

  # Generuj dane na starcie
  observe({
    if (is.null(ch1_case_data())) {
      ch1_case_data(generate_phone_data(40))
    }
  })

  zoom_plot_server("ch1_case_plot", reactive({
    d <- ch1_case_data()
    if (is.null(d)) return(NULL)

    ggplot(d, aes(x = grupa, y = koncentracja, fill = grupa, color = grupa)) +
      geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5) +
      geom_jitter(width = 0.15, alpha = 0.5, size = 2) +
      scale_fill_manual(values = c(col_accept, col_pvalue)) +
      scale_color_manual(values = c(col_accept, col_pvalue)) +
      labs(
           x = NULL, y = "Wynik (0–100 pkt)") +
      theme(legend.position = "none") +
      coord_cartesian(ylim = c(20, 100))
  }))

  output$ch1_case_stats <- renderUI({
    d <- ch1_case_data()
    if (is.null(d)) return(NULL)

    stats <- d %>%
      group_by(grupa) %>%
      summarise(m = round(mean(koncentracja), 1),
                s = round(sd(koncentracja), 1),
                .groups = "drop")

    diff_val <- round(stats$m[1] - stats$m[2], 1)

    tagList(
      lc_readout("Plecak", paste0(stats$m[1], " pkt"), color = col_accept),
      lc_readout("Biurko", paste0(stats$m[2], " pkt"), color = col_pvalue),
      lc_readout("Różnica", paste0(diff_val, " pkt"), color = upwr_secondary)
    )
  })

  # Obserwowana różnica średnich z case study
  ch1_observed_diff <- reactive({
    d <- ch1_case_data()
    if (is.null(d)) return(0)
    means <- tapply(d$koncentracja, d$grupa, mean)
    unname(means["Telefon w plecaku"] - means["Telefon na biurku"])
  })

  # --- Widget 1: Histogram różnic z powtórzonych eksperymentów ---
  ch1_sim_diffs <- reactiveVal(numeric(0))

  do_simulations <- function(k) {
    n <- 40  # jak w eksperymencie z rozdziału 01
    new_diffs <- sapply(seq_len(k), function(i) {
      g1 <- rnorm(n, mean = 70, sd = 13)
      g2 <- rnorm(n, mean = 70, sd = 13)
      mean(g1) - mean(g2)
    })
    ch1_sim_diffs(c(ch1_sim_diffs(), new_diffs))
  }

  observeEvent(input$ch1_sim_10, do_simulations(10))
  observeEvent(input$ch1_sim_200, do_simulations(200))
  observeEvent(input$ch1_sim_reset, ch1_sim_diffs(numeric(0)))

  output$ch1_sim_info <- renderUI({
    n_s <- length(ch1_sim_diffs())
    obs <- round(ch1_observed_diff(), 1)
    diffs <- ch1_sim_diffs()
    tagList(
      lc_readout("Eksperymentów", n_s, color = col_h0),
      lc_readout("Obs. różnica", paste0(obs, " pkt"), color = col_reject),
      lc_readout("p z symulacji",
                 if (n_s) sprintf("%.3f", mean(abs(diffs) >= abs(ch1_observed_diff()))) else "—",
                 color = col_pvalue)
    )
  })

  zoom_plot_server("ch1_sim_plot", reactive({
    diffs <- ch1_sim_diffs()
    obs <- ch1_observed_diff()

    if (length(diffs) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij „Powtórz” —\nsymulujemy eksperymenty bez efektu",
                 size = 5, color = upwr_reference) +
        theme_void()
    } else {
      df <- data.frame(diff = diffs, extreme = abs(diffs) >= abs(obs))
      ggplot(df, aes(x = diff, fill = extreme)) +
        geom_histogram(bins = 30, color = "white") +
        geom_vline(xintercept = obs, color = col_reject,
                   linewidth = 1.5, linetype = "solid") +
        geom_vline(xintercept = -obs, color = col_reject,
                   linewidth = 1, linetype = "dashed") +
        scale_fill_manual(values = c("TRUE" = col_pvalue, "FALSE" = col_h0),
                          labels = c("TRUE" = "co najmniej tak skrajne",
                                     "FALSE" = "bliżej zera"),
                          name = NULL) +
        labs(
             
             x = "Różnica średnich (grupa A − grupa B)", y = "Liczba") +
                theme(legend.position = "top")
    }
  }))

  output$ch1_pvalue_meaning_feedback <- renderUI({
    choice <- input$ch1_pvalue_meaning
    if (is.null(choice) || identical(choice, character(0))) return(NULL)

    if (identical(choice, "tail_prob")) {
      lc_status(
        lc_verdict(tags$strong("Tak."), type = "ok"),
        " p-wartość zakłada, że H₀ jest prawdziwa, i mówi, jak często wynik
        byłby co najmniej tak skrajny jak nasz."
      )
    } else {
      lc_status(
        lc_verdict(tags$strong("Nie."), type = "danger"),
        " p-wartość liczymy przy założeniu, że H₀ jest prawdziwa, więc nie jest
        prawdopodobieństwem H₀ ani „przypadkowości” wyniku. To prawdopodobieństwo
        wyniku co najmniej tak skrajnego przy prawdziwej H₀."
      )
    }
  })

  # --- Widget 2: Moc testu ---
  zoom_plot_server("ch1_power_plot", reactive({
    alpha <- input$ch1_alpha
    diff_means <- input$ch1_effect  # różnica średnich w punktach
    n <- input$ch1_power_n
    sigma <- 13  # stałe odchylenie standardowe (ukryte)
    mu0 <- 70
    mu1 <- mu0 + diff_means

    x <- seq(mu0 - 4 * sigma / sqrt(n), max(mu1, mu0) + 4 * sigma / sqrt(n), length.out = 500)
    se <- sigma / sqrt(n)

    y_h0 <- dnorm(x, mean = mu0, sd = se)
    y_h1 <- dnorm(x, mean = mu1, sd = se)

    crit_low <- mu0 + qnorm(alpha / 2) * se
    crit_high <- mu0 + qnorm(1 - alpha / 2) * se

    df_plot <- data.frame(
      x = rep(x, 2),
      y = c(y_h0, y_h1),
      dist = rep(c("H₀: μ = 70", "Hₐ: μ = 70 + odległość"), each = 500)
    )

    p <- ggplot(df_plot, aes(x = x, y = y, color = dist)) +
      geom_line(linewidth = 1.2) +
      geom_vline(xintercept = c(crit_low, crit_high), linetype = "dashed",
                 color = upwr_secondary) +
      scale_color_manual(values = c(col_h0, col_h1), name = "Rozkład") +
      labs(
           x = "Średnia koncentracja w próbie", y = "Gęstość") +
      theme(legend.position = "top")

    # Obszar odrzucenia H0 w teście dwustronnym.
    h0_tail <- x <= crit_low | x >= crit_high
    shade_h0 <- data.frame(
      x = x[h0_tail],
      y = y_h0[h0_tail],
      tail = ifelse(x[h0_tail] <= crit_low, "lewy", "prawy")
    )
    if (nrow(shade_h0) > 0) {
      p <- p + geom_area(data = shade_h0, aes(x = x, y = y, group = tail),
                         fill = col_reject, alpha = 0.2, inherit.aes = FALSE)
    }

    h1_tail <- x <= crit_low | x >= crit_high
    shade_h1 <- data.frame(
      x = x[h1_tail],
      y = y_h1[h1_tail],
      tail = ifelse(x[h1_tail] <= crit_low, "lewy", "prawy")
    )
    if (nrow(shade_h1) > 0) {
      p <- p + geom_area(data = shade_h1, aes(x = x, y = y, group = tail),
                         fill = col_accept, alpha = 0.2, inherit.aes = FALSE)
    }

    p
  }))

  output$ch1_power_stats <- renderUI({
    alpha <- input$ch1_alpha
    diff_means <- input$ch1_effect
    n <- input$ch1_power_n
    sigma <- 13
    se <- sigma / sqrt(n)
    mu0 <- 70
    mu1 <- mu0 + diff_means
    crit_low <- mu0 + qnorm(alpha / 2) * se
    crit_high <- mu0 + qnorm(1 - alpha / 2) * se
    power <- pnorm(crit_low, mean = mu1, sd = se) +
      (1 - pnorm(crit_high, mean = mu1, sd = se))

    tagList(
      lc_readout("Moc", paste0(round(power * 100, 1), "%"), color = col_accept),
      lc_readout("Błąd II", paste0(round((1 - power) * 100, 1), "%"), color = upwr_secondary)
    )
  })

  # --- Widget 3: Quiz (tiles) ---
  ch1_quiz_data <- reactiveVal(NULL)
  ch1_quiz_answered <- reactiveVal(FALSE)
  ch1_quiz_selected <- reactiveVal(NULL)

  generate_quiz <- function() {
    scenarios <- list(
      list(p = 0.003, alpha = 0.05,
           context = "Badanie wpływu kawy na czas reakcji: p = 0.003, α = 0.05"),
      list(p = 0.12, alpha = 0.05,
           context = "Czy notatki odręczne dają lepsze wyniki niż notatki na laptopie? p = 0.12, α = 0.05"),
      list(p = 0.048, alpha = 0.05,
           context = "Korelacja między długością snu a oceną z egzaminu: p = 0.048, α = 0.05"),
      list(p = 0.06, alpha = 0.01,
           context = "Czy kierunek studiów wpływa na zarobki po 5 latach? ANOVA: p = 0.06, α = 0.01"),
      list(p = 0.001, alpha = 0.01,
           context = "Czy płeć wpływa na wybór specjalizacji? Test χ²: p = 0.001, α = 0.01"),
      list(p = 0.052, alpha = 0.05,
           context = "Porównanie skuteczności dwóch metod nauki: p = 0.052, α = 0.05")
    )
    ch1_quiz_data(scenarios[[sample(length(scenarios), 1)]])
    ch1_quiz_answered(FALSE)
    ch1_quiz_selected(NULL)
  }

  observe({ generate_quiz() })
  observeEvent(input$ch1_quiz_next, { generate_quiz() })

  output$ch1_quiz_scenario <- renderUI({
    sc <- ch1_quiz_data()
    if (is.null(sc)) return(NULL)
    lc_status(
      p(tags$strong("Scenariusz:"), " ", sc$context)
    )
  })

  ch1_quiz_choices <- list(
    list(letter = "A", value = "reject", text = "Odrzucamy H₀"),
    list(letter = "B", value = "fail_to_reject", text = "Brak podstaw do odrzucenia H₀")
  )

  output$ch1_quiz_options <- renderUI({
    ch1_quiz_data()
    if (ch1_quiz_answered()) return(NULL)
    div(class = "quiz-tiles quiz-cols-2",
      lapply(ch1_quiz_choices, function(opt) {
        actionButton(paste0("ch1_tile_", opt$value),
          tagList(
            div(class = "tile-letter", opt$letter),
            div(class = "tile-text", opt$text)
          ),
          class = "quiz-tile"
        )
      })
    )
  })

  observe({
    for (opt in ch1_quiz_choices) {
      local({
        val <- opt$value
        observeEvent(input[[paste0("ch1_tile_", val)]], {
          if (ch1_quiz_answered()) return()
          ch1_quiz_selected(val)
          ch1_quiz_answered(TRUE)
        }, ignoreInit = TRUE)
      })
    }
  })

  output$ch1_quiz_feedback <- renderUI({
    req(ch1_quiz_answered())
    sc <- ch1_quiz_data()
    answer <- ch1_quiz_selected()
    if (is.null(sc) || is.null(answer)) return(NULL)

    correct <- if (sc$p < sc$alpha) "reject" else "fail_to_reject"
    fmt <- function(x) format(x)
    comparison <- paste0("p = ", fmt(sc$p), " ", ifelse(sc$p < sc$alpha, "<", "≥"),
                         " α = ", fmt(sc$alpha))
    if (answer == correct) {
      lc_status(
        lc_verdict(tags$strong("Poprawnie."), type = "ok"),
        p(comparison)
      )
    } else {
      lc_status(
        lc_verdict(tags$strong("Niepoprawnie."), type = "danger"),
        p(paste0(comparison, ", zatem ",
                 ifelse(correct == "reject", "odrzucamy H₀",
                        "nie mamy podstaw do odrzucenia H₀"), "."))
      )
    }
  })
}
