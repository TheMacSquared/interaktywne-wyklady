# ============================================================================
# CHAPTER 2: Wartość oczekiwana i wariancja
# ============================================================================

ch2_ev_var_ui <- list(
  id = "ch-ev-var", num = "02", title = "Wartość oczekiwana i wariancja",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 02 · Rozkłady prawdopodobieństwa",
      num    = "02",
      title  = "Wartość oczekiwana i wariancja.",
      lead   = "Rozkład prawdopodobieństwa wymienia wszystkie możliwe wyniki, ale do
                porównań potrzebny jest krótszy opis. Dane streszczaliśmy średnią
                i odchyleniem standardowym. Rozkład streszczają ich odpowiedniki:
                wartość oczekiwana mówi, na jaki wynik można liczyć w długim okresie,
                a wariancja — jak daleko od niego wypadają pojedyncze wyniki."
    ),

    lc_h2("ch2-ev-intro", "Od średniej do wartości oczekiwanej"),

    lc_p("W poprzednim rozdziale ", gloss("rozkład prawdopodobieństwa"), " opisywał
      ", gloss("zmienna losowa", "zmienną losową"), " w całości: podawał jej możliwe wartości i prawdopodobieństwo
      każdej z nich. Taki opis jest kompletny, ale niewygodny, gdy chcemy porównać
      dwa rozkłady. Ten sam problem mieliśmy w wykładzie 01 z danymi. ", gloss("histogram", "Histogram"), "
      pokazywał cały rozkład, a do porównań streszczaliśmy go dwiema liczbami:
      ", gloss("średnia", "średnią"), " i odchyleniem standardowym. Teraz zrobimy
      to samo z rozkładem prawdopodobieństwa."),

    lc_p("Średnią z danych rozumieliśmy jako punkt równowagi. Każda obserwacja
      to jednakowy ciężarek na linijce, a linijka balansuje w punkcie średniej.
      Rozkład nie składa się z obserwacji, tylko z wartości i ich
      prawdopodobieństw. Wystarczy więc, że ciężarek postawiony przy wartości x
      waży tyle, ile wynosi P(X = x). Punkt równowagi tak obciążonej linijki
      to ", gloss("wartość oczekiwana"), " E(X): suma wszystkich wartości
      pomnożonych przez ich prawdopodobieństwa."),

    lc_formula_annotated(
      "⟦1|E(X)⟧ = ⟦2|\\sum_x⟧ ⟦3|x⟧ \\cdot ⟦4|P(X = x)⟧",
      notes  = list("wartość oczekiwana", "suma po wartościach", "możliwa wartość", "prawdopodobieństwo (waga)"),
      colors = c("accent", "bursztyn", "niebo", "indygo"),
      above  = c(2, 4)
    ),

    lc_p("Wartość oczekiwana jest więc średnią ważoną: wartości prawdopodobne
      ważą w niej dużo, mało prawdopodobne — mało. Nie musi przy tym być wartością,
      którą zmienna może przyjąć. Dla rzutu kostką każda liczba oczek ma
      prawdopodobieństwo 1/6, więc E(X) = (1 + 2 + 3 + 4 + 5 + 6) · 1/6 = 3.5,
      choć 3.5 oczka nigdy nie wypada."),

    # ========================================================================
    # WIDGET 1: Loterie — symulacja wartości oczekiwanej
    # ========================================================================
    lc_h2("ch2-loterie", "Czego się spodziewać? — gry losowe"),

    lc_p("Zacznijmy od zdrapki kupionej w kiosku."),

    # SCENA (prototyp 2026-10-08, zaakceptowany 2026-10-29): Zdrap los (E(X) na dłuższą metę, ryzyko jako rozrzut)
    figure_panel(
      label = "Ryc. 2.1",
      width_mode = "text",
      scene_widget("ch2_zdrapka", "Zdrap los: od jednego losu do wartości oczekiwanej",
        steps = c("Bilans X", "Powtarzamy", "E(X) i rozrzut"),
        labels = c("Kup i zdrap los", "Kup i zdrap los", "Kup i zdrap los"),
        more_from = 2,
        config = list(kind = "scratch", ticket = "main", price = scene_ticket_price, height = 436,
                      tickets = lapply(scene_tickets, function(t) {
                        t$prizes <- I(t$prizes); t$probs <- I(t$probs); t }),
                      aria = "Kupujący zdrapuje los z kiosku; histogram bilansów (wygrana minus cena) i bieżący średni bilans na tle wartości oczekiwanej"))
    ),

    lc_p("Nazwa „oczekiwana” bierze się z gier losowych. Wartość oczekiwana
      wygranej mówi, ile średnio przynosi jedna gra, jeśli gramy wiele razy.
      Panel pozwala zagrać w trzy prawdziwe gry: ruletkę, zdrapkę z poprzedniego
      panelu i Lotto. Dla każdej znamy wygrane i ich prawdopodobieństwa, więc
      E(X) liczymy ze wzoru; przełącznik „Wartość oczekiwana” pokazuje obliczenie
      i linię E na wykresie. Gry mają różne stawki, więc wykres pokazuje, ile
      średnio wraca z każdej postawionej złotówki."),

    # Ryc. 2.2: prototyp 2a (2026-10-29) zastąpił dawną „Grę w loterie” A–D
    s3p_2a_ui,

    lc_p("Po kilku grach średnia skacze. W zdrapce pierwsza gra daje 0, 4, 10
      albo 100 zł, nigdy 3.80 zł. Z każdą kolejną grą pojedynczy wynik waży
      w średniej coraz mniej, wahania słabną, a linia zbliża się do E(X). To ",
      gloss("prawo wielkich liczb"), " z poprzedniego rozdziału, tym razem dla
      średniej zamiast częstości: średnia z wielu powtórzeń zbliża się do wartości
      oczekiwanej. Tempo zależy od gry. Ruletka siada na E(X) prawie od razu,
      zdrapka po kilkuset grach, a Lotto nawet po dziesięciu tysiącach zakładów
      leży zwykle poniżej, bo część wartości oczekiwanej to jackpot, który trafia
      się raz na 14 milionów zakładów."),

    lc_p("Jeśli liczy się tylko średni zwrot, najlepiej wypada ruletka: z każdej
      złotówki wraca 97 groszy, a w Lotto tylko 35. Mimo to Lotto ma rzesze
      graczy, bo sprzedaje szansę na wygraną, która zmienia życie. Wartość
      oczekiwana tej różnicy nie widzi. Wrócimy do niej za chwilę, przy wariancji."),

    lc_p("Ta sama luka działa w drugą stronę przy ubezpieczeniach. Ubezpieczyciel,
      tak jak kasyno, musi na dłuższą metę zarabiać, więc dla klienta polisa ma
      ujemną wartość oczekiwaną. Panel pokazuje rok z telefonem za 3000 zł bez
      polisy i z polisą, a suwak zmienia wysokość składki."),

    # Ryc. 2.3: prototyp 2c (2026-10-29), przypadek brzegowy E(X)
    s3p_2c_ui,

    # ========================================================================
    # WIDGET 3: Ryzyko a rozrzut — intuicja wariancji
    # ========================================================================
    lc_h2("ch2-wariancja", "Wariancja — rozrzut wokół oczekiwania"),

    lc_p("Zacznijmy od sytuacji, w której sama średnia nie wystarcza. Na egzamin
      prowadzą dwie trasy: obwodnica, gdzie przejazd trwa prawie zawsze tyle samo,
      i trasa przez centrum, średnio szybsza, ale nieprzewidywalna. Suwak ustawia,
      ile czasu zostało do egzaminu."),

    # Ryc. 2.4: prototyp 2b (2026-10-29), wprowadzenie wariancji
    s3p_2b_ui,

    lc_p("Ruletka, zdrapka i Lotto różnią się nie tylko wartością oczekiwaną.
      W ruletce każda wygrana leży blisko E(X): 0 albo 2 zł przy E(X) ≈ 0.97 zł.
      W zdrapce wygrana 100 zł leży aż 96 zł od E(X) = 3.80 zł.
      W wykładzie 01 taki rozrzut danych wokół średniej mierzyliśmy wariancją
      i odchyleniem standardowym. Dla rozkładu robimy to samo, tylko kwadraty
      odchyleń od E(X) ważymy prawdopodobieństwami, a nie dzielimy przez liczbę
      obserwacji. Tak powstaje ", gloss("wariancja"), " Var(X). Jej pierwiastek
      to ", gloss("odchylenie standardowe"), " SD(X), wyrażone w tych samych
      jednostkach co X."),

    lc_formula_annotated(
      "⟦1|Var(X)⟧ = ⟦2|\\sum_x⟧ \\big(⟦3|x⟧ - ⟦4|E(X)⟧\\big)^2 \\cdot ⟦5|P(X = x)⟧ \\qquad ⟦6|SD(X)⟧ = \\sqrt{Var(X)}",
      notes  = list("wariancja", "suma po wartościach", "możliwa wartość", "wartość oczekiwana", "prawdopodobieństwo", "odchylenie standardowe"),
      colors = c("wrzos", "bursztyn", "niebo", "accent", "indygo", "wrzos"),
      above  = c(2, 4, 6)
    ),

    lc_p("Dla ruletki: Var(X) = 18/37 · (2 − 0.97)² + 19/37 · (0 − 0.97)²
      ≈ 1.00 zł², więc SD(X) ≈ 1 zł. Dla zdrapki: Var(X) = 0.65 · (0 − 3.8)²
      + 0.25 · (4 − 3.8)² + 0.08 · (10 − 3.8)² + 0.02 · (100 − 3.8)² ≈ 197.6 zł²,
      więc SD(X) ≈ 14 zł, prawie cztery razy więcej niż E(X). Lotto ma SD około
      1340 zł przy stawce 3 zł i prawie cały ten rozrzut pochodzi z jackpotu.
      Dlatego średnia z Lotto w Ryc. 2.2 stabilizowała się tak wolno: im większa
      wariancja, tym więcej gier potrzeba, żeby średnia zbliżyła się do E(X)."),

    lc_p("Żeby oddzielić rozrzut od położenia, panel porównuje trzy loterie
      o tej samej wartości oczekiwanej, E(X) = 50 zł. Loteria A daje zawsze
      50 zł. Loteria B daje 0 albo 100 zł, każdą kwotę z prawdopodobieństwem 0.5.
      W loterii C rzucamy kostką i dostajemy (oczka − 1) · 20 zł, czyli 0, 20, 40,
      60, 80 albo 100 zł, każdą kwotę z prawdopodobieństwem 1/6. Każde kliknięcie
      to jedna gra w każdą z loterii, a kropka to jedna wypłata."),

    # Ryc. 2.5: prototyp 2.4-B (2026-10-29) zastąpił histogramy trzech loterii
    s3p_24b_ui,

    lc_p("Trzy chmury mają ten sam środek, a zupełnie różną szerokość. W loterii A
      wszystkie kropki leżą nad 50 zł: Var(X) = 0. W B każda wypłata leży 50 zł
      od E(X), więc Var(X) = 2500 zł², a SD(X) = 50 zł. W C wypłaty leżą od 10
      do 50 zł od E(X): Var(X) = (50² + 30² + 10²) · 2/6 ≈ 1167 zł², a SD(X)
      ≈ 34 zł. Klamry ±SD pokazują odchylenia policzone z rozegranych gier;
      przy setkach gier trzymają się blisko tych wartości teoretycznych."),

    lc_p("Przy grach losowych i inwestycjach odchylenie standardowe czyta się jako
      ryzyko. Mała wariancja oznacza wyniki skupione blisko E(X), duża — wyniki,
      które często i daleko od niej odbiegają. Zerowa wariancja oznacza brak
      losowości: wynik jest pewny. Pełny opis zmiennej losowej wymaga więc
      co najmniej dwóch liczb: E(X) mówi, gdzie leży środek, a SD(X), jak szeroko
      rozkładają się wokół niego wyniki."),

    lc_note("Zasada", rule = TRUE,
      "Wariancja ma jednostkę do kwadratu (zł²), odchylenie standardowe — tę samą
       co X (zł). Do opisu rozrzutu używaj SD, wariancja przydaje się w obliczeniach."
    ),

    # ========================================================================
    # Podsumowanie
    # ========================================================================
    lc_h2("ch2-od-danych", "Od danych do modelu"),

    lc_p("Każde pojęcie z tego rozdziału ma odpowiednik w ", gloss("statystyka opisowa", "statystyce opisowej"), "
      z wykładu 01. Wzory też są analogiczne: w średniej z próby każda obserwacja
      ma wagę 1/n, a w E(X) wartość x ma wagę P(X = x). Różnica leży w źródle
      liczb: statystyki z lewej kolumny liczymy z zebranych danych, parametry
      z prawej — z modelu, czyli z rozkładu prawdopodobieństwa."),

    lc_table(
      data.frame(
        what  = c("Środek", "Rozrzut (kwadrat)", "Rozrzut (jednostki)", "Źródło liczb"),
        data  = c("Średnia z próby x̄", "Wariancja z próby s²", "Odchylenie standardowe s",
                  "Zebrane dane"),
        model = c("Wartość oczekiwana E(X)", "Wariancja Var(X)", "Odchylenie standardowe SD(X)",
                  "Rozkład prawdopodobieństwa"),
        stringsAsFactors = FALSE
      ),
      cols = list(
        lc_col("what", "", "row"),
        lc_col("data", "Dane", "text"),
        lc_col("model", "Model", "text")
      ),
      prose = TRUE,
      label = "Porównanie statystyki opisowej i modelu",
      caption = "Statystyka opisowa liczy wielkości z danych, rachunek prawdopodobieństwa wyznacza ich odpowiedniki z modelu."
    ),

    lc_p("Obie kolumny łączy prawo wielkich liczb, które widzieliśmy przy
      ruletce, zdrapce i Lotto: im więcej obserwacji, tym bliżej x̄ leży E(X). Dlatego
      statystyki z próby mogą służyć do szacowania parametrów rozkładu, z którego
      dane pochodzą."),

    lc_chapter_next(
      num       = "03",
      title     = "Rozkłady dyskretne",
      lead      = "jak E(X) i Var(X) zależą od parametrów konkretnych rozkładów.",
      target_id = "ch-dyskretne"
    )
  )
)

# --------------------------------------------------------------------------
# Chapter 2 Server
# --------------------------------------------------------------------------

ch2_ev_var_server <- function(input, output, session) {

  # --- PROTOTYP SCENY (2026-10-08): Zdrap los ---
  tk <- scene_tickets$main
  scene_texts(input, output, "ch2_zdrapka", list(
    tagList("W kiosku los kosztuje ", lc_fmt(scene_ticket_price), " zł. Na zdrapce można wygrać 4, 10 albo 100 zł,
      ale najczęściej pod srebrną farbą nie ma nic. ", tags$code("X", .noWS = "outside"), " to bilans losu: wygrana
      minus cena. Kup i zdrap kilka losów."),
    tagList("Lewy wykres zlicza bilanse, prawy pokazuje średni bilans wszystkich kupionych losów. Dołóż 100
      i 1000 losów."),
    tagList("Średni bilans na dłuższą metę to E(X) = ", sprintf("%.2f", scene_ticket_ev(tk)), " zł,
      a Var(X) mówi, jak bardzo pojedyncze losy od niego odbiegają.")
  ))

}
