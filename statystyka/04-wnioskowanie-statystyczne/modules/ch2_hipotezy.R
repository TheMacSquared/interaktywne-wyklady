# ============================================================================
# CHAPTER 2: Formułowanie hipotez statystycznych
# ============================================================================

ch2h_ui <- list(
  id = "ch-hipotezy", num = "02", title = "Od pytania do hipotezy",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 02 · Testowanie hipotez",
      num    = "02",
      title  = "Od pytania do hipotezy.",
      lead   = "Test statystyczny nie odpowiada na pytanie zadane potocznie. Najpierw
                trzeba je zamienić na dwie przeciwstawne hipotezy o parametrze
                populacji, a od tej zamiany zależy, co test w ogóle może wykazać."
    ),

    lc_p("Eksperyment z telefonem z rozdziału 01 zostawił nas z różnicą średnich
      i z wątpliwością, czy nie jest ona dziełem przypadku. Zanim policzymy
      cokolwiek, musimy ustalić, o co dokładnie pytamy: jakiej liczby dotyczy
      pytanie i jaki wynik uznamy za sygnał efektu. Ten rozdział jest
      w całości poświęcony tej zamianie."),

    lc_h2("ch2h-zasada", "Zasada: od potocznego do formalnego"),

    lc_p("Pytania badawcze formułujemy zwykle swobodnym językiem:"),
    tags$ul(
      tags$li(em("„Czy mężczyźni są wyżsi od kobiet?”")),
      tags$li(em("„Czy korepetycje pomagają?”")),
      tags$li(em("„Czy lodów sprzedaje się więcej w ciepłe dni?”"))
    ),
    lc_p("Żadnego z nich nie da się sprawdzić danymi wprost. „Wyżsi” znaczy
      tu „przeciętnie wyżsi”, bo zawsze znajdzie się wysoka kobieta i niski
      mężczyzna. „Pomagają” trzeba wyrazić czymś, co da się zmierzyć.
      Test statystyczny wymaga hipotez, czyli precyzyjnych stwierdzeń
      o populacji, które dane mogą podważyć. Żeby je sformułować, odpowiadamy
      na dwa pytania:"),
    tags$ol(
      tags$li(tags$b("Co mierzymy lub porównujemy?"),
              " Średnią, proporcję, korelację, a może różnicę między grupami?"),
      tags$li(tags$b("Jaki jest kierunek pytania?"),
              " Czy pytamy o różnicę w ogóle, o wartość większą, o wartość
              mniejszą, czy o zgodność z wartością odniesienia?")
    ),
    lc_p("Pierwsza odpowiedź wskazuje ", gloss("parametr"), ", który pojawi się
      w hipotezach. Tak jak w wykładzie 03, chodzi o nieznaną liczbę opisującą
      populację, a nie o wynik z naszej próby. Druga odpowiedź wskazuje znak,
      który połączy parametr z wartością odniesienia albo z drugim parametrem."),
    lc_p("Hipotezy zawsze występują w parze. ",
      gloss("hipoteza zerowa", "Hipoteza zerowa"), " H₀ opisuje stan domyślny:
      brak efektu, brak różnicy, zgodność z normą. Zawiera znak równości (=, ≤
      albo ≥), bo dopiero konkretna wartość parametru pozwala policzyć, jakich
      wyników należałoby się spodziewać, gdyby H₀ była prawdziwa. ",
      gloss("hipoteza alternatywna", "Hipoteza alternatywna"), " Hₐ jest jej
      dopełnieniem (≠, > albo <) i wyraża to, co chcemy wykazać."),

    lc_h2("ch2h-rozbior", "Rozbiór przykładu: telefon a koncentracja"),

    lc_p("Przełóżmy w ten sposób pytanie z rozdziału 01:"),
    lc_note("Pytanie",
      tags$em("„Czy telefon na biurku wpływa na koncentrację?”")
    ),
    lc_p(strong("Krok 1 — parametr:"), " mamy dwie grupy (telefon w plecaku
      i telefon na biurku) i w każdej mierzymy wynik testu koncentracji.
      Interesuje nas średnia koncentracja w populacji, osobno dla warunku
      „plecak” i dla warunku „biurko”. Średnie z naszych 40-osobowych grup są
      tylko ich oszacowaniami."),
    lc_p(strong("Krok 2 — relacja:"), " pytanie jest neutralne. Pyta, ",
      tags$em("czy w ogóle"), " istnieje wpływ, i nie przesądza, czy telefon
      koncentrację obniża, czy podnosi. Hₐ mówi więc po prostu, że średnie się
      różnią, a jej znakiem jest „≠”."),
    lc_p(strong("Krok 3 — sformułowanie:")),
    lc_formula_box(
      p(tags$b("H₀ (stan domyślny):"),
        " średnia koncentracja w grupie z telefonem na biurku jest równa
        średniej koncentracji w grupie z telefonem w plecaku."),
      p(tags$b("Hₐ (to, co chcemy wykazać):"),
        " średnia koncentracja w grupie z telefonem na biurku różni się od
        średniej koncentracji w grupie z telefonem w plecaku.")
    ),
    lc_p("H₀ i Hₐ są przeciwstawne i razem wyczerpują wszystkie możliwości:
      średnie albo są równe, albo się różnią. Trzeciej możliwości nie ma, więc
      odrzucenie jednej hipotezy przemawia za drugą. Tej zasady pilnujemy przy
      każdej parze hipotez."),
    lc_p("Ponieważ Hₐ dopuszcza różnicę w obie strony, mówimy o ",
      gloss("test dwustronny", "teście dwustronnym"), ". Kiedy warto zamiast
      niego wskazać konkretny kierunek, omówimy w sekcji o teście jednostronnym
      i dwustronnym."),

    lc_h2("ch2h-formalizm", "Od hipotezy słownej do zapisu formalnego"),

    lc_p("Słowna wersja hipotez porządkuje sens badania i nie jest etapem
      „mniej statystycznym”. Dopiero kiedy wiemy, jaki parametr badamy i jaka
      relacja nas interesuje, możemy przejść do zapisu symbolicznego.
      Zaczynamy od nazwania parametrów. W przykładzie z telefonem:"),
    lc_formula_box(
      p(withMathJax("\\(\\mu_{plecak}\\)"),
        " — średnia koncentracja w populacji studentów, gdy telefon jest w plecaku"),
      p(withMathJax("\\(\\mu_{biurko}\\)"),
        " — średnia koncentracja w populacji studentów, gdy telefon leży na biurku")
    ),
    lc_p("Pytanie dotyczy wpływu w dowolną stronę, więc hipoteza alternatywna
      jest dwustronna:"),
    lc_formula_box(
      p(withMathJax("\\(H_0: \\mu_{biurko} = \\mu_{plecak}\\)")),
      p(withMathJax("\\(H_a: \\mu_{biurko} \\neq \\mu_{plecak}\\)"))
    ),
    lc_p("Tę samą parę można zapisać przez różnicę średnich:
      H₀ mówi, że różnica ", withMathJax("\\(\\mu_{biurko} - \\mu_{plecak}\\)"),
      " wynosi 0, a Hₐ, że jest od 0 różna. W wykładzie 03 liczyliśmy przedział
      ufności dla różnicy średnich. Pytanie, czy taki przedział obejmuje 0,
      i test tej pary hipotez to dwa spojrzenia na ten sam problem. Do tego
      związku wrócimy przy konkretnych testach."),
    lc_p("Pełny zapis formalny składa się z trzech elementów: definicji
      symboli, hipotezy zerowej i hipotezy alternatywnej. Bez definicji symboli
      wzór jest nieczytelny: ", withMathJax("\\(\\mu_1 < \\mu_2\\)"),
      " nic nie mówi, jeśli nie wiadomo, czym są grupa 1 i grupa 2. Dlatego
      warto zawsze iść tą samą ścieżką: ",
      tags$em(gloss("pytanie badawcze"), " → hipotezy słowne → definicja
      parametrów → zapis formalny"),
      ". Chroni to przed mechanicznym wpisywaniem znaków bez zrozumienia, co
      właściwie porównujemy. Każda para hipotez ma ten sam szkielet:"),
    lc_formula_box(
      p(tags$b("H₀:"), " parametr  =  /  ≤  /  ≥  wartość odniesienia"),
      p(tags$b("Hₐ:"), " parametr  ≠  /  >  /  <  wartość odniesienia")
    ),
    lc_p("Znaki łączą się w pary: równości w H₀ odpowiada „≠” w Hₐ, znakowi
      „≤” odpowiada „>”, a znakowi „≥” odpowiada „<”. W każdej parze dwa znaki
      razem pokrywają wszystkie możliwe wartości parametru."),

    # ========================================================================
    # WIDGET 1: Galeria przykładów (język naturalny) — część dwustronna
    # ========================================================================
    lc_h2("ch2h-galeria", "Galeria: sformułuj hipotezy sam"),

    lc_p("Czas przećwiczyć tę zamianę na pytaniach z różnych dziedzin.
      Dla każdego pytania ustal, jakiego parametru dotyczy i jakiej relacji
      szuka Hₐ. Zapisz obie hipotezy na boku w języku naturalnym, bez symboli,
      i dopiero wtedy porównaj je z odpowiedzią pod przyciskiem."),
    lc_p("W tej galerii ćwiczymy hipotezy nieskierowane: Hₐ nie wskazuje,
      w którą stronę miałby iść efekt. Pytania, które z góry wskazują kierunek,
      pojawią się po sekcji o teście jednostronnym i dwustronnym."),

    hypothesis_practice("ch2h_gal", list(
      list(
        question = "60 gospodarstw domowych, pomiary zużycia wody.
                    Norma projektowa: 150 l na osobę na dobę.
                    Czy średnie zużycie w naszej gminie spełnia normę?",
        h0 = "Średnie zużycie wody w gminie jest równe 150 l na osobę na dobę.",
        ha = "Średnie zużycie wody w gminie różni się od 150 l na osobę na dobę.",
        note = "Dwustronny — „spełnia normę” oznacza „nie odbiega” w żadną stronę."
      ),
      list(
        question = "Plan zagospodarowania: 300 działek podzielono według strefy
                    (centrum / przedmieścia / obrzeża) i typu (mieszkaniowa /
                    usługowa / przemysłowa / zielona). Czy typ zagospodarowania
                    zależy od strefy miasta?",
        h0 = "Typ zagospodarowania działki i strefa miasta są niezależne.",
        ha = "Typ zagospodarowania działki i strefa miasta są ze sobą powiązane.",
        note = "Dwie zmienne jakościowe — pytamy o niezależność albo powiązanie. Nie ma tu kierunku, więc nie mówimy o jedno- ani dwustronności."
      ),
      list(
        question = "Eksperyment: 3 typy opakowań jogurtu (szkło / plastik / karton),
                    po 20 próbek każdego. Czy rodzaj opakowania wpływa na trwałość?",
        h0 = "Średnia trwałość jogurtu jest taka sama dla wszystkich trzech typów opakowań.",
        ha = "Co najmniej jeden typ opakowania ma inną średnią trwałość niż pozostałe.",
        note = "Trzy grupy — porównanie wielu średnich. Hₐ mówi tylko, że co najmniej jedna średnia jest inna, a nie która; podział na jedno- i dwustronne nie ma tu klasycznego sensu."
      )
    )),

    lc_p("Te trzy pytania brzmią podobnie, a prowadzą do różnych par hipotez.
      Parametrem nie zawsze jest jedna średnia, a Hₐ nie zawsze da się zapisać
      pojedynczym znakiem „≠”. Rodzaj parametru zdecyduje później o wyborze
      testu. Rozdziały 04–09 omawiają po kolei testy dla tych sytuacji."),

    # ========================================================================
    # WIDGET 4: Jednostronny vs dwustronny
    # ========================================================================
    lc_h2("ch2h-jedno-dwustronny", "Test jednostronny a dwustronny"),

    lc_p("We wszystkich dotychczasowych przykładach Hₐ dopuszczała efekt
      w dowolną stronę. Bywa jednak, że samo pytanie wskazuje kierunek:
      interesuje nas, czy nawóz zwiększa plony, a nie czy je zmienia. Wtedy
      Hₐ zawiera znak „>” albo „<”, a test nazywamy ",
      gloss("test jednostronny", "jednostronnym"), ". Sformułowanie Hₐ
      decyduje o typie testu:"),
    lc_table(
      data.frame(
        c1 = c("Dwustronny", "Prawostronny", "Lewostronny"),
        c2 = I(list(
          withMathJax("\\(\\mu_1 \\neq \\mu_2\\)"),
          withMathJax("\\(\\mu_1 > \\mu_2\\)"),
          withMathJax("\\(\\mu_1 < \\mu_2\\)")
        )),
        c3 = c(
          "„Czy grupy się różnią?”",
          "„Czy lek działa lepiej niż placebo?”",
          "„Czy nowa metoda skraca czas pracy?”"
        ),
        c4 = c(
          "Gdy pytanie nie przesądza kierunku — wybór domyślny",
          "Gdy pytanie dotyczy tylko wzrostu, a kierunek ustalono przed zebraniem danych",
          "Gdy pytanie dotyczy tylko spadku, a kierunek ustalono przed zebraniem danych"
        )
      ),
      cols = list(
        lc_col("c1", "Typ", "row"),
        lc_col("c2", "Hₐ", "text"),
        lc_col("c3", "Przykład", "text"),
        lc_col("c4", "Kiedy?", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_p("Żeby zobaczyć, co ten wybór zmienia, trzeba zajrzeć na chwilę do
      mechanizmu decyzji, który szczegółowo omówimy w rozdziale 03. Z danych
      liczymy ", gloss("statystyka testowa", "statystykę testową"), ", czyli
      liczbę mierzącą, jak daleko wynik z próby odbiega od tego, czego
      oczekiwalibyśmy przy prawdziwej H₀. Jeśli H₀ jest prawdziwa, statystyka
      ma znany rozkład, a wartości z jego skrajów zdarzają się rzadko. Te
      skrajne wartości tworzą ", gloss("obszar odrzucenia"), ": gdy statystyka
      do niego wpada, odrzucamy H₀."),
    lc_p("Prawdopodobieństwo obszaru odrzucenia przy prawdziwej H₀ to ",
      gloss("poziom istotności"), " α. Jest to ryzyko, że odrzucimy H₀, choć
      jest prawdziwa. Zwyczajowo przyjmuje się α = 0.05. To umowa, którą
      ustala się przed analizą, tak jak poziom ufności w wykładzie 03. Oba
      pojęcia są zresztą ze sobą powiązane: poziom ufności 95% odpowiada
      α = 0.05. W teście dwustronnym α dzielimy na dwa ogony rozkładu, po α/2
      na każdy. W teście jednostronnym całe α leży w ogonie wskazanym
      przez Hₐ."),
    lc_p("Panel pokazuje rozkład statystyki testowej przy prawdziwej H₀
      (standardowy rozkład normalny). Zacieniowany jest obszar odrzucenia,
      a przerywane linie to ", gloss("wartość krytyczna", "wartości krytyczne"),
      ", czyli jego granice."),

    figure_panel(
      label = "Ryc. 2.4",
      title = "Wizualizacja: jedno- i dwustronny",
      lc_toolbar(
        lc_segmented("ch2h_sided", "Typ testu", choices = c(
              "Dwustronny (≠)" = "two.sided",
              "Prawostronny (>)" = "greater",
              "Lewostronny (<)" = "less"
            ), selected = "two.sided"),
        lc_slider("ch2h_alpha", "α", 0.01, 0.10, 0.05, 0.01)
      ),
      div(class = "ws-chart-wrap",
            tags$canvas(id = "ch2h_sided_chart")
          )
    ),

    lc_p("Przy α = 0.05 test dwustronny odrzuca H₀, gdy statystyka jest
      większa niż 1.96 lub mniejsza niż -1.96; w każdym ogonie leży 2.5%
      rozkładu. Test prawostronny odrzuca H₀ już powyżej 1.645, bo całe 5%
      mieści się w jednym ogonie. Lewostronny działa symetrycznie: odrzuca
      poniżej -1.645. Zmniejszenie α odsuwa granice od środka: przy α = 0.01
      wynoszą one 2.576 dla testu dwustronnego i 2.326 dla jednostronnego.
      Zwiększenie α do 0.10 przysuwa je do 1.645 i 1.282."),
    lc_p("Weźmy statystykę równą 1.8. W teście prawostronnym wpada ona
      w obszar odrzucenia, w dwustronnym nie. Test jednostronny łatwiej więc
      wykrywa efekt w zapowiedzianym kierunku, czyli ma w tym kierunku większą ",
      gloss("moc testu", "moc"), ". Płaci za to ślepotą na kierunek przeciwny:
      statystyka równa -3 w teście prawostronnym nie prowadzi do odrzucenia H₀,
      choć jest daleko od zera. Stąd wymóg, by kierunek ustalić przed
      zebraniem danych. Kto wybiera ogon po zobaczeniu znaku wyniku,
      w praktyce odrzuca H₀ zawsze, gdy statystyka wychodzi poza ±1.645.
      Przy prawdziwej H₀ zdarza się to w 10% prób, a nie w deklarowanych 5%."),

    lc_note("Zasada", rule = TRUE,
      "W razie wątpliwości wybieraj test dwustronny. Test jednostronny ma sens
       tylko wtedy, gdy kierunek wynika z pytania i został ustalony przed
       zebraniem danych."
    ),

    # ========================================================================
    # WIDGET 2: Galeria przykładów — jednostronne
    # ========================================================================
    lc_h2("ch2h-galeria-jedno", "Galeria: hipotezy jednostronne"),

    lc_p("Wracamy do ćwiczeń, tym razem z pytaniami, które z góry wskazują
      kierunek. Przy hipotezach jednostronnych łatwo zapomnieć, że H₀ nadal
      musi być dopełnieniem Hₐ. Jeśli Hₐ mówi „wyższy”, to H₀ obejmuje
      wszystko, co wyższe nie jest, czyli „nie wyższy niż” (≤). Razem znów
      wyczerpują wszystkie możliwości. Jak poprzednio, zapisz obie hipotezy
      przed odsłonięciem odpowiedzi."),

    hypothesis_practice("ch2h_gal_one", list(
      list(
        question = "Doświadczenie polowe: 30 poletek z nowym nawozem,
                    30 kontrolnych. Czy nowy nawóz daje wyższe plony?",
        h0 = "Średni plon na poletkach z nowym nawozem jest nie wyższy niż średni plon na poletkach kontrolnych.",
        ha = "Średni plon na poletkach z nowym nawozem jest wyższy niż średni plon na poletkach kontrolnych.",
        note = "Jednostronny — pytamy tylko o „wyższe”, nie o ogólną różnicę. H₀ to dopełnienie Hₐ: „nie wyższy niż” = równy lub niższy."
      ),
      list(
        question = "20 zakładów, w których mierzono liczbę wypadków przed
                    i po szkoleniu BHP. Czy szkolenie zmniejszyło liczbę wypadków?",
        h0 = "Średnia liczba wypadków po szkoleniu jest nie niższa niż przed szkoleniem.",
        ha = "Średnia liczba wypadków po szkoleniu jest niższa niż przed szkoleniem.",
        note = "Jednostronny („zmniejszyło”). Te same zakłady mierzono dwa razy, więc w praktyce użyjemy testu t dla danych sparowanych."
      ),
      list(
        question = "Laboratorium przebadało 120 próbek wody pitnej.
                    Czy ponad 80% próbek spełnia normy jakości?",
        h0 = "Odsetek próbek spełniających normy w populacji jest nie wyższy niż 80%.",
        ha = "Odsetek próbek spełniających normy w populacji jest wyższy niż 80%.",
        note = "Jednostronny („ponad”). Parametr to proporcja, nie średnia."
      ),
      list(
        question = "Ankieta wśród 150 studentów: godziny snu przed egzaminem
                    i ocena z egzaminu. Czy dłuższy sen wiąże się z lepszą oceną?",
        h0 = "Nie ma dodatniego związku między godzinami snu a oceną z egzaminu (związek zerowy lub ujemny).",
        ha = "Im więcej snu, tym wyższa ocena z egzaminu (dodatni związek).",
        note = "Jednostronny — „dłuższy → lepsza” wskazuje kierunek dodatniego związku."
      )
    )),

    lc_p("Kierunek zdradzają w pytaniach pojedyncze słowa: „wyższe”,
      „zmniejszyło”, „ponad”. Gdyby to samo pytanie zadać neutralnie,
      na przykład „czy szkolenie zmieniło liczbę wypadków?”, para hipotez
      stałaby się dwustronna. Dlatego przy czytaniu pytania badawczego warto
      zatrzymać się na każdym słowie, które coś porównuje."),

    # ========================================================================
    # Typowe błędy
    # ========================================================================
    lc_h2("ch2h-bledy", "Typowe błędy przy formułowaniu hipotez"),

    lc_p("Ćwiczenia pokazują, że zamiana pytania na hipotezy wymaga kilku
      decyzji, a każda z nich może pójść źle. Najczęstsze pomyłki są
      następujące:"),
    tags$ol(
      tags$li(
        tags$b("H₀ bez równości."),
        " Źle: ", withMathJax("\\(H_0: \\mu_1 \\neq \\mu_2\\)"),
        ". H₀ zawiera znak równości (=, ewentualnie ≤ lub ≥), bo tylko
        wtedy wiadomo, jakich wyników oczekiwać, gdy jest prawdziwa."
      ),
      tags$li(
        tags$b("Hipoteza o próbie zamiast populacji."),
        " Źle: „H₀: średnia w próbie = 170”. Średnią z próby znamy
        dokładnie, więc nie ma czego testować. Hipotezy dotyczą parametrów ",
        em_("populacji"), ", tak jak przedziały ufności w wykładzie 03."
      ),
      tags$li(
        tags$b("Brak precyzji."),
        " Źle: „H₀: dane są dobre”. Hipoteza musi wskazywać parametr
        i wartość odniesienia, inaczej nie da się jej sprawdzić danymi."
      ),
      tags$li(
        tags$b("Zmiana hipotezy po zobaczeniu danych (HARKing)."),
        " Hipotezy, także wybór testu jedno- lub dwustronnego, ustalamy ",
        tags$em("przed"), " analizą. Dopasowanie H₀ i Hₐ do wyniku sprawia,
        że test potwierdza tylko to, co już widać w danych, a rzeczywiste
        ryzyko fałszywego alarmu przestaje odpowiadać α."
      ),
      tags$li(
        tags$b("Zamienione role H₀ i Hₐ."),
        " To, co chcemy wykazać, trafia do Hₐ, a H₀ opisuje brak efektu.
        Test może H₀ odrzucić albo nie znaleźć podstaw do jej odrzucenia,
        ale nigdy nie dowodzi, że H₀ jest prawdziwa. Gdyby efekt umieścić
        w H₀, brak podstaw do jej odrzucenia wyglądałby jak potwierdzenie
        efektu, choć oznaczałby tylko, że dane są niejednoznaczne."
      )
    ),

    lc_p("Mamy więc parę hipotez i wiemy, na czym polega obszar odrzucenia.
      Każda decyzja podjęta na podstawie próby może jednak okazać się błędna,
      i to na dwa sposoby: możemy odrzucić prawdziwą H₀ albo nie odrzucić
      fałszywej. W następnym rozdziale nazwiemy oba błędy, przyjrzymy się
      bliżej α oraz poznamy p-wartość, czyli liczbę, na podstawie której
      w praktyce podejmuje się decyzję."),

    lc_chapter_next(
      num       = "03",
      title     = "Błędy, p-wartość i decyzja",
      lead      = "jak przejść od H₀ i Hₐ do formalnego werdyktu.",
      target_id = "ch-decyzja"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch2h_server <- function(input, output, session) {

  # --- Widget: Jednostronny vs dwustronny ---
  observe({
    req(input$ch2h_alpha, input$ch2h_sided)
    alpha <- input$ch2h_alpha
    sided <- input$ch2h_sided

    crit <- if (sided == "two.sided") {
      qnorm(1 - alpha / 2)
    } else if (sided == "greater") {
      qnorm(1 - alpha)
    } else {
      qnorm(alpha)
    }

    session$sendCustomMessage("ws_sided_chart", list(
      id = "ch2h_sided_chart",
      sided = sided,
      alpha = alpha,
      crit = crit
    ))
  })
}
