# ============================================================================
# CHAPTER 4: Porównanie modeli
# ============================================================================

ch4_ui <- list(
  id    = "ch-porownanie",
  num   = "04",
  title = "Porównanie modeli",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 04 · Regresja",
      num    = "04",
      title  = "Lepsze dopasowanie to jeszcze nie lepszy model.",
      lead   = "Każda dodatkowa zmienna poprawia wynik na danych, które model już
                widział. Porównanie modeli polega na tym, żeby odróżnić poprawę
                prawdziwą od dopasowania do przypadku."
    ),

    lc_p("W rozdziale 02 ocenialiśmy jeden model: oglądaliśmy reszty, liczyliśmy
      R² i ", gloss("RMSE"), ". W rozdziałach 03 i 03B modele zaczęły rosnąć: dochodziły kolejne
      ", gloss("predyktor", "predyktory"), ", zmienne jakościowe i interakcje. Każde takie rozszerzenie to
      nowy kandydat, więc zwykle mamy do wyboru kilka modeli dla tej samej
      zmiennej zależnej. Ten rozdział pokazuje, czym je porównywać i dlaczego
      najprostsza odpowiedź, czyli wybór modelu o najwyższym R², prowadzi na
      manowce."),

    lc_h2("ch4-problem", "Dlaczego sam R² nie wystarczy"),

    lc_p("R² ma przy porównaniach zdradliwą własność: po dodaniu predyktora nigdy
      nie spada. ", gloss("metoda najmniejszych kwadratów", "Metoda najmniejszych kwadratów"), " może nowemu predyktorowi nadać
      współczynnik zero i wtedy suma kwadratów reszt pozostaje taka jak
      wcześniej. Każdy inny współczynnik wybierze tylko wtedy, gdy zmniejsza
      tę sumę. \\(SS_{res}\\) nie rośnie, \\(SS_{tot}\\) się nie zmienia, więc
      R² może tylko wzrosnąć albo zostać w miejscu, nawet gdy nowa zmienna
      to czysty szum."),

    lc_p("Szum zawsze trochę koreluje z resztami w konkretnej próbie, a model
      tę przypadkową zgodność wykorzystuje. To ten sam mechanizm, który
      w wykładzie 05 opisywaliśmy przy ",
      gloss("porównania wielokrotne", "porównaniach wielokrotnych"), ": im więcej
      kandydatów sprawdzamy, tym większa szansa, że któryś pasuje do danych
      wyłącznie przez przypadek. Model zbudowany przez dokładanie zmiennych
      dopóty, dopóki rośnie R², dopasowuje się coraz lepiej do tej jednej
      próby, w tym do jej przypadkowych wahań. Takie zjawisko nazywamy ",
      gloss("przeuczenie", "przeuczeniem"), ". Do porównywania modeli
      o różnej liczbie parametrów potrzebujemy więc miar, które za każdy
      dodatkowy parametr pobierają opłatę."),

    lc_h2("ch4-efekt-dodawania", "Efekt dodawania zmiennych"),

    lc_p("Panel losuje dane 150 studentów: ocenę oraz cztery predyktory — godziny
      nauki, frekwencję, poziom stresu i liczbę godzin snu. Ocena powstaje
      z kombinacji wszystkich czterech zmiennych i losowego szumu. Panel
      dopasowuje pięć modeli, dokładając predyktory po kolei. W ostatnim kroku
      dokłada zmienną „szum”: liczbę losowaną niezależnie od oceny, która
      z oceną nie ma żadnego związku. Panel pokazuje R²
      oraz ", gloss("skorygowany R²", "R² skorygowany"), ", a w tabeli także ",
      gloss("AIC"), ", ", gloss("BIC"), " i RMSE. Te miary omawiamy
      w następnej sekcji."),

    figure_panel(
      label = "Ryc. 4.1", title = "Krok po kroku",
      full_width = TRUE,
      # Losowanie, modele i tabelę liczy przeglądarka (modules/figures.js, rycina s07_build).
      lc_sf_figure("s07_build", legend = TRUE,
        toolbar = lc_sf_action("build", "ch4_stepwise", "Buduj modele krok po kroku", variant = "solid"),
        ratio = "2.2/1", max_height = "300px",
        after = tags$div(`data-role` = "table")),
      lc_caption("Modele z 1–4 predyktorami i model z dodanym szumem.")
    ),

    lc_p("W pierwszych czterech krokach R² rośnie, przeciętnie od około 0.41 dla
      samych godzin nauki do około 0.71 dla wszystkich czterech predyktorów.
      R² skorygowany idzie tuż pod nim, AIC i BIC maleją, a RMSE się zmniejsza.
      Wszystkie miary są tu zgodne, bo dane wygenerowano tak, że każda z czterech
      zmiennych naprawdę wpływa na ocenę i każdy dodatek wnosi prawdziwą
      informację."),

    lc_p("Piąty krok jest inny. Szum nie ma z oceną nic wspólnego, a R² mimo to
      rośnie, zwykle tylko w trzecim miejscu po kropce (mediana przyrostu około
      0.001). R² nie umie więc odróżnić prawdziwego predyktora od przypadkowego.
      Miary z karą za złożoność reagują inaczej: w symulacji 4000 losowań
      R² skorygowany wzrósł po dodaniu szumu w około 31% przypadków, AIC wskazał
      większy model w około 16%, a BIC tylko w około 3%. Dlatego przy porównaniach
      patrzymy na nie, a nie na R²."),

    lc_h2("ch4-metryki", "Metryki porównawcze"),

    lc_p("Najprostsza poprawka dotyczy samego R². R² skorygowany liczy
      dopasowanie z uwzględnieniem liczby predyktorów k:"),

    # Skorygowany R² ze znacznikami symboli
    lc_formula_annotated(
      "⟦1|R^2_{adj}⟧ = 1 - \\frac{(1 - R^2)(⟦2|n⟧ - 1)}{n - ⟦3|k⟧ - 1}",
      notes  = list("skorygowany R²", "liczba obserwacji", "liczba predyktorów"),
      colors = c("accent", "szalwia", "szalwia"),
      above  = c(2)
    ),

    lc_p("Czynnik \\((n-1)/(n-k-1)\\) rośnie z każdym predyktorem, więc nowa
      zmienna musi zmniejszyć \\(1 - R^2\\) na tyle, żeby to zrównoważyć.
      Jeśli wnosi mniej, R² skorygowany spada. Może nawet wyjść ujemny, gdy
      model nie wyjaśnia prawie nic."),

    lc_p("Drugą rodzinę tworzą kryteria informacyjne. Oba zaczynają od tego,
      jak dobrze model odtwarza dane: \\(\\hat{L}\\) oznacza wiarygodność,
      czyli prawdopodobieństwo (dokładniej: gęstość) zaobserwowanych danych
      przy najlepiej dopasowanych parametrach. Do tego dochodzi kara za liczbę
      parametrów. W ", gloss("regresja liniowa", "regresji liniowej"), " z k predyktorami szacujemy k + 1
      współczynników i ", gloss("wariancja", "wariancję"), " reszt, razem k + 2 parametry:"),

    # AIC i BIC jako dopasowanie + kara
    lc_formula_annotated(
      "\\text{AIC} = ⟦1|-2\\ln\\hat{L}⟧ + ⟦2|2(k + 2)⟧, \\qquad \\text{BIC} = -2\\ln\\hat{L} + ⟦3|(k + 2)\\ln n⟧",
      notes  = list("niedopasowanie", "kara AIC", "kara BIC"),
      colors = c("niebo", "bursztyn", "terakota")
    ),

    lc_formula_box(withMathJax(
      "$$-2\\ln\\hat{L} = n\\ln\\left(\\text{RMSE}^2\\right) + n\\left(1 + \\ln 2\\pi\\right)$$"
    )),

    lc_p("Drugi wiersz pokazuje, co kryje się pod wiarygodnością w regresji
      liniowej: to RMSE w skali logarytmicznej, pomnożone przez liczbę
      obserwacji. AIC i BIC są więc sumą tego samego składnika dopasowania
      i kary za złożoność. Mniejsza wartość oznacza lepszy kompromis.
      Kryteria różnią się tylko karą: AIC dolicza 2 za każdy parametr,
      BIC — \\(\\ln n\\). Już przy ośmiu obserwacjach \\(\\ln n\\) przekracza 2,
      a przy n = 150 wynosi około 5, więc BIC mocniej karze złożoność
      i częściej wybiera prostszy model."),

    lc_p("Pojedyncza wartość AIC czy BIC nic nie mówi. Liczba 250 nie jest ani
      dobra, ani zła, bo zależy od skali Y i liczby obserwacji. Sens mają tylko
      różnice między modelami dopasowanymi do tych samych obserwacji tej samej
      zmiennej zależnej. Nie ma też uniwersalnej granicy, od której różnica
      rozstrzyga sprawę. Mała różnica oznacza, że dane słabo odróżniają modele,
      i wtedy rozsądnie jest wybrać prostszy albo ten lepiej uzasadniony
      merytorycznie."),

    lc_p("Kary mają też interpretację w języku testów. Gdy do modelu dokładamy
      jedną zmienną bez żadnego związku z Y, przy n = 150 R² skorygowany
      wzrośnie w około 32% przypadków, AIC uzna większy model za lepszy
      w około 16%, a BIC w około 2.5%. To odpowiednik ", gloss("poziom istotności", "poziomu istotności"), ":
      każda miara przepuszcza część fałszywych alarmów. Jeśli sprawdzimy
      dziesięć takich bezużytecznych kandydatów, każdego osobno, szansa,
      że przynajmniej jeden przejdzie przez AIC, wynosi około 80%. Problem
      porównań wielokrotnych wraca więc przy wyborze zmiennych, a kary go
      łagodzą, ale nie usuwają."),

    lc_h2("ch4-arena", "Arena modeli liniowych"),

    lc_p("Panel losuje dane z tego samego mechanizmu co poprzedni, ale pozwala
      zmienić liczbę obserwacji. Dla każdego z czterech modeli pokazuje
      R² skorygowany, AIC, BIC i RMSE, a w tabeli wyróżnia najlepszą wartość
      R² skorygowanego, AIC i BIC."),

    figure_panel(
      label = "Ryc. 4.2", title = "Porównanie modeli regresji",
      full_width = TRUE,
      # Losowanie, modele i tabelę liczy przeglądarka (modules/figures.js, rycina s07_arena).
      lc_sf_figure("s07_arena", legend = TRUE,
        control = lc_sf_slider("ch4_n", "n", 50, 300, 150, 25),
        toolbar = lc_sf_action("compare", "ch4_compare", "Buduj i porównaj modele", variant = "solid"),
        body = tagList(
          tags$style(HTML(".s07-quad{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:.4em 1em}
            @container lc-fig (min-width:620px){.s07-quad{grid-template-columns:repeat(4,minmax(0,1fr))}}
            .s07-quad h4{margin:.2em 0;font-size:.9em}.s07-quad h4[hidden]{display:none}")),
          tags$div(class = "s07-quad", lapply(seq_along(c("R² skorygowany", "AIC", "BIC", "RMSE")), function(k) {
            tags$div(tags$h4(`data-arena-head` = NA, hidden = NA, c("R² skorygowany", "AIC", "BIC", "RMSE")[k]),
                     lc_sf_chart(k - 1, "0.9/1", "260px"))
          }))),
        after = tags$div(`data-role` = "table")),
      lc_caption("Generujemy dane i budujemy 4 modele z różną
                    liczbą predyktorów.")
    ),

    lc_p("Przy n = 150 wszystkie kryteria praktycznie zawsze wskazują pełny model.
      Przy n = 50 zaczynają się rozchodzić. AIC nadal wybiera model z czterema
      predyktorami w około 86% losowań, BIC tylko w około 69%, a w co czwartym
      losowaniu woli model bez snu. Słaby efekt snu jest prawdziwy, ale
      w małej próbie trudno go odróżnić od szumu, a kara BIC jest na tyle
      surowa, że często go odrzuca."),

    lc_p("W kolumnie RMSE najmniejsza wartość zawsze przypada modelowi
      czwartemu, dlatego panel jej nie wyróżnia. RMSE liczone na tych samych
      danych, na których model dopasowano, zachowuje się jak R²: po dodaniu
      predyktora nie rośnie, więc nie nadaje się do porównywania modeli
      o różnej złożoności. Uczciwe RMSE trzeba policzyć na nowych danych.
      Tym zajmiemy się za chwilę."),

    lc_p("Gdy AIC i BIC się nie zgadzają, nie oznacza to, że jedno z nich się
      myli. AIC szuka modelu, który najlepiej przewiduje nowe obserwacje,
      i godzi się na dodatkowy parametr, jeśli ten choć trochę poprawia
      predykcję. BIC szuka modelu prostego, który zawiera tylko wyraźne efekty.
      Do przewidywania częściej sięga się po AIC, do wyjaśniania, które
      zmienne naprawdę mają znaczenie, po BIC."),

    lc_h2("ch4-overfitting", "Przeuczenie: model uczy się szumu"),

    lc_p("Dotąd porównywaliśmy modele z jedną, dwiema, trzema i czterema
      zmiennymi. Ten sam problem pojawia się, gdy złożoność rośnie
      w inny sposób, na przykład gdy jedną zmienną X wprowadzamy do modelu
      jako wielomian coraz wyższego stopnia. Wielomian stopnia d ma d + 1
      współczynników i każdy kolejny stopień pozwala krzywej mocniej się zgiąć.
      Wielomiany kolejnych stopni to ",
      gloss("modele zagnieżdżone", "modele zagnieżdżone"), ": niższy powstaje
      z wyższego przez usunięcie najwyższej potęgi. AIC i BIC nie wymagają
      jednak zagnieżdżenia. Wystarczy, że modele opisują tę samą zmienną
      zależną na tych samych obserwacjach."),

    lc_p("W rozdziale 02 widzieliśmy trzy wielomiany: zbyt prosty, rozsądny
      i przeuczony. Panel poniżej pozwala przejść przez wszystkie stopnie
      od 1 do 15. Dane powstają jako fala sinusoidalna z szumem
      o ", gloss("odchylenie standardowe", "odchyleniu standardowym"), " 1, a pod wykresem widać R², R² skorygowany,
      AIC i BIC dla wybranego stopnia."),

    figure_panel(
      label = "Ryc. 4.3", title = "Wielomian: dopasowanie a uogólnianie",
      full_width = TRUE,
      # Losowanie i dopasowanie liczy przeglądarka (modules/figures.js, rycina s07_poly).
      lc_sf_figure("s07_poly",
        control = list(lc_sf_slider("ch4_poly_degree", "Stopień wielomianu", 1, 15, 1, 1),
                       lc_sf_slider("ch4_poly_n", "n (punktów)", 15, 100, 30, 5)),
        toolbar = tagList(lc_sf_action("gen", "ch4_poly_gen", "Generuj", variant = "solid"),
                          lc_readouts(tags$div(`data-role` = "stats", style = "display: contents"))),
        ratio = "2.2/1", max_height = "300px")
    ),

    lc_p("Przy 30 punktach prosta (stopień 1) nie łapie fali, a R² jest bliski
      zera. Od stopnia 4 krzywa zaczyna podążać za danymi i R² skacze
      do około 0.78. Dalej R² rośnie powoli aż do około 0.91 przy stopniu 15,
      ale krzywa coraz bardziej faluje między punktami, a przy brzegach
      odlatuje. R² skorygowany zatrzymuje się w okolicach 0.80 od stopnia 6.
      BIC jest najmniejszy zwykle przy stopniach 4–6 i potem rośnie."),

    lc_p("AIC przy tak małej próbie zawodzi częściej. Jego kara jest łagodna,
      więc w mniej więcej co trzecim losowaniu najmniejszą wartość osiąga
      dla stopnia 12–15, czyli dla krzywej wyraźnie przeuczonej. Kryteria
      informacyjne same są oszacowaniami z tej samej próby i w małych
      próbach mogą się mylić. Dlatego potrzebny jest sprawdzian, który
      nie korzysta z danych użytych do dopasowania."),

    lc_h2("ch4-train-test", "Zbiór uczący i testowy"),

    lc_p("Najbardziej bezpośredni sprawdzian polega na rozdzieleniu danych.
      Model dopasowujemy na ",
      gloss("zbiór treningowy", "zbiorze uczącym"), " (inaczej treningowym),
      a jego błąd mierzymy na ",
      gloss("zbiór testowy", "zbiorze testowym"), ", czyli na obserwacjach,
      których przy dopasowaniu nie widział. RMSE na zbiorze testowym liczymy
      tym samym wzorem co w rozdziale 02, tylko z resztami
      \\(y_j - \\hat{y}_j\\) dla obserwacji testowych. Ta miara odpowiada
      wprost na pytanie, które naprawdę nas interesuje: jak bardzo model
      pomyli się na nowych danych."),

    lc_p("W panelu ciemne punkty to 35 obserwacji uczących, a jasne to 180
      obserwacji testowych z tego samego mechanizmu (fala z szumem
      o odchyleniu standardowym 1). Pod wykresem widać RMSE na obu zbiorach
      dla wybranego stopnia i stopień, który na zbiorze testowym wypada
      najlepiej."),

    figure_panel(
      label = "Ryc. 4.4", title = "Zbiór uczący i testowy: kiedy model przestaje uogólniać",
      full_width = TRUE,
      # Losowanie i dopasowania liczy przeglądarka (modules/figures.js, rycina s07_tt).
      lc_sf_figure("s07_tt",
        control = lc_sf_slider("ch4_tt_degree", "Stopień wielomianu", 1, 15, 1, 1),
        toolbar = tagList(lc_sf_action("new", "ch4_tt_new", "Nowy podział danych", variant = "solid"),
                          lc_readouts(tags$div(`data-role` = "stats", style = "display: contents"))),
        ratio = "2.2/1", max_height = "330px",
        after = tags$div(`data-role` = "note"))
    ),

    lc_p("Błąd na zbiorze uczącym maleje z każdym stopniem: typowo od około 2.2
      przy prostej do około 0.7 przy stopniu 15. Błąd testowy najpierw też
      spada, osiąga minimum w okolicach stopnia 6 (typowo około 1.1), a potem
      rośnie. Przy stopniu 15 w mniej więcej trzech podziałach na cztery
      jest większy niż dla zwykłej prostej, często kilkukrotnie. Żaden model
      nie zejdzie na zbiorze testowym wyraźnie poniżej 1, bo tyle wynosi
      odchylenie standardowe szumu, którego nie da się przewidzieć. RMSE
      uczące wyraźnie poniżej tej wartości to znak, że model dopasował się
      także do szumu."),

    lc_p("Rozjazd obu krzywych to rozpoznawalny objaw przeuczenia: model
      coraz lepiej pamięta punkty, które widział, i coraz gorzej radzi sobie
      z nowymi. Przy kolejnych podziałach najlepszy stopień się zmienia,
      zwykle mieści się między 4 a 9. Sam wynik testu też jest więc
      oszacowaniem z próby, tym pewniejszym, im większy zbiór testowy.
      Zbioru testowego nie wolno też używać wielokrotnie do dobierania
      modelu. Gdy sprawdzimy na nim kilkadziesiąt wariantów i wybierzemy
      najlepszy, wracamy do problemu porównań wielokrotnych, a zbiór
      testowy przestaje być dla modelu nowy."),

    lc_note("Zasada", rule = TRUE,
      "Model ocenia się po tym, jak przewiduje dane, których nie widział, i po
       tym, czy ma sens merytoryczny. R² na danych użytych do dopasowania
       przy dodawaniu zmiennych tylko rośnie."
    ),

    lc_h2("ch4-co-dalej", "Co dalej"),

    lc_p("Mamy komplet narzędzi do porównywania modeli: R² skorygowany, AIC
      i BIC, które karzą za liczbę parametrów, oraz błąd na zbiorze testowym,
      który mierzy przewidywanie wprost. Wszystkie modele w tym rozdziale
      miały jednak ciągłą zmienną zależną."),

    lc_p("Często Y przyjmuje tylko dwie wartości: student zdał albo nie zdał,
      klient kupił albo nie kupił. Regresja liniowa daje wtedy przewidywania
      spoza przedziału [0, 1], których nie da się czytać jako
      prawdopodobieństw. Następny rozdział wprowadza ", gloss("regresja logistyczna", "regresję logistyczną"), ",
      zbudowaną dla takich zmiennych. AIC i BIC przydadzą się w niej
      bez zmian."),

    lc_chapter_next(
      num       = "05",
      title     = "Regresja logistyczna",
      lead      = "gdy zmienna zależna jest binarna",
      target_id = "ch-logistyczna"
    )
  )
)
