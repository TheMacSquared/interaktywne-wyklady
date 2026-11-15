# ============================================================================
# CHAPTER 10: Siła efektu
# ============================================================================

# Dane przykładów rozdziału (dawniej w serwerze): podpowiedzi, scenariusze i poziomy efektu.
ch10_dist_hints <- list(
  TZ  = "Np. czas inaktywacji enzymu (s) w dwóch temperaturach blanszowania.",
  ROL = "Np. masa ziarna (g) z dwóch odmian pszenicy.",
  IB  = "Np. czas reakcji operatora (ms) na zmianie rannej vs nocnej."
)

ch10_d_scenarios <- list(
  TZ = list(
    "0.2" = list(
      x1 = 4.50, x2 = 4.56, s = 0.30,
      kontekst  = "pH jogurtu po fermentacji — różnica 0.06 pH między dwoma zakwasami.",
      jednostka = "pH",
      etyk1 = "Zakwas A", etyk2 = "Zakwas B"
    ),
    "0.5" = list(
      x1 = 20.0, x2 = 22.5, s = 5.0,
      kontekst  = "Wilgotność produktu suszonego (%) — bez vs ze stabilizatorem, różnica 2.5 pp.",
      jednostka = "%",
      etyk1 = "Bez stabilizatora", etyk2 = "Ze stabilizatorem"
    ),
    "0.8" = list(
      x1 = 8.0, x2 = 10.0, s = 2.5,
      kontekst  = "Czas inaktywacji enzymów (min) — blanszowanie 80°C vs 95°C, różnica 2 min.",
      jednostka = "min",
      etyk1 = "Blanszowanie 80°C", etyk2 = "Blanszowanie 95°C"
    ),
    "1.2" = list(
      x1 = 5.0, x2 = 8.6, s = 3.0,
      kontekst  = "Liczba drożdży (×10⁶/mL) — dwa szczepy hodowlane, różnica 3.6 × 10⁶/mL.",
      jednostka = "×10⁶/mL",
      etyk1 = "Szczep A", etyk2 = "Szczep B"
    )
  ),
  ROL = list(
    "0.2" = list(
      x1 = 5.0, x2 = 5.4, s = 2.0,
      kontekst  = "Plon pszenicy (t/ha) — kontrola vs nawożenie lekkie, różnica 0.4 t/ha.",
      jednostka = "t/ha",
      etyk1 = "Kontrola", etyk2 = "Nawożenie lekkie"
    ),
    "0.5" = list(
      x1 = 72, x2 = 78, s = 12,
      kontekst  = "Kiełkowalność nasion (%) — nasiona standardowe vs z podkładem, różnica 6 pp.",
      jednostka = "%",
      etyk1 = "Nasiona standardowe", etyk2 = "Nasiona z podkładem"
    ),
    "0.8" = list(
      x1 = 120, x2 = 140, s = 25,
      kontekst  = "Zawartość azotu w glebie (mg/kg) — bez nawozu vs z nawozem azotowym, różnica 20 mg/kg.",
      jednostka = "mg/kg",
      etyk1 = "Bez nawozu azotowego", etyk2 = "Z nawozem azotowym"
    ),
    "1.2" = list(
      x1 = 5.0, x2 = 8.6, s = 3.0,
      kontekst  = "Wzrost sadzonek w 30 dni (cm) — kontrola vs fitohormon, różnica 3.6 cm.",
      jednostka = "cm",
      etyk1 = "Kontrola", etyk2 = "Fitohormon"
    )
  ),
  IB = list(
    "0.2" = list(
      x1 = 72, x2 = 74, s = 10,
      kontekst  = "Tętno spoczynkowe (ud./min) — bez kofeiny vs po wypiciu herbaty czarnej, różnica 2 ud./min.",
      jednostka = "ud./min",
      etyk1 = "Bez kofeiny", etyk2 = "Po herbacie"
    ),
    "0.5" = list(
      x1 = 60, x2 = 66, s = 12,
      kontekst  = "Wynik egzaminu BHP (pkt) — bez korepetycji vs z korepetycjami, różnica 6 pkt.",
      jednostka = "pkt",
      etyk1 = "Bez korepetycji", etyk2 = "Z korepetycjami"
    ),
    "0.8" = list(
      x1 = 12.0, x2 = 14.0, s = 2.5,
      kontekst  = "Czas reakcji operatora (s) — zmiana ranna vs nocna, różnica 2 s.",
      jednostka = "s",
      etyk1 = "Zmiana ranna", etyk2 = "Zmiana nocna"
    ),
    "1.2" = list(
      x1 = 5.0, x2 = 8.6, s = 3.0,
      kontekst  = "Sen (godz.) — okres egzaminacyjny vs ferie, różnica 3.6 h.",
      jednostka = "godz.",
      etyk1 = "Egzaminy", etyk2 = "Ferie"
    )
  )
)

ch10_r_scenarios <- list(
  TZ = list(
    x_label   = "Temperatura fermentacji (°C)",
    y_label   = "pH jogurtu po 24 h",
    seed      = 101,
    x_center  = 42, x_scale = 5,
    y_center  = 4.5, y_scale = 0.2,
    r_sign    = 1
  ),
  ROL = list(
    x_label   = "Opad atmosferyczny (mm)",
    y_label   = "Plon pszenicy (t/ha)",
    seed      = 202,
    x_center  = 450, x_scale = 80,
    y_center  = 5.0, y_scale = 0.8,
    r_sign    = 1
  ),
  IB = list(
    x_label   = "Godziny szkolenia BHP",
    y_label   = "Wypadki na 100 pracowników",
    seed      = 303,
    x_center  = 20, x_scale = 8,
    y_center  = 8.0, y_scale = 2.0,
    r_sign    = -1
  )
)

ch10_r_hints <- list(
  TZ  = "Korelacja między temperaturą fermentacji a pH jogurtu.",
  ROL = "Korelacja między opadem atmosferycznym a plonem pszenicy.",
  IB  = "Korelacja ujemna: im więcej godzin szkolenia BHP, tym mniej wypadków."
)

ch10_v_scenarios <- list(
  TZ = list(
    grp_a = "Opakowanie A", grp_b = "Opakowanie B",
    stan_pos = "Pleśń", stan_neg = "Brak pleśni",
    hint = "Opakowanie a pojawienie się pleśni na produkcie."
  ),
  ROL = list(
    grp_a = "Odmiana A", grp_b = "Odmiana B",
    stan_pos = "Zachwaszczenie powyżej progu", stan_neg = "Zachwaszczenie poniżej progu",
    hint = "Odmiana rośliny a przekroczenie progu zachwaszczenia pola."
  ),
  IB = list(
    grp_a = "Zaszczepieni", grp_b = "Niezaszczepieni",
    stan_pos = "Infekcja", stan_neg = "Brak infekcji",
    hint = "Zaszczepieni vs niezaszczepieni — odsetek infekcji."
  )
)

ch10_v_examples <- list(
  "0.10" = list(p_a = 0.45, p_b = 0.55),
  "0.30" = list(p_a = 0.35, p_b = 0.65),
  "0.50" = list(p_a = 0.25, p_b = 0.75),
  "0.70" = list(p_a = 0.15, p_b = 0.85)
)

ch10_eta_scenarios <- list(
  TZ = list(
    grp    = c("Metoda A", "Metoda B", "Metoda C"),
    y_lab  = "Liczba kolonii bakterii (jedn.)",
    mu_ctr = 50, s = 10
  ),
  ROL = list(
    grp    = c("Nawóz A", "Nawóz B", "Nawóz C"),
    y_lab  = "Plon pszenicy (t/ha)",
    mu_ctr = 5.0, s = 1.5
  ),
  IB = list(
    grp    = c("Zmiana ranna", "Zmiana popołudniowa", "Zmiana nocna"),
    y_lab  = "Liczba wypadków na 100 pracowników (rocznie)",
    mu_ctr = 12, s = 4
  )
)

ch10_eta_examples <- list(
  "0.01" = list(kontekst = "Grupy dają niemal identyczne wyniki — czynnik symboliczny."),
  "0.06" = list(kontekst = "Grupy zauważalnie się różnią, ale rozrzut wewnątrz grup nadal dominuje."),
  "0.14" = list(kontekst = "Czynnik wyraźnie liczy się — 14% zmienności tłumaczy badany czynnik."),
  "0.30" = list(kontekst = "Dominujący efekt — czynnik wyjaśnia 30% wszystkich różnic.")
)

ch10_ui <- list(
  id = "ch-sila-efektu", num = "10", title = "Siła efektu",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 10 · Testowanie hipotez",
      num    = "10",
      title  = "Siła efektu.",
      lead   = "Wynik może być istotny statystycznie i jednocześnie zbyt mały, żeby
                cokolwiek znaczył w praktyce. Miary siły efektu mówią, jak duża jest
                różnica albo zależność, a ta informacja nie zmienia się wraz
                z liczebnością próby."
    ),

    lc_p("Każdy test z rozdziałów 04–09 kończył się jedną z dwóch decyzji:
      odrzucamy H₀ albo nie mamy podstaw, żeby ją odrzucić. Ta decyzja mówi,
      czy dane dają się pogodzić z brakiem efektu. Nie mówi natomiast, jak duży
      jest efekt, który wykryliśmy. Do tego służą miary ",
      gloss("wielkość efektu", "siły efektu"), ". W tym rozdziale każdemu
      poznanemu testowi przypiszemy jego miarę: testom t odpowiada d Cohena,
      korelacji współczynnik r, testowi χ² V Cramera, a ANOVA η²."),

    # ========================================================================
    # Sekcja 1: Motywacja
    # ========================================================================
    lc_h2("ch10-motywacja", "p-wartość nie mierzy ważności"),

    lc_p("Wróćmy do przykładu B4 z wykładu 04. Badanie porównywało IQ w dwóch
      województwach, po 20 000 osób w każdym. Średnie wyniosły 100.4 i 100.0
      punktu przy odchyleniu standardowym 15. ", gloss("przedział ufności", "Przedział ufności"), " dla różnicy,
      [0.11, 0.69] pkt, nie obejmował zera. ", gloss("test t", "Test t"), " daje ten sam werdykt:
      t ≈ 2.67, p ≈ 0.008, więc na poziomie α = 0.05 odrzucamy H₀ o równości
      średnich. Mimo to różnica 0.4 punktu to około 0.03 odchylenia
      standardowego IQ. Wynik jest istotny statystycznie, ale nie ma ",
      gloss("istotność praktyczna", "istotności praktycznej"), "."),

    lc_p("Rozbieżność bierze się stąd, że ", gloss("statystyka testowa"), " dzieli różnicę średnich
      przez ", gloss("błąd standardowy", "błąd standardowy"), ", a błąd
      standardowy maleje jak \\(1/\\sqrt{n}\\). Dla dwóch równolicznych grup
      o tym samym odchyleniu standardowym s statystykę t można zapisać przez
      różnicę wyrażoną w odchyleniach standardowych, którą oznaczymy d:"),

    # t przez d ze znacznikami (d w kolorze efektu, jak odczyty rozdziału)
    lc_formula_annotated(
      "⟦1|t⟧ = \\frac{\\bar{x}_1 - \\bar{x}_2}{s\\sqrt{2/n}} = ⟦2|d⟧ \\cdot \\sqrt{\\frac{⟦3|n⟧}{2}}, \\qquad d = \\frac{⟦4|\\bar{x}_1 - \\bar{x}_2⟧}{⟦5|s⟧}",
      notes  = list("statystyka t", "wielkość efektu", "liczebność grupy",
                    "różnica średnich", "odchylenie standardowe"),
      colors = c("indygo", "wrzos", "szalwia", "accent", "grafit"),
      above  = c(4)
    ),

    lc_p("Ten sam efekt d daje tym większe t i tym mniejszą ", gloss("p-wartość"), ", im
      większa jest próba. Przy dostatecznie dużym n każda niezerowa różnica
      stanie się istotna, a przy małym n nawet duża różnica może nie przekroczyć
      progu. Panel pokazuje dwie populacje oddalone o d odchyleń standardowych
      oraz wynik testu t dla prób po n obserwacji, w których różnica średnich
      wynosi dokładnie d."),

    figure_panel(
      label = "Ryc. 10.1",
      title = "p kontra d: to nie to samo",
      # Wykres i statystyki liczy przeglądarka (modules/figures.js, rycina s05_dist).
      lc_sf_figure("s05_dist",
        control = list(
          selectInput("ch10_dist_scenario", "Przykład", selectize = FALSE,
            choices = c(
              "Enzym (TŻ)"   = "TZ",
              "Ziarno (ROL)" = "ROL",
              "Reakcja (IB)" = "IB"
            ),
            selected = "TZ"
          ),
          lc_sf_slider("ch10_d", "Cohen's d (wielkość efektu)", 0.1, 1.5, 0.3, 0.05),
          lc_sf_slider("ch10_n", "n na grupę", 20, 300, 50, 10)
        ),
        ratio = "2.6/1", max_height = "280px", legend = TRUE,
        after = tagList(
          lc_sf_fill_table(lc_table(
            data.frame(c1 = "@@dist_n@@", c2 = "@@dist_d@@", c3 = "@@dist_t@@", c4 = "@@dist_p@@"),
            cols = list(
              lc_col("c1", "n / grupę", "row"),
              lc_col("c2", "Cohen's d", "text"),
              lc_col("c3", "t", "text"),
              lc_col("c4", "p", "text")
            ),
            narrow = "cards"
          )),
          tags$div(`data-role` = "verdict"),
          lc_variants("ch10_dist_scenario", lapply(ch10_dist_hints, lc_caption))
        ))
    ),

    lc_p("Przy ustawieniach startowych (d = 0.3, po 50 obserwacji w grupie)
      t = 1.50 i p ≈ 0.14, więc nie mamy podstaw do odrzucenia H₀. Ta sama
      różnica przy 90 obserwacjach w grupie daje p ≈ 0.046, a przy 300
      p < 0.001. Krzywe na wykresie przez cały czas wyglądają tak samo, bo
      efekt się nie zmienia. Zmienia się tylko precyzja, z jaką go mierzymy.
      Działa to także w drugą stronę: przy 20 obserwacjach w grupie d = 0.8
      daje p ≈ 0.016, ale d = 0.5 już tylko p ≈ 0.12. Brak istotności przy
      małej próbie nie dowodzi, że efektu nie ma. Oznacza jedynie, że próba
      była za mała, żeby odróżnić go od przypadku."),

    lc_p("Dlatego p-wartość i siła efektu odpowiadają na dwa różne pytania.
      p-wartość mówi, jak trudno pogodzić dane z H₀. Miara siły efektu mówi,
      jak duża jest różnica albo zależność. W raporcie potrzebne są obie,
      a najlepiej razem z przedziałem ufności z wykładu 04, który pokazuje
      jednocześnie, gdzie leży efekt i jak dokładnie go znamy."),

    # ========================================================================
    # Sekcja 2: Cohen's d
    # ========================================================================
    lc_h2("ch10-cohens-d", "Cohen's d — testy t"),

    lc_p("Wielkość d z poprzedniej sekcji ma swoją nazwę: ",
      gloss("d Cohena", "d Cohena"), " (ang. Cohen's d). Wyraża różnicę średnich
      w jednostkach ", gloss("odchylenie standardowe", "odchylenia standardowego"),
      ". Używamy go przy wszystkich wariantach testu t: dla dwóch grup
      niezależnych z rozdziału 08 w mianowniku stoi odchylenie standardowe
      połączone z obu grup."),

    # D Cohena ze znacznikami
    lc_formula_annotated(
      "⟦1|d⟧ = \\frac{⟦2|\\bar{x}_1 - \\bar{x}_2⟧}{⟦3|s_p⟧}, \\qquad s_p = \\sqrt{\\frac{(⟦4|n_1⟧ - 1)\\,⟦5|s_1^2⟧ + (n_2 - 1)\\,s_2^2}{n_1 + n_2 - 2}}",
      notes  = list("d Cohena", "różnica średnich", "połączone odchylenie standardowe",
                    "liczebność grupy", "wariancja w grupie"),
      colors = c("wrzos", "accent", "grafit", "szalwia", "wrzos"),
      above  = c(2, 4, 5)
    ),

    lc_p("W teście t jednej próby z rozdziału 04 porównujemy średnią z wartością
      odniesienia \\(\\mu_0\\), a w teście dla ", gloss("próby zależne", "prób zależnych"), " z rozdziału 08
      liczymy średnią różnic w parach \\(\\bar{d}\\) i dzielimy ją przez
      odchylenie standardowe tych różnic \\(s_d\\)."),

    lc_formula_box(withMathJax(
      "$$d = \\frac{\\bar{x} - \\mu_0}{s} \\quad \\text{(jedna próba)}, \\qquad d = \\frac{\\bar{d}}{s_d} \\quad \\text{(próby zależne)}$$"
    )),

    lc_p("Dzielenie przez odchylenie standardowe sprowadza różnice z różnych
      dziedzin do wspólnej skali. Różnica 5 cm wzrostu i różnica 5 punktów na
      egzaminie to zupełnie inne sytuacje, ale przeliczone na liczbę odchyleń
      standardowych dają się porównać. W przeciwieństwie do t, d nie rośnie
      wraz z n. Większa próba pozwala oszacować d dokładniej, ale nie robi go
      większym. Niektóre programy w mianowniku d dla dwóch grup używają
      pierwiastka ze średniej z dwóch ", gloss("wariancja", "wariancji"), " zamiast \\(s_p\\); przy
      równolicznych grupach obie wersje dają ten sam wynik."),

    lc_p("Cohen zaproponował orientacyjne progi: 0.2 to efekt mały, 0.5 średni,
      a 0.8 duży. Tabela pokazuje, jak wyglądają one w przykładach z panelu
      poniżej. Różnica IQ z przykładu B4 (d ≈ 0.03) leży daleko poniżej
      progu efektu małego."),

    lc_table(
      data.frame(
        c1 = c("mały", "średni", "duży"),
        c2 = c("0.2", "0.5", "0.8"),
        c3 = c(
          "pH jogurtu 4.50 i 4.56 przy SD 0.30",
          "wilgotność suszu 20.0% i 22.5% przy SD 5",
          "czas inaktywacji enzymów 8 i 10 min przy SD 2.5"
        )
      ),
      cols = list(
        lc_col("c1", "Wielkość efektu", "row"),
        lc_col("c2", "|d|", "num"),
        lc_col("c3", "Przykład", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_p("Panel zamienia wybraną wartość d na konkretne średnie i odchylenia
      standardowe w trzech dziedzinach."),

    figure_panel(
      label = "Ryc. 10.2",
      title = "Cohen's d w surowych liczbach",
      ch10_d_fig()
    ),

    lc_p("Nawet efekt średni oznacza silnie zachodzące na siebie rozkłady.
      Przy d = 0.5 krzywe dzielą około 80% powierzchni, a losowo wybrana
      obserwacja z grupy o wyższej średniej przewyższa losowo wybraną
      obserwację z drugiej grupy w około 64% przypadków. Przy d = 0.2 to
      tylko 56%, czyli niewiele więcej niż rzut monetą, a przy d = 0.8 około
      71%. Dopiero przy d = 1.2 wspólna część rozkładów spada do mniej więcej
      połowy. Efekt duży w sensie Cohena nie oznacza więc, że grupy się
      nie pokrywają."),

    # ========================================================================
    # Sekcja 3: r
    # ========================================================================
    lc_h2("ch10-r", "r — korelacja Pearsona"),

    lc_p("Dla korelacji z rozdziału 06 nie trzeba liczyć osobnej miary.
      Współczynnik ", gloss("korelacja Pearsona", "korelacji Pearsona"),
      " \\(r\\) sam jest miarą siły efektu: nie zależy od n i ma stałą skalę
      od -1 do +1. Test korelacji sprawdzał tylko, czy r z próby różni się od
      zera bardziej, niż wynikałoby z przypadku."),

    # Ten sam wzór na r co w rozdziale 06
    ch4_r_formula(),

    lc_p("Łatwiejszy do interpretacji jest często ",
      gloss("współczynnik determinacji", "kwadrat korelacji r²"), ". Mówi on,
      jaką część zmienności y wyjaśnia liniowa zależność od x. Przy r = 0.5
      mamy r² = 0.25: x wyjaśnia 25% zmienności y, a 75% zostaje na inne
      czynniki. Pamiętaj, że r mierzy tylko zależność liniową. Silna zależność
      krzywoliniowa może dać r bliskie zeru. Orientacyjne progi Cohena dla |r|
      to 0.1, 0.3 i 0.5."),

    lc_table(
      data.frame(
        c1 = c("mała", "średnia", "duża"),
        c2 = c("0.1", "0.3", "0.5"),
        c3 = c(
          "1% zmienności wyjaśnione",
          "9% zmienności wyjaśnione",
          "25% zmienności wyjaśnione"
        )
      ),
      cols = list(
        lc_col("c1", "Wielkość efektu", "row"),
        lc_col("c2", "|r|", "num"),
        lc_col("c3", "r²", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_p("Panel pokazuje 50 punktów wylosowanych z populacji o zadanej
      korelacji. Podtytuł wykresu podaje r policzone z tych punktów."),

    figure_panel(
      label = "Ryc. 10.3",
      title = "r w surowych liczbach",
      ch10_r_fig()
    ),

    lc_p("Przy ustawieniach startowych (zadane r = 0.5) z wylosowanych punktów
      wychodzi r = 0.55. Trend widać wyraźnie, ale punkty leżą daleko od
      prostej: x wyjaśnia około jednej czwartej zmienności y. Przy r = 0.3
      zależność da się jeszcze dostrzec, ale wyjaśnia tylko 9% zmienności,
      a przy r = 0.1 trudno ją zauważyć na wykresie. Różnica między r zadanym
      a policzonym z 50 punktów przypomina, że r z próby jest
      ", gloss("estymator", "estymatorem"), " i ma własny rozrzut."),

    # ========================================================================
    # Sekcja 4: Cramér's V
    # ========================================================================
    lc_h2("ch10-cramers-v", "Cramér's V — test chi kwadrat"),

    lc_p("Test χ² z rozdziału 07 ma ten sam problem co test t. Przy tych samych
      proporcjach w ", gloss("tabela kontyngencji", "tabeli kontyngencji"),
      " statystyka χ² rośnie proporcjonalnie do n, a do tego zależy od rozmiaru
      tabeli. Sama wartość χ² nie mówi więc, jak silny jest związek dwóch ",
      gloss("zmienna jakościowa", "zmiennych jakościowych"), ". ",
      gloss("V Cramera", "V Cramera"), " (ang. Cramér's V) dzieli χ² przez n
      i przez rozmiar tabeli, dzięki czemu przyjmuje wartości od 0 (brak
      związku) do 1 (pełna zależność)."),

    # V Cramera ze znacznikami
    lc_formula_annotated(
      "⟦1|V⟧ = \\sqrt{\\frac{⟦2|\\chi^2⟧}{⟦3|n⟧ \\cdot (⟦4|\\min(r, c)⟧ - 1)}}",
      notes  = list("V Cramera", "statystyka χ²", "liczba obserwacji", "mniejszy wymiar tabeli"),
      colors = c("wrzos", "indygo", "szalwia", "szalwia"),
      above  = c(2)
    ),

    lc_p("We wzorze \\(r\\) i \\(c\\) oznaczają liczbę wierszy i kolumn tabeli.
      Dla tabeli 2×2 V jest równe współczynnikowi φ (fi). Gdy obie grupy są
      równoliczne, a ogólny odsetek wynosi 50%, φ jest po prostu różnicą
      odsetków w grupach: V = 0.30 odpowiada na przykład 35% i 65%. Progi
      Cohena zależą od mniejszego wymiaru tabeli."),

    lc_table(
      data.frame(
        c1 = c("mały", "średni", "duży"),
        c2 = c("0.10", "0.30", "0.50"),
        c3 = c("0.07", "0.21", "0.35")
      ),
      cols = list(
        lc_col("c1", "Wielkość efektu", "row"),
        lc_col("c2", "V (min(r, c) = 2, np. 2×2, 2×3)", "num"),
        lc_col("c3", "V (min(r, c) = 3, np. 3×3)", "num")
      ),
      prose = TRUE
    ),

    lc_p("Panel pokazuje odsetki w dwóch równolicznych grupach dla wybranej
      wartości V w tabeli 2×2."),

    figure_panel(
      label = "Ryc. 10.4",
      title = "Cramér's V w tabeli 2×2",
      ch10_v_fig()
    ),

    lc_p("Przy ustawieniach startowych pleśń pojawia się na 35% produktów
      w opakowaniu A i na 65% w opakowaniu B. To różnica 30 punktów
      procentowych, a mimo to według progów Cohena jest to dopiero efekt
      średni. Efekt mały (V = 0.10) oznacza w tym układzie odsetki 45% i 55%,
      a duży (V = 0.50) 25% i 75%. Przy innych proporcjach grup albo innym
      ogólnym odsetku ta sama wartość V odpowiada nieco innej różnicy."),

    # ========================================================================
    # Sekcja 5: eta kwadrat
    # ========================================================================
    lc_h2("ch10-eta2", "eta kwadrat — ANOVA"),

    lc_p(gloss("ANOVA"), " z rozdziału 09 dzieliła całkowitą zmienność wyników na część
      między grupami i część wewnątrz grup. ", gloss("statystyka F", "Statystyka F"), " porównywała te części,
      ale podobnie jak t rośnie wraz z n. ",
      gloss("eta kwadrat", "Eta kwadrat"), " (\\(\\eta^2\\)) mówi, jaki udział
      całkowitej zmienności przypada na różnice między grupami, czyli jaką część
      zmienności wyników tłumaczy badany czynnik. To ta sama idea co r²
      w korelacji."),

    # η² ze znacznikami (bursztyn = między, jak w ANOVA)
    lc_formula_annotated(
      "⟦1|\\eta^2⟧ = \\frac{⟦2|SS_{\\text{między}}⟧}{⟦3|SS_{\\text{całkowita}}⟧}",
      notes  = list("eta kwadrat", "zmienność między grupami", "zmienność całkowita"),
      colors = c("wrzos", "bursztyn", "grafit"),
      above  = c(2)
    ),

    lc_p("Programy podają kilka wariantów tej miary: η² częściowe (ang. partial)
      i η² uogólnione. W jednoczynnikowej ANOVA dla grup
      niezależnych wszystkie trzy warianty są równe zwykłemu η². Różnią się
      dopiero w modelach z kilkoma czynnikami. Orientacyjne progi Cohena to
      0.01, 0.06 i 0.14."),

    lc_table(
      data.frame(
        c1 = c("mały", "średni", "duży"),
        c2 = c("0.01", "0.06", "0.14"),
        c3 = c(
          "czynnik tłumaczy około 1% zmienności",
          "czynnik tłumaczy około 6% zmienności",
          "czynnik tłumaczy co najmniej 14% zmienności"
        )
      ),
      cols = list(
        lc_col("c1", "Wielkość efektu", "row"),
        lc_col("c2", "η²", "num"),
        lc_col("c3", "Interpretacja", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_p("Panel losuje po 30 obserwacji w trzech grupach z populacji o zadanym
      η². Odczyty i tabela pod wykresem podają η² i średnie w populacji
      oraz w wylosowanej próbie."),

    figure_panel(
      label = "Ryc. 10.5",
      title = "η² w ANOVA — trzy grupy",
      ch10_eta_fig()
    ),

    lc_p("Przy ustawieniach startowych (η² = 0.06) średnie grup wynoszą 46.9,
      50 i 53.1 przy odchyleniu standardowym 10 w każdej grupie. Różnice
      średnich o około 3 jednostki giną w rozrzucie wewnątrz grup i pudełka
      niemal całkowicie na siebie zachodzą. Dopiero przy η² = 0.30 (średnie
      42.0, 50 i 58.0) grupy wyraźnie się rozsuwają, choć nadal częściowo
      się pokrywają. η² policzone z wylosowanych 90 punktów nie musi równać
      się wartości w populacji. Dla ustawień startowych wynosi 0.02, bo przy
      30 obserwacjach w grupie średnie z próby mocno się wahają."),

    # ========================================================================
    # Domknięcie
    # ========================================================================
    lc_p("Progi Cohena (mały, średni, duży) to konwencja zaproponowana
      w latach 60. jako punkt odniesienia na wypadek, gdy nic lepszego nie
      jest dostępne. Nie są bezwzględnym standardem. To, czy efekt jest ważny,
      zależy od dziedziny i od stawki. Lek, który obniża śmiertelność
      z 10% do 8%, ma według progów Cohena efekt poniżej małego (φ ≈ 0.035),
      a w dużej populacji może uratować wiele osób. Z kolei w ocenie sensorycznej żywności efekt mały bywa
      niezauważalny dla konsumenta. Najlepiej oceniać efekt także w jego
      naturalnych jednostkach: punktach IQ, minutach, tonach z hektara."),

    lc_p("Dla testu proporcji z rozdziału 05 nie wprowadzaliśmy osobnej miary.
      Tam efektem jest sama różnica między odsetkiem w próbie a wartością
      odniesienia, wyrażona w punktach procentowych, najlepiej razem
      z przedziałem ufności."),

    lc_note("Zasada", rule = TRUE,
      "Raportuj p-wartość razem z miarą siły efektu i oceniaj efekt w kontekście
       dziedziny, a nie tylko według progów Cohena."
    ),

    lc_p("Ten rozdział zamyka część narracyjną wykładu. Zaczęliśmy od logiki
      testu: zakładamy H₀ i sprawdzamy, jak zaskakujące byłyby przy niej
      nasze dane. Poziom istotności α ustalamy przed analizą, a p-wartość
      to prawdopodobieństwo wyniku co najmniej tak skrajnego jak obserwowany,
      gdy H₀ jest prawdziwa. Nie jest to prawdopodobieństwo, że H₀ jest
      prawdziwa, a brak podstaw do odrzucenia H₀ nie dowodzi, że jest ona
      prawdziwa. Potem poznaliśmy sześć testów dla różnych typów zmiennych,
      a na koniec miary, które mówią, jak duży jest wykryty efekt."),

    lc_p("Pozostaje praktyczne pytanie: który test wybrać do konkretnych danych.
      Odpowiada na nie drzewo decyzyjne w następnym rozdziale. Prowadzi ono
      od typu zmiennych i liczby grup do testu."),

    lc_chapter_next(
      num       = "11",
      title     = "Drzewo decyzyjne",
      lead      = "mapa wyboru testu — od typu zmiennych do konkretnego testu.",
      target_id = "ch-drzewo"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

# Rozdział nie ma logiki serwera: Ryc. 10.1 liczy przeglądarka (modules/figures.js), a przykłady
# w surowych liczbach (Ryc. 10.2–10.5) R opisuje wariantami przy budowaniu (modules/figures.R).
ch10_server <- function(input, output, session) {}
