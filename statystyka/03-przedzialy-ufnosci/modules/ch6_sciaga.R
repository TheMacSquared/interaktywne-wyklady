# ============================================================================
# CHAPTER 6: Sciaga - podsumowanie przedzialow ufnosci
# ============================================================================

ch6_ui <- list(
  id    = "ch-sciaga",
  num   = "06",
  title = "Ściąga",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 06 · Przedziały ufności",
      num    = "06",
      title  = "Ściąga.",
      lead   = "Praktyczna ściąga: jak klikać CI w jamovi, jak odczytać
                wynik i jak sformułować wniosek. Wzory — na końcu,
                dla zainteresowanych."
    ),

    lc_h2("ch6-jamovi", "Jak zrobić przedział ufności w jamovi"),

    tagList(
      p("W jamovi nie trzeba niczego liczyć — CI pojawia się w raporcie
        po zaznaczeniu jednego checkboxa. Zadanie analityka: nazwać go,
        zinterpretować, wyciągnąć wniosek."),

      lc_note("Jamovi", title = "Przedział dla średniej (jedna zmienna ilościowa)",
        tags$ol(
          tags$li(tags$b("Analyses → T-Tests → One Sample T-Test")),
          tags$li("Przeciągnij ", gloss("zmienna ilościowa", "zmienną ilościową"), " (np. wzrost, plon,
                   czas reakcji) do ", tags$em("Dependent Variables")),
          tags$li("W panelu ", tags$em("Additional Statistics"), " zaznacz ",
                  tags$b("Confidence interval"), " — domyślnie 95%"),
          tags$li("Odczytaj w tabeli wyników kolumny ",
                  tags$code("Mean"), ", ", tags$code("Lower"), ", ",
                  tags$code("Upper"))
        )
      ),

      lc_note("Jamovi", title = "Przedział dla różnicy średnich (dwie grupy)",
        tags$ol(
          tags$li(tags$b("Analyses → T-Tests → Independent Samples T-Test")),
          tags$li("Zmienna ilościowa → ", em_("Dependent Variable"),
                  ", zmienna grupująca → ", tags$em("Grouping Variable")),
          tags$li("W panelu ", tags$em("Additional Statistics"), " zaznacz ",
                  tags$b("Mean difference"), " i ", tags$b("Confidence interval")),
          tags$li("CI dla różnicy średnich pojawia się w wierszu ",
                  tags$code("Mean difference"), " — kolumny ",
                  tags$code("Lower"), " / ", tags$code("Upper"))
        ),
        p(tags$strong("Kluczowe:"),
          " jeśli przedział nie zawiera ", tags$b("0"),
          " — grupy różnią się istotnie.")
      ),

      lc_note("Jamovi", title = "Przedział dla proporcji (jedna zmienna kategorialna,
                     2 kategorie)",
        tags$ol(
          tags$li(tags$b("Analyses → Frequencies → 2 Outcomes — Binomial test")),
          tags$li("Przeciągnij zmienną binarną (np. zdany/niezdany)
                   do pola zmiennych"),
          tags$li("Zaznacz ", tags$b("Confidence interval"),
                  " — domyślnie 95%, metoda ",
                  gloss("przedział Cloppera-Pearsona", "Cloppera-Pearsona"),
                  " (bezpieczna)"),
          tags$li("Odczytaj w tabeli ", tags$code("Proportion"), ", ",
                  tags$code("Lower"), ", ", tags$code("Upper"))
        )
      ),

      lc_note("Jamovi", title = "Przedział dla różnicy proporcji (dwie grupy)",
        tags$ol(
          tags$li(tags$b("Analyses → Frequencies → Independent Samples — χ² test of association")),
          tags$li("Dwie zmienne kategorialne w polach ", tags$em("Rows"),
                  " i ", tags$em("Columns")),
          tags$li("W panelu ", tags$em("Statistics"), " zaznacz ",
                  tags$b("Log odds ratio"), " lub dla różnicy — użyj modułu ",
                  tags$em("jmv::propTest2"),
                  " (R) albo policz różnicę ręcznie z dwóch CI")
        ),
        p(tags$em("W praktyce: dla porównania dwóch odsetków w jamovi
                   najprościej odczytać dwa osobne CI i zobaczyć, czy się
                   nakrywają (ale pamiętaj: nakrywanie CI nie jest
                   równoznaczne z brakiem istotności — sprawdź test χ²)."))
      )
    ),

    lc_h2("ch6-jak-czytac", "Jak czytać wynik jamovi"),

    tagList(
      p("Załóżmy, że jamovi podał:"),
      tags$pre(class = "lc-code-block",
        tags$code(
"Mean       Lower (95%)   Upper (95%)
171.3      168.4         174.2"
        )
      ),
      p("Jak to czytać:"),
      tags$ul(
        tags$li(tags$b(gloss("estymata", "Estymata"), " punktowa:"),
                " w badanej próbie średnia wyniosła ",
                tags$code("171.3"), "."),
        tags$li(tags$b(gloss("przedział ufności", "Przedział ufności"), ":"),
                " nasze najlepsze oszacowanie średniej w całej populacji
                 mieści się między ", tags$code("168.4"), " a ",
                tags$code("174.2"), "."),
        tags$li(tags$b("Ufność 95%:"),
                " metoda, której użyliśmy, w 95% powtórzeń badania
                 wyprodukuje przedział obejmujący prawdziwą średnią.")
      )
    ),

    lc_h2("ch6-szablony", "Szablony wniosków — co napisać w raporcie"),

    tagList(
      lc_note("Przykład", title = "Średnia (pojedyncza zmienna)",
        p(tags$em("„Średni wzrost studentów wyniósł 171.3 cm
                   (95% CI: [168.4, 174.2]).”")),
        p("Trzy liczby — i gotowe. Jeśli jest wartość odniesienia
           (np. norma = 170):"),
        p(tags$em("„Przedział nie zawiera wartości 170, co sugeruje,
                   że średnia w populacji różni się od normy.”"))
      ),

      lc_note("Przykład", title = "Różnica średnich (dwie grupy)",
        p(tags$em("„Grupa eksperymentalna osiągnęła średni wynik
                   wyższy o 4.7 punktu od grupy kontrolnej
                   (95% CI różnicy: [1.2, 8.2]).”")),
        p(tags$strong("Sprawdź zero:"),
          " przedział nie zawiera 0 — różnica istotna.
           Jeśli zawierałby 0 — nie mamy podstaw mówić o różnicy.")
      ),

      lc_note("Przykład", title = "Proporcja",
        p(tags$em("„Odsetek zdających egzamin wyniósł 68%
                   (95% CI: [62%, 73%]).”")),
        p(tags$strong("Sprawdź wartość progową:"),
          " jeśli pytanie brzmi „czy więcej niż połowa?” —
           patrz, czy 50% leży w CI. Jeśli nie — mamy odpowiedź z 95%
           ufnością.")
      )
    ),

    lc_h2("ch6-ci-hipoteza", "Kiedy CI daje odpowiedź na hipotezę?"),

    tagList(
      lc_note("Zasada", rule = TRUE,
        tags$ul(
          tags$li(tags$b("CI dla średniej"), " vs wartość hipotetyczna ",
                  tags$code("μ₀"), ": jeśli ",
                  tags$code("μ₀"), " leży ", tags$em("poza"),
                  " CI — odrzucamy hipotezę „średnia = μ₀”."),
          tags$li(tags$b("CI dla różnicy średnich"),
                  ": jeśli ", tags$b("0"), " leży ", tags$em("poza"),
                  " CI — grupy różnią się istotnie."),
          tags$li(tags$b("CI dla proporcji"),
                  " vs wartość progowa (np. 0.5): jeśli próg leży ",
                  tags$em("poza"),
                  " CI — mamy rozstrzygnięcie z 95% ufnością.")
        )
      )
    ),

    lc_h2("ch6-typowe-bledy", "Typowe błędy interpretacji"),

    tagList(
      lc_warn("Błąd",
        tags$ul(
          tags$li("„Średnia populacji leży w tym przedziale z 95%
                   prawdopodobieństwem” — ",
                  tags$em("średnia populacji jest stała, nie losowa!"),
                  " To przedział jest losowy (zależy od próby)."),
          tags$li("„95% danych leży w tym przedziale” — ",
                  tags$em("to nie jest zakres danych!"),
                  " CI dotyczy ", gloss("parametr", "parametru"), " populacji (np. średniej),
                   nie pojedynczych obserwacji."),
          tags$li("„Średnia z próby leży w przedziale” — ",
                  tags$em("oczywiście że tak — jest w środku, z definicji."))
        )
      ),

      lc_note("Poprawnie",
        tags$ul(
          tags$li("„Gdybyśmy powtarzali badanie, 95% tak skonstruowanych
                   przedziałów zawierałoby prawdziwą średnią populacji.”"),
          tags$li("„Metoda, której użyliśmy, daje poprawne przedziały
                   w 95% przypadków.”"),
          tags$li("„Najlepsze oszacowanie średniej populacji wskazuje,
                   że leży ona między … a …, z ufnością 95%.”")
        )
      )
    ),

    lc_h2("ch6-drzewo", "Drzewo decyzyjne — który CI wybrać?"),

    tagList(
      tagList(
        p(tags$strong("1. Co szacujemy?")),
        tags$ul(
          tags$li(tags$b("Liczbę"),
                  " (średnią, np. wzrost, plon, czas) → CI dla średniej"),
          tags$li(tags$b("Odsetek / proporcję"),
                  " (np. % zdających, % wadliwych) → CI dla proporcji"),
          tags$li(tags$b("Różnicę między grupami"),
                  " → CI dla różnicy średnich lub proporcji")
        ),
        p(tags$strong("2. Ile mamy grup?")),
        tags$ul(
          tags$li(tags$b("Jedna"), " → jeden CI (np. One Sample T-Test)"),
          tags$li(tags$b("Dwie"),
                  " → CI dla różnicy (Independent Samples T-Test)
                   — sprawdzamy, czy zawiera 0"),
          tags$li(tags$b("Więcej niż dwie"),
                  " → ", gloss("ANOVA"), " + osobne CI dla każdej pary")
        ),
        p(tags$strong("3. Jaki poziom ufności?")),
        tags$ul(
          tags$li("Standard: ", tags$b("95%"),
                  " (zaznaczony domyślnie w jamovi)"),
          tags$li("Potrzebna większa ostrożność (np. medycyna) → 99%
                   (szerszy przedział)"),
          tags$li("Wystarczy zgrubny obraz → 90% (węższy przedział)")
        ),
        p(tags$em("Ważne: wybieraj ", gloss("poziom ufności"), " ",
                  tags$b("zanim"),
                  " pojawi się wynik. Potem można pokazać więcej
                   poziomów naraz (90%, 95%, 99%), ale nie wolno
                   wybierać „tego, który pasuje do oczekiwanej
                   konkluzji”."))
      )
    ),

    lc_h2("ch6-szerokosc-tab", "Co wpływa na szerokość CI"),

    lc_table(
      data.frame(
        factor = c("Wielkość próby (n)", "Poziom ufności", "Zmienność danych"),
        up = c("↑", "↑", "↑"),
        effect = c("↓ węższy (dokładniejszy)", "↑ szerszy (bardziej ostrożny)",
                   "↑ szerszy (więcej szumu)"),
        action = I(list(
          "Zbierz więcej danych — ale 4× więcej, by zawęzić CI o połowę",
          tagList("Wybierz świadomie, ", tags$em("przed"), " patrzeniem na wyniki"),
          "Kontroluj warunki pomiaru, sprawdź outliery"
        ))
      ),
      cols = list(
        lc_col("factor", "Czynnik", "row"),
        lc_col("up", "Wzrost →", "text"),
        lc_col("effect", "Efekt na CI", "text"),
        lc_col("action", "Co z tym zrobić?", "text")
      ),
      narrow = "cards",
      prose = TRUE
    ),

    lc_h2("ch6-wzory", "Dla zainteresowanych: wzory"),

    tagList(
      p(tags$em("Poniższe wzory są tym, co jamovi liczy pod spodem.
                 Nie trzeba ich pamiętać — ale żeby zobaczyć,
                 skąd biorą się liczby w kolumnach ",
                tags$code("Lower"), " / ", tags$code("Upper"),
                ", rozwiń którąś z sekcji.")),

      lc_more("Wzór dla średniej",
        lc_formula_box(
          h4("Przedział dla średniej (nieznane σ — STANDARDOWY)"),
          withMathJax(helpText(
            "$$\\bar{x} \\pm t^*_{\\alpha/2,\\, n-1} \\cdot \\frac{s}{\\sqrt{n}}$$"
          )),
          p("Wymaga: dane ilościowe, w przybliżeniu normalne
             (lub duże n). To wzór, którego używa jamovi
             w One Sample T-Test.")
        ),
        lc_formula_box(
          h4("Jeśli znamy σ populacji (rzadko)"),
          withMathJax(helpText(
            "$$\\bar{x} \\pm z^* \\cdot \\frac{\\sigma}{\\sqrt{n}}$$"
          )),
          p("W praktyce prawie nigdy nie używany — σ zwykle jest
             nieznane.")
        )
      ),

      lc_more("Wzory dla proporcji",
        lc_formula_box(
          h4("Przedział Walda (prosty, ale niedokładny)"),
          withMathJax(helpText(
            "$$\\hat{p} \\pm z^* \\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}$$"
          )),
          p("Działa źle przy małych n lub skrajnych p (blisko 0 albo 1).")
        ),
        lc_formula_box(
          h4("Przedział Wilsona (zalecany)"),
          p("Lepsze ", gloss("pokrycie"), " niż Wald.")
        ),
        lc_formula_box(
          h4("Przedział Cloppera-Pearsona (dokładny)"),
          p("Najbezpieczniejszy — używa dwumianu bez aproksymacji.
             jamovi stosuje go w Binomial test.")
        )
      ),

      lc_more("Wartości krytyczne (dla poziomów ufności)",
        lc_table(
          data.frame(level = c("90%", "95%", "99%"),
                     z = c(1.645, 1.960, 2.576),
                     alpha = c(0.10, 0.05, 0.01)),
          cols = list(
            lc_col("level", "Poziom ufności", "row"),
            lc_col("z", "Mnożnik", digits = 3),
            lc_col("alpha", "α", digits = 2)
          )
        ),
        lc_caption(
          "Dla średniej program bierze nieco większy mnożnik t* z ", gloss("rozkład t-Studenta", "rozkładu t"),
          "; przy dużych próbach różnica znika.")
      ),

      lc_more("Planowanie wielkości próby",
        lc_formula_box(
          h4("Dla średniej"),
          withMathJax(helpText(
            "$$n = \\left(\\frac{z^* \\cdot s}{ME_{max}}\\right)^2$$"
          )),
          p("Podaj oczekiwany ", gloss("margines błędu"), " ",
            withMathJax("\\(ME_{max}\\)"),
            " i przybliżonej zmienności ",
            withMathJax("\\(s\\)"),
            " — wynikiem jest minimalne n.")
        ),
        lc_formula_box(
          h4("Dla proporcji"),
          withMathJax(helpText(
            "$$n = \\frac{z^{*2} \\cdot \\hat{p}(1-\\hat{p})}{ME_{max}^2}$$"
          )),
          p("Gdy nie znamy p, używamy p = 0.5 (daje maksymalne n).")
        )
      )
    ),

    lc_chapter_next(
      num       = "07",
      title     = "Ćwiczenia",
      lead      = "czas zastosować tę wiedzę na rzeczywistym zbiorze danych",
      target_id = "ch-cwiczenia"
    )
  )
)

# ============================================================================
# SERVER (brak interaktywnych widgetow)
# ============================================================================

ch6_server <- function(input, output, session) {
  # Sciaga nie wymaga logiki server
}
