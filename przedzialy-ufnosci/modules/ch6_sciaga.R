# ============================================================================
# CHAPTER 6: Sciaga - podsumowanie przedzialow ufnosci
# ============================================================================

ch6_ui <- tabPanel("6. \u015aci\u0105ga",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Praktyczna \u015bci\u0105ga: jak klika\u0107 CI w jamovi, jak odczyta\u0107 wynik
       i jak sformu\u0142owa\u0107 wniosek. Wzory \u2014 na ko\u0144cu, dla zainteresowanych."
    ),

    # ========================================================================
    div(class = "section-title", "Jak zrobi\u0107 przedzia\u0142 ufno\u015bci w jamovi"),

    div(class = "narrative",
      p("W jamovi nie musisz niczego liczy\u0107 \u2014 CI pojawia si\u0119 w raporcie,
        gdy zaznaczysz jeden checkbox. Twoja robota: nazwa\u0107 go, zinterpretowa\u0107,
        wyci\u0105gn\u0105\u0107 wniosek.")
    ),

    div(class = "callout-info",
      tags$strong("Przedzia\u0142 dla \u015bredniej (jedna zmienna ilo\u015bciowa):"),
      tags$ol(
        tags$li(tags$b("Analyses \u2192 T-Tests \u2192 One Sample T-Test")),
        tags$li("Przeci\u0105gnij zmienn\u0105 ilo\u015bciow\u0105 (np. wzrost, plon, czas reakcji) do ",
                tags$em("Dependent Variables")),
        tags$li("W panelu ", tags$em("Additional Statistics"), " zaznacz ",
                tags$b("Confidence interval"), " \u2014 domy\u015blnie 95%"),
        tags$li("W tabeli wynik\u00f3w odczytasz kolumny ",
                tags$code("Mean"), ", ", tags$code("Lower"), ", ", tags$code("Upper"))
      ),
      p(tags$em("jamovi zawsze u\u017cywa rozk\u0142adu t \u2014 nie musisz wybiera\u0107 mi\u0119dzy z a t."))
    ),

    div(class = "callout-info",
      tags$strong("Przedzia\u0142 dla r\u00f3\u017cnicy \u015brednich (dwie grupy):"),
      tags$ol(
        tags$li(tags$b("Analyses \u2192 T-Tests \u2192 Independent Samples T-Test")),
        tags$li("Zmienna ilo\u015bciowa \u2192 ", tags$em("Dependent Variable"),
                ", zmienna grupuj\u0105ca \u2192 ", tags$em("Grouping Variable")),
        tags$li("W panelu ", tags$em("Additional Statistics"), " zaznacz ",
                tags$b("Mean difference"), " i ", tags$b("Confidence interval")),
        tags$li("CI dla r\u00f3\u017cnicy \u015brednich pojawia si\u0119 w wierszu ",
                tags$code("Mean difference"), " \u2014 kolumny ",
                tags$code("Lower"), " / ", tags$code("Upper"))
      ),
      p(tags$strong("Kluczowe:"), " je\u015bli przedzia\u0142 nie zawiera ", tags$b("0"),
        " \u2014 grupy r\u00f3\u017cni\u0105 si\u0119 istotnie.")
    ),

    div(class = "callout-info",
      tags$strong("Przedzia\u0142 dla proporcji (jedna zmienna kategorialna, 2 kategorie):"),
      tags$ol(
        tags$li(tags$b("Analyses \u2192 Frequencies \u2192 2 Outcomes \u2014 Binomial test")),
        tags$li("Przeci\u0105gnij zmienn\u0105 binarn\u0105 (np. zdany/niezdany) do pola zmiennych"),
        tags$li("Zaznacz ", tags$b("Confidence interval"),
                " \u2014 domy\u015blnie 95%, metoda Cloppera-Pearsona (bezpieczna)"),
        tags$li("W tabeli odczytasz ", tags$code("Proportion"), ", ",
                tags$code("Lower"), ", ", tags$code("Upper"))
      )
    ),

    div(class = "callout-info",
      tags$strong("Przedzia\u0142 dla r\u00f3\u017cnicy proporcji (dwie grupy):"),
      tags$ol(
        tags$li(tags$b("Analyses \u2192 Frequencies \u2192 Independent Samples \u2014 \u03c7\u00b2 test of association")),
        tags$li("Dwie zmienne kategorialne w polach ", tags$em("Rows"), " i ", tags$em("Columns")),
        tags$li("W panelu ", tags$em("Statistics"), " zaznacz ", tags$b("Log odds ratio"),
                " lub dla r\u00f3\u017cnicy \u2014 u\u017cyj modu\u0142u ", tags$em("jmv::propTest2"),
                " (R) albo policzmy r\u00f3\u017cnic\u0119 r\u0119cznie z dw\u00f3ch CI")
      ),
      p(tags$em("W praktyce: dla por\u00f3wnania dw\u00f3ch odsetk\u00f3w w jamovi najpro\u015bciej
                odczyta\u0107 dwa osobne CI i zobaczy\u0107, czy si\u0119 nakrywaj\u0105
                (ale pami\u0119taj: nakrywanie CI nie jest r\u00f3wnoznaczne z brakiem istotno\u015bci \u2014
                sprawd\u017a test \u03c7\u00b2)."))
    ),

    # ========================================================================
    div(class = "section-title", "Jak czyta\u0107 wynik jamovi"),

    div(class = "narrative",
      p("Za\u0142\u00f3\u017cmy, \u017ce jamovi poda\u0142:"),
      tags$pre(style = "background: #f8f9fa; padding: 12px; border-radius: 6px;",
        tags$code(
"Mean       Lower (95%)   Upper (95%)
171.3      168.4         174.2"
        )
      ),
      p("To czytasz tak:"),
      tags$ul(
        tags$li(tags$b("Estymata punktowa:"),
                " w badanej pr\u00f3bie \u015brednia wynios\u0142a ", tags$code("171.3"), "."),
        tags$li(tags$b("Przedzia\u0142 ufno\u015bci:"),
                " nasze najlepsze oszacowanie \u015bredniej w ca\u0142ej populacji mie\u015bci si\u0119
                 mi\u0119dzy ", tags$code("168.4"), " a ", tags$code("174.2"), "."),
        tags$li(tags$b("Ufno\u015b\u0107 95%:"),
                " metoda, kt\u00f3r\u0105 u\u017cyli\u015bmy, w 95% powt\u00f3rze\u0144 badania
                 wyprodukuje przedzia\u0142 obejmuj\u0105cy prawdziw\u0105 \u015bredni\u0105.")
      )
    ),

    # ========================================================================
    div(class = "section-title", "Szablony wniosk\u00f3w \u2014 co napisa\u0107 w raporcie"),

    div(class = "callout-success",
      tags$strong("\u015arednia (pojedyncza zmienna):"),
      p(tags$em("\"\u015aredni wzrost student\u00f3w wyni\u00f3s\u0142 171,3 cm
                (95% CI: [168,4; 174,2]).\"")),
      p("Trzy liczby \u2014 i gotowe. Je\u015bli masz warto\u015b\u0107 odniesienia (np. norma = 170):"),
      p(tags$em("\"Przedzia\u0142 nie zawiera warto\u015bci 170, co sugeruje,
                \u017ce \u015brednia w populacji r\u00f3\u017cni si\u0119 od normy.\""))
    ),

    div(class = "callout-success",
      tags$strong("R\u00f3\u017cnica \u015brednich (dwie grupy):"),
      p(tags$em("\"Grupa eksperymentalna osi\u0105gn\u0119\u0142a \u015bredni wynik
                 wy\u017cszy o 4,7 punktu od grupy kontrolnej
                 (95% CI r\u00f3\u017cnicy: [1,2; 8,2]).\"")),
      p(tags$strong("Sprawd\u017a zero:"),
        " przedzia\u0142 nie zawiera 0 \u2014 r\u00f3\u017cnica istotna.
        Je\u015bli zawiera\u0142by 0 \u2014 nie mamy podstaw m\u00f3wi\u0107 o r\u00f3\u017cnicy.")
    ),

    div(class = "callout-success",
      tags$strong("Proporcja:"),
      p(tags$em("\"Odsetek zdaj\u0105cych egzamin wyni\u00f3s\u0142 68%
                (95% CI: [62%; 73%]).\"")),
      p(tags$strong("Sprawd\u017a warto\u015b\u0107 progow\u0105:"),
        " je\u015bli interesuje ci\u0119 pytanie \"czy wi\u0119cej ni\u017c po\u0142owa?\" \u2014
        patrz czy 50% le\u017cy w CI. Je\u015bli nie \u2014 masz odpowied\u017a z 95% ufno\u015bci\u0105.")
    ),

    # ========================================================================
    div(class = "section-title", "Kiedy CI daje odpowied\u017a na hipotez\u0119?"),

    div(class = "callout-warning",
      tags$strong("Zasada pro\u015bcie:"),
      tags$ul(
        tags$li(tags$b("CI dla \u015bredniej"), " vs warto\u015b\u0107 hipotetyczna ",
                tags$code("\u03bc\u2080"), ": je\u015bli ",
                tags$code("\u03bc\u2080"), " le\u017cy ", tags$em("poza"),
                " CI \u2014 odrzucasz hipotez\u0119 \"\u015brednia = \u03bc\u2080\"."),
        tags$li(tags$b("CI dla r\u00f3\u017cnicy \u015brednich"),
                ": je\u015bli ", tags$b("0"), " le\u017cy ", tags$em("poza"),
                " CI \u2014 grupy r\u00f3\u017cni\u0105 si\u0119 istotnie."),
        tags$li(tags$b("CI dla proporcji"),
                " vs warto\u015b\u0107 progowa (np. 0.5): je\u015bli pr\u00f3g le\u017cy ",
                tags$em("poza"),
                " CI \u2014 mamy rozstrzygni\u0119cie z 95% ufno\u015bci\u0105.")
      )
    ),

    # ========================================================================
    div(class = "section-title", "Typowe b\u0142\u0119dy interpretacji"),

    div(class = "callout-danger",
      tags$strong("B\u0141\u0118DNE:"),
      tags$ul(
        tags$li("\"\u015arednia populacji le\u017cy w tym przedziale z 95% prawdopodobie\u0144stwem\" \u2014 ",
                tags$em("\u015brednia populacji jest sta\u0142a, nie losowa!"),
                " To przedzia\u0142 jest losowy (zale\u017cy od pr\u00f3by)."),
        tags$li("\"95% danych le\u017cy w tym przedziale\" \u2014 ",
                tags$em("to nie jest zakres danych!"),
                " CI dotyczy parametru populacji (np. \u015bredniej), nie pojedynczych obserwacji."),
        tags$li("\"\u015arednia z pr\u00f3by le\u017cy w przedziale\" \u2014 ",
                tags$em("oczywi\u015bcie \u017ce tak \u2014 jest w \u015brodku, z definicji."))
      )
    ),

    div(class = "callout-success",
      tags$strong("POPRAWNE:"),
      tags$ul(
        tags$li("\"Gdyby\u015bmy powtarzali badanie, 95% tak skonstruowanych przedzia\u0142\u00f3w
                 zawiera\u0142oby prawdziw\u0105 \u015bredni\u0105 populacji.\""),
        tags$li("\"Metoda, kt\u00f3rej u\u017cyli\u015bmy, daje poprawne przedzia\u0142y
                 w 95% przypadk\u00f3w.\""),
        tags$li("\"Najlepsze oszacowanie \u015bredniej populacji wskazuje,
                 \u017ce le\u017cy ona mi\u0119dzy ... a ..., z ufno\u015bci\u0105 95%.\"")
      )
    ),

    # ========================================================================
    div(class = "section-title", "Drzewo decyzyjne \u2014 kt\u00f3ry CI wybra\u0107?"),

    div(class = "callout-info",
      tags$strong("1. Co chcesz oszacowa\u0107?"),
      tags$ul(
        tags$li(tags$b("Liczb\u0119"), " (\u015bredni\u0105, np. wzrost, plon, czas) \u2192 CI dla \u015bredniej"),
        tags$li(tags$b("Odsetek / proporcj\u0119"), " (np. % zdaj\u0105cych, % wadliwych) \u2192 CI dla proporcji"),
        tags$li(tags$b("R\u00f3\u017cnic\u0119 mi\u0119dzy grupami"), " \u2192 CI dla r\u00f3\u017cnicy \u015brednich lub proporcji")
      ),
      tags$strong("2. Ile masz grup?"),
      tags$ul(
        tags$li(tags$b("Jedna"), " \u2192 jeden CI (np. One Sample T-Test)"),
        tags$li(tags$b("Dwie"), " \u2192 CI dla r\u00f3\u017cnicy (Independent Samples T-Test)
                 \u2014 sprawdzasz, czy zawiera 0"),
        tags$li(tags$b("Wi\u0119cej ni\u017c dwie"), " \u2192 ANOVA + osobne CI dla ka\u017cdej pary")
      ),
      tags$strong("3. Jaki poziom ufno\u015bci?"),
      tags$ul(
        tags$li("Standard: ", tags$b("95%"), " (zaznaczony domy\u015blnie w jamovi)"),
        tags$li("Chcesz by\u0107 bardziej ostro\u017cny (np. medycyna) \u2192 99% (szerszy przedzia\u0142)"),
        tags$li("Wystarczy zgrubny obraz \u2192 90% (w\u0119\u017cszy przedzia\u0142)")
      ),
      p(tags$em("Wa\u017cne: wybieraj poziom ufno\u015bci ",
                tags$b("zanim"), " zobaczysz wynik.
               Potem mo\u017cesz pokaza\u0107 wi\u0119cej poziom\u00f3w naraz (90%, 95%, 99%),
               ale nie wolno wybiera\u0107 \"tego, kt\u00f3ry pasuje do oczekiwanej konkluzji\"."))
    ),

    # ========================================================================
    div(class = "section-title", "Co wp\u0142ywa na szeroko\u015b\u0107 CI"),

    tags$table(class = "table table-bordered",
      style = "font-size: 15px;",
      tags$thead(
        tags$tr(
          tags$th("Czynnik"),
          tags$th("Wzrost \u2192"),
          tags$th("Efekt na CI"),
          tags$th("Co z tym zrobi\u0107?")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("Wielko\u015b\u0107 pr\u00f3by (n)"),
          tags$td("\u2191"),
          tags$td("\u2193 w\u0119\u017cszy (dok\u0142adniejszy)"),
          tags$td("Zbierz wi\u0119cej danych \u2014 ale 4\u00d7 wi\u0119cej, by zaw\u0119zi\u0107 CI o po\u0142ow\u0119")
        ),
        tags$tr(
          tags$td("Poziom ufno\u015bci"),
          tags$td("\u2191"),
          tags$td("\u2191 szerszy (bardziej ostro\u017cny)"),
          tags$td("Wybierz \u015bwiadomie, ", tags$em("przed"), " patrzeniem na wyniki")
        ),
        tags$tr(
          tags$td("Zmienno\u015b\u0107 danych"),
          tags$td("\u2191"),
          tags$td("\u2191 szerszy (wi\u0119cej szumu)"),
          tags$td("Kontroluj warunki pomiaru, sprawd\u017a outliery")
        )
      )
    ),

    # ========================================================================
    # DLA ZAINTERESOWANYCH: wzory schowane w <details>
    # ========================================================================
    div(class = "section-title", "Dla zainteresowanych: wzory"),

    div(class = "narrative",
      p(tags$em("Poni\u017csze wzory s\u0105 tym, co jamovi liczy pod spodem.
                Nie musisz ich pami\u0119ta\u0107 \u2014 ale je\u015bli chcesz zobaczy\u0107, sk\u0105d bior\u0105
                si\u0119 liczby w kolumnach ", tags$code("Lower"), " / ", tags$code("Upper"),
                ", rozwi\u0144 kt\u00f3r\u0105\u015b z sekcji."))
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f4d0"),
        "Wz\u00f3r dla \u015bredniej"
      ),
      div(class = "case-body",
        div(class = "formula-box",
          h4("Przedzia\u0142 dla \u015bredniej (nieznane \u03c3 \u2014 STANDARDOWY)"),
          withMathJax(helpText(
            "$$\\bar{x} \\pm t^*_{\\alpha/2,\\, n-1} \\cdot \\frac{s}{\\sqrt{n}}$$"
          )),
          p("Wymaga: dane ilo\u015bciowe, w przybli\u017ceniu normalne (lub du\u017ce n).
            To wz\u00f3r, kt\u00f3rego u\u017cywa jamovi w One Sample T-Test.")
        ),
        div(class = "formula-box",
          h4("Je\u015bli znamy \u03c3 populacji (rzadko)"),
          withMathJax(helpText(
            "$$\\bar{x} \\pm z^* \\cdot \\frac{\\sigma}{\\sqrt{n}}$$"
          )),
          p("W praktyce prawie nigdy nie u\u017cywany \u2014 \u03c3 zwykle jest nieznane.")
        )
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f4d0"),
        "Wzory dla proporcji"
      ),
      div(class = "case-body",
        div(class = "formula-box",
          h4("Przedzia\u0142 Walda (prosty, ale niedok\u0142adny)"),
          withMathJax(helpText(
            "$$\\hat{p} \\pm z^* \\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}$$"
          )),
          p("Dzia\u0142a \u017ale przy ma\u0142ych n lub skrajnych p (blisko 0 albo 1).")
        ),
        div(class = "formula-box",
          h4("Przedzia\u0142 Wilsona (zalecany)"),
          p("Lepsze pokrycie ni\u017c Wald. U\u017cywa go ", tags$code("prop.test()"),
            " w R.")
        ),
        div(class = "formula-box",
          h4("Przedzia\u0142 Cloppera-Pearsona (dok\u0142adny)"),
          p("Najbezpieczniejszy \u2014 u\u017cywa dwumianu bez aproksymacji.
            jamovi stosuje go w Binomial test.")
        )
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f4cf"),
        "Warto\u015bci krytyczne (dla poziom\u00f3w ufno\u015bci)"
      ),
      div(class = "case-body",
        tags$table(class = "table table-bordered table-striped",
          style = "font-size: 15px;",
          tags$thead(
            tags$tr(
              tags$th("Poziom ufno\u015bci"),
              tags$th(withMathJax("\\(z^*\\)")),
              tags$th(withMathJax("\\(\\alpha\\)"))
            )
          ),
          tags$tbody(
            tags$tr(tags$td("90%"), tags$td("1.645"), tags$td("0.10")),
            tags$tr(tags$td("95%"), tags$td("1.960"), tags$td("0.05")),
            tags$tr(tags$td("99%"), tags$td("2.576"), tags$td("0.01"))
          )
        ),
        p(style = "color: #7f8c8d; font-size: 14px;",
          "Dla rozk\u0142adu t warto\u015bci zale\u017c\u0105 od df = n\u22121; dla du\u017cych n s\u0105 bardzo bliskie z.")
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f4bb"),
        "Jak policzy\u0107 CI w R (zamiast jamovi)"
      ),
      div(class = "case-body",
        tags$pre(style = "background: #f8f9fa; padding: 12px; border-radius: 6px;",
          tags$code(
"# Przedzial dla sredniej (rstatix \u2014 preferowany)
library(rstatix)
dane %>% t_test(zmienna ~ 1, mu = 0, conf.level = 0.95)

# Lub base R
t.test(dane$zmienna, conf.level = 0.95)$conf.int

# Przedzial dla proporcji (Wilson, domyslnie)
prop.test(x = liczba_sukcesow, n = liczba_prob, conf.level = 0.95)

# Dokladny przedzial Cloppera-Pearsona (jak jamovi)
binom.test(x = liczba_sukcesow, n = liczba_prob, conf.level = 0.95)"
          )
        )
      )
    ),

    tags$details(class = "case-study",
      tags$summary(
        span(class = "case-icon", "\U0001f522"),
        "Planowanie wielko\u015bci pr\u00f3by"
      ),
      div(class = "case-body",
        div(class = "formula-box",
          h4("Dla \u015bredniej"),
          withMathJax(helpText(
            "$$n = \\left(\\frac{z^* \\cdot s}{ME_{max}}\\right)^2$$"
          )),
          p("Podaj oczekiwany margines b\u0142\u0119du ",
            withMathJax("\\(ME_{max}\\)"),
            " i przybli\u017conej zmienno\u015bci ",
            withMathJax("\\(s\\)"), " \u2014 dostaniesz minimalne n.")
        ),
        div(class = "formula-box",
          h4("Dla proporcji"),
          withMathJax(helpText(
            "$$n = \\frac{z^{*2} \\cdot \\hat{p}(1-\\hat{p})}{ME_{max}^2}$$"
          )),
          p("Gdy nie znamy p, u\u017cywamy p = 0.5 (daje maksymalne n).")
        )
      )
    ),

    # ========================================================================
    div(class = "chapter-transition",
      p("Czas zastosowa\u0107 t\u0119 wiedz\u0119 w praktyce na rzeczywistym zbiorze danych."),
      actionButton("ch6_to_ch7", "Dalej: \u0106wiczenia \u2192", class = "btn-primary")
    )

  ))
)

# ============================================================================
# SERVER (brak interaktywnych widgetow)
# ============================================================================

ch6_server <- function(input, output, session) {
  # Sciaga nie wymaga logiki server
}
