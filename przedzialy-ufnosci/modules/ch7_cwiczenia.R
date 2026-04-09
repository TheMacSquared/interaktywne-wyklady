# ============================================================================
# CHAPTER 7: Cwiczenia praktyczne na zbiorze CASchools (Jamovi)
# ============================================================================

ch7_ui <- tabPanel("7. \u0106wiczenia",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Czas zastosowa\u0107 wszystko, co poznali\u015bmy o przedzia\u0142ach ufno\u015bci na rzeczywistych danych."
    ),

    div(class = "section-title", "\u0106wiczenia praktyczne \u2014 CASchools"),

    div(class = "narrative",
      p(tags$b("Czas trwania:"), " ~ 90 minut \u00b7 ",
        tags$b("Narz\u0119dzie:"), " Jamovi \u00b7 ",
        tags$b("Zbi\u00f3r:"), " CASchools"),
      p("Pracujemy na klasycznym zbiorze ", tags$b("CASchools"),
        " \u2014 dane ze 420 okr\u0119g\u00f3w szkolnych w Kalifornii.
        Ka\u017cdy wiersz to jeden okr\u0119g; mamy m.in. \u015bredni wynik z czytania (",
        tags$code("read"), ") i matematyki (", tags$code("math"),
        "), liczb\u0119 uczni\u00f3w (", tags$code("students"), "), nauczycieli (",
        tags$code("teachers"), "), procent uczni\u00f3w ucz\u0105cych si\u0119 angielskiego jako drugiego j\u0119zyka (",
        tags$code("english"), "), \u015bredni doch\u00f3d w okr\u0119gu (", tags$code("income"), ") i inne."),
      p("Te okr\u0119gi mo\u017cna traktowa\u0107 jako pr\u00f3b\u0119 \u2014 wnioskujemy o ",
        tags$em("populacji"), " hipotetycznych okr\u0119g\u00f3w o podobnej charakterystyce.")
    ),

    div(class = "callout-info",
      p(tags$b("Otw\u00f3rz plik "), tags$code("dane/caschools.csv"),
        tags$b(" w Jamovi"),
        " \u2014 dalsze ruchy myszk\u0105 om\u00f3wimy razem na zaj\u0119ciach.
        Po ka\u017cdym zadaniu klikamy ", tags$em("\u201ePoka\u017c rozwi\u0105zanie\u201d"),
        ", \u017ceby sprawdzi\u0107, czy wnioski si\u0119 zgadzaj\u0105.")
    ),

    # ======================================================================
    # BLOK 1: CI dla sredniej
    # ======================================================================

    div(class = "section-title", "Blok 1: Przedzia\u0142 ufno\u015bci dla \u015bredniej (~25 min)"),

    # --- Zadanie 1 ---
    div(class = "widget-block",
      h4("Zadanie 1 \u2014 Jak czytaj\u0105 dzieci w Kalifornii?"),
      div(class = "narrative",
        p("Kuratorium o\u015bwiaty pyta nas o jedno: ",
          tags$em("\u201ejaki jest typowy \u015bredni wynik z czytania w kalifornijskim okr\u0119gu?\u201d"),
          " Maj\u0105 420 okr\u0119g\u00f3w \u2014 ca\u0142kiem spor\u0105 pr\u00f3b\u0119 \u2014 i chc\u0105 jednej liczby plus uczciwego oszacowania,
          jak bardzo ta liczba mog\u0142aby si\u0119 zmieni\u0107, gdyby pobra\u0107 inn\u0105 pr\u00f3b\u0119."),
        p("Wyznacz w Jamovi 95% przedzia\u0142 ufno\u015bci dla \u015bredniej zmiennej ",
          tags$code("read"), ". Zanim klikniesz rozwi\u0105zanie, zapisz w\u0142asn\u0105 odpowied\u017a:
          ile wynosi \u015brednia, ile wynosz\u0105 granice CI i \u2014 co wa\u017cniejsze \u2014 ",
          tags$em("co dok\u0142adnie"), " powiedzieliby\u015bmy kuratorium jednym zdaniem.")
      ),
      actionButton("ch7_ans1", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol1")
    ),

    # --- Zadanie 2 ---
    div(class = "widget-block",
      h4("Zadanie 2 \u2014 A z matematyk\u0105 lepiej czy gorzej?"),
      div(class = "narrative",
        p("Kurator chce por\u00f3wna\u0107 czytanie z matematyk\u0105. Powt\u00f3rz t\u0119 sam\u0105 analiz\u0119 dla zmiennej ",
          tags$code("math"), ". Spojrzyjcie na obydwa przedzia\u0142y obok siebie."),
        p("Pytanie nie brzmi ", tags$em("\u201ekt\u00f3ra \u015brednia jest wi\u0119ksza\u201d"),
          " (to wida\u0107 go\u0142ym okiem) tylko: dlaczego ", tags$b("przedzia\u0142y"),
          " maj\u0105 r\u00f3\u017cn\u0105 szeroko\u015b\u0107? Liczba okr\u0119g\u00f3w jest taka sama, wi\u0119c co decyduje?")
      ),
      actionButton("ch7_ans2", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol2")
    ),

    # --- Zadanie 3 ---
    div(class = "widget-block",
      h4("Zadanie 3 \u2014 Co je\u015bli mieliby\u015bmy tylko ma\u0142\u0105 pr\u00f3b\u0119?"),
      div(class = "narrative",
        p("Wyobra\u017a sobie inn\u0105 sytuacj\u0119: kurator dosta\u0142 dane tylko z 30 okr\u0119g\u00f3w \u2014
          mo\u017ce z jednej, lokalnej delegatury. Za\u0142\u00f3\u017cmy, \u017ce to pierwsze 30 wierszy zbioru."),
        p("Zr\u00f3b CI dla ", tags$code("read"),
          " na tej ma\u0142ej pr\u00f3bie i por\u00f3wnaj go z poprzednim. Zwr\u00f3\u0107 uwag\u0119,
          \u017ce zmieni\u0142y si\u0119 ", tags$em("trzy"),
          " rzeczy naraz \u2014 i ka\u017cda z nich pcha CI w t\u0119 sam\u0105 stron\u0119.
          Spr\u00f3bujcie wskaza\u0107 wszystkie trzy zanim zajrzycie do rozwi\u0105zania."),
        p(tags$em("Bonus do dyskusji:"),
          " czy \u015brednia z tych 30 okr\u0119g\u00f3w jest w og\u00f3le wiarygodna jako oszacowanie ca\u0142ej Kalifornii?
          Co m\u00f3wi nam o tym sam fakt, \u017ce wzi\u0119li\u015bmy ", tags$em("pierwsze"),
          " 30, a nie 30 losowo wybranych?")
      ),
      actionButton("ch7_ans3", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol3")
    ),

    # --- Zadanie 4 ---
    div(class = "widget-block",
      h4("Zadanie 4 \u2014 Ile ufno\u015bci kupujemy za szeroko\u015b\u0107?"),
      div(class = "narrative",
        p("Wy\u0142\u0105cz filtr i wr\u00f3\u0107 do pe\u0142nych 420 okr\u0119g\u00f3w.
          Tym razem zostawiamy zmienn\u0105 ", tags$code("read"),
          " i bawimy si\u0119 ", tags$em("poziomem ufno\u015bci"),
          ": 90%, 95%, 99%."),
        p("Zapiszcie sobie trzy pary granic i por\u00f3wnajcie marginesy b\u0142\u0119du.
          Pytanie nie jest \u201ekt\u00f3ry CI jest najwi\u0119kszy\u201d (oczywiste),
          tylko ", tags$b("o ile wi\u0119kszy"),
          " \u2014 i czy ten dodatkowy zysk ufno\u015bci jest tego wart, gdy m\u00f3wimy o wyniku z testu z punkt\u00f3w."),
        p(tags$em("Pytanie do dyskusji:"),
          " kto \u017c\u0105da\u0142by od was 99% \u2014 statystyk akademicki czy in\u017cynier od bezpiecze\u0144stwa lot\u00f3w? Dlaczego?")
      ),
      actionButton("ch7_ans4", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol4")
    ),

    # ======================================================================
    # BLOK 2: CI dla proporcji
    # ======================================================================

    div(class = "section-title", "Blok 2: Przedzia\u0142 ufno\u015bci dla proporcji (~20 min)"),

    # --- Zadanie 5 ---
    div(class = "widget-block",
      h4("Zadanie 5 \u2014 Przepe\u0142nione klasy"),
      div(class = "narrative",
        p("Pedagodzy alarmuj\u0105: przy stosunku ", tags$em("students/teachers"),
          " powy\u017cej 20 trudno m\u00f3wi\u0107 o indywidualnym podej\u015bciu do ucznia.
          Chcemy wiedzie\u0107: ", tags$b("jaki odsetek okr\u0119g\u00f3w w Kalifornii przekracza ten pr\u00f3g?"),
          " Nie pytamy o nasz konkretny zbi\u00f3r 420 okr\u0119g\u00f3w \u2014 pytamy o populacj\u0119,
          z kt\u00f3rej te okr\u0119gi pochodz\u0105."),
        p("Stw\u00f3rzcie w Jamovi zmienn\u0105 binarn\u0105 ", tags$em("\u201eSTR > 20: tak/nie\u201d"),
          " i wyznaczcie 95% CI dla proporcji \u201etak\u201d.
          Po drodze sprawd\u017acie, czy w og\u00f3le mo\u017cemy ufa\u0107 takiemu CI \u2014
          warunki sensowno\u015bci pami\u0119tamy ze \u015bci\u0105gi.")
      ),
      actionButton("ch7_ans5", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol5")
    ),

    # --- Zadanie 6 ---
    div(class = "widget-block",
      h4("Zadanie 6 \u2014 Dystrykty z du\u017cym odsetkiem English learners"),
      div(class = "narrative",
        p("Zmienna ", tags$code("english"),
          " m\u00f3wi nam, jaki procent uczni\u00f3w w danym okr\u0119gu uczy si\u0119 angielskiego jako drugiego j\u0119zyka.
          Powiedzmy, \u017ce okr\u0119gi z wynikiem ", tags$em("powy\u017cej 20%"),
          " uznajemy za \u201ej\u0119zykowo wymagaj\u0105ce\u201d \u2014 nauczyciele potrzebuj\u0105 tam dodatkowego wsparcia."),
        p("Polityk o\u015bwiatowy chce wiedzie\u0107: ", tags$em("ile takich okr\u0119g\u00f3w jest w populacji?"),
          " Powt\u00f3rzcie schemat z zadania 5 dla nowej zmiennej.
          Por\u00f3wnajcie szeroko\u015b\u0107 tego CI z poprzednim \u2014 jeden jest ciasniejszy.
          Dlaczego, skoro n jest takie samo?")
      ),
      actionButton("ch7_ans6", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol6")
    ),

    # --- Zadanie 7 ---
    div(class = "widget-block",
      h4("Zadanie 7 \u2014 Co je\u015bli mamy tylko 25 okr\u0119g\u00f3w?"),
      div(class = "narrative",
        p("Tym razem wyobra\u017amy sobie nowego stra\u017cnika danych:
          ma dane z dwudziestu pi\u0119ciu pierwszych okr\u0119g\u00f3w (znowu, regionalna delegatura).
          Zostawcie t\u0119 sam\u0105 zmienn\u0105 ", tags$em("english > 20"),
          " i policzcie 95% CI na tej szczup\u0142ej pr\u00f3bie."),
        p("Pierwsze, co rzuci si\u0119 w oczy, to ", tags$em("inna proporcja"),
          " \u2014 i b\u0119dzie to dla nas dobre paliwo do dyskusji o tym,
          czemu \u201epierwsze 25 wierszy\u201d to nie to samo, co \u201elosowa pr\u00f3ba 25 wierszy\u201d."),
        p("Drugie: szeroko\u015b\u0107 CI b\u0119dzie ", tags$em("dramatyczna"),
          ". Pomy\u015blcie, kiedy w og\u00f3le mo\u017cna takiemu przedzia\u0142owi ufa\u0107
          i dlaczego Jamovi w tle u\u017cywa metody Cloppera-Pearsona, a nie szkolnego wzoru Walda.")
      ),
      actionButton("ch7_ans7", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol7")
    ),

    # ======================================================================
    # BLOK 3: Planowanie i interpretacja
    # ======================================================================

    div(class = "section-title", "Blok 3: Planowanie pr\u00f3by i interpretacja (~20 min)"),

    # --- Zadanie 8 ---
    div(class = "widget-block",
      h4("Zadanie 8 \u2014 Ile okr\u0119g\u00f3w nale\u017cy zbada\u0107?"),
      div(class = "narrative",
        p("Wyobra\u017a sobie, \u017ce kuratorium planuje nowe, pe\u0142ne badanie wynik\u00f3w czytania
          i chce nie tyle wynik, co ", tags$b("gwarantowan\u0105 precyzj\u0119"),
          ": margines b\u0142\u0119du nie wi\u0119kszy ni\u017c 2 punkty, przy 95% ufno\u015bci.
          Pyta nas: ", tags$em("\u201eile okr\u0119g\u00f3w mamy w\u0142a\u015bciwie obj\u0105\u0107 badaniem?\u201d")),
        p("Z Descriptives w Jamovi mo\u017cecie odczyta\u0107 ", tags$code("s"),
          " dla ", tags$code("read"),
          " z naszych 420 okr\u0119g\u00f3w \u2014 to b\u0119dzie nasze pilota\u017cowe oszacowanie zmienno\u015bci.
          Reszta to kalkulator i wz\u00f3r z naszej \u015bci\u0105gi:",
          withMathJax("$$n = \\left(\\frac{z^* \\cdot s}{ME_{max}}\\right)^2$$")),
        p("Policzcie odpowied\u017a dla ME = 2. P\u00f3\u017aniej dla ", tags$em("\u017cartu"),
          " spr\u00f3bujcie ME = 1 i ME = 5 \u2014 zobaczycie, sk\u0105d wzi\u0119\u0142a si\u0119 regu\u0142a
          \u201echcesz po\u0142ow\u0119 b\u0142\u0119du, b\u0119dziesz mia\u0142 cztery razy wi\u0119cej roboty\u201d.")
      ),
      actionButton("ch7_ans8", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol8")
    ),

    # --- Zadanie 9 ---
    div(class = "widget-block",
      h4("Zadanie 9 \u2014 Prawda czy fa\u0142sz?"),
      div(class = "narrative",
        p("Przyjmijmy, \u017ce w zadaniu 1 dosta\u0142e\u015b 95% CI dla \u015bredniego ",
          tags$code("read"), " r\u00f3wny ", tags$b("[653.0, 656.9]"),
          ". Oce\u0144 ka\u017cde stwierdzenie:"),
        tags$ol(
          tags$li(tags$b("a)"), " \u201eZ prawdopodobie\u0144stwem 95% prawdziwa \u015brednia \u03bc le\u017cy mi\u0119dzy 653.0 a 656.9.\u201d"),
          tags$li(tags$b("b)"), " \u201e95% wszystkich okr\u0119g\u00f3w ma wynik z czytania mi\u0119dzy 653.0 a 656.9.\u201d"),
          tags$li(tags$b("c)"), " \u201eGdyby\u015bmy powt\u00f3rzyli to badanie wielokrotnie, oko\u0142o 95% tak skonstruowanych przedzia\u0142\u00f3w zawiera\u0142oby prawdziw\u0105 \u015bredni\u0105 \u03bc.\u201d"),
          tags$li(tags$b("d)"), " \u201e\u015arednia z naszej pr\u00f3by le\u017cy w przedziale [653.0, 656.9].\u201d"),
          tags$li(tags$b("e)"), " \u201eMamy 95% ufno\u015bci w metod\u0119, kt\u00f3ra wyprodukowa\u0142a ten przedzia\u0142.\u201d"),
          tags$li(tags$b("f)"), " \u201eGdyby\u015bmy podnie\u015bli poziom ufno\u015bci do 99%, przedzia\u0142 zw\u0119zi\u0142by si\u0119.\u201d")
        )
      ),
      actionButton("ch7_ans9", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol9")
    ),

    # --- Zadanie 10 ---
    div(class = "widget-block",
      h4("Zadanie 10 \u2014 Czy doch\u00f3d w okr\u0119gu ma znaczenie? (trudniejsze)"),
      div(class = "narrative",
        p("Stare pytanie socjologii edukacji: czy dzieci z bogatszych okr\u0119g\u00f3w ucz\u0105 si\u0119 lepiej?
          Mamy wszystko, czego trzeba: ", tags$code("read"),
          " i ", tags$code("income"), "."),
        p("Podzielcie okr\u0119gi na dwie po\u0142owy wzgl\u0119dem ", tags$em("mediany dochodu"),
          " i policzcie 95% CI dla r\u00f3\u017cnicy \u015brednich wynik\u00f3w z czytania mi\u0119dzy tymi grupami
          (Independent Samples T-Test w Jamovi pokazuje taki przedzia\u0142, je\u015bli zaznaczycie ",
          tags$em("Mean difference"), " i ", tags$em("Confidence interval"), ")."),
        p("Klucz interpretacyjny brzmi: ", tags$b("czy zero mie\u015bci si\u0119 w tym przedziale?"),
          " Je\u015bli tak \u2014 nie mamy przes\u0142anek do twierdzenia, \u017ce r\u00f3\u017cnica istnieje.
          Je\u015bli nie \u2014 mamy. Co m\u00f3wi\u0105 dane CASchools?"),
        p(tags$em("Pytanie pu\u0142apkowe na koniec:"),
          " nawet je\u015bli efekt jest wyra\u017any, czy mo\u017cemy z tego wywnioskowa\u0107,
          \u017ce ", tags$em("doch\u00f3d powoduje"),
          " lepsze wyniki w czytaniu? Co jeszcze mog\u0142oby dzia\u0142a\u0107 w tle?")
      ),
      actionButton("ch7_ans10", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol10")
    ),

    # ======================================================================
    # PODSUMOWANIE
    # ======================================================================

    div(class = "section-title", "Podsumowanie"),

    div(class = "callout-warning",
      p(tags$b("Po zako\u0144czeniu \u0107wicze\u0144 odpowiedz na pytania:")),
      tags$ol(
        tags$li("Co najsilniej wp\u0142yn\u0119\u0142o na szeroko\u015b\u0107 CI w Twoich zadaniach \u2014
          liczebno\u015b\u0107 ", tags$em("n"), ", odchylenie ", tags$em("s"),
          ", czy poziom ufno\u015bci?"),
        tags$li("Dlaczego CI dla proporcji w ma\u0142ej pr\u00f3bie (zadanie 7) by\u0142 a\u017c tak szeroki?
          Czy CI Walda da\u0142by si\u0119 tu w og\u00f3le sensownie zastosowa\u0107?"),
        tags$li("Dlaczego stwierdzenie \u201e95% danych le\u017cy w przedziale ufno\u015bci\u201d jest fa\u0142szywe,
          mimo \u017ce brzmi tak naturalnie?")
      )
    ),

    actionButton("ch7_ans_summary", "Poka\u017c odpowiedzi", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol_summary"),

    br(), br(), br()

  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch7_server <- function(input, output, session) {

  # Helper: toggle visibility of solution on button click
  .solution_toggle <- function(btn_id, output_id, content_ui) {
    vis <- reactiveVal(FALSE)
    observeEvent(input[[btn_id]], {
      vis(!vis())
      updateActionButton(session, btn_id,
        label = if (vis()) "Ukryj rozwi\u0105zanie" else "Poka\u017c rozwi\u0105zanie")
    })
    output[[output_id]] <- renderUI({
      if (vis()) div(class = "callout-success", style = "margin-top: 10px;", content_ui)
    })
  }

  # --- Zadanie 1 ---
  .solution_toggle("ch7_ans1", "ch7_sol1", withMathJax(tagList(
    p(tags$b("Wyniki z Jamovi dla zmiennej "), tags$code("read"), ":"),
    tags$ul(
      tags$li("n = 420"),
      tags$li("\u015arednia \u2248 ", tags$b("654.97")),
      tags$li("Odchylenie std s \u2248 ", tags$b("20.11")),
      tags$li("95% CI: ", tags$b("[653.04, 656.90]")),
      tags$li("Margines b\u0142\u0119du ME \u2248 ", tags$b("1.93"))
    ),
    p(tags$b("Sprawdzenie r\u0119czne:"),
      " \\(t^*_{0.975, 419} \\approx 1.966\\), \\(SE = s/\\sqrt{n} = 20.11/\\sqrt{420} \\approx 0.981\\),
      \\(ME = 1.966 \\cdot 0.981 \\approx 1.93\\). Zgadza si\u0119."),
    p(tags$b("Interpretacja:"), " mamy 95% ufno\u015bci w metod\u0119, kt\u00f3ra wyprodukowa\u0142a ten przedzia\u0142 \u2014
      \u015brednia populacji wynik\u00f3w z czytania prawdopodobnie le\u017cy w okolicach 654-657 punkt\u00f3w.")
  )))

  # --- Zadanie 2 ---
  .solution_toggle("ch7_ans2", "ch7_sol2", withMathJax(tagList(
    p(tags$b("Wyniki z Jamovi dla zmiennej "), tags$code("math"), ":"),
    tags$ul(
      tags$li("n = 420 (to samo)"),
      tags$li("\u015arednia \u2248 ", tags$b("653.34")),
      tags$li("s \u2248 ", tags$b("18.75")),
      tags$li("95% CI: ", tags$b("[651.54, 655.14]")),
      tags$li("ME \u2248 ", tags$b("1.80"))
    ),
    p(tags$b("Por\u00f3wnanie:")),
    tags$ul(
      tags$li("Szeroko\u015b\u0107 CI(read) \u2248 ", tags$b("3.86"), " punktu"),
      tags$li("Szeroko\u015b\u0107 CI(math) \u2248 ", tags$b("3.60"), " punktu"),
      tags$li("CI dla math jest ", tags$b("nieznacznie w\u0119\u017cszy"), ".")
    ),
    p(tags$b("Dlaczego?"), " n jest takie samo (420), wi\u0119c r\u00f3\u017cnica wynika wy\u0142\u0105cznie z ",
      tags$em("zmienno\u015bci"),
      ": s(math) = 18.75 < s(read) = 20.11. Mniejsza zmienno\u015b\u0107 daje mniejsze SE i ciasniejszy CI.")
  )))

  # --- Zadanie 3 ---
  .solution_toggle("ch7_ans3", "ch7_sol3", withMathJax(tagList(
    p(tags$b("Wyniki dla pierwszych 30 okr\u0119g\u00f3w "), "(", tags$code("read"), "):"),
    tags$ul(
      tags$li("n = 30"),
      tags$li("\u015arednia \u2248 ", tags$b("622.43"),
        " (uwaga: bardzo r\u00f3\u017cna od \u015bredniej z ca\u0142o\u015bci \u2014 pierwsze 30 okr\u0119g\u00f3w to nie pr\u00f3ba losowa!)"),
      tags$li("s \u2248 ", tags$b("18.31")),
      tags$li("\\(t^*_{0.975, 29} \\approx 2.045\\)"),
      tags$li("\\(SE = 18.31/\\sqrt{30} \\approx 3.34\\)"),
      tags$li("\\(ME \\approx 2.045 \\cdot 3.34 \\approx 6.84\\)"),
      tags$li("95% CI: ", tags$b("[615.60, 629.27]"))
    ),
    p(tags$b("Por\u00f3wnanie ME:"), " ME (n=30) \u2248 6.84 vs ME (n=420) \u2248 1.93 \u2014 ",
      tags$b("3.5\u00d7 szerszy CI"), "."),
    p(tags$b("Trzy przyczyny rozszerzenia CI w por\u00f3wnaniu z zadaniem 1:")),
    tags$ol(
      tags$li(tags$b("Mniejsze n"), " \u2192 \\(\\sqrt{n}\\) w mianowniku jest \\(\\sqrt{14}\\)\u00d7 mniejszy
        \u2192 SE wzros\u0142o ok. 3.74\u00d7."),
      tags$li(tags$b("Wi\u0119kszy mno\u017cnik t*"), " \u2014 dla df = 29 wynosi 2.045 zamiast 1.966 (df = 419).
        Nadmiar +4%."),
      tags$li(tags$b("Inne s"), " \u2014 w pr\u00f3bie 30 okr\u0119g\u00f3w odchylenie wysz\u0142o nieco mniejsze (18.31 vs 20.11),
        co cz\u0119\u015bciowo \u0142agodzi efekt, ale zostaje zdominowane przez \\(\\sqrt{n}\\).")
    ),
    p("Wniosek: ", tags$b("\\(SE \\propto 1/\\sqrt{n}\\)"),
      " \u2014 \u017ceby zmniejszy\u0107 CI o po\u0142ow\u0119 potrzeba 4\u00d7 wi\u0119cej danych.")
  )))

  # --- Zadanie 4 ---
  .solution_toggle("ch7_ans4", "ch7_sol4", withMathJax(tagList(
    p(tags$b("Wyniki dla "), tags$code("read"), tags$b(" przy r\u00f3\u017cnych poziomach ufno\u015bci"),
      " (n=420):"),
    tags$table(class = "table table-bordered table-striped",
      tags$thead(tags$tr(
        tags$th("Poziom"), tags$th("Dolne"), tags$th("G\u00f3rne"), tags$th("ME")
      )),
      tags$tbody(
        tags$tr(tags$td("90%"), tags$td("653.35"), tags$td("656.59"), tags$td("1.62")),
        tags$tr(tags$td("95%"), tags$td("653.04"), tags$td("656.90"), tags$td("1.93")),
        tags$tr(tags$td("99%"), tags$td("652.43"), tags$td("657.51"), tags$td("2.54"))
      )
    ),
    p(tags$b("ME ro\u015bnie monotonicznie."),
      " ME(99%)/ME(90%) \u2248 2.54/1.62 \u2248 ", tags$b("1.57"),
      " \u2014 zwi\u0119kszenie poziomu ufno\u015bci o 9 pp daje ~57% szerszy CI."),
    p(tags$b("Bilans:"),
      " wy\u017cszy poziom ufno\u015bci \u2192 mniejsze ryzyko, \u017ce CI nie zawiera \u03bc,
      ale ", tags$em("mniejsza precyzja"),
      ". W praktyce 95% to standardowy kompromis.")
  )))

  # --- Zadanie 5 ---
  .solution_toggle("ch7_ans5", "ch7_sol5", withMathJax(tagList(
    p(tags$b("Wyniki dla "), tags$code("STR_high = students/teachers > 20"), ":"),
    tags$ul(
      tags$li("Sukces\u00f3w (TRUE): ", tags$b("177")),
      tags$li("n = 420"),
      tags$li("Proporcja \u2248 ", tags$b("0.4214"), " (42.1%)"),
      tags$li("95% CI (Clopper-Pearson, domy\u015blny w Jamovi): ",
        tags$b("[0.374, 0.470]"))
    ),
    p(tags$b("Warunki sensowno\u015bci:")),
    tags$ul(
      tags$li("\\(np = 420 \\cdot 0.421 \\approx 177 \\geq 10\\) \u2713"),
      tags$li("\\(n(1-p) = 420 \\cdot 0.579 \\approx 243 \\geq 10\\) \u2713")
    ),
    p(tags$b("Interpretacja:"),
      " w hipotetycznej populacji podobnych okr\u0119g\u00f3w \u015brednio mi\u0119dzy 37% a 47% mia\u0142oby ",
      tags$em("STR > 20"),
      ". To do\u015b\u0107 \u015bcis\u0142e oszacowanie \u2014 problem przepe\u0142nionych klas dotyczy istotnej cz\u0119\u015bci okr\u0119g\u00f3w.")
  )))

  # --- Zadanie 6 ---
  .solution_toggle("ch7_ans6", "ch7_sol6", withMathJax(tagList(
    p(tags$b("Wyniki dla "), tags$code("english_high = english > 20"), ":"),
    tags$ul(
      tags$li("Sukces\u00f3w: ", tags$b("118")),
      tags$li("n = 420"),
      tags$li("Proporcja \u2248 ", tags$b("0.281"), " (28.1%)"),
      tags$li("95% CI (Clopper-Pearson): ", tags$b("[0.239, 0.327]"))
    ),
    p(tags$b("Warunki sensowno\u015bci:")),
    tags$ul(
      tags$li("\\(np = 118 \\geq 10\\) \u2713"),
      tags$li("\\(n(1-p) = 302 \\geq 10\\) \u2713")
    ),
    p(tags$b("Por\u00f3wnanie z zadaniem 5:"),
      " szeroko\u015b\u0107 CI z zadania 5 \u2248 0.097, tu \u2248 0.088 \u2014 podobne, choc tu CI jest ",
      tags$em("nieco w\u0119\u017cszy"), ". Powod: ",
      tags$b("im dalej p od 0.5, tym mniejsza wariancja "),
      "\\(p(1-p)\\). Dla p \u2248 0.28 mamy \\(p(1-p) \\approx 0.20\\), dla p \u2248 0.42 mamy \\(p(1-p) \\approx 0.244\\).")
  )))

  # --- Zadanie 7 ---
  .solution_toggle("ch7_ans7", "ch7_sol7", withMathJax(tagList(
    p(tags$b("Pierwsze 25 okr\u0119g\u00f3w, zmienna "), tags$code("english_high"), ":"),
    tags$ul(
      tags$li("Sukces\u00f3w: ", tags$b("19"), " (pierwsze 25 okr\u0119g\u00f3w to akurat region z wysokim odsetkiem English learners)"),
      tags$li("n = 25"),
      tags$li("Proporcja \u2248 ", tags$b("0.76")),
      tags$li("95% CI Clopper-Pearson: ", tags$b("[0.549, 0.906]"))
    ),
    p(tags$b("Por\u00f3wnanie z zadaniem 6:"),
      " CI z ca\u0142ego zbioru [0.239, 0.327] ma szeroko\u015b\u0107 ~0.09;
      CI z 25 obserwacji [0.549, 0.906] ma szeroko\u015b\u0107 ~", tags$b("0.36"),
      " \u2014 cztery razy szerszy."),
    p("Dwa efekty na\u0142o\u017cy\u0142y si\u0119: po pierwsze, ma\u0142e n,
      a po drugie pierwsze 25 okr\u0119g\u00f3w to nie pr\u00f3ba losowa \u2014 to pr\u00f3ba ",
      tags$em("obci\u0105\u017cona"), " (inne hrabstwa, inne realia). Dlatego ",
      tags$em("p\u0302 = 0.76"), " r\u00f3\u017cni si\u0119 dramatycznie od populacyjnego ~0.28."),
    p(tags$b("Dlaczego Clopper-Pearson, a nie Wald?")),
    tags$ul(
      tags$li("Wald zak\u0142ada przybli\u017cenie normalne; przy n=25 i p blisko 1 jest niedok\u0142adny."),
      tags$li("Wald m\u00f3g\u0142by da\u0107 g\u00f3rne ograniczenie ", tags$em(">1"),
        " (czego nie da Clopper-Pearson)."),
      tags$li("Clopper-Pearson gwarantuje pokrycie \u2265 95% \u2014 jest ",
        tags$em("konserwatywny, ale bezpieczny"), " dla ma\u0142ych pr\u00f3b.")
    )
  )))

  # --- Zadanie 8 ---
  .solution_toggle("ch7_ans8", "ch7_sol8", withMathJax(tagList(
    p(tags$b("Z Descriptives:"), " s(read) \u2248 ", tags$b("20.11"), "."),
    p(tags$b("Wz\u00f3r:"), " \\(n = (z^* \\cdot s / ME)^2 = (1.96 \\cdot 20.11 / 2)^2\\)."),
    p(tags$b("Obliczenia:")),
    tags$table(class = "table table-bordered",
      tags$thead(tags$tr(
        tags$th("ME"), tags$th("Wz\u00f3r"), tags$th("n (zaokr\u0105glone w g\u00f3r\u0119)")
      )),
      tags$tbody(
        tags$tr(tags$td("2.0"), tags$td("\\((1.96 \\cdot 20.11 / 2)^2\\)"), tags$td("389")),
        tags$tr(tags$td("1.0"), tags$td("\\((1.96 \\cdot 20.11 / 1)^2\\)"), tags$td("1554")),
        tags$tr(tags$td("5.0"), tags$td("\\((1.96 \\cdot 20.11 / 5)^2\\)"), tags$td("63"))
      )
    ),
    p(tags$b("Interpretacja:"),
      " mamy 420 okr\u0119g\u00f3w, wi\u0119c dla ME=2 jeste\u015bmy ", tags$em("ju\u017c"),
      " powy\u017cej minimum (389). Dla ME=1 trzeba by ponad 1500 okr\u0119g\u00f3w \u2014 niemal 4\u00d7 wi\u0119cej (zgodnie z regu\u0142\u0105 \u201ezmniejszenie ME o po\u0142ow\u0119 \u2192 4\u00d7 wi\u0119cej n\u201d)."),
    p(tags$em("Uwaga praktyczna:"),
      " formu\u0142a u\u017cywa z*, nie t*, wi\u0119c jest poprawnym przybli\u017ceniem dla du\u017cych n.
      W praktyce planowania zwykle nie znamy s i u\u017cywamy oszacowania pilota\u017cowego.")
  )))

  # --- Zadanie 9 ---
  .solution_toggle("ch7_ans9", "ch7_sol9", withMathJax(tagList(
    p(tags$b("Ocena zda\u0144:")),
    tags$ul(
      tags$li(tags$b("a) FA\u0141SZ."),
        " \u03bc jest sta\u0142e (cho\u0107 nieznane). To metoda ma 95% szans wyprodukowa\u0107 CI zawieraj\u0105cy \u03bc, a nie \u03bc \u201ema 95% szans by\u0107 w przedziale\u201d."),
      tags$li(tags$b("b) FA\u0141SZ."),
        " To by\u0142by ", tags$em("prediction interval"),
        " (90 punkt\u00f3w szeroki!), nie CI dla \u015bredniej. CI m\u00f3wi o ",
        tags$em("\u015bredniej"), ", nie o pojedynczych obserwacjach."),
      tags$li(tags$b("c) PRAWDA."),
        " To poprawna interpretacja cz\u0119stotliwo\u015bciowa CI."),
      tags$li(tags$b("d) PRAWDA, ale trywialna."),
        " \u015arednia z pr\u00f3by zawsze le\u017cy w \u015brodku CI z definicji \u2014 to nic nie m\u00f3wi o populacji."),
      tags$li(tags$b("e) PRAWDA."),
        " Standardowe sformu\u0142owanie po polsku oddaj\u0105ce sens cz\u0119stotliwo\u015bciowej ufno\u015bci."),
      tags$li(tags$b("f) FA\u0141SZ."),
        " Wy\u017cszy poziom ufno\u015bci \u2192 ", tags$em("szerszy"), " CI, nie w\u0119\u017cszy.")
    )
  )))

  # --- Zadanie 10 ---
  .solution_toggle("ch7_ans10", "ch7_sol10", withMathJax(tagList(
    p(tags$b("Mediana "), tags$code("income"),
      ": dzieli zbi\u00f3r na 210 okr\u0119g\u00f3w \u201elow\u201d i 210 \u201ehigh\u201d."),
    p(tags$b("\u015arednie z czytania:")),
    tags$ul(
      tags$li("low: \u2248 ", tags$b("644.18")),
      tags$li("high: \u2248 ", tags$b("665.77")),
      tags$li("R\u00f3\u017cnica (high - low): \u2248 ", tags$b("21.59"), " punktu")
    ),
    p(tags$b("95% CI dla r\u00f3\u017cnicy \u015brednich (Welch):"),
      tags$b("[18.33, 24.85]")),
    p(tags$b("Czy zero le\u017cy w przedziale?"),
      " ", tags$b("NIE"), " \u2014 ca\u0142y CI jest dodatni i zdecydowanie odsuni\u0119ty od zera."),
    p(tags$b("Interpretacja:"),
      " mamy bardzo silne wskazanie, \u017ce okr\u0119gi z wy\u017cszym dochodem maj\u0105 wy\u017csze \u015brednie wyniki z czytania
      \u2014 r\u00f3\u017cnica wynosi prawdopodobnie mi\u0119dzy 18 a 25 punkt\u00f3w. To kwota du\u017ca w skali tych test\u00f3w."),
    p(tags$em("Uwaga:"),
      " CI dla r\u00f3\u017cnicy \u015brednich pe\u0142ni rol\u0119 testu \u2014 jest to bezpo\u015brednie pytanie ",
      tags$em("\u201eczy efekt jest sp\u00f3jny ze zerem?\u201d"),
      ". Tu odpowied\u017a brzmi: zdecydowanie nie. Oczywi\u015bcie z korelacji nie wynika przyczynowo\u015b\u0107 \u2014
      doch\u00f3d wsp\u00f3\u0142wyst\u0119puje z wieloma innymi czynnikami.")
  )))

  # --- Podsumowanie ---
  .solution_toggle("ch7_ans_summary", "ch7_sol_summary", tagList(
    tags$ol(
      tags$li(tags$b("Co wp\u0142ywa na szeroko\u015b\u0107 CI?"),
        " Najsilniejszy efekt ma ", tags$em("liczebno\u015b\u0107 n"),
        " (przez \\(\\sqrt{n}\\) \u2014 patrz zadanie 3).
        Drugi to ", tags$em("zmienno\u015b\u0107 s"),
        " (zadanie 2). Poziom ufno\u015bci dzia\u0142a, ale s\u0142abiej (zadanie 4)."),
      tags$li(tags$b("Dlaczego CI w zadaniu 7 by\u0142 a\u017c tak szeroki?"),
        " n=25 to ekstremalnie ma\u0142a pr\u00f3ba dla proporcji.
        Wald zak\u0142ada normalne przybli\u017cenie i przy p=0.76 dawa\u0142by absurdy (g\u00f3rne ograniczenie blisko 1).
        Clopper-Pearson jest dok\u0142adny binomialnie, ale szeroki \u2014 to ", tags$em("uczciwa cena"),
        " braku danych."),
      tags$li(tags$b("Dlaczego \u201e95% danych le\u017cy w przedziale\u201d to fa\u0142sz?"),
        " CI dla \u015bredniej szacuje ", tags$em("po\u0142o\u017cenie \u015bredniej populacji"),
        ", a nie zakres pojedynczych obserwacji.
        Dla read CI jest \u2248 4 punkty szeroki, a dane rozci\u0105gaj\u0105 si\u0119 na ~80 punkt\u00f3w!
        \u017beby opisa\u0107 zakres danych potrzebujemy ", tags$em("prediction interval"),
        " lub po prostu \u015brednia \u00b1 2s.")
    )
  ))
}
