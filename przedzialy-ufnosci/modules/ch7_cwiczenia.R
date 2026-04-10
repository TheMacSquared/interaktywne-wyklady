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
      h4("Zadanie 3 \u2014 Co je\u015bli mieliby\u015bmy tylko mniejsz\u0105 pr\u00f3b\u0119?"),
      div(class = "narrative",
        p("Wyobra\u017a sobie inn\u0105 sytuacj\u0119: kurator chce spojrze\u0107 osobno na szko\u0142y
          z klasami K\u20136 (kindergarten do 6. klasy). Przefiltruj zbi\u00f3r po ",
          tags$code("grades == \"KK-06\""), "."),
        p("Zr\u00f3b CI dla ", tags$code("read"),
          " na tej podgrupie i por\u00f3wnaj go z poprzednim. Zwr\u00f3\u0107 uwag\u0119,
          \u017ce zmieni\u0142y si\u0119 ", tags$em("trzy"),
          " rzeczy naraz \u2014 i ka\u017cda z nich pcha CI w t\u0119 sam\u0105 stron\u0119.
          Spr\u00f3bujcie wskaza\u0107 wszystkie trzy zanim zajrzycie do rozwi\u0105zania."),
        p(tags$em("Bonus do dyskusji:"),
          " czy \u015brednia ze szk\u00f3\u0142 KK-06 jest wiarygodna jako oszacowanie ",
          tags$em("wszystkich"), " szk\u00f3\u0142 w Kalifornii?
          Szko\u0142y K\u20136 to inny typ plac\u00f3wek ni\u017c K\u20138 \u2014 mog\u0105 mie\u0107 systematycznie inne wyniki.")
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

    div(class = "section-title", "Blok 3: Interpretacja i my\u015blenie krytyczne (~25 min)"),

    # --- Zadanie 8 ---
    div(class = "widget-block",
      h4("Zadanie 8 \u2014 Prawda czy fa\u0142sz?"),
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
      actionButton("ch7_ans8", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol8")
    ),

    # --- Zadanie 9 --- (wieloetapowe: lunch \u2192 income \u2192 weryfikacja)
    div(class = "widget-block",
      h4("Zadanie 9 \u2014 Czy dotacje do obiad\u00f3w szkodz\u0105 uczniom? (trudniejsze)"),

      # --- Krok 9a ---
      div(class = "narrative",
        p(tags$b("Krok A."), " Zmienna ", tags$code("lunch"),
          " m\u00f3wi, jaki procent uczni\u00f3w w okr\u0119gu korzysta z dotowanego obiadu
          (program dla rodzin o niskich dochodach). Stw\u00f3rzcie now\u0105 zmienn\u0105 dziel\u0105c\u0105 okr\u0119gi na trzy grupy:"),
        tags$ul(
          tags$li(tags$b("ma\u0142o"), " \u2014 lunch < 33%"),
          tags$li(tags$b("\u015brednio"), " \u2014 lunch 33\u201366%"),
          tags$li(tags$b("du\u017co"), " \u2014 lunch > 66%")
        ),
        p("Policzcie 95% CI dla \u015bredniej ", tags$code("read"), " i ", tags$code("math"),
          " w ka\u017cdej z trzech grup. Co widzicie?")
      ),
      actionButton("ch7_ans9a", "Poka\u017c wyniki kroku A", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol9a"),
      br(),

      # --- Krok 9b --- (ukryty do momentu klikni\u0119cia 9a)
      conditionalPanel(
        condition = "input.ch7_ans9a % 2 == 1",
        div(class = "narrative",
          p(tags$b("Krok B."), " Wyniki s\u0105 dramatyczne \u2014 im wi\u0119kszy odsetek dotowanych obiad\u00f3w,
            tym gorsze wyniki w nauce. Kto\u015b pochopny m\u00f3g\u0142by zawy\u015bredniorokowa\u0107:
            \u201edotacje do obiad\u00f3w obni\u017caj\u0105 wyniki!\u201d"),
          p("Ale zanim wyci\u0105gniemy wnioski, sprawd\u017amy jeszcze jedn\u0105 rzecz.
            Podzielcie teraz okr\u0119gi wg zmiennej ", tags$code("income"),
            " (tysi\u0105ce dolar\u00f3w) na trzy grupy:"),
          tags$ul(
            tags$li(tags$b("niski"), " \u2014 income < 10"),
            tags$li(tags$b("\u015bredni"), " \u2014 income 10\u201320"),
            tags$li(tags$b("wysoki"), " \u2014 income > 20")
          ),
          p("Policzcie 95% CI dla \u015bredniej ", tags$code("read"), " i ", tags$code("math"),
            " w ka\u017cdej grupie dochodowej.")
        ),
        actionButton("ch7_ans9b", "Poka\u017c wyniki kroku B", class = "btn-outline-success btn-sm"),
        uiOutput("ch7_sol9b"),
        br(),

        # --- Krok 9c --- (ukryty do momentu klikni\u0119cia 9b)
        conditionalPanel(
          condition = "input.ch7_ans9b % 2 == 1",
          div(class = "narrative",
            p(tags$b("Krok C."), " Wi\u0119c bogatsze okr\u0119gi maj\u0105 lepsze wyniki \u2014 a przecie\u017c
              bogatsze okr\u0119gi to te, w kt\u00f3rych ", tags$em("mniej"),
              " rodzin potrzebuje dotacji do obiad\u00f3w.
              Mo\u017ce to nie dotacje \u201eszkodz\u0105\u201d, tylko bieda jest prawdziwym czynnikiem?"),
            p("Sprawd\u017amy to wprost: policzcie 95% CI dla \u015bredniej ", tags$code("income"),
              " w ka\u017cdej z trzech grup ", tags$code("lunch"),
              " (ma\u0142o / \u015brednio / du\u017co). Je\u015bli przedzia\u0142y nie nachodz\u0105 na siebie,
              to grupy dotacji to tak naprawd\u0119 grupy zamo\u017cno\u015bci.")
          ),
          actionButton("ch7_ans9c", "Poka\u017c wyniki kroku C i wnioski", class = "btn-outline-success btn-sm"),
          uiOutput("ch7_sol9c")
        )
      )
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
        tags$li("Czego nauczy\u0142o nas zadanie 9 o interpretacji zwi\u0105zk\u00f3w mi\u0119dzy zmiennymi?
          Dlaczego nie mo\u017cna od razu wnioskowa\u0107 o przyczynowo\u015bci?")
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
    p(tags$b("Wyniki dla szk\u00f3\u0142 KK-06 "), "(", tags$code("read"), "):"),
    tags$ul(
      tags$li("n = 61"),
      tags$li("\u015arednia \u2248 ", tags$b("662.08"),
        " (uwaga: wy\u017csza ni\u017c \u015brednia z ca\u0142o\u015bci (655.0) \u2014 szko\u0142y K\u20136 to specyficzna podgrupa!)"),
      tags$li("s \u2248 ", tags$b("20.51")),
      tags$li("\\(t^*_{0.975, 60} \\approx 2.000\\)"),
      tags$li("\\(SE = 20.51/\\sqrt{61} \\approx 2.63\\)"),
      tags$li("\\(ME \\approx 2.000 \\cdot 2.63 \\approx 5.25\\)"),
      tags$li("95% CI: ", tags$b("[656.82, 667.33]"))
    ),
    p(tags$b("Por\u00f3wnanie ME:"), " ME (n=61) \u2248 5.25 vs ME (n=420) \u2248 1.93 \u2014 ",
      tags$b("2.7\u00d7 szerszy CI"), "."),
    p(tags$b("Trzy przyczyny rozszerzenia CI w por\u00f3wnaniu z zadaniem 1:")),
    tags$ol(
      tags$li(tags$b("Mniejsze n"), " \u2192 \\(\\sqrt{n}\\) w mianowniku jest \\(\\sqrt{420}/\\sqrt{61} \\approx 2.62\\)\u00d7 mniejszy
        \u2192 SE wzros\u0142o ok. 2.7\u00d7."),
      tags$li(tags$b("Wi\u0119kszy mno\u017cnik t*"), " \u2014 dla df = 60 wynosi 2.000 zamiast 1.966 (df = 419).
        Nadmiar +1.7%."),
      tags$li(tags$b("Inne s"), " \u2014 w podgrupie KK-06 odchylenie wysz\u0142o nieco wi\u0119ksze (20.51 vs 20.11),
        co lekko powi\u0119ksza efekt, ale g\u0142\u00f3wnym czynnikiem pozostaje \\(\\sqrt{n}\\).")
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

  # --- Zadanie 9a --- (lunch vs wyniki)
  .solution_toggle("ch7_ans9a", "ch7_sol9a", tagList(
    p(tags$b("95% CI dla \u015bredniej read wg grup lunch:")),
    tags$table(class = "table table-bordered table-striped",
      tags$thead(tags$tr(
        tags$th("Grupa lunch"), tags$th("n"), tags$th("\u015arednia read"), tags$th("95% CI")
      )),
      tags$tbody(
        tags$tr(tags$td("ma\u0142o (< 33%)"), tags$td("168"), tags$td("671.8"), tags$td("[669.8, 673.8]")),
        tags$tr(tags$td("\u015brednio (33\u201366%)"), tags$td("142"), tags$td("653.1"), tags$td("[651.3, 654.9]")),
        tags$tr(tags$td("du\u017co (> 66%)"), tags$td("110"), tags$td("631.6"), tags$td("[629.3, 634.0]"))
      )
    ),
    p(tags$b("95% CI dla \u015bredniej math wg grup lunch:")),
    tags$table(class = "table table-bordered table-striped",
      tags$thead(tags$tr(
        tags$th("Grupa lunch"), tags$th("n"), tags$th("\u015arednia math"), tags$th("95% CI")
      )),
      tags$tbody(
        tags$tr(tags$td("ma\u0142o (< 33%)"), tags$td("168"), tags$td("668.6"), tags$td("[666.5, 670.8]")),
        tags$tr(tags$td("\u015brednio (33\u201366%)"), tags$td("142"), tags$td("650.2"), tags$td("[648.2, 652.1]")),
        tags$tr(tags$td("du\u017co (> 66%)"), tags$td("110"), tags$td("634.1"), tags$td("[632.0, 636.3]"))
      )
    ),
    p(tags$b("Obserwacja:"), " przedzia\u0142y ", tags$em("nie nachodz\u0105 na siebie"),
      " \u2014 r\u00f3\u017cnice s\u0105 ogromne. Im wi\u0119kszy odsetek dotowanych obiad\u00f3w,
      tym ", tags$b("ni\u017csze"), " wyniki z czytania i matematyki.
      R\u00f3\u017cnica mi\u0119dzy skrajnymi grupami to ok. 40 punkt\u00f3w!"),
    p(tags$em("Ale zanim wyci\u0105gniesz wnioski\u2026 przejd\u017a do kroku B."))
  ))

  # --- Zadanie 9b --- (income vs wyniki)
  .solution_toggle("ch7_ans9b", "ch7_sol9b", tagList(
    p(tags$b("95% CI dla \u015bredniej read wg grup income:")),
    tags$table(class = "table table-bordered table-striped",
      tags$thead(tags$tr(
        tags$th("Grupa income"), tags$th("n"), tags$th("\u015arednia read"), tags$th("95% CI")
      )),
      tags$tbody(
        tags$tr(tags$td("niski (< 10 tys.)"), tags$td("73"), tags$td("633.9"), tags$td("[630.1, 637.7]")),
        tags$tr(tags$td("\u015bredni (10\u201320 tys.)"), tags$td("280"), tags$td("654.7"), tags$td("[652.9, 656.5]")),
        tags$tr(tags$td("wysoki (> 20 tys.)"), tags$td("67"), tags$td("679.1"), tags$td("[675.3, 682.8]"))
      )
    ),
    p(tags$b("95% CI dla \u015bredniej math wg grup income:")),
    tags$table(class = "table table-bordered table-striped",
      tags$thead(tags$tr(
        tags$th("Grupa income"), tags$th("n"), tags$th("\u015brednia math"), tags$th("95% CI")
      )),
      tags$tbody(
        tags$tr(tags$td("niski (< 10 tys.)"), tags$td("73"), tags$td("636.3"), tags$td("[632.7, 640.0]")),
        tags$tr(tags$td("\u015bredni (10\u201320 tys.)"), tags$td("280"), tags$td("652.2"), tags$td("[650.6, 653.8]")),
        tags$tr(tags$td("wysoki (> 20 tys.)"), tags$td("67"), tags$td("676.7"), tags$td("[672.7, 680.6]"))
      )
    ),
    p(tags$b("Obserwacja:"), " ten sam wz\u00f3r! Bogatsze okr\u0119gi maj\u0105 wyra\u017anie lepsze wyniki.
      R\u00f3\u017cnica mi\u0119dzy skrajnymi grupami to ok. 40\u201345 punkt\u00f3w."),
    p(tags$em("Hmm\u2026 mo\u017ce to, co widzieli\u015bmy w kroku A, nie ma nic wsp\u00f3lnego z obiadami?
      Przejd\u017a do kroku C."))
  ))

  # --- Zadanie 9c --- (income w grupach lunch \u2014 weryfikacja)
  .solution_toggle("ch7_ans9c", "ch7_sol9c", tagList(
    p(tags$b("95% CI dla \u015bredniej income wg grup lunch:")),
    tags$table(class = "table table-bordered table-striped",
      tags$thead(tags$tr(
        tags$th("Grupa lunch"), tags$th("n"), tags$th("\u015aredni income (tys. $)"), tags$th("95% CI")
      )),
      tags$tbody(
        tags$tr(tags$td("ma\u0142o (< 33%)"), tags$td("168"), tags$td("20.33"), tags$td("[19.04, 21.63]")),
        tags$tr(tags$td("\u015brednio (33\u201366%)"), tags$td("142"), tags$td("13.17"), tags$td("[12.63, 13.71]")),
        tags$tr(tags$td("du\u017co (> 66%)"), tags$td("110"), tags$td("10.43"), tags$td("[9.91, 10.95]"))
      )
    ),
    p(tags$b("Przedzia\u0142y nie nachodz\u0105 na siebie"), " \u2014 grupy wydzielone wg odsetka dotacji
      to w praktyce ", tags$b("grupy zamo\u017cno\u015bci"),
      ". Okr\u0119gi z du\u017cym odsetkiem dotacji to okr\u0119gi biedne (\u015br. doch\u00f3d ~10 tys. $),
      a te z ma\u0142ym odsetkiem to okr\u0119gi zamożne (~20 tys. $)."),
    div(class = "callout-warning",
      p(tags$b("Wniosek:"), " to, co w kroku A wygl\u0105da\u0142o jak \u201edotacje szkodz\u0105\u201d,
        to w rzeczywisto\u015bci efekt ubóstwa. Programy dotacji do obiad\u00f3w nie ",
        tags$em("powoduj\u0105"), " gorszych wynik\u00f3w \u2014 wyst\u0119puj\u0105 tam, gdzie rodziny s\u0105 biedniejsze,
        a bieda jest zwi\u0105zana z ni\u017cszymi wynikami."),
      p("To klasyczny przyk\u0142ad ", tags$b("zmiennej zak\u0142\u00f3caj\u0105cej (confounding variable)"),
        ". Bez spojrzenia na ", tags$code("income"),
        " mogliby\u015bmy wyci\u0105gn\u0105\u0107 b\u0142\u0119dny wniosek i np. postulowa\u0107 likwidacj\u0119 programu,
        kt\u00f3ry w rzeczywisto\u015bci pomaga potrzebuj\u0105cym."),
      p(tags$em("Morał:"), " przedzia\u0142y ufno\u015bci daj\u0105 precyzj\u0119 oszacowania,
        ale ", tags$b("nie m\u00f3wi\u0105 o przyczynowo\u015bci"),
        ". Do tego potrzeba my\u015blenia o mechanizmach i zmiennych zak\u0142\u00f3caj\u0105cych.")
    )
  ))

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
      tags$li(tags$b("Czego uczy zadanie 9?"),
        " Korelacja nie oznacza przyczynowo\u015bci. Zmienna zak\u0142\u00f3caj\u0105ca (tu: doch\u00f3d) mo\u017ce
        sprawia\u0107, \u017ce dwa zjawiska wygl\u0105daj\u0105 na powi\u0105zane, cho\u0107 jedno nie powoduje drugiego.
        CI m\u00f3wi\u0105 ", tags$em("jak du\u017ca"), " jest r\u00f3\u017cnica, ale nie m\u00f3wi\u0105 ", tags$em("dlaczego"),
        " istnieje.")
    )
  ))
}
