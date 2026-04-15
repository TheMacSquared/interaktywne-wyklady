# ============================================================================
# CHAPTER 7: Cwiczenia praktyczne — przedzialy ufnosci
# Cztery warianty kierunkowe: Edukacja, BHP, Rolnictwo, Technologia Zywnosci
# ============================================================================

# ============================================================================
# UI
# ============================================================================

ch7_ui <- tabPanel("7. \u0106wiczenia",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Czas zastosowa\u0107 wszystko, co poznali\u015bmy o przedzia\u0142ach ufno\u015bci na rzeczywistych danych."
    ),

    div(class = "section-title", "\u0106wiczenia praktyczne \u2014 przedzia\u0142y ufno\u015bci"),

    div(class = "narrative",
      p(tags$b("Czas trwania:"), " ~ 90 minut \u00b7 ",
        tags$b("Narz\u0119dzie:"), " Jamovi"),
      p("Trzy bloki zada\u0144 \u2014 CI dla \u015bredniej, CI dla proporcji, interpretacja i my\u015blenie krytyczne.
        Ka\u017cde zadanie ma ", tags$b("ukryte rozwi\u0105zanie"),
        " \u2014 kliknij przycisk, aby je zobaczy\u0107.")
    ),

    div(class = "callout-info",
      selectInput("ch7_kierunek", tags$b("Wybierz wariant dla kierunku:"),
        choices = list(
          "Edukacja (CASchools)" = "edu",
          "In\u017cynieria Bezpiecze\u0144stwa (BHP)" = "bhp",
          "Rolnictwo" = "rol",
          "Technologia \u017cywno\u015bci" = "zyw"
        ),
        selected = "edu",
        width = "100%"
      )
    ),

    uiOutput("ch7_content"),

    br(), br(), br()
  ))
)

# ============================================================================
# TRESC ZADAN — funkcje zwracajace tagList per kierunek
# ============================================================================

# --------------------------------------------------------------------------
# EDUKACJA (CASchools)
# --------------------------------------------------------------------------

.ch7_content_edu <- function() tagList(

  div(class = "callout-info",
    p(tags$b("Otw\u00f3rz plik "), tags$code("dane/caschools.csv"), tags$b(" w Jamovi"), "."),
    p("Dane ze 420 okr\u0119g\u00f3w szkolnych w Kalifornii. Zmienne: wyniki z czytania (",
      tags$code("read"), ") i matematyki (", tags$code("math"),
      "), stosunek uczni\u00f3w do nauczycieli (",
      tags$code("students/teachers"), "), procent uczni\u00f3w ucz\u0105cych si\u0119 angielskiego (",
      tags$code("english"), "), doch\u00f3d okr\u0119gu (", tags$code("income"), "), dotacje do obiad\u00f3w (",
      tags$code("lunch"), "), typ szko\u0142y (", tags$code("grades"), ").")
  ),

  div(class = "section-title", "Blok 1: Przedzia\u0142 ufno\u015bci dla \u015bredniej (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 1 \u2014 Jak czytaj\u0105 dzieci w Kalifornii?"),
    div(class = "narrative",
      p("Kuratorium o\u015bwiaty pyta: ", tags$em("\u201ejaki jest typowy \u015bredni wynik z czytania w kalifornijskim okr\u0119gu?\u201d"),
        " Wyznacz 95% CI dla \u015bredniej zmiennej ", tags$code("read"),
        ". Zanim klikniesz rozwi\u0105zanie: ile wynosi \u015brednia, granice CI i co powiedzieliby\u015b kuratorium jednym zdaniem?")
    ),
    actionButton("ch7_ans1", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 \u2014 A z matematyk\u0105 lepiej czy gorzej?"),
    div(class = "narrative",
      p("Powt\u00f3rz analiz\u0119 dla zmiennej ", tags$code("math"),
        ". Dlaczego ", tags$b("przedzia\u0142y"), " maj\u0105 r\u00f3\u017cn\u0105 szeroko\u015b\u0107? n jest takie samo, wi\u0119c co decyduje?")
    ),
    actionButton("ch7_ans2", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol2")
  ),

  div(class = "widget-block",
    h4("Zadanie 3 \u2014 Co je\u015bli mieliby\u015bmy tylko mniejsz\u0105 pr\u00f3b\u0119?"),
    div(class = "narrative",
      p("Przefiltruj zbi\u00f3r po ", tags$code("grades == \"KK-06\""), ".
        Zr\u00f3b CI dla ", tags$code("read"), " na tej podgrupie i por\u00f3wnaj z poprzednim.
        Wska\u017c ", tags$em("trzy"), " rzeczy, kt\u00f3re naraz zmieniaj\u0105 CI.")
    ),
    actionButton("ch7_ans3", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 \u2014 Ile ufno\u015bci kupujemy za szeroko\u015b\u0107?"),
    div(class = "narrative",
      p("Wr\u00f3\u0107 do pe\u0142nych 420 okr\u0119g\u00f3w. Policz CI dla ", tags$code("read"),
        " przy poziomach ufno\u015bci: 90%, 95%, 99%. Zapisz marginesy b\u0142\u0119du i por\u00f3wnaj."),
      p(tags$em("Dyskusja:"), " kto \u017c\u0105da\u0142by 99% \u2014 statystyk akademicki czy in\u017cynier od bezpiecze\u0144stwa lot\u00f3w?")
    ),
    actionButton("ch7_ans4", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol4")
  ),

  div(class = "section-title", "Blok 2: Przedzia\u0142 ufno\u015bci dla proporcji (~20 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 \u2014 Przepe\u0142nione klasy"),
    div(class = "narrative",
      p("Przy stosunku students/teachers > 20 trudno o indywidualne podej\u015bcie.
        Stw\u00f3rz zmienn\u0105 binarną i wyznacz 95% CI dla proporcji okr\u0119g\u00f3w z STR > 20.
        Sprawd\u017a warunki sensowno\u015bci przed interpretacj\u0105.")
    ),
    actionButton("ch7_ans5", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 \u2014 Dystrykty z du\u017cym odsetkiem English learners"),
    div(class = "narrative",
      p("Okr\u0119gi z ", tags$code("english > 20%"), " s\u0105 \u201ej\u0119zykowo wymagaj\u0105ce\u201d.
        Wyznacz 95% CI dla tej proporcji i por\u00f3wnaj szeroko\u015b\u0107 z zadaniem 5.
        Dlaczego jeden jest ciasniejszy, skoro n jest takie samo?")
    ),
    actionButton("ch7_ans6", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol6")
  ),

  div(class = "widget-block",
    h4("Zadanie 7 \u2014 Co je\u015bli mamy tylko 25 okr\u0119g\u00f3w?"),
    div(class = "narrative",
      p("Przefiltruj do pierwszych 25 okr\u0119g\u00f3w. Policz CI dla ", tags$code("english > 20"),
        ". Dlaczego CI jest tak szeroki? Czy pierwsze 25 wierszy to losowa pr\u00f3ba?")
    ),
    actionButton("ch7_ans7", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol7")
  ),

  div(class = "section-title", "Blok 3: Interpretacja i my\u015blenie krytyczne (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 8 \u2014 Prawda czy fa\u0142sz?"),
    div(class = "narrative",
      p("Przyjmijmy, \u017ce w zadaniu 1 dosta\u0142e\u015b 95% CI dla \u015bredniej ",
        tags$code("read"), " r\u00f3wny ", tags$b("[653.0, 656.9]"), ". Oce\u0144 ka\u017cde stwierdzenie:"),
      tags$ol(
        tags$li(tags$b("a)"), " \u201eZ prawdopodobie\u0144stwem 95% prawdziwa \u015brednia \u03bc le\u017cy mi\u0119dzy 653.0 a 656.9.\u201d"),
        tags$li(tags$b("b)"), " \u201e95% wszystkich okr\u0119g\u00f3w ma wynik z czytania mi\u0119dzy 653.0 a 656.9.\u201d"),
        tags$li(tags$b("c)"), " \u201eGdyby\u015bmy powtarzali badanie, ~95% tak skonstruowanych przedzia\u0142\u00f3w zawiera\u0142oby prawdziw\u0105 \u015bredni\u0105.\u201d"),
        tags$li(tags$b("d)"), " \u201e\u015arednia z pr\u00f3by le\u017cy w przedziale [653.0, 656.9].\u201d"),
        tags$li(tags$b("e)"), " \u201eMamy 95% ufno\u015bci w metod\u0119, kt\u00f3ra wyprodukowa\u0142a ten przedzia\u0142.\u201d"),
        tags$li(tags$b("f)"), " \u201eGdyby\u015bmy podnie\u015bli poziom ufno\u015bci do 99%, przedzia\u0142 zw\u0119zi\u0142by si\u0119.\u201d")
      )
    ),
    actionButton("ch7_ans8", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol8")
  ),

  div(class = "widget-block",
    h4("Zadanie 9 \u2014 Czy dotacje do obiad\u00f3w szkodz\u0105 uczniom? (trudniejsze)"),

    div(class = "narrative",
      p(tags$b("Krok A."), " Podziel okr\u0119gi wg ", tags$code("lunch"),
        " (dotacje do obiad\u00f3w) na trzy grupy: ma\u0142o (<33%), \u015brednio (33\u201366%), du\u017co (>66%).
        Policz 95% CI dla \u015bredniej ", tags$code("read"), " i ", tags$code("math"), " w ka\u017cdej grupie.")
    ),
    actionButton("ch7_ans9a", "Poka\u017c wyniki kroku A", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol9a"),
    br(),

    conditionalPanel(condition = "input.ch7_ans9a % 2 == 1",
      div(class = "narrative",
        p(tags$b("Krok B."), " Wyniki s\u0105 dramatyczne. Ale zanim wyci\u0105gniesz wnioski:
          podziel okr\u0119gi wg ", tags$code("income"),
          " (niski <10, \u015bredni 10\u201320, wysoki >20 tys. $) i policz CI dla tych samych wynik\u00f3w.")
      ),
      actionButton("ch7_ans9b", "Poka\u017c wyniki kroku B", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol9b"),
      br(),

      conditionalPanel(condition = "input.ch7_ans9b % 2 == 1",
        div(class = "narrative",
          p(tags$b("Krok C."), " Policz 95% CI dla \u015bredniej ", tags$code("income"),
            " w ka\u017cdej z grup lunch. Je\u015bli przedzia\u0142y nie nachodz\u0105 na siebie,
            grupy dotacji to w rzeczywisto\u015bci grupy zamo\u017cno\u015bci.")
        ),
        actionButton("ch7_ans9c", "Poka\u017c wyniki kroku C i wnioski", class = "btn-outline-success btn-sm"),
        uiOutput("ch7_sol9c")
      )
    )
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zako\u0144czeniu \u0107wicze\u0144 odpowiedz:")),
    tags$ol(
      tags$li("Co najsilniej wp\u0142yn\u0119\u0142o na szeroko\u015b\u0107 CI \u2014 n, s, czy poziom ufno\u015bci?"),
      tags$li("Dlaczego CI dla proporcji w zadaniu 7 by\u0142 tak szeroki?"),
      tags$li("Czego nauczy\u0142o nas zadanie 9 o interpretacji zwi\u0105zk\u00f3w mi\u0119dzy zmiennymi?")
    )
  ),
  actionButton("ch7_ans_summary", "Poka\u017c odpowiedzi", class = "btn-outline-success btn-sm"),
  uiOutput("ch7_sol_summary"),
  br()
)

# --------------------------------------------------------------------------
# BHP
# --------------------------------------------------------------------------

.ch7_content_bhp <- function() tagList(

  div(class = "callout-info",
    p(tags$b("Otw\u00f3rz plik "), tags$code("dane/bhp_zaklady.csv"), tags$b(" w Jamovi"), "."),
    p("Dane z 320 zak\u0142ad\u00f3w produkcyjnych. Zmienne: wska\u017anik wypadk\u00f3w (",
      tags$code("wskaznik_wypadkow"), "), absencja (",
      tags$code("absencja_dni"), "), ha\u0142as ponad norm\u0119 (",
      tags$code("ponad_norma_halas"), "), naruszenia (",
      tags$code("naruszen_proc"), "), wielko\u015b\u0107 (",
      tags$code("wielkosc"), "), bran\u017ca (",
      tags$code("branza"), "), zmianowo\u015b\u0107 (",
      tags$code("zmiany"), ").")
  ),

  div(class = "section-title", "Blok 1: Przedzia\u0142 ufno\u015bci dla \u015bredniej (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 1 \u2014 Typowy wska\u017anik wypadk\u00f3w w polskich zak\u0142adach"),
    div(class = "narrative",
      p("Inspekcja pracy chce wiedzie\u0107: ", tags$em("\u201ejaki jest typowy wska\u017anik wypadk\u00f3w w polskim zak\u0142adzie produkcyjnym?\u201d"),
        " Wyznacz 95% CI dla \u015bredniej zmiennej ", tags$code("wskaznik_wypadkow"),
        ". Przed sprawdzeniem odpowiedzi: ile wynosi \u015brednia i granice CI?
        Jak powiedzieliby\u015b inspekcji jednym zdaniem?")
    ),
    actionButton("ch7_ans1", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 \u2014 A jak wygl\u0105da absencja?"),
    div(class = "narrative",
      p("Powt\u00f3rz analiz\u0119 dla zmiennej ", tags$code("absencja_dni"),
        ". Por\u00f3wnaj szeroko\u015b\u0107 obu przedzia\u0142\u00f3w. n jest takie samo \u2014 co decyduje o r\u00f3\u017cnicy?")
    ),
    actionButton("ch7_ans2", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol2")
  ),

  div(class = "widget-block",
    h4("Zadanie 3 \u2014 Duże zak\u0142ady osobno"),
    div(class = "narrative",
      p("Przefiltruj dane do zak\u0142ad\u00f3w du\u017cych (", tags$code("wielkosc == \"duzy\""), ").
        Policz CI dla ", tags$code("wskaznik_wypadkow"), " na tej podgrupie.
        Wska\u017c ", tags$em("trzy"), " rzeczy, kt\u00f3re zmieniaj\u0105 CI wzgl\u0119dem pe\u0142nej pr\u00f3by.")
    ),
    actionButton("ch7_ans3", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 \u2014 Ile ufno\u015bci kupujemy za szeroko\u015b\u0107?"),
    div(class = "narrative",
      p("Wr\u00f3\u0107 do pe\u0142nych 320 zak\u0142ad\u00f3w. Policz CI dla ", tags$code("wskaznik_wypadkow"),
        " przy 90%, 95%, 99%. Zapisz marginesy b\u0142\u0119du."),
      p(tags$em("Dyskusja:"), " przy jakim poziomie ufno\u015bci raportowa\u0142aby inspekcja pracy?")
    ),
    actionButton("ch7_ans4", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol4")
  ),

  div(class = "section-title", "Blok 2: Przedzia\u0142 ufno\u015bci dla proporcji (~20 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 \u2014 Jaki odsetek zak\u0142ad\u00f3w przekracza norm\u0119 ha\u0142asu?"),
    div(class = "narrative",
      p("Zmienna ", tags$code("ponad_norma_halas"),
        " m\u00f3wi, czy ha\u0142as przekracza 85 dB. Wyznacz 95% CI dla proporcji takich zak\u0142ad\u00f3w.
        Sprawd\u017a warunki sensowno\u015bci.")
    ),
    actionButton("ch7_ans5", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 \u2014 Cz\u0119ste naruszenia przepis\u00f3w"),
    div(class = "narrative",
      p("Utw\u00f3rz zmienn\u0105 binarną: ", tags$code("naruszen_proc > 20"),
        " (wi\u0119cej ni\u017c 20% kontroli ko\u0144czy si\u0119 naruszeniem).
        Wyznacz 95% CI i por\u00f3wnaj szeroko\u015b\u0107 z zadaniem 5. Dlaczego r\u00f3\u017cnica?")
    ),
    actionButton("ch7_ans6", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol6")
  ),

  div(class = "widget-block",
    h4("Zadanie 7 \u2014 Co je\u015bli mamy tylko 30 zak\u0142ad\u00f3w?"),
    div(class = "narrative",
      p("Przefiltruj do pierwszych 30 zak\u0142ad\u00f3w. Policz CI dla ",
        tags$code("ponad_norma_halas"), ". Dlaczego CI jest tak szeroki?
        Czy pierwsze 30 zak\u0142ad\u00f3w to reprezentatywna pr\u00f3ba?")
    ),
    actionButton("ch7_ans7", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol7")
  ),

  div(class = "section-title", "Blok 3: Interpretacja i my\u015blenie krytyczne (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 8 \u2014 Prawda czy fa\u0142sz?"),
    div(class = "narrative",
      p("Przyjmijmy, \u017ce w zadaniu 1 dosta\u0142e\u015b 95% CI dla \u015bredniej ",
        tags$code("wskaznik_wypadkow"), " r\u00f3wny ", tags$b("[9.33, 10.15]"),
        ". Oce\u0144 ka\u017cde stwierdzenie:"),
      tags$ol(
        tags$li(tags$b("a)"), " \u201eZ prawdopodobie\u0144stwem 95% prawdziwy \u015bredni wska\u017anik wypadk\u00f3w le\u017cy mi\u0119dzy 9.33 a 10.15.\u201d"),
        tags$li(tags$b("b)"), " \u201e95% zak\u0142ad\u00f3w ma wska\u017anik wypadk\u00f3w mi\u0119dzy 9.33 a 10.15.\u201d"),
        tags$li(tags$b("c)"), " \u201eGdyby\u015bmy powtarzali badanie, ~95% tak skonstruowanych przedzia\u0142\u00f3w zawiera\u0142oby prawdziw\u0105 \u015bredni\u0105.\u201d"),
        tags$li(tags$b("d)"), " \u201e\u015arednia z pr\u00f3by le\u017cy w przedziale [9.33, 10.15].\u201d"),
        tags$li(tags$b("e)"), " \u201eMamy 95% ufno\u015bci w metod\u0119, kt\u00f3ra wyprodukowa\u0142a ten przedzia\u0142.\u201d"),
        tags$li(tags$b("f)"), " \u201eGdyby\u015bmy podnie\u015bli poziom ufno\u015bci do 99%, przedzia\u0142 zw\u0119zi\u0142by si\u0119.\u201d")
      )
    ),
    actionButton("ch7_ans8", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol8")
  ),

  div(class = "widget-block",
    h4("Zadanie 9 \u2014 Czy zmianowo\u015b\u0107 naprawd\u0119 powoduje wi\u0119cej wypadk\u00f3w? (trudniejsze)"),

    div(class = "narrative",
      p(tags$b("Krok A."), " Podziel zak\u0142ady wg ", tags$code("zmiany"),
        " (jedna/dwie/trzy zmiany). Policz 95% CI dla \u015bredniej ",
        tags$code("wskaznik_wypadkow"), " w ka\u017cdej grupie.")
    ),
    actionButton("ch7_ans9a", "Poka\u017c wyniki kroku A", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol9a"),
    br(),

    conditionalPanel(condition = "input.ch7_ans9a % 2 == 1",
      div(class = "narrative",
        p(tags$b("Krok B."), " Wyniki sugeruj\u0105 wi\u0119cej wypadk\u00f3w przy trzech zmianach. Ale:
          podziel zak\u0142ady wg ", tags$code("branza"),
          " (spo\u017cywcza/metalowa/chemiczna) i policz CI dla wska\u017anika wypadk\u00f3w.")
      ),
      actionButton("ch7_ans9b", "Poka\u017c wyniki kroku B", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol9b"),
      br(),

      conditionalPanel(condition = "input.ch7_ans9b % 2 == 1",
        div(class = "narrative",
          p(tags$b("Krok C."), " Sprawd\u017a: policz CI dla wska\u017anika wypadk\u00f3w wg ",
            tags$code("branza"), " w ka\u017cdej grupie ", tags$code("zmiany"),
            ". Czy metalowe i chemiczne cz\u0119\u015bciej pracuj\u0105 na 3 zmiany?
            Je\u015bli tak \u2014 to bran\u017ca (a nie zmianowo\u015b\u0107) mo\u017ce by\u0107 prawdziwym czynnikiem.")
        ),
        actionButton("ch7_ans9c", "Poka\u017c wyniki kroku C i wnioski", class = "btn-outline-success btn-sm"),
        uiOutput("ch7_sol9c")
      )
    )
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zako\u0144czeniu \u0107wicze\u0144 odpowiedz:")),
    tags$ol(
      tags$li("Co najsilniej wp\u0142yn\u0119\u0142o na szeroko\u015b\u0107 CI \u2014 n, s, czy poziom ufno\u015bci?"),
      tags$li("Dlaczego CI dla proporcji w zadaniu 7 by\u0142 tak szeroki?"),
      tags$li("Czego nauczy\u0142o nas zadanie 9 o interpretacji zwi\u0105zk\u00f3w mi\u0119dzy zmiennymi?")
    )
  ),
  actionButton("ch7_ans_summary", "Poka\u017c odpowiedzi", class = "btn-outline-success btn-sm"),
  uiOutput("ch7_sol_summary"),
  br()
)

# --------------------------------------------------------------------------
# ROLNICTWO
# --------------------------------------------------------------------------

.ch7_content_rol <- function() tagList(

  div(class = "callout-info",
    p(tags$b("Otw\u00f3rz plik "), tags$code("dane/rolnictwo_pola.csv"), tags$b(" w Jamovi"), "."),
    p("Dane z 280 p\u00f3l uprawnych. Zmienne: plon pszenicy (",
      tags$code("plon_pszenicy"), " t/ha), plon rzepaku (",
      tags$code("plon_rzepa"), "), klasa gleby (",
      tags$code("klasa_gleby"), "), nawo\u017cenie (",
      tags$code("nawozenie"), "), wilgotno\u015b\u0107 (",
      tags$code("wilgotnosc_proc"), "), plon poni\u017cej 5 t/ha (",
      tags$code("plon_ponizej_5"), "), wilgotno\u015b\u0107 >70% (",
      tags$code("wilg_powyzej_70"), ").")
  ),

  div(class = "section-title", "Blok 1: Przedzia\u0142 ufno\u015bci dla \u015bredniej (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 1 \u2014 Typowy plon pszenicy w Polsce"),
    div(class = "narrative",
      p("Agencja rolna pyta: ", tags$em("\u201ejaki jest typowy plon pszenicy na polskim polu uprawnym?\u201d"),
        " Wyznacz 95% CI dla \u015bredniej zmiennej ", tags$code("plon_pszenicy"),
        ". Co powiedzieliby\u015b agencji jednym zdaniem?")
    ),
    actionButton("ch7_ans1", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 \u2014 A jak wypada rzepak?"),
    div(class = "narrative",
      p("Powt\u00f3rz analiz\u0119 dla zmiennej ", tags$code("plon_rzepa"),
        ". Por\u00f3wnaj szeroko\u015b\u0107 obu przedzia\u0142\u00f3w. n jest takie samo \u2014 co decyduje?")
    ),
    actionButton("ch7_ans2", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol2")
  ),

  div(class = "widget-block",
    h4("Zadanie 3 \u2014 Pola klasy I osobno"),
    div(class = "narrative",
      p("Przefiltruj dane do p\u00f3l klasy I (", tags$code("klasa_gleby == \"I\""), ").
        Policz CI dla ", tags$code("plon_pszenicy"), " na tej podgrupie.
        Wska\u017c ", tags$em("trzy"), " rzeczy, kt\u00f3re zmieniaj\u0105 CI wzgl\u0119dem pe\u0142nej pr\u00f3by.")
    ),
    actionButton("ch7_ans3", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 \u2014 Ile ufno\u015bci kupujemy za szeroko\u015b\u0107?"),
    div(class = "narrative",
      p("Wr\u00f3\u0107 do pe\u0142nych 280 p\u00f3l. Policz CI dla ", tags$code("plon_pszenicy"),
        " przy 90%, 95%, 99%. Zapisz marginesy b\u0142\u0119du."),
      p(tags$em("Dyskusja:"), " przy jakim poziomie ufno\u015bci raportowa\u0142aby agencja rolna?")
    ),
    actionButton("ch7_ans4", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol4")
  ),

  div(class = "section-title", "Blok 2: Przedzia\u0142 ufno\u015bci dla proporcji (~20 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 \u2014 Jaki odsetek p\u00f3l ma plon poni\u017cej op\u0142acalno\u015bci?"),
    div(class = "narrative",
      p("Zmienna ", tags$code("plon_ponizej_5"),
        " m\u00f3wi, czy plon pszenicy wynosi mniej ni\u017c 5 t/ha (pr\u00f3g op\u0142acalno\u015bci).
        Wyznacz 95% CI dla proporcji takich p\u00f3l. Sprawd\u017a warunki sensowno\u015bci.")
    ),
    actionButton("ch7_ans5", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 \u2014 Pola z nadmiern\u0105 wilgotno\u015bci\u0105"),
    div(class = "narrative",
      p("Zmienna ", tags$code("wilg_powyzej_70"),
        " m\u00f3wi, czy wilgotno\u015b\u0107 gleby przekracza 70% (ryzyko grzyb\u00f3w).
        Wyznacz 95% CI i por\u00f3wnaj szeroko\u015b\u0107 z zadaniem 5. Dlaczego r\u00f3\u017cnica?")
    ),
    actionButton("ch7_ans6", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol6")
  ),

  div(class = "widget-block",
    h4("Zadanie 7 \u2014 Co je\u015bli mamy tylko 30 p\u00f3l?"),
    div(class = "narrative",
      p("Przefiltruj do pierwszych 30 p\u00f3l. Policz CI dla ",
        tags$code("plon_ponizej_5"), ". Dlaczego CI jest tak szeroki?
        Czy pierwsze 30 p\u00f3l to reprezentatywna pr\u00f3ba?")
    ),
    actionButton("ch7_ans7", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol7")
  ),

  div(class = "section-title", "Blok 3: Interpretacja i my\u015blenie krytyczne (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 8 \u2014 Prawda czy fa\u0142sz?"),
    div(class = "narrative",
      p("Przyjmijmy, \u017ce w zadaniu 1 dosta\u0142e\u015b 95% CI dla \u015bredniej ",
        tags$code("plon_pszenicy"), " r\u00f3wny ", tags$b("[6.03, 6.31]"),
        ". Oce\u0144 ka\u017cde stwierdzenie:"),
      tags$ol(
        tags$li(tags$b("a)"), " \u201eZ prawdopodobie\u0144stwem 95% prawdziwy \u015bredni plon le\u017cy mi\u0119dzy 6.03 a 6.31.\u201d"),
        tags$li(tags$b("b)"), " \u201e95% p\u00f3l ma plon mi\u0119dzy 6.03 a 6.31 t/ha.\u201d"),
        tags$li(tags$b("c)"), " \u201eGdyby\u015bmy powtarzali badanie, ~95% tak skonstruowanych przedzia\u0142\u00f3w zawiera\u0142oby prawdziw\u0105 \u015bredni\u0105.\u201d"),
        tags$li(tags$b("d)"), " \u201e\u015arednia z pr\u00f3by le\u017cy w przedziale [6.03, 6.31].\u201d"),
        tags$li(tags$b("e)"), " \u201eMamy 95% ufno\u015bci w metod\u0119, kt\u00f3ra wyprodukowa\u0142a ten przedzia\u0142.\u201d"),
        tags$li(tags$b("f)"), " \u201eGdyby\u015bmy podnie\u015bli poziom ufno\u015bci do 99%, przedzia\u0142 zw\u0119zi\u0142by si\u0119.\u201d")
      )
    ),
    actionButton("ch7_ans8", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol8")
  ),

  div(class = "widget-block",
    h4("Zadanie 9 \u2014 Czy nawo\u017cenie naprawd\u0119 poprawia plony? (trudniejsze)"),

    div(class = "narrative",
      p(tags$b("Krok A."), " Podziel pola wg ", tags$code("nawozenie"),
        " (niskie/\u015brednie/wysokie). Policz 95% CI dla \u015bredniej ",
        tags$code("plon_pszenicy"), " w ka\u017cdej grupie.")
    ),
    actionButton("ch7_ans9a", "Poka\u017c wyniki kroku A", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol9a"),
    br(),

    conditionalPanel(condition = "input.ch7_ans9a % 2 == 1",
      div(class = "narrative",
        p(tags$b("Krok B."), " Wyniki sugeruj\u0105 wy\u017csze nawo\u017cenie = wy\u017cszy plon. Ale:
          podziel pola wg ", tags$code("klasa_gleby"),
          " (I/II/III) i policz CI dla plonu pszenicy.")
      ),
      actionButton("ch7_ans9b", "Poka\u017c wyniki kroku B", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol9b"),
      br(),

      conditionalPanel(condition = "input.ch7_ans9b % 2 == 1",
        div(class = "narrative",
          p(tags$b("Krok C."), " Sprawd\u017a: policz \u0107redni\u0105 ", tags$code("plon_pszenicy"),
            " wg klasy gleby w ka\u017cdej grupie nawo\u017cenia. Czy pola z wysokim nawo\u017ceniem
            to cz\u0119\u015bciej klasa I i II? Je\u015bli tak \u2014 klasa gleby mo\u017ce by\u0107 prawdziwym czynnikiem.")
        ),
        actionButton("ch7_ans9c", "Poka\u017c wyniki kroku C i wnioski", class = "btn-outline-success btn-sm"),
        uiOutput("ch7_sol9c")
      )
    )
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zako\u0144czeniu \u0107wicze\u0144 odpowiedz:")),
    tags$ol(
      tags$li("Co najsilniej wp\u0142yn\u0119\u0142o na szeroko\u015b\u0107 CI \u2014 n, s, czy poziom ufno\u015bci?"),
      tags$li("Dlaczego CI dla proporcji w zadaniu 7 by\u0142 tak szeroki?"),
      tags$li("Czego nauczy\u0142o nas zadanie 9 o interpretacji zwi\u0105zk\u00f3w mi\u0119dzy zmiennymi?")
    )
  ),
  actionButton("ch7_ans_summary", "Poka\u017c odpowiedzi", class = "btn-outline-success btn-sm"),
  uiOutput("ch7_sol_summary"),
  br()
)

# --------------------------------------------------------------------------
# TECHNOLOGIA ZYWNOSCI
# --------------------------------------------------------------------------

.ch7_content_zyw <- function() tagList(

  div(class = "callout-info",
    p(tags$b("Otw\u00f3rz plik "), tags$code("dane/zywnosc_partie.csv"), tags$b(" w Jamovi"), "."),
    p("Dane z 350 partii produkcyjnych. Zmienne: zawarto\u015b\u0107 bia\u0142ka (",
      tags$code("zawartosc_bialka"), " %), t\u0142uszczu (",
      tags$code("zawartosc_tluszczu"), " %), linia produkcyjna (",
      tags$code("linia"), "), dostawca (",
      tags$code("dostawca"), "), bia\u0142ko poni\u017cej normy (",
      tags$code("bialko_ponizej_normy"), "), t\u0142uszcz powy\u017cej normy (",
      tags$code("tluszcz_powyzej_normy"), ").")
  ),

  div(class = "section-title", "Blok 1: Przedzia\u0142 ufno\u015bci dla \u015bredniej (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 1 \u2014 Typowa zawarto\u015b\u0107 bia\u0142ka w produkcie"),
    div(class = "narrative",
      p("Dzia\u0142 jako\u015bci pyta: ", tags$em("\u201ejaka jest typowa zawarto\u015b\u0107 bia\u0142ka w naszych partiach?\u201d"),
        " Wyznacz 95% CI dla \u015bredniej zmiennej ", tags$code("zawartosc_bialka"),
        ". Co powiedzieliby\u015b dzia\u0142owi jako\u015bci jednym zdaniem?")
    ),
    actionButton("ch7_ans1", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 \u2014 A jak wygl\u0105da zawarto\u015b\u0107 t\u0142uszczu?"),
    div(class = "narrative",
      p("Powt\u00f3rz analiz\u0119 dla zmiennej ", tags$code("zawartosc_tluszczu"),
        ". Por\u00f3wnaj szeroko\u015b\u0107 obu przedzia\u0142\u00f3w. n jest takie samo \u2014 co decyduje?")
    ),
    actionButton("ch7_ans2", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol2")
  ),

  div(class = "widget-block",
    h4("Zadanie 3 \u2014 Linia A osobno"),
    div(class = "narrative",
      p("Przefiltruj dane do linii A (", tags$code("linia == \"A\""), ").
        Policz CI dla ", tags$code("zawartosc_bialka"), " na tej podgrupie.
        Wska\u017c ", tags$em("trzy"), " rzeczy, kt\u00f3re zmieniaj\u0105 CI wzgl\u0119dem pe\u0142nej pr\u00f3by.")
    ),
    actionButton("ch7_ans3", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 \u2014 Ile ufno\u015bci kupujemy za szeroko\u015b\u0107?"),
    div(class = "narrative",
      p("Wr\u00f3\u0107 do pe\u0142nych 350 partii. Policz CI dla ", tags$code("zawartosc_bialka"),
        " przy 90%, 95%, 99%. Zapisz marginesy b\u0142\u0119du."),
      p(tags$em("Dyskusja:"), " przy jakim poziomie ufno\u015bci raportowa\u0142by dzia\u0142 jako\u015bci?")
    ),
    actionButton("ch7_ans4", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol4")
  ),

  div(class = "section-title", "Blok 2: Przedzia\u0142 ufno\u015bci dla proporcji (~20 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 \u2014 Jaki odsetek partii nie spe\u0142nia normy bia\u0142ka?"),
    div(class = "narrative",
      p("Zmienna ", tags$code("bialko_ponizej_normy"),
        " m\u00f3wi, czy zawarto\u015b\u0107 bia\u0142ka spada poni\u017cej 26% (norma jako\u015bciowa).
        Wyznacz 95% CI dla proporcji takich partii. Sprawd\u017a warunki sensowno\u015bci.")
    ),
    actionButton("ch7_ans5", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 \u2014 Partie z za du\u017cym t\u0142uszczem"),
    div(class = "narrative",
      p("Zmienna ", tags$code("tluszcz_powyzej_normy"),
        " m\u00f3wi, czy zawarto\u015b\u0107 t\u0142uszczu przekracza 3.0% (norma).
        Wyznacz 95% CI i por\u00f3wnaj szeroko\u015b\u0107 z zadaniem 5. Dlaczego r\u00f3\u017cnica?")
    ),
    actionButton("ch7_ans6", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol6")
  ),

  div(class = "widget-block",
    h4("Zadanie 7 \u2014 Co je\u015bli mamy tylko 30 partii?"),
    div(class = "narrative",
      p("Przefiltruj do pierwszych 30 partii. Policz CI dla ",
        tags$code("bialko_ponizej_normy"), ". Dlaczego CI jest tak szeroki?
        Czy pierwsze 30 partii to reprezentatywna pr\u00f3ba?")
    ),
    actionButton("ch7_ans7", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol7")
  ),

  div(class = "section-title", "Blok 3: Interpretacja i my\u015blenie krytyczne (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 8 \u2014 Prawda czy fa\u0142sz?"),
    div(class = "narrative",
      p("Przyjmijmy, \u017ce w zadaniu 1 dosta\u0142e\u015b 95% CI dla \u015bredniej ",
        tags$code("zawartosc_bialka"), " r\u00f3wny ", tags$b("[26.57, 26.85]"),
        ". Oce\u0144 ka\u017cde stwierdzenie:"),
      tags$ol(
        tags$li(tags$b("a)"), " \u201eZ prawdopodobie\u0144stwem 95% prawdziwa \u015brednia zawarto\u015b\u0107 bia\u0142ka le\u017cy mi\u0119dzy 26.57 a 26.85.\u201d"),
        tags$li(tags$b("b)"), " \u201e95% partii ma zawarto\u015b\u0107 bia\u0142ka mi\u0119dzy 26.57 a 26.85%.\u201d"),
        tags$li(tags$b("c)"), " \u201eGdyby\u015bmy powtarzali badanie, ~95% tak skonstruowanych przedzia\u0142\u00f3w zawiera\u0142oby prawdziw\u0105 \u015bredni\u0105.\u201d"),
        tags$li(tags$b("d)"), " \u201e\u015arednia z pr\u00f3by le\u017cy w przedziale [26.57, 26.85].\u201d"),
        tags$li(tags$b("e)"), " \u201eMamy 95% ufno\u015bci w metod\u0119, kt\u00f3ra wyprodukowa\u0142a ten przedzia\u0142.\u201d"),
        tags$li(tags$b("f)"), " \u201eGdyby\u015bmy podnie\u015bli poziom ufno\u015bci do 99%, przedzia\u0142 zw\u0119zi\u0142by si\u0119.\u201d")
      )
    ),
    actionButton("ch7_ans8", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol8")
  ),

  div(class = "widget-block",
    h4("Zadanie 9 \u2014 Czy dostawca naprawd\u0119 wp\u0142ywa na jako\u015b\u0107? (trudniejsze)"),

    div(class = "narrative",
      p(tags$b("Krok A."), " Podziel partie wg ", tags$code("dostawca"),
        " (lokalny/krajowy/importowany). Policz 95% CI dla \u015bredniej ",
        tags$code("zawartosc_bialka"), " w ka\u017cdej grupie.")
    ),
    actionButton("ch7_ans9a", "Poka\u017c wyniki kroku A", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol9a"),
    br(),

    conditionalPanel(condition = "input.ch7_ans9a % 2 == 1",
      div(class = "narrative",
        p(tags$b("Krok B."), " Wyniki sugeruj\u0105, \u017ce importowany dostawca daje wi\u0119cej bia\u0142ka. Ale:
          podziel partie wg ", tags$code("linia"),
          " (A/B/C) i policz CI dla zawarto\u015bci bia\u0142ka.")
      ),
      actionButton("ch7_ans9b", "Poka\u017c wyniki kroku B", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol9b"),
      br(),

      conditionalPanel(condition = "input.ch7_ans9b % 2 == 1",
        div(class = "narrative",
          p(tags$b("Krok C."), " Sprawd\u017a: policz \u015bredni\u0105 ", tags$code("zawartosc_bialka"),
            " wg linii w ka\u017cdej grupie dostawcy. Czy importowany dostawca cz\u0119\u015bciej
            zasila lini\u0119 A? Je\u015bli tak \u2014 linia produkcyjna mo\u017ce by\u0107 prawdziwym czynnikiem.")
        ),
        actionButton("ch7_ans9c", "Poka\u017c wyniki kroku C i wnioski", class = "btn-outline-success btn-sm"),
        uiOutput("ch7_sol9c")
      )
    )
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zako\u0144czeniu \u0107wicze\u0144 odpowiedz:")),
    tags$ol(
      tags$li("Co najsilniej wp\u0142yn\u0119\u0142o na szeroko\u015b\u0107 CI \u2014 n, s, czy poziom ufno\u015bci?"),
      tags$li("Dlaczego CI dla proporcji w zadaniu 7 by\u0142 tak szeroki?"),
      tags$li("Czego nauczy\u0142o nas zadanie 9 o interpretacji zwi\u0105zk\u00f3w mi\u0119dzy zmiennymi?")
    )
  ),
  actionButton("ch7_ans_summary", "Poka\u017c odpowiedzi", class = "btn-outline-success btn-sm"),
  uiOutput("ch7_sol_summary"),
  br()
)

# ============================================================================
# ROZWIAZANIA — listy per kierunek
# ============================================================================

.ch7_solutions <- list(

  edu = list(
    sol1 = withMathJax(tagList(
      p(tags$b("Wyniki z Jamovi dla zmiennej "), tags$code("read"), ":"),
      tags$ul(
        tags$li("n = 420"),
        tags$li("\u015arednia \u2248 ", tags$b("654.97")),
        tags$li("Odchylenie std s \u2248 ", tags$b("20.11")),
        tags$li("95% CI: ", tags$b("[653.04, 656.90]")),
        tags$li("Margines b\u0142\u0119du ME \u2248 ", tags$b("1.93"))
      ),
      p(tags$b("Sprawdzenie r\u0119czne:"),
        " \\(t^*_{0.975,\\,419} \\approx 1.966\\),",
        " \\(SE = 20.11/\\sqrt{420} \\approx 0.981\\),",
        " \\(ME = 1.966 \\cdot 0.981 \\approx 1.93\\)."),
      p(tags$b("Interpretacja:"),
        " mamy 95% ufno\u015bci, \u017ce \u015brednia populacji wynik\u00f3w z czytania le\u017cy w okolicach 653\u2013657 punkt\u00f3w.")
    )),
    sol2 = withMathJax(tagList(
      p(tags$b("Wyniki dla zmiennej "), tags$code("math"), ":"),
      tags$ul(
        tags$li("\u015arednia \u2248 ", tags$b("653.34")),
        tags$li("s \u2248 ", tags$b("18.75")),
        tags$li("95% CI: ", tags$b("[651.54, 655.14]"), ", ME \u2248 ", tags$b("1.80"))
      ),
      p(tags$b("Dlaczego CI dla math jest w\u0119\u017cszy?"),
        " n jest takie samo (420), r\u00f3\u017cnica wynika wy\u0142\u0105cznie z ", tags$em("zmienno\u015bci"),
        ": s(math) = 18.75 < s(read) = 20.11. Mniejsza zmienno\u015b\u0107 \u2192 mniejsze SE \u2192 ci\u0105\u015bniejszy CI.")
    )),
    sol3 = withMathJax(tagList(
      p(tags$b("Wyniki dla szk\u00f3\u0142 KK-06"), " (", tags$code("read"), "):"),
      tags$ul(
        tags$li("n = 61"),
        tags$li("\u015arednia \u2248 ", tags$b("662.08"), " (wy\u017csza ni\u017c og\u00f3\u0142!)"),
        tags$li("s \u2248 ", tags$b("20.51")),
        tags$li("95% CI: ", tags$b("[656.82, 667.33]"), ", ME \u2248 ", tags$b("5.25"))
      ),
      p(tags$b("Trzy przyczyny szerszego CI:")),
      tags$ol(
        tags$li(tags$b("Mniejsze n"), " \u2014 g\u0142\u00f3wny czynnik: \\(\\sqrt{420}/\\sqrt{61} \\approx 2.62\\)\u00d7 wi\u0119ksze SE."),
        tags$li(tags$b("Wi\u0119kszy t*"), " \u2014 dla df = 60: t* \u2248 2.000 vs 1.966 dla df = 419."),
        tags$li(tags$b("Nieznacznie wi\u0119ksze s"), " \u2014 20.51 vs 20.11, efekt marginalny.")
      ),
      p("Wniosek: ", tags$b("\\(SE \\propto 1/\\sqrt{n}\\)"),
        " \u2014 \u017ceby zmniejszy\u0107 CI o po\u0142ow\u0119 potrzeba 4\u00d7 wi\u0119cej danych.")
    )),
    sol4 = withMathJax(tagList(
      p(tags$b("CI dla "), tags$code("read"), tags$b(" przy r\u00f3\u017cnych poziomach ufno\u015bci"), " (n=420):"),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Poziom"), tags$th("Dolne"), tags$th("G\u00f3rne"), tags$th("ME"))),
        tags$tbody(
          tags$tr(tags$td("90%"), tags$td("653.35"), tags$td("656.59"), tags$td("1.62")),
          tags$tr(tags$td("95%"), tags$td("653.04"), tags$td("656.90"), tags$td("1.93")),
          tags$tr(tags$td("99%"), tags$td("652.43"), tags$td("657.51"), tags$td("2.54"))
        )
      ),
      p("ME(99%)/ME(90%) \u2248 2.54/1.62 \u2248 ", tags$b("1.57"),
        " \u2014 9 pp wi\u0119cej ufno\u015bci kosztuje ~57% szerszy CI.")
    )),
    sol5 = withMathJax(tagList(
      p(tags$b("Wyniki dla STR > 20:"), " p = 177/420 \u2248 ", tags$b("0.421")),
      tags$ul(
        tags$li("95% CI (Clopper-Pearson): ", tags$b("[0.374, 0.470]")),
        tags$li("Warunki: np = 177 \u2265 10 \u2713, n(1\u2212p) = 243 \u2265 10 \u2713")
      ),
      p(tags$b("Interpretacja:"), " szacujemy, \u017ce w populacji podobnych okr\u0119g\u00f3w
        37\u201347% mia\u0142oby STR > 20.")
    )),
    sol6 = withMathJax(tagList(
      p(tags$b("Wyniki dla english > 20:"), " p = 118/420 \u2248 ", tags$b("0.281")),
      tags$ul(
        tags$li("95% CI: ", tags$b("[0.239, 0.327]")),
        tags$li("Szeroko\u015b\u0107 \u2248 0.088 vs 0.097 w zad. 5")
      ),
      p(tags$b("Dlaczego w\u0119\u017cszy?"), " Im dalej p od 0.5, tym mniejsza wariancja \\(p(1-p)\\).",
        " Dla p = 0.28: \\(p(1-p) \\approx 0.20\\), dla p = 0.42: \\(p(1-p) \\approx 0.244\\).")
    )),
    sol7 = withMathJax(tagList(
      p(tags$b("Pierwsze 25 okr\u0119g\u00f3w, english > 20:"), " p = 19/25 \u2248 ", tags$b("0.76")),
      tags$ul(
        tags$li("95% CI Clopper-Pearson: ", tags$b("[0.549, 0.906]"), " \u2014 szeroko\u015b\u0107 ~0.36!")
      ),
      p(tags$b("Dwa efekty:"), " (1) ma\u0142e n, (2) pierwsze 25 to ",
        tags$em("pr\u00f3ba obci\u0105\u017cona"), " \u2014 inne hrabstwa, inna charakterystyka.
        Dlatego p = 0.76 dramatycznie r\u00f3\u017cni si\u0119 od populacyjnego ~0.28."),
      p(tags$b("Dlaczego Clopper-Pearson, a nie Wald?"), " Przy n=25 i p blisko 1
        Wald jest niedok\u0142adny i m\u00f3g\u0142by da\u0107 g\u00f3rne ograniczenie > 1.")
    )),
    sol8 = withMathJax(tagList(
      tags$ul(
        tags$li(tags$b("a) FA\u0141SZ."), " \u03bc jest sta\u0142e. To metoda ma 95% szans wyprodukowa\u0107 CI zawieraj\u0105cy \u03bc."),
        tags$li(tags$b("b) FA\u0141SZ."), " To by\u0142by prediction interval, nie CI dla \u015bredniej."),
        tags$li(tags$b("c) PRAWDA."), " Poprawna interpretacja cz\u0119stotliwo\u015bciowa."),
        tags$li(tags$b("d) PRAWDA, ale trywialna."), " \u015arednia pr\u00f3by zawsze le\u017cy w \u015brodku CI z definicji."),
        tags$li(tags$b("e) PRAWDA."), " Standardowe sformu\u0142owanie ufno\u015bci."),
        tags$li(tags$b("f) FA\u0141SZ."), " Wy\u017cszy poziom ufno\u015bci \u2192 szerszy CI, nie w\u0119\u017cszy.")
      )
    )),
    sol9a = tagList(
      p(tags$b("95% CI dla \u015bredniej read wg grup lunch:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Grupa lunch"), tags$th("n"), tags$th("\u015arednia read"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("ma\u0142o (<33%)"), tags$td("168"), tags$td("671.8"), tags$td("[669.8, 673.8]")),
          tags$tr(tags$td("\u015brednio (33\u201366%)"), tags$td("142"), tags$td("653.1"), tags$td("[651.3, 654.9]")),
          tags$tr(tags$td("du\u017co (>66%)"), tags$td("110"), tags$td("631.6"), tags$td("[629.3, 634.0]"))
        )
      ),
      p(tags$b("Obserwacja:"), " przedzia\u0142y nie nachodz\u0105 na siebie \u2014 r\u00f3\u017cnica ~40 pkt mi\u0119dzy skrajnymi grupami."),
      p(tags$em("Ale zanim wyci\u0105gniesz wnioski\u2026 przejd\u017a do kroku B."))
    ),
    sol9b = tagList(
      p(tags$b("95% CI dla \u015bredniej read wg grup income:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Grupa income"), tags$th("n"), tags$th("\u015arednia read"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("niski (<10 tys.)"), tags$td("73"), tags$td("633.9"), tags$td("[630.1, 637.7]")),
          tags$tr(tags$td("\u015bredni (10\u201320 tys.)"), tags$td("280"), tags$td("654.7"), tags$td("[652.9, 656.5]")),
          tags$tr(tags$td("wysoki (>20 tys.)"), tags$td("67"), tags$td("679.1"), tags$td("[675.3, 682.8]"))
        )
      ),
      p(tags$b("Ten sam wzorzec!"), " Bogatsze okr\u0119gi \u2192 lepsze wyniki. R\u00f3\u017cnica ~45 pkt."),
      p(tags$em("Mo\u017ce to, co widzieli\u015bmy w kroku A, nie ma nic wsp\u00f3lnego z obiadami? Przejd\u017a do kroku C."))
    ),
    sol9c = tagList(
      p(tags$b("95% CI dla \u015bredniej income wg grup lunch:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Grupa lunch"), tags$th("n"), tags$th("\u015aredni income"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("ma\u0142o (<33%)"), tags$td("168"), tags$td("20.33 tys. $"), tags$td("[19.04, 21.63]")),
          tags$tr(tags$td("\u015brednio (33\u201366%)"), tags$td("142"), tags$td("13.17 tys. $"), tags$td("[12.63, 13.71]")),
          tags$tr(tags$td("du\u017co (>66%)"), tags$td("110"), tags$td("10.43 tys. $"), tags$td("[9.91, 10.95]"))
        )
      ),
      div(class = "callout-warning",
        p(tags$b("Wniosek:"), " grupy dotacji = grupy zamo\u017cno\u015bci. Okr\u0119gi z du\u017cymi dotacjami s\u0105 biedne (~10 tys. $)."),
        p("To klasyczny przyk\u0142ad ", tags$b("zmiennej zak\u0142\u00f3caj\u0105cej (confounding)"),
          ". Doch\u00f3d powoduje gorsze wyniki \u2014 nie dotacje do obiad\u00f3w.
          Likwidacja programu pogorszy\u0142aby sytuacj\u0119 potrzebuj\u0105cych."),
        p(tags$em("Mora\u0142:"), " CI daj\u0105 precyzj\u0119 oszacowania, ale nie m\u00f3wi\u0105 o przyczynowo\u015bci.")
      )
    ),
    sol_summary = tagList(
      tags$ol(
        tags$li(tags$b("Co wp\u0142ywa na CI?"),
          " Najsilniej n (przez \\(\\sqrt{n}\\), zad. 3). Potem s (zad. 2). Poziom ufno\u015bci \u2014 s\u0142abiej (zad. 4)."),
        tags$li(tags$b("CI w zad. 7:"),
          " n=25 to ma\u0142a pr\u00f3ba; p blisko 1 powoduje niestabilno\u015b\u0107 Walda.
          Clopper-Pearson jest szeroki, ale uczciwy."),
        tags$li(tags$b("Zad. 9:"),
          " Korelacja \u2260 przyczynowo\u015b\u0107. Zmienna zak\u0142\u00f3caj\u0105ca mo\u017ce ukry\u0107 prawdziwy mechanizm.")
      )
    )
  ),

  bhp = list(
    sol1 = withMathJax(tagList(
      p(tags$b("Wyniki dla zmiennej "), tags$code("wskaznik_wypadkow"), ":"),
      tags$ul(
        tags$li("n = 320"),
        tags$li("\u015arednia \u2248 ", tags$b("9.74")),
        tags$li("s \u2248 ", tags$b("3.67")),
        tags$li("95% CI: ", tags$b("[9.33, 10.15]"), ", ME \u2248 ", tags$b("0.41"))
      ),
      p(tags$b("Interpretacja:"),
        " szacujemy, \u017ce w populacji podobnych zak\u0142ad\u00f3w \u015bredni wska\u017anik wypadk\u00f3w
        wynosi od 9.3 do 10.2 wypadk\u00f3w na 1000 pracownik\u00f3w rocznie.")
    )),
    sol2 = withMathJax(tagList(
      p(tags$b("Wyniki dla zmiennej "), tags$code("absencja_dni"), ":"),
      tags$ul(
        tags$li("\u015arednia \u2248 ", tags$b("14.27")),
        tags$li("s \u2248 ", tags$b("4.79")),
        tags$li("95% CI: ", tags$b("[13.74, 14.80]"), ", ME \u2248 ", tags$b("0.53"))
      ),
      p(tags$b("Dlaczego CI dla absencji jest szerszy?"),
        " n = 320 takie samo, ale s(absencja) = 4.79 > s(wypadki) = 3.67.
        Wi\u0119ksza zmienno\u015b\u0107 \u2192 wi\u0119ksze SE \u2192 szerszy CI.")
    )),
    sol3 = withMathJax(tagList(
      p(tags$b("Wyniki dla du\u017cych zak\u0142ad\u00f3w"), " (wielkosc == \"duzy\"):"),
      tags$ul(
        tags$li("n \u2248 64"),
        tags$li("\u015arednia \u2248 ", tags$b("10.8")),
        tags$li("95% CI: ", tags$b("[9.8, 11.8]"), ", ME \u2248 ", tags$b("1.0"))
      ),
      p(tags$b("Trzy przyczyny szerszego CI:")),
      tags$ol(
        tags$li(tags$b("Mniejsze n"), " \u2014 g\u0142\u00f3wny czynnik: \\(\\sqrt{320}/\\sqrt{64} = 2.24\\)\u00d7 wi\u0119ksze SE."),
        tags$li(tags$b("Wi\u0119kszy t*"), " \u2014 dla df \u2248 63: t* \u2248 2.00 vs 1.967 dla df = 319."),
        tags$li(tags$b("Mo\u017cliwe inne s"), " \u2014 du\u017ce zak\u0142ady mog\u0105 mie\u0107 inn\u0105 zmienno\u015b\u0107.")
      )
    )),
    sol4 = withMathJax(tagList(
      p(tags$b("CI dla wskaznik_wypadkow przy r\u00f3\u017cnych poziomach ufno\u015bci"), " (n=320):"),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Poziom"), tags$th("Dolne"), tags$th("G\u00f3rne"), tags$th("ME"))),
        tags$tbody(
          tags$tr(tags$td("90%"), tags$td("9.40"), tags$td("10.08"), tags$td("0.34")),
          tags$tr(tags$td("95%"), tags$td("9.33"), tags$td("10.15"), tags$td("0.41")),
          tags$tr(tags$td("99%"), tags$td("9.20"), tags$td("10.28"), tags$td("0.54"))
        )
      ),
      p("ME(99%)/ME(90%) \u2248 1.59. W BHP i inspekcji pracy \u017c\u0105da si\u0119 cz\u0119sto 99%,
        bo konsekwencje b\u0142\u0119du s\u0105 powa\u017cne.")
    )),
    sol5 = withMathJax(tagList(
      p(tags$b("Wyniki dla ponad_norma_halas:"), " p = 0.312 (n=320)"),
      tags$ul(
        tags$li("Sukces\u00f3w: ~100"),
        tags$li("95% CI: ", tags$b("[0.261, 0.366]")),
        tags$li("Warunki: np \u2248 100 \u2265 10 \u2713, n(1\u2212p) \u2248 220 \u2265 10 \u2713")
      ),
      p(tags$b("Interpretacja:"),
        " w populacji podobnych zak\u0142ad\u00f3w 26\u201337% mia\u0142oby przekroczony pr\u00f3g ha\u0142asu.")
    )),
    sol6 = withMathJax(tagList(
      p(tags$b("Wyniki dla naruszen_proc > 20:"), " p = 0.162 (n=320)"),
      tags$ul(
        tags$li("95% CI: ", tags$b("[0.124, 0.206]")),
        tags$li("Szeroko\u015b\u0107 \u2248 0.082 vs 0.105 w zad. 5")
      ),
      p(tags$b("Dlaczego w\u0119\u017cszy?"), " p = 0.162 jest dalej od 0.5 ni\u017c p = 0.312.",
        " Dla p = 0.16: \\(p(1-p) \\approx 0.135\\), dla p = 0.31: \\(p(1-p) \\approx 0.215\\).")
    )),
    sol7 = withMathJax(tagList(
      p(tags$b("Pierwsze 30 zak\u0142ad\u00f3w, ponad_norma_halas:")),
      tags$ul(
        tags$li("n = 30, p empiryczne zale\u017cy od danych"),
        tags$li("95% CI: drastycznie szerszy ni\u017c dla pe\u0142nych 320"),
        tags$li("Szeroko\u015b\u0107 CI \u221d \\(1/\\sqrt{n}\\): \\(\\sqrt{320}/\\sqrt{30} \\approx 3.27\\)\u00d7 wi\u0119ksza")
      ),
      p(tags$b("Uwaga:"), " pierwsze 30 zak\u0142ad\u00f3w to nie losowa pr\u00f3ba \u2014
        mog\u0105 by\u0107 zak\u0142ady z jednego regionu lub bran\u017cy, co wprowadza obci\u0105\u017cenie.")
    )),
    sol8 = withMathJax(tagList(
      tags$ul(
        tags$li(tags$b("a) FA\u0141SZ."), " \u03bc jest sta\u0142e. To metoda ma 95% szans wyprodukowa\u0107 CI zawieraj\u0105cy \u03bc."),
        tags$li(tags$b("b) FA\u0141SZ."), " To by\u0142by PI dla pojedynczego zak\u0142adu, nie CI dla \u015bredniej."),
        tags$li(tags$b("c) PRAWDA."), " Poprawna interpretacja cz\u0119stotliwo\u015bciowa."),
        tags$li(tags$b("d) PRAWDA, ale trywialna."), " \u015arednia pr\u00f3by zawsze le\u017cy w \u015brodku CI."),
        tags$li(tags$b("e) PRAWDA."), " Standardowe sformu\u0142owanie ufno\u015bci."),
        tags$li(tags$b("f) FA\u0141SZ."), " Wy\u017cszy poziom ufno\u015bci \u2192 szerszy CI.")
      )
    )),
    sol9a = tagList(
      p(tags$b("95% CI dla wskaznik_wypadkow wg zmianowo\u015bci:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Zmiany"), tags$th("n"), tags$th("\u015arednia"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("jedna"), tags$td("\u226496"), tags$td("9.1"), tags$td("[8.4, 9.8]")),
          tags$tr(tags$td("dwie"), tags$td("\u2264144"), tags$td("10.2"), tags$td("[9.7, 10.7]")),
          tags$tr(tags$td("trzy"), tags$td("\u226480"), tags$td("12.1"), tags$td("[11.3, 12.9]"))
        )
      ),
      p(tags$b("Obserwacja:"), " wi\u0119cej zmian \u2192 wi\u0119cej wypadk\u00f3w. Przedzia\u0142y nie nachodz\u0105 na siebie."),
      p(tags$em("Ale czy to zmianowo\u015b\u0107 sama w sobie? Przejd\u017a do kroku B."))
    ),
    sol9b = tagList(
      p(tags$b("95% CI dla wskaznik_wypadkow wg bran\u017cy:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Bran\u017ca"), tags$th("n"), tags$th("\u015arednia"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("spo\u017cywcza"), tags$td("\u2264112"), tags$td("7.8"), tags$td("[7.2, 8.4]")),
          tags$tr(tags$td("metalowa"), tags$td("\u2264128"), tags$td("11.6"), tags$td("[11.0, 12.2]")),
          tags$tr(tags$td("chemiczna"), tags$td("\u226480"), tags$td("10.3"), tags$td("[9.5, 11.1]"))
        )
      ),
      p(tags$b("Ten sam wzorzec!"), " Metalowa i chemiczna maj\u0105 wy\u017csze wska\u017aniki wypadk\u00f3w."),
      p(tags$em("Mo\u017ce to bran\u017ca (a nie zmianowo\u015b\u0107) jest prawdziwym czynnikiem? Przejd\u017a do kroku C."))
    ),
    sol9c = tagList(
      p(tags$b("Weryfikacja:"), " metalowe i chemiczne zak\u0142ady cz\u0119\u015bciej pracuj\u0105 na 3 zmiany
        (ze wzgl\u0119du na technologi\u0119 procesu \u2014 ci\u0105g\u0142a produkcja)."),
      div(class = "callout-warning",
        p(tags$b("Wniosek:"), " zmianowo\u015b\u0107 i wypadkowo\u015b\u0107 s\u0105 powi\u0105zane,
          ale czynnikiem zak\u0142\u00f3caj\u0105cym jest ", tags$b("bran\u017ca"),
          ". Zak\u0142ady metalowe i chemiczne s\u0105 bardziej niebezpieczne z natury procesu,
          a jednocze\u015bnie cz\u0119\u015bciej wymagaj\u0105 pracy ci\u0105g\u0142ej."),
        p("Wniosek inspekcji: poprawa BHP w metalowej/chemicznej mo\u017ce by\u0107 skuteczniejsza
          ni\u017c skracanie zmian."),
        p(tags$em("Mora\u0142:"), " CI mierz\u0105 precyzj\u0119, ale nie zastept\u0105 analizy mechanizmu przyczynowego.")
      )
    ),
    sol_summary = tagList(
      tags$ol(
        tags$li(tags$b("Co wp\u0142ywa na CI?"),
          " Najsilniej n (zad. 3). Potem s (zad. 2). Poziom ufno\u015bci \u2014 s\u0142abiej (zad. 4)."),
        tags$li(tags$b("CI w zad. 7:"),
          " n=30 to za ma\u0142o; pierwsze 30 to pr\u00f3ba nielosowa, wi\u0119c CI jest szeroki i obci\u0105\u017cony."),
        tags$li(tags$b("Zad. 9:"),
          " Bran\u017ca jest zmienn\u0105 zak\u0142\u00f3caj\u0105c\u0105 (confounding) dla zwi\u0105zku zmianowo\u015b\u0107\u2013wypadki.")
      )
    )
  ),

  rol = list(
    sol1 = withMathJax(tagList(
      p(tags$b("Wyniki dla zmiennej "), tags$code("plon_pszenicy"), ":"),
      tags$ul(
        tags$li("n = 280"),
        tags$li("\u015arednia \u2248 ", tags$b("6.17")),
        tags$li("s \u2248 ", tags$b("1.20")),
        tags$li("95% CI: ", tags$b("[6.03, 6.31]"), ", ME \u2248 ", tags$b("0.14"))
      ),
      p(tags$b("Interpretacja:"),
        " szacujemy, \u017ce w populacji podobnych p\u00f3l \u015bredni plon pszenicy wynosi 6.0\u20136.3 t/ha.")
    )),
    sol2 = withMathJax(tagList(
      p(tags$b("Wyniki dla zmiennej "), tags$code("plon_rzepa"), ":"),
      tags$ul(
        tags$li("\u015arednia \u2248 ", tags$b("4.21")),
        tags$li("s \u2248 ", tags$b("0.72")),
        tags$li("95% CI: ", tags$b("[4.13, 4.29]"), ", ME \u2248 ", tags$b("0.08"))
      ),
      p(tags$b("Dlaczego CI dla rzepaku jest w\u0119\u017cszy?"),
        " n = 280 takie samo, ale s(rzepa) = 0.72 < s(pszenica) = 1.20.",
        " Mniejsza zmienno\u015b\u0107 plonu rzepaku \u2192 mniejsze SE \u2192 ci\u0105\u015bniejszy CI.")
    )),
    sol3 = withMathJax(tagList(
      p(tags$b("Wyniki dla p\u00f3l klasy I"), " (klasa_gleby == \"I\"):"),
      tags$ul(
        tags$li("n \u2248 70"),
        tags$li("\u015arednia \u2248 ", tags$b("7.12"), " (wy\u017csza ni\u017c og\u00f3\u0142!)"),
        tags$li("95% CI: ", tags$b("[6.89, 7.45]"), ", ME \u2248 ", tags$b("0.28"))
      ),
      p(tags$b("Trzy przyczyny szerszego CI:")),
      tags$ol(
        tags$li(tags$b("Mniejsze n"), " \u2014 \\(\\sqrt{280}/\\sqrt{70} = 2\\)\u00d7 wi\u0119ksze SE."),
        tags$li(tags$b("Wi\u0119kszy t*"), " \u2014 dla df \u2248 69: t* \u2248 2.00 vs 1.968 dla df = 279."),
        tags$li(tags$b("Mo\u017cliwa inna zmienno\u015b\u0107"), " \u2014 pola klasy I mog\u0105 mie\u0107 inne s.")
      )
    )),
    sol4 = withMathJax(tagList(
      p(tags$b("CI dla plon_pszenicy przy r\u00f3\u017cnych poziomach ufno\u015bci"), " (n=280):"),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Poziom"), tags$th("Dolne"), tags$th("G\u00f3rne"), tags$th("ME"))),
        tags$tbody(
          tags$tr(tags$td("90%"), tags$td("6.05"), tags$td("6.29"), tags$td("0.12")),
          tags$tr(tags$td("95%"), tags$td("6.03"), tags$td("6.31"), tags$td("0.14")),
          tags$tr(tags$td("99%"), tags$td("5.99"), tags$td("6.35"), tags$td("0.18"))
        )
      ),
      p("Przy normach skupu agencja rolna mo\u017ce preferowa\u0107 95% lub 99%,
        bo konsekwencje b\u0142\u0119dnego szacunku s\u0105 finansowe.")
    )),
    sol5 = withMathJax(tagList(
      p(tags$b("Wyniki dla plon_ponizej_5:"), " p = 0.154 (n=280)"),
      tags$ul(
        tags$li("Sukces\u00f3w: ~43"),
        tags$li("95% CI: ", tags$b("[0.113, 0.202]")),
        tags$li("Warunki: np \u2248 43 \u2265 10 \u2713, n(1\u2212p) \u2248 237 \u2265 10 \u2713")
      ),
      p(tags$b("Interpretacja:"),
        " szacujemy, \u017ce w populacji podobnych p\u00f3l 11\u201320% nie osi\u0105ga progu op\u0142acalno\u015bci 5 t/ha.")
    )),
    sol6 = withMathJax(tagList(
      p(tags$b("Wyniki dla wilg_powyzej_70:"), " p = 0.161 (n=280)"),
      tags$ul(
        tags$li("95% CI: ", tags$b("[0.119, 0.209]")),
        tags$li("Szeroko\u015b\u0107 \u2248 0.090 vs 0.089 w zad. 5 \u2014 bardzo podobne")
      ),
      p(tags$b("Dlaczego podobna szeroko\u015b\u0107?"), " p = 0.154 i p = 0.161 s\u0105 prawie takie same,
        wi\u0119c wariancja \\(p(1-p)\\) jest podobna. CI b\u0119d\u0105 zbli\u017czone.")
    )),
    sol7 = withMathJax(tagList(
      p(tags$b("Pierwsze 30 p\u00f3l, plon_ponizej_5:")),
      tags$ul(
        tags$li("n = 30, p empiryczne zale\u017cy od danych"),
        tags$li("95% CI: drastycznie szerszy ni\u017c dla pe\u0142nych 280"),
        tags$li("Szeroko\u015b\u0107 CI \u221d \\(1/\\sqrt{n}\\): \\(\\sqrt{280}/\\sqrt{30} \\approx 3.06\\)\u00d7 wi\u0119ksza")
      ),
      p(tags$b("Uwaga:"), " pierwsze 30 p\u00f3l to nie losowa pr\u00f3ba \u2014
        mog\u0105 pochodzi\u0107 z jednego rejonu, o podobnej klasie gleby.")
    )),
    sol8 = withMathJax(tagList(
      tags$ul(
        tags$li(tags$b("a) FA\u0141SZ."), " \u03bc jest sta\u0142e. To metoda ma 95% szans wyprodukowa\u0107 CI zawieraj\u0105cy \u03bc."),
        tags$li(tags$b("b) FA\u0141SZ."), " To by\u0142by PI dla pojedynczego pola, nie CI dla \u015bredniej."),
        tags$li(tags$b("c) PRAWDA."), " Poprawna interpretacja cz\u0119stotliwo\u015bciowa."),
        tags$li(tags$b("d) PRAWDA, ale trywialna."), " \u015arednia pr\u00f3by zawsze le\u017cy w \u015brodku CI."),
        tags$li(tags$b("e) PRAWDA."), " Standardowe sformu\u0142owanie ufno\u015bci."),
        tags$li(tags$b("f) FA\u0141SZ."), " Wy\u017cszy poziom ufno\u015bci \u2192 szerszy CI.")
      )
    )),
    sol9a = tagList(
      p(tags$b("95% CI dla plon_pszenicy wg nawo\u017cenia:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Nawo\u017cenie"), tags$th("n"), tags$th("\u015arednia (t/ha)"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("niskie"), tags$td("\u226470"), tags$td("5.5"), tags$td("[5.2, 5.8]")),
          tags$tr(tags$td("\u015brednie"), tags$td("\u2264140"), tags$td("6.3"), tags$td("[6.1, 6.5]")),
          tags$tr(tags$td("wysokie"), tags$td("\u226470"), tags$td("6.9"), tags$td("[6.6, 7.2]"))
        )
      ),
      p(tags$b("Obserwacja:"), " wy\u017csze nawo\u017cenie \u2192 wy\u017cszy plon. Przedzia\u0142y nie nachodz\u0105 na siebie."),
      p(tags$em("Ale czy to nawo\u017cenie samo w sobie? Przejd\u017a do kroku B."))
    ),
    sol9b = tagList(
      p(tags$b("95% CI dla plon_pszenicy wg klasy gleby:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Klasa gleby"), tags$th("n"), tags$th("\u015arednia (t/ha)"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("I"), tags$td("\u226470"), tags$td("7.1"), tags$td("[6.9, 7.4]")),
          tags$tr(tags$td("II"), tags$td("\u2264126"), tags$td("6.1"), tags$td("[5.9, 6.3]")),
          tags$tr(tags$td("III"), tags$td("\u226484"), tags$td("5.0"), tags$td("[4.8, 5.2]"))
        )
      ),
      p(tags$b("Ten sam wzorzem!"), " Lepsza klasa gleby \u2192 wy\u017cszy plon. R\u00f3\u017cnica ~2 t/ha mi\u0119dzy I a III."),
      p(tags$em("Czy rolnicy z lepsz\u0105 glebą wi\u0119cej nawo\u017c\u0105? Przejd\u017a do kroku C."))
    ),
    sol9c = tagList(
      p(tags$b("Weryfikacja:"), " pola z wysokim nawo\u017ceniem to cz\u0119\u015bciej klasy I i II
        (lepsi rolnicy inwestuj\u0105 w obie metody, lub lepsza gleba op\u0142aca si\u0119 nawo\u017ci\u0107)."),
      div(class = "callout-warning",
        p(tags$b("Wniosek:"), " nawo\u017cenie i plon s\u0105 powi\u0105zane, ale czynnikiem zak\u0142\u00f3caj\u0105cym jest ",
          tags$b("klasa gleby"),
          ". Pola z lepsz\u0105 glebą daj\u0105 wy\u017cszy plon niezale\u017cnie od nawo\u017cenia,
          a jednocze\u015bnie s\u0105 cz\u0119\u015bciej intensywnie nawo\u017cone."),
        p("Aby oceni\u0107 efekt samego nawo\u017cenia, nale\u017ca\u0142oby por\u00f3wna\u0107 pola ",
          tags$em("tej samej klasy gleby"), " z r\u00f3\u017cnym nawo\u017ceniem."),
        p(tags$em("Mora\u0142:"), " CI daj\u0105 precyzj\u0119, ale bez kontroli zmiennej zak\u0142\u00f3caj\u0105cej
          wnioski o przyczynach mog\u0105 by\u0107 b\u0142\u0119dne.")
      )
    ),
    sol_summary = tagList(
      tags$ol(
        tags$li(tags$b("Co wp\u0142ywa na CI?"),
          " Najsilniej n (zad. 3). Potem s (zad. 2). Poziom ufno\u015bci \u2014 s\u0142abiej (zad. 4)."),
        tags$li(tags$b("CI w zad. 7:"),
          " n=30 to za ma\u0142o; pierwsze 30 p\u00f3l to pr\u00f3ba nielosowa."),
        tags$li(tags$b("Zad. 9:"),
          " Klasa gleby jest zmienn\u0105 zak\u0142\u00f3caj\u0105c\u0105 dla zwi\u0105zku nawo\u017cenie\u2013plon.")
      )
    )
  ),

  zyw = list(
    sol1 = withMathJax(tagList(
      p(tags$b("Wyniki dla zmiennej "), tags$code("zawartosc_bialka"), ":"),
      tags$ul(
        tags$li("n = 350"),
        tags$li("\u015arednia \u2248 ", tags$b("26.71")),
        tags$li("s \u2248 ", tags$b("1.32")),
        tags$li("95% CI: ", tags$b("[26.57, 26.85]"), ", ME \u2248 ", tags$b("0.14"))
      ),
      p(tags$b("Interpretacja:"),
        " szacujemy, \u017ce w populacji podobnych partii \u015brednia zawarto\u015b\u0107 bia\u0142ka
        wynosi 26.57\u201326.85%.")
    )),
    sol2 = withMathJax(tagList(
      p(tags$b("Wyniki dla zmiennej "), tags$code("zawartosc_tluszczu"), ":"),
      tags$ul(
        tags$li("\u015arednia \u2248 ", tags$b("2.781")),
        tags$li("s \u2248 ", tags$b("0.244")),
        tags$li("95% CI: ", tags$b("[2.755, 2.807]"), ", ME \u2248 ", tags$b("0.026"))
      ),
      p(tags$b("Dlaczego CI dla t\u0142uszczu jest w\u0119\u017cszy?"),
        " n = 350 takie samo, ale s(t\u0142uszcz) = 0.244 < s(bia\u0142ko) = 1.32.",
        " Mniejsza zmienno\u015b\u0107 t\u0142uszczu \u2192 mniejsze SE \u2192 ci\u0105\u015bniejszy CI.")
    )),
    sol3 = withMathJax(tagList(
      p(tags$b("Wyniki dla linii A"), " (linia == \"A\"):"),
      tags$ul(
        tags$li("n \u2248 140"),
        tags$li("\u015arednia \u2248 ", tags$b("27.1")),
        tags$li("95% CI: ", tags$b("[26.9, 27.3]"), ", ME \u2248 ", tags$b("0.20"))
      ),
      p(tags$b("Trzy przyczyny szerszego CI:")),
      tags$ol(
        tags$li(tags$b("Mniejsze n"), " \u2014 \\(\\sqrt{350}/\\sqrt{140} = 1.58\\)\u00d7 wi\u0119ksze SE."),
        tags$li(tags$b("Wi\u0119kszy t*"), " \u2014 dla df \u2248 139: t* \u2248 1.977 vs 1.967 dla df = 349."),
        tags$li(tags$b("Mo\u017cliwa inna zmienno\u015b\u0107"), " \u2014 linia A mo\u017ce mie\u0107 inne s.")
      )
    )),
    sol4 = withMathJax(tagList(
      p(tags$b("CI dla zawartosc_bialka przy r\u00f3\u017cnych poziomach ufno\u015bci"), " (n=350):"),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Poziom"), tags$th("Dolne"), tags$th("G\u00f3rne"), tags$th("ME"))),
        tags$tbody(
          tags$tr(tags$td("90%"), tags$td("26.59"), tags$td("26.83"), tags$td("0.12")),
          tags$tr(tags$td("95%"), tags$td("26.57"), tags$td("26.85"), tags$td("0.14")),
          tags$tr(tags$td("99%"), tags$td("26.53"), tags$td("26.89"), tags$td("0.18"))
        )
      ),
      p("Dzia\u0142 jako\u015bci zwykle u\u017cywa 95%; przy normach prawnych (etykiety) mo\u017ce by\u0107 99%.")
    )),
    sol5 = withMathJax(tagList(
      p(tags$b("Wyniki dla bialko_ponizej_normy:"), " p = 0.291 (n=350)"),
      tags$ul(
        tags$li("Sukces\u00f3w: ~102"),
        tags$li("95% CI: ", tags$b("[0.245, 0.341]")),
        tags$li("Warunki: np \u2248 102 \u2265 10 \u2713, n(1\u2212p) \u2248 248 \u2265 10 \u2713")
      ),
      p(tags$b("Interpretacja:"),
        " szacujemy, \u017ce w populacji podobnych partii 24\u201334% nie spe\u0142nia normy bia\u0142ka.",
        " To powa\u017cny problem jako\u015bciowy.")
    )),
    sol6 = withMathJax(tagList(
      p(tags$b("Wyniki dla tluszcz_powyzej_normy:"), " p = 0.189 (n=350)"),
      tags$ul(
        tags$li("95% CI: ", tags$b("[0.149, 0.233]")),
        tags$li("Szeroko\u015b\u0107 \u2248 0.084 vs 0.096 w zad. 5")
      ),
      p(tags$b("Dlaczego w\u0119\u017cszy?"), " p = 0.189 jest dalej od 0.5 ni\u017c p = 0.291.",
        " Dla p = 0.19: \\(p(1-p) \\approx 0.153\\), dla p = 0.29: \\(p(1-p) \\approx 0.206\\).")
    )),
    sol7 = withMathJax(tagList(
      p(tags$b("Pierwsze 30 partii, bialko_ponizej_normy:")),
      tags$ul(
        tags$li("n = 30, p empiryczne zale\u017cy od danych"),
        tags$li("95% CI: drastycznie szerszy ni\u017c dla pe\u0142nych 350"),
        tags$li("Szeroko\u015b\u0107 CI \u221d \\(1/\\sqrt{n}\\): \\(\\sqrt{350}/\\sqrt{30} \\approx 3.42\\)\u00d7 wi\u0119ksza")
      ),
      p(tags$b("Uwaga:"), " pierwsze 30 partii to nie losowa pr\u00f3ba \u2014
        mog\u0105 pochodzi\u0107 z jednej linii lub jednego dostawcy.")
    )),
    sol8 = withMathJax(tagList(
      tags$ul(
        tags$li(tags$b("a) FA\u0141SZ."), " \u03bc jest sta\u0142e. To metoda ma 95% szans wyprodukowa\u0107 CI zawieraj\u0105cy \u03bc."),
        tags$li(tags$b("b) FA\u0141SZ."), " To by\u0142by PI dla pojedynczej partii, nie CI dla \u015bredniej."),
        tags$li(tags$b("c) PRAWDA."), " Poprawna interpretacja cz\u0119stotliwo\u015bciowa."),
        tags$li(tags$b("d) PRAWDA, ale trywialna."), " \u015arednia pr\u00f3by zawsze le\u017cy w \u015brodku CI."),
        tags$li(tags$b("e) PRAWDA."), " Standardowe sformu\u0142owanie ufno\u015bci."),
        tags$li(tags$b("f) FA\u0141SZ."), " Wy\u017cszy poziom ufno\u015bci \u2192 szerszy CI.")
      )
    )),
    sol9a = tagList(
      p(tags$b("95% CI dla zawartosc_bialka wg dostawcy:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Dostawca"), tags$th("n"), tags$th("\u015arednia (%)"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("lokalny"), tags$td("\u2264123"), tags$td("26.3"), tags$td("[26.1, 26.5]")),
          tags$tr(tags$td("krajowy"), tags$td("\u2264158"), tags$td("26.9"), tags$td("[26.7, 27.1]")),
          tags$tr(tags$td("importowany"), tags$td("\u226470"), tags$td("27.8"), tags$td("[27.5, 28.1]"))
        )
      ),
      p(tags$b("Obserwacja:"), " importowany dostawca \u2192 wy\u017csza zawarto\u015b\u0107 bia\u0142ka. Przedzia\u0142y nie nachodz\u0105."),
      p(tags$em("Ale czy to dostawca sam w sobie? Przejd\u017a do kroku B."))
    ),
    sol9b = tagList(
      p(tags$b("95% CI dla zawartosc_bialka wg linii:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Linia"), tags$th("n"), tags$th("\u015arednia (%)"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("A"), tags$td("\u2264140"), tags$td("27.1"), tags$td("[26.9, 27.3]")),
          tags$tr(tags$td("B"), tags$td("\u2264123"), tags$td("26.9"), tags$td("[26.7, 27.1]")),
          tags$tr(tags$td("C"), tags$td("\u226488"), tags$td("26.3"), tags$td("[26.0, 26.6]"))
        )
      ),
      p(tags$b("Ten sam wzorzec!"), " Linia A ma najwy\u017csze bia\u0142ko. Linia C \u2014 najni\u017csze."),
      p(tags$em("Czy importowany dostawca cz\u0119\u015bciej zasila lini\u0119 A? Przejd\u017a do kroku C."))
    ),
    sol9c = tagList(
      p(tags$b("Weryfikacja:"), " importowany dostawca cz\u0119\u015bciej zasila lini\u0119 A
        (umowy kontraktowe, specyfikacje jako\u015bciowe)."),
      div(class = "callout-warning",
        p(tags$b("Wniosek:"), " dostawca i bia\u0142ko s\u0105 powi\u0105zane, ale czynnikiem zak\u0142\u00f3caj\u0105cym jest ",
          tags$b("linia produkcyjna"),
          ". Linia A ma wy\u017csze bia\u0142ko ze wzgl\u0119du na swoje parametry technologiczne,
          a jednocze\u015bnie jest cz\u0119\u015bciej zasilana przez importowanego dostawc\u0119."),
        p("Aby oceni\u0107 efekt samego dostawcy, nale\u017ca\u0142oby por\u00f3wna\u0107 partie ",
          tags$em("tej samej linii"), " od r\u00f3\u017cnych dostawc\u00f3w."),
        p(tags$em("Mora\u0142:"), " CI precyzyjnie mierz\u0105 r\u00f3\u017cnice, ale bez kontroli zmiennej
          zak\u0142\u00f3caj\u0105cej wnioski o przyczynach mog\u0105 by\u0107 mylne.")
      )
    ),
    sol_summary = tagList(
      tags$ol(
        tags$li(tags$b("Co wp\u0142ywa na CI?"),
          " Najsilniej n (zad. 3). Potem s (zad. 2). Poziom ufno\u015bci \u2014 s\u0142abiej (zad. 4)."),
        tags$li(tags$b("CI w zad. 7:"),
          " n=30 to za ma\u0142o; pierwsze 30 partii to pr\u00f3ba nielosowa."),
        tags$li(tags$b("Zad. 9:"),
          " Linia produkcyjna jest zmienn\u0105 zak\u0142\u00f3caj\u0105c\u0105 dla zwi\u0105zku dostawca\u2013bia\u0142ko.")
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch7_server <- function(input, output, session) {

  sol_ids <- c("sol1","sol2","sol3","sol4","sol5","sol6","sol7","sol8",
               "sol9a","sol9b","sol9c","sol_summary")
  btn_ids <- c("ans1","ans2","ans3","ans4","ans5","ans6","ans7","ans8",
               "ans9a","ans9b","ans9c","ans_summary")

  # Stan widocznosci
  vis <- lapply(sol_ids, function(x) reactiveVal(FALSE))
  names(vis) <- sol_ids

  # Render tresci + reset stanow przy zmianie kierunku
  observeEvent(input$ch7_kierunek, {
    k <- input$ch7_kierunek
    for (sid in sol_ids) vis[[sid]](FALSE)
    for (bid in btn_ids) {
      updateActionButton(session, paste0("ch7_", bid), label = "Poka\u017c rozwi\u0105zanie")
    }
    updateActionButton(session, "ch7_ans9a", label = "Poka\u017c wyniki kroku A")
    updateActionButton(session, "ch7_ans9b", label = "Poka\u017c wyniki kroku B")
    updateActionButton(session, "ch7_ans9c", label = "Poka\u017c wyniki kroku C i wnioski")
    output$ch7_content <- renderUI({
      switch(k,
        edu = .ch7_content_edu(),
        bhp = .ch7_content_bhp(),
        rol = .ch7_content_rol(),
        zyw = .ch7_content_zyw()
      )
    })
  }, ignoreNULL = FALSE)

  # Toggle per zadanie
  .make_toggle <- function(sol_id_bare, sol_id_full, btn_id_full) {
    observeEvent(input[[btn_id_full]], {
      nowy_stan <- !vis[[sol_id_bare]]()
      vis[[sol_id_bare]](nowy_stan)
      updateActionButton(session, btn_id_full,
        label = if (nowy_stan) "Ukryj rozwi\u0105zanie" else "Poka\u017c rozwi\u0105zanie")
    }, ignoreInit = TRUE)

    output[[sol_id_full]] <- renderUI({
      if (!vis[[sol_id_bare]]()) return(NULL)
      k <- isolate(input$ch7_kierunek)
      sol <- .ch7_solutions[[k]][[sol_id_bare]]
      div(class = "callout-success", style = "margin-top: 10px;", sol)
    })
  }

  mapply(.make_toggle,
    sol_id_bare = sol_ids,
    sol_id_full = paste0("ch7_", sol_ids),
    btn_id_full = paste0("ch7_", btn_ids)
  )
}
