# ============================================================================
# CHAPTER 7: Cwiczenia praktyczne — przedzialy ufnosci
# Cztery warianty kierunkowe: Edukacja, BHP, Rolnictwo, Technologia Zywnosci
# ============================================================================

# ============================================================================
# UI
# ============================================================================

ch7_ui <- tabPanel("7. Ćwiczenia",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Czas zastosować wszystko, co poznaliśmy o przedziałach ufności na rzeczywistych danych."
    ),

    div(class = "section-title", "Ćwiczenia praktyczne — przedziały ufności"),

    div(class = "narrative",
      p(tags$b("Czas trwania:"), " ~ 90 minut · ",
        tags$b("Narzędzie:"), " Jamovi"),
      p("Trzy bloki zadań — CI dla średniej, CI dla proporcji, interpretacja i myślenie krytyczne.
        Każde zadanie ma ", tags$b("ukryte rozwiązanie"),
        " — kliknij przycisk, aby je zobaczyć.")
    ),

    div(class = "callout-info",
      selectInput("ch7_kierunek", tags$b("Wybierz wariant dla kierunku:"),
        choices = list(
          "Edukacja (CASchools)" = "edu",
          "Inżynieria Bezpieczeństwa (BHP)" = "bhp",
          "Rolnictwo" = "rol",
          "Technologia żywności" = "zyw"
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
    p(tags$b("Otwórz plik "), tags$code("dane/caschools.csv"), tags$b(" w Jamovi"), "."),
    p("Dane ze 420 okręgów szkolnych w Kalifornii. Zmienne: wyniki z czytania (",
      tags$code("read"), ") i matematyki (", tags$code("math"),
      "), stosunek uczniów do nauczycieli (",
      tags$code("students/teachers"), "), procent uczniów uczących się angielskiego (",
      tags$code("english"), "), dochód okręgu (", tags$code("income"), "), dotacje do obiadów (",
      tags$code("lunch"), "), typ szkoły (", tags$code("grades"), ").")
  ),

  div(class = "section-title", "Blok 1: Przedział ufności dla średniej (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 1 — Jak czytają dzieci w Kalifornii?"),
    div(class = "narrative",
      p("Kuratorium oświaty pyta: ", tags$em("„jaki jest typowy średni wynik z czytania w kalifornijskim okręgu?”"),
        " Wyznacz 95% CI dla średniej zmiennej ", tags$code("read"),
        ". Zanim klikniesz rozwiązanie: ile wynosi średnia, granice CI i co powiedzielibyś kuratorium jednym zdaniem?")
    ),
    actionButton("ch7_ans1", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 — A z matematyką lepiej czy gorzej?"),
    div(class = "narrative",
      p("Powtórz analizę dla zmiennej ", tags$code("math"),
        ". Dlaczego ", tags$b("przedziały"), " mają różną szerokość? n jest takie samo, więc co decyduje?")
    ),
    actionButton("ch7_ans2", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol2")
  ),

  div(class = "widget-block",
    h4("Zadanie 3 — CI dla każdej grupy"),
    div(class = "narrative",
      p("Wyznacz CI dla ", tags$code("read"), " dla każdej z grup ", tags$code("grades"), " osobno.")
    ),
    actionButton("ch7_ans3", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 — Ile ufności kupujemy za szerokość?"),
    div(class = "narrative",
      p("Wróć do pełnych 420 okręgów. Policz CI dla ", tags$code("read"),
        " przy poziomach ufności: 90%, 95%, 99%. Zapisz marginesy błędu i porównaj."),
      p(tags$em("Dyskusja:"), " kto żądałby 99% — statystyk akademicki czy inżynier od bezpieczeństwa lotów?")
    ),
    actionButton("ch7_ans4", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol4")
  ),

  div(class = "section-title", "Blok 2: Przedział ufności dla proporcji (~20 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 — Przepełnione klasy"),
    div(class = "narrative",
      p("Przy stosunku students/teachers > 20 trudno o indywidualne podejście.
        Stwórz zmienną binarną i wyznacz 95% CI dla proporcji okręgów z STR > 20.
        Sprawdź warunki sensowności przed interpretacją.")
    ),
    actionButton("ch7_ans5", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 — Dystrykty z dużym odsetkiem English learners"),
    div(class = "narrative",
      p("Okręgi z ", tags$code("english > 20%"), " są „językowo wymagające”.
        Wyznacz 95% CI dla tej proporcji i porównaj szerokość z zadaniem 5.
        Dlaczego jeden jest ciasniejszy, skoro n jest takie samo?")
    ),
    actionButton("ch7_ans6", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol6")
  ),

  div(class = "widget-block",
    h4("Zadanie 7 — Co jeśli mamy tylko 25 okręgów?"),
    div(class = "narrative",
      p("Przefiltruj do pierwszych 25 okręgów. Policz CI dla ", tags$code("english > 20"),
        ". Dlaczego CI jest tak szeroki? Czy pierwsze 25 wierszy to losowa próba?")
    ),
    actionButton("ch7_ans7", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol7")
  ),

  div(class = "section-title", "Blok 3: Interpretacja i myślenie krytyczne (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 8 — Prawda czy fałsz?"),
    div(class = "narrative",
      p("Przyjmijmy, że w zadaniu 1 dostałeś 95% CI dla średniej ",
        tags$code("read"), " równy ", tags$b("[653.0, 656.9]"), ". Oceń każde stwierdzenie:"),
      tags$ol(
        tags$li(tags$b("a)"), " „Z prawdopodobieństwem 95% prawdziwa średnia μ leży między 653.0 a 656.9.”"),
        tags$li(tags$b("b)"), " „95% wszystkich okręgów ma wynik z czytania między 653.0 a 656.9.”"),
        tags$li(tags$b("c)"), " „Gdybyśmy powtarzali badanie, ~95% tak skonstruowanych przedziałów zawierałoby prawdziwą średnią.”"),
        tags$li(tags$b("d)"), " „Średnia z próby leży w przedziale [653.0, 656.9].”"),
        tags$li(tags$b("e)"), " „Mamy 95% ufności w metodę, która wyprodukowała ten przedział.”"),
        tags$li(tags$b("f)"), " „Gdybyśmy podnieśli poziom ufności do 99%, przedział zwęziłby się.”")
      )
    ),
    actionButton("ch7_ans8", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol8")
  ),

  div(class = "widget-block",
    h4("Zadanie 9 — Czy dotacje do obiadów szkodzą uczniom? (trudniejsze)"),

    div(class = "narrative",
      p(tags$b("Krok A."), " Podziel okręgi wg ", tags$code("lunch"),
        " (dotacje do obiadów) na trzy grupy: mało (<33%), średnio (33–66%), dużo (>66%).
        Policz 95% CI dla średniej ", tags$code("read"), " i ", tags$code("math"), " w każdej grupie.")
    ),
    actionButton("ch7_ans9a", "Pokaż wyniki kroku A", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol9a"),
    br(),

    conditionalPanel(condition = "input.ch7_ans9a % 2 == 1",
      div(class = "narrative",
        p(tags$b("Krok B."), " Wyniki są dramatyczne. Ale zanim wyciągniesz wnioski:
          podziel okręgi wg ", tags$code("income"),
          " (niski <10, średni 10–20, wysoki >20 tys. $) i policz CI dla tych samych wyników.")
      ),
      actionButton("ch7_ans9b", "Pokaż wyniki kroku B", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol9b"),
      br(),

      conditionalPanel(condition = "input.ch7_ans9b % 2 == 1",
        div(class = "narrative",
          p(tags$b("Krok C."), " Policz 95% CI dla średniej ", tags$code("income"),
            " w każdej z grup lunch. Jeśli przedziały nie nachodzą na siebie,
            grupy dotacji to w rzeczywistości grupy zamożności.")
        ),
        actionButton("ch7_ans9c", "Pokaż wyniki kroku C i wnioski", class = "btn-outline-success btn-sm"),
        uiOutput("ch7_sol9c")
      )
    )
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zakończeniu ćwiczeń odpowiedz:")),
    tags$ol(
      tags$li("Co najsilniej wpłynęło na szerokość CI — n, s, czy poziom ufności?"),
      tags$li("Dlaczego CI dla proporcji w zadaniu 7 był tak szeroki?"),
      tags$li("Czego nauczyło nas zadanie 9 o interpretacji związków między zmiennymi?")
    )
  ),
  actionButton("ch7_ans_summary", "Pokaż odpowiedzi", class = "btn-outline-success btn-sm"),
  uiOutput("ch7_sol_summary"),
  br()
)

# --------------------------------------------------------------------------
# BHP
# --------------------------------------------------------------------------

.ch7_content_bhp <- function() tagList(

  div(class = "callout-info",
    p(tags$b("Otwórz plik "), tags$code("dane/bhp_zaklady.csv"), tags$b(" w Jamovi"), "."),
    p("Dane z 320 zakładów produkcyjnych. Zmienne: wskaźnik wypadków (",
      tags$code("wskaznik_wypadkow"), "), absencja (",
      tags$code("absencja_dni"), "), hałas ponad normę (",
      tags$code("ponad_norma_halas"), "), naruszenia (",
      tags$code("naruszen_proc"), "), wielkość (",
      tags$code("wielkosc"), "), branża (",
      tags$code("branza"), "), zmianowość (",
      tags$code("zmiany"), ").")
  ),

  div(class = "section-title", "Blok 1: Przedział ufności dla średniej (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 1 — Typowy wskaźnik wypadków w polskich zakładach"),
    div(class = "narrative",
      p("Inspekcja pracy chce wiedzieć: ", tags$em("„jaki jest typowy wskaźnik wypadków w polskim zakładzie produkcyjnym?”"),
        " Wyznacz 95% CI dla średniej zmiennej ", tags$code("wskaznik_wypadkow"),
        ". Przed sprawdzeniem odpowiedzi: ile wynosi średnia i granice CI?
        Jak powiedzielibyś inspekcji jednym zdaniem?")
    ),
    actionButton("ch7_ans1", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 — A jak wygląda absencja?"),
    div(class = "narrative",
      p("Powtórz analizę dla zmiennej ", tags$code("absencja_dni"),
        ". Porównaj szerokość obu przedziałów. n jest takie samo — co decyduje o różnicy?")
    ),
    actionButton("ch7_ans2", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol2")
  ),

  div(class = "widget-block",
    h4("Zadanie 3 — CI według wielkości zakładu"),
    div(class = "narrative",
      p("Wyznacz CI dla ", tags$code("wskaznik_wypadkow"), " dla każdej kategorii ", tags$code("wielkosc"), " osobno.")
    ),
    actionButton("ch7_ans3", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 — Ile ufności kupujemy za szerokość?"),
    div(class = "narrative",
      p("Wróć do pełnych 320 zakładów. Policz CI dla ", tags$code("wskaznik_wypadkow"),
        " przy 90%, 95%, 99%. Zapisz marginesy błędu."),
      p(tags$em("Dyskusja:"), " przy jakim poziomie ufności raportowałaby inspekcja pracy?")
    ),
    actionButton("ch7_ans4", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol4")
  ),

  div(class = "section-title", "Blok 2: Przedział ufności dla proporcji (~20 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 — Jaki odsetek zakładów przekracza normę hałasu?"),
    div(class = "narrative",
      p("Zmienna ", tags$code("ponad_norma_halas"),
        " mówi, czy hałas przekracza 85 dB. Wyznacz 95% CI dla proporcji takich zakładów.
        Sprawdź warunki sensowności.")
    ),
    actionButton("ch7_ans5", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 — Częste naruszenia przepisów"),
    div(class = "narrative",
      p("Utwórz zmienną binarną: ", tags$code("naruszen_proc > 20"),
        " (więcej niż 20% kontroli kończy się naruszeniem).
        Wyznacz 95% CI i porównaj szerokość z zadaniem 5. Dlaczego różnica?")
    ),
    actionButton("ch7_ans6", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol6")
  ),

  div(class = "widget-block",
    h4("Zadanie 7 — Co jeśli mamy tylko 30 zakładów?"),
    div(class = "narrative",
      p("Przefiltruj do pierwszych 30 zakładów. Policz CI dla ",
        tags$code("ponad_norma_halas"), ". Dlaczego CI jest tak szeroki?
        Czy pierwsze 30 zakładów to reprezentatywna próba?")
    ),
    actionButton("ch7_ans7", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol7")
  ),

  div(class = "section-title", "Blok 3: Interpretacja i myślenie krytyczne (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 8 — Prawda czy fałsz?"),
    div(class = "narrative",
      p("Przyjmijmy, że w zadaniu 1 dostałeś 95% CI dla średniej ",
        tags$code("wskaznik_wypadkow"), " równy ", tags$b("[9.33, 10.15]"),
        ". Oceń każde stwierdzenie:"),
      tags$ol(
        tags$li(tags$b("a)"), " „Z prawdopodobieństwem 95% prawdziwy średni wskaźnik wypadków leży między 9.33 a 10.15.”"),
        tags$li(tags$b("b)"), " „95% zakładów ma wskaźnik wypadków między 9.33 a 10.15.”"),
        tags$li(tags$b("c)"), " „Gdybyśmy powtarzali badanie, ~95% tak skonstruowanych przedziałów zawierałoby prawdziwą średnią.”"),
        tags$li(tags$b("d)"), " „Średnia z próby leży w przedziale [9.33, 10.15].”"),
        tags$li(tags$b("e)"), " „Mamy 95% ufności w metodę, która wyprodukowała ten przedział.”"),
        tags$li(tags$b("f)"), " „Gdybyśmy podnieśli poziom ufności do 99%, przedział zwęziłby się.”")
      )
    ),
    actionButton("ch7_ans8", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol8")
  ),

  div(class = "widget-block",
    h4("Zadanie 9 — Czy zmianowość naprawdę powoduje więcej wypadków? (trudniejsze)"),

    div(class = "narrative",
      p(tags$b("Krok A."), " Podziel zakłady wg ", tags$code("zmiany"),
        " (jedna/dwie/trzy zmiany). Policz 95% CI dla średniej ",
        tags$code("wskaznik_wypadkow"), " w każdej grupie.")
    ),
    actionButton("ch7_ans9a", "Pokaż wyniki kroku A", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol9a"),
    br(),

    conditionalPanel(condition = "input.ch7_ans9a % 2 == 1",
      div(class = "narrative",
        p(tags$b("Krok B."), " Wyniki sugerują więcej wypadków przy trzech zmianach. Ale:
          podziel zakłady wg ", tags$code("branza"),
          " (spożywcza/metalowa/chemiczna) i policz CI dla wskaźnika wypadków.")
      ),
      actionButton("ch7_ans9b", "Pokaż wyniki kroku B", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol9b"),
      br(),

      conditionalPanel(condition = "input.ch7_ans9b % 2 == 1",
        div(class = "narrative",
          p(tags$b("Krok C."), " Sprawdź: policz CI dla wskaźnika wypadków wg ",
            tags$code("branza"), " w każdej grupie ", tags$code("zmiany"),
            ". Czy metalowe i chemiczne częściej pracują na 3 zmiany?
            Jeśli tak — to branża (a nie zmianowość) może być prawdziwym czynnikiem.")
        ),
        actionButton("ch7_ans9c", "Pokaż wyniki kroku C i wnioski", class = "btn-outline-success btn-sm"),
        uiOutput("ch7_sol9c")
      )
    )
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zakończeniu ćwiczeń odpowiedz:")),
    tags$ol(
      tags$li("Co najsilniej wpłynęło na szerokość CI — n, s, czy poziom ufności?"),
      tags$li("Dlaczego CI dla proporcji w zadaniu 7 był tak szeroki?"),
      tags$li("Czego nauczyło nas zadanie 9 o interpretacji związków między zmiennymi?")
    )
  ),
  actionButton("ch7_ans_summary", "Pokaż odpowiedzi", class = "btn-outline-success btn-sm"),
  uiOutput("ch7_sol_summary"),
  br()
)

# --------------------------------------------------------------------------
# ROLNICTWO
# --------------------------------------------------------------------------

.ch7_content_rol <- function() tagList(

  div(class = "callout-info",
    p(tags$b("Otwórz plik "), tags$code("dane/rolnictwo_pola.csv"), tags$b(" w Jamovi"), "."),
    p("Dane z 280 pól uprawnych. Zmienne: plon pszenicy (",
      tags$code("plon_pszenicy"), " t/ha), plon rzepaku (",
      tags$code("plon_rzepa"), "), klasa gleby (",
      tags$code("klasa_gleby"), "), nawożenie (",
      tags$code("nawozenie"), "), wilgotność (",
      tags$code("wilgotnosc_proc"), "), plon poniżej 5 t/ha (",
      tags$code("plon_ponizej_5"), "), wilgotność >70% (",
      tags$code("wilg_powyzej_70"), ").")
  ),

  div(class = "section-title", "Blok 1: Przedział ufności dla średniej (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 1 — Typowy plon pszenicy w Polsce"),
    div(class = "narrative",
      p("Agencja rolna pyta: ", tags$em("„jaki jest typowy plon pszenicy na polskim polu uprawnym?”"),
        " Wyznacz 95% CI dla średniej zmiennej ", tags$code("plon_pszenicy"),
        ". Co powiedzielibyś agencji jednym zdaniem?")
    ),
    actionButton("ch7_ans1", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 — A jak wypada rzepak?"),
    div(class = "narrative",
      p("Powtórz analizę dla zmiennej ", tags$code("plon_rzepa"),
        ". Porównaj szerokość obu przedziałów. n jest takie samo — co decyduje?")
    ),
    actionButton("ch7_ans2", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol2")
  ),

  div(class = "widget-block",
    h4("Zadanie 3 — CI według klasy gleby"),
    div(class = "narrative",
      p("Wyznacz CI dla ", tags$code("plon_pszenicy"), " dla każdej klasy gleby (", tags$code("klasa_gleby"), ") osobno.")
    ),
    actionButton("ch7_ans3", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 — Ile ufności kupujemy za szerokość?"),
    div(class = "narrative",
      p("Wróć do pełnych 280 pól. Policz CI dla ", tags$code("plon_pszenicy"),
        " przy 90%, 95%, 99%. Zapisz marginesy błędu."),
      p(tags$em("Dyskusja:"), " przy jakim poziomie ufności raportowałaby agencja rolna?")
    ),
    actionButton("ch7_ans4", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol4")
  ),

  div(class = "section-title", "Blok 2: Przedział ufności dla proporcji (~20 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 — Jaki odsetek pól ma plon poniżej opłacalności?"),
    div(class = "narrative",
      p("Zmienna ", tags$code("plon_ponizej_5"),
        " mówi, czy plon pszenicy wynosi mniej niż 5 t/ha (próg opłacalności).
        Wyznacz 95% CI dla proporcji takich pól. Sprawdź warunki sensowności.")
    ),
    actionButton("ch7_ans5", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 — Pola z nadmierną wilgotnością"),
    div(class = "narrative",
      p("Zmienna ", tags$code("wilg_powyzej_70"),
        " mówi, czy wilgotność gleby przekracza 70% (ryzyko grzybów).
        Wyznacz 95% CI i porównaj szerokość z zadaniem 5. Dlaczego różnica?")
    ),
    actionButton("ch7_ans6", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol6")
  ),

  div(class = "widget-block",
    h4("Zadanie 7 — Co jeśli mamy tylko 30 pól?"),
    div(class = "narrative",
      p("Przefiltruj do pierwszych 30 pól. Policz CI dla ",
        tags$code("plon_ponizej_5"), ". Dlaczego CI jest tak szeroki?
        Czy pierwsze 30 pól to reprezentatywna próba?")
    ),
    actionButton("ch7_ans7", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol7")
  ),

  div(class = "section-title", "Blok 3: Interpretacja i myślenie krytyczne (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 8 — Prawda czy fałsz?"),
    div(class = "narrative",
      p("Przyjmijmy, że w zadaniu 1 dostałeś 95% CI dla średniej ",
        tags$code("plon_pszenicy"), " równy ", tags$b("[6.03, 6.31]"),
        ". Oceń każde stwierdzenie:"),
      tags$ol(
        tags$li(tags$b("a)"), " „Z prawdopodobieństwem 95% prawdziwy średni plon leży między 6.03 a 6.31.”"),
        tags$li(tags$b("b)"), " „95% pól ma plon między 6.03 a 6.31 t/ha.”"),
        tags$li(tags$b("c)"), " „Gdybyśmy powtarzali badanie, ~95% tak skonstruowanych przedziałów zawierałoby prawdziwą średnią.”"),
        tags$li(tags$b("d)"), " „Średnia z próby leży w przedziale [6.03, 6.31].”"),
        tags$li(tags$b("e)"), " „Mamy 95% ufności w metodę, która wyprodukowała ten przedział.”"),
        tags$li(tags$b("f)"), " „Gdybyśmy podnieśli poziom ufności do 99%, przedział zwęziłby się.”")
      )
    ),
    actionButton("ch7_ans8", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol8")
  ),

  div(class = "widget-block",
    h4("Zadanie 9 — Czy nawożenie naprawdę poprawia plony? (trudniejsze)"),

    div(class = "narrative",
      p(tags$b("Krok A."), " Podziel pola wg ", tags$code("nawozenie"),
        " (niskie/średnie/wysokie). Policz 95% CI dla średniej ",
        tags$code("plon_pszenicy"), " w każdej grupie.")
    ),
    actionButton("ch7_ans9a", "Pokaż wyniki kroku A", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol9a"),
    br(),

    conditionalPanel(condition = "input.ch7_ans9a % 2 == 1",
      div(class = "narrative",
        p(tags$b("Krok B."), " Wyniki sugerują wyższe nawożenie = wyższy plon. Ale:
          podziel pola wg ", tags$code("klasa_gleby"),
          " (I/II/III) i policz CI dla plonu pszenicy.")
      ),
      actionButton("ch7_ans9b", "Pokaż wyniki kroku B", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol9b"),
      br(),

      conditionalPanel(condition = "input.ch7_ans9b % 2 == 1",
        div(class = "narrative",
          p(tags$b("Krok C."), " Sprawdź: policz ćrednią ", tags$code("plon_pszenicy"),
            " wg klasy gleby w każdej grupie nawożenia. Czy pola z wysokim nawożeniem
            to częściej klasa I i II? Jeśli tak — klasa gleby może być prawdziwym czynnikiem.")
        ),
        actionButton("ch7_ans9c", "Pokaż wyniki kroku C i wnioski", class = "btn-outline-success btn-sm"),
        uiOutput("ch7_sol9c")
      )
    )
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zakończeniu ćwiczeń odpowiedz:")),
    tags$ol(
      tags$li("Co najsilniej wpłynęło na szerokość CI — n, s, czy poziom ufności?"),
      tags$li("Dlaczego CI dla proporcji w zadaniu 7 był tak szeroki?"),
      tags$li("Czego nauczyło nas zadanie 9 o interpretacji związków między zmiennymi?")
    )
  ),
  actionButton("ch7_ans_summary", "Pokaż odpowiedzi", class = "btn-outline-success btn-sm"),
  uiOutput("ch7_sol_summary"),
  br()
)

# --------------------------------------------------------------------------
# TECHNOLOGIA ZYWNOSCI
# --------------------------------------------------------------------------

.ch7_content_zyw <- function() tagList(

  div(class = "callout-info",
    p(tags$b("Otwórz plik "), tags$code("dane/zywnosc_partie.csv"), tags$b(" w Jamovi"), "."),
    p("Dane z 350 partii produkcyjnych. Zmienne: zawartość białka (",
      tags$code("zawartosc_bialka"), " %), tłuszczu (",
      tags$code("zawartosc_tluszczu"), " %), linia produkcyjna (",
      tags$code("linia"), "), dostawca (",
      tags$code("dostawca"), "), białko poniżej normy (",
      tags$code("bialko_ponizej_normy"), "), tłuszcz powyżej normy (",
      tags$code("tluszcz_powyzej_normy"), ").")
  ),

  div(class = "section-title", "Blok 1: Przedział ufności dla średniej (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 1 — Typowa zawartość białka w produkcie"),
    div(class = "narrative",
      p("Dział jakości pyta: ", tags$em("„jaka jest typowa zawartość białka w naszych partiach?”"),
        " Wyznacz 95% CI dla średniej zmiennej ", tags$code("zawartosc_bialka"),
        ". Co powiedzielibyś działowi jakości jednym zdaniem?")
    ),
    actionButton("ch7_ans1", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 — A jak wygląda zawartość tłuszczu?"),
    div(class = "narrative",
      p("Powtórz analizę dla zmiennej ", tags$code("zawartosc_tluszczu"),
        ". Porównaj szerokość obu przedziałów. n jest takie samo — co decyduje?")
    ),
    actionButton("ch7_ans2", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol2")
  ),

  div(class = "widget-block",
    h4("Zadanie 3 — CI według linii"),
    div(class = "narrative",
      p("Wyznacz CI dla ", tags$code("zawartosc_bialka"), " dla każdej linii (", tags$code("linia"), ") osobno.")
    ),
    actionButton("ch7_ans3", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 — Ile ufności kupujemy za szerokość?"),
    div(class = "narrative",
      p("Wróć do pełnych 350 partii. Policz CI dla ", tags$code("zawartosc_bialka"),
        " przy 90%, 95%, 99%. Zapisz marginesy błędu."),
      p(tags$em("Dyskusja:"), " przy jakim poziomie ufności raportowałby dział jakości?")
    ),
    actionButton("ch7_ans4", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol4")
  ),

  div(class = "section-title", "Blok 2: Przedział ufności dla proporcji (~20 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 — Jaki odsetek partii nie spełnia normy białka?"),
    div(class = "narrative",
      p("Zmienna ", tags$code("bialko_ponizej_normy"),
        " mówi, czy zawartość białka spada poniżej 26% (norma jakościowa).
        Wyznacz 95% CI dla proporcji takich partii. Sprawdź warunki sensowności.")
    ),
    actionButton("ch7_ans5", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 — Partie z za dużym tłuszczem"),
    div(class = "narrative",
      p("Zmienna ", tags$code("tluszcz_powyzej_normy"),
        " mówi, czy zawartość tłuszczu przekracza 3.0% (norma).
        Wyznacz 95% CI i porównaj szerokość z zadaniem 5. Dlaczego różnica?")
    ),
    actionButton("ch7_ans6", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol6")
  ),

  div(class = "widget-block",
    h4("Zadanie 7 — Co jeśli mamy tylko 30 partii?"),
    div(class = "narrative",
      p("Przefiltruj do pierwszych 30 partii. Policz CI dla ",
        tags$code("bialko_ponizej_normy"), ". Dlaczego CI jest tak szeroki?
        Czy pierwsze 30 partii to reprezentatywna próba?")
    ),
    actionButton("ch7_ans7", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol7")
  ),

  div(class = "section-title", "Blok 3: Interpretacja i myślenie krytyczne (~25 min)"),

  div(class = "widget-block",
    h4("Zadanie 8 — Prawda czy fałsz?"),
    div(class = "narrative",
      p("Przyjmijmy, że w zadaniu 1 dostałeś 95% CI dla średniej ",
        tags$code("zawartosc_bialka"), " równy ", tags$b("[26.57, 26.85]"),
        ". Oceń każde stwierdzenie:"),
      tags$ol(
        tags$li(tags$b("a)"), " „Z prawdopodobieństwem 95% prawdziwa średnia zawartość białka leży między 26.57 a 26.85.”"),
        tags$li(tags$b("b)"), " „95% partii ma zawartość białka między 26.57 a 26.85%.”"),
        tags$li(tags$b("c)"), " „Gdybyśmy powtarzali badanie, ~95% tak skonstruowanych przedziałów zawierałoby prawdziwą średnią.”"),
        tags$li(tags$b("d)"), " „Średnia z próby leży w przedziale [26.57, 26.85].”"),
        tags$li(tags$b("e)"), " „Mamy 95% ufności w metodę, która wyprodukowała ten przedział.”"),
        tags$li(tags$b("f)"), " „Gdybyśmy podnieśli poziom ufności do 99%, przedział zwęziłby się.”")
      )
    ),
    actionButton("ch7_ans8", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol8")
  ),

  div(class = "widget-block",
    h4("Zadanie 9 — Czy dostawca naprawdę wpływa na jakość? (trudniejsze)"),

    div(class = "narrative",
      p(tags$b("Krok A."), " Podziel partie wg ", tags$code("dostawca"),
        " (lokalny/krajowy/importowany). Policz 95% CI dla średniej ",
        tags$code("zawartosc_bialka"), " w każdej grupie.")
    ),
    actionButton("ch7_ans9a", "Pokaż wyniki kroku A", class = "btn-outline-success btn-sm"),
    uiOutput("ch7_sol9a"),
    br(),

    conditionalPanel(condition = "input.ch7_ans9a % 2 == 1",
      div(class = "narrative",
        p(tags$b("Krok B."), " Wyniki sugerują, że importowany dostawca daje więcej białka. Ale:
          podziel partie wg ", tags$code("linia"),
          " (A/B/C) i policz CI dla zawartości białka.")
      ),
      actionButton("ch7_ans9b", "Pokaż wyniki kroku B", class = "btn-outline-success btn-sm"),
      uiOutput("ch7_sol9b"),
      br(),

      conditionalPanel(condition = "input.ch7_ans9b % 2 == 1",
        div(class = "narrative",
          p(tags$b("Krok C."), " Sprawdź: policz średnią ", tags$code("zawartosc_bialka"),
            " wg linii w każdej grupie dostawcy. Czy importowany dostawca częściej
            zasila linię A? Jeśli tak — linia produkcyjna może być prawdziwym czynnikiem.")
        ),
        actionButton("ch7_ans9c", "Pokaż wyniki kroku C i wnioski", class = "btn-outline-success btn-sm"),
        uiOutput("ch7_sol9c")
      )
    )
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zakończeniu ćwiczeń odpowiedz:")),
    tags$ol(
      tags$li("Co najsilniej wpłynęło na szerokość CI — n, s, czy poziom ufności?"),
      tags$li("Dlaczego CI dla proporcji w zadaniu 7 był tak szeroki?"),
      tags$li("Czego nauczyło nas zadanie 9 o interpretacji związków między zmiennymi?")
    )
  ),
  actionButton("ch7_ans_summary", "Pokaż odpowiedzi", class = "btn-outline-success btn-sm"),
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
        tags$li("Średnia ≈ ", tags$b("654.97")),
        tags$li("Odchylenie std s ≈ ", tags$b("20.11")),
        tags$li("95% CI: ", tags$b("[653.04, 656.90]")),
        tags$li("Margines błędu ME ≈ ", tags$b("1.93"))
      ),
      p(tags$b("Sprawdzenie ręczne:"),
        " \\(t^*_{0.975,\\,419} \\approx 1.966\\),",
        " \\(SE = 20.11/\\sqrt{420} \\approx 0.981\\),",
        " \\(ME = 1.966 \\cdot 0.981 \\approx 1.93\\)."),
      p(tags$b("Interpretacja:"),
        " mamy 95% ufności, że średnia populacji wyników z czytania leży w okolicach 653–657 punktów.")
    )),
    sol2 = withMathJax(tagList(
      p(tags$b("Wyniki dla zmiennej "), tags$code("math"), ":"),
      tags$ul(
        tags$li("Średnia ≈ ", tags$b("653.34")),
        tags$li("s ≈ ", tags$b("18.75")),
        tags$li("95% CI: ", tags$b("[651.54, 655.14]"), ", ME ≈ ", tags$b("1.80"))
      ),
      p(tags$b("Dlaczego CI dla math jest węższy?"),
        " n jest takie samo (420), różnica wynika wyłącznie z ", tags$em("zmienności"),
        ": s(math) = 18.75 < s(read) = 20.11. Mniejsza zmienność → mniejsze SE → ciąśniejszy CI.")
    )),
    sol3 = withMathJax(tagList(
      p(tags$b("Wyniki dla szkół KK-06"), " (", tags$code("read"), "):"),
      tags$ul(
        tags$li("n = 61"),
        tags$li("Średnia ≈ ", tags$b("662.08"), " (wyższa niż ogół!)"),
        tags$li("s ≈ ", tags$b("20.51")),
        tags$li("95% CI: ", tags$b("[656.82, 667.33]"), ", ME ≈ ", tags$b("5.25"))
      ),
      p(tags$b("Trzy przyczyny szerszego CI:")),
      tags$ol(
        tags$li(tags$b("Mniejsze n"), " — główny czynnik: \\(\\sqrt{420}/\\sqrt{61} \\approx 2.62\\)× większe SE."),
        tags$li(tags$b("Większy t*"), " — dla df = 60: t* ≈ 2.000 vs 1.966 dla df = 419."),
        tags$li(tags$b("Nieznacznie większe s"), " — 20.51 vs 20.11, efekt marginalny.")
      ),
      p("Wniosek: ", tags$b("\\(SE \\propto 1/\\sqrt{n}\\)"),
        " — żeby zmniejszyć CI o połowę potrzeba 4× więcej danych.")
    )),
    sol4 = withMathJax(tagList(
      p(tags$b("CI dla "), tags$code("read"), tags$b(" przy różnych poziomach ufności"), " (n=420):"),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Poziom"), tags$th("Dolne"), tags$th("Górne"), tags$th("ME"))),
        tags$tbody(
          tags$tr(tags$td("90%"), tags$td("653.35"), tags$td("656.59"), tags$td("1.62")),
          tags$tr(tags$td("95%"), tags$td("653.04"), tags$td("656.90"), tags$td("1.93")),
          tags$tr(tags$td("99%"), tags$td("652.43"), tags$td("657.51"), tags$td("2.54"))
        )
      ),
      p("ME(99%)/ME(90%) ≈ 2.54/1.62 ≈ ", tags$b("1.57"),
        " — 9 pp więcej ufności kosztuje ~57% szerszy CI.")
    )),
    sol5 = withMathJax(tagList(
      p(tags$b("Wyniki dla STR > 20:"), " p = 177/420 ≈ ", tags$b("0.421")),
      tags$ul(
        tags$li("95% CI (Clopper-Pearson): ", tags$b("[0.374, 0.470]")),
        tags$li("Warunki: np = 177 ≥ 10 ✓, n(1−p) = 243 ≥ 10 ✓")
      ),
      p(tags$b("Interpretacja:"), " szacujemy, że w populacji podobnych okręgów
        37–47% miałoby STR > 20.")
    )),
    sol6 = withMathJax(tagList(
      p(tags$b("Wyniki dla english > 20:"), " p = 118/420 ≈ ", tags$b("0.281")),
      tags$ul(
        tags$li("95% CI: ", tags$b("[0.239, 0.327]")),
        tags$li("Szerokość ≈ 0.088 vs 0.097 w zad. 5")
      ),
      p(tags$b("Dlaczego węższy?"), " Im dalej p od 0.5, tym mniejsza wariancja \\(p(1-p)\\).",
        " Dla p = 0.28: \\(p(1-p) \\approx 0.20\\), dla p = 0.42: \\(p(1-p) \\approx 0.244\\).")
    )),
    sol7 = withMathJax(tagList(
      p(tags$b("Pierwsze 25 okręgów, english > 20:"), " p = 19/25 ≈ ", tags$b("0.76")),
      tags$ul(
        tags$li("95% CI Clopper-Pearson: ", tags$b("[0.549, 0.906]"), " — szerokość ~0.36!")
      ),
      p(tags$b("Dwa efekty:"), " (1) małe n, (2) pierwsze 25 to ",
        tags$em("próba obciążona"), " — inne hrabstwa, inna charakterystyka.
        Dlatego p = 0.76 dramatycznie różni się od populacyjnego ~0.28."),
      p(tags$b("Dlaczego Clopper-Pearson, a nie Wald?"), " Przy n=25 i p blisko 1
        Wald jest niedokładny i mógłby dać górne ograniczenie > 1.")
    )),
    sol8 = withMathJax(tagList(
      tags$ul(
        tags$li(tags$b("a) FAŁSZ."), " μ jest stałe. To metoda ma 95% szans wyprodukować CI zawierający μ."),
        tags$li(tags$b("b) FAŁSZ."), " To byłby prediction interval, nie CI dla średniej."),
        tags$li(tags$b("c) PRAWDA."), " Poprawna interpretacja częstotliwościowa."),
        tags$li(tags$b("d) PRAWDA, ale trywialna."), " Średnia próby zawsze leży w środku CI z definicji."),
        tags$li(tags$b("e) PRAWDA."), " Standardowe sformułowanie ufności."),
        tags$li(tags$b("f) FAŁSZ."), " Wyższy poziom ufności → szerszy CI, nie węższy.")
      )
    )),
    sol9a = tagList(
      p(tags$b("95% CI dla średniej read wg grup lunch:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Grupa lunch"), tags$th("n"), tags$th("Średnia read"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("mało (<33%)"), tags$td("168"), tags$td("671.8"), tags$td("[669.8, 673.8]")),
          tags$tr(tags$td("średnio (33–66%)"), tags$td("142"), tags$td("653.1"), tags$td("[651.3, 654.9]")),
          tags$tr(tags$td("dużo (>66%)"), tags$td("110"), tags$td("631.6"), tags$td("[629.3, 634.0]"))
        )
      ),
      p(tags$b("Obserwacja:"), " przedziały nie nachodzą na siebie — różnica ~40 pkt między skrajnymi grupami."),
      p(tags$em("Ale zanim wyciągniesz wnioski… przejdź do kroku B."))
    ),
    sol9b = tagList(
      p(tags$b("95% CI dla średniej read wg grup income:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Grupa income"), tags$th("n"), tags$th("Średnia read"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("niski (<10 tys.)"), tags$td("73"), tags$td("633.9"), tags$td("[630.1, 637.7]")),
          tags$tr(tags$td("średni (10–20 tys.)"), tags$td("280"), tags$td("654.7"), tags$td("[652.9, 656.5]")),
          tags$tr(tags$td("wysoki (>20 tys.)"), tags$td("67"), tags$td("679.1"), tags$td("[675.3, 682.8]"))
        )
      ),
      p(tags$b("Ten sam wzorzec!"), " Bogatsze okręgi → lepsze wyniki. Różnica ~45 pkt."),
      p(tags$em("Może to, co widzieliśmy w kroku A, nie ma nic wspólnego z obiadami? Przejdź do kroku C."))
    ),
    sol9c = tagList(
      p(tags$b("95% CI dla średniej income wg grup lunch:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Grupa lunch"), tags$th("n"), tags$th("Średni income"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("mało (<33%)"), tags$td("168"), tags$td("20.33 tys. $"), tags$td("[19.04, 21.63]")),
          tags$tr(tags$td("średnio (33–66%)"), tags$td("142"), tags$td("13.17 tys. $"), tags$td("[12.63, 13.71]")),
          tags$tr(tags$td("dużo (>66%)"), tags$td("110"), tags$td("10.43 tys. $"), tags$td("[9.91, 10.95]"))
        )
      ),
      div(class = "callout-warning",
        p(tags$b("Wniosek:"), " grupy dotacji = grupy zamożności. Okręgi z dużymi dotacjami są biedne (~10 tys. $)."),
        p("To klasyczny przykład ", tags$b("zmiennej zakłócającej (confounding)"),
          ". Dochód powoduje gorsze wyniki — nie dotacje do obiadów.
          Likwidacja programu pogorszyłaby sytuację potrzebujących."),
        p(tags$em("Morał:"), " CI dają precyzję oszacowania, ale nie mówią o przyczynowości.")
      )
    ),
    sol_summary = tagList(
      tags$ol(
        tags$li(tags$b("Co wpływa na CI?"),
          " Najsilniej n (przez \\(\\sqrt{n}\\), zad. 3). Potem s (zad. 2). Poziom ufności — słabiej (zad. 4)."),
        tags$li(tags$b("CI w zad. 7:"),
          " n=25 to mała próba; p blisko 1 powoduje niestabilność Walda.
          Clopper-Pearson jest szeroki, ale uczciwy."),
        tags$li(tags$b("Zad. 9:"),
          " Korelacja ≠ przyczynowość. Zmienna zakłócająca może ukryć prawdziwy mechanizm.")
      )
    )
  ),

  bhp = list(
    sol1 = withMathJax(tagList(
      p(tags$b("Wyniki dla zmiennej "), tags$code("wskaznik_wypadkow"), ":"),
      tags$ul(
        tags$li("n = 320"),
        tags$li("Średnia ≈ ", tags$b("9.74")),
        tags$li("s ≈ ", tags$b("3.67")),
        tags$li("95% CI: ", tags$b("[9.33, 10.15]"), ", ME ≈ ", tags$b("0.41"))
      ),
      p(tags$b("Interpretacja:"),
        " szacujemy, że w populacji podobnych zakładów średni wskaźnik wypadków
        wynosi od 9.3 do 10.2 wypadków na 1000 pracowników rocznie.")
    )),
    sol2 = withMathJax(tagList(
      p(tags$b("Wyniki dla zmiennej "), tags$code("absencja_dni"), ":"),
      tags$ul(
        tags$li("Średnia ≈ ", tags$b("14.27")),
        tags$li("s ≈ ", tags$b("4.79")),
        tags$li("95% CI: ", tags$b("[13.74, 14.80]"), ", ME ≈ ", tags$b("0.53"))
      ),
      p(tags$b("Dlaczego CI dla absencji jest szerszy?"),
        " n = 320 takie samo, ale s(absencja) = 4.79 > s(wypadki) = 3.67.
        Większa zmienność → większe SE → szerszy CI.")
    )),
    sol3 = withMathJax(tagList(
      p(tags$b("Wyniki dla dużych zakładów"), " (wielkosc == \"duzy\"):"),
      tags$ul(
        tags$li("n ≈ 64"),
        tags$li("Średnia ≈ ", tags$b("10.8")),
        tags$li("95% CI: ", tags$b("[9.8, 11.8]"), ", ME ≈ ", tags$b("1.0"))
      ),
      p(tags$b("Trzy przyczyny szerszego CI:")),
      tags$ol(
        tags$li(tags$b("Mniejsze n"), " — główny czynnik: \\(\\sqrt{320}/\\sqrt{64} = 2.24\\)× większe SE."),
        tags$li(tags$b("Większy t*"), " — dla df ≈ 63: t* ≈ 2.00 vs 1.967 dla df = 319."),
        tags$li(tags$b("Możliwe inne s"), " — duże zakłady mogą mieć inną zmienność.")
      )
    )),
    sol4 = withMathJax(tagList(
      p(tags$b("CI dla wskaznik_wypadkow przy różnych poziomach ufności"), " (n=320):"),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Poziom"), tags$th("Dolne"), tags$th("Górne"), tags$th("ME"))),
        tags$tbody(
          tags$tr(tags$td("90%"), tags$td("9.40"), tags$td("10.08"), tags$td("0.34")),
          tags$tr(tags$td("95%"), tags$td("9.33"), tags$td("10.15"), tags$td("0.41")),
          tags$tr(tags$td("99%"), tags$td("9.20"), tags$td("10.28"), tags$td("0.54"))
        )
      ),
      p("ME(99%)/ME(90%) ≈ 1.59. W BHP i inspekcji pracy żąda się często 99%,
        bo konsekwencje błędu są poważne.")
    )),
    sol5 = withMathJax(tagList(
      p(tags$b("Wyniki dla ponad_norma_halas:"), " p = 0.312 (n=320)"),
      tags$ul(
        tags$li("Sukcesów: ~100"),
        tags$li("95% CI: ", tags$b("[0.261, 0.366]")),
        tags$li("Warunki: np ≈ 100 ≥ 10 ✓, n(1−p) ≈ 220 ≥ 10 ✓")
      ),
      p(tags$b("Interpretacja:"),
        " w populacji podobnych zakładów 26–37% miałoby przekroczony próg hałasu.")
    )),
    sol6 = withMathJax(tagList(
      p(tags$b("Wyniki dla naruszen_proc > 20:"), " p = 0.162 (n=320)"),
      tags$ul(
        tags$li("95% CI: ", tags$b("[0.124, 0.206]")),
        tags$li("Szerokość ≈ 0.082 vs 0.105 w zad. 5")
      ),
      p(tags$b("Dlaczego węższy?"), " p = 0.162 jest dalej od 0.5 niż p = 0.312.",
        " Dla p = 0.16: \\(p(1-p) \\approx 0.135\\), dla p = 0.31: \\(p(1-p) \\approx 0.215\\).")
    )),
    sol7 = withMathJax(tagList(
      p(tags$b("Pierwsze 30 zakładów, ponad_norma_halas:")),
      tags$ul(
        tags$li("n = 30, p empiryczne zależy od danych"),
        tags$li("95% CI: drastycznie szerszy niż dla pełnych 320"),
        tags$li("Szerokość CI ∝ \\(1/\\sqrt{n}\\): \\(\\sqrt{320}/\\sqrt{30} \\approx 3.27\\)× większa")
      ),
      p(tags$b("Uwaga:"), " pierwsze 30 zakładów to nie losowa próba —
        mogą być zakłady z jednego regionu lub branży, co wprowadza obciążenie.")
    )),
    sol8 = withMathJax(tagList(
      tags$ul(
        tags$li(tags$b("a) FAŁSZ."), " μ jest stałe. To metoda ma 95% szans wyprodukować CI zawierający μ."),
        tags$li(tags$b("b) FAŁSZ."), " To byłby PI dla pojedynczego zakładu, nie CI dla średniej."),
        tags$li(tags$b("c) PRAWDA."), " Poprawna interpretacja częstotliwościowa."),
        tags$li(tags$b("d) PRAWDA, ale trywialna."), " Średnia próby zawsze leży w środku CI."),
        tags$li(tags$b("e) PRAWDA."), " Standardowe sformułowanie ufności."),
        tags$li(tags$b("f) FAŁSZ."), " Wyższy poziom ufności → szerszy CI.")
      )
    )),
    sol9a = tagList(
      p(tags$b("95% CI dla wskaznik_wypadkow wg zmianowości:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Zmiany"), tags$th("n"), tags$th("Średnia"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("jedna"), tags$td("≤96"), tags$td("9.1"), tags$td("[8.4, 9.8]")),
          tags$tr(tags$td("dwie"), tags$td("≤144"), tags$td("10.2"), tags$td("[9.7, 10.7]")),
          tags$tr(tags$td("trzy"), tags$td("≤80"), tags$td("12.1"), tags$td("[11.3, 12.9]"))
        )
      ),
      p(tags$b("Obserwacja:"), " więcej zmian → więcej wypadków. Przedziały nie nachodzą na siebie."),
      p(tags$em("Ale czy to zmianowość sama w sobie? Przejdź do kroku B."))
    ),
    sol9b = tagList(
      p(tags$b("95% CI dla wskaznik_wypadkow wg branży:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Branża"), tags$th("n"), tags$th("Średnia"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("spożywcza"), tags$td("≤112"), tags$td("7.8"), tags$td("[7.2, 8.4]")),
          tags$tr(tags$td("metalowa"), tags$td("≤128"), tags$td("11.6"), tags$td("[11.0, 12.2]")),
          tags$tr(tags$td("chemiczna"), tags$td("≤80"), tags$td("10.3"), tags$td("[9.5, 11.1]"))
        )
      ),
      p(tags$b("Ten sam wzorzec!"), " Metalowa i chemiczna mają wyższe wskaźniki wypadków."),
      p(tags$em("Może to branża (a nie zmianowość) jest prawdziwym czynnikiem? Przejdź do kroku C."))
    ),
    sol9c = tagList(
      p(tags$b("Weryfikacja:"), " metalowe i chemiczne zakłady częściej pracują na 3 zmiany
        (ze względu na technologię procesu — ciągła produkcja)."),
      div(class = "callout-warning",
        p(tags$b("Wniosek:"), " zmianowość i wypadkowość są powiązane,
          ale czynnikiem zakłócającym jest ", tags$b("branża"),
          ". Zakłady metalowe i chemiczne są bardziej niebezpieczne z natury procesu,
          a jednocześnie częściej wymagają pracy ciągłej."),
        p("Wniosek inspekcji: poprawa BHP w metalowej/chemicznej może być skuteczniejsza
          niż skracanie zmian."),
        p(tags$em("Morał:"), " CI mierzą precyzję, ale nie zasteptą analizy mechanizmu przyczynowego.")
      )
    ),
    sol_summary = tagList(
      tags$ol(
        tags$li(tags$b("Co wpływa na CI?"),
          " Najsilniej n (zad. 3). Potem s (zad. 2). Poziom ufności — słabiej (zad. 4)."),
        tags$li(tags$b("CI w zad. 7:"),
          " n=30 to za mało; pierwsze 30 to próba nielosowa, więc CI jest szeroki i obciążony."),
        tags$li(tags$b("Zad. 9:"),
          " Branża jest zmienną zakłócającą (confounding) dla związku zmianowość–wypadki.")
      )
    )
  ),

  rol = list(
    sol1 = withMathJax(tagList(
      p(tags$b("Wyniki dla zmiennej "), tags$code("plon_pszenicy"), ":"),
      tags$ul(
        tags$li("n = 280"),
        tags$li("Średnia ≈ ", tags$b("6.17")),
        tags$li("s ≈ ", tags$b("1.20")),
        tags$li("95% CI: ", tags$b("[6.03, 6.31]"), ", ME ≈ ", tags$b("0.14"))
      ),
      p(tags$b("Interpretacja:"),
        " szacujemy, że w populacji podobnych pól średni plon pszenicy wynosi 6.0–6.3 t/ha.")
    )),
    sol2 = withMathJax(tagList(
      p(tags$b("Wyniki dla zmiennej "), tags$code("plon_rzepa"), ":"),
      tags$ul(
        tags$li("Średnia ≈ ", tags$b("4.21")),
        tags$li("s ≈ ", tags$b("0.72")),
        tags$li("95% CI: ", tags$b("[4.13, 4.29]"), ", ME ≈ ", tags$b("0.08"))
      ),
      p(tags$b("Dlaczego CI dla rzepaku jest węższy?"),
        " n = 280 takie samo, ale s(rzepa) = 0.72 < s(pszenica) = 1.20.",
        " Mniejsza zmienność plonu rzepaku → mniejsze SE → ciąśniejszy CI.")
    )),
    sol3 = withMathJax(tagList(
      p(tags$b("Wyniki dla pól klasy I"), " (klasa_gleby == \"I\"):"),
      tags$ul(
        tags$li("n ≈ 70"),
        tags$li("Średnia ≈ ", tags$b("7.12"), " (wyższa niż ogół!)"),
        tags$li("95% CI: ", tags$b("[6.89, 7.45]"), ", ME ≈ ", tags$b("0.28"))
      ),
      p(tags$b("Trzy przyczyny szerszego CI:")),
      tags$ol(
        tags$li(tags$b("Mniejsze n"), " — \\(\\sqrt{280}/\\sqrt{70} = 2\\)× większe SE."),
        tags$li(tags$b("Większy t*"), " — dla df ≈ 69: t* ≈ 2.00 vs 1.968 dla df = 279."),
        tags$li(tags$b("Możliwa inna zmienność"), " — pola klasy I mogą mieć inne s.")
      )
    )),
    sol4 = withMathJax(tagList(
      p(tags$b("CI dla plon_pszenicy przy różnych poziomach ufności"), " (n=280):"),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Poziom"), tags$th("Dolne"), tags$th("Górne"), tags$th("ME"))),
        tags$tbody(
          tags$tr(tags$td("90%"), tags$td("6.05"), tags$td("6.29"), tags$td("0.12")),
          tags$tr(tags$td("95%"), tags$td("6.03"), tags$td("6.31"), tags$td("0.14")),
          tags$tr(tags$td("99%"), tags$td("5.99"), tags$td("6.35"), tags$td("0.18"))
        )
      ),
      p("Przy normach skupu agencja rolna może preferować 95% lub 99%,
        bo konsekwencje błędnego szacunku są finansowe.")
    )),
    sol5 = withMathJax(tagList(
      p(tags$b("Wyniki dla plon_ponizej_5:"), " p = 0.154 (n=280)"),
      tags$ul(
        tags$li("Sukcesów: ~43"),
        tags$li("95% CI: ", tags$b("[0.113, 0.202]")),
        tags$li("Warunki: np ≈ 43 ≥ 10 ✓, n(1−p) ≈ 237 ≥ 10 ✓")
      ),
      p(tags$b("Interpretacja:"),
        " szacujemy, że w populacji podobnych pól 11–20% nie osiąga progu opłacalności 5 t/ha.")
    )),
    sol6 = withMathJax(tagList(
      p(tags$b("Wyniki dla wilg_powyzej_70:"), " p = 0.161 (n=280)"),
      tags$ul(
        tags$li("95% CI: ", tags$b("[0.119, 0.209]")),
        tags$li("Szerokość ≈ 0.090 vs 0.089 w zad. 5 — bardzo podobne")
      ),
      p(tags$b("Dlaczego podobna szerokość?"), " p = 0.154 i p = 0.161 są prawie takie same,
        więc wariancja \\(p(1-p)\\) jest podobna. CI będą zbliżzone.")
    )),
    sol7 = withMathJax(tagList(
      p(tags$b("Pierwsze 30 pól, plon_ponizej_5:")),
      tags$ul(
        tags$li("n = 30, p empiryczne zależy od danych"),
        tags$li("95% CI: drastycznie szerszy niż dla pełnych 280"),
        tags$li("Szerokość CI ∝ \\(1/\\sqrt{n}\\): \\(\\sqrt{280}/\\sqrt{30} \\approx 3.06\\)× większa")
      ),
      p(tags$b("Uwaga:"), " pierwsze 30 pól to nie losowa próba —
        mogą pochodzić z jednego rejonu, o podobnej klasie gleby.")
    )),
    sol8 = withMathJax(tagList(
      tags$ul(
        tags$li(tags$b("a) FAŁSZ."), " μ jest stałe. To metoda ma 95% szans wyprodukować CI zawierający μ."),
        tags$li(tags$b("b) FAŁSZ."), " To byłby PI dla pojedynczego pola, nie CI dla średniej."),
        tags$li(tags$b("c) PRAWDA."), " Poprawna interpretacja częstotliwościowa."),
        tags$li(tags$b("d) PRAWDA, ale trywialna."), " Średnia próby zawsze leży w środku CI."),
        tags$li(tags$b("e) PRAWDA."), " Standardowe sformułowanie ufności."),
        tags$li(tags$b("f) FAŁSZ."), " Wyższy poziom ufności → szerszy CI.")
      )
    )),
    sol9a = tagList(
      p(tags$b("95% CI dla plon_pszenicy wg nawożenia:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Nawożenie"), tags$th("n"), tags$th("Średnia (t/ha)"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("niskie"), tags$td("≤70"), tags$td("5.5"), tags$td("[5.2, 5.8]")),
          tags$tr(tags$td("średnie"), tags$td("≤140"), tags$td("6.3"), tags$td("[6.1, 6.5]")),
          tags$tr(tags$td("wysokie"), tags$td("≤70"), tags$td("6.9"), tags$td("[6.6, 7.2]"))
        )
      ),
      p(tags$b("Obserwacja:"), " wyższe nawożenie → wyższy plon. Przedziały nie nachodzą na siebie."),
      p(tags$em("Ale czy to nawożenie samo w sobie? Przejdź do kroku B."))
    ),
    sol9b = tagList(
      p(tags$b("95% CI dla plon_pszenicy wg klasy gleby:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Klasa gleby"), tags$th("n"), tags$th("Średnia (t/ha)"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("I"), tags$td("≤70"), tags$td("7.1"), tags$td("[6.9, 7.4]")),
          tags$tr(tags$td("II"), tags$td("≤126"), tags$td("6.1"), tags$td("[5.9, 6.3]")),
          tags$tr(tags$td("III"), tags$td("≤84"), tags$td("5.0"), tags$td("[4.8, 5.2]"))
        )
      ),
      p(tags$b("Ten sam wzorzem!"), " Lepsza klasa gleby → wyższy plon. Różnica ~2 t/ha między I a III."),
      p(tags$em("Czy rolnicy z lepszą glebą więcej nawożą? Przejdź do kroku C."))
    ),
    sol9c = tagList(
      p(tags$b("Weryfikacja:"), " pola z wysokim nawożeniem to częściej klasy I i II
        (lepsi rolnicy inwestują w obie metody, lub lepsza gleba opłaca się nawożić)."),
      div(class = "callout-warning",
        p(tags$b("Wniosek:"), " nawożenie i plon są powiązane, ale czynnikiem zakłócającym jest ",
          tags$b("klasa gleby"),
          ". Pola z lepszą glebą dają wyższy plon niezależnie od nawożenia,
          a jednocześnie są częściej intensywnie nawożone."),
        p("Aby ocenić efekt samego nawożenia, należałoby porównać pola ",
          tags$em("tej samej klasy gleby"), " z różnym nawożeniem."),
        p(tags$em("Morał:"), " CI dają precyzję, ale bez kontroli zmiennej zakłócającej
          wnioski o przyczynach mogą być błędne.")
      )
    ),
    sol_summary = tagList(
      tags$ol(
        tags$li(tags$b("Co wpływa na CI?"),
          " Najsilniej n (zad. 3). Potem s (zad. 2). Poziom ufności — słabiej (zad. 4)."),
        tags$li(tags$b("CI w zad. 7:"),
          " n=30 to za mało; pierwsze 30 pól to próba nielosowa."),
        tags$li(tags$b("Zad. 9:"),
          " Klasa gleby jest zmienną zakłócającą dla związku nawożenie–plon.")
      )
    )
  ),

  zyw = list(
    sol1 = withMathJax(tagList(
      p(tags$b("Wyniki dla zmiennej "), tags$code("zawartosc_bialka"), ":"),
      tags$ul(
        tags$li("n = 350"),
        tags$li("Średnia ≈ ", tags$b("26.71")),
        tags$li("s ≈ ", tags$b("1.32")),
        tags$li("95% CI: ", tags$b("[26.57, 26.85]"), ", ME ≈ ", tags$b("0.14"))
      ),
      p(tags$b("Interpretacja:"),
        " szacujemy, że w populacji podobnych partii średnia zawartość białka
        wynosi 26.57–26.85%.")
    )),
    sol2 = withMathJax(tagList(
      p(tags$b("Wyniki dla zmiennej "), tags$code("zawartosc_tluszczu"), ":"),
      tags$ul(
        tags$li("Średnia ≈ ", tags$b("2.781")),
        tags$li("s ≈ ", tags$b("0.244")),
        tags$li("95% CI: ", tags$b("[2.755, 2.807]"), ", ME ≈ ", tags$b("0.026"))
      ),
      p(tags$b("Dlaczego CI dla tłuszczu jest węższy?"),
        " n = 350 takie samo, ale s(tłuszcz) = 0.244 < s(białko) = 1.32.",
        " Mniejsza zmienność tłuszczu → mniejsze SE → ciąśniejszy CI.")
    )),
    sol3 = withMathJax(tagList(
      p(tags$b("Wyniki dla linii A"), " (linia == \"A\"):"),
      tags$ul(
        tags$li("n ≈ 140"),
        tags$li("Średnia ≈ ", tags$b("27.1")),
        tags$li("95% CI: ", tags$b("[26.9, 27.3]"), ", ME ≈ ", tags$b("0.20"))
      ),
      p(tags$b("Trzy przyczyny szerszego CI:")),
      tags$ol(
        tags$li(tags$b("Mniejsze n"), " — \\(\\sqrt{350}/\\sqrt{140} = 1.58\\)× większe SE."),
        tags$li(tags$b("Większy t*"), " — dla df ≈ 139: t* ≈ 1.977 vs 1.967 dla df = 349."),
        tags$li(tags$b("Możliwa inna zmienność"), " — linia A może mieć inne s.")
      )
    )),
    sol4 = withMathJax(tagList(
      p(tags$b("CI dla zawartosc_bialka przy różnych poziomach ufności"), " (n=350):"),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Poziom"), tags$th("Dolne"), tags$th("Górne"), tags$th("ME"))),
        tags$tbody(
          tags$tr(tags$td("90%"), tags$td("26.59"), tags$td("26.83"), tags$td("0.12")),
          tags$tr(tags$td("95%"), tags$td("26.57"), tags$td("26.85"), tags$td("0.14")),
          tags$tr(tags$td("99%"), tags$td("26.53"), tags$td("26.89"), tags$td("0.18"))
        )
      ),
      p("Dział jakości zwykle używa 95%; przy normach prawnych (etykiety) może być 99%.")
    )),
    sol5 = withMathJax(tagList(
      p(tags$b("Wyniki dla bialko_ponizej_normy:"), " p = 0.291 (n=350)"),
      tags$ul(
        tags$li("Sukcesów: ~102"),
        tags$li("95% CI: ", tags$b("[0.245, 0.341]")),
        tags$li("Warunki: np ≈ 102 ≥ 10 ✓, n(1−p) ≈ 248 ≥ 10 ✓")
      ),
      p(tags$b("Interpretacja:"),
        " szacujemy, że w populacji podobnych partii 24–34% nie spełnia normy białka.",
        " To poważny problem jakościowy.")
    )),
    sol6 = withMathJax(tagList(
      p(tags$b("Wyniki dla tluszcz_powyzej_normy:"), " p = 0.189 (n=350)"),
      tags$ul(
        tags$li("95% CI: ", tags$b("[0.149, 0.233]")),
        tags$li("Szerokość ≈ 0.084 vs 0.096 w zad. 5")
      ),
      p(tags$b("Dlaczego węższy?"), " p = 0.189 jest dalej od 0.5 niż p = 0.291.",
        " Dla p = 0.19: \\(p(1-p) \\approx 0.153\\), dla p = 0.29: \\(p(1-p) \\approx 0.206\\).")
    )),
    sol7 = withMathJax(tagList(
      p(tags$b("Pierwsze 30 partii, bialko_ponizej_normy:")),
      tags$ul(
        tags$li("n = 30, p empiryczne zależy od danych"),
        tags$li("95% CI: drastycznie szerszy niż dla pełnych 350"),
        tags$li("Szerokość CI ∝ \\(1/\\sqrt{n}\\): \\(\\sqrt{350}/\\sqrt{30} \\approx 3.42\\)× większa")
      ),
      p(tags$b("Uwaga:"), " pierwsze 30 partii to nie losowa próba —
        mogą pochodzić z jednej linii lub jednego dostawcy.")
    )),
    sol8 = withMathJax(tagList(
      tags$ul(
        tags$li(tags$b("a) FAŁSZ."), " μ jest stałe. To metoda ma 95% szans wyprodukować CI zawierający μ."),
        tags$li(tags$b("b) FAŁSZ."), " To byłby PI dla pojedynczej partii, nie CI dla średniej."),
        tags$li(tags$b("c) PRAWDA."), " Poprawna interpretacja częstotliwościowa."),
        tags$li(tags$b("d) PRAWDA, ale trywialna."), " Średnia próby zawsze leży w środku CI."),
        tags$li(tags$b("e) PRAWDA."), " Standardowe sformułowanie ufności."),
        tags$li(tags$b("f) FAŁSZ."), " Wyższy poziom ufności → szerszy CI.")
      )
    )),
    sol9a = tagList(
      p(tags$b("95% CI dla zawartosc_bialka wg dostawcy:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Dostawca"), tags$th("n"), tags$th("Średnia (%)"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("lokalny"), tags$td("≤123"), tags$td("26.3"), tags$td("[26.1, 26.5]")),
          tags$tr(tags$td("krajowy"), tags$td("≤158"), tags$td("26.9"), tags$td("[26.7, 27.1]")),
          tags$tr(tags$td("importowany"), tags$td("≤70"), tags$td("27.8"), tags$td("[27.5, 28.1]"))
        )
      ),
      p(tags$b("Obserwacja:"), " importowany dostawca → wyższa zawartość białka. Przedziały nie nachodzą."),
      p(tags$em("Ale czy to dostawca sam w sobie? Przejdź do kroku B."))
    ),
    sol9b = tagList(
      p(tags$b("95% CI dla zawartosc_bialka wg linii:")),
      tags$table(class = "table table-bordered table-striped",
        tags$thead(tags$tr(tags$th("Linia"), tags$th("n"), tags$th("Średnia (%)"), tags$th("95% CI"))),
        tags$tbody(
          tags$tr(tags$td("A"), tags$td("≤140"), tags$td("27.1"), tags$td("[26.9, 27.3]")),
          tags$tr(tags$td("B"), tags$td("≤123"), tags$td("26.9"), tags$td("[26.7, 27.1]")),
          tags$tr(tags$td("C"), tags$td("≤88"), tags$td("26.3"), tags$td("[26.0, 26.6]"))
        )
      ),
      p(tags$b("Ten sam wzorzec!"), " Linia A ma najwyższe białko. Linia C — najniższe."),
      p(tags$em("Czy importowany dostawca częściej zasila linię A? Przejdź do kroku C."))
    ),
    sol9c = tagList(
      p(tags$b("Weryfikacja:"), " importowany dostawca częściej zasila linię A
        (umowy kontraktowe, specyfikacje jakościowe)."),
      div(class = "callout-warning",
        p(tags$b("Wniosek:"), " dostawca i białko są powiązane, ale czynnikiem zakłócającym jest ",
          tags$b("linia produkcyjna"),
          ". Linia A ma wyższe białko ze względu na swoje parametry technologiczne,
          a jednocześnie jest częściej zasilana przez importowanego dostawcę."),
        p("Aby ocenić efekt samego dostawcy, należałoby porównać partie ",
          tags$em("tej samej linii"), " od różnych dostawców."),
        p(tags$em("Morał:"), " CI precyzyjnie mierzą różnice, ale bez kontroli zmiennej
          zakłócającej wnioski o przyczynach mogą być mylne.")
      )
    ),
    sol_summary = tagList(
      tags$ol(
        tags$li(tags$b("Co wpływa na CI?"),
          " Najsilniej n (zad. 3). Potem s (zad. 2). Poziom ufności — słabiej (zad. 4)."),
        tags$li(tags$b("CI w zad. 7:"),
          " n=30 to za mało; pierwsze 30 partii to próba nielosowa."),
        tags$li(tags$b("Zad. 9:"),
          " Linia produkcyjna jest zmienną zakłócającą dla związku dostawca–białko.")
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
      updateActionButton(session, paste0("ch7_", bid), label = "Pokaż rozwiązanie")
    }
    updateActionButton(session, "ch7_ans9a", label = "Pokaż wyniki kroku A")
    updateActionButton(session, "ch7_ans9b", label = "Pokaż wyniki kroku B")
    updateActionButton(session, "ch7_ans9c", label = "Pokaż wyniki kroku C i wnioski")
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
        label = if (nowy_stan) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
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
