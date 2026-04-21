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
# DANE — wczytane raz przy ladowaniu modulu
# Wyniki w .ch7_solutions sa liczone inline z tych ramek, by uniknac
# rozjazdu "wartosc w kodzie vs wartosc w danych" przy regeneracji CSV.
# ============================================================================

.ch7_data <- list(
  edu = read.csv(file.path(app_dir, "dane", "caschools.csv")),
  bhp = read.csv(file.path(app_dir, "dane", "bhp_zaklady.csv")),
  rol = read.csv(file.path(app_dir, "dane", "rolnictwo_pola.csv")),
  zyw = read.csv(file.path(app_dir, "dane", "zywnosc_partie.csv"))
)

# ============================================================================
# ROZWIAZANIA — listy per kierunek
# ============================================================================

.ch7_solutions <- list(

  edu = list(
    sol1 = withMathJax({
      ci <- .ci_mean(.ch7_data$edu$read)
      tagList(
        p(tags$b("Wyniki dla zmiennej "), tags$code("read"), ":"),
        tags$ul(
          tags$li(sprintf("n = %d", ci$n)),
          tags$li("Średnia ≈ ", tags$b(.fmt_mean(ci))),
          tags$li("s ≈ ", tags$b(.fmt_sd(ci))),
          tags$li("95% CI: ", tags$b(.fmt_ci(ci))),
          tags$li("ME ≈ ", tags$b(.fmt_me(ci)))
        ),
        p(tags$b("Interpretacja:"),
          sprintf(" 95%% CI dla średniej populacji: od %.2f do %.2f.", ci$lo, ci$hi))
      )
    }),
    sol2 = withMathJax({
      ci <- .ci_mean(.ch7_data$edu$math)
      tagList(
        p(tags$b("Wyniki dla zmiennej "), tags$code("math"), ":"),
        tags$ul(
          tags$li(sprintf("n = %d", ci$n)),
          tags$li("Średnia ≈ ", tags$b(.fmt_mean(ci))),
          tags$li("s ≈ ", tags$b(.fmt_sd(ci))),
          tags$li("95% CI: ", tags$b(.fmt_ci(ci)), ", ME ≈ ", tags$b(.fmt_me(ci)))
        ),
        p(tags$b("Uwaga:"), " n identyczne jak w zad. 1, różnica w szerokości CI wynika ze zmienności s.")
      )
    }),
    sol3 = withMathJax({
      ci <- .ci_mean(.ch7_data$edu$read[.ch7_data$edu$grades == "KK-06"])
      tagList(
        p(tags$b("Wyniki dla szkół KK-06"), " (", tags$code("read"), "):"),
        tags$ul(
          tags$li(sprintf("n = %d", ci$n)),
          tags$li("Średnia ≈ ", tags$b(.fmt_mean(ci))),
          tags$li("s ≈ ", tags$b(.fmt_sd(ci))),
          tags$li("95% CI: ", tags$b(.fmt_ci(ci)), ", ME ≈ ", tags$b(.fmt_me(ci)))
        ),
        p(tags$b("Wniosek:"), " \\(SE \\propto 1/\\sqrt{n}\\) — mniejsze n → szerszy CI.")
      )
    }),
    sol4 = withMathJax({
      ci90 <- .ci_mean(.ch7_data$edu$read, level = 0.90)
      ci95 <- .ci_mean(.ch7_data$edu$read, level = 0.95)
      ci99 <- .ci_mean(.ch7_data$edu$read, level = 0.99)
      tagList(
        p(tags$b("CI dla "), tags$code("read"), tags$b(" przy różnych poziomach ufności"),
          sprintf(" (n=%d):", ci95$n)),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Poziom"), tags$th("Dolne"), tags$th("Górne"), tags$th("ME"))),
          tags$tbody(
            tags$tr(tags$td("90%"), tags$td(.fmt_mean(list(mean = ci90$lo))),
                    tags$td(.fmt_mean(list(mean = ci90$hi))), tags$td(.fmt_me(ci90))),
            tags$tr(tags$td("95%"), tags$td(.fmt_mean(list(mean = ci95$lo))),
                    tags$td(.fmt_mean(list(mean = ci95$hi))), tags$td(.fmt_me(ci95))),
            tags$tr(tags$td("99%"), tags$td(.fmt_mean(list(mean = ci99$lo))),
                    tags$td(.fmt_mean(list(mean = ci99$hi))), tags$td(.fmt_me(ci99)))
          )
        ),
        p(sprintf("ME(99%%)/ME(90%%) ≈ %.2f/%.2f ≈ ", ci99$me, ci90$me),
          tags$b(sprintf("%.2f", ci99$me / ci90$me)), ".")
      )
    }),
    sol5 = withMathJax({
      ci <- .ci_prop(.ch7_data$edu$student_teacher_ratio > 20)
      tagList(
        p(tags$b("Wyniki dla STR > 20:"),
          sprintf(" p = %d/%d ≈ ", ci$k, ci$n), tags$b(.fmt_prop(ci))),
        tags$ul(
          tags$li("95% CI: ", tags$b(sprintf("[%.3f, %.3f]", ci$lo, ci$hi))),
          tags$li(sprintf("Warunki: np = %d ≥ 10 ✓, n(1−p) = %d ≥ 10 ✓", ci$k, ci$n - ci$k))
        ),
        p(tags$b("Interpretacja:"),
          sprintf(" szacujemy, że w populacji podobnych okręgów od %.1f%% do %.1f%% miałoby STR > 20.",
                  100 * ci$lo, 100 * ci$hi))
      )
    }),
    sol6 = withMathJax({
      ci <- .ci_prop(.ch7_data$edu$english > 20)
      ci5 <- .ci_prop(.ch7_data$edu$student_teacher_ratio > 20)
      tagList(
        p(tags$b("Wyniki dla english > 20:"),
          sprintf(" p = %d/%d ≈ ", ci$k, ci$n), tags$b(.fmt_prop(ci))),
        tags$ul(
          tags$li("95% CI: ", tags$b(sprintf("[%.3f, %.3f]", ci$lo, ci$hi))),
          tags$li(sprintf("Szerokość ≈ %.3f vs %.3f w zad. 5", ci$hi - ci$lo, ci5$hi - ci5$lo))
        ),
        p(tags$b("Uwaga:"), " im dalej p od 0.5, tym mniejsza wariancja \\(p(1-p)\\) → węższy CI.")
      )
    }),
    sol7 = withMathJax({
      x <- head(.ch7_data$edu$english > 20, 25)
      k <- sum(x); n <- length(x); p <- k / n
      cp <- binom.test(k, n)$conf.int
      tagList(
        p(tags$b("Pierwsze 25 okręgów, english > 20:"),
          sprintf(" p = %d/%d ≈ ", k, n), tags$b(sprintf("%.3f", p))),
        tags$ul(
          tags$li("95% CI Clopper-Pearson: ",
                  tags$b(sprintf("[%.3f, %.3f]", cp[1], cp[2])),
                  sprintf(" — szerokość ≈ %.3f", cp[2] - cp[1]))
        ),
        p(tags$b("Uwaga:"), " pierwsze 25 to nie jest losowa próba — może być obciążona."),
        p(tags$b("Dlaczego Clopper-Pearson?"), " Przy małym n i p blisko 1 Wald jest niedokładny.")
      )
    }),
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
    sol9a = {
      d <- .ch7_data$edu
      grp <- cut(d$lunch, breaks = c(-Inf, 33, 66, Inf),
                 labels = c("mało (<33%)", "średnio (33–66%)", "dużo (>66%)"))
      rows <- lapply(levels(grp), function(lvl) {
        ci <- .ci_mean(d$read[grp == lvl])
        tags$tr(tags$td(lvl), tags$td(ci$n), tags$td(.fmt_mean(ci, 1)), tags$td(.fmt_ci(ci, 1)))
      })
      tagList(
        p(tags$b("95% CI dla średniej read wg grup lunch:")),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Grupa lunch"), tags$th("n"),
                             tags$th("Średnia read"), tags$th("95% CI"))),
          tags$tbody(rows)
        ),
        p(tags$em("Zanim wyciągniesz wnioski — przejdź do kroku B."))
      )
    },
    sol9b = {
      d <- .ch7_data$edu
      grp <- cut(d$income, breaks = c(-Inf, 10, 20, Inf),
                 labels = c("niski (<10 tys.)", "średni (10–20 tys.)", "wysoki (>20 tys.)"))
      rows <- lapply(levels(grp), function(lvl) {
        ci <- .ci_mean(d$read[grp == lvl])
        tags$tr(tags$td(lvl), tags$td(ci$n), tags$td(.fmt_mean(ci, 1)), tags$td(.fmt_ci(ci, 1)))
      })
      tagList(
        p(tags$b("95% CI dla średniej read wg grup income:")),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Grupa income"), tags$th("n"),
                             tags$th("Średnia read"), tags$th("95% CI"))),
          tags$tbody(rows)
        ),
        p(tags$em("Przejdź do kroku C."))
      )
    },
    sol9c = {
      d <- .ch7_data$edu
      grp <- cut(d$lunch, breaks = c(-Inf, 33, 66, Inf),
                 labels = c("mało (<33%)", "średnio (33–66%)", "dużo (>66%)"))
      rows <- lapply(levels(grp), function(lvl) {
        ci <- .ci_mean(d$income[grp == lvl])
        tags$tr(tags$td(lvl), tags$td(ci$n),
                tags$td(sprintf("%.2f tys. $", ci$mean)), tags$td(.fmt_ci(ci, 2)))
      })
      tagList(
        p(tags$b("95% CI dla średniej income wg grup lunch:")),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Grupa lunch"), tags$th("n"),
                             tags$th("Średni income"), tags$th("95% CI"))),
          tags$tbody(rows)
        ),
        div(class = "callout-warning",
          p(tags$b("Wniosek:"), " grupy dotacji pokrywają się z grupami zamożności — zmienna zakłócająca."),
          p(tags$em("Morał:"), " CI dają precyzję oszacowania, ale nie mówią o przyczynowości.")
        )
      )
    },
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
    sol1 = withMathJax({
      ci <- .ci_mean(.ch7_data$bhp$wskaznik_wypadkow)
      tagList(
        p(tags$b("Wyniki dla zmiennej "), tags$code("wskaznik_wypadkow"), ":"),
        tags$ul(
          tags$li(sprintf("n = %d", ci$n)),
          tags$li("Średnia ≈ ", tags$b(.fmt_mean(ci))),
          tags$li("s ≈ ", tags$b(.fmt_sd(ci))),
          tags$li("95% CI: ", tags$b(.fmt_ci(ci)), ", ME ≈ ", tags$b(.fmt_me(ci)))
        ),
        p(tags$b("Interpretacja:"),
          sprintf(" szacujemy, że w populacji podobnych zakładów średni wskaźnik wypadków wynosi od %.1f do %.1f wypadków na 1000 pracowników rocznie.",
                  ci$lo, ci$hi))
      )
    }),
    sol2 = withMathJax({
      ci <- .ci_mean(.ch7_data$bhp$absencja_dni)
      tagList(
        p(tags$b("Wyniki dla zmiennej "), tags$code("absencja_dni"), ":"),
        tags$ul(
          tags$li(sprintf("n = %d", ci$n)),
          tags$li("Średnia ≈ ", tags$b(.fmt_mean(ci))),
          tags$li("s ≈ ", tags$b(.fmt_sd(ci))),
          tags$li("95% CI: ", tags$b(.fmt_ci(ci)), ", ME ≈ ", tags$b(.fmt_me(ci)))
        ),
        p(tags$b("Uwaga:"), " n identyczne jak w zad. 1, różnica w szerokości CI wynika ze zmienności s.")
      )
    }),
    sol3 = withMathJax({
      ci <- .ci_mean(.ch7_data$bhp$wskaznik_wypadkow[.ch7_data$bhp$wielkosc == "duzy"])
      tagList(
        p(tags$b("Wyniki dla dużych zakładów"), " (wielkosc == \"duzy\"):"),
        tags$ul(
          tags$li(sprintf("n = %d", ci$n)),
          tags$li("Średnia ≈ ", tags$b(.fmt_mean(ci))),
          tags$li("s ≈ ", tags$b(.fmt_sd(ci))),
          tags$li("95% CI: ", tags$b(.fmt_ci(ci)), ", ME ≈ ", tags$b(.fmt_me(ci)))
        ),
        p(tags$b("Wniosek:"), " \\(SE \\propto 1/\\sqrt{n}\\) — mniejsze n → szerszy CI.")
      )
    }),
    sol4 = withMathJax({
      ci90 <- .ci_mean(.ch7_data$bhp$wskaznik_wypadkow, level = 0.90)
      ci95 <- .ci_mean(.ch7_data$bhp$wskaznik_wypadkow, level = 0.95)
      ci99 <- .ci_mean(.ch7_data$bhp$wskaznik_wypadkow, level = 0.99)
      tagList(
        p(tags$b("CI dla wskaznik_wypadkow przy różnych poziomach ufności"),
          sprintf(" (n=%d):", ci95$n)),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Poziom"), tags$th("Dolne"), tags$th("Górne"), tags$th("ME"))),
          tags$tbody(
            tags$tr(tags$td("90%"), tags$td(sprintf("%.2f", ci90$lo)),
                    tags$td(sprintf("%.2f", ci90$hi)), tags$td(.fmt_me(ci90))),
            tags$tr(tags$td("95%"), tags$td(sprintf("%.2f", ci95$lo)),
                    tags$td(sprintf("%.2f", ci95$hi)), tags$td(.fmt_me(ci95))),
            tags$tr(tags$td("99%"), tags$td(sprintf("%.2f", ci99$lo)),
                    tags$td(sprintf("%.2f", ci99$hi)), tags$td(.fmt_me(ci99)))
          )
        ),
        p(sprintf("ME(99%%)/ME(90%%) ≈ %.2f.", ci99$me / ci90$me))
      )
    }),
    sol5 = withMathJax({
      ci <- .ci_prop(.ch7_data$bhp$ponad_norma_halas)
      tagList(
        p(tags$b("Wyniki dla ponad_norma_halas:"),
          sprintf(" p = %d/%d ≈ ", ci$k, ci$n), tags$b(.fmt_prop(ci))),
        tags$ul(
          tags$li(sprintf("Sukcesów: %d", ci$k)),
          tags$li("95% CI: ", tags$b(sprintf("[%.3f, %.3f]", ci$lo, ci$hi))),
          tags$li(sprintf("Warunki: np = %d ≥ 10 ✓, n(1−p) = %d ≥ 10 ✓", ci$k, ci$n - ci$k))
        ),
        p(tags$b("Interpretacja:"),
          sprintf(" w populacji podobnych zakładów od %.1f%% do %.1f%% miałoby przekroczony próg hałasu.",
                  100 * ci$lo, 100 * ci$hi))
      )
    }),
    sol6 = withMathJax({
      ci <- .ci_prop(.ch7_data$bhp$naruszen_proc > 20)
      ci5 <- .ci_prop(.ch7_data$bhp$ponad_norma_halas)
      tagList(
        p(tags$b("Wyniki dla naruszen_proc > 20:"),
          sprintf(" p = %d/%d ≈ ", ci$k, ci$n), tags$b(.fmt_prop(ci))),
        tags$ul(
          tags$li("95% CI: ", tags$b(sprintf("[%.3f, %.3f]", ci$lo, ci$hi))),
          tags$li(sprintf("Szerokość ≈ %.3f vs %.3f w zad. 5", ci$hi - ci$lo, ci5$hi - ci5$lo))
        ),
        p(tags$b("Uwaga:"), " im dalej p od 0.5, tym mniejsza wariancja \\(p(1-p)\\) → węższy CI.")
      )
    }),
    sol7 = withMathJax({
      x <- head(.ch7_data$bhp$ponad_norma_halas, 30)
      k <- sum(x); n <- length(x); p <- k / n
      cp <- binom.test(k, n)$conf.int
      tagList(
        p(tags$b("Pierwsze 30 zakładów, ponad_norma_halas:"),
          sprintf(" p = %d/%d ≈ ", k, n), tags$b(sprintf("%.3f", p))),
        tags$ul(
          tags$li("95% CI Clopper-Pearson: ",
                  tags$b(sprintf("[%.3f, %.3f]", cp[1], cp[2])),
                  sprintf(" — szerokość ≈ %.3f", cp[2] - cp[1]))
        ),
        p(tags$b("Uwaga:"), " pierwsze 30 zakładów to nie losowa próba.")
      )
    }),
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
    sol9a = {
      d <- .ch7_data$bhp
      rows <- lapply(sort(unique(d$zmiany)), function(lvl) {
        ci <- .ci_mean(d$wskaznik_wypadkow[d$zmiany == lvl])
        tags$tr(tags$td(lvl), tags$td(ci$n), tags$td(.fmt_mean(ci, 1)), tags$td(.fmt_ci(ci, 1)))
      })
      tagList(
        p(tags$b("95% CI dla wskaznik_wypadkow wg zmianowości:")),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Zmiany"), tags$th("n"),
                             tags$th("Średnia"), tags$th("95% CI"))),
          tags$tbody(rows)
        ),
        p(tags$em("Przejdź do kroku B."))
      )
    },
    sol9b = {
      d <- .ch7_data$bhp
      rows <- lapply(sort(unique(d$branza)), function(lvl) {
        ci <- .ci_mean(d$wskaznik_wypadkow[d$branza == lvl])
        tags$tr(tags$td(lvl), tags$td(ci$n), tags$td(.fmt_mean(ci, 1)), tags$td(.fmt_ci(ci, 1)))
      })
      tagList(
        p(tags$b("95% CI dla wskaznik_wypadkow wg branży:")),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Branża"), tags$th("n"),
                             tags$th("Średnia"), tags$th("95% CI"))),
          tags$tbody(rows)
        ),
        p(tags$em("Przejdź do kroku C."))
      )
    },
    sol9c = {
      d <- .ch7_data$bhp
      tab <- table(d$branza, d$zmiany)
      rows <- lapply(rownames(tab), function(br) {
        tags$tr(tags$td(br),
                lapply(colnames(tab), function(zm) tags$td(tab[br, zm])))
      })
      tagList(
        p(tags$b("Tabela: branża × zmianowość (liczność):")),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Branża"),
                             lapply(colnames(tab), function(zm) tags$th(zm)))),
          tags$tbody(rows)
        ),
        div(class = "callout-warning",
          p(tags$b("Wniosek:"), " branża jest zmienną zakłócającą dla związku zmianowość–wypadki."),
          p(tags$em("Morał:"), " CI mierzą precyzję, ale nie zastępują analizy mechanizmu.")
        )
      )
    },
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
    sol1 = withMathJax({
      ci <- .ci_mean(.ch7_data$rol$plon_pszenicy)
      tagList(
        p(tags$b("Wyniki dla zmiennej "), tags$code("plon_pszenicy"), ":"),
        tags$ul(
          tags$li(sprintf("n = %d", ci$n)),
          tags$li("Średnia ≈ ", tags$b(.fmt_mean(ci))),
          tags$li("s ≈ ", tags$b(.fmt_sd(ci))),
          tags$li("95% CI: ", tags$b(.fmt_ci(ci)), ", ME ≈ ", tags$b(.fmt_me(ci)))
        ),
        p(tags$b("Interpretacja:"),
          sprintf(" 95%% CI dla średniego plonu pszenicy: od %.2f do %.2f t/ha.", ci$lo, ci$hi))
      )
    }),
    sol2 = withMathJax({
      ci <- .ci_mean(.ch7_data$rol$plon_rzepa)
      tagList(
        p(tags$b("Wyniki dla zmiennej "), tags$code("plon_rzepa"), ":"),
        tags$ul(
          tags$li(sprintf("n = %d", ci$n)),
          tags$li("Średnia ≈ ", tags$b(.fmt_mean(ci))),
          tags$li("s ≈ ", tags$b(.fmt_sd(ci))),
          tags$li("95% CI: ", tags$b(.fmt_ci(ci)), ", ME ≈ ", tags$b(.fmt_me(ci)))
        ),
        p(tags$b("Uwaga:"), " n identyczne jak w zad. 1; różnica w szerokości CI wynika ze zmienności s.")
      )
    }),
    sol3 = withMathJax({
      ci <- .ci_mean(.ch7_data$rol$plon_pszenicy[.ch7_data$rol$klasa_gleby == "I"])
      tagList(
        p(tags$b("Wyniki dla pól klasy I"), " (klasa_gleby == \"I\"):"),
        tags$ul(
          tags$li(sprintf("n = %d", ci$n)),
          tags$li("Średnia ≈ ", tags$b(.fmt_mean(ci))),
          tags$li("s ≈ ", tags$b(.fmt_sd(ci))),
          tags$li("95% CI: ", tags$b(.fmt_ci(ci)), ", ME ≈ ", tags$b(.fmt_me(ci)))
        ),
        p(tags$b("Wniosek:"), " \\(SE \\propto 1/\\sqrt{n}\\) — mniejsze n → szerszy CI.")
      )
    }),
    sol4 = withMathJax({
      ci90 <- .ci_mean(.ch7_data$rol$plon_pszenicy, level = 0.90)
      ci95 <- .ci_mean(.ch7_data$rol$plon_pszenicy, level = 0.95)
      ci99 <- .ci_mean(.ch7_data$rol$plon_pszenicy, level = 0.99)
      tagList(
        p(tags$b("CI dla plon_pszenicy przy różnych poziomach ufności"),
          sprintf(" (n=%d):", ci95$n)),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Poziom"), tags$th("Dolne"), tags$th("Górne"), tags$th("ME"))),
          tags$tbody(
            tags$tr(tags$td("90%"), tags$td(sprintf("%.2f", ci90$lo)),
                    tags$td(sprintf("%.2f", ci90$hi)), tags$td(.fmt_me(ci90))),
            tags$tr(tags$td("95%"), tags$td(sprintf("%.2f", ci95$lo)),
                    tags$td(sprintf("%.2f", ci95$hi)), tags$td(.fmt_me(ci95))),
            tags$tr(tags$td("99%"), tags$td(sprintf("%.2f", ci99$lo)),
                    tags$td(sprintf("%.2f", ci99$hi)), tags$td(.fmt_me(ci99)))
          )
        ),
        p(sprintf("ME(99%%)/ME(90%%) ≈ %.2f.", ci99$me / ci90$me))
      )
    }),
    sol5 = withMathJax({
      ci <- .ci_prop(.ch7_data$rol$plon_ponizej_5)
      tagList(
        p(tags$b("Wyniki dla plon_ponizej_5:"),
          sprintf(" p = %d/%d ≈ ", ci$k, ci$n), tags$b(.fmt_prop(ci))),
        tags$ul(
          tags$li(sprintf("Sukcesów: %d", ci$k)),
          tags$li("95% CI: ", tags$b(sprintf("[%.3f, %.3f]", ci$lo, ci$hi))),
          tags$li(sprintf("Warunki: np = %d ≥ 10 ✓, n(1−p) = %d ≥ 10 ✓", ci$k, ci$n - ci$k))
        ),
        p(tags$b("Interpretacja:"),
          sprintf(" szacujemy, że od %.1f%% do %.1f%% pól nie osiąga progu 5 t/ha.",
                  100 * ci$lo, 100 * ci$hi))
      )
    }),
    sol6 = withMathJax({
      ci <- .ci_prop(.ch7_data$rol$wilg_powyzej_70)
      ci5 <- .ci_prop(.ch7_data$rol$plon_ponizej_5)
      tagList(
        p(tags$b("Wyniki dla wilg_powyzej_70:"),
          sprintf(" p = %d/%d ≈ ", ci$k, ci$n), tags$b(.fmt_prop(ci))),
        tags$ul(
          tags$li("95% CI: ", tags$b(sprintf("[%.3f, %.3f]", ci$lo, ci$hi))),
          tags$li(sprintf("Szerokość ≈ %.3f vs %.3f w zad. 5", ci$hi - ci$lo, ci5$hi - ci5$lo))
        ),
        p(tags$b("Uwaga:"), " szerokości są podobne, bo p bliskie sobie → wariancja \\(p(1-p)\\) zbliżona.")
      )
    }),
    sol7 = withMathJax({
      x <- head(.ch7_data$rol$plon_ponizej_5, 30)
      k <- sum(x); n <- length(x); p <- k / n
      cp <- binom.test(k, n)$conf.int
      tagList(
        p(tags$b("Pierwsze 30 pól, plon_ponizej_5:"),
          sprintf(" p = %d/%d ≈ ", k, n), tags$b(sprintf("%.3f", p))),
        tags$ul(
          tags$li("95% CI Clopper-Pearson: ",
                  tags$b(sprintf("[%.3f, %.3f]", cp[1], cp[2])),
                  sprintf(" — szerokość ≈ %.3f", cp[2] - cp[1]))
        ),
        p(tags$b("Uwaga:"), " pierwsze 30 pól to nie losowa próba.")
      )
    }),
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
    sol9a = {
      d <- .ch7_data$rol
      rows <- lapply(c("niskie", "srednie", "wysokie"), function(lvl) {
        ci <- .ci_mean(d$plon_pszenicy[d$nawozenie == lvl])
        tags$tr(tags$td(lvl), tags$td(ci$n), tags$td(.fmt_mean(ci, 2)), tags$td(.fmt_ci(ci, 2)))
      })
      tagList(
        p(tags$b("95% CI dla plon_pszenicy wg nawożenia:")),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Nawożenie"), tags$th("n"),
                             tags$th("Średnia (t/ha)"), tags$th("95% CI"))),
          tags$tbody(rows)
        ),
        p(tags$em("Przejdź do kroku B."))
      )
    },
    sol9b = {
      d <- .ch7_data$rol
      rows <- lapply(c("I", "II", "III"), function(lvl) {
        ci <- .ci_mean(d$plon_pszenicy[d$klasa_gleby == lvl])
        tags$tr(tags$td(lvl), tags$td(ci$n), tags$td(.fmt_mean(ci, 2)), tags$td(.fmt_ci(ci, 2)))
      })
      tagList(
        p(tags$b("95% CI dla plon_pszenicy wg klasy gleby:")),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Klasa gleby"), tags$th("n"),
                             tags$th("Średnia (t/ha)"), tags$th("95% CI"))),
          tags$tbody(rows)
        ),
        p(tags$em("Przejdź do kroku C."))
      )
    },
    sol9c = {
      d <- .ch7_data$rol
      tab <- table(d$klasa_gleby, d$nawozenie)
      rows <- lapply(rownames(tab), function(kl) {
        tags$tr(tags$td(kl),
                lapply(colnames(tab), function(nw) tags$td(tab[kl, nw])))
      })
      tagList(
        p(tags$b("Tabela: klasa gleby × nawożenie (liczność):")),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Klasa gleby"),
                             lapply(colnames(tab), function(nw) tags$th(nw)))),
          tags$tbody(rows)
        ),
        div(class = "callout-warning",
          p(tags$b("Wniosek:"), " klasa gleby jest zmienną zakłócającą dla związku nawożenie–plon."),
          p(tags$em("Morał:"), " CI dają precyzję, ale bez kontroli zmiennej zakłócającej wnioski o przyczynach mogą być błędne.")
        )
      )
    },
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
    sol1 = withMathJax({
      ci <- .ci_mean(.ch7_data$zyw$zawartosc_bialka)
      tagList(
        p(tags$b("Wyniki dla zmiennej "), tags$code("zawartosc_bialka"), ":"),
        tags$ul(
          tags$li(sprintf("n = %d", ci$n)),
          tags$li("Średnia ≈ ", tags$b(.fmt_mean(ci))),
          tags$li("s ≈ ", tags$b(.fmt_sd(ci))),
          tags$li("95% CI: ", tags$b(.fmt_ci(ci)), ", ME ≈ ", tags$b(.fmt_me(ci)))
        ),
        p(tags$b("Interpretacja:"),
          sprintf(" 95%% CI dla średniej zawartości białka: od %.2f do %.2f%%.", ci$lo, ci$hi))
      )
    }),
    sol2 = withMathJax({
      ci <- .ci_mean(.ch7_data$zyw$zawartosc_tluszczu)
      tagList(
        p(tags$b("Wyniki dla zmiennej "), tags$code("zawartosc_tluszczu"), ":"),
        tags$ul(
          tags$li(sprintf("n = %d", ci$n)),
          tags$li("Średnia ≈ ", tags$b(.fmt_mean(ci, 3))),
          tags$li("s ≈ ", tags$b(.fmt_sd(ci, 3))),
          tags$li("95% CI: ", tags$b(.fmt_ci(ci, 3)), ", ME ≈ ", tags$b(.fmt_me(ci, 3)))
        ),
        p(tags$b("Uwaga:"), " n identyczne jak w zad. 1; różnica w szerokości CI wynika ze zmienności s.")
      )
    }),
    sol3 = withMathJax({
      ci <- .ci_mean(.ch7_data$zyw$zawartosc_bialka[.ch7_data$zyw$linia == "A"])
      tagList(
        p(tags$b("Wyniki dla linii A"), " (linia == \"A\"):"),
        tags$ul(
          tags$li(sprintf("n = %d", ci$n)),
          tags$li("Średnia ≈ ", tags$b(.fmt_mean(ci))),
          tags$li("s ≈ ", tags$b(.fmt_sd(ci))),
          tags$li("95% CI: ", tags$b(.fmt_ci(ci)), ", ME ≈ ", tags$b(.fmt_me(ci)))
        ),
        p(tags$b("Wniosek:"), " \\(SE \\propto 1/\\sqrt{n}\\) — mniejsze n → szerszy CI.")
      )
    }),
    sol4 = withMathJax({
      ci90 <- .ci_mean(.ch7_data$zyw$zawartosc_bialka, level = 0.90)
      ci95 <- .ci_mean(.ch7_data$zyw$zawartosc_bialka, level = 0.95)
      ci99 <- .ci_mean(.ch7_data$zyw$zawartosc_bialka, level = 0.99)
      tagList(
        p(tags$b("CI dla zawartosc_bialka przy różnych poziomach ufności"),
          sprintf(" (n=%d):", ci95$n)),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Poziom"), tags$th("Dolne"), tags$th("Górne"), tags$th("ME"))),
          tags$tbody(
            tags$tr(tags$td("90%"), tags$td(sprintf("%.2f", ci90$lo)),
                    tags$td(sprintf("%.2f", ci90$hi)), tags$td(.fmt_me(ci90))),
            tags$tr(tags$td("95%"), tags$td(sprintf("%.2f", ci95$lo)),
                    tags$td(sprintf("%.2f", ci95$hi)), tags$td(.fmt_me(ci95))),
            tags$tr(tags$td("99%"), tags$td(sprintf("%.2f", ci99$lo)),
                    tags$td(sprintf("%.2f", ci99$hi)), tags$td(.fmt_me(ci99)))
          )
        ),
        p(sprintf("ME(99%%)/ME(90%%) ≈ %.2f.", ci99$me / ci90$me))
      )
    }),
    sol5 = withMathJax({
      ci <- .ci_prop(.ch7_data$zyw$bialko_ponizej_normy)
      tagList(
        p(tags$b("Wyniki dla bialko_ponizej_normy:"),
          sprintf(" p = %d/%d ≈ ", ci$k, ci$n), tags$b(.fmt_prop(ci))),
        tags$ul(
          tags$li(sprintf("Sukcesów: %d", ci$k)),
          tags$li("95% CI: ", tags$b(sprintf("[%.3f, %.3f]", ci$lo, ci$hi))),
          tags$li(sprintf("Warunki: np = %d ≥ 10 ✓, n(1−p) = %d ≥ 10 ✓", ci$k, ci$n - ci$k))
        ),
        p(tags$b("Interpretacja:"),
          sprintf(" szacujemy, że od %.1f%% do %.1f%% partii nie spełnia normy białka.",
                  100 * ci$lo, 100 * ci$hi))
      )
    }),
    sol6 = withMathJax({
      ci <- .ci_prop(.ch7_data$zyw$tluszcz_powyzej_normy)
      ci5 <- .ci_prop(.ch7_data$zyw$bialko_ponizej_normy)
      tagList(
        p(tags$b("Wyniki dla tluszcz_powyzej_normy:"),
          sprintf(" p = %d/%d ≈ ", ci$k, ci$n), tags$b(.fmt_prop(ci))),
        tags$ul(
          tags$li("95% CI: ", tags$b(sprintf("[%.3f, %.3f]", ci$lo, ci$hi))),
          tags$li(sprintf("Szerokość ≈ %.3f vs %.3f w zad. 5", ci$hi - ci$lo, ci5$hi - ci5$lo))
        ),
        p(tags$b("Uwaga:"), " im dalej p od 0.5, tym mniejsza wariancja \\(p(1-p)\\) → węższy CI.")
      )
    }),
    sol7 = withMathJax({
      x <- head(.ch7_data$zyw$bialko_ponizej_normy, 30)
      k <- sum(x); n <- length(x); p <- k / n
      cp <- binom.test(k, n)$conf.int
      tagList(
        p(tags$b("Pierwsze 30 partii, bialko_ponizej_normy:"),
          sprintf(" p = %d/%d ≈ ", k, n), tags$b(sprintf("%.3f", p))),
        tags$ul(
          tags$li("95% CI Clopper-Pearson: ",
                  tags$b(sprintf("[%.3f, %.3f]", cp[1], cp[2])),
                  sprintf(" — szerokość ≈ %.3f", cp[2] - cp[1]))
        ),
        p(tags$b("Uwaga:"), " pierwsze 30 partii to nie losowa próba.")
      )
    }),
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
    sol9a = {
      d <- .ch7_data$zyw
      rows <- lapply(c("lokalny", "krajowy", "importowany"), function(lvl) {
        ci <- .ci_mean(d$zawartosc_bialka[d$dostawca == lvl])
        tags$tr(tags$td(lvl), tags$td(ci$n), tags$td(.fmt_mean(ci, 2)), tags$td(.fmt_ci(ci, 2)))
      })
      tagList(
        p(tags$b("95% CI dla zawartosc_bialka wg dostawcy:")),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Dostawca"), tags$th("n"),
                             tags$th("Średnia (%)"), tags$th("95% CI"))),
          tags$tbody(rows)
        ),
        p(tags$em("Przejdź do kroku B."))
      )
    },
    sol9b = {
      d <- .ch7_data$zyw
      rows <- lapply(c("A", "B", "C"), function(lvl) {
        ci <- .ci_mean(d$zawartosc_bialka[d$linia == lvl])
        tags$tr(tags$td(lvl), tags$td(ci$n), tags$td(.fmt_mean(ci, 2)), tags$td(.fmt_ci(ci, 2)))
      })
      tagList(
        p(tags$b("95% CI dla zawartosc_bialka wg linii:")),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Linia"), tags$th("n"),
                             tags$th("Średnia (%)"), tags$th("95% CI"))),
          tags$tbody(rows)
        ),
        p(tags$em("Przejdź do kroku C."))
      )
    },
    sol9c = {
      d <- .ch7_data$zyw
      tab <- table(d$linia, d$dostawca)
      rows <- lapply(rownames(tab), function(ln) {
        tags$tr(tags$td(ln),
                lapply(colnames(tab), function(ds) tags$td(tab[ln, ds])))
      })
      tagList(
        p(tags$b("Tabela: linia × dostawca (liczność):")),
        tags$table(class = "table table-bordered table-striped",
          tags$thead(tags$tr(tags$th("Linia"),
                             lapply(colnames(tab), function(ds) tags$th(ds)))),
          tags$tbody(rows)
        ),
        div(class = "callout-warning",
          p(tags$b("Wniosek:"), " linia produkcyjna jest zmienną zakłócającą dla związku dostawca–białko."),
          p(tags$em("Morał:"), " CI precyzyjnie mierzą różnice, ale bez kontroli zmiennej zakłócającej wnioski o przyczynach mogą być mylne.")
        )
      )
    },
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
