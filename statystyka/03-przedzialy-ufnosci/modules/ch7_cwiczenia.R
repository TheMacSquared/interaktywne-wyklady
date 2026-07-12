# ============================================================================
# CHAPTER 7: Cwiczenia praktyczne — przedzialy ufnosci
# Pięć wariantów kierunkowych, w tym dane satelitarne i kosmiczne
# ============================================================================

# ============================================================================
# UI
# ============================================================================

ch7_ui <- list(
  id    = "ch-cwiczenia",
  num   = "07",
  title = "Ćwiczenia",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 07 · Przedziały ufności",
      num    = "07",
      title  = "Ćwiczenia praktyczne.",
      lead   = "Czas zastosować wszystko, co poznaliśmy o przedziałach
                ufności na rzeczywistych danych."
    ),

    lc_h2("ch7-cwiczenia", "Ćwiczenia praktyczne — przedziały ufności"),

    tagList(
      p(tags$b("Czas trwania:"), " ~ 90 minut · ",
        tags$b("Narzędzie:"), " Jamovi"),
      p("Trzy bloki zadań — CI dla średniej, CI dla proporcji,
        interpretacja i myślenie krytyczne. Każde zadanie ma ",
        tags$b("ukryte rozwiązanie"),
        " — kliknij przycisk, aby je zobaczyć."),

      lc_feedback(type = "info",
        selectInput("ch7_kierunek", tags$b("Wybierz wariant dla kierunku:"),
          choices = list(
            "Edukacja (CASchools)" = "edu",
            "Inżynieria Bezpieczeństwa (BHP)" = "bhp",
            "Rolnictwo" = "rol",
            "Technologia żywności" = "zyw",
            "Inżynieria danych satelitarnych i kosmicznych" = "sat"
          ),
          selected = "edu",
          width = "100%"
        )
      )
    ),

    uiOutput("ch7_content")
  )
)

# ============================================================================
# TRESC ZADAN — funkcje zwracajace tagList per kierunek
# ============================================================================

# --------------------------------------------------------------------------
# EDUKACJA (CASchools)
# --------------------------------------------------------------------------

.ch7_content_edu <- function() tagList(

  lc_feedback(type = "info",
    p(tags$b("Otwórz plik "), tags$code("dane/caschools.csv"), tags$b(" w Jamovi"), "."),
    p("Dane ze 420 okręgów szkolnych w Kalifornii. Zmienne: wyniki z czytania (",
      tags$code("read"), ") i matematyki (", tags$code("math"),
      "), stosunek uczniów do nauczycieli (",
      tags$code("students/teachers"), "), procent uczniów uczących się angielskiego (",
      tags$code("english"), "), dochód okręgu (", tags$code("income"), "), dotacje do obiadów (",
      tags$code("lunch"), "), typ szkoły (", tags$code("grades"), ").")
  ),

  lc_h3("Blok 1: Przedział ufności dla średniej (~25 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 1 — Jak czytają dzieci w Kalifornii?"),
    tagList(
      p("Kuratorium oświaty pyta: ", tags$em("„jaki jest typowy średni wynik z czytania w kalifornijskim okręgu?”"),
        " Wyznacz 95% CI dla średniej zmiennej ", tags$code("read"),
        ". Zanim klikniesz rozwiązanie: ile wynosi średnia, granice CI i co powiedzielibyś kuratorium jednym zdaniem?")
    ),
    actionButton("ch7_ans1", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol1")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 2 — A z matematyką lepiej czy gorzej?"),
    tagList(
      p("Powtórz analizę dla zmiennej ", tags$code("math"),
        ". Dlaczego ", tags$b("przedziały"), " mają różną szerokość? n jest takie samo, więc co decyduje?")
    ),
    actionButton("ch7_ans2", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol2")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 3 — CI dla każdej grupy"),
    tagList(
      p("Wyznacz CI dla ", tags$code("read"), " dla każdej z grup ", tags$code("grades"), " osobno.")
    ),
    actionButton("ch7_ans3", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol3")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 4 — Ile ufności kupujemy za szerokość?"),
    tagList(
      p("Wróć do pełnych 420 okręgów. Policz CI dla ", tags$code("read"),
        " przy poziomach ufności: 90%, 95%, 99%. Zapisz marginesy błędu i porównaj."),
      p(tags$em("Dyskusja:"), " kto żądałby 99% — statystyk akademicki czy inżynier od bezpieczeństwa lotów?")
    ),
    actionButton("ch7_ans4", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol4")
  ),

  lc_h3("Blok 2: Przedział ufności dla proporcji (~20 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 5 — Przepełnione klasy"),
    tagList(
      p("Przy stosunku students/teachers > 20 trudno o indywidualne podejście.
        Stwórz zmienną binarną i wyznacz 95% CI dla proporcji okręgów z STR > 20.
        Sprawdź warunki sensowności przed interpretacją.")
    ),
    actionButton("ch7_ans5", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol5")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 6 — Dystrykty z dużym odsetkiem English learners"),
    tagList(
      p("Okręgi z ", tags$code("english > 20%"), " są „językowo wymagające”.
        Wyznacz 95% CI dla tej proporcji i porównaj szerokość z zadaniem 5.
        Dlaczego jeden jest ciasniejszy, skoro n jest takie samo?")
    ),
    actionButton("ch7_ans6", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol6")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 7 — Czy wysoki odsetek ELL idzie w parze z wysokim odsetkiem dotacji do obiadów?"),
    tagList(
      p("Weź dwie zmienne jakościowe zbudowane z danych: ",
        tags$code("english > 20"), " (dużo uczniów uczących się angielskiego jako drugiego języka) oraz ",
        tags$code("lunch > 50"), " (dużo uczniów z dotacjami na obiady). Oszacuj 95% CI dla ",
        tags$b("różnicy proporcji"),
        " okręgów z dotacjami wśród okręgów z wysokim ELL i bez. Na tej podstawie oceń, czy można stwierdzić związek między tymi zmiennymi."),
      p(tags$em("Wskazówka:"), " zacznij od tabeli krzyżowej ",
        tags$code("table(english > 20, lunch > 50)"), ".")
    ),
    actionButton("ch7_ans7", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol7")
  ),

  lc_h3("Blok 3: Interpretacja i myślenie krytyczne (~25 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 8 — Prawda czy fałsz?"),
    tagList(
      p("Przyjmijmy, że w zadaniu 1 dostałeś 95% CI dla średniej ",
        tags$code("read"), " równy ", tags$b("[653.0, 656.9]"), ". Oceń każde stwierdzenie:"),
      tags$ol(
        tags$li("„Z prawdopodobieństwem 95% prawdziwa średnia μ leży między 653.0 a 656.9.”"),
        tags$li("„95% wszystkich okręgów ma wynik z czytania między 653.0 a 656.9.”"),
        tags$li("„Gdybyśmy powtarzali badanie, ~95% tak skonstruowanych przedziałów zawierałoby prawdziwą średnią.”"),
        tags$li("„Średnia z próby leży w przedziale [653.0, 656.9].”"),
        tags$li("„Mamy 95% ufności w metodę, która wyprodukowała ten przedział.”"),
        tags$li("„Gdybyśmy podnieśli poziom ufności do 99%, przedział zwęziłby się.”"),
        tags$li("„Z ufnością 95% możemy stwierdzić, że przeciętny wynik z czytania w populacji przekracza 650 punktów.”"),
        tags$li("„Z ufnością 95% możemy stwierdzić, że przeciętny wynik z czytania przekracza 655 punktów.”"),
        tags$li("„Z ufnością 95% możemy stwierdzić, że przeciętny wynik z czytania przekracza 660 punktów.”")
      )
    ),
    actionButton("ch7_ans8", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol8")
  ),

  br()
)

# --------------------------------------------------------------------------
# BHP
# --------------------------------------------------------------------------

.ch7_content_bhp <- function() tagList(

  lc_feedback(type = "info",
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

  lc_h3("Blok 1: Przedział ufności dla średniej (~25 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 1 — Typowy wskaźnik wypadków w polskich zakładach"),
    tagList(
      p("Inspekcja pracy chce wiedzieć: ", tags$em("„jaki jest typowy wskaźnik wypadków w polskim zakładzie produkcyjnym?”"),
        " Wyznacz 95% CI dla średniej zmiennej ", tags$code("wskaznik_wypadkow"),
        ". Przed sprawdzeniem odpowiedzi: ile wynosi średnia i granice CI?
        Jak powiedzielibyś inspekcji jednym zdaniem?")
    ),
    actionButton("ch7_ans1", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol1")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 2 — A jak wygląda absencja?"),
    tagList(
      p("Powtórz analizę dla zmiennej ", tags$code("absencja_dni"),
        ". Porównaj szerokość obu przedziałów. n jest takie samo — co decyduje o różnicy?")
    ),
    actionButton("ch7_ans2", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol2")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 3 — CI według wielkości zakładu"),
    tagList(
      p("Wyznacz CI dla ", tags$code("wskaznik_wypadkow"), " dla każdej kategorii ", tags$code("wielkosc"), " osobno.")
    ),
    actionButton("ch7_ans3", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol3")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 4 — Ile ufności kupujemy za szerokość?"),
    tagList(
      p("Wróć do pełnych 320 zakładów. Policz CI dla ", tags$code("wskaznik_wypadkow"),
        " przy 90%, 95%, 99%. Zapisz marginesy błędu."),
      p(tags$em("Dyskusja:"), " przy jakim poziomie ufności raportowałaby inspekcja pracy?")
    ),
    actionButton("ch7_ans4", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol4")
  ),

  lc_h3("Blok 2: Przedział ufności dla proporcji (~20 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 5 — Jaki odsetek zakładów przekracza normę hałasu?"),
    tagList(
      p("Zmienna ", tags$code("ponad_norma_halas"),
        " mówi, czy hałas przekracza 85 dB. Wyznacz 95% CI dla proporcji takich zakładów.
        Sprawdź warunki sensowności.")
    ),
    actionButton("ch7_ans5", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol5")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 6 — Częste naruszenia przepisów"),
    tagList(
      p("Utwórz zmienną binarną: ", tags$code("naruszen_proc > 20"),
        " (więcej niż 20% kontroli kończy się naruszeniem).
        Wyznacz 95% CI i porównaj szerokość z zadaniem 5. Dlaczego różnica?")
    ),
    actionButton("ch7_ans6", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol6")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 7 — Czy przekroczona norma hałasu idzie w parze z wysokim odsetkiem naruszeń procedur?"),
    tagList(
      p("Weź dwie zmienne jakościowe: ", tags$code("ponad_norma_halas"),
        " (hałas powyżej normy) oraz ", tags$code("naruszen_proc > 20"),
        " (wysoki odsetek naruszeń procedur). Oszacuj 95% CI dla ",
        tags$b("różnicy proporcji"),
        " zakładów z wysokimi naruszeniami wśród zakładów hałaśliwych i niehałaśliwych. Na tej podstawie oceń, czy można stwierdzić związek między tymi zmiennymi."),
      p(tags$em("Wskazówka:"), " zacznij od tabeli krzyżowej ",
        tags$code("table(ponad_norma_halas, naruszen_proc > 20)"), ".")
    ),
    actionButton("ch7_ans7", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol7")
  ),

  lc_h3("Blok 3: Interpretacja i myślenie krytyczne (~25 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 8 — Prawda czy fałsz?"),
    tagList(
      p("Przyjmijmy, że w zadaniu 1 dostałeś 95% CI dla średniej ",
        tags$code("wskaznik_wypadkow"), " równy ", tags$b("[9.33, 10.15]"),
        ". Oceń każde stwierdzenie:"),
      tags$ol(
        tags$li("„Z prawdopodobieństwem 95% prawdziwy średni wskaźnik wypadków leży między 9.33 a 10.15.”"),
        tags$li("„95% zakładów ma wskaźnik wypadków między 9.33 a 10.15.”"),
        tags$li("„Gdybyśmy powtarzali badanie, ~95% tak skonstruowanych przedziałów zawierałoby prawdziwą średnią.”"),
        tags$li("„Średnia z próby leży w przedziale [9.33, 10.15].”"),
        tags$li("„Mamy 95% ufności w metodę, która wyprodukowała ten przedział.”"),
        tags$li("„Gdybyśmy podnieśli poziom ufności do 99%, przedział zwęziłby się.”"),
        tags$li("„Z ufnością 95% możemy stwierdzić, że przeciętny wskaźnik wypadków w populacji zakładów przekracza 9 na 1000 pracowników.”"),
        tags$li("„Z ufnością 95% możemy stwierdzić, że przeciętny wskaźnik wypadków przekracza 10 na 1000 pracowników.”"),
        tags$li("„Z ufnością 95% możemy stwierdzić, że przeciętny wskaźnik wypadków przekracza 11 na 1000 pracowników.”")
      )
    ),
    actionButton("ch7_ans8", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol8")
  ),

  br()
)

# --------------------------------------------------------------------------
# ROLNICTWO
# --------------------------------------------------------------------------

.ch7_content_rol <- function() tagList(

  lc_feedback(type = "info",
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

  lc_h3("Blok 1: Przedział ufności dla średniej (~25 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 1 — Typowy plon pszenicy w Polsce"),
    tagList(
      p("Agencja rolna pyta: ", tags$em("„jaki jest typowy plon pszenicy na polskim polu uprawnym?”"),
        " Wyznacz 95% CI dla średniej zmiennej ", tags$code("plon_pszenicy"),
        ". Co powiedzielibyś agencji jednym zdaniem?")
    ),
    actionButton("ch7_ans1", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol1")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 2 — A jak wypada rzepak?"),
    tagList(
      p("Powtórz analizę dla zmiennej ", tags$code("plon_rzepa"),
        ". Porównaj szerokość obu przedziałów. n jest takie samo — co decyduje?")
    ),
    actionButton("ch7_ans2", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol2")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 3 — CI według klasy gleby"),
    tagList(
      p("Wyznacz CI dla ", tags$code("plon_pszenicy"), " dla każdej klasy gleby (", tags$code("klasa_gleby"), ") osobno.")
    ),
    actionButton("ch7_ans3", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol3")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 4 — Ile ufności kupujemy za szerokość?"),
    tagList(
      p("Wróć do pełnych 280 pól. Policz CI dla ", tags$code("plon_pszenicy"),
        " przy 90%, 95%, 99%. Zapisz marginesy błędu."),
      p(tags$em("Dyskusja:"), " przy jakim poziomie ufności raportowałaby agencja rolna?")
    ),
    actionButton("ch7_ans4", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol4")
  ),

  lc_h3("Blok 2: Przedział ufności dla proporcji (~20 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 5 — Jaki odsetek pól ma plon poniżej opłacalności?"),
    tagList(
      p("Zmienna ", tags$code("plon_ponizej_5"),
        " mówi, czy plon pszenicy wynosi mniej niż 5 t/ha (próg opłacalności).
        Wyznacz 95% CI dla proporcji takich pól. Sprawdź warunki sensowności.")
    ),
    actionButton("ch7_ans5", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol5")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 6 — Pola z nadmierną wilgotnością"),
    tagList(
      p("Zmienna ", tags$code("wilg_powyzej_70"),
        " mówi, czy wilgotność gleby przekracza 70% (ryzyko grzybów).
        Wyznacz 95% CI i porównaj szerokość z zadaniem 5. Dlaczego różnica?")
    ),
    actionButton("ch7_ans6", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol6")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 7 — Czy niski plon idzie w parze z nadmierną wilgotnością?"),
    tagList(
      p("Weź dwie zmienne jakościowe: ", tags$code("plon_ponizej_5"),
        " (plon poniżej progu opłacalności) oraz ", tags$code("wilg_powyzej_70"),
        " (nadmierna wilgotność). Oszacuj 95% CI dla ",
        tags$b("różnicy proporcji"),
        " pól z nadmierną wilgotnością wśród pól z niskim plonem i pól z plonem normalnym. Na tej podstawie oceń, czy można stwierdzić związek między tymi zmiennymi."),
      p(tags$em("Wskazówka:"), " zacznij od tabeli krzyżowej ",
        tags$code("table(plon_ponizej_5, wilg_powyzej_70)"), ".")
    ),
    actionButton("ch7_ans7", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol7")
  ),

  lc_h3("Blok 3: Interpretacja i myślenie krytyczne (~25 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 8 — Prawda czy fałsz?"),
    tagList(
      p("Przyjmijmy, że w zadaniu 1 dostałeś 95% CI dla średniej ",
        tags$code("plon_pszenicy"), " równy ", tags$b("[6.03, 6.31]"),
        ". Oceń każde stwierdzenie:"),
      tags$ol(
        tags$li("„Z prawdopodobieństwem 95% prawdziwy średni plon leży między 6.03 a 6.31.”"),
        tags$li("„95% pól ma plon między 6.03 a 6.31 t/ha.”"),
        tags$li("„Gdybyśmy powtarzali badanie, ~95% tak skonstruowanych przedziałów zawierałoby prawdziwą średnią.”"),
        tags$li("„Średnia z próby leży w przedziale [6.03, 6.31].”"),
        tags$li("„Mamy 95% ufności w metodę, która wyprodukowała ten przedział.”"),
        tags$li("„Gdybyśmy podnieśli poziom ufności do 99%, przedział zwęziłby się.”"),
        tags$li("„Z ufnością 95% możemy stwierdzić, że przeciętny plon pszenicy w populacji pól przekracza 6 t/ha.”"),
        tags$li("„Z ufnością 95% możemy stwierdzić, że przeciętny plon pszenicy przekracza 6.2 t/ha.”"),
        tags$li("„Z ufnością 95% możemy stwierdzić, że przeciętny plon pszenicy przekracza 6.5 t/ha.”")
      )
    ),
    actionButton("ch7_ans8", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol8")
  ),

  br()
)

# --------------------------------------------------------------------------
# TECHNOLOGIA ZYWNOSCI
# --------------------------------------------------------------------------

.ch7_content_zyw <- function() tagList(

  lc_feedback(type = "info",
    p(tags$b("Otwórz plik "), tags$code("dane/zywnosc_partie.csv"), tags$b(" w Jamovi"), "."),
    p("Dane z 350 partii produkcyjnych. Zmienne: zawartość białka (",
      tags$code("zawartosc_bialka"), " %), tłuszczu (",
      tags$code("zawartosc_tluszczu"), " %), linia produkcyjna (",
      tags$code("linia"), "), dostawca (",
      tags$code("dostawca"), "), białko poniżej normy (",
      tags$code("bialko_ponizej_normy"), "), tłuszcz powyżej normy (",
      tags$code("tluszcz_powyzej_normy"), ").")
  ),

  lc_h3("Blok 1: Przedział ufności dla średniej (~25 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 1 — Typowa zawartość białka w produkcie"),
    tagList(
      p("Dział jakości pyta: ", tags$em("„jaka jest typowa zawartość białka w naszych partiach?”"),
        " Wyznacz 95% CI dla średniej zmiennej ", tags$code("zawartosc_bialka"),
        ". Co powiedzielibyś działowi jakości jednym zdaniem?")
    ),
    actionButton("ch7_ans1", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol1")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 2 — A jak wygląda zawartość tłuszczu?"),
    tagList(
      p("Powtórz analizę dla zmiennej ", tags$code("zawartosc_tluszczu"),
        ". Porównaj szerokość obu przedziałów. n jest takie samo — co decyduje?")
    ),
    actionButton("ch7_ans2", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol2")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 3 — CI według linii"),
    tagList(
      p("Wyznacz CI dla ", tags$code("zawartosc_bialka"), " dla każdej linii (", tags$code("linia"), ") osobno.")
    ),
    actionButton("ch7_ans3", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol3")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 4 — Ile ufności kupujemy za szerokość?"),
    tagList(
      p("Wróć do pełnych 350 partii. Policz CI dla ", tags$code("zawartosc_bialka"),
        " przy 90%, 95%, 99%. Zapisz marginesy błędu."),
      p(tags$em("Dyskusja:"), " przy jakim poziomie ufności raportowałby dział jakości?")
    ),
    actionButton("ch7_ans4", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol4")
  ),

  lc_h3("Blok 2: Przedział ufności dla proporcji (~20 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 5 — Jaki odsetek partii nie spełnia normy białka?"),
    tagList(
      p("Zmienna ", tags$code("bialko_ponizej_normy"),
        " mówi, czy zawartość białka spada poniżej 26% (norma jakościowa).
        Wyznacz 95% CI dla proporcji takich partii. Sprawdź warunki sensowności.")
    ),
    actionButton("ch7_ans5", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol5")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 6 — Partie z za dużym tłuszczem"),
    tagList(
      p("Zmienna ", tags$code("tluszcz_powyzej_normy"),
        " mówi, czy zawartość tłuszczu przekracza 3.0% (norma).
        Wyznacz 95% CI i porównaj szerokość z zadaniem 5. Dlaczego różnica?")
    ),
    actionButton("ch7_ans6", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol6")
  ),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 7 — Czy niedobór białka idzie w parze z przekroczoną normą tłuszczu?"),
    tagList(
      p("Weź dwie zmienne jakościowe: ", tags$code("bialko_ponizej_normy"),
        " (za mało białka) oraz ", tags$code("tluszcz_powyzej_normy"),
        " (za dużo tłuszczu). Oszacuj 95% CI dla ",
        tags$b("różnicy proporcji"),
        " partii z przekroczoną normą tłuszczu wśród partii z niedoborem białka i partii z białkiem w normie. Na tej podstawie oceń, czy można stwierdzić związek między tymi zmiennymi."),
      p(tags$em("Wskazówka:"), " zacznij od tabeli krzyżowej ",
        tags$code("table(bialko_ponizej_normy, tluszcz_powyzej_normy)"), ".")
    ),
    actionButton("ch7_ans7", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol7")
  ),

  lc_h3("Blok 3: Interpretacja i myślenie krytyczne (~25 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 8 — Prawda czy fałsz?"),
    tagList(
      p("Przyjmijmy, że w zadaniu 1 dostałeś 95% CI dla średniej ",
        tags$code("zawartosc_bialka"), " równy ", tags$b("[26.57, 26.85]"),
        ". Oceń każde stwierdzenie:"),
      tags$ol(
        tags$li("„Z prawdopodobieństwem 95% prawdziwa średnia zawartość białka leży między 26.57 a 26.85.”"),
        tags$li("„95% partii ma zawartość białka między 26.57 a 26.85%.”"),
        tags$li("„Gdybyśmy powtarzali badanie, ~95% tak skonstruowanych przedziałów zawierałoby prawdziwą średnią.”"),
        tags$li("„Średnia z próby leży w przedziale [26.57, 26.85].”"),
        tags$li("„Mamy 95% ufności w metodę, która wyprodukowała ten przedział.”"),
        tags$li("„Gdybyśmy podnieśli poziom ufności do 99%, przedział zwęziłby się.”"),
        tags$li("„Z ufnością 95% możemy stwierdzić, że przeciętna zawartość białka w populacji partii przekracza 25%.”"),
        tags$li("„Z ufnością 95% możemy stwierdzić, że przeciętna zawartość białka przekracza 26.7%.”"),
        tags$li("„Z ufnością 95% możemy stwierdzić, że przeciętna zawartość białka przekracza 27%.”")
      )
    ),
    actionButton("ch7_ans8", "Pokaż rozwiązanie", class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput("ch7_sol8")
  ),

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

# Helper dla zadania 7: tabela krzyżowa 2x2 + 95% CI dla różnicy proporcji
# (Wald przez prop.test, correct = FALSE). Wnioski: kierunek zależności
# formułowany w kategoriach jednostek danego kierunku.
.ch7_sol7_tab_diff <- function(x, y, x_lab, y_lab,
                               unit_plural_gen,  # np. "okręgów", "zakładów", "pól", "partii"
                               y_event_phrase) { # np. "z dotacjami do obiadów"
  x <- as.logical(x); y <- as.logical(y)
  tab <- table(X = x, Y = y)
  k1 <- tab["TRUE",  "TRUE"]; n1 <- sum(tab["TRUE",  ])
  k2 <- tab["FALSE", "TRUE"]; n2 <- sum(tab["FALSE", ])
  pt <- prop.test(c(k1, k2), c(n1, n2), correct = FALSE)
  p1 <- k1 / n1; p2 <- k2 / n2
  diff <- p1 - p2
  ci <- pt$conf.int

  verdict <- if (ci[1] <= 0 && 0 <= ci[2]) {
    tagList(
      tags$b("Wniosek:"),
      sprintf(" 95%% CI dla różnicy proporcji zawiera 0 ([%.3f, %.3f]). Nie ma podstaw, by stwierdzić związek między %s a %s.",
              ci[1], ci[2], x_lab, y_lab)
    )
  } else if (ci[1] > 0) {
    tagList(
      tags$b("Wniosek:"),
      sprintf(" 95%% CI dla różnicy proporcji leży powyżej 0 ([%.3f, %.3f]). Odsetek %s wśród %s z „%s = TRUE” jest wyższy o %.1f–%.1f pp niż wśród pozostałych — jest związek.",
              ci[1], ci[2], y_event_phrase, unit_plural_gen, x_lab,
              100 * ci[1], 100 * ci[2])
    )
  } else {
    tagList(
      tags$b("Wniosek:"),
      sprintf(" 95%% CI dla różnicy proporcji leży poniżej 0 ([%.3f, %.3f]). Odsetek %s wśród %s z „%s = TRUE” jest niższy o %.1f–%.1f pp niż wśród pozostałych — jest związek.",
              ci[1], ci[2], y_event_phrase, unit_plural_gen, x_lab,
              -100 * ci[2], -100 * ci[1])
    )
  }

  tagList(
    p(tags$b("Tabela krzyżowa "), tags$code(x_lab), tags$b(" × "), tags$code(y_lab), tags$b(":")),
    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      tags$thead(tags$tr(
        tags$th(""),
        tags$th(sprintf("%s = FALSE", y_lab)),
        tags$th(sprintf("%s = TRUE",  y_lab)),
        tags$th("suma")
      )),
      tags$tbody(
        tags$tr(tags$td(sprintf("%s = FALSE", x_lab)),
                tags$td(tab["FALSE","FALSE"]),
                tags$td(tab["FALSE","TRUE"]),
                tags$td(n2)),
        tags$tr(tags$td(sprintf("%s = TRUE", x_lab)),
                tags$td(tab["TRUE","FALSE"]),
                tags$td(tab["TRUE","TRUE"]),
                tags$td(n1))
      )
    ),
    tags$ul(
      tags$li(sprintf("p₁ = p(%s=TRUE | %s=TRUE)  = %d/%d ≈ %.3f",
                      y_lab, x_lab, k1, n1, p1)),
      tags$li(sprintf("p₂ = p(%s=TRUE | %s=FALSE) = %d/%d ≈ %.3f",
                      y_lab, x_lab, k2, n2, p2)),
      tags$li(sprintf("Różnica proporcji p₁ − p₂ ≈ %.3f", diff)),
      tags$li(sprintf("95%% CI dla różnicy: [%.3f, %.3f]", ci[1], ci[2]))
    ),
    p(verdict)
  )
}

# ============================================================================
# ROZWIAZANIA — listy per kierunek
# ============================================================================

source(file.path(app_dir, "modules", "ch7_sat.R"), local = TRUE)

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
        tags$table(class = "lc-table lc-table-bordered lc-table-striped",
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
      d <- .ch7_data$edu
      .ch7_sol7_tab_diff(
        x = d$english > 20,
        y = d$lunch > 50,
        x_lab = "english > 20",
        y_lab = "lunch > 50",
        unit_plural_gen = "okręgów",
        y_event_phrase = "okręgów z wysokim odsetkiem dotacji do obiadów"
      )
    }),
    sol8 = withMathJax(tagList(
      tags$ul(
        tags$li(tags$b("a) FAŁSZ."), " μ jest stałe. To metoda ma 95% szans wyprodukować CI zawierający μ."),
        tags$li(tags$b("b) FAŁSZ."), " To byłby prediction interval, nie CI dla średniej."),
        tags$li(tags$b("c) PRAWDA."), " Poprawna interpretacja częstotliwościowa."),
        tags$li(tags$b("d) PRAWDA, ale trywialna."), " Średnia próby zawsze leży w środku CI z definicji."),
        tags$li(tags$b("e) PRAWDA."), " Standardowe sformułowanie ufności."),
        tags$li(tags$b("f) FAŁSZ."), " Wyższy poziom ufności → szerszy CI, nie węższy."),
        tags$li(tags$b("g) TAK."), " Cały 95% CI leży powyżej 650 — z ufnością 95% przeciętny wynik przekracza 650 punktów."),
        tags$li(tags$b("h) NIE."), " 655 leży wewnątrz CI — nie mamy podstaw, by stwierdzić, że średnia przekracza 655 (ani że jej nie przekracza)."),
        tags$li(tags$b("i) NIE."), " Cały CI leży poniżej 660 — wręcz przeciwnie, z ufnością 95% możemy stwierdzić, że przeciętny wynik ",
                tags$em("nie"), " przekracza 660.")
      )
    ))
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
        tags$table(class = "lc-table lc-table-bordered lc-table-striped",
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
      d <- .ch7_data$bhp
      .ch7_sol7_tab_diff(
        x = d$ponad_norma_halas,
        y = d$naruszen_proc > 20,
        x_lab = "ponad_norma_halas",
        y_lab = "naruszen_proc > 20",
        unit_plural_gen = "zakładów",
        y_event_phrase = "zakładów z wysokim odsetkiem naruszeń procedur"
      )
    }),
    sol8 = withMathJax(tagList(
      tags$ul(
        tags$li(tags$b("a) FAŁSZ."), " μ jest stałe. To metoda ma 95% szans wyprodukować CI zawierający μ."),
        tags$li(tags$b("b) FAŁSZ."), " To byłby PI dla pojedynczego zakładu, nie CI dla średniej."),
        tags$li(tags$b("c) PRAWDA."), " Poprawna interpretacja częstotliwościowa."),
        tags$li(tags$b("d) PRAWDA, ale trywialna."), " Średnia próby zawsze leży w środku CI."),
        tags$li(tags$b("e) PRAWDA."), " Standardowe sformułowanie ufności."),
        tags$li(tags$b("f) FAŁSZ."), " Wyższy poziom ufności → szerszy CI."),
        tags$li(tags$b("g) TAK."), " Cały 95% CI leży powyżej 9 — z ufnością 95% przeciętny wskaźnik przekracza 9."),
        tags$li(tags$b("h) NIE."), " 10 leży wewnątrz CI — nie mamy podstaw, by stwierdzić, że średnia przekracza 10 (ani że jej nie przekracza)."),
        tags$li(tags$b("i) NIE."), " Cały CI leży poniżej 11 — wręcz przeciwnie, z ufnością 95% możemy stwierdzić, że przeciętny wskaźnik ",
                tags$em("nie"), " przekracza 11.")
      )
    ))
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
        tags$table(class = "lc-table lc-table-bordered lc-table-striped",
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
      d <- .ch7_data$rol
      .ch7_sol7_tab_diff(
        x = d$plon_ponizej_5,
        y = d$wilg_powyzej_70,
        x_lab = "plon_ponizej_5",
        y_lab = "wilg_powyzej_70",
        unit_plural_gen = "pól",
        y_event_phrase = "pól z nadmierną wilgotnością"
      )
    }),
    sol8 = withMathJax(tagList(
      tags$ul(
        tags$li(tags$b("a) FAŁSZ."), " μ jest stałe. To metoda ma 95% szans wyprodukować CI zawierający μ."),
        tags$li(tags$b("b) FAŁSZ."), " To byłby PI dla pojedynczego pola, nie CI dla średniej."),
        tags$li(tags$b("c) PRAWDA."), " Poprawna interpretacja częstotliwościowa."),
        tags$li(tags$b("d) PRAWDA, ale trywialna."), " Średnia próby zawsze leży w środku CI."),
        tags$li(tags$b("e) PRAWDA."), " Standardowe sformułowanie ufności."),
        tags$li(tags$b("f) FAŁSZ."), " Wyższy poziom ufności → szerszy CI."),
        tags$li(tags$b("g) TAK."), " Cały 95% CI leży powyżej 6 — z ufnością 95% przeciętny plon przekracza 6 t/ha."),
        tags$li(tags$b("h) NIE."), " 6.2 leży wewnątrz CI — nie mamy podstaw, by stwierdzić, że średnia przekracza 6.2 (ani że jej nie przekracza)."),
        tags$li(tags$b("i) NIE."), " Cały CI leży poniżej 6.5 — wręcz przeciwnie, z ufnością 95% możemy stwierdzić, że przeciętny plon ",
                tags$em("nie"), " przekracza 6.5 t/ha.")
      )
    ))
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
        tags$table(class = "lc-table lc-table-bordered lc-table-striped",
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
      d <- .ch7_data$zyw
      .ch7_sol7_tab_diff(
        x = d$bialko_ponizej_normy,
        y = d$tluszcz_powyzej_normy,
        x_lab = "bialko_ponizej_normy",
        y_lab = "tluszcz_powyzej_normy",
        unit_plural_gen = "partii",
        y_event_phrase = "partii z przekroczoną normą tłuszczu"
      )
    }),
    sol8 = withMathJax(tagList(
      tags$ul(
        tags$li(tags$b("a) FAŁSZ."), " μ jest stałe. To metoda ma 95% szans wyprodukować CI zawierający μ."),
        tags$li(tags$b("b) FAŁSZ."), " To byłby PI dla pojedynczej partii, nie CI dla średniej."),
        tags$li(tags$b("c) PRAWDA."), " Poprawna interpretacja częstotliwościowa."),
        tags$li(tags$b("d) PRAWDA, ale trywialna."), " Średnia próby zawsze leży w środku CI."),
        tags$li(tags$b("e) PRAWDA."), " Standardowe sformułowanie ufności."),
        tags$li(tags$b("f) FAŁSZ."), " Wyższy poziom ufności → szerszy CI."),
        tags$li(tags$b("g) TAK."), " Cały 95% CI leży powyżej 25 — z ufnością 95% przeciętna zawartość białka przekracza 25%."),
        tags$li(tags$b("h) NIE."), " 26.7 leży wewnątrz CI — nie mamy podstaw, by stwierdzić, że średnia przekracza 26.7% (ani że jej nie przekracza)."),
        tags$li(tags$b("i) NIE."), " Cały CI leży poniżej 27 — wręcz przeciwnie, z ufnością 95% możemy stwierdzić, że przeciętna zawartość białka ",
                tags$em("nie"), " przekracza 27%.")
      )
    ))
  )
)

.ch7_solutions$sat <- .ch7_sat_solutions

# ============================================================================
# SERVER
# ============================================================================

ch7_server <- function(input, output, session) {

  sol_ids <- c("sol1","sol2","sol3","sol4","sol5","sol6","sol7","sol8")
  btn_ids <- c("ans1","ans2","ans3","ans4","ans5","ans6","ans7","ans8")

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
    output$ch7_content <- renderUI({
      switch(k,
        edu = .ch7_content_edu(),
        bhp = .ch7_content_bhp(),
        rol = .ch7_content_rol(),
        zyw = .ch7_content_zyw(),
        sat = .ch7_content_sat()
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
      lc_feedback(type = "ok", style = "margin-top: 10px;", sol)
    })
  }

  mapply(.make_toggle,
    sol_id_bare = sol_ids,
    sol_id_full = paste0("ch7_", sol_ids),
    btn_id_full = paste0("ch7_", btn_ids)
  )
}
