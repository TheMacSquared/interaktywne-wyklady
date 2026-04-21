# ============================================================================
# CHAPTER 8: Cwiczenia praktyczne — typy danych i statystyka opisowa
# Trzy warianty kierunkowe: BHP, Rolnictwo, Technologia Zywnosci
# ============================================================================

# ============================================================================
# UI
# ============================================================================

ch8_ui <- tabPanel("8. Ćwiczenia",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Poprzednio: quiz z rozpoznawania typów zmiennych"
    ),

    div(class = "section-title", "Ćwiczenia praktyczne — typy danych i statystyka opisowa"),

    div(class = "narrative",
      p(tags$b("Czas:"), " 90 minut | ", tags$b("Narzędzie:"), " Jamovi"),
      p("Trzy bloki zadań — od rozpoznawania typów zmiennych przez dobór narzędzi
        po analizę prawdziwych danych. Każde zadanie ma ",
        tags$b("ukryte rozwiązanie"), " — kliknij przycisk, aby je zobaczyć.")
    ),

    div(class = "callout-info",
      selectInput("ch8_kierunek", tags$b("Wybierz wariant dla kierunku:"),
        choices = list(
          "Inżynieria Bezpieczeństwa (BHP)" = "bhp",
          "Rolnictwo"                                = "rol",
          "Technologia żywności"            = "zyw"
        ),
        selected = "bhp",
        width = "100%"
      )
    ),

    uiOutput("ch8_content"),

    br(), br(), br()
  ))
)

# ============================================================================
# TRESC ZADAN — funkcje zwracajace tagList per kierunek
# ============================================================================

# --------------------------------------------------------------------------
# BHP
# --------------------------------------------------------------------------

.ch8_content_bhp <- function() tagList(

  div(class = "section-title", "Blok 1: Rozpoznawanie typów zmiennych (20 min)"),
  div(class = "callout-info",
    p("W Jamovi: ", tags$b("Data → Setup"), " — sprawdź, jak Jamovi automatycznie rozpoznał typy zmiennych.
      Czy ma rację? Popraw, jeśli trzeba.")
  ),

  div(class = "widget-block",
    h4("Zadanie 1 — Klasyfikacja zmiennych BHP"),
    div(class = "narrative",
      p("Otwórz plik ", tags$code("dane/bhp_zaklady.csv"), " w Jamovi.
        Dla każdej zmiennej w zbiorze określ:"),
      tags$ol(
        tags$li("Jaki to typ zmiennej? (nominalna, porządkowa, dyskretna, ciągła)"),
        tags$li("Czy Jamovi poprawnie rozpoznał typ? (sprawdź ikonę przy nazwie zmiennej)"),
        tags$li("Jakie statystyki opisowe są sensowne dla tej zmiennej?"),
        tags$li("Jaki wykres jest odpowiedni?")
      ),
      p("Wypełnij tabelę:"),
      tags$table(class = "table table-striped table-bordered",
        tags$thead(tags$tr(
          tags$th("Zmienna"), tags$th("Typ"), tags$th("Statystyki"), tags$th("Wykres")
        )),
        tags$tbody(
          tags$tr(tags$td(tags$code("branza")), tags$td("?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td(tags$code("liczba_pracownikow")), tags$td("?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td(tags$code("liczba_wypadkow")), tags$td("?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td(tags$code("kategoria_ryzyka")), tags$td("?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td(tags$code("sredni_halas_db")), tags$td("?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td(tags$code("ma_certyfikat_iso")), tags$td("?"), tags$td("?"), tags$td("?"))
        )
      )
    ),
    actionButton("ch8_ans1", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 — Pułapki typowania"),
    div(class = "narrative",
      p("Odpowiedz na pytania:"),
      tags$ol(
        tags$li("Zmienna ", tags$code("kategoria_ryzyka"), " przyjmuje wartości 1, 2, 3.
          Czy to zmienna ilościowa? Uzasadnij."),
        tags$li("Zmienna ", tags$code("ma_certyfikat_iso"), " jest zakodowana jako 0/1.
          Jaki to faktycznie typ zmiennej?"),
        tags$li("Gdyby zmienna ", tags$code("liczba_wypadkow"), " miała wartości 0–300,
          czy zmieniłoby to jej typ? A gdybyśmy ją przekształcili na kategorie:
          „mało”, „średnio”, „dużo”?"),
        tags$li("Czy średnia z ", tags$code("kategoria_ryzyka"), " (np. 1.87) ma sens?
          Co byś użył/a zamiast tego?")
      )
    ),
    actionButton("ch8_ans2", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol2")
  ),

  div(class = "section-title", "Blok 2: Dobór narzędzi statystycznych (25 min)"),

  div(class = "widget-block",
    h4("Zadanie 3 — Statystyki dla każdego typu"),
    div(class = "narrative",
      p("W Jamovi: ", tags$b("Analyses → Exploration → Descriptives"), ".
        Dla każdej zmiennej oblicz ", tags$b("odpowiednie"), " statystyki opisowe."),
      tags$ol(
        tags$li("Dla ", tags$code("branza"), ": tabela częstości i wykres słupkowy.
          Która branża dominuje?"),
        tags$li("Dla ", tags$code("kategoria_ryzyka"), ": tabela częstości skumulowanych.
          Jaki procent zakładów ma ryzyko ≤ 2?"),
        tags$li("Dla ", tags$code("liczba_wypadkow"), ": średnia, mediana, SD, IQR.
          Czy średnia i mediana są bliskie? Co to mówi o kształcie rozkładu?"),
        tags$li("Dla ", tags$code("sredni_halas_db"), ": średnia, SD, histogram.
          Jaki kształt ma rozkład?"),
        tags$li(tags$em("Refleksja:"), " Które statystyki Jamovi ",
          tags$b("pozwala"), " obliczyć, ale które są ",
          tags$b("bezsensowne"), "? Podaj przykład.")
      )
    ),
    actionButton("ch8_ans3", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 — Wykres dobierz do typu"),
    div(class = "narrative",
      p("Stwórz w Jamovi po jednym wykresie dla każdego typu zmiennej:"),
      tags$ol(
        tags$li(tags$b("Nominalna"), " (", tags$code("branza"), "): wykres słupkowy"),
        tags$li(tags$b("Porządkowa"), " (", tags$code("kategoria_ryzyka"), "): wykres słupkowy z zachowaną kolejnością"),
        tags$li(tags$b("Dyskretna"), " (", tags$code("liczba_wypadkow"), "): wykres słupkowy lub punktowy"),
        tags$li(tags$b("Ciągła"), " (", tags$code("sredni_halas_db"), "): histogram + boxplot")
      ),
      p(tags$em("Pytanie:"), " Dlaczego histogram dla ", tags$code("liczba_wypadkow"),
        " może być mylący? (Podpowiedź: ile różnych wartości ma ta zmienna?)")
    ),
    actionButton("ch8_ans4", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol4")
  ),

  div(class = "section-title", "Blok 3: Analiza kompleksowa (45 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 — Profil zakładu pracy"),
    div(class = "narrative",
      p("Przygotuj krótki raport opisowy dla zbioru ", tags$code("bhp_zaklady.csv"),
        ". Raport powinien zawierać:"),
      tags$ol(
        tags$li("Ile zakładów jest w zbiorze? Ile zmiennych?"),
        tags$li("Rozbicie branżowe: tabela częstości + wykres słupkowy"),
        tags$li("Wypadkowość: średnia, mediana, SD, min, max, histogram"),
        tags$li("Poziom hałasu: średnia, SD, odsetek zakładów powyżej normy 85 dB"),
        tags$li("Porównanie: hałas wg kategorii ryzyka (boxplot)")
      ),
      p(tags$em("Wskazówka:"), " W Jamovi możesz rozbić analizę na grupy przez ",
        tags$b("Split by"), " w Descriptives.")
    ),
    actionButton("ch8_ans5", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 — Decyzja na podstawie danych"),
    div(class = "narrative",
      p("Inspektor BHP musi zdecydować, w których branżach przeprowadzić dodatkowe kontrole.
        Na podstawie danych odpowiedz:"),
      tags$ol(
        tags$li("W której branży jest najwyższa ", tags$b("mediana"), " liczby wypadków?
          Dlaczego mediana, a nie średnia?"),
        tags$li("W której branży jest najwyższy ", tags$b("odsetek"), " zakładów z hałasem > 85 dB?"),
        tags$li("Czy istnieje związek między kategorią ryzyka a liczbą wypadków?
          (Sprawdź boxplot: ", tags$code("liczba_wypadkow"), " ~ ", tags$code("kategoria_ryzyka"), ")"),
        tags$li("Sformułuj rekomendację jednym zdaniem: która branża wymaga pilnej kontroli i dlaczego?")
      )
    ),
    actionButton("ch8_ans6", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol6")
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zakończeniu ćwiczeń odpowiedz na pytania:")),
    tags$ol(
      tags$li("Dlaczego poprawna klasyfikacja typu zmiennej jest ważna ",
              tags$em("zanim"), " zaczniemy analizę?"),
      tags$li("Podaj przykład zmiennej, którą Jamovi automatycznie źle zaklasyfikuje. Dlaczego?"),
      tags$li("Jaki jest najczęstszy błąd związany z typem zmiennej, który zaobserwowałeś/aś na zajęciach?")
    )
  ),
  actionButton("ch8_ans_summary", "Pokaż odpowiedzi", class = "btn-outline-success btn-sm"),
  uiOutput("ch8_sol_summary")
)


# --------------------------------------------------------------------------
# ROLNICTWO
# --------------------------------------------------------------------------

.ch8_content_rol <- function() tagList(

  div(class = "section-title", "Blok 1: Rozpoznawanie typów zmiennych (20 min)"),
  div(class = "callout-info",
    p("W Jamovi: ", tags$b("Data → Setup"), " — sprawdź, jak Jamovi automatycznie rozpoznał typy zmiennych.
      Czy ma rację? Popraw, jeśli trzeba.")
  ),

  div(class = "widget-block",
    h4("Zadanie 1 — Klasyfikacja zmiennych rolniczych"),
    div(class = "narrative",
      p("Otwórz plik ", tags$code("dane/rolnictwo_pola.csv"), " w Jamovi.
        Dla każdej zmiennej w zbiorze określ:"),
      tags$ol(
        tags$li("Jaki to typ zmiennej? (nominalna, porządkowa, dyskretna, ciągła)"),
        tags$li("Czy Jamovi poprawnie rozpoznał typ?"),
        tags$li("Jakie statystyki opisowe są sensowne?"),
        tags$li("Jaki wykres jest odpowiedni?")
      ),
      p("Wypełnij tabelę:"),
      tags$table(class = "table table-striped table-bordered",
        tags$thead(tags$tr(
          tags$th("Zmienna"), tags$th("Typ"), tags$th("Statystyki"), tags$th("Wykres")
        )),
        tags$tbody(
          tags$tr(tags$td(tags$code("uprawa")), tags$td("?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td(tags$code("powierzchnia_ha")), tags$td("?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td(tags$code("plon_t_ha")), tags$td("?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td(tags$code("klasa_gleby")), tags$td("?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td(tags$code("liczba_zabiegow")), tags$td("?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td(tags$code("nawozenie_organiczne")), tags$td("?"), tags$td("?"), tags$td("?"))
        )
      )
    ),
    actionButton("ch8_ans1", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 — Pułapki typowania"),
    div(class = "narrative",
      tags$ol(
        tags$li("Zmienna ", tags$code("klasa_gleby"), " przyjmuje wartości I, II, III, IV, V, VI.
          Czy to zmienna nominalna? Uzasadnij."),
        tags$li("Zmienna ", tags$code("nawozenie_organiczne"), " jest zakodowana jako 0/1.
          Jaki to faktycznie typ zmiennej?"),
        tags$li("Gdybyśmy plon przekształcili na kategorie: „niski”, „średni”, „wysoki”
          — jak zmieniłby się typ zmiennej?"),
        tags$li("Czy średnia z ", tags$code("klasa_gleby"), " (np. 3.2) ma sens?
          Co byś użył/a zamiast tego?")
      )
    ),
    actionButton("ch8_ans2", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol2")
  ),

  div(class = "section-title", "Blok 2: Dobór narzędzi statystycznych (25 min)"),

  div(class = "widget-block",
    h4("Zadanie 3 — Statystyki dla każdego typu"),
    div(class = "narrative",
      p("W Jamovi: ", tags$b("Analyses → Exploration → Descriptives"), "."),
      tags$ol(
        tags$li("Dla ", tags$code("uprawa"), ": tabela częstości i wykres słupkowy.
          Która uprawa dominuje?"),
        tags$li("Dla ", tags$code("klasa_gleby"), ": tabela częstości skumulowanych.
          Jaki procent pól ma glebę klasy I–III?"),
        tags$li("Dla ", tags$code("liczba_zabiegow"), ": średnia, mediana, SD, IQR.
          Czy rozkład jest symetryczny?"),
        tags$li("Dla ", tags$code("plon_t_ha"), ": średnia, SD, histogram.
          Jak wygląda rozkład?"),
        tags$li(tags$em("Refleksja:"), " Co się stanie, gdy policzysz średnią z ",
          tags$code("uprawa"), " w Jamovi? Czy program Ci na to pozwoli?")
      )
    ),
    actionButton("ch8_ans3", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 — Wykres dobierz do typu"),
    div(class = "narrative",
      p("Stwórz w Jamovi po jednym wykresie dla każdego typu zmiennej:"),
      tags$ol(
        tags$li(tags$b("Nominalna"), " (", tags$code("uprawa"), "): wykres słupkowy"),
        tags$li(tags$b("Porządkowa"), " (", tags$code("klasa_gleby"), "): wykres słupkowy z kolejnością"),
        tags$li(tags$b("Dyskretna"), " (", tags$code("liczba_zabiegow"), "): wykres słupkowy"),
        tags$li(tags$b("Ciągła"), " (", tags$code("plon_t_ha"), "): histogram + boxplot")
      ),
      p(tags$em("Pytanie:"), " Dlaczego wykres kołowy dla ",
        tags$code("uprawa"), " byłby złym wyborem, jeśli upraw jest 6+?")
    ),
    actionButton("ch8_ans4", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol4")
  ),

  div(class = "section-title", "Blok 3: Analiza kompleksowa (45 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 — Profil gospodarstwa"),
    div(class = "narrative",
      p("Przygotuj krótki raport opisowy dla zbioru ", tags$code("rolnictwo_pola.csv"), ":"),
      tags$ol(
        tags$li("Ile pól jest w zbiorze? Ile zmiennych?"),
        tags$li("Struktura upraw: tabela częstości + wykres słupkowy"),
        tags$li("Plonowanie: średnia, mediana, SD, min, max, histogram"),
        tags$li("Powierzchnia: średnia, SD, histogram. Czy są pola wyjątkowo duże?"),
        tags$li("Porównanie: plon wg klasy gleby (boxplot)")
      )
    ),
    actionButton("ch8_ans5", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 — Decyzja agronomiczna"),
    div(class = "narrative",
      p("Doradca rolniczy musi zdecydować, które pola wymagają interwencji. Na podstawie danych:"),
      tags$ol(
        tags$li("Która uprawa ma najniższą ", tags$b("medianę"), " plonu? Dlaczego mediana?"),
        tags$li("Która klasa gleby ma największy ", tags$b("rozrzut"), " plonów (IQR)?"),
        tags$li("Czy pola z nawozeniem organicznym mają wyższe plony?
          (Boxplot: ", tags$code("plon_t_ha"), " ~ ", tags$code("nawozenie_organiczne"), ")"),
        tags$li("Sformułuj rekomendację: które pola wymagają uwagi i dlaczego?")
      )
    ),
    actionButton("ch8_ans6", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol6")
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zakończeniu ćwiczeń odpowiedz na pytania:")),
    tags$ol(
      tags$li("Dlaczego poprawna klasyfikacja typu zmiennej jest ważna ",
              tags$em("zanim"), " zaczniemy analizę?"),
      tags$li("Podaj przykład zmiennej, którą Jamovi automatycznie źle zaklasyfikuje. Dlaczego?"),
      tags$li("Jaki jest najczęstszy błąd związany z typem zmiennej, który zaobserwowałeś/aś na zajęciach?")
    )
  ),
  actionButton("ch8_ans_summary", "Pokaż odpowiedzi", class = "btn-outline-success btn-sm"),
  uiOutput("ch8_sol_summary")
)


# --------------------------------------------------------------------------
# TECHNOLOGIA ZYWNOSCI
# --------------------------------------------------------------------------

.ch8_content_zyw <- function() tagList(

  div(class = "section-title", "Blok 1: Rozpoznawanie typów zmiennych (20 min)"),
  div(class = "callout-info",
    p("W Jamovi: ", tags$b("Data → Setup"), " — sprawdź, jak Jamovi automatycznie rozpoznał typy zmiennych.
      Czy ma rację? Popraw, jeśli trzeba.")
  ),

  div(class = "widget-block",
    h4("Zadanie 1 — Klasyfikacja zmiennych technologicznych"),
    div(class = "narrative",
      p("Otwórz plik ", tags$code("dane/zywnosc_partie.csv"), " w Jamovi.
        Dla każdej zmiennej w zbiorze określ:"),
      tags$ol(
        tags$li("Jaki to typ zmiennej?"),
        tags$li("Czy Jamovi poprawnie rozpoznał typ?"),
        tags$li("Jakie statystyki opisowe są sensowne?"),
        tags$li("Jaki wykres jest odpowiedni?")
      ),
      p("Wypełnij tabelę:"),
      tags$table(class = "table table-striped table-bordered",
        tags$thead(tags$tr(
          tags$th("Zmienna"), tags$th("Typ"), tags$th("Statystyki"), tags$th("Wykres")
        )),
        tags$tbody(
          tags$tr(tags$td(tags$code("typ_produktu")), tags$td("?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td(tags$code("masa_netto_g")), tags$td("?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td(tags$code("liczba_reklamacji")), tags$td("?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td(tags$code("klasa_jakosci")), tags$td("?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td(tags$code("zawartosc_soli_pct")), tags$td("?"), tags$td("?"), tags$td("?")),
          tags$tr(tags$td(tags$code("spelnia_norme")), tags$td("?"), tags$td("?"), tags$td("?"))
        )
      )
    ),
    actionButton("ch8_ans1", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 — Pułapki typowania"),
    div(class = "narrative",
      tags$ol(
        tags$li("Zmienna ", tags$code("klasa_jakosci"), " przyjmuje wartości Premium, Standard, Ekonomiczna.
          Czy to zmienna nominalna? Uzasadnij."),
        tags$li("Zmienna ", tags$code("spelnia_norme"), " jest zakodowana jako TAK/NIE.
          Jaki to faktycznie typ zmiennej?"),
        tags$li("Gdybyśmy zawartość soli przekształcili na kategorie:
          „niska”, „normalna”, „wysoka” — jak zmieniłby się typ zmiennej?"),
        tags$li("Czy średnia z ", tags$code("klasa_jakosci"), " ma sens? Dlaczego?")
      )
    ),
    actionButton("ch8_ans2", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol2")
  ),

  div(class = "section-title", "Blok 2: Dobór narzędzi statystycznych (25 min)"),

  div(class = "widget-block",
    h4("Zadanie 3 — Statystyki dla każdego typu"),
    div(class = "narrative",
      p("W Jamovi: ", tags$b("Analyses → Exploration → Descriptives"), "."),
      tags$ol(
        tags$li("Dla ", tags$code("typ_produktu"), ": tabela częstości + wykres słupkowy.
          Który typ dominuje?"),
        tags$li("Dla ", tags$code("klasa_jakosci"), ": tabela częstości skumulowanych.
          Jaki procent partii to klasa Premium lub Standard?"),
        tags$li("Dla ", tags$code("liczba_reklamacji"), ": średnia, mediana, SD, IQR.
          Czy rozkład jest symetryczny?"),
        tags$li("Dla ", tags$code("zawartosc_soli_pct"), ": średnia, SD, histogram.
          Jaki procent partii przekracza normę 2.5%?"),
        tags$li(tags$em("Refleksja:"), " Jakie statystyki Jamovi pozwala obliczyć dla ",
          tags$code("typ_produktu"), ", ale które są bezsensowne?")
      )
    ),
    actionButton("ch8_ans3", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 — Wykres dobierz do typu"),
    div(class = "narrative",
      p("Stwórz po jednym wykresie dla każdego typu zmiennej:"),
      tags$ol(
        tags$li(tags$b("Nominalna"), " (", tags$code("typ_produktu"), "): wykres słupkowy"),
        tags$li(tags$b("Porządkowa"), " (", tags$code("klasa_jakosci"), "): wykres słupkowy z kolejnością"),
        tags$li(tags$b("Dyskretna"), " (", tags$code("liczba_reklamacji"), "): wykres słupkowy"),
        tags$li(tags$b("Ciągła"), " (", tags$code("zawartosc_soli_pct"), "): histogram + boxplot")
      ),
      p(tags$em("Pytanie:"), " Gdybyś chciał/a porównać zawartość soli między typami produktów,
        jaki wykres byś wybrał/a?")
    ),
    actionButton("ch8_ans4", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol4")
  ),

  div(class = "section-title", "Blok 3: Analiza kompleksowa (45 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 — Profil produkcji"),
    div(class = "narrative",
      p("Przygotuj raport opisowy dla zbioru ", tags$code("zywnosc_partie.csv"), ":"),
      tags$ol(
        tags$li("Ile partii jest w zbiorze? Ile zmiennych?"),
        tags$li("Struktura produkcji: tabela częstości typów + wykres słupkowy"),
        tags$li("Jakość: rozkład klas jakości + odsetek partii spełniających normę"),
        tags$li("Masa netto: średnia, SD, histogram. Czy masy skupiają się wokół wartości nominalnej?"),
        tags$li("Porównanie: zawartość soli wg typu produktu (boxplot)")
      )
    ),
    actionButton("ch8_ans5", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 — Decyzja technologa"),
    div(class = "narrative",
      p("Technolog musi zdecydować, które linie produkcyjne wymagają korekty:"),
      tags$ol(
        tags$li("Który typ produktu ma najwyższą ", tags$b("medianę"),
          " liczby reklamacji? Dlaczego mediana, a nie średnia?"),
        tags$li("Który typ produktu ma największy ", tags$b("rozrzut"),
          " masy netto (SD lub IQR)?"),
        tags$li("Czy partie klasy Premium różnią się zawartością soli od klasy Ekonomicznej?
          (Boxplot: ", tags$code("zawartosc_soli_pct"), " ~ ", tags$code("klasa_jakosci"), ")"),
        tags$li("Sformułuj rekomendację: która linia wymaga korekty i dlaczego?")
      )
    ),
    actionButton("ch8_ans6", "Pokaż rozwiązanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol6")
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zakończeniu ćwiczeń odpowiedz na pytania:")),
    tags$ol(
      tags$li("Dlaczego poprawna klasyfikacja typu zmiennej jest ważna ",
              tags$em("zanim"), " zaczniemy analizę?"),
      tags$li("Podaj przykład zmiennej, którą Jamovi automatycznie źle zaklasyfikuje. Dlaczego?"),
      tags$li("Jaki jest najczęstszy błąd związany z typem zmiennej, który zaobserwowałeś/aś na zajęciach?")
    )
  ),
  actionButton("ch8_ans_summary", "Pokaż odpowiedzi", class = "btn-outline-success btn-sm"),
  uiOutput("ch8_sol_summary")
)


# ============================================================================
# ROZWIAZANIA
# ============================================================================

.ch8_solutions <- list(

  bhp = list(
    sol1 = tagList(
      tags$table(class = "table table-striped table-bordered",
        tags$thead(tags$tr(
          tags$th("Zmienna"), tags$th("Typ"), tags$th("Statystyki"), tags$th("Wykres")
        )),
        tags$tbody(
          tags$tr(tags$td(tags$code("branza")), tags$td("Nominalna"),
            tags$td("Moda, częstości, proporcje"), tags$td("Słupkowy")),
          tags$tr(tags$td(tags$code("liczba_pracownikow")), tags$td("Dyskretna"),
            tags$td("Średnia, mediana, SD, IQR"), tags$td("Histogram lub słupkowy")),
          tags$tr(tags$td(tags$code("liczba_wypadkow")), tags$td("Dyskretna"),
            tags$td("Średnia, mediana, SD, IQR"), tags$td("Słupkowy")),
          tags$tr(tags$td(tags$code("kategoria_ryzyka")), tags$td("Porządkowa"),
            tags$td("Moda, częstości, cz. skumulowane"), tags$td("Słupkowy (z kolejnością)")),
          tags$tr(tags$td(tags$code("sredni_halas_db")), tags$td("Ciągła"),
            tags$td("Średnia, mediana, SD, skośność"), tags$td("Histogram, boxplot")),
          tags$tr(tags$td(tags$code("ma_certyfikat_iso")), tags$td("Nominalna (binarna)"),
            tags$td("Częstości, proporcje"), tags$td("Słupkowy"))
        )
      )
    ),
    sol2 = tagList(
      tags$b("1."), " Nie — to zmienna ", tags$b("porządkowa"),
        ". Liczby 1, 2, 3 oznaczają kategorie ryzyka z porządkiem, ale różnica
        między 1 a 2 nie jest taka sama jak między 2 a 3.", tags$br(),
      tags$b("2."), " Nominalna (binarna) — 0/1 to tylko kody dla TAK/NIE,
        nie wartości liczbowe.", tags$br(),
      tags$b("3."), " Zakres 0–300 nie zmienia typu — nadal dyskretna (liczby całkowite).
        Ale po przekształceniu na „mało/średnio/dużo” staje się ",
        tags$b("porządkowa"), ".", tags$br(),
      tags$b("4."), " Nie ma sensu! Kategorie 1, 2, 3 to etykiety, nie liczby.
        Lepiej: ", tags$b("moda"), " (najczęstsza kategoria) lub tabela częstości."
    ),
    sol3 = tagList(
      tags$b("1."), " Moda = dominująca branża (np. „Produkcja”). Proporcje z tabeli częstości.", tags$br(),
      tags$b("2."), " Częstości skumulowane: np. 72% zakładów ma ryzyko ≤ 2.", tags$br(),
      tags$b("3."), " Jeśli średnia > mediana → skośność prawostronna (duże zakłady z wieloma wypadkami ciągną średnią w górę).", tags$br(),
      tags$b("4."), " Rozkład prawdopodobnie zbliżony do normalnego z lekką skośnością.", tags$br(),
      tags$b("5."), " Jamovi pozwoli obliczyć średnią z ", tags$code("kategoria_ryzyka"),
        " jeśli jest zakodowana jako liczba — ale wynik jest ", tags$b("bezsensowny"), "."
    ),
    sol4 = tagList(
      p("Histogram dla ", tags$code("liczba_wypadkow"), " może być mylący, bo zmienna dyskretna
        ma niewiele unikalnych wartości. Biny histogramu łączą sąsiednie wartości,
        co zniekształca obraz. Lepszy jest wykres słupkowy (każda wartość = osobny słupek).")
    ),
    sol5 = tagList(
      p("Raport powinien opisywać każdą zmienną narzędziami odpowiednimi do jej typu.
        Kluczowe: boxplot hałasu wg kategorii ryzyka pokaże, czy wyższe ryzyko
        koreluje z wyższym hałasem.")
    ),
    sol6 = tagList(
      tags$b("1."), " Mediana, bo rozkład wypadków jest skośny — pojedyncze zakłady z wieloma wypadkami zawyżają średnią.", tags$br(),
      tags$b("2."), " Sprawdź proporcje per branża (filtr + Descriptives).", tags$br(),
      tags$b("3."), " Boxplot pokaże, czy mediany rosną z kategorią ryzyka.", tags$br(),
      tags$b("4."), " Np. „Branża X wymaga pilnej kontroli: najwyższa mediana wypadków i najwyższy odsetek przekroczeń normy hałasu.”"
    ),
    sol_summary = tagList(
      tags$b("1."), " Bo typ zmiennej determinuje, jakie statystyki i wykresy mają sens.
        Średnia z kodów kategorii to bezsensowna liczba.", tags$br(), tags$br(),
      tags$b("2."), " Np. ", tags$code("kategoria_ryzyka"), " zakodowana jako 1/2/3 — Jamovi uzna ją za ilościową,
        a to porządkowa.", tags$br(), tags$br(),
      tags$b("3."), " Obliczanie średniej z danych porządkowych lub nominalnych (np. średnia ocen w skali Likerta traktowana jako ciągła)."
    )
  ),

  rol = list(
    sol1 = tagList(
      tags$table(class = "table table-striped table-bordered",
        tags$thead(tags$tr(
          tags$th("Zmienna"), tags$th("Typ"), tags$th("Statystyki"), tags$th("Wykres")
        )),
        tags$tbody(
          tags$tr(tags$td(tags$code("uprawa")), tags$td("Nominalna"),
            tags$td("Moda, częstości, proporcje"), tags$td("Słupkowy")),
          tags$tr(tags$td(tags$code("powierzchnia_ha")), tags$td("Ciągła"),
            tags$td("Średnia, mediana, SD, IQR"), tags$td("Histogram, boxplot")),
          tags$tr(tags$td(tags$code("plon_t_ha")), tags$td("Ciągła"),
            tags$td("Średnia, mediana, SD, skośność"), tags$td("Histogram, boxplot")),
          tags$tr(tags$td(tags$code("klasa_gleby")), tags$td("Porządkowa"),
            tags$td("Moda, częstości, cz. skumulowane"), tags$td("Słupkowy (z kolejnością)")),
          tags$tr(tags$td(tags$code("liczba_zabiegow")), tags$td("Dyskretna"),
            tags$td("Średnia, mediana, SD, IQR"), tags$td("Słupkowy")),
          tags$tr(tags$td(tags$code("nawozenie_organiczne")), tags$td("Nominalna (binarna)"),
            tags$td("Częstości, proporcje"), tags$td("Słupkowy"))
        )
      )
    ),
    sol2 = tagList(
      tags$b("1."), " Nie nominalna — to ", tags$b("porządkowa"), "! Klasy I–VI mają
        naturalny porządek (I = najlepsza), ale różnice między klasami nie są równe.", tags$br(),
      tags$b("2."), " Nominalna (binarna) — 0/1 koduje TAK/NIE.", tags$br(),
      tags$b("3."), " Z ciągłej staje się ", tags$b("porządkowa"),
        " — tracimy precyzję pomiaru, ale zyskujemy prostotę interpretacji.", tags$br(),
      tags$b("4."), " Nie ma sensu! Klasy to kategorie, nie liczby.
        Użyj mody lub tabeli częstości."
    ),
    sol3 = tagList(
      tags$b("1."), " Moda = dominująca uprawa. Proporcje z tabeli częstości.", tags$br(),
      tags$b("2."), " Częstości skumulowane: np. 45% pól ma glebę I–III.", tags$br(),
      tags$b("3."), " Jeśli średnia ≈ mediana → symetryczny. Jeśli różne → skośny.", tags$br(),
      tags$b("4."), " Typowo rozkład plonów jest zbliżony do normalnego.", tags$br(),
      tags$b("5."), " Jamovi nie pozwoli obliczyć średniej z nominalnej (jeśli poprawnie ustawiona).
        Ale jeśli zmienisz typ na Continuous — policzy bezsensowną średnią."
    ),
    sol4 = tagList(
      p("Wykres kołowy jest zły dla wielu kategorii, bo ludzkie oko nie odróżnia
        kątów różniących się o 2–3%. Wykres słupkowy pozwala dokładnie porównać długości.")
    ),
    sol5 = tagList(
      p("Kluczowe: boxplot plonu wg klasy gleby pokaże, czy lepsza gleba = wyższy plon.
        Outliery w powierzchni mogą wskazywać na duże gospodarstwa intensywne.")
    ),
    sol6 = tagList(
      tags$b("1."), " Mediana, bo plon jest często skośny (susze, gradobicia obniżają plony nielicznych pól).", tags$br(),
      tags$b("2."), " IQR wg klasy gleby — słabsze gleby mają zwykle większy rozrzut.", tags$br(),
      tags$b("3."), " Boxplot: porównaj mediany i IQR obu grup.", tags$br(),
      tags$b("4."), " Np. „Pola na glebie V–VI z niską liczbą zabiegów wymagają uwagi — najniższe plony i największy rozrzut.”"
    ),
    sol_summary = tagList(
      tags$b("1."), " Bo typ zmiennej determinuje, jakie statystyki i wykresy mają sens.", tags$br(), tags$br(),
      tags$b("2."), " Np. ", tags$code("klasa_gleby"), " zakodowana jako I–VI — Jamovi może ją potraktować
        jako nominalna (bez porządku) lub ilościowa.", tags$br(), tags$br(),
      tags$b("3."), " Obliczanie średniej z danych porządkowych (np. średnia klasy gleby = 2.8 — co to znaczy?)."
    )
  ),

  zyw = list(
    sol1 = tagList(
      tags$table(class = "table table-striped table-bordered",
        tags$thead(tags$tr(
          tags$th("Zmienna"), tags$th("Typ"), tags$th("Statystyki"), tags$th("Wykres")
        )),
        tags$tbody(
          tags$tr(tags$td(tags$code("typ_produktu")), tags$td("Nominalna"),
            tags$td("Moda, częstości, proporcje"), tags$td("Słupkowy")),
          tags$tr(tags$td(tags$code("masa_netto_g")), tags$td("Ciągła"),
            tags$td("Średnia, mediana, SD, IQR"), tags$td("Histogram, boxplot")),
          tags$tr(tags$td(tags$code("liczba_reklamacji")), tags$td("Dyskretna"),
            tags$td("Średnia, mediana, SD, IQR"), tags$td("Słupkowy")),
          tags$tr(tags$td(tags$code("klasa_jakosci")), tags$td("Porządkowa"),
            tags$td("Moda, częstości, cz. skumulowane"), tags$td("Słupkowy (z kolejnością)")),
          tags$tr(tags$td(tags$code("zawartosc_soli_pct")), tags$td("Ciągła"),
            tags$td("Średnia, mediana, SD, skośność"), tags$td("Histogram, boxplot")),
          tags$tr(tags$td(tags$code("spelnia_norme")), tags$td("Nominalna (binarna)"),
            tags$td("Częstości, proporcje"), tags$td("Słupkowy"))
        )
      )
    ),
    sol2 = tagList(
      tags$b("1."), " Nie nominalna — to ", tags$b("porządkowa"), "!
        Premium > Standard > Ekonomiczna ma naturalny porządek jakości.", tags$br(),
      tags$b("2."), " Nominalna (binarna) — TAK/NIE to dwie kategorie bez porządku.", tags$br(),
      tags$b("3."), " Z ciągłej staje się ", tags$b("porządkowa"),
        " — tracimy dokładny pomiar.", tags$br(),
      tags$b("4."), " Nie — nie można uśredniać kategorii.
        Użyj mody (najczęstsza klasa) lub tabeli częstości."
    ),
    sol3 = tagList(
      tags$b("1."), " Moda = dominujący typ produktu.", tags$br(),
      tags$b("2."), " Częstości skumulowane: np. 85% partii to Premium lub Standard.", tags$br(),
      tags$b("3."), " Liczba reklamacji — prawdopodobnie skośna prawo (większość ma mało, kilka — dużo).", tags$br(),
      tags$b("4."), " Sprawdź: odsetek partii z solą > 2.5% to np. 15%.", tags$br(),
      tags$b("5."), " Jamovi pozwoli na średnią z nominalnej, jeśli niepoprawnie ustawiona — ale wynik nie ma sensu."
    ),
    sol4 = tagList(
      p("Boxplot grupowy (", tags$code("zawartosc_soli_pct"), " ~ ", tags$code("typ_produktu"),
        ") — porównanie rozkładów jednej zmiennej ciągłej między kategoriami nominalnej.")
    ),
    sol5 = tagList(
      p("Kluczowe: boxplot zawartości soli wg typu produktu pokaże, które produkty
        mają problem z przekroczeniem normy. Masa netto powinna skupiać się wokół
        wartości nominalnej z małym SD.")
    ),
    sol6 = tagList(
      tags$b("1."), " Mediana, bo reklamacje są skośne — kilka partii z wieloma reklamacjami zawyża średnią.", tags$br(),
      tags$b("2."), " SD lub IQR masy netto — wyższy rozrzut = gorsza powtarzalność procesu.", tags$br(),
      tags$b("3."), " Boxplot pokaże, czy Premium faktycznie ma niższą sól.", tags$br(),
      tags$b("4."), " Np. „Linia produkcyjna X wymaga kalibracji wagi — najwyższy rozrzut masy netto i najwięcej reklamacji.”"
    ),
    sol_summary = tagList(
      tags$b("1."), " Bo typ zmiennej determinuje, jakie statystyki i wykresy mają sens.", tags$br(), tags$br(),
      tags$b("2."), " Np. ", tags$code("klasa_jakosci"), " — Jamovi może ją potraktować jako tekst (nominalna)
        bez uwzględnienia porządku Premium > Standard > Ekonomiczna.", tags$br(), tags$br(),
      tags$b("3."), " Obliczanie średniej z danych porządkowych lub nominalnych."
    )
  )
)


# ============================================================================
# SERVER
# ============================================================================

ch8_server <- function(input, output, session) {

  sol_ids <- c("sol1", "sol2", "sol3", "sol4", "sol5", "sol6", "sol_summary")
  btn_ids <- c("ans1", "ans2", "ans3", "ans4", "ans5", "ans6", "ans_summary")

  # Stan widocznosci
  vis <- lapply(sol_ids, function(x) reactiveVal(FALSE))
  names(vis) <- sol_ids

  # Render tresci po zmianie kierunku + reset stanow
  observeEvent(input$ch8_kierunek, {
    k <- input$ch8_kierunek

    for (sid in sol_ids) vis[[sid]](FALSE)
    for (bid in btn_ids) {
      updateActionButton(session, paste0("ch8_", bid), label = "Pokaż rozwiązanie")
    }

    output$ch8_content <- renderUI({
      switch(k,
        bhp = .ch8_content_bhp(),
        rol = .ch8_content_rol(),
        zyw = .ch8_content_zyw()
      )
    })
  }, ignoreNULL = FALSE)

  # Helper toggle
  .make_toggle <- function(sol_id_bare, sol_id_full, btn_id_full) {
    observeEvent(input[[btn_id_full]], {
      nowy_stan <- !vis[[sol_id_bare]]()
      vis[[sol_id_bare]](nowy_stan)
      updateActionButton(session, btn_id_full,
        label = if (nowy_stan) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
    }, ignoreInit = TRUE)

    output[[sol_id_full]] <- renderUI({
      if (!vis[[sol_id_bare]]()) return(NULL)
      k <- isolate(input$ch8_kierunek)
      sol <- .ch8_solutions[[k]][[sol_id_bare]]
      div(class = "callout-success", style = "margin-top: 10px;", sol)
    })
  }

  mapply(.make_toggle,
    sol_id_bare = sol_ids,
    sol_id_full = paste0("ch8_", sol_ids),
    btn_id_full = paste0("ch8_", btn_ids)
  )
}
