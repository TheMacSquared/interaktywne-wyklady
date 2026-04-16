# ============================================================================
# CHAPTER 8: Cwiczenia praktyczne — typy danych i statystyka opisowa
# Trzy warianty kierunkowe: BHP, Rolnictwo, Technologia Zywnosci
# ============================================================================

# ============================================================================
# UI
# ============================================================================

ch8_ui <- tabPanel("8. \u0106wiczenia",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Poprzednio: quiz z rozpoznawania typ\u00f3w zmiennych"
    ),

    div(class = "section-title", "\u0106wiczenia praktyczne \u2014 typy danych i statystyka opisowa"),

    div(class = "narrative",
      p(tags$b("Czas:"), " 90 minut | ", tags$b("Narz\u0119dzie:"), " Jamovi"),
      p("Trzy bloki zada\u0144 \u2014 od rozpoznawania typ\u00f3w zmiennych przez dob\u00f3r narz\u0119dzi
        po analiz\u0119 prawdziwych danych. Ka\u017cde zadanie ma ",
        tags$b("ukryte rozwi\u0105zanie"), " \u2014 kliknij przycisk, aby je zobaczy\u0107.")
    ),

    div(class = "callout-info",
      selectInput("ch8_kierunek", tags$b("Wybierz wariant dla kierunku:"),
        choices = list(
          "In\u017cynieria Bezpiecze\u0144stwa (BHP)" = "bhp",
          "Rolnictwo"                                = "rol",
          "Technologia \u017cywno\u015bci"            = "zyw"
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

  div(class = "section-title", "Blok 1: Rozpoznawanie typ\u00f3w zmiennych (20 min)"),
  div(class = "callout-info",
    p("W Jamovi: ", tags$b("Data \u2192 Setup"), " \u2014 sprawd\u017a, jak Jamovi automatycznie rozpozna\u0142 typy zmiennych.
      Czy ma racj\u0119? Popraw, je\u015bli trzeba.")
  ),

  div(class = "widget-block",
    h4("Zadanie 1 \u2014 Klasyfikacja zmiennych BHP"),
    div(class = "narrative",
      p("Otw\u00f3rz plik ", tags$code("dane/bhp_zaklady.csv"), " w Jamovi.
        Dla ka\u017cdej zmiennej w zbiorze okre\u015bl:"),
      tags$ol(
        tags$li("Jaki to typ zmiennej? (nominalna, porz\u0105dkowa, dyskretna, ci\u0105g\u0142a)"),
        tags$li("Czy Jamovi poprawnie rozpozna\u0142 typ? (sprawd\u017a ikon\u0119 przy nazwie zmiennej)"),
        tags$li("Jakie statystyki opisowe s\u0105 sensowne dla tej zmiennej?"),
        tags$li("Jaki wykres jest odpowiedni?")
      ),
      p("Wype\u0142nij tabel\u0119:"),
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
    actionButton("ch8_ans1", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 \u2014 Pu\u0142apki typowania"),
    div(class = "narrative",
      p("Odpowiedz na pytania:"),
      tags$ol(
        tags$li("Zmienna ", tags$code("kategoria_ryzyka"), " przyjmuje warto\u015bci 1, 2, 3.
          Czy to zmienna ilo\u015bciowa? Uzasadnij."),
        tags$li("Zmienna ", tags$code("ma_certyfikat_iso"), " jest zakodowana jako 0/1.
          Jaki to faktycznie typ zmiennej?"),
        tags$li("Gdyby zmienna ", tags$code("liczba_wypadkow"), " mia\u0142a warto\u015bci 0\u2013300,
          czy zmieni\u0142oby to jej typ? A gdyby\u015bmy j\u0105 przekszta\u0142cili na kategorie:
          \u201ema\u0142o\u201d, \u201e\u015brednio\u201d, \u201edu\u017co\u201d?"),
        tags$li("Czy \u015brednia z ", tags$code("kategoria_ryzyka"), " (np. 1.87) ma sens?
          Co by\u015b u\u017cy\u0142/a zamiast tego?")
      )
    ),
    actionButton("ch8_ans2", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol2")
  ),

  div(class = "section-title", "Blok 2: Dob\u00f3r narz\u0119dzi statystycznych (25 min)"),

  div(class = "widget-block",
    h4("Zadanie 3 \u2014 Statystyki dla ka\u017cdego typu"),
    div(class = "narrative",
      p("W Jamovi: ", tags$b("Analyses \u2192 Exploration \u2192 Descriptives"), ".
        Dla ka\u017cdej zmiennej oblicz ", tags$b("odpowiednie"), " statystyki opisowe."),
      tags$ol(
        tags$li("Dla ", tags$code("branza"), ": tabela cz\u0119sto\u015bci i wykres s\u0142upkowy.
          Kt\u00f3ra bran\u017ca dominuje?"),
        tags$li("Dla ", tags$code("kategoria_ryzyka"), ": tabela cz\u0119sto\u015bci skumulowanych.
          Jaki procent zak\u0142ad\u00f3w ma ryzyko \u2264 2?"),
        tags$li("Dla ", tags$code("liczba_wypadkow"), ": \u015brednia, mediana, SD, IQR.
          Czy \u015brednia i mediana s\u0105 bliskie? Co to m\u00f3wi o kszta\u0142cie rozk\u0142adu?"),
        tags$li("Dla ", tags$code("sredni_halas_db"), ": \u015brednia, SD, histogram.
          Jaki kszta\u0142t ma rozk\u0142ad?"),
        tags$li(tags$em("Refleksja:"), " Kt\u00f3re statystyki Jamovi ",
          tags$b("pozwala"), " obliczy\u0107, ale kt\u00f3re s\u0105 ",
          tags$b("bezsensowne"), "? Podaj przyk\u0142ad.")
      )
    ),
    actionButton("ch8_ans3", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 \u2014 Wykres dobierz do typu"),
    div(class = "narrative",
      p("Stw\u00f3rz w Jamovi po jednym wykresie dla ka\u017cdego typu zmiennej:"),
      tags$ol(
        tags$li(tags$b("Nominalna"), " (", tags$code("branza"), "): wykres s\u0142upkowy"),
        tags$li(tags$b("Porz\u0105dkowa"), " (", tags$code("kategoria_ryzyka"), "): wykres s\u0142upkowy z zachowan\u0105 kolejno\u015bci\u0105"),
        tags$li(tags$b("Dyskretna"), " (", tags$code("liczba_wypadkow"), "): wykres s\u0142upkowy lub punktowy"),
        tags$li(tags$b("Ci\u0105g\u0142a"), " (", tags$code("sredni_halas_db"), "): histogram + boxplot")
      ),
      p(tags$em("Pytanie:"), " Dlaczego histogram dla ", tags$code("liczba_wypadkow"),
        " mo\u017ce by\u0107 mylący? (Podpowied\u017a: ile r\u00f3\u017cnych warto\u015bci ma ta zmienna?)")
    ),
    actionButton("ch8_ans4", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol4")
  ),

  div(class = "section-title", "Blok 3: Analiza kompleksowa (45 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 \u2014 Profil zak\u0142adu pracy"),
    div(class = "narrative",
      p("Przygotuj kr\u00f3tki raport opisowy dla zbioru ", tags$code("bhp_zaklady.csv"),
        ". Raport powinien zawiera\u0107:"),
      tags$ol(
        tags$li("Ile zak\u0142ad\u00f3w jest w zbiorze? Ile zmiennych?"),
        tags$li("Rozbicie bran\u017cowe: tabela cz\u0119sto\u015bci + wykres s\u0142upkowy"),
        tags$li("Wypadkowo\u015b\u0107: \u015brednia, mediana, SD, min, max, histogram"),
        tags$li("Poziom ha\u0142asu: \u015brednia, SD, odsetek zak\u0142ad\u00f3w powy\u017cej normy 85 dB"),
        tags$li("Por\u00f3wnanie: ha\u0142as wg kategorii ryzyka (boxplot)")
      ),
      p(tags$em("Wskaz\u00f3wka:"), " W Jamovi mo\u017cesz rozbi\u0107 analiz\u0119 na grupy przez ",
        tags$b("Split by"), " w Descriptives.")
    ),
    actionButton("ch8_ans5", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 \u2014 Decyzja na podstawie danych"),
    div(class = "narrative",
      p("Inspektor BHP musi zdecydowa\u0107, w kt\u00f3rych bran\u017cach przeprowadzi\u0107 dodatkowe kontrole.
        Na podstawie danych odpowiedz:"),
      tags$ol(
        tags$li("W kt\u00f3rej bran\u017cy jest najwy\u017csza ", tags$b("mediana"), " liczby wypadk\u00f3w?
          Dlaczego mediana, a nie \u015brednia?"),
        tags$li("W kt\u00f3rej bran\u017cy jest najwy\u017cszy ", tags$b("odsetek"), " zak\u0142ad\u00f3w z ha\u0142asem > 85 dB?"),
        tags$li("Czy istnieje zwi\u0105zek mi\u0119dzy kategori\u0105 ryzyka a liczb\u0105 wypadk\u00f3w?
          (Sprawd\u017a boxplot: ", tags$code("liczba_wypadkow"), " ~ ", tags$code("kategoria_ryzyka"), ")"),
        tags$li("Sformu\u0142uj rekomendacj\u0119 jednym zdaniem: kt\u00f3ra bran\u017ca wymaga pilnej kontroli i dlaczego?")
      )
    ),
    actionButton("ch8_ans6", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol6")
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zako\u0144czeniu \u0107wicze\u0144 odpowiedz na pytania:")),
    tags$ol(
      tags$li("Dlaczego poprawna klasyfikacja typu zmiennej jest wa\u017cna ",
              tags$em("zanim"), " zaczniemy analiz\u0119?"),
      tags$li("Podaj przyk\u0142ad zmiennej, kt\u00f3r\u0105 Jamovi automatycznie \u017ale zaklasyfikuje. Dlaczego?"),
      tags$li("Jaki jest najcz\u0119stszy b\u0142\u0105d zwi\u0105zany z typem zmiennej, kt\u00f3ry zaobserwowa\u0142e\u015b/a\u015b na zaj\u0119ciach?")
    )
  ),
  actionButton("ch8_ans_summary", "Poka\u017c odpowiedzi", class = "btn-outline-success btn-sm"),
  uiOutput("ch8_sol_summary")
)


# --------------------------------------------------------------------------
# ROLNICTWO
# --------------------------------------------------------------------------

.ch8_content_rol <- function() tagList(

  div(class = "section-title", "Blok 1: Rozpoznawanie typ\u00f3w zmiennych (20 min)"),
  div(class = "callout-info",
    p("W Jamovi: ", tags$b("Data \u2192 Setup"), " \u2014 sprawd\u017a, jak Jamovi automatycznie rozpozna\u0142 typy zmiennych.
      Czy ma racj\u0119? Popraw, je\u015bli trzeba.")
  ),

  div(class = "widget-block",
    h4("Zadanie 1 \u2014 Klasyfikacja zmiennych rolniczych"),
    div(class = "narrative",
      p("Otw\u00f3rz plik ", tags$code("dane/rolnictwo_pola.csv"), " w Jamovi.
        Dla ka\u017cdej zmiennej w zbiorze okre\u015bl:"),
      tags$ol(
        tags$li("Jaki to typ zmiennej? (nominalna, porz\u0105dkowa, dyskretna, ci\u0105g\u0142a)"),
        tags$li("Czy Jamovi poprawnie rozpozna\u0142 typ?"),
        tags$li("Jakie statystyki opisowe s\u0105 sensowne?"),
        tags$li("Jaki wykres jest odpowiedni?")
      ),
      p("Wype\u0142nij tabel\u0119:"),
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
    actionButton("ch8_ans1", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 \u2014 Pu\u0142apki typowania"),
    div(class = "narrative",
      tags$ol(
        tags$li("Zmienna ", tags$code("klasa_gleby"), " przyjmuje warto\u015bci I, II, III, IV, V, VI.
          Czy to zmienna nominalna? Uzasadnij."),
        tags$li("Zmienna ", tags$code("nawozenie_organiczne"), " jest zakodowana jako 0/1.
          Jaki to faktycznie typ zmiennej?"),
        tags$li("Gdyby\u015bmy plon przekszta\u0142cili na kategorie: \u201eniski\u201d, \u201e\u015bredni\u201d, \u201ewysoki\u201d
          \u2014 jak zmieni\u0142by si\u0119 typ zmiennej?"),
        tags$li("Czy \u015brednia z ", tags$code("klasa_gleby"), " (np. 3.2) ma sens?
          Co by\u015b u\u017cy\u0142/a zamiast tego?")
      )
    ),
    actionButton("ch8_ans2", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol2")
  ),

  div(class = "section-title", "Blok 2: Dob\u00f3r narz\u0119dzi statystycznych (25 min)"),

  div(class = "widget-block",
    h4("Zadanie 3 \u2014 Statystyki dla ka\u017cdego typu"),
    div(class = "narrative",
      p("W Jamovi: ", tags$b("Analyses \u2192 Exploration \u2192 Descriptives"), "."),
      tags$ol(
        tags$li("Dla ", tags$code("uprawa"), ": tabela cz\u0119sto\u015bci i wykres s\u0142upkowy.
          Kt\u00f3ra uprawa dominuje?"),
        tags$li("Dla ", tags$code("klasa_gleby"), ": tabela cz\u0119sto\u015bci skumulowanych.
          Jaki procent p\u00f3l ma gleb\u0119 klasy I\u2013III?"),
        tags$li("Dla ", tags$code("liczba_zabiegow"), ": \u015brednia, mediana, SD, IQR.
          Czy rozk\u0142ad jest symetryczny?"),
        tags$li("Dla ", tags$code("plon_t_ha"), ": \u015brednia, SD, histogram.
          Jak wygl\u0105da rozk\u0142ad?"),
        tags$li(tags$em("Refleksja:"), " Co si\u0119 stanie, gdy policzysz \u015bredni\u0105 z ",
          tags$code("uprawa"), " w Jamovi? Czy program Ci na to pozwoli?")
      )
    ),
    actionButton("ch8_ans3", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 \u2014 Wykres dobierz do typu"),
    div(class = "narrative",
      p("Stw\u00f3rz w Jamovi po jednym wykresie dla ka\u017cdego typu zmiennej:"),
      tags$ol(
        tags$li(tags$b("Nominalna"), " (", tags$code("uprawa"), "): wykres s\u0142upkowy"),
        tags$li(tags$b("Porz\u0105dkowa"), " (", tags$code("klasa_gleby"), "): wykres s\u0142upkowy z kolejno\u015bci\u0105"),
        tags$li(tags$b("Dyskretna"), " (", tags$code("liczba_zabiegow"), "): wykres s\u0142upkowy"),
        tags$li(tags$b("Ci\u0105g\u0142a"), " (", tags$code("plon_t_ha"), "): histogram + boxplot")
      ),
      p(tags$em("Pytanie:"), " Dlaczego wykres ko\u0142owy dla ",
        tags$code("uprawa"), " by\u0142by z\u0142ym wyborem, je\u015bli upraw jest 6+?")
    ),
    actionButton("ch8_ans4", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol4")
  ),

  div(class = "section-title", "Blok 3: Analiza kompleksowa (45 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 \u2014 Profil gospodarstwa"),
    div(class = "narrative",
      p("Przygotuj kr\u00f3tki raport opisowy dla zbioru ", tags$code("rolnictwo_pola.csv"), ":"),
      tags$ol(
        tags$li("Ile p\u00f3l jest w zbiorze? Ile zmiennych?"),
        tags$li("Struktura upraw: tabela cz\u0119sto\u015bci + wykres s\u0142upkowy"),
        tags$li("Plonowanie: \u015brednia, mediana, SD, min, max, histogram"),
        tags$li("Powierzchnia: \u015brednia, SD, histogram. Czy s\u0105 pola wyj\u0105tkowo du\u017ce?"),
        tags$li("Por\u00f3wnanie: plon wg klasy gleby (boxplot)")
      )
    ),
    actionButton("ch8_ans5", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 \u2014 Decyzja agronomiczna"),
    div(class = "narrative",
      p("Doradca rolniczy musi zdecydowa\u0107, kt\u00f3re pola wymagaj\u0105 interwencji. Na podstawie danych:"),
      tags$ol(
        tags$li("Kt\u00f3ra uprawa ma najni\u017csz\u0105 ", tags$b("median\u0119"), " plonu? Dlaczego mediana?"),
        tags$li("Kt\u00f3ra klasa gleby ma najwi\u0119kszy ", tags$b("rozrzut"), " plon\u00f3w (IQR)?"),
        tags$li("Czy pola z nawozeniem organicznym maj\u0105 wy\u017csze plony?
          (Boxplot: ", tags$code("plon_t_ha"), " ~ ", tags$code("nawozenie_organiczne"), ")"),
        tags$li("Sformu\u0142uj rekomendacj\u0119: kt\u00f3re pola wymagaj\u0105 uwagi i dlaczego?")
      )
    ),
    actionButton("ch8_ans6", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol6")
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zako\u0144czeniu \u0107wicze\u0144 odpowiedz na pytania:")),
    tags$ol(
      tags$li("Dlaczego poprawna klasyfikacja typu zmiennej jest wa\u017cna ",
              tags$em("zanim"), " zaczniemy analiz\u0119?"),
      tags$li("Podaj przyk\u0142ad zmiennej, kt\u00f3r\u0105 Jamovi automatycznie \u017ale zaklasyfikuje. Dlaczego?"),
      tags$li("Jaki jest najcz\u0119stszy b\u0142\u0105d zwi\u0105zany z typem zmiennej, kt\u00f3ry zaobserwowa\u0142e\u015b/a\u015b na zaj\u0119ciach?")
    )
  ),
  actionButton("ch8_ans_summary", "Poka\u017c odpowiedzi", class = "btn-outline-success btn-sm"),
  uiOutput("ch8_sol_summary")
)


# --------------------------------------------------------------------------
# TECHNOLOGIA ZYWNOSCI
# --------------------------------------------------------------------------

.ch8_content_zyw <- function() tagList(

  div(class = "section-title", "Blok 1: Rozpoznawanie typ\u00f3w zmiennych (20 min)"),
  div(class = "callout-info",
    p("W Jamovi: ", tags$b("Data \u2192 Setup"), " \u2014 sprawd\u017a, jak Jamovi automatycznie rozpozna\u0142 typy zmiennych.
      Czy ma racj\u0119? Popraw, je\u015bli trzeba.")
  ),

  div(class = "widget-block",
    h4("Zadanie 1 \u2014 Klasyfikacja zmiennych technologicznych"),
    div(class = "narrative",
      p("Otw\u00f3rz plik ", tags$code("dane/zywnosc_partie.csv"), " w Jamovi.
        Dla ka\u017cdej zmiennej w zbiorze okre\u015bl:"),
      tags$ol(
        tags$li("Jaki to typ zmiennej?"),
        tags$li("Czy Jamovi poprawnie rozpozna\u0142 typ?"),
        tags$li("Jakie statystyki opisowe s\u0105 sensowne?"),
        tags$li("Jaki wykres jest odpowiedni?")
      ),
      p("Wype\u0142nij tabel\u0119:"),
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
    actionButton("ch8_ans1", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol1")
  ),

  div(class = "widget-block",
    h4("Zadanie 2 \u2014 Pu\u0142apki typowania"),
    div(class = "narrative",
      tags$ol(
        tags$li("Zmienna ", tags$code("klasa_jakosci"), " przyjmuje warto\u015bci Premium, Standard, Ekonomiczna.
          Czy to zmienna nominalna? Uzasadnij."),
        tags$li("Zmienna ", tags$code("spelnia_norme"), " jest zakodowana jako TAK/NIE.
          Jaki to faktycznie typ zmiennej?"),
        tags$li("Gdyby\u015bmy zawarto\u015b\u0107 soli przekszta\u0142cili na kategorie:
          \u201eniska\u201d, \u201enormalna\u201d, \u201ewysoka\u201d \u2014 jak zmieni\u0142by si\u0119 typ zmiennej?"),
        tags$li("Czy \u015brednia z ", tags$code("klasa_jakosci"), " ma sens? Dlaczego?")
      )
    ),
    actionButton("ch8_ans2", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol2")
  ),

  div(class = "section-title", "Blok 2: Dob\u00f3r narz\u0119dzi statystycznych (25 min)"),

  div(class = "widget-block",
    h4("Zadanie 3 \u2014 Statystyki dla ka\u017cdego typu"),
    div(class = "narrative",
      p("W Jamovi: ", tags$b("Analyses \u2192 Exploration \u2192 Descriptives"), "."),
      tags$ol(
        tags$li("Dla ", tags$code("typ_produktu"), ": tabela cz\u0119sto\u015bci + wykres s\u0142upkowy.
          Kt\u00f3ry typ dominuje?"),
        tags$li("Dla ", tags$code("klasa_jakosci"), ": tabela cz\u0119sto\u015bci skumulowanych.
          Jaki procent partii to klasa Premium lub Standard?"),
        tags$li("Dla ", tags$code("liczba_reklamacji"), ": \u015brednia, mediana, SD, IQR.
          Czy rozk\u0142ad jest symetryczny?"),
        tags$li("Dla ", tags$code("zawartosc_soli_pct"), ": \u015brednia, SD, histogram.
          Jaki procent partii przekracza norm\u0119 2.5%?"),
        tags$li(tags$em("Refleksja:"), " Jakie statystyki Jamovi pozwala obliczy\u0107 dla ",
          tags$code("typ_produktu"), ", ale kt\u00f3re s\u0105 bezsensowne?")
      )
    ),
    actionButton("ch8_ans3", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol3")
  ),

  div(class = "widget-block",
    h4("Zadanie 4 \u2014 Wykres dobierz do typu"),
    div(class = "narrative",
      p("Stw\u00f3rz po jednym wykresie dla ka\u017cdego typu zmiennej:"),
      tags$ol(
        tags$li(tags$b("Nominalna"), " (", tags$code("typ_produktu"), "): wykres s\u0142upkowy"),
        tags$li(tags$b("Porz\u0105dkowa"), " (", tags$code("klasa_jakosci"), "): wykres s\u0142upkowy z kolejno\u015bci\u0105"),
        tags$li(tags$b("Dyskretna"), " (", tags$code("liczba_reklamacji"), "): wykres s\u0142upkowy"),
        tags$li(tags$b("Ci\u0105g\u0142a"), " (", tags$code("zawartosc_soli_pct"), "): histogram + boxplot")
      ),
      p(tags$em("Pytanie:"), " Gdyby\u015b chcia\u0142/a por\u00f3wna\u0107 zawarto\u015b\u0107 soli mi\u0119dzy typami produkt\u00f3w,
        jaki wykres by\u015b wybra\u0142/a?")
    ),
    actionButton("ch8_ans4", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol4")
  ),

  div(class = "section-title", "Blok 3: Analiza kompleksowa (45 min)"),

  div(class = "widget-block",
    h4("Zadanie 5 \u2014 Profil produkcji"),
    div(class = "narrative",
      p("Przygotuj raport opisowy dla zbioru ", tags$code("zywnosc_partie.csv"), ":"),
      tags$ol(
        tags$li("Ile partii jest w zbiorze? Ile zmiennych?"),
        tags$li("Struktura produkcji: tabela cz\u0119sto\u015bci typ\u00f3w + wykres s\u0142upkowy"),
        tags$li("Jako\u015b\u0107: rozk\u0142ad klas jako\u015bci + odsetek partii spe\u0142niaj\u0105cych norm\u0119"),
        tags$li("Masa netto: \u015brednia, SD, histogram. Czy masy skupiaj\u0105 si\u0119 wok\u00f3\u0142 warto\u015bci nominalnej?"),
        tags$li("Por\u00f3wnanie: zawarto\u015b\u0107 soli wg typu produktu (boxplot)")
      )
    ),
    actionButton("ch8_ans5", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol5")
  ),

  div(class = "widget-block",
    h4("Zadanie 6 \u2014 Decyzja technologa"),
    div(class = "narrative",
      p("Technolog musi zdecydowa\u0107, kt\u00f3re linie produkcyjne wymagaj\u0105 korekty:"),
      tags$ol(
        tags$li("Kt\u00f3ry typ produktu ma najwy\u017csz\u0105 ", tags$b("median\u0119"),
          " liczby reklamacji? Dlaczego mediana, a nie \u015brednia?"),
        tags$li("Kt\u00f3ry typ produktu ma najwi\u0119kszy ", tags$b("rozrzut"),
          " masy netto (SD lub IQR)?"),
        tags$li("Czy partie klasy Premium r\u00f3\u017cni\u0105 si\u0119 zawarto\u015bci\u0105 soli od klasy Ekonomicznej?
          (Boxplot: ", tags$code("zawartosc_soli_pct"), " ~ ", tags$code("klasa_jakosci"), ")"),
        tags$li("Sformu\u0142uj rekomendacj\u0119: kt\u00f3ra linia wymaga korekty i dlaczego?")
      )
    ),
    actionButton("ch8_ans6", "Poka\u017c rozwi\u0105zanie", class = "btn-outline-success btn-sm"),
    uiOutput("ch8_sol6")
  ),

  div(class = "section-title", "Podsumowanie"),
  div(class = "callout-warning",
    p(tags$b("Po zako\u0144czeniu \u0107wicze\u0144 odpowiedz na pytania:")),
    tags$ol(
      tags$li("Dlaczego poprawna klasyfikacja typu zmiennej jest wa\u017cna ",
              tags$em("zanim"), " zaczniemy analiz\u0119?"),
      tags$li("Podaj przyk\u0142ad zmiennej, kt\u00f3r\u0105 Jamovi automatycznie \u017ale zaklasyfikuje. Dlaczego?"),
      tags$li("Jaki jest najcz\u0119stszy b\u0142\u0105d zwi\u0105zany z typem zmiennej, kt\u00f3ry zaobserwowa\u0142e\u015b/a\u015b na zaj\u0119ciach?")
    )
  ),
  actionButton("ch8_ans_summary", "Poka\u017c odpowiedzi", class = "btn-outline-success btn-sm"),
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
            tags$td("Moda, cz\u0119sto\u015bci, proporcje"), tags$td("S\u0142upkowy")),
          tags$tr(tags$td(tags$code("liczba_pracownikow")), tags$td("Dyskretna"),
            tags$td("\u015arednia, mediana, SD, IQR"), tags$td("Histogram lub s\u0142upkowy")),
          tags$tr(tags$td(tags$code("liczba_wypadkow")), tags$td("Dyskretna"),
            tags$td("\u015arednia, mediana, SD, IQR"), tags$td("S\u0142upkowy")),
          tags$tr(tags$td(tags$code("kategoria_ryzyka")), tags$td("Porz\u0105dkowa"),
            tags$td("Moda, cz\u0119sto\u015bci, cz. skumulowane"), tags$td("S\u0142upkowy (z kolejno\u015bci\u0105)")),
          tags$tr(tags$td(tags$code("sredni_halas_db")), tags$td("Ci\u0105g\u0142a"),
            tags$td("\u015arednia, mediana, SD, sko\u015bno\u015b\u0107"), tags$td("Histogram, boxplot")),
          tags$tr(tags$td(tags$code("ma_certyfikat_iso")), tags$td("Nominalna (binarna)"),
            tags$td("Cz\u0119sto\u015bci, proporcje"), tags$td("S\u0142upkowy"))
        )
      )
    ),
    sol2 = tagList(
      tags$b("1."), " Nie \u2014 to zmienna ", tags$b("porz\u0105dkowa"),
        ". Liczby 1, 2, 3 oznaczaj\u0105 kategorie ryzyka z porz\u0105dkiem, ale r\u00f3\u017cnica
        mi\u0119dzy 1 a 2 nie jest taka sama jak mi\u0119dzy 2 a 3.", tags$br(),
      tags$b("2."), " Nominalna (binarna) \u2014 0/1 to tylko kody dla TAK/NIE,
        nie warto\u015bci liczbowe.", tags$br(),
      tags$b("3."), " Zakres 0\u2013300 nie zmienia typu \u2014 nadal dyskretna (liczby ca\u0142kowite).
        Ale po przekszta\u0142ceniu na \u201ema\u0142o/\u015brednio/du\u017co\u201d staje si\u0119 ",
        tags$b("porz\u0105dkowa"), ".", tags$br(),
      tags$b("4."), " Nie ma sensu! Kategorie 1, 2, 3 to etykiety, nie liczby.
        Lepiej: ", tags$b("moda"), " (najcz\u0119stsza kategoria) lub tabela cz\u0119sto\u015bci."
    ),
    sol3 = tagList(
      tags$b("1."), " Moda = dominuj\u0105ca bran\u017ca (np. \u201eProdukcja\u201d). Proporcje z tabeli cz\u0119sto\u015bci.", tags$br(),
      tags$b("2."), " Cz\u0119sto\u015bci skumulowane: np. 72% zak\u0142ad\u00f3w ma ryzyko \u2264 2.", tags$br(),
      tags$b("3."), " Je\u015bli \u015brednia > mediana \u2192 sko\u015bno\u015b\u0107 prawostronna (du\u017ce zak\u0142ady z wieloma wypadkami ci\u0105gn\u0105 \u015bredni\u0105 w g\u00f3r\u0119).", tags$br(),
      tags$b("4."), " Rozk\u0142ad prawdopodobnie zbli\u017cony do normalnego z lekk\u0105 sko\u015bno\u015bci\u0105.", tags$br(),
      tags$b("5."), " Jamovi pozwoli obliczy\u0107 \u015bredni\u0105 z ", tags$code("kategoria_ryzyka"),
        " je\u015bli jest zakodowana jako liczba \u2014 ale wynik jest ", tags$b("bezsensowny"), "."
    ),
    sol4 = tagList(
      p("Histogram dla ", tags$code("liczba_wypadkow"), " mo\u017ce by\u0107 mylący, bo zmienna dyskretna
        ma niewiele unikalnych warto\u015bci. Biny histogramu \u0142\u0105cz\u0105 s\u0105siednie warto\u015bci,
        co zniekszta\u0142ca obraz. Lepszy jest wykres s\u0142upkowy (ka\u017cda warto\u015b\u0107 = osobny s\u0142upek).")
    ),
    sol5 = tagList(
      p("Raport powinien opisywa\u0107 ka\u017cd\u0105 zmienn\u0105 narz\u0119dziami odpowiednimi do jej typu.
        Kluczowe: boxplot ha\u0142asu wg kategorii ryzyka poka\u017ce, czy wy\u017csze ryzyko
        koreluje z wy\u017cszym ha\u0142asem.")
    ),
    sol6 = tagList(
      tags$b("1."), " Mediana, bo rozkład wypadków jest skośny \u2014 pojedyncze zakłady z wieloma wypadkami zawyżają średnią.", tags$br(),
      tags$b("2."), " Sprawdź proporcje per branża (filtr + Descriptives).", tags$br(),
      tags$b("3."), " Boxplot pokaże, czy mediany rosną z kategorią ryzyka.", tags$br(),
      tags$b("4."), " Np. \u201eBranża X wymaga pilnej kontroli: najwyższa mediana wypadków i najwyższy odsetek przekroczeń normy hałasu.\u201d"
    ),
    sol_summary = tagList(
      tags$b("1."), " Bo typ zmiennej determinuje, jakie statystyki i wykresy mają sens.
        Średnia z kodów kategorii to bezsensowna liczba.", tags$br(), tags$br(),
      tags$b("2."), " Np. ", tags$code("kategoria_ryzyka"), " zakodowana jako 1/2/3 \u2014 Jamovi uzna ją za ilościową,
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
      tags$b("1."), " Nie nominalna \u2014 to ", tags$b("porz\u0105dkowa"), "! Klasy I\u2013VI maj\u0105
        naturalny porz\u0105dek (I = najlepsza), ale r\u00f3\u017cnice mi\u0119dzy klasami nie s\u0105 r\u00f3wne.", tags$br(),
      tags$b("2."), " Nominalna (binarna) \u2014 0/1 koduje TAK/NIE.", tags$br(),
      tags$b("3."), " Z ci\u0105g\u0142ej staje si\u0119 ", tags$b("porz\u0105dkowa"),
        " \u2014 tracimy precyzj\u0119 pomiaru, ale zyskujemy prostot\u0119 interpretacji.", tags$br(),
      tags$b("4."), " Nie ma sensu! Klasy to kategorie, nie liczby.
        U\u017cyj mody lub tabeli cz\u0119sto\u015bci."
    ),
    sol3 = tagList(
      tags$b("1."), " Moda = dominuj\u0105ca uprawa. Proporcje z tabeli cz\u0119sto\u015bci.", tags$br(),
      tags$b("2."), " Cz\u0119sto\u015bci skumulowane: np. 45% p\u00f3l ma gleb\u0119 I\u2013III.", tags$br(),
      tags$b("3."), " Je\u015bli \u015brednia \u2248 mediana \u2192 symetryczny. Je\u015bli r\u00f3\u017cne \u2192 sko\u015bny.", tags$br(),
      tags$b("4."), " Typowo rozk\u0142ad plon\u00f3w jest zbli\u017cony do normalnego.", tags$br(),
      tags$b("5."), " Jamovi nie pozwoli obliczy\u0107 \u015bredniej z nominalnej (je\u015bli poprawnie ustawiona).
        Ale je\u015bli zmienisz typ na Continuous \u2014 policzy bezsensown\u0105 \u015bredni\u0105."
    ),
    sol4 = tagList(
      p("Wykres ko\u0142owy jest z\u0142y dla wielu kategorii, bo ludzkie oko nie odr\u00f3\u017cnia
        k\u0105t\u00f3w r\u00f3\u017cni\u0105cych si\u0119 o 2\u20133%. Wykres s\u0142upkowy pozwala dok\u0142adnie por\u00f3wna\u0107 d\u0142ugo\u015bci.")
    ),
    sol5 = tagList(
      p("Kluczowe: boxplot plonu wg klasy gleby poka\u017ce, czy lepsza gleba = wy\u017cszy plon.
        Outliery w powierzchni mog\u0105 wskazywa\u0107 na du\u017ce gospodarstwa intensywne.")
    ),
    sol6 = tagList(
      tags$b("1."), " Mediana, bo plon jest cz\u0119sto sko\u015bny (susze, gradobicia obni\u017caj\u0105 plony nielicznych p\u00f3l).", tags$br(),
      tags$b("2."), " IQR wg klasy gleby \u2014 s\u0142absze gleby maj\u0105 zwykle wi\u0119kszy rozrzut.", tags$br(),
      tags$b("3."), " Boxplot: por\u00f3wnaj mediany i IQR obu grup.", tags$br(),
      tags$b("4."), " Np. \u201ePola na glebie V\u2013VI z nisk\u0105 liczb\u0105 zabieg\u00f3w wymagaj\u0105 uwagi \u2014 najni\u017csze plony i najwi\u0119kszy rozrzut.\u201d"
    ),
    sol_summary = tagList(
      tags$b("1."), " Bo typ zmiennej determinuje, jakie statystyki i wykresy maj\u0105 sens.", tags$br(), tags$br(),
      tags$b("2."), " Np. ", tags$code("klasa_gleby"), " zakodowana jako I\u2013VI \u2014 Jamovi mo\u017ce j\u0105 potraktowa\u0107
        jako nominalna (bez porz\u0105dku) lub ilo\u015bciowa.", tags$br(), tags$br(),
      tags$b("3."), " Obliczanie \u015bredniej z danych porz\u0105dkowych (np. \u015brednia klasy gleby = 2.8 \u2014 co to znaczy?)."
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
            tags$td("Moda, cz\u0119sto\u015bci, proporcje"), tags$td("S\u0142upkowy")),
          tags$tr(tags$td(tags$code("masa_netto_g")), tags$td("Ci\u0105g\u0142a"),
            tags$td("\u015arednia, mediana, SD, IQR"), tags$td("Histogram, boxplot")),
          tags$tr(tags$td(tags$code("liczba_reklamacji")), tags$td("Dyskretna"),
            tags$td("\u015arednia, mediana, SD, IQR"), tags$td("S\u0142upkowy")),
          tags$tr(tags$td(tags$code("klasa_jakosci")), tags$td("Porz\u0105dkowa"),
            tags$td("Moda, cz\u0119sto\u015bci, cz. skumulowane"), tags$td("S\u0142upkowy (z kolejno\u015bci\u0105)")),
          tags$tr(tags$td(tags$code("zawartosc_soli_pct")), tags$td("Ci\u0105g\u0142a"),
            tags$td("\u015arednia, mediana, SD, sko\u015bno\u015b\u0107"), tags$td("Histogram, boxplot")),
          tags$tr(tags$td(tags$code("spelnia_norme")), tags$td("Nominalna (binarna)"),
            tags$td("Cz\u0119sto\u015bci, proporcje"), tags$td("S\u0142upkowy"))
        )
      )
    ),
    sol2 = tagList(
      tags$b("1."), " Nie nominalna \u2014 to ", tags$b("porz\u0105dkowa"), "!
        Premium > Standard > Ekonomiczna ma naturalny porz\u0105dek jako\u015bci.", tags$br(),
      tags$b("2."), " Nominalna (binarna) \u2014 TAK/NIE to dwie kategorie bez porz\u0105dku.", tags$br(),
      tags$b("3."), " Z ci\u0105g\u0142ej staje si\u0119 ", tags$b("porz\u0105dkowa"),
        " \u2014 tracimy dok\u0142adny pomiar.", tags$br(),
      tags$b("4."), " Nie \u2014 nie mo\u017cna u\u015brednia\u0107 kategorii.
        U\u017cyj mody (najcz\u0119stsza klasa) lub tabeli cz\u0119sto\u015bci."
    ),
    sol3 = tagList(
      tags$b("1."), " Moda = dominuj\u0105cy typ produktu.", tags$br(),
      tags$b("2."), " Cz\u0119sto\u015bci skumulowane: np. 85% partii to Premium lub Standard.", tags$br(),
      tags$b("3."), " Liczba reklamacji \u2014 prawdopodobnie sko\u015bna prawo (wi\u0119kszo\u015b\u0107 ma ma\u0142o, kilka \u2014 du\u017co).", tags$br(),
      tags$b("4."), " Sprawd\u017a: odsetek partii z sol\u0105 > 2.5% to np. 15%.", tags$br(),
      tags$b("5."), " Jamovi pozwoli na \u015bredni\u0105 z nominalnej, je\u015bli niepoprawnie ustawiona \u2014 ale wynik nie ma sensu."
    ),
    sol4 = tagList(
      p("Boxplot grupowy (", tags$code("zawartosc_soli_pct"), " ~ ", tags$code("typ_produktu"),
        ") \u2014 por\u00f3wnanie rozk\u0142ad\u00f3w jednej zmiennej ci\u0105g\u0142ej mi\u0119dzy kategoriami nominalnej.")
    ),
    sol5 = tagList(
      p("Kluczowe: boxplot zawarto\u015bci soli wg typu produktu poka\u017ce, kt\u00f3re produkty
        maj\u0105 problem z przekroczeniem normy. Masa netto powinna skupia\u0107 si\u0119 wok\u00f3\u0142
        warto\u015bci nominalnej z ma\u0142ym SD.")
    ),
    sol6 = tagList(
      tags$b("1."), " Mediana, bo reklamacje s\u0105 sko\u015bne \u2014 kilka partii z wieloma reklamacjami zawy\u017ca \u015bredni\u0105.", tags$br(),
      tags$b("2."), " SD lub IQR masy netto \u2014 wy\u017cszy rozrzut = gorsza powtarzalno\u015b\u0107 procesu.", tags$br(),
      tags$b("3."), " Boxplot poka\u017ce, czy Premium faktycznie ma ni\u017csz\u0105 s\u00f3l.", tags$br(),
      tags$b("4."), " Np. \u201eLinia produkcyjna X wymaga kalibracji wagi \u2014 najwy\u017cszy rozrzut masy netto i najwi\u0119cej reklamacji.\u201d"
    ),
    sol_summary = tagList(
      tags$b("1."), " Bo typ zmiennej determinuje, jakie statystyki i wykresy maj\u0105 sens.", tags$br(), tags$br(),
      tags$b("2."), " Np. ", tags$code("klasa_jakosci"), " \u2014 Jamovi mo\u017ce j\u0105 potraktowa\u0107 jako tekst (nominalna)
        bez uwzgl\u0119dnienia porz\u0105dku Premium > Standard > Ekonomiczna.", tags$br(), tags$br(),
      tags$b("3."), " Obliczanie \u015bredniej z danych porz\u0105dkowych lub nominalnych."
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
      updateActionButton(session, paste0("ch8_", bid), label = "Poka\u017c rozwi\u0105zanie")
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
        label = if (nowy_stan) "Ukryj rozwi\u0105zanie" else "Poka\u017c rozwi\u0105zanie")
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
