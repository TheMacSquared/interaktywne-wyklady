# ============================================================================
# ROZDZIAŁ 1: Skąd bierze się niepewność?
# ============================================================================

ch1_ui <- lecture_chapter(
  id = "ch-niepewnosc",
  num = "01",
  title = "Skąd niepewność?",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 03 · Wnioskowanie",
      num = "01",
      title = "Próba, populacja i niepewność.",
      lead = "Wzory na b₀ i b₁ poznaliśmy w wykładzie 02. Teraz pytamy o coś trudniejszego: skoro każda próba daje inną estymatę, jak duża jest ta różnica i czy ma znaczenie?"
    ),

    lc_h2("ch1-pytanie", "Pytanie, którego nie da się ominąć"),
    lc_p("W wykładzie 02 patrzyliśmy na jedną próbę, jeden zestaw danych, jedną prostą KMNK. To jest sposób, w jaki najczęściej pracuje się z danymi w realnym świecie — dostajesz jeden plik z Excela i z tego pliku liczysz wszystko."),
    lc_p("Ale każda próba jest tylko fragmentem rzeczywistości. Gdyby ankieterzy zapukali do innych domów, gdyby firma wybrała inne 50 sklepów, gdyby badanie zaczęło się o miesiąc później — wyszłyby inne liczby. Pytanie ‚jak różne‘ stoi u progu całej statystyki wnioskowania."),

    lc_h2("ch1-rozroznienie", "Parametr β czy estymata b — zaglądamy ponownie"),
    lc_p("Dla porządku przypominamy podstawowe rozróżnienie z poprzedniego wykładu. Parametr populacyjny opisuje świat, którego nie widzimy w całości — to jest prawda, której szukamy. Estymata to liczba, którą faktycznie wyliczamy z konkretnej próby."),
    figure_panel(
      label = "Tabela 1.1",
      title = "Parametr populacji vs. estymata z próby",
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Cecha"),
          tags$th("Parametr β"),
          tags$th("Estymata b")
        )),
        tags$tbody(
          tags$tr(
            tags$td("Skąd pochodzi?"),
            tags$td("z całej populacji"),
            tags$td("z konkretnej próby")
          ),
          tags$tr(
            tags$td("Czy go znamy?"),
            tags$td("nie — to wartość, którą próbujemy odgadnąć"),
            tags$td("tak — to liczba, którą wyliczamy")
          ),
          tags$tr(
            tags$td("Czy jest losowy?"),
            tags$td("nie — to stała, choć nieznana"),
            tags$td("tak — zależy od tego, kogo trafiliśmy do próby")
          ),
          tags$tr(
            tags$td("Notacja"),
            tags$td("β₀, β₁ (litery greckie)"),
            tags$td("b₀, b₁ albo β̂₀, β̂₁ (z daszkiem)")
          )
        )
      )
    ),

    lc_h2("ch1-historia", "Trzy zespoły, ten sam mechanizm"),
    lc_p("Pewna sieć handlowa zleciła trzem firmom konsultingowym to samo pytanie: jak metraż sklepu wpływa na miesięczną sprzedaż? Każda firma dostała inną próbę 50 sklepów z tego samego rynku."),
    lc_p("Firma A oszacowała b₁ = 1,42 (tysiąca zł sprzedaży na metr kwadratowy). Firma B — b₁ = 1,61. Firma C — b₁ = 1,38. Klient wpadł w popłoch: która ma rację?"),
    lc_p("Odpowiedź: wszystkie trzy. Próby pochodzą z tej samej populacji, ale przez losowy dobór trafiły na nieco inne sklepy. Każda estymata jest poprawna — różnice biorą się z szumu w danych, nie z błędów rachunkowych. Pytanie, które naprawdę warto zadać: czy 1,42 i 1,61 to praktycznie ta sama odpowiedź, czy jednak istotnie różne wyniki?"),

    lc_h2("ch1-zrodla", "Trzy źródła zmienności estymaty"),
    lc_p("Niepewność b₁ nie spada z nieba — ma trzy konkretne źródła, z których każde możemy próbować ograniczać:"),
    tags$ul(
      tags$li(strong("Wielkość próby (n)."), " Im więcej obserwacji, tym mniej każda pojedyncza ‚dziwna‘ obserwacja waży na wyniku. Większa próba = stabilniejsza estymata."),
      tags$li(strong("Szum w danych (σ)."), " Gdy reszty są małe, dane układają się ciasno wokół prostej i nachylenie da się policzyć precyzyjnie. Gdy reszty są duże, ta sama prosta przesuwa się znacząco między próbami."),
      tags$li(strong("Rozrzut zmiennej X."), " Im szerszy zakres X w próbie, tym lepiej widać nachylenie. Próba ze sklepami tylko o powierzchni 50–60 m² da gorsze oszacowanie niż próba ze sklepami od 30 do 200 m².")
    ),

    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      open = TRUE,
      "Pojedyncza estymata to nie wyrok — to jeden punkt w rozkładzie wszystkich estymat, jakie mogłyby wyjść z różnych prób. Cała ta „chmura” możliwych wyników to coś, co opisuje błąd standardowy. Idziemy do niego w następnym rozdziale."
    ),

    lc_chapter_next(
      num = "02",
      title = "Błąd standardowy",
      lead = "miara wahliwości estymatora",
      target_id = "ch-se"
    )
  )
)

ch1_server <- function(input, output, session) {}
