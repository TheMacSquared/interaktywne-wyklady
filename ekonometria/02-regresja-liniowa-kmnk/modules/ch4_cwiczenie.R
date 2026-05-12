# ============================================================================
# ROZDZIAŁ 4: Czytanie wyników — ćwiczenie
# ============================================================================

ch4_ui <- lecture_chapter(
  id = "ch-cwiczenie",
  num = "04",
  title = "Czytanie wyników",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · KMNK",
      num = "04",
      title = "Czytanie wyników regresji.",
      lead = "Sprawdź się — czy umiesz przeczytać tabelę wyników regresji tak, jak czyta się ją na egzaminie i w raporcie? Trzy pytania na rozgrzewkę."
    ),

    lc_h2("ch4-pomysl", "Sytuacja"),
    lc_p("Lokalna lodziarnia zebrała dane z 30 dni sezonu letniego: średnią dzienną temperaturę w mieście (X, w stopniach Celsjusza) oraz dzienną sprzedaż lodów (Y, w tysiącach złotych). Po wczytaniu danych do Excela i uruchomieniu narzędzia ‚Regresja‘ właściciel dostał poniższą tabelę wyników:"),
    figure_panel(
      label = "Tabela 4.1",
      title = "Wyniki regresji KMNK: sprzedaż ~ temperatura",
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Współczynnik"),
          tags$th("Estymata"),
          tags$th("Błąd standardowy"),
          tags$th("Statystyka t"),
          tags$th("Wartość p")
        )),
        tags$tbody(
          tags$tr(
            tags$td("Wyraz wolny (b₀)"),
            tags$td("−1,20"),
            tags$td("0,85"),
            tags$td("−1,41"),
            tags$td("0,169")
          ),
          tags$tr(
            tags$td("Temperatura (b₁)"),
            tags$td("0,42"),
            tags$td("0,05"),
            tags$td("8,40"),
            tags$td("< 0,001")
          )
        )
      ),
      tags$p(tags$strong("R² = 0,715"), "    ", tags$strong("SE reszt = 0,74"), "    ", tags$strong("n = 30")),
      tags$p(em("Y w tys. zł, X w °C."))
    ),

    lc_h2("ch4-pytanie1", "Pytanie 1: jak interpretujemy współczynnik nachylenia?"),
    lc_p("Co oznacza b₁ = 0,42 w kontekście tych danych?"),
    radioButtons(
      "ch4_q1", NULL,
      choices = c(
        "Każdy stopień Celsjusza podnosi sprzedaż o 0,42 zł"                                = "a",
        "Każdy dodatkowy stopień Celsjusza podnosi dzienną sprzedaż średnio o 0,42 tys. zł" = "b",
        "Sprzedaż jest zawsze 0,42 razy większa od temperatury"                             = "c",
        "0,42 to procent zmienności sprzedaży wyjaśniony przez temperaturę"                 = "d"
      ),
      selected = character(0)
    ),
    uiOutput("ch4_feedback1"),

    lc_h2("ch4-pytanie2", "Pytanie 2: czy temperatura istotnie wpływa na sprzedaż?"),
    lc_p("Patrząc na kolumnę z wartościami p i przyjmując standardowy poziom istotności α = 0,05, jak odpowiadasz?"),
    radioButtons(
      "ch4_q2", NULL,
      choices = c(
        "Tak, temperatura istotnie wpływa na sprzedaż (p < 0,001)" = "a",
        "Nie, bo wyraz wolny jest nieistotny (p = 0,169)"          = "b",
        "Nie da się stwierdzić bez większej próby"                  = "c",
        "Tak, ale tylko w miesiącach letnich"                       = "d"
      ),
      selected = character(0)
    ),
    uiOutput("ch4_feedback2"),

    lc_h2("ch4-pytanie3", "Pytanie 3: ile zmienności sprzedaży tłumaczy temperatura?"),
    lc_p("R² = 0,715. Jak przekłada się to na słowny opis?"),
    radioButtons(
      "ch4_q3", NULL,
      choices = c(
        "0,715% zmienności sprzedaży"  = "a",
        "71,5% zmienności sprzedaży"   = "b",
        "0,74% zmienności sprzedaży"   = "c",
        "100% zmienności sprzedaży"    = "d"
      ),
      selected = character(0)
    ),
    uiOutput("ch4_feedback3"),

    lc_h2("ch4-podsumowanie", "Krótki raport — wzór odpowiedzi"),
    lc_p("Po tych trzech pytaniach widać, że umiejętność czytania regresji sprowadza się do sześciu liczb z tabeli. Złóż je w jedno zdanie raportu:"),
    inline_callout(
      label = "Przykład",
      color = "ok",
      em("‚Temperatura istotnie wpływa na dzienną sprzedaż lodów (b₁ = 0,42 tys. zł na 1°C; SE = 0,05; t = 8,4; p < 0,001). Każdy dodatkowy stopień podnosi sprzedaż średnio o 420 zł, a temperatura wyjaśnia 71,5% zmienności sprzedaży (R² = 0,715, SE reszt = 0,74 tys. zł, n = 30).‘")
    ),

    inline_callout(
      label = "Wskazówka",
      color = "wskazowka",
      "Wracaj do tej tabeli przy każdym ćwiczeniu z regresji. Sześć liczb — dwie estymaty, dwa błędy standardowe, R² i SE reszt — to wszystko, czego potrzebujesz, żeby opisać model słowami i obronić wnioski na egzaminie."
    ),

    lc_chapter_next(
      num = "05",
      title = "Następny rozdział",
      lead = "estymatory parametrów i błędy standardowe",
      target_id = "ch-rownanie"
    )
  )
)

ch4_server <- function(input, output, session) {
  output$ch4_feedback1 <- renderUI({
    ans <- input$ch4_q1
    if (is.null(ans) || !nzchar(ans)) return(NULL)
    if (ans == "b") {
      lc_feedback(
        type = "ok",
        strong("Dokładnie! "),
        "b₁ = 0,42 oznacza średnią zmianę zmiennej objaśnianej przy wzroście X o jednostkę — przy zachowaniu jednostek Y. Tu Y jest w tys. zł, więc wzrost o 1°C daje średnio 0,42 tys. zł = 420 zł dziennej sprzedaży więcej."
      )
    } else if (ans == "a") {
      lc_feedback(
        type = "warning",
        strong("Nie do końca. "),
        "Pomyliłeś jednostki: Y jest mierzone w tysiącach złotych, więc 0,42 oznacza 0,42 tys. zł = 420 zł, a nie 0,42 zł. Zawsze sprawdzaj jednostki obu zmiennych przed interpretacją współczynnika."
      )
    } else if (ans == "c") {
      lc_feedback(
        type = "warning",
        strong("Nie. "),
        "To brzmi jak proporcja, ale współczynnik nachylenia nie jest mnożnikiem między Y a X. Mówi tylko o zmianie krańcowej: o ile Y rośnie, gdy X wzrośnie o jedną jednostkę. Sama wartość Y zależy też od b₀ i ε."
      )
    } else {
      lc_feedback(
        type = "warning",
        strong("Nie. "),
        "Procent wyjaśnionej zmienności to R² (tu = 0,715, czyli 71,5%), a nie współczynnik nachylenia. b₁ ma jednostki: tys. zł na 1°C — i mówi o ", em("kierunku i sile"), " zależności, a nie o tym, ile model wyjaśnia."
      )
    }
  })

  output$ch4_feedback2 <- renderUI({
    ans <- input$ch4_q2
    if (is.null(ans) || !nzchar(ans)) return(NULL)
    if (ans == "a") {
      lc_feedback(
        type = "ok",
        strong("Dokładnie! "),
        "Wartość p < 0,001 dla współczynnika przy temperaturze jest dużo mniejsza od 0,05 — odrzucamy hipotezę zerową, że b₁ = 0. Czyli: dane mocno przemawiają za tym, że temperatura ", em("rzeczywiście"), " wpływa na sprzedaż, a nie tylko ‚wygląda jakby‘ przez przypadek."
      )
    } else if (ans == "b") {
      lc_feedback(
        type = "warning",
        strong("Nie. "),
        "Istotność wyrazu wolnego (b₀) i nachylenia (b₁) testuje się ", strong("osobno"), ". Nieistotny wyraz wolny oznacza tylko, że ‚sprzedaż przy 0°C‘ nie różni się statystycznie od zera — co nie ma większego znaczenia ekonomicznego. Pytanie o wpływ temperatury rozstrzyga się przez p dla b₁."
      )
    } else if (ans == "c") {
      lc_feedback(
        type = "warning",
        strong("Nie. "),
        "n = 30 to próba wystarczająco duża, żeby p-value miało sens — i ono jasno pokazuje istotność. Większa próba mogłaby zaostrzyć precyzję oszacowania, ale wnioski o istotności już są jasne na podstawie tego, co mamy."
      )
    } else {
      lc_feedback(
        type = "warning",
        strong("Nie. "),
        "Z samej tabeli regresji nie wynika nic o zróżnicowaniu efektu w czasie. Żeby sprawdzić, czy efekt temperatury zmienia się między porami roku, trzeba dodać do modelu zmienną sezonową albo przeprowadzić odrębne regresje na podpróbach."
      )
    }
  })

  output$ch4_feedback3 <- renderUI({
    ans <- input$ch4_q3
    if (is.null(ans) || !nzchar(ans)) return(NULL)
    if (ans == "b") {
      lc_feedback(
        type = "ok",
        strong("Dokładnie! "),
        "R² = 0,715 oznacza, że 71,5% zmienności dziennej sprzedaży lodów daje się wyjaśnić zmianami temperatury. Pozostałe 28,5% to wszystko inne — dni tygodnia, promocje, pogoda inna niż temperatura, przypadkowość — co trafia do reszt."
      )
    } else if (ans == "a") {
      lc_feedback(
        type = "warning",
        strong("Nie. "),
        "0,715 to ułamek dziesiętny, nie procent. Żeby zamienić na procent, mnożymy przez 100: 0,715 × 100% = 71,5%. To częsta pułapka — czytaj R² jak frakcję, a opisuj jak procent."
      )
    } else if (ans == "c") {
      lc_feedback(
        type = "warning",
        strong("Nie. "),
        "0,74 to SE reszt — typowy błąd predykcji wyrażony w jednostkach Y (tys. zł). To zupełnie inna wielkość niż R². R² mówi o ", em("udziale wyjaśnionej zmienności"), ", SE reszt — o ", em("typowym rozmiarze błędu"), "."
      )
    } else {
      lc_feedback(
        type = "warning",
        strong("Nie. "),
        "100% wyjaśnienia oznaczałoby, że wszystkie punkty leżą dokładnie na dopasowanej prostej (R² = 1). Tak idealnych modeli nie spotykamy w danych ekonomicznych. R² = 0,715 jest naprawdę bardzo dobrym wynikiem dla danych dziennych."
      )
    }
  })
}
