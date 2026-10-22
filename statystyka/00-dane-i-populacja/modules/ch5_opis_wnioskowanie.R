# ============================================================================
# CHAPTER 5: Opis i wnioskowanie
# ============================================================================

# Cztery węzły w rzędzie; klamra „opis” nad próbą, tabelą
# i statystyką, strzałka „wnioskowanie” pod spodem wraca do populacji.
ch5_flow_diagram <- function() {
  node <- function(col, name, sym, color, sym_color = color, small = FALSE) {
    tags$div(class = paste("fm-node", if (small) "is-small"),
      style = sprintf("grid-row:3;grid-column:%d;--fm-c:%s;--fm-sc:%s;", col, color, sym_color),
      tags$div(class = "fm-name", name),
      tags$div(class = "fm-sym", sym))
  }
  arrow <- function(col, label) {
    tags$div(class = "fm-arrow", style = sprintf("grid-row:3;grid-column:%d;", col),
      tags$span(label), tags$span(class = "fm-arr", "→"))
  }
  line <- function(row, col, cls) {
    tags$div(class = paste("fm-line", cls), style = sprintf("grid-row:%d;grid-column:%d;", row, col))
  }
  tagList(
    tags$style(HTML("
      .fm { display: grid; grid-template-columns: minmax(0,1fr) auto minmax(0,1fr) auto minmax(0,1fr) auto minmax(0,1fr);
            max-width: 720px; margin: 6px auto 2px; }
      .fm-node { border: 2px solid var(--fm-c); border-radius: 8px; padding: 10px 6px;
                 text-align: center; background: color-mix(in srgb, var(--fm-c) 9%, transparent); }
      .fm-name { font-weight: 600; font-size: 15px; color: var(--upwr-ink); }
      .fm-sym  { font-size: 20px; margin-top: 2px; color: var(--fm-sc); }
      .fm-node.is-small .fm-sym { font-size: 14px; margin-top: 6px; }
      .fm-arrow { display: flex; flex-direction: column; align-items: center; justify-content: center;
                  padding: 0 6px; font-size: 12px; color: var(--upwr-ink-soft); }
      .fm-arr { font-size: 22px; line-height: 1; }
      .fm-lab { text-align: center; font-size: 13px; font-weight: 600; letter-spacing: .02em; }
      .fm-lab-opis { grid-row: 1; grid-column: 3 / 8; color: var(--upwr-cat-bursztyn); }
      .fm-lab-wn   { grid-row: 5; grid-column: 1 / 8; color: var(--upwr-cat-wrzos); }
      .fm-line { height: 12px; border: 0 solid; }
      .fm-top  { border-top-width: 2px; border-color: var(--upwr-cat-bursztyn); margin-top: 4px; margin-bottom: 6px; }
      .fm-bot  { border-bottom-width: 2px; border-color: var(--upwr-cat-wrzos); border-style: dashed; margin-top: 6px; }
      .fm-start { margin-left: 50%; border-left-width: 2px; }
      .fm-end   { margin-right: 50%; border-right-width: 2px; }
      .fm-bot.fm-start { position: relative; }
      .fm-bot.fm-start::before { content: ''; position: absolute; left: -7px; top: -8px;
        border: 6px solid transparent; border-bottom: 8px solid var(--upwr-cat-wrzos); border-top: 0; }
      @media (max-width: 600px) { .fm-node { padding: 8px 2px; } .fm-name { font-size: 12px; } .fm-sym { font-size: 16px; }
        .fm-arrow { padding: 0 2px; } .fm-arrow span:first-child { display: none; } }
    ")),
    tags$div(class = "fm", role = "img",
      `aria-label` = "Schemat: populacja z parametrem, losowanie, próba, pomiar, tabela, liczenie, statystyka. Opis obejmuje próbę, tabelę i statystykę; wnioskowanie wraca od statystyki do parametru populacji.",
      tags$div(class = "fm-lab fm-lab-opis", "opis"),
      line(2, 3, "fm-top fm-start"), lapply(4:6, line, row = 2, cls = "fm-top"), line(2, 7, "fm-top fm-end"),
      node(1, "Populacja", "μ, p", "var(--upwr-cat-niebo)", "var(--upwr-cat-wrzos)"),
      arrow(2, "losowanie"),
      node(3, "Próba", "n", "var(--upwr-accent)"),
      arrow(4, "pomiar"),
      node(5, "Tabela", "wiersze × kolumny", "var(--upwr-ink-soft)", small = TRUE),
      arrow(6, "liczenie"),
      node(7, "Statystyka", "x̄, p̂", "var(--upwr-cat-bursztyn)"),
      line(4, 1, "fm-bot fm-start"), lapply(2:6, line, row = 4, cls = "fm-bot"), line(4, 7, "fm-bot fm-end"),
      tags$div(class = "fm-lab fm-lab-wn", "wnioskowanie")
    )
  )
}

ch5_ui <- list(
  id    = "ch-opis-wnioskowanie",
  num   = "05",
  title = "Opis i wnioskowanie",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 05 · Dane i populacja",
      num    = "05",
      title  = "Dwa pytania, które zadajemy danym.",
      lead   = "Pierwsze brzmi: jak wyglądają dane, które mamy. Drugie: co z nich
                wynika dla całej populacji. Odpowiedzią na pierwsze zajmuje się
                statystyka opisowa, na drugie wnioskowanie statystyczne.
                Ten podział porządkuje cały kurs."
    ),

    lc_p("Pojęcia z tego wykładu łączą się w jeden schemat. Mamy ", gloss("populacja", "populację"), "
      i interesujący nas ", gloss("parametr"), ". Losujemy ", gloss("próba", "próbę"), ", zapisujemy ", gloss("obserwacja", "obserwacje"), "
      w tabeli i liczymy ", gloss("statystyka", "statystykę"), ". Statystyka trafia w parametr tylko
      w przybliżeniu, bo zależy od tego, kto trafił do próby. Każdy kolejny
      wykład zajmuje się jednym odcinkiem tej drogi."),

    # Droga od populacji do statystyki i z powrotem
    figure_panel(
      label = "Ryc. 5.1",
      title = "Od populacji do statystyki",
      width_mode = "text",
      ch5_flow_diagram()
    ),

    lc_h2("ch5-dwa-zadania", "Opis próby i wniosek o populacji"),

    lc_p(gloss("statystyka opisowa", "Statystyka opisowa"), " streszcza
      dane, które mamy: liczy średnie, odsetki i miary rozrzutu, rysuje
      wykresy. Jej wynik dotyczy tylko zbadanych obserwacji. Zdanie
      „w naszej próbie 19 z 50 osób pracuje zarobkowo” jest opisem i jest
      prawdziwe bez żadnych dodatkowych założeń."),

    lc_p(gloss("wnioskowanie statystyczne", "Wnioskowanie statystyczne"),
      " idzie dalej: na podstawie próby mówi coś o populacji. Zdanie
      „na wydziale pracuje zarobkowo od 25% do 51% studentów” jest
      wnioskiem. Wnioskowanie zawsze niesie niepewność, bo inna próba
      dałaby inny wynik, i zawsze opiera się na założeniu, że próba
      powstała w sposób, który nie faworyzuje żadnej grupy."),

    lc_note("Przykład",
      "Gdy prowadzący liczy średnią ocen z kolokwium w swojej grupie, a grupa
       jest jedynym, co go interesuje, wykonuje opis: grupa jest wtedy
       populacją, a średnia parametrem. Gdy tę samą średnią traktuje jako
       informację o wszystkich studentach kierunku, wnioskuje i musi
       zapytać, czy jego grupa jest dla nich reprezentatywna."
    ),

    lc_h2("ch5-mapa", "Mapa kursu"),

    lc_p("Tabela pokazuje, gdzie w kursie wracają pojęcia z tego wykładu.
      Wykłady 01, 02 i 08 dotyczą głównie opisu i jakości danych, wykłady
      od 03 do 07 wnioskowania, a 09 i 10 łączą jedno z drugim
      w pełnej analizie."),

    lc_table(
      data.frame(
        num   = c("01", "02", "03", "04", "05", "06", "07", "08", "09–10"),
        title = c("Statystyka opisowa", "Związki dwóch zmiennych",
                  "Rozkłady prawdopodobieństwa",
                  "Przedziały ufności", "Testowanie hipotez",
                  "Założenia testów", "Regresja", "Dobre dane",
                  "Case studies, projekt badawczy"),
        link  = c(
          "Rodzaje zmiennych i statystyki opisujące próbę: x̄, s, p̂.",
          "Opis związku dwóch zmiennych w próbie: tabela krzyżowa, grupy, r.",
          "Dlaczego statystyki zmieniają się od próby do próby i jak bardzo.",
          "Zakres, w którym prawdopodobnie leży parametr, policzony z jednej próby.",
          "Czy różnica w próbie świadczy o różnicy w populacji.",
          "Kiedy przybliżenia z wykładów 04 i 05 przestają działać.",
          "Parametry modelu opisującego związek zmiennych.",
          "Jednostka obserwacji, operat i dobór próby w prawdziwych danych.",
          "Cała droga: od pytania o populację do wniosku z próby."
        ),
        stringsAsFactors = FALSE
      ),
      list(
        lc_col("num", "Nr", "row"),
        lc_col("title", "Wykład", "text"),
        lc_col("link", "Co robi z pojęciami z wykładu 00", "text")
      ),
      narrow = "cards",
      prose = TRUE,
      caption = "Pojęcia z wykładu 00 w kolejnych wykładach."
    ),

    lc_recap(
      "Wiersz tabeli to obserwacja, kolumna to zmienna. Jednostkę obserwacji
       wyznacza pytanie, a od niej zależy n.",
      "Populacja to wszyscy, o których chcemy coś powiedzieć; próba to ci,
       których zbadaliśmy.",
      "Parametr opisuje populację i zwykle go nie znamy. Statystyka opisuje
       próbę i zmienia się od próby do próby.",
      "Losowanie chroni przed obciążeniem, duże n zmniejsza zmienność
       próbkową. Jedno nie zastąpi drugiego.",
      "Wzorzec w próbie może być dziełem przypadku. Jak często sam szum
       daje taki obraz, zależy od n.",
      "Opis dotyczy próby, wnioskowanie przenosi wynik na populację
       i zawsze niesie niepewność."
    ),

    lc_chapter_next(
      num       = "06",
      title     = "Quiz",
      lead      = "populacja, próba, parametr czy statystyka?",
      target_id = "ch-quiz"
    )
  )
)

ch5_server <- function(input, output, session) {}
