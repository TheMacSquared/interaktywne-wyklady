# ============================================================================
# ROZDZIAŁ 1: Równanie regresji liniowej
# ============================================================================

ch1_ui <- lecture_chapter(
  id = "ch-rownanie",
  num = "01",
  title = "Równanie regresji",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · KMNK",
      num = "01",
      title = "Równanie regresji liniowej.",
      lead = "Korelacja powiedziała tylko, że dwie zmienne są powiązane. Regresja idzie dalej: modeluje ten związek liczbowo i pozwala odpowiadać na pytania ‚o ile’ — a nie tylko ‚czy’."
    ),

    lc_h2("ch1-po-co", "Po co regresja?"),
    lc_p("Wyobraź sobie, że prowadzisz lodziarnię i zauważyłeś, że w cieplejsze dni sprzedajesz więcej lodów. To jest korelacja — wiesz, że temperatura i sprzedaż się trzymają razem. Ale jeśli jutro prognoza pokazuje 26°C zamiast 22°C, to o ile więcej lodów zamówić u dostawcy? Korelacja na to pytanie nie odpowie. Regresja owszem."),
    lc_p("Albo inny obraz: rolnik patrzy na ilość nawozu (X) i plon z hektara (Y). Wie, że zależność jest dodatnia, ale chce konkretnej liczby — o ile kilogramów więcej da każdy dodatkowy worek nawozu? Regresja liniowa daje mu tę liczbę i pokazuje, jak bardzo można jej wierzyć."),
    lc_p("Krótko: regresja liniowa to narzędzie do mierzenia wpływu jednej zmiennej na drugą. Wpływu wyrażonego liczbą, którą można wstawić do prognozy, do umowy, do biznesplanu."),

    lc_h2("ch1-formula", "Równanie regresji liniowej z jedną zmienną"),
    lc_formula_box(
      withMathJax(helpText("$$Y_i = \\beta_0 + \\beta_1 X_i + \\varepsilon_i$$")),
      p(withMathJax("\\(Y_i\\)"), " — zmienna objaśniana dla obserwacji ", withMathJax("\\(i\\)"), " (np. sprzedaż w danym miesiącu)."),
      p(withMathJax("\\(X_i\\)"), " — zmienna objaśniająca (np. wydatki na reklamę w tym samym miesiącu)."),
      p(withMathJax("\\(\\beta_0\\)"), " — wyraz wolny: wartość Y, gdy X = 0."),
      p(withMathJax("\\(\\beta_1\\)"), " — nachylenie: o ile średnio zmienia się Y, gdy X rośnie o jedną jednostkę."),
      p(withMathJax("\\(\\varepsilon_i\\)"), " — składnik losowy: wszystko, czego model nie opisał (inne czynniki, błędy pomiaru, przypadkowość).")
    ),
    lc_p("To jest ten sam zapis, z którym spotkamy się w Excelu, Gretlu, R-ze i każdym podręczniku ekonometrii. Litery z indeksem ", withMathJax("\\(i\\)"), " to konkretne obserwacje (firma 1, firma 2, miesiąc 1, miesiąc 2…), a parametry ", withMathJax("\\(\\beta_0\\)"), " i ", withMathJax("\\(\\beta_1\\)"), " są dla wszystkich obserwacji wspólne — opisują regułę, którą próbujemy wykryć."),

    lc_h2("ch1-interpretacja", "Co znaczą β₀ i β₁ w praktyce?"),
    lc_p("Załóżmy, że dla pewnej firmy oszacowano model:"),
    lc_formula_box(
      withMathJax(helpText("$$\\widehat{Y_i} = 5{,}0 + 1{,}4 \\cdot X_i$$")),
      p("gdzie Y to miesięczna sprzedaż w tysiącach złotych, a X to wydatki na reklamę w tysiącach złotych.")
    ),
    lc_stat_grid(
      lc_stat_box("β₀ = 5,0", caption = "w miesiącu bez reklamy przewidujemy 5 tys. zł sprzedaży", color = upwr_secondary),
      lc_stat_box("β₁ = 1,4", caption = "każdy dodatkowy 1 tys. zł reklamy podnosi sprzedaż średnio o 1,4 tys. zł", color = unname(upwr_cat["szalwia"])),
      lc_stat_box("ε", caption = "konkretny miesiąc może odbiegać od tej reguły w obie strony", color = unname(upwr_cat["terakota"])),
      columns = 3
    ),
    lc_p("Dwie liczby — i już można rozmawiać o decyzji marketingowej. Jeśli wzrost wydatków o 1 tys. zł daje średnio 1,4 tys. zł sprzedaży, to ekonomicznie się to opłaca, póki marża pokrywa różnicę. Bez modelu mielibyśmy tylko poczucie, że „chyba reklama działa”."),

    inline_callout(
      label = "Wskazówka",
      color = "ok",
      "Słowo ‚średnio‘ jest tu kluczowe. β₁ = 1,4 nie znaczy, że każdy konkretny tysiąc reklamy doda dokładnie 1,4 tys. zł sprzedaży. Znaczy, że ", strong("przeciętnie"), " — w długim okresie, dla wielu miesięcy — taki będzie efekt. Pojedyncze obserwacje będą się odchylać przez ε, czyli przez wszystko, czego model nie kontroluje."
    ),

    inline_callout(
      label = "Pułapka",
      color = "uwaga",
      "Wyraz wolny β₀ ma sens ekonomiczny tylko wtedy, gdy X = 0 leży w zakresie zaobserwowanych danych. Jeśli nasze firmy wydają na reklamę od 10 do 50 tys. zł, β₀ to ekstrapolacja — formalnie poprawna, ale nie traktuj jej jak realnego scenariusza."
    ),

    lc_chapter_next(
      num = "02",
      title = "Dopasowanie KMNK",
      lead = "metoda najmniejszych kwadratów na danych",
      target_id = "ch-dopasowanie"
    )
  )
)

ch1_server <- function(input, output, session) {}
