# ============================================================================
# CHAPTER 9: Cwiczenia praktyczne — dane z kierunków UPWr
# Trzy syntetyczne zbiory (rolnictwo, inz. bezpieczenstwa, techn. zywnosci)
# pokrywajace wszystkie typy testow z wykladu.
# ============================================================================

# ============================================================================
# UI
# ============================================================================

ch9_ui <- list(
  id = "ch-cwiczenia", num = "11", title = "Ćwiczenia",

  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 11 · Testowanie hipotez",
      num    = "11",
      title  = "Ćwiczenia z danych UPWr.",
      lead   = "Trzy zestawy danych z kierunków studiów UPWr — zastosuj wszystkie
                narzędzia z wykładu na danych ze swojej dziedziny:
                test jednej próby, proporcji, korelację, test t dwóch grup,
                test χ² i ANOVA."
    ),

    lc_h2("ch9-intro", "§ 1", "Zbiory danych"),

    tagList(
      p("Każdy blok ćwiczeń opiera się na syntetycznym zbiorze danych
        specyficznym dla jednego kierunku. Dane są zróżnicowane pod względem
        zmiennych i kontekstu, ale struktura zadań jest analogiczna —
        możesz wybrać kierunek najbliższy Twojej specjalności."),
      tags$ul(
        tags$li(tags$b("Blok 1: Rolnictwo"),
          " — 200 pól uprawnych z Dolnego Śląska (plon, nawożenie, pH gleby)"),
        tags$li(tags$b("Blok 2: Inżynieria bezpieczeństwa"),
          " — 200 przedsiębiorstw (wypadkowość, szkolenia BHP, ŚOI)"),
        tags$li(tags$b("Blok 3: Technologia żywności"),
          " — 200 partii produktów spożywczych (białko, wilgotność, trwałość)")
      ),
      p(tags$b("Czas:"), " ~2 h · ",
        tags$b("Narzędzie:"), " Jamovi lub R · ",
        tags$b("Format:"), " 1 wariant kierunkowy × 6 zadań + krytyczne myślenie, ukryte rozwiązania."),
      lc_feedback(type = "info",
        selectInput("ch9_kierunek", tags$b("Wybierz wariant dla kierunku:"),
          choices = list(
            "Rolnictwo" = "rol",
            "Inżynieria bezpieczeństwa (BHP)" = "bhp",
            "Technologia żywności" = "tz"
          ),
          selected = "rol",
          width = "100%"
        )
      )
    ),

    # ---- Blok 1: Rolnictwo ----
    conditionalPanel(
      condition = "input.ch9_kierunek == 'rol'",
      lc_h2("ch9-rol", "Blok 1", "Rolnictwo — pola uprawne Dolnego Śląska"),

      lc_feedback(type = "info",
        p(tags$b("Dane: "), tags$code("dane/rolnictwo.csv"),
          " — 200 pól uprawnych (Dolny Śląsk, sezon 2022–2023)."),
        p("Zmienne: ", tags$code("plon"), " (t/ha), ",
          tags$code("nawozenie"), " (kg NPK/ha), ",
          tags$code("ph"), " (pH gleby), ",
          tags$code("opady"), " (mm/sezon), ",
          tags$code("uprawa"), ' ("pszenica"/"rzepak"), ',
          tags$code("nawadnianie"), ' ("tak"/"nie"), ',
          tags$code("region"), ' ("dolnośląskie"/"opolskie"/"lubuskie").')
      ),

    figure_panel(label = "Ćwiczenie 1.1",
      h4("Czy średni plon różni się od krajowej normy 5.0 t/ha?"),
      p("Przetestuj dwustronnie, czy średni ", tags$code("plon"),
        " różni się od normy 5.0 t/ha. Sformułuj H₀ i Hₐ, wykonaj test t
        jednej próby (α = 0.05) i oblicz Cohen's d."),
      actionButton("ch9_r_ans1", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_r_sol1")
    ),

    figure_panel(label = "Ćwiczenie 1.2",
      h4("Czy mniej niż 40% pól stosuje nawadnianie?"),
      p("Przetestuj ", tags$b("lewostronnie"), ", czy odsetek pól z ",
        tags$code("nawadnianie == \"tak\""), " jest niższy niż 40% (p₀ = 0.4).
        Użyj testu dwumianowego."),
      actionButton("ch9_r_ans2", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_r_sol2")
    ),

    figure_panel(label = "Ćwiczenie 1.3",
      h4("Czy wyższe nawożenie wiąże się z wyższym plonem?"),
      p("Oblicz korelację Pearsona między ", tags$code("nawozenie"),
        " a ", tags$code("plon"), ". Czy korelacja jest istotna?
        Jak interpretujesz siłę i kierunek związku?"),
      actionButton("ch9_r_ans3", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_r_sol3")
    ),

    figure_panel(label = "Ćwiczenie 1.4",
      h4("Czy pszenica i rzepak dają różne plony?"),
      p("Porównaj średni ", tags$code("plon"),
        " między uprawą ", tags$code("pszenica"), " a ", tags$code("rzepak"),
        ". Wykonaj test t dla prób niezależnych i oblicz Cohen's d."),
      actionButton("ch9_r_ans4", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_r_sol4")
    ),

    figure_panel(label = "Ćwiczenie 1.5",
      h4("Czy typ uprawy wiąże się ze stosowaniem nawadniania?"),
      p("Zbuduj tabelę krzyżową ", tags$code("uprawa"), " × ",
        tags$code("nawadnianie"),
        " i wykonaj test χ² niezależności. Oblicz Cramér's V."),
      actionButton("ch9_r_ans5", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_r_sol5")
    ),

    figure_panel(label = "Ćwiczenie 1.6",
      h4("Czy region różnicuje plony?"),
      p("Wykonaj jednoczynnikową ANOVA: ", tags$code("plon"), " ~ ",
        tags$code("region"),
        " (trzy regiony). Zapisz F, df, p, η². Wykonaj post-hoc Games-Howell
        i wskaż, które pary regionów różnią się istotnie."),
      actionButton("ch9_r_ans6", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_r_sol6")
      )
    ),

    # ---- Blok 2: Inzynieria bezpieczenstwa ----
    conditionalPanel(
      condition = "input.ch9_kierunek == 'bhp'",
      lc_h2("ch9-bhp", "Blok 2", "Inżynieria bezpieczeństwa — przedsiębiorstwa"),

      lc_feedback(type = "info",
        p(tags$b("Dane: "), tags$code("dane/bezpieczenstwo.csv"),
          " — 200 przedsiębiorstw (Polska, 2022)."),
        p("Zmienne: ", tags$code("wypadki"), " (wypadki/rok na 1000 pracowników), ",
          tags$code("szkolenia"), " (godz. BHP/rok), ",
          tags$code("soi_rate"), " (% stosowania ŚOI), ",
          tags$code("ryzyko_score"), " (wskaźnik ryzyka 1–10), ",
          tags$code("wielkosc"), ' ("małe"/"duże"), ',
          tags$code("sektor"), ' ("produkcja"/"budownictwo"), ',
          tags$code("poziom_ryzyka"), ' ("niski"/"średni"/"wysoki").')
      ),

    figure_panel(label = "Ćwiczenie 2.1",
      h4("Czy wskaźnik wypadkowości jest niższy od średniej branżowej 10?"),
      p("Przetestuj ", tags$b("jednostronnie (lewostronnie)"), ", czy średni ",
        tags$code("wypadki"), " jest niższy od normy 10 wypadków/1000 pracowników.
        Sformułuj H₀ i Hₐ, oblicz Cohen's d."),
      actionButton("ch9_b_ans1", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_b_sol1")
    ),

    figure_panel(label = "Ćwiczenie 2.2",
      h4("Czy więcej niż 50% przedsiębiorstw spełnia normę stosowania ŚOI?"),
      p('Przyjmij, że „spełnia normę" = ', tags$code("soi_rate ≥ 80%"),
        ". Przetestuj ", tags$b("jednostronnie (prawostronnie)"),
        ", czy odsetek takich firm przekracza 50% (p₀ = 0.5). Użyj testu dwumianowego."),
      actionButton("ch9_b_ans2", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_b_sol2")
    ),

    figure_panel(label = "Ćwiczenie 2.3",
      h4("Czy więcej szkoleń BHP wiąże się z niższą wypadkowością?"),
      p("Oblicz korelację Pearsona między ", tags$code("szkolenia"),
        " a ", tags$code("wypadki"),
        ". Jaki jest kierunek zależności? Jak silna jest korelacja?"),
      actionButton("ch9_b_ans3", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_b_sol3")
    ),

    figure_panel(label = "Ćwiczenie 2.4",
      h4("Czy małe i duże przedsiębiorstwa różnią się wypadkowością?"),
      p("Porównaj średni ", tags$code("wypadki"),
        " między grupami ", tags$code("wielkosc"),
        " (małe vs duże). Wykonaj test t dla prób niezależnych."),
      actionButton("ch9_b_ans4", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_b_sol4")
    ),

    figure_panel(label = "Ćwiczenie 2.5",
      h4("Czy sektor jest niezależny od poziomu stosowania ŚOI?"),
      p("Utwórz zmienną binarną: ", tags$code("soi_ok = (soi_rate ≥ 80)"),
        ". Zbuduj tabelę ", tags$code("sektor"), " × ",
        tags$code("soi_ok"),
        " i wykonaj test χ² niezależności. Oblicz Cramér's V."),
      actionButton("ch9_b_ans5", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_b_sol5")
    ),

    figure_panel(label = "Ćwiczenie 2.6",
      h4("Czy poziom ryzyka różnicuje wypadkowość?"),
      p("Wykonaj jednoczynnikową ANOVA: ", tags$code("wypadki"), " ~ ",
        tags$code("poziom_ryzyka"),
        " (niski/średni/wysoki). Zapisz F, df, p, η².
        Wykonaj post-hoc Games-Howell."),
      actionButton("ch9_b_ans6", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_b_sol6")
      )
    ),

    # ---- Blok 3: Technologia zywnosci ----
    conditionalPanel(
      condition = "input.ch9_kierunek == 'tz'",
      lc_h2("ch9-tz", "Blok 3", "Technologia żywności — partie produktów"),

      lc_feedback(type = "info",
        p(tags$b("Dane: "), tags$code("dane/technologia_zywnosci.csv"),
          " — 200 partii produktów spożywczych (Polska, 2023)."),
        p("Zmienne: ", tags$code("bialko"), " (g białka/100 g), ",
          tags$code("wilgotnosc"), " (%), ",
          tags$code("trwalosc"), " (dni), ",
          tags$code("ph"), " (pH produktu), ",
          tags$code("typ"), ' ("tradycyjny"/"funkcjonalny"), ',
          tags$code("zanieczyszczenie"), ' ("brak"/"wykryte"), ',
          tags$code("przechowywanie"), ' ("chłodnicze"/"atmosfera modyfikowana"/"suszenie").')
      ),

    figure_panel(label = "Ćwiczenie 3.1",
      h4("Czy zawartość białka spełnia normę ≥ 12 g/100 g?"),
      p("Przetestuj ", tags$b("dwustronnie"), ", czy średnia ",
        tags$code("bialko"), " różni się od normy 12 g/100 g.
        Sformułuj H₀ i Hₐ, oblicz Cohen's d."),
      actionButton("ch9_t_ans1", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_t_sol1")
    ),

    figure_panel(label = "Ćwiczenie 3.2",
      h4("Czy ponad 20% partii ma wykryte zanieczyszczenia?"),
      p("Przetestuj ", tags$b("jednostronnie (prawostronnie)"),
        ", czy odsetek partii z ", tags$code("zanieczyszczenie == \"wykryte\""),
        " przekracza 20% (p₀ = 0.2). Użyj testu dwumianowego."),
      actionButton("ch9_t_ans2", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_t_sol2")
    ),

    figure_panel(label = "Ćwiczenie 3.3",
      h4("Czy wilgotność jest ujemnie skorelowana z trwałością?"),
      p("Oblicz korelację Pearsona między ", tags$code("wilgotnosc"),
        " a ", tags$code("trwalosc"),
        ". Jaki jest oczekiwany kierunek? Jak silna jest zależność?"),
      actionButton("ch9_t_ans3", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_t_sol3")
    ),

    figure_panel(label = "Ćwiczenie 3.4",
      h4("Czy produkty tradycyjne i funkcjonalne różnią się trwałością?"),
      p("Porównaj średnią ", tags$code("trwalosc"),
        " między ", tags$code("typ"),
        " (tradycyjny vs funkcjonalny). Wykonaj test t dla prób niezależnych."),
      actionButton("ch9_t_ans4", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_t_sol4")
    ),

    figure_panel(label = "Ćwiczenie 3.5",
      h4("Czy typ produktu wiąże się z wykryciem zanieczyszczeń?"),
      p("Zbuduj tabelę ", tags$code("typ"), " × ",
        tags$code("zanieczyszczenie"),
        " i wykonaj test χ² niezależności. Oblicz Cramér's V."),
      actionButton("ch9_t_ans5", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_t_sol5")
    ),

    figure_panel(label = "Ćwiczenie 3.6",
      h4("Czy metoda przechowywania różnicuje trwałość?"),
      p("Wykonaj jednoczynnikową ANOVA: ", tags$code("trwalosc"), " ~ ",
        tags$code("przechowywanie"),
        " (3 metody). Zapisz F, df, p, η².
        Wykonaj post-hoc Games-Howell."),
      actionButton("ch9_t_ans6", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_t_sol6")
      )
    ),

    # ---- Blok 4: Myslenie krytyczne ----
    lc_h2("ch9-krit", "Blok 4", "Myślenie krytyczne (~10 min)"),

    figure_panel(label = "Ćwiczenie",
      h4("Prawda czy fałsz?"),
      tagList(
        p("Oceń każde stwierdzenie (T/F) i uzasadnij odpowiedź:"),
        tags$ol(
          tags$li('„Korelacja r = –0.45 między szkoleniami a wypadkowością oznacza,
                   że szkolenia powodują spadek wypadków."'),
          tags$li('„p < 0.05 w teście t oznacza, że z prawdopodobieństwem 95%
                   hipoteza alternatywna jest prawdziwa."'),
          tags$li('„Test χ² wykazał p = 0.03 — wiem więc, że związek między typem
                   produktu a zanieczyszczeniem jest silny."'),
          tags$li('„Jeśli ANOVA dała p < 0.05, to każda para regionów różni się
                   istotnie pod względem plonu."'),
          tags$li('„Cohen\'s d = 0.15 przy p < 0.001 oznacza, że różnica między
                   grupami jest duża i ważna praktycznie."'),
          tags$li('„Gdybyśmy zamiast ANOVA wykonali trzy oddzielne testy t
                   dla każdej pary grup, wnioski byłyby identyczne."'),
          tags$li('„Korelacja Pearsona r = –0.30 oznacza, że nawożenie wyjaśnia
                   9% zmienności plonu."'),
          tags$li('„Test jednostronny (lewy) jest mocniejszy od dwustronnego,
                   więc zawsze powinniśmy go wybierać."')
        )
      ),
      actionButton("ch9_krit_ans", "Pokaż rozwiązanie",
                   class = "lc-btn-ok-outline lc-btn-sm"),
      uiOutput("ch9_krit_sol")
    ),

    br(), br(), br()
  )
)

# ============================================================================
# DANE — generowane syntetycznie z set.seed (brak plikow zewnetrznych)
# ============================================================================

.ch9_data <- local({

  set.seed(42)
  n <- 200

  # ---- Rolnictwo ----
  region_lvls <- c("dolnośląskie", "opolskie", "lubuskie")
  region   <- sample(region_lvls, n, replace = TRUE,
                     prob = c(0.45, 0.35, 0.20))
  uprawa   <- sample(c("pszenica", "rzepak"), n, replace = TRUE,
                     prob = c(0.60, 0.40))
  nawadnianie <- ifelse(runif(n) < 0.30, "tak", "nie")

  # regiony maja nieco rozne srednisie plonow
  base_plon <- ifelse(region == "dolnośląskie", 5.3,
               ifelse(region == "opolskie", 5.0, 4.7))
  plon <- pmax(1.5, base_plon + rnorm(n, 0, 0.9))

  nawozenie <- pmax(80, rnorm(n, 180, 40))
  ph        <- pmax(4.5, pmin(8.0, rnorm(n, 6.4, 0.55)))
  opady     <- pmax(250, rnorm(n, 540, 85))

  rolnictwo <- data.frame(
    plon = round(plon, 2),
    nawozenie = round(nawozenie, 1),
    ph = round(ph, 2),
    opady = round(opady, 0),
    uprawa, nawadnianie, region,
    stringsAsFactors = FALSE
  )

  # ---- Inzynieria bezpieczenstwa ----
  sektor       <- sample(c("produkcja", "budownictwo"), n, replace = TRUE,
                         prob = c(0.55, 0.45))
  wielkosc     <- sample(c("małe", "duże"), n, replace = TRUE,
                         prob = c(0.60, 0.40))
  poziom_ryzyka <- sample(c("niski", "średni", "wysoki"), n, replace = TRUE,
                           prob = c(0.30, 0.45, 0.25))

  base_wyp <- ifelse(poziom_ryzyka == "niski", 6.5,
              ifelse(poziom_ryzyka == "średni", 8.8, 11.5))
  wypadki   <- pmax(0.5, base_wyp + rnorm(n, 0, 2.2))
  szkolenia <- pmax(4, rnorm(n, 24, 8))
  soi_rate  <- pmax(30, pmin(100, rnorm(n, 77, 13)))
  ryzyko_score <- pmax(1, pmin(10, rnorm(n, 5.1, 1.7)))

  bezpieczenstwo <- data.frame(
    wypadki = round(wypadki, 2),
    szkolenia = round(szkolenia, 1),
    soi_rate = round(soi_rate, 1),
    ryzyko_score = round(ryzyko_score, 2),
    wielkosc, sektor, poziom_ryzyka,
    stringsAsFactors = FALSE
  )

  # ---- Technologia zywnosci ----
  typ              <- sample(c("tradycyjny", "funkcjonalny"), n, replace = TRUE,
                              prob = c(0.55, 0.45))
  przechowywanie   <- sample(c("chłodnicze", "atmosfera modyfikowana", "suszenie"),
                              n, replace = TRUE, prob = c(0.40, 0.35, 0.25))
  # metody przechowywania roznicuja trwalosc
  base_trw <- ifelse(przechowywanie == "chłodnicze", 160,
              ifelse(przechowywanie == "atmosfera modyfikowana", 195, 230))
  trwalosc      <- pmax(30, base_trw + rnorm(n, 0, 28))
  bialko        <- pmax(5, rnorm(n, 12.3, 2.1))
  wilgotnosc    <- pmax(5, pmin(25, rnorm(n, 11.3, 1.9)))
  ph_food       <- pmax(3.5, pmin(8.0, rnorm(n, 5.7, 0.6)))
  zanieczyszczenie <- ifelse(runif(n) < 0.18, "wykryte", "brak")

  technologia_zywnosci <- data.frame(
    bialko = round(bialko, 2),
    wilgotnosc = round(wilgotnosc, 2),
    trwalosc = round(trwalosc, 0),
    ph = round(ph_food, 2),
    typ, zanieczyszczenie, przechowywanie,
    stringsAsFactors = FALSE
  )

  list(rol = rolnictwo, bhp = bezpieczenstwo, tz = technologia_zywnosci)
})

# ============================================================================
# HELPERY obliczen
# ============================================================================

.ch9_t1 <- function(x, mu, alternative = "two.sided") {
  x <- x[!is.na(x)]; n <- length(x); m <- mean(x); s <- sd(x)
  se <- s / sqrt(n); t_val <- (m - mu) / se; df <- n - 1
  p_val <- switch(alternative,
    two.sided = 2 * pt(-abs(t_val), df),
    greater   = pt(t_val, df, lower.tail = FALSE),
    less      = pt(t_val, df, lower.tail = TRUE)
  )
  list(n=n, m=m, s=s, t=t_val, df=df, p=p_val, d=(m-mu)/s)
}

.ch9_cor_test <- function(x, y) {
  ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]; n <- length(x)
  r <- cor(x, y); t_val <- r * sqrt((n-2)/(1-r^2)); df <- n - 2
  p_val <- 2 * pt(-abs(t_val), df)
  list(r=r, t=t_val, df=df, p=p_val, n=n, r2=r^2)
}

.ch9_t2 <- function(x, grp) {
  grp <- as.factor(grp); lvls <- levels(grp)
  x1 <- x[grp == lvls[1]]; x2 <- x[grp == lvls[2]]
  n1 <- length(x1); n2 <- length(x2)
  m1 <- mean(x1); m2 <- mean(x2); s1 <- sd(x1); s2 <- sd(x2)
  se <- sqrt(s1^2/n1 + s2^2/n2); t_val <- (m1 - m2) / se
  df <- (s1^2/n1 + s2^2/n2)^2 /
        ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
  p_val <- 2 * pt(-abs(t_val), df)
  sp <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2))
  list(lvls=lvls, n1=n1, n2=n2, m1=m1, m2=m2, s1=s1, s2=s2,
       t=t_val, df=df, p=p_val, d=(m1-m2)/sp)
}

.ch9_chi2 <- function(tab) {
  ct <- chisq.test(tab, correct = FALSE)
  n  <- sum(tab); k <- min(nrow(tab), ncol(tab))
  v  <- sqrt(unname(ct$statistic) / (n * (k - 1)))
  list(chi2=unname(ct$statistic), df=unname(ct$parameter), p=ct$p.value, v=v, tab=tab)
}

.ch9_anova_f <- function(outcome, group) {
  df_fit <- data.frame(y = outcome, g = group)
  fit    <- aov(y ~ g, data = df_fit)
  s      <- summary(fit)[[1]]
  F_val  <- s[["F value"]][1]; df1 <- s[["Df"]][1]; df2 <- s[["Df"]][2]
  p_val  <- s[["Pr(>F)"]][1]
  eta2   <- s[["Sum Sq"]][1] / sum(s[["Sum Sq"]])
  ph     <- TukeyHSD(fit)$g
  grp_stats <- tapply(outcome, group, function(x) c(n=length(x), m=mean(x), s=sd(x)))
  list(F=F_val, df1=df1, df2=df2, p=p_val, eta2=eta2, ph=ph,
       grp_stats=grp_stats, lvls=levels(as.factor(group)))
}

.ch9_fmt_p <- function(p) {
  if (p < 0.001) "p < 0.001" else sprintf("p = %s", format(round(p, 4), nsmall = 4))
}

.ch9_decision <- function(p) {
  if (p < 0.05)
    tags$b(style = paste0("color:", upwr_accent), "Odrzucamy H₀ (p < 0.05)")
  else
    tags$b("Brak podstaw do odrzucenia H₀ (p ≥ 0.05)")
}

.ch9_sol_t1 <- function(r, h0_text, ha_text, var_label, mu, unit = "") {
  tagList(
    p(tags$b("H₀: "), h0_text, " · ", tags$b("Hₐ: "), ha_text),
    tags$ul(
      tags$li(sprintf("n = %d, x̄ = %.2f%s, s = %.2f",
                      r$n, r$m, if (nchar(unit) > 0) paste0(" ", unit) else "", r$s)),
      tags$li(sprintf("t(%s) = %.3f, %s", round(r$df, 1), r$t, .ch9_fmt_p(r$p))),
      tags$li(sprintf("Cohen's d = %.3f (%s efekt)", r$d, effect_size_label(r$d)))
    ),
    .ch9_decision(r$p)
  )
}

.ch9_sol_cor <- function(r, var1, var2) {
  tagList(
    tags$ul(
      tags$li(sprintf("r = %.3f, t(%d) = %.3f, %s",
                      r$r, r$df, r$t, .ch9_fmt_p(r$p))),
      tags$li(sprintf("R² = %.3f — %s wyjaśnia %.1f%% wariancji %s",
                      r$r2, var1, 100 * r$r2, var2))
    ),
    .ch9_decision(r$p)
  )
}

.ch9_sol_t2 <- function(r, unit = "") {
  tagList(
    tags$ul(
      tags$li(sprintf("%s: n = %d, x̄ = %.2f%s",
                      r$lvls[1], r$n1, r$m1,
                      if (nchar(unit) > 0) paste0(" ", unit) else "")),
      tags$li(sprintf("%s: n = %d, x̄ = %.2f%s",
                      r$lvls[2], r$n2, r$m2,
                      if (nchar(unit) > 0) paste0(" ", unit) else "")),
      tags$li(sprintf("t(%s) = %.3f, %s", round(r$df, 1), r$t, .ch9_fmt_p(r$p))),
      tags$li(sprintf("Cohen's d = %.3f (%s efekt)", abs(r$d), effect_size_label(r$d)))
    ),
    .ch9_decision(r$p)
  )
}

.ch9_sol_chi2 <- function(r) {
  tab <- r$tab
  tagList(
    tags$table(class = "lc-table lc-table-bordered lc-table-sm",
      tags$thead(tags$tr(
        tags$th(""),
        lapply(colnames(tab), tags$th),
        tags$th("suma")
      )),
      tags$tbody(lapply(rownames(tab), function(g) {
        tags$tr(tags$td(g),
          lapply(colnames(tab), function(cn) tags$td(tab[g, cn])),
          tags$td(sum(tab[g, ])))
      }))
    ),
    tags$ul(
      tags$li(sprintf("χ²(%d) = %.3f, %s", r$df, r$chi2, .ch9_fmt_p(r$p))),
      tags$li(sprintf("Cramér's V = %.3f (%s efekt)", r$v, effect_size_label(r$v)))
    ),
    .ch9_decision(r$p)
  )
}

.ch9_sol_anova <- function(r, outcome_label = "y") {
  tagList(
    tags$table(class = "lc-table lc-table-bordered lc-table-sm",
      tags$thead(tags$tr(tags$th("Grupa"), tags$th("n"),
                         tags$th(paste0("x̄ (", outcome_label, ")")), tags$th("s"))),
      tags$tbody(lapply(r$lvls, function(g) {
        v <- r$grp_stats[[g]]
        tags$tr(tags$td(g), tags$td(v["n"]),
                tags$td(round(v["m"], 2)), tags$td(round(v["s"], 2)))
      }))
    ),
    tags$ul(
      tags$li(sprintf("F(%d, %d) = %.3f, %s",
                      r$df1, r$df2, r$F, .ch9_fmt_p(r$p))),
      tags$li(sprintf("η² = %.3f (%s efekt — %.1f%% wariancji wyjaśnionej)",
        r$eta2,
        if (r$eta2 < 0.01) "pomijalny"
        else if (r$eta2 < 0.06) "mały"
        else if (r$eta2 < 0.14) "średni" else "duży",
        100 * r$eta2))
    ),
    .ch9_decision(r$p),
    p(tags$b("Post-hoc Tukey HSD (przybliżenie — w Jamovi użyj Games-Howell):")),
    tags$ul(lapply(rownames(r$ph), function(nm) {
      pp <- r$ph[nm, "p adj"]
      tags$li(sprintf("%s: Δ = %.2f, p.adj %s",
        nm, r$ph[nm, "diff"],
        if (pp < 0.001) "< 0.001" else format(round(pp, 3), nsmall = 3)))
    }))
  )
}

# ============================================================================
# SERVER
# ============================================================================

ch9_server <- function(input, output, session) {

  # ---- Rolnictwo ----
  rol <- .ch9_data$rol

  .make_toggle_r <- function(vis_rv, btn_id, sol_fn) {
    observeEvent(input[[btn_id]], {
      nowy <- !vis_rv()
      vis_rv(nowy)
      updateActionButton(session, btn_id,
        label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
    }, ignoreInit = TRUE)
    out_id <- sub("_ans", "_sol", btn_id)
    output[[out_id]] <- renderUI({
      if (!vis_rv()) return(NULL)
      lc_feedback(type = "ok", style = "margin-top: 10px;", sol_fn())
    })
  }

  r_vis <- lapply(1:6, function(i) reactiveVal(FALSE))

  .make_toggle_r(r_vis[[1]], "ch9_r_ans1", function() {
    r <- .ch9_t1(rol$plon, mu = 5.0, alternative = "two.sided")
    tagList(
      .ch9_sol_t1(r, "μ_plon = 5.0 t/ha", "μ_plon ≠ 5.0 t/ha",
                  "plon", 5.0, "t/ha"),
      p(tags$b("Interpretacja: "),
        sprintf("Średni plon (%.2f t/ha) %s się od normy 5.0 t/ha (%s).
          Efekt %s (d = %.3f).",
          r$m,
          if (r$p < 0.05) "różni istotnie" else "nie różni istotnie",
          .ch9_fmt_p(r$p), effect_size_label(r$d), r$d))
    )
  })

  .make_toggle_r(r_vis[[2]], "ch9_r_ans2", function() {
    k <- sum(rol$nawadnianie == "tak")
    n <- nrow(rol); p_obs <- k / n
    bt <- binom.test(k, n, p = 0.4, alternative = "less")
    tagList(
      p(tags$b("H₀: "), "p_nawadnianie ≥ 0.4 · ",
        tags$b("Hₐ: "), "p_nawadnianie < 0.4"),
      tags$ul(
        tags$li(sprintf("k = %d, n = %d, p̂ = %.3f (%.1f%%)",
                        k, n, p_obs, 100 * p_obs)),
        tags$li(.ch9_fmt_p(bt$p.value), " (test dwumianowy, lewostrony)")
      ),
      .ch9_decision(bt$p.value),
      p(tags$b("Interpretacja: "),
        sprintf("%.1f%% pól stosuje nawadnianie. Odsetek %s poniżej 40%% (%s).",
          100 * p_obs,
          if (bt$p.value < 0.05) "istotnie leży" else "nieistotnie leży",
          .ch9_fmt_p(bt$p.value)))
    )
  })

  .make_toggle_r(r_vis[[3]], "ch9_r_ans3", function() {
    r <- .ch9_cor_test(rol$nawozenie, rol$plon)
    tagList(
      .ch9_sol_cor(r, "nawożenie", "plonu"),
      p(tags$b("Interpretacja: "),
        sprintf("r = %.3f — korelacja %s (%s). Wyższe nawożenie wiąże się %s
          z wyższym plonem.",
          r$r, if (r$r > 0) "dodatnia" else "ujemna",
          effect_size_label(abs(r$r)),
          if (abs(r$r) > 0.3) "wyraźnie" else "słabo"))
    )
  })

  .make_toggle_r(r_vis[[4]], "ch9_r_ans4", function() {
    r <- .ch9_t2(rol$plon, rol$uprawa)
    tagList(
      .ch9_sol_t2(r, "t/ha"),
      p(tags$b("Interpretacja: "),
        sprintf("Różnica %.2f t/ha między pszenicą a rzepakiem jest %s (%s).
          Efekt %s (d = %.3f).",
          abs(r$m1 - r$m2),
          if (r$p < 0.05) "istotna" else "nieistotna",
          .ch9_fmt_p(r$p), effect_size_label(r$d), r$d))
    )
  })

  .make_toggle_r(r_vis[[5]], "ch9_r_ans5", function() {
    r <- .ch9_chi2(table(uprawa = rol$uprawa, nawadnianie = rol$nawadnianie))
    tagList(
      .ch9_sol_chi2(r),
      p(tags$b("Interpretacja: "),
        "Test χ² wskazuje, czy typ uprawy i decyzja o nawadnianiu są zależne.
        Cramér's V opisuje siłę związku niezależnie od kierunku.")
    )
  })

  .make_toggle_r(r_vis[[6]], "ch9_r_ans6", function() {
    r <- .ch9_anova_f(rol$plon, rol$region)
    tagList(
      .ch9_sol_anova(r, "plon (t/ha)"),
      p(tags$b("Interpretacja: "),
        sprintf("F(%d, %d) = %.3f, %s, η² = %.3f.
          Regiony %s się pod względem plonu.",
          r$df1, r$df2, r$F, .ch9_fmt_p(r$p), r$eta2,
          if (r$p < 0.05) "różnią" else "nie różnią"))
    )
  })

  # ---- Inzynieria bezpieczenstwa ----
  bhp <- .ch9_data$bhp

  b_vis <- lapply(1:6, function(i) reactiveVal(FALSE))

  .make_toggle_b <- function(vis_rv, btn_id, sol_fn) {
    observeEvent(input[[btn_id]], {
      nowy <- !vis_rv()
      vis_rv(nowy)
      updateActionButton(session, btn_id,
        label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
    }, ignoreInit = TRUE)
    out_id <- sub("_ans", "_sol", btn_id)
    output[[out_id]] <- renderUI({
      if (!vis_rv()) return(NULL)
      lc_feedback(type = "ok", style = "margin-top: 10px;", sol_fn())
    })
  }

  .make_toggle_b(b_vis[[1]], "ch9_b_ans1", function() {
    r <- .ch9_t1(bhp$wypadki, mu = 10, alternative = "less")
    tagList(
      .ch9_sol_t1(r, "μ_wypadki ≥ 10", "μ_wypadki < 10",
                  "wypadki", 10, "wyp./1000"),
      p(tags$b("Interpretacja: "),
        sprintf("Średnia wypadkowość (%.2f/1000) jest %s niższa od normy 10 (%s).
          Efekt %s (d = %.3f).",
          r$m,
          if (r$p < 0.05) "istotnie" else "nieistotnie",
          .ch9_fmt_p(r$p), effect_size_label(abs(r$d)), r$d))
    )
  })

  .make_toggle_b(b_vis[[2]], "ch9_b_ans2", function() {
    soi_ok <- bhp$soi_rate >= 80
    k <- sum(soi_ok); n <- length(soi_ok); p_obs <- k / n
    bt <- binom.test(k, n, p = 0.5, alternative = "greater")
    tagList(
      p(tags$b("H₀: "), "p_soi_ok ≤ 0.5 · ", tags$b("Hₐ: "), "p_soi_ok > 0.5"),
      tags$ul(
        tags$li(sprintf("k = %d, n = %d, p̂ = %.3f (%.1f%%)",
                        k, n, p_obs, 100 * p_obs)),
        tags$li(.ch9_fmt_p(bt$p.value), " (test dwumianowy, prawostrony)")
      ),
      .ch9_decision(bt$p.value),
      p(tags$b("Interpretacja: "),
        sprintf("%.1f%% przedsiębiorstw spełnia normę ŚOI ≥ 80%%.
          Odsetek %s większy od 50%% (%s).",
          100 * p_obs,
          if (bt$p.value < 0.05) "istotnie" else "nieistotnie",
          .ch9_fmt_p(bt$p.value)))
    )
  })

  .make_toggle_b(b_vis[[3]], "ch9_b_ans3", function() {
    r <- .ch9_cor_test(bhp$szkolenia, bhp$wypadki)
    tagList(
      .ch9_sol_cor(r, "szkolenia", "wypadkowości"),
      p(tags$b("Interpretacja: "),
        sprintf("r = %.3f — korelacja %s. Więcej szkoleń wiąże się
          %s wypadkowością. Pamiętaj: to korelacja, nie dowód przyczynowy.",
          r$r, effect_size_label(abs(r$r)),
          if (r$r < 0) "z niższą" else "z wyższą"))
    )
  })

  .make_toggle_b(b_vis[[4]], "ch9_b_ans4", function() {
    r <- .ch9_t2(bhp$wypadki, bhp$wielkosc)
    tagList(
      .ch9_sol_t2(r, "wyp./1000"),
      p(tags$b("Interpretacja: "),
        sprintf("Różnica %.2f wyp./1000 jest %s (%s). Efekt %s (d = %.3f).",
          abs(r$m1 - r$m2),
          if (r$p < 0.05) "istotna" else "nieistotna",
          .ch9_fmt_p(r$p), effect_size_label(r$d), r$d))
    )
  })

  .make_toggle_b(b_vis[[5]], "ch9_b_ans5", function() {
    soi_ok <- bhp$soi_rate >= 80
    r <- .ch9_chi2(table(sektor = bhp$sektor, soi_ok = soi_ok))
    tagList(
      .ch9_sol_chi2(r),
      p(tags$b("Interpretacja: "),
        "Zależy, czy sektor (produkcja vs budownictwo) różnicuje stosowanie ŚOI.
        Cramér's V podaje siłę tego związku.")
    )
  })

  .make_toggle_b(b_vis[[6]], "ch9_b_ans6", function() {
    r <- .ch9_anova_f(bhp$wypadki, bhp$poziom_ryzyka)
    tagList(
      .ch9_sol_anova(r, "wypadki/1000"),
      p(tags$b("Interpretacja: "),
        sprintf("F(%d, %d) = %.3f, %s, η² = %.3f.
          Poziomy ryzyka %s się wypadkowością.",
          r$df1, r$df2, r$F, .ch9_fmt_p(r$p), r$eta2,
          if (r$p < 0.05) "różnią" else "nie różnią"))
    )
  })

  # ---- Technologia zywnosci ----
  tz <- .ch9_data$tz

  t_vis <- lapply(1:6, function(i) reactiveVal(FALSE))

  .make_toggle_t <- function(vis_rv, btn_id, sol_fn) {
    observeEvent(input[[btn_id]], {
      nowy <- !vis_rv()
      vis_rv(nowy)
      updateActionButton(session, btn_id,
        label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
    }, ignoreInit = TRUE)
    out_id <- sub("_ans", "_sol", btn_id)
    output[[out_id]] <- renderUI({
      if (!vis_rv()) return(NULL)
      lc_feedback(type = "ok", style = "margin-top: 10px;", sol_fn())
    })
  }

  .make_toggle_t(t_vis[[1]], "ch9_t_ans1", function() {
    r <- .ch9_t1(tz$bialko, mu = 12, alternative = "two.sided")
    tagList(
      .ch9_sol_t1(r, "μ_białko = 12 g/100 g", "μ_białko ≠ 12 g/100 g",
                  "białko", 12, "g/100 g"),
      p(tags$b("Interpretacja: "),
        sprintf("Średnia zawartość białka (%.2f g/100 g) %s się od normy 12 g/100 g
          (%s). Efekt %s (d = %.3f).",
          r$m,
          if (r$p < 0.05) "różni istotnie" else "nie różni istotnie",
          .ch9_fmt_p(r$p), effect_size_label(r$d), r$d))
    )
  })

  .make_toggle_t(t_vis[[2]], "ch9_t_ans2", function() {
    k <- sum(tz$zanieczyszczenie == "wykryte")
    n <- nrow(tz); p_obs <- k / n
    bt <- binom.test(k, n, p = 0.20, alternative = "greater")
    tagList(
      p(tags$b("H₀: "), "p_zanieczyszczenie ≤ 0.20 · ",
        tags$b("Hₐ: "), "p_zanieczyszczenie > 0.20"),
      tags$ul(
        tags$li(sprintf("k = %d, n = %d, p̂ = %.3f (%.1f%%)",
                        k, n, p_obs, 100 * p_obs)),
        tags$li(.ch9_fmt_p(bt$p.value), " (test dwumianowy, prawostrony)")
      ),
      .ch9_decision(bt$p.value),
      p(tags$b("Interpretacja: "),
        sprintf("%.1f%% partii ma wykryte zanieczyszczenia. Odsetek %s
          przekracza próg 20%% (%s).",
          100 * p_obs,
          if (bt$p.value < 0.05) "istotnie" else "nieistotnie",
          .ch9_fmt_p(bt$p.value)))
    )
  })

  .make_toggle_t(t_vis[[3]], "ch9_t_ans3", function() {
    r <- .ch9_cor_test(tz$wilgotnosc, tz$trwalosc)
    tagList(
      .ch9_sol_cor(r, "wilgotność", "trwałości"),
      p(tags$b("Interpretacja: "),
        sprintf("r = %.3f — korelacja %s. Wyższa wilgotność wiąże się
          %s trwałością produktu.",
          r$r, effect_size_label(abs(r$r)),
          if (r$r < 0) "z niższą" else "z wyższą"))
    )
  })

  .make_toggle_t(t_vis[[4]], "ch9_t_ans4", function() {
    r <- .ch9_t2(tz$trwalosc, tz$typ)
    tagList(
      .ch9_sol_t2(r, "dni"),
      p(tags$b("Interpretacja: "),
        sprintf("Różnica %.0f dni między typami produktu jest %s (%s).
          Efekt %s (d = %.3f).",
          abs(r$m1 - r$m2),
          if (r$p < 0.05) "istotna" else "nieistotna",
          .ch9_fmt_p(r$p), effect_size_label(r$d), r$d))
    )
  })

  .make_toggle_t(t_vis[[5]], "ch9_t_ans5", function() {
    r <- .ch9_chi2(table(typ = tz$typ, zanieczyszczenie = tz$zanieczyszczenie))
    tagList(
      .ch9_sol_chi2(r),
      p(tags$b("Interpretacja: "),
        "Test χ² wskazuje, czy typ produktu wiąże się z wykryciem zanieczyszczeń.
        Siłę związku opisuje Cramér's V.")
    )
  })

  .make_toggle_t(t_vis[[6]], "ch9_t_ans6", function() {
    r <- .ch9_anova_f(tz$trwalosc, tz$przechowywanie)
    tagList(
      .ch9_sol_anova(r, "trwałość (dni)"),
      p(tags$b("Interpretacja: "),
        sprintf("F(%d, %d) = %.3f, %s, η² = %.3f.
          Metody przechowywania %s się trwałością.",
          r$df1, r$df2, r$F, .ch9_fmt_p(r$p), r$eta2,
          if (r$p < 0.05) "różnią" else "nie różnią"))
    )
  })

  # ---- Myslenie krytyczne ----
  krit_vis <- reactiveVal(FALSE)

  observeEvent(input$ch9_kierunek, {
    lapply(c(r_vis, b_vis, t_vis, list(krit_vis)), function(vis_rv) vis_rv(FALSE))
    for (prefix in c("r", "b", "t")) {
      for (i in 1:6) {
        updateActionButton(session, paste0("ch9_", prefix, "_ans", i),
                           label = "Pokaż rozwiązanie")
      }
    }
    updateActionButton(session, "ch9_krit_ans", label = "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  observeEvent(input$ch9_krit_ans, {
    nowy <- !krit_vis()
    krit_vis(nowy)
    updateActionButton(session, "ch9_krit_ans",
      label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
  }, ignoreInit = TRUE)

  output$ch9_krit_sol <- renderUI({
    if (!krit_vis()) return(NULL)
    lc_feedback(type = "ok", style = "margin-top: 10px;",
      tags$ol(
        tags$li(tags$b("Fałsz."),
          " Korelacja nie implikuje przyczynowości. Wymagane byłoby badanie
          eksperymentalne lub quasi-eksperymentalne z kontrolą zmiennych."),
        tags$li(tags$b("Fałsz."),
          ' p < 0.05 oznacza: „dane byłyby mało prawdopodobne, gdyby H₀ była
          prawdziwa" — nie daje prawdopodobieństwa prawdziwości H_a.'),
        tags$li(tags$b("Fałsz."),
          " Test χ² informuje tylko o tym, czy zmienne są zależne — nie mówi,
          jak silny jest związek ani w jakim kierunku. Do siły służy Cramér's V."),
        tags$li(tags$b("Fałsz."),
          " ANOVA wskazuje, że co najmniej jedna para różni się. Które konkretnie?
          Odpowiada na to post-hoc (np. Games-Howell)."),
        tags$li(tags$b("Fałsz."),
          " Małe d = 0.15 przy dużym n może dać p < 0.001 — ale efekt jest pomijalny
          praktycznie. Istotność statystyczna ≠ istotność praktyczna."),
        tags$li(tags$b("Fałsz."),
          " Trzy oddzielne testy t influją błąd I rodzaju (problem porównań
          wielokrotnych). Przy α = 0.05 prawdopodobieństwo co najmniej jednego
          fałszywego odkrycia rośnie do ~14%."),
        tags$li(tags$b("Prawda."),
          " R² = r² = (–0.30)² = 0.09 — nawożenie wyjaśnia 9% zmienności plonu."),
        tags$li(tags$b("Fałsz."),
          " Test jednostronny jest mocniejszy tylko w założonym kierunku — jest
          całkowicie ślepy na efekt w przeciwnym kierunku. Kierunek hipotezy
          musimy ustalić przed zebraniem danych, nie na podstawie wyników.")
      )
    )
  })
}
