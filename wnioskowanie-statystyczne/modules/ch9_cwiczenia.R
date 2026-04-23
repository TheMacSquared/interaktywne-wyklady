# ============================================================================
# CHAPTER 9: Cwiczenia praktyczne — wnioskowanie statystyczne
# Zbiór CASchools (420 okręgów szkolnych Kalifornii) — pokrycie wszystkich
# typów testów z wykładu. System danych gotowy na rozszerzenie o kolejne
# zbiory wzorowany na przedzialy-ufnosci/modules/ch7_cwiczenia.R.
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
      title  = "Ćwiczenia z danymi.",
      lead   = "420 okręgów szkolnych Kalifornii — dane przekrojowe ze zmiennymi
                ciągłymi i jakościowymi. Czas zastosować wszystkie narzędzia
                z wykładu: od testu jednej próby po ANOVA."
    ),

    lc_h2("ch9-intro", "§ 1", "Zbiór danych CASchools"),

    div(class = "narrative",
      p("Dane pochodzą z badania przeprowadzonego w 1998–1999 r. przez California
        Department of Education i dotyczą 420 okręgów szkolnych (ang. ",
        tags$em("school districts"), ") w stanie Kalifornia. Każdy wiersz opisuje
        jeden okręg."),
      p("Kluczowe zmienne:"),
      tags$ul(
        tags$li(tags$code("read"), ", ", tags$code("math"),
          " — średni wynik standaryzowanego testu z czytania i matematyki"),
        tags$li(tags$code("income"),
          " — przeciętny dochód gospodarstw domowych w okręgu (tys. USD)"),
        tags$li(tags$code("english"),
          " — odsetek uczniów uczących się angielskiego jako drugiego języka (ELL, %)"),
        tags$li(tags$code("lunch"),
          " — odsetek uczniów z dotacją do obiadów (wskaźnik ubóstwa, %)"),
        tags$li(tags$code("student_teacher_ratio"),
          " — liczba uczniów na jednego nauczyciela (STR)"),
        tags$li(tags$code("grades"),
          " — zakres klas w okręgu: ", tags$code("KK-06"), " lub ", tags$code("KK-08"))
      ),
      p(tags$b("Czas:"), " ~2 h · ",
        tags$b("Narzędzie:"), " Jamovi · ",
        tags$b("Format:"), " 6 bloków, 11 zadań, ukryte rozwiązania.")
    ),

    figure_panel(
      label = "Dane",
      title = "Wybierz zbiór danych",
      div(
        selectInput("ch9_dataset", NULL,
          choices  = list("Edukacja (CASchools)" = "edu"),
          selected = "edu",
          width    = "100%"
        ),
        p(class = "lc-label",
          "Otwórz odpowiedni plik CSV z folderu ", tags$code("dane/"),
          " w Jamowi przed rozpoczęciem ćwiczeń.")
      )
    ),

    uiOutput("ch9_content"),

    br(), br(), br()
  )
)

# ============================================================================
# TRESC ZADAN — funkcja zwracajaca tagList per dataset
# ============================================================================

.ch9_task <- function(task_id, title, narrative, ans_btn_id, sol_out_id) {
  div(class = "widget-block",
    h4(task_id, " — ", title),
    div(class = "narrative", narrative),
    actionButton(ans_btn_id, "Pokaż rozwiązanie",
                 class = "btn-outline-success btn-sm"),
    uiOutput(sol_out_id)
  )
}

# ----------------------------------------------------------------------------
# EDUKACJA (CASchools)
# ----------------------------------------------------------------------------

.ch9_content_edu <- function() tagList(

  div(class = "callout-info",
    p(tags$b("Otwórz plik "), tags$code("dane/caschools.csv"), tags$b(" w Jamowi"), "."),
    p("Dane ze 420 okręgów szkolnych Kalifornii (1998–1999). Zmienne: ",
      tags$code("read"), ", ", tags$code("math"), ", ", tags$code("income"), ", ",
      tags$code("english"), ", ", tags$code("lunch"), ", ",
      tags$code("student_teacher_ratio"), ", ", tags$code("grades"), ".")
  ),

  # ---- Blok 1: Test jednej proby ----
  lc_h2("ch9-blok1", "Blok 1", "Test jednej próby (~20 min)"),

  .ch9_task("Zadanie 1",
    "Czy wyniki z czytania różnią się od krajowej normy 650 pkt?",
    p("Departament edukacji podaje normę 650 pkt. Przetestuj, czy średni wynik ",
      tags$code("read"), " w okręgach Kalifornii ", tags$b("istotnie różni się"),
      " od 650. Sformułuj H₀ i Hₐ, wykonaj test t jednej próby (α = 0.05)
      i oblicz Cohen's d. Co raportowałbyś departamentowi?"),
    "ch9_ans1", "ch9_sol1"
  ),

  .ch9_task("Zadanie 2",
    "Czy typowy dochód okręgu przekracza 15 tys. USD?",
    p('Hipoteza dyrekcji: „Nasz stan to stan zamożnych" — tzn. typowy okrąg
      ma dochód powyżej 15 tys. USD. Przetestuj jednostronnie (prawostronie) zmienną ',
      tags$code("income"), '. Skonstruuj H₀ i Hₐ dla hipotezy kierunkowej.
      Jaki wniosek? Czy wynik jest istotny statystycznie? A praktycznie?'),
    "ch9_ans2", "ch9_sol2"
  ),

  # ---- Blok 2: Korelacja ----
  lc_h2("ch9-blok2", "Blok 2", "Korelacja Pearsona (~20 min)"),

  .ch9_task("Zadanie 3",
    "Jak silnie czytanie i matematyka idą w parze?",
    p("Oblicz korelację Pearsona między zmiennymi ", tags$code("read"), " i ",
      tags$code("math"), ". Zanim klikniesz: czy spodziewasz się korelacji
      dodatniej czy ujemnej? Silnej czy słabej? Zanotuj swoje przewidywanie
      i sprawdź, jak daleko byłeś/aś od wyniku."),
    "ch9_ans3", "ch9_sol3"
  ),

  .ch9_task("Zadanie 4",
    "Czy zamożniejsze okręgi uczą się lepiej?",
    p("Oblicz korelację Pearsona między ", tags$code("income"), " a ",
      tags$code("read"), ". Jaki znak ma r? Czy korelacja jest istotna?
      Czy możesz z tego wyciągnąć wniosek przyczynowy — że wyższy dochód
      ", tags$em("powoduje"), " lepsze wyniki?"),
    "ch9_ans4", "ch9_sol4"
  ),

  .ch9_task("Zadanie 5",
    "Czy przeładowane klasy szkodzą wynikom?",
    p("Oblicz korelację Pearsona między ", tags$code("student_teacher_ratio"),
      " (STR) a ", tags$code("read"), ". Dlaczego korelacja jest ", tags$em("ujemna"),
      "? Czy jest istotna statystycznie? Czy silna praktycznie?"),
    "ch9_ans5", "ch9_sol5"
  ),

  # ---- Blok 3: Test dwóch grup ----
  lc_h2("ch9-blok3", "Blok 3", "Test t dwóch grup (~20 min)"),

  .ch9_task("Zadanie 6",
    "Czy typ szkoły różnicuje wyniki z czytania?",
    p("Okręgi dzielą się na szkoły zakresu ", tags$code("KK-06"), " i ",
      tags$code("KK-08"), ". Przetestuj, czy średnie wyniki ", tags$code("read"),
      " różnią się między grupami. Wykonaj test t dla prób niezależnych i oblicz
      Cohen's d. Jak duży jest efekt?"),
    "ch9_ans6", "ch9_sol6"
  ),

  .ch9_task("Zadanie 7",
    "Duże klasy vs małe — czy stosunek uczniów do nauczycieli ma znaczenie?",
    p("Stwórz zmienną binarną: ", tags$code("high_str = (student_teacher_ratio > 20)"),
      ". Porównaj wyniki ", tags$code("read"), " między okręgami z dużym (STR > 20)
      i małym (STR ≤ 20) stosunkiem. Czy różnica jest istotna? Jak duże jest
      przesunięcie w punktach?"),
    "ch9_ans7", "ch9_sol7"
  ),

  # ---- Blok 4: Chi-kwadrat niezaleznosci ----
  lc_h2("ch9-blok4", "Blok 4", "Test niezależności χ² (~20 min)"),

  .ch9_task("Zadanie 8",
    "Czy typ szkoły wiąże się z wysokim odsetkiem uczniów ELL?",
    p("Stwórz zmienną binarną: ", tags$code("high_english = (english > 20)"),
      ". Zbuduj tabelę krzyżową ", tags$code("grades"), " × ",
      tags$code("high_english"), " i wykonaj test χ² niezależności.
      Zapisz: χ², df, p. Co wynika z wyniku? Czy typ szkoły jest niezależny
      od odsetka uczniów uczących się angielskiego?"),
    "ch9_ans8", "ch9_sol8"
  ),

  .ch9_task("Zadanie 9",
    "Czy przeładowane klasy idą w parze z ubóstwem uczniów?",
    p("Stwórz dwie zmienne binarne: ", tags$code("high_str = (student_teacher_ratio > 20)"),
      " i ", tags$code("high_lunch = (lunch > 50)"),
      " (okręgi, gdzie ponad połowa uczniów dostaje dotację do obiadów).
      Wykonaj test χ² niezależności. Czy STR i ubóstwo są ze sobą powiązane?"),
    "ch9_ans9", "ch9_sol9"
  ),

  # ---- Blok 5: ANOVA ----
  lc_h2("ch9-blok5", "Blok 5", "ANOVA (~20 min)"),

  .ch9_task("Zadanie 10",
    "Czy wyniki czytania różnią się między tercylami dochodu?",
    p("Podziel okręgi na trzy równe grupy dochodowe (tercyle): ",
      tags$b("niski / średni / wysoki"), " (użyj ", tags$code("split into groups"),
      " w Jamowi lub utwórz zmienną ręcznie na podstawie kwantyli 0, 1/3, 2/3, 1).
      Wykonaj jednoczynnikową ANOVA dla zmiennej ", tags$code("read"),
      " między grupami. Zapisz: F, df, p, η². Wykonaj też post-hoc Games-Howell
      i wskaż, które pary różnią się istotnie."),
    "ch9_ans10", "ch9_sol10"
  ),

  # ---- Blok 6: Myslenie krytyczne ----
  lc_h2("ch9-blok6", "Blok 6", "Myślenie krytyczne (~10 min)"),

  .ch9_task("Zadanie 11",
    "Prawda czy fałsz?",
    tagList(
      p("Na podstawie wyników z powyższych zadań oceń każde stwierdzenie:"),
      tags$ol(
        tags$li('„Skoro korelacja między dochodem a wynikami z czytania jest istotna (Zadanie 4),
                 wyższy dochód okręgu powoduje wyższe wyniki testów."'),
        tags$li('„Korelacja r = –0.23 między STR a read oznacza, że STR wyjaśnia
                 ok. 5% zmienności wyników."'),
        tags$li('„Test t w Zadaniu 6 wykazał p < 0.05, więc różnica między KK-06
                 a KK-08 jest duża i praktycznie ważna."'),
        tags$li('„Test χ² z Zadania 8 dał p < 0.05, więc wiemy, o ile procent
                 różni się odsetek high_english między grupami grades."'),
        tags$li('„ANOVA w Zadaniu 10 wykazała F > 0, więc ', tags$em("wszystkie"),
                 ' trzy grupy dochodowe różnią się między sobą."'),
        tags$li('„Gdybyśmy zamiast ANOVA wykonali trzy oddzielne testy t
                 dla każdej pary grup, otrzymalibyśmy identyczne wnioski."'),
        tags$li('„W Zadaniu 1 test wykazał p < 0.05 — to znaczy, że
                 z prawdopodobieństwem 95% prawdziwa średnia różni się od 650."'),
        tags$li('„Istotność statystyczna gwarantuje, że wynik ma znaczenie
                 praktyczne (policy-relevant) dla systemu oświaty."')
      )
    ),
    "ch9_ans11", "ch9_sol11"
  ),

  br()
)

# ============================================================================
# DANE — wczytane raz przy ladowaniu modulu
# ============================================================================

.ch9_data <- list(
  edu = read.csv(file.path(app_dir, "dane", "caschools.csv"),
                 stringsAsFactors = FALSE)
)

# ============================================================================
# HELPERY obliczen (uzywane inline w .ch9_solutions)
# ============================================================================

.ch9_t1samp <- function(x, mu, alternative = "two.sided") {
  x <- x[!is.na(x)]
  n  <- length(x)
  m  <- mean(x); s <- sd(x)
  se <- s / sqrt(n)
  t  <- (m - mu) / se
  df <- n - 1
  p  <- switch(alternative,
    two.sided = 2 * pt(-abs(t), df),
    greater   = pt(t, df, lower.tail = FALSE),
    less      = pt(t, df, lower.tail = TRUE)
  )
  d_cohen <- (m - mu) / s
  list(n = n, mean = m, sd = s, se = se, t = t, df = df, p = p,
       mu = mu, d = d_cohen, alternative = alternative)
}

.ch9_cor <- function(x, y) {
  complete <- complete.cases(x, y)
  x <- x[complete]; y <- y[complete]
  n <- length(x)
  r <- cor(x, y)
  t <- r * sqrt((n - 2) / (1 - r^2))
  df <- n - 2
  p  <- 2 * pt(-abs(t), df)
  list(r = r, t = t, df = df, p = p, n = n)
}

.ch9_t2samp <- function(x, grp) {
  grp <- as.factor(grp)
  lvls <- levels(grp)
  x1 <- x[grp == lvls[1]]; x2 <- x[grp == lvls[2]]
  n1 <- length(x1); n2 <- length(x2)
  m1 <- mean(x1); m2 <- mean(x2)
  s1 <- sd(x1);   s2 <- sd(x2)
  se <- sqrt(s1^2/n1 + s2^2/n2)
  t  <- (m1 - m2) / se
  df <- (s1^2/n1 + s2^2/n2)^2 /
        ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
  p  <- 2 * pt(-abs(t), df)
  sp <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2))
  d  <- (m1 - m2) / sp
  list(lvls = lvls, n1 = n1, n2 = n2, m1 = m1, m2 = m2,
       s1 = s1, s2 = s2, t = t, df = df, p = p, d = d)
}

.ch9_chisq <- function(tab) {
  ct   <- chisq.test(tab, correct = FALSE)
  list(chi2 = ct$statistic, df = ct$parameter, p = ct$p.value, tab = tab)
}

.ch9_fmt_t <- function(r) {
  sprintf("t(%s) = %.3f, p %s %s",
    round(r$df, 1),
    r$t,
    if (r$p < 0.001) "<" else "=",
    if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))
}

.ch9_fmt_r <- function(r) {
  sprintf("r = %.3f, t(%d) = %.3f, p %s %s",
    r$r, r$df, r$t,
    if (r$p < 0.001) "<" else "=",
    if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))
}

.ch9_decision <- function(p, alpha = 0.05) {
  if (p < alpha)
    tagList(tags$b(style = paste0("color:", upwr_accent),   "Odrzucamy H₀"))
  else
    tagList(tags$b(style = paste0("color:", unname(upwr_cat["szalwia"])),
                   "Brak podstaw do odrzucenia H₀"))
}

# ============================================================================
# ROZWIAZANIA — lista per dataset
# ============================================================================

.ch9_solutions <- local({

  edu <- .ch9_data$edu

  # ---- sol1: test jednej proby, read vs 650 ----
  sol1 <- local({
    r <- .ch9_t1samp(edu$read, mu = 650, alternative = "two.sided")
    tagList(
      p(tags$b("H₀: "), "μ_read = 650 · ", tags$b("Hₐ: "), "μ_read ≠ 650"),
      tags$ul(
        tags$li(sprintf("n = %d, x̄ = %.2f, s = %.2f", r$n, r$mean, r$sd)),
        tags$li(.ch9_fmt_t(r)),
        tags$li(sprintf("Cohen's d = %.3f (%s efekt)", r$d, effect_size_label(r$d)))
      ),
      p(.ch9_decision(r$p)),
      p(tags$b("Interpretacja:"),
        sprintf(
          " Średni wynik z czytania (%.2f pkt) różni się istotnie od normy 650 pkt
           (p %s 0.05), jednak efekt jest %s (d = %.3f) — różnica %s pkt ma
           ograniczone znaczenie praktyczne.",
          r$mean,
          if (r$p < 0.001) "<" else "=",
          effect_size_label(r$d),
          r$d,
          round(r$mean - 650, 2)
        )
      )
    )
  })

  # ---- sol2: test jednej proby, income > 15, jednostronny ----
  sol2 <- local({
    r <- .ch9_t1samp(edu$income, mu = 15, alternative = "greater")
    tagList(
      p(tags$b("H₀: "), "μ_income ≤ 15 · ", tags$b("Hₐ: "), "μ_income > 15"),
      tags$ul(
        tags$li(sprintf("n = %d, x̄ = %.2f, s = %.2f (tys. USD)", r$n, r$mean, r$sd)),
        tags$li(.ch9_fmt_t(r)),
        tags$li(sprintf("Cohen's d = %.3f (%s efekt)", r$d, effect_size_label(r$d)))
      ),
      p(.ch9_decision(r$p)),
      p(tags$b("Interpretacja:"),
        sprintf(
          " Średni dochód (%.2f tys. USD) jest %s wyższy od 15 tys. (p %s 0.05).
           Efekt %s (d = %.3f). Pamiętaj: to jednostronny test — sformułowanie
           hipotezy kierunkowej jest uzasadnione tylko gdy masz merytoryczne
           podstawy, by zakładać dany kierunek z góry.",
          r$mean,
          if (r$p < 0.05) "istotnie" else "nieistotnie",
          if (r$p < 0.001) "<" else "=",
          effect_size_label(r$d),
          r$d
        )
      )
    )
  })

  # ---- sol3: korelacja read ~ math ----
  sol3 <- local({
    r <- .ch9_cor(edu$read, edu$math)
    tagList(
      tags$ul(
        tags$li(.ch9_fmt_r(r)),
        tags$li(sprintf("R² = %.3f (wspólna wariancja: %.1f%%)", r$r^2, 100*r$r^2))
      ),
      p(.ch9_decision(r$p)),
      p(tags$b("Interpretacja:"),
        sprintf(
          " r = %.3f — korelacja %s i dodatnia. Okręgi z lepszymi wynikami
           z czytania osiągają też lepsze wyniki z matematyki.
           Korelacja wyjaśnia %.1f%% wariancji wyników matematyki.",
          r$r,
          effect_size_label(r$r),
          100*r$r^2
        )
      )
    )
  })

  # ---- sol4: korelacja income ~ read ----
  sol4 <- local({
    r <- .ch9_cor(edu$income, edu$read)
    tagList(
      tags$ul(
        tags$li(.ch9_fmt_r(r)),
        tags$li(sprintf("R² = %.3f (wspólna wariancja: %.1f%%)", r$r^2, 100*r$r^2))
      ),
      p(.ch9_decision(r$p)),
      p(tags$b("Korelacja ≠ przyczynowość:"),
        " Korelacja jest istotna i dodatnia — bogatsze okręgi mają wyższe wyniki.
        Jednak nie możemy stwierdzić, że dochód ", tags$em("powoduje"),
        " lepsze wyniki. Trzecia zmienna (np. jakość nauczycieli, zasób kulturowy
        rodziny) może tłumaczyć obie. Potrzeba badania eksperymentalnego lub
        quasi-eksperymentalnego, by mówić o przyczynowości.")
    )
  })

  # ---- sol5: korelacja STR ~ read ----
  sol5 <- local({
    r <- .ch9_cor(edu$student_teacher_ratio, edu$read)
    tagList(
      tags$ul(
        tags$li(.ch9_fmt_r(r)),
        tags$li(sprintf("R² = %.3f (wspólna wariancja: %.1f%%)", r$r^2, 100*r$r^2))
      ),
      p(.ch9_decision(r$p)),
      p(tags$b("Interpretacja:"),
        sprintf(
          " r = %.3f — korelacja %s i ujemna: wyższy STR (więcej uczniów
           na nauczyciela) wiąże się z niższymi wynikami. Efekt %s —
           STR wyjaśnia tylko %.1f%% wariancji wyników. Uwaga: STR jest
           często proxy dla zasobności okręgu — konfunder dochodu może
           tłumaczyć część tej zależności.",
          r$r,
          effect_size_label(abs(r$r)),
          effect_size_label(abs(r$r)),
          100*r$r^2
        )
      )
    )
  })

  # ---- sol6: t-test grades (KK-06 vs KK-08) ----
  sol6 <- local({
    df2 <- edu[!is.na(edu$read) & !is.na(edu$grades), ]
    r <- .ch9_t2samp(df2$read, df2$grades)
    tagList(
      p(tags$b("H₀: "), "μ(KK-06) = μ(KK-08) · ", tags$b("Hₐ: "), "μ(KK-06) ≠ μ(KK-08)"),
      tags$ul(
        tags$li(sprintf("%s: n=%d, x̄=%.2f, s=%.2f", r$lvls[1], r$n1, r$m1, r$s1)),
        tags$li(sprintf("%s: n=%d, x̄=%.2f, s=%.2f", r$lvls[2], r$n2, r$m2, r$s2)),
        tags$li(.ch9_fmt_t(r)),
        tags$li(sprintf("Cohen's d = %.3f (%s efekt)", r$d, effect_size_label(r$d)))
      ),
      p(.ch9_decision(r$p)),
      p(tags$b("Interpretacja:"),
        sprintf(
          " Różnica między grupami (%.2f pkt) jest %s (p %s 0.05).
           Efekt %s (d = %.3f). Pamiętaj: różnica istotna statystycznie
           nie musi być edukacyjnie ważna — %.2f pkt na skali wyników to
           %s praktycznie.",
          abs(r$m1 - r$m2),
          if (r$p < 0.05) "istotna statystycznie" else "nieistotna statystycznie",
          if (r$p < 0.001) "<" else "=",
          effect_size_label(r$d),
          r$d,
          abs(r$m1 - r$m2),
          if (abs(r$d) < 0.2) "pomijalnie mała różnica"
          else if (abs(r$d) < 0.5) "mała różnica"
          else "znacząca różnica"
        )
      )
    )
  })

  # ---- sol7: t-test high_str (>20 vs <=20) ----
  sol7 <- local({
    high_str <- edu$student_teacher_ratio > 20
    r <- .ch9_t2samp(edu$read, high_str)
    n_hi <- sum(high_str); n_lo <- sum(!high_str)
    m_hi <- mean(edu$read[high_str]); m_lo <- mean(edu$read[!high_str])
    tagList(
      p(tags$b("H₀: "), "μ(STR≤20) = μ(STR>20) · ",
        tags$b("Hₐ: "), "μ(STR≤20) ≠ μ(STR>20)"),
      tags$ul(
        tags$li(sprintf("STR ≤ 20: n=%d, x̄=%.2f", n_lo, m_lo)),
        tags$li(sprintf("STR > 20: n=%d, x̄=%.2f", n_hi, m_hi)),
        tags$li(sprintf("Różnica: %.2f pkt", m_lo - m_hi)),
        tags$li(.ch9_fmt_t(r)),
        tags$li(sprintf("Cohen's d = %.3f (%s efekt)", abs(r$d), effect_size_label(r$d)))
      ),
      p(.ch9_decision(r$p)),
      p(tags$b("Uwaga:"),
        " STR > 20 to okręgi z większą liczbą uczniów na nauczyciela —
        a często to okręgi biedniejsze. Ujemna różnica może być konfundowana
        dochodem. Aby izolować efekt STR, potrzeba analizy regresji z kontrolą
        zmiennych towarzyszących.")
    )
  })

  # ---- sol8: chi-kwadrat grades x high_english ----
  sol8 <- local({
    high_eng <- edu$english > 20
    tab <- table(grades = edu$grades, high_english = high_eng)
    r   <- .ch9_chisq(tab)
    tagList(
      p(tags$b("H₀: "), "typ szkoły i high_english są niezależne · ",
        tags$b("Hₐ: "), "zmienne są zależne"),
      tags$table(class = "table table-bordered table-sm",
        tags$thead(tags$tr(
          tags$th("grades"),
          tags$th("high_english = FALSE"),
          tags$th("high_english = TRUE"),
          tags$th("suma")
        )),
        tags$tbody(
          tags$tr(tags$td("KK-06"),
                  tags$td(tab["KK-06", "FALSE"]), tags$td(tab["KK-06", "TRUE"]),
                  tags$td(sum(tab["KK-06", ]))),
          tags$tr(tags$td("KK-08"),
                  tags$td(tab["KK-08", "FALSE"]), tags$td(tab["KK-08", "TRUE"]),
                  tags$td(sum(tab["KK-08", ])))
        )
      ),
      tags$ul(
        tags$li(sprintf("χ²(%d) = %.3f", r$df, r$chi2)),
        tags$li(sprintf("p %s %s",
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4)))
      ),
      p(.ch9_decision(r$p)),
      p(tags$b("Interpretacja:"),
        " Test χ² mówi tylko, że zmienna jest ", tags$em("istotna"),
        " — nie jak duży jest związek ani w jakim kierunku.
        By ocenić siłę związku, oblicz Cramér's V; by zobaczyć kierunek —
        porównaj proporcje w tabeli.")
    )
  })

  # ---- sol9: chi-kwadrat high_str x high_lunch ----
  sol9 <- local({
    high_str   <- edu$student_teacher_ratio > 20
    high_lunch <- edu$lunch > 50
    tab <- table(high_str = high_str, high_lunch = high_lunch)
    r   <- .ch9_chisq(tab)
    n_both <- tab["TRUE", "TRUE"]
    p_hi_str_poor <- tab["TRUE","TRUE"] / sum(tab["TRUE",])
    p_lo_str_poor <- tab["FALSE","TRUE"] / sum(tab["FALSE",])
    tagList(
      p(tags$b("H₀: "), "high_str i high_lunch są niezależne · ",
        tags$b("Hₐ: "), "zmienne są zależne"),
      tags$table(class = "table table-bordered table-sm",
        tags$thead(tags$tr(
          tags$th("high_str"), tags$th("high_lunch = FALSE"),
          tags$th("high_lunch = TRUE"), tags$th("suma")
        )),
        tags$tbody(
          tags$tr(tags$td("FALSE"),
                  tags$td(tab["FALSE","FALSE"]), tags$td(tab["FALSE","TRUE"]),
                  tags$td(sum(tab["FALSE",]))),
          tags$tr(tags$td("TRUE"),
                  tags$td(tab["TRUE","FALSE"]), tags$td(tab["TRUE","TRUE"]),
                  tags$td(sum(tab["TRUE",])))
        )
      ),
      tags$ul(
        tags$li(sprintf("χ²(%d) = %.3f", r$df, r$chi2)),
        tags$li(sprintf("p %s %s",
          if (r$p < 0.001) "<" else "=",
          if (r$p < 0.001) "0.001" else format(round(r$p, 4), nsmall = 4))),
        tags$li(sprintf("Odsetek high_lunch wśród okręgów z STR>20: %.1f%%",
                        100 * p_hi_str_poor)),
        tags$li(sprintf("Odsetek high_lunch wśród okręgów z STR≤20: %.1f%%",
                        100 * p_lo_str_poor))
      ),
      p(.ch9_decision(r$p)),
      p(tags$b("Wniosek:"),
        " Okręgi z przeładowanymi klasami mają wyraźnie wyższy odsetek ubogich uczniów.
        STR może być wskaźnikiem zastępczym (proxy) dla zasobności — dlatego korelacja
        STR–read z Zadania 5 jest częściowo konfundowana dochodem.")
    )
  })

  # ---- sol10: ANOVA income_group x read ----
  sol10 <- local({
    quant <- quantile(edu$income, probs = c(0, 1/3, 2/3, 1))
    edu$income_group <- cut(edu$income, breaks = quant,
      labels = c("Niski", "Średni", "Wysoki"), include.lowest = TRUE)

    grp_stats <- tapply(edu$read, edu$income_group, function(x)
      c(n = length(x), m = mean(x), s = sd(x)))

    fit <- aov(read ~ income_group, data = edu)
    s   <- summary(fit)[[1]]
    F_val  <- s[["F value"]][1]
    df1    <- s[["Df"]][1]
    df2    <- s[["Df"]][2]
    p_val  <- s[["Pr(>F)"]][1]
    sst    <- sum(s[["Sum Sq"]])
    eta2   <- s[["Sum Sq"]][1] / sst

    ph <- TukeyHSD(fit)$income_group

    tagList(
      p(tags$b("H₀: "), "μ_niski = μ_średni = μ_wysoki · ",
        tags$b("Hₐ: "), "co najmniej jedna para się różni"),
      tags$table(class = "table table-bordered table-sm",
        tags$thead(tags$tr(tags$th("Tercyl"), tags$th("n"),
                           tags$th("x̄ read"), tags$th("s"))),
        tags$tbody(lapply(c("Niski","Średni","Wysoki"), function(g) {
          v <- grp_stats[[g]]
          tags$tr(tags$td(g), tags$td(v["n"]),
                  tags$td(round(v["m"],2)), tags$td(round(v["s"],2)))
        }))
      ),
      tags$ul(
        tags$li(sprintf("F(%d, %d) = %.3f", df1, df2, F_val)),
        tags$li(sprintf("p %s %s",
          if (p_val < 0.001) "<" else "=",
          if (p_val < 0.001) "0.001" else format(round(p_val, 4), nsmall = 4))),
        tags$li(sprintf("η² = %.3f (%s efekt — %.1f%% wariancji wyjaśnionej przez dochód)",
          eta2,
          if (eta2 < 0.01) "pomijalny" else if (eta2 < 0.06) "mały"
          else if (eta2 < 0.14) "średni" else "duży",
          100 * eta2))
      ),
      p(.ch9_decision(p_val)),
      p(tags$b("Post-hoc Tukey HSD:")),
      tags$ul(
        lapply(rownames(ph), function(nm) {
          pp <- ph[nm, "p adj"]
          tags$li(sprintf("%s: Δ = %.2f pkt, p %s %s",
            nm, ph[nm,"diff"],
            if (pp < 0.001) "<" else "=",
            if (pp < 0.001) "0.001" else format(round(pp, 3), nsmall = 3)
          ))
        })
      ),
      p(tags$b("Uwaga o post-hoc:"),
        " W Jamowi użyj Games-Howell (nie zakłada równych wariancji).
        Tu użyto Tukey HSD do zilustrowania idei; wyniki mogą się nieznacznie różnić.")
    )
  })

  # ---- sol11: prawda czy falsz ----
  sol11 <- local({
    r_inc  <- .ch9_cor(edu$income, edu$read)
    r_str  <- .ch9_cor(edu$student_teacher_ratio, edu$read)
    r_str_r2 <- round(100 * r_str$r^2, 1)

    tagList(
      tags$ol(
        tags$li(tags$b("Fałsz."),
          " Korelacja nie implikuje przyczynowości — jest to obserwacyjna miara
          współzależności, nie związku przyczynowego."),
        tags$li(tags$b("Prawda."),
          sprintf(" R² = r² = (%.3f)² ≈ %.3f, czyli STR wyjaśnia %.1f%% wariancji wyników.",
            r_str$r, r_str$r^2, r_str_r2)),
        tags$li(tags$b("Fałsz."),
          " Istotność statystyczna (p < 0.05) nie mówi nic o wielkości efektu.
          Trzeba sprawdzić Cohen's d — może być pomijalny."),
        tags$li(tags$b("Fałsz."),
          " Test χ² stwierdza ", tags$em("czy"), " zmienne są zależne, nie ",
          tags$em("jak bardzo"), " ani w jakim kierunku. Do opisu siły służy Cramér's V,
          do kierunku — proporcje w tabeli krzyżowej."),
        tags$li(tags$b("Fałsz."),
          " ANOVA testuje, czy ", tags$em("jakaś"), " para grup się różni —
          nie że ", tags$em("wszystkie"), " pary są różne. Post-hoc wskazuje,
          które konkretnie pary."),
        tags$li(tags$b("Fałsz."),
          " Trzy oddzielne testy t influją błąd I rodzaju (problem porównań wielokrotnych).
          Przy α = 0.05 i trzech testach prawdopodobieństwo co najmniej jednego
          fałszywie pozytywnego wyniku rośnie do ~14%."),
        tags$li(tags$b("Fałsz."),
          ' p < 0.05 oznacza, że dane byłyby mało prawdopodobne, gdyby H₀ była prawdziwa
          — nie daje "prawdopodobieństwa 95%", że H₀ jest fałszywa.'),
        tags$li(tags$b("Fałsz."),
          " Istotność statystyczna ≠ istotność praktyczna. Mała różnica (np. 1 pkt)
          może być istotna statystycznie przy dużym n, ale nieistotna dla polityków
          i praktyków.")
      )
    )
  })

  list(edu = list(
    sol1 = sol1, sol2 = sol2, sol3 = sol3, sol4 = sol4,
    sol5 = sol5, sol6 = sol6, sol7 = sol7, sol8 = sol8,
    sol9 = sol9, sol10 = sol10, sol11 = sol11
  ))
})

# ============================================================================
# SERVER
# ============================================================================

ch9_server <- function(input, output, session) {

  sol_ids <- paste0("sol", 1:11)
  btn_ids <- paste0("ans", 1:11)

  vis <- lapply(sol_ids, function(x) reactiveVal(FALSE))
  names(vis) <- sol_ids

  observeEvent(input$ch9_dataset, {
    k <- input$ch9_dataset
    for (sid in sol_ids) vis[[sid]](FALSE)
    for (bid in btn_ids) {
      updateActionButton(session, paste0("ch9_", bid), label = "Pokaż rozwiązanie")
    }
    output$ch9_content <- renderUI({
      switch(k, edu = .ch9_content_edu())
    })
  }, ignoreNULL = FALSE)

  .make_toggle <- function(sol_id_bare, sol_id_full, btn_id_full) {
    observeEvent(input[[btn_id_full]], {
      nowy <- !vis[[sol_id_bare]]()
      vis[[sol_id_bare]](nowy)
      updateActionButton(session, btn_id_full,
        label = if (nowy) "Ukryj rozwiązanie" else "Pokaż rozwiązanie")
    }, ignoreInit = TRUE)

    output[[sol_id_full]] <- renderUI({
      if (!vis[[sol_id_bare]]()) return(NULL)
      k <- isolate(input$ch9_dataset)
      div(class = "callout-success", style = "margin-top: 10px;",
          .ch9_solutions[[k]][[sol_id_bare]])
    })
  }

  mapply(.make_toggle,
    sol_id_bare = sol_ids,
    sol_id_full = paste0("ch9_", sol_ids),
    btn_id_full = paste0("ch9_", btn_ids)
  )
}
