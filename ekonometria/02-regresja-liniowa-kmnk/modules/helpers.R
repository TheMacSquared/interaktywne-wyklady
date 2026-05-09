# ============================================================================
# Lokalne helpery dla wykładu 02: Regresja liniowa i KMNK
# ============================================================================

# Generator danych „sklepy: metraż -> sprzedaż”. Spójny model używany
# we wszystkich rozdziałach wykładu 02.
eco02_kmnk_data <- function(n = 50, beta0 = 20, beta1 = 1.5,
                            sigma = 25, seed = 1) {
  set.seed(seed)
  x <- runif(n, 30, 200)
  y <- beta0 + beta1 * x + rnorm(n, 0, sigma)
  data.frame(
    x = x,
    y = y,
    fitted_true = beta0 + beta1 * x
  )
}

# Suma kwadratów reszt dla zadanej prostej Y = b0 + b1 X.
eco02_user_line_ssr <- function(data, b0, b1) {
  resid <- data$y - (b0 + b1 * data$x)
  sum(resid^2)
}

# Generator danych dla widgetu „suwak naruszenia”.
# zalozenie ∈ {"hetero","nieliniowosc","outliery"}, level ∈ [0, 100].
eco02_naruszenie_data <- function(zalozenie = "hetero", level = 0,
                                  n = 80, seed = 11) {
  set.seed(seed)
  x <- runif(n, 0, 100)
  base_sigma <- 8
  frac <- max(0, min(1, level / 100))

  if (zalozenie == "hetero") {
    sd_vec <- base_sigma + frac * 0.45 * x
    eps <- rnorm(n, 0, sd_vec)
    y <- 25 + 0.7 * x + eps
  } else if (zalozenie == "nieliniowosc") {
    bend <- frac * 0.04
    eps <- rnorm(n, 0, base_sigma)
    y <- 25 + 0.7 * x + bend * (x - 50)^2 + eps
  } else {
    eps <- rnorm(n, 0, base_sigma)
    y <- 25 + 0.7 * x + eps
    n_out <- round(frac * 6)
    if (n_out > 0) {
      idx <- sample(seq_len(n), n_out)
      y[idx] <- y[idx] + sample(c(-1, 1), n_out, replace = TRUE) * (40 + 20 * frac)
    }
  }
  data.frame(x = x, y = y)
}

# Werdykt opisowy dla widgetu „suwak naruszenia”.
eco02_werdykt_naruszenie <- function(zalozenie, level) {
  status <- if (level < 25) "ok" else if (level < 60) "warning" else "danger"

  opisy <- list(
    hetero = list(
      ok       = "Reszty rozkładają się równomiernie wokół zera. Wariancja składnika losowego nie zależy od X — założenie homoskedastyczności spełnione.",
      warning  = "Wariancja reszt zaczyna rosnąć z X — typowy „lejek”. Współczynniki KMNK pozostają nieobciążone, ale standardowe błędy są zaniżone, więc testy istotności mogą zwodzić.",
      danger   = "Wyraźny lejek w resztach. Bez korekty p-wartości i przedziały ufności są bezwartościowe."
    ),
    nieliniowosc = list(
      ok       = "Reszty są losowe, brak systematycznego wzorca. Liniowy model dobrze opisuje zależność.",
      warning  = "W resztach widać łagodny łuk — model liniowy nie łapie pełnego kształtu zależności. Współczynniki tracą interpretację „średniego efektu”.",
      danger   = "Wyraźny łuk — prawdziwa relacja jest krzywoliniowa. Liniowy KMNK myli się systematycznie i daje wyniki, których nie da się obronić."
    ),
    outliery = list(
      ok       = "Punkty leżą równomiernie wokół linii. Brak obserwacji ciągnących prostą za sobą.",
      warning  = "Pojawiło się kilka obserwacji oderwanych od reszty. Mogą wpływać na estymatę nachylenia — zwłaszcza gdy mają skrajne X.",
      danger   = "Skrajne obserwacje ciągną prostą za sobą. Estymata KMNK mówi więcej o tych kilku punktach niż o całej populacji."
    )
  )

  rekomendacje <- list(
    hetero       = "Zastosuj odporne błędy standardowe (HC0/HC1) albo ważoną MNK.",
    nieliniowosc = "Dodaj człon kwadratowy X² lub zlogarytmuj zmienne (log–log, log–lin).",
    outliery     = "Sprawdź dane — czy to błędy pomiaru, czy realne wyjątki. Rozważ regresję odporną."
  )

  list(
    type        = status,
    opis        = opisy[[zalozenie]][[status]],
    rekomendacja = rekomendacje[[zalozenie]]
  )
}

# Sześć scenariuszy interpretacyjnych do quizu w ch5.
# Każdy zwraca: tytul, opis, tabela (data.frame z kolumnami Wspolczynnik/Estymata/SE/t/p),
# pytanie, opcje (length 4), poprawna (1..4), wyjasnienia (length 4),
# meta (R^2, n).
eco02_scenariusz_kmnk <- function(idx) {
  scen <- list(
    list(
      tytul    = "Sprzedaż lodów ~ temperatura",
      opis     = "30 dni sezonu letniego: średnia temperatura w mieście (X, °C) i dzienna sprzedaż lodów (Y, tys. zł).",
      tabela   = data.frame(
        Wspolczynnik = c("Wyraz wolny", "Temperatura"),
        Estymata     = c("-1,20", "0,42"),
        SE           = c("0,85", "0,05"),
        t            = c("-1,41", "8,40"),
        p            = c("0,169", "<0,001")
      ),
      meta     = "R² = 0,71  ·  SE reszt = 0,52  ·  n = 30",
      pytanie  = "Jak interpretujesz współczynnik temperatury?",
      opcje    = c(
        "Każdy °C podnosi sprzedaż o 0,42 zł.",
        "Każdy °C podnosi sprzedaż średnio o 0,42 tys. zł.",
        "Sprzedaż jest 0,42 razy temperatura.",
        "0,42 to procent zmienności, którą tłumaczy temperatura."
      ),
      poprawna = 2,
      wyjasnienia = c(
        "Niedoszacowane o tysiąc — Y jest w tys. zł, więc 0,42 też.",
        "Dokładnie. Każdy dodatkowy °C → +0,42 tys. zł = +420 zł sprzedaży.",
        "To nie iloraz — β₁ to liniowe nachylenie.",
        "Procent zmienności tłumaczy R², nie b₁."
      )
    ),
    list(
      tytul    = "Wynagrodzenie ~ staż",
      opis     = "Próba 80 pracowników firmy produkcyjnej: staż pracy (X, lata) i miesięczne wynagrodzenie (Y, tys. zł).",
      tabela   = data.frame(
        Wspolczynnik = c("Wyraz wolny", "Staż"),
        Estymata     = c("3,50", "0,18"),
        SE           = c("0,42", "0,09"),
        t            = c("8,33", "2,00"),
        p            = c("<0,001", "0,048")
      ),
      meta     = "R² = 0,18  ·  SE reszt = 0,84  ·  n = 80",
      pytanie  = "p = 0,048 jest tuż pod 0,05. Co z tego wynika?",
      opcje    = c(
        "Efekt jest ‚istotny‘, ale niskie R² mówi, że staż wyjaśnia tylko 18% zmienności wynagrodzeń — wiele innych czynników gra rolę.",
        "Staż nie ma wpływu na wynagrodzenie — R² za niski.",
        "Wynik jest niejednoznaczny, lepiej zwiększyć poziom istotności do 0,10.",
        "Wystarczy 0,18 tys. zł różnicy między najmłodszym a najstarszym pracownikiem."
      ),
      poprawna = 1,
      wyjasnienia = c(
        "Tak — istotność i wielkość efektu to dwa osobne pytania. Staż istotnie podnosi pensję, ale wyjaśnia mały kawałek zmienności.",
        "p < 0,05 znaczy: dane zgodne z istnieniem efektu. Niskie R² nie unieważnia istotności.",
        "Zmiana α po obejrzeniu p-wartości to manipulacja — decyzję podejmujesz przed analizą.",
        "0,18 to wzrost na rok stażu. Różnica między 1 a 30 latami stażu to ~5,2 tys. zł."
      )
    ),
    list(
      tytul    = "Plon ~ nawóz",
      opis     = "45 gospodarstw w jednym regionie: ilość nawozu (X, kg/ha) i plon pszenicy (Y, dt/ha). Szczerze: p = 0,052.",
      tabela   = data.frame(
        Wspolczynnik = c("Wyraz wolny", "Nawóz"),
        Estymata     = c("25,0", "0,18"),
        SE           = c("3,5", "0,09"),
        t            = c("7,14", "2,00"),
        p            = c("<0,001", "0,052")
      ),
      meta     = "R² = 0,28  ·  SE reszt = 4,2  ·  n = 45",
      pytanie  = "Czy odrzucasz H₀: β₁ = 0 przy α = 0,05?",
      opcje    = c(
        "Tak, p ≈ 0,05.",
        "Nie, bo p = 0,052 ≥ α = 0,05 — formalnie nie ma podstaw do odrzucenia.",
        "Nie da się stwierdzić bez większej próby.",
        "Tak, bo R² = 0,28 jest wysokie."
      ),
      poprawna = 2,
      wyjasnienia = c(
        "‚Tuż pod‘ i ‚tuż nad‘ to dwa różne werdykty — α = 0,05 to próg, a nie sugestia.",
        "Dokładnie. Próg 0,05 jest twardy — przekroczony znaczy ‚nie odrzucamy‘. Ale warto zaraportować p i pozwolić czytelnikowi ocenić siłę dowodu.",
        "Można testować nawet na małych próbach — interpretacja po prostu uwzględnia szerokie przedziały ufności.",
        "R² i p mówią o różnych rzeczach. R² = 28% nie zmienia werdyktu p > α."
      )
    ),
    list(
      tytul    = "Cena mieszkania ~ liczba pokoi",
      opis     = "Bardzo duża próba ofert (n = 5000) na portalu nieruchomości: liczba pokoi (X) i cena (Y, tys. zł).",
      tabela   = data.frame(
        Wspolczynnik = c("Wyraz wolny", "Liczba pokoi"),
        Estymata     = c("450,0", "32,5"),
        SE           = c("8,1", "1,9"),
        t            = c("55,6", "17,1"),
        p            = c("<0,001", "<0,001")
      ),
      meta     = "R² = 0,04  ·  SE reszt = 188,0  ·  n = 5000",
      pytanie  = "p < 0,001 i R² = 0,04. Co zrobisz?",
      opcje    = c(
        "Przyjmę, że liczba pokoi mocno wpływa na cenę — p jest bardzo niskie.",
        "Statystycznie istotny, ale praktycznie pokoje wyjaśniają tylko 4% zmienności cen — sama liczba pokoi to słaby predyktor.",
        "Niski R² unieważnia istotność.",
        "Trzeba zmniejszyć próbę, żeby uzyskać sensowne R²."
      ),
      poprawna = 2,
      wyjasnienia = c(
        "Niskie p przy n = 5000 to pewność, że efekt nie jest zerowy — ale nie mówi o jego praktycznej wadze.",
        "Tak. Duża próba sprawia, że nawet drobne efekty są ‚istotne‘. R² mówi o praktycznej wadze.",
        "p i R² mierzą różne rzeczy. Niskie R² nie wyklucza istotności, zwłaszcza przy dużym n.",
        "Manipulacja próbą po analizie jest niedopuszczalna. Lepiej dodać predyktory (lokalizacja, metraż)."
      )
    ),
    list(
      tytul    = "Bezrobocie ~ wzrost PKB",
      opis     = "Dane kwartalne dla Polski 2010–2023: wzrost PKB (X, %) i zmiana stopy bezrobocia (Y, p.p. rok-do-roku). Klasyczny prawo Okuna.",
      tabela   = data.frame(
        Wspolczynnik = c("Wyraz wolny", "Wzrost PKB"),
        Estymata     = c("1,80", "-0,55"),
        SE           = c("0,32", "0,17"),
        t            = c("5,63", "-3,24"),
        p            = c("<0,001", "0,003")
      ),
      meta     = "R² = 0,42  ·  SE reszt = 0,68  ·  n = 56",
      pytanie  = "Jak zinterpretujesz ujemne nachylenie?",
      opcje    = c(
        "Ujemny znak to błąd modelu — bezrobocie powinno rosnąć z PKB.",
        "Każdy dodatkowy 1 p.p. wzrostu PKB obniża bezrobocie średnio o 0,55 p.p.",
        "Bezrobocie maleje o 55% z każdym 1% PKB.",
        "Wzrost PKB jest zawsze 0,55 razy zmiana bezrobocia."
      ),
      poprawna = 2,
      wyjasnienia = c(
        "Ujemny znak jest tu zgodny z teorią (Okun): wyższy wzrost gospodarczy ↘ niższe bezrobocie. Znak warto sprawdzać merytorycznie, ale tu jest poprawny.",
        "Tak — typowa interpretacja prawa Okuna na danych polskich.",
        "0,55 to p.p., nie procent. ‚Bezrobocie spada o 0,55 punktu procentowego‘.",
        "To nie iloraz — β₁ to liniowy efekt zmiany X o jednostkę."
      )
    ),
    list(
      tytul    = "Wydatki na reklamę ~ przychód firmy",
      opis     = "Próba 200 firm handlowych: roczne wydatki na reklamę (X, tys. zł) i roczny przychód (Y, tys. zł). Statystycznie wszystko gra.",
      tabela   = data.frame(
        Wspolczynnik = c("Wyraz wolny", "Reklama"),
        Estymata     = c("180,0", "8,40"),
        SE           = c("22,5", "0,42"),
        t            = c("8,00", "20,0"),
        p            = c("<0,001", "<0,001")
      ),
      meta     = "R² = 0,67  ·  SE reszt = 145,0  ·  n = 200",
      pytanie  = "Świetne wyniki: R² = 67%, p < 0,001. Czy z tego wynika, że reklama PODNOSI przychód?",
      opcje    = c(
        "Tak — model jest istotny, więc reklama wpływa na przychód.",
        "Tak, każde 1 tys. zł reklamy daje 8,4 tys. zł dodatkowego przychodu.",
        "Statystycznie tak, ale przyczynowo nie. Większe firmy stać na większą reklamę i mają większe przychody — to nie znaczy, że reklama jest sprawcą.",
        "Nie, p < 0,001 znaczy że nie ma żadnego efektu."
      ),
      poprawna = 3,
      wyjasnienia = c(
        "‚Istotny‘ to korelacja, nie przyczynowość. Tu typowy problem endogeniczności.",
        "To opis korelacji, nie efektu kauzalnego. Gdyby firma musiała wybrać między 0 a 1 mln reklamy, niekoniecznie zarobiłaby 8,4 mln więcej.",
        "Tak. Bez eksperymentu lub instrumentu nie odróżnimy ‚reklama → przychód‘ od ‚przychód → reklama‘ ani od wspólnej przyczyny (wielkość firmy).",
        "p < 0,001 znaczy ‚istotność statystyczna‘, nie ‚brak efektu‘ — odwrotnie."
      )
    )
  )
  if (idx < 1 || idx > length(scen)) return(NULL)
  scen[[idx]]
}
