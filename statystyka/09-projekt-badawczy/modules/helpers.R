# ============================================================================
# Helpery - projekt badawczy / TeachingRatings
# ============================================================================

proj_col_data <- unname(upwr_cat["niebo"])
proj_col_hyp  <- unname(upwr_cat["wrzos"])
proj_col_ctrl <- unname(upwr_cat["szalwia"])
proj_col_warn <- unname(upwr_cat["bursztyn"])
proj_col_risk <- upwr_accent
proj_col_ref  <- upwr_reference

load_teaching_ratings <- function() {
  dat <- NULL
  if (requireNamespace("AER", quietly = TRUE)) {
    dat <- tryCatch({
      data("TeachingRatings", package = "AER", envir = environment())
      get("TeachingRatings", envir = environment())
    }, error = function(e) NULL)
  }
  if (is.null(dat)) {
    dat <- read.csv(file.path(app_dir, "dane", "teaching_ratings.csv"),
                    stringsAsFactors = FALSE)
  }

  if ("rownames" %in% names(dat)) dat$rownames <- NULL
  dat$response.rate <- round(dat$students / dat$allstudents * 100, 1)
  dat$minority <- factor(dat$minority, levels = c("no", "yes"),
                         labels = c("nie", "tak"))
  dat$gender <- factor(dat$gender, levels = c("female", "male"),
                       labels = c("kobieta", "mężczyzna"))
  dat$credits <- factor(dat$credits, levels = c("more", "single"),
                        labels = c("więcej niż 1", "jednopunktowy"))
  dat$division <- factor(dat$division, levels = c("lower", "upper"),
                         labels = c("niższy", "wyższy"))
  dat$native <- factor(dat$native, levels = c("no", "yes"),
                       labels = c("nie", "tak"))
  dat$tenure <- factor(dat$tenure, levels = c("no", "yes"),
                       labels = c("nie", "tak"))
  dat$prof <- factor(dat$prof)
  dat$age_group <- cut(dat$age,
                       breaks = c(-Inf, 40, 55, Inf),
                       labels = c("do 40", "41-55", "powyżej 55"),
                       right = TRUE)
  dat
}

tr_data <- load_teaching_ratings()

tr_labels <- c(
  eval = "Ocena kursu",
  beauty = "Ocena atrakcyjności",
  age = "Wiek prowadzącego",
  age_group = "Grupa wieku",
  students = "Liczba odpowiedzi",
  allstudents = "Liczba zapisanych",
  response.rate = "Response rate (%)",
  gender = "Płeć",
  minority = "Mniejszość",
  native = "Native speaker",
  tenure = "Tenure track",
  division = "Poziom kursu",
  credits = "Kurs jednopunktowy (credits)"
)

tr_numeric_vars <- c("eval", "beauty", "age", "students", "allstudents", "response.rate")
tr_confounder_vars <- c("gender", "age", "tenure", "minority", "native", "credits", "division")

tr_fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  sprintf("%.3f", p)
}

tr_model_table <- function(models) {
  rows <- lapply(seq_along(models), function(i) {
    m <- models[[i]]$model
    g <- broom::glance(m)
    coefs <- broom::tidy(m)
    b <- coefs$estimate[coefs$term == "beauty"]
    p <- coefs$p.value[coefs$term == "beauty"]
    data.frame(
      model = models[[i]]$label,
      beta_beauty = b,
      p_beauty = p,
      adj_r2 = g$adj.r.squared,
      aic = AIC(m),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

tr_coef_labels <- c(
  "(Intercept)" = "Intercept",
  beauty = "Atrakcyjność",
  age = "Wiek",
  students = "Liczba odpowiedzi",
  allstudents = "Liczba zapisanych",
  response.rate = "Response rate",
  "gendermężczyzna" = "Płeć: mężczyzna",
  "minoritytak" = "Mniejszość: tak",
  "nativetak" = "Native: tak",
  "tenuretak" = "Tenure: tak",
  "divisionwyższy" = "Poziom: wyższy",
  "creditsjednopunktowy" = "Kurs jednopunktowy"
)

tr_label_term <- function(term) {
  out <- unname(tr_coef_labels[term])
  ifelse(is.na(out), term, out)
}

tr_mean_diff <- function(group_var) {
  dat <- tr_data[!is.na(tr_data$eval) & !is.na(tr_data[[group_var]]), ]
  grp <- droplevels(dat[[group_var]])
  lev <- levels(grp)
  if (length(lev) != 2) return(NULL)
  m1 <- mean(dat$eval[grp == lev[1]])
  m2 <- mean(dat$eval[grp == lev[2]])
  data.frame(
    group1 = lev[1],
    group2 = lev[2],
    mean1 = m1,
    mean2 = m2,
    diff = m2 - m1,
    stringsAsFactors = FALSE
  )
}

# Liczba z polskim przecinkiem dziesiętnym.
tr_fmt_num <- function(x, digits = 2) {
  if (is.na(x)) return("—")
  sprintf(paste0("%.", digits, "f"), x)
}

# Pełne statystyki opisowe do panelu wyniku: średnia, SD, mediana, Q1, Q3.
# Dla porównań grup: jeden wiersz na grupę (eval w danej grupie).
# Dla korelacji: wiersz dla predyktora i dla eval (obie zmienne ilościowe).
tr_desc_table <- function(id) {
  tr <- tr_tropy[[id]]
  one <- function(label, x) {
    qs <- quantile(x, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
    data.frame(
      label  = label,
      mean   = mean(x, na.rm = TRUE),
      sd     = sd(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      q1     = qs[1],
      q3     = qs[2],
      stringsAsFactors = FALSE
    )
  }
  if (tr$method == "cor") {
    rbind(
      one(unname(tr_labels[tr$var]), tr_data[[tr$var]]),
      one(unname(tr_labels["eval"]), tr_data$eval)
    )
  } else {
    dat <- tr_data[!is.na(tr_data$eval) & !is.na(tr_data[[tr$var]]), ]
    grp <- droplevels(dat[[tr$var]])
    do.call(rbind, lapply(levels(grp), function(g) one(g, dat$eval[grp == g])))
  }
}

tr_group_test <- function(group_var, method = "t") {
  dat <- tr_data[!is.na(tr_data$eval) & !is.na(tr_data[[group_var]]), ]
  dat[[group_var]] <- droplevels(dat[[group_var]])
  form <- as.formula(paste("eval ~", group_var))
  if (method == "wilcox") {
    res <- wilcox.test(form, data = dat)
    list(name = "Mann-Whitney", statistic = unname(res$statistic), p = res$p.value)
  } else {
    res <- t.test(form, data = dat)
    list(name = "test t", statistic = unname(res$statistic), p = res$p.value)
  }
}

tr_multi_group_test <- function(group_var, method = "anova") {
  dat <- tr_data[!is.na(tr_data$eval) & !is.na(tr_data[[group_var]]), ]
  form <- as.formula(paste("eval ~", group_var))
  if (method == "kruskal") {
    res <- kruskal.test(form, data = dat)
    list(name = "Kruskal-Wallis", statistic = unname(res$statistic), p = res$p.value)
  } else {
    res <- summary(aov(form, data = dat))[[1]]
    list(name = "ANOVA", statistic = unname(res$`F value`[1]), p = res$`Pr(>F)`[1])
  }
}

tr_research_verdict <- function(p, effect_text) {
  if (is.na(p)) {
    "Nie udało się policzyć testu dla tych danych."
  } else if (p < 0.05) {
    paste0("Wynik wzmacnia trop: ", effect_text,
           ". To jeszcze nie dowód przyczynowy, ale dobry powód, żeby zadać kolejne pytanie.")
  } else {
    paste0("Wynik osłabia prostą wersję tropu: ", effect_text,
           ". To nie znaczy, że temat znika; raczej trzeba zmienić hipotezę albo pomiar.")
  }
}

tr_assoc_strength <- function(x, z) {
  ok <- !is.na(x) & !is.na(z)
  x <- x[ok]
  z <- z[ok]
  if (is.numeric(z)) {
    val <- suppressWarnings(abs(cor(x, z, use = "complete.obs")))
    list(value = val, label = paste0("|r| = ", round(val, 3)))
  } else {
    z <- droplevels(factor(z))
    meds <- tapply(x, z, median, na.rm = TRUE)
    val <- max(meds, na.rm = TRUE) - min(meds, na.rm = TRUE)
    list(value = val, label = paste0("różnica median = ", round(val, 3)))
  }
}

tr_confounder_summary <- local({
  rows <- lapply(tr_confounder_vars, function(var) {
    bx <- tr_assoc_strength(tr_data$beauty, tr_data[[var]])
    ey <- tr_assoc_strength(tr_data$eval, tr_data[[var]])
    data.frame(
      variable = var,
      label = unname(tr_labels[var]),
      beauty_value = bx$value,
      beauty_label = bx$label,
      eval_value = ey$value,
      eval_label = ey$label,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
})

tr_confounder_row <- function(var) {
  tr_confounder_summary[tr_confounder_summary$variable == var, , drop = FALSE]
}

# ============================================================================
# WIĄZKA TROPÓW — jedno źródło prawdy dla całego wykładu
# ----------------------------------------------------------------------------
# Cały wykład realizuje jeden CEL badawczy: czy `eval` (ocena z ankiety) mierzy
# jakość nauczania, czy raczej mieszankę innych rzeczy? Tego celu NIE da się
# rozstrzygnąć jedną hipotezą — potrzebujemy wiązki konkurujących tropów, które
# razem oświetlają cel. Ta lista to ta sama wiązka, którą śledzimy od ciekawości
# (ch1), przez hipotezy (ch2), testy (ch5), iterację (ch6) aż po model (ch7).
# Wszystkie moduły czytają stąd, zamiast powielać własne listy.
# ============================================================================

tr_goal <- paste0(
  "Czy ocena z ankiety (eval) mierzy jakość nauczania, czy raczej mieszankę ",
  "jakości zajęć, sympatii, stereotypów i okoliczności kursu?"
)

tr_tropy <- list(
  beauty = list(
    id        = "beauty",
    short     = "Atrakcyjność",
    var       = "beauty",
    question  = "Czy prowadzący oceniani jako atrakcyjniejsi dostają wyższe oceny kursu?",
    hypothesis = "Wyższe `beauty` współwystępuje z wyższym `eval`.",
    method    = "cor",
    test_name = "korelacja Pearsona",
    analysis  = "wykres punktowy + korelacja",
    note      = "Obie zmienne są ilościowe, więc zaczynamy od związku liniowego.",
    alt = c(
      "Atrakcyjność może być powiązana z wiekiem lub płcią.",
      "Studenci mogą wyżej oceniać osoby bardziej pewne siebie, a nie wygląd sam w sobie.",
      "Efekt może zależeć od typu kursu."
    ),
    data_check = "Mamy `beauty`, `age`, `gender` oraz cechy kursu (`division`, `credits`). Nie mamy bezpośredniej miary pewności siebie ani stylu prowadzenia.",
    plan_check = "Sprawdzić związek `beauty` z `eval`, a potem uwzględnić wiek, płeć i typ kursu jako możliwe wyjaśnienia poboczne."
  ),
  gender = list(
    id        = "gender",
    short     = "Płeć",
    var       = "gender",
    question  = "Czy kobiety i mężczyźni prowadzący są oceniani podobnie?",
    hypothesis = "Średnia `eval` różni się między grupami `gender`.",
    method    = "t",
    test_name = "test t dla dwóch grup",
    analysis  = "boxplot + porównanie średnich",
    note      = "Pytanie porównuje dwie grupy prowadzących.",
    alt = c(
      "Kobiety i mężczyźni mogą prowadzić inne typy kursów.",
      "Różnice mogą wynikać z oczekiwań studentów wobec stylu prowadzenia.",
      "Nierówny response rate może zmieniać obraz."
    ),
    data_check = "Mamy `gender`, `eval`, cechy kursu oraz `response.rate`. Nie mamy danych o oczekiwaniach studentów ani stylu komunikacji prowadzącego.",
    plan_check = "Porównać oceny między grupami i sprawdzić, które cechy kursu oraz reprezentatywność odpowiedzi trzeba uwzględnić przy interpretacji."
  ),
  native = list(
    id        = "native",
    short     = "Native speaker",
    var       = "native",
    question  = "Czy status native speaker wiąże się z oceną kursu?",
    hypothesis = "Średnia `eval` różni się między `native = tak` i `native = nie`.",
    method    = "wilcox",
    test_name = "Mann-Whitney",
    analysis  = "boxplot + porównanie rozkładów",
    note      = "Używamy wariantu odpornego na nierówne i skośne grupy.",
    alt = c(
      "Status native może mieszać się z typem kursu.",
      "Native speakerzy mogą uczyć innych przedmiotów albo na innym poziomie.",
      "Grupy mogą mieć różną liczebność."
    ),
    data_check = "Mamy `native`, `division`, `credits` i liczebności grup. Nie mamy informacji o języku prowadzenia, poziomie trudności materiału ani doświadczeniu językowym studentów.",
    plan_check = "Sprawdzić, czy grupy różnią się oceną, oraz uwzględnić kontekst kursu i ostrożność przy nierównych liczebnościach."
  ),
  minority = list(
    id        = "minority",
    short     = "Mniejszość",
    var       = "minority",
    question  = "Czy prowadzący z grup mniejszościowych są oceniani inaczej?",
    hypothesis = "Średnia `eval` różni się między `minority = tak` i `minority = nie`.",
    method    = "wilcox",
    test_name = "Mann-Whitney",
    analysis  = "boxplot + porównanie rozkładów",
    note      = "To pytanie dotyczy sprawiedliwości ocen, więc wynik interpretujemy szczególnie ostrożnie.",
    alt = c(
      "Grupa mniejszościowa może być mało liczna — trudniej o stabilny wynik.",
      "Różnice mogą ujawniać się tylko w wybranych typach kursów.",
      "Status mniejszości może współwystępować z innymi cechami prowadzącego."
    ),
    data_check = "Mamy `minority`, `eval` i kilka cech prowadzącego/kursu. Nie mamy danych o doświadczeniach dyskryminacji, atmosferze zajęć ani treści komentarzy studentów.",
    plan_check = "Opisać liczebności grup, porównać oceny i sprawdzić, czy wynik nie wynika z innych cech kursu lub prowadzącego."
  ),
  response = list(
    id        = "response",
    short     = "Response rate",
    var       = "response.rate",
    question  = "Czy przy niskim odsetku odpowiedzi ocena kursu znaczy to samo?",
    hypothesis = "`response.rate` współwystępuje z `eval`.",
    method    = "cor",
    test_name = "korelacja Pearsona",
    analysis  = "wykres punktowy + korelacja",
    note      = "Sprawdzamy, czy reprezentatywność ankiety wiąże się z oceną.",
    alt = c(
      "Odpowiadają głównie osoby skrajnie zadowolone lub niezadowolone.",
      "Duże kursy mogą mieć niższy response rate.",
      "Zaangażowanie grupy może wpływać i na oceny, i na odsetek odpowiedzi."
    ),
    data_check = "Mamy `response.rate`, `students`, `allstudents` i `eval`. Nie mamy informacji, kto dokładnie odpowiedział ani dlaczego część osób pominęła ankietę.",
    plan_check = "Sprawdzić, czy odsetek odpowiedzi wiąże się z oceną i czy wielkość kursu zmienia interpretację tego związku."
  )
)

# Kolejność wiązki — jedna, używana wszędzie (od najmocniejszego do metodologicznego).
tr_trop_order <- c("beauty", "gender", "native", "minority", "response")

# ----------------------------------------------------------------------------
# tr_board_summary — prekalkulowana tablica wyników dla całej wiązki.
# Składa istniejące helpery (cor.test / tr_group_test / tr_mean_diff /
# tr_research_verdict). Liczone raz przy starcie, żeby UI był szybki.
# ----------------------------------------------------------------------------

tr_board_summary <- local({
  rows <- lapply(tr_trop_order, function(id) {
    tr <- tr_tropy[[id]]
    if (tr$method == "cor") {
      res <- cor.test(tr_data[[tr$var]], tr_data$eval)
      r <- unname(res$estimate)
      p <- res$p.value
      effect_text <- paste0("r = ", sprintf("%.3f", r))
      effect_label <- effect_text
    } else {
      res  <- tr_group_test(tr$var, tr$method)
      diff <- tr_mean_diff(tr$var)
      p <- res$p
      effect_text <- paste0("różnica średnich = ", round(diff$diff, 3))
      effect_label <- paste0("Δ = ", sprintf("%.2f", diff$diff),
                             " (", diff$group2, " − ", diff$group1, ")")
    }
    supported <- !is.na(p) && p < 0.05
    verdict <- if (is.na(p)) {
      "—"
    } else if (supported) {
      "trop wzmocniony"
    } else {
      "trop osłabiony"
    }
    data.frame(
      id        = id,
      short     = tr$short,
      question  = tr$question,
      test_name = tr$test_name,
      effect    = effect_label,
      p         = p,
      p_label   = tr_fmt_p(p),
      supported = supported,
      verdict   = verdict,
      full_verdict = tr_research_verdict(p, effect_text),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
})

tr_board_row <- function(id) {
  tr_board_summary[tr_board_summary$id == id, , drop = FALSE]
}

# ----------------------------------------------------------------------------
# tr_board_ui — narastająca tablica tropów.
#   reveal: które tropy są już "odkryte" (mają widoczny wynik). Pozostałe są
#           wyszarzone z placeholderem "—". Domyślnie wszystkie odkryte.
#   show_verdict: czy pokazać kolumnę werdyktu (włączamy od ch5 w górę).
# Ten sam komponent służy jako pusta tablica (ch1, reveal=character(0)),
# narastająca (ch5) i pełne podsumowanie (ch6, ch8, reveal=tr_trop_order).
# ----------------------------------------------------------------------------

tr_board_ui <- function(reveal = tr_trop_order, show_verdict = TRUE) {
  muted <- function(text) tags$span(class = "tropy-muted", text)
  cells <- lapply(tr_trop_order, function(id) {
    row <- tr_board_row(id)
    revealed <- id %in% reveal
    verdict_type <- if (row$supported) "ok" else "danger"
    list(
      short = row$short,
      question = row$question,
      test = if (revealed) row$test_name else muted("—"),
      effect = if (revealed) HTML(paste0(row$effect, " · p ", row$p_label))
               else muted("czeka na sprawdzenie"),
      verdict = if (revealed) lc_verdict(tags$strong(row$verdict), type = verdict_type)
                else muted("—")
    )
  })
  column_of <- function(key) I(lapply(cells, `[[`, key))
  df <- data.frame(
    short = vapply(cells, `[[`, character(1), "short"),
    question = vapply(cells, `[[`, character(1), "question"),
    stringsAsFactors = FALSE
  )
  df$test <- column_of("test")
  df$effect <- column_of("effect")
  df$verdict <- column_of("verdict")

  cols <- list(
    lc_col("short", "Trop", "row"),
    lc_col("question", "Pytanie badawcze", "text"),
    lc_col("test", "Narzędzie", "text"),
    lc_col("effect", "Miara efektu", "text"),
    if (show_verdict) lc_col("verdict", "Werdykt", "text")
  )

  lc_table(df, Filter(Negate(is.null), cols), narrow = "cards",
    row_class = ifelse(tr_trop_order %in% reveal,
                       "tropy-row tropy-row-on", "tropy-row tropy-row-off")
  )
}


# PROTOTYP mapa: etapy projektu badawczego (rozdziały 1–8) w dwóch fazach.
# current = k rysuje pasek z wyróżnionym etapem k (początek rozdziałów 2–8).
proj_stages <- list(
  list(name = "Cel",            sub = "pytanie i tropy",          col = "wrzos"),
  list(name = "Hipotezy",       sub = "alternatywne wyjaśnienia", col = "wrzos"),
  list(name = "Pomiar",         sub = "pojęcie → zmienna",        col = "wrzos"),
  list(name = "Konspekt",       sub = "plan przed analizą",       col = "wrzos"),
  list(name = "Pierwsze testy", sub = "jeden test na trop",       col = "niebo"),
  list(name = "Iteracja",       sub = "zmienne zakłócające",      col = "bursztyn"),
  list(name = "Model kontrolny", sub = "regresja wieloraka",      col = "szalwia"),
  list(name = "Wniosek",        sub = "skala i ograniczenia",     col = "accent")
)

proj_stage_map <- function(current = NULL) {
  mini <- !is.null(current)
  cvar <- function(col) if (col == "accent") "var(--upwr-accent)" else paste0("var(--upwr-cat-", col, ")")
  items <- lapply(seq_along(proj_stages), function(k) {
    s <- proj_stages[[k]]
    state <- if (!mini) "" else if (k == current) " is-current" else if (k < current) " is-done" else " is-next"
    tags$li(class = paste0("pm-step", state),
      style = paste0("--pm-c:", cvar(s$col), ";"),
      tags$span(class = "pm-num", sprintf("%02d", k)),
      tags$span(class = "pm-name", s$name),
      if (!mini) tags$span(class = "pm-sub", s$sub))
  })
  tags$div(class = paste("proj-map", if (mini) "is-mini"),
    role = "img",
    `aria-label` = paste("Etapy projektu:", paste(vapply(proj_stages, `[[`, "", "name"), collapse = ", ")),
    if (!mini) tags$div(class = "pm-phases",
      tags$span(class = "pm-phase", "Przed danymi"),
      tags$span(class = "pm-phase", "Z danymi")),
    tags$ol(class = "pm-steps", items)
  )
}


# PROTOTYP trojkat: schemat zmiennej zakłócającej Z → X, Z → Y, X → Y?
# (kopia case_confounder() z wykładu 08; tu grubość stała, liczą się kierunki).
proj_confounder <- function(nodes, edges, strength = c(0.3, 0.3, 0.3),
                            colors = c("bursztyn", "niebo", "grafit")) {
  W <- 640; H <- 250
  bw <- 214; bh <- 46
  pos <- list(z = c(320, 42), x = c(120, 205), y = c(520, 205))
  node <- function(p, label, col) {
    tags$g(class = "cf-node", style = paste0("--cf-c:var(--upwr-cat-", col, ");"),
      tags$rect(x = p[1] - bw / 2, y = p[2] - bh / 2, width = bw, height = bh, rx = 8),
      tags$text(x = p[1], y = p[2] + 6, `text-anchor` = "middle", label))
  }
  edge <- function(a, b, label, s, dashed = FALSE, lab_dx = 0, lab_dy = 0) {
    d <- b - a; u <- d / sqrt(sum(d^2))
    t0 <- min(abs((bw / 2 + 6) / u[1]), abs((bh / 2 + 6) / u[2]))
    p0 <- a + u * t0; p1 <- b - u * (t0 + 14); mid <- (p0 + p1) / 2
    tagList(
      tags$line(class = paste("cf-edge", if (dashed) "is-open"),
        x1 = p0[1], y1 = p0[2], x2 = p1[1], y2 = p1[2],
        `stroke-width` = round(1.5 + 7 * s, 1), `marker-end` = "url(#cf-arrow)"),
      tags$text(class = "cf-val", x = mid[1] + lab_dx, y = mid[2] + lab_dy,
                `text-anchor` = "middle", label))
  }
  tags$svg(class = "case-confounder", viewBox = paste(0, 0, W, H), role = "img",
    `aria-label` = sprintf("%s wiąże się z %s (%s) i z %s (%s); związek %s z %s: %s.",
      nodes[1], nodes[2], edges[1], nodes[3], edges[2], nodes[2], nodes[3], edges[3]),
    tags$defs(tags$marker(id = "cf-arrow", viewBox = "0 0 10 10", refX = 0, refY = 5,
      markerWidth = 16, markerHeight = 16, markerUnits = "userSpaceOnUse",
      orient = "auto-start-reverse", tags$path(d = "M0,0 L10,5 L0,10 z", class = "cf-head"))),
    edge(pos$z, pos$x, edges[1], strength[1], lab_dx = -70),
    edge(pos$z, pos$y, edges[2], strength[2], lab_dx = 70),
    edge(pos$x, pos$y, edges[3], strength[3], dashed = TRUE, lab_dy = -12),
    node(pos$z, nodes[1], colors[1]),
    node(pos$x, nodes[2], colors[2]),
    node(pos$y, nodes[3], colors[3])
  )
}
