# ============================================================================
# CHAPTER 4: Przedział dla proporcji
# ============================================================================

# ============================================================================
# PRZYKŁADY A1–C1 (case studies): rycina lcSF „s4-case” (sf_examples.js)
# Wspólne części (s4_case_*) są w cases_common.R; tu dane i opisy proporcji.
# ============================================================================

ch4_case_ci_prop <- function(x, n, conf = 0.95) {
  phat <- x / n
  z_star <- qnorm(1 - (1 - conf) / 2)
  se <- sqrt(phat * (1 - phat) / n)
  me <- z_star * se
  list(phat = phat, lower = phat - me, upper = phat + me, me = me, se = se, z_star = z_star)
}
ch4_case_ci_diff <- function(x1, n1, x2, n2, conf = 0.95) {
  p1 <- x1 / n1; p2 <- x2 / n2
  se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  z_star <- qnorm(1 - (1 - conf) / 2)
  diff <- p1 - p2
  me <- z_star * se
  list(diff = diff, lower = diff - me, upper = diff + me, me = me, se = se, z_star = z_star, p1 = p1, p2 = p2)
}

ch4_cases <- list(
  A1 = list(
    type = "single_prop",
    data = list(x = 212, n = 400),
    xlab = "Poparcie dla partii X",
    steps = c("1. Próba", "2. p̂", "3. ± SE", "4. Przedział"),
    hypotheses = list(
      list(text = "Poparcie dla partii X przekracza 50% (próg większości)",
           bound = 0.50, dir = "gt",
           explain_maybe = "CI (ok. 48–58%) obejmuje 50%. Mimo że p̂ = 53%, niepewność sondażu nie pozwala stwierdzić z 95% ufnością, że poparcie przekracza 50%."),
      list(text = "Poparcie dla partii X przekracza 60%",
           bound = 0.60, dir = "gt",
           explain_no = "Górna granica CI leży poniżej 60%. Cały CI leży poza obszarem hipotezy, więc nie ma podstaw do twierdzenia, że poparcie przekracza 60%.")
    )
  ),
  A2 = list(
    type = "compare_n_prop",
    data = list(phat = 0.08, ns = c(50, 200, 1000)),
    xlab = "Odsetek wadliwych produktów",
    steps = c("1. n = 50", "2. n = 200", "3. n = 1000"),
    hypotheses = list(
      list(text = "Odsetek wadliwych produktów przekracza 5%",
           bound = 0.05, dir = "gt",
           explain_yes = "Dla największej próby (n = 1000) dolna granica CI leży powyżej 5%, więc z 95% ufnością odsetek wadliwych przekracza normę 5%. Dla n = 50 i n = 200 CI obejmuje 5%, więc na mniejszej próbie nie dałoby się tego stwierdzić."),
      list(text = "Odsetek wadliwych produktów przekracza 12%",
           bound = 0.12, dir = "gt",
           explain_no = "Dla n = 1000 górna granica CI leży poniżej 12%. Dla n = 50 CI sięga prawie 16%, więc na małej próbie tej hipotezy nie dałoby się wykluczyć. Duże n daje bardziej jednoznaczne odpowiedzi.")
    )
  ),
  B1 = list(
    type = "diff_props",
    data = list(x1 = 124, n1 = 200, x2 = 84, n2 = 200,
                label1 = "Lek", label2 = "Placebo"),
    xlab = "Odsetek z ustąpieniem bólu",
    steps = c("1. Próby", "2. Dwie p̂", "3. Różnica", "4. ± SE", "5. Przedział"),
    hypotheses = list(
      list(text = "Lek działa skuteczniej niż placebo (różnica > 0)",
           bound = 0, dir = "gt",
           explain_yes = "Cały CI dla różnicy leży powyżej 0. Lek pomaga skuteczniej niż placebo, różnica jest istotna statystycznie."),
      list(text = "Lek poprawia skuteczność o więcej niż 25 punktów procentowych",
           bound = 0.25, dir = "gt",
           explain_maybe = "CI dla różnicy (ok. 10–30 punktów procentowych) obejmuje 25 punktów. Lek działa, ale dane nie rozstrzygają, czy poprawa względem placebo przekracza 25 punktów procentowych.")
    )
  ),
  B2 = list(
    type = "diff_props",
    data = list(x1 = 22, n1 = 250, x2 = 18, n2 = 250,
                label1 = "Linia A", label2 = "Linia B"),
    xlab = "Odsetek wadliwych",
    steps = c("1. Próby", "2. Dwie p̂", "3. Różnica", "4. ± SE", "5. Przedział"),
    hypotheses = list(
      list(text = "Linia A produkuje więcej braków niż linia B (różnica > 0)",
           bound = 0, dir = "gt",
           explain_maybe = "CI dla różnicy obejmuje 0. Mimo że p̂₁ (8.8%) jest wyższe niż p̂₂ (7.2%), nie możemy z 95% ufnością stwierdzić, że linia A jest gorsza. Różnica może być efektem przypadku."),
      list(text = "Linia A ma najwyżej o 5 punktów procentowych więcej braków niż B (różnica < 0.05)",
           bound = 0.05, dir = "lt",
           explain_maybe = "Górna granica CI (ok. 6.4 punktu procentowego) przekracza 5 punktów, więc nie możemy wykluczyć, że linia A jest gorsza o więcej niż 5 punktów procentowych. Żeby to rozstrzygnąć, potrzebna byłaby większa próba.")
    )
  ),
  B3 = list(
    type = "diff_props",
    data = list(x1 = 6, n1 = 30, x2 = 9, n2 = 30,
                label1 = "Zakład A", label2 = "Zakład B"),
    xlab = "Odsetek wypadków",
    steps = c("1. Próby", "2. Dwie p̂", "3. Różnica", "4. ± SE", "5. Przedział"),
    hypotheses = list(
      list(text = "Zakład A jest bezpieczniejszy niż B (różnica < 0)",
           bound = 0, dir = "lt",
           explain_maybe = "Mimo że p̂₁ = 20% jest wyraźnie mniejsze od p̂₂ = 30%, CI dla różnicy obejmuje 0. Próba 30 osób w każdym zakładzie to za mało, żeby z 95% ufnością stwierdzić, który jest bezpieczniejszy. To klasyczna pułapka: „duża” różnica w punktach procentowych może być statystycznie nieistotna przy małej próbie."),
      list(text = "Zakład A ma wypadkowość wyższą o ponad 30 punktów procentowych (różnica > 0.30)",
           bound = 0.30, dir = "gt",
           explain_no = "Górna granica CI (ok. 12 punktów procentowych) leży poniżej 30, więc dane wykluczają, że A jest aż o 30 punktów gorszy od B. W drugą stronę przedział sięga ok. -32 punktów: dużej przewagi B nad A wykluczyć nie można. Mała próba daje przedział zbyt szeroki, żeby wskazać, jaka jest różnica.")
    )
  ),
  C1 = list(
    type = "forest_prop",
    data = list(
      groups = c("Szpital A", "Szpital B", "Szpital C", "Szpital D"),
      x = c(12, 18, 9, 35),
      n = c(150, 180, 160, 170)
    ),
    xlab = "Odsetek powikłań pooperacyjnych",
    steps = c("1. Liczby", "2. Proporcje", "3. CI"),
    hypotheses = list(
      list(kind = "pairwise",
           text = "Które szpitale różnią się istotnie odsetkiem powikłań?",
           unit = "")
    )
  )
)

# Dane ryciny dla przykładu: liczebności, przedziały, osie i napisy.
ch4_case_config <- function(cfg) {
  d <- cfg$data; hyps <- cfg$hypotheses
  switch(cfg$type,
    single_prop = {
      ci <- ch4_case_ci_prop(d$x, d$n)
      # Stałe zbliżenie CI i wszystkich progów, niezależne od kroku i wyboru hipotezy.
      bounds <- vapply(hyps, function(h) h$bound, numeric(1))
      lim <- function(h) {
        xl <- range(c(ci$lower, ci$upper, bounds, h$bound))
        pad <- max(diff(xl) * 0.2, 0.01)
        c(max(0, xl[1] - pad), min(1, xl[2] + pad))
      }
      list(counts = c(d$n - d$x, d$x), phat = ci$phat, se = ci$se, lo = ci$lower, hi = ci$upper,
           y = s4_expand(c(-0.6, 0.6)), lims = s4_lims_by_hyp(hyps, lim),
           lab_phat = paste0("p̂ = ", round(ci$phat, 3)), lab_se = paste0("± SE = ±", round(ci$se, 3)),
           lab_ci = paste0("95% CI: [", round(ci$lower, 3), " ; ", round(ci$upper, 3), "]"))
    },
    compare_n_prop = {
      cis <- lapply(d$ns, function(n) ch4_case_ci_prop(round(d$phat * n), n))
      ph <- vapply(cis, `[[`, 0, "phat")
      lo <- vapply(cis, `[[`, 0, "lower"); hi <- vapply(cis, `[[`, 0, "upper")
      lim <- function(h) {
        xl <- c(max(0, min(lo) - 0.05), min(1, max(hi) + 0.05))
        if (!is.null(h)) xl <- pmin(1, pmax(0, range(c(xl, h$bound))))
        xl
      }
      list(phat = ph, lo = lo, hi = hi, y = s4_expand(c(0.3, length(d$ns) + 0.7)),
           lims = s4_lims_by_hyp(hyps, lim), lab_n = paste0("n = ", d$ns),
           lab_ci = paste0("[", round(lo, 3), " ; ", round(hi, 3), "]"))
    },
    diff_props = {
      cd <- ch4_case_ci_diff(d$x1, d$n1, d$x2, d$n2)
      lim <- function(h) {
        xl <- range(c(-0.3, 0.3, cd$diff - 1.3 * cd$me, cd$diff + 1.3 * cd$me, h$bound))
        xl + c(-1, 1) * diff(xl) * 0.08
      }
      list(label1 = d$label1, label2 = d$label2,
           counts = c(d$n1 - d$x1, d$x1, d$n2 - d$x2, d$x2),
           p1 = cd$p1, p2 = cd$p2, diff = cd$diff, se = cd$se, lo = cd$lower, hi = cd$upper,
           y_top = s4_expand(c(0.4, 2.6)), y_bot = s4_expand(c(-0.55, 0.55)),
           lims = s4_lims_by_hyp(hyps, lim),
           xlab_bot = paste0("Różnica proporcji  —  ", d$label1, " − ", d$label2),
           lab_p1 = paste0("p̂₁ = ", round(cd$p1, 3)), lab_p2 = paste0("p̂₂ = ", round(cd$p2, 3)),
           lab_diff = paste0("p̂₁ − p̂₂ = ", round(cd$diff, 3)),
           lab_se = paste0("± SE = ±", round(cd$se, 3)),
           lab_ci = paste0("95% CI: [", round(cd$lower, 3), " ; ", round(cd$upper, 3), "]"))
    },
    forest_prop = {
      cis <- lapply(seq_along(d$groups), function(i) ch4_case_ci_prop(d$x[i], d$n[i]))
      ph <- vapply(cis, `[[`, 0, "phat")
      lo <- vapply(cis, `[[`, 0, "lower"); hi <- vapply(cis, `[[`, 0, "upper")
      lim <- function(h) c(max(0, min(lo) - 0.03), min(1, max(hi) + 0.12))
      list(phat = ph, lo = lo, hi = hi, y = s4_expand(c(0.3, length(d$groups) + 0.7)),
           lims = s4_lims_by_hyp(hyps, lim),
           lab_groups = paste(d$groups, paste0(d$x, "/", d$n), sep = " · "))
    }
  )
}

ch4_case_verdict <- function(cfg) function(hyp) {
  d <- cfg$data
  ci <- switch(cfg$type,
    single_prop    = ch4_case_ci_prop(d$x, d$n),
    # Werdykt dla największego n (najbardziej precyzyjny CI).
    compare_n_prop = ch4_case_ci_prop(round(d$phat * max(d$ns)), max(d$ns)),
    diff_props     = ch4_case_ci_diff(d$x1, d$n1, d$x2, d$n2))
  s4_case_verdict(ci$lower, ci$upper, hyp$bound, hyp$dir)
}

# Macierz nakładania CI: TRUE, gdy przedziały są rozłączne.
ch4_case_pairs <- function(d) {
  k <- length(d$groups)
  cis <- lapply(seq_len(k), function(i) ch4_case_ci_prop(d$x[i], d$n[i]))
  m <- matrix(FALSE, nrow = k, ncol = k, dimnames = list(d$groups, d$groups))
  for (i in seq_len(k)) for (j in seq_len(k)) {
    if (i == j) next
    m[i, j] <- (cis[[i]]$upper < cis[[j]]$lower) || (cis[[j]]$upper < cis[[i]]$lower)
  }
  m
}

# Narracja „jak w raporcie” dla mapy par (proporcje, prezentacja w %).
ch4_case_narrative <- function(d, mat) {
  groups <- d$groups
  phats <- d$x / d$n
  k <- length(groups)

  diff_pairs <- list()
  for (i in seq_len(k - 1)) for (j in seq(i + 1, k)) {
    if (mat[i, j]) {
      diff_pairs[[length(diff_pairs) + 1]] <- if (phats[i] > phats[j]) {
        list(hi = groups[i], lo = groups[j])
      } else {
        list(hi = groups[j], lo = groups[i])
      }
    }
  }
  n_diff <- length(diff_pairs)

  if (n_diff == 0) {
    return(paste0(
      "Żadna para grup nie wykazała istotnej różnicy w odsetkach — ",
      "wszystkie 95% CI nakładają się wzajemnie. Na podstawie tych danych ",
      "nie możemy stwierdzić różnic między grupami."
    ))
  }

  # Czy jedna grupa odstaje od wszystkich innych?
  standout_idx <- which(sapply(seq_len(k), function(i) all(mat[i, -i])))
  if (length(standout_idx) == 1) {
    i <- standout_idx
    others <- phats[-i]
    direction <- if (phats[i] > max(others)) "wyższy" else "niższy"
    return(paste0(
      "Spośród wszystkich badanych grup wyraźnie odstaje ",
      tags$b(groups[i]), " (odsetek ", round(phats[i] * 100, 1), "%) — ma istotnie ",
      direction, " odsetek niż każda z pozostałych grup ",
      "(jego 95% CI nie nakłada się z żadnym innym). ",
      "Pozostałe grupy mają odsetki w przedziale ",
      round(min(others) * 100, 1), "%–", round(max(others) * 100, 1), "%, ",
      "a ich CI nakładają się — nie możemy stwierdzić między nimi istotnych różnic."
    ))
  }

  pair_strs <- sapply(diff_pairs, function(pp) paste0(tags$b(pp$hi), " > ", tags$b(pp$lo)))
  pairs_inline <- if (length(pair_strs) == 1) {
    pair_strs[1]
  } else if (length(pair_strs) == 2) {
    paste(pair_strs, collapse = " oraz ")
  } else {
    paste0(paste(pair_strs[-length(pair_strs)], collapse = ", "),
           " oraz ", pair_strs[length(pair_strs)])
  }
  intro <- if (n_diff == 1) {
    "Spośród wszystkich porównań jedynie jedna para wykazała istotną różnicę: "
  } else {
    paste0("Istotne różnice (CI nie nakładają się) wykazały ", n_diff, " pary: ")
  }
  paste0(
    intro, pairs_inline, ". ",
    "Pozostałe pary nie różnią się istotnie — ich 95% CI nakładają się, ",
    "więc na podstawie tych danych nie możemy między nimi rozróżnić."
  )
}

ch4_case_ui <- function(case_id) {
  cfg <- ch4_cases[[case_id]]
  id <- paste0("ch4_case", case_id)
  pairwise <- function(hyp) {
    mat <- ch4_case_pairs(cfg$data)
    lc_status(
      p(tags$strong("Hipoteza:"), " ", hyp$text),
      p(tags$strong("Werdykt — macierz par:")),
      lc_caption("✓ = grupy różnią się istotnie (CI nie nakładają się);  ",
                 "× = nie można stwierdzić różnicy (CI nakładają się)"),
      s4_pairwise_table(mat),
      p(tags$strong("Jak to opisać w raporcie:")),
      p(tags$em(HTML(ch4_case_narrative(cfg$data, mat))))
    )
  }
  s4_case_widget(id, cfg, ch4_case_config(cfg),
                 s4_case_notes(id, cfg$hypotheses, ch4_case_verdict(cfg), pairwise))
}

# Ryc. 4.1 i 4.3 (s04-pstep, s04-pdstep): kilka wykresów lcSF w siatce o stałej
# proporcji, jak układ patchwork w R. Wykres wypełnia swoją komórkę siatki.
s04_grid <- function(columns, ratio, max_height, ..., rows = 1, zoom_columns = columns) {
  tagList(
    # Lupa (data-lc-chart-large): siatka rośnie jak pojedyncze wykresy lcSF (min. 24em na wiersz);
    # zoom_columns poszerza kolumnę, gdy większe napisy by się w niej nie zmieściły.
    tags$style(".lc-sf[data-lc-chart-large='true'] .s04-grid { max-height: none !important;
                aspect-ratio: auto !important; min-height: calc(24em * var(--s04-rows, 1)) !important;
                grid-template-columns: var(--s04-zoom-cols) !important; }"),
    tags$div(class = "s04-grid", style = paste0("display:grid;grid-template-columns:", columns,
                          ";grid-auto-rows:1fr;gap:.4em;aspect-ratio:", ratio,
                          ";max-height:", max_height, ";min-height:18em;--s04-rows:", rows,
                          ";--s04-zoom-cols:", zoom_columns, ";"), ...))
}
s04_grid_chart <- function(k, rows = 1) {
  tags$div(class = "lc-sf-chart", `data-k` = k,
    style = paste0("height:100%;min-height:0;max-height:none;",
                   if (rows > 1) paste0("grid-row:span ", rows, ";")),
    tags$div(class = "lc-ec", `data-chart` = k, role = "img", `aria-label` = "Wykres"))
}

ch4_ui <- list(
  id    = "ch-proporcja",
  num   = "04",
  title = "Przedział dla proporcji",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 04 · Przedziały ufności",
      num    = "04",
      title  = "Przedział dla proporcji.",
      lead   = "Odsetek zdających, poparcie w sondażu, udział wadliwych sztuk:
                wiele pytań dotyczy proporcji, a nie średniej. Przedział buduje
                się tak samo jak dla średniej, inny jest tylko wzór na błąd
                standardowy."
    ),

    lc_h2("ch4-wzor", "Wzór"),

    lc_p("W poprzednim rozdziale przedział dla średniej składał się z trzech
      elementów: ", gloss("estymator", "estymatora"), " \\(\\bar{x}\\), błędu standardowego \\(s/\\sqrt{n}\\)
      i wartości krytycznej \\(t^*\\). Wiele pytań badawczych dotyczy jednak
      odsetka: jaka część studentów zdała egzamin, jaka część wyborców popiera
      partię, ile procent produktów jest wadliwych. Każda pojedyncza odpowiedź
      ma tu dwa warianty, TAK albo NIE, a ", gloss("parametr", "parametrem"), " ", gloss("populacja", "populacji"), " jest proporcja
      \\(p\\), czyli odsetek odpowiedzi TAK."),

    lc_p("Estymatorem \\(p\\) jest ", gloss("proporcja z próby"), " \\(\\hat{p}\\):
      liczba odpowiedzi TAK \\(x\\) podzielona przez ", gloss("liczebność"), " próby \\(n\\)."),

    lc_formula_annotated(
      "⟦1|\\hat{p}⟧ = \\frac{⟦2|x⟧}{⟦3|n⟧}",
      notes  = list("proporcja z próby", "liczba odpowiedzi TAK", "liczba obserwacji"),
      colors = c("accent", "niebo", "szalwia"),
      above  = 2
    ),

    lc_p("Jak bardzo \\(\\hat{p}\\) waha się z próby na próbę, wynika z wykładu 03.
      Liczba odpowiedzi TAK wśród \\(n\\) niezależnych odpowiedzi ma ",
      gloss("rozkład dwumianowy"), " B(n, p), z wartością oczekiwaną
      \\(E(X) = np\\) i ", gloss("wariancja", "wariancją"), " \\(Var(X) = np(1-p)\\). Proporcja z próby to
      \\(X\\) podzielone przez \\(n\\), więc \\(E(\\hat{p}) = p\\), a wariancja
      dzieli się przez \\(n^2\\) i wynosi \\(p(1-p)/n\\). Pierwiastek z niej to ",
      gloss("błąd standardowy"), " proporcji. CTG z wykładu 03 dodaje drugą
      informację: \\(\\hat{p}\\) jest średnią z \\(n\\) zer i jedynek, więc przy
      dużej próbie ma w przybliżeniu rozkład normalny."),

    lc_formula_annotated(
      "E(⟦1|\\hat{p}⟧) = ⟦2|p⟧ \\qquad ⟦3|SE⟧(\\hat{p}) = \\sqrt{\\frac{p(1-p)}{⟦4|n⟧}}",
      notes  = list("proporcja z próby", "proporcja w populacji", "błąd standardowy",
                    "liczba obserwacji"),
      colors = c("accent", "param", "wrzos", "szalwia"),
      above  = c(1, 3)
    ),

    lc_p("Prawdziwego \\(p\\) nie znamy, więc we wzorze na SE zastępujemy je przez
      \\(\\hat{p}\\). Dalej konstrukcja jest taka sama jak w rozdziale 3:
      estymator ± wartość krytyczna · SE. Ponieważ opieramy się na przybliżeniu
      normalnym, ", gloss("wartość krytyczna"), " pochodzi z ",
      gloss("rozkład normalny", "rozkładu normalnego"), ".
      Dla poziomu 95% to \\(z^* = 1.96\\),
      jak w wykładzie 03. Tak zbudowany ", gloss("przedział ufności"),
      " nazywa się ", gloss("przedział Walda", "przedziałem Walda"), "."),

    lc_formula_annotated(
      "⟦1|CI⟧ = ⟦2|\\hat{p}⟧ \\pm ⟦3|z^*_{\\alpha/2}⟧ \\cdot \\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{⟦4|n⟧}}",
      notes  = list("przedział ufności", "proporcja z próby", "wartość krytyczna",
                    "liczba obserwacji"),
      colors = c("niebo", "accent", "terakota", "szalwia"),
      above  = c(2, 3)
    ),

    lc_p("Przedział Walda opiera się na przybliżeniu normalnym, a ono ma swoje
      granice. Najpierw zobaczymy, jak przedział działa w typowej sytuacji,
      a potem, kiedy przybliżenie zawodzi."),

    lc_h2("ch4-budowa", "Budowa przedziału — krok po kroku"),

    lc_p("Panel przeprowadza tę konstrukcję na jednej próbie. Symulujemy
      50 odpowiedzi TAK/NIE, na przykład pytając 50 studentów, czy zdali
      egzamin, z populacji, w której prawdziwy odsetek TAK wynosi 60%.
      Przycisk „Nowa próba” losuje kolejnych 50 odpowiedzi."),

    figure_panel(
      label = "Ryc. 4.1",
      full_width = TRUE,
      # Rycina lcSF „s04-pstep” (sf_steps.js): słupki TAK/NIE | oś proporcji (1 : 2.5).
      lc_sf_figure("s04-pstep", bare = TRUE, body = lc_step_widget("ch4_step",
        title = "Konstruowanie przedziału",
        steps = c("Próba", "p̂", "± SE", "Przedział"),
        toolbar = lc_toolbar(
          lc_sf_action("new", "ch4_step_new_sample", "Nowa próba", icon = "shuffle",
                       variant = "outline"), lc_chart_zoom_button()
        ),
        body = s04_grid("1fr 2.5fr", "2.5/1", "420px", s04_grid_chart(0), s04_grid_chart(1))
      ))
    ),

    lc_p("Przy \\(p = 0.6\\) i \\(n = 50\\) błąd standardowy wynosi około
      \\(\\sqrt{0.6 \\cdot 0.4 / 50} \\approx 0.069\\), a margines błędu
      \\(1.96 \\cdot 0.069 \\approx 0.14\\). Przedział ma więc szerokość
      około 27 punktów procentowych: dla \\(\\hat{p} = 0.60\\) sięga od 46%
      do 74%. Kolejne próby przesuwają \\(\\hat{p}\\), a razem z nim cały
      przedział. Szerokość zmienia się przy tym niewiele, bo zależy od
      \\(\\hat{p}\\) tylko przez iloczyn \\(\\hat{p}(1-\\hat{p})\\), który w okolicy
      0.5 prawie się nie zmienia."),

    lc_p("Tak jak w rozdziale 2, poziom 95% opisuje metodę, a nie pojedynczy
      przedział. Konkretny przedział albo obejmuje 0.6, albo nie. Przy tych
      parametrach przedział Walda trafia w prawdziwe \\(p\\) w 94.1% prób,
      czyli niemal tak często, jak obiecuje. Przybliżenie normalne działa
      tu dobrze, bo w próbie jest typowo około 30 odpowiedzi TAK i 20 NIE."),

    lc_h2("ch4-wald-granice", "Kiedy przybliżenie zawodzi"),

    lc_p("Inaczej jest, gdy próba jest mała albo \\(\\hat{p}\\) leży blisko 0
      lub 1, a odpowiedzi TAK albo NIE jest tylko kilka. Rozkład dwumianowy jest
      wtedy wyraźnie skośny, jak B(50, 0.1) w wykładzie 03, a 95-procentowy
      przedział Walda obejmuje prawdziwe \\(p\\) rzadziej, niż obiecuje. Dla
      \\(p = 0.08\\) i \\(n = 50\\) jego rzeczywiste ", gloss("pokrycie"),
      " wynosi około 91%. Przy 4 wadliwych sztukach na 50 Wald daje przedział
      od 0.5% do 15.5%, a ", gloss("przedział Wilsona"), ", który poprawia wzór
      Walda, od 3.2% do 18.8%. Panel pokazuje oba przedziały dla kontroli
      jakości: ile sztuk sprawdzono i ile z nich okazało się wadliwych."),

    fig("4a"),

    lc_p("Przy dwóch wadliwych sztukach na 50 dolna granica Walda spada do −1.4%,
      czyli poniżej najmniejszego możliwego odsetka. Przy zerze braków
      \\(\\hat{p} = 0\\), więc SE też wynosi zero, a przedział Walda kurczy się
      do jednego punktu, jakby braków w całej produkcji na pewno nie było.
      Tymczasem brak wad w próbie nie oznacza braku wad w populacji: Wilson
      dla 0 z 50 daje przedział od 0 do 7.1%. Im bliżej 0 lub 1 leży proporcja
      i im mniejsza jest próba, tym gorzej działa przybliżenie Walda.
      W tym rozdziale liczymy przedziały Walda, bo ich wzór najprościej pokazuje
      konstrukcję. W części przykładów poniżej sukcesów albo porażek jest
      niewiele i tam przedziały są tylko przybliżone."),

    lc_p("Programy statystyczne często nie podają przedziału Walda, tylko ",
      gloss("przedział Cloppera-Pearsona", "przedział Cloppera-Pearsona"),
      ", oparty bezpośrednio na rozkładzie dwumianowym i bezpieczniejszy przy
      małych próbach. Jego granice mogą się więc nieco różnić od przedziałów
      Walda liczonych w tym rozdziale."),

    lc_h2("ch4-roznica", "Budowa przedziału dla różnicy proporcji"),

    lc_p("Częściej niż jeden odsetek porównujemy dwa: zdawalność w dwóch grupach,
      skuteczność leku i placebo, odsetek braków na dwóch liniach produkcyjnych.
      Parametrem jest wtedy różnica \\(p_1 - p_2\\), a jej estymatorem różnica
      proporcji z prób \\(\\hat{p}_1 - \\hat{p}_2\\). Gdy próby są niezależne,
      wariancja różnicy jest sumą wariancji obu proporcji, tak samo jak przy
      różnicy średnich w rozdziale 3. Błąd standardowy różnicy to pierwiastek
      z tej sumy:"),

    lc_formula_annotated(
      "CI = (⟦1|\\hat{p}_1 - \\hat{p}_2⟧) \\pm ⟦2|z^*⟧ \\cdot \\sqrt{\\frac{\\hat{p}_1(1-\\hat{p}_1)}{⟦3|n_1⟧} + \\frac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}}",
      notes  = list("różnica proporcji z prób", "wartość krytyczna", "liczebność grupy 1"),
      colors = c("accent", "terakota", "szalwia"),
      above  = 1
    ),

    lc_p("Dodają się wariancje, a nie błędy standardowe. Dlatego SE różnicy
      jest większy od SE każdej z proporcji, ale mniejszy od ich sumy. Panel
      losuje dwie niezależne próby po 60 osób z populacji, w których odsetek
      osób zadowolonych z usługi wynosi 70% i 50%."),

    figure_panel(
      label = "Ryc. 4.3",
      full_width = TRUE,
      # Rycina lcSF „s04-pdstep” (sf_steps.js): słupki obu grup | p̂ grup nad skalą różnicy.
      lc_sf_figure("s04-pdstep", bare = TRUE, body = lc_step_widget("ch4_dstep",
        title = "Konstruowanie CI dla różnicy",
        steps = c("Dwie próby", "Dwie p̂", "Różnica", "± SE", "Przedział"),
        toolbar = lc_toolbar(
          lc_sf_action("new", "ch4_dstep_new_sample", "Nowe próby", icon = "shuffle",
                       variant = "outline"), lc_chart_zoom_button()
        ),
        body = s04_grid("1fr 2fr", "2/1", "460px", rows = 2, zoom_columns = "1.5fr 2fr",
                        s04_grid_chart(0, rows = 2), s04_grid_chart(1), s04_grid_chart(2))
      ))
    ),

    lc_p("Dla tych parametrów SE obu proporcji wynosi około 0.059 i 0.065,
      a SE różnicy około 0.088, czyli mniej niż suma 0.124. Margines błędu to
      \\(1.96 \\cdot 0.088 \\approx 0.17\\), więc przedział dla różnicy ma
      szerokość około 34 punktów procentowych. To więcej niż przedział dla
      jednej proporcji na Ryc. 4.1, choć każda z prób jest większa, bo przedział
      różnicy zbiera niepewność z obu prób."),

    lc_p("Pionowa linia w zerze oznacza brak różnicy. Przedział, który jej nie
      obejmuje, wskazuje z 95% ufnością, która grupa ma wyższy odsetek.
      Prawdziwa różnica wynosi tu 20 punktów procentowych, a mimo to przedział
      leży w całości powyżej zera tylko w około 62% par prób. W pozostałych
      obejmuje zero i nie pozwala rozstrzygnąć, która grupa jest bardziej
      zadowolona. Dwie próby po 60 osób to za mało, żeby tak dużą różnicę
      wykrywać niezawodnie."),

    lc_h2("ch4-case-studies", "Case studies — jak interpretować CI w praktyce"),

    lc_p("Poniższe sytuacje mają ustalone dane, więc ich przedziały się nie
      zmieniają. W każdej budujemy przedział tymi samymi krokami co wyżej,
      a potem sprawdzamy dwie hipotezy. Werdykt zależy od położenia całego
      przedziału względem granicy hipotezy. Przedział w całości po stronie
      hipotezy oznacza TAK, w całości po drugiej stronie NIE, a przedział
      przecinający granicę nie pozwala rozstrzygnąć. Każdy przypadek rozwija
      się po kliknięciu nagłówka."),

    lc_h3("Przedział dla jednej proporcji", num = "A"),

    figure_panel(
      label = "Przykład A1",
      title = "Sondaż wyborczy — czytanie pojedynczego CI",
      p("Pracownia sondażowa zapytała 400 wyborców, czy poprą partię X.
        212 odpowiedziało TAK, czyli ", withMathJax("\\(\\hat{p} = 0.53\\)"),
        ". Budujemy przedział dla poparcia w populacji i sprawdzamy dwie hipotezy."),
      ch4_case_ui("A1")
    ),

    figure_panel(
      label = "Przykład A2",
      title = "Ten sam odsetek, trzy różne wielkości próby",
      p("Trzy badania mierzą odsetek wadliwych produktów w fabryce.
        W każdym ", withMathJax("\\(\\hat{p} = 0.08\\)"), " (8%), ale próby
        mają różną liczebność: 50, 200 i 1000 sztuk. Kolejne kroki dokładają
        przedziały od najmniejszej próby do największej."),
      ch4_case_ui("A2")
    ),

    lc_h3("Przedział dla różnicy proporcji", num = "B"),

    figure_panel(
      label = "Przykład B1",
      title = "Lek a placebo — odsetek wyleczonych",
      p("Badamy nowy lek przeciwbólowy.
        ", tags$b("Lek:"), " 200 pacjentów, 124 zgłosiło ustąpienie bólu (62%).
        ", tags$b("Placebo:"), " 200 pacjentów, 84 zgłosiło ustąpienie bólu (42%)."),
      ch4_case_ui("B1")
    ),

    figure_panel(
      label = "Przykład B2",
      title = "Dwie linie produkcyjne — odsetek braków",
      p("Porównujemy dwie linie produkcyjne pod względem odsetka wadliwych produktów.
        ", tags$b("Linia A:"), " skontrolowano 250 sztuk, 22 wadliwe (8.8%).
        ", tags$b("Linia B:"), " skontrolowano 250 sztuk, 18 wadliwych (7.2%)."),
      ch4_case_ui("B2")
    ),

    figure_panel(
      label = "Przykład B3",
      title = "Pułapka małej próby",
      p("Pilotaż nowej procedury BHP w dwóch zakładach.
        ", tags$b("Zakład A:"), " 30 pracowników, 6 miało wypadek (20%).
        ", tags$b("Zakład B:"), " 30 pracowników, 9 miało wypadek (30%).
        Różnica wygląda na dużą, ale czy z 95% ufnością możemy
        powiedzieć, że w zakładzie A jest bezpieczniej?"),
      ch4_case_ui("B3")
    ),

    lc_h3("Wiele grup — forest plot", num = "C"),

    figure_panel(
      label = "Przykład C1",
      title = "Cztery szpitale — odsetek powikłań pooperacyjnych",
      p("Porównujemy odsetek powikłań po tej samej operacji w czterech szpitalach.
        Dla każdego znamy liczbę wykonanych zabiegów i liczbę powikłań.
        Kolejne kroki pokazują liczby, proporcje i przedziały."),
      ch4_case_ui("C1")
    ),

    lc_p("Przypadki powtarzają kilka lekcji. W A2 ta sama proporcja 8% daje
      przedział o szerokości 15 punktów procentowych przy 50 sztukach i tylko
      3.4 punktu przy 1000 sztukach. Dwudziestokrotnie większa próba zwęża
      przedział około 4.5 raza, bo \\(n\\) stoi we wzorze pod pierwiastkiem.
      W B2 i B3 obserwowana różnica nie wystarcza do wniosku: w B3 dziesięć
      punktów procentowych różnicy przy 30 osobach w grupie daje przedział od
      -32 do +12 punktów, który obejmuje zero i różnice w obu kierunkach.
      W C1 szpital D (20.6% powikłań) odstaje od pozostałych trzech (od 5.6%
      do 10%), bo jego przedział nie nakłada się z żadnym innym. Porównywanie
      nakładania się osobnych przedziałów jest jednak kryterium ostrożnym.
      Jak pokazał rozdział 3, o różnicy dwóch grup rozstrzyga przedział
      dla różnicy."),

    lc_p("We wszystkich przykładach szerokość przedziału zależała od liczebności
      próby, a pośrednio także od samej proporcji i od przyjętego poziomu
      ufności. Tym czynnikom przyjrzymy się w następnym rozdziale."),

    lc_chapter_next(
      num       = "05",
      title     = "Co wpływa na szerokość?",
      lead      = "co decyduje o szerokości przedziału",
      target_id = "ch-czynniki"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  # Ryc. 4.1 i 4.3 (budowa przedziału dla proporcji i różnicy proporcji: losowanie,
  # wykresy i tekst kroku) liczy przeglądarka: ryciny lcSF „s04-pstep” i „s04-pdstep”
  # (sf_steps.js).

  # Przykłady A1–C1 (case studies) liczy i rysuje przeglądarka: rycina lcSF
  # „s4-case” (sf_examples.js); dane i werdykty — ch4_case_ui() na górze pliku.

}
