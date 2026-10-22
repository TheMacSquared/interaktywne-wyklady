# Przykłady do wzorów (3.4) i (3.5) w sekcji „Od drzewa do Bayesa”:
#   a3_stop_bayes_panel("ppv", trees) — „P(A | 🚨) w trzech sytuacjach”,
#   a3_stop_bayes_panel("npv", trees) — „P(Aᶜ | 🔕) w trzech sytuacjach”.
# Te same trzy sytuacje co w przykładzie do wzoru (3.3) (a3_total_raw z
# stop_calkowite.R) i ten sam układ: drzewo z podświetlonymi drogami (rysuje je
# block.R) i wzór rozpisany jako ułamek, składniki w kolorach swoich dróg.
# Przełączanie obsługuje modules/calkowite.js (te same klasy lc-pc-*).

a3_bayes_cases <- function(mode) {
  f <- a3_total_fmt_p; par <- a3_total_fmt_par; n <- a3_total_fmt_int
  lapply(a3_total_raw, function(s) {
    tab <- risk_detector_counts(a3_total_population, s$prevalence, s$sensitivity, s$fpr)
    p_a <- s$prevalence; p_ac <- 1 - p_a
    if (mode == "ppv") {
      # Licznik: droga przez awarię; drugi składnik mianownika: fałszywe alarmy.
      num1 <- s$sensitivity; num2 <- p_a; oth1 <- s$fpr; oth2 <- p_ac
      cnt_num <- tab$alarm[1]; cnt_oth <- tab$alarm[2]
    } else {
      # Licznik: cisza bez awarii; drugi składnik mianownika: przeoczone awarie.
      num1 <- 1 - s$fpr; num2 <- p_ac; oth1 <- 1 - s$sensitivity; oth2 <- p_a
      cnt_num <- tab$no_alarm[2]; cnt_oth <- tab$no_alarm[1]
    }
    t_num <- num1 * num2; t_oth <- oth1 * oth2
    post <- t_num / (t_num + t_oth)
    list(
      key = s$key, short = s$short, name = s$name, text = s$text,
      n1 = par(num1), n2 = par(num2), o1 = par(oth1), o2 = par(oth2),
      tNum = f(t_num), tOth = f(t_oth),
      post = if (mode == "ppv") sprintf("%.3f", post) else sprintf("%.4f", post),
      cNum = n(cnt_num), cOth = n(cnt_oth), cAll = n(cnt_num + cnt_oth)
    )
  })
}

a3_bayes_css <- "
.lc-pc-frac { display: inline-flex; flex-direction: column; align-items: center; vertical-align: middle; }
.lc-pc-frac > span { white-space: nowrap; padding: 0 .2em; }
.lc-pc-frac > span:first-child { border-bottom: 2px solid var(--upwr-ink); padding-bottom: .1em; }
.lc-pc-frac > span:last-child { padding-top: .1em; }
.lc-pc-quiet { color: var(--upwr-cat-szalwia); }
.lc-pc-bayes .lc-pc-formula { align-items: center; }
"

a3_stop_bayes_panel <- function(mode, trees) {
  cases <- a3_bayes_cases(mode)
  first <- cases[[1]]
  out <- function(key, cls = NULL) tags$span(class = cls, `data-pc-out` = key, first[[key]])
  ppv <- mode == "ppv"
  # Kolory składników: licznik i drugi składnik mianownika.
  c_num <- if (ppv) "lc-pc-tp" else "lc-pc-quiet"
  c_oth <- if (ppv) "lc-pc-fp" else "lc-pc-tp"
  lhs <- if (ppv) "P(A | 🚨) =" else "P(Aᶜ | 🔕) ="
  sym_num <- if (ppv) "P(🚨 | A)·P(A)" else "P(🔕 | Aᶜ)·P(Aᶜ)"
  sym_oth <- if (ppv) "P(🚨 | Aᶜ)·P(Aᶜ)" else "P(🔕 | A)·P(A)"
  frac <- function(num, den) tags$span(class = "lc-pc-frac", tags$span(num), tags$span(den))
  figure_panel(
    label = "Przykład",
    title = if (ppv) "P(A | 🚨) w trzech sytuacjach" else "P(Aᶜ | 🔕) w trzech sytuacjach",
    full_width = TRUE,
    tags$style(HTML(a3_stop_total_css)),
    tags$style(HTML(a3_bayes_css)),
    tags$div(class = "lc-pc lc-pc-bayes",
      `data-config` = jsonlite::toJSON(list(cases = cases), auto_unbox = TRUE, digits = NA),
      tags$div(class = "lc-pc-top",
        tags$div(class = "lc-seg", role = "group", `aria-label` = "Wybór sytuacji",
          lapply(seq_along(cases), function(i)
            tags$button(type = "button", `data-pc-go` = i - 1L,
              `aria-pressed` = if (i == 1L) "true" else "false",
              paste0(i, " · ", cases[[i]]$short)))
        )
      ),
      tags$div(class = "lc-sp-head",
        tags$span(class = "lc-sp-name", `data-pc-out` = "name", first$name),
        tags$span(class = "lc-sp-step", `data-pc-out` = "step", "Sytuacja 1 z 3")
      ),
      tags$p(class = "lc-pc-text", `data-pc-out` = "text", `aria-live` = "polite", first$text),
      tags$div(class = "lc-pc-body",
        tags$div(class = "lc-pc-trees",
          lapply(seq_along(trees), function(i)
            tags$div(`data-pc-tree` = i - 1L, hidden = if (i > 1L) NA, trees[[i]]))
        ),
        tags$div(class = "lc-pc-formula",
          `aria-label` = if (ppv) "Wzór 3.4 dla wybranej sytuacji" else "Wzór 3.5 dla wybranej sytuacji",
          tags$div(class = "lc-pc-lhs", lhs),
          tags$div(class = "lc-pc-rhs",
            frac(tags$span(class = c_num, sym_num),
              tagList(tags$span(class = c_num, sym_num), " + ", tags$span(class = c_oth, sym_oth)))),
          tags$div(class = "lc-pc-lhs is-big", "="),
          tags$div(class = "lc-pc-rhs is-big",
            frac(tags$span(class = c_num, out("n1"), "·", out("n2")),
              tagList(tags$span(class = c_num, out("n1"), "·", out("n2")), " + ",
                tags$span(class = c_oth, out("o1"), "·", out("o2"))))),
          tags$div(class = "lc-pc-lhs is-big", "="),
          tags$div(class = "lc-pc-rhs is-big",
            frac(out("tNum", c_num), tagList(out("tNum", c_num), " + ", out("tOth", c_oth))),
            " ≈ ", out("post", "lc-pc-res")),
          tags$div(class = "lc-pc-counts",
            out("cNum", c_num), " / (", out("cNum", c_num), " + ", out("cOth", c_oth), ") = ",
            out("cNum", c_num), " / ", tags$b(`data-pc-out` = "cAll", first$cAll), " ",
            tags$small(if (ppv) "alarmów to awarie" else "cichych zmian jest bez awarii"))
        )
      ),
      tags$div(class = "lc-sp-nav",
        tags$button(type = "button", class = "lc-action is-outline", `data-pc` = "prev", lc_icon("prev"), tags$span("Wstecz")),
        tags$button(type = "button", class = "lc-action is-solid", `data-pc` = "next", tags$span("Dalej"), lc_icon("next"))
      )
    ),
    lc_caption(if (ppv)
      "Licznik to droga przez awarię (burgund), mianownik — obie drogi kończące się alarmem (burgund i złoto)."
    else
      "Licznik to cisza bez awarii (zieleń), mianownik — obie drogi kończące się ciszą, także przeoczone awarie (burgund).")
  )
}
