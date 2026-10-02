# Komponenty widgetów v2 i tabel v2 (sekcja wspólna lecture_layout.R).
course_root <- if (exists("risk_root")) risk_root else stat_root

load_v2_env <- function() {
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(course_root, "R", "palette.R"), env)
  sys.source(file.path(course_root, "R", "lecture_layout.R"), env)
  env$zoom_plot_ui <- function(id, height = "300px") {
    shiny::plotOutput(id, height = height)
  }
  env
}
html <- function(x) htmltools::renderTags(x)$html

testthat::test_that("liczby mają kropkę dziesiętną i wyrównanie", {
  env <- load_v2_env()
  testthat::expect_equal(env$lc_fmt(c(0.07, 0.155, 7, -2.5, -0.0001, NA), 3),
                         c("0.07", "0.155", "7", "-2.5", "0", "–"))
  testthat::expect_equal(env$lc_num(0.155, 3), "0.155")
  testthat::expect_match(env$lc_num(0.07, 3), '0.07<span class="lc-pad" aria-hidden="true">0</span>', fixed = TRUE)
  testthat::expect_match(env$lc_num(7, 1), '7<span class="lc-pad" aria-hidden="true">.0</span>', fixed = TRUE)
  testthat::expect_equal(env$lc_num(58), "58")
  testthat::expect_equal(env$lc_num(51, int_width = 3),
                         '<span class="lc-pad" aria-hidden="true">0</span>51')
  testthat::expect_equal(env$.lc_int_width(c(51, 200, 7)), 3)
  testthat::expect_equal(env$lc_pval(0.0004), "&lt; 0.001")
})

testthat::test_that("lc_table buduje semantyczną tabelę z legendą skrótów i sumami", {
  env <- load_v2_env()
  df <- data.frame(kat = c("A", "B"), n = c(58, 44), f = c(0.29, 0.22))
  cols <- list(
    env$lc_col("kat", "Kategoria", "row"),
    env$lc_col("n", "Liczebność", short = "n", desc = "liczebność"),
    env$lc_col("f", "Częstość", digits = 3, class = "is-new")
  )
  out <- html(env$lc_table(df, cols, foot = list(kat = "Razem", n = 102, f = 1)))
  testthat::expect_match(out, 'class="lc-tbl is-fit"', fixed = TRUE)
  testthat::expect_match(out, '<th scope="row">A</th>', fixed = TRUE)
  testthat::expect_match(out, '<abbr title="Liczebność">n</abbr>', fixed = TRUE)
  testthat::expect_match(out, 'class="lc-tbl-key"', fixed = TRUE)
  testthat::expect_match(out, "<tfoot>", fixed = TRUE)
  testthat::expect_match(out, 'class="n is-new" data-label="Częstość"', fixed = TRUE)

  cards <- html(env$lc_table(df[, 1:2], cols[1:2], narrow = "cards", prose = TRUE,
                             caption = "Tytuł"))
  testthat::expect_match(cards, "is-cards", fixed = TRUE)
  testthat::expect_match(cards, 'role="cell"', fixed = TRUE)
  testthat::expect_match(cards, "lc-tbl-prose", fixed = TRUE)
  testthat::expect_match(cards, "<caption>Tytuł</caption>", fixed = TRUE)
})

testthat::test_that("lc_table_split pokazuje wariant szeroki i wąski z powtórzoną kolumną wierszy", {
  env <- load_v2_env()
  df <- data.frame(kat = c("A", "B"), n = c(1, 2), cn = c(1, 3))
  cols <- list(env$lc_col("kat", "Kat", "row"), env$lc_col("n", "n"),
               env$lc_col("cn", "N skum."))
  out <- html(env$lc_table_split(df, cols, groups = list("n", "cn"),
                                 foot = list(kat = "Razem", n = 3, cn = "")))
  testthat::expect_match(out, "lc-tbl-v-wide", fixed = TRUE)
  testthat::expect_match(out, "lc-tbl-v-narrow", fixed = TRUE)
  testthat::expect_equal(lengths(regmatches(out, gregexpr("<tfoot>", out))), 2L)
  testthat::expect_equal(lengths(regmatches(out, gregexpr('<th scope="row">A</th>', out))), 3L)
})

testthat::test_that("lc_crosstab oznacza podstawę procentów i komórkę docelową", {
  env <- load_v2_env()
  tab <- table(c("a", "a", "b"), c("x", "y", "y"))
  out <- html(env$lc_crosstab(tab, "row", target = c(1, 2), row_name = "R",
                              col_name = "K", input_id = "cell"))
  testthat::expect_match(out, "is-target", fixed = TRUE)
  testthat::expect_match(out, "is-base-val", fixed = TRUE)
  testthat::expect_match(out, 'data-lc-cell-input="cell"', fixed = TRUE)
  testthat::expect_match(out, "% wierszowe:", fixed = TRUE)
})

testthat::test_that("kontrolki v2 generują wejścia Shiny", {
  env <- load_v2_env()
  testthat::expect_false(grepl("lc-v2", html(env$figure_panel("Ryc.")), fixed = TRUE))
  seg <- html(env$lc_segmented("var", "Zmienna", c("A" = "a", "B" = "b"), selected = "b",
                               exclusive_with = "other"))
  testthat::expect_match(seg, 'class="lc-grp shiny-input-radiogroup lc-seg-input"', fixed = TRUE)
  testthat::expect_match(seg, 'value="b" checked', fixed = TRUE)
  testthat::expect_match(seg, 'data-lc-exclusive="other"', fixed = TRUE)
  slider <- html(env$lc_slider("p", "Prawdopodobieństwo", 0.01, 0.5, 0.1, 0.01))
  testthat::expect_match(slider, 'data-lc-slider="p" data-digits="2"', fixed = TRUE)
  testthat::expect_match(slider, '<span>0.01</span>', fixed = TRUE)
  steps <- html(env$lc_step_nav("st", c("A", "B")))
  testthat::expect_match(steps, 'data-lc-steps="2"', fixed = TRUE)
  testthat::expect_match(steps, "Zacznij", fixed = TRUE)
  testthat::expect_error(env$lc_action("x", variant = "ghost"), "aria_label")
  testthat::expect_match(html(env$lc_action_group(a = "+1", b = "+10", label = "Rzuć")),
                         'id="a" type="button" class="action-button"', fixed = TRUE)
  testthat::expect_match(html(env$lc_plot("pl", ratio = "2/1")), "--lc-plot-ratio:2/1;", fixed = TRUE)
  testthat::expect_false(grepl("--lc-plot-ratio", html(env$lc_plot("pl")), fixed = TRUE))
})

testthat::test_that("lc_table stronicuje po stronie przeglądarki i serwera", {
  env <- load_v2_env()
  df <- data.frame(id = as.character(1:25), x = c(1.5, 2, rep(100, 23)))
  cols <- list(env$lc_col("id", "Id", "row"), env$lc_col("x", "x", digits = 1))

  client <- html(env$lc_table(df, cols, page_size = 10))
  testthat::expect_equal(lengths(regmatches(client, gregexpr("is-paged-out", client))), 15L)
  testthat::expect_match(client, "Wiersze 1–10 z 25", fixed = TRUE)
  testthat::expect_match(client, "1 / 3", fixed = TRUE)
  testthat::expect_false(grepl("data-lc-page-input", client, fixed = TRUE))

  server <- html(env$lc_table(df, cols, page_size = 10, page = 3, page_input = "pg"))
  testthat::expect_match(server, 'data-lc-page-input="pg"', fixed = TRUE)
  testthat::expect_match(server, "Wiersze 21–25 z 25", fixed = TRUE)
  testthat::expect_equal(lengths(regmatches(server, gregexpr("<tr", server))), 6L)
  # Szerokość części całkowitej liczona z całego zbioru: 1.5 dopełnione do 100.
  first <- html(env$lc_table(df, cols, page_size = 10, page = 1, page_input = "pg"))
  testthat::expect_match(first, '<span class="lc-pad" aria-hidden="true">00</span>1.5', fixed = TRUE)
  testthat::expect_match(html(env$lc_table(df, cols, page_size = 10, page = 99, page_input = "pg")),
                         "3 / 3", fixed = TRUE)
  testthat::expect_false(grepl("lc-pager", html(env$lc_table(df, cols, page_size = 50)), fixed = TRUE))
})

testthat::test_that("bloki treści v2 i dawne callouty jako notki", {
  env <- load_v2_env()
  testthat::expect_match(html(env$lc_note("Uwaga", "Treść")), 'class="lc-note"', fixed = TRUE)
  testthat::expect_match(html(env$lc_note("Zasada", "Treść", rule = TRUE)), "lc-note lc-note-rule", fixed = TRUE)
  testthat::expect_match(html(env$inline_callout(label = "Zasada", "x", color = "ok", open = FALSE)),
                         "lc-note lc-note-rule", fixed = TRUE)
  testthat::expect_match(html(env$margin_callout(label = "Uwaga", "x", color = "uwaga")), "lc-note-l", fixed = TRUE)
  testthat::expect_false(grepl("<details", html(env$inline_callout(label = "Uwaga", "x")), fixed = TRUE))
  testthat::expect_match(html(env$lc_more("Rozwiązanie", "x")), "<details class=\"lc-more\">", fixed = TRUE)
  testthat::expect_match(html(env$lc_warn("Pułapka", "x")), 'role="note"', fixed = TRUE)
  testthat::expect_equal(lengths(regmatches(html(env$lc_recap("a", "b")), gregexpr("<li>", html(env$lc_recap("a", "b"))))), 2L)
  testthat::expect_match(html(env$lc_h3("Zgodność", num = "3")), '<span class="lc-h3-n">3</span>', fixed = TRUE)
  testthat::expect_match(html(env$lc_tracker("Wzrost", c(Me = "170.65"))), "<dd>170.65</dd>", fixed = TRUE)
  testthat::expect_match(html(env$lc_chips("h", c("H: μ = 170" = "170"), label = "Sprawdź:")),
                         'data-lc-chips="h"', fixed = TRUE)
  testthat::expect_match(html(htmltools::tags$p("jest ", env$b_("x"), ", gdy")), "</strong>, gdy", fixed = TRUE)
})
