# Komponenty widgetów v2 i tabel v2 (sekcja wspólna lecture_layout.R).
course_root <- if (exists("risk_root")) risk_root else stat_root

load_v2_env <- function() {
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(dirname(course_root), "shared", "palette.R"), env)
  sys.source(file.path(dirname(course_root), "shared", "lecture_layout.R"), env)
  env$zoom_plot_ui <- function(id, height = "300px", click = NULL) {
    shiny::plotOutput(id, height = height, click = click)
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

testthat::test_that("lc_variants trzyma wszystkie warianty w dokumencie", {
  env <- load_v2_env()
  out <- html(env$lc_variants("sc", list(a = shiny::tags$p("A"), b = shiny::tags$p("B"))))
  testthat::expect_match(out, 'class="lc-variants" data-lc-variants="sc"', fixed = TRUE)
  testthat::expect_match(out, '<div data-lc-variant="a">', fixed = TRUE)
  testthat::expect_match(out, '<div data-lc-variant="b" hidden>', fixed = TRUE)
  testthat::expect_false(grepl("shiny-html-output", out, fixed = TRUE))
  # selected wskazuje wariant widoczny na starcie.
  out <- html(env$lc_variants("sc", list(a = "A", b = "B"), selected = "b"))
  testthat::expect_match(out, '<div data-lc-variant="a" hidden>', fixed = TRUE)
  testthat::expect_match(out, '<div data-lc-variant="b">', fixed = TRUE)
  # reveal: wszystko ukryte do kliknięcia przycisku.
  out <- html(env$lc_variants("sc", list(a = "A", b = "B"), reveal = "sc_show"))
  testthat::expect_match(out, 'data-lc-variants-reveal="sc_show"', fixed = TRUE)
  testthat::expect_equal(lengths(regmatches(out, gregexpr(" hidden>", out, fixed = TRUE))), 2L)
  testthat::expect_error(env$lc_variants("sc", list("A", "B")))
})

testthat::test_that("lc_choice_feedback i lc_reveal nie tworzą wyjść serwera", {
  env <- load_v2_env()
  out <- html(shiny::tagList(
    env$lc_check_button("q_check", "Sprawdź", "q"),
    env$lc_choice_feedback("q", "q_out", list(a = env$lc_status("Dobrze."), .other = "Nie."),
                           none = "Wybierz.")
  ))
  testthat::expect_match(out, 'id="q_check"', fixed = TRUE)
  testthat::expect_match(out, 'data-lc-vote-check="q"', fixed = TRUE)
  testthat::expect_match(out, '<div id="q_out" class="lc-vote-feedback"></div>', fixed = TRUE)
  json <- sub('(?s).*<script type="application/json" data-lc-vote-data="q">(.*?)</script>.*', "\\1", out, perl = TRUE)
  cfg <- jsonlite::fromJSON(gsub("<\\/", "</", json, fixed = TRUE))
  testthat::expect_equal(cfg$output, "q_out")
  testthat::expect_setequal(names(cfg$feedback), c("a", ".other", ".none"))
  testthat::expect_match(cfg$feedback$a, 'class="lc-status"', fixed = TRUE)
  testthat::expect_equal(cfg$feedback$.none, "Wybierz.")
  testthat::expect_error(env$lc_choice_feedback("q", "q_out", list("bez nazwy")))

  rev <- html(env$lc_reveal("show", id = "sol", shiny::tags$p("Tok rozwiązania")))
  testthat::expect_match(rev, '<div id="sol" class="lc-reveal" data-lc-reveal="show" hidden>', fixed = TRUE)
  testthat::expect_match(rev, "Tok rozwiązania", fixed = TRUE)
  testthat::expect_false(grepl("shiny-html-output", paste(out, rev), fixed = TRUE))
})

testthat::test_that("lc_step_widget(texts = ) trzyma opisy kroków w dokumencie", {
  env <- load_v2_env()
  out <- html(env$lc_step_widget("sw", steps = c("A", "B", "C"), body = "treść",
                                 texts = list("Pierwszy.", shiny::tags$em("Drugi."), "Trzeci.")))
  testthat::expect_match(out, '<span id="sw_text" class="lc-step-texts">', fixed = TRUE)
  testthat::expect_match(out, '<span data-lc-step-text="1">Pierwszy.</span>', fixed = TRUE)
  testthat::expect_match(out, '<span data-lc-step-text="2" hidden>', fixed = TRUE)
  testthat::expect_match(out, '<span data-lc-step-text="3" hidden>Trzeci.</span>', fixed = TRUE)
  testthat::expect_false(grepl("shiny-html-output", out, fixed = TRUE))
  # Liczba opisów musi równać się liczbie kroków.
  testthat::expect_error(env$lc_step_widget("sw", steps = c("A", "B"), body = "x", texts = list("jeden")))
  # Bez texts opis kroku nadal pisze serwer albo skrypt wykładu.
  legacy <- html(env$lc_step_widget("sw", steps = c("A", "B"), body = "x"))
  testthat::expect_match(legacy, 'id="sw_text"', fixed = TRUE)
  testthat::expect_match(legacy, "shiny-html-output", fixed = TRUE)
})

testthat::test_that("lc_tile_quiz przekazuje pytania przeglądarce i nie tworzy wyjść serwera", {
  env <- load_v2_env()
  questions <- list(
    list(scenario = "Sytuacja </script>", question = "Co to?", options = c("A", "B", "C", "D"),
         correct = 2, explanation = "Bo tak."),
    list(question = "Drugie?", options = list("tak", "nie"), correct = 1, explanation = "Bo nie.")
  )
  out <- html(env$lc_tile_quiz("qz", questions, n = 10, summary = c("70" = "Dobrze.", "0" = "Jeszcze raz.")))
  testthat::expect_match(out, 'id="qz_start"', fixed = TRUE)
  testthat::expect_match(out, 'data-lc-quiz-start="qz"', fixed = TRUE)
  for (slot in c("progress", "question", "options", "feedback", "summary")) {
    testthat::expect_match(out, sprintf('<div id="qz_%s" data-lc-quiz-slot="%s"></div>', slot, slot), fixed = TRUE)
  }
  testthat::expect_false(grepl("shiny-html-output", out, fixed = TRUE))
  # Tekst pytania nie może zamknąć znacznika skryptu z danymi.
  testthat::expect_equal(lengths(regmatches(out, gregexpr("</script>", out, fixed = TRUE))), 1L)
  json <- sub('(?s).*<script type="application/json" data-lc-tile-quiz="qz">(.*?)</script>.*', "\\1", out, perl = TRUE)
  cfg <- jsonlite::fromJSON(gsub("<\\/", "</", json, fixed = TRUE), simplifyVector = FALSE)
  testthat::expect_equal(cfg$n, 2L)                     # n nie większe niż pula
  testthat::expect_equal(cfg$questions[[1]]$scenario, "Sytuacja </script>")
  testthat::expect_equal(cfg$questions[[1]]$correct, 2L)
  testthat::expect_equal(unlist(cfg$questions[[2]]$options), c("tak", "nie"))
  testthat::expect_equal(cfg$questions[[2]]$scenario, "")
  testthat::expect_equal(vapply(cfg$summary, function(s) s$min, numeric(1)), c(70, 0))
  testthat::expect_true(all(c("letter", "score") %in% names(cfg$colors)))
  # Klucz poza zakresem odpowiedzi to błąd autora.
  bad <- questions; bad[[2]]$correct <- 3
  testthat::expect_error(env$lc_tile_quiz("qz", bad, summary = c("0" = "x")))
})

testthat::test_that("lc_variants: wybór kliknięciem, wariant inline; przełącznik i widoki kroków", {
  env <- load_v2_env()
  pick <- html(env$lc_variants("leaf", pick = TRUE, list(a = "A", b = "B")))
  testthat::expect_match(pick, 'data-lc-variants="leaf" data-lc-variants-pick="true"', fixed = TRUE)
  testthat::expect_equal(lengths(regmatches(pick, gregexpr(" hidden>", pick, fixed = TRUE))), 2L)
  testthat::expect_equal(env$lc_pick("leaf", "a")[c("data-lc-pick", "data-value")],
                         list(`data-lc-pick` = "leaf", `data-value` = "a"))
  inline <- html(env$lc_variants("v", inline = TRUE, list(a = "A", b = "B")))
  testthat::expect_match(inline, '<span class="lc-variants" data-lc-variants="v">', fixed = TRUE)
  testthat::expect_false(grepl("<div", inline, fixed = TRUE))

  btn <- html(env$lc_toggle_button("t", "Pokaż", "Ukryj"))
  testthat::expect_match(btn, 'data-label-off="Pokaż" data-label-on="Ukryj"', fixed = TRUE)
  testthat::expect_match(btn, 'aria-pressed="false"', fixed = TRUE)
  box <- html(env$lc_toggle_content("t", off = "bez", on = "z"))
  testthat::expect_match(box, 'data-lc-toggle="t"', fixed = TRUE)
  testthat::expect_match(box, '<div data-lc-toggle-state="off">bez</div>', fixed = TRUE)
  testthat::expect_match(box, '<div data-lc-toggle-state="on" hidden>z</div>', fixed = TRUE)

  views <- html(env$lc_step_views(list("jeden", "dwa", "trzy")))
  testthat::expect_match(views, '<div data-lc-step-text="1">jeden</div>', fixed = TRUE)
  testthat::expect_match(views, '<div data-lc-step-text="3" hidden>trzy</div>', fixed = TRUE)
})

testthat::test_that("lc_tile_quiz ze stałymi odpowiedziami: klucz jako wartość, kolory kafelków", {
  env <- load_v2_env()
  options <- list(list(value = "x", label = "Iks", color = "#111111"),
                  list(value = "y", label = "Igrek", color = "#222222"))
  questions <- list(list(question = "Które?", correct = "y", explanation = "Bo igrek."))
  out <- html(env$lc_tile_quiz("qo", questions, summary = c("0" = "ok"), options = options,
                               tiles_class = "quiz-cols-4", labels = list(question = "Pytanie"),
                               colors = list(score_levels = list(start = "#a", ok = "#b", mid = "#c", low = "#d"))))
  json <- sub('(?s).*<script type="application/json" data-lc-tile-quiz="qo">(.*?)</script>.*', "\\1", out, perl = TRUE)
  cfg <- jsonlite::fromJSON(gsub("<\\/", "</", json, fixed = TRUE), simplifyVector = FALSE)
  testthat::expect_equal(unlist(cfg$questions[[1]]$options), c("Iks", "Igrek"))
  testthat::expect_equal(cfg$questions[[1]]$correct, 2L)
  testthat::expect_equal(unlist(cfg$option_colors), c("#111111", "#222222"))
  testthat::expect_equal(cfg$tiles_class, "quiz-cols-4")
  testthat::expect_equal(cfg$labels$question, "Pytanie")
  testthat::expect_equal(cfg$colors$score_levels$mid, "#c")
  testthat::expect_true(nzchar(cfg$colors$letter))        # domyślne kolory zostają
  bad <- questions; bad[[1]]$correct <- "z"
  testthat::expect_error(env$lc_tile_quiz("qo", bad, summary = c("0" = "ok"), options = options))
})

testthat::test_that("lc_static_fill wstawia treść w miejsca wyjść i nadaje przyrostek nagłówkom", {
  env <- load_v2_env()
  content <- shiny::tagList(env$lc_h2("blok1", "Blok 1"),
                            shiny::tags$details(shiny::uiOutput("x_sol1")), shiny::uiOutput("x_sol2"))
  out <- html(env$lc_static_fill(content, heading_suffix = "rol",
    fills = list(x_sol1 = shiny::tags$p("Rozwiązanie 1"), x_sol3 = "nie ma takiego miejsca")))
  testthat::expect_match(out, 'id="blok1-rol"', fixed = TRUE)
  testthat::expect_match(out, 'data-lc-section="blok1-rol"', fixed = TRUE)
  testthat::expect_match(out, "<p>Rozwiązanie 1</p>", fixed = TRUE)
  testthat::expect_false(grepl('id="x_sol1"', out, fixed = TRUE))
  testthat::expect_match(out, 'id="x_sol2"', fixed = TRUE)          # niewypełnione zostaje
  testthat::expect_false(grepl("nie ma takiego miejsca", out, fixed = TRUE))
})

testthat::test_that("lc_tile_quiz: część odpowiedzi w pytaniu i treść po odpowiedzi", {
  env <- load_v2_env()
  options <- lapply(c("a", "b", "c", "d"), function(v) list(value = v, label = toupper(v)))
  questions <- list(list(id = 1, question = "Q1", correct = "c", explanation = "E1", example = TRUE),
                    list(id = 2, question = "Q2", correct = "a", explanation = "E2"))
  out <- html(env$lc_tile_quiz("qx", questions, summary = c("0" = "ok"), options = options,
    option_count = 3, colors = list(letter = NULL),
    extra = function(q) if (isTRUE(q$example)) shiny::tags$div(class = "ex", paste("przykład", q$id))))
  json <- sub('(?s).*<script type="application/json" data-lc-tile-quiz="qx">(.*?)</script>.*', "\\1", out, perl = TRUE)
  cfg <- jsonlite::fromJSON(gsub("<\\/", "</", json, fixed = TRUE), simplifyVector = FALSE)
  testthat::expect_equal(cfg$option_count, 3L)
  testthat::expect_match(cfg$questions[[1]]$extra, '<div class="ex">przykład 1</div>', fixed = TRUE)
  testthat::expect_null(cfg$questions[[2]]$extra)
  testthat::expect_null(cfg$colors$letter)
  testthat::expect_null(cfg$option_colors)
})
