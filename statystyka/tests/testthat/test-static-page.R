testthat::test_that("tryb statyczny: wykład 02 ma wszystkie rozdziały i opisy kadrów w dokumencie", {
  testthat::skip_if_not_installed("callr")
  res <- callr::r(function(path) {
    setwd(path)
    options(lc.static = TRUE)
    app <- new.env(parent = globalenv())
    sys.source("app.R", envir = app)
    t <- htmltools::renderTags(app$ui)
    doc <- xml2::read_html(paste0("<div>", t$html, "</div>"))
    sections <- xml2::xml_find_all(doc, "//section[@data-lc-static-chapter]")
    steppers <- xml2::xml_find_all(doc, "//div[contains(@class, 'lc-stepper') and .//div[@class='lc-sc']]")
    list(
      ids = xml2::xml_attr(sections, "id"),
      chapters = vapply(app$.chapters, function(ch) ch$id, character(1)),
      hidden = !is.na(xml2::xml_attr(sections, "hidden")),
      outputs = length(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-html-output') or contains(@class, 'shiny-plot-output') or contains(@class, 'shiny-text-output')]")),
      stepper_ids = xml2::xml_attr(steppers, "id"),
      tabs = vapply(steppers, function(s) length(xml2::xml_find_all(s, ".//button[@data-lc-go]")), integer(1)),
      texts = vapply(steppers, function(s) length(xml2::xml_find_all(s, ".//span[@data-lc-step-text]")), integer(1)),
      visible = vapply(steppers, function(s) {
        sum(is.na(xml2::xml_attr(xml2::xml_find_all(s, ".//span[@data-lc-step-text]"), "hidden")))
      }, integer(1)),
      empty = sum(!nzchar(trimws(xml2::xml_text(xml2::xml_find_all(doc, "//span[@data-lc-step-text]")))))
    )
  }, args = list(path = file.path(stat_root, "02-zwiazki-zmiennych")), timeout = 60)

  testthat::expect_equal(res$ids, res$chapters)
  testthat::expect_equal(res$hidden, c(FALSE, rep(TRUE, length(res$ids) - 1)))
  testthat::expect_equal(res$outputs, 0L)
  # Trzy sceny kadrowe: opis dla każdego kroku, widoczny tylko pierwszy.
  testthat::expect_setequal(res$stepper_ids, c("ch2_stop", "ch3_pay", "ch4_tenure"))
  testthat::expect_equal(res$texts, res$tabs)
  testthat::expect_true(all(res$visible == 1L))
  testthat::expect_equal(res$empty, 0L)
})

testthat::test_that("wykład 02 nie ma logiki serwera poza powłoką", {
  files <- list.files(file.path(stat_root, "02-zwiazki-zmiennych", "modules"), pattern = "[.]R$", full.names = TRUE)
  text <- paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = "\n")
  testthat::expect_false(grepl("renderUI|renderText|renderPlot|observeEvent|reactiveVal|lc_step_server", text))
})

testthat::test_that("tryb statyczny: wykład 00 ma sceny, układy tabeli i quiz bez wyjść serwera", {
  testthat::skip_if_not_installed("callr")
  res <- callr::r(function(path) {
    setwd(path)
    options(lc.static = TRUE)
    app <- new.env(parent = globalenv())
    sys.source("app.R", envir = app)
    t <- htmltools::renderTags(app$ui)
    doc <- xml2::read_html(paste0("<div>", t$html, "</div>"))
    steppers <- xml2::xml_find_all(doc, "//div[contains(@class, 'lc-stepper') and .//div[@class='lc-sc']]")
    layout <- xml2::xml_find_all(doc, "//div[@data-lc-variants='ch1_layout']")
    quiz <- jsonlite::fromJSON(xml2::xml_text(xml2::xml_find_first(doc, "//script[@data-lc-tile-quiz='ch7']")),
                               simplifyVector = FALSE)
    list(
      outputs = length(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-html-output') or contains(@class, 'shiny-plot-output')]")),
      stepper_ids = xml2::xml_attr(steppers, "id"),
      tabs = vapply(steppers, function(s) length(xml2::xml_find_all(s, ".//button[@data-lc-go]")), integer(1)),
      texts = vapply(steppers, function(s) length(xml2::xml_find_all(s, ".//span[@data-lc-step-text]")), integer(1)),
      layout_boxes = length(layout),
      layout_values = lapply(layout, function(b) xml2::xml_attr(xml2::xml_find_all(b, "./div[@data-lc-variant]"), "data-lc-variant")),
      layout_choices = xml2::xml_attr(xml2::xml_find_all(doc, "//input[@name='ch1_layout']"), "value"),
      layout_rows = vapply(xml2::xml_find_all(doc, "//div[@data-lc-variants='ch1_layout']/div[.//table]"), function(v) {
        length(xml2::xml_find_all(v, ".//tbody/tr"))
      }, integer(1)),
      quiz_n = quiz$n, quiz_pool = length(quiz$questions),
      quiz_ok = all(vapply(quiz$questions, function(q) {
        length(q$options) == 4 && q$correct %in% 1:4 && nzchar(q$question) && nzchar(q$explanation)
      }, logical(1))),
      quiz_summary = vapply(quiz$summary, function(s) s$min, numeric(1)),
      script = grepl("lcTileQuiz", t$head, fixed = TRUE)
    )
  }, args = list(path = file.path(stat_root, "00-dane-i-populacja")), timeout = 60)

  testthat::expect_equal(res$outputs, 0L)
  testthat::expect_setequal(res$stepper_ids, c("ch1_tabela", "ch2_populacja", "ch3_worek", "ch3_latarka", "ch4_herbata"))
  testthat::expect_equal(res$texts, res$tabs)
  # Odczyty, tabela i podpis: każdy ma oba układy z segmentu „Wiersz to”.
  testthat::expect_equal(res$layout_boxes, 3L)
  for (values in res$layout_values) testthat::expect_setequal(values, res$layout_choices)
  testthat::expect_equal(res$layout_rows, c(5L, 15L))
  testthat::expect_equal(c(res$quiz_n, res$quiz_pool), c(10L, 20L))
  testthat::expect_true(res$quiz_ok)
  testthat::expect_equal(res$quiz_summary, c(90, 70, 50, 0))
  testthat::expect_true(res$script)
})

testthat::test_that("wykład 00 nie ma logiki serwera poza powłoką", {
  files <- list.files(file.path(stat_root, "00-dane-i-populacja", "modules"), pattern = "[.]R$", full.names = TRUE)
  text <- paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = "\n")
  testthat::expect_false(grepl("renderUI|renderText|renderPlot|observeEvent|reactiveVal|lc_step_server", text))
})

testthat::test_that("tryb statyczny: wykład 01 ma wszystkie widoki w dokumencie, bez wyjść serwera", {
  testthat::skip_if_not_installed("callr")
  res <- callr::r(function(path) {
    setwd(path)
    options(lc.static = TRUE)
    app <- new.env(parent = globalenv())
    sys.source("app.R", envir = app)
    t <- htmltools::renderTags(app$ui)
    doc <- xml2::read_html(paste0("<div>", t$html, "</div>"))
    boxes <- function(id) xml2::xml_find_all(doc, sprintf("//*[@data-lc-variants='%s']", id))
    values <- function(box) xml2::xml_attr(xml2::xml_find_all(box, "./*[@data-lc-variant]"), "data-lc-variant")
    radio <- function(name) xml2::xml_attr(xml2::xml_find_all(doc, sprintf("//input[@name='%s']", name)), "value")
    options_of <- function(id) xml2::xml_attr(xml2::xml_find_all(doc, sprintf("//select[@id='%s']/option", id)), "value")
    scenario <- jsonlite::fromJSON(xml2::xml_attr(xml2::xml_find_first(doc, "//div[contains(@class, 'lc-scenario')]"), "data-config"),
                                   simplifyVector = FALSE)
    quiz <- jsonlite::fromJSON(xml2::xml_text(xml2::xml_find_first(doc, "//script[@data-lc-tile-quiz='ch7']")),
                               simplifyVector = FALSE)
    six <- jsonlite::fromJSON(xml2::xml_attr(xml2::xml_find_first(doc, "//div[@data-six-obs]"), "data-config"))
    ids <- xml2::xml_attr(xml2::xml_find_all(doc, "//*[@id]"), "id")
    list(
      outputs = xml2::xml_attr(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-html-output') or contains(@class, 'shiny-plot-output') or contains(@class, 'shiny-text-output')]"), "id"),
      selectize = length(xml2::xml_find_all(doc, "//script[@data-for]")),
      duplicate_ids = unique(ids[duplicated(ids)]),
      tracker_boxes = length(boxes("tracked_var")),
      tracker_ok = all(vapply(boxes("tracked_var"), function(b) setequal(values(b), options_of("tracked_var")), logical(1))),
      leaf_values = values(boxes("ch1_leaf")[[1]]),
      leaf_picks = xml2::xml_attr(xml2::xml_find_all(doc, "//*[@data-lc-pick='ch1_leaf']"), "data-value"),
      freq_boxes = length(boxes("ch2_freq_var")),
      freq_ok = all(vapply(boxes("ch2_freq_var"), function(b) setequal(values(b), radio("ch2_freq_var")), logical(1))),
      freq_texts = length(xml2::xml_find_all(doc, "//div[@id='ch2_freq']//span[@data-lc-step-text]")),
      freq_views = length(xml2::xml_find_all(doc, "//div[@id='ch2_freq']//div[@data-lc-step-text]")),
      scenario_values = unique(unlist(lapply(boxes("ch2_scenario"), values))),
      scenario_choices = radio("ch2_scenario"),
      scenario_n = length(scenario$scenarios),
      scenario_orders_ok = all(vapply(scenario$scenarios, function(s) {
        identical(unlist(s$order), LETTERS[seq_along(s$data)][order(unlist(s$data))])
      }, logical(1))),
      scenario_reveal_buttons = length(xml2::xml_find_all(doc, "//button[@data-scn-reveal-button]")),
      scenario_dm = xml2::xml_attr(xml2::xml_find_all(doc, "//div[contains(@class, 'lc-scenario')]//div[@data-lc-dm-input]"), "data-lc-dm-input"),
      six = six,
      six_controls = all(c(six$input, six$low, six$ceo, six$clear) %in% ids),
      full_ok = all(vapply(boxes("ch5_full_var"), function(b) setequal(values(b), radio("ch5_full_var")), logical(1))),
      full_sets = length(boxes("ch5_full_set")),
      quiz_n = quiz$n, quiz_pool = length(quiz$questions), quiz_colors = length(quiz$option_colors),
      quiz_ok = all(vapply(quiz$questions, function(q) length(q$options) == 4 && q$correct %in% 1:4, logical(1))),
      direction_values = values(boxes("ch8_kierunek")[[1]]),
      direction_options = options_of("ch8_kierunek"),
      toggles = xml2::xml_attr(xml2::xml_find_all(doc, "//div[@data-lc-toggle]"), "data-lc-toggle"),
      toggle_buttons = xml2::xml_attr(xml2::xml_find_all(doc, "//button[@data-lc-toggle-button]"), "id"),
      conditional = xml2::xml_attr(xml2::xml_find_all(doc, "//*[@data-display-if]"), "data-display-if"),
      scripts = vapply(c("lcS01Six", "lcS01Scenario", "lcTileQuiz"), function(s) grepl(s, t$head, fixed = TRUE), logical(1))
    )
  }, args = list(path = file.path(stat_root, "01-typy-danych")), timeout = 120)

  # Jedyne elementy z klasą wyjścia Shiny to opisy kroków rycin lcSF: wypełnia je
  # przeglądarka (shared/lc_stat_figures.js), serwer nic do nich nie wysyła.
  testthat::expect_setequal(res$outputs, c("ch3_hist_text", "ch4_sd_text", "ch4_bp_text", "ch5_sk_text"))
  testthat::expect_equal(res$selectize, 0L)
  testthat::expect_length(res$duplicate_ids, 0L)
  # Śledzona zmienna: po jednym zestawie odczytów w rozdziałach 3, 4 i 5.
  testthat::expect_equal(res$tracker_boxes, 3L)
  testthat::expect_true(res$tracker_ok)
  testthat::expect_setequal(res$leaf_values, res$leaf_picks)
  # Ryc. 2.1: cztery opisy i cztery widoki tabeli, każdy dla obu zmiennych.
  testthat::expect_equal(c(res$freq_texts, res$freq_views, res$freq_boxes), c(4L, 4L, 8L))
  testthat::expect_true(res$freq_ok)
  # Ryc. 2.3: trzy scenariusze, w każdym własne karty i przycisk odsłonięcia.
  testthat::expect_setequal(res$scenario_values, res$scenario_choices)
  testthat::expect_equal(res$scenario_n, 3L)
  testthat::expect_true(res$scenario_orders_ok)
  testthat::expect_equal(res$scenario_reveal_buttons, 3L)
  testthat::expect_equal(res$scenario_dm, paste0("ch2_order_", 1:3))
  testthat::expect_equal(res$six$base, c(5, 3, 6, 4, 7))
  testthat::expect_true(res$six_controls)
  testthat::expect_true(res$full_ok)
  testthat::expect_equal(res$full_sets, 4L)
  testthat::expect_equal(c(res$quiz_n, res$quiz_pool, res$quiz_colors), c(15L, 75L, 4L))
  testthat::expect_true(res$quiz_ok)
  # Ćwiczenia: cztery warianty, w każdym tabela z przełącznikiem rozwiązania.
  testthat::expect_setequal(res$direction_values, res$direction_options)
  testthat::expect_setequal(res$toggles, paste0("ch8_ans1_", res$direction_options))
  testthat::expect_setequal(res$toggle_buttons, res$toggles)
  testthat::expect_equal(res$conditional, paste("input.ch1_ex ==", 1:4))
  testthat::expect_true(all(res$scripts))
})

testthat::test_that("wykład 01 nie ma logiki serwera poza powłoką", {
  dir <- file.path(stat_root, "01-typy-danych")
  files <- c(file.path(dir, "app.R"), list.files(file.path(dir, "modules"), pattern = "[.]R$", full.names = TRUE))
  text <- paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = "\n")
  testthat::expect_false(grepl("renderUI|renderText|renderPlot|observeEvent|reactiveVal|lc_step_server|sendCustomMessage|sendInputMessage|updateActionButton", text))
})

# Wykłady gotowe do budowania statycznego (scripts/check_static_ready.R): wspólne warunki.
static_ready_lectures <- c("00-dane-i-populacja", "01-typy-danych", "02-zwiazki-zmiennych",
                           "03-rozklady-prawdopodobienstwa", "04-przedzialy-ufnosci",
                           "05-wnioskowanie-statystyczne", "06-zalozenia-testow", "07-regresja", "09-case-studies",
                           "10-projekt-badawczy")

for (lecture in static_ready_lectures) local({
  lecture <- lecture
  testthat::test_that(paste("tryb statyczny:", lecture, "ma komplet rozdziałów i nie czeka na serwer"), {
    testthat::skip_if_not_installed("callr")
    res <- callr::r(function(path) {
      setwd(path)
      options(lc.static = TRUE)
      app <- new.env(parent = globalenv())
      sys.source("app.R", envir = app)
      t <- htmltools::renderTags(app$ui)
      doc <- xml2::read_html(paste0("<div>", t$html, "</div>"))
      ids <- xml2::xml_attr(xml2::xml_find_all(doc, "//*[@id]"), "id")
      list(
        sections = xml2::xml_attr(xml2::xml_find_all(doc, "//section[@data-lc-static-chapter]"), "id"),
        chapters = vapply(app$.chapters, function(ch) ch$id, character(1)),
        outputs = xml2::xml_attr(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-html-output') or contains(@class, 'shiny-plot-output') or contains(@class, 'shiny-text-output')]"), "id"),
        selectize = length(xml2::xml_find_all(doc, "//script[@data-for]")),
        duplicate_ids = unique(ids[duplicated(ids)])
      )
    }, args = list(path = file.path(stat_root, lecture)), timeout = 180)
    testthat::expect_equal(res$sections, res$chapters)
    # Klasę wyjścia Shiny mogą mieć tylko opisy kroków, które pisze przeglądarka
    # (ryciny lcSF i skrypty wykładu): identyfikator kończy się na _text.
    testthat::expect_true(all(grepl("_text$", res$outputs)), info = paste(res$outputs, collapse = ", "))
    testthat::expect_equal(res$selectize, 0L)
    testthat::expect_length(res$duplicate_ids, 0L)
  })
})
