#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
strict <- "--strict" %in% args

project_root <- normalizePath(file.path(getwd()), mustWork = TRUE)

class_token <- function(token) {
  paste0("class\\s*=\\s*['\"]([^'\"]*\\s)?", token, "(\\s|['\"])")
}

migrated_lectures <- c(
  "typy-danych",
  "rozklady-prawdopodobienstwa",
  "przedzialy-ufnosci",
  "wnioskowanie-statystyczne",
  "zalozenia-testow",
  "regresja"
)

rules <- data.frame(
  id = c(
    "old-shell-fluid",
    "old-shell-navbar",
    "old-shell-sidebar",
    "old-shell-tabpanel",
    "bslib-library",
    "bslib-theme",
    "old-heading-section",
    "old-heading-chapter",
    "old-layout-widget",
    "old-layout-narrative",
    "old-callout",
    "old-formula-box",
    "old-stat-box",
    "old-bootstrap-button",
    "old-bootstrap-table",
    "old-color-alias",
    "old-ui-hex",
    "old-theme",
    "shared-css-include"
  ),
  pattern = c(
    "\\bfluidPage\\s*\\(",
    "\\bnavbarPage\\s*\\(",
    "\\bsidebarLayout\\s*\\(",
    "\\btabPanel\\s*\\(",
    "\\blibrary\\s*\\(\\s*bslib\\s*\\)|\\brequire\\s*\\(\\s*bslib\\s*\\)",
    "\\bbs_theme\\s*\\(|\\bbslib::page_",
    class_token("section-title"),
    class_token("chapter-title"),
    class_token("widget-block"),
    class_token("narrative"),
    class_token("callout-(info|warning|success|danger)"),
    class_token("formula-box"),
    class_token("stat-box"),
    "(?<!lc-)\\bbtn-(primary|secondary|success|warning|danger|info|default|outline|outline-[a-z]+|sm|lg)\\b|class\\s*=\\s*['\"]btn\\s",
    "class\\s*=\\s*['\"]table(\\s|['\"])",
    "\\bcol_(primary|secondary|success|warning|danger|info|dark|teal)\\b",
    "#(7f8c8d|f8f9fa|e9ecef|2c3e50|bdc3c7|dee2e6|ecf0f1|555|95a5a6|27ae60|e74c3c|3498db|2980b9)\\b",
    "\\btheme_educational\\s*\\(|\\btheme_minimal\\s*\\(",
    "includeCSS\\s*\\([^\\n]*shared_styles\\.css"
  ),
  message = c(
    "Użyj lecture_page(), nie fluidPage().",
    "Użyj lecture_page(), nie navbarPage().",
    "Użyj komponentów lecture_layout, nie sidebarLayout().",
    "Rozdziały mają być listami lecture_chapter(), nie tabPanel().",
    "Nie ładuj bslib w wykładach opartych o lecture_page().",
    "Nie używaj bslib/Bootswatch do shell aplikacji.",
    "Zamień section-title na lc_h2().",
    "Zamień chapter-title na lc_chapter_hero().",
    "Zamień widget-block na figure_panel().",
    "Zamień narrative na lc_p() / lc_grid().",
    "Zamień callout-* na margin_callout(), margin_note() albo nowy komponent lc_*.",
    "Zamień formula-box na lc_formula_box().",
    "Zamień stat-box na lc_stat_grid() + lc_stat_box().",
    "Użyj klas lc-btn-* zamiast Bootstrap btn-*.",
    "Użyj klas lc-table* zamiast Bootstrap table*.",
    "Użyj palety UPWr zamiast dawnych aliasów col_*.",
    "Użyj zmiennych upwr_* albo var(--upwr-*) zamiast dawnych hexów UI.",
    "Użyj theme_upwr() albo globalnych lc_apply_ggplot_defaults().",
    "Wspólny CSS ładuje lecture_page(); nie includuj go lokalnie."
  ),
  stringsAsFactors = FALSE
)

list_r_files <- function(paths) {
  roots <- file.path(project_root, paths)
  roots <- roots[file.exists(roots)]
  unlist(lapply(roots, function(root) {
    list.files(root, pattern = "[.](R|r)$", recursive = TRUE, full.names = TRUE)
  }), use.names = FALSE)
}

files <- list_r_files(migrated_lectures)

find_matches <- function(file, rule) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  hits <- grep(rule$pattern, lines, perl = TRUE)
  if (!length(hits)) return(NULL)

  data.frame(
    file = rep(normalizePath(file, mustWork = FALSE), length(hits)),
    line = hits,
    rule = rule$id,
    message = rule$message,
    text = trimws(lines[hits]),
    stringsAsFactors = FALSE
  )
}

matches <- do.call(rbind, Filter(Negate(is.null), lapply(files, function(file) {
  do.call(rbind, Filter(Negate(is.null), lapply(seq_len(nrow(rules)), function(i) {
    find_matches(file, rules[i, ])
  })))
})))

cat("Design contract scan\n")
cat("Scope:", paste(migrated_lectures, collapse = ", "), "\n")
cat("Files scanned:", length(files), "\n\n")

if (is.null(matches) || !nrow(matches)) {
  cat("No violations found.\n")
  quit(status = 0)
}

summary <- sort(table(matches$rule), decreasing = TRUE)
cat("Violations by rule:\n")
for (rule_id in names(summary)) {
  rule_msg <- rules$message[match(rule_id, rules$id)]
  cat(sprintf("  %-20s %4d  %s\n", rule_id, summary[[rule_id]], rule_msg))
}

cat("\nFirst matches:\n")
max_show <- 80
shown <- head(matches, max_show)
for (i in seq_len(nrow(shown))) {
  rel <- sub(paste0("^", project_root, "/?"), "", shown$file[i])
  cat(sprintf(
    "%s:%d [%s] %s\n    %s\n",
    rel,
    shown$line[i],
    shown$rule[i],
    shown$message[i],
    shown$text[i]
  ))
}

if (nrow(matches) > max_show) {
  cat(sprintf("\n... %d more matches not shown.\n", nrow(matches) - max_show))
}

if (strict) {
  quit(status = 1)
}

cat("\nInformational mode: exiting with status 0. Use --strict to fail on violations.\n")
quit(status = 0)
