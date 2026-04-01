# Dzieli wyrenderowany HTML po markerach <!-- widget: nazwa -->
# Wejście: content/ch1_estymacja.html (wyrenderowany przez quarto render)
# Wynik:   content_html/ch1_estymacja_part1.html, _part2.html, ...

split_html_chapter <- function(html_path, output_dir = "content_html") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  base_name <- tools::file_path_sans_ext(basename(html_path))

  html_content <- paste(readLines(html_path, warn = FALSE), collapse = "\n")

  # Wyciągnij body content (między <body> i </body>, lub <main> i </main>)
  body <- html_content
  if (grepl("<main", body)) {
    body <- sub(".*<main[^>]*>", "", body)
    body <- sub("</main>.*", "", body)
  } else if (grepl("<body", body)) {
    body <- sub(".*<body[^>]*>", "", body)
    body <- sub("</body>.*", "", body)
  }

  # Dziel po markerach
  marker_pattern <- "<!-- widget: [a-zA-Z0-9_]+ -->"
  parts <- strsplit(body, marker_pattern)[[1]]

  # Zapisz każdy fragment
  for (i in seq_along(parts)) {
    part_path <- file.path(output_dir, paste0(base_name, "_part", i, ".html"))
    writeLines(trimws(parts[i]), part_path)
  }

  message(sprintf("Split %s -> %d parts", base_name, length(parts)))
  invisible(length(parts))
}

# Renderuj QMD (shell) i dziel
build_chapter <- function(qmd_path, output_dir = "content_html") {
  html_path <- sub("\\.qmd$", ".html", qmd_path)

  # Renderuj jeśli HTML nie istnieje lub QMD jest nowszy
  if (!file.exists(html_path) || file.mtime(qmd_path) > file.mtime(html_path)) {
    message("Rendering: ", qmd_path)
    system2("quarto", c("render", qmd_path), stdout = TRUE, stderr = TRUE)
  }

  split_html_chapter(html_path, output_dir)
}

# Buduj wszystkie rozdziały
build_all_content <- function(content_dir = "content", output_dir = "content_html") {
  qmd_files <- list.files(content_dir, pattern = "\\.qmd$", full.names = TRUE)
  for (qmd in qmd_files) build_chapter(qmd, output_dir)
}
