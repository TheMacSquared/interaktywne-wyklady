read_yaml_file <- function(path) {
  if (!file.exists(path)) stop("Nie istnieje plik: ", path, call. = FALSE)
  yaml::read_yaml(path)
}

merge_ontologies <- function(base, local = list()) {
  base_types <- unlist(base$typy_wezlow %||% list(), use.names = FALSE)
  local_types <- unlist(local$typy_wezlow %||% list(), use.names = FALSE)
  duplicate_types <- intersect(base_types, local_types)
  if (length(duplicate_types)) {
    stop(
      "Ontologia lokalna redefiniuje typy bazowe: ",
      paste(duplicate_types, collapse = ", "),
      call. = FALSE
    )
  }

  base_relations <- base$typy_relacji %||% list()
  local_relations <- local$typy_relacji %||% list()
  duplicate_relations <- intersect(names(base_relations), names(local_relations))
  if (length(duplicate_relations)) {
    stop(
      "Ontologia lokalna redefiniuje relacje bazowe: ",
      paste(duplicate_relations, collapse = ", "),
      call. = FALSE
    )
  }

  list(
    wersja = base$wersja %||% 1,
    typy_wezlow = c(base_types, local_types),
    typy_relacji = c(base_relations, local_relations),
    wartosci_stanu = base$wartosci_stanu %||% list()
  )
}

`%||%` <- function(x, fallback) {
  if (is.null(x)) fallback else x
}

parse_node_file <- function(path) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  match <- regexec(
    "(?s)\\A---\\r?\\n(.*?)\\r?\\n---\\r?\\n(.*)\\z",
    text,
    perl = TRUE
  )
  parts <- regmatches(text, match)[[1]]
  if (!length(parts)) stop("brak poprawnego frontmatter YAML", call. = FALSE)

  metadata <- yaml::yaml.load(parts[[2]])
  if (!is.list(metadata) || is.null(names(metadata))) {
    stop("frontmatter musi być mapą YAML", call. = FALSE)
  }

  body <- parts[[3]]
  heading_match <- regexec("(?m)^#\\s+(.+)$", body, perl = TRUE)
  heading_parts <- regmatches(body, heading_match)[[1]]
  if (!length(heading_parts)) stop("brak nagłówka H1", call. = FALSE)

  list(metadata = metadata, title = heading_parts[[2]], body = body)
}

check_cycle <- function(node_ids, edges, relation_type) {
  selected <- Filter(function(edge) identical(edge$typ, relation_type), edges)
  if (!length(selected)) return(FALSE)

  indegree <- stats::setNames(integer(length(node_ids)), node_ids)
  adjacency <- stats::setNames(vector("list", length(node_ids)), node_ids)
  for (edge in selected) {
    adjacency[[edge$source]] <- c(adjacency[[edge$source]], edge$target)
    indegree[[edge$target]] <- indegree[[edge$target]] + 1L
  }

  queue <- names(indegree)[indegree == 0L]
  processed <- 0L
  while (length(queue)) {
    current <- queue[[1]]
    queue <- queue[-1]
    processed <- processed + 1L
    for (target in adjacency[[current]]) {
      indegree[[target]] <- indegree[[target]] - 1L
      if (indegree[[target]] == 0L) queue <- c(queue, target)
    }
  }

  processed != length(node_ids)
}

build_graph_report <- function(nodes, edges) {
  targets_for <- function(type) {
    unlist(lapply(
      Filter(function(edge) identical(edge$typ, type), edges),
      function(edge) edge$target
    ), use.names = FALSE)
  }

  node_types <- vapply(nodes, function(node) node$typ, character(1))
  content_ids <- names(nodes)[!node_types %in% c("wyklad", "zrodlo", "projekt")]
  question_ids <- names(nodes)[node_types == "pytanie"]
  claim_ids <- names(nodes)[node_types == "twierdzenie"]
  discussed <- unique(targets_for("omawia"))
  answered <- unique(targets_for("odpowiada_na"))
  supported <- unique(targets_for("wspiera"))

  connected <- unique(unlist(lapply(edges, function(edge) {
    c(edge$source, edge$target)
  }), use.names = FALSE))

  list(
    nieomawiane = sort(setdiff(content_ids, discussed)),
    pytania_bez_odpowiedzi = sort(setdiff(question_ids, answered)),
    twierdzenia_bez_zrodel = sort(setdiff(claim_ids, supported)),
    wezly_odlaczone = sort(setdiff(names(nodes), connected))
  )
}

validate_state <- function(node_id, state, allowed_state, errors) {
  if (is.null(state)) return(errors)
  if (!is.list(state) || is.null(names(state))) {
    return(c(errors, paste0(node_id, ": pole 'stan' musi być mapą")))
  }
  for (field in names(state)) {
    if (is.null(allowed_state[[field]])) {
      errors <- c(errors, paste0(node_id, ": nieznane pole stanu '", field, "'"))
    } else if (!state[[field]] %in% unlist(allowed_state[[field]], use.names = FALSE)) {
      errors <- c(
        errors,
        paste0(node_id, ": niedozwolona wartość stanu '", state[[field]], "' dla '", field, "'")
      )
    }
  }
  errors
}

compile_graph <- function(root, entry, write_output = TRUE) {
  graph_path <- normalizePath(file.path(root, entry$sciezka), mustWork = TRUE)
  descriptor <- read_yaml_file(file.path(graph_path, "graf.yaml"))
  base_ontology <- read_yaml_file(file.path(root, "ontologia-bazowa.yaml"))
  local_path <- file.path(graph_path, descriptor$ontologia %||% "ontologia.yaml")
  local_ontology <- if (file.exists(local_path)) read_yaml_file(local_path) else list()
  ontology <- merge_ontologies(base_ontology, local_ontology)

  errors <- character()
  if (!identical(descriptor$id, entry$id)) {
    errors <- c(errors, "ID grafu w indeksie i graf.yaml są różne")
  }

  nodes_path <- file.path(graph_path, descriptor$katalog_wezlow %||% "wezly")
  files <- sort(list.files(nodes_path, pattern = "[.]md$", full.names = TRUE))
  nodes <- list()
  pending_relations <- list()

  for (file in files) {
    parsed <- tryCatch(
      parse_node_file(file),
      error = function(error) {
        errors <<- c(errors, paste0(basename(file), ": ", conditionMessage(error)))
        NULL
      }
    )
    if (is.null(parsed)) next

    metadata <- parsed$metadata
    node_id <- metadata$id
    if (!is.character(node_id) || length(node_id) != 1L || !nzchar(node_id)) {
      errors <- c(errors, paste0(basename(file), ": brak poprawnego id"))
      next
    }
    if (!grepl("^[a-z0-9]+(?:-[a-z0-9]+)*$", node_id)) {
      errors <- c(errors, paste0(node_id, ": id musi używać kebab-case ASCII"))
    }
    if (!identical(tools::file_path_sans_ext(basename(file)), node_id)) {
      errors <- c(errors, paste0(node_id, ": id nie odpowiada nazwie pliku ", basename(file)))
    }
    if (!is.null(nodes[[node_id]])) {
      errors <- c(errors, paste0(node_id, ": zduplikowane id"))
      next
    }
    node_type <- metadata$typ
    if (!is.character(node_type) || length(node_type) != 1L || !nzchar(node_type)) {
      errors <- c(errors, paste0(node_id, ": brak poprawnego typu"))
      node_type <- NA_character_
    } else if (!node_type %in% ontology$typy_wezlow) {
      errors <- c(errors, paste0(node_id, ": nieznany typ '", node_type, "'"))
    }
    errors <- validate_state(node_id, metadata$stan, ontology$wartosci_stanu, errors)

    tags <- metadata$tagi %||% list()
    if (!is.list(tags) && !is.character(tags)) {
      errors <- c(errors, paste0(node_id, ": pole 'tagi' musi być listą"))
      tags <- list()
    }

    nodes[[node_id]] <- list(
      id = node_id,
      typ = node_type,
      label = parsed$title,
      stan = metadata$stan %||% list(),
      tagi = tags,
      plik = basename(file),
      tresc = parsed$body
    )
    pending_relations[[node_id]] <- metadata$relacje %||% list()
  }

  edges <- list()
  for (source_id in names(pending_relations)) {
    relations <- pending_relations[[source_id]]
    if (!is.list(relations) || (length(relations) && !is.null(names(relations)))) {
      errors <- c(errors, paste0(source_id, ": pole 'relacje' musi być listą"))
      next
    }
    for (relation in relations) {
      if (!is.list(relation)) {
        errors <- c(errors, paste0(source_id, ": relacja musi być mapą"))
        next
      }
      relation_type <- relation$typ
      target_id <- relation$cel
      if (!is.character(relation_type) || length(relation_type) != 1L || !nzchar(relation_type) ||
          !is.character(target_id) || length(target_id) != 1L || !nzchar(target_id)) {
        errors <- c(errors, paste0(source_id, ": relacja wymaga tekstowych pól 'typ' i 'cel'"))
        next
      }
      spec <- ontology$typy_relacji[[relation_type]]
      if (is.null(spec)) {
        errors <- c(errors, paste0(source_id, ": nieznana relacja '", relation_type, "'"))
        next
      }
      if (is.null(nodes[[target_id]])) {
        errors <- c(errors, paste0(source_id, " -", relation_type, "-> ", target_id, ": cel nie istnieje"))
        next
      }
      source_type <- nodes[[source_id]]$typ
      target_type <- nodes[[target_id]]$typ
      if (!source_type %in% unlist(spec$zrodlo, use.names = FALSE) ||
          !target_type %in% unlist(spec$cel, use.names = FALSE)) {
        errors <- c(
          errors,
          paste0(source_id, " -", relation_type, "-> ", target_id,
                 ": niedozwolona dziedzina ", source_type, " -> ", target_type)
        )
      }
      edges[[length(edges) + 1L]] <- list(
        source = source_id,
        target = target_id,
        typ = relation_type,
        nota = relation$nota %||% NULL
      )
    }
  }

  acyclic_relations <- names(Filter(
    function(spec) isTRUE(spec$acykliczna),
    ontology$typy_relacji
  ))
  for (relation_type in acyclic_relations) {
    if (check_cycle(names(nodes), edges, relation_type)) {
      errors <- c(errors, paste0("cykl w relacji '", relation_type, "'"))
    }
  }

  result <- list(
    meta = list(
      id = descriptor$id,
      tytul = descriptor$tytul,
      wersja_ontologii = ontology$wersja,
      wezlow = length(nodes),
      krawedzi = length(edges)
    ),
    nodes = unname(nodes),
    edges = edges,
    raport = build_graph_report(nodes, edges)
  )

  if (length(errors)) {
    return(list(ok = FALSE, errors = unique(errors), graph = result, output = NULL))
  }

  output <- file.path(graph_path, "graf.json")
  if (isTRUE(write_output)) {
    temporary <- paste0(output, ".tmp")
    jsonlite::write_json(result, temporary, pretty = TRUE, auto_unbox = TRUE, null = "null")
    if (!file.rename(temporary, output)) {
      unlink(temporary)
      stop("Nie udało się atomowo zapisać ", output, call. = FALSE)
    }
  }
  list(ok = TRUE, errors = character(), graph = result, output = output)
}

compile_index <- function(root, graph_id = NULL, write_output = TRUE) {
  index <- read_yaml_file(file.path(root, "indeks.yaml"))
  entries <- index$grafy %||% list()
  if (!is.null(graph_id)) {
    entries <- Filter(function(entry) identical(entry$id, graph_id), entries)
    if (!length(entries)) stop("Nie ma grafu o id: ", graph_id, call. = FALSE)
  }
  lapply(entries, function(entry) compile_graph(root, entry, write_output = write_output))
}
