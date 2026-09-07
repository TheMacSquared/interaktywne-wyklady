# Hub wykładów — jeden launcher dla wszystkich przedmiotów
# Uruchamia wykłady jako osobne procesy i przełącza między nimi w przeglądarce.
#
# Uwaga: to nie jest wykład, tylko infrastruktura zajęciowa, więc świadomie
# nie używa lecture_page() ani DESIGN_CONTRACT.md. Paletę bierze z
# statystyka/R/palette.R, żeby wizualnie należeć do tej samej rodziny.

library(shiny)

# ============================================================================
# BOOTSTRAP
# ============================================================================

.find_hub_dir <- function() {
  has_hub <- function(dir) file.exists(file.path(dir, "R", "discovery.R"))

  candidates <- character(0)
  for (i in seq_len(sys.nframe())) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) candidates <- c(candidates, dirname(normalizePath(ofile)))
  }
  file_arg <- grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    candidates <- c(candidates, dirname(normalizePath(sub("--file=", "", file_arg[[1]]))))
  }
  candidates <- c(candidates, getwd())

  valid <- Filter(has_hub, candidates)
  if (length(valid) > 0) valid[[1]] else candidates[[1]]
}

hub_dir   <- .find_hub_dir()
repo_root <- normalizePath(dirname(hub_dir))

if (!requireNamespace("processx", quietly = TRUE)) {
  stop(
    "Hub potrzebuje pakietu processx do zarządzania procesami wykładów.\n",
    "Zainstaluj raz:  install.packages(\"processx\")",
    call. = FALSE
  )
}

source(file.path(hub_dir, "R", "discovery.R"), local = TRUE)
source(file.path(hub_dir, "R", "processes.R"), local = TRUE)

# Zamknięcie huba ubija wszystkie wykłady — bez tego zostają sieroty na portach.
onStop(function() hub_stop_all())

# ============================================================================
# UI
# ============================================================================

hub_tile <- function(row) {
  chapters_label <- if (is.na(row$chapters)) NULL else {
    span(sprintf("%d %s", row$chapters, hub_plural_chapters(row$chapters)))
  }

  div(
    class = "hub-tile",
    `data-key` = row$key,
    div(
      class = "hub-tile-top",
      span(class = "hub-num", if (nzchar(row$num)) row$num else "—"),
      span(class = "hub-status", span(class = "hub-port"), span(class = "hub-dot"))
    ),
    h3(class = "hub-title", row$title),
    div(
      class = "hub-meta",
      chapters_label,
      if (!is.na(row$module_label)) span(row$module_label)
    ),
    tags$button(type = "button", class = "hub-stop", `data-key` = row$key, "Zatrzymaj")
  )
}

hub_plural_chapters <- function(n) {
  if (n == 1) return("rozdział")
  last_two <- n %% 100
  last_one <- n %% 10
  if (last_one %in% 2:4 && !last_two %in% 12:14) "rozdziały" else "rozdziałów"
}

hub_section <- function(catalog, subject) {
  rows <- catalog[catalog$subject == subject, ]
  div(
    class = "hub-section",
    `data-subject` = subject,
    div(
      class = "hub-section-head",
      h2(rows$subject_label[[1]]),
      span(sprintf("%d %s", nrow(rows), if (nrow(rows) == 1) "wykład" else "wykładów"))
    ),
    div(class = "hub-grid", lapply(seq_len(nrow(rows)), function(i) hub_tile(rows[i, ])))
  )
}

ui <- bootstrapPage(
  title = "Wykłady — spis",
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@400;600;700&display=swap"),
    includeCSS(file.path(hub_dir, "R", "hub_styles.css"))
  ),

  div(
    id = "hub-overlay",
    div(tags$strong(id = "hub-overlay-title", ""), tags$span("Uruchamiam wykład — chwila cierpliwości…"))
  ),

  div(
    class = "hub-wrap",

    div(
      class = "hub-head",
      div(
        h1("Wykłady"),
        p("Kliknij wykład, żeby go otworzyć w osobnej karcie. Raz uruchomiony wykład zostaje żywy — powrót do niego zachowuje stan.")
      ),
      div(
        class = "hub-tools",
        uiOutput("hub_subject_picker", inline = TRUE),
        actionButton("hub_refresh", "Odśwież listę", class = "hub-btn"),
        actionButton("hub_stop_all", "Zatrzymaj wszystkie", class = "hub-btn")
      )
    ),

    div(id = "hub-message"),
    uiOutput("hub_catalog")
  ),

  tags$script(HTML("
(function () {
  var hubWindows = {};

  function tabName(key) { return 'wyklad_' + key.replace(/[^a-zA-Z0-9]/g, '_'); }

  function showOverlay(title) {
    document.getElementById('hub-overlay-title').textContent = title;
    document.getElementById('hub-overlay').classList.add('is-on');
  }
  function hideOverlay() {
    document.getElementById('hub-overlay').classList.remove('is-on');
  }
  function showMessage(html) {
    var box = document.getElementById('hub-message');
    box.innerHTML = html;
    box.classList.add('is-on');
  }
  function clearMessage() {
    document.getElementById('hub-message').classList.remove('is-on');
  }

  // Klik w kafelek: kartę otwieramy natychmiast, w geście użytkownika.
  // Gdyby okno otwierał dopiero serwer, przeglądarka uznałaby je za popup.
  // window.open('', name) na istniejącej karcie tylko ją przełącza — nie
  // przeładowuje jej, więc stan wykładu zostaje nietknięty.
  $(document).on('click', '.hub-tile', function (e) {
    if ($(e.target).closest('.hub-stop').length) return;

    var key = this.dataset.key;
    var name = tabName(key);
    var known = hubWindows[name];
    var hasTab = !!(known && known.win && !known.win.closed && known.loaded);

    var win = null;
    try { win = window.open('', name); } catch (err) { win = null; }
    hubWindows[name] = { win: win, loaded: hasTab };

    clearMessage();
    if (!hasTab) showOverlay(this.querySelector('.hub-title').textContent);

    Shiny.setInputValue('hub_open', {
      key: key, name: name, hasTab: hasTab, nonce: Math.random()
    }, { priority: 'event' });
  });

  $(document).on('click', '.hub-stop', function (e) {
    e.stopPropagation();
    var key = this.dataset.key;
    var rec = hubWindows[tabName(key)];
    if (rec && rec.win && !rec.win.closed) { try { rec.win.close(); } catch (err) {} }
    delete hubWindows[tabName(key)];
    Shiny.setInputValue('hub_stop', { key: key, nonce: Math.random() }, { priority: 'event' });
  });

  Shiny.addCustomMessageHandler('hub_open', function (m) {
    hideOverlay();
    var rec = hubWindows[m.name];
    var win = (rec && rec.win && !rec.win.closed) ? rec.win : null;

    if (m.action === 'error') {
      if (win) { try { win.close(); } catch (err) {} }
      delete hubWindows[m.name];
      showMessage('<strong>Nie udało się uruchomić wykładu.</strong><pre>' + m.message + '</pre>');
      return;
    }

    if (m.action === 'focus') { if (win) win.focus(); return; }

    if (!win) {
      win = window.open(m.url, m.name);
      if (!win) {
        showMessage('Przeglądarka zablokowała nową kartę. Otwórz ręcznie: ' +
          '<a href=\"' + m.url + '\" target=\"_blank\">' + m.url + '</a>');
        return;
      }
    } else {
      win.location.href = m.url;
      win.focus();
    }
    hubWindows[m.name] = { win: win, loaded: true };
  });

  Shiny.addCustomMessageHandler('hub_status', function (m) {
    var ports = (m.ports && !Array.isArray(m.ports)) ? m.ports : {};
    document.querySelectorAll('.hub-tile').forEach(function (tile) {
      var port = ports[tile.dataset.key];
      tile.classList.toggle('is-running', !!port);
      tile.querySelector('.hub-port').textContent = port ? ':' + port : '';
    });
  });

  // Wybór przedmiotu — pokazujemy jedną sekcję albo wszystkie.
  // Filtrowanie jest po stronie klienta, więc przełączenie jest natychmiastowe.
  function applySubject() {
    var picker = document.getElementById('hub_subject');
    if (!picker) return;
    var choice = picker.value;
    document.querySelectorAll('.hub-section').forEach(function (section) {
      section.hidden = choice !== '' && section.dataset.subject !== choice;
    });
  }

  $(document).on('change', '#hub_subject', function () {
    try { localStorage.setItem('hub-subject', this.value); } catch (err) {}
    applySubject();
  });

  // Dropdown jest renderowany z katalogu, więc po każdym renderze (start,
  // odświeżenie listy) przywracamy ostatni wybór — na zajęciach z jednego
  // przedmiotu nie trzeba go wybierać ponownie.
  $(document).on('shiny:value', function (e) {
    if (e.name !== 'hub_subject_picker') return;
    setTimeout(function () {
      var picker = document.getElementById('hub_subject');
      if (!picker) return;
      var saved = null;
      try { saved = localStorage.getItem('hub-subject'); } catch (err) {}
      if (saved !== null && picker.querySelector('option[value=' + JSON.stringify(saved) + ']')) {
        picker.value = saved;
      }
      applySubject();
    }, 0);
  });
})();
  "))
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  catalog <- reactiveVal(hub_discover(repo_root))

  observeEvent(input$hub_refresh, {
    catalog(hub_discover(repo_root))
  })

  output$hub_subject_picker <- renderUI({
    rows <- catalog()
    subjects <- unique(rows[, c("subject", "subject_label")])

    tags$select(
      id = "hub_subject", class = "hub-select",
      tags$option(value = "", sprintf("Wszystkie przedmioty (%d)", nrow(rows))),
      lapply(seq_len(nrow(subjects)), function(i) {
        subject <- subjects$subject[[i]]
        tags$option(
          value = subject,
          sprintf("%s (%d)", subjects$subject_label[[i]], sum(rows$subject == subject))
        )
      })
    )
  })

  output$hub_catalog <- renderUI({
    rows <- catalog()
    if (nrow(rows) == 0) {
      return(div(class = "hub-section", p("Nie znaleziono żadnego wykładu w ", repo_root)))
    }
    lapply(unique(rows$subject), function(subject) hub_section(rows, subject))
  })

  # Adres huba przekazujemy wykładom w LC_HUB_URL — czytamy go z przeglądarki,
  # więc działa niezależnie od tego, na jakim porcie hub został uruchomiony.
  hub_url <- reactive({
    port <- session$clientData$url_port
    sprintf("http://127.0.0.1:%s", if (is.null(port) || port == "") "7700" else port)
  })

  observeEvent(input$hub_open, {
    msg <- input$hub_open
    rows <- catalog()
    row <- rows[rows$key == msg$key, ]
    if (nrow(row) == 0) return()

    was_running <- hub_is_running(msg$key)

    entry <- tryCatch(
      hub_start(msg$key, row$dir[[1]], hub_url()),
      error = function(e) e
    )

    if (inherits(entry, "error")) {
      session$sendCustomMessage("hub_open", list(
        action = "error", name = msg$name, message = conditionMessage(entry)
      ))
      return()
    }

    # Wykład już działał i karta wciąż go pokazuje → tylko przełącz, nie
    # przeładowuj. To jest właśnie zachowanie stanu przy skakaniu tam i z powrotem.
    action <- if (isTRUE(msg$hasTab) && was_running) "focus" else "load"

    session$sendCustomMessage("hub_open", list(
      action = action, name = msg$name, url = hub_url_for(entry)
    ))
  })

  observeEvent(input$hub_stop, {
    hub_stop(input$hub_stop$key)
  })

  observeEvent(input$hub_stop_all, {
    hub_stop_all()
  })

  # Statusy pchamy komunikatem zamiast przerysowywać UI — bez migotania,
  # bez gubienia filtra i pozycji scrolla.
  observe({
    invalidateLater(2500, session)
    keys <- hub_running_keys()
    ports <- lapply(keys, function(k) hub_entry(k)$port)
    names(ports) <- keys
    session$sendCustomMessage("hub_status", list(ports = ports))
  })
}

shinyApp(ui = ui, server = server)
