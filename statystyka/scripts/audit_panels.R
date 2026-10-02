#!/usr/bin/env Rscript
# Audyt paneli figure_panel() we wszystkich wykładach: każda aplikacja, każdy
# rozdział, szerokość 1440 i 390 px, stan po wejściu do rozdziału (bez interakcji).
# Wynik: CSV z jednym wierszem na panel i szerokość (wylewanie, suwaki bez
# wartości, stare elementy, błędy Shiny).
#
# Użycie (z katalogu głównego repo):
#   Rscript statystyka/scripts/audit_panels.R wynik.csv [katalogi wykładów...]
# Wymaga pakietów chromote i processx oraz przeglądarki Chromium; ścieżkę do
# niej można podać w zmiennej CHROMOTE_CHROME.
library(chromote)
args <- commandArgs(trailingOnly = TRUE)
out_csv <- args[1]
apps <- if (length(args) > 1) args[-1] else c(
  Sys.glob("statystyka/0*"), Sys.glob("statystyka-2/0*"), Sys.glob("analiza-ryzyka/[01]*")
)

audit_js <- "(function(){
  var vis=[...document.querySelectorAll('.lc-chapter')].filter(e=>e.offsetParent!==null);
  var out=[];
  vis.forEach(function(ch){
    ch.querySelectorAll('.lc-figure-panel').forEach(function(p,k){
      if (p.offsetParent===null) return;
      var r=p.getBoundingClientRect();
      var badge=p.querySelector('.lc-figure-panel-badge'), title=p.querySelector('.lc-figure-panel-title');
      var irs=[...p.querySelectorAll('.irs')];
      var plots=[...p.querySelectorAll('.shiny-plot-output')];
      out.push({
        chapter: ch.id, k: k,
        label: ((badge?badge.textContent:'')+' | '+(title?title.textContent:'')).replace(/\\s+/g,' ').trim(),
        width: Math.round(r.width),
        overflow: p.scrollWidth > p.clientWidth + 2,
        slider_no_value: irs.filter(e=>{var v=e.querySelector('.irs-single, .irs-from'); return !e.closest('.lc-slider') && (!v || getComputedStyle(v).display==='none');}).length,
        sliders: irs.length,
        old_columns: p.querySelectorAll('.row > [class*=\"col-\"]').length,
        stat_box: p.querySelectorAll('.lc-stat-box').length,
        old_table: p.querySelectorAll('table.lc-table, table.table, table.shiny-table, .dataTables_wrapper').length,
        fixed_plot: plots.filter(e=>!e.closest('.lc-plot')).length,
        plots: plots.length,
        old_feedback: p.querySelectorAll('.lc-feedback').length,
        old_buttons: p.querySelectorAll('button.btn, .btn[class*=\"lc-btn-\"]').length,
        radios: p.querySelectorAll('.shiny-input-radiogroup:not(.lc-seg-input)').length,
        errors: p.querySelectorAll('.shiny-output-error:not(.shiny-output-error-validation)').length,
        v2: p.querySelectorAll('.lc-toolbar, .lc-tbl, .lc-plot').length
      });
    });
  });
  return JSON.stringify(out);
})()"

wait_for <- function(b, expr, timeout = 30) {
  t0 <- Sys.time()
  repeat {
    ok <- tryCatch(isTRUE(b$Runtime$evaluate(expr)$result$value), error = function(e) FALSE)
    if (ok) return(TRUE)
    if (as.numeric(Sys.time() - t0, units = "secs") > timeout) return(FALSE)
    Sys.sleep(0.4)
  }
}
set_width <- function(b, w) {
  b$Emulation$setDeviceMetricsOverride(width = w, height = 1000, deviceScaleFactor = 1, mobile = FALSE)
}

rows <- list()
port <- 7300
for (app in apps) {
  port <- port + 1
  proc <- processx::process$new("Rscript",
    c("-e", sprintf("shiny::runApp('%s', port=%d, launch.browser=FALSE)", app, port)),
    stdout = "|", stderr = "2>&1")
  t0 <- Sys.time(); ready <- FALSE
  while (as.numeric(Sys.time() - t0, units = "secs") < 90) {
    out <- proc$read_output_lines()
    if (any(grepl("Listening", out))) { ready <- TRUE; break }
    if (!proc$is_alive()) break
    Sys.sleep(0.5)
  }
  if (!ready) { message("APP FAIL ", app); proc$kill(); next }
  b <- ChromoteSession$new(width = 1440, height = 1000)
  b$Page$navigate(sprintf("http://127.0.0.1:%d/", port))
  wait_for(b, "!!(window.Shiny && Shiny.setInputValue && document.querySelector('.lc-chapter'))", 60)
  chapters <- jsonlite::fromJSON(b$Runtime$evaluate(
    "JSON.stringify([...new Set([...document.querySelectorAll('[data-lc-chapter]')].map(e=>e.getAttribute('data-lc-chapter')))])")$result$value)
  for (ch in chapters) {
    b$Runtime$evaluate(sprintf("Shiny.setInputValue('lc__switch_chapter','%s',{priority:'event'});", ch))
    wait_for(b, sprintf("(function(){var e=document.getElementById('%s'); return !!e && e.offsetParent!==null;})()", ch), 30)
    Sys.sleep(2.5)
    wait_for(b, "!document.documentElement.classList.contains('shiny-busy')", 20)
    for (w in c(1440, 390)) {
      set_width(b, w); Sys.sleep(1.2)
      res <- jsonlite::fromJSON(b$Runtime$evaluate(audit_js)$result$value)
      if (length(res) && NROW(res)) {
        res$app <- app; res$viewport <- w
        rows[[length(rows) + 1]] <- res
      }
    }
    set_width(b, 1440)
  }
  b$close(); proc$kill()
  message("DONE ", app, " (", length(chapters), " rozdz.)")
}
all <- do.call(rbind, rows)
if (is.null(all)) all <- data.frame()
write.csv(all, out_csv, row.names = FALSE)
message("Paneli zmierzonych: ", nrow(all) / 2,
        "; wylewa się: ", if (nrow(all)) sum(all$overflow) else 0,
        "; z błędami Shiny: ", if (nrow(all)) sum(all$errors > 0) else 0)
