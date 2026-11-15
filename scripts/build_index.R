#!/usr/bin/env Rscript

# Spis wykładów dla stron statycznych: <out>/index.html (oba kursy) oraz
# <out>/<kurs>/index.html. Lista powstaje z katalogów kursów (app.R: numer
# i tytuł wykładu); odnośnik dostaje każdy wykład, którego strona jest już
# zbudowana przez scripts/build_static.R, pozostałe są oznaczone jako niedostępne.
#
# Użycie (z katalogu repozytorium):
#   Rscript scripts/build_index.R              # dist/
#   Rscript scripts/build_index.R --out=dist

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1]]) else default
}
script_path <- local({
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(file_arg)) stop("Uruchom przez Rscript scripts/build_index.R.", call. = FALSE)
  normalizePath(sub("^--file=", "", file_arg[[1]]))
})
repo_root <- normalizePath(file.path(dirname(script_path), ".."))
out_root <- opt("out", file.path(repo_root, "dist"))
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

# Kursy przechodzące na strony statyczne (statystyka 2 zostaje na Shiny).
# anchor: kotwica na stronie startowej; cls: wariant barwny karty (CSS).
courses <- list(
  statystyka       = list(title = "Statystyka",     anchor = "statystyka", cls = "stat"),
  `analiza-ryzyka` = list(title = "Analiza ryzyka", anchor = "ryzyko",     cls = "risk")
)
repo_url <- "https://github.com/TheMacSquared/interaktywne-wyklady"

read_text <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
app_field <- function(txt, field) {
  hit <- regmatches(txt, regexpr(paste0(field, "\\s*=\\s*\"[^\"]*\""), txt))
  if (length(hit)) sub("^[^\"]*\"([^\"]*)\"$", "\\1", hit) else NA_character_
}
esc <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)

lectures_of <- function(course) {
  dirs <- sort(list.dirs(file.path(repo_root, course), recursive = FALSE, full.names = FALSE))
  dirs <- dirs[grepl("^[0-9]{2}-", dirs) & file.exists(file.path(repo_root, course, dirs, "app.R"))]
  do.call(rbind, lapply(dirs, function(d) {
    txt <- read_text(file.path(repo_root, course, d, "app.R"))
    num <- app_field(txt, "lecture_num")
    data.frame(dir = d, num = if (is.na(num)) substr(d, 1, 2) else num, title = app_field(txt, "lecture_title"),
               built = file.exists(file.path(out_root, course, paste0(d, ".html"))), stringsAsFactors = FALSE)
  }))
}

# Polska odmiana: 1 wykład, 2–4 wykłady, 5+ wykładów (z wyjątkiem 12–14).
n_lectures <- function(n) {
  form <- if (n == 1) "wykład" else if (n %% 10 %in% 2:4 && !(n %% 100 %in% 12:14)) "wykłady" else "wykładów"
  paste(n, form)
}

# ============================================================================
# STYL I SKRYPTY STRONY (strona samodzielna, bez plików obok)
# ============================================================================

css <- '
*{box-sizing:border-box}
html,body{margin:0}
body{background:#f5f0e6}
html[data-theme="dark"] body{background:#161412}

.home{
  --upwr-bg:#f5f0e6;--upwr-panel:#faf6ec;--upwr-surface:#fff;--upwr-surface-sunken:#f3eee3;
  --upwr-ink:#1c1a17;--upwr-ink-soft:#3d3832;--upwr-ink-subtle:#6e665c;
  --upwr-rule:#d9cfbc;--upwr-rule-soft:#ebe4d6;
  --upwr-accent:#6b1a2a;--upwr-accent-hover:#8a2535;--upwr-accent-tint:#f4e8ea;
  --upwr-gold:#8a6a1e;--upwr-gold-tint:#f5ecd8;
  --upwr-sans:\'Atkinson Hyperlegible\',-apple-system,sans-serif;
  --upwr-mono:\'JetBrains Mono\',ui-monospace,monospace;
  container-type:inline-size;
  background:var(--upwr-bg);color:var(--upwr-ink);font-family:var(--upwr-sans);font-size:17px;line-height:1.5;
}
html[data-theme="dark"] .home{
  --upwr-bg:#161412;--upwr-panel:#201d19;--upwr-surface:#26221d;--upwr-surface-sunken:#1b1815;
  --upwr-ink:#f7efe4;--upwr-ink-soft:#ded1c2;--upwr-ink-subtle:#a79b8e;
  --upwr-rule:#4a4238;--upwr-rule-soft:#342f29;
  --upwr-accent:#e3a0ad;--upwr-accent-hover:#efbcc5;--upwr-accent-tint:#3f2028;
  --upwr-gold:#d6b15b;--upwr-gold-tint:#3a301c;
}
.home a{color:inherit}
.h-in{max-width:72rem;margin:0 auto;padding:0 clamp(18px,5cqi,56px)}

/* Nagłówek */
.h-top{display:flex;justify-content:space-between;align-items:center;gap:12px;padding-block:18px;border-bottom:1px solid var(--upwr-rule)}
.h-mark{display:flex;align-items:center;gap:10px;font:700 .72em var(--upwr-mono);letter-spacing:.14em;text-transform:uppercase;color:var(--upwr-ink-soft);text-decoration:none}
.h-mark i{display:block;width:12px;height:12px;background:var(--upwr-accent)}
.h-nav{display:flex;gap:4px;align-items:center}
.h-nav a,.h-theme{display:inline-flex;align-items:center;min-height:40px;padding:0 12px;border-radius:6px;font:700 .82em var(--upwr-sans);color:var(--upwr-ink-soft);text-decoration:none;background:none;border:0;cursor:pointer}
.h-nav a:hover,.h-theme:hover{color:var(--upwr-accent);background:var(--upwr-surface-sunken)}
.h-theme{border:1px solid var(--upwr-rule);gap:6px}

/* Hero */
.h-hero{display:grid;grid-template-columns:minmax(0,1fr);gap:clamp(24px,5cqi,64px);align-items:end;padding-block:clamp(40px,8cqi,96px) clamp(32px,5cqi,56px)}
.h-kick{font:700 .7em var(--upwr-mono);letter-spacing:.14em;text-transform:uppercase;color:var(--upwr-accent);margin:0 0 14px}
.h-title{font-size:clamp(2.3em,7cqi,4.2em);line-height:1;letter-spacing:-.02em;margin:0 0 .35em;font-weight:700;text-wrap:balance}
.h-title span{color:var(--upwr-accent)}
.h-lead{font-size:1.15em;line-height:1.55;color:var(--upwr-ink-soft);max-width:30em;margin:0;text-wrap:pretty}

/* Kursy */
.h-courses{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:clamp(20px,4cqi,48px);padding-bottom:clamp(48px,7cqi,88px)}
.h-courses.one{grid-template-columns:minmax(0,1fr);max-width:44rem}
.h-course{background:var(--upwr-surface);border:1px solid var(--upwr-rule);padding:clamp(20px,3cqi,32px) clamp(18px,3cqi,32px) 12px;position:relative}
.h-course::before{content:"";position:absolute;inset:-1px -1px auto;height:4px;background:var(--c)}
.h-course.stat{--c:var(--upwr-accent);--ct:var(--upwr-accent-tint)}
.h-course.risk{--c:var(--upwr-gold);--ct:var(--upwr-gold-tint)}
.h-c-head{display:flex;justify-content:space-between;align-items:baseline;gap:12px;margin-bottom:4px}
.h-c-tag{font:700 .66em var(--upwr-mono);letter-spacing:.14em;text-transform:uppercase;color:var(--c)}
.h-c-count{font:.72em var(--upwr-mono);color:var(--upwr-ink-subtle)}
.h-c-title{font-size:1.75em;line-height:1.1;margin:0 0 .7em}
.h-c-start{display:inline-flex;align-items:center;gap:8px;min-height:44px;padding:0 16px;border-radius:6px;background:var(--c);color:var(--upwr-surface)!important;font-weight:700;font-size:.9em;text-decoration:none;margin-bottom:18px}
.h-c-start:hover{filter:brightness(1.12)}

/* Spis wykładów — jak lc-index */
.h-list{list-style:none;margin:0;padding:0;border-top:1px solid var(--upwr-rule)}
.h-list a,.h-list .h-off{display:grid;grid-template-columns:2.4em minmax(0,1fr) auto;align-items:center;gap:12px;min-height:48px;padding:6px 8px 6px 0;border-bottom:1px solid var(--upwr-rule-soft);text-decoration:none;color:var(--upwr-ink)}
.h-list li:last-child a,.h-list li:last-child .h-off{border-bottom:0}
.h-list a:hover{background:var(--ct);padding-left:8px;margin-left:-8px}
.h-list a:hover .h-n,.h-list a:hover .h-go{color:var(--c)}
.h-n{font:700 .8em var(--upwr-mono);color:var(--upwr-ink-subtle)}
.h-t{font-size:.98em;line-height:1.3}
.h-go{font:.85em var(--upwr-mono);color:var(--upwr-ink-subtle)}
.h-off .h-t{color:var(--upwr-ink-subtle)}
.h-off small{display:block;font-size:.8em}

/* Jak korzystać */
.h-how{border-top:2px solid var(--upwr-ink);padding-block:22px 48px;display:grid;grid-template-columns:12em repeat(3,minmax(0,1fr));gap:20px 32px}
.h-how-l{font:700 .7em/2 var(--upwr-mono);letter-spacing:.14em;text-transform:uppercase;color:var(--upwr-ink)}
.h-how h3{font-size:1em;margin:0 0 4px}
.h-how p{margin:0;font-size:.9em;color:var(--upwr-ink-soft);line-height:1.5}
.h-how kbd{font:.85em var(--upwr-mono);background:var(--upwr-surface-sunken);border:1px solid var(--upwr-rule);border-radius:4px;padding:0 4px}

.h-foot{border-top:1px solid var(--upwr-rule);padding-block:18px 28px;display:flex;flex-wrap:wrap;justify-content:space-between;gap:8px 24px;font-size:.8em;color:var(--upwr-ink-subtle)}
.h-foot a{color:var(--upwr-ink-soft)}

@container (max-width:820px){
  .h-hero{grid-template-columns:minmax(0,1fr);align-items:start}
  .h-courses{grid-template-columns:minmax(0,1fr)}
  .h-how{grid-template-columns:minmax(0,1fr)}
}
@container (max-width:480px){
  .home{font-size:16px}
  .h-nav a{display:none}
}
'

# Motyw zapisany pod tym samym kluczem co w wykładach (lc-theme), więc wybór
# na stronie startowej i w wykładach jest wspólny.
theme_head_js <- "
(function(){var t;try{t=localStorage.getItem('lc-theme')}catch(e){}
if(t!=='light'&&t!=='dark')t=matchMedia('(prefers-color-scheme: dark)').matches?'dark':'light';
document.documentElement.dataset.theme=t})();
"
theme_btn_js <- "
(function(){var b=document.getElementById('themeBtn'),r=document.documentElement;
function sync(){b.setAttribute('aria-pressed',r.dataset.theme==='dark')}sync();
b.addEventListener('click',function(){r.dataset.theme=r.dataset.theme==='dark'?'light':'dark';try{localStorage.setItem('lc-theme',r.dataset.theme)}catch(e){}sync()});})();
"
fonts_href <- "https://fonts.googleapis.com/css2?family=Atkinson+Hyperlegible:ital,wght@0,400;0,700;1,400&family=JetBrains+Mono:wght@400;500;700&display=swap"
theme_icon <- '<svg width="14" height="14" viewBox="0 0 16 16" aria-hidden="true"><circle cx="8" cy="8" r="6.5" fill="none" stroke="currentColor" stroke-width="1.5"/><path d="M8 1.5a6.5 6.5 0 0 1 0 13z" fill="currentColor"/></svg>'

# ============================================================================
# FRAGMENTY HTML
# ============================================================================

# prefix: ścieżka od strony do katalogu kursu ("statystyka/" na stronie startowej, "" w spisie kursu).
course_card <- function(course, index, prefix) {
  info <- courses[[course]]
  lec <- lectures_of(course)
  items <- vapply(seq_len(nrow(lec)), function(i) {
    num <- esc(lec$num[i]); title <- esc(lec$title[i])
    if (lec$built[i]) {
      sprintf('<li><a href="%s%s.html"><span class="h-n">%s</span><span class="h-t">%s</span><span class="h-go" aria-hidden="true">→</span></a></li>',
              prefix, lec$dir[i], num, title)
    } else {
      sprintf('<li><span class="h-off"><span class="h-n">%s</span><span class="h-t">%s<small>wersja bez serwera w przygotowaniu — wykład działa na razie pod Shiny</small></span><span></span></span></li>',
              num, title)
    }
  }, "")
  first <- which(lec$built)[1]
  start <- if (is.na(first)) "" else sprintf('<a class="h-c-start" href="%s%s.html">Zacznij od wykładu %s <span aria-hidden="true">→</span></a>',
                                             prefix, lec$dir[first], esc(lec$num[first]))
  head_id <- paste0("c", index)
  paste0(
    sprintf('<section class="h-course %s" id="%s" aria-labelledby="%s">', info$cls, info$anchor, head_id),
    sprintf('<div class="h-c-head"><span class="h-c-tag">Kurs %d</span><span class="h-c-count">%s</span></div>', index, n_lectures(nrow(lec))),
    sprintf('<h2 class="h-c-title" id="%s">%s</h2>', head_id, esc(info$title)),
    start, '<ol class="h-list">', paste(items, collapse = ""), "</ol></section>"
  )
}

# root: ścieżka do strony startowej ("" na niej samej, "../" w spisie kursu).
page <- function(title, root, nav, hero_kick, hero_title, cards, one = FALSE) {
  paste0(
    '<!DOCTYPE html>\n<html lang="pl">\n<head>\n<meta charset="utf-8">\n',
    '<meta name="viewport" content="width=device-width,initial-scale=1">\n',
    # Spis nie trafia do wyszukiwarek (publikacja na GitHub Pages bez indeksowania).
    '<meta name="robots" content="noindex, nofollow">\n',
    "<title>", esc(title), "</title>\n",
    '<link href="', fonts_href, '" rel="stylesheet">\n',
    "<script>", theme_head_js, "</script>\n",
    "<style>", css, "</style>\n</head>\n<body>\n",
    '<div class="home" id="home">\n',
    '<header class="h-in h-top">',
    '<a class="h-mark" href="', root, 'index.html"><i aria-hidden="true"></i>Interaktywne wykłady</a>',
    '<nav class="h-nav" aria-label="Kursy">', nav,
    '<button class="h-theme" id="themeBtn" type="button" aria-pressed="false">', theme_icon, "<span>Motyw</span></button>",
    "</nav></header>\n",
    '<section class="h-in h-hero"><div>',
    '<p class="h-kick">', esc(hero_kick), "</p>",
    '<h1 class="h-title">', esc(hero_title), "</h1>",
    '<p class="h-lead">Każdy wykład to jeden plik HTML: działa bez serwera, także z dysku.</p>',
    "</div></section>\n",
    '<main class="h-in h-courses', if (one) " one", '">', cards, "</main>\n",
    '<section class="h-in" aria-labelledby="how"><div class="h-how">',
    '<h2 class="h-how-l" id="how">Jak korzystać</h2>',
    "<div><h3>Online</h3><p>Otwórz wykład z listy.</p></div>",
    "<div><h3>Offline</h3><p>Zapisz plik wykładu (<kbd>Ctrl</kbd>+<kbd>S</kbd>) i otwórz go z dysku.</p></div>",
    "<div><h3>Motyw</h3><p>Jasny lub ciemny, domyślnie według ustawień systemu.</p></div>",
    "</div></section>\n",
    '<footer class="h-in h-foot"><span>Interaktywne wykłady</span>',
    '<a href="', repo_url, '">Kod źródłowy na GitHubie</a></footer>\n',
    "</div>\n<script>", theme_btn_js, "</script>\n</body>\n</html>\n"
  )
}

# ============================================================================
# BUDOWA
# ============================================================================

titles <- vapply(courses, `[[`, "", "title")

# Strona startowa: oba kursy.
nav_home <- paste(vapply(names(courses), function(course) {
  sprintf('<a href="#%s">%s</a>', courses[[course]]$anchor, esc(courses[[course]]$title))
}, ""), collapse = "")
cards <- paste(vapply(seq_along(courses), function(i) {
  course_card(names(courses)[i], i, prefix = paste0(names(courses)[i], "/"))
}, ""), collapse = "\n")
writeLines(page("Interaktywne wykłady", "", nav_home, paste(titles, collapse = " · "), "Interaktywne wykłady", cards),
           file.path(out_root, "index.html"), useBytes = TRUE)

# Spisy kursów: jedna karta, nawigacja do strony startowej i drugiego kursu.
for (i in seq_along(courses)) {
  course <- names(courses)[i]
  dir.create(file.path(out_root, course), showWarnings = FALSE)
  nav <- paste0('<a href="../index.html">Wszystkie kursy</a>',
                paste(vapply(setdiff(names(courses), course), function(other) {
                  sprintf('<a href="../%s/index.html">%s</a>', other, esc(courses[[other]]$title))
                }, ""), collapse = ""))
  writeLines(page(paste(titles[[course]], "— wykłady"), "../", nav, "Interaktywne wykłady", titles[[course]],
                  course_card(course, i, prefix = ""), one = TRUE),
             file.path(out_root, course, "index.html"), useBytes = TRUE)
}
cat("Spis wykładów:", file.path(out_root, "index.html"), "\n")
