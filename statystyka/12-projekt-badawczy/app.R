# Projekt badawczy: myślenie przed modelowaniem

library(shiny)
library(ggplot2)
library(dplyr)
library(broom)
library(tidyr)

.find_app_dir <- function() {
  for (i in seq_len(sys.nframe())) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  }
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("--file=", "", file_arg))))
  }
  getwd()
}
app_dir <- .find_app_dir()
project_root <- dirname(app_dir)
addResourcePath("projekt_badawczy_assets", app_dir)

source(file.path(project_root, "R", "palette.R"),        local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),     local = TRUE)
source(file.path(project_root, "R", "shared.R"),         local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)
lc_apply_ggplot_defaults()

source(file.path(app_dir, "modules", "helpers.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch0_dzien_dziecka.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch1_ciekawosc.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch2_hipotezy.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch3_pomiar.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch4_sprawdzenia.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch5_iteracja.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch6_model_kontrolny.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch7_checklist.R"), local = TRUE)

header_extras <- tagList(
  tags$style(HTML("
    .research-step {
      background: var(--upwr-panel);
      border-left: 4px solid var(--upwr-szalwia);
      padding: 12px 16px;
      margin: 16px 0;
      border-radius: 0 6px 6px 0;
    }
    .research-step .step-number {
      display: inline-flex;
      width: 28px;
      height: 28px;
      border-radius: 50%;
      align-items: center;
      justify-content: center;
      background: var(--upwr-szalwia);
      color: #fff;
      font-weight: 700;
      margin-right: 8px;
    }
    .construct-map {
      display: grid;
      grid-template-columns: repeat(4, minmax(0, 1fr));
      gap: 10px;
      margin: 16px 0;
    }
    .construct-cell {
      background: var(--upwr-surface);
      border: 1px solid var(--upwr-rule);
      border-radius: 8px;
      padding: 12px;
      min-height: 118px;
    }
    .construct-cell h4 {
      margin-top: 0;
      margin-bottom: 8px;
      font-size: calc(14px * var(--lc-font-scale));
      color: var(--upwr-ink);
    }
    .construct-cell p {
      margin: 0;
      font-size: calc(13px * var(--lc-font-scale));
      line-height: 1.45;
      color: var(--upwr-ink-soft);
    }
    .design-option {
      background: var(--upwr-surface);
      border: 1px solid var(--upwr-rule);
      border-radius: 8px;
      padding: 16px;
      margin: 12px 0;
    }
    .question-card {
      background: var(--upwr-surface);
      border: 1px solid var(--upwr-rule);
      border-radius: 8px;
      padding: 16px;
      margin: 12px 0;
    }
    .question-card h4 {
      margin-top: 0;
      margin-bottom: 8px;
      font-size: calc(16px * var(--lc-font-scale));
      color: var(--upwr-ink);
    }
    .research-ladder {
      display: grid;
      grid-template-columns: repeat(3, minmax(0, 1fr));
      gap: 12px;
      margin: 18px 0;
    }
    .research-ladder > div {
      background: var(--upwr-surface);
      border: 1px solid var(--upwr-rule);
      border-radius: 8px;
      padding: 14px;
    }
    .research-ladder strong {
      display: block;
      margin-bottom: 6px;
      color: var(--upwr-ink);
    }
    .two-plot-grid {
      display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 16px;
    }
    .data-legend {
      display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 8px 14px;
      margin-top: 10px;
    }
    .data-legend-item {
      border-left: 3px solid var(--upwr-szalwia);
      background: var(--upwr-surface);
      padding: 8px 10px;
      border-radius: 0 6px 6px 0;
      font-size: calc(13px * var(--lc-font-scale));
      line-height: 1.35;
    }
    .data-legend-item code {
      font-weight: 700;
    }

    /* --- Tablica tropów: narastający widok zbiorczy całej wiązki --- */
    .tropy-board { margin: 16px 0; }
    .tropy-board td, .tropy-board th {
      vertical-align: top;
      color: var(--upwr-ink);
      font-size: calc(13px * var(--lc-font-scale));
      line-height: 1.4;
    }
    .tropy-row-off { opacity: 0.6; }
    .tropy-row-on  { opacity: 1; transition: opacity .25s; }
    .tropy-muted   { color: var(--upwr-ink-subtle); font-style: italic; }
    .tropy-verdict {
      display: inline-block;
      padding: 2px 8px;
      border-radius: 999px;
      font-weight: 700;
      font-size: calc(12px * var(--lc-font-scale));
    }
    .tropy-verdict-on  { background: var(--upwr-sage-tint);   color: var(--upwr-sage); }
    .tropy-verdict-off { background: var(--upwr-accent-tint); color: var(--upwr-accent); }

    /* --- Rozłożone karty tropów: wszystkie hipotezy/wyniki naraz --- */
    .trop-stack { display: grid; gap: 14px; margin: 16px 0; }
    .trop-card {
      background: var(--upwr-surface);
      border: 1px solid var(--upwr-rule);
      border-left: 4px solid var(--upwr-szalwia);
      border-radius: 8px;
      padding: 14px 16px;
    }
    .trop-card h4 {
      margin: 0 0 6px 0;
      font-size: calc(15px * var(--lc-font-scale));
      color: var(--upwr-ink);
    }
    .trop-card p {
      margin: 4px 0;
      font-size: calc(13px * var(--lc-font-scale));
      line-height: 1.45;
      color: var(--upwr-ink-soft);
    }
    .trop-card .trop-alt {
      margin: 6px 0 0 0;
      padding-left: 18px;
    }
    .trop-card .trop-alt li {
      font-size: calc(12.5px * var(--lc-font-scale));
      color: var(--upwr-ink-soft);
      line-height: 1.4;
    }

    @media (max-width: 992px) {
      .construct-map { grid-template-columns: 1fr; }
      .research-ladder { grid-template-columns: 1fr; }
      .two-plot-grid { grid-template-columns: 1fr; }
      .data-legend { grid-template-columns: 1fr; }
    }
    .child-wheel-panel {
      background: var(--upwr-panel);
      border: 1px solid var(--upwr-rule);
      border-radius: 8px;
      padding: 22px;
      margin: 20px 0;
    }
    .child-wheel-stage {
      position: relative;
      width: min(460px, 88vw);
      aspect-ratio: 1;
      margin: 0 auto 18px auto;
    }
    .child-wheel {
      position: relative;
      width: 100%;
      height: 100%;
      border-radius: 50%;
      border: 10px solid var(--upwr-surface);
      box-shadow: 0 18px 40px rgba(28, 26, 23, 0.18), inset 0 0 0 2px rgba(28, 26, 23, 0.16);
      background:
        radial-gradient(circle at center, var(--upwr-surface) 0 13%, transparent 13.5%),
        conic-gradient(
          from -18deg,
          #6b1a2a 0deg 36deg,
          #c08540 36deg 72deg,
          #4a8a6a 72deg 108deg,
          #6a9dc4 108deg 144deg,
          #a07894 144deg 180deg,
          #cbb858 180deg 216deg,
          #3a5a8a 216deg 252deg,
          #b25838 252deg 288deg,
          #4a8a6a 288deg 324deg,
          #c08540 324deg 360deg
        );
      transform: rotate(0deg);
      transition: transform 3.8s cubic-bezier(.12,.72,.18,1);
    }
    .child-wheel::after {
      content: \"\";
      position: absolute;
      inset: 7%;
      border-radius: 50%;
      border: 1px dashed rgba(255, 255, 255, 0.58);
      pointer-events: none;
    }
    .child-wheel-pointer {
      position: absolute;
      z-index: 3;
      top: -4px;
      left: 50%;
      width: 0;
      height: 0;
      transform: translateX(-50%);
      border-left: 18px solid transparent;
      border-right: 18px solid transparent;
      border-top: 34px solid var(--upwr-ink);
      filter: drop-shadow(0 5px 5px rgba(28, 26, 23, 0.25));
    }
    .child-wheel-label {
      position: absolute;
      left: 50%;
      top: 50%;
      width: 82px;
      min-height: 42px;
      margin-left: -41px;
      margin-top: -21px;
      display: flex;
      align-items: center;
      justify-content: center;
      text-align: center;
      color: #fff;
      font-family: var(--upwr-sans);
      font-size: calc(11.5px * var(--lc-font-scale));
      font-weight: 700;
      line-height: 1.12;
      text-shadow: 0 1px 3px rgba(0, 0, 0, 0.42);
      transform-origin: center;
      pointer-events: none;
    }
    .child-wheel-label-0 { transform: rotate(0deg) translateY(-132px) rotate(0deg); }
    .child-wheel-label-1 { transform: rotate(36deg) translateY(-132px) rotate(-36deg); }
    .child-wheel-label-2 { transform: rotate(72deg) translateY(-132px) rotate(-72deg); }
    .child-wheel-label-3 { transform: rotate(108deg) translateY(-132px) rotate(-108deg); }
    .child-wheel-label-4 { transform: rotate(144deg) translateY(-132px) rotate(-144deg); }
    .child-wheel-label-5 { transform: rotate(180deg) translateY(-132px) rotate(-180deg); }
    .child-wheel-label-6 { transform: rotate(216deg) translateY(-132px) rotate(-216deg); }
    .child-wheel-label-7 { transform: rotate(252deg) translateY(-132px) rotate(-252deg); }
    .child-wheel-label-8 { transform: rotate(288deg) translateY(-132px) rotate(-288deg); }
    .child-wheel-label-9 { transform: rotate(324deg) translateY(-132px) rotate(-324deg); }
    .child-wheel-hub {
      position: absolute;
      left: 50%;
      top: 50%;
      width: 82px;
      height: 82px;
      border-radius: 50%;
      transform: translate(-50%, -50%);
      display: flex;
      align-items: center;
      justify-content: center;
      background: var(--upwr-surface);
      border: 2px solid rgba(28, 26, 23, 0.2);
      color: var(--upwr-accent);
      font-family: var(--upwr-sans);
      font-size: calc(30px * var(--lc-font-scale));
      font-weight: 700;
      box-shadow: 0 8px 20px rgba(28, 26, 23, 0.18);
    }
    .child-wheel-controls {
      display: grid;
      grid-template-columns: max-content 1fr;
      gap: 14px;
      align-items: center;
    }
    .child-spin-button {
      background: var(--upwr-accent);
      border-color: var(--upwr-accent);
      color: #fff;
      font-weight: 700;
      border-radius: 6px;
      padding: 10px 16px;
    }
    .child-spin-button:hover,
    .child-spin-button:focus {
      background: #4a0e1e;
      border-color: #4a0e1e;
      color: #fff;
    }
    .child-spin-button[disabled] {
      opacity: 0.72;
      cursor: wait;
    }
    .child-wheel-result {
      min-height: 54px;
      display: flex;
      align-items: center;
      padding: 10px 14px;
      border-left: 4px solid var(--upwr-accent);
      background: var(--upwr-surface);
      border-radius: 0 6px 6px 0;
      color: var(--upwr-ink);
      font-size: calc(15px * var(--lc-font-scale));
      line-height: 1.35;
    }
    .child-wheel-note {
      margin-top: 18px;
    }
    @media (max-width: 700px) {
      .child-wheel-panel { padding: 16px; }
      .child-wheel-stage { width: min(340px, 82vw); }
      .child-wheel { border-width: 7px; }
      .child-wheel-label {
        width: 68px;
        margin-left: -34px;
        min-height: 36px;
        margin-top: -18px;
        font-size: calc(9.5px * var(--lc-font-scale));
      }
      .child-wheel-label-0 { transform: rotate(0deg) translateY(-98px) rotate(0deg); }
      .child-wheel-label-1 { transform: rotate(36deg) translateY(-98px) rotate(-36deg); }
      .child-wheel-label-2 { transform: rotate(72deg) translateY(-98px) rotate(-72deg); }
      .child-wheel-label-3 { transform: rotate(108deg) translateY(-98px) rotate(-108deg); }
      .child-wheel-label-4 { transform: rotate(144deg) translateY(-98px) rotate(-144deg); }
      .child-wheel-label-5 { transform: rotate(180deg) translateY(-98px) rotate(-180deg); }
      .child-wheel-label-6 { transform: rotate(216deg) translateY(-98px) rotate(-216deg); }
      .child-wheel-label-7 { transform: rotate(252deg) translateY(-98px) rotate(-252deg); }
      .child-wheel-label-8 { transform: rotate(288deg) translateY(-98px) rotate(-288deg); }
      .child-wheel-label-9 { transform: rotate(324deg) translateY(-98px) rotate(-324deg); }
      .child-wheel-hub {
        width: 62px;
        height: 62px;
        font-size: calc(24px * var(--lc-font-scale));
      }
      .child-wheel-controls {
        grid-template-columns: 1fr;
      }
      .child-spin-button {
        width: 100%;
      }
    }
  ")),
  tags$script(HTML("
    (function() {
      var spinCount = 0;
      var running = false;
      var sadAudioUrl = 'projekt_badawczy_assets/sad.mp3';

      function playSadTromboneFallback() {
        var AudioContext = window.AudioContext || window.webkitAudioContext;
        if (!AudioContext) return;

        var ctx = new AudioContext();
        var master = ctx.createGain();
        master.gain.setValueAtTime(0.0001, ctx.currentTime);
        master.gain.exponentialRampToValueAtTime(0.18, ctx.currentTime + 0.03);
        master.gain.exponentialRampToValueAtTime(0.0001, ctx.currentTime + 1.65);
        master.connect(ctx.destination);

        var notes = [
          { start: 0.00, from: 392, to: 370, dur: 0.26 },
          { start: 0.30, from: 349, to: 330, dur: 0.28 },
          { start: 0.62, from: 311, to: 294, dur: 0.34 },
          { start: 1.00, from: 262, to: 196, dur: 0.58 }
        ];

        notes.forEach(function(note) {
          var osc = ctx.createOscillator();
          var gain = ctx.createGain();
          var filter = ctx.createBiquadFilter();
          var t0 = ctx.currentTime + note.start;
          var t1 = t0 + note.dur;

          osc.type = 'sawtooth';
          osc.frequency.setValueAtTime(note.from, t0);
          osc.frequency.exponentialRampToValueAtTime(note.to, t1);

          filter.type = 'lowpass';
          filter.frequency.setValueAtTime(950, t0);
          filter.Q.setValueAtTime(4, t0);

          gain.gain.setValueAtTime(0.0001, t0);
          gain.gain.exponentialRampToValueAtTime(0.55, t0 + 0.04);
          gain.gain.exponentialRampToValueAtTime(0.0001, t1);

          osc.connect(filter);
          filter.connect(gain);
          gain.connect(master);
          osc.start(t0);
          osc.stop(t1 + 0.04);
        });

        window.setTimeout(function() {
          if (ctx.state !== 'closed') ctx.close();
        }, 1900);
      }

      function playSadTrombone() {
        var audio = new Audio(sadAudioUrl);
        audio.volume = 0.85;
        audio.currentTime = 0;

        var played = audio.play();
        if (played && typeof played.catch === 'function') {
          played.catch(playSadTromboneFallback);
        }
      }

      document.addEventListener('click', function(event) {
        var button = event.target.closest('#child_spin_button');
        if (!button || running) return;

        var wheel = document.getElementById('child-wheel');
        var result = document.getElementById('child-wheel-result');
        if (!wheel || !result) return;

        running = true;
        spinCount += 1;
        button.disabled = true;
        button.textContent = 'Koło myśli...';
        result.textContent = 'Trwa niezależne, uczciwe i absolutnie niepodejrzane losowanie...';

        var finalAngle = spinCount * 2160 + (spinCount % 2 === 0 ? -10 : 12);
        wheel.style.transform = 'rotate(' + finalAngle + 'deg)';

        window.setTimeout(function() {
          result.innerHTML = '<strong>Wylosowano: kartkówka.</strong> ' +
            'Gratulacje, statystyka właśnie przypomniała sobie o pomiarze wiedzy.';
          playSadTrombone();
          button.disabled = false;
          button.textContent = 'Zakręć jeszcze raz';
          running = false;
        }, 3900);
      });
    })();
  "))
)

# Kolejność: po celu (ch1) od razu tropy (ch2). Model kontrolny (ch6) wchodzi
# przed checklist (ch7), bo checklist domyka cały projekt i musi być ostatni.
# Numery w hero każdego modułu są ustawione zgodnie z TĄ kolejnością (1..7).
.chapters <- list(ch0_ui, ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui,
                  ch6_ui, ch7_ui)

ui <- lecture_page(
  lecture_id    = "projekt-badawczy",
  lecture_num   = "12",
  lecture_title = "Projekt badawczy",
  module_label  = "Moduł XII",
  chapters      = .chapters,
  header_extras = header_extras
)

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)

  ch0_server(input, output, session)
  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch6_server(input, output, session)
  ch7_server(input, output, session)
}

shinyApp(ui = ui, server = server)
