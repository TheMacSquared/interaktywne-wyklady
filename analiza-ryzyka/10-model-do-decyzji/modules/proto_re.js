// Prototypy punktów stop wykładu 10, grupa re. Scena w przeglądarce, bez serwera R.
// Kontener w R: i10p_scene_ui("P10", …, "uklad", i10p_cfg, …) w stop_proto_re.R.
// Parametry modelu przychodzą z R w data-config (i10p_cfg); w tym pliku nie ma liczb modelu.
//
//   uklad — Piaskownica „Wspólny czas misji” (P10): suwak czasu misji (i10p10_t) i przełącznik
//           modelu wentylatora przeliczają zasilanie, sterownik, oba wentylatory, gałąź A ∥ B i R_sys naraz
//           na wspólnej, stałej skali 0–1.
(function () {
  "use strict";
  var Z = window.lcI10;
  if (!Z) return;
  var U = Z.util;

  // Wymiary rysunku (px): bloki 170 × 84, pasek wskaźnika 150 px = skala 0–1,
  // pasek R_sys 500 px od x = 150 (0) do x = 650 (1).
  var BLOCK_W = 170, BLOCK_H = 84, LINE_Y = 112, FAN_DY = 52;
  var BAR_W = 150, SYS_X0 = 150, SYS_W = 500, SYS_Y = 236;

  // Blok o środku pionowym cy.
  function block(x, cy, name, r, cls) {
    var cx = x + BLOCK_W / 2, top = cy - BLOCK_H / 2;
    return '<rect class="' + cls + '" x="' + x + '" y="' + U.g(top) + '" width="' + BLOCK_W +
      '" height="' + BLOCK_H + '" rx="6"/>' +
      '<text x="' + U.g(cx) + '" y="' + U.g(top + 21) + '" font-size="13" text-anchor="middle">' + U.esc(name) + "</text>" +
      '<text class="i10p-mono" x="' + U.g(cx) + '" y="' + U.g(top + 48) + '" font-size="20" font-weight="700" text-anchor="middle">' + U.fmtP(r) + "</text>" +
      '<rect class="i10p-fill-idle" x="' + U.g(x + 10) + '" y="' + U.g(top + 64) + '" width="' + BAR_W + '" height="8" rx="4"/>' +
      '<rect class="i10p-fill-ok" x="' + U.g(x + 10) + '" y="' + U.g(top + 64) + '" width="' + U.g(BAR_W * r) + '" height="8" rx="4"/>';
  }

  function path(d) { return '<path class="i10p-link" d="' + d + '"/>'; }

  function dot(x) {
    return '<circle class="i10p-ink" cx="' + U.g(x) + '" cy="' + LINE_Y + '" r="6"/>';
  }

  function stage(m, cfg, t) {
    var sysX = function (v) { return SYS_X0 + SYS_W * v; };
    var yA = LINE_Y - FAN_DY, yB = LINE_Y + FAN_DY;
    var body =
      path("M20," + LINE_Y + " H40") + dot(16) +
      block(40, LINE_Y, "Zasilanie P", m.power_r, "i10p-box") +
      path("M210," + LINE_Y + " H240") +
      block(240, LINE_Y, "Sterownik C", m.controller_r, "i10p-box") +
      path("M410," + LINE_Y + " H440 M440," + yA + " V" + yB + " M440," + yA + " H470 M440," + yB + " H470") +
      block(470, yA, "Wentylator A", m.fan_r, "i10p-box") +
      block(470, yB, "Wentylator B", m.fan_r, "i10p-box") +
      path("M640," + yA + " H670 V" + yB + " H640 M670," + LINE_Y + " H700") + dot(704) +
      '<text x="716" y="' + (LINE_Y - 12) + '" font-size="12" class="i10p-subtle">gałąź A ∥ B</text>' +
      '<text x="716" y="' + (LINE_Y + 12) + '" font-size="18" font-weight="700" class="i10p-mono">' + U.fmtP(m.parallel_r) + "</text>" +
      '<text x="40" y="' + (SYS_Y + 12) + '" font-size="14" class="i10p-mono">R_sys</text>' +
      '<rect class="i10p-fill-idle" x="' + SYS_X0 + '" y="' + SYS_Y + '" width="' + SYS_W + '" height="14" rx="4"/>' +
      '<rect class="i10p-fill-ok" x="' + SYS_X0 + '" y="' + SYS_Y + '" width="' + U.g(SYS_W * m.system_r) + '" height="14" rx="4"/>' +
      '<text class="i10p-mono" x="670" y="' + (SYS_Y + 13) + '" font-size="18" font-weight="700">' + U.fmtP(m.system_r) + "</text>" +
      U.axis(sysX, SYS_Y + 34, [0, 0.5, 1], null, ["0", "0.5", "1"]);
    return U.svgWrap(800, 300,
      "Schemat układu chłodzenia przy czasie misji " + t + " " + cfg.unit + ": zasilanie " +
      U.fmtP(m.power_r) + ", sterownik " + U.fmtP(m.controller_r) + ", wentylatory po " + U.fmtP(m.fan_r) +
      ", gałąź A ∥ B " + U.fmtP(m.parallel_r) + ", układ " + U.fmtP(m.system_r) + ".", body);
  }

  function readout(m, cfg, t) {
    var line = '<p class="i10p-read">R_sys = ' + U.fmtP(m.power_r) + " · " + U.fmtP(m.controller_r) + " · " +
      U.fmtP(m.parallel_r) + " = " + U.fmtP(m.system_r) + "</p>";
    var note = t === cfg.base_time
      ? "Przy " + cfg.base_time + " " + cfg.unit + " dane zasilania i sterownika nie wymagają przeliczenia."
      : "Gdyby zasilanie i sterownik zostały przy etykiecie " + cfg.base_time + " " + cfg.unit + ": " +
        U.fmtP(cfg.power_r * cfg.controller_r * m.parallel_r) + " (zawyżone).";
    return line + '<p class="i10p-caption">' + note + "</p>";
  }

  Z.register({
    uklad: function (cfg, ui) {
      var model = "exp";
      return {
        on: {
          model: function (btn) { model = U.press(btn); }
        },
        render: function () {
          var t = U.sliderValue(ui.root, "i10p10_t", cfg.time);
          var m = U.mission(cfg, { time: t, model: model });
          ui.set("stage", stage(m, cfg, t));
          ui.set("read", readout(m, cfg, t));
        }
      };
    }
  });
})();
