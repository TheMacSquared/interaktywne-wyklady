// Prototypy widgetów wykładu 06, rozdział 7 „Komunikat progowy” (P20A–P20C).
// Kontenery w R: z6p_scene_ui() w stop_proto_rk.R; konfiguracja i teksty z R.
//
//   redaktor — piaskownica „Redaktor komunikatu” (P20A): suwaki μ, σ, c, n, kształt
//              i działanie; trzy zdania komunikatu i ostrzeżenia składane na bieżąco.
//   raporty  — galeria „Popraw raport” (P20B): pięć raportów, w każdym jedno błędne
//              zdanie; kliknięcie zdania daje diagnozę i poprawkę.
//   budzet   — „Decyzja z budżetem” (P20C): pakiet działań z kosztami, cel ≤ 10 na
//              1000 pomiarów, komunikat z wybranego pakietu.
// Model temperatury: normalny N(μ, σ) albo skośny 82-style (przesunięta gamma o kształcie 3,
// ta sama średnia i σ co normalny, jak w rozdziale 6).
(function () {
  "use strict";
  var Z = window.lcZ6, D = window.lcDist;
  if (!Z || !D) return;
  var U = Z.util, SQ3 = Math.sqrt(3);

  // P(T > c) dla modelu: normalny albo skośny (gamma k = 3, ta sama średnia i σ).
  function tail(c, mu, sd, shape) {
    if (shape === "skew") {
      var scale = sd / SQ3, shift = mu - 3 * scale;
      return D.pgamma(c - shift, 3, scale, false);
    }
    return 1 - U.pnorm(c, mu, sd);
  }
  function per1000(p) {
    var v = 1000 * p;
    return v >= 100 ? U.fmt(v, 0) : v >= 10 ? U.fmt(v, 0) : v >= 1 ? U.fmt(v, 1) : U.fmt(v, 2);
  }
  function pct(p) { return U.fmt(100 * p, p >= 0.1 ? 0 : 1) + "%"; }
  function shiftP(p, n) { return 1 - Math.pow(1 - p, n); }
  function val(root, id) { var el = root.querySelector("#" + id); return el ? Number(el.value) : NaN; }
  function syncOutputs(root) {
    root.querySelectorAll("input[type=range]").forEach(function (inp) {
      var out = root.querySelector('output[data-value="' + inp.id + '"]');
      if (out) out.textContent = String(Number(Number(inp.value).toFixed(2)));
    });
  }
  function pressed(root, act, fallback) {
    var b = root.querySelector('[data-z6p="' + act + '"][aria-pressed="true"]');
    return b ? b.getAttribute("data-value") : fallback;
  }
  function li(cls, label, text) {
    return '<li class="z6p20-row ' + cls + '"><span class="z6p20-k">' + label + '</span><span>' + text + "</span></li>";
  }

  // Mała krzywa: stan po działaniu na tle stanu wyjściowego, próg, pole ogona.
  function miniCurve(base, now, shape) {
    var X0 = 20, X1 = 540, T0 = 68, T1 = 100, H = 150, B = 160;
    var xOf = function (t) { return X0 + (t - T0) / (T1 - T0) * (X1 - X0); };
    function dens(t, s) {
      if (shape === "skew") {
        var scale = s.sd / SQ3, shift = s.mu - 3 * scale, x = t - shift;
        return x <= 0 ? 0 : x * x * Math.exp(-x / scale) / (2 * scale * scale * scale);
      }
      return U.dnorm(t, s.mu, s.sd);
    }
    var top = 0.24;
    function path(s, from, to, close) {
      var pts = [], t, step = (to - from) / 160;
      for (t = from; t <= to + 1e-9; t += step) pts.push(U.g(xOf(t)) + "," + U.g(B - Math.min(dens(t, s), top) / top * H));
      var d = "M" + pts.join(" L");
      return close ? d + " L" + U.g(xOf(to)) + "," + B + " L" + U.g(xOf(from)) + "," + B + " Z" : d;
    }
    var body = '<path class="z6p-area" d="' + path(now, Math.max(now.c, T0), T1, true) + '"/>' +
      '<path class="z6p-curve-ghost" d="' + path(base, T0, T1) + '"/>' +
      '<path class="z6p-curve" d="' + path(now, T0, T1) + '"/>' +
      (now.c !== base.c ? '<line class="z6p-mean" x1="' + U.g(xOf(base.c)) + '" x2="' + U.g(xOf(base.c)) + '" y1="20" y2="' + B + '"/>' : "") +
      '<line class="z6p-limit" x1="' + U.g(xOf(now.c)) + '" x2="' + U.g(xOf(now.c)) + '" y1="12" y2="' + B + '"/>' +
      '<text class="z6p-mono" x="' + U.g(xOf(now.c) + 5) + '" y="22" font-size="12">próg ' + U.fmt(now.c, 1) + "°C</text>" +
      U.axis(xOf, B, [70, 75, 80, 85, 90, 95, 100], null, ["70", "75", "80", "85", "90", "95", "100°C"]);
    return U.svgWrap(560, 190, "Rozkład temperatury po działaniu na tle stanu wyjściowego, z progiem i polem przekroczeń.", body);
  }

  Z.register({
    // ---- P20A: Redaktor komunikatu ----------------------------------------------------
    redaktor: function (cfg, ui) {
      var root = ui.root;
      function render() {
        syncOutputs(root);
        var mu = val(root, "z6p20a_mu"), sd = val(root, "z6p20a_sd"), c = val(root, "z6p20a_c"),
          n = val(root, "z6p20a_n"), shape = pressed(root, "shape", "normal"), act = pressed(root, "act", "none");
        var base = { mu: mu, sd: sd, c: c }, now = { mu: mu, sd: sd, c: c };
        if (act === "cool") now.mu = mu - 2;
        if (act === "stab") now.sd = Math.max(0.5, sd - 1);
        if (act === "thr") now.c = c + 2;
        var p0 = tail(c, mu, sd, shape), p1 = tail(now.c, now.mu, now.sd, shape), z = (c - mu) / sd;
        var s0 = shiftP(p0, n);

        // Zdanie 1: liczba i horyzont.
        var s1 = "Około " + per1000(p0) + " na 1000 pomiarów przekracza " + U.fmt(c, 1) + "°C. Przy " + n +
          (n === 1 ? " pomiarze" : " pomiarach") + " na zmianę alarm pojawia się w około " + pct(s0) +
          " zmian, jeśli pomiary są niezależne.";
        // Zdanie 2: mechanizm.
        var s2 = z <= 0
          ? "Średnia " + U.fmt(mu, 1) + "°C leży na progu albo nad nim: przekroczenia są normą, nie wyjątkiem."
          : z < 1.5
            ? "Średnia leży tylko " + U.fmt(z, 1) + "σ pod progiem: ryzyko tworzą położenie i rozrzut temperatury."
            : z < 2.5
              ? "Próg leży " + U.fmt(z, 1) + "σ nad średnią: przekroczenia biorą się z rozrzutu, czyli z rzadszych gorących pomiarów."
              : "Próg leży " + U.fmt(z, 1) + "σ nad średnią: przekroczenia są rzadkie, a ich liczbę wyznacza kształt ogona.";
        // Zdanie 3: działanie.
        var s3 = {
          none: "Nie wybrano działania. Wybierz chłodzenie, stabilizację albo rewizję progu i porównaj skutek.",
          cool: "Chłodzenie o 2°C zmienia częstość z " + per1000(p0) + " do " + per1000(p1) + " na 1000 pomiarów.",
          stab: "Ograniczenie σ do " + U.fmt(now.sd, 1) + "°C zmienia częstość z " + per1000(p0) + " do " + per1000(p1) + " na 1000 pomiarów.",
          thr: "Podniesienie progu do " + U.fmt(now.c, 1) + "°C zmienia liczbę w raporcie z " + per1000(p0) + " do " + per1000(p1) +
            " na 1000, ale temperatura łożyska zostaje ta sama."
        }[act];

        var warn = [];
        if (act === "stab" && z < 0) warn.push("Próg leży pod średnią: zwężenie rozkładu podnosi ryzyko zamiast je obniżać.");
        if (act === "stab" && Math.abs(z) < 1e-9) warn.push("Próg równy średniej: stabilizacja nie zmienia ryzyka, zostaje 50%.");
        if (act === "thr") warn.push("Rewizja progu wymaga dowodu konstrukcyjnego (badania materiałowego), a nie liczby alarmów.");
        if (shape === "skew" && z >= 2) {
          var pn = tail(c, mu, sd, "normal");
          warn.push("Rozkład skośny: model normalny dałby tu " + per1000(pn) + " na 1000, czyli zaniżyłby ryzyko " +
            U.fmt(p0 / pn, 1) + " razy.");
        }
        if (n > 1) warn.push("Odsetek zmian z alarmem to górne oszacowanie: przy skorelowanych pomiarach będzie niższy.");

        ui.set("stage", miniCurve(base, now, shape));
        ui.set("body",
          '<ol class="z6p20-msg">' + li("", "Liczba i horyzont", U.esc(s1)) + li("", "Mechanizm", U.esc(s2)) +
          li(act === "thr" ? "is-warn" : "", "Działanie", U.esc(s3)) + "</ol>" +
          (warn.length ? '<ul class="z6p20-warn">' + warn.map(function (w) { return "<li>" + U.esc(w) + "</li>"; }).join("") + "</ul>" : ""));
      }
      return {
        on: {
          shape: function (btn) { U.press(btn); },
          act: function (btn) { U.press(btn); },
          base: function () {
            [["z6p20a_mu", cfg.mu], ["z6p20a_sd", cfg.sd], ["z6p20a_c", cfg.c], ["z6p20a_n", cfg.n]].forEach(function (p) {
              var el = root.querySelector("#" + p[0]); if (el) el.value = p[1];
            });
          }
        },
        render: render
      };
    },

    // ---- P20B: Popraw raport -----------------------------------------------------------
    raporty: function (cfg, ui) {
      var i = 0, picked = null, root = ui.root;
      function render() {
        var r = cfg.reports[i], ok = picked === r.wrong;
        var sents = r.sentences.map(function (s, k) {
          var cls = "z6p20-sent" + (picked === k ? (ok ? " is-bad" : " is-fine") : "") + (ok && k === r.wrong ? " is-bad" : "");
          return '<li><button type="button" class="' + cls + '" data-z6p="pick" data-value="' + k + '"' +
            (ok ? " disabled" : "") + ">" + U.esc(s) + "</button></li>";
        }).join("");
        var fb = picked === null
          ? '<p class="z6p-caption">Kliknij zdanie, które jest błędne.</p>'
          : ok
            ? '<div class="z6p20-diag"><p><strong>Błąd:</strong> ' + U.esc(r.diag) + '</p><p><strong>Poprawnie:</strong> ' + U.esc(r.fix) + "</p></div>"
            : '<p class="z6p-caption">To zdanie jest w porządku. ' + U.esc(r.hint) + "</p>";
        ui.set("stage", '<div class="z6p20-report"><div class="z6p20-rhead">' + U.esc(r.head) + "</div>" +
          '<ol class="z6p20-sents">' + sents + "</ol></div>");
        ui.set("body", fb);
        ui.set("step", "Raport " + (i + 1) + " z " + cfg.reports.length);
        var prev = root.querySelector('[data-z6p="prevr"]'), next = root.querySelector('[data-z6p="nextr"]');
        if (prev) prev.disabled = i === 0;
        if (next) next.disabled = i === cfg.reports.length - 1;
      }
      return {
        on: {
          pick: function (btn) { picked = Number(btn.getAttribute("data-value")); },
          prevr: function () { if (i > 0) { i--; picked = null; } },
          nextr: function () { if (i < cfg.reports.length - 1) { i++; picked = null; } }
        },
        render: render
      };
    },

    // ---- P20C: Decyzja z budżetem ------------------------------------------------------
    budzet: function (cfg, ui) {
      var root = ui.root, on = {};
      function render() {
        var mu = cfg.mu, sd = cfg.sd, c = cfg.c, cost = 0, thrNoProof = false, chosen = [];
        cfg.actions.forEach(function (a) {
          var b = root.querySelector('[data-z6p="toggle"][data-value="' + a.key + '"]');
          var is = b && b.getAttribute("aria-pressed") === "true";
          if (!is) return;
          chosen.push(a); cost += a.cost;
          if (a.dmu) mu += a.dmu;
          if (a.sd) sd = Math.min(sd, a.sd);
          if (a.dc) { c += a.dc; if (!a.proof) thrNoProof = true; }
        });
        var p0 = tail(cfg.c, cfg.mu, cfg.sd, "normal"), p = tail(c, mu, sd, "normal");
        var pTrue = thrNoProof ? tail(cfg.c, mu, sd, "normal") : p;   // bez dowodu łożysko „widzi” stary próg
        var over = cost > cfg.budget, hit = pTrue * 1000 <= cfg.target;
        var status = !chosen.length ? "Wybierz działania."
          : over ? "Budżet przekroczony o " + (cost - cfg.budget) + " tys. zł."
          : hit ? "Cel osiągnięty w budżecie."
          : "Cel nieosiągnięty: potrzeba mocniejszego pakietu.";
        var msg = chosen.length
          ? "Pakiet: " + chosen.map(function (a) { return a.short; }).join(", ") + ". Koszt " + cost + " tys. zł. " +
            "Częstość przekroczeń 85°C: z " + per1000(p0) + " do " + per1000(pTrue) + " na 1000 pomiarów" +
            (thrNoProof ? " (w raporcie " + per1000(p) + ", bo próg podniesiono bez badania)." : ".")
          : "";
        ui.set("stage", miniCurve({ mu: cfg.mu, sd: cfg.sd, c: cfg.c }, { mu: mu, sd: sd, c: thrNoProof ? cfg.c : c }, "normal"));
        ui.set("body",
          '<p class="z6p-read">Koszt: ' + cost + " / " + cfg.budget + " tys. zł · μ = " + U.fmt(mu, 1) + "°C · σ = " + U.fmt(sd, 1) +
          "°C · próg " + U.fmt(c, 0) + "°C · przekroczenia: " + per1000(pTrue) + " na 1000 (cel ≤ " + cfg.target + ")</p>" +
          '<p class="z6p20-status ' + (!chosen.length ? "" : over || !hit ? "is-bad" : "is-ok") + '">' + U.esc(status) + "</p>" +
          (msg ? '<p class="z6p-caption">' + U.esc(msg) + "</p>" : ""));
      }
      on.toggle = function (btn) {
        var now = btn.getAttribute("aria-pressed") === "true";
        btn.setAttribute("aria-pressed", now ? "false" : "true");
        // Rewizja progu z badaniem i bez badania wykluczają się nawzajem.
        var grp = btn.getAttribute("data-group");
        if (!now && grp) root.querySelectorAll('[data-z6p="toggle"][data-group="' + grp + '"]').forEach(function (b) {
          if (b !== btn) b.setAttribute("aria-pressed", "false");
        });
      };
      on.clear = function () {
        root.querySelectorAll('[data-z6p="toggle"]').forEach(function (b) { b.setAttribute("aria-pressed", "false"); });
      };
      return { on: on, render: render };
    }
  });
})();
