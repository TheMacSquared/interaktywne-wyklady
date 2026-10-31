// Wspólna obsługa Chart.js. Ładować po Chart.js, przed skryptami widgetów.
// onReady(selector, init) wiąże zasoby z kontenerem tworzonym przez renderUI.
// create(), listen(), onTheme(), onDispose() zwalniają je po usunięciu kontenera.
(function () {
  "use strict";
  var widgets = new Map(), registrations = [];
  var messages = new Map(), registeredMessages = new Set();

  // Kanały sesji żyją dłużej niż kontenery rozdziałów. Widget przechowuje
  // ostatnie dane i odtwarza je w onReady(), także gdy wiadomość wyprzedzi DOM.
  function registerMessages() {
    if (!window.Shiny || !window.Shiny.addCustomMessageHandler) return;
    messages.forEach(function (fn, name) {
      if (registeredMessages.has(name)) return;
      window.Shiny.addCustomMessageHandler(name, fn);
      registeredMessages.add(name);
    });
  }

  function onMessage(name, fn) {
    messages.set(name, fn);
    registerMessages();
  }

  function owner(node) {
    var found;
    widgets.forEach(function (widget, root) {
      if (root === node || root.contains(node)) found = widget;
    });
    if (!found) throw new Error("lcCharts: element poza zarejestrowanym widgetem");
    return found;
  }

  function dispose(root, widget) {
    widgets.delete(root);
    if (widget.frame !== null) cancelAnimationFrame(widget.frame);
    widget.cleanups.reverse().forEach(function (fn) {
      try { fn(); } catch (err) { console.error(err); }
    });
    widget.charts.forEach(function (chart) { chart.destroy(); });
    delete root.dataset.ready;
  }

  function scan() {
    widgets.forEach(function (widget, root) {
      if (!root.isConnected) dispose(root, widget);
    });
    if (typeof window.Chart === "undefined") return;
    registrations.forEach(function (registration) {
      document.querySelectorAll(registration.selector).forEach(function (root) {
        if (widgets.has(root)) return;
        // Nie inicjujemy niekompletnego kontenera podczas wstawiania HTML.
        if (!root.querySelector("canvas")) return;
        var widget = { charts: new Set(), cleanups: [], themes: new Set(), frame: null };
        widgets.set(root, widget);
        try {
          if (registration.init(root) === false) dispose(root, widget);
          else root.dataset.ready = "1";
        } catch (err) {
          dispose(root, widget);
          console.error(registration.selector, err);
        }
      });
    });
  }

  function onReady(selector, init) {
    registrations.push({ selector: selector, init: init });
    scan();
  }

  function create(canvas, config) {
    var widget = owner(canvas);
    var chart = new window.Chart(canvas, config);
    widget.charts.add(chart);
    return chart;
  }

  // Do wymiany wykresu we wciąż otwartym widgecie (np. nowy scenariusz).
  function destroy(chart) {
    if (!chart) return;
    widgets.forEach(function (widget) { widget.charts.delete(chart); });
    chart.destroy();
  }

  function onDispose(root, fn) { owner(root).cleanups.push(fn); }
  function onTheme(root, fn) { owner(root).themes.add(fn); }

  function listen(node, type, fn, options) {
    node.addEventListener(type, fn, options);
    onDispose(node, function () { node.removeEventListener(type, fn, options); });
  }

  function cancelTween(root) {
    var widget = owner(root);
    if (widget.frame !== null) cancelAnimationFrame(widget.frame);
    widget.frame = null;
  }

  // Jedna animacja wartości na widget; kolejny wybór zastępuje poprzedni.
  function tween(root, from, to, step, done) {
    var widget = owner(root), t0 = null;
    cancelTween(root);
    function frame(ts) {
      if (!root.isConnected) { widget.frame = null; return; }
      if (t0 === null) t0 = ts;
      var u = Math.min(1, (ts - t0) / 380), k = 1 - Math.pow(1 - u, 3);
      step(from + (to - from) * k);
      if (u < 1) widget.frame = requestAnimationFrame(frame);
      else { widget.frame = null; if (done) done(); }
    }
    widget.frame = requestAnimationFrame(frame);
  }

  function token(name, fallback) {
    return getComputedStyle(document.documentElement).getPropertyValue(name).trim() || fallback || "";
  }

  function alpha(hex, a) {
    var h = String(hex).replace("#", "");
    if (h.length === 3) h = h.split("").map(function (c) { return c + c; }).join("");
    if (!/^[0-9a-f]{6}$/i.test(h)) return hex;
    var n = parseInt(h, 16);
    return "rgba(" + (n >> 16 & 255) + "," + (n >> 8 & 255) + "," + (n & 255) + "," + a + ")";
  }

  // ---- Liczby jako tekst: kropka dziesiętna, zwykły minus, opcjonalnie spacja tysięcy
  // toFixed zaokrągla dokładną wartość binarną, jak round() i sprintf() w R:
  // mediana (170.6 + 170.7) / 2 = 170.64999999999998 daje 170.6 w obu.
  function fmt(v, d, big) {
    if (!isFinite(v)) return "–";
    var s = Math.abs(v).toFixed(d || 0);
    if (big) {
      var parts = s.split(".");
      parts[0] = parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, " ");
      s = parts.join(".");
    }
    var neg = v < 0 && parseFloat(Math.abs(v).toFixed(d || 0)) !== 0;
    return (neg ? "-" : "") + s;
  }
  function round(v, d) { return parseFloat(v.toFixed(d || 0)); }
  function fmtSigned(v, d, big) {
    var r = parseFloat(Math.abs(v).toFixed(d || 0));
    return (v > 0 && r !== 0 ? "+" : "") + fmt(v, d, big);
  }

  function palette() {
    return {
      ink: token("--upwr-ink", "#222"), soft: token("--upwr-ink-soft", "#444"),
      subtle: token("--upwr-ink-subtle", "#888"), rule: token("--upwr-rule", "#ccc"),
      ruleSoft: token("--upwr-rule-soft", "#ddd"), surface: token("--upwr-surface", "#fff"),
      ref: token("--upwr-reference", "#8b8175"), accent: token("--upwr-accent", "#7a2e3b"),
      niebo: token("--upwr-cat-niebo", "#2f6fa8"), szalwia: token("--upwr-cat-szalwia", "#5b7f3a"),
      bursztyn: token("--upwr-cat-bursztyn", "#b06a12"), terakota: token("--upwr-cat-terakota", "#a4472b"),
      secondary: token("--upwr-secondary", "#555"), mono: token("--upwr-mono", "monospace")
    };
  }

  function styleAxes(chart, colors, options) {
    var P = colors || palette();
    // chart.options zawiera resolvery Proxy. Przypisanie ich z powrotem do
    // konfiguracji psuje m.in. skalę logarytmiczną; edytujemy surowe opcje.
    var scales = chart.config.options.scales;
    Object.keys(scales).forEach(function (k) {
      var s = scales[k];
      if (s.display === false) return;
      s.grid = Object.assign({}, s.grid || {}, { color: P.ruleSoft });
      s.border = Object.assign({}, s.border || {}, { color: P.rule });
      var fontSize = options && options.fontSize || 12;
      s.ticks = Object.assign({}, s.ticks || {}, { color: P.subtle, font: { family: P.mono, size: fontSize } });
      if (options && options.includeBounds !== undefined) s.ticks.includeBounds = options.includeBounds;
      if (k === "x" && options && options.maxRotation !== undefined) s.ticks.maxRotation = options.maxRotation;
      if (s.title) { s.title.color = P.subtle; s.title.font = { family: P.mono, size: options && options.titleSize || fontSize }; }
    });
  }

  function axisFontSize(root) {
    var size = parseFloat(getComputedStyle(root).fontSize) || 16;
    return Math.max(12, size * 0.875) * (root.dataset.lcChartLarge === "true" ? 1.5 : 1);
  }

  function zoomControl(root) {
    var button = root.querySelector("[data-lc-chart-zoom]");
    if (!button) return;
    listen(button, "click", function () {
      var large = root.dataset.lcChartLarge !== "true";
      root.setAttribute("data-lc-chart-large", String(large));
      button.setAttribute("aria-pressed", String(large));
      var label = large ? "Przywróć zwykły rozmiar wykresu" : "Powiększ wykres i napisy";
      button.setAttribute("aria-label", label);
      button.setAttribute("title", label);
    });
  }

  var nextHandleId = 0;
  // Warstwa kontrolek należy do widgetu; usunięcie punktu zwalnia również
  // jego zdarzenia. onKey zwraca true tylko dla obsłużonego klawisza.
  function handleLayer(root) {
    var handles = new Set(), status = document.createElement("span");
    status.setAttribute("class", "lc-chart-sr");
    status.setAttribute("role", "status");
    status.setAttribute("aria-live", "polite");
    status.setAttribute("aria-atomic", "true");
    root.appendChild(status);
    onDispose(root, function () {
      handles.forEach(function (handle) { handle.remove(); });
      status.remove();
    });
    return {
      announce: function (text) { status.textContent = text; },
      add: function (canvas, options) {
        var element = document.createElement("button"), hint = document.createElement("span");
        hint.id = "lc-chart-keys-" + (++nextHandleId);
        hint.setAttribute("class", "lc-chart-sr");
        hint.textContent = options.hint;
        element.setAttribute("type", "button");
        element.setAttribute("class", "lc-chart-handle");
        element.setAttribute("role", options.role || "slider");
        element.setAttribute("aria-label", options.label);
        element.setAttribute("aria-describedby", hint.id);
        if (options.role !== "button") element.setAttribute("aria-orientation", "horizontal");
        canvas.parentElement.appendChild(element);
        root.appendChild(hint);
        function keydown(event) {
          if (!options.onKey(event)) return;
          event.preventDefault();
          event.stopPropagation();
        }
        function pointerdown(event) {
          if (!options.onPointerDown) return;
          element.focus();
          options.onPointerDown(event);
        }
        element.addEventListener("keydown", keydown);
        element.addEventListener("pointerdown", pointerdown);
        var handle = {
          element: element,
          position: function (x, y) { element.style.left = x + "px"; element.style.top = y + "px"; },
          value: function (value, min, max, text) {
            element.setAttribute("aria-valuenow", value);
            element.setAttribute("aria-valuemin", min);
            element.setAttribute("aria-valuemax", max);
            element.setAttribute("aria-valuetext", text);
          },
          remove: function () {
            element.removeEventListener("keydown", keydown);
            element.removeEventListener("pointerdown", pointerdown);
            element.remove(); hint.remove(); handles.delete(handle);
          }
        };
        handles.add(handle);
        return handle;
      }
    };
  }

  // Wspólne klawisze jednowymiarowe; semantyka i krok należą do widgetu.
  function keyValue(event, value, min, max, step, largeStep) {
    var delta = event.shiftKey ? largeStep : step;
    if (event.key === "Home") return min;
    if (event.key === "End") return max;
    if (event.key === "ArrowRight" || event.key === "ArrowUp") return Math.min(max, value + delta);
    if (event.key === "ArrowLeft" || event.key === "ArrowDown") return Math.max(min, value - delta);
    return null;
  }

  // To samo przybliżenie Abramowitza–Steguna co w dotychczasowych widgetach.
  function normalCdf(z) {
    var x = Math.abs(z) / Math.SQRT2, t = 1 / (1 + 0.3275911 * x);
    var erf = 1 - t * (0.254829592 + t * (-0.284496736 + t * (1.421413741 +
      t * (-1.453152027 + t * 1.061405429)))) * Math.exp(-x * x);
    return z >= 0 ? (1 + erf) / 2 : (1 - erf) / 2;
  }

  // Prosta MNK y = a + b·x, r Pearsona i SD reszt (jak sd(residuals(lm(y ~ x))) w R).
  function fit(xs, ys) {
    var n = xs.length, mx = 0, my = 0, sxx = 0, syy = 0, sxy = 0, i;
    for (i = 0; i < n; i++) { mx += xs[i]; my += ys[i]; }
    mx /= n; my /= n;
    for (i = 0; i < n; i++) {
      var dx = xs[i] - mx, dy = ys[i] - my;
      sxx += dx * dx; syy += dy * dy; sxy += dx * dy;
    }
    var b = sxy / sxx, r = sxy / Math.sqrt(sxx * syy), sse = 0;
    // Różnica syy − b·sxy traci małe reszty przy niemal idealnej prostej.
    // Liczymy kwadraty reszt w układzie wycentrowanym, bez odejmowania dużych sum.
    for (i = 0; i < n; i++) {
      var residual = (ys[i] - my) - b * (xs[i] - mx);
      sse += residual * residual;
    }
    r = Math.max(-1, Math.min(1, r));
    return { a: my - b * mx, b: b, r: r, sd: Math.sqrt(sse / (n - 1)) };
  }
  // Kwantyl ograniczony do zakresu rysunku, jak w widgecie „Odwrotne pytanie”.
  function normalQuantile(p, mu, sd, lo, hi) {
    var a = lo, b = hi;
    for (var i = 0; i < 60; i++) {
      var m = (a + b) / 2;
      if (normalCdf((m - mu) / sd) < p) a = m; else b = m;
    }
    return (a + b) / 2;
  }

  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  new MutationObserver(function (records) {
    widgets.forEach(function (widget, root) {
      var changed = records.some(function (record) {
        return record.target === document.documentElement || record.target === root;
      });
      if (changed && root.isConnected) widget.themes.forEach(function (fn) { fn(); });
    });
  }).observe(document.documentElement, {
    attributes: true, subtree: true,
    attributeFilter: ["data-lc-theme", "data-lc-font-size", "data-lc-chart-large"]
  });
  document.addEventListener("DOMContentLoaded", scan);
  document.addEventListener("shiny:connected", scan);
  window.addEventListener("load", scan);
  document.addEventListener("DOMContentLoaded", registerMessages);
  document.addEventListener("shiny:connected", registerMessages);
  window.addEventListener("load", registerMessages);
  if (window.jQuery) window.jQuery(document).on("shiny:connected", function () {
    registerMessages();
    scan();
  });

  window.lcCharts = {
    onReady: onReady, create: create, destroy: destroy, listen: listen,
    onTheme: onTheme, onDispose: onDispose, onMessage: onMessage,
    tween: tween, cancelTween: cancelTween,
    token: token, alpha: alpha, palette: palette, styleAxes: styleAxes, normalCdf: normalCdf,
    linearFit: fit, normalQuantile: normalQuantile,
    axisFontSize: axisFontSize, zoomControl: zoomControl,
    handleLayer: handleLayer, keyValue: keyValue,
    fmt: fmt, fmtSigned: fmtSigned, round: round
  };
})();
