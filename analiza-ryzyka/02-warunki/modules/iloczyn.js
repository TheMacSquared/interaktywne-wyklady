// Drzewo dróg z regułą iloczynu (rozdział „Mnożymy wzdłuż drogi”).
//
// Kontener .lc-il[data-config] z listą sytuacji (cfg.cases); każda sytuacja ma
// liczebności czterech liści na 1000 przypadków. Wybrana droga (klik w liść albo
// przycisk [data-il-path]) podświetla się: pierwsza gałąź bursztynowa, druga niebieska,
// liść burgundowy. Wzór pod drzewem (risk_annotated_formula w R) dostaje symbole,
// wartości i notki tej drogi przez [data-il]. Przełącznik [data-il-case] zmienia
// sytuację, gdy jest ich kilka.
//
// Całość działa w przeglądarce, bez serwera.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  // Liście: pi = B i A, pn = B bez A, ni = A bez B, nn = ani B, ani A.
  var LEAVES = ["pi", "pn", "ni", "nn"];

  function svg(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }
  function p3(x) { return x.toFixed(3); }
  function p2(x) { var s = x.toFixed(2); return Number(s) === x ? s : x.toFixed(3); }

  function init(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var cases = cfg.cases || [];
    var stage = root.querySelector(".lc-il-stage");
    var status = root.querySelector(".lc-il-status");
    var out = {};
    root.querySelectorAll("[data-il]").forEach(function (e) { out[e.getAttribute("data-il")] = e; });
    var ops = root.querySelectorAll(".lc-annot-op.lc-annot-value");
    var c = cases[0], path = "pi", W = 0;

    function counts() {
      var n = c.n, b = n.pi + n.pn, nb = n.ni + n.nn;
      return { n: n, b: b, nb: nb, total: b + nb };
    }

    function draw() {
      var width = stage.clientWidth;
      if (!width) return;
      // Na wąskim ekranie mniejszy viewBox: ten sam rozmiar czcionki, ciaśniejszy układ.
      var w = width < 560 ? 480 : 720;
      W = w;
      var k = counts(), h = 330, f = w < 600 ? 18 : 19;
      var xs = { root: w / 2, b: w / 4, nb: 3 * w / 4, pi: w / 8, pn: 3 * w / 8, ni: 5 * w / 8, nn: 7 * w / 8 };
      var ys = { root: 34, mid: 150, leaf: 272 };
      var leafW = w / 4 - 14, midW = w / 2 - 40, nodeH = 54;
      stage.innerHTML = "";
      var s = svg("svg", { viewBox: "0 0 " + w + " " + h, class: "lc-il-svg", role: "img",
        "aria-label": "Drzewo dróg: " + c.root + ", potem " + c.b + " albo " + c.nb + ", potem " + c.a + " albo " + c.na + "." }, stage);
      var edges = [
        { id: "b", from: "root", to: "b", p: k.b / k.total, y1: ys.root, y2: ys.mid },
        { id: "nb", from: "root", to: "nb", p: k.nb / k.total, y1: ys.root, y2: ys.mid },
        { id: "pi", from: "b", to: "pi", p: k.n.pi / k.b, y1: ys.mid, y2: ys.leaf },
        { id: "pn", from: "b", to: "pn", p: k.n.pn / k.b, y1: ys.mid, y2: ys.leaf },
        { id: "ni", from: "nb", to: "ni", p: k.n.ni / k.nb, y1: ys.mid, y2: ys.leaf },
        { id: "nn", from: "nb", to: "nn", p: k.n.nn / k.nb, y1: ys.mid, y2: ys.leaf }
      ];
      var first = path === "pi" || path === "pn" ? "b" : "nb";
      edges.forEach(function (e) {
        var role = e.id === first ? " is-first" : e.id === path ? " is-second" : " is-off";
        var x1 = xs[e.from], x2 = xs[e.to];
        svg("line", { x1: x1, y1: e.y1 + nodeH / 2, x2: x2, y2: e.y2 - nodeH / 2, class: "lc-il-edge" + role }, s);
        // Na drugim poziomie etykieta bliżej liścia, żeby sąsiednie się nie stykały.
        var t = e.from === "root" ? 0.5 : 0.62, pw = w < 600 ? 58 : 66;
        var mx = x1 + t * (x2 - x1), my = e.y1 + nodeH / 2 + t * (e.y2 - e.y1 - nodeH) + 4;
        var g = svg("g", { class: "lc-il-pill" + role }, s);
        svg("rect", { x: mx - pw / 2, y: my - 15, width: pw, height: 28, rx: 14 }, g);
        svg("text", { x: mx, y: my + 5, "text-anchor": "middle", style: "font-size:" + (f - 2) + "px" }, g, p3(e.p));
      });
      function node(id, x, y, nw, label, n, cls) {
        var g = svg("g", { class: "lc-il-node " + cls }, s);
        svg("rect", { x: x - nw / 2, y: y - nodeH / 2, width: nw, height: nodeH, rx: 7 }, g);
        svg("text", { x: x, y: y - 4, "text-anchor": "middle", style: "font-size:" + f + "px" }, g, label);
        svg("text", { x: x, y: y + 19, "text-anchor": "middle", class: "lc-il-num", style: "font-size:" + (f - 1) + "px" }, g, n);
        return g;
      }
      node("root", xs.root, ys.root, midW, c.root, k.total + " " + c.unit, "is-root");
      node("b", xs.b, ys.mid, midW, c.b, k.b, first === "b" ? "is-on" : "is-dim");
      node("nb", xs.nb, ys.mid, midW, c.nb, k.nb, first === "nb" ? "is-on" : "is-dim");
      LEAVES.forEach(function (id) {
        var yes = id === "pi" || id === "ni";
        var g = node(id, xs[id], ys.leaf, leafW, yes ? c.a : c.na, k.n[id],
          "is-leaf" + (id === path ? " is-sel" : " is-dim"));
        g.setAttribute("tabindex", "0");
        g.setAttribute("role", "button");
        g.setAttribute("aria-pressed", String(id === path));
        g.setAttribute("aria-label", leafName(id) + ": " + k.n[id] + " " + c.unit);
        g.addEventListener("click", function () { select(id); });
        g.addEventListener("keydown", function (e) {
          if (e.key === "Enter" || e.key === " ") { e.preventDefault(); select(id); }
        });
      });
    }

    function leafName(id) {
      return (id === "pi" || id === "pn" ? c.b : c.nb).toLowerCase() + ", " +
        (id === "pi" || id === "ni" ? c.a : c.na).toLowerCase();
    }

    function formula() {
      var k = counts(), inB = path === "pi" || path === "pn", yes = path === "pi" || path === "ni";
      var nGroup = inB ? k.b : k.nb, nLeaf = k.n[path];
      var A = yes ? "A" : "Aᶜ", B = inB ? "B" : "Bᶜ";
      var pPath = nLeaf / k.total, pFirst = nGroup / k.total, pSecond = nLeaf / nGroup;
      var exact = Math.abs(Number(p3(pFirst)) * Number(p3(pSecond)) - Number(p3(pPath))) < 1e-9;
      out.s0.textContent = "P(" + A + " ∩ " + B + ")";
      out.s1.textContent = "P(" + B + ")";
      out.s2.textContent = "P(" + A + " | " + B + ")";
      out.v0.textContent = p2(pPath);
      out.v1.textContent = p2(pFirst);
      out.v2.textContent = exact ? p2(pSecond) : p3(pSecond);
      if (ops[0]) ops[0].textContent = exact ? "=" : "≈";
      out.n0.textContent = nLeaf + " na " + k.total + " " + c.unit + ". Mianownik wraca do wszystkich " + c.unit + ".";
      out.n1.textContent = nGroup + " na " + k.total + " " + c.unit + ": " +
        ((inB ? c.note_b : c.note_nb) || (inB ? c.b : c.nb).toLowerCase()) + ".";
      out.n2.textContent = nLeaf + " na " + nGroup + " " + c.unit + " " + (inB ? c.in_b : c.in_nb) +
        ". Mianownik to " + nGroup + ".";
      status.textContent = "A — " + (c.a_def || c.a.toLowerCase()) + ", B — " + (c.b_def || c.b.toLowerCase()) +
        "; Aᶜ i Bᶜ to ich dopełnienia.";
    }

    function select(id) {
      path = id;
      root.querySelectorAll("[data-il-path]").forEach(function (b) {
        b.setAttribute("aria-pressed", String(b.getAttribute("data-il-path") === id));
      });
      draw(); formula();
    }

    root.addEventListener("click", function (e) {
      var b = e.target.closest("[data-il-path]");
      if (b) { select(b.getAttribute("data-il-path")); return; }
      var cb = e.target.closest("[data-il-case]");
      if (cb) {
        c = cases[Number(cb.getAttribute("data-il-case"))];
        root.querySelectorAll("[data-il-case]").forEach(function (x) {
          x.setAttribute("aria-pressed", String(x === cb));
        });
        select("pi");
      }
    });

    // Rozdział może być ukryty przy starcie: rysujemy, gdy panel dostanie szerokość.
    if (window.ResizeObserver) {
      new ResizeObserver(function () {
        var w = stage.clientWidth < 560 ? 480 : 720;
        if (w !== W) draw();
      }).observe(stage);
    }
    draw(); formula();
  }

  function scan() { document.querySelectorAll(".lc-il:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
