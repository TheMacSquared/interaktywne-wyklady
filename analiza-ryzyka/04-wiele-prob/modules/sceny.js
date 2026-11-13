// Sceny wykładu 04 „Wiele prób” rysowane w przeglądarce (SVG), bez serwera R.
//
// Kontener .p4s-scene[data-scene][data-config]; dane i teksty przychodzą z R
// (modules/stop_proto_sceny.R, stop_proto_tabliczka.R), rysunek i stan są tutaj.
// Sloty: [data-p4s-out="head|stage|caption|calc|phobia|story"], przyciski
// [data-p4s="prev|next|one|ten|hund|reset|phobia"]. Sceny:
//   banany    — „Pająk w kiści” (kadry, żart z fobiami)
//   prog      — „Kontroler i próg” (kadry)
//   paleta    — „Partia na palecie” (losowanie partii, stos liczby wad)
//   kierowcy  — „Hala, 100 kierowców” (kolejne lata, rachunek pod sceną)
//   odbior    — „Odbiór dostawy” (losowanie dostaw, tabela decyzji)
//   tabliczka — „Tabliczka magazyniera” (Ω → X → przegródki → rozkład)
// Funkcje rysujące są w window.lcP4Scenes (testy: tests/js/p4_sceny.test.js).
(function () {
  "use strict";

  // --- Liczby jak w R --------------------------------------------------------

  // sprintf("%g"): sześć cyfr znaczących, bez zbędnych zer
  function g(x) {
    if (x === 0) return "0";
    return String(Number(x.toPrecision(6)));
  }
  function fixed(x, d) { return x.toFixed(d); }
  // format(n, big.mark = " ")
  function big(n) {
    var s = String(n);
    return s.length > 4 || n >= 1000 ? s.replace(/\B(?=(\d{3})+(?!\d))/g, " ") : s;
  }
  function sup(n) {
    var map = "⁰¹²³⁴⁵⁶⁷⁸⁹";
    return String(n).replace(/\d/g, function (d) { return map[+d]; });
  }
  function choose3(k) { return [1, 3, 3, 1][k]; }
  // format(signif(x, 3), scientific = FALSE)
  function signif3(x) {
    var v = Number(x.toPrecision(3));
    if (v >= 1e-6) return String(v);
    return v.toFixed(Math.max(0, 2 - Math.floor(Math.log10(v)))).replace(/0+$/, "");
  }
  function esc(s) {
    return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
  }

  function svgWrap(w, h, aria, body) {
    return '<svg class="p4s-svg" style="max-width:' + w + 'px" viewBox="0 0 ' + w + " " + h +
      '" role="img" aria-label="' + esc(aria) + '">' + body + "</svg>";
  }
  function head(name, step) {
    return '<div class="p4s-head"><span class="p4s-name">' + esc(name) + '</span><span class="p4s-step">' + esc(step) + "</span></div>";
  }
  function caption(text) { return '<p class="p4s-caption">' + esc(text) + "</p>"; }
  function range(n) { var a = []; for (var i = 0; i < n; i++) a.push(i); return a; }
  function sum(a) { return a.reduce(function (s, x) { return s + (x ? +x : 0); }, 0); }

  // --- (1a) Pająk w kiści -----------------------------------------------------

  function bananaGuest(bx, by, phobia) {
    if (phobia === 1) {
      var legs = "";
      [-1, 1].forEach(function (sg) {
        [-6, -2, 2, 6].forEach(function (dy) {
          legs += '<path d="M ' + g(bx) + " " + g(by + dy) + " l " + g(sg * 12) + " " + g(-5 + dy * 0.8) + " l " + g(sg * 5) +
            ' 8" fill="none" stroke="#1c1a17" stroke-width="2"/>';
        });
      });
      return legs + '<ellipse cx="' + g(bx) + '" cy="' + g(by + 2) + '" rx="8" ry="10" fill="#1c1a17"/><circle cx="' + g(bx) +
        '" cy="' + g(by - 11) + '" r="5.5" fill="#1c1a17"/>';
    }
    if (phobia === 2) {
      return '<circle cx="' + g(bx - 12) + '" cy="' + g(by - 6) + '" r="7" fill="#e0457b"/><circle cx="' + g(bx + 12) + '" cy="' + g(by - 6) +
        '" r="7" fill="#3a8fd6"/><circle cx="' + g(bx) + '" cy="' + g(by - 16) + '" r="6" fill="#f2a516"/>' +
        '<circle cx="' + g(bx) + '" cy="' + g(by) + '" r="13" fill="#fffaf0" stroke="#1c1a17" stroke-width="1.4"/>' +
        '<circle cx="' + g(bx - 5) + '" cy="' + g(by - 4) + '" r="1.8" fill="#1c1a17"/><circle cx="' + g(bx + 5) + '" cy="' + g(by - 4) + '" r="1.8" fill="#1c1a17"/>' +
        '<circle cx="' + g(bx) + '" cy="' + g(by + 1) + '" r="4" fill="#d62828"/>' +
        '<path d="M ' + g(bx - 7) + " " + g(by + 5) + ' q 7 7 14 0" fill="none" stroke="#d62828" stroke-width="2" stroke-linecap="round"/>';
    }
    return '<path d="M ' + g(bx) + " " + g(by - 18) + " L " + g(bx + 17) + " " + g(by + 12) + " L " + g(bx - 17) + " " + g(by + 12) +
      ' Z" fill="#f2c200" stroke="#1c1a17" stroke-width="2" stroke-linejoin="round"/>' +
      '<rect x="' + g(bx - 2) + '" y="' + g(by - 8) + '" width="4" height="11" rx="1.5" fill="#1c1a17"/><circle cx="' + g(bx) + '" cy="' + g(by + 7) +
      '" r="2.4" fill="#1c1a17"/>';
  }

  function bananaOne(cx, cy, open, spider, phobia) {
    var bunch = [-12, 0, 12].map(function (dy) {
      var x = cx, y = cy + dy;
      return '<path d="M ' + g(x - 30) + " " + g(y - 8) + " Q " + g(x - 2) + " " + g(y + 22) + " " + g(x + 26) + " " + g(y - 6) +
        " Q " + g(x - 2) + " " + g(y + 8) + " " + g(x - 30) + " " + g(y - 8) +
        ' Z" fill="#e7c33c" stroke="#a9821a" stroke-width="1.4"/><circle cx="' + g(x - 30) + '" cy="' + g(y - 8) + '" r="2" fill="#5a4a20"/>';
    }).join("");
    var stem = '<path d="M ' + g(cx + 24) + " " + g(cy - 14) + ' q 8 -4 12 -14" fill="none" stroke="#6b5a2a" stroke-width="4" stroke-linecap="round"/>';
    var guest = open && spider ? bananaGuest(cx - 2, cy - 8, phobia || 1) : "";
    var ring = open ? '<circle cx="' + g(cx) + '" cy="' + g(cy - 2) + '" r="40" fill="none" stroke="' +
      (spider ? "var(--upwr-accent)" : "var(--upwr-sage)") + '" stroke-width="2"/>' : "";
    return ring + bunch + stem + guest;
  }

  function bananasSvg(step, phobia, spider, who) {
    var open = step >= 2;
    var items = spider.map(function (sp, i) {
      var cx = 60 + (i % 6) * 112, cy = 70 + Math.floor(i / 6) * 120;
      return bananaOne(cx, cy, open, sp, phobia) + (step >= 3 ? '<text class="p4s-mono" x="' + g(cx) + '" y="' + g(cy + 50) +
        '" text-anchor="middle" font-size="20" font-weight="700" fill="' + (sp ? "var(--upwr-accent)" : "var(--upwr-ink-subtle)") + '">' +
        (sp ? 1 : 0) + "</text>" : "");
    }).join("");
    var total = step >= 3 ? '<text x="670" y="272" text-anchor="end" font-size="15" font-weight="600">X = liczba kiści ' +
      esc(who) + " = " + sum(spider) + "</text>" : "";
    return svgWrap(680, 280, "Dwanaście kiści bananów; po otwarciu w dwóch coś siedzi.", items + total);
  }

  // --- (1b) Kontroler i próg ---------------------------------------------------

  function thresholdSvg(step, cfg) {
    var x0 = 110, dx = 62, yt = 30, yb = 210;
    var P = cfg.pressure;
    function y(v) { return yb - (v - 2.6) / 0.8 * (yb - yt); }
    var tol = step >= 4 ? cfg.tolLate : cfg.tol;
    function out(v, t) { return v < t[0] || v > t[1]; }
    function band(t, cls, edge) {
      return '<rect class="' + cls + '" x="' + g(x0 - 30) + '" y="' + g(y(t[1])) + '" width="' + g(dx * 10) + '" height="' + g(y(t[0]) - y(t[1])) + '"/>' +
        '<line class="' + edge + '" x1="' + g(x0 - 30) + '" x2="' + g(x0 - 30 + dx * 10) + '" y1="' + g(y(t[0])) + '" y2="' + g(y(t[0])) + '"/>' +
        '<line class="' + edge + '" x1="' + g(x0 - 30) + '" x2="' + g(x0 - 30 + dx * 10) + '" y1="' + g(y(t[1])) + '" y2="' + g(y(t[1])) + '"/>';
    }
    var ticks = range(5).map(function (i) {
      var v = 2.6 + i * 0.2;
      return '<line class="p4s-axis" x1="' + g(x0 - 38) + '" x2="' + g(x0 - 32) + '" y1="' + g(y(v)) + '" y2="' + g(y(v)) + '"/>' +
        '<text class="p4s-mono p4s-subtle" x="' + g(x0 - 42) + '" y="' + g(y(v) + 4) + '" text-anchor="end" font-size="12">' + v.toFixed(1) + "</text>";
    }).join("");
    var bands = (step >= 2 ? band(cfg.tol, "p4s-band", "p4s-band-edge") : "") +
      (step >= 4 ? band(cfg.tolLate, "p4s-band-late", "p4s-band-late-edge") : "");
    var nBad = 0;
    var dots = P.map(function (v, i) {
      var cx = x0 + i * dx, bad = out(v, tol);
      if (bad) nBad++;
      var cls = step < 3 ? "p4s-idle" : bad ? "p4s-bad" : "p4s-ok";
      return '<circle class="' + cls + '" cx="' + g(cx) + '" cy="' + g(y(v)) + '" r="9"/>' +
        (step >= 4 && out(v, cfg.tol) ? '<circle cx="' + g(cx) + '" cy="' + g(y(v)) +
          '" r="15" fill="none" stroke="var(--upwr-accent)" stroke-width="1.6" stroke-dasharray="4 3"/>' : "") +
        '<text class="p4s-mono p4s-subtle" x="' + g(cx) + '" y="' + g(y(v) - 15) + '" text-anchor="middle" font-size="11">' + fixed(v, 2) + "</text>" +
        (step >= 3 ? '<text class="p4s-mono" x="' + g(cx) + '" y="252" text-anchor="middle" font-size="20" font-weight="700" fill="' +
          (bad ? "var(--upwr-accent)" : "var(--upwr-ink-subtle)") + '">' + (bad ? 1 : 0) + "</text>" : "");
    }).join("");
    return svgWrap(760, 300, "Dziesięć pomiarów ciśnienia otwarcia zaworów na tle pasa tolerancji.",
      '<line class="p4s-axis" x1="' + g(x0 - 35) + '" x2="' + g(x0 - 35) + '" y1="' + yt + '" y2="' + yb + '"/>' +
      '<text class="p4s-subtle" x="12" y="24" font-size="12">bar</text>' + ticks + bands + dots +
      (step >= 3 ? '<text x="' + g(x0 - 30) + '" y="288" font-size="15" font-weight="600">X = liczba jedynek = ' + nBad + "</text>" : ""));
  }

  // --- (2) Partia na palecie ---------------------------------------------------

  // Animacja (CSS): przy „Skontroluj partię” i „+10” partie z tej porcji (recent)
  // pojawiają się na palecie jedna po drugiej, a z każdej wylatuje kropka na swój
  // słupek. slot to czas jednej partii w ms; przy „+100” recent = null.
  function palletGrid(cur, cls, style) {
    return '<g class="' + cls + '" style="' + style + '">' + cur.map(function (bad, i) {
      return '<circle class="' + (bad ? "p4s-bad" : "p4s-ok") + '" cx="' + g(40 + (i % 10) * 30) + '" cy="' + g(50 + Math.floor(i / 10) * 26) + '" r="10"/>';
    }).join("") + "</g>";
  }

  function palletSvg(cur, xs, recent, slot) {
    var m = xs.length, kmax = 8, nNew = recent ? recent.length : 0;
    function bx(k) { return 400 + k * 44; }
    function dotY(r) { return 290 - (r - 0.5) * 11; }
    var grid;
    if (nNew) grid = recent.map(function (b, j) { return palletGrid(b, "p4s-layer", "animation-delay:" + Math.round(j * slot) + "ms"); }).join("");
    else if (cur.length) grid = palletGrid(cur, "", "");
    else grid = range(100).map(function (i) {
      return '<circle class="p4s-idle" cx="' + g(40 + (i % 10) * 30) + '" cy="' + g(50 + Math.floor(i / 10) * 26) + '" r="10"/>';
    }).join("");
    // Stos partii: jedna kropka = jedna partia; gdy słupek przekracza 22 kropki,
    // stos zamienia się w wykres słupkowy (wysokość proporcjonalna, liczba partii
    // nad słupkiem), żeby rzadkie wartości 7 i 8+ nie znikały przez zaokrąglenie.
    var bins = xs.map(function (x) { return Math.min(x, kmax); });
    var counts = range(kmax + 1).map(function () { return 0; }), rank = [];
    bins.forEach(function (b) { counts[b]++; rank.push(counts[b]); });
    var cmax = Math.max.apply(null, counts);
    var scaled = m > 0 && cmax > 22;
    function barH(c) { return c === 0 ? 0 : Math.max(2, c / cmax * 220); }
    function fly(i, j, isOut) {
      var k = bins[i], y = scaled ? 294 - barH(counts[k]) + 5 : dotY(rank[i]);
      return '<circle class="p4s-bad ' + (isOut ? "p4s-fly is-out" : "p4s-fly") + '" cx="' + g(bx(k)) + '" cy="' + g(y) +
        '" r="5" style="--dx:' + g(180 - bx(k)) + "px;--dy:" + g(165 - y) + "px;animation-delay:" + Math.round(j * slot + 0.35 * slot) +
        "ms;animation-duration:" + Math.round(0.6 * slot) + 'ms"/>';
    }
    var stack = "";
    if (!scaled) {
      for (var i = 0; i < m; i++) {
        stack += i >= m - nNew ? fly(i, i - (m - nNew), false) :
          '<circle class="p4s-bad" style="opacity:.85" cx="' + g(bx(bins[i])) + '" cy="' + g(dotY(rank[i])) + '" r="5"/>';
      }
    } else {
      stack = range(kmax + 1).map(function (k) {
        var c = counts[k], h = barH(c);
        return (c ? '<rect class="p4s-bad" style="opacity:.85" x="' + g(bx(k) - 13) + '" y="' + g(294 - h) + '" width="26" height="' + g(h) + '" rx="2"/>' : "") +
          '<text class="p4s-mono p4s-subtle" x="' + g(bx(k)) + '" y="' + g(294 - h - 5) + '" text-anchor="middle" font-size="11">' + c + "</text>";
      }).join("");
      for (var q = m - nNew; q < m; q++) stack += fly(q, q - (m - nNew), true);
    }
    var labels = range(kmax + 1).map(function (k) {
      return '<text class="p4s-mono p4s-subtle" x="' + g(bx(k)) + '" y="310" text-anchor="middle" font-size="12">' + (k === kmax ? "8+" : k) + "</text>";
    }).join("");
    return svgWrap(780, 320, "Paleta stu zaworów z zaznaczonymi niesprawnymi i stos liczby wad w " + m + " partiach.",
      '<text x="25" y="22" font-size="14" font-weight="600">Ta partia</text>' +
      '<text x="378" y="22" font-size="14" font-weight="600">Wszystkie partie (' + m + ")</text>" +
      '<rect x="20" y="32" width="320" height="270" rx="6" fill="none" stroke="var(--upwr-rule)"/>' +
      grid + '<line class="p4s-axis" x1="380" x2="' + g(bx(kmax) + 20) + '" y1="294" y2="294"/>' + labels + stack +
      (scaled ? '<text class="p4s-subtle" x="378" y="40" font-size="11">liczba partii nad słupkiem</text>' : ""));
  }

  // --- (9) Hala, 100 kierowców -------------------------------------------------

  function driversSvg(year, first) {
    var nHit = 0;
    var figs = first.map(function (f, i) {
      var cx = 40 + (i % 20) * 36, cy = 40 + Math.floor(i / 20) * 56, hit = f <= 250 * year;
      if (hit) nHit++;
      var cls = hit ? "p4s-bad" : "p4s-idle";
      return '<circle class="' + cls + '" cx="' + g(cx) + '" cy="' + g(cy) + '" r="7"/><rect class="' + cls + '" x="' + g(cx - 8) + '" y="' + g(cy + 9) +
        '" width="16" height="20" rx="5"/>' + (hit ? '<text class="p4s-mono" x="' + g(cx) + '" y="' + g(cy + 42) +
        '" text-anchor="middle" font-size="10" fill="var(--upwr-accent)">r' + Math.ceil(f / 250) + "</text>" : "");
    }).join("");
    return svgWrap(760, 300, "Sto kierowców wózków po " + year + " latach; " + nHit + " miało co najmniej jedną kolizję.", figs);
  }

  // Rachunek pod sceną: ryzyko skumulowane jednego kierowcy po y latach, wzór (4.6),
  // obok odsetka kierowców z co najmniej jedną kolizją w hali.
  function driversCalc(year, first) {
    var n = 250 * year, none = Math.pow(0.9999, n);
    var hall = first.filter(function (f) { return f <= n; }).length;
    function line(l, v) {
      return '<div class="p4s-calc-row"><span class="p4s-calc-l">' + esc(l) + '</span><span class="p4s-calc-v">' + esc(v) + "</span></div>";
    }
    return '<div class="p4s-calc">' +
      line("Ekspozycja skumulowana:", year === 0 ? "n = 0 przejazdów" : "n = 250 · " + year + " = " + big(n) + " przejazdów") +
      line("Bez kolizji w jednym przejeździe:", "1 - p = 1 - 0.0001 = 0.9999") +
      line("Bez kolizji przez całe n:", year === 0 ? "—" : "0.9999" + sup(n) + " ≈ " + fixed(none, 3)) +
      line("Ryzyko skumulowane (4.6):", year === 0 ? "—" : "1 - " + fixed(none, 3) + " ≈ " + fixed(1 - none, 3)) +
      line("W hali:", year === 0 ? "—" : hall + " na 100 kierowców z co najmniej jedną kolizją") +
      "</div>";
  }

  // --- (11) Odbiór dostawy -----------------------------------------------------

  function receiptDraw(cfg, rnd) {
    var q = rnd() < cfg.shareGood ? "good" : "weak", s = [];
    for (var i = 0; i < cfg.n; i++) s.push(rnd() < cfg.quality[q]);
    return { quality: q, sample: s, accepted: sum(s) <= cfg.c };
  }

  function receiptSvg(d, cfg) {
    function cell(cls, i) {
      return '<circle class="' + cls + '" cx="' + g(40 + (i % 10) * 30) + '" cy="' + g(50 + Math.floor(i / 10) * 28) + '" r="10"/>';
    }
    var cells = d ? d.sample.map(function (b, i) { return cell(b ? "p4s-bad" : "p4s-ok", i); }).join("") :
      range(cfg.n).map(function (i) { return cell("p4s-idle", i); }).join("");
    var stamp = "";
    if (d) {
      var ok = d.accepted, good = d.quality === "good", right = ok === good;
      var col = good ? "var(--upwr-sage)" : "var(--upwr-accent)";
      var verdict = right ? "✓ Decyzja trafna" : good ? "✗ Błąd: odrzucono dobrą dostawę" : "✗ Błąd: przepuszczono słabą dostawę";
      stamp = '<g transform="rotate(-8 520 90)"><rect class="' + (ok ? "p4s-stamp-box-ok" : "p4s-stamp-box-bad") +
        '" x="410" y="62" width="220" height="50" rx="6"/><text class="p4s-stamp ' + (ok ? "p4s-stamp-ok" : "p4s-stamp-bad") +
        '" x="520" y="97" text-anchor="middle">' + (ok ? "PRZYJĘTA" : "ODRZUCONA") + "</text></g>" +
        '<text x="410" y="160" font-size="15">Niesprawnych w próbce: <tspan class="p4s-mono" font-weight="700">' + sum(d.sample) +
        "</tspan> (limit c = " + cfg.c + ")</text>" +
        '<rect x="404" y="170" width="290" height="38" rx="6" fill="' + col + '" fill-opacity=".18" stroke="' + col + '" stroke-width="1.8"/>' +
        '<text x="416" y="195" font-size="15">Po fakcie: <tspan font-weight="800" fill="' + col + '">' +
        (good ? "DOBRA dostawa, p = " + cfg.quality.good : "SŁABA dostawa, p = " + cfg.quality.weak) + "</tspan></text>" +
        '<text x="410" y="232" font-size="16" font-weight="700" style="fill:' + (right ? "var(--upwr-sage)" : "var(--upwr-accent)") + '">' + verdict + "</text>";
    }
    return svgWrap(700, 245, "Próbka 50 zaworów z dostawy, decyzja o przyjęciu i prawdziwa jakość dostawy.",
      '<text x="25" y="22" font-size="14" font-weight="600">Próbka 50 zaworów z palety</text>' + cells + stamp);
  }

  function receiptTally(log, cfg) {
    function cnt(q, a) { return log.filter(function (r) { return r.quality === q && r.accepted === a; }).length; }
    return '<table class="p4s-tally"><tr><th></th><th>przyjęta</th><th>odrzucona</th></tr>' +
      "<tr><th>dobra (p = " + cfg.quality.good + ")</th><td>" + cnt("good", true) + '</td><td class="is-err">' + cnt("good", false) + "</td></tr>" +
      "<tr><th>słaba (p = " + cfg.quality.weak + ')</th><td class="is-err">' + cnt("weak", true) + "</td><td>" + cnt("weak", false) + "</td></tr></table>";
  }

  // --- Tabliczka magazyniera ---------------------------------------------------

  function tallyItem(kind, x, y, hit, phobia) {
    if (kind === "banana") return '<g transform="translate(' + g(x) + " " + g(y + 1) + ') scale(.38)">' + bananaOne(0, 0, true, hit, phobia) + "</g>";
    if (kind === "valve") return '<circle class="' + (hit ? "p4s-bad" : "p4s-ok") + '" cx="' + g(x) + '" cy="' + g(y) + '" r="10"/>';
    return '<rect class="' + (hit ? "p4s-bad" : "p4s-idle") + '" x="' + g(x - 9) + '" y="' + g(y - 9) + '" width="18" height="18" rx="3"/><text x="' + g(x) +
      '" y="' + g(y + 5) + '" text-anchor="middle" font-size="13" font-weight="700" fill="' + (hit ? "#fff" : "var(--upwr-ink-subtle)") + '">' + (hit ? "✓" : "✗") + "</text>";
  }

  function tallyBox(kind, x, y, row, phobia) {
    return '<rect x="' + g(x) + '" y="' + g(y) + '" width="86" height="40" rx="5" fill="var(--upwr-surface)" stroke="var(--upwr-rule)" stroke-width="1.3"/>' +
      [0, 1, 2].map(function (j) { return tallyItem(kind, x + 16 + j * 27, y + 20, row[j] === 1, phobia); }).join("");
  }

  function tallySvg(step, st, phobia, outcomes) {
    var p = st.p, up = step >= 3 ? 110 : 0;
    var H = (step >= 4 ? 500 : step === 3 ? 340 : 190) - up;
    function rowX(i) { return 8 + i * 93; }
    var colX = [130, 300, 470, 640];
    var kOf = outcomes.map(sum), seen = [0, 0, 0, 0];
    var rankIn = kOf.map(function (k) { return ++seen[k]; });
    var subs = "₁₂₃₄₅₆₇₈";
    var boxes = outcomes.map(function (row, i) {
      var k = kOf[i], x, y;
      if (step < 3) {
        x = rowX(i); y = 36;
        return tallyBox(st.item, x, y, row, phobia) + '<text class="p4s-mono p4s-subtle" x="' + g(x + 43) + '" y="' + g(y - 6) +
          '" text-anchor="middle" font-size="15">ω' + subs[i] + "</text>";
      }
      x = colX[k] - 43; y = 200 + (rankIn[i] - 1) * 46;
      var anim = step === 3 ? ' class="p4s-slide" style="--dx:' + g(rowX(i) - x) + "px;--dy:" + g(36 + up - y) + "px;animation-delay:" + i * 90 + 'ms"' : "";
      return "<g" + anim + ">" + tallyBox(st.item, x, y, row, phobia) + "</g>";
    }).join("");
    var arrows = step === 2 ? outcomes.map(function (row, i) {
      return '<line x1="' + g(rowX(i) + 43) + '" y1="78" x2="' + g(colX[kOf[i]]) + '" y2="136" stroke="var(--upwr-accent)" stroke-width="1.4" marker-end="url(#p4t-arrow)"/>';
    }).join("") : "";
    var plaques = step >= 2 ? [0, 1, 2, 3].map(function (k) {
      var c = choose3(k);
      return '<rect x="' + g(colX[k] - 62) + '" y="132" width="124" height="46" rx="5" fill="#8a6a3a"/><rect x="' + g(colX[k] - 58) +
        '" y="136" width="116" height="38" rx="4" fill="#fffaf0"/>' +
        '<text class="p4s-mono" x="' + g(colX[k]) + '" y="163" text-anchor="middle" font-size="24" font-weight="700">X = ' + k + "</text>" +
        (step >= 3 ? '<text class="p4s-subtle" x="' + g(colX[k]) + '" y="192" text-anchor="middle" font-size="15">' + c + " " +
          esc(st.unit[c === 1 ? 0 : 1]) + "</text>" : "");
    }).join("") : "";
    var bars = "";
    if (step >= 4) {
      bars = '<line class="p4s-axis" x1="40" x2="720" y1="490" y2="490"/>' + [0, 1, 2, 3].map(function (k) {
        var one = Math.pow(p, k) * Math.pow(1 - p, 3 - k), pr = choose3(k) * one, h = Math.max(1.5, pr * 110);
        return '<text class="p4s-mono p4s-subtle" x="' + g(colX[k]) + '" y="' + (490 - 110 - 22) + '" text-anchor="middle" font-size="16">' +
          choose3(k) + " · " + signif3(one) + "</text>" +
          '<rect class="p4s-bad' + (step === 4 ? " p4s-grow" : "") + '" x="' + g(colX[k] - 28) + '" y="' + g(490 - h) + '" width="56" height="' + g(h) + '" rx="2"/>' +
          '<text class="p4s-mono" x="' + g(colX[k]) + '" y="' + g(490 - h - 6) + '" text-anchor="middle" font-size="20" font-weight="700">' +
          (pr < 0.001 ? fixed(pr, 6) : fixed(pr, 3)) + "</text>";
      }).join("");
    }
    return svgWrap(760, H, "Osiem możliwych wyników trzech prób, liczba X przypisana każdemu wynikowi i rozkład X przy p = " + g(p) + ".",
      '<defs><marker id="p4t-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" markerHeight="6" orient="auto-start-reverse"><path d="M0 0 L10 5 L0 10 z" fill="var(--upwr-accent)"/></marker></defs>' +
      '<text x="8" y="12" font-size="13" font-weight="600">' + (step < 3 ? "Ω: wszystkie możliwe wyniki" : "") + "</text>" +
      arrows + '<g transform="translate(0 ' + (up ? -up : 0) + ')">' + plaques + boxes + bars + "</g>");
  }

  // --- Sterowanie --------------------------------------------------------------

  function mulberry32(a) {
    return function () {
      a |= 0; a = a + 0x6D2B79F5 | 0;
      var t = Math.imul(a ^ a >>> 15, 1 | a);
      t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
      return ((t ^ t >>> 14) >>> 0) / 4294967296;
    };
  }

  function phobiaButton(label) {
    return typeof label === "string" && label ? '<button type="button" class="lc-action is-ghost" data-p4s="phobia"><span>' + esc(label) + "</span></button>" : "";
  }

  var scenes = {
    banany: function (cfg, ui) {
      var s = { f: 0, phobia: 1 }, n = cfg.frames.length;
      return {
        on: { prev: function () { s.f = Math.max(0, s.f - 1); }, next: function () { s.f = Math.min(n - 1, s.f + 1); },
          phobia: function () { s.phobia = Math.min(3, s.phobia + 1); } },
        render: function () {
          var F = cfg.frames[s.f], ph = cfg.phobia[s.phobia - 1];
          ui.set("head", head(F.name, "Kadr " + (s.f + 1) + " z " + n));
          ui.set("stage", bananasSvg(s.f + 1, s.phobia, cfg.spider, ph.who));
          ui.set("caption", caption(F.text));
          ui.set("phobia", s.f >= 1 ? phobiaButton(ph.button) : "");
          ui.ends(s.f, n);
        }
      };
    },
    prog: function (cfg, ui) {
      var s = { f: 0 }, n = cfg.frames.length;
      return {
        on: { prev: function () { s.f = Math.max(0, s.f - 1); }, next: function () { s.f = Math.min(n - 1, s.f + 1); } },
        render: function () {
          var F = cfg.frames[s.f];
          ui.set("head", head(F.name, "Kadr " + (s.f + 1) + " z " + n));
          ui.set("stage", thresholdSvg(s.f + 1, cfg));
          ui.set("caption", caption(F.text));
          ui.ends(s.f, n);
        }
      };
    },
    paleta: function (cfg, ui, rnd) {
      var s;
      function reset() { s = { cur: [], xs: [], recent: null, slot: 0 }; }
      function add(m) {
        var batch = range(m).map(function () { return range(cfg.size).map(function () { return rnd() < cfg.p; }); });
        batch.forEach(function (b) { s.xs.push(sum(b)); });
        s.cur = batch[m - 1]; s.recent = m <= 10 ? batch : null; s.slot = m === 1 ? 1400 : 500;
      }
      reset();
      return {
        on: { one: function () { add(1); }, ten: function () { add(10); }, hund: function () { add(100); }, reset: reset },
        render: function () {
          var m = s.xs.length;
          ui.set("head", head(cfg.title, m ? "Partia nr " + m + " · X = " + sum(s.cur) : "Paleta czeka"));
          ui.set("stage", palletSvg(s.cur, s.xs, s.recent, s.slot));
          ui.set("caption", caption(m === 0 ? cfg.captions[0] : m < 10 ? cfg.captions[1] : cfg.captions[2]));
        }
      };
    },
    kierowcy: function (cfg, ui) {
      var s = { y: 0 };
      return {
        on: { next: function () { s.y = Math.min(10, s.y + 1); }, reset: function () { s.y = 0; } },
        render: function () {
          var y = s.y, k = cfg.first.filter(function (f) { return f <= 250 * y; }).length;
          ui.set("head", head("Rok " + y + " · " + big(250 * y) + " przejazdów na kierowcę", "Z kolizją: " + k + " na 100"));
          ui.set("stage", driversSvg(y, cfg.first));
          ui.set("calc", driversCalc(y, cfg.first));
          ui.set("caption", caption(y === 0 ? cfg.captions[0] : y < 10 ? cfg.captions[1] :
            cfg.captions[2].replace("{k}", cfg.first.filter(function (f) { return f <= 2500; }).length)));
          var next = ui.root.querySelector('[data-p4s="next"]');
          if (next) next.disabled = y >= 10;
        }
      };
    },
    odbior: function (cfg, ui, rnd) {
      var s;
      function reset() { s = { last: null, log: [] }; }
      function add(m) {
        for (var i = 0; i < m; i++) { s.last = receiptDraw(cfg, rnd); s.log.push({ quality: s.last.quality, accepted: s.last.accepted }); }
      }
      reset();
      return {
        on: { one: function () { add(1); }, ten: function () { add(20); }, hund: function () { add(100); }, reset: reset },
        render: function () {
          ui.set("head", head(cfg.title, "Dostaw: " + s.log.length));
          ui.set("stage", receiptSvg(s.last, cfg));
          ui.set("caption", s.log.length ? receiptTally(s.log, cfg) + caption(cfg.captions[1]) : caption(cfg.captions[0]));
        }
      };
    },
    tabliczka: function (cfg, ui) {
      var s = { f: 0, story: cfg.storyOrder[0], phobia: 1 }, n = cfg.frameNames.length;
      return {
        on: { prev: function () { s.f = Math.max(0, s.f - 1); }, next: function () { s.f = Math.min(n - 1, s.f + 1); },
          phobia: function () { s.phobia = Math.min(3, s.phobia + 1); },
          story: function (btn) { s.story = btn.getAttribute("data-story"); } },
        render: function () {
          var st = cfg.stories[s.story], last = s.f === n - 1;
          ui.set("head", head(cfg.frameNames[s.f], "Kadr " + (s.f + 1) + " z " + n));
          ui.set("stage", tallySvg(s.f + 1, st, s.phobia, cfg.outcomes));
          ui.set("caption", caption(last ? cfg.frame5 : st.frames[s.f]));
          ui.set("phobia", s.story === cfg.storyOrder[0] ? phobiaButton(cfg.phobia[s.phobia - 1].button) : "");
          ui.set("story", last ? '<div class="lc-seg" role="group" aria-label="Historia">' + cfg.storyOrder.map(function (key) {
            return '<button type="button" data-p4s="story" data-story="' + key + '" aria-pressed="' + (key === s.story) + '">' +
              esc(cfg.stories[key].label) + "</button>";
          }).join("") + "</div>" : "");
          ui.ends(s.f, n);
        }
      };
    }
  };

  function init(root) {
    if (root.dataset.ready) return;
    var make = scenes[root.getAttribute("data-scene")];
    if (!make) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.getAttribute("data-config") || "{}");
    var rnd = cfg.seed ? mulberry32(cfg.seed) : Math.random;
    var ui = {
      root: root,
      set: function (slot, html) {
        var el = root.querySelector('[data-p4s-out="' + slot + '"]');
        if (el && el.innerHTML !== html) el.innerHTML = html;
      },
      ends: function (f, n) {
        var prev = root.querySelector('[data-p4s="prev"]'), next = root.querySelector('[data-p4s="next"]');
        if (prev) prev.disabled = f === 0;
        if (next) next.disabled = f === n - 1;
      }
    };
    var scene = make(cfg, ui, rnd);
    root.addEventListener("click", function (e) {
      var btn = e.target.closest && e.target.closest("[data-p4s]");
      if (!btn || !root.contains(btn)) return;
      var fn = scene.on[btn.getAttribute("data-p4s")];
      if (!fn) return;
      fn(btn);
      scene.render();
    });
    scene.render();
  }

  (typeof window !== "undefined" ? window : globalThis).lcP4Scenes = {
    bananasSvg: bananasSvg, thresholdSvg: thresholdSvg, palletSvg: palletSvg, driversSvg: driversSvg,
    driversCalc: driversCalc, receiptSvg: receiptSvg, receiptDraw: receiptDraw, tallySvg: tallySvg,
    mulberry32: mulberry32, init: init
  };

  if (typeof document === "undefined") return;
  function scan() { document.querySelectorAll(".p4s-scene:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
