// Ryc. 2.3: wykres kołowy i słupkowy. Ostatni scenariusz przeżywa zmianę rozdziału.
(function () {
  var Charts = window.lcCharts;
  var latest = null, current = null;

  function render(root, msg) {
    Charts.destroy(current.pie);
    Charts.destroy(current.bar);
    current.pie = current.bar = null;
    var pieCtx = root.querySelector("#ch2_pie_canvas");
    var barCtx = root.querySelector("#ch2_bar_canvas");
    current.pie = Charts.create(pieCtx, {
      type: 'pie',
      data: {
        labels: msg.labels,
        datasets: [{
          data: msg.data,
          backgroundColor: msg.colors,
          borderWidth: 1,
          borderColor: 'rgba(255,255,255,0.6)'
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        animation: { duration: 500 },
        plugins: {
          legend: { display: false },
          tooltip: {
            enabled: !!msg.reveal,
            callbacks: {
              label: function(ctx) { return ctx.label + ': ' + ctx.parsed + '%'; }
            }
          }
        }
      }
    });

    // Słupki (i ich wartości) rysujemy dopiero po odsłonięciu.
    if (msg.reveal) current.bar = Charts.create(barCtx, {
      type: 'bar',
      data: {
        labels: msg.labels,
        datasets: [{
          data: msg.data,
          backgroundColor: msg.colors,
          borderRadius: 4,
          barPercentage: 0.7
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        indexAxis: 'y',
        animation: { duration: 500 },
        scales: {
          x: {
            min: 0, max: 50,
            grid: { color: 'rgba(128,128,128,0.1)' },
            ticks: {
              callback: function(v) { return v + '%'; },
              color: 'rgba(128,128,128,0.5)',
              font: { size: 12 }
            }
          },
          y: {
            grid: { display: false },
            ticks: { color: 'rgba(128,128,128,0.6)', font: { size: 13 } }
          }
        },
        plugins: {
          legend: { display: false },
          tooltip: {
            callbacks: {
              label: function(ctx) { return ctx.parsed.x + '%'; }
            }
          }
        }
      }
    });
  }

  Charts.onReady(".lc-scenario", function (root) {
    if (!root.querySelector("#ch2_pie_canvas") || !root.querySelector("#ch2_bar_canvas")) return false;
    current = { root: root, pie: null, bar: null };
    Charts.onDispose(root, function () { current = null; });
    if (latest) render(root, latest);
  });

  Charts.onMessage("render_scenario", function (msg) {
    latest = msg;
    if (current && current.root.isConnected) render(current.root, msg);
  });
})();
