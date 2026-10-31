// Szkice B1–B8. Obliczenia rozkładów i kwantyle pochodzą z R.
(function () {
  "use strict";
  var C = window.lcCharts;
  if (!C) return;
  var fixed = function (v, d) { return Number(v).toFixed(d === undefined ? 2 : d); };
  var pct = function (v) { return fixed(100 * v, 1) + "%"; };
  var mean = function (xs) { return xs.reduce(function (a, b) { return a + b; }, 0) / xs.length; };
  var sd = function (xs) { var m = mean(xs); return xs.length > 1 ? Math.sqrt(xs.reduce(function (a, x) { return a + (x-m)*(x-m); }, 0) / (xs.length-1)) : NaN; };
  C.onReady(".bb-widget", function (root) {
    var cfg = JSON.parse(root.dataset.config), kind = root.dataset.kind;
    var canvas = root.querySelector("canvas"), read = root.querySelector(".bb-read");
    var state = { count: 0, batch: 0, truth: false, filtered: false, bad: new Set() };
    var P, inputs = {};
    root.querySelectorAll("[data-bb-input]").forEach(function (input) { inputs[input.dataset.bbInput] = input; });
    function val(key) { return Number(inputs[key].value); }
    function set(key, value) { inputs[key].value = value; }
    function say(text) { read.textContent = text; }
    function points(data, color, radius, label) { return { label: label || "", data: data,
      pointRadius: radius || 3, pointHoverRadius: (radius || 3) + 2,
      pointBackgroundColor: color, pointBorderWidth: 0, showLine: false }; }
    function line(lo, hi, y, color, width, label) { return { label: label || "",
      data: [{x:lo,y:y},{x:hi,y:y}], showLine:true, pointRadius:0,
      borderColor:color, borderWidth:width || 3 }; }
    var chart = C.create(canvas, {
      type: "scatter", data: {datasets:[]},
      options: { responsive:true, maintainAspectRatio:false, animation:false,
        layout: {padding:{top:16,right:12,bottom:8,left:12}},
        plugins: {legend:{display:false}, tooltip:{callbacks:{label:function(ctx) {
          return (ctx.dataset.label ? ctx.dataset.label + ": " : "") + fixed(ctx.parsed.x,2);
        }}}},
        scales: { x:{type:"linear", title:{display:true,text:""}, ticks:{maxTicksLimit:6,callback:function(v){
          return kind === "defects" ? Math.round(v*100)+"%" : String(Math.round(v*1000)/1000);
        }}},
          y:{type:"linear", min:0,max:5, afterBuildTicks:function(axis){
            if(axis.chart.$labels)axis.ticks=Object.keys(axis.chart.$labels).map(Number).sort(function(a,b){return a-b;}).map(function(v){return {value:v};});
          }, ticks:{stepSize:1,callback:function(y) {
            return this.chart.$labels && this.chart.$labels[y] || "";
          }},grid:{display:false}} }
      },
      plugins: [{id:"bbContext", beforeDatasetsDraw:function(ch) {
        var ctx=ch.ctx, xs=ch.scales.x, a=ch.chartArea;
        (ch.$bands || []).forEach(function(b) {
          ctx.save();ctx.fillStyle=b.color;ctx.fillRect(xs.getPixelForValue(b.lo),a.top,
            xs.getPixelForValue(b.hi)-xs.getPixelForValue(b.lo),a.bottom-a.top);ctx.restore();
        });
        (ch.$refs || []).forEach(function(x) {
          ctx.save();ctx.strokeStyle=C.palette().ink;ctx.lineWidth=1.2;ctx.setLineDash([5,5]);
          ctx.beginPath();ctx.moveTo(xs.getPixelForValue(x),a.top);ctx.lineTo(xs.getPixelForValue(x),a.bottom);ctx.stroke();ctx.restore();
        });
      },afterRender:function(){if(handle)syncHandle();}}]
    });
    function frame(lo, hi, ymin, ymax, labels, title) {
      var scales=chart.config.options.scales;
      scales.x.min=lo;scales.x.max=hi;scales.x.title.text=title;
      scales.y.min=ymin;scales.y.max=ymax;scales.y.display=!!labels;
      chart.$labels=labels;chart.$refs=[];chart.$bands=[];
    }
    function finish(ds) {
      chart.data.datasets=ds;
      C.styleAxes(chart,null,{fontSize:C.axisFontSize(root),maxRotation:0,includeBounds:false});
      chart.config.options.scales.x.title.text=C.axisTitle(chart.config.options.scales.x.title.text,Math.max(250,chart.width-40),C.axisFontSize(root));
      chart.config.options.scales.y.grid.display=false;
      chart.update("none");
      Object.keys(inputs).forEach(function(k) {
        var output=root.querySelector('[data-value="'+k+'"]');
        if(output)output.textContent=inputs[k].value;
      });
      if(kind==="difference") syncHandle();
    }
    var layer, handle, dragging=false;
    if(kind==="difference") {
      layer=C.handleLayer(root);
      handle=layer.add(canvas,{label:"Średnia linii 1",hint:"Strzałki zmieniają różnicę o 0.01 g/100 g; Shift o 0.1.",
        onKey:function(event){
          var v=C.keyValue(event,val("d"),0,.8,.01,.1);
          if(v===null)return false;set("d",fixed(v,2));draw();layer.announce(read.textContent);return true;
        },onPointerDown:function(event){dragging=true;canvas.setPointerCapture(event.pointerId);move(event);}});
      function move(event) {
        if(!dragging)return;
        var rect=canvas.getBoundingClientRect();
        var x=(event.clientX-rect.left)*chart.width/rect.width;
        var v=chart.scales.x.getValueForPixel(x);
        set("d",fixed(Math.max(0,Math.min(.8,v)),2));draw();
      }
      C.listen(canvas,"pointermove",move);
      C.listen(canvas,"pointerup",function(){dragging=false;layer.announce(read.textContent);});
      C.listen(canvas,"pointercancel",function(){dragging=false;});
      C.listen(canvas,"pointerdown",function(event){
        var rect=canvas.getBoundingClientRect(), y=(event.clientY-rect.top)*chart.height/rect.height;
        if(Math.abs(y-chart.scales.y.getPixelForValue(3))<24){dragging=true;canvas.setPointerCapture(event.pointerId);move(event);}
      });
    }
    function syncHandle() {
      if(!handle)return;
      handle.value(val("d"),0,.8,fixed(val("d"),2)+" g / 100 g");
      handle.position(chart.scales.x.getPixelForValue(val("d")),chart.scales.y.getPixelForValue(3));
    }
    if(kind==="coverage") {
      var map=root.querySelector(".bb-map");
      for(var m=0;m<100;m++)map.appendChild(document.createElement("span"));
    }
    if(kind==="defects") {
      var box=root.querySelector(".bb-products");
      for(var b=0;b<50;b++) {
        var button=document.createElement("button");button.type="button";button.textContent=String(b+1);
        button.dataset.product=String(b);button.setAttribute("aria-label","Sztuka "+(b+1)+": przełącz wadę");
        button.setAttribute("aria-pressed","false");box.appendChild(button);
      }
      C.listen(box,"click",function(e){
        var button=e.target.closest("[data-product]");if(!button)return;
        var k=Number(button.dataset.product);if(state.bad.has(k))state.bad.delete(k);else state.bad.add(k);draw();
      });
    }
    function draw() {
      P=C.palette(); var ds=[], rows, selected, i, y, lo, hi;
      if(kind==="measures") {
        frame(155,185,.4,4.6,{4:"Średnia",3:"Mediana",2:"Środek rozstępu",1:"Miarka +2"},"Oszacowanie wzrostu (cm)");
        rows=cfg.rows;selected=val("sample")-1;
        var keys=["mean_est","median_est","midrange_est","biased_est"];
        keys.forEach(function(key,j){
          var col=j===3?P.terakota:P.niebo;
          ds.push(points(rows.map(function(r,k){return {x:r[key],y:4-j+Math.sin(k*5.7)*.19};}),C.alpha(col,.25),2,cfg.labels[j]));
          ds.push(points([{x:rows[selected][key],y:4-j}],col,7,"Grupka "+(selected+1)));
        });
        if(state.truth)chart.$refs=[170];
        say("Grupka "+(selected+1)+" · średnia "+fixed(rows[selected].mean_est,1)+" · mediana "+fixed(rows[selected].median_est,1)+" · środek rozstępu "+fixed(rows[selected].midrange_est,1)+" · miarka +2: "+fixed(rows[selected].biased_est,1)+" cm");
      } else if(kind==="pizza") {
        frame(1,5,.5,2.5,{2:"5 opinii / dzień",1:"200 opinii / dzień"},"Średnia dzienna ocena (gwiazdki)");
        ["small","large"].forEach(function(key,j){
          var vals=cfg.slice(0,state.count).map(function(r,k){return {x:r[key],y:2-j+.18*Math.sin(k*7.7)};});
          ds.push(points(vals,C.alpha(j?P.niebo:P.bursztyn,.45),3));
          ds.push(points([{x:4.4,y:2-j}],P.accent,7,"Wczoraj"));
        });
        say(state.count?state.count+" dni · SD średnich: "+(state.count>1?fixed(sd(cfg.slice(0,state.count).map(function(r){return r.small;})),2)+" i "+fixed(sd(cfg.slice(0,state.count).map(function(r){return r.large;})),2):"jeszcze za mało dni")+" · kolejność: 5 i 200 opinii":"Wczoraj: obie 4.4. Której średniej bardziej ufasz?");
      } else if(kind==="coverage") {
        frame(158,182,0,31,null,"Średni wzrost (cm)");chart.$refs=[170];
        rows=cfg.groups[state.batch%cfg.groups.length];var tq=cfg.tq[val("level")-80],hits=0;
        rows.forEach(function(r,k){
          var me=tq*r.s/5,hit=r.xbar-me<=170&&r.xbar+me>=170;
          hits+=hit?1:0;map.children[k].classList.toggle("is-miss",!hit);
          if(k>=70)ds.push(line(r.xbar-me,r.xbar+me,k-69,hit?P.szalwia:P.terakota,2));
        });
        map.setAttribute("aria-label",hits+" trafień na 100 badań przy ufności "+val("level")+"%");
        say("Ufność "+val("level")+"% · trafia "+hits+" / 100 · kolor szałwiowy = obejmuje μ, terakota = chybia");
      } else if(kind==="people") {
        frame(130,210,-.45,1.08,null,"Wzrost (cm)");chart.$refs=[170];
        var curve=[],band=[];
        for(i=130;i<=210;i+=.25){var h=Math.exp(-.5*Math.pow((i-170)/10,2));curve.push({x:i,y:h});if(Math.abs(i-170)<=19.6)band.push({x:i,y:h});}
        ds.push({data:band,showLine:true,pointRadius:0,borderWidth:0,fill:{value:0},backgroundColor:C.alpha(P.niebo,.22)});
        ds.push({data:curve,showLine:true,pointRadius:0,borderColor:P.secondary,borderWidth:2});
        var me=cfg.z*10/Math.sqrt(val("n"));
        ds.push(line(150.4,189.6,-.12,P.secondary,4,"Centralne 95% wzrostów"));
        ds.push(line(170-me,170+me,-.32,P.accent,7,"95% CI średniej"));
        ds.push(points([{x:170,y:-.32}],P.accent,5));
        say("n = "+val("n")+" · pas wzrostów: 39.2 cm · CI średniej: "+fixed(2*me,2)+" cm · ludzie pozostają tak samo zróżnicowani");
      } else if(kind==="difference") {
        // Jedna oś wspólna: dane grup są pokazane jako odchylenia od stałej średniej linii 2.
        frame(-.55,1.25,.4,3.6,{3:"Linia 1",2:"Linia 2",1:"Różnica"},"Odchylenie od 9.08 lub różnica (g / 100 g)");
        chart.$refs=[0];var d=val("d"),a=cfg.margins[0],b=cfg.margins[1],dm=cfg.diff_margin;
        lo=Math.max(d-a,-b);hi=Math.min(d+a,b);
        if(lo<hi)chart.$bands=[{lo:lo,hi:hi,color:C.alpha(P.bursztyn,.16)}];
        ds.push(line(d-a,d+a,3,P.niebo,5),line(-b,b,2,P.secondary,5),line(d-dm,d+dm,1,P.accent,5));
        ds.push(points([{x:d,y:3},{x:0,y:2},{x:d,y:1}],P.bursztyn,5));
        say("Różnica "+fixed(d,2)+" · 95% CI ["+fixed(d-dm,2)+", "+fixed(d+dm,2)+"] · "+(d<=dm?"CI różnicy obejmuje zero":d<=a+b?"CI grup nakładają się, ale CI różnicy już nie obejmuje zera":"CI grup są rozłączne"));
      } else if(kind==="useful") {
        frame(-12,16,.5,1.5,null,"Oszczędność czasu: stara metoda − nowa (min)");
        chart.$refs=[0,2];chart.$bands=[{lo:-2,hi:2,color:C.alpha(P.szalwia,.14)}];
        var n=val("n"),effect=val("effect"),margin=cfg.tq[n-20]*10*Math.sqrt(2/n);
        lo=effect-margin;hi=effect+margin;
        ds.push(line(lo,hi,1,P.niebo,7),points([{x:effect,y:1}],P.accent,7));
        var stat=lo>0?"kierunek dodatni":hi<0?"kierunek ujemny":"kierunek nierozstrzygnięty";
        var practical=lo>2?"oszczędność przekracza 2 min":hi < -2?"nowa metoda wydłuża czas o ponad 2 min":lo>=-2&&hi<=2?"cały CI w pasie zmian mniejszych niż 2 min":"użyteczność nierozstrzygnięta";
        say("95% CI ["+fixed(lo,2)+", "+fixed(hi,2)+"] min · "+stat+" · "+practical);
      } else if(kind==="defects") {
        frame(-.12,1.12,.5,3.5,{3:"Wald",2:"Wilson",1:"Dokładny"},"Proporcja wadliwych sztuk");
        chart.$refs=[0,1];chart.$bands=[{lo:-.12,hi:0,color:C.alpha(P.terakota,.1)}];
        var x=state.bad.size,iv=cfg.intervals[x];
        ["wald","wilson","exact"].forEach(function(key,j){ds.push(line(iv[key][0],iv[key][1],3-j,j===0?P.terakota:j===1?P.niebo:P.szalwia,5,key));ds.push(points([{x:x/50,y:3-j}],P.bursztyn,4));});
        box.querySelectorAll("[data-product]").forEach(function(el){el.setAttribute("aria-pressed",String(state.bad.has(Number(el.dataset.product))));});
        say(x+" / 50 wad · Wald ["+pct(iv.wald[0])+", "+pct(iv.wald[1])+"] · Wilson ["+pct(iv.wilson[0])+", "+pct(iv.wilson[1])+"]"+(x===0?" · brak wad w próbie nie oznacza braku wad w populacji":""));
      } else if(kind==="publish") {
        frame(cfg.xlim[0],cfg.xlim[1],0,41,null,"Średni czas dojazdu (min)");chart.$refs=[26];
        rows=cfg.rows.slice(0,state.count);selected=rows.filter(function(r){return r.lo>26;});
        var visible=(state.filtered?selected:rows).slice(-40);
        visible.forEach(function(r,k){ds.push(line(r.lo,r.hi,k+1,r.lo<=26&&r.hi>=26?P.szalwia:P.terakota,2));});
        var cov=rows.filter(function(r){return r.lo<=26&&r.hi>=26;}).length;
        say(state.count?"Wszystkie: "+state.count+", pokrycie "+pct(cov/state.count)+" · wybrane: "+selected.length+" ("+pct(selected.length/state.count)+") · pokrycie w wybranych: "+(selected.length?"0%":"brak wyników")+" · widok: "+(state.filtered?"wybrane":"wszystkie"):"Najpierw wykonaj badania. Każdy wynik powstaje tą samą metodą 95% CI.");
      }
      if(kind==="pizza"||kind==="publish")root.querySelectorAll("[data-bb-action]").forEach(function(button){
        var key=button.dataset.bbAction;
        if(["one","twenty","hundred","thousand"].indexOf(key)>=0)button.disabled=state.count>=(kind==="pizza"?500:cfg.rows.length);
      });
      finish(ds);
    }
    Object.keys(inputs).forEach(function(k){C.listen(inputs[k],"input",draw);});
    C.listen(root,"click",function(e){
      var btn=e.target.closest("[data-bb-action]");if(!btn)return;var a=btn.dataset.bbAction;
      if(kind==="measures"&&a==="truth"){state.truth=!state.truth;btn.textContent=state.truth?"Ukryj μ":"Pokaż μ";btn.setAttribute("aria-pressed",String(state.truth));}
      if(kind==="pizza"){if(a==="reset")state.count=0;else state.count=Math.min(500,state.count+(a==="one"?1:a==="twenty"?20:100));}
      if(kind==="coverage"&&a==="new")state.batch++;
      if(kind==="people")set("n",Number(a.slice(1)));
      if(kind==="difference")set("d",a==="near"?.10:a==="trap"?.30:.60);
      if(kind==="useful"){set("effect",a==="tiny"?.4:a==="uncertain"?3:4);set("n",a==="tiny"?1000:a==="uncertain"?20:400);}
      if(kind==="defects"){state.bad.clear();for(var k=0;k<(a==="zero"?0:a==="one"?1:4);k++)state.bad.add(k);}
      if(kind==="publish"){
        if(a==="filter"){state.filtered=!state.filtered;btn.textContent=state.filtered?"Pokaż wszystkie":"Tylko dolna granica > 26";btn.setAttribute("aria-pressed",String(state.filtered));}
        else if(a==="reset"){state.count=0;state.filtered=false;var filter=root.querySelector('[data-bb-action="filter"]');filter.textContent="Tylko dolna granica > 26";filter.setAttribute("aria-pressed","false");}else state.count=Math.min(cfg.rows.length,state.count+(a==="one"?1:a==="hundred"?100:1000));
      }
      draw();
    });
    C.zoomControl(root);C.onTheme(root,draw);draw();
  });
})();
