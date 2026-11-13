/* Ryc. 3.2–3.3: dotychczasowy układ i kroki, wektorowy renderer ECharts.
   Próby, statystyki i tekst kroku liczy przeglądarka (działa offline): pierwsza próba
   z banku policzonego w R (ci_construction_bank), „Nowa próba” losuje przez lcDist. */
(function () {
  'use strict';
  const C = window.lcCharts;
  if (!C) return;
  const mean = xs => xs.reduce((a, b) => a + b, 0) / xs.length;
  const variance = xs => { const m = mean(xs); return xs.reduce((a, b) => a + (b - m) * (b - m), 0) / (xs.length - 1); };
  // Jak ci_construction_stats() w R: średnia (różnica), SE, df (Welch), t*, granice 95% CI.
  function stats(x, y) {
    let center = mean(x), se = Math.sqrt(variance(x) / x.length), df = x.length - 1;
    if (y) {
      const a = variance(x) / x.length, b = variance(y) / y.length;
      center -= mean(y); se = Math.sqrt(a + b);
      df = (a + b) * (a + b) / (a * a / (x.length - 1) + b * b / (y.length - 1));
    }
    const t = window.lcDist.qt(0.975, df);
    return {x, y: y || null, mean: center, se, df, t, lower: center - t * se, upper: center + t * se};
  }
  function sample(diff) {
    const r = window.lcDist.r, draw = (n, mu, sd) => r.many(n, () => r.norm(mu, sd));
    return diff ? stats(draw(25, 178, 7), draw(25, 165, 6)) : stats(draw(25, 170, 10));
  }
  // round(v, d) z R: bez końcowych zer.
  const rd = (v, d) => { const s = v.toFixed(d); return s.indexOf('.') < 0 ? s : s.replace(/0+$/, '').replace(/\.$/, ''); };
  // Tekst kroku jak dawny output$ch3_step_text / ch3_dstep_text w R. Wzory jako HTML,
  // bo wersja offline nie ma MathJax (wzory wstawiane dynamicznie zostałyby surowym TeX-em).
  const XB = '<i>x̄</i>', xb = (k) => '<i>x̄</i><sub>' + k + '</sub>';
  function stepText(d, step, diff) {
    if (!diff) {
      const s = d.se * Math.sqrt(d.x.length), n = d.x.length, me = d.t * d.se;
      if (step === 1) return n + ' pomiarów wzrostu, każda kropka to jedna osoba. ' + XB + ' = ' + rd(d.mean, 2) +
        ' cm, <i>s</i> = ' + rd(s, 2) + ' cm.';
      if (step === 2) return 'Średnia z próby ' + XB + ' = ' + rd(d.mean, 2) +
        ' cm to estymata punktowa μ. Inna próba dałaby inną wartość.';
      if (step === 3) return '<i>SE</i> = <i>s</i>/√<i>n</i> = ' + rd(s, 2) + '/√' + n + ' = ' + rd(d.se, 2) +
        ' cm. Pasek ' + XB + ' ± <i>SE</i> to tylko około 67% ufności.';
      const covers = d.mean - me <= 170 && 170 <= d.mean + me;
      return '<i>t</i>* = ' + rd(d.t, 3) + ' (df = ' + (n - 1) + '), <i>ME</i> = ' + rd(d.t, 3) + ' · ' + rd(d.se, 2) +
        ' = ' + rd(me, 2) + ' cm. 95% CI: [' + rd(d.mean - me, 2) + ' ; ' + rd(d.mean + me, 2) + ']. ' +
        (covers ? 'Ten przedział zawiera μ = 170 cm.' : 'Ten przedział nie zawiera μ = 170 cm.');
    }
    const n1 = d.x.length, n2 = d.y.length, s1 = Math.sqrt(variance(d.x)), s2 = Math.sqrt(variance(d.y));
    const me = d.t * d.se;
    if (step === 1) return n1 + ' mężczyzn (niebieskie punkty) i ' + n2 + ' kobiet (bursztynowe). Każdy punkt to jedna osoba.';
    if (step === 2) return xb(1) + ' = ' + rd(mean(d.x), 2) + ' cm, ' + xb(2) + ' = ' + rd(mean(d.y), 2) +
      ' cm. Każda średnia ma własną niepewność.';
    if (step === 3) return xb(1) + ' − ' + xb(2) + ' = ' + rd(d.mean, 2) +
      ' cm. Dolny panel to skala różnicy: linia 0 oznacza brak różnicy.';
    if (step === 4) return '<i>SE</i> = √(' + rd(s1, 2) + '²/' + n1 + ' + ' + rd(s2, 2) + '²/' + n2 + ') = ' + rd(d.se, 2) +
      ' cm, mniej niż suma SE obu średnich (' + rd(s1 / Math.sqrt(n1) + s2 / Math.sqrt(n2), 2) + ' cm).';
    const lower = d.mean - me, upper = d.mean + me;
    return 'df Welcha ≈ ' + rd(d.df, 1) + ', <i>t</i>* = ' + rd(d.t, 3) + ', <i>ME</i> = ' + rd(d.t, 3) + ' · ' +
      rd(d.se, 2) + ' = ' + rd(me, 2) + ' cm. 95% CI: [' + rd(lower, 2) + ' ; ' + rd(upper, 2) + '] cm. ' +
      (lower <= 0 && 0 <= upper ? 'Przedział obejmuje 0.' :
        'Przedział nie obejmuje 0: mężczyźni są średnio wyżsi o co najmniej ' + rd(lower, 1) + ' cm.');
  }
  function typeset(el) {
    const MJ = window.MathJax;
    if (MJ && MJ.Hub && MJ.Hub.Queue) MJ.Hub.Queue(['Typeset', MJ.Hub, el]);
    else if (MJ && MJ.typesetPromise) MJ.typesetPromise([el]);
  }
  window.lcCiConstruction = {stats, stepText};
  C.onReady('#ch3_step, #ch3_dstep', function (root) {
    const holder = root.querySelector('.ci-construction');
    if (!holder) return false;
    const kind = holder.dataset.kind, diff = kind === 'diff';
    const bank = JSON.parse(holder.dataset.config), el = holder.querySelector('.lc-ec');
    let data = bank[0], lastText = '';
    // Tekst kroku pisze przeglądarka: #<id>_text nie jest wyjściem Shiny (jak w lcSF).
    const textEl = root.querySelector('#' + root.id + '_text');
    if (textEl) {
      if (window.Shiny && window.Shiny.unbindAll && textEl.classList.contains('shiny-bound-output'))
        window.Shiny.unbindAll(textEl.parentNode);
      textEl.classList.remove('shiny-html-output', 'shiny-bound-output', 'recalculating');
    }
    const chart = C.chart(el, {}, {renderer: 'svg'});
    const f = n => n.toFixed(2), avg = xs => xs.reduce((a,b) => a+b,0)/xs.length;
    function draw() {
      const d = data, step = Number(root.dataset.lcStep) || 1;
      const P = C.palette(), fs = C.axisFontSize(root), series = [];
      const color = intro => step === intro ? P.accent : P.ink;
      const add = (s, panel) => {
        s.xAxisIndex = panel || 0; s.yAxisIndex = panel || 0; s.silent = true;
        s.id = 'part-' + series.length; series.push(s);
      };
      const point = (x,y,col,size,panel) => add({type:'scatter',data:[[x,y]],symbol:'diamond',
        symbolSize:size || 13,itemStyle:{color:col,opacity:1},z:5},panel);
      const line = (points,col,width,panel,dashed) => add({type:'line',data:points,symbol:'none',
        lineStyle:{color:col,width:width || 2,type:dashed?'dashed':'solid'},z:2},panel);
      const label = (x,y,text,col,panel,position) => add({type:'scatter',data:[[x,y]],symbolSize:0,
        label:{show:true,formatter:text,position:position || 'bottom',distance:5,color:col,
          fontFamily:C.token('--upwr-sans'),fontSize:fs},z:7},panel);
      const interval = (m,w,y,col,panel,width) => {
        line([[m-w,y],[m+w,y]],col,width,panel);
        [-1,1].forEach(sign => line([[m+sign*w,y-.035],[m+sign*w,y+.035]],col,width,panel));
      };
      const raw = (xs, y, col) => add({type:'scatter',
        data:xs.map((x,i)=>[x,y+((i*7%13)-6)*.023]),symbolSize:6,
        itemStyle:{color:col,opacity:.65},z:1});
      const xRange = values => {
        const lo=Math.min(...values), hi=Math.max(...values), pad=(hi-lo)*.06 || 1;
        return [lo-pad,hi+pad];
      };
      const topRange = xRange(diff ? d.x.concat(d.y) : d.x.concat([d.mean-1.2*d.t*d.se,d.mean+1.2*d.t*d.se]));
      const xAxis = [], yAxis = [], grids = [];
      const axis = (range,name,panel) => {
        xAxis.push(C.ecAxis(P,fs,{type:'value',gridIndex:panel,min:range[0],max:range[1],
          splitNumber:5,name:name,nameGap:Math.round(fs*2),
          nameTextStyle:{fontFamily:C.token('--upwr-sans'),fontSize:fs,color:P.soft},
          axisLabel:{color:P.subtle,fontSize:fs,fontFamily:P.mono,hideOverlap:true,
            formatter:v=>String(Math.round(v*10)/10)}}));
        yAxis.push({type:'value',gridIndex:panel,min:0,max:1,show:false});
      };
      if (!diff) {
        grids.push({left:22,right:24,top:16,bottom:Math.round(fs*3.2)});
        axis(topRange,'Wzrost (cm)',0);
        raw(d.x,.82,P.niebo);
        if(step>=2) {
          line([[d.mean,.04],[d.mean,.98]],P.ref,1,0,true);
          point(d.mean,.53,color(2)); label(d.mean,.53,'x̄',color(2));
        }
        if(step>=3) {
          interval(d.mean,d.se,.34,color(3),0,step===3?4:2);
          label(d.mean,.34,'± SE',color(3));
        }
        if(step>=4) {
          interval(d.mean,d.t*d.se,.12,P.accent,0,4);
          label(d.mean,.12,'95% CI',P.accent);
        }
      } else {
        // Jedna rycina o dotychczasowej proporcji 2:1; dwie niezależne skale.
        grids.push({left:22,right:24,top:16,height:'43%'},
          {left:22,right:24,top:'68%',bottom:Math.round(fs*3.2)});
        axis(topRange,'Wzrost (cm)',0);
        const bottomRange = xRange([0,d.mean-1.3*d.t*d.se,d.mean+1.3*d.t*d.se]);
        axis(bottomRange,'Różnica średnich (cm)',1);
        raw(d.x,.70,P.niebo); raw(d.y,.23,P.bursztyn);
        label(topRange[0],.86,'Mężczyźni',P.niebo,0,'right');
        label(topRange[0],.39,'Kobiety',P.bursztyn,0,'right');
        if(step>=2) {
          [[avg(d.x),.70,'x̄₁'],[avg(d.y),.23,'x̄₂']].forEach(([m,y,name])=>{
            line([[m,.04],[m,.98]],P.ref,1,0,true);
            point(m,y,color(2)); label(m,.98,name+' = '+f(m),color(2),0,'top');
          });
        }
        if(step>=3) {
          line([[0,.06],[0,.98]],P.ref,1,1,true);
          point(d.mean,.52,color(3),13,1);
          if(step===3) label(d.mean,.52,'x̄₁ − x̄₂ = '+f(d.mean),color(3),1);
        }
        if(step>=4) {
          interval(d.mean,d.se,.52,color(4),1,step===4?4:2);
          if(step===4) label(d.mean,.65,'± SE = ±'+f(d.se),color(4),1,'top');
        }
        if(step>=5) {
          interval(d.mean,d.t*d.se,.52,C.alpha(P.accent,.65),1,5);
          label(d.mean,.65,'95% CI: ['+f(d.lower)+'; '+f(d.upper)+']',P.accent,1,'top');
        }
      }
      chart.setOption({animation:false,grid:grids,xAxis,yAxis,series,
        textStyle:{fontFamily:C.token('--upwr-sans'),fontSize:fs},tooltip:{show:false}},true);
      el.setAttribute('aria-label', (diff?'Różnica średnich':'Średnia')+'; krok '+step+
        '. Estymata '+f(d.mean)+' cm, SE '+f(d.se)+' cm'+
        (step===(diff?5:4)?', 95% CI od '+f(d.lower)+' do '+f(d.upper)+' cm.':'.'));
      if (textEl) {
        const html = stepText(d, step, diff);
        if (html !== lastText) { lastText = html; textEl.innerHTML = html; typeset(textEl); }
      }
    }
    const observer=new MutationObserver(draw);
    observer.observe(root,{attributes:true,attributeFilter:['data-lc-step','data-lc-chart-large']});
    const newButton=root.querySelector('[id$="_new_sample"]');
    const next=()=>{data=sample(diff);draw();};
    C.listen(newButton,'click',next);
    C.zoomControl(root); C.onTheme(root,draw);
    C.onDispose(root,()=>{
      observer.disconnect(); newButton.removeEventListener('click',next);
    });
    draw();
  });
})();
