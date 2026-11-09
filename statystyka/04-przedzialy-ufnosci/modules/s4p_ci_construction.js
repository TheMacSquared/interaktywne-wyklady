/* Ryc. 3.2–3.3: dotychczasowy układ i kroki, wektorowy renderer ECharts. */
(function () {
  'use strict';
  const C = window.lcCharts;
  if (!C) return;
  const latest = {}, active = {};
  ['mean', 'diff'].forEach(kind => C.onMessage('ci-construction-' + kind, data => {
    latest[kind] = data;
    if (active[kind]) active[kind](data);
  }));
  C.onReady('#ch3_step, #ch3_dstep', function (root) {
    const holder = root.querySelector('.ci-construction');
    if (!holder) return false;
    const kind = holder.dataset.kind, diff = kind === 'diff';
    const bank = JSON.parse(holder.dataset.config), el = holder.querySelector('.lc-ec');
    let data = latest[kind] || bank[0], index = 0;
    const chart = C.chart(el, {}, {renderer: 'svg'});
    const f = n => n.toFixed(2), avg = xs => xs.reduce((a,b) => a+b,0)/xs.length;
    const connected = () => !!(window.Shiny && window.Shiny.shinyapp &&
      window.Shiny.shinyapp.$socket && window.Shiny.shinyapp.$socket.readyState === 1);
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
          label(0,.91,'0 = brak różnicy',P.ink,1,'right');
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
      if(!connected()) {
        const text=root.querySelector('#'+root.id+'_text');
        if(text) {
          const centerStep=diff?3:2, seStep=centerStep+1, ciStep=centerStep+2;
          const parts=[diff?'Dwie próby po 25 osób.':'25 pomiarów wzrostu.'];
          if(diff&&step>=2) parts.push('x̄₁ = '+f(avg(d.x))+' cm; x̄₂ = '+f(avg(d.y))+' cm.');
          if(step>=centerStep) parts.push((diff?'Różnica':'Średnia')+' = '+f(d.mean)+' cm.');
          if(step>=seStep) parts.push('SE = '+f(d.se)+' cm.');
          if(step>=ciStep) parts.push('t* = '+d.t.toFixed(3)+'; 95% CI: ['+f(d.lower)+'; '+f(d.upper)+'] cm.');
          text.textContent=parts.join(' ');
        }
      }
    }
    const receive = d => { data=d; draw(); };
    active[kind]=receive;
    const observer=new MutationObserver(draw);
    observer.observe(root,{attributes:true,attributeFilter:['data-lc-step','data-lc-chart-large']});
    const newButton=root.querySelector('[id$="_new_sample"]');
    const next=()=>{if(!connected()){index=(index+1)%bank.length;data=bank[index];draw();}};
    C.listen(newButton,'click',next);
    C.zoomControl(root); C.onTheme(root,draw);
    C.onDispose(root,()=>{
      observer.disconnect(); newButton.removeEventListener('click',next);
      if(active[kind]===receive) delete active[kind];
    });
    draw();
  });
})();
