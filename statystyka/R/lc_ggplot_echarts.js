/* Rysunek wykresów ggplot w ECharts SVG (lc_ggplot_echarts.R): dane i skale z R. */
(function () {
  'use strict';
  const C=window.lcCharts;
  if(!C) return;
  C.onReady('.lc-ec-plot',function(root) {
    const el=root.querySelector('.lc-ec'), config=root.querySelector('.lc-ec-plot-config');
    if(!el || !config || !config.querySelector('[data-ec-spec]')) return false;
    let chart;
    function draw() {
      if(!root.isConnected || (chart && chart.isDisposed())) return;
      const source=config.querySelector('[data-ec-spec]');
      if(!source || !chart) return;
      const spec=JSON.parse(source.dataset.ecSpec), P=C.palette();
      const stepper=root.closest('.lc-stepper');
      const fs=C.axisFontSize(stepper || root), W=chart.getWidth(), H=chart.getHeight();
      const grids=[],xAxis=[],yAxis=[],series=[],graphic=[];
      const color=original=>{
        const key=spec.color_keys[original];
        if(key) return C.token('--upwr-'+(['accent','secondary','reference'].includes(key)?key:'cat-'+key),original);
        return original;
      };
      spec.panels.forEach((p,pi)=>{
        const box=p.box, yWidth=p.y.show?Math.min(W*.24,Math.max(36,...p.y.labels.map(s=>s.length*fs*.53))):8;
        // Wielowierszowe etykiety osi x (np. zawinięte nazwy kategorii) przesuwają nazwę osi niżej.
        const xLines=p.x.show?Math.max(1,...p.x.labels.map(s=>String(s).split('\n').length)):1, xExtra=(xLines-1)*fs*1.2;
        const yName=p.y.name?fs*1.6:0, left=box[0]*W+yWidth+8+yName, top=box[1]*H+(p.label?fs*2:fs*.9);
        const width=Math.max(20,box[2]*W-yWidth-28-yName), height=Math.max(20,box[3]*H-fs*(p.label?5.2:4.2)-xExtra);
        grids.push({left,top,width,height,outerBoundsMode:'none'});
        const axis=(a,vertical)=>{
          const labels=a.labels,breaks=a.breaks;
          const fmt=v=>{
            const at=breaks.findIndex(b=>Math.abs(b-v)<1e-7*Math.max(1,Math.abs(v)));
            return at>=0?labels[at]:String(Math.round(v*100)/100);
          };
          const discrete=labels.some((s,i)=>!Number.isFinite(Number(s)) || Math.abs(Number(s)-breaks[i])>1e-6);
          // Kraniec osi dostaje etykietę tylko wtedy, gdy jest podziałką z R
          // (bez „24.45” czy „475.9” uciętych przy brzegu).
          const isBreak=v=>breaks.some(b=>Math.abs(b-v)<1e-7*Math.max(1,Math.abs(v)));
          // Etykiety tekstowe (np. „1 ★”, nazwy wierszy) stoją dokładnie na podziałkach R;
          // automatyczne podziałki ECharts zaczynają się od krańca osi i by je minęły.
          // To samo dla nierównych odstępów, np. wartości krytycznych ±1.96 i ±2.78.
          const step=breaks.length>1?breaks[1]-breaks[0]:0;
          const uneven=breaks.some((v,i)=>i>0 && Math.abs(v-breaks[i-1]-step)>1e-6*Math.max(1,Math.abs(step)));
          const pinned=discrete || uneven, custom=pinned?breaks:undefined;
          return C.ecAxis(P,fs,{type:'value',gridIndex:pi,min:a.range[0],max:a.range[1],
            splitNumber:5,name:a.name,nameGap:vertical?fs*3:fs*2+xExtra,
            nameTextStyle:{fontFamily:C.token('--upwr-sans'),fontSize:fs,color:P.soft},
            axisLabel:{show:a.show,customValues:custom,
              showMinLabel:pinned?undefined:isBreak(a.range[0]),showMaxLabel:pinned?undefined:isBreak(a.range[1]),
              color:P.subtle,fontSize:fs,fontFamily:vertical?C.token('--upwr-sans'):P.mono,
              hideOverlap:!pinned,formatter:pinned?v=>{const at=breaks.findIndex(b=>Math.abs(b-v)<1e-7*Math.max(1,Math.abs(v)));return at>=0?labels[at]:'';}:fmt},
            axisLine:{show:!vertical || a.show,onZero:false,lineStyle:{color:P.rule}},
            axisTick:{show:a.show,customValues:custom,lineStyle:{color:P.rule}},
            splitLine:{show:!pinned && (!vertical || a.show),lineStyle:{color:P.ruleSoft}}});
        };
        xAxis.push(axis(p.x,false));yAxis.push(axis(p.y,true));
        if(p.label) graphic.push({type:'text',left,top:box[1]*H,style:{text:p.label,fill:P.ink,fontSize:fs,fontFamily:C.token('--upwr-sans')}});
        series.push({id:'panel-'+pi,type:'custom',xAxisIndex:pi,yAxisIndex:pi,
          data:p.elements.map((_,i)=>[i]),silent:true,clip:false,
          renderItem:(params,api)=>{
            const e=p.elements[params.dataIndex],opacity=e.alpha;
            const stroke=color(e.color), fill=color(e.fill);
            const strokeWidth=Math.max(1,e.width*2), coords=e.coords;
            const coord=point=>api.coord(point);
            const dash=e.dash==='solid'||e.dash==='1'?undefined:e.dash==='dotted'?[2,3]:[5,4];
            let shape;
            if(e.type==='text') {
              const pt=coord(coords),size=Math.max(fs,fs*e.size/4.5);
              // vjust z ggplot oznacza przesunięcie względem wysokości tekstu.
              const x=pt[0],y=pt[1]+(e.vjust-.5)*size;
              return {type:'text',x,y,rotation:(e.angle||0)*Math.PI/180,
                style:{text:e.text,fill:stroke,opacity,fontFamily:C.token('--upwr-sans'),fontSize:size,
                  align:e.hjust<=.1?'left':e.hjust>=.9?'right':'center',verticalAlign:'middle'}};
            }
            if(e.type==='point') {
              const pt=coord(coords),r=Math.max(2,e.size*1.05);
              if(e.shape==='4') {
                // Krzyżyk (shape = 4 w ggplot): dwie kreski zamiast wypełnienia.
                const line=(a,b)=>({type:'line',shape:{x1:pt[0]+a*r,y1:pt[1]+b*r,x2:pt[0]-a*r,y2:pt[1]-b*r},
                  style:{stroke,lineWidth:Math.max(1.5,r/3),opacity}});
                shape=[line(1,1),line(1,-1)];
              } else {
              shape=e.shape==='18'?{type:'polygon',shape:{points:[[pt[0],pt[1]-r],[pt[0]+r,pt[1]],[pt[0],pt[1]+r],[pt[0]-r,pt[1]]]}}:
                {type:'circle',shape:{cx:pt[0],cy:pt[1],r}};
              shape.style={fill:stroke,opacity};
              }
            } else if(e.type==='line') {
              shape={type:'polyline',shape:{points:coords.map(coord)},style:{stroke,lineWidth:strokeWidth,lineDash:dash,fill:null,opacity}};
            } else if(e.type==='polygon') {
              shape={type:'polygon',shape:{points:coords.map(coord)},style:{fill,stroke:null,opacity}};
            } else if(e.type==='rect') {
              const a=coord(coords.slice(0,2)),b=coord(coords.slice(2));
              shape={type:'rect',shape:{x:Math.min(a[0],b[0]),y:Math.min(a[1],b[1]),width:Math.abs(a[0]-b[0]),height:Math.abs(a[1]-b[1])},
                style:{fill,stroke:e.color==='NA'?null:stroke,lineWidth:strokeWidth,opacity}};
            }
            // ECharts przy zmianie kroku używa ponownie elementów z tym samym indeksem.
            // Napis ma przesunięcie x/y; bez jawnego zera grupa i kształt
            // w jego miejscu dziedziczyłyby to przesunięcie i uciekały z wykresu.
            const still={x:0,y:0,rotation:0,scaleX:1,scaleY:1};
            const children=shape?[].concat(shape):[];
            children.forEach(child=>Object.assign(child,still));
            return {type:'group',...still,clipPath:{type:'rect',shape:{x:left,y:top,width,height}},children};
          }});
      });
      chart.setOption({animation:false,grid:grids,xAxis,yAxis,series,graphic,
        tooltip:{show:false},textStyle:{fontFamily:C.token('--upwr-sans'),fontSize:fs}},true);
      el.setAttribute('aria-label',spec.alt || spec.panels.map(p=>[p.x.name,p.y.name,...p.elements.filter(e=>e.type==='text').map(e=>e.text)].filter(Boolean).join('. ')).join('. '));
    }
    chart=C.chart(el,{}, {renderer:'svg',onResize:draw});
    const observer=new MutationObserver(draw);
    observer.observe(config,{childList:true,subtree:true,attributes:true,attributeFilter:['data-ec-spec']});
    const stepper=root.closest('.lc-stepper');
    if(stepper) observer.observe(stepper,{attributes:true,attributeFilter:['data-lc-chart-large']});
    C.onTheme(root,draw);C.onDispose(root,()=>observer.disconnect());
    // Własna lupa nie zwiększa wysokości ryciny.
    if(!stepper) C.zoomControl(root);
    else {
      const button=stepper.querySelector('[data-lc-chart-zoom]');
      const toggle=()=>{
        const large=stepper.dataset.lcChartLarge!=='true';stepper.dataset.lcChartLarge=String(large);
        button.setAttribute('aria-pressed',String(large));
      };
      // Przycisk należy do steppera, poza korzeniem .lc-ec-plot.
      // C.listen() szuka właściciela przycisku i w tym układzie rzuca błąd.
      if(button){button.addEventListener('click',toggle);C.onDispose(root,()=>button.removeEventListener('click',toggle));}
    }
    draw();
  });
})();
