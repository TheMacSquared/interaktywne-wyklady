// Scena „Zmierz grupkę” w stylu S1 (ilustracja redakcyjna) — standard wizualny scen wykładu 04.
// SVG, bez bitmap i dodatkowych bibliotek.
(function () {
  "use strict";
  var NS="http://www.w3.org/2000/svg", roots=new Map();
  var ink="var(--upwr-ink)", soft="var(--upwr-ink-subtle)", paper="var(--upwr-surface)", rule="var(--upwr-rule)";
  var colors=["var(--upwr-cat-niebo)","var(--upwr-cat-szalwia)","var(--upwr-cat-bursztyn)","var(--upwr-cat-indygo)","var(--upwr-cat-terakota)"];
  var skins=["#e3b896","#ad795b","#f1d2b4","#875c47","#c9926c"];
  function e(tag,attrs,parent,text) {
    var node=document.createElementNS(NS,tag);
    Object.keys(attrs||{}).forEach(function(k){node.setAttribute(k,attrs[k]);});
    if(text!==undefined)node.textContent=text;
    if(parent)parent.appendChild(node);return node;
  }
  function path(g,d,fill,stroke,width) {return e("path",{d:d,fill:fill||"none",stroke:stroke||"none","stroke-width":width||1.5},g);}
  function line(g,x1,y1,x2,y2,color,width,dash) {return e("line",{x1:x1,y1:y1,x2:x2,y2:y2,stroke:color||ink,"stroke-width":width||1.5,"stroke-dasharray":dash||""},g);}
  function rect(g,x,y,w,h,fill,rx,stroke) {return e("rect",{x:x,y:y,width:w,height:h,rx:rx||0,fill:fill,stroke:stroke||"none"},g);}
  function ellipse(g,x,y,rx,ry,fill) {return e("ellipse",{cx:x,cy:y,rx:rx,ry:ry,fill:fill},g);}
  function text(g,x,y,value,cls,size,anchor) {return e("text",{x:x,y:y,class:cls||"","font-size":size||14,"text-anchor":anchor||"start"},g,value);}
  function X(v){return 46+(v-140)*748/60;}

  // Shoes, legs, clothing, arms, neck, face and hair are drawn separately.
  function editorialPerson(parent,x,base,height,index,pose) {
    var g=e("g",{transform:"translate("+x+","+base+") scale("+(height/150)+")"},parent);
    var shirt=colors[index%5],skin=skins[index%5],hair=index%3===0?"#76523d":ink;
    ellipse(g,0,2,21,5,rule).setAttribute("opacity",".48");
    if(index%3===0){path(g,"M-17-111 Q-33-112-31-91 L-27-65 Q-20-58-15-68Z",colors[(index+2)%5]);path(g,"M-22-105 Q-14-100-12-85", "none",ink,1.5);}
    path(g,"M-16-63 L-13-8 Q-9-5-5-9 L-1-52 L5-8 Q10-5 14-9 L18-65Z",ink);
    path(g,"M-14-10 L-19-6 Q-22 0-16 0 L-5 0 L-5-8Z",soft);
    path(g,"M5-9 L5 0 L19 0 Q24-3 16-7 L14-10Z",soft);
    path(g,"M-11-119 Q-25-117-24-100 L-21-64 Q0-55 21-64 L23-101 Q24-117 11-120Z",shirt);
    path(g,"M-11-119 L0-108 L11-119", "none",paper,2);
    path(g,"M-19-108 Q-29-99-29-74 L-25-66 Q-20-63-20-70 L-16-98",shirt);
    path(g,pose==="measure"?"M18-109 Q27-101 27-75 L23-66 Q18-65 18-74 L15-98":"M18-109 Q28-96 27-82 L22-77 Q17-80 19-86 L14-100",shirt);
    ellipse(g,-25,-67,4,6,skin);ellipse(g,23,pose==="measure"?-67:-78,4,6,skin);
    path(g,"M0-104 L0-69 M6-92 L15-92", "none",paper,1.1).setAttribute("opacity",".45");
    rect(g,-5,-126,10,11,skin,3);
    ellipse(g,0,-135,12.5,15,skin);ellipse(g,12,-134,2,4,skin);
    if(index%4===0)path(g,"M-13-135 Q-18-154-1-153 Q17-154 14-133 L8-141 L2-143 L-6-138Z",hair);
    else if(index%4===1)path(g,"M-14-133 Q-18-152 0-153 Q18-152 13-131 L9-143 Q-4-137-10-145Z",hair);
    else if(index%4===2){path(g,"M-13-132 Q-17-155 1-154 Q20-150 13-128 L10-140 Q0-146-11-139Z",hair);ellipse(g,-13,-132,6,13,hair);}
    else path(g,"M-13-139 Q-11-155 4-153 Q16-150 14-140 Q-2-146-13-139Z",hair);
    line(g,2,-134,4,-134,ink,1.5);line(g,9,-134,10,-134,ink,1.5);
    path(g,"M6-132 L7-128 L4-128 M2-124 Q6-122 9-125", "none",hair,.8);
    if(index%5===1){e("circle",{cx:1,cy:-134,r:4,fill:"none",stroke:ink,"stroke-width":1},g);e("circle",{cx:10,cy:-134,r:4,fill:"none",stroke:ink,"stroke-width":1},g);}
    if(index%4===2)path(g,"M-14-116 Q-2-105 14-117 L8-106 L-7-105Z",colors[(index+3)%5]);
    return g;
  }
  function room(g) {
      rect(g,24,28,792,252,paper,18);
      rect(g,24,28,792,206,rule,18).setAttribute("opacity",".17");
      path(g,"M24 234 L816 234 L816 280 L24 280Z",colors[1]).setAttribute("opacity",".09");
      line(g,24,234,816,234,rule,1.2);
      rect(g,51,74,99,178,"#b68f68",5);rect(g,60,82,81,166,"#c5a17a",3);
      rect(g,70,93,60,47,paper,4);line(g,73,96,127,137,rule,2);line(g,96,95,126,119,rule,1);
      rect(g,128,168,4,24,ink,2);ellipse(g,129,175,5,3,soft);
      rect(g,53,43,95,23,ink,4);text(g,100,59,"SALA 21","",12,"middle").style.fill=paper;
      rect(g,657,54,119,72,paper,5,rule);rect(g,670,67,29,41,colors[0],3).setAttribute("opacity",".25");
      line(g,710,74,762,74,soft,2);line(g,710,85,750,85,rule,3);line(g,710,98,758,98,rule,3);
      // A few quiet, paper-like marks; no texture over the statistical plot.
      for(var i=0;i<30;i++)ellipse(g,36+(i*113)%765,38+(i*47)%182,.55,.55,rule).setAttribute("opacity",".4");
  }
  function station(g,head) {
    var x=278;
    rect(g,x-3,77,8,181,rule,2,ink);line(g,x-10,258,x+32,258,ink,2);
    for(var n=0;n<=18;n++)line(g,x-2,253-n*9,x+(n%5===0?10:5),253-n*9,ink,n%5===0?1.2:.6);
    line(g,x-5,head,x+40,head,colors[4],3);
    rect(g,x+32,head-3,15,6,colors[4],2);
    // Observer's clipboard makes the measurement task legible.
    path(g,"M231 155 L248 151 L258 183 L238 188Z","#d7b88e",ink,1.2);
    rect(g,235,151,10,5,soft,1);line(g,239,164,248,161,soft,1);line(g,242,173,251,170,soft,1);
  }
  // Wspólne elementy ilustracji dla sceny z siatkami w rozdziale 2.
  window.lcEditorialScene = {room: room, person: editorialPerson, station: station};

  function init(root) {
    var art=root.querySelector(".vs-art"),read=root.querySelector(".vs-read");
    var abort=new AbortController(),frame=null,seed=431031;
    var st={sample:null,means:[],progress:0,busy:false,truth:false,fly:0};
    function random(){seed^=seed<<13;seed^=seed>>>17;seed^=seed<<5;return ((seed>>>0)+.5)/4294967296;}
    function sample(){var values=[];for(var i=0;i<25;i++)values.push(170+10*Math.sqrt(-2*Math.log(random()))*Math.cos(2*Math.PI*random()));return values;}
    function avg(xs){return xs.reduce(function(a,b){return a+b;},0)/xs.length;}
    function sd(xs){var m=avg(xs);return Math.sqrt(xs.reduce(function(a,x){return a+(x-m)*(x-m);},0)/(xs.length-1));}
    var person=editorialPerson;
    function draw() {
      art.textContent="";
      var svg=e("svg",{viewBox:"0 0 840 454","aria-hidden":"true"},art);
      room(svg);
      var active=Math.max(0,Math.min(24,st.progress-1));
      var h=st.sample?st.sample[active]:170;
      // Body proportions are preserved while measured heights change.
      var hp=h*.86,base=258;
      var queue=e("g",{},svg);
      for(var j=5;j>=0;j--) {
        var id=(active+j+1)%25, height=st.sample?st.sample[id]:[168,178,163,174,181,169][j];
        var x=398+j*67,y=base;
        person(queue,x,y,height*.80,id+2,"wait");
      }
      person(svg,199,base,158,8,"observer");
      person(svg,307,base,hp,active,"measure");
      station(svg,base-hp);
      if(st.progress>0)text(svg,339,base-hp-8,h.toFixed(1)+" cm","vs-number",15);
      // Make the record-taking gesture visible rather than adding another figure.
      line(svg,218,164,237,170,skins[3],6);
      text(svg,46,308,"Pomiary osób","vs-note",13);
      line(svg,46,348,794,348,soft,1.1);
      for(var v=140;v<=200;v+=10){line(svg,X(v),348,X(v),354,soft,1);text(svg,X(v),372,String(v),"vs-number vs-note",13,"middle");}
      text(svg,794,393,"wzrost (cm)","vs-note",12,"end");
      if(st.sample)for(var k=0;k<st.progress;k++) {
        var value=st.sample[k],offset=9+((k*7)%3)*7;
        ellipse(svg,X(value),348-offset,3.8,3.8,colors[0]);
        if(k===st.progress-1&&st.busy)line(svg,307,272,X(value),333,rule,1,"2 5");
      }
      var current=st.sample&&st.progress===25?avg(st.sample):null;
      if(current!==null) {
        var xx=X(current);line(svg,xx,318,xx,352,"var(--upwr-accent)",3);
        e("path",{d:"M"+xx+" 314 l-5-8 h10Z",fill:"var(--upwr-accent)"},svg);
      }
      text(svg,46,425,"Średnie grupek","vs-note",13);
      line(svg,187,430,794,430,rule,1);
      var bins={},max=0;
      st.means.forEach(function(m){var b=Math.round(m*2);bins[b]=(bins[b]||0)+1;max=Math.max(max,bins[b]);});
      Object.keys(bins).forEach(function(b){var count=bins[b],x=X(Number(b)/2);var tall=max>7?29*count/max:count*4;rect(svg,x-2,430-tall,4,tall,colors[2],1);});
      if(current!==null&&!st.busy)ellipse(svg,X(current),426,4,4,colors[2]);
      if(current!==null&&st.fly>0)ellipse(svg,X(current),328+st.fly*97,5,5,"var(--upwr-accent)");
      if(st.truth){line(svg,X(170),301,X(170),433,ink,1.6,"5 5");}
      var n=st.means.length;
      if(st.busy)read.textContent="Zmierzono "+st.progress+" / 25 osób"+(current!==null?" · x̄ = "+current.toFixed(1)+" cm":"");
      else read.textContent=n?"Grupka "+n+" · x̄ = "+avg(st.sample).toFixed(1)+" cm · średnich na stosie: "+n+(n>1?" · SD(x̄) = "+sd(st.means).toFixed(2)+" cm":"")+(st.truth?" · μ = 170 cm":""):"Grupka 25 osób czeka na pomiar.";
      art.setAttribute("aria-label",read.textContent);
      root.querySelectorAll("[data-vs-action]").forEach(function(b){b.disabled=st.busy&&b.dataset.vsAction!=="reset";});
    }
    function finish() {st.means.push(avg(st.sample));st.progress=25;st.busy=false;st.fly=0;frame=null;draw();}
    function measure() {
      st.sample=sample();st.progress=0;st.fly=0;st.busy=true;
      if(window.matchMedia("(prefers-reduced-motion: reduce)").matches){finish();return;}
      var start=null,last=-1;
      function tick(time) {
        if(!root.isConnected)return;
        if(start===null)start=time;
        var u=Math.min(1,(time-start)/2400),p=Math.min(25,Math.floor(u/.8*25));
        st.fly=u>.8?(u-.8)/.2:0;
        if(p!==last||u>.8){st.progress=p;last=p;draw();}
        if(u<1)frame=requestAnimationFrame(tick);else finish();
      }
      frame=requestAnimationFrame(tick);draw();
    }
    function reset(){if(frame!==null)cancelAnimationFrame(frame);frame=null;seed=431031;st={sample:null,means:[],progress:0,busy:false,truth:false,fly:0};root.querySelector('[data-vs-action="truth"]').textContent="Pokaż μ";root.querySelector('[data-vs-action="truth"]').setAttribute("aria-pressed","false");draw();}
    root.addEventListener("click",function(event){
      var button=event.target.closest("[data-vs-action]");if(!button)return;
      var key=button.dataset.vsAction;
      if(key==="reset"){reset();return;}
      if(st.busy)return;
      if(key==="truth"){st.truth=!st.truth;button.textContent=st.truth?"Ukryj μ":"Pokaż μ";button.setAttribute("aria-pressed",String(st.truth));draw();return;}
      if(key==="go"){measure();return;}
      var count=Number(key.slice(1));
      for(var i=0;i<count;i++){st.sample=sample();st.means.push(avg(st.sample));}
      st.progress=25;draw();
    },{signal:abort.signal});
    root.dataset.ready="true";draw();
    return function(){abort.abort();if(frame!==null)cancelAnimationFrame(frame);};
  }
  function scan() {
    roots.forEach(function(dispose,root){if(!root.isConnected){dispose();roots.delete(root);}});
    document.querySelectorAll(".vs-widget").forEach(function(root){if(!roots.has(root))roots.set(root,init(root));});
  }
  new MutationObserver(scan).observe(document.documentElement,{childList:true,subtree:true});
  document.addEventListener("DOMContentLoaded",scan);scan();
})();
