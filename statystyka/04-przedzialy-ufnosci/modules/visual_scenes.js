// Trzy graficzne języki jednej sceny. SVG, bez bitmap i dodatkowych bibliotek.
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
  function isoPerson(parent,x,base,height,index,pose) {
    var g=e("g",{transform:"translate("+x+","+base+") scale("+(height/150)+")"},parent);
    var shirt=colors[index%5],skin=skins[index%5];
    ellipse(g,6,5,25,8,ink).setAttribute("opacity",".1");
    // Extruded shoes and trouser legs give the small model a volume.
    path(g,"M-12-65 L-11-9 L-1-4 L3-58 L10-7 L20-3 L20-62Z",ink);
    path(g,"M-11-9 L-21-3 L-11 3 L0-3 L-1-8Z",soft);
    path(g,"M11-8 L6-1 L18 5 L28 0 L20-5Z",soft);
    path(g,"M-18-114 L-25-102 L-22-68 L-11-61 L17-63 L23-103 L8-118Z",shirt);
    path(g,"M17-109 L23-103 L20-64 L11-60 L13-103Z",ink).setAttribute("opacity",".19");
    path(g,"M-18-113 L-9-120 L8-118 L17-109 L5-112Z",paper).setAttribute("opacity",".22");
    path(g,"M-22-105 L-28-76 L-21-70 L-14-100 M19-104 L28-79 L22-72 L13-99",shirt,shirt,5);
    ellipse(g,-24,-73,4,6,skin);ellipse(g,25,-75,4,6,skin);
    if(index%3===0){path(g,"M-19-110 L-32-104 L-30-77 L-19-71 L-13-77 L-14-103Z",colors[(index+2)%5]);path(g,"M-31-104 L-20-98 L-13-103", "none",paper,1.2);}
    rect(g,-3,-126,9,12,skin,2);
    path(g,"M-11-145 L-1-151 L12-145 L14-130 L3-122 L-9-128Z",skin);
    path(g,"M-11-145 L-1-153 L13-147 L14-138 L4-141 L-10-137Z",ink);
    path(g,"M-9-128 L3-122 L3-140 L-10-137Z",ink).setAttribute("opacity",".1");
    line(g,6,-136,8,-136,ink,1.7);path(g,"M9-132 L11-129 L7-129 M5-125 L9-126", "none",ink,1);
    if(index%4===2)ellipse(g,-13,-140,6,8,ink);
    return g;
  }
  function markerPerson(parent,x,base,height,index,pose) {
    var g=e("g",{transform:"translate("+x+","+base+") scale("+(height/150)+")"},parent);
    var wash=colors[index%5];
    path(g,"M-18-115 Q-27-104-23-66 Q0-60 21-65 L24-102 Q20-114 9-119Z",wash).setAttribute("opacity",".13");
    path(g,"M-18-113 Q-26-104-21-64 L21-65 Q26-104 12-117 M-19-112 L-9-121 M10-118 L0-110 L-9-120", "none",ink,2.4);
    path(g,"M-16-63 L-14-8 L-4-7 L0-49 L6-8 L16-8 L18-64 M-14-7 L-21-1 L-3 0 M6-7 L7 1 L24 0 L16-7", "none",ink,2.3);
    path(g,"M-20-108 L-29-78 L-23-68 M19-108 Q30-92 26-73 L21-67 M-5-126 L-5-119 M6-126 L7-118", "none",ink,2);
    path(g,"M-10-125 Q-20-132-12-146 Q-2-157 11-147 Q20-133 9-125 Q-1-121-10-125Z", "none",ink,2.1);
    path(g,"M-13-139 Q-8-154 4-151 Q16-150 15-140 M-12-142 Q-4-149 4-145", "none",ink,3);
    line(g,1,-136,3,-136,ink,2);line(g,10,-136,11,-136,ink,2);
    path(g,"M6-132 L8-129 L4-129 M2-125 Q6-123 10-126", "none",ink,1.2);
    for(var i=0;i<5;i++)line(g,-16+i*6,-96,-20+i*6,-82,wash,1.7);
    if(index%3===0)path(g,"M-22-108 Q-38-108-34-84 L-25-74 M-29-101 L-28-87", "none",ink,1.7);
    if(index%4===2){path(g,"M-15-137 Q-22-127-15-116 M13-143 Q24-129 16-115", "none",ink,3);}
    return g;
  }
  function room(g,style) {
    if(style==="isometric") {
      path(g,"M28 64 L192 24 L812 87 L812 261 L668 302 L28 244Z",paper);
      path(g,"M28 64 L192 24 L192 205 L28 244Z",rule).setAttribute("opacity",".32");
      path(g,"M192 24 L812 87 L812 261 L192 205Z",rule).setAttribute("opacity",".16");
      path(g,"M28 244 L192 205 L812 261 L668 302Z",colors[1]).setAttribute("opacity",".12");
      [[80,253,240,210],[220,268,380,223],[360,282,520,236],[510,294,660,248]].forEach(function(a){line(g,a[0],a[1],a[2],a[3],rule,1);});
      line(g,88,237,729,289,rule,1);line(g,133,224,774,275,rule,1);
      path(g,"M61 94 L145 74 L145 217 L61 237Z","#aa815a",ink,1.2);
      path(g,"M73 106 L132 92 L132 138 L73 151Z",paper,ink,1).setAttribute("opacity",".65");
      path(g,"M60 92 L145 72 L154 77 L71 98Z","#d4b28a");line(g,132,165,140,163,ink,3);
      path(g,"M65 65 L143 46 L143 67 L65 86Z",ink);text(g,79,70,"SALA 21","",12).style.fill=paper;
      path(g,"M623 73 L757 87 L757 132 L623 118Z",paper,rule,1);
      line(g,640,90,731,100,soft,1);line(g,640,100,705,107,rule,2);
    } else if(style==="marker") {
      path(g,"M22 277 L23 42 Q202 36 409 42 L816 39 L815 278", "none",rule,1.4);
      path(g,"M42 254 Q383 258 801 253 M44 257 Q412 255 800 257", "none",soft,1.2);
      path(g,"M54 250 L57 77 L138 79 L139 252 M61 84 L131 86 L130 245", "none",ink,2);
      path(g,"M67 97 L120 99 L120 139 L67 137Z", "none",ink,1.4);
      for(var a=0;a<5;a++)line(g,70+a*10,134,91+a*7,103,rule,1);
      ellipse(g,125,174,3,3,ink);text(g,60,63,"SALA 21","",15);
      path(g,"M652 68 L762 71 L759 126 L651 122Z", "none",rule,1.5);
      path(g,"M665 86 L747 89 M666 99 L721 100 M664 112 L735 114", "none",soft,1.3);
      for(var t=0;t<7;t++)line(g,568+t*25,263,581+t*25,268,rule,1);
    } else {
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
  }
  function station(g,style,head) {
    var iso=style==="isometric",marker=style==="marker",x=278;
    if(iso){path(g,"M258 258 L283 250 L321 257 L294 266Z",ink).setAttribute("opacity",".3");path(g,"M273 76 L284 73 L284 252 L273 255Z",soft);path(g,"M284 73 L291 78 L291 254 L284 252Z",ink);}
    else {rect(g,x-3,77,8,181,marker?"none":rule,2,ink);line(g,x-10,258,x+32,258,ink,2);}
    for(var n=0;n<=18;n++)line(g,x-2,253-n*9,x+(n%5===0?10:5),253-n*9,ink,n%5===0?1.2:.6);
    line(g,x-5,head,x+40,head,marker?ink:colors[4],3);
    if(!marker){rect(g,x+32,head-3,15,6,colors[4],2);}
    // Observer's clipboard makes the measurement task legible.
    path(g,"M231 155 L248 151 L258 183 L238 188Z",marker?"none":"#d7b88e",ink,1.2);
    rect(g,235,151,10,5,soft,1);line(g,239,164,248,161,soft,1);line(g,242,173,251,170,soft,1);
  }
  function init(root) {
    var style=root.dataset.style,art=root.querySelector(".vs-art"),read=root.querySelector(".vs-read");
    var abort=new AbortController(),frame=null,seed=431031;
    var st={sample:null,means:[],progress:0,busy:false,truth:false,fly:0};
    function random(){seed^=seed<<13;seed^=seed>>>17;seed^=seed<<5;return ((seed>>>0)+.5)/4294967296;}
    function sample(){var values=[];for(var i=0;i<25;i++)values.push(170+10*Math.sqrt(-2*Math.log(random()))*Math.cos(2*Math.PI*random()));return values;}
    function avg(xs){return xs.reduce(function(a,b){return a+b;},0)/xs.length;}
    var person=style==="marker"?markerPerson:style==="isometric"?isoPerson:editorialPerson;
    function draw() {
      art.textContent="";
      var svg=e("svg",{viewBox:"0 0 840 454","aria-hidden":"true"},art);
      room(svg,style);
      var active=Math.max(0,Math.min(24,st.progress-1));
      var h=st.sample?st.sample[active]:170;
      // Body proportions are preserved while measured heights change.
      var hp=h*.86,base=258;
      var queue=e("g",{},svg);
      for(var j=5;j>=0;j--) {
        var id=(active+j+1)%25, height=st.sample?st.sample[id]:[168,178,163,174,181,169][j];
        var x=398+j*67,y=base+(style==="isometric"?j*3:0);
        person(queue,x,y,height*.80,id+2,"wait");
      }
      person(svg,199,base,158,8,"observer");
      person(svg,307,base,hp,active,"measure");
      station(svg,style,base-hp);
      if(st.progress>0)text(svg,339,base-hp-8,h.toFixed(1)+" cm","vs-number",15);
      // Make the record-taking gesture visible rather than adding another figure.
      line(svg,218,164,237,170,style==="marker"?ink:skins[3],style==="marker"?2:6);
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
      else read.textContent=n?"Grupka "+n+" · x̄ = "+avg(st.sample).toFixed(1)+" cm · średnich w historii: "+n+(st.truth?" · μ = 170 cm":""):"Grupka 25 osób czeka na pomiar.";
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
