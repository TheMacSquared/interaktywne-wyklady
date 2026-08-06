(() => {
  "use strict";

  const DATA_URL = "../grafy/kurs-analiza-ryzyka/graf.json";
  const SVG_NS = "http://www.w3.org/2000/svg";
  const MODE_RELATIONS = {
    prerequisites: new Set(["wymaga", "uogolnia"]),
    syllabus: new Set(["omawia", "poprzedza"]),
    arguments: new Set(["odpowiada_na", "wspiera", "kwestionuje"])
  };
  const MODE_DESCRIPTIONS = {
    prerequisites: "Pojęcia bardziej zaawansowane są u góry, fundamenty niżej.",
    syllabus: "Wykłady tworzą kolumny; pod nimi znajdują się omawiane treści.",
    arguments: "Źródła wspierają twierdzenia, a pojęcia i metody odpowiadają na pytania."
  };

  const elements = {
    svg: document.getElementById("graph"),
    stats: document.getElementById("graph-stats"),
    search: document.getElementById("search"),
    lecture: document.getElementById("lecture-filter"),
    types: document.getElementById("type-filters"),
    decisions: document.getElementById("decision-list"),
    description: document.getElementById("view-description"),
    message: document.getElementById("graph-message"),
    detailPlaceholder: document.getElementById("detail-placeholder"),
    detail: document.getElementById("node-detail"),
    detailType: document.getElementById("detail-type"),
    detailTitle: document.getElementById("detail-title"),
    detailState: document.getElementById("detail-state"),
    detailContent: document.getElementById("detail-content"),
    detailRelations: document.getElementById("detail-relations")
  };

  const state = {
    data: null,
    nodesById: new Map(),
    mode: "prerequisites",
    selectedId: null,
    enabledTypes: new Set(),
    scale: 1
  };

  function svgElement(name, attributes = {}) {
    const element = document.createElementNS(SVG_NS, name);
    Object.entries(attributes).forEach(([key, value]) => element.setAttribute(key, value));
    return element;
  }

  function truncate(text, length = 27) {
    return text.length > length ? `${text.slice(0, length - 1)}…` : text;
  }

  function asArray(value) {
    if (Array.isArray(value)) return value;
    if (value === null || value === undefined || value === "") return [];
    return [value];
  }

  function relationTargets(sourceIds, relationType) {
    const result = new Set();
    state.data.edges.forEach(edge => {
      if (edge.typ === relationType && sourceIds.has(edge.source)) result.add(edge.target);
    });
    return result;
  }

  function prerequisiteClosure(initial) {
    const result = new Set(initial);
    let changed = true;
    while (changed) {
      changed = false;
      state.data.edges.forEach(edge => {
        if (edge.typ === "wymaga" && result.has(edge.source) && !result.has(edge.target)) {
          result.add(edge.target);
          changed = true;
        }
      });
    }
    return result;
  }

  function visibleGraph() {
    const relationTypes = MODE_RELATIONS[state.mode];
    const lectureId = elements.lecture.value;
    let allowedIds;

    if (lectureId) {
      const discussed = relationTargets(new Set([lectureId]), "omawia");
      allowedIds = prerequisiteClosure(discussed);
      if (state.mode === "syllabus") allowedIds.add(lectureId);
    } else {
      allowedIds = new Set();
      state.data.edges.forEach(edge => {
        if (relationTypes.has(edge.typ)) {
          allowedIds.add(edge.source);
          allowedIds.add(edge.target);
        }
      });
    }

    const query = elements.search.value.trim().toLocaleLowerCase("pl");
    let nodes = state.data.nodes.filter(node => {
      const matchesType = state.enabledTypes.has(node.typ);
      const matchesScope = allowedIds.has(node.id);
      const matchesQuery = !query || `${node.label} ${node.id} ${node.tresc}`.toLocaleLowerCase("pl").includes(query);
      return matchesType && matchesScope && matchesQuery;
    });

    const nodeIds = new Set(nodes.map(node => node.id));
    const edges = state.data.edges.filter(edge =>
      relationTypes.has(edge.typ) && nodeIds.has(edge.source) && nodeIds.has(edge.target)
    );

    if (query) {
      const connected = new Set();
      edges.forEach(edge => { connected.add(edge.source); connected.add(edge.target); });
      nodes = nodes.filter(node => connected.has(node.id) || nodeIds.has(node.id));
    }
    return { nodes, edges };
  }

  function distribute(items, width, yStart, yGap) {
    const coordinates = new Map();
    const gap = width / (items.length + 1);
    items.forEach((node, index) => coordinates.set(node.id, { x: gap * (index + 1), y: yStart + index * yGap }));
    return coordinates;
  }

  function prerequisiteLayout(nodes, edges) {
    const ranks = new Map(nodes.map(node => [node.id, 0]));
    const nodeIds = new Set(ranks.keys());
    const rankedEdges = edges.filter(edge => nodeIds.has(edge.source) && nodeIds.has(edge.target));
    for (let pass = 0; pass < nodes.length; pass += 1) {
      let changed = false;
      rankedEdges.forEach(edge => {
        const next = Math.min(nodes.length, (ranks.get(edge.source) || 0) + 1);
        if (next > (ranks.get(edge.target) || 0)) {
          ranks.set(edge.target, next);
          changed = true;
        }
      });
      if (!changed) break;
    }

    const groups = new Map();
    nodes.forEach(node => {
      const rank = ranks.get(node.id) || 0;
      if (!groups.has(rank)) groups.set(rank, []);
      groups.get(rank).push(node);
    });
    groups.forEach(group => group.sort((a, b) => a.label.localeCompare(b.label, "pl")));

    const maxGroup = Math.max(1, ...Array.from(groups.values(), group => group.length));
    const width = Math.max(900, maxGroup * 220 + 100);
    const maxRank = Math.max(0, ...groups.keys());
    const height = Math.max(620, maxRank * 125 + 150);
    const coordinates = new Map();
    groups.forEach((group, rank) => {
      const gap = width / (group.length + 1);
      group.forEach((node, index) => coordinates.set(node.id, {
        x: gap * (index + 1),
        y: 70 + rank * 125
      }));
    });
    return { coordinates, width, height };
  }

  function syllabusLayout(nodes, edges) {
    const lectures = nodes.filter(node => node.typ === "wyklad").sort((a, b) => a.id.localeCompare(b.id));
    const content = nodes.filter(node => node.typ !== "wyklad");
    const columns = Math.max(1, lectures.length);
    const width = Math.max(950, columns * 230 + 80);
    const assignments = new Map(lectures.map(lecture => [lecture.id, []]));
    const assigned = new Set();

    edges.filter(edge => edge.typ === "omawia").forEach(edge => {
      if (assignments.has(edge.source) && !assigned.has(edge.target)) {
        const node = state.nodesById.get(edge.target);
        if (node && content.some(item => item.id === node.id)) {
          assignments.get(edge.source).push(node);
          assigned.add(edge.target);
        }
      }
    });
    const unassigned = content.filter(node => !assigned.has(node.id));
    if (lectures.length && unassigned.length) assignments.get(lectures[0].id).push(...unassigned);

    const coordinates = new Map();
    const columnGap = width / (columns + 1);
    let maxItems = 1;
    lectures.forEach((lecture, index) => {
      const x = columnGap * (index + 1);
      coordinates.set(lecture.id, { x, y: 70 });
      const items = assignments.get(lecture.id).sort((a, b) => a.label.localeCompare(b.label, "pl"));
      maxItems = Math.max(maxItems, items.length);
      items.forEach((node, itemIndex) => coordinates.set(node.id, { x, y: 175 + itemIndex * 86 }));
    });
    if (!lectures.length) {
      const fallback = distribute(content, width, 80, 80);
      fallback.forEach((value, key) => coordinates.set(key, value));
      maxItems = content.length;
    }
    return { coordinates, width, height: Math.max(620, 240 + maxItems * 86) };
  }

  function argumentLayout(nodes) {
    const columns = [
      nodes.filter(node => node.typ === "zrodlo"),
      nodes.filter(node => !["zrodlo", "pytanie"].includes(node.typ)),
      nodes.filter(node => node.typ === "pytanie")
    ];
    columns.forEach(column => column.sort((a, b) => a.label.localeCompare(b.label, "pl")));
    const width = 1180;
    const xs = [150, 590, 1030];
    const coordinates = new Map();
    let maxItems = 1;
    columns.forEach((column, columnIndex) => {
      maxItems = Math.max(maxItems, column.length);
      const gap = Math.max(82, 570 / Math.max(1, column.length));
      column.forEach((node, index) => coordinates.set(node.id, { x: xs[columnIndex], y: 75 + index * gap }));
    });
    return { coordinates, width, height: Math.max(620, 150 + maxItems * 88) };
  }

  function layoutGraph(nodes, edges) {
    if (state.mode === "syllabus") return syllabusLayout(nodes, edges);
    if (state.mode === "arguments") return argumentLayout(nodes);
    return prerequisiteLayout(nodes, edges);
  }

  function edgePath(source, target) {
    const vertical = target.y - source.y;
    if (Math.abs(vertical) > 45) {
      const startY = source.y + Math.sign(vertical) * 31;
      const endY = target.y - Math.sign(vertical) * 31;
      const middleY = (startY + endY) / 2;
      return `M ${source.x} ${startY} C ${source.x} ${middleY}, ${target.x} ${middleY}, ${target.x} ${endY}`;
    }
    const direction = target.x >= source.x ? 1 : -1;
    return `M ${source.x + direction * 98} ${source.y} L ${target.x - direction * 98} ${target.y}`;
  }

  function renderGraph() {
    const { nodes, edges } = visibleGraph();
    elements.svg.replaceChildren();
    elements.message.hidden = nodes.length > 0;
    elements.message.textContent = nodes.length ? "" : "Brak węzłów spełniających wybrane warunki.";
    if (!nodes.length) return;

    const layout = layoutGraph(nodes, edges);
    const scaledWidth = layout.width / state.scale;
    const scaledHeight = layout.height / state.scale;
    elements.svg.setAttribute("viewBox", `0 0 ${scaledWidth} ${scaledHeight}`);

    const defs = svgElement("defs");
    const marker = svgElement("marker", {
      id: "arrowhead", viewBox: "0 0 10 10", refX: "9", refY: "5",
      markerWidth: "6", markerHeight: "6", orient: "auto-start-reverse"
    });
    marker.appendChild(svgElement("path", { d: "M 0 0 L 10 5 L 0 10 z", fill: "context-stroke" }));
    defs.appendChild(marker);
    elements.svg.appendChild(defs);

    const edgeLayer = svgElement("g", { "aria-hidden": "true" });
    edges.forEach(edge => {
      const source = layout.coordinates.get(edge.source);
      const target = layout.coordinates.get(edge.target);
      if (!source || !target) return;
      const path = svgElement("path", {
        d: edgePath(source, target),
        class: "graph-edge",
        "data-type": edge.typ,
        "marker-end": "url(#arrowhead)"
      });
      const title = svgElement("title");
      title.textContent = `${edge.source} —${edge.typ}→ ${edge.target}`;
      path.appendChild(title);
      edgeLayer.appendChild(path);
    });
    elements.svg.appendChild(edgeLayer);

    const nodeLayer = svgElement("g");
    nodes.forEach(node => {
      const position = layout.coordinates.get(node.id);
      if (!position) return;
      const status = node.stan && node.stan.zrozumienie ? node.stan.zrozumienie : "";
      const group = svgElement("g", {
        class: `graph-node${state.selectedId === node.id ? " is-selected" : ""}`,
        transform: `translate(${position.x - 98} ${position.y - 31})`,
        "data-type": node.typ,
        "data-status": status,
        tabindex: "0",
        role: "button",
        "aria-label": `${node.label}, typ ${node.typ}${status ? `, stan ${status}` : ""}`
      });
      group.appendChild(svgElement("rect", { width: "196", height: "62" }));

      const label = svgElement("text", { x: "12", y: "24", class: "node-label" });
      label.textContent = truncate(node.label);
      group.appendChild(label);
      const meta = svgElement("text", { x: "12", y: "45", class: "node-meta" });
      meta.textContent = status ? `${node.typ} · ${status}` : node.typ;
      group.appendChild(meta);

      const select = () => selectNode(node.id);
      group.addEventListener("click", select);
      group.addEventListener("keydown", event => {
        if (event.key === "Enter" || event.key === " ") {
          event.preventDefault();
          select();
        }
      });
      nodeLayer.appendChild(group);
    });
    elements.svg.appendChild(nodeLayer);
  }

  function cleanContent(node) {
    return node.tresc.trimStart().replace(/^#\s+.*(?:\r?\n)+/, "").trim();
  }

  function selectNode(nodeId) {
    const node = state.nodesById.get(nodeId);
    if (!node) return;
    state.selectedId = nodeId;
    elements.detailPlaceholder.hidden = true;
    elements.detail.hidden = false;
    elements.detailType.textContent = node.typ;
    elements.detailTitle.textContent = node.label;
    elements.detailContent.textContent = cleanContent(node);
    elements.detailState.replaceChildren();
    Object.entries(node.stan || {}).forEach(([key, value]) => {
      const item = document.createElement("span");
      item.textContent = `${key}: ${value}`;
      elements.detailState.appendChild(item);
    });

    elements.detailRelations.replaceChildren();
    const relations = state.data.edges.filter(edge => edge.source === nodeId || edge.target === nodeId);
    if (!relations.length) {
      const item = document.createElement("li");
      item.textContent = "Brak relacji";
      elements.detailRelations.appendChild(item);
    } else {
      relations.forEach(edge => {
        const outgoing = edge.source === nodeId;
        const relatedId = outgoing ? edge.target : edge.source;
        const item = document.createElement("li");
        item.append(`${outgoing ? edge.typ : `← ${edge.typ}`} `);
        const button = document.createElement("button");
        button.type = "button";
        button.className = "relation-target";
        button.textContent = state.nodesById.get(relatedId)?.label || relatedId;
        button.addEventListener("click", () => selectNode(relatedId));
        item.appendChild(button);
        elements.detailRelations.appendChild(item);
      });
    }
    renderGraph();
  }

  function renderTypeFilters() {
    const types = [...new Set(state.data.nodes.map(node => node.typ))].sort((a, b) => a.localeCompare(b, "pl"));
    elements.types.replaceChildren();
    types.forEach(type => {
      state.enabledTypes.add(type);
      const label = document.createElement("label");
      label.className = "type-filter";
      const checkbox = document.createElement("input");
      checkbox.type = "checkbox";
      checkbox.checked = true;
      checkbox.value = type;
      checkbox.addEventListener("change", () => {
        if (checkbox.checked) state.enabledTypes.add(type); else state.enabledTypes.delete(type);
        renderGraph();
      });
      const dot = document.createElement("span");
      dot.className = "type-dot";
      dot.dataset.type = type;
      label.append(checkbox, dot, document.createTextNode(type));
      elements.types.appendChild(label);
    });
  }

  function renderLectures() {
    state.data.nodes
      .filter(node => node.typ === "wyklad")
      .sort((a, b) => a.id.localeCompare(b.id))
      .forEach(node => {
        const option = document.createElement("option");
        option.value = node.id;
        option.textContent = node.label;
        elements.lecture.appendChild(option);
      });
  }

  function renderDecisions() {
    const sections = [
      ["Pytania bez odpowiedzi", asArray(state.data.raport.pytania_bez_odpowiedzi)],
      ["Twierdzenia bez źródeł", asArray(state.data.raport.twierdzenia_bez_zrodel)],
      ["Nieomawiane", asArray(state.data.raport.nieomawiane)],
      ["Odłączone", asArray(state.data.raport.wezly_odlaczone)]
    ].filter(([, ids]) => ids.length);
    elements.decisions.replaceChildren();
    if (!sections.length) {
      const empty = document.createElement("p");
      empty.className = "decision-empty";
      empty.textContent = "Brak otwartych decyzji.";
      elements.decisions.appendChild(empty);
      return;
    }
    sections.forEach(([label, ids]) => {
      const group = document.createElement("div");
      group.className = "decision-group";
      const heading = document.createElement("p");
      heading.textContent = label;
      group.appendChild(heading);
      ids.forEach(id => {
        const button = document.createElement("button");
        button.type = "button";
        button.className = "decision-link";
        button.textContent = state.nodesById.get(id)?.label || id;
        button.addEventListener("click", () => selectNode(id));
        group.appendChild(button);
      });
      elements.decisions.appendChild(group);
    });
  }

  function bindControls() {
    elements.search.addEventListener("input", renderGraph);
    elements.lecture.addEventListener("change", () => {
      state.selectedId = null;
      renderGraph();
    });
    document.querySelectorAll("[data-mode]").forEach(button => {
      button.addEventListener("click", () => {
        state.mode = button.dataset.mode;
        state.scale = 1;
        document.querySelectorAll("[data-mode]").forEach(item =>
          item.setAttribute("aria-pressed", String(item === button))
        );
        elements.description.textContent = MODE_DESCRIPTIONS[state.mode];
        renderGraph();
      });
    });
    document.getElementById("zoom-in").addEventListener("click", () => {
      state.scale = Math.min(1.8, state.scale + 0.15);
      renderGraph();
    });
    document.getElementById("zoom-out").addEventListener("click", () => {
      state.scale = Math.max(0.55, state.scale - 0.15);
      renderGraph();
    });
    document.getElementById("zoom-reset").addEventListener("click", () => {
      state.scale = 1;
      renderGraph();
    });
  }

  async function initialize() {
    try {
      const response = await fetch(DATA_URL);
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      state.data = await response.json();
      state.nodesById = new Map(state.data.nodes.map(node => [node.id, node]));
      elements.stats.textContent = `${state.data.meta.wezlow} węzłów · ${state.data.meta.krawedzi} relacji`;
      renderTypeFilters();
      renderLectures();
      renderDecisions();
      bindControls();
      renderGraph();
    } catch (error) {
      elements.stats.textContent = "Nie udało się wczytać grafu";
      elements.message.hidden = false;
      elements.message.textContent = "Uruchom przeglądarkę przez lokalny serwer zgodnie z README. Nie otwieraj index.html bezpośrednio jako pliku.";
      console.error(error);
    }
  }

  initialize();
})();
