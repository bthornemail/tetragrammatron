// web/demo/canvm.js
// Loads canvm.wasm, runs a CANB program, renders Fano highlight SVG.

const FANO_POINTS = [
  { id: 0, x: 160, y: 40 },
  { id: 1, x: 60,  y: 90 },
  { id: 2, x: 260, y: 90 },
  { id: 3, x: 80,  y: 220 },
  { id: 4, x: 240, y: 220 },
  { id: 5, x: 160, y: 260 },
  { id: 6, x: 160, y: 140 },
];

const FANO_LINES = [
  [0,1,2],
  [0,3,4],
  [0,5,6],
  [1,3,6],
  [1,4,5],
  [2,3,5],
  [2,4,6],
];

function el(tag, attrs = {}) {
  const n = document.createElementNS("http://www.w3.org/2000/svg", tag);
  for (const [k,v] of Object.entries(attrs)) n.setAttribute(k, String(v));
  return n;
}

function pointById(id) { return FANO_POINTS.find(p => p.id === id); }

function renderFano(svg, highlightLine = null) {
  while (svg.firstChild) svg.removeChild(svg.firstChild);

  for (let i = 0; i < FANO_LINES.length; i++) {
    const [a,b,c] = FANO_LINES[i];
    const pa = pointById(a), pb = pointById(b), pc = pointById(c);
    const isHL = (highlightLine && highlightLine.index === i);
    const path = el("path", {
      d: `M ${pa.x} ${pa.y} L ${pb.x} ${pb.y} L ${pc.x} ${pc.y} Z`,
      fill: "none",
      stroke: isHL ? "red" : "#666",
      "stroke-width": isHL ? 3 : 1.5,
      "stroke-linejoin": "round",
      opacity: isHL ? 1.0 : 0.35
    });
    svg.appendChild(path);
  }

  for (const p of FANO_POINTS) {
    const isHP = (highlightLine && highlightLine.points.includes(p.id));
    svg.appendChild(el("circle", {
      cx: p.x, cy: p.y, r: isHP ? 7 : 5,
      fill: isHP ? "red" : "white",
      stroke: "#111",
      "stroke-width": 1.5
    }));
    const t = document.createElementNS("http://www.w3.org/2000/svg", "text");
    t.setAttribute("x", p.x + 8);
    t.setAttribute("y", p.y + 4);
    t.setAttribute("font-size", "12");
    t.textContent = String(p.id);
    svg.appendChild(t);
  }
}

function pickLineFromEdge(aIdx, bIdx) {
  const A = aIdx % 7;
  const B = bIdx % 7;
  for (let i=0;i<FANO_LINES.length;i++) {
    const L = FANO_LINES[i];
    if (L.includes(A) && L.includes(B)) return { index: i, points: L };
  }
  for (let i=0;i<FANO_LINES.length;i++) {
    const L = FANO_LINES[i];
    if (L.includes(A)) return { index: i, points: L };
  }
  return null;
}

async function loadWasm(url) {
  const resp = await fetch(url);
  const bytes = await resp.arrayBuffer();
  const { instance } = await WebAssembly.instantiate(bytes, {});
  return instance;
}

export async function runDemo({ wasmUrl, canbUrl, svgId, logId }) {
  const inst = await loadWasm(wasmUrl);
  const { memory, can_init, can_load, can_step, can_drain_events } = inst.exports;

  const svg = document.getElementById(svgId);
  const log = document.getElementById(logId);
  const logln = (s) => { if (log) log.textContent += s + "\n"; };

  renderFano(svg, null);

  const canbResp = await fetch(canbUrl);
  const canbBytes = new Uint8Array(await canbResp.arrayBuffer());

  const progPtr = 1024;
  new Uint8Array(memory.buffer, progPtr, canbBytes.length).set(canbBytes);

  can_init();
  if (!can_load(1, progPtr, canbBytes.length)) {
    logln("load failed");
    return;
  }
  logln(`Loaded ${canbBytes.length} bytes`);

  const evPtr = 8192;
  const evCap = 64;

  for (let iter=0; iter<200; iter++) {
    const stepped = can_step(1, 50);
    const got = can_drain_events(1, evPtr, evCap);
    if (got) {
      const bytes = new Uint8Array(memory.buffer, evPtr, got * 16);
      for (let i=0;i<got;i++) {
        const off = i*16;
        const tag = bytes[off+0];
        const dv = new DataView(bytes.buffer, bytes.byteOffset + off, 16);
        const a = dv.getUint32(4, true);
        const b = dv.getUint32(8, true);
        const c = dv.getUint32(12, true);

        if (tag === 2) {
          const hl = pickLineFromEdge(a, b);
          if (hl) renderFano(svg, hl);
          logln(`EDGE_ADD a=${a} b=${b} line=${hl ? hl.index : "?"}`);
        } else if (tag === 1) {
          logln(`PUSH_ATOM ${a}`);
        } else if (tag === 3) {
          logln(`PROJ_FANO x=${a} r=${b}`);
        } else if (tag === 255) {
          logln("HALT");
          return;
        } else if (tag === 254) {
          logln(`FAULT code=${a} b=${b} c=${c}`);
          return;
        }
      }
    }
    if (stepped === 0) break;
  }
  logln("done");
}
