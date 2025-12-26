// Fano Plane → Merkaba (3D) Projection
// Generates 3D coordinates for visualizing the Fano plane as a dual tetrahedron

// Merkaba (dual tetrahedron) vertices - 7 imaginary octonion units
export const MERKABA_VERTICES = [
  // Tetrahedron 1 (upward)
  [ 1,  1,  1],  // Point 0 → e₁
  [-1, -1,  1],  // Point 1 → e₂
  [-1,  1, -1],  // Point 2 → e₃
  [ 1, -1, -1],  // Point 3 → e₄
  // Tetrahedron 2 (downward)
  [-1,  1,  1],  // Point 4 → e₅
  [ 1, -1,  1],  // Point 5 → e₆
  [ 1,  1, -1],  // Point 6 → e₇
];

// Fano plane lines (same as 2D version)
export const FANO_LINES = [
  [0, 1, 2],
  [0, 3, 4],
  [0, 5, 6],
  [1, 3, 6],
  [1, 4, 5],
  [2, 3, 5],
  [2, 4, 6],
];

// Scale and center for visualization
function normalizeVertex(v, scale = 1.0) {
  return [v[0] * scale, v[1] * scale, v[2] * scale];
}

// Generate edge list for rendering
export function generateMerkabaEdges(scale = 1.0) {
  const edges = [];
  const vertices = MERKABA_VERTICES.map(v => normalizeVertex(v, scale));

  // For each Fano line, create the 3 edges of the triangle
  for (const [a, b, c] of FANO_LINES) {
    edges.push({ from: vertices[a], to: vertices[b] });
    edges.push({ from: vertices[b], to: vertices[c] });
    edges.push({ from: vertices[c], to: vertices[a] });
  }

  return { vertices, edges };
}

// Map Fano point index to 3D coordinate
export function fanoPointTo3D(pointIndex, scale = 1.0) {
  if (pointIndex < 0 || pointIndex >= 7) return [0, 0, 0];
  return normalizeVertex(MERKABA_VERTICES[pointIndex], scale);
}

// Find which Fano line contains two points
export function findLineContainingEdge(aIdx, bIdx) {
  const A = aIdx % 7;
  const B = bIdx % 7;
  for (let i = 0; i < FANO_LINES.length; i++) {
    const line = FANO_LINES[i];
    if (line.includes(A) && line.includes(B)) {
      return { index: i, points: line };
    }
  }
  return null;
}

// Generate basic GLB structure (minimal implementation)
export function generateMerkabaGLB(relationEdges = []) {
  const { vertices, edges } = generateMerkabaEdges(1.0);

  // Convert to flat Float32Array
  const positions = new Float32Array(vertices.length * 3);
  vertices.forEach((v, i) => {
    positions[i * 3 + 0] = v[0];
    positions[i * 3 + 1] = v[1];
    positions[i * 3 + 2] = v[2];
  });

  // Edge indices (pair of vertex indices per edge)
  const indices = new Uint16Array(edges.length * 2);
  edges.forEach((edge, i) => {
    // Find vertex indices for this edge
    const fromIdx = vertices.findIndex(v =>
      v[0] === edge.from[0] && v[1] === edge.from[1] && v[2] === edge.from[2]
    );
    const toIdx = vertices.findIndex(v =>
      v[0] === edge.to[0] && v[1] === edge.to[1] && v[2] === edge.to[2]
    );
    indices[i * 2 + 0] = fromIdx;
    indices[i * 2 + 1] = toIdx;
  });

  // Minimal glTF structure
  const gltf = {
    asset: { version: "2.0", generator: "Tetragrammatron-OS Merkaba Projector" },
    scene: 0,
    scenes: [{ nodes: [0], name: "Fano Merkaba" }],
    nodes: [{ mesh: 0, name: "Merkaba" }],
    meshes: [{
      name: "Fano-Merkaba-Edges",
      primitives: [{
        attributes: { POSITION: 0 },
        indices: 1,
        mode: 1, // LINES
        material: 0
      }]
    }],
    materials: [{
      name: "Fano-Edge",
      pbrMetallicRoughness: {
        baseColorFactor: [1.0, 0.2, 0.2, 1.0], // Red
        metallicFactor: 0.0,
        roughnessFactor: 1.0
      }
    }],
    accessors: [
      {
        bufferView: 0,
        componentType: 5126, // FLOAT
        count: vertices.length,
        type: "VEC3",
        min: [-1, -1, -1],
        max: [ 1,  1,  1]
      },
      {
        bufferView: 1,
        componentType: 5123, // UNSIGNED_SHORT
        count: indices.length,
        type: "SCALAR"
      }
    ],
    bufferViews: [
      {
        buffer: 0,
        byteOffset: 0,
        byteLength: positions.byteLength,
        target: 34962 // ARRAY_BUFFER
      },
      {
        buffer: 0,
        byteOffset: positions.byteLength,
        byteLength: indices.byteLength,
        target: 34963 // ELEMENT_ARRAY_BUFFER
      }
    ],
    buffers: [{
      byteLength: positions.byteLength + indices.byteLength
    }]
  };

  return {
    gltf,
    binaryData: { positions, indices }
  };
}

// Encode to GLB binary format
export function encodeGLB(gltfJSON, binaryData) {
  const jsonString = JSON.stringify(gltfJSON);
  const jsonBuffer = new TextEncoder().encode(jsonString);

  // Pad JSON to 4-byte alignment
  const jsonPadding = (4 - (jsonBuffer.length % 4)) % 4;
  const jsonChunkLength = jsonBuffer.length + jsonPadding;

  // Combine binary buffers
  const { positions, indices } = binaryData;
  const binaryChunkLength = positions.byteLength + indices.byteLength;
  const binaryPadding = (4 - (binaryChunkLength % 4)) % 4;
  const totalBinaryLength = binaryChunkLength + binaryPadding;

  // GLB structure
  const totalLength = 12 + 8 + jsonChunkLength + 8 + totalBinaryLength;
  const glb = new ArrayBuffer(totalLength);
  const view = new DataView(glb);

  // Header
  view.setUint32(0, 0x46546C67, true);  // Magic: "glTF"
  view.setUint32(4, 2, true);           // Version: 2
  view.setUint32(8, totalLength, true); // Total length

  // JSON chunk
  view.setUint32(12, jsonChunkLength, true);  // Chunk length
  view.setUint32(16, 0x4E4F534A, true);       // Chunk type: "JSON"
  const jsonView = new Uint8Array(glb, 20, jsonBuffer.length);
  jsonView.set(jsonBuffer);
  // Padding with spaces
  for (let i = 0; i < jsonPadding; i++) {
    view.setUint8(20 + jsonBuffer.length + i, 0x20);
  }

  // Binary chunk
  const binOffset = 12 + 8 + jsonChunkLength;
  view.setUint32(binOffset, totalBinaryLength, true);   // Chunk length
  view.setUint32(binOffset + 4, 0x004E4942, true);      // Chunk type: "BIN\0"

  const posView = new Uint8Array(glb, binOffset + 8, positions.byteLength);
  posView.set(new Uint8Array(positions.buffer));

  const idxView = new Uint8Array(glb, binOffset + 8 + positions.byteLength, indices.byteLength);
  idxView.set(new Uint8Array(indices.buffer));

  // Padding with zeros
  for (let i = 0; i < binaryPadding; i++) {
    view.setUint8(binOffset + 8 + binaryChunkLength + i, 0);
  }

  return new Uint8Array(glb);
}

// Main export function
export function exportFanoMerkabaGLB() {
  const { gltf, binaryData } = generateMerkabaGLB();
  return encodeGLB(gltf, binaryData);
}
