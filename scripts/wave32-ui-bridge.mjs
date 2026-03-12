#!/usr/bin/env node
import http from 'node:http';
import net from 'node:net';
import { URL } from 'node:url';

import { formatSse, parseNdjsonChunk, toBridgeEvent } from '../src/waves/wave32/ui-bridge.mjs';

const LATTICE_HOST = process.env.LATTICE_HOST || '127.0.0.1';
const LATTICE_PORT = Number.parseInt(process.env.LATTICE_PORT || '9101', 10);
const BRIDGE_PORT = Number.parseInt(process.env.BRIDGE_PORT || '8080', 10);
const MATROID_SNAPSHOT_URL = process.env.MATROID_SNAPSHOT_URL || 'http://127.0.0.1:8081/snapshot';

const clients = new Set();
let latticeBuffer = '';

function broadcast(eventName, payload) {
  const msg = formatSse(eventName, payload);
  for (const res of clients) {
    res.write(msg);
  }
}

const lattice = net.createConnection({ host: LATTICE_HOST, port: LATTICE_PORT });

lattice.on('connect', () => {
  broadcast('status', { connected: true, host: LATTICE_HOST, port: LATTICE_PORT });
});

lattice.on('data', (chunk) => {
  const parsed = parseNdjsonChunk(latticeBuffer, chunk.toString('utf8'));
  latticeBuffer = parsed.buffer;
  for (const line of parsed.lines) {
    broadcast('mesh', toBridgeEvent(line));
  }
});

lattice.on('error', (error) => {
  broadcast('status', { connected: false, error: String(error.message || error) });
});

lattice.on('close', () => {
  broadcast('status', { connected: false, closed: true });
});

async function handleSnapshot(res) {
  try {
    const response = await fetch(MATROID_SNAPSHOT_URL);
    const text = await response.text();
    res.writeHead(response.status, { 'content-type': 'application/json; charset=utf-8' });
    res.end(text);
  } catch (error) {
    res.writeHead(502, { 'content-type': 'application/json; charset=utf-8' });
    res.end(JSON.stringify({ ok: false, error: String(error.message || error) }));
  }
}

const server = http.createServer((req, res) => {
  const url = new URL(req.url || '/', `http://${req.headers.host || 'localhost'}`);

  if (req.method === 'GET' && url.pathname === '/health') {
    res.writeHead(200, { 'content-type': 'application/json; charset=utf-8' });
    res.end(JSON.stringify({ ok: true, bridge_port: BRIDGE_PORT, lattice_host: LATTICE_HOST, lattice_port: LATTICE_PORT }));
    return;
  }

  if (req.method === 'GET' && url.pathname === '/stream') {
    res.writeHead(200, {
      'content-type': 'text/event-stream; charset=utf-8',
      'cache-control': 'no-cache',
      connection: 'keep-alive',
    });
    res.write(formatSse('status', { connected: !lattice.destroyed, host: LATTICE_HOST, port: LATTICE_PORT }));
    clients.add(res);
    req.on('close', () => {
      clients.delete(res);
    });
    return;
  }

  if (req.method === 'GET' && url.pathname === '/snapshot') {
    void handleSnapshot(res);
    return;
  }

  if (req.method === 'POST' && url.pathname === '/publish') {
    const chunks = [];
    req.on('data', (c) => chunks.push(c));
    req.on('end', () => {
      try {
        const body = Buffer.concat(chunks).toString('utf8');
        const parsed = JSON.parse(body || '{}');
        const line = `${JSON.stringify(parsed)}\n`;
        lattice.write(line);
        res.writeHead(200, { 'content-type': 'application/json; charset=utf-8' });
        res.end(JSON.stringify({ ok: true }));
      } catch (error) {
        res.writeHead(400, { 'content-type': 'application/json; charset=utf-8' });
        res.end(JSON.stringify({ ok: false, error: String(error.message || error) }));
      }
    });
    return;
  }

  res.writeHead(404, { 'content-type': 'application/json; charset=utf-8' });
  res.end(JSON.stringify({ ok: false, error: 'not_found' }));
});

server.listen(BRIDGE_PORT, () => {
  console.log(`ok wave32 ui bridge http=:${BRIDGE_PORT} lattice=${LATTICE_HOST}:${LATTICE_PORT}`);
});
