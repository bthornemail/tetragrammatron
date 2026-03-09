import { createServer } from 'node:http';

function sendJson(res, status, payload) {
  const body = `${JSON.stringify(payload)}\n`;
  res.writeHead(status, {
    'content-length': Buffer.byteLength(body),
    'content-type': 'application/json; charset=utf-8',
  });
  res.end(body);
}

async function readJsonBody(req) {
  const chunks = [];
  for await (const chunk of req) {
    chunks.push(chunk);
  }
  const text = Buffer.concat(chunks).toString('utf8');
  if (text.trim() === '') {
    return {};
  }
  return JSON.parse(text);
}

function statusForResult(result, fallback = 500) {
  if (!result || typeof result !== 'object') {
    return fallback;
  }
  if (result.ok === true) {
    return 200;
  }
  if (result.code === 'invalid_request' || result.code === 'invalid_stage' || result.code === 'invalid_sid' || result.code === 'unsupported_adapter') {
    return 400;
  }
  if (result.code === 'sid_not_found') {
    return 404;
  }
  if (result.code === 'not_implemented') {
    return 501;
  }
  if (result.meta?.category === 'protocol_reject') {
    return 422;
  }
  return fallback;
}

export async function handleCoreHttpRequest(host, { method, pathname, body = {} }) {
  const parts = pathname.split('/').filter(Boolean);

  if (method === 'POST' && pathname === '/resolve') {
    const result = await host.resolve(body);
    return { payload: result, status: statusForResult(result, 422) };
  }

  if (method === 'GET' && parts.length === 2 && parts[0] === 'sid') {
    const digest = decodeURIComponent(parts[1]);
    const result = await host.getDescriptorByDigest(digest);
    return { payload: result, status: statusForResult(result, 404) };
  }

  if (method === 'POST' && pathname === '/verify-capability') {
    const result = await host.verifyCapability(body);
    return { payload: result, status: statusForResult(result, 501) };
  }

  if (method === 'GET' && parts.length === 3 && parts[0] === 'adapter') {
    const label = decodeURIComponent(parts[1]);
    const sid = decodeURIComponent(parts[2]);
    const result = await host.deriveAdapter(label, sid, {});
    return { payload: result, status: statusForResult(result, 400) };
  }

  return {
    payload: { code: 'not_found', ok: false },
    status: 404,
  };
}

export function createCoreHttpServer(host) {
  return createServer(async (req, res) => {
    try {
      const method = req.method ?? 'GET';
      const url = new URL(req.url ?? '/', 'http://127.0.0.1');
      const body = method === 'POST' ? await readJsonBody(req) : {};
      const mapped = await handleCoreHttpRequest(host, {
        body,
        method,
        pathname: url.pathname,
      });
      return sendJson(res, mapped.status, mapped.payload);
    } catch (error) {
      const digest = Buffer.from(error?.message ?? 'unknown').toString('hex').slice(0, 32);
      return sendJson(res, 500, {
        code: 'internal_transport_mapping_error',
        digest,
        ok: false,
      });
    }
  });
}

export async function startCoreHttpServer(host, { port = 0 } = {}) {
  const server = createCoreHttpServer(host);
  await new Promise((resolve, reject) => {
    server.once('error', reject);
    server.listen(port, '127.0.0.1', resolve);
  });

  const address = server.address();
  if (!address || typeof address === 'string') {
    throw new Error('Core HTTP server failed to bind');
  }

  return {
    close: () => new Promise((resolve, reject) => server.close((err) => (err ? reject(err) : resolve()))),
    server,
    url: `http://127.0.0.1:${address.port}`,
  };
}
