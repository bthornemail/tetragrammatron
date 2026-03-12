# Wave32 UI Pipeline (Mesh + Browser)

This bridge keeps transport non-authoritative and uses existing infrastructure:

- mesh transport: `port-lattice/netcat` (TCP/UDP/Unix/SSL)
- deterministic state/replay: `port-matroid`
- semantic projection: Wave30 -> Wave32 integration in `tetragrammatron`

## Important note

Current `lattice-netcat` in this workspace does not expose a native `--websocket` mode.
This doc uses a thin HTTP/SSE bridge (`scripts/wave32-ui-bridge.mjs`) that connects to lattice TCP.

## Run

1. Start your lattice transport endpoint (example TCP):

```bash
lattice-netcat -l -p 9101
```

2. Start UI bridge:

```bash
cd /home/main/devops/tetragrammatron
LATTICE_HOST=127.0.0.1 LATTICE_PORT=9101 BRIDGE_PORT=8080 \
MATROID_SNAPSHOT_URL=http://127.0.0.1:8081/snapshot \
npm run -s wave32:ui-bridge
```

3. Serve and open browser example:

```bash
cd /home/main/devops/tetragrammatron/examples/wave32-ui
python3 -m http.server 8090
```

Open `http://127.0.0.1:8090`.

## Bridge API

- `GET /health` -> bridge/lattice status
- `GET /stream` -> SSE event stream (`status`, `mesh`)
- `POST /publish` -> publish JSON line into lattice socket
- `GET /snapshot` -> proxy `port-matroid` snapshot endpoint

## Boundary discipline

- bridge is transport/projection adapter only
- no authority mutation behavior in bridge
- canonical semantics remain in wave validators and matroid runtime
