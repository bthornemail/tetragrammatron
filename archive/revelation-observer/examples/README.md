# Revelation Observer - Examples

This directory contains examples and demos for the Revelation Observer.

## Quick Start

```bash
# Start web server
python3 -m http.server 8000

# Open in browser:
# http://localhost:8000/
```

## What's Here

### 1. **index.html** - Overview Page
Landing page that explains:
- What Revelation Observer is
- Real-world use cases
- How the 8-pass model works
- Links to the interactive demo

**Best for**: First-time visitors who want to understand the project

---

### 2. **structural-dna-viewer.html** - Interactive Demo ⭐
**The "holy shit" demo** - drag and drop two files, watch them diffuse in real-time.

**Features**:
- 8-pass diffusion (client-side JavaScript)
- Live SHA256 fingerprinting
- 3D visualization with Three.js
- Shapes morph together if structures match
- Shapes stay apart if structures differ

**Use cases shown**:
- Plagiarism detection (same structure, different names)
- Change detection (real changes vs formatting)
- Code similarity visualization

**Best for**: Showing someone the tool for the first time

---

### 3. **demo-comparison.sh** - Command-line Comparison
Shell script that:
- Creates test files
- Diffuses them to structural basis
- Shows uniqueness without semantic interpretation

**Run it**:
```bash
./demo-comparison.sh
```

**Best for**: Understanding the command-line workflow

---

### 4. **demo-evolution.sh** - Evolution Tracking
Shell script that:
- Creates 3 versions of a file
- Shows how structure evolves (or doesn't)
- Demonstrates that comments don't affect structure

**Run it**:
```bash
./demo-evolution.sh
```

**Best for**: Understanding what "structure" means

---

## The 1-2-3 Demo Flow

### For Convincing Someone This Is Useful:

**1. Open index.html**
```bash
python3 -m http.server 8000
# http://localhost:8000/
```

**2. Click "Try The Live Demo"**
Opens `structural-dna-viewer.html`

**3. Drop two files**
- Try files with same logic, different variable names
- Watch them converge (structural match)
- Try files with actual different logic
- Watch them stay apart (structural difference)

**Time to "wow" moment**: < 30 seconds

---

## Example Files to Test

Create these test files to demonstrate:

### Test 1: Plagiarism Detection

**original.py**:
```python
def calculate_total(items):
    sum = 0
    for item in items:
        sum = sum + item
    return sum
```

**copied.py**:
```python
# Added comments to fool plagiarism detector
def compute_sum(data):  # renamed function
    result = 0  # renamed variable
    for x in data:  # renamed variable
        result = result + x
    return result
```

**Expected**: ✅ Structural match (same hash)

---

### Test 2: Real Change Detection

**version1.c**:
```c
int main() {
    printf("Hello");
    return 0;
}
```

**version2.c**:
```c
int main() {
    // Added conditional logic
    if (argc > 1) {
        printf("Hello");
    }
    return 0;
}
```

**Expected**: ❌ Structural difference (different hashes)

---

### Test 3: Formatting Doesn't Matter

**formatted.js**:
```javascript
function add(a,b){return a+b;}
```

**reformatted.js**:
```javascript
function add(a, b) {
    return a + b;
}
```

**Expected**: ✅ Structural match (same hash)

---

## How The Web Demo Works

### Technology Stack
- **Frontend**: Pure HTML/CSS/JavaScript
- **3D Engine**: Three.js (via CDN)
- **Animation**: GSAP (via CDN)
- **Diffusion**: Client-side JS implementation of 8-pass algorithm
- **Hashing**: Web Crypto API (SHA-256)

### No Backend Needed
Everything runs in the browser:
1. User drops file
2. JavaScript reads it (File API)
3. 8 regex passes transform it
4. SHA-256 hash generated
5. 3D shape created based on hash
6. Shapes positioned based on match/differ

**Result**: Single HTML file you can email or host on GitHub Pages

---

## Customization Ideas

### Add More Visualizations
- Heatmap of which pass contributed most to difference
- Timeline showing diffusion progress
- Side-by-side diff of pass outputs

### Add More File Types
Currently works with any text file. Could add:
- Binary file support (hex view)
- Image structural comparison
- JSON/XML structure extraction

### Add Export
- Export comparison report as PDF
- Export 3D shapes as GLB
- Export diff as markdown

---

## Hosting The Demo

### Option 1: Local Server (What We Do Now)
```bash
python3 -m http.server 8000
```

### Option 2: GitHub Pages
```bash
# In your tetragrammatron repo:
# 1. Enable GitHub Pages
# 2. Point to revelation-observer/examples/
# 3. Access at: https://yourusername.github.io/tetragrammatron/revelation-observer/examples/
```

### Option 3: Static Hosting (Netlify, Vercel)
Just drop the `examples/` folder and it works.

---

## Credits

- **8-Pass Diffusion Algorithm**: Original design from Revelation Observer spec
- **Three.js**: https://threejs.org
- **GSAP**: https://greensock.com/gsap/
- **Concept**: Brian Thorne / Tetragrammatron Project

---

## Troubleshooting

**Q: 3D shapes not appearing?**
A: Check browser console. Three.js loads from CDN - need internet connection.

**Q: File diffusion seems slow?**
A: Large files (>1MB) take longer. This is JavaScript, not C. For production use the command-line tool.

**Q: Can I run this offline?**
A: Download Three.js and GSAP, update script src paths to local files.

**Q: Hash doesn't match command-line tool?**
A: The JavaScript implementation should match, but there might be subtle differences in regex engines. Use command-line for production.

---

## Next Steps

After trying the demo:
- Read the [Full Guide](../../docs/guides/revelation-observer.md)
- Try the [Command-line Tool](../bin/genesis-diffuse.sh)
- Explore [Integration Examples](../../integration/README.md)
- Understand [The Architecture](../../docs/architecture/overview.md)
