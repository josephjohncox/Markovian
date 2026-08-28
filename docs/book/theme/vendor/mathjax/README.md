# Vendored MathJax

The book uses the complete MathJax 3 SVG component from:

- Version: `3.2.2`
- Source: <https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-svg-full.js>
- SHA-256: `a4354ff94fd868aea0cc6eaaa79a57fda0588646fc46ee3700a349ee0a11cbe6`
- License: Apache-2.0; see `LICENSE`

The bundle is committed so mathematics renders without a third-party request. `scripts/check-book` verifies the digest, the generated local script tags, and every display-math block.
