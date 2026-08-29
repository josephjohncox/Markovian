# Documentation book

The Markovian Book uses [mdBook](https://rust-lang.github.io/mdBook/).

Install the pinned documentation tool and run the complete check:

```sh
scripts/install-doc-tools
scripts/check-book
```

The generated site is `docs/book/build/index.html`. The complete `docs/book/build` directory remains generated and ignored.

Mathematics uses the pinned local MathJax 3.2.2 SVG bundle under `theme/vendor/mathjax`; the built book does not require a MathJax CDN request. A startup pass restores TeX altered by Markdown emphasis or smart punctuation. The book check verifies the bundle digest, supported recovery markup, and every generated display block.

The public book is <https://josephjohncox.github.io/Markovian/>. [Pages run 33126170927](https://github.com/josephjohncox/Markovian/actions/runs/33126170927) records the first successful deployment from `main`.

Edit chapter order in `src/SUMMARY.md`. Read [Build and maintain this book](src/book-workflow.md) for the full workflow.
