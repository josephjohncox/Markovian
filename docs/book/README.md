# Documentation book

The Markovian Book uses [mdBook](https://rust-lang.github.io/mdBook/).

Install the pinned documentation tool and run the complete gate:

```sh
scripts/install-doc-tools
scripts/check-book
```

The generated site is `docs/book/build/index.html`. The complete `docs/book/build` directory remains generated and ignored.

The configured public URL is <https://josephjohncox.github.io/Markovian/>. Publication requires a successful Pages deployment from `main`.

Edit chapter order in `src/SUMMARY.md`. Read [Build and maintain this book](src/book-workflow.md) for the full workflow.
