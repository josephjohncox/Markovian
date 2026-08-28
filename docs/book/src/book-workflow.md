# Build and maintain this book

## Source layout

The book configuration is `docs/book/book.toml`. Chapter sources are in `docs/book/src`.

`SUMMARY.md` defines the navigation order. Add each new chapter to this file.

The generated HTML is in `docs/book/build`. Git ignores this directory.

## Install the pinned tool

```sh
scripts/install-doc-tools
```

The script reads `MARKOVIAN_MDBOOK_VERSION` from `toolchain.env`. It installs that exact crate release with Cargo's locked dependency graph.

## Run the complete book check

```sh
scripts/check-book
```

This command performs these checks:

1. Check the installed `mdbook` version.
2. Validate local Markdown links and anchors.
3. Validate escaped and balanced display-math delimiters.
4. Build the complete HTML book.
5. Check the output index and MathJax loader.

CI runs the same command.

## Publish with GitHub Pages

The `Pages` workflow runs for each push to `main`. A manual run also publishes only when it uses `main`.

The build job installs the exact `MARKOVIAN_MDBOOK_VERSION` through `scripts/install-doc-tools`. It then runs `scripts/check-book` and uploads `docs/book/build` as the Pages artifact.

The deployment job needs the successful build job. Only this job has `pages: write` and `id-token: write` permissions. It uses the `github-pages` environment and reports the URL from the deployment action.

All actions use immutable commit SHAs. The workflow uses `actions/checkout` v7.0.1, `actions/upload-pages-artifact` v5.0.0, `actions/configure-pages` v6.0.0, and `actions/deploy-pages` v5.0.0.

The configured project URL is <https://josephjohncox.github.io/Markovian/>. The mdBook `site-url` is `/Markovian/` for the project-site base path.

The generated `docs/book/build` directory remains ignored. Do not commit its files.

A local book check proves only the local source and generated output. Publication requires a successful hosted `Pages` run for the same revision and a reachable public URL.

The Pages workflow does not replace the package CI workflow. Record package CI evidence separately when a change requires those checks.

## Write mathematics

The mdBook Markdown parser consumes one backslash from a display-math delimiter. Write two backslashes in the Markdown source:

```text
\\[
V(s)=\sum_x p(x)r(x).
\\]
```

Do not write a single-backslash delimiter or `$$`. The [mdBook MathJax guide](https://rust-lang.github.io/mdBook/format/mathjax.html) documents this requirement.

`check-book-links` rejects single-backslash and unbalanced delimiters. `check-book` also verifies that the generated HTML loads MathJax and contains a display-math delimiter.

## Cite mathematical claims

Use a published primary paper or a standard book for each mathematical structure. Give a chapter or section when it helps the reader locate the result.

Separate three kinds of statement:

1. Link an implemented fact to an executable fixture.
2. Give the derivation for a mathematical deduction.
3. State the boundary that prevents a stronger claim.

Prefer DOI, publisher, journal, arXiv, or author-hosted links. Do not use an uncited blog as the authority for a law.

Add each durable source to [References and further reading](references.md). Add a short further-reading section to the chapter that uses it.

## Write examples

Prefer a complete executable source over an isolated fragment. The first MDP chapter includes `app/Sample/Main.hs` directly.

When a fragment omits imports or error conversion, explain that fact. Do not present pseudocode as a complete program.

Use exact rational numbers for semantic examples. State an approximation boundary before you use `Double`.

## Document a semantic change

Update all affected documents in one change:

1. Update the relevant book chapter.
2. Update `README.md` if the feature list changes.
3. Update `CHANGELOG.md` for user-visible behavior.
4. Update `docs/ARCHITECTURE.md` for a contract change.
5. Add a decision to `docs/DECISIONS.md` for a new proof boundary.
6. Update `docs/CONTEXT.md` and `TODO.md` when project state changes.

## Verify prose

Use direct technical prose. Use one term for one concept. Keep procedures in numbered steps.

Run the repository's prose checker:

```sh
python3 /home/josephcox/.pi/agent/skills/ste-writing/scripts/ste-lint.py \
  README.md TODO.md docs/*.md docs/book/src/*.md
```

The checker is heuristic. Review equations, code, links, and semantic claims manually.

## Scope of the book

The book explains the supported public semantics. It also states important counterexamples and unsupported claims.

Do not turn a research proposal into user documentation. Record the technical decision and evidence before you describe a new abstraction as implemented.
