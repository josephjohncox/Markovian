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

1. Check the installed `mdbook` version, capability records, snippet classification and static generated-output receipt.
2. Validate local Markdown links and anchors.
3. Validate escaped and balanced display-math delimiters.
4. Build the complete HTML book.
5. Verify the pinned local MathJax digest, generated local script tags, supported Markdown-to-TeX recovery markup, and every source-to-generated display block.

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

`check-book-links` rejects single-backslash and unbalanced delimiters. Markdown also consumes TeX punctuation escapes. Write two source backslashes for commands such as `\\,`, `\\!`, and `\\{`. Use `\star` or `\ast` instead of a bare `*`, because Markdown can turn paired stars into emphasis across a formula.

mdBook can also turn paired TeX subscripts into `<em>` elements and ASCII primes into smart punctuation. The local MathJax configuration restores those forms before typesetting. `check-book` rejects unsupported generated tags and verifies that every source display block survives in its generated chapter HTML. A release check executes all generated equations through MathJax startup and requires no raw delimiters or MathJax errors.

The bundle, license, source URL, and digest are recorded under `docs/book/theme/vendor/mathjax`. Update them together. Do not replace the versioned local bundle with an unpinned network URL.

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

## Snippet classification

Every Haskell fence in the book, including Markdown appendices, is classified in
[`docs/learning/fences.json`](../../learning/fences.json). Fence IDs are the
source path and the one-based Haskell-fence ordinal. Adding, deleting or moving
a fence requires updating the manifest. Three classes have different promises:

- **Runnable:** the complete `Sample` program or a complete laboratory module
  invoked by the registered existing test component. The checker builds the
  component, executes the fixture and compares its entire stdout with the
  displayed generated file. These examples are not pseudocode.
- **Compiled-source fragment:** an excerpt from a registered source module.
  Matching allows line wrapping and different indentation widths, but preserves
  the relative column ordering of displayed line starts against the source.
  This catches layout drift without claiming a standalone Haskell parser.
  The manifest verifies that module belongs to the named Cabal component;
  `--run` compiles that component. The book omits imports,
  inferred parameters and surrounding result handling. Blank lines can separate
  excerpts with distinct contexts/error types; do not paste them as one `do`
  block. The new `*Contexts.hs` test modules supply the complete wrappers. They
  infer parameter constraints rather than presenting new public signatures.
- **Explicit pseudocode:** only the conceptual `Parametric` sketch in
  [categorical learning](categorical-learning.md), the proposed `Flow`/`Signal`
  sketch in [polarity and games](polarity-and-games.md), and the historical
  opaque API synopsis in the decisions appendix. Each has a reason and a pinned
  text digest. None is a runnable example or a new implemented API.

All other Haskell fences are compiled fragments unless identified as runnable
above. The snippets retain real API calls: different `Either` error types are
converted explicitly with `either (fail . show) pure` where a single IO context
joins them. Type-checking a fragment does not claim its callbacks run successfully
for arbitrary parameters or that CUDA hardware was exercised.

```sh
python3 scripts/check-learning          # static references and output receipt
python3 scripts/check-learning --run    # compile contexts, run and compare outputs
python3 scripts/test_learning.py        # deliberate checker failures
python3 scripts/check-learning --write  # execute before regenerating; review the diff
```

The static receipt binds displayed outputs to fixture/context sources, their
Cabal membership, the manifest/checker and package implementation sources. It
is a freshness check, not a cryptographic proof that someone ran a compiler.
Source CI executes `--run`; `check-book`/Pages need only Python for the static
gate. Source archives include the same artifacts. The archive gate reconstructs
the original package layout without Git and repeats compilation and execution.
Do not edit a receipt or displayed answer to make a failed experiment pass.

## Document a semantic change

Update all affected documents in one change:

1. Update the relevant book chapter.
2. Update `README.md` if the feature list changes.
3. Update `CHANGELOG.md` for user-visible behavior.
4. Update `docs/ARCHITECTURE.md` for a contract change.
5. Add a decision to `docs/DECISIONS.md` for a new proof boundary.
6. Update `docs/CONTEXT.md` and `TODO.md` when project state changes.

## Review narrative coherence

A technical chapter must fit the book, not only pass its local equations.

1. State what the reader must already know.
2. Connect the chapter to the previous concept before adding new notation.
3. Give the finite Markovian case before a broader categorical analogy.
4. Mark design sketches, implemented APIs, deductions, and unsupported extensions explicitly.
5. Check that one term has one meaning across neighboring chapters.
6. Add a reading route with a primary source or standard book for each new structure.
7. Ask a mathematical reviewer to test claims and a pedagogical reviewer to test the learning sequence.
8. Preserve correct existing material unless the review identifies a contradiction, unsupported claim, or narrative break.

Record the review findings before publication. A prose review does not replace equation, link, build, or browser-rendering checks.

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
