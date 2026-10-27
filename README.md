# difft-pr

A GitHub pull-request viewer that renders **structural** diffs instead of
line-based ones, by running [difftastic](https://difftastic.wilfred.me.uk/)
in the browser as WebAssembly.

It's a [Lamdera](https://lamdera.com) app — full-stack Elm, one codebase for
frontend and backend. The backend talks to the GitHub REST API (so the OAuth
token never reaches the browser) and ships raw before/after file contents to
the frontend, which hands them to the difftastic wasm module to diff
client-side.

## Prerequisites

| Tool | Version | Notes |
| --- | --- | --- |
| [Lamdera](https://dashboard.lamdera.app/docs/download) | 1.3.0 | Compiler + dev server. Bundles its own Elm. |
| Node.js | 20+ | CI builds on 20. |
| Rust (via [rustup](https://rustup.rs)) | 1.85.0 | Only to build the wasm — pinned by difftastic's `rust-toolchain.toml`. |
| [wasi-libc](https://github.com/WebAssembly/wasi-libc) | — | `brew install wasi-libc` |
| LLVM | — | `brew install llvm`. Apple's bundled clang can't target WASI. |

You only need the bottom three if you're building the wasm yourself, which
right now everyone is — see the next section for why.

## Quick start

```sh
git clone git@github.com:perkee/difft-pr.git
cd difft-pr
npm ci
# then build public/difftastic.wasm (see below), and:
lamdera live     # http://localhost:8000
```

## Building the difftastic wasm

`public/difftastic.wasm` is **not in this repo**. It's a ~110 MiB build
artifact, which is over GitHub's 100 MB per-file hard limit, so it's
`.gitignore`d. A fresh clone will compile and serve fine, but every diff will
fail at runtime until this file exists.

It's built from a sibling checkout of difftastic on the
`elm-class-list-injection` branch, which adds the `wasm_api.rs` entry points
this app calls:

```sh
git clone -b elm-class-list-injection <difftastic-fork> ../difftastic
cd ../difftastic
rustup target add --toolchain 1.85.0 wasm32-wasip1

RUSTC="$(rustup which --toolchain 1.85.0 rustc)" \
CC_wasm32_wasip1=$(brew --prefix llvm)/bin/clang \
AR_wasm32_wasip1=$(brew --prefix llvm)/bin/llvm-ar \
CFLAGS_wasm32_wasip1=--sysroot=$(brew --prefix wasi-libc)/share/wasi-sysroot \
  "$(rustup which --toolchain 1.85.0 cargo)" \
  build --lib --release --target wasm32-wasip1

cp target/wasm32-wasip1/release/difftastic.wasm ../difft-pr/public/
```

That invocation looks over-specified, and the reason is worth reading before
you simplify it:

- **If you have Rust from Homebrew as well as rustup, plain `cargo` and
  `rustc` are the Homebrew ones.** `/opt/homebrew/bin` sits ahead of
  `~/.cargo/bin` on a default macOS `PATH`, so `cargo +1.85.0 …` dies with
  ``error: no such command: `+1.85.0` `` — only rustup's shim understands
  `+toolchain`. Check with `which -a cargo rustc`.
- **Fixing `cargo` alone isn't enough, and this is the confusing part.**
  Cargo resolves `rustc` through `PATH` too, so `~/.cargo/bin/cargo +1.85.0`
  still compiles with Homebrew's `rustc`, which has only the host target
  installed. The build then fails on dozens of dependencies with
  ``can't find crate for `core` `` and the misleading note "the
  `wasm32-wasip1` target may not be installed" — even though you did install
  it, just for a toolchain that isn't being used. Pinning `RUSTC` is what
  actually fixes it. Note that `rustup run 1.85.0 cargo …` does **not**: it
  leaves Homebrew's `rustc` first on `PATH`.
- **The target must be installed for 1.85.0 specifically**, not for your
  default toolchain. `rustup target list --installed --toolchain 1.85.0`
  should list `wasm32-wasip1`.
- **The three `*_wasm32_wasip1` variables are for the C dependencies**
  (tree-sitter's grammars), not for Rust. Without them the C sources compile
  against the host sysroot and fail. Apple's bundled clang can't target WASI,
  which is why this points at Homebrew's LLVM.

The target is `wasm32-wasip1`, not `wasm32-unknown-unknown`, so there's no
wasm-bindgen JS glue; `public/difft-wasm.js` hand-writes the nine WASI imports
the module actually needs. Rust 1.85.0 is pinned by difftastic's
`rust-toolchain.toml`.

Expect roughly 110 MiB of output. A cold build is dominated by tree-sitter's
C grammars; rebuilds after touching only difftastic's own sources are quick.

## Compiling and checking the app

All of these are npm scripts, and prefer them over raw commands:

```sh
npm run build      # lamdera make src/Frontend.elm --output=/dev/null
npm test           # elm-test --compiler lamdera
npm run review     # elm-review
npm run test:e2e   # playwright, boots `lamdera live` itself
npm run check      # build + test + review + lamdera check
```

The Playwright run needs browsers installed once, with
`npx playwright install chromium`. It serves the app itself unless you point
it elsewhere with `BASE_URL`.

`npm run check` is what CI runs. Its last step, `lamdera check`, compares your
types against the **deployed** production app to work out whether an Evergreen
migration is needed; it fails with an HTTP 404 if the app has never been
deployed, which is unrelated to whether your code compiles.

Elm tests need `--compiler lamdera` (and `elm-test init` needs
`--compiler $(which lamdera)`) because the project depends on Lamdera-only
packages that stock Elm can't resolve.

## GitHub OAuth

`src/Env.elm` holds the OAuth app's client ID and secret. For local
development, register an OAuth app under GitHub *Settings → Developer
settings → OAuth Apps* with the callback URL `http://localhost:8000`, and put
its credentials there.

`src/Env.elm` is tracked in git, so treat anything committed to it as public
and override the real production values through the Lamdera dashboard rather
than committing them.

## Debugging the wasm layer

`public/demo.html` is a standalone page — no Elm, no Lamdera — that imports
`public/difft-wasm.js` directly and diffs two textareas. When diffs misbehave,
it's the fastest way to tell a wasm/JS problem from an Elm one. Serve
`public/` and open `demo.html`.

The Elm side connects through two ports, `requestDiffs` and `diffsComputed`,
wired up in `elm-pkg-js/difft-wasm.js`.

## Layout

```
src/                 Elm: Frontend, Backend, Types, Env, FileTree, Debounce
elm-pkg-js/          JS wired to Elm ports (difftastic, sticky scrollbar)
public/              Static web root — demo.html, difft-wasm.js, difftastic.wasm
tests/               elm-test suites
e2e/                 Playwright specs
review/              elm-review configuration
```
