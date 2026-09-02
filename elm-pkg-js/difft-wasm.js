// Wires the `requestDiffs` / `diffsComputed` ports (declared in
// src/Frontend.elm) to the difftastic wasm module, so PR diffs are
// computed client-side in the browser instead of by a server-side native
// binary. See public/difft-wasm.js for the actual wasm loading/calling
// logic (kept there, not here, so public/demo.html can import it too as a
// plain ES module without any Elm/Lamdera involved).
//
// elm-pkg-js files export an `init(app)` called once the Elm app starts;
// `app.ports.<name>.subscribe`/`.send` is how JS and Elm talk after that.
// See https://github.com/lamdera/elm-pkg-js and this repo's CLAUDE.md.

exports.init = async function init(app) {
  if (!app.ports || !app.ports.requestDiffs) {
    // No diff-related ports on this page (e.g. a future non-PR page).
    return;
  }

  const { loadDifftastic } = await import('/difft-wasm.js');
  const difftasticPromise = loadDifftastic('/difftastic.wasm');

  app.ports.requestDiffs.subscribe(async (files) => {
    try {
      const { diffFiles } = await difftasticPromise;
      app.ports.diffsComputed.send({ ok: true, results: diffFiles(files) });
    } catch (err) {
      app.ports.diffsComputed.send({
        ok: false,
        error: String(err && err.message ? err.message : err),
      });
    }
  });
};
