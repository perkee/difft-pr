// Minimal browser-side loader for the difftastic WASM build.
//
// The module is compiled for wasm32-wasip1 (see ../../difftastic, branch
// elm-class-list-injection), not wasm32-unknown-unknown, so it needs WASI
// imports satisfied rather than wasm-bindgen's JS glue. Its actual WASI
// surface is tiny (checked with `wasm-objdump -x`): fd_write, random_get,
// environ_get, environ_sizes_get, fd_close, fd_prestat_get,
// fd_prestat_dir_name, fd_seek, proc_exit. Since that's small and fixed,
// this hand-writes just those instead of pulling in a full WASI polyfill.
//
// The module exports four plain functions (see difftastic's src/wasm_api.rs
// for the authoritative contract):
//   alloc(len) -> ptr                         reserve `len` bytes
//   dealloc(ptr, len)                         free a buffer from alloc
//   diff(dp_ptr, dp_len, b_ptr, b_len, a_ptr, a_len) -> ptr
//     display_path/before/after as (ptr, len) pairs (UTF-8 bytes already
//     written into memory via alloc). Returns a pointer to a 4-byte
//     little-endian length prefix followed by that many bytes of UTF-8
//     JSON (difft --display=json shape for one file).
//   free_result(ptr, content_len)             free a buffer from diff

/**
 * Build the WASI import object. Only the calls difftastic's wasm build
 * actually makes are implemented — this is not a general-purpose WASI
 * runtime.
 */
function makeWasiImports(getMemory) {
  const WASI_ESUCCESS = 0;
  const WASI_EBADF = 8;

  return {
    // No env vars: report zero of them, and never get asked to write any.
    environ_sizes_get(environCountPtr, environBufSizePtr) {
      const view = new DataView(getMemory().buffer);
      view.setUint32(environCountPtr, 0, true);
      view.setUint32(environBufSizePtr, 0, true);
      return WASI_ESUCCESS;
    },
    environ_get() {
      return WASI_ESUCCESS;
    },

    // Fill with real randomness; this seeds Rust's hashmap RNG.
    random_get(bufPtr, bufLen) {
      const bytes = new Uint8Array(getMemory().buffer, bufPtr, bufLen);
      crypto.getRandomValues(bytes);
      return WASI_ESUCCESS;
    },

    // No preopened directories or real file descriptors: every fd
    // operation just reports "not a valid fd", which is how a WASI
    // program discovers there's nothing there.
    fd_prestat_get() {
      return WASI_EBADF;
    },
    fd_prestat_dir_name() {
      return WASI_EBADF;
    },
    fd_close() {
      return WASI_EBADF;
    },
    fd_seek() {
      return WASI_EBADF;
    },

    // stdout/stderr: decode the iovecs and forward to console. Useful for
    // Rust panic messages; difft's own diff output never goes through
    // this (it's returned directly from the `diff` export instead).
    fd_write(fd, iovsPtr, iovsLen, nwrittenPtr) {
      const memory = getMemory();
      const view = new DataView(memory.buffer);
      let text = '';
      let totalWritten = 0;
      for (let i = 0; i < iovsLen; i++) {
        const base = view.getUint32(iovsPtr + i * 8, true);
        const len = view.getUint32(iovsPtr + i * 8 + 4, true);
        text += new TextDecoder().decode(new Uint8Array(memory.buffer, base, len));
        totalWritten += len;
      }
      view.setUint32(nwrittenPtr, totalWritten, true);
      if (fd === 1) {
        console.log(text);
      } else if (fd === 2) {
        console.error(text);
      }
      return WASI_ESUCCESS;
    },

    // Difftastic's diff-computation path should never hit this — it's
    // only reachable via a Rust panic/abort. Throwing unwinds out of the
    // wasm call instead of letting execution continue past an exit.
    proc_exit(code) {
      throw new Error(`difftastic wasm module called proc_exit(${code})`);
    },
  };
}

/**
 * Load and instantiate the difftastic wasm module from `wasmUrl`, and
 * return a `diffFiles` function for computing structural diffs.
 */
export async function loadDifftastic(wasmUrl) {
  let memory;
  const wasiImports = makeWasiImports(() => memory);

  // Not instantiateStreaming: that requires the server to send
  // `Content-Type: application/wasm`, which not every static file server
  // does by default (Lamdera's dev server, for one, sends
  // application/octet-stream) — instantiateStreaming rejects anything
  // else outright. Fetching the bytes and instantiating them directly
  // works regardless of the server's MIME type configuration, at the
  // cost of streaming compilation.
  const bytes = await fetch(wasmUrl).then((response) => response.arrayBuffer());
  const { instance } = await WebAssembly.instantiate(bytes, {
    wasi_snapshot_preview1: wasiImports,
  });

  memory = instance.exports.memory;
  const { alloc, dealloc, free_result: freeResult, diff } = instance.exports;

  function writeString(str) {
    const bytes = new TextEncoder().encode(str);
    const ptr = alloc(bytes.length);
    new Uint8Array(memory.buffer, ptr, bytes.length).set(bytes);
    return { ptr, len: bytes.length };
  }

  /**
   * Diff two pieces of source text. `displayPath` only needs a plausible
   * file extension (e.g. "Foo.elm") — difftastic uses it purely to guess
   * the language, not to read a real file. Returns the parsed JSON
   * result (the same shape as `difft --display=json` for one file).
   */
  function diffOne(displayPath, before, after) {
    const dp = writeString(displayPath);
    const b = writeString(before);
    const a = writeString(after);

    let resultPtr;
    try {
      resultPtr = diff(dp.ptr, dp.len, b.ptr, b.len, a.ptr, a.len);
    } finally {
      dealloc(dp.ptr, dp.len);
      dealloc(b.ptr, b.len);
      dealloc(a.ptr, a.len);
    }

    const view = new DataView(memory.buffer, resultPtr, 4);
    const contentLen = view.getUint32(0, true);
    const jsonBytes = new Uint8Array(memory.buffer.slice(resultPtr + 4, resultPtr + 4 + contentLen));
    freeResult(resultPtr, contentLen);

    return JSON.parse(new TextDecoder().decode(jsonBytes));
  }

  /**
   * Diff a batch of files. Each entry is `{ filename, lhsContent,
   * rhsContent }`; returns the same entries with a `diff` field added
   * (the parsed difft JSON result), or an `error` field if that one file
   * failed to diff (a single bad file shouldn't sink the whole batch).
   */
  function diffFiles(files) {
    return files.map((file) => {
      try {
        return { ...file, diff: diffOne(file.filename, file.lhsContent, file.rhsContent) };
      } catch (err) {
        return { ...file, error: String(err && err.message ? err.message : err) };
      }
    });
  }

  return { diffOne, diffFiles };
}
