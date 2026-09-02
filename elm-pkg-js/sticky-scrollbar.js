// Defines <difft-scrollbar>, the single horizontal scrollbar rendered
// beneath each file's diff (see `viewFileDiff` in src/Frontend.elm).
//
// The diff columns are fixed 50/50 viewports that never scroll as boxes:
// each line slides inside its stationary `.line-clip` window by the
// `--hscroll` custom property (CSS in Frontend.elm). This element is the
// thing that sets that property: it recreates the scroll range with a
// spacer child sized to (own width + the longest line's overhang past
// its clip window), sticks to the viewport bottom while the file is in
// view, and feeds its scrollLeft into `--hscroll` on the `.diff-chunks`
// sibling — one scrollbar, both columns shifting by the same amount.
// Trackpad/shift-wheel horizontal gestures over the diff are also routed
// here, since the diff itself has no scroll container anymore.
//
// Elm renders the element empty and only ever touches its `wrap`
// attribute, so the spacer child and classes added here are safe from
// the virtual-dom differ.
//
// elm-pkg-js files export an `init(app)` called once the Elm app starts;
// this one needs no ports, just a chance to register the element.

exports.init = async function init() {
  if (customElements.get('difft-scrollbar')) {
    return;
  }

  customElements.define(
    'difft-scrollbar',
    class extends HTMLElement {
      // Elm flips `wrap` with the line-wrap toggle. Wrapping changes
      // every line's overhang without any DOM mutation or box resize, so
      // this is the only signal that a re-measure is needed.
      static get observedAttributes() {
        return ['wrap'];
      }

      connectedCallback() {
        const target = this.previousElementSibling;
        if (!target || !target.classList.contains('diff-chunks')) {
          // Failed diffs and empty bodies render other siblings; stay inert.
          return;
        }
        this._target = target;

        if (!this._spacer) {
          this._spacer = document.createElement('div');
          this._spacer.style.height = '1px';
          this.appendChild(this._spacer);
        }

        // Re-measure at most once per frame: MutationObserver can fire in
        // bursts, and attributeChangedCallback runs mid-patch, before the
        // ancestor's wrap-lines class has necessarily been applied.
        this._refresh = () => {
          if (this._refreshQueued) {
            return;
          }
          this._refreshQueued = true;
          requestAnimationFrame(() => {
            this._refreshQueued = false;
            if (!this._target) {
              return;
            }
            let range = 0;
            for (const clip of this._target.querySelectorAll('.line-clip')) {
              const overhang = clip.scrollWidth - clip.clientWidth;
              if (overhang > range) {
                range = overhang;
              }
            }
            this._range = range;
            // Order matters: unhiding first, so clientWidth is real when
            // the spacer is sized (a display:none box measures 0).
            this.classList.toggle('has-overflow', range > 1);
            this._spacer.style.width = this.clientWidth + range + 'px';
            // The browser clamps scrollLeft when the range shrinks; push
            // the (possibly clamped) value back out so lines never stay
            // shifted past their new end.
            this._applyScroll();
          });
        };

        this._applyScroll = () => {
          this._target.style.setProperty('--hscroll', this.scrollLeft + 'px');
        };
        this.addEventListener('scroll', this._applyScroll, { passive: true });

        // The diff has no scroll container of its own anymore, so
        // horizontal trackpad/shift-wheel gestures over it land here.
        // Vertical scrolling stays the page's.
        this._onWheel = (e) => {
          if (this._range > 0 && Math.abs(e.deltaX) > Math.abs(e.deltaY)) {
            this.scrollLeft += e.deltaX;
            e.preventDefault();
          }
        };
        target.addEventListener('wheel', this._onWheel, { passive: false });

        // Container box changes (window/sidebar resize) …
        this._resizeObserver = new ResizeObserver(this._refresh);
        this._resizeObserver.observe(target);
        // … and content changes (expanding context lines) both move the
        // longest line's overhang.
        this._mutationObserver = new MutationObserver(this._refresh);
        this._mutationObserver.observe(target, { childList: true, subtree: true });

        this._refresh();
      }

      attributeChangedCallback() {
        if (this._refresh) {
          this._refresh();
        }
      }

      disconnectedCallback() {
        if (this._resizeObserver) {
          this._resizeObserver.disconnect();
          this._resizeObserver = null;
        }
        if (this._mutationObserver) {
          this._mutationObserver.disconnect();
          this._mutationObserver = null;
        }
        if (this._target) {
          this._target.removeEventListener('wheel', this._onWheel);
          this.removeEventListener('scroll', this._applyScroll);
          this._target = null;
        }
      }
    }
  );
};
