// Defines <difft-scrollbar>, the always-visible horizontal scrollbar
// rendered beneath each file's diff (see `viewFileDiff` in
// src/Frontend.elm). The real scroll container (.diff-chunks) spans the
// whole file, so its native scrollbar sits at the bottom of the diff
// block — off-screen for any tall file. This element sticks to the
// viewport bottom instead (CSS in Frontend.elm), mirrors the container's
// scroll width with a 1px spacer, and keeps the two scrollLefts in sync
// both ways.
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
      // Elm flips `wrap` with the line-wrap toggle. Wrapped lines change
      // the container's scroll width without any DOM mutation or box
      // resize, so this is the only signal that a re-measure is needed.
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
            this._spacer.style.width = this._target.scrollWidth + 'px';
            this.classList.toggle(
              'has-overflow',
              this._target.scrollWidth > this._target.clientWidth + 1
            );
          });
        };

        // Two-way sync, loop-free without flags: assignment only happens
        // when the values differ, so the echo event finds them equal and
        // stops.
        this._onTargetScroll = () => {
          if (Math.abs(this.scrollLeft - target.scrollLeft) >= 1) {
            this.scrollLeft = target.scrollLeft;
          }
        };
        this._onSelfScroll = () => {
          if (Math.abs(target.scrollLeft - this.scrollLeft) >= 1) {
            target.scrollLeft = this.scrollLeft;
          }
        };
        target.addEventListener('scroll', this._onTargetScroll, { passive: true });
        this.addEventListener('scroll', this._onSelfScroll, { passive: true });

        // Container box changes (window/sidebar resize) …
        this._resizeObserver = new ResizeObserver(this._refresh);
        this._resizeObserver.observe(target);
        // … and content changes (expanding context lines) both move the
        // scroll width.
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
          this._target.removeEventListener('scroll', this._onTargetScroll);
          this.removeEventListener('scroll', this._onSelfScroll);
          this._target = null;
        }
      }
    }
  );
};
