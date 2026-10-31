# Revision history for mjml

## 0.1.0.2 -- 2026-10-31

* Build with `text-builder-linear` 0.1.3 again. `mj-button` decided whether to
  emit a `target` attribute by comparing the rendered tag against `"a"`, which
  needs an `Eq Builder` instance that only arrived in `text-builder-linear`
  0.1.4. The declared lower bound was 0.1.3, so installing against an older
  package index failed to compile; the tag is now chosen from the same
  condition that produced it. No change to the rendered HTML.

## 0.1.0.1 -- 2026-10-31

* README: a worked example with screenshots of the rendered email.

## 0.1.0.0 -- 2026-10-31

* First version.
