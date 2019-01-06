# Adopting Erlang

## Requirements

Two emacs packages are required for converting `adoptingerlang.org` to pages for the Hugo website or LaTeX for conversion to pdf, mobi, etc:

``` emacs-lisp
(eval-when-compile
  (require 'use-package)
  (require 'package))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package org)
(use-package ox-hugo)
```

## Hugo generated website

To export a section to its Hugo markdown page move the cursor to the section in `adoptingerlang.org` and run `M-x org-hugo-export-wim-to-md`. This will create or update `content/docs/<section filename>.md`.

The side menu is setup in `content/docs/menu/index.md`.

## LaTeX

Exporting to latex is done with `M-x org-latex-export-to-latex`.

