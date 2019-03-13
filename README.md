# Adopting Erlang

The Hugo theme is a submodule, so when cloning use the `--recursive` option:

``` shell
git clone --recursive https://github.com/tsloughter/adoptingerlang
```

## Requirements

Two emacs packages are required for converting `adoptingerlang.org` to pages for the Hugo website or LaTeX for conversion to pdf, mobi, etc. The following can be added to the beginning of your `~/.emacs.d/init.el` file if you have one or create that file with this content:

``` emacs-lisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'package))

(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))

(use-package org
  :ensure t
  :mode (("\\.org\\'" . org-mode))
  :config
  (progn
    (setq org-startup-indented t)
    (add-hook 'org-mode-hook #'visual-line-mode)
    (setq org-src-fontify-natively t)))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ox-hugo
  :ensure t)
```

## Hugo generated website

See [https://ox-hugo.scripter.co/doc/formatting/](https://ox-hugo.scripter.co/doc/formatting/) for basic `Org` formatting compared to the Markdown equivalent.

To export a section to its Hugo markdown page move the cursor to the section in `adoptingerlang.org` and run `M-x org-hugo-export-wim-to-md`. This will create or update `content/docs/<section filename>.md`.

The side menu is setup in `content/docs/menu/index.md`.

## LaTeX

Exporting to latex is done with `M-x org-latex-export-to-latex`.

## LaTex to PDF

Easiest way to convert to a pdf is to just use a LaTeX docker image:

``` shell
$ docker run --rm -it -v .:/source schickling/latex
# pdflatex adoptingerlang.tex
```

