# Adopting Erlang

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

## Creating Snapshots

These are instructions for how we create snapshots.

First, create a variable with the date in format `<year>-<month>-<day>`:

```
$ DATE=`date +"%Y-%m-%d"`
```

In Netlify under `Branches and deploy contexts` add the branch `${DATE}` to the
list of `Branch deploys`.

Next, create a branch with today's date, prefixed by `s-`:

```
$ git checkout -b s-${DATE}`
```

Edit `config.toml` to set the version to the current date:

```
[params]
version = "${DATE}"
```

Add a row to the menu in `./themes/custom/layouts/partials/docs/menu.html` using
that version as the subdomain and check for which row to have selected:

```
<option value="http://${DATE}.adoptingerlang.org" {{if eq
.Site.Params.Version "${DATE}" }}selected{{end}} >${DATE}</option>
```

Commit those changes:

```
$ git commit -a -m "create snapshot-${DATE}"
```

Push to the branch and create a new tag, `snapshot-${DATE}` to push:

```
$ git push origin s-${DATE}:s-${DATE}

$ git tag -s snapshot-${DATE} -m "snapshot of site on ${DATE}"

$ git push origin snapshot-${DATE}
```

This will trigger `Push snapshot branch` in Github Actions which will create the
branch `${DATE}` after building the site so that Netlify can deploy the content.

Finally, update DNS to point a CNAME for the Netlify branch deploy, `CNAME ${DATE} -> ${DATE}-subdomain.netlify.app`.
