(require 'package)

(package-initialize)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-refresh-contents)

(package-install 'org-plus-contrib)
(package-install 'ox-hugo)

(require 'org)

(with-eval-after-load 'org
  ;; (setq org-latex-listings 'minted
  ;;       org-latex-packages-alist '(("" "minted"))
  ;;       org-latex-pdf-process
  ;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; (with-temp-file "/home/tristan/Devel/adoptingerlang/adoptingerlang.org"
  (org-reload)
  (org-hugo-export-wim-to-md :all-subtrees nil nil :noerror)
  ;; (org-latex-export-to-latex)
  )
