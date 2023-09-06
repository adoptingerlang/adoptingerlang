;;; install necessary packages and convert org mode files to markdown for hugo
(require 'package)

(package-initialize)

(package-install 'tomelr)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-refresh-contents)

(package-install 'org)
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
