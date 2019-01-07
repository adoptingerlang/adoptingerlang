(require 'package)

(package-initialize)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-refresh-contents)

(package-install 'org-plus-contrib)
(package-install 'ox-hugo)

(require 'org)

(with-eval-after-load 'org
  ;; (with-temp-file "/home/tristan/Devel/adoptingerlang/adoptingerlang.org"
  (org-hugo-export-wim-to-md :all-subtrees nil nil :noerror))
