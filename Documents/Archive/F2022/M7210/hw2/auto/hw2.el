(TeX-add-style-hook
 "hw2"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "geometry"
    "amsmath"
    "amssymb"
    "amsthm"
    "minted"
    "tgpagella")
   (LaTeX-add-amsthm-newtheorems
    "prop"))
 :latex)

