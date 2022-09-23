(TeX-add-style-hook
 "hw3"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "geometry"
    "tgpagella"
    "amsmath"
    "amsthm"
    "amssymb"
    "siunitx"
    "physics"
    "chemmacros")
   (LaTeX-add-amsthm-newtheorems
    "prob"
    "lem"))
 :latex)

