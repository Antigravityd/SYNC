(TeX-add-style-hook
 "hw2"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper") ("siunitx" "separate-uncertainty=true")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "geometry"
    "tgpagella"
    "amsmath"
    "amsthm"
    "amssymb"
    "physics"
    "siunitx")
   (LaTeX-add-amsthm-newtheorems
    "prob"))
 :latex)

