(TeX-add-style-hook
 "hw1"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "geometry"
    "amsmath"
    "amsthm"
    "amssymb"
    "siunitx")
   (LaTeX-add-amsthm-newtheorems
    "prob"))
 :latex)

