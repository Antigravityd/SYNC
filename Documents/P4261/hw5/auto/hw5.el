(TeX-add-style-hook
 "hw5"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "geometry"
    "amsmath"
    "amssymb"
    "siunitx"
    "graphicx"
    "tikz")
   (TeX-add-symbols
    '("Ox" 2)
    '("Ti" 2)
    '("Ba" 2)))
 :latex)

