(TeX-add-style-hook
 "hw7"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "siunitx"
    "geometry"
    "amsmath"
    "amssymb"
    "graphicx"))
 :latex)

