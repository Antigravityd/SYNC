(TeX-add-style-hook
 "hw8"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "geometry"
    "amsmath"
    "siunitx"
    "graphicx")
   (TeX-add-symbols
    "sech"))
 :latex)

