(TeX-add-style-hook
 "q5"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "geometry"
    "amsmath")
   (TeX-add-symbols
    "Var"
    "Ci"))
 :latex)

