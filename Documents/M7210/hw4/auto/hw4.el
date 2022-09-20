(TeX-add-style-hook
 "hw4"
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
    "amssymb"
    "amsthm")
   (TeX-add-symbols
    "nsub")
   (LaTeX-add-environments
    "lem"))
 :latex)

