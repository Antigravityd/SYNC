(TeX-add-style-hook
 "hw3"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "geometry"
    "tgpagella"
    "amsmath"
    "amssymb"
    "amsthm"
    "tikz"
    "graphicx"
    "minted"
    "physics"
    "siunitx")
   (TeX-add-symbols
    "sgn")
   (LaTeX-add-amsthm-newtheorems
    "plm"
    "lem"))
 :latex)

