(TeX-add-style-hook
 "hw4"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper")))
   (TeX-run-style-hooks
    "latex2e"
    "plt"
    "article"
    "art10"
    "geometry"
    "tgpagella"
    "amsmath"
    "amssymb"
    "amsthm"
    "tikz"
    "minted"
    "physics"
    "siunitx"
    "mhchem"
    "float"
    "endiagram"
    "gnuplot-lua-tikz")
   (LaTeX-add-environments
    "plm"))
 :latex)

