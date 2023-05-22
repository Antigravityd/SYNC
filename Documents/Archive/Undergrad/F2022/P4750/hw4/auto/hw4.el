(TeX-add-style-hook
 "hw4"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (TeX-run-style-hooks
    "latex2e"
    "p1"
    "p2"
    "p4"
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
    "gnuplot-lua-tikz")
   (LaTeX-add-amsthm-newtheorems
    "plm"))
 :latex)

