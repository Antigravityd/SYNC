(TeX-add-style-hook
 "hw5"
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
    "tikz-feynman"
    "minted"
    "physics"
    "siunitx"
    "mhchem")
   (LaTeX-add-amsthm-newtheorems
    "plm"))
 :latex)

