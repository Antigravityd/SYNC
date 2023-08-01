(TeX-add-style-hook
 "sicp"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper") ("minted" "outputdir=cache")))
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
    "minted"
    "physics")
   (LaTeX-add-amsthm-newtheorems
    "plm"))
 :latex)

