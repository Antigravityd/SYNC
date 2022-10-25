(TeX-add-style-hook
 "hw7"
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
    "amsthm"
    "tikz"
    "minted"
    "physics"
    "siunitx")
   (TeX-add-symbols
    "lcm")
   (LaTeX-add-environments
    "plm"
    "lem"))
 :latex)

