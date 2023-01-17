(TeX-add-style-hook
 "aia"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (TeX-run-style-hooks
    "latex2e"
    "front/preface"
    "front/how2read"
    "chap1/chap1"
    "chap2/chap2"
    "chap3/chap3"
    "chap4/chap4"
    "app1/app1"
    "app2/app2"
    "book"
    "bk10"
    "geometry"
    "tgpagella"
    "amsmath"
    "amssymb"
    "amsthm"
    "tikz"
    "minted"
    "physics"
    "siunitx"
    "cite"
    "appendix")
   (TeX-add-symbols
    "hooktwoheadrightarrow")
   (LaTeX-add-environments
    "philosophy")
   (LaTeX-add-bibliographies
    "ref")
   (LaTeX-add-counters
    "philosophy")
   (LaTeX-add-amsthm-newtheorems
    "definition"
    "thm"
    "lem"
    "cor"
    "prob"
    "exam"))
 :latex)

