(TeX-add-style-hook
 "hw7"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "geometry"
    "amsmath"
    "amssymb"
    "siunitx"
    "graphicx"
    "minted"
    "tikz"
    "tikz-gnuplot")
   (LaTeX-add-environments
    '("gnuplot" LaTeX-env-args ["argument"] 0)
    '("annotationimage" LaTeX-env-args ["argument"] 2)))
 :latex)

