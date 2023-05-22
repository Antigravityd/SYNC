(TeX-add-style-hook
 "hw8"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (TeX-run-style-hooks
    "latex2e"
    "plot2"
    "article"
    "art10"
    "geometry"
    "amsmath"
    "siunitx"
    "tikz"
    "gnuplot-lua-tikz"
    "minted"))
 :latex)

