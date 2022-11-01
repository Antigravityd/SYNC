(TeX-add-style-hook
 "proposal"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper") ("fontsize" "fontsize=12pt")))
   (TeX-run-style-hooks
    "latex2e"
    "profiling/hist"
    "article"
    "art10"
    "geometry"
    "tgpagella"
    "amsmath"
    "siunitx"
    "fontsize"
    "tikz"
    "gnuplot-lua-tikz"
    "wrapfig")
   (LaTeX-add-labels
    "sec:project-narrative"
    "fig:mcruntime"))
 :latex)

