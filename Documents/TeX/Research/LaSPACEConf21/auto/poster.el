(TeX-add-style-hook
 "poster"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("beamerposter" "orientation=landscape" "size=custom" "width=48" "height=36" "scale=0.5" "debug") ("textpos" "absolute" "overlay") ("adjustbox" "export")))
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "beamerposter"
    "graphicx"
    "textpos"
    "amsmath"
    "siunitx"
    "adjustbox"
    "tikz"
    "xcolor"
    "ragged2e"
    "mathrsfs"
    "array"
    "lmodern"
    "bm"
    "enumitem"
    "caption"
    "subcaption"
    "verbatim"
    "tabu"))
 :latex)

