(TeX-add-style-hook
 "sicp.org"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("color" "usenames") ("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "color"
    "inputenc"
    "fontenc"
    "graphicx"
    "ulem"
    "amsmath"
    "amssymb"))
 :latex)

