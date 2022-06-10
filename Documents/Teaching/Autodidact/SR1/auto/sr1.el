(TeX-add-style-hook
 "sr1"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "10pt")))
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "graphicx"
    "subfig"
    "siunitx"
    "amsmath")
   (LaTeX-add-environments
    "mydef"))
 :latex)

