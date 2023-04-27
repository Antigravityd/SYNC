(TeX-add-style-hook
 "presentation"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "10pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("caption" "labelformat=empty")))
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "graphicx"
    "subcaption"
    "siunitx"
    "amsmath"
    "physics"
    "tikz-cd"
    "caption")
   (LaTeX-add-environments
    "mydef"
    "thm"))
 :latex)

