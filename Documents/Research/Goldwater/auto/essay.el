(TeX-add-style-hook
 "essay"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt" "arial" "letterpaper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=1in")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "helvet"
    "geometry"
    "cite"
    "fancyhdr")
   (LaTeX-add-bibliographies
    "ref"))
 :latex)

