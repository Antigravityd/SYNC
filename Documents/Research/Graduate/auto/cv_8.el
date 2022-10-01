(TeX-add-style-hook
 "cv_8"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("classicthesis" "nochapters") ("currvita" "LabelsAligned")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "scrartcl"
    "scrartcl10"
    "tgpagella"
    "classicthesis"
    "currvita"
    "hyperref")
   (TeX-add-symbols
    '("Description" 1)
    '("NewEntry" 3)
    '("MarginText" 1))
   (LaTeX-add-lengths
    "datebox"))
 :latex)

