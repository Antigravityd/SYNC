(TeX-add-style-hook
 "chap1"
 (lambda ()
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (LaTeX-add-labels
    "phil:suppress"
    "phil:nothing"))
 :latex)

