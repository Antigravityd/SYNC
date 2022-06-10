(TeX-add-style-hook
 "autodidact-beamer"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "aspectratio=169")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("textpos" "absolute" "overlay") ("siunitx" "binary-units=true")))
   (add-to-list 'LaTeX-verbatim-environments-local "semiverbatim")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "pgfpages"
    "textpos"
    "tcolorbox"
    "siunitx"
    "mathtools"
    "amsmath"
    "amssymb"
    "nicefrac"
    "bm"
    "array"
    "tabularx"
    "adjustbox"
    "longtable"
    "ltablex"
    "multicol"
    "multirow"
    "booktabs"
    "threeparttable"
    "colortbl"
    "csquotes"
    "pifont"
    "caption"
    "subcaption"
    "appendixnumberbeamer")
   (TeX-add-symbols
    '("maketitle" ["argument"] 0)
    '("copyrightnotice" 1)
    "cutoc"
    "newline"
    "opaqueness"
    "cover")
   (LaTeX-add-xcolor-definecolors
    "bg"
    "bg-alt"
    "comment"
    "variable"
    "keyword"
    "number"
    "string"
    "builtin"
    "light"
    "beamer@freeze\\the\\beamer@coveringdepth")
   (LaTeX-add-tcolorbox-newtcolorboxes
    '("cublock" "1" "[" ""))
   (LaTeX-add-tcolorbox-tcbuselibraries
    "skins")
   (LaTeX-add-array-newcolumntypes
    "L"
    "C"
    "R"))
 :latex)

