;;; Phits-mode.el --- Summary
;;; Commentary:
;; Manual-copying borrowed from  https://github.com/kbat/mc-tools/blob/master/mctools/phits/phits-mode.el

;;; Code:

(require 'font-lock)


(defvar phits-mode-syntax-table
  (let ((st (make-syntax-table))) ;; borrowed from fortran-mode.el syntax table
    (modify-syntax-entry ?$  "<"  st)
    (modify-syntax-entry ?\; "."  st)
    (modify-syntax-entry ?\r " "  st)
    (modify-syntax-entry ?+  "."  st)
    (modify-syntax-entry ?-  "."  st)
    (modify-syntax-entry ?=  "."  st)
    (modify-syntax-entry ?*  "."  st)
    (modify-syntax-entry ?/  "."  st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?.  "_"  st)
    (modify-syntax-entry ?_  "_"  st)
    (modify-syntax-entry ?\! "<"  st)
    (modify-syntax-entry ?\# "<"  st)
    (modify-syntax-entry ?\n ">"  st)
    (modify-syntax-entry ?%  "<"  st)
    st)
  "Syntax table used for .inp files.")


;;(defun align-to-equals (begin end)
;;  "Align region to equal signs"
;;   (interactive "r")
;;   (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))



(defvar phits-archaic-comment-font-lock
  '("^ \\{,4\\}c.*$" . font-lock-comment-face)
  "Handle F77-style fixed-form comments.")

(defvar phits-section-font-lock
  '("^ \\{,4\\}\\[.*\\] *$" . font-lock-warning-face)
  "Highlight section header (anything inside brackets alone on a line that starts in the first 4 columns).")

(defvar phits-parameter-font-lock
  `("^ *\\([[:alnum:]<>-]+\\)\\( *\\|(.*)\\|\\[.*\\]\\) *="   ,(list 1  font-lock-variable-name-face))
  "Highlight anything on the left hand side of an equals sign that is also the first word on a line")

(defvar phits-label-font-lock
  `("^\\(\\w*\\):"  ,(list 1 font-lock-keyword-face))
  "Highlight directives that look like C label statements, e.g. ^set: varable.")

;; TODO: currently matches both 208Pb and Pb-208 syntaxes at once
(defvar phits-particle-font-lock
  `(,(concat "\\<\\([0-9]\\{,3\\}"
	     (regexp-opt '("H" "He" "Li" "Be" "B" "C" "N" "O" "F" "Ne" "Na" "Mg" "Al" "Si" "P" "S" "Cl" "Ar"
			   "K" "Ca" "Sc" "Ti" "V" "Cr" "Mn" "Fe" "Co" "Ni" "Cu" "Zn" "Ga" "Ge" "As" "Se" "Br" "Kr"
			   "Rb" "Sr" "Y" "Zr" "Nb" "Mo" "Tc" "Ru" "Rh" "Pd" "Ag" "Cd" "In" "Sn" "Sb" "Te" "I" "Xe"
			   "Cs" "Ba" "La" "Ce" "Pr" "Nd" "Pm" "Sm" "Eu" "Gd" "Tb" "Dy" "Ho" "Er" "Tm" "Yb" "Lu"
			   "Hf" "Ta" "W" "Re" "Os" "Ir" "Pt" "Au" "Hg" "Tl" "Pb" "Bi" "Po" "At" "Rn" "Fr" "Ra" "Ac"
 			   "Th" "Pa" "U" "Np" "Pu" "Am" "Cm" "Bk" "Cf" "Es" "Fm" "Md" "No" "Lr" "Rf" "Db" "Sg" "Bh"
			   "Hs" "Mt" "Ds" "Rg" "Cn" "Nh" "Fl" "Mc" "Lv" "Ts" "Og")
			 t)
	     "\\(-[0-9]\\{,3\\}\\)?\\(.[0-9]+[[:alpha:]]\\)?\\|"
	     (regexp-opt '("all" "proton" "neutron" "pion+" "pion0" "pion-" "muon+" "muon-" "kaon+" "kaon0" "kaon-"
			   "other" "electron" "positron"  "photon" "gamma" "deuteron" "triton" "3he" "alpha" "nucleus" "pi")
			 t)
	     "\\)\\>") . font-lock-function-name-face)
  "Highlight any isotopes presented in the correct syntax, as well as any supported special particles.")

(defvar phits-function-font-lock
  `(,(regexp-opt
      '("float" "int" "abs""exp" "log" "log10" "max" "min" "mod" "nint" "sign" "sqrt" "acos" "asin" "atan"
	"atan2" "cos" "cosh" "sin" "sinh" "tan" "tanh" "pi")
      'words)  . font-lock-builtin-face)
  "Highlight supported mathematical functions and constants.")

;; TODO: solve collision of p, s, and u with the isotopes above
(defvar phits-special-font-lock
  `(,(concat "\\<\\(mat\\[ *[0-9]+ *\\]\\|mt?[0-9]+\\|tr[0-9]+\\|"
	     (regexp-opt '("like" "but" "p" "px" "py" "pz" "so" "s" "sx" "sy" "sz" "c/x" "c/y" "c/z" "cx" "cy" "cz"
			   "k/x" "k/y" "k/z" "kx" "ky" "kz" "sq" "gq" "tx" "ty" "tz" "box" "rpp" "sph" "rcc" "rhp"
			   "hex" "rec" "trc" "ell" "wed" "vol" "tmp" "trcl" "u" "lat" "fill" "mat" "rho") t)
	     "\\)\\>"
	     "\\|^\\( *[[:alpha:]] *\\)+") . font-lock-type-face)
  "Highlight special names in material and surface sections, as well as any header lines of array-like sections")


(defvar phits-font-lock
  (list phits-archaic-comment-font-lock
	   phits-section-font-lock
	   phits-parameter-font-lock
	   phits-label-font-lock
	   phits-particle-font-lock
	   phits-function-font-lock
	   phits-special-font-lock))


;;;###autoload
(define-derived-mode phits-mode prog-mode "PHITS Input"
  "Testing mode I whipped up that's inspired in small part by https://github.com/kbat/mc-tools/blob/master/mctools/phits/phits-mode.el"
  :syntax-table phits-mode-syntax-table
  (setq-local abbrev-all-caps t)
  (setq-local font-lock-defaults `(,phits-font-lock nil t))
  (setq-local indent-line-function #'phits-indent-line))

;; (add-to-list 'auto-mode-alist '(".inp\\'" phits-mode))

(provide 'phits-mode)

;;; phits-mode.el ends here
