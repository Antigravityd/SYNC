;;; Phits-mode.el --- Summary
;;; Commentary:
;;

;;; Code:

(require 'font-lock)
(require 'align)


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
    (modify-syntax-entry ?\# "<"  st) ;; allowing "#" as a comment mark will break on some [Cell] sections
    (modify-syntax-entry ?\n ">"  st)
    (modify-syntax-entry ?%  "<"  st)
    st)
  "Syntax table used for .inp files.")

;; Line continuation not handled at all

(setq phits-comment-regexp "^ \\{,4\\}c.*$") ;; for the archaic fixed-format comments
(setq phits-section-regexp "^ \\{,4\\}\\[.*\\] *$")
(setq phits-parameter-regexp "\\s-*\\([[:alnum:]<>-]+\\)\\((.*)\\|\\[.*\\]\\)?\\s-*=")
(setq phits-label-regexp "^\\s-*\\(\\w*\\):")

(setq phits-function-regexp
      (regexp-opt
       '("float" "int" "abs""exp" "log" "log10" "max" "min" "mod" "nint" "sign" "sqrt" "acos" "asin" "atan"
	 "atan2" "cos" "cosh" "sin" "sinh" "tan" "tanh" "pi")
       'words))


(setq phits-particle-regexp
      (concat "\\(\\<\\(\\([0-9]\\{,3\\}"
	      (regexp-opt '("H" "He" "Li" "Be" "B" "C" "N" "O" "F" "Ne" "Na" "Mg" "Al" "Si" "P" "S" "Cl" "Ar"
			    "K" "Ca" "Sc" "Ti" "V" "Cr" "Mn" "Fe" "Co" "Ni" "Cu" "Zn" "Ga" "Ge" "As" "Se" "Br" "Kr"
			    "Rb" "Sr" "Y" "Zr" "Nb" "Mo" "Tc" "Ru" "Rh" "Pd" "Ag" "Cd" "In" "Sn" "Sb" "Te" "I" "Xe"
			    "Cs" "Ba" "La" "Ce" "Pr" "Nd" "Pm" "Sm" "Eu" "Gd" "Tb" "Dy" "Ho" "Er" "Tm" "Yb" "Lu"
			    "Hf" "Ta" "W" "Re" "Os" "Ir" "Pt" "Au" "Hg" "Tl" "Pb" "Bi" "Po" "At" "Rn" "Fr" "Ra" "Ac"
 			    "Th" "Pa" "U" "Np" "Pu" "Am" "Cm" "Bk" "Cf" "Es" "Fm" "Md" "No" "Lr" "Rf" "Db" "Sg" "Bh"
			    "Hs" "Mt" "Ds" "Rg" "Cn" "Nh" "Fl" "Mc" "Lv" "Ts" "Og")
			  t)
	      "\\)\\|\\("
	      (regexp-opt '("H" "He" "Li" "Be" "B" "C" "N" "O" "F" "Ne" "Na" "Mg" "Al" "Si" "P" "S" "Cl" "Ar"
			    "K" "Ca" "Sc" "Ti" "V" "Cr" "Mn" "Fe" "Co" "Ni" "Cu" "Zn" "Ga" "Ge" "As" "Se" "Br" "Kr"
			    "Rb" "Sr" "Y" "Zr" "Nb" "Mo" "Tc" "Ru" "Rh" "Pd" "Ag" "Cd" "In" "Sn" "Sb" "Te" "I" "Xe"
			    "Cs" "Ba" "La" "Ce" "Pr" "Nd" "Pm" "Sm" "Eu" "Gd" "Tb" "Dy" "Ho" "Er" "Tm" "Yb" "Lu"
			    "Hf" "Ta" "W" "Re" "Os" "Ir" "Pt" "Au" "Hg" "Tl" "Pb" "Bi" "Po" "At" "Rn" "Fr" "Ra" "Ac"
 			    "Th" "Pa" "U" "Np" "Pu" "Am" "Cm" "Bk" "Cf" "Es" "Fm" "Md" "No" "Lr" "Rf" "Db" "Sg" "Bh"
			    "Hs" "Mt" "Ds" "Rg" "Cn" "Nh" "Fl" "Mc" "Lv" "Ts" "Og")
			  t)
	      "\\(-[0-9]\\{,3\\}\\)\\)\\|" ;; collisions common for "all," "other," and "pi"
	      (regexp-opt '("all" "proton" "neutron" "pion0"  "kaon0" "other" "electron" "positron"  "photon"
			    "gamma" "deuteron" "triton" "3he" "alpha" "nucleus" "pi")
			  t)
	      "\\)\\>\\|pion\\+\\|pion-\\|kaon\\+\\|kaon-\\|muon\\+\\|muon-\\)"))


(setq phits-special-regexp
      (concat "\\<\\(mat\\[.*\\]\\|mt?[0-9]+\\|tr[0-9]+\\|"
	      (regexp-opt '("like" "but" "p" "px" "py" "pz" "so" "s" "sx" "sy" "sz" "c/x" "c/y" "c/z" "cx" "cy" "cz"
			    "k/x" "k/y" "k/z" "kx" "ky" "kz" "sq" "gq" "tx" "ty" "tz" "box" "rpp" "sph" "rcc" "rhp"
			    "hex" "rec" "trc" "ell" "wed" "vol" "tmp" "trcl" "u" "lat" "fill" "mat" "rho")
			  t)
	      "\\)\\>"))


(defvar phits-archaic-comment-font-lock
      (cons phits-comment-regexp  font-lock-comment-face))

(defvar phits-section-font-lock
      (cons phits-section-regexp font-lock-warning-face))

(defvar phits-parameter-font-lock
      (list phits-parameter-regexp 1 font-lock-variable-name-face))

(defvar phits-label-font-lock
      (list phits-label-regexp 1 font-lock-keyword-face))

(defvar phits-particle-font-lock
      (list phits-particle-regexp 1 font-lock-function-name-face))

(defvar phits-function-font-lock
      (cons phits-function-regexp  font-lock-builtin-face))

;; TODO: solve collision of p, s, and u with the isotopes above
(defvar phits-special-font-lock
      (cons phits-special-regexp  font-lock-type-face))


(setq phits-font-lock
      (list phits-archaic-comment-font-lock
	    phits-section-font-lock
	    phits-parameter-font-lock
	    phits-label-font-lock
	    phits-particle-font-lock
	    phits-function-font-lock
	    phits-special-font-lock))

(defun phits-indent-line ()
  (align-region nil
		nil
		'group
		'((phits-parameter
		   (regexp  . "^\\(\\s-*[[:alnum:]<>()-]+\\(?:(.*)\\|\\[.*\\]\\)?\\)\\(\\s-*\\)=\\(\\s-*\\)")
		   (group   . (1 2 3))
		   (justify . t))
		  (phits-label
		   (regexp  . "^\\(\\s-*\\)\\w+\\(\\s-*\\):\\(\\s-*\\)")
		   (group   . (1 3))
		   (spacing . (0 1)))
		  (phits-material ;; Possibly collapse everything here too?
		   (regexp  . "^\\(\\s-*\\)\\w+\\s-*\\[.*\\]")
		   (spacing . 0))
		  ;; TODO (phits-grid)
		  (phits-comment-normal
		   (regexp  . "\\(\\s-*\\)[#!$%]")))
		nil))

;;(defvar phits-source-dir "~/PHITS/phits326A/phits"
;;"The directory unpacked from the archive obtained from JAEA that contains the PHITS source files.")

(defvar phits-command "phits "
  "The shell command through which PHITS is to be run.  Should be everything but the filename (which will be placed at the end), and contain a trailing space.")

(defun run-phits ()
  "Run phits-command on the current buffer asynchronously, displaying output in a new buffer and placing output files in the same directory as the current buffer's file."
  (interactive)
  (let ((default-directory (file-name-directory buffer-file-name)))
    (async-shell-command (concat phits-command buffer-file-name))))

(defun phits-view-results ()
  "Open summary of tally results in external image viewer.")

;;;###autoload
(define-derived-mode phits-mode prog-mode "PHITS Input"
  "Testing mode I whipped up that's inspired in small part by https://github.com/kbat/mc-tools/blob/master/mctools/phits/phits-mode.el"
  :syntax-table phits-mode-syntax-table
  (setq-local font-lock-defaults `(,phits-font-lock nil t)) ;; TODO: last arg in the quasiquote sets case-insensitivity; re-enabling it for the particles would be amazing so it doesn't flag the word "is" as a particle &c
  (setq-local indent-line-function #'phits-indent-line))

(provide 'phits-mode)

;;; phits-mode.el ends here
