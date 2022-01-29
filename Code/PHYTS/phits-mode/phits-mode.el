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

(defvar phits-comment-regexp "^ \\{,4\\}c.*$")
(defvar phits-section-regexp "^ \\{,4\\}\\[.*\\] *$")
(defvar phits-parameter-regexp "^\\([[:alnum:]<>-]+\\)\\((.*)\\|\\[.*\\]\\)?[[:blank:]]*=") ;; TODO: removed blanks from beginning; check if worked.
(defvar phits-label-regexp "^\\(\\w*\\):")

(defvar phits-function-regexp
  (regexp-opt
   '("float" "int" "abs""exp" "log" "log10" "max" "min" "mod" "nint" "sign" "sqrt" "acos" "asin" "atan"
     "atan2" "cos" "cosh" "sin" "sinh" "tan" "tanh" "pi")
   'words))

(defvar phits-particle-regexp
  (concat "\\<\\([0-9]\\{,3\\}"
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
	  "\\)\\>"))

(defvar phits-special-regexp
  (concat "\\<\\(mat\\[ *[0-9]+ *\\]\\|mt?[0-9]+\\|tr[0-9]+\\|"
	  (regexp-opt '("like" "but" "p" "px" "py" "pz" "so" "s" "sx" "sy" "sz" "c/x" "c/y" "c/z" "cx" "cy" "cz"
			"k/x" "k/y" "k/z" "kx" "ky" "kz" "sq" "gq" "tx" "ty" "tz" "box" "rpp" "sph" "rcc" "rhp"
			"hex" "rec" "trc" "ell" "wed" "vol" "tmp" "trcl" "u" "lat" "fill" "mat" "rho") t)
	  "\\)\\>"
	  "\\|^\\( *[[:alpha:]] *\\)+"))

(defvar phits-delimeter-param-regexp "\\(<[[:alnum:]]>\\)")

(defun align-to-equals (begin end)
  "Align region to equal signs."
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))

(defun phits-line-matching (regexp)
  "Return t if current line will match REGEXP, otherwise nil."
  (when (string-match regexp
		    (buffer-substring-no-properties
		     (line-beginning-position)
		     (line-end-position)))
    t))

(defun phits-get-line ()
  "Return current line as a string."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun phits-prev-line ()
  "Return previous line as a string."
  (forward-line -1)
  (phits-get-line))

(defun phits-match-length (regexp string)
  "Return length of the match of REGEXP in STRING."
  (length (progn (string-match regexp string) (match-string 1 string))))

(defun phits-count-previous-parameters ()
  "Return the number of parameter lines in a row above the current one"

)

(defun phits-parameter-align ()
  "Align current equals sign with equals sign above.  If there's no space, align every parameter above to accomodate current.  If there's no equals sign above, left-justify and add one space before equals sign."
  (let ((word-length (phits-match-length "<?\\(\\w\\|-\\)+>?" (phits-get-line)))
	(equals-idx (string-match "=" (phits-get-prev-line))))
    (if equals-idx
	(if (< word-length equals-idx)
	    (progn
	      (beginning-of-line)
	      (re-search-forward "^[[:blank:]]*" (line-end-position))
	      (replace-match (apply 'concat (make-list (- equals-idx word-length 1) " ")))
	      (re-search-forward "[[:blank:]]*=")
	      (replace-match " ="))
	  (progn
	    (setq max-above-word-len word-length)
	    (while max-above-word-len)))
      (progn
	(beginning-of-line)
	(re-search-forward "^[[:blank:]]*")
	(replace-match "")))))

(defun phits-indent-line (&optional indent)
  "Indent current line as a PHITS .inp file."
  (interactive)
  (cond ((phits-line-matching phits-section-regexp)
	 (replace-regexp "^[[:blank:]]+" "" nil (line-beginning-position) (line-end-position)))
	((phits-line-matching phits-parameter-regexp)
	 (;; TODO: complicated enough for a separate function call.
	  ))
	((phits-line-matching phits-))))


(defvar phits-archaic-comment-font-lock
  '(phits-comment-regexp . font-lock-comment-face)
  "Handle F77-style fixed-form comments.")

(defvar phits-section-font-lock
  '(phits-section-regexp . font-lock-warning-face)
  "Highlight section header (anything inside brackets alone on a line that starts in the first 4 columns).")

(defvar phits-parameter-font-lock
  `(phits-parameter-regexp   ,(list 1  font-lock-variable-name-face))
  "Highlight anything on the left hand side of an equals sign that is also the first word on a line.")

(defvar phits-label-font-lock
  `(phits-label-regexp  ,(list 1 font-lock-keyword-face))
  "Highlight directives that look like C label statements, e.g. ^set: varable.")

;; TODO: currently matches both 208Pb and Pb-208 syntaxes at once, e.g. 208Pb-208
(defvar phits-particle-font-lock
  '(phits-particle-regexp . font-lock-function-name-face)
  "Highlight any isotopes presented in the correct syntax, as well as any supported special particles.")

(defvar phits-function-font-lock
  '(phits-function-regexp . font-lock-builtin-face)
  "Highlight supported mathematical functions and constants.")

;; TODO: solve collision of p, s, and u with the isotopes above
(defvar phits-special-font-lock
  '(phits-special-regexp . font-lock-type-face)
  "Highlight special names in material and surface sections, as well as any header lines of array-like sections.")


(defvar phits-font-lock
  (list phits-archaic-comment-font-lock
	phits-section-font-lock
	phits-parameter-font-lock
	phits-label-font-lock
	phits-particle-font-lock
	phits-function-font-lock
	phits-special-font-lock))

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
  (setq-local abbrev-all-caps t)
  (setq-local font-lock-defaults `(,phits-font-lock nil t))
  ;; TODO  (setq-local indent-line-function #'phits-indent-line)
  )

;; (add-to-list 'auto-mode-alist '(".inp\\'" phits-mode))
;; (add-to-list 'auto-mode-alist '(".out\\'" phits-mode))



(provide 'phits-mode)

;;; phits-mode.el ends here
