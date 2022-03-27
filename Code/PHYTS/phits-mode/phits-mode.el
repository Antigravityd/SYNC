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

(setq phits-comment-regexp "^ \\{,4\\}c.*$")
(setq phits-section-regexp "^ \\{,4\\}\\[.*\\] *$")
(setq phits-parameter-regexp "\\([[:alnum:]<>-]+\\)\\((.*)\\|\\[.*\\]\\)?[[:blank:]]*=")
(defvar phits-label-regexp "^[[:blank:]]\\(\\w*\\):")

(setq phits-function-regexp
      (regexp-opt
       '("float" "int" "abs""exp" "log" "log10" "max" "min" "mod" "nint" "sign" "sqrt" "acos" "asin" "atan"
	 "atan2" "cos" "cosh" "sin" "sinh" "tan" "tanh" "pi")
       'words))

(setq phits-particle-regexp
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

(setq phits-special-regexp
      (concat "\\<\\(mat\\[ *[0-9]+ *\\]\\|mt?[0-9]+\\|tr[0-9]+\\|"
	      (regexp-opt '("like" "but" "p" "px" "py" "pz" "so" "s" "sx" "sy" "sz" "c/x" "c/y" "c/z" "cx" "cy" "cz"
			    "k/x" "k/y" "k/z" "kx" "ky" "kz" "sq" "gq" "tx" "ty" "tz" "box" "rpp" "sph" "rcc" "rhp"
			    "hex" "rec" "trc" "ell" "wed" "vol" "tmp" "trcl" "u" "lat" "fill" "mat" "rho") t)
	      "\\)\\>"
	      "\\|^\\( *[[:alpha:]] *\\)+")) ;; should probably remove the last p-exp in the regex



(defun align-to-equals (begin end)
  "Align region (between BEGIN and END) to equal signs."
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
  (save-excursion
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position))))

(defun phits-prev-line ()
  "Return previous line as a string."
  (save-excursion
    (forward-line -1)
    (phits-get-line)))

(defun phits-match-length (regexp string &optional group)
  "Return length of the match of REGEXP in STRING.
GROUP is a number representing which parenthesized group to select."
  (length (progn (unless group (setq group 0))
		 (string-match regexp string)
		 (match-string group string))))

(defun phits-parameter-block-start ()
  "Return the point value at the start of the line on which the current contiguous block of parameter statements begins."
  (save-excursion
    (while (phits-line-matching phits-parameter-regexp) (forward-line -1))
    (point)))


(defun phits-parameter-align-region (begin end)
  "Align.el was some combination of too convoluted and too poorly documented.  Aligns any parameters between BEGIN and END."
  (save-excursion
    (goto-char begin)
    (let ((longest-name-len 0)
	  (longest-value-len 0))
      (while (< (point) end)
	(beginning-of-line)
	(let ((current-name-len
	       (phits-match-length "^[[:blank:]]*\\([[:alnum:]<>()-]*\\)" (phits-get-line) 1))
	      (current-value-len
	       (phits-match-length "=[[:blank:]]*\\(\\S-*\\)" (phits-get-line) 1)))

	  (if (< longest-name-len current-name-len)
	      (setq longest-name-len current-name-len))
	  (if (< longest-value-len current-value-len)
		(setq longest-value-len current-value-len)))

	(forward-line))
      (goto-char begin)
      (let* ((eq-column (1+ longest-name-len))
	     (rightmost-column (+ eq-column longest-value-len 1)))
	(while (< (point) end)

	  (let ((name-length (phits-match-length "^[[:blank:]]*\\([[:alnum:]<>()-]*\\)" (phits-get-line) 1))
		(value-length (phits-match-length "=[[:blank:]]*\\(\\S-*\\)" (phits-get-line) 1)))

	    (beginning-of-line)
	    (re-search-forward "^[[:blank:]]*" (line-end-position))
	    (replace-match (apply 'concat (make-list (- eq-column name-length) " ")))
	    (beginning-of-line)
	    (re-search-forward "[[:blank:]]*=" (line-end-position))
	    (replace-match " =")
	    (beginning-of-line)
	    (re-search-forward "=[[:blank:]]*" (line-end-position))
	    (replace-match (concat "=" (apply 'concat (make-list (- rightmost-column value-length eq-column) " "))))
	    (forward-line)
	    (beginning-of-line)))))))

(defun phits-parameter-align-line () ;; probably doesn't handle line continuation well...
  "Align current equals sign with equals sign above.  If there's no space, align every parameter above to accomodate current.  If there's no equals sign above, left-justify and add one space before equals sign."
  (let ((name-length (phits-match-length "^[[:blank:]]*\\([[:alnum:]<>()-]*\\)" (phits-get-line) 1))
	(value-length  (phits-match-length "=[[:blank:]]*\\(\\S-*\\)" (phits-get-line) 1))
	(prev-value-length  (phits-match-length "=[[:blank:]]*\\(\\S-*\\)" (phits-prev-line) 1))
;;	(equals-idx (string-match "=" (phits-get-line)))
	(prev-equals-idx (string-match "=" (phits-prev-line)))
	(prev-value-gap-length (phits-match-length "=[[:blank:]]*" (phits-prev-line))))
    (if prev-equals-idx
	(if (< name-length prev-equals-idx)
	    (progn
	      (beginning-of-line)
	      (re-search-forward "^[[:blank:]]*" (line-end-position))
	      (replace-match (apply 'concat (make-list (- prev-equals-idx name-length 1) " ")))
	      (re-search-forward "[[:blank:]]*=")
	      (replace-match " =")
	      (beginning-of-line)
	      (re-search-forward "=[[:blank:]]*" (line-end-position))
	      (replace-match (concat "=" (apply 'concat (make-list (+ prev-value-gap-length
								      (- prev-value-length
									 value-length
									 1))
								      " ")))))
	  (phits-parameter-align-region (save-excursion
					  (while (phits-line-matching phits-parameter-regexp)
					    (forward-line -1))
					  (forward-line)
					  (point))
					(line-end-position)))
      (progn
	(beginning-of-line)
	(re-search-forward "^[[:blank:]]*")
	(replace-match "")
	(beginning-of-line)
	(re-search-forward "[[:blank:]]*=")
	(replace-match " =")))))


(defun phits-grid-align ()
  "Align a gridlike region to columns as best as can be managed.")



(defun phits-indent-line ()
  "Indent current line as a PHITS .inp file."
  (cond ((or (phits-line-matching phits-label-regexp) (phits-line-matching phits-section-regexp))
	 (beginning-of-line)
	 (re-search-forward "^[[:blank:]]*" (line-end-position))
	 (replace-match ""))
	((phits-line-matching phits-parameter-regexp)
	 (phits-parameter-align-line))))


(setq phits-archaic-comment-font-lock
      (cons phits-comment-regexp  font-lock-comment-face))
;; "Handle F77-style fixed-form comments.")

(setq phits-section-font-lock
      (cons phits-section-regexp font-lock-warning-face))

(setq phits-parameter-font-lock
      (list phits-parameter-regexp 1 font-lock-variable-name-face))

(setq phits-label-font-lock
      (list phits-label-regexp 1 font-lock-keyword-face))

;; TODO: currently matches both 208Pb and Pb-208 syntaxes at once, e.g. 208Pb-208
(setq phits-particle-font-lock
      (cons phits-particle-regexp  font-lock-function-name-face))

(setq phits-function-font-lock
      (cons phits-function-regexp  font-lock-builtin-face))

;; TODO: solve collision of p, s, and u with the isotopes above
(setq phits-special-font-lock
      (cons phits-special-regexp  font-lock-type-face))


(setq phits-font-lock
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
  (setq-local indent-line-function #'phits-indent-line))

;; (add-to-list 'auto-mode-alist '(".inp\\'" phits-mode))
;; (add-to-list 'auto-mode-alist '(".out\\'" phits-mode))



(provide 'phits-mode)

;;; phits-mode.el ends here
