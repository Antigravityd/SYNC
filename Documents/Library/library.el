;;; library.el --- Forms control file for library management  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Duncan W

;; Author: Duncan W <dnw@functorial.xyz>
;; Keywords:

(setq forms-file "library.tsv"
      forms-format-list '("====== Library Book ======\n\n"
			  ;; Basic info.
			  "\"" title "\""
			  " by " author ".\n"
			  "Description:\n" description "\n"
			  "\n\n"
			  ;; Publication information.
			  "Edition " edition
			  (if (string-equal (nth volume forms-fields) "")
			      " "
			    ", volume ")
			  volume
			  (if (string-equal (nth volume forms-fields) "")
			      ""
			    "; ")

			  "published by " publisher ", "

			  (if (string-equal (nth series forms-fields) "")
			      ""
			    "in the ")
			  series
			  (if (string-equal (nth series forms-fields) "")
			      ""
			    " series, ")

			  "in " year "."
			  "\n\n"
			  ;; Physical information.
			  "Hardcover: " hardcover? "\n"
			  "Jacket: " jacket? "\n"
			  condition " condition; "
			  page-count " pages.\n"
			  "Last location: " location
			  "\n\n"
			  ;; Classification information.
			  "ISBN-13: " isbn-13
			  "\nLC Classification: " lc
			  "\nLibgen MD5: " md5
			  "\n\n"
			  ;; My relationship with it.
			  "Reading status " status ": "
			  (cond ((string-equal (nth status forms-fields) "0") "On the wishlist.")
				((string-equal (nth status forms-fields) "1") "Unread.")
				((string-equal (nth status forms-fields) "2") "Skimmed a little.")
				((string-equal (nth status forms-fields) "3") "Read at surface level.")
				((string-equal (nth status forms-fields) "4") "Read a good chunk in depth.")
				((string-equal (nth status forms-fields) "5") "Completely mastered.")
				(t ""))

			  "\nBought from " purchase-from
			  ", " purchase-date ".\n"
			  "Opinion:\n" opinion)
      forms-number-of-fields (forms-enumerate
			      '(title
				author
				publisher
				series
				edition
				volume
				year
				page-count
				condition
			        hardcover?
				jacket?
				location
				isbn-13
				lc
				md5
				status
				purchase-from
				purchase-date
				opinion
				description))
      forms-field-sep "\t"
      forms-read-only nil
      forms-multi-line ""
      forms-read-file-filter nil
      forms-write-file-filter nil
      forms-new-record-filter nil
      forms-insert-after nil
      forms-check-number-of-fields nil)
