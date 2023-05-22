(use-modules (guix packages)
	     (guix download)
	     (guix git-download)
	     (guix build-system python)
	     (guix build utils)
	     (guix licenses)
	     (guix gexp)
	     (ice-9 match)
	     ((srfi srfi-1) #:hide (zip))
	     (gnu packages)
	     (gnu packages tex)
	     (gnu packages check)
	     (gnu packages python-build)
	     (gnu packages python-xyz))


(package
  (name "python-hypothesis")
  (version "6.75.3")
  (source (origin
            (method url-fetch)
            (uri (pypi-uri "hypothesis" version))
            (sha256
             (base32
              "1xl3wpqak0f40vsqhzwvrrf5nn7vlikd490nb23r1ibs1awavk8m"))))

  (build-system python-build-system)
  (arguments
   ;; XXX: Tests are not distributed with the PyPI archive.
   '(#:tests? #f))
  (propagated-inputs
   `(("python-attrs" ,python-attrs-bootstrap)
     ("python-sortedcontainers" ,python-sortedcontainers)
     ("python-pytest" ,python-pytest)
     ("python-exceptiongroup" ,python-exceptiongroup)))
  (synopsis "Library for property based testing")
  (description "Hypothesis is a library for testing your Python code against a
much larger range of examples than you would ever want to write by hand.  It’s
based on the Haskell library, Quickcheck, and is designed to integrate
seamlessly into your existing Python unit testing work flow.")
  (home-page "https://hypothesis.works/")
  (license mpl2.0))

;; (package
;;   (name "python-hypothesis")
;;   (version "6.75.3")
;;   (source (origin
;;             (method url-fetch)
;;             (uri (pypi-uri "hypothesis" version))
;;             (sha256
;;              (base32
;;               "1xl3wpqak0f40vsqhzwvrrf5nn7vlikd490nb23r1ibs1awavk8m"))))
;;   (build-system python-build-system)
;;   (arguments
;;    '(#:tests? #f))
;;   (propagated-inputs (list python-attrs python-exceptiongroup
;;                            python-sortedcontainers
;; 			   python-pytest))
;;   (native-inputs (list python-pytest))
;;   (synopsis "Library for property based testing")
;;   (description "Hypothesis is a library for testing your Python code against a
;; much larger range of examples than you would ever want to write by hand.  It’s
;; based on the Haskell library, Quickcheck, and is designed to integrate
;; seamlessly into your existing Python unit testing work flow.")
;;   (home-page "https://hypothesis.works/")
;;   (license mpl2.0))
