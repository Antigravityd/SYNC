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
	     (gnu packages python-build))


(package
  (name "python-fortranformat")
  (version "1.2.2")
  (source (origin
            (method url-fetch)
            (uri (pypi-uri "fortranformat" version))
            (sha256
             (base32
              "0aqvrayqp8ipq4rnh834n2cigxadsraqgwqprbk4815lkfrimi58"))))
  (build-system python-build-system)
  (arguments
   '(#:phases (modify-phases %standard-phases
		(delete 'check))))
  (inputs `(("python-setuptools" ,python-setuptools)))
  (home-page "https://github.com/brendanarnold/py-fortranformat")
  (synopsis "Mimics Fortran textual IO in Python")
  (description "Mimics Fortran textual IO in Python")
  (license expat))
