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
  (name "python-lark-parser")
  (version "1.1.5")
  (source (origin
            (method git-fetch)
            (uri (git-reference
		  (url "https://github.com/lark-parser/lark")
		  (commit version)))
            (sha256
             (base32
              "1b0hcd3wda7qnn6vfjzlwrnlik1kddxsdpvadghxlffj8gxwbfvn"))))
  (build-system python-build-system)
  (home-page "https://github.com/lark-parser/lark")
  (synopsis "Multi-language parser for Python")
  (description
   "Lark is a parser built with a focus on ergonomics, performance and
resilience.  Lark can parse all context-free languages.  That means it is
capable of parsing almost any programming language out there, and to
some degree most natural languages too.")
  (license expat))
