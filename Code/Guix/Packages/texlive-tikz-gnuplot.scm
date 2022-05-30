(use-modules (guix packages)
	     (guix download)
	     (guix git-download)
	     (guix build-system texlive)
	     (guix build-system gnu)
	     (guix build utils)
	     (guix licenses)
	     (guix gexp)
	     (ice-9 match)
	     ((srfi srfi-1) #:hide (zip))
	     (gnu packages)
	     (gnu packages tex))

(package
 (name "texlive-tikz-gnuplot")
 (version "0.8")
 (source (origin
	  (method git-fetch)
	  (uri (git-reference
		(url "https://github.com/Antigravityd/tikz-gnuplot")
		(commit (string-append "v" version))))
	  (sha256
	   (base32 "1n6zlb3skmiphmp6lyglprl5bqbw7yn1s9q35ppx8psbdg0l1chz"))
	  (snippet
	   #~(begin
	       (delete-file "README.org")
	       #t))))
 (outputs '("out"))
 (build-system texlive-build-system)
 (arguments
  '(#:tex-directory "latex/tikz-gnuplot"))
 (inputs `(("texlive" ,texlive)))
 (home-page "https://github.com/Antigravityd/tikz-gnuplot")
 (synopsis "Gnuplot tikz terminal output processing for TeX and LaTeX")
 (description
  "Gnuplot with =set terminal tikz= outputs Latex code for which one needs a package
generated by calling a Lua script buried in the Gnuplot source tree.
This package is that so-generated.")
 (license gpl3+))
