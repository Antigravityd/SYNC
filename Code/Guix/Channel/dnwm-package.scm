(use-modules (gnu) (gnu packages suckless) (gnu packages) (guix build-system gnu) (guix git-download) (guix packages) (gnu packages xorg) (gnu packages fonts) (gnu packages gtk) (gnu packages fontutils) (gnu packages pkg-config))

(package (inherit dwm)
 (name "dnwm")
 (version "0.3")
 (source (origin
	  (method git-fetch)
	  (uri (git-reference
		(url "https://github.com/Antigravityd/dnwm3")
		(commit (string-append "v" version))))
	  (sha256
	   (base32 "07nnfhgpaqwbviljm9bbdbvms1ala3nynm0dqxaj0j8zf247f8z3"))))
 (build-system gnu-build-system)
 (arguments
  `(#:tests? #f
    #:make-flags (list (string-append "FREETYPEINC="
				      (assoc-ref %build-inputs "freetype")
				      "/include/freetype2"))
    #:phases
    (modify-phases %standard-phases
		   (replace 'configure
			    (lambda _
			      (substitute* "Makefile" (("\\$\\{CC\\}") "gcc"))
			      #t))
		   (replace 'install
			    (lambda* (#:key outputs #:allow-other-keys)
				     (let ((out (assoc-ref outputs "out")))
				       (invoke "make" "install"
					       (string-append "DESTDIR=" out) "PREFIX="))))
		   (add-after 'build 'install-xsession
			      (lambda* (#:key outputs #:allow-other-keys)
				       (let* ((output (assoc-ref outputs "out"))
					     (xsessions (string-append output "/share/xsessions")))
					 (mkdir-p xsessions)
					 (with-output-to-file
					     (string-append xsessions "/dwm.desktop")
					   (lambda _
					     (format #t
						     "[Desktop Entry]~@
                                                      Name=dnwm~@
                                                      Comment=dnw's dwm fork~@
                                                      Exec=~a/bin/dnwm~@
                                                      TryExec=~@*~a/bin/dnwm~@
                                                      Icon=~@
                                                      Type=Application~%"
						     output)))
					 #t))))))
 (inputs
  (list freetype libx11 libxft libxinerama pango pkg-config)))
