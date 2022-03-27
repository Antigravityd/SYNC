(define-module (dnwm)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages pkg-config))

(define-public dnwm
(package (inherit dwm)
 (name "dnwm")
 (version "1.2")
 (source (origin
	  (method git-fetch)
	  (uri (git-reference
		(url "https://github.com/Antigravityd/dnwm")
		(commit (string-append "v" version))))
	  (sha256
	   (base32 "1badd23w7dcf8hkja7hi9hvdm1ydi2rpdxhnmid8ayqsisx67v5i"))))

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
					     (string-append xsessions "/dnwm.desktop")
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
  (list freetype libx11 libxft libxinerama pango pkg-config font-iosevka font-awesome font-gnu-unifont))
 (home-page "https://github.com/Antigravityd/dnwm")
 (synopsis "dnw's dwm fork")
 (description
  "dwm is a cool program. I spent a lot of time making the patches alpha, clientindicatorshidevacant, default-tag-apps, extrabar, onlyquitonempty, pango, ru_gaps, and swallow play nice together.")))
