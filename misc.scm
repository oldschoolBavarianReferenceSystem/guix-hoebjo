;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (misc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system texlive)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages node)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module ((srfi srfi-1) #:hide (zip)))


(define-public guix-tools
  (let ((commit "ea01b1697394549f727198c90d3816e48a908226")
	(revision "9"))
    (package
     (name "guix-tools")
     (version (git-version "0.1.0" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/hoebjo/guix-tools.git")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "183fq816j1j0sg12z35gn786x997lv91sxablil5cmg975bj7x7j"))))
     (build-system gnu-build-system)
     (arguments
      `(#:tests?
	#f
	#:phases
	(modify-phases %standard-phases
		       (delete 'configure)
		       (delete 'build)
		       (add-before
		        'install 'subst
		        (lambda* (#:key inputs #:allow-other-keys)
			  (let* ((coreutils (assoc-ref inputs "coreutils"))
		        	 (tee (string-append coreutils "/bin/tee"))
				 (espeak-ng (assoc-ref inputs "espeak-ng"))
				 (espeak (string-append espeak-ng "/bin/espeak")))
		            (substitute* '("gbuild")
		        		 (("tee")
		        		  tee))
			    (substitute* '("gbuildn")
		        		 (("espeak")
		        		  espeak)))))
		       (replace 'install
				(lambda* (#:key outputs #:allow-other-keys)
					 (let* ((out (assoc-ref outputs "out"))
						(bin (string-append out "/bin")))
					   (install-file "gbuild" bin)
					   (install-file "gbuildn" bin)
					   (install-file "gdev" bin)
					   #t))))))

     (inputs
      `(("coreutils" ,coreutils)
	("espeak-ng" ,espeak-ng)))
     (synopsis "Guix build tools")
     (description "Guix build tools are my tiny, personal scripts
 and helpers to build GNU Guix.")
     (home-page "https://gitlab.com/hoebjo/guix-tools")
     (license license:gpl3+))))


(define* (simple-texlive-package name locations hash
                                 #:key trivial?)
  "Return a template for a simple TeX Live package with the given NAME,
downloading from a list of LOCATIONS in the TeX Live repository, and expecting
the provided output HASH.  If TRIVIAL? is provided, all files will simply be
copied to their outputs; otherwise the TEXLIVE-BUILD-SYSTEM is used."
  (define with-documentation?
    (and trivial?
         (any (lambda (location)
                (string-prefix? "/doc" location))
              locations)))
  (package
    (name name)
    (version (number->string %texlive-revision))
    (source (texlive-origin name version
                            locations hash))
    (outputs (if with-documentation?
                 '("out" "doc")
                 '("out")))
    (build-system (if trivial?
                      gnu-build-system
                      texlive-build-system))
    (arguments
     (let ((copy-files
            `(lambda* (#:key outputs inputs #:allow-other-keys)
               (let (,@(if with-documentation?
                           `((doc (string-append (assoc-ref outputs "doc")
                                                 "/share/texmf-dist/")))
                           '())
                     (out (string-append (assoc-ref outputs "out")
                                         "/share/texmf-dist/")))
                 ,@(if with-documentation?
                       '((mkdir-p doc)
                         (copy-recursively
                          (string-append (assoc-ref inputs "source") "/doc")
                          (string-append doc "/doc")))
                       '())
                 (mkdir-p out)
                 (copy-recursively (assoc-ref inputs "source") out)
                 ,@(if with-documentation?
                       '((delete-file-recursively (string-append out "/doc")))
                       '())
                 #t))))
       (if trivial?
           `(#:tests? #f
             #:phases
             (modify-phases %standard-phases
               (delete 'configure)
               (replace 'build (const #t))
               (replace 'install ,copy-files)))
           `(#:phases
             (modify-phases %standard-phases
               (add-after 'install 'copy-files ,copy-files))))))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public texlive-moderncv
  (package
    (inherit (simple-texlive-package
              "texlive-moderncv"
              (list "/tex/latex/base/docstrip.tex")
              (base32
               "1f9sx1lp7v34zwm186msf03q2h28rrg0lh65z59zc0cvqffs6dvb")
              #:trivial? #t))
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xdanaux/moderncv.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "texlive-moderncv" version))
              (sha256
               (base32
                "1slgjsyiagglpzx4fqwmhbq6bnz40ii5kl8g2vbh144nnlql0smj"))))

    (home-page "https://github.com/xdanaux/moderncv/")
    (synopsis "Moderncv is a modern curriculum vitae class")
    (description
     "Moderncv provides a documentclass for typesetting curricula vitae in
various styles.  Moderncv aims to be both straightforward to use and
customizable, providing four ready-made styles (classic, casual, banking and
oldstyle) and allowing one to define his own by modifying colors, fonts,
icons, etc.")
    (license license:lppl1.3c)))
