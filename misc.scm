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
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public guix-tools
  (let ((commit "41050c8f2c23016666fe889ca836fb0ce0051b37")
	(revision "8"))
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
                "07wgpryxrg1iddpd9kxasw6n0iqyn0897cvcw3fad5sh81k0q12y"))))
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
