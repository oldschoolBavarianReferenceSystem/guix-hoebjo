;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
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

(define-module (eclipse)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
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
  #:use-module (gnu packages java)
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
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public eclipse-ide
  ;; XXX: This package is not built from source.
  (package
    (name "eclipse-ide")
    (version "2019-06")
    ;; TODO: Handle i686 alternate origin and augment 'supported-systems'.
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.eclipse.org/downloads/download.php?"
                    "r=1&file=/technology/epp/downloads/release/"
                    version "/R/" name "-"
                    version "-R-linux-gtk-x86_64.tar.gz"))
              (sha256
               (base32
                "17rzcimwyxlkldyilmx3x4265zdl5a6hk2b2zgs8qv6f3qbsdhkj"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source  (assoc-ref %build-inputs "source"))
                (out     (assoc-ref %outputs "out"))
                (eclipse (string-append out "/eclipse/eclipse")))
           (mkdir-p (string-append out "/bin"))
           (set-path-environment-variable "PATH" '("bin")
                                          (map cdr %build-inputs))
           (and
            (invoke "tar" "xvf" source "-C" out)
            (let ((ld-so (string-append (assoc-ref %build-inputs "libc")
                                        "/lib/ld-linux-x86-64.so.2")))
              (invoke "patchelf" "--set-interpreter" ld-so eclipse))
            (let ((jdk (assoc-ref %build-inputs "jdk")))
              (define (libdir input)
                (string-append (assoc-ref %build-inputs input) "/lib"))
              (wrap-program eclipse
                `("PATH" prefix (,(string-append jdk "/bin")))
                `("LD_LIBRARY_PATH" prefix ,(map libdir
                                                 '("gtk+" "libxtst" "glib" "webkitgtk"))))
              (symlink eclipse (string-append out "/bin/eclipse"))
              #t))))))
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)
       ("patchelf" ,patchelf)))
    (inputs
     `(("bash" ,bash)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("jdk" ,icedtea-8)
       ("webkitgtk" ,webkitgtk)
       ("libc" ,glibc)
       ("libxtst" ,libxtst)))
    (supported-systems '("x86_64-linux"))
    (synopsis "Java Integrated Development Environment")
    (description
     "Eclipse is an integrated development environment (IDE) used in computer
programming, in particular for Java development.  It contains a base workspace
and an extensible plug-in system for customizing the environment.")
    (home-page "https://www.eclipse.org/")
    (license license:mpl2.0)))
