;; package.lisp

;; Copyright © 2023 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(defpackage :dbus-tools

  (:nicknames :dbt)

  (:use #:cl #:alexandria)
  (:export
           #:use-inspector

           #:has-interface

           #:inspect-introspected-object
           #:inspect-dbus
           #:get-bus
           #:list-interfaces
           #:list-all-names
           #:list-all-paths
           #:list-paths-at
           #:list-managed-objects
           #:get-all-properties

           #:query-objects-in-service

           #:invoke-method-simple

           #:who-owns

           ;; Not yet implemented...
           #:describe-type

           #:managed-object-name
           #:managed-object-value
           #:find-value

           #:pp
           ))
