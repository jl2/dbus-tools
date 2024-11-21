;; package.lisp

;; Copyright (c) 2023 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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
  (:export #:bt-list-objects
           #:bt-list-adapters
           #:bt-scan
           #:bt-list-devices
           #:bt-list-services
           #:bt-battery-levels
           #:bt-first-with-interface
           #:bt-connect
           #:bt-pair
           #:bt-disconnect
           #:bt-device-p

           #:bt-inspect-device
           #:use-inspector

           #:volume-up
           #:volume-down

           #:inspect-introspected-object
           #:inspect-dbus

           #:list-interfaces
           #:list-all-names
           #:list-all-paths
           #:list-paths-at
           #:read-gatt-characteristic-by-service
           #:read-gatt-characteristic-by-uuid
           #:to-string
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
