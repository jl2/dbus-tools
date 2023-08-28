;; dbus-tools.lisp

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

(in-package :dbus-tools)

(deftype bus-type () '(member :system :session))

(defun get-bus (which)
  (case which
    (:system (dbus:system-server-addresses))
    (:session (dbus:session-server-addresses))))

(defun list-all-names (which-bus)
  (declare (type bus-type which-bus))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (dbus:list-names bus)))

(defun list-paths (which-bus service)
  (let ((uri "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd")
        (pathname (asdf:system-relative-pathname :dbus-tools "introspect.dtd")))
    (flet ((resolver (pubid sysid)
	         (declare (ignore pubid))
	         (when (puri:uri= sysid (puri:parse-uri uri))
	           (open pathname :element-type '(unsigned-byte 8)))))
      (let ((xml-doc (cxml:parse (dbus-tools:invoke-method-simple which-bus
                                                                  service
                                                                  "/"
                                                                  "org.freedesktop.DBus.Introspectable"
                                                                  "Introspect")
                                 (cxml-dom:make-dom-builder)
                                 :entity-resolver #'resolver
                                 :validate nil)))
        ;; (xpath:map-node-set (curry #'xpath:evaluate "string(@name)") (xpath:evaluate "/node/node/*" xml-doc))
        xml-doc))))


(defun get-managed-objects (which-bus service object)
  (declare (type bus-type which-bus))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (dbus:get-managed-objects bus service object)))

(defun managed-object-name (object)
  (car object))

(defun managed-object-value (object)
  (cdr object))

(defun get-all-properties (which-bus service object interface)
  (declare (type bus-type which-bus))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (dbus:get-all-properties bus service object interface)))

(defun get-all-methods (which-bus service object interface)
  (declare (type bus-type which-bus))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (let* ((obj (dbus:make-object-from-introspection (dbus:bus-connection bus) object service))
           (interface (gethash (slot-value obj 'DBUS/INTROSPECT::interfaces) interface)))
      interface)))

(defun inspect-dbus (which-bus)
  (declare (type bus-type which-bus))
  (dbus-tools:inspect-introspected-object which-bus
                                          "org.freedesktop.DBus"
                                          "/org/freedesktop/DBus"))

(defun invoke-method-simple (which-bus service object interface method &optional (signature "") &rest args)
  (declare (type bus-type which-bus))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (dbus:invoke-method (dbus:bus-connection bus)
                        method
                        :arguments args
                        :path object
                        :signature signature
                        :interface interface
                        :destination service)))

(defun inspect-introspected-object (which-bus service object)
  (declare (type bus-type which-bus))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (let ((obj (dbus:make-object-from-introspection (dbus:bus-connection bus) object service)))
      (swank:inspect-in-emacs obj))))

(defun list-interfaces (which-bus service object)
  (declare (type bus-type which-bus))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (let ((obj (dbus:make-object-from-introspection (dbus:bus-connection bus) object service)))
      (hash-table-keys (slot-value obj 'DBUS/INTROSPECT::interfaces)))))

;; (defun list-paths (&key (service "org.bluez") (object "/org/bluez/hci0/dev_00_0A_45_1A_13_5E"))
;;   (dbus:with-open-bus (bus (get-bus which-bus)) 
;;     (dbus:invoke-method (dbus:bus-connection bus)
;;                         "Introspect"
;;                         :path object
;;                         :int
;;                         :interface "org.freedesktop.Introspectable")))

(defun who-owns (which-bus service)
  (declare (type bus-type which-bus))
  (invoke-method-simple
   which-bus
   "org.freedesktop.DBus"
   "/org/freedesktop/DBus"
   "org.freedesktop.DBus"
   "GetConnectionUnixProcessID"
   service
   ""))


(defun has-interface (interface object)
  (loop
    :for obj :in (cdr object)
    :do
       (loop
         :for (interface-name interface-data) :in obj
         :when (string= interface interface-name)
           :do
              (return-from has-interface t)))
  nil)


(defun describe-type (type-string)
  type-string)
