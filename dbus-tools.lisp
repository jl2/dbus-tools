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
  "Returns the :system or :session bus address."
  (declare (type bus-type which))
  (case which
    (:system (dbus:system-server-addresses))
    (:session (dbus:session-server-addresses))))

(defun list-all-names (which-bus)
  "Return all services visible on the specified bus."
  (declare (type bus-type which-bus))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (dbus:list-names bus)))

(defun list-paths-at (which-bus service path)
  "List object paths accessible from path.
For example (list-paths-at :system \:org.bluez\" \"/\") -> (\"/org\")"
  (declare (type bus-type which-bus)
           (string service path))
  (let ((uris (mapcar #'puri:parse-uri
                      '("https://www.freedesktop.org/standards/dbus/1.0/introspect.dtd"
                        "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd")))
        (pathname (asdf:system-relative-pathname :dbus-tools "introspect.dtd")))
    (flet ((resolver (pubid sysid)
             (declare (ignore pubid))
             (when (find sysid uris :test #'puri:uri=)
               (open pathname :element-type '(unsigned-byte 8)))))

      (let* ((xml-text (dbus-tools:invoke-method-simple which-bus
                                                        service
                                                        path
                                                        "org.freedesktop.DBus.Introspectable"
                                                        "Introspect"))
             (xml-doc (cxml:parse xml-text
                                  (cxml-dom:make-dom-builder)
                                  :entity-resolver #'resolver
                                  :validate nil))
             (paths nil))

        (xpath:do-node-set (node (xpath:evaluate "/node" xml-doc))
          (xpath:do-node-set (inner-node (xpath:evaluate "node" node))
            (push (concatenate 'string
                               path
                               (if (string/= path "/")
                                   "/"
                                   "")
                               (xpath:evaluate "string(@name)" inner-node))
                  paths)))
        paths))))

(defun list-all-paths (which-bus service &optional (bases '("/")))
  "Recursively find all object paths visible under the given base paths."
  (declare (type bus-type which-bus)
           (string service)
           (list bases))
  (loop
    :for path :in bases
    :for next = (list-paths-at which-bus service path)
    :when next
      :nconc (list-all-paths which-bus service next)
    :when (not next)
      :nconc (list path)))

(defun get-managed-objects (which-bus service object)
  (declare (type bus-type which-bus)
           (string service object))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (dbus:get-managed-objects bus service object)))

(defun managed-object-name (object)
  "Return the name or 'key' in a DBus list object.
(managed-object-name (name1 ((name2 value2) (name3 value3)))) -> name1"
  (declare (type list object))
  (car object))

(defun managed-object-value (object)
  "Return the value in a DBus list object.
(managed-object-value (name1 ((name2 value2) (name3 value3)))) -> ((name2 value2) (name3 value3))"
  (declare (type list object))
  (cadr object))

(defun get-value (object name)
  "Returns a child value in a DBus list object.
(get-value (name1 ((name2 value2) (name3 value3))) name3) -> value3"
  (declare (type list object)
           (type string name))
  (cadr (assoc name object :test #'string=)))

(defun get-all-properties (which-bus service object interface)
  "Get all properties from the object using the given interface."
  (declare (type bus-type which-bus)
           (type string service object interface))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (dbus:get-all-properties bus service object interface)))

(defun get-all-methods (which-bus service object interface)
  "Get all methods that can be invoked on the object using the given interface."
  (declare (type bus-type which-bus)
           (type string service object)
           (type hash-table interface))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (let* ((obj (dbus:make-object-from-introspection (dbus:bus-connection bus) object service))
           (interface (gethash (slot-value obj 'DBUS/INTROSPECT::interfaces) interface)))
      interface)))

(defun inspect-dbus (which-bus)
  "Inspect the org.freedesktop.DBus /org/freedesktop/DBus object."
  (declare (type bus-type which-bus))
  (dbus-tools:inspect-introspected-object which-bus
                                          "org.freedesktop.DBus"
                                          "/org/freedesktop/DBus"))

(defun invoke-method-simple (which-bus service object interface method &optional (signature "") &rest args)
  "Invoke a method on object through the specified interface."
  (declare (type bus-type which-bus)
           (type string service object interface method signature))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (dbus:invoke-method (dbus:bus-connection bus)
                        method
                        :arguments args
                        :path object
                        :signature signature
                        :interface interface
                        :destination service)))

(defun inspect-introspected-object (which-bus service object)
  (declare (type bus-type which-bus)
           (type string service object))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (let ((obj (dbus:make-object-from-introspection (dbus:bus-connection bus) object service)))
      (swank:inspect-in-emacs obj))))

(defun list-interfaces (which-bus service object)
  (declare (type bus-type which-bus)
           (type string service object))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (let ((obj (dbus:make-object-from-introspection (dbus:bus-connection bus) object service)))
      (hash-table-keys (slot-value obj 'DBUS/INTROSPECT::interfaces)))))

(defun who-owns (which-bus service)
  (declare (type bus-type which-bus)
           (type string service))
  (invoke-method-simple which-bus
                        "org.freedesktop.DBus"
                        "/org/freedesktop/DBus"
                        "org.freedesktop.DBus"
                        "GetConnectionUnixProcessID"
                        service
                        ""))

(defun has-interface (interface object)
  (declare (type string interface)
           (type list object))
  (loop
    :for obj :in (cdr object)
    :do
       (loop
         :for (interface-name interface-data) :in obj
         :when (string= interface interface-name)
           :do (return-from has-interface t)))
  nil)

(defun describe-type (type-string)
  type-string)

(defun pp (result &key (stream t) (indent 0))
  (loop :for thing :in result
        :do (typecase thing
              (cons
               (cond ((= (length thing) 1)
                      (format stream "~a" thing))
                     (t
                      (format stream
                              "~%~a~a: "
                              (make-string indent :initial-element #\space)
                              (car thing))
                      (pp (cdr thing) :stream stream :indent (+ 2 indent)))))
              (t
               (format stream "~a" thing)))))

(defun read-gatt-characteristic-by-service (service-path)
  (declare (type string service-path))
  (let ((values (dbus-tools:invoke-method-simple :system
                                                 "org.bluez"
                                                 service-path
                                                 "org.bluez.GattCharacteristic1"
                                                 "ReadValue"
                                                 "a{sv}"
                                                 nil)))
    (make-array (length values)
                :initial-contents values
                :element-type '(unsigned-byte 8))))

(defun read-gatt-characteristic-by-uuid (device uuid)
  (declare (type string device uuid))
  (let ((services (list-bluetooth-services device)))
    (flet ((matches-uuid (service)
             (string= (dbt:get-value
                       (dbt:get-value (dbt:managed-object-value service)
                                      "org.bluez.GattCharacteristic1")
                       "UUID")
                      uuid)))
      (read-gatt-characteristic-by-service (car (find-if #'matches-uuid
                                                         services))))))

(defun to-string (buffer)
  (declare (type vector buffer))
  ;; Chop the trailing 0 bytes
  (babel:octets-to-string (subseq buffer
                                  0 (search #(0)
                                            buffer))))
