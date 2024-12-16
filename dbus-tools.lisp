;; dbus-tools.lisp

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

(defun list-managed-objects (which-bus service object)
  "List all managed objects under object."
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

(defun find-value (object name)
  "Returns a child value in a DBus list object.
(find-value (name1 ((name2 value2) (name3 value3))) name3) -> value3"
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

(defun invoke-method-simple (which-bus
                             service
                             object
                             interface
                             method
                             &optional (signature "")
                             &rest args)
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

(deftype inspector-type ()
  '(member :swank :mcclim :built-in))

(defparameter *inspect-function* #'inspect)

(defun use-inspector (&optional (which :built-in))
  (declare (type inspector-type which))
  (ecase which
    (:built-in
     (setf *inspect-function* #'inspect))
    #+mcclim
    (:mcclim
     (setf *inspect-function* #'fi:insp))
    #+swank
    (:swank
     (setf *inspect-function* #+swank #'swank:inspect-in-emacs))))

(defun inspect-introspected-object (which-bus service object)
  "Open an instrospected object in the Slime Inspector."
  (declare (type bus-type which-bus)
           (type string service object))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (let ((obj (dbus:make-object-from-introspection (dbus:bus-connection bus) object service)))

      (funcall *inspect-function* obj))))

(defun list-interfaces (which-bus service object)
  "List all interfaces that object satisfies."
  (declare (type bus-type which-bus)
           (type string service object))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (let ((obj (dbus:make-object-from-introspection (dbus:bus-connection bus) object service)))
      (hash-table-keys (slot-value obj 'DBUS/INTROSPECT::interfaces)))))

(defun who-owns (which-bus service)
  "See which process owns a service."
  (declare (type bus-type which-bus)
           (type string service))
  (invoke-method-simple which-bus
                        "org.freedesktop.DBus"
                        "/org/freedesktop/DBus"
                        "org.freedesktop.DBus"
                        "GetConnectionUnixProcessID"
                        "s"
                        service
                        ""))

(defun has-interface (object interface)
  "Check if object satisfies interface."
  (declare (type string interface)
           (type list object))
  (loop
    :for obj :in (cdr object)
    :do (loop
          :for (interface-name interface-data) :in obj
          :when (string= interface interface-name)
            :do (return-from has-interface t)))
  nil)

(defun describe-type (type-string)
  type-string)

(defun pp (values &key (stream t) (indent 0))
  "Pretty print values - a list structure returned by the dbus API."
  (loop :for thing :in values
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

(defun to-string (buffer)
  "Convert an octet buffer into a string."
  (declare (type vector buffer))
  ;; Chop the trailing 0 bytes
  (babel:octets-to-string (subseq buffer
                                  0 (search #(0)
                                            buffer))))
