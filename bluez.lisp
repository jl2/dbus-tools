;; bluez.lisp

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
(defun inspect-bluetooth-device (which-bus &optional (device-path "/org/bluez/hci0/dev_00_0A_45_1A_13_5E"))
  (declare (type bus-type which-bus))
  (inspect-introspected-object which-bus
                               "org.bluez"
                               device-path))

(defun volume-up (which-bus &optional (device-path "/org/bluez/hci0/dev_00_0A_45_1A_13_5E"))
  (declare (type bus-type which-bus))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (dbus:invoke-method (dbus:bus-connection bus)
                        "VolumeUp"
                        :path device-path
                        :interface "org.bluez.MediaControl1"
                        :destination "org.bluez")))

(defun volume-down (which-bus &optional (device-path "/org/bluez/hci0/dev_00_0A_45_1A_13_5E"))
  (declare (type bus-type which-bus))
  (dbus:with-open-bus (bus (get-bus which-bus))
    (dbus:invoke-method (dbus:bus-connection bus)
                        "VolumeDown"
                        :path device-path
                        :interface "org.bluez.MediaControl1"
                        :destination "org.bluez")))

(defun list-bluetooth-objects ()
  (declare (type bus-type :system))
  (dbus:with-open-bus (bus (get-bus :system))
    (dbus:get-managed-objects bus "org.bluez" "/")))

(defun is-bluetooth-device (object)
  (cl-ppcre:scan-to-strings "^/org/bluez/hci[0-9]+/dev_\(..\)_\(..\)_\(..\)_\(..\)_\(..\)_\(..\)$" (car object)))

(defun is-bluetooth-service (object)
  (cl-ppcre:scan-to-strings "^/org/bluez/hci[0-9]+/dev_\(..\)_\(..\)_\(..\)_\(..\)_\(..\)_\(..\)/service.*$" (car object)))

(defun list-bluetooth-adapters ()
  (remove-if-not (curry #'has-interface "org.bluez.Adapter1") (list-bluetooth-objects)))

(defun list-bluetooth-devices ()
  (remove-if-not (curry #'has-interface "org.bluez.Device1") (list-bluetooth-objects)))

(defun bluetooth-connect (device-name)
  (dbus-tools:invoke-method-simple :system
                                   "org.bluez"
                                   device-name
                                   "org.bluez.Device1"
                                   "Connect"))

(defun bluetooth-disconnect (device-name)
  (dbus-tools:invoke-method-simple :system
                                   "org.bluez"
                                   device-name
                                   "org.bluez.Device1"
                                   "Disconnect"))

(defun list-bluetooth-services (&optional device)
  (remove-if-not (lambda (value)
                   (and (is-bluetooth-service value)
                        (if device
                            (cl-ppcre:scan (format nil "^~a.*" device) (car value))
                            t)))
                 (list-bluetooth-objects)))

(defun list-bluetooth-battery-levels ()
  (loop
    :for device :in (dbus-tools:list-bluetooth-devices)
    :when (dbus-tools::has-interface "org.bluez.Battery1" device)
      :collect (cons (dbus-tools::managed-object-name device)
                     (dbus-tools:get-all-properties :system
                                                    "org.bluez"
                                                    (dbus-tools::managed-object-name device)
                                                    "org.bluez.Battery1"))))
