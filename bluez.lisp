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

(defun bt-inspect-device (&optional (device-path
                                     (managed-object-name
                                      (bt-first-with-interface "org.bluez.MediaControl1"))))
  "Inspect a Bluez device object by device path.  Default path is the first device implementing MediaControl1."
  (inspect-introspected-object :system
                               "org.bluez"
                               device-path))

(defun bt-first-with-interface (interface)
  (loop :for device :in (bt-list-devices)
        :when (has-interface device interface)
          :return device))

(defun volume-up (&key
                    (steps 1)
                    (device-path (managed-object-name
                                  (bt-first-with-interface "org.bluez.MediaControl1"))))
  "Execute VolumeUp method one or more times on a org.bluez.MediaControl1 object."
  (dbus:with-open-bus (bus (get-bus :system))
    (dotimes (i steps)
      (dbus:invoke-method (dbus:bus-connection bus)
                          "VolumeUp"
                          :path device-path
                          :interface "org.bluez.MediaControl1"
                          :destination "org.bluez"))))

(defun volume-down (&key
                      (steps 1)
                      (device-path (managed-object-name
                                    (bt-first-with-interface "org.bluez.MediaControl1"))))
  "Execute VolumeDown method one or more times on a org.bluez.MediaControl1 object."
  (dbus:with-open-bus (bus (get-bus :system))
    (dotimes (i steps)
      (dbus:invoke-method (dbus:bus-connection bus)
                          "VolumeDown"
                          :path device-path
                          :interface "org.bluez.MediaControl1"
                          :destination "org.bluez"))))

(defun bt-list-objects ()
  "List all org.bluez managed objects."
  (dbus:with-open-bus (bus (get-bus :system))
    (dbus:get-managed-objects bus "org.bluez" "/")))

(defun bt-device-p (object)
  "Test if object looks like a bluetooth device."
  (has-interface object "org.bluez.Device1"))

(defun bt-service-p (object)
  (cl-ppcre:scan-to-strings
   "^/org/bluez/hci[0-9]+/dev_\(..\)_\(..\)_\(..\)_\(..\)_\(..\)_\(..\)/service.*$"
   (managed-object-name object)))


(defun bt-list-adapters ()
  (remove-if-not (rcurry #'has-interface "org.bluez.Adapter1") (bt-list-objects)))

(defun bt-list-media-controllers ()
  (remove-if-not (rcurry #'has-interface "org.bluez.MediaControl1") (bt-list-objects)))

(defun bt-scan (&key (timeout 5)
                  (adapter (managed-object-name (first (bt-list-adapters)))))
  (invoke-method-simple :system
                        "org.bluez"
                        adapter
                        "org.bluez.Adapter1"
                        "StartDiscovery")
  (sleep timeout)
  (invoke-method-simple :system
                        "org.bluez"
                        adapter
                        "org.bluez.Adapter1"
                        "StopDiscovery"))
(defun nested-get (dev &rest args)
  (loop :for val = dev :then (find-value val arg)
        :for arg :in args
        :finally (return val)))


(defun bt-list-devices (&key
                          (interfaces nil)
                          (full nil)
                          )
  (flet ((bt-predicate (obj)
           (and (has-interface obj "org.bluez.Device1")
                (every (curry #'has-interface obj) (ensure-list interfaces)))))

    (let ((devs (remove-if-not #'bt-predicate (bt-list-objects))))
      (if full
          devs
          (loop
            :for device :in devs
            :for dev = (managed-object-value device)
            :collect (nested-get dev "org.bluez.Device1" "Name"))))))

(defun bt-connect (device-path)
  (dbus-tools:invoke-method-simple :system
                                   "org.bluez"
                                   device-path
                                   "org.bluez.Device1"
                                   "Connect"))
(defun bt-pair (device-path)
  (dbus-tools:invoke-method-simple :system
                                   "org.bluez"
                                   device-path
                                   "org.bluez.Device1"
                                   "Pair"))

(defun bt-disconnect (device-path)
  (dbus-tools:invoke-method-simple :system
                                   "org.bluez"
                                   device-path
                                   "org.bluez.Device1"
                                   "Disconnect"))

(defun bt-list-services (&optional device)
  (remove-if-not (lambda (value)
                   (and (bt-service-p value)
                        (if device
                            (cl-ppcre:scan (format nil "^~a.*" device) (car value))
                            t)))
                 (bt-list-objects)))

(defun bt-battery-levels ()
  "Return a list of (device name . battery level) for all connected Bluetooth devices."
  (loop
    :for device :in (bt-list-devices :interfaces '("org.bluez.Battery1")
                                     :full t)
    :for dev = (managed-object-value device)
    :for is-connected = (nested-get dev "org.bluez.Device1" "Connected")
    :for is-paired = (nested-get dev "org.bluez.Device1" "Paired")
    :when  (or is-paired is-connected)
      :collect (cons (nested-get dev "org.bluez.Device1" "Name")
                     (nested-get dev "org.bluez.Battery1" "Percentage"))))
