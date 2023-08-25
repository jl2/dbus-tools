# dbus-tools

### _Jeremiah LaRocco <jeremiah_larocco@fastmail.com>_

Convenience functions for using and inspecting DBus.


```common-lisp
  (ql:quickload :dbus-tools)
  (dbus-tools:all-names :system)
  (dbus-tools:all-names :session)
  (dbus-tools:all-bluetooth-devices)

  (dbus-tools:list-interfaces :system
                              "org.bluez" "/")

  (dbus-tools:get-managed-objects :system
                                  "org.bluez" "/")

  (dbus-tools:inspect-introspected-object :system
                                          "org.bluez"
                                          "/org/bluez/hci0/dev_00_0A_45_1A_13_5E")
  (dbus-tools:get-all-properties :system
                                 "org.bluez"
                                 "/org/bluez/hci0/dev_00_0A_45_1A_13_5E"
                                 "org.bluez.Device1")

  (dbus-tools:invoke-method-simple :system
                                   "org.bluez"
                                   "/"
                                   "org.freedesktop.DBus.Introspectable"
                                   "Introspect")
```
## License

ISC

Copyright (c) 2023 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


