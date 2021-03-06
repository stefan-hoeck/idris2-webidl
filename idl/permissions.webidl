// Extracted from https://w3c.github.io/permissions/

dictionary PermissionDescriptor {
  required PermissionName name;
};

enum PermissionState {
  "granted",
  "denied",
  "prompt",
};

[Exposed=(Window,Worker)]
interface PermissionStatus : EventTarget {
  readonly attribute PermissionState state;
  attribute EventHandler onchange;
};

[Exposed=(Window)]
partial interface Navigator {
  [SameObject] readonly attribute Permissions permissions;
};

[Exposed=(Worker)]
partial interface WorkerNavigator {
  [SameObject] readonly attribute Permissions permissions;
};

[Exposed=(Window,Worker)]
interface Permissions {
  Promise<PermissionStatus> query(object permissionDesc);
};

enum PermissionName {
  "geolocation",
  "notifications",
  "push",
  "midi",
  "camera",
  "microphone",
  "speaker-selection",
  "device-info",
  "background-fetch",
  "background-sync",
  "bluetooth",
  "persistent-storage",
  "ambient-light-sensor",
  "accelerometer",
  "gyroscope",
  "magnetometer",
  "clipboard-read",
  "clipboard-write",
  "display-capture",
  "nfc",
};

dictionary PushPermissionDescriptor : PermissionDescriptor {
  boolean userVisibleOnly = false;
};

dictionary MidiPermissionDescriptor : PermissionDescriptor {
  boolean sysex = false;
};

dictionary DevicePermissionDescriptor : PermissionDescriptor {
  DOMString deviceId;
};

dictionary CameraDevicePermissionDescriptor : DevicePermissionDescriptor {
  boolean panTiltZoom = false;
};

dictionary PermissionSetParameters {
  required PermissionDescriptor descriptor;
  required PermissionState state;
  boolean oneRealm = false;
};
