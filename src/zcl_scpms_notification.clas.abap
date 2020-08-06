class ZCL_SCPMS_NOTIFICATION definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_SCPMS_NOTIFICATION
      all methods abstract .

  aliases GET_PAYLOAD
    for ZIF_SCPMS_NOTIFICATION~GET_PAYLOAD .

  class-methods CREATE_APNS_NOTIFICATION
    returning
      value(NOTIFICATION) type ref to ZCL_SCPMS_APNS_NOTIFICATION .
  class-methods CREATE_FCM_NOTIFICATION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SCPMS_NOTIFICATION IMPLEMENTATION.


METHOD create_apns_notification.

  CREATE OBJECT notification.

ENDMETHOD.


METHOD create_fcm_notification.
ENDMETHOD.
ENDCLASS.
