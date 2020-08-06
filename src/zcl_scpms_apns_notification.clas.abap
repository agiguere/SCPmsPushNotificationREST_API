class ZCL_SCPMS_APNS_NOTIFICATION definition
  public
  inheriting from ZCL_SCPMS_NOTIFICATION
  final
  create public .

public section.

  methods SET_SOUND
    importing
      !SOUND type STRING .
  methods SET_BADGE
    importing
      !BADGE type INT4 .
  methods SET_ALERT
    importing
      !ALERT type STRING .
  methods SET_TITLE
    importing
      !TITLE type STRING .
  methods SET_BODY
    importing
      !BODY type STRING .
  methods SET_TITLE_LOC_KEY
    importing
      !TITLE_LOC_KEY type STRING .
  methods SET_TITLE_LOC_ARGS .
  methods SET_LOC_KEY
    importing
      !LOC_KEY type STRING .
  methods SET_LOC_ARGS .
  methods SET_LAUNCH_IMAGE
    importing
      !LAUNCH_IMAGE type STRING .
  type-pools ABAP .
  methods SET_CONTENT_AVAILABLE
    importing
      !CONTENT_AVAILABLE type ABAP_BOOL .
  methods SET_CATEGORY
    importing
      !CATEGORY type STRING .

  methods ZIF_SCPMS_NOTIFICATION~GET_PAYLOAD
    redefinition .
protected section.
private section.

  data SOUND type STRING .
  data BADGE type INT4 .
  data ALERT type STRING .
  data TITLE type STRING .
  data BODY type STRING .
  data TITLE_LOC_KEY type STRING .
  data LOC_KEY type STRING .
  type-pools ABAP .
  data CONTENT_AVAILABLE type ABAP_BOOL .
  data CATEGORY type STRING .
  data LAUNCH_IMAGE type STRING .

  methods SERIALIZE_CUSTOM_PARAMETERS
    returning
      value(JSON) type STRING .
  methods SERIALIZE_ALERT
    returning
      value(JSON) type STRING .
ENDCLASS.



CLASS ZCL_SCPMS_APNS_NOTIFICATION IMPLEMENTATION.


METHOD serialize_alert.

  "SAP Documentation
  "https://help.sap.com/viewer/38dbd9fbb49240f3b4d954e92335e670/Cloud/en-US/1a74d910d7634a43bb53a37a5060d429.html

  IF alert IS NOT INITIAL.
    json = '"alert": "' && alert && '"'.
    RETURN.
  ENDIF.

  CHECK title IS NOT INITIAL
     OR body IS NOT INITIAL
     OR title_loc_key IS NOT INITIAL
     OR loc_key IS NOT INITIAL
     OR launch_image IS NOT INITIAL.

  IF title IS NOT INITIAL.
    json = '\"title\": \"' && title && '\"'.
  ENDIF.

  IF body IS NOT INITIAL.
    IF json IS INITIAL.
      json = '\"body\": \"' && body && '\"'.
    ELSE.
      json = json && ', \"body\": \"' && body && '\"'.
    ENDIF.
  ENDIF.

  IF title_loc_key IS NOT INITIAL.
    IF json IS INITIAL.
      json = '\"title-loc-key\": \"' && title_loc_key && '\"'.
    ELSE.
      json = json && ', \"title-loc-key\": \"' && title_loc_key && '\"'.
    ENDIF.
  ENDIF.

  IF loc_key IS NOT INITIAL.
    IF json IS INITIAL.
      json = '\"loc-key\": \"' && loc_key && '\"'.
    ELSE.
      json = json && ', \"loc-key\": \"' && loc_key && '\"'.
    ENDIF.
  ENDIF.

  IF launch_image IS NOT INITIAL.
    IF json IS INITIAL.
      json = '\"launch-image\": \"' && launch_image && '\"'.
    ELSE.
      json = json && ', \"launch-image\": \"' && launch_image && '\"'.
    ENDIF.
  ENDIF.

  json = '"alert": "{' && json && ' }"'.

ENDMETHOD.


METHOD serialize_custom_parameters.

  CHECK category IS NOT INITIAL OR content_available IS NOT INITIAL.

  IF category IS NOT INITIAL.
    json = '"apns.category": "' && category && '"'.
  ENDIF.

  IF content_available = abap_true.
    IF json IS NOT INITIAL.
      json = json && ', '.
    ENDIF.

    json = json && '"apns.contentAvailable": true'.
  ENDIF.

  json = '"customParameters": {' && json && ' }'.

ENDMETHOD.


METHOD set_alert.

  me->alert = alert.

ENDMETHOD.


METHOD set_badge.

  me->badge = badge.

ENDMETHOD.


METHOD set_body.

  me->body = body.

ENDMETHOD.


METHOD set_category.

  me->category = category.

ENDMETHOD.


METHOD set_content_available.

  me->content_available = content_available.

ENDMETHOD.


METHOD set_launch_image.

  me->launch_image = launch_image.

ENDMETHOD.


METHOD set_loc_args.
ENDMETHOD.


METHOD set_loc_key.

  me->loc_key = loc_key.

ENDMETHOD.


METHOD set_sound.

  me->sound = sound.

ENDMETHOD.


METHOD set_title.

  me->title = title.

ENDMETHOD.


METHOD set_title_loc_args.
ENDMETHOD.


METHOD set_title_loc_key.

  me->title_loc_key = title_loc_key.

ENDMETHOD.


METHOD zif_scpms_notification~get_payload.

  DATA: json_alert        TYPE string,
        custom_parameters TYPE string.

  IF sound IS NOT INITIAL.
    payload = '"sound": "' && sound && '"'.
  ENDIF.

  IF badge IS NOT INITIAL.
    IF payload IS INITIAL.
      payload = '"badge": ' && badge.
    ELSE.
      payload = payload && ', "badge": ' && badge.
    ENDIF.
  ENDIF.

  json_alert = serialize_alert( ).

  custom_parameters = serialize_custom_parameters( ).

  IF json_alert IS NOT INITIAL.
    IF payload IS INITIAL.
      payload = json_alert.
    ELSE.
      payload = payload && ', ' && json_alert.
    ENDIF.
  ENDIF.

  IF custom_parameters IS NOT INITIAL.
    IF payload IS INITIAL.
      payload = custom_parameters.
    ELSE.
      payload = payload && ', ' && custom_parameters.
    ENDIF.
  ENDIF.

  payload = '{ ' && payload && ' }'.

ENDMETHOD.
ENDCLASS.
