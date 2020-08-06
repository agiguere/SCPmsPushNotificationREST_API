class ZCL_SCPMS_PUSH_NOTIFICATION definition
  public
  final
  create private .

public section.

  type-pools ABAP .
  class-methods GET_INSTANCE
    importing
      !REUSE_CONNECTION type ABAP_BOOL default ABAP_FALSE
    returning
      value(PUSH_NOTIFICATION) type ref to ZCL_SCPMS_PUSH_NOTIFICATION
    raising
      ZCX_SCPMS_PUSH_NOTIFICATION .
  methods PUSH_TO_APPLICATION
    importing
      !APPLICATION_ID type ZSCPMS_APPLICATION_ID
      !NOTIFICATION type ref to ZIF_SCPMS_NOTIFICATION optional
    returning
      value(RESPONSE) type ref to IF_HTTP_RESPONSE
    raising
      ZCX_SCPMS_PUSH_NOTIFICATION .
  methods PUSH_TO_APP_USER_DEVICES
    importing
      !APPLICATION_ID type ZSCPMS_APPLICATION_ID
      !USER_NAME type ZSCPMS_USER_NAME
      !NOTIFICATION type ref to ZIF_SCPMS_NOTIFICATION optional
    returning
      value(RESPONSE) type ref to IF_HTTP_RESPONSE
    raising
      ZCX_SCPMS_PUSH_NOTIFICATION .
  methods PUSH_TO_APP_USERS
    importing
      !APPLICATION_ID type ZSCPMS_APPLICATION_ID
      !USERS type ZSCPMS_USER_T
      !NOTIFICATION type ref to ZIF_SCPMS_NOTIFICATION
    returning
      value(RESPONSE) type ref to IF_HTTP_RESPONSE
    raising
      ZCX_SCPMS_PUSH_NOTIFICATION .
  methods PUSH_TO_APP_REGISTRATION
    importing
      !REGISTRATION_ID type ZSCPMS_APP_REGISTRATION_ID
      !NOTIFICATION type ref to ZIF_SCPMS_NOTIFICATION optional
    returning
      value(RESPONSE) type ref to IF_HTTP_RESPONSE
    raising
      ZCX_SCPMS_PUSH_NOTIFICATION .
  methods PUSH_TO_APP_REGISTRATION_LIST
    importing
      !REGISTRATION_LIST type ZSCPMS_APP_REGISTRATION_T
      !NOTIFICATION type ref to ZIF_SCPMS_NOTIFICATION optional
    returning
      value(RESPONSE) type ref to IF_HTTP_RESPONSE
    raising
      ZCX_SCPMS_PUSH_NOTIFICATION .
  methods TEST_URL
    importing
      !URL type STRING
    returning
      value(RESPONSE) type ref to IF_HTTP_RESPONSE
    raising
      ZCX_SCPMS_PUSH_NOTIFICATION .
  methods CREATE_APNS_NOTIFICATION
    returning
      value(APNS_NOTIFICATION) type ref to ZCL_SCPMS_APNS_NOTIFICATION .
  methods CLOSE_CONNECTION
    raising
      ZCX_SCPMS_PUSH_NOTIFICATION .
protected section.
private section.

  constants DESTINATION_NAME type RFCDEST value 'SCPMS_ENHANCED_PUSH' ##NO_TEXT.
  data NOTIFICATION type ref to ZIF_SCPMS_NOTIFICATION .
  data HTTP_CLIENT type ref to IF_HTTP_CLIENT .

  methods CONSTRUCTOR
    importing
      !HTTP_CLIENT type ref to IF_HTTP_CLIENT optional .
  methods SEND_PUSH
    importing
      !URI type STRING
      !PAYLOAD type STRING
    returning
      value(RESPONSE) type ref to IF_HTTP_RESPONSE
    raising
      ZCX_SCPMS_PUSH_NOTIFICATION .
  methods SERIALIZE_NOTIFICATION_DATA
    importing
      !NOTIFICATION_DATA type ZSCPMS_NOTIFICATION_DATA
    returning
      value(JSON) type STRING .
  methods SERIALIZE_REGISTRATION_LIST
    importing
      !REGISTRATION_LIST type ZSCPMS_APP_REGISTRATION_T
    returning
      value(JSON) type STRING .
  methods SERIALIZE_USER_LIST
    importing
      !USER_LIST type ZSCPMS_USER_T
    returning
      value(JSON) type STRING .
  class-methods CREATE_CONNECTION
    returning
      value(HTTP_CLIENT) type ref to IF_HTTP_CLIENT
    raising
      ZCX_SCPMS_PUSH_NOTIFICATION .
ENDCLASS.



CLASS ZCL_SCPMS_PUSH_NOTIFICATION IMPLEMENTATION.


METHOD close_connection.

  CHECK http_client IS BOUND.

  http_client->close(
    EXCEPTIONS
      http_invalid_state = 1
      OTHERS             = 2 ).

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>close_connection_failed.
  ENDIF.

ENDMETHOD.


METHOD constructor.

  me->http_client = http_client.

ENDMETHOD.


METHOD create_apns_notification.

  notification = apns_notification = zcl_scpms_notification=>create_apns_notification( ).

ENDMETHOD.


METHOD create_connection.

  CALL METHOD cl_http_client=>create_by_destination
    EXPORTING
      destination              = destination_name
    IMPORTING
      client                   = http_client
    EXCEPTIONS
      argument_not_found       = 1
      destination_not_found    = 2
      destination_no_authority = 3
      plugin_not_active        = 4
      internal_error           = 5
      OTHERS                   = 6.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>create_url_failed.
  ENDIF.

ENDMETHOD.


METHOD get_instance.

  DATA http_client TYPE REF TO if_http_client.

  IF reuse_connection = abap_true.
    http_client = create_connection( ).
  ENDIF.

  CREATE OBJECT push_notification
    EXPORTING
      http_client = http_client.

ENDMETHOD.


METHOD push_to_application.

  DATA payload TYPE string.

  DATA uri TYPE string.

  IF application_id IS INITIAL.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>application_id_empty.

  ELSEIF notification IS NOT BOUND AND me->notification IS NOT BOUND.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>notification_data_empty.
  ENDIF.

  uri = '/application/' && application_id.

  IF notification IS BOUND.
    payload = notification->get_payload( ).
  ELSE.
    payload = me->notification->get_payload( ).
  ENDIF.

  response = send_push(
    uri     = uri
    payload = payload ).

ENDMETHOD.


METHOD push_to_app_registration.

  DATA payload TYPE string.

  DATA uri TYPE string.

  IF registration_id IS INITIAL.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>registration_id_empty.

  ELSEIF notification IS NOT BOUND AND me->notification IS NOT BOUND.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>notification_data_empty.
  ENDIF.

  uri = '/registration/' && registration_id.

  IF notification IS BOUND.
    payload = notification->get_payload( ).
  ELSE.
    payload = me->notification->get_payload( ).
  ENDIF.

  response = send_push(
    uri     = uri
    payload = payload ).

ENDMETHOD.


METHOD push_to_app_registration_list.

  DATA: payload              TYPE string,
        notification_payload TYPE string.

  DATA uri TYPE string.

  IF registration_list[] IS INITIAL.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>registration_list_empty.

  ELSEIF notification IS NOT BOUND AND me->notification IS NOT BOUND.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>notification_data_empty.
  ENDIF.

  uri = '/registration'.

  IF notification IS BOUND.
    notification_payload = notification->get_payload( ).
  ELSE.
    notification_payload = me->notification->get_payload( ).
  ENDIF.

  payload = '{ "notification": '
    && notification_payload && ', '
    && serialize_registration_list( registration_list ) && '}'.

  response = send_push(
    uri     = uri
    payload = payload ).

ENDMETHOD.


METHOD push_to_app_users.

  DATA: payload              TYPE string,
        notification_payload TYPE string.

  DATA uri TYPE string.

  IF application_id IS INITIAL.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>application_id_empty.

  ELSEIF users[] IS INITIAL.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>user_list_empty.

  ELSEIF notification IS NOT BOUND AND me->notification IS NOT BOUND.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>notification_data_empty.
  ENDIF.

  uri = '/application/' && application_id && '/user'.

  IF notification IS BOUND.
    notification_payload = notification->get_payload( ).
  ELSE.
    notification_payload = me->notification->get_payload( ).
  ENDIF.

  payload = '{ "notification": '
    && notification_payload && ', '
    && serialize_user_list( users ) && '}'.

  response = send_push(
    uri     = uri
    payload = payload ).

ENDMETHOD.


METHOD push_to_app_user_devices.

  DATA payload TYPE string.

  DATA uri TYPE string.

  IF application_id IS INITIAL.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>application_id_empty.

  ELSEIF user_name IS INITIAL.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>user_name_empty.

  ELSEIF notification IS NOT BOUND AND me->notification IS NOT BOUND.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>notification_data_empty.
  ENDIF.

  uri = '/application/' && application_id && '/user/' && user_name.

  IF notification IS BOUND.
    payload = notification->get_payload( ).
  ELSE.
    payload = me->notification->get_payload( ).
  ENDIF.

  response = send_push(
    uri     = uri
    payload = payload ).

ENDMETHOD.


METHOD send_push.

  DATA payload_x TYPE xstring.

  DATA http_client TYPE REF TO if_http_client.

  IF me->http_client IS BOUND.
    http_client = me->http_client.
  ELSE.
    http_client = create_connection( ).
  ENDIF.

  cl_http_utility=>set_request_uri( request = http_client->request
                                    uri     = uri ).

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = payload
    IMPORTING
      buffer = payload_x.

  http_client->request->set_method( 'POST' ).
  http_client->request->set_content_type( 'application/json' ).
  http_client->request->set_data( payload_x ).

  http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2 ).

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>send_request_failed.
  ENDIF.

  http_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3 ).

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>receive_response_failed.
  ENDIF.

  response = http_client->response.

ENDMETHOD.


METHOD serialize_notification_data.

  IF notification_data-alert IS NOT INITIAL.
    json = '"alert": "' && notification_data-alert && '"'.
  ENDIF.

  IF notification_data-badge IS NOT INITIAL.
    IF json IS INITIAL.
      json = '"badge": ' && notification_data-badge.
    ELSE.
      json = json && ', "badge": ' && notification_data-badge.
    ENDIF.
  ENDIF.

  IF notification_data-data IS NOT INITIAL.
    IF json IS INITIAL.
      json = '"data": "' && notification_data-data && '"'.
    ELSE.
      json = json && ', "data": "' && notification_data-data && '"'.
    ENDIF.
  ENDIF.

  IF notification_data-sound IS NOT INITIAL.
    IF json IS INITIAL.
      json = '"sound": "' && notification_data-sound && '"'.
    ELSE.
      json = json && ', "sound": "' && notification_data-sound && '"'.
    ENDIF.
  ENDIF.

  json = '{' && json && '}'.

ENDMETHOD.


METHOD serialize_registration_list.

  FIELD-SYMBOLS <registration> TYPE zscpms_app_registration_s.

  DATA length TYPE i.

  json = '"registrations": ['.

  LOOP AT registration_list ASSIGNING <registration>.
    json = json && ' "' && <registration>-id && '",'.
  ENDLOOP.

  "Remove last character
  length = strlen( json ).
  length = length - 1.
  json = json+0(length).

  json = json && ']'.

ENDMETHOD.


METHOD serialize_user_list.

  FIELD-SYMBOLS <user> TYPE zscpms_user_s.

  DATA length TYPE i.

  json = '"users": ['.

  LOOP AT user_list ASSIGNING <user>.
    json = json && ' "' && <user>-name && '",'.
  ENDLOOP.

  "Remove last character
  length = strlen( json ).
  length = length - 1.
  json = json+0(length).

  json = json && ']'.

ENDMETHOD.


METHOD test_url.

  DATA payload_x TYPE xstring.

  DATA http_client TYPE REF TO if_http_client.

  cl_http_client=>create_by_url(
    EXPORTING
      url                = url
    IMPORTING
      client             = http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4 ).

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>create_url_failed.
  ENDIF.

  http_client->authenticate(
    username = ''
    password = '' ).

  http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2 ).

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>send_request_failed.
  ENDIF.

  http_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3 ).

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_scpms_push_notification
      EXPORTING
        textid = zcx_scpms_push_notification=>receive_response_failed.
  ENDIF.

  response = http_client->response.

ENDMETHOD.
ENDCLASS.
