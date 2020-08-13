CLASS zcl_scpms_push_notification DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPE-POOLS abap.

    CLASS-METHODS get_instance
      IMPORTING
        !destination_name TYPE rfcdest
      RETURNING
        VALUE(push_notification) TYPE REF TO zcl_scpms_push_notification
      RAISING
        zcx_scpms_push_notification.

    METHODS push_to_application
      IMPORTING
        !application_id TYPE zscpms_application_id
        !notification   TYPE REF TO zif_scpms_notification
      RETURNING
        VALUE(response) TYPE REF TO if_http_response
      RAISING
        zcx_scpms_push_notification.

    METHODS push_to_app_user_devices
      IMPORTING
        !application_id TYPE zscpms_application_id
        !user_name      TYPE zscpms_user_name
        !notification   TYPE REF TO zif_scpms_notification
      RETURNING
        VALUE(response) TYPE REF TO if_http_response
      RAISING
        zcx_scpms_push_notification.

    METHODS push_to_app_users
      IMPORTING
        !application_id TYPE zscpms_application_id
        !users          TYPE zscpms_user_t
        !notification   TYPE REF TO zif_scpms_notification
      RETURNING
        VALUE(response) TYPE REF TO if_http_response
      RAISING
        zcx_scpms_push_notification.

    METHODS push_to_app_registration
      IMPORTING
        !registration_id TYPE zscpms_app_registration_id
        !notification    TYPE REF TO zif_scpms_notification
      RETURNING
        VALUE(response)  TYPE REF TO if_http_response
      RAISING
        zcx_scpms_push_notification.

    METHODS push_to_app_registration_list
      IMPORTING
        !registration_list TYPE zscpms_app_registration_t
        !notification      TYPE REF TO zif_scpms_notification
      RETURNING
        VALUE(response)    TYPE REF TO if_http_response
      RAISING
        zcx_scpms_push_notification.

    METHODS close_connection
      RAISING
        zcx_scpms_push_notification.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA http_client TYPE REF TO if_http_client.
    DATA destination_name TYPE rfcdest.

    METHODS constructor
      IMPORTING
        !destination_name TYPE rfcdest
        !http_client TYPE REF TO if_http_client.

    METHODS send_push
      IMPORTING
        !uri            TYPE string
        !payload        TYPE string
      RETURNING
        VALUE(response) TYPE REF TO if_http_response
      RAISING
        zcx_scpms_push_notification.

    METHODS serialize_notification_data
      IMPORTING
        !notification_data TYPE zscpms_notification_data
      RETURNING
        VALUE(json)        TYPE string.

    METHODS serialize_registration_list
      IMPORTING
        !registration_list TYPE zscpms_app_registration_t
      RETURNING
        VALUE(json)        TYPE string.

    METHODS serialize_user_list
      IMPORTING
        !user_list  TYPE zscpms_user_t
      RETURNING
        VALUE(json) TYPE string.

    CLASS-METHODS create_connection
      IMPORTING
        !destination_name  TYPE rfcdest
      RETURNING
        VALUE(http_client) TYPE REF TO if_http_client
      RAISING
        zcx_scpms_push_notification.

ENDCLASS.



CLASS zcl_scpms_push_notification IMPLEMENTATION.


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

    me->destination_name = destination_name.
    me->http_client = http_client.

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

    http_client = create_connection( destination_name ).

    CREATE OBJECT push_notification
      EXPORTING
        destination_name = destination_name
        http_client      = http_client.

  ENDMETHOD.


  METHOD push_to_application.

    DATA payload TYPE string.

    DATA uri TYPE string.

    IF application_id IS INITIAL.
      RAISE EXCEPTION TYPE zcx_scpms_push_notification
        EXPORTING
          textid = zcx_scpms_push_notification=>application_id_empty.

    ELSEIF notification IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_scpms_push_notification
        EXPORTING
          textid = zcx_scpms_push_notification=>notification_data_empty.
    ENDIF.

    uri = '/application/' && application_id.

    payload = notification->get_payload( ).

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

    ELSEIF notification IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_scpms_push_notification
        EXPORTING
          textid = zcx_scpms_push_notification=>notification_data_empty.
    ENDIF.

    uri = '/registration/' && registration_id.

    payload = notification->get_payload( ).

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

    ELSEIF notification IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_scpms_push_notification
        EXPORTING
          textid = zcx_scpms_push_notification=>notification_data_empty.
    ENDIF.

    uri = '/registration'.

    notification_payload = notification->get_payload( ).

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

    ELSEIF notification IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_scpms_push_notification
        EXPORTING
          textid = zcx_scpms_push_notification=>notification_data_empty.
    ENDIF.

    uri = '/application/' && application_id && '/user'.

    notification_payload = notification->get_payload( ).

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

    ELSEIF notification IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_scpms_push_notification
        EXPORTING
          textid = zcx_scpms_push_notification=>notification_data_empty.
    ENDIF.

    uri = '/application/' && application_id && '/user/' && user_name.

    payload = notification->get_payload( ).

    response = send_push(
      uri     = uri
      payload = payload ).

  ENDMETHOD.


  METHOD send_push.

    DATA payload_x TYPE xstring.

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

ENDCLASS.
