class ZCX_SCPMS_PUSH_NOTIFICATION definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of APPLICATION_ID_EMPTY,
      msgid type symsgid value 'ZSCPMS_PUSH_NOTIF',
      msgno type symsgno value '001',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of APPLICATION_ID_EMPTY .
  constants:
    begin of REGISTRATION_ID_EMPTY,
      msgid type symsgid value 'ZSCPMS_PUSH_NOTIF',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of REGISTRATION_ID_EMPTY .
  constants:
    begin of REGISTRATION_LIST_EMPTY,
      msgid type symsgid value 'ZSCPMS_PUSH_NOTIF',
      msgno type symsgno value '003',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of REGISTRATION_LIST_EMPTY .
  constants:
    begin of USER_LIST_EMPTY,
      msgid type symsgid value 'ZSCPMS_PUSH_NOTIF',
      msgno type symsgno value '004',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of USER_LIST_EMPTY .
  constants:
    begin of SUBACCOUNT_URL_NOT_DEFINED,
      msgid type symsgid value 'ZSCPMS_PUSH_NOTIF',
      msgno type symsgno value '005',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SUBACCOUNT_URL_NOT_DEFINED .
  constants:
    begin of NOTIFICATION_DATA_EMPTY,
      msgid type symsgid value 'ZSCPMS_PUSH_NOTIF',
      msgno type symsgno value '009',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NOTIFICATION_DATA_EMPTY .
  constants:
    begin of USER_NAME_EMPTY,
      msgid type symsgid value 'ZSCPMS_PUSH_NOTIF',
      msgno type symsgno value '010',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of USER_NAME_EMPTY .
  constants:
    begin of CREATE_URL_FAILED,
      msgid type symsgid value 'ZSCPMS_PUSH_NOTIF',
      msgno type symsgno value '006',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CREATE_URL_FAILED .
  constants:
    begin of SEND_REQUEST_FAILED,
      msgid type symsgid value 'ZSCPMS_PUSH_NOTIF',
      msgno type symsgno value '007',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SEND_REQUEST_FAILED .
  constants:
    begin of RECEIVE_RESPONSE_FAILED,
      msgid type symsgid value 'ZSCPMS_PUSH_NOTIF',
      msgno type symsgno value '008',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of RECEIVE_RESPONSE_FAILED .
  constants:
    begin of CLOSE_CONNECTION_FAILED,
      msgid type symsgid value 'ZSCPMS_PUSH_NOTIF',
      msgno type symsgno value '011',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CLOSE_CONNECTION_FAILED .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_SCPMS_PUSH_NOTIFICATION IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.
ENDCLASS.
