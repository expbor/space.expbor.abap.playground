CLASS zcl_log DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_log .

    METHODS constructor
      IMPORTING
        !iv_balobj      TYPE balobj_d
        !iv_balsubobj   TYPE balsubobj
        !iv_inactive    TYPE abap_bool OPTIONAL
        !is_msg_filter  TYPE bal_s_mfil OPTIONAL
        !iv_max_day_del TYPE zde_max_day_del OPTIONAL
      RAISING
        zcx_log_exception .
  PROTECTED SECTION.

    DATA ms_bal_s_log TYPE bal_s_log .
    DATA mv_balloghndl TYPE balloghndl .
    DATA mv_max_day_del TYPE zde_max_day_del .

    METHODS save_log .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_LOG IMPLEMENTATION.


  METHOD constructor.
    DATA: ls_bal_s_conf TYPE bal_s_conf.

    ms_bal_s_log-object = iv_balobj.
    ms_bal_s_log-subobject = iv_balsubobj.

    mv_max_day_del = iv_max_day_del.
    ls_bal_s_conf-collect-inactive = iv_inactive.
    ls_bal_s_conf-collect-msg_filter = is_msg_filter.

    CALL FUNCTION 'BAL_GLB_CONFIG_SET'
      EXPORTING
        i_s_configuration = ls_bal_s_conf
      EXCEPTIONS
        not_authorized    = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_log_exception
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD save_log.
    DATA lt_bal_logh TYPE bal_t_logh.

    APPEND mv_balloghndl TO lt_bal_logh.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client             = sy-mandt
        i_in_update_task     = abap_false
        i_save_all           = abap_false
        i_t_log_handle       = lt_bal_logh
        i_2th_connection     = abap_true
        i_2th_connect_commit = abap_true
        i_link2job           = abap_true
      EXCEPTIONS
        log_not_found        = 1
        save_not_allowed     = 2
        numbering_error      = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_log_exception_no_check
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD zif_log~add_exception.
    DATA: lo_message TYPE REF TO if_t100_message.
    TRY.
        lo_message ?= io_exception.
        cl_message_helper=>set_msg_vars_for_if_t100_msg( text = lo_message ).

        sy-msgty = 'E'.
      CATCH cx_sy_message_illegal_text cx_sy_move_cast_error  INTO DATA(lx_message_exception).    "
        RAISE EXCEPTION TYPE zcx_log_exception_no_check EXPORTING previous = lx_message_exception.
    ENDTRY.
    zif_log~add_message( iv_balprobcl = iv_probcl ).
  ENDMETHOD.


  METHOD zif_log~add_message.
    DATA: ls_bal_s_msg TYPE bal_s_msg.

    ls_bal_s_msg-msgid = sy-msgid.
    ls_bal_s_msg-msgno = sy-msgno.
    ls_bal_s_msg-msgty = sy-msgty.
    ls_bal_s_msg-msgv1 = sy-msgv1.
    ls_bal_s_msg-msgv2 = sy-msgv2.
    ls_bal_s_msg-msgv3 = sy-msgv3.
    ls_bal_s_msg-msgv4 = sy-msgv4.
    ls_bal_s_msg-probclass = iv_balprobcl.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = mv_balloghndl
        i_s_msg          = ls_bal_s_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_log_exception_no_check
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    save_log( ).
  ENDMETHOD.


  METHOD zif_log~create_log.
    ms_bal_s_log-extnumber = iv_balnrext.

    IF mv_max_day_del IS NOT INITIAL.
      ms_bal_s_log-aldate_del = sy-datum + mv_max_day_del.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ms_bal_s_log
      IMPORTING
        e_log_handle            = mv_balloghndl
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_log_exception
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
