CLASS zcl_log_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_log
      IMPORTING
        !iv_balobj    TYPE balobj_d
        !iv_balsubobj TYPE balsubobj
      RETURNING
        VALUE(ro_log) TYPE REF TO zif_log
      RAISING
        zcx_log_exception .

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-METHODS init_cust
      IMPORTING
        !iv_balobj     TYPE balobj_d
        !iv_balsubobj  TYPE balsubobj
      EXPORTING
        !es_msg_filter TYPE bal_s_mfil
        !es_log_cust   TYPE zlog_cust
      RAISING
        zcx_log_exception .
ENDCLASS.



CLASS ZCL_LOG_FACTORY IMPLEMENTATION.


  METHOD get_log.

    init_cust(
      EXPORTING
        iv_balobj         = iv_balobj
        iv_balsubobj      = iv_balsubobj
      IMPORTING
        es_msg_filter     = DATA(ls_msg_filter)
        es_log_cust       = DATA(ls_log_cust)
    ).

    ro_log = NEW zcl_log(
        iv_balobj         = iv_balobj
        iv_balsubobj      = iv_balsubobj
        iv_inactive       = ls_log_cust-bal_deactivate
        is_msg_filter     = ls_msg_filter
        iv_max_day_del    = ls_log_cust-max_days
    ).
  ENDMETHOD.


  METHOD init_cust.
    SELECT SINGLE * FROM zlog_cust WHERE balobj = @iv_balobj AND balsubobj = @iv_balsubobj INTO @es_log_cust.
    IF sy-subrc <> 0.
      "Kein Customzing für &1 - &2 vorhanden.
      RAISE EXCEPTION TYPE zcx_log_exception MESSAGE e000(zmasc_log) WITH iv_balobj iv_balsubobj.
    ENDIF.

    "Alle Problemklassen die schwerwigender sind als die maximale Problemklasse werden geloggt.
    IF es_log_cust-max_balprobcl IS INITIAL.
      es_log_cust-max_balprobcl = 4.
    ENDIF.

    es_msg_filter-probclass = VALUE #( ( sign = 'I' option = 'LE' low = es_log_cust-max_balprobcl ) ).
  ENDMETHOD.
ENDCLASS.
