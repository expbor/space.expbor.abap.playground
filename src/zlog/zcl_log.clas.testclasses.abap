*"* use this source file for your ABAP unit test classes

CLASS ltcl_log DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltcl_Log
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_LOG
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zif_log.  "class under test

    CLASS-DATA: mc_balobj TYPE balobj_d VALUE 'ZLOG'.
    CLASS-DATA: mc_balobjt TYPE balobjt_d VALUE 'LOG Objekt für ABAP Unit Test'.
    CLASS-DATA: mc_balsubobj TYPE balsubobj VALUE 'ZSUB'.
    CLASS-DATA: mc_balsubobjt TYPE balsubobjt VALUE 'SUBLOG Objekt für ABAP Unit Test'.
    CLASS-DATA: mv_teardown TYPE abap_bool VALUE abap_true.
    DATA: mc_balnrext TYPE balnrext VALUE 'ABAP UNIT Test'.

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: add_exception FOR TESTING.
    METHODS: add_message FOR TESTING.
    METHODS: add_multi_message FOR TESTING.
    METHODS: add_different_probclass FOR TESTING.
    METHODS: add_new_entry FOR TESTING.
    METHODS: ignore_rollback FOR TESTING.
ENDCLASS.       "ltcl_Log


CLASS ltcl_log IMPLEMENTATION.
  METHOD class_setup.
    "SLG0 Eintrag erstellen
    DATA: ls_balobj TYPE balobj.
    DATA: ls_balobjt TYPE balobjt.
    DATA: ls_balsub TYPE balsub.
    DATA: ls_balsubt TYPE balsubt.

    SELECT SINGLE * FROM balobj WHERE object = @mc_balobj INTO @DATA(ls_obj_exist).
    IF sy-subrc = 0.
      "BAL Objekt existiert bereits.
      mv_teardown = abap_false.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    SELECT SINGLE * FROM balsub WHERE object = @mc_balobj AND subobject = @mc_balsubobj INTO @DATA(ls_sub_obj_exist).
    IF sy-subrc = 0.
      "BAL Subobjekt existiert bereits.
      mv_teardown = abap_false.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    ls_balobj-object = mc_balobj.

    ls_balobjt-object = mc_balobj.
    ls_balobjt-objtxt = mc_balobjt.
    ls_balobjt-spras = sy-langu.

    INSERT balobj FROM ls_balobj.
    IF sy-subrc <> 0.
      mv_teardown = abap_false.
      cl_abap_unit_assert=>fail( ).
    ENDIF.
    INSERT balobjt FROM ls_balobjt.
    IF sy-subrc <> 0.
      mv_teardown = abap_false.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    ls_balsub-object = mc_balobj.
    ls_balsub-subobject = mc_balsubobj.

    ls_balsubt-object = mc_balobj.
    ls_balsubt-subobject = mc_balsubobj.
    ls_balsubt-spras = sy-langu.
    ls_balsubt-subobjtxt = mc_balsubobjt.

    INSERT balsub FROM ls_balsub.
    IF sy-subrc <> 0.
      mv_teardown = abap_false.
      cl_abap_unit_assert=>fail( ).
    ENDIF.
    INSERT balsubt FROM ls_balsubt.
    IF sy-subrc <> 0.
      mv_teardown = abap_false.
      cl_abap_unit_assert=>fail( ).
    ENDIF.

    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD class_teardown.
    IF mv_teardown = abap_false.
      RETURN.
    ENDIF.

    DELETE FROM balobj WHERE object = mc_balobj.
    DELETE FROM balobjt WHERE object = mc_balobj AND spras = sy-langu.
    DELETE FROM balsub WHERE object = mc_balobj AND subobject = mc_balsubobj.
    DELETE FROM balsubt WHERE object = mc_balobj AND subobject = mc_balsubobj AND spras = sy-langu.

    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD setup.
    TRY.
        f_cut = zcl_log_factory=>get_log(
                iv_balobj         = mc_balobj
                iv_balsubobj      = mc_balsubobj
            ).

        f_cut->create_log( iv_balnrext = mc_balnrext ).
      CATCH zcx_log_exception.  "
        cl_abap_unit_assert=>fail( ).
    ENDTRY.
  ENDMETHOD.

  METHOD add_exception.
    TRY.
        "Test Message miit Variable 1: &1 2: &2 3: &3 4: &4
        RAISE EXCEPTION TYPE zcx_log_exception MESSAGE i001(zlog) WITH 'a' 'b' 'c' 'd'.
      CATCH zcx_log_exception INTO DATA(lx_exception).
        f_cut->add_exception(
          EXPORTING
            io_exception = lx_exception    " Dynamischer Zugriff auf T100-Texte, mit Attributen
        ).
    ENDTRY.
  ENDMETHOD.


  METHOD add_message.
    MESSAGE i001(zlog) WITH 'a' 'b' 'c' 'd' INTO DATA(lv_message).
    f_cut->add_message( ).

  ENDMETHOD.

  METHOD add_multi_message.
    MESSAGE i001(zlog) WITH 'a' 'b' 'c' 'd' INTO DATA(lv_message).
    f_cut->add_message( ).

    MESSAGE w001(zlog) WITH 'a' 'b' 'c' 'd' INTO lv_message.
    f_cut->add_message( ).

    MESSAGE s001(zlog) WITH 'a' 'b' 'c' 'd' INTO lv_message.
    f_cut->add_message( ).
  ENDMETHOD.

  METHOD add_different_probclass.
    MESSAGE i001(zlog) WITH 'a' 'b' 'c' 'd' INTO DATA(lv_message).
    f_cut->add_message(
        iv_balprobcl = '2'
    ).

    MESSAGE w001(zlog) WITH 'a' 'b' 'c' 'd' INTO lv_message.
    f_cut->add_message(
        iv_balprobcl = '2'
    ).

    MESSAGE s001(zlog) WITH 'a' 'b' 'c' 'd' INTO lv_message.
    f_cut->add_message(
        iv_balprobcl = '2'
    ).

    MESSAGE i001(zlog) WITH 'a' 'b' 'c' 'd' INTO lv_message.
    f_cut->add_message(
        iv_balprobcl = '3'
    ).

    MESSAGE w001(zlog) WITH 'a' 'b' 'c' 'd' INTO lv_message.
    f_cut->add_message(
        iv_balprobcl = '3'
    ).

    MESSAGE s001(zlog) WITH 'a' 'b' 'c' 'd' INTO lv_message.
    f_cut->add_message(
        iv_balprobcl = '3'
    ).

    MESSAGE i001(zlog) WITH 'a' 'b' 'c' 'd' INTO lv_message.
    f_cut->add_message(
        iv_balprobcl = '4'
    ).

    MESSAGE w001(zlog) WITH 'a' 'b' 'c' 'd' INTO lv_message.
    f_cut->add_message(
        iv_balprobcl = '4'
    ).

    MESSAGE s001(zlog) WITH 'a' 'b' 'c' 'd' INTO lv_message.
    f_cut->add_message(
        iv_balprobcl = '4'
    ).
  ENDMETHOD.

  METHOD add_new_entry.
    MESSAGE i001(zlog) WITH 'a' 'b' 'c' 'd' INTO DATA(lv_message).
    f_cut->add_message( ).

    TRY.
        f_cut->create_log( iv_balnrext = mc_balnrext ).
      CATCH zcx_log_exception.    "
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

    MESSAGE w001(zlog) WITH 'a' 'b' 'c' 'd' INTO lv_message.
    f_cut->add_message( ).
  ENDMETHOD.

  METHOD ignore_rollback.
    MESSAGE i001(zlog) WITH 'a' 'b' 'c' 'd' INTO DATA(lv_message).
    f_cut->add_message( ).

    ROLLBACK WORK.

    MESSAGE w001(zlog) WITH 'a' 'b' 'c' 'd' INTO lv_message.
    f_cut->add_message( ).
  ENDMETHOD.
ENDCLASS.
