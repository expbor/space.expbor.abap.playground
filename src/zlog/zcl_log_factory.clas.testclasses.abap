*"* use this source file for your ABAP unit test classes

CLASS ltcl_log_factory DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltcl_Log_Factory
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_LOG_FACTORY
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.

    METHODS: get_log_defined FOR TESTING.
    METHODS: get_log_not_defined FOR TESTING.
ENDCLASS.       "ltcl_Log_Factory


CLASS ltcl_log_factory IMPLEMENTATION.

  METHOD get_log_defined.

    DATA iv_balobj TYPE balobj_d VALUE 'ZLOG'.
    DATA iv_balsubobj TYPE balsubobj VALUE 'ZSUB'.
    DATA ro_log TYPE REF TO zif_log.

    TRY.
        ro_log = zcl_log_factory=>get_log(
            iv_balobj = iv_balobj
            iv_balsubobj = iv_balsubobj ).
      CATCH zcx_log_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act              = ro_log
    ).


  ENDMETHOD.

  METHOD get_log_not_defined.
    DATA iv_balobj TYPE balobj_d VALUE 'ZLOG'.
    DATA iv_balsubobj2 TYPE balsubobj VALUE 'ZSUB1'.
    DATA ro_log TYPE REF TO zif_log.

    TRY.
        ro_log = zcl_log_factory=>get_log(
            iv_balobj = iv_balobj
            iv_balsubobj = iv_balsubobj2 ).

        cl_abap_unit_assert=>fail( ).
      CATCH zcx_log_exception.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
