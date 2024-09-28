*"* use this source file for your ABAP unit test classes

CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_Test
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_MATRIX_SORT
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
    METHODS: sort FOR TESTING.
ENDCLASS.       "ltc_Test


CLASS ltcl_test IMPLEMENTATION.

  METHOD sort.

    DATA lv_dimx TYPE zde_count_dim VALUE 3.
    DATA lv_dimy TYPE zde_count_dim VALUE 2.
    DATA lv_deg TYPE decfloat16 VALUE 90.
    DATA lt_table TYPE TABLE OF string.
    DATA lt_table_exp TYPE TABLE OF string.

    lt_table = VALUE #( ( `1` ) ( `2` ) ( `3` ) ( `4` ) ( `5` ) ( `6` ) ).
    lt_table_exp = VALUE #( ( `3` ) ( `6` ) ( `2` ) ( `5` ) ( `1` ) ( `4` ) ).

    TRY.
        zcl_matrix_sort=>sort(
          EXPORTING
            iv_dimx    = lv_dimx    " Anzahl Elemente Dimension
            iv_dimy    = lv_dimy    " Anzahl Elemente Dimension
            iv_deg     = lv_deg         " Winkel in Grad
          CHANGING
            ct_table   = lt_table    " Tabelle die Sortiert werden soll
        ).

        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = lt_table
            exp                  = lt_table_exp
        ).

      CATCH zcx_matrix.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
