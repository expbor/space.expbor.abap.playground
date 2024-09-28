CLASS zcl_matrix_sort DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_matrix_line.
    TYPES dimx TYPE int4.
    TYPES dimy TYPE int4.
    TYPES line TYPE int4.
    TYPES END OF ty_matrix_line .
    TYPES:
      ty_marix TYPE STANDARD TABLE OF ty_matrix_line .

    CONSTANTS mc_pi TYPE f VALUE '3.14159265359' ##NO_TEXT.

    CLASS-METHODS deg_in_rad
      IMPORTING
        !iv_deg       TYPE decfloat16
      RETURNING
        VALUE(rv_rad) TYPE f .
    CLASS-METHODS sort
      IMPORTING
        !iv_dimx  TYPE zde_count_dim
        !iv_dimy  TYPE zde_count_dim
        !iv_deg   TYPE decfloat16
      CHANGING
        !ct_table TYPE STANDARD TABLE
      RAISING
        zcx_matrix .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MATRIX_SORT IMPLEMENTATION.


  METHOD deg_in_rad.
    rv_rad = ( mc_pi * iv_deg ) / 180.
  ENDMETHOD.


  METHOD sort.
    DATA: lt_matrix TYPE ty_marix.
    DATA: ls_matrix TYPE ty_matrix_line.
    DATA: lv_table_index TYPE int4 VALUE 0.
    DATA: lr_new_table TYPE REF TO data.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    DATA(lv_element_count) = iv_dimx * iv_dimy.

    IF lines( ct_table ) <> lv_element_count.
      "Anzahl Zeilen <> Anzahl Elemente Matrix
      RAISE EXCEPTION TYPE zcx_matrix MESSAGE e000.
    ENDIF.

    "Matrix aufbauen
    DO iv_dimy TIMES.
      DATA(lv_y) = sy-index.
      DO iv_dimx TIMES.
        DATA(lv_x) = sy-index.
        ls_matrix-dimx = lv_x.
        ls_matrix-dimy = - lv_y.
        lv_table_index = lv_table_index + 1.
        ls_matrix-line = lv_table_index.
        APPEND ls_matrix TO lt_matrix.
      ENDDO.
    ENDDO.

    "Winkel in Rad umrechnen
    DATA(lv_radian) = deg_in_rad( iv_deg = iv_deg ).
    "Matrix drehen
    LOOP AT lt_matrix ASSIGNING FIELD-SYMBOL(<ls_matrix>).
      DATA(lv_newx) = <ls_matrix>-dimx * cos( lv_radian ) - <ls_matrix>-dimy * sin( lv_radian ).
      DATA(lv_newy) = <ls_matrix>-dimx * sin( lv_radian ) + <ls_matrix>-dimy * cos( lv_radian ).

      <ls_matrix>-dimx = lv_newx.
      <ls_matrix>-dimy = lv_newy.
    ENDLOOP.

    "Tabelle sortieren
    SORT lt_matrix BY dimy DESCENDING dimx ASCENDING.

    CREATE DATA lr_new_table LIKE ct_table.
    ASSIGN lr_new_table->* TO <lt_table>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_matrix ASSIGNING <ls_matrix>.
      APPEND ct_table[ <ls_matrix>-line ] TO <lt_table>.
    ENDLOOP.

    ct_table = <lt_table>.
  ENDMETHOD.
ENDCLASS.
