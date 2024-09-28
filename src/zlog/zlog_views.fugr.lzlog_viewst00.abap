*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLOG_CUST.......................................*
DATA:  BEGIN OF STATUS_ZLOG_CUST                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLOG_CUST                     .
CONTROLS: TCTRL_ZLOG_CUST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLOG_CUST                     .
TABLES: ZLOG_CUST                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
