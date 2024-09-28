interface ZIF_LOG
  public .


  methods ADD_MESSAGE
    importing
      !IV_BALPROBCL type BALPROBCL default '1' .
  methods ADD_EXCEPTION
    importing
      !IO_EXCEPTION type ref to CX_ROOT
      !IV_PROBCL type BALPROBCL default '1' .
  methods CREATE_LOG
    importing
      !IV_BALNREXT type BALNREXT
    raising
      ZCX_LOG_EXCEPTION .
endinterface.
