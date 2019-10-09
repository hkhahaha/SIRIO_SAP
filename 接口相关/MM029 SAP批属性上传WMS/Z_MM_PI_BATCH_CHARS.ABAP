FUNCTION z_mm_pi_batch_chars. "MM029
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ZMT_WMS2SAP_BATCH_CHARS
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZMT_WMS2SAP_BATCH_CHARS_RET
*"----------------------------------------------------------------------
  DO 5 TIMES.



  PERFORM frm_chk_data USING input
                        CHANGING output.

  IF output-mt_wms2sap_batch_chars_ret-ret_code <> 'E'.
    PERFORM frm_pro_data USING input
                         CHANGING output.
  ENDIF.

  WAIT UP TO 1 SECONDS.
  ENDDO.



ENDFUNCTION.