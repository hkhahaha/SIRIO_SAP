  METHOD if_ex_me_process_po_cust~post.

*    MM022-委外采购订单下传WMS
*    CALL FUNCTION 'ZMM_OUT_PURCHASE_SAP2WMS_FUN' STARTING NEW TASK 'ZMM_OUT_PURCHASE_SAP2WMS_FUN'
*      EXPORTING
*        im_ebeln  = im_ebeln.
*    DATA ls_header TYPE mepoheader.
*    ls_header = im_header->get_data( ).
*采购订单保存的时候更新APS计划
    CALL FUNCTION 'Z_MM_CHANGE_APS_PLAN'
      EXPORTING
        im_ebeln  = im_ebeln
        im_header = im_header
        iv_type   = '1'.

    DATA ls_header TYPE mepoheader.
    ls_header = im_header->get_data( ).
    IF sy-tcode = 'ME29N' OR sy-tcode = 'ME28' AND ls_header-frgke = 'F'.
      CALL FUNCTION 'Z_MM_SAP2WMS_OUT_FUN' " MM022委外采购订单下传WMS
        EXPORTING
          im_header = ls_header-ebeln.
*      IMPORTING
*        ch_failed = ch_failed.
    ENDIF.


  ENDMETHOD.