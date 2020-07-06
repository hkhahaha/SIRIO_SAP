FUNCTION z_oa2sap_infoadd.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ZMT_OA2SAP_INFOADD
*"     REFERENCE(MESSAGE_ID) TYPE  SXMSGUID
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZMT_OA2SAP_INFOADD_RET
*"----------------------------------------------------------------------
  DATA: lt_zqmt052 TYPE TABLE OF zqmt052,
        ls_zqmt052 LIKE LINE OF lt_zqmt052.

  DATA:lt_head   TYPE zdt_oa2sap_infoadd_head_tab,
       ls_head   TYPE zdt_oa2sap_infoadd_head,
       lt_mara   TYPE zdt_oa2sap_infoadd_mara_tab,
       ls_mara   TYPE zdt_oa2sap_infoadd_mara,
       lt_marc   TYPE zdt_oa2sap_infoadd_werks_tab,
       ls_marc   TYPE zdt_oa2sap_infoadd_werks,
       lt_output TYPE zdt_oa2sap_infoadd_ret_zmd_tab,
       ls_output TYPE zdt_oa2sap_infoadd_ret_zmd001.



  lt_head = input-mt_oa2sap_infoadd-head.
  LOOP AT lt_head INTO ls_head.
    lt_mara = ls_head-mara.
    LOOP AT lt_mara INTO ls_mara.
      MOVE-CORRESPONDING ls_mara TO ls_zqmt052.
      ls_zqmt052-zif = '否'.
      ls_zqmt052-zcreate_date = sy-datum.
      ls_zqmt052-zcreate_account = sy-ucomm.
      ls_zqmt052-id = zcl_bc_public=>get_guid( ).
      APPEND ls_zqmt052 TO lt_zqmt052.
      CLEAR:ls_mara.
    ENDLOOP.
    CLEAR ls_head.
  ENDLOOP.
  INSERT zqmt052 FROM TABLE lt_zqmt052.
  IF sy-subrc = 0.
    ls_output-msg_type = 'S'.
    ls_output-message = '成功'.
    APPEND ls_output TO lt_output.
    output-mt_oa2sap_infoadd_ret-zmd001 = lt_output.
  ELSE.
    ls_output-msg_type = 'E'.
    ls_output-MESSAGE = '失败'.
    APPEND ls_output TO lt_output.
    OUTPUT-mt_oa2sap_infoadd_ret-zmd001 = lt_output.
  ENDIF.
  CLEAR lt_zqmt052.

ENDFUNCTION.