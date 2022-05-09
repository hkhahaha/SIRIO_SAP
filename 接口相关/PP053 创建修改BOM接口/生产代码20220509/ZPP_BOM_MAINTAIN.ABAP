FUNCTION zpp_bom_maintain.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_INPUT) TYPE  ZDT_PLM2SAP_CREATECHANGE_BOM
*"  EXPORTING
*"     VALUE(E_OUTPUT) TYPE  ZDT_PLM2SAP_CREATECHANGE_BOM_R
*"----------------------------------------------------------------------

  DATA: lv_ret_msg  TYPE zdt_plm2sap_createchange_bom_1,
        lt_ret_msg  TYPE zdt_plm2sap_createchange__tab1,
        l_data      LIKE LINE OF i_input-bom_header,
        l_change_no LIKE aenrb-aennr,    "ECN号码
        l_flag      TYPE c.
  g_chtxt = i_input-remark.
  g_guid_input  = i_input-guid.
*  g_flag  = i_input-flag.
  g_cn_rfq_code  = i_input-cn_rfq_code.

  "先删除，后新建/修改
  CLEAR:l_flag.
  REFRESH: lt_ret_msg.
  LOOP AT i_input-bom_header INTO l_data. " WHERE flag = 'D'.
    PERFORM frm_maintain_bom USING l_data CHANGING l_change_no lv_ret_msg.
    APPEND lv_ret_msg TO lt_ret_msg.
    IF lv_ret_msg-type = 'E'.
      l_flag = 'X'.
    ENDIF.
  ENDLOOP.
*  IF l_flag IS NOT INITIAL.
*    RETURN.
*  ENDIF.
*
*  LOOP AT i_input-bom_header INTO l_data WHERE flag NE 'D'.
*    PERFORM frm_maintain_bom USING l_data CHANGING l_change_no lv_ret_msg.
*    APPEND lv_ret_msg TO lt_ret_msg.
*    IF lv_ret_msg-type = 'E'.
*      l_flag = 'X'.
*    ENDIF.
*  ENDLOOP.

  "按Input的顺序返回output的值
  LOOP AT i_input-bom_header INTO l_data.
    READ TABLE lt_ret_msg INTO lv_ret_msg WITH KEY "guid     = l_data-guid
                                                   plant    = l_data-plant
                                                   material = l_data-material
*                                                   stlnr    = l_data-stlnr
                                                   stlal    = l_data-stlal
                                                   .
    IF sy-subrc = 0.
      APPEND lv_ret_msg TO e_output-detail.
    ENDIF.

  ENDLOOP.

  IF i_input-bom_header[] IS INITIAL.
    CLEAR: lv_ret_msg.
    lv_ret_msg-type = 'E'.
    lv_ret_msg-message = '传入参数为空'.
    APPEND lv_ret_msg TO e_output-detail.
  ENDIF.

ENDFUNCTION.