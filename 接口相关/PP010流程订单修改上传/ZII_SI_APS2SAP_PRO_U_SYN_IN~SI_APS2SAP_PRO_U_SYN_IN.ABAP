METHOD zii_si_aps2sap_pro_u_syn_in~si_aps2sap_pro_u_syn_in.
    DATA:lo_server_context  TYPE REF TO if_ws_server_context.
    DATA:g_message_protocol TYPE REF TO if_wsprotocol_message_id.
    DATA:g_message_id       TYPE zbc_guid."sxmsguid.
    DATA:g_lo_sys_exception TYPE REF TO cx_ai_system_fault.
    DATA:lv_intf_id  TYPE ze_intf_id,
         lv_err_info TYPE bapi_msg.

    DATA:ls_standard    TYPE zexchange_fault_data,
         ls_detail_data TYPE zexchange_log_data.

    CLEAR:ls_standard,ls_detail_data,g_message_id,lv_intf_id,lv_err_info.

*----------------------------------------------------------------------
* 检查接口是否激活
*----------------------------------------------------------------------
    lv_intf_id = 'PP010'. "计划订单上传APS->SAP
*----------------------------------------------------------------------
*   获取接口Message ID
*----------------------------------------------------------------------
    TRY .
        lo_server_context   = cl_proxy_access=>get_server_context( ).
        g_message_protocol ?= lo_server_context->get_protocol( if_wsprotocol=>message_id ).
*       获取Message ID
        g_message_id = g_message_protocol->get_message_id( ).

      CATCH cx_ai_system_fault INTO g_lo_sys_exception.
        EXIT.
    ENDTRY.
*----------------------------------------------------------------------
*   日志记录
*----------------------------------------------------------------------
    DATA(l_flag) = zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id ) iv_intid = lv_intf_id iv_ptype = 'I' is_data = input ). "记录输入数据
    IF l_flag EQ 'A'."接口是否启用判断
      output-mt_aps2sap_pro_u_ret-type = 'E'.
      output-mt_aps2sap_pro_u_ret-message = '接口未激活，调用失败！'.
      EXIT.
    ELSE.
      output-mt_aps2sap_pro_u_ret-type = 'S'.
      output-mt_aps2sap_pro_u_ret-message = '接口调用成功，详细处理结果请查询上面的接口返回表'.
    ENDIF.
*----------------------------------------------------------------------
*   接口逻辑处理
*----------------------------------------------------------------------
    IF g_message_id IS NOT INITIAL.
      CALL FUNCTION 'Z_PP_PI_PROCESS_ORDER_CHANGE1'
        EXPORTING
          input      = input
          message_id = g_message_id
        IMPORTING
          output     = output.
      "记录返回数据
      zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id ) iv_intid = lv_intf_id iv_ptype = 'E' is_data = output ). "记录返回数据
    ENDIF.
  ENDMETHOD.