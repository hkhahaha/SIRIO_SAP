METHOD zii_si_oa2sap_infoadd_in~si_oa2sap_infoadd_in.
    *** **** INSERT IMPLEMENTATION HERE **** ***
        DATA:lo_server_context  TYPE REF TO if_ws_server_context.
        DATA:g_message_protocol TYPE REF TO if_wsprotocol_message_id.
        DATA:g_message_id       TYPE sxmsguid.
        DATA:g_lo_sys_exception TYPE REF TO cx_ai_system_fault.
        DATA:lv_intf_id(6) TYPE c,
             lv_err_info   TYPE bapi_msg.
    
        DATA:ls_standard    TYPE zexchange_fault_data,
             ls_detail_data TYPE zexchange_log_data.
    
        DATA:lt_zqmt052_ret TYPE zdt_oa2sap_infoadd_ret_zmd_tab,
             ls_zqmt052_ret TYPE zdt_oa2sap_infoadd_ret_zmd001.
    *----------------------------------------------------------------------
    * 检查接口是否激活
    *----------------------------------------------------------------------
        lv_intf_id = 'QM052'. "销售订单上传APS->SAP
    
        CALL FUNCTION 'Z_PI_CK'
          EXPORTING
            intf_id  = lv_intf_id
          IMPORTING
            err_info = lv_err_info.
    
        IF lv_err_info IS NOT INITIAL.
          ls_zqmt052_ret-message = '接口没有启用'.
          ls_zqmt052_ret-msg_type = 'E'.
          APPEND ls_zqmt052_ret TO lt_zqmt052_ret.
          EXIT.
        ENDIF.
    
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
    *     DATA(l_guid) = zcl_bc_public=>get_guid( ). "生成本次调用唯一标识
        zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
        iv_intid = 'QM052' iv_ptype = 'I'
        is_data = input ). "记录输入参数
    
    
    *----------------------------------------------------------------------
    *   函数处理
    *----------------------------------------------------------------------
        CALL FUNCTION 'Z_OA2SAP_INFOADD'
          EXPORTING
            input      = input
            message_id = g_message_id
          IMPORTING
            output     = output.
    
    
    *    "输出参数记录日志
        zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
        iv_intid = 'QM052' iv_ptype = 'E'
        is_data = output ). "记录输入参数
    
      ENDMETHOD.