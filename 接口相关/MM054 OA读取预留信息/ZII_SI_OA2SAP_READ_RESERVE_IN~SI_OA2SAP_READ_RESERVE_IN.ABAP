METHOD zii_si_oa2sap_read_reserve_in~si_oa2sap_read_reserve_in.
*** **** INSERT IMPLEMENTATION HERE **** ***
    
        DATA: lo_server_context TYPE REF TO if_ws_server_context.
        DATA: g_message_protocol TYPE REF TO if_wsprotocol_message_id.
        DATA: g_message_id TYPE sxmsguid.
        DATA: g_lo_sys_exception TYPE REF TO cx_ai_system_fault.
        DATA: lv_intf_id(6) TYPE c,
              lv_err_info   TYPE bapi_msg.
    
        DATA: ls_standard TYPE zexchange_fault_data.
        DATA: ls_detail_data TYPE zexchange_log_data.
    
        DATA: ls_ret TYPE zdt_oa2sap_read_reserve_ret.
    
    
        "----------------------------------------------------------------------
        "获取接口Message ID
        "----------------------------------------------------------------------
        TRY .
            lo_server_context   = cl_proxy_access=>get_server_context( ).
            g_message_protocol ?= lo_server_context->get_protocol( if_wsprotocol=>message_id ).
            "获取Message ID
            g_message_id = g_message_protocol->get_message_id( ).
    
          CATCH cx_ai_system_fault INTO g_lo_sys_exception.
            EXIT.
        ENDTRY.
        "记录入参日志
        zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
        iv_intid = 'MM054' iv_ptype = 'X'
        is_data = input ).
    
        "检查接口激活状态
        lv_intf_id = 'MM054'."销售订单变更上传SAP
    
        DATA(lv_ret) = zcl_bc_public=>int_actived( lv_intf_id ).
    
        IF lv_ret = abap_false.
          ls_ret-message = '接口没有激活'.
          ls_ret-type = 'E'.
          output-mt_oa2sap_read_reserve_ret = ls_ret.
        ELSE.
    
    
          "接口逻辑处理
          IF g_message_id IS NOT INITIAL.
            CALL FUNCTION 'Z_MM_OA2SAP_READ_RESERVE'
              EXPORTING
                input      = input
                message_id = g_message_id
              IMPORTING
                output     = output.
          ENDIF.
        ENDIF.
    
        "记录出参日志
        zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
        iv_intid = 'MM054' iv_ptype = 'Y'
        is_data = output ).
    
      ENDMETHOD.