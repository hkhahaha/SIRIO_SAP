METHOD zii_si_oa2other_custom_in~si_oa2other_custom_in.
    *** **** INSERT IMPLEMENTATION HERE **** ***
        DATA: lo_server_context TYPE REF TO if_ws_server_context.
        DATA: g_message_protocol TYPE REF TO if_wsprotocol_message_id.
        DATA: g_message_id TYPE sxmsguid.
        DATA: g_lo_sys_exception TYPE REF TO cx_ai_system_fault.
        DATA: lv_intf_id(6) TYPE c,
              lv_err_info   TYPE bapi_msg.
        DATA: lw_return TYPE zdt_oa2other_custom_ret.
    
    
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
    
        CLEAR lv_intf_id.
    
        lv_intf_id = input-mt_oa2other_custom-common-intf_id.  "接口编号
        "记录入参日志&检查接口激活状态
        DATA(lv_flag) = zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
                                      iv_intid = lv_intf_id iv_ptype = 'I'
                                      is_data = input ).
    
    
        IF lv_flag NE 'S'.
          output-mt_oa2other_custom_ret-message = '接口没有激活'.
          output-mt_oa2other_custom_ret-type = 'E'.
        ELSE.
    
          "接口逻辑处理
          IF g_message_id IS NOT INITIAL.
            CALL FUNCTION 'ZSD_MAINTAIN_CUSTOMER_BP'
              EXPORTING
                i_input  = input-mt_oa2other_custom
              IMPORTING
                e_output = output-MT_OA2OTHER_CUSTOM_RET.
          ENDIF.
        ENDIF.
    
        zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
                                  iv_intid = lv_intf_id iv_ptype = 'E'
                                  is_data  = output ).
      ENDMETHOD.