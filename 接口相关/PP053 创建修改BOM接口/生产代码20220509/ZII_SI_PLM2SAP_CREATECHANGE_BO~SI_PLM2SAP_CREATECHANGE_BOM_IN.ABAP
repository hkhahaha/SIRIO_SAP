METHOD zii_si_plm2sap_createchange_bo~si_plm2sap_createchange_bom_in.
    *** **** INSERT IMPLEMENTATION HERE **** ***
    
    *    "日志记录
    *    DATA:LV_GUID       TYPE SXMSGUID.
    *    LV_GUID = ZCL_BC_PUBLIC=>GET_GUID( ).
    *    DATA(L_FLAG) = ZCL_BC_PUBLIC=>WRITE_LOG( IV_LOGID = CONV #( LV_GUID )
    *                                             IV_INTID = 'PP053'    "OA传输SAP规格标准
    *                                             IV_PTYPE = 'I'
    *                                             IS_DATA = INPUT ). "记录输入参数
    *    CALL FUNCTION 'ZPP_BOM_CREATE'
    *      EXPORTING
    *        I_DATA   = INPUT
    *      IMPORTING
    *        O_OUTPUT = OUTPUT.
    *
    *
    *    ZCL_BC_PUBLIC=>WRITE_LOG( IV_LOGID = CONV #( LV_GUID )
    *                              IV_INTID = 'PP053'
    *                              IV_PTYPE = 'E'
    *                              IS_DATA = OUTPUT ).
    
        DATA: lo_server_context TYPE REF TO if_ws_server_context.
        DATA: g_message_protocol TYPE REF TO if_wsprotocol_message_id.
        DATA: g_message_id TYPE sxmsguid.
        DATA: g_lo_sys_exception TYPE REF TO cx_ai_system_fault.
        DATA: lv_intf_id(6) TYPE c,
              lv_err_info   TYPE bapi_msg.
        DATA: lv_ret_msg  TYPE zdt_plm2sap_createchange_bom_1.
    
    
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
    
        lv_intf_id = 'PP053'. "input-mt_oa2sap_bp-intf_id."供应商接口编号
        "记录入参日志&检查接口激活状态
        DATA(lv_flag) = zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
                                      iv_intid = lv_intf_id iv_ptype = 'I'
                                      is_data = input ).
    
        IF lv_flag NE 'S'.
          CLEAR: lv_ret_msg.
          MOVE-CORRESPONDING input-mt_plm2sap_createchange_bom TO lv_ret_msg.
          lv_ret_msg-message = '接口没有激活'.
          lv_ret_msg-type = 'E'.
          APPEND lv_ret_msg TO output-mt_plm2sap_createchange_bom_re-detail.
        ELSE.
    
          "接口逻辑处理
          IF g_message_id IS NOT INITIAL.
            CALL FUNCTION 'ZPP_BOM_MAINTAIN'
              EXPORTING
                i_input  = input-mt_plm2sap_createchange_bom
              IMPORTING
                e_output = output-mt_plm2sap_createchange_bom_re.
          ENDIF.
        ENDIF.
    
        "记录出参日志
        zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
                                  iv_intid = lv_intf_id iv_ptype = 'E'
                                  is_data  = output ).
      ENDMETHOD.