  METHOD zii_si_wms2sap_batch_chars_in~si_wms2sap_batch_chars_in.
*** **** INSERT IMPLEMENTATION HERE **** ***


    DATA: lo_server_context  TYPE REF TO if_ws_server_context,
          g_message_protocol TYPE REF TO if_wsprotocol_message_id,
          g_message_id       TYPE sxmsguid,
          g_lo_sys_exception TYPE REF TO cx_ai_system_fault,
          lv_intf_id(6)      TYPE c,
          lv_err_info        TYPE bapi_msg.

    DATA: ls_standard TYPE zexchange_fault_data.
    DATA: ls_detail_data TYPE zexchange_log_data.




    "检查接口激活状态
    lv_intf_id = 'MM029'."WMS批次属性上传SAP



    "----------------------------------------------------------------------
    "获取接口Message ID
    "----------------------------------------------------------------------
    G_MESSAGE_ID = ZCL_BC_PUBLIC=>get_guid(  ).
    "日志记录

    zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
                                    iv_intid = lv_intf_id iv_ptype = 'X'
                                    is_data = input ). "记录输入参数

    DATA(lv_ret) = zcl_bc_public=>int_actived( lv_intf_id ).

    IF lv_ret = abap_false.
      output-mt_wms2sap_batch_chars_ret-ret_code = 'E'.
      output-mt_wms2sap_batch_chars_ret-ret_msg = '接口没有激活'.
    ELSE.
      "接口逻辑处理
      IF g_message_id IS NOT INITIAL.
        CALL FUNCTION 'Z_MM_PI_BATCH_CHARS'
          EXPORTING
            input      = input
          IMPORTING
            output     = output.
      ENDIF.
    ENDIF.

    "输出参数记录日志
    zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
                                  iv_intid = lv_intf_id iv_ptype = 'Y'
                                  is_data = output ).



  ENDMETHOD.