  METHOD zii_si_aps2sap_select_tou_liao~si_aps2sap_select_tou_liao_in.
**  call method lcl_fill_data=>ZMT_APS2SAP_SELECT_TOU_LIAO_RE
**    importing
**      out = OUTPUT.

    DATA:lo_server_context  TYPE REF TO if_ws_server_context.
    DATA:g_message_protocol TYPE REF TO if_wsprotocol_message_id.
    DATA:g_message_id       TYPE sxmsguid.
    DATA:g_lo_sys_exception TYPE REF TO cx_ai_system_fault.
    DATA:lv_intf_id(6) TYPE c,
         lv_err_info   TYPE bapi_msg.


    DATA:ls_standard    TYPE zexchange_fault_data,
         ls_detail_data TYPE zexchange_log_data.
*  DATA:LT_RET TYPE TABLE OF ZDT_APS2SAP_GETSTOCK_RET,
*               LS_RET LIKE LINE OF LT_RET.

    CLEAR:ls_standard,ls_detail_data,g_message_id,lv_intf_id,lv_err_info.


*----------------------------------------------------------------------
* 检查接口是否激活
*----------------------------------------------------------------------
    lv_intf_id = 'PP084'. " APS读取SAP库存

    CALL FUNCTION 'Z_PI_CK'
      EXPORTING
        intf_id  = lv_intf_id
      IMPORTING
        err_info = lv_err_info.

    IF lv_err_info IS NOT INITIAL.
      output-mt_aps2sap_select_tou_liao_ret-message =  '接口开关没有打开'.
      output-mt_aps2sap_select_tou_liao_ret-type =  'E'.

    ELSE.
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
      iv_intid = 'PP084' iv_ptype = 'I'
      is_data = input ). "记录输入参数

*----------------------------------------------------------------------
*   接口逻辑处理
*----------------------------------------------------------------------
    IF G_MESSAGE_ID IS NOT INITIAL.
      try.
      CALL FUNCTION 'Z_PP_PP084_SELECTTOULIAO'
      EXPORTING
        INPUT      = INPUT
        MESSAGE_ID = G_MESSAGE_ID
      IMPORTING
        OUTPUT     = OUTPUT.
      CATCH cx_root INTO data(lo_root).
        OUTPUT-mt_aps2sap_select_tou_liao_ret-type = 'E'.
        OUTPUT-mt_aps2sap_select_tou_liao_ret-message = lo_root->get_text( ).
      ENDTRY.
    ENDIF.
    ENDIF.

    IF OUTPUT IS INITIAL.
      OUTPUT-mt_aps2sap_select_tou_liao_ret-type = 'E'.
      OUTPUT-mt_aps2sap_select_tou_liao_ret-message = '输入的信息可能有误，或者当前工厂不存在数据'.
    ENDIF.

    "输出参数记录日志
    zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
    iv_intid = 'PP084' iv_ptype = 'O'
    is_data = output ). "记录输入参数
  ENDMETHOD.