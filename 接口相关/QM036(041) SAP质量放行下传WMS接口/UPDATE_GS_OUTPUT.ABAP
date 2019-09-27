  METHOD update_gs_output.
    DATA: lo_server_context TYPE REF TO if_ws_server_context.
    DATA: g_message_protocol TYPE REF TO if_wsprotocol_message_id.
    DATA: g_message_id TYPE sxmsguid.
    DATA: g_lo_sys_exception TYPE REF TO cx_ai_system_fault.
    DATA: lv_intf_id(6) TYPE c,
          lv_err_info   TYPE bapi_msg.

    DATA: ls_standard TYPE zexchange_fault_data.
    DATA: ls_detail_data TYPE zexchange_log_data.
*----------------------------------------------------------------------
* 检查接口是否激活
*----------------------------------------------------------------------
  LV_INTF_ID = 'MM041'. "销售订单上传APS->SAP

  CALL FUNCTION 'Z_PI_CK'
  EXPORTING
    INTF_ID  = LV_INTF_ID
  IMPORTING
    ERR_INFO = LV_ERR_INFO.

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
    " 记录出参的日志
    zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
    iv_intid = 'MM041' iv_ptype = 'X'
    is_data = newvalue ).
    IF sy-subrc = 0.
      " 调用接口（MM041的接口）
      DATA: lv_error           TYPE c,

            lo_api             TYPE REF TO zco_si_sap2wms_qmrel_out,
            lo_system_fault    TYPE REF TO cx_ai_system_fault,
            lo_async_messaging TYPE REF TO if_wsprotocol_async_messaging,
            l_ack_request      TYPE prx_ack_request_details.
      DATA: lv_out TYPE zmt_wms_qmrel,
            lv_in  TYPE zmt_wms_qmrel_ret.
      CREATE OBJECT lo_api.
      me->gs_output-sap_docno  = newvalue-prueflos   ."SAP检验批号
*      me->gs_output-sap_docno = zcl_bc_public=>conv_by_ddic( i_input = me->gs_output-sap_docno i_out = 'X' )."去掉前导0
      me->gs_output-werks  = newvalue-werk  ."工厂
      me->gs_output-lgort  = newvalue-lagortchrg   ."库存地点
      me->gs_output-matnr  = newvalue-matnr ."物料编码
*      <fs_output>-vbeln = zcl_bc_public=>conv_by_ddic( i_input = i_input-header-vbeln i_out = 'X' ).
      me->gs_output-matnr = zcl_bc_public=>conv_by_ddic( i_input = me->gs_output-matnr i_out = 'X' ).
      SELECT  SINGLE maktx INTO me->gs_output-maktx FROM makt WHERE spras = 1
      AND matnr = newvalue-matnr.
      me->gs_output-cg  = newvalue-charg  ."SAP批号
      me->gs_output-menge  = newvalue-lmengeist   ."总数量
      me->gs_output-qmrel_quan  = newvalue-lmenge01  ."质量放行数量
      me->gs_output-qmblk_menge  = newvalue-lmenge04 ."质量放行冻结量
      me->gs_output-qmret_menge = '0'. " 质量放行退回量
*      因为WMS字段定义为数值型，如果说字段的值是空的话，需要调整字段值为默认为0
      IF me->gs_output-qmblk_menge IS INITIAL OR me->gs_output-qmblk_menge = 0.
        me->gs_output-qmblk_menge = '0'.
      ENDIF.
      me->gs_output-meins  = newvalue-mengeneinh      ."计量单位
      me->gs_output-qmsam_menge = newvalue-lmenge03." 质量放行抽样量
      SELECT SINGLE mseht INTO me->gs_output-mseht FROM t006a
      WHERE msehi = newvalue-mengeneinh AND spras = 1 ."计量单位文本  ***
      MOVE-CORRESPONDING me->gs_output TO lv_out-mt_wms_qmrel.
      MOVE me->gs_output-cg TO lv_out-mt_wms_qmrel-charg.
      lv_out-mt_wms_qmrel-wms_custid = '8100'.
      lv_out-mt_wms_qmrel-bus_type = 'C03'.
      TRY.
          CALL METHOD lo_api->si_sap2wms_qmrel_out
            EXPORTING
              output = lv_out
            IMPORTING
              input  = lv_in.
        CATCH cx_ai_system_fault INTO lo_system_fault.
          lv_error = 'E'.
      ENDTRY.

    ENDIF.





**********************************************************************
* 调用PO 下传给WMS TODO
*
**********************************************************************

*    DATA: go_async_messaging TYPE REF TO if_wsprotocol_async_messaging,
*          go_system_fault    TYPE REF TO cx_ai_system_fault,
*          gs_output1         TYPE zco_si_sap2wms_qmrel_in, "下发给OA系统的参数
*          gv_error           TYPE char1. "下发给OA系统成功标识
*
*
*    IF ( new_ud_data-vcode ='1' ).
*
**      调用代理类下发内部订单给oa系统
*      DATA(go_sap2wms_api) = NEW zco_si_sap2wms_qmrel_out( ).
*
*      TRY .
*          go_async_messaging ?= go_sap2wms_api->get_protocol( if_wsprotocol=>async_messaging ).
*          CLEAR l_ack_request.
*          l_ack_request = if_wsprotocol_async_messaging=>co_transport_acknowledgment.
*          go_async_messaging->set_acknowledgment_requested( if_wsprotocol_async_messaging=>co_transport_acknowledgment ).
*        CATCH cx_ai_system_fault INTO go_system_fault .
*      ENDTRY.
*
*      TRY.
**           调用代理类的下发方法发送内部订单给OA系统
*          go_sap2wms_api->si_sap2wms_qmrel_out( output = gs_output1 ).
*          COMMIT WORK AND WAIT.
*        CATCH cx_ai_system_fault INTO  go_system_fault .
*          gv_error = 'E'.
*      ENDTRY.
*    ENDIF.


  ENDMETHOD.