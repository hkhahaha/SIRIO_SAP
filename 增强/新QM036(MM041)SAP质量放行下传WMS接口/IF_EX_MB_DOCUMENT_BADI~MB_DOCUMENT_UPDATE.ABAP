  METHOD if_ex_mb_document_badi~mb_document_update.
*&---------------------------------------------------------------------*
*& 程序名：ZCL_IM_EQM001=================CM002
*&作者：Seashell Huang
*&模块：
*&创建日期：11.10.2019 15:20:33
*&功能描述：
*&---------------------------------------------------------------------*
*&修改记录：
*&
*&---------------------------------------------------------------------*


    DATA:lt_xmseg TYPE TABLE OF ty_t_mseg,
         ls_xmseg TYPE mseg.

    "定义接口字段的类型，用于存储数据
    TYPES:BEGIN OF ty_out.
    TYPES:wms_custid TYPE    string  , "  WMS货主ID
          mblnr      TYPE    string  , "  物料凭证编号
          mjahr      TYPE    string  , "  物料凭证的年份
          zeile      TYPE    string  , "  物料凭证中的项目
          bus_type   TYPE    string  , "  业务类型
          bwart      TYPE    string  , " 移动类型
          werks      TYPE    string  , "  工厂
          lgort      TYPE    string  , "  发出库存地点
          note       TYPE    string  , "  备注
          matnr      TYPE    string  , "  物料号
          maktx      TYPE    string  , "  物料名称
          charg      TYPE    string  , "  SAP批号
          wms_ext_st TYPE    string  , "  WMS状态控制字
          qmrel_quan TYPE    string  , "  数量
          dlgort     TYPE    string  , "  目标库存地点
          stock_st   TYPE    string  , "  库存状态
          meins      TYPE    string  , "  计量单位
          mseht      TYPE    string  , "  计量单位文本
          END OF ty_out.
    DATA:lv_e_objek TYPE  cuobn,
         lv_e_obtab TYPE  tabelle,
         lv_e_klart TYPE  klassenart,
         lv_e_class TYPE  klasse_d.

    DATA:lv_objectkey      TYPE  bapi1003_key-object,
         lv_objecttable    TYPE  bapi1003_key-objecttable,
         lv_classnum       TYPE  bapi1003_key-classnum,
         lv_classtype      TYPE  bapi1003_key-classtype,
         lv_objectkey_long TYPE  bapi1003_key-object_long.

    DATA: ls_key             TYPE bapi1003_key,
          lv_status          TYPE bapi1003_key-status,
          lt_allocvaluesnum  TYPE TABLE OF bapi1003_alloc_values_num,
          ls_allocvaluesnum  TYPE bapi1003_alloc_values_num,
          lt_allocvalueschar TYPE TABLE OF bapi1003_alloc_values_char,
          ls_allocvalueschar TYPE bapi1003_alloc_values_char,
          lt_allocvaluescurr TYPE TABLE OF bapi1003_alloc_values_curr,
          ls_allocvaluescurr TYPE bapi1003_alloc_values_curr,
          lt_return          TYPE TABLE OF bapiret2,
          ls_return          TYPE bapiret2.
    DATA:lt_out   TYPE TABLE OF ty_out,
         ls_out   TYPE ty_out,
         lt_makt  TYPE TABLE OF makt,
         ls_makt  TYPE makt,
         ls_t006a TYPE t006a.
    DATA:ls_zmmwms_ext_st TYPE zmmwms_ext_st.
    DATA: lv_intf_id(6) TYPE c,
          lv_guid       TYPE sxmsguid,
          lv_err_info   TYPE bapi_msg.
    DATA:ls_output      TYPE zdt_wms_qmrel_ret,
         lr_service     TYPE REF TO zco_si_sap2wms_qmrel_out,
         ls_output_main TYPE zmt_wms_qmrel,
         ls_output2     TYPE zdt_wms_subk,
         ls_input       TYPE zmt_wms_qmrel_ret,
         lt_output      TYPE zdt_wms_qmrel_linesofqm_tab,
         ls_output_line TYPE zdt_wms_qmrel_linesofqm.
*    DEFINE  message_exit.
*      output-success = &1.
*      output-message = &2.
*      IF &3 = 'X'.
*        EXIT.
*      ENDIF.
*    END-OF-DEFINITION.
    "SY-TCODE = 'QA11' 或者SY-TCODE = 'QA12' 时
    IF sy-tcode = 'QA11' OR sy-tcode = 'QA12'.
*----------------------------------------------------------------------
* 检查接口是否激活
*----------------------------------------------------------------------
      lv_intf_id = 'MM041'. "销售订单上传APS->SAP

      CALL FUNCTION 'Z_PI_CK'
        EXPORTING
          intf_id  = lv_intf_id
        IMPORTING
          err_info = lv_err_info.

      IF lv_err_info IS NOT INITIAL.
        ls_output-returndesc =  '接口开关没有打开'.
        ls_output-returncode =  'E'.
      ENDIF.
*获取接口Message ID
      DATA: lr_server_context   TYPE REF TO if_ws_server_context,
            lr_message_protocol TYPE REF TO if_wsprotocol_message_id.
      TRY .
          lr_server_context  = cl_proxy_access=>get_server_context( ).
          lr_message_protocol ?= lr_server_context->get_protocol( if_wsprotocol=>message_id ).
          lv_guid = lr_message_protocol->get_message_id( ).
        CATCH cx_ai_system_fault INTO DATA(lr_sys_exception).
          EXIT.
      ENDTRY.







      LOOP AT xmseg INTO ls_xmseg.
        "当移动类型为321/350/331时
        IF ls_xmseg-bwart = '321' OR ls_xmseg-bwart = '350' OR ls_xmseg-bwart = '331'.
          "当移动类型为321或者350时，取SHKZG = 'S''
          IF ls_xmseg-bwart = '321' OR ls_xmseg-bwart = '350'.
            CHECK ls_xmseg-shkzg = 'S'.
          ENDIF.

          " WMS_CUSTID  WMS货主ID
          ls_out-wms_custid = ls_xmseg-werks.
          "	MBLNR	物料凭证编号
          ls_out-mblnr = ls_xmseg-mblnr.
          "	MJAHR	物料凭证的年份
          ls_out-mjahr = ls_xmseg-mjahr.
          "	ZEILE	物料凭证中的项目
          ls_out-zeile = ls_xmseg-zeile.
          " BUS_TYPE  业务类型
          ls_out-bus_type = 'C03'.
          "移动类型BWART
          ls_out-bwart = ls_xmseg-bwart.

          "	WERKS	工厂
          ls_out-werks = ls_xmseg-werks.
          "	LGORT	发出库存地点
          ls_out-lgort = ls_xmseg-lgort.
          " NOTE  备注

          "	MAKTX	物料名称
          SELECT SINGLE
            maktx
          INTO CORRESPONDING FIELDS OF ls_makt
          FROM makt
          WHERE makt~spras = '1'
            AND makt~matnr = ls_xmseg-matnr.
          ls_out-maktx = ls_makt-maktx.
          "	MATNR	物料号
          ls_xmseg-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_xmseg-matnr i_out = 'X' ).
          ls_out-matnr = ls_xmseg-matnr.
          "	CHARG	SAP批号
          ls_out-charg = ls_xmseg-charg.
          " WMS_EXT_ST  WMS状态控制字
          "外寄标识ZOUT_X
          "质量标准ZST_CODE
          "XCONSIGN寄售库存标记
          IF ls_xmseg-matnr IS NOT INITIAL AND ls_xmseg-werks IS NOT INITIAL AND ls_xmseg-charg IS NOT INITIAL.
            CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
              EXPORTING
                i_matnr = ls_xmseg-matnr
                i_charg = ls_xmseg-charg
                i_werks = ls_xmseg-werks
              IMPORTING
                e_objek = lv_e_objek
                e_obtab = lv_e_obtab
                e_klart = lv_e_klart
                e_class = lv_e_class.
**          获取批次对象的属性信息
            IF sy-subrc = 0.
              lv_objectkey  =    lv_e_objek.
              lv_objecttable   = lv_e_obtab.
              lv_classnum  = lv_e_class.
              lv_classtype  = lv_e_klart.
              lv_objectkey_long = lv_e_objek.
              CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
                EXPORTING
                  objectkey       = lv_objectkey
                  objecttable     = lv_objecttable
                  classnum        = lv_classnum
                  classtype       = lv_classtype
                  objectkey_long  = lv_objectkey_long
                TABLES
                  allocvaluesnum  = lt_allocvaluesnum
                  allocvalueschar = lt_allocvalueschar
                  allocvaluescurr = lt_allocvaluescurr
                  return          = lt_return.
              IF lt_return IS NOT INITIAL AND lt_return[ 1 ]-type = 'S'.
              ENDIF.
            ENDIF.
            "质量标识
            READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZST_CODE'.
            IF sy-subrc = 0.
              ls_zmmwms_ext_st-zst_code = ls_allocvalueschar-value_char.
            ENDIF.
            "外寄标识
            READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZOUT_X'.
            IF sy-subrc = 0.
              ls_zmmwms_ext_st-zout_x = ls_allocvalueschar-value_char.
            ENDIF.
            " 获取寄售标识
            SELECT SINGLE
            sobkz
            INTO ls_zmmwms_ext_st-xconsign
            FROM mkol
            WHERE mkol~matnr = ls_xmseg-matnr
            AND mkol~werks = ls_xmseg-werks
            AND mkol~charg = ls_xmseg-charg.

            SELECT SINGLE
            wms_ext_st
            INTO ls_zmmwms_ext_st-wms_ext_st
            FROM zmmwms_ext_st
            WHERE zmmwms_ext_st~zout_x = ls_zmmwms_ext_st-zout_x
            AND zmmwms_ext_st~zst_code = ls_zmmwms_ext_st-zst_code
            AND zmmwms_ext_st~xconsign = ls_zmmwms_ext_st-xconsign.

            ls_out-wms_ext_st = ls_zmmwms_ext_st-wms_ext_st.
          ENDIF.

          " QMREL_QUAN  数量
          ls_out-qmrel_quan = ls_xmseg-menge.
          " DLGORT  目标库存地点
          IF ls_xmseg-bwart = '331'.
            ls_out-dlgort = ls_xmseg-lgort.
          ELSE.
            ls_out-dlgort = ls_xmseg-umlgo.
          ENDIF.

          " STOCK_ST  库存状态
          "空-非限制321
          IF ls_xmseg-bwart = '321'.
            ls_out-stock_st = ''.
            "B-冻结350
          ELSEIF ls_xmseg-bwart = '331'.
            ls_out-stock_st = 'B'.
            "331-C
          ELSEIF ls_xmseg-bwart = '331'.
            ls_out-stock_st = 'C'.
          ENDIF.
          "	MEINS	计量单位
          ls_out-meins = ls_xmseg-meins.
          "	MSEHT	计量单位文本
          SELECT SINGLE
           mseht
          INTO CORRESPONDING FIELDS OF ls_t006a
          FROM t006a
          WHERE t006a~spras = '1'
          AND t006a~msehi = ls_xmseg-meins.
          ls_out-mseht = ls_t006a-mseht.

          MOVE-CORRESPONDING ls_out TO ls_output_line.
          APPEND ls_output_line TO ls_output_main-mt_wms_qmrel-linesofqm.
        ENDIF.
      ENDLOOP.



*日志记录

      DATA: ls_log  TYPE zbctintlog2,
            l_json  TYPE string,
            lr_type TYPE REF TO cl_abap_typedescr.
      CLEAR ls_log.
      ls_log-guid  = cl_system_uuid=>create_uuid_c32_static( ).
      ls_log-logid = CONV #( lv_guid ).
      IF ls_log-logid IS INITIAL. cl_fdt_uuid=>get_uuid( ). ENDIF.
      ls_log-intid = 'MM041'.
      ls_log-ptype = 'I'.
      ls_log-cname = sy-uname.
      ls_log-cdate = sy-datum.
      ls_log-ctime = sy-uzeit.
      ls_log-mtype = 'S'.
      ls_log-message = '接口调用成功！'.


      TRY.
          lr_type ?= cl_abap_typedescr=>describe_by_data( ls_output_main ).
          IF lr_type IS BOUND.
            IF lr_type->is_ddic_type( ).
              ls_log-struc = lr_type->absolute_name+6(100).
              ls_log-content = /ui2/cl_json=>serialize( data = ls_output_main ).
            ELSE.
              ls_log-mtype = 'E'.
              ls_log-message = ls_log-message && '非DDIC类型无法记录日志！'.
            ENDIF.
          ENDIF.
        CATCH cx_root INTO DATA(lr_root).
          ls_log-mtype = 'E'.
          ls_log-message = lr_root->get_text( ).
      ENDTRY.

      INSERT zbctintlog2 FROM ls_log.



*      TRY.
*          CREATE OBJECT lr_service.
*        CATCH cx_ai_system_fault .
*          MESSAGE '创建代理类失败，联系IT' TYPE 'E'.
*      ENDTRY.

*  接口调用
      DATA: lv_error           TYPE c,

            lo_api             TYPE REF TO zco_si_sap2wms_qmrel_out,
            lo_system_fault    TYPE REF TO cx_ai_system_fault,
            lo_async_messaging TYPE REF TO if_wsprotocol_async_messaging,
            l_ack_request      TYPE prx_ack_request_details.

      CREATE OBJECT lo_api.
      TRY.
*      CALL METHOD lo_api->si_wms_subk_ret
          CALL METHOD lo_api->si_sap2wms_qmrel_out
            EXPORTING
              output = ls_output_main
            IMPORTING
              input  = ls_input.
        CATCH cx_ai_system_fault INTO lo_system_fault.
          lv_error = 'E'.
      ENDTRY.
      IF lv_error <> 'E'.
        MESSAGE '数据下发成功！' TYPE 'S'.
      ENDIF.

    ENDIF.

  ENDMETHOD.