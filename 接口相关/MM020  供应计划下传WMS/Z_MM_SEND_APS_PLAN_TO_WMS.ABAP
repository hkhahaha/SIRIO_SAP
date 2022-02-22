FUNCTION z_mm_send_aps_plan_to_wms.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(INPUT) TYPE  ZSZMMWMS_POH_POL
*"     VALUE(IV_INTF_ID) TYPE  ZE_INTF_ID OPTIONAL
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  ZSMESSAGE_OUT
*"  TABLES
*"      IT_PO_ITEM STRUCTURE  MEPOITEM OPTIONAL
*"----------------------------------------------------------------------

  DATA: lr_service      TYPE REF TO zco_si_sap2wms_dlvplan_out,
        lr_system_fault TYPE REF TO cx_ai_system_fault,
        ls_output       TYPE zmt_sap2wms_dlvplan,
        ls_item         TYPE zdt_sap2wms_dlvplan_sub,
        ls_input        TYPE zmt_sap2wms_dlvplan_ret.

  DATA:lv_intf_id(6) TYPE c,
       lv_guid       TYPE sxmsguid.

  DATA : lv_lifnr TYPE lfa1-lifnr .

  DATA:lv_return TYPE c.

  DEFINE message_exit.
    output-success = &1.
    output-message = &2.
    IF &3 = 'X'.
      EXIT.
    ENDIF.
  END-OF-DEFINITION.

*获取接口Message ID
  lv_guid = zcl_bc_public=>get_guid( ).

  CLEAR: ls_output, ls_input.

  MOVE-CORRESPONDING input-head TO ls_output-mt_sap2wms_dlvplan.

*供应商简称
  SELECT SINGLE sortl
    INTO ls_output-mt_sap2wms_dlvplan-sortl
    FROM lfa1
   WHERE lifnr = ls_output-mt_sap2wms_dlvplan-lifnr.

  "ADD BY LANJINGCUN 20201207 供应商名称丢失问题-S47
*供应商名称
  IF ls_output-mt_sap2wms_dlvplan-name1_gp = '' .
    lv_lifnr = ls_output-mt_sap2wms_dlvplan-lifnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_lifnr
      IMPORTING
        output = lv_lifnr.
    SELECT SINGLE name1
      INTO ls_output-mt_sap2wms_dlvplan-name1_gp
      FROM lfa1
     WHERE lifnr = ls_output-mt_sap2wms_dlvplan-lifnr.
  ENDIF .
  "ADD BY LANJINGCUN 20201207 供应商名称丢失问题-E47

  ls_output-mt_sap2wms_dlvplan-bus_type = 'A01'.
  SHIFT ls_output-mt_sap2wms_dlvplan-lifnr LEFT DELETING LEADING '0'.



  CLEAR lv_return.
  LOOP AT input-item INTO DATA(ls_pol) WHERE dlv_planno = input-head-dlv_planno.

**********************add by sujx 20191204*****************************
* 行项目中有一条不在ZMM_LGORT 就不下发
    SELECT SINGLE wms_sys INTO @DATA(lv_wms_sys)
      FROM zmm_lgort
     WHERE werks = @ls_pol-werks
       AND lgort = @ls_pol-lgort.
    IF lv_wms_sys IS INITIAL .
      lv_return = 'X'.
      EXIT. "  有一条不符合的逻辑就退出循环
    ENDIF.
***********************end add ********************************************
    IF ls_output-mt_sap2wms_dlvplan-wms_custid IS INITIAL.
      SELECT SINGLE wms_custid
        INTO ls_output-mt_sap2wms_dlvplan-wms_custid
        FROM zmmwms_sys
       WHERE werks = ls_pol-werks.
    ENDIF.

    MOVE-CORRESPONDING ls_pol TO ls_item.

***************** pdlv_date_ext 为空和为初始化不是一个意思  add by sujx 20191213 *********
    IF  ls_pol-pdlv_date_ext = ''.
      CLEAR  ls_pol-pdlv_date_ext .
    ENDIF.
*******************************************************************************************
    IF ls_pol-pdlv_date_ext IS NOT INITIAL.
      ls_item-pdlv_date = ls_pol-pdlv_date_ext.
    ENDIF.

**计量单位文本
*    SELECT SINGLE mseht
*      INTO ls_item-mseht
*      FROM t006a
*     WHERE spras = sy-langu
*       AND msehi = ls_item-meins.

*物料描述
    SELECT SINGLE maktx
      INTO ls_item-maktx
      FROM makt
     WHERE matnr = ls_item-matnr
       AND spras = sy-langu.

    IF ls_item-producer IS INITIAL.
      SELECT SINGLE mfrnr INTO ls_item-producer FROM mara WHERE matnr = ls_item-matnr.
    ENDIF.

*供应商信息
    SELECT SINGLE name1
      INTO ls_item-producer_name
      FROM lfa1
     WHERE lifnr = ls_item-producer.

*采购订单信息
    IF it_po_item[] IS NOT INITIAL.
      READ TABLE it_po_item INTO DATA(ls_po_item) WITH KEY ebeln = ls_item-ebeln ebelp = ls_item-ebelp.
      IF sy-subrc = 0.
        ls_item-uebto = ls_po_item-uebto.
*        ls_item-untto = ls_po_item-untto.
        ls_item-insmk = ls_po_item-insmk.
*        ls_item-loekz = ls_po_item-loekz.
*        ls_item-abskz = ls_po_item-abskz.
*        ls_item-elikz = ls_po_item-elikz.
        ls_item-menge = ls_po_item-menge.
        IF ls_po_item-pstyp = '2'.
          ls_item-consign_flag = 'K'.
        ENDIF.
      ENDIF.
    ELSE.
      SELECT SINGLE  ekpo~uebto,
                     ekpo~untto,
                     ekpo~insmk,
                     ekpo~loekz,
                     ekpo~abskz,
                     ekpo~elikz,
                     ekpo~menge,
                     ekpo~pstyp
        FROM ekpo AS ekpo
       WHERE ekpo~ebeln = @ls_item-ebeln
         AND ekpo~ebelp = @ls_item-ebelp
        INTO @DATA(ls_po).
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_po TO ls_item.
        IF ls_po-pstyp = '2'.
          ls_item-consign_flag = 'K'.
        ENDIF.
      ENDIF.
    ENDIF.
*********Seashell 20210304 添加生产商字段的判断，如果制造商字段为空的话取生产商
    IF ls_item-producer IS INITIAL OR ls_item-producer = ''.
      SELECT SINGLE zmfrnr INTO ls_item-producer FROM ekpo WHERE ekpo~ebeln = ls_item-ebeln AND ekpo~ebelp = ls_item-ebelp.
      IF sy-subrc = 0.
        SELECT SINGLE name1
        INTO ls_item-producer_name
        FROM lfa1
        WHERE lifnr = ls_item-producer.
      ENDIF.
    ENDIF.
*******************END************
    IF ls_item-insmk = 'X'.
      ls_item-insmk = 'I'.
    ENDIF.

    SHIFT ls_item-matnr LEFT DELETING LEADING '0'.
    SHIFT ls_item-producer LEFT DELETING LEADING '0'.

    APPEND ls_item TO ls_output-mt_sap2wms_dlvplan-list_of_dlv_plan.
    CLEAR: ls_item.

  ENDLOOP.

******************  add by sujx 20191204 ******************************
* 若满足条件，返回上级处理
  IF lv_return IS NOT INITIAL  .
    RETURN.
  ENDIF.
*********************end add*************************************************

*日志记录
  DATA(l_flag) = zcl_bc_public=>write_log( iv_logid = CONV #( lv_guid )
                                           iv_intid = iv_intf_id    "OA传输SAP规格标准
                                           iv_ptype = 'I'
                                           is_data = ls_output ). "记录输入参数

  TRY.
      CREATE OBJECT lr_service.
    CATCH cx_ai_system_fault .
      message_exit 'E' '创建代理类失败，请联系IT' 'X'.
  ENDTRY.

  TRY.
      CALL METHOD lr_service->si_sap2wms_dlvplan_out
        EXPORTING
          output = ls_output
        IMPORTING
          input  = ls_input.
    CATCH cx_ai_system_fault INTO lr_system_fault.
      output-message = lr_system_fault->if_message~get_text( ).

      message_exit 'E' output-message 'X'.
  ENDTRY.


  output-message = ls_input-mt_sap2wms_dlvplan_ret-return_desc.
  IF ls_input-mt_sap2wms_dlvplan_ret-return_flag = '1'.
    output-success = 'S'.
  ELSE.
    output-success = 'E'.
  ENDIF.

  zcl_bc_public=>write_log( iv_logid = CONV #( lv_guid )
                            iv_intid = iv_intf_id
                            iv_ptype = 'E'
                            is_data = output ).

ENDFUNCTION.