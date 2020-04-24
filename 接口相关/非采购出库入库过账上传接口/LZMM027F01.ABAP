*----------------------------------------------------------------------*
***INCLUDE LZMM027F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_MIGO_CP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ITEM
*&      <-- LS_OUT
*&---------------------------------------------------------------------*
FORM frm_migo_cp  USING    us_header TYPE zdt_wms2sap_others
                  CHANGING cs_out    TYPE zdt_wms2sap_others_ret.
  DEFINE alpha_input.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input         = &1
      IMPORTING
        output        = &1.
  END-OF-DEFINITION.
  DEFINE matn1_input.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input           = &1
      IMPORTING
        output          = &1
      EXCEPTIONS
        length_error    = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.
  END-OF-DEFINITION.
  DEFINE cunit_input.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input                = &1
        language             = sy-langu
      IMPORTING
        output               = &2
      EXCEPTIONS
        unit_not_found       = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  END-OF-DEFINITION.
  DATA: lv_noteno TYPE char10,
        lv_sapno  TYPE rsnum,
        lv_line   TYPE zsapnote_line,
*        LV_SAPNO1 TYPE CHAR10,
        lv_rspos  TYPE rspos,
        lv_aufnr  TYPE aufnr,
        lv_kdauf  TYPE kdauf,
        lv_kdpos  TYPE kdpos,
        lv_matnr  TYPE matnr,
        lv_dmatnr TYPE matnr,
        lv_meins  TYPE meins,
        lt_t0003  TYPE STANDARD TABLE OF zppt0003,
        ls_t0003  TYPE zppt0003,
        lt_nopur  TYPE STANDARD TABLE OF zmmmigo_nopur,
        ls_nopur  TYPE zmmmigo_nopur,
        lt_ret    TYPE zdt_wms2sap_others_ret_lis_tab,
        ls_ret    TYPE zdt_wms2sap_others_ret_list_of,
        lt_item   TYPE zdt_wms2sap_others_sub_tab,
        ls_item   TYPE zdt_wms2sap_others_sub.
  DATA: lw_gmhead TYPE bapi2017_gm_head_01,
        lw_gmcode TYPE bapi2017_gm_code,
        lt_gmitem TYPE TABLE OF bapi2017_gm_item_create,
        lw_gmitem TYPE bapi2017_gm_item_create,
        lt_return TYPE TABLE OF bapiret2,
        lw_return TYPE bapiret2,
        lv_ebelp  TYPE ekpo-ebelp,
        lv_mblnr  TYPE bapi2017_gm_head_ret-mat_doc,
        lv_mess   TYPE char255.

  CLEAR: lt_item, ls_item, lt_nopur, ls_nopur, lt_ret, ls_ret.
  lt_item = us_header-list_of_others_post.

  CLEAR: lw_gmhead, lw_gmcode, lt_gmitem, lw_gmitem, lt_nopur, ls_nopur, lv_mess.

  lw_gmhead-pstng_date = sy-datum.
  lw_gmhead-doc_date   = sy-datum.
  lw_gmhead-pr_uname   = sy-uname.
  lw_gmhead-header_txt = us_header-sapnote_no.
  lw_gmcode-gm_code = '01'.

  SORT lt_item BY wms_itemno.
  LOOP AT lt_item INTO ls_item.

    IF ls_item-charg IS NOT INITIAL.
      " 更新批次生产日期（通过SAP通知单号和行号去ZPPT0003取得）
      PERFORM frm_batch_change USING ls_item us_header CHANGING lv_mess.
    ENDIF.

    CLEAR: lv_rspos, lv_matnr, lv_dmatnr, lv_meins.
    lv_matnr = ls_item-matnr.
    lv_dmatnr = ls_item-dmatnr.
    matn1_input: lv_matnr.
    matn1_input: lv_dmatnr.
    lv_rspos = ls_item-sapnote_line.
    alpha_input: lv_rspos.

    cunit_input: ls_item-meins lv_meins.
    lw_gmitem-plant     = us_header-werks.             "工厂
    lw_gmitem-material  = lv_matnr.
    lw_gmitem-batch  = ls_item-charg.
    lw_gmitem-stge_loc  = ls_item-lgort.               " 库存地点
    lw_gmitem-move_type = ls_item-bwart.               " 移动类型
    lw_gmitem-mvt_ind   = ' '.
    lw_gmitem-costcenter = ls_item-kostl.              " 成本中心
    lw_gmitem-move_stloc = ls_item-dlgort.             " 库存地点-CHANGE
    lw_gmitem-move_mat = lv_dmatnr.                    " 物料-CHANGE
    lw_gmitem-move_batch = ls_item-dcharg.             " 批次-CHANGE
    lw_gmitem-move_plant = ls_item-dwerks.             " 工厂-CHANGE
    lw_gmitem-entry_qnt = ls_item-menge.               " 数量
    IF lv_meins IS NOT INITIAL.
      lw_gmitem-entry_uom = lv_meins.
    ELSE.
      lw_gmitem-entry_uom = ls_item-meins.
    ENDIF.
    lw_gmitem-spec_stock = ls_item-sobkz.              " 特殊库存标识
    lw_gmitem-stck_type = ls_item-stock_st.            " 库存类型
*    IF LV_SAPNO IS NOT INITIAL.
*      LW_GMITEM-RESERV_NO = LV_SAPNO.                  " 预留/相关需求的编号
*      LW_GMITEM-RES_ITEM = LV_RSPOS.                   " 预留 / 相关需求的项目编号
*    ENDIF.
    lw_gmitem-vendor = ls_item-lifnr.                  " 供应商号
    CLEAR: lv_kdauf, lv_kdpos, lv_aufnr.
    lv_kdauf = ls_item-kdauf.
    lv_kdpos = ls_item-kdpos.
    lv_aufnr = ls_item-aufnr.
    alpha_input: lv_kdauf.
    alpha_input: lv_kdpos.
    alpha_input: lv_aufnr.
    IF ls_item-kdauf IS NOT INITIAL.
*      LW_GMITEM-SALES_ORD = LS_ITEM-KDAUF.             " 销售订单
*      LW_GMITEM-S_ORD_ITEM = LS_ITEM-KDPOS.            " 销售订单行号
      lw_gmitem-sales_ord = lv_kdauf.
      lw_gmitem-s_ord_item = lv_kdpos.
      lw_gmitem-spec_stock = 'E'.
    ENDIF.
    IF ls_item-aufnr IS NOT INITIAL.
*      LW_GMITEM-ORDERID = LS_ITEM-AUFNR.               " 生产订单
      lw_gmitem-orderid = lv_aufnr.
      lw_gmitem-order_itno = '0001'.                   " 生产订单行号
      lw_gmitem-mvt_ind   = 'F'.
    ENDIF.
    CLEAR: ls_t0003, lv_sapno, lv_line.
    lv_sapno = us_header-sapnote_no.
    lv_line = ls_item-sapnote_line.
    alpha_input: lv_sapno.
    alpha_input: lv_line.
    SELECT SINGLE * FROM zppt0003 INTO ls_t0003
      WHERE zsapnote_no = lv_sapno AND zsapnote_line = lv_line.
    IF sy-subrc = 0.
      lw_gmitem-gr_rcpt = ls_t0003-wempf.              " 收货方
      lw_gmitem-unload_pt = ls_t0003-ablad.            " 卸货点
    ENDIF.
    APPEND lw_gmitem TO lt_gmitem.
    CLEAR: lw_gmitem.
  ENDLOOP.
  " 生成物料凭证
  IF lv_mess IS INITIAL.
    CLEAR: lv_mblnr, lt_return.
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = lw_gmhead
        goodsmvt_code    = lw_gmcode
      IMPORTING
        materialdocument = lv_mblnr
      TABLES
        goodsmvt_item    = lt_gmitem
        return           = lt_return.
    CLEAR: lw_return, lv_mess.
    LOOP AT lt_return INTO lw_return WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X'.
      CONCATENATE lv_mess '|' lw_return-message INTO lv_mess.
      CLEAR: lw_return.
    ENDLOOP.
    IF lv_mess IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      cs_out-wms_custid = us_header-wms_custid.
      cs_out-wms_docno = us_header-wms_docno.
      cs_out-ret_code = 'E'.
      cs_out-ret_msg = lv_mess.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      cs_out-wms_custid = us_header-wms_custid.
      cs_out-wms_docno = us_header-wms_docno.
      cs_out-sap_docno = lv_mblnr.
      cs_out-ret_code = 'S'.
      cs_out-ret_msg = '成功'.
      CLEAR: lv_ebelp.
      LOOP AT lt_item INTO ls_item.
        IF ls_item-bwart = '101'.
          CLEAR: lt_t0003, ls_t0003, lv_sapno, lv_line.
          lv_sapno = us_header-sapnote_no.
          lv_line = ls_item-sapnote_line.
          alpha_input: lv_sapno.
          alpha_input: lv_line.
          SELECT SINGLE * FROM zppt0003 INTO ls_t0003
            WHERE zsapnote_no = lv_sapno AND zsapnote_line = lv_line.
          IF sy-subrc = 0.
            ls_t0003-zwmsmng = ls_t0003-zwmsmng + ls_item-menge.
            APPEND ls_t0003 TO lt_t0003.
            MODIFY zppt0003 FROM TABLE lt_t0003.
          ENDIF.
        ENDIF.

        CLEAR: lv_rspos.
        lv_rspos = ls_item-sapnote_line.
        alpha_input: lv_rspos.
        ls_ret-wms_itemno = ls_item-wms_itemno.
        ls_ret-sap_itemno = lv_rspos.
        ls_ret-stock_sts = ls_item-stock_st.
        APPEND ls_ret TO lt_ret.
        CLEAR: ls_ret.

        ls_nopur-mandt = sy-mandt.
        ls_nopur-mblnr = lv_mblnr.
        ls_nopur-zeile = lv_rspos.
        ls_nopur-mjahr = sy-datum+0(4).
        ls_nopur-wms_docno = us_header-wms_docno.
        ls_nopur-wms_itemno = ls_item-wms_itemno.
        ls_nopur-sapnote_no = us_header-sapnote_no.
        ls_nopur-sapnote_line = lv_rspos.
        ls_nopur-docdate = sy-datum.
        APPEND ls_nopur TO lt_nopur.
        CLEAR: ls_nopur.
      ENDLOOP.
      cs_out-list_of_others_post_ack = lt_ret.
      MODIFY zmmmigo_nopur FROM TABLE lt_nopur.
    ENDIF.
  ELSE.
    cs_out-wms_custid = us_header-wms_custid.
    cs_out-wms_docno = us_header-wms_docno.
    cs_out-ret_code = 'E'.
    cs_out-ret_msg = lv_mess.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_MIGO_OTHER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_HEADER
*&      <-- LS_OUT
*&---------------------------------------------------------------------*
FORM frm_migo_other  USING    us_header TYPE zdt_wms2sap_others
                     CHANGING cs_out    TYPE zdt_wms2sap_others_ret.
  DEFINE alpha_input.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input         = &1
      IMPORTING
        output        = &1.
  END-OF-DEFINITION.
  DEFINE matn1_input.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input           = &1
      IMPORTING
        output          = &1
      EXCEPTIONS
        length_error    = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.
  END-OF-DEFINITION.
  DEFINE cunit_input.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input                = &1
        language             = sy-langu
      IMPORTING
        output               = &2
      EXCEPTIONS
        unit_not_found       = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  END-OF-DEFINITION.
  DATA: lv_noteno TYPE char10,
        lv_sapno  TYPE rsnum,
        lv_sapno1 TYPE char10,
        lv_sapno2 TYPE char10,
        lv_line   TYPE zsapnote_line,
        lv_rspos  TYPE rspos,
        lv_matnr  TYPE matnr,
        lv_dmatnr TYPE matnr,
        lv_lifnr  TYPE lifnr,
        lv_aufnr  TYPE aufnr,
        lv_kdauf  TYPE kdauf,
        lv_kdpos  TYPE kdpos,
        lv_flag   TYPE c,
        lv_meins  TYPE meins,
        lt_t0003  TYPE STANDARD TABLE OF zppt0003,
        ls_t0003  TYPE zppt0003,
        lt_nopur  TYPE STANDARD TABLE OF zmmmigo_nopur,
        ls_nopur  TYPE zmmmigo_nopur,
        lt_ret    TYPE zdt_wms2sap_others_ret_lis_tab,
        ls_ret    TYPE zdt_wms2sap_others_ret_list_of,
        lt_item   TYPE zdt_wms2sap_others_sub_tab,
        ls_item   TYPE zdt_wms2sap_others_sub.
  DATA: lw_gmhead TYPE bapi2017_gm_head_01,
        lw_gmcode TYPE bapi2017_gm_code,
        lt_gmitem TYPE TABLE OF bapi2017_gm_item_create,
        lw_gmitem TYPE bapi2017_gm_item_create,
        lt_return TYPE TABLE OF bapiret2,
        lw_return TYPE bapiret2,
        ls_resb   TYPE resb,
        lv_ebelp  TYPE ekpo-ebelp,
        lv_mblnr1 TYPE bapi2017_gm_head_ret-mat_doc,
        lv_mblnr2 TYPE bapi2017_gm_head_ret-mat_doc,
        lv_mess   TYPE char255.

  CLEAR: lt_item, ls_item, lt_nopur, ls_nopur, lt_ret, ls_ret.
  lt_item = us_header-list_of_others_post.
  " 通过流水号取预留单
  IF us_header-sapnote_no IS NOT INITIAL.
    CLEAR: lv_noteno, lv_sapno.
    lv_noteno = us_header-sapnote_no.
    SELECT SINGLE rsnum FROM zmm026_record INTO lv_sapno
      WHERE zreserv_no = lv_noteno.
    IF sy-subrc <> 0.
      " 通过流水号取通知单
      CLEAR: lv_sapno1.
      SELECT SINGLE zsapnote_no FROM zmm_changeno INTO lv_sapno1
        WHERE zsapnote_no = lv_noteno.
    ENDIF.
  ENDIF.

  CLEAR: lw_gmhead, lw_gmcode, lt_gmitem, lw_gmitem, lt_nopur, ls_nopur, lv_flag.

  lw_gmhead-pstng_date = sy-datum.
  lw_gmhead-doc_date   = sy-datum.
  lw_gmhead-pr_uname   = sy-uname.
  IF lv_sapno1 IS NOT INITIAL.
    lw_gmhead-header_txt = lv_sapno1.
  ELSE.
    lw_gmhead-header_txt = ''.
  ENDIF.
  lw_gmcode-gm_code = '04'.

  SORT lt_item BY wms_itemno.
  LOOP AT lt_item INTO ls_item.

    CLEAR: lv_rspos, lv_matnr, lv_dmatnr, lv_lifnr.
    lv_matnr = ls_item-matnr.
    lv_dmatnr = ls_item-dmatnr.
    lv_lifnr = ls_item-lifnr.
    matn1_input: lv_matnr.
    matn1_input: lv_dmatnr.
    alpha_input: lv_lifnr.
    lv_rspos = ls_item-sapnote_line.
    alpha_input: lv_rspos.
    alpha_input: lv_sapno.
    IF ls_item-bwart = '201'
      OR ls_item-bwart = '291' "add by ljm20191218
      OR ls_item-bwart = 'Z55' "add by waymon 20191214.
      OR ls_item-bwart = 'Y61'
      OR ls_item-bwart = 'Z61'.
      lw_gmcode-gm_code = '03'.
    ELSEIF ls_item-bwart = '511'
      OR ls_item-bwart = '512'
      OR ls_item-bwart = '531'
      OR ls_item-bwart = '532'.
      lw_gmcode-gm_code = '05'.
    ELSEIF ls_item-bwart = 'Y01'.
*      OR LS_ITEM-BWART = 'Y02'.
      " Y01样品入库时migo成功后需要再411/E脱单
      lw_gmcode-gm_code = '01'.
      lv_flag = 'X'.
      CLEAR: ls_t0003, lv_noteno, lv_line.
      lv_noteno = us_header-sapnote_no.
      lv_line = ls_item-sapnote_line.
      alpha_input: lv_noteno.
      alpha_input: lv_line.
      SELECT SINGLE * FROM zppt0003 INTO ls_t0003
        WHERE zsapnote_no = lv_noteno AND zsapnote_line = lv_line.
      IF sy-subrc = 0.
        lw_gmitem-gr_rcpt = ls_t0003-wempf.              " 收货方
        lw_gmitem-unload_pt = ls_t0003-ablad.            " 卸货点
      ENDIF.
    ENDIF.
    CLEAR: lv_meins.
    cunit_input: ls_item-meins lv_meins.
*    LW_GMITEM-PLANT     = US_HEADER-WERKS.             "工厂
    lw_gmitem-material  = lv_matnr.
    lw_gmitem-batch  = ls_item-charg.
*    LW_GMITEM-STGE_LOC  = LS_ITEM-LGORT.               " 库存地点
    lw_gmitem-move_type = ls_item-bwart.               " 移动类型
    lw_gmitem-mvt_ind   = ' '.
    lw_gmitem-costcenter = ls_item-kostl.              " 成本中心
*    LW_GMITEM-MOVE_STLOC = LS_ITEM-DLGORT.             " 库存地点-CHANGE
    lw_gmitem-move_mat = lv_dmatnr.                    " 物料-CHANGE
    lw_gmitem-move_batch = ls_item-dcharg.             " 批次-CHANGE
*    LW_GMITEM-MOVE_PLANT = LS_ITEM-DWERKS.             " 工厂-CHANGE
    lw_gmitem-entry_qnt = ls_item-menge.               " 数量
    IF lv_meins IS NOT INITIAL.
      lw_gmitem-entry_uom = lv_meins.
    ELSE.
      lw_gmitem-entry_uom = ls_item-meins.
    ENDIF.
    IF ls_item-stock_st = 'B'.
      lw_gmitem-stck_type = '3'.
    ELSEIF ls_item-stock_st = 'I'.
      lw_gmitem-stck_type = '2'.
    ELSEIF ls_item-stock_st = ''.
      lw_gmitem-stck_type = ''.
    ENDIF.
    lw_gmitem-spec_stock = ls_item-sobkz.              " 特殊库存标识
*    LW_GMITEM-STCK_TYPE = LS_ITEM-STOCK_ST.            " 库存类型
    IF lv_sapno IS NOT INITIAL.
      " 取预留的工厂和库存地点
      CLEAR: ls_resb.
      SELECT SINGLE rsnum rspos werks lgort umwrk umlgo kdauf kdpos
          FROM resb INTO CORRESPONDING FIELDS OF ls_resb
          WHERE rsnum = lv_sapno AND rspos = lv_rspos.
      IF sy-subrc = 0.
        lw_gmitem-plant = ls_resb-werks.
        lw_gmitem-stge_loc = ls_resb-lgort.
        lw_gmitem-move_plant = ls_resb-umwrk.
        lw_gmitem-move_stloc = ls_resb-umlgo.
        lw_gmitem-reserv_no = lv_sapno.                  " 预留/相关需求的编号
        lw_gmitem-res_item = lv_rspos.                   " 预留 / 相关需求的项目编号
        "移动类型291 设置销售订单号及行项目 add by ljm 20191218---start---
        IF ls_item-bwart = '291' .
          lw_gmitem-sales_ord = ls_resb-kdauf."销售订单号
          lw_gmitem-s_ord_item = ls_resb-kdpos."销售订单行项目
        ENDIF.
        "移动类型291 设置销售订单号及行项目 add by ljm 20191218---end---
      ELSE.
        lw_gmitem-plant = us_header-werks.
        lw_gmitem-stge_loc = ls_item-lgort.
        lw_gmitem-move_plant = ls_item-dwerks.
        lw_gmitem-move_stloc = ls_item-dlgort.
      ENDIF.
    ELSE.
      lw_gmitem-plant = us_header-werks.
      lw_gmitem-stge_loc = ls_item-lgort.
      lw_gmitem-move_plant = ls_item-dwerks.
      lw_gmitem-move_stloc = ls_item-dlgort.
    ENDIF.
    IF lv_lifnr IS NOT INITIAL.
      lw_gmitem-vendor = lv_lifnr.                       " 供应商号
      lw_gmitem-spec_stock = 'K'.
    ENDIF.
    CLEAR: lv_kdauf, lv_kdpos, lv_aufnr.
    lv_kdauf = ls_item-kdauf.
    lv_kdpos = ls_item-kdpos.
    lv_aufnr = ls_item-aufnr.
    alpha_input: lv_kdauf.
    alpha_input: lv_kdpos.
    alpha_input: lv_aufnr.
    IF lv_kdauf IS NOT INITIAL AND ls_item-bwart = 'Y01'.
*      LW_GMITEM-SALES_ORD = LS_ITEM-KDAUF.               " 销售订单
*      LW_GMITEM-S_ORD_ITEM = LS_ITEM-KDPOS.              " 销售订单行号
      lw_gmitem-sales_ord = lv_kdauf.
      lw_gmitem-s_ord_item = lv_kdpos.
*      LW_GMITEM-SPEC_STOCK = 'E'.
    ENDIF.
    IF lv_kdauf IS NOT INITIAL AND ls_item-bwart = 'Z11'.
*      LW_GMITEM-SALES_ORD = LS_ITEM-KDAUF.               " 销售订单
*      LW_GMITEM-S_ORD_ITEM = LS_ITEM-KDPOS.              " 销售订单行号
      lw_gmitem-sales_ord = lv_kdauf.
      lw_gmitem-s_ord_item = lv_kdpos.
      lw_gmitem-spec_stock = 'E'.
    ENDIF.
    IF lv_kdauf IS NOT INITIAL AND ( ls_item-bwart = '343' OR ls_item-bwart = '344' ).
      lw_gmitem-sales_ord = lv_kdauf.
      lw_gmitem-s_ord_item = lv_kdpos.
      lw_gmitem-val_sales_ord = lv_kdauf.
      lw_gmitem-val_s_ord_item = lv_kdpos.

      lw_gmitem-spec_stock = 'E'.
    ENDIF.
    "HK 2020.04.24.   移动类型为325的时候，增加特殊库存标记
    IF lv_kdauf IS NOT INITIAL AND ls_item-bwart = '325'.
      lw_gmitem-spec_stock = 'E'.
    ENDIF.
    "修改结束位置
    IF ls_item-aufnr IS NOT INITIAL.
*      LW_GMITEM-ORDERID = LS_ITEM-AUFNR.               " 生产订单
      lw_gmitem-orderid = lv_aufnr.
      lw_gmitem-order_itno = '0001'.                   " 生产订单行号
      " Z61和Y61时为内部订单，其他为生产订单
      IF ls_item-bwart = 'Z61' OR ls_item-bwart = 'Y61'.
        lw_gmitem-mvt_ind   = ''.
      ELSE.
        lw_gmitem-mvt_ind   = 'F'.
      ENDIF.
*      LW_GMITEM-MVT_IND   = 'F'.
    ENDIF.
    APPEND lw_gmitem TO lt_gmitem.
    CLEAR: lw_gmitem.
  ENDLOOP.
  " 生成物料凭证
  CLEAR: lv_mblnr1, lt_return.
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = lw_gmhead
      goodsmvt_code    = lw_gmcode
    IMPORTING
      materialdocument = lv_mblnr1
    TABLES
      goodsmvt_item    = lt_gmitem
      return           = lt_return.
  CLEAR: lw_return, lv_mess.
  LOOP AT lt_return INTO lw_return WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X'.
    CONCATENATE lv_mess '|' lw_return-message INTO lv_mess.
    CLEAR: lw_return.
  ENDLOOP.
  IF lv_mess IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    cs_out-wms_custid = us_header-wms_custid.
    cs_out-wms_docno = us_header-wms_docno.
    cs_out-ret_code = 'E'.
    cs_out-ret_msg = lv_mess.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    IF lv_flag IS NOT INITIAL.
      CLEAR: lw_gmhead, lw_gmcode, lt_gmitem, lw_gmitem, lt_nopur, ls_nopur.

      lw_gmhead-pstng_date = sy-datum.
      lw_gmhead-doc_date   = sy-datum.
      lw_gmhead-pr_uname   = sy-uname.
      IF lv_sapno1 IS NOT INITIAL.
        lw_gmhead-header_txt = lv_sapno1.
      ELSE.
        lw_gmhead-header_txt = ''.
      ENDIF.
      lw_gmcode-gm_code = '04'.
      LOOP AT lt_item INTO ls_item.
        CLEAR: lv_rspos, lv_matnr, lv_dmatnr.
        lv_matnr = ls_item-matnr.
        matn1_input: lv_matnr.
        lv_lifnr = ls_item-lifnr.
        alpha_input: lv_lifnr.
*        LW_GMITEM-PLANT     = US_HEADER-WERKS.             "工厂
        lw_gmitem-material  = lv_matnr.
        lw_gmitem-batch  = ls_item-charg.
*        LW_GMITEM-STGE_LOC  = LS_ITEM-LGORT.               " 库存地点
        lw_gmitem-move_type = '411'.                       " 移动类型
        lw_gmitem-mvt_ind   = ' '.
        lw_gmitem-costcenter = ls_item-kostl.              " 成本中心
*        LW_GMITEM-MOVE_STLOC = LS_ITEM-LGORT.              " 库存地点-CHANGE
        lw_gmitem-move_mat = lv_matnr.                     " 物料-CHANGE
        lw_gmitem-move_batch = ls_item-charg.              " 批次-CHANGE
*        LW_GMITEM-MOVE_PLANT = US_HEADER-WERKS.            " 工厂-CHANGE
        lw_gmitem-entry_qnt = ls_item-menge.               " 数量
        CLEAR: lv_meins.
        cunit_input: ls_item-meins lv_meins.
        IF lv_meins IS NOT INITIAL.
          lw_gmitem-entry_uom = lv_meins.
        ELSE.
          lw_gmitem-entry_uom = ls_item-meins.
        ENDIF.
        lv_rspos = ls_item-sapnote_line.
        alpha_input: lv_rspos.
        alpha_input: lv_sapno.
        IF lv_sapno IS NOT INITIAL.
          CLEAR: ls_resb.
          SELECT SINGLE rsnum rspos werks lgort umwrk umlgo
              FROM resb INTO CORRESPONDING FIELDS OF ls_resb
              WHERE rsnum = lv_sapno AND rspos = lv_rspos.
          IF sy-subrc = 0.
            lw_gmitem-plant = ls_resb-werks.
            lw_gmitem-stge_loc = ls_resb-lgort.
            lw_gmitem-move_plant = ls_resb-umwrk.
            lw_gmitem-move_stloc = ls_resb-umlgo.
          ELSE.
            lw_gmitem-plant = us_header-werks.
            lw_gmitem-stge_loc = ls_item-lgort.
*            LW_GMITEM-MOVE_PLANT = LS_ITEM-DWERKS.
*            LW_GMITEM-MOVE_STLOC = LS_ITEM-DLGORT.
          ENDIF.
        ELSE.
          lw_gmitem-plant = us_header-werks.
          lw_gmitem-stge_loc = ls_item-lgort.
*          LW_GMITEM-MOVE_PLANT = LS_ITEM-DWERKS.
*          LW_GMITEM-MOVE_STLOC = LS_ITEM-DLGORT.
        ENDIF.
        lw_gmitem-spec_stock = 'E'.                        " 特殊库存标识
*        LW_GMITEM-STCK_TYPE = LS_ITEM-STOCK_ST.            " 库存类型
        IF ls_item-stock_st = 'B'.
          lw_gmitem-stck_type = '3'.
        ELSEIF ls_item-stock_st = 'I'.
          lw_gmitem-stck_type = '2'.
        ELSEIF ls_item-stock_st = ''.
          lw_gmitem-stck_type = ''.
        ENDIF.
        lw_gmitem-vendor = lv_lifnr.                       " 供应商号
        CLEAR: lv_kdauf, lv_kdpos.
        lv_kdauf = ls_item-kdauf.
        lv_kdpos = ls_item-kdpos.
        alpha_input: lv_kdauf.
        alpha_input: lv_kdpos.
        lw_gmitem-val_sales_ord = lv_kdauf.
        lw_gmitem-val_s_ord_item = lv_kdpos.
*        LW_GMITEM-VAL_SALES_ORD = LS_ITEM-KDAUF.           " 销售订单
*        LW_GMITEM-VAL_S_ORD_ITEM = LS_ITEM-KDPOS.          " 销售订单行号
        CLEAR: ls_t0003, lv_sapno, lv_line.
        lv_sapno = us_header-sapnote_no.
        lv_line = ls_item-sapnote_line.
        alpha_input: lv_sapno.
        alpha_input: lv_line.
        SELECT SINGLE * FROM zppt0003 INTO ls_t0003
          WHERE zsapnote_no = lv_sapno AND zsapnote_line = lv_line.
        IF sy-subrc = 0.
          lw_gmitem-gr_rcpt = ls_t0003-wempf.              " 收货方
          lw_gmitem-unload_pt = ls_t0003-ablad.            " 卸货点
        ENDIF.
        APPEND lw_gmitem TO lt_gmitem.
        CLEAR: lw_gmitem.
      ENDLOOP.
      " 生成物料凭证
      CLEAR: lv_mblnr2, lt_return.
      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = lw_gmhead
          goodsmvt_code    = lw_gmcode
        IMPORTING
          materialdocument = lv_mblnr2
        TABLES
          goodsmvt_item    = lt_gmitem
          return           = lt_return.
      CLEAR: lw_return, lv_mess.
      LOOP AT lt_return INTO lw_return WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X'.
        CONCATENATE lv_mess '|' lw_return-message INTO lv_mess.
        CLEAR: lw_return.
      ENDLOOP.
      IF lv_mess IS NOT INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        cs_out-wms_custid = us_header-wms_custid.
        cs_out-wms_docno = us_header-wms_docno.
        cs_out-ret_code = 'E'.
        cs_out-ret_msg = lv_mess.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        cs_out-wms_custid = us_header-wms_custid.
        cs_out-wms_docno = us_header-wms_docno.
        cs_out-sap_docno = lv_mblnr1 && lv_mblnr2.
        cs_out-ret_code = 'S'.
        cs_out-ret_msg = '成功'.
        CLEAR: lv_ebelp.
        LOOP AT lt_item INTO ls_item.
          IF ls_item-bwart = 'Y01'.
            CLEAR: lt_t0003, ls_t0003, lv_sapno2, lv_line.
            lv_sapno2 = us_header-sapnote_no.
            lv_line = ls_item-sapnote_line.
            alpha_input: lv_sapno2.
            alpha_input: lv_line.
            SELECT SINGLE * FROM zppt0003 INTO ls_t0003
              WHERE zsapnote_no = lv_sapno2 AND zsapnote_line = lv_line.
            IF sy-subrc = 0.
              ls_t0003-zwmsmng = ls_t0003-zwmsmng + ls_item-menge.
              APPEND ls_t0003 TO lt_t0003.
              MODIFY zppt0003 FROM TABLE lt_t0003.
            ENDIF.
          ENDIF.

          CLEAR: lv_rspos.
          lv_rspos = ls_item-sapnote_line.
          alpha_input: lv_rspos.
          ls_ret-wms_itemno = ls_item-wms_itemno.
          ls_ret-sap_itemno = lv_rspos.
          ls_ret-stock_sts = ls_item-stock_st.
          APPEND ls_ret TO lt_ret.
          CLEAR: ls_ret.

          ls_nopur-mandt = sy-mandt.
          ls_nopur-mblnr = lv_mblnr2.
          ls_nopur-zeile = lv_rspos.
          ls_nopur-mjahr = sy-datum+0(4).
          ls_nopur-wms_docno = us_header-wms_docno.
          ls_nopur-wms_itemno = ls_item-wms_itemno.
          ls_nopur-sapnote_no = us_header-sapnote_no.
          ls_nopur-sapnote_line = lv_rspos.
          ls_nopur-docdate = sy-datum.
          APPEND ls_nopur TO lt_nopur.
          CLEAR: ls_nopur.
        ENDLOOP.
        cs_out-list_of_others_post_ack = lt_ret.
        MODIFY zmmmigo_nopur FROM TABLE lt_nopur.
      ENDIF.
    ELSE.

      cs_out-wms_custid = us_header-wms_custid.
      cs_out-wms_docno = us_header-wms_docno.
      cs_out-sap_docno = lv_mblnr1.
      cs_out-ret_code = 'S'.
      cs_out-ret_msg = '成功'.
      CLEAR: lv_ebelp.
      LOOP AT lt_item INTO ls_item.
        IF ls_item-bwart = 'Y01'.
          CLEAR: lt_t0003, ls_t0003, lv_sapno2, lv_line.
          lv_sapno2 = us_header-sapnote_no.
          lv_line = ls_item-sapnote_line.
          alpha_input: lv_sapno2.
          alpha_input: lv_line.
          SELECT SINGLE * FROM zppt0003 INTO ls_t0003
            WHERE zsapnote_no = lv_sapno2 AND zsapnote_line = lv_line.
          IF sy-subrc = 0.
            ls_t0003-zwmsmng = ls_t0003-zwmsmng + ls_item-menge.
            APPEND ls_t0003 TO lt_t0003.
            MODIFY zppt0003 FROM TABLE lt_t0003.
          ENDIF.
        ENDIF.

        CLEAR: lv_rspos.
        lv_rspos = ls_item-sapnote_line.
        alpha_input: lv_rspos.
        ls_ret-wms_itemno = ls_item-wms_itemno.
        ls_ret-sap_itemno = lv_rspos.
        ls_ret-stock_sts = ls_item-stock_st.
        APPEND ls_ret TO lt_ret.
        CLEAR: ls_ret.

        ls_nopur-mandt = sy-mandt.
        ls_nopur-mblnr = lv_mblnr1.
        ls_nopur-zeile = lv_rspos.
        ls_nopur-mjahr = sy-datum+0(4).
        ls_nopur-wms_docno = us_header-wms_docno.
        ls_nopur-wms_itemno = ls_item-wms_itemno.
        ls_nopur-sapnote_no = us_header-sapnote_no.
        ls_nopur-sapnote_line = lv_rspos.
        ls_nopur-docdate = sy-datum.
        APPEND ls_nopur TO lt_nopur.
        CLEAR: ls_nopur.
      ENDLOOP.
      cs_out-list_of_others_post_ack = lt_ret.
      MODIFY zmmmigo_nopur FROM TABLE lt_nopur.
    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_BATCH_CHANGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_ITEM
*&      <-- LV_FLAG
*&      <-- CLEAR
*&      <-- LV_RSPOS
*&---------------------------------------------------------------------*
FORM frm_batch_change  USING    us_item   TYPE zdt_wms2sap_others_sub
                                us_header TYPE zdt_wms2sap_others
                       CHANGING cv_mess   TYPE char255.
  DEFINE alpha_input.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input         = &1
      IMPORTING
        output        = &1.
  END-OF-DEFINITION.
  DEFINE matn1_input.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input           = &1
      IMPORTING
        output          = &1
      EXCEPTIONS
        length_error    = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.
  END-OF-DEFINITION.
  DATA: lv_matnr           TYPE matnr18,
        lv_charg           TYPE charg_d,
        lv_werks           TYPE werks_d,
        lv_sapnote_no      TYPE zsapnote_no,
        lv_sapnote_line    TYPE zsapnote_line,
        ls_t0003           TYPE zppt0003,
        lw_batchattributes TYPE bapibatchatt,
        lw_batchstatus     TYPE bapibatchstatus,
        lw_batchattributex TYPE bapibatchattx,
        lw_batchattributei TYPE bapibatchatt,
        lt_return          TYPE STANDARD TABLE OF bapiret2,
        lw_return          TYPE bapiret2.
  CLEAR: lv_matnr, lv_charg, lv_werks.
  lv_matnr = us_item-matnr.
  lv_charg = us_item-charg.
  lv_werks = us_header-werks.
  matn1_input: lv_matnr.
  alpha_input: lv_charg.

  CLEAR: lt_return, lw_return, lw_batchattributes, lw_batchstatus.
  CALL FUNCTION 'BAPI_BATCH_GET_DETAIL'
    EXPORTING
      material        = lv_matnr
      batch           = lv_charg
      plant           = lv_werks
    IMPORTING
      batchattributes = lw_batchattributes
      batchstatus     = lw_batchstatus
    TABLES
      return          = lt_return.
  LOOP AT lt_return INTO lw_return WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X'.
*    CONCATENATE LV_MESS '|' LW_RETURN-MESSAGE INTO LV_MESS.
*    CLEAR: LW_RETURN.
  ENDLOOP.
  IF sy-subrc <> 0.
    CLEAR: ls_t0003, lv_sapnote_no, lv_sapnote_line.
    lv_sapnote_no = us_header-sapnote_no.
    lv_sapnote_line = us_item-sapnote_line.
    alpha_input: lv_sapnote_no.
    alpha_input: lv_sapnote_line.
    SELECT SINGLE * FROM zppt0003 INTO ls_t0003
      WHERE zsapnote_no = lv_sapnote_no AND zsapnote_line = lv_sapnote_line.
    IF sy-subrc = 0.
      lw_batchattributes-prod_date = ls_t0003-zhsdat.
      lw_batchattributex-prod_date = 'X'.
      CLEAR: lw_batchattributei, lt_return.
      CALL FUNCTION 'BAPI_BATCH_CHANGE'
        EXPORTING
          material         = lv_matnr
          batch            = lv_charg
          plant            = lv_werks
          batchattributes  = lw_batchattributes
          batchattributesx = lw_batchattributex
        IMPORTING
          batchattributes  = lw_batchattributei
        TABLES
          return           = lt_return.
      LOOP AT lt_return INTO lw_return WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X'.
        IF cv_mess IS INITIAL.
          cv_mess = lw_return-message.
        ELSE.
          CONCATENATE cv_mess '|' lw_return-message INTO cv_mess.
        ENDIF.
        CLEAR: lw_return.
      ENDLOOP.
    ELSE.
      IF cv_mess IS INITIAL.
        CONCATENATE lv_sapnote_no lv_sapnote_line '未找到生产日期' INTO cv_mess.
        cv_mess = '未找到生产日期'.
      ELSE.
        CONCATENATE cv_mess '|' lv_sapnote_no lv_sapnote_line '未找到生产日期' INTO cv_mess.
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR: ls_t0003, lv_sapnote_no, lv_sapnote_line, lw_batchattributes.
    lv_sapnote_no = us_header-sapnote_no.
    lv_sapnote_line = us_item-sapnote_line.
    alpha_input: lv_sapnote_no.
    alpha_input: lv_sapnote_line.
    SELECT SINGLE * FROM zppt0003 INTO ls_t0003
      WHERE zsapnote_no = lv_sapnote_no AND zsapnote_line = lv_sapnote_line.
    IF sy-subrc = 0.
      lw_batchattributes-prod_date = ls_t0003-zhsdat.
    ENDIF.
    CLEAR: lw_batchattributei, lt_return.
    CALL FUNCTION 'BAPI_BATCH_CREATE'
      EXPORTING
        material        = lv_matnr
        batch           = lv_charg
        plant           = lv_werks
        batchattributes = lw_batchattributes
      IMPORTING
        batch           = lv_charg
        batchattributes = lw_batchattributei
      TABLES
        return          = lt_return.
    LOOP AT lt_return INTO lw_return WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X'.
      IF cv_mess IS INITIAL.
        cv_mess = lw_return-message.
      ELSE.
        CONCATENATE cv_mess '|' lw_return-message INTO cv_mess.
      ENDIF.
      CLEAR: lw_return.
    ENDLOOP.
  ENDIF.

  WAIT UP TO 2 SECONDS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_MIGO_JS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_HEADER
*&      <-- LS_OUT
*&---------------------------------------------------------------------*
FORM frm_migo_js  USING    us_header TYPE zdt_wms2sap_others
                  CHANGING cs_out    TYPE zdt_wms2sap_others_ret.

  DEFINE alpha_input.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input         = &1
      IMPORTING
        output        = &1.
  END-OF-DEFINITION.
  DEFINE matn1_input.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input           = &1
      IMPORTING
        output          = &1
      EXCEPTIONS
        length_error    = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.
  END-OF-DEFINITION.
  DEFINE cunit_input.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input                = &1
        language             = sy-langu
      IMPORTING
        output               = &2
      EXCEPTIONS
        unit_not_found       = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  END-OF-DEFINITION.
  DATA: lv_noteno TYPE char10,
        lv_sapno  TYPE rsnum,
        lv_sapno1 TYPE char10,
        lv_sapno2 TYPE char10,
        lv_line   TYPE zsapnote_line,
        lv_rspos  TYPE rspos,
        lv_matnr  TYPE matnr,
        lv_dmatnr TYPE matnr,
        lv_lifnr  TYPE lifnr,
        lv_aufnr  TYPE aufnr,
        lv_flag   TYPE c,
        lv_meins  TYPE meins,
        lt_nopur  TYPE STANDARD TABLE OF zmmmigo_nopur,
        ls_nopur  TYPE zmmmigo_nopur,
        lt_ret    TYPE zdt_wms2sap_others_ret_lis_tab,
        ls_ret    TYPE zdt_wms2sap_others_ret_list_of,
        lt_item   TYPE zdt_wms2sap_others_sub_tab,
        ls_item   TYPE zdt_wms2sap_others_sub.
  DATA: lw_gmhead TYPE bapi2017_gm_head_01,
        lw_gmcode TYPE bapi2017_gm_code,
        lt_gmitem TYPE TABLE OF bapi2017_gm_item_create,
        lw_gmitem TYPE bapi2017_gm_item_create,
        lt_return TYPE TABLE OF bapiret2,
        lw_return TYPE bapiret2,
        lw_headrt TYPE bapi2017_gm_head_ret,
        ls_resb   TYPE resb,
        lv_ebelp  TYPE ekpo-ebelp,
        lv_mblnr1 TYPE bapi2017_gm_head_ret-mat_doc,
        lv_mblnr2 TYPE bapi2017_gm_head_ret-mat_doc,
        lv_mess   TYPE char255.

  CLEAR: lt_item, ls_item, lt_nopur, ls_nopur, lt_ret, ls_ret.
  lt_item = us_header-list_of_others_post.
  " 通过流水号取预留单
  IF us_header-sapnote_no IS NOT INITIAL.
    CLEAR: lv_noteno, lv_sapno.
    lv_noteno = us_header-sapnote_no.
    SELECT SINGLE rsnum FROM zmm026_record INTO lv_sapno
      WHERE zreserv_no = lv_noteno.
    IF sy-subrc <> 0.
      " 通过流水号取通知单
      CLEAR: lv_sapno1.
      SELECT SINGLE zsapnote_no FROM zmm_changeno INTO lv_sapno1
        WHERE zsapnote_no = lv_noteno.
    ENDIF.
  ENDIF.

  CLEAR: lw_gmhead, lw_gmcode, lt_gmitem, lw_gmitem, lt_nopur, ls_nopur, lv_flag.

  lw_gmhead-pstng_date = sy-datum.
  lw_gmhead-doc_date   = sy-datum.
  lw_gmhead-pr_uname   = sy-uname.
  IF lv_sapno1 IS NOT INITIAL.
    lw_gmhead-header_txt = lv_sapno1.
  ELSE.
    lw_gmhead-header_txt = ''.
  ENDIF.
  lw_gmcode-gm_code = '04'.

  SORT lt_item BY wms_itemno.
*  LOOP AT LT_ITEM INTO LS_ITEM WHERE BWART = 'Z11' AND LIFNR IS NOT INITIAL.
  LOOP AT lt_item INTO ls_item WHERE lifnr IS NOT INITIAL
    AND ( bwart = '201'OR bwart = 'Z61'OR bwart = 'Y61'OR bwart = 'Z11' ).
*    OR BWART = '343' OR BWART = '344' ).
    CLEAR: lv_matnr, lv_dmatnr, lv_lifnr.
    lv_matnr = ls_item-matnr.
    lv_dmatnr = ls_item-dmatnr.
    lv_lifnr = ls_item-lifnr.
    matn1_input: lv_matnr.
    matn1_input: lv_dmatnr.
    alpha_input: lv_lifnr.
    CLEAR: lv_meins.
    cunit_input: ls_item-meins lv_meins.
    lw_gmitem-plant     = us_header-werks.             "工厂
    lw_gmitem-material  = lv_matnr.
    lw_gmitem-batch  = ls_item-charg.
    lw_gmitem-stge_loc  = ls_item-lgort.               " 库存地点
    lw_gmitem-move_type = '411'.                       " 移动类型
    lw_gmitem-mvt_ind   = ' '.
    lw_gmitem-costcenter = ls_item-kostl.              " 成本中心
    lw_gmitem-entry_qnt = ls_item-menge.               " 数量
    IF lv_meins IS NOT INITIAL.
      lw_gmitem-entry_uom = lv_meins.
    ELSE.
      lw_gmitem-entry_uom = ls_item-meins.
    ENDIF.
    lw_gmitem-spec_stock = 'K'.                        " 特殊库存标识
    lw_gmitem-stck_type = ls_item-stock_st.            " 库存类型
    lw_gmitem-vendor = lv_lifnr.                       " 供应商号

    APPEND lw_gmitem TO lt_gmitem.
    CLEAR: lw_gmitem.
  ENDLOOP.
  " 生成物料凭证
  CLEAR: lv_mblnr1, lt_return.
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = lw_gmhead
      goodsmvt_code    = lw_gmcode
    IMPORTING
      materialdocument = lv_mblnr1
    TABLES
      goodsmvt_item    = lt_gmitem
      return           = lt_return.
  CLEAR: lw_return, lv_mess.
  LOOP AT lt_return INTO lw_return WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X'.
    CONCATENATE lv_mess '|' lw_return-message INTO lv_mess.
    CLEAR: lw_return.
  ENDLOOP.
  IF lv_mess IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    cs_out-wms_custid = us_header-wms_custid.
    cs_out-wms_docno = us_header-wms_docno.
    cs_out-ret_code = 'E'.
    cs_out-ret_msg = lv_mess.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    CLEAR: lt_gmitem, lw_gmitem.

    LOOP AT lt_item INTO ls_item.
      IF ls_item-bwart = '201'
        OR ls_item-bwart = 'Y61'
        OR ls_item-bwart = 'Z61'.
        lw_gmcode-gm_code = '03'.
      ENDIF.
      CLEAR: lv_rspos, lv_matnr, lv_dmatnr.
      lv_matnr = ls_item-matnr.
      lv_dmatnr = ls_item-dmatnr.
      matn1_input: lv_matnr.
      matn1_input: lv_dmatnr.
      lv_rspos = ls_item-sapnote_line.
      alpha_input: lv_rspos.
      alpha_input: lv_sapno.
      CLEAR: lv_meins.
      cunit_input: ls_item-meins lv_meins.
      lw_gmitem-move_type = ls_item-bwart.               " 移动类型
      lw_gmitem-spec_stock = ''.                         " 特殊库存标识
      lw_gmitem-material  = lv_matnr.
      lw_gmitem-batch  = ls_item-charg.
      lw_gmitem-costcenter = ls_item-kostl.              " 成本中心
*      LW_GMITEM-PLANT     = US_HEADER-WERKS.             "工厂
*      LW_GMITEM-STGE_LOC  = LS_ITEM-LGORT.               " 库存地点
*      LW_GMITEM-MOVE_STLOC = LS_ITEM-DLGORT.             " 库存地点-CHANGE
      lw_gmitem-mvt_ind   = ' '.
      lw_gmitem-entry_qnt = ls_item-menge.               " 数量
      lw_gmitem-entry_uom = lv_meins.
      lw_gmitem-stck_type = ls_item-stock_st.            " 库存类型
      IF lv_sapno IS NOT INITIAL.
        CLEAR: ls_resb.
        SELECT SINGLE rsnum rspos werks lgort umwrk umlgo
          FROM resb INTO CORRESPONDING FIELDS OF ls_resb
          WHERE rsnum = lv_sapno AND rspos = lv_rspos.
        IF sy-subrc = 0.
          lw_gmitem-plant = ls_resb-werks.
          lw_gmitem-stge_loc = ls_resb-lgort.
          lw_gmitem-move_plant = ls_resb-umwrk.
          lw_gmitem-move_stloc = ls_resb-umlgo.
          lw_gmitem-reserv_no = lv_sapno.                  " 预留/相关需求的编号
          lw_gmitem-res_item = lv_rspos.                   " 预留 / 相关需求的项目编号
        ELSE.
          lw_gmitem-plant = us_header-werks.
          lw_gmitem-stge_loc = ls_item-lgort.
          lw_gmitem-move_plant = ls_item-dwerks.
          lw_gmitem-move_stloc = ls_item-dlgort.
        ENDIF.
      ELSE.
        lw_gmitem-plant = us_header-werks.
        lw_gmitem-stge_loc = ls_item-lgort.
        lw_gmitem-move_plant = ls_item-dwerks.
        lw_gmitem-move_stloc = ls_item-dlgort.
      ENDIF.
      IF ls_item-aufnr IS NOT INITIAL.
        CLEAR: lv_aufnr.
        lv_aufnr = ls_item-aufnr.
        alpha_input: lv_aufnr.
*        LW_GMITEM-ORDERID = LS_ITEM-AUFNR.               " 生产订单
        lw_gmitem-orderid = lv_aufnr.
        lw_gmitem-order_itno = '0001'.                   " 生产订单行号
      ENDIF.

      APPEND lw_gmitem TO lt_gmitem.
      CLEAR: lw_gmitem.
    ENDLOOP.
    " 生成物料凭证
    CLEAR: lv_mblnr2, lt_return.
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = lw_gmhead
        goodsmvt_code    = lw_gmcode
      IMPORTING
        materialdocument = lv_mblnr2
      TABLES
        goodsmvt_item    = lt_gmitem
        return           = lt_return.
    CLEAR: lw_return, lv_mess.
    LOOP AT lt_return INTO lw_return WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X'.
      CONCATENATE lv_mess '|' lw_return-message INTO lv_mess.
      CLEAR: lw_return.
    ENDLOOP.
    IF lv_mess IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      cs_out-wms_custid = us_header-wms_custid.
      cs_out-wms_docno = us_header-wms_docno.
      cs_out-ret_code = 'E'.
      cs_out-ret_msg = lv_mess.
      " 取消411 K 物料凭证
      CLEAR: lw_headrt, lt_return, lv_mess.
      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
        EXPORTING
          materialdocument = lv_mblnr1
          matdocumentyear  = sy-datum+0(4)
        IMPORTING
          goodsmvt_headret = lw_headrt
        TABLES
          return           = lt_return.
      LOOP AT lt_return INTO lw_return WHERE type EQ 'E' OR type EQ 'A' OR type EQ 'X'.
        CONCATENATE lv_mess '|' lw_return-message INTO lv_mess.
        CLEAR: lw_return.
      ENDLOOP.
      IF lv_mess IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        cs_out-wms_custid = us_header-wms_custid.
        cs_out-wms_docno = us_header-wms_docno.
        cs_out-sap_docno = lv_mblnr1.
        cs_out-ret_code = 'E'.
        CONCATENATE '物料凭证' lv_mblnr1 '冲销失败，请到SAP手动冲销' INTO cs_out-ret_msg.
      ENDIF.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      cs_out-wms_custid = us_header-wms_custid.
      cs_out-wms_docno = us_header-wms_docno.
      CONCATENATE lv_mblnr1 '|' lv_mblnr2 INTO cs_out-sap_docno.
*      CS_OUT-SAP_DOCNO = LV_MBLNR1 && LV_MBLNR2.
      cs_out-ret_code = 'S'.
      cs_out-ret_msg = '成功'.
      CLEAR: lv_ebelp.
      LOOP AT lt_item INTO ls_item.
        CLEAR: lv_rspos.
        lv_rspos = ls_item-sapnote_line.
        alpha_input: lv_rspos.
        ls_ret-wms_itemno = ls_item-wms_itemno.
        ls_ret-sap_itemno = lv_rspos.
        ls_ret-stock_sts = ls_item-stock_st.
        APPEND ls_ret TO lt_ret.
        CLEAR: ls_ret.

        ls_nopur-mandt = sy-mandt.
        ls_nopur-mblnr = lv_mblnr2.
        ls_nopur-zeile = lv_rspos.
        ls_nopur-mjahr = sy-datum+0(4).
        ls_nopur-wms_docno = us_header-wms_docno.
        ls_nopur-wms_itemno = ls_item-wms_itemno.
        ls_nopur-sapnote_no = us_header-sapnote_no.
        ls_nopur-sapnote_line = lv_rspos.
        ls_nopur-docdate = sy-datum.
        APPEND ls_nopur TO lt_nopur.
        CLEAR: ls_nopur.
      ENDLOOP.
      cs_out-list_of_others_post_ack = lt_ret.
      MODIFY zmmmigo_nopur FROM TABLE lt_nopur.
    ENDIF.
  ENDIF.
ENDFORM.