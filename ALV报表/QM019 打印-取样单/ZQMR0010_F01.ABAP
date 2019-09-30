*&---------------------------------------------------------------------*
*& 包含               ZQMROOO9_F01
*&---------------------------------------------------------------------*
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE lvc_t_fcat WITH HEADER LINE,
     layout   TYPE lvc_s_layo,
     w_repid  TYPE sy-repid. "记录系统当前的程序名
FORM frm_download.
  DATA: p_app_name   TYPE char200,
        lv_sheetname TYPE char50,
        lv_inplace   TYPE c VALUE 'X'.
  CONSTANTS: gco_callback_status       TYPE slis_formname  VALUE 'FRM_USER_STATUS'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = '正在输出,请稍等。。。'
    EXCEPTIONS
      OTHERS = 1.

  CHECK gv_initialized IS INITIAL.

* first get the SAP DOI i_oi_container_control interface
  CALL METHOD c_oi_container_control_creator=>get_container_control
    IMPORTING
      control = grf_control
      error   = gs_error.

* check no errors occured
  CALL METHOD gs_error->raise_message
    EXPORTING
      type = 'E'.
  p_app_name = sy-repid.
  CALL METHOD grf_control->init_control
    EXPORTING
      r3_application_name      = p_app_name
      inplace_enabled          = lv_inplace
      inplace_scroll_documents = 'X'
*     parent                   = GRF_container
      parent                   = cl_gui_container=>screen0
    IMPORTING
      error                    = gt_error.
  APPEND gt_error.
* ask the SAP DOI GRF_container for a i_oi_document_proxy for Excel
  CALL METHOD grf_control->get_document_proxy
    EXPORTING
      document_type  = 'Excel.Sheet'
      no_flush       = ''
    IMPORTING
      document_proxy = grf_document
      error          = gt_error.
  APPEND gt_error.
*   Open an existing docu from BDS
  PERFORM frm_get_document_url.
* open a document saved in business document service.
  CALL METHOD grf_document->open_document
    EXPORTING
      open_inplace = lv_inplace
      document_url = gv_item_url.

  DATA: has TYPE i.
  CALL METHOD grf_document->has_spreadsheet_interface
    EXPORTING
      no_flush     = ''
    IMPORTING
      is_available = has
      error        = gt_error.
  APPEND gt_error.

  CALL METHOD grf_document->get_spreadsheet_interface
    EXPORTING
      no_flush        = ''
    IMPORTING
      sheet_interface = grf_spreadsheet
      error           = gt_error.
  APPEND gt_error.

  LOOP AT gt_error.
    CALL METHOD gt_error->raise_message
      EXPORTING
        type = 'E'.
  ENDLOOP.
  FREE gt_error.
  lv_sheetname = '数据源'.

  CALL METHOD grf_spreadsheet->select_sheet
    EXPORTING
      name  = lv_sheetname
    IMPORTING
      error = gt_error.
  APPEND gt_error.

  LOOP AT gt_error.
    CALL METHOD gt_error->raise_message
      EXPORTING
        type = 'E'.
  ENDLOOP.

  FREE gt_error.
  gv_initialized = 'X'.
  PERFORM frm_output_to_excel_sheet.
*  PERFORM frm_sub_set_sheet. "自动调整宽度
ENDFORM.

FORM frm_get_document_url .
  DATA: bds_instance TYPE REF TO cl_bds_document_set.
  DATA: doc_signature    TYPE sbdst_signature,
        wa_doc_signature LIKE LINE OF doc_signature,
        doc_components   TYPE sbdst_components,
        doc_uris         TYPE sbdst_uri,
        wa_doc_uris      LIKE LINE OF doc_uris.
* Document template profile(Maintain T-Code:OAOR)
  CONSTANTS:
    doc_classname  TYPE sbdst_classname  VALUE 'HRFPM_EXCEL_STANDARD',
    doc_classtype  TYPE sbdst_classtype  VALUE 'OT',
    doc_object_key TYPE sbdst_object_key VALUE 'ZQMR0010'.
  wa_doc_signature-prop_value = 'ZQMR0010'.
  wa_doc_signature-prop_name = 'BDS_KEYWORD'.
  APPEND wa_doc_signature TO doc_signature.
  CREATE OBJECT bds_instance.
  CALL METHOD bds_instance->get_with_url
    EXPORTING
      classname  = doc_classname
      classtype  = doc_classtype
      object_key = doc_object_key
    CHANGING
      uris       = doc_uris
      signature  = doc_signature.
  FREE bds_instance.
  CLEAR gv_item_url.
  READ TABLE doc_uris INTO wa_doc_uris INDEX 1.
  gv_item_url = wa_doc_uris-uri.
ENDFORM.

FORM frm_output_to_excel_sheet.
  DATA :lv_i      TYPE i, "行号
        lv_strlen TYPE i. "字符串长度


  CLEAR gs_out.
*  gt_out2 = gt_out.
  LOOP AT gt_out INTO gs_out WHERE sel = 'X'.
    APPEND gs_out TO gt_out2.
  ENDLOOP.
  CLEAR gs_out.
  READ TABLE gt_out2 INTO gs_out INDEX 1.
  IF gs_out IS NOT INITIAL.
    " 编号
    PERFORM frm_fill_cell USING 3 1 lv_out-number.
    PERFORM frm_fill_cell USING 4 2 gs_out-matnr."物料代码
    PERFORM frm_fill_cell USING 5 2 gs_out-maktx."品名
    PERFORM frm_fill_cell USING 6 2 gs_out-charg."批次号
    PERFORM frm_fill_cell USING 7 2 gs_out-zpack_sta."规格
    PERFORM frm_fill_cell USING 8 2 gs_out-lmengeist."批量
    PERFORM frm_fill_cell USING 9 2 gs_out-zchanx."取样区域
    PERFORM frm_fill_cell USING 10 2 gs_out-lifnr."供应商
    PERFORM frm_fill_cell USING 11 2 gs_out-mfrnr."制造商
    PERFORM frm_fill_cell USING 12 2 gs_out-num."抽样规则（开箱数）
    PERFORM frm_fill_cell USING 13 2 gs_out-station."检验项/岗位
    " 取样/留样类型
    PERFORM frm_fill_cell USING 15 2 gs_out-gesstichpr."检验量
    PERFORM frm_fill_cell USING 16 2 gs_out-menge2."每批日常留样
    PERFORM frm_fill_cell USING 17 2 gs_out-verwmerkm."标样
    PERFORM frm_fill_cell USING 18 2 gs_out-zcqwdxly."长期稳定性考察
    PERFORM frm_fill_cell USING 19 2 gs_out-zjswdxly."加速稳定性考察
    PERFORM frm_fill_cell USING 20 2 gs_out-zdqwdxly."短期稳定性考察
    PERFORM frm_fill_cell USING 21 2 gs_out-zsfxbz."留样观察
    PERFORM frm_fill_cell USING 22 2 gs_out-zndwjly."年度外检
  ENDIF.
  CLEAR gs_out.
  READ TABLE gt_out2 INTO gs_out INDEX 2.
  IF gs_out IS NOT INITIAL.
    " 编号
    PERFORM frm_fill_cell USING 3 4 lv_out-number.
    PERFORM frm_fill_cell USING 4 5 gs_out-matnr."物料代码
    PERFORM frm_fill_cell USING 5 5 gs_out-maktx."品名
    PERFORM frm_fill_cell USING 6 5 gs_out-charg."批次号
    PERFORM frm_fill_cell USING 7 5 gs_out-zpack_sta."规格
    PERFORM frm_fill_cell USING 8 5 gs_out-lmengeist."批量
    PERFORM frm_fill_cell USING 9 5 gs_out-zchanx."取样区域
    PERFORM frm_fill_cell USING 10 5 gs_out-lifnr."供应商
    PERFORM frm_fill_cell USING 11 5 gs_out-mfrnr."制造商
    PERFORM frm_fill_cell USING 12 5 gs_out-num."抽样规则（开箱数）
    PERFORM frm_fill_cell USING 13 5 gs_out-station."检验项/岗位
    " 取样/留样类型
    PERFORM frm_fill_cell USING 15 5 gs_out-gesstichpr."检验量
    PERFORM frm_fill_cell USING 16 5 gs_out-menge2."每批日常留样
    PERFORM frm_fill_cell USING 17 5 gs_out-verwmerkm."标样
    PERFORM frm_fill_cell USING 18 5 gs_out-zcqwdxly."长期稳定性考察
    PERFORM frm_fill_cell USING 19 5 gs_out-zjswdxly."加速稳定性考察
    PERFORM frm_fill_cell USING 20 5 gs_out-zdqwdxly."短期稳定性考察
    PERFORM frm_fill_cell USING 21 5 gs_out-zsfxbz."留样观察
    PERFORM frm_fill_cell USING 22 5 gs_out-zndwjly."年度外检
  ENDIF.
  CLEAR gs_out.
  READ TABLE gt_out2 INTO gs_out INDEX 3.
  IF gs_out IS NOT INITIAL.
    " 编号
    PERFORM frm_fill_cell USING 25 1 lv_out-number.
    PERFORM frm_fill_cell USING 26 2 gs_out-matnr."物料代码
    PERFORM frm_fill_cell USING 27 2 gs_out-maktx."品名
    PERFORM frm_fill_cell USING 28 2 gs_out-charg."批次号
    PERFORM frm_fill_cell USING 29 2 gs_out-zpack_sta."规格
    PERFORM frm_fill_cell USING 30 2 gs_out-lmengeist."批量
    PERFORM frm_fill_cell USING 31 2 gs_out-zchanx."取样区域
    PERFORM frm_fill_cell USING 32 2 gs_out-lifnr."供应商
    PERFORM frm_fill_cell USING 33 2 gs_out-mfrnr."制造商
    PERFORM frm_fill_cell USING 34 2 gs_out-num."抽样规则（开箱数）
    PERFORM frm_fill_cell USING 35 2 gs_out-station."检验项/岗位
    " 取样/留样类型
    PERFORM frm_fill_cell USING 37 2 gs_out-gesstichpr."检验量
    PERFORM frm_fill_cell USING 38 2 gs_out-menge2."每批日常留样
    PERFORM frm_fill_cell USING 39 2 gs_out-verwmerkm."标样
    PERFORM frm_fill_cell USING 40 2 gs_out-zcqwdxly."长期稳定性考察
    PERFORM frm_fill_cell USING 41 2 gs_out-zjswdxly."加速稳定性考察
    PERFORM frm_fill_cell USING 42 2 gs_out-zdqwdxly."短期稳定性考察
    PERFORM frm_fill_cell USING 43 2 gs_out-zsfxbz."留样观察
    PERFORM frm_fill_cell USING 44 2 gs_out-zndwjly."年度外检
  ENDIF.
  CLEAR gs_out.
  READ TABLE gt_out2 INTO gs_out INDEX 4.
  IF gs_out IS NOT INITIAL.
    " 编号
    PERFORM frm_fill_cell USING 25 4 lv_out-number.
    PERFORM frm_fill_cell USING 26 5 gs_out-matnr."物料代码
    PERFORM frm_fill_cell USING 27 5 gs_out-maktx."品名
    PERFORM frm_fill_cell USING 28 5 gs_out-charg."批次号
    PERFORM frm_fill_cell USING 29 5 gs_out-zpack_sta."规格
    PERFORM frm_fill_cell USING 30 5 gs_out-lmengeist."批量
    PERFORM frm_fill_cell USING 31 5 gs_out-zchanx."取样区域
    PERFORM frm_fill_cell USING 32 5 gs_out-lifnr."供应商
    PERFORM frm_fill_cell USING 33 5 gs_out-mfrnr."制造商
    PERFORM frm_fill_cell USING 34 5 gs_out-num."抽样规则（开箱数）
    PERFORM frm_fill_cell USING 35 5 gs_out-station."检验项/岗位
    " 取样/留样类型
    PERFORM frm_fill_cell USING 37 5 gs_out-gesstichpr."检验量
    PERFORM frm_fill_cell USING 38 5 gs_out-menge2."每批日常留样
    PERFORM frm_fill_cell USING 39 5 gs_out-verwmerkm."标样
    PERFORM frm_fill_cell USING 40 5 gs_out-zcqwdxly."长期稳定性考察
    PERFORM frm_fill_cell USING 41 5 gs_out-zjswdxly."加速稳定性考察
    PERFORM frm_fill_cell USING 42 5 gs_out-zdqwdxly."短期稳定性考察
    PERFORM frm_fill_cell USING 43 5 gs_out-zsfxbz."留样观察
    PERFORM frm_fill_cell USING 44 5 gs_out-zndwjly."年度外检
  ENDIF.
  "检验批信息
*  PERFORM frm_fill_cell USING 2 2 gs_out-prueflos."检验批号
*  PERFORM frm_fill_cell USING 3 2 gs_out-charg."批次号
*  PERFORM frm_fill_cell USING 4 2 gs_out-lmengeist."实际批量
*  PERFORM frm_fill_cell USING 5 2 gs_out-mengeneinh."批量单位
*  PERFORM frm_fill_cell USING 6 2 gs_out-vbewertung."使用决策
*  PERFORM frm_fill_cell USING 7 2 gs_out-matnr."物料代码
*  PERFORM frm_fill_cell USING 8 2 gs_out-sellifnr."供应商代码
*  PERFORM frm_fill_cell USING 9 2 gs_out-name1."供应商名称



ENDFORM.
FORM frm_fill_cell USING i j val.

  DATA: columns_number TYPE i,
        rows_number    TYPE i,
        c(100)         TYPE c.

  REFRESH:gt_range, gt_excel_input.
  columns_number = 1.
  rows_number = 1.
  CALL METHOD grf_spreadsheet->insert_range_dim
    EXPORTING
      name     = 'CELL'
      no_flush = ''
      top      = i
      left     = j
      rows     = rows_number
      columns  = columns_number
    IMPORTING
      error    = gt_error.

  LOOP AT gt_error.
    CALL METHOD gt_error->raise_message
      EXPORTING
        type = 'E'.
  ENDLOOP.
  FREE gt_error.
  gs_range-name = 'CELL'.
  gs_range-columns = 1.
  gs_range-rows = 1.
  APPEND gs_range TO gt_range.
  gs_excel_input-column = 1.
  gs_excel_input-row = 1.
  "负数处理
  c = val.
  gs_excel_input-value = c.

  CONDENSE  gs_excel_input-value NO-GAPS.
  APPEND gs_excel_input TO gt_excel_input.
  CALL METHOD grf_spreadsheet->set_ranges_data
    EXPORTING
      ranges   = gt_range
      contents = gt_excel_input
      no_flush = ''
    IMPORTING
      error    = gt_error
      retcode  = gv_retcode.
  APPEND gt_error.
  LOOP AT gt_error.
    CALL METHOD gt_error->raise_message
      EXPORTING
        type = 'E'.
  ENDLOOP.
  FREE gt_error.
  REFRESH: gt_range, gt_excel_input.
ENDFORM.

FORM frm_get_data.
  DATA: lt_output TYPE TABLE OF zmaterial_tx_value,
        ls_output TYPE zmaterial_tx_value.
  DATA:lt_qals     TYPE TABLE OF qals,
       ls_qals     TYPE qals,
       lt_makt     TYPE TABLE OF makt,
       ls_makt     TYPE makt,
       lt_zppt0001 TYPE TABLE OF zppt0001,
       ls_zppt0001 TYPE zppt0001,
       lt_mara     TYPE TABLE OF mara,
       ls_mara     TYPE mara,
       lt_qpmk     TYPE TABLE OF qpmk,
       ls_qpmk     TYPE qpmk,
       lt_vbap     TYPE TABLE OF vbap,
       ls_vbap     TYPE vbap,
       lt_qprs     TYPE TABLE OF qprs,
       ls_qprs     TYPE qprs,
       lt_ausp     TYPE TABLE OF ausp,
       ls_ausp     TYPE ausp,
       ls_but000   TYPE but000,
       ls_afpo     TYPE afpo,
       ls_qamv     TYPE qamv,
       lt_qamv     TYPE TABLE OF qamv,
       lt_tq11t    TYPE TABLE OF tq11t,
       ls_tq11t    TYPE tq11t.
  TYPES:BEGIN OF lv_out,
          pqtxt TYPE tq11t-pqtxt,
        END OF lv_out.
  DATA:lt_out TYPE TABLE OF lv_out,
       ls_out TYPE lv_out.
  DATA:e_objek TYPE  cuobn,
       e_obtab TYPE  tabelle,
       e_klart TYPE  klassenart,
       e_class TYPE  klasse_d.

  DATA: ls_key             TYPE bapi1003_key,
        lv_status          LIKE bapi1003_key-status,
        lt_allocvaluesnum  LIKE TABLE OF bapi1003_alloc_values_num,
        ls_allocvaluesnum  LIKE bapi1003_alloc_values_num,
        lt_allocvalueschar LIKE TABLE OF bapi1003_alloc_values_char,
        ls_allocvalueschar LIKE bapi1003_alloc_values_char,
        lt_allocvaluescurr LIKE TABLE OF bapi1003_alloc_values_curr,
        ls_allocvaluescurr LIKE bapi1003_alloc_values_curr,
        lt_return          LIKE TABLE OF bapiret2.
  SELECT
    *
  INTO CORRESPONDING FIELDS OF TABLE lt_qals
  FROM qals
  WHERE qals~werk IN p_werks
  AND qals~matnr IN p_matnr
  AND qals~charg IN p_charg
  AND qals~prueflos IN p_pruef.
  "编号
  SELECT SINGLE
    ztxbh
  INTO  lv_out-number
  FROM zqmt007
  WHERE zqmt007~werks IN p_werks.
  lv_out-number = '编号:' && lv_out-number.
  IF lt_qals IS NOT INITIAL.
    LOOP AT lt_qals INTO ls_qals.

      gs_out-matnr = ls_qals-matnr."物料代码
      gs_out-matnr = zcl_bc_public=>conv_by_ddic( i_input = gs_out-matnr i_out = 'X' ).
      gs_out-charg = ls_qals-charg." 批次号
      gs_out-lmengeist = ls_qals-lmengeist && ls_qals-mengeneinh." 批量
      gs_out-gesstichpr = ls_qals-gesstichpr && ls_qals-einhprobe." 检验量
      "长期稳定性考察
      IF ls_qals-zcqwdxly IS INITIAL.
        gs_out-zcqwdxly = '无'.
      ELSE.
        gs_out-zcqwdxly = ls_qals-zcqwdxly.
      ENDIF.
      " 加速稳定性考察
      IF ls_qals-zjswdxly IS INITIAL.
        gs_out-zjswdxly = '无'.
      ELSE.
        gs_out-zjswdxly = ls_qals-zjswdxly.
      ENDIF.
      " 短期稳定性考察
      IF ls_qals-zdqwdxly IS INITIAL.
        gs_out-zdqwdxly = '无'.
      ELSE.
        gs_out-zdqwdxly = ls_qals-zdqwdxly.
      ENDIF.
      " 年度外检
      IF ls_qals-zndwjly IS INITIAL.
        gs_out-zndwjly = '无'.
      ELSE.
        gs_out-zndwjly = ls_qals-zndwjly.
      ENDIF.
      " 品名
      SELECT SINGLE
        maktx
      INTO CORRESPONDING FIELDS OF ls_makt
      FROM makt
      WHERE makt~matnr = ls_qals-matnr
      AND makt~spras = '1'.
      gs_out-maktx = ls_makt-maktx.

      " 规格
      SELECT SINGLE
        mtart
        groes
        mfrnr
      INTO CORRESPONDING FIELDS OF ls_mara
      FROM mara
      WHERE mara~matnr = ls_qals-matnr.
      "判断对应的规格类型
      IF ls_mara-mtart = 'HALB' OR ls_mara-mtart = 'Z030'."半成品
        "半成品的取样区域
        SELECT SINGLE
          *
        INTO CORRESPONDING FIELDS OF ls_zppt0001
        FROM zppt0001
        WHERE zppt0001~matnr = ls_qals-matnr
        AND zppt0001~charg = ls_qals-charg.
        gs_out-zchanx = ls_zppt0001-zchanx.
        " 规格
        gs_out-zpack_sta = ls_mara-groes.
        "留样观察
        SELECT SINGLE
        *
        INTO CORRESPONDING FIELDS OF ls_zppt0001
        FROM zppt0001
        WHERE zppt0001~aufnr = ls_qals-aufnr.

        SELECT SINGLE
        zsfxbz
        INTO ls_vbap-zsfxbz
        FROM vbap
        WHERE vbap~vbeln = ls_zppt0001-kdauf
        AND vbap~posnr = ls_zppt0001-kdpos.
        IF ls_vbap-zsfxbz = 'ABC'.
          gs_out-zsfxbz = '7个包装单位'.
        ENDIF.
*        gs_out-zsfxbz = ls_vbap-zsfxbz.
      ELSEIF ls_mara-mtart = 'FERT' OR ls_mara-mtart = 'Z050'."成品.

*        SELECT SINGLE
*          atwrt
*        INTO ls_ausp-atwrt
*        FROM ausp
*        WHERE ausp~objek = ls_qals-matnr
*          AND ausp~atinn = 'ZPSPE'.
*        gs_out-zpack_sta = ls_ausp-atwrt.
        IF ls_qals-charg IS NOT INITIAL AND ls_qals-matnr IS NOT INITIAL AND ls_qals-werk IS NOT INITIAL.
          ls_qals-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_qals-matnr ).
          CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
          EXPORTING
            i_matnr = ls_qals-matnr
            i_charg = ls_qals-charg
            i_werks = ls_qals-werk
          IMPORTING
            e_objek = e_objek
            e_obtab = e_obtab
            e_klart = e_klart
            e_class = e_class.

*  ls_key-object = e_objek.
          ls_key-objecttable = e_obtab.
          ls_key-classnum = e_class.
          ls_key-classtype = e_klart.
          ls_key-object_long = e_objek.
          SELECT SINGLE
            MATKL
          INTO LS_KEY-classnum
          FROM MARA
          WHERE MARA~MATNR = ls_qals-matnr.

          CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
          EXPORTING
*             objectkey       = ls_key-object
            objecttable     = ls_key-objecttable
            classnum        = ls_key-classnum
            classtype       = '001'
            objectkey_long  = ls_key-object_long
          IMPORTING
            status          = lv_status
*             STANDARDCLASS   =
          TABLES
            allocvaluesnum  = lt_allocvaluesnum
            allocvalueschar = lt_allocvalueschar
            allocvaluescurr = lt_allocvaluescurr
            RETURN          = lt_return.
*          CHECK lv_status IS NOT INITIAL.
          READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZPSPE'.
          gs_out-zpack_sta = ls_allocvalueschar-value_char_long.
        ENDIF.




        "留样观察（成品）

        SELECT SINGLE
          *
        INTO CORRESPONDING FIELDS OF ls_afpo
        FROM afpo
        WHERE afpo~aufnr = ls_qals-aufnr.

        SELECT SINGLE
          zsfxbz
        INTO ls_vbap-zsfxbz
        FROM vbap
        WHERE vbap~vbeln = ls_afpo-kdauf
        AND vbap~posnr = ls_afpo-kdpos.
        IF ls_vbap-zsfxbz = 'ABC'.
          gs_out-zsfxbz = '7个包装单位'.
        ENDIF.

        CLEAR ls_zppt0001.
        "成品的取样区域
        SELECT SINGLE
          *
        INTO CORRESPONDING FIELDS OF ls_zppt0001
        FROM zppt0001
        WHERE zppt0001~kdauf = ls_afpo-kdauf
        AND zppt0001~kdpos = ls_afpo-kdpos.
        gs_out-zchanx = ls_zppt0001-zchanx.
      ELSE." 原辅料包材
*        SELECT SINGLE
*        atwrt
*        INTO ls_ausp-atwrt
*        FROM ausp
*        WHERE ausp~objek = ls_qals-matnr
*        AND ausp~atinn = 'ZGROES'.
*        gs_out-zpack_sta = ls_ausp-atwrt.


*        CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
*          EXPORTING
*            matnr  = ls_qals-matnr
*            atnam  = 'ZGROES'
*          TABLES
*            output = lt_output.
*        READ TABLE lt_output INTO ls_output INDEX 1.
*        gs_out-zpack_sta = ls_output-atwrt.
*
*        gs_out-zsfxbz = '无'.
*        gs_out-zchanx = '无'.
        IF ls_qals-charg IS NOT INITIAL AND ls_qals-matnr IS NOT INITIAL AND ls_qals-werk IS NOT INITIAL.
          ls_qals-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_qals-matnr ).
          CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
            EXPORTING
              i_matnr = ls_qals-matnr
              i_charg = ls_qals-charg
              i_werks = ls_qals-werk
            IMPORTING
              e_objek = e_objek
              e_obtab = e_obtab
              e_klart = e_klart
              e_class = e_class.

*  ls_key-object = e_objek.
          ls_key-objecttable = e_obtab.
          ls_key-classnum = e_class.
          ls_key-classtype = e_klart.
          ls_key-object_long = e_objek.

          CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
            EXPORTING
*             objectkey       = ls_key-object
              objecttable     = ls_key-objecttable
              classnum        = ls_key-classnum
              classtype       = ls_key-classtype
              objectkey_long  = ls_key-object_long
            IMPORTING
              status          = lv_status
*             STANDARDCLASS   =
            TABLES
              allocvaluesnum  = lt_allocvaluesnum
              allocvalueschar = lt_allocvalueschar
              allocvaluescurr = lt_allocvaluescurr
              return          = lt_return.
*          CHECK lv_status IS NOT INITIAL.
          READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZPACK_STA'.
          gs_out-zpack_sta = ls_allocvalueschar-value_char_long.
        ENDIF.





      ENDIF.
      IF gs_out-zchanx IS INITIAL.
        gs_out-zchanx = '无'.
      ENDIF.
      " 抽样规则
*      SELECT SINGLE
*      atwrt
*      INTO ls_ausp-atwrt
*      FROM ausp
*      WHERE ausp~objek = ls_qals-matnr
*      AND ausp~atinn = 'ZBOX_NUM'.
*      gs_out-num = ls_ausp-atwrt.
*      IF gs_out-num IS INITIAL.
*        gs_out-num = '无'.
*      ENDIF.


*      CLEAR LS_OUTPUT.
*      CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
*        EXPORTING
*          matnr  = ls_qals-matnr
*          atnam  = 'ZBOX_NUM'
*        TABLES
*          output = lt_output.
*      READ TABLE lt_output INTO ls_output INDEX 1.
*      gs_out-num = ls_output-atwrt.

      " 抽样规则
      "获取批次信息
      IF ls_qals-charg IS NOT INITIAL AND ls_qals-matnr IS NOT INITIAL AND ls_qals-werk IS NOT INITIAL.
        ls_qals-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_qals-matnr ).
        CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
          EXPORTING
            i_matnr = ls_qals-matnr
            i_charg = ls_qals-charg
            i_werks = ls_qals-werk
          IMPORTING
            e_objek = e_objek
            e_obtab = e_obtab
            e_klart = e_klart
            e_class = e_class.

*  ls_key-object = e_objek.
        ls_key-objecttable = e_obtab.
        ls_key-classnum = e_class.
        ls_key-classtype = e_klart.
        ls_key-object_long = e_objek.

        CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
          EXPORTING
*           objectkey       = ls_key-object
            objecttable     = ls_key-objecttable
            classnum        = ls_key-classnum
            classtype       = ls_key-classtype
            objectkey_long  = ls_key-object_long
          IMPORTING
            status          = lv_status
*           STANDARDCLASS   =
          TABLES
            allocvaluesnum  = lt_allocvaluesnum
            allocvalueschar = lt_allocvalueschar
            allocvaluescurr = lt_allocvaluescurr
            return          = lt_return.
*        CHECK lv_status IS NOT INITIAL.
        READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZBOX_NUM'.
        gs_out-num = ls_allocvalueschar-value_char_long.
        IF gs_out-num IS INITIAL.
          gs_out-num = '无'.
        ENDIF.
      ENDIF.


      " 每批日常留样量
      SELECT SINGLE
        menge
        meinh
      INTO CORRESPONDING FIELDS OF ls_qprs
      FROM qprs
      WHERE qprs~charg = ls_qals-charg
      AND qprs~prtyp = '3'.
      gs_out-menge2 = ls_qprs-menge && ls_qprs-meinh.
*      gs_out-menge2 = gs_out-menge2 && '123'.
      " 供应商
      SELECT SINGLE
        name_org1
      INTO  ls_but000-name_org1
      FROM but000
      WHERE but000~partner = ls_qals-lifnr.
      gs_out-lifnr = ls_but000-name_org1.
      IF gs_out-lifnr IS INITIAL.
        gs_out-lifnr = '无'.
      ENDIF.
      CLEAR ls_but000.
      "制造商
      SELECT SINGLE
        name_org1
      INTO ls_but000-name_org1
      FROM but000
      WHERE but000~partner = ls_mara-mfrnr.
      gs_out-mfrnr = ls_but000-name_org1.
      IF gs_out-mfrnr IS INITIAL.
        gs_out-mfrnr = '无'.
      ENDIF.
      " 标样
      CLEAR ls_qamv.
      SELECT SINGLE
        *
      INTO CORRESPONDING FIELDS OF ls_qamv
      FROM qamv
      WHERE qamv~prueflos = ls_qals-prueflos
        AND qamv~verwmerkm LIKE 'BY%'
        AND qamv~satzstatus = '1'.
      IF ls_qamv IS NOT INITIAL.
        gs_out-verwmerkm = '需要'.
      ELSE.
        gs_out-verwmerkm = '不需要'.
      ENDIF.

      "检验项/岗位
      CLEAR ls_qamv.
      CLEAR ls_qpmk.
      SELECT
        verwmerkm
      INTO CORRESPONDING FIELDS OF TABLE lt_qamv
      FROM qamv
      WHERE qamv~prueflos = ls_qals-prueflos.

      LOOP AT lt_qamv INTO ls_qamv.
        SELECT DISTINCT
          pqtxt
        INTO CORRESPONDING FIELDS OF TABLE lt_tq11t
        FROM tq11t
        INNER JOIN qpmk
        ON tq11t~prfql = qpmk~prfql
        WHERE qpmk~mkmnr = ls_qamv-verwmerkm
        AND tq11t~sprsl = '1'.
        LOOP AT lt_tq11t INTO ls_tq11t.
          ls_out-pqtxt = ls_tq11t-pqtxt.
          APPEND ls_out TO lt_out.
          SORT lt_out BY pqtxt.
          DELETE ADJACENT DUPLICATES FROM lt_out COMPARING ALL FIELDS.
          DELETE TABLE lt_out WITH TABLE KEY pqtxt = ''.
        ENDLOOP.
      ENDLOOP.
      SORT lt_out BY pqtxt.
      DELETE ADJACENT DUPLICATES FROM lt_out COMPARING ALL FIELDS.
      LOOP AT lt_out INTO ls_out.
        IF gs_out-station IS INITIAL.
          gs_out-station = ls_out-pqtxt.
        ELSE.
          CONCATENATE gs_out-station ls_out-pqtxt INTO gs_out-station SEPARATED BY '、'.
        ENDIF.
      ENDLOOP.

      IF gs_out-zsfxbz IS INITIAL.
        gs_out-zsfxbz = '无'.
      ENDIF.
      APPEND gs_out TO gt_out.
      CLEAR gs_out.
    ENDLOOP.
  ENDIF.
ENDFORM.

*FORM frm_check_message .
*  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
*   ID 'BUKRS' FIELD p_bukrs
*   ID 'ACTVT' DUMMY .
*  IF sy-subrc <> 0.
*    MESSAGE i002.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
*ENDFORM.

FORM frm_sub_set_sheet ."自动优化单元格宽度
  CALL METHOD grf_spreadsheet->fit_widest
    EXPORTING
      name     = space
      no_flush = 'X'.
ENDFORM.

FORM catalog.
  CLEAR fieldcat.

  DEFINE fieldcatset."宏定义
    fieldcat-just = 'C'."字段居中显示
    fieldcat-outputlen = 10."自定义字段的长度
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-coltext = &2."ALV列名
    fieldcat-col_pos = &3."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.
  layout-zebra = 'X'.
  layout-box_fname = 'sel'.
  layout-cwidth_opt = 'X'.


  fieldcatset   'MATNR'           '物料代码'     sy-tabix.
  fieldcatset   'MAKTX'           '品名'     sy-tabix.
  fieldcatset   'CHARG'           '批次号'    sy-tabix.
  fieldcatset   'ZPACK_STA'      '规格'    sy-tabix.
  fieldcatset   'LMENGEIST'       '批量'     sy-tabix.
  fieldcatset   'ZCHANX'          '取样区域'     sy-tabix.
  fieldcatset   'LIFNR'           '供应商'    sy-tabix.
  fieldcatset   'MFRNR'          '制造商'     sy-tabix.
  fieldcatset   'NUM'            '抽样规则（开箱数）'     sy-tabix.
  fieldcatset   'STATION'         '检验项/岗位'     sy-tabix.
  fieldcatset   'GESSTICHPr'      '检验量'    sy-tabix.
  fieldcatset   'MENGE2'         '每批日常留样'    sy-tabix.
  fieldcatset   'VERWMERKM'      '标样'    sy-tabix.
  fieldcatset   'ZCQWDXLY'        '长期稳定性考察'    sy-tabix.
  fieldcatset   'ZJSWDXLY'        '加速稳定性考察'    sy-tabix.
  fieldcatset   'ZDQWDXLY'        '短期稳定性考察'    sy-tabix.
  fieldcatset   'ZSFXBZ'          '留样观察'     sy-tabix.
  fieldcatset   'ZNDWJLY'         '年度外检'     sy-tabix.
*  fieldcatset    'sel'             '标识'     sy-tabix.

ENDFORM.


FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_USER_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
      i_grid_title             = '打印取样单'
*     I_GRID_SETTINGS          =
      is_layout_lvc            = layout
      it_fieldcat_lvc          = fieldcat[]
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = gt_out
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.

FORM frm_user_status  USING i_it_extab TYPE slis_t_extab.
  SET PF-STATUS '0100' .
ENDFORM.                    " FRM_SET_PF_STATUS

FORM frm_user_command USING i_ucomm       TYPE sy-ucomm
      i_wa_selfield TYPE slis_selfield.
  CASE i_ucomm.
    WHEN 'CANC'.  "click POP button in application toolbar
      MESSAGE '退出程序' TYPE 'E'.
      EXIT.
      RETURN.
    WHEN OTHERS.

  ENDCASE.
  CALL SCREEN 0100.                  .  " FORM-Routines
ENDFORM.