*&---------------------------------------------------------------------*
*& 包含               ZQMROOO9_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      DEFINE  DEF_SET_RANGE_MATNR
*&---------------------------------------------------------------------*
*       TEXT：添加明胶物料代码RANGE
*----------------------------------------------------------------------*
DEFINE def_set_range_matnr.
  gs_matnr_mj-sign   = 'I'.
  gs_matnr_mj-option = 'EQ'.
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
  EXPORTING
    input        = &1
  IMPORTING
    output       = lv_str.
*                  EXCEPTIONS
*                    LENGTH_ERROR       = 1
*                    OTHERS             = 2
  gs_matnr_mj-low    = lv_str.
  APPEND gs_matnr_mj TO gr_matnr_mj.
END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*&      FORM  FRM_DOWNLOAD
*&---------------------------------------------------------------------*
*       TEXT：DOI输出EXCEL
*----------------------------------------------------------------------*
FORM frm_download.
  DATA: lv_app_name  TYPE char200,
        lv_sheetname TYPE char50,
        lv_inplace   TYPE c VALUE 'X'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = '正在输出,请稍等。。。'.

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
  lv_app_name = sy-repid.
  CALL METHOD grf_control->init_control
    EXPORTING
      r3_application_name      = lv_app_name
      inplace_enabled          = lv_inplace
      inplace_scroll_documents = 'X'
      parent                   = cl_gui_container=>screen0
    IMPORTING
      error                    = gt_error.
  APPEND gt_error.
* ask the SAP DOI GRF_container for a i_oi_document_proxy for Excel
  CALL METHOD grf_control->get_document_proxy
    EXPORTING
      document_type  = 'Excel.Sheet'
      no_flush       = 'X'
    IMPORTING
      document_proxy = grf_document
      error          = gt_error.
  APPEND gt_error.
  LOOP AT gt_error.
    CALL METHOD gt_error->raise_message
      EXPORTING
        type = 'E'.
  ENDLOOP.
*   Open an existing docu from BDS
  PERFORM frm_get_document_url.
*  open a document saved in business document service.
  CALL METHOD grf_document->open_document
    EXPORTING
      open_inplace = lv_inplace
      document_url = gv_item_url.

  DATA: lv_has TYPE i.
  CALL METHOD grf_document->has_spreadsheet_interface
    EXPORTING
      no_flush     = ''
    IMPORTING
      is_available = lv_has
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
  lv_sheetname = cns_sheetname.
  CALL METHOD grf_spreadsheet->select_sheet
    EXPORTING
      name = lv_sheetname.

  FREE gt_error.
  gv_initialized = 'X'.
  PERFORM frm_fill_excel.
  PERFORM frm_sub_set_sheet.

  lv_sheetname = cns_sheetname2.
  CALL METHOD grf_spreadsheet->select_sheet
    EXPORTING
      name = lv_sheetname.
ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM  FRM_GET_DOCUMENT_URL
*&---------------------------------------------------------------------*
*       TEXT：获取模板URL
*----------------------------------------------------------------------*
FORM frm_get_document_url .
  DATA: lo_bds_instance TYPE REF TO cl_bds_document_set.
  DATA: lt_doc_signature TYPE sbdst_signature,
        ls_doc_signature LIKE LINE OF lt_doc_signature,
        lt_doc_uris      TYPE sbdst_uri,
        ls_doc_uris      LIKE LINE OF lt_doc_uris.
* Document template profile(Maintain T-Code:OAOR)
  ls_doc_signature-prop_value = cns_doc_object_key.
  ls_doc_signature-prop_name = 'BDS_KEYWORD'.
  APPEND ls_doc_signature TO lt_doc_signature.
  CREATE OBJECT lo_bds_instance.
  CALL METHOD lo_bds_instance->get_with_url
    EXPORTING
      classname  = cns_doc_classname
      classtype  = cns_doc_classtype
      object_key = cns_doc_object_key
    CHANGING
      uris       = lt_doc_uris
      signature  = lt_doc_signature.
  FREE lo_bds_instance.
  CLEAR gv_item_url.
*  READ TABLE lt_doc_uris INTO ls_doc_uris INDEX 1.
  LOOP AT lt_doc_uris INTO ls_doc_uris WHERE uri CP '*ZQMR0009.XLS*'.
    gv_item_url = ls_doc_uris-uri.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM  FRM_RANGE_DIM
*&---------------------------------------------------------------------*
*       TEXT：定义数据EXCEL RANGE
*----------------------------------------------------------------------*
FORM frm_range_dim USING uv_i uv_j uv_rows uv_cols uv_name.
  REFRESH gt_range.
  REFRESH gt_excel_input.
  CALL METHOD grf_spreadsheet->insert_range_dim
    EXPORTING
      name     = uv_name
      no_flush = 'X'
      top      = uv_i
      left     = uv_j
      rows     = uv_rows
      columns  = uv_cols.
  gs_range-name         = uv_name.
  gs_range-columns      = uv_cols.
  gs_range-rows         = uv_rows.
  APPEND gs_range TO gt_range.
ENDFORM.

*&---------------------------------------------------------------------*
*&      DEFINE  DEF_SET_RANGE_INPUT
*&---------------------------------------------------------------------*
*       TEXT：添加输出到对应RANGE的数据
*----------------------------------------------------------------------*
DEFINE def_set_range_input.
  gs_excel_input-row = &1.
  gs_excel_input-column = &2.
  lv_str = &3.
  SHIFT lv_str LEFT DELETING LEADING space.
  gs_excel_input-value = lv_str.
*  CONDENSE gs_excel_input-value NO-GAPS.
  APPEND gs_excel_input TO gt_excel_input.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      FORM  FRM_FILL_EXCEL
*&---------------------------------------------------------------------*
*       TEXT：将数据输出至EXCEL
*----------------------------------------------------------------------*
FORM frm_fill_excel.
  DATA:lv_i        TYPE i VALUE 1, "行
       lv_j        TYPE i VALUE 1, "列序号
       lv_str(256) TYPE c. "临时字符串用作去除空格

  "输出检验计划信息
  PERFORM frm_range_dim USING 2 2 9 1 'RANGE_PLOS'.
  DO 9 TIMES.
    ASSIGN COMPONENT sy-index OF STRUCTURE gs_plos TO FIELD-SYMBOL(<fs_plos>).
    IF <fs_plos> IS NOT INITIAL.
      def_set_range_input sy-index 1 <fs_plos>.
    ELSE.
      def_set_range_input sy-index 1 space.
    ENDIF.
  ENDDO.
  CALL METHOD grf_spreadsheet->set_ranges_data
    EXPORTING
      ranges   = gt_range
      contents = gt_excel_input
      no_flush = 'X'.
  "输出物料数据信息
  PERFORM frm_range_dim USING 2 7 11 1 'RANGE_MARA'.
  DO 11 TIMES.
    ASSIGN COMPONENT sy-index OF STRUCTURE gs_material TO FIELD-SYMBOL(<fs_material>).
    IF <fs_material> IS NOT INITIAL.
      def_set_range_input sy-index 1 <fs_material>.
    ELSE.
      def_set_range_input sy-index 1 space.
    ENDIF.
  ENDDO.
  CALL METHOD grf_spreadsheet->set_ranges_data
    EXPORTING
      ranges   = gt_range
      contents = gt_excel_input
      no_flush = 'X'.

  "输出检验计划文本
  PERFORM frm_range_dim USING 14 2 1 1 'RANGE_PLOS_KTEXT'.
  IF gs_plos-ktext IS NOT INITIAL.
    def_set_range_input 1 1 gs_plos-ktext.
  ELSE.
    def_set_range_input 1 1 space.
  ENDIF.
  CALL METHOD grf_spreadsheet->set_ranges_data
    EXPORTING
      ranges   = gt_range
      contents = gt_excel_input
      no_flush = 'X'.
**************HK 添加销售订单信息区域、S码检验计划数据等**************
  "输出基本信息，带出工厂
  PERFORM frm_range_dim USING 2 29 1 1 'RANGE_BASIS_WERKS'.
  IF gs_plos-werks IS NOT INITIAL.
    def_set_range_input 1 1 gs_plos-werks.
  ELSE.
    def_set_range_input 1 1 space.
  ENDIF.
  CALL METHOD grf_spreadsheet->set_ranges_data
    EXPORTING
      ranges   = gt_range
      contents = gt_excel_input
      no_flush = 'X'.

  "输出检验计划文本
  PERFORM frm_range_dim USING 2 23 2 1 'RANGE_SALES'.
  DO 2 TIMES.
    ASSIGN COMPONENT sy-index OF STRUCTURE gs_range_sales TO FIELD-SYMBOL(<fs_sales>).
    IF <fs_sales> IS NOT INITIAL.
      def_set_range_input sy-index 1 <fs_sales>.
    ELSE.
      def_set_range_input sy-index 1 space.
    ENDIF.
  ENDDO.
  CALL METHOD grf_spreadsheet->set_ranges_data
    EXPORTING
      ranges   = gt_range
      contents = gt_excel_input
      no_flush = 'X'.

  PERFORM frm_range_dim USING 14 7 1 1 'RANGE_PLOS_KTEXT'.
  IF gs_range_plandata_s-detail IS NOT INITIAL.
    def_set_range_input 1 1 gs_range_plandata_s-detail.
  ELSE.
    def_set_range_input 1 1 space.
  ENDIF.
  CALL METHOD grf_spreadsheet->set_ranges_data
    EXPORTING
      ranges   = gt_range
      contents = gt_excel_input
      no_flush = 'X'.
  "组合装信息
  PERFORM frm_range_dim USING 2 35 9 1 'RANGE_COMBIN'.
  DO 9 TIMES.
    ASSIGN COMPONENT sy-index OF STRUCTURE gs_range_combin TO FIELD-SYMBOL(<fs_combin>).
    IF <fs_combin> IS NOT INITIAL.
      def_set_range_input sy-index 1 <fs_combin>.
    ELSE.
      def_set_range_input sy-index 1 space.
    ENDIF.
  ENDDO.
  CALL METHOD grf_spreadsheet->set_ranges_data
    EXPORTING
      ranges   = gt_range
      contents = gt_excel_input
      no_flush = 'X'.


***********************END**************************
  "输出样本信息
  PERFORM frm_range_dim USING 18 2 5 1 'RANGE_SAMPLE'.

  DO 5 TIMES.
    ASSIGN COMPONENT sy-index OF STRUCTURE gs_sample TO FIELD-SYMBOL(<fs_sample>).
    IF <fs_sample> IS NOT INITIAL.
      def_set_range_input sy-index 1 <fs_sample>.
    ELSE.
      def_set_range_input sy-index 1 space.
    ENDIF.
  ENDDO.
  CALL METHOD grf_spreadsheet->set_ranges_data
    EXPORTING
      ranges   = gt_range
      contents = gt_excel_input
      no_flush = 'X'.


  "输出S码样本信息
  PERFORM frm_range_dim USING 18 7 5 1 'RANGE_S_SAMPLE'.
**********************HK添加S码检验批的判断，如果为空的话样本信息这块也为空
  IF gs_s_info-prueflos IS NOT INITIAL.
    DO 5 TIMES.
      ASSIGN COMPONENT sy-index OF STRUCTURE gs_s_sample TO FIELD-SYMBOL(<fs_s_sample>).
      IF <fs_s_sample> IS NOT INITIAL.
        def_set_range_input sy-index 1 <fs_s_sample>.
      ELSE.
        def_set_range_input sy-index 1 space.
      ENDIF.
    ENDDO.
    CALL METHOD grf_spreadsheet->set_ranges_data
      EXPORTING
        ranges   = gt_range
        contents = gt_excel_input
        no_flush = 'X'.
  ENDIF.

  "输出批次信息
  PERFORM frm_range_dim USING 2 15 4 1 'RANGE_CHARG'.
  DO 4 TIMES.
    ASSIGN COMPONENT sy-index OF STRUCTURE gs_charg TO FIELD-SYMBOL(<fs_charg>).
    IF <fs_charg> IS NOT INITIAL.
      def_set_range_input sy-index 1 <fs_charg>.
    ELSE.
      def_set_range_input sy-index 1 space.
    ENDIF.
  ENDDO.
  CALL METHOD grf_spreadsheet->set_ranges_data
    EXPORTING
      ranges   = gt_range
      contents = gt_excel_input
      no_flush = 'X'.

  "输出S码信息
  PERFORM frm_range_dim USING 10 15 4 1 'RANGE_S_INFO'.
  DO 4 TIMES.
    ASSIGN COMPONENT sy-index OF STRUCTURE gs_s_info TO FIELD-SYMBOL(<fs_s_info>).
    IF <fs_s_info> IS NOT INITIAL.
      def_set_range_input sy-index 1 <fs_s_info>.
    ELSE.
      def_set_range_input sy-index 1 space.
    ENDIF.
  ENDDO.
  CALL METHOD grf_spreadsheet->set_ranges_data
    EXPORTING
      ranges   = gt_range
      contents = gt_excel_input
      no_flush = 'X'.

  "输出M码已检特性信息
  DESCRIBE TABLE gt_test LINES DATA(lv_rows).
  PERFORM frm_range_dim USING 26 1 lv_rows 19 'RANGE_TEST_TX'.
  LOOP AT gt_test INTO gs_test.
    lv_j = 1.
    DO 19 TIMES.
      ASSIGN COMPONENT lv_j OF STRUCTURE gs_test TO FIELD-SYMBOL(<fs_test>).
      IF <fs_test> IS NOT INITIAL.
        def_set_range_input lv_i lv_j <fs_test>.
      ELSE.
        def_set_range_input lv_i lv_j space.
      ENDIF.
      lv_j = lv_j + 1.
    ENDDO.
    lv_i = lv_i + 1.
  ENDLOOP.

  CALL METHOD grf_spreadsheet->set_ranges_data
    EXPORTING
      ranges   = gt_range
      contents = gt_excel_input
      no_flush = 'X'.

  "输出S码已检特性信息
  DESCRIBE TABLE gt_test_s LINES lv_rows.
  PERFORM frm_range_dim USING 26 20 lv_rows 19 'RANGE_TEST_S'.
  lv_i = 1.
  LOOP AT gt_test_s INTO gs_test.
    lv_j = 1.
    DO 19 TIMES.
      ASSIGN COMPONENT lv_j OF STRUCTURE gs_test TO FIELD-SYMBOL(<fs_test_s>).
      IF <fs_test_s> IS NOT INITIAL.
        def_set_range_input lv_i lv_j <fs_test_s>.
      ELSE.
        def_set_range_input lv_i lv_j space.
      ENDIF.
      lv_j = lv_j + 1.
    ENDDO.
    lv_i = lv_i + 1.
  ENDLOOP.
  CALL METHOD grf_spreadsheet->set_ranges_data
    EXPORTING
      ranges   = gt_range
      contents = gt_excel_input
      no_flush = 'X'.

  "输出S码未检特性信息
  DESCRIBE TABLE gt_untest_s LINES lv_rows.
  PERFORM frm_range_dim USING 26 39 lv_rows 12 'RANGE_UNTEST_S'.
  lv_i = 1.
  LOOP AT gt_untest_s INTO gs_untest_s.
    lv_j = 1.
    DO 12 TIMES.
      ASSIGN COMPONENT lv_j OF STRUCTURE gs_untest_s TO FIELD-SYMBOL(<fs_untest_s>).
      IF <fs_untest_s> IS NOT INITIAL.
        def_set_range_input lv_i lv_j <fs_untest_s>.
      ELSE.
        def_set_range_input lv_i lv_j space.
      ENDIF.
      lv_j = lv_j + 1.
    ENDDO.
    lv_i = lv_i + 1.
  ENDLOOP.
  CALL METHOD grf_spreadsheet->set_ranges_data
    EXPORTING
      ranges   = gt_range
      contents = gt_excel_input
      no_flush = 'X'.

  "输出明胶检验信息
  DESCRIBE TABLE gt_gelatin LINES lv_rows.
  PERFORM frm_range_dim USING 26 51 lv_rows 9 'RANGE_GELATIN'.
  lv_i = 1.
  LOOP AT gt_gelatin INTO gs_gelatin.
    lv_j = 1.
    DO 9 TIMES.
      ASSIGN COMPONENT lv_j OF STRUCTURE gs_gelatin TO FIELD-SYMBOL(<fs_gelatin>).
      IF <fs_gelatin> IS NOT INITIAL.
        def_set_range_input lv_i lv_j <fs_gelatin>.
      ELSE.
        def_set_range_input lv_i lv_j space.
      ENDIF.
      lv_j = lv_j + 1.
    ENDDO.
    lv_i = lv_i + 1.
  ENDLOOP.
  CALL METHOD grf_spreadsheet->set_ranges_data
    EXPORTING
      ranges   = gt_range
      contents = gt_excel_input
      no_flush = 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       TEXT: 获取数据
*----------------------------------------------------------------------*
FORM frm_get_data.

  DATA lt_material_value TYPE TABLE OF zmaterial_tx_value.

  "抓取检验批次计划
  SELECT SINGLE prueflos,charg,offennlzmk,lmengeist,mengeneinh,matnr,sellifnr,
  plnnr,plnal,aufnr,werk,kdauf,plnty,zaehl,stat14
  INTO @DATA(ls_qals)
        FROM qals
        WHERE prueflos EQ @p_pruef.

  "抓取使用决策
  SELECT SINGLE prueflos,vbewertung
  INTO @DATA(ls_qave)
        FROM qave
        WHERE prueflos EQ @p_pruef.

*  IF ls_qals-offennlzmk NE 0.
*    MESSAGE '该检验批尚未完成检验'  TYPE 'S' DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING.
*  ENDIF.


**************2020.03.23.HK修复**********************
  IF p_pruef IS INITIAL AND p_pruef2 IS NOT INITIAL.
    MESSAGE '信息不完整' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF p_pruef IS NOT INITIAL AND ls_qals-stat14 IS INITIAL.
    CLEAR p_flag.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
*       TITLEBAR       = ' '
*       DIAGNOSE_OBJECT             = ' '
        text_question  = '该检验批尚未完成检验。是否选择继续加载？'
        text_button_1  = '是'(001)
*       ICON_BUTTON_1  = ' '
        text_button_2  = '否'(002)
*       ICON_BUTTON_2  = ' '
*       DEFAULT_BUTTON = '1'
*       DISPLAY_CANCEL_BUTTON       = 'X'
*       USERDEFINED_F1_HELP         = ' '
*       START_COLUMN   = 25
*       START_ROW      = 6
*       POPUP_TYPE     =
*       IV_QUICKINFO_BUTTON_1       = ' '
*       IV_QUICKINFO_BUTTON_2       = ' '
      IMPORTING
        answer         = p_flag
*     TABLES
*       PARAMETER      =
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    IF p_flag EQ 2.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDIF.

*****************END**********************



  IF ls_qals IS NOT INITIAL.
    SELECT SINGLE msehi,msehl
    INTO @DATA(ls_t006a1)
          FROM t006a
          WHERE msehi = @ls_qals-mengeneinh
          AND spras = '1'.

    "抓取批次信息
    SELECT SINGLE charg,hsdat,vfdat
    INTO @DATA(ls_mcha)
          FROM mcha
          WHERE charg EQ @ls_qals-charg.

    "抓取供应商信息
    SELECT SINGLE lifnr,name1
    INTO @DATA(ls_lfa1)
          FROM lfa1
          WHERE lifnr EQ @ls_qals-sellifnr
          AND spras EQ @sy-langu.

    "抓取检验计划描述
    SELECT SINGLE plnnr,plnal,ktext
    INTO @DATA(ls_plko)
          FROM plko
          WHERE plnnr EQ @ls_qals-plnnr
          AND plnal EQ @ls_qals-plnal.

    "抓取物料数据
    SELECT SINGLE matnr,mhdhb,iprkz,raube,mtart,groes
    INTO @DATA(ls_mara)
          FROM mara
          WHERE matnr EQ @ls_qals-matnr.

    "抓取S码批次
    SELECT SINGLE a~aufnr,a~charg,a~matnr,b~mtart
    INTO @DATA(ls_mseg)
          FROM mseg AS a
          INNER JOIN mara AS b
          ON a~matnr EQ b~matnr
          WHERE a~aufnr EQ @ls_qals-aufnr
          AND a~bwart EQ @cns_bwart
          AND b~mtart EQ @cns_mtart_bcp.

    "抓取存储条件文本
    IF ls_mara IS NOT INITIAL.
      SELECT SINGLE raube,rbtxt
      INTO @DATA(ls_t142t)
            FROM t142t
            WHERE raube EQ @ls_mara-raube
            AND spras EQ @sy-langu.
    ENDIF.

    "抓取物料描述
    SELECT SINGLE matnr,maktx
    INTO @DATA(ls_makt)
          FROM makt
          WHERE matnr EQ @ls_qals-matnr
          AND spras EQ @sy-langu.

    MOVE-CORRESPONDING ls_qals TO gs_plos.
    MOVE ls_qals-werk TO gs_plos-werks.
************HK M码检验计划描述******************
    CLEAR gs_plos-ktext.
    "取长文本描述
    DATA name LIKE  thead-tdname.
    name = sy-mandt && ls_qals-plnty && ls_qals-plnnr && ls_qals-plnal && ls_qals-zaehl.
    DATA: lines    LIKE TABLE OF tline,
          ls_lines LIKE LINE OF lines.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = 'PLKO'
        language                = sy-langu
        name                    = name
        object                  = 'ROUTING'
      TABLES
        lines                   = lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    CLEAR gs_range_plandata_s-detail.
    IF lines IS NOT INITIAL.
      LOOP AT lines INTO ls_lines.
        gs_plos-ktext = gs_plos-ktext && ls_lines-tdline.
      ENDLOOP.
    ELSE.
      "长文本 没有的话取短文本
      SELECT SINGLE
      ktext
      INTO gs_plos-ktext
      FROM plko
      WHERE plko~plnty = ls_qals-plnty
      AND plko~plnnr = ls_qals-plnnr
      AND plko~plnal = ls_qals-plnal
      AND plko~zaehl = ls_qals-zaehl.
    ENDIF.
    CLEAR:name,lines,ls_lines.
*********END*******************
    CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
      EXPORTING
        matnr  = ls_qals-matnr
*       ATINN  =
      TABLES
        output = lt_material_value[].

    gs_charg-hsdat = ls_mcha-hsdat.
    gs_charg-vfdat = ls_mcha-vfdat.
    PERFORM frm_get_charg_speciailty USING ls_qals-matnr ls_qals-werk ls_qals-charg CHANGING gs_charg gs_material-zpack_sta.

    "读取客户检索项
    READ TABLE lt_material_value INTO DATA(ls_material_value) WITH KEY atnam = cns_zcuscode.
    IF sy-subrc = 0.
      gs_material-zcuscode = ls_material_value-atwrt.
    ENDIF.

    "读取客户物料代码
    READ TABLE lt_material_value INTO ls_material_value WITH KEY atnam = cns_zcumcode.
    IF sy-subrc = 0.
      gs_material-zcumcode = ls_material_value-atwrt.
    ENDIF.

    "读取标准名称
    READ TABLE lt_material_value INTO ls_material_value WITH KEY atnam = cns_zstdname.
    IF sy-subrc = 0.
      gs_material-zstdname = ls_material_value-atwrt.
    ENDIF.

    "读取包装规格
    READ TABLE lt_material_value INTO ls_material_value WITH KEY atnam = cns_zpspe .
    IF sy-subrc = 0.
      gs_material-zpspe = ls_material_value-atwrt.
    ENDIF.

    "读取内容物规格
    IF ls_mara IS NOT INITIAL.
      gs_material-zwgspe = ls_mara-groes.
    ENDIF.

    gs_plos-vbewertung = ls_qave-vbewertung.
    gs_plos-name1      = ls_lfa1-name1.
*    gs_plos-ktext      = ls_plko-ktext.
    gs_plos-mseht      = ls_t006a1-msehl.
******************HK 添加销售订单信息区域*******************
    IF ls_qals-kdauf IS NOT INITIAL.
      SELECT SINGLE
      partner AS kunnr
      bu_sort1
      INTO CORRESPONDING FIELDS OF gs_range_sales
      FROM but000
      INNER JOIN vbak
      ON vbak~kunnr = but000~partner
      WHERE vbak~vbeln = ls_qals-kdauf.
    ENDIF.
************************END ************************************

    MOVE-CORRESPONDING ls_mara TO gs_material.
    gs_material-maktx = ls_makt-maktx.
    gs_material-rbtxt = ls_t142t-rbtxt.
    IF ls_mseg IS NOT INITIAL.
      gs_s_info-aufnr = ls_mseg-aufnr.
      gs_s_info-charg = ls_mseg-charg.
      gs_s_info-matnr = ls_mseg-matnr.
      "抓取S码批次对应的检验批号(原先取的是单条数据)
      SELECT prueflos,charg,aufnr,objnr
      INTO TABLE @DATA(lt_qals_s)
            FROM qals
            WHERE charg EQ @ls_mseg-charg.
      DATA ls_qals_s LIKE LINE OF lt_qals_s.
***************************读取循环内表数据************************8
      LOOP AT lt_qals_s INTO ls_qals_s.
*************************Seashell Huang 20191219*****************
        "修改S码检验结果的取数逻辑，修改的DS版本为第二版
        DATA:status    LIKE TABLE OF jstat,
             ls_status LIKE LINE OF status.
        CALL FUNCTION 'STATUS_READ'
          EXPORTING
            client           = sy-mandt
            objnr            = ls_qals_s-objnr
            only_active      = 'X'
*       IMPORTING
*           OBTYP            =
*           STSMA            =
*           STONR            =
          TABLES
            status           = status
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
        READ TABLE status INTO ls_status WITH KEY stat = 'I0224'.
        IF sy-subrc = 0.
          CLEAR ls_qals_s.
        ENDIF.
*****************************END**********************************
      ENDLOOP.
***************************End****************************************
      gs_s_info-prueflos = ls_qals_s-prueflos.
*****************************HK 添加处理S码检验批相等逻辑
      IF gs_s_info-prueflos = p_pruef.
        CLEAR gs_s_info.
      ENDIF.
********************end


******************** HK S码检验计划数据***************************
      SELECT SINGLE
        prueflos,
        plnty,
        plnnr,
        plnal,
        zaehl
      INTO @DATA(ls_qals2)
      FROM qals
      WHERE qals~prueflos = @ls_qals_s-prueflos.
      "取长文本描述
      name = sy-mandt && ls_qals2-plnty && ls_qals2-plnnr && ls_qals2-plnal && ls_qals2-zaehl.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client                  = sy-mandt
          id                      = 'PLKO'
          language                = sy-langu
          name                    = name
          object                  = 'ROUTING'
        TABLES
          lines                   = lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      CLEAR gs_range_plandata_s-detail.
      IF lines IS NOT INITIAL.
        LOOP AT lines INTO ls_lines.
          gs_range_plandata_s-detail = gs_range_plandata_s-detail && ls_lines-tdline.
        ENDLOOP.
      ELSE.
        "长文本 没有的话取短文本
        SELECT SINGLE
        ktext
        INTO gs_range_plandata_s-detail
        FROM plko
        WHERE plko~plnty = ls_qals2-plnty
        AND plko~plnnr = ls_qals2-plnnr
        AND plko~plnal = ls_qals2-plnal
        AND plko~zaehl = ls_qals2-zaehl.
      ENDIF.
********************END***********************************
      PERFORM frm_get_test_speciality USING ls_qals_s-prueflos CHANGING gt_test_s gs_s_sample.
      PERFORM frm_get_untest_speciality USING ls_qals_s-prueflos .
      PERFORM frm_get_gelatin_test USING ls_qals_s-aufnr.
    ENDIF.
    PERFORM frm_get_test_speciality USING p_pruef CHANGING gt_test gs_sample.
  ENDIF.

*************2020.03.24添加组合装信息的获取*************
  PERFORM frm_get_combin USING p_pruef.
*******************END*******************************
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

*&---------------------------------------------------------------------*
*&      FORM  FRM_SUB_SET_SHEET
*&---------------------------------------------------------------------*
*       TEXT：自动优化单元格宽度
*----------------------------------------------------------------------*
FORM frm_sub_set_sheet .
  CALL METHOD grf_spreadsheet->fit_widest
    EXPORTING
      name     = space
      no_flush = 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM  FRM_GET_TEST_SPECIALITY
*&---------------------------------------------------------------------*
*       TEXT：获取已检特性
*----------------------------------------------------------------------*
FORM frm_get_test_speciality USING uv_pruef LIKE p_pruef
CHANGING ct_test TYPE STANDARD TABLE
  cs_sample TYPE typ_sample."抓取的检修特性类型：
  "T-已检特性 S-S码已检
  DATA: lv_date   LIKE  sy-datum, "标准日期
        lv_time   LIKE  sy-uzeit, "标准时间
        lv_strlen TYPE  i.        "字符串长度

  "根据检验批号抓取检验组供后续检查
  SELECT SINGLE prueflos,plnnr,plnty,aufpl
  INTO @DATA(ls_qals)
        FROM qals
        WHERE prueflos EQ @uv_pruef.

  "抓取采样图形信息采集编号/批准日期
  SELECT SINGLE plos,pn_nr,frgdt
  INTO @DATA(ls_qprn)
        FROM qprn
        WHERE plos EQ @uv_pruef.

  IF ls_qprn IS NOT INITIAL.
    "抓取详细样本信息
    DATA ls_qprs TYPE qprs.
    SELECT SINGLE pn_nr,menge,meinh,gbtyp,objnr
    INTO CORRESPONDING FIELDS OF @ls_qprs
    FROM qprs
    WHERE pn_nr EQ @ls_qprn-pn_nr
    AND prtyp EQ '1'.
****************************Seashell Huang 2019.12.17***************排除STAT包含I0076的条目*************
    DATA:status    LIKE TABLE OF jstat,
         ls_status LIKE LINE OF status,
         ls_qprs2  TYPE qprs,
         lt_qprs   TYPE TABLE OF qprs.
    SELECT  pn_nr,menge,meinh,gbtyp,objnr
    INTO CORRESPONDING FIELDS OF TABLE @lt_qprs
    FROM qprs
    WHERE plos2 EQ @uv_pruef
      AND qprs~prtyp = '1'
    ORDER BY PRIMARY KEY.
    CLEAR ls_qprs.
    LOOP AT lt_qprs INTO ls_qprs2.
      CALL FUNCTION 'STATUS_READ'
        EXPORTING
          client           = sy-mandt
          objnr            = ls_qprs2-objnr
          only_active      = 'X'
* IMPORTING
*         OBTYP            =
*         STSMA            =
*         STONR            =
        TABLES
          status           = status
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      READ TABLE status INTO ls_status WITH KEY stat = 'I0076'.
      IF sy-subrc = 0.
        CLEAR ls_qprs.
      ELSEIF sy-subrc <> 0.
        MOVE-CORRESPONDING ls_qprs2 TO ls_qprs.
      ENDIF.
    ENDLOOP.
    IF ls_qprs IS NOT INITIAL.
      CLEAR ls_qprn.
      "抓取采样图形信息采集编号/批准日期
      SELECT SINGLE plos,pn_nr,frgdt
      INTO CORRESPONDING FIELDS OF @ls_qprn
      FROM qprn
      WHERE plos EQ @uv_pruef
      AND pn_nr EQ @ls_qprs-pn_nr.
    ENDIF.
***********************************end************************************************
    IF ls_qprs IS NOT INITIAL.
      "抓取样本单位文本
      SELECT SINGLE msehi,msehl
      INTO @DATA(ls_t006a2)
            FROM t006a
            WHERE msehi = @ls_qprs-meinh
            AND spras = '1'.
      "抓取样本容器文本
      SELECT SINGLE gebindetyp,kurztext
      INTO @DATA(ls_tq42t)
            FROM tq42t
            WHERE gebindetyp EQ @ls_qprs-gbtyp
            AND sprache    EQ '1'.
    ENDIF.
  ENDIF.

  "抓取已检特性信息
  SELECT prueflos,merknr,mbewertg,mittelwert,pruefbemkt,
  vorglfnr
  INTO TABLE @DATA(lt_qamr)
        FROM qamr
        WHERE prueflos EQ @uv_pruef.

  IF lt_qamr IS NOT INITIAL.
    "抓取签名ID
    SELECT prueflos,vorglfnr,merknr,sign_id,original_input,mittelwert,pruefbemkt,prltextkz,probenr
    INTO TABLE @DATA(lt_qasr)
          FROM qasr
          FOR ALL ENTRIES IN @lt_qamr
          WHERE prueflos EQ @lt_qamr-prueflos
          AND vorglfnr EQ @lt_qamr-vorglfnr
          AND merknr   EQ @lt_qamr-merknr.

    "抓取检验方法,检验单位等信息
    SELECT prueflos,vorglfnr,merknr,pmtversion,
    dummy40,kurztext,pruefquali,
    steuerkz,verwmerkm,mkversion,
    qpmk_werks,pmethode,qmtb_werks,masseinhsw
    INTO TABLE @DATA(lt_qamv)
          FROM qamv
          FOR ALL ENTRIES IN @lt_qamr
          WHERE prueflos EQ @uv_pruef
          AND vorglfnr EQ @lt_qamr-vorglfnr
          AND merknr   EQ @lt_qamr-merknr.
    SORT lt_qamv BY vorglfnr ASCENDING merknr ASCENDING.

    IF lt_qamv IS NOT INITIAL.
      "抓取检验结果单位文本
      SELECT msehi,msehl
      INTO TABLE @DATA(lt_t006a)
            FROM t006a
            FOR ALL ENTRIES IN @lt_qamv
            WHERE msehi = @lt_qamv-masseinhsw
            AND spras = '1'.
      "抓取检验员资格文本
      SELECT prfql,pqtxt
      INTO TABLE @DATA(lt_tq11t)
            FROM tq11t
            FOR ALL ENTRIES IN @lt_qamv
            WHERE prfql EQ @lt_qamv-pruefquali.

      "抓取工厂,检验版本信息
      SELECT mkmnr,werks,version,sortfeld,dummy40
      INTO TABLE @DATA(lt_qpmk)
            FROM qpmk
            FOR ALL ENTRIES IN @lt_qamv
            WHERE mkmnr   EQ @lt_qamv-verwmerkm
            AND version EQ @lt_qamv-mkversion
            AND werks   EQ @lt_qamv-qpmk_werks.

      SELECT aufpl,plnkn,arbid
      INTO TABLE @DATA(lt_afvc)
            FROM afvc
            FOR ALL ENTRIES IN @lt_qamv
            WHERE aufpl EQ @ls_qals-aufpl
            AND plnnr EQ @ls_qals-plnnr
            AND plnty EQ @ls_qals-plnty
            AND aplzl EQ @lt_qamv-vorglfnr.

      IF lt_afvc IS NOT INITIAL.
        "抓取工作中心
        SELECT objid,arbpl
        INTO TABLE @DATA(lt_crhd)
              FROM crhd
              FOR ALL ENTRIES IN @lt_afvc
              WHERE objid EQ @lt_afvc-arbid.
      ENDIF.
    ENDIF.

    "抓取审核/批准人信息
    SELECT sign_id,sign_index,sign_tstamp,
    sign_name_last,sign_name_first
    INTO TABLE @DATA(lt_signs)
          FROM signs
          FOR ALL ENTRIES IN @lt_qasr
          WHERE sign_id EQ @lt_qasr-sign_id.

    "抓取规格标准文本
    SELECT mkmnr,zaehler,version,sprache,kurztext,ltextkz
    INTO TABLE @DATA(lt_qpmt)
          FROM qpmt
          FOR ALL ENTRIES IN @lt_qamv
          WHERE mkmnr EQ @lt_qamv-verwmerkm
          AND version EQ @lt_qamv-mkversion
          AND zaehler EQ @lt_qamv-qpmk_werks
          AND sprache IN ( 'Z','E','(' ).

    "抓取检验方法文本
    SELECT pmtnr,werks,version,sprache,kurztext,ltextkz
    INTO TABLE @DATA(lt_qmtt)
          FROM qmtt
          FOR ALL ENTRIES IN @lt_qamv
          WHERE pmtnr   EQ @lt_qamv-pmethode
          AND version EQ @lt_qamv-pmtversion
          AND werks   EQ @lt_qamv-qmtb_werks
          AND sprache IN ( '1','E' ).
  ENDIF.

*  IF ls_qals-prueflos IS NOT INITIAL AND ls_qals-prueflos <> ls_qprn-plos.
  cs_sample-frgdt = ls_qprn-frgdt.
  MOVE-CORRESPONDING ls_qprs TO cs_sample.
  cs_sample-mseht = ls_t006a2-msehl.
  cs_sample-tq42t_kurztext = ls_tq42t-kurztext.
*  ENDIF.


  DATA: lt_line  TYPE TABLE OF tline, "长文本表
        lv_fname TYPE thead-tdname. "长文本对象
  LOOP AT lt_qamv INTO DATA(ls_qamv).
    READ TABLE lt_qamr  INTO DATA(ls_qamr)    WITH KEY prueflos = uv_pruef merknr = ls_qamv-merknr vorglfnr = ls_qamv-vorglfnr.
    READ TABLE lt_qpmk  INTO DATA(ls_qpmk)    WITH KEY mkmnr   = ls_qamv-verwmerkm  version = ls_qamv-mkversion   werks   = ls_qamv-qpmk_werks.
    READ TABLE lt_qpmt  INTO DATA(ls_qpmt_z1) WITH KEY mkmnr   = ls_qamv-verwmerkm  version = ls_qamv-mkversion   zaehler = ls_qamv-qpmk_werks  sprache = 'Z'.
    READ TABLE lt_qpmt  INTO DATA(ls_qpmt_en) WITH KEY mkmnr   = ls_qamv-verwmerkm  version = ls_qamv-mkversion   zaehler = ls_qamv-qpmk_werks  sprache = 'E'.
    READ TABLE lt_qpmt  INTO DATA(ls_qpmt_z2) WITH KEY mkmnr   = ls_qamv-verwmerkm  version = ls_qamv-mkversion   zaehler = ls_qamv-qpmk_werks  sprache = '('.
    READ TABLE lt_qmtt  INTO DATA(ls_qmtt_zh) WITH KEY pmtnr   = ls_qamv-pmethode   version = ls_qamv-pmtversion  werks   = ls_qamv-qmtb_werks  sprache = '1'.
    READ TABLE lt_qmtt  INTO DATA(ls_qmtt_en) WITH KEY pmtnr   = ls_qamv-pmethode   version = ls_qamv-pmtversion  werks   = ls_qamv-qmtb_werks sprache = 'E'.
    READ TABLE lt_tq11t INTO DATA(ls_tq11t)   WITH KEY prfql   = ls_qamv-pruefquali.
    READ TABLE lt_qasr  INTO DATA(ls_qasr) WITH KEY prueflos   = ls_qamr-prueflos   vorglfnr = ls_qamr-vorglfnr merknr = ls_qamr-merknr.
    READ TABLE lt_signs INTO DATA(ls_signs_index2) WITH KEY sign_id  = ls_qasr-sign_id sign_index = '2'.
    READ TABLE lt_signs INTO DATA(ls_signs_index3) WITH KEY sign_id  = ls_qasr-sign_id sign_index = '3'.
    READ TABLE lt_t006a INTO DATA(ls_t006a) WITH KEY msehi = ls_qamv-masseinhsw.
    READ TABLE lt_afvc  INTO DATA(ls_afvc)  WITH KEY plnkn     = ls_qamv-vorglfnr.
    IF sy-subrc = 0.
      READ TABLE lt_crhd  INTO DATA(ls_crhd)  WITH KEY objid = ls_afvc-arbid.
    ENDIF.
    CLEAR gs_test.
    gs_test-verwmerkm      = ls_qamv-verwmerkm.
    gs_test-original_input = ls_qasr-original_input.
    gs_test-mbewertg       = ls_qamr-mbewertg.
    CONCATENATE ls_qamv-prueflos ls_qamv-vorglfnr ls_qamv-merknr ls_qamv-mkversion '1' INTO lv_fname.
    PERFORM frm_get_longtext USING 'QPRP' '1' lv_fname 'QEEPROBE' CHANGING lt_line.
    IF lt_line IS NOT INITIAL.
      LOOP AT lt_line INTO DATA(ls_line).
        CONCATENATE gs_test-pruefbemkt ls_line-tdline INTO gs_test-pruefbemkt.
      ENDLOOP.
      FREE lt_line.
    ELSE.
      gs_test-pruefbemkt  = ls_qasr-pruefbemkt.
    ENDIF.
****************判断是否有长文本标识，如果有的话取长文本HK
    IF ls_qasr-prltextkz = 'X'.
      DATA lv_long TYPE thead-tdname.
      DATA:tline LIKE TABLE OF tline,
           line  TYPE tdline.
      CLEAR:line.
      lv_long = ls_qasr-prueflos && ls_qasr-vorglfnr && ls_qasr-merknr && ls_qasr-probenr.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client                  = sy-mandt
          id                      = 'QPRP'
          language                = sy-langu
          name                    = lv_long
          object                  = 'QEEPROBE'
*         ARCHIVE_HANDLE          = 0
*         LOCAL_CAT               = ' '
*       IMPORTING
*         HEADER                  =
*         OLD_LINE_COUNTER        =
        TABLES
          lines                   = tline
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      CLEAR gs_test-pruefbemkt.
      CLEAR ls_line.
      LOOP AT tline INTO ls_line.
        gs_test-pruefbemkt = gs_test-pruefbemkt && ls_line-tdline.
        CLEAR line.
      ENDLOOP.

    ENDIF.
**********************END******************************

    IF gs_test-verwmerkm NE ''.
*      gs_test-sortfeld         = ls_qpmk-sortfeld.
****************HK 20200102修改DUmmy40的取值
      gs_test-sortfeld         = ls_qpmk-dummy40.
***************END******************
      CONCATENATE sy-mandt ls_qamv-qpmk_werks ls_qamv-verwmerkm ls_qamv-mkversion '1' INTO lv_fname.
      PERFORM frm_get_longtext USING 'QPMT' '1' lv_fname 'QPMERKMAL' CHANGING lt_line.
      IF lt_line IS NOT INITIAL.
        LOOP AT lt_line INTO ls_line.
          CONCATENATE gs_test-qamv_kurztext ls_line-tdline INTO gs_test-qamv_kurztext.
        ENDLOOP.
        FREE lt_line.
      ELSE.
        gs_test-qamv_kurztext  = ls_qamv-kurztext.
      ENDIF.
      gs_test-mseht            = ls_t006a-msehl.
      IF gs_test-mseht EQ ''.
        gs_test-mseht = '/'.
      ENDIF.
      gs_test-pmtversion       = ls_qamv-pmtversion.

      lv_strlen = strlen( ls_qamv-steuerkz ) - 1.
      IF lv_strlen > 0.
        gs_test-steuerkz = ls_qamv-steuerkz+25(1)."是否打印
      ENDIF.

      CONCATENATE sy-mandt ls_qamv-qpmk_werks ls_qamv-verwmerkm ls_qamv-mkversion 'Z' INTO lv_fname.
      PERFORM frm_get_longtext USING 'QPMT' 'Z' lv_fname 'QPMERKMAL' CHANGING lt_line.
      IF lt_line IS NOT INITIAL.
        LOOP AT lt_line INTO ls_line.
          CONCATENATE gs_test-qpmt_kurztext_z1 ls_line-tdline INTO gs_test-qpmt_kurztext_z1.
        ENDLOOP.
        FREE lt_line.
      ELSE.
        gs_test-qpmt_kurztext_z1  = ls_qpmt_z1-kurztext.
      ENDIF.
      IF ls_qpmt_z1-ltextkz <> 'X'.
        gs_test-qpmt_kurztext_z1  = ls_qpmt_z1-kurztext.
      ENDIF.

      CONCATENATE sy-mandt ls_qamv-qpmk_werks ls_qamv-verwmerkm ls_qamv-mkversion 'E' INTO lv_fname.
      PERFORM frm_get_longtext USING 'QPMT' 'E' lv_fname 'QPMERKMAL' CHANGING lt_line.
      IF lt_line IS NOT INITIAL.
        LOOP AT lt_line INTO ls_line.
          CONCATENATE gs_test-qpmt_kurztext_en ls_line-tdline INTO gs_test-qpmt_kurztext_en.
        ENDLOOP.
        FREE lt_line.
      ELSE.
        gs_test-qpmt_kurztext_en = ls_qpmt_en-kurztext.
      ENDIF.
      IF ls_qpmt_en-ltextkz <> 'X'.
        gs_test-qpmt_kurztext_en = ls_qpmt_en-kurztext.
      ENDIF.

      CONCATENATE sy-mandt ls_qamv-qpmk_werks ls_qamv-verwmerkm ls_qamv-mkversion '(' INTO lv_fname.
      PERFORM frm_get_longtext USING 'QPMT' '(' lv_fname 'QPMERKMAL' CHANGING lt_line.
      IF lt_line IS NOT INITIAL.
        LOOP AT lt_line INTO ls_line.
          CONCATENATE gs_test-dummy40  ls_line-tdline INTO gs_test-dummy40.
        ENDLOOP.
        FREE lt_line.
      ELSE.
        gs_test-dummy40 = ls_qpmt_z2-kurztext.
      ENDIF.
      IF ls_qpmt_z2-ltextkz <> 'X'.
        gs_test-dummy40 = ls_qpmt_z2-kurztext.
      ENDIF.

      CONCATENATE ls_signs_index2-sign_name_last ls_signs_index2-sign_name_first INTO gs_test-sign_name_index2.
      CONCATENATE ls_signs_index3-sign_name_last ls_signs_index3-sign_name_first INTO gs_test-sign_name_index3.

      "时戳转换为标准日期
      CALL FUNCTION 'IB_CONVERT_FROM_TIMESTAMP'
        EXPORTING
          i_timestamp = ls_signs_index3-sign_tstamp
*         I_TZONE     = SY-ZONLO
        IMPORTING
          e_datlo     = lv_date
          e_timlo     = lv_time.

      CONCATENATE lv_date '' lv_time INTO gs_test-tstamp.
      CONDENSE gs_test-tstamp NO-GAPS.
      gs_test-arbpl            = ls_crhd-arbpl.
*          gs_test-s_or_m           = 'M'.
      gs_test-pqtxt            = ls_tq11t-pqtxt.

      CONCATENATE sy-mandt ls_qamv-qpmk_werks ls_qamv-pmethode ls_qamv-pmtversion '1' INTO lv_fname.
      PERFORM frm_get_longtext USING 'QMTT' '1' lv_fname 'QPMETHODE' CHANGING lt_line.
      IF lt_line IS NOT INITIAL.
        LOOP AT lt_line INTO ls_line.
          CONCATENATE gs_test-qmtt_kurztext_z1  ls_line-tdline INTO gs_test-qmtt_kurztext_z1.
        ENDLOOP.
        FREE lt_line.
      ELSE.
        gs_test-qmtt_kurztext_z1  = ls_qmtt_zh-kurztext.
      ENDIF.
      IF ls_qmtt_zh-ltextkz <> 'X'.
        gs_test-qmtt_kurztext_z1  = ls_qmtt_zh-kurztext.
      ENDIF.
      CONCATENATE sy-mandt ls_qamv-qpmk_werks ls_qamv-pmethode ls_qamv-pmtversion 'E' INTO lv_fname.
      PERFORM frm_get_longtext USING 'QMTT' 'E' lv_fname 'QPMETHODE' CHANGING lt_line.
      IF lt_line IS NOT INITIAL.
        LOOP AT lt_line INTO ls_line.
          CONCATENATE gs_test-qmtt_kurztext_en ls_line-tdline INTO gs_test-qmtt_kurztext_en.
        ENDLOOP.
        FREE lt_line.
      ELSE.
        gs_test-qmtt_kurztext_en  = ls_qmtt_en-kurztext.
      ENDIF.
      IF ls_qmtt_en-ltextkz <> 'X'.
        gs_test-qmtt_kurztext_en  = ls_qmtt_en-kurztext.
      ENDIF.
      APPEND gs_test TO ct_test.

    ENDIF.

    CLEAR ls_qamv.
    CLEAR ls_qpmk.
    CLEAR ls_qpmt_z1.
    CLEAR ls_qpmt_z2.
    CLEAR ls_qpmt_en.
    CLEAR ls_qmtt_zh.
    CLEAR ls_qmtt_en.
    CLEAR ls_tq11t.
    CLEAR ls_t006a.
    CLEAR ls_signs_index2.
    CLEAR ls_signs_index3.
    CLEAR ls_afvc.
    CLEAR ls_crhd.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM  frm_get_untest_speciality
*&---------------------------------------------------------------------*
*       TEXT：抓取的未检特性仅对S码：
*----------------------------------------------------------------------*
FORM frm_get_untest_speciality USING uv_pruef LIKE p_pruef.
  DATA: lv_vorglfnr LIKE qamr-vorglfnr, "节点编号
        lv_strlen   TYPE i.             "字符串长度

  "抓取特性组
  SELECT SINGLE prueflos,plnnr,plnty,aufpl
  INTO @DATA(ls_qals)
        FROM qals
        WHERE prueflos EQ @uv_pruef.

  SELECT prueflos,merknr,vorglfnr
  INTO TABLE @DATA(lt_qamr)
        FROM qamr
        WHERE prueflos EQ @uv_pruef.

  SELECT prueflos,vorglfnr,merknr,pmtversion,
  dummy40,kurztext,pruefquali,
  steuerkz,verwmerkm,mkversion,
  qpmk_werks,pmethode,qmtb_werks,masseinhsw,
  zaehl
  INTO TABLE @DATA(lt_qamv)
        FROM qamv
        WHERE prueflos EQ @uv_pruef
          AND satzstatus = 4.
  SORT lt_qamv BY vorglfnr ASCENDING merknr ASCENDING.

  "去除QAMV中有并且QAMR中也有的特性,
  LOOP AT lt_qamv ASSIGNING FIELD-SYMBOL(<fs_qamv>).
    READ TABLE lt_qamr TRANSPORTING NO FIELDS WITH KEY prueflos = <fs_qamv>-prueflos merknr = <fs_qamv>-merknr vorglfnr = <fs_qamv>-vorglfnr.
    IF sy-subrc = 0.
      DELETE lt_qamv WHERE prueflos = <fs_qamv>-prueflos AND merknr = <fs_qamv>-merknr AND vorglfnr = <fs_qamv>-vorglfnr.
    ENDIF.
  ENDLOOP.
  IF ls_qals IS NOT INITIAL.
    IF lt_qamv IS NOT INITIAL.
      "抓取检验结果单位文本
      SELECT msehi,msehl
      INTO TABLE @DATA(lt_t006a)
            FROM t006a
            FOR ALL ENTRIES IN @lt_qamv
            WHERE msehi = @lt_qamv-masseinhsw
            AND spras = '1'.

      "抓检验员资格文本
      SELECT prfql,pqtxt
      INTO TABLE @DATA(lt_tq11t)
            FROM tq11t
            FOR ALL ENTRIES IN @lt_qamv
            WHERE prfql EQ @lt_qamv-pruefquali.

      "获取工厂信息
      SELECT mkmnr,werks,version,sortfeld
      INTO TABLE @DATA(lt_qpmk)
            FROM qpmk
            FOR ALL ENTRIES IN @lt_qamv
            WHERE mkmnr   EQ @lt_qamv-verwmerkm
            AND version EQ @lt_qamv-mkversion
            AND werks   EQ @lt_qamv-qpmk_werks.

      SELECT aufpl,plnkn,arbid
      INTO TABLE @DATA(lt_afvc)
            FROM afvc
            FOR ALL ENTRIES IN @lt_qamv
            WHERE aufpl EQ @ls_qals-aufpl
            AND plnnr EQ @ls_qals-plnnr
            AND plnty EQ @ls_qals-plnty
            AND plnkn EQ @lt_qamv-vorglfnr.

      IF lt_afvc IS NOT INITIAL.
        "抓取工作中心
        SELECT objid,arbpl
        INTO TABLE @DATA(lt_crhd)
              FROM crhd
              FOR ALL ENTRIES IN @lt_afvc
              WHERE objid EQ @lt_afvc-arbid.
      ENDIF.
    ENDIF.

    "抓取检验方法,版本
    SELECT mkmnr,zaehler,version,sprache,kurztext
    INTO TABLE @DATA(lt_qpmt)
          FROM qpmt
          FOR ALL ENTRIES IN @lt_qamv
          WHERE mkmnr   EQ @lt_qamv-verwmerkm
          AND version EQ @lt_qamv-mkversion
          AND zaehler EQ @lt_qamv-qpmk_werks
          AND sprache IN ( 'Z','E','(' ).

    "抓取检验方法描述
    SELECT pmtnr,werks,version,sprache,kurztext
    INTO TABLE @DATA(lt_qmtt)
          FROM qmtt
          FOR ALL ENTRIES IN @lt_qamv
          WHERE pmtnr   EQ @lt_qamv-pmethode
          AND version EQ @lt_qamv-pmtversion
          AND werks   EQ @lt_qamv-qmtb_werks
          AND sprache IN ( '1','E' ).
  ENDIF.

  DATA: lt_line  TYPE TABLE OF tline, "长文本表
        lv_fname TYPE thead-tdname. "长文本对象
  LOOP AT lt_qamv ASSIGNING <fs_qamv>.
    READ TABLE lt_qpmk  INTO DATA(ls_qpmk)    WITH KEY mkmnr   = <fs_qamv>-verwmerkm  version  = <fs_qamv>-mkversion   werks    = <fs_qamv>-qpmk_werks.
    READ TABLE lt_qpmt  INTO DATA(ls_qpmt_z1) WITH KEY mkmnr   = <fs_qamv>-verwmerkm  version  = <fs_qamv>-mkversion   zaehler  = <fs_qamv>-qpmk_werks sprache = 'Z'.
    READ TABLE lt_qpmt  INTO DATA(ls_qpmt_en) WITH KEY mkmnr   = <fs_qamv>-verwmerkm  version  = <fs_qamv>-mkversion   zaehler  = <fs_qamv>-qpmk_werks sprache = 'E'.
    READ TABLE lt_qpmt  INTO DATA(ls_qpmt_z2) WITH KEY mkmnr   = <fs_qamv>-verwmerkm  version  = <fs_qamv>-mkversion   zaehler  = <fs_qamv>-qpmk_werks  sprache = '('.
    READ TABLE lt_qmtt  INTO DATA(ls_qmtt_zh) WITH KEY pmtnr   = <fs_qamv>-pmethode   version  = <fs_qamv>-pmtversion  werks    = <fs_qamv>-qmtb_werks sprache = '1'.
    READ TABLE lt_qmtt  INTO DATA(ls_qmtt_en) WITH KEY pmtnr   = <fs_qamv>-pmethode   version  = <fs_qamv>-pmtversion  werks    = <fs_qamv>-qmtb_werks sprache = 'E'.
    READ TABLE lt_tq11t INTO DATA(ls_tq11t)   WITH KEY prfql   = <fs_qamv>-pruefquali.
    READ TABLE lt_t006a INTO DATA(ls_t006a)   WITH KEY msehi   = <fs_qamv>-masseinhsw.
    READ TABLE lt_afvc  INTO DATA(ls_afvc)    WITH KEY plnkn   = <fs_qamv>-vorglfnr.
    IF sy-subrc = 0.
      READ TABLE lt_crhd  INTO DATA(ls_crhd)    WITH KEY objid   = ls_afvc-arbid.
    ENDIF.
    gs_untest_s-verwmerkm = <fs_qamv>-verwmerkm.
    IF gs_untest_s-verwmerkm NE ''.
      gs_untest_s-sortfeld         = ls_qpmk-sortfeld.
      CONCATENATE sy-mandt <fs_qamv>-qpmk_werks <fs_qamv>-verwmerkm <fs_qamv>-mkversion '1' INTO lv_fname.
      PERFORM frm_get_longtext USING 'QPMT' '1' lv_fname 'QPMERKMAL' CHANGING lt_line.
      IF lt_line IS NOT INITIAL.
        LOOP AT lt_line INTO  DATA(ls_line).
          CONCATENATE gs_untest_s-qamv_kurztext ls_line-tdline INTO gs_untest_s-qamv_kurztext.
        ENDLOOP.
        FREE lt_line.
      ELSE.
        gs_untest_s-qamv_kurztext  = <fs_qamv>-kurztext.
      ENDIF.
      gs_untest_s-pmtversion       = <fs_qamv>-pmtversion.
      lv_strlen = strlen( <fs_qamv>-steuerkz ) - 1.
      IF lv_strlen > 0.
        gs_untest_s-steuerkz = <fs_qamv>-steuerkz+25(1)."是否打印
      ENDIF.

      CONCATENATE sy-mandt <fs_qamv>-qpmk_werks <fs_qamv>-verwmerkm <fs_qamv>-mkversion 'Z' INTO lv_fname.
      PERFORM frm_get_longtext USING 'QPMT' 'Z' lv_fname 'QPMERKMAL' CHANGING lt_line.
      IF lt_line IS NOT INITIAL.
        LOOP AT lt_line INTO ls_line.
          CONCATENATE gs_untest_s-qpmt_kurztext_z1 ls_line-tdline INTO gs_untest_s-qpmt_kurztext_z1.
        ENDLOOP.
        FREE lt_line.
      ELSE.
        gs_untest_s-qpmt_kurztext_z1  = ls_qpmt_z1-kurztext.
      ENDIF.

      CONCATENATE sy-mandt <fs_qamv>-qpmk_werks <fs_qamv>-verwmerkm <fs_qamv>-mkversion 'E' INTO lv_fname.
      PERFORM frm_get_longtext USING 'QPMT' 'E' lv_fname 'QPMERKMAL' CHANGING lt_line.
      IF lt_line IS NOT INITIAL.
        LOOP AT lt_line INTO ls_line.
          CONCATENATE gs_untest_s-qpmt_kurztext_en ls_line-tdline INTO gs_untest_s-qpmt_kurztext_en.
        ENDLOOP.
        FREE lt_line.
      ELSE.
        gs_untest_s-qpmt_kurztext_en  = ls_qpmt_en-kurztext.
      ENDIF.


      CONCATENATE sy-mandt <fs_qamv>-qpmk_werks <fs_qamv>-verwmerkm <fs_qamv>-mkversion '(' INTO lv_fname.
      PERFORM frm_get_longtext USING 'QPMT' '(' lv_fname 'QPMERKMAL' CHANGING lt_line.
      IF lt_line IS NOT INITIAL.
        LOOP AT lt_line INTO ls_line.
          CONCATENATE gs_untest_s-dummy40 ls_line-tdline INTO gs_untest_s-dummy40.
        ENDLOOP.
        FREE lt_line.
      ELSE.
        gs_untest_s-dummy40  = ls_qpmt_z2-kurztext.
      ENDIF.

      CONCATENATE sy-mandt <fs_qamv>-qpmk_werks <fs_qamv>-pmethode <fs_qamv>-pmtversion '1' INTO lv_fname.
      PERFORM frm_get_longtext USING 'QMTT' '1' lv_fname 'QPMETHODE' CHANGING lt_line.
      IF lt_line IS NOT INITIAL.
        LOOP AT lt_line INTO ls_line.
          CONCATENATE gs_untest_s-qmtt_kurztext_z1 ls_line-tdline INTO gs_untest_s-qmtt_kurztext_z1.
        ENDLOOP.
        FREE lt_line.
      ELSE.
        gs_untest_s-qmtt_kurztext_z1 = ls_qmtt_zh-kurztext.
      ENDIF.

      CONCATENATE sy-mandt <fs_qamv>-qpmk_werks <fs_qamv>-pmethode <fs_qamv>-pmtversion 'E' INTO lv_fname.
      PERFORM frm_get_longtext USING 'QMTT' 'E' lv_fname 'QPMETHODE' CHANGING lt_line.
      IF lt_line IS NOT INITIAL.
        LOOP AT lt_line INTO ls_line.
          CONCATENATE gs_untest_s-qmtt_kurztext_en ls_line-tdline INTO gs_untest_s-qmtt_kurztext_en.
        ENDLOOP.
        FREE lt_line.
      ELSE.
        gs_untest_s-qmtt_kurztext_en  = ls_qmtt_en-kurztext.
      ENDIF.

      gs_untest_s-mseht            = ls_t006a-msehl.
      IF gs_untest_s-mseht EQ ''.
        gs_untest_s-mseht = '/'.
      ENDIF.
      gs_untest_s-pqtxt            = ls_tq11t-pqtxt.
      APPEND gs_untest_s TO gt_untest_s.
      CLEAR gs_untest_s.
    ENDIF.
    CLEAR ls_qpmk.
    CLEAR ls_qpmt_z1.
    CLEAR ls_t006a.
    CLEAR ls_afvc.
    CLEAR ls_qpmt_z2.
    CLEAR ls_qpmt_en.
    CLEAR ls_qmtt_zh.
    CLEAR ls_qmtt_en.
    CLEAR ls_tq11t.
    CLEAR ls_crhd.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM  FRM_CHARG_SPECIAILTY
*&---------------------------------------------------------------------*
*       TEXT：调用FUNCTION 获取批次特性
*----------------------------------------------------------------------*
FORM frm_get_charg_speciailty USING uv_matnr TYPE matnr
      uv_werks LIKE qals-werk
      uv_charg LIKE qals-charg
CHANGING cs_charg TYPE typ_charg
  cs_zpack_sta TYPE atwrt.
  DATA: lv_objek              TYPE cuobn,
        lv_obtab              TYPE tabelle,
        lv_klart              TYPE klassenart,
        lv_class              TYPE klasse_d,
        lv_fltp_to_string(16) TYPE c,
        "批次特性通用返回类型表 num型 char型 curr型
        lt_num                TYPE TABLE OF bapi1003_alloc_values_num,
        lt_char               TYPE TABLE OF bapi1003_alloc_values_char,
        lt_curr               TYPE TABLE OF bapi1003_alloc_values_curr,
        lt_return             TYPE TABLE OF bapiret2.

  CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
    EXPORTING
      i_matnr = uv_matnr
      i_charg = uv_charg
      i_werks = uv_werks
    IMPORTING
      e_objek = lv_objek "对象
      e_obtab = lv_obtab "表
      e_klart = lv_klart "类型
      e_class = lv_class. "类
  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      objectkey_long  = lv_objek
      objecttable     = lv_obtab
      classnum        = lv_class
      classtype       = lv_klart
    TABLES
      allocvaluesnum  = lt_num
      allocvalueschar = lt_char
      allocvaluescurr = lt_curr
      return          = lt_return.

  LOOP AT lt_char ASSIGNING FIELD-SYMBOL(<fs_char>).
    CASE <fs_char>-charact.
      WHEN cns_zchargl."原辅包长批号
        cs_charg-zchargl = <fs_char>-value_neutral_long.
      WHEN cns_z_batch_dypc."成品打印批号
        cs_charg-z_batch_dypc = <fs_char>-value_neutral_long.
      WHEN cns_zpack_sta.
        cs_zpack_sta = <fs_char>-value_neutral_long.
    ENDCASE.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM  FRM_GET_GELATIN_TEST
*&---------------------------------------------------------------------*
*       TEXT：抓取明胶检验结果
*----------------------------------------------------------------------*
FORM frm_get_gelatin_test USING uv_aufnr LIKE qals-aufnr                                 .
  "抓取明胶物料代码
  SELECT zgelatin
  INTO TABLE @DATA(lt_zqmt009)
        FROM zqmt009.

  "抓取需要输出的明胶检验特性编码
  SELECT verwmerkm
  INTO TABLE @DATA(lt_zqmt010)
        FROM zqmt010.

  "添加明胶物料代码RANGE
  DATA lv_str(40) TYPE c. "临时变量用于添加前导0
  LOOP AT lt_zqmt009 INTO DATA(ls_zqmt009).
    def_set_range_matnr ls_zqmt009-zgelatin.
  ENDLOOP.

  "根据S码批次获取明胶批次
  SELECT a~aufnr,a~charg,a~matnr
  INTO TABLE @DATA(lt_mseg)
        FROM mseg AS a
        WHERE a~aufnr EQ @uv_aufnr
        AND a~bwart EQ @cns_bwart
        AND a~matnr IN @gr_matnr_mj.

  IF lt_mseg IS NOT INITIAL.
    "根据明胶批次号获取对应检验批号
    SELECT prueflos,charg,plnnr,plnty
    INTO TABLE @DATA(lt_qals_mj)
          FROM qals
          FOR ALL ENTRIES IN @lt_mseg
          WHERE charg EQ @lt_mseg-charg.

    IF lt_qals_mj IS NOT INITIAL.
      "根据检验批号查询检验结果
      SELECT prueflos,merknr,mbewertg,mittelwert,pruefbemkt,
      vorglfnr
      INTO TABLE @DATA(lt_qamr)
            FROM qamr
            FOR ALL ENTRIES IN @lt_qals_mj
            WHERE prueflos EQ @lt_qals_mj-prueflos.
      SORT lt_qamr BY vorglfnr ASCENDING merknr ASCENDING.

      IF lt_qamr IS NOT INITIAL.
        "抓取检验方法,工厂,检验员资格等信息
        SELECT prueflos,vorglfnr,merknr,pmtversion,
        kurztext,verwmerkm,mkversion,qmtb_werks,
        qpmk_werks,masseinhsw,pmethode
        INTO TABLE @DATA(lt_qamv)
              FROM qamv
              FOR ALL ENTRIES IN @lt_qamr
              WHERE prueflos EQ @lt_qamr-prueflos
              AND vorglfnr EQ @lt_qamr-vorglfnr
              AND merknr   EQ @lt_qamr-merknr.

        SELECT prueflos,vorglfnr,merknr,mittelwert,pruefbemkt,original_input
        INTO TABLE @DATA(lt_qasr)
              FROM qasr
              FOR ALL ENTRIES IN @lt_qamr
              WHERE prueflos EQ @lt_qamr-prueflos
              AND vorglfnr EQ @lt_qamr-vorglfnr
              AND merknr   EQ @lt_qamr-merknr.

        IF lt_qamv IS NOT INITIAL.
          "抓取检验结果单位文本
          SELECT msehi,msehl
          INTO TABLE @DATA(lt_t006a)
                FROM t006a
                FOR ALL ENTRIES IN @lt_qamv
                WHERE msehi = @lt_qamv-masseinhsw
                AND spras = '1'.

          "抓取检验方法版本
          SELECT mkmnr,werks,version,sortfeld
          INTO TABLE @DATA(lt_qpmk)
                FROM qpmk
                FOR ALL ENTRIES IN @lt_qamv
                WHERE mkmnr   EQ @lt_qamv-verwmerkm
                AND version EQ @lt_qamv-mkversion
                AND werks   EQ @lt_qamv-qpmk_werks.

          "抓取检验方法文本
          SELECT pmtnr,werks,version,sprache,kurztext
          INTO TABLE @DATA(lt_qmtt)
                FROM qmtt
                FOR ALL ENTRIES IN @lt_qamv
                WHERE pmtnr   EQ @lt_qamv-pmethode
                AND version EQ @lt_qamv-pmtversion
                AND werks   EQ @lt_qamv-qmtb_werks
                AND sprache IN ( '1','E' ).

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  DATA: lt_line  TYPE TABLE OF tline, "长文本表
        lv_fname TYPE thead-tdname. "长文本对象
  LOOP AT lt_qamr ASSIGNING FIELD-SYMBOL(<fs_qamr>).
    READ TABLE lt_qamv ASSIGNING FIELD-SYMBOL(<fs_qamv>) WITH KEY prueflos = <fs_qamr>-prueflos merknr = <fs_qamr>-merknr vorglfnr = <fs_qamr>-vorglfnr.
    IF sy-subrc = 0.
      READ TABLE lt_qpmk  INTO DATA(ls_qpmk) WITH KEY mkmnr = <fs_qamv>-verwmerkm version = <fs_qamv>-mkversion werks = <fs_qamv>-qpmk_werks.
      READ TABLE lt_qmtt  INTO DATA(ls_qmtt_zh) WITH KEY  pmtnr = <fs_qamv>-pmethode version = <fs_qamv>-pmtversion werks  = <fs_qamv>-qmtb_werks sprache = '1'.
      READ TABLE lt_t006a INTO DATA(ls_t006a) WITH KEY msehi = <fs_qamv>-masseinhsw.
      READ TABLE lt_qasr  INTO DATA(ls_qasr) WITH KEY prueflos = <fs_qamr>-prueflos vorglfnr = <fs_qamr>-vorglfnr merknr = <fs_qamr>-merknr.
      READ TABLE lt_qals_mj INTO DATA(ls_qals_mj) WITH KEY prueflos = <fs_qamr>-prueflos.
      gs_gelatin-prueflos        = <fs_qamr>-prueflos.
      gs_gelatin-verwmerkm       = <fs_qamv>-verwmerkm.
      gs_gelatin-mbewertg        = <fs_qamr>-mbewertg.
      gs_gelatin-original_input  = ls_qasr-original_input.

      CONCATENATE <fs_qamv>-prueflos <fs_qamv>-vorglfnr <fs_qamv>-merknr <fs_qamv>-mkversion INTO lv_fname.
      PERFORM frm_get_longtext USING 'QPRP' '1' lv_fname 'QEEPROBE' CHANGING lt_line.
      IF lt_line IS NOT INITIAL.
        LOOP AT lt_line INTO DATA(ls_line).
          CONCATENATE gs_gelatin-pruefbemkt ls_line-tdline INTO gs_gelatin-pruefbemkt.
        ENDLOOP.
        FREE lt_line.
      ELSE.
        gs_gelatin-pruefbemkt    = ls_qasr-pruefbemkt.
      ENDIF.

      gs_gelatin-mseht           = ls_t006a-msehl.
      IF gs_gelatin-mseht EQ ''.
        gs_gelatin-mseht = '/'.
      ENDIF.
      IF gs_gelatin-verwmerkm NE ''.
        gs_gelatin-sortfeld      = ls_qpmk-sortfeld.

        CONCATENATE sy-mandt <fs_qamv>-qpmk_werks <fs_qamv>-verwmerkm <fs_qamv>-mkversion '1' INTO lv_fname.
        PERFORM frm_get_longtext USING 'QPMT' '1' lv_fname 'QPMERKMAL' CHANGING lt_line.
        IF lt_line IS NOT INITIAL.
          LOOP AT lt_line INTO ls_line.
            CONCATENATE gs_gelatin-qamv_kurztext ls_line-tdline INTO gs_gelatin-qamv_kurztext.
          ENDLOOP.
          FREE lt_line.
        ELSE.
          gs_gelatin-qamv_kurztext  = <fs_qamv>-kurztext.
        ENDIF.

        CONCATENATE sy-mandt <fs_qamv>-qmtb_werks <fs_qamv>-pmethode <fs_qamv>-pmtversion '1' INTO lv_fname.
        PERFORM frm_get_longtext USING 'QMTT' '1' lv_fname 'QPMETHODE' CHANGING lt_line.
        IF lt_line IS NOT INITIAL.
          LOOP AT lt_line INTO ls_line.
            CONCATENATE gs_gelatin-qmtt_kurztext ls_line-tdline INTO gs_gelatin-qmtt_kurztext.
          ENDLOOP.
          FREE lt_line.
        ELSE.
          gs_gelatin-qmtt_kurztext = ls_qmtt_zh-kurztext.
        ENDIF.

        gs_gelatin-charg         = ls_qals_mj-charg.
        READ TABLE lt_zqmt010 TRANSPORTING NO FIELDS WITH KEY verwmerkm = gs_gelatin-verwmerkm.
        IF sy-subrc = 0.
          APPEND gs_gelatin TO gt_gelatin.
        ENDIF.
      ENDIF.
      CLEAR ls_qpmk.
      CLEAR ls_qmtt_zh.
      CLEAR ls_qals_mj.
      CLEAR ls_qasr.
      CLEAR ls_t006a.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM frm_get_longtext USING uv_id uv_langu uv_name uv_objet
CHANGING ct_line TYPE STANDARD TABLE.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = uv_id
      language                = uv_langu
      name                    = uv_name
      object                  = uv_objet
    TABLES
      lines                   = ct_line
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_COMBIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> 获取组合装区域的信息
*&---------------------------------------------------------------------*
FORM frm_get_combin  USING  u_pruef LIKE p_pruef.
  IF u_pruef IS NOT INITIAL AND p_pruef2 IS NOT INITIAL AND p_num IS NOT INITIAL.
    SELECT SINGLE
    prueflos
    charg
    matnr
    kdauf
    INTO (gs_range_combin-prueflos,gs_range_combin-charg,
    gs_range_combin-matnr,gs_range_combin-kdauf)
    FROM qals
    WHERE prueflos = u_pruef.
  ENDIF.
  IF gs_range_combin-matnr IS NOT INITIAL.
    SELECT SINGLE
      rbtxt
    INTO (gs_range_combin-rbtxt)
    FROM mara
    INNER JOIN t142t
    ON t142t~raube = mara-raube
    WHERE mara~matnr = gs_range_combin-matnr
      AND t142t~spras = '1'.
    DATA objek TYPE ausp-objek.
    objek = gs_range_combin-matnr.
    SELECT
      objek,
      atinn,
      atwrt
    INTO TABLE @DATA(lt_ausp_vice)
    FROM ausp
    WHERE objek = @objek.
    READ TABLE lt_ausp_vice INTO DATA(ls_ausp_vice) WITH KEY atinn = 'ZPSPE'.
    IF sy-subrc = 0.
      gs_range_combin-zpspe = ls_ausp_vice-atwrt.
    ENDIF.
    CLEAR ls_ausp_vice.
    READ TABLE lt_ausp_vice INTO ls_ausp_vice WITH KEY atinn = 'ZCUMCODE'.
    IF sy-subrc = 0.
      gs_range_combin-zcumcode = ls_ausp_vice-atwrt.
      CLEAR ls_ausp_vice.
    ENDIF.
  ENDIF.

ENDFORM.