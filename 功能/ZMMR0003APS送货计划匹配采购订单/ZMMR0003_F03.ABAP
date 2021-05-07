*&---------------------------------------------------------------------*
*& 包含               ZMMR0003_F03
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form FRM_GET_EXCEL_F4
*&---------------------------------------------------------------------*
*& 选择文件
*&---------------------------------------------------------------------*
*&      <-- cv_file
*&---------------------------------------------------------------------*
FORM frm_get_excel_f4  CHANGING cv_file.

    DATA: lv_mask(50).
  
    lv_mask = ',Excel,*.XLSX;*.XLS.;*.XLSM'.
  
    CALL FUNCTION 'WS_FILENAME_GET'
      EXPORTING
        def_path         = 'C:\'
        mask             = lv_mask
        title            = 'EXCEL文件'
      IMPORTING
        filename         = cv_file
      EXCEPTIONS
        inv_winsys       = 1
        no_batch         = 2
        selection_cancel = 3
        selection_error  = 4
        OTHERS           = 5.
  
    IF sy-subrc <> 0 AND sy-subrc <> 3.
      MESSAGE '选择文件错误' TYPE 'I' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_EXCEL_DATA
  *&---------------------------------------------------------------------*
  *& 获取EXCEL数据
  *&---------------------------------------------------------------------*
  FORM frm_get_excel_data .
  
    PERFORM frm_update_excel.              "获取excel数据
    PERFORM frm_get_excel_data_from_table. "把EXCEL数据转换成内表
    PERFORM frm_process_excel_data.        "处理上传的EXCEL数据
  
    IF gt_alv[] IS INITIAL.
      PERFORM frm_stop_program USING '没有找到EXCEL数据，或者EXCEL里面数据您无权限上传' .
    ENDIF.
  
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_DOWNLOAD_EXECL_TMP
  *&---------------------------------------------------------------------*
  *& 下载上传模板
  *&---------------------------------------------------------------------*
  *&      --> uv_ucomm
  *&---------------------------------------------------------------------*
  FORM frm_download_execl_tmp USING uv_ucomm.
  
  *  CALL SELECTION-SCREEN 0300 STARTING AT 10 5.
  *  IF sy-subrc EQ 0.
  *    PERFORM frm_get_structure USING 'ZMMAPS_TEMP'.
    PERFORM frm_save_excel USING 'X'.
  *  ENDIF.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *&      Form  frm_update_excel
  *&---------------------------------------------------------------------*
  *       获取excel数据
  *----------------------------------------------------------------------*
  FORM frm_update_excel. "##excel##
  
    PERFORM frm_show_bottom_message USING 'Loading Data.....'.
    PERFORM frm_get_excel_data_to_table TABLES gt_excel  USING 1 2 23 15000.
  
  ENDFORM. "frm_update_excel
  
  *&---------------------------------------------------------------------*
  *&      Form  frm_get_excel_data_to_table
  *&---------------------------------------------------------------------*
  *       ##excel######
  *----------------------------------------------------------------------*
  FORM frm_get_excel_data_to_table TABLES it_excel LIKE gt_excel
                                    USING uv_begin_col uv_begin_row uv_end_col uv_end_row.
  
    CLEAR:it_excel[].
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_file
        i_begin_col             = uv_begin_col              "EXCEL######
        i_begin_row             = uv_begin_row              "EXCEL######
        i_end_col               = uv_end_col                "EXCEL######
        i_end_row               = uv_end_row                "EXCEL######
      TABLES
        intern                  = it_excel
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE 'Excel read data failed , please check excel' TYPE 'I' DISPLAY LIKE 'E'.
      STOP.
    ELSEIF it_excel[] IS INITIAL.
      MESSAGE 'Excel data is empty, please check' TYPE 'I' DISPLAY LIKE 'E'.
      STOP.
    ELSE.
      SORT it_excel BY row col.                       "######
    ENDIF.
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *&      Form  frm_show_bottom_message
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  *      -->uv_text       text
  *----------------------------------------------------------------------*
  FORM frm_show_bottom_message USING uv_text.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 0
        text       = uv_text
      EXCEPTIONS
        OTHERS     = 1.
  ENDFORM. "frm_show_bottom_message
  *&---------------------------------------------------------------------*
  *&      Form  frm_get_excel_data_from_table
  *&---------------------------------------------------------------------*
  *       把EXCEL数据转换成内表
  *----------------------------------------------------------------------*
  FORM frm_get_excel_data_from_table. "把EXCEL数据转换成内表
  
    FIELD-SYMBOLS:<fv_com>   TYPE any,
                  <ls_excel> LIKE gs_excel,
                  <ls_alv>   LIKE gs_alv.
  
    DATA: ls_alv  LIKE gs_alv.
  
    LOOP AT gt_excel ASSIGNING <ls_excel>.
  
      AT NEW row.
        UNASSIGN <ls_alv>.
        APPEND INITIAL LINE TO gt_alv ASSIGNING <ls_alv>.
      ENDAT.
  
      ASSIGN COMPONENT <ls_excel>-col OF STRUCTURE <ls_alv> TO <fv_com>.
  *    IF <ls_excel>-col = 6 OR <ls_excel>-col = 22 .
  *      REPLACE ALL OCCURRENCES OF '.' IN <ls_excel>-value WITH space.
  *      REPLACE ALL OCCURRENCES OF '/' IN <ls_excel>-value WITH space.
  *      REPLACE ALL OCCURRENCES OF '-' IN <ls_excel>-value WITH space.
  *      CONDENSE  <ls_excel>-value NO-GAPS.
  *    ENDIF.
      <fv_com> = <ls_excel>-value.
  
    ENDLOOP.
  
  ENDFORM. "frm_get_excel_data_from_table
  
  *&---------------------------------------------------------------------*
  *& Form FRM_PROCESS_EXCEL_DATA
  *&---------------------------------------------------------------------*
  *& 处理上传的EXCEL数据
  *&---------------------------------------------------------------------*
  FORM frm_process_excel_data .
  
    DATA: lv_success,
          lv_date    LIKE sy-datum,
          ls_temp    LIKE zmmaps_temp.
  
    DEFINE set_message.
      <fs_alv>-light = &1.
      <fs_alv>-message = &2.
      CONTINUE.
    END-OF-DEFINITION.
  
    DATA : lv_datum TYPE sy-datum .
    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).
  
      DATA(lv_tabix) = sy-tabix.
  
      IF <fs_alv>-po_dlvdate <> '' .
        PERFORM frm_chack_days USING <fs_alv>-po_dlvdate lv_datum .
        IF lv_datum IS NOT INITIAL .
          <fs_alv>-po_dlvdate = lv_datum .
        ELSE .
          set_message icon_led_red '订单交货日期为非法日期'.
        ENDIF.
      ENDIF .
  
      IF <fs_alv>-plan_date <> '' .
        PERFORM frm_chack_days USING <fs_alv>-plan_date  lv_datum .
        IF lv_datum IS NOT INITIAL .
          <fs_alv>-plan_date  = lv_datum .
        ELSE .
          set_message icon_led_red '计划日期为非法日期'.
        ENDIF.
      ENDIF .
  
      IF <fs_alv>-reqdate <> '' .
        PERFORM frm_chack_days USING <fs_alv>-reqdate   lv_datum .
        IF lv_datum IS NOT INITIAL .
          <fs_alv>-reqdate   = lv_datum .
        ELSE .
          set_message icon_led_red '日期为非法日期'.
        ENDIF.
      ENDIF .
  
      PERFORM frm_chack_days USING <fs_alv>-ack_date  lv_datum .
      IF lv_datum IS NOT INITIAL .
        <fs_alv>-ack_date  = lv_datum .
      ELSE .
        set_message icon_led_red '确认送货日期为非法日期'.
      ENDIF.
  
      "数字千分号处理
  *         po_menge(20),      "采购数量
  *         assign_menge(20),  "匹配数量
  *         ack_menge(20),     "确认数量
      REPLACE ',' WITH '' INTO <fs_alv>-po_menge .
      CONDENSE <fs_alv>-po_menge  NO-GAPS .
      REPLACE ',' WITH '' INTO <fs_alv>-assign_menge .
      CONDENSE <fs_alv>-assign_menge  NO-GAPS .
      REPLACE ',' WITH '' INTO <fs_alv>-ack_menge .
      CONDENSE <fs_alv>-ack_menge  NO-GAPS .
  
      IF <fs_alv>-ack_date IS INITIAL.
        set_message icon_led_red '确认送货日期不能为空'.
      ELSE.
        IF <fs_alv>-ack_date < sy-datum.
          set_message icon_led_red '确认送货日期不能小于当前日期'.
        ENDIF.
      ENDIF.
  
      IF <fs_alv>-ack_menge IS INITIAL.
        set_message icon_led_red '确认数量不能为空'.
      ENDIF.
  
      IF <fs_alv>-ack_menge < 0.
        set_message icon_led_red '确认数量不能为负数'.
      ENDIF.
  
      IF <fs_alv>-po_menge < 0.
        set_message icon_led_red '采购数量不能为负数'.
      ENDIF.
  
      IF <fs_alv>-assign_menge < 0.
        set_message icon_led_red '匹配数量不能为负数'.
      ENDIF.
  
      MOVE-CORRESPONDING <fs_alv> TO ls_temp.
      <fs_alv>-ebel1 = <fs_alv>-ebeln .
      <fs_alv>-ebep1 = <fs_alv>-ebelp .
      PERFORM frm_buling CHANGING <fs_alv>-matnr.
      PERFORM frm_buling CHANGING <fs_alv>-ebeln.
      PERFORM frm_buling CHANGING <fs_alv>-ebelp.
      PERFORM frm_buling CHANGING <fs_alv>-ebel1.
      PERFORM frm_buling CHANGING <fs_alv>-ebep1.
  *采购组
      SELECT SINGLE ekgrp
        INTO @DATA(lv_ekgrp)
        FROM marc
       WHERE matnr = @<fs_alv>-matnr
         AND werks = @ls_temp-werks.
  
  *权限检查
      AUTHORITY-CHECK OBJECT 'ZMM020'
       ID 'WERKS' FIELD <fs_alv>-werks
       ID 'EKGRP' FIELD lv_ekgrp.
      IF sy-subrc <> 0.
        set_message icon_led_red '您没有处理该采购组的数据，请检查'.
      ENDIF.
  
  *    READ TABLE gt_marc INTO gs_marc WITH KEY matnr = gs_pdlv_data-matnr
  *                                             werks = gs_pdlv_data-werks
  *                                             BINARY SEARCH.
  *    IF sy-subrc = 0.
  *      PERFORM frm_check_m_best_ekg1 USING lv_ekgrp lv_success.
  *      IF lv_success = 'E'.
  *        set_message icon_led_red '您没有处理该采购组的数据，请检查'.
  *      ENDIF.
  *    ENDIF.
  
  *数据是否存在检查
      SELECT SINGLE version, guid
        INTO @DATA(ls_guid)
        FROM zmmaps_temp
       WHERE guid = @ls_temp-guid
  *       and werks = @ls_temp-werks
  *       AND lgort = @ls_temp-lgort
  *       AND version = @ls_temp-version
  *       AND reqdate = @ls_temp-reqdate
  *       AND matnr = @<fs_alv>-matnr
  *       AND st_code = @ls_temp-st_code
         AND ebeln = @<fs_alv>-ebeln
         AND ebelp = @<fs_alv>-ebelp
         AND inactive = ''
         AND released = ''
        .
  
      IF sy-subrc <> 0.
        set_message icon_led_red '该条记录不存在或者状态有误，请检查'.
      ENDIF.
  
      SELECT SINGLE remain_days
        FROM zmmaps_pl
       WHERE guid = @ls_guid-guid
        INTO @DATA(lv_remain_days).
  
      PERFORM frm_get_date_add_days USING ls_guid-version lv_remain_days CHANGING lv_date.
  
      IF lv_date < <fs_alv>-ack_date.
        set_message icon_led_red '计划日期超过要求送货截止日期'.
      ENDIF.
  
      SELECT SINGLE menge, uebto INTO @DATA(ls_ekpo) FROM ekpo WHERE ebeln = @<fs_alv>-ebeln AND ebelp = @<fs_alv>-ebelp.
      DATA(lv_menge) = ls_ekpo-menge * ( 1 + ls_ekpo-uebto ).
      IF lv_menge < <fs_alv>-ack_menge.
        set_message icon_led_red '数量不能超过采购订单允许的容差数量'.
      ENDIF.
  
    ENDLOOP.
  
    PERFORM sub_check_kefen ."检查可分配的数量
  
  ENDFORM.
  
  *&--------------------------------------------------------------------*
  *&      Form  frm_buling
  *&--------------------------------------------------------------------*
  *       字符串自动补零
  *       限制#件：输入的字段类型必须为字符型或者n型
  *---------------------------------------------------------------------*
  *      -->INPUT_DATA   输入的字符
  *      -->OUTPUT_DATA  补0后的字符
  *---------------------------------------------------------------------*
  FORM frm_buling CHANGING input_data.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = input_data
      IMPORTING
        output = input_data.
  ENDFORM. "frm_buling
  
  *&---------------------------------------------------------------------*
  *& Form FRM_COMMIT_TMP_DATA
  *&---------------------------------------------------------------------*
  *& 提交临时数据
  *&---------------------------------------------------------------------*
  FORM frm_commit_tmp_data .
  
    DATA: lv_answer,
          lt_temp   LIKE TABLE OF zmmaps_temp,
          ls_temp   LIKE zmmaps_temp,
          ls_alv    LIKE gs_alv.
  
    DATA: lt_return TYPE ztmm_change_aps_plan_return.
  
    READ TABLE gt_alv TRANSPORTING NO FIELDS WITH KEY light = icon_led_red.
    IF sy-subrc = 0.
      MESSAGE '数据还有错误，请修正后再上传处理!' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  
    READ TABLE gt_alv TRANSPORTING NO FIELDS WITH KEY light = icon_led_green.
    IF sy-subrc = 0.
      MESSAGE '数据已经处理过，请重新上传处理!' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  
    PERFORM frm_showconfirm USING '保存确认?' '导入供应商确认后的匹配计划' '确认' '' 2 CHANGING lv_answer.
    CHECK lv_answer = '1'.
  
    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).
  
      MOVE-CORRESPONDING <fs_alv> TO ls_temp.
  
      UPDATE zmmaps_temp
         SET pdlv_date = <fs_alv>-ack_date
             assign_menge = <fs_alv>-ack_menge
       WHERE guid = <fs_alv>-guid
  *       and werks = ls_temp-werks
  *       AND lgort = ls_temp-lgort
  *       AND version = ls_temp-version
  *       AND reqdate = ls_temp-reqdate
  *       AND matnr = <fs_alv>-matnr
  *       AND st_code = ls_temp-st_code
         AND ebeln = ls_temp-ebeln
         AND ebelp = ls_temp-ebelp
         .
      IF sy-subrc = 0.
        <fs_alv>-light = icon_led_green.
        <fs_alv>-message = '确认成功'.
      ENDIF.
    ENDLOOP.
  
    COMMIT WORK.
  *  SELECT *
  *    INTO TABLE lt_zmmaps_pol2
  *    FROM zmmaps_pol
  *     FOR ALL ENTRIES IN gt_alv
  *   WHERE ebeln     = gt_alv-ebeln
  *     AND ebelp     = gt_alv-ebelp
  *     AND pdlv_date = gt_alv-reqdate
  *     AND zaps_sts <> 'C'
  *    .
  *
  *  SORT gt_alv BY lifnr.
  *
  *  LOOP AT gt_alv INTO gs_alv.
  *
  *    ls_alv = gs_alv.
  *
  *    MOVE-CORRESPONDING ls_alv TO ls_temp.
  *    APPEND ls_temp TO lt_temp.
  *
  *    AT NEW lifnr.
  *      CLEAR: ls_zmmaps_pol, ls_zmmaps_pol.
  *
  *      READ TABLE lt_zmmaps_pol2 INTO ls_zmmaps_pol2 WITH KEY ebeln     = ls_alv-ebeln
  *                                                             ebelp     = ls_alv-ebelp
  *                                                             pdlv_date = ls_alv-reqdate.
  *      IF sy-subrc = 0.
  *        ls_zmmaps_poh-dlv_planno = ls_zmmaps_pol2-dlv_planno.
  *      ELSE.
  **获取主键
  *        PERFORM frm_get_num_key CHANGING ls_zmmaps_poh-dlv_planno.
  *        ls_zmmaps_poh-lifnr = ls_alv-lifnr.
  *        ls_zmmaps_poh-ernam = sy-uname.
  *        APPEND ls_zmmaps_poh TO lt_zmmaps_poh.
  *      ENDIF.
  *
  *    ENDAT.
  *
  *    MOVE-CORRESPONDING ls_alv TO ls_zmmaps_pol.
  *
  *    ls_zmmaps_pol-dlv_planno = ls_zmmaps_poh-dlv_planno.
  *
  *    READ TABLE lt_zmmaps_pol2 INTO ls_zmmaps_pol2 WITH KEY ebeln     = ls_alv-ebeln
  *                                                           ebelp     = ls_alv-ebelp
  *                                                           pdlv_date = ls_alv-reqdate.
  *    IF sy-subrc = 0.
  *      ls_zmmaps_pol = ls_zmmaps_pol2.
  *      ls_zmmaps_pol-pdlv_menge = ls_zmmaps_pol-pdlv_menge + ls_alv-assign_menge.
  *    ELSE.
  *      ls_zmmaps_pol-dlv_itemno = ls_zmmaps_pol-dlv_itemno + 10.
  *      ls_zmmaps_pol-pdlv_date = ls_alv-reqdate.
  *      ls_zmmaps_pol-pdlv_menge = ls_alv-assign_menge.
  *      ls_zmmaps_pol-zaps_sts = 'M'.
  *    ENDIF.
  *
  *    APPEND ls_zmmaps_pol TO lt_zmmaps_pol.
  *
  *    gs_alv-dlv_planno = ls_zmmaps_pol-dlv_planno.
  *    gs_alv-dlv_itemno = ls_zmmaps_pol-dlv_itemno.
  *    MODIFY gt_alv FROM gs_alv.
  *    CLEAR: gs_alv, ls_zmmaps_pol.
  *
  *  ENDLOOP.
  *
  *  IF lt_zmmaps_pol[] IS NOT INITIAL.
  *
  **创建/修改APS计划的同时，发送计划数据到WMS
  *    CALL FUNCTION 'Z_MM_CHANGE_APS_PLAN'
  *      EXPORTING
  *        iv_type   = '3'
  *      TABLES
  *        it_poh    = lt_zmmaps_poh
  *        it_pol    = lt_zmmaps_pol
  *        it_return = lt_return.
  *
  *    READ TABLE lt_return INTO DATA(ls_return) WITH KEY success = 'E'.
  *    IF sy-subrc <> 0.
  *
  *      IF lt_zmmaps_poh[] IS NOT INITIAL.
  *        MODIFY zmmaps_poh FROM TABLE lt_zmmaps_poh[].
  *      ENDIF.
  *      MODIFY zmmaps_pol FROM TABLE lt_zmmaps_pol[].
  *      IF sy-subrc = 0.
  *        DELETE zmmaps_temp FROM TABLE lt_temp[].
  *      ENDIF.
  *
  *      MESSAGE '提交成功' TYPE 'S'.
  *
  *      PERFORM frm_leave_screen.
  *
  *    ELSE.
  *
  *      LOOP AT gt_alv INTO gs_alv.
  *        READ TABLE lt_return INTO ls_return WITH KEY dlv_planno = gs_alv-dlv_planno.
  *        IF sy-subrc = 0.
  *          gs_alv-message = ls_return-message.
  *          IF ls_return-success = 'S'.
  *            gs_alv-light = icon_led_green.
  *          ELSE.
  *            gs_alv-light = icon_led_red.
  *          ENDIF.
  *        ENDIF.
  *        MODIFY gt_alv FROM gs_alv.
  *        CLEAR  gs_alv.
  *      ENDLOOP.
  *    ENDIF.
  *
  *  ENDIF.
  
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_NUM_KEY
  *&---------------------------------------------------------------------*
  *& 获取主键
  *&---------------------------------------------------------------------*
  *&      <-- CV_NUM
  *&---------------------------------------------------------------------*
  FORM frm_get_num_key  CHANGING cv_num.
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = 'ZDLV_PL'
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.
  *  **如果号码范围存在
    IF sy-subrc EQ 0 .
  *  ****得到一个号码，
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZDLV_PL'
        IMPORTING
          number                  = cv_num
  *       quantity                = quant
  *       returncode              = code
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
  *  ***将号码累加
      CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
        EXPORTING
          object           = 'ZDLV_PL'
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
    ELSE .
      RAISE num_range_error .
    ENDIF .
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form SUB_CHECK_KEFEN
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM sub_check_kefen .
    DATA : lv_meng2 TYPE ekpo-menge .
  
  
    "已分配 = aps temp总分配 + PO总收货 + 期初pol - zmatdoc总收货 + POL多收
    PERFORM sub_get_ekpo ."获取po信息
    PERFORM sub_get_ekbe ."获取PO总的收货
    PERFORM sub_get_pol ."获取po对应pol
    PERFORM sub_get_zmatdoc ."获取po对应pol的收货数
    PERFORM sub_get_zmmaps_temp ."获取aps temp总分配
  
  
    FIELD-SYMBOLS <ls_alv> TYPE ty_excel_alv .
    DATA : ls_alv TYPE ty_excel_alv .
  
    LOOP AT gt_alv ASSIGNING <ls_alv> WHERE light = '' .
  
      CLEAR gs_ekpo .
      READ TABLE gt_ekpo INTO gs_ekpo WITH KEY ebeln = <ls_alv>-ebel1
                                               ebelp = <ls_alv>-ebep1 .
  
      DATA(lv_menge) = gs_ekpo-menge * ( 100 + gs_ekpo-uebto ) / 100."订单行允许的数量
  
  *         ASSIGN_MENGE(20),  "匹配数量
  *         ACK_MENGE(20),     "确认数量
      CLEAR lv_meng2 .
  *    LV_MENG2 = LV_MENG2 + <ls_alv>-ASSIGN_MENGE .
  
  
      "aps POL总分配
      CLEAR gs_zmmaps_temp .
      LOOP AT gt_zmmaps_temp INTO gs_zmmaps_temp WHERE ebeln = gs_ekpo-ebeln
                                                   AND ebelp = gs_ekpo-ebelp .
  
        CLEAR ls_alv .
        READ TABLE gt_alv INTO ls_alv WITH KEY guid = gs_zmmaps_temp-guid
                                               ebeln = gs_ekpo-ebeln
                                               ebelp = gs_ekpo-ebelp  .
        IF sy-subrc = 0 ."如果是本次alv上的数据 优先取
          lv_meng2 = lv_meng2 + ls_alv-ack_menge .
        ELSE .
          lv_meng2 = lv_meng2 + gs_zmmaps_temp-assign_menge .
        ENDIF .
  
      ENDLOOP .
  
      "PO总收货
      CLEAR gs_ekbe1 .
      READ TABLE gt_ekbe1 INTO gs_ekbe1 WITH KEY ebeln = gs_ekpo-ebeln
                                               ebelp = gs_ekpo-ebelp .
      lv_meng2 = lv_meng2 + gs_ekbe1-menge .
  
      "zmatdoc总收货
      CLEAR gs_zmatdoc1 .
      LOOP AT gt_zmatdoc1 INTO gs_zmatdoc1 WHERE  ebeln = gs_ekpo-ebeln
                                                  AND ebelp = gs_ekpo-ebelp .
  
        IF gs_zmatdoc1-shkzg = 'S' .
          lv_meng2 = lv_meng2 - gs_zmatdoc1-menge .
        ELSE .
          lv_meng2 = lv_meng2 + gs_zmatdoc1-menge .
        ENDIF .
      ENDLOOP .
  
  
      " POL多收  "期初pol
      CLEAR gs_zmmwms_pol .
      LOOP AT gt_zmmwms_pol INTO gs_zmmwms_pol WHERE  ebeln = gs_ekpo-ebeln
                                                  AND ebelp = gs_ekpo-ebelp .
  
        "期初pol
        IF gs_zmmwms_pol-origin = 'M' ."期初数据
          "期初数据要加入占用
          lv_meng2 = lv_meng2 + gs_zmmwms_pol-pdlv_menge .
        ENDIF .
  
        " POL多收
        CLEAR gs_zmatdoc1 .
        LOOP AT gt_zmatdoc1 INTO gs_zmatdoc1 WHERE dlv_planno = gs_zmmwms_pol-dlv_planno
                                             AND dlv_itemno = gs_zmmwms_pol-dlv_itemno .
  
          IF gs_zmatdoc1-shkzg = 'S' .
            gs_zmmwms_pol-pdlv_menge = gs_zmmwms_pol-pdlv_menge - gs_zmatdoc1-menge .
          ELSE .
            gs_zmmwms_pol-pdlv_menge = gs_zmmwms_pol-pdlv_menge + gs_zmatdoc1-menge .
          ENDIF .
        ENDLOOP .
        IF gs_zmmwms_pol-pdlv_menge < 0 ."计划数量小于收货数量 这部分为pol多收
          lv_meng2 = lv_meng2 - gs_zmmwms_pol-pdlv_menge .
        ENDIF .
  
      ENDLOOP .
  
  
      IF lv_menge < lv_meng2 .
        <ls_alv>-light = icon_led_red.
        <ls_alv>-message = '分配数量超过采购订单允许的容差数量'.
      ENDIF .
  
    ENDLOOP  .
  
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form SUB_GET_EKBE
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM sub_get_ekbe .
    CLEAR gt_ekbe1 .
  
    SELECT
    b~ebeln,
    b~ebelp,
  *  zekkn
  *  vgabe
  *  gjahr
  *  belnr
  *  buzei
  *  bwart
   SUM( CASE a~shkzg WHEN 'S' THEN a~menge WHEN 'H' THEN 0 - a~menge END ) AS menge
  *  shkzg
    FROM @gt_ekpo AS b
    JOIN ekbe AS a
       ON a~ebeln = b~ebeln
      AND a~ebelp = b~ebelp
      AND a~bwart <> ''
             GROUP BY b~ebeln,b~ebelp
             ORDER BY b~ebeln,b~ebelp
   INTO TABLE @gt_ekbe1.
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form SUB_GET_POL
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM sub_get_pol .
  
    CLEAR gt_zmmwms_pol ." ZMMWMS_POL .
    SELECT *
      INTO TABLE gt_zmmwms_pol
      FROM zmmwms_pol
  *    WHERE ORIGIN = 'M' "期初
      FOR ALL ENTRIES IN gt_ekpo
      WHERE ebeln = gt_ekpo-ebeln
        AND ebelp = gt_ekpo-ebelp
        AND pdlv_menge > 0
        AND xclosed = '' ."未关闭
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form SUB_GET_ZMATDOC
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM sub_get_zmatdoc .
  
    CLEAR gt_zmatdoc1 .
    SELECT *
      INTO TABLE gt_zmatdoc1
      FROM zmatdoc
      FOR ALL ENTRIES IN gt_ekpo
      WHERE ebeln =  gt_ekpo-ebeln
    AND ebelp =  gt_ekpo-ebelp
     AND shkzg IN ('S','H')
     AND EXISTS ( SELECT *
                          FROM zmmwms_pol
                          WHERE dlv_planno  = zmatdoc~dlv_planno
                            AND dlv_itemno  = zmatdoc~dlv_itemno
                            AND  xclosed = ''  ).
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form SUB_GET_ZMMAPS_TEMP
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM sub_get_zmmaps_temp .
  
    CLEAR gt_zmmaps_temp .
    SELECT *
      INTO TABLE gt_zmmaps_temp
      FROM zmmaps_temp
      FOR ALL ENTRIES IN gt_ekpo
      WHERE ebeln =  gt_ekpo-ebeln
    AND ebelp =  gt_ekpo-ebelp
    AND inactive = ''.
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form SUB_GET_EKPO
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM sub_get_ekpo .
  
    DATA : lt_alv TYPE TABLE OF ty_excel_alv .
  
    lt_alv[] = gt_alv[] .
    DELETE lt_alv WHERE light <>  '' .
    SORT lt_alv BY ebeln ebelp .
    DELETE ADJACENT DUPLICATES FROM lt_alv
                         COMPARING ebeln ebelp .
  
    CLEAR gt_ekpo .
    SELECT
      ebeln
      ebelp
      menge
      uebto
      INTO TABLE gt_ekpo
      FROM ekpo
      FOR ALL ENTRIES IN lt_alv
      WHERE ebeln =  lt_alv-ebel1
        AND ebelp =  lt_alv-ebep1 .
  
  ENDFORM.