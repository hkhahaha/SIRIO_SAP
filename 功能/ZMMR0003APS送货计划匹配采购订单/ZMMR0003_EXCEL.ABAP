*&---------------------------------------------------------------------*
*& 包含               ZMMR0003_EXCEL
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  frm_get_structure
*&---------------------------------------------------------------------*
*       #####
*----------------------------------------------------------------------*
FORM frm_get_structure USING uv_table TYPE tabname.

    *  ref_table_des ?= cl_abap_typedescr=>describe_by_name( p_name ).
    *  idetails[] = ref_table_des->components[].
    
      SELECT    dd03l~position  AS position
                dd03l~tabname   AS tabname
                dd03l~fieldname AS fieldname
                dd03l~datatype  AS datatype
                dd03l~leng      AS ddleng
                dd03l~decimals  AS decimals
                dd03t~ddtext    AS ddtext
                dd04t~ddtext    AS eddtext
                dd01t~ddtext    AS dddtext
        INTO CORRESPONDING FIELDS OF TABLE gt_outtab
        FROM dd03l
        LEFT JOIN dd03t
          ON dd03l~tabname = dd03t~tabname
         AND dd03l~fieldname = dd03t~fieldname
         AND dd03t~ddlanguage = sy-langu
        LEFT JOIN dd04t
          ON dd03l~rollname = dd04t~rollname
         AND dd04t~ddlanguage = sy-langu
        LEFT JOIN dd01t
          ON dd03l~domname = dd01t~domname
         AND dd01t~ddlanguage = sy-langu
       WHERE dd03l~tabname =  uv_table
         AND dd03l~fieldname NOT LIKE '.I%'
         ORDER BY dd03l~position.
    
      IF gt_outtab[] IS INITIAL.
        MESSAGE '该表不存在' TYPE 'I' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    
      SORT gt_outtab BY position.
      LOOP AT gt_outtab INTO gs_outtab.
        IF gs_outtab-ddtext = space.
          IF gs_outtab-eddtext = space.
            gs_outtab-ddtext = gs_outtab-dddtext.
          ELSE.
            gs_outtab-ddtext = gs_outtab-eddtext.
          ENDIF.
        ENDIF.
        MODIFY gt_outtab FROM gs_outtab.
        CLEAR  gs_outtab.
      ENDLOOP.
    
    ENDFORM. "frm_get_structure
    
    *&---------------------------------------------------------------------*
    *&      Form  frm_save_excel
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *      -->uv_tmp     text
    *----------------------------------------------------------------------*
    FORM frm_save_excel USING uv_tmp.
    
      PERFORM frm_create_excel.
      PERFORM frm_write_data USING uv_tmp.
      PERFORM frm_save.
    
    ENDFORM. "frm_save_excel
    
    
    *&---------------------------------------------------------------------*
    *&    (1)   Form  frm_create_excel
    *&---------------------------------------------------------------------*
    *       ##excel
    *----------------------------------------------------------------------*
    FORM frm_create_excel.
    
      CREATE OBJECT gole_excel 'excel.APPLICATION'.
      IF sy-subrc <> 0.
        MESSAGE 'EXCEL ERROR' TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
      CALL METHOD OF
        gole_excel
          'WORKBOOKS' = gole_book.
      SET PROPERTY OF gole_excel 'VISIBLE' = 1.
      SET PROPERTY OF gole_excel 'SHEETSINNEWWORKBOOK' = 1.
      CALL METHOD OF
        gole_book
          'ADD' = gole_sheet.
      CALL METHOD OF
        gole_sheet
        'ACTIVE'.
    ENDFORM. "frm_create_excel
    
    *&---------------------------------------------------------------------*
    *&      Form  frm_write_data
    *&---------------------------------------------------------------------*
    *       写入EXCEL数据
    *----------------------------------------------------------------------*
    FORM frm_write_data USING uv_tmp.
    
      DATA: lv_row TYPE i,
            lv_col TYPE i,
            ls_alv LIKE gs_alv.
    
    *--##excel###sheet
      lv_col = 1.
      PERFORM frm_open_excel_sheet USING lv_col.
    
      PERFORM frm_write_cell USING 1 1 'APS唯一标识'.
      PERFORM frm_write_cell USING 1 2 '采购订单'.
      PERFORM frm_write_cell USING 1 3 '采购订单行'.
      PERFORM frm_write_cell USING 1 4 '采购数量'.
      PERFORM frm_write_cell USING 1 5 '采购订单单位'.
      PERFORM frm_write_cell USING 1 6 '订单交货日期'.
      PERFORM frm_write_cell USING 1 7 '物料编码'.
      PERFORM frm_write_cell USING 1 8 '物料描述'.
      PERFORM frm_write_cell USING 1 9 '供应商'.
      PERFORM frm_write_cell USING 1 10 '供应商名称'.
      PERFORM frm_write_cell USING 1 11 '数量'.
      PERFORM frm_write_cell USING 1 12 '计量单位'.
      PERFORM frm_write_cell USING 1 13 '计划日期'.
      PERFORM frm_write_cell USING 1 14 '需求日期'.
      PERFORM frm_write_cell USING 1 15 '指定标准'.
      PERFORM frm_write_cell USING 1 16 '版本'.
      PERFORM frm_write_cell USING 1 17 '工厂'.
      PERFORM frm_write_cell USING 1 18 '送货至工厂'.
      PERFORM frm_write_cell USING 1 19 '库存地点'.
      PERFORM frm_write_cell USING 1 20 '送货地点'.
      PERFORM frm_write_cell USING 1 21 '送货厂区'.
      PERFORM frm_write_cell USING 1 22 '确认送货日期'.
      PERFORM frm_write_cell USING 1 23 '确认数量'.
    
    *  LOOP AT gt_outtab INTO gs_outtab FROM 2 TO 9.
    *    IF p_r03 = 'X'.
    *      PERFORM frm_write_cell USING 1 lv_col gs_outtab-fieldname.
    *    ELSEIF p_r04 = 'X'.
    *      PERFORM frm_write_cell USING 1 lv_col gs_outtab-ddtext.
    *    ENDIF.
    *    lv_col = lv_col + 1.
    *  ENDLOOP.
    
      CHECK uv_tmp IS INITIAL.
    
      lv_row = 2.
      LOOP AT gt_temp_export INTO gs_temp_export.
    
        CLEAR gs_alv.
    
        MOVE-CORRESPONDING gs_temp_export TO gs_alv.
    
        gs_alv-vonder = gs_temp_export-lifnr.
    
        SELECT SINGLE ktokk,
                      name1,
                      name2
          INTO @DATA(ls_vonder)
          FROM lfa1
         WHERE lifnr = @gs_temp_export-lifnr.
    
        SELECT SINGLE menge
                      meins
                      matnr
          INTO (gs_alv-menge,gs_alv-bprme,gs_alv-matnr)
          FROM ekpo
         WHERE ebeln = gs_temp_export-ebeln
           AND ebelp = gs_temp_export-ebelp.
    
        SELECT SINGLE eindt
          INTO gs_alv-eindt
          FROM eket
         WHERE ebeln = gs_temp_export-ebeln
           AND ebelp = gs_temp_export-ebelp.
    
        IF ls_vonder-ktokk = 'XL02'.
          SELECT SINGLE maktx INTO gs_alv-maktx FROM makt WHERE matnr = gs_alv-matnr AND spras = 'E'.
          IF sy-subrc <> 0.
            SELECT SINGLE maktx INTO gs_alv-maktx FROM makt WHERE matnr = gs_alv-matnr AND spras = '1'.
          ENDIF.
          SELECT SINGLE name2 INTO gs_alv-werks_name FROM t001w WHERE werks = gs_temp_export-werks.
        ELSE.
          SELECT SINGLE maktx INTO gs_alv-maktx FROM makt WHERE matnr = gs_alv-matnr AND spras = '1'.
          SELECT SINGLE name1 INTO gs_alv-werks_name FROM t001w WHERE werks = gs_temp_export-werks.
        ENDIF.
    
    *送货地点描述
        IF gs_temp_export-lgort IS INITIAL.
          SELECT SINGLE lgort INTO gs_alv-lgort FROM ekpo WHERE ebeln = gs_temp_export-ebeln AND ebelp = gs_temp_export-ebelp.
        ENDIF.
        SELECT SINGLE lgobe INTO gs_alv-lgobe FROM t001l WHERE lgort = gs_temp_export-lgort.
    
        SELECT SINGLE meins INTO gs_alv-meins FROM mara WHERE matnr = gs_alv-matnr.
        SELECT SINGLE name1 INTO gs_alv-vonder_name FROM lfa1 WHERE lifnr = gs_alv-vonder.
    
        WRITE gs_alv-menge TO gs_alv-po_menge DECIMALS 3.
        gs_alv-po_dlvdate = gs_alv-eindt.
    
        PERFORM frm_write_cell USING: lv_row 1 gs_temp_export-guid,
                                      lv_row 2 gs_alv-ebeln,
                                      lv_row 3 gs_alv-ebelp,
                                      lv_row 4 gs_alv-po_menge,
                                      lv_row 5 gs_alv-bprme,
                                      lv_row 6 gs_alv-po_dlvdate,
                                      lv_row 7 gs_alv-matnr,
                                      lv_row 8 gs_alv-maktx,
                                      lv_row 9 gs_alv-vonder,
                                      lv_row 10 gs_alv-vonder_name,
                                      lv_row 11 gs_alv-assign_menge,
                                      lv_row 12 gs_alv-meins,
                                      lv_row 13 gs_alv-plan_date,
                                      lv_row 14 gs_alv-reqdate,
                                      lv_row 15 gs_alv-st_code,
                                      lv_row 16 gs_alv-version,
                                      lv_row 17 gs_alv-werks,
                                      lv_row 18 gs_alv-werks_name,
                                      lv_row 19 gs_alv-lgort,
                                      lv_row 20 gs_alv-lgobe,
                                      lv_row 21 gs_alv-plant_area,
                                      lv_row 22 gs_alv-ack_date,
                                      lv_row 23 gs_alv-ack_menge.
        lv_row = lv_row + 1.
      ENDLOOP.
    
    ENDFORM. "frm_write_data
    
    *&---------------------------------------------------------------------*
    *& Form FRM_OPEN_EXCEL_SHEET
    *&---------------------------------------------------------------------*
    * -->P_gole_SHEET_NUMBER sheet number
    *----------------------------------------------------------------------*
    FORM frm_open_excel_sheet USING pa_sheet_number.
      SET PROPERTY OF gole_excel 'Visible' = 1.
      CALL METHOD OF
      gole_book
      'Sheets' = gole_sheet
      EXPORTING
        #1       = pa_sheet_number.
    ENDFORM. " FRM_OPEN_EXCEL_SHEET
    
    *&---------------------------------------------------------------------*
    *& Form FRM_WRITE_CELL
    *&---------------------------------------------------------------------*
    * -->P_L_ROW number of row
    * -->P_L_COL number of collum
    * -->P_L_VAL cell value
    *----------------------------------------------------------------------*
    FORM frm_write_cell USING pa_row
                                pa_col
                                pa_val.
      CALL METHOD OF gole_excel
        'Cells' = gole_cell
      EXPORTING
        #1      = pa_row
        #2      = pa_col.
      SET PROPERTY OF gole_cell 'Value' = pa_val.
    ENDFORM. " FRM_WRITE_CELL
    
    *&---------------------------------------------------------------------*
    *&      Form  frm_save
    *&---------------------------------------------------------------------*
    *       text
    *----------------------------------------------------------------------*
    *      -->P_DOWN     text
    *----------------------------------------------------------------------*
    FORM frm_save.
      SET PROPERTY OF gole_excel 'DisplayAlerts' = 0.
    
    *****##WORKSHEET****
    *  CALL METHOD OF gole_sheet 'CLOSE'.
    *****##WORKBOOK
    *  CALL METHOD OF gole_book 'CLOSE'.
    *****##EXCEL##****
    **  CALL METHOD OF gole_excel 'QUIT'.
    
      FREE OBJECT gole_cell.
      FREE OBJECT gole_sheet.
      FREE OBJECT gole_book.
      FREE OBJECT gole_excel.
    ENDFORM. "frm_save
    *&---------------------------------------------------------------------*
    *& Form FRM_CHACK_DAYS
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *&      --> <FS_ALV>_PO_DLVDATE
    *&      --> LV_DATUM
    *&---------------------------------------------------------------------*
    FORM frm_chack_days  USING    p_day1
                                  p_day2 TYPE sy-datum .
    
      CLEAR p_day2 .
      CALL FUNCTION 'Z_MM_CHANGE_DAY'
        EXPORTING
          i_input  = p_day1
        IMPORTING
          e_output = p_day2.
    
    ENDFORM.