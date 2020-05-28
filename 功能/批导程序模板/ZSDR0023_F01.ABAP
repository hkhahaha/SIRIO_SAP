*&---------------------------------------------------------------------*
*& 包含               ZSDR0023_F01
*&---------------------------------------------------------------------*
*选择文件
FORM frm_file_input .

    DATA:lt_tab         TYPE filetable,         "存放文件名的内表
         lv_file_filter TYPE string,            "file filter
         lv_rc          TYPE i.                 "函数返回值
  
    REFRESH:lt_tab.
    CLEAR:lv_file_filter,lv_rc.
  
    lv_file_filter = 'EXCEL文件|*.XLSX;*.XLS'.
  
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = '选择数据文件'
        file_filter             = lv_file_filter
  *     initial_directory       = 'C:\'
      CHANGING
        file_table              = lt_tab
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.
  
    IF sy-subrc = 0 AND lv_rc = 1.
      READ TABLE lt_tab INTO p_file INDEX 1.
    ELSE.
      MESSAGE '没有选择文件或是选择文件错误！' TYPE 'S'.
    ENDIF.
  
  ENDFORM.
  
  *下载导入模板
  FORM frm_downdload_template .
  
    DATA: w_objdata     LIKE wwwdatatab,
          w_mime        LIKE w3mime,
          w_filename    TYPE string,
          w_fullpath    TYPE string  VALUE 'C:/TEMP/',
          w_path        TYPE string VALUE 'C:/TEMP/',
          w_destination LIKE rlgrap-filename,
          w_objnam      TYPE string,
          w_rc          LIKE sy-subrc,
          w_errtxt      TYPE string,
          w_action      TYPE i.
    DATA:w_objid TYPE wwwdatatab-objid,
         w_dest  LIKE sapb-sappfad.
  
    w_objid = 'ZSDR0023'.
    w_filename  ='产品资质-印刷品关联表批量导入模板'.
  
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title         = '下载导入模板'
        default_extension    = 'XLS'
        default_file_name    = w_filename
      CHANGING
        filename             = w_filename
        path                 = w_path
        fullpath             = w_fullpath
        user_action          = w_action
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.
  
    IF sy-subrc = 0 AND w_action EQ 0.
      w_dest = w_fullpath.
      CONDENSE w_objnam NO-GAPS.
      SELECT SINGLE relid objid
        INTO CORRESPONDING FIELDS OF w_objdata
        FROM wwwdata
        WHERE srtf2 = 0 AND relid = 'MI' AND objid = w_objid.
      IF sy-subrc NE 0 OR w_objdata-objid EQ space.
        CONCATENATE '模板文件' w_objnam '不存在' INTO w_errtxt.
        MESSAGE w_errtxt TYPE 'I'.
      ENDIF.
      w_destination = w_dest.
      CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
        EXPORTING
          key         = w_objdata
          destination = w_destination
        IMPORTING
          rc          = w_rc.
      IF w_rc NE 0.
        CONCATENATE '模板文件：' w_objnam '下载失败' INTO w_errtxt.
        MESSAGE w_errtxt TYPE 'E'.
      ENDIF.
    ENDIF.
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_UPLOAD_DATA
  *&---------------------------------------------------------------------*
  *& text 上传数据到内表
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_upload_data .
  
    CLEAR: gt_alv[], gt_upload[].
  
    CALL FUNCTION 'Z_FI_EXCEL_TO_TABLE'
      EXPORTING
        i_filename              = p_file
        i_begin_col             = 1
        i_begin_row             = 2
      TABLES
        t_data                  = gt_upload
      EXCEPTIONS
        inconsistent_parameters = 1
        exit_error              = 2
        conversion_error        = 3
        OTHERS                  = 4.
  
    IF sy-subrc NE 0 OR gt_upload[] IS INITIAL.
      MESSAGE '模板上传失败，请重试！' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  
    gt_alv[] = CORRESPONDING #( gt_upload ).
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_CHECK_DATA
  *&---------------------------------------------------------------------*
  *& text 检查数据
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_check_data .
    DATA: ls_zsdt0013 TYPE zsdt0013.
    SELECT matnr maktx
      INTO TABLE lt_makt
       FROM makt
      WHERE spras = '1'.
  
    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).
      IF <fs_alv>-matnr IS INITIAL.
        <fs_alv>-light = gc_red.
        <fs_alv>-msg = '信息不全！'.
        <fs_alv>-error = 'X'.
      ELSE.
        DATA(lv_matnr) = zcl_bc_public=>conv_by_ddic( <fs_alv>-matnr ).
        READ TABLE lt_makt INTO ls_makt WITH KEY matnr = lv_matnr.
        IF sy-subrc <> 0 .
          <fs_alv>-light = gc_red.
          <fs_alv>-msg = '物料不存在！'.
          <fs_alv>-error = 'X'.
        ELSE.
          CLEAR:ls_zsdt0013.
          SELECT SINGLE * INTO ls_zsdt0013 FROM zsdt0013 WHERE matnr = lv_matnr.
          IF sy-subrc EQ 0 .
            <fs_alv>-light = gc_red.
            <fs_alv>-msg = '记录已存在！'.
            <fs_alv>-error = 'X'.
          ELSE.
  
            <fs_alv>-maktx = ls_makt-maktx.
            <fs_alv>-matnr = lv_matnr.
            <fs_alv>-light = gc_yellow.
            <fs_alv>-uname = sy-uname.
            <fs_alv>-erdat = sy-datum.
            <fs_alv>-erzet = sy-timlo.
          ENDIF.
        ENDIF.
  
      ENDIF.
  
    ENDLOOP.
  
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_TEXT
  *&---------------------------------------------------------------------*
  *& text 获取相关描述
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_get_text .
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_DISPLAY_DATA
  *&---------------------------------------------------------------------*
  *& text 展示内表数据
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_display_data .
  
    PERFORM build_fieldcat.
  
    gs_layout-zebra                = 'X'.
  *  gs_layout-colwidth_optimize    = 'X'.
    BREAK guican.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_default                = 'X'
        i_save                   = ' '
        is_layout                = gs_layout
        it_fieldcat              = gt_fieldcat
        i_callback_pf_status_set = 'SET_PF_STATUS'
        i_callback_user_command  = 'USER_COMMAND'
      TABLES
        t_outtab                 = gt_alv
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form BUILD_FIELDCAT
  *&---------------------------------------------------------------------*
  *& text 设置字段
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM build_fieldcat .
  
  
    PERFORM sub_fill_fieldcat_alv USING 'GT_ALV'  'LIGHT' '指示灯' '6'.
    PERFORM sub_fill_fieldcat_alv USING 'GT_ALV'  'MSG'  '消息'  '20'.
    PERFORM sub_fill_fieldcat_alv USING 'GT_ALV'  'MATNR'   '物料'  '20'.
    PERFORM sub_fill_fieldcat_alv USING 'GT_ALV'  'MAKTX'   '物料描述'  '80'.
    PERFORM sub_fill_fieldcat_alv USING 'GT_ALV'  'ZPWSX'    '批文属性'  '2'.
    PERFORM sub_fill_fieldcat_alv USING 'GT_ALV'  'ZJYRQ'    '禁用日期'  '8'.
    PERFORM sub_fill_fieldcat_alv USING 'GT_ALV'  'ZPJNO'    '批件编码'  '10'.
    PERFORM sub_fill_fieldcat_alv USING 'GT_ALV'  'ZQBNO'    '企标编码'  '10'.
    PERFORM sub_fill_fieldcat_alv USING 'GT_ALV'  'ZXKZNO'    '生产许可证编码'  '10'.
    PERFORM sub_fill_fieldcat_alv USING 'GT_ALV'  'UNAME'    '用户名'  '12'.
    PERFORM sub_fill_fieldcat_alv USING 'GT_ALV'  'ERDAT'    '创建日期'  '8'.
    PERFORM sub_fill_fieldcat_alv USING 'GT_ALV'  'ERZET'    '时间'  '6'.
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form SUB_FILL_FIELDCAT_ALV
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> P_
  *&      --> P_
  *&      --> P_
  *&      --> P_
  *&---------------------------------------------------------------------*
  FORM sub_fill_fieldcat_alv  USING  tabname
                                    fieldname
                                    fieldlabel
                                    pv_outputlen.
  
    DATA: lv_col_pos TYPE i.
  
    lv_col_pos = lv_col_pos + 1.
  
    APPEND INITIAL LINE TO gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).
  
    <fs_fieldcat>-fieldname = fieldname.
    IF fieldlabel <> space.
      <fs_fieldcat>-seltext_l = fieldlabel.
      <fs_fieldcat>-seltext_m = fieldlabel.
      <fs_fieldcat>-seltext_s = fieldlabel.
      <fs_fieldcat>-ddictxt = 'M'.
      <fs_fieldcat>-reptext_ddic = fieldlabel.
    ENDIF.
    <fs_fieldcat>-key = space.
    <fs_fieldcat>-do_sum = space.
    <fs_fieldcat>-col_pos = lv_col_pos.
    <fs_fieldcat>-no_out = space.
    <fs_fieldcat>-hotspot = space.
    <fs_fieldcat>-tabname = tabname.
    <fs_fieldcat>-outputlen  = pv_outputlen .
  
  ENDFORM.
  
  
  FORM set_pf_status USING extab TYPE slis_t_extab.
  
  *  DATA: lv_error_flag(1) TYPE c.
  *
  *  CLEAR: lv_error_flag.
  *  REFRESH gt_exclude.
  *
  *  LOOP AT gt_alv INTO gs_alv WHERE msg <> ''.   "有错误
  *    lv_error_flag = 'X'.
  *  ENDLOOP.
  *
  **&****当有错误信息的时候，“凭证检验” 和“凭证过账” 按钮不可用
  **&****在执行 ”凭证过账“ 之前需要先执行 ”凭证检验“
  *  IF lv_error_flag = 'X'.
  *    IF gv_check_button <> 'X'.
  *      APPEND INITIAL LINE TO gt_exclude ASSIGNING FIELD-SYMBOL(<fs_exclude>).
  *      <fs_exclude>-fcode = 'ZCHECK'.
  *    ENDIF.
  *    APPEND INITIAL LINE TO gt_exclude ASSIGNING <fs_exclude>.
  *    <fs_exclude>-fcode = 'ZPOST'.
  *  ELSE.
  *    IF gv_check_button <> 'X'.
  *      APPEND INITIAL LINE TO gt_exclude ASSIGNING <fs_exclude>.
  *      <fs_exclude>-fcode = 'ZPOST'.
  *    ENDIF.
  *  ENDIF.
  *
  **&***当凭证导入成功后，把 “凭证过帐” 按钮隐藏掉
  *  LOOP AT gt_alv INTO gs_alv WHERE belnr <> ''.
  *    APPEND INITIAL LINE TO gt_exclude ASSIGNING <fs_exclude>.
  *    <fs_exclude>-fcode = 'ZPOST'.
  *  ENDLOOP.
  
    SET PF-STATUS 'PF_STATUS' ."EXCLUDING gt_exclude IMMEDIATELY.
  
  ENDFORM.
  
  
  FORM user_command  USING r_ucomm LIKE sy-ucomm
                           rs_selfield TYPE slis_selfield.
  
    DATA: lv_answer(1) TYPE c.
  
    CASE r_ucomm .
      WHEN 'ZUPLOAD'.
        PERFORM frm_save_data.
  
  *      gv_code = r_ucomm.
  *      CLEAR: r_ucomm.
  *      gv_check_button = 'X'.
  *      LOOP AT gt_alv INTO gs_alv WHERE msg <> ''.
  *        MESSAGE e001(00) WITH '数据检查有误，请看错误信息！'.
  *      ENDLOOP.
  *      PERFORM frm_process_data_for_bapi.
  
  *    WHEN 'ZPOST'.
  *      gv_code = r_ucomm.
  *      CLEAR: r_ucomm.
  *      PERFORM frm_process_data_for_bapi.
  
  *    WHEN '&IC1'.
  *      READ TABLE gt_alv INTO gs_alv INDEX rs_selfield-tabindex.
  *      IF rs_selfield-value IS NOT INITIAL AND rs_selfield-fieldname = 'BELNR'.
  *        SET PARAMETER ID 'BLN' FIELD gs_alv-belnr.
  *        SET PARAMETER ID 'BUK' FIELD gs_alv-bukrs.
  *        SET PARAMETER ID 'GJR' FIELD gs_alv-budat(4).
  *        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  *      ENDIF.
  
      WHEN OTHERS .
  
    ENDCASE .
  
    rs_selfield-refresh = 'X'.
  
  ENDFORM .                    "USER_COMMAND
  *&---------------------------------------------------------------------*
  *& Form FRM_SAVE_DATA
  *&---------------------------------------------------------------------*
  *& text 保存数据
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_save_data .
  
    DATA: ls_account TYPE zsdt0013,
          lt_account TYPE TABLE OF zsdt0013.
  
    FIELD-SYMBOLS:<fs_alv> TYPE ty_alv.
  
    LOOP AT gt_alv ASSIGNING <fs_alv>.
      IF <fs_alv>-error <> 'X'.
        MOVE-CORRESPONDING <fs_alv> TO ls_account.
        APPEND ls_account TO lt_account.
      ELSE.
  *      <fs_alv>-msg = '信息不全，上传失败！'.
      ENDIF.
    ENDLOOP.
  
  
    MODIFY zsdt0013 FROM TABLE lt_account.
    IF sy-subrc EQ 0.
      LOOP AT gt_alv ASSIGNING <fs_alv> WHERE error = ''.
        <fs_alv>-msg = '上传成功'.
        <fs_alv>-light = gc_green.
      ENDLOOP.
    ENDIF.
  
  
    READ TABLE gt_alv INTO DATA(ls_alv) WITH KEY error = 'X'.
  
    IF sy-subrc NE 0.
      MESSAGE '数据全部上传成功！' TYPE 'S'.
    ELSE.
      MESSAGE '数据部分上传失败！' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  
  
  ENDFORM.