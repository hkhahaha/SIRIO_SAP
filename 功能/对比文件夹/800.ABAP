*&---------------------------------------------------------------------*
*& Report ZFIR0045
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir0045.

TABLES: bseg,acdoca,bkpf,zfit0020a,zfit0020b,sscrfields.

TYPE-POOLS: slis.

DATA: BEGIN OF gt_upload OCCURS 0,
*        BUKRS LIKE BKPF-BUKRS,
*        GJAHR LIKE BKPF-GJAHR,
*        MONAT LIKE BKPF-MONAT,
        kostl LIKE csks-kostl,
        rstgr LIKE bseg-rstgr,
        hsl   LIKE acdoca-hsl,
        sgtxt LIKE acdoca-sgtxt,
      END OF gt_upload.

DATA: BEGIN OF gt_output OCCURS 0,
        light   TYPE string,
        message TYPE string,
        bukrs   LIKE bkpf-bukrs,
        gjahr   LIKE bkpf-gjahr,
        monat   LIKE bkpf-monat,
        kostl   LIKE csks-kostl,
        docln   LIKE acdoca-docln,
        ltext   LIKE cskt-ltext,
        hsl     LIKE acdoca-hsl,
        rstgr   LIKE bseg-rstgr,
        txt40   LIKE t053s-txt40,
        sgtxt   LIKE acdoca-sgtxt,
        sel     TYPE c,
      END OF gt_output.

DATA: BEGIN OF gt_zjjh OCCURS 0,
        sel(1), "CHECKBOX
        bukrs   LIKE bseg-bukrs,
        gjahr   LIKE bseg-gjahr,
        jhqj    LIKE bkpf-monat,   "计划期间
        belnr   LIKE bseg-belnr,
        buzei   LIKE bseg-buzei,
        h_budat LIKE bseg-h_budat,
        netdt   LIKE bseg-netdt,
        shkzg   LIKE bseg-shkzg,
        hkont   LIKE bseg-hkont,
        kostl   LIKE csks-kostl,
        ltext   LIKE cskt-ltext,
        txt50   LIKE skat-txt50,
        dmbtr   LIKE bseg-dmbtr,
        lifnr   LIKE bseg-lifnr,
        gysms   LIKE but000-name_org1,
        kunnr   LIKE bseg-kunnr,
        khms    LIKE but000-name_org1,
        rstgr   LIKE bseg-rstgr,
        txt40   LIKE t053s-txt40,
        awkey   LIKE bseg-awkey,
        xsfp    LIKE vbrk-vbeln,
        cgfp    LIKE rbkp-belnr,
        koart   LIKE bseg-koart,
        sgtxt   LIKE bseg-sgtxt,
*        sel     TYPE c,
      END OF gt_zjjh.

DATA: BEGIN OF gt_xh OCCURS 0,
        znum  TYPE i,
        rstgr LIKE bseg-rstgr,
        txt40 LIKE t053s-txt40,
        sjje  LIKE bseg-dmbtr,
        jhje  LIKE bseg-dmbtr,
        zcy   LIKE bseg-dmbtr,
        zbl   TYPE string,
        sel   TYPE c,
      END OF gt_xh.

TYPES: BEGIN OF ts_fagl,
         ryear  TYPE  gjahr, "会计年度
         rbukrs TYPE  bukrs, "公司代码
         rpmax  TYPE  rpmax, "期间
         hslvt  TYPE  hslvt12, "用本币计算的结转余额
         hsl01  TYPE  hslvt12, "按本位币的期间中的业务总计
         hsl02  TYPE  hslvt12, "按本位币的期间中的业务总计
         hsl03  TYPE  hslvt12, "按本位币的期间中的业务总计
         hsl04  TYPE  hslvt12, "按本位币的期间中的业务总计
         hsl05  TYPE  hslvt12, "按本位币的期间中的业务总计
         hsl06  TYPE  hslvt12, "按本位币的期间中的业务总计
         hsl07  TYPE  hslvt12, "按本位币的期间中的业务总计
         hsl08  TYPE  hslvt12, "按本位币的期间中的业务总计
         hsl09  TYPE  hslvt12, "按本位币的期间中的业务总计
         hsl10  TYPE  hslvt12, "按本位币的期间中的业务总计
         hsl11  TYPE  hslvt12, "按本位币的期间中的业务总计
         hsl12  TYPE  hslvt12, "按本位币的期间中的业务总计
       END OF ts_fagl,
       tt_fagl TYPE STANDARD TABLE OF ts_fagl.





DATA gt_zjjh_mx LIKE gt_zjjh OCCURS 0 WITH HEADER LINE.
DATA gt_sjje_mx LIKE gt_zjjh OCCURS 0 WITH HEADER LINE.

SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: r11 RADIOBUTTON GROUP g1 USER-COMMAND uc,
            r12 RADIOBUTTON GROUP g1,
            r13 RADIOBUTTON GROUP g1.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_bukrs LIKE bkpf-bukrs MEMORY ID buk.
PARAMETERS: p_gjahr LIKE bkpf-gjahr MEMORY ID gja.
PARAMETERS: p_monat LIKE bkpf-monat MEMORY ID mon .
PARAMETERS: p_fname TYPE rlgrap-filename MODIF ID a1.

SELECTION-SCREEN SKIP.
PARAMETERS: c1 AS CHECKBOX MODIF ID a2.

SELECTION-SCREEN END OF BLOCK b2.

*SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
*PARAMETERS: R21 RADIOBUTTON GROUP G2 MODIF ID M1,
*            R22 RADIOBUTTON GROUP G2 MODIF ID M1.
*SELECTION-SCREEN END OF BLOCK B3.

INITIALIZATION.
  PERFORM frm_init.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF r12 = 'X' OR r13 = 'X'.
      IF screen-group1 = 'A1'.
        screen-active = '0'.
      ENDIF.
    ENDIF.
    IF r12 <> 'X'.
      IF screen-group1 = 'A2'.
        screen-active = '0'.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM frm_get_upfile.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM frm_export_file.
  ENDCASE.

START-OF-SELECTION.
  PERFORM frm_data_check.
  PERFORM frm_authority_check.
  IF r11 = 'X'.
    PERFORM frm_upload_data.
    PERFORM frm_show_data.
  ELSE.
    PERFORM frm_get_zjjh.
    IF r12 = 'X'.
      PERFORM frm_display1.
    ENDIF.
    IF r13 = 'X'.
      PERFORM frm_zjjhxh.
      PERFORM frm_display2.
    ENDIF.
  ENDIF.


**********************************************************************

FORM frm_init.  "模板图标
  sscrfields-functxt_01+40(4) = icon_export.
  sscrfields-functxt_01+44(10) = '下载模板'.
ENDFORM.

FORM frm_get_upfile.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      mask             = ',Excel Files,*.xls;*.xlsx,All Files,*.*.'
      title            = '选择上传模板'
    IMPORTING
      filename         = p_fname
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.
  IF sy-subrc NE 0 AND sy-subrc NE 3.
    MESSAGE '选择模板文件出错！' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
ENDFORM.

FORM frm_export_file.
  DATA: lv_fname TYPE string,
        lv_fpath TYPE string,
        lv_temp1 TYPE string,
        lv_temp2 TYPE string.

  DATA: lv_filename TYPE rlgrap-filename,
        lv_rc       TYPE sy-subrc,
        lv_errtxt   TYPE string,
        ls_wwwdata  TYPE wwwdatatab.

  SELECT SINGLE *
    FROM wwwdata
    INTO CORRESPONDING FIELDS OF ls_wwwdata
   WHERE objid = 'ZFIR0045'.
  IF sy-subrc NE 0.
    MESSAGE '获取模板失败！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  lv_fname = ls_wwwdata-text && '.xlsx'.
  CALL METHOD cl_gui_frontend_services=>get_desktop_directory
    CHANGING
      desktop_directory    = lv_fpath
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_file_name    = lv_fname
      file_filter          = cl_gui_frontend_services=>filetype_excel
      initial_directory    = lv_fpath
      prompt_on_overwrite  = 'X'
    CHANGING
      filename             = lv_temp1
      path                 = lv_temp2
      fullpath             = lv_fpath
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  CHECK lv_fpath IS NOT INITIAL.

  lv_filename = lv_fpath.
  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      key         = ls_wwwdata
      destination = lv_filename
    IMPORTING
      rc          = lv_rc.
  IF lv_rc NE 0.
    MESSAGE '导入模板文件下载失败！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
ENDFORM.

FORM frm_data_check.
  IF p_bukrs IS INITIAL.
    MESSAGE '请输入公司代码！' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  IF p_gjahr IS INITIAL.
    MESSAGE '请输入年度！' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  IF p_monat IS INITIAL.
    MESSAGE '请输入期间！' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  IF p_fname IS INITIAL AND r11 = 'X'.
    MESSAGE '请选择上传文件！' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
ENDFORM.

FORM frm_authority_check.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'BUKRS'FIELD p_bukrs.
  IF sy-subrc <> 0.
    MESSAGE '您没有公司代码的权限!' TYPE 'E'.
  ENDIF.
ENDFORM.

FORM frm_upload_data.
  CLEAR gt_upload[].
  CALL FUNCTION 'Z_FI_EXCEL_TO_TABLE'
    EXPORTING
      i_filename              = p_fname
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
ENDFORM.

FORM frm_show_data.

  IF gt_upload[] IS NOT INITIAL.
    LOOP AT gt_upload.
      gt_output-bukrs = p_bukrs.
      gt_output-gjahr = p_gjahr.
      gt_output-monat = p_monat.
      MOVE-CORRESPONDING gt_upload TO gt_output.
      APPEND gt_output.CLEAR gt_output.
    ENDLOOP.

    "BEGIN-获取成本中心描述
    DATA: BEGIN OF lt_csks OCCURS 0,
            kostl LIKE csks-kostl,
            ltext LIKE cskt-ltext,
          END OF lt_csks.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_csks
      FROM cskt AS a
      INNER JOIN csks AS b
      ON a~kokrs = b~kokrs
      AND a~kostl = b~kostl
      AND a~datbi = b~datbi
      FOR ALL ENTRIES IN gt_upload
      WHERE a~kostl = gt_upload-kostl
      AND a~spras = 1
      AND b~bukrs = p_bukrs
      AND a~kokrs = '1000'
      AND a~datbi = '99991231'.
    "END-获取成本中心描述

    "BEGIN-获取原因代码描述
    DATA: BEGIN OF lt_t053s OCCURS 0,
            rstgr LIKE t053s-rstgr,
            txt40 LIKE t053s-txt40,
          END OF lt_t053s.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_t053s
      FROM t053s
      FOR ALL ENTRIES IN gt_upload
      WHERE rstgr = gt_upload-rstgr
      AND bukrs = p_bukrs
      AND spras = 1.
    "END-获取原因代码描述

    DATA n1 TYPE i.
    LOOP AT gt_output.
      READ TABLE lt_csks WITH KEY kostl = gt_output-kostl.  "成本中心描述赋值
      IF sy-subrc = 0.
        gt_output-ltext = lt_csks-ltext.
      ENDIF.

      READ TABLE lt_t053s WITH KEY rstgr = gt_output-rstgr.   "获取原因代码描述
      IF sy-subrc = 0.
        gt_output-txt40 = lt_t053s-txt40.
      ENDIF.

      gt_output-light = '@08@'.
      IF gt_output-ltext IS INITIAL.
        gt_output-light = '@0A@'.
        gt_output-message = '成本中心不存在；'.
      ENDIF.
      IF gt_output-txt40 IS INITIAL.
        gt_output-light = '@0A@'.
        gt_output-message = gt_output-message && '原因代码不存在；'.
      ENDIF.

      n1 = n1 + 1.
      gt_output-docln = n1.

      MODIFY gt_output.
    ENDLOOP.

  ENDIF.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_fieldcat TYPE lvc_s_fcat,
        lt_sort     TYPE lvc_t_sort,
        lt_events   TYPE slis_t_event,
        ls_layout   TYPE lvc_s_layo,
        ls_variant  TYPE disvariant,
        l_pos       TYPE i VALUE 0.

  DEFINE setfieldcat.
    l_pos = l_pos + 1.
    wa_fieldcat-col_pos = l_pos.
    wa_fieldcat-fieldname = &1.
    wa_fieldcat-coltext   = &2.
    wa_fieldcat-scrtext_l = &2.
    wa_fieldcat-scrtext_m = &2.
    wa_fieldcat-scrtext_s = &2.
    IF &3 NE space.
      SPLIT &3 AT '-' INTO wa_fieldcat-ref_table wa_fieldcat-ref_field.
    ENDIF.

    wa_fieldcat-outputlen = &4.  "字段长度
    wa_fieldcat-edit = &5.       "字段是否可编辑
    wa_fieldcat-key = &6.       "键值
    wa_fieldcat-hotspot = &7.       "热键
    APPEND wa_fieldcat TO lt_fieldcat. CLEAR: wa_fieldcat.

  END-OF-DEFINITION.

  ls_layout-zebra      = 'X'.
  ls_layout-sel_mode   = 'A'.
  ls_layout-cwidth_opt = 'X'.     "自动调整字段宽度
  ls_layout-box_fname  = 'SEL'.
*  ls_layout-stylefname = 'STYLE'.
  "ls_layout-totals_bef = 'X'.

  ls_variant-report = sy-repid.
  ls_variant-handle = '0001'.  "区分不同布局
  setfieldcat 'LIGHT' '状态' '' '' '' '' ''.
  setfieldcat 'BUKRS' '公司代码' 'BKPF-BUKRS' '' '' '' ''.
  setfieldcat 'GJAHR' '计划年度' 'BKPF-GJAHR' '' '' '' ''.
  setfieldcat 'MONAT' '计划月份' 'BKPF-MONAT' '' '' '' ''.
  setfieldcat 'DOCLN' '序号' 'ACDOCA-DOCLN' '' '' '' ''.
  setfieldcat 'KOSTL' '成本中心' 'CSKS-KOSTL' '' '' '' ''.
  setfieldcat 'LTEXT' '成本中心名称' 'CSKS-LTEXT' '' '' '' ''.
  setfieldcat 'RSTGR' '原因代码' 'BSEG-RSTGR' '' '' '' ''.
  setfieldcat 'TXT40' '原因代码描述' 'T053S-TXT40' '' '' '' ''.
  setfieldcat 'HSL' '本位币金额' 'ACDOCA-HSL' '' '' '' ''.
  setfieldcat 'SGTXT' '说明' 'ACDOCA-SGTXT' '' '' '' ''.
  setfieldcat 'MESSAGE' '处理消息' '' '50' '' '' ''.

*  APPEND VALUE #( name = 'CALLER_EXIT' form = 'FRM_CALLER_EXIT' ) TO lt_events.
*  APPEND VALUE #( spos = 1 fieldname = 'MATNR' up = 'X' ) TO lt_sort.

***BEGIN***单元格编辑回填数据
  DATA: i_grid_settings TYPE lvc_s_glay.
  i_grid_settings-edt_cll_cb = 'X'.
***END***单元格编辑回填数据

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_PF_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND'
      it_fieldcat_lvc          = lt_fieldcat[]
*     it_events                = lt_events
*     it_sort_lvc              = lt_sort
      is_layout_lvc            = ls_layout
      is_variant               = ls_variant    "区分不同布局
      i_save                   = 'A'
      i_grid_settings          = i_grid_settings         "单元格编辑之后数据可以存入内表
    TABLES
      t_outtab                 = gt_output[].

ENDFORM.

FORM frm_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'S1000' EXCLUDING rt_extab.
ENDFORM.

FORM frm_user_command USING r_ucomm LIKE sy-ucomm rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN '&SAVE'.
      PERFORM frm_save_data.
      rs_selfield-refresh = 'X'.
  ENDCASE.

ENDFORM.

FORM frm_save_data.
  LOOP AT gt_output WHERE light = '@0A@'.
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    MESSAGE '存在错误数据，不能够保存！' TYPE 'E'.
    RETURN.
  ENDIF.

  DATA l_answer TYPE c.
  CALL FUNCTION 'POPUP_TO_CONFIRM'   "弹窗提示
    EXPORTING
      text_question         = TEXT-011 "确定此次操作码？
      icon_button_1         = TEXT-012 "是
      icon_button_2         = TEXT-013 "否
      default_button        = '2'
      display_cancel_button = 'X'   "是否显示取消按钮
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = l_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF l_answer <> '1'.
    RETURN.
  ELSE.
    DATA lt_zfit0020c LIKE zfit0020c OCCURS 0 WITH HEADER LINE.
    LOOP AT gt_output.
      MOVE-CORRESPONDING gt_output TO lt_zfit0020c.
      APPEND lt_zfit0020c.CLEAR lt_zfit0020c.
    ENDLOOP.

    DELETE FROM zfit0020c WHERE bukrs = p_bukrs AND gjahr = p_gjahr AND monat = p_monat.

    COMMIT WORK AND WAIT.   "同步处理
    IF sy-subrc = 0.
      LOOP AT lt_zfit0020c.
        INSERT zfit0020c FROM lt_zfit0020c.
      ENDLOOP.

      COMMIT WORK AND WAIT.   "同步处理
      IF sy-subrc = 0.
        MESSAGE '数据更新成功！' TYPE 'I'.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ELSE.
      ROLLBACK WORK.
      MESSAGE '数据更新失败！' TYPE 'I'.
    ENDIF.

  ENDIF.

ENDFORM.


FORM frm_get_zjjh.   "资金计划报表

  DATA lt_zfit0020a LIKE zfit0020a OCCURS 0 WITH HEADER LINE.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_zfit0020a
    FROM zfit0020a
    WHERE bukrs = p_bukrs
    AND gjahr_jh = p_gjahr
    AND monat = p_monat.

  DATA: BEGIN OF lt_bseg OCCURS 0,
          bukrs   LIKE bseg-bukrs,
          gjahr   LIKE bseg-gjahr,
          belnr   LIKE bseg-belnr,
          buzei   LIKE bseg-buzei,
          h_budat LIKE bseg-h_budat,
          netdt   LIKE bseg-netdt,
          shkzg   LIKE bseg-shkzg,
          hkont   LIKE bseg-hkont,
          dmbtr   LIKE bseg-dmbtr,
          lifnr   LIKE bseg-lifnr,
          kunnr   LIKE bseg-kunnr,
          rstgr   LIKE bseg-rstgr,
          awkey   LIKE bseg-awkey,
          koart   LIKE bseg-koart,
          sgtxt   LIKE bseg-sgtxt,
        END OF lt_bseg.

  IF lt_zfit0020a[] IS NOT INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_bseg
      FROM bseg
      FOR ALL ENTRIES IN lt_zfit0020a
      WHERE bukrs = lt_zfit0020a-bukrs
      AND gjahr = lt_zfit0020a-gjahr
      AND belnr = lt_zfit0020a-belnr
      AND buzei = lt_zfit0020a-buzei.
  ENDIF.

  IF lt_zfit0020a[] IS INITIAL OR c1 = 'X'.
    DATA: f_date LIKE bkpf-budat.   "期间第一天
    DATA: l_date LIKE bkpf-budat.   "期间最后一天

    f_date = p_gjahr && p_monat && '01'.

    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = f_date
      IMPORTING
        last_day_of_month = l_date
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.

    DATA: BEGIN OF l_saknr OCCURS 0,
            saknr LIKE ska1-saknr,
            txt50 LIKE skat-txt50,
          END OF l_saknr.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE l_saknr
      FROM zfit0020b AS a
      INNER JOIN skat AS b
      ON a~saknr = b~saknr
      AND spras = '1'
      AND ktopl = '1000'.

    IF l_saknr[] IS NOT INITIAL.
*      SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_bseg
      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE lt_bseg
        FROM bseg
        FOR ALL ENTRIES IN l_saknr
        WHERE hkont = l_saknr-saknr
        AND bukrs = p_bukrs
        AND augbl = ''
        AND netdt <= l_date.
*    排除9开头凭证
      IF sy-subrc = 0.
        SORT lt_bseg BY belnr buzei.
        DELETE lt_bseg WHERE belnr+0(1)  = 9.
      ENDIF.
    ENDIF.
  ENDIF.

  "删除重复项 202220117
  SORT lt_bseg BY bukrs gjahr belnr buzei.
  DELETE ADJACENT DUPLICATES FROM lt_bseg COMPARING ALL FIELDS.

  IF lt_bseg[] IS NOT INITIAL.
    LOOP AT lt_bseg.
      IF lt_bseg-shkzg = 'H'.
        lt_bseg-dmbtr = 0 - lt_bseg-dmbtr.
      ENDIF.
*      如果原因代码为空，默认为201
      IF lt_bseg-rstgr = ''.
        lt_bseg-rstgr = '201'.
      ENDIF.
      MOVE-CORRESPONDING lt_bseg TO gt_zjjh.
      APPEND gt_zjjh.CLEAR gt_zjjh.
      MODIFY lt_bseg.
    ENDLOOP.
  ENDIF.

  DATA lt_zfit0020c LIKE zfit0020c OCCURS 0 WITH HEADER LINE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_zfit0020c
    FROM zfit0020c
    WHERE bukrs = p_bukrs
    AND gjahr = p_gjahr
    AND monat = p_monat.

  DATA: BEGIN OF lt_kostl OCCURS 0,
          kostl LIKE csks-kostl,
          ltext LIKE cskt-ltext,
        END OF lt_kostl.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_kostl
  FROM cskt
  WHERE spras = '1'
  AND datbi = '99991231'
  AND kokrs = '1000'.

  LOOP AT lt_zfit0020c.
    READ TABLE lt_kostl WITH KEY kostl = lt_zfit0020c-kostl.
    IF sy-subrc = 0.
      gt_zjjh-ltext = lt_kostl-ltext.
    ENDIF.

    MOVE-CORRESPONDING lt_zfit0020c TO gt_zjjh.
    gt_zjjh-dmbtr = 0 - lt_zfit0020c-hsl.
    gt_zjjh-hkont = '1123020000'.
    gt_zjjh-txt50 = '预付账款-集团外'.
    APPEND gt_zjjh.CLEAR gt_zjjh.

  ENDLOOP.

  "BEGIN-获取原因代码描述
  DATA: BEGIN OF lt_t053s OCCURS 0,
          rstgr LIKE t053s-rstgr,
          txt40 LIKE t053s-txt40,
        END OF lt_t053s.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_t053s
    FROM t053s
    WHERE bukrs = p_bukrs
    AND spras = 1.
  "END-获取原因代码描述

  "BEGIN-获取科目描述
  DATA: BEGIN OF l_saknr1 OCCURS 0,
          saknr LIKE ska1-saknr,
          txt50 LIKE skat-txt50,
        END OF l_saknr1.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE l_saknr1
    FROM skat
    WHERE spras = '1'
    AND ktopl = '1000'.
  "END-获取科目码描述

  DATA: BEGIN OF lt_but000 OCCURS 0,
          partner   LIKE but000-partner,
          name_org1 LIKE but000-name_org1,
        END OF lt_but000.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_but000
    FROM but000.

  IF gt_zjjh[] IS NOT INITIAL.
    LOOP AT gt_zjjh.
      READ TABLE l_saknr WITH KEY saknr = gt_zjjh-hkont.
      IF sy-subrc = 0.
        gt_zjjh-txt50 = l_saknr-txt50.
      ENDIF.
      gt_zjjh-jhqj = p_monat.

      IF gt_zjjh-rstgr IS NOT INITIAL.
        READ TABLE lt_t053s WITH KEY rstgr = gt_zjjh-rstgr.
        IF sy-subrc = 0.
          gt_zjjh-txt40 = lt_t053s-txt40.
        ENDIF.
      ENDIF.

      IF gt_zjjh-hkont IS NOT INITIAL.
        READ TABLE l_saknr1 WITH KEY saknr = gt_zjjh-hkont.
        IF sy-subrc = 0.
          gt_zjjh-txt50 = l_saknr1-txt50.
        ENDIF.
      ENDIF.

      IF gt_zjjh-koart = 'K'.
        READ TABLE lt_but000 WITH KEY partner = gt_zjjh-lifnr.
        IF sy-subrc = 0.
          gt_zjjh-gysms = lt_but000-name_org1.
        ENDIF.
        gt_zjjh-cgfp = gt_zjjh-awkey+0(10).
      ENDIF.

      IF gt_zjjh-koart = 'D'.
        READ TABLE lt_but000 WITH KEY partner = gt_zjjh-kunnr.
        IF sy-subrc = 0.
          gt_zjjh-khms = lt_but000-name_org1.
        ENDIF.
        gt_zjjh-xsfp = gt_zjjh-awkey+0(10).
      ENDIF.

      MODIFY gt_zjjh.
    ENDLOOP.

  ENDIF.

  SORT gt_zjjh BY hkont h_budat.

ENDFORM.

FORM frm_display1.
  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_fieldcat TYPE lvc_s_fcat,
        lt_sort     TYPE lvc_t_sort,
        lt_events   TYPE slis_t_event,
        ls_layout   TYPE lvc_s_layo,
        ls_variant  TYPE disvariant,
        l_pos       TYPE i VALUE 0.

  DEFINE setfieldcat.
    l_pos = l_pos + 1.
    wa_fieldcat-col_pos = l_pos.
    wa_fieldcat-fieldname = &1.
    wa_fieldcat-coltext   = &2.
    wa_fieldcat-scrtext_l = &2.
    wa_fieldcat-scrtext_m = &2.
    wa_fieldcat-scrtext_s = &2.
    IF &3 NE space.
      SPLIT &3 AT '-' INTO wa_fieldcat-ref_table wa_fieldcat-ref_field.
    ENDIF.
    wa_fieldcat-outputlen = &4.  "字段长度
    wa_fieldcat-edit = &5.       "字段是否可编辑
    wa_fieldcat-key = &6.       "键值
    wa_fieldcat-hotspot = &7.       "热键
    APPEND wa_fieldcat TO lt_fieldcat. CLEAR: wa_fieldcat.
  END-OF-DEFINITION.

  ls_layout-zebra      = 'X'.
  ls_layout-sel_mode   = 'A'.
  ls_layout-cwidth_opt = 'X'.     "自动调整字段宽度
*  ls_layout-box_fname  = 'SEL'.
*  ls_layout-stylefname = 'STYLE'.
  "ls_layout-totals_bef = 'X'.

  ls_variant-report = sy-repid.
  ls_variant-handle = '0002'.  "区分不同布局
  IF c1 = 'X'.
    setfieldcat 'SEL' '选择' '' '' 'X' '' ''.
  ENDIF.
  setfieldcat 'BUKRS' '公司代码' 'BKPF-BUKRS' '' '' '' ''.
  setfieldcat 'GJAHR' '计划年度' 'BKPF-GJAHR' '' '' '' ''.
  setfieldcat 'JHQJ' '计划月份' 'BKPF-MONAT' '' '' '' ''.
  setfieldcat 'BELNR' '凭证编号' 'BSEG-BELNR' '' '' '' 'X'.
  setfieldcat 'BUZEI' '行项目' 'BSEG-BUZEI' '' '' '' ''.
  setfieldcat 'H_BUDAT' '过账日期' 'BSEG-H_BUDAT' '' '' '' ''.
  setfieldcat 'NETDT' '到期日' 'BSEG-NETDT' '' '' '' ''.
  setfieldcat 'HKONT' '科目' 'BSEG-HKONT' '' '' '' ''.
  setfieldcat 'TXT50' '科目描述' '' '' '' '' ''.
  setfieldcat 'RSTGR' '原因代码' 'BSEG-RSTGR' '' '' '' ''.
  setfieldcat 'TXT40' '原因代码描述' '' '' '' '' ''.
  setfieldcat 'DMBTR' '本位币金额' 'BSEG-DMBTR' '' '' '' ''.
  setfieldcat 'SGTXT' '说明' '' '' '' '' ''.
  setfieldcat 'LIFNR' '供应商' 'BSEG-LIFNR' '' '' '' ''.
  setfieldcat 'GYSMS' '供应商名称' '' '' '' '' ''.
  setfieldcat 'CGFP' '采购发票' '' '' '' '' ''.
  setfieldcat 'KUNNR' '客户' 'BSEG-KUNNR' '' '' '' ''.
  setfieldcat 'KHMS' '客户名称' '' '' '' '' ''.
  setfieldcat 'XSFP' '销售发票' '' '' '' '' ''.
  setfieldcat 'KOSTL' '成本中心' 'CSKS-KOSTL' '' '' '' ''.
  setfieldcat 'LTEXT' '成本中心名称' '' '' '' '' ''.
  READ TABLE lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fcat>) WITH KEY fieldname = 'SEL'.
  IF sy-subrc = 0.
    <ls_fcat>-checkbox = 'X'.
  ENDIF.
*  APPEND VALUE #( name = 'CALLER_EXIT' form = 'FRM_CALLER_EXIT' ) TO lt_events.
*  APPEND VALUE #( spos = 1 fieldname = 'MATNR' up = 'X' ) TO lt_sort.

***BEGIN***单元格编辑回填数据
  DATA: i_grid_settings TYPE lvc_s_glay.
  i_grid_settings-edt_cll_cb = 'X'.
***END***单元格编辑回填数据

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_PF_STATUS2'
      i_callback_user_command  = 'FRM_USER_COMMAND_DETAIL0'
      it_fieldcat_lvc          = lt_fieldcat[]
      it_events                = lt_events
*     it_sort_lvc              = lt_sort
      is_layout_lvc            = ls_layout
      is_variant               = ls_variant    "区分不同布局
      i_save                   = 'A'
      i_grid_settings          = i_grid_settings         "单元格编辑之后数据可以存入内表
    TABLES
      t_outtab                 = gt_zjjh[].
ENDFORM.

FORM frm_pf_status2 USING rt_extab TYPE slis_t_extab.
  IF c1 = ''. "重新获取取数为空时 不显示alv 选择
    SET PF-STATUS 'S3000' EXCLUDING rt_extab.

  ELSE.
    SET PF-STATUS 'S2000' EXCLUDING rt_extab.

  ENDIF.
ENDFORM.

FORM frm_user_command_detail0 USING  pv_ucomm    LIKE sy-ucomm
                                     ps_selfield TYPE slis_selfield.
  ps_selfield-refresh = 'X'. "自动刷新,需要写在最后面，不然不会更新到内表
  ps_selfield-row_stable = 'X'. "在自动刷新时不变
  ps_selfield-col_stable = 'X'.
  CASE pv_ucomm.
    WHEN '&IC1'.
      IF ps_selfield-fieldname EQ 'BELNR' AND ps_selfield-value IS NOT INITIAL.
        READ TABLE gt_zjjh INTO DATA(ls_alv) INDEX ps_selfield-tabindex.
        SET PARAMETER ID 'BLN' FIELD ls_alv-belnr.
        SET PARAMETER ID 'GJR' FIELD ls_alv-gjahr.
        SET PARAMETER ID 'BUK' FIELD ls_alv-bukrs.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .
      ENDIF.
    WHEN '&SAVE'.
*      PERFORM frm_save_data2.
      PERFORM frm_save_seldata.
    WHEN 'ALL'."全选
      PERFORM frm_sel_all.
    WHEN 'SAL'."全不选
      PERFORM frm_sel_dall.
    WHEN 'FX'  ."反选
      PERFORM frm_sel_fx.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

FORM frm_save_data2.



  DATA l_answer TYPE c.
  CALL FUNCTION 'POPUP_TO_CONFIRM'   "弹窗提示
    EXPORTING
      text_question         = TEXT-011 "确定此次操作码？
      icon_button_1         = TEXT-012 "是
      icon_button_2         = TEXT-013 "否
      default_button        = '2'
      display_cancel_button = 'X'   "是否显示取消按钮
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = l_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF l_answer <> '1'.
    RETURN.
  ELSE.

    DATA lt_zfit0020a LIKE zfit0020a OCCURS 0 WITH HEADER LINE.
    DATA znum TYPE i.
    LOOP AT gt_zjjh WHERE belnr IS NOT INITIAL.
      znum = znum + 1.
      lt_zfit0020a-bukrs = p_bukrs.
      lt_zfit0020a-gjahr_jh = p_gjahr.
      lt_zfit0020a-monat = p_monat.
      lt_zfit0020a-docln = znum.
      lt_zfit0020a-gjahr = gt_zjjh-h_budat+0(4).
      lt_zfit0020a-belnr = gt_zjjh-belnr.
      lt_zfit0020a-buzei = gt_zjjh-buzei.
      APPEND lt_zfit0020a.CLEAR lt_zfit0020a.
    ENDLOOP.

    DELETE FROM zfit0020a WHERE bukrs = p_bukrs AND gjahr = p_gjahr AND monat = p_monat.

    COMMIT WORK AND WAIT.   "同步处理
    IF sy-subrc = 0.
      LOOP AT lt_zfit0020a.
        INSERT zfit0020a FROM lt_zfit0020a.
      ENDLOOP.

      COMMIT WORK AND WAIT.   "同步处理
      IF sy-subrc = 0.
        MESSAGE '数据更新成功！' TYPE 'I'.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ELSE.
      ROLLBACK WORK.
      MESSAGE '数据更新失败！' TYPE 'I'.
    ENDIF.
  ENDIF.

ENDFORM.

FORM frm_zjjhxh.

  DATA gt_zjjh2 LIKE gt_zjjh OCCURS 0 WITH HEADER LINE.
  DATA gt_zjjh3 LIKE gt_zjjh OCCURS 0 WITH HEADER LINE.
  DATA lt_fagl  TYPE tt_fagl.

  LOOP AT gt_zjjh.
    gt_zjjh2-rstgr = gt_zjjh-rstgr.
    gt_zjjh2-txt40 = gt_zjjh-txt40.
    gt_zjjh2-dmbtr = gt_zjjh-dmbtr.
    COLLECT gt_zjjh2 INTO gt_zjjh3.
  ENDLOOP.

  DATA lt_xh LIKE gt_xh OCCURS 0 WITH HEADER LINE.
  DATA lt_xh2 LIKE gt_xh OCCURS 0 WITH HEADER LINE.
  LOOP AT gt_zjjh3.
    lt_xh-rstgr = gt_zjjh3-rstgr.
    lt_xh-jhje = gt_zjjh3-dmbtr.
    COLLECT lt_xh INTO lt_xh2.
  ENDLOOP.

  DATA: BEGIN OF lt_sjje OCCURS 0,
          rstgr LIKE bseg-rstgr,
          dmbtr LIKE bseg-dmbtr,
          shkzg LIKE bseg-shkzg,
        END OF lt_sjje.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_sjje
    FROM bseg
    WHERE ( hkont LIKE '1001%' OR hkont LIKE '1002%' OR hkont LIKE '1012%')
    AND bukrs = p_bukrs
    AND h_monat = p_monat
    AND gjahr = p_gjahr.

* 计划、实际期初余额取FAGLFLEXT表中RACCT=1001010000-1012999999，
*公司代码RBUKRS、年度RYEAR等于选择界面输入的数据，取日期<选择界面日期合计。
  SELECT ryear
         rbukrs
         rpmax
         hslvt
         hsl01
         hsl02
         hsl03
         hsl04
         hsl05
         hsl06
         hsl07
         hsl08
         hsl09
         hsl10
         hsl11
         hsl12
    INTO TABLE lt_fagl
    FROM faglflext
   WHERE ryear  = p_gjahr
*     AND rpmax <= p_monat
     AND racct BETWEEN '1001010000' AND '1012999999'
     AND rbukrs = p_bukrs.
  IF sy-subrc = 0.
    SORT lt_fagl BY rbukrs ryear rpmax.
  ENDIF.

  DATA lt_sjje2 LIKE lt_sjje OCCURS 0 WITH HEADER LINE.
  DATA lt_sjje3 LIKE lt_sjje OCCURS 0 WITH HEADER LINE.
  LOOP AT lt_sjje.
    IF lt_sjje-shkzg = 'H'.
      lt_sjje-dmbtr = 0 - lt_sjje-dmbtr.
    ENDIF.
    lt_sjje2-rstgr = lt_sjje-rstgr.
    lt_sjje2-dmbtr = lt_sjje-dmbtr.

    COLLECT lt_sjje2 INTO lt_sjje3.
  ENDLOOP.

  LOOP AT lt_sjje3.
    lt_xh2-rstgr = lt_sjje3-rstgr.
    lt_xh2-sjje = lt_sjje3-dmbtr.
    APPEND lt_xh2.CLEAR lt_xh2.
  ENDLOOP.

  DATA lt_xh3 LIKE gt_xh OCCURS 0 WITH HEADER LINE.
  LOOP AT lt_xh2.
    COLLECT lt_xh2 INTO lt_xh3.
  ENDLOOP.


  gt_xh-znum = 1.gt_xh-rstgr = '101'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 2.gt_xh-rstgr = '102'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 3.gt_xh-rstgr = '103'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 4.gt_xh-rstgr = 'A10'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 5.gt_xh-rstgr = '201'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 6.gt_xh-rstgr = '202'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 7.gt_xh-rstgr = '203'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 8.gt_xh-rstgr = '204'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 9.gt_xh-rstgr = 'A20'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 10.gt_xh-rstgr = 'Z10'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 11.gt_xh-rstgr = '301'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 12.gt_xh-rstgr = '302'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 13.gt_xh-rstgr = '303'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 14.gt_xh-rstgr = '304'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 15.gt_xh-rstgr = '305'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 16.gt_xh-rstgr = 'A30'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 17.gt_xh-rstgr = '401'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 18.gt_xh-rstgr = '402'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 19.gt_xh-rstgr = '403'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 20.gt_xh-rstgr = '404'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 21.gt_xh-rstgr = 'A40'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 22.gt_xh-rstgr = 'Z20'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 23.gt_xh-rstgr = '501'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 24.gt_xh-rstgr = '502'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 25.gt_xh-rstgr = '503'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 26.gt_xh-rstgr = 'A50'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 27.gt_xh-rstgr = '601'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 28.gt_xh-rstgr = '602'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 29.gt_xh-rstgr = '603'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 30.gt_xh-rstgr = 'A60'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 31.gt_xh-rstgr = 'Z30'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 32.gt_xh-rstgr = 'X10'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 33.gt_xh-rstgr = 'X20'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 34.gt_xh-rstgr = 'Z40'.APPEND gt_xh.CLEAR gt_xh.
  gt_xh-znum = 35.gt_xh-rstgr = 'Z50'.APPEND gt_xh.CLEAR gt_xh.
  SORT gt_xh BY znum.

  DATA l_bl TYPE p DECIMALS 2.
  DATA l_a100_jh TYPE p DECIMALS 2.
  DATA l_a100_sj TYPE p DECIMALS 2.
  DATA l_a200_jh TYPE p DECIMALS 2.
  DATA l_a200_sj TYPE p DECIMALS 2.
  DATA l_a300_jh TYPE p DECIMALS 2.
  DATA l_a300_sj TYPE p DECIMALS 2.
  DATA l_a400_jh TYPE p DECIMALS 2.
  DATA l_a400_sj TYPE p DECIMALS 2.
  DATA l_a500_jh TYPE p DECIMALS 2.
  DATA l_a500_sj TYPE p DECIMALS 2.
  DATA l_a600_jh TYPE p DECIMALS 2.
  DATA l_a600_sj TYPE p DECIMALS 2.
  DATA l_z100_jh TYPE p DECIMALS 2.
  DATA l_z100_sj TYPE p DECIMALS 2.
  DATA l_z200_jh TYPE p DECIMALS 2.
  DATA l_z200_sj TYPE p DECIMALS 2.
  DATA l_z300_jh TYPE p DECIMALS 2.
  DATA l_z300_sj TYPE p DECIMALS 2.
  DATA l_z500_jh TYPE p DECIMALS 2.
  DATA l_z500_sj TYPE p DECIMALS 2.


  LOOP AT gt_xh.
    READ TABLE lt_xh3 WITH KEY rstgr = gt_xh-rstgr.
    IF sy-subrc = 0.
      gt_xh-sjje = lt_xh3-sjje.
      gt_xh-jhje = lt_xh3-jhje.
    ENDIF.

    IF gt_xh-rstgr+0(1) = '1'.
      l_a100_sj = l_a100_sj + gt_xh-sjje.
      l_a100_jh = l_a100_jh + gt_xh-jhje.
    ENDIF.
    IF gt_xh-rstgr+0(1) = '2'.
      l_a200_sj = l_a200_sj + gt_xh-sjje.
      l_a200_jh = l_a200_jh + gt_xh-jhje.
    ENDIF.
    IF gt_xh-rstgr+0(1) = '3'.
      l_a300_sj = l_a300_sj + gt_xh-sjje.
      l_a300_jh = l_a300_jh + gt_xh-jhje.
    ENDIF.
    IF gt_xh-rstgr+0(1) = '4'.
      l_a400_sj = l_a400_sj + gt_xh-sjje.
      l_a400_jh = l_a400_jh + gt_xh-jhje.
    ENDIF.
    IF gt_xh-rstgr+0(1) = '5'.
      l_a500_sj = l_a500_sj + gt_xh-sjje.
      l_a500_jh = l_a500_jh + gt_xh-jhje.
    ENDIF.
    IF gt_xh-rstgr+0(1) = '6'.
      l_a600_sj = l_a600_sj + gt_xh-sjje.
      l_a600_jh = l_a600_jh + gt_xh-jhje.
    ENDIF.

    IF gt_xh-rstgr = 'A10'.
      gt_xh-txt40 = '经营活动现金流入小计'.
      gt_xh-sjje = l_a100_sj.
      gt_xh-jhje = l_a100_jh.
    ELSEIF gt_xh-rstgr = 'A20'.
      gt_xh-txt40 = '经营活动现金流出小计'.
      gt_xh-sjje = l_a200_sj.
      gt_xh-jhje = l_a200_jh.
    ELSEIF gt_xh-rstgr = 'Z10'.
      gt_xh-txt40 = '经营活动产生的现金流量净额'.
    ELSEIF gt_xh-rstgr = 'A30'.
      gt_xh-txt40 = '投资活动现金流入小计'.
      gt_xh-sjje = l_a300_sj.
      gt_xh-jhje = l_a300_jh.
    ELSEIF gt_xh-rstgr = 'A40'.
      gt_xh-txt40 = '投资活动现金流出小计'.
      gt_xh-sjje = l_a400_sj.
      gt_xh-jhje = l_a400_jh.
    ELSEIF gt_xh-rstgr = 'Z20'.
      gt_xh-txt40 = '投资活动产生的现金流量净额'.
    ELSEIF gt_xh-rstgr = 'A50'.
      gt_xh-txt40 = '筹资活动现金流入小计'.
      gt_xh-sjje = l_a500_sj.
      gt_xh-jhje = l_a500_jh.
    ELSEIF gt_xh-rstgr = 'A60'.
      gt_xh-txt40 = '筹资活动现金流出小计'.
      gt_xh-sjje = l_a600_sj.
      gt_xh-jhje = l_a600_jh.
    ELSEIF gt_xh-rstgr = 'Z30'.
      gt_xh-txt40 = '筹资活动产生的现金流量净额'.
    ELSEIF gt_xh-rstgr = 'X10'.
      gt_xh-txt40 = '库存现金及银行存款'.
    ELSEIF gt_xh-rstgr = 'X20'.
      gt_xh-txt40 = '其他货币资金'.
    ELSEIF gt_xh-rstgr = 'Z40'.
      gt_xh-txt40 = '期初余额'.
      "  计划、实际期初余额 取值逻辑更改
      PERFORM frm_edit_z40 USING lt_fagl CHANGING gt_xh-sjje gt_xh-jhje .
    ELSEIF gt_xh-rstgr = 'Z50'.
      gt_xh-txt40 = '当月现金流量净额'.
      " 当月现金流量净额，当月计划金额=Z10+Z20+Z30+Z40
      PERFORM frm_edit_z50 CHANGING gt_xh-jhje.
    ENDIF.

    IF gt_xh-rstgr = 'A10' OR gt_xh-rstgr = 'A20' .
      l_z100_jh = l_z100_jh + gt_xh-jhje.
      l_z100_sj = l_z100_sj + gt_xh-sjje.
    ENDIF.
    IF gt_xh-rstgr = 'Z10'.
      gt_xh-jhje = l_z100_jh.
      gt_xh-sjje = l_z100_sj.
    ENDIF.

    IF gt_xh-rstgr = 'A30' OR gt_xh-rstgr = 'A40' .
      l_z200_jh = l_z200_jh + gt_xh-jhje.
      l_z200_sj = l_z200_sj + gt_xh-sjje.
    ENDIF.
    IF gt_xh-rstgr = 'Z20'.
      gt_xh-jhje = l_z200_jh.
      gt_xh-sjje = l_z200_sj.
    ENDIF.

    IF gt_xh-rstgr = 'A50' OR gt_xh-rstgr = 'A60' .
      l_z300_jh = l_z300_jh + gt_xh-jhje.
      l_z300_sj = l_z300_sj + gt_xh-sjje.
    ENDIF.
    IF gt_xh-rstgr = 'Z30'.
      gt_xh-jhje = l_z300_jh.
      gt_xh-sjje = l_z300_sj.
    ENDIF.

    IF gt_xh-rstgr = 'Z10' OR gt_xh-rstgr = 'Z20' OR gt_xh-rstgr = 'Z30' .
      l_z500_jh = l_z500_jh + gt_xh-jhje.
      l_z500_sj = l_z500_sj + gt_xh-sjje.
    ENDIF.
    IF gt_xh-rstgr = 'Z50'.
*      gt_xh-jhje = l_z500_jh. "计划金额用更改后的逻辑
      gt_xh-sjje = l_z500_sj.
    ENDIF.

    gt_xh-zcy = gt_xh-sjje - gt_xh-jhje.
    IF gt_xh-jhje <> 0.
      CLEAR l_bl.
      l_bl = gt_xh-zcy / gt_xh-jhje * 100.
    ELSE.
      l_bl = 0.
    ENDIF.
    gt_xh-zbl = l_bl && '%'.
    MODIFY gt_xh.
  ENDLOOP.

  "BEGIN-获取原因代码描述
  DATA: BEGIN OF lt_t053s OCCURS 0,
          rstgr LIKE t053s-rstgr,
          txt40 LIKE t053s-txt40,
        END OF lt_t053s.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_t053s
    FROM t053s
    WHERE bukrs = p_bukrs
    AND spras = 1.
  "END-获取原因代码描述

  LOOP AT gt_xh.
    READ TABLE lt_t053s WITH KEY rstgr = gt_xh-rstgr.
    IF sy-subrc = 0.
      gt_xh-txt40 = lt_t053s-txt40.
      MODIFY gt_xh.
    ENDIF.
  ENDLOOP.

ENDFORM.


FORM frm_display2.
  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_fieldcat TYPE lvc_s_fcat,
        lt_sort     TYPE lvc_t_sort,
        lt_events   TYPE slis_t_event,
        ls_layout   TYPE lvc_s_layo,
        ls_variant  TYPE disvariant,
        l_pos       TYPE i VALUE 0.

  DEFINE setfieldcat.
    l_pos = l_pos + 1.
    wa_fieldcat-col_pos = l_pos.
    wa_fieldcat-fieldname = &1.
    wa_fieldcat-coltext   = &2.
    wa_fieldcat-scrtext_l = &2.
    wa_fieldcat-scrtext_m = &2.
    wa_fieldcat-scrtext_s = &2.
    IF &3 NE space.
      SPLIT &3 AT '-' INTO wa_fieldcat-ref_table wa_fieldcat-ref_field.
    ENDIF.
    wa_fieldcat-outputlen = &4.  "字段长度
    wa_fieldcat-edit = &5.       "字段是否可编辑
    wa_fieldcat-key = &6.       "键值
    wa_fieldcat-hotspot = &7.       "热键
    APPEND wa_fieldcat TO lt_fieldcat. CLEAR: wa_fieldcat.
  END-OF-DEFINITION.

  ls_layout-zebra      = 'X'.
  ls_layout-sel_mode   = 'A'.
  ls_layout-cwidth_opt = 'X'.     "自动调整字段宽度
  ls_layout-box_fname  = 'SEL'.
*  ls_layout-stylefname = 'STYLE'.
  "ls_layout-totals_bef = 'X'.

  ls_variant-report = sy-repid.
  ls_variant-handle = '0003'.  "区分不同布局
  setfieldcat 'RSTGR' '原因代码' 'BSEG-RSTGR' '' '' '' ''.
  setfieldcat 'TXT40' '原因代码描述' '' '' '' '' ''.
  setfieldcat 'JHJE' '当月计划金额' '' '' '' '' 'X'.
  setfieldcat 'SJJE' '当月实际金额' '' '' '' '' 'X'.
  setfieldcat 'ZCY' '差异金额' '' '' '' '' ''.
  setfieldcat 'ZBL' '差异比例' '' '' '' '' ''.

*  APPEND VALUE #( name = 'CALLER_EXIT' form = 'FRM_CALLER_EXIT' ) TO lt_events.
*  APPEND VALUE #( spos = 1 fieldname = 'MATNR' up = 'X' ) TO lt_sort.

***BEGIN***单元格编辑回填数据
  DATA: i_grid_settings TYPE lvc_s_glay.
  i_grid_settings-edt_cll_cb = 'X'.
***END***单元格编辑回填数据

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program      = sy-repid
*     I_CALLBACK_PF_STATUS_SET = 'FRM_PF_STATUS'
      i_callback_user_command = 'FRM_USER_COMMAND2'
      it_fieldcat_lvc         = lt_fieldcat[]
*     it_events               = lt_events
*     it_sort_lvc             = lt_sort
      is_layout_lvc           = ls_layout
      is_variant              = ls_variant    "区分不同布局
      i_save                  = 'A'
      i_grid_settings         = i_grid_settings         "单元格编辑之后数据可以存入内表
    TABLES
      t_outtab                = gt_xh[].
ENDFORM.

FORM frm_user_command2 USING r_ucomm LIKE sy-ucomm
                             rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&IC1'.
      CLEAR r_ucomm.
      READ TABLE gt_xh INDEX rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
        WHEN 'JHJE'.
          PERFORM frm_display_detail1.
        WHEN 'SJJE'.
          PERFORM frm_display_detail2.
        WHEN OTHERS.
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.

FORM frm_display_detail1.
  CLEAR gt_zjjh_mx[].
  IF gt_xh-rstgr = 'A10'.
    LOOP AT gt_zjjh WHERE rstgr+0(1) = 1.
      MOVE-CORRESPONDING gt_zjjh TO gt_zjjh_mx.
      APPEND gt_zjjh_mx.CLEAR gt_zjjh_mx.
    ENDLOOP.
  ELSEIF gt_xh-rstgr = 'A20'.
    LOOP AT gt_zjjh WHERE rstgr+0(1) = 2.
      MOVE-CORRESPONDING gt_zjjh TO gt_zjjh_mx.
      APPEND gt_zjjh_mx.CLEAR gt_zjjh_mx.
    ENDLOOP.
  ELSEIF gt_xh-rstgr = 'A30'.
    LOOP AT gt_zjjh WHERE rstgr+0(1) = 3.
      MOVE-CORRESPONDING gt_zjjh TO gt_zjjh_mx.
      APPEND gt_zjjh_mx.CLEAR gt_zjjh_mx.
    ENDLOOP.
  ELSEIF gt_xh-rstgr = 'A40'.
    LOOP AT gt_zjjh WHERE rstgr+0(1) = 4.
      MOVE-CORRESPONDING gt_zjjh TO gt_zjjh_mx.
      APPEND gt_zjjh_mx.CLEAR gt_zjjh_mx.
    ENDLOOP.
  ELSEIF gt_xh-rstgr = 'A50'.
    LOOP AT gt_zjjh WHERE rstgr+0(1) = 5.
      MOVE-CORRESPONDING gt_zjjh TO gt_zjjh_mx.
      APPEND gt_zjjh_mx.CLEAR gt_zjjh_mx.
    ENDLOOP.
  ELSEIF gt_xh-rstgr = 'A60'.
    LOOP AT gt_zjjh WHERE rstgr+0(1) = 6.
      MOVE-CORRESPONDING gt_zjjh TO gt_zjjh_mx.
      APPEND gt_zjjh_mx.CLEAR gt_zjjh_mx.
    ENDLOOP.
  ELSEIF gt_xh-rstgr = 'Z10'.
    LOOP AT gt_zjjh WHERE rstgr+0(1) = 1 OR rstgr+0(1) = 2.
      MOVE-CORRESPONDING gt_zjjh TO gt_zjjh_mx.
      APPEND gt_zjjh_mx.CLEAR gt_zjjh_mx.
    ENDLOOP.
  ELSEIF gt_xh-rstgr = 'Z20'.
    LOOP AT gt_zjjh WHERE rstgr+0(1) = 3 OR rstgr+0(1) = 4.
      MOVE-CORRESPONDING gt_zjjh TO gt_zjjh_mx.
      APPEND gt_zjjh_mx.CLEAR gt_zjjh_mx.
    ENDLOOP.
  ELSEIF gt_xh-rstgr = 'Z30'.
    LOOP AT gt_zjjh WHERE rstgr+0(1) = 5 OR rstgr+0(1) = 6.
      MOVE-CORRESPONDING gt_zjjh TO gt_zjjh_mx.
      APPEND gt_zjjh_mx.CLEAR gt_zjjh_mx.
    ENDLOOP.
  ELSEIF gt_xh-rstgr = 'Z50'.
    LOOP AT gt_zjjh WHERE rstgr+0(1) = 1 OR rstgr+0(1) = 2 OR rstgr+0(1) = 3 OR rstgr+0(1) = 4 OR rstgr+0(1) = 5 OR rstgr+0(1) = 6.
      MOVE-CORRESPONDING gt_zjjh TO gt_zjjh_mx.
      APPEND gt_zjjh_mx.CLEAR gt_zjjh_mx.
    ENDLOOP.
  ELSE.
    LOOP AT gt_zjjh WHERE rstgr = gt_xh-rstgr.
      MOVE-CORRESPONDING gt_zjjh TO gt_zjjh_mx.
      APPEND gt_zjjh_mx.CLEAR gt_zjjh_mx.
    ENDLOOP.
  ENDIF.

  IF gt_zjjh_mx[] IS INITIAL.
    RETURN.
  ENDIF.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_fieldcat TYPE lvc_s_fcat,
        lt_sort     TYPE lvc_t_sort,
        lt_events   TYPE slis_t_event,
        ls_layout   TYPE lvc_s_layo,
        ls_variant  TYPE disvariant,
        l_pos       TYPE i VALUE 0.

  DEFINE setfieldcat.
    l_pos = l_pos + 1.
    wa_fieldcat-col_pos = l_pos.
    wa_fieldcat-fieldname = &1.
    wa_fieldcat-coltext   = &2.
    wa_fieldcat-scrtext_l = &2.
    wa_fieldcat-scrtext_m = &2.
    wa_fieldcat-scrtext_s = &2.
    IF &3 NE space.
      SPLIT &3 AT '-' INTO wa_fieldcat-ref_table wa_fieldcat-ref_field.
    ENDIF.
    wa_fieldcat-outputlen = &4.  "字段长度
    wa_fieldcat-edit = &5.       "字段是否可编辑
    wa_fieldcat-key = &6.       "键值
    wa_fieldcat-hotspot = &7.       "热键
    APPEND wa_fieldcat TO lt_fieldcat. CLEAR: wa_fieldcat.
  END-OF-DEFINITION.

  ls_layout-zebra      = 'X'.
  ls_layout-sel_mode   = 'A'.
  ls_layout-cwidth_opt = 'X'.     "自动调整字段宽度
  ls_layout-box_fname  = 'SEL'.
*  ls_layout-stylefname = 'STYLE'.
  "ls_layout-totals_bef = 'X'.

  ls_variant-report = sy-repid.
  ls_variant-handle = '0003'.  "区分不同布局
  setfieldcat 'BUKRS' '公司代码' 'BKPF-BUKRS' '' '' '' ''.
  setfieldcat 'GJAHR' '计划年度' 'BKPF-GJAHR' '' '' '' ''.
  setfieldcat 'JHQJ' '计划月份' 'BKPF-MONAT' '' '' '' ''.
  setfieldcat 'BELNR' '凭证编号' 'BSEG-BELNR' '' '' '' 'X'.
  setfieldcat 'BUZEI' '行项目' 'BSEG-BUZEI' '' '' '' ''.
  setfieldcat 'H_BUDAT' '过账日期' 'BSEG-H_BUDAT' '' '' '' ''.
  setfieldcat 'NETDT' '到期日' 'BSEG-NETDT' '' '' '' ''.
  setfieldcat 'HKONT' '科目' 'BSEG-HKONT' '' '' '' ''.
  setfieldcat 'TXT50' '科目描述' '' '' '' '' ''.
  setfieldcat 'RSTGR' '原因代码' 'BSEG-RSTGR' '' '' '' ''.
  setfieldcat 'TXT40' '原因代码描述' '' '' '' '' ''.
  setfieldcat 'DMBTR' '本位币金额' 'BSEG-DMBTR' '' '' '' ''.
  setfieldcat 'SGTXT' '说明' '' '' '' '' ''.
  setfieldcat 'LIFNR' '供应商' 'BSEG-LIFNR' '' '' '' ''.
  setfieldcat 'GYSMS' '供应商名称' '' '' '' '' ''.
  setfieldcat 'CGFP' '采购发票' '' '' '' '' ''.
  setfieldcat 'KUNNR' '客户' 'BSEG-KUNNR' '' '' '' ''.
  setfieldcat 'KHMS' '客户名称' '' '' '' '' ''.
  setfieldcat 'XSFP' '销售发票' '' '' '' '' ''.
  setfieldcat 'KOSTL' '成本中心' 'CSKS-KOSTL' '' '' '' ''.
  setfieldcat 'LTEXT' '成本中心名称' '' '' '' '' ''.

*  APPEND VALUE #( name = 'CALLER_EXIT' form = 'FRM_CALLER_EXIT' ) TO lt_events.
*  APPEND VALUE #( spos = 1 fieldname = 'MATNR' up = 'X' ) TO lt_sort.

***BEGIN***单元格编辑回填数据
  DATA: i_grid_settings TYPE lvc_s_glay.
  i_grid_settings-edt_cll_cb = 'X'.
***END***单元格编辑回填数据

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program      = sy-repid
*     I_CALLBACK_PF_STATUS_SET = 'FRM_PF_STATUS'
      i_callback_user_command = 'FRM_USER_COMMAND_DETAIL1'
      it_fieldcat_lvc         = lt_fieldcat[]
*     it_events               = lt_events
*     it_sort_lvc             = lt_sort
      is_layout_lvc           = ls_layout
      is_variant              = ls_variant    "区分不同布局
      i_save                  = 'A'
      i_grid_settings         = i_grid_settings         "单元格编辑之后数据可以存入内表
    TABLES
      t_outtab                = gt_zjjh_mx[].

ENDFORM.

FORM frm_user_command_detail1 USING r_ucomm LIKE sy-ucomm rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN '&IC1'.
      IF rs_selfield-fieldname EQ 'BELNR' AND rs_selfield-value IS NOT INITIAL.
        READ TABLE gt_zjjh_mx INTO DATA(ls_alv) INDEX rs_selfield-tabindex.
        SET PARAMETER ID 'BLN' FIELD ls_alv-belnr.
        SET PARAMETER ID 'GJR' FIELD ls_alv-gjahr.
        SET PARAMETER ID 'BUK' FIELD ls_alv-bukrs.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .
      ENDIF.
  ENDCASE.

ENDFORM.

FORM frm_display_detail2.

  CLEAR gt_sjje_mx[].
  IF gt_xh-rstgr = 'A10'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_sjje_mx
    FROM bseg
    WHERE ( hkont LIKE '1001%' OR hkont LIKE '1002%' OR hkont LIKE '1012%')
    AND bukrs = p_bukrs
    AND h_monat = p_monat
    AND gjahr = p_gjahr
    AND rstgr LIKE '1%'.
  ELSEIF gt_xh-rstgr = 'A20'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_sjje_mx
    FROM bseg
    WHERE ( hkont LIKE '1001%' OR hkont LIKE '1002%' OR hkont LIKE '1012%')
    AND bukrs = p_bukrs
    AND h_monat = p_monat
    AND gjahr = p_gjahr
    AND rstgr LIKE '2%'.
  ELSEIF gt_xh-rstgr = 'A30'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_sjje_mx
    FROM bseg
    WHERE ( hkont LIKE '1001%' OR hkont LIKE '1002%' OR hkont LIKE '1012%')
    AND bukrs = p_bukrs
    AND h_monat = p_monat
    AND gjahr = p_gjahr
    AND rstgr LIKE '3%'.
  ELSEIF gt_xh-rstgr = 'A40'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_sjje_mx
    FROM bseg
    WHERE ( hkont LIKE '1001%' OR hkont LIKE '1002%' OR hkont LIKE '1012%')
    AND bukrs = p_bukrs
    AND h_monat = p_monat
    AND gjahr = p_gjahr
    AND rstgr LIKE '4%'.
  ELSEIF gt_xh-rstgr = 'A50'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_sjje_mx
    FROM bseg
    WHERE ( hkont LIKE '1001%' OR hkont LIKE '1002%' OR hkont LIKE '1012%')
    AND bukrs = p_bukrs
    AND h_monat = p_monat
    AND gjahr = p_gjahr
    AND rstgr LIKE '5%'.
  ELSEIF gt_xh-rstgr = 'A60'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_sjje_mx
    FROM bseg
    WHERE ( hkont LIKE '1001%' OR hkont LIKE '1002%' OR hkont LIKE '1012%')
    AND bukrs = p_bukrs
    AND h_monat = p_monat
    AND gjahr = p_gjahr
    AND rstgr LIKE '6%'.
  ELSEIF gt_xh-rstgr = 'Z10'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_sjje_mx
    FROM bseg
    WHERE ( hkont LIKE '1001%' OR hkont LIKE '1002%' OR hkont LIKE '1012%')
    AND bukrs = p_bukrs
    AND h_monat = p_monat
    AND gjahr = p_gjahr
    AND ( rstgr LIKE '1%' OR rstgr LIKE '2%' ).
  ELSEIF gt_xh-rstgr = 'Z20'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_sjje_mx
    FROM bseg
    WHERE ( hkont LIKE '1001%' OR hkont LIKE '1002%' OR hkont LIKE '1012%')
    AND bukrs = p_bukrs
    AND h_monat = p_monat
    AND gjahr = p_gjahr
    AND ( rstgr LIKE '3%' OR rstgr LIKE '4%' ).
  ELSEIF gt_xh-rstgr = 'Z30'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_sjje_mx
    FROM bseg
    WHERE ( hkont LIKE '1001%' OR hkont LIKE '1002%' OR hkont LIKE '1012%')
    AND bukrs = p_bukrs
    AND h_monat = p_monat
    AND gjahr = p_gjahr
    AND ( rstgr LIKE '5%' OR rstgr LIKE '6%' ).
  ELSEIF gt_xh-rstgr = 'Z50'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_sjje_mx
    FROM bseg
    WHERE ( hkont LIKE '1001%' OR hkont LIKE '1002%' OR hkont LIKE '1012%')
    AND bukrs = p_bukrs
    AND h_monat = p_monat
    AND gjahr = p_gjahr
    AND ( rstgr LIKE '1%' OR rstgr LIKE '2%' OR rstgr LIKE '3%' OR rstgr LIKE '4%'OR rstgr LIKE '5%' OR rstgr LIKE '6%' ).
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_sjje_mx
FROM bseg
WHERE ( hkont LIKE '1001%' OR hkont LIKE '1002%' OR hkont LIKE '1012%')
AND bukrs = p_bukrs
AND h_monat = p_monat
AND gjahr = p_gjahr
AND rstgr = gt_xh-rstgr.
  ENDIF.

  IF gt_sjje_mx[] IS INITIAL.
    RETURN.
  ENDIF.

  "BEGIN-获取科目描述
  DATA: BEGIN OF l_saknr OCCURS 0,
          saknr LIKE ska1-saknr,
          txt50 LIKE skat-txt50,
        END OF l_saknr.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE l_saknr
    FROM skat
    WHERE spras = '1'
    AND ktopl = '1000'.
  "END-获取科目码描述

  "BEGIN-获取原因代码描述
  DATA: BEGIN OF lt_t053s OCCURS 0,
          rstgr LIKE t053s-rstgr,
          txt40 LIKE t053s-txt40,
        END OF lt_t053s.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_t053s
    FROM t053s
    WHERE bukrs = p_bukrs
    AND spras = 1.
  "END-获取原因代码描述

  LOOP AT gt_sjje_mx.
    READ TABLE l_saknr WITH KEY saknr = gt_sjje_mx-hkont.
    IF sy-subrc = 0.
      gt_sjje_mx-txt50 = l_saknr-txt50.
    ENDIF.

    READ TABLE lt_t053s WITH KEY rstgr = gt_sjje_mx-rstgr.
    IF sy-subrc = 0.
      gt_sjje_mx-txt40 = lt_t053s-txt40.
    ENDIF.
    gt_sjje_mx-jhqj = p_monat.

    IF gt_sjje_mx-shkzg = 'H'.
      gt_sjje_mx-dmbtr = 0 - gt_sjje_mx-dmbtr.
    ENDIF.

    MODIFY gt_sjje_mx.
  ENDLOOP.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_fieldcat TYPE lvc_s_fcat,
        lt_sort     TYPE lvc_t_sort,
        lt_events   TYPE slis_t_event,
        ls_layout   TYPE lvc_s_layo,
        ls_variant  TYPE disvariant,
        l_pos       TYPE i VALUE 0.

  DEFINE setfieldcat.
    l_pos = l_pos + 1.
    wa_fieldcat-col_pos = l_pos.
    wa_fieldcat-fieldname = &1.
    wa_fieldcat-coltext   = &2.
    wa_fieldcat-scrtext_l = &2.
    wa_fieldcat-scrtext_m = &2.
    wa_fieldcat-scrtext_s = &2.
    IF &3 NE space.
      SPLIT &3 AT '-' INTO wa_fieldcat-ref_table wa_fieldcat-ref_field.
    ENDIF.
    wa_fieldcat-outputlen = &4.  "字段长度
    wa_fieldcat-edit = &5.       "字段是否可编辑
    wa_fieldcat-key = &6.       "键值
    wa_fieldcat-hotspot = &7.       "热键
    APPEND wa_fieldcat TO lt_fieldcat. CLEAR: wa_fieldcat.
  END-OF-DEFINITION.

  ls_layout-zebra      = 'X'.
  ls_layout-sel_mode   = 'A'.
  ls_layout-cwidth_opt = 'X'.     "自动调整字段宽度
  ls_layout-box_fname  = 'SEL'.
*  ls_layout-stylefname = 'STYLE'.
  "ls_layout-totals_bef = 'X'.

  ls_variant-report = sy-repid.
  ls_variant-handle = '0004'.  "区分不同布局
  setfieldcat 'BUKRS' '公司代码' 'BKPF-BUKRS' '' '' '' ''.
  setfieldcat 'GJAHR' '会计年度' 'BKPF-GJAHR' '' '' '' ''.
  setfieldcat 'JHQJ' '过账期间' 'BKPF-MONAT' '' '' '' ''.
  setfieldcat 'BELNR' '凭证编号' 'BSEG-BELNR' '' '' '' 'X'.
  setfieldcat 'BUZEI' '行项目' 'BSEG-BUZEI' '' '' '' ''.
  setfieldcat 'H_BUDAT' '过账日期' 'BSEG-H_BUDAT' '' '' '' ''.
*  SETFIELDCAT 'NETDT' '到期日' 'BSEG-NETDT' '' '' '' ''.
  setfieldcat 'HKONT' '科目' 'BSEG-HKONT' '' '' '' ''.
  setfieldcat 'TXT50' '科目描述' '' '' '' '' ''.
  setfieldcat 'RSTGR' '原因代码' 'BSEG-RSTGR' '' '' '' ''.
  setfieldcat 'TXT40' '原因代码描述' '' '' '' '' ''.
  setfieldcat 'DMBTR' '本位币金额' 'BSEG-DMBTR' '' '' '' ''.
  setfieldcat 'SGTXT' '说明' '' '' '' '' ''.
*  SETFIELDCAT 'LIFNR' '供应商' 'BSEG-LIFNR' '' '' '' ''.
*  SETFIELDCAT 'GYSMS' '供应商名称' '' '' '' '' ''.
*  SETFIELDCAT 'CGFP' '采购发票' '' '' '' '' ''.
*  SETFIELDCAT 'KUNNR' '客户' 'BSEG-KUNNR' '' '' '' ''.
*  SETFIELDCAT 'KHMS' '客户名称' '' '' '' '' ''.
*  SETFIELDCAT 'XSFP' '销售发票' '' '' '' '' ''.
*  SETFIELDCAT 'KOSTL' '成本中心' 'CSKS-KOSTL' '' '' '' ''.
*  SETFIELDCAT 'LTEXT' '成本中心名称' '' '' '' '' ''.

*  APPEND VALUE #( name = 'CALLER_EXIT' form = 'FRM_CALLER_EXIT' ) TO lt_events.
*  APPEND VALUE #( spos = 1 fieldname = 'MATNR' up = 'X' ) TO lt_sort.

***BEGIN***单元格编辑回填数据
  DATA: i_grid_settings TYPE lvc_s_glay.
  i_grid_settings-edt_cll_cb = 'X'.
***END***单元格编辑回填数据

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program      = sy-repid
*     I_CALLBACK_PF_STATUS_SET = 'FRM_PF_STATUS'
      i_callback_user_command = 'FRM_USER_COMMAND_DETAIL2'
      it_fieldcat_lvc         = lt_fieldcat[]
*     it_events               = lt_events
*     it_sort_lvc             = lt_sort
      is_layout_lvc           = ls_layout
      is_variant              = ls_variant    "区分不同布局
      i_save                  = 'A'
      i_grid_settings         = i_grid_settings         "单元格编辑之后数据可以存入内表
    TABLES
      t_outtab                = gt_sjje_mx[].

ENDFORM.

FORM frm_user_command_detail2 USING r_ucomm LIKE sy-ucomm rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN '&IC1'.
      IF rs_selfield-fieldname EQ 'BELNR' AND rs_selfield-value IS NOT INITIAL.
        READ TABLE gt_sjje_mx INTO DATA(ls_alv) INDEX rs_selfield-tabindex.
        SET PARAMETER ID 'BLN' FIELD ls_alv-belnr.
        SET PARAMETER ID 'GJR' FIELD ls_alv-gjahr.
        SET PARAMETER ID 'BUK' FIELD ls_alv-bukrs.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_EDIT_Z40
*&---------------------------------------------------------------------*
*&  获取计划、实际的期初余额
*&---------------------------------------------------------------------*
*&      --> LT_FAGL
*&      <-- GT_XH
*&---------------------------------------------------------------------*
FORM frm_edit_z40  USING    it_fagl  TYPE tt_fagl
                  CHANGING  cv_sjje  TYPE bseg-dmbtr
                            cv_jhje  TYPE bseg-dmbtr.
  DATA lv_times  TYPE i.
  DATA lv_hslxx  TYPE hslvt12.
  DATA lv_hslxx_tot  TYPE hslvt12.
  lv_times = p_monat - 1.
*****如果现在界面输入2021年1月，期初余额=HSLVT
*****如果现在界面输入2021年2月，期初余额=HSLVT+ HSL01
*****如果现在界面输入2021年3月，期初余额=HSLVT+ HSL01+ HSL02
  CASE p_monat.
    WHEN 1.
      LOOP AT it_fagl INTO DATA(ls_fagl).
        lv_hslxx = lv_hslxx + ls_fagl-hslvt.
        CLEAR ls_fagl.
      ENDLOOP.
    WHEN 2.
      LOOP AT it_fagl INTO ls_fagl .
        lv_hslxx = lv_hslxx + ls_fagl-hslvt + ls_fagl-hsl01.
        CLEAR ls_fagl.
      ENDLOOP.
    WHEN OTHERS.
      LOOP AT it_fagl INTO ls_fagl .
        DO lv_times TIMES  VARYING lv_hslxx_tot FROM ls_fagl-hsl01 NEXT ls_fagl-hsl02.
          lv_hslxx = lv_hslxx_tot + lv_hslxx .
        ENDDO.
        lv_hslxx = lv_hslxx +  ls_fagl-hslvt. "
        CLEAR ls_fagl.
      ENDLOOP.
  ENDCASE.
  cv_sjje = cv_jhje = lv_hslxx.
  CLEAR lv_hslxx_tot.
  CLEAR lv_hslxx.
  CLEAR lv_times.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_EDIT_Z50
*&---------------------------------------------------------------------*
*& 当月现金流量净额，当月计划金额=Z10+Z20+Z30+Z40
*&---------------------------------------------------------------------*
*&      <-- GT_XH_JHJE
*&---------------------------------------------------------------------*
FORM frm_edit_z50  CHANGING  cv_jhje  TYPE bseg-dmbtr.
  LOOP AT gt_xh INTO DATA(ls_xh) WHERE rstgr = 'Z10'
                                    OR rstgr = 'Z20'
                                    OR rstgr = 'Z30'
                                    OR rstgr = 'Z40'.
    cv_jhje = cv_jhje + ls_xh-jhje.
    CLEAR ls_xh.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SEL_ALL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_sel_all .
  LOOP AT gt_zjjh ASSIGNING FIELD-SYMBOL(<ls_zjjh>) WHERE sel IS INITIAL.
    <ls_zjjh>-sel = 'X'.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SEL_DALL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_sel_dall .
  LOOP AT gt_zjjh ASSIGNING FIELD-SYMBOL(<ls_zjjh>) WHERE sel IS NOT INITIAL.
    <ls_zjjh>-sel = ''.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SEL_FX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_sel_fx .
  LOOP AT gt_zjjh ASSIGNING FIELD-SYMBOL(<ls_zjjh>) .
    IF <ls_zjjh>-sel = ''.
      <ls_zjjh>-sel = 'X'.
    ELSE.
      <ls_zjjh>-sel = ''.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SAVE_SELDATA
*&---------------------------------------------------------------------*
*& 保存选中的数据
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_save_seldata .
  DATA lt_zfit0020a LIKE TABLE OF zfit0020a .
  DATA lv_num TYPE i.
  DATA lv_answer TYPE c.
  READ TABLE gt_zjjh TRANSPORTING NO FIELDS WITH KEY sel = 'X'.
  IF sy-subrc <> 0.
    MESSAGE s135(bp) WITH '请选择要保存的数据' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  LOOP AT gt_zjjh ASSIGNING FIELD-SYMBOL(<ls_zjjh>) WHERE sel = 'X'.
    lv_num = lv_num + 1.
    lt_zfit0020a = VALUE #( BASE lt_zfit0020a (
                                bukrs = p_bukrs
                                gjahr_jh = p_gjahr
                                monat = p_monat
                                docln = lv_num
                                gjahr = <ls_zjjh>-h_budat+0(4)
                                belnr = <ls_zjjh>-belnr
                                buzei = <ls_zjjh>-buzei
                                    ) ).

  ENDLOOP.

  CHECK lt_zfit0020a IS NOT INITIAL.
*  “重新获取数据 ,删除地表里面的数据，将本次选择的数据更新进去
  IF c1 = 'X'.
    CALL FUNCTION 'POPUP_TO_CONFIRM'   "弹窗提示
      EXPORTING
        text_question         = TEXT-011 "确定此次操作码？
        icon_button_1         = TEXT-012 "是
        icon_button_2         = TEXT-013 "否
        default_button        = '2'
        display_cancel_button = 'X'   "是否显示取消按钮
        start_column          = 25
        start_row             = 6
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF lv_answer <> '1'.
      RETURN.
    ENDIF.
  ENDIF.

  SELECT * INTO TABLE @DATA(lt_20)
    FROM zfit0020a
   WHERE bukrs = @p_bukrs
     AND gjahr_jh = @p_gjahr
     AND monat = @p_monat.
  IF sy-subrc = 0.
    DELETE zfit0020a FROM TABLE lt_20.
    COMMIT WORK AND WAIT.   "同步处理
    IF sy-subrc = 0.
      MODIFY  zfit0020a FROM TABLE lt_zfit0020a.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.   "同步处理
        MESSAGE '数据更新成功！' TYPE 'I'.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ELSE.
      ROLLBACK WORK.
      MESSAGE '数据更新失败！' TYPE 'I'.
    ENDIF.
  ELSE.
    MODIFY  zfit0020a FROM TABLE lt_zfit0020a.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.   "同步处理
      MESSAGE '数据更新成功！' TYPE 'I'.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
ENDFORM.