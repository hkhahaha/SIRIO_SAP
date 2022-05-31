*&---------------------------------------------------------------------*
*& 包含               ZPPR0028F03
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_DISPLAY_MAINTAIN_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_maintain_alv .
    CASE 'X'.
      WHEN p_halb."维护工厂物料组生产时间
        PERFORM frm_get_zppt0006.
      WHEN p_time."维护工厂物料准备时间
        PERFORM frm_get_zppt0007.
      WHEN OTHERS.
    ENDCASE.
    CALL SCREEN '9000'.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_ZPPT0006
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_get_zppt0006 .
    REFRESH gt_zppt0006.
    SELECT z6~werks,z6~matkl,t0~wgbez,z6~zdzeit,z6~erdat,z6~erzet,z6~uname
      FROM zppt0006 AS z6
      LEFT JOIN t023t AS t0 ON z6~matkl = t0~matkl AND t0~spras = @sy-langu
      WHERE z6~werks IN @s_werks
      AND z6~matkl IN @s_matkl
      INTO CORRESPONDING FIELDS OF TABLE @gt_zppt0006.
  ENDFORM.
  FORM frm_get_zppt0007 .
    REFRESH gt_zppt0007.
    SELECT z7~werks,z7~matnr,t0~maktx,z7~zpretime,z7~erdat,z7~erzet,z7~uname
      FROM zppt0007 AS z7
      LEFT JOIN makt AS t0 ON z7~matnr = t0~matnr AND t0~spras = @sy-langu
      WHERE z7~werks IN @s_werks
      AND z7~matnr IN @s_matnr
      INTO CORRESPONDING FIELDS OF TABLE @gt_zppt0007.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Module STATUS_9000 OUTPUT
  *&---------------------------------------------------------------------*
  *&
  *&---------------------------------------------------------------------*
  MODULE status_9000 OUTPUT.
    SET PF-STATUS 'STATUS9000'.
    IF go_docking IS NOT BOUND.
      CREATE OBJECT go_docking
        EXPORTING
  *       parent                      =
          repid                       = sy-repid
          dynnr                       = sy-dynnr
          extension                   = 2000 "宽度
  *       side                        = cl_gui_docking_container=>dock_at_bottom
  *       extension                   = 50
  *       style                       =
  *       lifetime                    = lifetime_default
  *       caption                     = ''
  *       metric                      = 0
  *       ratio                       = 85 "容器离屏幕top的外边距比率
  *       no_autodef_progid_dynnr     =
  *       name                        =
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.
      go_alv_grid = NEW cl_gui_alv_grid( i_parent = go_docking ).
      "注册编辑事件，使ALV编辑后回车，或离开焦点时可以进行数据校验
  
      SET HANDLER go_event_receiver->handle_after_user_command FOR go_alv_grid.
      SET HANDLER go_event_receiver->handle_toolbar FOR go_alv_grid.
      SET HANDLER go_event_receiver->handle_data_changed_finished FOR go_alv_grid.
      go_alv_grid->set_toolbar_interactive( ).
      go_alv_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
  
      IF p_halb = 'X'."工厂物料组生产时间维护
        SET TITLEBAR 'MAIN9001'.
        PERFORM frm_crt_zppt0006_alv.
      ELSEIF p_roh = 'X'."工厂原辅料准备时间维护
        SET TITLEBAR 'MAIN9002'.
        PERFORM frm_crt_zppt0007_alv.
      ENDIF.
    ENDIF.
  ENDMODULE.
  *&---------------------------------------------------------------------*
  *& Form FRM_CREATE_GRID_ALV
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_crt_zppt0006_alv.
    IF go_alv_grid IS NOT INITIAL.
  
  *     ALV字段目录
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'ZSPPT0006'
        CHANGING
          ct_fieldcat            = gt_alv_fidcat
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.
      "调整字段"APS资源代码","SAP资源描述"的显示位置
      LOOP AT gt_alv_fidcat ASSIGNING FIELD-SYMBOL(<fidcat>).
        CASE <fidcat>-fieldname.
          WHEN 'WERKS' OR  'MATKL' OR 'ZDZEIT'.
            <fidcat>-edit = abap_true.
          WHEN 'ZUPD'.
            <fidcat>-tech = abap_true.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
  
      "显示ALV
      CALL METHOD go_alv_grid->set_table_for_first_display
        EXPORTING
          i_save                        = 'A'
          i_default                     = 'X'
          is_variant                    = VALUE #( report = sy-repid )
          is_layout                     = gs_layout
        CHANGING
          it_outtab                     = gt_zppt0006
          it_fieldcatalog               = gt_alv_fidcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
    ENDIF.
  ENDFORM.
  FORM frm_crt_zppt0007_alv.
    IF go_alv_grid IS NOT INITIAL.
  
  *     ALV字段目录
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'ZSPPT0007'
        CHANGING
          ct_fieldcat            = gt_alv_fidcat
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.
      "调整字段"APS资源代码","SAP资源描述"的显示位置
      LOOP AT gt_alv_fidcat ASSIGNING FIELD-SYMBOL(<fidcat>).
        CASE <fidcat>-fieldname.
          WHEN 'WERKS' OR  'MATNR' OR 'ZPRETIME'.
            <fidcat>-edit = abap_true.
          WHEN 'ZUPD'.
            <fidcat>-tech = abap_true.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
  *  gs_layout-grid_title = '工厂物料组生产日期维护'.
      "显示ALV
      CALL METHOD go_alv_grid->set_table_for_first_display
        EXPORTING
          i_save                        = 'A'
          i_default                     = 'X'
          is_variant                    = VALUE #( report = sy-repid )
          is_layout                     = gs_layout
        CHANGING
          it_outtab                     = gt_zppt0007
          it_fieldcatalog               = gt_alv_fidcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
    ENDIF.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *&      Module  USER_COMMAND_9000  INPUT
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  MODULE user_command_9000 INPUT.
    save_code = ok_code.
    CLEAR ok_code.
    CASE save_code.
      WHEN '&SAVE'.
        IF p_halb = 'X'.                                      "保存到ZPPT0006
          PERFORM frm_save_zppt0006.
        ELSEIF p_roh = 'X'.                                   "保存到ZPPT0007
          PERFORM frm_save_zppt0007.
        ENDIF.
        CALL METHOD go_alv_grid->refresh_table_display( is_stable = gs_stable ).
  
      WHEN OTHERS.
    ENDCASE.
  
  ENDMODULE.
  *&---------------------------------------------------------------------*
  *&      Module  USER_COMMAND_9000_EXIT  INPUT
  *&---------------------------------------------------------------------*
  *       text
  *----------------------------------------------------------------------*
  MODULE user_command_9000_exit INPUT.
    DATA: lv_answer TYPE char10.
    save_code = ok_code.
    CLEAR ok_code.
    CASE save_code.
      WHEN 'BACK'.
  *      CALL FUNCTION 'POPUP_TO_CONFIRM'
  *        EXPORTING
  *          titlebar              = '退出提示'
  **         DIAGNOSE_OBJECT       = ' '
  *          text_question         = '是否退回到上一个屏幕'
  **         TEXT_BUTTON_1         = 'Ja'(001)
  **         ICON_BUTTON_1         = ' '
  **         TEXT_BUTTON_2         = 'Nein'(002)
  **         ICON_BUTTON_2         = ' '
  **         DEFAULT_BUTTON        = '1'
  *          display_cancel_button = ''
  **         USERDEFINED_F1_HELP   = ' '
  **         START_COLUMN          = 25
  **         START_ROW             = 6
  **         POPUP_TYPE            =
  **         IV_QUICKINFO_BUTTON_1 = ' '
  **         IV_QUICKINFO_BUTTON_2 = ' '
  *        IMPORTING
  *          answer                = lv_answer.
  *
  *      IF lv_answer = '1'.
  *        LEAVE TO SCREEN 0.
  *      ENDIF.
        LEAVE TO SCREEN 0.
      WHEN OTHERS.
    ENDCASE.
  ENDMODULE.
  *&---------------------------------------------------------------------*
  *& Form FRM_CHECK_ZPPT0006
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_save_zppt0006 .
    DATA: lv_e_valid       TYPE char1,
          ls_zppt0006_save TYPE zppt0006,
          lt_zppt0006_save TYPE TABLE OF zppt0006,
          lv_tabix         TYPE sy-tabix,
          lv_error         TYPE char1.
    "执行ALV数据有效性检查
    go_alv_grid->check_changed_data( IMPORTING e_valid = lv_e_valid ).
    IF lv_e_valid = ''.
      RETURN.
    ENDIF.
    "需要保存的修改记录
    LOOP AT gt_zppt0006 ASSIGNING FIELD-SYMBOL(<fs_zppt0006>) WHERE upd = abap_true.
      lv_tabix = sy-tabix.
      CLEAR lv_error.
      IF <fs_zppt0006>-werks IS INITIAL OR <fs_zppt0006>-matkl  IS INITIAL OR <fs_zppt0006>-zdzeit IS INITIAL.
        lv_error = abap_true.
        EXIT.
      ENDIF.
      <fs_zppt0006>-uname = sy-uname.
      <fs_zppt0006>-erdat = sy-datum.
      <fs_zppt0006>-erzet = sy-uzeit.
      MOVE-CORRESPONDING <fs_zppt0006> TO ls_zppt0006_save.
      APPEND ls_zppt0006_save TO lt_zppt0006_save.
    ENDLOOP.
    IF lv_error = abap_true.
      MESSAGE s000 WITH |{ '第' }{ lv_tabix }{ '行的' }{ '工厂、物料组、生产时间必须输入' }| DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    IF lt_zppt0006_save IS INITIAL .
      MESSAGE s000 WITH '未发现被修改的数据,无需保存!' .
      RETURN.
    ENDIF.
    "保存修改记录到ZPPT0006
    MODIFY zppt0006 FROM TABLE lt_zppt0006_save.
    COMMIT WORK AND WAIT.
    IF sy-subrc = 0.
      LOOP AT gt_zppt0006 ASSIGNING <fs_zppt0006>.
        CLEAR <fs_zppt0006>-upd.
      ENDLOOP.
      MESSAGE s000 WITH '保存成功!'.
    ENDIF.
  ENDFORM.
  FORM frm_save_zppt0007 .
    DATA: lv_e_valid       TYPE char1,
          ls_zppt0007_save TYPE zppt0007,
          lt_zppt0007_save TYPE TABLE OF zppt0007,
          lv_tabix         TYPE sy-tabix,
          lv_error         TYPE char1.
    "执行ALV数据有效性检查
    go_alv_grid->check_changed_data( IMPORTING e_valid = lv_e_valid ).
    IF lv_e_valid = ''.
      RETURN.
    ENDIF.
    "需要保存的修改记录
    LOOP AT gt_zppt0007 ASSIGNING FIELD-SYMBOL(<fs_zppt0007>) WHERE upd = abap_true.
      lv_tabix = sy-tabix.
      CLEAR lv_error.
      IF <fs_zppt0007>-werks IS INITIAL OR <fs_zppt0007>-matnr IS INITIAL OR <fs_zppt0007>-zpretime IS INITIAL.
        lv_error = abap_true.
        EXIT.
      ENDIF.
      <fs_zppt0007>-uname = sy-uname.
      <fs_zppt0007>-erdat = sy-datum.
      <fs_zppt0007>-erzet = sy-uzeit.
      MOVE-CORRESPONDING <fs_zppt0007> TO ls_zppt0007_save.
      APPEND ls_zppt0007_save TO lt_zppt0007_save.
    ENDLOOP.
    IF lv_error = abap_true.
      MESSAGE s000 WITH |{ '第' }{ lv_tabix }{ '行的' }{ '工厂、物料编号、准备时间必须输入' }| DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    IF lt_zppt0007_save IS INITIAL .
      MESSAGE s000 WITH '未发现被修改的数据,无需保存!' .
      RETURN.
    ENDIF.
    "保存修改记录到ZPPT0006
    MODIFY zppt0007 FROM TABLE lt_zppt0007_save.
    COMMIT WORK AND WAIT.
    IF sy-subrc = 0.
      LOOP AT gt_zppt0007 ASSIGNING <fs_zppt0007> WHERE upd = abap_true.
        CLEAR <fs_zppt0007>-upd.
      ENDLOOP.
      MESSAGE s000 WITH '保存成功!'.
    ENDIF.
  ENDFORM.