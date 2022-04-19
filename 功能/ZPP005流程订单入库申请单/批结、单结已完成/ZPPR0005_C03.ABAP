*&---------------------------------------------------------------------*
*& 包含               ZPPR0005_C03
*&---------------------------------------------------------------------*
CLASS zcl_qrmail_oper DEFINITION.
    PUBLIC SECTION.
      METHODS:
        display_alv,
        save_mail.
    PRIVATE SECTION.
      METHODS:
        handle_after_user_command FOR EVENT after_user_command OF  cl_gui_alv_grid IMPORTING e_ucomm  e_saved e_not_processed,
        "LINK_CLICK事件处理程序
        "增加/减少 toolbar 按钮
        handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive,
        "ALV单元格编辑后的处理事件
        handle_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING sender e_modified et_good_cells.
  ENDCLASS.
  CLASS zcl_qrmail_oper IMPLEMENTATION.
    "alv的toolbar按钮增加删除实现
    METHOD handle_toolbar.
      "删除不需要的按钮
      DELETE e_object->mt_toolbar
       WHERE function = '&&SEP00'
         OR function = '&REFRESH'
         OR function = '&LOCAL&APPEND'
         OR function = '&LOCAL&UNDO'
         OR function ='&LOCAL&INSERT_ROW'
         OR function ='&LOCAL&DELETE_ROW'
         OR function = '&LOCAL&COPY_ROW'
         OR function = '&&SEP01'
         OR function = '&LOCAL&CUT'
         OR function = '&LOCAL&COPY'
         OR function = '&LOCAL&PASTE'
         OR function = '&LOCAL&UNDO'
         OR function = '&&SEP02'.
      "增加一个新按钮
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_button>).
      <fs_button>-butn_type = '3'.  "分割符
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-function = 'ADDROW'.
      <fs_button>-icon = '@17@'.
      <fs_button>-text = '插入行'.
  
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-butn_type = '3'.  "分割符
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-function = 'DELROW'.
      <fs_button>-icon = '@18@'.
      <fs_button>-text = '删除行'.
    ENDMETHOD.
    "ALV单元格编辑后的处理程序
    METHOD handle_data_changed_finished.
      DATA lv_refresh_flag TYPE flag.
      LOOP AT et_good_cells INTO DATA(ls_good_cells) WHERE   error = ''.
        ASSIGN gt_qrmail[ ls_good_cells-row_id ] TO <fs_qrmail>.
        CHECK <fs_qrmail> IS ASSIGNED.
        CASE ls_good_cells-fieldname.
          WHEN 'WERKS'.
            SELECT SINGLE werks INTO @DATA(lv_werks) FROM t001w WHERE werks = @ls_good_cells-value.
            IF sy-subrc <> 0.
              CLEAR <fs_qrmail>-werks.
              MESSAGE s000 WITH '输入的工厂无效'.
            ENDIF.
            lv_refresh_flag = abap_true.
            <fs_qrmail>-zupd = abap_true."修改标识
          WHEN 'SMTP_ADDR' OR 'ZSENDHG' OR 'ZSENDSP' OR 'ZSENDSC'.
            <fs_qrmail>-zupd = abap_true."修改标识
            lv_refresh_flag = abap_true.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
      IF lv_refresh_flag = abap_true.
        go_alv_grid_mail->set_frontend_layout( gs_layout_bill ).
        CALL METHOD go_alv_grid_mail->refresh_table_display
          EXPORTING
            is_stable = gs_stable
          EXCEPTIONS
            finished  = 1
            OTHERS    = 2.
      ENDIF.
    ENDMETHOD.
    METHOD display_alv.
      IF go_docking_mail IS INITIAL.
        CREATE OBJECT go_docking_mail
          EXPORTING
  *         parent                      =
            repid                       = sy-repid
            dynnr                       = sy-dynnr
            extension                   = 2000 "宽度
  *         side                        = cl_gui_docking_container=>dock_at_bottom
  *         extension                   = 50
  *         style                       =
  *         lifetime                    = lifetime_default
  *         caption                     = ''
  *         metric                      = 0
  *         ratio                       = 85 "容器离屏幕top的外边距比率
  *         no_autodef_progid_dynnr     =
  *         name                        =
          EXCEPTIONS
            cntl_error                  = 1
            cntl_system_error           = 2
            create_error                = 3
            lifetime_error              = 4
            lifetime_dynpro_dynpro_link = 5
            OTHERS                      = 6.
        go_alv_grid_mail  = NEW cl_gui_alv_grid( i_parent = go_docking_mail ).
  
  *     alv字段目录
        CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
          EXPORTING
            i_structure_name       = 'ZPPT0009'
          CHANGING
            ct_fieldcat            = gt_alv_fidcat_mail
          EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.
        LOOP AT gt_alv_fidcat_mail ASSIGNING FIELD-SYMBOL(<fidcat>).
          CASE <fidcat>-fieldname.
            WHEN 'MANDT'.
              <fidcat>-tech = abap_true.
            WHEN 'WERKS' OR 'SMTP_ADDR'.
              <fidcat>-edit = abap_true.
            WHEN 'ZSENDHG' OR 'ZSENDSP' OR 'ZSENDSC'.
              <fidcat>-edit = abap_true.
              <fidcat>-checkbox = abap_true.
          ENDCASE.
        ENDLOOP.
        "注册编辑事件，使ALV编辑后回车，或离开焦点时可以进行数据校验
        SET HANDLER go_qrmail_oper->handle_toolbar FOR go_alv_grid_mail.
        SET HANDLER go_qrmail_oper->handle_data_changed_finished FOR go_alv_grid_mail.
        SET HANDLER go_qrmail_oper->handle_after_user_command FOR go_alv_grid_mail.
        go_alv_grid_mail->set_toolbar_interactive( ).
        go_alv_grid_mail->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
      ENDIF.
      SELECT * FROM zppt0009 INTO CORRESPONDING FIELDS OF TABLE gt_qrmail.
      "显示ALV
      CALL METHOD go_alv_grid_mail->set_table_for_first_display
        EXPORTING
          i_save                        = 'A'
          i_default                     = 'X'
          is_variant                    = VALUE #( report = sy-repid )
          is_layout                     = gs_layout_bill
        CHANGING
          it_outtab                     = gt_qrmail
          it_fieldcatalog               = gt_alv_fidcat_mail
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
  
    ENDMETHOD.
    METHOD save_mail.
      DATA: lv_e_valid  TYPE flag,
            ls_zppt0009 TYPE zppt0009.
      "执行ALV数据有效性检查
      go_alv_grid_mail->check_changed_data( IMPORTING e_valid = lv_e_valid ).
      IF lv_e_valid = ''.
        RETURN.
      ENDIF.
      REFRESH gt_zppt0009_save.
      LOOP AT gt_qrmail INTO gs_qrmail WHERE zupd = abap_true.
        IF gs_qrmail-werks IS INITIAL OR gs_qrmail-smtp_addr IS INITIAL.
          MESSAGE s000 WITH |{ '第' }{ sy-tabix }{ '行的工厂或邮箱地址不能为空' }|.
          EXIT.
        ENDIF.
        MOVE-CORRESPONDING gs_qrmail TO ls_zppt0009.
        APPEND ls_zppt0009 TO gt_zppt0009_save.
      ENDLOOP.
      IF gt_zppt0009_save IS INITIAL.
        MESSAGE s000 WITH '没有数据更改过'.
        RETURN.
      ENDIF.
      MODIFY zppt0009 FROM TABLE gt_zppt0009_save.
      COMMIT WORK.
      IF sy-subrc = 0.
        MESSAGE s000 WITH '保存成功'.
      ELSE.
        MESSAGE s000 WITH '保存失败'.
      ENDIF.
    ENDMETHOD.
    METHOD handle_after_user_command.
      DATA:lt_sel_rows     TYPE lvc_t_row,
           ls_zppt0009     TYPE zppt0009,
           lt_zppt0009_del TYPE TABLE OF zppt0009.
      CASE e_ucomm.
        WHEN 'ADDROW'.
          APPEND INITIAL LINE TO gt_qrmail.
          go_alv_grid_mail->refresh_table_display( is_stable = gs_stable ).
        WHEN 'DELROW'.
          REFRESH lt_sel_rows.
          go_alv_grid_mail->get_selected_rows( IMPORTING  et_index_rows = lt_sel_rows ).
          IF lt_sel_rows IS  INITIAL.
            MESSAGE s000 WITH '请选中待删除的行'.
            RETURN.
          ENDIF.
  
          LOOP AT lt_sel_rows INTO DATA(ls_row).
            ASSIGN  gt_qrmail[ ls_row-index ] TO <fs_qrmail>.
            CHECK <fs_qrmail> IS ASSIGNED.
            <fs_qrmail>-zupd = 'D'."删除标识
            IF <fs_qrmail>-werks IS NOT INITIAL AND <fs_qrmail>-smtp_addr IS NOT INITIAL.
              MOVE-CORRESPONDING <fs_qrmail> TO ls_zppt0009.
              APPEND ls_zppt0009 TO lt_zppt0009_del.
            ENDIF.
          ENDLOOP.
          IF lt_zppt0009_del IS NOT INITIAL.
            DELETE zppt0009 FROM TABLE lt_zppt0009_del.
            COMMIT WORK.
          ENDIF.
          IF sy-subrc <> 0.
            MESSAGE s000 WITH '删除数据库表失败'.
            RETURN.
          ENDIF.
          DELETE gt_qrmail WHERE zupd = 'D'.
          go_alv_grid_mail->refresh_table_display( is_stable = gs_stable ).
          MESSAGE s000 WITH '删除成功!'.
      ENDCASE.
    ENDMETHOD.
  ENDCLASS.