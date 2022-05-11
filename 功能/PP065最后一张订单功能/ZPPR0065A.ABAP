*&---------------------------------------------------------------------*
*& Report ZQMR0054
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr0065a MESSAGE-ID zpp.

DATA:BEGIN OF gs_zppt0065c.
       INCLUDE TYPE zppt0065c.
       DATA upd TYPE char1.
DATA END OF gs_zppt0065c.
DATA:gs_layout     TYPE lvc_s_layo,
     gs_stable     TYPE lvc_s_stbl,
     go_docking    TYPE REF TO cl_gui_docking_container,
     go_alv_grid   TYPE REF TO cl_gui_alv_grid,
     gt_alv_fidcat TYPE lvc_t_fcat,
     ok_code       TYPE sy-tcode,
     save_code     TYPE sy-tcode,
     gt_zppt0065c  LIKE TABLE OF gs_zppt0065c,
     gv_flag       TYPE flag.
FIELD-SYMBOLS:<fs_zppt0065c> LIKE  gs_zppt0065c.

DATA: lv_e_valid TYPE char1,
      lv_tabix   TYPE sy-tabix,
      lv_invalid TYPE flag,
      lv_msg     TYPE bapiret2-message,
      ls_save    TYPE zppt0065c,
      lt_save    TYPE TABLE OF zppt0065c.
DATA:lt_zppt0065b TYPE TABLE OF zppt0065b.
FIELD-SYMBOLS <fs_zppt0065b> TYPE zppt0065b.


CLASS zlcl_event_receiver DEFINITION DEFERRED.
DATA go_event_receiver  TYPE REF TO zlcl_event_receiver.
CLASS zlcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      "增加/减少 toolbar 按钮
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive,
      handle_after_user_command FOR EVENT after_user_command OF  cl_gui_alv_grid IMPORTING e_ucomm  e_saved e_not_processed,
      "ALV单元格编辑后的处理事件
      handle_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING sender e_modified et_good_cells.
ENDCLASS.
CLASS zlcl_event_receiver  IMPLEMENTATION.
  METHOD handle_after_user_command.
    DATA:lt_sel_rows TYPE lvc_t_row,
         ls_row      TYPE lvc_s_row,
         lv_index    TYPE sy-tabix,
         lr_uuid     TYPE RANGE OF zppt0065c-uuid.
    CASE e_ucomm.
      WHEN 'ADDROW'.
        APPEND INITIAL LINE TO gt_zppt0065c ASSIGNING <fs_zppt0065c>.
        CHECK <fs_zppt0065c> IS ASSIGNED.
        <fs_zppt0065c>-zcreatedate = sy-datum.
        <fs_zppt0065c>-zcreateuser = sy-uname.
        <fs_zppt0065c>-upd = 'I'.
        go_alv_grid->refresh_table_display( is_stable = gs_stable ).
      WHEN 'DELROW'.
        REFRESH lt_sel_rows.
        go_alv_grid->get_selected_rows( IMPORTING  et_index_rows = lt_sel_rows ).
        IF lt_sel_rows IS  INITIAL.
          MESSAGE s000 WITH '请选中待删除的行'.
          RETURN.
        ENDIF.
        LOOP AT lt_sel_rows INTO ls_row.
          gs_zppt0065c = gt_zppt0065c[ ls_row-index ].
          gt_zppt0065c[ ls_row-index ]-upd = 'D'."待删除标识
          IF gs_zppt0065c-uuid IS NOT INITIAL .
            lr_uuid  = VALUE #( BASE lr_uuid ( sign = 'I' option = 'EQ' low = gs_zppt0065c-uuid ) ).
          ENDIF.
        ENDLOOP.
        DELETE gt_zppt0065c WHERE upd = 'D'.
        IF lr_uuid IS NOT INITIAL.
          DELETE FROM zppt0065c WHERE uuid IN lr_uuid .
          COMMIT WORK AND WAIT .
        ENDIF.
        go_alv_grid->refresh_table_display( is_stable = gs_stable ).
        MESSAGE s000 WITH '删除成功!'.
*      WHEN 'COPYROW'.
*        REFRESH lt_sel_rows.
*        go_alv_grid->get_selected_rows( IMPORTING  et_index_rows = lt_sel_rows ).
*        IF lt_sel_rows IS  INITIAL.
*          MESSAGE s000 WITH '请选中待删除的行'.
*          RETURN.
*        ENDIF.
*        SORT lt_sel_rows BY index DESCENDING.
*        LOOP AT lt_sel_rows INTO ls_row.
*          CLEAR:lv_index, gs_zppt0065c.
*          lv_index = ls_row-index.
*          gs_zppt0065c = gt_zppt0065c[ ls_row-index ].
*          CLEAR gs_zppt0065c-uuid.
*          gs_zppt0065c-upd =  'I'.
*          INSERT gs_zppt0065c INTO  gt_zppt0065c INDEX lv_index + 1.
*        ENDLOOP.
*        go_alv_grid->refresh_table_display( is_stable = gs_stable ).
*        MESSAGE s000 WITH '复制成功!'.
    ENDCASE.

  ENDMETHOD.
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

*    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
*    <fs_button>-butn_type = '3'.  "分割符
*    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
*    <fs_button>-function = 'COPYROW'.
*    <fs_button>-icon = '@14@'.
*    <fs_button>-text = '复制行'.

*    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
*    <fs_button>-butn_type = '3'.  "分割符
*    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
*    <fs_button>-function = 'DELROW'.
*    <fs_button>-icon = '@18@'.
*    <fs_button>-text = '删除行'.

  ENDMETHOD.
  "ALV单元格编辑后的处理程序
  METHOD handle_data_changed_finished.
    DATA lv_refresh TYPE flag.
    LOOP AT et_good_cells INTO DATA(ls_good_cells) WHERE   error = ''.
      ASSIGN gt_zppt0065c[ ls_good_cells-row_id ] TO <fs_zppt0065c>.
      CHECK <fs_zppt0065c> IS ASSIGNED.
      CASE ls_good_cells-fieldname.
*        WHEN 'LIFNR'."供应商
*          CLEAR <fs_ZPPT0065C>-zlifnrname.
*          SELECT SINGLE name1,name2,name3,name4 FROM lfa1 WHERE lifnr = @<fs_ZPPT0065C>-lifnr INTO @DATA(ls_lfa1) .
*          IF sy-subrc = 0.
*            <fs_ZPPT0065C>-zlifnrname = |{ ls_lfa1-name1 }{ ls_lfa1-name2 }{ ls_lfa1-name3 }{ ls_lfa1-name4 }|.
*          ELSE.
*            CLEAR <fs_ZPPT0065C>-lifnr.
*            MESSAGE s000 WITH '请输入有效的供应商'.
*          ENDIF.
*        WHEN 'ZMFRNR'. "生产制造商
*          CLEAR <fs_ZPPT0065C>-zmfrnrname.
*          SELECT SINGLE name1,name2,name3,name4 FROM lfa1 WHERE lifnr = @<fs_ZPPT0065C>-zmfrnr INTO @ls_lfa1 .
*          IF sy-subrc = 0.
*            <fs_ZPPT0065C>-zmfrnrname = |{ ls_lfa1-name1 }{ ls_lfa1-name2 }{ ls_lfa1-name3 }{ ls_lfa1-name4 }|.
*          ELSE.
*            CLEAR <fs_ZPPT0065C>-zmfrnr.
*            MESSAGE s000 WITH '请输入有效的生产制造商'.
*          ENDIF.
        WHEN OTHERS.
      ENDCASE.
      <fs_zppt0065c>-upd = 'U'."修改标识
      <fs_zppt0065c>-zcreatedate = sy-datum.
      <fs_zppt0065c>-zcreateuser = sy-uname.
      lv_refresh = abap_true.
    ENDLOOP.
    IF lv_refresh = abap_true.
      go_alv_grid->set_frontend_layout( gs_layout ).
      CALL METHOD go_alv_grid->refresh_table_display
        EXPORTING
          is_stable = gs_stable
        EXCEPTIONS
          finished  = 1
          OTHERS    = 2.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


START-OF-SELECTION.
  "查询数据，然后把数据展示在ALV界面上
  SELECT * FROM zppt0065c INTO CORRESPONDING FIELDS OF TABLE @gt_zppt0065c.

  "把自定义表B的数据也加上，方便用户操作
  SELECT zcp,werks FROM zppt0065b AS b APPENDING CORRESPONDING FIELDS OF TABLE @gt_zppt0065c
    WHERE NOT EXISTS ( SELECT * FROM zppt0065c AS c WHERE b~werks = c~werks AND b~zcp = c~zcp ).
  SORT gt_zppt0065c BY zcp werks.
  DELETE ADJACENT DUPLICATES FROM gt_zppt0065c COMPARING ALL FIELDS.
  CALL SCREEN '9000'.
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'STATUS9000'.
  SET TITLEBAR 'MAIN9000'.
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
    CREATE OBJECT go_event_receiver.
    SET HANDLER go_event_receiver->handle_after_user_command FOR go_alv_grid.
    SET HANDLER go_event_receiver->handle_toolbar FOR go_alv_grid.
    SET HANDLER go_event_receiver->handle_data_changed_finished FOR go_alv_grid.
    go_alv_grid->set_toolbar_interactive( ).
    go_alv_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
    PERFORM frm_display_alv.
  ENDIF.
ENDMODULE.
FORM frm_display_alv.
  DATA:ls_lvc_s_fcat TYPE lvc_s_fcat.
  IF go_alv_grid IS NOT INITIAL.
*     ALV字段目录
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZPPT0065C'
      CHANGING
        ct_fieldcat            = gt_alv_fidcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    DATA(lv_line) = lines( gt_alv_fidcat ).
    "调整字段"APS资源代码","SAP资源描述"的显示位置
    LOOP AT gt_alv_fidcat ASSIGNING FIELD-SYMBOL(<fidcat>).
      CASE <fidcat>-fieldname.
        WHEN 'MANDT' OR 'UUID'.
          <fidcat>-tech = abap_true.
        WHEN 'ZCREATEUSER'.
          <fidcat>-coltext = '人员'.
          <fidcat>-edit = abap_false.
        WHEN 'ZCREATEDATE'.
          <fidcat>-coltext = '日期'.
          <fidcat>-edit = abap_false.
        WHEN 'ZLAST_SIGN'.
          <fidcat>-coltext = '最后一张订单标识'.
          <fidcat>-edit = abap_true.
        WHEN 'OLD_SIGN'.
          <fidcat>-coltext = '历史标识'.
          <fidcat>-edit = abap_false.
        WHEN 'NEW_SIGN'.
          <fidcat>-coltext = '新标识'.
          <fidcat>-edit = abap_false.
        WHEN OTHERS.
          <fidcat>-edit = abap_true.
      ENDCASE.
    ENDLOOP.
    "增加一个字段
    ls_lvc_s_fcat-fieldname = 'UPD'.
    ls_lvc_s_fcat-col_pos = lv_line.
    ls_lvc_s_fcat-tech = ''.
    ls_lvc_s_fcat-datatype = 'CHAR'.
    ls_lvc_s_fcat-inttype = 'C'.
    ls_lvc_s_fcat-intlen = '1'.
    ls_lvc_s_fcat-coltext = '操作标识'.
    ls_lvc_s_fcat-scrtext_l = '操作标识'.
    ls_lvc_s_fcat-scrtext_m = '操作标识'.
    ls_lvc_s_fcat-scrtext_s = '操作标识'.
    APPEND ls_lvc_s_fcat TO gt_alv_fidcat.
    gs_stable-row = abap_true.
    gs_stable-col = abap_true.
    gs_layout-cwidth_opt = abap_true.
    gs_layout-sel_mode = 'D'.

    "显示ALV
    CALL METHOD go_alv_grid->set_table_for_first_display
      EXPORTING
        i_save                        = 'A'
        i_default                     = 'X'
        is_variant                    = VALUE #( report = sy-repid )
        is_layout                     = gs_layout
      CHANGING
        it_outtab                     = gt_zppt0065c
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
      PERFORM frm_save_data CHANGING gv_flag.
      CALL METHOD go_alv_grid->refresh_table_display( is_stable = gs_stable ).
      IF gv_flag = abap_true.
        MESSAGE s000 WITH '保存成功'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
FORM frm_save_data CHANGING cv_flag TYPE flag.



  CLEAR  cv_flag.
  "执行ALV数据有效性检查
  go_alv_grid->check_changed_data( IMPORTING e_valid = lv_e_valid ).
  IF lv_e_valid = ''.
    RETURN.
  ENDIF.
  LOOP AT gt_zppt0065c ASSIGNING <fs_zppt0065c> WHERE upd IS NOT INITIAL OR zcreateuser IS INITIAL.
    CLEAR: lv_invalid,lv_msg,ls_save.
    lv_tabix = sy-tabix.
    IF <fs_zppt0065c>-uuid IS INITIAL.
      <fs_zppt0065c>-uuid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
    ENDIF.
    IF <fs_zppt0065c>-zcp IS INITIAL.
      lv_msg = |{ '第' }{ lv_tabix }{ '行的物料代码不能为空' }|.
      lv_invalid  = abap_true.
    ENDIF.
    IF <fs_zppt0065c>-werks IS INITIAL.
      lv_msg = |{ '第' }{ lv_tabix }{ '行的工厂代码不能为空' }|.
      lv_invalid  = abap_true.
    ENDIF.

    "自动带出数据
    <fs_zppt0065c>-zcreatedate = sy-datum.
    <fs_zppt0065c>-zcreateuser = sy-uname.


    IF lv_invalid  = abap_true. .
      REFRESH lt_save.
      EXIT.
    ELSE.
      MOVE-CORRESPONDING <fs_zppt0065c> TO ls_save.
      APPEND ls_save TO lt_save.
    ENDIF.
  ENDLOOP.
  IF lv_invalid = abap_true.
    MESSAGE s000 WITH lv_msg.
    RETURN.
  ENDIF.
  IF lt_save IS NOT INITIAL.
    UPDATE zppt0065c FROM TABLE lt_save.
    "更新自建表B的数据，存时修改自建B表中的所有成品物料、工厂的数据中的“最后一张订单“标识为“Y”
    IF sy-subrc = 0.
      PERFORM fm_zppt0065b.
    ELSE.
      SORT gt_zppt0065c BY zcp werks.
      DELETE ADJACENT DUPLICATES FROM gt_zppt0065c COMPARING zcp werks.
      IF sy-subrc = 0.
        MESSAGE '存在重复的数据，请处理' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      MODIFY zppt0065c FROM TABLE lt_save.
      IF sy-subrc = 0.
        PERFORM fm_zppt0065b.
      ENDIF.
    ENDIF.
    LOOP AT gt_zppt0065c ASSIGNING <fs_zppt0065c> WHERE upd IS NOT INITIAL.
      CLEAR <fs_zppt0065c>-upd.
    ENDLOOP.
    cv_flag = abap_true.
  ELSE.
    MESSAGE s000 WITH '没有变更的数据，不需要保存!'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000_exit INPUT.
  save_code = ok_code.
  CLEAR ok_code.
  CASE save_code.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

FORM fm_zppt0065b.
  SELECT
  *
  FROM zppt0065b AS b
  INTO TABLE @lt_zppt0065b
  FOR ALL ENTRIES IN @lt_save
  WHERE zcp = @lt_save-zcp
    AND werks = @lt_save-werks.
  IF lt_zppt0065b IS NOT INITIAL.

    LOOP AT lt_zppt0065b ASSIGNING <fs_zppt0065b>.
      READ TABLE lt_save INTO ls_save WITH KEY zcp = <fs_zppt0065b>-zcp
                                               werks = <fs_zppt0065b>-werks.
      IF sy-subrc = 0.
        <fs_zppt0065b>-zlast_sign = 'Y'.
      ENDIF.

    ENDLOOP.
    MODIFY zppt0065b FROM TABLE lt_zppt0065b.
  ENDIF.
ENDFORM.