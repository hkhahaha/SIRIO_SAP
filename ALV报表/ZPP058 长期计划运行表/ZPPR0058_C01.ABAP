*&---------------------------------------------------------------------*
*& 包含               ZPPR0058_C01
*&---------------------------------------------------------------------*
CLASS zcl_wh_event_receiver DEFINITION.
    PUBLIC SECTION.
      METHODS:
        handle_after_user_command FOR EVENT after_user_command OF  cl_gui_alv_grid IMPORTING e_ucomm  e_saved e_not_processed,
        "LINK_CLICK事件处理程序
        "增加/减少 toolbar 按钮
        handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive,
        "ALV单元格编辑后的处理事件
        handle_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING sender e_modified et_good_cells.
  ENDCLASS.
  CLASS zcl_wh_event_receiver IMPLEMENTATION.
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
        ASSIGN gt_zppt0011[ ls_good_cells-row_id ] TO <fs_zppt0011>.
        CHECK <fs_zppt0011> IS ASSIGNED.
        CASE ls_good_cells-fieldname.
          WHEN 'WERKS'.
            SELECT SINGLE werks INTO @DATA(lv_werks) FROM t001w WHERE werks = @ls_good_cells-value.
            IF sy-subrc <> 0.
              CLEAR <fs_zppt0011>-werks.
              MESSAGE s000 WITH '输入的工厂无效'.
            ENDIF.
            lv_refresh_flag = abap_true.
          WHEN 'LGORT' OR  'Z010' OR 'Z030' OR 'Z050'.
            lv_refresh_flag = abap_true.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
      IF lv_refresh_flag = abap_true.
        go_alv_grid_wh->set_frontend_layout( gs_layout ).
        CALL METHOD go_alv_grid_wh->refresh_table_display
          EXPORTING
            is_stable = gs_stable
          EXCEPTIONS
            finished  = 1
            OTHERS    = 2.
      ENDIF.
    ENDMETHOD.
  
    METHOD handle_after_user_command.
      DATA:lt_sel_rows     TYPE lvc_t_row,
           lt_zppt0011_del TYPE TABLE OF zppt0011.
      CASE e_ucomm.
        WHEN 'ADDROW'.
          APPEND INITIAL LINE TO gt_zppt0011.
          go_alv_grid_wh->refresh_table_display( is_stable = gs_stable ).
        WHEN 'DELROW'.
          REFRESH lt_sel_rows.
          go_alv_grid_wh->get_selected_rows( IMPORTING  et_index_rows = lt_sel_rows ).
          IF lt_sel_rows IS  INITIAL.
            MESSAGE s000 WITH '请选中待删除的行'.
            RETURN.
          ENDIF.
  
          LOOP AT lt_sel_rows INTO DATA(ls_row).
            ASSIGN  gt_zppt0011[ ls_row-index ] TO <fs_zppt0011>.
            CHECK <fs_zppt0011> IS ASSIGNED.
            IF <fs_zppt0011>-uuid IS NOT INITIAL.
              APPEND <fs_zppt0011> TO lt_zppt0011_del.
            ENDIF.
          ENDLOOP.
          IF lt_zppt0011_del IS NOT INITIAL.
            DELETE zppt0011 FROM TABLE lt_zppt0011_del.
            COMMIT WORK.
          ENDIF.
          IF sy-subrc <> 0.
            MESSAGE s000 WITH '删除数据库表失败'.
            RETURN.
          ENDIF.
          LOOP AT lt_sel_rows INTO ls_row.
            READ TABLE gt_zppt0011 INDEX ls_row-index ASSIGNING <fs_zppt0011>.
            IF sy-subrc = 0.
              <fs_zppt0011>-z010 = 'D'.
            ENDIF.
          ENDLOOP.
          DELETE gt_zppt0011 WHERE z010 = 'D'.
          go_alv_grid_wh->refresh_table_display( is_stable = gs_stable ).
          MESSAGE s000 WITH '删除成功!'.
      ENDCASE.
    ENDMETHOD.
  ENDCLASS.
  DATA go_cl_wh_event_receiver TYPE REF TO zcl_wh_event_receiver.