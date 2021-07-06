*&---------------------------------------------------------------------*
*& 包含               ZPPR0058_C01
*&---------------------------------------------------------------------*
CLASS zcl_event_receiver DEFINITION.
    PUBLIC SECTION.
      METHODS:
        handle_after_user_command FOR EVENT after_user_command OF  cl_gui_alv_grid IMPORTING e_ucomm  e_saved e_not_processed,
        "LINK_CLICK事件处理程序
        handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive.
    PRIVATE SECTION.
      METHODS:
        save_bill.
  ENDCLASS.
  CLASS zcl_event_receiver IMPLEMENTATION.
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
  *    "增加一个新按钮
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_button>).
      <fs_button>-butn_type = '3'.  "分割符
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-function = 'ZSAVE'.
      <fs_button>-icon = '@2L@'.
      <fs_button>-text = '保存'.
  *
  *    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
  *    <fs_button>-butn_type = '3'.  "分割符
  *    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
  *    <fs_button>-function = 'DELROW'.
  *    <fs_button>-icon = '@18@'.
  *    <fs_button>-text = '删除行'.
    ENDMETHOD.
    METHOD handle_after_user_command.
      CASE e_ucomm.
        WHEN 'ZSAVE'.
          save_bill( ).
        WHEN OTHERS.
      ENDCASE.
    ENDMETHOD.
    METHOD save_bill.
      DATA: lv_e_valid  TYPE flag,
            ls_zppt0012 TYPE zppt0012,
            lt_zppt0012 TYPE TABLE OF zppt0012.
      "执行ALV数据有效性检查
      go_alv_grid->check_changed_data( IMPORTING e_valid = lv_e_valid ).
      IF lv_e_valid = ''.
        RETURN.
      ENDIF.
      LOOP AT gt_pro_sl INTO DATA(ls_pro) WHERE znote IS NOT INITIAL.
        CHECK ls_pro-aufnr IS NOT INITIAL.
        CLEAR ls_zppt0012.
        ls_zppt0012-aufnr = ls_pro-aufnr.
        ls_zppt0012-znote = ls_pro-znote.
        APPEND ls_zppt0012 TO lt_zppt0012.
      ENDLOOP.
      IF lt_zppt0012 IS NOT INITIAL.
        MODIFY zppt0012 FROM TABLE lt_zppt0012.
        COMMIT WORK.
        "go_alv_grid->refresh_table_display( is_stable = gs_stable ).
        IF sy-subrc = 0.
          MESSAGE s000 WITH '保存成功'.
        ELSE.
          MESSAGE s000 WITH '保存失败'.
        ENDIF.
      ENDIF.
    ENDMETHOD.
  
  ENDCLASS.
  DATA go_cl_event_receiver TYPE REF TO zcl_event_receiver.