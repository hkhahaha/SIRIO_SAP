*&---------------------------------------------------------------------*
*& 包含               ZCOR0001_C01
*&---------------------------------------------------------------------*

"事件监听器（本地类）
CLASS lcl_event_receiver DEFINITION .
    PUBLIC SECTION.
      "处理ALV数据改动事件
      METHODS : handle_data_changed_finished
                  FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING sender e_modified et_good_cells,
        handle_data_changed_fis_del FOR EVENT data_changed_finished OF cl_gui_alv_grid
          IMPORTING sender e_modified et_good_cells.
      "增加/减少 toolbar 按钮
      METHODS : handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive.
        "handle_toolbar_cancel FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING  e_object e_interactive,
        "handle_toolbar_detail FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive.
      "处理ALV按钮用户事件
      METHODS : handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
        "handle_user_command_del FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
        "handle_user_command_cal FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
      "处理ALV doubleclick单击事件
      METHODS : handle_double_click
                  FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.
      "处理ALV hotspot_click单击事件
      METHODS: handle_hotspot_click FOR EVENT hotspot_click OF  cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no,
  
        handle_hotspot_click_cal FOR EVENT hotspot_click OF  cl_gui_alv_grid
          IMPORTING e_row_id e_column_id es_row_no.
  
      "处理ALV ONF4搜索帮助事件
      METHODS:handle_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.
      "处理dialogbox_container关闭事件
      METHODS: handle_dialogbox_con_colse FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender.
    PRIVATE SECTION.
      DATA dialogbox_status TYPE c. " 'X': does exist, SPACE: does not exist
  
  
  ENDCLASS.                    "handle_event definition
  
  
  "事件监听类方法实现
  CLASS lcl_event_receiver IMPLEMENTATION.
    "MAIN ALV数据改动事件实现
    METHOD handle_data_changed_finished.
  *    修改待确认的数量时，联动计算处理标准工时、机时
  *   待确认工时ZISM01 = 标准值VGW01 / 基本数量BMSCH * 待确认数量ZMNGA
      DATA lv_refresh TYPE char1.
      LOOP AT et_good_cells INTO DATA(ls_good_cells) WHERE   error = ''.
        CASE ls_good_cells-fieldname.
          WHEN 'ZZ001'."需求报价单号
  *          READ TABLE gt_output INDEX ls_good_cells-row_id ASSIGNING FIELD-SYMBOL(<fs>).
  *          IF sy-subrc = 0 AND <fs>-bmsch IS NOT INITIAL.
  *            <fs>-zism01 = CONV zmnga(  ls_good_cells-value ) * <fs>-vgw01 / <fs>-bmsch.
  *            <fs>-zism02 = CONV zmnga(  ls_good_cells-value ) * <fs>-vgw02 / <fs>-bmsch.
  
            "ENDIF.
  
            lv_refresh = abap_true.
  
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
  
  
  
      IF lv_refresh = abap_true.
        CALL METHOD go_alv_grid_main->refresh_table_display
          EXPORTING
            is_stable = gs_stable
          EXCEPTIONS
            finished  = 1
            OTHERS    = 2.
      ENDIF.
    ENDMETHOD.
    "DETAIL ALV数据改动事件实现
    METHOD handle_data_changed_fis_del.
  
    ENDMETHOD.
    "main alv的toolbar按钮增加删除实现
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
  *       OR function = '&LOCAL&UNDO'
         OR function = '&&SEP02'.
      "增加一个新按钮
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_button>).
      <fs_button>-butn_type = '3'.  "分割符
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-function = 'CHECKALL'.
      <fs_button>-icon = '@4B@'.
      <fs_button>-text = '全选'.
  
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-butn_type = '3'.  "分割符
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-function = 'CANCELALL'.
      <fs_button>-icon = '@4D@'.
      <fs_button>-text = '取消全选'.
  
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-butn_type = '3'.
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-function = 'SAVE'.
      <fs_button>-icon = '@2L@'.
      <fs_button>-text = '发送'.
    ENDMETHOD.
  
  
    "处理ALV用户按钮事件
    METHOD handle_user_command.
      CASE e_ucomm.
        WHEN  'CHECKALL'.
          LOOP AT gt_output INTO DATA(ls_output).
            IF ls_output-flag <> 'Y'.
              ls_output-zchecked = 'X'.
              MODIFY gt_output FROM ls_output.
            ENDIF.
          ENDLOOP.
  
        WHEN 'CANCELALL'.
          LOOP AT gt_output INTO DATA(ls_output2).
            IF ls_output-flag <> 'Y'.
              ls_output2-zchecked = ''.
              MODIFY gt_output FROM ls_output2.
            ENDIF.
          ENDLOOP.
        WHEN 'SAVE'.
          PERFORM frm_save_data.
        WHEN OTHERS.
      ENDCASE.
      go_alv_grid_main->refresh_table_display( is_stable = VALUE lvc_s_stbl( row = 'X' col = 'X')  ).
    ENDMETHOD.
  
    "double_click单击事件实现
    METHOD handle_double_click.
  
    ENDMETHOD.
  
  *  MAIN ALV hotspot_click单击事件实现
    METHOD handle_hotspot_click.
  
  
    ENDMETHOD.
  *  CANCEL ALV hotspot_click单击事件实现
    METHOD handle_hotspot_click_cal.
  
  
    ENDMETHOD.
    "onf4搜索帮助实现
    METHOD handle_onf4.
      CASE e_fieldname.
        WHEN 'ZZ001'.
          DATA lt_ddshretval TYPE TABLE OF ddshretval.
          DATA(lv_rowid) = es_row_no-row_id. "ALV正在操作的行号
          DATA(lv_matnr) = gt_output[ lv_rowid ]-matnr ."ALV当前行的物料编号
          DATA(lv_werks) = gt_output[ lv_rowid ]-werks ."ALV当前行的工厂编号
  ***        过滤物料编号的报价单号
          DATA(lt_help) = FILTER #( gt_help  USING KEY matnr_werks_zz001 WHERE matnr = lv_matnr  AND werks = lv_werks ).
          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
            EXPORTING
              retfield        = 'ZZ001' "返回字段
              value_org       = 'S'
            TABLES
              value_tab       = lt_help
              return_tab      = lt_ddshretval
            EXCEPTIONS
              parameter_error = 1
              no_values_found = 2
              OTHERS          = 3.
          IF sy-subrc = 0 AND lt_ddshretval IS NOT INITIAL.
            READ TABLE lt_help WITH KEY matnr = lt_ddshretval[ 1 ]-fieldval INTO DATA(ls_help).
            IF sy-subrc = 0.
              gt_output[ lv_rowid ]-zz001 = ls_help-zz001.
            ENDIF.
          ENDIF.
  
          er_event_data->m_event_handled = 'X'.
          go_alv_grid_main->refresh_table_display( is_stable = VALUE lvc_s_stbl( row = 'X' col = 'X') i_soft_refresh = abap_true  ).
        WHEN OTHERS.
      ENDCASE.
    ENDMETHOD.
  
    " dialogbox_container关闭事件实现
    METHOD handle_dialogbox_con_colse.
  * set dialogbox invisible
  * (the dialogbox is destroyed outomatically when the user
  * switches to another dynpro).
  
      sender->set_visible( visible = space ).
  
  * In this example closing the dialogbox leads
  * to make it invisible. It is also conceivable to destroy it
  * and recreate it if the user doubleclicks a line again.
  * Displaying a great amount of data has a greater impact on performance.
    ENDMETHOD.
  ENDCLASS.                    "handle_event implementation
  DATA go_event_receiver TYPE REF TO lcl_event_receiver.