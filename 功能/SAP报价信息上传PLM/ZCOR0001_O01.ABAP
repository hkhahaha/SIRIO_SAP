*&---------------------------------------------------------------------*
*& 包含               ZCOR0001_O01
*&---------------------------------------------------------------------*

MODULE status_9100 OUTPUT.
  SET PF-STATUS 'STATUS_9100'.
  SET TITLEBAR 'MAIN9100'.
  IF go_docking IS NOT BOUND.
    go_docking = NEW cl_gui_docking_container(
      repid = sy-repid
      dynnr = sy-dynnr
     extension = 2000 "屏幕宽度
    ).
    go_alv_grid_main = NEW cl_gui_alv_grid(
      i_parent = go_docking
    ).

***  注册事件
    go_event_receiver = NEW lcl_event_receiver( ).
***  注册编辑事件，使ALV编辑后回车，或离开焦点时可以进行数据校验
    go_alv_grid_main->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
***注册数据修改事件，从而使程序可以在ALV数据有改动时调用data_changed_finished方法
    SET HANDLER go_event_receiver->handle_data_changed_finished FOR go_alv_grid_main.
***注册双击事件
    "SET HANDLER go_event_receiver->handle_double_click FOR go_alv_grid_main.
***  注册HOTSPOT_CLICK单击事件
    "SET HANDLER go_event_receiver->handle_hotspot_click FOR go_alv_grid_main.
***    注册ALV用户命令事件
    SET HANDLER go_event_receiver->handle_user_command FOR go_alv_grid_main.
***    注册toolbar
    SET HANDLER go_event_receiver->handle_toolbar FOR go_alv_grid_main.

***    注册ONF4搜索帮助事件
    SET HANDLER go_event_receiver->handle_onf4 FOR go_alv_grid_main.
***    使go_event_receiver->handle_onf4注册事件生效
    go_alv_grid_main->register_f4_for_fields( it_f4 = VALUE lvc_t_f4(
        (
        fieldname = 'ZZ001'
        register = abap_true
        getbefore = abap_true
        chngeafter = abap_true
        internal = abap_true
        )
     ) ).

*** 使 go_event_receiver->handle_toolbar注册事件生效
    go_alv_grid_main->set_toolbar_interactive( ).

    CALL METHOD go_alv_grid_main->set_table_for_first_display
      EXPORTING
        i_structure_name = 'ZSCOR001'
        is_variant       = gs_disvariant  "列表变式
        i_save           = 'A' " 能保存默认及用户变式
        i_default        = 'X'  " 能保存默认变式
        is_layout        = gs_layout    " 画面布局
      CHANGING
        it_outtab        = gt_output
        it_fieldcatalog  = gt_fieldcatalog.
  ENDIF.

  cl_gui_control=>set_focus( control = go_alv_grid_main ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*