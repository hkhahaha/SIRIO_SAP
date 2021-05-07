*&---------------------------------------------------------------------*
*& 包含               ZMMR0064_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9100 OUTPUT.
  SET PF-STATUS 'STATUS9100'.
  SET TITLEBAR 'STATUS9100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ALV_DISPLAY OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE alv_display OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.

  IF go_split_container IS INITIAL.
    CREATE OBJECT go_container
      EXPORTING
        container_name = 'GO_CONTAINER'.

    "拆分屏幕
    CREATE OBJECT go_split_container
      EXPORTING
        link_dynnr = sy-dynnr
        link_repid = sy-repid
        parent     = go_container
        rows       = 1
        columns    = 2
        name       = 'GO_SPLIT_CONTAINER'.

    "左半部分
    CALL METHOD go_split_container->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = go_container1.
    "ALV控件
    CREATE OBJECT go_alv_control1
      EXPORTING
        i_parent = go_container1.
    "添加自定义按钮
    CREATE OBJECT event_receiver.
    " 注册事件handler方法
    SET HANDLER event_receiver->handle_hotspot_click  FOR go_alv_control1.
    SET HANDLER event_receiver->handle_double_click   FOR go_alv_control1.
    SET HANDLER event_receiver->handle_toolbar FOR go_alv_control1.
    SET HANDLER event_receiver->handle_command FOR go_alv_control1.
    CALL METHOD go_alv_control1->set_table_for_first_display
      CHANGING
        it_outtab       = gt_sflight
        it_fieldcatalog = gt_field_cat1.

    "右半部分
    CALL METHOD go_split_container->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = go_container2.

    "ALV控件
    CREATE OBJECT go_alv_control2
      EXPORTING
        i_parent = go_container2.

    SET HANDLER event_receiver->handle_hotspot_click  FOR go_alv_control2.
    SET HANDLER event_receiver->handle_double_click   FOR go_alv_control2.
    SET HANDLER event_receiver->handle_toolbar2 FOR go_alv_control2.
    SET HANDLER event_receiver->handle_command FOR go_alv_control2.

    CALL METHOD go_alv_control2->set_table_for_first_display
      CHANGING
        it_outtab       = gt_spfli
        it_fieldcatalog = gt_field_cat2.
  ELSE.
    CALL METHOD go_alv_control1->refresh_table_display.
    CALL METHOD go_alv_control2->refresh_table_display.
  ENDIF.
ENDMODULE.