*&---------------------------------------------------------------------*
*& 包含               ZMMR0064_TOP
*&---------------------------------------------------------------------*
TABLES:sflight,spfli,sscrfields.
TABLES:zmmaps_pl,zmmaps_temp_new,zmmaps_pol,marc.

"A功能：送货需求匹配
"定义A功能OOALV的界面的相关参数
"定义ALV相关的容器，其中1代表的是左边，2代表的是右边
"由于屏幕分成左右两部分，因此需要2个容器，外层套一层splitter 容器，然后再套多一个容器，所以总共4个
DATA: go_split_container TYPE REF TO cl_gui_splitter_container.
DATA: go_container TYPE REF TO cl_gui_custom_container.
DATA: go_container1 TYPE REF TO cl_gui_container.
DATA: go_container2 TYPE REF TO cl_gui_container.
DATA: go_alv_control1 TYPE REF TO cl_gui_alv_grid.
DATA: go_alv_control2 TYPE REF TO cl_gui_alv_grid.
"定义样式
DATA: gs_field_cat1 TYPE lvc_s_fcat.
DATA: gs_field_cat2 TYPE lvc_s_fcat.

DATA: gt_field_cat1 TYPE lvc_t_fcat.
DATA: gt_field_cat2 TYPE lvc_t_fcat.

DATA:gv_control1(1) VALUE 'X'.
DATA:gv_control2(1) VALUE 'X'.
DATA: gs_toolbar TYPE stb_button.




DATA: BEGIN OF gs_sflight,
        carrid TYPE sflight-carrid,
        connid TYPE sflight-connid,
        fldate TYPE sflight-fldate,
        price  TYPE sflight-price,
      END   OF gs_sflight.

DATA: BEGIN OF gs_spfli,
        carrid   TYPE spfli-carrid,
        connid   TYPE spfli-connid,
        cityfrom TYPE spfli-cityfrom,
        cityto   TYPE spfli-cityto,
      END   OF gs_spfli.
DATA: gt_sflight LIKE STANDARD TABLE OF gs_sflight,
      gt_spfli   LIKE STANDARD TABLE OF gs_spfli.

DEFINE set_button.
  CLEAR gs_toolbar.
  MOVE &2 TO gs_toolbar-function.      "功能码
* --> This function code is evaluated in 'handle_menu_button'
  MOVE &3 TO gs_toolbar-text.             "显示文本
  MOVE &3 TO gs_toolbar-quickinfo.   "提示文本
  MOVE &4 TO gs_toolbar-butn_type.               "按钮类型--0：普通按钮
  MOVE space TO gs_toolbar-disabled.
  APPEND gs_toolbar TO &1.
END-OF-DEFINITION.