*-----------------------------------------------------------------------
* TITLE           : 订单制造周期分析报表
* AUTHOR          : LJM
* DATE            : 2020.03.17
* DEVELOPMENT ID  :
* DESCRIPTION     : 订单制造周期分析报表
*                 :
*-----------------------------------------------------------------------
* CHANGE HISTORY LOG
*-----------------------------------------------------------------------
*  DATE  | AUTHOR     | CHANGE REF | DESCRIPTION
*-----------------------------------------------------------------------
* DDMMYY |            |            |
*-----------------------------------------------------------------------
REPORT zppr0028 MESSAGE-ID zpp.

INCLUDE zppr0028top.
INCLUDE zppr0028scr.
INCLUDE zppr0028c01."本地类
INCLUDE zppr0028f01. "PBO
INCLUDE zppr0028f02. "ALV相关
INCLUDE zppr0028f03."GRID ALV相关


*** initial data
INITIALIZATION.
  PERFORM frm_init.
*maintain selection screen output
AT SELECTION-SCREEN OUTPUT .
*修改屏幕
  PERFORM frm_change_screen.
*** prepare report data
START-OF-SELECTION.
  go_event_receiver = NEW zlcl_event_receiver( ).
  CASE 'X'.
    WHEN p_query."查询报表
*      PERFORM frm_set_rsdswhere."设置动态WHERE条件
      PERFORM frm_get_sodata.      "获取销售订单
      PERFORM frm_process_sodata."处理销售订单数据
      PERFORM frm_get_detaildata. "获取BOM数据
      PERFORM frm_alv_output_data."处理BOM数据
      PERFORM frm_show_data.   "显示ALV
    WHEN p_time. "维护时间
      PERFORM frm_authority_check. "权限检查
      PERFORM frm_display_maintain_alv."显示物料组生产时间或原辅料准备时间的ALV
    WHEN OTHERS.
  ENDCASE.