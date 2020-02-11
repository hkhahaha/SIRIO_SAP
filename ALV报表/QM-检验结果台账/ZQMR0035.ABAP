*-----------------------------------------------------------------------
* TITLE           : 检验结果台账
* AUTHOR          : Huxy
* DATE            : 2019.10.15
* DEVELOPMENT ID  :
* DESCRIPTION     : 检验结果台账
*                 :
*-----------------------------------------------------------------------
* CHANGE HISTORY LOG
*-----------------------------------------------------------------------
*  DATE  | AUTHOR     | CHANGE REF | DESCRIPTION
*-----------------------------------------------------------------------
* DDMMYY |            |            |
*-----------------------------------------------------------------------
REPORT zqmr0035.

INCLUDE zqmr0035top.
INCLUDE zqmr0035f01. "Form
INCLUDE zqmr0035f02. "ALV相关

*** initial data
INITIALIZATION.
  PERFORM frm_init.

*** prepare report data
START-OF-SELECTION.

  PERFORM frm_authority_check. "权限检查
  PERFORM frm_get_data.        "获取数据
  PERFORM frm_process_data.    "处理数据
  PERFORM frm_show_data TABLES gt_alv.   "显示ALV

*** output report
END-OF-SELECTION.