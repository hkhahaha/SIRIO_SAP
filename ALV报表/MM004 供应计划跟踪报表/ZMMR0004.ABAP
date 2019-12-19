*&------------------------------------------------------------------
*& PROGRAM NAME: ZMMR0004
*& T-CODE:  ZMM004
*& PORGRAM TYPE: REPORT
*& DESCRIPTION: 供应计划跟踪报表
*& DS NUMBER: MM-DS-004
*&------------------------------------------------------------------
*& AUTHOR:  soarstars
*& DATE:    2019.10.25
*&------------------------------------------------------------------
*& MODIFICATION LOG:
*& VERSION   DATE        AUTHOR       DESCRIPTION     CHANGE REQUEST
*& -------- ----------  -----------  -------------  -----------------
*& 1.0      2019.10.25   soarstars
*&-------------------------------------------------------------------
REPORT zmmr0004.

INCLUDE zmmr0004_top.
INCLUDE zmmr0004_src.
INCLUDE zca_authority_check.
INCLUDE zmmr0004_f01.


INITIALIZATION.
  PERFORM frm_init_list.

AT SELECTION-SCREEN.


START-OF-SELECTION.

  "权限检查
  PERFORM frm_authority_check.
  "获取数据
  PERFORM frm_get_data.
  "处理数据
  PERFORM frm_process_data.
  "显示ALV
  PERFORM frm_display_alv.