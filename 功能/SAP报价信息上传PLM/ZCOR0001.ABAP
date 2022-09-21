*&---------------------------------------------------------------------*
*& Report ZCOR0001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcor0001.
INCLUDE zcor0001_top."全局变量
INCLUDE zcor0001_c01.  "本地类
INCLUDE zcor0001_src. "选择屏幕
INCLUDE zcor0001_f01. "子例程
INCLUDE zcor0001_o01. "ALV PBO
INCLUDE zcor0001_i01.  "ALV_PIO
AT SELECTION-SCREEN OUTPUT.


AT SELECTION-SCREEN .
*权限检查
  PERFORM frm_authority_check.

START-OF-SELECTION.

  PERFORM frm_get_data."从数据库获取数据
  PERFORM frm_get_zz001. "获取报价单号
  PERFORM frm_process_data.  "设置输出内表其他字段值

END-OF-SELECTION.
  PERFORM frm_set_layout.    "设置页面布局
  PERFORM frm_set_fieldcatalog. "设置Main ALV 字段目录

**  显示ALV
    CALL SCREEN '9100'.