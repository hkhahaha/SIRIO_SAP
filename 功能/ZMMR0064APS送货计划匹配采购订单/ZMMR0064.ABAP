*&---------------------------------------------------------------------*
*& Report ZMMR0064
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr0064.

INCLUDE zmmr0064_top."变量定义
INCLUDE zmmr0064_scr."选择屏幕
INCLUDE zmmr0064_f01."各种函数
INCLUDE zmmr0064_cla."各种类
INCLUDE zmmr0064_pai."PAI处理
INCLUDE zmmr0064_pbo."PBO处理


INITIALIZATION.
  PERFORM frm_init.


START-OF-SELECTION.
  PERFORM frm_screen_check.    "屏幕检查
  PERFORM frm_authority_check. "权限检查
  PERFORM frm_get_data.        "获取数据

  CASE 'X'.
    WHEN p_r01."A功能
      PERFORM frm_alv_a.
      CALL SCREEN 9100.
    WHEN p_r02.
      WRITE 'b'.
    WHEN p_r03.
      WRITE 'c'.
  ENDCASE.

END-OF-SELECTION.