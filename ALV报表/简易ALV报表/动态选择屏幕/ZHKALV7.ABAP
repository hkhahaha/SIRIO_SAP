*&---------------------------------------------------------------------*
*& Report ZHKALV7
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhkalv7.
TABLES:sflight,spfli,sscrfields.
INCLUDE zhkalv7_top."变量定义
INCLUDE zhkalv7_scr."选择屏幕
INCLUDE zhkalv7_f01."各种函数
INCLUDE zhkalv7_CLA."各种类


INITIALIZATION.
 PERFORM frm_init.


START-OF-SELECTION.
  CASE 'X'.
    WHEN p_r01.
      WRITE 'A'.
    WHEN p_r02.
      WRITE 'b'.
    WHEN p_r03.
      WRITE 'c'.
  ENDCASE.

END-OF-SELECTION.