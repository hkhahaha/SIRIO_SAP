*&---------------------------------------------------------------------*
*& Report ZQMR0045
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zqmr0045.

INCLUDE zqmr0045top."变量的定义
INCLUDE zqmr0045scr."选择屏幕
INCLUDE zqmr0045f01.
INCLUDE zqmr0045f02.
INCLUDE zqmr0045c01.

INITIALIZATION.
  PERFORM frm_init.
  PERFORM check_value.

START-OF-SELECTION.

  PERFORM frm_get_ext_st.
  CASE 'X'.
    WHEN r_01.
*  PERFORM frm_authority_check_01.
      CALL SCREEN 100.
    WHEN r_02.
      PERFORM frm_screen_check.    "屏幕检查
      PERFORM frm_authority_check. "权限检查
      PERFORM getdata.        "获取数据
      PERFORM catalog.   "处理ALV
      PERFORM showalv.   "显示ALV
  ENDCASE.