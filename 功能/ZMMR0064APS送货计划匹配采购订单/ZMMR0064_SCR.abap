*&---------------------------------------------------------------------*
*& 包含               ZMMR0064_SCR
*&---------------------------------------------------------------------*

"定义上部分的按钮选择框
SELECTION-SCREEN BEGIN OF BLOCK blk_1 WITH FRAME TITLE TEXT-001.

PARAMETERS:
  p_r01 RADIOBUTTON GROUP g1 USER-COMMAND hk DEFAULT 'X',
  p_r02 RADIOBUTTON GROUP g1,
  p_r03 RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK blk_1.





"选择屏幕下部分的明细框
SELECTION-SCREEN BEGIN OF BLOCK blk_2 WITH FRAME TITLE TEXT-002."定义屏幕
SELECT-OPTIONS:
               s_carrid FOR sflight-carrid,"条件输入框，一个for对应一个
               s_connid FOR sflight-connid,"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错
               s_fldate FOR sflight-fldate MODIF ID m1,
               s_werks FOR zmmaps_pl-werks MODIF ID m1,
               s_ekgrp FOR marc-ekgrp MODIF ID m1,
               s_matnr FOR marc-matnr MODIF ID m1,
               s_ver FOR zmmaps_pl-version MODIF ID m1,
               s_stcode FOR zmmaps_pl-st_code MODIF ID m1.

SELECT-OPTIONS:
              s_count FOR spfli-countryfr MODIF ID m2.
PARAMETERS:p_pc AS CHECKBOX MODIF ID M1.
SELECTION-SCREEN END OF BLOCK blk_2.

SELECTION-SCREEN FUNCTION KEY 1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM frm_change_screen.

AT SELECTION-SCREEN.
  IF sscrfields-ucomm = 'FC01'.
    MESSAGE '按了按钮' TYPE 'S'.
  ENDIF.