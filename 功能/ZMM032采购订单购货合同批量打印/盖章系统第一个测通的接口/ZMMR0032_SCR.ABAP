*&---------------------------------------------------------------------*
*& 包含               ZMMR0032_SCR
*&---------------------------------------------------------------------*




INITIALIZATION.
*定义选择屏幕
  SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001.
  PARAMETERS:r_01 RADIOBUTTON GROUP rg1 USER-COMMAND crt DEFAULT 'X'.
  PARAMETERS: r_02 RADIOBUTTON GROUP rg1.
  PARAMETERS:r_03 RADIOBUTTON GROUP rg1 MODIF ID m5.

  SELECTION-SCREEN END OF BLOCK blk.




  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.

  SELECT-OPTIONS: s_ebeln FOR ekko-ebeln."凭证编号
  SELECT-OPTIONS: s_lifnr FOR ekko-lifnr."供应商
*  SELECT-OPTIONS: s_bukrs FOR ekko-bukrs."公司
  SELECT-OPTIONS: s_ekorg FOR ekko-ekorg."采购组织
  SELECT-OPTIONS: s_ekgrp FOR ekko-ekgrp."采购组
  SELECT-OPTIONS: s_bsart FOR ekko-bsart."凭证类型
  SELECT-OPTIONS: s_bedat FOR ekko-bedat."凭证日期

  PARAMETERS: p_werks TYPE ekpo-werks OBLIGATORY.
  SELECT-OPTIONS: s_ernam FOR ekko-ernam."制单人
  SELECT-OPTIONS: s_ver FOR ls_zmmt0032-version MODIF ID m3."版本号
  SELECT-OPTIONS: s_date FOR ls_zmmt0032-last_changed_time MODIF ID m3."记录建立日期
  SELECT-OPTIONS: s_flag FOR ls_zmmt0032-flag MODIF ID m3."合同唯一标识
  PARAMETERS: p_sign TYPE zmmt0032-sign MODIF ID m3 OBLIGATORY."合同状态

  SELECTION-SCREEN END OF BLOCK b1.
*
*  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
*  PARAMETERS p_po RADIOBUTTON GROUP g1.  "采购订单
*  PARAMETERS p_cn RADIOBUTTON GROUP g1. "
*
*  SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION."屏幕初始化
*  s_bedat-low = sy-datum.  ******ZZD 去掉凭证日期默认值
*  APPEND s_bedat.

AT SELECTION-SCREEN OUTPUT.
*修改屏幕
  PERFORM frm_change_screen.

*取数
START-OF-SELECTION.

*  PERFORM FRM_GET_DATE.

*  PERFORM LAYOUT_BUILD. "2.用于定义ALV表单的相关格式、属性

*显示alv
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*用户交互
MODULE user_command_0100 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'SAVE'.
*        PERFORM frm_save_data.
    WHEN 'PRT'.
*        PERFORM frm_down_load.
    WHEN OTHERS.
*     perform frm_prcess_long_text change gv_text.
  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STA100'.
*SET TITLEBAR 'TB01'.
ENDMODULE.