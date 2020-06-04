*&---------------------------------------------------------------------*
*& 包含               ZMMR0032_SCR
*&---------------------------------------------------------------------*


INITIALIZATION.
*定义选择屏幕
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.

  SELECT-OPTIONS: s_ebeln FOR ekko-ebeln."凭证编号
  SELECT-OPTIONS: s_lifnr FOR ekko-lifnr."供应商
*  SELECT-OPTIONS: s_bukrs FOR ekko-bukrs."公司
  SELECT-OPTIONS: s_ekorg FOR ekko-ekorg."采购组织
  SELECT-OPTIONS: s_ekgrp FOR ekko-ekgrp."采购组
  SELECT-OPTIONS: s_bsart FOR ekko-bsart."凭证类型
  SELECT-OPTIONS: s_bedat FOR ekko-bedat."凭证日期
  PARAMETERS: p_werks TYPE ekpo-werks OBLIGATORY.

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