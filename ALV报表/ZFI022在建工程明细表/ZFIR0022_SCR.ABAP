*&---------------------------------------------------------------------*
*& 包含               ZFIR0022_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.

PARAMETERS:
  p_bukrs LIKE bkpf-bukrs OBLIGATORY DEFAULT '8100' "公司代码
  "p_gjahr LIKE bkpf-gjahr DEFAULT sy-datum+0(4) OBLIGATORY "财务年度
.
SELECT-OPTIONS:
  s_gjahr for gv_GJAHR,    "年度
  s_lifnr FOR gv_lifnr, "供应商
*  s_ebeln FOR gv_ebeln,"采购订单
  s_aufnr FOR gv_aufnr. "内部订单
*  s_aufgr FOR gv_aufgr."内部订单组

SELECTION-SCREEN:END OF BLOCK blk1.