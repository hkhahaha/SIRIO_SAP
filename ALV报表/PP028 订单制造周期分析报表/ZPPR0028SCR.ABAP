*&---------------------------------------------------------------------*
*& 包含               ZPPR0028SCR
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*  选择屏幕
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-006.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(6) TEXT-007.
SELECTION-SCREEN POSITION 8.

PARAMETERS:p_query   RADIOBUTTON GROUP rg1 USER-COMMAND crt DEFAULT 'X'.
SELECTION-SCREEN COMMENT 14(10) TEXT-008 FOR FIELD p_query.
SELECTION-SCREEN POSITION 25.

PARAMETERS p_time RADIOBUTTON GROUP rg1 .
SELECTION-SCREEN COMMENT 38(10) TEXT-009 FOR FIELD p_time.
SELECTION-SCREEN POSITION 50.

PARAMETERS p_plifz RADIOBUTTON GROUP rg1 .
SELECTION-SCREEN COMMENT 52(16) TEXT-010 FOR FIELD p_time.
SELECTION-SCREEN POSITION 50.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-001.
PARAMETERS:p_halb RADIOBUTTON GROUP rg2 USER-COMMAND crt2 DEFAULT 'X' MODIF ID m2,
           p_roh  RADIOBUTTON GROUP rg2 MODIF ID m2.
SELECT-OPTIONS: s_kunnr  FOR vbak-kunnr MODIF ID m1,
                s_matnr  FOR vbap-matnr MODIF ID m4,
                s_werks  FOR vbap-werks MODIF ID m5,
                s_vbeln  FOR vbak-vbeln MODIF ID m1,
                s_erdat  FOR vbak-erdat MODIF ID m1,    "下单时间
                s_znbjhq FOR vbap-znbjhq MODIF ID m1,   "要求交货日期
                s_ywy    FOR vbak-kunnr MODIF ID m1,    "业务员
                s_vtweg  FOR vbak-vtweg MODIF ID m1,    "分销渠道
                s_matkl  FOR mara-matkl MODIF ID m3,    "剂型
                s_auart  FOR tvakt-auart MODIF ID m1.   "订单类型
SELECTION-SCREEN END OF BLOCK blk2.

*======================================================================*
*  Selection Screen Events
*======================================================================*