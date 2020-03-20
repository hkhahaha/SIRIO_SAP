*&---------------------------------------------------------------------*
*& 包含               ZQMR0045SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001.
PARAMETERS:r_01 RADIOBUTTON GROUP rg1 USER-COMMAND crt DEFAULT 'X', "创建通知单
           r_02 RADIOBUTTON GROUP rg1.
SELECTION-SCREEN END OF BLOCK blk.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_werks  TYPE marc-werks DEFAULT '8100' MODIF ID m1,
            p_day(6) TYPE c MODIF ID m1, "到期提前天数
            p_bwart  LIKE mseg-bwart AS LISTBOX VISIBLE LENGTH 20 OBLIGATORY MODIF ID m1. "移动类型

SELECT-OPTIONS: s_matnr FOR mara-matnr MODIF ID m1,"物料
s_charg FOR qals-charg MODIF ID m1,"批次
s_mtart FOR mara-mtart MODIF ID m1,"物料类型
s_lgort FOR mard-lgort MODIF ID m1,"库存地点
"下面是通知单号的字段
s_no FOR zmm_changeno-zsapnote_no MODIF ID m2,"通知单号
s_werks FOR marc-werks MODIF ID m2,"工厂
s_matnr2 FOR mara-matnr MODIF ID m2,"物料
s_charg2 FOR qals-charg MODIF ID m2,"批次
s_mtart2 FOR mara-mtart MODIF ID m2,"物料类型
s_lgort2 FOR mard-lgort MODIF ID m2,"库存地点
s_userna FOR sy-uname MODIF ID m2,"用户名
s_date FOR sy-datum MODIF ID m2,"日期
s_time FOR sy-uzeit MODIF ID m2."时间
PARAMETERS: p_no AS CHECKBOX DEFAULT 'X' MODIF ID m2.

SELECTION-SCREEN END OF BLOCK blk1.

AT SELECTION-SCREEN OUTPUT.
*修改屏幕
  PERFORM frm_change_screen.