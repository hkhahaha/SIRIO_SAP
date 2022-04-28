*&---------------------------------------------------------------------*
*& 包含               ZQMR0011_S01
*&---------------------------------------------------------------------*
TYPES:BEGIN OF ty_range,
        zcpdy(20),
      END OF ty_range.
DATA lt_range TYPE TABLE OF ty_range WITH HEADER LINE.
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-b01.
PARAMETERS: p_werks TYPE afpo-pwerk OBLIGATORY.
SELECT-OPTIONS: s_ersda FOR mcha-ersda.               " 日期
SELECT-OPTIONS: s_kdauf FOR afpo-kdauf.               " 销售订单号
SELECT-OPTIONS: s_sort1 FOR but000-bu_sort1.          " 客户检索号
SELECT-OPTIONS: s_matnr FOR afpo-matnr.               " 客户检索号
SELECT-OPTIONS: s_charg FOR mseg-charg.               " 工艺批号
SELECT-OPTIONS: s_smatn FOR mseg-matnr.               " S产品代码
SELECT-OPTIONS: s_zcpdy FOR lt_range-zcpdy.           " 包装批号
SELECTION-SCREEN:END OF BLOCK blk1.