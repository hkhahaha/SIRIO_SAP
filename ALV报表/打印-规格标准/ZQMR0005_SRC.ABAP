*&---------------------------------------------------------------------*
*& 包含               ZQMR0005_SRC
*&---------------------------------------------------------------------*
TABLES mapl.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
PARAMETERS p_werks TYPE werks_d OBLIGATORY.
SELECT-OPTIONS:s_matnr FOR mapl-matnr,
               s_plnnr FOR mapl-plnnr.
PARAMETERS p_znkbz AS  CHECKBOX.
SELECTION-SCREEN END OF BLOCK blk1.