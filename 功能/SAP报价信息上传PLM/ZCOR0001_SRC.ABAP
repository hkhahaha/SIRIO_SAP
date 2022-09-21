*&---------------------------------------------------------------------*
*& 包含               ZCOR0001_SRC
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& 包含               ZPPR0003_SRC
*&---------------------------------------------------------------------*
TABLES zsppr003.
*选择屏幕
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: s_matnr FOR keko-matnr OBLIGATORY.
PARAMETERS p_werks TYPE keko-werks OBLIGATORY.
SELECT-OPTIONS: s_bwdat FOR keko-bwdat OBLIGATORY.
PARAMETERS p_bwvar TYPE keko-bwvar  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk1.