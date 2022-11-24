*&---------------------------------------------------------------------*
*& 包含               ZPPR0058_SEL_SCREEN
*&---------------------------------------------------------------------*
"选择屏幕
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_werks TYPE pbim-werks OBLIGATORY. "工厂
SELECT-OPTIONS: s_pbdnr FOR pbim-pbdnr OBLIGATORY. "需求计划
PARAMETERS  p_perxx TYPE pbed-perxx OBLIGATORY. "计划开始期间
SELECT-OPTIONS:s_matnr FOR pbim-matnr.

SELECTION-SCREEN END OF BLOCK blk1.