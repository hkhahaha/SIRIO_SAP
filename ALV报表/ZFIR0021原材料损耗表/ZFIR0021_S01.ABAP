*&---------------------------------------------------------------------*
*& 包含               ZFIR0021_S01
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: P_WERKS TYPE AUFK-WERKS OBLIGATORY.
  PARAMETERS: P_GJAHR TYPE CHAR04 OBLIGATORY DEFAULT SY-DATUM+0(4).
  SELECT-OPTIONS: S_MONTH FOR ACDOCA-POPER OBLIGATORY DEFAULT SY-DATUM+4(2).
  SELECT-OPTIONS: S_MATNR FOR MATDOC-MATNR.
  SELECT-OPTIONS: S_AUART FOR AUFK-AUART.
SELECTION-SCREEN END OF BLOCK BL1.