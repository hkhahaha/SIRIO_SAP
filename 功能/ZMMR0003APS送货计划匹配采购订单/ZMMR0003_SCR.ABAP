*&---------------------------------------------------------------------*
*& 包含               ZMMR0003_SCR
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Selection Screen
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK_SCR2 WITH FRAME TITLE TEXT-002.

PARAMETERS:
*            p_r01 RADIOBUTTON GROUP g1 ,
  P_R02 RADIOBUTTON GROUP G1 USER-COMMAND CM1 DEFAULT 'X',
  P_R05 RADIOBUTTON GROUP G1,
  P_R06 RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK BLK_SCR2.

SELECTION-SCREEN BEGIN OF BLOCK BLK_SCR1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: S_WERKS FOR ZMMAPS_PL-WERKS MODIF ID M1,
                S_LGORT FOR ZMMAPS_PL-LGORT MODIF ID M1,
                S_DATE2 FOR SY-DATUM MODIF ID M1,
                S_DATE  FOR SY-DATUM NO-EXTENSION NO INTERVALS MODIF ID M1,
                S_EKGRP FOR MARC-EKGRP MODIF ID M1,
                S_MATKL FOR MARA-MATKL MODIF ID M1,
                S_MATNR FOR ZMMAPS_PL-MATNR MODIF ID M1,
                S_CODE2 FOR ZMMAPS_PL-ST_CODE MODIF ID M1,
                S_STS   FOR ZMMAPS_PL-ZPR_STS  MODIF ID M1.
PARAMETERS: P_PC AS CHECKBOX DEFAULT 'X' MODIF ID M1.

SELECT-OPTIONS: S_EBELN FOR ZMMWMS_POL-EBELN MODIF ID M6,
                S_EBELP FOR ZMMWMS_POL-EBELP MODIF ID M6,
                S_DATE3 FOR ZMMWMS_POL-PDLV_DATE MODIF ID M6,
                S_MATNR2 FOR ZMMWMS_POL-MATNR MODIF ID M6,
                S_WERKS2 FOR ZMMWMS_POL-WERKS MODIF ID M6,
                S_LGORT2 FOR ZMMWMS_POL-LGORT MODIF ID M6,
                S_CODE   FOR ZMMWMS_POL-ST_CODE MODIF ID M6,
                S_EKGR1 FOR MARC-EKGRP MODIF ID M6,
                S_ERNAM  FOR EKKO-ERNAM MODIF ID M6,
                S_LIFNR  FOR EKKO-LIFNR MODIF ID M6,
                S_VERSI  FOR ZMMAPS_TEMP-VERSION MODIF ID M6,
                S_PLANNO FOR ZMMWMS_POL-DLV_PLANNO MODIF ID M6.
*                S_ITEMNO FOR ZMMWMS_POL-DLV_ITEMNO MODIF ID M6.
PARAMETERS: R_01 RADIOBUTTON GROUP G2 DEFAULT 'X' MODIF ID M6,
            R_02 RADIOBUTTON GROUP G2 MODIF ID M6,
            R_03 RADIOBUTTON GROUP G2 MODIF ID M6.

PARAMETERS: P_FILE LIKE KONSPARAM-PCU40_FILE DEFAULT 'C:\' MODIF ID M5.

SELECTION-SCREEN END OF BLOCK BLK_SCR1.

**下载模板选项
*SELECTION-SCREEN BEGIN OF SCREEN 0300 TITLE TEXT-044.
*SELECTION-SCREEN BEGIN OF BLOCK blk_300 WITH FRAME TITLE TEXT-046.
*PARAMETERS: "p_r03 RADIOBUTTON GROUP g2 NO-DISPLAY,
*            p_r04 RADIOBUTTON GROUP g2 DEFAULT 'X'.
*SELECTION-SCREEN END OF BLOCK blk_300.
*SELECTION-SCREEN END OF SCREEN 0300.

*工具栏按钮,最多5个
SELECTION-SCREEN FUNCTION KEY 1.

*======================================================================*
*  Selection Screen Events
*======================================================================*
**  maintain selection screen output
AT SELECTION-SCREEN OUTPUT.
*修改屏幕
  PERFORM FRM_CHANGE_SCREEN.

**** F4 value help
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM FRM_GET_EXCEL_F4 CHANGING P_FILE.                   "F4####

AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM = 'FC01'.      "工具栏按钮CODE为:'FC01'-'FC05'，屏幕上按钮CODE为：屏幕上的USER-COMMAND but1.
    PERFORM FRM_DOWNLOAD_EXECL_TMP USING 'FC01'.
  ENDIF.

*AT SELECTION-SCREEN ON <f>.
*AT SELECTION-SCREEN ON BLOCK <>.
****CHECK ON SELECT SCREEN INPUT