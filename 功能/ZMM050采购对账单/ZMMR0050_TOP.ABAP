*&---------------------------------------------------------------------*
*& 包含               ZMMR0050_TOP
*&---------------------------------------------------------------------*

TYPE-POOLS:SLIS,TRUXS ,ICON.

TABLES:SSCRFIELDS,EKBE,LFA1,EKKO,BKPF,MAKT,EKPO,ZMMT0004.


TYPES:BEGIN OF   TY_TAB,
        SEL      TYPE C,
        MARK     TYPE C,
        STYLE    TYPE LVC_T_STYL,   "单元格控制
        UMREZ    TYPE EKPO-UMREZ,
        UMREN    TYPE EKPO-UMREN,
        RETPO    TYPE EKPO-RETPO,
        KBETR    TYPE KONV-KBETR,
        EKORG    TYPE EKKO-EKORG,
        CUOBJ_BM TYPE MCHA-CUOBJ_BM,
        ZXGZTMX  TYPE CHAR20,
        MESSAGE  TYPE CHAR200,
        ZDZZTMS  TYPE CHAR10,
        ZDEL     TYPE C,
        MODE     TYPE C LENGTH 1.
        INCLUDE TYPE ZMMT0004.
      TYPES:END OF TY_TAB.

TYPES : BEGIN OF TY_RSEG ,
          BELNR TYPE RSEG-BELNR,
          GJAHR TYPE RSEG-GJAHR,
          BUZEI TYPE RSEG-BUZEI,
          SHKZG TYPE RSEG-SHKZG,
          MENGE TYPE RSEG-MENGE,
          BSTME TYPE RSEG-BSTME,
          BPMNG TYPE RSEG-BPMNG,
          BPRME TYPE RSEG-BPRME,
          LFGJA TYPE RSEG-LFGJA,
          LFBNR TYPE RSEG-LFBNR,
          LFPOS TYPE RSEG-LFPOS,
        END OF TY_RSEG .
DATA : GS_RSEG TYPE TY_RSEG .
DATA : GT_RSEG TYPE TABLE OF TY_RSEG .


DATA:GT_OUT TYPE TABLE OF TY_TAB,
     GS_OUT TYPE TY_TAB.


DATA:GS_ALV TYPE TY_TAB,
     GT_ALV LIKE TABLE OF GS_ALV.
DATA : GT_ALVO LIKE TABLE OF GS_ALV.


DATA:GS_ZMMT0004 TYPE ZMMT0004,
     GT_ZMMT0004 LIKE TABLE OF GS_ZMMT0004.


DATA:BEGIN OF GS_AUSP,
       OBJEK TYPE  AUSP-OBJEK,
       ATINN TYPE  AUSP-ATINN,
       ATWRT TYPE  AUSP-ATWRT,
     END OF GS_AUSP,
     GT_AUSP LIKE TABLE OF GS_AUSP.



DATA:BEGIN OF GS_A003,
       KSCHL TYPE  A003-KSCHL,
       ALAND TYPE  A003-ALAND,
       MWSKZ TYPE  A003-MWSKZ,
       KNUMH TYPE  A003-KNUMH,
       KBETR TYPE  KONP-KBETR,
     END OF GS_A003,
     GT_A003 LIKE TABLE OF GS_A003.


DATA:BEGIN OF GS_MSEG,
       MBLNR TYPE  MSEG-MBLNR,
       MJAHR TYPE  MSEG-MJAHR,
       ZEILE TYPE  MSEG-ZEILE,
       SJAHR TYPE  MSEG-SJAHR,
       SMBLN TYPE  MSEG-SMBLN,
       SMBLP TYPE  MSEG-SMBLP,
     END OF GS_MSEG,
     GT_MSEG LIKE TABLE OF GS_MSEG.

DATA:GT_MSEG1 LIKE TABLE OF GS_MSEG.


DATA:BEGIN OF GS_MCHA,
       MATNR    TYPE  MCHA-MATNR,
       WERKS    TYPE  MCHA-WERKS,
       CHARG    TYPE  MCHA-CHARG,
       CUOBJ_BM TYPE MCHA-CUOBJ_BM,
     END OF GS_MCHA,
     GT_MCHA LIKE TABLE OF GS_MCHA.

DATA:GS_ALV1 TYPE TY_TAB,
     GT_ALV1 LIKE TABLE OF GS_ALV1.

DATA:GS_ALV_ITEM TYPE TY_TAB,
     GT_ALV_ITEM LIKE  TABLE OF GS_ALV_ITEM.


DATA:GT_DOM_APSTT  TYPE STANDARD TABLE OF DD07V.
DATA : GV_TABIX1 TYPE SY-TABIX .
DATA : GV_UCOMM TYPE SY-UCOMM .

DATA:GV_TABIX LIKE SY-TABIX.
DATA:GS_LAYOUT   TYPE LVC_S_LAYO.
DATA:GT_FIELDCAT TYPE LVC_T_FCAT,
     GS_FIELDCAT TYPE LVC_S_FCAT.
DATA:G_VARIANT1  TYPE DISVARIANT.
DATA:GT_EXCLUDE  TYPE UI_FUNCTIONS." 功能G: 设定屏蔽标准ALV 工具栏的按钮
DATA:GS_EDIT     TYPE LVC_S_STYL.
DATA:OK_CODE     TYPE SY-UCOMM.

DATA :  I_SORT TYPE LVC_T_SORT WITH HEADER LINE .

DATA:G_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
DATA:G_GUI_ALV   TYPE REF TO CL_GUI_ALV_GRID.
DATA:GV_TITLE    TYPE STRING.

DATA : GS_UP_DATA TYPE TY_TAB.


CONSTANTS:C_LINE_LENGTH TYPE I VALUE 4000.
DATA : G_PR_CON   TYPE REF TO CL_GUI_CUSTOM_CONTAINER .
DATA : G_PR_LTXT  TYPE REF TO CL_GUI_TEXTEDIT .
DATA : G_PV_CNAM  TYPE C .
DATA : G_PV_EDIT  TYPE C VALUE 'X'. "‘X’-可编辑，‘’不可编辑
DATA : G_PV_VALUE TYPE C.

DATA : GT_UP_DATA  TYPE TABLE OF TY_TAB .
DATA: GCL_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID.
DATA : GS_SETTING    TYPE LVC_S_GLAY.
DATA: NAME2 TYPE RLGRAP-FILENAME.

DATA:GS_LAYOUT1   TYPE LVC_S_LAYO.
DATA:GT_FIELDCAT1 TYPE LVC_T_FCAT,
     GS_FIELDCAT1 TYPE LVC_S_FCAT.


FIELD-SYMBOLS:<FS> TYPE  TY_TAB.

DEFINE FILL_FIELD_LIST.
  gs_fieldcat-fieldname       = &1.
  gs_fieldcat-coltext         = &2.
  gs_fieldcat-ref_table       = &3.
  gs_fieldcat-ref_field       = &4.
*  gs_fieldcat-key             = &5.
  gs_fieldcat-DO_SUM            = &5.
  gs_fieldcat-checkbox        = &6.
  gs_fieldcat-edit            = &7.
  gs_fieldcat-f4availabl       = &8.
  gs_fieldcat-outputlen       = &9.
  IF gs_fieldcat-fieldname = 'MATNR' .
  gs_fieldcat-NO_ZERO       = 'X'.
  ENDIF.
  IF gs_fieldcat-fieldname = 'SEL' .
 " gs_fieldcat-NO_OUT        = 'X'.
  ENDIF.

*  IF gs_fieldcat-fieldname = 'BELNR'
*    OR gs_fieldcat-fieldname = 'EBELN' .
*    gs_fieldcat-HOTSPOT       = 'X'.
*  ENDIF.
  APPEND gs_fieldcat TO gt_fieldcat.
 clear: gs_fieldcat.
END-OF-DEFINITION.



DEFINE FILL_FIELD_LIST1.
  gs_fieldcat1-fieldname       = &1.
  gs_fieldcat1-coltext         = &2.
  gs_fieldcat1-ref_table       = &3.
  gs_fieldcat1-ref_field       = &4.
  gs_fieldcat1-key             = &5.
  gs_fieldcat1-checkbox        = &6.
  gs_fieldcat1-edit            = &7.
  gs_fieldcat1-f4availabl       = &8.
  gs_fieldcat1-outputlen       = &9.
  IF gs_fieldcat1-fieldname = 'MATNR' .
  gs_fieldcat-NO_ZERO       = 'X'.
  ENDIF.
  IF gs_fieldcat1-fieldname = 'SEL' .
 " gs_fieldcat-NO_OUT        = 'X'.
  ENDIF.
  APPEND gs_fieldcat1 TO gt_fieldcat1.
 clear: gs_fieldcat.
END-OF-DEFINITION.




SELECTION-SCREEN BEGIN OF BLOCK BL01 WITH FRAME TITLE TEXT-T01.
SELECT-OPTIONS:S_ZDZDH  FOR ZMMT0004-ZDZDH MODIF ID Z01,
               S_ZDZZT  FOR ZMMT0004-ZDZZT MODIF ID Z01,
               S_NAMLA  FOR ZMMT0004-NAMLA MODIF ID Z01,
               S_LIFNR  FOR EKKO-LIFNR,
               S_ERNAM  FOR EKKO-ERNAM,
               S_BUDAT  FOR BKPF-BUDAT,
               S_EBELN  FOR EKKO-EBELN,
               S_BSART  FOR EKKO-BSART,
               S_PSTYP  FOR EKPO-PSTYP,
               S_CHARG  FOR EKBE-CHARG,
               S_WAERS  FOR EKBE-WAERS,
               S_WERKS  FOR EKBE-WERKS,
               S_MATNR  FOR MAKT-MATNR.
PARAMETERS:RB_01 RADIOBUTTON GROUP A DEFAULT 'X' USER-COMMAND AB,
           RB_02 RADIOBUTTON GROUP A,
           RB_03 RADIOBUTTON GROUP A,
           RB_04 RADIOBUTTON GROUP A.

SELECTION-SCREEN END OF BLOCK BL01.