*****************************************************************
*  System      :  仙乐二期ERP项目
*  Module      :  FI
*  ProgramID   :  ZFI049
*  Program     :  ZFIR0049
*  Author      :  gaozhongzhao
*  Date        :  18.11.2020 14:00:44
*  Description :  销售运费分摊表
*****************************************************************
*  Modified Recorder :
*  Date       C#NO         Author        Content
*  --------  ----------    ---------     ---------------
*  20210408  S4DK904689    04815         预提过账和差异过账项目文本调整，增加承运商
*****************************************************************
*  System      :  运维
*  Module      :  FI
*  ProgramID   :  ZFI049
*  Program     :  ZFIR0049
*  Author      :  Seashell Huang
*  Date        :  2021.6.25
*  Description :  销售运费分摊表
*****************************************************************
*  Modified Recorder :
*  Date       C#NO         Author        Content
*  --------  ----------    ---------     ---------------
*  20210625  S4DK904898       05025        FI: 销售运费分摊表-20210625
*****************************************************************
REPORT ZFIR0049 NO STANDARD PAGE HEADING LINE-SIZE 150 .
*&---------------------------------------------------------------------*
* ALV TYPES
*&---------------------------------------------------------------------*
TYPE-POOLS: SLIS .

DATA: GS_LAYOUT     TYPE LVC_S_LAYO.            "ALV布局工作区
DATA: GS_LAYOUT1     TYPE LVC_S_LAYO.            "ALV布局工作区

DATA : GV_LIGHTS_NAME TYPE LVC_CIFNM VALUE 'LIGHT' ."红绿灯

*DATA: GCL_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID.

DATA: GS_FIELDCAT TYPE LVC_S_FCAT,              "字段工作区
      GT_FIELDCAT TYPE LVC_T_FCAT.
DATA: GS_FIELDCAT1 TYPE LVC_S_FCAT,              "字段工作区
      GT_FIELDCAT1 TYPE LVC_T_FCAT.
DEFINE MAC01 .
  CLEAR GS_FIELDCAT .
  GS_FIELDCAT-FIELDNAME     = &1 .
  GS_FIELDCAT-SCRTEXT_L     = &2 .
  GS_FIELDCAT-EDIT          = &3 .
  GS_FIELDCAT-KEY           = &4 .
  GS_FIELDCAT-REF_FIELD     = &5 .
  GS_FIELDCAT-REF_TABLE     = &6 .
  GS_FIELDCAT-NO_ZERO       = &7 .

  APPEND  GS_FIELDCAT TO  GT_FIELDCAT .
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
* TYPES
*&---------------------------------------------------------------------*
TYPES : BEGIN OF TY_LIKP ,
          VBELN TYPE ZFIT0025-VBELN,  "抬头交货

          BUKRS TYPE TVKO-BUKRS,      "公司代码
          VKORG TYPE ZFIT0025-VKORG,  "抬头销售组织
          WADAT TYPE ZFIT0025-WADAT,  "抬头
          KUNNR TYPE ZFIT0025-KUNNR,  "抬头客户
          NAME1 TYPE ZFIT0025-NAME1,  "抬头客户名称
          VTWEG TYPE ZFIT0025-VTWEG,  "抬头分销渠道
          ORT01 TYPE ZFIT0025-ORT01,  "抬头目的地
          LFIMG TYPE ZFIT0025-LFIMG,  "抬头发货数量（件）
          VOLUM TYPE ZFIT0025-VOLUM,  "抬头体积
          VOLEH TYPE ZFIT0025-VOLEH,  "抬头体积单位
          ZCYS1 TYPE ZFIT0025-ZCYS1,  "抬头承运商
          KBETR TYPE ZFIT0025-KBETR,  "抬头金额
          WAERS TYPE ZFIT0025-WAERS,  "抬头货币码


        END OF TY_LIKP .

TYPES : BEGIN OF TY_ALV .
          INCLUDE TYPE TY_LIKP .
          TYPES : SELEC     TYPE C,
          LIGHT(1)  TYPE C,
          MESS(100),
          MATNR     TYPE LIPS-MATNR,     "行项目的料号
          POSNR     TYPE LIPS-POSNR, "交货行项目
          LFIMGM    TYPE LIPS-LFIMG, "行项目发货数量（件）
          VGBEL     TYPE LIPS-VGBEL, "参考单据的单据编号
          PRCTR     TYPE LIPS-PRCTR, "参考码
          MAKTX     TYPE MAKT-MAKTX,
          VOLUMM    TYPE MARA-VOLUM,         "行项目单位体积
          VOLEHM    TYPE MARA-VOLEH,         "行项目体积单位
          ZYTJE     TYPE ZFIT0025-KBETR,     "预提金额
          ZCYJE     TYPE ZFIT0025-KBETR,     "差异金额
          ZCYHB     TYPE ZFIT0025-WAERS,     "差异币别
          GJAHR     TYPE BKPF-GJAHR,     "会计年度
          MONAT     TYPE BKPF-MONAT,     "会计期间
          ZYTPZ     TYPE ZFIT0026-ZYTPZ,  "预提凭证
          ZSJPZ     TYPE ZFIT0026-ZSJPZ,  "实际凭证
          ZZT01     TYPE ZFIT0026-ZZT01,  "实际状态
          KOSTL     TYPE ZFIT0027-KOSTL,  "成本中心
          ZZTMS     TYPE CHAR20,          "凭证状态描述
        END OF TY_ALV .

TYPES : BEGIN OF TY_KNA1 ,
          KUNNR TYPE KNA1-KUNNR, "客户编号
          NAME1 TYPE KNA1-NAME1, "客户名称
          ORT01 TYPE KNA1-ORT01, "目的地
        END OF TY_KNA1 .

TYPES : BEGIN OF TY_MARA ,
          MATNR TYPE MARA-MATNR, "物料
          VOLUM TYPE MARA-VOLUM, "业务量
          VOLEH TYPE MARA-VOLEH, "体积/重量单位
        END OF TY_MARA.

TYPES : BEGIN OF TY_LIPS ,

          VBELN  TYPE LIPS-VBELN, "交货
          MATNR  TYPE LIPS-MATNR, "行项目的料号
          POSNR  TYPE LIPS-POSNR, "交货行项目
          KCMENG TYPE LIPS-KCMENG, "发货数量（件）
          VGBEL  TYPE LIPS-VGBEL, "参考单据的单据编号
          VTWEG  TYPE LIPS-VTWEG,
          UMVKN  TYPE LIPS-UMVKN,
          UMVKZ  TYPE LIPS-UMVKZ,
          LFIMG  TYPE LIPS-LFIMG,
          PRCTR  TYPE LIPS-PRCTR, "参考码
        END OF TY_LIPS .

*&---------------------------------------------------------------------*
* GLOBAL DATA
*&---------------------------------------------------------------------*
DATA : GS_LIKP TYPE TY_LIKP .
DATA : GS_ALV  TYPE TY_ALV .
DATA : GS_KNA1 TYPE TY_KNA1 .
DATA : GS_MARA TYPE TY_MARA.
DATA : GS_MAKT TYPE MAKT.
DATA : GS_LIPS TYPE TY_LIPS.
DATA : GS_ZFIT0026 TYPE ZFIT0026.
DATA : GS_ZFIT0027 TYPE ZFIT0027.


*&---------------------------------------------------------------------*
* GLOBAL INTERNAL TABLES
*&---------------------------------------------------------------------*
DATA : GT_LIKP TYPE TABLE OF TY_LIKP .
DATA : GT_ALV  TYPE TABLE OF TY_ALV .
DATA : GT_KNA1 TYPE TABLE OF TY_KNA1 .
DATA : GT_MARA TYPE TABLE OF TY_MARA .
DATA : GT_MAKT TYPE TABLE OF MAKT .
DATA : GT_LIPS TYPE TABLE OF TY_LIPS .
DATA : GT_ZFIT0026 TYPE TABLE OF ZFIT0026 .
DATA : GT_ZFIT0027 TYPE TABLE OF ZFIT0027 .



*&---------------------------------------------------------------------*
* GLOBAL VARIABLE
*&---------------------------------------------------------------------*
FIELD-SYMBOLS: <FS_ALV> LIKE GS_ALV.
DATA: GR_ERDAT LIKE RANGE OF  LIPS-ERDAT.
DATA: GV_VOLUM TYPE LIPS-VOLUM.  "总体积
DATA: GV_KBETR TYPE ZFIT0025-KBETR.  "总金额

*&---------------------------------------------------------------------*
* SELECTION-SCREEN
*&---------------------------------------------------------------------*
TABLES : TVKO,BKPF,ZFIT0025,LIKP.
SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-B01.
PARAMETERS:
  P_BUKRS LIKE TVKO-BUKRS MODIF ID MI2.
SELECT-OPTIONS:
  S_GJAHR FOR BKPF-GJAHR NO INTERVALS NO-EXTENSION MODIF ID MI2,
  S_MONAT FOR BKPF-MONAT  NO-EXTENSION MODIF ID MI2..

PARAMETERS:
  R_X1 RADIOBUTTON GROUP RG1 USER-COMMAND UC1 DEFAULT 'X',
  R_X2 RADIOBUTTON GROUP RG1.

SELECTION-SCREEN BEGIN OF BLOCK B03 WITH FRAME TITLE TEXT-B03.
SELECT-OPTIONS: S_ZCYS1 FOR ZFIT0025-ZCYS1 NO INTERVALS NO-EXTENSION MODIF ID MI1,
                S_WADAT FOR LIKP-WADAT_IST NO-EXTENSION MODIF ID MI1."实际发货日期
PARAMETERS:

  P_KBETR LIKE ZFIT0025-KBETR MODIF ID MI1,
  P_WAERS LIKE ZFIT0025-WAERS MODIF ID MI1.

SELECTION-SCREEN END OF BLOCK B03.
SELECTION-SCREEN END OF BLOCK B01.

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  PERFORM SET_SCREEN_ATTR.  "先择屏幕做控制

AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON  VALUE-REQUEST FOR S_ZCYS1-LOW.
  PERFORM FRM_REQUEST_F4_INSMK USING 'S_ZCYS1-LOW'.

START-OF-SELECTION.
  IF P_BUKRS IS INITIAL OR
   S_GJAHR  IS INITIAL OR
   S_MONAT  IS INITIAL .
    MESSAGE '必填字段不能为空' TYPE 'S' DISPLAY LIKE 'E'. EXIT.
  ENDIF.

  CASE 'X'.
    WHEN R_X1.

    WHEN R_X2.
      IF
         S_ZCYS1 IS INITIAL OR
         P_KBETR IS INITIAL OR
         P_WAERS IS INITIAL .
        MESSAGE '必填字段不能为空' TYPE 'S' DISPLAY LIKE 'E'. EXIT.
      ENDIF.
  ENDCASE.


  PERFORM SUB_GET_DATA.             "获取数据
  PERFORM SUB_DEAL_DATA.            "处理数据

END-OF-SELECTION.

  PERFORM SUB_SHOW_ALV.


*&---------------------------------------------------------------------*
*& Form SUB_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SUB_GET_DATA .

  PERFORM SUB_GET_LIKP ."
  PERFORM SUB_GET_LIPS .
  PERFORM SUB_GET_KNA1 .
  PERFORM SUB_GET_MARA .
  PERFORM SUB_GET_MAKT .
  PERFORM SUB_GET_ZFIT0026 .
  PERFORM SUB_GET_ZFIT0025 .
  PERFORM SUB_GET_ZFIT0027 .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_LIKP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&-------------------------------------------------------------------~~*
FORM  SUB_GET_LIKP .
  CLEAR GT_LIKP .
  READ TABLE S_GJAHR INTO DATA(LS_GJAHR) INDEX 1.

  READ TABLE S_MONAT INTO DATA(LS_MONST) INDEX 1.
  DATA(MONTH_L) = SWITCH #( LS_MONST-LOW
                    WHEN '00' THEN '01'
                    ELSE LS_MONST-LOW
                          ).
  DATA(MONTH_H) = SWITCH #( LS_MONST-HIGH
                    WHEN '00' THEN LS_MONST-LOW
                    ELSE LS_MONST-HIGH
                     ).
  DATA(DATE_L) = CONV SY-DATUM( LS_GJAHR-LOW && MONTH_L && '01' ).
  DATA(DATE_H) = CONV SY-DATUM( LS_GJAHR-LOW && MONTH_H && '01' ).
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = DATE_H
    IMPORTING
      LAST_DAY_OF_MONTH = DATE_H
    EXCEPTIONS
      DAY_IN_NO_DATE    = 1
      OTHERS            = 2.
  IF SY-SUBRC <> 0.
    PERFORM STANDARD_MESSAGE.
  ENDIF.

  APPEND VALUE #( SIGN   = 'I'
                  OPTION = 'BT'
                  LOW    = DATE_L
                  HIGH   = DATE_H
                ) TO GR_ERDAT.

  CASE 'X'.
    WHEN R_X1.

      SELECT
        TVKO~BUKRS
        ZFIT0025~VBELN
        ZFIT0025~VKORG
        ZFIT0025~WADAT
        ZFIT0025~KUNNR
        ZFIT0025~NAME1
        ZFIT0025~VTWEG
        ZFIT0025~ORT01
        ZFIT0025~LFIMG
        ZFIT0025~VOLUM
        ZFIT0025~VOLEH
        ZFIT0025~ZCYS1
        ZFIT0025~KBETR
        ZFIT0025~WAERS

        INTO CORRESPONDING FIELDS OF TABLE GT_LIKP
        FROM TVKO
        INNER JOIN ZFIT0025 ON TVKO~VKORG =  ZFIT0025~VKORG
        WHERE TVKO~BUKRS = P_BUKRS
        AND   WADAT IN GR_ERDAT.


    WHEN R_X2.

      SELECT
        TVKO~BUKRS
        ZFIT0025~VBELN
        ZFIT0025~VKORG
        ZFIT0025~WADAT
        ZFIT0025~KUNNR
        ZFIT0025~NAME1
        ZFIT0025~VTWEG
        ZFIT0025~ORT01
        ZFIT0025~LFIMG
        ZFIT0025~VOLUM
        ZFIT0025~VOLEH
        ZFIT0025~ZCYS1
        ZFIT0025~KBETR
        ZFIT0025~WAERS

        INTO CORRESPONDING FIELDS OF TABLE GT_LIKP
        FROM TVKO
        INNER JOIN ZFIT0025 ON TVKO~VKORG =  ZFIT0025~VKORG
        WHERE TVKO~BUKRS = P_BUKRS
        AND   ZFIT0025~ZCYS1 IN S_ZCYS1
        AND   ZFIT0025~WADAT IN S_WADAT.

  ENDCASE.

  IF SY-SUBRC <> 0 .
    MESSAGE '没有符合条件的数据' TYPE 'S' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
  ENDIF .

  SORT GT_LIKP BY VBELN ASCENDING .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_LIPS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SUB_GET_LIPS .
  DATA : LT_LIKP TYPE TABLE OF TY_LIKP .
  LT_LIKP[] = GT_LIKP[] .
  SORT LT_LIKP BY VBELN.
  DELETE ADJACENT DUPLICATES FROM LT_LIKP
                COMPARING VBELN .

  CLEAR GT_LIPS[] .
  SELECT
    VBELN
    POSNR
    KCMENG
    MATNR
    VGBEL
    VTWEG
    UMVKN
    UMVKZ
    LFIMG
    PRCTR
    INTO CORRESPONDING FIELDS OF TABLE GT_LIPS
    FROM LIPS
    FOR ALL ENTRIES IN LT_LIKP
    WHERE VBELN = LT_LIKP-VBELN
    AND   CHARG <> ''.

  SORT GT_LIPS BY VBELN MATNR ASCENDING .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_KNA1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SUB_GET_KNA1 .
  DATA : LT_LIKP TYPE TABLE OF TY_LIKP .
  LT_LIKP[] = GT_LIKP[] .
  SORT LT_LIKP BY KUNNR.
  DELETE ADJACENT DUPLICATES FROM LT_LIKP
                COMPARING KUNNR .

  CLEAR GT_KNA1[] .
  SELECT
    KUNNR
    NAME1
    ORT01
    INTO CORRESPONDING FIELDS OF TABLE GT_KNA1
    FROM KNA1
    FOR ALL ENTRIES IN LT_LIKP
    WHERE KUNNR = LT_LIKP-KUNNR .

  SORT GT_KNA1 BY KUNNR ASCENDING .
  DELETE ADJACENT DUPLICATES FROM GT_KNA1
                 COMPARING KUNNR .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_MARA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SUB_GET_MARA .

  DATA : LT_LIPS TYPE TABLE OF TY_LIPS .
  IF GT_LIPS IS NOT INITIAL.
    LT_LIPS[] = GT_LIPS[] .
    SORT LT_LIPS BY MATNR .
    DELETE ADJACENT DUPLICATES FROM LT_LIPS
                  COMPARING MATNR.
    CLEAR GT_MARA[] .
    SELECT
      MATNR
      VOLUM
      VOLEH
      INTO CORRESPONDING FIELDS OF TABLE GT_MARA
      FROM MARA
      FOR ALL ENTRIES IN LT_LIPS
      WHERE MATNR = LT_LIPS-MATNR .

    SORT GT_MARA BY MATNR ASCENDING .
    DELETE ADJACENT DUPLICATES FROM GT_MARA
                   COMPARING MATNR.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_MAKT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SUB_GET_MAKT .
  DATA : LT_LIPS TYPE TABLE OF TY_LIPS .
  IF GT_LIPS IS NOT INITIAL.
    LT_LIPS[] = GT_LIPS[] .
    SORT LT_LIPS BY MATNR .
    DELETE ADJACENT DUPLICATES FROM LT_LIPS
                  COMPARING MATNR.

    CLEAR GT_MAKT[] .
    SELECT
      *
      INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
      FROM MAKT
      FOR ALL ENTRIES IN LT_LIPS
      WHERE MATNR = LT_LIPS-MATNR
      AND   SPRAS = SY-LANGU .

    SORT GT_MAKT BY MATNR ASCENDING .
    DELETE ADJACENT DUPLICATES FROM GT_MAKT
                   COMPARING MATNR.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_ZFIT0026
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SUB_GET_ZFIT0026 .
  DATA : LT_LIKP TYPE TABLE OF TY_LIKP .
  LT_LIKP[] = GT_LIKP[] .
  SORT LT_LIKP BY VBELN.
  DELETE ADJACENT DUPLICATES FROM LT_LIKP
                COMPARING VBELN .

  CLEAR GT_ZFIT0026[] .
  SELECT
     *
    INTO CORRESPONDING FIELDS OF TABLE GT_ZFIT0026
    FROM ZFIT0026
    FOR ALL ENTRIES IN LT_LIKP
    WHERE VBELN = LT_LIKP-VBELN.

  SORT GT_ZFIT0026 BY VBELN MATNR ASCENDING .
  DELETE ADJACENT DUPLICATES FROM GT_ZFIT0026
                 COMPARING VBELN MATNR.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_ZFIT0025
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SUB_GET_ZFIT0025 .
  CLEAR GV_VOLUM.
  SELECT SUM( VOLUM ) SUM( KBETR )  FROM ZFIT0025 INTO (GV_VOLUM,GV_KBETR) WHERE ZCYS1 IN S_ZCYS1  AND WADAT IN S_WADAT .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_ZFIT0027
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SUB_GET_ZFIT0027 .
  DATA : LT_LIKP TYPE TABLE OF TY_LIKP .
  LT_LIKP[] = GT_LIKP[] .
  SORT LT_LIKP BY VKORG VTWEG.
  DELETE ADJACENT DUPLICATES FROM LT_LIKP
                COMPARING VKORG VTWEG.

  CLEAR GT_ZFIT0027[] .
  SELECT
     *
    INTO CORRESPONDING FIELDS OF TABLE GT_ZFIT0027
    FROM ZFIT0027
    FOR ALL ENTRIES IN LT_LIKP
    WHERE VKORG = LT_LIKP-VKORG
    AND   VTWEG = LT_LIKP-VTWEG.

  SORT GT_ZFIT0027 BY VKORG VTWEG ASCENDING .
  DELETE ADJACENT DUPLICATES FROM GT_ZFIT0027
                 COMPARING VKORG VTWEG.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_DEAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SUB_DEAL_DATA .
  DATA:LV_ZCYJE  TYPE ZFIT0025-KBETR, "记录差异金额
       LV_LFIMGM TYPE LIPS-LFIMG,     "记录发货数量（件
       LV_VOLUMM TYPE LIPS-VOLUM,     "记录体积/重量'
       LV_ZYTJE  TYPE ZFIT0026-ZYTJE. "记录预提金额

  DATA:LS_LIPS TYPE TY_LIPS.

  READ TABLE GR_ERDAT INTO DATA(RS_ERDAT) INDEX 1. "读取会计期间的最后一天
  CLEAR GT_ALV .
  CLEAR GS_LIKP .
  CLEAR: LV_ZCYJE,LV_ZYTJE.

  P_KBETR = P_KBETR - GV_KBETR.

  LOOP AT GT_LIKP INTO GS_LIKP .

    CLEAR GS_ALV .
    MOVE-CORRESPONDING GS_LIKP TO GS_ALV .
    GS_ALV-BUKRS = P_BUKRS.

    GS_ALV-GJAHR = RS_ERDAT-HIGH+0(4).
    GS_ALV-MONAT = RS_ERDAT-HIGH+4(2).

    CLEAR GS_KNA1 .
    READ TABLE GT_KNA1 INTO GS_KNA1 WITH  KEY KUNNR = GS_ALV-KUNNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_ALV-NAME1 = GS_KNA1-NAME1 .
      GS_ALV-ORT01 = GS_KNA1-ORT01 .
      DATA(LV_VBELN) = GS_LIKP-VBELN.
    ENDIF.

    CLEAR GS_ZFIT0027.
    READ TABLE GT_ZFIT0027 INTO GS_ZFIT0027 WITH KEY VKORG = GS_ALV-VKORG  VTWEG = GS_ALV-VTWEG BINARY SEARCH.
    IF SY-SUBRC = 0.
      GS_ALV-KOSTL = GS_ZFIT0027-KOSTL.
    ENDIF.
    AT LAST.
      LOOP AT GT_LIPS INTO GS_LIPS WHERE VBELN = LV_VBELN.
        LV_LFIMGM = LV_LFIMGM + GS_LIPS-LFIMG .

        CLEAR GS_MARA.
        READ TABLE GT_MARA INTO GS_MARA WITH KEY MATNR = GS_LIPS-MATNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          LV_VOLUMM = LV_VOLUMM + GS_MARA-VOLUM * GS_LIPS-LFIMG."体积/重量' = "VOLUM * 发货数量
          GS_ALV-VOLEHM = GS_MARA-VOLEH."单位
        ENDIF.
        "最后一条做减法
        CLEAR LS_LIPS.
        LS_LIPS = GS_LIPS.
        AT END OF MATNR.

          GS_ALV-MATNR  = LS_LIPS-MATNR.
          GS_ALV-PRCTR  = LS_LIPS-PRCTR.
          GS_ALV-LFIMGM = LV_LFIMGM.
          GS_ALV-VOLUMM = LV_VOLUMM.
          CLEAR GS_MAKT .
          READ TABLE GT_MAKT INTO GS_MAKT WITH  KEY MATNR = GS_ALV-MATNR BINARY SEARCH.
          IF SY-SUBRC = 0.
            GS_ALV-MAKTX = GS_MAKT-MAKTX .
          ENDIF.

          CLEAR GS_ZFIT0026.
          READ TABLE GT_ZFIT0026 INTO GS_ZFIT0026 WITH KEY VBELN = GS_ALV-VBELN  MATNR = GS_ALV-MATNR.
          IF SY-SUBRC = 0.
            GS_ALV-ZYTPZ = GS_ZFIT0026-ZYTPZ.
            GS_ALV-ZSJPZ = GS_ZFIT0026-ZSJPZ.
            GS_ALV-ZZT01 = GS_ZFIT0026-ZZT01.
            GS_ALV-ZYTJE = GS_ZFIT0026-ZYTJE.
            GS_ALV-ZCYJE = GS_ZFIT0026-ZCYJE.
            GS_ALV-ZCYHB = GS_ZFIT0026-ZCYHB.

          ENDIF.
          GS_ALV-ZZTMS = SWITCH #( GS_ALV-ZZT01
               WHEN 0 THEN '未过账'
               WHEN 1 THEN '预提过账'
               WHEN 2 THEN '差异过账'
               WHEN ' ' THEN '未过账'  ).

          IF GS_ZFIT0026-ZZT01 IS INITIAL.
            GS_ALV-ZCYHB = P_WAERS.
          ENDIF.

          IF GS_ZFIT0026-ZZT01 = 0.
            GS_ALV-ZYTJE = GS_ALV-KBETR * ( GS_ALV-VOLUMM /  GS_ALV-VOLUM ).  "1)分摊金额=运费录入表金额*分摊表体积/录入表体积
          ENDIF.
          LV_ZYTJE = LV_ZYTJE + GS_ALV-ZYTJE. "记录预提金额。
          IF GS_ZFIT0026-ZZT01 <> 2.
            IF R_X2 = 'X'.
              IF P_KBETR <> 0.
                GS_ALV-ZCYJE = P_KBETR * ( GS_ALV-VOLUMM /  GV_VOLUM ). "2) 实际差额=实际金额*分摊表体积/录入表体积-预提金额（小数点差异摊入最后一行）

                AT LAST.
*                  IF LV_ZCYJE <> 0.
                    GS_ALV-ZCYJE = P_KBETR - LV_ZCYJE .  "屏幕数据金额 - 前面差异金额汇总  = 最后一条差异金额
*                  ENDIF.
                ENDAT.
                LV_ZCYJE = LV_ZCYJE + GS_ALV-ZCYJE.
              ENDIF.
            ENDIF.
          ENDIF.
*          GS_ALV-LIGHT = 2.
          APPEND GS_ALV TO GT_ALV .
          CLEAR:LV_LFIMGM,
                LV_VOLUMM.
        ENDAT.
        CLEAR GS_LIKP.
      ENDLOOP.

      EXIT.
    ENDAT.


    LOOP AT GT_LIPS INTO GS_LIPS WHERE VBELN = GS_LIKP-VBELN.
      LV_LFIMGM = LV_LFIMGM + GS_LIPS-LFIMG .

      CLEAR GS_MARA.
      READ TABLE GT_MARA INTO GS_MARA WITH KEY MATNR = GS_LIPS-MATNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        LV_VOLUMM = LV_VOLUMM + GS_MARA-VOLUM * GS_LIPS-LFIMG."体积/重量' = "VOLUM * 发货数量
        GS_ALV-VOLEHM = GS_MARA-VOLEH."单位
      ENDIF.
      CLEAR LS_LIPS.
      LS_LIPS = GS_LIPS.
      AT END OF MATNR.
        GS_ALV-MATNR  = LS_LIPS-MATNR.
        GS_ALV-PRCTR  = LS_LIPS-PRCTR.
        GS_ALV-LFIMGM = LV_LFIMGM.
        GS_ALV-VOLUMM = LV_VOLUMM.
        CLEAR GS_MAKT .
        READ TABLE GT_MAKT INTO GS_MAKT WITH  KEY MATNR = GS_ALV-MATNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          GS_ALV-MAKTX = GS_MAKT-MAKTX .
        ENDIF.

        CLEAR GS_ZFIT0026.
        READ TABLE GT_ZFIT0026 INTO GS_ZFIT0026 WITH KEY VBELN = GS_ALV-VBELN  MATNR = GS_ALV-MATNR.
        IF SY-SUBRC = 0.
          GS_ALV-ZYTPZ = GS_ZFIT0026-ZYTPZ.
          GS_ALV-ZSJPZ = GS_ZFIT0026-ZSJPZ.
          GS_ALV-ZZT01 = GS_ZFIT0026-ZZT01.
          GS_ALV-ZYTJE = GS_ZFIT0026-ZYTJE.
          GS_ALV-ZCYJE = GS_ZFIT0026-ZCYJE.
          GS_ALV-ZCYHB = GS_ZFIT0026-ZCYHB.
        ENDIF.
        GS_ALV-ZZTMS = SWITCH #( GS_ALV-ZZT01
         WHEN 0 THEN '未过账'
         WHEN 1 THEN '预提过账'
         WHEN 2 THEN '差异过账'
         WHEN ' ' THEN '未过账'  ).
        IF GS_ZFIT0026-ZCYHB IS INITIAL.
          GS_ALV-ZCYHB = P_WAERS.
        ENDIF.

        IF GS_ZFIT0026-ZZT01 = 0.
          GS_ALV-ZYTJE = GS_ALV-KBETR * ( GS_ALV-VOLUMM /  GS_ALV-VOLUM ).  "1)分摊金额=运费录入表金额*分摊表体积/录入表体积
        ENDIF.
        LV_ZYTJE = LV_ZYTJE + GS_ALV-ZYTJE. "记录预提金额。
        IF GS_ZFIT0026-ZZT01 <> 2.
          IF R_X2 = 'X'.
            IF P_KBETR <> 0.
              GS_ALV-ZCYJE = P_KBETR * ( GS_ALV-VOLUMM /  GV_VOLUM ). "2) 实际差额=实际金额*分摊表体积/录入表体积-预提金额（小数点差异摊入最后一行）

              LV_ZCYJE = LV_ZCYJE + GS_ALV-ZCYJE.
            ENDIF.
          ENDIF.
        ENDIF.
*        GS_ALV-LIGHT = 2.
        APPEND GS_ALV TO GT_ALV .
        CLEAR:LV_LFIMGM,
              LV_VOLUMM.
      ENDAT.
      CLEAR GS_LIKP.
    ENDLOOP.

  ENDLOOP .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_SHOW_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SUB_SHOW_ALV .

*-行属性
  CLEAR GS_LAYOUT.

  GS_LAYOUT-ZEBRA        = 'X' .              " 显示界面成色带交替
  GS_LAYOUT-SEL_MODE     = 'B' .              " 选择模式()
  GS_LAYOUT-DETAILINIT   = 'X' .              " 是否可以弹出细节屏幕
  GS_LAYOUT-CWIDTH_OPT   = 'X' .              " 优化宽度
  GS_LAYOUT-BOX_FNAME    = 'SELEC' .            " 指明复选框
  GS_LAYOUT-GRID_TITLE = 'Flight'(100).
  GS_LAYOUT-EXCP_FNAME = GV_LIGHTS_NAME.

*-列属性
  CLEAR GT_FIELDCAT[] .
*  MAC01 : 'MESS' '返回消息文本' '' '' '' '' ''.
  MAC01 : 'BUKRS' '公司代码' '' '' '' '' 'X'.
  MAC01 : 'GJAHR' '会计年度' '' '' '' '' ''.
  MAC01 : 'MONAT' '会计期间' '' '' '' '' 'X'.
  MAC01 : 'VKORG' '销售组织' '' '' '' '' 'X'.
  MAC01 : 'WADAT' '发货日期' '' '' '' '' ''.
  MAC01 : 'VBELN' 'ERP单号' '' '' '' '' 'X'.
  MAC01 : 'KUNNR' '客户代码' '' '' '' '' 'X'.
  MAC01 : 'NAME1' '客户名称' '' '' '' '' ''.
  MAC01 : 'VTWEG' '销售渠道' '' '' '' '' 'X'.
  MAC01 : 'ORT01' '目的地' '' '' '' '' ''.
  MAC01 : 'MATNR' '物料' '' '' '' '' 'X'.
  MAC01 : 'MAKTX' '物料描述' '' '' '' '' ''.
  MAC01 : 'LFIMGM' '发货数量（件）' '' '' '' '' ''.
  MAC01 : 'VOLUMM' '体积/重量' '' '' '' '' ''.
  MAC01 : 'VOLEHM' '体积/重量单位' '' '' '' '' ''.
  MAC01 : 'ZCYS1' '承运商' '' '' '' '' ''.
  MAC01 : 'ZYTJE' '预提金额' '' '' '' '' ''.
  MAC01 : 'WAERS' '货币' '' '' '' '' ''.
  MAC01 : 'ZCYJE' '差异金额' '' '' '' '' ''.
  MAC01 : 'ZCYHB' '货币' '' '' '' '' ''.
  MAC01 : 'ZYTPZ' '预提凭证' '' '' '' '' 'X'.
  MAC01 : 'ZSJPZ' '实际凭证' '' '' '' '' 'X'.
  MAC01 : 'ZZTMS' '凭证状态' '' '' '' '' ''.

*-ALV函数
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'SUB_STATUS'
      I_CALLBACK_USER_COMMAND  = 'SUB_USER_COMMAND'
      IS_LAYOUT_LVC            = GS_LAYOUT
      IT_FIELDCAT_LVC          = GT_FIELDCAT
      I_SAVE                   = 'A'
*     IT_EVENTS                = GT_EVENT[]
    TABLES
      T_OUTTAB                 = GT_ALV[]
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  SUB_STATUS
*&---------------------------------------------------------------------*
*       状态栏
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM SUB_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB .

  CLEAR:   RT_EXTAB.
  REFRESH: RT_EXTAB.
  SET PF-STATUS 'ZFIR0049' EXCLUDING RT_EXTAB .
*  SET TITLEBAR  'ZRQEVAC5001'  .

ENDFORM. " LVC_SET_STATUS
*&---------------------------------------------------------------------*
*&      FORM  SUB_USER_COMMAND
*&---------------------------------------------------------------------*
*       控制按钮
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*

FORM SUB_USER_COMMAND USING P_UCOMM TYPE SY-UCOMM
                             P_RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA:
   LS_GRID     TYPE REF TO CL_GUI_ALV_GRID.         "GRID定义

* 取得屏幕更新数据
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = LS_GRID.

  CALL METHOD LS_GRID->CHECK_CHANGED_DATA.

  CASE P_UCOMM.
    WHEN '&IC1' .   " 判断用户的动作
      CLEAR GS_ALV.
      READ TABLE GT_ALV INTO GS_ALV INDEX P_RS_SELFIELD-TABINDEX.

      CASE  P_RS_SELFIELD-FIELDNAME .
        WHEN 'ZYTPZ' .
          SET PARAMETER ID 'BLN' FIELD GS_ALV-ZYTPZ.
          SET PARAMETER ID 'BUK' FIELD GS_ALV-BUKRS.
          SET PARAMETER ID 'GJR' FIELD GS_ALV-GJAHR.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        WHEN 'ZSJPZ'.
          SET PARAMETER ID 'BLN' FIELD GS_ALV-ZSJPZ.
          SET PARAMETER ID 'BUK' FIELD GS_ALV-BUKRS.
          SET PARAMETER ID 'GJR' FIELD GS_ALV-GJAHR.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        WHEN 'VBELN'.
          SET PARAMETER ID 'VL' FIELD GS_ALV-VBELN.
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.

      ENDCASE .

    WHEN '&CS'.
      PERFORM FRM_CALL_BAPI USING 'X'.  "测试过账
    WHEN 'GZ'.
      PERFORM FRM_CALL_BAPI USING ''.   "测试过账
  ENDCASE.

*-同时内表中的数据也随之刷新
  P_RS_SELFIELD-COL_STABLE = 'X' .   " 行
  P_RS_SELFIELD-ROW_STABLE = 'X' .   " 列
  P_RS_SELFIELD-REFRESH    = 'X' .   " 当用户在显式界面上对数据进行修改时，

ENDFORM. " LVC_USER_COMMAND

*&---------------------------------------------------------------------*
*& Form SET_SCREEN_ATTR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_SCREEN_ATTR .
  CASE 'X'.
    WHEN R_X1.
      LOOP AT SCREEN.
        CASE SCREEN-GROUP1.
          WHEN 'MI2'.
            IF SCREEN-NAME NS 'HIGH'.
              SCREEN-REQUIRED = '2'.
            ENDIF.
          WHEN 'MI1'.
            SCREEN-ACTIVE = '0'.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.

    WHEN R_X2.
      LOOP AT SCREEN.
        CASE SCREEN-GROUP1.
          WHEN 'MI1' OR 'MI2'.
            IF SCREEN-NAME NS 'HIGH'.
              SCREEN-REQUIRED = '2'.
            ENDIF.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CALL_BAPI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM FRM_CALL_BAPI  USING P_TEST.
  DATA: LS_ZFIT0026 TYPE ZFIT0026,
        LT_ZFIT0026 TYPE TABLE OF ZFIT0026.

  DATA LT_ERRORMSG TYPE TSMESG.
  DATA LS_ERRORMSG LIKE LINE OF LT_ERRORMSG.

  DATA: DOCUMENTHEADER       TYPE BAPIACHE09,
        LS_ACCOUNTGL         TYPE BAPIACGL09,
        LT_ACCOUNTGL         TYPE TABLE OF BAPIACGL09,
        LS_ACCOUNTRECEIVABLE TYPE BAPIACAR09,
        LT_ACCOUNTRECEIVABLE TYPE TABLE OF BAPIACAR09,
        LS_ACCOUNTPAYABLE    TYPE BAPIACAP09,
        LT_ACCOUNTPAYABLE    TYPE TABLE OF BAPIACAP09,
        LS_EXTENSION2        TYPE BAPIPAREX,
        LT_EXTENSION2        TYPE TABLE OF BAPIPAREX,
        LS_CURRENCYAMOUNT    TYPE BAPIACCR09,
        LT_CURRENCYAMOUNT    TYPE TABLE OF BAPIACCR09,
        ES_RETURN            TYPE BAPIRET2,
        ET_RETURN            TYPE TABLE OF BAPIRET2.


  DATA: LS_EXTEN2 TYPE ZFIS_ACC_DOCUMENT,
        L_TYPE    TYPE BAPIACHE09-OBJ_TYPE,
        L_KEY     TYPE BAPIACHE09-OBJ_KEY,
        L_SYS     TYPE BAPIACHE09-OBJ_SYS,
        L_ITEMNO1 TYPE BAPIACGL09-ITEMNO_ACC,
        L_ITEMNO2 TYPE BAPIACGL09-ITEMNO_ACC.

  DATA: LT_ALV    TYPE TABLE OF TY_ALV.
  DATA: LV_ZYTJE  TYPE ZFIT0025-KBETR.  "预提过账计算
  DATA: LV_ZCYJE  TYPE ZFIT0025-KBETR.  "差异过账计算
  DATA: lv_text   type BAPIACGL09-item_text.  "行项目文本                                                                    "add by 04815 S4DK904689


  READ TABLE GT_ALV TRANSPORTING NO FIELDS WITH KEY SELEC = ABAP_TRUE.
  IF SY-SUBRC GT 0.
    MESSAGE '未选中有效数据！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*  LT_ALV[] = GT_ALV[].
*  DELETE LT_ALV WHERE SELEC  = SPACE.
*  DELETE ADJACENT DUPLICATES FROM LT_ALV
*               COMPARING VBELN MATNR.
  CLEAR:LT_ZFIT0026.
  READ TABLE GR_ERDAT INTO DATA(RS_ERDAT) INDEX 1. "读取会计期间的最后一天
  CLEAR: DOCUMENTHEADER, LT_ACCOUNTGL[], LT_ACCOUNTRECEIVABLE[], LT_ACCOUNTPAYABLE[], LT_CURRENCYAMOUNT[], LT_EXTENSION2[].
  CLEAR:L_ITEMNO1,L_ITEMNO2.

  LOOP AT GT_ALV ASSIGNING <FS_ALV> WHERE SELEC  = 'X'.
    CASE 'X'.             "判断如果状态是1才能过2，0才能过1，
      WHEN R_X1.
        IF <FS_ALV>-ZZT01 <> 0.
          CONTINUE.
        ENDIF.
        IF  <FS_ALV>-ZYTJE = 0.
          CONTINUE.
        ENDIF.
      WHEN R_X2.
        IF <FS_ALV>-ZZT01 <> 1.
          CONTINUE.
        ENDIF.
        IF  <FS_ALV>-ZCYJE = 0.
          CONTINUE.
        ENDIF.
    ENDCASE.
    L_ITEMNO1 =  L_ITEMNO2 + 1.
    L_ITEMNO2 =  L_ITEMNO1 + 1.
    CLEAR:<FS_ALV>-MESS.

    DOCUMENTHEADER-BUS_ACT    = 'RFBU'.
    DOCUMENTHEADER-USERNAME   = SY-UNAME.        "系统当前用户
    DOCUMENTHEADER-COMP_CODE  = <FS_ALV>-BUKRS.   "公司代码

    DOCUMENTHEADER-DOC_DATE   = RS_ERDAT-HIGH.   "凭证中的凭证日期
    DOCUMENTHEADER-PSTNG_DATE = RS_ERDAT-HIGH.   "凭证中的记帐日期

*    DOCUMENTHEADER-TRANS_DATE = SY-DATUM.        "换算日期

    DOCUMENTHEADER-FISC_YEAR  = <FS_ALV>-GJAHR."会计年度
    DOCUMENTHEADER-FIS_PERIOD = <FS_ALV>-MONAT.   "会计期间

    DOCUMENTHEADER-DOC_TYPE   = 'SA'.   "凭证类型
*    DOCUMENTHEADER-REF_DOC_NO = <FS_ALV>-XBLNR.   "参考凭证号
    IF R_X1 = 'X'.
      DOCUMENTHEADER-HEADER_TXT =  '运费预提分摊过账-' && <FS_ALV>-zcys1 && <FS_ALV>-WADAT+0(6).   "凭证抬头文本              "modify by 04815 S4DK904689
      lv_text = '运费预提分摊过账-' && <FS_ALV>-zcys1 && '-'.                                                                "add by 04815 S4DK904689
      "预提和实际金额不一样，所以要分开写
      CLEAR LV_ZYTJE.
      IF <FS_ALV>-ZYTJE > 0.
        PERFORM FRM_CALL_ACCOUNTGL  TABLES LT_ACCOUNTGL  USING L_ITEMNO1 '6910180102' RS_ERDAT-HIGH lv_text. "总帐科目项     "modify by 04815 S4DK904689
        PERFORM FRM_CALL_ACCOUNTGL  TABLES LT_ACCOUNTGL  USING L_ITEMNO2 '2241031900' RS_ERDAT-HIGH lv_text.                "modify by 04815 S4DK904689

        PERFORM FRM_CALL_CURRENCYAMOUNT  TABLES LT_CURRENCYAMOUNT  USING L_ITEMNO1 <FS_ALV>-ZYTJE.  "货币项目
        LV_ZYTJE = <FS_ALV>-ZYTJE * -1.
        PERFORM FRM_CALL_CURRENCYAMOUNT  TABLES LT_CURRENCYAMOUNT  USING L_ITEMNO2 LV_ZYTJE.

        PERFORM FRM_CALL_EXTEN2  TABLES LT_EXTENSION2  USING L_ITEMNO1 '40'.     "增强字段
        PERFORM FRM_CALL_EXTEN2  TABLES LT_EXTENSION2  USING L_ITEMNO2 '50'..
      ELSE.
        PERFORM FRM_CALL_ACCOUNTGL  TABLES LT_ACCOUNTGL  USING L_ITEMNO1 '6910180102' RS_ERDAT-HIGH lv_text.                "modify by 04815 S4DK904689
        PERFORM FRM_CALL_ACCOUNTGL  TABLES LT_ACCOUNTGL  USING L_ITEMNO2 '2241031900' RS_ERDAT-HIGH lv_text.                "modify by 04815 S4DK904689

        PERFORM FRM_CALL_CURRENCYAMOUNT  TABLES LT_CURRENCYAMOUNT  USING L_ITEMNO1 <FS_ALV>-ZYTJE.
        LV_ZYTJE = <FS_ALV>-ZYTJE * -1.
        PERFORM FRM_CALL_CURRENCYAMOUNT  TABLES LT_CURRENCYAMOUNT  USING L_ITEMNO2 LV_ZYTJE.

        PERFORM FRM_CALL_EXTEN2  TABLES LT_EXTENSION2  USING L_ITEMNO1 '50'.     "增强字段
        PERFORM FRM_CALL_EXTEN2  TABLES LT_EXTENSION2  USING L_ITEMNO2 '40'.

      ENDIF.

    ELSE.
      DOCUMENTHEADER-HEADER_TXT = '实际运费差额过账'&& <FS_ALV>-zcys1 && <FS_ALV>-WADAT.   "凭证抬头文本                      "modify by 04815 S4DK904689
      lv_text = '实际运费差额过账-' && <FS_ALV>-zcys1 && '-'.                                                                "add by 04815 S4DK904689
      CLEAR LV_ZCYJE.
      IF <FS_ALV>-ZCYJE > 0.
        PERFORM FRM_CALL_ACCOUNTGL  TABLES LT_ACCOUNTGL  USING L_ITEMNO1 '6910180102' RS_ERDAT-HIGH lv_text. "总帐科目项     "modify by 04815 S4DK904689
        PERFORM FRM_CALL_ACCOUNTGL  TABLES LT_ACCOUNTGL  USING L_ITEMNO2 '2241031900' RS_ERDAT-HIGH lv_text.                "modify by 04815 S4DK904689

        PERFORM FRM_CALL_CURRENCYAMOUNT  TABLES LT_CURRENCYAMOUNT  USING L_ITEMNO1 <FS_ALV>-ZCYJE.  "货币项目
        LV_ZCYJE = <FS_ALV>-ZCYJE * -1.
        PERFORM FRM_CALL_CURRENCYAMOUNT  TABLES LT_CURRENCYAMOUNT  USING L_ITEMNO2 LV_ZCYJE.

        PERFORM FRM_CALL_EXTEN2  TABLES LT_EXTENSION2  USING L_ITEMNO1 '40'.     "增强字段
        PERFORM FRM_CALL_EXTEN2  TABLES LT_EXTENSION2  USING L_ITEMNO2 '50'..
      ELSE.
        PERFORM FRM_CALL_ACCOUNTGL  TABLES LT_ACCOUNTGL  USING L_ITEMNO1 '6910180102' RS_ERDAT-HIGH lv_text.                "modify by 04815 S4DK904689
        PERFORM FRM_CALL_ACCOUNTGL  TABLES LT_ACCOUNTGL  USING L_ITEMNO2 '2241031900' RS_ERDAT-HIGH lv_text.                "modify by 04815 S4DK904689

        PERFORM FRM_CALL_CURRENCYAMOUNT  TABLES LT_CURRENCYAMOUNT  USING L_ITEMNO1 <FS_ALV>-ZCYJE.
        LV_ZCYJE = <FS_ALV>-ZCYJE * -1.
        PERFORM FRM_CALL_CURRENCYAMOUNT  TABLES LT_CURRENCYAMOUNT  USING L_ITEMNO2 LV_ZCYJE.

        PERFORM FRM_CALL_EXTEN2  TABLES LT_EXTENSION2  USING L_ITEMNO1 '50'.     "增强字段
        PERFORM FRM_CALL_EXTEN2  TABLES LT_EXTENSION2  USING L_ITEMNO2 '40'.

      ENDIF.
    ENDIF.
  ENDLOOP.
  CLEAR ET_RETURN[].

  IF P_TEST EQ 'X'.
    IF DOCUMENTHEADER IS NOT INITIAL.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          DOCUMENTHEADER    = DOCUMENTHEADER       "录入凭证头
        TABLES
          ACCOUNTGL         = LT_ACCOUNTGL         "总帐科目项
          ACCOUNTRECEIVABLE = LT_ACCOUNTRECEIVABLE "客户项目
          ACCOUNTPAYABLE    = LT_ACCOUNTPAYABLE    "供应商项目
          CURRENCYAMOUNT    = LT_CURRENCYAMOUNT    "货币项目
          RETURN            = ET_RETURN            "返回值
          EXTENSION2        = LT_EXTENSION2.       "扩展字段
      LOOP AT ET_RETURN INTO ES_RETURN WHERE TYPE = 'A' OR TYPE = 'E' .
*      <FS_ALV>-LIGHT = 1.
        LS_ERRORMSG-MSGTY = ES_RETURN-TYPE.
        LS_ERRORMSG-ARBGB = ES_RETURN-ID.
        LS_ERRORMSG-TXTNR = ES_RETURN-NUMBER.
        LS_ERRORMSG-MSGV1 = ES_RETURN-MESSAGE.
        APPEND LS_ERRORMSG TO LT_ERRORMSG.
      ENDLOOP.
      IF SY-SUBRC NE 0.
        MESSAGE S000(OO) WITH '测试成功'.
      ENDIF.
    ELSE.
      MESSAGE S000(OO) WITH '测试成功'.
    ENDIF.

  ELSE.
    IF DOCUMENTHEADER IS NOT INITIAL.
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          DOCUMENTHEADER    = DOCUMENTHEADER       "录入凭证头
        IMPORTING
          OBJ_TYPE          = L_TYPE
          OBJ_KEY           = L_KEY
          OBJ_SYS           = L_SYS
        TABLES
          ACCOUNTGL         = LT_ACCOUNTGL         "总帐科目项
          ACCOUNTRECEIVABLE = LT_ACCOUNTRECEIVABLE "客户项目
          ACCOUNTPAYABLE    = LT_ACCOUNTPAYABLE    "供应商项目
          CURRENCYAMOUNT    = LT_CURRENCYAMOUNT    "货币项目
          RETURN            = ET_RETURN            "返回值
          EXTENSION2        = LT_EXTENSION2.       "扩展字段

      LOOP AT ET_RETURN INTO ES_RETURN WHERE TYPE = 'A' OR TYPE = 'E' .
*      <FS_ALV>-LIGHT = 1.
        LS_ERRORMSG-MSGTY = ES_RETURN-TYPE.
        LS_ERRORMSG-ARBGB = ES_RETURN-ID.
        LS_ERRORMSG-TXTNR = ES_RETURN-NUMBER.
        LS_ERRORMSG-MSGV1 = ES_RETURN-MESSAGE.
        APPEND LS_ERRORMSG TO LT_ERRORMSG.
      ENDLOOP.

      IF SY-SUBRC EQ 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CLEAR: LS_ZFIT0026.
        <FS_ALV>-LIGHT = 3.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING WAIT = 'X'.

      ENDIF.
    ENDIF.
  ENDIF.

  IF LT_ERRORMSG[] IS NOT INITIAL.
    CALL FUNCTION 'FB_MESSAGES_DISPLAY_POPUP'
      EXPORTING
        IT_SMESG        = LT_ERRORMSG[]
      EXCEPTIONS
        NO_MESSAGES     = 1
        POPUP_CANCELLED = 2
        OTHERS          = 3.
  ELSE.
    IF P_TEST EQ ''.
      LOOP AT GT_ALV ASSIGNING <FS_ALV> WHERE SELEC  = 'X' .
        CASE 'X'.             "判断如果状态是1才能过2，0才能过1，
          WHEN R_X1.
            IF <FS_ALV>-ZZT01 <> 0.
              <FS_ALV>-LIGHT = 1.
              <FS_ALV>-MESS ='请选择未过账的状态'.
              CONTINUE.
            ENDIF.
            <FS_ALV>-ZZTMS = '预提过账'.

            <FS_ALV>-ZYTPZ = L_KEY+0(10).
            <FS_ALV>-ZZT01 = 1.
            LS_ZFIT0026-ZYTRQ = SY-DATUM.
            LS_ZFIT0026-ZYTSJ = SY-UZEIT.
            LS_ZFIT0026-ZYTNA = SY-UNAME.

          WHEN R_X2.
            IF <FS_ALV>-ZZT01 <> 1.
              <FS_ALV>-LIGHT = 1.
              <FS_ALV>-MESS ='请选择预提过账的状态'.
              CONTINUE.
            ENDIF.
            <FS_ALV>-ZZTMS = '差异过账'.
            <FS_ALV>-ZSJPZ = L_KEY+0(10).
            <FS_ALV>-ZZT01 = 2.
            LS_ZFIT0026-ZCYRQ = SY-DATUM.
            LS_ZFIT0026-ZCYSJ = SY-UZEIT.
            LS_ZFIT0026-ZCYNA = SY-UNAME.
        ENDCASE.
        <FS_ALV>-LIGHT = 3.
        MOVE-CORRESPONDING <FS_ALV> TO LS_ZFIT0026 .

        LS_ZFIT0026-LFIMG = <FS_ALV>-LFIMGM.
        LS_ZFIT0026-VOLUM = <FS_ALV>-VOLUMM.
        LS_ZFIT0026-VOLEH = <FS_ALV>-VOLEHM.
        LS_ZFIT0026-ZCYHB = <FS_ALV>-ZCYHB.

        APPEND LS_ZFIT0026 TO LT_ZFIT0026.
        CLEAR LS_ZFIT0026.

      ENDLOOP.

    ENDIF.
  ENDIF.

  IF P_TEST EQ ''.
    IF LT_ZFIT0026 IS NOT INITIAL.
      MODIFY ZFIT0026 FROM TABLE LT_ZFIT0026.
      COMMIT WORK.
      MESSAGE S000(OO) WITH '审核成功'.
    ELSE.
      MESSAGE '审核失败' TYPE 'S' DISPLAY LIKE 'E' .
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REQUEST_F4_INSMK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FRM_REQUEST_F4_INSMK USING PV_FIELD TYPE HELP_INFO-DYNPROFLD.


  DATA: BEGIN OF LS_VALUE_TAB,
          ZCYS1 TYPE ZFIT0025-ZCYS1,
        END OF LS_VALUE_TAB.
  DATA: LT_VALUE_TAB LIKE TABLE OF LS_VALUE_TAB.

  SELECT  DISTINCT
       ZCYS1
       INTO CORRESPONDING FIELDS OF TABLE LT_VALUE_TAB
       FROM ZFIT0025.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'ZCYS1'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = PV_FIELD
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = LT_VALUE_TAB
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  IF SY-SUBRC <> 0.
*   Implement suitable error handling here
  ENDIF.

ENDFORM.
FORM STANDARD_MESSAGE.

  SY-MSGTY = COND #( WHEN SY-MSGTY = '' THEN 'E' ELSE SY-MSGTY ).
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH
  SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CALL_ACCOUNTGL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ACCOUNTGL
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM FRM_CALL_ACCOUNTGL  TABLES   PT_ACCOUNTGL STRUCTURE  BAPIACGL09
                          USING   P_ITEMNO_ACC
                                  P_GL_ACCOUNT
                                  P_HIGH
                                  P_TEXT.

  DATA:LS_ACCOUNTGL         TYPE BAPIACGL09.

  LS_ACCOUNTGL-ITEMNO_ACC = P_ITEMNO_ACC.           "会计凭证行项目编号
  LS_ACCOUNTGL-GL_ACCOUNT = P_GL_ACCOUNT.
  LS_ACCOUNTGL-DOC_TYPE   = 'SA'.
  LS_ACCOUNTGL-COMP_CODE  = <FS_ALV>-BUKRS.
  LS_ACCOUNTGL-FIS_PERIOD = <FS_ALV>-MONAT.
  LS_ACCOUNTGL-FISC_YEAR  = <FS_ALV>-GJAHR.
  LS_ACCOUNTGL-PSTNG_DATE = P_HIGH.
  LS_ACCOUNTGL-MATERIAL_LONG = <FS_ALV>-MATNR.

  DATA(LV_MATNR)           = |{ <FS_ALV>-MATNR ALPHA = OUT }|. "短物料
  LS_ACCOUNTGL-MATERIAL    = LV_MATNR.
  LS_ACCOUNTGL-REF_KEY_1   = <FS_ALV>-PRCTR.
  LS_ACCOUNTGL-COSTCENTER  = <FS_ALV>-KOSTL.
  LS_ACCOUNTGL-ITEM_TEXT   = P_TEXT && <FS_ALV>-WADAT+0(6).
  APPEND LS_ACCOUNTGL TO PT_ACCOUNTGL. CLEAR LS_ACCOUNTGL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CALL_CURRENCYAMOUNT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_CURRENCYAMOUNT
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM FRM_CALL_CURRENCYAMOUNT  TABLES   PT_CURRENCYAMOUNT STRUCTURE BAPIACCR09
                              USING  P_ITEMNO_ACC P_ZYTJE.
  DATA  LS_CURRENCYAMOUNT    TYPE BAPIACCR09.

  LS_CURRENCYAMOUNT-ITEMNO_ACC = P_ITEMNO_ACC.
  LS_CURRENCYAMOUNT-CURR_TYPE  = '00'.
  LS_CURRENCYAMOUNT-CURRENCY = <FS_ALV>-WAERS.
  LS_CURRENCYAMOUNT-AMT_DOCCUR = P_ZYTJE . "金额
  APPEND LS_CURRENCYAMOUNT TO PT_CURRENCYAMOUNT. CLEAR  LS_CURRENCYAMOUNT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CALL_EXTEN2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_EXTENSION2
*&      --> P_
*&---------------------------------------------------------------------*
FORM FRM_CALL_EXTEN2  TABLES PT_EXTENSION2 STRUCTURE BAPIPAREX
                       USING P_POSNR P_BSCHL.

  DATA: LS_EXTENSION2 TYPE BAPIPAREX,
        LS_EXTEN2     TYPE ZFIS_ACC_DOCUMENT.

  CLEAR LS_EXTEN2.
  LS_EXTEN2-POSNR = P_POSNR.
  LS_EXTEN2-BSCHL = P_BSCHL. "记账码
  LS_EXTENSION2-STRUCTURE  = 'ZFIS_ACC_DOCUMENT'.
  LS_EXTENSION2-VALUEPART1 = LS_EXTEN2.

  APPEND LS_EXTENSION2 TO PT_EXTENSION2. CLEAR LS_EXTENSION2.

ENDFORM.