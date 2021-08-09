*&---------------------------------------------------------------------*
*& 包含               ZMMR0050_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form FRM_REFRESH_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> G_GUI_ALV
*&      --> GS_LAYOUT
*&---------------------------------------------------------------------*
FORM FRM_REFRESH_ALV  USING  P_GRID TYPE REF TO  CL_GUI_ALV_GRID
                              P_LAY  TYPE LVC_S_LAYO.
  DATA: LS_STABLE TYPE LVC_S_STBL.
  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.

*  CLEAR I_SORT.
*  CLEAR I_SORT[].
*  I_SORT-FIELDNAME = 'ZDZDH'.
*  I_SORT-SPOS = 1.
*  I_SORT-UP = 'X'.
*  I_SORT-SUBTOT = 'X'.
*  APPEND I_SORT  .

  CALL METHOD P_GRID->SET_FRONTEND_LAYOUT
    EXPORTING
      IS_LAYOUT = P_LAY.

*  CALL METHOD P_GRID->SET_FRONTEND_FIELDCATALOG
*    EXPORTING
*      IT_FIELDCATALOG = GT_FIELDCAT.
*
*  CALL METHOD P_GRID->SET_FLAG_TOTAL_COLUMN
*    EXPORTING
*      I_UCOMM = '&SUBTOT'.

  CALL METHOD P_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_STABLE
      I_SOFT_REFRESH = 'X'.


*  DATA LS_FCAT TYPE LVC_S_FCAT .
*  DATA LT_FCAT TYPE LVC_T_FCAT .
*  DATA LS_LAYOUT TYPE LVC_S_LAYO .
*  CALL METHOD P_GRID->GET_FRONTEND_FIELDCATALOG
*    IMPORTING
*      ET_FIELDCATALOG = LT_FCAT[].
*  LOOP AT LT_FCAT INTO LS_FCAT .
*    IF LS_FCAT-FIELDNAME = 'PAYMENTSUM' .
*      LS_FCAT-NO_OUT = SPACE .
*      MODIFY LT_FCAT FROM LS_FCAT .
*    ENDIF .
*  ENDLOOP .
*  CALL METHOD P_GRID->SET_FRONTEND_FIELDCATALOG
*    EXPORTING
*      IT_FIELDCATALOG = LT_FCAT[].
*
*  CALL METHOD P_GRID->GET_FRONTEND_LAYOUT
*    IMPORTING
*      ES_LAYOUT = LS_LAYOUT.
*  LS_LAYOUT-GRID_TITLE = 'Flights (with Payment Sums)' .
*  CALL METHOD P_GRID->SET_FRONTEND_LAYOUT
*    EXPORTING
*      IS_LAYOUT = LS_LAYOUT.






ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SUB_GET_DATA .
  PERFORM FRM_GET_ZDZZTMS.

  CASE 'X'.
    WHEN RB_01 .

      PERFORM FRM_GET_EKBE.

      PERFORM FRM_GET_RSEG .

      PERFORM FRM_GET_MSEG. "获取冲销凭证

      PERFORM FRM_GET_DZXX.  "获取以对账信息

      PERFORM FRM_GET_CABN.  "获取长批号

      PERFORM FRM_GET_ZSL.  " 获取税率

      PERFORM FRM_GET_OUT.   "最终数据数据处理

    WHEN RB_03 OR RB_02.
      PERFORM FRM_GET_DATA2.

    WHEN RB_04.
      PERFORM FRM_GET_DATA3.

  ENDCASE.
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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_EKBE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_EKBE .

  SELECT EKBE~EBELN
         EKBE~EBELP
         EKBE~GJAHR
   	     EKBE~BELNR
         EKBE~BUZEI
*         EKPO~EBELN
         EKKO~ERNAM AS NAMPO
         EKKO~LIFNR
         EKBE~WERKS
         EKBE~MATNR
         EKBE~CHARG
         EKBE~BEWTP
         EKBE~BWART
         EKBE~SHKZG
         EKBE~BUDAT
         EKBE~MENGE
         EKPO~MEINS
         EKPO~UMREZ
         EKPO~UMREN
         EKBE~HSWAE
         EKPO~MWSKZ
         EKBE~DMBTR
         EKBE~WAERS
         MAKT~MAKTX
         EKBE~LFGJA
         EKBE~LFBNR
         EKBE~LFPOS
         FROM EKBE
         INNER JOIN EKPO
         ON  EKBE~EBELN  = EKPO~EBELN
         AND EKBE~EBELP  = EKPO~EBELP
         INNER JOIN EKKO
         ON  EKBE~EBELN = EKKO~EBELN
         INNER JOIN MAKT
         ON EKPO~MATNR = MAKT~MATNR
         INTO CORRESPONDING FIELDS OF TABLE GT_OUT
         WHERE EKKO~ERNAM IN  S_ERNAM
         AND   EKKO~LIFNR IN   S_LIFNR
         AND   EKBE~BUDAT IN   S_BUDAT
         AND   EKBE~EBELN IN   S_EBELN
         AND   EKBE~CHARG IN   S_CHARG
         AND   EKBE~WAERS IN   S_WAERS
         AND   EKKO~BSART IN   S_BSART
         AND   EKBE~WERKS IN   S_WERKS
         AND   EKPO~MATNR IN   S_MATNR
         AND   EKPO~REPOS <> ''
         AND   EKPO~KNTTP <> 'A'
*         AND   EKPO~PSTYP <> '2'
         AND   EKPO~PSTYP IN   S_PSTYP
         AND   EKBE~BEWTP = 'E'
*         AND   EXISTS ( SELECT * FROM MSEG WHERE MJAHR   = EKBE-GJAHR
*                                             AND   MJAHR   = EKBE-BELNR
*                                             AND   ZEILE   = EKBE-BUZEI
*                                             AND   SMBLN   = ''  ). "排除已经冲销的凭证
*         AND NOT EXISTS ( SELECT * FROM EKBE WHERE  BWART = ''
*                                             AND    LFGJA   = EKBE-GJAHR
*                                             AND   LFBNR   = EKBE-BELNR
*                                             AND   LFPOS   = EKBE-BUZEI   )"排除已经发票检验过的
*       AND NOT EXISTS ( SELECT * FROM RSEG
*                              INNER JOIN RBKP
*                              ON RBKP~BELNR =  RSEG~BELNR
*                             AND RBKP~GJAHR =  RSEG~GJAHR
*                               WHERE RSEG~LFGJA = EKBE~GJAHR
*                                AND  RSEG~LFBNR = EKBE~BELNR
*                                AND  RSEG~LFPOS = EKBE~BUZEI
*                                AND  RBKP~STBLG = ''   )
    .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_DATA2 .


  IF  RB_02 = 'X'.
    SELECT  ZMMT0004~ZDZDH
            ZMMT0004~ZDZXH
            ZMMT0004~ZDZZT
            ZMMT0004~EBELN
            ZMMT0004~EBELP
            ZMMT0004~GJAHR
            ZMMT0004~BELNR
            ZMMT0004~BUZEI
            ZMMT0004~LIFNR
            ZMMT0004~NAME1
            ZMMT0004~WERKS
            ZMMT0004~MATNR
            ZMMT0004~MAKTX
            ZMMT0004~ATWRT
            ZMMT0004~CHARG
            ZMMT0004~BEWTP
            ZMMT0004~BWART
            ZMMT0004~SHKZG
            ZMMT0004~BUDAT
            ZMMT0004~MENGE
            ZMMT0004~MEINS
            ZMMT0004~ZDWHSL
            ZMMT0004~ZSHSL
            ZMMT0004~HSWAE
            ZMMT0004~MWSKZ
            ZMMT0004~DMBTR
            ZMMT0004~ZSL
            ZMMT0004~ZHSDJ
            ZMMT0004~ZSJHJ
            ZMMT0004~ZSJZT
            ZMMT0004~ERNAM
            ZMMT0004~ERDAT
            ZMMT0004~ZERNAM_D
            ZMMT0004~ZERDAT_D
            ZMMT0004~ZERNAM_Z
            ZMMT0004~ZERDAT_Z
           ZMMT0004~LFGJA
           ZMMT0004~LFBNR
           ZMMT0004~LFPOS
           ZMMT0004~NAMPO
           ZMMT0004~ZFKTS
           ZMMT0004~ZFKRQ
           ZMMT0004~ZSHUI
        FROM ZMMT0004
         INNER JOIN EKPO
         ON ZMMT0004~EBELN = EKPO~EBELN
         AND ZMMT0004~EBELP = EKPO~EBELP
         INTO CORRESPONDING FIELDS OF TABLE GT_ALV
           WHERE ZMMT0004~LIFNR IN   S_LIFNR
           AND   ZMMT0004~BUDAT IN   S_BUDAT
           AND   ZMMT0004~ZDZZT IN   S_ZDZZT
           AND   ZMMT0004~EBELN IN   S_EBELN
           AND   ZMMT0004~WERKS IN   S_WERKS
           AND   ZMMT0004~NAMLA IN   S_NAMLA
           AND   ZMMT0004~CHARG IN   S_CHARG
           AND   ZMMT0004~WAERS IN   S_WAERS
           AND   ZMMT0004~MATNR IN   S_MATNR
           AND   ZMMT0004~ZDZDH IN   S_ZDZDH
           AND   ZMMT0004~ZDZZT NOT IN  ('Y','J')
           AND   ZMMT0004~NAMPO IN S_ERNAM
           AND   EKPO~PSTYP IN   S_PSTYP
           AND EXISTS ( SELECT * FROM EKKO WHERE EBELN = ZMMT0004~EBELN AND BSART IN S_BSART ).
  ELSE.
    SELECT  ZMMT0004~ZDZDH
            ZMMT0004~ZDZXH
            ZMMT0004~ZDZZT
            ZMMT0004~EBELN
            ZMMT0004~EBELP
            ZMMT0004~GJAHR
            ZMMT0004~BELNR
            ZMMT0004~BUZEI
            ZMMT0004~LIFNR
            ZMMT0004~NAME1
            ZMMT0004~WERKS
            ZMMT0004~MATNR
            ZMMT0004~MAKTX
            ZMMT0004~ATWRT
            ZMMT0004~CHARG
            ZMMT0004~BEWTP
            ZMMT0004~BWART
            ZMMT0004~SHKZG
            ZMMT0004~BUDAT
            ZMMT0004~MENGE
            ZMMT0004~MEINS
            ZMMT0004~ZDWHSL
            ZMMT0004~ZSHSL
            ZMMT0004~HSWAE
            ZMMT0004~MWSKZ
            ZMMT0004~DMBTR
            ZMMT0004~ZSL
            ZMMT0004~ZHSDJ
            ZMMT0004~ZSJHJ
            ZMMT0004~ZSJZT
            ZMMT0004~ERNAM
            ZMMT0004~ERDAT
            ZMMT0004~ZERNAM_D
            ZMMT0004~ZERDAT_D
            ZMMT0004~ZERNAM_Z
            ZMMT0004~ZERDAT_Z
           ZMMT0004~LFGJA
           ZMMT0004~LFBNR
           ZMMT0004~LFPOS
           ZMMT0004~NAMPO
           ZMMT0004~ZFKTS
           ZMMT0004~ZFKRQ
           ZMMT0004~ZSHUI
       FROM ZMMT0004
       INNER JOIN EKPO
       ON ZMMT0004~EBELN = EKPO~EBELN
       AND ZMMT0004~EBELP = EKPO~EBELP
       INTO CORRESPONDING FIELDS OF TABLE GT_ALV
         WHERE ZMMT0004~LIFNR IN   S_LIFNR
         AND   ZMMT0004~BUDAT IN   S_BUDAT
         AND   ZMMT0004~ZDZZT IN   S_ZDZZT
         AND   ZMMT0004~EBELN IN   S_EBELN
         AND   ZMMT0004~WERKS IN   S_WERKS
           AND   ZMMT0004~NAMLA IN   S_NAMLA
           AND   ZMMT0004~CHARG IN   S_CHARG
           AND   ZMMT0004~WAERS IN   S_WAERS
         AND   ZMMT0004~MATNR IN   S_MATNR
           AND   ZMMT0004~ZDZDH IN   S_ZDZDH
           AND   ZMMT0004~NAMPO IN S_ERNAM
         AND   EKPO~PSTYP IN   S_PSTYP
         AND EXISTS ( SELECT * FROM EKKO WHERE EBELN = ZMMT0004~EBELN AND BSART IN S_BSART ).
  ENDIF.

  "一个对账单可能存在不同的批次和不同的最后修改人 因此重新进行获取对账单
  IF S_CHARG[] IS NOT INITIAL
  OR S_NAMLA[] IS NOT INITIAL .
    PERFORM FRM_RE_GET_ZMMT0004 TABLES GT_ALV ."
  ENDIF .

  LOOP AT  GT_ALV INTO GS_ALV .

    READ TABLE GT_DOM_APSTT INTO DATA(LS_DOM_APSTT) WITH KEY DOMVALUE_L = GS_ALV-ZDZZT.
    IF  SY-SUBRC = 0.
      GS_ALV-ZDZZTMS  = LS_DOM_APSTT-DDTEXT.
    ENDIF.

    MODIFY GT_ALV FROM GS_ALV.
  ENDLOOP.

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

  CALL SCREEN 0100.

ENDFORM.



*&---------------------------------------------------------------------*
*& Form INIT_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INIT_ALV .
  CLEAR GT_FIELDCAT[] .
  IF  RB_04 = 'X'.
    FILL_FIELD_LIST  'SEL'    '选择'  '' ''  ''  'X'   'X' '' '' .
    FILL_FIELD_LIST  'MESSAGE'  '消息'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZDZDH'  '对账单号'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZDZZT'  '对账状态'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZDZZTMS'  '对账状态描述'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'LIFNR'  '供应商编号'  'LFA1' 'LIFNR'  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'NAME1'  '供应商'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'NAMPO'  '制单人'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZSHSL'  '数量'  '' ''  'X'  ''   '' '' '' .
    FILL_FIELD_LIST  'ZSHUI'  '税额'  '' ''  'X'  ''   '' '' '' .
    FILL_FIELD_LIST  'DMBTR'  '净值'  '' ''  'X'  ''   '' '' '' .
    FILL_FIELD_LIST  'ZSJHJ'  '价税合计'  '' ''  'X'  ''   '' '' '' .
    FILL_FIELD_LIST  'ZBUDAT_Q'  '发票校验过账日期'  'ZMMT0004' 'BUDAT'  ''  ''   'X' '' '' .
    FILL_FIELD_LIST  'BLINE_DATE'  '起算日期'  'ZMMT0004' 'BUDAT'  ''  ''   'X' '' '' .
    FILL_FIELD_LIST  'PMNTTRMS'  '付款方式'  '' ''  ''  ''   'X' 'X' '' .
    FILL_FIELD_LIST  'ZFKTS'  '付款天数'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZFKRQ'  '付款日期'  'ZMMT0004' 'BUDAT'  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZDZWB'  '抬头文本'  '' ''  ''  ''   'X' '' '200' .
    FILL_FIELD_LIST  'ERNAM'  '对账单创建人'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ERDAT'  '对账单创建日期'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZERNAM_D'  '对账确认人'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZERDAT_D'  '对账确认时间'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZERNAM_Z'  '取消对账人'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZERDAT_Z'  '取消对账日期'  '' ''  ''  ''   '' '' '' .
*    FILL_FIELD_LIST  'WAERS'  '币种'  '' ''  ''  ''   '' '' '' .
*    FILL_FIELD_LIST  'CHARG'  '批次'  '' ''  ''  ''   '' '' '' .


  ELSE.

    IF RB_01 = 'X' OR RB_02 = 'X'.
      FILL_FIELD_LIST  'SEL'    '选择'  '' ''  ''  'X'   'X' '' '' .
    ENDIF.
    FILL_FIELD_LIST  'ZDZDH'  '对账单号'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZDZXH'  '对账单行项目'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZDZZT'  '对账状态'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZDZZTMS'  '对账状态描述'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZSJZT'  '数据状态'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'EBELN'  '采购凭证'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'EBELP'  '项目'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'BELNR'  '物料凭证'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'BUZEI'  '物料凭证项目'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'LIFNR'  '供应商编号'  'LFA1' 'LIFNR'  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'NAME1'  '供应商'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'WERKS'  '工厂'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'NAMPO'  '制单人'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'MATNR'  '物料编码'  'MARA' 'MATNR'  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'MAKTX'  '物料描述'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ATWRT'  '长批号'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'CHARG'  'SAP批号'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'WAERS'  '币种'  '' ''  ''  ''   '' '' '' .
*    FILL_FIELD_LIST  'CHARG'  '批次'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'BEWTP'  '采购订单历史记录类别'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'BWART'  '移动类型'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'SHKZG'  '借/贷标识'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'BUDAT'  '过账日期'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'MENGE'  '收货数量'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'MEINS'  '单位'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZDWHSL' '单位换算率'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZSHSL'  '收货数量(采购单位)'  '' ''  'X'  ''   '' '' '' .
    FILL_FIELD_LIST  'HSWAE'  '币别'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'MWSKZ'  '税码'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'DMBTR'  '未含税金额'  '' ''  'X'  ''   '' '' '' .
    FILL_FIELD_LIST  'ZSL'    '税率'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZSHUI'    '税额'  '' ''  'X'  ''   '' '' '' .
    FILL_FIELD_LIST  'ZHSDJ'  '含税单价'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZSJHJ'  '税价合计'  '' ''  'X'  ''   '' '' '' .
    FILL_FIELD_LIST  'LFGJA'  '参考凭证年度'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'LFBNR'  '参考凭证'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'LFPOS'  '参考凭证行项目'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ERNAM'  '对账单创建人'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ERDAT'  '对账单创建日期'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZERNAM_D'  '对账确认人'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZERDAT_D'  '对账确认时间'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZERNAM_Z'  '取消对账人'  '' ''  ''  ''   '' '' '' .
    FILL_FIELD_LIST  'ZERDAT_Z'  '取消对账日期'  '' ''  ''  ''   '' '' '' .
  ENDIF.



*  LOOP AT GT_OUT INTO GS_OUT.
*    GV_TABIX = SY-TABIX.
*
*    CLEAR:GS_OUT-STYLE.
*    LOOP AT GT_FIELDCAT INTO GS_FIELDCAT WHERE  FIELDNAME = 'SEL'.
*      GS_EDIT-FIELDNAME = GS_FIELDCAT-FIELDNAME.
*      GS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*      INSERT GS_EDIT INTO TABLE GS_OUT-STYLE.
*    ENDLOOP.
*    MODIFY GT_OUT FROM GS_OUT INDEX GV_TABIX TRANSPORTING STYLE.
*    CLEAR : GS_OUT.
*  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form FRM_PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GS_LAYOUT
*&---------------------------------------------------------------------*
FORM FRM_PREPARE_LAYOUT  CHANGING GW_LAYOUT TYPE LVC_S_LAYO .
  GW_LAYOUT-ZEBRA         = 'X' .
  GW_LAYOUT-CWIDTH_OPT    = 'X'.
  GW_LAYOUT-KEYHOT        = 'X'.
  GW_LAYOUT-STYLEFNAME    = 'STYLE'.
  GW_LAYOUT-BOX_FNAME     = 'MARK'.
  GW_LAYOUT-SEL_MODE      = 'A'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_MSEG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_MSEG .

  DATA:LT_ALV LIKE TABLE OF GS_ALV.


  APPEND LINES OF GT_OUT TO LT_ALV.

  SORT LT_ALV BY GJAHR
                 BELNR
                 BUZEI.
  DELETE ADJACENT DUPLICATES FROM LT_ALV COMPARING GJAHR
                                                   BELNR
                                                   BUZEI.

  IF  LT_ALV IS NOT INITIAL .
**取出该凭证是否是冲销其他凭证的物料凭证
    SELECT MBLNR
           MJAHR
           ZEILE
           SJAHR
           SMBLN
           SMBLP
           FROM MSEG INTO TABLE GT_MSEG
           FOR ALL ENTRIES IN LT_ALV
           WHERE SJAHR  = LT_ALV-GJAHR
           AND   SMBLN  = LT_ALV-BELNR
           AND   SMBLP  = LT_ALV-BUZEI.
****取出凭证已经被冲销的凭证
    SELECT MBLNR
         MJAHR
         ZEILE
         SJAHR
         SMBLN
         SMBLP
         FROM MSEG INTO TABLE GT_MSEG
         FOR ALL ENTRIES IN LT_ALV
         WHERE SJAHR  = LT_ALV-GJAHR
         AND   SMBLN  = LT_ALV-BELNR
         AND   SMBLP  =  LT_ALV-BUZEI
         AND   SJAHR  NE '0000'
         AND   SMBLN  NE ''
         AND   SMBLP  NE '0000'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DZXX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_DZXX .

  DATA:LT_ALV TYPE TABLE OF TY_TAB.
  APPEND LINES OF GT_OUT TO LT_ALV.

  SORT LT_ALV BY GJAHR BELNR BUZEI.

  DELETE ADJACENT DUPLICATES FROM LT_ALV COMPARING GJAHR BELNR BUZEI.

  IF  LT_ALV IS NOT INITIAL .

    SELECT * FROM ZMMT0004 INTO TABLE GT_ZMMT0004
       FOR ALL ENTRIES IN LT_ALV
       WHERE  GJAHR   = LT_ALV-GJAHR
       AND    BELNR   = LT_ALV-BELNR
       AND    BUZEI   = LT_ALV-BUZEI
       AND    ZDZZT   IN ( 'Y' , 'N' , 'Q' ).
  ENDIF.
  FREE LT_ALV.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_OUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_OUT .


  DATA:BEGIN OF LS_LFA1,
         LIFNR TYPE  LFA1-LIFNR,
         NAME1 TYPE  LFA1-NAME1,
       END OF LS_LFA1,
       LT_LFA1 LIKE TABLE OF LS_LFA1.

  DATA : LV_MENGE TYPE EKBE-MENGE .

  DATA:LT_ALV TYPE TABLE OF TY_TAB.

  APPEND LINES OF GT_OUT TO LT_ALV.

  SORT LT_ALV BY LIFNR.

  DELETE ADJACENT DUPLICATES FROM LT_ALV  COMPARING LIFNR.

  IF  LT_ALV IS NOT INITIAL .
    SELECT LIFNR
           NAME1
           FROM LFA1
           INTO TABLE LT_LFA1
           FOR ALL ENTRIES IN LT_ALV
           WHERE LIFNR = LT_ALV-LIFNR.

    SORT LT_LFA1 BY LIFNR.
  ENDIF.

  FREE LT_ALV.


  SORT GT_ZMMT0004 BY GJAHR BELNR BUZEI.
  SORT GT_MSEG BY SJAHR
                  SMBLN
                  SMBLP.


  GT_MSEG1[] = GT_MSEG[] .
  SORT GT_MSEG1 BY MBLNR
                   MJAHR
                   ZEILE.

  LOOP AT  GT_OUT INTO GS_OUT.

    "检查是否已开票
    CLEAR:GS_ZMMT0004 .
    READ TABLE GT_ZMMT0004 INTO GS_ZMMT0004 WITH KEY GJAHR = GS_OUT-GJAHR BELNR = GS_OUT-BELNR BUZEI = GS_OUT-BUZEI BINARY SEARCH .
    IF  SY-SUBRC = 0.

      CONTINUE.
    ENDIF.

    "检查是否被冲销
    CLEAR:GS_MSEG.
    READ TABLE GT_MSEG INTO GS_MSEG WITH KEY SJAHR  = GS_OUT-GJAHR
                                             SMBLN  = GS_OUT-BELNR
                                             SMBLP  = GS_OUT-BUZEI BINARY SEARCH .
    IF  SY-SUBRC = 0.
      CONTINUE.

    ENDIF.

    "检查是否为冲销凭证
    CLEAR:GS_MSEG.
    READ TABLE GT_MSEG1 INTO GS_MSEG WITH KEY MJAHR  = GS_OUT-GJAHR
                                              MBLNR  = GS_OUT-BELNR
                                              ZEILE  = GS_OUT-BUZEI BINARY SEARCH.
    IF  SY-SUBRC = 0.
      CONTINUE.
    ENDIF.


    "计算开票剩余数量
    CLEAR LV_MENGE .
    LV_MENGE = GS_OUT-MENGE .
    IF GS_OUT-SHKZG =  'H'.
      LV_MENGE = 0 - LV_MENGE .
    ENDIF .

    CLEAR GS_RSEG .
    LOOP AT GT_RSEG INTO GS_RSEG   WHERE LFGJA = GS_OUT-GJAHR
                                     AND LFBNR = GS_OUT-BELNR
                                     AND LFPOS = GS_OUT-BUZEI .
      IF GS_RSEG-SHKZG =  'H'.
        GS_RSEG-MENGE = 0 - GS_RSEG-MENGE .
      ENDIF .
      LV_MENGE = LV_MENGE - GS_RSEG-MENGE .

    ENDLOOP .
    CHECK LV_MENGE <> 0 .

    MOVE-CORRESPONDING GS_OUT TO GS_ALV.

    CLEAR:LS_LFA1.
    READ TABLE LT_LFA1 INTO LS_LFA1 WITH KEY LIFNR = GS_OUT-LIFNR BINARY SEARCH .
    IF  SY-SUBRC = 0.
      GS_ALV-NAME1 = LS_LFA1-NAME1.
    ENDIF.

    CLEAR:GS_MCHA.
    READ TABLE GT_MCHA INTO GS_MCHA WITH KEY MATNR = GS_ALV-MATNR WERKS = GS_ALV-WERKS CHARG = GS_ALV-CHARG BINARY SEARCH.
    IF  SY-SUBRC = 0.
      GS_ALV-CUOBJ_BM = GS_MCHA-CUOBJ_BM.
    ENDIF.


    CLEAR:GS_AUSP.

    READ TABLE GT_AUSP INTO GS_AUSP WITH KEY OBJEK = GS_ALV-CUOBJ_BM BINARY SEARCH.
    IF  SY-SUBRC = 0.
      GS_ALV-ATWRT = GS_AUSP-ATWRT.


    ENDIF.

    CLEAR:GS_A003.
    READ TABLE GT_A003 INTO GS_A003 WITH KEY MWSKZ = GS_ALV-MWSKZ  BINARY SEARCH .
    IF  SY-SUBRC = 0.
      GS_ALV-KBETR = GS_A003-KBETR.
    ENDIF.
    IF GS_ALV-UMREN NE 0 .


      GS_ALV-ZDWHSL = GS_ALV-UMREZ / GS_ALV-UMREN.
    ENDIF.

    LV_MENGE = ABS( LV_MENGE ) .
    GS_ALV-ZSL = GS_ALV-KBETR / 1000.

    GS_ALV-ZHSDJ = ( 1 + GS_ALV-ZSL ) * GS_ALV-DMBTR / GS_ALV-MENGE.

    GS_ALV-ZSJHJ = ( 1 + GS_ALV-ZSL ) * GS_ALV-DMBTR / GS_ALV-MENGE * LV_MENGE .

*    GS_ALV-ZSHUI = GS_ALV-ZSJHJ - GS_ALV-DMBTR.
    GS_ALV-ZSHUI = GS_ALV-ZSL  * GS_ALV-DMBTR / GS_ALV-MENGE * LV_MENGE .
    GS_ALV-MENGE = LV_MENGE .
    GS_ALV-ZSJZT = 'N'.
    IF  GS_ALV-ZDWHSL = 0  .
      GS_ALV-ZSHSL =   GS_ALV-MENGE.
    ELSE .
      GS_ALV-ZSHSL =   GS_ALV-MENGE  / GS_ALV-ZDWHSL.
    ENDIF.

    GS_ALV-ZDZZT = 'N'.
    IF  GS_ALV-SHKZG =  'H'.
      GS_ALV-ZSJHJ  =  0  -  GS_ALV-ZSJHJ .
      GS_ALV-DMBTR  =  0  -  GS_ALV-DMBTR .
      GS_ALV-ZSHUI  =  0  -  GS_ALV-ZSHUI .
      GS_ALV-ZSHSL  =  0  -  GS_ALV-ZSHSL .
      GS_ALV-MENGE  =  0  -  GS_ALV-MENGE .
    ENDIF.

    GS_ALV-ERNAM       =  SY-UNAME.
    GS_ALV-ERDAT       =  SY-DATUM.

    READ TABLE GT_DOM_APSTT INTO DATA(LS_DOM_APSTT) WITH KEY DOMVALUE_L = GS_ALV-ZDZZT.
    IF  SY-SUBRC = 0.
      GS_ALV-ZDZZTMS  = LS_DOM_APSTT-DDTEXT.
    ENDIF.

    APPEND GS_ALV TO GT_ALV.
    CLEAR:GS_ALV.
  ENDLOOP.

  SORT GT_ALV BY LIFNR EBELN EBELP.

  FREE:LT_LFA1 ,GT_AUSP,GT_ZMMT0004,GT_A003,GT_MSEG.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_CABN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_CABN .

  DATA:BEGIN OF LS_OBJEK,
         OBJEK TYPE AUSP-OBJEK,
       END OF LS_OBJEK,
       LT_OBJEK LIKE TABLE OF LS_OBJEK.

  DATA:LV_ATINN TYPE CABN-ATINN.

  DATA:LT_ALV LIKE TABLE OF GS_ALV.

  APPEND LINES OF GT_OUT TO LT_ALV.

  SORT LT_ALV BY MATNR WERKS CHARG.
  DELETE ADJACENT DUPLICATES FROM LT_ALV COMPARING  MATNR WERKS CHARG.
  IF  LT_ALV IS NOT INITIAL .
    SELECT MATNR
           WERKS
           CHARG
           CUOBJ_BM
           FROM MCHA
           INTO TABLE GT_MCHA
           FOR ALL ENTRIES IN LT_ALV
           WHERE MATNR   = LT_ALV-MATNR
           AND   WERKS   = LT_ALV-WERKS
           AND   CHARG   = LT_ALV-CHARG .
  ENDIF.


  LT_OBJEK = CORRESPONDING #( GT_MCHA MAPPING OBJEK = CUOBJ_BM ).

  SORT LT_OBJEK BY OBJEK  .

  DELETE ADJACENT DUPLICATES FROM LT_OBJEK  COMPARING OBJEK .


  SELECT SINGLE ATINN INTO LV_ATINN FROM CABN WHERE ATNAM = 'ZCHARGL'.


  IF  LT_OBJEK IS NOT INITIAL .

    SELECT OBJEK
           ATINN
           ATWRT
           FROM AUSP
           INTO TABLE GT_AUSP
           FOR ALL ENTRIES IN LT_OBJEK
           WHERE OBJEK = LT_OBJEK-OBJEK
           AND   ATINN = LV_ATINN.
  ENDIF.

  "AUSP
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZSL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_ZSL .
**因为税率固定 故 将所有税率取出


  SELECT A003~KSCHL
         A003~ALAND
         A003~MWSKZ
         A003~KNUMH
         KONP~KBETR
         FROM A003
         INNER JOIN KONP
         ON  A003~KNUMH = KONP~KNUMH
         INTO TABLE GT_A003
         WHERE A003~KSCHL = 'MWVS'
         AND   A003~ALAND = 'CN'.

  SORT GT_A003 BY MWSKZ.


ENDFORM.



*&---------------------------------------------------------------------*
*& Form FRM_GET_ZSAVE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_ZSAVE .

  DATA:LV_ZDZDH TYPE  ZMMT0004-ZDZDH,
       LV_ZDZXH TYPE  ZMMT0004-ZDZXH.

  DATA:LS_ZMMT0004 TYPE ZMMT0004,
       LT_ZMMT0004 LIKE TABLE OF LS_ZMMT0004.


  DATA:BEGIN OF LS_LIFNR,
         LIFNR TYPE LFA1-LIFNR,
         MWSKZ TYPE ZMMT0004-MWSKZ,
         HSWAE TYPE ZMMT0004-HSWAE,
       END OF LS_LIFNR,
       LT_LIFNR LIKE TABLE OF LS_LIFNR.


  DATA:LT_ALV LIKE TABLE OF GS_ALV.


  APPEND LINES OF GT_ALV TO LT_ALV.

  DELETE LT_ALV WHERE SEL = ''  OR ZDZDH IS NOT INITIAL .

  LT_LIFNR = CORRESPONDING #( LT_ALV MAPPING LIFNR = LIFNR MWSKZ = MWSKZ  HSWAE = HSWAE ).

  SORT LT_LIFNR BY LIFNR MWSKZ HSWAE.
  DELETE ADJACENT DUPLICATES FROM LT_LIFNR COMPARING LIFNR MWSKZ HSWAE .
****对选择的数据  按照供应商为维度生成对账单号  “新增税率跟货币类型
  LOOP AT  LT_LIFNR INTO LS_LIFNR.
    CLEAR:LV_ZDZXH , LV_ZDZDH.
    PERFORM FRM_GET_SNRO USING LV_ZDZDH.

    LOOP AT  GT_ALV ASSIGNING FIELD-SYMBOL(<FS_ALV>) WHERE  LIFNR = LS_LIFNR-LIFNR
                                                      AND    MWSKZ = LS_LIFNR-MWSKZ
                                                      AND   HSWAE  = LS_LIFNR-HSWAE
                                                      AND    SEL = 'X'.
      LV_ZDZXH = LV_ZDZXH + 1.
      <FS_ALV>-ZDZDH  = LV_ZDZDH.
      <FS_ALV>-ZDZXH  = LV_ZDZXH.
      IF  <FS_ALV>-ZDZZT IS INITIAL .
        <FS_ALV>-ZDZZT = 'N'.
      ENDIF.

      READ TABLE GT_DOM_APSTT INTO DATA(LS_DOM_APSTT) WITH KEY DOMVALUE_L = <FS_ALV>-ZDZZT.
      IF  SY-SUBRC = 0.
        <FS_ALV>-ZDZZTMS  = LS_DOM_APSTT-DDTEXT.
      ENDIF.
      <FS_ALV>-NAMLA = SY-UNAME.
      <FS_ALV>-BUDLA = SY-DATUM.

*      <FS_ALV>-ZDEL = 'X'.
      MOVE-CORRESPONDING <FS_ALV> TO LS_ZMMT0004.
      APPEND LS_ZMMT0004 TO LT_ZMMT0004.
      CLEAR:LS_ZMMT0004.
    ENDLOOP.
  ENDLOOP.
  IF  RB_01 = 'X'.
    DELETE  GT_ALV  WHERE  SEL = ''.
  ENDIF.
  IF  LT_ZMMT0004 IS NOT INITIAL .
    MODIFY ZMMT0004 FROM TABLE LT_ZMMT0004.
    COMMIT WORK AND WAIT .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_SNRO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_ALV_ZDZDH
*&---------------------------------------------------------------------*
FORM FRM_GET_SNRO  USING    O_ZDZDH.

  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
    EXPORTING
      OBJECT           = 'ZMM050'   "Create with SNUM
    EXCEPTIONS
      FOREIGN_LOCK     = 1
      OBJECT_NOT_FOUND = 2
      SYSTEM_FAILURE   = 3
      OTHERS           = 4.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = '01'
      OBJECT                  = 'ZMM050'
    IMPORTING
      NUMBER                  = O_ZDZDH
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERNAL_OVERFLOW       = 6
      OTHERS                  = 7.

  CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
    EXPORTING
      OBJECT = 'ZMM050'.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZMOD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_ZMOD .

  READ TABLE GT_ALV INTO GS_ALV WITH KEY SEL = 'X' ZSJZT = 'Y'.
  IF  SY-SUBRC = 0.
    GV_TABIX1 = SY-TABIX .
    CALL  SCREEN 200 STARTING AT 5 5 ENDING AT  55 23.
  ELSE.
    MESSAGE '您选择的数据不能修改！' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form FRM_GET_ZDEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_ZDEL .

  DATA:LS_ZMMT0004 TYPE ZMMT0004,
       LT_ZMMT0004 LIKE TABLE OF LS_ZMMT0004.

*  LOOP AT  GT_ALV INTO GS_ALV WHERE ( SEL = 'X' AND ZSJZT = 'Y'  AND ZDZZT NE 'J') OR ( SEL = 'X' AND ZSJZT = 'N' AND ZDZZT NE 'J' ).
*  LOOP AT  GT_ALV INTO GS_ALV WHERE  SEL = 'X' AND ZSJZT <> 'Y'  AND ZDZZT NE 'J'.
  LOOP AT  GT_ALV INTO GS_ALV WHERE  SEL = 'X'   AND ZDZZT NE 'J'.
*J  已发票校验
*C  取消对账
*N  暂存
*Y  已对账
*Q  取消发票校验
*Z  作废
    IF GS_ALV-ZDZZT = 'J'
    OR GS_ALV-ZDZZT = 'Y'
    OR GS_ALV-ZDZZT = 'Q'  .

    ELSE .

      GS_ALV-ZDEL = 'X'.
      MODIFY GT_ALV FROM GS_ALV.

      MOVE-CORRESPONDING GS_ALV TO LS_ZMMT0004.
      APPEND LS_ZMMT0004 TO LT_ZMMT0004.
      CLEAR:LS_ZMMT0004.
    ENDIF .
  ENDLOOP.


  DELETE GT_ALV WHERE ZDEL = 'X'.

**如果是修改 需要对采购对账表对应的数据进行删除
  IF  LT_ZMMT0004 IS INITIAL.
    MESSAGE '请选择正确需要删除的数据！'  TYPE  'S' DISPLAY LIKE 'E'.
  ENDIF.


  IF  RB_02 = 'X' AND LT_ZMMT0004 IS NOT INITIAL .
    DELETE ZMMT0004 FROM TABLE LT_ZMMT0004.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZQRDZ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_ZQRDZ .

  DATA:LS_ZMMT0004 TYPE ZMMT0004,
       LT_ZMMT0004 LIKE TABLE OF LS_ZMMT0004.

  LOOP AT  GT_ALV ASSIGNING FIELD-SYMBOL(<FS_ALV1>) WHERE  SEL = 'X' AND ZDZZT = 'N'.
    <FS_ALV1>-ZDZZT = 'Y'.
    READ TABLE GT_DOM_APSTT INTO DATA(LS_DOM_APSTT) WITH KEY DOMVALUE_L = <FS_ALV1>-ZDZZT.
    IF  SY-SUBRC = 0.
      <FS_ALV1>-ZDZZTMS  = LS_DOM_APSTT-DDTEXT.
    ENDIF.

    <FS_ALV1>-NAMLA = SY-UNAME.
    <FS_ALV1>-BUDLA = SY-DATUM.

    MOVE-CORRESPONDING <FS_ALV1> TO LS_ZMMT0004.
    APPEND LS_ZMMT0004 TO LT_ZMMT0004..
    CLEAR:LS_ZMMT0004.
  ENDLOOP.

  IF  LT_ZMMT0004 IS NOT INITIAL .
    MODIFY ZMMT0004 FROM TABLE LT_ZMMT0004.
    IF  SY-SUBRC = 0.
      MESSAGE '确认成功！'  TYPE 'S' .
    ENDIF.
  ELSE.
    MESSAGE '只有暂存的数据才能对账！'  TYPE 'S' .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZCXDZ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_ZCXDZ .
  DATA:LS_ZMMT0004 TYPE ZMMT0004,
       LT_ZMMT0004 LIKE TABLE OF LS_ZMMT0004.

  DATA : LV_MARKS(1) TYPE C .
  DATA : LV_MESS(220) TYPE C .
  DATA : LS_ALV TYPE TY_TAB .

  CLEAR LS_ALV .
  CLEAR LV_MARKS .
*  LOOP AT GT_ALV INTO LS_ALV WHERE  SEL = 'X' AND ZDZZT = 'Y'
*                                 AND ( BLINE_DATE IS INITIAL  OR ZDZWB IS INITIAL ) .
*
*    CONCATENATE '对账单：'  LS_ALV-ZDZDH '起算日期和抬头文本必填' INTO LV_MESS .
*    MESSAGE LV_MESS TYPE 'S' DISPLAY LIKE 'E' .
*    EXIT .
*  ENDLOOP .

  CHECK LV_MARKS IS INITIAL .

  LOOP AT  GT_ALV ASSIGNING FIELD-SYMBOL(<FS_ALV1>) WHERE  SEL = 'X' AND ZDZZT <> 'Y' AND ZDZDH <> ''.
    <FS_ALV1>-ZDZZT = 'Z'.
    <FS_ALV1>-ZDZZTMS = '作废'.
    <FS_ALV1>-ZERNAM_D  = SY-UNAME.
    <FS_ALV1>-ZERDAT_D  = SY-DATUM.

    <FS_ALV1>-NAMLA = SY-UNAME.
    <FS_ALV1>-BUDLA = SY-DATUM.

    MOVE-CORRESPONDING <FS_ALV1> TO LS_ZMMT0004.
    APPEND LS_ZMMT0004 TO LT_ZMMT0004.
    CLEAR:LS_ZMMT0004.
  ENDLOOP.

  IF  LT_ZMMT0004 IS NOT INITIAL .
    MODIFY ZMMT0004 FROM TABLE LT_ZMMT0004.
    IF  SY-SUBRC = 0.
      MESSAGE '变更成功！'  TYPE 'S' .
    ENDIF.
  ELSE.
    MESSAGE '只有已保存的数据才能对账！'  TYPE 'S' .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZQXDZ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_ZQXDZ .
  DATA:LS_ZMMT0004 TYPE ZMMT0004,
       LT_ZMMT0004 LIKE TABLE OF LS_ZMMT0004.

  LOOP AT  GT_ALV ASSIGNING FIELD-SYMBOL(<FS_ALV1>) WHERE ( SEL = 'X' AND  ZDZZT = 'Y' ) OR ( SEL = 'X' AND  ZDZZT = 'N' ) .
    <FS_ALV1>-ZDZZT = 'Z'.
    <FS_ALV1>-ZERNAM_Z  = SY-UNAME.
    <FS_ALV1>-ZERDAT_Z  = SY-DATUM.
    MOVE-CORRESPONDING <FS_ALV1> TO LS_ZMMT0004.
    APPEND LS_ZMMT0004 TO LT_ZMMT0004..
    CLEAR:LS_ZMMT0004.
  ENDLOOP.

  IF  LT_ZMMT0004 IS NOT INITIAL .
    MODIFY ZMMT0004 FROM TABLE LT_ZMMT0004.
    IF  SY-SUBRC = 0.
      MESSAGE '对账作废成功！'  TYPE 'S' .
    ENDIF.
  ELSE.
    MESSAGE '请选择需要对账作废的数据！'  TYPE 'S' .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA3
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_DATA3 .
  DATA:LT_ALV TYPE TABLE OF TY_TAB.
  DATA : LS_T052 TYPE T052 .

  SELECT   ZMMT0004~ZDZDH
           ZMMT0004~ZDZXH
           ZMMT0004~ZDZZT
           ZMMT0004~EBELN
           ZMMT0004~EBELP
           ZMMT0004~GJAHR
           ZMMT0004~BELNR
           ZMMT0004~BUZEI
           ZMMT0004~LIFNR
           ZMMT0004~NAME1
           ZMMT0004~WERKS
           ZMMT0004~MATNR
           ZMMT0004~MAKTX
           ZMMT0004~ATWRT
           ZMMT0004~CHARG
           ZMMT0004~BEWTP
           ZMMT0004~BWART
           ZMMT0004~SHKZG
           ZMMT0004~BUDAT
           ZMMT0004~MENGE
           ZMMT0004~MEINS
           ZMMT0004~ZDWHSL
           ZMMT0004~ZSHSL
           ZMMT0004~HSWAE
           ZMMT0004~MWSKZ
           ZMMT0004~DMBTR
           ZMMT0004~ZSL
           ZMMT0004~ZHSDJ
           ZMMT0004~ZSJHJ
           ZMMT0004~ZSJZT
           ZMMT0004~ERNAM
           ZMMT0004~ERDAT
           ZMMT0004~ZERNAM_D
           ZMMT0004~ZERDAT_D
           ZMMT0004~ZERNAM_Z
           ZMMT0004~ZERDAT_Z
           ZMMT0004~LFGJA
           ZMMT0004~LFBNR
           ZMMT0004~LFPOS
           ZMMT0004~PMNTTRMS
           ZMMT0004~BLINE_DATE
           ZMMT0004~ZBUDAT_Q
           ZMMT0004~ZDZWB
           ZMMT0004~NAMPO
              ZMMT0004~ZFKTS
           ZMMT0004~ZFKRQ
           ZMMT0004~ZSHUI
           EKKO~EKORG
     FROM ZMMT0004
     INNER JOIN EKPO
     ON ZMMT0004~EBELN = EKPO~EBELN
     AND ZMMT0004~EBELP = EKPO~EBELP
     INNER JOIN EKKO
     ON ZMMT0004~EBELN = EKKO~EBELN
     INTO CORRESPONDING FIELDS OF TABLE GT_ALV
     WHERE ZMMT0004~LIFNR IN   S_LIFNR
     AND   ZMMT0004~BUDAT IN   S_BUDAT
     AND   ZMMT0004~ZDZZT IN   S_ZDZZT
     AND   ZMMT0004~EBELN IN   S_EBELN
     AND   ZMMT0004~WERKS IN   S_WERKS
     AND   ZMMT0004~NAMLA IN   S_NAMLA
     AND   ZMMT0004~CHARG IN   S_CHARG
     AND   ZMMT0004~WAERS IN   S_WAERS
     AND   ZMMT0004~MATNR IN   S_MATNR
     AND   ZMMT0004~ZDZDH IN   S_ZDZDH
     AND   ZMMT0004~ZDZZT NOT IN (  'J', 'Z' )
     AND   ZMMT0004~NAMPO IN S_ERNAM
     AND   EKPO~PSTYP IN   S_PSTYP
     AND EXISTS ( SELECT * FROM EKKO WHERE EBELN = ZMMT0004~EBELN AND BSART IN S_BSART ).
  IF  GT_ALV IS INITIAL .
    MESSAGE '您的查询数据无有效数据！'  TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE  LIST-PROCESSING AND RETURN TO SCREEN 0.
  ENDIF.

  "一个对账单可能存在不同的批次和不同的最后修改人 因此重新进行获取对账单
  IF S_CHARG[] IS NOT INITIAL
  OR S_NAMLA[] IS NOT INITIAL .
    PERFORM FRM_RE_GET_ZMMT0004 TABLES GT_ALV ."
  ENDIF .

  APPEND LINES OF GT_ALV TO GT_ALV1.


  SORT GT_ALV BY ZDZDH LIFNR.

  DELETE ADJACENT DUPLICATES FROM GT_ALV COMPARING ZDZDH LIFNR.


  APPEND LINES OF GT_ALV TO LT_ALV.
  SORT LT_ALV BY LIFNR EKORG.
  DELETE   ADJACENT DUPLICATES FROM LT_ALV COMPARING LIFNR EKORG.

  IF  LT_ALV IS NOT INITIAL .
    SELECT LIFNR,
           EKORG,
           ZTERM
           FROM LFM1
           INTO TABLE @DATA(LT_LFM1)
           FOR ALL ENTRIES IN @LT_ALV
           WHERE LIFNR = @LT_ALV-LIFNR
           AND   EKORG = @LT_ALV-EKORG.

  ENDIF.

  LOOP AT  GT_ALV ASSIGNING FIELD-SYMBOL(<FS_ALV_4>).
    IF  <FS_ALV_4>-PMNTTRMS IS INITIAL .
      READ TABLE LT_LFM1 INTO DATA(LS_LFM1) WITH KEY  LIFNR = <FS_ALV_4>-LIFNR EKORG = <FS_ALV_4>-EKORG.
      IF  SY-SUBRC = 0.
        <FS_ALV_4>-PMNTTRMS = LS_LFM1-ZTERM.
      ELSE.
        <FS_ALV_4>-MESSAGE = '供应商组数据没有维护付款方式'.
      ENDIF.
    ENDIF.

    IF  <FS_ALV_4>-ZBUDAT_Q IS INITIAL .
      <FS_ALV_4>-ZBUDAT_Q    = SY-DATUM.
    ENDIF.

    CLEAR <FS_ALV_4>-ZSHUI .
    CLEAR <FS_ALV_4>-DMBTR .
    CLEAR <FS_ALV_4>-ZSJHJ .
    CLEAR <FS_ALV_4>-ZSHSL .
    CLEAR GS_ALV1 .
    LOOP AT GT_ALV1 INTO GS_ALV1 WHERE ZDZDH = <FS_ALV_4>-ZDZDH .
      <FS_ALV_4>-ZSHUI = <FS_ALV_4>-ZSHUI  +   GS_ALV1-ZSHUI  .
      <FS_ALV_4>-DMBTR = <FS_ALV_4>-DMBTR  +   GS_ALV1-DMBTR  .
      <FS_ALV_4>-ZSJHJ = <FS_ALV_4>-ZSJHJ  +   GS_ALV1-ZSJHJ  .
      <FS_ALV_4>-ZSHSL = <FS_ALV_4>-ZSHSL  +   GS_ALV1-ZSHSL  .
    ENDLOOP .

*    IF <FS_ALV_4>-BLINE_DATE IS INITIAL  .
*      <FS_ALV_4>-BLINE_DATE  = SY-DATUM.
*    ENDIF.

    IF <FS_ALV_4>-PMNTTRMS <> '' .

      CLEAR LS_T052 .
      CALL FUNCTION 'FI_FIND_PAYMENT_CONDITIONS'
        EXPORTING
          I_ZTERM            = <FS_ALV_4>-PMNTTRMS
          I_BLDAT            = <FS_ALV_4>-ZBUDAT_Q
          I_BUDAT            = <FS_ALV_4>-ZBUDAT_Q
        IMPORTING
          E_T052             = LS_T052
        EXCEPTIONS
          TERMS_INCORRECT    = 1
          TERMS_NOT_FOUND    = 2
          NO_DATE_ENTERED    = 3
          NO_DAY_LIMIT_FOUND = 4
          OTHERS             = 5.

      <FS_ALV_4>-ZFKTS  = LS_T052-ZTAG1 .

    ENDIF .

    IF <FS_ALV_4>-BLINE_DATE IS NOT INITIAL  .
      <FS_ALV_4>-ZFKRQ = <FS_ALV_4>-BLINE_DATE + <FS_ALV_4>-ZFKTS .
    ENDIF .

    READ TABLE GT_DOM_APSTT INTO DATA(LS_DOM_APSTT) WITH KEY DOMVALUE_L = <FS_ALV_4>-ZDZZT.
    IF  SY-SUBRC = 0.
      <FS_ALV_4>-ZDZZTMS  = LS_DOM_APSTT-DDTEXT.
    ENDIF.

  ENDLOOP.

  FREE:LT_ALV.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZQRDZ1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_ZQRDZ1 .

  DATA:LS_ZMMT0004 TYPE ZMMT0004,
       LT_ZMMT0004 LIKE TABLE OF LS_ZMMT0004.

  DATA:BEGIN OF LS_ZDZDH,
         ZDZDH      TYPE ZMMT0004-ZDZDH,
         PMNTTRMS   TYPE ZMMT0004-PMNTTRMS,
         BLINE_DATE TYPE ZMMT0004-BLINE_DATE,
         ZDZWB      TYPE ZMMT0004-ZDZWB,
         ZBUDAT_Q   TYPE ZMMT0004-ZBUDAT_Q  , "过账期间
       END OF LS_ZDZDH,
       LT_ZDZDH LIKE TABLE OF LS_ZDZDH.

  DATA : LV_MARKS(1) TYPE C .
  DATA : LV_MESS(220) TYPE C .
  DATA : LS_ALV TYPE TY_TAB .

  CLEAR LS_ALV .
  CLEAR LV_MARKS .
  LOOP AT GT_ALV INTO LS_ALV WHERE  SEL = 'X' AND ( ZDZZT = 'N' OR  ZDZZT = 'Q' OR  ZDZZT = 'C' )
                                 AND ( BLINE_DATE IS INITIAL  OR ZDZWB IS INITIAL ) .

    LV_MARKS  = 'X' .
    CONCATENATE '对账单：'  LS_ALV-ZDZDH '起算日期和抬头文本必填' INTO LV_MESS .
    MESSAGE LV_MESS TYPE 'S' DISPLAY LIKE 'E' .
    EXIT .
  ENDLOOP .

  CHECK LV_MARKS IS INITIAL .

  LOOP AT  GT_ALV ASSIGNING FIELD-SYMBOL(<FS_ALV1>) WHERE  SEL = 'X' AND ( ZDZZT = 'N' OR  ZDZZT = 'Q' OR  ZDZZT = 'C' ).
    <FS_ALV1>-ZDZZT = 'Y'.
    "   MOVE-CORRESPONDING <FS_ALV1> TO LS_ZMMT0004.
    READ TABLE GT_DOM_APSTT INTO DATA(LS_DOM_APSTT) WITH KEY DOMVALUE_L = <FS_ALV1>-ZDZZT.
    IF  SY-SUBRC = 0.
      <FS_ALV1>-ZDZZTMS  = LS_DOM_APSTT-DDTEXT.
    ENDIF.


    <FS_ALV1>-ZERNAM_D  = SY-UNAME.
    <FS_ALV1>-ZERDAT_D  = SY-DATUM.
    LS_ZDZDH-ZDZDH      = <FS_ALV1>-ZDZDH.
    LS_ZDZDH-PMNTTRMS   = <FS_ALV1>-PMNTTRMS  .
    LS_ZDZDH-BLINE_DATE = <FS_ALV1>-BLINE_DATE.
    LS_ZDZDH-ZDZWB      = <FS_ALV1>-ZDZWB     .
    LS_ZDZDH-ZBUDAT_Q   = <FS_ALV1>-ZBUDAT_Q  .
    APPEND LS_ZDZDH TO LT_ZDZDH.
  ENDLOOP.

  IF  LT_ZDZDH IS NOT INITIAL .
    SELECT * FROM ZMMT0004 INTO TABLE LT_ZMMT0004
             FOR ALL ENTRIES IN LT_ZDZDH
             WHERE ZDZDH = LT_ZDZDH-ZDZDH.
  ENDIF.

  IF  LT_ZMMT0004 IS NOT INITIAL .
    LOOP AT  LT_ZMMT0004  INTO LS_ZMMT0004 .
      LS_ZMMT0004-ZDZZT = 'Y'.
      LS_ZMMT0004-ZERNAM_D = SY-UNAME.
      LS_ZMMT0004-ZERDAT_D = SY-DATUM.
      LS_ZMMT0004-NAMLA = SY-UNAME.
      LS_ZMMT0004-BUDLA = SY-DATUM.

      CLEAR:LS_ZDZDH.
      READ TABLE LT_ZDZDH INTO LS_ZDZDH WITH KEY ZDZDH = LS_ZMMT0004-ZDZDH.
      IF  SY-SUBRC = 0.
        LS_ZMMT0004-PMNTTRMS   = LS_ZDZDH-PMNTTRMS.
        LS_ZMMT0004-BLINE_DATE = LS_ZDZDH-BLINE_DATE.
        LS_ZMMT0004-ZDZWB      = LS_ZDZDH-ZDZWB.
        LS_ZMMT0004-ZBUDAT_Q   = LS_ZDZDH-ZBUDAT_Q.
      ENDIF.
      MODIFY LT_ZMMT0004 FROM LS_ZMMT0004.
    ENDLOOP.

    MODIFY ZMMT0004 FROM TABLE LT_ZMMT0004.
    IF  SY-SUBRC = 0.
      MESSAGE '确认成功！'  TYPE 'S' .
    ENDIF.
  ELSE.
    MESSAGE '只有暂存的数据才能对账！'  TYPE 'S' .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZQXDZ1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_ZQXDZ1 .
  DATA:LS_ZMMT0004 TYPE ZMMT0004,
       LT_ZMMT0004 LIKE TABLE OF LS_ZMMT0004.

  DATA:BEGIN OF LS_ZDZDH,
         ZDZDH TYPE ZMMT0004-ZDZDH,
       END OF LS_ZDZDH,
       LT_ZDZDH LIKE TABLE OF LS_ZDZDH.

  LOOP AT  GT_ALV ASSIGNING FIELD-SYMBOL(<FS_ALV1>) WHERE  SEL = 'X' AND ( ZDZZT = 'Y' OR  ZDZZT = 'Q'  ).
    <FS_ALV1>-ZDZZT = 'C'.
    <FS_ALV1>-MESSAGE = '取消对账成功'.
    READ TABLE GT_DOM_APSTT INTO DATA(LS_DOM_APSTT) WITH KEY DOMVALUE_L = <FS_ALV1>-ZDZZT.
    IF  SY-SUBRC = 0.
      <FS_ALV1>-ZDZZTMS  = LS_DOM_APSTT-DDTEXT.
    ENDIF.
    <FS_ALV1>-ZERNAM_Z = SY-UNAME.
    <FS_ALV1>-ZERDAT_Z = SY-DATUM.
    LS_ZDZDH-ZDZDH = <FS_ALV1>-ZDZDH.
    APPEND LS_ZDZDH TO LT_ZDZDH.
  ENDLOOP.

  IF  LT_ZDZDH IS NOT INITIAL .
    SELECT * FROM ZMMT0004 INTO TABLE LT_ZMMT0004
             FOR ALL ENTRIES IN LT_ZDZDH
             WHERE ZDZDH = LT_ZDZDH-ZDZDH.
  ENDIF.

  IF  LT_ZMMT0004 IS NOT INITIAL .
    LOOP AT  LT_ZMMT0004  INTO LS_ZMMT0004 .
      LS_ZMMT0004-ZDZZT = 'C'.
      LS_ZMMT0004-ZERNAM_D = SY-UNAME.
      LS_ZMMT0004-ZERDAT_D = SY-DATUM.
      LS_ZMMT0004-NAMLA = SY-UNAME.
      LS_ZMMT0004-BUDLA = SY-DATUM.
      MODIFY LT_ZMMT0004 FROM LS_ZMMT0004.
    ENDLOOP.

    MODIFY ZMMT0004 FROM TABLE LT_ZMMT0004.
    IF  SY-SUBRC = 0.
      MESSAGE '取消对账成功！'  TYPE 'S' .
    ENDIF.
  ELSE.
    MESSAGE '只有已对账的数据才能作废！'  TYPE 'S' .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZSAVE4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_ZSAVE4 .

  DATA:LS_ZMMT0004 TYPE ZMMT0004,
       LT_ZMMT0004 LIKE TABLE OF LS_ZMMT0004.

  DATA:BEGIN OF LS_ZDZDH,
         ZDZDH      TYPE ZMMT0004-ZDZDH,
         PMNTTRMS   TYPE ZMMT0004-PMNTTRMS,
         BLINE_DATE TYPE ZMMT0004-BLINE_DATE,
         ZDZWB      TYPE ZMMT0004-ZDZWB,
         ZBUDAT_Q   TYPE ZMMT0004-ZBUDAT_Q  , "过账期间
       END OF LS_ZDZDH,
       LT_ZDZDH LIKE TABLE OF LS_ZDZDH.

  LOOP AT  GT_ALV ASSIGNING FIELD-SYMBOL(<FS_ALV1>) WHERE  SEL = 'X' .
    "   <FS_ALV1>-ZDZZT = 'Y'.
*    <FS_ALV1>-ZERNAM_D = SY-UNAME.
*    <FS_ALV1>-ZERDAT_D = SY-DATUM.
    LS_ZDZDH-ZDZDH       = <FS_ALV1>-ZDZDH.
    LS_ZDZDH-PMNTTRMS    = <FS_ALV1>-PMNTTRMS  .
    LS_ZDZDH-BLINE_DATE  = <FS_ALV1>-BLINE_DATE .
    LS_ZDZDH-ZDZWB       = <FS_ALV1>-ZDZWB      .
    LS_ZDZDH-ZBUDAT_Q    = <FS_ALV1>-ZBUDAT_Q   .
*          GS_ALV-NAMLA = SY-UNAME.
*          GS_ALV-BUDLA = SY-DATUM.
    APPEND LS_ZDZDH TO LT_ZDZDH.
    CLEAR:LS_ZDZDH.
  ENDLOOP.

  IF  LT_ZDZDH IS NOT INITIAL .
    SELECT * FROM ZMMT0004 INTO TABLE LT_ZMMT0004
             FOR ALL ENTRIES IN LT_ZDZDH
             WHERE ZDZDH = LT_ZDZDH-ZDZDH.
  ENDIF.

  IF  LT_ZMMT0004 IS NOT INITIAL .
    LOOP AT  LT_ZMMT0004  INTO LS_ZMMT0004 .
      CLEAR:LS_ZDZDH.
      READ TABLE LT_ZDZDH INTO LS_ZDZDH WITH KEY ZDZDH = LS_ZMMT0004-ZDZDH .
      IF SY-SUBRC = 0 .
        LS_ZMMT0004-PMNTTRMS   = LS_ZDZDH-PMNTTRMS.
        LS_ZMMT0004-BLINE_DATE = LS_ZDZDH-BLINE_DATE.
        LS_ZMMT0004-ZDZWB      = LS_ZDZDH-ZDZWB.
        LS_ZMMT0004-ZBUDAT_Q   = LS_ZDZDH-ZBUDAT_Q.
        LS_ZMMT0004-NAMLA = SY-UNAME.
        LS_ZMMT0004-BUDLA = SY-DATUM.
      ENDIF.
      MODIFY LT_ZMMT0004 FROM LS_ZMMT0004.
    ENDLOOP.

    MODIFY ZMMT0004 FROM TABLE LT_ZMMT0004.
    IF  SY-SUBRC = 0.
      MESSAGE '保存成功！'  TYPE 'S' .
    ENDIF.
  ELSE.
    MESSAGE '请选择需要保存的数据！'  TYPE 'S' .
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_SCREEN .

  LOOP AT SCREEN.
    CASE  SCREEN-GROUP1 .
      WHEN 'Z01'.
        CASE 'X'.
          WHEN RB_01.
            SCREEN-ACTIVE = '0'.
          WHEN OTHERS.
            SCREEN-ACTIVE = '1'.
        ENDCASE.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZDZZTMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_ZDZZTMS .

**获取允差描述
  DATA: LV_RETURN    TYPE SY-SUBRC,
        LV_WA_DOMTAB TYPE DD07V.

  IF GT_DOM_APSTT IS INITIAL.
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        DOMNAME        = 'ZMMO_DZZT'   "域名
        TEXT           = 'X'
        LANGU          = SY-LANGU
      IMPORTING
        RC             = LV_RETURN
      TABLES
        DD07V_TAB      = GT_DOM_APSTT
      EXCEPTIONS
        WRONG_TEXTFLAG = 1
        OTHERS         = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_ITEM .
  CLEAR:GT_ALV_ITEM.

  APPEND LINES OF GT_ALV1 TO GT_ALV_ITEM.

  DELETE GT_ALV_ITEM WHERE ZDZDH NE GS_ALV-ZDZDH.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SHOW_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_SHOW_ITEM .
  CLEAR:GT_FIELDCAT1.
  FILL_FIELD_LIST1  'ZDZDH'  '对账单号'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'ZDZXH'  '对账单行项目'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'ZDZZT'  '对账状态'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'ZSJZT'  '数据状态'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'EBELN'  '采购凭证'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'EBELP'  '项目'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'BELNR'  '物料凭证'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'BUZEI'  '物料凭证项目'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'LIFNR'  '供应商编号'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'NAME1'  '供应商'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'WERKS'  '工厂'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'MATNR'  '物料编码'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'MAKTX'  '物料描述'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'ATWRT'  '长批号'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'CHARG'  'SAP批号'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'BEWTP'  '采购订单历史记录类别'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'BWART'  '移动类型'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'SHKZG'  '借/贷标识'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'BUDAT'  '过账日期'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'MENGE'  '收货数量'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'MEINS'  '单位'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'ZDWHSL' '单位换算率'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'ZSHSL'  '收货数量(采购单位)'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'HSWAE'  '币别'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'MWSKZ'  '税码'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'DMBTR'  '未含税金额'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'ZSL'    '税率'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'ZHSDJ'  '含税单价'  '' ''  ''  ''   '' '' '' .
  FILL_FIELD_LIST1  'ZSJHJ'  '税价合计'  '' ''  ''  ''   '' '' '' .


  DATA LV_LAYOUT_LVC TYPE  LVC_S_LAYO.
  LV_LAYOUT_LVC-CWIDTH_OPT   = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
      IS_LAYOUT_LVC      = LV_LAYOUT_LVC
      IT_FIELDCAT_LVC    = GT_FIELDCAT1
      I_DEFAULT          = 'X'
      I_SAVE             = 'U'
    TABLES
      T_OUTTAB           = GT_ALV_ITEM.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CALL_LONG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_CALL_LONG .
  CALL   SCREEN 2000 STARTING AT 20 10
                       ENDING AT 60 20 .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_RSEG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_RSEG .


  SELECT
  RSEG~BELNR
  RSEG~GJAHR
  RSEG~BUZEI
  RSEG~SHKZG
  RSEG~MENGE
  RSEG~BSTME
  RSEG~BPMNG
  RSEG~BPRME
  RSEG~LFGJA
  RSEG~LFBNR
  RSEG~LFPOS
  INTO TABLE GT_RSEG
  FROM RSEG
  INNER JOIN RBKP
  ON RBKP~BELNR =  RSEG~BELNR
  AND RBKP~GJAHR =  RSEG~GJAHR
  FOR ALL ENTRIES IN GT_OUT
  WHERE RSEG~LFGJA = GT_OUT-GJAHR
    AND RSEG~LFBNR = GT_OUT-BELNR
    AND RSEG~LFPOS = GT_OUT-BUZEI
    AND RBKP~STBLG = '' .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_RE_GET_ZMMT004
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_RE_GET_ZMMT0004  TABLES T_OUT STRUCTURE GS_ALV .

  DATA : LT_OUT TYPE TABLE OF TY_TAB .

  SORT T_OUT BY ZDZDH .
  DELETE ADJACENT DUPLICATES FROM T_OUT
                COMPARING ZDZDH .

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_OUT
    FROM ZMMT0004
    FOR ALL ENTRIES IN T_OUT
    WHERE ZDZDH = T_OUT-ZDZDH .

  T_OUT[] = LT_OUT[] .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_SET_ALV .

  IF G_CONTAINER IS INITIAL.
    PERFORM INIT_ALV.
    CREATE OBJECT G_CONTAINER
      EXPORTING
        CONTAINER_NAME = 'ALV1'
        REPID          = SY-REPID
        DYNNR          = '0100'
*       SIDE           = G_CONTAINER->DOCK_AT_LEFT
**       side      = cl_gui_docking_container=>dock_at_right
*       EXTENSION      = 1500.
      .

    CREATE OBJECT G_GUI_ALV " 功能A: 基本显示
      EXPORTING
        I_PARENT          = G_CONTAINER
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    DATA: LT_F4 TYPE LVC_T_F4 WITH HEADER LINE.
    DATA: LT_F4_DATA TYPE LVC_S_F4.
    CLEAR: LT_F4_DATA.
    LT_F4_DATA-FIELDNAME = 'PMNTTRMS'.
    LT_F4_DATA-REGISTER = 'X' .
    LT_F4_DATA-GETBEFORE = 'X' .
    LT_F4_DATA-CHNGEAFTER  ='X'.
    INSERT LT_F4_DATA INTO TABLE LT_F4.

    CALL METHOD G_GUI_ALV->REGISTER_F4_FOR_FIELDS
      EXPORTING
        IT_F4 = LT_F4[].

    CREATE OBJECT LCL_EVENT.
    SET HANDLER LCL_EVENT->HANDLE_TOOLBAR FOR G_GUI_ALV.
    SET HANDLER LCL_EVENT->HANDLE_COMMAND FOR G_GUI_ALV.
    SET HANDLER LCL_EVENT->HANDLE_ONF4    FOR G_GUI_ALV.
    SET HANDLER LCL_EVENT->HANDLE_DOUBLE_CLICK FOR G_GUI_ALV.
    SET HANDLER LCL_EVENT->HANDLE_DATA_CHANGED        FOR G_GUI_ALV.
    CALL METHOD G_GUI_ALV->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER. "回车事件

    CALL METHOD G_GUI_ALV->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    PERFORM FRM_PREPARE_LAYOUT CHANGING GS_LAYOUT.
    G_VARIANT1 = SY-REPID.

    CALL METHOD G_GUI_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        IS_VARIANT           = G_VARIANT1
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = GT_EXCLUDE " 功能G: 设定屏蔽标准ALV 工具栏的按钮
      CHANGING
        IT_SORT              = I_SORT[]
        IT_FIELDCATALOG      = GT_FIELDCAT[]
        IT_OUTTAB            = GT_ALV.
*    CALL METHOD g_gui_alv->set_ready_for_input
*      EXPORTING
*        i_ready_for_input = 1.
*    CALL METHOD g_gui_alv->set_toolbar_interactive.

*    PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
  ELSE.
    PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_FREE_CREAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_FREE_CREAT .


  CALL METHOD G_CONTAINER->FREE .
**   FREE  G_CONTAINER .
*    FREE  G_GUI_ALV .
  FREE  G_CONTAINER .
  PERFORM FRM_SET_ALV .


ENDFORM.