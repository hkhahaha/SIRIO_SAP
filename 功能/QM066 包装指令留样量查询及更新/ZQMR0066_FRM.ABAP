*&---------------------------------------------------------------------*
*& 包含               ZQMR0066_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_INIT .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PBO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_PBO .

  IF R1 = 'X'.
    LOOP AT SCREEN .

      IF SCREEN-GROUP1 = 'X2'.
        SCREEN-ACTIVE = '0'.
      ENDIF.

      IF SCREEN-GROUP1 = 'X1'.
        SCREEN-REQUIRED = '2'.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ELSEIF R2 = 'X'.
    LOOP AT SCREEN .

      IF SCREEN-GROUP1 = 'X1'.
        SCREEN-ACTIVE = '0'.
      ENDIF.

      IF SCREEN-GROUP1 = 'X1'.
        SCREEN-REQUIRED = '2'.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_INPUT_R1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_CHECK_INPUT_R1 .

  CLEAR: LV_MARK,LS_ITAB,LT_ITAB.

  IF S_AUFNR[] IS INITIAL.
    MESSAGE S001(00) WITH '工单号为必填项.' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING.
  ENDIF.

  SELECT WERKS
  INTO CORRESPONDING FIELDS OF TABLE LT_ITAB
  FROM AUFK
  WHERE AUFNR IN S_AUFNR[].

  IF LT_ITAB[] IS INITIAL.
    MESSAGE S001(00) WITH '工单号不存在.' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT LT_ITAB BY WERKS.

  DELETE ADJACENT DUPLICATES FROM LT_ITAB COMPARING WERKS.

  LOOP AT LT_ITAB INTO LS_ITAB.   " 查看权限检查

    AUTHORITY-CHECK OBJECT 'ZQM066'
         ID 'WERKS' FIELD LS_ITAB-WERKS.

    IF SY-SUBRC NE 0 .
      MESSAGE S001(00) WITH '没有权限查看工厂：' LS_ITAB-WERKS DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING .
    ENDIF.

    CLEAR: LS_ITAB.
  ENDLOOP.

  LOOP AT LT_ITAB INTO LS_ITAB.  " 修改权限检查

    AUTHORITY-CHECK OBJECT 'ZQM066A'
         ID 'WERKS' FIELD LS_ITAB-WERKS.

    IF SY-SUBRC NE 0 .
      LV_MARK = 'X'.
    ENDIF.

    CLEAR: LS_ITAB.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA_R1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_DATA_R1 .

  " 读取 包装指令留样自建表 数据

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE LT_LIST
  FROM ZQMT0066A
  WHERE AUFNR IN S_AUFNR.

  IF LT_LIST IS INITIAL.
    MESSAGE S001(00) WITH '没有满足条件的数据.' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT LT_LIST BY AUFNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_INPUT_R2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_CHECK_INPUT_R2 .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA_R2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_GET_DATA_R2 .

  " 读取底表LOG日志记录

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE LT_LOG
  FROM ZQMT0066_LOG
  WHERE AUFNR IN S_ZAUFNR AND CREATEDATE IN S_CREDAT.

  IF LT_LOG IS INITIAL.
    MESSAGE S001(00) WITH '没有满足条件的日志记录.' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT LT_LOG BY CREATEDATE CREATETIME AUFNR ZSIGN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PROCESS_DATA_R1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA_R1 .

  CLEAR: LV_COUNT.

  LOOP AT LT_LIST INTO LS_LIST.

    LV_COUNT = LV_COUNT + 1.

    MOVE-CORRESPONDING LS_LIST TO LS_ALVR.

    LS_ALVR-ZINDEX = LV_COUNT.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = LS_ALVR-AUFNR
      IMPORTING
        OUTPUT = LS_ALVR-ZAUFNR.

    CASE LS_ALVR-ZSIGN.
      WHEN 'A'.
        LS_ALVR-ZSIGNTXT = '组合装工单'.
      WHEN 'B'.
        LS_ALVR-ZSIGNTXT = '非组合装工单'.
      WHEN OTHERS.
    ENDCASE.

    APPEND LS_ALVR TO LT_ALVR.

    CLEAR: LS_LIST,LS_ALVR.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SHOW_ALV_R1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SHOW_ALV_R1 .

* FIELDCAT设置
  PERFORM FRM_SET_FIELDCAT1.

* LAYOUT设置
  PERFORM FRM_SET_LAYOUT1.

* ALV画面输出
  PERFORM FRM_OUTPUT_ALV1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PROCESS_DATA_R2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_PROCESS_DATA_R2 .

  LOOP AT LT_LOG INTO LS_LOG.

    MOVE-CORRESPONDING LS_LOG TO LS_LOGSHOW.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = LS_LOGSHOW-AUFNR
      IMPORTING
        OUTPUT = LS_LOGSHOW-ZAUFNR.

    CASE LS_LOGSHOW-ZSIGN.
      WHEN 'A'.
        LS_LOGSHOW-ZSIGNTXT = '组合装工单'.
      WHEN 'B'.
        LS_LOGSHOW-ZSIGNTXT = '非组合装工单'.
      WHEN OTHERS.
    ENDCASE.

    APPEND LS_LOGSHOW TO LT_LOGSHOW.

    CLEAR: LS_LOG,LS_LOGSHOW.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SHOW_ALV_R2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SHOW_ALV_R2 .

* FIELDCAT设置
  PERFORM FRM_SET_FIELDCAT2.

* LAYOUT设置
  PERFORM FRM_SET_LAYOUT2.

* ALV画面输出
  PERFORM FRM_OUTPUT_ALV2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_FIELDCAT1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_SET_FIELDCAT1 .

  CLEAR: TD_FIELDCAT[].

  PERFORM FRM_SET_FIELDCAT_ATTRIBUTE  USING: 'SEL' '选择' 'X' 'X' '4' '0' SPACE SPACE 'X' SPACE SPACE,
                                             'ZAUFNR' '订单号' SPACE SPACE '12' '0' SPACE SPACE 'X' SPACE SPACE,
                                             'ZBY' '标样' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZJYQYL' '检验取样量' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZMPLYL' '每批日常留样' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZNDWJLY' '年度外检留样量' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZLYGC' '留样观察' SPACE SPACE '10' '0'SPACE SPACE SPACE SPACE SPACE,
                                             'ZCQWDXLY' '长期留样量' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZDQWDXLY' '短期留样量' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZJSWDXLY' '加速留样量' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'UJY' '检验样单位' SPACE SPACE '5' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ULY' '每批日常留样单位' SPACE SPACE '5' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZQTLY' '其它留样' SPACE SPACE '20' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZSIGN' '处理标识' SPACE SPACE '6' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZSIGNTXT' '处理标识描述' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,

                                             'ICON' '返回状态' SPACE SPACE '6' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'STATUS' '返回结果' SPACE SPACE '6' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'MESSAGE' '返回消息' SPACE SPACE '30' '0' SPACE SPACE SPACE SPACE SPACE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_LAYOUT1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_SET_LAYOUT1 .

  CLEAR: TH_LAYOUT.

  TH_LAYOUT-ZEBRA = 'X'.
  TH_LAYOUT-STYLEFNAME = 'STYLE'.
  TH_LAYOUT-INFO_FNAME = 'LINE_COL'.

  IS_SETTING-EDT_CLL_CB = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_OUTPUT_ALV1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_OUTPUT_ALV1 .

  DATA: LTD_EVENT TYPE TABLE OF SLIS_ALV_EVENT,
        LTH_EVENT TYPE SLIS_ALV_EVENT,
        W_TITLE   TYPE SY-TITLE.
  DATA: LT_EVENT TYPE SLIS_T_EVENT,
        LS_EVENT TYPE SLIS_ALV_EVENT.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'SET_PF'
      I_CALLBACK_USER_COMMAND  = 'USER_COM'
      IS_LAYOUT_LVC            = TH_LAYOUT
      IT_FIELDCAT_LVC          = TD_FIELDCAT[]
      I_GRID_SETTINGS          = IS_SETTING
      I_SAVE                   = 'A'
      IT_SORT_LVC              = IT_SORT_LVC
      IT_EVENTS                = LT_EVENT
    TABLES
      T_OUTTAB                 = LT_ALVR
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_FIELDCAT2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_SET_FIELDCAT2 .

  CLEAR: TD_FIELDCAT[].

  PERFORM FRM_SET_FIELDCAT_ATTRIBUTE  USING: 'ZAUFNR' '订单号' SPACE SPACE '12' '0' SPACE SPACE 'X' SPACE SPACE,
                                             'FUNNAME' '执行处理内容' SPACE SPACE '16' '0' SPACE SPACE 'X' SPACE SPACE,
                                             'ZNAME' '创建者' SPACE SPACE '12' '0' SPACE SPACE 'X' SPACE SPACE,
                                             'CREATEDATE' '创建日期' SPACE SPACE '10' '0' SPACE SPACE 'X' SPACE SPACE,
                                             'CREATETIME' '创建时间' SPACE SPACE '8' '0' SPACE SPACE 'X' SPACE SPACE,
                                             'ZBY' '标样' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZJYQYL' '检验取样量' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZMPLYL' '每批日常留样' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZNDWJLY' '年度外检留样量' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZLYGC' '留样观察' SPACE SPACE '10' '0'SPACE SPACE SPACE SPACE SPACE,
                                             'ZCQWDXLY' '长期留样量' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZDQWDXLY' '短期留样量' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZJSWDXLY' '加速留样量' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'UJY' '检验样单位' SPACE SPACE '5' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ULY' '每批日常留样单位' SPACE SPACE '5' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZQTLY' '其它留样' SPACE SPACE '20' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZSIGN' '处理标识' SPACE SPACE '6' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'ZSIGNTXT' '处理标识描述' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,

                                             'TYPE' '返回结果' SPACE SPACE '5' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'MESSAGE' '返回消息' SPACE SPACE '10' '0' SPACE SPACE SPACE SPACE SPACE,
                                             'GUID' '唯一标识值' SPACE SPACE '32' '0' SPACE SPACE SPACE SPACE SPACE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_LAYOUT2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_SET_LAYOUT2 .

  CLEAR: TH_LAYOUT.

  TH_LAYOUT-ZEBRA = 'X'.
  TH_LAYOUT-STYLEFNAME = 'STYLE'.
  TH_LAYOUT-INFO_FNAME = 'LINE_COL'.

  IS_SETTING-EDT_CLL_CB = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_OUTPUT_ALV2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FRM_OUTPUT_ALV2 .

  DATA: LTD_EVENT TYPE TABLE OF SLIS_ALV_EVENT,
        LTH_EVENT TYPE SLIS_ALV_EVENT,
        W_TITLE   TYPE SY-TITLE.
  DATA: LT_EVENT TYPE SLIS_T_EVENT,
        LS_EVENT TYPE SLIS_ALV_EVENT.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
**      I_CALLBACK_PF_STATUS_SET = 'SET_PF'
**      I_CALLBACK_USER_COMMAND  = 'USER_COM'
      IS_LAYOUT_LVC      = TH_LAYOUT
      IT_FIELDCAT_LVC    = TD_FIELDCAT[]
      I_GRID_SETTINGS    = IS_SETTING
      I_SAVE             = 'A'
      IT_SORT_LVC        = IT_SORT_LVC
      IT_EVENTS          = LT_EVENT
    TABLES
      T_OUTTAB           = LT_LOGSHOW
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_SET_FIELDCAT_ATTRIBUTE
*&---------------------------------------------------------------------*
*       FIELDCAT 各项属性设置
*----------------------------------------------------------------------*
*      -->I_FIELDNAME    字段名
*      -->I_SCRTEXT_L    标签名
*      -->I_EDIT         可编辑
*----------------------------------------------------------------------*

FORM FRM_SET_FIELDCAT_ATTRIBUTE USING I_FIELDNAME TYPE LVC_S_FCAT-FIELDNAME
                                       I_SCRTEXT_L  TYPE LVC_S_FCAT-SCRTEXT_L
                                       I_EDIT       TYPE LVC_S_FCAT-EDIT
                                       I_CHECKBOX   TYPE LVC_S_FCAT-CHECKBOX
                                       I_OUTPUTLEN  TYPE LVC_S_FCAT-OUTPUTLEN
                                       I_DECIMALS   TYPE LVC_S_FCAT-DECIMALS
                                       I_REF_FIELD  TYPE LVC_S_FCAT-REF_FIELD
                                       I_REF_TABLE  TYPE LVC_S_FCAT-REF_TABLE
                                       I_FIX_COLUMN TYPE LVC_S_FCAT-FIX_COLUMN
                                       I_NO_OUT     TYPE LVC_S_FCAT-NO_OUT
                                       I_F4AVAILABL TYPE LVC_S_FCAT-F4AVAILABL.

* 局部变量的定义

  DATA: LTH_FIELDCAT TYPE LVC_S_FCAT.

  LTH_FIELDCAT-FIELDNAME  = I_FIELDNAME.
  LTH_FIELDCAT-SCRTEXT_L  = I_SCRTEXT_L.
  LTH_FIELDCAT-EDIT       = I_EDIT.
  LTH_FIELDCAT-CHECKBOX   = I_CHECKBOX.
  LTH_FIELDCAT-OUTPUTLEN   = I_OUTPUTLEN.
  LTH_FIELDCAT-DECIMALS = I_DECIMALS.
  LTH_FIELDCAT-REF_FIELD = I_REF_FIELD.
  LTH_FIELDCAT-REF_TABLE = I_REF_TABLE.
  LTH_FIELDCAT-FIX_COLUMN = I_FIX_COLUMN.
  LTH_FIELDCAT-NO_OUT = I_NO_OUT.
  LTH_FIELDCAT-F4AVAILABL = I_F4AVAILABL.

  APPEND LTH_FIELDCAT TO TD_FIELDCAT.

  CLEAR: LTH_FIELDCAT.

ENDFORM. " FRM_SET_FIELDCAT_ATTRIBUTE
*&---------------------------------------------------------------------*
*&      Form  SET_PF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->EXTAB      text
*----------------------------------------------------------------------*
FORM SET_PF USING EXTAB TYPE SLIS_T_EXTAB.

  CLEAR: GT_CODE.

  IF LV_MARK = 'X'. " 如果无修改权限，修改与重新计算 按钮隐藏.
    APPEND 'ZUPDATE' TO GT_CODE.
    APPEND 'ZPOST'   TO GT_CODE.
  ENDIF.

  SET PF-STATUS 'STANDARD1' EXCLUDING GT_CODE.

ENDFORM. "SET_PF
*&---------------------------------------------------------------------*
*&      Form  USER_COM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM USER_COM USING R_UCOMM LIKE SY-UCOMM RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA: G_GRID TYPE REF TO CL_GUI_ALV_GRID.
  DATA: STBL TYPE LVC_S_STBL.    "稳定刷新
  DATA: LT_ROW TYPE  LVC_T_ROID,    "内表
        LS_ROW TYPE  LVC_S_ROID.    "工作区

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = G_GRID.

  CALL METHOD G_GRID->CHECK_CHANGED_DATA.

  STBL-ROW = 'X'." 基于行的稳定刷新
  STBL-COL = 'X'." 基于列稳定刷新

  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = STBL
    EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2.

  CASE R_UCOMM.

    WHEN 'ZALLY' .

      LS_ALVR-SEL = 'X'.
      MODIFY LT_ALVR FROM LS_ALVR TRANSPORTING SEL WHERE SEL = SPACE.

      RS_SELFIELD-REFRESH = 'X'.

    WHEN  'ZALLN'.

      LS_ALVR-SEL = SPACE.
      MODIFY LT_ALVR FROM LS_ALVR TRANSPORTING SEL WHERE SEL = 'X'.

      RS_SELFIELD-REFRESH = 'X'.

    WHEN  'ZUPDATE'.  "修改 留样相关参数数据

      CLEAR: LT_ALVRTMP,LS_ALVRTMP,LV_ANSWER,LV_COUNT,LV_ERR,LS_ALVRTMP,GS_ZQMT0066A,LV_CHECK.

      LOOP AT LT_ALVR INTO LS_ALVR.

        IF LS_ALVR-SEL = 'X'.

          MOVE-CORRESPONDING LS_ALVR TO LS_ALVRTMP.

          APPEND LS_ALVRTMP TO LT_ALVRTMP.

        ENDIF.

        CLEAR: LS_ALVR,LS_ALVRTMP.
      ENDLOOP.

      IF LT_ALVRTMP[] IS INITIAL.
        MESSAGE '请选择要处理的数据.' TYPE 'I'.
        RETURN.
      ENDIF.

      IF LINES( LT_ALVRTMP[] ) > 1.
        MESSAGE '请不要选择多行进行修改.' TYPE 'I'.
        RETURN.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TEXT_QUESTION         = TEXT-T01 "'确认修改信息?'
          DISPLAY_CANCEL_BUTTON = ''
          TEXT_BUTTON_1         = TEXT-T02 "'是'
          TEXT_BUTTON_2         = TEXT-T03 "'否'
        IMPORTING
          ANSWER                = LV_ANSWER.

      IF LV_ANSWER = '1'.

        READ TABLE LT_ALVRTMP INTO LS_ALVRTMP INDEX 1.

        MOVE-CORRESPONDING LS_ALVRTMP TO GS_ZQMT0066A.

        CALL SCREEN 9001 STARTING AT 10 2   " 弹出修改数据的自定义子屏幕 9001
                         ENDING AT 112 16.

      ENDIF.

      RS_SELFIELD-REFRESH = 'X'.

    WHEN  'ZPOST'. " 重新计算 留样数据

      CLEAR: LT_ALVRTMP,LS_ALVRTMP,LV_ANSWER,LV_COUNT,LV_ERR,LS_ALVRTMP,GS_ZQMT0066A,LV_CHECK.

      LOOP AT LT_ALVR INTO LS_ALVR.

        IF LS_ALVR-SEL = 'X'.

          MOVE-CORRESPONDING LS_ALVR TO LS_ALVRTMP.

          APPEND LS_ALVRTMP TO LT_ALVRTMP.

        ENDIF.

        CLEAR: LS_ALVR,LS_ALVRTMP.
      ENDLOOP.

      IF LT_ALVRTMP[] IS INITIAL.
        MESSAGE '请选择要处理的数据.' TYPE 'I'.
        RETURN.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TEXT_QUESTION         = TEXT-T05 "'确认重新计算?'
          DISPLAY_CANCEL_BUTTON = ''
          TEXT_BUTTON_1         = TEXT-T02 "'是'
          TEXT_BUTTON_2         = TEXT-T03 "'否'
        IMPORTING
          ANSWER                = LV_ANSWER.

      IF LV_ANSWER = '1'.

        LOOP AT LT_ALVRTMP INTO LS_ALVRTMP.

          IF LS_ALVRTMP-ZSIGN = 'A'.

            MOVE-CORRESPONDING LS_ALVRTMP TO LS_LISTIN.

            CALL FUNCTION 'Z_QM_GET_ZLYGC'  " 执行计算获得留样量
              EXPORTING
                I_INPUT  = LS_LISTIN
                I_ZSIGN  = 'A'
              IMPORTING
                E_OUTPUT = LS_LISTOUT
                E_TYPE   = LV_TYPE
                E_MEG    = LV_MEG.

            IF LV_TYPE = 'S'.

              MOVE-CORRESPONDING LS_ALVRTMP TO GS_ZQMT0066A.

              GS_ZQMT0066A-ZMPLYL = LS_LISTOUT-ZMPLYL.
              GS_ZQMT0066A-ZQTLY = LS_LISTOUT-ZQTLY.
              GS_ZQMT0066A-ULY = LS_LISTOUT-ULY.
              GS_ZQMT0066A-ZLYGC = LS_LISTOUT-ZLYGC.

              MODIFY ZQMT0066A FROM GS_ZQMT0066A.

              IF SY-SUBRC = 0.

                COMMIT WORK AND WAIT.

                CALL FUNCTION 'Z_QM_GET_GUID'   "生成随机唯一值
                  IMPORTING
                    E_GUID = LV_GUID.

                MOVE-CORRESPONDING GS_ZQMT0066A TO LS_LOG.

                LS_LOG-GUID = LV_GUID.
                LS_LOG-FUNNAME = 'COUNT_AGAIN'.
                LS_LOG-CREATEDATE = SY-DATUM.
                LS_LOG-CREATETIME = SY-UZEIT.
                LS_LOG-ZNAME = SY-UNAME.
                LS_LOG-TYPE = 'S'.
                LS_LOG-MESSAGE = '数据重新计算.'.

                MODIFY ZQMT0066_LOG FROM LS_LOG. "日志记录

                IF SY-SUBRC = 0.
                  COMMIT WORK AND WAIT.
                ELSE.
                  ROLLBACK WORK.
                ENDIF.

                MOVE-CORRESPONDING GS_ZQMT0066A TO LS_ALVR.

                LS_ALVR-ICON = ICON_GREEN_LIGHT.
                LS_ALVR-STATUS = 'S'.
                LS_ALVR-MESSAGE = '数据重新计算成功.'.

                MODIFY LT_ALVR FROM LS_ALVR TRANSPORTING ZMPLYL ZQTLY ULY ZLYGC ICON STATUS MESSAGE
                    WHERE AUFNR = LS_ALVR-AUFNR.

                LV_CHECK = 'X'.

              ELSE.

                ROLLBACK WORK.
              ENDIF.

            ENDIF.

          ELSEIF LS_ALVRTMP-ZSIGN = 'B'.

            MOVE-CORRESPONDING LS_ALVRTMP TO LS_LISTIN.

**            SELECT SINGLE *
**            INTO CORRESPONDING FIELDS OF LS_QALS
**            FROM QALS
**            WHERE AUFNR = LS_LISTIN-AUFNR.
**
**            IF SY-SUBRC = 0.
**              LS_LISTIN-ZJYQYL = LS_QALS-ZJYQYL.
**              LS_LISTIN-ZMPLYL = LS_QALS-ZMPLYL.
**            ENDIF.

            CALL FUNCTION 'Z_QM_GET_ZLYGC'
              EXPORTING
                I_INPUT  = LS_LISTIN
                I_ZSIGN  = 'B'
              IMPORTING
                E_OUTPUT = LS_LISTOUT
                E_TYPE   = LV_TYPE
                E_MEG    = LV_MEG.

            IF LV_TYPE = 'S'.

              MOVE-CORRESPONDING LS_ALVRTMP TO GS_ZQMT0066A.

**              GS_ZQMT0066A-ZJYQYL = LS_LISTOUT-ZJYQYL.
**              GS_ZQMT0066A-ZMPLYL = LS_LISTOUT-ZMPLYL.
              GS_ZQMT0066A-ZLYGC = LS_LISTOUT-ZLYGC.
              GS_ZQMT0066A-UJY = LS_LISTOUT-UJY.
              GS_ZQMT0066A-ULY = LS_LISTOUT-ULY.
              GS_ZQMT0066A-ZQTLY = LS_LISTOUT-ZQTLY.

              MODIFY ZQMT0066A FROM GS_ZQMT0066A.

              IF SY-SUBRC = 0.

                COMMIT WORK AND WAIT.

                CALL FUNCTION 'Z_QM_GET_GUID'
                  IMPORTING
                    E_GUID = LV_GUID.

                MOVE-CORRESPONDING GS_ZQMT0066A TO LS_LOG.

                LS_LOG-GUID = LV_GUID.
                LS_LOG-FUNNAME = 'COUNT_AGAIN'.
                LS_LOG-CREATEDATE = SY-DATUM.
                LS_LOG-CREATETIME = SY-UZEIT.
                LS_LOG-ZNAME = SY-UNAME.
                LS_LOG-TYPE = 'S'.
                LS_LOG-MESSAGE = '数据重新计算.'.

                MODIFY ZQMT0066_LOG FROM LS_LOG.

                IF SY-SUBRC = 0.
                  COMMIT WORK AND WAIT.
                ELSE.
                  ROLLBACK WORK.
                ENDIF.

                MOVE-CORRESPONDING GS_ZQMT0066A TO LS_ALVR.

                LS_ALVR-ICON = ICON_GREEN_LIGHT.
                LS_ALVR-STATUS = 'S'.
                LS_ALVR-MESSAGE = '数据重新计算成功.'.

                MODIFY LT_ALVR FROM LS_ALVR TRANSPORTING ZLYGC UJY ULY ZQTLY ICON STATUS MESSAGE
                    WHERE AUFNR = LS_ALVR-AUFNR.

                LV_CHECK = 'X'.

              ELSE.

                ROLLBACK WORK.
              ENDIF.

            ENDIF.

          ELSE.

            LV_CHECK = 'X'.
          ENDIF.

          IF LV_CHECK <> 'X'.

            MOVE-CORRESPONDING LS_ALVRTMP TO LS_ALVR.

            LS_ALVR-ICON = ICON_RED_LIGHT.
            LS_ALVR-STATUS = 'E'.
            LS_ALVR-MESSAGE = '数据重新计算失败.'.

            MODIFY LT_ALVR FROM LS_ALVR TRANSPORTING ICON STATUS MESSAGE WHERE AUFNR = LS_ALVR-AUFNR.
          ENDIF.

          CLEAR: LS_ALVRTMP,GS_ZQMT0066A,LS_LISTIN,LS_LISTOUT,LV_TYPE,LV_MEG,LS_ALVR,LS_LOG,LV_GUID,LV_CHECK.
        ENDLOOP.

        LV_INFO = '数据重新计算完成.'.

        MESSAGE I001(00) WITH LV_INFO.

      ENDIF.

      RS_SELFIELD-REFRESH = 'X'.

    WHEN  'ZLOG'.  " 查看工单对应操作日志

      CLEAR: LT_ALVRTMP,LS_ALVRTMP,LV_ANSWER,LV_COUNT,LV_ERR,LS_ALVRTMP,GS_ZQMT0066A,LV_CHECK.

      LOOP AT LT_ALVR INTO LS_ALVR.

        IF LS_ALVR-SEL = 'X'.

          MOVE-CORRESPONDING LS_ALVR TO LS_ALVRTMP.

          APPEND LS_ALVRTMP TO LT_ALVRTMP.

        ENDIF.

        CLEAR: LS_ALVR,LS_ALVRTMP.
      ENDLOOP.

      IF LT_ALVRTMP[] IS INITIAL.
        MESSAGE '请选择要查询的数据.' TYPE 'I'.
        RETURN.
      ENDIF.

      CLEAR: R_ZAUFNR[],R_ZAUFNR.

      LOOP AT LT_ALVRTMP INTO LS_ALVRTMP.

        R_ZAUFNR-SIGN = 'I'.
        R_ZAUFNR-OPTION = 'EQ'.
        R_ZAUFNR-LOW = LS_ALVRTMP-AUFNR.
        APPEND R_ZAUFNR.

        CLEAR: LS_ALVRTMP.
      ENDLOOP.

      " 调用当前自身程序，查询相关工单的业务操作日志

      SUBMIT ZQMR0066
          WITH S_ZAUFNR IN R_ZAUFNR
          WITH R1 = ''
          WITH R2 = 'X'
          AND RETURN.

      RS_SELFIELD-REFRESH = 'X'.

    WHEN OTHERS.
  ENDCASE.

ENDFORM. "USER_COM
*&---------------------------------------------------------------------*
*& Module STATUS_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.

  SET PF-STATUS 'PF_9001'.
  SET TITLEBAR 'TI_9001'.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module HEAD_SHOW_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE HEAD_SHOW_9001 OUTPUT.

  MOVE-CORRESPONDING GS_ZQMT0066A TO ZQMS0066.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001_EXIT INPUT.

  SAVE_OK = OK_CODE_9001.
  CLEAR: OK_CODE_9001.

  CASE SAVE_OK.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.

  SAVE_OK = OK_CODE_9001.
  CLEAR: OK_CODE_9001.

  CLEAR: LV_ANSWER1,LV_INFO,LS_LOG,LV_GUID,LS_ALVR.

  CASE SAVE_OK.

    WHEN 'SUBMIT'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TEXT_QUESTION         = TEXT-T04 "'确认保存提交?'
          DISPLAY_CANCEL_BUTTON = ''
          TEXT_BUTTON_1         = TEXT-T02 "'是'
          TEXT_BUTTON_2         = TEXT-T03 "'否'
        IMPORTING
          ANSWER                = LV_ANSWER1.

      IF LV_ANSWER1 = '1'.

        GS_ZQMT0066A-ZBY = ZQMS0066-ZBY.
        GS_ZQMT0066A-ZJYQYL = ZQMS0066-ZJYQYL.
        GS_ZQMT0066A-ZMPLYL = ZQMS0066-ZMPLYL.
        GS_ZQMT0066A-ZNDWJLY = ZQMS0066-ZNDWJLY.
        GS_ZQMT0066A-ZLYGC = ZQMS0066-ZLYGC.
        GS_ZQMT0066A-ZCQWDXLY = ZQMS0066-ZCQWDXLY.
        GS_ZQMT0066A-ZDQWDXLY = ZQMS0066-ZDQWDXLY.
        GS_ZQMT0066A-ZJSWDXLY = ZQMS0066-ZJSWDXLY.
        GS_ZQMT0066A-ZQTLY = ZQMS0066-ZQTLY.

        MODIFY ZQMT0066A FROM GS_ZQMT0066A. " 修改底表相关参数数据

        IF SY-SUBRC = 0.

          COMMIT WORK AND WAIT.

          CALL FUNCTION 'Z_QM_GET_GUID'
            IMPORTING
              E_GUID = LV_GUID.

          MOVE-CORRESPONDING GS_ZQMT0066A TO LS_LOG.

          LS_LOG-GUID = LV_GUID.
          LS_LOG-FUNNAME = 'UPDATE_INFO'.
          LS_LOG-CREATEDATE = SY-DATUM.
          LS_LOG-CREATETIME = SY-UZEIT.
          LS_LOG-ZNAME = SY-UNAME.
          LS_LOG-TYPE = 'S'.
          LS_LOG-MESSAGE = '数据信息修改成功.'.

          MODIFY ZQMT0066_LOG FROM LS_LOG.

          IF SY-SUBRC = 0.
            COMMIT WORK AND WAIT.
          ELSE.
            ROLLBACK WORK.
          ENDIF.

          MOVE-CORRESPONDING GS_ZQMT0066A TO LS_ALVR.

          LS_ALVR-ICON = ICON_GREEN_LIGHT.
          LS_ALVR-STATUS = 'S'.
          LS_ALVR-MESSAGE = '数据信息修改成功.'.

          MODIFY LT_ALVR FROM LS_ALVR TRANSPORTING ZBY ZJYQYL ZMPLYL ZNDWJLY ZLYGC ZCQWDXLY
              ZDQWDXLY ZJSWDXLY ZQTLY ICON STATUS MESSAGE WHERE AUFNR = LS_ALVR-AUFNR.

          LV_INFO = '数据保存成功.'.

          MESSAGE I001(00) WITH LV_INFO.

          LEAVE TO SCREEN 0.
        ELSE.

          ROLLBACK WORK.

          LV_INFO = '数据保存失败.'.

          MESSAGE I001(00) WITH LV_INFO.
        ENDIF.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.