*&---------------------------------------------------------------------*
*& 包含               ZMMR0050_MODULE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  CASE 'X'.
    WHEN RB_01.
      GV_TITLE = '对账单创建'.
    WHEN RB_02.
      GV_TITLE = '对账单修改'.
    WHEN RB_03.
      GV_TITLE = '对账单查询'.
  ENDCASE.

  SET PF-STATUS 'STATUS'.
  SET TITLEBAR 'ALVTITLE' WITH GV_TITLE.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module SET_SCREEN_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE SET_SCREEN_0100 OUTPUT.

  PERFORM FRM_SET_ALV .

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT' OR'STOP'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.




*&---------------------------------------------------------------------*
*&      Module  MOD_GET_MATKL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MOD_GET_MAKTX INPUT.

  SELECT SINGLE MAKTX FROM MAKT INTO GS_ALV-MAKTX WHERE MATNR = GS_ALV-MATNR.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'STATUS_200'.
  SET TITLEBAR 'TITLE_200'.

  CASE  'X'.
    WHEN RB_01.
      LOOP AT SCREEN .
        IF SCREEN-NAME = 'GS_ALV-ZDZDH'.
          SCREEN-INPUT = 0.
          "    SCREEN-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN RB_02.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  DATA : LT_ALV_PAI TYPE TABLE OF TY_TAB .
  DATA : LS_ALV_PAI TYPE  TY_TAB .

  GS_ALV-ZSJHJ = GS_ALV-DMBTR + GS_ALV-ZSHUI .
  CASE  OK_CODE.
    WHEN 'ZSAVE'.
      IF  RB_02 = 'X' AND GS_ALV-ZDZDH  IS INITIAL .
        MESSAGE '在修改时必须输入对账单号！' TYPE 'E'. "DISPLAY LIKE 'E'.
      ENDIF.

      GS_ALV-ERNAM       =  SY-UNAME.
      GS_ALV-ERDAT       =  SY-DATUM.
      GS_ALV-NAMLA = SY-UNAME.
      GS_ALV-BUDLA = SY-DATUM.
      IF GV_UCOMM = 'ZMOD' ."OR ( GS_ALV-ZDZDH <> ''  AND GS_ALV-ZDZXH <> '' )."修改的时候 直接更改原行
        MODIFY GT_ALV FROM GS_ALV INDEX GV_TABIX1 .
      ELSE .
        GS_ALV-ZSJZT = 'Y'.
*        GS_ALV-ZDZZT = 'N'.
        READ TABLE GT_DOM_APSTT INTO DATA(LS_DOM_APSTT) WITH KEY DOMVALUE_L = GS_ALV-ZDZZT.
        IF  SY-SUBRC = 0.
          GS_ALV-ZDZZTMS  = LS_DOM_APSTT-DDTEXT.
        ENDIF.

*      IF   RB_02 = 'X' .
        IF  GS_ALV-ZDZDH <> '' AND GS_ALV-ZDZXH = ''  .
          DATA:LS_ZMMT0004 TYPE ZMMT0004,
               LT_ZMMT0004 LIKE TABLE OF LS_ZMMT0004.
          DATA:LV_ZDZXH TYPE ZMMT0004-ZDZXH.

          SELECT SINGLE MAX( ZDZXH ) INTO LV_ZDZXH
               FROM ZMMT0004 WHERE ZDZDH = GS_ALV-ZDZDH. "获取对账单号目前最大的行项目
          IF SY-SUBRC <> 0 ."如果获取不到的时候 从屏幕上读取
            CLEAR LT_ALV_PAI .
            LT_ALV_PAI[] = GT_ALV[] .
            SORT LT_ALV_PAI BY ZDZDH ZDZXH DESCENDING .
            CLEAR LS_ALV_PAI .
            READ TABLE LT_ALV_PAI INTO LS_ALV_PAI WITH KEY ZDZDH = GS_ALV-ZDZDH .
            LV_ZDZXH = LS_ALV_PAI-ZDZXH .
          ENDIF .

          GS_ALV-ZDZXH = LV_ZDZXH  + 1.
*          GS_ALV-DMBTR = GS_ALV-ZSJHJ.
          MOVE-CORRESPONDING GS_ALV TO LS_ZMMT0004 .
*        "       APPEND LS_ZMMT0004 TO LT_ZMMT0004.
          MODIFY ZMMT0004 FROM LS_ZMMT0004.
          COMMIT WORK AND WAIT .
        ENDIF.
*      APPEND GS_ALV TO GT_ALV.
        GV_TABIX1 = GV_TABIX1  + 1 .
        INSERT GS_ALV INTO GT_ALV INDEX GV_TABIX1 .
*      SORT GT_ALV BY LIFNR EBELN EBELP.

      ENDIF .
      MOVE-CORRESPONDING GS_ALV TO LS_ZMMT0004 .
      IF LS_ZMMT0004-ZDZXH <> '' .
        MODIFY ZMMT0004 FROM LS_ZMMT0004.
        COMMIT WORK AND WAIT .
      ENDIF .
      PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
      PERFORM FRM_LEAVE_SCREEN.

    WHEN 'ZNO'.
      PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
      PERFORM FRM_LEAVE_SCREEN.
  ENDCASE.

ENDMODULE.


MODULE EXIT_COMMAND_0200 INPUT.
  CASE OK_CODE.
    WHEN 'NO'.
      PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
      PERFORM FRM_LEAVE_SCREEN.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MOD_GET_NAME1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MOD_GET_NAME1 INPUT.
  SELECT SINGLE NAME1 FROM LFA1 INTO GS_ALV-NAME1 WHERE LIFNR = GS_ALV-LIFNR.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_2000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_2000 OUTPUT.
  SET PF-STATUS 'STATUS_2000'.
* SET TITLEBAR 'xxx'.

  PERFORM FRM_SET_LONGTEXT USING G_PR_CON
                                 G_PR_LTXT
*                               'Z' "G_PV_CNAM
                                 G_PV_EDIT
                                 GS_ALV-ZDZWB ."传入的值内容
ENDMODULE .
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_LONGTEXT
*&---------------------------------------------------------------------*
*       显示长文本空间
*----------------------------------------------------------------------*
FORM FRM_SET_LONGTEXT
                    USING    PR_CON  TYPE REF TO CL_GUI_CUSTOM_CONTAINER
                             PR_LTXT TYPE REF TO CL_GUI_TEXTEDIT
*                             PV_CNAM TYPE C
                             PV_EDIT TYPE C  "‘X’-可编辑，‘’不可编辑
                             PV_VALUE.
  DATA:LV_LENGTH TYPE I,
       LV_POS    TYPE I,
       LV_MODE   TYPE I.
  DATA:BEGIN OF LS_TABLE,
         LINE(C_LINE_LENGTH) TYPE C,
       END OF LS_TABLE,
       LT_TABLE LIKE TABLE OF LS_TABLE.
  DESCRIBE FIELD PV_VALUE LENGTH LV_LENGTH IN CHARACTER MODE.
  IF PR_CON IS INITIAL.
*   创建容器
    CREATE OBJECT PR_CON
      EXPORTING
        CONTAINER_NAME              = 'ZZ01' "屏幕上创建自定义区域
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
*   创建文本框 ==
    CREATE OBJECT PR_LTXT
      EXPORTING
        MAX_NUMBER_CHARS           = LV_LENGTH      "可输入文本长度
        WORDWRAP_MODE              =
                                     CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
        WORDWRAP_POSITION          = '10' "行长度
        WORDWRAP_TO_LINEBREAK_MODE = CL_GUI_TEXTEDIT=>TRUE
        PARENT                     = PR_CON
      EXCEPTIONS
        ERROR_CNTL_CREATE          = 1
        ERROR_CNTL_INIT            = 2
        ERROR_CNTL_LINK            = 3
        ERROR_DP_CREATE            = 4
        GUI_TYPE_NOT_SUPPORTED     = 5
        OTHERS                     = 6.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
  CHECK PR_LTXT IS NOT INITIAL.
  IF PV_EDIT EQ 'X'.
    LV_MODE   = 0.
  ELSE.
    LV_MODE   = 1.
  ENDIF.
  PR_LTXT->SET_READONLY_MODE(
    EXPORTING
      READONLY_MODE          = LV_MODE
    EXCEPTIONS
      ERROR_CNTL_CALL_METHOD = 1
      INVALID_PARAMETER      = 2
         ).
  IF SY-SUBRC <> 0.
*   Implement suitable error handling here
  ENDIF.
*设置显示文本 根据行长度进行拆分
  CLEAR:LT_TABLE[],LV_POS.
  DO.
    CLEAR:LS_TABLE.
    IF LV_POS >= LV_LENGTH.
      EXIT.
    ELSEIF ( LV_POS + C_LINE_LENGTH ) > LV_LENGTH.
      LS_TABLE-LINE = PV_VALUE+LV_POS.
    ELSE.
      LS_TABLE-LINE = PV_VALUE+LV_POS(C_LINE_LENGTH).
    ENDIF.
    APPEND LS_TABLE TO LT_TABLE.
    ADD C_LINE_LENGTH TO LV_POS.
  ENDDO.
  PR_LTXT->SET_TEXT_AS_R3TABLE(
    EXPORTING
      TABLE           = LT_TABLE
    EXCEPTIONS
      ERROR_DP        = 1
      ERROR_DP_CREATE = 2
         ).
  IF SY-SUBRC <> 0.
*   Implement suitable error handling here
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_LONGTEXT
*&---------------------------------------------------------------------*
*       读取长文本
*----------------------------------------------------------------------*
FORM FRM_GET_LONGTEXT  USING
                             PR_LTXT TYPE REF TO CL_GUI_TEXTEDIT
                             PV_VALUE TYPE C.
  DATA:BEGIN OF LS_TABLE,
         LINE(C_LINE_LENGTH) TYPE C,
       END OF LS_TABLE,
       LT_TABLE LIKE TABLE OF LS_TABLE.
  PR_LTXT->GET_TEXT_AS_R3TABLE(
   IMPORTING
     TABLE           = LT_TABLE
   EXCEPTIONS
     ERROR_DP        = 1
     ERROR_DP_CREATE = 2
        ).
  IF SY-SUBRC <> 0.
*   Implement suitable error handling here
  ELSE.
    CLEAR:PV_VALUE.
    LOOP AT LT_TABLE INTO LS_TABLE.
      CONCATENATE PV_VALUE LS_TABLE-LINE INTO PV_VALUE.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  MOD_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MOD_EXIT INPUT.
  PERFORM FRM_LEAVE_SCREEN.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form FRM_LEAVE_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM FRM_LEAVE_SCREEN .
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  MOD_GET_ZSJHJ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MOD_GET_ZSJHJ INPUT.

  GS_ALV-ZSJHJ = GS_ALV-DMBTR + GS_ALV-ZSHUI .

ENDMODULE.