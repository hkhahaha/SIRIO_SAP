*&---------------------------------------------------------------------*
*& Report ZPPR0064
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPPR0064.
TYPE-POOLS:SLIS."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:FIELDCAT TYPE LVC_T_FCAT WITH HEADER LINE,
     LAYOUT   TYPE LVC_S_LAYO,
     W_REPID  TYPE SY-REPID. "记录系统当前的程序名
TYPES:BEGIN OF TY_ALV,
        VBELN    TYPE VBAP-VBELN,
        POSNR    TYPE VBAP-POSNR,
        ERNAM    TYPE VBAP-ERNAM,
        WERKS    TYPE VBAP-WERKS  , " 工厂
        MATNR    TYPE VBAP-MATNR  , "  物料
        ARKTX    TYPE VBAP-ARKTX  , "  物料描述
        MATKL    TYPE VBAP-MATKL  , "  物料组
        ZATPDFRQ TYPE VBAP-ZATPDFRQ  , "  ATP答复日期 S4DK905875
        ZNBJHQ   TYPE VBAP-ZNBJHQ  , "  内部交货期 S4DK905875
        STATUS   TYPE ZPPT0064-STATUS, "状态
        MESSAGE  TYPE ZPPT0064-MESSAGE, "信息
      END OF TY_ALV.
DATA:LT_ALV TYPE TABLE OF TY_ALV,
     LS_ALV TYPE TY_ALV.
"定义屏幕
TABLES: VBAP.
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
S_WERKS FOR VBAP-WERKS,"工厂
S_VBELN FOR VBAP-VBELN,"销售订单
S_POSNR FOR VBAP-POSNR."行项目.

SELECTION-SCREEN END OF BLOCK BLK.

START-OF-SELECTION.

  DATA : LT_INPUT       TYPE ZMT_OA2SAP_MATERIAL_CHECK,
         RECIPIENT      TYPE REF TO IF_RECIPIENT_BCS,
         MAIN_TEXT      TYPE BCSY_TEXT,
         SEND_REQUEST   TYPE REF TO CL_BCS,
         DOCUMENT       TYPE REF TO CL_DOCUMENT_BCS,
         BCS_EXCEPTION  TYPE REF TO CX_BCS,
         LO_SENDER      TYPE REF TO CL_SAPUSER_BCS,
         SIZE           TYPE SO_OBJ_LEN,
         BINARY_CONTENT TYPE SOLIX_TAB,
         SENT_TO_ALL    TYPE OS_BOOLEAN,
         GC_TAB         TYPE C VALUE CL_BCS_CONVERT=>GC_TAB,
         GC_CRLF        TYPE C VALUE CL_BCS_CONVERT=>GC_CRLF,
         LT_INPUT_TAB   TYPE ZDT_OA2SAP_MATERIAL_CHECK__TAB,
         LS_INPUT_ITEM  TYPE ZDT_OA2SAP_MATERIAL_CHECK_ITEM,
         LT_OUTPUT      TYPE ZMT_OA2SAP_MATERIAL_CHECK_RET,

         LT_OUTPUT_TAB  TYPE ZDT_OA2SAP_MATERAIL_CHECK_TAB1,
         LS_OUTPUT_ITEM TYPE ZDT_OA2SAP_MATERAIL_CHECK_RET1.
  SELECT VBAP~* INTO TABLE @DATA(LT_VBAP) FROM VBAP
  LEFT JOIN VBEP ON VBAP~VBELN = VBEP~VBELN AND VBAP~POSNR = VBEP~POSNR
  WHERE VBEP~ETTYP = 'ZA'                                   "S4DK905875
    AND VBAP~WERKS IN @S_WERKS
    AND VBAP~VBELN IN @S_VBELN
    AND VBAP~POSNR IN @S_POSNR.
  SELECT * INTO TABLE @DATA(LT_ZPPT0064) FROM ZPPT0064.
  SORT LT_VBAP BY MATNR WERKS.
  DATA LT_ZPPT0064_1 TYPE TABLE OF ZPPT0064.

*  检查 S4DK905875
  PERFORM CHECKBOM.
*  后台job直接检查并发送邮件
  IF SY-BATCH IS NOT INITIAL.

*  后台任务发送邮件
    PERFORM SENDEMAIL_BOM.                                  "S4DK905744
    PERFORM SENDEMAIL_VAL.                                  "S4DK905744
  ELSE.
*  前台查询数据
    PERFORM GETDATA.
*    展示数据
    PERFORM CATALOG.
    PERFORM ALVSHOW.
  ENDIF.

END-OF-SELECTION.

FORM GETDATA.
  LOOP AT LT_VBAP INTO DATA(LS_VBAP).
    CLEAR LS_ALV.
    READ TABLE LT_ZPPT0064_1 INTO DATA(LS_ZPPT0064) WITH KEY VBELN = LS_VBAP-VBELN
                                                             POSNR = LS_VBAP-POSNR
                                                             MATNR = |{ LS_VBAP-MATNR ALPHA = OUT WIDTH = 9 }| "S4DK905875
                                                             WERKS = LS_VBAP-WERKS.
    LS_ALV-VBELN = LS_VBAP-VBELN.
    LS_ALV-POSNR = LS_VBAP-POSNR.
    LS_ALV-ERNAM = LS_VBAP-ERNAM.                           "S4DK905875
    LS_ALV-WERKS = LS_VBAP-WERKS.
    LS_ALV-MATNR = LS_VBAP-MATNR.
    LS_ALV-ARKTX = LS_VBAP-ARKTX.
    LS_ALV-MATKL = LS_VBAP-MATKL.
    LS_ALV-ZATPDFRQ = LS_VBAP-ZATPDFRQ.                     "S4DK905875
    LS_ALV-ZNBJHQ = LS_VBAP-ZNBJHQ.                         "S4DK905875
    LS_ALV-STATUS = LS_ZPPT0064-STATUS.
    LS_ALV-MESSAGE = LS_ZPPT0064-MESSAGE.

    APPEND LS_ALV TO LT_ALV.
  ENDLOOP.
ENDFORM.

FORM CHECKBOM.
  DELETE ADJACENT DUPLICATES FROM LT_VBAP COMPARING VBELN POSNR WERKS MATNR .
  LOOP AT LT_VBAP INTO DATA(LS_VBAP).
*    READ TABLE LT_ZPPT0064 INTO DATA(LS_ZPPT0064) WITH KEY MATNR = LS_VBAP-MATNR
*                                                           WERKS = LS_VBAP-WERKS
*                                                           STATUS = 'S'.
*    IF LS_ZPPT0064 IS INITIAL.
    PERFORM CHECK USING LS_VBAP.
*    ENDIF.
*    CLEAR LS_ZPPT0064.

  ENDLOOP.
ENDFORM.

FORM CHECK USING LS_VBAP TYPE VBAP.
  DATA LS_ZPPT0064 TYPE ZPPT0064.
  CLEAR :LT_INPUT, LS_INPUT_ITEM,LT_INPUT_TAB,LT_OUTPUT.
  LS_INPUT_ITEM-MATNR = LS_VBAP-MATNR.
  LS_INPUT_ITEM-WERKS = LS_VBAP-WERKS.
  APPEND LS_INPUT_ITEM TO LT_INPUT_TAB.
  LT_INPUT-MT_OA2SAP_MATERIAL_CHECK-ITEMS = LT_INPUT_TAB.

  CALL FUNCTION 'ZPP_OA2SAP_CHECK'
    EXPORTING
      INPUT  = LT_INPUT
    IMPORTING
      OUTPUT = LT_OUTPUT.
  LS_ZPPT0064-VBELN = LS_VBAP-VBELN.
  LS_ZPPT0064-POSNR = LS_VBAP-POSNR.
  LS_ZPPT0064-ERNAM = LS_VBAP-ERNAM.
  LS_ZPPT0064-MATNR = LS_INPUT_ITEM-MATNR.
  LS_ZPPT0064-WERKS = LS_INPUT_ITEM-WERKS.
  LS_ZPPT0064-ZATPDFRQ = LS_VBAP-ZATPDFRQ.
  LS_ZPPT0064-ZNBJHQ = LS_VBAP-ZNBJHQ.
  LS_ZPPT0064-UDLDATE = SY-DATUM.
  LS_ZPPT0064-STATUS = 'S'.
  IF LT_OUTPUT-MT_OA2SAP_MATERIAL_CHECK_RET-TYPE = 'E'.
    LS_ZPPT0064-STATUS = LT_OUTPUT-MT_OA2SAP_MATERIAL_CHECK_RET-TYPE.
    LS_ZPPT0064-MESSAGE = LT_OUTPUT-MT_OA2SAP_MATERIAL_CHECK_RET-MSG.
    RETURN.
  ELSE.
    DATA LS_ZPPT0064_1 TYPE ZPPT0064.
    LT_OUTPUT_TAB = LT_OUTPUT-MT_OA2SAP_MATERIAL_CHECK_RET-RETURN.
    LOOP AT LT_OUTPUT_TAB INTO LS_OUTPUT_ITEM.
      LS_ZPPT0064_1-VBELN = LS_VBAP-VBELN.
      LS_ZPPT0064_1-POSNR = LS_VBAP-POSNR.
      LS_ZPPT0064_1-MATNR = LS_OUTPUT_ITEM-MATNR.
      LS_ZPPT0064_1-WERKS = LS_OUTPUT_ITEM-WERKS.
      LS_ZPPT0064_1-UDLDATE = SY-DATUM.
      LS_ZPPT0064_1-ERNAM = LS_VBAP-ERNAM.
      LS_ZPPT0064_1-ZATPDFRQ = LS_VBAP-ZATPDFRQ.
      LS_ZPPT0064_1-ZNBJHQ = LS_VBAP-ZNBJHQ.

      LS_ZPPT0064_1-STATUS = LS_OUTPUT_ITEM-RETURNCODE.
      LS_ZPPT0064_1-MESSAGE = LS_OUTPUT_ITEM-RETURNINFO.
      IF LS_OUTPUT_ITEM-RETURNCODE = 'E'.
        LS_ZPPT0064-STATUS = LS_OUTPUT_ITEM-RETURNCODE.
        LS_ZPPT0064-MESSAGE = LS_ZPPT0064-MESSAGE && LS_OUTPUT_ITEM-RETURNINFO.
      ENDIF.
      APPEND LS_ZPPT0064_1 TO LT_ZPPT0064_1.
    ENDLOOP.
  ENDIF.
  IF LS_ZPPT0064-STATUS = 'S'.
    LS_ZPPT0064-MESSAGE = '检查成功'.
  ENDIF.
  MODIFY ZPPT0064 FROM LS_ZPPT0064.
  CLEAR LS_ZPPT0064.
ENDFORM.
FORM CATALOG.
  CLEAR: FIELDCAT[], SY-TABIX.

  DEFINE FIELDCATSET."宏定义
    fieldcat-just = 'C'."字段居中显示
    fieldcat-outputlen = 10."自定义字段的长度
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-coltext = &2."ALV列名
    fieldcat-col_pos = sy-tabix."列位置
    fieldcat-NO_ZERO = 'X'.
    APPEND fieldcat.
  END-OF-DEFINITION.

  FIELDCATSET 'VBELN' '销售订单'.
  FIELDCATSET 'POSNR' '行项目'.
  FIELDCATSET 'ERNAM' '制单人'.                                "S4DK905875
  FIELDCATSET 'WERKS' '工厂'.
*  FIELDCATSET 'ETENR' '计划行'.
*  FIELDCATSET 'ETTYP' '计划行类别'.
  FIELDCATSET 'MATNR' '物料'.
  FIELDCATSET 'ARKTX' '物料描述'.
  FIELDCATSET 'MATKL' '物料组'.
  FIELDCATSET 'ZATPDFRQ' 'ATP答复日期'.                         "S4DK905875
  FIELDCATSET 'ZNBJHQ' '内部交货期'.                             "S4DK905875
  FIELDCATSET 'STATUS' '状态'.
  FIELDCATSET 'MESSAGE' '信息'.
  LAYOUT-ZEBRA = 'X'.
*  LAYOUT-BOX_FNAME = 'SEL'.
  LAYOUT-CWIDTH_OPT = 'X'.
ENDFORM.

FORM ALVSHOW.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'FRM_USER_STATUS'
      I_CALLBACK_USER_COMMAND  = 'FRM_USER_COMMAND'
      I_GRID_TITLE             = '物料可用性检查'
      IS_LAYOUT_LVC            = LAYOUT
      IT_FIELDCAT_LVC          = FIELDCAT[]
      I_SAVE                   = 'A'
    TABLES
      T_OUTTAB                 = LT_ALV
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.



FORM FRM_USER_STATUS  USING I_IT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS '0100' .
ENDFORM.                    " FRM_SET_PF_STATUS

FORM FRM_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.
    WHEN '&IC1'.
      READ TABLE LT_ALV INTO DATA(LS_ALV1) INDEX RS_SELFIELD-TABINDEX.
      IF SY-SUBRC EQ 0.
*        zcl_bc_public=>show_json( ls_alv1-MESSAGE ).
        DATA(OUT) = CL_DEMO_OUTPUT=>NEW( )->BEGIN_SECTION( '信息' ).
        OUT->WRITE_TEXT( ls_alv1-MESSAGE )->END_SECTION( )->DISPLAY( ).
      ENDIF.
  ENDCASE.

ENDFORM.

FORM SENDEMAIL_BOM.
  SELECT * INTO TABLE @DATA(LT_ZPPT0064B) FROM ZPPT0064B WHERE ACTIVE = 'X' AND ZSEND_TYPE = '1'. "S4DK905744
  IF SY-SUBRC <> 0.
*    未维护邮箱
    RETURN.
  ENDIF.
  PERFORM CREATE_CONTENT.
  CLEAR RECIPIENT.
  TRY.
      CLEAR MAIN_TEXT.
*     -------- create persistent send request ------------------------
      SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).

*     -------- create and set document with attachment ---------------
*     create document object from internal table with text
      APPEND 'Dear ALL：' TO MAIN_TEXT.
      APPEND '  附件为未清销售订单物料(未交货及传APS排产物料)及下阶物料可用性检查：物料状态、有效BOM、有效主配方、有效生产版本、BOP；详细返回信息见附件。' TO MAIN_TEXT.
      APPEND '  知悉！' TO MAIN_TEXT.

      DATA:LV_HEADER TYPE SO_OBJ_DES,
           LV_FILE   TYPE CHAR50.

      LV_HEADER = '未清销售订单物料可用性检查' && SY-DATUM.
      DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
      I_TYPE    = 'RAW'
      I_TEXT    = MAIN_TEXT
      I_SUBJECT =  LV_HEADER ).                             "#EC NOTEXT


      LV_FILE = SY-DATUM."'可用性检查日志' && SY-DATUM.
*     add the spread sheet as attachment to document object
      DOCUMENT->ADD_ATTACHMENT(
      I_ATTACHMENT_TYPE    = 'xls'                          "#EC NOTEXT
      I_ATTACHMENT_SUBJECT = LV_FILE                        "#EC NOTEXT
*      i_attachment_subject = 'Sales Daily.XLS' "#EC NOTEXT
      I_ATTACHMENT_SIZE    = SIZE
      I_ATT_CONTENT_HEX    = BINARY_CONTENT ).
*     发件人
*     前提是这个邮箱地址能发邮件，并且不需要密码
      LO_SENDER = CL_SAPUSER_BCS=>CREATE( 'MAILSERVER' ).
      SEND_REQUEST->SET_SENDER( LO_SENDER ).
*     add document object to send request
      SEND_REQUEST->SET_DOCUMENT( DOCUMENT ).

***********添加多个收件人************************
      DATA LV_STRING TYPE AD_SMTPADR.
      LOOP AT LT_ZPPT0064B INTO DATA(LS_ZPPT0064B).
        LV_STRING = LS_ZPPT0064B-MAIL.
        RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( LV_STRING ).
        SEND_REQUEST->ADD_RECIPIENT( RECIPIENT ).
        CLEAR LV_STRING.
      ENDLOOP.

*****************END*****************************
*     ---------- send document ---------------------------------------
*      sent_to_all = send_request->send( i_with_error_screen = 'X' ).

      SEND_REQUEST->SET_SEND_IMMEDIATELY( 'X' ).  "设置立即发送
      SEND_REQUEST->SEND_REQUEST->SET_LINK_TO_OUTBOX( 'X' ). "与outbox关联
      CALL METHOD SEND_REQUEST->SET_DOCUMENT( DOCUMENT ).
      CALL METHOD SEND_REQUEST->SEND(
        EXPORTING
          I_WITH_ERROR_SCREEN = 'X'
        RECEIVING
          RESULT              = SENT_TO_ALL ).
      COMMIT WORK.

      IF SENT_TO_ALL = 'X'.
        MESSAGE '发送成功' TYPE 'S'.
      ELSE.
        ROLLBACK WORK.
        MESSAGE '发送失败' TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

*   ------------ exception handling ----------------------------------
*   replace this rudimentary exception handling with your own one !!!
    CATCH CX_BCS INTO BCS_EXCEPTION.
      MESSAGE I865(SO) WITH BCS_EXCEPTION->ERROR_TYPE.
  ENDTRY.
  CLEAR:LV_FILE,BINARY_CONTENT.
ENDFORM.

*S4DK905744-S
FORM SENDEMAIL_VAL.
  SELECT * INTO TABLE @DATA(LT_ZPPT0064B) FROM ZPPT0064B WHERE ACTIVE = 'X' AND ZSEND_TYPE = '2'.
  IF SY-SUBRC <> 0.
*    未维护邮箱
    RETURN.
  ENDIF.
  PERFORM CREATE_CONTENT_VAL.
  CLEAR RECIPIENT.
  TRY.
      CLEAR MAIN_TEXT.
*     -------- create persistent send request ------------------------
      SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).

*     -------- create and set document with attachment ---------------
*     create document object from internal table with text
      APPEND 'Dear ALL：' TO MAIN_TEXT.
      APPEND '    附件为流程订单中自制品未发布标准价；详细返回信息见附件。' TO MAIN_TEXT.
      APPEND '  知悉！' TO MAIN_TEXT.

      DATA:LV_HEADER TYPE SO_OBJ_DES,
           LV_FILE   TYPE CHAR50.

      LV_HEADER = '流程订单中自制品未发布标准价' && SY-DATUM.
      DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
      I_TYPE    = 'RAW'
      I_TEXT    = MAIN_TEXT
      I_SUBJECT =  LV_HEADER ).                             "#EC NOTEXT


      LV_FILE = SY-DATUM."'流程订单中自制品未发布标准价清单' && SY-DATUM.
*     add the spread sheet as attachment to document object
      DOCUMENT->ADD_ATTACHMENT(
      I_ATTACHMENT_TYPE    = 'xls'                          "#EC NOTEXT
      I_ATTACHMENT_SUBJECT = LV_FILE                        "#EC NOTEXT
*      i_attachment_subject = 'Sales Daily.XLS' "#EC NOTEXT
      I_ATTACHMENT_SIZE    = SIZE
      I_ATT_CONTENT_HEX    = BINARY_CONTENT ).
*     发件人
*     前提是这个邮箱地址能发邮件，并且不需要密码
      LO_SENDER = CL_SAPUSER_BCS=>CREATE( 'MAILSERVER' ).
      SEND_REQUEST->SET_SENDER( LO_SENDER ).
*     add document object to send request
      SEND_REQUEST->SET_DOCUMENT( DOCUMENT ).

***********添加多个收件人************************
      DATA LV_STRING TYPE AD_SMTPADR.
      LOOP AT LT_ZPPT0064B INTO DATA(LS_ZPPT0064B).
        LV_STRING = LS_ZPPT0064B-MAIL.
        RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( LV_STRING ).
        SEND_REQUEST->ADD_RECIPIENT( RECIPIENT ).
        CLEAR LV_STRING.
      ENDLOOP.

*****************END*****************************
*     ---------- send document ---------------------------------------
*      sent_to_all = send_request->send( i_with_error_screen = 'X' ).

      SEND_REQUEST->SET_SEND_IMMEDIATELY( 'X' ).  "设置立即发送
      SEND_REQUEST->SEND_REQUEST->SET_LINK_TO_OUTBOX( 'X' ). "与outbox关联
      CALL METHOD SEND_REQUEST->SET_DOCUMENT( DOCUMENT ).
      CALL METHOD SEND_REQUEST->SEND(
        EXPORTING
          I_WITH_ERROR_SCREEN = 'X'
        RECEIVING
          RESULT              = SENT_TO_ALL ).
      COMMIT WORK.

      IF SENT_TO_ALL = 'X'.
        MESSAGE '发送成功' TYPE 'S'.
      ELSE.
        ROLLBACK WORK.
        MESSAGE '发送失败' TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

*   ------------ exception handling ----------------------------------
*   replace this rudimentary exception handling with your own one !!!
    CATCH CX_BCS INTO BCS_EXCEPTION.
      MESSAGE I865(SO) WITH BCS_EXCEPTION->ERROR_TYPE.
  ENDTRY.
  CLEAR:LV_FILE,BINARY_CONTENT.
ENDFORM.
*S4DK905744-E

*&---------------------------------------------------------------------*
*&      Form  create_content
*&---------------------------------------------------------------------*
* Create Example Content
* 1) Write example text into a string
* 2) convert this string to solix_tab
*----------------------------------------------------------------------*
FORM CREATE_CONTENT.

  DATA LV_STRING TYPE STRING.

  CLEAR:LV_STRING.
  CONCATENATE LV_STRING
  '销售订单'  GC_TAB
  '行号'  GC_TAB
  '制单人'  GC_TAB                                             "S4DK905875
  '工厂'  GC_TAB
  '物料'  GC_TAB
  '物料描述'  GC_TAB
  '物料组'   GC_TAB
  'ATP答复日期'   GC_TAB                                        "S4DK905875
  '内部交货期'   GC_TAB                                          "S4DK905875
  '成功代码' 	GC_TAB
  '错误结果信息' GC_CRLF
  INTO LV_STRING.
  DATA:LV_1(30),
       LV_2(30),
       LV_3(30),
       LV_4(30),
       LV_5(30),
       LV_6(30),
       LV_7(30),
       LV_8(30),
       LV_9(30),
       LV_10(30),
       LV_11(300) .
  LOOP AT LT_ZPPT0064_1 INTO DATA(LS_ZPPT0064_1) WHERE STATUS <> 'S'."只发送检查失败的数据
    CLEAR:
    LV_1,
    LV_2,
    LV_3,
    LV_4,
    LV_5,
    LV_6,
    LV_7,
    LV_8,
    LV_9,
    LV_10,
    LV_11.
    READ TABLE LT_VBAP INTO DATA(LS_VBAP) WITH KEY VBELN = LS_ZPPT0064_1-VBELN POSNR = LS_ZPPT0064_1-POSNR.
    LV_1 = LS_ZPPT0064_1-VBELN.
    LV_2 = LS_ZPPT0064_1-POSNR.
    LV_3 = LS_ZPPT0064_1-ERNAM.                             "S4DK905875
    LV_4 = LS_ZPPT0064_1-WERKS.
    LV_5 = LS_ZPPT0064_1-MATNR.
    LV_6 = LS_VBAP-ARKTX.
    LV_7 = LS_VBAP-MATKL.
    LV_8 = LS_ZPPT0064_1-ZATPDFRQ.                          "S4DK905875
    LV_9 = LS_ZPPT0064_1-ZNBJHQ.                            "S4DK905875
    LV_10 = LS_ZPPT0064_1-STATUS.
    LV_11 = LS_ZPPT0064_1-MESSAGE.
    CLEAR LS_VBAP.

    CONCATENATE LV_STRING
    LV_1  GC_TAB"
    LV_2  GC_TAB"
    LV_3  GC_TAB"
    LV_4  GC_TAB"
    LV_5  GC_TAB"
    LV_6  GC_TAB"
    LV_7  GC_TAB"
    LV_8  GC_TAB"
    LV_9  GC_TAB"
    LV_10  GC_TAB"
    LV_11  GC_CRLF"
    INTO LV_STRING.
    CLEAR:LS_ZPPT0064_1.
  ENDLOOP.

* --------------------------------------------------------------
* convert the text string into UTF-16LE binary data including
* byte-order-mark. Mircosoft Excel prefers these settings
* all this is done by new class cl_bcs_convert (see note 1151257)

  TRY.
      CL_BCS_CONVERT=>STRING_TO_SOLIX(
      EXPORTING
        IV_STRING   = LV_STRING
        IV_CODEPAGE = '4103'  "suitable for MS Excel, leave empty
        IV_ADD_BOM  = 'X'     "for other doc types
      IMPORTING
        ET_SOLIX  = BINARY_CONTENT
        EV_SIZE   = SIZE ).
    CATCH CX_BCS.
      MESSAGE E445(SO).
  ENDTRY.

ENDFORM.

*S4DK905744-S
*&---------------------------------------------------------------------*
*&      Form  create_content_VAL
*&---------------------------------------------------------------------*
* Create Example Content
* 1) Write example text into a string
* 2) convert this string to solix_tab
*----------------------------------------------------------------------*
FORM CREATE_CONTENT_VAL.

  DATA LV_STRING TYPE STRING.

  CLEAR:LV_STRING.
  CONCATENATE LV_STRING
  '工厂'  GC_TAB
  '物料'  GC_TAB
  '物料描述'  GC_TAB
  '错误结果信息' GC_CRLF
  INTO LV_STRING.
  DATA:LV_1(30),
       LV_2(30),
       LV_3(30),
       LV_4(300) .


  DATA: BEGIN OF GS_AUFK,
          AUFNR TYPE AUFK-AUFNR,
          OBJNR TYPE AUFK-OBJNR,
          WERKS TYPE AUFK-WERKS,
          MATNR TYPE AFPO-MATNR,
          STTXT TYPE CAUFVD-STTXT,
        END OF GS_AUFK.
  DATA: GT_AUFK LIKE TABLE OF GS_AUFK.

  SELECT
    AUFK~AUFNR,
    AUFK~OBJNR,
    AUFK~WERKS,
    AFPO~MATNR
    INTO  CORRESPONDING FIELDS OF TABLE @GT_AUFK
     FROM AUFK
    LEFT JOIN AFPO ON AUFK~AUFNR = AFPO~AUFNR
    WHERE NOT EXISTS ( SELECT STAT FROM JEST WHERE OBJNR = AUFK~OBJNR AND ( STAT IN ( 'I0012', 'I0074' )  AND INACT = '' ) ).
  DELETE ADJACENT DUPLICATES FROM GT_AUFK COMPARING WERKS MATNR.

  IF GT_AUFK IS NOT INITIAL.
*   物料描述
    SELECT *
      INTO TABLE @DATA(LT_MAKT)
      FROM MAKT
      FOR ALL ENTRIES IN @GT_AUFK
      WHERE MATNR = @GT_AUFK-MATNR
      AND SPRAS = @SY-LANGU.
*   价格
    SELECT MATNR,
      BWKEY,
      STPRS
      INTO TABLE @DATA(LT_MBEW)
      FROM MBEW
      FOR ALL ENTRIES IN @GT_AUFK
      WHERE BWKEY = @GT_AUFK-WERKS
      AND MATNR = @GT_AUFK-MATNR.
    DELETE LT_MBEW WHERE STPRS <> 0.
  ENDIF.

*  LOOP AT GT_AUFK ASSIGNING FIELD-SYMBOL(<LS_AUFK>).
*    "设置流程订单状态
*    CALL FUNCTION 'STATUS_TEXT_EDIT'
*      EXPORTING
*        OBJNR            = <LS_AUFK>-OBJNR
*        SPRAS            = SY-LANGU
*      IMPORTING
*        LINE             = <LS_AUFK>-STTXT
*      EXCEPTIONS
*        OBJECT_NOT_FOUND = 1
*        OTHERS           = 2.
*  ENDLOOP.

  LOOP AT LT_MBEW INTO DATA(LS_MBEW).
    CLEAR: LV_1,LV_2,LV_3,LV_4.
    LV_1 = LS_MBEW-BWKEY.
    LV_2 = LS_MBEW-MATNR.
    READ TABLE LT_MAKT INTO DATA(LS_MAKT) WITH KEY MATNR = LS_MBEW-MATNR.
    LV_3 = LS_MAKT-MAKTX.
    LV_4 = '未发布标准价'.


    CONCATENATE LV_STRING
    LV_1  GC_TAB"
    LV_2  GC_TAB"
    LV_3  GC_TAB"
    LV_4  GC_CRLF"
    INTO LV_STRING.
    CLEAR:LS_MBEW,LS_MAKT.
  ENDLOOP.

* --------------------------------------------------------------
* convert the text string into UTF-16LE binary data including
* byte-order-mark. Mircosoft Excel prefers these settings
* all this is done by new class cl_bcs_convert (see note 1151257)

  TRY.
      CL_BCS_CONVERT=>STRING_TO_SOLIX(
      EXPORTING
        IV_STRING   = LV_STRING
        IV_CODEPAGE = '4103'  "suitable for MS Excel, leave empty
        IV_ADD_BOM  = 'X'     "for other doc types
      IMPORTING
        ET_SOLIX  = BINARY_CONTENT
        EV_SIZE   = SIZE ).
    CATCH CX_BCS.
      MESSAGE E445(SO).
  ENDTRY.

ENDFORM.
*S4DK905744-E