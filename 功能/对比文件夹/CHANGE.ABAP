*----------------------------------------------------------------------*
***INCLUDE LZMM_PRF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_WRITE_BAPI_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_DATA
*&---------------------------------------------------------------------*
FORM FRM_WRITE_BAPI_DATA  USING P_DATA TYPE ZMT_OA2SAP_PR_CREATE.
    DATA: LV_NUM LIKE BAPIMEREQITEMIMP-PREQ_ITEM.
    DATA:LV_MATNR TYPE MARA-MATNR.
    DATA:LV_DZEKKN TYPE BAPIMEREQACCOUNT-SERIAL_NO.
    DATA:LV_ASSET_NO TYPE BAPIMEREQACCOUNT-ASSET_NO.
    DATA : LV_MEINS TYPE MARA-MEINS .
  
    CLEAR:GS_BAPIMEREQHEADER,GS_BAPIMEREQHEADERX,GV_PR_NUM,GT_PR_ITEM,
          GS_PR_EXT,GS_PR_BAPIMEREQITEMTEXT,GS_PR_BAPIMEREQITEMTEXT.
  
    REFRESH:GT_PR_ITEMX,GT_PR_RETURN,GT_PR_EXT,GT_PR_BAPIMEREQITEMTEXT.
  
  ***********抬头
    GS_BAPIMEREQHEADER-PR_TYPE    = P_DATA-MT_OA2SAP_PR_CREATE-PR_TYPE.
    GS_BAPIMEREQHEADER-CTRL_IND   = ''.
    GS_BAPIMEREQHEADER-CREATE_IND = 'R'.
  
  ********更新标识
    GS_BAPIMEREQHEADERX-PR_TYPE    = C_X.
    GS_BAPIMEREQHEADERX-CTRL_IND   = C_X.
    GS_BAPIMEREQHEADERX-CREATE_IND = C_X.
  
  ********抬头文本
    APPEND VALUE #( TEXT_ID   = 'B01'
                    TEXT_LINE = P_DATA-MT_OA2SAP_PR_CREATE-PRHEADERTEXT
                    TEXT_FORM = '*'
                    ) TO GT_PR_PRHEADERTEXT.
  
    LOOP AT P_DATA-MT_OA2SAP_PR_CREATE-LIST_OF_OTHERS ASSIGNING FIELD-SYMBOL(<FS_LIST_OF_OTHERS>).
      LV_NUM = LV_NUM + 10.
  ********************行项目
      CLEAR:LV_MATNR.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = <FS_LIST_OF_OTHERS>-MATERIAL
        IMPORTING
          OUTPUT       = LV_MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.
      IF SY-SUBRC <> 0.
  * Implement suitable error handling here
      ENDIF.
  
  
      LV_MEINS = <FS_LIST_OF_OTHERS>-UNIT .
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          INPUT          = LV_MEINS
  *       LANGUAGE       = SY-LANGU
        IMPORTING
          OUTPUT         = LV_MEINS
        EXCEPTIONS
          UNIT_NOT_FOUND = 1
          OTHERS         = 2.
  
      APPEND VALUE #( PREQ_ITEM = LV_NUM
                      CTRL_IND = ''
                      MATERIAL = LV_MATNR
                      MATERIAL_EXTERNAL = LV_MATNR
                      SHORT_TEXT = <FS_LIST_OF_OTHERS>-SHORT_TEXT
                      PLANT = <FS_LIST_OF_OTHERS>-PLANT
                      STORE_LOC = <FS_LIST_OF_OTHERS>-STORE_LOC "仓库地点
                      QUANTITY = <FS_LIST_OF_OTHERS>-QUANTITY
                      UNIT =  LV_MEINS  "<FS_LIST_OF_OTHERS>-UNIT
                      BATCH = <FS_LIST_OF_OTHERS>-BATCH
                      PREQ_NAME = <FS_LIST_OF_OTHERS>-PREQ_NAME
                      TRACKINGNO = <FS_LIST_OF_OTHERS>-TRACKINGNO
                      PREQ_DATE = <FS_LIST_OF_OTHERS>-PREQ_DATE
                      DELIV_DATE = <FS_LIST_OF_OTHERS>-DELIV_DATE
                      ITEM_CAT = <FS_LIST_OF_OTHERS>-ITEM_CAT
                      ACCTASSCAT = <FS_LIST_OF_OTHERS>-ACCTASSCAT
                      PUR_GROUP = <FS_LIST_OF_OTHERS>-PUR_GROUP
                      MATL_GROUP = <FS_LIST_OF_OTHERS>-MATL_GROUP
                      REL_DATE = <FS_LIST_OF_OTHERS>-REL_DATE
                     ) TO GT_PR_ITEM.
  
  ****************行项目 更新标记
      APPEND VALUE #( PREQ_ITEM = LV_NUM
                      CTRL_IND = C_X
                      MATERIAL = C_X
                      MATERIAL_EXTERNAL = C_X
                      SHORT_TEXT = C_X
                      PLANT = C_X
                      STORE_LOC = C_X "仓库地点
                      QUANTITY = C_X
                      UNIT = C_X
                      BATCH = C_X
                      PREQ_NAME = C_X
                      TRACKINGNO = C_X
                      PREQ_DATE = C_X
                      DELIV_DATE = C_X
                      ITEM_CAT = C_X
                      ACCTASSCAT = C_X
                      PUR_GROUP = C_X
                      MATL_GROUP = C_X
                      REL_DATE = C_X
                     ) TO GT_PR_ITEMX.
  
  ****************行项目文本
      APPEND VALUE #( PREQ_ITEM = LV_NUM
                      TEXT_ID   = 'B01'
                      TEXT_LINE = <FS_LIST_OF_OTHERS>-TEXT_LINE
                      TEXT_FORM = '*'
                      ) TO GT_PR_BAPIMEREQITEMTEXT.
  
  *****************行项目文本更新标记
      APPEND VALUE #( PREQ_ITEM = LV_NUM
                      TEXT_ID   = C_X
                      TEXT_LINE = C_X
                      TEXT_FORM = C_X
                      ) TO GT_PR_BAPIMEREQITEMTEXTX.
  
  *****************项目科目分配
      CLEAR:LV_DZEKKN.
      LOOP AT  <FS_LIST_OF_OTHERS>-LIST_OF_POSNR ASSIGNING FIELD-SYMBOL(<FS_LIST_OF_POSNR>).
        CLEAR:LV_ASSET_NO.
        LV_DZEKKN = LV_DZEKKN + 1.
        IF <FS_LIST_OF_POSNR>-QUANTITY IS NOT INITIAL.
          DATA(LV_QUANTITY) = <FS_LIST_OF_POSNR>-QUANTITY.
        ELSE.
          LV_QUANTITY = <FS_LIST_OF_OTHERS>-QUANTITY.
        ENDIF.
        IF <FS_LIST_OF_POSNR>-GL_ACCOUNT IS NOT INITIAL.
          DATA(LV_ACCOUNT) = 'X'.
        ELSE.
          LV_ACCOUNT = SPACE.
        ENDIF.
        IF <FS_LIST_OF_POSNR>-ORDERID IS NOT INITIAL.
          DATA(LV_ORDERID) = 'X'.
        ELSE.
          LV_ORDERID = SPACE.
        ENDIF.
        IF <FS_LIST_OF_POSNR>-ASSET_NO IS NOT INITIAL.
          LV_ASSET_NO = <FS_LIST_OF_POSNR>-ASSET_NO.
          UNPACK LV_ASSET_NO TO LV_ASSET_NO.
          DATA(LV_ASSET) = 'X'.
        ELSE.
          LV_ASSET = SPACE.
        ENDIF.
  
        APPEND VALUE #( PREQ_ITEM = LV_NUM
  *                      SERIAL_NO   = <FS_LIST_OF_POSNR>-SERIAL_NO
                        SERIAL_NO   = LV_DZEKKN
                        QUANTITY = LV_QUANTITY
                        ASSET_NO = LV_ASSET_NO
                        COSTCENTER = <FS_LIST_OF_POSNR>-COSTCENTER
                        GL_ACCOUNT = <FS_LIST_OF_POSNR>-GL_ACCOUNT
                        ORDERID = <FS_LIST_OF_POSNR>-ORDERID
                        ) TO GT_PR_BAPIMEREQACCOUNT.
  
  ***********更新标记
        APPEND VALUE #( PREQ_ITEM = LV_NUM
                        SERIAL_NO   = LV_DZEKKN
                        QUANTITY = C_X
                        ASSET_NO = LV_ASSET
                        COSTCENTER = C_X
                        GL_ACCOUNT = LV_ACCOUNT
                        ORDERID = LV_ORDERID
                        ) TO GT_PR_BAPIMEREQACCOUNTX.
  
      ENDLOOP.
    ENDLOOP.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_CALL_PR_BAPI
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM FRM_CALL_PR_BAPI USING P_DATA TYPE ZMT_OA2SAP_PR_CREATE.
    DATA:LW_RET_LIST TYPE ZDT_OA2SAP_PR_CREATE_RET_LIST.
    DATA:LV_MESSAEG TYPE STRING.
    DATA:LT_RETURN  TYPE TABLE OF BAPIRET2.
  
    CLEAR:LW_RET_LIST,LV_MESSAEG.
  
    REFRESH:LT_RETURN.
  
    IF  GT_PR_ITEM[] IS NOT INITIAL.
  
  *    GCL_LOG->DELETE_ALL_MESSAGES( ).
  
      CALL FUNCTION 'BAPI_PR_CREATE'
        EXPORTING
          PRHEADER     = GS_BAPIMEREQHEADER
          PRHEADERX    = GS_BAPIMEREQHEADERX
        IMPORTING
          NUMBER       = GV_PR_NUM
        TABLES
          RETURN       = GT_PR_RETURN
          PRITEM       = GT_PR_ITEM
          PRITEMX      = GT_PR_ITEMX
          PRITEMTEXT   = GT_PR_BAPIMEREQITEMTEXT
          PRHEADERTEXT = GT_PR_PRHEADERTEXT
          PRACCOUNT    = GT_PR_BAPIMEREQACCOUNT
          PRACCOUNTX   = GT_PR_BAPIMEREQACCOUNTX
          EXTENSIONIN  = GT_PR_EXT.
      CLEAR:LW_RET_LIST-RET_MSG,LV_MESSAEG.
  
  *    GCL_LOG->ADD_MSG_FROM_DATA( IT_MSG_TABLE = GT_PR_RETURN ).
      LOOP AT GT_PR_RETURN ASSIGNING FIELD-SYMBOL(<FS_RETURN>) WHERE TYPE CA 'AE'.
        EXIT.
      ENDLOOP.
      IF  SY-SUBRC NE 0.
        COMMIT WORK AND WAIT.
        LW_RET_LIST-RET_MSG = '创建成功'.
        LW_RET_LIST-PREQ_NO = GV_PR_NUM.
        LW_RET_LIST-RET_CODE = 'S'.
        APPEND LW_RET_LIST TO GT_RET_LIST.
        READ TABLE P_DATA-MT_OA2SAP_PR_CREATE-LIST_OF_OTHERS
         ASSIGNING FIELD-SYMBOL(<FS_LIST_OF_OTHERS>) INDEX 1.
        IF SY-SUBRC EQ 0.
          UPDATE EBAN SET ERNAM = <FS_LIST_OF_OTHERS>-CREATED_BY
            WHERE BANFN EQ GV_PR_NUM.
          IF SY-SUBRC EQ 0.
            COMMIT WORK AND WAIT.
          ENDIF.
        ENDIF.
  
  ************成功后审批
  *      IF P_DATA-MT_OA2SAP_PR_CREATE-PR_SIGN EQ 'A'.
  *        LOOP AT GT_PR_ITEM ASSIGNING FIELD-SYMBOL(<FS_PR_ITEM>).
        CALL FUNCTION 'BAPI_REQUISITION_RELEASE_GEN'
          EXPORTING
            NUMBER   = GV_PR_NUM
            REL_CODE = 'Z1'
  *         NO_COMMIT_WORK          = ' '
  *         IMPORTING
  *         REL_STATUS_NEW          =
  *         REL_INDICATOR_NEW       =
          TABLES
            RETURN   = LT_RETURN.
  
  *        CALL FUNCTION 'BAPI_REQUISITION_RELEASE_GEN'
  *          EXPORTING
  *            NUMBER   = GV_PR_NUM
  *            REL_CODE = 'Z1'
  *            ITEM     = <FS_PR_ITEM>-PREQ_ITEM
  **           USE_EXCEPTIONS = 'X'
  *          TABLES
  *            RETURN   = LT_RETURN.
        CLEAR:LW_RET_LIST.
        LOOP AT LT_RETURN ASSIGNING <FS_RETURN> WHERE TYPE CA 'AE'.
          MESSAGE ID <FS_RETURN>-ID TYPE <FS_RETURN>-TYPE NUMBER <FS_RETURN>-NUMBER
           WITH <FS_RETURN>-MESSAGE_V1 <FS_RETURN>-MESSAGE_V2 <FS_RETURN>-MESSAGE_V3
                <FS_RETURN>-MESSAGE_V4 INTO LV_MESSAEG.
          LW_RET_LIST-RET_MSG = LW_RET_LIST-RET_MSG && LV_MESSAEG.
        ENDLOOP.
  ************审批不成功
        IF SY-SUBRC EQ 0.
          LW_RET_LIST-RET_MSG = 'PR:' && GV_PR_NUM && LW_RET_LIST-RET_MSG.
          LW_RET_LIST-PREQ_NO = GV_PR_NUM.
          LW_RET_LIST-RET_CODE = 'E'.
          APPEND LW_RET_LIST TO GT_RET_LIST.
        ELSE.
          LW_RET_LIST-RET_MSG = 'PR:' && GV_PR_NUM && '审批成功'.
          LW_RET_LIST-PREQ_NO = GV_PR_NUM.
          LW_RET_LIST-RET_CODE = 'S'.
          APPEND LW_RET_LIST TO GT_RET_LIST.
        ENDIF.
  *        ENDLOOP.
  *      ENDIF.
      ELSE.
        LOOP AT GT_PR_RETURN INTO DATA(LS_RETURN) WHERE TYPE CA 'AE'.
          MESSAGE ID LS_RETURN-ID TYPE LS_RETURN-TYPE NUMBER LS_RETURN-NUMBER
           WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2 LS_RETURN-MESSAGE_V3
                LS_RETURN-MESSAGE_V4 INTO LV_MESSAEG.
          LW_RET_LIST-RET_MSG = LW_RET_LIST-RET_MSG && LV_MESSAEG.
        ENDLOOP.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        LW_RET_LIST-RET_CODE = 'E'.
        APPEND LW_RET_LIST TO GT_RET_LIST.
      ENDIF.
    ELSE.
      LW_RET_LIST-RET_MSG = '没有可创建的数据'.
      LW_RET_LIST-RET_CODE = 'E'.
      APPEND LW_RET_LIST TO GT_RET_LIST.
    ENDIF.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_CHECK_PROXY
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM FRM_CHECK_PROXY USING P_ERROE P_INTID.
    DATA: LV_INTF_ID(6) TYPE C.
  *  DATA:LW_RET_LIST TYPE ZDT_OA2SAP_PR_RET.
    "检查接口激活状态
    LV_INTF_ID = P_INTID.
  
    DATA(LV_RET) = ZCL_BC_PUBLIC=>INT_ACTIVED( LV_INTF_ID ).
  
    IF LV_RET = ABAP_FALSE.
  *    GW_OUTPUT1-MT_OA2SAP_PR_CHANGE_RET-RET_CODE = 'E'.
  *    GW_OUTPUT1-MT_OA2SAP_PR_CHANGE_RET-RET_MSG = '接口没有激活'.
      P_ERROE = 'X'.
    ELSE.
      P_ERROE = ''.
    ENDIF.
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_WRITE_BAPI_DATA_M
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> I_DATA
  *&---------------------------------------------------------------------*
  FORM FRM_WRITE_BAPI_DATA_M  USING P_DATA TYPE ZMT_OA2SAP_PR_CHANGE.
    DATA: LV_NUM LIKE BAPIMEREQITEMIMP-PREQ_ITEM.
    DATA:LV_MATNR TYPE MARA-MATNR.
    DATA:LV_DZEKKN TYPE BAPIMEREQACCOUNT-SERIAL_NO.
    DATA:LV_ASSET_NO TYPE BAPIMEREQACCOUNT-ASSET_NO.
    DATA : LV_SERIAL_NO TYPE BAPIMEREQACCOUNT-SERIAL_NO .
    DATA : LV_MEINS TYPE MARA-MEINS .
  
    CLEAR:GS_BAPIMEREQHEADER,GS_BAPIMEREQHEADERX,GV_PR_NUM,GT_PR_ITEM,
          GS_PR_EXT,GS_PR_BAPIMEREQITEMTEXT,GS_PR_BAPIMEREQITEMTEXT.
  
    REFRESH:GT_PR_ITEMX,GT_PR_RETURN,GT_PR_EXT,GT_PR_BAPIMEREQITEMTEXT.
  
  ************抬头
  *  GS_BAPIMEREQHEADER-PR_TYPE    = P_DATA-MT_OA2SAP_PR_CHANGE-PR_TYPE.
  *  GS_BAPIMEREQHEADER-PREQ_NO    = P_DATA-MT_OA2SAP_PR_CHANGE-PREQ_NO.
  *  GS_BAPIMEREQHEADER-CTRL_IND   = ''.
  *  GS_BAPIMEREQHEADER-CREATE_IND = 'R'.
  *
  *********更新标识
  *  GS_BAPIMEREQHEADERX-PR_TYPE    = C_X.
  *  GS_BAPIMEREQHEADERX-PR_TYPE    = C_X.
  *  GS_BAPIMEREQHEADERX-CTRL_IND   = C_X.
  *  GS_BAPIMEREQHEADERX-CREATE_IND = C_X.
  
  ********抬头文本
    APPEND VALUE #( TEXT_ID   = 'B01'
                    TEXT_LINE = P_DATA-MT_OA2SAP_PR_CHANGE-PRHEADERTEXT
                    TEXT_FORM = '*'
                    ) TO GT_PR_PRHEADERTEXT.
  
  ********************行项目
    LOOP AT P_DATA-MT_OA2SAP_PR_CHANGE-LIST_OF_OTHERS ASSIGNING FIELD-SYMBOL(<FS_LIST_OF_OTHERS>).
      LV_NUM = LV_NUM + 10.
      CLEAR:LV_MATNR.
      LV_MATNR = <FS_LIST_OF_OTHERS>-MATERIAL.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = LV_MATNR
        IMPORTING
          OUTPUT       = LV_MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.
      IF SY-SUBRC <> 0.
  * Implement suitable error handling here
      ENDIF.
  
      LV_MEINS = <FS_LIST_OF_OTHERS>-UNIT .
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          INPUT          = LV_MEINS
  *       LANGUAGE       = SY-LANGU
        IMPORTING
          OUTPUT         = LV_MEINS
        EXCEPTIONS
          UNIT_NOT_FOUND = 1
          OTHERS         = 2.
  
      APPEND VALUE #( PREQ_ITEM = <FS_LIST_OF_OTHERS>-PREQ_ITEM
                      CTRL_IND = ''
                      MATERIAL = LV_MATNR
                      MATERIAL_EXTERNAL = LV_MATNR
                      SHORT_TEXT = <FS_LIST_OF_OTHERS>-SHORT_TEXT
                      PLANT = <FS_LIST_OF_OTHERS>-PLANT
                      STORE_LOC = <FS_LIST_OF_OTHERS>-STORE_LOC "仓库地点
                      QUANTITY = <FS_LIST_OF_OTHERS>-QUANTITY
                      UNIT = LV_MEINS "<FS_LIST_OF_OTHERS>-UNIT
                      BATCH = <FS_LIST_OF_OTHERS>-BATCH
                      TRACKINGNO = <FS_LIST_OF_OTHERS>-TRACKINGNO
                      PREQ_DATE = <FS_LIST_OF_OTHERS>-PREQ_DATE
                      DELIV_DATE = <FS_LIST_OF_OTHERS>-DELIV_DATE
                      ITEM_CAT = <FS_LIST_OF_OTHERS>-ITEM_CAT
                      ACCTASSCAT = <FS_LIST_OF_OTHERS>-ACCTASSCAT
                      PUR_GROUP = <FS_LIST_OF_OTHERS>-PUR_GROUP
                      MATL_GROUP = <FS_LIST_OF_OTHERS>-MATL_GROUP
                      REL_DATE = <FS_LIST_OF_OTHERS>-REL_DATE
                      DELETE_IND = <FS_LIST_OF_OTHERS>-DELETE_IND
                     ) TO GT_PR_ITEM.
  
  ***********行项目更新标记
      APPEND VALUE #( PREQ_ITEM = <FS_LIST_OF_OTHERS>-PREQ_ITEM
                      CTRL_IND = C_X
                      MATERIAL = C_X
                      MATERIAL_EXTERNAL = C_X
                      SHORT_TEXT = C_X
                      PLANT = C_X
                      STORE_LOC = C_X "仓库地点
                      QUANTITY = C_X
                      UNIT = C_X
                      BATCH = C_X
                      TRACKINGNO = C_X
                      PREQ_DATE = C_X
                      DELIV_DATE = C_X
                      ITEM_CAT = C_X
                      ACCTASSCAT = C_X
                      PUR_GROUP = C_X
                      MATL_GROUP = C_X
                      REL_DATE = C_X
                      DELETE_IND = C_X
                     ) TO GT_PR_ITEMX.
  
  ****************行项目文本
      APPEND VALUE #( PREQ_ITEM = <FS_LIST_OF_OTHERS>-PREQ_ITEM
                      TEXT_ID   = 'B01'
                      TEXT_LINE = <FS_LIST_OF_OTHERS>-TEXT_LINE
                      TEXT_FORM = '*'
                      ) TO GT_PR_BAPIMEREQITEMTEXT.
  
  *****************行项目文本更新标记
      APPEND VALUE #( PREQ_ITEM = <FS_LIST_OF_OTHERS>-PREQ_ITEM
                      TEXT_ID   = C_X
                      TEXT_LINE = C_X
                      TEXT_FORM = C_X
                      ) TO GT_PR_BAPIMEREQITEMTEXTX.
  
  
  ************科目分配
      LOOP AT  <FS_LIST_OF_OTHERS>-LIST_OF_POSNR ASSIGNING FIELD-SYMBOL(<FS_LIST_OF_POSNR>)..
        IF <FS_LIST_OF_POSNR>-QUANTITY IS NOT INITIAL.
          DATA(LV_QUANTITY) = <FS_LIST_OF_POSNR>-QUANTITY.
        ELSE.
          LV_QUANTITY = <FS_LIST_OF_OTHERS>-QUANTITY.
        ENDIF.
        IF <FS_LIST_OF_POSNR>-ORDERID IS NOT INITIAL.
          DATA(LV_ORDERID) = 'X'.
        ELSE.
          LV_ORDERID = SPACE.
        ENDIF.
        IF <FS_LIST_OF_POSNR>-GL_ACCOUNT IS NOT INITIAL.
          DATA(LV_ACCOUNT) = 'X'.
        ELSE.
          LV_ACCOUNT = SPACE.
        ENDIF.
        IF <FS_LIST_OF_POSNR>-ASSET_NO IS NOT INITIAL.
          LV_ASSET_NO = <FS_LIST_OF_POSNR>-ASSET_NO.
          UNPACK LV_ASSET_NO TO LV_ASSET_NO.
          DATA(LV_ASSET) = 'X'.
        ELSE.
          LV_ASSET = SPACE.
        ENDIF.
  
        IF <FS_LIST_OF_POSNR>-SERIAL_NO = '' .
          LV_SERIAL_NO = '01' .
        ELSE .
          LV_SERIAL_NO = <FS_LIST_OF_POSNR>-SERIAL_NO .
        ENDIF .
  
        APPEND VALUE #( PREQ_ITEM = <FS_LIST_OF_OTHERS>-PREQ_ITEM
                        SERIAL_NO   = LV_SERIAL_NO "<FS_LIST_OF_POSNR>-SERIAL_NO
                        QUANTITY = LV_QUANTITY
                        ASSET_NO = LV_ASSET_NO
                        COSTCENTER = <FS_LIST_OF_POSNR>-COSTCENTER
                        GL_ACCOUNT = <FS_LIST_OF_POSNR>-GL_ACCOUNT
                        ORDERID = <FS_LIST_OF_POSNR>-ORDERID
                        ) TO GT_PR_BAPIMEREQACCOUNT.
  
  ***********科目分配更新标记
        APPEND VALUE #( PREQ_ITEM = <FS_LIST_OF_OTHERS>-PREQ_ITEM
                        SERIAL_NO   = LV_SERIAL_NO "<FS_LIST_OF_POSNR>-SERIAL_NO
                        QUANTITY = C_X
                        ASSET_NO = LV_ASSET
                        COSTCENTER = C_X
                        GL_ACCOUNT = LV_ACCOUNT
                        ORDERID = LV_ORDERID
                        ) TO GT_PR_BAPIMEREQACCOUNTX.
      ENDLOOP.
    ENDLOOP.
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_CALL_PR_BAPI
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM FRM_CALL_PR_BAPI_M USING P_DATA TYPE ZMT_OA2SAP_PR_CHANGE.
    DATA:LW_RET_LIST TYPE ZDT_OA2SAP_PR_CREATE_RET_LIST.
    DATA:LV_MESSAEG TYPE STRING.
    DATA:LT_RETURN  TYPE TABLE OF BAPIRET2.
    DATA:BEGIN OF LT_EBAN OCCURS 0,
           BANFN TYPE EBAN-BANFN,
           BNFPO TYPE EBAN-BNFPO,
           FRGST TYPE EBAN-FRGST, "批准策略
           FRGGR TYPE EBAN-FRGGR, "审批组
         END OF LT_EBAN.
    DATA:LV_PRE_FIELD TYPE STRING VALUE 'FRGC'.
    DATA:LV_FIELDNAME TYPE STRING.
    FIELD-SYMBOLS:<FS_FIELD> TYPE T16FS-FRGC1.
    DATA:LV_ERROE TYPE C.
    DATA:LV_PREQ_NO TYPE BAPI2009OB-PREQ_NO.
  
    DATA : LV_BANFN TYPE EBAN-BANFN .
    DATA : LV_BNFPO TYPE EBAN-BNFPO .
  
    CLEAR:LW_RET_LIST,LV_MESSAEG.
  
    REFRESH:LT_RETURN.
  
    IF  GT_PR_ITEM[] IS NOT INITIAL.
  
  *    GCL_LOG->DELETE_ALL_MESSAGES( ).
  
  ********************************取消审批
  ***********获取审批组策略
      CLEAR:LV_PREQ_NO.
      UNPACK P_DATA-MT_OA2SAP_PR_CHANGE-PREQ_NO TO LV_PREQ_NO.
      REFRESH:LT_EBAN.
      SELECT BANFN
             BNFPO
             FRGST  "批准策略
             FRGGR  "审批组
      INTO TABLE LT_EBAN
      FROM EBAN
      WHERE BANFN = LV_PREQ_NO "P_DATA-MT_OA2SAP_PR_CHANGE-PREQ_NO
        AND FRGZU NE ''. "空 未审核 X一级审核 XX二级审核
  
      READ TABLE LT_EBAN INDEX 1.
      IF SY-SUBRC = 0.
        SELECT SINGLE * INTO @DATA(LS_T16FS) FROM T16FS
          WHERE FRGGR = @LT_EBAN-FRGGR
          AND   FRGSX = @LT_EBAN-FRGST.
        IF SY-SUBRC = 0.
          "多级审批需逐级审批，反审批可以直接置为未审批状态
          CONCATENATE LV_PRE_FIELD '1' INTO LV_FIELDNAME.
          "通过指针获取批准代码
          ASSIGN COMPONENT LV_FIELDNAME OF STRUCTURE LS_T16FS TO <FS_FIELD>.
          REFRESH:LT_RETURN.
          CALL FUNCTION 'BAPI_REQUISITION_RESET_REL_GEN'
            EXPORTING
              NUMBER   = LV_PREQ_NO
              REL_CODE = <FS_FIELD>
            TABLES
              RETURN   = LT_RETURN.
          CLEAR:LW_RET_LIST.
          LOOP AT LT_RETURN ASSIGNING FIELD-SYMBOL(<FS_RETURN>) WHERE TYPE CA 'AE'.
            MESSAGE ID <FS_RETURN>-ID TYPE <FS_RETURN>-TYPE NUMBER <FS_RETURN>-NUMBER
             WITH <FS_RETURN>-MESSAGE_V1 <FS_RETURN>-MESSAGE_V2 <FS_RETURN>-MESSAGE_V3
                  <FS_RETURN>-MESSAGE_V4 INTO LV_MESSAEG.
            LW_RET_LIST-RET_MSG = LW_RET_LIST-RET_MSG && LV_MESSAEG.
          ENDLOOP.
  ************审批不成功
          IF SY-SUBRC EQ 0.
            ROLLBACK WORK.
            LW_RET_LIST-RET_MSG = 'PR:' && P_DATA-MT_OA2SAP_PR_CHANGE-PREQ_NO
                                  && LW_RET_LIST-RET_MSG.
            LW_RET_LIST-PREQ_NO = GV_PR_NUM.
            LW_RET_LIST-RET_CODE = 'E'.
            APPEND LW_RET_LIST TO GT_RET_LIST.
            LV_ERROE = 'X'.
          ELSE.
            COMMIT WORK AND WAIT.
            LW_RET_LIST-RET_MSG = 'PR:' && P_DATA-MT_OA2SAP_PR_CHANGE-PREQ_NO && '审批取消成功'.
            LW_RET_LIST-PREQ_NO = GV_PR_NUM.
            LW_RET_LIST-RET_CODE = 'S'.
            APPEND LW_RET_LIST TO GT_RET_LIST.
            LV_ERROE = SPACE.
          ENDIF.
        ENDIF.
      ENDIF.
  
  ********************************修改审批
      IF LV_ERROE IS INITIAL.
  
        DO  3000 TIMES.
  
          SELECT COUNT(*)
          FROM EBAN
          WHERE BANFN = LV_PREQ_NO
          AND FRGZU = ''. "空 未审核 X一级审核 XX二级审核
          IF SY-SUBRC = 0 .
            EXIT .
          ENDIF .
  
        ENDDO.
  
        CALL FUNCTION 'BAPI_PR_CHANGE'
          EXPORTING
            NUMBER       = LV_PREQ_NO
            PRHEADER     = GS_BAPIMEREQHEADER
            PRHEADERX    = GS_BAPIMEREQHEADERX
  *        IMPORTING
  *         NUMBER       = GV_PR_NUM
          TABLES
            RETURN       = GT_PR_RETURN
            PRITEM       = GT_PR_ITEM
            PRITEMX      = GT_PR_ITEMX
            PRITEMTEXT   = GT_PR_BAPIMEREQITEMTEXT
  *         PRITEMTEXTX  = GT_PR_BAPIMEREQITEMTEXTX
            PRHEADERTEXT = GT_PR_PRHEADERTEXT
            PRACCOUNT    = GT_PR_BAPIMEREQACCOUNT
            PRACCOUNTX   = GT_PR_BAPIMEREQACCOUNTX
            EXTENSIONIN  = GT_PR_EXT.
        CLEAR:LW_RET_LIST-RET_MSG,LV_MESSAEG.
  
  *      GCL_LOG->ADD_MSG_FROM_DATA( IT_MSG_TABLE = GT_PR_RETURN ).
        LOOP AT GT_PR_RETURN ASSIGNING <FS_RETURN> WHERE TYPE CA 'AE'.
          MESSAGE ID <FS_RETURN>-ID TYPE <FS_RETURN>-TYPE NUMBER <FS_RETURN>-NUMBER
           WITH <FS_RETURN>-MESSAGE_V1 <FS_RETURN>-MESSAGE_V2 <FS_RETURN>-MESSAGE_V3
                <FS_RETURN>-MESSAGE_V4 INTO LV_MESSAEG.
          LW_RET_LIST-RET_MSG = LW_RET_LIST-RET_MSG && LV_MESSAEG.
        ENDLOOP.
        IF SY-SUBRC NE 0.
  
  
  
          COMMIT WORK AND WAIT.
          LW_RET_LIST-RET_MSG = '修改成功'.
          LW_RET_LIST-PREQ_NO = LV_PREQ_NO.
          LW_RET_LIST-RET_CODE = 'S'.
          APPEND LW_RET_LIST TO GT_RET_LIST.
  
          LOOP AT P_DATA-MT_OA2SAP_PR_CHANGE-LIST_OF_OTHERS
         ASSIGNING FIELD-SYMBOL(<FS_LIST_OF_OTHERS>) .
  
            LV_BANFN = LV_PREQ_NO .
            LV_BNFPO = <FS_LIST_OF_OTHERS>-PREQ_ITEM .
  
            READ TABLE LT_EBAN WITH KEY BANFN = LV_BANFN
                                        BNFPO = LV_BNFPO .
            IF SY-SUBRC <> 0 .
              UPDATE EBAN SET ERNAM = <FS_LIST_OF_OTHERS>-CREATED_BY
                WHERE BANFN EQ LV_BANFN
                  AND BNFPO EQ LV_BNFPO.
            ENDIF .
  
          ENDLOOP .
          COMMIT WORK AND WAIT .
  
  ********************************成功后审批
  *        IF P_DATA-MT_OA2SAP_PR_CHANGE-PR_SIGN EQ 'C'.
          REFRESH:LT_RETURN.
  *          LOOP AT GT_PR_ITEM ASSIGNING FIELD-SYMBOL(<FS_PR_ITEM>).
          CALL FUNCTION 'BAPI_REQUISITION_RELEASE_GEN'
            EXPORTING
              NUMBER   = LV_PREQ_NO
              REL_CODE = 'Z1'
  *           ITEM     = <FS_PR_ITEM>-PREQ_ITEM
  *           USE_EXCEPTIONS = 'X'
            TABLES
              RETURN   = LT_RETURN.
          CLEAR:LW_RET_LIST.
          LOOP AT LT_RETURN ASSIGNING <FS_RETURN> WHERE TYPE CA 'AE'.
            MESSAGE ID <FS_RETURN>-ID TYPE <FS_RETURN>-TYPE NUMBER <FS_RETURN>-NUMBER
             WITH <FS_RETURN>-MESSAGE_V1 <FS_RETURN>-MESSAGE_V2 <FS_RETURN>-MESSAGE_V3
                  <FS_RETURN>-MESSAGE_V4 INTO LV_MESSAEG.
            LW_RET_LIST-RET_MSG = LW_RET_LIST-RET_MSG && LV_MESSAEG.
          ENDLOOP.
  ************审批不成功
          IF SY-SUBRC EQ 0.
            ROLLBACK WORK.
            LW_RET_LIST-RET_MSG = 'PR:' && GV_PR_NUM && LW_RET_LIST-RET_MSG.
            LW_RET_LIST-PREQ_NO = LV_PREQ_NO.
            LW_RET_LIST-RET_CODE = 'E'.
            APPEND LW_RET_LIST TO GT_RET_LIST.
          ELSE.
            COMMIT WORK AND WAIT.
            LW_RET_LIST-RET_MSG = 'PR:' && GV_PR_NUM && '审批成功'.
            LW_RET_LIST-PREQ_NO = LV_PREQ_NO.
            LW_RET_LIST-RET_CODE = 'S'.
            APPEND LW_RET_LIST TO GT_RET_LIST.
  
          ENDIF.
  
  
  
  
  *          ENDLOOP.
  *        ENDIF.
        ELSE.
  *        LOOP AT GT_PR_RETURN INTO DATA(LS_RETURN) WHERE TYPE CA 'AE'.
  *          MESSAGE ID LS_RETURN-ID TYPE LS_RETURN-TYPE NUMBER LS_RETURN-NUMBER
  *           WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2 LS_RETURN-MESSAGE_V3
  *                LS_RETURN-MESSAGE_V4 INTO LV_MESSAEG.
  *          LW_RET_LIST-RET_MSG = LW_RET_LIST-RET_MSG && LV_MESSAEG.
  *        ENDLOOP.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          LW_RET_LIST-RET_CODE = 'E'.
          APPEND LW_RET_LIST TO GT_RET_LIST.
        ENDIF.
      ENDIF.
    ELSE.
      LW_RET_LIST-RET_MSG = '没有可修改的数据'.
      LW_RET_LIST-RET_CODE = 'E'.
      APPEND LW_RET_LIST TO GT_RET_LIST.
    ENDIF.
  ENDFORM.