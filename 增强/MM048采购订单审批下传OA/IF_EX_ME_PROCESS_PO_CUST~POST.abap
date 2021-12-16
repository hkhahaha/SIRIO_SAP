METHOD IF_EX_ME_PROCESS_PO_CUST~POST.

    *    MM022-委外采购订单下传WMS
    *    CALL FUNCTION 'ZMM_OUT_PURCHASE_SAP2WMS_FUN' STARTING NEW TASK 'ZMM_OUT_PURCHASE_SAP2WMS_FUN'
    *      EXPORTING
    *        im_ebeln  = im_ebeln.
    *    DATA ls_header TYPE mepoheader.
    *    ls_header = im_header->get_data( ).
    *采购订单保存的时候更新APS计划
        CALL FUNCTION 'Z_MM_CHANGE_APS_PLAN'
          EXPORTING
            IM_EBELN  = IM_EBELN
            IM_HEADER = IM_HEADER
            IV_TYPE   = '1'.
    
    *    CALL FUNCTION 'Z_MM_SAP2WMS_OUT_FUN' " MM022委外采购订单下传WMS
    *      EXPORTING
    *        im_header = im_header.
    **      IMPORTING
    **        ch_failed = ch_failed.
    
    ***************BEGIN  YANGHUIBO  20200526  增加 下发到OA
    
        DATA(LV_TASKNAME) = SY-DATUM && SY-UZEIT.
        DATA T_PRCD TYPE TABLE OF PRCD_ELEMENTS .
        DATA LS_HEAD  TYPE MEPOHEADER.
    
    **抬头信息
    *    LS_HEAD = IM_HEADER->GET_DATA( ).
    *    SELECT *
    *      INTO TABLE T_PRCD
    *      FROM PRCD_ELEMENTS
    *      WHERE KNUMV = LS_HEAD-KNUMV .
    
    
        DATA: LR_ITEMS TYPE PURCHASE_ORDER_ITEMS,
              LR_ITEM  TYPE PURCHASE_ORDER_ITEM,
              LS_ITEM  TYPE MEPOITEM,
              LT_ITEM  TYPE TABLE OF MEPOITEM.
    
    *    LR_ITEMS = IM_HEADER->GET_ITEMS( ).
    *
    **行项目信息
    *    LOOP AT LR_ITEMS INTO LR_ITEM.
    *      LS_ITEM = LR_ITEM-ITEM->GET_DATA( ).
    **      CHECK LS_ITEM-LOEKZ = '' .
    *      APPEND LS_ITEM TO LT_ITEM.
    *    ENDLOOP.
    
        DATA : LS_A017 TYPE A017 .
        DATA : LT_A017 TYPE TABLE OF A017 .
        DATA : LT_KONP TYPE TABLE OF KONP .
        DATA : LS_EKKO TYPE EKKO .
    
        SELECT *
         INTO CORRESPONDING FIELDS OF TABLE LT_ITEM
         FROM EKPO
         WHERE EBELN = IM_EBELN .
    
        SELECT SINGLE *
          INTO LS_EKKO
          FROM EKKO
          WHERE EBELN = IM_EBELN .
        IF SY-SUBRC = 0 .
    
          SELECT  *
            INTO TABLE LT_A017
            FROM A017
            FOR ALL ENTRIES IN LT_ITEM
            WHERE  KAPPL = 'M'
               AND KSCHL = 'ZPB0'
              AND LIFNR = LS_EKKO-LIFNR
              AND MATNR = LT_ITEM-MATNR
              AND EKORG = LS_EKKO-EKORG
              AND WERKS = LT_ITEM-WERKS
               AND ESOKZ = '2'
              AND DATBI >= LS_EKKO-BEDAT
              AND DATAB <= LS_EKKO-BEDAT .
          IF SY-SUBRC = 0 .
    
            SELECT *
              INTO TABLE LT_KONP
              FROM KONP
              FOR ALL ENTRIES IN LT_A017
              WHERE KNUMH = LT_A017-KNUMH
                AND KAPPL = 'M'
               AND KSCHL = 'ZPB0' .
    
          ENDIF .
    
        ENDIF .
    
        CALL FUNCTION 'ZMM_SEND_PO_TO_OA' STARTING NEW TASK LV_TASKNAME
          EXPORTING
            EBELN  = IM_EBELN
          TABLES
            T_PRCD = T_PRCD
            T_ITEM = LT_ITEM
            T_KONP = LT_KONP
            T_A017 = LT_A017 .
    
    *    DATA:LW_ZMT_SAP2OA_ISSUE TYPE ZMT_SAP2OA_ISSUE.
    *    DATA:LW_ZDT_SAP2OA_ISSUE TYPE ZDT_SAP2OA_ISSUE_SUB.
    *    DATA:LT_ZDT_SAP2OA_ISSUE TYPE TABLE OF ZDT_SAP2OA_ISSUE_SUB.
    *    DATA:LO_CLIENTPROXY       TYPE REF TO ZCO_SI_SAP2OA_ISSUE_OUT,
    *         LO_SYSTEM_FAULT      TYPE REF TO CX_AI_SYSTEM_FAULT,
    *         LO_ACK               TYPE REF TO IF_WS_ACKNOWLEDGMENT,
    *         LO_ACK_STATUS_SIMPLE TYPE PRX_ACK_STATUS,
    *         LO_ACK_STATUS_DETAIL TYPE PRX_ACK_STATUS_DETAIL_TABLE,
    *         LO_ACK_REQUEST       TYPE PRX_ACK_REQUEST_DETAILS,
    *         LO_ASYNC_MESSAGING   TYPE REF TO IF_WSPROTOCOL_ASYNC_MESSAGING,
    *         LO_MSG_ID_PROTOCOL   TYPE REF TO IF_WSPROTOCOL_MESSAGE_ID,
    *         LV_MSG_ID            TYPE SXMSGUID,
    *         LV_ERROR             TYPE C,
    *         LV_MSGTY             TYPE SY-MSGTY,
    *         LV_MSG               TYPE STRING.
    *
    *    DATA:LV_NAME1 TYPE LFA1-NAME1,
    *         LV_NAME2 TYPE LFA1-NAME2,
    *         LV_NAME3 TYPE LFA1-NAME3,
    *         LV_NAME4 TYPE LFA1-NAME4.
    *
    *    TYPES:BEGIN OF LW_MAKT,
    *            MATNR TYPE MAKT-MATNR,
    *            MAKTX TYPE MAKT-MAKTX,
    *          END OF LW_MAKT.
    *
    *    TYPES:BEGIN OF LW_EKET,
    *            EBELN TYPE EKET-EBELN,
    *            EBELP TYPE EKET-EBELP,
    *            EINDT TYPE EKET-EINDT,
    *            CHARG TYPE EKET-CHARG,
    *          END OF LW_EKET.
    *
    *    TYPES:BEGIN OF LW_NAME_ORG1,
    *            MATNR     TYPE MARA-MATNR,
    *            MFRNR     TYPE MARA-MFRNR,
    *            NAME_ORG1 TYPE BUT000-NAME_ORG1,
    *          END OF LW_NAME_ORG1.
    *
    *    DATA:LT_FLINES TYPE TABLE OF TLINE.
    *    DATA:LT_MAKT TYPE TABLE OF LW_MAKT.
    *    DATA:LT_EKET TYPE TABLE OF LW_EKET.
    *    DATA:LT_NAME_ORG1 TYPE TABLE OF LW_NAME_ORG1.
    *    DATA:LV_TDNAME TYPE STXL-TDNAME.
    *
    *
    **&---------------------------------------------------------------------*
    **& DEFINE CONSTANTS
    **&---------------------------------------------------------------------*
    *    CONSTANTS: GC_MSGTY_SUCCESS      TYPE C VALUE 'S',
    *               GC_MSGTY_ERROR        TYPE C VALUE 'E',
    *               GC_ZDWXZ_GONGSI       TYPE C VALUE '1',
    *               GC_OTYPE_ORGANIZATION TYPE TEXT02 VALUE 'O',
    *               GC_INIF_ID(6)         TYPE C VALUE 'MM048', "接口编号
    *               GC_SRC_SYSTEM(3)      TYPE C VALUE 'SAP',    "源系统
    *               GC_DEST_SYSTEM(3)     TYPE C VALUE 'OA'.    "目标系统
    *
    *
    *    CLEAR:LW_ZMT_SAP2OA_ISSUE,LW_ZDT_SAP2OA_ISSUE,LV_NAME1,LV_NAME2,
    *          LV_NAME3,LV_NAME4.
    *    REFRESH:LT_ZDT_SAP2OA_ISSUE,LT_MAKT,LT_EKET,LT_NAME_ORG1.
    *
    *
    *
    *    DATA: LR_ITEMS TYPE PURCHASE_ORDER_ITEMS,
    *          LR_ITEM  TYPE PURCHASE_ORDER_ITEM,
    *          LS_ITEM  TYPE MEPOITEM,
    *          LT_ITEM  TYPE TABLE OF MEPOITEM,
    *          LS_HEAD  TYPE MEPOHEADER.
    *
    **&---------------------------------------------------------------------*
    ** 1实例化proxy
    **&---------------------------------------------------------------------*
    *    TRY.
    *        CREATE OBJECT LO_CLIENTPROXY.
    *        LO_ASYNC_MESSAGING ?= LO_CLIENTPROXY->GET_PROTOCOL( IF_WSPROTOCOL=>ASYNC_MESSAGING ).
    **     ask for transport acknowledgment
    *        CLEAR LO_ACK_REQUEST.
    *        LO_ACK_REQUEST = IF_WSPROTOCOL_ASYNC_MESSAGING=>CO_TRANSPORT_ACKNOWLEDGMENT.
    *        LO_ASYNC_MESSAGING->SET_ACKNOWLEDGMENT_REQUESTED( LO_ACK_REQUEST ).
    *      CATCH CX_AI_SYSTEM_FAULT INTO LO_SYSTEM_FAULT.
    *        LV_MSG   = LO_SYSTEM_FAULT->GET_TEXT( ).
    *        LV_MSGTY = GC_MSGTY_ERROR.
    *        LV_ERROR = ABAP_TRUE.
    *    ENDTRY.
    *
    **抬头信息
    *    LS_HEAD = IM_HEADER->GET_DATA( ).
    *
    *    LR_ITEMS = IM_HEADER->GET_ITEMS( ).
    *
    **行项目信息
    *    REFRESH:LT_ITEM.
    *    LOOP AT LR_ITEMS INTO LR_ITEM.
    *      LS_ITEM = LR_ITEM-ITEM->GET_DATA( ).
    *      APPEND LS_ITEM TO LT_ITEM.
    *    ENDLOOP.
    *
    ************检查是否下发
    *    SELECT SINGLE * FROM ZMMT0010 INTO @DATA(LS_ZMMT0010)
    *      WHERE BUKRS EQ @LS_HEAD-BUKRS
    *      AND   EKGRP EQ @LS_HEAD-EKGRP
    *      AND   SENDS NE @SPACE.
    *
    *    CHECK SY-SUBRC EQ 0.
    *
    *    CHECK LS_HEAD-MEMORY IS INITIAL.
    *
    *
    *************抬头
    *    MOVE-CORRESPONDING  LS_HEAD TO LW_ZMT_SAP2OA_ISSUE-MT_SAP2OA_ISSUE.
    *
    *    SELECT SINGLE EKOTX FROM T024E
    *      INTO LW_ZMT_SAP2OA_ISSUE-MT_SAP2OA_ISSUE-EKOTX
    *      WHERE EKORG EQ LW_ZMT_SAP2OA_ISSUE-MT_SAP2OA_ISSUE-EKORG.
    *
    *    SELECT SINGLE EKNAM FROM T024
    *      INTO LW_ZMT_SAP2OA_ISSUE-MT_SAP2OA_ISSUE-EKNAM
    *      WHERE EKGRP EQ LW_ZMT_SAP2OA_ISSUE-MT_SAP2OA_ISSUE-EKGRP.
    *
    *    SELECT SINGLE BUTXT FROM T001
    *      INTO LW_ZMT_SAP2OA_ISSUE-MT_SAP2OA_ISSUE-BUTXT
    *      WHERE BUKRS EQ LW_ZMT_SAP2OA_ISSUE-MT_SAP2OA_ISSUE-BUKRS.
    *
    *    SELECT SINGLE NAME1 NAME2 NAME3 NAME4
    *      INTO ( LV_NAME1,LV_NAME2,LV_NAME3,LV_NAME4 )
    *      FROM LFA1
    *      WHERE LIFNR EQ LW_ZMT_SAP2OA_ISSUE-MT_SAP2OA_ISSUE-LIFNR.
    *
    *    LW_ZMT_SAP2OA_ISSUE-MT_SAP2OA_ISSUE-LIFNR_NAME = LV_NAME1 && LV_NAME2 &&
    *                                                     LV_NAME3 && LV_NAME4.
    *
    *    SELECT SINGLE BATXT FROM T161T
    *      INTO LW_ZMT_SAP2OA_ISSUE-MT_SAP2OA_ISSUE-BATXT
    *      WHERE BSART EQ LW_ZMT_SAP2OA_ISSUE-MT_SAP2OA_ISSUE-BSART.
    **************行项目
    **    MOVE-CORRESPONDING IM_EKPO[] TO LW_ZMT_SAP2OA_ISSUE-MT_SAP2OA_ISSUE-LIST_OF_OTHERS[].
    *    IF LT_ITEM[] IS NOT INITIAL.
    ********物料描述
    *      DATA(LT_EKPO) = LT_ITEM[].
    **      SORT LT_EKPO BY MATNR.
    **      DELETE ADJACENT DUPLICATES FROM LT_EKPO COMPARING MATNR.
    **      SELECT MATNR
    **             MAKTX
    **        FROM MAKT INTO TABLE LT_MAKT
    **      FOR ALL ENTRIES IN LT_EKPO
    **      WHERE ( MATNR EQ LT_EKPO-MATNR
    **      OR      MATNR EQ LT_EKPO-EMATN )
    **      AND   SPRAS EQ SY-LANGU.
    *
    ************计划行
    **      SELECT EBELN
    **             EBELP
    **             EINDT
    **             CHARG
    **       FROM EKET INTO TABLE LT_EKET
    **      FOR ALL ENTRIES IN LT_EKPO
    **      WHERE EBELN EQ LT_EKPO-EBELN
    **      AND   EBELP EQ LT_EKPO-EBELP.
    *
    *      DELETE ADJACENT DUPLICATES FROM LT_EKPO COMPARING INFNR MATNR.
    *      SELECT * FROM EINA INTO TABLE @DATA(LT_EINA)
    *        FOR ALL ENTRIES IN @LT_EKPO
    *        WHERE LIFNR EQ @LS_HEAD-LIFNR
    *        AND   MATNR EQ @LT_EKPO-MATNR.
    *      IF LT_EINA[] IS NOT INITIAL.
    *        SELECT * FROM EINE INTO TABLE @DATA(LT_EINE)
    *          FOR ALL ENTRIES IN @LT_EINA
    *         WHERE INFNR EQ @LT_EINA-INFNR
    *         AND   EKORG EQ @LS_HEAD-EKORG.
    *      ENDIF.
    *
    *      LT_EKPO[] = LT_ITEM[].
    *      SORT LT_EKPO BY MFRNR.
    *      DELETE ADJACENT DUPLICATES FROM LT_EKPO COMPARING MFRNR.
    *      SELECT MARA~MATNR
    *             MARA~MFRNR
    *             BUT000~NAME_ORG1
    *      INTO TABLE LT_NAME_ORG1
    *      FROM MARA AS MARA INNER JOIN BUT000 AS BUT000
    *        ON MARA~MFRNR EQ BUT000~PARTNER
    *       FOR ALL ENTRIES IN LT_EKPO
    *      WHERE MFRNR EQ LT_EKPO-MFRNR.
    *
    *    ENDIF.
    *
    *    LOOP AT LT_ITEM
    *      ASSIGNING FIELD-SYMBOL(<FS_IM_EKPO>).
    *      MOVE-CORRESPONDING <FS_IM_EKPO> TO LW_ZDT_SAP2OA_ISSUE.
    *      DATA(LV_MATNR) = <FS_IM_EKPO>-MATNR.
    *
    *      SELECT SINGLE GROES
    *          INTO  LW_ZDT_SAP2OA_ISSUE-SIZE
    *          FROM MARA
    *          WHERE MATNR = LV_MATNR .
    *      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
    *        EXPORTING
    *          INPUT        = LV_MATNR
    *        IMPORTING
    *          OUTPUT       = LV_MATNR
    *        EXCEPTIONS
    *          LENGTH_ERROR = 1
    *          OTHERS       = 2.
    *
    *      IF LV_MATNR+0(2) EQ '10' AND LW_ZMT_SAP2OA_ISSUE-MT_SAP2OA_ISSUE-BSART <> 'ZJS'.
    *        LV_MATNR = <FS_IM_EKPO>-EMATN.
    *        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
    *          EXPORTING
    *            INPUT        = LV_MATNR
    *          IMPORTING
    *            OUTPUT       = LV_MATNR
    *          EXCEPTIONS
    *            LENGTH_ERROR = 1
    *            OTHERS       = 2.
    *      ENDIF.
    *
    *      LW_ZDT_SAP2OA_ISSUE-MATNR = LV_MATNR.
    *
    **      READ TABLE LT_MAKT ASSIGNING FIELD-SYMBOL(<FS_MAKT>)
    **       WITH KEY MATNR = LW_ZDT_SAP2OA_ISSUE-MATNR.
    **      IF SY-SUBRC EQ 0.
    **        LW_ZDT_SAP2OA_ISSUE-MAKTX = <FS_MAKT>-MAKTX.
    **      ENDIF.
    *      LW_ZDT_SAP2OA_ISSUE-MAKTX = <FS_IM_EKPO>-TXZ01.
    *********含税单价 /含税总额
    *      IF <FS_IM_EKPO>-PSTYP NE '2'.
    *        LW_ZDT_SAP2OA_ISSUE-PRICE = <FS_IM_EKPO>-BRTWR / <FS_IM_EKPO>-MENGE.
    *        LW_ZDT_SAP2OA_ISSUE-BRTWR = <FS_IM_EKPO>-BRTWR.
    *      ELSE.
    *        LOOP AT LT_EINA ASSIGNING FIELD-SYMBOL(<FS_EINA>)
    *          WHERE LIFNR = LS_HEAD-LIFNR
    *          AND   MATNR = <FS_IM_EKPO>-MATNR.
    *          READ TABLE LT_EINE ASSIGNING FIELD-SYMBOL(<FS_EINE>)
    *           WITH KEY INFNR = <FS_EINA>-INFNR
    *                    EKORG = LS_HEAD-EKORG
    *                    WERKS = <FS_IM_EKPO>-WERKS
    *                    ESOKZ = '2'.
    *          IF SY-SUBRC EQ 0.
    *            LW_ZDT_SAP2OA_ISSUE-PRICE = <FS_EINE>-NETPR / <FS_EINE>-PEINH.
    *          ENDIF.
    *        ENDLOOP.
    *        LW_ZDT_SAP2OA_ISSUE-BRTWR = LW_ZDT_SAP2OA_ISSUE-PRICE * <FS_IM_EKPO>-MENGE.
    *      ENDIF.
    *
    *      READ TABLE LT_EKET ASSIGNING FIELD-SYMBOL(<FS_EKET>)
    *       WITH KEY EBELN = <FS_IM_EKPO>-EBELN
    *                EBELP = <FS_IM_EKPO>-EBELP.
    *      IF SY-SUBRC EQ 0.
    *        LW_ZDT_SAP2OA_ISSUE-EINDT = <FS_EKET>-EINDT.
    *        LW_ZDT_SAP2OA_ISSUE-CHARG = <FS_EKET>-CHARG.
    *      ENDIF.
    *
    *      READ TABLE LT_NAME_ORG1 ASSIGNING FIELD-SYMBOL(<FS_NAME_ORG1>)
    *       WITH KEY MATNR = <FS_IM_EKPO>-EMATN.
    *      IF SY-SUBRC EQ 0.
    *        LW_ZDT_SAP2OA_ISSUE-MFRNR_NAME = <FS_NAME_ORG1>-NAME_ORG1.
    *      ENDIF.
    *
    ************文本
    *      REFRESH:LT_FLINES.
    *      CLEAR:LV_TDNAME.
    *      LV_TDNAME = <FS_IM_EKPO>-EBELN && <FS_IM_EKPO>-EBELP.
    *      SELECT SINGLE * FROM STXL INTO @DATA(LS_STXL)
    *      WHERE TDOBJECT = 'EKPO'
    *      AND   TDNAME = @LV_TDNAME
    *      AND   TDID  = 'F01'.
    *      IF SY-SUBRC EQ 0.
    *        CALL FUNCTION 'READ_TEXT'
    *          EXPORTING
    *            CLIENT                  = SY-MANDT
    *            ID                      = LS_STXL-TDID
    *            LANGUAGE                = SY-LANGU
    *            NAME                    = LS_STXL-TDNAME
    *            OBJECT                  = LS_STXL-TDOBJECT
    *          TABLES
    *            LINES                   = LT_FLINES
    *          EXCEPTIONS
    *            ID                      = 1
    *            LANGUAGE                = 2
    *            NAME                    = 3
    *            NOT_FOUND               = 4
    *            OBJECT                  = 5
    *            REFERENCE_CHECK         = 6
    *            WRONG_ACCESS_TO_ARCHIVE = 7
    *            OTHERS                  = 8.
    *        IF SY-SUBRC EQ 0.
    *          LOOP AT LT_FLINES ASSIGNING FIELD-SYMBOL(<FS_FLINES>).
    *            CONCATENATE LW_ZDT_SAP2OA_ISSUE-ITEMNOTE  <FS_FLINES>-TDLINE INTO LW_ZDT_SAP2OA_ISSUE-ITEMNOTE .
    *          ENDLOOP.
    *        ENDIF.
    *      ENDIF.
    *
    *      REFRESH:LT_FLINES.
    *      CLEAR:LS_STXL.
    *      SELECT SINGLE * FROM STXL INTO LS_STXL
    *      WHERE TDOBJECT = 'EKPO'
    *      AND   TDNAME = LV_TDNAME
    *      AND   TDID  = 'F03'.
    *      IF SY-SUBRC EQ 0.
    *        CALL FUNCTION 'READ_TEXT'
    *          EXPORTING
    *            CLIENT                  = SY-MANDT
    *            ID                      = LS_STXL-TDID
    *            LANGUAGE                = SY-LANGU
    *            NAME                    = LS_STXL-TDNAME
    *            OBJECT                  = LS_STXL-TDOBJECT
    *          TABLES
    *            LINES                   = LT_FLINES
    *          EXCEPTIONS
    *            ID                      = 1
    *            LANGUAGE                = 2
    *            NAME                    = 3
    *            NOT_FOUND               = 4
    *            OBJECT                  = 5
    *            REFERENCE_CHECK         = 6
    *            WRONG_ACCESS_TO_ARCHIVE = 7
    *            OTHERS                  = 8.
    *        IF SY-SUBRC EQ 0.
    *          LOOP AT LT_FLINES ASSIGNING <FS_FLINES>.
    *            CONCATENATE  LW_ZDT_SAP2OA_ISSUE-PONOTE  <FS_FLINES>-TDLINE INTO LW_ZDT_SAP2OA_ISSUE-PONOTE  .
    *          ENDLOOP.
    *        ENDIF.
    *        CLEAR:LS_STXL.
    *      ENDIF.
    *      IF <FS_IM_EKPO>-REPOS EQ SPACE.
    *        LW_ZDT_SAP2OA_ISSUE-UMSON = 'X'.
    *      ELSE.
    *        LW_ZDT_SAP2OA_ISSUE-UMSON = SPACE.
    *      ENDIF.
    *      IF LW_ZDT_SAP2OA_ISSUE-LOEKZ EQ 'L'.
    *        LW_ZDT_SAP2OA_ISSUE-LOEKZ = 'X'.
    *      ENDIF.
    *      APPEND LW_ZDT_SAP2OA_ISSUE TO LW_ZMT_SAP2OA_ISSUE-MT_SAP2OA_ISSUE-LIST_OF_OTHERS.
    *      CLEAR:LW_ZDT_SAP2OA_ISSUE.
    *    ENDLOOP.
    *
    *
    ************SEND
    *    TRY.
    *        CALL METHOD LO_CLIENTPROXY->SI_SAP2OA_ISSUE_OUT
    *          EXPORTING
    *            OUTPUT = LW_ZMT_SAP2OA_ISSUE.
    *      CATCH CX_AI_SYSTEM_FAULT INTO LO_SYSTEM_FAULT.
    *        LV_MSG   = LO_SYSTEM_FAULT->GET_TEXT( ).
    *        LV_ERROR = ABAP_TRUE.
    *        LV_MSGTY = GC_MSGTY_ERROR.
    *    ENDTRY.
    *
    *
    *    TRY .
    *        LO_MSG_ID_PROTOCOL ?=  LO_CLIENTPROXY->GET_PROTOCOL( IF_WSPROTOCOL=>MESSAGE_ID ).
    *        LV_MSG_ID = LO_MSG_ID_PROTOCOL->GET_MESSAGE_ID( ).
    *      CATCH CX_AI_SYSTEM_FAULT INTO LO_SYSTEM_FAULT.
    *        EXIT.
    *    ENDTRY.
    *    IF LV_MSGTY EQ 'E'.
    *      MESSAGE E000(ZMM) WITH  LV_MSG.
    *    ELSE.
    *      DATA(LV_TASKNAME) = SY-DATUM && SY-UZEIT.
    *      IF LS_HEAD-FRGKE EQ 'I'.
    *        CALL FUNCTION 'ZMM_PO_RELEASE1' STARTING NEW TASK LV_TASKNAME
    *          EXPORTING
    *            IV_EBELN    = LS_HEAD-EBELN
    *            IV_REL_CODE = 'Z1'.
    *      ENDIF.
    *    ENDIF.
    
    ***************BEGIN  YANGHUIBO  20200526  增加 下发到OA
      ENDMETHOD.