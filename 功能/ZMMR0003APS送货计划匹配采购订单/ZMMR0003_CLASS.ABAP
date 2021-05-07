*&---------------------------------------------------------------------*
*& 包含               ZMMR0003_CLASS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Event Definition
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

    DATA: GCL_EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.
    
    *----------------------------------------------------------------------*
    *       CLASS LCL_EVENT_RECEIVER DEFINITION
    *----------------------------------------------------------------------*
    *
    *----------------------------------------------------------------------*
    CLASS LCL_EVENT_RECEIVER DEFINITION.
    
      PUBLIC SECTION.
        METHODS:
    * ADD FUNCTION TO TOOLBAR
          ON_ADD_TOOLBAR_HEAD FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
            IMPORTING E_OBJECT E_INTERACTIVE,
    
          ON_ADD_TOOLBAR_PO FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
            IMPORTING E_OBJECT E_INTERACTIVE,
    
          ON_ADD_TOOLBAR_DETAIL FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
            IMPORTING E_OBJECT E_INTERACTIVE,
    
          ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
            IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM,
    
          ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
            IMPORTING E_MODIFIED ET_GOOD_CELLS,
    
          ON_DATA_CHANGED_DETAIL FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
            IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM,
    
          ON_DATA_CHANGED_FINISHED_D FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
            IMPORTING E_MODIFIED ET_GOOD_CELLS,
    
          ON_DOUBLE_CLICK_HEAD FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO,
    
          ON_DOUBLE_CLICK_PO FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO,
    
          ON_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
            IMPORTING E_UCOMM.
    
    ENDCLASS. "LCL_EVENT_RECEIVER DEFINITION
    
    *----------------------------------------------------------------------*
    *       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
    *----------------------------------------------------------------------*
    *     * ######
    *----------------------------------------------------------------------*
    CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
    
    * ADD FUNCTION TO HEAD TOOLBAR
      METHOD ON_ADD_TOOLBAR_HEAD.
    
    *删除不用的标准按钮
        PERFORM FRM_DELETE_UNUSED_BUTTON CHANGING E_OBJECT.
    
    *添加自定义按钮
        PERFORM FRM_ADD_TOOLBAR_BUTTON CHANGING E_OBJECT.
    
      ENDMETHOD.                    "ON_ADD_TOOLBAR
    
      METHOD ON_ADD_TOOLBAR_PO.
    
    *删除不用的标准按钮
        PERFORM FRM_DELETE_UNUSED_BUTTON CHANGING E_OBJECT.
    
    *添加自定义按钮
        PERFORM FRM_ADD_TOOLBAR_BUTTON_PO CHANGING E_OBJECT.
    
      ENDMETHOD.                    "ON_ADD_TOOLBAR
    
      METHOD ON_ADD_TOOLBAR_DETAIL.
    
    *删除不用的标准按钮
        PERFORM FRM_DELETE_UNUSED_BUTTON CHANGING E_OBJECT.
    
      ENDMETHOD.                    "ON_ADD_TOOLBAR
    
      METHOD ON_DATA_CHANGED.
        PERFORM FRM_DATA_CHANGE USING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM.
      ENDMETHOD.
    
      METHOD ON_DATA_CHANGED_FINISHED.
        PERFORM FRM_DATA_CHANGED_FINISHED USING E_MODIFIED ET_GOOD_CELLS.
      ENDMETHOD.
    
      METHOD ON_DATA_CHANGED_DETAIL.
        PERFORM FRM_DATA_CHANGE_DETAIL USING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM.
      ENDMETHOD.
    
      METHOD ON_DATA_CHANGED_FINISHED_D.
        PERFORM FRM_DATA_CHANGED_FINISHED_D USING E_MODIFIED ET_GOOD_CELLS.
      ENDMETHOD.
    
      METHOD ON_DOUBLE_CLICK_HEAD.
        PERFORM FRM_CALL_DETAIL_ALV USING E_ROW_ID E_COLUMN_ID ES_ROW_NO 'HEAD'.  "弹出分配详细ALV
      ENDMETHOD.
    
      METHOD ON_DOUBLE_CLICK_PO.
        PERFORM FRM_CALL_DETAIL_ALV USING E_ROW_ID E_COLUMN_ID ES_ROW_NO 'PO'.    "弹出分配详细ALV
      ENDMETHOD.
    
      METHOD ON_USER_COMMAND.
        PERFORM FRM_USER_COMMAND_HEAD USING E_UCOMM.
      ENDMETHOD.
    
    ENDCLASS. "LCL_EVENT_RECEIVER IMPLEMENTATION
    
    
    ****给标准函数ALV添加更新事件
    *----------------------------------------------------------------------*
    *       CLASS cl_event_receiver DEFINITION
    *----------------------------------------------------------------------*
    CLASS CL_EVENT_RECEIVER DEFINITION.
      PUBLIC SECTION.
        METHODS: HANDLE_DATA_CHANGED
                    FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
          IMPORTING ER_DATA_CHANGED
                    E_ONF4
                    E_ONF4_BEFORE
                    E_ONF4_AFTER
                    E_UCOMM,
          HANDLE_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
            IMPORTING E_MODIFIED ET_GOOD_CELLS.
    ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION
    *---------------------------------------------------------------------*
    *       CLASS CL_EVENT_RECEIVER IMPLEMENTATION
    *----------------------------------------------------------------------*
    *
    *----------------------------------------------------------------------*
    CLASS CL_EVENT_RECEIVER IMPLEMENTATION.
      METHOD HANDLE_DATA_CHANGED.
        PERFORM FRM_METHOD_DATA_CHANGED USING ER_DATA_CHANGED
                                              E_ONF4
                                              E_ONF4_BEFORE
                                              E_ONF4_AFTER
                                              E_UCOMM.
      ENDMETHOD.                    "handle_data_changed
    
      METHOD HANDLE_DATA_CHANGED_FINISHED.
    
      ENDMETHOD.
    
    ENDCLASS.                    "CL_EVENT_RECEIVER IMPLEMENTATION
    *&---------------------------------------------------------------------*
    
    *&---------------------------------------------------------------------*
    *&      Form  frm_method_data_changed
    *&---------------------------------------------------------------------*
    *       事件内容
    *----------------------------------------------------------------------*
    FORM FRM_METHOD_DATA_CHANGED  USING UR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                        U_ONF4
                                        U_ONF4_BEFORE
                                        U_ONF4_AFTER
                                        U_UCOMM TYPE SY-UCOMM.
    
      DATA: LT_FTAB         TYPE LVC_T_MODI,
            LS_FTAB         TYPE LVC_S_MODI,
            LV_FLAG,
            LV_UPDATE,
            LV_MESSAGE(100).
    
      DATA: LS_PDLV  LIKE GS_PDLV_DATA,
            LV_DATE  LIKE SY-DATUM,
            LV_DATE2 LIKE SY-DATUM.
    
      LT_FTAB = UR_DATA_CHANGED->MT_MOD_CELLS.
    
      LOOP AT LT_FTAB INTO LS_FTAB.
    
        READ TABLE GT_PDLV_DATA INTO DATA(LS_PDLV_DATA) INDEX LS_FTAB-ROW_ID.
    
        CASE LS_FTAB-FIELDNAME.
          WHEN 'ASSIGN_MENGE'.
    *      检测是否为纯数字型
            PERFORM FRM_CHECK_NUM USING LS_FTAB-VALUE CHANGING LS_PDLV-ASSIGN_MENGE LV_FLAG.
            CHECK LV_FLAG <> 'E'.
          WHEN 'PDLV_DATE'.
            LS_PDLV-PDLV_DATE = ZCL_CM_PUBLIC=>CONVERT_DATE_TO_INTERNAL( DATE_EXTERNAL = LS_FTAB-VALUE ).
            CHECK LS_PDLV-PDLV_DATE IS NOT INITIAL.
          WHEN 'PDLV_DATE_EXT'.
            LS_PDLV-PDLV_DATE_EXT = ZCL_CM_PUBLIC=>CONVERT_DATE_TO_INTERNAL( DATE_EXTERNAL = LS_FTAB-VALUE ).
            CHECK LS_PDLV-PDLV_DATE_EXT IS NOT INITIAL.
        ENDCASE.
    
        IF LS_FTAB-FIELDNAME = 'ASSIGN_MENGE'.
    
          PERFORM FRM_CHECK_PO_QTY USING LS_PDLV_DATA LS_PDLV-ASSIGN_MENGE LS_FTAB-ROW_ID CHANGING LV_MESSAGE.
    *设置数据修改时的报错信息
          IF LV_MESSAGE IS NOT INITIAL.
    
            PERFORM FRM_SET_DATA_CHANGE_MESSAGE USING UR_DATA_CHANGED '00' 'E' '001'
                                                        LV_MESSAGE
                                                        ''
                                                        ''
                                                        ''
                                                        'ASSIGN_MENGE'
                                                        LS_FTAB-ROW_ID
                                                        LS_FTAB-TABIX.
            CALL METHOD UR_DATA_CHANGED->MODIFY_CELL
              EXPORTING
                I_ROW_ID    = LS_FTAB-ROW_ID
                I_FIELDNAME = 'ASSIGN_MENGE'
                I_VALUE     = LS_PDLV_DATA-ASSIGN_MENGE.
    
          ELSE.
            LV_UPDATE = 'X'.
          ENDIF.
        ELSEIF LS_FTAB-FIELDNAME = 'PDLV_DATE' OR LS_FTAB-FIELDNAME = 'PDLV_DATE_EXT'.
    
          IF LS_FTAB-VALUE IS NOT INITIAL.
    
            LV_DATE = ZCL_CM_PUBLIC=>CONVERT_DATE_TO_INTERNAL( DATE_EXTERNAL = LS_FTAB-VALUE ).
    
            CHECK LV_DATE IS NOT INITIAL.
    
            PERFORM FRM_CHECK_PDLV_DATE USING LS_PDLV_DATA LV_DATE CHANGING LV_MESSAGE.
    
            IF LV_MESSAGE IS NOT INITIAL.
              PERFORM FRM_SET_DATA_CHANGE_MESSAGE USING UR_DATA_CHANGED '00' 'E' '001'
                                                          LV_MESSAGE
                                                          ''
                                                          ''
                                                          ''
                                                          LS_FTAB-FIELDNAME
                                                          LS_FTAB-ROW_ID
                                                          LS_FTAB-TABIX.
    
              IF LS_FTAB-FIELDNAME = 'PDLV_DATE' .
                LV_DATE2 = LS_PDLV_DATA-PDLV_DATE.
              ELSE.
                LV_DATE2 = LS_PDLV_DATA-PDLV_DATE_EXT.
              ENDIF.
    
              CALL METHOD UR_DATA_CHANGED->MODIFY_CELL
                EXPORTING
                  I_ROW_ID    = LS_FTAB-ROW_ID
                  I_FIELDNAME = LS_FTAB-FIELDNAME
                  I_VALUE     = LV_DATE2.
            ELSE.
              LV_UPDATE = 'X'.
            ENDIF.
    
          ENDIF.
    
        ELSE.
          LV_UPDATE = 'X'.
        ENDIF.
    
        IF LV_UPDATE = 'X'.
          CALL METHOD UR_DATA_CHANGED->MODIFY_CELL
            EXPORTING
              I_ROW_ID    = LS_FTAB-ROW_ID
              I_FIELDNAME = 'UPDATE_FLAG'
              I_VALUE     = 'X'.
          LS_PDLV_DATA-UPDATE_FLAG = 'X'.
          MODIFY GT_PDLV_DATA FROM LS_PDLV_DATA INDEX LS_FTAB-ROW_ID.
        ENDIF.
    
      ENDLOOP.
    
    ENDFORM.                    " SUB_METHOD_DATA_CHANGED
    
    *&---------------------------------------------------------------------*
    *& Form FRM_CHECK_NUM
    *&---------------------------------------------------------------------*
    *& 检查是否为纯数字
    *&---------------------------------------------------------------------*
    *&      --> uv_num
    *&      <-- cv_num
    *&      <-- cv_flag
    *&---------------------------------------------------------------------*
    FORM FRM_CHECK_NUM USING UV_NUM CHANGING CV_NUM CV_FLAG.
    
      ZCL_CM_PUBLIC=>CONVER_CHAR_TO_NUM( EXPORTING UV_NUM  = UV_NUM
                                         IMPORTING CV_NUM  = CV_NUM
                                         RECEIVING CV_FLAG = CV_FLAG ).
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_CHECK_PO_QTY
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *&      --> UV_ROW_ID
    *&      --> UV_VALUE
    *&---------------------------------------------------------------------*
    FORM FRM_CHECK_PO_QTY  USING US_PDLV_DATA LIKE GS_PDLV_DATA
                                 UV_VALUE UV_ROW_ID
                           CHANGING CV_MESSAGE.
    
      DATA: LV_QTY LIKE EKPO-MENGE.
      DATA: LV_QT1 LIKE EKPO-MENGE.
      DATA : LT_ZMATDOC TYPE TABLE OF ZMATDOC .
      DATA : LS_ZMATDOC TYPE ZMATDOC .
      DATA : LS_ZMMWMS_POL TYPE ZMMWMS_POL .
      DATA : LT_ZMMWMS_POL TYPE TABLE OF ZMMWMS_POL .
      DATA : LV_QTYC1(30) TYPE C .
      DATA : LV_QTYC2(30) TYPE C .
    
      CLEAR CV_MESSAGE.
    
      IF UV_VALUE = 0.
        CV_MESSAGE = '数量不能为0，请使用关闭功能'.
        EXIT.
      ENDIF.
    
    *  IF US_PDLV_DATA-RECEIVED_QUAN > UV_VALUE  .
      IF US_PDLV_DATA-RECEIVED_QUAN >= US_PDLV_DATA-ASSIGN_MENGE  .
        CV_MESSAGE = '当前已收数量超过当前分配数量不允许更改'.
        EXIT.
      ENDIF .
    
      LOOP AT GT_PDLV_DATA INTO DATA(LS_PDLV_DATA) WHERE DLV_PLANNO = US_PDLV_DATA-DLV_PLANNO
                                                     AND DLV_ITEMNO = US_PDLV_DATA-DLV_ITEMNO
                                                     AND XCLOSED = '' . "add by lanjingcun 20200802 排除已关闭的行
    *    CHECK LS_PDLV_DATA-UNIQUEID <> US_PDLV_DATA-UNIQUEID.
        CHECK LS_PDLV_DATA-INDEX <> US_PDLV_DATA-INDEX."ADD BY LANJINGCUN 20200830 用字段index判断是否为当前行
        LV_QTY = LV_QTY + LS_PDLV_DATA-ASSIGN_MENGE.
      ENDLOOP.
    
      LV_QTY = LV_QTY + UV_VALUE.
    
      IF US_PDLV_DATA-RECEIVED_QUAN > 0.
    
        IF LV_QTY <= US_PDLV_DATA-RECEIVED_QUAN.
          CV_MESSAGE = '数量不能小于等于已收货数量'.
          EXIT.
        ENDIF.
      ENDIF.
    
      SELECT SINGLE MENGE, UEBTO
        FROM EKPO
       WHERE UNIQUEID = @US_PDLV_DATA-UNIQUEID
        INTO @DATA(LS_EKPO).
    
      DATA(LV_MENGE) = LS_EKPO-MENGE * ( 100 + LS_EKPO-UEBTO ) / 100.
    
      CLEAR: LV_QTY.
      LOOP AT GT_PDLV_DATA INTO LS_PDLV_DATA WHERE UNIQUEID = US_PDLV_DATA-UNIQUEID
                                               AND XCLOSED = '' . "add by lanjingcun 20200802 排除已关闭的行
    *    CHECK LS_PDLV_DATA-GUID <> US_PDLV_DATA-GUID.
        CHECK LS_PDLV_DATA-INDEX <> US_PDLV_DATA-INDEX."ADD BY LANJINGCUN 20200830 用字段index判断是否为当前行
        LV_QTY = LV_QTY + LS_PDLV_DATA-ASSIGN_MENGE.
      ENDLOOP.
    
      LV_QTY = LV_QTY + UV_VALUE.
    
    
    
    ******************************************************
      "总分配数 = 屏幕上PO总分配（未关闭）
      " + POL期初数（未关闭）
      " + PO总收货（ekbe）（包含已关闭、前台操作数据、期初和未关闭收货）
      " - PO对应未关闭收货计划的已收货（如果该已收货大于分配数时,此时应该减去该行分配数量）
      " - POL期初已收货（如果该已收货大于分配数时,此时应该减去该行分配数量）
    
    
      "屏幕上PO总分配 +  PO总收货（ekbe）
      LV_QTY =  LV_QTY + US_PDLV_DATA-PORECEIVED_QUAN .
    
      "获取采购订单对应pol的收货
      CLEAR LT_ZMATDOC .
      SELECT *
        INTO TABLE LT_ZMATDOC
        FROM ZMATDOC
        WHERE EBELN =  US_PDLV_DATA-EBELN
      AND EBELP =  US_PDLV_DATA-EBELP .
    
      " - PO对应未关闭收货计划的已收货（如果该已收货大于分配数时,此时应该减去该行分配数量）
      DATA : LT_PDLV_DATAL LIKE GT_PDLV_DATA .
      DATA : LS_PDLV_DATAL LIKE LINE OF GT_PDLV_DATA .
      "因为有可能存在多行 送货计划号和送货计划行完全一致的数据 因为要按照送货计划号和行进行汇总比对
      LT_PDLV_DATAL[] = GT_PDLV_DATA[] .
      DELETE LT_PDLV_DATAL WHERE UNIQUEID <> US_PDLV_DATA-UNIQUEID .
      DELETE LT_PDLV_DATAL WHERE XCLOSED <> '' .
      SORT LT_PDLV_DATAL BY DLV_PLANNO DLV_ITEMNO .
      DELETE ADJACENT DUPLICATES FROM LT_PDLV_DATAL
                         COMPARING DLV_PLANNO DLV_ITEMNO .
    
      LOOP AT LT_PDLV_DATAL INTO LS_PDLV_DATAL .
    
    
        "按送货计划汇总已分配
        CLEAR LS_PDLV_DATAL-ASSIGN_MENGE .
        LOOP AT GT_PDLV_DATA INTO LS_PDLV_DATA WHERE UNIQUEID = US_PDLV_DATA-UNIQUEID
                                                  AND DLV_PLANNO = LS_PDLV_DATAL-DLV_PLANNO
                                                  AND DLV_ITEMNO = LS_PDLV_DATAL-DLV_ITEMNO
                                                  AND XCLOSED = '' .
    
          IF LS_PDLV_DATA-INDEX = US_PDLV_DATA-INDEX .
            LS_PDLV_DATAL-ASSIGN_MENGE = LS_PDLV_DATAL-ASSIGN_MENGE + UV_VALUE .
          ELSE .
            LS_PDLV_DATAL-ASSIGN_MENGE = LS_PDLV_DATAL-ASSIGN_MENGE + LS_PDLV_DATA-ASSIGN_MENGE .
          ENDIF .
    
        ENDLOOP .
    
        "按送货计划汇总已收货
        CLEAR LS_ZMATDOC .
        CLEAR LV_QT1 .
        LOOP AT LT_ZMATDOC INTO LS_ZMATDOC WHERE  DLV_PLANNO = LS_PDLV_DATAL-DLV_PLANNO
                                              AND DLV_ITEMNO = LS_PDLV_DATAL-DLV_ITEMNO .
    
          IF LS_ZMATDOC-SHKZG = 'S' .
            LV_QT1 = LV_QT1 + LS_ZMATDOC-MENGE .
          ELSEIF LS_ZMATDOC-SHKZG = 'H' .
            LV_QT1 = LV_QT1 - LS_ZMATDOC-MENGE .
          ENDIF .
    
        ENDLOOP .
        " PO对应未关闭收货计划的已收货
    *    IF  LS_PDLV_DATA-GUID = US_PDLV_DATA-GUID."为当前行时 取时时数据UV_VALUE做比较
    *      IF LV_QT1 < UV_VALUE .
    *        LV_QTY = LV_QTY - LV_QT1 .
    *      ELSE .
    *        LV_QTY = LV_QTY - UV_VALUE  ."（如果该已收货大于分配数时,此时应该减去该行分配数量）
    *      ENDIF .
    *    ELSE .
    *      IF LV_QT1 < LS_PDLV_DATA-ASSIGN_MENGE .
    *        LV_QTY = LV_QTY - LV_QT1 .
    *      ELSE .
    *        LV_QTY = LV_QTY - LS_PDLV_DATA-ASSIGN_MENGE  .
    *      ENDIF .
    *    ENDIF .
        IF LV_QT1 < LS_PDLV_DATAL-ASSIGN_MENGE .
          LV_QTY = LV_QTY - LV_QT1 .
        ELSE .
          LV_QTY = LV_QTY - LS_PDLV_DATAL-ASSIGN_MENGE  .
        ENDIF .
    
      ENDLOOP .
    
      "未关闭期初的分配(逻辑主表从tmp表获取，所以没有期初数据)
      CLEAR LT_ZMMWMS_POL ." ZMMWMS_POL .
      SELECT *
        INTO TABLE LT_ZMMWMS_POL
        FROM ZMMWMS_POL
        WHERE ORIGIN = 'M' "期初
          AND UNIQUEID =  US_PDLV_DATA-UNIQUEID
          AND PDLV_MENGE > 0
          AND XCLOSED = '' ."未关闭
    
      "添加期初的占用
      LOOP AT LT_ZMMWMS_POL INTO LS_ZMMWMS_POL .
        " + POL期初数（未关闭）
        LV_QTY = LV_QTY + LS_ZMMWMS_POL-PDLV_MENGE .
    
        " - POL期初已收货（如果该已收货大于分配数时,此时应该减去该行分配数量）
        CLEAR LS_ZMATDOC .
        CLEAR LV_QT1 .
        LOOP AT LT_ZMATDOC INTO LS_ZMATDOC WHERE  DLV_PLANNO = LS_ZMMWMS_POL-DLV_PLANNO
                                              AND DLV_ITEMNO = LS_ZMMWMS_POL-DLV_ITEMNO .
    
          IF LS_ZMATDOC-SHKZG = 'S' .
            LV_QT1 = LV_QT1 + LS_ZMATDOC-MENGE .
          ELSEIF LS_ZMATDOC-SHKZG = 'H' .
            LV_QT1 = LV_QT1 - LS_ZMATDOC-MENGE .
          ENDIF .
    
        ENDLOOP .
        IF LV_QT1 < LS_ZMMWMS_POL-PDLV_MENGE .
          LV_QTY = LV_QTY - LV_QT1 .
        ELSE .
          LV_QTY = LV_QTY - LS_ZMMWMS_POL-PDLV_MENGE  .
        ENDIF .
    
      ENDLOOP .
    
    
    
    **************************************************************
    
    *  "ADD BY LANJINGCUN 关闭的,但是已收货的需要考虑  前台单独运行收货的 未关闭期初的分配(逻辑主表从tmp表获取，所以没有期初数据)
    *  "关闭的 但是已收货的需要考虑（根据po行项目取 所以包含正常和期初的数据）
    *  CLEAR LT_ZMATDOC .
    *  SELECT *
    *    INTO TABLE LT_ZMATDOC
    *    FROM ZMATDOC
    *    WHERE EXISTS ( SELECT *
    *                    FROM ZMMWMS_POL
    *                     WHERE DLV_PLANNO = ZMATDOC~DLV_PLANNO
    *                       AND DLV_ITEMNO = ZMATDOC~DLV_ITEMNO
    *                       AND UNIQUEID =  US_PDLV_DATA-UNIQUEID
    *                       AND XCLOSED = 'X'
    *    ) .
    *
    *  "前台单独运行收货的
    *  SELECT *
    *    APPENDING CORRESPONDING FIELDS OF TABLE LT_ZMATDOC
    *    FROM EKBE
    *    WHERE EBELN = US_PDLV_DATA-EBELN
    *      AND EBELP = US_PDLV_DATA-EBELP
    *      AND BWART <> ''
    *      AND NOT EXISTS ( SELECT *
    *                   FROM ZMATDOC
    *                   WHERE MBLNR  = EKBE~BELNR
    *                     AND MJAHR  = EKBE~GJAHR
    *                     AND ZEILE  = EKBE~BUZEI
    *    ) .
    *
    *
    *    "未关闭期初的分配(逻辑主表从tmp表获取，所以没有期初数据)
    *    CLEAR ZMMWMS_POL .
    *    SELECT *
    *      INTO TABLE LT_ZMMWMS_POL
    *      FROM ZMMWMS_POL
    *      WHERE ORIGIN = 'M' "期初
    *        AND UNIQUEID =  US_PDLV_DATA-UNIQUEID
    *        AND PDLV_MENGE > 0
    *        AND XCLOSED = '' ."未关闭
    *
    *
    *    "添加已关闭 但是已收货的占用
    *    CLEAR LS_ZMATDOC .
    *    LOOP AT LT_ZMATDOC INTO LS_ZMATDOC .
    *
    *      IF LS_ZMATDOC-SHKZG = 'S' .
    *        LV_QTY = LV_QTY + LS_ZMATDOC-MENGE .
    *      ELSEIF LS_ZMATDOC-SHKZG = 'H' .
    *        LV_QTY = LV_QTY - LS_ZMATDOC-MENGE .
    *      ENDIF .
    *
    *    ENDLOOP .
    *
    *    "添加期初的占用
    *    LOOP AT LT_ZMMWMS_POL INTO LS_ZMMWMS_POL .
    *      LV_QTY = LV_QTY + LS_ZMMWMS_POL-PDLV_MENGE .
    *    ENDLOOP .
    *    "ADD BY LANJINGCUN 关闭的 但是已收货的需要考虑
    
    
      IF LV_QTY > LV_MENGE.
        LV_QTYC1 = LV_QTY .
        LV_QTYC2 = LV_MENGE .
        CONDENSE LV_QTYC1 NO-GAPS .
        CONDENSE LV_QTYC2 NO-GAPS .
    *      CV_MESSAGE = '数量不能超过采购订单允许的容差数量'.
        CONCATENATE '数量' LV_QTYC1 '不能超过采购订单允许的容差数量' LV_QTYC2  INTO  CV_MESSAGE  .
    
        EXIT.
      ENDIF.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form frm_check_pdlv_date
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *&      --> UV_ROW_ID
    *&      --> UV_VALUE
    *&---------------------------------------------------------------------*
    FORM FRM_CHECK_PDLV_DATE  USING US_PDLV_DATA LIKE GS_PDLV_DATA
                                    UV_VALUE
                           CHANGING CV_MESSAGE.
    
      DATA: LV_DATE LIKE SY-DATUM.
    
      CLEAR CV_MESSAGE.
    
      IF UV_VALUE < SY-DATUM.
        CV_MESSAGE = '送货日期不能小于当前日期'.
        EXIT.
      ENDIF.
    
      SELECT SINGLE VERSION, REMAIN_DAYS
        INTO @DATA(LS_PL)
        FROM ZMMAPS_PL
       WHERE GUID = @US_PDLV_DATA-GUID.
    
      "UP BY LANJINGCUN 20200426 最晚答复日期检查 start
    *  PERFORM frm_get_date_add_days USING ls_pl-version ls_pl-remain_days CHANGING lv_date.
      LV_DATE = US_PDLV_DATA-ZZWDFR .
      "UP BY LANJINGCUN 20200426 最晚答复日期检查 end
    
      IF UV_VALUE > LV_DATE.
    *    CV_MESSAGE = '送货日期不能大于APS允许的最迟送货日期'.
        CV_MESSAGE = '送货日期不能大于最晚答复日期'.
        EXIT.
      ENDIF.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_SET_DATA_CHANGE_MESSAGE
    *&---------------------------------------------------------------------*
    *& 设置数据修改时的报错信息
    *&---------------------------------------------------------------------*
    *&      --> UR_DATA_CHANGED
    *&      --> U_MSGID
    *&      --> U_MSGTY
    *&      --> U_MSGNO
    *&      --> U_MSGV1
    *&      --> U_FIELDNAME
    *&---------------------------------------------------------------------*
    FORM FRM_SET_DATA_CHANGE_MESSAGE  USING    UR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                               U_MSGID
                                               U_MSGTY
                                               U_MSGNO
                                               U_MSGV1
                                               U_MSGV2
                                               U_MSGV3
                                               U_MSGV4
                                               U_FIELDNAME
                                               U_ROW_ID
                                               U_TABIX.
    
      CALL METHOD UR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
        EXPORTING
          I_MSGID     = U_MSGID
          I_MSGTY     = U_MSGTY
          I_MSGNO     = U_MSGNO
          I_MSGV1     = U_MSGV1
          I_MSGV2     = U_MSGV2
          I_MSGV3     = U_MSGV3
          I_MSGV4     = U_MSGV4
          I_FIELDNAME = U_FIELDNAME
          I_ROW_ID    = U_ROW_ID
          I_TABIX     = U_TABIX.
    
    ENDFORM.