*&---------------------------------------------------------------------*
*& 包含               ZMMR0050_CLASS
*&---------------------------------------------------------------------*

CLASS LCL_EVENT DEFINITION.
    " data_changed_fininsed
  
    PUBLIC SECTION.
  *    METHODS
  **     回车事件
  *      handle_enter        FOR EVENT data_changed_finished OF cl_gui_alv_grid
  *        IMPORTING
  *          e_modified
  *          et_good_cells.
  
      METHODS:
        HANDLE_ONF4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
          IMPORTING E_FIELDNAME ES_ROW_NO ER_EVENT_DATA.
  
      METHODS:
        HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
          IMPORTING E_OBJECT E_INTERACTIVE.
  
      METHODS:
        HANDLE_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
          IMPORTING E_UCOMM.
  
      METHODS:
        HANDLE_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
          IMPORTING E_ROW E_COLUMN ES_ROW_NO .
  
      METHODS
  *     回车事件
        HANDLE_DATA_CHANGED          FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
          IMPORTING E_MODIFIED
                    ET_GOOD_CELLS.
  
  
  *    METHODS: check_fieldname  "获取选择单元格
  *      IMPORTING
  *        ps_good_planetype TYPE lvc_s_modi
  *        pr_data_changed   TYPE REF TO cl_alv_changed_data_protocol.
  
  
  
  
  ENDCLASS.
  
  
  
  CLASS LCL_EVENT IMPLEMENTATION.
  
    METHOD HANDLE_ONF4.
      DATA: LS_LS_MODI TYPE LVC_S_MODI,
            LT_LS_MODI LIKE TABLE OF LS_LS_MODI,
            LS_RET_TAB TYPE DDSHRETVAL,
            LT_RET_TAB LIKE TABLE OF LS_RET_TAB.
      DATA: LS_RETURN_TAB	TYPE DDSHRETVAL,
            LT_RETURN_TAB LIKE TABLE OF LS_RETURN_TAB.
      CLEAR:LS_LS_MODI,LT_LS_MODI,LS_RET_TAB,LT_RET_TAB,LS_RETURN_TAB,LT_RETURN_TAB.
  
      DATA : LS_T052 TYPE T052 .
  
      CASE E_FIELDNAME.
        WHEN 'PMNTTRMS'.
  
          DATA:LT_T052U TYPE TABLE OF T052U,
               LS_T052U TYPE T052U.
  
  
          SELECT * FROM T052U
                   INTO TABLE LT_T052U
                   WHERE SPRAS = '1'.
  
          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
            EXPORTING
              RETFIELD        = 'ZTERM'
              VALUE_ORG       = 'S'
            TABLES
              VALUE_TAB       = LT_T052U
              RETURN_TAB      = LT_RETURN_TAB
            EXCEPTIONS
              PARAMETER_ERROR = 1
              NO_VALUES_FOUND = 2
              OTHERS          = 3.
          IF SY-SUBRC = 0 AND LT_RETURN_TAB IS NOT INITIAL.
            READ TABLE GS_ALV-STYLE INTO GS_EDIT WITH KEY FIELDNAME = 'PMNTTRMS'.
            IF SY-SUBRC = 0.
              RETURN.
            ENDIF.
  
            READ TABLE LT_RETURN_TAB INTO LS_RETURN_TAB  INDEX 1.
            IF SY-SUBRC = 0.
              READ TABLE LT_T052U INTO LS_T052U WITH KEY ZTERM =  LS_RETURN_TAB-FIELDVAL.
              IF SY-SUBRC = 0.
                READ TABLE GT_ALV INTO  GS_ALV INDEX ES_ROW_NO-ROW_ID .
                GS_ALV-PMNTTRMS = LS_T052U-ZTERM.
  
                IF GS_ALV-PMNTTRMS <> '' .
  
                  CLEAR LS_T052 .
                  CALL FUNCTION 'FI_FIND_PAYMENT_CONDITIONS'
                    EXPORTING
                      I_ZTERM            = GS_ALV-PMNTTRMS
                      I_BLDAT            = GS_ALV-ZBUDAT_Q
                      I_BUDAT            = GS_ALV-ZBUDAT_Q
                    IMPORTING
                      E_T052             = LS_T052
                    EXCEPTIONS
                      TERMS_INCORRECT    = 1
                      TERMS_NOT_FOUND    = 2
                      NO_DATE_ENTERED    = 3
                      NO_DAY_LIMIT_FOUND = 4
                      OTHERS             = 5.
  
                  GS_ALV-ZFKTS  = LS_T052-ZTAG1 .
  
                ENDIF .
  
                GS_ALV-ZFKRQ = GS_ALV-BLINE_DATE + GS_ALV-ZFKTS .
  
                MODIFY GT_ALV FROM GS_ALV INDEX ES_ROW_NO-ROW_ID TRANSPORTING PMNTTRMS
                     ZFKTS  ZFKRQ.
              ENDIF.
            ENDIF.
          ENDIF.
  
      ENDCASE.
  
      ER_EVENT_DATA->M_EVENT_HANDLED = 'X'.
  
      PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
    ENDMETHOD.
  
  
    METHOD HANDLE_TOOLBAR.
      DATA:LS_TOOLBAR TYPE STB_BUTTON.
  
  
      CASE 'X'.
  
        WHEN RB_01.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZQX'.
          LS_TOOLBAR-QUICKINFO = '全 选'.
          LS_TOOLBAR-TEXT = '全 选'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZQXQX'.
          LS_TOOLBAR-QUICKINFO = '取消全选'.
          LS_TOOLBAR-TEXT = '取消全选'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
  
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZADD'.
          LS_TOOLBAR-QUICKINFO = '新 增'.
          LS_TOOLBAR-TEXT = '新 增'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZMOD'.
          LS_TOOLBAR-QUICKINFO = '修 改'.
          LS_TOOLBAR-TEXT = '修 改'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZDEL'.
          LS_TOOLBAR-QUICKINFO = '删 除'.
          LS_TOOLBAR-TEXT = '删 除'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZSAVE'.
          LS_TOOLBAR-QUICKINFO = '保 存'.
          LS_TOOLBAR-TEXT = '保 存'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
          CLEAR:LS_TOOLBAR.
  
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
  
        WHEN RB_02.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZQX'.
          LS_TOOLBAR-QUICKINFO = '全 选'.
          LS_TOOLBAR-TEXT = '全 选'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZQXQX'.
          LS_TOOLBAR-QUICKINFO = '取消全选'.
          LS_TOOLBAR-TEXT = '取消全选'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZADD'.
          LS_TOOLBAR-QUICKINFO = '新 增'.
          LS_TOOLBAR-TEXT = '新 增'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZMOD'.
          LS_TOOLBAR-QUICKINFO = '修 改'.
          LS_TOOLBAR-TEXT = '修 改'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZDEL'.
          LS_TOOLBAR-QUICKINFO = '删 除'.
          LS_TOOLBAR-TEXT = '删 除'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
  
  *        CLEAR:LS_TOOLBAR.   “取消在行项目阶段的确认对账  跟取消对账功能
  *        LS_TOOLBAR-FUNCTION = 'ZQRDZ'.
  *        LS_TOOLBAR-QUICKINFO = '确认对账'.
  *        LS_TOOLBAR-TEXT = '确认对账'.
  *        LS_TOOLBAR-DISABLED = ''.
  *        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  *
  *        CLEAR:LS_TOOLBAR.
  *        LS_TOOLBAR-BUTN_TYPE = 3.
  *        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
  *        CLEAR:LS_TOOLBAR.  "del by lanjingcun 取消对账作废按钮 20200810
  *        LS_TOOLBAR-FUNCTION = 'ZCXDZ'.
  *        LS_TOOLBAR-QUICKINFO = '对账作废'.
  *        LS_TOOLBAR-TEXT = '对账作废'.
  *        LS_TOOLBAR-DISABLED = ''.
  *        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
  *        CLEAR:LS_TOOLBAR.  “取消在行项目阶段的确认对账  跟取消对账功能
  *        LS_TOOLBAR-FUNCTION = 'ZQXDZ'.
  *        LS_TOOLBAR-QUICKINFO = '取消对账'.
  *        LS_TOOLBAR-TEXT = '取消对账'.
  *        LS_TOOLBAR-DISABLED = ''.
  *        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  *
  *        CLEAR:LS_TOOLBAR.
  *        LS_TOOLBAR-BUTN_TYPE = 3.
  *        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
        WHEN RB_04.
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZQX'.
          LS_TOOLBAR-QUICKINFO = '全 选'.
          LS_TOOLBAR-TEXT = '全 选'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZQXQX'.
          LS_TOOLBAR-QUICKINFO = '取消全选'.
          LS_TOOLBAR-TEXT = '取消全选'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZQRDZ'.
          LS_TOOLBAR-QUICKINFO = '确认对账'.
          LS_TOOLBAR-TEXT = '确认对账'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZQXDZ'.
          LS_TOOLBAR-QUICKINFO = '取消对账'.
          LS_TOOLBAR-TEXT = '取消对账'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
          CLEAR:LS_TOOLBAR.
          LS_TOOLBAR-FUNCTION = 'ZSAVE'.
          LS_TOOLBAR-QUICKINFO = '保 存'.
          LS_TOOLBAR-TEXT = '保 存'.
          LS_TOOLBAR-DISABLED = ''.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
          CLEAR:LS_TOOLBAR.
  
          LS_TOOLBAR-BUTN_TYPE = 3.
          APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  
      ENDCASE.
  
      IF RB_03 = '' .
        CLEAR:LS_TOOLBAR.
        LS_TOOLBAR-FUNCTION = 'ZTONG'.
        LS_TOOLBAR-QUICKINFO = '统 计'.
        LS_TOOLBAR-TEXT = '统 计'.
        LS_TOOLBAR-DISABLED = ''.
        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
        CLEAR:LS_TOOLBAR.
  
        LS_TOOLBAR-BUTN_TYPE = 3.
        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
      ENDIF .
  
  
      DELETE E_OBJECT->MT_TOOLBAR WHERE FUNCTION = '&GRAPH'
      OR FUNCTION = '&INFO'
      OR FUNCTION = '&REFRESH'
      OR FUNCTION = '&CHECK'
      OR FUNCTION = '&LOCAL&CUT'
      OR FUNCTION = '&LOCAL&COPY'
      OR FUNCTION = '&LOCAL&PASTE'
      OR FUNCTION = '&LOCAL&UNDO'
      OR FUNCTION = '&LOCAL&APPEND'
      OR FUNCTION = '&LOCAL&INSERT_ROW'
      OR FUNCTION = '&LOCAL&DELETE_ROW'
      OR FUNCTION = '&LOCAL&COPY_ROW'
      .
  
    ENDMETHOD.
  
  
  
    METHOD HANDLE_COMMAND.
      DATA   L_TABIX TYPE SY-TABIX.
      DATA: ET_INDEX_ROWS	TYPE LVC_T_ROW,
            ES_INDEX_ROW  TYPE LVC_S_ROW.
      CLEAR:ET_INDEX_ROWS,ES_INDEX_ROW.
      DATA: LS_STABLE TYPE LVC_S_STBL.
      CLEAR:LS_STABLE.
      DATA : LS_ALV TYPE TY_TAB .
  
      DATA : LV_TXT1 TYPE STRING .
      DATA : LV_TXT2 TYPE STRING .
      DATA : LV_TXT3 TYPE STRING .
      DATA : LV_TXT4 TYPE STRING .
  
  
      CLEAR GV_UCOMM .
      GV_UCOMM = E_UCOMM .
  
      " CALL METHOD g_gui_alv->DATA_CHANGED_FINISHED.
  
  *  DATA: L_VALID TYPE C .
  *    CALL METHOD g_gui_alv->CHECK_CHANGED_DATA
  *    IMPORTING
  *      E_VALID = L_VALID.
      DATA:LS_OUT TYPE TY_TAB,
           LT_OUT LIKE TABLE OF LS_OUT.
  
      CASE E_UCOMM.
        WHEN 'ZQX' .
          LOOP AT  GT_ALV ASSIGNING FIELD-SYMBOL(<FS_ALVQ>) .
            <FS_ALVQ>-SEL = 'X'.
          ENDLOOP.
          PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
        WHEN 'ZQXQX' .
          LOOP AT  GT_ALV ASSIGNING FIELD-SYMBOL(<FS_ALVX>) .
            <FS_ALVX>-SEL = ' '.
          ENDLOOP.
          PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
        WHEN 'ZTONG' .
  
          CLEAR LS_ALV .
          CLEAR LV_TXT1 .
          CLEAR LV_TXT2 .
          CLEAR LV_TXT3 .
          CLEAR LV_TXT4 .
          LOOP AT GT_ALV INTO LS_ALV WHERE SEL = 'X' .
  
            LV_TXT2  = LV_TXT2 +   LS_ALV-ZSHUI  .
            LV_TXT3  = LV_TXT3 +   LS_ALV-DMBTR  .
            LV_TXT4  = LV_TXT4 +   LS_ALV-ZSJHJ  .
            LV_TXT1  = LV_TXT1 +   LS_ALV-ZSHSL  .
  
          ENDLOOP .
          IF SY-SUBRC = 0 .
            CONCATENATE '所选行 数量 总计:' LV_TXT1 INTO LV_TXT1 .
            CONCATENATE '所选行 税额 总计:' LV_TXT2 INTO LV_TXT2 .
            CONCATENATE '所选行 净值 总计:' LV_TXT3 INTO LV_TXT3 .
            CONCATENATE '所选行 价税 总计:' LV_TXT4 INTO LV_TXT4 .
  
            CALL FUNCTION 'POPUP_TO_INFORM'
              EXPORTING
                TITEL = '统计'
                TXT1  = LV_TXT1
                TXT2  = LV_TXT2
                TXT3  = LV_TXT3
                TXT4  = LV_TXT4.
  
          ELSE .
            MESSAGE '请选择需要统计的行。'  TYPE 'S' .
          ENDIF .
          PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
        WHEN 'ZADD' .
  
          CLEAR GV_TABIX1 .
  
          READ TABLE GT_ALV INTO GS_ALV WITH KEY SEL = 'X'.
          IF  SY-SUBRC = 0.
            GV_TABIX1  = SY-TABIX .
            LS_ALV = GS_ALV.
            CLEAR:GS_ALV.
            GS_ALV-SEL = 'X'.
            GS_ALV-ZDZDH = LS_ALV-ZDZDH.
            GS_ALV-ZSJZT = 'Y'.
            GS_ALV-EBELN = LS_ALV-EBELN.
            GS_ALV-EBELP = LS_ALV-EBELP.
            GS_ALV-MATNR = LS_ALV-MATNR.
            "  GS_ALV-MAKTX = LS_ALV-MAKTX.
            GS_ALV-LIFNR = LS_ALV-LIFNR.
            GS_ALV-NAME1 = LS_ALV-NAME1.
            GS_ALV-LFGJA = LS_ALV-LFGJA.
            GS_ALV-LFBNR = LS_ALV-LFBNR.
            GS_ALV-LFPOS = LS_ALV-LFPOS.
            GS_ALV-WERKS = LS_ALV-WERKS.
            GS_ALV-ZDZZT = LS_ALV-ZDZZT.
            GS_ALV-MEINS = LS_ALV-MEINS.
  *          GS_ALV-MEINS = LS_ALV-ZSJZT.
            GS_ALV-MWSKZ = LS_ALV-MWSKZ.
            GS_ALV-ZSL = LS_ALV-ZSL.
            GS_ALV-HSWAE = LS_ALV-HSWAE.
            CALL  SCREEN 200 STARTING AT 5 5 ENDING AT  55 23.
          ENDIF.
  
  *          PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
          IF OK_CODE = 'ZSAVE' .
            PERFORM FRM_FREE_CREAT .
          ENDIF .
        WHEN 'ZMOD'.
  
          PERFORM FRM_GET_ZMOD.
  *         PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
          IF OK_CODE = 'ZSAVE' .
            PERFORM FRM_FREE_CREAT .
          ENDIF .
  
        WHEN 'ZSAVE'.
          CASE  'X' .
            WHEN  RB_04.
              PERFORM FRM_GET_ZSAVE4.
              PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
            WHEN OTHERS.
              PERFORM FRM_GET_ZSAVE.
              PERFORM FRM_FREE_CREAT .
          ENDCASE.
  
  
        WHEN 'ZDEL'.
  
          PERFORM FRM_GET_ZDEL.
          PERFORM FRM_FREE_CREAT .
  
        WHEN 'ZQRDZ'.
          IF  RB_04 = 'X'.
            PERFORM FRM_GET_ZQRDZ1.
          ELSE.
            PERFORM FRM_GET_ZQRDZ.
          ENDIF.
  
          PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
        WHEN 'ZCXDZ'. "作废
  
          PERFORM FRM_GET_ZCXDZ.
          PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
  
        WHEN 'ZQXDZ'. "取消对账
          IF  RB_04 = 'X'.
            PERFORM FRM_GET_ZQXDZ1.
          ELSE.
  *          PERFORM FRM_GET_ZQXDZ.
          ENDIF.
          PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
      ENDCASE.
  
  *    CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
  *      EXPORTING
  *        NEW_CODE = '&SUBTOT'.
  
  
    ENDMETHOD.
  
  
    METHOD HANDLE_DATA_CHANGED.
  
      DATA:LS_ALV TYPE TY_TAB.
      " PERFORM fm_enter USING e_modified et_good_cells.
      DATA: LV_VALUE TYPE STRING.
      DATA: LS_GOOD TYPE LVC_S_MODI.
      FIELD-SYMBOLS <LS_ALV> TYPE TY_TAB.
  
      IF  RB_04 = 'X'.
  
        CLEAR GT_ALVO[] .
        GT_ALVO[] = GT_ALV[] .
        LOOP AT GT_ALV ASSIGNING <LS_ALV>  .
          <LS_ALV>-ZFKRQ = <LS_ALV>-BLINE_DATE + <LS_ALV>-ZFKTS .
        ENDLOOP .
        IF GT_ALVO[] <> GT_ALV[]
          AND SY-UCOMM = ''.
          PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
        ENDIF .
  
  *      LOOP AT ER_DATA_CHANGED->MT_MOD_CELLS INTO LS_GOOD.
  *        CASE LS_GOOD-FIELDNAME.
  *          WHEN 'BLINE_DATE'.
  *            READ TABLE  GT_ALV INTO GS_ALV INDEX  LS_GOOD-ROW_ID .
  *            IF SY-SUBRC = 0 .
  *              GS_ALV-ZFKRQ = GS_ALV-ZFKRQ + GS_ALV-ZFKTS .
  *              MODIFY GT_ALV FROM GS_ALV INDEX  LS_GOOD-ROW_ID.
  *            ENDIF .
  **            IF  GS_ALV-SEL = '' .
  **              LOOP AT  GT_ALV ASSIGNING FIELD-SYMBOL(<FS_ALVS>)  WHERE  EBELN = GS_ALV-EBELN.
  **                <FS_ALVS>-SEL = 'X'.
  **              ENDLOOP.
  **            ELSE.
  **              LOOP AT  GT_ALV ASSIGNING FIELD-SYMBOL(<FS_ALVS1>)  WHERE  EBELN = GS_ALV-EBELN.
  **                <FS_ALVS1>-SEL = ' '.
  **              ENDLOOP.
  **            ENDIF.
  **
  *        ENDCASE.
  *      ENDLOOP.
      ENDIF.
      IF SY-UCOMM <> '' .
        PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
      ENDIF .
    ENDMETHOD.
  
    METHOD HANDLE_DOUBLE_CLICK .
      CASE  E_COLUMN-FIELDNAME  .
        WHEN 'ZDZWB' ."add by lanjc 备注的处理
          READ TABLE GT_ALV INTO GS_ALV INDEX ES_ROW_NO-ROW_ID.
          IF  SY-SUBRC = 0.
            PERFORM FRM_CALL_LONG  .
  
            PERFORM FRM_GET_LONGTEXT  USING
                    G_PR_LTXT
                    GS_ALV-ZDZWB .
            MODIFY GT_ALV FROM GS_ALV INDEX ES_ROW_NO-ROW_ID.
  
          ENDIF.
          PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
        WHEN 'EBELN' .
          READ TABLE GT_ALV INTO GS_ALV INDEX ES_ROW_NO-ROW_ID.
          IF  SY-SUBRC = 0.
  
  *        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN .
  *        SET PARAMETER ID 'BES' FIELD GS_ALV-EBELN .
            CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
              EXPORTING
                I_EBELN              = GS_ALV-EBELN
                I_EBELP              = GS_ALV-EBELP
              EXCEPTIONS
                NOT_FOUND            = 1
                NO_AUTHORITY         = 2
                INVALID_CALL         = 3
                PREVIEW_NOT_POSSIBLE = 4
                OTHERS               = 5.
            IF SY-SUBRC <> 0.
  * Implement suitable error handling here
            ENDIF.
  
  
          ENDIF .
  
        WHEN 'BELNR' .
          READ TABLE GT_ALV INTO GS_ALV INDEX ES_ROW_NO-ROW_ID.
          IF  SY-SUBRC = 0.
            CALL FUNCTION 'MIGO_DIALOG'
              EXPORTING
                I_ACTION            = 'A04'
                I_REFDOC            = 'R02'
                I_MBLNR             = GS_ALV-BELNR
                I_MJAHR             = GS_ALV-GJAHR
              EXCEPTIONS
                ILLEGAL_COMBINATION = 1
                OTHERS              = 2.
            IF SY-SUBRC <> 0.
  
            ENDIF.
  
  
          ENDIF .
        WHEN OTHERS .
          IF  RB_04 = 'X'.
  
            READ TABLE GT_ALV INTO GS_ALV INDEX ES_ROW_NO-ROW_ID.
            IF  SY-SUBRC = 0.
              PERFORM FRM_GET_ITEM.
              PERFORM FRM_SHOW_ITEM.
            ENDIF.
          ENDIF.
      ENDCASE .
    ENDMETHOD.
  
  
  
  ENDCLASS.
  
  
  DATA: LCL_EVENT TYPE REF TO LCL_EVENT.