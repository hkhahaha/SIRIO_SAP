*&---------------------------------------------------------------------*
*& 包含          ZPPR0005_C01
*&---------------------------------------------------------------------*
*入库申请单单据-类
CLASS ZCL_PRO_GRREQ_BILL DEFINITION.
    PUBLIC SECTION.
      DATA PROMPT_WHEN_EXIT TYPE CHAR1. "退出单据时，提示信息标识
      INTERFACES ZIF_CUSTOM_BILL_COMMON.
      ALIASES:
              BILL_STATUS   FOR ZIF_CUSTOM_BILL_COMMON~BILL_STATUS ,
              CNS_STATUS_CREATED  FOR ZIF_CUSTOM_BILL_COMMON~CNS_STATUS_CREATED,
              CNS_STATUS_CHANGED  FOR ZIF_CUSTOM_BILL_COMMON~CNS_STATUS_CHANGED,
              CNS_STATUS_DISPLAY  FOR ZIF_CUSTOM_BILL_COMMON~CNS_STATUS_DISPLAY,
              SAVE_BILL FOR ZIF_CUSTOM_BILL_COMMON~SAVE_BILL.
  
  
      "声明事件
      EVENTS:BILL_STATUS_CHANGED EXPORTING VALUE(E_BILL_STATUS) TYPE I VALUE(E_REFRESH_ALV) TYPE CHAR1 DEFAULT ABAP_FALSE."单据状态改变时事件
      CLASS-EVENTS AFTER_SAVE EXPORTING VALUE(ES_BAPIRET1) TYPE BAPIRET1."保存后触发的事件
      CLASS-METHODS: CAL_MARA_VFDAT IMPORTING MATNR TYPE MARA-MATNR START_DATE TYPE DATS RETURNING VALUE(VFDAT) TYPE DATS.
  
      "声明方法
      METHODS:
        SET_BILL_STATUS IMPORTING IMP_BILL_STATUS TYPE I IMP_REFRESH_ALV TYPE CHAR1 DEFAULT ABAP_FALSE,"设置单据状态及刷新ALV标识
        DISPLAY,"显示单据
        REGISTER_ALV_HANDLE_EVENT, "注册alv事件处理程序方法
        "101移动类型不能选择样品仓、Y01只能选择样品仓
        CHECK_LGORT_BY_BWART IMPORTING IV_BWART TYPE BWART IV_LGORT TYPE LGORT_D RETURNING VALUE(RV_MSG) TYPE BAPI_MSG,
        BEFORE_SAVE RETURNING VALUE(CHECK_RESULT) TYPE  BAPIRET1,"保存单据前的数据准备及检查,返回检查结果
        SET_SCREEN_FIELD_EDITABLE IMPORTING IMP_BILL_STATUS TYPE I,"根据单据状态设置屏幕字段
        SET_ALV_FIELDCATELOG IMPORTING   IMP_BILL_STATUS TYPE I,"根据单据状态设置ALV字段目录属性
        ENQUEUE_ZSAPNOTE_NO_LOCK IMPORTING BILLNO TYPE ZSAPNOTE_NO,"入库申请单加锁
        DEQUEUE_ZSAPNOTE_NO_LOCK IMPORTING BILLNO TYPE  ZSAPNOTE_NO,"释放入库申请单锁
        DEQUEUE_ALL_LOCK, "释放所有锁
        "声明ALV事件处理程序方法
        HANDLE_ALV_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING E_OBJECT E_INTERACTIVE,"增加/减少toolbar按钮
        HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM,"自定义按钮事件
        HANDLE_ALV_ONF4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID IMPORTING E_FIELDNAME E_FIELDVALUE ES_ROW_NO ER_EVENT_DATA ET_BAD_CELLS E_DISPLAY,"处理ALV ONF4搜索帮助事件
        "ALV单元格编辑后的处理事件
        HANDLE_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
          IMPORTING SENDER E_MODIFIED ET_GOOD_CELLS,
        HANDLE_BILL_STATUS_CHANGED FOR EVENT BILL_STATUS_CHANGED OF ZCL_PRO_GRREQ_BILL IMPORTING E_BILL_STATUS  E_REFRESH_ALV,"单据状态改变时的事件处理程序方法
        SET_DRDN_TABLE."设置下拉框列表值
  ENDCLASS.
  
  CLASS ZCL_PRO_GRREQ_BILL IMPLEMENTATION.
    "设置下拉框列表值
    METHOD SET_DRDN_TABLE.
      DATA: LT_DRAL TYPE LVC_T_DRAL,                          "#EC NEEDED
            LS_DRAL TYPE LVC_S_DRAL.                          "#EC NEEDED
      SELECT * FROM DD07V WHERE DOMNAME  = 'ZWEMPF' AND DDLANGUAGE = @SY-LANGU INTO TABLE @DATA(LT_DD07V).
      LOOP AT LT_DD07V INTO DATA(LS_DD07V).
        LS_DRAL-HANDLE = '1'.
        LS_DRAL-INT_VALUE = LS_DD07V-DOMVALUE_L.
        LS_DRAL-VALUE = LS_DD07V-DDTEXT.
        APPEND LS_DRAL TO LT_DRAL.
      ENDLOOP.
      GO_ALV_GRID_CREATE->SET_DROP_DOWN_TABLE( IT_DROP_DOWN_ALIAS = LT_DRAL ).
  *    go_alv_grid_create->set_drop_down_table( it_drop_down = lt_dropdown ).
    ENDMETHOD.
    METHOD DEQUEUE_ZSAPNOTE_NO_LOCK.
      CALL FUNCTION 'DEQUEUE_EZ_PP_ZPPT0003'
        EXPORTING
  *       MODE_ZPPT0003       = 'E'
  *       MANDT       = SY-MANDT
          ZSAPNOTE_NO = BILLNO
  *       X_ZSAPNOTE_NO       = ' '
  *       _SCOPE      = '3'
  *       _SYNCHRON   = ' '
  *       _COLLECT    = ' '
        .
  
    ENDMETHOD.
    "释放所有锁
    METHOD DEQUEUE_ALL_LOCK.
      CALL FUNCTION 'DEQUEUE_ALL'
  *     EXPORTING
  *       _SYNCHRON       = ' '
        .
  
    ENDMETHOD.
  
    "显示入库申请单界面
    METHOD DISPLAY.
      CALL SCREEN '9100'.
    ENDMETHOD.
    "计算物料的货架寿命起始日期的过期日
    METHOD CAL_MARA_VFDAT.
      SELECT SINGLE IPRKZ, RDMHD, MHDHB FROM MARA  WHERE MATNR = @MATNR INTO @DATA(LS_MARA).
      IF SY-SUBRC = 0.
        CALL FUNCTION 'VB_MAINTAIN_MHD'
          EXPORTING
            I_IPRKZ_HBD = LS_MARA-IPRKZ "货架寿命到期日的期间标识
            I_RDMHD     = LS_MARA-RDMHD "货架寿命到期日期计算舍入规则
            I_GESHB     = LS_MARA-MHDHB  "总货架寿命
            I_HSDAT     = START_DATE "起算日期
            CHECK       = ' '
            CALC        = 'X'
          IMPORTING
            O_MHDAT     = VFDAT "有效日期
          EXCEPTIONS
            OTHERS      = 99.
      ENDIF.
    ENDMETHOD.
    "101移动类型不能选择样品仓、Y01只能选择样品仓
    METHOD CHECK_LGORT_BY_BWART.
      DATA:LV_LGOBE TYPE LGOBE.
      CLEAR RV_MSG.
      CHECK IV_BWART IS NOT INITIAL AND IV_LGORT IS NOT INITIAL.
      SELECT SINGLE LGOBE INTO LV_LGOBE FROM T001L WHERE WERKS = P_DWERK AND LGORT = IV_LGORT.
      IF SY-SUBRC <> 0.
        RV_MSG = |{ IV_LGORT }{ '仓库不存在' }|.
        RETURN.
      ENDIF.
      IF IV_BWART = '101'.
        IF  LV_LGOBE CA '样'.
          RV_MSG = '101移动类型不能输入样品仓'.
        ENDIF.
      ELSEIF IV_BWART = 'Y01'.
        IF LV_LGOBE CA '样'.
          RV_MSG = ''.
        ELSE.
          RV_MSG = 'Y01移动类型只能输入样品仓'.
        ENDIF.
      ENDIF.
    ENDMETHOD.
  
    "ALV按钮事件处理程序方法：删除或增加按钮
    METHOD HANDLE_ALV_TOOLBAR.
      "删除不需要的按钮
      DELETE E_OBJECT->MT_TOOLBAR
       WHERE FUNCTION = '&&SEP00'
         OR FUNCTION = '&REFRESH'
         OR FUNCTION = '&LOCAL&APPEND'
         OR FUNCTION = '&LOCAL&UNDO'
         OR FUNCTION ='&LOCAL&INSERT_ROW'
        OR FUNCTION ='&LOCAL&DELETE_ROW'
         OR FUNCTION = '&LOCAL&COPY_ROW'
         OR FUNCTION = '&&SEP01'
         OR FUNCTION = '&LOCAL&CUT'
         OR FUNCTION = '&LOCAL&COPY'
         OR FUNCTION = '&LOCAL&PASTE'
        OR FUNCTION = '&LOCAL&UNDO'
         OR FUNCTION = '&&SEP02'.
  
      "增加一个新按钮
      APPEND INITIAL LINE TO E_OBJECT->MT_TOOLBAR ASSIGNING FIELD-SYMBOL(<FS_BUTTON>).
      <FS_BUTTON>-BUTN_TYPE = '3'.  "分割符
      APPEND INITIAL LINE TO E_OBJECT->MT_TOOLBAR ASSIGNING <FS_BUTTON>.
      <FS_BUTTON>-FUNCTION = 'DELETE_ALL'.
      <FS_BUTTON>-ICON = '@18@'.
      <FS_BUTTON>-TEXT = '删除全部行'.
    ENDMETHOD.
    "实现自定义按钮事件处理方法
    METHOD HANDLE_USER_COMMAND.
      CASE E_UCOMM.
        WHEN 'DELETE_ALL'.
          IF GO_ZCL_PRO_GRREQ_BILL->BILL_STATUS <> ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CHANGED.
            RETURN.
          ELSE.
            LOOP AT GT_ZPPT0003 ASSIGNING FIELD-SYMBOL(<FS>).
              <FS>-ZDELFLG = ABAP_TRUE.
            ENDLOOP.
            GO_ALV_GRID_CREATE->REFRESH_TABLE_DISPLAY( IS_STABLE = GS_STABLE ).
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDMETHOD.
  
    "onf4搜索帮助实现
    METHOD HANDLE_ALV_ONF4.
      DATA LT_DDSHRETVAL TYPE TABLE OF DDSHRETVAL.
      DATA LV_MATNR TYPE MATNR.
      DATA(LV_ROWID) = ES_ROW_NO-ROW_ID. "ALV正在操作的行号
      CASE E_FIELDNAME.
        WHEN 'LGORT'  ."仓库
          READ TABLE GT_ZPPT0003 INDEX LV_ROWID ASSIGNING <FS_ZPPT0003>.
          LV_MATNR = <FS_ZPPT0003>-MATNR.
          SELECT MARD~LGORT,T001L~LGOBE FROM MARD
            LEFT JOIN T001L ON MARD~WERKS = T001L~WERKS AND MARD~LGORT = T001L~LGORT
            WHERE MARD~WERKS = @P_DWERK
            AND MARD~MATNR = @LV_MATNR
            INTO TABLE @DATA(LT_T001L).
  
          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
            EXPORTING
              RETFIELD        = 'LGORT' "返回字段
              VALUE_ORG       = 'S'
            TABLES
              VALUE_TAB       = LT_T001L
              RETURN_TAB      = LT_DDSHRETVAL
            EXCEPTIONS
              PARAMETER_ERROR = 1
              NO_VALUES_FOUND = 2
              OTHERS          = 3.
          IF SY-SUBRC = 0 AND LT_DDSHRETVAL IS NOT INITIAL.
            READ TABLE LT_T001L WITH KEY LGORT = LT_DDSHRETVAL[ 1 ]-FIELDVAL INTO GS_LGORT.
            IF SY-SUBRC = 0.
              <FS_ZPPT0003>-LGORT = GS_LGORT-LGORT.
              <FS_ZPPT0003>-LGOBE = GS_LGORT-LGOBE.
            ENDIF.
          ENDIF.
          "移动类型101不能输入样品仓库，Y01只能输入样品仓库.
          IF LT_DDSHRETVAL[] IS NOT INITIAL .
            CLEAR GV_MSG.
            GV_MSG =  CHECK_LGORT_BY_BWART( EXPORTING IV_BWART = ZSPP034_HEAD-BWART IV_LGORT = CONV LGORT_D( LT_DDSHRETVAL[ 1 ]-FIELDVAL ) ).
            IF GV_MSG IS NOT INITIAL.
              CLEAR:<FS_ZPPT0003>-LGORT, <FS_ZPPT0003>-LGOBE.
              MESSAGE GV_MSG TYPE 'S'.
            ENDIF.
          ENDIF.
        WHEN 'ZWSLGORT' ."车间仓库
  
          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
            EXPORTING
              RETFIELD        = 'LGORT' "返回字段
              VALUE_ORG       = 'S'
            TABLES
              VALUE_TAB       = GT_T001L
              RETURN_TAB      = LT_DDSHRETVAL
            EXCEPTIONS
              PARAMETER_ERROR = 1
              NO_VALUES_FOUND = 2
              OTHERS          = 3.
          IF SY-SUBRC = 0 AND LT_DDSHRETVAL IS NOT INITIAL.
            READ TABLE GT_T001L WITH KEY LGORT = LT_DDSHRETVAL[ 1 ]-FIELDVAL INTO GS_LGORT.
            IF SY-SUBRC = 0.
              GT_ZPPT0003[ LV_ROWID ]-ZWSLGORT = GS_LGORT-LGORT.
              GT_ZPPT0003[ LV_ROWID ]-ZWSLGOBE = GS_LGORT-LGOBE.
            ENDIF.
          ENDIF.
        WHEN 'CHARG'."批次
          READ TABLE GT_CHARG WITH KEY AUFNR = GT_ZPPT0003[ LV_ROWID ]-AUFNR INTO GS_CHARG.
          IF SY-SUBRC = 0 AND GS_CHARG-CHARG IS NOT INITIAL.
            GT_ZPPT0003[ LV_ROWID  ]-CHARG = GS_CHARG-CHARG.
            MESSAGE '流程订单已存在批号了,您修改的批号无效!' TYPE 'I'.
          ELSE."获取当前行物料的批次号
            LV_MATNR = GT_ZPPT0003[ LV_ROWID  ]-MATNR.
            SELECT MATNR,WERKS,CHARG FROM MCHA WHERE MATNR = @LV_MATNR AND WERKS = @P_DWERK INTO TABLE @DATA(LT_MCHA).
            IF LT_MCHA IS NOT INITIAL.
              CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
                EXPORTING
                  RETFIELD        = 'CHARG' "返回字段
                  VALUE_ORG       = 'S'
                TABLES
                  VALUE_TAB       = LT_MCHA
                  RETURN_TAB      = LT_DDSHRETVAL
                EXCEPTIONS
                  PARAMETER_ERROR = 1
                  NO_VALUES_FOUND = 2
                  OTHERS          = 3.
              IF SY-SUBRC = 0 AND LT_DDSHRETVAL IS NOT INITIAL.
                GT_ZPPT0003[ LV_ROWID  ]-CHARG = LT_DDSHRETVAL[ 1 ]-FIELDVAL.
              ENDIF.
  
            ENDIF.
          ENDIF.
  
        WHEN OTHERS.
      ENDCASE.
      ER_EVENT_DATA->M_EVENT_HANDLED = 'X'."终止后续的搜索帮助
      GO_ALV_GRID_CREATE->REFRESH_TABLE_DISPLAY( IS_STABLE = VALUE LVC_S_STBL( ROW = 'X' COL = 'X') I_SOFT_REFRESH = ABAP_TRUE  ).
    ENDMETHOD.
    "ALV单元格编辑后的处理程序
    METHOD HANDLE_DATA_CHANGED_FINISHED.
      DATA: LV_REFRESH TYPE CHAR1,
            LV_MESSAGE TYPE CHAR40,
            LV_AUFNR   TYPE AFPO-AUFNR,
            LV_LGORT   TYPE MARD-LGORT,
            L_KEY      TYPE CHAR90,
            LS_VALUE   TYPE BAPI1003_ALLOC_VALUES_CHAR,
            LT_VALUE   TYPE TABLE OF BAPI1003_ALLOC_VALUES_CHAR.
  
      LOOP AT ET_GOOD_CELLS INTO DATA(LS_GOOD_CELLS) WHERE   ERROR = ''.
        ASSIGN GT_ZPPT0003[ LS_GOOD_CELLS-ROW_ID ] TO <FS_ZPPT0003>.
        CHECK SY-SUBRC = 0.
        CASE LS_GOOD_CELLS-FIELDNAME.
          WHEN 'LGORT'."仓库
            CLEAR LV_LGORT.
  *          READ TABLE gt_t001l  WITH  KEY lgort = ls_good_cells-value INTO gs_lgort.
  *          IF sy-subrc = 0.
  *            <fs_zppt0003>-lgobe = gs_lgort-lgobe.
  *          ELSE.
  *            CLEAR: <fs_zppt0003>-lgort,<fs_zppt0003>-lgobe.
  *            MESSAGE '输入的仓库无效,请检查!' TYPE 'I'.
  *          ENDIF.
            "检查MMSC是否扩充物料工厂仓库地点
            SELECT SINGLE LGORT INTO LV_LGORT FROM MARD
              WHERE MATNR = <FS_ZPPT0003>-MATNR
              AND WERKS = <FS_ZPPT0003>-WERKS
              AND LGORT = LS_GOOD_CELLS-VALUE.
            IF SY-SUBRC = 0.
              READ TABLE GT_T001L  WITH  KEY LGORT = LS_GOOD_CELLS-VALUE INTO GS_LGORT.
              IF SY-SUBRC = 0.
                <FS_ZPPT0003>-LGOBE = GS_LGORT-LGOBE."仓库名称
              ENDIF.
              "移动类型101不能输入样品仓库，Y01只能输入样品仓库.
              CLEAR GV_MSG.
              GV_MSG =  CHECK_LGORT_BY_BWART( EXPORTING IV_BWART = ZSPP034_HEAD-BWART IV_LGORT = CONV LGORT_D( LS_GOOD_CELLS-VALUE ) ).
              IF GV_MSG IS NOT INITIAL.
                MESSAGE GV_MSG TYPE 'I'.
                CLEAR:<FS_ZPPT0003>-LGORT, <FS_ZPPT0003>-LGOBE.
              ENDIF.
            ELSE.
              MESSAGE '物料未扩充此仓库地点,请检查!' TYPE 'I'.
              CLEAR: <FS_ZPPT0003>-LGORT,<FS_ZPPT0003>-LGOBE.
            ENDIF.
          WHEN 'ZWSLGORT'."车间仓库
            READ TABLE GT_T001L  WITH  KEY LGORT = LS_GOOD_CELLS-VALUE INTO GS_LGORT.
            IF SY-SUBRC = 0.
              <FS_ZPPT0003>-ZWSLGOBE = GS_LGORT-LGOBE.
            ELSE.
              CLEAR: <FS_ZPPT0003>-ZWSLGORT,<FS_ZPPT0003>-ZWSLGOBE.
              MESSAGE '输入的车间仓库无效,请检查!' TYPE 'I'.
            ENDIF.
          WHEN 'ZJSMNG' OR 'ZWSMNG'. "申请件数、申请尾数
  *  输入申请件数或申请尾数后，计算申请数量，公式为：申请数量 = 申请件数 * 基本单位/PC + 申请尾数
            <FS_ZPPT0003>-MENGE = <FS_ZPPT0003>-ZUMREN * <FS_ZPPT0003>-ZJSMNG + <FS_ZPPT0003>-ZWSMNG.
          WHEN 'CHARG'. "批次号
            LV_AUFNR = <FS_ZPPT0003>-AUFNR.
            READ TABLE GT_CHARG WITH KEY AUFNR = LV_AUFNR  XCHPF = 'X' INTO GS_CHARG.
            IF SY-SUBRC = 0 .
              IF GS_CHARG-CHARG IS NOT INITIAL.
                <FS_ZPPT0003>-CHARG = GS_CHARG-CHARG.
                LV_MESSAGE = '流程订单已存在批号了,您修改的批号无效!'.
              ENDIF.
            ELSE.
              CLEAR  <FS_ZPPT0003>-CHARG.
              LV_MESSAGE = '物料未启用批次管理(工厂),不需要输入批号!'.
            ENDIF.
  
            IF LV_MESSAGE IS NOT INITIAL.
              MESSAGE LV_MESSAGE TYPE 'I'.
            ELSE.
              READ TABLE GT_PRO_OUTPUT WITH KEY AUFNR = LV_AUFNR  INTO GS_PRO_OUTPUT.
              IF SY-SUBRC = 0.
                TRY .
                    L_KEY(40) = <FS_ZPPT0003>-MATNR.
                    L_KEY+40(4) = ZSPP034_HEAD-WERKS.
                    L_KEY+44(10) = LS_GOOD_CELLS-VALUE.
                    IF GS_PRO_OUTPUT-MTART = 'Z050'.
                      LT_VALUE = ZCL_BC_PUBLIC=>GET_CLASS_VALUES( KEY = L_KEY CLASSNUM = 'BATCH_CP' CLASSTYPE = '022' OBJTABLE = 'MCHA' ).
                      READ TABLE LT_VALUE WITH KEY CHARACT = 'Z_BATCH_SCRQ' INTO LS_VALUE.
                      IF SY-SUBRC = 0.
                        <FS_ZPPT0003>-ZHSDAT = CONV #( LS_VALUE-VALUE_CHAR ).
                      ENDIF.
  
                      READ TABLE LT_VALUE WITH KEY CHARACT = 'Z_BATCH_YXQZ' INTO LS_VALUE.
                      IF SY-SUBRC = 0 AND LS_VALUE-VALUE_CHAR IS  NOT INITIAL.
                        <FS_ZPPT0003>-ZVFDAT = CONV #( LS_VALUE-VALUE_CHAR ).
                      ENDIF.
  
                      READ TABLE LT_VALUE WITH KEY CHARACT = 'Z_BATCH_DYPC' INTO LS_VALUE.
                      IF SY-SUBRC = 0 AND LS_VALUE-VALUE_CHAR IS  NOT INITIAL.
                        <FS_ZPPT0003>-ZPACK_CHARG = CONV #( LS_VALUE-VALUE_CHAR ).
                      ENDIF.
                    ELSE.
                      LT_VALUE = ZCL_BC_PUBLIC=>GET_CLASS_VALUES( KEY = L_KEY CLASSNUM = 'ZBATCH_SIRIO' CLASSTYPE = '022' OBJTABLE = 'MCHA' ).
                      READ TABLE LT_VALUE WITH KEY CHARACT = 'ZHSDAT' INTO LS_VALUE.
                      IF SY-SUBRC = 0.
                        <FS_ZPPT0003>-ZHSDAT = CONV #( LS_VALUE-VALUE_CHAR ).
                      ENDIF.
  
                      READ TABLE LT_VALUE WITH KEY CHARACT = 'ZVFDAT' INTO LS_VALUE.
                      IF SY-SUBRC = 0 .
                        <FS_ZPPT0003>-ZVFDAT = CONV #( LS_VALUE-VALUE_CHAR ).
                      ENDIF.
  
                      READ TABLE LT_VALUE WITH KEY CHARACT = 'ZPACK_CHARG' INTO LS_VALUE.
                      IF SY-SUBRC = 0 .
                        <FS_ZPPT0003>-ZPACK_CHARG = CONV #( LS_VALUE-VALUE_CHAR )."包装批号（打印）
                      ENDIF.
                    ENDIF.
                  CATCH CX_ROOT .
                ENDTRY.
              ENDIF.
            ENDIF.
          WHEN 'ZHSDAT'. "生产日期
            ZPPT0003 = <FS_ZPPT0003>.
            READ TABLE GT_CHARG WITH KEY AUFNR = ZPPT0003-AUFNR XCHPF = 'X' INTO GS_CHARG.
            IF SY-SUBRC <> 0.
              CLEAR <FS_ZPPT0003>-ZHSDAT.
              MESSAGE '物料未启用批次管理(工厂),不需要输入生产日期!' TYPE 'I'.
            ELSE.
              "计算无效剩余货架有效期
              <FS_ZPPT0003>-ZVFDAT = ZCL_PRO_GRREQ_BILL=>CAL_MARA_VFDAT( MATNR = ZPPT0003-MATNR START_DATE =  CONV VFDAT( LS_GOOD_CELLS-VALUE ) ).
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
      GO_ALV_GRID_CREATE->SET_FRONTEND_LAYOUT( GS_LAYOUT_BILL ).
      CALL METHOD GO_ALV_GRID_CREATE->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = GS_STABLE
        EXCEPTIONS
          FINISHED  = 1
          OTHERS    = 2.
    ENDMETHOD.
  
    "注册alv事件处理程序方法
    METHOD REGISTER_ALV_HANDLE_EVENT.
  
      SET HANDLER: GO_ZCL_PRO_GRREQ_BILL->HANDLE_ALV_TOOLBAR FOR GO_ALV_GRID_CREATE,
                   GO_ZCL_PRO_GRREQ_BILL->HANDLE_USER_COMMAND FOR GO_ALV_GRID_CREATE,
                    GO_ZCL_PRO_GRREQ_BILL->HANDLE_ALV_ONF4 FOR GO_ALV_GRID_CREATE,
                    GO_ZCL_PRO_GRREQ_BILL->HANDLE_DATA_CHANGED_FINISHED FOR GO_ALV_GRID_CREATE,
                    GO_ZCL_PRO_GRREQ_BILL->HANDLE_BILL_STATUS_CHANGED FOR GO_ZCL_PRO_GRREQ_BILL.
  
      "注册编辑事件，使ALV编辑后回车，或离开焦点时可以进行数据校验
      GO_ALV_GRID_CREATE->REGISTER_EDIT_EVENT( CL_GUI_ALV_GRID=>MC_EVT_MODIFIED ).
  
      "注册ALV字段onf4搜索事件
      DATA(LT_F4) = VALUE LVC_T_F4(  ( FIELDNAME = 'LGORT' REGISTER = 'X' GETBEFORE = 'X' CHNGEAFTER = 'X' INTERNAL = 'X' )
                                     ( FIELDNAME = 'ZWSLGORT' REGISTER = 'X' GETBEFORE = 'X' CHNGEAFTER = 'X' INTERNAL = 'X' )
                                     ( FIELDNAME = 'CHARG' REGISTER = 'X' GETBEFORE = 'X' CHNGEAFTER = 'X' INTERNAL = 'X' )
                                   ) .
  
      GO_ALV_GRID_CREATE->REGISTER_F4_FOR_FIELDS( IT_F4 = LT_F4 ).
    ENDMETHOD.
  
    "保存单据前的数据准备及检查,返回检查结果
    METHOD BEFORE_SAVE.
      DATA:LV_MENGE TYPE CO_PSMNG,
           LV_PSMNG TYPE CO_PSMNG.
      "检查输入的车间仓库编号
      READ TABLE GT_T001L  WITH  KEY LGORT = ZSPP034_HEAD-ZWSLGORT INTO GS_LGORT.
      IF SY-SUBRC <> 0.
        CHECK_RESULT-TYPE = 'E'.
        CHECK_RESULT-MESSAGE = '输入的车间仓库编号无效'.
        RETURN.
      ENDIF.
      IF ZSPP034_HEAD-BWART IS INITIAL.
        CHECK_RESULT-TYPE = 'E'.
        CHECK_RESULT-MESSAGE = '请输入移动类型'.
        RETURN.
      ENDIF.
      "移动类型101、Y01下发WMS时，需检查物料的标准价
      IF ZSPP034_HEAD-BWART = '101' OR ZSPP034_HEAD-BWART = 'Y01'.
        MOVE-CORRESPONDING GT_ZPPT0003 TO GT_MATNR.
        SORT GT_MATNR BY MATNR.
        DELETE ADJACENT DUPLICATES FROM GT_MATNR COMPARING MATNR.
  
        SELECT SINGLE BWKEY FROM T001W WHERE WERKS = @ZSPP034_HEAD-WERKS INTO @DATA(LV_BWKEY).
        SELECT MATNR,STPRS FROM MBEW FOR ALL ENTRIES IN @GT_MATNR
         WHERE MATNR = @GT_MATNR-MATNR
           AND BWKEY = @LV_BWKEY
           AND VPRSV = 'S'
  *         AND lfgja = @sy-datum+0(4)
  *         AND lfmon = @sy-datum+4(2)
          INTO TABLE @DATA(LT_MBEW).
  
  *      SELECT matnr , stprs FROM mbewh FOR ALL ENTRIES IN @gt_matnr
  *       WHERE matnr = @gt_matnr-matnr
  *        AND bwkey = @lv_bwkey
  *        AND vprsv = 'S'
  *        AND lfgja = @sy-datum+0(4)
  *        AND lfmon = @sy-datum+4(2)
  *        APPENDING TABLE @lt_mbew.
  
        SORT LT_MBEW BY MATNR.
      ENDIF.
  
      "获取单号
      IF ZSPP034_HEAD-ZSAPNOTE_NO IS  INITIAL.
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            NR_RANGE_NR             = '15'
            OBJECT                  = 'ZMM030'
          IMPORTING
            NUMBER                  = ZSPP034_HEAD-ZSAPNOTE_NO
          EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            BUFFER_OVERFLOW         = 7
            OTHERS                  = 8.
        IF SY-SUBRC <> 0 OR ZSPP034_HEAD-ZSAPNOTE_NO IS INITIAL.
          CHECK_RESULT-TYPE = 'E'.
          CHECK_RESULT-MESSAGE = '生成入库申请单编号失败'.
          RETURN.
        ENDIF.
      ENDIF.
  
      LOOP AT GT_ZPPT0003 ASSIGNING FIELD-SYMBOL(<FS>).
        DATA(LV_TABIX) = SY-TABIX.
        "移动类型101、Y01下发WMS时，需检查物料的标准价
        IF ZSPP034_HEAD-BWART = '101' OR ZSPP034_HEAD-BWART = 'Y01'.
          READ TABLE LT_MBEW WITH KEY MATNR = <FS>-MATNR BINARY SEARCH INTO DATA(LS_MBEW).
          IF SY-SUBRC <> 0 OR ( SY-SUBRC = 0 AND LS_MBEW-STPRS <= 0 ) .
            CHECK_RESULT-TYPE = 'E'.
            CHECK_RESULT-MESSAGE = |{ '第' }{ LV_TABIX }{ '行的物料' } { ZCL_BC_PUBLIC=>CONV_BY_DDIC( I_INPUT = <FS>-MATNR I_OUT = 'X' ) }{ '需要下发给WMS,但是未维护标准价格,请联系财务人员处理' }|.
            RETURN.
          ENDIF.
        ENDIF.
  
        IF <FS>-LGORT IS INITIAL.
          CHECK_RESULT-TYPE = 'E'.
          CHECK_RESULT-MESSAGE = |{ '第' }{ LV_TABIX }{ '行的仓库不能为空,请检查!' }|.
          RETURN.
        ENDIF.
  
        IF <FS>-MENGE IS INITIAL.
          CHECK_RESULT-TYPE = 'E'.
          CHECK_RESULT-MESSAGE = |{ '第' }{ LV_TABIX }{ '行的申请数量不能为空,请检查!' }|.
          RETURN.
        ENDIF.
        "( MENGE申请数量 + WEMNG已收货数量 )  不等大于 PSMNG计划数量 * ( 1 + UEBTO过量交货限度 / 100 )
        LV_MENGE = <FS>-MENGE + <FS>-WEMNG.
        LV_PSMNG = <FS>-PSMNG * ( 1 + <FS>-UEBTO / 100 ).
        IF LV_MENGE > LV_PSMNG.
          SELECT SINGLE UEBTK INTO @DATA(LV_UEBTK) FROM AFPO WHERE AUFNR = @<FS>-AUFNR.
          IF LV_UEBTK = 'X'.
            MESSAGE |{ '第' }{ LV_TABIX }{ '行超容差收货,' }{ '在无约束的超量发货情况下， 容差被忽略' }| TYPE 'W'.
          ELSE.
            CHECK_RESULT-TYPE = 'E'.
            CHECK_RESULT-MESSAGE = |{ '第' }{ LV_TABIX }{ '行的已收货+申请数量' }{ LV_MENGE }{ '不能超过订单最大入库量' }{ LV_PSMNG }|.
            RETURN.
          ENDIF.
        ENDIF.
        <FS>-ZSAPNOTE_NO = ZSPP034_HEAD-ZSAPNOTE_NO. "编号
        <FS>-BWART = ZSPP034_HEAD-BWART."移动类型
        <FS>-ZBKTXT = ZSPP034_HEAD-ZBKTXT."抬头备注
        <FS>-ERDAT = SY-DATUM.
        <FS>-ERZET  = SY-UZEIT.
        <FS>-UNAME = SY-UNAME.
        <FS>-ZWSLGORT = GS_LGORT-LGORT.
        <FS>-ZWSLGOBE = GS_LGORT-LGOBE.
  
        READ TABLE GT_CHARG WITH KEY AUFNR = <FS>-AUFNR INTO GS_CHARG.
        IF SY-SUBRC = 0.
          CASE GS_CHARG-XCHPF. "批次管理(工厂)
            WHEN ABAP_FALSE.
              IF <FS>-CHARG IS NOT  INITIAL OR <FS>-ZHSDAT IS NOT INITIAL  OR <FS>-ZVFDAT IS NOT INITIAL.
                CHECK_RESULT-TYPE = 'E'.
                CHECK_RESULT-MESSAGE = |{ '第' }{ LV_TABIX }{ '行的物料未启用批次管理(工厂),不能输入批号、生产日期、有效期,请检查!' }|.
                RETURN.
              ENDIF.
            WHEN ABAP_TRUE.
              IF <FS>-CHARG IS INITIAL OR <FS>-ZHSDAT IS  INITIAL OR <FS>-ZVFDAT IS  INITIAL.
                CHECK_RESULT-TYPE = 'E'.
                CHECK_RESULT-MESSAGE = |{ '第' }{ LV_TABIX }{ '行的物料已启用批次管理(工厂),批号、生产日期及有效期不能为空,请检查!' }|.
                RETURN.
              ENDIF.
            WHEN OTHERS.
          ENDCASE.
  
        ENDIF.
  
        "修改单据时，记录修改人
        IF GO_ZCL_PRO_GRREQ_BILL->BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CHANGED.
          <FS>-AENAM = SY-UNAME.
          <FS>-AEDAT  = SY-DATUM.
          <FS>-AEZEIT = SY-UZEIT.
        ENDIF.
  
      ENDLOOP.
  
  
      CHECK_RESULT-TYPE = 'S'.
      CHECK_RESULT-MESSAGE = '检查成功'.
    ENDMETHOD.
  
    "单据状态改变时的事件处理程序方法
    METHOD HANDLE_BILL_STATUS_CHANGED.
      "退出单据界面时，提示用户是否保存
      IF E_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CREATED OR E_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CHANGED.
        ME->PROMPT_WHEN_EXIT = ABAP_TRUE. "提示
      ELSE.
        ME->PROMPT_WHEN_EXIT = ABAP_FALSE. "不提示
      ENDIF.
  
  
      "切换ALV编辑模式
  *      IF me->bill_status = zcl_pro_grreq_bill=>cns_status_created OR me->bill_status =  zcl_pro_grreq_bill=>cns_status_changed.
  *        IF go_alv_grid_create->is_ready_for_input( ) = 0."显示状态
  *          go_alv_grid_create->set_ready_for_input( 1 )."切换显示状态
  *        ENDIF.
  *      ELSEIF me->bill_status = zcl_pro_grreq_bill=>cns_status_display.
  *
  *        IF go_alv_grid_create->is_ready_for_input( ) = 1."显示状态
  *          go_alv_grid_create->set_ready_for_input( 0 )."切换编辑状态
  *        ENDIF.
  *      ENDIF.
      IF GO_ZCL_PRO_GRREQ_BILL IS  BOUND AND  GO_ALV_GRID_CREATE IS BOUND.
        GO_ZCL_PRO_GRREQ_BILL->SET_ALV_FIELDCATELOG( GO_ZCL_PRO_GRREQ_BILL->BILL_STATUS )."设置ALV字段目录属性
        GO_ALV_GRID_CREATE->SET_FRONTEND_FIELDCATALOG( GT_ALV_FIDCAT ). "设置ALV界面字段
      ENDIF.
  
      "刷新ALV界面
      IF GO_ALV_GRID_CREATE IS  BOUND AND E_REFRESH_ALV = ABAP_TRUE .
        CALL METHOD GO_ALV_GRID_CREATE->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = GS_STABLE
  *         i_soft_refresh =
          EXCEPTIONS
            FINISHED  = 1
            OTHERS    = 2.
      ENDIF.
    ENDMETHOD.
    "设置单据状态
    METHOD SET_BILL_STATUS.
      ME->BILL_STATUS = IMP_BILL_STATUS.
      RAISE EVENT BILL_STATUS_CHANGED EXPORTING E_BILL_STATUS = IMP_BILL_STATUS ."单据状态改变时事件
    ENDMETHOD.
    "保存单据
    METHOD SAVE_BILL.
      "准备下发WMS数据
      DATA: LS_OUTPUT        TYPE ZMT_SAP2WMS_OTHER, "下发参数
            LS_INPUT         TYPE ZMT_SAP2WSM_OTHER_RET, "响应参数
            LS_ITEM          TYPE ZDT_SAP2WMS_OTHER_LIST_OF_OTH1, "行项目
            LV_XCLOSED_H     TYPE CHAR1, "整单删除，只下发表头数据给WMS标识
            LV_SEND_WMS      TYPE CHAR1, "下发WMS标识
            LV_MESSAGE       TYPE BAPI_MSG,
            LV_QMATV         TYPE MARC-QMATV,
            LV_ZSAPNOTE_NO   TYPE ZPPT0003-ZSAPNOTE_NO,
            LO_SEND_LIMS     TYPE REF TO ZCL_LIMS_OPER,
            LS_LIMS_RESPONSE TYPE ZSIGN_BATCH_SESSIONLESS_SOAP_O.
  
      "只下发移动类型(101,Y01)的数据
      ZPPT0003 = GT_ZPPT0003[ 1 ].
      LV_ZSAPNOTE_NO = ZPPT0003-ZSAPNOTE_NO.
      IF ZPPT0003-BWART = '101' OR ZPPT0003-BWART = 'Y01' .
        LV_SEND_WMS = ABAP_TRUE.
      ENDIF.
      IF LV_SEND_WMS = ABAP_TRUE.
        "抬头数据
        READ TABLE GT_ZPPT0003 WITH KEY ZDELFLG = '' INTO DATA(LS_ZPPT0003)."整单删除的判断逻辑
        IF SY-SUBRC <> 0.
          LV_XCLOSED_H = ABAP_TRUE."整单都被删除了，不需要下发行项目
        ENDIF.
        LS_OUTPUT-MT_SAP2WMS_OTHER-WMS_CUSTID = ZPPT0003-WERKS."WMS货主ID
        LS_OUTPUT-MT_SAP2WMS_OTHER-SAPNOTE_NO = ZPPT0003-ZSAPNOTE_NO."入库申请单号
        LS_OUTPUT-MT_SAP2WMS_OTHER-BUS_TYPE = 'A05'."业务类型
        LS_OUTPUT-MT_SAP2WMS_OTHER-WERKS = ZPPT0003-WERKS."工厂
        LS_OUTPUT-MT_SAP2WMS_OTHER-BWART = ZPPT0003-BWART."移动类型
        READ TABLE GT_VRM_VALUES WITH KEY KEY = ZPPT0003-BWART INTO DATA(LS_WRM_VALUES).
        IF SY-SUBRC = 0.
          LS_OUTPUT-MT_SAP2WMS_OTHER-BWTXT = LS_WRM_VALUES-TEXT."移动类型文本
        ENDIF.
        LS_OUTPUT-MT_SAP2WMS_OTHER-USNAM = ZPPT0003-UNAME."制单员
        LS_OUTPUT-MT_SAP2WMS_OTHER-NOTE = ZPPT0003-ZBKTXT."抬头文本
        LS_OUTPUT-MT_SAP2WMS_OTHER-XCLOSED_H = LV_XCLOSED_H."删除标志
        LS_OUTPUT-MT_SAP2WMS_OTHER-SHKZG = 'S'."出入库标记
  
        "行项目数据
        IF LV_XCLOSED_H <> ABAP_TRUE.
          CLEAR GT_MATNR.
          MOVE-CORRESPONDING GT_ZPPT0003 TO GT_MATNR.
          SORT GT_MATNR.
          DELETE ADJACENT DUPLICATES FROM GT_MATNR.
          SELECT MATNR,MTART FROM MARA FOR ALL ENTRIES IN @GT_MATNR
            WHERE MATNR = @GT_MATNR-MATNR INTO TABLE @DATA(LT_MARA).
          LOOP AT GT_ZPPT0003 INTO ZPPT0003 WHERE ZDELFLG = ABAP_FALSE.
            CLEAR LS_ITEM.
            LS_ITEM-SAPNOTE_LINE = ZPPT0003-ZSAPNOTE_LINE."行号
            LS_ITEM-MATNR = ZCL_BC_PUBLIC=>CONV_BY_DDIC( I_INPUT = ZPPT0003-MATNR I_OUT = 'X' )."物料编号
            LS_ITEM-MAKTX = ZPPT0003-MAKTX."物料描述
            LS_ITEM-CHARG = ZPPT0003-CHARG. "批号
            LS_ITEM-WMS_EXT_ST = '111'. "固定标识
            LS_ITEM-LINES_OF_CHARACTER = VALUE #( ( ATNAM = 'ZHSDAT' ATWRT = ZPPT0003-ZHSDAT )
                                                  ( ATNAM = 'ZLICHA' ATWRT = ZPPT0003-ZPACK_CHARG )
                                                  ( ATNAM = 'ZVFDAT' ATWRT = ZPPT0003-ZVFDAT )
                                                 )."批次特征值
            LS_ITEM-MENGE = ZPPT0003-MENGE. "申请数量
            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
              EXPORTING
                INPUT  = ZPPT0003-MEINS
              IMPORTING
                OUTPUT = GV_MEINS.
            LS_ITEM-MEINS = GV_MEINS. "单位
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = ZPPT0003-KDAUF
              IMPORTING
                OUTPUT = ZPPT0003-KDAUF.
            LS_ITEM-KDAUF = ZPPT0003-KDAUF.  "销售订单号
            IF ZPPT0003-KDPOS IS NOT INITIAL.
              LS_ITEM-KDPOS = ZPPT0003-KDPOS.  "销售订单行项目
            ENDIF.
            READ TABLE LT_MARA WITH KEY MATNR = ZPPT0003-MATNR INTO DATA(LS_MARA).
            IF  SY-SUBRC  = 0 AND ( LS_MARA-MTART = 'Z010' OR LS_MARA-MTART = 'Z050' ).
              IF LS_OUTPUT-MT_SAP2WMS_OTHER-BWART = '101'.
                "2021-01-05 新增逻辑：物料质量视图中的检验设置字段(QMATV)为空字符时，不能传输质检库存状态
                CLEAR LV_QMATV.
                SELECT SINGLE QMATV INTO LV_QMATV FROM MARC WHERE WERKS = ZPPT0003-WERKS AND MATNR = LS_MARA-MATNR.
                IF SY-SUBRC = 0.
                  IF LV_QMATV = ''.
                    LS_ITEM-STOCK_ST = ''.  "非限制库存状态
                  ELSE.
                    LS_ITEM-STOCK_ST = 'I'.  "质检库存状态
                  ENDIF.
                ENDIF.
              ELSEIF LS_OUTPUT-MT_SAP2WMS_OTHER-BWART = 'Y01' AND LS_MARA-MTART = 'Z050'.
                LS_ITEM-STOCK_ST = ''.  "非限制库存状态
              ENDIF.
            ENDIF.
            LS_ITEM-SHKZG = 'S'.  "入库/出库标记：S-入库，H-出库
            LS_ITEM-LGORT = ZPPT0003-ZWSLGORT.  "车间仓库
            LS_ITEM-DWERKS = ZPPT0003-WERKS.  "对方工厂
            LS_ITEM-DLGORT = ZPPT0003-LGORT.  "对方仓库
            LS_ITEM-AUFNR = ZPPT0003-AUFNR.  "生产订单
            LS_ITEM-SGTXT = ZPPT0003-WEMPF.  "备注1
            LS_ITEM-ZRSV01 = ZPPT0003-ABLAD.  "备注2
            LS_ITEM-ZRSV02 = ZPPT0003-ZJSMNG.  "申请件数
            LS_ITEM-ZRSV03 = ZPPT0003-ZWSMNG.  "申请尾数
            APPEND LS_ITEM TO LS_OUTPUT-MT_SAP2WMS_OTHER-LIST_OF_OTHERS.
          ENDLOOP.
        ENDIF.
      ENDIF.
      "接口开关检查
      CALL FUNCTION 'Z_PI_CK'
        EXPORTING
          INTF_ID  = 'MM026'
        IMPORTING
          ERR_INFO = LV_MESSAGE.
  *  接口未打开
      IF LV_MESSAGE  IS NOT INITIAL.
        RET_BAPIRET1-TYPE = 'E'.
        RET_BAPIRET1-MESSAGE = 'MM026下发WMS接口不打开'.
        RETURN.
      ENDIF.
      TRY .
          "下发给WMS
          DATA(LV_SAP_WMS_API) = NEW ZCO_SI_SAP2WMS_OTHER_OUT( )."下发接口代理类对象
          LV_SAP_WMS_API->SI_SAP2WMS_OTHER_OUT( EXPORTING OUTPUT  = LS_OUTPUT IMPORTING INPUT = LS_INPUT ).
          IF LS_INPUT-MT_SAP2WSM_OTHER_RET-RETURNFLAG = '1'."1-成功，0-全部失败，2-部分成功
            MODIFY ZPPT0003 FROM TABLE GT_ZPPT0003."插入或更新单据数据
            GO_ZCL_PRO_GRREQ_BILL->DEQUEUE_ALL_LOCK( ). "释放所有锁
            RET_BAPIRET1-TYPE = 'S'.
            RET_BAPIRET1-MESSAGE = | { '单号：' }{ LV_ZSAPNOTE_NO }{ '下发WMS成功' }|.
          ELSE.
            RET_BAPIRET1-TYPE = 'E'.
            RET_BAPIRET1-MESSAGE = | { '保存失败,下发WMS失败：' }{ LS_INPUT-MT_SAP2WSM_OTHER_RET-RETURNDESC }|.
          ENDIF.
        CATCH CX_ROOT INTO DATA(LS_ROOT).
          RET_BAPIRET1-TYPE = 'E'.
          RET_BAPIRET1-MESSAGE = | { '保存失败,下发WMS失败：' }{ LS_ROOT->GET_TEXT( ) }|.
      ENDTRY.
      "下发批结信息给LIMS
      IF ZPPT0003-BWART = '101' .
        LO_SEND_LIMS = NEW ZCL_LIMS_OPER( ).
        LS_LIMS_RESPONSE = LO_SEND_LIMS->SEND_TO_LIMS( ).
        IF LS_LIMS_RESPONSE-SIGN_BATCH_SESSIONLESS_RESULT-RET_CODE = 'S'.
          RET_BAPIRET1-TYPE = 'S'.
          RET_BAPIRET1-MESSAGE =  '批结信息下发LIMS成功'.
        ELSEIF LS_LIMS_RESPONSE-SIGN_BATCH_SESSIONLESS_RESULT-RET_MSG = '批结信息为空'.
          RET_BAPIRET1-TYPE = 'S'.
          RET_BAPIRET1-MESSAGE =  '批结信息为空,不需要下发LIMS'.
        ELSE.
          RET_BAPIRET1-TYPE = 'E'.
          RET_BAPIRET1-MESSAGE = |{ '批结信息下发LIMS失败,异常信息为：' }{ LS_LIMS_RESPONSE-SIGN_BATCH_SESSIONLESS_RESULT-RET_MSG }|.
        ENDIF.
      ENDIF.
      IF P_CREATE = 'X'.
        RAISE EVENT AFTER_SAVE EXPORTING ES_BAPIRET1 = RET_BAPIRET1."触发事件
      ENDIF.
    ENDMETHOD.
  
    "根据单据状态设置屏幕字段
    METHOD SET_SCREEN_FIELD_EDITABLE.
      LOOP AT SCREEN.
        CASE SCREEN-NAME.
          WHEN 'ZSPP034_HEAD-BWART' .
            IF IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CREATED .
              SCREEN-INPUT = 1.
            ELSE.
              SCREEN-INPUT = 0.
            ENDIF.
          WHEN  'ZSPP034_HEAD-ZBKTXT' OR 'ZSPP034_HEAD-ZWSLGORT'.
            IF IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CREATED OR IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CHANGED.
              SCREEN-INPUT = 1.
            ELSE.
              SCREEN-INPUT = 0.
            ENDIF.
  
          WHEN OTHERS.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
    ENDMETHOD.
    "根据单据状态设置ALV字段目录属性
    METHOD SET_ALV_FIELDCATELOG.
      LOOP AT GT_ALV_FIDCAT ASSIGNING FIELD-SYMBOL(<FS_FIDCAT>).
        CASE <FS_FIDCAT>-FIELDNAME.
          WHEN 'ZSAPNOTE_NO' OR 'BWART' OR 'ZBKTXT'  OR 'TYPE' OR 'MESSAGE' OR 'ZWSLGORT' OR 'ZWSLGOBE'  .
            <FS_FIDCAT>-TECH = ABAP_TRUE.
          WHEN 'LGORT'  ."仓库
            <FS_FIDCAT>-F4AVAILABL = ABAP_TRUE.
            IF IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CREATED OR IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CHANGED.
              <FS_FIDCAT>-EDIT = ABAP_TRUE.
            ELSE.
              <FS_FIDCAT>-EDIT = ABAP_FALSE.
            ENDIF.
          WHEN 'PSMNG'.
            <FS_FIDCAT>-SCRTEXT_S = '订单数量'.
            <FS_FIDCAT>-SCRTEXT_M = '订单数量'.
            <FS_FIDCAT>-SCRTEXT_L = '订单数量'.
          WHEN 'ZWMSMNG'.
            IF IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CREATED .
              <FS_FIDCAT>-NO_OUT = ABAP_TRUE.
            ELSE.
              <FS_FIDCAT>-NO_OUT =  ABAP_FALSE.
            ENDIF.
          WHEN 'MENGE' OR 'ZJSMNG' OR 'ZWSMNG' OR 'ZHSDAT' OR 'ZVFDAT'. "申请数量、申请件数、申请尾数、生产日期、失效期
            IF IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CREATED OR IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CHANGED.
              <FS_FIDCAT>-EDIT = ABAP_TRUE.
            ELSE.
              <FS_FIDCAT>-EDIT = ABAP_FALSE.
            ENDIF.
          WHEN 'CHARG'. "批号
            <FS_FIDCAT>-SCRTEXT_S = '流程订单批号'.
            <FS_FIDCAT>-SCRTEXT_M = '流程订单批号'.
            <FS_FIDCAT>-SCRTEXT_L = '流程订单批号'.
            <FS_FIDCAT>-EDIT = ABAP_TRUE.
  *          <fs_fidcat>-ref_table = 'AFPO'.
  *          <fs_fidcat>-ref_field = 'CHARG'.
            IF IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CREATED OR IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CHANGED.
              <FS_FIDCAT>-EDIT = ABAP_TRUE.
            ELSE.
              <FS_FIDCAT>-EDIT = ABAP_FALSE.
            ENDIF.
          WHEN 'WEMPF'.
            "<fs_fidcat>-edit = abap_true.
            <FS_FIDCAT>-DRDN_HNDL = '1'.
            <FS_FIDCAT>-OUTPUTLEN = 15.
            <FS_FIDCAT>-DRDN_ALIAS = ABAP_TRUE.
  *Field 'checktable' is set to avoid shortdumps that are caused
  * by inconsistend data in check tables. You may comment this out
  * when the test data of the flight model is consistent in your system.
            "  <fs_fidcat>-checktable = abap_false.        "do not check foreign keys
            IF IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CREATED OR IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CHANGED.
              <FS_FIDCAT>-EDIT = ABAP_TRUE.
            ELSE.
              <FS_FIDCAT>-EDIT = ABAP_FALSE.
            ENDIF.
          WHEN 'ABLAD'.
            <FS_FIDCAT>-SCRTEXT_L = '备注2'.
            <FS_FIDCAT>-SCRTEXT_M = '备注2'.
            <FS_FIDCAT>-SCRTEXT_S = '备注2'.
            IF IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CREATED OR IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CHANGED.
              <FS_FIDCAT>-EDIT = ABAP_TRUE.
            ELSE.
              <FS_FIDCAT>-EDIT = ABAP_FALSE.
            ENDIF.
          WHEN 'ZDELFLG'.
            <FS_FIDCAT>-CHECKBOX = ABAP_TRUE.
            IF  IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CHANGED.
              <FS_FIDCAT>-EDIT = ABAP_TRUE.
            ELSE.
              <FS_FIDCAT>-EDIT = ABAP_FALSE.
            ENDIF.
  *add S4DK904849 start
          WHEN 'ZJINTGEW' OR 'ZNTGEW' OR 'ZBRGEW' OR 'ZWJNTGEW' OR 'ZWNTGEW' OR 'ZWBRGEW'. "净净重、净重、毛重、尾数箱净净重、净重、毛重
            IF IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CREATED OR IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CHANGED.
              <FS_FIDCAT>-EDIT = ABAP_TRUE.
            ELSE.
              <FS_FIDCAT>-EDIT = ABAP_FALSE.
            ENDIF.
  *add S4DK904849 end
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
  
  
  
    ENDMETHOD.
  
    "入库申请单加锁
    METHOD ENQUEUE_ZSAPNOTE_NO_LOCK.
      "已加锁检查
  *    DATA: lv_garg TYPE seqg3-garg,
  *          lt_enq  TYPE TABLE OF seqg3.
  *    lv_garg = |{ sy-mandt }{ billno }{ '####' }|.  "键值由锁参数关键字组成
  *    CALL FUNCTION 'ENQUEUE_READ'
  *      EXPORTING
  *        gname                 = 'ZPPT0003'
  *        garg                  = lv_garg   "键值由锁参数关键字组成
  *        guname                = ''
  *      TABLES
  *        enq                   = lt_enq
  *      EXCEPTIONS
  *        communication_failure = 1
  *        system_failure        = 2
  *        OTHERS                = 3.
  *    IF lt_enq IS NOT INITIAL.
  *      READ TABLE lt_enq WITH KEY gobj = 'EZ_PP_ZPPT0003' INTO DATA(ls_enq).
  *      IF sy-subrc = 0.
  *        MESSAGE |{ '用户' }{ ls_enq-guname }{ '从' }{ ls_enq-gtdate DATE = ISO } { ls_enq-gttime TIME = ISO }{ '起正在修改此单了!' }| TYPE 'E'.
  *      ENDIF.
  *    ENDIF.
  
      "加锁
      CALL FUNCTION 'ENQUEUE_EZ_PP_ZPPT0003'
        EXPORTING
          MODE_ZPPT0003  = 'E'
          MANDT          = SY-MANDT
          ZSAPNOTE_NO    = BILLNO
  *       X_ZSAPNOTE_NO  = ' '
  *       _SCOPE         = '2'
  *       _WAIT          = ' '
  *       _COLLECT       = ' '
        EXCEPTIONS
          FOREIGN_LOCK   = 1
          SYSTEM_FAILURE = 2
          OTHERS         = 3.
      IF SY-SUBRC <> 0.
        MESSAGE ID SYST-MSGID TYPE SYST-MSGTY NUMBER SYST-MSGNO
             WITH  SYST-MSGV1  SYST-MSGV2  SYST-MSGV3  SYST-MSGV4.
      ENDIF.
  
    ENDMETHOD.
  
  ENDCLASS.
  
  CLASS ZCL_PRO_GRREQ DEFINITION.
    PUBLIC SECTION.
      CLASS-DATA:
        CNS_ACTVT_CREATE  TYPE ACTIV_AUTH VALUE '01', "作业类型-创建
        CNS_ACTVT_CHANGE  TYPE ACTIV_AUTH VALUE '02', "作业类型-修改
        CNS_ACTVT_DISPLAY TYPE ACTIV_AUTH VALUE '03', "作业类型-显示
        CNS_ACTVT_PRINT   TYPE ACTIV_AUTH VALUE '04'. "作业类型-打印
      CLASS-METHODS:
  *      set_initial_value,
        USER_AUTHORITY_CHECK IMPORTING WERKS TYPE WERKS_D ACTVT TYPE ACTIV_AUTH,"用户权限对象检查,传入工厂及作业类型
        GET_DATA,"从数据库读取报表数据
        ACTIVE_SELECTION_CRITERION,"激活选择屏幕字段
        SET_SELECTION_CRITERION, "设置选择条件值
        SHOW_ALV, "显示ALV
        ENQUEUE_AUFNR_LOCK IMPORTING IT_AUFNR TYPE T_AUFNR,"流程订单号加锁
        ON_PRO_SALV_USER_COMMAND   FOR EVENT ADDED_FUNCTION OF CL_SALV_EVENTS_TABLE IMPORTING E_SALV_FUNCTION, "pro_salv自定义按钮事件处理程序
        HANDLE_AFTER_SAVE          FOR EVENT AFTER_SAVE OF ZCL_PRO_GRREQ_BILL IMPORTING ES_BAPIRET1, "入库申请单保存后的事件
        HANDLE_DOUBLE_CLICK        FOR EVENT DOUBLE_CLICK OF CL_SALV_EVENTS_TABLE IMPORTING ROW COLUMN. "双击单元格事件
    PRIVATE SECTION.
      CLASS-METHODS:GET_CREATE_DATA,
        GET_QUERY_DATA.
  ENDCLASS.
  CLASS ZCL_PRO_GRREQ IMPLEMENTATION.
  *  METHOD set_initial_value.
  *    gs_layout_bill = VALUE lvc_s_layo( cwidth_opt = abap_true sel_mode = 'D' ).
  *    gs_stable = VALUE lvc_s_stbl( row = abap_true col = abap_true ).
  *    gt_vrm_values = VALUE vrm_values( ( key = '101' text = '流程订单收货' )
  *                                            ( key = '102' text = '流程订单取消收货' )
  *                                            ( key = 'Y01' text = '流程订单样品收货' )
  *                                            ( key = 'Y02' text = '流程订单样品取消收货' ) ).
  *  ENDMETHOD.
  *  用户权限检查
    METHOD USER_AUTHORITY_CHECK.
      DATA LV_ACTVT TYPE CHAR10.
      AUTHORITY-CHECK OBJECT 'ZPP001'
       ID 'WERKS' FIELD WERKS
       ID 'ACTVT' FIELD ACTVT.
      IF SY-SUBRC <> 0.
        CASE ACTVT.
          WHEN CNS_ACTVT_CREATE.
            LV_ACTVT = '创建'.
          WHEN CNS_ACTVT_CHANGE.
            LV_ACTVT = '修改'.
          WHEN CNS_ACTVT_DISPLAY.
            LV_ACTVT = '显示'.
          WHEN CNS_ACTVT_PRINT.
            LV_ACTVT = '打印'.
          WHEN OTHERS.
        ENDCASE.
        MESSAGE |{ '您没有流程订单入库申请单' }{ WERKS }{ '工厂的' }{ LV_ACTVT }{ '权限!' }| TYPE 'E'.
      ENDIF.
    ENDMETHOD.
  *  激活选择屏幕字段
    METHOD ACTIVE_SELECTION_CRITERION.
      LOOP AT SCREEN.
        IF P_CREATE = 'X'.
          IF SCREEN-GROUP1 = 'M1'.
            SCREEN-ACTIVE = '1'.
          ELSEIF SCREEN-GROUP1 = 'M2' OR SCREEN-GROUP1 = 'M3'.
            SCREEN-ACTIVE = '0'.
          ENDIF.
        ELSEIF P_QUERY = 'X'.
          IF SCREEN-GROUP1 = 'M2'.
            SCREEN-ACTIVE = '1'.
          ELSEIF SCREEN-GROUP1 = 'M1' OR SCREEN-GROUP1 = 'M3'.
            SCREEN-ACTIVE = '0'.
          ENDIF.
        ELSEIF P_RQR = 'X'.
          IF SCREEN-GROUP1 = 'M2' OR SCREEN-GROUP1 = 'M3'.
            SCREEN-ACTIVE = '1'.
          ELSEIF SCREEN-GROUP1 = 'M1'.
            SCREEN-ACTIVE = '0'.
          ENDIF.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    ENDMETHOD.
  *  设置选择条件
    METHOD SET_SELECTION_CRITERION.
      CASE 'X'.
        WHEN P_CREATE.
          USER_AUTHORITY_CHECK( WERKS = P_DWERK ACTVT = CNS_ACTVT_CREATE )."创建权限检查
          IF P_STTXT = 'X'.
            GR_IDAT = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = '00000000' ) )."排除TECO状态的订单条件
          ENDIF.
  
        WHEN P_QUERY.
          USER_AUTHORITY_CHECK( WERKS = P_DWERK ACTVT = CNS_ACTVT_DISPLAY )."显示权限检查
          "不显示删除行
          IF  P_DEL = ABAP_FALSE.
            GR_ZDELFLG = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = '' ) ).
          ENDIF.
        WHEN P_RQR.
          CLEAR GS_ZQRSCANAUTH.
          AUTHORITY-CHECK OBJECT 'ZPP005' ID 'WERKS' FIELD P_DWERK ID 'ZQRSCAN' FIELD '1'. "查询
          IF SY-SUBRC <> 0.
            MESSAGE E000 WITH |{ '您没有' }{ P_DWERK }{ '工厂二维码扫描查询的权限' }| DISPLAY LIKE 'S'.
          ENDIF.
          GS_ZQRSCANAUTH-QUERY_FLAG = ABAP_TRUE."有查询权限
          AUTHORITY-CHECK OBJECT 'ZPP005' ID 'WERKS' FIELD P_DWERK ID 'ZQRSCAN' FIELD '2'. "上传
          IF SY-SUBRC = 0.
            GS_ZQRSCANAUTH-UPLOAD_FLAG = ABAP_TRUE."有上传权限
          ENDIF.
          AUTHORITY-CHECK OBJECT 'ZPP005' ID 'WERKS' FIELD P_DWERK ID 'ZQRSCAN' FIELD '3'. "审批
          IF SY-SUBRC = 0.
            GS_ZQRSCANAUTH-AUDIT_FLAG  = ABAP_TRUE."有审批权限
          ENDIF.
          AUTHORITY-CHECK OBJECT 'ZPP005' ID 'WERKS' FIELD P_DWERK ID 'ZQRSCAN' FIELD '4'. "清空文本
          IF SY-SUBRC = 0.
            GS_ZQRSCANAUTH-EMPTY_FLAG = ABAP_TRUE."有上传权限
          ENDIF.
          AUTHORITY-CHECK OBJECT 'ZPP005' ID 'WERKS' FIELD P_DWERK ID 'ZQRSCAN' FIELD '5'. "发送邮件及保存
          IF SY-SUBRC = 0.
            GS_ZQRSCANAUTH-SAVE_FLAG = ABAP_TRUE."有上传权限
          ENDIF.
          AUTHORITY-CHECK OBJECT 'ZPP005' ID 'WERKS' FIELD P_DWERK ID 'ZQRSCAN' FIELD '6'. "检验合格
          IF SY-SUBRC = 0.
            GS_ZQRSCANAUTH-HEGE_FLAG = ABAP_TRUE."有上传权限
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDMETHOD.
    METHOD GET_CREATE_DATA.
  *获取流程订单表头数据
      SELECT
      AFPO~DWERK, " 工厂
      AFPO~DAUAT, " 订单类型
      AFPO~AUFNR, " 订单号
      AUFK~KTEXT, " 描述
  *        afko~rsnum,  "预留
      AFPO~MATNR, " 物料
      AFPO~MEINS, " 基本计量单位
      AFPO~UNTTO, "交货不足限度
      AFPO~UEBTO, "过量交货限度
      AFPO~PSMNG, " 订单数量
      AFKO~IGMNG, " 确认的产量
      AFPO~WEMNG, " 收货数量
      AFPO~LGORT,  "仓库
      AFPO~KDAUF, " 销售凭证
      AFPO~KDPOS, " 销售凭证项目
      AFPO~ELIKZ, " 交货已完成
      AFKO~GSTRP, " 基本开始日期
      AFKO~GLTRP, " 基本完成日期
      AFKO~FTRMI, " 实际下达日期
      AUFK~IDAT2,"技术关闭日期
      AFPO~CHARG, " 批次
      AFPO~WEMPF,  "备注1-交货日期
      AFPO~ABLAD,  "备注2
      AFKO~DISPO, " MRP控制者
      AFKO~FEVOR, " 生产管理员
      AUFK~OBJNR  "对象号
      FROM AFPO
      INNER JOIN AFKO ON AFPO~AUFNR = AFKO~AUFNR
      INNER JOIN AUFK ON  AFKO~AUFNR = AUFK~AUFNR
      WHERE AFPO~AUFNR IN @S_AUFNR  "流程订单号
      AND AFPO~DWERK = @P_DWERK "工厂
      AND AFPO~MATNR IN @S_MATNR "物料
      AND AFPO~KDAUF IN @S_KDAUF  "销售订单
      AND AFPO~KDPOS IN @S_KDPOS  "销售订单项
      AND AFPO~VERID IN @S_VERID  "生产版本
      AND AFPO~CHARG IN @S_CHARG   "批次号
      AND AFPO~LGORT IN @S_LGORT  "仓库
      AND AUFK~AUTYP = 40 "订单类别40 为流程订单
      AND AUFK~IDAT2 IN @GR_IDAT "技术完成日期
      AND AUFK~LOEKZ = '' " 删除标志
      AND AFKO~GSTRP IN @S_GSTRP  " 基本开始日期
      AND AFKO~FEVOR IN @S_FEVOR "生产管理员
      INTO CORRESPONDING FIELDS OF TABLE @GT_PRO_OUTPUT.
  
      IF GT_PRO_OUTPUT IS NOT INITIAL.
        MOVE-CORRESPONDING GT_PRO_OUTPUT TO GT_MATNR.
        SORT GT_MATNR.
        DELETE ADJACENT DUPLICATES FROM GT_MATNR.
  
        SELECT MARA~MATNR,MARA~MATKL,MARA~MTART,MARA~GEWEI,MAKT~MAKTX,T023T~WGBEZ "add S4DK904849
          FROM MARA
          LEFT JOIN MAKT ON MARA~MATNR = MAKT~MATNR AND MAKT~SPRAS = @SY-LANGU
          LEFT JOIN T023T ON MARA~MATKL = T023T~MATKL AND T023T~SPRAS = @SY-LANGU
           FOR ALL ENTRIES IN @GT_MATNR
          WHERE  MARA~MATNR = @GT_MATNR-MATNR
          INTO TABLE @DATA(LT_MARDESC).
  
        SELECT  LGORT,LGOBE
          FROM T001L
          WHERE WERKS = @P_DWERK
          INTO TABLE @GT_T001L.
  
        MOVE-CORRESPONDING GT_PRO_OUTPUT TO GT_KDAUF.
        DELETE GT_KDAUF WHERE TABLE_LINE IS INITIAL.
        SORT GT_KDAUF BY KDAUF KDPOS.
        DELETE ADJACENT DUPLICATES FROM GT_KDAUF COMPARING KDAUF KDPOS.
  
        SELECT VBELN,POSNR,ZYPSL FROM VBAP FOR ALL ENTRIES IN @GT_KDAUF
          WHERE VBELN = @GT_KDAUF-KDAUF
           AND  POSNR = @GT_KDAUF-KDPOS
          INTO TABLE @DATA(LT_VBAP).
  
        LOOP AT GT_PRO_OUTPUT ASSIGNING FIELD-SYMBOL(<FS_PRO>).
          READ TABLE LT_MARDESC WITH KEY MATNR = <FS_PRO>-MATNR INTO DATA(LS_MARDESC).
          IF SY-SUBRC = 0.
            <FS_PRO>-MAKTX = LS_MARDESC-MAKTX.
            <FS_PRO>-MTART = LS_MARDESC-MTART.
            <FS_PRO>-MATKL = LS_MARDESC-MATKL.
            <FS_PRO>-WGBEZ = LS_MARDESC-WGBEZ.
            <FS_PRO>-GEWEI = LS_MARDESC-GEWEI.
          ENDIF.
  
          READ TABLE GT_T001L WITH KEY LGORT = <FS_PRO>-LGORT INTO DATA(LS_T001L).
          IF SY-SUBRC = 0.
            <FS_PRO>-LGOBE = LS_T001L-LGOBE.
          ENDIF.
  
          IF <FS_PRO>-KDAUF IS NOT INITIAL.
            READ TABLE LT_VBAP WITH KEY VBELN = <FS_PRO>-KDAUF POSNR = <FS_PRO>-KDPOS INTO DATA(LS_VBAP).
            IF SY-SUBRC = 0.
              <FS_PRO>-ZYPSL = LS_VBAP-ZYPSL.
            ENDIF.
          ENDIF.
  
          "设置流程订单状态
          CALL FUNCTION 'STATUS_TEXT_EDIT'
            EXPORTING
              OBJNR            = <FS_PRO>-OBJNR
              SPRAS            = SY-LANGU
            IMPORTING
              LINE             = <FS_PRO>-STTXT
            EXCEPTIONS
              OBJECT_NOT_FOUND = 1
              OTHERS           = 2.
        ENDLOOP.
      ENDIF.
    ENDMETHOD.
    METHOD GET_QUERY_DATA.
      DATA:LT_KDAUF LIKE TABLE OF GS_KDAUF.
      SELECT
      T1~MANDT,T1~ZSAPNOTE_NO,T1~ZSAPNOTE_LINE, T1~BUDAT,T1~WERKS,T1~BWART,T1~ZBKTXT,T1~AUFNR,T1~MATNR,T1~MAKTX,
      T1~ZSPEC,T1~ZPSPE,T1~ZUMREN,T1~LGORT,T1~LGOBE,T1~ZWSLGORT,T1~ZWSLGOBE,T1~PSMNG,T2~WEMNG,T1~ZWMSMNG,T1~MENGE,
      T1~ZJSMNG,T1~ZWSMNG,T1~MEINS,T1~UNTTO,T1~UEBTO,T1~CHARG,T1~ZPACK_CHARG,T1~ZHSDAT,T1~ZVFDAT,T1~KDAUF,T1~KDPOS,
      T1~ZYPSL,T1~WEMPF,T1~ABLAD,T1~ZDELFLG,T1~ERDAT,T1~ERZET,T1~UNAME,T1~TYPE,T1~MESSAGE,T1~AENAM,T1~AEDAT,T1~AEZEIT,
  *add S4DK904849 start
            T1~ZJINTGEW,
            T1~ZNTGEW,
            T1~ZBRGEW,
            T1~ZWJNTGEW,
            T1~ZWNTGEW,
            T1~ZWBRGEW,
            T1~GEWEI
  *add S4DK904849 end
      FROM ZPPT0003 AS T1
      LEFT JOIN AFPO AS T2  ON T1~AUFNR = T2~AUFNR
      WHERE T1~ZSAPNOTE_NO IN @S_REQNO
      AND T1~WERKS = @P_DWERK
      AND T1~AUFNR IN @S_AUFNR
      AND T1~MATNR IN @S_MATNR
      AND T1~CHARG IN @S_CHARG
      AND T1~UNAME IN @S_UNAME
      AND T1~ZDELFLG IN @GR_ZDELFLG
      AND T1~BUDAT IN @S_BUDAT
      INTO CORRESPONDING FIELDS OF TABLE @GT_ZPPT0003_LIST.
      CHECK GT_ZPPT0003_LIST IS NOT INITIAL.
      "物料组、物料组描述
      SELECT MARA~MATNR,MARA~MATKL,T023T~WGBEZ
              FROM MARA
              LEFT JOIN T023T ON MARA~MATKL = T023T~MATKL AND T023T~SPRAS = @SY-LANGU
               FOR ALL ENTRIES IN @GT_ZPPT0003_LIST
              WHERE  MARA~MATNR = @GT_ZPPT0003_LIST-MATNR
              INTO TABLE @DATA(LT_MARDESC).
      MOVE-CORRESPONDING GT_ZPPT0003_LIST TO LT_KDAUF.
      SORT LT_KDAUF BY KDAUF KDPOS.
      DELETE ADJACENT DUPLICATES FROM LT_KDAUF COMPARING KDAUF KDPOS.
      IF LT_KDAUF IS NOT INITIAL.
        SELECT VBELN AS KDAUF,POSNR AS KDPOS,ZSFXBZ
         FROM VBAP FOR ALL ENTRIES IN @LT_KDAUF
        WHERE  VBELN = @LT_KDAUF-KDAUF
          AND POSNR = @LT_KDAUF-KDPOS
          INTO TABLE @DATA(LT_VBAP1).
        "销售订单的实际客户的属性1描述
        DELETE ADJACENT DUPLICATES FROM LT_KDAUF COMPARING KDAUF.
        SELECT VBELN, KUNNR FROM VBPA FOR ALL ENTRIES IN @LT_KDAUF
          WHERE VBELN = @LT_KDAUF-KDAUF
          AND POSNR IS INITIAL
          AND PARVW = 'Z1'
          INTO TABLE @DATA(LT_VBPA).
        SELECT V~VBELN AS KDAUF, TVK1T~VTEXT
          FROM @LT_VBPA AS V
          INNER JOIN KNA1 ON V~KUNNR = KNA1~KUNNR
          INNER JOIN TVK1T ON KNA1~KATR1 = TVK1T~KATR1 AND TVK1T~SPRAS = @SY-LANGU
          INTO TABLE @DATA(LT_KNA1).
        SORT LT_VBAP1 BY KDAUF KDPOS.
        SORT LT_KNA1 BY KDAUF.
      ENDIF.
      SORT LT_MARDESC BY MATNR.
      LOOP AT GT_ZPPT0003_LIST ASSIGNING FIELD-SYMBOL(<FS_ZPPT0003_LIST>).
        READ TABLE LT_MARDESC WITH KEY MATNR = <FS_ZPPT0003_LIST>-MATNR BINARY SEARCH INTO DATA(LS_MARDESC).
        IF SY-SUBRC = 0.
          <FS_ZPPT0003_LIST>-MATKL = LS_MARDESC-MATKL.
          <FS_ZPPT0003_LIST>-WGBEZ = LS_MARDESC-WGBEZ.
        ENDIF.
        READ TABLE LT_VBAP1 WITH KEY KDAUF = <FS_ZPPT0003_LIST>-KDAUF KDPOS = <FS_ZPPT0003_LIST>-KDPOS BINARY SEARCH INTO DATA(LS_VBAP1).
        IF SY-SUBRC = 0.
          <FS_ZPPT0003_LIST>-ZSFXBZ = LS_VBAP1-ZSFXBZ.
        ENDIF.
        READ TABLE LT_KNA1 WITH KEY KDAUF = <FS_ZPPT0003_LIST>-KDAUF  BINARY SEARCH INTO DATA(LS_KNA1).
        IF SY-SUBRC  = 0.
          <FS_ZPPT0003_LIST>-VTEXT = LS_KNA1-VTEXT.
        ENDIF.
      ENDLOOP.
      "存储仓库地点
      SELECT  LGORT,LGOBE
        FROM T001L
        WHERE WERKS = @P_DWERK
        INTO TABLE @GT_T001L.
    ENDMETHOD.
  
  *  根据选择条件读取流程订单或入库申请单
    METHOD GET_DATA.
      CASE 'X'."点击哪个radiobutton
        WHEN P_CREATE. "创建
          GET_CREATE_DATA( ).
        WHEN P_QUERY.  "维护
          GET_QUERY_DATA( ).
        WHEN OTHERS.
      ENDCASE.
  
    ENDMETHOD.
    "流程订单号加锁
    METHOD ENQUEUE_AUFNR_LOCK.
      IF IT_AUFNR IS NOT INITIAL.
        "已加锁检查
        DATA: LV_LOCK_ERROR TYPE FLAG,
              LV_GARG       TYPE SEQG3-GARG,
              LT_ENQ        TYPE TABLE OF SEQG3.
        LOOP AT IT_AUFNR INTO DATA(LV_AUFNR).
          LV_GARG = |{ SY-MANDT }{ LV_AUFNR }{ '0001' }|.  "键值由锁参数关键字组成
          CALL FUNCTION 'ENQUEUE_READ'
            EXPORTING
              GNAME                 = 'AFPO'
              GARG                  = LV_GARG   "键值由锁参数关键字组成
              GUNAME                = ''
            TABLES
              ENQ                   = LT_ENQ
            EXCEPTIONS
              COMMUNICATION_FAILURE = 1
              SYSTEM_FAILURE        = 2
              OTHERS                = 3.
          IF LT_ENQ IS NOT INITIAL.
            READ TABLE LT_ENQ WITH KEY GOBJ = 'EZ_PP_AFPO' INTO DATA(LS_ENQ).
            IF SY-SUBRC = 0.
              LV_AUFNR = |{ LV_AUFNR  ALPHA = OUT }|.
              CONDENSE LV_AUFNR NO-GAPS.
              MESSAGE |{ '流程订单' }{ LV_AUFNR }{ '已被用户' }{ LS_ENQ-GUNAME }{ '在' }{ LS_ENQ-GTDATE DATE = ISO } { LS_ENQ-GTTIME TIME = ISO }{ '引用创建入库申请单了' }| TYPE 'E'.
            ENDIF.
          ENDIF.
        ENDLOOP.
  
        "加锁
        LOOP AT IT_AUFNR INTO LV_AUFNR.
          CALL FUNCTION 'ENQUEUE_EZ_PP_AFPO'
            EXPORTING
              MODE_AFPO      = 'S'
              MANDT          = SY-MANDT
              AUFNR          = LV_AUFNR
              POSNR          = '0001'
  *           X_AUFNR        = ' ''
  *           X_POSNR        = ' '
  *           _SCOPE         = '2'
  *           _WAIT          = ' '
              _COLLECT       = 'X'  "收集后统一提交
            EXCEPTIONS
              FOREIGN_LOCK   = 1
              SYSTEM_FAILURE = 2
              OTHERS         = 3.
          IF SY-SUBRC <> 0..
            LV_LOCK_ERROR = 'X'.
            EXIT.
          ENDIF.
        ENDLOOP.
  
        IF LV_LOCK_ERROR = ''.
          "使Lock Container中的缓存锁信息一次性更新到锁管理系统中
          CALL FUNCTION 'FLUSH_ENQUEUE'
            EXCEPTIONS
              FOREIGN_LOCK   = 1
              SYSTEM_FAILURE = 2
              OTHERS         = 3.
          CALL FUNCTION 'RESET_ENQUEUE'. "清除Lock Container中锁信息
        ELSE.
          CALL FUNCTION 'RESET_ENQUEUE'. "清除Lock Container中锁信息
          MESSAGE |{ '流程订单' }{ LV_AUFNR }{ '加锁失败' }| TYPE 'E'.
        ENDIF.
      ENDIF.
    ENDMETHOD.
  
  *显示ALV
    METHOD SHOW_ALV.
      CASE 'X'.
        WHEN P_CREATE."创建
          "1.创建SALV对象
          CL_SALV_TABLE=>FACTORY( IMPORTING R_SALV_TABLE = GO_PRO_SALV
                                  CHANGING  T_TABLE = GT_PRO_OUTPUT ).
          "2.保存布局设置
          GS_PROGRAM-REPORT = SY-REPID.
          GO_PRO_SALV->GET_LAYOUT( )->SET_KEY( GS_PROGRAM ).
          GO_PRO_SALV->GET_LAYOUT( )->SET_SAVE_RESTRICTION( CL_SALV_LAYOUT=>RESTRICT_NONE ).
          GO_PRO_SALV->GET_DISPLAY_SETTINGS( )->SET_LIST_HEADER( '流程订单列表' )."设置标题
  
          "3.字段目录属性设置
          DATA(LO_COLUMNS) = GO_PRO_SALV->GET_COLUMNS( ).
          LO_COLUMNS->GET_COLUMN( 'OBJNR' )->SET_TECHNICAL( ABAP_TRUE ).
          LO_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ). "列宽自动优化
          LO_COLUMNS->SET_EXCEPTION_COLUMN('EXCEPTION'). "异常字段
  
          DATA(LO_COL_ERRTXT) = LO_COLUMNS->GET_COLUMN('ERRTXT').
          LO_COL_ERRTXT->SET_SHORT_TEXT('异常消息文本').
          LO_COL_ERRTXT->SET_MEDIUM_TEXT('异常消息文本').
          LO_COL_ERRTXT->SET_LONG_TEXT('异常消息文本').
  
          DATA(LO_COL_WEMPF) = LO_COLUMNS->GET_COLUMN('WEMPF').
          LO_COL_WEMPF->SET_SHORT_TEXT('备注1').
          LO_COL_WEMPF->SET_MEDIUM_TEXT('备注1').
          LO_COL_WEMPF->SET_LONG_TEXT('备注1').
  
          DATA(LO_COL_ABLAD) = LO_COLUMNS->GET_COLUMN('ABLAD').
          LO_COL_ABLAD->SET_SHORT_TEXT('备注2').
          LO_COL_ABLAD->SET_MEDIUM_TEXT('备注2').
          LO_COL_ABLAD->SET_LONG_TEXT('备注2').
  
          "4.选择模式
          GO_PRO_SALV->GET_SELECTIONS( )->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>ROW_COLUMN )."行与字段的选择模式，报表左边追加了可以选择行的MARK按钮
  
          "5.设置ALV工具栏
          GO_PRO_SALV->SET_SCREEN_STATUS( REPORT = SY-REPID
                                          PFSTATUS = 'PRO_SALV'
                                          SET_FUNCTIONS = GO_PRO_SALV->C_FUNCTIONS_ALL ).
          "6.注册事件
          SET HANDLER ON_PRO_SALV_USER_COMMAND FOR GO_PRO_SALV->GET_EVENT( ).
          SET HANDLER HANDLE_AFTER_SAVE .
          "7.显示SALV
          GO_PRO_SALV->DISPLAY( ).
        WHEN P_QUERY. "维护
          "1.创建SALV对象
          CL_SALV_TABLE=>FACTORY( IMPORTING R_SALV_TABLE = GO_PRO_SALV
                                  CHANGING  T_TABLE = GT_ZPPT0003_LIST ).
          "2.保存布局设置
          GS_PROGRAM-REPORT = SY-REPID.
          GO_PRO_SALV->GET_LAYOUT( )->SET_KEY( GS_PROGRAM ).
          GO_PRO_SALV->GET_LAYOUT( )->SET_SAVE_RESTRICTION( CL_SALV_LAYOUT=>RESTRICT_NONE ).
  
          GO_PRO_SALV->GET_DISPLAY_SETTINGS( )->SET_LIST_HEADER( '流程订入库申请单单列表：双击一行以显示入库单申请单进行维护' )."设置标题
  
          "3.字段目录属性设置
          LO_COLUMNS = GO_PRO_SALV->GET_COLUMNS( ).
          LO_COLUMNS->GET_COLUMN( 'MANDT' )->SET_TECHNICAL( ABAP_TRUE ).
          LO_COLUMNS->GET_COLUMN( 'UPD' )->SET_TECHNICAL( ABAP_TRUE ).
          LO_COLUMNS->SET_OPTIMIZE( ABAP_TRUE ). "列宽自动优化
          DATA(LO_COL_VTEXT) = LO_COLUMNS->GET_COLUMN( 'VTEXT' ).
          LO_COL_VTEXT->SET_SHORT_TEXT( '保健品送检规则' ).
          LO_COL_VTEXT->SET_MEDIUM_TEXT( '保健品送检规则' ).
          LO_COL_VTEXT->SET_LONG_TEXT( '保健品送检规则' ).
          "4.选择模式
          GO_PRO_SALV->GET_SELECTIONS( )->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>ROW_COLUMN )."行与字段的选择模式，报表左边追加了可以选择行的MARK按钮
          "5.设置ALV工具栏
          GO_PRO_SALV->GET_FUNCTIONS( )->SET_ALL( ABAP_TRUE ).
          "6.注册事件
          SET HANDLER HANDLE_DOUBLE_CLICK FOR GO_PRO_SALV->GET_EVENT( ).
          "7.显示SALV
          GO_PRO_SALV->DISPLAY( ).
        WHEN OTHERS.
      ENDCASE.
    ENDMETHOD.
  
  *  实现pro_salv自定义功能按钮事件处理程序
    METHOD ON_PRO_SALV_USER_COMMAND.
      DATA: L_KEY          TYPE CHAR90,
            LV_ZHSDAT      TYPE ZPPT0003-ZHSDAT,
            LV_ZPACK_CHARG TYPE ZPPT0003-ZPACK_CHARG,
            LV_ZPSPE       TYPE ZPPT0003-ZPSPE,
            LT_ZPSPE       LIKE TABLE OF GS_ZPSPE,
            LV_ZUMREN      TYPE ZPPT0003-ZUMREN,
            LV_TABIX       TYPE SY-TABIX.
      CASE E_SALV_FUNCTION.
        WHEN '&CREATE'.
          DATA LT_AUFNR TYPE T_AUFNR.
          "获取选中的行索引
          GT_ROWID =  GO_PRO_SALV->GET_SELECTIONS( )->GET_SELECTED_ROWS( ).
          IF GT_ROWID IS INITIAL.
            MESSAGE '请选择流程订单!' TYPE 'E' DISPLAY LIKE 'S'.
          ENDIF.
          "对选中的流程订单进行锁检查及加锁
          LOOP AT GT_ROWID INTO DATA(LS_ROW).
            GS_PRO_OUTPUT = GT_PRO_OUTPUT[ LS_ROW ].
            IF GS_PRO_OUTPUT-EXCEPTION = '3'."已成功创建了，不允许再创建
              MESSAGE |{ '第' }{ LS_ROW }{ '行订单已创建入库申请单了' } | TYPE 'E'.
              EXIT.
            ENDIF.
            SEARCH GS_PRO_OUTPUT-STTXT FOR 'REL'.
            IF SY-SUBRC <> 0.
              MESSAGE |{ '第' }{ LS_ROW }{ '行订单' }{ GS_PRO_OUTPUT-AUFNR }{ '不是下达状态' } | TYPE 'E'.
              EXIT.
            ENDIF.
            APPEND GS_PRO_OUTPUT-AUFNR TO LT_AUFNR.
            "lt_aufnr = VALUE #( BASE lt_aufnr ( gt_pro_output[ ls_row ]-aufnr ) ).
          ENDLOOP.
          ENQUEUE_AUFNR_LOCK( LT_AUFNR ).
          CLEAR: GT_ZPPT0003,GT_CHARG,GT_MATNR.
  
          LOOP AT GT_ROWID INTO LS_ROW.
            LV_TABIX = SY-TABIX.
            GS_PRO_OUTPUT = GT_PRO_OUTPUT[ LS_ROW ].
            "获取批次号的生产日期
            IF GS_PRO_OUTPUT-CHARG IS NOT INITIAL.
              TRY .
                  CLEAR: LV_ZHSDAT,LV_ZPACK_CHARG.
                  L_KEY(40) = GS_PRO_OUTPUT-MATNR.
                  L_KEY+40(4) = P_DWERK.
                  L_KEY+44(10) = GS_PRO_OUTPUT-CHARG.
                  IF GS_PRO_OUTPUT-MTART = 'Z050'.
                    DATA(LT_VALUE) = ZCL_BC_PUBLIC=>GET_CLASS_VALUES( KEY = L_KEY CLASSNUM = 'BATCH_CP' CLASSTYPE = '022' OBJTABLE = 'MCHA' ).
                    READ TABLE LT_VALUE WITH KEY CHARACT = 'Z_BATCH_DYPC' INTO DATA(LS_VALUE).
                    IF SY-SUBRC = 0.
                      LV_ZPACK_CHARG = CONV #( LS_VALUE-VALUE_CHAR ).
                    ENDIF.
  
                    READ TABLE LT_VALUE WITH KEY CHARACT = 'Z_BATCH_SCRQ' INTO LS_VALUE.
                    IF SY-SUBRC = 0 AND LS_VALUE-VALUE_CHAR IS  NOT INITIAL.
                      LV_ZHSDAT = CONV #( LS_VALUE-VALUE_CHAR ).
                    ENDIF.
                  ELSE.
                    LT_VALUE = ZCL_BC_PUBLIC=>GET_CLASS_VALUES( KEY = L_KEY CLASSNUM = 'ZBATCH_SIRIO' CLASSTYPE = '022' OBJTABLE = 'MCHA' ).
                    READ TABLE LT_VALUE WITH KEY CHARACT = 'ZPACK_CHARG' INTO LS_VALUE.
                    IF SY-SUBRC = 0.
                      LV_ZPACK_CHARG = CONV #( LS_VALUE-VALUE_CHAR ).
                    ENDIF.
  
                    READ TABLE LT_VALUE WITH KEY CHARACT = 'ZHSDAT' INTO LS_VALUE.
                    IF SY-SUBRC = 0 AND LS_VALUE-VALUE_CHAR IS  NOT INITIAL.
                      LV_ZHSDAT = CONV #( LS_VALUE-VALUE_CHAR ).
                    ENDIF.
                  ENDIF.
                CATCH CX_ROOT .
              ENDTRY.
            ENDIF.
            "计算物料货架剩余有效期
            IF LV_ZHSDAT IS NOT INITIAL.
              DATA(LV_ZVFDAT) = ZCL_PRO_GRREQ_BILL=>CAL_MARA_VFDAT( MATNR = GS_PRO_OUTPUT-MATNR START_DATE = LV_ZHSDAT ).
            ENDIF.
            GT_ZPPT0003 = VALUE #( BASE GT_ZPPT0003 (
                            ZSAPNOTE_LINE = LV_TABIX
                            BUDAT = SY-DATUM
                            WERKS = GS_PRO_OUTPUT-DWERK
                            AUFNR = GS_PRO_OUTPUT-AUFNR
                            MATNR = GS_PRO_OUTPUT-MATNR
                            MAKTX = GS_PRO_OUTPUT-MAKTX
  *                          lgort = gs_pro_output-lgort
  *                          lgobe = gs_pro_output-lgobe
                            UNTTO = GS_PRO_OUTPUT-UNTTO
                            UEBTO = GS_PRO_OUTPUT-UEBTO
                            PSMNG = GS_PRO_OUTPUT-PSMNG
                            WEMNG = GS_PRO_OUTPUT-WEMNG
                            MEINS = GS_PRO_OUTPUT-MEINS
                            CHARG = GS_PRO_OUTPUT-CHARG
                            ZPACK_CHARG = LV_ZPACK_CHARG
                            ZHSDAT = LV_ZHSDAT
                            ZVFDAT = LV_ZVFDAT
                            KDAUF = GS_PRO_OUTPUT-KDAUF
                            KDPOS = GS_PRO_OUTPUT-KDPOS
                            ZYPSL = GS_PRO_OUTPUT-ZYPSL
                            WEMPF = GS_PRO_OUTPUT-WEMPF
                            ABLAD = GS_PRO_OUTPUT-ABLAD
  *add S4DK904849 start
  *                          ZJINTGEW = GS_PRO_OUTPUT-ZJINTGEW
  *                          ZNTGEW = GS_PRO_OUTPUT-ZNTGEW
  *                          ZBRGEW = GS_PRO_OUTPUT-ZBRGEW
  *                          ZWJNTGEW = GS_PRO_OUTPUT-ZWJNTGEW
  *                          ZWNTGEW = GS_PRO_OUTPUT-ZWNTGEW
  *                          ZWBRGEW = GS_PRO_OUTPUT-ZWBRGEW
                            GEWEI = GS_PRO_OUTPUT-GEWEI
  *add S4DK904849 end
                           ) ).
  
            "物料批次号信息，用于ALV批次单元格编辑后的验证,以及单据保存前的批次检查
            GT_CHARG = VALUE #( BASE GT_CHARG ( AUFNR = GS_PRO_OUTPUT-AUFNR  MATNR = GS_PRO_OUTPUT-MATNR CHARG = GS_PRO_OUTPUT-CHARG HSDAT = LV_ZHSDAT ) ).
            GT_MATNR = VALUE #( BASE GT_MATNR ( MATNR = GS_PRO_OUTPUT-MATNR MEINS = GS_PRO_OUTPUT-MEINS ) ).
          ENDLOOP.
          "更新物料规格、包装规格、物料PC单位与基本计量单位换算率
          SORT GT_MATNR.
          DELETE ADJACENT DUPLICATES FROM GT_MATNR.
          SELECT MATNR,MEINS,GROES,MATKL FROM MARA FOR ALL ENTRIES IN @GT_MATNR
         WHERE MARA~MATNR = @GT_MATNR-MATNR
         INTO TABLE @DATA(LT_GEROS).
          LOOP AT LT_GEROS INTO DATA(LS_GREOS).
            "物料分类的包装规格
            LV_ZPSPE = ZCL_BC_PUBLIC=>GET_CLASS_VALUE( ATNAM = 'ZPSPE' KEY = LS_GREOS-MATNR CLASSTYPE = '001' CLASSNUM = LS_GREOS-MATKL OBJTABLE = 'MARA' )."包装规格特征值
            "物料PC单位等于多少基本计量单位数量
            CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
              EXPORTING
                I_MATNR              = LS_GREOS-MATNR "物料编号
                I_IN_ME              = 'ST' "PC(件)单位的内码
                I_OUT_ME             = LS_GREOS-MEINS "基本计量单位
                I_MENGE              = 1
              IMPORTING
                E_MENGE              = LV_ZUMREN
              EXCEPTIONS
                ERROR_IN_APPLICATION = 1
                ERROR                = 2
                OTHERS               = 3.
            IF SY-SUBRC <> 0.
              CLEAR LV_ZUMREN.
            ENDIF.
            LT_ZPSPE = VALUE #( BASE LT_ZPSPE ( MATNR = LS_GREOS-MATNR ZPSPE = LV_ZPSPE ZUMREN = LV_ZUMREN ) ).
          ENDLOOP.
  
          LOOP AT GT_ZPPT0003 ASSIGNING FIELD-SYMBOL(<FS>).
            READ TABLE LT_GEROS WITH KEY MATNR = <FS>-MATNR INTO DATA(LS_GEROS).
            IF SY-SUBRC = 0.
              <FS>-ZSPEC = LS_GEROS-GROES.
            ENDIF.
            READ TABLE LT_ZPSPE WITH KEY MATNR = <FS>-MATNR INTO GS_ZPSPE.
            IF SY-SUBRC = 0.
              <FS>-ZPSPE = GS_ZPSPE-ZPSPE. "包装规格
              <FS>-ZUMREN = GS_ZPSPE-ZUMREN."PC与基本单位换算率
            ENDIF.
          ENDLOOP.
  
          "存储物料启用批次管理(工厂)标识,用于单据保存的批次检查
          CLEAR GT_MATNR.
          MOVE-CORRESPONDING GT_CHARG TO GT_MATNR.
          SORT GT_MATNR.
          DELETE ADJACENT DUPLICATES FROM GT_MATNR.
          IF GT_MATNR IS NOT INITIAL.
            SELECT MATNR,XCHPF FROM MARC FOR ALL ENTRIES IN @GT_MATNR
              WHERE MATNR = @GT_MATNR-MATNR
              AND WERKS = @P_DWERK
              INTO TABLE @DATA(LT_MARC).
  
            SORT LT_MARC BY MATNR.
            LOOP AT GT_CHARG ASSIGNING FIELD-SYMBOL(<FS_CHARG>).
              READ TABLE LT_MARC WITH KEY MATNR = <FS_CHARG>-MATNR BINARY SEARCH INTO DATA(LS_MARC).
              IF  SY-SUBRC = 0.
                <FS_CHARG>-XCHPF = LS_MARC-XCHPF.
              ENDIF.
            ENDLOOP.
          ENDIF.
          "显示创建入库申请单界面
          IF GO_ZCL_PRO_GRREQ_BILL IS NOT BOUND.
            GO_ZCL_PRO_GRREQ_BILL = NEW ZCL_PRO_GRREQ_BILL(  ).
          ENDIF.
          GO_ZCL_PRO_GRREQ_BILL->SET_BILL_STATUS( IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_CREATED ).
          GO_ZCL_PRO_GRREQ_BILL->DISPLAY( )."显示创建入库申请界面
  
        WHEN OTHERS.
      ENDCASE.
    ENDMETHOD.
  
    "入库申请单保存后回写操作结果消息到此报表中
    METHOD HANDLE_AFTER_SAVE.
      DATA: LV_EXCEPTION TYPE CHAR1.   "信号灯字段-处理结果
      IF ES_BAPIRET1-TYPE = 'S'.
        LV_EXCEPTION = '3'.
      ELSE.
        LV_EXCEPTION = '1'.
      ENDIF.
      LOOP AT GT_ROWID INTO DATA(LS_ROWID).
        GT_PRO_OUTPUT[ LS_ROWID ]-EXCEPTION = LV_EXCEPTION.
        GT_PRO_OUTPUT[ LS_ROWID ]-ERRTXT = ES_BAPIRET1-MESSAGE.
      ENDLOOP.
      GO_PRO_SALV->REFRESH( VALUE LVC_S_STBL( ROW = 'X'  COL = 'X'  )  ).
    ENDMETHOD.
  
    "双击单元格实现入库申请单
    METHOD HANDLE_DOUBLE_CLICK.
      ZPPT0003 = GT_ZPPT0003_LIST[ ROW ].
      SELECT
         T1~MANDT,
         T1~ZSAPNOTE_NO,
         T1~ZSAPNOTE_LINE,
         T1~BUDAT,
         T1~WERKS,
         T1~BWART,
         T1~ZBKTXT,
         T1~AUFNR,
         T1~MATNR,
         T1~MAKTX,
         T1~ZSPEC,
         T1~ZPSPE,
         T1~ZUMREN,
         T1~LGORT,
         T1~LGOBE,
         T1~ZWSLGORT,
         T1~ZWSLGOBE,
         T1~UNTTO,
         T1~UEBTO,
         T1~PSMNG,
         T2~WEMNG,
         T1~ZWMSMNG,
         T1~MENGE,
         T1~ZJSMNG,
         T1~ZWSMNG,
         T1~MEINS,
         T1~CHARG,
         T1~ZPACK_CHARG,
         T1~ZHSDAT,
         T1~ZVFDAT,
         T1~KDAUF,
         T1~KDPOS,
         T1~ZYPSL,
         T1~WEMPF,
         T1~ABLAD,
         T1~ZDELFLG,
         T1~ERDAT,
         T1~ERZET,
         T1~UNAME,
         T1~TYPE,
         T1~MESSAGE,
         T1~AENAM,
         T1~AEDAT,
  *add S4DK904849 start
         T1~ZJINTGEW,
         T1~ZNTGEW,
         T1~ZBRGEW,
         T1~ZWJNTGEW,
         T1~ZWNTGEW,
         T1~ZWBRGEW,
         T1~GEWEI
  *add S4DK904849 end
     FROM ZPPT0003 AS T1
     LEFT JOIN AFPO AS T2  ON T1~AUFNR = T2~AUFNR
     WHERE T1~ZSAPNOTE_NO = @ZPPT0003-ZSAPNOTE_NO
     INTO CORRESPONDING FIELDS OF TABLE @GT_ZPPT0003.
  
      IF GT_ZPPT0003 IS INITIAL.
        MESSAGE |{ '入库申请单' }{ ZPPT0003-ZSAPNOTE_NO }{ '的数据不存在!' }| TYPE 'E'.
      ENDIF.
      "存储物料启用批次管理(工厂)标识,用于单据保存的批次检查
      CLEAR GT_CHARG.
      MOVE-CORRESPONDING GT_ZPPT0003 TO GT_CHARG.
      SORT GT_CHARG.
      DELETE ADJACENT DUPLICATES FROM GT_CHARG.
  
      IF GT_CHARG IS NOT INITIAL.
        DATA(LV_WERKS) = GT_ZPPT0003_LIST[ ROW ]-WERKS.
        SELECT MATNR,XCHPF FROM MARC FOR ALL ENTRIES IN @GT_CHARG
            WHERE MATNR = @GT_CHARG-MATNR
            AND WERKS = @LV_WERKS
            INTO TABLE @DATA(LT_MARC).
        SORT LT_MARC BY MATNR.
        LOOP AT GT_CHARG ASSIGNING FIELD-SYMBOL(<FS_CHARG>).
          READ TABLE LT_MARC WITH KEY MATNR = <FS_CHARG>-MATNR BINARY SEARCH INTO DATA(LS_MARC).
          IF  SY-SUBRC = 0.
            <FS_CHARG>-XCHPF = LS_MARC-XCHPF.
          ENDIF.
        ENDLOOP.
      ENDIF.
      "显示创建入库申请单界面
      IF GO_ZCL_PRO_GRREQ_BILL IS NOT BOUND.
        GO_ZCL_PRO_GRREQ_BILL = NEW ZCL_PRO_GRREQ_BILL( ).
      ENDIF.
      GO_ZCL_PRO_GRREQ_BILL->SET_BILL_STATUS( IMP_BILL_STATUS = ZCL_PRO_GRREQ_BILL=>CNS_STATUS_DISPLAY ).
      GO_ZCL_PRO_GRREQ_BILL->DISPLAY( )."显示创建入库申请界面
    ENDMETHOD.
  
  
  ENDCLASS.