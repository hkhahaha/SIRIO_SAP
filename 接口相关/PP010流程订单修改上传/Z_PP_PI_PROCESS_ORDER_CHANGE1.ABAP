FUNCTION Z_PP_PI_PROCESS_ORDER_CHANGE1 .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ZMT_APS2SAP_PRO_U
*"     REFERENCE(MESSAGE_ID) TYPE  ZBC_GUID
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZMT_APS2SAP_PRO_U_RET
*"----------------------------------------------------------------------
*&------------------------------------------------------------------
*& FM NAME: Z_PP_PI_PROCESS_ORDER_CHANGE
*& T-CODE:  N/A
*& DESCRIPTION: 流程订单修改上传接口函数
*& DS NUMBER: PP010
*&------------------------------------------------------------------
*& AUTHOR:  LJM
*& DATE:    2019.12.28
*&------------------------------------------------------------------
*& MODIFICATION LOG:
*& VERSION   DATE        AUTHOR       DESCRIPTION     CHANGE REQUEST
*& -------- ----------  -----------  -------------  -----------------
*& 1.0       2019.12.28   LJM
*&-------------------------------------------------------------------
*---M（礼盒）的批次不需要任何程序填写，用户最后手工填
*---m（瓶）的批次不需要指定批次号(接口不会传)，批次会在拆分程序中添加
*---S(药丸)及其子订单的批次需要指定批次号

*-----程序主体逻辑
*---1.1初始化接口返回数据
*---如果是S类型
*---2.1.1 模拟修改S订单及子订单
*---2.1.2 修改S订单及子订单
*---如果是M类型
*---2.2.1 修改M订单
*---2.2.2 记录日志表
*---如果是m类型
*---2.3.1 检查订单是否被被拆单
*---2.3.2 修改m订单
  DATA LV_AUFNR_IN TYPE AUFNR.
  DATA LS_LOG TYPE ZPITLOG0005.
  DATA LT_ZPITLOG0005 TYPE TABLE OF ZPITLOG0005. "日志表
  DATA LS_PRO_RET TYPE ZDT_APS2SAP_PRO_U_RET_PRO_RET. "接口返回消息结构
  DATA LT_PRO_RET TYPE ZDT_APS2SAP_PRO_U_RET_PRO__TAB. "接口返回消息内表
  DATA LT_ORDER_CHANGE TYPE TT_ORDER_CHANGE. "修改BAPI传入参数内表
  DATA LS_ORDER_CHANGE TYPE TY_ORDER_CHANGE. "修改BAPI传入参数结构
  DATA LV_ERROR TYPE FLAG.
  DATA LV_LEN TYPE I.
  DATA LV_STAT TYPE JEST-STAT.
  DATA LV_CHARG TYPE CHARG_D.
  DATA LO_EX_ROOT TYPE REF TO CX_ROOT.
  DATA LT_AUFNR TYPE TABLE OF TY_AUFNR.
  FIELD-SYMBOLS:<FS_ZPITLOG0005> TYPE ZPITLOG0005,
                <FS_PRO_RET>     TYPE ZDT_APS2SAP_PRO_U_RET_PRO_RET.
  GET RUN TIME FIELD DATA(LV_STARTTIME)."记录程序执行开始时间
*---检查数据
  PERFORM FRM_ORDER_CHANGE_DATA_CHECK USING INPUT CHANGING OUTPUT LV_ERROR.
  IF LV_ERROR = 'X'.
    RETURN.
  ENDIF.
  LOOP AT INPUT-MT_APS2SAP_PRO_U-PRO_REQ INTO DATA(LS_INPUT).
    CLEAR: LV_AUFNR_IN,LV_ERROR,LT_AUFNR,LS_ORDER_CHANGE,LT_ORDER_CHANGE.
    "初始化返回消息、修改订单日志行
    APPEND INITIAL LINE TO LT_PRO_RET ASSIGNING <FS_PRO_RET>.
    APPEND INITIAL LINE TO LT_ZPITLOG0005 ASSIGNING <FS_ZPITLOG0005>.
    "返回消息与修改日志行初始化失败时需返回消息
    IF <FS_PRO_RET> IS NOT ASSIGNED OR <FS_ZPITLOG0005> IS NOT ASSIGNED.
      LT_PRO_RET = VALUE #( BASE LT_PRO_RET ( AUFNR = LS_INPUT-AUFNR TYPE = 'E' MESSAGE = '返回消息或修改日志初始化失败' ) ).
      CONTINUE.
    ENDIF.
    "订单修改日志初始化
    MOVE-CORRESPONDING LS_INPUT TO <FS_ZPITLOG0005>.
    <FS_ZPITLOG0005>-AUFNR_REF = LS_INPUT-AUFNR.
    <FS_ZPITLOG0005>-UNAME = SY-UNAME.
    <FS_ZPITLOG0005>-DATUM = SY-DATUM.
    <FS_ZPITLOG0005>-UZEIT = SY-UZEIT.
    "接口返回消息初始化
    <FS_PRO_RET>-AUFNR =  LS_INPUT-AUFNR."流程订单号
    "订单号转换
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = LS_INPUT-AUFNR
      IMPORTING
        OUTPUT = LV_AUFNR_IN.
    "批次号转换
    CONDENSE LS_INPUT-CHARG NO-GAPS.
    IF LS_INPUT-CHARG IS NOT INITIAL.
      LV_LEN = STRLEN( LS_INPUT-CHARG ).
      IF LV_LEN > 10.
        LS_INPUT-CHARG = LS_INPUT-CHARG+0(10).
      ENDIF.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LS_INPUT-CHARG
        IMPORTING
          OUTPUT = LV_CHARG.
      LS_INPUT-CHARG = LV_CHARG.
    ENDIF.
    IF LS_INPUT-ZSFS IS NOT INITIAL."S订单
*----S订单及子订单模拟修改
      PERFORM FRM_CHANGE_ORDER_S_SIMU1 USING LS_INPUT LV_AUFNR_IN CHANGING  LV_ERROR LT_ORDER_CHANGE <FS_PRO_RET>.
      IF LV_ERROR = 'X'."模拟处理出错,本订单不可更改
        CONTINUE.
      ENDIF.
*----S订单及子订单修改
      LOOP AT LT_ORDER_CHANGE  INTO LS_ORDER_CHANGE.
        "正式修改流程订单的传入参数已经在模拟修改那边设置好了，这边就不再重复准备传入参数了
        PERFORM FRM_CHANGE_PROCESSORDER USING LS_ORDER_CHANGE 'S' CHANGING LV_ERROR <FS_PRO_RET>-MESSAGE.
        IF LV_ERROR = 'X'."修改订单失败
          <FS_PRO_RET>-TYPE = 'E'.
          EXIT.
        ENDIF.
        <FS_PRO_RET>-MESSAGE = |{ <FS_PRO_RET>-MESSAGE } { <FS_PRO_RET>-MESSAGE }|.
      ENDLOOP.
      "S订单及子订单下达
*      IF ls_input-status = 'REL'.
*        IF lt_order_change IS NOT INITIAL.
*          SELECT aufk~aufnr FROM aufk
*            INNER JOIN jest ON aufk~objnr = jest~objnr
*            FOR ALL ENTRIES IN @lt_order_change
*            WHERE aufk~aufnr = @lt_order_change-number
*            AND jest~stat = 'I0002' "下达状态
*            AND jest~inact = 'X' "下达状态未激活
*            INTO TABLE @DATA(lt_jest).
*        ENDIF.
*        LOOP AT lt_jest  INTO DATA(ls_jest).
*          PERFORM frm_release_order_chg USING ls_jest-aufnr CHANGING <fs_zpitlog0005>.
*        ENDLOOP.
*      ENDIF.
      <FS_PRO_RET>-TYPE = 'S'.
      <FS_PRO_RET>-MESSAGE = '修改成功'.
    ELSE."M礼盒订单,m非礼盒订单
      CLEAR LS_ORDER_CHANGE.
      "----------修改BAPI传入参数赋值---begin----
      LS_ORDER_CHANGE-NUMBER = LV_AUFNR_IN."流程订单号
      LS_ORDER_CHANGE-BASIC_START_DATE = LS_INPUT-GSTRP."基本开始日期
      LS_ORDER_CHANGE-BASIC_START_TIME = LS_INPUT-ZRSV04."基本开始时间 S4DK906031
      LS_ORDER_CHANGE-BASIC_END_DATE = LS_INPUT-GLTRP. "基本完成日期
      LS_ORDER_CHANGE-BASIC_END_TIME = LS_INPUT-ZRSV05. "基本完成时间 S4DK906031
      "生产版本
      IF LS_INPUT-VERID IS NOT INITIAL.
        LS_ORDER_CHANGE-PROD_VERSION = LS_INPUT-VERID.
      ENDIF.
      "厂区
      IF LS_INPUT-ZSTCQ IS NOT INITIAL.
        LS_ORDER_CHANGE-ZSTCQ = LS_INPUT-ZSTCQ.
      ENDIF.
      "车间
      IF LS_INPUT-ZCHEJ IS NOT INITIAL.
        LS_ORDER_CHANGE-ZCHEJ = LS_INPUT-ZCHEJ.
      ENDIF.
      "产线
      IF LS_INPUT-ZCHANX IS NOT INITIAL.
        LS_ORDER_CHANGE-ZCHANX = LS_INPUT-ZCHANX.
      ENDIF.
      LS_ORDER_CHANGE-QUANTITY = CONV GAMNG( LS_INPUT-GAMNG )."订单修改数量
      "ls_input-zmpc = 1为m非礼盒订单需检查是否已被拆单，如果已被拆单了则不能修改数量
      IF LS_INPUT-ZMPC = '1'.
        SELECT  AUFNR FROM AUFK WHERE ZAUFNR_SPLIT = @LV_AUFNR_IN INTO TABLE @LT_AUFNR.
        "m未被拆单,需修改数量
        IF LT_AUFNR IS NOT INITIAL.
          CLEAR LS_ORDER_CHANGE-QUANTITY.
          "APS传过来的修改数量是净需求,需重算下装配报废数量
*        SELECT SINGLE  marc~ausss,afpo~meins
*          FROM afpo
*          INNER JOIN marc ON afpo~matnr = marc~matnr AND afpo~dwerk = marc~werks
*          WHERE aufnr = @lv_aufnr_in
*          INTO @DATA(ls_marc).
*        IF sy-subrc = 0.
*          TRY .
*              ls_order_change-quantity = CONV gamng( ls_input-gamng ).
*              ls_order_change-quantity = ls_order_change-quantity + ( ls_order_change-quantity * ls_marc-ausss / 100 )."修改数量包含报废数量
*              "订单报废数量
*              ls_order_change-scrap_quantity =  ls_order_change-scrap_quantity - ls_order_change-scrap_quantity / ( 1 + ls_marc-ausss / 100 ) .
*              "基本单位没有小数点时，数量向上取整
*              SELECT SINGLE andec FROM t006 WHERE msehi = @ls_marc-meins INTO @DATA(lv_andec).
*              IF lv_andec = 0.
*                ls_order_change-quantity = ceil( ls_order_change-quantity ).
*                ls_order_change-scrap_quantity = ceil( ls_order_change-scrap_quantity ).
*              ENDIF.
*            CATCH cx_root INTO DATA(lo_cx_root).
*              cv_error = 'X'.
*              cs_output-type = 'E'.
*              cs_output-message = |{ iv_aufnr_in ALPHA = OUT }{ '订单数量计算报废数量有误' }|.
*              CONTINUE.
*          ENDTRY.
*        ENDIF.
        ENDIF.
      ENDIF.
      "----------修改BAPI传入参数赋值---end----
      "修改流程订单
      PERFORM   FRM_CHANGE_PROCESSORDER USING LS_ORDER_CHANGE 'M' CHANGING LV_ERROR <FS_PRO_RET>-MESSAGE.
      IF LV_ERROR IS INITIAL."修改成功
        <FS_PRO_RET>-TYPE = 'S'.
        "如果订单已被拆单，需回执信息
        IF LT_AUFNR IS NOT INITIAL.
          <FS_PRO_RET>-MESSAGE = |{ '修改成功,但订单已被拆单不更新数量' }|.
        ELSE.
          <FS_PRO_RET>-MESSAGE = '修改成功'.
        ENDIF.
      ELSE.
        <FS_PRO_RET>-TYPE = 'E'.
      ENDIF.
      "订单下达
*      IF ls_input-status = 'REL' AND lv_error IS INITIAL.
*        "订单未下达状态
*        CLEAR lv_stat.
*        SELECT SINGLE jest~stat INTO lv_stat FROM aufk
*          INNER JOIN jest ON aufk~objnr = jest~objnr
*          WHERE aufk~aufnr = ls_order_change-number
*          AND jest~stat = 'I0002' "下达状态
*          AND jest~inact = 'X'. "下达状态未激活
*        IF lv_stat IS NOT INITIAL.
*          PERFORM frm_release_order_chg USING ls_order_change-number CHANGING <fs_zpitlog0005>.
*        ENDIF.
*      ENDIF.
    ENDIF.
  ENDLOOP.
  "记录日志表
  MODIFY ZPITLOG0005 FROM TABLE LT_ZPITLOG0005.
  GET RUN TIME FIELD DATA(LV_ENDTIME)."记录程序执行结束时间
  OUTPUT-MT_APS2SAP_PRO_U_RET-TYPE = 'S'.
  OUTPUT-MT_APS2SAP_PRO_U_RET-MESSAGE = | { '修改完成,耗时' }{ LV_ENDTIME - LV_STARTTIME STYLE = SCIENTIFIC }{ '微秒' }|.
  OUTPUT-MT_APS2SAP_PRO_U_RET-PRO_RET = LT_PRO_RET.


ENDFUNCTION.