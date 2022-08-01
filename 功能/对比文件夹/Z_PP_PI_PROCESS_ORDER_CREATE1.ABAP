FUNCTION Z_PP_PI_PROCESS_ORDER_CREATE1 .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ZMT_APS2SAP_PRO
*"     REFERENCE(MESSAGE_ID) TYPE  ZBC_GUID
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZMT_APS2SAP_PRO_RET
*"----------------------------------------------------------------------
*&------------------------------------------------------------------
*& FM NAME: Z_PP_PI_PROCESS_ORDER_CREATE
*& T-CODE:  N/A
*& DESCRIPTION: 流程订单创建上传接口函数
*& DS NUMBER: PP009
*&------------------------------------------------------------------
*& AUTHOR:  ljm
*& DATE:    2019.12.27
*&------------------------------------------------------------------
*& MODIFICATION LOG:
*& VERSION   DATE        AUTHOR       DESCRIPTION     CHANGE REQUEST
*& -------- ----------  -----------  -------------  -----------------
*& 1.0       2019.12.27   LJM
*&-------------------------------------------------------------------
*---M（礼盒）的批次不需要任何程序填写，用户最后手工填
*---m（瓶）的批次不需要指定批次号(接口不会传)，批次会在拆分程序中添加
*---S(药丸)及其子订单的批次需要指定批次号

*-----程序主体逻辑
*---1.1生成日志和返回记录（后续进行写入）
*---1.2判断是否需要创建传入的数据
*---如果需要创建继续执行如下逻辑
*---如果是S需求
*---2.1.1 模拟创建S及S子订单
*---2.1.2 为S本身创建流程订单
*---2.1.3 为生产S所需的所有半成品创建流程订单
*---2.1.4 为创建成功的所有流程订单创建批次
*---2.1.5 为创建成功的所有流程订单下达生产订单
*---如果是非S需求
*---2.2.1 为非S产品创建流程订单
*---2.2.2 下达生产订单
*---3.创建订单日志写入zpitlog0004

  DATA LS_STB TYPE STPOX.
  DATA LT_STB_MERGE TYPE TABLE OF STPOX.
  DATA LT_STB_MERGE_FINAL TYPE TABLE OF STPOX.
  DATA LV_MATNR TYPE MATNR.
  DATA LV_WERKS TYPE WERKS_D.
  DATA LS_ORDER_DATA TYPE BAPI_PI_ORDER_CREATE.
  DATA LS_RETURN TYPE BAPIRET2.
  DATA LV_AUFNR TYPE AUFNR.
  DATA LV_AUFNR_S TYPE AUFNR.
  DATA LV_AUFNR_ALL TYPE STRING.
  DATA LV_AUFNR_DB TYPE AUFNR.
  DATA LS_ZPITLOG0004 TYPE  ZPITLOG0004.
  DATA LT_ZPITLOG0004 TYPE TABLE OF ZPITLOG0004.
  DATA LT_PRO_RET TYPE ZDT_APS2SAP_PRO_RET_PRO_RE_TAB.
  DATA LS_PRO_RET TYPE LINE OF ZDT_APS2SAP_PRO_RET_PRO_RE_TAB.
  DATA LV_ERROR TYPE FLAG.
  DATA LV_ERROR1 TYPE FLAG.
  DATA LV_OBJNR TYPE J_OBJNR.
  DATA LT_STATUS TYPE TABLE OF JSTAT.
*  DATA lv_vbeln TYPE vbeln.
  DATA LV_CHARG TYPE CHARG_D.
  DATA LV_LEN TYPE I.
  DATA LT_BAPI_ORDER_KEY TYPE TT_BAPI_ORDER_KEY.
*  DATA lv_meins TYPE meins.
  DATA LS_BAPIRET2 TYPE BAPIRET2.
  DATA LV_ELPRO TYPE SA_ELPRO.
  DATA LV_VERID TYPE MKAL-VERID.
  DATA:BEGIN OF LS_MENGE,
         MENGE_S TYPE AUFM-MENGE,
         MENGE_H TYPE AUFM-MENGE,
       END OF LS_MENGE.
*---检查业务数据
  GET RUN TIME FIELD DATA(LV_STARTTIME)."开始执行时间
  PERFORM FRM_ORDER_CREATE_DATA_CHECK USING INPUT CHANGING OUTPUT LV_ERROR.
  CHECK LV_ERROR <> 'X'.
  LOOP AT INPUT-MT_APS2SAP_PRO-PRO_REQ INTO DATA(LS_INPUT).
    CLEAR: LT_BAPI_ORDER_KEY,LV_AUFNR_DB,LS_ZPITLOG0004,LS_PRO_RET,LV_MATNR,LV_WERKS,LV_CHARG.
    "日志表初始化信息
    MOVE-CORRESPONDING LS_INPUT TO LS_ZPITLOG0004.
    LS_ZPITLOG0004-UNAME = SY-UNAME.
    LS_ZPITLOG0004-DATUM = SY-DATUM.
    LS_ZPITLOG0004-UZEIT = SY-UZEIT.
    "返回参数初始化信息
    MOVE-CORRESPONDING LS_INPUT TO LS_PRO_RET.
    "物料号转换
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT        = LS_INPUT-MATNR
      IMPORTING
        OUTPUT       = LV_MATNR
      EXCEPTIONS
        LENGTH_ERROR = 1
        OTHERS       = 2.
    IF SY-SUBRC <> 0."无效物料号
      LS_PRO_RET-TYPE = 'E'.
      LS_PRO_RET-MESSAGE = '无效物料号'.
      LS_ZPITLOG0004-TYPE = 'E'.
      LS_ZPITLOG0004-MESSAGE = '无效物料号'.
      APPEND LS_PRO_RET TO LT_PRO_RET.
      APPEND LS_ZPITLOG0004 TO LT_ZPITLOG0004.
      CONTINUE.
    ENDIF.
    LS_INPUT-MATNR = LV_MATNR.
    LV_WERKS = LS_INPUT-WERKS."工厂
    "批次号转换
    CONDENSE LS_INPUT-CHARG NO-GAPS.
    IF LS_INPUT-CHARG IS NOT INITIAL.
      LV_LEN = STRLEN( LS_INPUT-CHARG ).
      IF LV_LEN > 10.
        LS_INPUT-CHARG = LS_INPUT-CHARG+0(10).
      ENDIF.
      CLEAR LV_CHARG.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LS_INPUT-CHARG
        IMPORTING
          OUTPUT = LV_CHARG.
      LS_INPUT-CHARG = LV_CHARG.
    ENDIF.


*根据APS传输的唯一标识码识别是否成功处理过此条电文，若已成功处理过，
*则检查生成的流程订单，对满足以下任一条件的数据不再重新创建
*1.CRTD 创建状态
*2.REL  释放状态
*3.TECO 并且有报工（CNF或PCNF）或有总和不为0的收发料
*检查GUID是否已经创建了流程订单
    SELECT SINGLE AUFNR FROM AUFK INTO LV_AUFNR_DB WHERE LOEKZ = '' AND ZAPSGUID = LS_INPUT-GUID.
    IF SY-SUBRC = 0 AND LV_AUFNR_DB IS NOT INITIAL.
      LS_ZPITLOG0004-AUFNR = LV_AUFNR_DB."GUID已经创建过订单了
      "检查订单状态
      LV_OBJNR = 'OR' && LV_AUFNR_DB.
      CALL FUNCTION 'STATUS_READ'
        EXPORTING
          OBJNR            = LV_OBJNR
          ONLY_ACTIVE      = 'X'
        TABLES
          STATUS           = LT_STATUS
        EXCEPTIONS
          OBJECT_NOT_FOUND = 1
          OTHERS           = 2.
      IF SY-SUBRC = 0.
        READ TABLE LT_STATUS INTO DATA(LS_STATUS) WITH KEY STAT = 'I0001'."CRTD 已创建
        IF SY-SUBRC = 0.
          LS_PRO_RET-TYPE = 'E'.
          LS_PRO_RET-MESSAGE = '相同GUID已生成订单，并且没有关闭'.
          LS_ZPITLOG0004-TYPE = 'E'.
          LS_ZPITLOG0004-MESSAGE = '相同GUID已生成订单，并且没有关闭'.
          APPEND LS_PRO_RET TO LT_PRO_RET.
          APPEND LS_ZPITLOG0004 TO LT_ZPITLOG0004.
          CONTINUE."执行下一条数据
        ENDIF.
        READ TABLE LT_STATUS INTO LS_STATUS WITH KEY STAT = 'I0002'."REL 已释放
        IF SY-SUBRC = 0.
          LS_PRO_RET-TYPE = 'E'.
          LS_PRO_RET-MESSAGE = '相同GUID已生成订单并释放，并且没有关闭'.
          LS_ZPITLOG0004-TYPE = 'E'.
          LS_ZPITLOG0004-MESSAGE = '相同GUID已生成订单并释放，并且没有关闭'.
          APPEND LS_PRO_RET TO LT_PRO_RET.
          APPEND LS_ZPITLOG0004 TO LT_ZPITLOG0004.
          CONTINUE."执行下一条数据
        ENDIF.
        "检查生成的流程订单状态是否为TECO且有PCNF/CNF中的任意一个
        READ TABLE LT_STATUS INTO LS_STATUS WITH KEY STAT = 'I0045'."TECO 技术性完成 并且有报工（CNF或PCNF）或有总和不为0的收发料
        IF SY-SUBRC = 0.
          READ TABLE LT_STATUS INTO LS_STATUS WITH KEY STAT = 'I0009'."CNF  已确认
          IF SY-SUBRC = 0.
            LS_PRO_RET-TYPE = 'E'.
            LS_PRO_RET-MESSAGE = '相同GUID已生成订单且已技术性完成并确认'.
            LS_ZPITLOG0004-TYPE = 'E'.
            LS_ZPITLOG0004-MESSAGE = '相同GUID已生成订单且已技术性完成并确认'.
            APPEND LS_PRO_RET TO LT_PRO_RET.
            APPEND LS_ZPITLOG0004 TO LT_ZPITLOG0004.
            CONTINUE."执行下一条数据
          ENDIF.
          READ TABLE LT_STATUS INTO LS_STATUS WITH KEY STAT = 'I0010'."PCNF	部分确认
          IF SY-SUBRC = 0.
            LS_PRO_RET-TYPE = 'E'.
            LS_PRO_RET-MESSAGE = '相同GUID已生成订单且已技术性完成并部分确认'.
            LS_ZPITLOG0004-TYPE = 'E'.
            LS_ZPITLOG0004-MESSAGE = '相同GUID已生成订单且已技术性完成并部分确认'.
            APPEND LS_PRO_RET TO LT_PRO_RET.
            APPEND LS_ZPITLOG0004 TO LT_ZPITLOG0004.
            CONTINUE."执行下一条数据
          ENDIF.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = LV_AUFNR_DB
            IMPORTING
              OUTPUT = LV_AUFNR_DB.
          SELECT SUM( CASE SHKZG WHEN 'S'  THEN MENGE ELSE 0 END  ) AS MENGE_S,
                 SUM( CASE SHKZG WHEN 'H'  THEN MENGE ELSE 0 END  ) AS MENGE_H
           FROM AUFM WHERE AUFNR = @LV_AUFNR_DB INTO @LS_MENGE.
          IF LS_MENGE-MENGE_S - LS_MENGE-MENGE_H <> 0."判断订单收发料总和不为0
            LS_PRO_RET-TYPE = 'E'.
            LS_PRO_RET-MESSAGE = '相同GUID已生成订单且已产生货物移动'.
            LS_ZPITLOG0004-TYPE = 'E'.
            LS_ZPITLOG0004-MESSAGE = '相同GUID已生成订单且已产生货物移动'.
            APPEND LS_PRO_RET TO LT_PRO_RET.
            APPEND LS_ZPITLOG0004 TO LT_ZPITLOG0004.
            CONTINUE."执行下一条数据
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    "---数据需要创建流程订单，继续执行
    CLEAR: LV_ERROR,LV_AUFNR.
*    REFRESH lt_stb.
    "计量单位外码转内码
*    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
*      EXPORTING
*        input          = ls_input-gmein
**       LANGUAGE       = SY-LANGU
*      IMPORTING
*        output         = lv_meins
*      EXCEPTIONS
*        unit_not_found = 1
*        OTHERS         = 2.
*    IF sy-subrc <> 0.
*      CLEAR ls_input-gmein.
*    ELSE.
*      ls_input-gmein = lv_meins.
*    ENDIF.
    CLEAR LS_INPUT-GMEIN.
*----若“是否为S”标识为S，则通过展开S的BOM,计算下阶所有半成品（物料类型不为原辅料）的数量,并创建订单(S和S子订单)
    IF LS_INPUT-ZSFS = 'S'.
*--------S模拟创建检查，模拟创建S及S子订单
      PERFORM FRM_S_CREATE_SIMU1 USING LS_INPUT CHANGING LV_ERROR LV_AUFNR LS_PRO_RET LS_ZPITLOG0004.
      IF LV_ERROR = 'X'."数据有错误，不能继续创建
        LS_PRO_RET-TYPE = 'E'.
        LS_PRO_RET-MESSAGE = |{ LV_MATNR ALPHA  = OUT }{ '模拟创建订单失败:' }{ LS_PRO_RET-MESSAGE }|.
        LS_ZPITLOG0004-TYPE = 'E'.
        LS_ZPITLOG0004-MESSAGE = |{ LV_MATNR ALPHA  = OUT }{ '模拟创建订单失败:' }{ LS_PRO_RET-MESSAGE }|.
        APPEND LS_PRO_RET TO LT_PRO_RET.
        APPEND LS_ZPITLOG0004 TO LT_ZPITLOG0004.
        CONTINUE.
      ENDIF.
*-----为S本身创建流程订单
      CLEAR:LV_AUFNR_ALL,LV_AUFNR_S.
      PERFORM FRM_CREATE_PROCESS_ORDER1 USING LS_INPUT CHANGING LV_ERROR LV_AUFNR_S LS_PRO_RET LS_ZPITLOG0004.
***----获取处理完物料主数据的装配报废率（MARC-AUSSS）和组件报废率（STPO-AUSCH）的组件需求数
      IF LV_ERROR IS INITIAL."创建成功
        "创建物料批次号
        PERFORM FRM_CREATE_MATERIAL_BATCH USING LV_MATNR LV_WERKS LV_CHARG LV_AUFNR_S CHANGING LS_BAPIRET2.
        IF LS_BAPIRET2-TYPE = 'E'."批次创建失败
          LS_ZPITLOG0004-MESSAGE = |{ LS_ZPITLOG0004-MESSAGE }{ ',' }{ LS_BAPIRET2-MESSAGE }|.
        ENDIF.
*-------------------------------处理S展开的子订单-------------------------------
        LS_INPUT-GAMNG = LS_ZPITLOG0004-GAMNG."S订单最终数量
        CLEAR LT_STB_MERGE_FINAL.
        PERFORM FRM_CALCULATE_BOM_REQ USING LS_INPUT '' CHANGING LT_STB_MERGE_FINAL.
*----------为生产S所需的所有半成品创建流程订单
        LV_AUFNR_ALL = |{ 'S订单及子订单:' } { LV_AUFNR_S  ALPHA = OUT }|."拼接S及下阶所有订单号
        "创建成功的订单号用于后续的下达操作
        LT_BAPI_ORDER_KEY = VALUE #( BASE LT_BAPI_ORDER_KEY ( ORDER_NUMBER = LV_AUFNR_S ) ).
        "S物料的生产版本上的发货存储位置，如果存在值需作为下阶30物料流程订单的收货仓库，
        "因为不同仓库地点的MRP区域不同. // 2020-08-18
        LV_VERID = CONV CHAR4( LS_INPUT-VERID ).
        PERFORM FRM_MAR_VERSION_STORAGE USING LV_MATNR LV_WERKS LV_VERID CHANGING LV_ELPRO.
        "// 2020-08-18
        LOOP AT LT_STB_MERGE_FINAL INTO LS_STB."重组好的半成品
          CLEAR: LS_ORDER_DATA,LS_RETURN,LV_AUFNR.
          LS_ORDER_DATA-MATERIAL = LS_STB-IDNRK."物料
          LS_ORDER_DATA-PLANT = LS_STB-WERKS."生产工厂
          LS_ORDER_DATA-ORDER_TYPE = LS_INPUT-AUART."订单类型
          LS_ORDER_DATA-BASIC_START_DATE = LS_INPUT-GSTRP."基本开始日期
          LS_ORDER_DATA-BASIC_START_TIME = LS_INPUT-ZRSV04."基本开始时间 S4DK906031
          LS_ORDER_DATA-BASIC_END_DATE = LS_INPUT-GLTRP."基本完成日期
          LS_ORDER_DATA-BASIC_END_TIME = LS_INPUT-ZRSV05."基本完成时间 S4DK906031
          LS_ORDER_DATA-QUANTITY = LS_STB-MNGLG."计划生产数量
          LS_ORDER_DATA-QUANTITY_UOM = LS_STB-MMEIN."订单通用单位
          LS_ORDER_DATA-PROD_VERSION = LS_INPUT-VERID."生产版本
          "流程订单上的抬头收货仓库地点可以决定不同的MRP区域
          IF LV_ELPRO IS NOT INITIAL.
            LS_ORDER_DATA-STORAGE_LOCATION = LV_ELPRO.
          ENDIF.
          "传输销售订单信息给BAPI质检出口ZXQPAU04(S订单BAPI不填销售订单)
*          lv_vbeln = ls_input-kdauf.
          LS_ORDER_DATA-ZKDAUF_S = LS_INPUT-KDAUF.
          LS_ORDER_DATA-ZKDPOS_S = LS_INPUT-KDPOS.
*          SET PARAMETER ID 'ZAPS_VBELN' FIELD ''."首先清空
*          SET PARAMETER ID 'ZAPS_VBELN' FIELD lv_vbeln.
          "-------增强字段赋值-----------
          LS_ORDER_DATA-ZAUFNR_S = LV_AUFNR_S."S下阶订单的参考S订单号赋值
          LS_ORDER_DATA-ZCHARG = LS_INPUT-CHARG."批次号
          LS_ORDER_DATA-ZCRD_BY_EX = 'X'."APS接口创建标识，必须维护否则增强结构字段值会赋值失败
          "S子订单创建时需记录下物料装配报废率
          SELECT SINGLE AUSSS FROM MARC INTO LS_ORDER_DATA-ZAUSSS_CRD
            WHERE MATNR = LS_STB-IDNRK
              AND WERKS = LS_STB-WERKS.
          PERFORM FRM_CRD_CI_AUFK USING LS_INPUT '' LV_AUFNR_S CHANGING LS_ORDER_DATA. "S4DK906031
          "-------增强字段赋值-----------
          CALL FUNCTION 'BAPI_PROCORD_CREATE'
            EXPORTING
              ORDERDATA    = LS_ORDER_DATA
            IMPORTING
              RETURN       = LS_RETURN
              ORDER_NUMBER = LV_AUFNR.
          IF LS_RETURN-TYPE = 'E' OR LS_RETURN-TYPE = 'A'.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            LV_ERROR = 'X'.
            LS_PRO_RET-TYPE = 'E'.
            LS_PRO_RET-MESSAGE = LS_RETURN-MESSAGE.
            CLEAR: LS_ZPITLOG0004-AUFNR."清除S自身订单号
            LS_ZPITLOG0004-TYPE = 'E'.
            LS_ZPITLOG0004-MESSAGE = LS_RETURN-MESSAGE.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING  WAIT = 'X'.
          ENDIF.
          IF LV_ERROR = 'X'."S订单中的任何一个子订单创建失败，则整个S订单都不创建
            EXIT.
          ENDIF.
          LV_AUFNR_ALL = |{ LV_AUFNR_ALL }{ ',' }{ LV_AUFNR ALPHA = OUT }|."拼接S及下阶所有订单号
          "创建成功的订单号用于后续的下达操作
          LT_BAPI_ORDER_KEY = VALUE #( BASE LT_BAPI_ORDER_KEY ( ORDER_NUMBER = LV_AUFNR ) ).
          "创建物料批次号
          PERFORM FRM_CREATE_MATERIAL_BATCH USING LS_STB-IDNRK LS_STB-WERKS LV_CHARG LV_AUFNR CHANGING LS_BAPIRET2.
          IF LS_BAPIRET2-TYPE = 'E'."批次创建失败
            LS_ZPITLOG0004-MESSAGE = |{ LS_ZPITLOG0004-MESSAGE }{ ',' }{ LS_BAPIRET2-MESSAGE }|.
          ENDIF.
        ENDLOOP.
      ENDIF.
      IF LV_ERROR IS INITIAL."S订单中的所有子订单全部创建成功
        LS_PRO_RET-TYPE = 'S'.
        LS_PRO_RET-MESSAGE = |{ LV_AUFNR_ALL }{ '创建成功' }| .
      ELSE.
        LS_PRO_RET-TYPE = 'E'.
        LS_PRO_RET-MESSAGE = |{ LV_MATNR ALPHA  = OUT }{ '创建订单失败,' }{ LS_PRO_RET-MESSAGE }|.
        LS_ZPITLOG0004-TYPE = 'E'.
        LS_ZPITLOG0004-MESSAGE = |{ LV_MATNR ALPHA  = OUT }{ '创建订单失败,' }{ LS_ZPITLOG0004-MESSAGE }|.
        APPEND LS_PRO_RET TO LT_PRO_RET.
        APPEND LS_ZPITLOG0004 TO LT_ZPITLOG0004.
        CONTINUE."S及其子流程订单创建失败，继续处理下一条传入数据
      ENDIF.
*---------为创建成功的所有流程订单下达生产订单
*      IF ls_input-status = 'REL'.
*        PERFORM frm_release_order1 USING lt_bapi_order_key "需下达的订单号
*                                   CHANGING ls_pro_ret ""APS请求的响应消息
*                                            ls_zpitlog0004."订单创建日志
*      ENDIF.
    ELSE.
*--------模拟创建非S即M流程订单,因M流程订单不需要创建下级子订单，所以不需要模拟
*      CLEAR lv_error.
*      PERFORM frm_create_process_order_simu1 USING ls_input "APS请求消息
*                                             CHANGING lv_error "订单创建成功标识
*                                             lv_aufnr "订单创建成功后的订单号
*                                             ls_pro_ret "APS请求的响应消息
*                                             ls_zpitlog0004. "订单创建日志
*      IF lv_error = 'X'.
*        CONTINUE.
*      ENDIF.
*----为非S产品创建流程订单
      CLEAR: LV_ERROR,LV_AUFNR,LT_BAPI_ORDER_KEY.
      PERFORM FRM_CREATE_PROCESS_ORDER1 USING LS_INPUT "APS请求消息
                                        CHANGING LV_ERROR "订单创建成功标识
                                                 LV_AUFNR "订单创建成功后的订单号
                                                 LS_PRO_RET "APS请求的响应消息
                                                 LS_ZPITLOG0004."订单创建日志
      IF LV_ERROR = 'X'.
        LS_PRO_RET-TYPE = 'E'.
        LS_PRO_RET-MESSAGE = |{ LV_MATNR ALPHA  = OUT }{ '创建订单失败' }{ LS_PRO_RET-MESSAGE }|.
        LS_ZPITLOG0004-TYPE = 'E'.
        LS_ZPITLOG0004-MESSAGE = |{ LV_MATNR ALPHA  = OUT }{ '创建订单失败' }{ LS_PRO_RET-MESSAGE }|.
        APPEND LS_PRO_RET TO LT_PRO_RET.
        APPEND LS_ZPITLOG0004 TO LT_ZPITLOG0004.
        CONTINUE.
      ENDIF.
      LS_PRO_RET-TYPE = 'S'.
      LS_PRO_RET-MESSAGE = '订单' && LV_AUFNR && '创建成功'.
*---------下达生产订单
*      IF ls_input-status = 'REL' .
*        lt_bapi_order_key = VALUE #(  ( order_number = lv_aufnr ) ).
*        PERFORM frm_release_order1 USING lt_bapi_order_key "需下达的订单号
*                                 CHANGING ls_pro_ret ""APS请求的响应消息
*                                          ls_zpitlog0004."订单创建日志
*      ENDIF.
    ENDIF.
    APPEND LS_ZPITLOG0004 TO LT_ZPITLOG0004."追加订单创建日志
    APPEND LS_PRO_RET TO LT_PRO_RET."追加请求响应消息
  ENDLOOP.
  OUTPUT-MT_APS2SAP_PRO_RET-PRO_RET = LT_PRO_RET."返回响应消息
  MODIFY ZPITLOG0004 FROM TABLE LT_ZPITLOG0004.
  "执行结束时间
  GET RUN TIME FIELD DATA(LV_ENDTIME).
  OUTPUT-MT_APS2SAP_PRO_RET-TYPE = 'S'.
  OUTPUT-MT_APS2SAP_PRO_RET-MESSAGE = |{ '完成创建,耗时' }{ LV_ENDTIME - LV_STARTTIME STYLE = SCIENTIFIC }{ '微秒' }|.
ENDFUNCTION.