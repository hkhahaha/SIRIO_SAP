*----------------------------------------------------------------------*
***INCLUDE LZMM033F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_EXIST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LW_MRAR_MATNR
*&      <-- LV_SUBRC
*&---------------------------------------------------------------------*
FORM FRM_CHECK_EXIST  USING    UV_MATNR TYPE MATNR
                               UV_PLANT TYPE WERKS_D
*                               UV_SALES TYPE VKORG
*                               UV_DISTR TYPE VTWEG
                      CHANGING CV_SUBRC TYPE C.
  DATA: LS_CLIENTDATA TYPE BAPI_MARA_GA,
        LS_PLANTDATA  TYPE BAPI_MARC_GA,
        LS_SALESDATA  TYPE BAPI_MVKE_GA,
        LV_MATNR      TYPE BAPI_MARA_GA-MATERIAL.
  CLEAR: LS_CLIENTDATA, LS_PLANTDATA, LS_SALESDATA, LV_MATNR.
  LV_MATNR = UV_MATNR.
  CALL FUNCTION 'BAPI_MATERIAL_GET_ALL'
    EXPORTING
      MATERIAL   = LV_MATNR
      PLANT      = UV_PLANT
*     SALESORG   = UV_SALES
*     DISTR_CHAN = UV_DISTR
    IMPORTING
      CLIENTDATA = LS_CLIENTDATA
      PLANTDATA  = LS_PLANTDATA
      SALESDATA  = LS_SALESDATA.
  " CHECK MATERIAL DATA EXIST
  IF LS_CLIENTDATA IS INITIAL.
    CV_SUBRC = 'X'.
  ELSE.
    " CHECK PLANT DATA EXIST
    IF UV_PLANT IS NOT INITIAL.
      IF LS_PLANTDATA IS INITIAL.
        CV_SUBRC = 'X'.
      ENDIF.
    ENDIF.
*    " CHECK SALESORG DATA EXIST
*    IF UV_SALES IS NOT INITIAL AND UV_DISTR IS NOT INITIAL.
*      IF LS_SALESDATA IS INITIAL.
*        CV_SUBRC = 'X'.
*      ENDIF.
*    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREATE_MATNR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LW_MRAR
*&      --> ENDIF
*&---------------------------------------------------------------------*
FORM FRM_CREATE_MATNR  USING    US_INPUT  TYPE ZDT_OA2SAP_MDM_MARA
                       CHANGING CS_RETURN TYPE ZDT_OA2SAP_MDM_RET_ZMD001.
  DATA: LW_HEADDATA            TYPE BAPIMATHEAD,
        LW_CLIENTDATA          TYPE BAPI_MARA,
        LW_CLIENTDATAX         TYPE BAPI_MARAX,
        LW_RETURN              TYPE BAPIRET2,
        LT_UNITSOFMEASURE      TYPE STANDARD TABLE OF BAPI_MARM,
        LW_UNITSOFMEASURE      TYPE BAPI_MARM,
        LT_UNITSOFMEASUREX     TYPE STANDARD TABLE OF BAPI_MARMX,
        LW_UNITSOFMEASUREX     TYPE BAPI_MARMX,
        LT_MATERIALDESCRIPTION TYPE STANDARD TABLE OF BAPI_MAKT,
        LW_MATERIALDESCRIPTION TYPE BAPI_MAKT.

  DATA: LT_SMEINH TYPE ZDT_OA2SAP_MDM_MARA_SMEINH_TAB,
        LS_SMEINH TYPE ZDT_OA2SAP_MDM_MARA_SMEINH,
        LV_MTART  TYPE MTART,
        LV_MATNR  TYPE MATNR,
        LV_FLAG   TYPE C.
  " 取物料码段
  CLEAR: LV_MATNR, LV_MTART, LV_FLAG.
  LV_MTART = US_INPUT-MTART.
  PERFORM FRM_GET_MATNR USING LV_MTART CHANGING LV_MATNR.
  " 创建物料基本视图
  CLEAR: LW_HEADDATA, LW_CLIENTDATA, LW_CLIENTDATAX.
  LW_HEADDATA-MATERIAL = LV_MATNR.                        " 物料号
  LW_HEADDATA-MATL_TYPE = US_INPUT-MTART.                 " 物料类型
  LW_HEADDATA-IND_SECTOR = 'P'.                           " 行业领域
  LW_HEADDATA-BASIC_VIEW = 'X'.                           " 基本数据视图
  LW_CLIENTDATA-MATL_GROUP = US_INPUT-MATKL.              " 物料组
  LW_CLIENTDATAX-MATL_GROUP = 'X'.
  LW_CLIENTDATA-OLD_MAT_NO = US_INPUT-BISMT.              " 旧物料号
  LW_CLIENTDATAX-OLD_MAT_NO = 'X'.
  LW_CLIENTDATA-BASE_UOM = US_INPUT-MEINS.                " 基本计量单位
  LW_CLIENTDATAX-BASE_UOM = 'X'.
  LW_CLIENTDATA-SIZE_DIM = US_INPUT-GROES.                " 规格型号（大小/量纲）
  LW_CLIENTDATAX-SIZE_DIM = 'X'.
  LW_CLIENTDATA-MFR_NO = US_INPUT-MFRNR.                  " 制造商
  LW_CLIENTDATAX-MFR_NO = 'X'.
  LW_CLIENTDATA-PUR_VALKEY = US_INPUT-EKWSL.              " 采购价值代码
  LW_CLIENTDATAX-PUR_VALKEY = 'X'.
  CLEAR: LT_MATERIALDESCRIPTION, LW_MATERIALDESCRIPTION.
  LW_MATERIALDESCRIPTION-LANGU = '1'.
  LW_MATERIALDESCRIPTION-MATL_DESC = US_INPUT-MAKTX.      " 物料描述
  APPEND LW_MATERIALDESCRIPTION TO LT_MATERIALDESCRIPTION.
  CLEAR: LT_UNITSOFMEASURE, LW_UNITSOFMEASURE, LT_UNITSOFMEASUREX, LW_UNITSOFMEASUREX, LT_SMEINH, LS_SMEINH.
  LT_SMEINH = US_INPUT-SMEINH.
  LOOP AT LT_SMEINH INTO LS_SMEINH.
    LW_UNITSOFMEASURE-GROSS_WT = US_INPUT-BRGEW.          " 毛重
    LW_UNITSOFMEASUREX-GROSS_WT = 'X'.
    LW_UNITSOFMEASURE-VOLUME = US_INPUT-VOLUM.            " 体积
    LW_UNITSOFMEASUREX-VOLUME = 'X'.
    LW_UNITSOFMEASURE-UNIT_OF_WT = US_INPUT-GEWEI.        " 重量单位
    LW_UNITSOFMEASUREX-UNIT_OF_WT = 'X'.
    LW_UNITSOFMEASURE-VOLUMEUNIT = US_INPUT-VOLEH.        " 体积单位
    LW_UNITSOFMEASUREX-VOLUMEUNIT = 'X'.
    LW_UNITSOFMEASURE-DENOMINATR = LS_SMEINH-UMREN.       " 转换为基本计量单位的分母
    LW_UNITSOFMEASUREX-DENOMINATR = 'X'.
    LW_UNITSOFMEASURE-ALT_UNIT = LS_SMEINH-MEINH.         " 用于显示的计量单位
    LW_UNITSOFMEASUREX-ALT_UNIT = LS_SMEINH-MEINH.
    LW_UNITSOFMEASURE-NUMERATOR = LS_SMEINH-UMREZ.        " 基本计量单位转换分子
    LW_UNITSOFMEASUREX-NUMERATOR = 'X'.
    APPEND LW_UNITSOFMEASURE TO LT_UNITSOFMEASURE.
    APPEND LW_UNITSOFMEASUREX TO LT_UNITSOFMEASUREX.
    CLEAR: LW_UNITSOFMEASURE, LW_UNITSOFMEASUREX.
  ENDLOOP.
  CLEAR: LW_RETURN.
  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      HEADDATA            = LW_HEADDATA
      CLIENTDATA          = LW_CLIENTDATA
      CLIENTDATAX         = LW_CLIENTDATAX
    IMPORTING
      RETURN              = LW_RETURN
    TABLES
      MATERIALDESCRIPTION = LT_MATERIALDESCRIPTION
      UNITSOFMEASURE      = LT_UNITSOFMEASURE
      UNITSOFMEASUREX     = LT_UNITSOFMEASUREX.
  IF LW_RETURN-TYPE <> 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
    " 扩张工厂视图
    IF US_INPUT-WERKS IS NOT INITIAL.
      PERFORM FRM_PLANT_VIEW USING US_INPUT LV_MATNR CHANGING CS_RETURN.
    ENDIF.
    " 扩张MRP AREA
    IF US_INPUT-MRP_AERA IS NOT INITIAL.
      PERFORM FRM_MRP_AERA_CREATE USING US_INPUT LV_MATNR CHANGING CS_RETURN.
    ENDIF.
    " 扩张销售组织视图
    IF US_INPUT-VKORG IS NOT INITIAL.
      PERFORM FRM_SALESORG_VIEW USING US_INPUT LV_MATNR CHANGING CS_RETURN.
    ENDIF.
    " 维护分类视图
    IF US_INPUT-CLASS IS NOT INITIAL.
      PERFORM FRM_CLASS_VIEW USING US_INPUT LV_MATNR LV_FLAG CHANGING CS_RETURN.
    ENDIF.
    IF CS_RETURN IS INITIAL.
      CS_RETURN-MSG_MATNR = LV_MATNR.
      CS_RETURN-MSG_TYPE = 'S'.
      CS_RETURN-MESSAGE = '成功'.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    CS_RETURN-MSG_MATNR = LV_MATNR.
    CS_RETURN-MSG_TYPE = 'E'.
    CS_RETURN-MESSAGE = LW_RETURN-MESSAGE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_MATNR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_MATNR
*&---------------------------------------------------------------------*
FORM FRM_GET_MATNR  USING    UV_MTART TYPE MTART
                    CHANGING CV_MATNR TYPE MATNR.
  DEFINE MATN1_INPUT.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input         = &1
      IMPORTING
        output        = &1
      EXCEPTIONS
        length_error  = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  END-OF-DEFINITION.
  DATA: LV_MATNR TYPE CHAR18.
  CLEAR: LV_MATNR.
  IF UV_MTART = 'Z010'.
    SELECT MAX( MATNR ) INTO CV_MATNR FROM MARA
      WHERE MATNR BETWEEN '000000000100000001' AND '000000000109999999'.
    IF SY-SUBRC <> 0.
      CV_MATNR = '000000000100000001'.
    ELSE.
      LV_MATNR = CV_MATNR.
      LV_MATNR = LV_MATNR + 1.
      CV_MATNR = LV_MATNR.
      CONDENSE CV_MATNR NO-GAPS.
      MATN1_INPUT: CV_MATNR.
    ENDIF.
  ELSEIF UV_MTART = 'Z020'.
    SELECT MAX( MATNR ) INTO CV_MATNR FROM MARA
      WHERE MATNR BETWEEN '000000000200000001' AND '000000000209999999'.
    IF SY-SUBRC <> 0.
      CV_MATNR = '000000000200000001'.
    ELSE.
      LV_MATNR = CV_MATNR.
      LV_MATNR = LV_MATNR + 1.
      CV_MATNR = LV_MATNR.
      CONDENSE CV_MATNR NO-GAPS.
      MATN1_INPUT: CV_MATNR.
    ENDIF.
  ELSEIF UV_MTART = 'Z030'.
    SELECT MAX( MATNR ) INTO CV_MATNR FROM MARA
      WHERE MATNR BETWEEN '000000000300000001' AND '000000000309999999'.
    IF SY-SUBRC <> 0.
      CV_MATNR = '000000000300000001'.
    ELSE.
      LV_MATNR = CV_MATNR.
      LV_MATNR = LV_MATNR + 1.
      CV_MATNR = LV_MATNR.
      CONDENSE CV_MATNR NO-GAPS.
      MATN1_INPUT: CV_MATNR.
    ENDIF.
  ELSEIF UV_MTART = 'Z040'.
    SELECT MAX( MATNR ) INTO CV_MATNR FROM MARA
      WHERE MATNR BETWEEN '000000000400000001' AND '000000000409999999'.
    IF SY-SUBRC <> 0.
      CV_MATNR = '000000000400000001'.
    ELSE.
      LV_MATNR = CV_MATNR.
      LV_MATNR = LV_MATNR + 1.
      CV_MATNR = LV_MATNR.
      CONDENSE CV_MATNR NO-GAPS.
      MATN1_INPUT: CV_MATNR.
    ENDIF.
  ELSEIF UV_MTART = 'Z050'.
    SELECT MAX( MATNR ) INTO CV_MATNR FROM MARA
      WHERE MATNR BETWEEN '000000000500000001' AND '000000000509999999'.
    IF SY-SUBRC <> 0.
      CV_MATNR = '000000000500000001'.
    ELSE.
      LV_MATNR = CV_MATNR.
      LV_MATNR = LV_MATNR + 1.
      CV_MATNR = LV_MATNR.
      CONDENSE CV_MATNR NO-GAPS.
      MATN1_INPUT: CV_MATNR.
    ENDIF.
  ELSEIF UV_MTART = 'Z060'.
    SELECT MAX( MATNR ) INTO CV_MATNR FROM MARA
      WHERE MATNR BETWEEN '000000000600000001' AND '000000000609999999'.
    IF SY-SUBRC <> 0.
      CV_MATNR = '000000000600000001'.
    ELSE.
      LV_MATNR = CV_MATNR.
      LV_MATNR = LV_MATNR + 1.
      CV_MATNR = LV_MATNR.
      CONDENSE CV_MATNR NO-GAPS.
      MATN1_INPUT: CV_MATNR.
    ENDIF.
  ELSEIF UV_MTART = 'Z070'.
    SELECT MAX( MATNR ) INTO CV_MATNR FROM MARA
      WHERE MATNR BETWEEN '000000000700000001' AND '000000000709999999'.
    IF SY-SUBRC <> 0.
      CV_MATNR = '000000000700000001'.
    ELSE.
      LV_MATNR = CV_MATNR.
      LV_MATNR = LV_MATNR + 1.
      CV_MATNR = LV_MATNR.
      CONDENSE CV_MATNR NO-GAPS.
      MATN1_INPUT: CV_MATNR.
    ENDIF.
  ELSEIF UV_MTART = 'Z080'.
    SELECT MAX( MATNR ) INTO CV_MATNR FROM MARA
      WHERE MATNR BETWEEN '000000000800000001' AND '000000000809999999'.
    IF SY-SUBRC <> 0.
      CV_MATNR = '000000000800000001'.
    ELSE.
      LV_MATNR = CV_MATNR.
      LV_MATNR = LV_MATNR + 1.
      CV_MATNR = LV_MATNR.
      CONDENSE CV_MATNR NO-GAPS.
      MATN1_INPUT: CV_MATNR.
    ENDIF.
  ELSEIF UV_MTART = 'Z090'.
    SELECT MAX( MATNR ) INTO CV_MATNR FROM MARA
      WHERE MATNR BETWEEN '000000000900000001' AND '000000000909999999'.
    IF SY-SUBRC <> 0.
      CV_MATNR = '000000000900000001'.
    ELSE.
      LV_MATNR = CV_MATNR.
      LV_MATNR = LV_MATNR + 1.
      CV_MATNR = LV_MATNR.
      CONDENSE CV_MATNR NO-GAPS.
      MATN1_INPUT: CV_MATNR.
    ENDIF.
  ELSEIF UV_MTART = 'Z098'.
    SELECT MAX( MATNR ) INTO CV_MATNR FROM MARA
      WHERE MATNR BETWEEN '000000000980000001' AND '000000000989999999'.
    IF SY-SUBRC <> 0.
      CV_MATNR = '000000000980000001'.
    ELSE.
      LV_MATNR = CV_MATNR.
      LV_MATNR = LV_MATNR + 1.
      CV_MATNR = LV_MATNR.
      CONDENSE CV_MATNR NO-GAPS.
      MATN1_INPUT: CV_MATNR.
    ENDIF.
  ELSEIF UV_MTART = 'Z099'.
    SELECT MAX( MATNR ) INTO CV_MATNR FROM MARA
      WHERE MATNR BETWEEN '000000000990000001' AND '000000000999999999'.
    IF SY-SUBRC <> 0.
      CV_MATNR = '000000000990000001'.
    ELSE.
      LV_MATNR = CV_MATNR.
      LV_MATNR = LV_MATNR + 1.
      CV_MATNR = LV_MATNR.
      CONDENSE CV_MATNR NO-GAPS.
      MATN1_INPUT: CV_MATNR.
    ENDIF.
  ELSEIF UV_MTART = 'HERS'.
    SELECT MAX( MATNR ) INTO CV_MATNR FROM MARA
      WHERE MATNR BETWEEN '000000001000000001' AND '000000001099999999'.
    IF SY-SUBRC <> 0.
      CV_MATNR = '000000001000000001'.
    ELSE.
      LV_MATNR = CV_MATNR.
      LV_MATNR = LV_MATNR + 1.
      CV_MATNR = LV_MATNR.
      CONDENSE CV_MATNR NO-GAPS.
      MATN1_INPUT: CV_MATNR.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PLANT_VIEW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> US_INPUT
*&---------------------------------------------------------------------*
FORM FRM_PLANT_VIEW  USING    US_INPUT  TYPE ZDT_OA2SAP_MDM_MARA
                              UV_MATNR  TYPE MATNR
                     CHANGING CS_RETURN TYPE ZDT_OA2SAP_MDM_RET_ZMD001.
  DEFINE MATN1_INPUT.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input           = &1
      IMPORTING
        output          = &1
      EXCEPTIONS
        length_error    = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.
  END-OF-DEFINITION.

  DATA: LT_WERKS   TYPE ZDT_OA2SAP_MDM_MARA_WERKS_TAB,
        LS_WERKS   TYPE ZDT_OA2SAP_MDM_MARA_WERKS,
        LT_QMAT    TYPE ZDT_OA2SAP_MDM_MARA_WERKS__TAB,
        LS_QMAT    TYPE ZDT_OA2SAP_MDM_MARA_WERKS_QMAT,
        LT_TQ34    TYPE STANDARD TABLE OF TQ34,
        LW_TQ34    TYPE TQ34,
        LV_MATNR   TYPE MATNR18,
        LV_NFMAT40 TYPE NFMAT40,
        LV_MESS    TYPE C LENGTH 1000,
        LV_MESS1   TYPE C LENGTH 1000.
  DATA: LW_HEADDATA       TYPE BAPIMATHEAD,
        LW_CLIENTDATA     TYPE BAPI_MARA,
        LW_CLIENTDATAX    TYPE BAPI_MARAX,
        LT_RETURN         TYPE STANDARD TABLE OF BAPIRET2,
        LW_RETURN         TYPE BAPIRET2,
        LW_PLANTDATA      TYPE BAPI_MARC,
        LW_PLANTDATAX     TYPE BAPI_MARCX,
        LW_VALUATIONDATA  TYPE BAPI_MBEW,
        LW_VALUATIONDATAX TYPE BAPI_MBEWX.
  DATA: LT_INSPECTIONCTRL TYPE STANDARD TABLE OF BAPI1001004_QMAT,
        LW_INSPECTIONCTRL TYPE BAPI1001004_QMAT.

  DATA: LS_MARC TYPE MARC,
        LS_MBEW TYPE MBEW,
        LS_MARA TYPE MARA.


  " 检验类型缺省值
  CLEAR: LT_TQ34.
  SELECT * FROM TQ34 INTO TABLE LT_TQ34.

  CLEAR: LT_WERKS, LS_WERKS, LV_MESS.
  LT_WERKS = US_INPUT-WERKS.


  LOOP AT LT_WERKS INTO LS_WERKS.
    IF LS_WERKS-WERKS IS NOT INITIAL.


***************** add by sujx 20191201**********************************
      CLEAR: LS_MBEW, LS_MARC, LS_MARA.

      SELECT SINGLE * INTO LS_MBEW
        FROM MBEW
       WHERE MATNR = UV_MATNR
         AND BWKEY = LS_WERKS-WERKS.
      IF SY-SUBRC <> 0.
        CLEAR: LS_MBEW.
      ENDIF.

      SELECT SINGLE * INTO LS_MARC
        FROM MARC
       WHERE MATNR = UV_MATNR
         AND WERKS = LS_WERKS-WERKS.
      IF SY-SUBRC <> 0.
        CLEAR: LS_MARC.
      ENDIF.

      SELECT SINGLE * INTO LS_MARA
        FROM MARA
       WHERE MATNR = UV_MATNR.
      IF SY-SUBRC <> 0.
        CLEAR: LS_MARA.
      ENDIF.

******************* end add ******************************************


      CLEAR: LW_HEADDATA.
      LW_HEADDATA-MATERIAL = UV_MATNR.                                    " 物料号
      LW_HEADDATA-MATL_TYPE = US_INPUT-MTART.                             " 物料类型
      LW_HEADDATA-IND_SECTOR = 'P'.                                       " 行业领域
      LW_HEADDATA-PURCHASE_VIEW = 'X'.                                    " 采购视图
      IF US_INPUT-MTART <> 'Z080'
        AND US_INPUT-MTART <> 'Z098'.
*        AND us_input-mtart <> 'Z099'.
        LW_HEADDATA-MRP_VIEW = 'X'.                                       " MRP视图
      ENDIF.
      LW_HEADDATA-WORK_SCHED_VIEW = 'X'.                                  " 工作计划视图
      IF US_INPUT-MTART <> 'Z099'.
        LW_HEADDATA-ACCOUNT_VIEW = 'X'.                                   " 会计视图
        LW_HEADDATA-STORAGE_VIEW = 'X'.                                   " 库存视图
        LW_HEADDATA-COST_VIEW = 'X'.                                      " 成本视图
      ENDIF.
      IF US_INPUT-MTART <> 'Z040'
        AND US_INPUT-MTART <> 'Z099'.
        LW_HEADDATA-QUALITY_VIEW = 'X'.                                   " 质量管理视图
      ENDIF.
      CLEAR: LW_CLIENTDATA, LW_CLIENTDATAX.

      " 温度条件
      IF LS_WERKS-TEMPB <> LS_MARA-TEMPB OR LS_MARA IS INITIAL.
        LW_CLIENTDATA-TEMP_CONDS = LS_WERKS-TEMPB.
        LW_CLIENTDATAX-TEMP_CONDS = 'X'.
      ENDIF.

      " 存储条件
      IF LS_WERKS-RAUBE <> LS_MARA-RAUBE OR LS_MARA IS INITIAL.
        LW_CLIENTDATA-STOR_CONDS = LS_WERKS-RAUBE.
        LW_CLIENTDATAX-STOR_CONDS = 'X'.
      ENDIF.

      " 最小剩余货架寿命
      IF LS_WERKS-MHDRZ <> LS_MARA-MHDRZ OR LS_MARA IS INITIAL.
        LW_CLIENTDATA-MINREMLIFE = LS_WERKS-MHDRZ.
        LW_CLIENTDATAX-MINREMLIFE = 'X'.
      ENDIF.

*      lw_clientdata-temp_conds = ls_werks-tempb.                          " 温度条件
*      lw_clientdata-stor_conds = ls_werks-raube.                          " 存储条件
*      lw_clientdata-minremlife = ls_werks-mhdrz.                          " 最小剩余货架寿命
      " 总货架寿命
      IF US_INPUT-MTART <> 'Z010'
        AND US_INPUT-MTART <> 'Z020'
        AND US_INPUT-MTART <> 'Z040'.
        IF LS_WERKS-MHDHB <> LS_MARA-MHDHB OR LS_MARA IS INITIAL.
          LW_CLIENTDATA-SHELF_LIFE = LS_WERKS-MHDHB.
          LW_CLIENTDATAX-SHELF_LIFE = 'X'.
        ENDIF.
*        lw_clientdata-shelf_life = ls_werks-mhdhb.                        " 总货架寿命
*        lw_clientdatax-shelf_life = 'X'.
      ENDIF.
      " 激活采购中的 QM
      IF US_INPUT-MTART = 'Z010'
        OR US_INPUT-MTART = 'Z020'.
        IF LS_MARA-QMPUR IS INITIAL OR LS_MARA IS INITIAL.
          LW_CLIENTDATA-QM_PROCMNT = 'X'.
          LW_CLIENTDATAX-QM_PROCMNT = 'X'.
        ENDIF.
*        lw_clientdata-qm_procmnt = 'X'.                                   " 激活采购中的 QM
*        lw_clientdatax-qm_procmnt = 'X'.
      ENDIF.

      " 期间标识
      IF LS_WERKS-IPRKZ <> LS_MARA-IPRKZ OR LS_MARA IS INITIAL.
        LW_CLIENTDATA-PERIOD_IND_EXPIRATION_DATE = LS_WERKS-IPRKZ.
        LW_CLIENTDATAX-PERIOD_IND_EXPIRATION_DATE = 'X'.
      ENDIF.

      " 舍入规则
      IF LS_WERKS-RDMHD <> LS_MARA-RDMHD OR LS_MARA IS INITIAL.
        LW_CLIENTDATA-ROUND_UP_RULE_EXPIRATION_DATE = LS_WERKS-RDMHD.
        LW_CLIENTDATAX-ROUND_UP_RULE_EXPIRATION_DATE = 'X'.
      ENDIF.

      " 采购价值代码
      IF US_INPUT-EKWSL <> LS_MARA-EKWSL OR LS_MARA IS INITIAL.
        LW_CLIENTDATA-PUR_VALKEY = US_INPUT-EKWSL.
        LW_CLIENTDATAX-PUR_VALKEY = 'X'.
      ENDIF.
*      lw_clientdata-period_ind_expiration_date = ls_werks-iprkz.          " 期间标识
*      lw_clientdata-round_up_rule_expiration_date = ls_werks-rdmhd.       " 舍入规则
*      lw_clientdata-pur_valkey = us_input-ekwsl.                          " 采购价值代码
*      lw_clientdatax-pur_valkey = 'X'.
*      lw_clientdatax-temp_conds = 'X'.
*      lw_clientdatax-stor_conds = 'X'.
*      lw_clientdatax-minremlife = 'X'.
*      lw_clientdatax-period_ind_expiration_date = 'X'.
*      lw_clientdatax-round_up_rule_expiration_date = 'X'.
********************因为特殊采购标识字段作废，所以下面这段逻辑判断作废*******
      " 制造商部件的参数文件
*      IF us_input-mtart = 'Z010' AND ls_werks-sobsl IS INITIAL.
      IF LS_MARA-MPROF <> LS_MARA-MPROF OR LS_MARA IS INITIAL.
        LW_CLIENTDATA-MANUF_PROF = US_INPUT-MPROF.
        LW_CLIENTDATAX-MANUF_PROF = 'X'.
      ENDIF.
**        lw_clientdata-manuf_prof = 'IMPN'.
**        lw_clientdatax-manuf_prof = 'X'.
*      ENDIF.
*******************作废结束*************************************************
      CLEAR: LW_PLANTDATA, LW_PLANTDATAX.
      LW_PLANTDATA-PLANT = LS_WERKS-WERKS.                              " 工厂
      LW_PLANTDATAX-PLANT = LS_WERKS-WERKS.
      LW_PLANTDATA-DEL_FLAG = LS_WERKS-LVOMA.                           " 删除标记
      LW_PLANTDATAX-DEL_FLAG = 'X'.

*      CRIT_PART
      " 采购组
      IF LS_MARC-EKGRP <> LS_WERKS-EKGRP OR LS_MARC IS INITIAL.
        LW_PLANTDATA-PUR_GROUP = LS_WERKS-EKGRP.
        LW_PLANTDATAX-PUR_GROUP = 'X'.
      ENDIF.
      " 源清单
      IF LS_MARC-KORDB <> LS_WERKS-KORDB OR LS_MARC IS INITIAL.
        LW_PLANTDATA-SOURCELIST = LS_WERKS-KORDB.
        LW_PLANTDATAX-SOURCELIST = 'X'.
      ENDIF.
      " 批次管理
      IF LS_MARC-XCHPF <> LS_WERKS-XCHPF OR LS_MARC IS INITIAL.
        LW_PLANTDATA-BATCH_MGMT = LS_WERKS-XCHPF.
        LW_PLANTDATAX-BATCH_MGMT = 'X'.
      ENDIF.
      " 利润中心
      IF LS_MARC-PRCTR <> LS_WERKS-PRCTL OR LS_MARC IS INITIAL.
        LW_PLANTDATA-PROFIT_CTR = LS_WERKS-PRCTL.
        LW_PLANTDATAX-PROFIT_CTR = 'X'.
      ENDIF.
      " 不计算成本
      IF LS_MARC-NCOST <> LS_WERKS-NCOST OR LS_MARC IS INITIAL.
        LW_PLANTDATA-NO_COSTING = LS_WERKS-NCOST.
        LW_PLANTDATAX-NO_COSTING = 'X'.
      ENDIF.
      " 特定工厂的物料状态
      IF LS_MARC-MMSTA <> LS_WERKS-MMSTA OR LS_MARC IS INITIAL.
        LW_PLANTDATA-PUR_STATUS = LS_WERKS-MMSTA.
        LW_PLANTDATAX-PUR_STATUS = 'X'.
      ENDIF.
      " 特定工厂的物料状态
      IF LS_MARC-MMSTD <> LS_WERKS-MMSTD OR LS_MARC IS INITIAL.
        LW_PLANTDATA-PVALIDFROM = LS_WERKS-MMSTD.
        LW_PLANTDATAX-PVALIDFROM = 'X'.
      ENDIF.
      " 最大批量大小
      IF LS_MARC-BSTMA <> LS_WERKS-BSTMA OR LS_MARC IS INITIAL.
        LW_PLANTDATA-MAXLOTSIZE = LS_WERKS-BSTMA.
        LW_PLANTDATAX-MAXLOTSIZE = 'X'.
      ENDIF.
      " 最小批量大小
      IF LS_MARC-BSTMI <> LS_WERKS-BSTMI OR LS_MARC IS INITIAL.
        LW_PLANTDATA-MINLOTSIZE = LS_WERKS-BSTMI.
        LW_PLANTDATAX-MINLOTSIZE = 'X'.
      ENDIF.
      " 舍入值
      IF LS_MARC-BSTRF <> LS_WERKS-BSTRF OR LS_MARC IS INITIAL.
        LW_PLANTDATA-ROUND_VAL = LS_WERKS-BSTRF.
        LW_PLANTDATAX-ROUND_VAL = 'X'.
      ENDIF.
      " 计划交货时间
      IF LS_MARC-PLIFZ <> LS_WERKS-PLIFZ OR LS_MARC IS INITIAL.
        LW_PLANTDATA-PLND_DELRY = LS_WERKS-PLIFZ.
        LW_PLANTDATAX-PLND_DELRY = 'X'.
      ENDIF.
      " MRP类型
      IF LS_MARC-DISMM <> LS_WERKS-DISMM OR LS_MARC IS INITIAL.
        LW_PLANTDATA-MRP_TYPE = LS_WERKS-DISMM.
        LW_PLANTDATAX-MRP_TYPE = 'X'.
      ENDIF.
      " MRP控制者/物料需求计划控制员
      IF LS_MARC-DISPO <> LS_WERKS-DISPO OR LS_MARC IS INITIAL.
        LW_PLANTDATA-MRP_CTRLER = LS_WERKS-DISPO.
        LW_PLANTDATAX-MRP_CTRLER = 'X'.
      ENDIF.
      " 批量大小
      IF LS_MARC-DISLS <> LS_WERKS-DISLS OR LS_MARC IS INITIAL.
        LW_PLANTDATA-LOTSIZEKEY = LS_WERKS-DISLS.
        LW_PLANTDATAX-LOTSIZEKEY = 'X'.
      ENDIF.
      " 采购类型
      IF LS_MARC-BESKZ <> LS_WERKS-BESKZ OR LS_MARC IS INITIAL.
        LW_PLANTDATA-PROC_TYPE = LS_WERKS-BESKZ.
        LW_PLANTDATAX-PROC_TYPE = 'X'.
      ENDIF.
      " 特殊采购类
      IF LS_MARC-SOBSL <> LS_WERKS-SOBSL OR LS_MARC IS INITIAL.
        LW_PLANTDATA-SPPROCTYPE = LS_WERKS-SOBSL.
        LW_PLANTDATAX-SPPROCTYPE = 'X'.
      ENDIF.
      " 安全库存
      IF LS_MARC-EISBE <> LS_WERKS-EISBE OR LS_MARC IS INITIAL.
        LW_PLANTDATA-SAFETY_STK = LS_WERKS-EISBE.
        LW_PLANTDATAX-SAFETY_STK = 'X'.
      ENDIF.
      " 策略组
      IF LS_MARC-STRGR <> LS_WERKS-STRGR OR LS_MARC IS INITIAL.
        LW_PLANTDATA-PLAN_STRGP = LS_WERKS-STRGR.
        LW_PLANTDATAX-PLAN_STRGP = 'X'.
      ENDIF.
      " 不足交货容差
      IF LS_MARC-UNETO <> LS_WERKS-UNETO OR LS_MARC IS INITIAL.
        LW_PLANTDATA-UNDER_TOL = LS_WERKS-UNETO.
        LW_PLANTDATAX-UNDER_TOL = 'X'.
      ENDIF.
      " 厂内生产时间
      IF LS_MARC-DZEIT <> LS_WERKS-DZEIT OR LS_MARC IS INITIAL.
        LW_PLANTDATA-INHSEPRODT = LS_WERKS-DZEIT.
        LW_PLANTDATAX-INHSEPRODT = 'X'.
      ENDIF.
      " 固定批量
      IF LS_MARC-BSTFE <> LS_WERKS-BSTFE OR LS_MARC IS INITIAL.
        LW_PLANTDATA-FIXED_LOT = LS_WERKS-BSTFE.
        LW_PLANTDATAX-FIXED_LOT = 'X'.
      ENDIF.
      " 过度交货容差
      IF LS_MARC-UEETO <> LS_WERKS-UEETO OR LS_MARC IS INITIAL.
        LW_PLANTDATA-OVER_TOL = LS_WERKS-UEETO.
        LW_PLANTDATAX-OVER_TOL = 'X'.
      ENDIF.
      " 检查间隔
      IF LS_MARC-PRFRQ <> LS_WERKS-PRFRQ OR LS_MARC IS INITIAL.
        LW_PLANTDATA-INSP_INT = LS_WERKS-PRFRQ.
        LW_PLANTDATAX-INSP_INT = 'X'.
      ENDIF.
      " MRP组
      IF LS_MARC-DISGR <> LS_WERKS-DISGR OR LS_MARC IS INITIAL.
        LW_PLANTDATA-MRP_GROUP = LS_WERKS-DISGR.
        LW_PLANTDATAX-MRP_GROUP = 'X'.
      ENDIF.
      " 计划时界
      IF LS_MARC-FXHOR <> LS_WERKS-FXHOR OR LS_MARC IS INITIAL.
        LW_PLANTDATA-PL_TI_FNCE = LS_WERKS-FXHOR.
        LW_PLANTDATAX-PL_TI_FNCE = 'X'.
      ENDIF.
      " 反冲
      IF LS_MARC-RGEKZ <> LS_WERKS-RGEKZ OR LS_MARC IS INITIAL.
        LW_PLANTDATA-BACKFLUSH = LS_WERKS-RGEKZ.
        LW_PLANTDATAX-BACKFLUSH = 'X'.
      ENDIF.
      " 计划边际码
      IF LS_MARC-FHORI <> LS_WERKS-FHORI OR LS_MARC IS INITIAL.
        LW_PLANTDATA-SM_KEY = LS_WERKS-FHORI.
        LW_PLANTDATAX-SM_KEY = 'X'.
      ENDIF.
      " 消耗模式
      IF LS_MARC-VRMOD <> LS_WERKS-VRMOD OR LS_MARC IS INITIAL.
        LW_PLANTDATA-CONSUMMODE = LS_WERKS-VRMOD.
        LW_PLANTDATAX-CONSUMMODE = 'X'.
      ENDIF.
      " 向前消耗时间
      IF LS_MARC-VINT2 <> LS_WERKS-VINT1 OR LS_MARC IS INITIAL.
        LW_PLANTDATA-FWD_CONS = LS_WERKS-VINT1.
        LW_PLANTDATAX-FWD_CONS = 'X'.
      ENDIF.
      " 逆向消耗时间
      IF LS_MARC-VINT1 <> LS_WERKS-VINT2 OR LS_MARC IS INITIAL.
        LW_PLANTDATA-BWD_CONS = LS_WERKS-VINT2.
        LW_PLANTDATAX-BWD_CONS = 'X'.
      ENDIF.
      " 独立/集中
      IF LS_MARC-SBDKZ <> LS_WERKS-SBDKZ OR LS_MARC IS INITIAL.
        LW_PLANTDATA-DEP_REQ_ID = LS_WERKS-SBDKZ.
        LW_PLANTDATAX-DEP_REQ_ID = 'X'.
      ENDIF.
      " 非连续标识
      IF LS_MARC-KZAUS <> LS_WERKS-KZAUS OR LS_MARC IS INITIAL.
        LW_PLANTDATA-DISCONTINU = LS_WERKS-KZAUS.
        LW_PLANTDATAX-DISCONTINU = 'X'.
      ENDIF.
      " 生效期
      IF LS_MARC-AUSDT <> LS_WERKS-AUSDT OR LS_MARC IS INITIAL.
        LW_PLANTDATA-EFF_O_DAY = LS_WERKS-AUSDT.
        LW_PLANTDATAX-EFF_O_DAY = 'X'.
      ENDIF.
      " 后继物料
      CLEAR: LV_NFMAT40.
      LV_NFMAT40 = LS_WERKS-NFMAT.
      MATN1_INPUT: LV_NFMAT40.
      IF LS_MARC-NFMAT <> LV_NFMAT40 OR LS_MARC IS INITIAL.
        LW_PLANTDATA-FOLLOW_UP_LONG = LV_NFMAT40.
        LW_PLANTDATAX-FOLLOW_UP_LONG = 'X'.
      ENDIF.
      " 生产管理员
      IF LS_MARC-FEVOR <> LS_WERKS-FEVOR OR LS_MARC IS INITIAL.
        LW_PLANTDATA-PRODUCTION_SCHEDULER = LS_WERKS-FEVOR.
        LW_PLANTDATAX-PRODUCTION_SCHEDULER = 'X'.
      ENDIF.
      " 生产计划参数文件
      IF LS_MARC-SFCPF <> LS_WERKS-SFCPF OR LS_MARC IS INITIAL.
        LW_PLANTDATA-PRODPROF = LS_WERKS-SFCPF.
        LW_PLANTDATAX-PRODPROF = 'X'.
      ENDIF.
      " 处理时间
      IF LS_MARC-BEARZ <> LS_WERKS-BEARZ OR LS_MARC IS INITIAL.
        LW_PLANTDATA-PROC_TIME = LS_WERKS-BEARZ.
        LW_PLANTDATAX-PROC_TIME = 'X'.
      ENDIF.
      " 装配报废比例
      IF LS_MARC-AUSSS <> LS_WERKS-AUSSS OR LS_MARC IS INITIAL.
        LW_PLANTDATA-ASSY_SCRAP = LS_WERKS-AUSSS.
        LW_PLANTDATAX-ASSY_SCRAP = 'X'.
      ENDIF.
      " 外部采购仓储地点
      IF LS_MARC-LGFSB <> LS_WERKS-LGFSB OR LS_MARC IS INITIAL.
        LW_PLANTDATA-SLOC_EXPRC = LS_WERKS-LGFSB.
        LW_PLANTDATAX-SLOC_EXPRC = 'X'.
      ENDIF.
      " 准备时间
      IF LS_MARC-RUEZT <> LS_WERKS-RUEZT OR LS_MARC IS INITIAL.
        LW_PLANTDATA-SETUPTIME = LS_WERKS-RUEZT.
        LW_PLANTDATAX-SETUPTIME = 'X'.
      ENDIF.
      " 收货处理时间
      IF LS_MARC-WEBAZ <> LS_WERKS-WEBAZ OR LS_MARC IS INITIAL.
        LW_PLANTDATA-GR_PR_TIME = LS_WERKS-WEBAZ.
        LW_PLANTDATAX-GR_PR_TIME = 'X'.
      ENDIF.
      " 批量产品成本核算
      IF LS_MARC-LOSGR <> LS_WERKS-LOSGR OR LS_MARC IS INITIAL.
        LW_PLANTDATA-LOT_SIZE = LS_WERKS-LOSGR.
        LW_PLANTDATAX-LOT_SIZE = 'X'.
      ENDIF.
      " 凭证需求
      IF LS_MARC-KZDKZ IS INITIAL OR LS_MARC IS INITIAL.
        LW_PLANTDATA-DOC_REQD = 'X'.
        LW_PLANTDATAX-DOC_REQD = 'X'.
      ENDIF.
      " 可用性检查
      IF LS_MARC-MTVFP <> LS_WERKS-MTVFP OR LS_MARC IS INITIAL.
        LW_PLANTDATA-AVAILCHECK = LS_WERKS-MTVFP.
        LW_PLANTDATAX-AVAILCHECK = 'X'.
      ENDIF.
      IF US_INPUT-MTART = 'Z010'
        OR US_INPUT-MTART = 'Z020'.
        " 采购中质量管理的控制码
        IF LS_MARC-SSQSS IS INITIAL OR LS_MARC IS INITIAL.
          LW_PLANTDATA-CTRL_KEY = 'ZQMCER'.
          LW_PLANTDATAX-CTRL_KEY = 'X'.
        ENDIF.
        " 证书类型
        IF LS_MARC-QZGTP IS INITIAL OR LS_MARC IS INITIAL.
          LW_PLANTDATA-CERT_TYPE = 'Z001'.
          LW_PLANTDATAX-CERT_TYPE = 'X'.
        ENDIF.
      ENDIF.
      "ADD BY LANGJINGCUN 20200409 增加字段 bengin
      " 关键部件
      IF LS_MARC-KZKRI <> LS_WERKS-KZKRI OR LS_MARC IS INITIAL.
        LW_PLANTDATA-CRIT_PART = LS_WERKS-KZKRI.
        LW_PLANTDATAX-CRIT_PART = 'X'.
      ENDIF.
      "ADD BY LANGJINGCUN 20200409 增加字段 end




*********************************Seashell Huang 20191219  质量管理视图添加“QM物料授权"
      LW_PLANTDATA-QM_AUTHGRP = 'ZQM001'.
      LW_PLANTDATAX-QM_AUTHGRP = 'X'.
******************************END*****************************************************
*      lw_plantdata-pur_group = ls_werks-ekgrp.                          " 采购组
*      lw_plantdata-sourcelist = ls_werks-kordb.                         " 源清单
*      lw_plantdata-batch_mgmt = ls_werks-xchpf.                         " 批次管理
*      lw_plantdata-profit_ctr = ls_werks-prctl.                         " 利润中心
*      lw_plantdata-no_costing = ls_werks-ncost.                         " 不计算成本
*      lw_plantdata-pur_status = ls_werks-mmsta.                         " 特定工厂的物料状态
*      lw_plantdata-pvalidfrom = ls_werks-mmstd.                         " 有效起始期
*      lw_plantdata-maxlotsize = ls_werks-bstma.                         " 最大批量大小
*      lw_plantdata-minlotsize = ls_werks-bstmi.                         " 最小批量大小
*      lw_plantdata-round_val = ls_werks-bstrf.                          " 舍入值
*      lw_plantdata-plnd_delry = ls_werks-plifz.                         " 计划交货时间
*      lw_plantdata-mrp_type = ls_werks-dismm.                           " MRP类型
*      lw_plantdata-mrp_ctrler = ls_werks-dispo.                         " MRP控制者/物料需求计划控制员
*      lw_plantdata-lotsizekey = ls_werks-disls.                         " 批量大小
*      lw_plantdata-proc_type = ls_werks-beskz.                          " 采购类型
*      lw_plantdata-spproctype = ls_werks-sobsl.                         " 特殊采购类
*      lw_plantdata-safety_stk = ls_werks-eisbe.                         " 安全库存
*      lw_plantdata-plan_strgp = ls_werks-strgr.                         " 策略组
*      lw_plantdata-under_tol = ls_werks-uneto.                          " 不足交货容差
*      lw_plantdata-inhseprodt = ls_werks-dzeit.                         " 厂内生产时间
*      lw_plantdata-fixed_lot = ls_werks-bstfe.                          " 固定批量
*      lw_plantdata-over_tol = ls_werks-ueeto.                           " 过度交货容差
*      LW_PLANTDATA-CERT_TYPE = LS_WERKS-QZGTYP.                         " 证书类型
*      lw_plantdata-insp_int = ls_werks-prfrq.                           " 检查间隔
*      lw_plantdata-mrp_group = ls_werks-disgr.                          " MRP组
*      lw_plantdata-pl_ti_fnce = ls_werks-fxhor.                         " 计划时界
*      lw_plantdata-backflush = ls_werks-rgekz.                          " 反冲
*      lw_plantdata-sm_key = ls_werks-fhori.                             " 计划边际码
*      lw_plantdata-consummode = ls_werks-vrmod.                         " 消耗模式
*      lw_plantdata-fwd_cons = ls_werks-vint1.                           " 向前消耗时间
*      lw_plantdata-bwd_cons = ls_werks-vint2.                           " 逆向消耗时间
*      lw_plantdata-dep_req_id = ls_werks-sbdkz.                         " 独立/集中
*      lw_plantdata-discontinu = ls_werks-kzaus.                         " 非连续标识
*      lw_plantdata-eff_o_day = ls_werks-ausdt.                          " 生效期
*      lw_plantdata-follow_up_long = ls_werks-nfmat.                     " 后继物料
*      lw_plantdata-production_scheduler = ls_werks-fevor.               " 生产管理员
*      lw_plantdata-prodprof = ls_werks-sfcpf.                           " 生产计划参数文件
*      lw_plantdata-proc_time = ls_werks-bearz.                          " 处理时间
*      lw_plantdata-assy_scrap = ls_werks-ausss.                         " 装配报废比例
*      lw_plantdata-sloc_exprc = ls_werks-lgfsb.                         " 外部采购仓储地点
*      lw_plantdata-setuptime = ls_werks-ruezt.                          " 准备时间
*      lw_plantdata-gr_pr_time = ls_werks-webaz.                         " 收货处理时间
*      lw_plantdata-lot_size = ls_werks-peinh_1.                         " 批量产品成本核算
*      lw_plantdata-doc_reqd = 'X'.                                      " 凭证需求
*      lw_plantdata-availcheck = '02'.                                   " 可用性检查

*      lw_plantdatax-plant = ls_werks-werks.
*      lw_plantdatax-del_flag = 'X'.
*      lw_plantdatax-batch_mgmt = 'X'.
*      lw_plantdatax-no_costing = 'X'.
*      lw_plantdatax-gr_pr_time = 'X'.
*      lw_plantdatax-pur_group = 'X'.
*      lw_plantdatax-sourcelist = 'X'.
*      lw_plantdatax-pur_status = 'X'.
*      lw_plantdatax-pvalidfrom = 'X'.
*      lw_plantdatax-minlotsize = 'X'.
*      lw_plantdatax-maxlotsize = 'X'.
*      lw_plantdatax-round_val = 'X'.
*      lw_plantdatax-plnd_delry = 'X'.
*      lw_plantdatax-mrp_ctrler = 'X'.
*      lw_plantdatax-lotsizekey = 'X'.
*      lw_plantdatax-spproctype = 'X'.
*      lw_plantdatax-safety_stk = 'X'.
*      lw_plantdatax-plan_strgp = 'X'.
*      lw_plantdatax-under_tol = 'X'.
*      lw_plantdatax-inhseprodt = 'X'.
*      lw_plantdatax-fixed_lot = 'X'.
*      lw_plantdatax-over_tol = 'X'.
**      LW_PLANTDATAX-CERT_TYPE = 'X'.
*      lw_plantdatax-insp_int = 'X'.
*      lw_plantdatax-mrp_group = 'X'.
*      lw_plantdatax-pl_ti_fnce = 'X'.
*      lw_plantdatax-backflush = 'X'.
*      lw_plantdatax-sm_key = 'X'.
*      lw_plantdatax-consummode = 'X'.
*      lw_plantdatax-fwd_cons = 'X'.
*      lw_plantdatax-bwd_cons = 'X'.
*      lw_plantdatax-dep_req_id = 'X'.
*      lw_plantdatax-discontinu = 'X'.
*      lw_plantdatax-eff_o_day = 'X'.
*      lw_plantdatax-follow_up = 'X'.
*      lw_plantdatax-production_scheduler = 'X'.
*      lw_plantdatax-prodprof = 'X'.
*      lw_plantdatax-proc_time = 'X'.
*      lw_plantdatax-assy_scrap = 'X'.
*      lw_plantdatax-sloc_exprc = 'X'.
*      lw_plantdatax-setuptime = 'X'.
*      lw_plantdatax-lot_size = 'X'.
*      IF lw_plantdata-profit_ctr IS NOT INITIAL.
*        lw_plantdatax-profit_ctr = 'X'.
*      ENDIF.
*      IF lw_plantdata-doc_reqd IS NOT INITIAL.
*        lw_plantdatax-doc_reqd = 'X'.
*      ENDIF.
*      IF lw_plantdata-availcheck IS NOT INITIAL.
*        lw_plantdatax-availcheck = 'X'.
*      ENDIF.
*      IF lw_plantdata-mrp_type IS NOT INITIAL.
*        lw_plantdatax-mrp_type = 'X'.
*      ENDIF.
*      IF lw_plantdata-proc_type IS NOT INITIAL.
*        lw_plantdatax-proc_type = 'X'.
*      ENDIF.

      IF US_INPUT-MTART = 'Z099'.
        IF LS_WERKS-DISMM IS INITIAL AND LS_MARC-DISMM <> LS_WERKS-DISMM.
          LW_PLANTDATA-MRP_TYPE = 'ND'.
          LW_PLANTDATAX-MRP_TYPE = 'X'.
          LW_PLANTDATA-MRP_CTRLER = ''.
          LW_PLANTDATAX-MRP_CTRLER = 'X'.
          LW_PLANTDATA-LOTSIZEKEY = ''.
          LW_PLANTDATAX-LOTSIZEKEY = 'X'.
        ENDIF.
        IF LS_WERKS-BESKZ IS INITIAL AND LS_MARC-BESKZ <> 'F'.
          LW_PLANTDATA-PROC_TYPE = 'F'.
          LW_PLANTDATAX-PROC_TYPE = 'X'.
        ENDIF.
        IF LS_WERKS-SOBSL IS INITIAL AND LS_MARC-SOBSL <> '50'.
          LW_PLANTDATA-SPPROCTYPE = '50'.
          LW_PLANTDATAX-SPPROCTYPE = 'X'.
        ENDIF.
        IF LS_WERKS-SBDKZ IS INITIAL AND LS_MARC-SBDKZ <> '2'.
          LW_PLANTDATA-DEP_REQ_ID = '2'.
          LW_PLANTDATAX-DEP_REQ_ID = 'X'.
        ENDIF.
      ENDIF.

      CLEAR: LW_VALUATIONDATA, LW_VALUATIONDATAX.
      LW_VALUATIONDATA-VAL_AREA = LS_WERKS-WERKS.                       " 估价范围
      LW_VALUATIONDATAX-VAL_AREA = LS_WERKS-WERKS.
      " 评估分类
      IF LS_MBEW-BKLAS <> LS_WERKS-BKLAS OR LS_MBEW IS INITIAL.
        LW_VALUATIONDATA-VAL_CLASS = LS_WERKS-BKLAS.
        LW_VALUATIONDATAX-VAL_CLASS = 'X'.
      ENDIF.
      " 上期的评价分类
      IF LS_MBEW-VMBKL <> LS_WERKS-BKLAS OR LS_MBEW IS INITIAL.
        LW_VALUATIONDATA-VCLASS_PP = LS_WERKS-BKLAS.
        LW_VALUATIONDATAX-VCLASS_PP = 'X'.
      ENDIF.
      " 上年的评价分类
      IF LS_MBEW-VJBKL <> LS_WERKS-BKLAS OR LS_MBEW IS INITIAL.
        LW_VALUATIONDATA-VCLASS_PY = LS_WERKS-BKLAS.
        LW_VALUATIONDATAX-VCLASS_PY = 'X'.
      ENDIF.
      " 价格控制
      IF LS_MBEW-VPRSV <> LS_WERKS-VPRSV_1 OR LS_MBEW IS INITIAL.
        LW_VALUATIONDATA-PRICE_CTRL = LS_WERKS-VPRSV_1.
        LW_VALUATIONDATAX-PRICE_CTRL = 'X'.
      ENDIF.
      " 价格控制上一期间
      IF LS_MBEW-VMVPR <> LS_WERKS-VPRSV_1 OR LS_MBEW IS INITIAL.
        LW_VALUATIONDATA-PR_CTRL_PY = LS_WERKS-VPRSV_1.
        LW_VALUATIONDATAX-PR_CTRL_PY = 'X'.
      ENDIF.
      " 价格控制上一年度
      IF LS_MBEW-VJVPR <> LS_WERKS-VPRSV_1 OR LS_MBEW IS INITIAL.
        LW_VALUATIONDATA-PR_CTRL_PP = LS_WERKS-VPRSV_1.
        LW_VALUATIONDATAX-PR_CTRL_PP = 'X'.
      ENDIF.
      " 价格单位
      IF LS_MBEW-PEINH <> LS_WERKS-PEINH_1 OR LS_MBEW IS INITIAL.
        LW_VALUATIONDATA-PRICE_UNIT = LS_WERKS-PEINH_1.
        LW_VALUATIONDATAX-PRICE_UNIT = 'X'.
      ENDIF.
      " 计划价格1
      IF LS_MBEW-ZPLP1 <> LS_WERKS-ZPLP1 OR LS_MBEW IS INITIAL.
        LW_VALUATIONDATA-PLNDPRICE1 = LS_WERKS-ZPLP1.
        LW_VALUATIONDATAX-PLNDPRICE1 = 'X'.
      ENDIF.
      " 计划价格日期1
      IF LS_MBEW-ZPLD1 <> LS_WERKS-ZPLD1 OR LS_MBEW IS INITIAL.
        LW_VALUATIONDATA-PLNDPRDATE1 = LS_WERKS-ZPLD1.
        LW_VALUATIONDATAX-PLNDPRDATE1 = 'X'.
      ENDIF.
      " 计划价格2
      IF LS_MBEW-ZPLP2 <> LS_WERKS-ZPLP2 OR LS_MBEW IS INITIAL.
        LW_VALUATIONDATA-PLNDPRICE2 = LS_WERKS-ZPLP2.
        LW_VALUATIONDATAX-PLNDPRICE2 = 'X'.
      ENDIF.
      " 计划价格日期2
      IF LS_MBEW-ZPLD2 <> LS_WERKS-ZPLD2 OR LS_MBEW IS INITIAL.
        LW_VALUATIONDATA-PLNDPRDATE2 = LS_WERKS-ZPLD2.
        LW_VALUATIONDATAX-PLNDPRDATE2 = 'X'.
      ENDIF.
      " 计划价格3
      IF LS_MBEW-ZPLP3 <> LS_WERKS-ZPLP3 OR LS_MBEW IS INITIAL.
        LW_VALUATIONDATA-PLNDPRICE3 = LS_WERKS-ZPLP3.
        LW_VALUATIONDATAX-PLNDPRICE3 = 'X'.
      ENDIF.
      " 计划价格日期3
      IF LS_MBEW-ZPLD3 <> LS_WERKS-ZPLD3 OR LS_MBEW IS INITIAL.
        LW_VALUATIONDATA-PLNDPRDATE3 = LS_WERKS-ZPLD3.
        LW_VALUATIONDATAX-PLNDPRDATE3 = 'X'.
      ENDIF.
      " 用QS的成本估算
      IF LS_MBEW-EKALR <> LS_WERKS-EKALR OR LS_MBEW IS INITIAL.
        LW_VALUATIONDATA-QTY_STRUCT = LS_WERKS-EKALR.
        LW_VALUATIONDATAX-QTY_STRUCT = 'X'.
      ENDIF.
      " 物料来源
      IF LS_MBEW-HKMAT IS INITIAL OR LS_MBEW IS INITIAL.
        LW_VALUATIONDATA-ORIG_MAT = 'X'.
        LW_VALUATIONDATAX-ORIG_MAT = 'X'.
      ENDIF.
*      lw_valuationdata-val_class = ls_werks-bklas.                      " 评估分类
*      lw_valuationdata-vclass_pp = ls_werks-bklas.                      " 上期的评价分类
*      lw_valuationdata-vclass_py = ls_werks-bklas.                      " 上年的评价分类
*      lw_valuationdata-price_ctrl = ls_werks-vprsv_1.                   " 价格控制
*      lw_valuationdata-pr_ctrl_py = ls_werks-vprsv_1.                   " 价格控制上一期间
*      lw_valuationdata-pr_ctrl_pp = ls_werks-vprsv_1.                   " 价格控制上一年度
**    LW_VALUATIONDATA-STD_PRICE = LS_WERKS-STPRS.                      " 标准价格
**    LW_VALUATIONDATA-MOVING_PR = LS_WERKS-VERPR.                      " 周期单位价格
*      lw_valuationdata-price_unit = ls_werks-peinh_1.                   " 价格单位
*      lw_valuationdata-plndprice1 = ls_werks-zplp1.                     " 计划价格1
*      lw_valuationdata-plndprdate1 = ls_werks-zpld1.                    " 计划价格日期1
*      lw_valuationdata-plndprice2 = ls_werks-zplp2.                     " 计划价格2
*      lw_valuationdata-plndprdate2 = ls_werks-zpld2.                    " 计划价格日期2
*      lw_valuationdata-plndprice3 = ls_werks-zplp3.                     " 计划价格3
*      lw_valuationdata-plndprdate3 = ls_werks-zpld3.                    " 计划价格日期3
*      lw_valuationdata-qty_struct = ls_werks-ekalr.                     " 用QS的成本估算
**      lw_valuationdata-mat_origin = 'X'.                                " 物料来源
**      lw_valuationdatax-mat_origin = 'X'.
*
*      lw_valuationdata-orig_mat = 'X'.                                " 物料来源
*      lw_valuationdatax-orig_mat = 'X'.
*      lw_valuationdatax-qty_struct = 'X'.
*      IF lw_valuationdata-val_area IS NOT INITIAL.
*        lw_valuationdatax-val_area = ls_werks-werks.
*      ENDIF.
*      IF lw_valuationdata-val_class IS NOT INITIAL.
*        lw_valuationdatax-val_class = 'X'.
*      ENDIF.
*      IF lw_valuationdata-vclass_pp IS NOT INITIAL.
*        lw_valuationdatax-vclass_pp = 'X'.
*      ENDIF.
*      IF lw_valuationdata-vclass_py IS NOT INITIAL.
*        lw_valuationdatax-vclass_py = 'X'.
*      ENDIF.
*      IF lw_valuationdata-price_ctrl IS NOT INITIAL.
*        lw_valuationdatax-price_ctrl = 'X'.
*      ENDIF.
*      IF lw_valuationdata-pr_ctrl_py IS NOT INITIAL.
*        lw_valuationdatax-pr_ctrl_py = 'X'.
*      ENDIF.
*      IF lw_valuationdata-pr_ctrl_pp IS NOT INITIAL.
*        lw_valuationdatax-pr_ctrl_pp = 'X'.
*      ENDIF.
*      IF lw_valuationdata-ml_settle IS NOT INITIAL.
*        lw_valuationdatax-ml_settle = 'X'.
*      ENDIF.
**    LW_VALUATIONDATAX-STD_PRICE = 'X'.
**    LW_VALUATIONDATAX-MOVING_PR = 'X'.
*      lw_valuationdatax-price_unit = 'X'.
*      lw_valuationdatax-plndprice1 = 'X'.
*      lw_valuationdatax-plndprdate1 = 'X'.
*      lw_valuationdatax-plndprice2 = 'X'.
*      lw_valuationdatax-plndprdate2 = 'X'.
*      lw_valuationdatax-plndprice3 = 'X'.
*      lw_valuationdatax-plndprdate3 = 'X'.


*********************add by sujx 20191130*********************************
*      LW_VALUATIONDATA-ML_SETTLE = LS_WERKS-MLAST.                      " 价格确定

      IF LS_MBEW IS NOT INITIAL .
        LW_PLANTDATA-PROFIT_CTR = ''.     "利润中心
        LW_PLANTDATAX-PROFIT_CTR = ''.
        LW_VALUATIONDATA-PRICE_CTRL = ''.      "价格控制
        LW_VALUATIONDATAX-PRICE_CTRL = ''.
        LW_VALUATIONDATA-PR_CTRL_PY = ''.
        LW_VALUATIONDATAX-PR_CTRL_PY = ''.
        LW_VALUATIONDATA-PR_CTRL_PP = ''.
        LW_VALUATIONDATAX-PR_CTRL_PP = ''.
        LW_VALUATIONDATA-ML_SETTLE = ''.       "价格确定
        LW_VALUATIONDATAX-ML_SETTLE = ''.
        LW_VALUATIONDATA-VAL_CLASS = '' .      "评估类
        LW_VALUATIONDATAX-VAL_CLASS = '' .
        LW_VALUATIONDATA-VCLASS_PP = '' .
        LW_VALUATIONDATAX-VCLASS_PP = '' .
        LW_VALUATIONDATA-VCLASS_PY = '' .
        LW_VALUATIONDATAX-VCLASS_PY = '' .
      ELSE.
        IF LW_VALUATIONDATA-PRICE_CTRL = 'S'.
          LW_VALUATIONDATA-ML_SETTLE = '3'.
          LW_VALUATIONDATAX-PRICE_CTRL = 'X'.
          LW_VALUATIONDATAX-ML_SETTLE = 'X'.
        ENDIF.
        IF LW_VALUATIONDATA-PRICE_CTRL = 'V' .
          LW_VALUATIONDATA-ML_SETTLE = '2'.
          LW_VALUATIONDATAX-PRICE_CTRL = 'X'.
          LW_VALUATIONDATAX-ML_SETTLE = 'X'.
        ENDIF.
      ENDIF.

      IF LS_MARC IS NOT INITIAL.
        LW_PLANTDATA-BATCH_MGMT = ''.     " 批次管理
        LW_PLANTDATAX-BATCH_MGMT = ''.
      ENDIF.

      CLEAR:LS_MBEW,LS_MARC.

***********************end add***********************************************

      " 创建采购，MRP，质量，会计，成本视图
      CLEAR: LW_RETURN.
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          HEADDATA       = LW_HEADDATA
          CLIENTDATA     = LW_CLIENTDATA
          CLIENTDATAX    = LW_CLIENTDATAX
          PLANTDATA      = LW_PLANTDATA
          PLANTDATAX     = LW_PLANTDATAX
          VALUATIONDATA  = LW_VALUATIONDATA
          VALUATIONDATAX = LW_VALUATIONDATAX
        IMPORTING
          RETURN         = LW_RETURN.
      IF LW_RETURN-TYPE = 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        IF LV_MESS IS INITIAL.
          CONCATENATE LS_WERKS-WERKS '-' LW_RETURN-MESSAGE INTO LV_MESS.
        ELSE.
          CONCATENATE LV_MESS '|' LS_WERKS-WERKS '-' LW_RETURN-MESSAGE INTO LV_MESS.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
        " 视图创建成功时，创建质检视图
        IF US_INPUT-MTART <> 'Z040'
          AND US_INPUT-MTART <> 'Z099'.
          CLEAR: LT_QMAT, LS_QMAT, LT_INSPECTIONCTRL, LW_INSPECTIONCTRL.
          LT_QMAT = LS_WERKS-QMAT.
          LOOP AT LT_QMAT INTO LS_QMAT.
            CLEAR: LW_TQ34.
            READ TABLE LT_TQ34 INTO LW_TQ34 WITH KEY ART = LS_QMAT-ART.
            IF SY-SUBRC = 0.
              CLEAR: LV_MATNR.
              LV_MATNR = UV_MATNR.
              LW_INSPECTIONCTRL-INSPTYPE = LS_QMAT-ART.                     " 检验类型
              LW_INSPECTIONCTRL-MATERIAL = LV_MATNR.                        " 物料编号（18 个字符）
              LW_INSPECTIONCTRL-PLANT = LS_WERKS-WERKS.                     " 工厂
              LW_INSPECTIONCTRL-AVE_INSP_DURATION = LS_QMAT-MPDAU.          " 平均检验期
              LW_INSPECTIONCTRL-PREFERRED_INSPTYPE = LS_QMAT-QAPA.          " 首选检验类型
              LW_INSPECTIONCTRL-IND_INSPTYPE_MAT_ACTIVE = 'X'.              " 检验类型 － 物料合并已激活
              LW_INSPECTIONCTRL-IND_INSP_WITH_TSK_LIST = LW_TQ34-PPL.       " 有任务清单的检验
              LW_INSPECTIONCTRL-IND_SPEC_MATSPEC = LW_TQ34-SPEZUEBER.       " 使用物料说明的检验
              LW_INSPECTIONCTRL-IND_SPEC_CONFIG = LW_TQ34-CONF.             " 来自配置的检验规范
              LW_INSPECTIONCTRL-IND_SPEC_BATCH = LW_TQ34-TLS.               " 批次确定的检验规范
              LW_INSPECTIONCTRL-IND_AUTO_ASSIGN = LW_TQ34-APP.              " 自动规格分配
              LW_INSPECTIONCTRL-IND_INSP_BY_CHARAC = LW_TQ34-MER.           " 按特征检验
              LW_INSPECTIONCTRL-IND_POST_TO_INSP_STOCK = LW_TQ34-INSMK.     " 过帐到检验库存
              LW_INSPECTIONCTRL-IND_AUTOMATIC_UD = LW_TQ34-AVE.             " 计划的自动用途决定
              LW_INSPECTIONCTRL-SAMPLING_PROCEDURE = LW_TQ34-STICHPRVER.    " 采样程序
              LW_INSPECTIONCTRL-DYN_MODIF_RULE = LW_TQ34-DYNREGEL.          " 动态修改规则
              LW_INSPECTIONCTRL-INSP_PERCENTAGE = LW_TQ34-SPROZ.            " 检验百分比
              LW_INSPECTIONCTRL-IND_100_PERCENT_INSPECTION = LW_TQ34-HPZ.   " 100% 检验
              LW_INSPECTIONCTRL-IND_SKIPS_ALLOWED = LW_TQ34-DYN.            " 允许略过
              LW_INSPECTIONCTRL-IND_MANUAL_SAMPLE = LW_TQ34-MPB.            " 手工输入采样
              LW_INSPECTIONCTRL-IND_MANUAL_SAMPLE_CALC = LW_TQ34-MST.       " 手动触发采样计算
              LW_INSPECTIONCTRL-IND_SINGLE_UNITS_POSSIBLE = LW_TQ34-EIN.    " 可能的序列号管理
              LW_INSPECTIONCTRL-CONTR_INSP_LOT_CREATE = LW_TQ34-CHG.        " 控制检验批创建（批次摘要）
              LW_INSPECTIONCTRL-QUAL_SCORE_PROCEDURE = LW_TQ34-QKZVERF.     " 计算质量记分的程序
              LW_INSPECTIONCTRL-ALLOWED_SCRAP_SHARE = LW_TQ34-QPMAT.        " 检验批中允许的废品份额（百分比）
              LW_INSPECTIONCTRL-IND_HU_INSPECTION = LW_TQ34-AFR.            " 处理单位的检验
              LW_INSPECTIONCTRL-MS_FLAG = LW_TQ34-MS_FLAG.                  " 标识：多个规范
              APPEND LW_INSPECTIONCTRL TO LT_INSPECTIONCTRL.
              CLEAR: LW_INSPECTIONCTRL.
            ENDIF.
          ENDLOOP.
          CLEAR: LT_RETURN, LV_MESS1.
          CALL FUNCTION 'BAPI_MATINSPCTRL_SAVEREPLICA'
            TABLES
              RETURN         = LT_RETURN
              INSPECTIONCTRL = LT_INSPECTIONCTRL.
          LOOP AT LT_RETURN INTO LW_RETURN WHERE TYPE = 'E'.
            IF LV_MESS1 IS INITIAL.
              CONCATENATE LS_WERKS-WERKS '-' LW_RETURN-MESSAGE INTO LV_MESS1.
            ELSE.
              CONCATENATE LV_MESS '/' LW_RETURN-MESSAGE INTO LV_MESS1.
            ENDIF.
          ENDLOOP.
          IF SY-SUBRC = 0.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            IF LV_MESS IS INITIAL.
              LV_MESS = LV_MESS1.
            ELSE.
              CONCATENATE LV_MESS '|' LV_MESS1 INTO LV_MESS.
            ENDIF.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = 'X'.
          ENDIF.
        ENDIF.
        " 对于物料类型Z010、Z020、Z030、Z040、Z050的物料，在处理工厂8100时同时更新工厂8199，处理工厂8200时同时更新8299, 工厂8199和8299的利润中心都赋值为DUMMY
        IF US_INPUT-MTART = 'Z010'
          OR US_INPUT-MTART = 'Z020'
          OR US_INPUT-MTART = 'Z030'
          OR US_INPUT-MTART = 'Z040'
          OR US_INPUT-MTART = 'Z050'.
          CLEAR: LW_HEADDATA, LW_CLIENTDATA, LW_CLIENTDATA, LW_PLANTDATA, LW_PLANTDATAX.
          LW_HEADDATA-MATERIAL = UV_MATNR.                                    " 物料号
          LW_HEADDATA-MATL_TYPE = US_INPUT-MTART.                             " 物料类型
          LW_HEADDATA-IND_SECTOR = 'P'.                                       " 行业领域
          LW_HEADDATA-STORAGE_VIEW = 'X'.                                     " 库存视图
          LW_CLIENTDATA-TEMP_CONDS = LS_WERKS-TEMPB.                          " 温度条件
          LW_CLIENTDATA-STOR_CONDS = LS_WERKS-RAUBE.                          " 存储条件
          LW_CLIENTDATA-MINREMLIFE = LS_WERKS-MHDRZ.                          " 最小剩余货架寿命
          LW_CLIENTDATA-SHELF_LIFE = LS_WERKS-MHDHB.                          " 总货架寿命
          LW_CLIENTDATA-PERIOD_IND_EXPIRATION_DATE = LS_WERKS-IPRKZ.          " 期间标识
          LW_CLIENTDATA-ROUND_UP_RULE_EXPIRATION_DATE = LS_WERKS-RDMHD.       " 舍入规则
*********************虚拟工厂添加批次管理标识和外部采购仓储地点   HK  2020.1.20.
          LW_CLIENTDATA-BATCH_MGMT = LS_WERKS-XCHPF.                          " 批次管理标识
          LW_PLANTDATA-SLOC_EXPRC = LS_WERKS-LGFSB.                " 外部采购仓储地点
          LW_PLANTDATAX-SLOC_EXPRC = 'X'.
          LW_CLIENTDATAX-BATCH_MGMT = 'X'.
*********************END***************************************
          LW_CLIENTDATAX-TEMP_CONDS = 'X'.
          LW_CLIENTDATAX-STOR_CONDS = 'X'.
          LW_CLIENTDATAX-MINREMLIFE = 'X'.
          LW_CLIENTDATAX-SHELF_LIFE = 'X'.
          LW_CLIENTDATAX-PERIOD_IND_EXPIRATION_DATE = 'X'.
          LW_CLIENTDATAX-ROUND_UP_RULE_EXPIRATION_DATE = 'X'.
          IF US_INPUT-MTART = 'Z010'.
            LW_CLIENTDATA-MANUF_PROF = US_INPUT-MPROF.
            LW_CLIENTDATAX-MANUF_PROF = 'X'.
          ENDIF.
          LW_PLANTDATA-DEL_FLAG = LS_WERKS-LVOMA.                             " 删除标记
          LW_PLANTDATAX-DEL_FLAG = 'X'.

          IF LS_WERKS-WERKS = '8100'.
            LW_PLANTDATA-PLANT = '8199'.
            LW_PLANTDATAX-PLANT = '8199'.
            LW_PLANTDATA-PROFIT_CTR = 'DUMMY'.
            LW_PLANTDATAX-PROFIT_CTR = 'X'.
            LW_VALUATIONDATA-VAL_AREA = '8199'.
            LW_VALUATIONDATAX-VAL_AREA = '8199'.
          ELSEIF LS_WERKS-WERKS = '8200'.
            LW_PLANTDATA-PLANT = '8299'.
            LW_PLANTDATAX-PLANT = '8299'.
            LW_PLANTDATA-PROFIT_CTR = 'DUMMY'.
            LW_PLANTDATAX-PROFIT_CTR = 'X'.
            LW_VALUATIONDATA-VAL_AREA = '8299'.
            LW_VALUATIONDATAX-VAL_AREA = '8299'.
          ENDIF.

**********************add by sujx 20191130***********************************************
*          SELECT SINGLE * INTO ls_mbew
*                  FROM mbew
*                 WHERE matnr = uv_matnr
*                   AND bwkey =  lw_plantdata-plant.
*
*          SELECT SINGLE * INTO ls_marc
*            FROM marc
*           WHERE matnr = uv_matnr
*             AND werks =  lw_plantdata-plant.
*
**          IF ls_marc IS NOT INITIAL.
**            lw_plantdata-batch_mgmt = ''.     " 批次管理
**            lw_plantdatax-batch_mgmt = ''.
**          ENDIF.
*
*          IF ls_mbew IS NOT INITIAL.
*            lw_plantdata-profit_ctr = ''.     "利润中心
*            lw_plantdatax-profit_ctr = ''.
*          ENDIF.
*          CLEAR: ls_marc,ls_mbew.
********************** end add ************************************************

        ENDIF.
        CLEAR: LW_RETURN.
        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
          EXPORTING
            HEADDATA    = LW_HEADDATA
            CLIENTDATA  = LW_CLIENTDATA
            CLIENTDATAX = LW_CLIENTDATAX
            PLANTDATA   = LW_PLANTDATA
            PLANTDATAX  = LW_PLANTDATAX
          IMPORTING
            RETURN      = LW_RETURN.
        IF LW_RETURN-TYPE = 'E'.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          IF LV_MESS IS INITIAL.
            CONCATENATE LW_PLANTDATA-PLANT '-' LW_RETURN-MESSAGE INTO LV_MESS.
          ELSE.
            CONCATENATE LV_MESS '|' LW_PLANTDATA-PLANT '-' LW_RETURN-MESSAGE INTO LV_MESS.
          ENDIF.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.
        ENDIF.
      ENDIF.
    ELSE.
      LV_MESS = '工厂不能为空'.
    ENDIF.
    CLEAR LS_MBEW.
  ENDLOOP.
  IF LV_MESS IS NOT INITIAL.
    CS_RETURN-MSG_MATNR = UV_MATNR.
    CS_RETURN-MSG_TYPE = 'E'.
    CS_RETURN-MESSAGE = LV_MESS.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SALESORG_VIEW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> US_INPUT
*&---------------------------------------------------------------------*
FORM FRM_SALESORG_VIEW  USING    US_INPUT  TYPE ZDT_OA2SAP_MDM_MARA
                                 UV_MATNR  TYPE MATNR
                        CHANGING CS_RETURN TYPE ZDT_OA2SAP_MDM_RET_ZMD001.
  DEFINE MATN1_INPUT.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input         = &1
      IMPORTING
        output        = &1
      EXCEPTIONS
        length_error  = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  END-OF-DEFINITION.
  DEFINE CUNIT_INPUT.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input                = &1
        language             = sy-langu
      IMPORTING
        output               = &2
      EXCEPTIONS
        unit_not_found       = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  END-OF-DEFINITION.
  DATA: LT_VKORG TYPE ZDT_OA2SAP_MDM_MARA_VKORG_TAB,
        LS_VKORG TYPE ZDT_OA2SAP_MDM_MARA_VKORG,
        LV_PMATN TYPE MATNR,
        LV_VRKME TYPE VRKME,
        LS_MVKE  TYPE MVKE,
        LS_MARC  TYPE MARC,
        LS_MARA  TYPE MARA,
        LV_MESS  TYPE C LENGTH 1000.
  DATA: LW_HEADDATA           TYPE BAPIMATHEAD,
        LW_CLIENTDATA         TYPE BAPI_MARA,
        LW_CLIENTDATAX        TYPE BAPI_MARAX,
        LW_SALESDATA          TYPE BAPI_MVKE,
        LW_SALESDATAX         TYPE BAPI_MVKEX,
        LW_PLANTDATA          TYPE BAPI_MARC,
        LW_PLANTDATAX         TYPE BAPI_MARCX,
        LT_TAXCLASSIFICATIONS TYPE STANDARD TABLE OF BAPI_MLAN,
        LW_TAXCLASSIFICATIONS TYPE BAPI_MLAN,
        LT_RETURN             TYPE STANDARD TABLE OF BAPIRET2,
        LW_RETURN             TYPE BAPIRET2.

  CLEAR: LT_VKORG, LS_VKORG, LV_MESS.
  LT_VKORG = US_INPUT-VKORG.
  LW_HEADDATA-MATERIAL = UV_MATNR.                                    " 物料号
  LW_HEADDATA-MATL_TYPE = US_INPUT-MTART.                             " 物料类型
  LW_HEADDATA-IND_SECTOR = 'P'.                                       " 行业领域
  LW_HEADDATA-SALES_VIEW = 'X'.                                       " 销售视图
  LW_HEADDATA-PURCHASE_VIEW = 'X'.                                    " 采购视图

  LOOP AT LT_VKORG INTO LS_VKORG.
    IF LS_VKORG-VKORG IS INITIAL OR LS_VKORG-VTWEG IS INITIAL.
    ELSE.
      CLEAR: LV_PMATN, LV_VRKME, LS_MARC, LS_MVKE, LS_MARA.

      SELECT SINGLE * FROM MARA INTO LS_MARA
        WHERE MATNR = UV_MATNR.
      IF SY-SUBRC <> 0.
        CLEAR: LS_MARA.
      ENDIF.

      SELECT SINGLE * FROM MARC INTO LS_MARC
        WHERE MATNR = UV_MATNR AND WERKS = LS_VKORG-VKORG.
      IF SY-SUBRC <> 0.
        CLEAR: LS_MARC.
      ENDIF.

      SELECT SINGLE * FROM MVKE INTO LS_MVKE
        WHERE MATNR = UV_MATNR AND VKORG = LS_VKORG-VKORG AND VTWEG = LS_VKORG-VTWEG.
      IF SY-SUBRC <> 0.
        CLEAR: LS_MVKE.
      ENDIF.


      LV_PMATN = LS_VKORG-PMATN.
      MATN1_INPUT: LV_PMATN.
      CUNIT_INPUT: LS_VKORG-VRKME LV_VRKME.
      " 产品组
      IF LS_VKORG-SPART <> LS_MARA-SPART OR LS_MARA IS INITIAL.
        LW_CLIENTDATA-DIVISION = LS_VKORG-SPART.
        LW_CLIENTDATAX-DIVISION = 'X'.
      ENDIF.
      " 运输组
      IF LS_VKORG-TRAGR <> LS_MARA-TRAGR OR LS_MARA IS INITIAL.
        LW_CLIENTDATA-TRANS_GRP = LS_VKORG-TRAGR.
        LW_CLIENTDATAX-TRANS_GRP = 'X'.
      ENDIF.
*      lw_clientdata-division = ls_vkorg-spart.                          " 产品组
*      lw_clientdata-trans_grp = ls_vkorg-tragr.                         " 运输组
*      lw_clientdatax-division = 'X'.
*      lw_clientdatax-trans_grp = 'X'.

      LW_PLANTDATA-PLANT = LS_VKORG-VKORG.                              " 工厂
      LW_PLANTDATAX-PLANT = LS_VKORG-VKORG.
      " 可用性检查
      IF LS_VKORG-MTVFP <> LS_MARC-MTVFP OR LS_MARC IS INITIAL.
        LW_PLANTDATA-AVAILCHECK = LS_VKORG-MTVFP.
        LW_PLANTDATAX-AVAILCHECK = 'X'.
      ENDIF.
      " 装载组
      IF LS_VKORG-LADGR <> LS_MARC-LADGR OR LS_MARC IS INITIAL.
        LW_PLANTDATA-LOADINGGRP = LS_VKORG-LADGR.
        LW_PLANTDATAX-LOADINGGRP = 'X'.
      ENDIF.
*      lw_plantdata-availcheck = ls_vkorg-mtvfp.                         " 可用性检查
*      lw_plantdata-loadinggrp = ls_vkorg-ladgr.                         " 装载组
*      IF lw_plantdata-availcheck IS NOT INITIAL.
*        lw_plantdatax-availcheck = 'X'.
*      ENDIF.
*      IF lw_plantdata-loadinggrp IS NOT INITIAL.
*        lw_plantdatax-loadinggrp = 'X'.
*      ENDIF.

      LW_SALESDATA-SALES_ORG = LS_VKORG-VKORG.                          " 销售组织
      LW_SALESDATA-DISTR_CHAN = LS_VKORG-VTWEG.                         " 分销渠道
      LW_SALESDATAX-SALES_ORG = LS_VKORG-VKORG.
      LW_SALESDATAX-DISTR_CHAN = LS_VKORG-VTWEG.
      " 交货工厂
      IF LS_VKORG-DELYG_PLNT <> LS_MVKE-DWERK OR LS_MVKE IS INITIAL.
        LW_SALESDATA-DELYG_PLNT = LS_VKORG-DELYG_PLNT.
        LW_SALESDATAX-DELYG_PLNT = 'X'.
      ENDIF.
      " 科目设置组
      IF LS_VKORG-KTGRM <> LS_MVKE-KTGRM OR LS_MVKE IS INITIAL.
        LW_SALESDATA-ACCT_ASSGT = LS_VKORG-KTGRM.
        LW_SALESDATAX-ACCT_ASSGT = 'X'.
      ENDIF.
      " 项目类别组
      IF LS_VKORG-MTPOS <> LS_MVKE-MTPOS OR LS_MVKE IS INITIAL.
        LW_SALESDATA-ITEM_CAT = LS_VKORG-MTPOS.
        LW_SALESDATAX-ITEM_CAT = 'X'.
      ENDIF.
      " 物料组1
      IF LS_VKORG-MVGR1 <> LS_MVKE-MVGR1 OR LS_MVKE IS INITIAL.
        LW_SALESDATA-MATL_GRP_1 = LS_VKORG-MVGR1.
        LW_SALESDATAX-MATL_GRP_1 = 'X'.
      ENDIF.
      " 物料组2
      IF LS_VKORG-MVGR2 <> LS_MVKE-MVGR2 OR LS_MVKE IS INITIAL.
        LW_SALESDATA-MATL_GRP_2 = LS_VKORG-MVGR2.
        LW_SALESDATAX-MATL_GRP_2 = 'X'.
      ENDIF.
      " 定价参考物料
      IF LV_PMATN <> LS_MVKE-PMATN OR LS_MVKE IS INITIAL.
        LW_SALESDATA-PR_REF_MAT = LV_PMATN.
        LW_SALESDATAX-PR_REF_MAT = 'X'.
      ENDIF.
      " 销售单位
      IF LV_VRKME <> LS_MVKE-VRKME OR LS_MVKE IS INITIAL.
        LW_SALESDATA-SALES_UNIT = LV_VRKME.
        LW_SALESDATAX-SALES_UNIT = 'X'.
      ENDIF.
      " 指定分销链的物料状态
      IF LS_VKORG-VMSTA <> LS_MVKE-VMSTA OR LS_MVKE IS INITIAL.
        LW_SALESDATA-SAL_STATUS = LS_VKORG-VMSTA.
        LW_SALESDATAX-SAL_STATUS = 'X'.
        IF LS_VKORG-VMSTA IS NOT INITIAL.
          LW_SALESDATA-VALID_FROM = SY-DATUM.                             " 来自指定分销链的物料状态是有效的日期
        ELSE.
          LW_SALESDATA-VALID_FROM = '00000000'.
        ENDIF.
        LW_SALESDATAX-VALID_FROM = 'X'.
      ENDIF.
*      lw_salesdata-delyg_plnt = ls_vkorg-delyg_plnt.                    " 交货工厂
*      lw_salesdata-acct_assgt = ls_vkorg-ktgrm.                         " 科目设置组
*      lw_salesdata-item_cat = ls_vkorg-mtpos.                           " 项目类别组
*      lw_salesdata-matl_grp_1 = ls_vkorg-mvgr1.                         " 物料组1
*      lw_salesdata-matl_grp_2 = ls_vkorg-mvgr2.                         " 物料组2
*      lw_salesdata-pr_ref_mat = lv_pmatn.                               " 定价参考物料
*      lw_salesdata-sales_unit = lv_vrkme.                               " 销售单位
*      lw_salesdata-sal_status = ls_vkorg-vmsta.                         " 指定分销链的物料状态
*      IF ls_vkorg-vmsta IS NOT INITIAL.
*        lw_salesdata-valid_from = sy-datum.                             " 来自指定分销链的物料状态是有效的日期
*      ELSE.
*        lw_salesdata-valid_from = '00000000'.
*      ENDIF.
*      lw_salesdatax-delyg_plnt = 'X'.
*      lw_salesdatax-acct_assgt = 'X'.
*      lw_salesdatax-item_cat = 'X'.
*      lw_salesdatax-matl_grp_1 = 'X'.
*      lw_salesdatax-matl_grp_2 = 'X'.
*      lw_salesdatax-pr_ref_mat = 'X'.
*      lw_salesdatax-sales_unit = 'X'.
*      lw_salesdatax-sal_status = 'X'.
*      lw_salesdatax-valid_from = 'X'.

      CLEAR: LT_TAXCLASSIFICATIONS, LW_TAXCLASSIFICATIONS.
      LW_TAXCLASSIFICATIONS-DEPCOUNTRY = 'CN'.                          " 发货国(由该国家发货)
      LW_TAXCLASSIFICATIONS-TAX_TYPE_1 = 'MWST'.                        " 税类别(营业税，联邦营业税，...)
      LW_TAXCLASSIFICATIONS-TAXCLASS_1 = LS_VKORG-TAXM1.                " 物料的税分类
      APPEND LW_TAXCLASSIFICATIONS TO LT_TAXCLASSIFICATIONS.
      CLEAR: LW_RETURN, LW_TAXCLASSIFICATIONS.
      " CREATE SALESORG VIEW
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          HEADDATA           = LW_HEADDATA
          CLIENTDATA         = LW_CLIENTDATA
          CLIENTDATAX        = LW_CLIENTDATAX
          PLANTDATA          = LW_PLANTDATA
          PLANTDATAX         = LW_PLANTDATAX
          SALESDATA          = LW_SALESDATA
          SALESDATAX         = LW_SALESDATAX
        IMPORTING
          RETURN             = LW_RETURN
        TABLES
          TAXCLASSIFICATIONS = LT_TAXCLASSIFICATIONS.
      IF LW_RETURN-TYPE = 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        IF LV_MESS IS INITIAL.
          CONCATENATE LS_VKORG-VKORG '-' LW_RETURN-MESSAGE INTO LV_MESS.
        ELSE.
          CONCATENATE LV_MESS '|' LS_VKORG-VKORG '-' LW_RETURN-MESSAGE INTO LV_MESS.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF LV_MESS IS NOT INITIAL.
    CS_RETURN-MSG_MATNR = UV_MATNR.
    CS_RETURN-MSG_TYPE = 'E'.
    IF CS_RETURN-MESSAGE IS NOT INITIAL.
      CONCATENATE CS_RETURN-MESSAGE LV_MESS INTO CS_RETURN-MESSAGE.
    ELSE.
      CS_RETURN-MESSAGE = LV_MESS.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CLASS_VIEW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> US_INPUT
*&      --> LV_MATNR
*&---------------------------------------------------------------------*
FORM FRM_CLASS_VIEW  USING    US_INPUT  TYPE ZDT_OA2SAP_MDM_MARA
                              UV_MATNR  TYPE MATNR
                              UV_FLAG   TYPE C
                     CHANGING CS_RETURN TYPE ZDT_OA2SAP_MDM_RET_ZMD001.
  DATA: LT_CLASS    TYPE ZDT_OA2SAP_MDM_MARA_CLASS_TAB,
        LS_CLASS    TYPE ZDT_OA2SAP_MDM_MARA_CLASS,
        LT_WERKS    TYPE ZDT_OA2SAP_MDM_MARA_WERKS_TAB,
        LS_WERKS    TYPE ZDT_OA2SAP_MDM_MARA_WERKS,
        LV_CLASSNUM TYPE KLASSE_D,
        LV_MESS     TYPE C LENGTH 1000.
  DATA: LW_ALLOCLIST TYPE BAPI1003_ALLOC_LIST.
  CLEAR: LT_CLASS, LS_CLASS, LV_MESS, LV_CLASSNUM, LT_WERKS, LS_WERKS.
  LT_CLASS = US_INPUT-CLASS.
  LT_WERKS = US_INPUT-WERKS.
  LOOP AT LT_CLASS INTO LS_CLASS.
    IF LS_CLASS-CLASSTYPE = '001'.
      IF LS_CLASS-CLASSNAME = US_INPUT-MATKL.
        " CHECK IF CLASS 001 EXIST
        CLEAR: LW_ALLOCLIST.
        PERFORM FRM_GET_CLASS USING UV_MATNR '001' CHANGING LW_ALLOCLIST.
        IF LW_ALLOCLIST IS INITIAL.
          LV_CLASSNUM = LS_CLASS-CLASSNAME.
          PERFORM FRM_CLASS_CREATE USING UV_MATNR '001' LV_CLASSNUM LS_CLASS CHANGING CS_RETURN.
        ELSE.
          PERFORM FRM_CLASS_CHANGE USING UV_MATNR LW_ALLOCLIST LS_CLASS CHANGING CS_RETURN.
        ENDIF.
      ELSE.
        IF LV_MESS IS INITIAL.
          CONCATENATE LS_CLASS-CLASSNAME '物料分类名称与物料组字段值不一致' INTO LV_MESS.
        ELSE.
          CONCATENATE LV_MESS '|' LS_CLASS-CLASSNAME '物料分类名称与物料组字段值不一致' INTO LV_MESS.
        ENDIF.
      ENDIF.
    ELSE.
      " CHECK IF CLASS 022 EXIST
      CLEAR: LW_ALLOCLIST.
      PERFORM FRM_GET_CLASS USING UV_MATNR '022' CHANGING LW_ALLOCLIST.
      IF LW_ALLOCLIST IS INITIAL.
        " CHECK IF BATCH_MGMT IS TICKED
        LOOP AT LT_WERKS INTO LS_WERKS WHERE XCHPF = 'X'.
        ENDLOOP.
        IF SY-SUBRC = 0.
          IF US_INPUT-MTART = 'Z010'
            OR US_INPUT-MTART = 'Z020'
            OR US_INPUT-MTART = 'Z030'
            OR US_INPUT-MTART = 'Z040'
            OR US_INPUT-MTART = 'Z090'.
            LV_CLASSNUM = 'ZBATCH_SIRIO'.
          ELSEIF US_INPUT-MTART = 'Z050'
            OR US_INPUT-MTART = 'Z080'.
            LV_CLASSNUM = 'BATCH_CP'.
          ENDIF.
          PERFORM FRM_CLASS_CREATE USING UV_MATNR '022' LV_CLASSNUM LS_CLASS CHANGING CS_RETURN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_MRP_AERA_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> US_INPUT
*&      --> LV_MATNR
*&      <-- CS_RETURN
*&---------------------------------------------------------------------*
FORM FRM_MRP_AERA_CREATE  USING    US_INPUT  TYPE ZDT_OA2SAP_MDM_MARA
                                   UV_MATNR  TYPE MATNR
                          CHANGING CS_RETURN TYPE ZDT_OA2SAP_MDM_RET_ZMD001.
  DATA: LT_MRP    TYPE ZDT_OA2SAP_MDM_MARA_MRP_A_TAB1,
        LS_MRP    TYPE ZDT_OA2SAP_MDM_MARA_MRP_AREA,
        LT_DETAIL TYPE ZDT_OA2SAP_MDM_MARA_MRP_AR_TAB,
        LS_DETAIL TYPE ZDT_OA2SAP_MDM_MARA_MRP_AREA_M,
        LV_BERTY  TYPE BERTY,
        LV_SUBRC  TYPE C,
        LV_FLAG   TYPE C,
        LV_MESS   TYPE C LENGTH 1000.
  DATA: LV_MATNR  TYPE MATNR,
        LV_WERKS  TYPE WERKDP,
        LV_BERID  TYPE BERID,
        LW_MDMA   TYPE MDMA,
        LW_DPOP   TYPE DPOP,
        LW_FIELDS TYPE SDIBE_MASSFIELDS,
        LW_RETURN TYPE BAPIRETURN1.

  CLEAR: LT_MRP, LS_MRP, LV_MESS.
  LT_MRP = US_INPUT-MRP_AERA.

  LOOP AT LT_MRP INTO LS_MRP.
    CLEAR: LV_MATNR, LV_WERKS, LV_BERID, LT_DETAIL, LS_DETAIL.
    LT_DETAIL = LS_MRP-MRPAREA_DETAIL.
    LV_MATNR = UV_MATNR.
    LV_BERID = LS_MRP-BERID.
    LV_WERKS = LV_BERID+0(4).
    PERFORM FRM_CHECK_EXIST USING LV_MATNR LV_WERKS CHANGING LV_SUBRC.
    IF LV_SUBRC IS INITIAL.
      LOOP AT LT_DETAIL INTO LS_DETAIL.
        CLEAR: LW_MDMA, LW_FIELDS, LW_DPOP, LV_FLAG, LV_BERTY.
        LV_BERTY = LS_DETAIL-ZDISMM.
        LW_MDMA-MANDT = SY-MANDT.                     " 集团
        LW_MDMA-MATNR = LV_MATNR.                     " 物料编号
        LW_MDMA-BERID = LV_BERID.                     " MRP 范围
        LW_MDMA-WERKS = LV_WERKS.                     " MRP 范围：工厂
        LW_MDMA-DISMM = LS_DETAIL-ZDISMM.             " MRP类型
        LW_MDMA-DISGR = LS_DETAIL-ZDISGR.             " MRP组
        LW_MDMA-DISPO = LS_DETAIL-ZDISPO.             " MRP控制者/物料需求计划控制员
        LW_MDMA-DISLS = LS_DETAIL-ZDISLS.             " 批量大小
        LW_MDMA-SOBSL = LS_DETAIL-ZSOBSL.             " 特殊采购类
        LW_MDMA-BSTMA = LS_DETAIL-ZBSTMA.             " 最大批量大小
        LW_MDMA-BSTMI = LS_DETAIL-ZBSTMI.             " 最小批量大小
        LW_MDMA-BSTRF = LS_DETAIL-ZBSTRF.             " 舍入值
        LW_MDMA-BSTFE = LS_DETAIL-ZBSTFE.             " 固定批量
        LW_MDMA-LGFSB = LS_DETAIL-ZLGFSB.             " 外部采购仓储地点

        LW_FIELDS-XDISMM = LS_DETAIL-ZDISMM.          " MRP类型
        LW_FIELDS-XDISGR = LS_DETAIL-ZDISGR.          " MRP组
        LW_FIELDS-XDISPO = LS_DETAIL-ZDISPO.          " MRP控制者/物料需求计划控制员
        LW_FIELDS-XDISLS = LS_DETAIL-ZDISLS.          " 批量大小
        LW_FIELDS-XSOBSL = LS_DETAIL-ZSOBSL.          " 特殊采购类
        LW_FIELDS-XBSTMA = LS_DETAIL-ZBSTMA.          " 最大批量大小
        LW_FIELDS-XBSTMI = LS_DETAIL-ZBSTMI.          " 最小批量大小
        LW_FIELDS-XBSTRF = LS_DETAIL-ZBSTRF.          " 舍入值
        LW_FIELDS-XBSTFE = LS_DETAIL-ZBSTFE.          " 固定批量
        LW_FIELDS-XLGFSB = LS_DETAIL-ZLGFSB.          " 外部采购仓储地点
        PERFORM FRM_GET_MRPAREA USING LV_MATNR LV_WERKS LV_BERID CHANGING LV_FLAG.
        IF LV_FLAG IS NOT INITIAL.
          CALL FUNCTION 'MD_MRP_LEVEL_CHANGE_DATA'
            EXPORTING
              I_MATNR        = LV_MATNR
              I_WERK         = LV_WERKS
              I_MRP_AREA     = LV_BERID
              I_BERTY        = LV_BERTY
              I_SELFIELDS    = LW_FIELDS
              I_MDMA         = LW_MDMA
              I_DPOP         = LW_DPOP
            IMPORTING
              E_ERROR_RETURN = LW_RETURN.
        ELSE.
          CLEAR: LW_RETURN.
          CALL FUNCTION 'MD_MRP_LEVEL_CREATE_DATA'
            EXPORTING
              I_MATNR        = LV_MATNR
              I_WERK         = LV_WERKS
              I_MRP_AREA     = LV_BERID
              I_SELFIELDS    = LW_FIELDS
              I_MDMA         = LW_MDMA
              I_DPOP         = LW_DPOP
            IMPORTING
              E_ERROR_RETURN = LW_RETURN.
        ENDIF.
        IF LW_RETURN-TYPE = 'E'.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          IF LV_MESS IS INITIAL.
            CONCATENATE LV_BERID '-' LW_RETURN-MESSAGE INTO LV_MESS.
          ELSE.
            CONCATENATE LV_MESS '|' LV_BERID '-' LW_RETURN-MESSAGE INTO LV_MESS.
          ENDIF.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
  IF LV_MESS IS NOT INITIAL.
    CS_RETURN-MSG_MATNR = UV_MATNR.
    CS_RETURN-MSG_TYPE = 'E'.
    IF CS_RETURN-MESSAGE IS NOT INITIAL.
      CONCATENATE CS_RETURN-MESSAGE LV_MESS INTO CS_RETURN-MESSAGE.
    ELSE.
      CS_RETURN-MESSAGE = LV_MESS.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_CLASS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> UV_MATNR
*&      --> P_
*&      <-- LW_ALLOCLIST
*&---------------------------------------------------------------------*
FORM FRM_GET_CLASS  USING    UV_MATNR     TYPE MATNR
                             UV_CLASSTYPE TYPE KLASSENART
                    CHANGING CS_ALLOCLIST TYPE BAPI1003_ALLOC_LIST.
  DATA: LT_ALLOCLIST TYPE STANDARD TABLE OF BAPI1003_ALLOC_LIST,
        LT_RETURN    TYPE STANDARD TABLE OF BAPIRET2,
        LV_OBJECTKEY TYPE OBJNUM.

  CLEAR: LV_OBJECTKEY, LT_ALLOCLIST, LT_RETURN.
  LV_OBJECTKEY = UV_MATNR.

  CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
    EXPORTING
      OBJECTKEY_IMP   = LV_OBJECTKEY
      OBJECTTABLE_IMP = 'MARA'
      CLASSTYPE_IMP   = UV_CLASSTYPE
    TABLES
      ALLOCLIST       = LT_ALLOCLIST
      RETURN          = LT_RETURN.
  LOOP AT LT_RETURN INTO DATA(LS_RETURN) WHERE TYPE = 'E'.
  ENDLOOP.
  IF SY-SUBRC <> 0.
    READ TABLE LT_ALLOCLIST INTO DATA(LS_ALLOCLIST) INDEX 1.
    IF SY-SUBRC = 0.
      CS_ALLOCLIST = LS_ALLOCLIST.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CLASS_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> UV_MATNR
*&      --> P_
*&      --> LW_ALLOCLIST
*&      --> LS_CLASS
*&      <-- CS_RETURN
*&---------------------------------------------------------------------*
FORM FRM_CLASS_CREATE  USING    UV_MATNR     TYPE MATNR
                                UV_CLASSTYPE TYPE KLASSENART
                                UV_CLASSNUM  TYPE KLASSE_D
                                US_CLASS     TYPE ZDT_OA2SAP_MDM_MARA_CLASS
                       CHANGING CS_RETURN    TYPE ZDT_OA2SAP_MDM_RET_ZMD001.
  DATA: LV_OBJECTKEYNEW    TYPE OBJNUM,
        LT_ALLOCVALUESNUM  TYPE STANDARD TABLE OF BAPI1003_ALLOC_VALUES_NUM,
        LS_ALLOCVALUESNUM  TYPE BAPI1003_ALLOC_VALUES_NUM,
        LT_ALLOCVALUESCHAR TYPE STANDARD TABLE OF BAPI1003_ALLOC_VALUES_CHAR,
        LS_ALLOCVALUESCHAR TYPE BAPI1003_ALLOC_VALUES_CHAR,
        LT_ALLOCVALUESCURR TYPE STANDARD TABLE OF BAPI1003_ALLOC_VALUES_CURR,
        LS_ALLOCVALUESCURR TYPE BAPI1003_ALLOC_VALUES_CURR,
        LT_RETURN          TYPE STANDARD TABLE OF BAPIRET2,
        LS_RETURN          TYPE BAPIRET2.
  DATA: LT_CLASSIFY TYPE ZDT_OA2SAP_MDM_MARA_CLASS__TAB,
        LS_CLASSIFY LIKE LINE OF LT_CLASSIFY,
        LV_MESS     TYPE C LENGTH 1000.

  CLEAR: LT_CLASSIFY, LS_CLASSIFY, LV_OBJECTKEYNEW, LT_RETURN, LS_RETURN, LV_MESS.
  LT_CLASSIFY = US_CLASS-CLASSIFY.
  LV_OBJECTKEYNEW = UV_MATNR.
  IF UV_CLASSTYPE = '001'.
    CLEAR: LT_ALLOCVALUESNUM, LS_ALLOCVALUESNUM, LT_ALLOCVALUESCHAR, LS_ALLOCVALUESCHAR, LT_ALLOCVALUESCURR, LS_ALLOCVALUESCURR.
    LOOP AT LT_CLASSIFY INTO LS_CLASSIFY.
      LS_ALLOCVALUESCHAR-CHARACT = LS_CLASSIFY-ATTRNAME.
      LS_ALLOCVALUESCHAR-CHARACT_DESCR = LS_CLASSIFY-ATTRNAME.
*      LS_ALLOCVALUESCHAR-VALUE_CHAR = LS_CLASSIFY-ATTRVALUE.
*      LS_ALLOCVALUESCHAR-VALUE_NEUTRAL = LS_CLASSIFY-ATTRVALUE.
      LS_ALLOCVALUESCHAR-VALUE_CHAR_LONG = LS_CLASSIFY-ATTRVALUE.
      LS_ALLOCVALUESCHAR-VALUE_NEUTRAL_LONG = LS_CLASSIFY-ATTRVALUE.
      APPEND LS_ALLOCVALUESCHAR TO LT_ALLOCVALUESCHAR.
      CLEAR: LS_ALLOCVALUESCHAR.
    ENDLOOP.
    CALL FUNCTION 'BAPI_OBJCL_CREATE'
      EXPORTING
        OBJECTKEYNEW    = LV_OBJECTKEYNEW
        OBJECTTABLENEW  = 'MARA'
        CLASSNUMNEW     = UV_CLASSNUM
        CLASSTYPENEW    = '001'
      TABLES
*       ALLOCVALUESNUM  =
        ALLOCVALUESCHAR = LT_ALLOCVALUESCHAR
*       ALLOCVALUESCURR =
        RETURN          = LT_RETURN.
    LOOP AT LT_RETURN INTO LS_RETURN WHERE TYPE = 'E'.
      IF LV_MESS IS INITIAL.
        CONCATENATE US_CLASS-CLASSTYPE US_CLASS-CLASSNAME '-' LS_RETURN-MESSAGE INTO LV_MESS.
      ELSE.
        CONCATENATE LV_MESS '|' US_CLASS-CLASSTYPE US_CLASS-CLASSNAME '-' LS_RETURN-MESSAGE INTO LV_MESS.
      ENDIF.
    ENDLOOP.
  ELSE.
    CALL FUNCTION 'BAPI_OBJCL_CREATE'
      EXPORTING
        OBJECTKEYNEW   = LV_OBJECTKEYNEW
        OBJECTTABLENEW = 'MARA'
        CLASSNUMNEW    = UV_CLASSNUM
        CLASSTYPENEW   = '022'
        STANDARDCLASS  = 'X'
      TABLES
        RETURN         = LT_RETURN.
    LOOP AT LT_RETURN INTO LS_RETURN WHERE TYPE = 'E'.
      IF LV_MESS IS INITIAL.
        CONCATENATE US_CLASS-CLASSTYPE US_CLASS-CLASSNAME '-' LS_RETURN-MESSAGE INTO LV_MESS.
      ELSE.
        CONCATENATE LV_MESS '|' US_CLASS-CLASSTYPE US_CLASS-CLASSNAME '-' LS_RETURN-MESSAGE INTO LV_MESS.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF LV_MESS IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    CS_RETURN-MSG_MATNR = UV_MATNR.
    CS_RETURN-MSG_TYPE = 'E'.
    IF CS_RETURN-MESSAGE IS NOT INITIAL.
      CONCATENATE CS_RETURN-MESSAGE LV_MESS INTO CS_RETURN-MESSAGE.
    ELSE.
      CS_RETURN-MESSAGE = LV_MESS.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CLASS_CHANGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> UV_MATNR
*&      --> LW_ALLOCLIST
*&      --> LS_CLASS
*&      <-- CS_RETURN
*&---------------------------------------------------------------------*
FORM FRM_CLASS_CHANGE  USING    UV_MATNR     TYPE MATNR
                                US_ALLOCLIST TYPE BAPI1003_ALLOC_LIST
                                US_CLASS     TYPE ZDT_OA2SAP_MDM_MARA_CLASS
                       CHANGING CS_RETURN    TYPE ZDT_OA2SAP_MDM_RET_ZMD001.
  DATA: LV_OBJECTKEYNEW    TYPE OBJNUM,
        LV_CLASSNUMNEW     TYPE KLASSE_D,
        LT_ALLOCVALUESNUM  TYPE STANDARD TABLE OF BAPI1003_ALLOC_VALUES_NUM,
        LS_ALLOCVALUESNUM  TYPE BAPI1003_ALLOC_VALUES_NUM,
        LT_ALLOCVALUESCHAR TYPE STANDARD TABLE OF BAPI1003_ALLOC_VALUES_CHAR,
        LS_ALLOCVALUESCHAR TYPE BAPI1003_ALLOC_VALUES_CHAR,
        LT_ALLOCVALUESCURR TYPE STANDARD TABLE OF BAPI1003_ALLOC_VALUES_CURR,
        LS_ALLOCVALUESCURR TYPE BAPI1003_ALLOC_VALUES_CURR,
        LS_VALUESCHAR      TYPE BAPI1003_ALLOC_VALUES_CHAR,
        LT_RETURN          TYPE STANDARD TABLE OF BAPIRET2,
        LS_RETURN          TYPE BAPIRET2.
  DATA: LT_CLASSIFY TYPE ZDT_OA2SAP_MDM_MARA_CLASS__TAB,
        LS_CLASSIFY LIKE LINE OF LT_CLASSIFY,
        LV_MESS     TYPE C LENGTH 1000.

  CLEAR: LT_CLASSIFY, LS_CLASSIFY, LT_RETURN, LS_RETURN, LV_OBJECTKEYNEW, LV_CLASSNUMNEW, LV_MESS.
  LT_CLASSIFY = US_CLASS-CLASSIFY.
  LV_OBJECTKEYNEW = UV_MATNR.
  LV_CLASSNUMNEW = US_ALLOCLIST-CLASSNUM.
  CLEAR: LT_ALLOCVALUESNUM, LS_ALLOCVALUESNUM, LT_ALLOCVALUESCHAR, LS_ALLOCVALUESCHAR, LT_ALLOCVALUESCURR, LS_ALLOCVALUESCURR.
  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      OBJECTKEY       = LV_OBJECTKEYNEW
      OBJECTTABLE     = 'MARA'
      CLASSNUM        = LV_CLASSNUMNEW
      CLASSTYPE       = '001'
    TABLES
      ALLOCVALUESNUM  = LT_ALLOCVALUESNUM
      ALLOCVALUESCHAR = LT_ALLOCVALUESCHAR
      ALLOCVALUESCURR = LT_ALLOCVALUESCURR
      RETURN          = LT_RETURN.
  LOOP AT LT_RETURN INTO LS_RETURN WHERE TYPE = 'E'.
    IF LV_MESS IS INITIAL.
      CONCATENATE US_CLASS-CLASSTYPE US_CLASS-CLASSNAME '-' LS_RETURN-MESSAGE INTO LV_MESS.
    ELSE.
      CONCATENATE LV_MESS '|' US_CLASS-CLASSTYPE US_CLASS-CLASSNAME '-' LS_RETURN-MESSAGE INTO LV_MESS.
    ENDIF.
  ENDLOOP.
  IF SY-SUBRC <> 0.
    LOOP AT LT_ALLOCVALUESCHAR ASSIGNING FIELD-SYMBOL(<FS_CHAR>).
      <FS_CHAR>-VALUE_CHAR = ''.
      <FS_CHAR>-VALUE_NEUTRAL = ''.
      <FS_CHAR>-VALUE_CHAR_LONG = ''.
      <FS_CHAR>-VALUE_NEUTRAL_LONG = ''.
    ENDLOOP.
*    CLEAR: LS_VALUESCHAR.
    LOOP AT LT_CLASSIFY INTO LS_CLASSIFY.
      READ TABLE LT_ALLOCVALUESCHAR ASSIGNING <FS_CHAR>
        WITH KEY CHARACT = LS_CLASSIFY-ATTRNAME.
      IF SY-SUBRC = 0.
        <FS_CHAR>-VALUE_CHAR = ''.
        <FS_CHAR>-VALUE_NEUTRAL = ''.
        <FS_CHAR>-VALUE_CHAR_LONG = LS_CLASSIFY-ATTRVALUE.
        <FS_CHAR>-VALUE_NEUTRAL_LONG = LS_CLASSIFY-ATTRVALUE.
      ELSE.
        LS_ALLOCVALUESCHAR-CHARACT = LS_CLASSIFY-ATTRNAME.
        LS_ALLOCVALUESCHAR-CHARACT_DESCR = LS_CLASSIFY-ATTRNAME.
*        LS_ALLOCVALUESCHAR-VALUE_CHAR = LS_CLASSIFY-ATTRVALUE.
*        LS_ALLOCVALUESCHAR-VALUE_NEUTRAL = LS_CLASSIFY-ATTRVALUE.
        LS_ALLOCVALUESCHAR-VALUE_CHAR_LONG = LS_CLASSIFY-ATTRVALUE.
        LS_ALLOCVALUESCHAR-VALUE_NEUTRAL_LONG = LS_CLASSIFY-ATTRVALUE.
        APPEND LS_ALLOCVALUESCHAR TO LT_ALLOCVALUESCHAR.
        CLEAR: LS_ALLOCVALUESCHAR.
      ENDIF.
*      LS_VALUESCHAR-CHARACT = LS_CLASSIFY-ATTRNAME.
*      LS_VALUESCHAR-CHARACT_DESCR = LS_CLASSIFY-ATTRNAME.
*      LS_VALUESCHAR-VALUE_CHAR = LS_CLASSIFY-ATTRVALUE.
*      LS_VALUESCHAR-VALUE_NEUTRAL = LS_CLASSIFY-ATTRVALUE.
*      LS_VALUESCHAR-VALUE_CHAR_LONG = LS_CLASSIFY-ATTRVALUE.
*      LS_VALUESCHAR-VALUE_NEUTRAL_LONG = LS_CLASSIFY-ATTRVALUE.
*      MODIFY LT_ALLOCVALUESCHAR FROM LS_VALUESCHAR.
*      CLEAR: LS_VALUESCHAR.
    ENDLOOP.
    IF LT_ALLOCVALUESCHAR IS NOT INITIAL.
      CLEAR: LT_RETURN, LS_RETURN.
      CALL FUNCTION 'BAPI_OBJCL_CHANGE'
        EXPORTING
          OBJECTKEY          = LV_OBJECTKEYNEW
          OBJECTTABLE        = 'MARA'
          CLASSNUM           = LV_CLASSNUMNEW
          CLASSTYPE          = '001'
        TABLES
          ALLOCVALUESNUMNEW  = LT_ALLOCVALUESNUM
          ALLOCVALUESCHARNEW = LT_ALLOCVALUESCHAR
          ALLOCVALUESCURRNEW = LT_ALLOCVALUESCURR
          RETURN             = LT_RETURN.
      LOOP AT LT_RETURN INTO LS_RETURN WHERE TYPE = 'E'.
        IF LV_MESS IS INITIAL.
          CONCATENATE US_CLASS-CLASSTYPE US_CLASS-CLASSNAME '-' LS_RETURN-MESSAGE INTO LV_MESS.
        ELSE.
          CONCATENATE LV_MESS '|' US_CLASS-CLASSTYPE US_CLASS-CLASSNAME '-' LS_RETURN-MESSAGE INTO LV_MESS.
        ENDIF.
      ENDLOOP.
      IF LV_MESS IS NOT INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.
  IF LV_MESS IS NOT INITIAL.
    CS_RETURN-MSG_MATNR = UV_MATNR.
    CS_RETURN-MSG_TYPE = 'E'.
    IF CS_RETURN-MESSAGE IS NOT INITIAL.
      CONCATENATE CS_RETURN-MESSAGE LV_MESS INTO CS_RETURN-MESSAGE.
    ELSE.
      CS_RETURN-MESSAGE = LV_MESS.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHANGE_MATNR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LW_MRAR
*&      <-- LW_RETURN
*&---------------------------------------------------------------------*
FORM FRM_CHANGE_MATNR  USING    US_INPUT  TYPE ZDT_OA2SAP_MDM_MARA
                       CHANGING CS_RETURN TYPE ZDT_OA2SAP_MDM_RET_ZMD001.
  DEFINE MATN1_INPUT.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input         = &1
      IMPORTING
        output        = &1
      EXCEPTIONS
        length_error  = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  END-OF-DEFINITION.
  DEFINE ALPHA_OUTPUT.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input         = &1
      IMPORTING
        output        = &1.
  END-OF-DEFINITION.
  DEFINE ALPHA_INPUT.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input         = &1
      IMPORTING
        output        = &1.
  END-OF-DEFINITION.
  DEFINE CUNIT_INPUT.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input                = &1
        language             = sy-langu
      IMPORTING
        output               = &2
      EXCEPTIONS
        unit_not_found       = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  END-OF-DEFINITION.
  DATA: LW_HEADDATA            TYPE BAPIMATHEAD,
        LW_CLIENTDATA          TYPE BAPI_MARA,
        LW_CLIENTDATAX         TYPE BAPI_MARAX,
        LW_RETURN              TYPE BAPIRET2,
        LT_MATERIALLONGTEXT    TYPE STANDARD TABLE OF BAPI_MLTX,
        LW_MATERIALLONGTEXT    TYPE BAPI_MLTX,
        LT_UNITSOFMEASURE      TYPE STANDARD TABLE OF BAPI_MARM,
        LW_UNITSOFMEASURE      TYPE BAPI_MARM,
        LT_UNITSOFMEASUREX     TYPE STANDARD TABLE OF BAPI_MARMX,
        LW_UNITSOFMEASUREX     TYPE BAPI_MARMX,
        LT_MATERIALDESCRIPTION TYPE STANDARD TABLE OF BAPI_MAKT,
        LW_MATERIALDESCRIPTION TYPE BAPI_MAKT.

  DATA: LT_SMEINH    TYPE ZDT_OA2SAP_MDM_MARA_SMEINH_TAB,
        LS_SMEINH    TYPE ZDT_OA2SAP_MDM_MARA_SMEINH,
        LV_MATNR     TYPE MATNR,
        LV_MATNR_T   TYPE MATNR,
        LV_MATNR_MPN TYPE BMATN,
        LV_MFRNR     TYPE MFRNR,
        LV_OLD_NO    TYPE BISMT,
        LV_BISMT     TYPE C LENGTH 500,
        LV_MAKTXL    TYPE C LENGTH 500,
        LV_LENGTH    TYPE I,
        LV_MEINS     TYPE MEINS,
        LV_MEINH     TYPE MEINS,
        LV_FLAG      TYPE C.

  DATA : LV_MPROF TYPE MARA-MPROF .

  CLEAR: LV_MATNR, LV_FLAG, LV_MAKTXL, LV_LENGTH, LV_MFRNR, LV_MEINS, LV_BISMT, LV_OLD_NO.
  LV_MATNR = US_INPUT-MATNR.
  MATN1_INPUT: LV_MATNR.
  LV_MFRNR = US_INPUT-MFRNR.
  ALPHA_INPUT: LV_MFRNR.
  LV_MAKTXL = US_INPUT-MAKTX.
  LV_LENGTH = STRLEN( LV_MAKTXL ).
  CUNIT_INPUT: US_INPUT-MEINS LV_MEINS.
  LV_BISMT = US_INPUT-BISMT.
  CONDENSE LV_BISMT NO-GAPS.
  IF LV_LENGTH > 40.
    CS_RETURN-MSG_MATNR = LV_MATNR.
    CS_RETURN-MSG_TYPE = 'E'.
    CONCATENATE LV_MAKTXL '物料描述过长' INTO CS_RETURN-MESSAGE.
  ELSE.

    SELECT MATNR, MEINH, UMREZ, UMREN
      FROM MARM
      INTO TABLE @DATA(LT_MARM)
      WHERE MATNR = @LV_MATNR.

    IF US_INPUT-MTART <> 'HERS'.
      CLEAR: LV_LENGTH.
      LV_LENGTH = STRLEN( LV_BISMT ).
      IF LV_LENGTH > 40.
        LV_OLD_NO = LV_BISMT+0(39) && '*'.
*        CLEAR: lt_materiallongtext, lw_materiallongtext.
*        lw_materiallongtext-applobject = 'MATERIAL'.
*        lw_materiallongtext-text_name = lv_matnr.
*        lw_materiallongtext-text_id = 'IVER'.
*        lw_materiallongtext-langu = '1'.
*        lw_materiallongtext-format_col = 1.
*        lw_materiallongtext-text_line = lv_bismt+0(132).
*        APPEND lw_materiallongtext TO lt_materiallongtext.
      ELSE.
        LV_OLD_NO = LV_BISMT.
      ENDIF.
      CLEAR: LT_MATERIALLONGTEXT, LW_MATERIALLONGTEXT.
      LW_MATERIALLONGTEXT-APPLOBJECT = 'MATERIAL'.
      LW_MATERIALLONGTEXT-TEXT_NAME = LV_MATNR.
      LW_MATERIALLONGTEXT-TEXT_ID = 'IVER'.
      LW_MATERIALLONGTEXT-LANGU = '1'.
      LW_MATERIALLONGTEXT-FORMAT_COL = 1.
      LW_MATERIALLONGTEXT-TEXT_LINE = LV_BISMT+0(132).
      APPEND LW_MATERIALLONGTEXT TO LT_MATERIALLONGTEXT.
      " 创建物料基本视图
      SELECT SINGLE * INTO @DATA(LS_MARA)
        FROM MARA
        WHERE MATNR = @LV_MATNR.
      CLEAR: LW_HEADDATA, LW_CLIENTDATA, LW_CLIENTDATAX.
      LW_HEADDATA-MATERIAL = LV_MATNR.                        " 物料号
      LW_HEADDATA-MATL_TYPE = US_INPUT-MTART.                 " 物料类型
      LW_HEADDATA-IND_SECTOR = 'P'.                           " 行业领域
      LW_HEADDATA-BASIC_VIEW = 'X'.                           " 基本数据视图
      LW_HEADDATA-PURCHASE_VIEW = 'X'.                        " 采购视图
      IF US_INPUT-MATKL <> LS_MARA-MATKL OR LS_MARA IS INITIAL.
        LW_CLIENTDATA-MATL_GROUP = US_INPUT-MATKL.              " 物料组
        LW_CLIENTDATAX-MATL_GROUP = 'X'.
      ENDIF.
      IF LV_OLD_NO <> LS_MARA-BISMT OR LS_MARA IS INITIAL.
        LW_CLIENTDATA-OLD_MAT_NO_LONG = LV_OLD_NO.              " 旧物料号
        LW_CLIENTDATAX-OLD_MAT_NO_LONG = 'X'.
      ENDIF.
      IF LS_MARA IS INITIAL.
        LW_CLIENTDATA-BASE_UOM = LV_MEINS.                      " 基本计量单位
        LW_CLIENTDATAX-BASE_UOM = 'X'.
      ELSE.
        LW_CLIENTDATA-BASE_UOM = ''.
        LW_CLIENTDATAX-BASE_UOM = ''.
      ENDIF.
      IF US_INPUT-GROES <> LS_MARA-GROES OR LS_MARA IS INITIAL.
        LW_CLIENTDATA-SIZE_DIM = US_INPUT-GROES.                " 规格型号（大小/量纲）
        LW_CLIENTDATAX-SIZE_DIM = 'X'.
      ENDIF.
      IF LV_MFRNR <> LS_MARA-MFRNR OR LS_MARA IS INITIAL.
        LW_CLIENTDATA-MFR_NO = LV_MFRNR.                        " 制造商
        LW_CLIENTDATAX-MFR_NO = 'X'.
      ENDIF.

      IF US_INPUT-EKWSL <> LS_MARA-EKWSL OR LS_MARA IS INITIAL.
        LW_CLIENTDATA-PUR_VALKEY = US_INPUT-EKWSL.              " 采购价值代码
        LW_CLIENTDATAX-PUR_VALKEY = 'X'.
      ENDIF.

*      SELECT SINGLE * INTO @DATA(ls_mara)
*         FROM mara
*         WHERE matnr = @lv_matnr.
*     IF sy-subrc = 0.
*      lw_clientdatax-old_mat_no_long = ''.
*      lw_clientdata-base_uom = ''.                      " 基本计量单位
*     ENDIF.

      IF US_INPUT-MPROF <> LS_MARA-MPROF OR LS_MARA IS INITIAL.
        LW_CLIENTDATA-MANUF_PROF = US_INPUT-MPROF.            " 制造商参数文件
        LW_CLIENTDATAX-MANUF_PROF = 'X'.
      ENDIF.

*      IF lv_mfrnr IS INITIAL AND us_input-mtart = 'Z010'.
*        lw_clientdata-manuf_prof = 'IMPN'.                    " 制造商参数文件
*        lw_clientdatax-manuf_prof = 'X'.
*      ENDIF.

      IF US_INPUT-LVOMA IS NOT INITIAL.
        LW_CLIENTDATA-DEL_FLAG = 'X'.                         " 在客户级标记要删除的物料
        LW_CLIENTDATAX-DEL_FLAG = 'X'.
      ELSE.
        LW_CLIENTDATA-DEL_FLAG = ''.                          " 在客户级标记要删除的物料
        LW_CLIENTDATAX-DEL_FLAG = 'X'.
      ENDIF.
      " 物料描述
      CLEAR: LT_MATERIALDESCRIPTION, LW_MATERIALDESCRIPTION.
      LW_MATERIALDESCRIPTION-LANGU = '1'.
      LW_MATERIALDESCRIPTION-MATL_DESC = US_INPUT-MAKTX.      " 物料描述
      APPEND LW_MATERIALDESCRIPTION TO LT_MATERIALDESCRIPTION.
      " 物料描述长文本
      IF US_INPUT-MAKTXL IS NOT INITIAL.
        CLEAR: LW_MATERIALLONGTEXT, LV_MAKTXL.
        LV_MAKTXL = US_INPUT-MAKTXL.
        LW_MATERIALLONGTEXT-APPLOBJECT = 'MATERIAL'.
        LW_MATERIALLONGTEXT-TEXT_NAME = LV_MATNR.
        LW_MATERIALLONGTEXT-TEXT_ID = 'GRUN'.
        LW_MATERIALLONGTEXT-LANGU = '1'.
        LW_MATERIALLONGTEXT-FORMAT_COL = 1.
        LW_MATERIALLONGTEXT-TEXT_LINE = LV_MAKTXL+0(132).
        APPEND LW_MATERIALLONGTEXT TO LT_MATERIALLONGTEXT.
        CLEAR: LW_MATERIALLONGTEXT.
        LW_MATERIALLONGTEXT-APPLOBJECT = 'MATERIAL'.
        LW_MATERIALLONGTEXT-TEXT_NAME = LV_MATNR.
        LW_MATERIALLONGTEXT-TEXT_ID = 'GRUN'.
        LW_MATERIALLONGTEXT-LANGU = '1'.
        LW_MATERIALLONGTEXT-FORMAT_COL = 2.
        LW_MATERIALLONGTEXT-TEXT_LINE = LV_MAKTXL+132(132).
        APPEND LW_MATERIALLONGTEXT TO LT_MATERIALLONGTEXT.
      ENDIF.
      " 换算单位
      CLEAR: LT_UNITSOFMEASURE, LW_UNITSOFMEASURE, LT_UNITSOFMEASUREX, LW_UNITSOFMEASUREX, LT_SMEINH, LS_SMEINH.

*      IF ls_mara IS INITIAL .
      LT_SMEINH = US_INPUT-SMEINH.
      LOOP AT LT_SMEINH INTO LS_SMEINH.
        CLEAR: LV_MEINH.
        CUNIT_INPUT: LS_SMEINH-MEINH LV_MEINH.
********************2019.12.23  Seashell Huang*******************
        LW_UNITSOFMEASURE-GROSS_WT = US_INPUT-BRGEW.          " 毛重
        LW_UNITSOFMEASUREX-GROSS_WT = 'X'.
        LW_UNITSOFMEASURE-VOLUME = US_INPUT-VOLUM.            " 体积
        LW_UNITSOFMEASUREX-VOLUME = 'X'.
        IF US_INPUT-GEWEI IS NOT INITIAL.
          LW_UNITSOFMEASURE-UNIT_OF_WT = US_INPUT-GEWEI.      " 重量单位
        ELSE.
          LW_UNITSOFMEASURE-UNIT_OF_WT = 'KG'.
        ENDIF.
        LW_UNITSOFMEASUREX-UNIT_OF_WT = 'X'.
        LW_UNITSOFMEASURE-VOLUMEUNIT = US_INPUT-VOLEH.        " 体积单位
        LW_UNITSOFMEASUREX-VOLUMEUNIT = 'X'.
********************END******************************************
*        IF lv_meinh <> ls_mara-meins."旧版通过判断单位是否相等，不相等的话正常，但是相等的话会报错，所以去掉
        LW_UNITSOFMEASURE-DENOMINATR = LS_SMEINH-UMREN.       " 转换为基本计量单位的分母
        LW_UNITSOFMEASUREX-DENOMINATR = 'X'.
        LW_UNITSOFMEASURE-ALT_UNIT = LV_MEINH.                " 用于显示的计量单位
        LW_UNITSOFMEASUREX-ALT_UNIT = LV_MEINH.
        LW_UNITSOFMEASURE-NUMERATOR = LS_SMEINH-UMREZ.        " 基本计量单位转换分子
        LW_UNITSOFMEASUREX-NUMERATOR = 'X'.
*        ENDIF.
        APPEND LW_UNITSOFMEASURE TO LT_UNITSOFMEASURE.
        APPEND LW_UNITSOFMEASUREX TO LT_UNITSOFMEASUREX.
        CLEAR: LW_UNITSOFMEASURE, LW_UNITSOFMEASUREX.
      ENDLOOP.
      LOOP AT LT_MARM INTO DATA(LS_MARM).
        READ TABLE LT_UNITSOFMEASURE TRANSPORTING NO FIELDS WITH KEY ALT_UNIT = LS_MARM-MEINH.
        IF SY-SUBRC <> 0.
          IF LS_MARM-MEINH <> LS_MARA-MEINS.
            LW_UNITSOFMEASURE-DENOMINATR = LS_SMEINH-UMREN.       " 转换为基本计量单位的分母
            LW_UNITSOFMEASUREX-DENOMINATR = 'X'.
            LW_UNITSOFMEASURE-ALT_UNIT = LS_MARM-MEINH.           " 用于显示的计量单位
            LW_UNITSOFMEASUREX-ALT_UNIT = LS_MARM-MEINH.
            LW_UNITSOFMEASURE-NUMERATOR = LS_SMEINH-UMREZ.        " 基本计量单位转换分子
            LW_UNITSOFMEASUREX-NUMERATOR = 'X'.
            LW_UNITSOFMEASURE-DEL_FLAG = 'X'.                     " 删除标记
            APPEND LW_UNITSOFMEASURE TO LT_UNITSOFMEASURE.
            APPEND LW_UNITSOFMEASUREX TO LT_UNITSOFMEASUREX.
            CLEAR: LW_UNITSOFMEASURE, LW_UNITSOFMEASUREX.
          ENDIF.
        ENDIF.
      ENDLOOP.
*      ENDIF.
      CLEAR: LW_RETURN.
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          HEADDATA            = LW_HEADDATA
          CLIENTDATA          = LW_CLIENTDATA
          CLIENTDATAX         = LW_CLIENTDATAX
        IMPORTING
          RETURN              = LW_RETURN
        TABLES
          MATERIALDESCRIPTION = LT_MATERIALDESCRIPTION
          MATERIALLONGTEXT    = LT_MATERIALLONGTEXT
          UNITSOFMEASURE      = LT_UNITSOFMEASURE
          UNITSOFMEASUREX     = LT_UNITSOFMEASUREX.
      IF LW_RETURN-TYPE <> 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
*        CLEAR: LV_LENGTH.
*        LV_LENGTH = STRLEN( LV_BISMT ).
*        IF LV_LENGTH > 40.
*          DATA: LW_HEADER TYPE THEAD,
*                LT_LINES  TYPE STANDARD TABLE OF TLINE,
*                LW_LINES  TYPE TLINE.
*          CLEAR: LW_HEADER, LW_LINES, LT_LINES.
*          LW_HEADER-TDOBJECT = 'MATERIAL'.
*          LW_HEADER-TDNAME = LV_MATNR.
*          LW_HEADER-TDID = 'IVER'.
*          LW_HEADER-TDSPRAS = '1'.
*          LW_LINES-TDLINE = LV_BISMT.
*          LW_LINES-TDFORMAT = '*'.
*          APPEND LW_LINES TO LT_LINES.
*          CALL FUNCTION 'SAVE_TEXT'
*            EXPORTING
*              HEADER                  = LW_HEADER
*            TABLES
*              LINES                   = LT_LINES
*            EXCEPTIONS
*              ID                      = 1
*              LANGUAGE                = 2
*              NAME                    = 3
*              OBJECT                  = 4
*              OTHERS                  = 5.
*          IF SY-SUBRC = 0.
*            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*              EXPORTING
*                WAIT = 'X'.
*          ELSE.
*            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*          ENDIF.
*        ENDIF.
        " 扩张工厂视图
        IF US_INPUT-WERKS IS NOT INITIAL.
          PERFORM FRM_PLANT_VIEW USING US_INPUT LV_MATNR CHANGING CS_RETURN.
        ENDIF.
        " 扩张MRP AREA
        IF US_INPUT-MRP_AERA IS NOT INITIAL.
          PERFORM FRM_MRP_AERA_CREATE USING US_INPUT LV_MATNR CHANGING CS_RETURN.
        ENDIF.
        " 扩张销售组织视图
        IF US_INPUT-VKORG IS NOT INITIAL.
          PERFORM FRM_SALESORG_VIEW USING US_INPUT LV_MATNR CHANGING CS_RETURN.
        ENDIF.
        " 维护分类视图
        IF US_INPUT-CLASS IS NOT INITIAL.
          PERFORM FRM_CLASS_VIEW USING US_INPUT LV_MATNR LV_FLAG CHANGING CS_RETURN.
        ENDIF.
        IF CS_RETURN IS INITIAL.
          CS_RETURN-MSG_MATNR = LV_MATNR.
          CS_RETURN-MSG_TYPE = 'S'.
          CS_RETURN-MESSAGE = '成功'.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CS_RETURN-MSG_MATNR = LV_MATNR.
        CS_RETURN-MSG_TYPE = 'E'.
        CS_RETURN-MESSAGE = LW_RETURN-MESSAGE.
      ENDIF.
    ELSE.
      CLEAR: LV_MATNR_MPN, LV_MATNR_T.
      LV_MATNR_MPN = LV_MATNR+0(16).
      MATN1_INPUT: LV_MATNR_MPN.
      SELECT SINGLE MATNR FROM MARA INTO LV_MATNR_T
        WHERE MATNR = LV_MATNR_MPN AND MPROF = 'IMPN'.
      IF SY-SUBRC = 0.
        " 创建物料基本视图
        CLEAR: LW_HEADDATA, LW_CLIENTDATA, LW_CLIENTDATAX.
        LW_HEADDATA-MATERIAL = LV_MATNR.                        " 物料号
        LW_HEADDATA-MATL_TYPE = US_INPUT-MTART.                 " 物料类型
        LW_HEADDATA-IND_SECTOR = 'P'.                           " 行业领域
        LW_HEADDATA-BASIC_VIEW = 'X'.                           " 基本数据视图
        LW_HEADDATA-PURCHASE_VIEW = 'X'.                        " 采购视图
        LW_CLIENTDATA-MATL_GROUP = US_INPUT-MATKL.              " 物料组
        LW_CLIENTDATAX-MATL_GROUP = 'X'.
        LW_CLIENTDATA-BASE_UOM = LV_MEINS.                      " 基本计量单位
        LW_CLIENTDATAX-BASE_UOM = 'X'.
        LW_CLIENTDATA-MFR_NO = LV_MFRNR.                        " 制造商
        LW_CLIENTDATAX-MFR_NO = 'X'.
        LW_CLIENTDATA-INV_MAT_NO = LV_MATNR_MPN.                " 公司自有（内部）库存管理的物料编号
        LW_CLIENTDATAX-INV_MAT_NO = 'X'.
        LW_CLIENTDATA-MANU_MAT = LV_MATNR.                      " 制造商零件编号
        LW_CLIENTDATAX-MANU_MAT = 'X'.

        LV_MPROF = US_INPUT-ZRSV01 .
        IF LV_MPROF <> '' .
          LW_CLIENTDATA-MANUF_PROF = LV_MPROF.                      " 基本计量单位
          LW_CLIENTDATAX-MANUF_PROF = 'X'.
        ENDIF .

        IF US_INPUT-LVOMA IS NOT INITIAL.
          LW_CLIENTDATA-DEL_FLAG = 'X'.                         " 在客户级标记要删除的物料
          LW_CLIENTDATAX-DEL_FLAG = 'X'.
        ELSE.
          LW_CLIENTDATA-DEL_FLAG = ''.                          " 在客户级标记要删除的物料
          LW_CLIENTDATAX-DEL_FLAG = 'X'.
        ENDIF.

        " 物料描述
        CLEAR: LT_MATERIALDESCRIPTION, LW_MATERIALDESCRIPTION.
        LW_MATERIALDESCRIPTION-LANGU = '1'.
        LW_MATERIALDESCRIPTION-MATL_DESC = US_INPUT-MAKTX.      " 物料描述
        APPEND LW_MATERIALDESCRIPTION TO LT_MATERIALDESCRIPTION.

        " 物料描述长文本
        IF US_INPUT-MAKTXL IS NOT INITIAL.
          CLEAR: LT_MATERIALLONGTEXT, LW_MATERIALLONGTEXT, LV_MAKTXL.
          LV_MAKTXL = US_INPUT-MAKTXL.
          LW_MATERIALLONGTEXT-APPLOBJECT = 'MATERIAL'.
          LW_MATERIALLONGTEXT-TEXT_NAME = LV_MATNR.
          LW_MATERIALLONGTEXT-TEXT_ID = 'GRUN'.
          LW_MATERIALLONGTEXT-LANGU = '1'.
          LW_MATERIALLONGTEXT-FORMAT_COL = 1.
          LW_MATERIALLONGTEXT-TEXT_LINE = LV_MAKTXL+0(132).
          APPEND LW_MATERIALLONGTEXT TO LT_MATERIALLONGTEXT.
          CLEAR: LW_MATERIALLONGTEXT.
          LW_MATERIALLONGTEXT-APPLOBJECT = 'MATERIAL'.
          LW_MATERIALLONGTEXT-TEXT_NAME = LV_MATNR.
          LW_MATERIALLONGTEXT-TEXT_ID = 'GRUN'.
          LW_MATERIALLONGTEXT-LANGU = '1'.
          LW_MATERIALLONGTEXT-FORMAT_COL = 2.
          LW_MATERIALLONGTEXT-TEXT_LINE = LV_MAKTXL+132(132).
          APPEND LW_MATERIALLONGTEXT TO LT_MATERIALLONGTEXT.
        ENDIF.
        CLEAR: LW_RETURN.
        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
          EXPORTING
            HEADDATA            = LW_HEADDATA
            CLIENTDATA          = LW_CLIENTDATA
            CLIENTDATAX         = LW_CLIENTDATAX
          IMPORTING
            RETURN              = LW_RETURN
          TABLES
            MATERIALDESCRIPTION = LT_MATERIALDESCRIPTION
            MATERIALLONGTEXT    = LT_MATERIALLONGTEXT.
        IF LW_RETURN-TYPE <> 'E'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.
          CS_RETURN-MSG_MATNR = LV_MATNR.
          CS_RETURN-MSG_TYPE = 'S'.
          CS_RETURN-MESSAGE = '成功'.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          CS_RETURN-MSG_MATNR = LV_MATNR.
          CS_RETURN-MSG_TYPE = 'E'.
          CS_RETURN-MESSAGE = LW_RETURN-MESSAGE.
        ENDIF.
      ELSE.
        CS_RETURN-MSG_MATNR = LV_MATNR.
        CS_RETURN-MSG_TYPE = 'E'.
        CONCATENATE '未找到MPN物料' LV_MATNR_MPN INTO CS_RETURN-MESSAGE.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_MRPAREA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_MATNR
*&      --> LV_WERKS
*&      --> LV_BERID
*&      <-- LV_FLAG
*&---------------------------------------------------------------------*
FORM FRM_GET_MRPAREA  USING    UV_MATNR TYPE MATNR
                               UV_WERKS TYPE WERKDP
                               UV_BERID TYPE BERID
                      CHANGING CV_FLAG  TYPE C.
  DATA: LW_MT61D TYPE MT61D,
        LW_MDKP  TYPE MDKP,
        LW_CM61M TYPE CM61M,
        LW_MDSTA TYPE MDSTA,
        LW_ERGBZ TYPE SFILT-ERGBZ,
        LT_MDPSX TYPE STANDARD TABLE OF MDPS,
        LT_MDEZX TYPE STANDARD TABLE OF MDEZ,
        LT_MDSUX TYPE STANDARD TABLE OF MDSU.
  CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
    EXPORTING
      MATNR                    = UV_MATNR
      WERKS                    = UV_WERKS
      BERID                    = UV_BERID
    IMPORTING
      E_MT61D                  = LW_MT61D
      E_MDKP                   = LW_MDKP
      E_CM61M                  = LW_CM61M
      E_MDSTA                  = LW_MDSTA
      E_ERGBZ                  = LW_ERGBZ
    TABLES
      MDPSX                    = LT_MDPSX
      MDEZX                    = LT_MDEZX
      MDSUX                    = LT_MDSUX
    EXCEPTIONS
      MATERIAL_PLANT_NOT_FOUND = 1
      PLANT_NOT_FOUND          = 2
      OTHERS                   = 3.
  IF SY-SUBRC = 0 AND LW_MT61D IS NOT INITIAL.
    CV_FLAG = 'X'.
  ENDIF.

ENDFORM.