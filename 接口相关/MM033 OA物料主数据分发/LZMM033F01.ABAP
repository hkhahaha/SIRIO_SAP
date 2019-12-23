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
FORM frm_check_exist  USING    uv_matnr TYPE matnr
                               uv_plant TYPE werks_d
*                               UV_SALES TYPE VKORG
*                               UV_DISTR TYPE VTWEG
                      CHANGING cv_subrc TYPE c.
  DATA: ls_clientdata TYPE bapi_mara_ga,
        ls_plantdata  TYPE bapi_marc_ga,
        ls_salesdata  TYPE bapi_mvke_ga,
        lv_matnr      TYPE bapi_mara_ga-material.
  CLEAR: ls_clientdata, ls_plantdata, ls_salesdata, lv_matnr.
  lv_matnr = uv_matnr.
  CALL FUNCTION 'BAPI_MATERIAL_GET_ALL'
    EXPORTING
      material   = lv_matnr
      plant      = uv_plant
*     SALESORG   = UV_SALES
*     DISTR_CHAN = UV_DISTR
    IMPORTING
      clientdata = ls_clientdata
      plantdata  = ls_plantdata
      salesdata  = ls_salesdata.
  " CHECK MATERIAL DATA EXIST
  IF ls_clientdata IS INITIAL.
    cv_subrc = 'X'.
  ELSE.
    " CHECK PLANT DATA EXIST
    IF uv_plant IS NOT INITIAL.
      IF ls_plantdata IS INITIAL.
        cv_subrc = 'X'.
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
FORM frm_create_matnr  USING    us_input  TYPE zdt_oa2sap_mdm_mara
                       CHANGING cs_return TYPE zdt_oa2sap_mdm_ret_zmd001.
  DATA: lw_headdata            TYPE bapimathead,
        lw_clientdata          TYPE bapi_mara,
        lw_clientdatax         TYPE bapi_marax,
        lw_return              TYPE bapiret2,
        lt_unitsofmeasure      TYPE STANDARD TABLE OF bapi_marm,
        lw_unitsofmeasure      TYPE bapi_marm,
        lt_unitsofmeasurex     TYPE STANDARD TABLE OF bapi_marmx,
        lw_unitsofmeasurex     TYPE bapi_marmx,
        lt_materialdescription TYPE STANDARD TABLE OF bapi_makt,
        lw_materialdescription TYPE bapi_makt.

  DATA: lt_smeinh TYPE zdt_oa2sap_mdm_mara_smeinh_tab,
        ls_smeinh TYPE zdt_oa2sap_mdm_mara_smeinh,
        lv_mtart  TYPE mtart,
        lv_matnr  TYPE matnr,
        lv_flag   TYPE c.
  " 取物料码段
  CLEAR: lv_matnr, lv_mtart, lv_flag.
  lv_mtart = us_input-mtart.
  PERFORM frm_get_matnr USING lv_mtart CHANGING lv_matnr.
  " 创建物料基本视图
  CLEAR: lw_headdata, lw_clientdata, lw_clientdatax.
  lw_headdata-material = lv_matnr.                        " 物料号
  lw_headdata-matl_type = us_input-mtart.                 " 物料类型
  lw_headdata-ind_sector = 'P'.                           " 行业领域
  lw_headdata-basic_view = 'X'.                           " 基本数据视图
  lw_clientdata-matl_group = us_input-matkl.              " 物料组
  lw_clientdatax-matl_group = 'X'.
  lw_clientdata-old_mat_no = us_input-bismt.              " 旧物料号
  lw_clientdatax-old_mat_no = 'X'.
  lw_clientdata-base_uom = us_input-meins.                " 基本计量单位
  lw_clientdatax-base_uom = 'X'.
  lw_clientdata-size_dim = us_input-groes.                " 规格型号（大小/量纲）
  lw_clientdatax-size_dim = 'X'.
  lw_clientdata-mfr_no = us_input-mfrnr.                  " 制造商
  lw_clientdatax-mfr_no = 'X'.
  lw_clientdata-pur_valkey = us_input-ekwsl.              " 采购价值代码
  lw_clientdatax-pur_valkey = 'X'.
  CLEAR: lt_materialdescription, lw_materialdescription.
  lw_materialdescription-langu = '1'.
  lw_materialdescription-matl_desc = us_input-maktx.      " 物料描述
  APPEND lw_materialdescription TO lt_materialdescription.
  CLEAR: lt_unitsofmeasure, lw_unitsofmeasure, lt_unitsofmeasurex, lw_unitsofmeasurex, lt_smeinh, ls_smeinh.
  lt_smeinh = us_input-smeinh.
  LOOP AT lt_smeinh INTO ls_smeinh.
    lw_unitsofmeasure-gross_wt = us_input-brgew.          " 毛重
    lw_unitsofmeasurex-gross_wt = 'X'.
    lw_unitsofmeasure-volume = us_input-volum.            " 体积
    lw_unitsofmeasurex-volume = 'X'.
    lw_unitsofmeasure-unit_of_wt = us_input-gewei.        " 重量单位
    lw_unitsofmeasurex-unit_of_wt = 'X'.
    lw_unitsofmeasure-volumeunit = us_input-voleh.        " 体积单位
    lw_unitsofmeasurex-volumeunit = 'X'.
    lw_unitsofmeasure-denominatr = ls_smeinh-umren.       " 转换为基本计量单位的分母
    lw_unitsofmeasurex-denominatr = 'X'.
    lw_unitsofmeasure-alt_unit = ls_smeinh-meinh.         " 用于显示的计量单位
    lw_unitsofmeasurex-alt_unit = ls_smeinh-meinh.
    lw_unitsofmeasure-numerator = ls_smeinh-umrez.        " 基本计量单位转换分子
    lw_unitsofmeasurex-numerator = 'X'.
    APPEND lw_unitsofmeasure TO lt_unitsofmeasure.
    APPEND lw_unitsofmeasurex TO lt_unitsofmeasurex.
    CLEAR: lw_unitsofmeasure, lw_unitsofmeasurex.
  ENDLOOP.
  CLEAR: lw_return.
  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      headdata            = lw_headdata
      clientdata          = lw_clientdata
      clientdatax         = lw_clientdatax
    IMPORTING
      return              = lw_return
    TABLES
      materialdescription = lt_materialdescription
      unitsofmeasure      = lt_unitsofmeasure
      unitsofmeasurex     = lt_unitsofmeasurex.
  IF lw_return-type <> 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    " 扩张工厂视图
    IF us_input-werks IS NOT INITIAL.
      PERFORM frm_plant_view USING us_input lv_matnr CHANGING cs_return.
    ENDIF.
    " 扩张MRP AREA
    IF us_input-mrp_aera IS NOT INITIAL.
      PERFORM frm_mrp_aera_create USING us_input lv_matnr CHANGING cs_return.
    ENDIF.
    " 扩张销售组织视图
    IF us_input-vkorg IS NOT INITIAL.
      PERFORM frm_salesorg_view USING us_input lv_matnr CHANGING cs_return.
    ENDIF.
    " 维护分类视图
    IF us_input-class IS NOT INITIAL.
      PERFORM frm_class_view USING us_input lv_matnr lv_flag CHANGING cs_return.
    ENDIF.
    IF cs_return IS INITIAL.
      cs_return-msg_matnr = lv_matnr.
      cs_return-msg_type = 'S'.
      cs_return-message = '成功'.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    cs_return-msg_matnr = lv_matnr.
    cs_return-msg_type = 'E'.
    cs_return-message = lw_return-message.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_MATNR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_MATNR
*&---------------------------------------------------------------------*
FORM frm_get_matnr  USING    uv_mtart TYPE mtart
                    CHANGING cv_matnr TYPE matnr.
  DEFINE matn1_input.
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
  DATA: lv_matnr TYPE char18.
  CLEAR: lv_matnr.
  IF uv_mtart = 'Z010'.
    SELECT MAX( matnr ) INTO cv_matnr FROM mara
      WHERE matnr BETWEEN '000000000100000001' AND '000000000109999999'.
    IF sy-subrc <> 0.
      cv_matnr = '000000000100000001'.
    ELSE.
      lv_matnr = cv_matnr.
      lv_matnr = lv_matnr + 1.
      cv_matnr = lv_matnr.
      CONDENSE cv_matnr NO-GAPS.
      matn1_input: cv_matnr.
    ENDIF.
  ELSEIF uv_mtart = 'Z020'.
    SELECT MAX( matnr ) INTO cv_matnr FROM mara
      WHERE matnr BETWEEN '000000000200000001' AND '000000000209999999'.
    IF sy-subrc <> 0.
      cv_matnr = '000000000200000001'.
    ELSE.
      lv_matnr = cv_matnr.
      lv_matnr = lv_matnr + 1.
      cv_matnr = lv_matnr.
      CONDENSE cv_matnr NO-GAPS.
      matn1_input: cv_matnr.
    ENDIF.
  ELSEIF uv_mtart = 'Z030'.
    SELECT MAX( matnr ) INTO cv_matnr FROM mara
      WHERE matnr BETWEEN '000000000300000001' AND '000000000309999999'.
    IF sy-subrc <> 0.
      cv_matnr = '000000000300000001'.
    ELSE.
      lv_matnr = cv_matnr.
      lv_matnr = lv_matnr + 1.
      cv_matnr = lv_matnr.
      CONDENSE cv_matnr NO-GAPS.
      matn1_input: cv_matnr.
    ENDIF.
  ELSEIF uv_mtart = 'Z040'.
    SELECT MAX( matnr ) INTO cv_matnr FROM mara
      WHERE matnr BETWEEN '000000000400000001' AND '000000000409999999'.
    IF sy-subrc <> 0.
      cv_matnr = '000000000400000001'.
    ELSE.
      lv_matnr = cv_matnr.
      lv_matnr = lv_matnr + 1.
      cv_matnr = lv_matnr.
      CONDENSE cv_matnr NO-GAPS.
      matn1_input: cv_matnr.
    ENDIF.
  ELSEIF uv_mtart = 'Z050'.
    SELECT MAX( matnr ) INTO cv_matnr FROM mara
      WHERE matnr BETWEEN '000000000500000001' AND '000000000509999999'.
    IF sy-subrc <> 0.
      cv_matnr = '000000000500000001'.
    ELSE.
      lv_matnr = cv_matnr.
      lv_matnr = lv_matnr + 1.
      cv_matnr = lv_matnr.
      CONDENSE cv_matnr NO-GAPS.
      matn1_input: cv_matnr.
    ENDIF.
  ELSEIF uv_mtart = 'Z060'.
    SELECT MAX( matnr ) INTO cv_matnr FROM mara
      WHERE matnr BETWEEN '000000000600000001' AND '000000000609999999'.
    IF sy-subrc <> 0.
      cv_matnr = '000000000600000001'.
    ELSE.
      lv_matnr = cv_matnr.
      lv_matnr = lv_matnr + 1.
      cv_matnr = lv_matnr.
      CONDENSE cv_matnr NO-GAPS.
      matn1_input: cv_matnr.
    ENDIF.
  ELSEIF uv_mtart = 'Z070'.
    SELECT MAX( matnr ) INTO cv_matnr FROM mara
      WHERE matnr BETWEEN '000000000700000001' AND '000000000709999999'.
    IF sy-subrc <> 0.
      cv_matnr = '000000000700000001'.
    ELSE.
      lv_matnr = cv_matnr.
      lv_matnr = lv_matnr + 1.
      cv_matnr = lv_matnr.
      CONDENSE cv_matnr NO-GAPS.
      matn1_input: cv_matnr.
    ENDIF.
  ELSEIF uv_mtart = 'Z080'.
    SELECT MAX( matnr ) INTO cv_matnr FROM mara
      WHERE matnr BETWEEN '000000000800000001' AND '000000000809999999'.
    IF sy-subrc <> 0.
      cv_matnr = '000000000800000001'.
    ELSE.
      lv_matnr = cv_matnr.
      lv_matnr = lv_matnr + 1.
      cv_matnr = lv_matnr.
      CONDENSE cv_matnr NO-GAPS.
      matn1_input: cv_matnr.
    ENDIF.
  ELSEIF uv_mtart = 'Z090'.
    SELECT MAX( matnr ) INTO cv_matnr FROM mara
      WHERE matnr BETWEEN '000000000900000001' AND '000000000909999999'.
    IF sy-subrc <> 0.
      cv_matnr = '000000000900000001'.
    ELSE.
      lv_matnr = cv_matnr.
      lv_matnr = lv_matnr + 1.
      cv_matnr = lv_matnr.
      CONDENSE cv_matnr NO-GAPS.
      matn1_input: cv_matnr.
    ENDIF.
  ELSEIF uv_mtart = 'Z098'.
    SELECT MAX( matnr ) INTO cv_matnr FROM mara
      WHERE matnr BETWEEN '000000000980000001' AND '000000000989999999'.
    IF sy-subrc <> 0.
      cv_matnr = '000000000980000001'.
    ELSE.
      lv_matnr = cv_matnr.
      lv_matnr = lv_matnr + 1.
      cv_matnr = lv_matnr.
      CONDENSE cv_matnr NO-GAPS.
      matn1_input: cv_matnr.
    ENDIF.
  ELSEIF uv_mtart = 'Z099'.
    SELECT MAX( matnr ) INTO cv_matnr FROM mara
      WHERE matnr BETWEEN '000000000990000001' AND '000000000999999999'.
    IF sy-subrc <> 0.
      cv_matnr = '000000000990000001'.
    ELSE.
      lv_matnr = cv_matnr.
      lv_matnr = lv_matnr + 1.
      cv_matnr = lv_matnr.
      CONDENSE cv_matnr NO-GAPS.
      matn1_input: cv_matnr.
    ENDIF.
  ELSEIF uv_mtart = 'HERS'.
    SELECT MAX( matnr ) INTO cv_matnr FROM mara
      WHERE matnr BETWEEN '000000001000000001' AND '000000001099999999'.
    IF sy-subrc <> 0.
      cv_matnr = '000000001000000001'.
    ELSE.
      lv_matnr = cv_matnr.
      lv_matnr = lv_matnr + 1.
      cv_matnr = lv_matnr.
      CONDENSE cv_matnr NO-GAPS.
      matn1_input: cv_matnr.
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
FORM frm_plant_view  USING    us_input  TYPE zdt_oa2sap_mdm_mara
                              uv_matnr  TYPE matnr
                     CHANGING cs_return TYPE zdt_oa2sap_mdm_ret_zmd001.
  DEFINE matn1_input.
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

  DATA: lt_werks   TYPE zdt_oa2sap_mdm_mara_werks_tab,
        ls_werks   TYPE zdt_oa2sap_mdm_mara_werks,
        lt_qmat    TYPE zdt_oa2sap_mdm_mara_werks__tab,
        ls_qmat    TYPE zdt_oa2sap_mdm_mara_werks_qmat,
        lt_tq34    TYPE STANDARD TABLE OF tq34,
        lw_tq34    TYPE tq34,
        lv_matnr   TYPE matnr18,
        lv_nfmat40 TYPE nfmat40,
        lv_mess    TYPE c LENGTH 1000,
        lv_mess1   TYPE c LENGTH 1000.
  DATA: lw_headdata       TYPE bapimathead,
        lw_clientdata     TYPE bapi_mara,
        lw_clientdatax    TYPE bapi_marax,
        lt_return         TYPE STANDARD TABLE OF bapiret2,
        lw_return         TYPE bapiret2,
        lw_plantdata      TYPE bapi_marc,
        lw_plantdatax     TYPE bapi_marcx,
        lw_valuationdata  TYPE bapi_mbew,
        lw_valuationdatax TYPE bapi_mbewx.
  DATA: lt_inspectionctrl TYPE STANDARD TABLE OF bapi1001004_qmat,
        lw_inspectionctrl TYPE bapi1001004_qmat.

  DATA: ls_marc TYPE marc,
        ls_mbew TYPE mbew,
        ls_mara TYPE mara.


  " 检验类型缺省值
  CLEAR: lt_tq34.
  SELECT * FROM tq34 INTO TABLE lt_tq34.

  CLEAR: lt_werks, ls_werks, lv_mess.
  lt_werks = us_input-werks.


  LOOP AT lt_werks INTO ls_werks.
    IF ls_werks-werks IS NOT INITIAL.


***************** add by sujx 20191201**********************************
      CLEAR: ls_mbew, ls_marc, ls_mara.

      SELECT SINGLE * INTO ls_mbew
        FROM mbew
       WHERE matnr = uv_matnr
         AND bwkey = ls_werks-werks.
      IF sy-subrc <> 0.
        CLEAR: ls_mbew.
      ENDIF.

      SELECT SINGLE * INTO ls_marc
        FROM marc
       WHERE matnr = uv_matnr
         AND werks = ls_werks-werks.
      IF sy-subrc <> 0.
        CLEAR: ls_marc.
      ENDIF.

      SELECT SINGLE * INTO ls_mara
        FROM mara
       WHERE matnr = uv_matnr.
      IF sy-subrc <> 0.
        CLEAR: ls_mara.
      ENDIF.

******************* end add ******************************************


      CLEAR: lw_headdata.
      lw_headdata-material = uv_matnr.                                    " 物料号
      lw_headdata-matl_type = us_input-mtart.                             " 物料类型
      lw_headdata-ind_sector = 'P'.                                       " 行业领域
      lw_headdata-purchase_view = 'X'.                                    " 采购视图
      IF us_input-mtart <> 'Z080'
        AND us_input-mtart <> 'Z098'.
*        AND us_input-mtart <> 'Z099'.
        lw_headdata-mrp_view = 'X'.                                       " MRP视图
      ENDIF.
      lw_headdata-work_sched_view = 'X'.                                  " 工作计划视图
      IF us_input-mtart <> 'Z099'.
        lw_headdata-account_view = 'X'.                                   " 会计视图
        lw_headdata-storage_view = 'X'.                                   " 库存视图
        lw_headdata-cost_view = 'X'.                                      " 成本视图
      ENDIF.
      IF us_input-mtart <> 'Z040'
        AND us_input-mtart <> 'Z099'.
        lw_headdata-quality_view = 'X'.                                   " 质量管理视图
      ENDIF.
      CLEAR: lw_clientdata, lw_clientdatax.

      " 温度条件
      IF ls_werks-tempb <> ls_mara-tempb OR ls_mara IS INITIAL.
        lw_clientdata-temp_conds = ls_werks-tempb.
        lw_clientdatax-temp_conds = 'X'.
      ENDIF.

      " 存储条件
      IF ls_werks-raube <> ls_mara-raube OR ls_mara IS INITIAL.
        lw_clientdata-stor_conds = ls_werks-raube.
        lw_clientdatax-stor_conds = 'X'.
      ENDIF.

      " 最小剩余货架寿命
      IF ls_werks-mhdrz <> ls_mara-mhdrz OR ls_mara IS INITIAL.
        lw_clientdata-minremlife = ls_werks-mhdrz.
        lw_clientdatax-minremlife = 'X'.
      ENDIF.

*      lw_clientdata-temp_conds = ls_werks-tempb.                          " 温度条件
*      lw_clientdata-stor_conds = ls_werks-raube.                          " 存储条件
*      lw_clientdata-minremlife = ls_werks-mhdrz.                          " 最小剩余货架寿命
      " 总货架寿命
      IF us_input-mtart <> 'Z010'
        AND us_input-mtart <> 'Z020'
        AND us_input-mtart <> 'Z040'.
        IF ls_werks-mhdhb <> ls_mara-mhdhb OR ls_mara IS INITIAL.
          lw_clientdata-shelf_life = ls_werks-mhdhb.
          lw_clientdatax-shelf_life = 'X'.
        ENDIF.
*        lw_clientdata-shelf_life = ls_werks-mhdhb.                        " 总货架寿命
*        lw_clientdatax-shelf_life = 'X'.
      ENDIF.
      " 激活采购中的 QM
      IF us_input-mtart = 'Z010'
        OR us_input-mtart = 'Z020'.
        IF ls_mara-qmpur IS INITIAL OR ls_mara IS INITIAL.
          lw_clientdata-qm_procmnt = 'X'.
          lw_clientdatax-qm_procmnt = 'X'.
        ENDIF.
*        lw_clientdata-qm_procmnt = 'X'.                                   " 激活采购中的 QM
*        lw_clientdatax-qm_procmnt = 'X'.
      ENDIF.

      " 期间标识
      IF ls_werks-iprkz <> ls_mara-iprkz OR ls_mara IS INITIAL.
        lw_clientdata-period_ind_expiration_date = ls_werks-iprkz.
        lw_clientdatax-period_ind_expiration_date = 'X'.
      ENDIF.

      " 舍入规则
      IF ls_werks-rdmhd <> ls_mara-rdmhd OR ls_mara IS INITIAL.
        lw_clientdata-round_up_rule_expiration_date = ls_werks-rdmhd.
        lw_clientdatax-round_up_rule_expiration_date = 'X'.
      ENDIF.

      " 采购价值代码
      IF us_input-ekwsl <> ls_mara-ekwsl OR ls_mara IS INITIAL.
        lw_clientdata-pur_valkey = us_input-ekwsl.
        lw_clientdatax-pur_valkey = 'X'.
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

      " 制造商部件的参数文件
      IF us_input-mtart = 'Z010' AND ls_werks-sobsl IS INITIAL.
        IF ls_mara-mprof IS INITIAL OR ls_mara IS INITIAL.
          lw_clientdata-manuf_prof = 'IMPN'.
          lw_clientdatax-manuf_prof = 'X'.
        ENDIF.
*        lw_clientdata-manuf_prof = 'IMPN'.
*        lw_clientdatax-manuf_prof = 'X'.
      ENDIF.

      CLEAR: lw_plantdata, lw_plantdatax.
      lw_plantdata-plant = ls_werks-werks.                              " 工厂
      lw_plantdatax-plant = ls_werks-werks.
      lw_plantdata-del_flag = ls_werks-lvoma.                           " 删除标记
      lw_plantdatax-del_flag = 'X'.
      " 采购组
      IF ls_marc-ekgrp <> ls_werks-ekgrp OR ls_marc IS INITIAL.
        lw_plantdata-pur_group = ls_werks-ekgrp.
        lw_plantdatax-pur_group = 'X'.
      ENDIF.
      " 源清单
      IF ls_marc-kordb <> ls_werks-kordb OR ls_marc IS INITIAL.
        lw_plantdata-sourcelist = ls_werks-kordb.
        lw_plantdatax-sourcelist = 'X'.
      ENDIF.
      " 批次管理
      IF ls_marc-xchpf <> ls_werks-xchpf OR ls_marc IS INITIAL.
        lw_plantdata-batch_mgmt = ls_werks-xchpf.
        lw_plantdatax-batch_mgmt = 'X'.
      ENDIF.
      " 利润中心
      IF ls_marc-prctr <> ls_werks-prctl OR ls_marc IS INITIAL.
        lw_plantdata-profit_ctr = ls_werks-prctl.
        lw_plantdatax-profit_ctr = 'X'.
      ENDIF.
      " 不计算成本
      IF ls_marc-ncost <> ls_werks-ncost OR ls_marc IS INITIAL.
        lw_plantdata-no_costing = ls_werks-ncost.
        lw_plantdatax-no_costing = 'X'.
      ENDIF.
      " 特定工厂的物料状态
      IF ls_marc-mmsta <> ls_werks-mmsta OR ls_marc IS INITIAL.
        lw_plantdata-pur_status = ls_werks-mmsta.
        lw_plantdatax-pur_status = 'X'.
      ENDIF.
      " 特定工厂的物料状态
      IF ls_marc-mmstd <> ls_werks-mmstd OR ls_marc IS INITIAL.
        lw_plantdata-pvalidfrom = ls_werks-mmstd.
        lw_plantdatax-pvalidfrom = 'X'.
      ENDIF.
      " 最大批量大小
      IF ls_marc-bstma <> ls_werks-bstma OR ls_marc IS INITIAL.
        lw_plantdata-maxlotsize = ls_werks-bstma.
        lw_plantdatax-maxlotsize = 'X'.
      ENDIF.
      " 最小批量大小
      IF ls_marc-bstmi <> ls_werks-bstmi OR ls_marc IS INITIAL.
        lw_plantdata-minlotsize = ls_werks-bstmi.
        lw_plantdatax-minlotsize = 'X'.
      ENDIF.
      " 舍入值
      IF ls_marc-bstrf <> ls_werks-bstrf OR ls_marc IS INITIAL.
        lw_plantdata-round_val = ls_werks-bstrf.
        lw_plantdatax-round_val = 'X'.
      ENDIF.
      " 计划交货时间
      IF ls_marc-plifz <> ls_werks-plifz OR ls_marc IS INITIAL.
        lw_plantdata-plnd_delry = ls_werks-plifz.
        lw_plantdatax-plnd_delry = 'X'.
      ENDIF.
      " MRP类型
      IF ls_marc-dismm <> ls_werks-dismm OR ls_marc IS INITIAL.
        lw_plantdata-mrp_type = ls_werks-dismm.
        lw_plantdatax-mrp_type = 'X'.
      ENDIF.
      " MRP控制者/物料需求计划控制员
      IF ls_marc-dispo <> ls_werks-dispo OR ls_marc IS INITIAL.
        lw_plantdata-mrp_ctrler = ls_werks-dispo.
        lw_plantdatax-mrp_ctrler = 'X'.
      ENDIF.
      " 批量大小
      IF ls_marc-disls <> ls_werks-disls OR ls_marc IS INITIAL.
        lw_plantdata-lotsizekey = ls_werks-disls.
        lw_plantdatax-lotsizekey = 'X'.
      ENDIF.
      " 采购类型
      IF ls_marc-beskz <> ls_werks-beskz OR ls_marc IS INITIAL.
        lw_plantdata-proc_type = ls_werks-beskz.
        lw_plantdatax-proc_type = 'X'.
      ENDIF.
      " 特殊采购类
      IF ls_marc-sobsl <> ls_werks-sobsl OR ls_marc IS INITIAL.
        lw_plantdata-spproctype = ls_werks-sobsl.
        lw_plantdatax-spproctype = 'X'.
      ENDIF.
      " 安全库存
      IF ls_marc-eisbe <> ls_werks-eisbe OR ls_marc IS INITIAL.
        lw_plantdata-safety_stk = ls_werks-eisbe.
        lw_plantdatax-safety_stk = 'X'.
      ENDIF.
      " 策略组
      IF ls_marc-strgr <> ls_werks-strgr OR ls_marc IS INITIAL.
        lw_plantdata-plan_strgp = ls_werks-strgr.
        lw_plantdatax-plan_strgp = 'X'.
      ENDIF.
      " 不足交货容差
      IF ls_marc-uneto <> ls_werks-uneto OR ls_marc IS INITIAL.
        lw_plantdata-under_tol = ls_werks-uneto.
        lw_plantdatax-under_tol = 'X'.
      ENDIF.
      " 厂内生产时间
      IF ls_marc-dzeit <> ls_werks-dzeit OR ls_marc IS INITIAL.
        lw_plantdata-inhseprodt = ls_werks-dzeit.
        lw_plantdatax-inhseprodt = 'X'.
      ENDIF.
      " 固定批量
      IF ls_marc-bstfe <> ls_werks-bstfe OR ls_marc IS INITIAL.
        lw_plantdata-fixed_lot = ls_werks-bstfe.
        lw_plantdatax-fixed_lot = 'X'.
      ENDIF.
      " 过度交货容差
      IF ls_marc-ueeto <> ls_werks-ueeto OR ls_marc IS INITIAL.
        lw_plantdata-over_tol = ls_werks-ueeto.
        lw_plantdatax-over_tol = 'X'.
      ENDIF.
      " 检查间隔
      IF ls_marc-prfrq <> ls_werks-prfrq OR ls_marc IS INITIAL.
        lw_plantdata-insp_int = ls_werks-prfrq.
        lw_plantdatax-insp_int = 'X'.
      ENDIF.
      " MRP组
      IF ls_marc-disgr <> ls_werks-disgr OR ls_marc IS INITIAL.
        lw_plantdata-mrp_group = ls_werks-disgr.
        lw_plantdatax-mrp_group = 'X'.
      ENDIF.
      " 计划时界
      IF ls_marc-fxhor <> ls_werks-fxhor OR ls_marc IS INITIAL.
        lw_plantdata-pl_ti_fnce = ls_werks-fxhor.
        lw_plantdatax-pl_ti_fnce = 'X'.
      ENDIF.
      " 反冲
      IF ls_marc-rgekz <> ls_werks-rgekz OR ls_marc IS INITIAL.
        lw_plantdata-backflush = ls_werks-rgekz.
        lw_plantdatax-backflush = 'X'.
      ENDIF.
      " 计划边际码
      IF ls_marc-fhori <> ls_werks-fhori OR ls_marc IS INITIAL.
        lw_plantdata-sm_key = ls_werks-fhori.
        lw_plantdatax-sm_key = 'X'.
      ENDIF.
      " 消耗模式
      IF ls_marc-vrmod <> ls_werks-vrmod OR ls_marc IS INITIAL.
        lw_plantdata-consummode = ls_werks-vrmod.
        lw_plantdatax-consummode = 'X'.
      ENDIF.
      " 向前消耗时间
      IF ls_marc-vint2 <> ls_werks-vint1 OR ls_marc IS INITIAL.
        lw_plantdata-fwd_cons = ls_werks-vint1.
        lw_plantdatax-fwd_cons = 'X'.
      ENDIF.
      " 逆向消耗时间
      IF ls_marc-vint1 <> ls_werks-vint2 OR ls_marc IS INITIAL.
        lw_plantdata-bwd_cons = ls_werks-vint2.
        lw_plantdatax-bwd_cons = 'X'.
      ENDIF.
      " 独立/集中
      IF ls_marc-sbdkz <> ls_werks-sbdkz OR ls_marc IS INITIAL.
        lw_plantdata-dep_req_id = ls_werks-sbdkz.
        lw_plantdatax-dep_req_id = 'X'.
      ENDIF.
      " 非连续标识
      IF ls_marc-kzaus <> ls_werks-kzaus OR ls_marc IS INITIAL.
        lw_plantdata-discontinu = ls_werks-kzaus.
        lw_plantdatax-discontinu = 'X'.
      ENDIF.
      " 生效期
      IF ls_marc-ausdt <> ls_werks-ausdt OR ls_marc IS INITIAL.
        lw_plantdata-eff_o_day = ls_werks-ausdt.
        lw_plantdatax-eff_o_day = 'X'.
      ENDIF.
      " 后继物料
      CLEAR: lv_nfmat40.
      lv_nfmat40 = ls_werks-nfmat.
      matn1_input: lv_nfmat40.
      IF ls_marc-nfmat <> lv_nfmat40 OR ls_marc IS INITIAL.
        lw_plantdata-follow_up_long = lv_nfmat40.
        lw_plantdatax-follow_up_long = 'X'.
      ENDIF.
      " 生产管理员
      IF ls_marc-fevor <> ls_werks-fevor OR ls_marc IS INITIAL.
        lw_plantdata-production_scheduler = ls_werks-fevor.
        lw_plantdatax-production_scheduler = 'X'.
      ENDIF.
      " 生产计划参数文件
      IF ls_marc-sfcpf <> ls_werks-sfcpf OR ls_marc IS INITIAL.
        lw_plantdata-prodprof = ls_werks-sfcpf.
        lw_plantdatax-prodprof = 'X'.
      ENDIF.
      " 处理时间
      IF ls_marc-bearz <> ls_werks-bearz OR ls_marc IS INITIAL.
        lw_plantdata-proc_time = ls_werks-bearz.
        lw_plantdatax-proc_time = 'X'.
      ENDIF.
      " 装配报废比例
      IF ls_marc-ausss <> ls_werks-ausss OR ls_marc IS INITIAL.
        lw_plantdata-assy_scrap = ls_werks-ausss.
        lw_plantdatax-assy_scrap = 'X'.
      ENDIF.
      " 外部采购仓储地点
      IF ls_marc-lgfsb <> ls_werks-lgfsb OR ls_marc IS INITIAL.
        lw_plantdata-sloc_exprc = ls_werks-lgfsb.
        lw_plantdatax-sloc_exprc = 'X'.
      ENDIF.
      " 准备时间
      IF ls_marc-ruezt <> ls_werks-ruezt OR ls_marc IS INITIAL.
        lw_plantdata-setuptime = ls_werks-ruezt.
        lw_plantdatax-setuptime = 'X'.
      ENDIF.
      " 收货处理时间
      IF ls_marc-webaz <> ls_werks-webaz OR ls_marc IS INITIAL.
        lw_plantdata-gr_pr_time = ls_werks-webaz.
        lw_plantdatax-gr_pr_time = 'X'.
      ENDIF.
      " 批量产品成本核算
      IF ls_marc-losgr <> ls_werks-losgr OR ls_marc IS INITIAL.
        lw_plantdata-lot_size = ls_werks-losgr.
        lw_plantdatax-lot_size = 'X'.
      ENDIF.
      " 凭证需求
      IF ls_marc-kzdkz IS INITIAL OR ls_marc IS INITIAL.
        lw_plantdata-doc_reqd = 'X'.
        lw_plantdatax-doc_reqd = 'X'.
      ENDIF.
      " 可用性检查
      IF ls_marc-mtvfp <> ls_werks-mtvfp OR ls_marc IS INITIAL.
        lw_plantdata-availcheck = ls_werks-mtvfp.
        lw_plantdatax-availcheck = 'X'.
      ENDIF.
      IF us_input-mtart = 'Z010'
        OR us_input-mtart = 'Z020'.
        " 采购中质量管理的控制码
        IF ls_marc-ssqss IS INITIAL OR ls_marc IS INITIAL.
          lw_plantdata-ctrl_key = 'ZQMCER'.
          lw_plantdatax-ctrl_key = 'X'.
        ENDIF.
        " 证书类型
        IF ls_marc-qzgtp IS INITIAL OR ls_marc IS INITIAL.
          lw_plantdata-cert_type = 'Z001'.
          lw_plantdatax-cert_type = 'X'.
        ENDIF.
      ENDIF.
*********************************Seashell Huang 20191219  质量管理视图添加“QM物料授权"
      lw_plantdata-qm_authgrp = 'ZQM001'.
      lw_plantdatax-qm_authgrp = 'X'.
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

      IF us_input-mtart = 'Z099'.
        IF ls_werks-dismm IS INITIAL AND ls_marc-dismm <> ls_werks-dismm.
          lw_plantdata-mrp_type = 'ND'.
          lw_plantdatax-mrp_type = 'X'.
          lw_plantdata-mrp_ctrler = ''.
          lw_plantdatax-mrp_ctrler = 'X'.
          lw_plantdata-lotsizekey = ''.
          lw_plantdatax-lotsizekey = 'X'.
        ENDIF.
        IF ls_werks-beskz IS INITIAL AND ls_marc-beskz <> 'F'.
          lw_plantdata-proc_type = 'F'.
          lw_plantdatax-proc_type = 'X'.
        ENDIF.
        IF ls_werks-sobsl IS INITIAL AND ls_marc-sobsl <> '50'.
          lw_plantdata-spproctype = '50'.
          lw_plantdatax-spproctype = 'X'.
        ENDIF.
        IF ls_werks-sbdkz IS INITIAL AND ls_marc-sbdkz <> '2'.
          lw_plantdata-dep_req_id = '2'.
          lw_plantdatax-dep_req_id = 'X'.
        ENDIF.
      ENDIF.

      CLEAR: lw_valuationdata, lw_valuationdatax.
      lw_valuationdata-val_area = ls_werks-werks.                       " 估价范围
      lw_valuationdatax-val_area = ls_werks-werks.
      " 评估分类
      IF ls_mbew-bklas <> ls_werks-bklas OR ls_mbew IS INITIAL.
        lw_valuationdata-val_class = ls_werks-bklas.
        lw_valuationdatax-val_class = 'X'.
      ENDIF.
      " 上期的评价分类
      IF ls_mbew-vmbkl <> ls_werks-bklas OR ls_mbew IS INITIAL.
        lw_valuationdata-vclass_pp = ls_werks-bklas.
        lw_valuationdatax-vclass_pp = 'X'.
      ENDIF.
      " 上年的评价分类
      IF ls_mbew-vjbkl <> ls_werks-bklas OR ls_mbew IS INITIAL.
        lw_valuationdata-vclass_py = ls_werks-bklas.
        lw_valuationdatax-vclass_py = 'X'.
      ENDIF.
      " 价格控制
      IF ls_mbew-vprsv <> ls_werks-vprsv_1 OR ls_mbew IS INITIAL.
        lw_valuationdata-price_ctrl = ls_werks-vprsv_1.
        lw_valuationdatax-price_ctrl = 'X'.
      ENDIF.
      " 价格控制上一期间
      IF ls_mbew-vmvpr <> ls_werks-vprsv_1 OR ls_mbew IS INITIAL.
        lw_valuationdata-pr_ctrl_py = ls_werks-vprsv_1.
        lw_valuationdatax-pr_ctrl_py = 'X'.
      ENDIF.
      " 价格控制上一年度
      IF ls_mbew-vjvpr <> ls_werks-vprsv_1 OR ls_mbew IS INITIAL.
        lw_valuationdata-pr_ctrl_pp = ls_werks-vprsv_1.
        lw_valuationdatax-pr_ctrl_pp = 'X'.
      ENDIF.
      " 价格单位
      IF ls_mbew-peinh <> ls_werks-peinh_1 OR ls_mbew IS INITIAL.
        lw_valuationdata-price_unit = ls_werks-peinh_1.
        lw_valuationdatax-price_unit = 'X'.
      ENDIF.
      " 计划价格1
      IF ls_mbew-zplp1 <> ls_werks-zplp1 OR ls_mbew IS INITIAL.
        lw_valuationdata-plndprice1 = ls_werks-zplp1.
        lw_valuationdatax-plndprice1 = 'X'.
      ENDIF.
      " 计划价格日期1
      IF ls_mbew-zpld1 <> ls_werks-zpld1 OR ls_mbew IS INITIAL.
        lw_valuationdata-plndprdate1 = ls_werks-zpld1.
        lw_valuationdatax-plndprdate1 = 'X'.
      ENDIF.
      " 计划价格2
      IF ls_mbew-zplp2 <> ls_werks-zplp2 OR ls_mbew IS INITIAL.
        lw_valuationdata-plndprice2 = ls_werks-zplp2.
        lw_valuationdatax-plndprice2 = 'X'.
      ENDIF.
      " 计划价格日期2
      IF ls_mbew-zpld2 <> ls_werks-zpld2 OR ls_mbew IS INITIAL.
        lw_valuationdata-plndprdate2 = ls_werks-zpld2.
        lw_valuationdatax-plndprdate2 = 'X'.
      ENDIF.
      " 计划价格3
      IF ls_mbew-zplp3 <> ls_werks-zplp3 OR ls_mbew IS INITIAL.
        lw_valuationdata-plndprice3 = ls_werks-zplp3.
        lw_valuationdatax-plndprice3 = 'X'.
      ENDIF.
      " 计划价格日期3
      IF ls_mbew-zpld3 <> ls_werks-zpld3 OR ls_mbew IS INITIAL.
        lw_valuationdata-plndprdate3 = ls_werks-zpld3.
        lw_valuationdatax-plndprdate3 = 'X'.
      ENDIF.
      " 用QS的成本估算
      IF ls_mbew-ekalr <> ls_werks-ekalr OR ls_mbew IS INITIAL.
        lw_valuationdata-qty_struct = ls_werks-ekalr.
        lw_valuationdatax-qty_struct = 'X'.
      ENDIF.
      " 物料来源
      IF ls_mbew-hkmat IS INITIAL OR ls_mbew IS INITIAL.
        lw_valuationdata-orig_mat = 'X'.
        lw_valuationdatax-orig_mat = 'X'.
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

      IF ls_mbew IS NOT INITIAL .
        lw_plantdata-profit_ctr = ''.     "利润中心
        lw_plantdatax-profit_ctr = ''.
        lw_valuationdata-price_ctrl = ''.      "价格控制
        lw_valuationdatax-price_ctrl = ''.
        lw_valuationdata-pr_ctrl_py = ''.
        lw_valuationdatax-pr_ctrl_py = ''.
        lw_valuationdata-pr_ctrl_pp = ''.
        lw_valuationdatax-pr_ctrl_pp = ''.
        lw_valuationdata-ml_settle = ''.       "价格确定
        lw_valuationdatax-ml_settle = ''.
        lw_valuationdata-val_class = '' .      "评估类
        lw_valuationdatax-val_class = '' .
        lw_valuationdata-vclass_pp = '' .
        lw_valuationdatax-vclass_pp = '' .
        lw_valuationdata-vclass_py = '' .
        lw_valuationdatax-vclass_py = '' .
      ELSE.
        IF lw_valuationdata-price_ctrl = 'S'.
          lw_valuationdata-ml_settle = '3'.
          lw_valuationdatax-price_ctrl = 'X'.
          lw_valuationdatax-ml_settle = 'X'.
        ENDIF.
        IF lw_valuationdata-price_ctrl = 'V' .
          lw_valuationdata-ml_settle = '2'.
          lw_valuationdatax-price_ctrl = 'X'.
          lw_valuationdatax-ml_settle = 'X'.
        ENDIF.
      ENDIF.

      IF ls_marc IS NOT INITIAL.
        lw_plantdata-batch_mgmt = ''.     " 批次管理
        lw_plantdatax-batch_mgmt = ''.
      ENDIF.

      CLEAR:ls_mbew,ls_marc.

***********************end add***********************************************

      " 创建采购，MRP，质量，会计，成本视图
      CLEAR: lw_return.
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata       = lw_headdata
          clientdata     = lw_clientdata
          clientdatax    = lw_clientdatax
          plantdata      = lw_plantdata
          plantdatax     = lw_plantdatax
          valuationdata  = lw_valuationdata
          valuationdatax = lw_valuationdatax
        IMPORTING
          return         = lw_return.
      IF lw_return-type = 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        IF lv_mess IS INITIAL.
          CONCATENATE ls_werks-werks '-' lw_return-message INTO lv_mess.
        ELSE.
          CONCATENATE lv_mess '|' ls_werks-werks '-' lw_return-message INTO lv_mess.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        " 视图创建成功时，创建质检视图
        IF us_input-mtart <> 'Z040'
          AND us_input-mtart <> 'Z099'.
          CLEAR: lt_qmat, ls_qmat, lt_inspectionctrl, lw_inspectionctrl.
          lt_qmat = ls_werks-qmat.
          LOOP AT lt_qmat INTO ls_qmat.
            CLEAR: lw_tq34.
            READ TABLE lt_tq34 INTO lw_tq34 WITH KEY art = ls_qmat-art.
            IF sy-subrc = 0.
              CLEAR: lv_matnr.
              lv_matnr = uv_matnr.
              lw_inspectionctrl-insptype = ls_qmat-art.                     " 检验类型
              lw_inspectionctrl-material = lv_matnr.                        " 物料编号（18 个字符）
              lw_inspectionctrl-plant = ls_werks-werks.                     " 工厂
              lw_inspectionctrl-ave_insp_duration = ls_qmat-mpdau.          " 平均检验期
              lw_inspectionctrl-preferred_insptype = ls_qmat-qapa.          " 首选检验类型
              lw_inspectionctrl-ind_insptype_mat_active = 'X'.              " 检验类型 － 物料合并已激活
              lw_inspectionctrl-ind_insp_with_tsk_list = lw_tq34-ppl.       " 有任务清单的检验
              lw_inspectionctrl-ind_spec_matspec = lw_tq34-spezueber.       " 使用物料说明的检验
              lw_inspectionctrl-ind_spec_config = lw_tq34-conf.             " 来自配置的检验规范
              lw_inspectionctrl-ind_spec_batch = lw_tq34-tls.               " 批次确定的检验规范
              lw_inspectionctrl-ind_auto_assign = lw_tq34-app.              " 自动规格分配
              lw_inspectionctrl-ind_insp_by_charac = lw_tq34-mer.           " 按特征检验
              lw_inspectionctrl-ind_post_to_insp_stock = lw_tq34-insmk.     " 过帐到检验库存
              lw_inspectionctrl-ind_automatic_ud = lw_tq34-ave.             " 计划的自动用途决定
              lw_inspectionctrl-sampling_procedure = lw_tq34-stichprver.    " 采样程序
              lw_inspectionctrl-dyn_modif_rule = lw_tq34-dynregel.          " 动态修改规则
              lw_inspectionctrl-insp_percentage = lw_tq34-sproz.            " 检验百分比
              lw_inspectionctrl-ind_100_percent_inspection = lw_tq34-hpz.   " 100% 检验
              lw_inspectionctrl-ind_skips_allowed = lw_tq34-dyn.            " 允许略过
              lw_inspectionctrl-ind_manual_sample = lw_tq34-mpb.            " 手工输入采样
              lw_inspectionctrl-ind_manual_sample_calc = lw_tq34-mst.       " 手动触发采样计算
              lw_inspectionctrl-ind_single_units_possible = lw_tq34-ein.    " 可能的序列号管理
              lw_inspectionctrl-contr_insp_lot_create = lw_tq34-chg.        " 控制检验批创建（批次摘要）
              lw_inspectionctrl-qual_score_procedure = lw_tq34-qkzverf.     " 计算质量记分的程序
              lw_inspectionctrl-allowed_scrap_share = lw_tq34-qpmat.        " 检验批中允许的废品份额（百分比）
              lw_inspectionctrl-ind_hu_inspection = lw_tq34-afr.            " 处理单位的检验
              lw_inspectionctrl-ms_flag = lw_tq34-ms_flag.                  " 标识：多个规范
              APPEND lw_inspectionctrl TO lt_inspectionctrl.
              CLEAR: lw_inspectionctrl.
            ENDIF.
          ENDLOOP.
          CLEAR: lt_return, lv_mess1.
          CALL FUNCTION 'BAPI_MATINSPCTRL_SAVEREPLICA'
            TABLES
              return         = lt_return
              inspectionctrl = lt_inspectionctrl.
          LOOP AT lt_return INTO lw_return WHERE type = 'E'.
            IF lv_mess1 IS INITIAL.
              CONCATENATE ls_werks-werks '-' lw_return-message INTO lv_mess1.
            ELSE.
              CONCATENATE lv_mess '/' lw_return-message INTO lv_mess1.
            ENDIF.
          ENDLOOP.
          IF sy-subrc = 0.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            IF lv_mess IS INITIAL.
              lv_mess = lv_mess1.
            ELSE.
              CONCATENATE lv_mess '|' lv_mess1 INTO lv_mess.
            ENDIF.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ENDIF.
        ENDIF.
        " 对于物料类型Z010、Z020、Z030、Z040、Z050的物料，在处理工厂8100时同时更新工厂8199，处理工厂8200时同时更新8299, 工厂8199和8299的利润中心都赋值为DUMMY
        IF us_input-mtart = 'Z010'
          OR us_input-mtart = 'Z020'
          OR us_input-mtart = 'Z030'
          OR us_input-mtart = 'Z040'
          OR us_input-mtart = 'Z050'.
          CLEAR: lw_headdata, lw_clientdata, lw_clientdata, lw_plantdata, lw_plantdatax.
          lw_headdata-material = uv_matnr.                                    " 物料号
          lw_headdata-matl_type = us_input-mtart.                             " 物料类型
          lw_headdata-ind_sector = 'P'.                                       " 行业领域
          lw_headdata-storage_view = 'X'.                                     " 库存视图
          lw_clientdata-temp_conds = ls_werks-tempb.                          " 温度条件
          lw_clientdata-stor_conds = ls_werks-raube.                          " 存储条件
          lw_clientdata-minremlife = ls_werks-mhdrz.                          " 最小剩余货架寿命
          lw_clientdata-shelf_life = ls_werks-mhdhb.                          " 总货架寿命
          lw_clientdata-period_ind_expiration_date = ls_werks-iprkz.          " 期间标识
          lw_clientdata-round_up_rule_expiration_date = ls_werks-rdmhd.       " 舍入规则
          lw_clientdatax-temp_conds = 'X'.
          lw_clientdatax-stor_conds = 'X'.
          lw_clientdatax-minremlife = 'X'.
          lw_clientdatax-shelf_life = 'X'.
          lw_clientdatax-period_ind_expiration_date = 'X'.
          lw_clientdatax-round_up_rule_expiration_date = 'X'.
          IF us_input-mtart = 'Z010' AND ls_werks-sobsl IS INITIAL.
            lw_clientdata-manuf_prof = 'IMPN'.
            lw_clientdatax-manuf_prof = 'X'.
          ENDIF.
          lw_plantdata-del_flag = ls_werks-lvoma.                             " 删除标记
          lw_plantdatax-del_flag = 'X'.

          IF ls_werks-werks = '8100'.
            lw_plantdata-plant = '8199'.
            lw_plantdatax-plant = '8199'.
            lw_plantdata-profit_ctr = 'DUMMY'.
            lw_plantdatax-profit_ctr = 'X'.
            lw_valuationdata-val_area = '8199'.
            lw_valuationdatax-val_area = '8199'.
          ELSEIF ls_werks-werks = '8200'.
            lw_plantdata-plant = '8299'.
            lw_plantdatax-plant = '8299'.
            lw_plantdata-profit_ctr = 'DUMMY'.
            lw_plantdatax-profit_ctr = 'X'.
            lw_valuationdata-val_area = '8299'.
            lw_valuationdatax-val_area = '8299'.
          ENDIF.

**********************add by sujx 20191130***********************************************
          SELECT SINGLE * INTO ls_mbew
                  FROM mbew
                 WHERE matnr = uv_matnr
                   AND bwkey =  lw_plantdata-plant.

          SELECT SINGLE * INTO ls_marc
            FROM marc
           WHERE matnr = uv_matnr
             AND werks =  lw_plantdata-plant.

          IF ls_marc IS NOT INITIAL.
            lw_plantdata-batch_mgmt = ''.     " 批次管理
            lw_plantdatax-batch_mgmt = ''.
          ENDIF.

          IF ls_mbew IS NOT INITIAL.
            lw_plantdata-profit_ctr = ''.     "利润中心
            lw_plantdatax-profit_ctr = ''.
          ENDIF.
          CLEAR: ls_marc,ls_mbew.
********************** end add ************************************************

        ENDIF.
        CLEAR: lw_return.
        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
          EXPORTING
            headdata    = lw_headdata
            clientdata  = lw_clientdata
            clientdatax = lw_clientdatax
            plantdata   = lw_plantdata
            plantdatax  = lw_plantdatax
          IMPORTING
            return      = lw_return.
        IF lw_return-type = 'E'.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          IF lv_mess IS INITIAL.
            CONCATENATE lw_plantdata-plant '-' lw_return-message INTO lv_mess.
          ELSE.
            CONCATENATE lv_mess '|' lw_plantdata-plant '-' lw_return-message INTO lv_mess.
          ENDIF.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ENDIF.
      ENDIF.
    ELSE.
      lv_mess = '工厂不能为空'.
    ENDIF.
    CLEAR ls_mbew.
  ENDLOOP.
  IF lv_mess IS NOT INITIAL.
    cs_return-msg_matnr = uv_matnr.
    cs_return-msg_type = 'E'.
    cs_return-message = lv_mess.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SALESORG_VIEW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> US_INPUT
*&---------------------------------------------------------------------*
FORM frm_salesorg_view  USING    us_input  TYPE zdt_oa2sap_mdm_mara
                                 uv_matnr  TYPE matnr
                        CHANGING cs_return TYPE zdt_oa2sap_mdm_ret_zmd001.
  DEFINE matn1_input.
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
  DEFINE cunit_input.
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
  DATA: lt_vkorg TYPE zdt_oa2sap_mdm_mara_vkorg_tab,
        ls_vkorg TYPE zdt_oa2sap_mdm_mara_vkorg,
        lv_pmatn TYPE matnr,
        lv_vrkme TYPE vrkme,
        ls_mvke  TYPE mvke,
        ls_marc  TYPE marc,
        ls_mara  TYPE mara,
        lv_mess  TYPE c LENGTH 1000.
  DATA: lw_headdata           TYPE bapimathead,
        lw_clientdata         TYPE bapi_mara,
        lw_clientdatax        TYPE bapi_marax,
        lw_salesdata          TYPE bapi_mvke,
        lw_salesdatax         TYPE bapi_mvkex,
        lw_plantdata          TYPE bapi_marc,
        lw_plantdatax         TYPE bapi_marcx,
        lt_taxclassifications TYPE STANDARD TABLE OF bapi_mlan,
        lw_taxclassifications TYPE bapi_mlan,
        lt_return             TYPE STANDARD TABLE OF bapiret2,
        lw_return             TYPE bapiret2.

  CLEAR: lt_vkorg, ls_vkorg, lv_mess.
  lt_vkorg = us_input-vkorg.
  lw_headdata-material = uv_matnr.                                    " 物料号
  lw_headdata-matl_type = us_input-mtart.                             " 物料类型
  lw_headdata-ind_sector = 'P'.                                       " 行业领域
  lw_headdata-sales_view = 'X'.                                       " 销售视图
  lw_headdata-purchase_view = 'X'.                                    " 采购视图

  LOOP AT lt_vkorg INTO ls_vkorg.
    IF ls_vkorg-vkorg IS INITIAL OR ls_vkorg-vtweg IS INITIAL.
    ELSE.
      CLEAR: lv_pmatn, lv_vrkme, ls_marc, ls_mvke, ls_mara.

      SELECT SINGLE * FROM mara INTO ls_mara
        WHERE matnr = uv_matnr.
      IF sy-subrc <> 0.
        CLEAR: ls_mara.
      ENDIF.

      SELECT SINGLE * FROM marc INTO ls_marc
        WHERE matnr = uv_matnr AND werks = ls_vkorg-vkorg.
      IF sy-subrc <> 0.
        CLEAR: ls_marc.
      ENDIF.

      SELECT SINGLE * FROM mvke INTO ls_mvke
        WHERE matnr = uv_matnr AND vkorg = ls_vkorg-vkorg AND vtweg = ls_vkorg-vtweg.
      IF sy-subrc <> 0.
        CLEAR: ls_mvke.
      ENDIF.


      lv_pmatn = ls_vkorg-pmatn.
      matn1_input: lv_pmatn.
      cunit_input: ls_vkorg-vrkme lv_vrkme.
      " 产品组
      IF ls_vkorg-spart <> ls_mara-spart OR ls_mara IS INITIAL.
        lw_clientdata-division = ls_vkorg-spart.
        lw_clientdatax-division = 'X'.
      ENDIF.
      " 运输组
      IF ls_vkorg-tragr <> ls_mara-tragr OR ls_mara IS INITIAL.
        lw_clientdata-trans_grp = ls_vkorg-tragr.
        lw_clientdatax-trans_grp = 'X'.
      ENDIF.
*      lw_clientdata-division = ls_vkorg-spart.                          " 产品组
*      lw_clientdata-trans_grp = ls_vkorg-tragr.                         " 运输组
*      lw_clientdatax-division = 'X'.
*      lw_clientdatax-trans_grp = 'X'.

      lw_plantdata-plant = ls_vkorg-vkorg.                              " 工厂
      lw_plantdatax-plant = ls_vkorg-vkorg.
      " 可用性检查
      IF ls_vkorg-mtvfp <> ls_marc-mtvfp OR ls_marc IS INITIAL.
        lw_plantdata-availcheck = ls_vkorg-mtvfp.
        lw_plantdatax-availcheck = 'X'.
      ENDIF.
      " 装载组
      IF ls_vkorg-ladgr <> ls_marc-ladgr OR ls_marc IS INITIAL.
        lw_plantdata-loadinggrp = ls_vkorg-ladgr.
        lw_plantdatax-loadinggrp = 'X'.
      ENDIF.
*      lw_plantdata-availcheck = ls_vkorg-mtvfp.                         " 可用性检查
*      lw_plantdata-loadinggrp = ls_vkorg-ladgr.                         " 装载组
*      IF lw_plantdata-availcheck IS NOT INITIAL.
*        lw_plantdatax-availcheck = 'X'.
*      ENDIF.
*      IF lw_plantdata-loadinggrp IS NOT INITIAL.
*        lw_plantdatax-loadinggrp = 'X'.
*      ENDIF.

      lw_salesdata-sales_org = ls_vkorg-vkorg.                          " 销售组织
      lw_salesdata-distr_chan = ls_vkorg-vtweg.                         " 分销渠道
      lw_salesdatax-sales_org = ls_vkorg-vkorg.
      lw_salesdatax-distr_chan = ls_vkorg-vtweg.
      " 交货工厂
      IF ls_vkorg-delyg_plnt <> ls_mvke-dwerk OR ls_mvke IS INITIAL.
        lw_salesdata-delyg_plnt = ls_vkorg-delyg_plnt.
        lw_salesdatax-delyg_plnt = 'X'.
      ENDIF.
      " 科目设置组
      IF ls_vkorg-ktgrm <> ls_mvke-ktgrm OR ls_mvke IS INITIAL.
        lw_salesdata-acct_assgt = ls_vkorg-ktgrm.
        lw_salesdatax-acct_assgt = 'X'.
      ENDIF.
      " 项目类别组
      IF ls_vkorg-mtpos <> ls_mvke-mtpos OR ls_mvke IS INITIAL.
        lw_salesdata-item_cat = ls_vkorg-mtpos.
        lw_salesdatax-item_cat = 'X'.
      ENDIF.
      " 物料组1
      IF ls_vkorg-mvgr1 <> ls_mvke-mvgr1 OR ls_mvke IS INITIAL.
        lw_salesdata-matl_grp_1 = ls_vkorg-mvgr1.
        lw_salesdatax-matl_grp_1 = 'X'.
      ENDIF.
      " 物料组2
      IF ls_vkorg-mvgr2 <> ls_mvke-mvgr2 OR ls_mvke IS INITIAL.
        lw_salesdata-matl_grp_2 = ls_vkorg-mvgr2.
        lw_salesdatax-matl_grp_2 = 'X'.
      ENDIF.
      " 定价参考物料
      IF lv_pmatn <> ls_mvke-pmatn OR ls_mvke IS INITIAL.
        lw_salesdata-pr_ref_mat = lv_pmatn.
        lw_salesdatax-pr_ref_mat = 'X'.
      ENDIF.
      " 销售单位
      IF lv_vrkme <> ls_mvke-vrkme OR ls_mvke IS INITIAL.
        lw_salesdata-sales_unit = lv_vrkme.
        lw_salesdatax-sales_unit = 'X'.
      ENDIF.
      " 指定分销链的物料状态
      IF ls_vkorg-vmsta <> ls_mvke-vmsta OR ls_mvke IS INITIAL.
        lw_salesdata-sal_status = ls_vkorg-vmsta.
        lw_salesdatax-sal_status = 'X'.
        IF ls_vkorg-vmsta IS NOT INITIAL.
          lw_salesdata-valid_from = sy-datum.                             " 来自指定分销链的物料状态是有效的日期
        ELSE.
          lw_salesdata-valid_from = '00000000'.
        ENDIF.
        lw_salesdatax-valid_from = 'X'.
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

      CLEAR: lt_taxclassifications, lw_taxclassifications.
      lw_taxclassifications-depcountry = 'CN'.                          " 发货国(由该国家发货)
      lw_taxclassifications-tax_type_1 = 'MWST'.                        " 税类别(营业税，联邦营业税，...)
      lw_taxclassifications-taxclass_1 = ls_vkorg-taxm1.                " 物料的税分类
      APPEND lw_taxclassifications TO lt_taxclassifications.
      CLEAR: lw_return, lw_taxclassifications.
      " CREATE SALESORG VIEW
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata           = lw_headdata
          clientdata         = lw_clientdata
          clientdatax        = lw_clientdatax
          plantdata          = lw_plantdata
          plantdatax         = lw_plantdatax
          salesdata          = lw_salesdata
          salesdatax         = lw_salesdatax
        IMPORTING
          return             = lw_return
        TABLES
          taxclassifications = lt_taxclassifications.
      IF lw_return-type = 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        IF lv_mess IS INITIAL.
          CONCATENATE ls_vkorg-vkorg '-' lw_return-message INTO lv_mess.
        ELSE.
          CONCATENATE lv_mess '|' ls_vkorg-vkorg '-' lw_return-message INTO lv_mess.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF lv_mess IS NOT INITIAL.
    cs_return-msg_matnr = uv_matnr.
    cs_return-msg_type = 'E'.
    IF cs_return-message IS NOT INITIAL.
      CONCATENATE cs_return-message lv_mess INTO cs_return-message.
    ELSE.
      cs_return-message = lv_mess.
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
FORM frm_class_view  USING    us_input  TYPE zdt_oa2sap_mdm_mara
                              uv_matnr  TYPE matnr
                              uv_flag   TYPE c
                     CHANGING cs_return TYPE zdt_oa2sap_mdm_ret_zmd001.
  DATA: lt_class    TYPE zdt_oa2sap_mdm_mara_class_tab,
        ls_class    TYPE zdt_oa2sap_mdm_mara_class,
        lt_werks    TYPE zdt_oa2sap_mdm_mara_werks_tab,
        ls_werks    TYPE zdt_oa2sap_mdm_mara_werks,
        lv_classnum TYPE klasse_d,
        lv_mess     TYPE c LENGTH 1000.
  DATA: lw_alloclist TYPE bapi1003_alloc_list.
  CLEAR: lt_class, ls_class, lv_mess, lv_classnum, lt_werks, ls_werks.
  lt_class = us_input-class.
  lt_werks = us_input-werks.
  LOOP AT lt_class INTO ls_class.
    IF ls_class-classtype = '001'.
      IF ls_class-classname = us_input-matkl.
        " CHECK IF CLASS 001 EXIST
        CLEAR: lw_alloclist.
        PERFORM frm_get_class USING uv_matnr '001' CHANGING lw_alloclist.
        IF lw_alloclist IS INITIAL.
          lv_classnum = ls_class-classname.
          PERFORM frm_class_create USING uv_matnr '001' lv_classnum ls_class CHANGING cs_return.
        ELSE.
          PERFORM frm_class_change USING uv_matnr lw_alloclist ls_class CHANGING cs_return.
        ENDIF.
      ELSE.
        IF lv_mess IS INITIAL.
          CONCATENATE ls_class-classname '物料分类名称与物料组字段值不一致' INTO lv_mess.
        ELSE.
          CONCATENATE lv_mess '|' ls_class-classname '物料分类名称与物料组字段值不一致' INTO lv_mess.
        ENDIF.
      ENDIF.
    ELSE.
      " CHECK IF CLASS 022 EXIST
      CLEAR: lw_alloclist.
      PERFORM frm_get_class USING uv_matnr '022' CHANGING lw_alloclist.
      IF lw_alloclist IS INITIAL.
        " CHECK IF BATCH_MGMT IS TICKED
        LOOP AT lt_werks INTO ls_werks WHERE xchpf = 'X'.
        ENDLOOP.
        IF sy-subrc = 0.
          IF us_input-mtart = 'Z010'
            OR us_input-mtart = 'Z020'
            OR us_input-mtart = 'Z030'
            OR us_input-mtart = 'Z040'
            OR us_input-mtart = 'Z090'.
            lv_classnum = 'ZBATCH_SIRIO'.
          ELSEIF us_input-mtart = 'Z050'
            OR us_input-mtart = 'Z080'.
            lv_classnum = 'BATCH_CP'.
          ENDIF.
          PERFORM frm_class_create USING uv_matnr '022' lv_classnum ls_class CHANGING cs_return.
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
FORM frm_mrp_aera_create  USING    us_input  TYPE zdt_oa2sap_mdm_mara
                                   uv_matnr  TYPE matnr
                          CHANGING cs_return TYPE zdt_oa2sap_mdm_ret_zmd001.
  DATA: lt_mrp    TYPE zdt_oa2sap_mdm_mara_mrp_a_tab1,
        ls_mrp    TYPE zdt_oa2sap_mdm_mara_mrp_area,
        lt_detail TYPE zdt_oa2sap_mdm_mara_mrp_ar_tab,
        ls_detail TYPE zdt_oa2sap_mdm_mara_mrp_area_m,
        lv_berty  TYPE berty,
        lv_subrc  TYPE c,
        lv_flag   TYPE c,
        lv_mess   TYPE c LENGTH 1000.
  DATA: lv_matnr  TYPE matnr,
        lv_werks  TYPE werkdp,
        lv_berid  TYPE berid,
        lw_mdma   TYPE mdma,
        lw_dpop   TYPE dpop,
        lw_fields TYPE sdibe_massfields,
        lw_return TYPE bapireturn1.

  CLEAR: lt_mrp, ls_mrp, lv_mess.
  lt_mrp = us_input-mrp_aera.

  LOOP AT lt_mrp INTO ls_mrp.
    CLEAR: lv_matnr, lv_werks, lv_berid, lt_detail, ls_detail.
    lt_detail = ls_mrp-mrparea_detail.
    lv_matnr = uv_matnr.
    lv_berid = ls_mrp-berid.
    lv_werks = lv_berid+0(4).
    PERFORM frm_check_exist USING lv_matnr lv_werks CHANGING lv_subrc.
    IF lv_subrc IS INITIAL.
      LOOP AT lt_detail INTO ls_detail.
        CLEAR: lw_mdma, lw_fields, lw_dpop, lv_flag, lv_berty.
        lv_berty = ls_detail-zdismm.
        lw_mdma-mandt = sy-mandt.                     " 集团
        lw_mdma-matnr = lv_matnr.                     " 物料编号
        lw_mdma-berid = lv_berid.                     " MRP 范围
        lw_mdma-werks = lv_werks.                     " MRP 范围：工厂
        lw_mdma-dismm = ls_detail-zdismm.             " MRP类型
        lw_mdma-disgr = ls_detail-zdisgr.             " MRP组
        lw_mdma-dispo = ls_detail-zdispo.             " MRP控制者/物料需求计划控制员
        lw_mdma-disls = ls_detail-zdisls.             " 批量大小
        lw_mdma-sobsl = ls_detail-zsobsl.             " 特殊采购类
        lw_mdma-bstma = ls_detail-zbstma.             " 最大批量大小
        lw_mdma-bstmi = ls_detail-zbstmi.             " 最小批量大小
        lw_mdma-bstrf = ls_detail-zbstrf.             " 舍入值
        lw_mdma-bstfe = ls_detail-zbstfe.             " 固定批量
        lw_mdma-lgfsb = ls_detail-zlgfsb.             " 外部采购仓储地点

        lw_fields-xdismm = ls_detail-zdismm.          " MRP类型
        lw_fields-xdisgr = ls_detail-zdisgr.          " MRP组
        lw_fields-xdispo = ls_detail-zdispo.          " MRP控制者/物料需求计划控制员
        lw_fields-xdisls = ls_detail-zdisls.          " 批量大小
        lw_fields-xsobsl = ls_detail-zsobsl.          " 特殊采购类
        lw_fields-xbstma = ls_detail-zbstmi.          " 最大批量大小
        lw_fields-xbstmi = ls_detail-zbstma.          " 最小批量大小
        lw_fields-xbstrf = ls_detail-zbstrf.          " 舍入值
        lw_fields-xbstfe = ls_detail-zbstfe.          " 固定批量
        lw_fields-xlgfsb = ls_detail-zlgfsb.          " 外部采购仓储地点
        PERFORM frm_get_mrparea USING lv_matnr lv_werks lv_berid CHANGING lv_flag.
        IF lv_flag IS NOT INITIAL.
          CALL FUNCTION 'MD_MRP_LEVEL_CHANGE_DATA'
            EXPORTING
              i_matnr        = lv_matnr
              i_werk         = lv_werks
              i_mrp_area     = lv_berid
              i_berty        = lv_berty
              i_selfields    = lw_fields
              i_mdma         = lw_mdma
              i_dpop         = lw_dpop
            IMPORTING
              e_error_return = lw_return.
        ELSE.
          CLEAR: lw_return.
          CALL FUNCTION 'MD_MRP_LEVEL_CREATE_DATA'
            EXPORTING
              i_matnr        = lv_matnr
              i_werk         = lv_werks
              i_mrp_area     = lv_berid
              i_selfields    = lw_fields
              i_mdma         = lw_mdma
              i_dpop         = lw_dpop
            IMPORTING
              e_error_return = lw_return.
        ENDIF.
        IF lw_return-type = 'E'.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          IF lv_mess IS INITIAL.
            CONCATENATE lv_berid '-' lw_return-message INTO lv_mess.
          ELSE.
            CONCATENATE lv_mess '|' lv_berid '-' lw_return-message INTO lv_mess.
          ENDIF.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
  IF lv_mess IS NOT INITIAL.
    cs_return-msg_matnr = uv_matnr.
    cs_return-msg_type = 'E'.
    IF cs_return-message IS NOT INITIAL.
      CONCATENATE cs_return-message lv_mess INTO cs_return-message.
    ELSE.
      cs_return-message = lv_mess.
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
FORM frm_get_class  USING    uv_matnr     TYPE matnr
                             uv_classtype TYPE klassenart
                    CHANGING cs_alloclist TYPE bapi1003_alloc_list.
  DATA: lt_alloclist TYPE STANDARD TABLE OF bapi1003_alloc_list,
        lt_return    TYPE STANDARD TABLE OF bapiret2,
        lv_objectkey TYPE objnum.

  CLEAR: lv_objectkey, lt_alloclist, lt_return.
  lv_objectkey = uv_matnr.

  CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
    EXPORTING
      objectkey_imp   = lv_objectkey
      objecttable_imp = 'MARA'
      classtype_imp   = uv_classtype
    TABLES
      alloclist       = lt_alloclist
      return          = lt_return.
  LOOP AT lt_return INTO DATA(ls_return) WHERE type = 'E'.
  ENDLOOP.
  IF sy-subrc <> 0.
    READ TABLE lt_alloclist INTO DATA(ls_alloclist) INDEX 1.
    IF sy-subrc = 0.
      cs_alloclist = ls_alloclist.
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
FORM frm_class_create  USING    uv_matnr     TYPE matnr
                                uv_classtype TYPE klassenart
                                uv_classnum  TYPE klasse_d
                                us_class     TYPE zdt_oa2sap_mdm_mara_class
                       CHANGING cs_return    TYPE zdt_oa2sap_mdm_ret_zmd001.
  DATA: lv_objectkeynew    TYPE objnum,
        lt_allocvaluesnum  TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
        ls_allocvaluesnum  TYPE bapi1003_alloc_values_num,
        lt_allocvalueschar TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
        ls_allocvalueschar TYPE bapi1003_alloc_values_char,
        lt_allocvaluescurr TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
        ls_allocvaluescurr TYPE bapi1003_alloc_values_curr,
        lt_return          TYPE STANDARD TABLE OF bapiret2,
        ls_return          TYPE bapiret2.
  DATA: lt_classify TYPE zdt_oa2sap_mdm_mara_class__tab,
        ls_classify LIKE LINE OF lt_classify,
        lv_mess     TYPE c LENGTH 1000.

  CLEAR: lt_classify, ls_classify, lv_objectkeynew, lt_return, ls_return, lv_mess.
  lt_classify = us_class-classify.
  lv_objectkeynew = uv_matnr.
  IF uv_classtype = '001'.
    CLEAR: lt_allocvaluesnum, ls_allocvaluesnum, lt_allocvalueschar, ls_allocvalueschar, lt_allocvaluescurr, ls_allocvaluescurr.
    LOOP AT lt_classify INTO ls_classify.
      ls_allocvalueschar-charact = ls_classify-attrname.
      ls_allocvalueschar-charact_descr = ls_classify-attrname.
*      LS_ALLOCVALUESCHAR-VALUE_CHAR = LS_CLASSIFY-ATTRVALUE.
*      LS_ALLOCVALUESCHAR-VALUE_NEUTRAL = LS_CLASSIFY-ATTRVALUE.
      ls_allocvalueschar-value_char_long = ls_classify-attrvalue.
      ls_allocvalueschar-value_neutral_long = ls_classify-attrvalue.
      APPEND ls_allocvalueschar TO lt_allocvalueschar.
      CLEAR: ls_allocvalueschar.
    ENDLOOP.
    CALL FUNCTION 'BAPI_OBJCL_CREATE'
      EXPORTING
        objectkeynew    = lv_objectkeynew
        objecttablenew  = 'MARA'
        classnumnew     = uv_classnum
        classtypenew    = '001'
      TABLES
*       ALLOCVALUESNUM  =
        allocvalueschar = lt_allocvalueschar
*       ALLOCVALUESCURR =
        return          = lt_return.
    LOOP AT lt_return INTO ls_return WHERE type = 'E'.
      IF lv_mess IS INITIAL.
        CONCATENATE us_class-classtype us_class-classname '-' ls_return-message INTO lv_mess.
      ELSE.
        CONCATENATE lv_mess '|' us_class-classtype us_class-classname '-' ls_return-message INTO lv_mess.
      ENDIF.
    ENDLOOP.
  ELSE.
    CALL FUNCTION 'BAPI_OBJCL_CREATE'
      EXPORTING
        objectkeynew   = lv_objectkeynew
        objecttablenew = 'MARA'
        classnumnew    = uv_classnum
        classtypenew   = '022'
        standardclass  = 'X'
      TABLES
        return         = lt_return.
    LOOP AT lt_return INTO ls_return WHERE type = 'E'.
      IF lv_mess IS INITIAL.
        CONCATENATE us_class-classtype us_class-classname '-' ls_return-message INTO lv_mess.
      ELSE.
        CONCATENATE lv_mess '|' us_class-classtype us_class-classname '-' ls_return-message INTO lv_mess.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF lv_mess IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    cs_return-msg_matnr = uv_matnr.
    cs_return-msg_type = 'E'.
    IF cs_return-message IS NOT INITIAL.
      CONCATENATE cs_return-message lv_mess INTO cs_return-message.
    ELSE.
      cs_return-message = lv_mess.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
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
FORM frm_class_change  USING    uv_matnr     TYPE matnr
                                us_alloclist TYPE bapi1003_alloc_list
                                us_class     TYPE zdt_oa2sap_mdm_mara_class
                       CHANGING cs_return    TYPE zdt_oa2sap_mdm_ret_zmd001.
  DATA: lv_objectkeynew    TYPE objnum,
        lv_classnumnew     TYPE klasse_d,
        lt_allocvaluesnum  TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
        ls_allocvaluesnum  TYPE bapi1003_alloc_values_num,
        lt_allocvalueschar TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
        ls_allocvalueschar TYPE bapi1003_alloc_values_char,
        lt_allocvaluescurr TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
        ls_allocvaluescurr TYPE bapi1003_alloc_values_curr,
        ls_valueschar      TYPE bapi1003_alloc_values_char,
        lt_return          TYPE STANDARD TABLE OF bapiret2,
        ls_return          TYPE bapiret2.
  DATA: lt_classify TYPE zdt_oa2sap_mdm_mara_class__tab,
        ls_classify LIKE LINE OF lt_classify,
        lv_mess     TYPE c LENGTH 1000.

  CLEAR: lt_classify, ls_classify, lt_return, ls_return, lv_objectkeynew, lv_classnumnew, lv_mess.
  lt_classify = us_class-classify.
  lv_objectkeynew = uv_matnr.
  lv_classnumnew = us_alloclist-classnum.
  CLEAR: lt_allocvaluesnum, ls_allocvaluesnum, lt_allocvalueschar, ls_allocvalueschar, lt_allocvaluescurr, ls_allocvaluescurr.
  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      objectkey       = lv_objectkeynew
      objecttable     = 'MARA'
      classnum        = lv_classnumnew
      classtype       = '001'
    TABLES
      allocvaluesnum  = lt_allocvaluesnum
      allocvalueschar = lt_allocvalueschar
      allocvaluescurr = lt_allocvaluescurr
      return          = lt_return.
  LOOP AT lt_return INTO ls_return WHERE type = 'E'.
    IF lv_mess IS INITIAL.
      CONCATENATE us_class-classtype us_class-classname '-' ls_return-message INTO lv_mess.
    ELSE.
      CONCATENATE lv_mess '|' us_class-classtype us_class-classname '-' ls_return-message INTO lv_mess.
    ENDIF.
  ENDLOOP.
  IF sy-subrc <> 0.
    LOOP AT lt_allocvalueschar ASSIGNING FIELD-SYMBOL(<fs_char>).
      <fs_char>-value_char = ''.
      <fs_char>-value_neutral = ''.
      <fs_char>-value_char_long = ''.
      <fs_char>-value_neutral_long = ''.
    ENDLOOP.
*    CLEAR: LS_VALUESCHAR.
    LOOP AT lt_classify INTO ls_classify.
      READ TABLE lt_allocvalueschar ASSIGNING <fs_char>
        WITH KEY charact = ls_classify-attrname.
      IF sy-subrc = 0.
        <fs_char>-value_char = ''.
        <fs_char>-value_neutral = ''.
        <fs_char>-value_char_long = ls_classify-attrvalue.
        <fs_char>-value_neutral_long = ls_classify-attrvalue.
      ELSE.
        ls_allocvalueschar-charact = ls_classify-attrname.
        ls_allocvalueschar-charact_descr = ls_classify-attrname.
*        LS_ALLOCVALUESCHAR-VALUE_CHAR = LS_CLASSIFY-ATTRVALUE.
*        LS_ALLOCVALUESCHAR-VALUE_NEUTRAL = LS_CLASSIFY-ATTRVALUE.
        ls_allocvalueschar-value_char_long = ls_classify-attrvalue.
        ls_allocvalueschar-value_neutral_long = ls_classify-attrvalue.
        APPEND ls_allocvalueschar TO lt_allocvalueschar.
        CLEAR: ls_allocvalueschar.
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
    IF lt_allocvalueschar IS NOT INITIAL.
      CLEAR: lt_return, ls_return.
      CALL FUNCTION 'BAPI_OBJCL_CHANGE'
        EXPORTING
          objectkey          = lv_objectkeynew
          objecttable        = 'MARA'
          classnum           = lv_classnumnew
          classtype          = '001'
        TABLES
          allocvaluesnumnew  = lt_allocvaluesnum
          allocvaluescharnew = lt_allocvalueschar
          allocvaluescurrnew = lt_allocvaluescurr
          return             = lt_return.
      LOOP AT lt_return INTO ls_return WHERE type = 'E'.
        IF lv_mess IS INITIAL.
          CONCATENATE us_class-classtype us_class-classname '-' ls_return-message INTO lv_mess.
        ELSE.
          CONCATENATE lv_mess '|' us_class-classtype us_class-classname '-' ls_return-message INTO lv_mess.
        ENDIF.
      ENDLOOP.
      IF lv_mess IS NOT INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.
  IF lv_mess IS NOT INITIAL.
    cs_return-msg_matnr = uv_matnr.
    cs_return-msg_type = 'E'.
    IF cs_return-message IS NOT INITIAL.
      CONCATENATE cs_return-message lv_mess INTO cs_return-message.
    ELSE.
      cs_return-message = lv_mess.
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
FORM frm_change_matnr  USING    us_input  TYPE zdt_oa2sap_mdm_mara
                       CHANGING cs_return TYPE zdt_oa2sap_mdm_ret_zmd001.
  DEFINE matn1_input.
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
  DEFINE alpha_output.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input         = &1
      IMPORTING
        output        = &1.
  END-OF-DEFINITION.
  DEFINE alpha_input.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input         = &1
      IMPORTING
        output        = &1.
  END-OF-DEFINITION.
  DEFINE cunit_input.
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
  DATA: lw_headdata            TYPE bapimathead,
        lw_clientdata          TYPE bapi_mara,
        lw_clientdatax         TYPE bapi_marax,
        lw_return              TYPE bapiret2,
        lt_materiallongtext    TYPE STANDARD TABLE OF bapi_mltx,
        lw_materiallongtext    TYPE bapi_mltx,
        lt_unitsofmeasure      TYPE STANDARD TABLE OF bapi_marm,
        lw_unitsofmeasure      TYPE bapi_marm,
        lt_unitsofmeasurex     TYPE STANDARD TABLE OF bapi_marmx,
        lw_unitsofmeasurex     TYPE bapi_marmx,
        lt_materialdescription TYPE STANDARD TABLE OF bapi_makt,
        lw_materialdescription TYPE bapi_makt.

  DATA: lt_smeinh    TYPE zdt_oa2sap_mdm_mara_smeinh_tab,
        ls_smeinh    TYPE zdt_oa2sap_mdm_mara_smeinh,
        lv_matnr     TYPE matnr,
        lv_matnr_t   TYPE matnr,
        lv_matnr_mpn TYPE bmatn,
        lv_mfrnr     TYPE mfrnr,
        lv_old_no    TYPE bismt,
        lv_bismt     TYPE c LENGTH 500,
        lv_maktxl    TYPE c LENGTH 500,
        lv_length    TYPE i,
        lv_meins     TYPE meins,
        lv_meinh     TYPE meins,
        lv_flag      TYPE c.

  CLEAR: lv_matnr, lv_flag, lv_maktxl, lv_length, lv_mfrnr, lv_meins, lv_bismt, lv_old_no.
  lv_matnr = us_input-matnr.
  matn1_input: lv_matnr.
  lv_mfrnr = us_input-mfrnr.
  alpha_input: lv_mfrnr.
  lv_maktxl = us_input-maktx.
  lv_length = strlen( lv_maktxl ).
  cunit_input: us_input-meins lv_meins.
  lv_bismt = us_input-bismt.
  CONDENSE lv_bismt NO-GAPS.
  IF lv_length > 40.
    cs_return-msg_matnr = lv_matnr.
    cs_return-msg_type = 'E'.
    CONCATENATE lv_maktxl '物料描述过长' INTO cs_return-message.
  ELSE.

    SELECT matnr, meinh, umrez, umren
      FROM marm
      INTO TABLE @DATA(lt_marm)
      WHERE matnr = @lv_matnr.

    IF us_input-mtart <> 'HERS'.
      CLEAR: lv_length.
      lv_length = strlen( lv_bismt ).
      IF lv_length > 40.
        lv_old_no = lv_bismt+0(39) && '*'.
*        CLEAR: lt_materiallongtext, lw_materiallongtext.
*        lw_materiallongtext-applobject = 'MATERIAL'.
*        lw_materiallongtext-text_name = lv_matnr.
*        lw_materiallongtext-text_id = 'IVER'.
*        lw_materiallongtext-langu = '1'.
*        lw_materiallongtext-format_col = 1.
*        lw_materiallongtext-text_line = lv_bismt+0(132).
*        APPEND lw_materiallongtext TO lt_materiallongtext.
      ELSE.
        lv_old_no = lv_bismt.
      ENDIF.
      CLEAR: lt_materiallongtext, lw_materiallongtext.
      lw_materiallongtext-applobject = 'MATERIAL'.
      lw_materiallongtext-text_name = lv_matnr.
      lw_materiallongtext-text_id = 'IVER'.
      lw_materiallongtext-langu = '1'.
      lw_materiallongtext-format_col = 1.
      lw_materiallongtext-text_line = lv_bismt+0(132).
      APPEND lw_materiallongtext TO lt_materiallongtext.
      " 创建物料基本视图
      SELECT SINGLE * INTO @DATA(ls_mara)
        FROM mara
        WHERE matnr = @lv_matnr.
      CLEAR: lw_headdata, lw_clientdata, lw_clientdatax.
      lw_headdata-material = lv_matnr.                        " 物料号
      lw_headdata-matl_type = us_input-mtart.                 " 物料类型
      lw_headdata-ind_sector = 'P'.                           " 行业领域
      lw_headdata-basic_view = 'X'.                           " 基本数据视图
      lw_headdata-purchase_view = 'X'.                        " 采购视图
      IF us_input-matkl <> ls_mara-matkl OR ls_mara IS INITIAL.
        lw_clientdata-matl_group = us_input-matkl.              " 物料组
        lw_clientdatax-matl_group = 'X'.
      ENDIF.
      IF lv_old_no <> ls_mara-bismt OR ls_mara IS INITIAL.
        lw_clientdata-old_mat_no_long = lv_old_no.              " 旧物料号
        lw_clientdatax-old_mat_no_long = 'X'.
      ENDIF.
      IF ls_mara IS INITIAL.
        lw_clientdata-base_uom = lv_meins.                      " 基本计量单位
        lw_clientdatax-base_uom = 'X'.
      ELSE.
        lw_clientdata-base_uom = ''.
        lw_clientdatax-base_uom = ''.
      ENDIF.
      IF us_input-groes <> ls_mara-groes OR ls_mara IS INITIAL.
        lw_clientdata-size_dim = us_input-groes.                " 规格型号（大小/量纲）
        lw_clientdatax-size_dim = 'X'.
      ENDIF.
      IF lv_mfrnr <> ls_mara-mfrnr OR ls_mara IS INITIAL.
        lw_clientdata-mfr_no = lv_mfrnr.                        " 制造商
        lw_clientdatax-mfr_no = 'X'.
      ENDIF.
      IF us_input-ekwsl <> ls_mara-ekwsl OR ls_mara IS INITIAL.
        lw_clientdata-pur_valkey = us_input-ekwsl.              " 采购价值代码
        lw_clientdatax-pur_valkey = 'X'.
      ENDIF.

*      SELECT SINGLE * INTO @DATA(ls_mara)
*         FROM mara
*         WHERE matnr = @lv_matnr.
*     IF sy-subrc = 0.
*      lw_clientdatax-old_mat_no_long = ''.
*      lw_clientdata-base_uom = ''.                      " 基本计量单位
*     ENDIF.

      IF us_input-mprof <> ls_mara-mprof OR ls_mara IS INITIAL.
        lw_clientdata-manuf_prof = us_input-mprof.            " 制造商参数文件
        lw_clientdatax-manuf_prof = 'X'.
      ENDIF.

*      IF lv_mfrnr IS INITIAL AND us_input-mtart = 'Z010'.
*        lw_clientdata-manuf_prof = 'IMPN'.                    " 制造商参数文件
*        lw_clientdatax-manuf_prof = 'X'.
*      ENDIF.

      IF us_input-lvoma IS NOT INITIAL.
        lw_clientdata-del_flag = 'X'.                         " 在客户级标记要删除的物料
        lw_clientdatax-del_flag = 'X'.
      ELSE.
        lw_clientdata-del_flag = ''.                          " 在客户级标记要删除的物料
        lw_clientdatax-del_flag = 'X'.
      ENDIF.
      " 物料描述
      CLEAR: lt_materialdescription, lw_materialdescription.
      lw_materialdescription-langu = '1'.
      lw_materialdescription-matl_desc = us_input-maktx.      " 物料描述
      APPEND lw_materialdescription TO lt_materialdescription.
      " 物料描述长文本
      IF us_input-maktxl IS NOT INITIAL.
        CLEAR: lw_materiallongtext, lv_maktxl.
        lv_maktxl = us_input-maktxl.
        lw_materiallongtext-applobject = 'MATERIAL'.
        lw_materiallongtext-text_name = lv_matnr.
        lw_materiallongtext-text_id = 'GRUN'.
        lw_materiallongtext-langu = '1'.
        lw_materiallongtext-format_col = 1.
        lw_materiallongtext-text_line = lv_maktxl+0(132).
        APPEND lw_materiallongtext TO lt_materiallongtext.
        CLEAR: lw_materiallongtext.
        lw_materiallongtext-applobject = 'MATERIAL'.
        lw_materiallongtext-text_name = lv_matnr.
        lw_materiallongtext-text_id = 'GRUN'.
        lw_materiallongtext-langu = '1'.
        lw_materiallongtext-format_col = 2.
        lw_materiallongtext-text_line = lv_maktxl+132(132).
        APPEND lw_materiallongtext TO lt_materiallongtext.
      ENDIF.
      " 换算单位
      CLEAR: lt_unitsofmeasure, lw_unitsofmeasure, lt_unitsofmeasurex, lw_unitsofmeasurex, lt_smeinh, ls_smeinh.

*      IF ls_mara IS INITIAL .
      lt_smeinh = us_input-smeinh.
      LOOP AT lt_smeinh INTO ls_smeinh.
        CLEAR: lv_meinh.
        cunit_input: ls_smeinh-meinh lv_meinh.
        IF lv_meinh <> ls_mara-meins.
          lw_unitsofmeasure-gross_wt = us_input-brgew.          " 毛重
          lw_unitsofmeasurex-gross_wt = 'X'.
          lw_unitsofmeasure-volume = us_input-volum.            " 体积
          lw_unitsofmeasurex-volume = 'X'.
          IF us_input-gewei IS NOT INITIAL.
            lw_unitsofmeasure-unit_of_wt = us_input-gewei.      " 重量单位
          ELSE.
            lw_unitsofmeasure-unit_of_wt = 'KG'.
          ENDIF.
          lw_unitsofmeasurex-unit_of_wt = 'X'.
          lw_unitsofmeasure-volumeunit = us_input-voleh.        " 体积单位
          lw_unitsofmeasurex-volumeunit = 'X'.
          lw_unitsofmeasure-denominatr = ls_smeinh-umren.       " 转换为基本计量单位的分母
          lw_unitsofmeasurex-denominatr = 'X'.
          lw_unitsofmeasure-alt_unit = lv_meinh.                " 用于显示的计量单位
          lw_unitsofmeasurex-alt_unit = lv_meinh.
          lw_unitsofmeasure-numerator = ls_smeinh-umrez.        " 基本计量单位转换分子
          lw_unitsofmeasurex-numerator = 'X'.
          APPEND lw_unitsofmeasure TO lt_unitsofmeasure.
          APPEND lw_unitsofmeasurex TO lt_unitsofmeasurex.
          CLEAR: lw_unitsofmeasure, lw_unitsofmeasurex.
        ENDIF.
      ENDLOOP.
      LOOP AT lt_marm INTO DATA(ls_marm).
        READ TABLE lt_unitsofmeasure TRANSPORTING NO FIELDS WITH KEY alt_unit = ls_marm-meinh.
        IF sy-subrc <> 0.
          IF ls_marm-meinh <> ls_mara-meins.
            lw_unitsofmeasure-denominatr = ls_smeinh-umren.       " 转换为基本计量单位的分母
            lw_unitsofmeasurex-denominatr = 'X'.
            lw_unitsofmeasure-alt_unit = ls_marm-meinh.           " 用于显示的计量单位
            lw_unitsofmeasurex-alt_unit = ls_marm-meinh.
            lw_unitsofmeasure-numerator = ls_smeinh-umrez.        " 基本计量单位转换分子
            lw_unitsofmeasurex-numerator = 'X'.
            lw_unitsofmeasure-del_flag = 'X'.                     " 删除标记
            APPEND lw_unitsofmeasure TO lt_unitsofmeasure.
            APPEND lw_unitsofmeasurex TO lt_unitsofmeasurex.
            CLEAR: lw_unitsofmeasure, lw_unitsofmeasurex.
          ENDIF.
        ENDIF.
      ENDLOOP.
*      ENDIF.
      CLEAR: lw_return.
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata            = lw_headdata
          clientdata          = lw_clientdata
          clientdatax         = lw_clientdatax
        IMPORTING
          return              = lw_return
        TABLES
          materialdescription = lt_materialdescription
          materiallongtext    = lt_materiallongtext
          unitsofmeasure      = lt_unitsofmeasure
          unitsofmeasurex     = lt_unitsofmeasurex.
      IF lw_return-type <> 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
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
        IF us_input-werks IS NOT INITIAL.
          PERFORM frm_plant_view USING us_input lv_matnr CHANGING cs_return.
        ENDIF.
        " 扩张MRP AREA
        IF us_input-mrp_aera IS NOT INITIAL.
          PERFORM frm_mrp_aera_create USING us_input lv_matnr CHANGING cs_return.
        ENDIF.
        " 扩张销售组织视图
        IF us_input-vkorg IS NOT INITIAL.
          PERFORM frm_salesorg_view USING us_input lv_matnr CHANGING cs_return.
        ENDIF.
        " 维护分类视图
        IF us_input-class IS NOT INITIAL.
          PERFORM frm_class_view USING us_input lv_matnr lv_flag CHANGING cs_return.
        ENDIF.
        IF cs_return IS INITIAL.
          cs_return-msg_matnr = lv_matnr.
          cs_return-msg_type = 'S'.
          cs_return-message = '成功'.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        cs_return-msg_matnr = lv_matnr.
        cs_return-msg_type = 'E'.
        cs_return-message = lw_return-message.
      ENDIF.
    ELSE.
      CLEAR: lv_matnr_mpn, lv_matnr_t.
      lv_matnr_mpn = lv_matnr+0(16).
      matn1_input: lv_matnr_mpn.
      SELECT SINGLE matnr FROM mara INTO lv_matnr_t
        WHERE matnr = lv_matnr_mpn AND mprof = 'IMPN'.
      IF sy-subrc = 0.
        " 创建物料基本视图
        CLEAR: lw_headdata, lw_clientdata, lw_clientdatax.
        lw_headdata-material = lv_matnr.                        " 物料号
        lw_headdata-matl_type = us_input-mtart.                 " 物料类型
        lw_headdata-ind_sector = 'P'.                           " 行业领域
        lw_headdata-basic_view = 'X'.                           " 基本数据视图
        lw_headdata-purchase_view = 'X'.                        " 采购视图
        lw_clientdata-matl_group = us_input-matkl.              " 物料组
        lw_clientdatax-matl_group = 'X'.
        lw_clientdata-base_uom = lv_meins.                      " 基本计量单位
        lw_clientdatax-base_uom = 'X'.
        lw_clientdata-mfr_no = lv_mfrnr.                        " 制造商
        lw_clientdatax-mfr_no = 'X'.
        lw_clientdata-inv_mat_no = lv_matnr_mpn.                " 公司自有（内部）库存管理的物料编号
        lw_clientdatax-inv_mat_no = 'X'.
        lw_clientdata-manu_mat = lv_matnr.                      " 制造商零件编号
        lw_clientdatax-manu_mat = 'X'.

        IF us_input-lvoma IS NOT INITIAL.
          lw_clientdata-del_flag = 'X'.                         " 在客户级标记要删除的物料
          lw_clientdatax-del_flag = 'X'.
        ELSE.
          lw_clientdata-del_flag = ''.                          " 在客户级标记要删除的物料
          lw_clientdatax-del_flag = 'X'.
        ENDIF.

        " 物料描述
        CLEAR: lt_materialdescription, lw_materialdescription.
        lw_materialdescription-langu = '1'.
        lw_materialdescription-matl_desc = us_input-maktx.      " 物料描述
        APPEND lw_materialdescription TO lt_materialdescription.

        " 物料描述长文本
        IF us_input-maktxl IS NOT INITIAL.
          CLEAR: lt_materiallongtext, lw_materiallongtext, lv_maktxl.
          lv_maktxl = us_input-maktxl.
          lw_materiallongtext-applobject = 'MATERIAL'.
          lw_materiallongtext-text_name = lv_matnr.
          lw_materiallongtext-text_id = 'GRUN'.
          lw_materiallongtext-langu = '1'.
          lw_materiallongtext-format_col = 1.
          lw_materiallongtext-text_line = lv_maktxl+0(132).
          APPEND lw_materiallongtext TO lt_materiallongtext.
          CLEAR: lw_materiallongtext.
          lw_materiallongtext-applobject = 'MATERIAL'.
          lw_materiallongtext-text_name = lv_matnr.
          lw_materiallongtext-text_id = 'GRUN'.
          lw_materiallongtext-langu = '1'.
          lw_materiallongtext-format_col = 2.
          lw_materiallongtext-text_line = lv_maktxl+132(132).
          APPEND lw_materiallongtext TO lt_materiallongtext.
        ENDIF.
        CLEAR: lw_return.
        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
          EXPORTING
            headdata            = lw_headdata
            clientdata          = lw_clientdata
            clientdatax         = lw_clientdatax
          IMPORTING
            return              = lw_return
          TABLES
            materialdescription = lt_materialdescription
            materiallongtext    = lt_materiallongtext.
        IF lw_return-type <> 'E'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          cs_return-msg_matnr = lv_matnr.
          cs_return-msg_type = 'S'.
          cs_return-message = '成功'.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          cs_return-msg_matnr = lv_matnr.
          cs_return-msg_type = 'E'.
          cs_return-message = lw_return-message.
        ENDIF.
      ELSE.
        cs_return-msg_matnr = lv_matnr.
        cs_return-msg_type = 'E'.
        CONCATENATE '未找到MPN物料' lv_matnr_mpn INTO cs_return-message.
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
FORM frm_get_mrparea  USING    uv_matnr TYPE matnr
                               uv_werks TYPE werkdp
                               uv_berid TYPE berid
                      CHANGING cv_flag  TYPE c.
  DATA: lw_mt61d TYPE mt61d,
        lw_mdkp  TYPE mdkp,
        lw_cm61m TYPE cm61m,
        lw_mdsta TYPE mdsta,
        lw_ergbz TYPE sfilt-ergbz,
        lt_mdpsx TYPE STANDARD TABLE OF mdps,
        lt_mdezx TYPE STANDARD TABLE OF mdez,
        lt_mdsux TYPE STANDARD TABLE OF mdsu.
  CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
    EXPORTING
      matnr                    = uv_matnr
      werks                    = uv_werks
      berid                    = uv_berid
    IMPORTING
      e_mt61d                  = lw_mt61d
      e_mdkp                   = lw_mdkp
      e_cm61m                  = lw_cm61m
      e_mdsta                  = lw_mdsta
      e_ergbz                  = lw_ergbz
    TABLES
      mdpsx                    = lt_mdpsx
      mdezx                    = lt_mdezx
      mdsux                    = lt_mdsux
    EXCEPTIONS
      material_plant_not_found = 1
      plant_not_found          = 2
      OTHERS                   = 3.
  IF sy-subrc = 0 AND lw_mt61d IS NOT INITIAL.
    cv_flag = 'X'.
  ENDIF.

ENDFORM.