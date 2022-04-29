*&---------------------------------------------------------------------*
*& 包含               ZPPR0005_C04
*&---------------------------------------------------------------------*
"定义LIMS的用户名跟密码
DATA lv_user(10) VALUE 'STARLIMS'.
DATA lv_pwd(10) VALUE 'Lims!1345'.

CLASS zcl_lims_oper DEFINITION.
  PUBLIC SECTION.
    TYPES ty_tab_zppt0003 TYPE TABLE OF zppt0003.
    METHODS:
      send_to_lims IMPORTING flag TYPE char2 OPTIONAL RETURNING VALUE(rs_result) TYPE zsign_batch_sessionless_soap_o."下发批结信息给LIMS
*      send_to_lims_pj RETURNING VALUE(rs_result) TYPE zsign_batch_sessionless_soap_o."点击批结按钮下发批结信息给LIMS
  PRIVATE SECTION.
    METHODS:set_lims_request IMPORTING it_zppt0003 TYPE ty_tab_zppt0003  flag TYPE char2 OPTIONAL  EXPORTING es_output TYPE zsign_batch_sessionless_soap_i."设置LIMS下发消息
*    METHODS:set_lims_request_pj IMPORTING it_zppt0003 TYPE ty_tab_zppt0003 EXPORTING es_output TYPE zsign_batch_sessionless_soap_i."设置LIMS下发消息
ENDCLASS.
CLASS zcl_lims_oper IMPLEMENTATION.

  "下发LIMS
  METHOD send_to_lims.
    DATA: lv_intid  TYPE ze_intf_id,
          ls_output TYPE  zsign_batch_sessionless_soap_i,
          lo_proxy  TYPE REF TO zco_si_sap2lims_info_batch_out.
    lv_intid = 'QM0564'.
    "样品收货Y01不下发LIMS
    IF zspp034_head-bwart = '102' OR  zspp034_head-bwart = 'Y01' OR zspp034_head-bwart = 'Y02'.
      RETURN.
    ENDIF.
    "设置请求参数
    IF flag = 'PJ'.
      set_lims_request( EXPORTING it_zppt0003 = gt_zppt0003 flag = 'PJ' IMPORTING es_output = ls_output ).
    ELSE.
      set_lims_request( EXPORTING it_zppt0003 = gt_zppt0003 IMPORTING es_output = ls_output ).
    ENDIF.

    "调用通用接口下发到LIMS.
    IF ls_output-arr_parameters-batch_sign_dto IS NOT INITIAL.
**   日志记录-记录输入参数
**----------------------------------------------------------------------
      DATA(l_guid) = zcl_bc_public=>get_guid( )."生成本次调用唯一标识
      DATA(l_flag) = zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = lv_intid iv_ptype = 'I' is_data = ls_output ). "记录输入参数
      IF l_flag EQ 'A'."接口是否启用判断
        rs_result-sign_batch_sessionless_result-ret_code = 'E'.
        rs_result-sign_batch_sessionless_result-ret_msg = '接口未启用！'.
        RETURN.
      ENDIF.
      CREATE OBJECT lo_proxy.
      CLEAR rs_result.
      lo_proxy->si_sap2lims_info_batch_out( EXPORTING output = ls_output IMPORTING input = rs_result ).
*----------------------------------------------------------------------
*   日志记录-记录返回参数
*----------------------------------------------------------------------
      zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = lv_intid iv_ptype = 'E' is_data = rs_result ). "记录输出参数

    ELSE.
      rs_result-sign_batch_sessionless_result-ret_code = 'W'.
      rs_result-sign_batch_sessionless_result-ret_msg = '批结信息为空'.
    ENDIF.
    IF rs_result-sign_batch_sessionless_result-ret_code = 'S'.
      lv_type = 'S'.
    ENDIF.
  ENDMETHOD.
  "20220414点击批结按钮下发LIMS的方法
  "下发LIMS
*  METHOD send_to_lims_pj.
*    DATA: lv_intid  TYPE ze_intf_id,
*          ls_output TYPE  zsign_batch_sessionless_soap_i,
*          lo_proxy  TYPE REF TO zco_si_sap2lims_info_batch_out.
*    lv_intid = 'QM0564'.
*    "样品收货Y01不下发LIMS
*    IF zspp034_head-bwart = '102' OR  zspp034_head-bwart = 'Y01' OR zspp034_head-bwart = 'Y02'.
*      RETURN.
*    ENDIF.
*    "设置请求参数
*    set_lims_request_pj( EXPORTING it_zppt0003 = gt_zppt0003 IMPORTING es_output = ls_output ).
*    "调用通用接口下发到LIMS.
*    IF ls_output-arr_parameters-batch_sign_dto IS NOT INITIAL.
***   日志记录-记录输入参数
***----------------------------------------------------------------------
*      DATA(l_guid) = zcl_bc_public=>get_guid( )."生成本次调用唯一标识
*      DATA(l_flag) = zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = lv_intid iv_ptype = 'I' is_data = ls_output ). "记录输入参数
*      IF l_flag EQ 'A'."接口是否启用判断
*        rs_result-sign_batch_sessionless_result-ret_code = 'E'.
*        rs_result-sign_batch_sessionless_result-ret_msg = '接口未启用！'.
*        RETURN.
*      ENDIF.
*      CREATE OBJECT lo_proxy.
*      CLEAR rs_result.
*      lo_proxy->si_sap2lims_info_batch_out( EXPORTING output = ls_output IMPORTING input = rs_result ).
**----------------------------------------------------------------------
**   日志记录-记录返回参数
**----------------------------------------------------------------------
*      zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = lv_intid iv_ptype = 'E' is_data = rs_result ). "记录输出参数
*    ELSE.
*      rs_result-sign_batch_sessionless_result-ret_code = 'W'.
*      rs_result-sign_batch_sessionless_result-ret_msg = '批结信息为空'.
*    ENDIF.
*
*  ENDMETHOD.
  "20220414点击批结按钮下发LIMS的方法end
  "设置下发LIMS消息
  METHOD set_lims_request.
    "定义含有本次WMS确认收货数量以及本次样本量字段的批结数据结构
    DATA:BEGIN OF ls_zbatch_sign_dto_check.
           INCLUDE TYPE zbatch_sign_dto.
           DATA:menge TYPE zppt0003-menge,
         END OF  ls_zbatch_sign_dto_check.
    DATA lt_batch_sign_dto_check  LIKE TABLE OF ls_zbatch_sign_dto_check.

    DATA:ls_zbatch_sign_dto TYPE zbatch_sign_dto,
         lt_batch_sign_dto  TYPE zbatch_sign_dto_tab.
    CLEAR es_output.
    IF it_zppt0003 IS NOT INITIAL.
      "流程订单累计收货
*      SELECT aufnr,wemng FROM afpo FOR ALL ENTRIES IN @it_zppt0003
*        WHERE aufnr = @it_zppt0003-aufnr
*        INTO TABLE @DATA(lt_afpo).
      "检验批次单号,排除批量取消的检验批次
      SELECT qa~aufnr,qa~prueflos FROM qals AS qa FOR ALL ENTRIES IN @it_zppt0003
        WHERE qa~aufnr = @it_zppt0003-aufnr
        AND qa~art <> 'G04' "G04为组合装入库检验类型
       AND EXISTS (  SELECT stat FROM jest WHERE objnr = qa~objnr AND  ( stat <> 'I0224' AND  inact = '' ) )
        INTO TABLE @DATA(lt_qals).
      "如果是点击批结或者此单结束按钮的话，无需判断前台的选择屏幕字段
      IF flag = 'PJ'.
        LOOP AT it_zppt0003 INTO DATA(ls_zppt0003) WHERE zdelflg IS INITIAL.
          CLEAR ls_zbatch_sign_dto.
          READ TABLE lt_qals WITH KEY aufnr = ls_zppt0003-aufnr INTO DATA(ls_qals).
          CHECK sy-subrc = 0.
          ls_zbatch_sign_dto-prueflos = ls_qals-prueflos."检验批次号
          ls_zbatch_sign_dto-aufnr = ls_zppt0003-aufnr."流程订单号
*          ls_zbatch_sign_dto-zwmsmng = ls_zppt0003-menge + ls_zppt0003-wemng."本次入库申请数量 + 已入库
          ls_zbatch_sign_dto-wempf = ls_zppt0003-wempf."结案信息
          ls_zbatch_sign_dto-zmeins_t = ls_zppt0003-meins."基本计量单位
          APPEND ls_zbatch_sign_dto TO lt_batch_sign_dto.

          "V1.6样本量变更调整，保留本次入库申请数量，以留作计算
          MOVE-CORRESPONDING ls_zbatch_sign_dto TO ls_zbatch_sign_dto_check.
          ls_zbatch_sign_dto_check-menge = ls_zppt0003-menge."本次入库申请数量
          ls_zbatch_sign_dto_check-zwmsmng = ls_zppt0003-wemng."本次收货数量
*          "判断本次条目WMS申请入库数量是否为0；如果为0，则加上本次申请数量；如果不为0，则毋需再加
*          IF ls_zbatch_sign_dto_check-zwmsmng = 0 OR ls_zbatch_sign_dto_check-zwmsmng IS INITIAL.
*            ls_zbatch_sign_dto_check-zwmsmng = ls_zbatch_sign_dto_check-zwmsmng + ls_zppt0003-menge.
*          ENDIF.
          APPEND ls_zbatch_sign_dto_check TO lt_batch_sign_dto_check.
          CLEAR:ls_zppt0003,ls_zbatch_sign_dto_check.
        ENDLOOP.
      ELSE.
        LOOP AT it_zppt0003 INTO ls_zppt0003 WHERE wempf IS NOT INITIAL AND zdelflg IS INITIAL.
          CLEAR ls_zbatch_sign_dto.
          READ TABLE lt_qals WITH KEY aufnr = ls_zppt0003-aufnr INTO ls_qals.
          CHECK sy-subrc = 0.
          ls_zbatch_sign_dto-prueflos = ls_qals-prueflos."检验批次号
          ls_zbatch_sign_dto-aufnr = ls_zppt0003-aufnr."流程订单号
*          ls_zbatch_sign_dto-zwmsmng = ls_zppt0003-menge + ls_zppt0003-wemng."本次入库申请数量 + 已入库
          ls_zbatch_sign_dto-wempf = ls_zppt0003-wempf."结案信息
          ls_zbatch_sign_dto-zmeins_t = ls_zppt0003-meins."基本计量单位
          APPEND ls_zbatch_sign_dto TO lt_batch_sign_dto.
          "V1.6样本量变更调整，保留本次入库申请数量，以留作计算
          MOVE-CORRESPONDING ls_zbatch_sign_dto TO ls_zbatch_sign_dto_check.
          ls_zbatch_sign_dto_check-menge = ls_zppt0003-menge."本次入库申请数量
          ls_zbatch_sign_dto_check-zwmsmng = ls_zppt0003-wemng."本次收货数量
*          "判断本次条目WMS申请入库数量是否为0；如果为0，则加上本次申请数量；如果不为0，则毋需再加
*          IF ls_zbatch_sign_dto_check-zwmsmng = 0 OR ls_zbatch_sign_dto_check-zwmsmng IS INITIAL.
*            ls_zbatch_sign_dto_check-zwmsmng = ls_zbatch_sign_dto_check-zwmsmng + ls_zppt0003-menge.
*          ENDIF.
          APPEND ls_zbatch_sign_dto_check TO lt_batch_sign_dto_check.
          CLEAR:ls_zppt0003,ls_zbatch_sign_dto_check.
        ENDLOOP.
      ENDIF.
      CLEAR ls_zppt0003.
      "样本量逻辑调整，下传LIMS的入库申请数量调整如下
      IF lt_batch_sign_dto IS NOT INITIAL.
        "读取自建表以及售达方数据
        SELECT DISTINCT
          vbeln AS kdauf,
          kunnr,
          zppt0003~aufnr
        FROM vbak
        INNER JOIN zppt0003
        ON zppt0003~kdauf = vbak~vbeln
        WHERE EXISTS ( SELECT * FROM zqmt0564 WHERE kunnr = vbak~kunnr )
        INTO CORRESPONDING FIELDS OF TABLE @gt_kunnr.

        "首先根据工单反查全部的数据
        SELECT
          *
        FROM zppt0003
        FOR ALL ENTRIES IN @it_zppt0003
        WHERE aufnr = @it_zppt0003-aufnr
          AND zdelflg IS INITIAL
        INTO CORRESPONDING FIELDS OF TABLE @gt_zppt0003_list_all.

        "将全部的数据赋值给到gt_zppt0003_list_menge，统计相关的数据，按照移动类型、工单汇总分类，进行统计
        MOVE-CORRESPONDING gt_zppt0003_list_all TO gt_zppt0003_zwmsmng_tmp.
        LOOP AT gt_zppt0003_zwmsmng_tmp INTO gs_zppt0003_zwmsmng.
          COLLECT gs_zppt0003_zwmsmng INTO gt_zppt0003_zwmsmng.
          CLEAR gs_zppt0003_zwmsmng.
        ENDLOOP.

        "对批结信息的接口结构进行处理
        LOOP AT lt_batch_sign_dto ASSIGNING FIELD-SYMBOL(<ls_zbatch_sign_dto>).
          "判断计算规则
          READ TABLE gt_kunnr INTO gs_kunnr WITH KEY aufnr = ls_zbatch_sign_dto-aufnr.
          "当SY-subrc = 0 的时候，证明符合条件，需要101-102，加上Y01-Y02
          IF sy-subrc = 0.
            READ TABLE gt_zppt0003_zwmsmng INTO DATA(gs_zppt0003_zwmsmng_101) WITH KEY bwart = '101' aufnr = <ls_zbatch_sign_dto>-aufnr.
            READ TABLE gt_zppt0003_zwmsmng INTO DATA(gs_zppt0003_zwmsmng_102) WITH KEY bwart = '102' aufnr = <ls_zbatch_sign_dto>-aufnr.
            READ TABLE gt_zppt0003_zwmsmng INTO DATA(gs_zppt0003_zwmsmng_y01) WITH KEY bwart = 'Y01' aufnr = <ls_zbatch_sign_dto>-aufnr.
            READ TABLE gt_zppt0003_zwmsmng INTO DATA(gs_zppt0003_zwmsmng_y02) WITH KEY bwart = 'Y02' aufnr = <ls_zbatch_sign_dto>-aufnr.
            "判断本次申请数量、本次收货数量
            READ TABLE lt_batch_sign_dto_check INTO ls_zbatch_sign_dto_check WITH KEY aufnr = <ls_zbatch_sign_dto>-aufnr.
            IF <ls_zbatch_sign_dto>-zwmsmng IS INITIAL OR <ls_zbatch_sign_dto>-zwmsmng = 0.
              "若本次WMS已收货数量ZPPT0003-ZWMSMN=0，则再加上本次申请数量ZPPT0003-MENGE。
              <ls_zbatch_sign_dto>-zwmsmng = gs_zppt0003_zwmsmng_101-zwmsmng - gs_zppt0003_zwmsmng_102-zwmsmng + gs_zppt0003_zwmsmng_y01-zwmsmng - gs_zppt0003_zwmsmng_y02-zwmsmng + ls_zbatch_sign_dto_check-menge.
            ELSE."若本次WMS已收货数量ZPPT0003-ZWMSMN<>0，则不需要加上本次申请数量ZPPT0003-MENGE。
              <ls_zbatch_sign_dto>-zwmsmng = gs_zppt0003_zwmsmng_101-zwmsmng - gs_zppt0003_zwmsmng_102-zwmsmng + gs_zppt0003_zwmsmng_y01-zwmsmng - gs_zppt0003_zwmsmng_y02-zwmsmng.
            ENDIF.
          ELSE."SY-subrc <> 0的时候，证明不符合条件，需要101-102
            READ TABLE gt_zppt0003_zwmsmng INTO gs_zppt0003_zwmsmng_101 WITH KEY bwart = '101' aufnr = <ls_zbatch_sign_dto>-aufnr.
            READ TABLE gt_zppt0003_zwmsmng INTO gs_zppt0003_zwmsmng_102 WITH KEY bwart = '102' aufnr = <ls_zbatch_sign_dto>-aufnr.
            "判断本次申请数量、本次收货数量
            READ TABLE lt_batch_sign_dto_check INTO ls_zbatch_sign_dto_check WITH KEY aufnr = <ls_zbatch_sign_dto>-aufnr.
            IF <ls_zbatch_sign_dto>-zwmsmng IS INITIAL OR <ls_zbatch_sign_dto>-zwmsmng = 0.
              "若本次WMS已收货数量ZPPT0003-ZWMSMN=0，则再加上本次申请数量ZPPT0003-MENGE。
              <ls_zbatch_sign_dto>-zwmsmng = gs_zppt0003_zwmsmng_101-zwmsmng - gs_zppt0003_zwmsmng_102-zwmsmng + ls_zbatch_sign_dto_check-menge.
            ELSE."若本次WMS已收货数量ZPPT0003-ZWMSMN<>0，则不需要加上本次申请数量ZPPT0003-MENGE。
              <ls_zbatch_sign_dto>-zwmsmng = gs_zppt0003_zwmsmng_101-zwmsmng - gs_zppt0003_zwmsmng_102-zwmsmng + gs_zppt0003_zwmsmng_y01-zwmsmng - gs_zppt0003_zwmsmng_y02-zwmsmng.
            ENDIF.
          ENDIF.
          CLEAR:ls_zbatch_sign_dto,gs_zppt0003_zwmsmng_101,gs_zppt0003_zwmsmng_102,gs_zppt0003_zwmsmng_y01,gs_zppt0003_zwmsmng_y02.
        ENDLOOP.


      ENDIF.
      "样本量逻辑调整，下传LIMS的入库申请数量调整结束

      es_output-user = lv_user.
      es_output-pwd = lv_pwd.
      es_output-arr_parameters-batch_sign_dto = lt_batch_sign_dto.
    ENDIF.
  ENDMETHOD.

*  "20220414点击批结按钮设置下发参数
*  "设置下发LIMS消息
*  METHOD set_lims_request_pj.
*    DATA:ls_zbatch_sign_dto TYPE zbatch_sign_dto,
*         lt_batch_sign_dto  TYPE zbatch_sign_dto_tab.
*    CLEAR es_output.
*    IF it_zppt0003 IS NOT INITIAL.
*      "流程订单累计收货
**      SELECT aufnr,wemng FROM afpo FOR ALL ENTRIES IN @it_zppt0003
**        WHERE aufnr = @it_zppt0003-aufnr
**        INTO TABLE @DATA(lt_afpo).
*      "检验批次单号,排除批量取消的检验批次
*      SELECT qa~aufnr,qa~prueflos FROM qals AS qa FOR ALL ENTRIES IN @it_zppt0003
*        WHERE qa~aufnr = @it_zppt0003-aufnr
*        AND qa~art <> 'G04' "G04为组合装入库检验类型
*       AND EXISTS (  SELECT stat FROM jest WHERE objnr = qa~objnr AND  ( stat <> 'I0224' AND  inact = '' ) )
*        INTO TABLE @DATA(lt_qals).
*      LOOP AT it_zppt0003 INTO DATA(ls_zppt0003) WHERE zdelflg IS INITIAL.
*        CLEAR ls_zbatch_sign_dto.
*        READ TABLE lt_qals WITH KEY aufnr = ls_zppt0003-aufnr INTO DATA(ls_qals).
*        CHECK sy-subrc = 0.
*        ls_zbatch_sign_dto-prueflos = ls_qals-prueflos."检验批次号
*        ls_zbatch_sign_dto-aufnr = ls_zppt0003-aufnr."流程订单号
*        ls_zbatch_sign_dto-zwmsmng = ls_zppt0003-menge + ls_zppt0003-wemng."本次入库申请数量 + 已入库
*        ls_zbatch_sign_dto-wempf = ls_zppt0003-wempf."结案信息
*        ls_zbatch_sign_dto-zmeins_t = ls_zppt0003-meins."基本计量单位
*        APPEND ls_zbatch_sign_dto TO lt_batch_sign_dto.
*      ENDLOOP.
*      es_output-user = lv_user.
*      es_output-pwd = lv_pwd.
*      es_output-arr_parameters-batch_sign_dto = lt_batch_sign_dto.
*    ENDIF.
*  ENDMETHOD.
*  "20220414点击批结按钮设置下发参数end
ENDCLASS.