FUNCTION z_pp_pi_process_order_create .
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
*& AUTHOR:  郭磊
*& DATE:    2019.08.23
*&------------------------------------------------------------------
*& MODIFICATION LOG:
*& VERSION   DATE        AUTHOR       DESCRIPTION     CHANGE REQUEST
*& -------- ----------  -----------  -------------  -----------------
*& 1.0       2019.08.23   郭磊
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
*---2.1.4 为创建成功的所有流程订单更新批次和长文本
*---2.1.5 为创建成功的所有流程订单下达生产订单
*---如果是非S需求
*---2.2.1 模拟创建非S流程订单
*---2.2.2 为非S产品创建流程订单
*---2.2.3 为创建成功的流程订单更新容差
*---2.2.4 为创建成功的流程订单更新长文本
*---2.2.5 下达生产订单
*---3.记录写入业务记录查询表ZPPT0001
  DATA ls_topmat TYPE cstmat.
  DATA lt_stb TYPE TABLE OF stpox.
  DATA ls_stb TYPE stpox.
  DATA lt_stb_merge TYPE TABLE OF stpox.
  DATA lt_stb_merge_final TYPE TABLE OF stpox.
  DATA lv_matnr TYPE matnr.
  DATA lv_werks TYPE werks_d.
  DATA lv_verid TYPE verid.
  DATA lv_datuv TYPE sy-datum.
  DATA lv_emeng TYPE basmn.
  DATA lv_mtart TYPE mtart.
  DATA ls_order_data TYPE bapi_pi_order_create.
  DATA ls_return TYPE bapiret2.
  DATA lv_aufnr TYPE aufnr.
  DATA lv_aufnr_db TYPE aufnr.
  DATA lt_zpitlog0004 TYPE TABLE OF zpitlog0004.
  DATA lt_zppt0001_subs TYPE TABLE OF zppt0001_subs. "存储所有S订单下的所有子订单
  DATA lt_zppt0001_subs_temp TYPE TABLE OF zppt0001_subs. "存储每个S订单下的所有子订单
  DATA ls_zppt0001_subs TYPE zppt0001_subs.
  DATA lv_error TYPE flag.
  DATA lv_count TYPE i.
  DATA lv_mblnr TYPE mblnr.
  DATA lv_objnr TYPE j_objnr.
  DATA lt_status TYPE TABLE OF jstat.
  DATA lv_menge_s TYPE menge_d.
  DATA lv_menge_h TYPE menge_d.
  DATA lt_component TYPE TABLE OF bapi_order_component.
  DATA lt_component_merge TYPE TABLE OF bapi_order_component.
  DATA ls_objects TYPE bapi_pi_order_objects.
  DATA lv_vbeln TYPE vbeln.
  DATA lv_longtext TYPE string.
  DATA lv_ausss TYPE ausss.
  DATA lv_andec TYPE andec.

*---检查业务数据
  PERFORM frm_order_create_data_check USING input CHANGING output lv_error.

  CHECK lv_error <> 'X'.
  LOOP AT input-mt_aps2sap_pro-pro_req INTO DATA(ls_input).
    "物料号转换
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = ls_input-matnr
      IMPORTING
        output       = ls_input-matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    REFRESH lt_zppt0001_subs_temp.
    CLEAR: ls_zppt0001_subs, lv_aufnr_db.
    "日志表初始化信息
    APPEND INITIAL LINE TO lt_zpitlog0004 ASSIGNING FIELD-SYMBOL(<fs_log>).
    IF <fs_log> IS ASSIGNED.
      MOVE-CORRESPONDING ls_input TO <fs_log>.
      <fs_log>-uname = sy-uname.
      <fs_log>-datum = sy-datum.
      <fs_log>-uzeit = sy-uzeit.
    ENDIF.
    "返回参数初始化信息
    APPEND INITIAL LINE TO output-mt_aps2sap_pro_ret-pro_ret ASSIGNING FIELD-SYMBOL(<fs_output>).
    IF <fs_output> IS ASSIGNED.
      MOVE-CORRESPONDING ls_input TO <fs_output>.
    ENDIF.
*根据APS传输的唯一标识码识别是否成功处理过此条电文，若已成功处理过，
*则检查生成的流程订单，对满足以下任一条件的数据不再重新创建
*1.CRTD 创建状态
*2.REL  释放状态
*3.TECO 并且有报工（CNF或PCNF）或有总和不为0的收发料
*---因为每次接口传送的数据不是很多（最多70条），所以下面直接select single了
    SELECT SINGLE aufnr FROM zpitlog0004 INTO lv_aufnr_db
      WHERE guid = ls_input-guid.
    IF sy-subrc = 0 AND lv_aufnr_db IS NOT INITIAL.
      IF <fs_log> IS ASSIGNED."之前已经创建的流程订单不能更覆盖掉
        <fs_log>-aufnr = lv_aufnr_db.
      ENDIF.
      lv_objnr = 'OR' && lv_aufnr_db.
      CALL FUNCTION 'STATUS_READ'
        EXPORTING
          objnr            = lv_objnr
          only_active      = 'X'
        TABLES
          status           = lt_status
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc = 0.
        READ TABLE lt_status INTO DATA(ls_status) WITH KEY stat = 'I0001'."CRTD 已创建
        IF sy-subrc = 0.
          IF <fs_output> IS ASSIGNED.
            <fs_output>-type = 'E'.
            <fs_output>-message = '相同GUID已生成订单，并且没有关闭'.
          ENDIF.
          IF <fs_log> IS ASSIGNED.
            <fs_log>-type = 'E'.
            <fs_log>-message = '相同GUID已生成订单，并且没有关闭'.
          ENDIF.
          CONTINUE."执行下一条数据
        ENDIF.
        READ TABLE lt_status INTO ls_status WITH KEY stat = 'I0002'."REL 已释放
        IF sy-subrc = 0.
          IF <fs_output> IS ASSIGNED.
            <fs_output>-type = 'E'.
            <fs_output>-message = '相同GUID已生成订单并释放，并且没有关闭'.
          ENDIF.
          IF <fs_log> IS ASSIGNED.
            <fs_log>-type = 'E'.
            <fs_log>-message = '相同GUID已生成订单并释放，并且没有关闭'.
          ENDIF.
          CONTINUE."执行下一条数据
        ENDIF.
        "检查生成的流程订单状态是否为TECO且有PCNF/CNF中的任意一个
        READ TABLE lt_status INTO ls_status WITH KEY stat = 'I0045'."TECO 技术性完成 并且有报工（CNF或PCNF）或有总和不为0的收发料
        IF sy-subrc = 0.
          READ TABLE lt_status INTO ls_status WITH KEY stat = 'I0009'."CNF  已确认
          IF sy-subrc = 0.
            IF <fs_output> IS ASSIGNED.
              <fs_output>-type = 'E'.
              <fs_output>-message = '相同GUID已生成订单且已技术性完成并确认'.
            ENDIF.
            IF <fs_log> IS ASSIGNED.
              <fs_log>-type = 'E'.
              <fs_log>-message = '相同GUID已生成订单且已技术性完成并确认'.
            ENDIF.
            CONTINUE."执行下一条数据
          ENDIF.
          READ TABLE lt_status INTO ls_status WITH KEY stat = 'I0010'."PCNF	部分确认
          IF sy-subrc = 0.
            IF <fs_output> IS ASSIGNED.
              <fs_output>-type = 'E'.
              <fs_output>-message = '相同GUID已生成订单且已技术性完成并部分确认'.
            ENDIF.
            IF <fs_log> IS ASSIGNED.
              <fs_log>-type = 'E'.
              <fs_log>-message = '相同GUID已生成订单且已技术性完成并部分确认'.
            ENDIF.
            CONTINUE."执行下一条数据
          ENDIF.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_aufnr_db
            IMPORTING
              output = lv_aufnr_db.
          CLEAR: lv_menge_s, lv_menge_h.
          SELECT SUM( menge ) FROM aufm INTO lv_menge_s
            WHERE aufnr = lv_aufnr_db AND shkzg = 'S'.
          SELECT SUM( menge ) FROM aufm INTO lv_menge_h
            WHERE aufnr = lv_aufnr_db AND shkzg = 'H'.
          IF lv_menge_s - lv_menge_h <> 0."判断订单收发料总和不为0
            IF <fs_output> IS ASSIGNED.
              <fs_output>-type = 'E'.
              <fs_output>-message = '相同GUID已生成订单且已产生货物移动'.
            ENDIF.
            IF <fs_log> IS ASSIGNED.
              <fs_log>-type = 'E'.
              <fs_log>-message = '相同GUID已生成订单且已产生货物移动'.
            ENDIF.
            CONTINUE."执行下一条数据
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    "---数据需要创建流程订单，继续执行
    CLEAR: lv_datuv,lv_matnr,lv_emeng,lv_werks,lv_verid,ls_topmat,lv_count,lv_error,lv_ausss,lv_andec.
    REFRESH lt_stb.
    "计量单位外码转内码
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = ls_input-gmein
*       LANGUAGE       = SY-LANGU
      IMPORTING
        output         = ls_input-gmein
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
*----若“是否为S”标识为S，则通过展开S的BOM,计算下阶所有半成品（物料类型不为原辅料）的数量,并创建订单(S和S子订单)
    IF ls_input-zsfs = 'S'.
      REFRESH lt_stb_merge_final.
*--------S模拟创建检查，模拟创建S及S子订单
      PERFORM frm_s_create_simu USING ls_input CHANGING lv_error lv_aufnr <fs_output> <fs_log>.
      IF lv_error = 'X'."数据有错误，不能继续创建
        CONTINUE.
      ENDIF.
*-----为S本身创建流程订单
      PERFORM frm_create_process_order USING ls_input CHANGING lv_error lv_aufnr <fs_output> <fs_log>.
***----获取处理完物料主数据的装配报废率（MARC-AUSSS）和组件报废率（STPO-AUSCH）的组件需求数
      IF lv_error IS INITIAL.
*-------------------------------处理S展开的子订单-------------------------------
        ls_input-gamng = <fs_log>-gamng."S订单最终数量
*        CLEAR lv_ausss.
*        "计算装配报废
*        SELECT SINGLE ausss FROM marc INTO lv_ausss
*        WHERE matnr = ls_input-matnr
*          AND werks = ls_input-werks.
*        IF sy-subrc = 0 AND lv_ausss <> 0."有装配报废
*          SELECT SINGLE andec FROM t006 INNER JOIN mara
*          ON t006~msehi = mara~meins
*                INTO lv_andec
*          WHERE mara~matnr = ls_input-matnr.
*          IF sy-subrc = 0 AND lv_andec = 0."该单位不支持小数位,计算结果向上取整
*            ls_input-gamng = ls_input-gamng + ceil( ls_input-gamng * lv_ausss / 100 ).
*          ELSE."该单位支持小数位
*            ls_input-gamng = ls_input-gamng + ( ls_input-gamng * lv_ausss / 100 ).
*          ENDIF.
*        ENDIF.
        PERFORM frm_calculate_bom_req USING ls_input '' CHANGING lt_stb_merge_final.
*----------为生产S所需的所有半成品创建流程订单
        LOOP AT lt_stb_merge_final INTO ls_stb."重组好的半成品
          CLEAR: ls_order_data,ls_return,lv_aufnr.
          ls_order_data-material = ls_stb-idnrk.
          ls_order_data-plant = ls_stb-werks.
          ls_order_data-order_type = ls_input-auart.
          ls_order_data-basic_start_date = ls_input-gstrp.
          ls_order_data-basic_end_date = ls_input-gltrp.
          ls_order_data-quantity = ls_stb-mnglg.
          ls_order_data-quantity_uom = ls_stb-meins.
          ls_order_data-prod_version = ls_input-verid.
          "传输销售订单信息给BAPI质检出口ZXQPAU04(S订单BAPI不填销售订单)
          lv_vbeln = ls_input-kdauf.
          SET PARAMETER ID 'ZAPS_VBELN' FIELD ''."首先清空
          SET PARAMETER ID 'ZAPS_VBELN' FIELD lv_vbeln.
          CALL FUNCTION 'BAPI_PROCORD_CREATE'
            EXPORTING
              orderdata    = ls_order_data
            IMPORTING
              return       = ls_return
              order_number = lv_aufnr.
          IF ls_return-type = 'E' OR ls_return-type = 'A'.
            lv_error = 'X'.
            IF <fs_output> IS ASSIGNED.
              <fs_output>-type = 'E'.
              <fs_output>-message = ls_return-message.
            ENDIF.
            IF <fs_log> IS ASSIGNED.
              CLEAR: <fs_log>-aufnr.
              <fs_log>-type = 'E'.
              <fs_log>-message = ls_return-message.
            ENDIF.
          ELSE.
            "保存日志记录（S子订单信息）
            APPEND INITIAL LINE TO lt_zppt0001_subs_temp ASSIGNING FIELD-SYMBOL(<fs_log0004a_temp>).
            IF <fs_log0004a_temp> IS ASSIGNED AND <fs_log> IS ASSIGNED.
              MOVE-CORRESPONDING <fs_log> TO <fs_log0004a_temp>."批次工厂赋值
              <fs_log0004a_temp>-type = 'S'.
              <fs_log0004a_temp>-aufnr = lv_aufnr.
              <fs_log0004a_temp>-matnr = ls_stb-idnrk.
              <fs_log0004a_temp>-menge = ls_stb-mnglg.
              <fs_log0004a_temp>-aufnr_s = <fs_log>-aufnr.
              <fs_log0004a_temp>-matnr_s = <fs_log>-matnr.
              <fs_log0004a_temp>-menge_s = <fs_log>-gamng.
              <fs_log0004a_temp>-message = '流程订单' && lv_aufnr && '创建成功'.
            ENDIF.
          ENDIF.
          IF lv_error = 'X'."S订单中的任何一个子订单创建失败，则整个S订单都不创建
            REFRESH lt_zppt0001_subs_temp.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
      IF lv_error IS INITIAL."S订单中的所有子订单全部创建成功
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        IF <fs_output> IS ASSIGNED AND <fs_log> IS ASSIGNED.
          <fs_output>-type = 'S'.
          IF lt_zppt0001_subs_temp IS NOT INITIAL.
            <fs_output>-message = '订单' && <fs_log>-aufnr && '及其子订单'.
            LOOP AT lt_zppt0001_subs_temp ASSIGNING <fs_log0004a_temp>.
              IF sy-tabix = 1.
                CONCATENATE <fs_output>-message <fs_log0004a_temp>-aufnr INTO <fs_output>-message.
              ELSE.
                CONCATENATE <fs_output>-message <fs_log0004a_temp>-aufnr INTO <fs_output>-message SEPARATED BY ','.
              ENDIF.
            ENDLOOP.
            CONCATENATE <fs_output>-message '创建成功' INTO <fs_output>-message.
            IF <fs_log0004a_temp> IS ASSIGNED.
*              DO 20 TIMES."检查最后一个子订单创建成功，已经写入数据库
*                WAIT UP TO '0.1' SECONDS.
*                SELECT SINGLE aufnr FROM afko INTO @DATA(lv_aufnr_check)
*                  WHERE aufnr = @<fs_log0004a_temp>-aufnr.
*                IF sy-subrc = 0.
*                  EXIT.
*                ENDIF.
*              ENDDO.
            ENDIF.
          ELSE.
            <fs_output>-message = '订单' && <fs_log>-aufnr && '创建成功'.
*            DO 20 TIMES."当S没有子订单时，需要此检查
*              SELECT SINGLE aufnr FROM afko INTO @lv_aufnr_check
*                WHERE aufnr = @<fs_log>-aufnr.
*              IF sy-subrc = 0.
*                EXIT.
*              ENDIF.
*              WAIT UP TO '0.1' SECONDS.
*            ENDDO.
          ENDIF.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CONTINUE."S及其子流程订单创建失败，继续处理下一条传入数据
      ENDIF.
*-------为创建成功的所有流程订单更新批次
      IF <fs_log> IS ASSIGNED AND <fs_log>-charg IS NOT INITIAL.
        "为S订单本身创建
        IF <fs_log>-aufnr IS NOT INITIAL.
          PERFORM frm_update_order_batch USING <fs_log>-aufnr <fs_log>-matnr <fs_log>-werks <fs_log>-charg CHANGING <fs_log>.
        ENDIF.
        "为S所有子订单创建
        LOOP AT lt_zppt0001_subs_temp ASSIGNING <fs_log0004a_temp>.
          PERFORM frm_update_suborder_batch
          USING <fs_log0004a_temp>-aufnr <fs_log0004a_temp>-matnr <fs_log0004a_temp>-werks <fs_log0004a_temp>-charg CHANGING <fs_log0004a_temp>.
        ENDLOOP.
      ENDIF.
*-------为创建成功的所有流程订单更新长文本
      CLEAR lv_longtext.
      lv_longtext = '/内部订单号:' && ls_input-zrsv01 && '/成本中心号:' && ls_input-zrsv02 && '/备注（半成品报废）:' && ls_input-zrsv03 && '/长文本：' && ls_input-ztext.
      IF lv_longtext IS NOT INITIAL AND <fs_log> IS ASSIGNED.
*        "为S订单本身更新
*        PERFORM frm_save_long_text USING <fs_log>-aufnr lv_longtext CHANGING <fs_log>.
*        "为S所有子订单更新
*        LOOP AT lt_zppt0001_subs_temp ASSIGNING <fs_log0004a_temp>.
*          PERFORM frm_save_suborder_longtext USING <fs_log0004a_temp>-aufnr lv_longtext CHANGING <fs_log0004a_temp>.
*        ENDLOOP.
        DATA(lt_text) = VALUE zsds_so_text_t( tdobject = 'AUFK' tdname = sy-mandt && <fs_log>-aufnr ( tdid = 'KOPF' text = lv_longtext ) ). "创建长文本 add by litao 20191202
        LOOP AT lt_zppt0001_subs_temp ASSIGNING FIELD-SYMBOL(<fs_temp>).
          APPEND VALUE #( tdobject = 'AUFK' tdid = 'KOPF' tdname = sy-mandt && <fs_temp>-aufnr text = lv_longtext ) TO lt_text.
        ENDLOOP.
        PERFORM frm_create_text USING lt_text.
      ENDIF.
*---------为创建成功的所有流程订单下达生产订单
      IF ls_input-status = 'REL' AND <fs_log> IS ASSIGNED.
        PERFORM frm_release_order USING <fs_log>-aufnr CHANGING <fs_log>.
*        PERFORM frm_set_release USING <fs_log>-aufnr CHANGING <fs_log>-type <fs_log>-message. "订单下达 add by litao 20191202
        LOOP AT lt_zppt0001_subs_temp ASSIGNING <fs_log0004a_temp>.
          PERFORM frm_release_suborder USING <fs_log0004a_temp>-aufnr CHANGING <fs_log0004a_temp>.
*          PERFORM frm_set_release USING <fs_log0004a_temp>-aufnr CHANGING <fs_log0004a_temp>-type <fs_log0004a_temp>-message. "订单下达 add by litao 20191202
        ENDLOOP.
      ENDIF.
      APPEND LINES OF lt_zppt0001_subs_temp TO lt_zppt0001_subs.
    ELSE.
*--------模拟创建非S流程订单
      PERFORM frm_create_process_order_simu USING ls_input CHANGING lv_error lv_aufnr <fs_output> <fs_log>.
      IF lv_error = 'X'.
        CONTINUE.
      ENDIF.
*----为非S产品创建流程订单
      PERFORM frm_create_process_order USING ls_input CHANGING lv_error lv_aufnr <fs_output> <fs_log>.
      IF lv_error IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        IF <fs_output> IS ASSIGNED AND <fs_log> IS ASSIGNED.
          <fs_output>-type = 'S'.
          <fs_output>-message = '订单' && <fs_log>-aufnr && '创建成功'.
        ENDIF.
*        DO 20 TIMES.
*          WAIT UP TO '0.1' SECONDS.
*          SELECT SINGLE aufnr FROM afko INTO @lv_aufnr_check
*            WHERE aufnr = @<fs_log>-aufnr.
*          IF sy-subrc = 0.
*            EXIT.
*          ENDIF.
*        ENDDO.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.
*-------为创建成功的流程订单更新容差
      IF <fs_log> IS ASSIGNED AND <fs_log>-aufnr IS NOT INITIAL.
        PERFORM frm_upd_order_under_over_dlv USING <fs_log>-aufnr <fs_log>-matnr <fs_log>-werks <fs_log>-charg CHANGING <fs_log>.
      ENDIF.
*-------为创建成功的流程订单更新长文本
      CLEAR lv_longtext.
      lv_longtext = '/内部订单号:' && ls_input-zrsv01 && '/成本中心号:' && ls_input-zrsv02 && '/备注（半成品报废）:' && ls_input-zrsv03 && '/长文本：' && ls_input-ztext.
      IF lv_longtext IS NOT INITIAL AND <fs_log> IS ASSIGNED.
*        PERFORM frm_save_long_text USING <fs_log>-aufnr lv_longtext CHANGING <fs_log>.
        lt_text = VALUE #( tdobject = 'AUFK' tdname = sy-mandt && <fs_log>-aufnr ( tdid = 'KOPF' text = lv_longtext ) ). "创建长文本 add by litao 20191130
        PERFORM frm_create_text USING lt_text.
      ENDIF.
*---------下达生产订单
      IF ls_input-status = 'REL' AND <fs_log> IS ASSIGNED.
       PERFORM frm_release_order USING <fs_log>-aufnr CHANGING <fs_log>.
*        PERFORM frm_set_release USING <fs_log>-aufnr CHANGING <fs_log>-type <fs_log>-message. "创建长文本 add by litao 20191202
      ENDIF.
    ENDIF.
  ENDLOOP.

  MODIFY zpitlog0004 FROM TABLE lt_zpitlog0004.
  SORT lt_zppt0001_subs BY werks charg aufnr_s aufnr.
  DELETE ADJACENT DUPLICATES FROM lt_zppt0001_subs COMPARING werks charg aufnr_s aufnr.
  MODIFY zppt0001_subs FROM TABLE lt_zppt0001_subs.

*---记录写入业务记录查询表ZPPT0001
  PERFORM frm_save_aps_create_record USING lt_zpitlog0004 lt_zppt0001_subs.
  PERFORM frm_create_job."add by litao 20191205 用于修复未下达未修改成文本

ENDFUNCTION.