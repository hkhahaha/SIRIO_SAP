*----------------------------------------------------------------------*
***INCLUDE LZPP016F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_INPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_INPUT
*&      <-- LV_RET_MSG
*&---------------------------------------------------------------------*
FORM frm_check_input  USING    u_input TYPE zdt_plm2sap_createchange_bom_2
                      CHANGING c_ret_msg TYPE zdt_plm2sap_createchange_bom_1.

  DATA: ls_stko  TYPE stko,
        lt_stko  TYPE TABLE OF stko,
        lv_lines TYPE int4,
        l_flag   TYPE c,
        lv_matnr TYPE marc-matnr,
        lv_mmsta TYPE marc-mmsta,
        lv_mmstd TYPE marc-mmstd, "Seashell 20220509 工厂特定物料状态有效的起始日期
        lv_meinh TYPE marm-meinh,
        lt_item  TYPE TABLE OF zdt_plm2sap_createchange_bom_b,
        ls_item  TYPE zdt_plm2sap_createchange_bom_b.

  TRANSLATE g_flag TO UPPER CASE.
  IF g_flag NE 'A' AND g_flag NE 'C' AND g_flag NE 'D'.
    c_ret_msg-type = 'E'.
    c_ret_msg-message = '参数错误，flag的值只能是A/C/D'.
    RETURN.
  ENDIF.

  SELECT b~stkoz b~loekz b~datuv b~valid_to INTO CORRESPONDING FIELDS OF TABLE lt_stko
    FROM mast AS a
    INNER JOIN stko AS b ON  a~stlnr = b~stlnr AND a~stlal = b~stlal
    WHERE a~matnr = g_matnr
      AND a~werks = u_input-plant
      AND a~stlan = u_input-bom_usage
      AND a~stlal = g_stlal
      AND b~stlty = 'M'
      .
  LOOP AT lt_stko INTO ls_stko WHERE loekz = 'X'.
    c_ret_msg-type = 'E'.
  ENDLOOP.
  IF c_ret_msg-type = 'E'.
    c_ret_msg-message = 'BOM已标记删除，不能有任何操作'.
    RETURN.
  ENDIF.
  IF  g_flag = 'D'. "u_input-flag = 'C' OR  "新建和修改不判断
    CLEAR: l_flag.
    LOOP AT lt_stko INTO ls_stko WHERE loekz = '' AND datuv LE sy-datum AND
      ( valid_to GE sy-datum OR valid_to IS INITIAL ).
      l_flag = 'X'.
      EXIT.
    ENDLOOP.
    IF l_flag IS INITIAL.
      c_ret_msg-type = 'E'.
      c_ret_msg-message = '参数错误，BOM不存在'.
      RETURN.
    ENDIF.
  ELSE."创建或修改校验 liuzk 20201209
    CLEAR:lv_mmsta,lv_mmstd.
    "Seashell 20220509 工厂特定物料状态有效的起始日期
**    SELECT SINGLE mmsta INTO lv_mmsta
**      FROM marc
**      WHERE matnr = g_matnr
**        AND werks = u_input-plant.
    SELECT SINGLE mmsta,mmstd INTO ( @lv_mmsta,@lv_mmstd )
      FROM marc
      WHERE matnr = @g_matnr
        AND werks = @u_input-plant.
    IF sy-subrc <> 0.
      c_ret_msg-type = 'E'.
      c_ret_msg-message = '参数错误，物料' && g_matnr && '工厂' && u_input-plant && '在MARC不存在'.
      RETURN.
    ELSE.
**      IF lv_mmsta = '02' OR lv_mmsta = '81' OR lv_mmsta = 'Z2' OR lv_mmsta = 'Z3' OR lv_mmsta = 'Z4'.
**        c_ret_msg-type = 'E'.
**        c_ret_msg-message = '参数错误，物料' && g_matnr && '在工厂' && u_input-plant && 'BOM冻结'.
**        RETURN.
**      ENDIF.
      IF lv_mmsta = '02' OR lv_mmsta = '81' OR lv_mmsta = 'Z2' OR lv_mmsta = 'Z3' OR lv_mmsta = 'Z4'.
        IF lv_mmstd > sy-datum.
          c_ret_msg-type = 'E'.
          c_ret_msg-message = '参数错误，物料' && g_matnr && '在工厂' && u_input-plant && 'BOM冻结'.
        ENDIF.
        RETURN.
      ENDIF.
      "end
    ENDIF.

    CLEAR:lv_mmsta,lv_mmstd.
    SELECT SINGLE mstae INTO lv_mmsta
      FROM mara
      WHERE matnr = g_matnr.
    IF sy-subrc = 0.
      IF lv_mmsta = '02' OR lv_mmsta = '81' OR lv_mmsta = 'Z2' OR lv_mmsta = 'Z3' OR lv_mmsta = 'Z4'.
        c_ret_msg-type = 'E'.
        c_ret_msg-message = '参数错误，物料' && g_matnr && 'BOM冻结'.
        RETURN.
      ENDIF.
    ENDIF.

    lt_item[] = u_input-bom_item[].

    IF g_stlal <> '09'.
      READ TABLE lt_item INTO ls_item WITH KEY component = u_input-material.
      IF sy-subrc = 0.
        c_ret_msg-type = 'E'.
        c_ret_msg-message = '参数错误，物料' && u_input-material && '父物料和组件物料相同'.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT lt_item INTO ls_item.
      CLEAR lv_lines.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = ls_item-component
        IMPORTING
          output       = ls_item-component
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        c_ret_msg-type = 'E'.
        c_ret_msg-message = '参数错误，物料' && ls_item-component && '无效'.
        EXIT.
      ENDIF.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = ls_item-comp_unit
          language       = sy-langu
        IMPORTING
          output         = ls_item-comp_unit
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        c_ret_msg-type = 'E'.
        c_ret_msg-message = '参数错误，物料' && ls_item-component && '单位' && ls_item-comp_unit && '无效'.
        EXIT.
      ENDIF.
      "BOM组件的基本单位与组件单位不一样的话，如果他们属于不同的单位组，物料需提前维护组件单位
      SELECT SINGLE meins FROM mara WHERE matnr = @ls_item-component INTO @DATA(lv_meins).
      IF sy-subrc <> 0.
        c_ret_msg-type = 'E'.
        c_ret_msg-message = '参数错误，物料' && ls_item-component && '无效'.
        EXIT.
      ENDIF.
      IF lv_meins <> ls_item-comp_unit."基本单位与组件单位不一样
        SELECT dimid FROM t006 WHERE msehi IN ( @lv_meins,@ls_item-comp_unit ) INTO TABLE @DATA(lt_t006).
        lv_lines = lines( lt_t006 ).
        IF lv_lines <> 2.
          c_ret_msg-type = 'E'.
          c_ret_msg-message = '参数错误，物料' && ls_item-component && '单位' && ls_item-comp_unit && '在T006不存在'.
          EXIT.
        ELSE."基本单位与组件单位不属于同一个组，需检查物料主数据上的单位
          IF lt_t006[ 1 ]-dimid <> lt_t006[ 2 ]-dimid.
            SELECT SINGLE meinh INTO lv_meinh FROM marm
                    WHERE matnr = ls_item-component
                    AND meinh = ls_item-comp_unit.
            IF sy-subrc <> 0.
              c_ret_msg-type = 'E'.
              c_ret_msg-message = '参数错误，物料' && ls_item-component && '单位' && ls_item-comp_unit && '在MARM不存在'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      IF ls_item-issue_loc IS NOT INITIAL.
        SELECT SINGLE matnr INTO lv_matnr
        FROM mard
        WHERE matnr = ls_item-component
          AND werks = u_input-plant
          AND lgort = ls_item-issue_loc.
        IF sy-subrc <> 0.
          c_ret_msg-type = 'E'.
          c_ret_msg-message = '参数错误，物料' && ls_item-component && '工厂' && u_input-plant && '仓储地点' && ls_item-issue_loc && '在MARD不存在'.
          RETURN.
        ENDIF.
      ENDIF.

      CLEAR lv_mmsta.
      SELECT SINGLE mmsta INTO lv_mmsta
        FROM marc
        WHERE matnr = ls_item-component
          AND werks = u_input-plant.
      IF sy-subrc = 0.
        IF lv_mmsta = '02' OR lv_mmsta = '81' OR lv_mmsta = 'Z2' OR lv_mmsta = 'Z3' OR lv_mmsta = 'Z4'.
          c_ret_msg-type = 'E'.
          c_ret_msg-message = '参数错误，物料' && ls_item-component && '在工厂' && u_input-plant && 'BOM冻结'.
          RETURN.
        ENDIF.
      ENDIF.

      CLEAR lv_mmsta.
      SELECT SINGLE mstae INTO lv_mmsta
        FROM mara
        WHERE matnr = ls_item-component.
      IF sy-subrc = 0.
        IF lv_mmsta = '02' OR lv_mmsta = '81' OR lv_mmsta = 'Z2' OR lv_mmsta = 'Z3' OR lv_mmsta = 'Z4'.
          c_ret_msg-type = 'E'.
          c_ret_msg-message = '参数错误，物料' && ls_item-component && 'BOM冻结'.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_FORMULA_INPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_DATA
*&      <-- C_RET_MSG
*&---------------------------------------------------------------------*
FORM frm_check_formula_input  USING u_data    TYPE zdt_plm2sap_formula_head
                                     u_matnr TYPE mara-matnr
                                     u_plant TYPE t001w-werks
                           CHANGING c_flag
                                    c_ret_msg TYPE zdt_plm2sap_formula_ret_detail.

  DATA: lt_mapl TYPE TABLE OF mapl,
        l_lines TYPE i.

  REFRESH lt_mapl.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_mapl
    FROM mapl
    WHERE matnr = u_matnr
      AND werks = u_plant
      AND plnty = '2'
      AND loekz = ''
      .
  IF sy-subrc NE 0.
    c_flag = 'A'.
  ELSE.
    DESCRIBE TABLE lt_mapl LINES l_lines.
    IF l_lines = 1.
      c_flag = 'C'.
    ELSE.
      "研发物料只存在一个主配方
      MOVE-CORRESPONDING u_data TO c_ret_msg.
      c_ret_msg-type = 'E'.
      c_ret_msg-message = '输入错误，输入的物料存在多个版本的主配方'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHK_AND_CONV_REQ_DATA
*&---------------------------------------------------------------------*
*& 检查、转换接口传入参数
*&---------------------------------------------------------------------*
*&      --> LS_INPUT
*&      <-- LS_BOM_ABAP
*&      <-- LS_RET_MSG
*&---------------------------------------------------------------------*
FORM frm_chk_and_conv_req_data  USING    is_input TYPE zdt_plm2sap_createchange_bom_2
                                CHANGING cs_bom_abap TYPE ty_req_bom_abap
                                         cs_ret_msg TYPE zdt_plm2sap_createchange_bom_1.
  DATA:ls_bom_item_abap TYPE ty_bom_item,
       lv_tabix         TYPE sy-tabix,
       lv_error         TYPE flag,
       lv_lines         TYPE int4,
       lv_matnr         TYPE mara-matnr,
       lv_meins         TYPE mara-meins,
       lv_meinh         TYPE marm,
       lv_mmsta         TYPE marc-mmsta.
  CLEAR:cs_bom_abap.
  "1、接口标志
  TRANSLATE is_input-flag TO UPPER CASE.
  IF is_input-flag NE 'A' AND is_input-flag NE 'C' AND is_input-flag NE 'D'.
    cs_ret_msg-type = 'E'.
    cs_ret_msg-message = '参数错误，flag的值只能是A/C/D'.
    RETURN.
  ENDIF.
  cs_bom_abap-flag = is_input-flag."接口标志

  "2、工厂
  IF is_input-plant IS INITIAL.
    cs_ret_msg-type = 'E'.
    cs_ret_msg-message = '参数错误，工厂不能为空'.
    RETURN.
  ENDIF.
  SELECT SINGLE werks FROM t001w WHERE werks = @is_input-plant INTO @DATA(lv_werks).
  IF sy-subrc <> 0.
    cs_ret_msg-type = 'E'.
    cs_ret_msg-message = |{ '参数错误，工厂' }{ is_input-plant }{ '不存在' }|.
    RETURN.
  ENDIF.
  cs_bom_abap-plant = is_input-plant.

  "3、BOM抬头物料
  IF is_input-material IS INITIAL.
    cs_ret_msg-type = 'E'.
    cs_ret_msg-message = '参数错误，BOM抬头物料不能为空'.
    RETURN.
  ENDIF.
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = is_input-material
    IMPORTING
      output       = cs_bom_abap-material
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    cs_ret_msg-type = 'E'.
    cs_ret_msg-message = '参数错误，BOM抬头物料转换失败，不是有效的物料号'.
    RETURN.
  ENDIF.
  SELECT SINGLE  mara~meins,marc~mmsta INTO ( @cs_bom_abap-meins,@lv_mmsta )
     FROM mara
     INNER JOIN marc ON mara~matnr = marc~matnr
    WHERE mara~matnr = @cs_bom_abap-material AND marc~werks = @is_input-plant.
  IF sy-subrc <> 0.
    cs_ret_msg-type = 'E'.
    cs_ret_msg-message = | { '参数错误，BOM抬头物料' }{ is_input-material }{ '在工厂' }{ is_input-plant }{ '不存在' }|.
    RETURN.
  ENDIF.
  "创建或修改BOM时，检查物料的冻结状态
  IF is_input-flag = 'A' OR is_input-flag = 'C'.
    "特定工厂下的冻结状态
    IF lv_mmsta = '02' OR lv_mmsta = '81' OR lv_mmsta = 'Z2' OR lv_mmsta = 'Z3' OR lv_mmsta = 'Z4'.
      cs_ret_msg-type = 'E'.
      cs_ret_msg-message = |{ '参数错误，物料' }{ is_input-material }{ '在工厂' }{ is_input-plant }{ 'BOM冻结' }|.
      RETURN.
    ENDIF.
    "跨工厂下的冻结状态
    CLEAR lv_mmsta.
    SELECT SINGLE mstae INTO lv_mmsta FROM mara  WHERE matnr =  cs_bom_abap-material.
    IF sy-subrc = 0.
      IF lv_mmsta = '02' OR lv_mmsta = '81' OR lv_mmsta = 'Z2' OR lv_mmsta = 'Z3' OR lv_mmsta = 'Z4'.
        cs_ret_msg-type = 'E'.
        cs_ret_msg-message = |{ '参数错误，物料' }{ is_input-material }{ 'BOM冻结' }|.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  "4、备选物料清单
  IF is_input-stlal IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = is_input-stlal
      IMPORTING
        output = cs_bom_abap-stlal.
  ENDIF.

  "5、基本数量
  IF is_input-flag = 'A' AND is_input-base_qty IS INITIAL."创建BOM
    cs_ret_msg-type = 'E'.
    cs_ret_msg-message = |{ '参数错误，物料' }{ is_input-material }{ '基本数量不能空' }|.
    RETURN.
  ENDIF.
  IF is_input-base_qty IS NOT INITIAL.
    CALL FUNCTION 'UNITS_STRING_CONVERT'
      EXPORTING
        units_string = is_input-base_qty
        dcpfm        = 'X' "这个是根据su01里你的数字格式来设置，sap标准的格式有三种：空，X,Y
*       MLLN         = 'M'
*       TSND         = 'T'
      IMPORTING
        units        = cs_bom_abap-base_qty
      EXCEPTIONS
        invalid_type = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      cs_ret_msg-type = 'E'.
      cs_ret_msg-message = |{ '参数错误，物料' }{ is_input-material }{ '基本数量无效' }|.
      RETURN.
    ENDIF.
  ENDIF.

  "6、权限组
  IF is_input-flag = 'A' AND is_input-auth_group IS INITIAL."创建BOM
    cs_ret_msg-type = 'E'.
    cs_ret_msg-message =  '参数错误，权限组不能为空'.
    RETURN.
  ENDIF.
  cs_bom_abap-auth_group =  is_input-auth_group.

  "7、BOM用途
  IF is_input-bom_usage IS INITIAL.
    cs_bom_abap-bom_usage = '1'.
  ELSE.
    cs_bom_abap-bom_usage = is_input-bom_usage.
  ENDIF.

  "8、已删除标记的BOM不在进行任何操作
  SELECT SINGLE a~stlnr FROM mast AS a
  INNER JOIN stko AS b ON  a~stlnr = b~stlnr AND a~stlal = b~stlal
  WHERE a~matnr = @cs_bom_abap-material
    AND a~werks = @cs_bom_abap-plant
    AND a~stlan = @cs_bom_abap-bom_usage
    AND a~stlal =  @cs_bom_abap-stlal "备选物料清单
    AND b~stlty = 'M'
    AND b~loekz = 'X'
    INTO @DATA(lv_stlnr).
  IF sy-subrc = 0.
    cs_ret_msg-type = 'E'.
    cs_ret_msg-message =  '此BOM已做了删除标记，不允许再做任何操作'.
    RETURN.
  ENDIF.

  "BOM行项目检查
  LOOP AT is_input-bom_item INTO DATA(ls_req_bom_item).
    CLEAR:ls_bom_item_abap, lv_error,lv_tabix.
    lv_tabix = sy-tabix.
    "项目序号
    IF ls_req_bom_item-item_no IS INITIAL.
      lv_error = abap_true.
      cs_ret_msg-type = 'E'.
      cs_ret_msg-message =  |{ '参数错误，行项目第' }{ lv_tabix }{ '行项目序号为空' }|.
      EXIT.
    ENDIF.
    CALL FUNCTION 'CONVERSION_EXIT_NUMCV_INPUT'
      EXPORTING
        input  = ls_req_bom_item-item_no
      IMPORTING
        output = ls_bom_item_abap-item_no.
    "项目类别
    IF ls_req_bom_item-item_categ IS INITIAL.
      ls_bom_item_abap-item_categ = 'L'.
    ELSE.
      ls_bom_item_abap-item_categ = ls_req_bom_item-item_categ.
    ENDIF.
    "组件编码
    IF ls_req_bom_item-component IS INITIAL.
      lv_error = abap_true.
      cs_ret_msg-type = 'E'.
      cs_ret_msg-message =  |{ '参数错误，第' }{ lv_tabix }{ '行组件编码为空' }|.
      EXIT.
    ENDIF.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = ls_req_bom_item-component
      IMPORTING
        output       = ls_bom_item_abap-component
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      lv_error = abap_true.
      cs_ret_msg-type = 'E'.
      cs_ret_msg-message =  |{ '参数错误，第' }{ lv_tabix }{ '行组件编码无效' }|.
      EXIT.
    ENDIF.
    SELECT SINGLE matnr INTO lv_matnr FROM marc
    WHERE matnr = ls_bom_item_abap-component AND werks = is_input-plant AND lvorm = 'X'.
    IF sy-subrc = 0.
      lv_error = abap_true.
      cs_ret_msg-type = 'E'.
      cs_ret_msg-message =  |{ '参数错误，第' }{ ls_req_bom_item-component }{ '在' }{ is_input-plant }{ '工厂已设置了删除标识' }|.
      EXIT.
    ENDIF.
    "组件数量
    IF ls_req_bom_item-comp_qty IS NOT INITIAL.
      CALL FUNCTION 'UNITS_STRING_CONVERT'
        EXPORTING
          units_string = ls_req_bom_item-comp_qty
          dcpfm        = 'X' "这个是根据su01里你的数字格式来设置，sap标准的格式有三种：空，X,Y
*         MLLN         = 'M'
*         TSND         = 'T'
        IMPORTING
          units        = ls_bom_item_abap-comp_qty
        EXCEPTIONS
          invalid_type = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        lv_error = abap_true.
        cs_ret_msg-type = 'E'.
        cs_ret_msg-message =  |{ '参数错误，第' }{ lv_tabix }{ '行组件数量无效' }|.
        RETURN.
      ENDIF.
    ENDIF.
    "计量单位
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = ls_req_bom_item-comp_unit
        language       = sy-langu
      IMPORTING
        output         = ls_bom_item_abap-comp_unit
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      lv_error = abap_true.
      cs_ret_msg-type = 'E'.
      cs_ret_msg-message = |{ '参数错误，第' }{ lv_tabix }{ '行计量单位转换失败' }|.
      EXIT.
    ENDIF.
    "BOM组件的基本单位与组件单位不一样的话，如果他们属于不同的单位组，物料需提前维护组件单位
    SELECT SINGLE meins,matkl FROM mara WHERE matnr = @ls_bom_item_abap-component INTO ( @lv_meins,@ls_bom_item_abap-comp_matkl ).
    IF sy-subrc <> 0.
      lv_error = abap_true.
      cs_ret_msg-type = 'E'.
      cs_ret_msg-message = |{ '参数错误，第' }{ lv_tabix }{ '行组件编码在MARA中不存在' }|.
      EXIT.
    ENDIF.
    IF lv_meins <> ls_bom_item_abap-comp_unit."基本单位与组件单位不一样
      SELECT dimid FROM t006 WHERE msehi IN ( @lv_meins,@ls_bom_item_abap-comp_unit ) INTO TABLE @DATA(lt_t006).
      lv_lines = lines( lt_t006 ).
      IF lv_lines <> 2.
        lv_error = abap_true.
        cs_ret_msg-type = 'E'.
        cs_ret_msg-message = |{ '参数错误，第' }{ lv_tabix }{ '行组件编码' }{ ls_req_bom_item-component }{ '单位' }{ ls_req_bom_item-comp_unit }{ '在T006不存在' }|.
        EXIT.
      ELSE."基本单位与组件单位不属于同一个组，需检查物料主数据上的单位
        IF lt_t006[ 1 ]-dimid <> lt_t006[ 2 ]-dimid.
          SELECT SINGLE meinh INTO lv_meinh FROM marm
                  WHERE matnr = ls_bom_item_abap-component
                  AND meinh = ls_bom_item_abap-comp_unit.
          IF sy-subrc <> 0.
            lv_error = abap_true.
            cs_ret_msg-type = 'E'.
            cs_ret_msg-message = |{ '参数错误，物料' }{ ls_req_bom_item-component }{ '单位' }{ ls_req_bom_item-comp_unit }{ '在MARM不存在' }|.
            EXIT.
          ENDIF.
        ELSEIF lt_t006[ 1 ]-dimid = 'AAAADL'."无量纲组不存在换算关系
          lv_error = abap_true.
          cs_ret_msg-type = 'E'.
          cs_ret_msg-message = |{ '参数错误，物料' }{ ls_req_bom_item-component }{ '单位' }{ ls_req_bom_item-comp_unit }{ '在MARM不存在' }|.
          EXIT.
        ENDIF.
      ENDIF.
      REFRESH lt_t006.
    ENDIF.
    "生产仓储地点
    IF ls_req_bom_item-issue_loc IS NOT INITIAL.
      SELECT SINGLE matnr INTO lv_matnr FROM mard
       WHERE matnr = ls_bom_item_abap-component
      AND werks = is_input-plant
      AND lgort = ls_req_bom_item-issue_loc.
      IF sy-subrc <> 0.
        lv_error = abap_true.
        cs_ret_msg-type = 'E'.
        cs_ret_msg-message = |{ '参数错误，物料' }{ ls_req_bom_item-component }{ '工厂' }{ is_input-plant }{ '仓储地点' }{ ls_req_bom_item-issue_loc }{ '在MARD不存在' }|.
        EXIT.
      ENDIF.
    ENDIF.
    ls_bom_item_abap-issue_loc = ls_req_bom_item-issue_loc."生产仓储地点
    ls_bom_item_abap-comp_scrap = ls_req_bom_item-comp_scrap."子项废品率（%）损耗率(%)
    ls_bom_item_abap-sortstring = ls_req_bom_item-sortstring."排序字符串量
    ls_bom_item_abap-valid_from = ls_req_bom_item-valid_from."有效起始日期
    ls_bom_item_abap-ai_group = ls_req_bom_item-ai_group."替代项目组
    ls_bom_item_abap-ai_strateg = ls_req_bom_item-ai_strateg."策略
    ls_bom_item_abap-usage_prob = ls_req_bom_item-usage_prob."使用可能性
    IF ls_req_bom_item-fixed_qty IS NOT INITIAL.
      ls_bom_item_abap-fixed_qty = 'X'."固定数量
    ENDIF.
    APPEND ls_bom_item_abap TO cs_bom_abap-bom_item.
  ENDLOOP.
  "非返工BOM检查
  IF lv_error = abap_false AND cs_bom_abap-stlal <> '09'.
    READ TABLE cs_bom_abap-bom_item WITH KEY component = cs_bom_abap-material TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      cs_ret_msg-type = 'E'.
      cs_ret_msg-message = |{ '参数错误，物料' }{ is_input-material }{ '父物料和组件物料相同' }|.
      RETURN.
    ENDIF.
  ENDIF.
ENDFORM.