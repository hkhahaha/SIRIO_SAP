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
    CLEAR lv_mmsta.
    SELECT SINGLE mmsta INTO lv_mmsta
      FROM marc
      WHERE matnr = g_matnr
        AND werks = u_input-plant.
    IF sy-subrc <> 0.
      c_ret_msg-type = 'E'.
      c_ret_msg-message = '参数错误，物料' && g_matnr && '工厂' && u_input-plant && '在MARC不存在'.
      RETURN.
    ELSE.
      IF lv_mmsta = '02' OR lv_mmsta = '81' OR lv_mmsta = 'Z2' OR lv_mmsta = 'Z3' OR lv_mmsta = 'Z4'.
        c_ret_msg-type = 'E'.
        c_ret_msg-message = '参数错误，物料' && g_matnr && '在工厂' && u_input-plant && 'BOM冻结'.
        RETURN.
      ENDIF.
    ENDIF.

    CLEAR lv_mmsta.
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
              exit.
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