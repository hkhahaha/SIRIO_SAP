*&---------------------------------------------------------------------*
*& 包含               ZMMR0004_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_DISPLAY_ALV 显示alv报表
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_alv .

    cl_salv_table=>factory("全屏模式的SALV
      IMPORTING r_salv_table = gr_table
      CHANGING t_table = gt_alv ).
  
    gr_layout = gr_table->get_layout( ).
    gs_program-report = sy-repid.
    gr_layout->set_key( gs_program )."设置布局保存为变式时，这些变式应属性此Key（本程序名）
    gr_layout->set_save_restriction( cl_salv_layout=>restrict_none )."允许保存布局为变式
    gr_table->get_display_settings( )->set_list_header( '供应计划跟踪' ).
    gr_table->set_screen_status(
       pfstatus = 'STANDARD'"
       report = sy-repid
       set_functions = gr_table->c_functions_all )."显示所有通用的预设按钮
    gr_columns = gr_table->get_columns( ).
    gr_columns->set_optimize( 'X' ).
  
    PERFORM  frm_set_field.  "字段处理
  
  
    gr_table->display( )."显示ALV
  
  
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_DATA
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_get_data .
  
  
  *&---------------------------------------------------------------------*
  *  判断物料编码、物料组三个选择条件是否存在一个有填写，
  *  若有，则先通过MARA表对这两个条件进行过滤出物料编码，生成物料编码查询条件
  *&---------------------------------------------------------------------*
    IF s_matnr IS NOT INITIAL OR s_matkl IS NOT INITIAL.
      SELECT 'I' AS sign,
         'EQ' AS options ,
          matnr AS low INTO TABLE @gr_matnr
        FROM mara
        WHERE matnr IN @s_matnr
        AND matkl IN @s_matkl.
  
    ENDIF.
  
  
    SELECT  a~dlv_planno,         "计划号
            a~dlv_itemno,         "计划行号
            a~guid,
            a~ebeln,              "采购订单号
            a~ebelp,              "采购订单行号
            b~lifnr,              "供应商编号
  *         NAME1_GP              "供应商名称
            a~pdlv_date,          "答复交货日期
            a~matnr,              "物料编码
            c~maktx,              "物料描述
            a~producer,           "制造商编号
  *         PRODUCER_NAM          "制造商名称
            a~pdlv_date_ext,      "调整后交货日期
            a~meins,              "单位
  *         MSEHT                 "单位文本
            a~pdlv_menge,         "确认送货数量
  *         RECV_MENGE            "确认日期净收货量
  *         UNDLV_MENDE           "剩余未到货量
  *         delay                 "延期天数
  *         count_year            "当前交货次数
  *         MIN_DLV               "最小订购批量
            a~origin,             "计划来源
            a~xclosed
  *         responsible          "采购部负责人
  *         made_in_china        "原产地
  *         xprecious            "贵重标识
      INTO CORRESPONDING FIELDS OF TABLE @gt_alv
      FROM zmmwms_pol AS a INNER JOIN ekko AS b ON a~ebeln = b~ebeln
      INNER JOIN makt AS c ON a~matnr = c~matnr AND c~spras = @sy-langu
      WHERE werks IN @s_werks
        "AND b~ekorg = @p_ekorg
        AND b~ekgrp IN @s_ekgrp
        AND a~matnr IN @gr_matnr
        AND b~lifnr IN @s_lifnr
        "AND a~st_code = @p_code
        AND pdlv_date IN @s_dat." and dlv_planno = '0000000567'.
  
  
    DATA: lt_alv TYPE TABLE OF ty_alv.
  
    "拷贝内表gt_alv到lt_alv_x
    APPEND LINES OF gt_alv TO lt_alv.
  
    "删除关闭的记录
    DELETE gt_alv WHERE xclosed = 'X'.
  
    "删除未关闭的记录，过滤出关闭的记录
    DELETE lt_alv WHERE xclosed = ''.
  
  *----循环关闭的记录，找出已收货数量不为零的记录，把该记录插入到gt_alv内表，然后输出alv
  *       1) 通过zmatdoc表对已经收货和退货的数量行进行合计
  *       2) 找出净收货量大于零的记录
  *       3) 将该部分记录插入到gt_alv表
  *-------------------------------------------------------------------------------------
  
  
  
    "1) 通过zmatdoc表对已经收货和退货的数量行进行合计
    SELECT dlv_planno,
           dlv_itemno,
           CASE
             WHEN shkzg = 'H' THEN menge * -1
             WHEN shkzg = 'S' THEN menge
           END AS menge
      INTO TABLE @DATA(lt_zmatdoc)
      FROM zmatdoc
      WHERE shkzg <> ''.
  
    DATA: ls_zmatdoc_merge LIKE LINE OF lt_zmatdoc,
          lt_zmatdoc_merge LIKE TABLE OF ls_zmatdoc_merge.
  
    LOOP AT lt_zmatdoc INTO DATA(ls_zmatdoc).
      COLLECT ls_zmatdoc INTO lt_zmatdoc_merge.
    ENDLOOP.
  
    "2) 找出净收货量大于零的记录
    DELETE lt_zmatdoc_merge WHERE menge <= 0.
  
    "循环净收货量大于零的记录，根据匹配计划号和行号将记录插入到gt_alv内表
    LOOP AT lt_zmatdoc_merge INTO ls_zmatdoc.
      READ TABLE lt_alv INTO gs_alv WITH KEY dlv_planno = ls_zmatdoc-dlv_planno dlv_itemno = ls_zmatdoc-dlv_itemno.
      IF sy-subrc EQ 0.
        APPEND gs_alv TO gt_alv.
      ENDIF.
    ENDLOOP.
  
  ENDFORM.
  
  
  FORM frm_set_field.
  
    gr_columns->set_optimize( abap_true ). "列宽自动优化
  
    gr_columns->set_exception_column( 'LIGHT' ).
    gr_column ?= gr_columns->get_column( 'LIGHT' ).
    gr_column->set_long_text( '状态指示灯' ).
    gr_column->set_medium_text( '状态指示灯' ).
    gr_column->set_short_text( '状态指示灯' ).
  
    gr_column ?= gr_columns->get_column( 'PDLV_DATE' ).
    gr_column->set_long_text( '答复送货日期' ).
    gr_column->set_medium_text( '答复送货日期' ).
    gr_column->set_short_text( '答复送货日期' ).
  
    gr_column ?= gr_columns->get_column( 'NAME1_GP' ).
    gr_column->set_long_text( '供应商名称' ).
    gr_column->set_medium_text( '供应商名称' ).
    gr_column->set_short_text( '供应商名称' ).
  
    gr_column ?= gr_columns->get_column( 'PRODUCER_NAME' ).
    gr_column->set_long_text( '制造商名称' ).
    gr_column->set_medium_text( '制造商名称' ).
    gr_column->set_short_text( '制造商名称' ).
  
    gr_column ?= gr_columns->get_column( 'MSEHT' ).
    gr_column->set_long_text( '单位文本' ).
    gr_column->set_medium_text( '文本' ).
    gr_column->set_short_text( '文本' ).
  
    gr_column ?= gr_columns->get_column( 'PDLV_MENGE' ).
    gr_column->set_long_text( '答复送货数量' ).
    gr_column->set_medium_text( '送货数量' ).
    gr_column->set_short_text( '数量' ).
  
  
  
    gr_column ?= gr_columns->get_column( 'RECV_MENGE' ).
    gr_column->set_long_text( '确认日期净收货量' ).
    gr_column->set_medium_text( '净收货量' ).
    gr_column->set_short_text( '净收货量' ).
  
    gr_column ?= gr_columns->get_column( 'UNDLV_MENDE' ).
    gr_column->set_long_text( '剩余未到货量' ).
    gr_column->set_medium_text( '剩余未到货量' ).
    gr_column->set_short_text( '未到货量' ).
  
    gr_column ?= gr_columns->get_column( 'DELAY' ).
    gr_column->set_long_text( '延期天数' ).
    gr_column->set_medium_text( '延期天数' ).
    gr_column->set_short_text( '延期天数' ).
  
    gr_column ?= gr_columns->get_column( 'COUNT_YEAR' ).
    gr_column->set_long_text( '当前交货次数' ).
    gr_column->set_medium_text( '交货次数' ).
    gr_column->set_short_text( '次数' ).
  
    gr_columns->get_column( 'ORIGIN' )->set_technical( abap_true ).
    gr_column ?= gr_columns->get_column( 'ORIGIN_TXT' ).
    gr_column->set_long_text( '计划来源' ).
    gr_column->set_medium_text( '来源' ).
    gr_column->set_short_text( '来源' ).
  
  
    gr_column ?= gr_columns->get_column( 'RESPONSIBLE' ).
    gr_column->set_long_text( '采购负责人' ).
    gr_column->set_medium_text( '负责人' ).
    gr_column->set_short_text( '采购' ).
  
    gr_column ?= gr_columns->get_column( 'MADE_IN_CHINA' ).
    gr_column->set_long_text( '原产地' ).
    gr_column->set_medium_text( '原产地' ).
    gr_column->set_short_text( '原产地' ).
  
    gr_column ?= gr_columns->get_column( 'XPRECIOUS' ).
    gr_column->set_long_text( '贵重标识' ).
    gr_column->set_medium_text( '贵重' ).
    gr_column->set_short_text( '贵重' ).
  
  
  *  gr_column ?= gr_columns->get_column( 'QYZT' )."不显示字段
  *  gr_column->set_technical( 'X' ).
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_AUTHORITY_CHECK
  *&---------------------------------------------------------------------*
  *& text 检查权限
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_authority_check .
  
    "调用外部的form ZCA_AUTHORITY_CHECK
    PERFORM frm_check_m_best_wrk TABLES s_werks.
    PERFORM frm_check_m_best_ekg TABLES s_ekgrp.
  
  
  ENDFORM.
  **&---------------------------------------------------------------------*
  **& Form FRM_CHECK_M_BEST_WRK
  **&---------------------------------------------------------------------*
  **& text 检查工厂权限
  **&---------------------------------------------------------------------*
  **&      --> S_WERKS
  **&---------------------------------------------------------------------*
  *FORM frm_check_m_best_wrk  TABLES   p_s_werks STRUCTURE s_werks.
  *
  *ENDFORM.
  **&---------------------------------------------------------------------*
  **& Form FRM_CHECK_M_BEST_EKG
  **&---------------------------------------------------------------------*
  **& text 检查采购组权限
  **&---------------------------------------------------------------------*
  **&      --> S_EKGRP
  **&---------------------------------------------------------------------*
  *FORM frm_check_m_best_ekg  TABLES   p_s_ekgrp STRUCTURE s_ekgrp.
  *
  *ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_PROCESS_DATA
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_process_data .
  
  
    DATA lv_dat TYPE dats. "最新的答复送货日期
  
    "获取匹配临时表表的数据，过滤出与gt_alv表中计划号和行号相同的记录
    SELECT dlv_planno, dlv_itemno, guid, reqdate
      FROM zmmaps_temp AS t
      INTO CORRESPONDING FIELDS OF TABLE @gt_aps
      FOR ALL ENTRIES IN @gt_alv
      WHERE dlv_planno ne ''
      and dlv_itemno ne ''
      and dlv_planno = @gt_alv-dlv_planno
      and dlv_itemno = @gt_alv-dlv_itemno.
  
    "获取供应商
    SELECT a~lifnr,
           a~name1
      INTO TABLE @DATA(lt_lifnr)
      FROM lfa1 AS a
      FOR ALL ENTRIES IN @gt_alv
      WHERE a~lifnr = @gt_alv-lifnr OR a~lifnr = @gt_alv-producer.
  
    "获取单位文本
    SELECT msehi,
           mseht
      INTO TABLE @DATA(lt_meins)
      FROM t006a
      FOR ALL ENTRIES IN @gt_alv
      WHERE msehi = @gt_alv-meins AND spras = @sy-langu.
  
    "获取最小批量和物料组
    SELECT a~matnr,
           a~bstmi,
           b~matkl
      INTO TABLE @DATA(lt_bstmi)
      FROM marc AS a INNER JOIN mara AS b ON a~matnr = b~matnr
      FOR ALL ENTRIES IN @gt_alv
      WHERE a~matnr = @gt_alv-matnr.
  
  
  
    SELECT dlv_planno,
           dlv_itemno,
           CASE
             WHEN shkzg = 'S' THEN menge
             WHEN shkzg = 'H' THEN -1 * menge
           END AS menge
      INTO TABLE @DATA(lt_zmatdoc)
      FROM zmatdoc
      WHERE shkzg <> ''.
  
    DATA: ls_zmatdoc_merge LIKE LINE OF lt_zmatdoc,
          lt_zmatdoc_merge LIKE TABLE OF ls_zmatdoc_merge.
  
  
  
    LOOP AT lt_zmatdoc INTO DATA(ls_zmatdoc).
      COLLECT ls_zmatdoc INTO lt_zmatdoc_merge.
    ENDLOOP.
  
    DATA lt_bwart_merge_ret TYPE TABLE OF ty_bwart_merge.
    PERFORM frm_count_year TABLES lt_bwart_merge_ret.
  
    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).
  
  
      "供应商名称
      READ TABLE lt_lifnr INTO DATA(ls_lifnr) WITH KEY lifnr = <fs_alv>-lifnr.
      IF sy-subrc EQ 0.
        <fs_alv>-name1_gp = ls_lifnr-name1.
      ENDIF.
  
      "预计到货天数   答复送货日期/调整后的日期-当前日期
      PERFORM frm_get_delivery_days USING <fs_alv> CHANGING <fs_alv>-delivery_days.
  
      "制造商名称
      READ TABLE lt_lifnr INTO ls_lifnr WITH KEY lifnr = <fs_alv>-producer.
      IF sy-subrc EQ 0.
        <fs_alv>-producer_name = ls_lifnr-name1.
      ENDIF.
  
      "APS需求日期
      READ TABLE gt_aps into data(ls_aps) with key dlv_planno = <fs_alv>-dlv_planno dlv_itemno = <fs_alv>-dlv_itemno.
      IF sy-subrc eq 0.
        <fs_alv>-aps_req_date = ls_aps-reqdate.
      ENDIF.
  
      "实际到货日期，应该去首次到货日期还是最后一次到货日期
  
      "APS需求量
  
  
  
  
  
      "计划来源
      IF <fs_alv>-origin = 'X'.
        <fs_alv>-origin_txt = 'APS'.
      ELSEIF <fs_alv>-origin = 'M'.
        <fs_alv>-origin_txt = '按单'.
        "按单情况下，调整后的送货日期设置为空
        <fs_alv>-pdlv_date_ext = ''.
      ENDIF.
  
      "单位文本
      READ TABLE lt_meins INTO DATA(ls_meins) WITH KEY msehi = <fs_alv>-meins.
      IF sy-subrc EQ 0.
        <fs_alv>-mseht = ls_meins-mseht.
      ENDIF.
  
      "净收货量 = 已收货量-退货量
      READ TABLE lt_zmatdoc_merge INTO DATA(ls_merge) WITH KEY dlv_planno = <fs_alv>-dlv_planno dlv_itemno = <fs_alv>-dlv_itemno.
      IF sy-subrc EQ 0.
        <fs_alv>-recv_menge = ls_merge-menge.
      ENDIF.
  
      "剩余未到货量 = 计划送货量-已收货量
      READ TABLE lt_zmatdoc INTO ls_zmatdoc WITH KEY dlv_planno = <fs_alv>-dlv_planno dlv_itemno = <fs_alv>-dlv_itemno.
      IF sy-subrc EQ 0.
        <fs_alv>-undlv_mende = <fs_alv>-pdlv_menge - ls_merge-menge.
      ELSE.
        <fs_alv>-undlv_mende = <fs_alv>-pdlv_menge.
      ENDIF.
  
      "延期天数
      IF <fs_alv>-undlv_mende > 0.
  
  
  
        IF lv_dat < sy-datum.
  
          CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
            EXPORTING
              i_date_from    = lv_dat
              i_date_to      = sy-datum
              i_flg_separate = ''
            IMPORTING
              e_days         = <fs_alv>-delay.
        ENDIF.
      ENDIF.
  
      "指示灯
      IF <fs_alv>-delay > 0.
        <fs_alv>-light = '1'.
      ELSEIF <fs_alv>-pdlv_date < sy-datum.
        <fs_alv>-light = '2'.
      ELSE.
        <fs_alv>-light = '3'.
      ENDIF.
  
  
      "交货次数
      READ TABLE lt_bwart_merge_ret INTO DATA(ls_bwart_merge_ret) WITH KEY dlv_planno = <fs_alv>-dlv_planno dlv_itemno = <fs_alv>-dlv_itemno.
      IF sy-subrc EQ 0.
        <fs_alv>-count_year = ls_bwart_merge_ret-num.
      ENDIF.
  
      "最小批量
      READ TABLE lt_bstmi INTO DATA(ls_bstmi) WITH KEY matnr = <fs_alv>-matnr.
      IF sy-subrc EQ 0.
        <fs_alv>-min_dlv = ls_bstmi-bstmi.
        <fs_alv>-matkl = ls_bstmi-matkl.
      ENDIF.
  
      "分类特性
      DATA(lt_value) = zcl_bc_public=>get_class_values( key = <fs_alv>-matnr objtable = 'MARA' classnum = <fs_alv>-matkl classtype = '001' ).
      READ TABLE lt_value INTO DATA(ls_value) WITH KEY charact = 'ZPURRES'."采购负责人
      IF sy-subrc EQ 0.
        <fs_alv>-responsible = ls_value-value_char.
      ENDIF.
  
      READ TABLE lt_value INTO ls_value WITH KEY charact = 'ZORIADD'."原产地
      IF sy-subrc EQ 0.
        <fs_alv>-made_in_china = ls_value-value_char.
      ENDIF.
  
      READ TABLE lt_value INTO ls_value WITH KEY charact = 'ZEXPMAT'."贵重标识
      IF sy-subrc EQ 0.
        <fs_alv>-xprecious = ls_value-value_char.
      ENDIF.
  
    ENDLOOP.
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_COUNT_YEAR
  *&---------------------------------------------------------------------*
  *& text 计算交货次数
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_count_year TABLES lt_bwart_merge_ret .
  
    DATA: BEGIN OF ls_bwart,
            dlv_planno TYPE zmmwms_pol-dlv_planno,
            dlv_itemno TYPE zmmwms_pol-dlv_itemno,
            bwart      TYPE bwart,
            num        TYPE i VALUE 1,
          END OF ls_bwart.
  
    DATA: lt_bwart       LIKE TABLE OF ls_bwart,
          ls_bwart_merge TYPE ty_bwart_merge,
          lt_bwart_merge TYPE TABLE OF ty_bwart_merge.
  
    "通过zmatdoc表中的物料凭证号关联mseg找出移动类型，统计每个匹配计划行号的101次数减去102次数，得出交货次数
    SELECT a~dlv_planno,
           a~dlv_itemno,
  *         CASE
  *           WHEN b~bwart = '101' then 1
  *           WHEN b~bwart = '102' then -1
  *         END as num
            b~bwart
      INTO CORRESPONDING FIELDS OF TABLE @lt_bwart
      FROM zmatdoc AS a INNER JOIN mseg AS b ON a~mblnr = b~mblnr
      FOR ALL ENTRIES IN @gt_alv
      WHERE a~dlv_planno = @gt_alv-dlv_planno
        AND a~dlv_itemno = @gt_alv-dlv_itemno.
  
    LOOP AT lt_bwart ASSIGNING FIELD-SYMBOL(<fs_bwart>).
      IF <fs_bwart>-bwart = '101'.
        <fs_bwart>-num = 1.
      ENDIF.
      IF <fs_bwart>-bwart = '102'.
        <fs_bwart>-num = -1.
      ENDIF.
      APPEND  VALUE #( dlv_planno = <fs_bwart>-dlv_planno dlv_itemno = <fs_bwart>-dlv_itemno num = <fs_bwart>-num )  TO  lt_bwart_merge.
      "lt_bwart_merge = VALUE #( BASE lt_bwart_merge ( dlv_planno = ''  dlv_itemno = ''  num = '' ) ).
    ENDLOOP.
  
    LOOP AT lt_bwart_merge ASSIGNING FIELD-SYMBOL(<fs_bwart_ret>).
      COLLECT <fs_bwart_ret> INTO lt_bwart_merge_ret.
    ENDLOOP.
  
  ENDFORM.
  
  
  FORM frm_init_list .
    "下拉框代码实现
    DATA :list  TYPE vrm_values , "list box的值列表
          value LIKE LINE OF list . "list box的结构
  
    value-key = '1'.
    value-text = '未关闭'.
    APPEND value TO list .
  
    value-key = '2'.
    value-text = '已关闭'.
    APPEND value TO list .
  
  
  
    "调用函数展示
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = 'p_closed'
        values = list.
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_DLV_DATE
  *&---------------------------------------------------------------------*
  *& text 获取最新的答复送货日期
  *&---------------------------------------------------------------------*
  *&      --> LV_DAT
  *&---------------------------------------------------------------------*
  FORM frm_get_dlv_date  USING ls_alv TYPE ty_alv
        CHANGING   p_lv_dat TYPE dats.
    IF ls_alv-pdlv_date_ext IS NOT INITIAL AND ls_alv-pdlv_date_ext NE '        '.
      p_lv_dat = ls_alv-pdlv_date_ext.
    ELSE.
      p_lv_dat = ls_alv-pdlv_date.
    ENDIF.
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_DELIVERY_DAYS
  *&---------------------------------------------------------------------*
  *& text 计算预计送货天数
  *&---------------------------------------------------------------------*
  *&      --> <FS_ALV>
  *&---------------------------------------------------------------------*
  FORM frm_get_delivery_days  USING u_fs_alv CHANGING c_days TYPE i.
    DATA lv_date TYPE dats.
    "获取答复送货日期，如果有调整，取调整后的日期
    PERFORM frm_get_dlv_date USING u_fs_alv CHANGING lv_date.
  
    IF lv_date GE sy-datum.
      "计算当前日期与送货日期的相差天数
      PERFORM frm_get_days_count USING sy-datum lv_date CHANGING c_days.
    ELSE.
      c_days = 0.
    ENDIF.
  
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_DELIVERY_DAYS
  *&---------------------------------------------------------------------*
  *& text 计算u_date_from与u_date_to相差天数
  *&---------------------------------------------------------------------*
  *&      --> <FS_ALV>
  *&---------------------------------------------------------------------*
  FORM frm_get_days_count USING u_date_from u_date_to
                          CHANGING lv_days TYPE i.
    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        i_date_from    = u_date_from
        i_date_to      = u_date_to
        i_flg_separate = ''
      IMPORTING
        e_days         = lv_days.
  
  ENDFORM.