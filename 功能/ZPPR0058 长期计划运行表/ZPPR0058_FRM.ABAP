*&---------------------------------------------------------------------*
*& 包含               ZPPR0058_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form D0100_USER_COMMAND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM d0100_user_command .
    gv_savecode = gv_okcode.
    CLEAR  gv_okcode.
    CASE  gv_savecode.
      WHEN 'BACK'.
        PERFORM exit_program.
      WHEN 'EXIT'.
        PERFORM exit_program.
      WHEN 'CANC'.
        PERFORM exit_program.
      WHEN OTHERS.
        EXIT.
    ENDCASE.
  ENDFORM.
  FORM exit_program .
    CLEAR:gv_savecode, gv_okcode.
    IF go_con_pir_gross IS NOT INITIAL.
      go_con_pir_gross->free( ).
    ENDIF.
    IF go_con_pir_raw_gross  IS NOT INITIAL.
      go_con_pir_raw_gross->free( ).
    ENDIF.
    IF go_con_pir_net  IS NOT INITIAL.
      go_con_pir_net->free( ).
    ENDIF.
    IF go_con_pir_raw_net  IS NOT INITIAL.
      go_con_pir_raw_net->free( ).
    ENDIF.
    IF go_con_warehouse  IS NOT INITIAL.
      go_con_warehouse->free( ).
    ENDIF.
  
    IF go_salv_table_pir_raw_gross  IS NOT INITIAL.
      FREE go_salv_table_pir_raw_gross .
    ENDIF.
    IF go_salv_table_pir_gross IS NOT INITIAL.
      FREE go_salv_table_pir_gross.
    ENDIF.
    IF go_salv_table_pir_net IS NOT INITIAL.
      FREE go_salv_table_pir_net.
    ENDIF.
    IF go_salv_table_pir_raw_net IS NOT INITIAL.
      FREE go_salv_table_pir_raw_net.
    ENDIF.
    LEAVE TO SCREEN 0.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form DISPLAY_PIR_ALV
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_display_pir_gross_alv .
    IF go_con_pir_gross IS NOT BOUND.
      CREATE OBJECT go_con_pir_gross
        EXPORTING
          container_name = 'CON_PIR_GROSS'.
  *    TRY.
      "1.创建SALV对象
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          list_display = if_salv_c_bool_sap=>false
          r_container  = go_con_pir_gross
        " container_name = 'dockingcon1'
        IMPORTING
          r_salv_table = go_salv_table_pir_gross
        CHANGING
          t_table      = gt_pir_gross.
      "2.保存布局设置
      go_salv_table_pir_gross->get_layout( )->set_key( VALUE salv_s_layout_key( report = sy-repid ) ).
      go_salv_table_pir_gross->get_layout( )->set_save_restriction( cl_salv_layout=>restrict_none ).
      "go_salv_table_pir_gross->get_display_settings( )->set_list_header( '选中要执行LTP的行，未选中任何行即表示全部执行' )."设置标题
      "3.字段目录属性设置
      go_salv_columns =  go_salv_table_pir_gross->get_columns( ).
      go_salv_columns->set_optimize( abap_true ). "列宽自动优化
      PERFORM: frm_set_col_text USING 'BZTXT' '二级部门',
               frm_set_col_text USING 'NAME_ORG1' '业务员',
               frm_set_col_text USING 'BU_SORT1' '客户简称'.
  
      "设置12个月标签
      PERFORM frm_set_ym_desc.
      "4.选择模式
      go_salv_table_pir_gross->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column )."行与字段的选择模式，报表左边追加了可以选择行的MARK按钮
      "5.设置ALV工具栏
      go_salv_table_pir_gross->get_functions( )->set_all( abap_true ).
      "6.显示SALV
      go_salv_table_pir_gross->display( ).
  *     CATCH cx_salv_msg .
  *    ENDTRY.
    ENDIF.
    "go_salv_table_pir_gross->display( ).
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form DISPLAY_LTP_ALV
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_display_pir_raw_gross_alv.
    "DATA lv_field_desc TYPE scrtext_s.
    IF go_con_pir_raw_gross  IS NOT BOUND.
      CREATE OBJECT go_con_pir_raw_gross
        EXPORTING
          container_name = 'CON_PIR_RAW_GROSS'.
  *    TRY.
      "1.创建SALV对象
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          list_display = if_salv_c_bool_sap=>false
          r_container  = go_con_pir_raw_gross
        " container_name = 'dockingcon1'
        IMPORTING
          r_salv_table = go_salv_table_pir_raw_gross
        CHANGING
          t_table      = gt_pir_raw_gross.
      "2.保存布局设置
      go_salv_table_pir_raw_gross->get_layout( )->set_key( VALUE salv_s_layout_key( report = sy-repid ) ).
      go_salv_table_pir_raw_gross->get_layout( )->set_save_restriction( cl_salv_layout=>restrict_none ).
      "go_salv_table_pir_gross->get_display_settings( )->set_list_header( '选中要执行LTP的行，未选中任何行即表示全部执行' )."设置标题
      "3.字段目录属性设置
      go_salv_columns =  go_salv_table_pir_raw_gross->get_columns( ).
      go_salv_columns->set_optimize( abap_true ). "列宽自动优化
      PERFORM: frm_set_col_text USING 'ZID' '序号',
               frm_set_col_text USING 'BZTXT' '二级部门',
               frm_set_col_text USING 'IDNRK' '组件物料编号',
               frm_set_col_text USING 'OJTXP' '组件物料描述',
               frm_set_col_text USING 'MMEIN' '组件基本单位',
               frm_set_col_text USING 'MATNR' '产品编号',
               frm_set_col_text USING 'MAKTX' '产品描述',
               frm_set_col_text USING 'NAME_ORG1' '业务员',
               frm_set_col_text USING 'BU_SORT1' '客户简称'.
      "设置12个月标签
      PERFORM frm_set_ym_desc.
      "4.选择模式
      go_salv_table_pir_raw_gross->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column )."行与字段的选择模式，报表左边追加了可以选择行的MARK按钮
      "5.设置ALV工具栏
      go_salv_table_pir_raw_gross->get_functions( )->set_all( abap_true ).
  *     CATCH cx_salv_msg .
  *    ENDTRY.
    ENDIF.
    "6.显示SALV
    go_salv_table_pir_raw_gross->display( ).
  ENDFORM.
  FORM frm_set_ym_desc.
    PERFORM:  frm_set_col_text USING 'YM1' gs_ymfield_desc-ym1,
              frm_set_col_text USING 'YM2' gs_ymfield_desc-ym2,
              frm_set_col_text USING 'YM3' gs_ymfield_desc-ym3,
              frm_set_col_text USING 'YM4' gs_ymfield_desc-ym4,
              frm_set_col_text USING 'YM5' gs_ymfield_desc-ym5,
              frm_set_col_text USING 'YM6' gs_ymfield_desc-ym6,
              frm_set_col_text USING 'YM7' gs_ymfield_desc-ym7,
              frm_set_col_text USING 'YM8' gs_ymfield_desc-ym8,
              frm_set_col_text USING 'YM9' gs_ymfield_desc-ym9,
              frm_set_col_text USING 'YM10' gs_ymfield_desc-ym10,
              frm_set_col_text USING 'YM11' gs_ymfield_desc-ym11,
              frm_set_col_text USING 'YM12' gs_ymfield_desc-ym12.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_EXEC_LTP
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_calc_pir_raw_gross.
    DATA:ls_resp_header TYPE zpps_bom_resp_header,
         lt_resp_item   TYPE TABLE OF zpps_bom_resp_item,
         lv_bmeng       TYPE basmn, "基本数量
         "lv_plnmg       TYPE plnmg, "BOM项目组件数量
         "lv_plnmg(7)    TYPE p DECIMALS 6,
         lv_plnmg1(7)   TYPE p DECIMALS 6,
         lv_plnmg2(7)   TYPE p DECIMALS 6,
         lv_index       TYPE int4.
  
    CLEAR gs_bapiret.
    REFRESH:gt_req,gt_resp_header,gt_resp_item.
    MOVE-CORRESPONDING gt_pir_gross TO gt_req.
    SORT gt_req BY werks matnr.
    DELETE ADJACENT DUPLICATES FROM gt_req COMPARING werks matnr.
    LOOP AT gt_req ASSIGNING FIELD-SYMBOL(<fs_req>).
      <fs_req>-zblx = '1'.
      <fs_req>-zbzkbs = '1'.
    ENDLOOP.
    CALL FUNCTION 'Z_PP_BOM_QUERY'
      EXPORTING
        iv_aumng          = 0 "报废数量:0 计算包含装配报废率,1不包含
      IMPORTING
        es_bapiret        = gs_bapiret
      TABLES
        it_req            = gt_req
        it_resp_header    = gt_resp_header
        it_resp_item      = gt_resp_item
      EXCEPTIONS
        invalid_req_paras = 1
        OTHERS            = 2.
    "显示PIR展开BOM后的Z010、Z020物料相关需求
    REFRESH gt_pir_raw_gross.
    LOOP AT gt_pir_gross INTO gs_pir_gross.
      CLEAR:lv_index,gs_pir_raw_gross.
      REFRESH lt_resp_item.
      lv_index = sy-tabix.
      MOVE-CORRESPONDING gs_pir_gross TO gs_pir_raw_gross.
      gs_pir_raw_gross-zid = lv_index.
      APPEND gs_pir_raw_gross TO gt_pir_raw_gross.
      READ TABLE gt_resp_header WITH KEY matnr = gs_pir_gross-matnr werks = gs_pir_gross-werks INTO ls_resp_header.
      IF sy-subrc = 0.
        lt_resp_item = FILTER #( gt_resp_item USING KEY bom_zid WHERE zid  = ls_resp_header-zid ).
        IF lt_resp_item IS NOT INITIAL.
          SELECT mara~matnr,marc~werks,mara~mtart,marc~plifz,marc~webaz
            FROM mara JOIN marc ON mara~matnr = marc~matnr
            FOR ALL ENTRIES IN @lt_resp_item
            WHERE mara~matnr = @lt_resp_item-idnrk
            AND marc~werks = @ls_resp_header-werks
            INTO TABLE @DATA(lt_marc).
          SORT lt_marc BY matnr werks.
        ENDIF.
        "追加Z010、Z020物料类型的需求数量
        lv_bmeng = CONV #( ls_resp_header-bmeng )."BOM抬头基本数量
        LOOP AT lt_resp_item INTO DATA(ls_resp_item).
          READ TABLE lt_marc WITH KEY matnr = ls_resp_item-idnrk werks = ls_resp_header-werks INTO DATA(ls_marc).
          CHECK sy-subrc = 0 AND ( ls_marc-mtart = 'Z010' OR ls_marc-mtart = 'Z020' ).
          gs_pir_raw_gross-idnrk = ls_resp_item-idnrk.
          gs_pir_raw_gross-ojtxp = ls_resp_item-ojtxp.
          gs_pir_raw_gross-mmein = ls_resp_item-mmein.
          gs_pir_raw_gross-plifz = ls_marc-plifz."计划交货时间
          gs_pir_raw_gross-webaz = ls_marc-webaz."以天计的收货处理时间
          lv_plnmg1 = ls_resp_item-mnglg."标准数量-避免数量太小，丢失结果
          lv_plnmg2 = ls_resp_item-zmnglg."固定数量-避免数量太小，丢失结果
          "lv_plnmg = lv_plnmg1 + lv_plnmg2."避免数量太小，丢失结果
          "lv_plnmg = CONV plnmg( ls_resp_item-mnglg ) + CONV plnmg( ls_resp_item-zmnglg ).
          gs_pir_raw_gross-ym1 = gs_pir_gross-ym1 / lv_bmeng * lv_plnmg1.
          gs_pir_raw_gross-ym2 = gs_pir_gross-ym2 / lv_bmeng * lv_plnmg1.
          gs_pir_raw_gross-ym3 = gs_pir_gross-ym3 / lv_bmeng * lv_plnmg1.
          gs_pir_raw_gross-ym4 = gs_pir_gross-ym4 / lv_bmeng * lv_plnmg1.
          gs_pir_raw_gross-ym5 = gs_pir_gross-ym5 / lv_bmeng * lv_plnmg1.
          gs_pir_raw_gross-ym6 = gs_pir_gross-ym6 / lv_bmeng * lv_plnmg1.
          gs_pir_raw_gross-ym7 = gs_pir_gross-ym7 / lv_bmeng * lv_plnmg1.
          gs_pir_raw_gross-ym8 = gs_pir_gross-ym8 / lv_bmeng * lv_plnmg1.
          gs_pir_raw_gross-ym9 = gs_pir_gross-ym9 / lv_bmeng * lv_plnmg1.
          gs_pir_raw_gross-ym10 = gs_pir_gross-ym10 / lv_bmeng * lv_plnmg1.
          gs_pir_raw_gross-ym11 = gs_pir_gross-ym11 / lv_bmeng * lv_plnmg1.
          gs_pir_raw_gross-ym12 = gs_pir_gross-ym12 / lv_bmeng * lv_plnmg1.
          "添加固定数量
          gs_pir_raw_gross-ym1 = COND #( WHEN gs_pir_raw_gross-ym1 > 0 THEN gs_pir_raw_gross-ym1 + lv_plnmg2 ELSE gs_pir_raw_gross-ym1 ).
          gs_pir_raw_gross-ym2 = COND #( WHEN gs_pir_raw_gross-ym2 > 0 THEN gs_pir_raw_gross-ym2 + lv_plnmg2 ELSE gs_pir_raw_gross-ym2 ).
          gs_pir_raw_gross-ym3 = COND #( WHEN gs_pir_raw_gross-ym3 > 0 THEN gs_pir_raw_gross-ym3 + lv_plnmg2 ELSE gs_pir_raw_gross-ym3 ).
          gs_pir_raw_gross-ym4 = COND #( WHEN gs_pir_raw_gross-ym4 > 0 THEN gs_pir_raw_gross-ym4 + lv_plnmg2 ELSE gs_pir_raw_gross-ym4 ).
          gs_pir_raw_gross-ym5 = COND #( WHEN gs_pir_raw_gross-ym5 > 0 THEN gs_pir_raw_gross-ym5 + lv_plnmg2 ELSE gs_pir_raw_gross-ym5 ).
          gs_pir_raw_gross-ym6 = COND #( WHEN gs_pir_raw_gross-ym6 > 0 THEN gs_pir_raw_gross-ym6 + lv_plnmg2 ELSE gs_pir_raw_gross-ym6 ).
          gs_pir_raw_gross-ym7 = COND #( WHEN gs_pir_raw_gross-ym7 > 0 THEN gs_pir_raw_gross-ym7 + lv_plnmg2 ELSE gs_pir_raw_gross-ym7 ).
          gs_pir_raw_gross-ym8 = COND #( WHEN gs_pir_raw_gross-ym8 > 0 THEN gs_pir_raw_gross-ym8 + lv_plnmg2 ELSE gs_pir_raw_gross-ym8 ).
          gs_pir_raw_gross-ym9 = COND #( WHEN gs_pir_raw_gross-ym9 > 0 THEN gs_pir_raw_gross-ym9 + lv_plnmg2 ELSE gs_pir_raw_gross-ym9 ).
          gs_pir_raw_gross-ym10 = COND #( WHEN gs_pir_raw_gross-ym10 > 0 THEN gs_pir_raw_gross-ym10 + lv_plnmg2 ELSE gs_pir_raw_gross-ym10 ).
          gs_pir_raw_gross-ym11 = COND #( WHEN gs_pir_raw_gross-ym11 > 0 THEN gs_pir_raw_gross-ym11 + lv_plnmg2 ELSE gs_pir_raw_gross-ym11 ).
          gs_pir_raw_gross-ym12 = COND #( WHEN gs_pir_raw_gross-ym12 > 0 THEN gs_pir_raw_gross-ym12 + lv_plnmg2 ELSE gs_pir_raw_gross-ym12 ).
          APPEND gs_pir_raw_gross TO gt_pir_raw_gross.
        ENDLOOP.
        REFRESH lt_marc.
      ENDIF.
    ENDLOOP.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_PIR
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_get_pir .
    DATA:lv_field_name TYPE char30,
         lt_matnr      LIKE TABLE OF gs_matnr.
    FIELD-SYMBOLS <fs_plnmg> TYPE plnmg.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50     "百分比的数值，用来控制秒表的指针。
        text       = '正在查询'.   "状态栏显示的文本。
    SELECT pbim~werks, pbim~matnr, pbim~versb,pbim~pbdnr,left( pbed~pdatu,6 ) AS perxx,SUM( pbed~plnmg ) AS plnmg, pbed~meins
      FROM pbim INNER JOIN pbed ON pbim~bdzei = pbed~bdzei
      WHERE matnr IN @s_matnr "物料编号
      AND pbim~werks = @p_werks "工厂
      AND pbim~pbdnr IN @s_pbdnr "需求计划
      AND pbim~loevr = '' "删除标志
      AND pbed~pdatu IN @gr_pdatu "交货/订单完成日期
      AND pbed~loevr = '' "删除标志
      GROUP BY pbim~werks, pbim~matnr, pbim~versb,pbim~pbdnr,left( pbed~pdatu,6 ),pbed~meins
      ORDER BY pbim~werks, pbim~matnr, pbim~versb,pbim~pbdnr,perxx
      INTO TABLE @DATA(lt_pbrecord).
    IF lt_pbrecord IS  INITIAL.
      MESSAGE s000 WITH '未查询到数据'.
      LEAVE TO LIST-PROCESSING."返回到选择屏幕
    ENDIF.
    MOVE-CORRESPONDING lt_pbrecord TO lt_matnr.
    SORT lt_matnr BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_matnr COMPARING matnr.
    LOOP AT lt_matnr ASSIGNING FIELD-SYMBOL(<fs_matnr>).
      <fs_matnr>-objek = CONV cuobn( <fs_matnr>-matnr ).
    ENDLOOP.
    "物料描述、物料分类001类型-包装规格
    IF lt_matnr IS NOT INITIAL .
      SELECT matnr,maktx FROM makt FOR ALL ENTRIES IN @lt_matnr
        WHERE matnr = @lt_matnr-matnr
        AND spras = @sy-langu
        INTO TABLE @DATA(lt_makt).
      SORT lt_makt BY matnr.
      "从AUSP读取物料001分类特征值
      SELECT SINGLE atinn FROM cabn WHERE atnam = 'ZPSPE' INTO @DATA(lv_atinn).
      IF sy-subrc = 0.
        SELECT m~matnr,ausp~atwrt FROM @lt_matnr AS m
          INNER JOIN ausp ON ausp~objek = m~objek
          WHERE ausp~atinn = @lv_atinn
          AND ausp~klart = '001' "物料分类
          INTO TABLE @DATA(lt_ausp).
        SORT lt_ausp BY matnr.
      ENDIF.
    ENDIF.
    "客户简称、二级部门、业务员
    SELECT p~pbdnr,p~werks,z15~kunnr,p~matnr FROM @lt_pbrecord AS p
      INNER JOIN zsdt0015 AS z15 ON p~pbdnr = z15~pbdnr
      AND  p~werks = z15~vkorg
      AND  p~matnr = z15~matnr
      INTO TABLE @DATA(lt_sd015).
    IF lt_sd015 IS NOT INITIAL.
      DATA(lt_sd015_temp) = lt_sd015.
      SORT lt_sd015_temp BY werks kunnr.
      DELETE ADJACENT DUPLICATES FROM lt_sd015_temp COMPARING werks kunnr.
      "客户简称
      SELECT k1~kunnr,b1~bu_sort1
         FROM kna1 AS k1
        INNER JOIN but000 AS b1 ON b1~partner = k1~kunnr
        FOR ALL ENTRIES IN @lt_sd015_temp
        WHERE k1~kunnr = @lt_sd015_temp-kunnr
        INTO TABLE @DATA(lt_but).
      "二级部门
      SELECT  a~vkorg AS werks,a~kunnr,c~bezei
         FROM knvv AS a
        INNER JOIN tvkbt AS c ON c~vkbur EQ a~vkbur AND c~spras = @sy-langu
        FOR ALL ENTRIES IN @lt_sd015_temp
        WHERE a~kunnr = @lt_sd015_temp-kunnr AND a~vkorg = @lt_sd015_temp-werks
        INTO TABLE @DATA(lt_tvkbt).
      "业务员
      SELECT  a~vkorg AS werks,a~kunnr, b~name_org1
        FROM knvp AS a
        INNER JOIN but000 AS b ON b~partner = a~kunn2
        FOR ALL ENTRIES IN @lt_sd015_temp
        WHERE a~kunnr = @lt_sd015_temp-kunnr AND a~vkorg = @lt_sd015_temp-werks AND a~parvw = 'Z3'
        INTO TABLE @DATA(lt_ywy).
    ENDIF.
  
    "处理数据:12月的计划数量按横向显示
    SORT:lt_sd015 BY  matnr werks pbdnr,
         lt_but BY kunnr,
         lt_tvkbt BY werks kunnr,
         lt_ywy BY werks kunnr.
    LOOP AT lt_pbrecord INTO DATA(ls_pbrecord).
      CLEAR:lv_field_name,gs_pir_gross.
      READ TABLE gt_pir_gross WITH KEY werks = ls_pbrecord-werks matnr = ls_pbrecord-matnr pbdnr = ls_pbrecord-pbdnr ASSIGNING <fs_pir_gross>.
      IF sy-subrc <> 0."新增一行
        APPEND INITIAL LINE TO gt_pir_gross ASSIGNING <fs_pir_gross>.
        <fs_pir_gross>-werks = ls_pbrecord-werks.
        <fs_pir_gross>-matnr = ls_pbrecord-matnr.
        <fs_pir_gross>-versb = ls_pbrecord-versb.
        <fs_pir_gross>-pbdnr = ls_pbrecord-pbdnr.
        <fs_pir_gross>-meins = ls_pbrecord-meins.
        "物料描述
        READ TABLE lt_makt WITH KEY matnr = ls_pbrecord-matnr INTO DATA(ls_makt) BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_pir_gross>-maktx = ls_makt-maktx.
        ENDIF.
        "包装规格
        READ TABLE lt_ausp WITH KEY matnr = ls_pbrecord-matnr INTO DATA(ls_ausp) BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_pir_gross>-zpspe = ls_ausp-atwrt.
        ENDIF.
      ENDIF.
      " 动态更新期间的计划数量
      READ TABLE gt_pir_period WITH KEY perxx = ls_pbrecord-perxx INTO gs_pir_period.
      IF sy-subrc = 0.
        lv_field_name = '<fs_pir_gross>-' && gs_pir_period-field.
        ASSIGN (lv_field_name) TO <fs_plnmg> .
        IF <fs_plnmg> IS ASSIGNED.
          <fs_plnmg> = ls_pbrecord-plnmg.
        ENDIF.
      ENDIF.
      READ TABLE lt_sd015 WITH KEY  matnr = ls_pbrecord-matnr werks = ls_pbrecord-werks pbdnr = ls_pbrecord-pbdnr INTO DATA(ls_sd015) BINARY SEARCH.
      CHECK sy-subrc  = 0.
      <fs_pir_gross>-kunnr = ls_sd015-kunnr.
      "简称
      READ TABLE lt_but WITH KEY kunnr = ls_sd015-kunnr INTO DATA(ls_but) BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_pir_gross>-bu_sort1 = ls_but-bu_sort1.
      ENDIF.
      "业务员
      READ TABLE lt_ywy WITH KEY  werks = ls_pbrecord-werks kunnr = ls_sd015-kunnr INTO DATA(ls_ywy) BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_pir_gross>-name_org1 = ls_ywy-name_org1.
      ENDIF.
      "二级部门
      READ TABLE lt_tvkbt WITH KEY werks = ls_pbrecord-werks kunnr = ls_sd015-kunnr INTO DATA(ls_tvkbt) BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_pir_gross>-bztxt = ls_tvkbt-bezei.
      ENDIF.
    ENDLOOP.
    SORT gt_pir_gross BY pbdnr werks matnr.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_SET_PDATU
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_set_pdatu .
    DATA: lv_s_date LIKE sy-datum,
          lv_e_date LIKE sy-datum,
          lv_date   LIKE sy-datum,
          lv_index  TYPE int4,
          lv_dlymo  TYPE dlymo.
    FIELD-SYMBOLS <fs_field_desc> TYPE char10.
    lv_s_date = |{ p_perxx }{ '01' }|."开始期间的日期
    TRY .
        CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
          EXPORTING
            date                      = lv_s_date
          EXCEPTIONS
            plausibility_check_failed = 1
            OTHERS                    = 2.
        IF sy-subrc <> 0.
          MESSAGE s000 WITH '开始期间为无效日期'.
          STOP.
        ENDIF.
      CATCH cx_sy_dyn_call_illegal_type.
        MESSAGE s000 WITH '开始期间为无效字符'.
        STOP.
    ENDTRY.
    "查询日期的截止日期
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_s_date "开始日期
        days      = 0
        months    = 12
  *     SIGNUM    = '+'
        years     = 0
      IMPORTING
        calc_date = lv_e_date. "截止日期
    lv_e_date = lv_e_date - 1."PBED-PDATU 交货/订单完成日期的查询截止日期
    "12期间的标签
    REFRESH gt_pir_period.
    DO 12 TIMES.
      lv_index = sy-index.
      CLEAR:lv_dlymo, lv_date,gs_pir_period.
      lv_dlymo = CONV #( lv_index ).
      lv_dlymo = lv_dlymo - 1.
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = lv_s_date "开始日期
          days      = 0
          months    = lv_dlymo
  *       SIGNUM    = '+'
          years     = 0
        IMPORTING
          calc_date = lv_date.
      "计划独立需求12个月的期间范围值
      gs_pir_period-perxx = lv_date(6).
      gs_pir_period-field = 'YM' && lv_index.
      gs_pir_period-field_desc = | { lv_date(4) }{ '年' }{ lv_date+4(2) }{ '月' } |.
      APPEND gs_pir_period TO gt_pir_period.
      ASSIGN COMPONENT lv_index OF STRUCTURE gs_ymfield_desc TO <fs_field_desc>.
      IF <fs_field_desc> IS ASSIGNED.
        <fs_field_desc> = | { lv_date(4) }{ '年' }{ lv_date+4(2) }{ '月' } |.
      ENDIF.
    ENDDO.
    "查询计划独立需求的PBED-PDATU范围值
    REFRESH gr_pdatu.
    gr_pdatu = VALUE #( ( sign = 'I' option = 'BT' low = lv_s_date high = lv_e_date ) ).
  ENDFORM.
  FORM frm_set_col_text USING iv_colname TYPE lvc_fname iv_coltext  TYPE scrtext_s.
    DATA(lo_column) = go_salv_columns->get_column( iv_colname ).
    lo_column->set_short_text( iv_coltext ).
    lo_column->set_medium_text( CONV scrtext_m( iv_coltext ) ).
    lo_column->set_long_text( CONV scrtext_l( iv_coltext ) ).
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form DISPLAY_ALV_WAREHOUSE
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_display_alv_wh.
    IF go_con_warehouse IS INITIAL.
      CREATE OBJECT go_con_warehouse EXPORTING container_name = 'CON_WAREHOUSE'.
      go_alv_grid_wh  = NEW cl_gui_alv_grid( i_parent = go_con_warehouse ).
      "alv字段目录
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'ZPPT0011'
        CHANGING
          ct_fieldcat            = gt_alv_fidcat_wh
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.
      LOOP AT gt_alv_fidcat_wh ASSIGNING FIELD-SYMBOL(<fidcat>).
        CASE <fidcat>-fieldname.
          WHEN 'MANDT'.
            <fidcat>-tech = abap_true.
          WHEN 'WERKS' OR 'LGORT'.
            <fidcat>-edit = abap_true.
          WHEN 'Z010' OR 'Z030' OR 'Z050'.
            <fidcat>-edit = abap_true.
            <fidcat>-checkbox = abap_true.
        ENDCASE.
      ENDLOOP.
      gs_layout-cwidth_opt = abap_true.
      gs_layout-sel_mode = 'D'.
      CREATE OBJECT go_cl_wh_event_receiver.
      "注册编辑事件，使ALV编辑后回车，或离开焦点时可以进行数据校验
      SET HANDLER go_cl_wh_event_receiver->handle_toolbar FOR go_alv_grid_wh.
      SET HANDLER go_cl_wh_event_receiver->handle_data_changed_finished FOR go_alv_grid_wh.
      SET HANDLER go_cl_wh_event_receiver->handle_after_user_command FOR go_alv_grid_wh.
      go_alv_grid_wh->set_toolbar_interactive( ).
      go_alv_grid_wh->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
    ENDIF.
    "显示ALV
    CALL METHOD go_alv_grid_wh->set_table_for_first_display
      EXPORTING
        i_save                        = 'A'
        i_default                     = 'X'
        is_variant                    = VALUE #( report = sy-repid )
        is_layout                     = gs_layout
      CHANGING
        it_outtab                     = gt_zppt0011
        it_fieldcatalog               = gt_alv_fidcat_wh
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ENDFORM.
  FORM frm_get_zppt0011.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_zppt0011 FROM zppt0011.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_SAVE_ZPPT0011
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_save_zppt0011 .
    DATA: lv_e_valid TYPE flag,
          lv_uuid    TYPE  sysuuid_c36. "GUID
    "执行ALV数据有效性检查
    go_alv_grid_wh->check_changed_data( IMPORTING e_valid = lv_e_valid ).
    IF lv_e_valid = ''.
      RETURN.
    ENDIF.
    DELETE gt_zppt0011 WHERE table_line IS INITIAL.
    LOOP AT gt_zppt0011 ASSIGNING <fs_zppt0011>.
      IF <fs_zppt0011>-werks IS INITIAL OR <fs_zppt0011>-lgort IS INITIAL..
        MESSAGE s000 WITH |{ '第' }{ sy-tabix }{ '行的工厂或仓储地点不能为空' }|.
        lv_e_valid = ''.
        EXIT.
      ENDIF.
      IF <fs_zppt0011>-uuid IS INITIAL.
        TRY .
            <fs_zppt0011>-uuid = cl_system_uuid=>create_uuid_c36_static( )."guid唯一标识符
          CATCH cx_uuid_error.
            <fs_zppt0011>-uuid = |{ sy-datum }{ sy-uzeit }{ sy-tabix }|.
        ENDTRY.
      ENDIF.
    ENDLOOP.
    CHECK lv_e_valid = 'X'.
    MODIFY zppt0011 FROM TABLE gt_zppt0011.
    COMMIT WORK.
    go_alv_grid_wh->refresh_table_display( is_stable = gs_stable ).
    IF sy-subrc = 0.
      MESSAGE s000 WITH '保存成功'.
    ELSE.
      MESSAGE s000 WITH '保存失败'.
    ENDIF.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_PIR_NET
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_get_pir_net .
    DATA lt_pir_net LIKE TABLE OF  gs_pir_net.
    FIELD-SYMBOLS <fs_pir_net> LIKE gs_pir_net.
    REFRESH: gt_pir_net.
  
    "工厂、物料汇总
    LOOP AT gt_pir_gross INTO gs_pir_gross.
      READ TABLE gt_pir_net WITH KEY  werks = gs_pir_gross-werks matnr = gs_pir_gross-matnr ASSIGNING <fs_pir_net>.
      IF sy-subrc = 0.
        <fs_pir_net>-ym1 = <fs_pir_net>-ym1 + gs_pir_gross-ym1.
        <fs_pir_net>-ym2 = <fs_pir_net>-ym2 + gs_pir_gross-ym2.
        <fs_pir_net>-ym3 = <fs_pir_net>-ym3 + gs_pir_gross-ym3.
        <fs_pir_net>-ym4 = <fs_pir_net>-ym4 + gs_pir_gross-ym4.
        <fs_pir_net>-ym5 = <fs_pir_net>-ym5 + gs_pir_gross-ym5.
        <fs_pir_net>-ym6 = <fs_pir_net>-ym6 + gs_pir_gross-ym6.
        <fs_pir_net>-ym7 = <fs_pir_net>-ym7 + gs_pir_gross-ym7.
        <fs_pir_net>-ym8 = <fs_pir_net>-ym8 + gs_pir_gross-ym8.
        <fs_pir_net>-ym9 = <fs_pir_net>-ym9 + gs_pir_gross-ym9.
        <fs_pir_net>-ym10 = <fs_pir_net>-ym10 + gs_pir_gross-ym10.
        <fs_pir_net>-ym11 = <fs_pir_net>-ym11 + gs_pir_gross-ym11.
        <fs_pir_net>-ym12 = <fs_pir_net>-ym12 + gs_pir_gross-ym12.
      ELSE.
        CLEAR gs_pir_net.
        MOVE-CORRESPONDING gs_pir_gross TO gs_pir_net.
        APPEND gs_pir_net TO gt_pir_net.
      ENDIF.
    ENDLOOP.
    IF gt_pir_net IS INITIAL.
      RETURN.
    ENDIF.
    PERFORM frm_get_pir_so_inventory."销售订单库存
    PERFORM frm_get_pir_deliveryquan USING 'M1'."第M1期间的销售订单发货数量
    PERFORM frm_get_pir_wip_inventory."WIP库存
    "更新销售订单库存、销售订单数量、WIP库存
    LOOP AT gt_pir_net ASSIGNING <fs_pir_net>.
      "销售订单库存
      READ TABLE gt_pir_so_inventory WITH KEY werks = <fs_pir_net>-werks matnr = <fs_pir_net>-matnr INTO gs_pir_so_inventory BINARY SEARCH.
      IF sy-subrc = 0.
        "非限制数量 + 质检数量 + 冻结数量
        <fs_pir_net>-kalab = gs_pir_so_inventory-kalab + gs_pir_so_inventory-kains +  gs_pir_so_inventory-kaspe.
      ENDIF.
      "第M1期销售订单发货数量
      READ TABLE gt_pir_delivery WITH KEY  werks = <fs_pir_net>-werks matnr = <fs_pir_net>-matnr INTO gs_pir_delivery BINARY SEARCH.
      IF sy-subrc = 0.
        "移动类型601 - 移动类型602 的发货数量
        <fs_pir_net>-menge = gs_pir_delivery-menge_s -  gs_pir_delivery-menge_h.
      ENDIF.
      "WIP库存数量
      READ TABLE gt_pir_wip_inventory WITH KEY werks = <fs_pir_net>-werks matnr = <fs_pir_net>-matnr INTO gs_pir_so_inventory BINARY SEARCH.
      IF sy-subrc = 0.
        "非限制数量 + 质检数量 + 冻结数量
        <fs_pir_net>-menge_wip = gs_pir_so_inventory-kalab + gs_pir_so_inventory-kains +  gs_pir_so_inventory-kaspe.
      ENDIF.
    ENDLOOP.
    SORT gt_pir_net BY werks matnr.
    "追加WIP库存的新物料到PIR中
    LOOP AT gt_pir_wip_inventory INTO gs_pir_wip_inventory.
      CHECK gs_pir_wip_inventory-mtart = 'Z030' OR gs_pir_wip_inventory-mtart = 'Z050'.
      READ TABLE gt_pir_net WITH KEY  werks = gs_pir_wip_inventory-werks matnr = gs_pir_wip_inventory-matnr TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc <> 0.
        CLEAR gs_pir_net.
        gs_pir_net-werks = gs_pir_wip_inventory-werks.
        gs_pir_net-matnr = gs_pir_wip_inventory-matnr.
        gs_pir_net-maktx = gs_pir_wip_inventory-maktx.
        gs_pir_net-meins = gs_pir_wip_inventory-meins.
        "非限制数量 + 质检数量 + 冻结数量
        gs_pir_net-menge_wip = gs_pir_wip_inventory-kalab + gs_pir_wip_inventory-kains +  gs_pir_wip_inventory-kaspe.
        gs_pir_net-wip_flag = 'X'.
        APPEND gs_pir_net TO lt_pir_net.
      ENDIF.
    ENDLOOP.
    DELETE lt_pir_net WHERE menge_wip <= 0.
    IF lt_pir_net IS NOT INITIAL.
      APPEND LINES OF lt_pir_net TO gt_pir_net.
    ENDIF.
  ENDFORM.
  FORM frm_get_pir_wip_inventory.
    DATA:lt_pir_wip_inventory LIKE TABLE OF gs_pir_wip_inventory.
    FIELD-SYMBOLS <fs_wip> LIKE gs_pir_wip_inventory.
    SELECT werks,lgort FROM zppt0011 WHERE werks = @p_werks AND z030 = 'X' INTO TABLE  @DATA(lt_lgort) .
    CHECK lt_lgort IS NOT INITIAL.
    "WIP自由库存
    SELECT m~werks,m~matnr,SUM( m~labst ) AS kalab,SUM( m~insme ) AS kains,SUM( m~speme ) AS kaspe
     FROM @lt_lgort AS t1
     INNER JOIN mard AS m ON t1~werks = m~werks AND t1~lgort = m~lgort
     GROUP BY m~werks,m~matnr
    INTO TABLE @lt_pir_wip_inventory.
    "WIP 销售订单库存
    SELECT m~werks,m~matnr,SUM( m~kalab ) AS kalab,SUM( m~kains ) AS kains,SUM( m~kaspe ) AS kaspe
    FROM @lt_lgort AS t1
    INNER JOIN mska AS m ON t1~werks = m~werks AND t1~lgort = m~lgort
    WHERE m~sobkz = 'E'"特殊库存
    GROUP BY m~werks,m~matnr
    APPENDING TABLE @lt_pir_wip_inventory.
  
    DELETE lt_pir_wip_inventory WHERE kalab = 0 AND kains = 0 AND kaspe = 0.
    REFRESH gt_pir_wip_inventory.
    LOOP AT lt_pir_wip_inventory INTO gs_pir_wip_inventory.
      READ TABLE gt_pir_wip_inventory WITH KEY werks = gs_pir_wip_inventory-werks matnr =  gs_pir_wip_inventory-matnr ASSIGNING <fs_wip>.
      IF sy-subrc  = 0.
        <fs_wip>-kalab = <fs_wip>-kalab + gs_pir_wip_inventory-kalab.
        <fs_wip>-kains = <fs_wip>-kains + gs_pir_wip_inventory-kains.
        <fs_wip>-kaspe = <fs_wip>-kaspe + gs_pir_wip_inventory-kaspe.
      ELSE.
        APPEND   gs_pir_wip_inventory TO gt_pir_wip_inventory.
      ENDIF.
    ENDLOOP.
    IF gt_pir_wip_inventory IS NOT INITIAL.
      SELECT mara~matnr,mara~mtart,mara~meins, makt~maktx FROM mara
        LEFT JOIN makt ON mara~matnr = makt~matnr AND makt~spras = @sy-langu
        FOR ALL ENTRIES IN @gt_pir_wip_inventory
        WHERE mara~matnr = @gt_pir_wip_inventory-matnr
        INTO TABLE @DATA(lt_mara).
    ENDIF.
    SORT lt_mara BY matnr.
    LOOP AT gt_pir_wip_inventory ASSIGNING <fs_wip>.
      READ TABLE lt_mara WITH KEY matnr = <fs_wip>-matnr BINARY SEARCH INTO DATA(ls_mara).
      IF sy-subrc = 0.
        <fs_wip>-mtart = ls_mara-mtart.
        <fs_wip>-meins = ls_mara-meins.
        <fs_wip>-maktx = ls_mara-maktx.
      ENDIF.
    ENDLOOP.
    DELETE gt_pir_wip_inventory WHERE mtart <> 'Z030' AND mtart <> 'Z050'.
    SORT gt_pir_wip_inventory BY werks matnr.
    FREE:lt_mara,lt_pir_wip_inventory.
  ENDFORM.
  FORM frm_get_raw_inventory.
    DATA: lr_lgort  TYPE RANGE OF mska-lgort.
    REFRESH gt_raw_inventory.
    IF gt_pir_raw_net_sum IS NOT INITIAL.
      SELECT lgort FROM zppt0011 WHERE werks = @p_werks AND z010 = 'X' INTO TABLE  @DATA(lt_lgort) .
      LOOP AT lt_lgort INTO DATA(ls_lgort).
        lr_lgort = VALUE #( BASE lr_lgort ( sign = 'I' option = 'EQ' low = ls_lgort-lgort ) ).
      ENDLOOP.
      "PIR产品展开的原辅包材库存
      SELECT m~werks,m~matnr,SUM( m~labst ) AS kalab,SUM( m~insme ) AS kains,SUM( m~speme ) AS kaspe
       FROM @gt_pir_raw_net_sum AS t1
       INNER JOIN mard AS m ON t1~werks = m~werks AND t1~idnrk = m~matnr
        WHERE m~lgort IN @lr_lgort
       GROUP BY m~werks,m~matnr
      INTO TABLE @gt_raw_inventory.
    ENDIF.
  ENDFORM.
  *销售订单库存
  FORM frm_get_pir_so_inventory.
    DATA: lr_lgort  TYPE RANGE OF mska-lgort.
    FIELD-SYMBOLS <fs_pir_so_inv> LIKE gs_pir_so_inventory.
    "销售订单库存
    REFRESH gt_pir_so_inventory.
    "需要查询工厂的仓储地点
    SELECT lgort FROM zppt0011 WHERE werks = @p_werks AND z050 = 'X' INTO TABLE  @DATA(lt_lgort) .
    LOOP AT lt_lgort INTO DATA(ls_lgort).
      lr_lgort = VALUE #( BASE lr_lgort ( sign = 'I' option = 'EQ' low = ls_lgort-lgort ) ).
    ENDLOOP.
    SELECT t1~werks,t1~matnr,SUM( m~kalab ) AS kalab,SUM( m~kains ) AS kains,SUM( m~kaspe ) AS kaspe
     FROM @gt_pir_net AS t1
    INNER JOIN mska AS m ON t1~werks = m~werks AND t1~matnr = m~matnr
    WHERE m~sobkz = 'E'"特殊库存
      AND t1~werks = @p_werks
      AND m~lgort IN @lr_lgort
      GROUP BY t1~werks,t1~matnr
    INTO TABLE @gt_pir_so_inventory.
    SORT gt_pir_so_inventory BY werks matnr.
  ENDFORM.
  *按会计期间查询PIR工厂物料销售订单基本单位发货数量
  FORM frm_get_pir_deliveryquan USING iv_field TYPE char6.
    DATA: lr_lgort  TYPE RANGE OF mska-lgort,
          lv_date_s TYPE sy-datum,
          lv_date_e TYPE sy-datum,
          lr_budat  TYPE RANGE OF matdoc-budat.
    REFRESH gt_pir_delivery.
    IF gt_pir_net IS INITIAL OR iv_field IS INITIAL.
      RETURN.
    ENDIF.
    READ TABLE gt_pir_period WITH KEY field = iv_field INTO gs_pir_period.
    IF sy-subrc = 0.
      lv_date_s = |{ gs_pir_period-perxx }{ '01' }|.
      CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
        EXPORTING
          i_date = lv_date_s
        IMPORTING
          e_date = lv_date_e.
      lr_budat = VALUE #( ( sign = 'I' option = 'BT' low = lv_date_s high = lv_date_e ) ).
    ENDIF.
    CHECK lr_budat IS NOT INITIAL.
    "销售订单发货数量
  
    "需要查询工厂的仓储地点
    SELECT lgort FROM zppt0011 WHERE werks = @p_werks AND z050 = 'X' INTO TABLE  @DATA(lt_lgort) .
    LOOP AT lt_lgort INTO DATA(ls_lgort).
      lr_lgort = VALUE #( BASE lr_lgort ( sign = 'I' option = 'EQ' low = ls_lgort-lgort ) ).
    ENDLOOP.
    SELECT t1~werks,t1~matnr,SUM( CASE m~bwart WHEN '601' THEN m~menge END ) AS menge_s,SUM( CASE m~bwart WHEN '602' THEN m~menge END ) AS menge_h
     FROM @gt_pir_net AS t1
    INNER JOIN matdoc AS m ON t1~werks = m~werks AND t1~matnr = m~matbf
    WHERE m~sobkz = 'E'"特殊库存
      AND t1~werks = @p_werks
      AND m~lgort_sid IN @lr_lgort
      AND m~budat IN @lr_budat "过账日期
      AND m~bwart IN ( '601','602' )
      GROUP BY t1~werks,t1~matnr
      INTO TABLE @gt_pir_delivery.
    SORT  gt_pir_delivery BY werks matnr.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_DISPLAY_ALV_PIR_NET
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_display_alv_pir_net .
    DATA lv_lable TYPE char10.
    IF go_con_pir_net IS NOT BOUND.
      CREATE OBJECT go_con_pir_net EXPORTING container_name = 'CON_PIR_NET'.
  *    TRY.
      "1.创建SALV对象
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          list_display = if_salv_c_bool_sap=>false
          r_container  = go_con_pir_net
        " container_name = 'dockingcon1'
        IMPORTING
          r_salv_table = go_salv_table_pir_net
        CHANGING
          t_table      = gt_pir_net.
      "2.保存布局设置
      go_salv_table_pir_net->get_layout( )->set_key( VALUE salv_s_layout_key( report = sy-repid ) ).
      go_salv_table_pir_net->get_layout( )->set_save_restriction( cl_salv_layout=>restrict_none ).
      "go_salv_table_pir_gross->get_display_settings( )->set_list_header( '选中要执行LTP的行，未选中任何行即表示全部执行' )."设置标题
      "3.字段目录属性设置
      go_salv_columns =  go_salv_table_pir_net->get_columns( ).
      go_salv_columns->set_optimize( abap_true ). "列宽自动优化
      lv_lable = |{ gs_ymfield_desc-ym1+3(6) }{ 'SO发货' }|.
      PERFORM: frm_set_col_text USING 'KALAB' '销售订单库存',
               frm_set_col_text USING 'MENGE' lv_lable,
               frm_set_col_text USING 'MENGE_WIP' '在制品还原库存',
               frm_set_col_text USING 'WIP_FLAG' '仅WIP还原'.
  
      "设置12个月标签
      PERFORM frm_set_ym_desc.
      "4.选择模式
      go_salv_table_pir_net->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column )."行与字段的选择模式，报表左边追加了可以选择行的MARK按钮
      "5.设置ALV工具栏
      go_salv_table_pir_net->get_functions( )->set_all( abap_true ).
  *     CATCH cx_salv_msg .
  *    ENDTRY.
    ENDIF.
    "6.显示SALV
    go_salv_table_pir_net->display( ).
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_CALC_PIR_NET
  *&---------------------------------------------------------------------*
  *& PIR净需求计算逻辑：
  *1、M1 = 需求量 - 销售订单库存 - 销售订单发货 - 在制品还原库存;
  *2、如果M1 >= 0即表示存在需求量，则M2-M12的需求量保持不变，无需计算;
  *3、如果M1 < 0即表示不存在需求量，则计算M2的净需求 = M1 + M2;
  *4、如果M2 >= 0即表示存在需求量，后面期间的需求量保持不变;
  *5、如果M2 < 0即表示不存在需求了，结算计算后面的需求量，参照步骤3.
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_calc_pir_net.
    DATA: lv_pre_field TYPE char30,
          lv_field     TYPE char30,
          lv_pre_plnmg TYPE plnmg,
          lv_index     TYPE sy-index.
    FIELD-SYMBOLS <fs_pre_plnmg> TYPE plnmg.
    FIELD-SYMBOLS <fs_plnmg> TYPE plnmg.
    LOOP AT gt_pir_net ASSIGNING FIELD-SYMBOL(<fs_pir_net>) WHERE wip_flag = ''.
      DO 12 TIMES.
        lv_index = sy-index.
        IF lv_index = 1.
          "M1 = 需求量 - 销售订单库存 - 销售订单发货 - 在制品还原库存;
          <fs_pir_net>-ym1 = <fs_pir_net>-ym1 - <fs_pir_net>-kalab - <fs_pir_net>-menge - <fs_pir_net>-menge_wip.
          IF <fs_pir_net>-ym1 >= 0.
            EXIT.
          ENDIF.
        ELSE.
          "计算M(n) = M(n) + M(n-1).
          IF <fs_pre_plnmg> IS ASSIGNED.
            UNASSIGN <fs_pre_plnmg>.
          ENDIF.
          IF <fs_plnmg> IS ASSIGNED.
            UNASSIGN <fs_plnmg>.
          ENDIF.
          lv_field = |{ '<FS_PIR_NET>-YM' }{ lv_index }|."当期的字段名称
          lv_pre_field = |{ '<FS_PIR_NET>-YM' }{ lv_index - 1 }|."上一期的字段名称
          ASSIGN (lv_pre_field) TO <fs_pre_plnmg>.
          ASSIGN (lv_field) TO <fs_plnmg>.
          IF <fs_pre_plnmg> IS ASSIGNED AND <fs_plnmg> IS ASSIGNED.
            IF <fs_pre_plnmg> >= 0 .
              EXIT.
            ELSE.
              <fs_plnmg> = <fs_plnmg> + <fs_pre_plnmg>. "M(n) = M(n) + M(n-1).
            ENDIF.
            IF <fs_plnmg> >= 0.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_SET_INITIAL_VALUE
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_set_initial_value .
    gs_stable-col = 'X'.
    gs_stable-row = 'X'.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_CALC_PIR_RAW_NET
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_calc_pir_raw_net .
    DATA:ls_resp_header TYPE zpps_bom_resp_header,
         lt_resp_item   TYPE TABLE OF zpps_bom_resp_item,
         lv_bmeng       TYPE basmn, "基本数量
         "lv_plnmg(7)    TYPE p DECIMALS 6,
         lv_plnmg1(7)   TYPE p DECIMALS 6,
         lv_plnmg2(7)   TYPE p DECIMALS 6,
         lv_index       TYPE int4.
  
    CLEAR gs_bapiret.
    REFRESH:gt_req,gt_resp_header,gt_resp_item.
    MOVE-CORRESPONDING gt_pir_net TO gt_req.
    SORT gt_req BY werks matnr.
    DELETE ADJACENT DUPLICATES FROM gt_req COMPARING werks matnr.
    LOOP AT gt_req ASSIGNING FIELD-SYMBOL(<fs_req>).
      <fs_req>-zblx = '1'.
      <fs_req>-zbzkbs = '1'.
    ENDLOOP.
    CALL FUNCTION 'Z_PP_BOM_QUERY'
      EXPORTING
        iv_aumng          = 0 "报废数量:0 计算包含装配报废率,1不包含
      IMPORTING
        es_bapiret        = gs_bapiret
      TABLES
        it_req            = gt_req
        it_resp_header    = gt_resp_header
        it_resp_item      = gt_resp_item
      EXCEPTIONS
        invalid_req_paras = 1
        OTHERS            = 2.
    "显示PIR展开BOM后的Z010、Z020物料相关需求
    REFRESH gt_pir_raw_net.
    LOOP AT gt_pir_net INTO gs_pir_net.
      CLEAR:lv_index,gs_pir_raw_net.
      lv_index = sy-tabix.
      MOVE-CORRESPONDING gs_pir_net TO gs_pir_raw_net.
      gs_pir_raw_net-zid = lv_index.
      APPEND gs_pir_raw_net TO gt_pir_raw_net.
      READ TABLE gt_resp_header WITH KEY matnr = gs_pir_net-matnr werks = gs_pir_net-werks INTO ls_resp_header.
      IF sy-subrc = 0.
        REFRESH lt_resp_item.
        lt_resp_item = FILTER #( gt_resp_item USING KEY bom_zid WHERE zid  = ls_resp_header-zid ).
        IF lt_resp_item IS NOT INITIAL.
          SELECT mara~matnr,marc~werks,mara~mtart,marc~plifz,marc~webaz
            FROM mara JOIN marc ON mara~matnr = marc~matnr
            FOR ALL ENTRIES IN @lt_resp_item
            WHERE mara~matnr = @lt_resp_item-idnrk
            AND marc~werks = @ls_resp_header-werks
            INTO TABLE @DATA(lt_marc).
          SORT lt_marc BY matnr werks.
        ENDIF.
        "追加Z010、Z020物料类型的需求数量
        TRY .
            lv_bmeng = CONV #( ls_resp_header-bmeng )."BOM抬头基本数量
            LOOP AT lt_resp_item INTO DATA(ls_resp_item).
              READ TABLE lt_marc WITH KEY matnr = ls_resp_item-idnrk werks = ls_resp_header-werks INTO DATA(ls_marc).
              CHECK sy-subrc = 0 AND ( ls_marc-mtart = 'Z010' OR ls_marc-mtart = 'Z020' ).
              gs_pir_raw_net-idnrk = ls_resp_item-idnrk.
              gs_pir_raw_net-ojtxp = ls_resp_item-ojtxp.
              gs_pir_raw_net-mmein = ls_resp_item-mmein.
              gs_pir_raw_net-plifz = ls_marc-plifz."计划交货时间
              gs_pir_raw_net-webaz = ls_marc-webaz."以天计的收货处理时间
              lv_plnmg1 = ls_resp_item-mnglg."标准数量-避免数量太小，丢失结果
              lv_plnmg2 = ls_resp_item-zmnglg."固定数量-避免数量太小，丢失结果
              "lv_plnmg = lv_plnmg1 ."避免数量太小，丢失结果
              "lv_plnmg = CONV plnmg( ls_resp_item-mnglg ) + CONV plnmg( ls_resp_item-zmnglg ).
              IF gs_pir_net-wip_flag IS INITIAL.
                gs_pir_raw_net-ym1 = COND #( WHEN gs_pir_net-ym1 < 0 THEN 0 ELSE gs_pir_net-ym1 / lv_bmeng * lv_plnmg1 ).
                gs_pir_raw_net-ym2 = COND #( WHEN gs_pir_net-ym2 < 0 THEN 0 ELSE gs_pir_net-ym2 / lv_bmeng * lv_plnmg1 ).
                gs_pir_raw_net-ym3 = COND #( WHEN gs_pir_net-ym3 < 0 THEN 0 ELSE gs_pir_net-ym3 / lv_bmeng * lv_plnmg1 ).
                gs_pir_raw_net-ym4 = COND #( WHEN gs_pir_net-ym4 < 0 THEN 0 ELSE gs_pir_net-ym4 / lv_bmeng * lv_plnmg1 ).
                gs_pir_raw_net-ym5 = COND #( WHEN gs_pir_net-ym5 < 0 THEN 0 ELSE gs_pir_net-ym5 / lv_bmeng * lv_plnmg1 ).
                gs_pir_raw_net-ym6 = COND #( WHEN gs_pir_net-ym6 < 0 THEN 0 ELSE gs_pir_net-ym6 / lv_bmeng * lv_plnmg1 ).
                gs_pir_raw_net-ym7 = COND #( WHEN gs_pir_net-ym7 < 0 THEN 0 ELSE gs_pir_net-ym7 / lv_bmeng * lv_plnmg1 ).
                gs_pir_raw_net-ym8 = COND #( WHEN gs_pir_net-ym8 < 0 THEN 0 ELSE gs_pir_net-ym8 / lv_bmeng * lv_plnmg1 ).
                gs_pir_raw_net-ym9 = COND #( WHEN gs_pir_net-ym9 < 0 THEN 0 ELSE gs_pir_net-ym9 / lv_bmeng * lv_plnmg1 ).
                gs_pir_raw_net-ym10 = COND #( WHEN gs_pir_net-ym10 < 0 THEN 0 ELSE gs_pir_net-ym10 / lv_bmeng * lv_plnmg1 ).
                gs_pir_raw_net-ym11 = COND #( WHEN gs_pir_net-ym11 < 0 THEN 0 ELSE gs_pir_net-ym11 / lv_bmeng * lv_plnmg1 ).
                gs_pir_raw_net-ym12 = COND #( WHEN gs_pir_net-ym12 < 0 THEN 0 ELSE gs_pir_net-ym12 / lv_bmeng * lv_plnmg1 ).
              ELSE."WIP库存还原原辅包材
                gs_pir_raw_net-wip_flag = 'X'.
                gs_pir_raw_net-raw_stock = COND #( WHEN gs_pir_net-menge_wip < 0 THEN 0 ELSE gs_pir_net-menge_wip  / lv_bmeng * lv_plnmg1 ).
              ENDIF.
              "增加固定数量
              gs_pir_raw_net-ym1 = COND #( WHEN gs_pir_net-ym1 > 0 THEN gs_pir_raw_net-ym1 + lv_plnmg2 ELSE 0 ).
              gs_pir_raw_net-ym2 = COND #( WHEN gs_pir_net-ym2 > 0 THEN gs_pir_raw_net-ym2 + lv_plnmg2 ELSE 0 ).
              gs_pir_raw_net-ym3 = COND #( WHEN gs_pir_net-ym3 > 0 THEN gs_pir_raw_net-ym3 + lv_plnmg2 ELSE 0 ).
              gs_pir_raw_net-ym4 = COND #( WHEN gs_pir_net-ym4 > 0 THEN gs_pir_raw_net-ym4 + lv_plnmg2 ELSE 0 ).
              gs_pir_raw_net-ym5 = COND #( WHEN gs_pir_net-ym5 > 0 THEN gs_pir_raw_net-ym5 + lv_plnmg2 ELSE 0 ).
              gs_pir_raw_net-ym6 = COND #( WHEN gs_pir_net-ym6 > 0 THEN gs_pir_raw_net-ym6 + lv_plnmg2 ELSE 0 ).
              gs_pir_raw_net-ym7 = COND #( WHEN gs_pir_net-ym7 > 0 THEN gs_pir_raw_net-ym7 + lv_plnmg2 ELSE 0 ).
              gs_pir_raw_net-ym8 = COND #( WHEN gs_pir_net-ym8 > 0 THEN gs_pir_raw_net-ym8 + lv_plnmg2 ELSE 0 ).
              gs_pir_raw_net-ym9 = COND #( WHEN gs_pir_net-ym9 > 0 THEN gs_pir_raw_net-ym9 + lv_plnmg2 ELSE 0 ).
              gs_pir_raw_net-ym10 = COND #( WHEN gs_pir_net-ym10 > 0 THEN gs_pir_raw_net-ym10 + lv_plnmg2 ELSE 0 ).
              gs_pir_raw_net-ym11 = COND #( WHEN gs_pir_net-ym11 > 0 THEN gs_pir_raw_net-ym11 + lv_plnmg2 ELSE 0 ).
              gs_pir_raw_net-ym12 = COND #( WHEN gs_pir_net-ym12 > 0 THEN gs_pir_raw_net-ym12 + lv_plnmg2 ELSE 0 ).
              gs_pir_raw_net-raw_stock = COND #( WHEN gs_pir_raw_net-raw_stock > 0 THEN gs_pir_raw_net-raw_stock + lv_plnmg2 ELSE 0 ).
              APPEND gs_pir_raw_net TO gt_pir_raw_net.
            ENDLOOP.
          CATCH cx_sy_arithmetic_overflow.
            MESSAGE w000 WITH |{ ls_resp_item-idnrk }{ '还原库存数量太大导致溢出异常，请检查' }|.
        ENDTRY.
        REFRESH lt_marc.
      ENDIF.
    ENDLOOP.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_DISPLAY_PIR_RAW_NET_ALV
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_display_pir_raw_net_alv .
    DATA lv_lable TYPE scrtext_s.
    IF go_con_pir_raw_net  IS NOT BOUND.
      CREATE OBJECT go_con_pir_raw_net EXPORTING container_name = 'CON_PIR_RAW_NET'.
  *    TRY.
      "1.创建SALV对象
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          list_display = if_salv_c_bool_sap=>false
          r_container  = go_con_pir_raw_net
        " container_name = 'dockingcon1'
        IMPORTING
          r_salv_table = go_salv_table_pir_raw_net
        CHANGING
          t_table      = gt_pir_raw_net.
      "2.保存布局设置
      go_salv_table_pir_raw_net->get_layout( )->set_key( VALUE salv_s_layout_key( report = sy-repid ) ).
      go_salv_table_pir_raw_net->get_layout( )->set_save_restriction( cl_salv_layout=>restrict_none ).
      "go_salv_table_pir_gross->get_display_settings( )->set_list_header( '选中要执行LTP的行，未选中任何行即表示全部执行' )."设置标题
      "3.字段目录属性设置
      go_salv_columns =  go_salv_table_pir_raw_net->get_columns( ).
      go_salv_columns->set_optimize( abap_true ). "列宽自动优化
      go_salv_columns->set_column_position( columnname = 'RAW_STOCK' position = 15 ).
      lv_lable = |{ gs_ymfield_desc-ym1+3(6) }{ 'SO发货' }|.
      PERFORM: frm_set_col_text USING 'ZID' '序号',
               frm_set_col_text USING 'KALAB' '销售订单库存',
               frm_set_col_text USING 'MENGE' lv_lable,
               frm_set_col_text USING 'MENGE_WIP' '在制品还原库存',
               frm_set_col_text USING 'RAW_STOCK' '原辅包材库存',
               frm_set_col_text USING 'WIP_FLAG' '仅WIP还原'.
      "设置12个月标签
      PERFORM frm_set_ym_desc.
      "4.选择模式
      go_salv_table_pir_raw_net->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column )."行与字段的选择模式，报表左边追加了可以选择行的MARK按钮
      "5.设置ALV工具栏
      go_salv_table_pir_raw_net->get_functions( )->set_all( abap_true ).
  *     CATCH cx_salv_msg .
  *    ENDTRY
    ENDIF.
    IF gv_action_by_zbn_switch = 'X'."切换明细/汇总视图
      go_salv_columns =  go_salv_table_pir_raw_net->get_columns( ).
      IF gv_switch = '1'."明细视图
        go_salv_columns->get_column( 'ZID' )->set_technical( abap_false ).
        go_salv_columns->get_column( 'MATNR' )->set_technical( abap_false ).
        go_salv_columns->get_column( 'MAKTX' )->set_technical( abap_false ).
        go_salv_columns->get_column( 'MEINS' )->set_technical( abap_false ).
        go_salv_columns->get_column( 'KALAB' )->set_technical( abap_false ).
        go_salv_columns->get_column( 'MENGE' )->set_technical( abap_false ).
        go_salv_columns->get_column( 'MENGE_WIP' )->set_technical( abap_false ).
        "go_salv_columns->get_column( 'RAW_STOCK' )->set_technical( abap_true ).
        go_salv_columns->get_column( 'WIP_FLAG' )->set_technical( abap_false ).
      ELSEIF  gv_switch = '2'."汇总视图
        go_salv_columns->get_column( 'ZID' )->set_technical( abap_true ).
        go_salv_columns->get_column( 'MATNR' )->set_technical( abap_true ).
        go_salv_columns->get_column( 'MAKTX' )->set_technical( abap_true ).
        go_salv_columns->get_column( 'MEINS' )->set_technical( abap_true ).
        go_salv_columns->get_column( 'KALAB' )->set_technical( abap_true ).
        go_salv_columns->get_column( 'MENGE' )->set_technical( abap_true ).
        go_salv_columns->get_column( 'MENGE_WIP' )->set_technical( abap_true ).
        go_salv_columns->get_column( 'WIP_FLAG' )->set_technical( abap_true ).
        "go_salv_columns->get_column( 'RAW_STOCK' )->set_technical( abap_false ).
      ENDIF.
      CLEAR gv_action_by_zbn_switch.
      go_salv_table_pir_raw_net->refresh( s_stable = gs_stable ).
    ENDIF.
    "6.显示SALV
    go_salv_table_pir_raw_net->display( ).
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_SWITCH_DETAIL_2_SUM
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_switch_detail_2_sum .
    DATA: ls_pir_raw_net         LIKE  gs_pir_raw_net,
          lt_pir_raw_net         LIKE TABLE OF gs_pir_raw_net,
          lt_pir_raw_wip_net     LIKE TABLE OF gs_pir_raw_net,
          lt_pir_raw_wip_net_sum LIKE TABLE OF gs_pir_raw_net.
    FIELD-SYMBOLS <fs_pir_raw_net> LIKE gs_pir_raw_net.
  
    lt_pir_raw_net = gt_pir_raw_net_bak.
    DELETE lt_pir_raw_net WHERE idnrk IS INITIAL."删除30、50物料
    "汇总PIR物料展开的原辅包材净需求
    REFRESH gt_pir_raw_net_sum.
    LOOP AT lt_pir_raw_net INTO gs_pir_raw_net WHERE wip_flag = ''.
      READ TABLE gt_pir_raw_net_sum WITH KEY werks = gs_pir_raw_net-werks idnrk = gs_pir_raw_net-idnrk ASSIGNING <fs_pir_raw_net>.
      IF sy-subrc = 0.
        <fs_pir_raw_net>-ym1 = <fs_pir_raw_net>-ym1 + gs_pir_raw_net-ym1.
        <fs_pir_raw_net>-ym2 = <fs_pir_raw_net>-ym2 + gs_pir_raw_net-ym2.
        <fs_pir_raw_net>-ym3 = <fs_pir_raw_net>-ym3 + gs_pir_raw_net-ym3.
        <fs_pir_raw_net>-ym4 = <fs_pir_raw_net>-ym4 + gs_pir_raw_net-ym4.
        <fs_pir_raw_net>-ym5 = <fs_pir_raw_net>-ym5 + gs_pir_raw_net-ym5.
        <fs_pir_raw_net>-ym6 = <fs_pir_raw_net>-ym6 + gs_pir_raw_net-ym6.
        <fs_pir_raw_net>-ym7 = <fs_pir_raw_net>-ym7 + gs_pir_raw_net-ym7.
        <fs_pir_raw_net>-ym8 = <fs_pir_raw_net>-ym8 + gs_pir_raw_net-ym8.
        <fs_pir_raw_net>-ym9 = <fs_pir_raw_net>-ym9 + gs_pir_raw_net-ym9.
        <fs_pir_raw_net>-ym10 = <fs_pir_raw_net>-ym10 + gs_pir_raw_net-ym10.
        <fs_pir_raw_net>-ym11 = <fs_pir_raw_net>-ym11 + gs_pir_raw_net-ym11.
        <fs_pir_raw_net>-ym12 = <fs_pir_raw_net>-ym12 + gs_pir_raw_net-ym12.
      ELSE.
        APPEND gs_pir_raw_net TO gt_pir_raw_net_sum.
      ENDIF.
    ENDLOOP.
    "汇总WIP库存物料展开的原辅包材毛需求
    LOOP AT lt_pir_raw_net INTO gs_pir_raw_net WHERE wip_flag = 'X'.
      READ TABLE lt_pir_raw_wip_net_sum WITH KEY werks = gs_pir_raw_net-werks idnrk = gs_pir_raw_net-idnrk ASSIGNING <fs_pir_raw_net>.
      IF sy-subrc = 0.
        <fs_pir_raw_net>-raw_stock = <fs_pir_raw_net>-raw_stock + gs_pir_raw_net-raw_stock.
      ELSE.
        APPEND gs_pir_raw_net TO lt_pir_raw_wip_net_sum.
      ENDIF.
    ENDLOOP.
    "PIR物料展开的原辅包材毛需求库存
    PERFORM frm_get_raw_inventory.
    "WIP库存物料展开的原辅包材毛需求追加到PIR物料展开的原辅包材净需求
    SORT lt_pir_raw_wip_net_sum BY werks idnrk.
    SORT gt_raw_inventory BY werks matnr.
    LOOP AT gt_pir_raw_net_sum ASSIGNING <fs_pir_raw_net>.
      READ TABLE lt_pir_raw_wip_net_sum WITH KEY werks = <fs_pir_raw_net>-werks idnrk = <fs_pir_raw_net>-idnrk INTO gs_pir_raw_net BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_pir_raw_net>-raw_stock = <fs_pir_raw_net>-raw_stock + gs_pir_raw_net-raw_stock.
      ENDIF.
      READ TABLE gt_raw_inventory WITH KEY werks =  <fs_pir_raw_net>-werks matnr = <fs_pir_raw_net>-idnrk INTO gs_pir_so_inventory BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_pir_raw_net>-raw_stock = <fs_pir_raw_net>-raw_stock + gs_pir_so_inventory-kalab + gs_pir_so_inventory-kaspe + gs_pir_so_inventory-kains.
      ENDIF.
    ENDLOOP.
    SORT gt_pir_raw_net_sum BY werks idnrk.
    REFRESH:lt_pir_raw_net,lt_pir_raw_wip_net,lt_pir_raw_wip_net_sum.
    FREE:lt_pir_raw_net,lt_pir_raw_wip_net,lt_pir_raw_wip_net_sum.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_AUTH_CHECK
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_auth_check .
    AUTHORITY-CHECK OBJECT 'ZPP003' ID 'WERKS' FIELD p_werks.
    IF sy-subrc <> 0.
      MESSAGE s000 WITH |{ '你没有工厂' }{ p_werks }{ '的权限!' }|.
      STOP.
    ENDIF.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_CALC_RAW_NET
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_calc_raw_net .
    DATA: lv_pre_field TYPE char30,
          lv_field     TYPE char30,
          lv_pre_plnmg TYPE plnmg,
          lv_index     TYPE sy-index.
    FIELD-SYMBOLS <fs_pre_plnmg> TYPE plnmg.
    FIELD-SYMBOLS <fs_plnmg> TYPE plnmg.
    LOOP AT gt_pir_raw_net_sum ASSIGNING FIELD-SYMBOL(<fs_net_sum>).
      DO 12 TIMES.
        lv_index = sy-index.
        IF lv_index = 1.
          "M1 = 需求量 - 库存(raw_stock);
          <fs_net_sum>-ym1 = <fs_net_sum>-ym1 - <fs_net_sum>-raw_stock.
          IF <fs_net_sum>-ym1 >= 0.
            EXIT.
          ENDIF.
        ELSE.
          "计算M(n) = M(n) + M(n-1).
          IF <fs_pre_plnmg> IS ASSIGNED.
            UNASSIGN <fs_pre_plnmg>.
          ENDIF.
          IF <fs_plnmg> IS ASSIGNED.
            UNASSIGN <fs_plnmg>.
          ENDIF.
          lv_field = |{ '<FS_NET_SUM>-YM' }{ lv_index }|."当期的字段名称
          lv_pre_field = |{ '<FS_NET_SUM>-YM' }{ lv_index - 1 }|."上一期的字段名称
          ASSIGN (lv_pre_field) TO <fs_pre_plnmg>.
          ASSIGN (lv_field) TO <fs_plnmg>.
          IF <fs_pre_plnmg> IS ASSIGNED AND <fs_plnmg> IS ASSIGNED.
            IF <fs_pre_plnmg> >= 0 .
              EXIT.
            ELSE.
              <fs_plnmg> = <fs_plnmg> + <fs_pre_plnmg>. "M(n) = M(n) + M(n-1).
            ENDIF.
            "当前字段计算完成后，如果前一个字段数量小于0，需重置为0.
            IF <fs_pre_plnmg> < 0.
              <fs_pre_plnmg> = 0.
            ENDIF.
            "计算到最后1个字段时，如果值小于0，需重置为0
            IF lv_index = 12 AND <fs_plnmg> < 0.
              <fs_plnmg> = 0.
            ENDIF.
            IF <fs_plnmg> >= 0.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDFORM.