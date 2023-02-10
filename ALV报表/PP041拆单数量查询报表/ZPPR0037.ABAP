*&---------------------------------------------------------------------*
*& Report ZPP0037
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr0037 MESSAGE-ID zpp.
TABLES:mara,marc.
DATA:BEGIN OF gs_output,
       stufe       TYPE stpox-stufe, "BOM级别,最多3级
       matnr       TYPE mara-matnr, "物料编号
       mtart       TYPE mara-mtart, "物料类型
       matkl       TYPE mara-matkl, "物料组
       groes       TYPE mara-groes, "大小/量纲
       zmold       TYPE char70, "模具
       maktx       TYPE makt-maktx, "物料描述
       maktxl      TYPE string, "物料长描述
       werks       TYPE marc-werks, "工厂
       bstmi       TYPE marc-bstmi, "最小批量
       bstma       TYPE marc-bstma, "最大批量大小
       bstfe       TYPE marc-bstfe, "固定批量大小
       bstrf       TYPE marc-bstrf, "采购订单数量的舍入值
       first_sh    TYPE rc29p-ausch, "首个50-30组件损耗 Seashell 20230112
       ausss       TYPE marc-ausss, "装配报废百分比
       ausss_total TYPE marc-ausss, "合并装配报废百分比
       bstmin      TYPE marc-bstmi, "最小批量净产出
       bstman      TYPE marc-bstma, "最大批量净产出
       bstfen      TYPE marc-bstfe, "固定批量净产出
       meins       TYPE mara-meins, "基本计量单位
       bstmis      TYPE marc-bstmi, "建议下单最小批量
       bstmas      TYPE marc-bstma, "建议下单最大批量
       bstfes      TYPE marc-bstfe, "建议固定下单批量
       vrkme       TYPE mvke-vrkme, "销售单位
       umrez       TYPE smeinh-umrez, "基本计量单位转换分子
     END OF gs_output.
DATA:BEGIN OF gs_matnr,
       matnr TYPE mara-matnr,
       mtart TYPE mara-mtart,
     END OF gs_matnr.
DATA:BEGIN OF gs_maktxl,
       matnr  TYPE mara-matnr, "物料编号
       maktxl TYPE string, "物料长描述
     END OF gs_maktxl.
DATA:BEGIN OF gs_objek,
       objek TYPE char90,
     END OF gs_objek.
TYPES: ty_tab_output LIKE TABLE OF gs_output.
DATA:gt_output       LIKE TABLE OF gs_output,
     go_salv_table   TYPE REF TO cl_salv_table,
     go_salv_columns TYPE REF TO cl_salv_columns_table,
     gt_objek_001    LIKE SORTED TABLE OF gs_objek WITH UNIQUE KEY objek.

FIELD-SYMBOLS: <fs_output> LIKE gs_output.
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:s_werks FOR marc-werks  NO INTERVALS,
s_matnr FOR mara-matnr,
s_matkl FOR mara-matkl.
SELECTION-SCREEN END OF BLOCK blk1.

START-OF-SELECTION.
  PERFORM frm_selection_fields_check.
  PERFORM frm_get_data.
  PERFORM frm_process_data.

END-OF-SELECTION.
  PERFORM frm_display_alv.


FORM frm_selection_fields_check .
  IF s_werks[] IS INITIAL.
    MESSAGE s000 WITH '请输入工厂' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  "工厂权限检查
*  LOOP AT  s_werks INTO DATA(ls_werks).
*    AUTHORITY-CHECK OBJECT 'ZPP003'
*      ID 'WERKS' FIELD ls_werks-low.
*    IF sy-subrc <> 0.
*      MESSAGE s000 WITH |{ '您没有' }{ ls_werks-low }{ '工厂的权限' }| DISPLAY LIKE 'E'.
*      STOP.
*    ENDIF.
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_data.
  DATA: lt_output  LIKE TABLE OF gs_output,
        lv_index   TYPE sy-tabix,
        lv_not_bom TYPE flag.
  "根据条件查询Z050物料信息
  SELECT mara~matnr,mara~matkl,mara~meins,marc~werks,marc~ausss,marc~bstmi,marc~bstma,marc~bstfe,marc~bstrf
    FROM mara INNER JOIN marc ON mara~matnr = marc~matnr
    WHERE mara~matnr IN @s_matnr
    AND mara~mtart = 'Z050' "物料类型
    AND mara~matkl NOT IN ( '501601','5016'  ) "物料组
    AND mara~matkl IN @s_matkl
    AND mara~lvorm = '' "在客户级标记要删除的物料
    AND marc~werks IN @s_werks
    AND marc~lvorm = '' "在工厂级别将物料标记为删除
  INTO TABLE @DATA(lt_marc).

  IF lt_marc IS INITIAL.
    MESSAGE e000 WITH '未查询到Z050物料信息'.
  ENDIF.
  "查询物料BOM抬头基本信息
  SELECT  mast~matnr,mast~werks,mast~stlnr, mast~stlal,s~bmeng, s~bmein,s~stktx,s~stlst
    FROM mast
    JOIN stko AS s  ON  mast~stlnr = s~stlnr AND mast~stlal = s~stlal
    FOR ALL ENTRIES IN @lt_marc
    WHERE mast~matnr = @lt_marc-matnr
    AND mast~werks = @lt_marc-werks
    AND mast~stlan = '1'  "BOM用途
    "最小的可选BOM
    AND mast~stlal =  ( SELECT MIN( stlal ) FROM mast WHERE stlal < '09' AND matnr = @lt_marc-matnr AND werks = @lt_marc-werks AND stlan = '1')
    AND s~stlty = 'M' "BOM 类别
    AND s~datuv = ( SELECT MAX( datuv ) FROM stko WHERE stlty = 'M' AND stlnr = s~stlnr AND stlal = s~stlal AND  datuv <= @sy-datum )  "有效期
    AND s~loekz = ''    " 删除标识
  INTO TABLE @DATA(lt_stko).
  IF lt_stko IS INITIAL.
    MESSAGE e000 WITH '不存在物料BOM'.
  ENDIF.
  SORT lt_stko BY matnr werks.
  "逐个物料展BOM获取下阶2级Z030组件物料信息
  LOOP AT lt_marc INTO DATA(ls_marc).
    CLEAR lv_not_bom.
    READ TABLE lt_stko WITH KEY matnr = ls_marc-matnr werks = ls_marc-werks BINARY SEARCH INTO DATA(ls_stko).
    IF sy-subrc = 0.
      PERFORM frm_exp_bom USING  ls_stko-bmeng ls_stko-matnr ls_stko-stlal ls_stko-werks ls_marc-matkl CHANGING lt_output.
      IF lt_output IS INITIAL.
        lv_not_bom = 'X'.
      ELSE.
        INSERT LINES OF lt_output INTO TABLE gt_output.
      ENDIF.
    ELSE.
      lv_not_bom = 'X'.
    ENDIF.
    IF lv_not_bom = 'X'.
      APPEND INITIAL LINE TO gt_output ASSIGNING <fs_output>.
      CHECK <fs_output> IS ASSIGNED.
      MOVE-CORRESPONDING ls_marc TO <fs_output>.
      <fs_output>-stufe = 1.
      <fs_output>-mtart = 'Z050'.
      SELECT SINGLE maktx INTO <fs_output>-maktx FROM makt WHERE matnr = ls_marc-matnr AND spras = sy-langu.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_EXP_BOM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_exp_bom USING VALUE(iv_bmeng) TYPE stko-bmeng
                        VALUE(iv_matnr) TYPE mara-matnr
                        VALUE(iv_stlal) TYPE stko-stlal
                        VALUE(iv_werks) TYPE mast-werks
                        VALUE(iv_matkl) TYPE mara-matkl
                  CHANGING  ct_output TYPE  ty_tab_output.
  DATA: lt_stb    TYPE TABLE OF stpox,
        ls_cstmat TYPE cstmat,
        lv_ausss1 TYPE stpox-ausss,
        lv_ausss2 TYPE stpox-ausss,
        lv_stufe  TYPE stpox-stufe.
  FIELD-SYMBOLS <fs_output1> LIKE gs_output.
  REFRESH ct_output.
  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      aumng                 = 0  "输入非零的值，最上级的物料不考虑装配报废率、但下级的物料还是会考虑装配报废，0考虑装配报废，需配合AUSKZ=‘X'使用
      auskz                 = 'X'  "计算组件损耗(会考虑装配报废率，需配合AUMNG使用)
      capid                 = 'PP01'  "BOM应用
      datuv                 = sy-datum  "有效起始日
      delnl                 = abap_false  "组件删除标识
      emeng                 = iv_bmeng  "需求数量
      mehrs                 = abap_true    "多层展开标识-多层
      mtnrv                 = iv_matnr "展开物料号
      stlan                 = '1'  "BOM用途
      stlal                 = iv_stlal "可选BOM
      werks                 = iv_werks  "工厂
    IMPORTING
      topmat                = ls_cstmat
*     dstst                 = pv_dstst
    TABLES
      stb                   = lt_stb
*     matcat                = pt_matcat
    EXCEPTIONS
      alt_not_found         = 1
      call_invalid          = 2
      material_not_found    = 3
      missing_authorization = 4
      no_bom_found          = 5
      no_plant_data         = 6
      no_suitable_bom_found = 7
      conversion_error      = 8
      OTHERS                = 9.
  CHECK sy-subrc = 0.
  SORT lt_stb BY stufe."按展开的BOM级别排序
  LOOP AT lt_stb INTO DATA(ls_stb).
    CHECK ls_stb-stufe <= 2.
    IF ls_stb-mtart = 'Z030' AND ls_stb-loekz = abap_false AND ls_stb-postp = 'L' AND ls_stb-datuv <= sy-datum AND ls_stb-datub >= sy-datum AND ls_stb-mnglg > 0.
      CLEAR gs_output.
      gs_output-stufe = ls_stb-stufe + 1."BOM级别
      gs_output-matnr = ls_stb-idnrk."组件物料编码
      gs_output-matkl = ls_stb-matmk."组件物料组
      gs_output-maktx = ls_stb-ojtxp."组件物料描述
      gs_output-werks = ls_stb-werks."组件工厂
      gs_output-ausss = ls_stb-ausss."组件物料装配损耗率
      gs_output-meins = ls_stb-mmein."组件基本计量单位
      gs_output-mtart = 'Z030'.
      APPEND gs_output TO ct_output.
    ENDIF.
  ENDLOOP.
  "BOM抬头物料信息
  CLEAR gs_output.
  gs_output-stufe = 1."BOM级别
  gs_output-matnr = ls_cstmat-matnr."组件物料编码
  gs_output-matkl = iv_matkl."抬头物料物料组
  gs_output-maktx = ls_cstmat-maktx."抬头物料物料描述
  gs_output-werks = ls_cstmat-werks."抬头物料工厂
  gs_output-ausss = ls_cstmat-ausss."抬头物料装配损耗率
  gs_output-meins = ls_cstmat-emgme."抬头物料基本计量单位
  gs_output-mtart = 'Z050'.
  APPEND gs_output TO ct_output.
  SORT ct_output BY matnr.
  DELETE ADJACENT DUPLICATES FROM ct_output COMPARING matnr.
  SORT ct_output BY stufe DESCENDING."按BOM级别降序
  "物料工厂下的批量大小
  SELECT marc~matnr,marc~werks,marc~bstmi,marc~bstma,marc~bstfe,marc~bstrf
    FROM  marc  FOR ALL ENTRIES IN @ct_output
    WHERE marc~matnr = @ct_output-matnr
    AND marc~werks = @ct_output-werks
  INTO TABLE @DATA(lt_marc).
  "物料销售单位
  SELECT DISTINCT matnr,vkorg,vrkme FROM mvke FOR ALL ENTRIES IN @ct_output
    WHERE mvke~matnr = @ct_output-matnr
    AND mvke~vkorg =  @ct_output-werks
    AND mvke~vrkme IS NOT INITIAL
  INTO TABLE @DATA(lt_mvke).

  SORT lt_marc BY matnr werks.
  SORT lt_mvke BY matnr vkorg.

  LOOP AT ct_output ASSIGNING <fs_output>.
    lv_stufe = <fs_output>-stufe.
    "更新批量大小
    READ TABLE lt_marc WITH KEY matnr = <fs_output>-matnr werks = <fs_output>-werks BINARY SEARCH INTO DATA(ls_marc).
    IF sy-subrc = 0.
      <fs_output>-bstmi = ls_marc-bstmi.
      <fs_output>-bstma = ls_marc-bstma.
      <fs_output>-bstfe = ls_marc-bstfe.
      <fs_output>-bstrf = ls_marc-bstrf.
    ENDIF.
    IF lv_stufe = 1.
      <fs_output>-ausss_total = ( ( 1 + <fs_output>-ausss / 100 ) * ( 1 + lv_ausss2 / 100 ) - 1 ) * 100.
    ELSEIF lv_stufe = 2.
      <fs_output>-ausss_total = ( ( 1 + <fs_output>-ausss / 100 ) * ( 1 + lv_ausss1 / 100 ) - 1 ) * 100.
      lv_ausss2 = <fs_output>-ausss_total.
    ELSEIF lv_stufe = 3.
      <fs_output>-ausss_total = <fs_output>-ausss.
      lv_ausss1 =  <fs_output>-ausss.
    ENDIF.
    "Seashell 20230112 新增首个50-30组件损耗
    DELETE lt_stb WHERE STLAL = '09'.
    READ TABLE lt_stb INTO ls_stb WITH KEY mtart = 'Z030'
                                           stufe = <fs_output>-stufe.
    IF sy-subrc = 0.
      <fs_output>-first_sh = ls_stb-ausch.
      <fs_output>-ausss_total = <fs_output>-ausss_total * ( 1 + <fs_output>-first_sh / 100 ).
    ENDIF.
    "end

    CLEAR ls_stb.
  ENDLOOP.
  SORT ct_output BY stufe ASCENDING.
  LOOP AT ct_output ASSIGNING <fs_output>.
    lv_stufe = <fs_output>-stufe.
    "更新批量大小
    IF lv_stufe = 2.
      IF <fs_output>-ausss_total <> 0.
        <fs_output>-bstmin = floor( <fs_output>-bstmi / ( 1 + <fs_output>-ausss_total / 100 ) ) ."最小批量净产出 bstmin =  bstmi / ausss_total
        <fs_output>-bstman =  floor( <fs_output>-bstma / ( 1 + <fs_output>-ausss_total / 100 ) ). "最大批量净产出 bstman =  bstma / ausss_total
        <fs_output>-bstfen = floor( <fs_output>-bstfe / ( 1 + <fs_output>-ausss_total / 100 ) ). "固定批量净产出 bstfen =  bstfe / ausss_total
        READ TABLE ct_output WITH KEY stufe = 1 ASSIGNING <fs_output1>.
        IF sy-subrc = 0.
          <fs_output1>-bstman = floor( <fs_output>-bstman )."第1行的最大批量净产出
          <fs_output1>-bstmin = floor( <fs_output>-bstmin )."第1行的最小批量净产出
          <fs_output1>-bstfen = floor( <fs_output>-bstfen )."第1行的固定批量净产出

          "销售订单单位数量转换
          READ TABLE lt_mvke WITH KEY matnr = <fs_output1>-matnr vkorg = <fs_output>-werks BINARY SEARCH INTO DATA(ls_mvke).
          IF sy-subrc = 0.
            <fs_output1>-vrkme = ls_mvke-vrkme."销售单位
            CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
              EXPORTING
                input   = 1
                kzmeinh = 'X' "基本单位数量 / 销售单位
                matnr   = <fs_output1>-matnr
                meinh   = ls_mvke-vrkme
                meins   = <fs_output1>-meins
              IMPORTING
                output  = <fs_output1>-umrez.
            IF <fs_output1>-umrez <> 0.
              "建议下单最小批量
              <fs_output1>-bstmis = floor( <fs_output1>-bstmin / <fs_output1>-umrez ).
              <fs_output1>-bstmas = floor( <fs_output1>-bstman / <fs_output1>-umrez ).
              <fs_output1>-bstfes = floor( <fs_output1>-bstfen / <fs_output1>-umrez ).
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSEIF lv_stufe = 3.
      READ TABLE ct_output WITH KEY stufe = 2  ASSIGNING <fs_output1>.
      IF sy-subrc = 0.
        <fs_output>-bstmin = floor(  <fs_output1>-bstmin )."第3级的最小批量净产出
        <fs_output>-bstman = floor(  <fs_output1>-bstman )."第3级的最大批量净产出
        <fs_output>-bstfen = floor(  <fs_output1>-bstfen )."第3级的固定批量净产出
      ENDIF.

    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PROCESS_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_process_data .
  DATA: lt_matnr  LIKE TABLE OF gs_matnr,
        lt_maktxl LIKE TABLE OF gs_maktxl,
        lv_index  TYPE sy-tabix,
        lt_tline  TYPE TABLE OF tline.
  FIELD-SYMBOLS <fs_maktxl> LIKE gs_maktxl.
  MOVE-CORRESPONDING gt_output TO lt_matnr.
  "物料长文本
  SORT lt_matnr BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_matnr COMPARING matnr.
  IF lt_matnr IS NOT INITIAL.
    SELECT matnr,groes FROM mara FOR ALL ENTRIES IN @lt_matnr
      WHERE matnr = @lt_matnr-matnr
      INTO TABLE @DATA(lt_groes).
    SORT lt_groes BY matnr.
  ENDIF.

  LOOP AT lt_matnr INTO gs_matnr.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'GRUN'
        language                = sy-langu
        name                    = CONV tdobname( gs_matnr-matnr )
        object                  = 'MATERIAL'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
* IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = lt_tline
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc = 0.
      APPEND INITIAL LINE TO lt_maktxl ASSIGNING <fs_maktxl>.
      <fs_maktxl>-matnr = gs_matnr-matnr.
      LOOP AT lt_tline INTO DATA(ls_tline).
        <fs_maktxl>-maktxl = |{ <fs_maktxl>-maktxl }{ ls_tline-tdline }|.
      ENDLOOP.
    ENDIF.
    CHECK gs_matnr-mtart = 'Z030'.
    "AUSP查询物料分类的对象主键
    gs_objek-objek = CONV char90( gs_matnr-matnr ).
    INSERT gs_objek INTO TABLE gt_objek_001.
  ENDLOOP.
  IF gt_objek_001 IS NOT INITIAL.
    "从AUSP读取物料001分类特征值
    SELECT ausp~objek,cabn~atnam,ausp~atwrt
    FROM cabn
    INNER JOIN ausp ON cabn~atinn = ausp~atinn
    FOR ALL ENTRIES IN @gt_objek_001
    WHERE cabn~atnam IN ( 'ZTPY','ZMOLD'  ) "类别
    AND ausp~objek  =  @gt_objek_001-objek
    AND ausp~klart = '001' "物料分类
    INTO TABLE @DATA(lt_ausp_001).
  ENDIF.
  SORT lt_maktxl BY matnr.
  SORT lt_ausp_001 BY objek atnam.
  LOOP AT gt_output ASSIGNING <fs_output>.
    lv_index = sy-tabix.
    "Z030物料分类的类别ZTPY的特征值不是S的不显示
    IF <fs_output>-mtart = 'Z030'.
      READ TABLE lt_ausp_001 WITH KEY objek = CONV char90( <fs_output>-matnr ) atnam = 'ZTPY'  BINARY SEARCH INTO DATA(ls_ausp_001).
      IF sy-subrc = 0 .
        TRANSLATE ls_ausp_001-atwrt TO UPPER CASE.
        IF ls_ausp_001-atwrt <> 'S'.
          DELETE gt_output INDEX lv_index.
          CONTINUE.
        ENDIF.
      ELSE.
        DELETE gt_output INDEX lv_index.
        CONTINUE.
      ENDIF.
    ENDIF.
    READ TABLE lt_ausp_001 WITH KEY objek = CONV char90( <fs_output>-matnr ) atnam = 'ZMOLD'  BINARY SEARCH INTO ls_ausp_001.
    IF sy-subrc = 0 .
      <fs_output>-zmold = ls_ausp_001-atwrt.
    ENDIF.
    "长描述
    READ TABLE lt_maktxl WITH KEY matnr = <fs_output>-matnr BINARY SEARCH INTO gs_maktxl.
    IF sy-subrc = 0.
      <fs_output>-maktxl = gs_maktxl-maktxl.
    ENDIF.
    "大小/量纲
    READ TABLE lt_groes WITH KEY matnr = <fs_output>-matnr BINARY SEARCH INTO DATA(ls_groes).
    IF sy-subrc = 0.
      <fs_output>-groes = ls_groes-groes.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DISPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_alv .
  "1.创建SALV对象
  cl_salv_table=>factory( IMPORTING r_salv_table = go_salv_table
                          CHANGING  t_table = gt_output ).

  "2.保存布局设置
  go_salv_table->get_layout( )->set_key( VALUE salv_s_layout_key( report = sy-repid ) ).
  go_salv_table->get_layout( )->set_save_restriction( cl_salv_layout=>restrict_none ).
  go_salv_table->get_display_settings( )->set_list_header( '拆弹批量报表 注：物料组501601,5016的产品不参与查询' )."设置标题

  "3.字段目录属性设置
  go_salv_columns = go_salv_table->get_columns( ).
  go_salv_columns->set_optimize( abap_true ). "列宽自动优化
  "设置字段标签文本
  PERFORM frm_set_col_text USING 'ZMOLD' '模具'.
  PERFORM frm_set_col_text USING 'MAKTXL' '物料长描述'.
  PERFORM frm_set_col_text USING 'FIRST_SH' '首个50_30组件损耗'.
  PERFORM frm_set_col_text USING 'AUSSS_TOTAL' '合并装配报废百分比'.
  PERFORM frm_set_col_text USING 'BSTMIN' '最小批量净产出'.
  PERFORM frm_set_col_text USING 'BSTMAN' '最大批量净产出'.
  PERFORM frm_set_col_text USING 'BSTFEN' '固定批量净产出'.
  PERFORM frm_set_col_text USING 'BSTMIS' '建议下单最小批量'.
  PERFORM frm_set_col_text USING 'BSTMAS' '建议下单最大批量'.
  PERFORM frm_set_col_text USING 'BSTFES' '建议固定下单批量'.
  PERFORM frm_set_col_text USING 'UMREZ' '基本单位/销售单位'.
  "4.选择模式
  go_salv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column )."行与字段的选择模式，报表左边追加了可以选择行的MARK按钮
  "5.设置ALV工具栏
  go_salv_table->get_functions( )->set_all( abap_true ).
  "6.显示SALV
  go_salv_table->display( ).
ENDFORM.
FORM frm_set_col_text USING iv_colname TYPE lvc_fname iv_coltext  TYPE scrtext_m.
  DATA(lo_column) = go_salv_columns->get_column( iv_colname ).
  lo_column->set_short_text( CONV scrtext_s( iv_coltext ) ).
  lo_column->set_medium_text( CONV scrtext_m( iv_coltext ) ).
  lo_column->set_long_text( CONV scrtext_l( iv_coltext ) ).
ENDFORM.