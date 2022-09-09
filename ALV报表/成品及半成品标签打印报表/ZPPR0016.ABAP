*&---------------------------------------------------------------------*
*& Report ZPPR0016
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*****************************************************************
*  Modified Recorder :
*  Date      C#NO        Author      Content
*  --------  ----------  ----------  ---------------
*  20210721  S4DK904998	 05168       PP:ZPP016报表修改 20210721
*****************************************************************
REPORT zppr0016.
TABLES afpo.
DATA:BEGIN OF gs_output,
       matnr       TYPE afpo-matnr, "物料号
       dwerk       TYPE afpo-dwerk, "工厂
       charg       TYPE afpo-charg, "批次号
       vfdat       TYPE mcha-vfdat, "货架寿命到期日
       hsdat       TYPE mcha-hsdat, "生产日期
       zpspe       TYPE char70, "包装规格，物料类型为Z030去批次分类ZBATCH_SIRIO的特征值ZPACK_STA，Z050取物料分类的ZPSPE特征值
       zallergen   TYPE char70, "过敏原,物料分类的ZALLERGEN特征值
       ztrsgen     TYPE char4, "转基因,物料分类的ZTRSGEN特征值
       mtart       TYPE mara-mtart, "物料类型
       matkl       TYPE mara-matkl, "物料组
       mhdhb       TYPE mara-mhdhb, "总货架寿命
       groes       TYPE mara-groes, "规格
       meins       TYPE mara-meins, "基本计量单位
       zbqgg       TYPE char120, "标签规格
       maktx       TYPE  makt-maktx, "物料描述
       znote       TYPE char10, "备注
       zxianghao   TYPE char10, "箱号
       zbatchglno  TYPE char15, "批号管理台账编号
       zerweima    TYPE char100, "二维码内容
       zcpdypc     TYPE char20, "成品打印批次，BATCH_CP批次分类的Z_BATCH_DYPC特征值
       kdauf       TYPE afpo-kdauf, "销售订单号
       kdpos       TYPE afpo-kdpos, "销售订单行项目
       aufnr       TYPE  afpo-aufnr, "流程订单号
       zstdname    TYPE char70, "标准名称,物料分类01的zstdname特征值
       baozhiqi    TYPE mhdhb, "保质期 MARA-MHDHB/30取整
       raube       TYPE mara-raube, "存储条件代码
       cctj        TYPE  zqmt011-cctj, "存储条件
       "Seashell 20220907 新增字段
       produceline(2), "产线
       zwsbs(6),"尾数标识

       znote1      TYPE char10, "备注2
       znote2      TYPE char10, "备注3
     END OF gs_output.
DATA:BEGIN OF gs_matnr,
       matnr TYPE mara-matnr,
       raube TYPE mara-raube, "存储条件代码
     END OF gs_matnr.

DATA: go_salv_table TYPE REF TO cl_salv_table,
      gt_output     LIKE TABLE OF gs_output,
      gt_matnr      LIKE TABLE OF gs_matnr,
      l_key(90),
      lv_unit_name  TYPE char30,
      gs_value      TYPE bapi1003_alloc_values_char,
      gt_value      TYPE TABLE OF bapi1003_alloc_values_char,
      gt_value01    TYPE TABLE OF bapi1003_alloc_values_char,
      zmatnr(10)    TYPE c,
      zkdauf(10)    TYPE c.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
PARAMETERS p_dwerk TYPE afpo-dwerk OBLIGATORY.
SELECT-OPTIONS:s_aufnr FOR afpo-aufnr,
               s_matnr FOR afpo-matnr,
               s_charg FOR afpo-charg,
               s_kdauf FOR afpo-kdauf,
               s_kdpos FOR afpo-kdpos.
SELECTION-SCREEN END OF BLOCK blk1.

AT SELECTION-SCREEN.
  AUTHORITY-CHECK OBJECT 'ZPP003'
   ID 'WERKS' FIELD p_dwerk.
  IF sy-subrc <> 0.
    MESSAGE |{ '您没有' }{ p_dwerk }{ '工厂的权限!' }| TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  PERFORM frm_get_data.

END-OF-SELECTION.
  PERFORM frm_display_alv.

FORM frm_get_data.
**  SELECT afpo~matnr,afpo~dwerk,afpo~charg ,mara~mtart,mara~matkl,mara~mhdhb,mara~groes,mara~meins,afpo~kdauf,afpo~kdpos,afpo~aufnr,mara~raube
**  FROM afpo
**  LEFT JOIN mara ON afpo~matnr = mara~matnr
**  WHERE afpo~dwerk = @p_dwerk
**  AND afpo~aufnr IN @s_aufnr
**  AND afpo~matnr IN @s_matnr
**  AND afpo~charg IN @s_charg
**  AND afpo~kdauf IN @s_kdauf
**  AND afpo~kdpos IN @s_kdpos
**  AND afpo~xloek = '' "删除标识
***  AND NOT EXISTS
***    ( SELECT inact
***       FROM aufk
***       JOIN jest ON aufk~objnr = jest~objnr
***       WHERE aufk~aufnr = afpo~aufnr
***       AND jest~stat IN ( 'I0045','I0046' ) AND jest~inact = ''
***     )
**  INTO CORRESPONDING FIELDS OF TABLE @gt_output.

  "Seashell 20220907 上面注释，使用下面的新逻辑，新增产线，同个产品可能在不同的产线上进行生产
  SELECT
    afpo~matnr,afpo~dwerk,afpo~charg ,mara~mtart,mara~matkl,
    mara~mhdhb,mara~groes,mara~meins,afpo~kdauf,afpo~kdpos,afpo~aufnr,mara~raube,
    zppt0003~produceline
    FROM afpo
    LEFT JOIN mara ON afpo~matnr = mara~matnr
    INNER JOIN zppt0003
    ON zppt0003~matnr = afpo~matnr
    AND zppt0003~werks = afpo~dwerk
    AND zppt0003~charg = afpo~charg
    WHERE afpo~dwerk = @p_dwerk
    AND afpo~aufnr IN @s_aufnr
    AND afpo~matnr IN @s_matnr
    AND afpo~charg IN @s_charg
    AND afpo~kdauf IN @s_kdauf
    AND afpo~kdpos IN @s_kdpos
    AND afpo~xloek = '' "删除标识
    INTO CORRESPONDING FIELDS OF TABLE @gt_output.


  IF gt_output IS NOT INITIAL.
    "获取物料描述
    MOVE-CORRESPONDING gt_output TO gt_matnr.
    SORT gt_matnr BY matnr.
    DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr.
    SELECT matnr,maktx FROM makt FOR ALL ENTRIES IN @gt_matnr
      WHERE matnr = @gt_matnr-matnr
      AND spras = @sy-langu
    INTO TABLE @DATA(lt_makt).
    "获取存储条件值
    DELETE gt_matnr WHERE raube = ''.
    IF gt_matnr IS NOT INITIAL.
      SORT gt_matnr BY raube.
      DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING raube.
      SELECT sc AS raube, cctj FROM zqmt011 FOR ALL ENTRIES IN @gt_matnr
        WHERE spras = @sy-langu
          AND sc = @gt_matnr-raube
      INTO TABLE @DATA(lt_qmt011).
    ENDIF.
    "获取批次表的生产日期、货架寿命到期日
    SELECT matnr,werks,charg,hsdat,vfdat FROM mcha FOR ALL ENTRIES IN @gt_output
      WHERE matnr = @gt_output-matnr
        AND werks  = @gt_output-dwerk
        AND charg = @gt_output-charg
    INTO TABLE @DATA(lt_mcha).

    "Seashell 20220907 获取产线
*    SELECT matnr,werks,charg,produceline,aufnr FROM zppt0003 FOR ALL ENTRIES IN @gt_output
*      WHERE matnr = @gt_output-matnr
*        AND werks  = @gt_output-dwerk
*        AND charg = @gt_output-charg
*        AND aufnr = @gt_output-aufnr
*    INTO TABLE @DATA(lt_zppt0003).

    "扩充输出报表字段
    SORT: lt_makt BY matnr,
          lt_mcha BY matnr werks charg,
          lt_qmt011 BY raube.
    LOOP AT  gt_output ASSIGNING FIELD-SYMBOL(<fs_output>).
      CLEAR:gs_value, gt_value,gt_value01.
      "物料分类特征值
      gt_value01 = zcl_bc_public=>get_class_values( key = <fs_output>-matnr objtable = 'MARA' classtype = '001' classnum = <fs_output>-matkl ).
      READ TABLE gt_value01 WITH KEY charact = 'ZPSPE' INTO gs_value.
      IF sy-subrc = 0.
        <fs_output>-zpspe = gs_value-value_char_long."包装规格
      ENDIF.
      "Seashell 20220907 新增字段
      "产线
*      READ TABLE lt_zppt0003 INTO DATA(ls_zppt0003) WITH KEY matnr = <fs_output>-matnr werks = <fs_output>-dwerk charg = <fs_output>-charg aufnr = <fs_output>-aufnr.
*      IF sy-subrc = 0.
*        <fs_output>-produceline = ls_zppt0003-produceline.
*        CLEAR ls_zppt0003.
*      ENDIF.
      "尾数标识
      <fs_output>-zwsbs = '0'.

      "批次特征值
      IF <fs_output>-charg IS NOT INITIAL.
        READ TABLE lt_mcha WITH KEY  matnr = <fs_output>-matnr werks = <fs_output>-dwerk charg = <fs_output>-charg BINARY SEARCH INTO DATA(ls_mcha).
        IF sy-subrc = 0.
          <fs_output>-vfdat = ls_mcha-vfdat."货架寿命到期日
          <fs_output>-hsdat = ls_mcha-hsdat."生产日期
        ENDIF.
        "分类特征值
        l_key(40) = <fs_output>-matnr.
        l_key+40(4) = <fs_output>-dwerk.
        l_key+44(10) = <fs_output>-charg.
        IF <fs_output>-mtart = 'Z050'."成品
          "打印批次
          gt_value = zcl_bc_public=>get_class_values( key = l_key classnum = 'BATCH_CP' classtype = '022' objtable = 'MCHA' ).
          READ TABLE gt_value WITH KEY charact = 'Z_BATCH_DYPC' INTO gs_value.
          IF sy-subrc = 0.
            <fs_output>-zcpdypc = gs_value-value_char_long."打印批次
          ENDIF.
*START S4DK905039
        ELSEIF <fs_output>-mtart = 'Z030'."半成品
          gt_value = zcl_bc_public=>get_class_values( key = l_key classnum = 'ZBATCH_SIRIO' classtype = '022' objtable = 'MCHA' ).
          READ TABLE gt_value WITH KEY charact = 'ZPACK_STA' INTO gs_value.
          IF sy-subrc = 0.
            <fs_output>-zpspe = gs_value-value_char_long."包装规格
          ENDIF.
*END S4DK905039
        ENDIF.
      ENDIF.
      "物料描述
      READ TABLE lt_makt WITH KEY matnr = <fs_output>-matnr BINARY SEARCH INTO DATA(ls_makt).
      IF  sy-subrc = 0.
        <fs_output>-maktx = ls_makt-maktx.
      ENDIF.
      "去空格
      CONDENSE <fs_output>-groes NO-GAPS.
      CONDENSE <fs_output>-zpspe NO-GAPS.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input          = <fs_output>-meins
          language       = sy-langu
        IMPORTING
*         LONG_TEXT      =
*         output         = <fs_output>-meins
          short_text     = lv_unit_name
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      <fs_output>-zbqgg = |{ <fs_output>-groes }{ '/' }{ lv_unit_name }{ '*' }{ <fs_output>-zpspe }|."标签规格
      "存储条件
      READ TABLE lt_qmt011 WITH KEY raube = <fs_output>-raube BINARY SEARCH INTO DATA(ls_qmt011).
      IF sy-subrc = 0.
        <fs_output>-cctj = ls_qmt011-cctj.
      ENDIF.

      "物料分类特征值
      IF gt_value01 IS  INITIAL."在上面Z050条件判断是否取值过
        gt_value01 = zcl_bc_public=>get_class_values( key = <fs_output>-matnr objtable = 'MARA' classtype = '001' classnum = <fs_output>-matkl ).
      ENDIF.
      IF gt_value01 IS NOT INITIAL.
        LOOP AT gt_value01 INTO gs_value.
          CASE gs_value-charact.
            WHEN 'ZALLERGEN'."过敏原
              <fs_output>-zallergen = gs_value-value_char_long.
            WHEN 'ZTRSGEN'. "转基因
              <fs_output>-ztrsgen = gs_value-value_char.
            WHEN 'ZSTDNAME'."标准名称
              <fs_output>-zstdname = gs_value-value_char_long.
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.
      ENDIF.

      "保质期 MARA-MHDHB/30取整
      <fs_output>-baozhiqi = floor( <fs_output>-mhdhb / 30  ).
      "二维码取值： 物料%批次-销售订单号 S4DK904998
      CLEAR: zmatnr,zkdauf.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = <fs_output>-matnr
        IMPORTING
          output = zmatnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = <fs_output>-kdauf
        IMPORTING
          output = zkdauf.
      <fs_output>-zerweima = zmatnr && '%' && <fs_output>-charg && '-' && zkdauf.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM frm_display_alv.
  "1.创建SALV对象
  cl_salv_table=>factory( IMPORTING r_salv_table = go_salv_table
                          CHANGING  t_table = gt_output ).
  "2.保存布局设置
*    gs_program-report = sy-repid.
  go_salv_table->get_layout( )->set_key( VALUE #( report = sy-repid ) ).
  go_salv_table->get_layout( )->set_save_restriction( cl_salv_layout=>restrict_none ).
  "3.字段目录属性设置
  DATA(lo_columns) = go_salv_table->get_columns( ).
  lo_columns->set_optimize( abap_true ). "列宽自动优化

  DATA(lo_col_zpspe) = lo_columns->get_column( 'ZPSPE' ).
  lo_col_zpspe->set_short_text( '包装规格' ).
  lo_col_zpspe->set_medium_text( '包装规格' ).
  lo_col_zpspe->set_long_text( '包装规格' ).
  DATA(lo_col_groes) = lo_columns->get_column( 'GROES' ).
  lo_col_groes->set_short_text( '规格' ).
  lo_col_groes->set_medium_text( '规格' ).
  lo_col_groes->set_long_text( '规格' ).
  DATA(lo_col_zbqgg) = lo_columns->get_column( 'ZBQGG' ).
  lo_col_zbqgg->set_short_text( '打印标签规格' ).
  lo_col_zbqgg->set_medium_text( '打印标签规格' ).
  lo_col_zbqgg->set_long_text( '打印标签规格' ).
  DATA(lo_col_zallergen) = lo_columns->get_column( 'ZALLERGEN' ).
  lo_col_zallergen->set_short_text( '过敏原' ).
  lo_col_zallergen->set_medium_text( '过敏原' ).
  lo_col_zallergen->set_long_text( '过敏原' ).
  DATA(lo_col_ztrsgen) = lo_columns->get_column( 'ZTRSGEN' ).
  lo_col_ztrsgen->set_short_text( '转基因' ).
  lo_col_ztrsgen->set_medium_text( '转基因' ).
  lo_col_ztrsgen->set_long_text( '转基因' ).
  DATA(lo_col_znote) = lo_columns->get_column( 'ZNOTE' ).
  lo_col_znote->set_short_text( '备注' ).
  lo_col_znote->set_medium_text( '备注' ).
  lo_col_znote->set_long_text( '备注' ).
  DATA(lo_col_zxianghao) = lo_columns->get_column( 'ZXIANGHAO' ).
  lo_col_zxianghao->set_short_text( '箱号' ).
  lo_col_zxianghao->set_medium_text( '箱号' ).
  lo_col_zxianghao->set_long_text( '箱号' ).
  DATA(lo_col_zbatchglno) = lo_columns->get_column( 'ZBATCHGLNO' ).
  lo_col_zbatchglno->set_short_text( '批号管理台账编号' ).
  lo_col_zbatchglno->set_medium_text( '批号管理台账编号' ).
  lo_col_zbatchglno->set_long_text( '批号管理台账编号' ).
  DATA(lo_col_zerweima) = lo_columns->get_column( 'ZERWEIMA' ). "S4DK904998
  lo_col_zerweima->set_short_text( '二维码内容' ).
  lo_col_zerweima->set_medium_text( '二维码内容' ).
  lo_col_zerweima->set_long_text( '二维码内容' ).
  DATA(lo_col_zcpdypc) = lo_columns->get_column( 'ZCPDYPC' ).
  lo_col_zcpdypc->set_short_text( '成品打印批次' ).
  lo_col_zcpdypc->set_medium_text( '成品打印批次' ).
  lo_col_zcpdypc->set_long_text( '成品打印批次' ).
  DATA(lo_col_zstdname) = lo_columns->get_column( 'ZSTDNAME' ).
  lo_col_zstdname->set_short_text( '标准名称' ).
  lo_col_zstdname->set_medium_text( '标准名称' ).
  lo_col_zstdname->set_long_text( '标准名称' ).
  DATA(lo_col_baozhiqi) = lo_columns->get_column( 'BAOZHIQI' ).
  lo_col_baozhiqi->set_short_text( '保质期(月)' ).
  lo_col_baozhiqi->set_medium_text( '保质期(月)' ).
  lo_col_baozhiqi->set_long_text( '保质期(月)' ).
  DATA(lo_col_cctj) = lo_columns->get_column( 'CCTJ' ).
  lo_col_cctj->set_short_text( '存储条件描述' ).
  lo_col_cctj->set_medium_text( '存储条件描述' ).
  lo_col_cctj->set_long_text( '存储条件描述' ).
  "Seashell 20220907 新增字段
  DATA(lo_col_produceline) = lo_columns->get_column( 'PRODUCELINE' ).
  lo_col_produceline->set_short_text( '产线' ).
  lo_col_produceline->set_medium_text( '产线' ).
  lo_col_produceline->set_long_text( '产线' ).
  DATA(lo_col_zwsbs) = lo_columns->get_column( 'ZWSBS' ).
  lo_col_zwsbs->set_short_text( '尾数标识' ).
  lo_col_zwsbs->set_medium_text( '尾数标识' ).
  lo_col_zwsbs->set_long_text( '尾数标识' ).


  DATA(lo_col_znote1) = lo_columns->get_column( 'ZNOTE1' ).
  lo_col_znote1->set_short_text( '备注1' ).
  lo_col_znote1->set_medium_text( '备注1' ).
  lo_col_znote1->set_long_text( '备注1' ).
  DATA(lo_col_znote2) = lo_columns->get_column( 'ZNOTE2' ).
  lo_col_znote2->set_short_text( '备注2' ).
  lo_col_znote2->set_medium_text( '备注2' ).
  lo_col_znote2->set_long_text( '备注2' ).
*    lo_columns->get_column( 'ZINDEX' )->set_technical( abap_true ).
  "4.选择模式
  go_salv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column )."行与字段的选择模式，报表左边追加了可以选择行的MARK按钮
  "5.设置ALV工具栏
  go_salv_table->get_functions( )->set_all( abap_true ).
  "6.显示SALV
  go_salv_table->display( ).

ENDFORM.