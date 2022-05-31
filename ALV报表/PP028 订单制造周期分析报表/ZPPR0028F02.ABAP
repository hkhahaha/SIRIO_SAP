*&---------------------------------------------------------------------*
*& 包含               ZMMR0020F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  frm_SHOW_DATA
*&---------------------------------------------------------------------*
*       显示ALV
*----------------------------------------------------------------------*
FORM frm_show_data.
    DATA:lo_col_vbeln     TYPE REF TO cl_salv_column_table.
    "1.创建SALV对象
    cl_salv_table=>factory( IMPORTING r_salv_table = go_salv_table
                            CHANGING  t_table = gt_alv_output ).
 
    "2.保存布局设置
    DATA(lo_layout) = go_salv_table->get_layout( ).
    lo_layout->set_default( abap_true )."保存布局显示缺省选项框
    lo_layout->set_key( VALUE salv_s_layout_key( report = sy-repid ) ).
    lo_layout->set_save_restriction( cl_salv_layout=>restrict_none ).
    go_salv_table->get_display_settings( )->set_list_header( '订单制造周期分析报表' )."设置标题
 
    "3.字段目录属性设置
    go_salv_columns = go_salv_table->get_columns( ).
    go_salv_columns->get_column( 'OBJNR' )->set_technical( abap_true ).
    go_salv_columns->set_optimize( abap_true ). "列宽自动优化
    lo_col_vbeln ?= go_salv_columns->get_column( 'VBELN' ).
    lo_col_vbeln->set_cell_type( if_salv_c_cell_type=>hotspot ).
 
    "SEASHELL 20220530 设置隐藏列
    go_salv_columns->get_column( 'PLIFZ' )->set_visible( if_salv_c_bool_sap=>false ).
    go_salv_columns->get_column( 'VKORG' )->set_visible( if_salv_c_bool_sap=>false ).
    "设置字段标签文本
    PERFORM frm_set_col_text USING 'KUNNR' '实际客户编号'.
    PERFORM frm_set_col_text USING 'KUNNR_DEC' '实际客户名称'.
    PERFORM frm_set_col_text USING 'BU_SORT1' '实际客户的检索项'.
    PERFORM frm_set_col_text USING 'KUKLATXT' '客户分类描述'.
    PERFORM frm_set_col_text USING 'BEZEI' '销售办事处描述'.
    PERFORM frm_set_col_text USING 'TXT30' '订单审核状态'.
    PERFORM frm_set_col_text USING 'YWY' '业务员编号'.
    PERFORM frm_set_col_text USING 'YWY_DEC' '业务员名称'.
    PERFORM frm_set_col_text USING 'ERNAMT' '创建人名称'.
    PERFORM frm_set_col_text USING 'BMENG' '销售数量基本单位'.
    PERFORM frm_set_col_text USING 'MEINS' '基本单位'.
    PERFORM frm_set_col_text USING 'UDATE' '二审日期'.
    PERFORM frm_set_col_text USING 'UTIME' '二审时间'.
    PERFORM frm_set_col_text USING 'DDTEXT' '备货策略描述'.
    PERFORM frm_set_col_text USING 'ZPRO' '是否益生菌'.
    PERFORM frm_set_col_text USING 'BUDAT_ZAO' '最早入库日期'.
    PERFORM frm_set_col_text USING 'BUDAT_WAN' '最晚入库日期'.
    PERFORM frm_set_col_text USING 'GREUNAME' '入库申请单创建者'.
    PERFORM frm_set_col_text USING 'BUDAT_B' '首次收货过账日期'.
    PERFORM frm_set_col_text USING 'CPUTM_B' '首次收货过账时间'.
    PERFORM frm_set_col_text USING 'BUDAT_E' '末次收货过账日期'.
    PERFORM frm_set_col_text USING 'CPUTM_E' '末次收货过账时间'.
    PERFORM frm_set_col_text USING 'WEMNG' 'M流程订单入库量'.
    PERFORM frm_set_col_text USING 'YFHSL' '已发货数量'.
    PERFORM frm_set_col_text USING 'WFHSL' '未发货数量'.
    PERFORM frm_set_col_text USING 'MATKL_4' '剂型编号'.
    PERFORM frm_set_col_text USING 'CPXL_DEC' '成品小类描述'.
    PERFORM frm_set_col_text USING 'MJX_DEC' '剂型'.
    PERFORM frm_set_col_text USING 'SJZQ' 'M实际周期'.
    PERFORM frm_set_col_text USING 'MATNR_M' 'm物料编码'.
    PERFORM frm_set_col_text USING 'MAKTX_M' 'm描述'.
    PERFORM frm_set_col_text USING 'SL_M' 'm数量'.
    PERFORM frm_set_col_text USING 'MEINS_M' 'm单位'.
    PERFORM frm_set_col_text USING 'MATNR_S' '对应S码'.
    PERFORM frm_set_col_text USING 'MAKTX_S' 'S描述'.
    PERFORM frm_set_col_text USING 'MATKL_S' 'S小类编号'.
    PERFORM frm_set_col_text USING 'MATKL_S4' 'S中类编号'.
    PERFORM frm_set_col_text USING 'BCPXL_DEC' 'S小类描述'.
    PERFORM frm_set_col_text USING 'SJX_DEC' 'S中类描述'.
    PERFORM frm_set_col_text USING 'SL_S' 'S数量'.
    PERFORM frm_set_col_text USING 'MEINS_S' 'S单位'.
    PERFORM frm_set_col_text USING 'ZMATSCAT_S' '压制时间'.
    PERFORM frm_set_col_text USING 'MATNR_YFL' '原辅料物料编码'.
    PERFORM frm_set_col_text USING 'MAKTX_YFL' '原辅料物料描述'.
    PERFORM frm_set_col_text USING 'RUEZT_YFL' '原辅料准备时间'.
    PERFORM frm_set_col_text USING 'PLIFZ_YFL' '原辅料采购周期'.
    PERFORM frm_set_col_text USING 'WEBAZ_YFL' '原辅料检验周期'.
    PERFORM frm_set_col_text USING 'MATNR_NB' '内包物料编码'.
    PERFORM frm_set_col_text USING 'RUEZT_NB' '内包物料准备时间'.
    PERFORM frm_set_col_text USING 'MAKTX_NB' '内包物料描述'.
    PERFORM frm_set_col_text USING 'PLIFZ_NB' '内包物料采购周期'.
    PERFORM frm_set_col_text USING 'WEBAZ_NB' '内包物料检验周期'.
    PERFORM frm_set_col_text USING 'MATNR_WB' '外包物料编码'.
    PERFORM frm_set_col_text USING 'MAKTX_WB' '外包物料描述'.
    PERFORM frm_set_col_text USING 'RUEZT_WB' '外包物料准备时间'.
    PERFORM frm_set_col_text USING 'PLIFZ_WB' '外包物料采购周期'.
    PERFORM frm_set_col_text USING 'WEBAZ_WB' '外包物料检验周期'.
    PERFORM frm_set_col_text USING 'ZSCBLDH' '是否有手工备料'.
    PERFORM frm_set_col_text USING 'LTXTS' '销售订单行项目长文本'.
    PERFORM frm_set_col_text USING 'SJGZ' '保健品送检规则'.
    PERFORM frm_set_col_text USING 'PMATN' '定价参考物料'.
    PERFORM frm_set_col_text USING 'PMATNX' '定价参考物料描述'.
    PERFORM frm_set_col_text USING 'ZAPPSOU' '批文来源'.
    "4.选择模式
    go_salv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column )."行与字段的选择模式，报表左边追加了可以选择行的MARK按钮
    "5.设置ALV工具栏
    go_salv_table->get_functions( )->set_all( abap_true ).
    "6.注册事件
    SET HANDLER go_event_receiver->handle_link_click FOR go_salv_table->get_event( ).
    "7.显示SALV
    go_salv_table->display( ).
  ENDFORM. " frm_SHOW_DATA
  FORM frm_set_col_text USING iv_colname TYPE lvc_fname iv_coltext  TYPE scrtext_s.
    DATA(lo_column) = go_salv_columns->get_column( iv_colname ).
    lo_column->set_short_text( iv_coltext ).
    lo_column->set_medium_text( CONV scrtext_m( iv_coltext ) ).
    lo_column->set_long_text( CONV scrtext_l( iv_coltext ) ).
  ENDFORM.