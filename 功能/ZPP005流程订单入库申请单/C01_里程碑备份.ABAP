*&---------------------------------------------------------------------*
*& 包含          ZPPR0005_C01
*&---------------------------------------------------------------------*
*入库申请单单据-类
CLASS zcl_pro_grreq_bill DEFINITION.
    PUBLIC SECTION.
      DATA prompt_when_exit TYPE char1. "退出单据时，提示信息标识
      INTERFACES zif_custom_bill_common.
      ALIASES:
              bill_status   FOR zif_custom_bill_common~bill_status ,
              cns_status_created  FOR zif_custom_bill_common~cns_status_created,
              cns_status_changed  FOR zif_custom_bill_common~cns_status_changed,
              cns_status_display  FOR zif_custom_bill_common~cns_status_display,
              save_bill FOR zif_custom_bill_common~save_bill.
  
  
      "声明事件
      EVENTS:bill_status_changed EXPORTING VALUE(e_bill_status) TYPE i VALUE(e_refresh_alv) TYPE char1 DEFAULT abap_false."单据状态改变时事件
      CLASS-EVENTS after_save EXPORTING VALUE(es_bapiret1) TYPE bapiret1."保存后触发的事件
      CLASS-METHODS: cal_mara_vfdat IMPORTING matnr TYPE mara-matnr start_date TYPE dats RETURNING VALUE(vfdat) TYPE dats.
  
      "声明方法
      METHODS:
        set_bill_status IMPORTING imp_bill_status TYPE i imp_refresh_alv TYPE char1 DEFAULT abap_false,"设置单据状态及刷新ALV标识
        display,"显示单据
        register_alv_handle_event, "注册alv事件处理程序方法
        "101移动类型不能选择样品仓、Y01只能选择样品仓
        check_lgort_by_bwart IMPORTING iv_bwart TYPE bwart iv_lgort TYPE lgort_d RETURNING VALUE(rv_msg) TYPE bapi_msg,
        before_save RETURNING VALUE(check_result) TYPE  bapiret1,"保存单据前的数据准备及检查,返回检查结果
        set_screen_field_editable IMPORTING imp_bill_status TYPE i,"根据单据状态设置屏幕字段
        set_alv_fieldcatelog IMPORTING   imp_bill_status TYPE i,"根据单据状态设置ALV字段目录属性
        enqueue_zsapnote_no_lock IMPORTING billno TYPE zsapnote_no,"入库申请单加锁
        dequeue_zsapnote_no_lock IMPORTING billno TYPE  zsapnote_no,"释放入库申请单锁
        dequeue_all_lock, "释放所有锁
        "声明ALV事件处理程序方法
        handle_alv_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive,"增加/减少toolbar按钮
        handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,"自定义按钮事件
        handle_alv_onf4 FOR EVENT onf4 OF cl_gui_alv_grid IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display,"处理ALV ONF4搜索帮助事件
        "ALV单元格编辑后的处理事件
        handle_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
          IMPORTING sender e_modified et_good_cells,
        handle_bill_status_changed FOR EVENT bill_status_changed OF zcl_pro_grreq_bill IMPORTING e_bill_status  e_refresh_alv,"单据状态改变时的事件处理程序方法
        set_drdn_table."设置下拉框列表值
  ENDCLASS.
  
  CLASS zcl_pro_grreq_bill IMPLEMENTATION.
    "设置下拉框列表值
    METHOD set_drdn_table.
      DATA: lt_dral TYPE lvc_t_dral,                          "#EC NEEDED
            ls_dral TYPE lvc_s_dral.                          "#EC NEEDED
      SELECT * FROM dd07v WHERE domname  = 'ZWEMPF' AND ddlanguage = @sy-langu INTO TABLE @DATA(lt_dd07v).
      LOOP AT lt_dd07v INTO DATA(ls_dd07v).
        ls_dral-handle = '1'.
        ls_dral-int_value = ls_dd07v-domvalue_l.
        ls_dral-value = ls_dd07v-ddtext.
        APPEND ls_dral TO lt_dral.
      ENDLOOP.
      go_alv_grid_create->set_drop_down_table( it_drop_down_alias = lt_dral ).
  *    go_alv_grid_create->set_drop_down_table( it_drop_down = lt_dropdown ).
    ENDMETHOD.
    METHOD dequeue_zsapnote_no_lock.
      CALL FUNCTION 'DEQUEUE_EZ_PP_ZPPT0003'
        EXPORTING
  *       MODE_ZPPT0003       = 'E'
  *       MANDT       = SY-MANDT
          zsapnote_no = billno
  *       X_ZSAPNOTE_NO       = ' '
  *       _SCOPE      = '3'
  *       _SYNCHRON   = ' '
  *       _COLLECT    = ' '
        .
  
    ENDMETHOD.
    "释放所有锁
    METHOD dequeue_all_lock.
      CALL FUNCTION 'DEQUEUE_ALL'
  *     EXPORTING
  *       _SYNCHRON       = ' '
        .
  
    ENDMETHOD.
  
    "显示入库申请单界面
    METHOD display.
      CALL SCREEN '9100'.
    ENDMETHOD.
    "计算物料的货架寿命起始日期的过期日
    METHOD cal_mara_vfdat.
      SELECT SINGLE iprkz, rdmhd, mhdhb FROM mara  WHERE matnr = @matnr INTO @DATA(ls_mara).
      IF sy-subrc = 0.
        CALL FUNCTION 'VB_MAINTAIN_MHD'
          EXPORTING
            i_iprkz_hbd = ls_mara-iprkz "货架寿命到期日的期间标识
            i_rdmhd     = ls_mara-rdmhd "货架寿命到期日期计算舍入规则
            i_geshb     = ls_mara-mhdhb  "总货架寿命
            i_hsdat     = start_date "起算日期
            check       = ' '
            calc        = 'X'
          IMPORTING
            o_mhdat     = vfdat "有效日期
          EXCEPTIONS
            OTHERS      = 99.
      ENDIF.
    ENDMETHOD.
    "101移动类型不能选择样品仓、Y01只能选择样品仓
    METHOD check_lgort_by_bwart.
      DATA:lv_lgobe TYPE lgobe.
      CLEAR rv_msg.
      CHECK iv_bwart IS NOT INITIAL AND iv_lgort IS NOT INITIAL.
      SELECT SINGLE lgobe INTO lv_lgobe FROM t001l WHERE werks = p_dwerk AND lgort = iv_lgort.
      IF sy-subrc <> 0.
        rv_msg = |{ iv_lgort }{ '仓库不存在' }|.
        RETURN.
      ENDIF.
      IF iv_bwart = '101'.
        IF  lv_lgobe CA '样'.
          rv_msg = '101移动类型不能输入样品仓'.
        ENDIF.
      ELSEIF iv_bwart = 'Y01'.
        IF lv_lgobe CA '样'.
          rv_msg = ''.
        ELSE.
          rv_msg = 'Y01移动类型只能输入样品仓'.
        ENDIF.
      ENDIF.
    ENDMETHOD.
  
    "ALV按钮事件处理程序方法：删除或增加按钮
    METHOD handle_alv_toolbar.
      "删除不需要的按钮
      DELETE e_object->mt_toolbar
       WHERE function = '&&SEP00'
         OR function = '&REFRESH'
         OR function = '&LOCAL&APPEND'
         OR function = '&LOCAL&UNDO'
         OR function ='&LOCAL&INSERT_ROW'
        OR function ='&LOCAL&DELETE_ROW'
         OR function = '&LOCAL&COPY_ROW'
         OR function = '&&SEP01'
         OR function = '&LOCAL&CUT'
         OR function = '&LOCAL&COPY'
         OR function = '&LOCAL&PASTE'
        OR function = '&LOCAL&UNDO'
         OR function = '&&SEP02'.
  
      "增加一个新按钮
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_button>).
      <fs_button>-butn_type = '3'.  "分割符
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-function = 'DELETE_ALL'.
      <fs_button>-icon = '@18@'.
      <fs_button>-text = '删除全部行'.
    ENDMETHOD.
    "实现自定义按钮事件处理方法
    METHOD handle_user_command.
      CASE e_ucomm.
        WHEN 'DELETE_ALL'.
          IF go_zcl_pro_grreq_bill->bill_status <> zcl_pro_grreq_bill=>cns_status_changed.
            RETURN.
          ELSE.
            LOOP AT gt_zppt0003 ASSIGNING FIELD-SYMBOL(<fs>).
              <fs>-zdelflg = abap_true.
            ENDLOOP.
            go_alv_grid_create->refresh_table_display( is_stable = gs_stable ).
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDMETHOD.
  
    "onf4搜索帮助实现
    METHOD handle_alv_onf4.
      DATA lt_ddshretval TYPE TABLE OF ddshretval.
      DATA lv_matnr TYPE matnr.
      DATA(lv_rowid) = es_row_no-row_id. "ALV正在操作的行号
      CASE e_fieldname.
        WHEN 'LGORT'  ."仓库
          READ TABLE gt_zppt0003 INDEX lv_rowid ASSIGNING <fs_zppt0003>.
          lv_matnr = <fs_zppt0003>-matnr.
          SELECT mard~lgort,t001l~lgobe FROM mard
            LEFT JOIN t001l ON mard~werks = t001l~werks AND mard~lgort = t001l~lgort
            WHERE mard~werks = @p_dwerk
            AND mard~matnr = @lv_matnr
            INTO TABLE @DATA(lt_t001l).
  
          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
            EXPORTING
              retfield        = 'LGORT' "返回字段
              value_org       = 'S'
            TABLES
              value_tab       = lt_t001l
              return_tab      = lt_ddshretval
            EXCEPTIONS
              parameter_error = 1
              no_values_found = 2
              OTHERS          = 3.
          IF sy-subrc = 0 AND lt_ddshretval IS NOT INITIAL.
            READ TABLE lt_t001l WITH KEY lgort = lt_ddshretval[ 1 ]-fieldval INTO gs_lgort.
            IF sy-subrc = 0.
              <fs_zppt0003>-lgort = gs_lgort-lgort.
              <fs_zppt0003>-lgobe = gs_lgort-lgobe.
            ENDIF.
          ENDIF.
          "移动类型101不能输入样品仓库，Y01只能输入样品仓库.
          IF lt_ddshretval[] IS NOT INITIAL .
            CLEAR gv_msg.
            gv_msg =  check_lgort_by_bwart( EXPORTING iv_bwart = zspp034_head-bwart iv_lgort = CONV lgort_d( lt_ddshretval[ 1 ]-fieldval ) ).
            IF gv_msg IS NOT INITIAL.
              CLEAR:<fs_zppt0003>-lgort, <fs_zppt0003>-lgobe.
              MESSAGE gv_msg TYPE 'S'.
            ENDIF.
          ENDIF.
        WHEN 'ZWSLGORT' ."车间仓库
  
          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
            EXPORTING
              retfield        = 'LGORT' "返回字段
              value_org       = 'S'
            TABLES
              value_tab       = gt_t001l
              return_tab      = lt_ddshretval
            EXCEPTIONS
              parameter_error = 1
              no_values_found = 2
              OTHERS          = 3.
          IF sy-subrc = 0 AND lt_ddshretval IS NOT INITIAL.
            READ TABLE gt_t001l WITH KEY lgort = lt_ddshretval[ 1 ]-fieldval INTO gs_lgort.
            IF sy-subrc = 0.
              gt_zppt0003[ lv_rowid ]-zwslgort = gs_lgort-lgort.
              gt_zppt0003[ lv_rowid ]-zwslgobe = gs_lgort-lgobe.
            ENDIF.
          ENDIF.
        WHEN 'CHARG'."批次
          READ TABLE gt_charg WITH KEY aufnr = gt_zppt0003[ lv_rowid ]-aufnr INTO gs_charg.
          IF sy-subrc = 0 AND gs_charg-charg IS NOT INITIAL.
            gt_zppt0003[ lv_rowid  ]-charg = gs_charg-charg.
            MESSAGE '流程订单已存在批号了,您修改的批号无效!' TYPE 'I'.
          ELSE."获取当前行物料的批次号
            lv_matnr = gt_zppt0003[ lv_rowid  ]-matnr.
            SELECT matnr,werks,charg FROM mcha WHERE matnr = @lv_matnr AND werks = @p_dwerk INTO TABLE @DATA(lt_mcha).
            IF lt_mcha IS NOT INITIAL.
              CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
                EXPORTING
                  retfield        = 'CHARG' "返回字段
                  value_org       = 'S'
                TABLES
                  value_tab       = lt_mcha
                  return_tab      = lt_ddshretval
                EXCEPTIONS
                  parameter_error = 1
                  no_values_found = 2
                  OTHERS          = 3.
              IF sy-subrc = 0 AND lt_ddshretval IS NOT INITIAL.
                gt_zppt0003[ lv_rowid  ]-charg = lt_ddshretval[ 1 ]-fieldval.
              ENDIF.
  
            ENDIF.
          ENDIF.
  
        WHEN OTHERS.
      ENDCASE.
      er_event_data->m_event_handled = 'X'."终止后续的搜索帮助
      go_alv_grid_create->refresh_table_display( is_stable = VALUE lvc_s_stbl( row = 'X' col = 'X') i_soft_refresh = abap_true  ).
    ENDMETHOD.
    "ALV单元格编辑后的处理程序
    METHOD handle_data_changed_finished.
      DATA: lv_refresh TYPE char1,
            lv_message TYPE char40,
            lv_aufnr   TYPE afpo-aufnr,
            lv_lgort   TYPE mard-lgort,
            l_key      TYPE char90,
            ls_value   TYPE bapi1003_alloc_values_char,
            lt_value   TYPE TABLE OF bapi1003_alloc_values_char.
  
      LOOP AT et_good_cells INTO DATA(ls_good_cells) WHERE   error = ''.
        ASSIGN gt_zppt0003[ ls_good_cells-row_id ] TO <fs_zppt0003>.
        CHECK sy-subrc = 0.
        CASE ls_good_cells-fieldname.
          WHEN 'LGORT'."仓库
            CLEAR lv_lgort.
  *          READ TABLE gt_t001l  WITH  KEY lgort = ls_good_cells-value INTO gs_lgort.
  *          IF sy-subrc = 0.
  *            <fs_zppt0003>-lgobe = gs_lgort-lgobe.
  *          ELSE.
  *            CLEAR: <fs_zppt0003>-lgort,<fs_zppt0003>-lgobe.
  *            MESSAGE '输入的仓库无效,请检查!' TYPE 'I'.
  *          ENDIF.
            "检查MMSC是否扩充物料工厂仓库地点
            SELECT SINGLE lgort INTO lv_lgort FROM mard
              WHERE matnr = <fs_zppt0003>-matnr
              AND werks = <fs_zppt0003>-werks
              AND lgort = ls_good_cells-value.
            IF sy-subrc = 0.
              READ TABLE gt_t001l  WITH  KEY lgort = ls_good_cells-value INTO gs_lgort.
              IF sy-subrc = 0.
                <fs_zppt0003>-lgobe = gs_lgort-lgobe."仓库名称
              ENDIF.
              "移动类型101不能输入样品仓库，Y01只能输入样品仓库.
              CLEAR gv_msg.
              gv_msg =  check_lgort_by_bwart( EXPORTING iv_bwart = zspp034_head-bwart iv_lgort = CONV lgort_d( ls_good_cells-value ) ).
              IF gv_msg IS NOT INITIAL.
                MESSAGE gv_msg TYPE 'I'.
                CLEAR:<fs_zppt0003>-lgort, <fs_zppt0003>-lgobe.
              ENDIF.
            ELSE.
              MESSAGE '物料未扩充此仓库地点,请检查!' TYPE 'I'.
              CLEAR: <fs_zppt0003>-lgort,<fs_zppt0003>-lgobe.
            ENDIF.
          WHEN 'ZWSLGORT'."车间仓库
            READ TABLE gt_t001l  WITH  KEY lgort = ls_good_cells-value INTO gs_lgort.
            IF sy-subrc = 0.
              <fs_zppt0003>-zwslgobe = gs_lgort-lgobe.
            ELSE.
              CLEAR: <fs_zppt0003>-zwslgort,<fs_zppt0003>-zwslgobe.
              MESSAGE '输入的车间仓库无效,请检查!' TYPE 'I'.
            ENDIF.
          WHEN 'ZJSMNG' OR 'ZWSMNG'. "申请件数、申请尾数
  *  输入申请件数或申请尾数后，计算申请数量，公式为：申请数量 = 申请件数 * 基本单位/PC + 申请尾数
            <fs_zppt0003>-menge = <fs_zppt0003>-zumren * <fs_zppt0003>-zjsmng + <fs_zppt0003>-zwsmng.
          WHEN 'CHARG'. "批次号
            lv_aufnr = <fs_zppt0003>-aufnr.
            READ TABLE gt_charg WITH KEY aufnr = lv_aufnr  xchpf = 'X' INTO gs_charg.
            IF sy-subrc = 0 .
              IF gs_charg-charg IS NOT INITIAL.
                <fs_zppt0003>-charg = gs_charg-charg.
                lv_message = '流程订单已存在批号了,您修改的批号无效!'.
              ENDIF.
            ELSE.
              CLEAR  <fs_zppt0003>-charg.
              lv_message = '物料未启用批次管理(工厂),不需要输入批号!'.
            ENDIF.
  
            IF lv_message IS NOT INITIAL.
              MESSAGE lv_message TYPE 'I'.
            ELSE.
              READ TABLE gt_pro_output WITH KEY aufnr = lv_aufnr  INTO gs_pro_output.
              IF sy-subrc = 0.
                TRY .
                    l_key(40) = <fs_zppt0003>-matnr.
                    l_key+40(4) = zspp034_head-werks.
                    l_key+44(10) = ls_good_cells-value.
                    IF gs_pro_output-mtart = 'Z050'.
                      lt_value = zcl_bc_public=>get_class_values( key = l_key classnum = 'BATCH_CP' classtype = '022' objtable = 'MCHA' ).
                      READ TABLE lt_value WITH KEY charact = 'Z_BATCH_SCRQ' INTO ls_value.
                      IF sy-subrc = 0.
                        <fs_zppt0003>-zhsdat = CONV #( ls_value-value_char ).
                      ENDIF.
  
                      READ TABLE lt_value WITH KEY charact = 'Z_BATCH_YXQZ' INTO ls_value.
                      IF sy-subrc = 0 AND ls_value-value_char IS  NOT INITIAL.
                        <fs_zppt0003>-zvfdat = CONV #( ls_value-value_char ).
                      ENDIF.
  
                      READ TABLE lt_value WITH KEY charact = 'Z_BATCH_DYPC' INTO ls_value.
                      IF sy-subrc = 0 AND ls_value-value_char IS  NOT INITIAL.
                        <fs_zppt0003>-zpack_charg = CONV #( ls_value-value_char ).
                      ENDIF.
                    ELSE.
                      lt_value = zcl_bc_public=>get_class_values( key = l_key classnum = 'ZBATCH_SIRIO' classtype = '022' objtable = 'MCHA' ).
                      READ TABLE lt_value WITH KEY charact = 'ZHSDAT' INTO ls_value.
                      IF sy-subrc = 0.
                        <fs_zppt0003>-zhsdat = CONV #( ls_value-value_char ).
                      ENDIF.
  
                      READ TABLE lt_value WITH KEY charact = 'ZVFDAT' INTO ls_value.
                      IF sy-subrc = 0 .
                        <fs_zppt0003>-zvfdat = CONV #( ls_value-value_char ).
                      ENDIF.
  
                      READ TABLE lt_value WITH KEY charact = 'ZPACK_CHARG' INTO ls_value.
                      IF sy-subrc = 0 .
                        <fs_zppt0003>-zpack_charg = CONV #( ls_value-value_char )."包装批号（打印）
                      ENDIF.
                    ENDIF.
                  CATCH cx_root .
                ENDTRY.
              ENDIF.
            ENDIF.
          WHEN 'ZHSDAT'. "生产日期
            zppt0003 = <fs_zppt0003>.
            READ TABLE gt_charg WITH KEY aufnr = zppt0003-aufnr xchpf = 'X' INTO gs_charg.
            IF sy-subrc <> 0.
              CLEAR <fs_zppt0003>-zhsdat.
              MESSAGE '物料未启用批次管理(工厂),不需要输入生产日期!' TYPE 'I'.
            ELSE.
              "计算无效剩余货架有效期
              <fs_zppt0003>-zvfdat = zcl_pro_grreq_bill=>cal_mara_vfdat( matnr = zppt0003-matnr start_date =  CONV vfdat( ls_good_cells-value ) ).
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
      go_alv_grid_create->set_frontend_layout( gs_layout_bill ).
      CALL METHOD go_alv_grid_create->refresh_table_display
        EXPORTING
          is_stable = gs_stable
        EXCEPTIONS
          finished  = 1
          OTHERS    = 2.
    ENDMETHOD.
  
    "注册alv事件处理程序方法
    METHOD register_alv_handle_event.
  
      SET HANDLER: go_zcl_pro_grreq_bill->handle_alv_toolbar FOR go_alv_grid_create,
                   go_zcl_pro_grreq_bill->handle_user_command FOR go_alv_grid_create,
                    go_zcl_pro_grreq_bill->handle_alv_onf4 FOR go_alv_grid_create,
                    go_zcl_pro_grreq_bill->handle_data_changed_finished FOR go_alv_grid_create,
                    go_zcl_pro_grreq_bill->handle_bill_status_changed FOR go_zcl_pro_grreq_bill.
  
      "注册编辑事件，使ALV编辑后回车，或离开焦点时可以进行数据校验
      go_alv_grid_create->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
  
      "注册ALV字段onf4搜索事件
      DATA(lt_f4) = VALUE lvc_t_f4(  ( fieldname = 'LGORT' register = 'X' getbefore = 'X' chngeafter = 'X' internal = 'X' )
                                     ( fieldname = 'ZWSLGORT' register = 'X' getbefore = 'X' chngeafter = 'X' internal = 'X' )
                                     ( fieldname = 'CHARG' register = 'X' getbefore = 'X' chngeafter = 'X' internal = 'X' )
                                   ) .
  
      go_alv_grid_create->register_f4_for_fields( it_f4 = lt_f4 ).
    ENDMETHOD.
  
    "保存单据前的数据准备及检查,返回检查结果
    METHOD before_save.
      DATA:lv_menge TYPE co_psmng,
           lv_psmng TYPE co_psmng.
      "检查输入的车间仓库编号
      READ TABLE gt_t001l  WITH  KEY lgort = zspp034_head-zwslgort INTO gs_lgort.
      IF sy-subrc <> 0.
        check_result-type = 'E'.
        check_result-message = '输入的车间仓库编号无效'.
        RETURN.
      ENDIF.
      IF zspp034_head-bwart IS INITIAL.
        check_result-type = 'E'.
        check_result-message = '请输入移动类型'.
        RETURN.
      ENDIF.
      "移动类型101、Y01下发WMS时，需检查物料的标准价
      IF zspp034_head-bwart = '101' OR zspp034_head-bwart = 'Y01'.
        MOVE-CORRESPONDING gt_zppt0003 TO gt_matnr.
        SORT gt_matnr BY matnr.
        DELETE ADJACENT DUPLICATES FROM gt_matnr COMPARING matnr.
  
        SELECT SINGLE bwkey FROM t001w WHERE werks = @zspp034_head-werks INTO @DATA(lv_bwkey).
        SELECT matnr,stprs FROM mbew FOR ALL ENTRIES IN @gt_matnr
         WHERE matnr = @gt_matnr-matnr
           AND bwkey = @lv_bwkey
           AND vprsv = 'S'
  *         AND lfgja = @sy-datum+0(4)
  *         AND lfmon = @sy-datum+4(2)
          INTO TABLE @DATA(lt_mbew).
  
  *      SELECT matnr , stprs FROM mbewh FOR ALL ENTRIES IN @gt_matnr
  *       WHERE matnr = @gt_matnr-matnr
  *        AND bwkey = @lv_bwkey
  *        AND vprsv = 'S'
  *        AND lfgja = @sy-datum+0(4)
  *        AND lfmon = @sy-datum+4(2)
  *        APPENDING TABLE @lt_mbew.
  
        SORT lt_mbew BY matnr.
      ENDIF.
  
      "获取单号
      IF zspp034_head-zsapnote_no IS  INITIAL.
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '15'
            object                  = 'ZMM030'
          IMPORTING
            number                  = zspp034_head-zsapnote_no
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.
        IF sy-subrc <> 0 OR zspp034_head-zsapnote_no IS INITIAL.
          check_result-type = 'E'.
          check_result-message = '生成入库申请单编号失败'.
          RETURN.
        ENDIF.
      ENDIF.
  
      LOOP AT gt_zppt0003 ASSIGNING FIELD-SYMBOL(<fs>).
        DATA(lv_tabix) = sy-tabix.
        "移动类型101、Y01下发WMS时，需检查物料的标准价
        IF zspp034_head-bwart = '101' OR zspp034_head-bwart = 'Y01'.
          READ TABLE lt_mbew WITH KEY matnr = <fs>-matnr BINARY SEARCH INTO DATA(ls_mbew).
          IF sy-subrc <> 0 OR ( sy-subrc = 0 AND ls_mbew-stprs <= 0 ) .
            check_result-type = 'E'.
            check_result-message = |{ '第' }{ lv_tabix }{ '行的物料' } { zcl_bc_public=>conv_by_ddic( i_input = <fs>-matnr i_out = 'X' ) }{ '需要下发给WMS,但是未维护标准价格,请联系财务人员处理' }|.
            RETURN.
          ENDIF.
        ENDIF.
  
        IF <fs>-lgort IS INITIAL.
          check_result-type = 'E'.
          check_result-message = |{ '第' }{ lv_tabix }{ '行的仓库不能为空,请检查!' }|.
          RETURN.
        ENDIF.
  
        IF <fs>-menge IS INITIAL.
          check_result-type = 'E'.
          check_result-message = |{ '第' }{ lv_tabix }{ '行的申请数量不能为空,请检查!' }|.
          RETURN.
        ENDIF.
        "( MENGE申请数量 + WEMNG已收货数量 )  不等大于 PSMNG计划数量 * ( 1 + UEBTO过量交货限度 / 100 )
        lv_menge = <fs>-menge + <fs>-wemng.
        lv_psmng = <fs>-psmng * ( 1 + <fs>-uebto / 100 ).
        IF lv_menge > lv_psmng.
          SELECT SINGLE uebtk INTO @DATA(lv_uebtk) FROM afpo WHERE aufnr = @<fs>-aufnr.
          IF lv_uebtk = 'X'.
            MESSAGE |{ '第' }{ lv_tabix }{ '行超容差收货,' }{ '在无约束的超量发货情况下， 容差被忽略' }| TYPE 'W'.
          ELSE.
            check_result-type = 'E'.
            check_result-message = |{ '第' }{ lv_tabix }{ '行的已收货+申请数量' }{ lv_menge }{ '不能超过订单最大入库量' }{ lv_psmng }|.
            RETURN.
          ENDIF.
        ENDIF.
        <fs>-zsapnote_no = zspp034_head-zsapnote_no. "编号
        <fs>-bwart = zspp034_head-bwart."移动类型
        <fs>-zbktxt = zspp034_head-zbktxt."抬头备注
        <fs>-erdat = sy-datum.
        <fs>-erzet  = sy-uzeit.
        <fs>-uname = sy-uname.
        <fs>-zwslgort = gs_lgort-lgort.
        <fs>-zwslgobe = gs_lgort-lgobe.
  
        READ TABLE gt_charg WITH KEY aufnr = <fs>-aufnr INTO gs_charg.
        IF sy-subrc = 0.
          CASE gs_charg-xchpf. "批次管理(工厂)
            WHEN abap_false.
              IF <fs>-charg IS NOT  INITIAL OR <fs>-zhsdat IS NOT INITIAL  OR <fs>-zvfdat IS NOT INITIAL.
                check_result-type = 'E'.
                check_result-message = |{ '第' }{ lv_tabix }{ '行的物料未启用批次管理(工厂),不能输入批号、生产日期、有效期,请检查!' }|.
                RETURN.
              ENDIF.
            WHEN abap_true.
              IF <fs>-charg IS INITIAL OR <fs>-zhsdat IS  INITIAL OR <fs>-zvfdat IS  INITIAL.
                check_result-type = 'E'.
                check_result-message = |{ '第' }{ lv_tabix }{ '行的物料已启用批次管理(工厂),批号、生产日期及有效期不能为空,请检查!' }|.
                RETURN.
              ENDIF.
            WHEN OTHERS.
          ENDCASE.
  
        ENDIF.
  
        "修改单据时，记录修改人
        IF go_zcl_pro_grreq_bill->bill_status = zcl_pro_grreq_bill=>cns_status_changed.
          <fs>-aenam = sy-uname.
          <fs>-aedat  = sy-datum.
          <fs>-aezeit = sy-uzeit.
        ENDIF.
  
      ENDLOOP.
  
  
      check_result-type = 'S'.
      check_result-message = '检查成功'.
    ENDMETHOD.
  
    "单据状态改变时的事件处理程序方法
    METHOD handle_bill_status_changed.
      "退出单据界面时，提示用户是否保存
      IF e_bill_status = zcl_pro_grreq_bill=>cns_status_created OR e_bill_status = zcl_pro_grreq_bill=>cns_status_changed.
        me->prompt_when_exit = abap_true. "提示
      ELSE.
        me->prompt_when_exit = abap_false. "不提示
      ENDIF.
  
  
      "切换ALV编辑模式
  *      IF me->bill_status = zcl_pro_grreq_bill=>cns_status_created OR me->bill_status =  zcl_pro_grreq_bill=>cns_status_changed.
  *        IF go_alv_grid_create->is_ready_for_input( ) = 0."显示状态
  *          go_alv_grid_create->set_ready_for_input( 1 )."切换显示状态
  *        ENDIF.
  *      ELSEIF me->bill_status = zcl_pro_grreq_bill=>cns_status_display.
  *
  *        IF go_alv_grid_create->is_ready_for_input( ) = 1."显示状态
  *          go_alv_grid_create->set_ready_for_input( 0 )."切换编辑状态
  *        ENDIF.
  *      ENDIF.
      IF go_zcl_pro_grreq_bill IS  BOUND AND  go_alv_grid_create IS BOUND.
        go_zcl_pro_grreq_bill->set_alv_fieldcatelog( go_zcl_pro_grreq_bill->bill_status )."设置ALV字段目录属性
        go_alv_grid_create->set_frontend_fieldcatalog( gt_alv_fidcat ). "设置ALV界面字段
      ENDIF.
  
      "刷新ALV界面
      IF go_alv_grid_create IS  BOUND AND e_refresh_alv = abap_true .
        CALL METHOD go_alv_grid_create->refresh_table_display
          EXPORTING
            is_stable = gs_stable
  *         i_soft_refresh =
          EXCEPTIONS
            finished  = 1
            OTHERS    = 2.
      ENDIF.
    ENDMETHOD.
    "设置单据状态
    METHOD set_bill_status.
      me->bill_status = imp_bill_status.
      RAISE EVENT bill_status_changed EXPORTING e_bill_status = imp_bill_status ."单据状态改变时事件
    ENDMETHOD.
    "保存单据
    METHOD save_bill.
      "准备下发WMS数据
      DATA: ls_output        TYPE zmt_sap2wms_other, "下发参数
            ls_input         TYPE zmt_sap2wsm_other_ret, "响应参数
            ls_item          TYPE zdt_sap2wms_other_list_of_oth1, "行项目
            lv_xclosed_h     TYPE char1, "整单删除，只下发表头数据给WMS标识
            lv_send_wms      TYPE char1, "下发WMS标识
            lv_message       TYPE bapi_msg,
            lv_qmatv         TYPE marc-qmatv,
            lv_zsapnote_no   TYPE zppt0003-zsapnote_no,
            lo_send_lims     TYPE REF TO zcl_lims_oper,
            ls_lims_response TYPE zsign_batch_sessionless_soap_o.
  
      "只下发移动类型(101,Y01)的数据
      zppt0003 = gt_zppt0003[ 1 ].
      lv_zsapnote_no = zppt0003-zsapnote_no.
      IF zppt0003-bwart = '101' OR zppt0003-bwart = 'Y01' .
        lv_send_wms = abap_true.
      ENDIF.
      IF lv_send_wms = abap_true.
        "抬头数据
        READ TABLE gt_zppt0003 WITH KEY zdelflg = '' INTO DATA(ls_zppt0003)."整单删除的判断逻辑
        IF sy-subrc <> 0.
          lv_xclosed_h = abap_true."整单都被删除了，不需要下发行项目
        ENDIF.
        ls_output-mt_sap2wms_other-wms_custid = zppt0003-werks."WMS货主ID
        ls_output-mt_sap2wms_other-sapnote_no = zppt0003-zsapnote_no."入库申请单号
        ls_output-mt_sap2wms_other-bus_type = 'A05'."业务类型
        ls_output-mt_sap2wms_other-werks = zppt0003-werks."工厂
        ls_output-mt_sap2wms_other-bwart = zppt0003-bwart."移动类型
        READ TABLE gt_vrm_values WITH KEY key = zppt0003-bwart INTO DATA(ls_wrm_values).
        IF sy-subrc = 0.
          ls_output-mt_sap2wms_other-bwtxt = ls_wrm_values-text."移动类型文本
        ENDIF.
        ls_output-mt_sap2wms_other-usnam = zppt0003-uname."制单员
        ls_output-mt_sap2wms_other-note = zppt0003-zbktxt."抬头文本
        ls_output-mt_sap2wms_other-xclosed_h = lv_xclosed_h."删除标志
        ls_output-mt_sap2wms_other-shkzg = 'S'."出入库标记
  
        "行项目数据
        IF lv_xclosed_h <> abap_true.
          CLEAR gt_matnr.
          MOVE-CORRESPONDING gt_zppt0003 TO gt_matnr.
          SORT gt_matnr.
          DELETE ADJACENT DUPLICATES FROM gt_matnr.
          SELECT matnr,mtart FROM mara FOR ALL ENTRIES IN @gt_matnr
            WHERE matnr = @gt_matnr-matnr INTO TABLE @DATA(lt_mara).
          LOOP AT gt_zppt0003 INTO zppt0003 WHERE zdelflg = abap_false.
            CLEAR ls_item.
            ls_item-sapnote_line = zppt0003-zsapnote_line."行号
            ls_item-matnr = zcl_bc_public=>conv_by_ddic( i_input = zppt0003-matnr i_out = 'X' )."物料编号
            ls_item-maktx = zppt0003-maktx."物料描述
            ls_item-charg = zppt0003-charg. "批号
            ls_item-wms_ext_st = '111'. "固定标识
            ls_item-lines_of_character = VALUE #( ( atnam = 'ZHSDAT' atwrt = zppt0003-zhsdat )
                                                  ( atnam = 'ZLICHA' atwrt = zppt0003-zpack_charg )
                                                  ( atnam = 'ZVFDAT' atwrt = zppt0003-zvfdat )
                                                 )."批次特征值
            ls_item-menge = zppt0003-menge. "申请数量
            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
              EXPORTING
                input  = zppt0003-meins
              IMPORTING
                output = gv_meins.
            ls_item-meins = gv_meins. "单位
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = zppt0003-kdauf
              IMPORTING
                output = zppt0003-kdauf.
            ls_item-kdauf = zppt0003-kdauf.  "销售订单号
            IF zppt0003-kdpos IS NOT INITIAL.
              ls_item-kdpos = zppt0003-kdpos.  "销售订单行项目
            ENDIF.
            READ TABLE lt_mara WITH KEY matnr = zppt0003-matnr INTO DATA(ls_mara).
            IF  sy-subrc  = 0 AND ( ls_mara-mtart = 'Z010' OR ls_mara-mtart = 'Z050' ).
              IF ls_output-mt_sap2wms_other-bwart = '101'.
                "2021-01-05 新增逻辑：物料质量视图中的检验设置字段(QMATV)为空字符时，不能传输质检库存状态
                CLEAR lv_qmatv.
                SELECT SINGLE qmatv INTO lv_qmatv FROM marc WHERE werks = zppt0003-werks AND matnr = ls_mara-matnr.
                IF sy-subrc = 0.
                  IF lv_qmatv = ''.
                    ls_item-stock_st = ''.  "非限制库存状态
                  ELSE.
                    ls_item-stock_st = 'I'.  "质检库存状态
                  ENDIF.
                ENDIF.
              ELSEIF ls_output-mt_sap2wms_other-bwart = 'Y01' AND ls_mara-mtart = 'Z050'.
                ls_item-stock_st = ''.  "非限制库存状态
              ENDIF.
            ENDIF.
            ls_item-shkzg = 'S'.  "入库/出库标记：S-入库，H-出库
            ls_item-lgort = zppt0003-zwslgort.  "车间仓库
            ls_item-dwerks = zppt0003-werks.  "对方工厂
            ls_item-dlgort = zppt0003-lgort.  "对方仓库
            ls_item-aufnr = zppt0003-aufnr.  "生产订单
            ls_item-sgtxt = zppt0003-wempf.  "备注1
            ls_item-zrsv01 = zppt0003-ablad.  "备注2
            ls_item-zrsv02 = zppt0003-zjsmng.  "申请件数
            ls_item-zrsv03 = zppt0003-zwsmng.  "申请尾数
            APPEND ls_item TO ls_output-mt_sap2wms_other-list_of_others.
          ENDLOOP.
        ENDIF.
      ENDIF.
      "接口开关检查
      CALL FUNCTION 'Z_PI_CK'
        EXPORTING
          intf_id  = 'MM026'
        IMPORTING
          err_info = lv_message.
  *  接口未打开
      IF lv_message  IS NOT INITIAL.
        ret_bapiret1-type = 'E'.
        ret_bapiret1-message = 'MM026下发WMS接口未打开'.
        RETURN.
      ENDIF.
      TRY .
          "下发给WMS
          DATA(lv_sap_wms_api) = NEW zco_si_sap2wms_other_out( )."下发接口代理类对象
          lv_sap_wms_api->si_sap2wms_other_out( EXPORTING output  = ls_output IMPORTING input = ls_input ).
          IF ls_input-mt_sap2wsm_other_ret-returnflag = '1'."1-成功，0-全部失败，2-部分成功
            MODIFY zppt0003 FROM TABLE gt_zppt0003."插入或更新单据数据
            go_zcl_pro_grreq_bill->dequeue_all_lock( ). "释放所有锁
            ret_bapiret1-type = 'S'.
            ret_bapiret1-message = | { '单号：' }{ lv_zsapnote_no }{ '下发WMS成功' }|.
  
            "20220414 修改逻辑，WMS保存成功的时候才调用LIMS的接口，如果失败的话不下发LIMS
            "下发批结信息给LIMS
            "20220414 修复ret_bapiret1-message 拼接上消息，避免wms的返回消息被覆盖
            IF zppt0003-bwart = '101' .
              lo_send_lims = NEW zcl_lims_oper( ).
              ls_lims_response = lo_send_lims->send_to_lims( ).
              IF ls_lims_response-sign_batch_sessionless_result-ret_code = 'S'.
                ret_bapiret1-type = 'S'.
                ret_bapiret1-message = ret_bapiret1-message && '批结信息下发LIMS成功'.
              ELSEIF ls_lims_response-sign_batch_sessionless_result-ret_msg = '批结信息为空'.
                ret_bapiret1-type = 'S'.
                ret_bapiret1-message =  ret_bapiret1-message &&  '批结信息为空,不需要下发LIMS'.
              ELSE.
                ret_bapiret1-type = 'E'.
                ret_bapiret1-message = ret_bapiret1-message && |{ '批结信息下发LIMS失败,异常信息为：' }{ ls_lims_response-sign_batch_sessionless_result-ret_msg }|.
              ENDIF.
            ENDIF.
            "20220414 修改逻辑，WMS保存成功的时候才调用LIMS的接口，如果失败的话不下发LIMS end
          ELSE.
            ret_bapiret1-type = 'E'.
            ret_bapiret1-message = ret_bapiret1-message && | { '保存失败,下发WMS失败：' }{ ls_input-mt_sap2wsm_other_ret-returndesc }|.
          ENDIF.
        CATCH cx_root INTO DATA(ls_root).
          ret_bapiret1-type = 'E'.
          ret_bapiret1-message = ret_bapiret1-message && | { '保存失败,下发WMS失败：' }{ ls_root->get_text( ) }|.
      ENDTRY.
  
      IF p_create = 'X'.
        RAISE EVENT after_save EXPORTING es_bapiret1 = ret_bapiret1."触发事件
      ENDIF.
    ENDMETHOD.
  
    "根据单据状态设置屏幕字段
    METHOD set_screen_field_editable.
      LOOP AT SCREEN.
        CASE screen-name.
          WHEN 'ZSPP034_HEAD-BWART' .
            IF imp_bill_status = zcl_pro_grreq_bill=>cns_status_created .
              screen-input = 1.
            ELSE.
              screen-input = 0.
            ENDIF.
          WHEN  'ZSPP034_HEAD-ZBKTXT' OR 'ZSPP034_HEAD-ZWSLGORT'.
            IF imp_bill_status = zcl_pro_grreq_bill=>cns_status_created OR imp_bill_status = zcl_pro_grreq_bill=>cns_status_changed.
              screen-input = 1.
            ELSE.
              screen-input = 0.
            ENDIF.
  
          WHEN OTHERS.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
    ENDMETHOD.
    "根据单据状态设置ALV字段目录属性
    METHOD set_alv_fieldcatelog.
      LOOP AT gt_alv_fidcat ASSIGNING FIELD-SYMBOL(<fs_fidcat>).
        CASE <fs_fidcat>-fieldname.
          WHEN 'ZSAPNOTE_NO' OR 'BWART' OR 'ZBKTXT'  OR 'TYPE' OR 'MESSAGE' OR 'ZWSLGORT' OR 'ZWSLGOBE'  .
            <fs_fidcat>-tech = abap_true.
          WHEN 'LGORT'  ."仓库
            <fs_fidcat>-f4availabl = abap_true.
            IF imp_bill_status = zcl_pro_grreq_bill=>cns_status_created OR imp_bill_status = zcl_pro_grreq_bill=>cns_status_changed.
              <fs_fidcat>-edit = abap_true.
            ELSE.
              <fs_fidcat>-edit = abap_false.
            ENDIF.
          WHEN 'PSMNG'.
            <fs_fidcat>-scrtext_s = '订单数量'.
            <fs_fidcat>-scrtext_m = '订单数量'.
            <fs_fidcat>-scrtext_l = '订单数量'.
          WHEN 'ZWMSMNG'.
            IF imp_bill_status = zcl_pro_grreq_bill=>cns_status_created .
              <fs_fidcat>-no_out = abap_true.
            ELSE.
              <fs_fidcat>-no_out =  abap_false.
            ENDIF.
          WHEN 'MENGE' OR 'ZJSMNG' OR 'ZWSMNG' OR 'ZHSDAT' OR 'ZVFDAT'. "申请数量、申请件数、申请尾数、生产日期、失效期
            IF imp_bill_status = zcl_pro_grreq_bill=>cns_status_created OR imp_bill_status = zcl_pro_grreq_bill=>cns_status_changed.
              <fs_fidcat>-edit = abap_true.
            ELSE.
              <fs_fidcat>-edit = abap_false.
            ENDIF.
          WHEN 'CHARG'. "批号
            <fs_fidcat>-scrtext_s = '流程订单批号'.
            <fs_fidcat>-scrtext_m = '流程订单批号'.
            <fs_fidcat>-scrtext_l = '流程订单批号'.
            <fs_fidcat>-edit = abap_true.
  *          <fs_fidcat>-ref_table = 'AFPO'.
  *          <fs_fidcat>-ref_field = 'CHARG'.
            IF imp_bill_status = zcl_pro_grreq_bill=>cns_status_created OR imp_bill_status = zcl_pro_grreq_bill=>cns_status_changed.
              <fs_fidcat>-edit = abap_true.
            ELSE.
              <fs_fidcat>-edit = abap_false.
            ENDIF.
          WHEN 'WEMPF'.
            "<fs_fidcat>-edit = abap_true.
            <fs_fidcat>-drdn_hndl = '1'.
            <fs_fidcat>-outputlen = 15.
            <fs_fidcat>-drdn_alias = abap_true.
  *Field 'checktable' is set to avoid shortdumps that are caused
  * by inconsistend data in check tables. You may comment this out
  * when the test data of the flight model is consistent in your system.
            "  <fs_fidcat>-checktable = abap_false.        "do not check foreign keys
            IF imp_bill_status = zcl_pro_grreq_bill=>cns_status_created OR imp_bill_status = zcl_pro_grreq_bill=>cns_status_changed.
              <fs_fidcat>-edit = abap_true.
            ELSE.
              <fs_fidcat>-edit = abap_false.
            ENDIF.
          WHEN 'ABLAD'.
            <fs_fidcat>-scrtext_l = '备注2'.
            <fs_fidcat>-scrtext_m = '备注2'.
            <fs_fidcat>-scrtext_s = '备注2'.
            IF imp_bill_status = zcl_pro_grreq_bill=>cns_status_created OR imp_bill_status = zcl_pro_grreq_bill=>cns_status_changed.
              <fs_fidcat>-edit = abap_true.
            ELSE.
              <fs_fidcat>-edit = abap_false.
            ENDIF.
          WHEN 'ZDELFLG'.
            <fs_fidcat>-checkbox = abap_true.
            IF  imp_bill_status = zcl_pro_grreq_bill=>cns_status_changed.
              <fs_fidcat>-edit = abap_true.
            ELSE.
              <fs_fidcat>-edit = abap_false.
            ENDIF.
  *add S4DK904849 start
          WHEN 'ZJINTGEW' OR 'ZNTGEW' OR 'ZBRGEW' OR 'ZWJNTGEW' OR 'ZWNTGEW' OR 'ZWBRGEW'. "净净重、净重、毛重、尾数箱净净重、净重、毛重
            IF imp_bill_status = zcl_pro_grreq_bill=>cns_status_created OR imp_bill_status = zcl_pro_grreq_bill=>cns_status_changed.
              <fs_fidcat>-edit = abap_true.
            ELSE.
              <fs_fidcat>-edit = abap_false.
            ENDIF.
  *add S4DK904849 end
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
  
  
  
    ENDMETHOD.
  
    "入库申请单加锁
    METHOD enqueue_zsapnote_no_lock.
      "已加锁检查
  *    DATA: lv_garg TYPE seqg3-garg,
  *          lt_enq  TYPE TABLE OF seqg3.
  *    lv_garg = |{ sy-mandt }{ billno }{ '####' }|.  "键值由锁参数关键字组成
  *    CALL FUNCTION 'ENQUEUE_READ'
  *      EXPORTING
  *        gname                 = 'ZPPT0003'
  *        garg                  = lv_garg   "键值由锁参数关键字组成
  *        guname                = ''
  *      TABLES
  *        enq                   = lt_enq
  *      EXCEPTIONS
  *        communication_failure = 1
  *        system_failure        = 2
  *        OTHERS                = 3.
  *    IF lt_enq IS NOT INITIAL.
  *      READ TABLE lt_enq WITH KEY gobj = 'EZ_PP_ZPPT0003' INTO DATA(ls_enq).
  *      IF sy-subrc = 0.
  *        MESSAGE |{ '用户' }{ ls_enq-guname }{ '从' }{ ls_enq-gtdate DATE = ISO } { ls_enq-gttime TIME = ISO }{ '起正在修改此单了!' }| TYPE 'E'.
  *      ENDIF.
  *    ENDIF.
  
      "加锁
      CALL FUNCTION 'ENQUEUE_EZ_PP_ZPPT0003'
        EXPORTING
          mode_zppt0003  = 'E'
          mandt          = sy-mandt
          zsapnote_no    = billno
  *       X_ZSAPNOTE_NO  = ' '
  *       _SCOPE         = '2'
  *       _WAIT          = ' '
  *       _COLLECT       = ' '
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        MESSAGE ID syst-msgid TYPE syst-msgty NUMBER syst-msgno
             WITH  syst-msgv1  syst-msgv2  syst-msgv3  syst-msgv4.
      ENDIF.
  
    ENDMETHOD.
  
  ENDCLASS.
  
  CLASS zcl_pro_grreq DEFINITION.
    PUBLIC SECTION.
      CLASS-DATA:
        cns_actvt_create  TYPE activ_auth VALUE '01', "作业类型-创建
        cns_actvt_change  TYPE activ_auth VALUE '02', "作业类型-修改
        cns_actvt_display TYPE activ_auth VALUE '03', "作业类型-显示
        cns_actvt_print   TYPE activ_auth VALUE '04'. "作业类型-打印
      CLASS-METHODS:
  *      set_initial_value,
        user_authority_check IMPORTING werks TYPE werks_d actvt TYPE activ_auth,"用户权限对象检查,传入工厂及作业类型
        get_data,"从数据库读取报表数据
        active_selection_criterion,"激活选择屏幕字段
        set_selection_criterion, "设置选择条件值
        show_alv, "显示ALV
        enqueue_aufnr_lock IMPORTING it_aufnr TYPE t_aufnr,"流程订单号加锁
        on_pro_salv_user_command   FOR EVENT added_function OF cl_salv_events_table IMPORTING e_salv_function, "pro_salv自定义按钮事件处理程序
        on_pro_salv_b_user_command   FOR EVENT added_function OF cl_salv_events_table IMPORTING e_salv_function, "pro_salv_B自定义按钮事件处理程序
        handle_after_save          FOR EVENT after_save OF zcl_pro_grreq_bill IMPORTING es_bapiret1, "入库申请单保存后的事件
        handle_double_click        FOR EVENT double_click OF cl_salv_events_table IMPORTING row column. "双击单元格事件
    PRIVATE SECTION.
      CLASS-METHODS:get_create_data,
        get_query_data.
  ENDCLASS.
  CLASS zcl_pro_grreq IMPLEMENTATION.
  *  METHOD set_initial_value.
  *    gs_layout_bill = VALUE lvc_s_layo( cwidth_opt = abap_true sel_mode = 'D' ).
  *    gs_stable = VALUE lvc_s_stbl( row = abap_true col = abap_true ).
  *    gt_vrm_values = VALUE vrm_values( ( key = '101' text = '流程订单收货' )
  *                                            ( key = '102' text = '流程订单取消收货' )
  *                                            ( key = 'Y01' text = '流程订单样品收货' )
  *                                            ( key = 'Y02' text = '流程订单样品取消收货' ) ).
  *  ENDMETHOD.
  *  用户权限检查
    METHOD user_authority_check.
      DATA lv_actvt TYPE char10.
      AUTHORITY-CHECK OBJECT 'ZPP001'
       ID 'WERKS' FIELD werks
       ID 'ACTVT' FIELD actvt.
      IF sy-subrc <> 0.
        CASE actvt.
          WHEN cns_actvt_create.
            lv_actvt = '创建'.
          WHEN cns_actvt_change.
            lv_actvt = '修改'.
          WHEN cns_actvt_display.
            lv_actvt = '显示'.
          WHEN cns_actvt_print.
            lv_actvt = '打印'.
          WHEN OTHERS.
        ENDCASE.
        MESSAGE |{ '您没有流程订单入库申请单' }{ werks }{ '工厂的' }{ lv_actvt }{ '权限!' }| TYPE 'E'.
      ENDIF.
    ENDMETHOD.
  *  激活选择屏幕字段
    METHOD active_selection_criterion.
      LOOP AT SCREEN.
        IF p_create = 'X'.
          IF screen-group1 = 'M1'.
            screen-active = '1'.
          ELSEIF screen-group1 = 'M2' OR screen-group1 = 'M3'.
            screen-active = '0'.
          ENDIF.
        ELSEIF p_query = 'X'.
          IF screen-group1 = 'M2'.
            screen-active = '1'.
          ELSEIF screen-group1 = 'M1' OR screen-group1 = 'M3'.
            screen-active = '0'.
          ENDIF.
        ELSEIF p_rqr = 'X'.
          IF screen-group1 = 'M2' OR screen-group1 = 'M3'.
            screen-active = '1'.
          ELSEIF screen-group1 = 'M1'.
            screen-active = '0'.
          ENDIF.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    ENDMETHOD.
  *  设置选择条件
    METHOD set_selection_criterion.
      CASE 'X'.
        WHEN p_create.
          user_authority_check( werks = p_dwerk actvt = cns_actvt_create )."创建权限检查
          IF p_sttxt = 'X'.
            gr_idat = VALUE #( ( sign = 'I' option = 'EQ' low = '00000000' ) )."排除TECO状态的订单条件
          ENDIF.
  
        WHEN p_query.
          user_authority_check( werks = p_dwerk actvt = cns_actvt_display )."显示权限检查
          "不显示删除行
          IF  p_del = abap_false.
            gr_zdelflg = VALUE #( ( sign = 'I' option = 'EQ' low = '' ) ).
          ENDIF.
        WHEN p_rqr.
          CLEAR gs_zqrscanauth.
          AUTHORITY-CHECK OBJECT 'ZPP005' ID 'WERKS' FIELD p_dwerk ID 'ZQRSCAN' FIELD '1'. "查询
          IF sy-subrc <> 0.
            MESSAGE e000 WITH |{ '您没有' }{ p_dwerk }{ '工厂二维码扫描查询的权限' }| DISPLAY LIKE 'S'.
          ENDIF.
          gs_zqrscanauth-query_flag = abap_true."有查询权限
          AUTHORITY-CHECK OBJECT 'ZPP005' ID 'WERKS' FIELD p_dwerk ID 'ZQRSCAN' FIELD '2'. "上传
          IF sy-subrc = 0.
            gs_zqrscanauth-upload_flag = abap_true."有上传权限
          ENDIF.
          AUTHORITY-CHECK OBJECT 'ZPP005' ID 'WERKS' FIELD p_dwerk ID 'ZQRSCAN' FIELD '3'. "审批
          IF sy-subrc = 0.
            gs_zqrscanauth-audit_flag  = abap_true."有审批权限
          ENDIF.
          AUTHORITY-CHECK OBJECT 'ZPP005' ID 'WERKS' FIELD p_dwerk ID 'ZQRSCAN' FIELD '4'. "清空文本
          IF sy-subrc = 0.
            gs_zqrscanauth-empty_flag = abap_true."有上传权限
          ENDIF.
          AUTHORITY-CHECK OBJECT 'ZPP005' ID 'WERKS' FIELD p_dwerk ID 'ZQRSCAN' FIELD '5'. "发送邮件及保存
          IF sy-subrc = 0.
            gs_zqrscanauth-save_flag = abap_true."有上传权限
          ENDIF.
          AUTHORITY-CHECK OBJECT 'ZPP005' ID 'WERKS' FIELD p_dwerk ID 'ZQRSCAN' FIELD '6'. "检验合格
          IF sy-subrc = 0.
            gs_zqrscanauth-hege_flag = abap_true."有上传权限
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDMETHOD.
    METHOD get_create_data.
  *获取流程订单表头数据
      SELECT
      afpo~dwerk, " 工厂
      afpo~dauat, " 订单类型
      afpo~aufnr, " 订单号
      aufk~ktext, " 描述
  *        afko~rsnum,  "预留
      afpo~matnr, " 物料
      afpo~meins, " 基本计量单位
      afpo~untto, "交货不足限度
      afpo~uebto, "过量交货限度
      afpo~psmng, " 订单数量
      afko~igmng, " 确认的产量
      afpo~wemng, " 收货数量
      afpo~lgort,  "仓库
      afpo~kdauf, " 销售凭证
      afpo~kdpos, " 销售凭证项目
      afpo~elikz, " 交货已完成
      afko~gstrp, " 基本开始日期
      afko~gltrp, " 基本完成日期
      afko~ftrmi, " 实际下达日期
      aufk~idat2,"技术关闭日期
      afpo~charg, " 批次
      afpo~wempf,  "备注1-交货日期
      afpo~ablad,  "备注2
      afko~dispo, " MRP控制者
      afko~fevor, " 生产管理员
      aufk~objnr  "对象号
      FROM afpo
      INNER JOIN afko ON afpo~aufnr = afko~aufnr
      INNER JOIN aufk ON  afko~aufnr = aufk~aufnr
      WHERE afpo~aufnr IN @s_aufnr  "流程订单号
      AND afpo~dwerk = @p_dwerk "工厂
      AND afpo~matnr IN @s_matnr "物料
      AND afpo~kdauf IN @s_kdauf  "销售订单
      AND afpo~kdpos IN @s_kdpos  "销售订单项
      AND afpo~verid IN @s_verid  "生产版本
      AND afpo~charg IN @s_charg   "批次号
      AND afpo~lgort IN @s_lgort  "仓库
      AND aufk~autyp = 40 "订单类别40 为流程订单
      AND aufk~idat2 IN @gr_idat "技术完成日期
      AND aufk~loekz = '' " 删除标志
      AND afko~gstrp IN @s_gstrp  " 基本开始日期
      AND afko~fevor IN @s_fevor "生产管理员
      INTO CORRESPONDING FIELDS OF TABLE @gt_pro_output.
  
      IF gt_pro_output IS NOT INITIAL.
        MOVE-CORRESPONDING gt_pro_output TO gt_matnr.
        SORT gt_matnr.
        DELETE ADJACENT DUPLICATES FROM gt_matnr.
  
        SELECT mara~matnr,mara~matkl,mara~mtart,mara~gewei,makt~maktx,t023t~wgbez "add S4DK904849
          FROM mara
          LEFT JOIN makt ON mara~matnr = makt~matnr AND makt~spras = @sy-langu
          LEFT JOIN t023t ON mara~matkl = t023t~matkl AND t023t~spras = @sy-langu
           FOR ALL ENTRIES IN @gt_matnr
          WHERE  mara~matnr = @gt_matnr-matnr
          INTO TABLE @DATA(lt_mardesc).
  
        SELECT  lgort,lgobe
          FROM t001l
          WHERE werks = @p_dwerk
          INTO TABLE @gt_t001l.
  
        MOVE-CORRESPONDING gt_pro_output TO gt_kdauf.
        DELETE gt_kdauf WHERE table_line IS INITIAL.
        SORT gt_kdauf BY kdauf kdpos.
        DELETE ADJACENT DUPLICATES FROM gt_kdauf COMPARING kdauf kdpos.
  
        SELECT vbeln,posnr,zypsl FROM vbap FOR ALL ENTRIES IN @gt_kdauf
          WHERE vbeln = @gt_kdauf-kdauf
           AND  posnr = @gt_kdauf-kdpos
          INTO TABLE @DATA(lt_vbap).
  
        LOOP AT gt_pro_output ASSIGNING FIELD-SYMBOL(<fs_pro>).
          READ TABLE lt_mardesc WITH KEY matnr = <fs_pro>-matnr INTO DATA(ls_mardesc).
          IF sy-subrc = 0.
            <fs_pro>-maktx = ls_mardesc-maktx.
            <fs_pro>-mtart = ls_mardesc-mtart.
            <fs_pro>-matkl = ls_mardesc-matkl.
            <fs_pro>-wgbez = ls_mardesc-wgbez.
            <fs_pro>-gewei = ls_mardesc-gewei.
          ENDIF.
  
          READ TABLE gt_t001l WITH KEY lgort = <fs_pro>-lgort INTO DATA(ls_t001l).
          IF sy-subrc = 0.
            <fs_pro>-lgobe = ls_t001l-lgobe.
          ENDIF.
  
          IF <fs_pro>-kdauf IS NOT INITIAL.
            READ TABLE lt_vbap WITH KEY vbeln = <fs_pro>-kdauf posnr = <fs_pro>-kdpos INTO DATA(ls_vbap).
            IF sy-subrc = 0.
              <fs_pro>-zypsl = ls_vbap-zypsl.
            ENDIF.
          ENDIF.
  
          "设置流程订单状态
          CALL FUNCTION 'STATUS_TEXT_EDIT'
            EXPORTING
              objnr            = <fs_pro>-objnr
              spras            = sy-langu
            IMPORTING
              line             = <fs_pro>-sttxt
            EXCEPTIONS
              object_not_found = 1
              OTHERS           = 2.
        ENDLOOP.
      ENDIF.
    ENDMETHOD.
    METHOD get_query_data.
      DATA:lt_kdauf LIKE TABLE OF gs_kdauf.
      SELECT
      t1~mandt,t1~zsapnote_no,t1~zsapnote_line, t1~budat,t1~werks,t1~bwart,t1~zbktxt,t1~aufnr,t1~matnr,t1~maktx,
      t1~zspec,t1~zpspe,t1~zumren,t1~lgort,t1~lgobe,t1~zwslgort,t1~zwslgobe,t1~psmng,t2~wemng,t1~zwmsmng,t1~menge,
      t1~zjsmng,t1~zwsmng,t1~meins,t1~untto,t1~uebto,t1~charg,t1~zpack_charg,t1~zhsdat,t1~zvfdat,t1~kdauf,t1~kdpos,
      t1~zypsl,t1~wempf,t1~ablad,t1~zdelflg,t1~erdat,t1~erzet,t1~uname,t1~type,t1~message,t1~aenam,t1~aedat,t1~aezeit,
  *add S4DK904849 start
            t1~zjintgew,
            t1~zntgew,
            t1~zbrgew,
            t1~zwjntgew,
            t1~zwntgew,
            t1~zwbrgew,
            t1~gewei
  *add S4DK904849 end
      FROM zppt0003 AS t1
      LEFT JOIN afpo AS t2  ON t1~aufnr = t2~aufnr
      WHERE t1~zsapnote_no IN @s_reqno
      AND t1~werks = @p_dwerk
      AND t1~aufnr IN @s_aufnr
      AND t1~matnr IN @s_matnr
      AND t1~charg IN @s_charg
      AND t1~uname IN @s_uname
      AND t1~zdelflg IN @gr_zdelflg
      AND t1~budat IN @s_budat
      INTO CORRESPONDING FIELDS OF TABLE @gt_zppt0003_list.
      CHECK gt_zppt0003_list IS NOT INITIAL.
      "20220414 新增批结、单结按钮所需要的全部的数据
      SELECT
      t1~mandt,t1~zsapnote_no,t1~zsapnote_line, t1~budat,t1~werks,t1~bwart,t1~zbktxt,t1~aufnr,t1~matnr,t1~maktx,
      t1~zspec,t1~zpspe,t1~zumren,t1~lgort,t1~lgobe,t1~zwslgort,t1~zwslgobe,t1~psmng,t2~wemng,t1~zwmsmng,t1~menge,
      t1~zjsmng,t1~zwsmng,t1~meins,t1~untto,t1~uebto,t1~charg,t1~zpack_charg,t1~zhsdat,t1~zvfdat,t1~kdauf,t1~kdpos,
      t1~zypsl,t1~wempf,t1~ablad,t1~zdelflg,t1~erdat,t1~erzet,t1~uname,t1~type,t1~message,t1~aenam,t1~aedat,t1~aezeit,
  *add S4DK904849 start
            t1~zjintgew,
            t1~zntgew,
            t1~zbrgew,
            t1~zwjntgew,
            t1~zwntgew,
            t1~zwbrgew,
            t1~gewei
  *add S4DK904849 end
      FROM zppt0003 AS t1
      LEFT JOIN afpo AS t2  ON t1~aufnr = t2~aufnr
        WHERE  t1~werks = @p_dwerk
  *    WHERE t1~zsapnote_no IN @s_reqno
  *    AND t1~werks = @p_dwerk
  *    AND t1~aufnr IN @s_aufnr
  *    AND t1~matnr IN @s_matnr
  *    AND t1~charg IN @s_charg
  *    AND t1~uname IN @s_uname
  *    AND t1~zdelflg IN @gr_zdelflg
  *    AND t1~budat IN @s_budat
      INTO CORRESPONDING FIELDS OF TABLE @gt_zppt0003_list_all.
      "20220414end
      "物料组、物料组描述
      SELECT mara~matnr,mara~matkl,t023t~wgbez
              FROM mara
              LEFT JOIN t023t ON mara~matkl = t023t~matkl AND t023t~spras = @sy-langu
               FOR ALL ENTRIES IN @gt_zppt0003_list
              WHERE  mara~matnr = @gt_zppt0003_list-matnr
              INTO TABLE @DATA(lt_mardesc).
      MOVE-CORRESPONDING gt_zppt0003_list TO lt_kdauf.
      SORT lt_kdauf BY kdauf kdpos.
      DELETE ADJACENT DUPLICATES FROM lt_kdauf COMPARING kdauf kdpos.
      IF lt_kdauf IS NOT INITIAL.
        SELECT vbeln AS kdauf,posnr AS kdpos,zsfxbz
         FROM vbap FOR ALL ENTRIES IN @lt_kdauf
        WHERE  vbeln = @lt_kdauf-kdauf
          AND posnr = @lt_kdauf-kdpos
          INTO TABLE @DATA(lt_vbap1).
        "销售订单的实际客户的属性1描述
        DELETE ADJACENT DUPLICATES FROM lt_kdauf COMPARING kdauf.
        SELECT vbeln, kunnr FROM vbpa FOR ALL ENTRIES IN @lt_kdauf
          WHERE vbeln = @lt_kdauf-kdauf
          AND posnr IS INITIAL
          AND parvw = 'Z1'
          INTO TABLE @DATA(lt_vbpa).
        SELECT v~vbeln AS kdauf, tvk1t~vtext
          FROM @lt_vbpa AS v
          INNER JOIN kna1 ON v~kunnr = kna1~kunnr
          INNER JOIN tvk1t ON kna1~katr1 = tvk1t~katr1 AND tvk1t~spras = @sy-langu
          INTO TABLE @DATA(lt_kna1).
        SORT lt_vbap1 BY kdauf kdpos.
        SORT lt_kna1 BY kdauf.
      ENDIF.
      SORT lt_mardesc BY matnr.
      LOOP AT gt_zppt0003_list ASSIGNING FIELD-SYMBOL(<fs_zppt0003_list>).
        READ TABLE lt_mardesc WITH KEY matnr = <fs_zppt0003_list>-matnr BINARY SEARCH INTO DATA(ls_mardesc).
        IF sy-subrc = 0.
          <fs_zppt0003_list>-matkl = ls_mardesc-matkl.
          <fs_zppt0003_list>-wgbez = ls_mardesc-wgbez.
        ENDIF.
        READ TABLE lt_vbap1 WITH KEY kdauf = <fs_zppt0003_list>-kdauf kdpos = <fs_zppt0003_list>-kdpos BINARY SEARCH INTO DATA(ls_vbap1).
        IF sy-subrc = 0.
          <fs_zppt0003_list>-zsfxbz = ls_vbap1-zsfxbz.
        ENDIF.
        READ TABLE lt_kna1 WITH KEY kdauf = <fs_zppt0003_list>-kdauf  BINARY SEARCH INTO DATA(ls_kna1).
        IF sy-subrc  = 0.
          <fs_zppt0003_list>-vtext = ls_kna1-vtext.
        ENDIF.
      ENDLOOP.
  
      "20220414批结单结按钮-赋值漏掉的数据
      LOOP AT gt_zppt0003_list_all ASSIGNING FIELD-SYMBOL(<fs_zppt0003_list_all>).
        READ TABLE lt_mardesc WITH KEY matnr = <fs_zppt0003_list>-matnr BINARY SEARCH INTO ls_mardesc.
        IF sy-subrc = 0.
          <fs_zppt0003_list_all>-matkl = ls_mardesc-matkl.
          <fs_zppt0003_list_all>-wgbez = ls_mardesc-wgbez.
        ENDIF.
        READ TABLE lt_vbap1 WITH KEY kdauf = <fs_zppt0003_list_all>-kdauf kdpos = <fs_zppt0003_list_all>-kdpos BINARY SEARCH INTO ls_vbap1.
        IF sy-subrc = 0.
          <fs_zppt0003_list_all>-zsfxbz = ls_vbap1-zsfxbz.
        ENDIF.
        READ TABLE lt_kna1 WITH KEY kdauf = <fs_zppt0003_list_all>-kdauf  BINARY SEARCH INTO ls_kna1.
        IF sy-subrc  = 0.
          <fs_zppt0003_list_all>-vtext = ls_kna1-vtext.
        ENDIF.
      ENDLOOP.
      "20220414END
      "存储仓库地点
      SELECT  lgort,lgobe
        FROM t001l
        WHERE werks = @p_dwerk
        INTO TABLE @gt_t001l.
    ENDMETHOD.
  
  *  根据选择条件读取流程订单或入库申请单
    METHOD get_data.
      CASE 'X'."点击哪个radiobutton
        WHEN p_create. "创建
          get_create_data( ).
        WHEN p_query.  "维护
          get_query_data( ).
        WHEN OTHERS.
      ENDCASE.
  
    ENDMETHOD.
    "流程订单号加锁
    METHOD enqueue_aufnr_lock.
      IF it_aufnr IS NOT INITIAL.
        "已加锁检查
        DATA: lv_lock_error TYPE flag,
              lv_garg       TYPE seqg3-garg,
              lt_enq        TYPE TABLE OF seqg3.
        LOOP AT it_aufnr INTO DATA(lv_aufnr).
          lv_garg = |{ sy-mandt }{ lv_aufnr }{ '0001' }|.  "键值由锁参数关键字组成
          CALL FUNCTION 'ENQUEUE_READ'
            EXPORTING
              gname                 = 'AFPO'
              garg                  = lv_garg   "键值由锁参数关键字组成
              guname                = ''
            TABLES
              enq                   = lt_enq
            EXCEPTIONS
              communication_failure = 1
              system_failure        = 2
              OTHERS                = 3.
          IF lt_enq IS NOT INITIAL.
            READ TABLE lt_enq WITH KEY gobj = 'EZ_PP_AFPO' INTO DATA(ls_enq).
            IF sy-subrc = 0.
              lv_aufnr = |{ lv_aufnr  ALPHA = OUT }|.
              CONDENSE lv_aufnr NO-GAPS.
              MESSAGE |{ '流程订单' }{ lv_aufnr }{ '已被用户' }{ ls_enq-guname }{ '在' }{ ls_enq-gtdate DATE = ISO } { ls_enq-gttime TIME = ISO }{ '引用创建入库申请单了' }| TYPE 'E'.
            ENDIF.
          ENDIF.
        ENDLOOP.
  
        "加锁
        LOOP AT it_aufnr INTO lv_aufnr.
          CALL FUNCTION 'ENQUEUE_EZ_PP_AFPO'
            EXPORTING
              mode_afpo      = 'S'
              mandt          = sy-mandt
              aufnr          = lv_aufnr
              posnr          = '0001'
  *           X_AUFNR        = ' ''
  *           X_POSNR        = ' '
  *           _SCOPE         = '2'
  *           _WAIT          = ' '
              _collect       = 'X'  "收集后统一提交
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc <> 0..
            lv_lock_error = 'X'.
            EXIT.
          ENDIF.
        ENDLOOP.
  
        IF lv_lock_error = ''.
          "使Lock Container中的缓存锁信息一次性更新到锁管理系统中
          CALL FUNCTION 'FLUSH_ENQUEUE'
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          CALL FUNCTION 'RESET_ENQUEUE'. "清除Lock Container中锁信息
        ELSE.
          CALL FUNCTION 'RESET_ENQUEUE'. "清除Lock Container中锁信息
          MESSAGE |{ '流程订单' }{ lv_aufnr }{ '加锁失败' }| TYPE 'E'.
        ENDIF.
      ENDIF.
    ENDMETHOD.
  
  *显示ALV
    METHOD show_alv.
      CASE 'X'.
        WHEN p_create."创建
          "1.创建SALV对象
          cl_salv_table=>factory( IMPORTING r_salv_table = go_pro_salv
                                  CHANGING  t_table = gt_pro_output ).
          "2.保存布局设置
          gs_program-report = sy-repid.
          go_pro_salv->get_layout( )->set_key( gs_program ).
          go_pro_salv->get_layout( )->set_save_restriction( cl_salv_layout=>restrict_none ).
          go_pro_salv->get_display_settings( )->set_list_header( '流程订单列表' )."设置标题
  
          "3.字段目录属性设置
          DATA(lo_columns) = go_pro_salv->get_columns( ).
          lo_columns->get_column( 'OBJNR' )->set_technical( abap_true ).
          lo_columns->set_optimize( abap_true ). "列宽自动优化
          lo_columns->set_exception_column('EXCEPTION'). "异常字段
  
          DATA(lo_col_errtxt) = lo_columns->get_column('ERRTXT').
          lo_col_errtxt->set_short_text('异常消息文本').
          lo_col_errtxt->set_medium_text('异常消息文本').
          lo_col_errtxt->set_long_text('异常消息文本').
  
          DATA(lo_col_wempf) = lo_columns->get_column('WEMPF').
          lo_col_wempf->set_short_text('备注1').
          lo_col_wempf->set_medium_text('备注1').
          lo_col_wempf->set_long_text('备注1').
  
          DATA(lo_col_ablad) = lo_columns->get_column('ABLAD').
          lo_col_ablad->set_short_text('备注2').
          lo_col_ablad->set_medium_text('备注2').
          lo_col_ablad->set_long_text('备注2').
  
          "4.选择模式
          go_pro_salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column )."行与字段的选择模式，报表左边追加了可以选择行的MARK按钮
  
          "5.设置ALV工具栏
          go_pro_salv->set_screen_status( report = sy-repid
                                          pfstatus = 'PRO_SALV'
                                          set_functions = go_pro_salv->c_functions_all ).
          "6.注册事件
          SET HANDLER on_pro_salv_user_command FOR go_pro_salv->get_event( ).
          SET HANDLER handle_after_save .
          "7.显示SALV
          go_pro_salv->display( ).
        WHEN p_query. "维护
          "1.创建SALV对象
          cl_salv_table=>factory( IMPORTING r_salv_table = go_pro_salv
                                  CHANGING  t_table = gt_zppt0003_list ).
          "2.保存布局设置
          gs_program-report = sy-repid.
          go_pro_salv->get_layout( )->set_key( gs_program ).
          go_pro_salv->get_layout( )->set_save_restriction( cl_salv_layout=>restrict_none ).
  
          go_pro_salv->get_display_settings( )->set_list_header( '流程订入库申请单单列表：双击一行以显示入库单申请单进行维护' )."设置标题
  
          "3.字段目录属性设置
          lo_columns = go_pro_salv->get_columns( ).
          lo_columns->get_column( 'MANDT' )->set_technical( abap_true ).
          lo_columns->get_column( 'UPD' )->set_technical( abap_true ).
          lo_columns->set_optimize( abap_true ). "列宽自动优化
          DATA(lo_col_vtext) = lo_columns->get_column( 'VTEXT' ).
          lo_col_vtext->set_short_text( '保健品送检规则' ).
          lo_col_vtext->set_medium_text( '保健品送检规则' ).
          lo_col_vtext->set_long_text( '保健品送检规则' ).
          "4.选择模式
          go_pro_salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column )."行与字段的选择模式，报表左边追加了可以选择行的MARK按钮
          "5.设置ALV工具栏
  *        GO_PRO_SALV->GET_FUNCTIONS( )->SET_ALL( ABAP_TRUE ).
  *&---------------------------------------------------------------------*
  *&创建日期：12.10.2021 16:19:35
  *&功能描述：新增按钮
  *&---------------------------------------------------------------------*
          go_pro_salv->set_screen_status( report = sy-repid
                                          pfstatus = 'PRO_SALV_B'
                                          set_functions = go_pro_salv->c_functions_all ).
  **************************END***********
          "6.注册事件
          SET HANDLER handle_double_click FOR go_pro_salv->get_event( ).
          SET HANDLER on_pro_salv_b_user_command FOR go_pro_salv->get_event( ).
          "7.显示SALV
          go_pro_salv->display( ).
        WHEN OTHERS.
      ENDCASE.
    ENDMETHOD.
  
  *  实现pro_salv自定义功能按钮事件处理程序
    METHOD on_pro_salv_user_command.
      DATA: l_key          TYPE char90,
            lv_zhsdat      TYPE zppt0003-zhsdat,
            lv_zpack_charg TYPE zppt0003-zpack_charg,
            lv_zpspe       TYPE zppt0003-zpspe,
            lt_zpspe       LIKE TABLE OF gs_zpspe,
            lv_zumren      TYPE zppt0003-zumren,
            lv_tabix       TYPE sy-tabix.
      CASE e_salv_function.
        WHEN '&CREATE'.
          DATA lt_aufnr TYPE t_aufnr.
          "获取选中的行索引
          gt_rowid =  go_pro_salv->get_selections( )->get_selected_rows( ).
          IF gt_rowid IS INITIAL.
            MESSAGE '请选择流程订单!' TYPE 'E' DISPLAY LIKE 'S'.
          ENDIF.
          "对选中的流程订单进行锁检查及加锁
          LOOP AT gt_rowid INTO DATA(ls_row).
            gs_pro_output = gt_pro_output[ ls_row ].
            IF gs_pro_output-exception = '3'."已成功创建了，不允许再创建
              MESSAGE |{ '第' }{ ls_row }{ '行订单已创建入库申请单了' } | TYPE 'E'.
              EXIT.
            ENDIF.
            SEARCH gs_pro_output-sttxt FOR 'REL'.
            IF sy-subrc <> 0.
              MESSAGE |{ '第' }{ ls_row }{ '行订单' }{ gs_pro_output-aufnr }{ '不是下达状态' } | TYPE 'E'.
              EXIT.
            ENDIF.
            APPEND gs_pro_output-aufnr TO lt_aufnr.
            "lt_aufnr = VALUE #( BASE lt_aufnr ( gt_pro_output[ ls_row ]-aufnr ) ).
          ENDLOOP.
          enqueue_aufnr_lock( lt_aufnr ).
          CLEAR: gt_zppt0003,gt_charg,gt_matnr.
  
          LOOP AT gt_rowid INTO ls_row.
            lv_tabix = sy-tabix.
            gs_pro_output = gt_pro_output[ ls_row ].
            "获取批次号的生产日期
            IF gs_pro_output-charg IS NOT INITIAL.
              TRY .
                  CLEAR: lv_zhsdat,lv_zpack_charg.
                  l_key(40) = gs_pro_output-matnr.
                  l_key+40(4) = p_dwerk.
                  l_key+44(10) = gs_pro_output-charg.
                  IF gs_pro_output-mtart = 'Z050'.
                    DATA(lt_value) = zcl_bc_public=>get_class_values( key = l_key classnum = 'BATCH_CP' classtype = '022' objtable = 'MCHA' ).
                    READ TABLE lt_value WITH KEY charact = 'Z_BATCH_DYPC' INTO DATA(ls_value).
                    IF sy-subrc = 0.
                      lv_zpack_charg = CONV #( ls_value-value_char ).
                    ENDIF.
  
                    READ TABLE lt_value WITH KEY charact = 'Z_BATCH_SCRQ' INTO ls_value.
                    IF sy-subrc = 0 AND ls_value-value_char IS  NOT INITIAL.
                      lv_zhsdat = CONV #( ls_value-value_char ).
                    ENDIF.
                  ELSE.
                    lt_value = zcl_bc_public=>get_class_values( key = l_key classnum = 'ZBATCH_SIRIO' classtype = '022' objtable = 'MCHA' ).
                    READ TABLE lt_value WITH KEY charact = 'ZPACK_CHARG' INTO ls_value.
                    IF sy-subrc = 0.
                      lv_zpack_charg = CONV #( ls_value-value_char ).
                    ENDIF.
  
                    READ TABLE lt_value WITH KEY charact = 'ZHSDAT' INTO ls_value.
                    IF sy-subrc = 0 AND ls_value-value_char IS  NOT INITIAL.
                      lv_zhsdat = CONV #( ls_value-value_char ).
                    ENDIF.
                  ENDIF.
                CATCH cx_root .
              ENDTRY.
            ENDIF.
            "计算物料货架剩余有效期
            IF lv_zhsdat IS NOT INITIAL.
              DATA(lv_zvfdat) = zcl_pro_grreq_bill=>cal_mara_vfdat( matnr = gs_pro_output-matnr start_date = lv_zhsdat ).
            ENDIF.
            gt_zppt0003 = VALUE #( BASE gt_zppt0003 (
                            zsapnote_line = lv_tabix
                            budat = sy-datum
                            werks = gs_pro_output-dwerk
                            aufnr = gs_pro_output-aufnr
                            matnr = gs_pro_output-matnr
                            maktx = gs_pro_output-maktx
  *                          lgort = gs_pro_output-lgort
  *                          lgobe = gs_pro_output-lgobe
                            untto = gs_pro_output-untto
                            uebto = gs_pro_output-uebto
                            psmng = gs_pro_output-psmng
                            wemng = gs_pro_output-wemng
                            meins = gs_pro_output-meins
                            charg = gs_pro_output-charg
                            zpack_charg = lv_zpack_charg
                            zhsdat = lv_zhsdat
                            zvfdat = lv_zvfdat
                            kdauf = gs_pro_output-kdauf
                            kdpos = gs_pro_output-kdpos
                            zypsl = gs_pro_output-zypsl
                            wempf = gs_pro_output-wempf
                            ablad = gs_pro_output-ablad
  *add S4DK904849 start
  *                          ZJINTGEW = GS_PRO_OUTPUT-ZJINTGEW
  *                          ZNTGEW = GS_PRO_OUTPUT-ZNTGEW
  *                          ZBRGEW = GS_PRO_OUTPUT-ZBRGEW
  *                          ZWJNTGEW = GS_PRO_OUTPUT-ZWJNTGEW
  *                          ZWNTGEW = GS_PRO_OUTPUT-ZWNTGEW
  *                          ZWBRGEW = GS_PRO_OUTPUT-ZWBRGEW
                            gewei = gs_pro_output-gewei
  *add S4DK904849 end
                           ) ).
  
            "物料批次号信息，用于ALV批次单元格编辑后的验证,以及单据保存前的批次检查
            gt_charg = VALUE #( BASE gt_charg ( aufnr = gs_pro_output-aufnr  matnr = gs_pro_output-matnr charg = gs_pro_output-charg hsdat = lv_zhsdat ) ).
            gt_matnr = VALUE #( BASE gt_matnr ( matnr = gs_pro_output-matnr meins = gs_pro_output-meins ) ).
          ENDLOOP.
          "更新物料规格、包装规格、物料PC单位与基本计量单位换算率
          SORT gt_matnr.
          DELETE ADJACENT DUPLICATES FROM gt_matnr.
          SELECT matnr,meins,groes,matkl FROM mara FOR ALL ENTRIES IN @gt_matnr
         WHERE mara~matnr = @gt_matnr-matnr
         INTO TABLE @DATA(lt_geros).
          LOOP AT lt_geros INTO DATA(ls_greos).
            "物料分类的包装规格
            lv_zpspe = zcl_bc_public=>get_class_value( atnam = 'ZPSPE' key = ls_greos-matnr classtype = '001' classnum = ls_greos-matkl objtable = 'MARA' )."包装规格特征值
            "物料PC单位等于多少基本计量单位数量
            CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
              EXPORTING
                i_matnr              = ls_greos-matnr "物料编号
                i_in_me              = 'ST' "PC(件)单位的内码
                i_out_me             = ls_greos-meins "基本计量单位
                i_menge              = 1
              IMPORTING
                e_menge              = lv_zumren
              EXCEPTIONS
                error_in_application = 1
                error                = 2
                OTHERS               = 3.
            IF sy-subrc <> 0.
              CLEAR lv_zumren.
            ENDIF.
            lt_zpspe = VALUE #( BASE lt_zpspe ( matnr = ls_greos-matnr zpspe = lv_zpspe zumren = lv_zumren ) ).
          ENDLOOP.
  
          LOOP AT gt_zppt0003 ASSIGNING FIELD-SYMBOL(<fs>).
            READ TABLE lt_geros WITH KEY matnr = <fs>-matnr INTO DATA(ls_geros).
            IF sy-subrc = 0.
              <fs>-zspec = ls_geros-groes.
            ENDIF.
            READ TABLE lt_zpspe WITH KEY matnr = <fs>-matnr INTO gs_zpspe.
            IF sy-subrc = 0.
              <fs>-zpspe = gs_zpspe-zpspe. "包装规格
              <fs>-zumren = gs_zpspe-zumren."PC与基本单位换算率
            ENDIF.
          ENDLOOP.
  
          "存储物料启用批次管理(工厂)标识,用于单据保存的批次检查
          CLEAR gt_matnr.
          MOVE-CORRESPONDING gt_charg TO gt_matnr.
          SORT gt_matnr.
          DELETE ADJACENT DUPLICATES FROM gt_matnr.
          IF gt_matnr IS NOT INITIAL.
            SELECT matnr,xchpf FROM marc FOR ALL ENTRIES IN @gt_matnr
              WHERE matnr = @gt_matnr-matnr
              AND werks = @p_dwerk
              INTO TABLE @DATA(lt_marc).
  
            SORT lt_marc BY matnr.
            LOOP AT gt_charg ASSIGNING FIELD-SYMBOL(<fs_charg>).
              READ TABLE lt_marc WITH KEY matnr = <fs_charg>-matnr BINARY SEARCH INTO DATA(ls_marc).
              IF  sy-subrc = 0.
                <fs_charg>-xchpf = ls_marc-xchpf.
              ENDIF.
            ENDLOOP.
          ENDIF.
          "显示创建入库申请单界面
          IF go_zcl_pro_grreq_bill IS NOT BOUND.
            go_zcl_pro_grreq_bill = NEW zcl_pro_grreq_bill(  ).
          ENDIF.
          go_zcl_pro_grreq_bill->set_bill_status( imp_bill_status = zcl_pro_grreq_bill=>cns_status_created ).
          go_zcl_pro_grreq_bill->display( )."显示创建入库申请界面
  
        WHEN OTHERS.
      ENDCASE.
    ENDMETHOD.
  **************Seashell 20211013新增按钮
    METHOD on_pro_salv_b_user_command.
      DATA:   lv_tabix       TYPE sy-tabix.
      DATA: lo_send_lims     TYPE REF TO zcl_lims_oper,
            ls_lims_response TYPE zsign_batch_sessionless_soap_o,
            i_wa_selfield    TYPE slis_selfield.
  
      DATA:
        ls_layout TYPE lvc_s_layo,
        lv_grid   TYPE REF TO cl_gui_alv_grid.
      DATA ls_stable TYPE lvc_s_stbl.
      CLEAR gt_rowid.
      CLEAR:gt_zppt0003_lista,gt_zppt0003_listb,gt_zppt0003_list_del,gt_zppt0003_list_add,gt_zppt0003_list_mod.
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
  *   EXPORTING
  *     IR_SALV_FULLSCREEN_ADAPTER       =
        IMPORTING
  *       ET_EXCLUDING                     =
  *       E_REPID                          =
  *       E_CALLBACK_PROGRAM               =
  *       E_CALLBACK_ROUTINE               =
          e_grid = lv_grid.
  
  
      CASE e_salv_function.
  
          "当点击批结按钮
        WHEN '&PJ'.
          gt_rowid =  go_pro_salv->get_selections( )->get_selected_rows( ).
          IF gt_rowid IS INITIAL .
            MESSAGE '没有选择数据' TYPE 'E'.
          ENDIF.
          LOOP AT gt_rowid INTO DATA(gs_rowid).
  
            READ TABLE gt_zppt0003_list INTO gs_zppt0003_list INDEX gs_rowid.
            IF sy-subrc = 0.
              "判断订单是否已经批结或者已经单结
              IF gs_zppt0003_list-wempf = '批结' OR gs_zppt0003_list-wempf = '此单结束'.
                CLEAR gt_zppt0003_lista.
                MESSAGE '含有已批结或单结的订单' TYPE 'E'.
              ENDIF.
              "如果上面的校验符合要求的话，将上面的数据写入到LISTA表中
              APPEND gs_zppt0003_list TO gt_zppt0003_lista.
            ENDIF.
            CLEAR:gs_rowid,gs_zppt0003_list.
          ENDLOOP.
          "剔除Y01移动类型
          DELETE gt_zppt0003_lista WHERE bwart = 'Y01'.
          "获取流程订单号最大的订单，以便后续的操作
          "获取流程订单号最大的订单，目的是用于判断是否为最晚日期的订单
          "1.对LISTA表根据工单号、流程订单号进行倒叙排序，然后去重，这样可以保留申请单号最大的那张订单
          IF gt_zppt0003_lista IS NOT INITIAL.
            SORT gt_zppt0003_lista  BY aufnr zsapnote_no DESCENDING.
            DELETE ADJACENT DUPLICATES FROM gt_zppt0003_lista COMPARING aufnr zsapnote_no.
          ENDIF.
          "将全部的数据进行排序，然后保存在gt_zppt0003_list_temp表中，gt_zppt0003_list_temp表保存的是界面的申请单号最大的订单，然后方便后续2张内表做对比
          MOVE gt_zppt0003_list_all TO gt_zppt0003_list_temp.
  
          "剔除Y01移动类型
          DELETE gt_zppt0003_list_temp WHERE bwart = 'Y01'.
          SORT gt_zppt0003_list_temp BY aufnr zsapnote_no DESCENDING.
          "去重，保留界面订单号最大的订单
          DELETE ADJACENT DUPLICATES FROM gt_zppt0003_list_temp COMPARING aufnr.
          LOOP AT gt_zppt0003_lista INTO gs_zppt0003_list.
            READ TABLE gt_zppt0003_list_temp INTO DATA(gs_zppt0003_list_temp) WITH KEY aufnr = gs_zppt0003_list-aufnr.
            IF sy-subrc = 0.
              APPEND gs_zppt0003_list_temp TO gt_zppt0003_listb.
            ENDIF.
            CLEAR:gs_zppt0003_list,gs_zppt0003_list_temp.
          ENDLOOP.
  
          "LISTB表保存的是，选中的流程订单号在界面数据中申请单号最大的订单
          "LISTA表保存的是，选中的流程订单号在选中数据中申请单号最大的订单
          "对比2张表，如果存在不一致的情况的话，那就证明说存在冲突，即存在不是最晚日期的订单
          CALL FUNCTION 'CTVB_COMPARE_TABLES'
            EXPORTING
              table_old  = gt_zppt0003_lista[]
              table_new  = gt_zppt0003_listb[]
              key_length = '22'
  *           IF_SORTED  =
            IMPORTING
              table_del  = gt_zppt0003_list_del
              table_add  = gt_zppt0003_list_add
              table_mod  = gt_zppt0003_list_mod
  *           NO_CHANGES =
            .
  
          IF gt_zppt0003_list_del IS NOT INITIAL OR gt_zppt0003_list_add IS NOT INITIAL OR gt_zppt0003_list_mod IS NOT INITIAL.
            CLEAR:gt_zppt0003_lista,gt_zppt0003_listb,gt_zppt0003_list_temp,gs_zppt0003_list.
            MESSAGE '存在不是最晚日期的订单' TYPE 'E'.
          ENDIF.
  
          "进行批结的操作
          LOOP AT gt_zppt0003_lista INTO gs_zppt0003_list.
            "只允许移动类型101的数据下发LIMS
            IF gs_zppt0003_list-bwart = '101'.
              gs_zppt0003_list-wempf = '批结'.
              "保存需要发送的数据
              APPEND gs_zppt0003_list TO gt_zppt0003.
              "调用LIMS的接口
              lo_send_lims = NEW zcl_lims_oper( ).
              "***************通过批结按钮触发LIMS接口***********************
              ls_lims_response = lo_send_lims->send_to_lims_pj( ).
              "需要下发选中的记录，调用LIMS成功后再更新入库申请单记录
              IF ls_lims_response-sign_batch_sessionless_result-ret_code = 'S'.
                MESSAGE '成功' TYPE 'S'.
                READ TABLE gt_zppt0003_list INTO gs_zppt0003_list_temp WITH KEY aufnr = gs_zppt0003_list-aufnr zsapnote_no = gs_zppt0003_list-zsapnote_no.
                IF sy-subrc = 0.
                  gs_zppt0003_list_temp-wempf = '批结'.
                  MODIFY gt_zppt0003_list FROM gs_zppt0003_list_temp TRANSPORTING wempf WHERE aufnr = gs_zppt0003_list_temp-aufnr AND zsapnote_no = gs_zppt0003_list_temp-zsapnote_no.
                  MODIFY zppt0003 FROM gs_zppt0003_list_temp.
                  COMMIT WORK.
                ENDIF.
  
              ELSEIF ls_lims_response-sign_batch_sessionless_result-ret_code <> 'S'.
                MESSAGE ls_lims_response-sign_batch_sessionless_result-ret_msg TYPE 'E'.
              ENDIF.
            ENDIF.
          ENDLOOP.
          "刷新界面数据
          i_wa_selfield-refresh = 'X'.
          i_wa_selfield-col_stable = 'X'.
          i_wa_selfield-row_stable = 'X'.
          CALL METHOD lv_grid->get_frontend_layout
            IMPORTING
              es_layout = ls_layout.
  
          ls_layout-cwidth_opt = 'X'.
  
          CALL METHOD lv_grid->set_frontend_layout
            EXPORTING
              is_layout = ls_layout.
          CALL METHOD lv_grid->check_changed_data."获取ALV改变值
          CALL METHOD lv_grid->refresh_table_display."刷新ALV界面
  
          "当点击此单结束按钮
        WHEN '&CDJS'.
          DATA gt_close_success LIKE TABLE OF gs_close.
          "获取选中的数据
          gt_rowid =  go_pro_salv->get_selections( )->get_selected_rows( ).
          IF gt_rowid IS INITIAL .
            MESSAGE '没有选择数据' TYPE 'E'.
          ENDIF.
          LOOP AT gt_rowid INTO gs_rowid.
  
            READ TABLE gt_zppt0003_list INTO gs_zppt0003_list INDEX gs_rowid.
            IF sy-subrc = 0.
              "判断订单是否已经批结或者已经单结
              IF gs_zppt0003_list-wempf = '批结' OR gs_zppt0003_list-wempf = '此单结束'.
                CLEAR gt_zppt0003_lista.
                MESSAGE '含有已批结或单结的订单' TYPE 'E'.
              ENDIF.
              "如果上面的校验符合要求的话，将上面的数据写入到LISTA表中
              APPEND gs_zppt0003_list TO gt_zppt0003_lista.
            ENDIF.
            CLEAR:gs_rowid,gs_zppt0003_list.
          ENDLOOP.
          "剔除Y01移动类型
          DELETE gt_zppt0003_lista WHERE bwart = 'Y01'.
          "获取流程订单号最大的订单，以便后续的操作
          "获取流程订单号最大的订单，目的是用于判断是否为最晚日期的订单
          "1.对LISTA表根据工单号、流程订单号进行倒叙排序，然后去重，这样可以保留申请单号最大的那张订单
          IF gt_zppt0003_lista IS NOT INITIAL.
            SORT gt_zppt0003_lista  BY aufnr zsapnote_no DESCENDING.
            DELETE ADJACENT DUPLICATES FROM gt_zppt0003_lista COMPARING aufnr zsapnote_no.
          ENDIF.
          "将全部的数据进行排序，然后保存在TEMP表中，TEMP表保存的是界面的申请单号最大的订单，然后方便后续2张内表做对比
          MOVE gt_zppt0003_list_all TO gt_zppt0003_list_temp.
          "剔除Y01移动类型
          DELETE gt_zppt0003_list_temp WHERE bwart = 'Y01'.
          SORT gt_zppt0003_list_temp BY aufnr zsapnote_no DESCENDING.
          "去重，保留界面订单号最大的订单
          DELETE ADJACENT DUPLICATES FROM gt_zppt0003_list_temp COMPARING aufnr.
          LOOP AT gt_zppt0003_lista INTO gs_zppt0003_list.
            READ TABLE gt_zppt0003_list_temp INTO gs_zppt0003_list_temp WITH KEY aufnr = gs_zppt0003_list-aufnr.
            IF sy-subrc = 0.
              APPEND gs_zppt0003_list_temp TO gt_zppt0003_listb.
            ENDIF.
            CLEAR:gs_zppt0003_list,gs_zppt0003_list_temp.
          ENDLOOP.
          "LISTB表保存的是，选中的流程订单号在界面数据中申请单号最大的订单
          "LISTA表保存的是，选中的流程订单号在选中数据中申请单号最大的订单
          "对比2张表，如果存在不一致的情况的话，那就证明说存在冲突，即存在不是最晚日期的订单
          CALL FUNCTION 'CTVB_COMPARE_TABLES'
            EXPORTING
              table_old  = gt_zppt0003_lista[]
              table_new  = gt_zppt0003_listb[]
              key_length = '22'
  *           IF_SORTED  =
            IMPORTING
              table_del  = gt_zppt0003_list_del
              table_add  = gt_zppt0003_list_add
              table_mod  = gt_zppt0003_list_mod
  *           NO_CHANGES =
            .
  
          IF gt_zppt0003_list_del IS NOT INITIAL OR gt_zppt0003_list_add IS NOT INITIAL OR gt_zppt0003_list_mod IS NOT INITIAL.
            CLEAR:gt_zppt0003_lista,gt_zppt0003_listb,gt_zppt0003_list_temp,gs_zppt0003_list.
            MESSAGE '存在不是最晚日期的订单' TYPE 'E'.
          ENDIF.
          "根据选中的数据，反查流程订单相关的销售订单行，对行项目进行关单操作
          IF gt_zppt0003_lista IS NOT INITIAL.
            SELECT DISTINCT
              vbeln
              posnr
              etenr
            INTO TABLE gt_close
            FROM vbep
            FOR ALL ENTRIES IN gt_zppt0003_lista
            WHERE vbep~vbeln = gt_zppt0003_lista-kdauf
              AND vbep~posnr = gt_zppt0003_lista-kdpos
            ORDER BY PRIMARY KEY.
          ENDIF.
  
          "循环读取需要关闭的订单，进行关单的操作
          LOOP AT gt_close INTO gs_close.
            CLEAR: salesdocument,order_header_in,order_header_inx,order_item_in,order_item_inx,return1,return,schedule_lines,schedule_linesx.
            CLEAR: order_item_in[],order_item_inx[],return1[],return[],schedule_lines[],schedule_linesx[].
            CLEAR: ls_schedule_linesx,ls_schedule_lines,ls_order_item_in,ls_order_item_inx.
            order_header_inx-updateflag = 'U'.
  
            ls_order_item_in-itm_number     =  gs_close-posnr.
            APPEND ls_order_item_in TO order_item_in.
  
            ls_order_item_inx-itm_number     =  gs_close-posnr.
            ls_order_item_inx-updateflag     =  'U'.
            APPEND ls_order_item_inx TO order_item_inx.
  *
            ls_schedule_lines-itm_number = gs_close-posnr.
            ls_schedule_lines-sched_line = gs_close-etenr.
            ls_schedule_lines-sched_type = 'ZB'.
  *        SCHEDULE_LINES-REQ_DATE = '20180808'.
  *        SCHEDULE_LINES-REQ_QTY    = '35'. "4.  "数量
            APPEND ls_schedule_lines TO  schedule_lines.
  *
            ls_schedule_linesx-itm_number = gs_close-posnr.
            ls_schedule_linesx-sched_line = gs_close-etenr.
            ls_schedule_linesx-updateflag = 'U'.
            ls_schedule_linesx-sched_type = 'X'.
  *        SCHEDULE_LINESX-REQ_QTY    = 'X'.
  *        SCHEDULE_LINESX-REQ_DATE = 'X'.
  *
  *        APPEND SCHEDULE_LINESX.
  *        SCHEDULE_LINESX-ITM_NUMBER = '000010'.
  *        SCHEDULE_LINESX-UPDATEFLAG = 'D'.
  *        SCHEDULE_LINESX-SCHED_TYPE = 'X'.
  *        SCHEDULE_LINESX-REQ_QTY    = 'X'.
  *        SCHEDULE_LINESX-REQ_DATE = 'X'.
  *
            APPEND ls_schedule_linesx TO schedule_linesx.
  
            CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
              EXPORTING
                salesdocument    = gs_close-vbeln
                order_header_inx = order_header_inx
              TABLES
                return           = return1
                order_item_in    = order_item_in
                order_item_inx   = order_item_inx
                schedule_lines   = schedule_lines
                schedule_linesx  = schedule_linesx.
            READ TABLE return1 INTO ls_return1 WITH KEY type = 'E'.
            IF ls_return1 <> 'E'.
  
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.
              MOVE return1 TO return.
              "将关闭的订单反写前台表数据,只需要修改当前工单，其他工单不受影响
              LOOP AT gt_zppt0003_lista INTO gs_zppt0003_list WHERE kdauf = gs_close-vbeln AND kdpos = gs_close-posnr.
                gs_zppt0003_list-wempf = '此单结束'.
                MODIFY gt_zppt0003_list FROM gs_zppt0003_list TRANSPORTING wempf WHERE aufnr = gs_zppt0003_list-aufnr AND zsapnote_no = gs_zppt0003_list-zsapnote_no.
                MODIFY zppt0003 FROM gs_zppt0003_list.
                COMMIT WORK.
              ENDLOOP.
              MESSAGE '此单结束成功' TYPE 'S'.
  *            LOOP AT gt_zppt0003_list INTO gs_zppt0003_list WHERE kdauf = gs_close-vbeln AND kdpos = gs_close-posnr.
  *              gs_zppt0003_list-wempf = '此单结束'.
  *              MODIFY gt_zppt0003_list FROM gs_zppt0003_list TRANSPORTING wempf WHERE aufnr = gs_zppt0003_list-aufnr AND zsapnote_no = gs_zppt0003_list-zsapnote_no.
  *              MODIFY zppt0003 FROM gs_zppt0003_list.
  *              COMMIT WORK.
  *            ENDLOOP.
              APPEND gs_close TO gt_close_success.
            ELSE.
  
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  
            ENDIF.
            CLEAR gs_close.
          ENDLOOP.
          "单结要调用lims 的接口，单结成功的数据保存在了gt_close_success
          "反查选中的lista表的数据，剔除掉关单失败的数据
          LOOP AT gt_zppt0003_lista INTO gs_zppt0003_list.
            READ TABLE gt_close_success INTO gs_close  WITH KEY vbeln  = gs_zppt0003_list-kdauf
                                                                posnr = gs_zppt0003_list-kdpos.
            IF sy-subrc <> 0.
              DELETE gt_zppt0003_lista FROM gs_zppt0003_list.
            ENDIF.
          ENDLOOP.
          "接下来调用lims的接口
          "进行批结的操作
          LOOP AT gt_zppt0003_lista INTO gs_zppt0003_list.
            "只允许移动类型101的数据下发LIMS
            IF gs_zppt0003_list-bwart = '101'.
              gs_zppt0003_list-wempf = '此单结束'.
              "保存需要发送的数据
              APPEND gs_zppt0003_list TO gt_zppt0003.
              "调用LIMS的接口
              lo_send_lims = NEW zcl_lims_oper( ).
              ls_lims_response = lo_send_lims->send_to_lims_pj( ).
            ELSEIF ls_lims_response-sign_batch_sessionless_result-ret_code <> 'S'.
              MESSAGE ls_lims_response-sign_batch_sessionless_result-ret_msg TYPE 'S' DISPLAY LIKE 'E'.
            ENDIF.
          ENDLOOP.
  
  
          "刷新界面数据
          i_wa_selfield-refresh = 'X'.
          i_wa_selfield-col_stable = 'X'.
          i_wa_selfield-row_stable = 'X'.
          CALL METHOD lv_grid->get_frontend_layout
            IMPORTING
              es_layout = ls_layout.
  
          ls_layout-cwidth_opt = 'X'.
  
          CALL METHOD lv_grid->set_frontend_layout
            EXPORTING
              is_layout = ls_layout.
          CALL METHOD lv_grid->check_changed_data."获取ALV改变值
          CALL METHOD lv_grid->refresh_table_display."刷新ALV界面
  
  
        WHEN OTHERS.
      ENDCASE.
    ENDMETHOD.
  
  ****************end***************
  
  
  
    "入库申请单保存后回写操作结果消息到此报表中
    METHOD handle_after_save.
      DATA: lv_exception TYPE char1.   "信号灯字段-处理结果
      IF es_bapiret1-type = 'S'.
        lv_exception = '3'.
      ELSE.
        lv_exception = '1'.
      ENDIF.
      LOOP AT gt_rowid INTO DATA(ls_rowid).
        gt_pro_output[ ls_rowid ]-exception = lv_exception.
        gt_pro_output[ ls_rowid ]-errtxt = es_bapiret1-message.
      ENDLOOP.
      go_pro_salv->refresh( VALUE lvc_s_stbl( row = 'X'  col = 'X'  )  ).
    ENDMETHOD.
  
    "双击单元格实现入库申请单
    METHOD handle_double_click.
      zppt0003 = gt_zppt0003_list[ row ].
      SELECT
         t1~mandt,
         t1~zsapnote_no,
         t1~zsapnote_line,
         t1~budat,
         t1~werks,
         t1~bwart,
         t1~zbktxt,
         t1~aufnr,
         t1~matnr,
         t1~maktx,
         t1~zspec,
         t1~zpspe,
         t1~zumren,
         t1~lgort,
         t1~lgobe,
         t1~zwslgort,
         t1~zwslgobe,
         t1~untto,
         t1~uebto,
         t1~psmng,
         t2~wemng,
         t1~zwmsmng,
         t1~menge,
         t1~zjsmng,
         t1~zwsmng,
         t1~meins,
         t1~charg,
         t1~zpack_charg,
         t1~zhsdat,
         t1~zvfdat,
         t1~kdauf,
         t1~kdpos,
         t1~zypsl,
         t1~wempf,
         t1~ablad,
         t1~zdelflg,
         t1~erdat,
         t1~erzet,
         t1~uname,
         t1~type,
         t1~message,
         t1~aenam,
         t1~aedat,
  *add S4DK904849 start
         t1~zjintgew,
         t1~zntgew,
         t1~zbrgew,
         t1~zwjntgew,
         t1~zwntgew,
         t1~zwbrgew,
         t1~gewei
  *add S4DK904849 end
     FROM zppt0003 AS t1
     LEFT JOIN afpo AS t2  ON t1~aufnr = t2~aufnr
     WHERE t1~zsapnote_no = @zppt0003-zsapnote_no
     INTO CORRESPONDING FIELDS OF TABLE @gt_zppt0003.
  
      IF gt_zppt0003 IS INITIAL.
        MESSAGE |{ '入库申请单' }{ zppt0003-zsapnote_no }{ '的数据不存在!' }| TYPE 'E'.
      ENDIF.
      "存储物料启用批次管理(工厂)标识,用于单据保存的批次检查
      CLEAR gt_charg.
      MOVE-CORRESPONDING gt_zppt0003 TO gt_charg.
      SORT gt_charg.
      DELETE ADJACENT DUPLICATES FROM gt_charg.
  
      IF gt_charg IS NOT INITIAL.
        DATA(lv_werks) = gt_zppt0003_list[ row ]-werks.
        SELECT matnr,xchpf FROM marc FOR ALL ENTRIES IN @gt_charg
            WHERE matnr = @gt_charg-matnr
            AND werks = @lv_werks
            INTO TABLE @DATA(lt_marc).
        SORT lt_marc BY matnr.
        LOOP AT gt_charg ASSIGNING FIELD-SYMBOL(<fs_charg>).
          READ TABLE lt_marc WITH KEY matnr = <fs_charg>-matnr BINARY SEARCH INTO DATA(ls_marc).
          IF  sy-subrc = 0.
            <fs_charg>-xchpf = ls_marc-xchpf.
          ENDIF.
        ENDLOOP.
      ENDIF.
      "显示创建入库申请单界面
      IF go_zcl_pro_grreq_bill IS NOT BOUND.
        go_zcl_pro_grreq_bill = NEW zcl_pro_grreq_bill( ).
      ENDIF.
      go_zcl_pro_grreq_bill->set_bill_status( imp_bill_status = zcl_pro_grreq_bill=>cns_status_display ).
      go_zcl_pro_grreq_bill->display( )."显示创建入库申请界面
    ENDMETHOD.
  
  
  ENDCLASS.