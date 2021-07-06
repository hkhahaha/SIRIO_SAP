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
    IF go_con_tab1 IS NOT INITIAL.
      go_con_tab1->free( ).
    ENDIF.
    IF go_con_tab2  IS NOT INITIAL.
      go_con_tab2->free( ).
    ENDIF.
    IF go_salv_table  IS NOT INITIAL.
      FREE go_salv_table .
    ENDIF.
    IF go_alv_grid IS NOT INITIAL.
      FREE go_alv_grid.
    ENDIF.
    LEAVE TO SCREEN 0.
  ENDFORM.
  
  FORM frm_get_data.
    DATA: lv_tabix TYPE sy-tabix,
          gv_line  TYPE bsvx-sttxt.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING percentage = 50 text = '正在查询'.
    SELECT afko~aufnr,afpo~dwerk,afpo~charg,afpo~verid,afko~aufld,afpo~matnr,afpo~psmng,afpo~psamg,
      afpo~wemng,aufk~erdat,afpo~meins,afko~gstri,aufk~zchej,aufk~zchanx,aufk~objnr
      FROM afko
      INNER JOIN aufk ON afko~aufnr = aufk~aufnr
      INNER JOIN afpo ON afko~aufnr = afpo~aufnr
      WHERE afko~gstri IN @s_gstri "实际开始日期
      AND afpo~dwerk IN @s_dwerk "工厂
      AND afpo~charg IN @s_charg
      AND afpo~xloek = '' "实际开始日期
      AND aufk~zchej IN @s_zchej "车间
      AND aufk~zchanx IN @s_zchanx "产线
      INTO  CORRESPONDING FIELDS OF TABLE @gt_pro.
    IF gt_pro IS INITIAL.
      MESSAGE s000 WITH '未查询流程订单批次'.
      STOP.
    ENDIF.
    IF gt_pro IS NOT INITIAL.
      SELECT mara~matnr,mara~mtart,mara~matkl,makt~maktx
        FROM mara
        LEFT JOIN makt ON mara~matnr = makt~matnr AND spras = @sy-langu
        FOR ALL ENTRIES IN @gt_pro
            WHERE mara~matnr = @gt_pro-matnr
            INTO TABLE @DATA(lt_mara).
    ENDIF.
    SORT lt_mara BY matnr.
    LOOP AT gt_pro ASSIGNING FIELD-SYMBOL(<fs_pro>).
      lv_tabix = sy-tabix.
      READ TABLE lt_mara WITH KEY matnr = <fs_pro>-matnr BINARY SEARCH INTO DATA(ls_mara).
      CHECK sy-subrc = 0.
      IF ls_mara-mtart = 'Z030'.
        <fs_pro>-mtart = ls_mara-mtart.
        <fs_pro>-matkl = ls_mara-matkl.
        <fs_pro>-maktx = ls_mara-maktx.
        CALL FUNCTION 'STATUS_TEXT_EDIT'
          EXPORTING
            objnr            = <fs_pro>-objnr
            spras            = sy-langu
            flg_user_stat    = 'X'
          IMPORTING
            line             = gv_line
  *         user_line        =
          EXCEPTIONS
            object_not_found = 01.
        IF sy-subrc = 0.
          <fs_pro>-sttxt = gv_line.
        ENDIF.
      ELSE.
        DELETE gt_pro INDEX lv_tabix.
      ENDIF.
    ENDLOOP.
    DELETE gt_pro WHERE charg IS INITIAL.
    IF gt_pro IS INITIAL.
      MESSAGE s000 WITH '未查询流程订单批次'.
      STOP.
    ENDIF.
    SORT gt_pro BY charg aufnr.
  ENDFORM.
  FORM frm_reset_selcondition.
    DATA:lt_charg1 LIKE TABLE OF gs_charg,
         lt_charg2 LIKE TABLE OF gs_charg,
         lt_charg  LIKE TABLE OF gs_charg.
    "物料或物料组选择条件不为空的时候，需重置批次选择条件
    IF s_matnr IS NOT INITIAL OR s_matkl IS NOT INITIAL.
      SELECT charg FROM afpo INNER JOIN mara ON afpo~matnr = mara~matnr
        WHERE afpo~matnr IN @s_matnr
        AND afpo~dwerk IN @s_dwerk
        AND afpo~xloek = '' "删除标记
        AND afpo~dauty = '40' "订单类别
        AND mara~matkl IN @s_matkl
       INTO TABLE @lt_charg1.
      SORT lt_charg1 BY charg.
      DELETE ADJACENT DUPLICATES FROM lt_charg1 COMPARING charg.
      IF lt_charg1 IS INITIAL.
        MESSAGE s000 WITH '所指定的物料或物料组未查询流程订单批次'.
        STOP.
      ENDIF.
      IF s_charg IS NOT INITIAL.
        SELECT charg FROM afpo
             WHERE  afpo~charg IN @s_charg
             AND afpo~xloek = '' "删除标记
             AND afpo~dauty = '40' "订单类别
            INTO TABLE @lt_charg2.
        SORT lt_charg2 BY charg.
        DELETE ADJACENT DUPLICATES FROM lt_charg2 COMPARING charg.
        IF lt_charg2 IS INITIAL.
          MESSAGE s000 WITH '所指定的批次未查询流程订单批次'.
          STOP.
        ENDIF.
      ENDIF.
      "重置批次选择条件
      IF s_charg IS  INITIAL.
        REFRESH s_charg.
        LOOP AT lt_charg1 INTO gs_charg.
          CLEAR s_charg.
          s_charg-sign = 'I'.
          s_charg-option = 'EQ'.
          s_charg-low = gs_charg-charg.
          APPEND s_charg.
        ENDLOOP.
      ELSE.
        REFRESH s_charg.
        LOOP AT lt_charg1 INTO gs_charg.
          READ TABLE lt_charg2 WITH KEY charg = gs_charg-charg BINARY SEARCH TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            CLEAR s_charg.
            s_charg-sign = 'I'.
            s_charg-option = 'EQ'.
            s_charg-low = gs_charg-charg.
            APPEND s_charg.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDFORM.
  FORM frm_set_col_text USING iv_colname TYPE lvc_fname iv_coltext  TYPE scrtext_s.
    DATA(lo_column) = go_salv_columns->get_column( iv_colname ).
    lo_column->set_short_text( iv_coltext ).
    lo_column->set_medium_text( CONV scrtext_m( iv_coltext ) ).
    lo_column->set_long_text( CONV scrtext_l( iv_coltext ) ).
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
  *& Form FRM_AUTH_CHECK
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_auth_check .
    SELECT werks FROM t001w WHERE werks IN @s_dwerk INTO TABLE @DATA(lt_t001w).
    IF lt_t001w IS INITIAL.
      MESSAGE s000 WITH '工厂参数无效'.
      LEAVE TO LIST-PROCESSING."返回到选择屏幕
    ENDIF.
    LOOP AT lt_t001w INTO DATA(ls_t001w).
      AUTHORITY-CHECK OBJECT 'ZPP003' ID 'WERKS' FIELD ls_t001w-werks.
      IF sy-subrc <> 0.
        MESSAGE s000 WITH |{ '你没有工厂' }{ ls_t001w-werks }{ '的权限!' }|.
        LEAVE TO LIST-PROCESSING."返回到选择屏幕
      ENDIF.
    ENDLOOP.
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_DISPLAY_PRO_ALV
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_display_pro_alv .
    IF go_con_tab1 IS NOT BOUND.
      CREATE OBJECT go_con_tab1 EXPORTING container_name = 'ZCON_9100'.
  *    TRY.
      "1.创建SALV对象
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          list_display = if_salv_c_bool_sap=>false
          r_container  = go_con_tab1
        " container_name = 'dockingcon1'
        IMPORTING
          r_salv_table = go_salv_table
        CHANGING
          t_table      = gt_pro.
      "2.保存布局设置
      go_salv_table->get_layout( )->set_key( VALUE salv_s_layout_key( report = sy-repid ) ).
      go_salv_table->get_layout( )->set_save_restriction( cl_salv_layout=>restrict_none ).
      "go_salv_table->get_display_settings( )->set_list_header( '选中要执行LTP的行，未选中任何行即表示全部执行' )."设置标题
      "3.字段目录属性设置
      go_salv_columns =  go_salv_table->get_columns( ).
      go_salv_columns->set_optimize( abap_true ). "列宽自动优化
      go_salv_columns->get_column( 'SJSL' )->set_technical( abap_true ).
      go_salv_columns->get_column( 'AUSSS1' )->set_technical( abap_true ).
      go_salv_columns->get_column( 'AUSSS2' )->set_technical( abap_true ).
      go_salv_columns->get_column( 'LLSL' )->set_technical( abap_true ).
      go_salv_columns->get_column( 'PSAMG1' )->set_technical( abap_true ).
      go_salv_columns->get_column( 'PSAMG2' )->set_technical( abap_true ).
      go_salv_columns->get_column( 'PROAUSSS1' )->set_technical( abap_true ).
      go_salv_columns->get_column( 'PROAUSSS2' )->set_technical( abap_true ).
      go_salv_columns->get_column( 'SCLLSL' )->set_technical( abap_true ).
      "4.选择模式
      go_salv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column )."行与字段的选择模式，报表左边追加了可以选择行的MARK按钮
      "5.设置ALV工具栏
      go_salv_table->get_functions( )->set_all( abap_true ).
      "6.显示SALV
      go_salv_table->display( ).
  *     CATCH cx_salv_msg .
  *    ENDTRY.
    ENDIF.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_CONV_DATE_FOR_9200
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_conv_data_for_9200 .
    DATA: lt_pro   LIKE TABLE OF zppsprofis,
          lt_charg LIKE TABLE OF gs_charg.
    SORT gt_pro BY charg aufnr.
    MOVE-CORRESPONDING gt_pro TO lt_charg.
    SORT lt_charg BY charg.
    DELETE ADJACENT DUPLICATES FROM lt_charg COMPARING charg.
    LOOP AT lt_charg INTO gs_charg.
      CHECK gs_charg-charg IS NOT INITIAL.
      REFRESH lt_pro.
      lt_pro = FILTER #( gt_pro USING KEY sk_charg WHERE charg = gs_charg-charg ).
      PERFORM frm_add_data_to_prosl USING lt_pro.
    ENDLOOP.
  *DATA: result_tab TYPE match_result_tab.
  *FIND ALL OCCURRENCES OF REGEX `((ab)|(ba))`
  *IN 'abba'
  *     RESULTS result_tab.
  ENDFORM.
  *添加批次的流程订单到产品收率表
  FORM frm_add_data_to_prosl USING it_pro TYPE ty_tab_zppsprofis.
    DATA: lt_result_tab TYPE match_result_tab,
          lv_error      TYPE flag,
          ls_pro        TYPE zppsprofis,
          ls_pro1       TYPE zppsprofis,
          ls_pro2       TYPE zppsprofis,
          lv_lines      TYPE int4,
          lv_mng        TYPE co_wemng,
          lv_progasmg1  TYPE gasmg,
          lv_progasmg2  TYPE gasmg,
          lv_ausss      TYPE p DECIMALS 4,
          lv_ausss1     TYPE ausss,
          lv_ausss2     TYPE ausss.
    CHECK it_pro IS NOT INITIAL.
    SORT it_pro BY aufnr.
    "批次的流程订单只要有任何一个流程订单收货数量存在0，既不显示到结果
    READ TABLE it_pro WITH KEY wemng = 0 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.
    "批次的流程订单只要有任何一个流程订单状态不包含(TECO|结算)，既不显示到结果
    LOOP AT it_pro INTO ls_pro.
      CLEAR lv_error.
      REFRESH lt_result_tab.
      FIND ALL OCCURRENCES OF REGEX '((TECO)|(结算))'
      IN ls_pro-sttxt
      RESULTS lt_result_tab.
      IF lt_result_tab IS INITIAL.
        lv_error = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF lv_error = 'X'.
      RETURN.
    ENDIF.
    "添加流程订单到产品收率表
    ls_pro1 = it_pro[ 1 ]."第一个流程订单
    lv_lines = lines( it_pro ).
    IF lv_lines > 1.
      ls_pro2 = it_pro[ 2 ]."第二个流程订单
    ENDIF.
    CLEAR ls_pro.
    ls_pro-aufnr = ls_pro1-aufnr.
    ls_pro-dwerk = ls_pro1-dwerk.
    ls_pro-charg = ls_pro1-charg.
    ls_pro-verid = ls_pro1-verid.
    ls_pro-aufld = ls_pro1-aufld."BOM展开/工艺路线转换的日期
    ls_pro-matnr = ls_pro1-matnr.
    ls_pro-mtart = ls_pro1-mtart.
    ls_pro-matkl = ls_pro1-matkl.
    ls_pro-maktx = ls_pro1-maktx.
    IF ls_pro1-meins = ls_pro2-meins.
      ls_pro-psmng = ls_pro2-psmng.
    ELSE.
      ls_pro-psmng = ls_pro1-psmng.
    ENDIF.
    ls_pro-wemng = ls_pro1-wemng.
    ls_pro-erdat = ls_pro1-erdat.
    ls_pro-meins = ls_pro1-meins.
    ls_pro-gstri = ls_pro1-gstri.
    ls_pro-zchej = ls_pro1-zchej.
    ls_pro-zchanx = ls_pro1-zchanx.
    lv_mng = ls_pro-wemng / ls_pro-psmng."实际收率
    ls_pro-sjsl = |{ lv_mng * 100 DECIMALS = 2 }{ '%' }|."实际收率(%)
    "装配报废率1(%)
    CLEAR: lv_ausss,lv_ausss1,lv_ausss2.
    SELECT SINGLE ausss INTO lv_ausss1 FROM marc WHERE matnr = ls_pro1-matnr AND werks = ls_pro1-dwerk.
    IF lv_ausss1 IS NOT INITIAL.
      ls_pro-ausss1 = |{ lv_ausss1 }{ '%' }|.
    ENDIF.
    "装配报废率2(%)
    IF ls_pro2-matnr IS NOT INITIAL AND ls_pro2-dwerk IS NOT INITIAL.
      SELECT SINGLE ausss INTO lv_ausss2 FROM marc WHERE matnr = ls_pro2-matnr AND werks = ls_pro2-dwerk.
      IF lv_ausss2 IS NOT INITIAL.
        ls_pro-ausss2 = |{ lv_ausss2 }{ '%' }|.
      ENDIF.
    ENDIF.
    "当前理论收率
    lv_ausss = 1 / ( ( 1 + lv_ausss1 / 100 ) * ( 1 + lv_ausss2 / 100 ) ).
    IF lv_ausss IS NOT INITIAL.
      ls_pro-llsl = |{ lv_ausss * 100 DECIMALS = 2 }{ '%' }|.
    ENDIF.
    ls_pro-psamg1 = ls_pro1-psamg."废品1
    ls_pro-psamg2 = ls_pro2-psamg."废品2
    CLEAR: lv_progasmg1, lv_progasmg2.
    "订单1报废率
    IF ls_pro1-psamg > 0.
      lv_progasmg1 = ls_pro1-psamg / ( ls_pro1-psmng - ls_pro1-psamg ).
      ls_pro-proausss1 = |{ lv_progasmg1 * 100 DECIMALS = 2 }{ '%' }|.
    ENDIF.
    "订单2报废率
    IF ls_pro2-psamg > 0.
      lv_progasmg2 = ls_pro2-psamg / ( ls_pro2-psmng - ls_pro2-psamg ).
      ls_pro-proausss2 = |{ lv_progasmg2 * 100 DECIMALS = 2 }{ '%' }|.
    ENDIF.
    lv_mng = 1 / ( ( 1 + lv_progasmg1 ) * ( 1 + lv_progasmg2 ) )."生产时理论收率
    IF lv_mng IS NOT INITIAL.
      ls_pro-scllsl = |{ lv_mng * 100 DECIMALS = 2 }{ '%' }|."生产时理论收率
    ENDIF.
    ls_pro-objnr = ls_pro1-objnr.
    ls_pro-sttxt = ls_pro1-sttxt.
    SELECT SINGLE znote FROM zppt0012 WHERE aufnr = @ls_pro1-aufnr INTO @DATA(lv_znote).
    IF sy-subrc = 0.
      ls_pro-znote = lv_znote.
    ENDIF.
    APPEND ls_pro TO  gt_pro_sl.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_DISPLAY_GRID_ALV
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_display_grid_alv .
    IF go_con_tab2 IS NOT BOUND.
      CREATE OBJECT go_con_tab2 EXPORTING container_name = 'ZCON_9200'.
      go_alv_grid  = NEW cl_gui_alv_grid( i_parent = go_con_tab2 ).
      "alv字段目录
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'ZPPSPROFIS'
        CHANGING
          ct_fieldcat            = gt_alv_fidcat
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.
      LOOP AT gt_alv_fidcat ASSIGNING FIELD-SYMBOL(<fidcat>).
        CASE <fidcat>-fieldname.
          WHEN 'OBJNR' OR 'PSAMG' OR 'STTXT' OR 'PSAMG1' OR 'PSAMG2'.
            <fidcat>-tech = abap_true.
          WHEN 'ZNOTE'.
            <fidcat>-edit = abap_true.
        ENDCASE.
      ENDLOOP.
      gs_layout-cwidth_opt = abap_true.
      gs_layout-sel_mode = 'D'.
      CREATE OBJECT go_cl_event_receiver.
      "注册编辑事件，使ALV编辑后回车，或离开焦点时可以进行数据校验
      SET HANDLER go_cl_event_receiver->handle_toolbar FOR go_alv_grid.
      SET HANDLER go_cl_event_receiver->handle_after_user_command FOR go_alv_grid.
      go_alv_grid->set_toolbar_interactive( ).
      go_alv_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
    ENDIF.
    "显示ALV
    CALL METHOD go_alv_grid->set_table_for_first_display
      EXPORTING
        i_save                        = 'A'
        i_default                     = 'X'
        is_variant                    = VALUE #( report = sy-repid )
        is_layout                     = gs_layout
      CHANGING
        it_outtab                     = gt_pro_sl
        it_fieldcatalog               = gt_alv_fidcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ENDFORM.