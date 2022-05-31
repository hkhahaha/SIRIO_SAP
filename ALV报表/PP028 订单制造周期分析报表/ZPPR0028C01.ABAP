*&---------------------------------------------------------------------*
*& 包含               ZPPR0028C01
*&---------------------------------------------------------------------*
CLASS zlcl_event_receiver DEFINITION.
    PUBLIC SECTION.
      METHODS:
        "增加/减少 toolbar 按钮
        handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive,
        handle_after_user_command FOR EVENT after_user_command OF  cl_gui_alv_grid IMPORTING e_ucomm  e_saved e_not_processed,
        "LINK_CLICK事件处理程序
        handle_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row   column,
        "ALV单元格编辑后的处理事件
        handle_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING sender e_modified et_good_cells.
  ENDCLASS.
  CLASS zlcl_event_receiver  IMPLEMENTATION.
  *  *LINK_CLICK事件处理程序
    METHOD  handle_link_click .
      gs_alv =  gt_alv_output[ row ].
      CASE column.
        WHEN 'VBELN' .
          SET PARAMETER ID 'AUN' FIELD gs_alv-vbeln.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    ENDMETHOD.
    METHOD handle_after_user_command.
      DATA:lt_sel_rows TYPE lvc_t_row,
           lr_matkl    TYPE RANGE OF zppt0006-matkl,
           lr_matnr    TYPE RANGE OF zppt0007-matnr,
           lr_werks    TYPE RANGE OF zppt0006-werks.
      CASE e_ucomm.
        WHEN 'ADDROW'.
          IF p_halb = 'X'.
            APPEND INITIAL LINE TO gt_zppt0006.
          ELSEIF p_roh = 'X'.
            APPEND INITIAL LINE TO gt_zppt0007.
          ENDIF.
          go_alv_grid->refresh_table_display( is_stable = gs_stable ).
        WHEN 'DELROW'.
          REFRESH lt_sel_rows.
          go_alv_grid->get_selected_rows( IMPORTING  et_index_rows = lt_sel_rows ).
          IF lt_sel_rows IS  INITIAL.
            MESSAGE s000 WITH '请选中待删除的行'.
            RETURN.
          ENDIF.
          IF p_halb = 'X'."删除ZPPT0006表数据
            LOOP AT lt_sel_rows INTO DATA(ls_row).
              gs_zppt0006 = gt_zppt0006[ ls_row-index ].
              gt_zppt0006[ ls_row-index ]-werks = 'DELD'."待删除标识
              IF gs_zppt0006-matkl IS NOT INITIAL AND gs_zppt0006-werks IS NOT INITIAL.
                lr_matkl = VALUE #( BASE lr_matkl ( sign = 'I' option = 'EQ' low = gs_zppt0006-matkl ) ).
                lr_werks = VALUE #( BASE lr_werks ( sign = 'I' option = 'EQ' low = gs_zppt0006-werks ) ).
              ENDIF.
            ENDLOOP.
            DELETE  gt_zppt0006 WHERE werks = 'DELD'.
            IF lr_werks IS NOT INITIAL AND lr_matkl IS NOT INITIAL.
              DELETE FROM zppt0006 WHERE werks IN lr_werks AND matkl IN lr_matkl.
              COMMIT WORK AND WAIT .
            ENDIF.
          ELSEIF p_roh = 'X'."删除ZPPT0007表数据
            LOOP AT lt_sel_rows INTO ls_row.
              gs_zppt0007 = gt_zppt0007[ ls_row-index ].
              gt_zppt0007[ ls_row-index ]-werks = 'DELD'."待删除标识
              IF gs_zppt0007-matnr IS NOT INITIAL AND gs_zppt0007-werks IS NOT INITIAL.
                lr_matnr = VALUE #( BASE lr_matnr ( sign = 'I' option = 'EQ' low = gs_zppt0007-matnr ) ).
                lr_werks = VALUE #( BASE lr_werks ( sign = 'I' option = 'EQ' low = gs_zppt0007-werks ) ).
              ENDIF.
            ENDLOOP.
            DELETE  gt_zppt0007 WHERE werks = 'DELD'.
            IF lr_werks IS NOT INITIAL AND lr_matnr IS NOT INITIAL.
              DELETE FROM zppt0007 WHERE werks IN lr_werks AND matnr IN lr_matnr.
              COMMIT WORK AND WAIT.
            ENDIF.
          ENDIF.
          go_alv_grid->refresh_table_display( is_stable = gs_stable ).
          MESSAGE s000 WITH '删除成功!'.
      ENDCASE.
  
    ENDMETHOD.
    "alv的toolbar按钮增加删除实现
    METHOD handle_toolbar.
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
      <fs_button>-function = 'ADDROW'.
      <fs_button>-icon = '@17@'.
      <fs_button>-text = '插入行'.
  
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-butn_type = '3'.  "分割符
      APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <fs_button>.
      <fs_button>-function = 'DELROW'.
      <fs_button>-icon = '@18@'.
      <fs_button>-text = '删除行'.
  
    ENDMETHOD.
    "ALV单元格编辑后的处理程序
    METHOD handle_data_changed_finished.
      DATA:lv_refresh TYPE char1.
      LOOP AT et_good_cells INTO DATA(ls_good_cells) WHERE   error = ''.
        IF p_halb = 'X'.
          ASSIGN gt_zppt0006[ ls_good_cells-row_id ] TO FIELD-SYMBOL(<fs_zppt0006>).
          CHECK <fs_zppt0006> IS ASSIGNED.
          CASE ls_good_cells-fieldname.
            WHEN 'MATKL'."物料组
              SELECT SINGLE t023~matkl, t023t~wgbez
                FROM t023
                LEFT JOIN t023t ON t023~matkl = t023t~matkl AND t023t~spras = @sy-langu
                WHERE t023~matkl = @ls_good_cells-value
                INTO @DATA(ls_t023).
              IF sy-subrc = 0.
                <fs_zppt0006>-wgbez = ls_t023-wgbez.
              ELSE.
                CLEAR:<fs_zppt0006>-matkl,<fs_zppt0006>-wgbez.
                MESSAGE s000 WITH '输入的物料组无效'.
              ENDIF.
              lv_refresh = abap_true.
            WHEN 'WERKS'.
              SELECT SINGLE werks INTO @DATA(lv_werks) FROM t001w WHERE werks = @ls_good_cells-value.
              IF sy-subrc <> 0.
                CLEAR <fs_zppt0006>-werks.
                MESSAGE s000 WITH '输入的工厂无效'.
              ENDIF.
              lv_refresh = abap_true.
            WHEN 'ZDZEIT'.
              lv_refresh = abap_true.
            WHEN OTHERS.
          ENDCASE.
          <fs_zppt0006>-upd = abap_true."修改标识
        ELSEIF p_roh = 'X'.
          ASSIGN gt_zppt0007[ ls_good_cells-row_id ] TO FIELD-SYMBOL(<fs_zppt0007>).
          CHECK <fs_zppt0007> IS ASSIGNED.
          CASE ls_good_cells-fieldname.
            WHEN 'MATNR'."物料编号
              SELECT SINGLE mara~matnr,makt~maktx
                FROM mara LEFT JOIN makt ON mara~matnr = makt~matnr AND makt~spras = @sy-langu
                WHERE mara~matnr = @ls_good_cells-value
                INTO @DATA(ls_mara).
              IF sy-subrc = 0.
                <fs_zppt0007>-maktx = ls_mara-maktx.
              ELSE.
                CLEAR:<fs_zppt0007>-matnr,<fs_zppt0007>-maktx.
                MESSAGE s000 WITH '输入的物料编号无效'.
              ENDIF.
              lv_refresh = abap_true.
            WHEN 'WERKS'.
              SELECT SINGLE werks INTO @lv_werks FROM t001w WHERE werks = @ls_good_cells-value.
              IF sy-subrc <> 0.
                CLEAR <fs_zppt0007>-werks.
                MESSAGE s000 WITH '输入的工厂无效'.
              ENDIF.
              lv_refresh = abap_true.
            WHEN 'ZPRETIME'.
              lv_refresh = abap_true.
            WHEN OTHERS.
          ENDCASE.
          <fs_zppt0007>-upd = abap_true."修改标识
        ENDIF.
      ENDLOOP.
      IF   lv_refresh = abap_true..
        go_alv_grid->set_frontend_layout( gs_layout ).
        CALL METHOD go_alv_grid->refresh_table_display
          EXPORTING
            is_stable = gs_stable
          EXCEPTIONS
            finished  = 1
            OTHERS    = 2.
      ENDIF.
  
    ENDMETHOD.
  ENDCLASS.
  DATA go_event_receiver  TYPE REF TO zlcl_event_receiver .