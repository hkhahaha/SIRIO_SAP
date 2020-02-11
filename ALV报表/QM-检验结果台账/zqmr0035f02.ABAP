*&---------------------------------------------------------------------*
*& 包含               ZMMR0020F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  frm_SHOW_DATA
*&---------------------------------------------------------------------*
*       显示ALV
*----------------------------------------------------------------------*
FORM frm_show_data TABLES pt_table.
    CLEAR:gt_fieldcat[],gs_layout.
 *ALV布局
    PERFORM frm_alv_list_layout.
 * ALV事件
 *  PERFORM frm_event_build USING GT_events[].
 *ALV字段设置
    PERFORM frm_alv_fill_name USING gt_fieldcat.
 * ALV排序
 *  PERFORM frm_sort_field CHANGING GT_sort[].
 *ALV显示
    PERFORM frm_alv_list_disply TABLES pt_table.
  ENDFORM. " frm_SHOW_DATA
 
 *&---------------------------------------------------------------------*
 *&      Form  frm_ALV_LIST_LAYOUT
 *&---------------------------------------------------------------------*
 *       LAYOUT##
 *----------------------------------------------------------------------*
  FORM frm_alv_list_layout .
 *   gs_layout-box_fieldname        = 'SEL'.
    gs_layout-colwidth_optimize    = 'X'.
    gs_layout-zebra                = 'X'.
    gs_layout-detail_initial_lines = 'X'.
 *  gs_layout-numc_sum = 'X'.
 *  gs_layout-coltab_fieldname = 'CELL_COLOR'.
  ENDFORM. " frm_ALV_LIST_LAYOUT
 
 *&---------------------------------------------------------------------*
 *&      Form  frm_alv_fill_name
 *&---------------------------------------------------------------------*
 *       ####
 *----------------------------------------------------------------------*
  FORM frm_alv_fill_name USING gt_fieldcat TYPE slis_t_fieldcat_alv.
 
    PERFORM frm_alv_set_fieldcat USING:
         space 'prueflos' space 'qals' space space space space space space space 'X' space space space space space space gt_fieldcat,
         space 'pastrterm' space 'qals' space space space space space space space space space space space space space space gt_fieldcat,
         space 'paendterm' space 'qals' space space space space space space space space space space space space space space gt_fieldcat,
         space 'aufnr' space 'afpo' space space space space space space space 'X' space space space space space space gt_fieldcat,
         space 'kdauf' space 'afpo' space space space space space space space 'X' space space space space space space gt_fieldcat,
         space 'landx' space space space space '出口国家' space space space space space space space space space space space gt_fieldcat,
         space 'name1' space space space space '客户名称' space space space space space space space space space space space gt_fieldcat,
         space 'matnr ' space space space space '产品编号' space space space space space space space space space space space gt_fieldcat,
         space 'maktx ' space space space space '产品名称' space space space space space space space space space space space gt_fieldcat,
         space 'zpspe ' space space space space '规格型号' space space space space space space space space space space space gt_fieldcat,
         space 'lmengeist ' space 'qals' space space '数量' space space space space space space space space space 'X' space gt_fieldcat,
         space 'mengeneinh ' space 'qals' space space '单位' space space space space space space space space space space space gt_fieldcat,
         space 'charg ' space 'qals' space space '批次' space space space space space space space space space 'X' space gt_fieldcat,
         space 'matnr_bcp ' space space space space '半成品编码' space space space space space space space space space space space gt_fieldcat,
         space 'maktx_bcp ' space space space space '半成品名称' space space space space space space space space space space space gt_fieldcat,
         space 'prueflos_bcp' space space space space '半成品检验批' space space space space 'X' space space space space space space gt_fieldcat,
         space 'charg_bcp ' space space space space '工艺批号' space space space space space space space space space 'X' space gt_fieldcat,
         space 'sortfeld ' space space space space '检验项目' space space space space space space space space space space space gt_fieldcat,
 *        不显示下述两列，waymon 20191029
 *        space 'mittelwert ' space space space space '结果 - 定量' space space space space space space space space space space gt_fieldcat,
 *        space 'pruefbemkt ' space space space space '结果 - 定性' space space space space space space space space space space gt_fieldcat,
         space 'jg ' space space space space '结果' space space space space space space space space space 'X' 'X' gt_fieldcat,
         space 'mbewertg ' space space space space '评价' space space space space space space space space space space space gt_fieldcat,
         space 'pjzw ' space space space space '评价中文' space space space space space space space space space space space gt_fieldcat
         .
 
  ENDFORM. " frm_alv_fill_name
 
 *&-------------------------------------------------------------------*
 *&      Form  frm_alv_set_fieldcat
 *&-------------------------------------------------------------------*
 *       Adds an entry to the field catalog.
 *--------------------------------------------------------------------*
  FORM frm_alv_set_fieldcat USING uv_key TYPE any
                                   uv_fieldname     TYPE any
                                   uv_ref_fieldname TYPE any
                                   uv_ref_tabname   TYPE any
                                   uv_outputlen     TYPE any
                                   uv_noout         TYPE any
                                   uv_seltext_m     TYPE any
                                   uv_seltext_l     TYPE any
                                   uv_seltext_s     TYPE any
                                   uv_reptext_ddic  TYPE any
                                   uv_ddictxt       TYPE any
                                   uv_hotspot       TYPE any
                                   uv_showasicon    TYPE any
                                   uv_checkbox      TYPE any
                                   uv_edit          TYPE any
                                   uv_dosum         TYPE any
                                   uv_no_zero       TYPE any
                                   uv_lzero         TYPE any
                                   ct_fieldcat      TYPE slis_t_fieldcat_alv.
    DATA: ls_fieldcat  TYPE slis_fieldcat_alv,
          lv_fname(20).
    CLEAR ls_fieldcat.
 
 * General settings
    IF uv_fieldname IS NOT INITIAL.
      lv_fname = uv_fieldname.
      TRANSLATE lv_fname TO UPPER CASE.
      ls_fieldcat-fieldname = lv_fname.
    ELSE.
      EXIT.
    ENDIF.
 
    ls_fieldcat-key = uv_key.
 
    ls_fieldcat-no_out = uv_noout.
    ls_fieldcat-hotspot = uv_hotspot.
    ls_fieldcat-checkbox = uv_checkbox.
    ls_fieldcat-icon = uv_showasicon.
    ls_fieldcat-do_sum = uv_dosum.
 *  ls_fieldcat-fix_column = uv_fix_column.
    IF uv_no_zero IS INITIAL.
      ls_fieldcat-no_zero = 'X'.
    ENDIF.
 
    IF uv_ref_tabname IS INITIAL.
      ls_fieldcat-rollname =   uv_ref_fieldname.
    ELSE.
      ls_fieldcat-ref_tabname = uv_ref_tabname.
      IF uv_ref_fieldname EQ space.
        ls_fieldcat-ref_fieldname =   ls_fieldcat-fieldname.
      ELSE.
        ls_fieldcat-ref_fieldname =   uv_ref_fieldname.
      ENDIF.
      TRANSLATE ls_fieldcat-ref_tabname TO UPPER CASE.
    ENDIF.
 
 * Set output length.
    IF NOT uv_outputlen IS INITIAL.
      ls_fieldcat-outputlen = uv_outputlen.
    ENDIF.
 
 * Set text headers.
    IF NOT uv_seltext_m IS INITIAL.
      ls_fieldcat-seltext_m = uv_seltext_m.
    ENDIF.
    IF NOT uv_seltext_l IS INITIAL.
      ls_fieldcat-seltext_l = uv_seltext_l.
    ENDIF.
    IF NOT uv_seltext_s IS INITIAL.
      ls_fieldcat-seltext_s = uv_seltext_s.
    ENDIF.
    IF NOT uv_reptext_ddic IS INITIAL.
      ls_fieldcat-reptext_ddic = uv_reptext_ddic.
    ENDIF.
    IF NOT uv_ddictxt IS INITIAL.
      ls_fieldcat-ddictxt = uv_ddictxt.
    ENDIF.
 
 * Set as editable or not.
    IF uv_edit IS NOT INITIAL.
      ls_fieldcat-input     = 'X'.
      ls_fieldcat-edit     = 'X'.
    ENDIF.
    APPEND ls_fieldcat TO ct_fieldcat.
  ENDFORM. "frm_alv_set_fieldcat
 
 **&---------------------------------------------------------------------*
 **&      Form  frm_sort_field
 **&---------------------------------------------------------------------*
 **       text
 **----------------------------------------------------------------------*
 **      -->PT_SOTR    text
 **----------------------------------------------------------------------*
 * FORM frm_sort_field CHANGING pt_sort TYPE slis_t_sortinfo_alv.
 *
 *   DEFINE set_sort.
 *     gs_sort-fieldname = &1.
 *     gs_sort-up        = &2.
 *     gs_sort-down      = &3.
 *     APPEND gs_sort TO &4.
 *     CLEAR  gs_sort.
 *   END-OF-DEFINITION.
 *
 **  set_sort:'ZDATE'  space 'X' pt_sort,
 **           'ZTIME'  space 'X' pt_sort,
 **           'DOCNUM' space 'X' pt_sort.
 *
 * ENDFORM. "frm_sort_field
 
 *&---------------------------------------------------------------------*
 *&      Form  frm_ALV_LIST_DISPLY
 *&---------------------------------------------------------------------*
 *       ALV显示
 *----------------------------------------------------------------------*
  FORM frm_alv_list_disply TABLES pt_table.
 
 *ALV显示
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = sy-repid
 *      i_callback_pf_status_set = gv_status
        i_callback_user_command = gv_comand
        it_fieldcat             = gt_fieldcat
        is_layout               = gs_layout
 *      it_sort                 = gt_sort
 *      i_callback_html_top_of_page = gv_top_of_page "HTML_TOP_OF_PAGE'
 *      i_grid_title            = gv_title
        i_save                  = 'A'
      TABLES
        t_outtab                = pt_table.
  ENDFORM. " frm_ALV_LIST_DISPLY
 
 *&---------------------------------------------------------------------*
 *&      Form  FRM_STANDARD_ALV
 *&---------------------------------------------------------------------*
 *       设置菜单
 *----------------------------------------------------------------------*
  FORM frm_standard_alv USING ut_extab TYPE slis_t_extab.
 
    DEFINE set_menu.
      gs_status_tab = &1.
      APPEND gs_status_tab TO gt_status_tab.
    END-OF-DEFINITION.
 
    CLEAR gt_status_tab[].
 
    SET PF-STATUS 'PF001' EXCLUDING gt_status_tab.
 *  SET TITLEBAR 'TITLE_0100' WITH lv_title.
 
  ENDFORM. "FRM_STANDARD_ALV
 
 *&---------------------------------------------------------------------*
 *&      Form  FRM_USER_COMMAND
 *&---------------------------------------------------------------------*
 *       用户响应事件
 *----------------------------------------------------------------------*
  FORM frm_user_command USING uv_ucomm LIKE sy-ucomm
                                 us_selfield  TYPE slis_selfield.
 
    us_selfield-refresh = 'X'.     "刷新
    us_selfield-col_stable = 'X'.  "列位置
    us_selfield-row_stable = 'X'.  "行位置
 
    CASE uv_ucomm.
 *  双击
      WHEN '&IC1'.
        READ TABLE gt_alv INTO gs_alv INDEX us_selfield-tabindex.
        CASE us_selfield-fieldname.
          WHEN 'PRUEFLOS_BCP'.
            zcl_cm_public=>call_tcode( iv_id = 'QLS' iv_id_value = gs_alv-prueflos_bcp iv_tcode = 'QA03' ).
          WHEN 'PRUEFLOS'.
            zcl_cm_public=>call_tcode( iv_id = 'QLS' iv_id_value = gs_alv-prueflos iv_tcode = 'QA03' ).
          WHEN 'KDAUF'.
            zcl_cm_public=>call_tcode( iv_id = 'AUN' iv_id_value = gs_alv-kdauf iv_tcode = 'VA03' ).
          WHEN 'AUFNR'.
            zcl_cm_public=>call_tcode( iv_id = 'BR1' iv_id_value = gs_alv-aufnr iv_tcode = 'COR3' ).
        ENDCASE.
    ENDCASE.
 
  ENDFORM. "FRM_USER_COMMAND