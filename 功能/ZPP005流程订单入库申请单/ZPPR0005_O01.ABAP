*----------------------------------------------------------------------*
***INCLUDE ZPPR0005_O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9100 OUTPUT.
  SET PF-STATUS 'STATUS9100'.
  IF go_zcl_pro_grreq_bill->bill_status = zcl_pro_grreq_bill=>cns_status_created.
    SET TITLEBAR 'MAIN9100_1'.
  ELSEIF go_zcl_pro_grreq_bill->bill_status = zcl_pro_grreq_bill=>cns_status_changed.
    SET TITLEBAR 'MAIN9100_2'.
  ELSE.
    SET TITLEBAR 'MAIN9100_3'.
  ENDIF.
  IF go_docking IS NOT BOUND.
    CREATE OBJECT go_docking
      EXPORTING
*       parent                      =
        repid                       = sy-repid
        dynnr                       = sy-dynnr
        side                        = cl_gui_docking_container=>dock_at_bottom
*       extension                   = 50
*       style                       =
*       lifetime                    = lifetime_default
*       caption                     = ''
*       metric                      = 0
        ratio                       = 85 "容器离屏幕top的外边距比率
*       no_autodef_progid_dynnr     =
*       name                        =
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.


    go_alv_grid_create = NEW cl_gui_alv_grid( i_parent = go_docking ).
    go_zcl_pro_grreq_bill->register_alv_handle_event( )."注册ALV事件处理程序方法
    go_zcl_pro_grreq_bill->set_drdn_table( )."订单状态字段设置下拉框

*ALV字段目录
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZPPT0003'
      CHANGING
        ct_fieldcat            = gt_alv_fidcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.


**    设置单据头移动类型字段的下拉列表值
*
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        id     = 'ZSPP034_HEAD-BWART'
*        values = gt_vrm_values.
*
*    "不同单据状态，设置字段的呈现方式
*    IF go_zcl_pro_grreq_bill->bill_status = zcl_pro_grreq_bill=>cns_status_created."新增单据状态
*      zspp034_head-werks = p_dwerk.
*      go_zcl_pro_grreq_bill->set_alv_fieldcatelog( zcl_pro_grreq_bill=>cns_status_created )."设置ALV字段目录属性
*      go_zcl_pro_grreq_bill->set_bill_status( imp_bill_status = zcl_pro_grreq_bill=>cns_status_created imp_refresh_alv = abap_false ). "设置新增单据状态
*
*    ELSEIF go_zcl_pro_grreq_bill->bill_status = zcl_pro_grreq_bill=>cns_status_changed."修改单据状态
*
*
*    ENDIF.
*
*
*    CALL METHOD go_alv_grid_create->set_table_for_first_display
*      EXPORTING
*        is_layout                     = VALUE #( cwidth_opt = abap_true sel_mode = 'D' )
*      CHANGING
*        it_outtab                     = gt_zppt0003
*        it_fieldcatalog               = gt_alv_fidcat
*      EXCEPTIONS
*        invalid_parameter_combination = 1
*        program_error                 = 2
*        too_many_lines                = 3
*        OTHERS                        = 4.
*
*
*  ELSE.
*    go_alv_grid_create->refresh_table_display( ).
  ENDIF.

*    设置单据头移动类型字段的下拉列表值

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'ZSPP034_HEAD-BWART'
      values = gt_vrm_values.

  "设置不同单据状态抬头屏幕值
  IF go_zcl_pro_grreq_bill->bill_status = zcl_pro_grreq_bill=>cns_status_created."新增单据状态
    CLEAR zspp034_head-zsapnote_no.
    zspp034_head-werks = p_dwerk.
    IF zspp034_head-bwart IS INITIAL.
      zspp034_head-bwart = gt_vrm_values[ 1 ]-key.
    ENDIF.

    go_zcl_pro_grreq_bill->set_bill_status( imp_bill_status = zcl_pro_grreq_bill=>cns_status_created ). "设置新增单据状态

  ELSEIF go_zcl_pro_grreq_bill->bill_status = zcl_pro_grreq_bill=>cns_status_changed."修改状态
    go_zcl_pro_grreq_bill->set_bill_status( imp_bill_status = zcl_pro_grreq_bill=>cns_status_changed ). "设置修改单据状态
  ELSE.
    MOVE-CORRESPONDING gt_zppt0003[ 1 ] TO zspp034_head."单据抬头赋值
    go_zcl_pro_grreq_bill->set_bill_status( imp_bill_status = zcl_pro_grreq_bill=>cns_status_display ). "设置显示单据状态
  ENDIF.

*设置屏幕字段编辑状态
  go_zcl_pro_grreq_bill->set_screen_field_editable( go_zcl_pro_grreq_bill->bill_status ).

  CALL METHOD go_alv_grid_create->set_table_for_first_display
    EXPORTING
      i_save                        = 'A'
      is_variant                    = VALUE #( report = sy-repid )
      is_layout                     = gs_layout_bill
    CHANGING
      it_outtab                     = gt_zppt0003
      it_fieldcatalog               = gt_alv_fidcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.



ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9200 OUTPUT.
  SET PF-STATUS 'STATUS9200'.
  SET TITLEBAR 'MAIN9200'.
  IF go_docking IS NOT BOUND.
    CREATE OBJECT go_docking
      EXPORTING
*       parent                      =
        repid                       = sy-repid
        dynnr                       = sy-dynnr
*       side                        = cl_gui_docking_container=>dock_at_bottom
        extension                   = 2000
*       style                       =
*       lifetime                    = lifetime_default
*       caption                     = ''
*       metric                      = 0
*       ratio                       = 85 "容器离屏幕top的外边距比率
*       no_autodef_progid_dynnr     =
*       name                        =
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    go_alv_grid_create = NEW cl_gui_alv_grid( i_parent = go_docking ).
    SET HANDLER go_zcl_qr_oper->handle_alv_toolbar FOR go_alv_grid_create.
    SET HANDLER go_zcl_qr_oper->handle_after_user_command FOR go_alv_grid_create.
    SET HANDLER go_zcl_qr_oper->handle_data_changed_finished FOR go_alv_grid_create.
    go_alv_grid_create->set_toolbar_interactive( ).
    go_alv_grid_create->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
*ALV字段目录
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZPPT0003'
      CHANGING
        ct_fieldcat            = gt_alv_fidcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    LOOP AT gt_alv_fidcat ASSIGNING FIELD-SYMBOL(<fs_fidcat>).
      CASE <fs_fidcat>-fieldname.
        WHEN 'MANDT' OR 'ZDELFLG' OR 'TYPE' OR 'ERDAT' OR 'ERZET' OR 'UNAME'  OR 'AENAM' OR 'AEDAT' OR 'AEZEIT'.
          <fs_fidcat>-tech = abap_true.
        WHEN 'QROPERSTS'."下拉框
*          <fs_fidcat>-edit = abap_true.
          <fs_fidcat>-drdn_hndl = '1'.
          <fs_fidcat>-outputlen = 10.
          <fs_fidcat>-drdn_alias = abap_true.
*Field 'checktable' is set to avoid shortdumps that are caused
* by inconsistend data in check tables. You may comment this out
* when the test data of the flight model is consistent in your system.
          <fs_fidcat>-checktable = '!'.        "do not check foreign keys
        WHEN 'UPD'.
          <fs_fidcat>-reptext = '修改标识'.
          <fs_fidcat>-scrtext_s = '修改标识'.
          <fs_fidcat>-scrtext_m = '修改标识'.
          <fs_fidcat>-scrtext_l = '修改标识'.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
    go_zcl_qr_oper->set_drdn_table( )."ALV下拉框列表
  ENDIF.
  CALL METHOD go_alv_grid_create->set_table_for_first_display
    EXPORTING
      i_save                        = 'A'
      is_variant                    = VALUE #( report = sy-repid )
      is_layout                     = gs_layout_bill
    CHANGING
      it_outtab                     = gt_zppt0003_list
      it_fieldcatalog               = gt_alv_fidcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9300 OUTPUT.
  SET PF-STATUS 'STATUS9300'.
  SET TITLEBAR 'MAIN9300'.
  IF go_qrmail_oper IS INITIAL.
    CREATE OBJECT go_qrmail_oper.
  ENDIF.
  go_qrmail_oper->display_alv( ).
ENDMODULE.