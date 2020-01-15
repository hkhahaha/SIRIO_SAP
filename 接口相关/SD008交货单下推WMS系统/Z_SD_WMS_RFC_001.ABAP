FUNCTION z_sd_wms_rfc_001.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_INPUT) TYPE  ZSDS0001
*"  EXPORTING
*"     VALUE(E_OUTPUT) TYPE  ZSDS0001_OUT
*"----------------------------------------------------------------------
  DATA l_intid TYPE ze_intf_id VALUE 'SD008'.
*----------------------------------------------------------------------
*   日志记录-记录输入参数
*----------------------------------------------------------------------
  DATA(l_guid) = zcl_bc_public=>get_guid( )."生成本次调用唯一标识
  DATA(l_flag) = zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = l_intid iv_ptype = 'I' is_data = i_input ). "记录输入参数
  IF l_flag EQ 'A'."接口是否启用判断
    e_output-type = 'E'.
    e_output-message = '接口未启用！'.
    EXIT.
  ENDIF.
*----------------------------------------------------------------------
*   业务逻辑处理
*----------------------------------------------------------------------
  DATA: lr_proxy    TYPE REF TO zco_si_sap2wms_dn_out, "代理类
        ls_output   TYPE zmt_wms_dn,
        ls_input    TYPE zmt_wms_dn_ret.

  TRY.
      CREATE OBJECT lr_proxy.

      CLEAR ls_output.
      APPEND INITIAL LINE TO ls_output-mt_wms_dn-dn ASSIGNING FIELD-SYMBOL(<fs_output>).
      MOVE-CORRESPONDING i_input-header TO <fs_output>.
      <fs_output>-vbeln = zcl_bc_public=>conv_by_ddic( i_input = i_input-header-vbeln i_out = 'X' ).
      <fs_output>-kunag = zcl_bc_public=>conv_by_ddic( i_input = i_input-header-kunag i_out = 'X' ).
*      <fs_output>-zfhrq = zcl_bc_public=>datetime_to_string( iv_date = i_input-header-zfhrq iv_time = '000000' iv_format = 'YYYY-MM-DD HH:MM:SS' ).
*      <fs_output>-erdat = zcl_bc_public=>datetime_to_string( iv_date = i_input-header-erdat iv_time = '000000' iv_format = 'YYYY-MM-DD HH:MM:SS' ).
      LOOP AT i_input-items INTO DATA(ls_item).
        IF ls_item-lgort IS INITIAL.
          e_output-type = 'E'.
          e_output-message = '库存地点不能为空！'.
          RETURN.
        ENDIF.
        APPEND INITIAL LINE TO <fs_output>-item ASSIGNING FIELD-SYMBOL(<fs_item>).
        MOVE-CORRESPONDING ls_item TO <fs_item>.
        <fs_item>-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_item-matnr i_out = 'X' ).
        <fs_item>-vgbel = zcl_bc_public=>conv_by_ddic( i_input = ls_item-vgbel i_out = 'X' ).
      ENDLOOP.

      lr_proxy->si_sap2wms_dn_out( EXPORTING output = ls_output
                                   IMPORTING input = ls_input )."调用pi接口

      IF ls_input-mt_wms_dn_ret-ret-ret_flag EQ '1'.
        e_output-type = 'S'.
      ELSE.
        e_output-type = 'E'.
      ENDIF.
      e_output-message = ls_input-mt_wms_dn_ret-ret-ret_desc.
    CATCH cx_root INTO DATA(lr_root).
      e_output-type = 'E'.
      e_output-message = lr_root->get_text(  ).
  ENDTRY.
*----------------------------------------------------------------------
*   日志记录-记录返回参数
*----------------------------------------------------------------------
  zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = l_intid iv_ptype = 'E' is_data = e_output ). "记录输出参数

ENDFUNCTION.