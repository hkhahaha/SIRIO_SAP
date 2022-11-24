*&---------------------------------------------------------------------*
*& 包含               ZPPR0058_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  GET_TABSTRIP_ACTIVE_TAB  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_tabstrip_active_tab INPUT.
  gv_savecode = gv_okcode ."tabstrip的function code
  "设置激活的TAB
  IF gv_savecode = gc_tab_funs-zfc_pir_gross "
    OR gv_savecode = gc_tab_funs-zfc_pir_raw_gross
    OR gv_savecode = gc_tab_funs-zfc_pir_net
    OR gv_savecode = gc_tab_funs-zfc_pir_raw_net
    OR gv_savecode = gc_tab_funs-zfc_warehouse.
    gs_tab_appl-pressed_tab = gv_savecode.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  PERFORM d0100_user_command.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  gv_savecode = gv_okcode.
  CASE gv_savecode.
    WHEN 'ZBN_PIR_GROSS'."PIR原辅料毛需求计算
      "设置gs_tab_appl-pressed_tab后，程序会重新执行9000主屏幕的动态子屏幕9200
      IF gv_calc_pir_raw_gross_flag IS INITIAL.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING percentage = 50 text = '正在计算...'.
        PERFORM frm_calc_pir_raw_gross."准备数据
        gv_calc_pir_raw_gross_flag = 'X'.
      ENDIF.
      gs_tab_appl-pressed_tab = gc_tab_funs-zfc_pir_raw_gross."激活TAB-PIR原辅料毛需求
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9500 INPUT.
  gv_savecode = gv_okcode.
  "此处不能清除okcode,如果清除会导致在9000屏幕中的MODULE get_tabstrip_active_tab
  "的okcode为空
  "CLEAR gv_okcode.
  CASE gv_savecode.
    WHEN 'ZBN_SAVE_WH'."保存仓储地点维护
      PERFORM frm_save_zppt0011.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9300 INPUT.
  gv_savecode = gv_okcode.
  "此处不能清除okcode,如果清除会导致在9000屏幕中的MODULE get_tabstrip_active_tab
  "的okcode为空
  "CLEAR gv_okcode.
  CASE gv_savecode.
    WHEN 'ZBN_PIR_NET'."计算PIR净需求
      IF gv_calc_pir_net_flag  = ''.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING percentage = 50 text = '正在计算...'.
        PERFORM frm_calc_pir_net.
        gv_calc_pir_net_flag  = 'X'.
        go_salv_table_pir_net->refresh( s_stable = gs_stable ).
        MESSAGE s000 WITH '计算完成'.
      ELSE.
        MESSAGE s000 WITH '已经计算过了,请勿重复操作'.
      ENDIF.
    WHEN 'ZBN_PIR_RAW_NET'."计算PIR原辅包材净需求
      IF gv_calc_pir_net_flag = ''.
        MESSAGE s000 WITH '请先执行第1步骤,计算净需求'.
        RETURN.
      ENDIF.
      IF gv_calc_pir_raw_net_flag IS INITIAL.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING percentage = 50 text = '正在计算...'.
        PERFORM frm_calc_pir_raw_net."准备数据
        gv_calc_pir_raw_net_flag =  'X'.
        gv_switch = '1'."明细视图
      ENDIF.
      "设置gs_tab_appl-pressed_tab后，程序会重新执行9000主屏幕的动态子屏幕9400
      gs_tab_appl-pressed_tab = gc_tab_funs-zfc_pir_raw_net."激活TAB-PIR原辅料净需求
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9400 INPUT.
  gv_savecode = gv_okcode.
  "此处不能清除okcode,如果清除会导致在9000屏幕中的MODULE get_tabstrip_active_tab
  "的okcode为空
  "CLEAR gv_okcode.
  CASE gv_savecode.
    WHEN 'ZBN_SWITCH'."却换显示明细/汇总
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING percentage = 50 text = '正在切换...'.
      IF gt_pir_raw_net_bak IS INITIAL.
        gt_pir_raw_net_bak  = gt_pir_raw_net."备份最原始数据
      ENDIF.
      IF gv_switch  = '1'."明细->汇总
        IF gv_detail_2_sum_flag IS INITIAL.
          PERFORM frm_switch_detail_2_sum.
        ENDIF.
        gt_pir_raw_net = gt_pir_raw_net_sum.
        gv_detail_2_sum_flag = 'X'.
        gv_switch  = '2'."当前视图为汇总视图
      ELSEIF gv_switch = '2'."汇总->明细
        gt_pir_raw_net = gt_pir_raw_net_bak.
        gv_switch  = '1'."当前视图为明细视图
      ENDIF.
      gv_action_by_zbn_switch = 'X'."设置标识用于后续的ALV字段显示或隐藏
    WHEN 'ZBN_SUM_NET'."计算原辅料汇总净需求
      IF gv_switch  = '2'."当前视图为汇总视图
        IF gv_calc_raw_net_flag  = ''.
          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING percentage = 50 text = '正在计算...'.
          PERFORM frm_calc_raw_net.
          gt_pir_raw_net = gt_pir_raw_net_sum.
          gv_calc_raw_net_flag  = 'X'.
          go_salv_table_pir_raw_net->refresh( s_stable = gs_stable ).
          MESSAGE s000 WITH '计算完成'.
        ELSE.
          MESSAGE s000 WITH '已经计算过了,请勿重复操作'.
        ENDIF.
      ELSE.
        MESSAGE s000 WITH '当前为明细表，请先切换为汇总表'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.