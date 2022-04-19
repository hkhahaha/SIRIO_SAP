*&---------------------------------------------------------------------*
*& Report ZPPR0005
*&---------------------------------------------------------------------*
*&流程订单入库申请单
*&20191109

*& S4DK904849	PP：zpp005增加对应新字段 20210610
*&---------------------------------------------------------------------*
REPORT zppr0005 MESSAGE-ID zpp.
INCLUDE zppr0005_top. "全局变量
INCLUDE zppr0005_src."选择屏幕
INCLUDE zppr0005_c04."订单批结信息下发LIMS
INCLUDE zppr0005_c01. "本地类
INCLUDE zppr0005_c02."二维码扫描操作类
INCLUDE zppr0005_c03."邮箱维护操作类
INCLUDE zppr0005_o01. "PBO
INCLUDE zppr0005_i01. "PAI
INCLUDE zppr0005_f01.
AT SELECTION-SCREEN OUTPUT.
  zcl_pro_grreq=>active_selection_criterion( )    ."激活选择屏幕字段

AT SELECTION-SCREEN.

  zcl_pro_grreq=>set_selection_criterion( )."权限检查及选择条件值设置

START-OF-SELECTION.

  PERFORM frm_set_inivalue.

  IF p_rqr = abap_true. "二维码扫描维护
    go_zcl_qr_oper = NEW zcl_qr_oper( ).
    go_zcl_qr_oper->get_qr_data( ).
    CALL SCREEN '9200'.
  ELSE.
    zcl_pro_grreq=>get_data( )."获取报表数据
    zcl_pro_grreq=>show_alv( )."显示ALV
  ENDIF.