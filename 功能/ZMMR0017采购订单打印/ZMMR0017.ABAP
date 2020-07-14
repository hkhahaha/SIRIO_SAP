*&---------------------------------------------------------------------*
*& Report ZMMR0017
*&---------------------------------------------------------------------*
*& Tcode    ：
*& 功能     ：采购订单打印（订单&合同）
*&---------------------------------------------------------------------*
*& Author:  ZhangHB
*& Date:    2019-09-20
*&---------------------------------------------------------------------*
*& Modification Log:
*& Version   Date        Author       DESCRIPTION     CHANGE REQUEST
*& -------- ----------  -----------  -------------  -----------------
*&---------------------------------------------------------------------*
REPORT zmmr0017.


INCLUDE: zmmr0017_top,
         zmmr0017_scr,
         zmmr0017_f01.

*&---------------------------------------------------------------------*
* INITIALIZATION
*&---------------------------------------------------------------------*
* INITIALIZATION.
*  PERFORM frm_init_data.



*&---------------------------------------------------------------------*
* START-OF-SELECTION
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_ebeln.
  PERFORM frm_f4_resnum.

START-OF-SELECTION.

  DATA: lv_message(100).

  PERFORM frm_auth_check.
  PERFORM frm_get_data TABLES gt_detail USING s_ebeln p_werks CHANGING gs_head lv_message.
  IF lv_message IS NOT INITIAL.
    MESSAGE lv_message TYPE 'E'.
  ENDIF.


  "调用smartform进行打印显示

  PERFORM frm_print TABLES gt_detail
                        USING gs_head
                              'ZMM017_PC'.

*  IF s_pro = 'X'.
*    PERFORM frm_print TABLES gt_detail
*                          USING gs_head
*                                'ZMM017_PC'.
*  ELSE.
*    PERFORM frm_print TABLES gt_detail
*                        USING gs_head
*                              'ZMM017_PO'.
*  ENDIF.

*   PERFORM frm_display_data.
*  PERFORM frm_print_data.