FORM frm_set_inivalue.
    *  gs_layout_bill = VALUE lvc_s_layo( cwidth_opt = abap_true sel_mode = 'D' ).
      gs_layout_bill-cwidth_opt = abap_true.
      gs_layout_bill-sel_mode = 'D'.
      gs_stable-row = abap_true.
      gs_stable-col = abap_true.
      gt_vrm_values = VALUE vrm_values( ( key = '101' text = '流程订单收货' )
                                              ( key = '102' text = '流程订单取消收货' )
                                              ( key = 'Y01' text = '流程订单样品收货' )
                                              ( key = 'Y02' text = '流程订单样品取消收货' ) ).
    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Form FRM_CHECK_LGORT
    *&---------------------------------------------------------------------*
    *& 移动类型值改变时，重新检查单据体的仓库
    *&---------------------------------------------------------------------*
    *& -->  p1        text
    *& <--  p2        text
    *&---------------------------------------------------------------------*
    FORM frm_check_lgort .
      DATA:lv_tabix TYPE sy-tabix.
      LOOP AT gt_zppt0003 ASSIGNING <fs_zppt0003>.
        lv_tabix = sy-tabix.
        "移动类型101不能输入样品仓库，Y01只能输入样品仓库.
        CLEAR gv_msg.
        CHECK <fs_zppt0003>-lgort IS NOT INITIAL.
        gv_msg =  go_zcl_pro_grreq_bill->check_lgort_by_bwart( EXPORTING iv_bwart = zspp034_head-bwart iv_lgort = <fs_zppt0003>-lgort ).
        IF gv_msg IS NOT INITIAL.
          gv_msg = |{ '第' }{ lv_tabix }{ '行：' }{ gv_msg }|.
          MESSAGE gv_msg TYPE 'I'.
          CLEAR:<fs_zppt0003>-lgort, <fs_zppt0003>-lgobe.
        ENDIF.
      ENDLOOP.
    ENDFORM.