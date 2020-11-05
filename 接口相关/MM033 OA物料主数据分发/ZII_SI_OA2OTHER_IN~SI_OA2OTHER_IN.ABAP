METHOD zii_si_oa2other_in~si_oa2other_in.
  *** **** INSERT IMPLEMENTATION HERE **** ***
      DATA:flag(1).
      DATA:g_message_id TYPE sxmsguid.
      " 接口是否启用判断
      DATA(l_flag) = zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
                                               iv_intid = 'MM033' iv_ptype = 'X'
                                               is_data = input
                                                ).
      IF l_flag EQ 'A'.
        output-mt_oa2other_mdm_ret-zmd001-msg_matnr = input-mt_oa2other_mdm-mrar-matnr.
        output-mt_oa2other_mdm_ret-zmd001-msg_type = 'E'.
        output-mt_oa2other_mdm_ret-zmd001-message = '接口未启用！'.
      ELSE.
        CALL FUNCTION 'Z_MM_MATERIAL_MAINTAIN'
          EXPORTING
            input  = input
          IMPORTING
            output = output.
  *    " 输出参数记录日志
  *      ZCL_BC_PUBLIC=>WRITE_LOG( IV_LOGID = CONV #( G_MESSAGE_ID )
  *                                  IV_INTID = 'MM027' IV_PTYPE = 'Y'
  *                                  IS_DATA = OUTPUT ).
        CALL FUNCTION 'Z_MM_LGORT_CHANG' STARTING NEW TASK 'Z_MM_LGORT_CHANG'
          EXPORTING
            input = output.
      ENDIF.
  
    ENDMETHOD.