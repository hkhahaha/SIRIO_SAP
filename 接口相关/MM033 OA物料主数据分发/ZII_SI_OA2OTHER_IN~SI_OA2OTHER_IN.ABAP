method ZII_SI_OA2OTHER_IN~SI_OA2OTHER_IN.
    *** **** INSERT IMPLEMENTATION HERE **** ***
    
        DATA:G_MESSAGE_ID TYPE SXMSGUID.
      " 接口是否启用判断
        DATA(L_FLAG) = ZCL_BC_PUBLIC=>WRITE_LOG( IV_LOGID = CONV #( G_MESSAGE_ID )
                                                 IV_INTID = 'MM033' IV_PTYPE = 'X'
                                                 IS_DATA = INPUT
                                                  ).
        IF L_FLAG EQ 'A'.
          OUTPUT-MT_OA2OTHER_MDM_RET-ZMD001-MSG_MATNR = INPUT-MT_OA2OTHER_MDM-MRAR-MATNR.
          OUTPUT-MT_OA2OTHER_MDM_RET-ZMD001-MSG_TYPE = 'E'.
          OUTPUT-MT_OA2OTHER_MDM_RET-ZMD001-MESSAGE = '接口未启用！'.
        ELSE.
          CALL FUNCTION 'Z_MM_MATERIAL_MAINTAIN'
            EXPORTING
              INPUT  = INPUT
            IMPORTING
              OUTPUT = OUTPUT.
    *    " 输出参数记录日志
    *      ZCL_BC_PUBLIC=>WRITE_LOG( IV_LOGID = CONV #( G_MESSAGE_ID )
    *                                  IV_INTID = 'MM027' IV_PTYPE = 'Y'
    *                                  IS_DATA = OUTPUT ).
        ENDIF.
    
      endmethod.