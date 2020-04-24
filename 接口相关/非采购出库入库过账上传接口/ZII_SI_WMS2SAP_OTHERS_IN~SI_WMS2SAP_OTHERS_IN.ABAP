method ZII_SI_WMS2SAP_OTHERS_IN~SI_WMS2SAP_OTHERS_IN.
    *** **** INSERT IMPLEMENTATION HERE **** ***
        DATA:G_MESSAGE_ID TYPE SXMSGUID.
      " 接口是否启用判断
        DATA(L_FLAG) = ZCL_BC_PUBLIC=>WRITE_LOG( IV_LOGID = CONV #( G_MESSAGE_ID )
                                                 IV_INTID = 'MM027' IV_PTYPE = 'I'
                                                 IS_DATA = INPUT
                                                  ).
        IF L_FLAG EQ 'A'.
          OUTPUT-MT_WMS2SAP_OTHERS_RET-RET_CODE = 'E'.
          OUTPUT-MT_WMS2SAP_OTHERS_RET-RET_MSG = '接口未启用！'.
        ELSE.
          CALL FUNCTION 'Z_MM_UPLOAD_MIGO_INFO'
            EXPORTING
              INPUT  = INPUT
            IMPORTING
              OUTPUT = OUTPUT.
        " 输出参数记录日志
          ZCL_BC_PUBLIC=>WRITE_LOG( IV_LOGID = CONV #( G_MESSAGE_ID )
                                      IV_INTID = 'MM027' IV_PTYPE = 'E'
                                      IS_DATA = OUTPUT ).
        ENDIF.
    
    
    
    *    output-MT_WMS2SAP_OTHERS_RET-WMS_SYS = 'TEST1'.
    *    output-MT_WMS2SAP_OTHERS_RET-WMS_DOCNO = 'TEST1'.
    *    output-MT_WMS2SAP_OTHERS_RET-TYPE = 's'.
    *    output-MT_WMS2SAP_OTHERS_RET-SAP_DOCNO = 'TEST1'.
    *
    *    data lt_data type ZDT_WMS2SAP_OTHERS_RET_LIS_TAB.
    *
    *    data ls_data type ZDT_WMS2SAP_OTHERS_RET_LIST_OF.
    *
    *    ls_data-WMS_ITEMNO = 'TEST1'.
    *    ls_data-SAP_ITEMNO = 'TEST1'.
    *
    *    APPEND ls_data to lt_data.
    *    APPEND ls_data to lt_data.
    *
    *    output-MT_WMS2SAP_OTHERS_RET-LIST_OF_OTHERS_POST_ACK = lt_data.
      endmethod.