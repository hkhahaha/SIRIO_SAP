  method ZII_SI_OA2SAP_SALESORDER2~SI_OA2SAP_SALESORDER2.
*** **** INSERT IMPLEMENTATION HERE **** ***
  DATA:LO_SERVER_CONTEXT  TYPE REF TO IF_WS_SERVER_CONTEXT.
  DATA:G_MESSAGE_PROTOCOL TYPE REF TO IF_WSPROTOCOL_MESSAGE_ID.
  DATA:G_MESSAGE_ID       TYPE SXMSGUID.
  DATA:G_LO_SYS_EXCEPTION TYPE REF TO CX_AI_SYSTEM_FAULT.
  DATA:LV_INTF_ID(6) TYPE C,
        LV_ERR_INFO   TYPE BAPI_MSG.

  DATA:LS_STANDARD    TYPE ZEXCHANGE_FAULT_DATA,
        LS_DETAIL_DATA TYPE ZEXCHANGE_LOG_DATA.
  DATA:LT_RET TYPE TABLE OF ZDT_OA2SAP_SALESORDER_RET,
        LS_RET LIKE LINE OF LT_RET.

  CLEAR:LS_STANDARD,LS_DETAIL_DATA,G_MESSAGE_ID,LV_INTF_ID,LV_ERR_INFO.

*----------------------------------------------------------------------
* 检查接口是否激活
*----------------------------------------------------------------------
  LV_INTF_ID = 'SD001'. "销售订单上传APS->SAP

  CALL FUNCTION 'Z_PI_CK'
  EXPORTING
    INTF_ID  = LV_INTF_ID
  IMPORTING
    ERR_INFO = LV_ERR_INFO.

  IF LV_ERR_INFO IS NOT INITIAL.
    OUTPUT-mt_oa2sap_salesorder_ret2-MESSAGE =  '接口开关没有打开'.
    OUTPUT-mt_oa2sap_salesorder_ret2-TYPE =  'E'.

  ELSE.
*----------------------------------------------------------------------
*   获取接口Message ID
*----------------------------------------------------------------------
    TRY .
      LO_SERVER_CONTEXT   = CL_PROXY_ACCESS=>GET_SERVER_CONTEXT( ).
      G_MESSAGE_PROTOCOL ?= LO_SERVER_CONTEXT->GET_PROTOCOL( IF_WSPROTOCOL=>MESSAGE_ID ).
*       获取Message ID
      G_MESSAGE_ID = G_MESSAGE_PROTOCOL->GET_MESSAGE_ID( ).

    CATCH CX_AI_SYSTEM_FAULT INTO G_LO_SYS_EXCEPTION.
      EXIT.
    ENDTRY.

*----------------------------------------------------------------------
*   日志记录
*----------------------------------------------------------------------
*     DATA(l_guid) = zcl_bc_public=>get_guid( ). "生成本次调用唯一标识
    ZCL_BC_PUBLIC=>WRITE_LOG( IV_LOGID = CONV #( G_MESSAGE_ID )
    IV_INTID = 'SD001' IV_PTYPE = 'I'
    IS_DATA = INPUT ). "记录输入参数

*----------------------------------------------------------------------
*   接口逻辑处理
*----------------------------------------------------------------------
    IF G_MESSAGE_ID IS NOT INITIAL.
      CALL FUNCTION 'Z_SD_SALES_SELECT2'
      EXPORTING
        INPUT      = INPUT
        MESSAGE_ID = G_MESSAGE_ID
      IMPORTING
        OUTPUT     = OUTPUT.
    ENDIF.
  ENDIF.

  "输出参数记录日志
  ZCL_BC_PUBLIC=>WRITE_LOG( IV_LOGID = CONV #( G_MESSAGE_ID )
  IV_INTID = 'SD001' IV_PTYPE = 'E'
  IS_DATA = OUTPUT ). "记录输入参数

  endmethod.