FUNCTION Z_MM_MATERIAL_MAINTAIN.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ZMT_OA2OTHER_MDM
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZMT_OA2OTHER_MDM_RET
*"----------------------------------------------------------------------
  DEFINE MATN1_INPUT.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT         = &1
      IMPORTING
        OUTPUT        = &1
      EXCEPTIONS
        LENGTH_ERROR  = 1
        OTHERS        = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
  END-OF-DEFINITION.
  DATA: LW_MRAR   TYPE ZDT_OA2SAP_MDM_MARA,
        LW_RETURN TYPE ZDT_OA2SAP_MDM_RET_ZMD001,
        LV_MATNR  TYPE MATNR,
        LV_PLANT  TYPE WERKS_D,
        LV_SALES  TYPE VKORG,
        LV_DISTR  TYPE VTWEG,
        LV_GUID   TYPE SYSUUID_X16,
        LV_SUBRC  TYPE C,
        LT_LOG    TYPE STANDARD TABLE OF ZMMOA_MM033_LOG,
        LS_LOG    TYPE ZMMOA_MM033_LOG.

  CLEAR: LW_MRAR, LW_RETURN.

  LW_MRAR = INPUT-MT_OA2OTHER_MDM-MRAR.

*  " 检查物料是否存在
*  IF LW_MRAR-MATNR IS NOT INITIAL.
*    CLEAR: LV_MATNR, LV_SUBRC.
*    LV_MATNR = LW_MRAR-MATNR.
*    MATN1_INPUT: LV_MATNR.
*    PERFORM FRM_CHECK_EXIST USING LV_MATNR '' '' '' CHANGING LV_SUBRC.
*    IF LV_SUBRC IS NOT INITIAL.
*      LW_RETURN-MSG_MATNR = LW_MRAR-MATNR.
*      LW_RETURN-MSG_TYPE = 'E'.
*      LW_RETURN-MESSAGE = '物料号不存在'.
*    ELSE.
*      " 物料号存在，修改物料
*      PERFORM FRM_CHANGE_MATNR USING LW_MRAR CHANGING LW_RETURN.
*    ENDIF.
*  ELSE.
*    " 物料号不存在，新建物料
*    PERFORM FRM_CREATE_MATNR USING LW_MRAR CHANGING LW_RETURN.
*  ENDIF.
  PERFORM FRM_CHANGE_MATNR USING LW_MRAR CHANGING LW_RETURN.

  OUTPUT-MT_OA2OTHER_MDM_RET-ZMD001 = LW_RETURN.
  " 日志表
  CLEAR: LV_GUID, LT_LOG, LS_LOG.
  TRY .

      LV_GUID = CL_SYSTEM_UUID=>CREATE_UUID_X16_STATIC( ).

    CATCH CX_UUID_ERROR.

*      EXCEPTION HANDLE

  ENDTRY.

  LS_LOG-MANDT = SY-MANDT.
  LS_LOG-GUID = LV_GUID.
  LS_LOG-SRC_TICKETID = INPUT-MT_OA2OTHER_MDM-SRC_TICKETID.
  LS_LOG-TIMESTAMP = INPUT-MT_OA2OTHER_MDM-SEND_TIME.
  LS_LOG-MATNR = LW_RETURN-MSG_MATNR.
  LS_LOG-MESSAGE = LW_RETURN-MESSAGE.
  APPEND LS_LOG TO LT_LOG.
  MODIFY ZMMOA_MM033_LOG FROM TABLE LT_LOG.
ENDFUNCTION.