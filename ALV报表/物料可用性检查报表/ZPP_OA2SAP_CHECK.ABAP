FUNCTION ZPP_OA2SAP_CHECK.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ZMT_OA2SAP_MATERIAL_CHECK
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZMT_OA2SAP_MATERIAL_CHECK_RET
*"----------------------------------------------------------------------
  DATA:LT_STB   LIKE  TABLE OF STPOX,
       LT_STB1  LIKE  TABLE OF STPOX,
       LS_STB   LIKE  STPOX,
       LV_MATNR TYPE MARA-MATNR,
       LV_WERKS TYPE MARC-WERKS,
       MESSAGE  TYPE STRING,
       LT_ITEM  TYPE ZDT_OA2SAP_MATERAIL_CHECK_RET1.

  TYPES: BEGIN OF TY_BOM,
           MANTR TYPE MARC-MATNR,
           WERKS TYPE MARC-WERKS,
           BOMS  LIKE LT_STB,
         END OF TY_BOM.
  DATA: LT_BOM TYPE TABLE OF TY_BOM,
        LS_BOM TYPE TY_BOM.
  IF INPUT-MT_OA2SAP_MATERIAL_CHECK-ITEMS IS NOT INITIAL.
*   批量展BOM
    LOOP AT INPUT-MT_OA2SAP_MATERIAL_CHECK-ITEMS INTO DATA(LS_INPUT).
      SELECT * FROM MARC INTO TABLE @DATA(LT_MARC1) WHERE MATNR = @LS_INPUT-MATNR
        AND WERKS = @LS_INPUT-WERKS
        AND BESKZ IN ('F', 'X').
      IF LT_MARC1 IS NOT INITIAL.
        LT_ITEM-RETURNCODE = 'S'.
        LT_ITEM-MATNR = |{ LS_INPUT-MATNR ALPHA = OUT WIDTH = 9 }|.
        LT_ITEM-WERKS = LS_INPUT-WERKS.
        APPEND LT_ITEM TO LT_ITEM_TAB.
        CLEAR LT_MARC1.
        CLEAR LT_ITEM.
        CONTINUE.
      ENDIF.
      LV_MATNR = ZCL_BC_PUBLIC=>CONV_BY_DDIC( I_INPUT = LS_INPUT-MATNR I_ABAP = LV_MATNR ).
      LV_WERKS = ZCL_BC_PUBLIC=>CONV_BY_DDIC( I_INPUT = LS_INPUT-WERKS I_ABAP = LV_WERKS ).

      CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
        EXPORTING
          WERKS                 = LV_WERKS
          MTNRV                 = LV_MATNR
          DATUV                 = SY-DATUM
          CAPID                 = 'PP01'
          MEHRS                 = 'X' "多级展开
          STLAL                 = '01' "可选BOM
          STLAN                 = '1' "BOM用途
          DELNL                 = ABAP_FALSE
        TABLES
          STB                   = LT_STB1
        EXCEPTIONS
          ALT_NOT_FOUND         = 1
          CALL_INVALID          = 2
          MATERIAL_NOT_FOUND    = 3
          MISSING_AUTHORIZATION = 4
          NO_BOM_FOUND          = 5
          NO_PLANT_DATA         = 6
          NO_SUITABLE_BOM_FOUND = 7
          CONVERSION_ERROR      = 8
          OTHERS                = 9.
      IF SY-SUBRC <> 0.
*        S4DK905744-S
        LT_ITEM-RETURNCODE = 'E'.
        LT_ITEM-MATNR = |{ LV_MATNR ALPHA = OUT WIDTH = 9 }|.
        LT_ITEM-WERKS = LV_WERKS.
        LT_ITEM-RETURNINFO = '对此物料' && LT_ITEM-MATNR && '/工厂' && LT_ITEM-WERKS && '/使用没有找到 BOM'.
        APPEND LT_ITEM TO LT_ITEM_TAB.
        CLEAR LT_ITEM.
*        S4DK905744-E
      ELSE.
        LS_BOM-MANTR = LV_MATNR.
        LS_BOM-WERKS = LV_WERKS.
        LS_STB-IDNRK = LV_MATNR.
        LS_STB-WERKS = LV_WERKS.
        LS_STB-MENGE = 1.
        APPEND LS_STB TO LT_STB1.
        CLEAR LS_STB.
        APPEND LINES OF LT_STB1 TO LT_STB.
        LS_BOM-BOMS = LT_STB1.
        APPEND LS_BOM TO LT_BOM.
      ENDIF.

      CLEAR : LV_WERKS,LV_MATNR,LT_STB1,LS_BOM.

    ENDLOOP.
*  ELSE.
*    OUTPUT-MT_OA2SAP_MATERIAL_CHECK_RET-TYPE = 'E'.
*    OUTPUT-MT_OA2SAP_MATERIAL_CHECK_RET-MSG = '未传入物料'.
*    EXIT.
  ENDIF.

  IF LT_STB IS NOT INITIAL.
*    物料
    SELECT * INTO TABLE @DATA(LT_MARA) FROM MARA
      FOR ALL ENTRIES IN @LT_STB
      WHERE MARA~MATNR = @LT_STB-IDNRK.
*   物料工厂
    SELECT * INTO TABLE @DATA(LT_MARC) FROM MARC
    FOR ALL ENTRIES IN @LT_STB
    WHERE MARC~MATNR = @LT_STB-IDNRK
      AND MARC~WERKS = @LT_STB-WERKS.
*   生产版本
    SELECT * INTO TABLE @DATA(LT_MKAL) FROM MKAL
      FOR ALL ENTRIES IN @LT_STB
      WHERE MATNR = @LT_STB-IDNRK
      AND WERKS = @LT_STB-WERKS.
*    主配方
    SELECT * INTO TABLE @DATA(LT_MAPL) FROM MAPL
      FOR ALL ENTRIES IN @LT_STB
      WHERE MATNR = @LT_STB-IDNRK
      AND WERKS = @LT_STB-WERKS
      AND PLNTY = '2'.
*     物料清单
    SELECT * INTO TABLE @DATA(LT_MAST) FROM MAST
      FOR ALL ENTRIES IN @LT_STB
      WHERE MATNR = @LT_STB-IDNRK
      AND WERKS = @LT_STB-WERKS.
    IF LT_MAPL IS NOT INITIAL.
*     任务清单
      SELECT * INTO TABLE @DATA(LT_PLKO1) FROM PLKO
        FOR ALL ENTRIES IN @LT_MAPL
        WHERE PLNTY = @LT_MAPL-PLNTY
        AND PLNNR = @LT_MAPL-PLNNR
        AND PLNAL = @LT_MAPL-PLNAL.
    ENDIF.
    IF LT_MKAL IS NOT INITIAL.

*     任务清单
      SELECT * INTO TABLE @DATA(LT_PLKO) FROM PLKO
        FOR ALL ENTRIES IN @LT_MKAL
        WHERE PLNTY = @LT_MKAL-PLNTY
        AND PLNNR = @LT_MKAL-PLNNR
        AND PLNAL = @LT_MKAL-ALNAL.
    ENDIF.
    IF LT_MAST IS NOT INITIAL.
*     BOM 表头
      SELECT * INTO TABLE @DATA(LT_STKO) FROM STKO
              FOR ALL ENTRIES IN @LT_MAST
              WHERE STLNR = @LT_MAST-STLNR
              AND STLAL = @LT_MAST-STLAL.
    ENDIF.

*   剔除数量小于0
    LOOP AT LT_BOM INTO LS_BOM.
      LT_ITEM-RETURNCODE = 'S'.
      LT_ITEM-MATNR = |{ LS_BOM-MANTR ALPHA = OUT WIDTH = 9 }|.
      LT_ITEM-WERKS = LS_BOM-WERKS.
      LOOP AT LS_BOM-BOMS INTO LS_STB WHERE MENGE > 0.
        READ TABLE LT_MARA INTO DATA(LS_MARA) WITH KEY MATNR = LS_STB-IDNRK.
        READ TABLE LT_MARC INTO DATA(LS_MARC) WITH KEY MATNR = LS_STB-IDNRK.
*        IF LS_MARC-BESKZ = 'E'.
*          CLEAR LS_STB.
*          CONTINUE.
*        ENDIF.

*       10,20物料剔除外购
        IF ( LS_MARA-MATKL+0(2) EQ '10' OR LS_MARA-MATKL+0(2) EQ '20' )
          AND ( LS_MARC-BESKZ = 'F' OR LS_MARC-BESKZ = 'X').
          CLEAR :  LS_STB,LS_MARA,LS_MARC.                  "S4DK905744
          CONTINUE.
        ELSE.
          "剔除99虚拟物料S4DK905875
          IF LS_MARA-MTART EQ 'Z099'.
            CLEAR :  LS_STB,LS_MARA,LS_MARC.
            CONTINUE.
          ENDIF.
*生产版本检查 start
          READ TABLE LT_MKAL INTO DATA(LS_MKAL) WITH KEY MATNR = LS_STB-IDNRK
                                                     WERKS = LS_STB-WERKS.
          IF SY-SUBRC <> 0.
            LT_ITEM-RETURNCODE = 'E'.
            LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO &&  |{ LS_STB-IDNRK ALPHA = OUT WIDTH = 9 }| && '没有生产版本;'.
          ELSE.
            READ TABLE LT_MAST INTO DATA(LS_MAST) WITH KEY  MATNR = LS_STB-IDNRK
                  WERKS = LS_STB-WERKS
                  STLAN = LS_MKAL-STLAN
                  STLAL = LS_MKAL-STLAL.
            IF LS_MAST IS NOT INITIAL.
              READ TABLE LT_STKO INTO DATA(LS_STKO) WITH KEY STLNR = LS_MAST-STLNR
                    STLAL = LS_MAST-STLAL.
              IF LS_STKO-LKENZ = 'X'
              OR LS_STKO-STLST <> '1'.
                LT_ITEM-RETURNCODE = 'E'.
                LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && |{ LS_STB-IDNRK ALPHA = OUT WIDTH = 9 }| && '没有有效生产版本1；'.
              ELSE.
                READ TABLE LT_PLKO INTO DATA(LS_PLKO) WITH KEY PLNTY = LS_MKAL-PLNTY
                      PLNNR = LS_MKAL-PLNNR
                      PLNAL = LS_MKAL-ALNAL.
                IF LS_PLKO-LOEKZ = 'X' OR LS_PLKO-STATU <> '4'.
                  LT_ITEM-RETURNCODE = 'E'.
                  LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && |{ LS_STB-IDNRK ALPHA = OUT WIDTH = 9 }| && '没有有效生产版本2；'.
                ENDIF.
              ENDIF.
            ELSE.
              LT_ITEM-RETURNCODE = 'E'.
              LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && |{ LS_STB-IDNRK ALPHA = OUT WIDTH = 9 }| && '没有有效生产版本3；'.
            ENDIF.
          ENDIF.
*生产版本检查 start

*BOM检查 start
          CLEAR LS_MAST.
          READ TABLE LT_MAST INTO LS_MAST WITH KEY  MATNR = LS_STB-IDNRK
                                                          WERKS = LS_STB-WERKS
                                                          STLAN = '1'
                                                          STLAL = '01'.
          IF SY-SUBRC <> 0.
            LT_ITEM-RETURNCODE = 'E'.
            LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && |{ LS_STB-IDNRK ALPHA = OUT WIDTH = 9 }| && 'BOM不存在；'.
          ELSE.
            CLEAR LS_STKO.
            READ TABLE LT_STKO INTO LS_STKO WITH KEY STLNR = LS_MAST-STLNR
                                                       STLAL = LS_MAST-STLAL.
            IF LS_STKO-LKENZ = 'X'
              OR LS_STKO-STLST <> '01'.
              LT_ITEM-RETURNCODE = 'E'.
              LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && |{ LS_STB-IDNRK ALPHA = OUT WIDTH = 9 }| && 'BOM无效；'.
            ENDIF.
          ENDIF.
*BOM检查 end
*主配方检查 start
          READ TABLE LT_MAPL INTO DATA(LS_MAPL) WITH KEY MATNR = LS_STB-IDNRK
                                                         WERKS = LS_STB-WERKS.
          IF SY-SUBRC <> 0.
            LT_ITEM-RETURNCODE = 'E'.
            LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && |{ LS_STB-IDNRK ALPHA = OUT WIDTH = 9 }| && '不存在主配方；'.
          ELSE.
            READ TABLE LT_PLKO1 INTO DATA(LS_PLKO1) WITH KEY PLNTY = LS_MKAL-PLNTY
                                                            PLNNR = LS_MKAL-PLNNR
                                                            PLNAL = LS_MKAL-ALNAL.
            IF LS_PLKO1-LOEKZ = 'X' OR LS_PLKO1-STATU <> '4'.
              LT_ITEM-RETURNCODE = 'E'.
              LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && |{ LS_STB-IDNRK ALPHA = OUT WIDTH = 9 }| && '没有有效主配方；'.
            ENDIF.
          ENDIF.
          CLEAR LS_MAPL.
*主配方检查 end
*          APPEND LT_ITEM TO LT_ITEM_TAB.
          CLEAR : LS_STB,LS_MARA,LS_MARC.                   "S4DK905744
        ENDIF.
      ENDLOOP.

      LOOP AT LS_BOM-BOMS INTO LS_STB WHERE IDNRK IS NOT INITIAL.
*物料检查 start
        READ TABLE LT_ITEM_TAB INTO LT_ITEM WITH KEY MATNR = LS_STB-IDNRK WERKS = LS_STB-WERKS.
        READ TABLE LT_MARA INTO LS_MARA WITH KEY MATNR = LS_STB-IDNRK.
        READ TABLE LT_MARC INTO LS_MARC WITH KEY MATNR = LS_STB-IDNRK.
*        LT_ITEM-MATNR = LS_STB-IDNRK.S4DK905744
*        LT_ITEM-WERKS = LS_STB-WERKS.S4DK905744
        "剔除99虚拟物料S4DK905875
        IF LS_MARA-MTART EQ 'Z099'.
          CLEAR :  LS_STB,LS_MARA,LS_MARC.
          CONTINUE.
        ENDIF.
        IF LS_MARC-LVORM = 'X'.
          LT_ITEM-RETURNCODE = 'E'.
          LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && |{ LS_STB-IDNRK ALPHA = OUT WIDTH = 9 }| && '已经做删除标识；'.
        ENDIF.
        IF LS_MARC-MMSTA = 'Z1'
          OR LS_MARC-MMSTA = 'Z2'
          OR LS_MARC-MMSTA = 'Z3'
          OR LS_MARC-MMSTA = 'Z4'.
          IF SY-DATUM > LS_MARC-MMSTD.
            LT_ITEM-RETURNCODE = 'E'.
            LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && |{ LS_STB-IDNRK ALPHA = OUT WIDTH = 9 }| && '已经做工厂级冻结；'.
          ELSE.
*            LT_ITEM-RETURNCODE = 'E'. 有效期大于当前日期不提示 S4DK905875
*            LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && |{ LS_STB-IDNRK ALPHA = OUT WIDTH = 9 }| && LS_MARC-MMSTD && '已经做工厂级冻结；'.
          ENDIF.
        ENDIF.

        IF LS_MARA-MSTAE = 'Z1'
          OR LS_MARA-MSTAE = 'Z2'
          OR LS_MARA-MSTAE = 'Z3'
          OR LS_MARA-MSTAE = 'Z4'.
          IF SY-DATUM > LS_MARA-MSTDE.
            LT_ITEM-RETURNCODE = 'E'.
            LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && |{ LS_STB-IDNRK ALPHA = OUT WIDTH = 9 }| && '已经做集团冻结；'.
          ELSE.
*            LT_ITEM-RETURNCODE = 'E'. 有效期大于当前日期不提示 S4DK905875
*            LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && |{ LS_STB-IDNRK ALPHA = OUT WIDTH = 9 }| && LS_MARA-MSTDE && '已经做集团冻结；'.
          ENDIF.
        ENDIF.
*物料检查 end
*       READ TABLE LT_ITEM_TAB INTO LT_ITEM WITH KEY MATNR = LS_STB-IDNRK WERKS = LS_STB-WERKS.
*       IF SY-SUBRC = 0.
*         MODIFY TABLE LT_ITEM_TAB FROM LT_ITEM.
*       ELSE.
*         IF LT_ITEM-RETURNCODE <> 'E'.
*           LT_ITEM-RETURNCODE = 'S'.
*           LT_ITEM-RETURNINFO = |{ LS_STB-IDNRK ALPHA = OUT WIDTH = 9 }| && '检查成功'.
*         ENDIF.
*         APPEND LT_ITEM TO LT_ITEM_TAB.
*       ENDIF.
*        MODIFY LT_ITEM_TAB FROM LT_ITEM.
        CLEAR : LS_MARA,LS_MARC.                            "S4DK905744

      ENDLOOP.

*BOP检查 start

      LS_IN_EXT-WERKS =  LS_BOM-WERKS.
      LS_IN_EXT-MATNR =  LS_BOM-MANTR.
*     递归查询bom输出调用物料清单用于查询BOP
      PERFORM BOM_EXTEND USING LS_IN_EXT.

      APPEND LS_IN_EXT TO LT_EXT.
      PERFORM FRM_SEND_PI CHANGING LT_ITEM.
*    APPEND LINES OF LT_EXT TO LT_IN_EXT.
*BOP检查 end
      IF LT_ITEM-RETURNCODE <> 'E'.
        LT_ITEM-RETURNCODE = 'S'.
        LT_ITEM-RETURNINFO = |{ LS_STB-IDNRK ALPHA = OUT WIDTH = 9 }| && '检查成功'.
      ENDIF.
      APPEND LT_ITEM TO LT_ITEM_TAB.
      CLEAR : LT_ITEM,LT_IN_EXT,LT_EXT,LS_INPUT,LS_IN_EXT.
    ENDLOOP.

  ENDIF.
* S4DK905744-S
  LOOP AT LT_ITEM_TAB INTO LT_ITEM.
    IF LT_ITEM-RETURNCODE EQ 'S'.
      LT_ITEM-RETURNINFO = |{ LT_ITEM-MATNR ALPHA = OUT WIDTH = 9 }| && '检查成功'.
      MODIFY LT_ITEM_TAB FROM LT_ITEM.
*    ELSE.
*      OUTPUT-MT_OA2SAP_MATERIAL_CHECK_RET-TYPE = 'E'.
    ENDIF.
  ENDLOOP.
*  IF OUTPUT-MT_OA2SAP_MATERIAL_CHECK_RET-TYPE = 'E'.
*    OUTPUT-MT_OA2SAP_MATERIAL_CHECK_RET-MSG = '检查失败'.
*  ELSE.
  OUTPUT-MT_OA2SAP_MATERIAL_CHECK_RET-TYPE = 'S'.
  OUTPUT-MT_OA2SAP_MATERIAL_CHECK_RET-MSG = '接口检查成功'.
*  ENDIF.S4DK905744-E

  OUTPUT-MT_OA2SAP_MATERIAL_CHECK_RET-RETURN = LT_ITEM_TAB.


  CLEAR LT_ITEM_TAB.

ENDFUNCTION.

* 递归查询BOM
FORM BOM_EXTEND  USING  TY_EXT TYPE TY_EXT.
  DATA:LS_STB   TYPE STPOX,
       LT_STB   TYPE TABLE OF STPOX,
       LV_MATNR TYPE MARC-MATNR,
       LV_WERKS TYPE MARC-WERKS,
       LT_STBX  TYPE TABLE OF STPOX.
  LV_MATNR = ZCL_BC_PUBLIC=>CONV_BY_DDIC( I_INPUT = TY_EXT-MATNR I_ABAP = LV_MATNR ).
  LV_WERKS = ZCL_BC_PUBLIC=>CONV_BY_DDIC( I_INPUT = TY_EXT-WERKS I_ABAP = LV_WERKS ).
* 展开bom
  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      CAPID                 = 'PP01'   "BOM Application
      DATUV                 = SY-DATUM
      EHNDL                 = '1'
      MTNRV                 = LV_MATNR "MATERIAL
      STLAN                 = '1'
      STLAL                 = '1'
      MKTLS                 = 'X'
      MEHRS                 = '' "   lv_cc "是否多层次展开，X代表多层
      WERKS                 = LV_WERKS  "PLANT
    TABLES
      STB                   = LT_STB
    EXCEPTIONS
      ALT_NOT_FOUND         = 1
      CALL_INVALID          = 2
      MATERIAL_NOT_FOUND    = 3
      MISSING_AUTHORIZATION = 4
      NO_BOM_FOUND          = 5
      NO_PLANT_DATA         = 6
      NO_SUITABLE_BOM_FOUND = 7
      OTHERS                = 8.
* 展开后的物料
  LOOP AT LT_STB INTO LS_STB WHERE MATKL CP '50*' OR MATKL CP '30*'.
    CLEAR LS_EXT.
* 小s不需要再拆分，直接赋值
    IF LS_STB-MATKL CP '30*'.
      LS_EXT-MATNR =  LS_STB-IDNRK. " BOM 组件
      LS_EXT-WERKS =  LS_STB-WERKS. " 工厂
      COLLECT LS_EXT INTO LT_EXT.
* 大M需要再拆分bom
    ELSEIF LS_STB-MATKL CP '50*'.
      LS_EXT-MATNR =  LS_STB-IDNRK.
      LS_EXT-WERKS =  LS_STB-WERKS.
      COLLECT LS_EXT INTO LT_EXT.
* 递归执行拆分bom
      PERFORM BOM_EXTEND USING LS_EXT .

    ENDIF.
  ENDLOOP.

  REFRESH LT_STB.
ENDFORM.


*调用接口查询APS
FORM FRM_SEND_PI CHANGING LT_ITEM TYPE ZDT_OA2SAP_MATERAIL_CHECK_RET1.
  DATA: L_INTID        TYPE ZE_INTF_ID VALUE 'PP064B',
        LR_PROXY       TYPE REF TO ZCO_SI_SAP2APS_CHECK_BOP_OUT, "代理类
        LS_OUTPUT      TYPE ZMT_SAP2APS_CHECK_BOP,
        LS_INPUT       TYPE ZMT_SAP2APS_CHECK_BOP_RET,
        LS_OUTPUT_TAB  TYPE ZDT_SAP2APS_CHECK_BOP_ITEM_TAB,
        LS_OUTPUT_ITEM TYPE ZDT_SAP2APS_CHECK_BOP_ITEMS.
*        LT_ITEM        TYPE ZDT_OA2SAP_MATERAIL_CHECK_RET1.
  LOOP AT LT_EXT INTO LS_EXT.
    LS_OUTPUT_ITEM-MATNR = ZCL_BC_PUBLIC=>CONV_BY_DDIC( I_INPUT = LS_EXT-MATNR I_OUT = 'X' ).
    LS_OUTPUT_ITEM-WERKS = ZCL_BC_PUBLIC=>CONV_BY_DDIC( I_INPUT = LS_EXT-WERKS I_OUT = 'X' ).
    APPEND LS_OUTPUT_ITEM TO LS_OUTPUT_TAB.
  ENDLOOP.
  LS_OUTPUT-MT_SAP2APS_CHECK_BOP-ITEMS = LS_OUTPUT_TAB.

*----------------------------------------------------------------------
*   日志记录-记录输入参数
*----------------------------------------------------------------------
  DATA(L_GUID) = ZCL_BC_PUBLIC=>GET_GUID( )."生成本次调用唯一标识
  DATA(L_FLAG) = ZCL_BC_PUBLIC=>WRITE_LOG( IV_LOGID = L_GUID IV_INTID = L_INTID IV_PTYPE = 'I' IS_DATA = LS_OUTPUT ). "记录输入参数
  IF L_FLAG EQ 'A'."接口是否启用判断
    LOOP AT LT_EXT INTO LS_EXT.
*      LOOP AT LT_ITEM_TAB INTO LT_ITEM WHERE MATNR = LS_EXT-MATNR AND WERKS = LS_EXT-WERKS.
        LT_ITEM-RETURNCODE = 'E'.
        LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && 'ZPP064B接口未启用;'.
*        MODIFY LT_ITEM_TAB FROM LT_ITEM.
*      ENDLOOP.
    ENDLOOP.
    RETURN.
  ENDIF.
*----------------------------------------------------------------------
*   业务逻辑处理
*----------------------------------------------------------------------
  TRY.
      CREATE OBJECT LR_PROXY.

      LR_PROXY->SI_SAP2APS_CHECK_BOP_OUT(  EXPORTING OUTPUT = LS_OUTPUT
                                       IMPORTING INPUT = LS_INPUT )."调用pi接口

      IF LS_INPUT-MT_SAP2APS_CHECK_BOP_RET-TYPE = 'E'.
        LT_ITEM-RETURNCODE = 'E'.
        LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && LS_INPUT-MT_SAP2APS_CHECK_BOP_RET-MSG.
      ELSE.
        LOOP AT LS_INPUT-MT_SAP2APS_CHECK_BOP_RET-RETURN INTO DATA(LS_APS_RETURN).
          IF LS_APS_RETURN-RETURNCODE = 'E'.
*            LOOP AT LT_ITEM_TAB INTO LT_ITEM WHERE MATNR = |{ LS_APS_RETURN-MATNR ALPHA = IN WIDTH = 9 }| AND WERKS = LS_APS_RETURN-WERKS.
              LT_ITEM-RETURNCODE = 'E'.
              LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && LS_APS_RETURN-MATNR && LS_APS_RETURN-RETURNINFO && '；'.
*              MODIFY LT_ITEM_TAB FROM LT_ITEM.
*            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDIF.
    CATCH CX_ROOT INTO DATA(LR_ROOT).
      MESSAGE LR_ROOT->GET_TEXT(  ) TYPE 'I'.
      LOOP AT LT_EXT INTO LS_EXT.
*        LOOP AT LT_ITEM_TAB INTO LT_ITEM WHERE MATNR = LS_EXT-MATNR AND WERKS = LS_EXT-WERKS.
          LT_ITEM-RETURNCODE = 'E'.
          LT_ITEM-RETURNINFO = LT_ITEM-RETURNINFO && 'ZPP064B接口异常;'.
*          MODIFY LT_ITEM_TAB FROM LT_ITEM.
*        ENDLOOP.
      ENDLOOP.
  ENDTRY.
*----------------------------------------------------------------------
*   日志记录-记录返回参数
*----------------------------------------------------------------------
  ZCL_BC_PUBLIC=>WRITE_LOG( IV_LOGID = L_GUID IV_INTID = L_INTID IV_PTYPE = 'E' IS_DATA = LS_INPUT ). "记录输出参数

ENDFORM.