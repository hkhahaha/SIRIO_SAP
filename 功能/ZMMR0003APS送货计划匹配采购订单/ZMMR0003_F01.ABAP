*&---------------------------------------------------------------------*
*& 包含               ZMMR0003_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form FRM_INIT
*&---------------------------------------------------------------------*
*& 程序初始化
*&---------------------------------------------------------------------*
FORM FRM_INIT .
    *  DELETE FROM zmmaps_temp.
    *  DELETE FROM zmmwms_poh.
    *  DELETE FROM zmmwms_pol.
    *  DELETE FROM zmmaps_pl.
      CONCATENATE ICON_XXL TEXT-I01 INTO SSCRFIELDS-FUNCTXT_01.
    
      CALL FUNCTION 'Z_MM_APS_CLOSE_PLAN'.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_SCREEN_CHECK
    *&---------------------------------------------------------------------*
    *& 屏幕检查
    *&---------------------------------------------------------------------*
    FORM FRM_SCREEN_CHECK .
    
      CASE 'X'.
        WHEN P_R05.
          IF P_FILE IS INITIAL.
            PERFORM FRM_STOP_PROGRAM  USING TEXT-E04 .
          ENDIF.
        WHEN P_R06.
    
        WHEN OTHERS.
          IF S_WERKS[] IS INITIAL.
            PERFORM FRM_STOP_PROGRAM  USING TEXT-E02 .
          ENDIF.
          IF S_DATE[] IS INITIAL.
            PERFORM FRM_STOP_PROGRAM  USING TEXT-E03 .
          ENDIF.
      ENDCASE.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_STOP_PROGRAM
    *&---------------------------------------------------------------------*
    *& 显示消息并退出程序
    *&---------------------------------------------------------------------*
    *&      --> UV_MESSAGE
    *&---------------------------------------------------------------------*
    FORM FRM_STOP_PROGRAM  USING UV_MESSAGE.
      MESSAGE UV_MESSAGE TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_AUTHORITY_CHECK
    *&---------------------------------------------------------------------*
    *& 权限检查
    *&---------------------------------------------------------------------*
    FORM FRM_AUTHORITY_CHECK .
    
      CASE 'X'.
        WHEN P_R02.
          PERFORM FRM_CHECK_M_BEST_WRK TABLES S_WERKS.
          PERFORM FRM_CHECK_M_BEST_EKG TABLES S_EKGRP.
        WHEN P_R05.
    
      ENDCASE.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_DATA
    *&---------------------------------------------------------------------*
    *& 获取数据
    *&---------------------------------------------------------------------*
    FORM FRM_GET_DATA .
    
      CASE 'X'.
        WHEN P_R05."上传供应商送货计划
          PERFORM FRM_GET_EXCEL_DATA.    "获取EXCEL数据
        WHEN P_R06."供应商送货计划维护
          PERFORM FRM_GET_TEMP_OK.       "获取供应商送货计划
        WHEN OTHERS.
          PERFORM FRM_GET_MAIN_DATA.     "获取要排计划的数据
          PERFORM FRM_PROCESS_DATA.    "处理数据
          PERFORM FRM_LOCK_DATA.       "锁定数据
      ENDCASE.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_MAIN_DATA
    *&---------------------------------------------------------------------*
    *& 获取要排计划的数据
    *&---------------------------------------------------------------------*
    FORM FRM_GET_MAIN_DATA .
      CASE 'X'.
    *    WHEN p_r01.
    *      gv_screen = '100'.
        WHEN P_R02.
          GV_SCREEN = '10'.
      ENDCASE.
    
    *获取抬头数据
      PERFORM FRM_GET_HEAD_DATA.
    *获取采购订单数据
      PERFORM FRM_GET_PO_DATA.
    *获取临时表数据
      PERFORM FRM_GET_TEMP_DATA.
    *获取已经确认交期的数据
      PERFORM FRM_GET_PDLV_DATA.
    **设置KEY字段
      PERFORM FRM_SET_KEY.
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_PROCESS_DATA
    *&---------------------------------------------------------------------*
    *& 处理数据
    *&---------------------------------------------------------------------*
    FORM FRM_PROCESS_DATA .
    
      DATA: LV_DUOJIAN_QTY     LIKE EKPO-MENGE,    "实际收货数量 - 计划收货数量
            LV_PDLV_MENGE_INIT TYPE ZMMWMS_POL-PDLV_MENGE, "期初导入的计划送货数量--ljm 2019-11-28
            LV_POARRIVE_MENGE  TYPE  ZMMWMS_POL-PDLV_MENGE, "PO收货数量
            LV_ZYFWF           TYPE  ZMMWMS_POL-PDLV_MENGE, "订单已分配未发货
            LV_MENGE           TYPE  ZMMWMS_POL-PDLV_MENGE, "PO收货数量或确认预期交货量
            LT_PLANNO_ITEM     LIKE TABLE OF GS_PLANNO_ITEM,
            LS_PLANNO_ITEM     LIKE  GS_PLANNO_ITEM.
      FIELD-SYMBOLS: <FV_APS_PL>  LIKE GS_APS_PL,
                     <FV_PO_DATA> LIKE GS_PO_DATA.
    
      CHECK P_R02 = 'X'.
      PERFORM FRM_GET_MAKT.   "物料描述
      PERFORM FRM_GET_T006A.  "计量单位文本
      PERFORM FRM_GET_LFA1.   "供应商名称
    
      PERFORM FRM_GET_DOMAIN_TXT USING 'ZZASP_STS'.  "DOMAIN值对应文本
    
      SORT GT_MAKT  BY MATNR.
      SORT GT_T006A BY MSEHI.
      SORT GT_LFA1  BY LIFNR.
      SORT GT_DD07V BY DOMNAME DOMVALUE_L.
    
      LOOP AT GT_APS_PL ASSIGNING <FV_APS_PL>.
    
    *物料描述
        READ TABLE GT_MAKT INTO GS_MAKT WITH KEY MATNR = <FV_APS_PL>-KEY-MATNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          <FV_APS_PL>-MAKTX = GS_MAKT-MAKTX.
        ENDIF.
    
    *采购组
        SELECT SINGLE EKGRP INTO <FV_APS_PL>-EKGRP FROM MARC WHERE MATNR = <FV_APS_PL>-MATNR AND WERKS = <FV_APS_PL>-WERKS.
    
    *大小量纲
        SELECT SINGLE GROES MEINS MFRNR
          INTO (<FV_APS_PL>-GROES, <FV_APS_PL>-MEINS, <FV_APS_PL>-MFRNR)
          FROM MARA
         WHERE MATNR = <FV_APS_PL>-MATNR.
    
    *计量单位文本
        READ TABLE GT_T006A INTO GS_T006A WITH KEY MSEHI = <FV_APS_PL>-MEINS BINARY SEARCH.
        IF SY-SUBRC = 0.
          <FV_APS_PL>-MSEHT = GS_T006A-MSEHT.
        ENDIF.
    
    *供应商名称
        READ TABLE GT_LFA1 INTO GS_LFA1 WITH KEY LIFNR = <FV_APS_PL>-MFRNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          <FV_APS_PL>-PRODUCER_NAME = GS_LFA1-NAME1.
        ENDIF.
    
    *DOMAIN值对应文本
        READ TABLE GT_DD07V INTO GS_DD07V WITH KEY DOMNAME = 'ZZASP_STS' DOMVALUE_L = <FV_APS_PL>-ZPR_STS BINARY SEARCH.
        IF SY-SUBRC = 0.
          <FV_APS_PL>-ZPR_STSTXT = GS_DD07V-DDTEXT.
        ENDIF.
    
    *物料长描述
        DATA(LV_MAKTXL) = ZCL_CM_PUBLIC=>GET_LONG_TXT( ID = 'GRUN' NAME = <FV_APS_PL>-MATNR OBJECT = 'MATERIAL' ).
        IF LV_MAKTXL IS NOT INITIAL.
          <FV_APS_PL>-MAKTXL = LV_MAKTXL.
        ENDIF.
    
    ***获取已经收货数量
    *    PERFORM FRM_GET_RECEIVED_QUAN CHANGING <FV_APS_PL>."DEL BY LANJINGCUN 未分配数量不需要计算已收货数量
    
    *获取已经分配数量
        PERFORM FRM_GET_ASSIGNED_MENGE CHANGING <FV_APS_PL>.
    
        "UP DATA BY LANJINGCUN 20200420  更新剩余需要分配的数量(R2)-start
    *剩余需要分配的数量
    *    <FV_APS_PL>-SY_MENGE = <FV_APS_PL>-APS_MENGE - <FV_APS_PL>-PDLV_MENGE - <FV_APS_PL>-ASSIGNED_MENGE.
        "调整后的逻辑 temp 匹配ZMATDOC获取已收数量，该数量不会大于temp分配数量 因此剩余需要分配数量 = APS数量 - temp分配数量
        <FV_APS_PL>-SY_MENGE = <FV_APS_PL>-APS_MENGE  - <FV_APS_PL>-ASSIGNED_MENGE.
        "UP DATA BY LANJINGCUN 20200420  更新剩余需要分配的数量(R2)-END
    
      ENDLOOP.
    
      IF P_PC IS NOT INITIAL.
        DELETE GT_APS_PL WHERE SY_MENGE <= 0.
        IF GT_APS_PL[] IS INITIAL.
          PERFORM FRM_STOP_PROGRAM  USING TEXT-E01.
        ENDIF.
      ENDIF.
    
    
      "UP DATA BY LANJINGCUN 20200420  更新检查PO匹配数量 后续作为锁定用(R2)-start
    *  LOOP AT GT_PO_DATA ASSIGNING <FV_PO_DATA>.
    *
    **    <fv_po_data>-match_menge = <fv_po_data>-left_quan.
    *
    **获取单位数量转换
    *    PERFORM FRM_GET_UNIT_CONVERT USING <FV_PO_DATA>-MATNR
    *                                       <FV_PO_DATA>-MEINS
    *                                       <FV_PO_DATA>-LMEIN
    *                                       <FV_PO_DATA>-MENGE
    *                              CHANGING <FV_PO_DATA>-BASE_MENGE.
    *
    ***KEY字段赋值
    **    <fv_po_data>-key-werks = <fv_po_data>-werks.
    **    <fv_po_data>-key-lgort = <fv_po_data>-lgort.
    **    <fv_po_data>-key-matnr = <fv_po_data>-matnr.
    **    <fv_po_data>-key-st_code = <fv_po_data>-st_code.
    **
    **    MOVE-CORRESPONDING <fv_po_data>-key TO <fv_po_data>-key2.
    *
    **已收数量
    *    READ TABLE GT_EKBE INTO GS_EKBE WITH KEY EBELN = <FV_PO_DATA>-EBELN
    *                                             EBELP = <FV_PO_DATA>-EBELP
    *                                             BINARY SEARCH.
    *    IF SY-SUBRC = 0.
    *      PERFORM FRM_GET_UNIT_CONVERT USING <FV_PO_DATA>-MATNR
    *                                         <FV_PO_DATA>-MEINS
    *                                         <FV_PO_DATA>-LMEIN
    *                                         GS_EKBE-MENGE
    *                                CHANGING <FV_PO_DATA>-ARRIVE_MENGE.
    *    ENDIF.
    *
    **供应商名称
    *    SELECT SINGLE NAME1 INTO <FV_PO_DATA>-NAME1 FROM LFA1 WHERE LIFNR = <FV_PO_DATA>-LIFNR.
    *
    **制造商号
    *    IF <FV_PO_DATA>-EMATN <> <FV_PO_DATA>-MATNR.
    *      SELECT SINGLE MFRNR INTO <FV_PO_DATA>-MFRNR FROM MARA WHERE MATNR = <FV_PO_DATA>-EMATN.
    *    ELSE.
    *      SELECT SINGLE MFRNR INTO <FV_PO_DATA>-MFRNR FROM MARA WHERE MATNR = <FV_PO_DATA>-MATNR.
    *    ENDIF.
    *
    **制造商名称
    *    IF <FV_PO_DATA>-MFRNR IS NOT INITIAL.
    *      SELECT SINGLE NAME1 INTO <FV_PO_DATA>-PRODUCER_NAME FROM LFA1 WHERE LIFNR = <FV_PO_DATA>-MFRNR.
    *    ENDIF.
    *
    *    IF <FV_PO_DATA>-MENGE <> 0.
    *      <FV_PO_DATA>-PRICE = <FV_PO_DATA>-KZWI1 / <FV_PO_DATA>-MENGE.
    *    ENDIF.
    *
    **匹配总量 / 确认预期交货量
    *    LOOP AT GT_MMAPS_TEMP INTO GS_MMAPS_TEMP WHERE EBELN = <FV_PO_DATA>-EBELN
    *                                                AND EBELP = <FV_PO_DATA>-EBELP.
    *      IF GS_MMAPS_TEMP-PDLV_DATE IS INITIAL.
    *        <FV_PO_DATA>-ASSIGNED_MENGE = <FV_PO_DATA>-ASSIGNED_MENGE + GS_MMAPS_TEMP-ASSIGN_MENGE.   "匹配总量
    *      ELSE.
    *        <FV_PO_DATA>-CONFIRM_MENGE = <FV_PO_DATA>-CONFIRM_MENGE + GS_MMAPS_TEMP-ASSIGN_MENGE.     "确认预期交货量
    *      ENDIF.
    *
    *    ENDLOOP.
    *
    **把多减的数量加回来
    *    CLEAR:LV_PDLV_MENGE_INIT, LV_DUOJIAN_QTY,LT_PLANNO_ITEM,LV_POARRIVE_MENGE.
    *    LOOP AT GT_PDLV_DATA INTO GS_PDLV_DATA WHERE EBELN = <FV_PO_DATA>-EBELN AND EBELP = <FV_PO_DATA>-EBELP.
    *      LV_POARRIVE_MENGE = GS_PDLV_DATA-PORECEIVED_QUAN.
    *      IF GS_PDLV_DATA-RECEIVED_QUAN >= GS_PDLV_DATA-PDLV_MENGE.
    *        LV_DUOJIAN_QTY = LV_DUOJIAN_QTY + GS_PDLV_DATA-PDLV_MENGE.
    *      ELSE.
    *        LV_DUOJIAN_QTY = LV_DUOJIAN_QTY + GS_PDLV_DATA-RECEIVED_QUAN.
    *      ENDIF.
    *      "期初导入到ZMMWMS_POL表中的计划送货数量 by ljm 2019-11-28
    *      IF GS_PDLV_DATA-ORIGIN = 'M'."期初导入标识
    *        LT_PLANNO_ITEM = VALUE #( BASE LT_PLANNO_ITEM ( DLV_PLANNO = GS_PDLV_DATA-DLV_PLANNO
    *                                                        DLV_ITEMNO = GS_PDLV_DATA-DLV_ITEMNO
    *                                                        PDLV_MENGE = GS_PDLV_DATA-PDLV_MENGE ) ).
    **        lv_pdlv_menge_init = lv_pdlv_menge_init + gs_pdlv_data-pdlv_menge. "采购订单行号的计划送货数量合计
    *      ENDIF.
    *    ENDLOOP.
    *    SORT LT_PLANNO_ITEM BY DLV_PLANNO DLV_ITEMNO.
    *    DELETE ADJACENT DUPLICATES FROM LT_PLANNO_ITEM COMPARING DLV_PLANNO DLV_ITEMNO.
    *    SELECT PDLV~DLV_PLANNO,
    *           PDLV~DLV_ITEMNO,
    *           PDLV~PDLV_MENGE,
    *           SUM( CASE M~SHKZG WHEN 'S' THEN M~MENGE ELSE 0 END ) AS MENGE_S,
    *           SUM( CASE M~SHKZG WHEN 'H' THEN M~MENGE ELSE 0 END ) AS MENGE_H
    *      FROM @LT_PLANNO_ITEM AS PDLV
    *      LEFT JOIN ZMATDOC AS M ON M~DLV_PLANNO = PDLV~DLV_PLANNO AND M~DLV_ITEMNO = PDLV~DLV_ITEMNO
    *       GROUP BY PDLV~DLV_PLANNO,PDLV~DLV_ITEMNO,PDLV~PDLV_MENGE
    *       ORDER BY PDLV~DLV_PLANNO,PDLV~DLV_ITEMNO
    *      INTO TABLE @DATA(LT_PLANNO_MENGE).
    *    "计算期初导入的送货计划行号未收货数量
    *    LOOP AT LT_PLANNO_MENGE INTO DATA(LS_PLANNO_MENGE).
    *      LV_PDLV_MENGE_INIT = LV_PDLV_MENGE_INIT + LS_PLANNO_MENGE-PDLV_MENGE - ( LS_PLANNO_MENGE-MENGE_S - LS_PLANNO_MENGE-MENGE_H ).
    *    ENDLOOP.
    *    IF LV_PDLV_MENGE_INIT < 0.
    *      LV_PDLV_MENGE_INIT = 0.
    *    ENDIF.
    *    IF <FV_PO_DATA>-CONFIRM_MENGE < 0.
    *      <FV_PO_DATA>-CONFIRM_MENGE = 0.
    *    ENDIF.
    *
    **剩余可用数量 = 采购订单数量 - ( PO已收数量 或确认预期交货量) - 未确认匹配量 -（ 期初计划送货数量 by ljm 2019-11-28)
    *    "PO已收数量与确认预期交货量哪个数量大就减谁
    *    CLEAR LV_MENGE.
    **    IF lv_poarrive_menge >= <fv_po_data>-confirm_menge.
    **      lv_menge = lv_poarrive_menge.
    **    ELSE.
    **      lv_menge = <fv_po_data>-confirm_menge.
    **    ENDIF.
    *    <FV_PO_DATA>-LEFT_QUAN = <FV_PO_DATA>-BASE_MENGE  - LV_POARRIVE_MENGE
    *                                                     " + lv_duojian_qty  "注释 by ljm 2019-11-28
    *                                                      - <FV_PO_DATA>-CONFIRM_MENGE
    *                                                      - <FV_PO_DATA>-ASSIGNED_MENGE
    *                                                      - LV_PDLV_MENGE_INIT. "（ 期初计划送货数量 by ljm 2019-11-28)
    *    IF <FV_PO_DATA>-CONFIRM_MENGE > 0 OR <FV_PO_DATA>-ASSIGNED_MENGE > 0.
    *      <FV_PO_DATA>-SORT = '1'.
    *    ELSE.
    *      <FV_PO_DATA>-SORT = '2'.
    *    ENDIF.
    *  ENDLOOP.
    
      SORT GT_EKBE BY EBELN  EBELP ."add by lanjingcun 20210213
      LOOP AT GT_PO_DATA ASSIGNING <FV_PO_DATA>.
    
    *    <fv_po_data>-match_menge = <fv_po_data>-left_quan.
    
    *获取单位数量转换
        PERFORM FRM_GET_UNIT_CONVERT USING <FV_PO_DATA>-MATNR
                                           <FV_PO_DATA>-MEINS
                                           <FV_PO_DATA>-LMEIN
                                           <FV_PO_DATA>-MENGE
                                  CHANGING <FV_PO_DATA>-BASE_MENGE.
    
    *已收数量
        READ TABLE GT_EKBE INTO GS_EKBE WITH KEY EBELN = <FV_PO_DATA>-EBELN
                                                 EBELP = <FV_PO_DATA>-EBELP
                                                 BINARY SEARCH.
        IF SY-SUBRC = 0.
          PERFORM FRM_GET_UNIT_CONVERT USING <FV_PO_DATA>-MATNR
                                             <FV_PO_DATA>-MEINS
                                             <FV_PO_DATA>-LMEIN
                                             GS_EKBE-MENGE
                                    CHANGING <FV_PO_DATA>-ARRIVE_MENGE.
        ENDIF.
    
    *供应商名称
        SELECT SINGLE NAME1 INTO <FV_PO_DATA>-NAME1 FROM LFA1 WHERE LIFNR = <FV_PO_DATA>-LIFNR.
    
    *制造商号
        IF <FV_PO_DATA>-EMATN <> <FV_PO_DATA>-MATNR.
          SELECT SINGLE MFRNR INTO <FV_PO_DATA>-MFRNR FROM MARA WHERE MATNR = <FV_PO_DATA>-EMATN.
        ELSE.
          SELECT SINGLE MFRNR INTO <FV_PO_DATA>-MFRNR FROM MARA WHERE MATNR = <FV_PO_DATA>-MATNR.
        ENDIF.
    
    *制造商名称
        IF <FV_PO_DATA>-MFRNR IS NOT INITIAL.
          SELECT SINGLE NAME1 INTO <FV_PO_DATA>-PRODUCER_NAME FROM LFA1 WHERE LIFNR = <FV_PO_DATA>-MFRNR.
        ENDIF.
    
        IF <FV_PO_DATA>-MENGE <> 0.
          <FV_PO_DATA>-PRICE = <FV_PO_DATA>-KZWI1 / <FV_PO_DATA>-MENGE.
        ENDIF.
    
    *匹配总量 / 确认预期交货量
        LOOP AT GT_MMAPS_TEMP INTO GS_MMAPS_TEMP WHERE EBELN = <FV_PO_DATA>-EBELN
                                                    AND EBELP = <FV_PO_DATA>-EBELP.
          IF GS_MMAPS_TEMP-PDLV_DATE IS INITIAL.
            <FV_PO_DATA>-ASSIGNED_MENGE = <FV_PO_DATA>-ASSIGNED_MENGE + GS_MMAPS_TEMP-ASSIGN_MENGE.   "匹配总量
          ELSE.
            <FV_PO_DATA>-CONFIRM_MENGE = <FV_PO_DATA>-CONFIRM_MENGE + GS_MMAPS_TEMP-ASSIGN_MENGE.     "确认预期交货量
          ENDIF.
    
        ENDLOOP.
    
    *获取期初
        CLEAR:LV_PDLV_MENGE_INIT, LV_DUOJIAN_QTY,LT_PLANNO_ITEM,LV_POARRIVE_MENGE.
        LOOP AT GT_PDLV_DATA_M INTO GS_PDLV_DATA WHERE EBELN = <FV_PO_DATA>-EBELN AND EBELP = <FV_PO_DATA>-EBELP.
    *      LV_POARRIVE_MENGE = GS_PDLV_DATA-PORECEIVED_QUAN.
    *      IF GS_PDLV_DATA-RECEIVED_QUAN >= GS_PDLV_DATA-PDLV_MENGE.
    *        LV_DUOJIAN_QTY = LV_DUOJIAN_QTY + GS_PDLV_DATA-PDLV_MENGE.
    *      ELSE.
    *        LV_DUOJIAN_QTY = LV_DUOJIAN_QTY + GS_PDLV_DATA-RECEIVED_QUAN.
    *      ENDIF.
          "期初导入到ZMMWMS_POL表中的计划送货数量 by ljm 2019-11-28
          IF GS_PDLV_DATA-ORIGIN = 'M'."期初导入标识
            LT_PLANNO_ITEM = VALUE #( BASE LT_PLANNO_ITEM ( DLV_PLANNO = GS_PDLV_DATA-DLV_PLANNO
                                                            DLV_ITEMNO = GS_PDLV_DATA-DLV_ITEMNO
                                                            PDLV_MENGE = GS_PDLV_DATA-PDLV_MENGE ) ).
    *        lv_pdlv_menge_init = lv_pdlv_menge_init + gs_pdlv_data-pdlv_menge. "采购订单行号的计划送货数量合计
          ENDIF.
        ENDLOOP.
        SORT LT_PLANNO_ITEM BY DLV_PLANNO DLV_ITEMNO.
        DELETE ADJACENT DUPLICATES FROM LT_PLANNO_ITEM COMPARING DLV_PLANNO DLV_ITEMNO.
        SELECT PDLV~DLV_PLANNO,
               PDLV~DLV_ITEMNO,
               PDLV~PDLV_MENGE,
               SUM( CASE M~SHKZG WHEN 'S' THEN M~MENGE ELSE 0 END ) AS MENGE_S,
               SUM( CASE M~SHKZG WHEN 'H' THEN M~MENGE ELSE 0 END ) AS MENGE_H
          FROM @LT_PLANNO_ITEM AS PDLV
          LEFT JOIN ZMATDOC AS M ON M~DLV_PLANNO = PDLV~DLV_PLANNO AND M~DLV_ITEMNO = PDLV~DLV_ITEMNO
           GROUP BY PDLV~DLV_PLANNO,PDLV~DLV_ITEMNO,PDLV~PDLV_MENGE
           ORDER BY PDLV~DLV_PLANNO,PDLV~DLV_ITEMNO
          INTO TABLE @DATA(LT_PLANNO_MENGE).
        "计算期初导入的送货计划行号未收货数量
        LOOP AT LT_PLANNO_MENGE INTO DATA(LS_PLANNO_MENGE).
          LV_PDLV_MENGE_INIT = LV_PDLV_MENGE_INIT + LS_PLANNO_MENGE-PDLV_MENGE - ( LS_PLANNO_MENGE-MENGE_S - LS_PLANNO_MENGE-MENGE_H ).
        ENDLOOP.
        IF LV_PDLV_MENGE_INIT < 0.
          LV_PDLV_MENGE_INIT = 0.
        ENDIF.
        IF <FV_PO_DATA>-CONFIRM_MENGE < 0.
          <FV_PO_DATA>-CONFIRM_MENGE = 0.
        ENDIF.
    
    **剩余可用数量 = 采购订单数量 - ( PO已收数量 或确认预期交货量) - 未确认匹配量 -（ 期初计划送货数量 by ljm 2019-11-28)
    *    "PO已收数量与确认预期交货量哪个数量大就减谁
    *    CLEAR LV_MENGE.
    **    IF lv_poarrive_menge >= <fv_po_data>-confirm_menge.
    **      lv_menge = lv_poarrive_menge.
    **    ELSE.
    **      lv_menge = <fv_po_data>-confirm_menge.
    **    ENDIF.
    *    <FV_PO_DATA>-LEFT_QUAN = <FV_PO_DATA>-BASE_MENGE  - LV_POARRIVE_MENGE
    *                                                     " + lv_duojian_qty  "注释 by ljm 2019-11-28
    *                                                      - <FV_PO_DATA>-CONFIRM_MENGE
    *                                                      - <FV_PO_DATA>-ASSIGNED_MENGE
    *                                                      - LV_PDLV_MENGE_INIT. "（ 期初计划送货数量 by ljm 2019-11-28)
    
        "订单已分配未发
        CLEAR GS_MATCH_DATA .
        CLEAR LV_ZYFWF .
        LOOP AT GT_MATCH_DATA INTO GS_MATCH_DATA WHERE EBELN = <FV_PO_DATA>-EBELN
                                                    AND EBELP = <FV_PO_DATA>-EBELP  .
    
          LV_ZYFWF = LV_ZYFWF + GS_MATCH_DATA-MATCH_MENGE .
        ENDLOOP .
    
    
        " 剩余可用数量 = 采购订单数量 - 订单已收货数量 - 订单已分配未收货数量 - 期初未收货数量
        <FV_PO_DATA>-LEFT_QUAN =  <FV_PO_DATA>-BASE_MENGE  - <FV_PO_DATA>-ARRIVE_MENGE
            - LV_ZYFWF - LV_PDLV_MENGE_INIT .
    
        IF <FV_PO_DATA>-CONFIRM_MENGE > 0 OR <FV_PO_DATA>-ASSIGNED_MENGE > 0.
          <FV_PO_DATA>-SORT = '1'.
        ELSE.
          <FV_PO_DATA>-SORT = '2'.
        ENDIF.
      ENDLOOP.
    
      "UP DATA BY LANJINGCUN 20200420  更新检查PO匹配数量 后续作为锁定用(R2)-end
      SORT GT_PO_DATA BY MATNR SORT LEFT_QUAN WERKS LGORT ST_CODE.
    *  DELETE GT_PO_DATA WHERE LEFT_QUAN = 0 AND ASSIGNED_MENGE = 0.
      DELETE GT_PO_DATA WHERE LEFT_QUAN <= 0 AND ASSIGNED_MENGE = 0."UP BY LANJINGCUN 剩余可用量有可能小于0 删除小于等于0 20210304
      IF GT_PO_DATA[] IS INITIAL.
        MESSAGE '没有可供匹配的采购订单，请注意' TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_LOCK_DATA
    *&---------------------------------------------------------------------*
    *& 锁定数据
    *&---------------------------------------------------------------------*
    FORM FRM_LOCK_DATA .
    
      DATA: LV_MESSAGE TYPE CHAR200.
    
      CHECK P_R02 = 'X'.
    
      CHECK GV_CHANGE = 'X'.
    
      LOOP AT GT_PO_DATA INTO GS_PO_DATA.
    
        CALL FUNCTION 'ENQUEUE_EZMM_EKPO'
          EXPORTING
            EBELN          = GS_PO_DATA-EBELN
            EBELP          = GS_PO_DATA-EBELP
          EXCEPTIONS
            FOREIGN_LOCK   = 1
            SYSTEM_FAILURE = 2
            OTHERS         = 3.
        IF SY-SUBRC <> 0.
          LV_MESSAGE = |采购订单:{ GS_PO_DATA-EBELN } 行项目:{ GS_PO_DATA-EBELP }已经被{ SY-MSGV1 }锁定，请稍后再试|.
          PERFORM FRM_STOP_PROGRAM  USING LV_MESSAGE.
        ENDIF.
    
      ENDLOOP.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_MAKT
    *&---------------------------------------------------------------------*
    *& 物料描述
    *&---------------------------------------------------------------------*
    FORM FRM_GET_MAKT .
    
      DATA: LT_APS_PL LIKE GT_APS_PL.
    
      LT_APS_PL[] = GT_APS_PL[].
      DELETE LT_APS_PL WHERE KEY-MATNR IS INITIAL.
      SORT LT_APS_PL BY KEY-MATNR.
      DELETE ADJACENT DUPLICATES FROM LT_APS_PL COMPARING KEY-MATNR.
    
      SELECT MATNR MAKTX
        INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
        FROM MAKT
         FOR ALL ENTRIES IN LT_APS_PL
       WHERE MATNR = LT_APS_PL-KEY-MATNR
         AND SPRAS = SY-LANGU.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_T006A
    *&---------------------------------------------------------------------*
    *& 计量单位文本
    *&---------------------------------------------------------------------*
    FORM FRM_GET_T006A .
    
      SELECT MSEHI MSEHT
        INTO CORRESPONDING FIELDS OF TABLE GT_T006A
        FROM T006A
       WHERE SPRAS = SY-LANGU
    .
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_LFA1
    *&---------------------------------------------------------------------*
    *& 供应商名称
    *&---------------------------------------------------------------------*
    FORM FRM_GET_LFA1 .
    
      DATA: LT_APS_PL LIKE GT_APS_PL.
    
      LT_APS_PL[] = GT_APS_PL[].
      DELETE LT_APS_PL WHERE MFRNR IS INITIAL .
      SORT LT_APS_PL BY MFRNR.
      DELETE ADJACENT DUPLICATES FROM LT_APS_PL COMPARING MFRNR.
    
      SELECT LIFNR NAME1
        INTO CORRESPONDING FIELDS OF TABLE GT_LFA1
        FROM LFA1
         FOR ALL ENTRIES IN LT_APS_PL
       WHERE LIFNR = LT_APS_PL-MFRNR.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_DOMAIN_TXT
    *&---------------------------------------------------------------------*
    *& 获取DOMAIN值对应的文本
    *&---------------------------------------------------------------------*
    *&      --> U_DOMAIN_NAME
    *&---------------------------------------------------------------------*
    FORM FRM_GET_DOMAIN_TXT  USING U_DOMAIN_NAME.
    
      DATA: LV_DOMNAME LIKE DD07L-DOMNAME,
            LT_DD07V   TYPE TABLE OF DD07V,
            LS_DD07V   TYPE DD07V.
    
      LV_DOMNAME = U_DOMAIN_NAME.
    
      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          DOMNAME        = LV_DOMNAME
          TEXT           = 'X'
          LANGU          = SY-LANGU
        TABLES
          DD07V_TAB      = LT_DD07V
        EXCEPTIONS
          WRONG_TEXTFLAG = 1
          OTHERS         = 2.
      IF LT_DD07V[] IS NOT INITIAL.
        APPEND LINES OF LT_DD07V TO GT_DD07V.
      ENDIF.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_PDLV_MENGE
    *&---------------------------------------------------------------------*
    *& 获取已经收货数量
    *&---------------------------------------------------------------------*
    *&      <-- CS_APS_PL
    *&---------------------------------------------------------------------*
    FORM FRM_GET_RECEIVED_QUAN  CHANGING CS_APS_PL LIKE GS_APS_PL.
    
      LOOP AT GT_PDLV_DATA INTO GS_PDLV_DATA WHERE GUID = CS_APS_PL-GUID.
        CS_APS_PL-RECEIVED_QUAN = CS_APS_PL-RECEIVED_QUAN + GS_PDLV_DATA-RECEIVED_QUAN.
      ENDLOOP.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_ASSIGNED_MENGE
    *&---------------------------------------------------------------------*
    *& 获取已经分配数量
    *&---------------------------------------------------------------------*
    FORM FRM_GET_ASSIGNED_MENGE CHANGING CS_APS_PL LIKE GS_APS_PL.
    
      LOOP AT GT_MMAPS_TEMP INTO GS_MMAPS_TEMP WHERE GUID = CS_APS_PL-GUID.
        CS_APS_PL-ASSIGNED_MENGE = CS_APS_PL-ASSIGNED_MENGE + GS_MMAPS_TEMP-ASSIGN_MENGE.
    *匹配供应商送货数
        PERFORM FRM_SET_VONDER_QTY USING GS_MMAPS_TEMP-LIFNR GS_MMAPS_TEMP-ASSIGN_MENGE CHANGING CS_APS_PL.
      ENDLOOP.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_LEAVE_SCREEN
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    FORM FRM_LEAVE_SCREEN .
      SET SCREEN 0.
      LEAVE SCREEN.
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_HEAD_ALV_INIT
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    FORM FRM_HEAD_ALV_INIT .
    
      IF GCL_ALV_GRID_HEAD IS NOT INITIAL.
        PERFORM FRM_REFRESH_ALV_DATA USING 'HEAD'.
        EXIT.
      ENDIF.
    
    * Create Object
      CASE 'X'.
    *    WHEN p_r01.
    *      CREATE OBJECT gcl_container_head
    *        EXPORTING
    *          container_name = 'C_HEAD'.
    *      CREATE OBJECT gcl_alv_grid_head
    *        EXPORTING
    *          i_parent      = gcl_container_head
    *          i_appl_events = 'X'.
        WHEN P_R02.
          CALL METHOD GCL_SPLITTER->GET_CONTAINER
            EXPORTING
              ROW       = 1
              COLUMN    = 1
            RECEIVING
              CONTAINER = GCL_CONTAINER_HEAD2.
          CREATE OBJECT GCL_ALV_GRID_HEAD
            EXPORTING
              I_PARENT      = GCL_CONTAINER_HEAD2
              I_APPL_EVENTS = 'X'.
      ENDCASE.
    
    *ALV布局
      PERFORM FRM_ALV_LIST_LAYOUT.
    
    * Get field for Display ALV
      PERFORM FRM_ALV_FILL_NAME USING GT_FIELDCAT_HEAD 'HEAD'.
    
      CALL METHOD GCL_ALV_GRID_HEAD->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_LAYOUT                     = GS_LAYOUT_HEAD
          IS_VARIANT                    = GS_VARIAT
          I_SAVE                        = 'A'
        CHANGING
          IT_FIELDCATALOG               = GT_FIELDCAT_HEAD[]
          IT_OUTTAB                     = GT_APS_PL[]
        EXCEPTIONS
          INVALID_PARAMETER_COMBINATION = 1
          PROGRAM_ERROR                 = 2
          TOO_MANY_LINES                = 3
          OTHERS                        = 4.
    
      CREATE OBJECT GCL_EVENT_RECEIVER.
      SET HANDLER GCL_EVENT_RECEIVER->ON_ADD_TOOLBAR_HEAD   FOR GCL_ALV_GRID_HEAD.
      CALL METHOD GCL_ALV_GRID_HEAD->SET_TOOLBAR_INTERACTIVE.
    *  SET HANDLER gcl_event_receiver->on_data_changed FOR gcl_alv_grid_head.
      SET HANDLER GCL_EVENT_RECEIVER->ON_DOUBLE_CLICK_HEAD FOR GCL_ALV_GRID_HEAD.
      SET HANDLER GCL_EVENT_RECEIVER->ON_USER_COMMAND FOR GCL_ALV_GRID_HEAD.
    
    ENDFORM.
    
    
    *&---------------------------------------------------------------------*
    *&      Form  FRM_REFRESH_ALV_DATA
    *&---------------------------------------------------------------------*
    *       REFRESH ALV TABLE
    *----------------------------------------------------------------------*
    FORM FRM_REFRESH_ALV_DATA USING P_TYPE.
    
      DATA LS_STABLE TYPE LVC_S_STBL.
    
      LS_STABLE-ROW = 'X'.
      LS_STABLE-COL = 'X'.
    
      CASE P_TYPE.
        WHEN 'HEAD'.
          CALL METHOD GCL_ALV_GRID_HEAD->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = LS_STABLE
            EXCEPTIONS
              FINISHED  = 1
              OTHERS    = 2.
    *      IF gt_row_no[] IS NOT INITIAL.
    *
    *        CALL METHOD gcl_alv_grid->set_selected_rows
    *          EXPORTING
    *            it_row_no = gt_row_no.
    *      ENDIF.
        WHEN 'PO'.
          CALL METHOD GCL_ALV_GRID_PO->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = LS_STABLE
            EXCEPTIONS
              FINISHED  = 1
              OTHERS    = 2.
        WHEN 'DETAIL'.
          CALL METHOD GCL_ALV_GRID_DETAIL->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = LS_STABLE
            EXCEPTIONS
              FINISHED  = 1
              OTHERS    = 2.
      ENDCASE.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_USER_COMMAND_HEAD
    *&---------------------------------------------------------------------*
    *& Head ALV按钮命令
    *&---------------------------------------------------------------------*
    *&      --> U_UCOMM
    *&---------------------------------------------------------------------*
    FORM FRM_USER_COMMAND_HEAD  USING U_UCOMM TYPE SY-UCOMM.
    
      CASE U_UCOMM.
    *抬头按钮
        WHEN 'SEL_ALL_H'.    "抬头全选
          GS_APS_PL-SEL = 'X'.
          MODIFY GT_APS_PL FROM GS_APS_PL TRANSPORTING SEL WHERE SEL = ''.
        WHEN 'SEL_NULL_H'.  "抬头全不选
          CLEAR: GS_APS_PL-SEL.
          MODIFY GT_APS_PL FROM GS_APS_PL TRANSPORTING SEL WHERE SEL = 'X'.
        WHEN 'SEARCH'.    "抬头筛选
          PERFORM FRM_SEARCH_FROM_HEAD.
        WHEN 'QXFP'.      "抬头取消分配
          PERFORM FRM_QXFP.
        WHEN 'EXPORT_FP'. "导出已经分配的数据
          PERFORM FRM_EXPORT_FP.
    
    *采购订单按钮
        WHEN 'SEL_ALL_P'.    "PO全选
          GS_PO_DATA-SEL = 'X'.
          MODIFY GT_PO_SHOW FROM GS_PO_DATA TRANSPORTING SEL WHERE SEL = ''.
        WHEN 'SEL_NULL_P'.  "PO全不选
          GS_PO_DATA-SEL = ''.
          MODIFY GT_PO_SHOW FROM GS_PO_DATA TRANSPORTING SEL WHERE SEL = 'X'.
        WHEN 'RG'.          "PO人工分配数量
          PERFORM FRM_RGFP.
        WHEN 'AUTO'.        "PO自动分配数量
          PERFORM FRM_AUTO.
      ENDCASE.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_HEAD_DATA
    *&---------------------------------------------------------------------*
    *& 获取抬头数据
    *&---------------------------------------------------------------------*
    FORM FRM_GET_HEAD_DATA .
    
      "add BY LANJINGCUN 20200426  最晚答复日期的检验 临时结构start
      DATA : BEGIN OF LS_VERSION ,"工厂最大版本号
               WERKS   TYPE T001W-WERKS,
               VERSION TYPE ZMMAPS_PL-VERSION,
             END OF LS_VERSION .
      DATA : BEGIN OF LS_MATNR ,"物料保留天数
               MATNR       TYPE MARA-MATNR,
               REMAIN_DAYS TYPE ZMMAPS_PL-REMAIN_DAYS,
             END OF LS_MATNR .
    
      DATA : LT_MATNR LIKE TABLE OF LS_MATNR .
      DATA : LT_VERSION LIKE TABLE OF LS_VERSION .
      DATA : LT_ZPLTA TYPE TABLE OF TY_ZPLTA .
      DATA : LS_ZPLTA TYPE TY_ZPLTA .
      DATA : LT_MARAL TYPE TABLE OF MARA .
      DATA : LS_MARAL TYPE MARA .
      DATA : LV_DAYS TYPE SY-DATUM .
      DATA : LV_MATKL TYPE MARA-MATKL .
      DATA : LV_WHERE(20) TYPE C .
    
      CLEAR GT_ZJFSDT .
      CLEAR GT_ZPLTA .
    
      "add BY LANJINGCUN 20200426  最晚答复日期的检验 临时结构end
    
      FIELD-SYMBOLS: <FV_APS_PL>  LIKE GS_APS_PL.
    
      IF S_EKGRP[] IS INITIAL .
        SELECT *
          INTO CORRESPONDING FIELDS OF TABLE @GT_APS_PL
          FROM ZMMAPS_PL AS Z
         WHERE Z~WERKS IN @S_WERKS
           AND Z~LGORT IN @S_LGORT
           AND Z~MATNR IN @S_MATNR
           AND Z~CPUDT IN @S_DATE2
           AND Z~ST_CODE IN @S_CODE2
           AND Z~REQDATE <= @S_DATE-LOW
           AND Z~ZPR_STS IN @S_STS
           AND Z~ZPR_STS <> 'C'.
      ELSE.
        SELECT *
          INTO CORRESPONDING FIELDS OF TABLE @GT_APS_PL
          FROM ZMMAPS_PL AS Z
          LEFT JOIN MARA AS A
            ON A~MATNR = Z~MATNR
          LEFT JOIN MARC AS C
            ON C~MATNR = Z~MATNR
           AND C~WERKS = Z~WERKS
         WHERE Z~WERKS IN @S_WERKS
           AND Z~LGORT IN @S_LGORT
           AND Z~ST_CODE IN @S_CODE2
           AND C~EKGRP IN @S_EKGRP
           AND A~MATKL IN @S_MATKL
           AND Z~MATNR IN @S_MATNR
           AND Z~CPUDT IN @S_DATE2
           AND Z~REQDATE <= @S_DATE-LOW
           AND Z~ZPR_STS IN @S_STS
           AND Z~ZPR_STS <> 'C' .
      ENDIF.
    
      IF GT_APS_PL[] IS INITIAL .
        DATA(LV_MESSAGE1) = '没有符合条件的数据'.
        PERFORM FRM_STOP_PROGRAM  USING LV_MESSAGE1 .
      ENDIF .
    
    *筛选需要关闭的计划
      LOOP AT GT_APS_PL INTO GS_APS_PL.
        "UP BY LANJINGCUN 20200426  最晚答复日期的检验 start
    *    DATA(LV_DATE) = GS_APS_PL-VERSION + GS_APS_PL-REMAIN_DAYS.
    *    IF LV_DATE < SY-DATUM.
    *      DELETE GT_APS_PL INDEX SY-TABIX.
    *    ENDIF.
        "获取最大版本号
        CLEAR LS_VERSION .
        READ TABLE LT_VERSION INTO LS_VERSION WITH  KEY WERKS = GS_APS_PL-WERKS .
        IF SY-SUBRC <> 0 .
          SELECT MAX( VERSION )
          INTO LS_VERSION-VERSION
          FROM ZMMAPS_PL
          WHERE WERKS = GS_APS_PL-WERKS
            AND ZRSV01 = ''. "排除手工导入的
          LS_VERSION-WERKS = GS_APS_PL-WERKS .
          APPEND LS_VERSION TO LT_VERSION .
        ENDIF .
    
        "获取保留天数
        CLEAR LS_MATNR .
        SORT LT_MATNR BY MATNR .
        READ TABLE LT_MATNR INTO LS_MATNR WITH  KEY MATNR = GS_APS_PL-MATNR BINARY SEARCH.
        IF SY-SUBRC <> 0 .
    
          "1．	通过最后1次版本号和物料号从PL表获取保留天数；
          SELECT MAX( REMAIN_DAYS )
            INTO LS_MATNR-REMAIN_DAYS
            FROM ZMMAPS_PL
            WHERE MATNR = GS_APS_PL-MATNR
              AND VERSION = LS_VERSION-VERSION
              AND WERKS = GS_APS_PL-WERKS
              AND ZRSV01 = ''. "排除手工导入的
          IF LS_MATNR-REMAIN_DAYS = 0 .
    *           2      通过最新版本号，该物料的物料组从PL表获取物料和保留天数清单，然后循环这个清单，获取这个清单中的物料，
    *                 从物料分类视图键值ZJFSDT获取值，如果该值与该物料从物料分类视图键值ZJFSDT获取的值一样，则跳出循环，将内表的保留天数的值赋值给这个物料；
    
            SELECT SINGLE MATKL
            INTO LV_MATKL
            FROM MARA
            WHERE MATNR = GS_APS_PL-MATNR .
    
            CLEAR GS_ZPLTA .
            LOOP AT GT_ZPLTA INTO GS_ZPLTA WHERE MATKL+0(2) = LV_MATKL+0(2) .
    
              CLEAR GS_ZJFSDT .
              SORT GT_ZJFSDT BY MATNR .
              READ TABLE GT_ZJFSDT INTO GS_ZJFSDT WITH KEY MATNR = GS_ZPLTA-MATNR BINARY SEARCH.
              IF SY-SUBRC <> 0 .
                PERFORM FRM_GET_ZJFSDT USING GS_ZPLTA-MATNR GS_ZPLTA-MATKL  ."获取分配视图
              ENDIF .
              IF GS_ZPLTA-REMAIN_DAYS = GS_ZJFSDT-ZJFSDT .
                "如果该值与该物料从物料分类视图键值ZJFSDT获取的值一样，则跳出循环，将内表的保留天数的值赋值给这个物料；
                LS_MATNR-REMAIN_DAYS = GS_ZJFSDT-ZJFSDT .
                EXIT .
              ENDIF .
    
            ENDLOOP .
            IF SY-SUBRC <> 0 .
              "
              CLEAR LT_ZPLTA .
              CLEAR LT_MARAL .
              "获取同物料组物料
              CLEAR LV_WHERE .
              CONCATENATE  LV_MATKL+0(2) '%' INTO LV_WHERE .
              SELECT *
                INTO TABLE LT_MARAL
                FROM MARA
                WHERE MATKL LIKE LV_WHERE.
    
              "获取同物料组物料
              SELECT *
                INTO TABLE LT_ZPLTA
                FROM ZMMAPS_PL
                FOR ALL ENTRIES IN LT_MARAL
                WHERE MATNR = LT_MARAL-MATNR
                  AND VERSION = LS_VERSION-VERSION
                AND WERKS = GS_APS_PL-WERKS
                AND ZRSV01 = ''. "排除手工导入的.
              CLEAR LS_ZPLTA .
              LOOP AT LT_ZPLTA INTO LS_ZPLTA  .
    
                "保存临时运算表
                CLEAR LS_MARAL .
                SORT LT_MARAL BY MATNR .
                READ TABLE LT_MARAL INTO LS_MARAL WITH KEY MATNR = LS_ZPLTA-MATNR BINARY SEARCH .
                LS_ZPLTA-MATKL = LS_MARAL-MATKL .
                APPEND LS_ZPLTA TO GT_ZPLTA .
    
                CLEAR GS_ZJFSDT .
                SORT GT_ZJFSDT BY MATNR .
                READ TABLE GT_ZJFSDT INTO GS_ZJFSDT WITH KEY MATNR = LS_ZPLTA-MATNR BINARY SEARCH .
                IF SY-SUBRC <> 0 .
                  PERFORM FRM_GET_ZJFSDT USING LS_ZPLTA-MATNR LS_MARAL-MATKL  ."获取分配视图
                ENDIF .
                IF LS_ZPLTA-REMAIN_DAYS = GS_ZJFSDT-ZJFSDT .
                  "如果该值与该物料从物料分类视图键值ZJFSDT获取的值一样，则跳出循环，将内表的保留天数的值赋值给这个物料；
                  LS_MATNR-REMAIN_DAYS = GS_ZJFSDT-ZJFSDT .
                  EXIT .
                ENDIF .
    
              ENDLOOP .
    
            ENDIF .
            IF LS_MATNR-REMAIN_DAYS = 0 .
    *                  "3．  如果仍未空，则以物料分类视图键值ZJFSDT作为该物料的保留天数
              CLEAR GS_ZJFSDT .
              SORT GT_ZJFSDT BY MATNR .
              READ TABLE GT_ZJFSDT INTO GS_ZJFSDT WITH KEY MATNR = GS_APS_PL-MATNR BINARY SEARCH .
              IF SY-SUBRC <> 0 .
                PERFORM FRM_GET_ZJFSDT USING GS_APS_PL-MATNR LV_MATKL  ."获取分配视图
              ENDIF .
              LS_MATNR-REMAIN_DAYS = GS_ZJFSDT-ZJFSDT .
              IF LS_MATNR-REMAIN_DAYS = 0 .
                "4．  如果仍未空，则默认包材（20开头）7天，原辅料（10开头）15天，其他7天
                IF LV_MATKL+0(2) = '10' .
                  LS_MATNR-REMAIN_DAYS = 15.
                ELSE .
                  LS_MATNR-REMAIN_DAYS = 7.
                ENDIF .
              ENDIF .
            ENDIF .
    
          ENDIF .
          LS_MATNR-MATNR = GS_APS_PL-MATNR .
          APPEND LS_MATNR TO LT_MATNR .
        ENDIF .
        "取“计算保留天数”和对应的送料计划行上的“保留天数”的最大值来作为“参考保留天数”
        GS_APS_PL-ZJSYUL  =  LS_MATNR-REMAIN_DAYS  ."计算的保留日期
        IF LS_MATNR-REMAIN_DAYS < GS_APS_PL-REMAIN_DAYS .
          LS_MATNR-REMAIN_DAYS = GS_APS_PL-REMAIN_DAYS  .
        ENDIF .
    
        GS_APS_PL-VERSIOM  = LS_VERSION-VERSION ."计算最大版本号
        LV_DAYS = LS_VERSION-VERSION + LS_MATNR-REMAIN_DAYS - 1.
        IF LV_DAYS < SY-DATUM.
          DELETE GT_APS_PL INDEX SY-TABIX.
        ELSE .
          GS_APS_PL-ZZWDFR  = LV_DAYS ."计算的最晚回复日期
          MODIFY GT_APS_PL FROM GS_APS_PL
                     TRANSPORTING  ZJSYUL ZZWDFR VERSIOM
                     WHERE GUID = GS_APS_PL-GUID .
        ENDIF.
    
    
        "UP BY LANJINGCUN 20200426  最晚答复日期的检验 end
      ENDLOOP.
    
    *过滤权限
      SELECT MATNR, WERKS, EKGRP
        INTO TABLE @DATA(LT_MARC)
        FROM MARC
         FOR ALL ENTRIES IN @GT_APS_PL
       WHERE MATNR = @GT_APS_PL-MATNR
         AND WERKS = @GT_APS_PL-WERKS.
    
      SORT LT_MARC BY MATNR WERKS.
    
      LOOP AT GT_APS_PL INTO GS_APS_PL.
    
        DATA(LV_TABIX) = SY-TABIX.
    
        READ TABLE LT_MARC INTO DATA(LS_MARC) WITH KEY MATNR = GS_APS_PL-MATNR WERKS = GS_APS_PL-WERKS BINARY SEARCH.
        CHECK SY-SUBRC = 0.
    
        AUTHORITY-CHECK OBJECT 'ZMM020'
         ID 'WERKS' FIELD GS_APS_PL-WERKS
         ID 'EKGRP' FIELD GS_APS_PL-EKGRP.
        IF SY-SUBRC <> 0.
          DELETE GT_APS_PL INDEX LV_TABIX.
          CONTINUE.
        ENDIF.
    
      ENDLOOP.
    
      IF GT_APS_PL[] IS INITIAL.
        DATA(LV_MESSAGE) = |{ '您没有' }{ GS_APS_PL-WERKS }{ '工厂,' }{ GS_APS_PL-EKGRP }{ '采购组的权限!' }|.
        PERFORM FRM_STOP_PROGRAM  USING LV_MESSAGE .
      ENDIF.
    
      IF S_DATE-LOW >= SY-DATUM.
        GV_CHANGE = 'X'.
      ENDIF.
    
      IF GV_CHANGE IS INITIAL.
        GS_MENU-FCODE = 'SAVE'.
        APPEND GS_MENU TO GT_MENU.
      ENDIF.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_TEMP_DATA
    *&---------------------------------------------------------------------*
    *& 获取临时表数据
    *&---------------------------------------------------------------------*
    FORM FRM_GET_TEMP_DATA .
      DATA: LT_PLANNO_ITEM  LIKE TABLE OF GS_PLANNO_ITEM,
            LT_PLANNO_MENGE LIKE TABLE OF GS_PLANNO_MENGE,
            LV_MFRNR        LIKE MARA-MFRNR.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE GT_MMAPS_TEMP
        FROM ZMMAPS_TEMP
         FOR ALL ENTRIES IN GT_APS_PL
       WHERE GUID = GT_APS_PL-GUID
         AND ASSIGN_MENGE > 0
         AND INACTIVE     = ''.
      IF GT_PO_DATA[] IS NOT INITIAL.
        SELECT *
          APPENDING CORRESPONDING FIELDS OF TABLE GT_MMAPS_TEMP
          FROM ZMMAPS_TEMP
           FOR ALL ENTRIES IN GT_PO_DATA
         WHERE EBELN = GT_PO_DATA-EBELN
           AND EBELP = GT_PO_DATA-EBELP
           AND ASSIGN_MENGE > 0
           AND INACTIVE     = ''.
      ENDIF.
    
      SORT GT_MMAPS_TEMP.
      DELETE ADJACENT DUPLICATES FROM GT_MMAPS_TEMP COMPARING ALL FIELDS.
      "UP DATA BY LANJINGCUN 20200420  更新获取收货数量数据(R2 APS送货计划匹配)-start
    *  "ASSIGN_MENGE分配数量减去已收货数量 addby ljm 20191225-------
    *  MOVE-CORRESPONDING GT_MMAPS_TEMP TO LT_PLANNO_ITEM.
    *  DELETE LT_PLANNO_ITEM WHERE DLV_PLANNO IS INITIAL OR DLV_ITEMNO IS INITIAL.
    *  SORT LT_PLANNO_ITEM BY DLV_PLANNO DLV_ITEMNO.
    *  DELETE ADJACENT DUPLICATES FROM LT_PLANNO_ITEM COMPARING DLV_PLANNO DLV_ITEMNO.
    *  "根据计划交货行从ZMATDOC获取已交货数量
    *  SELECT PDLV~DLV_PLANNO,
    *         PDLV~DLV_ITEMNO,
    *         SUM( CASE M~SHKZG WHEN 'S' THEN M~MENGE ELSE 0 END ) AS MENGE_S,
    *         SUM( CASE M~SHKZG WHEN 'H' THEN M~MENGE ELSE 0 END ) AS MENGE_H
    *    FROM @LT_PLANNO_ITEM AS PDLV
    *    JOIN ZMATDOC AS M
    *      ON M~DLV_PLANNO = PDLV~DLV_PLANNO
    *     AND M~DLV_ITEMNO = PDLV~DLV_ITEMNO
    *         GROUP BY PDLV~DLV_PLANNO,PDLV~DLV_ITEMNO
    *         ORDER BY PDLV~DLV_PLANNO,PDLV~DLV_ITEMNO
    *    INTO TABLE @LT_PLANNO_MENGE.
    *
    *  "ASSIGN_MENGE分配数量减去已收货数量 addby ljm 20191225-------
    *  LOOP AT gt_mmaps_temp ASSIGNING FIELD-SYMBOL(<fs_mmaps_temp>) .
    *    READ TABLE lt_planno_menge WITH KEY dlv_planno = <fs_mmaps_temp>-dlv_planno dlv_itemno = <fs_mmaps_temp>-dlv_itemno BINARY SEARCH INTO gs_planno_menge.
    *    IF sy-subrc = 0.
    *      <fs_mmaps_temp>-assign_menge = <fs_mmaps_temp>-assign_menge - gs_planno_menge-menge_s + gs_planno_menge-menge_h.
    *      DELETE lt_planno_menge WHERE dlv_planno = <fs_mmaps_temp>-dlv_planno AND dlv_itemno = <fs_mmaps_temp>-dlv_itemno."删除计划行，避免多次减数
    *    ENDIF.
    *
    *    MOVE-CORRESPONDING <fs_mmaps_temp> TO gs_match_data.
    *    gs_match_data-match_menge =  <fs_mmaps_temp>-assign_menge."分配数量
    *    SELECT SINGLE maktx INTO gs_match_data-maktx FROM makt WHERE matnr = gs_match_data-matnr AND spras = sy-langu.
    *    SELECT SINGLE meins mfrnr
    *      INTO (gs_match_data-meins,lv_mfrnr)
    *      FROM mara
    *     WHERE matnr = gs_match_data-matnr.
    *
    *    SELECT SINGLE name1 INTO gs_match_data-vendor_name FROM lfa1 WHERE lifnr = gs_match_data-lifnr.
    *    SELECT SINGLE mfrnr
    *      INTO gs_match_data-producer
    *      FROM ekpo
    *     WHERE ebeln = gs_match_data-ebeln
    *       AND ebelp = gs_match_data-ebelp.
    *    IF gs_match_data-producer IS INITIAL.
    *      gs_match_data-producer = lv_mfrnr.
    *    ENDIF.
    *
    *    IF gs_match_data-producer IS NOT INITIAL.
    *      SELECT SINGLE name1 INTO gs_match_data-producer_name FROM lfa1 WHERE lifnr = gs_match_data-producer.
    *    ENDIF.
    *
    *    IF <fs_mmaps_temp>-released IS NOT INITIAL.
    *      gs_match_data-light = icon_complete.
    *    ELSEIF gs_match_data-pdlv_date IS NOT INITIAL."确认计划交货日期
    *      gs_match_data-light = icon_led_green.
    *    ELSE.
    *      gs_match_data-light = icon_led_yellow.
    *    ENDIF.
    *
    *    APPEND gs_match_data TO gt_match_data.
    *    CLEAR: gs_match_data, lv_mfrnr.
    *
    *  ENDLOOP.
    
      DATA : LT_PDLV_DATA LIKE GT_MMAPS_TEMP .
    *  DATA : LT_ZMATDOC TYPE TABLE OF TY_ZMATDOC .
      FIELD-SYMBOLS <LS_ZMATDOC> TYPE TY_ZMATDOC .
    
      "获取对应匹配的送货计划号
      LT_PDLV_DATA[] = GT_MMAPS_TEMP[] .
      DELETE LT_PDLV_DATA WHERE DLV_PLANNO = '' .
      SORT LT_PDLV_DATA BY DLV_PLANNO DLV_ITEMNO.
      DELETE ADJACENT DUPLICATES FROM LT_PDLV_DATA COMPARING DLV_PLANNO DLV_ITEMNO.
    
      "根据送货计划号 获取ZMATDOC 表数据
      CLEAR GT_ZMATDOC .
      IF LT_PDLV_DATA[] IS NOT INITIAL .
        "根据计划交货行从ZMATDOC获取已交货数量
        SELECT PDLV~DLV_PLANNO,
               PDLV~DLV_ITEMNO,
               SUM( CASE M~SHKZG WHEN 'S' THEN M~MENGE WHEN 'H' THEN 0 - M~MENGE  ELSE 0  END ) AS MENGE
          FROM @LT_PDLV_DATA AS PDLV
          JOIN ZMATDOC AS M
            ON M~DLV_PLANNO = PDLV~DLV_PLANNO
           AND M~DLV_ITEMNO = PDLV~DLV_ITEMNO
               GROUP BY PDLV~DLV_PLANNO,PDLV~DLV_ITEMNO
               ORDER BY PDLV~DLV_PLANNO,PDLV~DLV_ITEMNO
          INTO TABLE @GT_ZMATDOC.
      ENDIF .
      SORT GT_ZMATDOC BY DLV_PLANNO DLV_ITEMNO .
      LOOP AT GT_MMAPS_TEMP ASSIGNING FIELD-SYMBOL(<FS_MMAPS_TEMP>) .
    
        READ TABLE GT_ZMATDOC ASSIGNING <LS_ZMATDOC> WITH KEY DLV_PLANNO = <FS_MMAPS_TEMP>-DLV_PLANNO
                                                               DLV_ITEMNO = <FS_MMAPS_TEMP>-DLV_ITEMNO BINARY SEARCH .
        IF SY-SUBRC = 0 AND <LS_ZMATDOC>-MENGE <> 0.
    
          IF <FS_MMAPS_TEMP>-ASSIGN_MENGE >= <LS_ZMATDOC>-MENGE ."如果分配数量 大于等于收货数量
            <FS_MMAPS_TEMP>-ASSIGN_MENGE  = <FS_MMAPS_TEMP>-ASSIGN_MENGE - <LS_ZMATDOC>-MENGE ."剩余分配数量 等于分配数量减交货数量
            <LS_ZMATDOC>-MENGE = 0 ."收货数量清零
          ELSE .
            <LS_ZMATDOC>-MENGE = <LS_ZMATDOC>-MENGE - <FS_MMAPS_TEMP>-ASSIGN_MENGE ."收货数量减掉分配数量
            <FS_MMAPS_TEMP>-ASSIGN_MENGE = 0 ."剩余分配数量为0
          ENDIF .
    
        ENDIF.
    
        CHECK <FS_MMAPS_TEMP>-ASSIGN_MENGE > 0 ."只有大于0的才参与运算
        MOVE-CORRESPONDING <FS_MMAPS_TEMP> TO GS_MATCH_DATA.
        GS_MATCH_DATA-MATCH_MENGE =  <FS_MMAPS_TEMP>-ASSIGN_MENGE."分配数量
        SELECT SINGLE MAKTX INTO GS_MATCH_DATA-MAKTX FROM MAKT WHERE MATNR = GS_MATCH_DATA-MATNR AND SPRAS = SY-LANGU.
        SELECT SINGLE MEINS MFRNR
          INTO (GS_MATCH_DATA-MEINS,LV_MFRNR)
          FROM MARA
         WHERE MATNR = GS_MATCH_DATA-MATNR.
    
        SELECT SINGLE NAME1 INTO GS_MATCH_DATA-VENDOR_NAME FROM LFA1 WHERE LIFNR = GS_MATCH_DATA-LIFNR.
        SELECT SINGLE MFRNR
          INTO GS_MATCH_DATA-PRODUCER
          FROM EKPO
         WHERE EBELN = GS_MATCH_DATA-EBELN
           AND EBELP = GS_MATCH_DATA-EBELP.
        IF GS_MATCH_DATA-PRODUCER IS INITIAL.
          GS_MATCH_DATA-PRODUCER = LV_MFRNR.
        ENDIF.
    
        IF GS_MATCH_DATA-PRODUCER IS NOT INITIAL.
          SELECT SINGLE NAME1 INTO GS_MATCH_DATA-PRODUCER_NAME FROM LFA1 WHERE LIFNR = GS_MATCH_DATA-PRODUCER.
        ENDIF.
    
        IF <FS_MMAPS_TEMP>-RELEASED IS NOT INITIAL.
          GS_MATCH_DATA-LIGHT = ICON_COMPLETE.
        ELSEIF GS_MATCH_DATA-PDLV_DATE IS NOT INITIAL."确认计划交货日期
          GS_MATCH_DATA-LIGHT = ICON_LED_GREEN.
        ELSE.
          GS_MATCH_DATA-LIGHT = ICON_LED_YELLOW.
        ENDIF.
    
        APPEND GS_MATCH_DATA TO GT_MATCH_DATA.
        CLEAR: GS_MATCH_DATA, LV_MFRNR.
    
      ENDLOOP.
      "UP DATA BY LANJINGCUN 20200420  更新获取收货数量数据(R2 APS送货计划匹配)-end
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_PDLV_DATA
    *&---------------------------------------------------------------------*
    *& 获取已经确认交期的数据
    *&---------------------------------------------------------------------*
    FORM FRM_GET_PDLV_DATA .
    
      CHECK GT_PO_DATA[] IS NOT INITIAL.
      DATA : LT_PDLV_DATA   TYPE TABLE OF TY_PDLV_DATA .
    
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE LT_PDLV_DATA
        FROM ZMMWMS_POL
        JOIN ZMMWMS_POH
          ON ZMMWMS_POH~DLV_PLANNO = ZMMWMS_POL~DLV_PLANNO
         FOR ALL ENTRIES IN GT_PO_DATA
       WHERE ZMMWMS_POL~UNIQUEID = GT_PO_DATA-UNIQUEID
    *     AND zmmwms_pol~origin  = 'X'
         AND ZMMWMS_POL~XCLOSED <> 'X'
         AND ZMMWMS_POL~PDLV_MENGE > 0
        .
    
      "ADD BY LANJINGCUN 20200420 表GT_PDLV_DAT应该为temp的值 在此通过pol获取对应的temp表-START
      GT_PDLV_DATA_M[] = LT_PDLV_DATA[] .
      DELETE GT_PDLV_DATA_M WHERE ORIGIN <> 'M'."期初导入标识
    
      SORT LT_PDLV_DATA BY DLV_PLANNO DLV_ITEMNO.
      DELETE ADJACENT DUPLICATES FROM LT_PDLV_DATA
                         COMPARING DLV_PLANNO DLV_ITEMNO.
    
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE GT_PDLV_DATA
        FROM ZMMAPS_TEMP
        FOR ALL ENTRIES IN LT_PDLV_DATA
        WHERE DLV_PLANNO = LT_PDLV_DATA-DLV_PLANNO
          AND DLV_ITEMNO = LT_PDLV_DATA-DLV_ITEMNO .
      "ADD BY LANJINGCUN 20200420 表GT_PDLV_DAT应该为temp的值 在此通过pol获取对应的temp表-END
    
      PERFORM FRM_GET_PDLV_RECEIVED_QUAN.
    
    
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_PO_DATA
    *&---------------------------------------------------------------------*
    *& 获取采购订单数据
    *&---------------------------------------------------------------------*
    FORM FRM_GET_PO_DATA .
    
      CHECK GT_APS_PL[] IS NOT INITIAL.
    
      SELECT EKPO~EBELN
             EKPO~EBELP
             EKPO~UNIQUEID
             EKKO~LIFNR
             EKPO~WERKS
             EKPO~LGORT
             EKPO~MATNR "AS ematn
             EKPO~EMATN "AS matnr
             EKPO~ST_CODE
             EKKO~BEDAT AS PO_DATE
             EKPO~MENGE
             EKPO~MEINS
             EKPO~KZWI1
             EKKO~WAERS
             EKKO~FRGSX
             EKKO~FRGKE
             EKPO~LMEIN
        INTO CORRESPONDING FIELDS OF TABLE GT_PO_DATA
        FROM EKPO
        JOIN EKKO
          ON EKKO~EBELN = EKPO~EBELN
         FOR ALL ENTRIES IN GT_APS_PL
       WHERE EKKO~BSART IN ('ZNB')
         AND EKPO~WERKS = GT_APS_PL-WERKS
    *     AND ekpo~lgort = gt_aps_pl-lgort
         AND EKPO~MATNR = GT_APS_PL-MATNR
         AND EKPO~ST_CODE = GT_APS_PL-ST_CODE
         AND EKKO~MEMORY = ''
         AND EKPO~LOEKZ = ''
         AND EKPO~ABSKZ = ''
         AND EKPO~ELIKZ = ''
        .
    
      CHECK GT_PO_DATA[] IS NOT INITIAL.
    
    *物料主数据有001分类时属性ZSHBS为空的订单
      TYPES: BEGIN OF TY_MATNR_ZSHBS,
               MATNR LIKE MARA-MATNR,
               ZSHBS TYPE ATWRT70,
             END OF TY_MATNR_ZSHBS.
    
      DATA: LS_MATNR_ZSHBS TYPE TY_MATNR_ZSHBS.
    
      LOOP AT GT_PO_DATA INTO DATA(LS_PO_DATA).
        DATA(LV_TABIX) = SY-TABIX.
        SELECT SINGLE CLINT, ZAEHL
          FROM KSSK
         WHERE OBJEK = @LS_PO_DATA-MATNR
           AND MAFID = 'O'
           AND KLART = '001'
           AND DATUB >= @SY-DATUM
          INTO @DATA(LS_KSSK).
        CHECK SY-SUBRC = 0.
        CHECK ZCL_CM_PUBLIC=>GET_TX_SIGLE( I_MATNR = LS_PO_DATA-MATNR I_ATNAM = 'ZSHBS' ) IS NOT INITIAL.
        DELETE GT_PO_DATA INDEX LV_TABIX.
      ENDLOOP.
    
      SELECT FRGKE FROM T16FB WHERE KZFRE = 'X' INTO TABLE @DATA(LT_SP).
      LOOP AT GT_PO_DATA INTO LS_PO_DATA.
        LV_TABIX = SY-TABIX.
        IF LS_PO_DATA-FRGSX IS NOT INITIAL.
          READ TABLE LT_SP TRANSPORTING NO FIELDS WITH KEY FRGKE = LS_PO_DATA-FRGKE.
          IF SY-SUBRC <> 0.
            DELETE GT_PO_DATA INDEX LV_TABIX.
          ENDIF.
        ENDIF.
      ENDLOOP.
    
      CHECK GT_PO_DATA[] IS NOT INITIAL.
    
      SELECT K~EBELN,
             K~EBELP,
             K~BELNR,
             K~BUZEI,
             K~SHKZG,
             K~MENGE
        FROM @GT_PO_DATA AS PO
        JOIN EKBE AS K
          ON PO~EBELN = K~EBELN
         AND PO~EBELP = K~EBELP
       WHERE K~VGABE = '1'
        INTO TABLE @DATA(LT_EKBE).
    
      LOOP AT LT_EKBE INTO DATA(LS_EKBE).
    
        IF LS_EKBE-SHKZG = 'H'.
          LS_EKBE-MENGE = 0 - LS_EKBE-MENGE.
        ENDIF.
    
        GS_EKBE-EBELN = LS_EKBE-EBELN.
        GS_EKBE-EBELP = LS_EKBE-EBELP.
        GS_EKBE-MENGE = LS_EKBE-MENGE.
    
        COLLECT GS_EKBE INTO GT_EKBE.
        CLEAR   GS_EKBE.
    
      ENDLOOP.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form frm_set_key
    *&---------------------------------------------------------------------*
    *& 设置KEY字段
    *&---------------------------------------------------------------------*
    FORM FRM_SET_KEY.
    
      LOOP AT GT_APS_PL ASSIGNING FIELD-SYMBOL(<FV_APS_PL>).
    
    *设置主键
        <FV_APS_PL>-KEY-WERKS    = <FV_APS_PL>-WERKS.
        <FV_APS_PL>-KEY-LGORT    = <FV_APS_PL>-LGORT.
        <FV_APS_PL>-KEY-MATNR    = <FV_APS_PL>-MATNR.
        <FV_APS_PL>-KEY-ST_CODE  = <FV_APS_PL>-ST_CODE.
        <FV_APS_PL>-KEY-REQDATE = <FV_APS_PL>-REQDATE.
    
        MOVE-CORRESPONDING <FV_APS_PL>-KEY TO <FV_APS_PL>-KEY2.
    
      ENDLOOP.
    
      LOOP AT GT_PDLV_DATA INTO GS_PDLV_DATA.
    
        GS_PDLV_DATA-KEY-WERKS = GS_PDLV_DATA-WERKS.
        GS_PDLV_DATA-KEY-LGORT = GS_PDLV_DATA-LGORT.
        GS_PDLV_DATA-KEY-MATNR = GS_PDLV_DATA-MATNR.
        GS_PDLV_DATA-KEY-ST_CODE = GS_PDLV_DATA-ST_CODE.
        GS_PDLV_DATA-KEY-REQDATE = GS_PDLV_DATA-PDLV_DATE.
        MODIFY GT_PDLV_DATA FROM GS_PDLV_DATA.
    
      ENDLOOP.
    
      LOOP AT GT_PO_DATA ASSIGNING FIELD-SYMBOL(<FV_PO_DATA>).
    
    *KEY字段赋值
        <FV_PO_DATA>-KEY-WERKS = <FV_PO_DATA>-WERKS.
        <FV_PO_DATA>-KEY-LGORT = <FV_PO_DATA>-LGORT.
        <FV_PO_DATA>-KEY-MATNR = <FV_PO_DATA>-MATNR.
        <FV_PO_DATA>-KEY-ST_CODE = <FV_PO_DATA>-ST_CODE.
    
        MOVE-CORRESPONDING <FV_PO_DATA>-KEY TO <FV_PO_DATA>-KEY2.
    
      ENDLOOP.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_PO_ALV_INIT
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    FORM FRM_PO_ALV_INIT .
    
      IF GCL_ALV_GRID_PO IS NOT INITIAL.
        PERFORM FRM_REFRESH_ALV_DATA USING 'PO'.
        EXIT.
      ENDIF.
    
    * Create Object
      CASE 'X'.
    *    WHEN p_r01.
    *      CREATE OBJECT gcl_container_po
    *        EXPORTING
    *          container_name = 'C_PO'.
    *
    *      CREATE OBJECT gcl_alv_grid_po
    *        EXPORTING
    *          i_parent      = gcl_container_po
    *          i_appl_events = 'X'.
        WHEN P_R02.
          CALL METHOD GCL_SPLITTER->GET_CONTAINER
            EXPORTING
              ROW       = 2
              COLUMN    = 1
            RECEIVING
              CONTAINER = GCL_CONTAINER_PO2.
    
          CREATE OBJECT GCL_ALV_GRID_PO
            EXPORTING
              I_PARENT      = GCL_CONTAINER_PO2
              I_APPL_EVENTS = 'X'.
      ENDCASE.
    
    
    *ALV布局
      PERFORM FRM_ALV_LIST_LAYOUT.
    
    * Get field for Display ALV
      PERFORM FRM_ALV_FILL_NAME USING GT_FIELDCAT_PO 'PO'.
    
      CALL METHOD GCL_ALV_GRID_PO->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_LAYOUT                     = GS_LAYOUT_PO
          IS_VARIANT                    = GS_VARIAT
          I_SAVE                        = 'A'
        CHANGING
          IT_FIELDCATALOG               = GT_FIELDCAT_PO[]
          IT_OUTTAB                     = GT_PO_SHOW[]
        EXCEPTIONS
          INVALID_PARAMETER_COMBINATION = 1
          PROGRAM_ERROR                 = 2
          TOO_MANY_LINES                = 3
          OTHERS                        = 4.
    
      CREATE OBJECT GCL_EVENT_RECEIVER.
      SET HANDLER GCL_EVENT_RECEIVER->ON_ADD_TOOLBAR_PO   FOR GCL_ALV_GRID_PO.
      CALL METHOD GCL_ALV_GRID_PO->SET_TOOLBAR_INTERACTIVE.
      SET HANDLER GCL_EVENT_RECEIVER->ON_DATA_CHANGED FOR GCL_ALV_GRID_PO.
      SET HANDLER GCL_EVENT_RECEIVER->ON_DATA_CHANGED_FINISHED FOR GCL_ALV_GRID_PO.
      SET HANDLER GCL_EVENT_RECEIVER->ON_DOUBLE_CLICK_PO FOR GCL_ALV_GRID_PO.
      SET HANDLER GCL_EVENT_RECEIVER->ON_USER_COMMAND FOR GCL_ALV_GRID_PO.
    
      CALL METHOD GCL_ALV_GRID_PO->REGISTER_EDIT_EVENT( I_EVENT_ID = 19 ).
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_DELETE_UNUSED_BUTTON
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *&      <-- E_OBJECT
    *&---------------------------------------------------------------------*
    FORM FRM_DELETE_UNUSED_BUTTON  CHANGING E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET.
    
      DATA: LS_TOOLBAR TYPE STB_BUTTON,
            L_FLG_DEL  TYPE CHAR1.
    
      RANGES: LR_FUNCTION FOR STB_BUTTON-FUNCTION.
    
      FIELD-SYMBOLS: <LS_TOOLBAR>  TYPE STB_BUTTON.
    
      APPEND_RANGE: LR_FUNCTION '&DETAIL' '',
                    LR_FUNCTION '&SORT_ASC' '',
                    LR_FUNCTION '&SORT_DSC' '',
                    LR_FUNCTION '&FIND' '',
                    LR_FUNCTION '&FIND_MORE' '',
                    LR_FUNCTION '&MB_FILTER' '',
                    LR_FUNCTION '&MB_SUBTOT' '',
                    LR_FUNCTION '&MB_VARIANT' '',
                    LR_FUNCTION '&MB_SUM' '',
                    LR_FUNCTION '&MB_EXPORT' '',
                    LR_FUNCTION '&COL0' ''.
    
    * DELETE UNUSED FUNCTION BUTTON FROM STANDARD
      LOOP AT E_OBJECT->MT_TOOLBAR ASSIGNING <LS_TOOLBAR> .
        IF <LS_TOOLBAR>-FUNCTION NOT IN LR_FUNCTION.
          IF <LS_TOOLBAR>-BUTN_TYPE = 3 AND L_FLG_DEL = ''.
            L_FLG_DEL = 'X'.
            CONTINUE.
          ENDIF.
          DELETE E_OBJECT->MT_TOOLBAR INDEX SY-TABIX.
        ELSE.
          CLEAR L_FLG_DEL.
        ENDIF.
      ENDLOOP.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_ADD_TOOLBAR_BUTTON
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *&      <-- E_OBJECT
    *&---------------------------------------------------------------------*
    FORM FRM_ADD_TOOLBAR_BUTTON  CHANGING E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET.
    
      SET_BUTTON: E_OBJECT->MT_TOOLBAR 'SEL_ALL_H'  TEXT-037 0,
                  E_OBJECT->MT_TOOLBAR 'SEL_NULL_H' TEXT-038 0,
                  E_OBJECT->MT_TOOLBAR 'SEARCH'     TEXT-039 0.
    
      IF GV_CHANGE = 'X'.
        SET_BUTTON: E_OBJECT->MT_TOOLBAR 'QXFP' TEXT-040 0.
      ENDIF.
    
      SET_BUTTON: E_OBJECT->MT_TOOLBAR 'EXPORT_FP' TEXT-043 0.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form frm_add_toolbar_button_po
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *&      <-- E_OBJECT
    *&---------------------------------------------------------------------*
    FORM FRM_ADD_TOOLBAR_BUTTON_PO  CHANGING E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET.
    
      IF GV_CHANGE = 'X'.
        SET_BUTTON: E_OBJECT->MT_TOOLBAR 'SEL_ALL_P'   TEXT-037 0,
                    E_OBJECT->MT_TOOLBAR 'SEL_NULL_P'  TEXT-038 0,
                    E_OBJECT->MT_TOOLBAR 'RG'          TEXT-041 0,
                    E_OBJECT->MT_TOOLBAR 'AUTO'        TEXT-042 0.
    *                e_object->mt_toolbar 'MATCH_OK' '确认匹配' 0.
      ENDIF.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_DATA_CHANGE
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *&      --> ur_data_changed
    *&      --> u_onf4
    *&      --> u_onf4_before
    *&      --> u_onf4_after
    *&      --> u_ucomm
    *&---------------------------------------------------------------------*
    FORM FRM_DATA_CHANGE  USING UR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                U_ONF4
                                U_ONF4_BEFORE
                                U_ONF4_AFTER
                                U_UCOMM TYPE SY-UCOMM.
    
      DATA: LT_FTAB TYPE LVC_T_MODI,
            LS_FTAB TYPE LVC_S_MODI,
            LV_FLAG.
    
      DATA: LS_PO LIKE GS_PO_DATA.
    
      LT_FTAB = UR_DATA_CHANGED->MT_MOD_CELLS.
      LOOP AT LT_FTAB INTO LS_FTAB.
    ****未确认匹配量
        IF LS_FTAB-FIELDNAME = 'MATCH_MENGE'.
    *      检测是否为纯数字型
          PERFORM FRM_CHECK_NUM USING LS_FTAB-VALUE CHANGING LS_PO-MATCH_MENGE LV_FLAG.
          IF LV_FLAG = 'E'.
            EXIT.
          ENDIF.
    
          CLEAR GS_PO_DATA.
          READ TABLE GT_PO_SHOW INTO GS_PO_DATA INDEX LS_FTAB-ROW_ID.
    
    **剩余可用数量 = 采购订单数量 - 已收数量 - 确认预期交货量 - 未确认匹配量
    *      GS_PO_DATA-LEFT_QUAN = GS_PO_DATA-BASE_MENGE - GS_PO_DATA-ARRIVE_MENGE
    *                                              - GS_PO_DATA-CONFIRM_MENGE
    *                                              - LS_PO-MATCH_MENGE
    *                                              - GS_PO_DATA-ASSIGNED_MENGE .
    
          IF  GS_PO_DATA-LEFT_QUAN < LS_PO-MATCH_MENGE .
    *      IF GS_PO_DATA-LEFT_QUAN < 0.
    *设置数据修改时的报错信息
            PERFORM FRM_SET_DATA_CHANGE_MESSAGE USING UR_DATA_CHANGED '00' 'E' '001'
                                                        '未确认匹配量不能大于剩余可用数量'
                                                        ''
                                                        ''
                                                        ''
                                                        'MATCH_MENGE'
                                                        LS_FTAB-ROW_ID
                                                        LS_FTAB-TABIX
                                                        .
    
            LV_FLAG = 'E'.
    
    *      ELSE.
    *        CALL METHOD ur_data_changed->modify_cell
    *          EXPORTING
    *            i_row_id    = ls_ftab-row_id
    *            i_fieldname = 'LEFT_QUAN'
    *            i_value     = gs_po_data-left_quan.
          ENDIF.
    
          IF LV_FLAG = 'E'.
            CALL METHOD UR_DATA_CHANGED->MODIFY_CELL
              EXPORTING
                I_ROW_ID    = LS_FTAB-ROW_ID
                I_FIELDNAME = 'MATCH_MENGE'
                I_VALUE     = '0.000'.
          ENDIF.
    
        ENDIF.
      ENDLOOP.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_DATA_CHANGED_FINISHED
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *&      --> u_modified
    *&      --> ut_good_cells
    *&---------------------------------------------------------------------*
    FORM FRM_DATA_CHANGED_FINISHED  USING U_MODIFIED
                                          UT_GOOD_CELLS TYPE LVC_T_MODI.
    
      CHECK U_MODIFIED IS NOT INITIAL.
    
    ENDFORM.
    
    
    *&---------------------------------------------------------------------*
    *& Form frm_data_change_DETAIL
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *&      --> ur_data_changed
    *&      --> u_onf4
    *&      --> u_onf4_before
    *&      --> u_onf4_after
    *&      --> u_ucomm
    *&---------------------------------------------------------------------*
    FORM FRM_DATA_CHANGE_DETAIL  USING UR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                U_ONF4
                                U_ONF4_BEFORE
                                U_ONF4_AFTER
                                U_UCOMM TYPE SY-UCOMM.
    
      DATA: LT_FTAB TYPE LVC_T_MODI,
            LS_FTAB TYPE LVC_S_MODI,
            LV_FLAG.
    
      DATA: LS_MA LIKE GS_MATCH_DATA.
    
      LT_FTAB = UR_DATA_CHANGED->MT_MOD_CELLS.
      LOOP AT LT_FTAB INTO LS_FTAB.
    ****未确认匹配量
        IF LS_FTAB-FIELDNAME = 'MATCH_MENGE'.
    *      检测是否为纯数字型
          PERFORM FRM_CHECK_NUM USING LS_FTAB-VALUE CHANGING LS_MA-MATCH_MENGE LV_FLAG.
          IF LV_FLAG = 'E'.
            EXIT.
          ENDIF.
    
          CLEAR GS_MATCH_DATA.
          READ TABLE GT_MATCH_SHOW INTO GS_MATCH_DATA INDEX LS_FTAB-ROW_ID.
          READ TABLE GT_PO_DATA INTO GS_PO_DATA WITH KEY EBELN = GS_MATCH_DATA-EBELN
                                                         EBELP = GS_MATCH_DATA-EBELP.
    
          GS_PO_DATA-LEFT_QUAN = GS_PO_DATA-LEFT_QUAN + GS_MATCH_DATA-MATCH_MENGE - LS_MA-MATCH_MENGE.
    
          IF GS_PO_DATA-LEFT_QUAN < 0.
    *设置数据修改时的报错信息
            PERFORM FRM_SET_DATA_CHANGE_MESSAGE USING UR_DATA_CHANGED '00' 'E' '001'
                                                        '采购订单剩余可用量不足，请在外面进行修改'
                                                        ''
                                                        ''
                                                        ''
                                                        'MATCH_MENGE'
                                                        LS_FTAB-ROW_ID
                                                        LS_FTAB-TABIX
                                                        .
            LV_FLAG = 'E'.
    
          ELSE.
    
            READ TABLE GT_APS_PL INTO GS_APS_PL WITH KEY GUID = GS_MATCH_DATA-GUID.
            IF SY-SUBRC = 0.
              GS_APS_PL-SY_MENGE = GS_APS_PL-SY_MENGE + GS_MATCH_DATA-MATCH_MENGE - LS_MA-MATCH_MENGE.
              IF GS_APS_PL-SY_MENGE < 0.
    *设置数据修改时的报错信息
                PERFORM FRM_SET_DATA_CHANGE_MESSAGE USING UR_DATA_CHANGED '00' 'E' '001'
                                                            '分配数量已经超出APS需求数量，请在外面进行修改'
                                                            ''
                                                            ''
                                                            ''
                                                            'MATCH_MENGE'
                                                            LS_FTAB-ROW_ID
                                                            LS_FTAB-TABIX
                                                            .
                LV_FLAG = 'E'.
    
              ENDIF.
            ENDIF.
    
          ENDIF.
    
          IF LV_FLAG = 'E'.
            CALL METHOD UR_DATA_CHANGED->MODIFY_CELL
              EXPORTING
                I_ROW_ID    = LS_FTAB-ROW_ID
                I_FIELDNAME = 'MATCH_MENGE'
                I_VALUE     = GS_MATCH_DATA-MATCH_MENGE.
          ELSE.
            GS_MATCH_DATA-UPDATE_FLAG = 'X'.
            MODIFY GT_MATCH_SHOW FROM GS_MATCH_DATA INDEX LS_FTAB-ROW_ID.
          ENDIF.
    
        ENDIF.
      ENDLOOP.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form frm_data_changed_finished_D
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *&      --> u_modified
    *&      --> ut_good_cells
    *&---------------------------------------------------------------------*
    FORM FRM_DATA_CHANGED_FINISHED_D  USING U_MODIFIED
                                          UT_GOOD_CELLS TYPE LVC_T_MODI.
    
      CHECK U_MODIFIED IS NOT INITIAL.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_QXFP
    *&---------------------------------------------------------------------*
    *& 抬头取消分配
    *&---------------------------------------------------------------------*
    FORM FRM_QXFP .
    
      DATA: LV_QTY    LIKE EKPO-MENGE,
            LV_ANSWER,
            LV_TABIX  TYPE I.
    
    *检查APS是否有选择
      PERFORM FRM_CHECK_APS_SEL CHANGING LV_ANSWER.
      CHECK LV_ANSWER IS INITIAL.
    
    *弹出选择确认窗口
      PERFORM FRM_SHOWCONFIRM USING '确认?' '取消分配会清空所有选择的分配' '确认' '' 2 CHANGING LV_ANSWER.
      CHECK LV_ANSWER = '1'.
    
      CLEAR: GT_PO_SHOW[].
      LOOP AT GT_APS_PL INTO GS_APS_PL WHERE SEL = 'X' AND ASSIGNED_MENGE > 0.
        LOOP AT GT_MATCH_DATA INTO GS_MATCH_DATA WHERE GUID = GS_APS_PL-GUID AND RELEASED IS INITIAL.
    
          LV_TABIX = SY-TABIX.
    
          LV_QTY = 0 - GS_MATCH_DATA-MATCH_MENGE.
    
    *更新供应商送货数
          PERFORM FRM_SET_VONDER_QTY USING GS_MATCH_DATA-LIFNR LV_QTY CHANGING GS_APS_PL.
          GS_APS_PL-ASSIGNED_MENGE = GS_APS_PL-ASSIGNED_MENGE - GS_MATCH_DATA-MATCH_MENGE.
          GS_APS_PL-SY_MENGE = GS_APS_PL-SY_MENGE + GS_MATCH_DATA-MATCH_MENGE.
    
          READ TABLE GT_PO_DATA INTO GS_PO_DATA WITH KEY EBELN = GS_MATCH_DATA-EBELN EBELP = GS_MATCH_DATA-EBELP.
          IF SY-SUBRC = 0.
            GS_PO_DATA-ASSIGNED_MENGE = GS_PO_DATA-ASSIGNED_MENGE - GS_MATCH_DATA-MATCH_MENGE.
            GS_PO_DATA-LEFT_QUAN      = GS_PO_DATA-LEFT_QUAN + GS_MATCH_DATA-MATCH_MENGE.
            MODIFY GT_PO_DATA FROM GS_PO_DATA INDEX SY-TABIX.
          ENDIF.
    
          READ TABLE GT_MMAPS_TEMP INTO GS_MMAPS_TEMP WITH KEY GUID = GS_APS_PL-GUID
                                                               EBELN = GS_MATCH_DATA-EBELN
                                                               EBELP = GS_MATCH_DATA-EBELP.
          IF SY-SUBRC = 0.
            APPEND GS_MMAPS_TEMP TO GT_MMAPS_DEL.
            CLEAR  GS_MMAPS_TEMP.
          ENDIF.
    
          DELETE GT_MATCH_DATA INDEX LV_TABIX.
    
        ENDLOOP.
    
    *更新抬头供应商信息
        PERFORM FRM_MODIFY_VONDER_INFO CHANGING GS_APS_PL.
    
        MODIFY GT_APS_PL FROM GS_APS_PL.
        CLEAR  GS_APS_PL.
    
      ENDLOOP.
    
    *刷新分割屏幕高度
      PERFORM FRM_REFRESH_SPLIT_HEIGHT.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_EXPORT_FP
    *&---------------------------------------------------------------------*
    *& 导出已经分配的数据
    *&---------------------------------------------------------------------*
    FORM FRM_EXPORT_FP .
    
      DATA: LV_ANSWER.
    
    *检查APS是否有选择
      PERFORM FRM_CHECK_APS_SEL CHANGING LV_ANSWER.
      CHECK LV_ANSWER IS INITIAL.
    
    *弹出选择确认窗口
      PERFORM FRM_SHOWCONFIRM USING '导出已分配采购订单' '只会导出已经保存的数据，确认导出?' '确认' '' 2 CHANGING LV_ANSWER.
      CHECK LV_ANSWER = '1'.
    
    *导出临时表分配数据
      PERFORM FRM_EXPORT_MMAPS_TEMP.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_RGFP
    *&---------------------------------------------------------------------*
    *& 人工分配数量
    *&---------------------------------------------------------------------*
    FORM FRM_RGFP .
    
    *分配数量
      PERFORM FRM_ASSIGNE_MENGE USING 'RGFP'.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_CALL_DETAIL_ALV
    *&---------------------------------------------------------------------*
    *& 弹出分配详细ALV
    *&---------------------------------------------------------------------*
    *&      --> u_row
    *&      --> u_column
    *&      --> u_row_no
    *&      --> u_type       HEAD: APS上面点击打开    PO：采购订单明细上点击打开
    *&---------------------------------------------------------------------*
    FORM FRM_CALL_DETAIL_ALV  USING    U_ROW TYPE LVC_S_ROW
                                       U_COLUMN TYPE LVC_S_COL
                                       U_ROW_NO TYPE LVC_S_ROID
                                       U_TYPE.
    
      DATA: LS_MATCH_DATA LIKE GS_MATCH_DATA.
    
      DATA: LS_EDIT TYPE LVC_S_STYL,
            LT_EDIT TYPE LVC_T_STYL.
    
      DEFINE FILTE_SHOW_MATCH_DATA.
        CHECK &2 IS NOT INITIAL.
        DELETE gt_match_show WHERE lifnr <> &1.
      END-OF-DEFINITION.
    
      DEFINE SET_DISEDIT_STYLE.
        CLEAR: lt_edit[].
    
        ls_edit-fieldname = &1.
        ls_edit-style = cl_gui_alv_grid=>mc_style_disabled.
        ls_edit-style2 = space.
        ls_edit-style3 = space.
        ls_edit-style4 = space.
        ls_edit-maxlen = 13.
        INSERT ls_edit INTO TABLE lt_edit.
      END-OF-DEFINITION.
    
      CLEAR: GT_MATCH_SHOW[], GS_PDLV_DATA, GS_MATCH_DATA.
    
      SET_DISEDIT_STYLE 'MATCH_MENGE'.    "设置栏位不可编辑
    
      CASE U_TYPE.
        WHEN 'HEAD'.
    
          READ TABLE GT_APS_PL INTO GS_APS_PL INDEX U_ROW_NO-ROW_ID.
          CHECK SY-SUBRC = 0.
    
    *      IF u_column-fieldname <> 'ASSIGNED_MENGE'.
    *        set_disedit_style 'MATCH_MENGE'.    "设置栏位不可编辑
    *        LOOP AT gt_pdlv_data INTO gs_pdlv_data WHERE guid = gs_aps_pl-guid.
    *          MOVE-CORRESPONDING gs_pdlv_data TO gs_match_data.
    *          gs_match_data-match_menge = gs_pdlv_data-pdlv_menge.
    *
    *          gs_match_data-cell_styles = lt_edit[].
    *
    *          APPEND gs_match_data TO gt_match_show.
    *          CLEAR  gs_match_data.
    *        ENDLOOP.
    *      ENDIF.
    
          IF U_COLUMN-FIELDNAME <> 'PDLV_MENGE'.
    
            LOOP AT GT_MATCH_DATA INTO GS_MATCH_DATA WHERE GUID = GS_APS_PL-GUID.
              IF GS_MATCH_DATA-RELEASED <> '' OR GS_MATCH_DATA-PDLV_DATE IS NOT INITIAL.
                GS_MATCH_DATA-CELL_STYLES = LT_EDIT[].
              ENDIF.
              APPEND GS_MATCH_DATA TO GT_MATCH_SHOW.
              CLEAR  GS_MATCH_DATA.
            ENDLOOP.
    
          ENDIF.
    
          CASE U_COLUMN-FIELDNAME.
    
            WHEN 'ASSIGNED_MENGE'.
    
            WHEN 'VMENGE1'.
              FILTE_SHOW_MATCH_DATA GS_APS_PL-LIFNR1 GS_APS_PL-VMENGE1.
            WHEN 'VMENGE2'.
              FILTE_SHOW_MATCH_DATA GS_APS_PL-LIFNR2 GS_APS_PL-VMENGE2.
            WHEN 'VMENGE3'.
              FILTE_SHOW_MATCH_DATA GS_APS_PL-LIFNR3 GS_APS_PL-VMENGE3.
            WHEN 'VMENGE4'.
              FILTE_SHOW_MATCH_DATA GS_APS_PL-LIFNR4 GS_APS_PL-VMENGE4.
            WHEN OTHERS.
    
          ENDCASE.
    
        WHEN 'PO'.
          READ TABLE GT_PO_SHOW INTO GS_PO_DATA INDEX U_ROW_NO-ROW_ID.
          CHECK SY-SUBRC = 0.
          CASE U_COLUMN-FIELDNAME.
            WHEN 'EBELN'.
              PERFORM FRM_CALL_TCODE USING 'ME23N' 'BES' GS_PO_DATA-EBELN.
            WHEN 'ASSIGNED_MENGE'.
              LOOP AT GT_MATCH_DATA INTO GS_MATCH_DATA WHERE EBELN = GS_PO_DATA-EBELN
                                                          AND EBELP = GS_PO_DATA-EBELP
                                                          AND RELEASED = '' AND PDLV_DATE IS INITIAL.
                APPEND GS_MATCH_DATA TO GT_MATCH_SHOW.
                CLEAR  GS_MATCH_DATA.
              ENDLOOP.
            WHEN 'CONFIRM_MENGE'.
              LOOP AT GT_MATCH_DATA INTO GS_MATCH_DATA WHERE EBELN = GS_PO_DATA-EBELN
                                                          AND EBELP = GS_PO_DATA-EBELP
                                                          AND PDLV_DATE IS NOT INITIAL.
                GS_MATCH_DATA-CELL_STYLES = LT_EDIT[].
                APPEND GS_MATCH_DATA TO GT_MATCH_SHOW.
                CLEAR  GS_MATCH_DATA.
              ENDLOOP.
            WHEN OTHERS.
          ENDCASE.
    
      ENDCASE.
    
      CHECK GT_MATCH_SHOW[] IS NOT INITIAL.
    
      CALL SCREEN '0200' STARTING AT 10 2.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_DETAIL_ALV_INIT
    *&---------------------------------------------------------------------*
    *& 分配明细ALV控件初始化
    *&---------------------------------------------------------------------*
    *& -->  p1        text
    *& <--  p2        text
    *&---------------------------------------------------------------------*
    FORM FRM_DETAIL_ALV_INIT .
    
      CLEAR: GV_DETAIL_MODIFY.
    
      IF GCL_ALV_GRID_DETAIL IS NOT INITIAL.
        PERFORM FRM_REFRESH_ALV_DATA USING 'DETAIL'.
        EXIT.
      ENDIF.
    
    * Create Object
      CREATE OBJECT GCL_CONTAINER_DETAIL
        EXPORTING
          CONTAINER_NAME = 'C_DETAIL'.
    
      CREATE OBJECT GCL_ALV_GRID_DETAIL
        EXPORTING
          I_PARENT      = GCL_CONTAINER_DETAIL
          I_APPL_EVENTS = 'X'.
    
    *ALV布局
      PERFORM FRM_ALV_LIST_LAYOUT.
    
    * Get field for Display ALV
      PERFORM FRM_ALV_FILL_NAME USING GT_FIELDCAT_DETAIL 'DETAIL'.
    
      CALL METHOD GCL_ALV_GRID_DETAIL->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_LAYOUT                     = GS_LAYOUT_DETAIL
          IS_VARIANT                    = GS_VARIAT
          I_SAVE                        = 'A'
        CHANGING
          IT_FIELDCATALOG               = GT_FIELDCAT_DETAIL[]
          IT_OUTTAB                     = GT_MATCH_SHOW[]
        EXCEPTIONS
          INVALID_PARAMETER_COMBINATION = 1
          PROGRAM_ERROR                 = 2
          TOO_MANY_LINES                = 3
          OTHERS                        = 4.
    
      CREATE OBJECT GCL_EVENT_RECEIVER.
      SET HANDLER GCL_EVENT_RECEIVER->ON_ADD_TOOLBAR_DETAIL   FOR GCL_ALV_GRID_DETAIL.
      CALL METHOD GCL_ALV_GRID_DETAIL->SET_TOOLBAR_INTERACTIVE.
      SET HANDLER GCL_EVENT_RECEIVER->ON_DATA_CHANGED_DETAIL FOR GCL_ALV_GRID_DETAIL.
      SET HANDLER GCL_EVENT_RECEIVER->ON_DATA_CHANGED_FINISHED_D FOR GCL_ALV_GRID_DETAIL.
    *  SET HANDLER gcl_event_receiver->on_double_click_detail FOR gcl_alv_grid_detail.
    *  SET HANDLER gcl_event_receiver->on_user_command FOR gcl_alv_grid_detail.
    *
      CALL METHOD GCL_ALV_GRID_DETAIL->REGISTER_EDIT_EVENT( I_EVENT_ID = 19 ).
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_SET_VONDER_QTY
    *&---------------------------------------------------------------------*
    *& 匹配供应商送货数
    *&---------------------------------------------------------------------*
    *&      --> UV_LIFNR
    *&      --> UV_QTY           支持正负数
    *&      <-- CS_APS_PL
    *&---------------------------------------------------------------------*
    FORM FRM_SET_VONDER_QTY  USING    UV_LIFNR
                                      UV_QTY
                             CHANGING CS_APS_PL LIKE GS_APS_PL.
    
      DEFINE SET_QTY.
        IF &1 IS INITIAL.
          &1 = uv_lifnr.
          &3 = &3 + uv_qty.
          SELECT SINGLE name1 INTO &2 FROM lfa1 WHERE lifnr = uv_lifnr.
          EXIT.
        ELSE.
          IF &1 = uv_lifnr.
            &3 = &3 + uv_qty.
            EXIT.
          ENDIF.
        ENDIF.
      END-OF-DEFINITION.
    
      SET_QTY: CS_APS_PL-LIFNR1 CS_APS_PL-NAME1_1 CS_APS_PL-VMENGE1,
               CS_APS_PL-LIFNR2 CS_APS_PL-NAME1_2 CS_APS_PL-VMENGE2,
               CS_APS_PL-LIFNR3 CS_APS_PL-NAME1_3 CS_APS_PL-VMENGE3,
               CS_APS_PL-LIFNR4 CS_APS_PL-NAME1_4 CS_APS_PL-VMENGE4.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_CHANGE_FROM_DETAIL
    *&---------------------------------------------------------------------*
    *& 详细屏幕修改传输数据
    *&---------------------------------------------------------------------*
    FORM FRM_CHANGE_FROM_DETAIL .
    
      DATA: LS_MATCH_DATA LIKE GS_MATCH_DATA,
            LS_PO_DATA    LIKE GS_PO_DATA,
            LV_BHL        LIKE GS_MATCH_DATA-MATCH_MENGE.
    
      LOOP AT GT_MATCH_SHOW INTO GS_MATCH_DATA WHERE UPDATE_FLAG = 'X'.
    
        CLEAR LV_BHL.
    
    *更新匹配内表数据
        READ TABLE GT_MATCH_DATA INTO LS_MATCH_DATA WITH KEY EBELN = GS_MATCH_DATA-EBELN
                                                             EBELP = GS_MATCH_DATA-EBELP
                                                              GUID = GS_MATCH_DATA-GUID
                                                          RELEASED = ''.
        IF GS_MATCH_DATA-MATCH_MENGE = 0.
          DELETE GT_MATCH_DATA INDEX SY-TABIX.
    
          READ TABLE GT_MMAPS_TEMP INTO GS_MMAPS_TEMP WITH KEY EBELN = GS_MATCH_DATA-EBELN
                                                               EBELP = GS_MATCH_DATA-EBELP
                                                                GUID = GS_MATCH_DATA-GUID
                                                               RELEASED = ''.
          IF SY-SUBRC = 0.
            APPEND GS_MMAPS_TEMP TO GT_MMAPS_DEL.
            CLEAR  GS_MMAPS_TEMP.
          ENDIF.
        ELSE.
          MODIFY GT_MATCH_DATA FROM GS_MATCH_DATA INDEX SY-TABIX.
        ENDIF.
    
    *更新采购订单数据
        LV_BHL = GS_MATCH_DATA-MATCH_MENGE - LS_MATCH_DATA-MATCH_MENGE.    "变化量 = 屏幕显示的数量 - 原来的数量
    
        READ TABLE GT_PO_DATA INTO GS_PO_DATA WITH KEY EBELN = GS_MATCH_DATA-EBELN
                                                       EBELP = GS_MATCH_DATA-EBELP.
        GS_PO_DATA-ASSIGNED_MENGE = GS_PO_DATA-ASSIGNED_MENGE + LV_BHL.
        GS_PO_DATA-LEFT_QUAN = GS_PO_DATA-LEFT_QUAN - LV_BHL.
        MODIFY GT_PO_DATA FROM GS_PO_DATA INDEX SY-TABIX.
    
    *更新显示的采购订单数据
        READ TABLE GT_PO_SHOW INTO LS_PO_DATA WITH KEY EBELN = GS_MATCH_DATA-EBELN
                                                       EBELP = GS_MATCH_DATA-EBELP.
        IF SY-SUBRC = 0.
          LS_PO_DATA-LEFT_QUAN = GS_PO_DATA-LEFT_QUAN.
          LS_PO_DATA-ASSIGNED_MENGE = GS_PO_DATA-ASSIGNED_MENGE.
          MODIFY GT_PO_SHOW FROM LS_PO_DATA INDEX SY-TABIX.
        ENDIF.
    
    *更新抬头信息
        READ TABLE GT_APS_PL INTO GS_APS_PL WITH KEY GUID = GS_MATCH_DATA-GUID.
        GS_APS_PL-SY_MENGE = GS_APS_PL-SY_MENGE - LV_BHL.
    
    *匹配供应商送货数
        PERFORM FRM_SET_VONDER_QTY USING GS_MATCH_DATA-LIFNR LV_BHL CHANGING GS_APS_PL.
    
    *更新抬头供应商信息
        PERFORM FRM_MODIFY_VONDER_INFO CHANGING GS_APS_PL.
    
    *更新匹配未确定量
        GS_APS_PL-ASSIGNED_MENGE = GS_APS_PL-ASSIGNED_MENGE + LV_BHL.
        MODIFY GT_APS_PL FROM GS_APS_PL INDEX SY-TABIX.
    
      ENDLOOP.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_MODIFY_VONDER_INFO
    *&---------------------------------------------------------------------*
    *& 更新抬头供应商信息
    *&---------------------------------------------------------------------*
    *&      <-- CS_APS_PL
    *&---------------------------------------------------------------------*
    FORM FRM_MODIFY_VONDER_INFO  CHANGING CS_APS_PL LIKE GS_APS_PL.
    
      DEFINE CLEAN_VONDER_INFO.
        IF &3 IS INITIAL.
          CLEAR: &1, &2.
        ENDIF.
      END-OF-DEFINITION.
    
      CLEAN_VONDER_INFO: CS_APS_PL-LIFNR1 CS_APS_PL-NAME1_1 CS_APS_PL-VMENGE1,
                         CS_APS_PL-LIFNR2 CS_APS_PL-NAME1_2 CS_APS_PL-VMENGE2,
                         CS_APS_PL-LIFNR3 CS_APS_PL-NAME1_3 CS_APS_PL-VMENGE3,
                         CS_APS_PL-LIFNR4 CS_APS_PL-NAME1_4 CS_APS_PL-VMENGE4.
    
    ENDFORM.
    
    *******弹出选择框
    FORM FRM_SHOWCONFIRM USING U_TITLE U_QEUSTION U_YES U_NO U_DEFAULT CHANGING C_ANSWER.
    
      DATA: LV_NO(10).
    
      CLEAR: C_ANSWER.
    
      IF U_NO IS INITIAL.
        LV_NO = '关闭'.
      ELSE.
        LV_NO = U_NO.
      ENDIF.
    
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR       = U_TITLE
          TEXT_QUESTION  = U_QEUSTION
          TEXT_BUTTON_1  = U_YES
          TEXT_BUTTON_2  = LV_NO
          DEFAULT_BUTTON = U_DEFAULT
    *     DISPLAY_CANCEL_BUTTON       = 'X'
    *     USERDEFINED_F1_HELP         = ' '
          START_COLUMN   = 25
          START_ROW      = 6
    *     POPUP_TYPE     =
    *     IV_QUICKINFO_BUTTON_1       = ' '
    *     IV_QUICKINFO_BUTTON_2       = ' '
        IMPORTING
          ANSWER         = C_ANSWER
    *     TABLES
    *     PARAMETER      =
        EXCEPTIONS
          TEXT_NOT_FOUND = 1
          OTHERS         = 2.
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_AUTO
    *&---------------------------------------------------------------------*
    *& PO自动分配数量
    *&---------------------------------------------------------------------*
    FORM FRM_AUTO .
    *分配数量
      PERFORM FRM_ASSIGNE_MENGE USING 'AUTO'.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_ASSIGNE_MENGE
    *&---------------------------------------------------------------------*
    *& 分配数量
    *&---------------------------------------------------------------------*
    *&      --> U_TYPE
    *&---------------------------------------------------------------------*
    FORM FRM_ASSIGNE_MENGE  USING U_TYPE.
    
      DATA: LS_PO_DATA LIKE GS_PO_DATA,
            LV_QTY     LIKE GS_PO_DATA-MENGE,
            LV_QTY2    LIKE GS_PO_DATA-MENGE, "本次预计分配数量
            LV_CZ_QTY  LIKE GS_PO_DATA-MENGE.
    
      CHECK GT_PO_SHOW[] IS NOT INITIAL.
    
      READ TABLE GT_PO_SHOW INTO GS_PO_DATA WITH KEY SEL = 'X'.
      IF SY-SUBRC <> 0.
        MESSAGE '请至少选择一条采购订单' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    
      LOOP AT GT_PO_SHOW INTO GS_PO_DATA WHERE SEL = 'X'.
    
        CLEAR LV_QTY.
    
        LS_PO_DATA = GS_PO_DATA.
    
        IF U_TYPE = 'RGFP'.
          LV_QTY = GS_PO_DATA-MATCH_MENGE.
        ELSEIF U_TYPE = 'AUTO'.
          LV_QTY = GS_PO_DATA-LEFT_QUAN.
        ENDIF.
        LV_QTY2 = LV_QTY .
    
        LOOP AT GT_APS_PL INTO GS_APS_PL WHERE SEL = 'X'
                                            AND KEY2 = GS_PO_DATA-KEY2
                                            AND SY_MENGE > 0.
    
          CLEAR  GS_MATCH_DATA.
    
          CHECK LV_QTY > 0.
    
          IF LV_QTY >= GS_APS_PL-SY_MENGE.
    
            READ TABLE GT_MATCH_DATA INTO GS_MATCH_DATA WITH KEY  GUID = GS_APS_PL-GUID
                                                                 UNIQUEID = GS_PO_DATA-UNIQUEID.
            IF SY-SUBRC = 0.
              IF GS_MATCH_DATA-RELEASED = '' AND GS_MATCH_DATA-PDLV_DATE IS INITIAL.
                GS_MATCH_DATA-MATCH_MENGE = GS_MATCH_DATA-MATCH_MENGE + GS_APS_PL-SY_MENGE.
                LV_CZ_QTY = GS_APS_PL-SY_MENGE.
                MODIFY GT_MATCH_DATA FROM GS_MATCH_DATA INDEX SY-TABIX.
              ELSE.
                CONTINUE.
              ENDIF.
            ELSE.
              MOVE-CORRESPONDING GS_PO_DATA TO GS_MATCH_DATA.
              GS_MATCH_DATA-MATCH_MENGE = GS_APS_PL-SY_MENGE.
    *          gs_match_data-pdlv_date = gs_aps_pl-reqdate.
              GS_MATCH_DATA-REQDATE = GS_APS_PL-REQDATE.
              GS_MATCH_DATA-KEY-REQDATE = GS_APS_PL-REQDATE.
              GS_MATCH_DATA-GUID = GS_APS_PL-GUID.
              GS_MATCH_DATA-VERSION = GS_APS_PL-VERSION.
              APPEND GS_MATCH_DATA TO GT_MATCH_DATA.
              LV_CZ_QTY = GS_MATCH_DATA-MATCH_MENGE.
            ENDIF.
    
            LV_QTY = LV_QTY - GS_APS_PL-SY_MENGE.
            GS_APS_PL-SY_MENGE = 0.
    
          ELSEIF LV_QTY <= GS_APS_PL-SY_MENGE.
    
            READ TABLE GT_MATCH_DATA INTO GS_MATCH_DATA WITH KEY GUID = GS_APS_PL-GUID
                                                                 UNIQUEID = GS_PO_DATA-UNIQUEID.
            IF SY-SUBRC = 0.
    *如果已经有下达的匹配项，则跳过
              IF GS_MATCH_DATA-RELEASED = '' AND GS_MATCH_DATA-PDLV_DATE IS INITIAL.
                GS_MATCH_DATA-MATCH_MENGE = GS_MATCH_DATA-MATCH_MENGE + LV_QTY.
                LV_CZ_QTY = LV_QTY.
                MODIFY GT_MATCH_DATA FROM GS_MATCH_DATA INDEX SY-TABIX.
              ELSE.
                CONTINUE.
              ENDIF.
            ELSE.
              MOVE-CORRESPONDING GS_PO_DATA TO GS_MATCH_DATA.
              GS_MATCH_DATA-KEY-REQDATE = GS_APS_PL-REQDATE.
              GS_MATCH_DATA-GUID = GS_APS_PL-GUID.
    *          gs_match_data-pdlv_date = gs_aps_pl-reqdate.
              GS_MATCH_DATA-REQDATE = GS_APS_PL-REQDATE.
              GS_MATCH_DATA-VERSION = GS_APS_PL-VERSION.
              GS_MATCH_DATA-MATCH_MENGE = LV_QTY.
              APPEND GS_MATCH_DATA TO GT_MATCH_DATA.
              LV_CZ_QTY = GS_MATCH_DATA-MATCH_MENGE.
            ENDIF.
    
            GS_APS_PL-SY_MENGE = GS_APS_PL-SY_MENGE - LV_CZ_QTY.
            LV_QTY = 0.
    
          ENDIF.
    
    *匹配供应商送货数
          PERFORM FRM_SET_VONDER_QTY USING GS_PO_DATA-LIFNR LV_CZ_QTY CHANGING GS_APS_PL.
    
    *更新匹配未确定量
          GS_APS_PL-ASSIGNED_MENGE = GS_APS_PL-ASSIGNED_MENGE + LV_CZ_QTY.
    
          MODIFY GT_APS_PL FROM GS_APS_PL.
          CLEAR  GS_APS_PL.
    
          IF LV_QTY = 0.
            EXIT.
          ENDIF.
    
        ENDLOOP.
        IF SY-SUBRC <> 0.
          IF U_TYPE = 'RGFP'.
            LV_QTY = GS_PO_DATA-MATCH_MENGE.
          ELSEIF U_TYPE = 'AUTO'.
            LV_QTY = GS_PO_DATA-LEFT_QUAN.
          ENDIF.
        ENDIF.
    
        IF U_TYPE = 'RGFP'.
          GS_PO_DATA-MATCH_MENGE = LV_QTY.
          GS_PO_DATA-ASSIGNED_MENGE = GS_PO_DATA-ASSIGNED_MENGE + LS_PO_DATA-MATCH_MENGE - GS_PO_DATA-MATCH_MENGE.
          GS_PO_DATA-LEFT_QUAN = GS_PO_DATA-LEFT_QUAN - LS_PO_DATA-MATCH_MENGE + GS_PO_DATA-MATCH_MENGE.
        ELSEIF U_TYPE = 'AUTO'.
          "UP BY LANJINGCUN 20200522 自动分配计算逻辑不正确处理-start
    *      GS_PO_DATA-LEFT_QUAN = LV_QTY.
    *      GS_PO_DATA-ASSIGNED_MENGE = GS_PO_DATA-ASSIGNED_MENGE + LS_PO_DATA-LEFT_QUAN - GS_PO_DATA-LEFT_QUAN.
          GS_PO_DATA-MATCH_MENGE = LV_QTY.
          GS_PO_DATA-ASSIGNED_MENGE = GS_PO_DATA-ASSIGNED_MENGE + LV_QTY2 - LV_QTY.
          GS_PO_DATA-LEFT_QUAN = LV_QTY.
          "UP BY LANJINGCUN 20200522 自动分配计算逻辑不正确处理-END
        ENDIF.
    
        MODIFY GT_PO_SHOW FROM GS_PO_DATA.
        CLEAR  GS_PO_DATA.
    
      ENDLOOP.
    
    *把结果更新到总表
      LOOP AT GT_PO_SHOW INTO GS_PO_DATA WHERE SEL = 'X'.
    
        READ TABLE GT_PO_DATA TRANSPORTING NO FIELDS WITH KEY UNIQUEID = GS_PO_DATA-UNIQUEID.
    
        MODIFY GT_PO_DATA FROM GS_PO_DATA INDEX SY-TABIX.
      ENDLOOP.
    
      MESSAGE '自动匹配完成' TYPE 'I' .
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_SAVE_DATA
    *&---------------------------------------------------------------------*
    *& 保存数据
    *&---------------------------------------------------------------------*
    FORM FRM_SAVE_DATA .
    
      DATA: LV_ANSWER,
            LV_FLAG,
            LT_MMAPS_TEMP LIKE TABLE OF ZMMAPS_TEMP,
            LS_MMAPS_TEMP LIKE ZMMAPS_TEMP,
            LT_ZMMAPS_PL  LIKE GT_APS_PL,
            LS_ZMMAPS_PL  LIKE GS_APS_PL.
    
      DATA(LT_MATCH) = GT_MATCH_DATA[].
    
      LOOP AT LT_MATCH INTO DATA(LS_MATCH).
        DATA(LV_TABIX) = SY-TABIX.
        READ TABLE GT_MMAPS_TEMP INTO GS_MMAPS_TEMP WITH KEY  GUID = LS_MATCH-GUID
                                                              UNIQUEID = LS_MATCH-UNIQUEID
                                                              ASSIGN_MENGE = LS_MATCH-MATCH_MENGE.
        IF SY-SUBRC = 0.
          DELETE LT_MATCH INDEX LV_TABIX.
        ENDIF.
      ENDLOOP.
    
      IF LT_MATCH[] IS INITIAL AND GT_MMAPS_DEL[] IS INITIAL.
        MESSAGE '没有要保存的数据' TYPE 'I' DISPLAY LIKE 'S'.
        EXIT.
      ENDIF.
    
      PERFORM FRM_SHOWCONFIRM USING '保存确认?' '保存这次的分配' '确认' '' 2 CHANGING LV_ANSWER.
      CHECK LV_ANSWER = '1'.
    
      IF GT_MMAPS_DEL[] IS NOT INITIAL.
        DELETE ZMMAPS_TEMP FROM TABLE GT_MMAPS_DEL[].
    
    *是否需要更新APS状态
        LOOP AT GT_MMAPS_DEL INTO GS_MMAPS_TEMP.
    
          READ TABLE GT_APS_PL INTO GS_APS_PL WITH KEY GUID = GS_MMAPS_TEMP-GUID.
          IF SY-SUBRC = 0 AND GS_APS_PL-ZPR_STS = 'M'.
            READ TABLE GT_MATCH_DATA TRANSPORTING NO FIELDS WITH KEY GUID = GS_MMAPS_TEMP-GUID.
            IF SY-SUBRC = 0.
              CONTINUE.
            ENDIF.
            READ TABLE LT_ZMMAPS_PL TRANSPORTING NO FIELDS WITH KEY GUID = GS_MMAPS_TEMP-GUID.
            IF SY-SUBRC <> 0.
              CLEAR  GS_APS_PL-ZPR_STS.
              APPEND GS_APS_PL TO LT_ZMMAPS_PL.
              CLEAR  GS_APS_PL.
            ENDIF.
          ENDIF.
    
        ENDLOOP.
    
      ENDIF.
    
      LOOP AT GT_MATCH_DATA INTO GS_MATCH_DATA.
        MOVE-CORRESPONDING GS_MATCH_DATA TO LS_MMAPS_TEMP.
    
        READ TABLE GT_MMAPS_TEMP INTO GS_MMAPS_TEMP WITH KEY GUID = GS_MATCH_DATA-GUID
                                                         UNIQUEID = LS_MMAPS_TEMP-UNIQUEID.
        IF SY-SUBRC = 0 AND GS_MMAPS_TEMP-ASSIGN_MENGE = GS_MATCH_DATA-MATCH_MENGE.
          CONTINUE.
        ENDIF.
    
        CLEAR LS_MMAPS_TEMP-INACTIVE .
        LS_MMAPS_TEMP-SEND_DATE = SY-DATUM.
        LS_MMAPS_TEMP-ASSIGN_MENGE = GS_MATCH_DATA-MATCH_MENGE.
    
        READ TABLE GT_APS_PL INTO GS_APS_PL WITH KEY GUID = GS_MATCH_DATA-GUID.
        IF SY-SUBRC = 0.
          LS_MMAPS_TEMP-LGORT = GS_APS_PL-LGORT.
          LS_MMAPS_TEMP-REQDATE = GS_APS_PL-REQDATE.
    
    *是否需要更新APS状态
          IF GS_APS_PL-ZPR_STS = ''.
            READ TABLE LT_ZMMAPS_PL INTO LS_ZMMAPS_PL WITH KEY GUID = GS_MATCH_DATA-GUID.
            IF SY-SUBRC = 0.
              IF LS_ZMMAPS_PL-ZPR_STS = ''.
                DELETE LT_ZMMAPS_PL INDEX SY-TABIX.
              ENDIF.
            ELSE.
              GS_APS_PL-ZPR_STS = 'M'.
              APPEND GS_APS_PL TO LT_ZMMAPS_PL.
              CLEAR  GS_APS_PL.
            ENDIF.
    
          ENDIF.
        ENDIF.
    
        APPEND LS_MMAPS_TEMP TO LT_MMAPS_TEMP.
        CLEAR  LS_MMAPS_TEMP.
    
      ENDLOOP.
    
      IF LT_MMAPS_TEMP[] IS NOT INITIAL.
    *    BREAK liujianmin.
        MODIFY ZMMAPS_TEMP FROM TABLE LT_MMAPS_TEMP.
      ENDIF.
    
      IF LT_ZMMAPS_PL[] IS NOT INITIAL.
        LOOP AT LT_ZMMAPS_PL INTO GS_APS_PL.
    
          UPDATE ZMMAPS_PL
             SET ZPR_STS = GS_APS_PL-ZPR_STS
           WHERE WERKS = GS_APS_PL-WERKS
             AND LGORT = GS_APS_PL-LGORT
             AND REQDATE = GS_APS_PL-REQDATE
             AND MATNR = GS_APS_PL-MATNR
             AND ST_CODE = GS_APS_PL-ST_CODE
             AND ZPR_STS <> 'C'.
    
        ENDLOOP.
      ENDIF.
    
      COMMIT WORK.
    
      PERFORM FRM_LEAVE_SCREEN.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_SPLIT_INIT
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    FORM FRM_SPLIT_INIT .
    
      IF GCL_CONTAINER_SPLIT IS INITIAL.
    
        CREATE OBJECT GCL_CONTAINER_SPLIT
          EXPORTING
            CONTAINER_NAME = 'C_SPLIT'.
    
        " Create SPLITTER and SPLITTER panes
        CREATE OBJECT GCL_SPLITTER
          EXPORTING
            PARENT  = GCL_CONTAINER_SPLIT
            ROWS    = 2
            COLUMNS = 1.
    
    *设置分割屏幕高度
        PERFORM FRM_SET_SPLIT_SCREEN_HEIGHT USING 1 GV_SPLIT_HEIGHT.
        PERFORM FRM_SET_SPLIT_SCREEN_HEIGHT USING 2 0.
    
      ENDIF.
    
      PERFORM FRM_HEAD_ALV_INIT .
      PERFORM FRM_PO_ALV_INIT.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_SEARCH_FROM_HEAD
    *&---------------------------------------------------------------------*
    *& 根据抬头选择筛选PO数据
    *&---------------------------------------------------------------------*
    FORM FRM_SEARCH_FROM_HEAD .
    
      CLEAR: GT_PO_SHOW[].
      LOOP AT GT_APS_PL INTO GS_APS_PL WHERE SEL = 'X'.
        READ TABLE GT_PO_SHOW TRANSPORTING NO FIELDS WITH KEY KEY2 = GS_APS_PL-KEY2.
        IF SY-SUBRC <> 0.
          LOOP AT GT_PO_DATA INTO GS_PO_DATA WHERE KEY2 = GS_APS_PL-KEY2.
            GS_PO_DATA-MAKTX = GS_APS_PL-MAKTX.
            APPEND GS_PO_DATA TO GT_PO_SHOW.
            CLEAR  GS_PO_DATA.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    
    *刷新分割屏幕高度
      PERFORM FRM_REFRESH_SPLIT_HEIGHT.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_SET_SPLIT_SCREEN_HEIGHT
    *&---------------------------------------------------------------------*
    *& 设置分割屏幕高度
    *&---------------------------------------------------------------------*
    *&      --> UV_ID
    *&      --> UV_HEIGHT
    *&---------------------------------------------------------------------*
    FORM FRM_SET_SPLIT_SCREEN_HEIGHT  USING UV_ID
                                            UV_HEIGHT.
    
      CALL METHOD GCL_SPLITTER->SET_ROW_HEIGHT
        EXPORTING
          ID                = UV_ID
          HEIGHT            = UV_HEIGHT
    *    IMPORTING
    *     result            =
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3.
      IF SY-SUBRC <> 0.
    *   Implement suitable error handling here
      ENDIF.
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_CHECK_APS_SEL
    *&---------------------------------------------------------------------*
    *& 检查APS是否有选择
    *&---------------------------------------------------------------------*
    *&      <-- CV_OK
    *&---------------------------------------------------------------------*
    FORM FRM_CHECK_APS_SEL  CHANGING CV_OK.
    
      CLEAR CV_OK.
    
      READ TABLE GT_APS_PL INTO GS_APS_PL WITH KEY SEL = 'X'.
      IF SY-SUBRC <> 0.
        MESSAGE '请至少选择一条记录' TYPE 'I' DISPLAY LIKE 'E'.
        CV_OK = 'E'.
      ENDIF.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_EXPORT_MMAPS_TEMP
    *&---------------------------------------------------------------------*
    *& 导出临时表分配数据
    *&---------------------------------------------------------------------*
    FORM FRM_EXPORT_MMAPS_TEMP .
    
      DATA: LT_APS_PL LIKE GT_APS_PL,
            LS_APS_PL LIKE GS_APS_PL.
    
      LOOP AT GT_APS_PL INTO LS_APS_PL WHERE SEL = 'X'.
        APPEND LS_APS_PL TO LT_APS_PL.
      ENDLOOP.
    
      CHECK LT_APS_PL[] IS NOT INITIAL.
    
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE GT_TEMP_EXPORT
        FROM ZMMAPS_TEMP
         FOR ALL ENTRIES IN LT_APS_PL
       WHERE GUID = LT_APS_PL-GUID
         AND PDLV_DATE = '00000000'
         AND ASSIGN_MENGE > 0
         AND INACTIVE = ''
        .
    
      IF GT_TEMP_EXPORT[] IS INITIAL.
        MESSAGE '选择的APS还没有保存的分配' TYPE 'I'.
        EXIT.
      ENDIF.
    
    *  CALL SELECTION-SCREEN 0300 STARTING AT 10 5.
    *  IF sy-subrc EQ 0.
    *  PERFORM frm_get_structure USING 'ZMMAPS_TEMP'.
      PERFORM FRM_SAVE_EXCEL USING ''.
    *  ENDIF.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_UNIT_CONVERT
    *&---------------------------------------------------------------------*
    *& 获取单位数量转换
    *&---------------------------------------------------------------------*
    *&      --> UV_MATNR
    *&      --> UV_UNIT_IN
    *&      --> UV_UNIT_OUT
    *&      --> UV_QTY
    *&      <-- CV_QTY
    *&---------------------------------------------------------------------*
    FORM FRM_GET_UNIT_CONVERT  USING    UV_MATNR
                                        UV_UNIT_IN
                                        UV_UNIT_OUT
                                        UV_QTY
                               CHANGING CV_QTY.
    
      IF UV_UNIT_IN = UV_UNIT_OUT.
        CV_QTY = UV_QTY.
        EXIT.
      ENDIF.
    
      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          I_MATNR              = UV_MATNR
          I_IN_ME              = UV_UNIT_IN
          I_OUT_ME             = UV_UNIT_OUT
          I_MENGE              = UV_QTY
        IMPORTING
          E_MENGE              = CV_QTY
        EXCEPTIONS
          ERROR_IN_APPLICATION = 1
          ERROR                = 2
          OTHERS               = 3.
      IF SY-SUBRC <> 0.
        CV_QTY = UV_QTY.
      ENDIF.
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_REFRESH_SPLIT_HEIGHT
    *&---------------------------------------------------------------------*
    *& 刷新分割屏幕高度
    *&---------------------------------------------------------------------*
    FORM FRM_REFRESH_SPLIT_HEIGHT .
    
      DATA: LV_SP1 TYPE I.
    
      CHECK P_R02 IS NOT INITIAL.
    
      IF GT_PO_SHOW[] IS NOT INITIAL.
    
        LV_SP1 = GV_SPLIT_HEIGHT / 2.
    
    *设置分割屏幕高度
        PERFORM FRM_SET_SPLIT_SCREEN_HEIGHT USING 1 LV_SP1.
        PERFORM FRM_SET_SPLIT_SCREEN_HEIGHT USING 2 LV_SP1.
    
      ELSE.
    *设置分割屏幕高度
        PERFORM FRM_SET_SPLIT_SCREEN_HEIGHT USING 1 GV_SPLIT_HEIGHT.
        PERFORM FRM_SET_SPLIT_SCREEN_HEIGHT USING 2 0.
      ENDIF.
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *&      Form  FRM_CHANGE_SCREEN
    *&---------------------------------------------------------------------*
    *       修改选择屏幕
    *----------------------------------------------------------------------*
    FORM FRM_CHANGE_SCREEN .
    
      LOOP AT SCREEN .
        CASE SCREEN-GROUP1.
          WHEN 'M1'.
            IF P_R02 = 'X'.
              SCREEN-ACTIVE = '1'.
            ELSE.
              SCREEN-ACTIVE = '0'.
            ENDIF.
          WHEN 'M5'.
            IF P_R05 = 'X'.
              SCREEN-ACTIVE = '1'.
            ELSE.
              SCREEN-ACTIVE = '0'.
            ENDIF.
          WHEN 'M6'.
            IF P_R06 = 'X'.
              SCREEN-ACTIVE = '1'.
            ELSE.
              SCREEN-ACTIVE = '0'.
            ENDIF.
        ENDCASE.
    
    
    
        MODIFY SCREEN.
      ENDLOOP.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_MARC
    *&---------------------------------------------------------------------*
    *& 采购组信息
    *&---------------------------------------------------------------------*
    FORM FRM_GET_MARC .
    
      DATA: LT_PDLV LIKE GT_PDLV_DATA.
    
      LT_PDLV[] = GT_PDLV_DATA[].
      DELETE LT_PDLV WHERE MATNR IS INITIAL OR WERKS IS INITIAL.
      SORT LT_PDLV BY MATNR WERKS.
      DELETE ADJACENT DUPLICATES FROM LT_PDLV COMPARING MATNR WERKS.
    
      SELECT MATNR WERKS EKGRP
        INTO CORRESPONDING FIELDS OF TABLE GT_MARC
        FROM MARC
         FOR ALL ENTRIES IN LT_PDLV
       WHERE MATNR = LT_PDLV-MATNR
         AND WERKS = LT_PDLV-WERKS.
    
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_CALL_TCODE
    *&---------------------------------------------------------------------*
    *& 跳转TCODE
    *&---------------------------------------------------------------------*
    *&      --> UV_TCODE
    *&      --> UV_ID
    *&      --> UV_VALUE
    *&---------------------------------------------------------------------*
    FORM FRM_CALL_TCODE  USING    UV_TCODE
                                  UV_ID
                                  UV_VALUE.
    
      CHECK UV_VALUE IS NOT INITIAL.
    
    *设置对应屏幕里的ID值
      SET PARAMETER ID: UV_ID FIELD UV_VALUE.
    *跳转T-CODE，并跳到第FIRST屏幕
      CALL TRANSACTION UV_TCODE AND SKIP FIRST SCREEN.
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_PDLV_RECEIVED_QUAN
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    FORM FRM_GET_PDLV_RECEIVED_QUAN .
      "UP DATA BY LANJINGCUN 20200420  更新获取收货数量数据(R6 供应商送货计划维护)-start
    *  DATA:LT_PLANNO_ITEM  LIKE TABLE OF GS_PLANNO_ITEM,
    *       LT_PLANNO_MENGE LIKE TABLE OF GS_PLANNO_MENGE,
    *       LT_EBE          LIKE TABLE OF GS_EBE,
    *       LT_EBE_MENGE    LIKE TABLE OF GS_EBE_MENGE.
    *  CHECK GT_PDLV_DATA[] IS NOT INITIAL.
    *  "存在计划交货行则根据计划交货行从ZMATDOC获取已交货数量，否则根据采购订单行从MATDOC获取已交货数量
    *  LOOP AT GT_PDLV_DATA INTO GS_PDLV_DATA_PO.
    *    LT_EBE = VALUE #( BASE LT_EBE (  EBELN = GS_PDLV_DATA_PO-EBELN   EBELP = GS_PDLV_DATA_PO-EBELP ) ).
    *    IF GS_PDLV_DATA_PO-DLV_PLANNO IS NOT INITIAL AND GS_PDLV_DATA_PO-DLV_ITEMNO IS NOT INITIAL.
    *      LT_PLANNO_ITEM = VALUE #( BASE LT_PLANNO_ITEM ( DLV_PLANNO  = GS_PDLV_DATA_PO-DLV_PLANNO  DLV_ITEMNO = GS_PDLV_DATA_PO-DLV_ITEMNO ) ).
    *    ENDIF.
    *  ENDLOOP.
    *  DELETE LT_EBE WHERE EBELN IS INITIAL OR EBELP IS INITIAL.
    *  DELETE LT_PLANNO_ITEM WHERE DLV_PLANNO IS INITIAL OR DLV_ITEMNO IS INITIAL.
    *  SORT LT_EBE BY EBELN EBELP.
    *  DELETE ADJACENT DUPLICATES FROM LT_EBE COMPARING EBELN EBELP.
    *  SORT  LT_PLANNO_ITEM BY DLV_PLANNO DLV_ITEMNO.
    *  DELETE ADJACENT DUPLICATES FROM LT_PLANNO_ITEM COMPARING DLV_PLANNO DLV_ITEMNO.
    *  "根据采购订单行从MATDOC获取已交货数量
    *  SELECT EB~EBELN,EB~EBELP,
    *        SUM( CASE MATDOC~BWART WHEN '101' THEN MATDOC~MENGE ELSE 0 END ) AS MENGE101,
    *        SUM( CASE MATDOC~BWART WHEN '102' THEN MATDOC~MENGE ELSE 0 END ) AS MENGE102,
    *        SUM( CASE MATDOC~BWART WHEN '122' THEN MATDOC~MENGE ELSE 0 END ) AS MENGE122,
    *        SUM( CASE MATDOC~BWART WHEN '123' THEN MATDOC~MENGE ELSE 0 END ) AS MENGE123
    *    FROM @LT_EBE AS EB
    *    JOIN MATDOC ON MATDOC~EBELN = EB~EBELN AND MATDOC~EBELP = EB~EBELP
    *        GROUP BY EB~EBELN,EB~EBELP
    *        ORDER BY EB~EBELN,EB~EBELP
    *   INTO TABLE @LT_EBE_MENGE.
    *
    *  "根据计划交货行从ZMATDOC获取已交货数量
    *  SELECT PDLV~DLV_PLANNO,
    *         PDLV~DLV_ITEMNO,
    *         SUM( CASE M~SHKZG WHEN 'S' THEN M~MENGE ELSE 0 END ) AS MENGE_S,
    *         SUM( CASE M~SHKZG WHEN 'H' THEN M~MENGE ELSE 0 END ) AS MENGE_H
    *    FROM @LT_PLANNO_ITEM AS PDLV
    *    JOIN ZMATDOC AS M
    *      ON M~DLV_PLANNO = PDLV~DLV_PLANNO
    *     AND M~DLV_ITEMNO = PDLV~DLV_ITEMNO
    *         GROUP BY PDLV~DLV_PLANNO,PDLV~DLV_ITEMNO
    *         ORDER BY PDLV~DLV_PLANNO,PDLV~DLV_ITEMNO
    *    INTO TABLE @LT_PLANNO_MENGE.
    *  LOOP AT gt_pdlv_data ASSIGNING FIELD-SYMBOL(<fs_pdlv>).
    *    "PO收货数量、如果不存在匹配计划号及行号则收货数量等PO收货数量
    *    READ TABLE lt_ebe_menge WITH KEY ebeln = <fs_pdlv>-ebeln ebelp = <fs_pdlv>-ebelp BINARY SEARCH INTO gs_ebe_menge.
    *    IF  sy-subrc = 0.
    *      <fs_pdlv>-poreceived_quan = gs_ebe_menge-menge101 - gs_ebe_menge-menge102 - gs_ebe_menge-menge122 + gs_ebe_menge-menge123. "已收货数量 = 数量101 - 数量102 - 数量122 +  数量123
    *      IF <fs_pdlv>-dlv_planno IS INITIAL.
    *        <fs_pdlv>-received_quan = <fs_pdlv>-poreceived_quan.
    *      ENDIF.
    *    ENDIF.
    *    "存在计划交货行，已交货数量从MATDOC获取并更新
    *    CLEAR <fs_pdlv>-received_quan.
    *    IF <fs_pdlv>-dlv_planno IS NOT INITIAL .
    *      READ TABLE lt_planno_menge WITH KEY dlv_planno = <fs_pdlv>-dlv_planno dlv_itemno = <fs_pdlv>-dlv_itemno BINARY SEARCH INTO gs_planno_menge.
    *      IF sy-subrc = 0.
    *        <fs_pdlv>-received_quan = gs_planno_menge-menge_s - gs_planno_menge-menge_h. "已收货数量 = 借方 - 贷方
    *      ENDIF.
    *    ENDIF.
    *    IF <fs_pdlv>-received_quan < <fs_pdlv>-pdlv_menge."已交货数量 <  计划送货数量
    *      <fs_pdlv>-wjh_qty = <fs_pdlv>-pdlv_menge - <fs_pdlv>-received_quan."未交货数量 = 计划送货数量 - 已交货数量
    *    ENDIF.
    *  ENDLOOP.
    
    
      DATA : LT_PDLV_DATA LIKE GT_PDLV_DATA .
    *  DATA : LT_ZMATDOC TYPE TABLE OF TY_ZMATDOC .
      FIELD-SYMBOLS <LS_ZMATDOC> TYPE TY_ZMATDOC .
      DATA LT_EBE          LIKE TABLE OF GS_EBE .
      DATA: LT_EBE_MENGE    LIKE TABLE OF GS_EBE_MENGE.
    
      "获取对应匹配的送货计划号
      LT_PDLV_DATA[] = GT_PDLV_DATA[] .
      DELETE LT_PDLV_DATA WHERE DLV_PLANNO = '' .
      SORT LT_PDLV_DATA BY DLV_PLANNO DLV_ITEMNO.
      DELETE ADJACENT DUPLICATES FROM LT_PDLV_DATA COMPARING DLV_PLANNO DLV_ITEMNO.
    
      "根据送货计划号 获取ZMATDOC 表数据
      IF LT_PDLV_DATA[] IS NOT INITIAL .
        "根据计划交货行从ZMATDOC获取已交货数量
        CLEAR GT_ZMATDOC .
        SELECT PDLV~DLV_PLANNO,
               PDLV~DLV_ITEMNO,
               SUM( CASE M~SHKZG WHEN 'S' THEN M~MENGE WHEN 'H' THEN 0 - M~MENGE ELSE 0 END ) AS MENGE
          FROM @LT_PDLV_DATA AS PDLV
          JOIN ZMATDOC AS M
            ON M~DLV_PLANNO = PDLV~DLV_PLANNO
           AND M~DLV_ITEMNO = PDLV~DLV_ITEMNO
               GROUP BY PDLV~DLV_PLANNO,PDLV~DLV_ITEMNO
               ORDER BY PDLV~DLV_PLANNO,PDLV~DLV_ITEMNO
          INTO TABLE @GT_ZMATDOC.
      ENDIF .
    
      "根据采购订单行从BKBE获取已交货数量
      MOVE-CORRESPONDING GT_PDLV_DATA[] TO LT_EBE.
      DELETE LT_EBE WHERE EBELN = '' .
      SORT LT_EBE BY EBELN EBELP.
      DELETE ADJACENT DUPLICATES FROM LT_EBE COMPARING EBELN EBELP.
      IF LT_EBE[] IS NOT INITIAL .
        SELECT EB~EBELN,EB~EBELP,
              SUM( CASE EKBE~BWART WHEN '101' THEN EKBE~MENGE ELSE 0 END ) AS MENGE101,
              SUM( CASE EKBE~BWART WHEN '102' THEN EKBE~MENGE ELSE 0 END ) AS MENGE102,
              SUM( CASE EKBE~BWART WHEN '122' THEN EKBE~MENGE ELSE 0 END ) AS MENGE122,
              SUM( CASE EKBE~BWART WHEN '123' THEN EKBE~MENGE ELSE 0 END ) AS MENGE123
          FROM @LT_EBE AS EB
          JOIN EKBE ON EKBE~EBELN = EB~EBELN AND EKBE~EBELP = EB~EBELP
              GROUP BY EB~EBELN,EB~EBELP
              ORDER BY EB~EBELN,EB~EBELP
         INTO TABLE @LT_EBE_MENGE.
      ENDIF .
    
    
      SORT GT_ZMATDOC BY DLV_PLANNO DLV_ITEMNO .
      SORT LT_EBE_MENGE BY EBELN EBELP .
      LOOP AT GT_PDLV_DATA ASSIGNING FIELD-SYMBOL(<FS_PDLV>).
    
        "PO收货数量
        READ TABLE LT_EBE_MENGE WITH KEY EBELN = <FS_PDLV>-EBELN EBELP = <FS_PDLV>-EBELP BINARY SEARCH INTO GS_EBE_MENGE.
        IF SY-SUBRC = 0 .
          "已收货数量 = 数量101 - 数量102 - 数量122 +  数量123
          <FS_PDLV>-PORECEIVED_QUAN = GS_EBE_MENGE-MENGE101 - GS_EBE_MENGE-MENGE102 - GS_EBE_MENGE-MENGE122 + GS_EBE_MENGE-MENGE123.
        ENDIF .
    
        READ TABLE GT_ZMATDOC ASSIGNING <LS_ZMATDOC> WITH KEY DLV_PLANNO = <FS_PDLV>-DLV_PLANNO
                                                               DLV_ITEMNO = <FS_PDLV>-DLV_ITEMNO BINARY SEARCH .
        IF SY-SUBRC = 0 AND <LS_ZMATDOC>-MENGE <> 0.
    
          IF <FS_PDLV>-ASSIGN_MENGE >= <LS_ZMATDOC>-MENGE ."如果分配数量 大于等于收货数量
            <FS_PDLV>-RECEIVED_QUAN  = <LS_ZMATDOC>-MENGE ."已收数量直接等于pol收货数量
            <LS_ZMATDOC>-MENGE = 0 ."收货数量清零
          ELSE .
            <FS_PDLV>-RECEIVED_QUAN  = <FS_PDLV>-ASSIGN_MENGE ."已收数量等于分配数量
            <LS_ZMATDOC>-MENGE = <LS_ZMATDOC>-MENGE - <FS_PDLV>-ASSIGN_MENGE ."收货数量减掉分配数量
          ENDIF .
    
    *      IF <FS_PDLV>-RECEIVED_QUAN < <FS_PDLV>-PDLV_MENGE."已交货数量 <  计划送货数量
    *        <FS_PDLV>-WJH_QTY = <FS_PDLV>-PDLV_MENGE - <FS_PDLV>-RECEIVED_QUAN."未交货数量 = 计划送货数量 - 已交货数量
    *      ENDIF.
          <FS_PDLV>-WJH_QTY = <FS_PDLV>-ASSIGN_MENGE - <FS_PDLV>-RECEIVED_QUAN .
    
        ENDIF.
    
      ENDLOOP .
    
      "已收货数量大于0  直接分配给temp 第一行数据
      LOOP AT GT_ZMATDOC ASSIGNING <LS_ZMATDOC> WHERE MENGE > 0 ."
    
        READ TABLE GT_PDLV_DATA ASSIGNING <FS_PDLV> WITH KEY DLV_PLANNO = <LS_ZMATDOC>-DLV_PLANNO
                                                            DLV_ITEMNO = <LS_ZMATDOC>-DLV_ITEMNO  .
        IF SY-SUBRC = 0 .
          <FS_PDLV>-RECEIVED_QUAN = <FS_PDLV>-RECEIVED_QUAN + <LS_ZMATDOC>-MENGE .
        ENDIF .
    
      ENDLOOP .
      "UP DATA BY LANJINGCUN 20200420  更新获取收货数量数据(R6 供应商送货计划维护)-end
    ENDFORM.
    
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_DATE_ADD_DAYS
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *&      --> UV_DATE
    *&      --> UV_DAYS
    *&      <-- CV_DATE
    *&---------------------------------------------------------------------*
    FORM FRM_GET_DATE_ADD_DAYS  USING    UV_DATE
                                         UV_DAYS
                                CHANGING CV_DATE.
    
      CV_DATE = UV_DATE + UV_DAYS.
    *
    *  DATA: lv_days  LIKE t5a4a-dlydy.
    *
    *  lv_days = uv_days.
    *
    *  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    *    EXPORTING
    *      date      = uv_date
    *      days      = lv_days
    *      months    = 0
    **     SIGNUM    = '+'
    *      years     = 0
    *    IMPORTING
    *      calc_date = cv_date.
    
    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_PDLV_RECEIVED_QUAN_NEW
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *& -->  p1        text
    *& <--  p2        text
    *&---------------------------------------------------------------------*
    FORM FRM_GET_PDLV_RECEIVED_QUAN_NEW .
      DATA:LT_PLANNO_ITEM  LIKE TABLE OF GS_PLANNO_ITEM,
           LT_PLANNO_MENGE LIKE TABLE OF GS_PLANNO_MENGE,
           LT_EBE          LIKE TABLE OF GS_EBE,
           LT_EBE_MENGE    LIKE TABLE OF GS_EBE_MENGE.
      CHECK GT_PDLV_DATA[] IS NOT INITIAL.
      "存在计划交货行则根据计划交货行从ZMATDOC获取已交货数量，否则根据采购订单行从MATDOC获取已交货数量
      LOOP AT GT_PDLV_DATA INTO GS_PDLV_DATA_PO.
        LT_EBE = VALUE #( BASE LT_EBE (  EBELN = GS_PDLV_DATA_PO-EBELN   EBELP = GS_PDLV_DATA_PO-EBELP ) ).
        IF GS_PDLV_DATA_PO-DLV_PLANNO IS NOT INITIAL AND GS_PDLV_DATA_PO-DLV_ITEMNO IS NOT INITIAL.
          LT_PLANNO_ITEM = VALUE #( BASE LT_PLANNO_ITEM ( DLV_PLANNO  = GS_PDLV_DATA_PO-DLV_PLANNO  DLV_ITEMNO = GS_PDLV_DATA_PO-DLV_ITEMNO ) ).
        ENDIF.
      ENDLOOP.
      DELETE LT_EBE WHERE EBELN IS INITIAL OR EBELP IS INITIAL.
      DELETE LT_PLANNO_ITEM WHERE DLV_PLANNO IS INITIAL OR DLV_ITEMNO IS INITIAL.
      SORT LT_EBE BY EBELN EBELP.
      DELETE ADJACENT DUPLICATES FROM LT_EBE COMPARING EBELN EBELP.
      SORT  LT_PLANNO_ITEM BY DLV_PLANNO DLV_ITEMNO.
      DELETE ADJACENT DUPLICATES FROM LT_PLANNO_ITEM COMPARING DLV_PLANNO DLV_ITEMNO.
    
      "根据采购订单行从MATDOC获取已交货数量
      SELECT EB~EBELN,EB~EBELP,
            SUM( CASE MATDOC~BWART WHEN '101' THEN MATDOC~MENGE ELSE 0 END ) AS MENGE101,
            SUM( CASE MATDOC~BWART WHEN '102' THEN MATDOC~MENGE ELSE 0 END ) AS MENGE102,
            SUM( CASE MATDOC~BWART WHEN '122' THEN MATDOC~MENGE ELSE 0 END ) AS MENGE122,
            SUM( CASE MATDOC~BWART WHEN '123' THEN MATDOC~MENGE ELSE 0 END ) AS MENGE123
        FROM @LT_EBE AS EB
        JOIN MATDOC ON MATDOC~EBELN = EB~EBELN AND MATDOC~EBELP = EB~EBELP
            GROUP BY EB~EBELN,EB~EBELP
            ORDER BY EB~EBELN,EB~EBELP
       INTO TABLE @LT_EBE_MENGE.
      "根据计划交货行从ZMATDOC获取已交货数量
      SELECT PDLV~DLV_PLANNO,
             PDLV~DLV_ITEMNO,
             SUM( CASE M~SHKZG WHEN 'S' THEN M~MENGE ELSE 0 END ) AS MENGE_S,
             SUM( CASE M~SHKZG WHEN 'H' THEN M~MENGE ELSE 0 END ) AS MENGE_H
        FROM @LT_PLANNO_ITEM AS PDLV
        JOIN ZMATDOC AS M
          ON M~DLV_PLANNO = PDLV~DLV_PLANNO
         AND M~DLV_ITEMNO = PDLV~DLV_ITEMNO
             GROUP BY PDLV~DLV_PLANNO,PDLV~DLV_ITEMNO
             ORDER BY PDLV~DLV_PLANNO,PDLV~DLV_ITEMNO
        INTO TABLE @LT_PLANNO_MENGE.
    
      SORT LT_EBE_MENGE BY EBELN EBELP ."add by lanjingcun 20210213
      SORT LT_PLANNO_MENGE BY DLV_PLANNO DLV_ITEMNO ."add by lanjingcun 20210213
      LOOP AT GT_PDLV_DATA ASSIGNING FIELD-SYMBOL(<FS_PDLV>).
        "PO收货数量、如果不存在匹配计划号及行号则收货数量等PO收货数量
        READ TABLE LT_EBE_MENGE WITH KEY EBELN = <FS_PDLV>-EBELN EBELP = <FS_PDLV>-EBELP BINARY SEARCH INTO GS_EBE_MENGE.
        IF  SY-SUBRC = 0.
          <FS_PDLV>-PORECEIVED_QUAN = GS_EBE_MENGE-MENGE101 - GS_EBE_MENGE-MENGE102 - GS_EBE_MENGE-MENGE122 + GS_EBE_MENGE-MENGE123. "已收货数量 = 数量101 - 数量102 - 数量122 +  数量123
          IF <FS_PDLV>-DLV_PLANNO IS INITIAL.
            <FS_PDLV>-RECEIVED_QUAN = <FS_PDLV>-PORECEIVED_QUAN.
          ENDIF.
        ENDIF.
        "存在计划交货行，已交货数量从MATDOC获取并更新
        CLEAR <FS_PDLV>-RECEIVED_QUAN.
        IF <FS_PDLV>-DLV_PLANNO IS NOT INITIAL .
          READ TABLE LT_PLANNO_MENGE WITH KEY DLV_PLANNO = <FS_PDLV>-DLV_PLANNO DLV_ITEMNO = <FS_PDLV>-DLV_ITEMNO BINARY SEARCH INTO GS_PLANNO_MENGE.
          IF SY-SUBRC = 0.
            <FS_PDLV>-RECEIVED_QUAN = GS_PLANNO_MENGE-MENGE_S - GS_PLANNO_MENGE-MENGE_H. "已收货数量 = 借方 - 贷方
          ENDIF.
        ENDIF.
        IF <FS_PDLV>-RECEIVED_QUAN < <FS_PDLV>-PDLV_MENGE."已交货数量 <  计划送货数量
          <FS_PDLV>-WJH_QTY = <FS_PDLV>-PDLV_MENGE - <FS_PDLV>-RECEIVED_QUAN."未交货数量 = 计划送货数量 - 已交货数量
        ENDIF.
      ENDLOOP.
    ENDFORM.
    *&---------------------------------------------------------------------*
    *& Form FRM_GET_ZJFSDT
    *&---------------------------------------------------------------------*
    *& text
    *&---------------------------------------------------------------------*
    *&      --> GS_ZPLTA_MATNR
    *&      --> LS_MARA_MATKL
    *&---------------------------------------------------------------------*
    FORM FRM_GET_ZJFSDT  USING    P_MATNR TYPE MARA-MATNR
                                  P_MATKL TYPE MARA-MATKL .
    
      DATA :OBJECTKEY LIKE  BAPI1003_KEY-OBJECT               .
      DATA :OBJECTTABLE LIKE  BAPI1003_KEY-OBJECTTABLE        .
      DATA :CLASSNUM  LIKE  BAPI1003_KEY-CLASSNUM             .
      DATA :CLASSTYPE LIKE  BAPI1003_KEY-CLASSTYPE            .
      DATA :KEYDATE LIKE  BAPI1003_KEY-KEYDATE                .
      DATA :UNVALUATED_CHARS  TYPE  FLAG                      .
      DATA :LANGUAGE  LIKE  BAPIFIELDSCACL-BAPILANGUA         .
      DATA :OBJECTKEY_LONG  LIKE  BAPI1003_KEY-OBJECT_LONG    .
    
      DATA : LS_ALLOCVALUESNUM    LIKE  BAPI1003_ALLOC_VALUES_NUM  .
      DATA : LS_ALLOCVALUESCHAR LIKE  BAPI1003_ALLOC_VALUES_CHAR .
      DATA : LS_ALLOCVALUESCURR LIKE  BAPI1003_ALLOC_VALUES_CURR .
      DATA : LS_RETURN  LIKE  BAPIRET2                             .
      DATA : LT_ALLOCVALUESNUM    LIKE TABLE OF BAPI1003_ALLOC_VALUES_NUM  .
      DATA : LT_ALLOCVALUESCHAR	LIKE TABLE OF	BAPI1003_ALLOC_VALUES_CHAR .
      DATA : LT_ALLOCVALUESCURR	LIKE TABLE OF	BAPI1003_ALLOC_VALUES_CURR .
      DATA : LT_RETURN          LIKE TABLE OF BAPIRET2                             .
    
      CLEAR GS_ZJFSDT .
      CLEAR LT_ALLOCVALUESNUM .
      CLEAR LT_ALLOCVALUESCHAR .
      CLEAR LT_ALLOCVALUESCURR .
      CLEAR LT_RETURN .
      OBJECTKEY = P_MATNR .
      CLASSNUM = P_MATKL .
      CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
        EXPORTING
          OBJECTKEY       = OBJECTKEY
          OBJECTTABLE     = 'MARA'
          CLASSNUM        = CLASSNUM
          CLASSTYPE       = '001'
    *     KEYDATE         = LS_ZMMAPS_PL-VERSION
        TABLES
          ALLOCVALUESNUM  = LT_ALLOCVALUESNUM
          ALLOCVALUESCHAR = LT_ALLOCVALUESCHAR
          ALLOCVALUESCURR = LT_ALLOCVALUESCURR
          RETURN          = LT_RETURN.
    
      CLEAR LS_ALLOCVALUESCHAR .
      READ TABLE LT_ALLOCVALUESCHAR INTO LS_ALLOCVALUESCHAR WITH KEY CHARACT = 'ZJFSDT' .
      GS_ZJFSDT-ZJFSDT = LS_ALLOCVALUESCHAR-VALUE_CHAR .
      GS_ZJFSDT-MATNR = P_MATNR .
    
      APPEND  GS_ZJFSDT TO GT_ZJFSDT .
    
    ENDFORM.