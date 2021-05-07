*&---------------------------------------------------------------------*
*& 包含               ZMMR0003_F05
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form FRM_GET_DEL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM frm_get_temp_ok .

    DATA: lv_tabix   LIKE sy-tabix,
          lv_success.
    IF r_01 = 'X' .
  
      IF s_planno[] IS INITIAL .
  *    AND s_itemno IS INITIAL .
        WITH
          +result AS (
                SELECT temp~*,
                       poh~name1_gp,
                       poh~ernam,
                       poh~note,
                       pol~pdlv_menge,
                       pol~xclosed
  *                 pol~received_quan
  
                  FROM zmmaps_temp AS temp
                  LEFT JOIN zmmwms_pol AS pol
                    ON pol~dlv_planno = temp~dlv_planno
                   AND pol~dlv_itemno = temp~dlv_itemno
                  LEFT JOIN zmmwms_poh AS poh
                    ON poh~dlv_planno = temp~dlv_planno
                 WHERE temp~ebeln IN @s_ebeln
                   AND temp~version IN @s_versi
                   AND temp~ebelp IN @s_ebelp
                   AND temp~pdlv_date IN @s_date3
                   AND temp~matnr IN @s_matnr2
                   AND temp~werks IN @s_werks2
                   AND temp~lgort IN @s_lgort2
                   AND temp~st_code IN @s_code
                   AND temp~inactive = ''
                   AND EXISTS ( SELECT *
                                  FROM ekko
                                  WHERE ebeln =  temp~ebeln
                                    AND ernam IN @s_ernam
                                    AND lifnr IN @s_lifnr
                                    AND ekgrp IN @s_ekgr1  )
           )
          SELECT t1~*,t2~remain_days,makt~maktx
            FROM +result AS t1
            LEFT JOIN zmmaps_pl AS t2 ON t1~guid = t2~guid
            LEFT JOIN makt ON t1~matnr = makt~matnr AND makt~spras = @sy-langu
            INTO CORRESPONDING FIELDS OF TABLE @gt_pdlv_data.
      ELSE .
        WITH
          +result AS (
                SELECT temp~*,
                       poh~name1_gp,
                       poh~ernam,
                       poh~note,
                       pol~pdlv_menge,
                       pol~xclosed
  *                 pol~received_quan
  
                  FROM zmmaps_temp AS temp
                  INNER  JOIN zmmwms_pol AS pol
                    ON pol~dlv_planno = temp~dlv_planno
                   AND pol~dlv_itemno = temp~dlv_itemno
                  LEFT JOIN zmmwms_poh AS poh
                    ON poh~dlv_planno = temp~dlv_planno
                 WHERE temp~ebeln IN @s_ebeln
                   AND temp~ebelp IN @s_ebelp
                   AND temp~version IN @s_versi
                   AND temp~dlv_planno IN @s_planno
  *                 AND temp~dlv_itemno IN @s_itemno
                   AND temp~pdlv_date IN @s_date3
                   AND temp~matnr IN @s_matnr2
                   AND temp~werks IN @s_werks2
                   AND temp~lgort IN @s_lgort2
                   AND temp~st_code IN @s_code
                   AND temp~inactive = ''
                   AND EXISTS ( SELECT *
                                  FROM ekko
                                  WHERE ebeln =  temp~ebeln
                                    AND ernam IN @s_ernam
                                    AND lifnr IN @s_lifnr
                                    AND ekgrp IN @s_ekgr1  )
           )
          SELECT t1~*,t2~remain_days,makt~maktx
            FROM +result AS t1
            LEFT JOIN zmmaps_pl AS t2 ON t1~guid = t2~guid
            LEFT JOIN makt ON t1~matnr = makt~matnr AND makt~spras = @sy-langu
            INTO CORRESPONDING FIELDS OF TABLE @gt_pdlv_data.
      ENDIF  .
  
    ELSE .
      WITH
        +result AS (
              SELECT temp~*,
                     poh~name1_gp,
                     poh~ernam,
                     poh~note,
                     pol~pdlv_menge,
                     pol~xclosed
  *                 pol~received_quan
  
                FROM zmmaps_temp AS temp
                LEFT JOIN zmmwms_pol AS pol
                  ON pol~dlv_planno = temp~dlv_planno
                 AND pol~dlv_itemno = temp~dlv_itemno
                LEFT JOIN zmmwms_poh AS poh
                  ON poh~dlv_planno = temp~dlv_planno
               WHERE temp~ebeln IN @s_ebeln
  *               AND TEMP~VERSION IN @S_VERSI
                 AND temp~ebelp IN @s_ebelp
                 AND temp~pdlv_date IN @s_date3
                 AND temp~matnr IN @s_matnr2
                 AND temp~werks IN @s_werks2
                 AND temp~lgort IN @s_lgort2
                 AND temp~st_code IN @s_code
                 AND temp~inactive = ''
                 AND EXISTS ( SELECT *
                                FROM ekko
                                WHERE ebeln =  temp~ebeln
                                  AND ernam IN @s_ernam
                                  AND lifnr IN @s_lifnr
                                  AND ekgrp IN @s_ekgr1  )
         )
        SELECT t1~*,t2~remain_days,makt~maktx
          FROM +result AS t1
          LEFT JOIN zmmaps_pl AS t2 ON t1~guid = t2~guid
          LEFT JOIN makt ON t1~matnr = makt~matnr AND makt~spras = @sy-langu
          INTO CORRESPONDING FIELDS OF TABLE @gt_pdlv_data.
  
    ENDIF .
    "up by lanjingcun 20200522 设置筛选器 start
  *  CASE 'X'.
  *    WHEN R_02."未发送WMS
  *      DELETE GT_PDLV_DATA WHERE DLV_PLANNO IS NOT INITIAL.
  *    WHEN R_03."已发送WMS
  *      DELETE GT_PDLV_DATA WHERE DLV_PLANNO IS INITIAL.
  *  ENDCASE.
  
  
  
  
  
    CLEAR gt_filt[] .
  
    CASE 'X'.
      WHEN r_02."未发送WMS
  *      DELETE GT_PDLV_DATA WHERE DLV_PLANNO IS NOT INITIAL.
        CLEAR gs_filt .
        gs_filt-fieldname   = 'DLV_ITEMNO'   .
        gs_filt-tabname     = 'GT_PDLV_DATA'  .
        gs_filt-low         = ''  .
        gs_filt-sign        = 'I'  .
        gs_filt-option      = 'EQ'  .
        APPEND gs_filt TO gt_filt .
        IF s_planno[] IS NOT INITIAL .
  *        OR s_itemno[] IS NOT INITIAL .
          MESSAGE '没有符合条件的数据' TYPE 'S' DISPLAY LIKE 'E' .
          LEAVE LIST-PROCESSING .
        ENDIF .
  
      WHEN r_03."已发送WMS
  *      DELETE GT_PDLV_DATA WHERE DLV_PLANNO IS INITIAL.
        CLEAR gs_filt .
        gs_filt-fieldname   = 'DLV_ITEMNO'   .
        gs_filt-tabname     = 'GT_PDLV_DATA'  .
        gs_filt-low         = ''  .
        gs_filt-sign        = 'I'  .
        gs_filt-option      = 'NE'  .
        APPEND gs_filt TO gt_filt .
  
        LOOP AT s_planno .
          gs_filt-fieldname   = 'DLV_PLANNO'   .
          gs_filt-tabname     = 'GT_PDLV_DATA'  .
          gs_filt-low         = s_planno-low .
          gs_filt-high        = s_planno-high .
          gs_filt-sign        = s_planno-sign    .
  *        IF s_planno-option = '' .
  *          IF s_planno-high <> '' .
  *            gs_filt-option      = 'BT' .
  *          ELSE .
  *            gs_filt-option      = 'EQ' .
  *          ENDIF .
  *        ELSE .
  *          gs_filt-option      = s_planno-option .
  *        ENDIF .
          gs_filt-option      = s_planno-option .
          APPEND gs_filt TO gt_filt .
        ENDLOOP .
  
  *      LOOP AT s_itemno .
  *        gs_filt-fieldname   = 'DLV_ITEMNO'   .
  *        gs_filt-tabname     = 'GT_PDLV_DATA'  .
  *        gs_filt-low         = s_itemno-low .
  *        gs_filt-high        = s_itemno-high .
  *        gs_filt-sign        = s_itemno-sign    .
  *        gs_filt-option      = s_itemno-option .
  *        IF s_itemno-option = '' .
  *          IF s_itemno-high <> '' .
  *            gs_filt-option      = 'BT' .
  *          ELSE .
  *            gs_filt-option      = 'EQ' .
  *          ENDIF .
  *        ELSE .
  *          gs_filt-option      = s_itemno-option .
  *        ENDIF .
  *        APPEND gs_filt TO gt_filt .
  *      ENDLOOP .
      WHEN OTHERS .
        LOOP AT s_planno .
          gs_filt-fieldname   = 'DLV_PLANNO'   .
          gs_filt-tabname     = 'GT_PDLV_DATA'  .
          gs_filt-low         = s_planno-low .
          gs_filt-high        = s_planno-high .
          gs_filt-sign        = s_planno-sign    .
  *        IF s_planno-option = '' .
  *          IF s_planno-high <> '' .
  *            gs_filt-option      = 'BT' .
  *          ELSE .
  *            gs_filt-option      = 'EQ' .
  *          ENDIF .
  *        ELSE .
  *          gs_filt-option      = s_planno-option .
  *        ENDIF .
          gs_filt-option      = s_planno-option .
          APPEND gs_filt TO gt_filt .
        ENDLOOP .
  
  *      LOOP AT s_itemno .
  *        gs_filt-fieldname   = 'DLV_ITEMNO'   .
  *        gs_filt-tabname     = 'GT_PDLV_DATA'  .
  *        gs_filt-low         = s_itemno-low .
  *        gs_filt-high        = s_itemno-high .
  *        gs_filt-sign        = s_itemno-sign    .
  *        gs_filt-option      = s_itemno-option .
  *        IF s_itemno-option = '' .
  *          IF s_itemno-high <> '' .
  *            gs_filt-option      = 'BT' .
  *          ELSE .
  *            gs_filt-option      = 'EQ' .
  *          ENDIF .
  *        ELSE .
  *          gs_filt-option      = s_itemno-option .
  *        ENDIF .
  *        APPEND gs_filt TO gt_filt .
  *      ENDLOOP .
    ENDCASE.
  
    LOOP AT s_versi .
      gs_filt-fieldname   = 'VERSION'   .
      gs_filt-tabname     = 'GT_PDLV_DATA'  .
      gs_filt-low         = s_versi-low .
      gs_filt-high        = s_versi-high .
      gs_filt-sign        = s_versi-sign    .
      gs_filt-option      = s_versi-option .
      APPEND gs_filt TO gt_filt .
    ENDLOOP .
  
    "up by lanjingcun 20200522 设置筛选器 end
  
  
    IF gt_pdlv_data[] IS INITIAL.
      PERFORM frm_stop_program  USING TEXT-e01.
    ENDIF.
  
    PERFORM frm_get_marc.   "采购组
  
    SORT gt_marc BY matnr werks.
  
    LOOP AT gt_pdlv_data INTO gs_pdlv_data.
  
      lv_tabix = sy-tabix.
  
      READ TABLE gt_marc INTO gs_marc WITH KEY matnr = gs_pdlv_data-matnr
                                               werks = gs_pdlv_data-werks
                                               BINARY SEARCH.
      IF sy-subrc = 0.
        AUTHORITY-CHECK OBJECT 'ZMM020'
         ID 'WERKS' FIELD gs_pdlv_data-werks
         ID 'EKGRP' FIELD gs_marc-ekgrp.
        IF sy-subrc <> 0.
          DELETE gt_pdlv_data INDEX lv_tabix.
          CONTINUE.
        ENDIF.
  *      PERFORM frm_check_m_best_ekg1 USING gs_marc-ekgrp lv_success.
  *      IF lv_success = 'E'.
  *        DELETE gt_pdlv_data INDEX lv_tabix.
  *      ENDIF.
      ENDIF.
  
    ENDLOOP.
  
    IF gt_pdlv_data[] IS INITIAL.
      PERFORM frm_stop_program  USING TEXT-e01.
    ENDIF.
  
    PERFORM frm_get_pdlv_received_quan.
    PERFORM frm_process_r06.
    PERFORM frm_set_edit_style.
  
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_SEND_WMS_AND_SAVE
  *&---------------------------------------------------------------------*
  *& 发送WMS并保存数据
  *&---------------------------------------------------------------------*
  FORM frm_send_wms_and_save .
  
    DATA: lv_flag,
          lv_answer,
          lt_pdlv     LIKE gt_pdlv_data,
          lt_pdlv_wms LIKE gt_pdlv_data,
          lt_pdlv_tmp LIKE gt_pdlv_data.
  
    DATA : lv_message(220) TYPE c .
  
    LOOP AT gt_pdlv_data INTO DATA(ls_pdlv_data) WHERE update_flag = 'X'
                                                    AND light IS INITIAL.
      "RECEIVED_QUAN送货行已收数量 >= PDLV_MENGE计划送货数量不下发WMS
      CHECK ls_pdlv_data-received_quan = 0 OR ( ls_pdlv_data-received_quan <  ls_pdlv_data-pdlv_menge ).
      APPEND ls_pdlv_data TO lt_pdlv.
    ENDLOOP.
    IF sy-subrc <> 0.
      LOOP AT gt_pdlv_data INTO ls_pdlv_data WHERE update_flag = 'X' AND light IS INITIAL.
  *      CHECK LS_PDLV_DATA-RECEIVED_QUAN <  LS_PDLV_DATA-PDLV_MENGE.
        CHECK ls_pdlv_data-received_quan = 0 OR ( ls_pdlv_data-received_quan <  ls_pdlv_data-pdlv_menge ).
        APPEND ls_pdlv_data TO lt_pdlv.
      ENDLOOP.
      IF lt_pdlv[] IS NOT INITIAL .
        PERFORM frm_showconfirm USING '发送确认?' '您没有选择要保存的数据，但是有修改数据，需要保存全部有修改的数据吗?' '确认' '' 2 CHANGING lv_answer.
        CHECK lv_answer = '1'.
      ELSE.
        MESSAGE '没有找到需要保存的数据' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.
  
    LOOP AT lt_pdlv INTO ls_pdlv_data.
      PERFORM frm_check_po_qty USING ls_pdlv_data ls_pdlv_data-assign_menge  ''  CHANGING lv_message.
      IF lv_message <> '' .
        CONCATENATE '计划行' ls_pdlv_data-dlv_planno '/' ls_pdlv_data-dlv_itemno lv_message INTO lv_message .
        MESSAGE lv_message TYPE 'E' .
      ENDIF .
      IF lv_flag IS INITIAL AND ls_pdlv_data-released = 'X'.
        READ TABLE lt_pdlv TRANSPORTING NO FIELDS WITH KEY uniqueid = ls_pdlv_data-uniqueid pdlv_date = ls_pdlv_data-pdlv_date released = ''.
        IF sy-subrc = 0.
          lv_flag = 'X'.
        ENDIF.
      ENDIF.
  
      IF ls_pdlv_data-released = 'X'.
        APPEND ls_pdlv_data TO lt_pdlv_wms.
      ELSE.
        APPEND ls_pdlv_data TO lt_pdlv_tmp.
      ENDIF.
  
    ENDLOOP.
  
    SORT lt_pdlv_tmp BY pdlv_date_ext DESCENDING.
  
    IF lv_flag = 'X'.
      PERFORM frm_showconfirm USING '保存确认?' '未发送WMS的数据与已发送的数据可以合并，需要合并发送WMS吗' '确认' '' 2 CHANGING lv_answer.
      CHECK lv_answer = '1' OR lv_answer = '2'.
      CASE lv_answer.
        WHEN '1'.
  
          LOOP AT lt_pdlv_tmp INTO ls_pdlv_data.
            DATA(lv_tabix) = sy-tabix.
            READ TABLE lt_pdlv_wms INTO DATA(ls_pdlv) WITH KEY ebeln = ls_pdlv_data-ebeln ebelp = ls_pdlv_data-ebelp pdlv_date = ls_pdlv_data-pdlv_date.
            IF sy-subrc = 0.
              ls_pdlv_data-dlv_planno = ls_pdlv-dlv_planno.
              ls_pdlv_data-dlv_itemno = ls_pdlv-dlv_itemno.
              ls_pdlv_data-update_temp = 'X'.
              APPEND ls_pdlv_data TO lt_pdlv_wms.
              DELETE lt_pdlv_tmp INDEX lv_tabix.
            ENDIF.
          ENDLOOP.
  
      ENDCASE.
  
    ENDIF.
  
  
    IF lt_pdlv_wms[] IS NOT INITIAL.
      PERFORM frm_send_and_save TABLES lt_pdlv_wms.
    ENDIF.
  
    IF lt_pdlv_tmp[] IS NOT INITIAL.
      PERFORM frm_save_temp TABLES lt_pdlv_tmp.
    ENDIF.
  
  ENDFORM.
  
  
  
  *&---------------------------------------------------------------------*
  *& Form FRM_SET_EDIT_STYLE
  *&---------------------------------------------------------------------*
  *& 设置编辑状态
  *&---------------------------------------------------------------------*
  FORM frm_set_edit_style .
  
    DATA: ls_edit TYPE lvc_s_styl,
          lt_edit TYPE lvc_t_styl.
  
    set_disedit_style 'PDLV_DATE'.    "设置栏位不可编辑
  
    "add BY LANJINGCUN 20200426  最晚答复日期的检验 临时结构start
    DATA : BEGIN OF ls_version ,"工厂最大版本号
             werks   TYPE t001w-werks,
             version TYPE zmmaps_pl-version,
           END OF ls_version .
    DATA : BEGIN OF ls_matnr ,"物料保留天数
             matnr       TYPE mara-matnr,
             remain_days TYPE zmmaps_pl-remain_days,
           END OF ls_matnr .
  
    DATA : lt_matnr LIKE TABLE OF ls_matnr .
    DATA : lt_version LIKE TABLE OF ls_version .
    DATA : lt_zplta TYPE TABLE OF ty_zplta .
    DATA : ls_zplta TYPE ty_zplta .
    DATA : lt_maral TYPE TABLE OF mara .
    DATA : ls_maral TYPE mara .
    DATA : lv_days TYPE sy-datum .
    DATA : lv_matkl TYPE mara-matkl .
    DATA : lv_where(20) TYPE c .
  
    CLEAR gt_zjfsdt .
    CLEAR gt_zplta .
    DATA : lv_index TYPE i .
  
    "add BY LANJINGCUN 20200426  最晚答复日期的检验 临时结构end
  
  
  
    LOOP AT gt_pdlv_data ASSIGNING FIELD-SYMBOL(<fv_pdlv>).
      CLEAR: lt_edit[].
      IF <fv_pdlv>-released = 'X'.
        set_disedit_style 'PDLV_DATE'.    "设置栏位不可编辑
        <fv_pdlv>-cell_styles = lt_edit[].
      ELSE.
        set_disedit_style 'PDLV_DATE_EXT'.    "设置栏位不可编辑
        <fv_pdlv>-cell_styles = lt_edit[].
      ENDIF.
  
      <fv_pdlv>-zkshsl =  <fv_pdlv>-assign_menge - <fv_pdlv>-received_quan ."ADD BY LANJINGCUN 20200520 增加可收数量字段
  
      "add BY LANJINGCUN 20200426  最晚答复日期的检验 start
      "获取最大版本号
      CLEAR ls_version .
      READ TABLE lt_version INTO ls_version WITH  KEY werks = <fv_pdlv>-werks .
      IF sy-subrc <> 0 .
        SELECT MAX( version )
        INTO ls_version-version
        FROM zmmaps_pl
        WHERE werks = <fv_pdlv>-werks
          AND zrsv01 = ''. "排除手工导入的
        ls_version-werks = <fv_pdlv>-werks .
        APPEND ls_version TO lt_version .
      ENDIF .
  
      "获取保留天数
      CLEAR ls_matnr .
      READ TABLE lt_matnr INTO ls_matnr WITH  KEY matnr = <fv_pdlv>-matnr .
      IF sy-subrc <> 0 .
  
        "1．	通过最后1次版本号和物料号从PL表获取保留天数；
        SELECT MAX( remain_days )
          INTO ls_matnr-remain_days
          FROM zmmaps_pl
          WHERE matnr = <fv_pdlv>-matnr
            AND version = ls_version-version
            AND werks = <fv_pdlv>-werks
            AND zrsv01 = ''. "排除手工导入的
        IF ls_matnr-remain_days = 0 .
  *           2      通过最新版本号，该物料的物料组从PL表获取物料和保留天数清单，然后循环这个清单，获取这个清单中的物料，
  *                 从物料分类视图键值ZJFSDT获取值，如果该值与该物料从物料分类视图键值ZJFSDT获取的值一样，则跳出循环，将内表的保留天数的值赋值给这个物料；
  
          SELECT SINGLE matkl
          INTO lv_matkl
          FROM mara
          WHERE matnr = <fv_pdlv>-matnr .
  
          CLEAR gs_zplta .
          LOOP AT gt_zplta INTO gs_zplta WHERE matkl+0(2) = lv_matkl+0(2) .
  
            CLEAR gs_zjfsdt .
            SORT gt_zjfsdt BY matnr .
            READ TABLE gt_zjfsdt INTO gs_zjfsdt WITH KEY matnr = gs_zplta-matnr BINARY SEARCH .
            IF sy-subrc <> 0 .
              PERFORM frm_get_zjfsdt USING gs_zplta-matnr gs_zplta-matkl  ."获取分配视图
            ENDIF .
            IF gs_zplta-remain_days = gs_zjfsdt-zjfsdt .
              "如果该值与该物料从物料分类视图键值ZJFSDT获取的值一样，则跳出循环，将内表的保留天数的值赋值给这个物料；
              ls_matnr-remain_days = gs_zjfsdt-zjfsdt .
              EXIT .
            ENDIF .
  
          ENDLOOP .
          IF sy-subrc <> 0 .
            "
            CLEAR lt_zplta .
            CLEAR lt_maral .
            "获取同物料组物料
            CLEAR lv_where .
            CONCATENATE  lv_matkl+0(2) '%' INTO lv_where .
            SELECT *
              INTO TABLE lt_maral
              FROM mara
              WHERE matkl LIKE lv_where.
  
            "获取同物料组物料
            SELECT *
              INTO TABLE lt_zplta
              FROM zmmaps_pl
              FOR ALL ENTRIES IN lt_maral
              WHERE matnr = lt_maral-matnr
                AND version = ls_version-version
              AND werks = <fv_pdlv>-werks
              AND zrsv01 = ''. "排除手工导入的.
            CLEAR ls_zplta .
            LOOP AT lt_zplta INTO ls_zplta  .
  
              "保存临时运算表
              CLEAR ls_maral .
              SORT lt_maral BY matnr .
              READ TABLE lt_maral INTO ls_maral WITH KEY matnr = ls_zplta-matnr BINARY SEARCH .
              ls_zplta-matkl = ls_maral-matkl .
              APPEND ls_zplta TO gt_zplta .
  
              CLEAR gs_zjfsdt .
              SORT gt_zjfsdt BY matnr .
              READ TABLE gt_zjfsdt INTO gs_zjfsdt WITH KEY matnr = ls_zplta-matnr BINARY SEARCH .
              IF sy-subrc <> 0 .
                PERFORM frm_get_zjfsdt USING ls_zplta-matnr ls_maral-matkl  ."获取分配视图
              ENDIF .
              IF ls_zplta-remain_days = gs_zjfsdt-zjfsdt .
                "如果该值与该物料从物料分类视图键值ZJFSDT获取的值一样，则跳出循环，将内表的保留天数的值赋值给这个物料；
                ls_matnr-remain_days = gs_zjfsdt-zjfsdt .
                EXIT .
              ENDIF .
  
            ENDLOOP .
  
          ENDIF .
          IF ls_matnr-remain_days = 0 .
  *                  "3．  如果仍未空，则以物料分类视图键值ZJFSDT作为该物料的保留天数
            CLEAR gs_zjfsdt .
            SORT gt_zjfsdt BY matnr .
            READ TABLE gt_zjfsdt INTO gs_zjfsdt WITH KEY matnr = <fv_pdlv>-matnr BINARY SEARCH .
            IF sy-subrc <> 0 .
              PERFORM frm_get_zjfsdt USING <fv_pdlv>-matnr lv_matkl  ."获取分配视图
            ENDIF .
            ls_matnr-remain_days = gs_zjfsdt-zjfsdt .
            IF ls_matnr-remain_days = 0 .
              "4．  如果仍未空，则默认包材（20开头）7天，原辅料（10开头）15天，其他7天
              IF lv_matkl+0(2) = '10' .
                ls_matnr-remain_days = 15.
              ELSE .
                ls_matnr-remain_days = 7.
              ENDIF .
            ENDIF .
          ENDIF .
  
        ENDIF .
        ls_matnr-matnr = <fv_pdlv>-matnr .
        APPEND ls_matnr TO lt_matnr .
      ENDIF .
      "取“计算保留天数”和对应的送料计划行上的“保留天数”的最大值来作为“参考保留天数”
      <fv_pdlv>-zjsyul  = ls_matnr-remain_days  ."计算的保留日期
      IF ls_matnr-remain_days < <fv_pdlv>-remain_days .
        ls_matnr-remain_days = <fv_pdlv>-remain_days  .
      ENDIF .
      <fv_pdlv>-versiom  = ls_version-version ."计算最大版本号
      lv_days = ls_version-version + ls_matnr-remain_days - 1.
      <fv_pdlv>-zzwdfr  = lv_days ."计算的最晚回复日期
  
      "add BY LANJINGCUN 20200426  最晚答复日期的检验 end
      lv_index = lv_index + 1 .
      <fv_pdlv>-index = lv_index .
  
    ENDLOOP.
  
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_PROCESS_R06
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  FORM frm_process_r06 .
  
    SELECT frgke FROM t16fb WHERE kzfre = 'X' INTO TABLE @DATA(lt_sp).
  
    LOOP AT gt_pdlv_data ASSIGNING FIELD-SYMBOL(<fv_pdlv>).
  
      SELECT SINGLE ekko~ernam
                    ekko~frgsx
                    ekko~frgke
                    ekpo~menge
                    ekpo~ematn
                    ekpo~lgort
                    ekpo~uebto
        INTO (<fv_pdlv>-ernam, <fv_pdlv>-frgsx, <fv_pdlv>-frgke,<fv_pdlv>-menge, <fv_pdlv>-ematn, <fv_pdlv>-lgort, <fv_pdlv>-uebto)
        FROM ekko
        JOIN ekpo
          ON ekpo~ebeln = ekko~ebeln
       WHERE ekko~ebeln = <fv_pdlv>-ebeln
         AND ekpo~ebelp = <fv_pdlv>-ebelp.
      SELECT SINGLE name1 INTO <fv_pdlv>-name1_gp FROM lfa1 WHERE lifnr = <fv_pdlv>-lifnr.
      DATA(lv_note) = zcl_cm_public=>get_long_txt( id = 'F02' name = <fv_pdlv>-ebeln object = 'EKKO' ).
      IF lv_note IS NOT INITIAL.
        <fv_pdlv>-note = lv_note.
      ENDIF.
  
      IF <fv_pdlv>-producer IS INITIAL.
        IF <fv_pdlv>-ematn <> <fv_pdlv>-matnr.
          SELECT SINGLE mfrpn INTO <fv_pdlv>-mfrpn    FROM mara WHERE matnr = <fv_pdlv>-ematn.
          SELECT SINGLE mfrnr INTO <fv_pdlv>-producer FROM mara WHERE matnr = <fv_pdlv>-ematn.
        ELSE.
          SELECT SINGLE mfrpn INTO <fv_pdlv>-mfrpn    FROM mara WHERE matnr = <fv_pdlv>-matnr.
          SELECT SINGLE mfrnr INTO <fv_pdlv>-producer FROM mara WHERE matnr = <fv_pdlv>-matnr.
        ENDIF.
        IF sy-subrc = 0.
          SELECT SINGLE name1 INTO <fv_pdlv>-name1_p FROM lfa1 WHERE lifnr = <fv_pdlv>-producer.
        ENDIF.
      ENDIF.
  
      IF <fv_pdlv>-frgsx IS NOT INITIAL.
        READ TABLE lt_sp TRANSPORTING NO FIELDS WITH KEY frgke = <fv_pdlv>-frgke.
        IF sy-subrc = 0.
          <fv_pdlv>-pz = 'X'.
        ENDIF.
      ELSE.
        <fv_pdlv>-pz = 'X'.
      ENDIF.
  
    ENDLOOP.
  
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_SAVE_TEMP
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> IT_PDLV_TMP
  *&---------------------------------------------------------------------*
  FORM frm_save_temp TABLES it_pdlv_tmp LIKE gt_pdlv_data.
  
    DATA: lt_temp LIKE TABLE OF zmmaps_temp.
  
    LOOP AT it_pdlv_tmp INTO DATA(ls_pdlv_tmp).
      UPDATE zmmaps_temp
         SET pdlv_date    = ls_pdlv_tmp-pdlv_date
             assign_menge = ls_pdlv_tmp-assign_menge
       WHERE guid = ls_pdlv_tmp-guid
         AND uniqueid = ls_pdlv_tmp-uniqueid.
  
      READ TABLE gt_pdlv_data INTO DATA(ls_pdlv_data) WITH KEY update_flag = 'X' light = '' guid = ls_pdlv_tmp-guid uniqueid = ls_pdlv_tmp-uniqueid.
      IF sy-subrc = 0.
        ls_pdlv_data-light = icon_led_green.
        ls_pdlv_data-message = '保存成功'.
        ls_pdlv_data-zkshsl =  ls_pdlv_data-assign_menge - ls_pdlv_data-received_quan ."ADD BY LANJINGCUN 20200520 增加可收数量字段
        CLEAR: ls_pdlv_data-update_flag.
        MODIFY gt_pdlv_data FROM ls_pdlv_data INDEX sy-tabix.
      ENDIF.
  
    ENDLOOP.
  
    IF r_02 = 'X'.
      MESSAGE '保存成功' TYPE 'S'.
      SET SCREEN 0.
      LEAVE SCREEN.
    ENDIF.
  
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_SEND_AND_SAVE
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> IT_PDLV_TMP
  *&---------------------------------------------------------------------*
  FORM frm_send_and_save TABLES it_pdlv_tmp LIKE gt_pdlv_data.
  
    DATA: lt_pdlv LIKE gt_pdlv_data,
          lt_poh  LIKE TABLE OF zmmwms_poh,
          ls_poh  LIKE zmmwms_poh,
          lt_pol  LIKE TABLE OF zmmwms_pol,
          ls_pol  LIKE zmmwms_pol,
          lt_pol2 LIKE TABLE OF zmmwms_pol.
  
    DATA: ls_input  TYPE zszmmwms_poh_pol,
          ls_output TYPE zsmessage_out.
  
  
    DATA: ls_edit TYPE lvc_s_styl,
          lt_edit TYPE lvc_t_styl.
  
    CHECK it_pdlv_tmp[] IS NOT INITIAL.
  
    lt_pdlv[] = it_pdlv_tmp[].
    SORT lt_pdlv BY dlv_planno.
    DELETE ADJACENT DUPLICATES FROM lt_pdlv COMPARING dlv_planno.
  
    SELECT *
      INTO TABLE lt_poh
      FROM zmmwms_poh
       FOR ALL ENTRIES IN lt_pdlv
     WHERE dlv_planno = lt_pdlv-dlv_planno
       AND closed_h = ''.
  
    SELECT *
      INTO TABLE lt_pol
      FROM zmmwms_pol
       FOR ALL ENTRIES IN lt_pdlv
     WHERE dlv_planno = lt_pdlv-dlv_planno
       AND xclosed = ''.
  
    SELECT *
      INTO TABLE @DATA(lt_temp)
      FROM zmmaps_temp
       FOR ALL ENTRIES IN @lt_pdlv
     WHERE dlv_planno = @lt_pdlv-dlv_planno
       AND inactive = ''.
  
    LOOP AT lt_pdlv INTO DATA(ls_pdlv)  .
  
      CLEAR: ls_poh, lt_pol2[].
  
      READ TABLE lt_poh INTO ls_poh WITH KEY dlv_planno = ls_pdlv-dlv_planno.
  
      LOOP AT lt_pol INTO ls_pol WHERE dlv_planno = ls_pdlv-dlv_planno.
        "RECEIVED_QUAN 送货行已收数量 >= PDLV_MENGE  计划送货数量不传WMS、不更新数据库表
        CHECK ls_pol-received_quan < ls_pol-pdlv_menge.
        APPEND ls_pol TO lt_pol2.
      ENDLOOP.
  
      LOOP AT lt_pol2 INTO ls_pol.
  
        LOOP AT it_pdlv_tmp INTO DATA(ls_pdlv_tmp) WHERE dlv_planno = ls_pol-dlv_planno AND dlv_itemno = ls_pol-dlv_itemno .
          READ TABLE lt_temp INTO DATA(ls_temp) WITH KEY guid    = ls_pdlv_tmp-guid
                                                         uniqueid = ls_pdlv_tmp-uniqueid
                                                         dlv_planno = ls_pol-dlv_planno
                                                         dlv_itemno = ls_pol-dlv_itemno .
          IF sy-subrc = 0.
            IF ls_pdlv_tmp-assign_menge <> ls_temp-assign_menge.
              ls_pol-pdlv_menge = ls_pol-pdlv_menge + ls_pdlv_tmp-assign_menge - ls_temp-assign_menge.
            ENDIF.
          ELSE.
            ls_pol-pdlv_menge = ls_pol-pdlv_menge + ls_pdlv_tmp-assign_menge.
          ENDIF.
          ls_pol-pdlv_date_ext = ls_pdlv_tmp-pdlv_date_ext.
        ENDLOOP.
        MODIFY lt_pol2 FROM ls_pol.
        CLEAR  ls_pol.
      ENDLOOP.
      ls_input-head = ls_poh.
      ls_input-item[] = lt_pol2[].
      "RECEIVED_QUAN 送货行已收数量 >= assign_menge  分配数量不传WMS
      LOOP AT ls_input-item INTO ls_pol.
        IF ls_pol-received_quan >= ls_pol-pdlv_menge.
          "add by lanjingcun 20200519  送货数量大于等于分配数量下发的时候不提示成功 start
          CLEAR  gs_pdlv_data .
          MOVE-CORRESPONDING ls_pol TO gs_pdlv_data .
          gs_pdlv_data-light = icon_led_yellow .
          gs_pdlv_data-message = '送货数量大于等于分配数量，保存但不下发WMS' .
          MODIFY gt_pdlv_data  FROM gs_pdlv_data TRANSPORTING light  message
               WHERE dlv_planno = gs_pdlv_data-dlv_planno
                 AND dlv_itemno = gs_pdlv_data-dlv_itemno
                 .
          "add by lanjingcun 20200519  送货数量大于等于分配数量下发的时候不提示成功 start
          DELETE ls_input-item INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
  
  *MM_020  供应计划下传WMS
      IF ls_input-item[] IS NOT INITIAL.
        CALL FUNCTION 'Z_MM_SEND_APS_PLAN_TO_WMS'
          EXPORTING
            input      = ls_input
            iv_intf_id = 'MM020'
          IMPORTING
            output     = ls_output.
      ELSE.
        ls_output-success = 'S'.
      ENDIF.
      IF ls_output-success = 'S'.
        LOOP AT it_pdlv_tmp INTO ls_pdlv_tmp WHERE dlv_planno = ls_pdlv-dlv_planno.
          UPDATE zmmaps_temp
             SET released = 'X'
                 assign_menge = ls_pdlv_tmp-assign_menge
                 pdlv_date_ext = ls_pdlv_tmp-pdlv_date_ext
                 dlv_planno = ls_pdlv_tmp-dlv_planno
                 dlv_itemno = ls_pdlv_tmp-dlv_itemno
           WHERE guid = ls_pdlv_tmp-guid
             AND uniqueid = ls_pdlv_tmp-uniqueid.
        ENDLOOP.
  
        LOOP AT lt_pol2 INTO ls_pol.
          UPDATE zmmwms_pol
             SET pdlv_menge = ls_pol-pdlv_menge
                 pdlv_date_ext = ls_pol-pdlv_date_ext
           WHERE dlv_planno = ls_pol-dlv_planno
             AND dlv_itemno = ls_pol-dlv_itemno.
        ENDLOOP.
  
      ENDIF.
  
      LOOP AT gt_pdlv_data INTO gs_pdlv_data WHERE update_flag = 'X'
                                                AND light = ''.
  
        READ TABLE it_pdlv_tmp INTO ls_pdlv_tmp WITH KEY guid = gs_pdlv_data-guid uniqueid = gs_pdlv_data-uniqueid.
        CHECK sy-subrc = 0.
  
        IF gs_pdlv_data-dlv_planno IS INITIAL.
          gs_pdlv_data-dlv_planno = ls_pdlv_tmp-dlv_planno.
          gs_pdlv_data-dlv_itemno = ls_pdlv_tmp-dlv_itemno.
          CLEAR: gs_pdlv_data-cell_styles, lt_edit[].
  
          set_disedit_style 'PDLV_DATE'.    "设置栏位不可编辑
          gs_pdlv_data-cell_styles = lt_edit[].
        ENDIF.
  
        gs_pdlv_data-message = ls_output-message.
  
        IF ls_output-success = 'S'.
          gs_pdlv_data-light = icon_led_green.
        ELSE.
          gs_pdlv_data-light = icon_led_red.
        ENDIF.
  
        gs_pdlv_data-zkshsl =  gs_pdlv_data-assign_menge - gs_pdlv_data-received_quan ."ADD BY LANJINGCUN 20200520 增加可收数量字段
  
        CLEAR: gs_pdlv_data-update_flag.
        MODIFY gt_pdlv_data FROM gs_pdlv_data.
        CLEAR  gs_pdlv_data.
      ENDLOOP.
  
    ENDLOOP.
  
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_CLOSE_POL
  *&---------------------------------------------------------------------*
  *& 关闭交货计划
  *&---------------------------------------------------------------------*
  FORM frm_close_pol .
  
    DATA: ls_pdlv_data LIKE gs_pdlv_data,
          lt_pdlv_data LIKE gt_pdlv_data,
          ls_pol       LIKE zmmwms_pol,
          lt_pol       LIKE TABLE OF zmmwms_pol.
  
    LOOP AT gt_pdlv_data INTO gs_pdlv_data WHERE sel = 'X' AND light = ''.
      APPEND gs_pdlv_data TO lt_pdlv_data.
      CLEAR  gs_pdlv_data.
    ENDLOOP.
    IF sy-subrc <> 0.
      MESSAGE '没有需要关闭的数据!' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_pol
      FROM zmmwms_pol
       FOR ALL ENTRIES IN lt_pdlv_data
     WHERE dlv_planno = lt_pdlv_data-dlv_planno
       AND dlv_itemno = lt_pdlv_data-dlv_itemno
       AND xclosed = ''.
  
    SELECT h~*
      FROM @lt_pdlv_data AS alv
      JOIN zmmwms_poh AS h
        ON h~dlv_planno = alv~dlv_planno
     WHERE h~closed_h = ''
      INTO TABLE @DATA(lt_poh).
  
    SELECT l~*
      FROM @lt_pdlv_data AS alv
      JOIN zmmwms_pol AS l
        ON l~dlv_planno = alv~dlv_planno
     WHERE l~xclosed = ''
      INTO TABLE @DATA(lt_pol_db).
  
    SORT lt_poh BY dlv_planno.
    SORT lt_pol_db BY dlv_planno dlv_itemno.
    DELETE ADJACENT DUPLICATES FROM lt_poh COMPARING dlv_planno.
    DELETE ADJACENT DUPLICATES FROM lt_pol_db COMPARING dlv_planno dlv_itemno.
  
    LOOP AT lt_poh INTO DATA(ls_poh).
      DATA(lt_pol_t) = lt_pol[].
      DATA(lt_pol_t2) = lt_pol_db[].
      DELETE lt_pol_t WHERE dlv_planno <> ls_poh-dlv_planno.
      DELETE lt_pol_t2 WHERE dlv_planno <> ls_poh-dlv_planno.
      DESCRIBE TABLE lt_pol_t  LINES DATA(lv_lines1).
      DESCRIBE TABLE lt_pol_t2 LINES DATA(lv_lines2).
      CHECK lv_lines1 = lv_lines2.
      ls_poh-closed_h = 'X'.
      MODIFY lt_poh FROM ls_poh.
      CLEAR  ls_poh.
    ENDLOOP.
  
    DATA: lt_return TYPE ztmm_change_aps_plan_return.
    CALL FUNCTION 'Z_MM_CHANGE_APS_PLAN'
      EXPORTING
        iv_type    = '2'
        iv_intf_id = 'MM020'
      TABLES
        it_poh     = lt_poh
        it_pol     = lt_pol
        it_return  = lt_return.
  
    LOOP AT lt_poh INTO ls_poh.
  
      LOOP AT gt_pdlv_data INTO gs_pdlv_data WHERE light = '' AND dlv_planno = ls_poh-dlv_planno.
        READ TABLE lt_return INTO DATA(ls_return) WITH KEY dlv_planno = gs_pdlv_data-dlv_planno.
        IF sy-subrc = 0.
          "ADD BY LANJINGCUN 20201104 按选中的行项目提示-S928
          READ TABLE lt_pol INTO ls_pol WITH  KEY  dlv_planno = ls_poh-dlv_planno
                                                   dlv_itemno = gs_pdlv_data-dlv_itemno .
          IF sy-subrc = 0 .
            "ADD BY LANJINGCUN 20201104 按选中的行项目提示-E928
            gs_pdlv_data-message = ls_return-message.
            IF ls_return-success = 'S'.
              gs_pdlv_data-light = icon_led_green.
              gs_pdlv_data-message = '关闭成功'.
              gs_pdlv_data-xclosed = 'X'.
  
              "add by lanjingcun 20200824 关闭成功的时候 送货行已收为0 清空交期-S1
              IF gs_pdlv_data-received_quan = 0 .
                CLEAR gs_pdlv_data-pdlv_date  .
                UPDATE zmmaps_temp
  *           SET RELEASED = 'X'
  *               DLV_PLANNO = LS_POL-DLV_PLANNO
  *               DLV_ITEMNO = LS_POL-DLV_ITEMNO
                    SET   pdlv_date = gs_pdlv_data-pdlv_date "确认计划交货日期"
  *               ASSIGN_MENGE = GS_PDLV_DATA-ASSIGN_MENGE "分配数量
                 WHERE guid = gs_pdlv_data-guid
                   AND uniqueid = gs_pdlv_data-uniqueid.
                COMMIT WORK AND WAIT .
              ENDIF .
  *      UPDATE ZMMWMS_POL
  *         SET PDLV_DATE = '00000000'
  *         WHERE DLV_PLANNO = GS_PDLV_DATA-DLV_PLANNO
  *           AND DLV_ITEMNO = GS_PDLV_DATA-DLV_ITEMNO .
              "add by lanjingcun 20200824 关闭成功的时候 清空交期-E1
            ELSE.
              gs_pdlv_data-light = icon_led_red.
            ENDIF.
          ENDIF .
        ENDIF.
  
        MODIFY gt_pdlv_data FROM gs_pdlv_data.
        CLEAR  gs_pdlv_data.
      ENDLOOP.
  
    ENDLOOP.
  
  
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_SEND_WMS
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  FORM frm_send_wms .
  
    TYPES: BEGIN OF ty_head,
             lifnr    LIKE zmmaps_poh-lifnr,
             name1_gp LIKE zmmaps_poh-name1_gp,
             ernam    LIKE zmmaps_poh-ernam,
             note     LIKE zmmaps_poh-note,
             lgort    LIKE zmmaps_pol-lgort,
           END OF ty_head.
  
    DATA: lt_alv     TYPE TABLE OF ty_head,
          lt_alvl    TYPE TABLE OF ty_head,
          ls_alv     TYPE ty_head,
          ls_poh     LIKE zmmwms_poh,
          lt_poh     LIKE TABLE OF zmmwms_poh,
          lt_pol     LIKE TABLE OF zmmwms_pol,
          lt_poh_add LIKE TABLE OF zmmwms_pol,
          ls_pol     LIKE zmmwms_pol,
          lv_item    LIKE zmmwms_pol-dlv_itemno.
  
    DATA: lt_pol_temp TYPE TABLE OF ty_pol_temp.
    DATA: lt_pol_temp1 TYPE TABLE OF ty_pol_temp.
    DATA: ls_pol_temp1 TYPE  ty_pol_temp.
  
    DATA ls_pol_temp LIKE LINE OF lt_pol_temp .
    DATA ls_pdlv_data LIKE LINE OF gt_pdlv_data .
  
  *  DATA: lv_item LIKE zmmwms_pol-dlv_itemno.
  
  
    DATA: ls_input  TYPE zszmmwms_poh_pol,
          ls_output TYPE zsmessage_out.
    DATA:lv_save TYPE flag.
    "DEL BY LANJINGCUN 20200426  修改不需要对应pol进行合并 后续pol与poh（取消） 一一对应-1 star
    LOOP AT gt_pdlv_data INTO gs_pdlv_data WHERE sel = 'X' AND light IS INITIAL AND pz = 'X' AND lgort IS NOT INITIAL AND pdlv_date IS NOT INITIAL.
  *  LOOP AT GT_PDLV_DATA INTO GS_PDLV_DATA WHERE UPDATE_FLAG = 'X' AND LIGHT IS INITIAL AND PZ = 'X' AND LGORT IS NOT INITIAL AND PDLV_DATE IS NOT INITIAL.
      MOVE-CORRESPONDING gs_pdlv_data TO ls_alv.
      COLLECT ls_alv INTO lt_alv.
      CLEAR   ls_alv.
    ENDLOOP.
    IF sy-subrc <> 0.
      MESSAGE '没有找到可以传输的数据，请检查审批状态，库位等信息' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  
    "add by lanjingcun 20200705 按供应商判断是否进行拆分-S1
  ***********************拆分下发***********
    CLEAR lt_alvl[] .
    LOOP AT lt_alv INTO ls_alv.
  
      CLEAR: ls_poh, lt_pol[], lv_item, lt_pol_temp[].
  
      MOVE-CORRESPONDING ls_alv TO ls_poh.
  
      "add by lanjingcun 20200705 按供应商判断是否进行拆分-S1
      SELECT COUNT(*)
        FROM zmmt0011
        WHERE lifnr = ls_alv-lifnr
          AND zcfbs = 'X' .
      IF  sy-subrc <> 0 .
        APPEND ls_alv TO  lt_alvl ."保存需要做合并的供应商
        CONTINUE .
      ENDIF .
  
      "add by lanjingcun 20200705 按供应商判断是否进行拆分-E1
  
  
      "add BY LANJINGCUN 20200426  修改不需要对应pol进行合并 后续pol与poh （取消）一一对应-2 start
  *    LOOP AT GT_PDLV_DATA INTO GS_PDLV_DATA WHERE  UPDATE_FLAG = 'X'
      LOOP AT gt_pdlv_data INTO gs_pdlv_data WHERE  sel = 'X'
                                              AND light IS INITIAL
                                              AND lifnr = ls_alv-lifnr
                                              AND pz = 'X'
                                              AND lgort IS NOT INITIAL
                                              AND pdlv_date IS NOT INITIAL.
  
  
        CLEAR: ls_poh, lt_pol[], lv_item, lt_pol_temp[].
  
        MOVE-CORRESPONDING gs_pdlv_data TO ls_poh.
  
  *获取主键
        PERFORM frm_get_num_key CHANGING ls_poh-dlv_planno.
  
        ls_poh-origin = 'X'.
        CLEAR  ls_pol.
        MOVE-CORRESPONDING gs_pdlv_data TO ls_pol.
  
        ls_pol-dlv_planno = ls_poh-dlv_planno.
        ls_pol-pdlv_menge = gs_pdlv_data-assign_menge.
        ls_pol-origin = 'X'.
  
        APPEND ls_pol TO lt_pol.
  
  
        PERFORM frm_get_pol_temp IN PROGRAM zmmr0020 TABLES lt_pol lt_pol_temp.
  
        ls_input-head = ls_poh.
        ls_input-item[] = lt_pol[].
  
  *MM_020  供应计划下传WMS
        CALL FUNCTION 'Z_MM_SEND_APS_PLAN_TO_WMS'
          EXPORTING
            input      = ls_input
            iv_intf_id = 'MM020'
          IMPORTING
            output     = ls_output.
  
        IF ls_output-success = 'S'.
  
          APPEND ls_poh TO lt_poh.
  
          APPEND LINES OF lt_pol TO lt_poh_add.
  
          "UP BY LANJC 20200820 因为是拆分下传 所以直接更新temp表就可以-s08201
  *        LOOP AT LT_POL INTO LS_POL.
  *          "add byljm 20200305
  *          "替代保存按钮操作，业务要求下发WMS时保存数据。发现保存按钮所做的操作就是把更新ZMMAPS_TEMP的pdlv_date、assign_menge
  *          "这2个字段值，所以在下面UPDATE zmmaps_temp语句增加了2个字段。这里特备注下
  *          "end by ljm 20200305
  *          READ TABLE GT_PDLV_DATA WITH KEY GUID = LS_POL-GUID UNIQUEID = LS_POL-UNIQUEID INTO LS_PDLV_DATA."del by lanjc 20200820  不需要更新日期和数量
  *          IF LS_POL-DUOGE = 'X'.
  *            LOOP AT LT_POL_TEMP INTO LS_POL_TEMP WHERE DLV_PLANNO = LS_POL-DLV_PLANNO AND DLV_ITEMNO = LS_POL-DLV_ITEMNO.
  *              UPDATE ZMMAPS_TEMP
  *                 SET RELEASED = 'X'
  *                     DLV_PLANNO = LS_POL-DLV_PLANNO
  *                     DLV_ITEMNO = LS_POL-DLV_ITEMNO
  **                     PDLV_DATE = LS_PDLV_DATA-PDLV_DATE "确认计划交货日期"del by lanjc 20200820  不需要更新日期和数量
  **                     ASSIGN_MENGE = LS_PDLV_DATA-ASSIGN_MENGE "分配数量"del by lanjc 20200820  不需要更新日期和数量
  *              WHERE GUID = LS_POL_TEMP-GUID
  *                AND UNIQUEID = LS_POL_TEMP-UNIQUEID.
  *            ENDLOOP.
  *          ELSE.
  *            UPDATE ZMMAPS_TEMP
  *               SET RELEASED = 'X'
  *                   DLV_PLANNO = LS_POL-DLV_PLANNO
  *                   DLV_ITEMNO = LS_POL-DLV_ITEMNO
  **                   PDLV_DATE = LS_PDLV_DATA-PDLV_DATE "确认计划交货日期"del by lanjc 20200820  不需要更新日期和数量
  **                   ASSIGN_MENGE = LS_PDLV_DATA-ASSIGN_MENGE "分配数量"del by lanjc 20200820  不需要更新日期和数量
  *             WHERE GUID = LS_POL-GUID
  *               AND UNIQUEID = LS_POL-UNIQUEID.
  *          ENDIF.
  *        ENDLOOP.
  
          CLEAR ls_pol .
          READ TABLE lt_pol INTO ls_pol INDEX 1 .
          UPDATE zmmaps_temp
             SET released = 'X'
                 dlv_planno = ls_pol-dlv_planno
                 dlv_itemno = ls_pol-dlv_itemno
                 pdlv_date = gs_pdlv_data-pdlv_date "确认计划交货日期"
                 assign_menge = gs_pdlv_data-assign_menge "分配数量
           WHERE guid = gs_pdlv_data-guid
             AND uniqueid = gs_pdlv_data-uniqueid.
          "UP BY LANJC 20200820 因为是拆分下传 所以直接更新temp表就可以-e08201
        ELSE.
          CLEAR: lt_pol[], gs_alv, lt_pol_temp[].
        ENDIF.
  
        CLEAR: lv_item.
  
        gs_pdlv_data-message = ls_output-message.
        IF ls_output-success = 'S'.
          gs_pdlv_data-dlv_planno = ls_poh-dlv_planno.
          gs_pdlv_data-dlv_itemno = '00010'.
  *        lv_item = lv_item + 10.
  *        gs_alv-dlv_itemno = lv_item.
        ENDIF.
  
        IF ls_output-success = 'S'.
          gs_pdlv_data-light = icon_led_green.
        ELSE.
          gs_pdlv_data-light = icon_led_red.
        ENDIF.
  
        MODIFY gt_pdlv_data FROM gs_pdlv_data.
        CLEAR  gs_pdlv_data.
  
      ENDLOOP.
  
    ENDLOOP .
    lt_alv[] = lt_alvl[] ."还需要处理的合并供应商
    "add by lanjingcun 20200705 按供应商判断是否进行拆分-E1
  
  
  
  ***********************合并下发***********************
    LOOP AT lt_alv INTO ls_alv.
  
      CLEAR: ls_poh, lt_pol[], lv_item, lt_pol_temp[].
  
      MOVE-CORRESPONDING ls_alv TO ls_poh.
  
  
  *获取主键
      PERFORM frm_get_num_key CHANGING ls_poh-dlv_planno.
  
  
      ls_poh-origin = 'X'.
  
      CLEAR lv_item  .
  *    LOOP AT GT_PDLV_DATA INTO GS_PDLV_DATA WHERE UPDATE_FLAG = 'X'
      LOOP AT gt_pdlv_data INTO gs_pdlv_data WHERE sel = 'X'
                                               AND light IS INITIAL
                                               AND pdlv_date IS NOT INITIAL
                                               AND lifnr = ls_alv-lifnr
                                               AND lgort = ls_alv-lgort.
  *  "DEL BY LANJINGCUN 20200426  修改不需要对应pol进行合并 后续pol与poh（取消） 一一对应-1 end
  
  *  "add BY LANJINGCUN 20200426  修改不需要对应pol进行合并 后续pol与poh （取消）一一对应-2 start
  *  LOOP AT GT_PDLV_DATA INTO GS_PDLV_DATA WHERE  SEL = 'X'
  *                                          AND LIGHT IS INITIAL
  *                                          AND PZ = 'X'
  *                                          AND LGORT IS NOT INITIAL
  *                                          AND PDLV_DATE IS NOT INITIAL.
  *
  *
  *    CLEAR: LS_POH, LT_POL[], LV_ITEM, LT_POL_TEMP[].
  *
  *    MOVE-CORRESPONDING GS_PDLV_DATA TO LS_POH.
  *
  **获取主键
  *    PERFORM FRM_GET_NUM_KEY CHANGING LS_POH-DLV_PLANNO.
  *
  *    LS_POH-ORIGIN = 'X'.
  *    "add BY LANJINGCUN 20200426  修改不需要对应pol进行合并 后续pol与poh （取消）一一对应-2 end
  
  
        MOVE-CORRESPONDING gs_pdlv_data TO ls_pol.
        lv_item  = lv_item  + 10 .
        ls_pol-dlv_itemno = lv_item .
        ls_pol-dlv_planno = ls_poh-dlv_planno.
        ls_pol-pdlv_menge = gs_pdlv_data-assign_menge.
        ls_pol-origin = 'X'.
  
        APPEND ls_pol TO lt_pol.
        CLEAR  ls_pol.
  
      ENDLOOP."DEL BY LANJINGCUN 20200426  修改不需要对应pol进行合并 后续pol与poh（取消）
  
  *    PERFORM frm_get_pol_temp IN PROGRAM zmmr0020 TABLES lt_pol lt_pol_temp."del by lanjingcun 取消合并功能20201103
      "ADD BY LANJC 20200820 经过前一行的代码 当交货单行计划交期一样时lt_Pol会被合并 合并的值会放在temp
      "因为不合并不会放到LT_POL LT_POL_TEMP 为后续的运算方便在此将lt_POL一并加入 LT_POL_TEMP .
      CLEAR lt_pol_temp1[] .
      MOVE-CORRESPONDING lt_pol  TO lt_pol_temp1 .
      APPEND LINES OF lt_pol_temp1 TO lt_pol_temp .
      "END
  
  
  
      ls_input-head = ls_poh.
      ls_input-item[] = lt_pol[].
  
  *MM_020  供应计划下传WMS
      CALL FUNCTION 'Z_MM_SEND_APS_PLAN_TO_WMS'
        EXPORTING
          input      = ls_input
          iv_intf_id = 'MM020'
        IMPORTING
          output     = ls_output.
  
      IF ls_output-success = 'S'.
  
        APPEND ls_poh TO lt_poh.
  
        APPEND LINES OF lt_pol TO lt_poh_add.
        "UP BY LANJC 20200820 当进行多行合并下发时，下发会再进行自动合并，需要逻辑调整-s202008202
  *      LOOP AT LT_POL INTO LS_POL.
  *        "add byljm 20200305
  *        "替代保存按钮操作，业务要求下发WMS时保存数据。发现保存按钮所做的操作就是把更新ZMMAPS_TEMP的pdlv_date、assign_menge
  *        "这2个字段值，所以在下面UPDATE zmmaps_temp语句增加了2个字段。这里特备注下
  *        "end by ljm 20200305
  **        READ TABLE GT_PDLV_DATA WITH KEY GUID = LS_POL-GUID UNIQUEID = LS_POL-UNIQUEID INTO LS_PDLV_DATA."del by lanjc 20200820  不需要更新日期和数量
  *        IF LS_POL-DUOGE = 'X'.
  *
  *          LOOP AT LT_POL_TEMP INTO LS_POL_TEMP WHERE DLV_PLANNO = LS_POL-DLV_PLANNO AND DLV_ITEMNO = LS_POL-DLV_ITEMNO.
  *            UPDATE ZMMAPS_TEMP
  *               SET RELEASED = 'X'
  *                   DLV_PLANNO = LS_POL-DLV_PLANNO
  *                   DLV_ITEMNO = LS_POL-DLV_ITEMNO
  **                   PDLV_DATE = LS_PDLV_DATA-PDLV_DATE "确认计划交货日期
  **                   ASSIGN_MENGE = LS_PDLV_DATA-ASSIGN_MENGE "分配数量
  *            WHERE GUID = LS_POL_TEMP-GUID
  *              AND UNIQUEID = LS_POL_TEMP-UNIQUEID.
  *          ENDLOOP.
  *        ELSE.
  *          UPDATE ZMMAPS_TEMP
  *             SET RELEASED = 'X'
  *                 DLV_PLANNO = LS_POL-DLV_PLANNO
  *                 DLV_ITEMNO = LS_POL-DLV_ITEMNO
  **                 PDLV_DATE = LS_PDLV_DATA-PDLV_DATE "确认计划交货日期
  **                 ASSIGN_MENGE = LS_PDLV_DATA-ASSIGN_MENGE "分配数量
  *           WHERE GUID = LS_POL-GUID
  *             AND UNIQUEID = LS_POL-UNIQUEID.
  *        ENDIF.
  *      ENDLOOP.
        CLEAR lt_pol_temp1[] .
        lt_pol_temp1[] = lt_pol_temp[] .
        SORT lt_pol_temp1 BY guid uniqueid .
        DELETE ADJACENT DUPLICATES FROM lt_pol_temp1
                            COMPARING guid uniqueid .
        LOOP AT lt_pol_temp1 INTO ls_pol_temp1 .
  
          READ TABLE gt_pdlv_data WITH KEY guid = ls_pol_temp1-guid uniqueid = ls_pol_temp1-uniqueid INTO ls_pdlv_data.
          IF sy-subrc = 0 .
            UPDATE zmmaps_temp
               SET released = 'X'
                   dlv_planno = ls_pol_temp1-dlv_planno
                   dlv_itemno = ls_pol_temp1-dlv_itemno
                   pdlv_date = ls_pdlv_data-pdlv_date "确认计划交货日期"
                   assign_menge = ls_pdlv_data-assign_menge "分配数量
             WHERE guid = ls_pdlv_data-guid
               AND uniqueid = ls_pdlv_data-uniqueid.
          ENDIF .
  
        ENDLOOP .
  
  
  
        "UP BY LANJC 20200820 当进行多行合并下发时，下发会再进行自动合并，需要逻辑调整-s202008202
      ELSE.
        CLEAR: lt_pol[], gs_alv, lt_pol_temp[].
      ENDIF.
  
      CLEAR: lv_item.
  
      "DEL BY LANJINGCUN 20200426  修改不需要对应pol进行合并 后续pol与po（取消）h 一一对应-3-satrt
  *    LOOP AT GT_PDLV_DATA INTO GS_PDLV_DATA WHERE UPDATE_FLAG = 'X'
      LOOP AT gt_pdlv_data INTO gs_pdlv_data WHERE sel = 'X'
                                                AND light IS INITIAL
                                                AND pdlv_date IS NOT INITIAL
                                                AND lifnr = ls_alv-lifnr
                                                AND lgort = ls_alv-lgort.
        "DEL BY LANJINGCUN 20200426  修改不需要对应pol进行合并 后续pol与poh （取消）一一对应-3-end
        gs_pdlv_data-message = ls_output-message.
        IF ls_output-success = 'S'.
          gs_pdlv_data-dlv_planno = ls_poh-dlv_planno.
  *        lv_item = lv_item + 10.
  *        gs_alv-dlv_itemno = lv_item.
        ENDIF.
  
        IF ls_output-success = 'S'.
          gs_pdlv_data-light = icon_led_green.
        ELSE.
          gs_pdlv_data-light = icon_led_red.
        ENDIF.
  
        MODIFY gt_pdlv_data FROM gs_pdlv_data.
        CLEAR  gs_pdlv_data.
      ENDLOOP."DEL BY LANJINGCUN 20200426  修改不需要对应pol进行合并 后续pol与poh（取消）
  
    ENDLOOP.
  
  
    IF lt_poh[] IS NOT INITIAL.
      MODIFY zmmwms_poh FROM TABLE lt_poh.
      MODIFY zmmwms_pol FROM TABLE lt_poh_add.
  *    MODIFY zmmwms_pol_temp FROM TABLE lt_pol_temp_add.
    ENDIF.
  
    COMMIT WORK.
  
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_CLOSE_TEMP
  *&---------------------------------------------------------------------*
  *& 关闭匹配数据
  *&---------------------------------------------------------------------*
  FORM frm_close_temp .
  
    DATA: ls_pdlv_data LIKE gs_pdlv_data,
          lt_pdlv_data LIKE gt_pdlv_data,
          lt_pol       LIKE TABLE OF zmmwms_pol.
  
    LOOP AT gt_pdlv_data INTO gs_pdlv_data WHERE sel = 'X' AND light = ''.
      UPDATE zmmaps_temp
         SET inactive = 'X'
       WHERE guid = gs_pdlv_data-guid
         AND uniqueid = gs_pdlv_data-uniqueid.
      gs_pdlv_data-message = '关闭成功'.
      gs_pdlv_data-xclosed = 'X'.
      gs_pdlv_data-light = icon_led_green.
      MODIFY gt_pdlv_data FROM gs_pdlv_data.
      CLEAR  gs_pdlv_data.
    ENDLOOP.
    IF sy-subrc <> 0.
      MESSAGE '没有需要关闭的数据!' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  
    COMMIT WORK.
  
  ENDFORM.