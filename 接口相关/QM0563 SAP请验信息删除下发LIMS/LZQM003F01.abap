*----------------------------------------------------------------------*
***INCLUDE LZQM003F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module      :  QM
*& Author      :  SHUAIQUAN
*& Date        :  2020.11.20
*& Description :  lims与SAP相关接口子例程
*&---------------------------------------------------------------------*
*& Modified Recorder :
*& Date         C#NO          Author  Content
*& ----------   ----------    -----   ---------------
*& 2021.11.16   S4DK905288    04815   QM：QM057样品取样量上传SAP扣减需求调整-20211116
*&---------------------------------------------------------------------*

"定义处理HTC的转换函数"20220429 Seashell
FORM htc_change CHANGING ls_input_row TYPE zbatch_dto.
    DATA:zkunnr1      TYPE zkunnr1,
         zkunnr2      TYPE zkunnr2,
         zmatnr       TYPE matnr,
         lv_kunnr(40).
    CLEAR:zkunnr1,zkunnr2,lv_kunnr.
    IF ls_input_row-kdauf IS NOT INITIAL.
      "保留售达方的信息
      lv_kunnr = ls_input_row-t_kunnr.
      "反查售达方的编号
      SELECT SINGLE
        kunnr
        FROM vbak
        WHERE vbak~vbeln = @ls_input_row-kdauf
        INTO @zkunnr1.
  *      zkunnr1 = ls_input_row-t_kunnr.
  
      zmatnr = ls_input_row-matnr.
      zmatnr = zcl_bc_public=>conv_by_ddic( i_input = zmatnr i_abap = 'MATNR').
      zkunnr2 = ls_input_row-sjkhcode.
      zkunnr2 = zcl_bc_public=>conv_by_ddic( i_input = zkunnr2 i_abap = 'KUNNR').
      "如果是半成品物料的话需要反查对应的销售订单，然后查到对应的成品物料
      SELECT SINGLE
        mtart
      FROM mara
      INTO @DATA(lv_mtart)
      WHERE matnr = @zmatnr.
      IF lv_mtart = 'Z030'.
        SELECT SINGLE
          matnr,
          kunnr
        FROM aufk
        INNER JOIN vbpa
        ON vbpa~vbeln = aufk~zkdauf_s
        INNER JOIN vbap
        ON vbap~vbeln = aufk~zkdauf_s
        AND vbap~posnr = aufk~zkdpos_s
        WHERE vbpa~parvw = 'Z1'
          AND aufk~aufnr = @ls_input_row-aufnr
        INTO ( @zmatnr,@zkunnr2 ).
  
      ENDIF.
  
      "调用通用转换函数
      CALL FUNCTION 'Z_SD_SO_CHECK_015'
        EXPORTING
          matnr     = zmatnr
          i_zkunnr1 = zkunnr1
          i_zkunnr2 = zkunnr2
        IMPORTING
          o_zkunnr1 = zkunnr1
          o_zkunnr2 = zkunnr2.
  *        ls_input_row-t_kunnr = zkunnr1.
  *    S4DK905926-S
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = zkunnr2
            IMPORTING
              output = ls_input_row-sjkhcode.
  *    ls_input_row-sjkhcode = zkunnr2.
  *    S4DK905926-E
      "如果相同的话证明发生了替换，需要把名称也转换
      IF zkunnr1 = zkunnr2.
        ls_input_row-sjkh = lv_kunnr.
      ENDIF.
    ENDIF.
  
  *      "转换判断结束
  ENDFORM.
  
  
  
  
  
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_BATCHINFO
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> LT_BATCH_CHARA
  *&      --> LS_MCHA
  *&---------------------------------------------------------------------*
  
  
  
  
  FORM frm_get_batchinfo  TABLES   pt_batch_chara STRUCTURE gs_batch_chara
                          USING    ps_mcha LIKE mcha.
  
    CHECK ps_mcha IS NOT INITIAL.
  
    CLEAR:pt_batch_chara[].
    CLEAR:gt_batch_chara.
  
  * 批次信息
    DATA: lv_objek              TYPE cuobn,
          lv_obtab              TYPE tabelle,
          lv_klart              TYPE klassenart,
          lv_class              TYPE klasse_d,
          lv_fltp_to_string(16) TYPE c,
          "批次特性通用返回类型表 num型 char型 curr型
          lt_num                TYPE TABLE OF bapi1003_alloc_values_num,
          lt_char               TYPE TABLE OF bapi1003_alloc_values_char,
          lt_curr               TYPE TABLE OF bapi1003_alloc_values_curr,
          lt_return             TYPE TABLE OF bapiret2.
  
    CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
      EXPORTING
        i_matnr = ps_mcha-matnr
        i_charg = ps_mcha-charg
        i_werks = ps_mcha-werks
      IMPORTING
        e_objek = lv_objek "对象
        e_obtab = lv_obtab "表
        e_klart = lv_klart "类型
        e_class = lv_class. "类
  
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey_long  = lv_objek
        objecttable     = lv_obtab
        classnum        = lv_class
        classtype       = lv_klart
      TABLES
        allocvaluesnum  = lt_num
        allocvalueschar = lt_char
        allocvaluescurr = lt_curr
        return          = lt_return.
  
    READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'S'.
    CHECK sy-subrc = 0.
  
    SELECT atnam,
           atfor,
           anzst
      FROM cabn
      INTO TABLE @DATA(lt_cabn).
  
    SORT  lt_cabn BY atnam.
  
    DATA: lv_sollwert TYPE qsollwertc,
          lv_qstellen TYPE qstellen.
  
    LOOP AT lt_num INTO DATA(ls_valuesum).
      CLEAR gs_batch_chara.
      gs_batch_chara-atnam = ls_valuesum-charact.
      gs_batch_chara-atbez = ls_valuesum-charact_descr.
      gs_batch_chara-atwrt = ls_valuesum-value_from.
  
  *   日期类型NUM转CHAR显示
      READ TABLE lt_cabn WITH KEY atnam = ls_valuesum-charact BINARY SEARCH INTO DATA(ls_cabn).
      IF sy-subrc = 0.
        gs_batch_chara-atfor = ls_cabn-atfor."格式(DATE、NUM）
        gs_batch_chara-anzst = ls_cabn-anzst."字符个数
        CASE ls_cabn-atfor.
          WHEN 'DATE'.
            CLEAR lv_sollwert.
            CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
              EXPORTING
                i_number_of_digits = 0
                i_fltp_value       = ls_valuesum-value_from
  *             I_VALUE_NOT_INITIAL_FLAG       = 'X'
  *             I_SCREEN_FIELDLENGTH           = 16
              IMPORTING
                e_char_field       = lv_sollwert.
            CONDENSE lv_sollwert NO-GAPS.
            IF lv_sollwert = '0'.
              CLEAR lv_sollwert.
            ENDIF.
            gs_batch_chara-atwrt = lv_sollwert.
          WHEN 'NUM'."转字符格式显示
            CLEAR lv_sollwert.
            lv_qstellen = CONV #( ls_cabn-anzst )."字符个数
            CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
              EXPORTING
                i_number_of_digits = lv_qstellen "字符个数
                i_fltp_value       = ls_valuesum-value_from
  *             I_VALUE_NOT_INITIAL_FLAG       = 'X'
  *             I_SCREEN_FIELDLENGTH           = 16
              IMPORTING
                e_char_field       = lv_sollwert.
            CONDENSE lv_sollwert NO-GAPS.
            gs_batch_chara-atwrt = zcl_bc_public=>trailing_quan_zero( CONV #( lv_sollwert ) ).
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
      APPEND gs_batch_chara TO gt_batch_chara.
    ENDLOOP.
  
  *
    LOOP AT lt_char INTO DATA(ls_valueschar).
      CLEAR gs_batch_chara.
      gs_batch_chara-atnam = ls_valueschar-charact.
      gs_batch_chara-atbez = ls_valueschar-charact_descr.
      gs_batch_chara-atwrt = ls_valueschar-value_char_long.
      READ TABLE lt_cabn WITH KEY atnam = ls_valueschar-charact BINARY SEARCH INTO ls_cabn.
      IF sy-subrc = 0.
        gs_batch_chara-atfor = ls_cabn-atfor."格式(DATE、NUM）
        gs_batch_chara-anzst = ls_cabn-anzst."字符个数
      ENDIF.
      APPEND gs_batch_chara TO gt_batch_chara.
    ENDLOOP.
  
  
    pt_batch_chara[] = gt_batch_chara.
    CLEAR:gt_batch_chara.
  
  ENDFORM.
  
  
  *&---------------------------------------------------------------------*
  *& Form frm_get_qm0561
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      <-- E_OUTPUT
  *&---------------------------------------------------------------------*
  FORM frm_get_qm0561 USING ps_input TYPE zqms_lims_0003
                      CHANGING ps_output TYPE zqms_ret
                               ps_data_0003 LIKE gs_data_0003.
  
  
    DATA:ls_qals       LIKE qals.  "检验批记录
    DATA:ls_mcha       LIKE mcha.  "批次
    DATA:ls_but000_gy  LIKE but000."供应商
    DATA:ls_but000_zz  LIKE but000."制造商
    DATA:ls_but000_sdf LIKE but000."售达方
    DATA:ls_kna1       LIKE kna1.  "客户主数据
    DATA : ls_lfa1     LIKE lfa1 .
    DATA:ls_afko       LIKE afko.  "订单号
    DATA:ls_vbak       LIKE vbak.  "销售凭证 ： 抬头数据
    DATA : ls_vbpa TYPE vbpa .
    DATA:ls_t005t      LIKE t005t. "国家名
    DATA:lt_t005t      LIKE STANDARD TABLE OF t005t. "国家名
    DATA:ls_t006a      LIKE t006a. "单位
    DATA:lt_t006a      LIKE STANDARD TABLE OF t006a.
    DATA:ls_aufk       LIKE aufk.
    DATA:ls_resb       LIKE resb.
    DATA:lt_resb       LIKE STANDARD TABLE OF resb.
    DATA:ls_mara      LIKE mara.
  
  * 批次信息
    DATA:lt_batch_chara LIKE STANDARD TABLE OF gs_batch_chara.
    DATA:ls_batch_chara LIKE  gs_batch_chara.
  
    CLEAR:ps_data_0003.
  
  
  * 获取单位信息
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_t006a
      FROM t006a
     WHERE spras = sy-langu.
  
    SORT lt_t006a BY msehi.
  
  * 获取国家名
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_t005t
      FROM t005t
     WHERE spras = sy-langu.
  
    SORT lt_t005t BY land1.
  
  
  * 获取检验批记录信息
    DO  20 TIMES.
      SELECT SINGLE *
        INTO ls_qals
        FROM qals
       WHERE prueflos = ps_input-prueflos.
      IF sy-subrc <> 0 .
        WAIT UP TO 1 SECONDS .
      ELSE .
        EXIT .
      ENDIF .
    ENDDO.
  
    IF ls_qals IS  INITIAL.
      ps_output-code  = 'E'.
      ps_output-msg = '没有查询到对应的检验批号'.
      EXIT.
    ENDIF.
  
  
  
  
    IF ls_qals-prueflos  IS NOT INITIAL.
      ps_data_0003-prueflos    =   ls_qals-prueflos.  "请验单号
      ps_data_0003-art         =   ls_qals-art.       "检验类型
      ps_data_0003-matnr       =   ls_qals-matnr.     "物料编码
      ps_data_0003-werk        =   ls_qals-werk.      "工厂
  *    PS_DATA_0003-CHARG       =  '19491001'." LS_QALS-CHARG.     "SAP批次号
      ps_data_0003-charg       =   ls_qals-charg.     "SAP批次号
      ps_data_0003-lagortchrg  =   ls_qals-lagortchrg."库存地
      ps_data_0003-quickflag  =   ls_qals-zquick."快速标识 S4DK905625
  
  
  *      物料工厂批次信息
      SELECT SINGLE *
        INTO CORRESPONDING FIELDS OF ls_mcha
        FROM mcha
       WHERE matnr = ls_qals-matnr
         AND werks = ls_qals-werk
         AND charg = ls_qals-charg.
  *
      ps_data_0003-lwedt   = ls_mcha-lwedt. "到货日期
      ps_data_0003-licha    = ls_mcha-licha. "供应商批次号
      ps_data_0003-w_hsdat  = ls_mcha-hsdat. "物料生产日期
      ps_data_0003-w_vfdat  = ls_mcha-vfdat. "物料有效日期
  
      SELECT SINGLE *
        INTO CORRESPONDING FIELDS OF  ls_mara
        FROM mara
       WHERE matnr = ls_mcha-matnr.
  
  
  *       供应商 modify S4DK904914 start
      CLEAR:ls_but000_gy.
      SELECT SINGLE *
        INTO ls_but000_gy
        FROM but000
       WHERE partner = ls_qals-lifnr.
  
      ps_data_0003-t_lifnr       =  ls_but000_gy-name_org1."供应商名称
      ps_data_0003-lifnr         =  ls_qals-lifnr.         "供应商代码
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ps_data_0003-lifnr
        IMPORTING
          output = ps_data_0003-lifnr.
  *    PS_DATA_0003-LIFNR         =  LS_BUT000_GY-BU_SORT1.         "供应商代码
  
  *       制造商
  *old 20210629
  *"由QALS-PRUEFLOS取出QALS-EBELN，再由EKKO-EBELN=QALS-EBELN取出EKKO-BSART。
  *若EKKO-BSART字段的值不等于ZJS，则由QALS-PRUEFLOS取出QALS-HERSTELLER，再由BUT000-PARNER=QALS-HERSTELLER取出BUT000-NAME_ORG1。
  *若EKKO-BSART字段的值等于ZJS，则由EKPO-EBELN=QALS-EBELN取出EKPO-ZMFRNR，再由BUT000-PARNER=EKPO-ZMFRNR取出BUT000-NAME_ORG1。"
  *new 20210701
  *"若QALS-HERSTELLER非空，则取QALS-HERSTELLER；
  *若QALS-HERSTELLER为空，由EKPO-EBELN=QALS-EBELN取出EKPO-ZMFRNR。"
      DATA : lv_partner TYPE but000-partner .
      IF ls_qals-hersteller IS INITIAL.
        SELECT SINGLE ekpo~zmfrnr
              INTO lv_partner
              FROM ekpo
              WHERE ekpo~ebeln = ls_qals-ebeln
                AND ekpo~ebelp = ls_qals-ebelp .
      ELSE.
        lv_partner = ls_qals-hersteller.
      ENDIF.
  
  *    LV_PARTNER = LS_QALS-HERSTELLER .
  
  
      CLEAR:ls_but000_zz.
      SELECT SINGLE *
        INTO ls_but000_zz
        FROM but000
       WHERE partner = lv_partner .
      ps_data_0003-t_hersteller       =  ls_but000_zz-name_org1."制造商
      ps_data_0003-mfgcode            =  lv_partner."制造商编号 add S4DK904914
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ps_data_0003-mfgcode
        IMPORTING
          output = ps_data_0003-mfgcode.
  *modify S4DK904914 end
  
  *       获取供应商等级
      IF ls_mara-mtart = 'Z010'.
        lv_partner = ls_qals-hersteller .
      ELSEIF ls_mara-mtart = 'Z020' OR ls_mara-mtart = 'Z090' .
        lv_partner = ls_qals-lifnr .
      ENDIF .
      CLEAR:ls_lfa1.
      SELECT SINGLE *
        INTO ls_lfa1
        FROM lfa1
  *     WHERE LIFNR = LS_QALS-LIFNR.
       WHERE lifnr = lv_partner.
      IF sy-subrc = 0 .
        SELECT SINGLE regiogroup
          INTO ps_data_0003-zmfr_grd
          FROM adrc
          WHERE addrnumber = ls_lfa1-adrnr .
  *    PS_DATA_0003-ZMFR_GRD  =   LS_KNA1-KATR1."供应商等级
      ENDIF .
  
  *       单位
      IF lt_t006a[] IS NOT INITIAL.
        CLEAR:ls_t006a.
        READ TABLE lt_t006a INTO ls_t006a WITH KEY msehi = ls_qals-mengeneinh BINARY SEARCH."
        ps_data_0003-t_mengeneinh = ls_t006a-mseh3.
      ENDIF.
  
  *    SELECT SINGLE BU_SORT1
  *      INTO PS_DATA_0003-LIFNR
  *      FROM BUT000
  *      WHERE PARTNER = LS_QALS-LIFNR.
  
  *       获取订单数据信息
      CLEAR:ls_afko.
      SELECT SINGLE *
        INTO ls_afko
        FROM afko
       WHERE aufnr = ls_qals-aufnr.
  
      ps_data_0003-pastrterm =   ls_afko-gstrp."计划生产日期
  
  *
  *    PS_DATA_0003-LMENGEIST   =   LS_QALS-LMENGEIST."物料批量
      IF ls_mara-mtart = 'Z010'
      OR ls_mara-mtart = 'Z020'
      OR ls_mara-mtart = 'Z090' .
        ps_data_0003-lmengeist   =   ls_qals-lmengeist."物料批量
      ELSE .
        ps_data_0003-lmengeist   =   ls_qals-losmenge."物料批量
      ENDIF .
  
      IF ls_mara-mtart = 'Z050'.
        ps_data_0003-kdauf       =   ls_qals-kdauf.    "销售凭证
      ELSEIF ls_mara-mtart = 'Z030' .
        SELECT SINGLE zkdauf_s
          INTO ps_data_0003-kdauf
          FROM aufk
          WHERE aufnr = ls_qals-aufnr .
      ENDIF.
  
      ps_data_0003-aufnr       =   ls_qals-aufnr.    "流程订单
  
  *       获取订单数据信息
      IF ps_data_0003-kdauf IS NOT INITIAL.
        CLEAR:ls_vbak.
        SELECT SINGLE *
          INTO ls_vbak
          FROM vbak
         WHERE vbeln = ps_data_0003-kdauf.
  
        IF   ls_vbak IS NOT INITIAL.
  *          客户地区
          CLEAR:ls_t005t.
          READ TABLE lt_t005t INTO ls_t005t WITH KEY land1 = ls_vbak-zckgj BINARY SEARCH."国家名
          ps_data_0003-ch_landx = ls_t005t-landx."客户地区
  
  *          售达方
          CLEAR:ls_but000_sdf.
          SELECT SINGLE *
            INTO ls_but000_sdf
            FROM but000
           WHERE partner = ls_vbak-kunnr.
  
          ps_data_0003-t_kunnr       =  ls_but000_sdf-bu_sort1."售达方搜索词1
  
  
          "SJKH 实际客户从KNVP取数改为VBPA取数 S4DK904914
          CLEAR ls_vbpa .
          SELECT SINGLE *
            INTO ls_vbpa
            FROM vbpa
            WHERE vbeln = ls_vbak-vbeln
              AND parvw = 'Z1' .
          IF sy-subrc = 0 .
            CLEAR:ls_but000_sdf.
            SELECT SINGLE *
              INTO ls_but000_sdf
              FROM but000
             WHERE partner = ls_vbpa-kunnr.
            ps_data_0003-sjkh       =  ls_but000_sdf-bu_sort1."实际客户搜索词1
            ps_data_0003-sjkhcode   =  ls_vbpa-kunnr."实际客户编码 add S4DK904914
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = ps_data_0003-sjkhcode
              IMPORTING
                output = ps_data_0003-sjkhcode.
          ENDIF .
        ENDIF.
      ENDIF.
  
  *       获取批次信息
      PERFORM frm_get_batchinfo TABLES lt_batch_chara USING ls_mcha.
  
      SORT  lt_batch_chara BY atnam.
  
  *      长批号
      CLEAR:ls_batch_chara.
      READ TABLE lt_batch_chara INTO  ls_batch_chara WITH KEY atnam = 'ZCHARGL' BINARY SEARCH.
      IF ls_batch_chara IS NOT INITIAL.
        ps_data_0003-zchargl = ls_batch_chara-atwrt.
      ENDIF.
  
  *     打印批次号
      CLEAR:ls_batch_chara.
      READ TABLE lt_batch_chara INTO  ls_batch_chara WITH KEY atnam = 'Z_BATCH_DYPC' BINARY SEARCH.
      IF ls_batch_chara IS NOT INITIAL.
        ps_data_0003-z_batch_dypc = ls_batch_chara-atwrt.
      ENDIF.
  
  *      产品生产日期
      CLEAR:ls_batch_chara.
      READ TABLE lt_batch_chara INTO  ls_batch_chara WITH KEY atnam = 'Z_BATCH_SCRQ' BINARY SEARCH.
      IF ls_batch_chara IS NOT INITIAL.
        ps_data_0003-z_batch_scrq = ls_batch_chara-atwrt.
        "  ELSE .
        "    PS_DATA_0003-Z_BATCH_SCRQ = LS_MCHA-HSDAT .
      ENDIF.
  
  *      产品有效期至
      CLEAR:ls_batch_chara.
      READ TABLE lt_batch_chara INTO  ls_batch_chara WITH KEY atnam = 'Z_BATCH_YXQZ' BINARY SEARCH.
      IF ls_batch_chara IS NOT INITIAL.
        ps_data_0003-z_batch_yxqz = ls_batch_chara-atwrt.
        "  ELSE .
        "   PS_DATA_0003-Z_BATCH_YXQZ = LS_MCHA-VFDAT .
      ENDIF.
  
  *      小M打印批次号
      CLEAR:ls_batch_chara.
  *    READ TABLE LT_BATCH_CHARA INTO  LS_BATCH_CHARA WITH KEY ATNAM = 'Z_BATCH_XMDYPC' BINARY SEARCH.
  *    IF LS_BATCH_CHARA IS NOT INITIAL.
  *      PS_DATA_0003-Z_BATCH_XMDYPC = LS_BATCH_CHARA-ATWRT.
  *    ENDIF.
      LOOP AT lt_batch_chara INTO  ls_batch_chara WHERE atnam = 'Z_BATCH_XMDYPC' .
  
        IF ps_data_0003-z_batch_xmdypc = '' .
          ps_data_0003-z_batch_xmdypc = ls_batch_chara-atwrt.
        ELSE .
          CONCATENATE ls_batch_chara-atwrt '|'  ps_data_0003-z_batch_xmdypc INTO ps_data_0003-z_batch_xmdypc .
        ENDIF .
  
      ENDLOOP .
  
  
  *      批次包装规格
      IF ls_mara-mtart = 'Z010'
       OR ls_mara-mtart = 'Z020'
       OR ls_mara-mtart = 'Z090' .
  
        CLEAR:ls_batch_chara.
        READ TABLE lt_batch_chara INTO  ls_batch_chara WITH KEY atnam = 'ZPACK_STA' BINARY SEARCH.
        IF ls_batch_chara IS NOT INITIAL.
          ps_data_0003-zpspe = ls_batch_chara-atwrt.
        ENDIF.
      ELSEIF ls_mara-mtart = 'Z030'
       OR ls_mara-mtart = 'Z050' .
  
        SELECT SINGLE atwrt
          INTO ps_data_0003-zpspe
          FROM ausp
          INNER JOIN cabn
          ON cabn~atinn = ausp~atinn
          WHERE ausp~objek = ls_qals-matnr
            AND ausp~klart = '001'
            AND cabn~atnam = 'ZPSPE' .
  
      ENDIF .
  
      "批量件数
      DATA : lv_menge1 TYPE mseg-menge .
      DATA : lv_menge2 TYPE i .
      CLEAR lv_menge1 .
      CALL FUNCTION 'Z_QM_LIMS_GUIG'
        EXPORTING
          i_input = ps_data_0003-zpspe
        IMPORTING
          output  = lv_menge1.
      IF lv_menge1 <> 0 .
        lv_menge2 = ceil( ls_qals-losmenge / lv_menge1 ) .
        ps_data_0003-pkgs = lv_menge2 .
        CONDENSE ps_data_0003-pkgs NO-GAPS .
      ENDIF .
      IF  ls_mara-mtart = 'Z030'
       OR ls_mara-mtart = 'Z050' .
        CLEAR ps_data_0003-zpspe  ."成品物料不需传输包装规格
      ENDIF .
  
  **      供应商批次编号
  *    CLEAR:LS_BATCH_CHARA.
  *    READ TABLE LT_BATCH_CHARA INTO  LS_BATCH_CHARA WITH KEY ATNAM = 'ZLICHA' BINARY SEARCH.
  *    IF LS_BATCH_CHARA IS NOT INITIAL.
  *      PS_DATA_0003-LICHA = LS_BATCH_CHARA-ATWRT.
  *    ENDIF.
  
  *      订单信息相关
      IF ls_qals-aufnr IS NOT INITIAL.
        SELECT SINGLE *
          INTO CORRESPONDING FIELDS OF ls_aufk
          FROM aufk
         WHERE aufnr  = ls_qals-aufnr.
  
        ps_data_0003-zchej  = ls_aufk-zchej."车间
        ps_data_0003-zchanx = ls_aufk-zchanx."产线
  
        IF ls_mara-mtart = 'Z030'.
          ps_data_0003-kdauf =   ls_aufk-zkdauf_s.
        ENDIF.
  
  
        SELECT *
          INTO CORRESPONDING FIELDS OF TABLE lt_resb
          FROM resb
  *         WHERE RSNUM = LS_AUFK-PROCNR
          WHERE aufnr = ls_qals-aufnr
           AND charg <> ''.
  
        LOOP AT  lt_resb INTO  ls_resb.
          ps_data_0003-s_charg  = ls_resb-charg ."S码SAP批次号
        ENDLOOP.
  
      ENDIF.
    ENDIF.
  *
  
  
  ENDFORM.
  
  
  
  *&---------------------------------------------------------------------*
  *& Form FRM_GOTO_LIMS_QM0561
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> IS_INPUT
  *&      <-- E_OUTPUT
  *&      <-- GS_DATA_0003
  *&---------------------------------------------------------------------*
  FORM frm_goto_lims_qm0561  USING ps_input TYPE zqms_lims_0003
                          CHANGING ps_output TYPE zqms_ret
                            ps_data_0003 LIKE gs_data_0003.
  
    IF ps_output-code = 'E'.
      EXIT.
    ENDIF.
  
  ********
  *----------------------------------------------------------------------
  *   业务逻辑处理
  *----------------------------------------------------------------------
    DATA:lv_error     TYPE c,
         lv_str       TYPE string,
         lr_proxy     TYPE REF TO zco_si_sap2lims_check_create_o, "代理类
         lr_protocol  TYPE REF TO if_wsprotocol_message_id,
         ls_input     TYPE zcreate_test_request_sessionl1, "推送信息
         ls_output    TYPE zcreate_test_request_sessionle, "返回信息
         ls_input_row TYPE zbatch_dto.
  
    CLEAR:ls_input_row.
  
  
  * 调用PI下发至LIMS
    TRY.
        CREATE OBJECT lr_proxy.
  
        MOVE-CORRESPONDING ps_data_0003 TO ls_input_row.
  
        IF ps_data_0003-lwedt = 0 .
          CLEAR ls_input_row-lwedt .
        ENDIF .
  
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = ps_data_0003-matnr
          IMPORTING
            output = ls_input_row-matnr.
  *      htc_change.
        "20220429 Seashell HTC上下游替换
        PERFORM htc_change USING ls_input_row.
  
  
        APPEND   ls_input_row TO ls_input-arr_parameters-batch_dto."业务数据
  
        ls_input-user  = gc_limsuser."访问LIMS系统用户名
        ls_input-pwd   = gc_limspwd. "访问LIMS系统密码
  
  
  *    接口数据下发
        CALL METHOD lr_proxy->si_sap2lims_check_create_out
          EXPORTING
            output = ls_input
          IMPORTING
            input  = ls_output.
  
        ps_output-code     = ls_output-create_test_request_sessionles-ret_code.
        ps_output-msg      = ls_output-create_test_request_sessionles-ret_msg.
  
      CATCH cx_root INTO DATA(lr_root).
        ps_output-code = 'A'.
        ps_output-msg = lr_root->get_text(  ).
  
    ENDTRY.
  
  
  * 记录返回结果
    PERFORM frm_record_ret USING ps_input  CHANGING ps_output .
  
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GOTO_LIMS_QM0561
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> IS_INPUT
  *&      <-- E_OUTPUT
  *&      <-- GS_DATA_0003
  *&---------------------------------------------------------------------*
  FORM frm_goto_lims_qm0561_c  USING ps_input TYPE zqms_lims_0014
                          CHANGING ps_output TYPE zqms_ret
                            ps_data_0003 LIKE gs_data_0003.
  
    IF ps_output-code = 'E'.
      EXIT.
    ENDIF.
  
  ********
  *----------------------------------------------------------------------
  *   业务逻辑处理
  *----------------------------------------------------------------------
    DATA:lv_error     TYPE c,
         lv_str       TYPE string,
         lr_proxy     TYPE REF TO zco_si_sap2lims_check_create_o, "代理类
         lr_protocol  TYPE REF TO if_wsprotocol_message_id,
         ls_input     TYPE zcreate_test_request_sessionl1, "推送信息
         ls_output    TYPE zcreate_test_request_sessionle, "返回信息
         ls_input_row TYPE zbatch_dto.
  
    CLEAR:ls_input_row.
  
  
  * 调用PI下发至LIMS
    TRY.
        CREATE OBJECT lr_proxy.
  
        MOVE-CORRESPONDING ps_data_0003 TO ls_input_row.
  
  
  
        IF ps_data_0003-lwedt = 0 .
          CLEAR ls_input_row-lwedt .
        ENDIF .
  
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = ps_data_0003-matnr
          IMPORTING
            output = ls_input_row-matnr.
  
  *      htc_change.
  *      htc_change.
        "20220429 Seashell HTC上下游替换
        PERFORM htc_change USING ls_input_row.
  
  
  
        APPEND   ls_input_row TO ls_input-arr_parameters-batch_dto."业务数据
  
        ls_input-user  = gc_limsuser."访问LIMS系统用户名
        ls_input-pwd   = gc_limspwd. "访问LIMS系统密码
  
  *    接口数据下发
        CALL METHOD lr_proxy->si_sap2lims_check_create_out
          EXPORTING
            output = ls_input
          IMPORTING
            input  = ls_output.
  
        ps_output-code     = ls_output-create_test_request_sessionles-ret_code.
        ps_output-msg      = ls_output-create_test_request_sessionles-ret_msg.
  
      CATCH cx_root INTO DATA(lr_root).
        ps_output-code = 'A'.
        ps_output-msg = lr_root->get_text(  ).
  
    ENDTRY.
  
  
  * 记录返回结果
    PERFORM frm_record_ret_c USING ps_input  CHANGING ps_output .
  
  
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_RECORD_RET
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> PS_INPUT
  *&      <-- PS_OUTPUT
  *&---------------------------------------------------------------------*
  FORM frm_record_ret USING ps_input TYPE zqms_lims_0003
                          CHANGING ps_output TYPE zqms_ret.
  
    DATA:ls_zqmt_intf_result TYPE zqmt_intf_result.
    DATA:lt_zqmt_intf_result TYPE STANDARD TABLE OF  zqmt_intf_result.
  
  *   记录返回结果
    CLEAR:ls_zqmt_intf_result.
    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF ls_zqmt_intf_result
      FROM zqmt_intf_result
     WHERE zkey    = ps_input-prueflos
       AND intid   = ps_input-intid.
  
  * Z_QM_LIMS_0003-输入参数
    gv_json =  /ui2/cl_json=>serialize( EXPORTING data = ps_input pretty_name = /ui2/cl_json=>pretty_mode-none   )."转换成JSON字符串
  
    ls_zqmt_intf_result-intid   = ps_input-intid..
    ls_zqmt_intf_result-zkey    = ps_input-prueflos.
    ls_zqmt_intf_result-type    = ps_output-code.
    ls_zqmt_intf_result-msg     = ps_output-msg.
    ls_zqmt_intf_result-zdata   = gv_json."记录输入参数
    APPEND ls_zqmt_intf_result  TO lt_zqmt_intf_result.
  
    MODIFY zqmt_intf_result FROM TABLE lt_zqmt_intf_result.
    COMMIT WORK AND WAIT.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_RECORD_RET
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> PS_INPUT
  *&      <-- PS_OUTPUT
  *&---------------------------------------------------------------------*
  FORM frm_record_ret_c USING ps_input TYPE zqms_lims_0014
                          CHANGING ps_output TYPE zqms_ret.
  
    DATA:ls_zqmt_intf_result TYPE zqmt_intf_result.
    DATA:lt_zqmt_intf_result TYPE STANDARD TABLE OF  zqmt_intf_result.
  
  *   记录返回结果
    CLEAR:ls_zqmt_intf_result.
    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF ls_zqmt_intf_result
      FROM zqmt_intf_result
     WHERE zkey    = ps_input-prueflos
       AND intid   = ps_input-intid.
  
  * Z_QM_LIMS_0003-输入参数
    gv_json =  /ui2/cl_json=>serialize( EXPORTING data = ps_input pretty_name = /ui2/cl_json=>pretty_mode-none   )."转换成JSON字符串
  
    ls_zqmt_intf_result-intid   = ps_input-intid..
    ls_zqmt_intf_result-zkey    = ps_input-prueflos.
    ls_zqmt_intf_result-type    = ps_output-code.
    ls_zqmt_intf_result-msg     = ps_output-msg.
    ls_zqmt_intf_result-zdata   = gv_json."记录输入参数
    APPEND ls_zqmt_intf_result  TO lt_zqmt_intf_result.
  
    MODIFY zqmt_intf_result FROM TABLE lt_zqmt_intf_result.
  *  COMMIT WORK AND WAIT.
  ENDFORM.
  
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_QM0562
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> IS_INPUT
  *&      <-- E_OUTPUT
  *&      <-- GS_DATA_0003
  *&---------------------------------------------------------------------*
  FORM frm_get_qm0562 USING ps_input TYPE zqms_lims_0003
                        CHANGING ps_output TYPE zqms_ret
                          ps_data_0003 LIKE gs_data_0003.
  
  
    DATA:ls_qals       LIKE qals.  "检验批记录
    DATA:ls_mcha       LIKE mcha.  "批次
    DATA:ls_but000_gy  LIKE but000."供应商
    DATA:ls_but000_zz  LIKE but000."制造商
    DATA:ls_but000_sdf LIKE but000."售达方
    DATA:ls_kna1       LIKE kna1.  "客户主数据
    DATA : ls_lfa1 TYPE lfa1 .
    DATA:ls_afko       LIKE afko.  "订单号
    DATA:ls_vbak       LIKE vbak.  "销售凭证 ： 抬头数据
    DATA:ls_t005t      LIKE t005t. "国家名
    DATA:lt_t005t      LIKE STANDARD TABLE OF t005t. "国家名
    DATA:ls_t006a      LIKE t006a. "单位
    DATA:lt_t006a      LIKE STANDARD TABLE OF t006a.
    DATA:ls_aufk       LIKE aufk.
    DATA:ls_resb       LIKE resb.
    DATA:lt_resb       LIKE STANDARD TABLE OF resb.
    DATA:ls_mara      LIKE mara.
  
  * 批次信息
    DATA:lt_batch_chara LIKE STANDARD TABLE OF gs_batch_chara.
    DATA:ls_batch_chara LIKE  gs_batch_chara.
  
    CLEAR:ps_data_0003.
  
  * 获取单位信息
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_t006a
      FROM t006a
     WHERE spras = sy-langu.
  
  * 获取检验批记录信息
    SELECT SINGLE *
      INTO ls_qals
      FROM qals
     WHERE prueflos = ps_input-prueflos.
  
    IF ls_qals IS  INITIAL.
      ps_output-code  = 'E'.
      ps_output-msg = '没有查询到对应的检验批号'.
      EXIT.
    ENDIF.
  
  
    IF ls_qals-prueflos IS NOT INITIAL.
      ps_data_0003-prueflos    =   ls_qals-prueflos.  "请验单号
      ps_data_0003-quickflag  =   ls_qals-zquick."快速标识 S4DK905625
  
  *   物料工厂批次信息
      SELECT SINGLE *
        INTO CORRESPONDING FIELDS OF ls_mcha
        FROM mcha
       WHERE matnr = ls_qals-matnr
         AND werks = ls_qals-werk
         AND charg = ls_qals-charg.
  
  *
      ps_data_0003-licha   = ls_mcha-licha. "供应商批次号
      ps_data_0003-w_hsdat  = ls_mcha-hsdat. "物料生产日期
      ps_data_0003-w_vfdat  = ls_mcha-vfdat. "物料有效日期-物料货架寿命到期日
  
  *   获取批次信息
      PERFORM frm_get_batchinfo TABLES lt_batch_chara USING ls_mcha.
  
      SORT  lt_batch_chara BY atnam.
  
  *   长批号
      CLEAR:ls_batch_chara.
      READ TABLE lt_batch_chara INTO  ls_batch_chara WITH KEY atnam = 'ZCHARGL' BINARY SEARCH.
      IF ls_batch_chara IS NOT INITIAL.
        ps_data_0003-zchargl = ls_batch_chara-atwrt.
      ENDIF.
  
  
  ***   供应商批次编号
  **    CLEAR:LS_BATCH_CHARA.
  **    READ TABLE LT_BATCH_CHARA INTO  LS_BATCH_CHARA WITH KEY ATNAM = 'ZLICHA' BINARY SEARCH.
  **    IF LS_BATCH_CHARA IS NOT INITIAL.
  **      PS_DATA_0003-LICHA = LS_BATCH_CHARA-ATWRT.
  **    ENDIF.
  *
  **    PS_DATA_0003-LMENGEIST   =   LS_QALS-LMENGEIST."物料批量
  *    PS_DATA_0003-LMENGEIST   =   LS_QALS-LOSMENGE."物料批量
  *
  **       单位
  *    IF LT_T006A[] IS NOT INITIAL.
  *      CLEAR:LS_T006A.
  *      READ TABLE LT_T006A INTO LS_T006A WITH KEY MSEHI = LS_QALS-MENGENEINH BINARY SEARCH."
  *      PS_DATA_0003-T_MENGENEINH = LS_T006A-MSEH3.
  *    ENDIF.
  *
  **       获取客户主数据信息
  *    CLEAR:LS_LFA1.
  *    SELECT SINGLE *
  *      INTO LS_LFA1
  *      FROM LFA1
  *     WHERE LIFNR = LS_QALS-LIFNR.
  *    IF SY-SUBRC = 0 .
  *      SELECT SINGLE REGIOGROUP
  *        INTO PS_DATA_0003-ZMFR_GRD
  *        FROM ADRC
  *        WHERE ADDRNUMBER = LS_LFA1-ADRNR .
  **    PS_DATA_0003-ZMFR_GRD  =   LS_KNA1-KATR1."供应商等级
  *    ENDIF .
  *
  **      订单信息相关
  *    IF LS_QALS-AUFNR IS NOT INITIAL.
  **      SELECT SINGLE *
  **        INTO CORRESPONDING FIELDS OF LS_AUFK
  **        FROM AUFK
  **       WHERE AUFNR  = LS_QALS-AUFNR.
  **
  **      PS_DATA_0003-ZCHEJ  = LS_AUFK-ZCHEJ."车间
  **      PS_DATA_0003-ZCHANX = LS_AUFK-ZCHANX."产线
  **
  **      IF LS_MARA-MTART = 'Z030'.
  **        PS_DATA_0003-KDAUF =   LS_AUFK-ZKDAUF_S.
  **      ENDIF.
  *      SELECT *
  *        INTO CORRESPONDING FIELDS OF TABLE LT_RESB
  *        FROM RESB
  **         WHERE RSNUM = LS_AUFK-PROCNR
  *        WHERE AUFNR = LS_QALS-AUFNR
  *         AND CHARG <> ''.
  *
  *      LOOP AT  LT_RESB INTO  LS_RESB.
  *        PS_DATA_0003-S_CHARG  = LS_RESB-CHARG ."S码SAP批次号
  *      ENDLOOP.
  *
  *
  **      批次包装规格
  *      IF LS_MARA-MTART = 'Z010'
  *       OR LS_MARA-MTART = 'Z020'
  *       OR LS_MARA-MTART = 'Z090' .
  *
  *        CLEAR:LS_BATCH_CHARA.
  *        READ TABLE LT_BATCH_CHARA INTO  LS_BATCH_CHARA WITH KEY ATNAM = 'ZPACK_STA' BINARY SEARCH.
  *        IF LS_BATCH_CHARA IS NOT INITIAL.
  *          PS_DATA_0003-PKGS = LS_BATCH_CHARA-ATWRT.
  *        ENDIF.
  *      ELSEIF LS_MARA-MTART = 'Z030'
  *       OR LS_MARA-MTART = 'Z050' .
  *
  *        SELECT SINGLE ATWRT
  *          INTO PS_DATA_0003-PKGS
  *          FROM AUSP
  *          WHERE OBJEK = LS_QALS-MATNR
  *            AND MAFID = '001'
  *            AND ATINN = 'ZPSPE' .
  *
  *      ENDIF .
  *
  *      "批量件数
  *      DATA : LV_MENGE1 TYPE MSEG-MENGE .
  *      DATA : LV_MENGE2 TYPE I .
  *      CLEAR LV_MENGE1 .
  *      CALL FUNCTION 'Z_QM_LIMS_GUIG'
  *        EXPORTING
  *          I_INPUT = PS_DATA_0003-PKGS
  *        IMPORTING
  *          OUTPUT  = LV_MENGE1.
  *      IF LV_MENGE1 <> 0 .
  *        LV_MENGE2 = CEIL( LS_QALS-LOSMENGE / LV_MENGE1 ) .
  *        PS_DATA_0003-ZPSPE = LV_MENGE2 .
  *        CONDENSE PS_DATA_0003-ZPSPE NO-GAPS .
  *      ENDIF .
  *
  *    ENDIF.
  
    ENDIF.
  
  
  
  
  ENDFORM.
  
  
  
  *&---------------------------------------------------------------------*
  *& Form FRM_GOTO_LIMS_QM0562
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> IS_INPUT
  *&      <-- E_OUTPUT
  *&      <-- GS_DATA_0003
  *&---------------------------------------------------------------------*
  FORM frm_goto_lims_qm0562 USING ps_input TYPE zqms_lims_0003
                            CHANGING ps_output TYPE zqms_ret
                              ps_data_0003 LIKE gs_data_0003.
  
  
    IF ps_output-code = 'E'.
      EXIT.
    ENDIF.
  
  ********
  *----------------------------------------------------------------------
  *   业务逻辑处理
  *----------------------------------------------------------------------
    DATA:lv_error     TYPE c,
         lv_str       TYPE string,
         lr_proxy     TYPE REF TO zco_si_sap2lims_check_update_o, "代理类
         lr_protocol  TYPE REF TO if_wsprotocol_message_id,
         ls_input     TYPE zupdate_test_request_sessionl1, "推送信息
         ls_output    TYPE zupdate_test_request_sessionle, "返回信息
         ls_input_row TYPE zbatch_dto.
  
    CLEAR:ls_input_row.
  
  
  * 调用PI下发至LIMS
    TRY.
        CREATE OBJECT lr_proxy.
  
        MOVE-CORRESPONDING ps_data_0003 TO ls_input_row.
  
  *      htc_change.
  *      htc_change.
        "20220429 Seashell HTC上下游替换
        PERFORM htc_change USING ls_input_row.
  
        APPEND   ls_input_row TO ls_input-arr_parameters-batch_dto."业务数据
  
        ls_input-user  = gc_limsuser."访问LIMS系统用户名
        ls_input-pwd   = gc_limspwd. "访问LIMS系统密码
  
        IF ps_data_0003-lwedt = 0 .
          CLEAR ls_input_row-lwedt .
        ENDIF .
  
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = ps_data_0003-matnr
          IMPORTING
            output = ls_input_row-matnr.
  
  
  *      接口数据下发
        CALL METHOD lr_proxy->si_sap2lims_check_update_out
          EXPORTING
            output = ls_input
          IMPORTING
            input  = ls_output.
  
        ps_output-code     = ls_output-update_test_request_sessionles-ret_code.
        ps_output-msg      = ls_output-update_test_request_sessionles-ret_msg.
  
      CATCH cx_root INTO DATA(lr_root).
        ps_output-code = 'A'.
        ps_output-msg = lr_root->get_text(  ).
  
    ENDTRY.
  
  
  * 记录返回结果
    PERFORM frm_record_ret USING ps_input  CHANGING ps_output .
  
  
  ENDFORM.
  
  
  
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_QM0563
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> IS_INPUT
  *&      <-- E_OUTPUT
  *&      <-- GS_DATA_0003
  *&---------------------------------------------------------------------*
  FORM frm_get_qm0563  USING ps_input TYPE zqms_lims_0003
                        CHANGING ps_output TYPE zqms_ret
                          ps_data_0003 LIKE gs_data_0003.
  
  
    DATA:ls_qals       LIKE qals.  "检验批记录
    DATA:ls_mcha       LIKE mcha.  "批次
    DATA:ls_but000_gy  LIKE but000."供应商
    DATA:ls_but000_zz  LIKE but000."制造商
    DATA:ls_but000_sdf LIKE but000."售达方
    DATA:ls_kna1       LIKE kna1.  "客户主数据
    DATA:ls_afko       LIKE afko.  "订单号
    DATA:ls_vbak       LIKE vbak.  "销售凭证 ： 抬头数据
    DATA:ls_t005t      LIKE t005t. "国家名
    DATA:lt_t005t      LIKE STANDARD TABLE OF t005t. "国家名
    DATA:ls_t006a      LIKE t006a. "单位
    DATA:lt_t006a      LIKE STANDARD TABLE OF t006a.
    DATA:ls_aufk       LIKE aufk.
    DATA:ls_resb       LIKE resb.
    DATA:lt_resb       LIKE STANDARD TABLE OF resb.
    DATA:ls_mara      LIKE mara.
  
  * 批次信息
    DATA:lt_batch_chara LIKE STANDARD TABLE OF gs_batch_chara.
    DATA:ls_batch_chara LIKE  gs_batch_chara.
  
    CLEAR:ps_data_0003.
  
  
  
  * 获取检验批记录信息
    SELECT SINGLE *
      INTO ls_qals
      FROM qals
     WHERE prueflos = ps_input-prueflos.
  
    IF ls_qals IS  INITIAL.
      ps_output-code  = 'E'.
      ps_output-msg = '没有查询到对应的检验批号'.
      EXIT.
    ENDIF.
  
  
    IF ls_qals-prueflos IS NOT INITIAL.
      ps_data_0003-prueflos    =   ls_qals-prueflos.  "请验单号
  
    ENDIF.
  
  
  
  
  ENDFORM.
  
  
  
  *&---------------------------------------------------------------------*
  *& Form FRM_GOTO_LIMS_QM0563
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> IS_INPUT
  *&      <-- E_OUTPUT
  *&      <-- GS_DATA_0003
  *&---------------------------------------------------------------------*
  FORM frm_goto_lims_qm0563  USING ps_input TYPE zqms_lims_0003
                        CHANGING ps_output TYPE zqms_ret
                          ps_data_0003 LIKE gs_data_0003.
  
  
    IF ps_output-code = 'E'.
      EXIT.
    ENDIF.
  
  ********
  *----------------------------------------------------------------------
  *   业务逻辑处理
  *----------------------------------------------------------------------
    DATA:lv_error     TYPE c,
         lv_str       TYPE string,
         lr_proxy     TYPE REF TO zco_si_sap2lims_check_remove_o, "代理类
         lr_protocol  TYPE REF TO if_wsprotocol_message_id,
         ls_input     TYPE zremove_test_request_sessionl1, "推送信息
         ls_output    TYPE zremove_test_request_sessionle, "返回信息
         ls_input_row TYPE zbatch_dto.
  
    CLEAR:ls_input_row.
  
  
  * 调用PI下发至LIMS
    TRY.
        CREATE OBJECT lr_proxy.
  
        MOVE-CORRESPONDING ps_data_0003 TO ls_input_row.
  
        APPEND   ls_input_row TO ls_input-arr_parameters-batch_dto."业务数据
  
        ls_input-user  = gc_limsuser."访问LIMS系统用户名
        ls_input-pwd   = gc_limspwd. "访问LIMS系统密码
  
        IF ps_data_0003-lwedt = 0 .
          CLEAR ls_input_row-lwedt .
        ENDIF .
  
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = ps_data_0003-matnr
          IMPORTING
            output = ls_input_row-matnr.
  
  *    接口数据下发
        CALL METHOD lr_proxy->si_sap2lims_check_remove_out
          EXPORTING
            output = ls_input
          IMPORTING
            input  = ls_output.
  
        ps_output-code     = ls_output-remove_test_request_sessionles-ret_code.
        ps_output-msg      = ls_output-remove_test_request_sessionles-ret_msg.
  
      CATCH cx_root INTO DATA(lr_root).
        ps_output-code = 'A'.
        ps_output-msg = lr_root->get_text(  ).
  
    ENDTRY.
  
  
  * 记录返回结果
    PERFORM frm_record_ret USING ps_input  CHANGING ps_output .
  
  ENDFORM.
  
  
  
  *&---------------------------------------------------------------------*
  *& Form FRM_SHOW_TESTRESULTS
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> IS_INPUT
  *&      <-- E_OUTPUT
  *&---------------------------------------------------------------------*
  FORM frm_show_testresults  USING    ps_qals TYPE qals
                                       ps_mara TYPE mara
                             CHANGING ps_output TYPE bapiret2.
  
  *
  *  1.MARA-MTART=Z010，且QALS-ART=Y04      则将检验批号查询ZQMT059中“批属追溯”字段，取出对应条目；
  *
  *  2.MARA-MTART=Z050，且QALS-ART=Z04或04，则将当前检验批号赋给ZQMT059中“检验批号”字段，取出对应的“批属追溯”字段，再将该字段的值赋给ZQMT059的“检验批号”字段，取出对应条目；
  *
  *  3.MARA-MTART=Z050，且QALS-ART=C04，    则由检验批号QALS- PRUEFLOS取出QALS-AUFNR。
  *  根据（QALS-AUFNR）在（MSEG-BWART ）261于表mseg且物料类型（MARA-MTART=Z030）的物料凭证条目，取出S码批次号MSEG-CHARG给到QALS-CHARG，
  *  从而取出S码批次号。用ZQMT059-charg = S码批次号条件，查询自建表ZQMT059"
  
  *  4.MARA-MTART=Z050，且QALS-ART=G04，   则由检验批号QALS- PRUEFLOS取出QALS-AUFNR。
  *  根据（QALS-AUFNR）在（MSEG-BWART ）261于表mseg且物料类型（MARA-MTART=Z050）的物料凭证条目，取出M码批次号MSEG-CHARG给到QALS-CHARG，
  *  从而取出M码批次号。再去表QALS-charg = 小M批次号 ，得到检验批次号表
  *  检验类型QALS-ART = C04 ，执行第三种操作
  *  检验类型QALS-ART = 04 或Z０４，执行第二种操作"
  
  
    CLEAR:gt_zqmt059.
  
    DATA:lt_zqmt059 TYPE STANDARD TABLE OF zqmt059.
    DATA:lt_zqmt0592 TYPE STANDARD TABLE OF zqmt059.
  
    DATA:BEGIN OF ls_mseg,
           mblnr TYPE mseg-mblnr,
           mjahr TYPE mseg-mjahr,
           zeile TYPE mseg-zeile,
           charg TYPE mseg-charg,
           matnr TYPE mseg-matnr,
           bwart TYPE mseg-bwart,
           mtart TYPE mara-mtart,
         END OF ls_mseg.
    DATA:lt_mseg LIKE STANDARD TABLE OF ls_mseg.
  
    DATA:ls_qals TYPE qals.
    DATA:lt_qals TYPE STANDARD TABLE OF qals.
    DATA:lt_qals1 TYPE STANDARD TABLE OF qals.
    DATA:lt_qals2 TYPE STANDARD TABLE OF qals.
  
    CLEAR:lt_zqmt059.
  
  * 第一种情况
    IF ps_mara-mtart = 'Z010' AND   ps_qals-art = '01'.
  **   先检验批号查询zqmt059-folderno
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE gt_zqmt059
        FROM zqmt059
       WHERE folderno = ps_qals-prueflos.
  
  *   查不到再用检验批号查询zqmt059-FOLDERNO_REL
  *      IF   LT_ZQMT059[] IS INITIAL.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_zqmt059
        FROM zqmt059
       WHERE folderno_rel = ps_qals-prueflos.
  *      ENDIF.
  
  
      IF lt_zqmt059[] IS NOT INITIAL.
        APPEND LINES OF lt_zqmt059 TO gt_zqmt059.
      ENDIF.
  
  * 第二种情况
    ELSEIF ps_mara-mtart = 'Z050' AND ( ps_qals-art = 'Z04' OR ps_qals-art = '04' ).
  *    IF PS_QALS-ART <> ''.
  *   先检验批号查询zqmt059-folderno
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE gt_zqmt059
        FROM zqmt059
       WHERE folderno = ps_qals-prueflos.
  
  *   查到lt_zqmt059有folderno_rel，再用folderno_rel反查一次zqmt059
      IF   gt_zqmt059[] IS NOT INITIAL.
        SELECT *
          APPENDING CORRESPONDING FIELDS OF TABLE lt_zqmt0592
          FROM zqmt059
           FOR ALL ENTRIES IN gt_zqmt059
         WHERE folderno = gt_zqmt059-folderno_rel.
  
        APPEND LINES OF lt_zqmt0592 TO lt_zqmt059.
      ENDIF.
  
      IF lt_zqmt059[] IS NOT INITIAL.
        APPEND LINES OF lt_zqmt059 TO gt_zqmt059.
      ENDIF.
  *    ENDIF.
  
  * 第三种情况
    ELSEIF ps_mara-mtart = 'Z050' AND ps_qals-art = 'C04' AND ps_qals-aufnr IS NOT INITIAL.
  *    IF PS_QALS-ART <> ''.
  *   获取S码批次号
      SELECT  mseg~mblnr
              mseg~mjahr
              mseg~zeile
              mseg~charg
              mseg~matnr
              mseg~bwart
              mara~mtart
        INTO TABLE lt_mseg
        FROM mseg
       INNER JOIN mara ON mara~matnr = mseg~matnr
       WHERE mseg~bwart = '261'
         AND mtart = 'Z030'
         AND charg <> ''
         AND mseg~aufnr = ps_qals-aufnr.
  
  
  *   通过获取的获取S码批次号，查询zqmt059
      IF lt_mseg[] IS NOT INITIAL.
        SELECT *
          INTO CORRESPONDING FIELDS OF TABLE lt_zqmt059
          FROM zqmt059
           FOR ALL ENTRIES IN lt_mseg
         WHERE charg  = lt_mseg-charg.
      ENDIF.
  
      IF lt_zqmt059[] IS NOT INITIAL.
        APPEND LINES OF lt_zqmt059 TO gt_zqmt059.
      ENDIF.
  *    ENDIF.
  
  * 第四种情况
    ELSEIF ps_mara-mtart = 'Z050' AND ps_qals-art = 'G04' AND ps_qals-aufnr IS NOT INITIAL..
  *    IF PS_QALS-ART <> ''.
  *   获取小M批次号
      SELECT  mseg~mblnr
              mseg~mjahr
              mseg~zeile
              mseg~charg
              mseg~matnr
              mseg~bwart
              mara~mtart
        INTO TABLE lt_mseg
        FROM mseg
       INNER JOIN mara ON mara~matnr = mseg~matnr
       WHERE mseg~bwart = '261'
         AND mara~mtart = 'Z050'
         AND mseg~charg <> ''
         AND mseg~aufnr =  ps_qals-aufnr.
  
  
  *   通过获取的获取小M批次号，查询qals
      IF lt_mseg[] IS NOT INITIAL.
        SELECT *
          INTO CORRESPONDING FIELDS OF TABLE lt_qals
          FROM qals
           FOR ALL ENTRIES IN lt_mseg
         WHERE charg  = lt_mseg-charg
           AND art IN ( 'C04' ,'04' ,'Z04'  ).
      ENDIF.
  
  
  *   检验类型QALS-ART = C04 ，执行第三种类似操作
      lt_qals1 = lt_qals.
      DELETE lt_qals1 WHERE art <> 'C04'.
      DELETE lt_qals1 WHERE aufnr = ''.
  
      IF lt_qals1[] IS NOT INITIAL.
        CLEAR:lt_zqmt059.
  
  *   获取S码批次号
        SELECT  mseg~mblnr
                mseg~mjahr
                mseg~zeile
                mseg~charg
                mseg~matnr
                mseg~bwart
                mara~mtart
          INTO TABLE lt_mseg
          FROM mseg
         INNER JOIN mara ON mara~matnr = mseg~matnr
           FOR ALL ENTRIES IN lt_qals1
         WHERE mseg~bwart = '261'
           AND mtart = 'Z030'
           AND charg <> ''
           AND aufnr = lt_qals1-aufnr.
  
  
  *   通过获取的获取S码批次号，查询zqmt059
        IF lt_mseg[] IS NOT INITIAL.
          SELECT *
            INTO CORRESPONDING FIELDS OF TABLE lt_zqmt059
            FROM zqmt059
             FOR ALL ENTRIES IN lt_mseg
           WHERE charg  = lt_mseg-charg.
        ENDIF.
  
        IF lt_zqmt059[] IS NOT INITIAL.
          APPEND LINES OF lt_zqmt059 TO gt_zqmt059.
        ENDIF.
      ENDIF.
  
  
  
  *   检验类型QALS-ART = 04 或 Z04 ，执行第二种类似操作
      lt_qals2 = lt_qals.
      DELETE lt_qals2 WHERE art <> '04' AND  art <> 'Z04' .
      IF lt_qals2[] IS NOT INITIAL.
        CLEAR:lt_zqmt059.
        CLEAR:lt_zqmt0592.
  
  *     先检验批号查询zqmt059-folderno
        SELECT *
          INTO CORRESPONDING FIELDS OF TABLE lt_zqmt059
          FROM zqmt059
           FOR ALL ENTRIES IN lt_qals2
         WHERE folderno = lt_qals2-prueflos.
  
  *     查到lt_zqmt059有folderno_rel，再用folderno_rel反查一次zqmt059
        IF   lt_zqmt059[] IS NOT INITIAL.
          SELECT *
            APPENDING CORRESPONDING FIELDS OF TABLE lt_zqmt0592
            FROM zqmt059
             FOR ALL ENTRIES IN lt_zqmt059
           WHERE folderno = lt_zqmt059-folderno_rel.
  
          APPEND LINES OF lt_zqmt0592 TO lt_zqmt059.
        ENDIF.
  
        IF lt_zqmt059[] IS NOT INITIAL.
          APPEND LINES OF lt_zqmt059 TO gt_zqmt059.
        ENDIF.
      ENDIF.
  
  *    ENDIF.
    ELSE .
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE gt_zqmt059
        FROM zqmt059
       WHERE folderno = ps_qals-prueflos.
    ENDIF.
  
  
  
  
    IF gt_zqmt059[] IS INITIAL.
      ps_output-type = 'E'.
      ps_output-message = '无符合条件的条目'.
      EXIT.
    ENDIF.
  
    CALL SCREEN 100 STARTING AT 5 5 ENDING AT 80 20.
  
  
  ENDFORM.
  
  
  *&---------------------------------------------------------------------*
  *& Form FRM_GOTO_LIMS_QM0565
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> LT_MSEG
  *&      --> GV_INTID
  *&      <-- E_OUTPUT
  *&---------------------------------------------------------------------*
  FORM frm_goto_lims_qm0565  TABLES   pt_mseg STRUCTURE mseg
                             USING    pv_intid TYPE ze_intf_id
                             CHANGING  ps_output TYPE zqms_ret.
  
  *  DATA:BEGIN OF LS_MSEG,
  *         MBLNR TYPE MSEG-MBLNR, "物料凭证编号
  *         MJAHR TYPE MSEG-MJAHR, "物料凭证年份
  *         ZEILE TYPE MSEG-ZEILE, "物料凭证项目
  *         WERKS TYPE MSEG-WERKS, "工厂
  *         MATNR TYPE MSEG-MATNR, "物料编码
  *         LIFNR TYPE MSEG-LIFNR, "供应商
  *         BUDAT TYPE MKPF-BUDAT, "入库日期
  *         MENGE TYPE MSEG-MENGE, "入库数量
  *         MEINS TYPE MSEG-MEINS, "基本计量单位
  *       END OF LS_MSEG,
    DATA : ls_mseg TYPE mseg,
           lt_mseg LIKE STANDARD TABLE OF ls_mseg.
  
    DATA:gs_input  TYPE  zqms_lims_generic,
         gv_intid  TYPE  ze_intf_id,
         gs_output TYPE  zqms_ret.
    DATA:gv_strline TYPE string.
  
    IF ps_output-code = 'E'.
      EXIT.
    ENDIF.
  
  * 输入参数-结构转换
    lt_mseg  = CORRESPONDING #( pt_mseg[] ).
  
  ********
  *----------------------------------------------------------------------
  *   业务逻辑处理
  *----------------------------------------------------------------------
    DATA:lv_error     TYPE c,
         lv_str       TYPE string,
         lr_proxy     TYPE REF TO zco_si_sap2lims_purchase_out, "代理类
         lr_proxy2    TYPE REF TO zco_si_sap2lims_purchase_out, "代理类
         lr_protocol  TYPE REF TO if_wsprotocol_message_id,
         ls_input     TYPE zmt_sap2lims_purchase, "推送信息
         ls_input1    TYPE zmt_sap2lims_purchase, "推送信息
         ls_input2    TYPE zmt_sap2lims_purchase, "推送信息
         ls_output    TYPE zmt_sap2lims_purchase_ret, "返回信息
         ls_input_row TYPE zdt_sap2lims_purchase_inventor,
         lt_input_row TYPE zdt_sap2lims_purchase_inve_tab.
    TABLES zmmt0055.
    CLEAR:ls_input_row,lt_input_row.
  
    DATA : ls_but000 TYPE but000 .
    "新增LIMS2个站点的查询条件，按照条件来判断对应站点
    RANGES:g_kostl FOR zmmt0055-kostl,
           g_werks FOR zmmt0055-werks,
           g_knttp FOR zmmt0055-knttp,
           g_saknr FOR zmmt0055-saknr.
  
  
  
  
  
    LOOP AT pt_mseg INTO ls_mseg WHERE bwart = '101'.
  
      MOVE-CORRESPONDING ls_mseg TO ls_input_row.
      ls_input_row-budat = ls_mseg-budat_mkpf .
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          input  = ls_mseg-matnr
        IMPORTING
          output = ls_input_row-matnr.
  
      SELECT SINGLE mseh3
      INTO ls_input_row-meins
      FROM t006a
      WHERE spras = sy-langu
        AND msehi = ls_mseg-meins .
  
      CLEAR ls_but000 .
      SELECT SINGLE *
        INTO ls_but000
        FROM but000
        WHERE partner = ls_mseg-lifnr .
      CONCATENATE ls_but000-name_org1
                  ls_but000-name_org2
      INTO   ls_input_row-lifnr .
  
      "添加站点判断
  *    IF ls_mseg-kostl IS NOT INITIAL.
  *      g_kostl-sign = 'I'.
  *      g_kostl-option = 'EQ'.
  *      g_kostl-low = ls_mseg-kostl.
  *      APPEND g_kostl.
  *    ENDIF.
  *    IF ls_mseg-werks IS NOT INITIAL.
  *      g_werks-sign = 'I'.
  *      g_werks-option = 'EQ'.
  *      g_werks-low = ls_mseg-werks.
  *      APPEND g_werks.
  *    ENDIF.
  *    IF ls_mseg-knttp_gr IS NOT INITIAL.
  *      g_knttp-sign = 'I'.
  *      g_knttp-option = 'EQ'.
  *      g_knttp-low = ls_mseg-knttp_gr.
  *      APPEND g_knttp.
  *    ENDIF.
  *    IF ls_mseg-sakto IS NOT INITIAL .
  *      g_saknr-sign = 'I'.
  *      g_saknr-option = 'EQ'.
  *      g_saknr-low = ls_mseg-sakto.
  *      APPEND g_saknr.
  *    ENDIF.
  
  *    "成本中心为81001H0002不需要判断总账科目
  *    IF ls_mseg-kostl = '81001H0002'.
  *      CLEAR:g_saknr,g_saknr[].
  *    ENDIF.
  
      "根据采购订单、行项目获取科目类型knttp
      SELECT SINGLE
        knttp
      INTO ls_mseg-knttp_gr
      FROM ekpo
      WHERE ebeln = ls_mseg-ebeln
        AND ebelp = ls_mseg-ebelp.
  
  
      SELECT SINGLE
        *
      INTO @DATA(ls_zmmt0055)
      FROM zmmt0055
      WHERE werks = @ls_mseg-werks
        AND kostl = @ls_mseg-kostl
        AND knttp = @ls_mseg-knttp_gr
        AND saknr = @ls_mseg-sakto.
      IF ls_zmmt0055 IS INITIAL.
        SELECT SINGLE
        *
       INTO @ls_zmmt0055
       FROM zmmt0055
       WHERE werks = @ls_mseg-werks
        AND kostl = @ls_mseg-kostl
        AND knttp = @ls_mseg-knttp_gr
        AND type = 'X'.
      ENDIF.
  
  
      IF ls_zmmt0055-lims_site = 'Q'.
        APPEND  ls_input_row TO ls_input1-mt_sap2lims_purchase-arr_parameters-inventory_dto."QC站点业务数据
      ELSEIF ls_zmmt0055-lims_site = 'Y'.
        APPEND  ls_input_row TO ls_input2-mt_sap2lims_purchase-arr_parameters-inventory_dto."研发站点业务数据
      ENDIF.
  
      CLEAR:ls_zmmt0055,ls_input_row,ls_mseg.
      "END
  *        APPEND  ls_input_row TO ls_input1-mt_sap2lims_purchase-arr_parameters-inventory_dto."业务数据
    ENDLOOP.
  
    IF ls_input1-mt_sap2lims_purchase-arr_parameters-inventory_dto IS NOT INITIAL."QC站点
      ls_input1-mt_sap2lims_purchase-lims_site = 'Q'.
      ls_input1-mt_sap2lims_purchase-user  = gc_limsuser."访问LIMS系统用户名
      ls_input1-mt_sap2lims_purchase-pwd   = gc_limspwd. "访问LIMS系统密码
  *    接口数据下发
  * 调用PI下发至LIMS
      TRY.
          CREATE OBJECT lr_proxy.
          CALL METHOD lr_proxy->si_sap2lims_purchase_out
            EXPORTING
              output = ls_input1
            IMPORTING
              input  = ls_output.
  
          ps_output-code     = ls_output-mt_sap2lims_purchase_ret-place_inventory_sessionless_re-ret_code.
          ps_output-msg      = ls_output-mt_sap2lims_purchase_ret-place_inventory_sessionless_re-ret_msg.
  
        CATCH cx_root INTO DATA(lr_root).
          ps_output-code = 'E'.
          ps_output-msg = lr_root->get_text(  ).
  
      ENDTRY.
    ENDIF.
    IF ls_input2-mt_sap2lims_purchase-arr_parameters-inventory_dto IS NOT INITIAL."研发站点
      ls_input2-mt_sap2lims_purchase-lims_site = 'Y'.
      ls_input2-mt_sap2lims_purchase-user  = gc_limsuser."访问LIMS系统用户名
      ls_input2-mt_sap2lims_purchase-pwd   = gc_limspwd. "访问LIMS系统密码
  *    接口数据下发
  * 调用PI下发至LIMS
      TRY.
          CREATE OBJECT lr_proxy2.
          CALL METHOD lr_proxy2->si_sap2lims_purchase_out
            EXPORTING
              output = ls_input2
            IMPORTING
              input  = ls_output.
  
          ps_output-code     = ls_output-mt_sap2lims_purchase_ret-place_inventory_sessionless_re-ret_code.
          ps_output-msg      = ls_output-mt_sap2lims_purchase_ret-place_inventory_sessionless_re-ret_msg.
  
        CATCH cx_root INTO DATA(lr_root2).
          ps_output-code = 'E'.
          ps_output-msg = lr_root->get_text(  ).
  
      ENDTRY.
    ENDIF.
    "日志记录
    DATA l_intid TYPE ze_intf_id VALUE 'QM0565'.
    IF ls_input1 IS NOT INITIAL.
      DATA(l_guid) = zcl_bc_public=>get_guid( )."生成本次调用唯一标识
      DATA(l_flag) = zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = l_intid iv_ptype = 'I' is_data = ls_input1 ).
      zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = l_intid iv_ptype = 'E' is_data = ls_output ). "记录输出参数
    ENDIF.
    IF ls_input2 IS NOT INITIAL.
      DATA(l_guid2) = zcl_bc_public=>get_guid( )."生成本次调用唯一标识
      l_flag = zcl_bc_public=>write_log( iv_logid = l_guid2 iv_intid = l_intid iv_ptype = 'I' is_data = ls_input2 ).
      zcl_bc_public=>write_log( iv_logid = l_guid2 iv_intid = l_intid iv_ptype = 'E' is_data = ls_output ). "记录输出参数
    ENDIF.
  
  
  
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form SUB_EDIT_102
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> I_MKPF
  *&      --> I_MSEG
  *&---------------------------------------------------------------------*
  FORM sub_edit_102  USING    i_mkpf TYPE ty_t_mkpf
                              i_mseg TYPE ty_t_mseg  .
  
    DATA:ls_input TYPE  zqms_lims_0003.
    DATA:ls_mara  TYPE mara.
    DATA:lv_ts TYPE c."S 存表，T推送
    DATA : ls_qals TYPE qals .
    DATA : ls_qamb TYPE qamb .
    DATA : ls_mseg TYPE mseg .
  
    LOOP AT i_mseg INTO ls_mseg WHERE bwart = '102' .
  
      CLEAR lv_ts .
      CLEAR ls_qals .
      CLEAR ls_qamb .
      CLEAR ls_input .
      CLEAR ls_mara .
  
      DO  100 TIMES.
        SELECT SINGLE *
          INTO ls_qamb
          FROM qamb
          WHERE typ = '6'
            AND zaehler = '1'
            AND mblnr = ls_mseg-smbln
            AND mjahr = ls_mseg-sjahr
            AND zeile = ls_mseg-smblp
           .
        IF sy-subrc = 0 .
          EXIT .
        ELSE .
          WAIT UP TO '0.5' SECONDS  .
        ENDIF .
      ENDDO .
  
      CHECK sy-subrc = 0 .
  
      SELECT SINGLE *
        INTO ls_qals
        FROM qals
        WHERE prueflos = ls_qamb-prueflos .
      CHECK sy-subrc = 0 .
  
  * 1.请验信息及批属关系下发LIMS-查询执行结果，调用LIMS新增功能
  * 若QALS-ART=01,Z01,Y04,04,C04,G04,Z08时，触发对LIMS请验新增接口的调用，推送请验新增信息。
  * 若QALS-ART=Z04时，由QALS-MATNR=MARA-MATNR取出MARA-MTART，
  *	若MARA-MTART=Z050则触发对LIMS请验新增接口的调用，推送请验新增信息。
  *	若MARA-MTART=Z030则进一步判读MARA-MATKL的值，当该字段的值等于301001或301002或301201或301202时触发对LIMS请验新增接口的调用，推送请验新增信息；
  *	                           若MARA-MATKL的值不等于301001或301002或301201或301202时则将条目写入自建表ZQMT056。
      IF ls_qals-art = '01' OR ls_qals-art = 'Z01' OR ls_qals-art = 'Y04' OR ls_qals-art = '04' OR
         ls_qals-art = 'C04' OR ls_qals-art = 'G04' OR ls_qals-art = 'Z08' .
        lv_ts = 'T'.
      ENDIF.
  
      IF lv_ts = 'T'.
  *      没有处理成功或没有处理过的，需执行LIMS新增功能
  *
        ls_input-prueflos = ls_qals-prueflos.
        ls_input-intid = 'QM0563'.
  
  *      调用LIMS请验删除
        CALL FUNCTION 'Z_QM_LIMS_0008' "STARTING NEW TASK 'UP01'
          EXPORTING
  *         I_MARKS  = 'X'
  *         i_prueflos = ls_input.
            is_input = ls_input.
  
      ENDIF.
  
    ENDLOOP .
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form SUB_EDIT_101
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *&      --> I_MKPF
  *&      --> I_MSEG
  *&---------------------------------------------------------------------*
  FORM sub_edit_101  USING    i_mkpf TYPE ty_t_mkpf
                              i_mseg TYPE ty_t_mseg   .
  
    DATA : ls_zqmt0562 TYPE zqmt0562 .
    DATA : lt_zqmt0562 TYPE TABLE OF zqmt0562 .
  
    DATA:ls_mara  TYPE mara.
    DATA:lv_ts TYPE c."S 存表，T推送
    DATA : ls_qals TYPE qals .
    DATA : ls_qamb TYPE qamb .
    DATA : lt_qamb TYPE TABLE OF  qamb .
    DATA : ls_mseg TYPE mseg .
    DATA : lv_budat TYPE sy-datum .
    DATA : ls_mcha  LIKE mcha.  "批次
    DATA : lv_line TYPE i .
    DATA:ls_input TYPE  zqms_lims_0003 .
  
  * 批次信息
    DATA:lt_batch_chara LIKE STANDARD TABLE OF gs_batch_chara.
    DATA:ls_batch_chara LIKE  gs_batch_chara.
  
  *   若①MSEG-BWART=101；
    LOOP AT i_mseg INTO ls_mseg WHERE bwart = '101' .
  
  *且②由QAMB-MBLNR=MSEG-SMBLN，QAMB-MJAHR=MSEG-SJAHR，QAMB-ZEILE=MSEG-SMBLP，QAMB-TYP=1检索到QAMB-PRUEFLOS，取出该检验批号下对应的QAMB-TYP=1条目至少有2个；
      CLEAR lv_ts .
      CLEAR ls_qals .
      CLEAR ls_qamb .
      CLEAR ls_mara .
      CLEAR lt_qamb .
  
      DO  100 TIMES.
        SELECT  *
          INTO TABLE lt_qamb
          FROM qamb
          WHERE typ = '1'
  *        AND ZAEHLER = '1'
            AND mblnr = ls_mseg-mblnr
            AND mjahr = ls_mseg-mjahr .
        IF sy-subrc = 0 .
          EXIT .
        ELSE .
          WAIT UP TO '0.5' SECONDS .
        ENDIF .
      ENDDO.
  
  
      READ TABLE lt_qamb INTO ls_qamb INDEX 1 .
  
      CLEAR lt_qamb .
      SELECT  *
        INTO TABLE lt_qamb
        FROM qamb
        WHERE typ = '1'
          AND prueflos = ls_qamb-prueflos .
  
      lv_line = lines( lt_qamb ) .
  
      CHECK lv_line >= 2 .
      READ TABLE lt_qamb INTO ls_qamb INDEX 1 .
  
  
  **且③由QAMB-PRUEFLOS取出QAMB-TYP=1, QAMB-ZAEHLER=1对应的MSEG- BUDAT_MKPF，判断是否与SY-DATUM相匹配，匹配则进入条件④的判断；
  *    CLEAR LV_BUDAT  .
  *    SELECT SINGLE BUDAT_MKPF
  *          INTO LV_BUDAT
  *          FROM MSEG
  *          WHERE MBLNR = LS_QAMB-MBLNR
  *            AND MJAHR = LS_QAMB-MJAHR
  *            AND ZEILE = LS_QAMB-ZEILE   .
  *    CHECK LV_BUDAT = LS_MSEG-BUDAT_MKPF .
  
  
      SELECT SINGLE *
        INTO ls_qals
        FROM qals
        WHERE prueflos = ls_qamb-prueflos .
      CHECK sy-subrc = 0 .
  *且④由QALS-PRUEFLOS=QAMB-PRUEFLOS，QALS-ART=01，Z01，Z08时，则触发对LIMS请验更新接口的调用，推送请验更新信息。
      IF ls_qals-art = '01' OR ls_qals-art = 'Z01' OR  ls_qals-art = 'Z08' .
        lv_ts = 'T'.
      ENDIF.
  
      IF lv_ts = 'T'.
        CLEAR ls_input .
        ls_input-prueflos  = ls_qamb-prueflos.
        ls_input-intid     = 'QM0562'.
  
        CALL FUNCTION 'Z_QM_LIMS_0007'  "修改
          EXPORTING
            is_input = ls_input.
  *      IMPORTING
  *        E_OUTPUT = LS_OUTPUT.
  *      CLEAR LS_ZQMT0562 .
  *      LS_ZQMT0562-QPLOS = LS_QALS-PRUEFLOS .
  *
  **      物料工厂批次信息
  *      SELECT SINGLE *
  *        INTO CORRESPONDING FIELDS OF LS_MCHA
  *        FROM MCHA
  *       WHERE MATNR = LS_QALS-MATNR
  *         AND WERKS = LS_QALS-WERK
  *         AND CHARG = LS_QALS-CHARG.
  *
  **       获取批次信息
  *      PERFORM FRM_GET_BATCHINFO TABLES LT_BATCH_CHARA USING LS_MCHA.
  *
  *      SORT  LT_BATCH_CHARA BY ATNAM.
  *
  **      长批号
  *      CLEAR:LS_BATCH_CHARA.
  *      READ TABLE LT_BATCH_CHARA INTO  LS_BATCH_CHARA WITH KEY ATNAM = 'Z_BATCH_DYPC' BINARY SEARCH.
  *      IF LS_BATCH_CHARA IS NOT INITIAL.
  *        LS_ZQMT0562-ZDYPC = LS_BATCH_CHARA-ATWRT.
  *      ENDIF.
  *
  *      LS_ZQMT0562-ZJYPC = SY-DATUM ."LS_QALS-ERSTELDAT .
  *      LS_ZQMT0562-ZSFTS = '' .
  *      LS_ZQMT0562-ERDAT = SY-DATUM .
  *      LS_ZQMT0562-ERZET = SY-UZEIT .
  *      LS_ZQMT0562-ERNAM = SY-UNAME .
  *      MODIFY ZQMT0562 FROM LS_ZQMT0562 .
  
      ENDIF .
  
    ENDLOOP .
  
  
  ENDFORM.
  
  
  "S-S4DK905288
  FORM frm_check_qm057_input USING is_input TYPE zmt_lims2sap_material_sample
        CHANGING e_output TYPE zmt_lims2sap_material_sample_r
          lv_menge TYPE mseg-menge.
  
    DATA :ls_qals  LIKE qals,
          ls_t006a TYPE t006a,
          lv_matnr TYPE mara-matnr,
          lv_bukrs TYPE vbak-bukrs_vf,
          ls_marv  TYPE marv,
          lv_gsber TYPE vbap-gsber,
          lv_werks TYPE vbap-werks,
          lfgja    TYPE marv-lfgja,
          lfmon    TYPE marv-lfmon.
  
  
  *-----检查物料账期 begin--------*
    "根据工厂获取公司代码
    lv_werks = is_input-mt_lims2sap_material_sample-werk.
    CALL FUNCTION 'SD_GET_KOKRS_BUKRS_FROM_WERKS'
      EXPORTING
        i_werks = lv_werks
        i_gsber = lv_gsber
      IMPORTING
        e_bukrs = lv_bukrs
  *     E_KOKRS =
  *   EXCEPTIONS
  *     PLANT_NOT_VALID                = 1
  *     VALUATION_AREA_NOT_VALID       = 2
  *     NO_KOKRS_FOUND                 = 3
  *     OTHERS  = 4
      .
    IF sy-subrc <> 0.
      e_output-mt_lims2sap_material_sample_re-ret_code = 'E'.
      e_output-mt_lims2sap_material_sample_re-ret_msg = is_input-mt_lims2sap_material_sample-werk && '工厂不存在所属的公司'.
      EXIT.
    ENDIF.
  
    SELECT SINGLE *
      INTO ls_marv
      FROM marv
      WHERE bukrs = lv_bukrs.
  
    CALL FUNCTION 'CACS_DATE_GET_YEAR_MONTH'
      EXPORTING
        i_date  = sy-datum
      IMPORTING
        e_month = lfmon
        e_year  = lfgja.
    IF lfgja NE ls_marv-lfgja OR lfmon NE ls_marv-lfmon.
      IF ls_marv-xruem EQ 'X'.
        IF lfgja NE ls_marv-vjgja OR lfmon NE ls_marv-vjmon.
          e_output-mt_lims2sap_material_sample_re-ret_code = 'E'.
          e_output-mt_lims2sap_material_sample_re-ret_msg = '只能在公司代码 ' && lv_bukrs && ' 的期间 '&& lfmon  && '和' && ls_marv-vjmon && '中记帐'.
          EXIT.
        ENDIF.
      ELSE.
        e_output-mt_lims2sap_material_sample_re-ret_code = 'E'.
        e_output-mt_lims2sap_material_sample_re-ret_msg = '只能在公司代码 ' && lv_bukrs && ' 的期间 '&& ls_marv-lfmon  && '中记帐'.
        EXIT.
      ENDIF.
  
    ENDIF.
  *-----检查物料账期 end--------*
  *-----检查财务账期 begin--------*
  
    CALL FUNCTION 'PERIOD_CHECK'
      EXPORTING
        i_bukrs = lv_bukrs
        i_gjahr = sy-datum+0(4)
        i_koart = '+'
        i_konto = '*'
        i_monat = sy-datum+4(2)
  *  EXCEPTIONS
  *     error_period     = 1
  *     error_period_acc = 2
  *     OTHERS  = 3
      .
    IF sy-subrc NE 0.
      e_output-mt_lims2sap_material_sample_re-ret_code = 'E'.
      e_output-mt_lims2sap_material_sample_re-ret_msg = '公司代码： ' && lv_bukrs && '下账户类型：+  期间 '&& sy-datum+4(2)  && '未开启！'.
      EXIT.
    ENDIF.
  
  
    CALL FUNCTION 'PERIOD_CHECK'
      EXPORTING
        i_bukrs = lv_bukrs
        i_gjahr = sy-datum+0(4)
        i_koart = 'S'
        i_konto = '*'
        i_monat = sy-datum+4(2)
  *  EXCEPTIONS
  *     error_period     = 1
  *     error_period_acc = 2
  *     OTHERS  = 3
      .
    IF sy-subrc NE 0.
      e_output-mt_lims2sap_material_sample_re-ret_code = 'E'.
      e_output-mt_lims2sap_material_sample_re-ret_msg = '公司代码： ' && lv_bukrs && '下账户类型：S  期间 '&& sy-datum+4(2)  && '未开启！'.
      EXIT.
    ENDIF.
  
  *-----检查财务账期 end--------*
  
  
  
    CLEAR ls_t006a.
    "检查单位是否存在
    SELECT SINGLE *
          INTO ls_t006a
          FROM t006a
          WHERE spras = sy-langu
            AND mseh3 = is_input-mt_lims2sap_material_sample-meins_t .
    IF sy-subrc <> 0 .
      e_output-mt_lims2sap_material_sample_re-ret_code = 'E'.
      e_output-mt_lims2sap_material_sample_re-ret_msg = 'SAP内单位查找不到'.
      EXIT .
    ENDIF .
  
  
    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF ls_qals
      FROM qals
      WHERE prueflos = is_input-mt_lims2sap_material_sample-prueflos..
  
    "检查单位转换关系是否存在
    lv_matnr = |{ is_input-mt_lims2sap_material_sample-matnr ALPHA = IN WIDTH = 18 }|.  "补前导零
    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = lv_matnr
        i_in_me              = ls_t006a-msehi
        i_out_me             = ls_qals-mengeneinh
        i_menge              = lv_menge
      IMPORTING
        e_menge              = lv_menge
      EXCEPTIONS
        error_in_application = 1
        error                = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      e_output-mt_lims2sap_material_sample_re-ret_code = 'E'.
      e_output-mt_lims2sap_material_sample_re-ret_msg = 'SAP内没有对应的转换关系'.
      EXIT .
    ENDIF.
  
  
  
  ENDFORM.
  "E-S4DK905288