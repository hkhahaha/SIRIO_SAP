*&---------------------------------------------------------------------*
*& 包含               ZQMR006_F01
*&---------------------------------------------------------------------*


FORM frm_get_data .
  DATA: lv_obtyp TYPE obtyp.
  "---------WWJ 20200413 改动--start------
  "时间
  DATA: sysdate  TYPE dats,
        countday TYPE vtbbewe-atage.
  sysdate = sy-datum.

  "首进物料字段所需要的类型结构
  TYPES: BEGIN OF ty_mseg,"采购订单 ne 0
           matnr TYPE mseg-matnr,
           werks TYPE mseg-werks,
           charg TYPE mseg-charg,
           mblnr TYPE mseg-mblnr,
           budat TYPE mkpf-budat,
         END OF ty_mseg.
  "---------WWJ 20200413 改动--end------
  " 定义所需的结构体

  DATA: lt_qals   TYPE TABLE OF qals,
        ls_qals   TYPE qals,

        lt_makt   TYPE TABLE OF makt,
        ls_makt   TYPE makt,

        lt_ausp   TYPE TABLE OF ausp,
        ls_ausp   TYPE ausp,

        lt_mara   TYPE TABLE OF mara,
        ls_mara   TYPE mara,

        lt_marc   TYPE TABLE OF marc,
        ls_marc   TYPE marc,
        lt_t142t  TYPE TABLE OF t142t,
        ls_t142t  TYPE t142t,
        lt_but000 TYPE TABLE OF but000,
        ls_but000 TYPE but000,
        lt_t001l  TYPE TABLE OF t001l,
        ls_t001l  TYPE t001l,
        lt_mseg   TYPE TABLE OF ty_mseg,
        ls_mseg   TYPE ty_mseg.

  DATA:BEGIN OF ls_qprs2 OCCURS 0,
         plos2 TYPE qprs-plos2,
         phynr TYPE qprs-phynr,
         pn_nr TYPE qprs-pn_nr,
         objnr TYPE qprs-objnr,
       END OF ls_qprs2.
  DATA: BEGIN OF lt_obtyp OCCURS 0,
          plos2 TYPE qplos,
          objnr TYPE objnr,
          stat  TYPE j_status,
        END OF lt_obtyp,
        ls_obtyp LIKE LINE OF lt_obtyp.

  DATA: lt_stat LIKE TABLE OF jstat.


  " 批次属性结构定义

  DATA:lv_e_objek TYPE  cuobn,
       lv_e_obtab TYPE  tabelle,
       lv_e_klart TYPE  klassenart,
       lv_e_class TYPE  klasse_d.

  DATA:lv_objectkey      TYPE  bapi1003_key-object,
       lv_objecttable    TYPE  bapi1003_key-objecttable,
       lv_classnum       TYPE  bapi1003_key-classnum,
       lv_classtype      TYPE  bapi1003_key-classtype,
       lv_objectkey_long TYPE  bapi1003_key-object_long.

  DATA: lt_allocvaluesnum  TYPE TABLE OF bapi1003_alloc_values_num,
        lt_allocvalueschar TYPE TABLE OF bapi1003_alloc_values_char,
        lt_allocvaluescurr TYPE TABLE OF bapi1003_alloc_values_curr,
        lt_return          TYPE TABLE OF bapiret2.

  "物料特征字段结构定义
  DATA: ltoutput   TYPE TABLE OF zmaterial_tx_value.

  " 取出 报表所需系统表的字段

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_t142t FROM t142t.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_t001l FROM t001l.
  SELECT partner name_org1 INTO CORRESPONDING FIELDS OF TABLE lt_but000 FROM but000.
  SELECT
         prueflos
         a~matnr
         charg
         lmengeist
         lagortchrg
         lifnr
         hersteller
         werk
         a~matnr AS objek
        FROM qals AS a INNER JOIN marc AS b ON a~matnr = b~matnr AND a~werk = b~werks
        INNER JOIN mara AS c ON a~matnr = c~matnr AND c~mtart IN ('Z010','Z020')
        INTO CORRESPONDING FIELDS OF TABLE lt_qals
        WHERE a~matnr IN s_matnr
    AND charg IN s_charg
    AND werk = s_werk
    AND lifnr IN s_lifnr
    AND hersteller IN s_herst
    AND enstehdat IN s_endat.

  IF lt_qals IS NOT INITIAL.

    " 取出取样状态
    SELECT plos2, objnr INTO TABLE @DATA(lt_qprs)
           FROM qprs
           FOR ALL ENTRIES IN @lt_qals
           WHERE plos2 = @lt_qals-prueflos.
    "---------WWJ 20200410 改动--start------
    " 取出取样状态
    SELECT plos2,phynr,pn_nr,objnr
      INTO TABLE @DATA(lt_qprs2)
          FROM qprs
          FOR ALL ENTRIES IN @lt_qals
          WHERE plos2 = @lt_qals-prueflos
            AND prtyp = '1'.
    "2020.06.22 Seashell 添加排除I0076取消的条件
    LOOP AT lt_qprs2 INTO DATA(ls_qprs3).
      CALL FUNCTION 'STATUS_READ'
        EXPORTING
*         CLIENT           = SY-MANDT
          objnr            = ls_qprs3-objnr
          only_active      = 'X' "waymon 新增修改 20190904 原本为注释
*        IMPORTING
*         obtyp            = ls_obtyp-obtyp
*         STSMA            =
*         STONR            =
        TABLES
          status           = lt_stat
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc = 0.
        READ TABLE lt_stat TRANSPORTING NO FIELDS WITH KEY stat = 'I0076'. "检验批取消
        IF sy-subrc = 0.
          DELETE lt_qprs2 WHERE plos2 = ls_qprs3-plos2 AND objnr = ls_qprs3-objnr.
          CONTINUE.
        ENDIF.
      ENDIF.
      CLEAR:lt_stat,ls_qprs3.
    ENDLOOP.
    "---------WWJ 20200410 改动--end------
    " 通过函数STATUS_READ，读取OBTYP
    LOOP AT lt_qprs INTO DATA(ls_qprs).
      CLEAR ls_obtyp.
      CALL FUNCTION 'STATUS_READ'
        EXPORTING
*         CLIENT           = SY-MANDT
          objnr            = ls_qprs-objnr
          only_active      = 'X' "waymon 新增修改 20190904 原本为注释
*        IMPORTING
*         obtyp            = ls_obtyp-obtyp
*         STSMA            =
*         STONR            =
        TABLES
          status           = lt_stat
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc = 0.
        ls_obtyp-plos2 = ls_qprs-plos2.

        "---------WWJ 20200410 改动--start------
        READ TABLE lt_stat TRANSPORTING NO FIELDS WITH KEY stat = 'I0224'. "检验批取消
        IF sy-subrc = 0.
          DELETE lt_qals WHERE prueflos = ls_qprs-plos2.
          CONTINUE.
        ENDIF.
        READ TABLE lt_stat TRANSPORTING NO FIELDS WITH KEY stat = 'I0076'."样本取消
        IF sy-subrc = 0.
*          DELETE lt_qals WHERE prueflos = ls_qprs-plos2."2020.05.08样本取消重新匹配显示****黄铠
          CONTINUE.
        ENDIF.
        "---------WWJ 20200410 改动--end------

        READ TABLE lt_stat TRANSPORTING NO FIELDS WITH KEY stat = 'I0002'.
        IF sy-subrc = 0.
          ls_obtyp-stat = 'I0002'.
        ENDIF.

        APPEND ls_obtyp TO lt_obtyp.
      ENDIF.
    ENDLOOP.


    "取出MARA物料表所需的字段
    SELECT
      matnr
      raube
      mfrnr
           INTO CORRESPONDING FIELDS OF TABLE lt_mara
           FROM mara
           FOR ALL ENTRIES IN lt_qals
           WHERE matnr = lt_qals-matnr.

    IF lt_mara IS NOT INITIAL.

      "取出MAKT物料描述表所需的字段
      SELECT matnr
        maktx
           INTO CORRESPONDING FIELDS OF TABLE lt_makt
          FROM makt
          FOR ALL ENTRIES IN lt_qals
          WHERE matnr = lt_qals-matnr
          AND maktx IN s_maktx.




      "取出字段判断首进物料
      SELECT mseg~matnr,
        mseg~werks,
        mseg~charg,
        mkpf~budat
        INTO CORRESPONDING FIELDS OF TABLE @lt_mseg
        FROM mseg LEFT JOIN mkpf ON mseg~mblnr = mkpf~mblnr
        FOR ALL ENTRIES IN @lt_qals
        WHERE mseg~matnr = @lt_qals-matnr AND
          mseg~werks = @lt_qals-werk AND
          mseg~charg = @lt_qals-charg AND
          mseg~bwart = '101' AND
          mseg~ebeln NE '0'.


    ENDIF.


  ENDIF.


  " LOOP循环取数
  LOOP AT lt_qals INTO ls_qals.

    MOVE-CORRESPONDING ls_qals TO gs_alv.
    READ TABLE  lt_mara INTO ls_mara WITH KEY matnr = ls_qals-matnr.  " 将物料表字段赋予
    IF sy-subrc = 0 .
      MOVE-CORRESPONDING ls_mara TO gs_alv.
    ENDIF.


    READ TABLE  lt_but000 INTO ls_but000 WITH KEY partner = ls_qals-lifnr. " 将供应商表字段赋予
    IF sy-subrc = 0 .
      gs_alv-lifnr = ls_but000-name_org1.
    ENDIF.

*    READ TABLE  lt_but000 INTO ls_but000 WITH KEY partner = gs_alv-mfrnr. "" 将供应商表字段赋予
    READ TABLE  lt_but000 INTO ls_but000 WITH KEY partner = ls_qals-hersteller. "" 将制造商表字段赋予
    IF sy-subrc = 0 .
      gs_alv-hersteller = ls_but000-name_org1.
    ENDIF.
    READ TABLE lt_makt INTO ls_makt WITH KEY matnr = gs_alv-matnr.   "将物料描述表字段赋予
    IF sy-subrc = 0 .
*      IF sy-subrc = 0 .
*      MOVE-CORRESPONDING ls_makt TO gs_alv.
*
*      READ TABLE lt_makt INTO ls_makt  WITH KEY matnr = gs_alv-matnr
*                                                BINARY SEARCH.
*      IF sy-subrc = 0 .
      gs_alv-maktx = ls_makt-maktx.
    ENDIF.

    READ TABLE lt_t142t INTO ls_t142t  WITH KEY raube = gs_alv-raube. "将存储条件描述表字段赋予
    IF sy-subrc = 0 .
      gs_alv-rbtxt = ls_t142t-rbtxt.
    ENDIF.
    READ TABLE lt_t001l INTO ls_t001l  WITH KEY lgort = gs_alv-lagortchrg     "将库存地点描述表字段赋予
                                                werks = gs_alv-werk.
    IF sy-subrc = 0 .
      gs_alv-lgobe = ls_t001l-lgobe.
    ENDIF.
*    ENDIF.

    " 获取批次属性和物料特征

    IF gs_alv-matnr IS NOT INITIAL.
      IF gs_alv-charg IS NOT INITIAL.
**          获取物料的批次信息  -- 包装规格 长批号 开箱数 到货日期 生产日期 有效日期
        CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
          EXPORTING
            i_matnr = gs_alv-matnr
            i_charg = gs_alv-charg
            i_werks = gs_alv-werk
          IMPORTING
            e_objek = lv_e_objek
            e_obtab = lv_e_obtab
            e_klart = lv_e_klart
            e_class = lv_e_class.
**          获取批次对象的属性信息
        IF sy-subrc = 0.
          lv_objectkey  =    lv_e_objek.
          lv_objecttable   = lv_e_obtab.
          lv_classnum  = lv_e_class.
          lv_classtype  = lv_e_klart.
          lv_objectkey_long = lv_e_objek.
          CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
            EXPORTING
              objectkey       = lv_objectkey
              objecttable     = lv_objecttable
              classnum        = lv_classnum
              classtype       = lv_classtype
              objectkey_long  = lv_objectkey_long
            TABLES
              allocvaluesnum  = lt_allocvaluesnum
              allocvalueschar = lt_allocvalueschar
              allocvaluescurr = lt_allocvaluescurr
              return          = lt_return.
          IF lt_return IS NOT INITIAL AND lt_return[ 1 ]-type = 'S'.
            DATA lv_date TYPE n LENGTH 8.
***              包装规格
            READ TABLE lt_allocvalueschar WITH KEY charact = 'ZPACK_STA' INTO DATA(ls_allocvalueschar).
            IF sy-subrc = 0.
              gs_alv-zpack_sta = ls_allocvalueschar-value_char. "包装规格
            ENDIF.
***              长批号
            CLEAR ls_allocvalueschar.
            READ TABLE lt_allocvalueschar WITH KEY charact = 'ZCHARGL' INTO ls_allocvalueschar.
            IF sy-subrc = 0.
              gs_alv-zchargl = ls_allocvalueschar-value_char_long.  "长批号
            ENDIF.
            " 开箱数
            CLEAR ls_allocvalueschar.
            READ TABLE lt_allocvalueschar WITH KEY charact = 'ZBOX_NUM' INTO ls_allocvalueschar.
            IF sy-subrc = 0.
              gs_alv-zbox_num = ls_allocvalueschar-value_char.  "开箱数
            ENDIF.
            " 到货日期
            CLEAR ls_allocvalueschar.
            READ TABLE lt_allocvaluesnum  WITH KEY charact = 'ZLWEDT' INTO DATA(ls_allocvaluesnum) .
            IF sy-subrc = 0.
              MOVE ls_allocvaluesnum-value_from TO lv_date.
              gs_alv-zlwedt = lv_date.  "到货日期
            ENDIF.
            " 生产日期
            CLEAR ls_allocvaluesnum.
            READ TABLE lt_allocvaluesnum WITH KEY charact = 'ZHSDAT' INTO ls_allocvaluesnum.
            IF sy-subrc = 0.
              MOVE ls_allocvaluesnum-value_from TO lv_date.
              gs_alv-zhsdat = lv_date.  "生产日期
            ENDIF.
            " 有效日期
            CLEAR ls_allocvaluesnum.
            READ TABLE lt_allocvaluesnum WITH KEY charact = 'ZVFDAT' INTO ls_allocvaluesnum.
            IF sy-subrc = 0.
              MOVE ls_allocvaluesnum-value_from TO lv_date.
              gs_alv-zvfdat = lv_date.  "有效日期
            ENDIF.
          ENDIF.
        ENDIF.

*      ELSE.

        CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
          EXPORTING
            matnr  = gs_alv-matnr
*           ATINN  =
          TABLES
            output = ltoutput.
*         " 开始取物料特征  " 过敏源
        READ TABLE  ltoutput WITH KEY atnam = 'ZALLERGEN' INTO DATA(ls_output).
        IF sy-subrc = 0.
          gs_alv-zallergen = ls_output-atwrt.
        ENDIF.
        "贵重物料
        CLEAR ls_output.
        READ TABLE  ltoutput WITH KEY atnam = 'ZEXPMAT' INTO ls_output.
        IF sy-subrc = 0.
          gs_alv-zexpmat = ls_output-atwrt.  " 贵重物料
        ENDIF.
        " 产地
        CLEAR ls_output.
        READ TABLE  ltoutput WITH KEY atnam = 'ZORIADD' INTO ls_output.
        IF sy-subrc = 0.
          gs_alv-zoriadd  = ls_output-atwrt.  " 产地
        ENDIF.
        CLEAR ls_output.
        READ TABLE  ltoutput WITH KEY atnam = 'ZMATDET' INTO ls_output.
        IF sy-subrc = 0.
          gs_alv-atwrt  = ls_output-atwrt.  " 材质明细
        ENDIF.
        CLEAR ls_output.
        READ TABLE  ltoutput WITH KEY atnam = 'ZNRMODE' INTO ls_output.
        IF sy-subrc = 0.
          gs_alv-znrmode  = ls_output-atwrt.  " 新零售
        ENDIF.
        "---------WWJ 20200410 改动--start------
        CLEAR ls_qprs2.
        READ TABLE lt_qprs2  INTO ls_qprs2  WITH KEY plos2 = ls_qals-prueflos .
        IF sy-subrc = 0.
          gs_alv-phynr  = ls_qprs2-phynr.  " 主样本号
          gs_alv-pn_nr  = ls_qprs2-pn_nr.  " 取样编号
        ENDIF.
        "---------WWJ 20200410 改动---end-----
      ENDIF.
    ENDIF.

    " 去除物料前面的零
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_alv-matnr
      IMPORTING
        output = gs_alv-matnr.
*   EXCEPTIONS
*     LENGTH_ERROR       = 1
*     OTHERS             = 2

    "  判断取样状态 ，取数
    READ TABLE lt_obtyp INTO ls_obtyp WITH KEY plos2 = gs_alv-prueflos.
    "---------WWJ 20200413 改动 增加是否首进物料字段--start------
    READ TABLE lt_mseg INTO ls_mseg WITH KEY  matnr = ls_qals-matnr
                                              werks = ls_qals-werk
                                              charg = ls_qals-charg.
    gs_alv-ftmat = '否'.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
        EXPORTING
          i_date_from = ls_mseg-budat
*         I_KEY_DAY_FROM       =
          i_date_to   = sysdate
*         I_KEY_DAY_TO         =
*         I_FLG_SEPARATE       = ' '
        IMPORTING
          e_days      = countday.
*       E_MONTHS    =
*       E_YEARS     =
      .
      IF countday LT 730.
        gs_alv-ftmat = '是'.
      ENDIF.
    ENDIF.
    "---------WWJ 20200413 改动 增加是否首进物料字段---end-----


    IF s_lsbox IS NOT INITIAL.
      IF s_lsbox = '10'.

        IF ls_obtyp-stat = 'I0002'.

*          gs_alv-obtyp = ls_obtyp-obtyp.
          APPEND gs_alv TO gt_alv.

        ENDIF.
      ELSEIF s_lsbox = '20'.

        IF ls_obtyp-stat <> 'I0002'.
*          gs_alv-obtyp = ls_obtyp-obtyp.
          APPEND gs_alv TO gt_alv.
        ENDIF.

      ENDIF.
    ELSE.
      APPEND gs_alv TO gt_alv.
    ENDIF.

    CLEAR gs_alv.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DIS_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_dis_alv .

  DATA: l_colpos TYPE lvc_s_fcat-col_pos VALUE 0.


  CLEAR gs_layout.

  gs_layout-cwidth_opt  = 'X'.   "设置Grid的字段列宽度自动适应


  CLEAR gt_fieldcat.



  macro_fill_fcat:
    l_colpos 'PRUEFLOS'    '样品编号'     '' ' ' ' ' '' '' '',
    l_colpos 'MATNR'    '物料编码'     '' ' ' ' ' '' '' '',
    l_colpos 'MAKTX'    '物料品名'     '' ' ' ' ' '' '' '',
    l_colpos 'CHARG'    'SAP批次号'     '' ' ' ' ' '' '' '',
    l_colpos 'ZCHARGL'    '长批号'     '' ' ' ' ' '' '' '',
    l_colpos 'ZPACK_STA'    '包装规格'     '' ' ' ' ' '' '' '',
    l_colpos 'LMENGEIST'    '批量'     '' ' ' ' ' '' '' '',
    l_colpos 'ZALLERGEN'    '过敏源'     '' ' ' ' ' '' '' '',
    l_colpos 'ZEXPMAT'    '贵重物料'     '' ' ' ' ' '' '' '',
    l_colpos 'LGOBE'    '库存地'     '' ' ' ' ' '' '' '',
    l_colpos 'ZBOX_NUM'    '开箱数'     '' ' ' ' ' '' '' '',
    l_colpos 'ZLWEDT'    '到货日期'     '' ' ' ' ' '' '' '',
    l_colpos 'LIFNR'    '供应商'     '' ' ' ' ' '' '' '',
    l_colpos 'HERSTELLER'    '制造商'     '' ' ' ' ' '' '' '',
    l_colpos 'ZORIADD'    '产地'     '' ' ' ' ' '' '' '',
    l_colpos 'ZHSDAT'    '生产日期'     '' ' ' ' ' '' '' '',
    l_colpos 'ZVFDAT'    '有效日期'     '' ' ' ' ' '' '' '',
    l_colpos 'RBTXT'    '储存条件'     '' ' ' ' ' '' '' '',
    l_colpos 'ATWRT'    '材质明细'     '' ' ' ' ' '' '' '',
    l_colpos 'PHYNR'    '主样本号'     '' ' ' ' ' '' '' '',
    l_colpos 'PN_NR'    '取样编号'     '' ' ' ' ' '' '' '',
*    l_colpos 'FTMAT'    '首进物料'     '' ' ' ' ' '' '' '',
    l_colpos 'ZNRMODE'    '新零售'     '' ' ' ' ' '' '' ''.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
*     I_CALLBACK_PF_STATUS_SET = 'FRM_SET_PF_STATUS'
*     I_CALLBACK_USER_COMMAND  = 'FRM_USER_COMMAND'
      is_layout_lvc      = gs_layout
      it_fieldcat_lvc    = gt_fieldcat
*     IT_EVENTS          = I_EVENTS[]
      i_save             = 'A'
    TABLES
      t_outtab           = gt_alv
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text  设置alv状态
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM frm_set_pf_status USING pt_extab TYPE slis_t_extab.


  SET PF-STATUS 'STANDARD_FULLSCREEN'  .

ENDFORM.                    "_SET_PF_STATUS

FORM frm_user_command USING pv_ucomm LIKE sy-ucomm
                             ps_selfield TYPE slis_selfield.
  DATA: lv_grid  TYPE REF TO cl_gui_alv_grid.
  DATA: lv_flag  TYPE c.
  DATA:BEGIN OF ls_mblnr,
         mblnr TYPE mseg-mblnr,
       END OF ls_mblnr.
  DATA lt_mblnr LIKE TABLE OF ls_mblnr.
  DATA num TYPE i.


*&将变更的数据刷新
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lv_grid.
  CALL METHOD lv_grid->check_changed_data.
  ps_selfield-refresh = 'X'.

  TRY.
      DATA(ls_alv) = gt_alv[ ps_selfield-tabindex ].
    CATCH cx_sy_itab_line_not_found.

  ENDTRY.





  CALL METHOD lv_grid->refresh_table_display.
ENDFORM.

FORM frm_authority_check.
  AUTHORITY-CHECK OBJECT 'Q_INSPLOT'
*    ID 'QACT_ILOT' FIELD 'xxxxxxxx'
   ID 'WERKS' FIELD s_werk.
*    ID 'QPRUEFART' FIELD 'xxxxxxxx'
*    ID 'QMATAUTH' FIELD 'xxxxxxxx'.
  IF sy-subrc <> 0.
*   Implement a suitable exception handling here
    MESSAGE '权限检查失败' TYPE 'E'.
    gv_sel_error = 'X'.
  ELSE.
    MESSAGE '有权限' TYPE 'S'.
    gv_sel_error = ''.
  ENDIF.

ENDFORM.