*&---------------------------------------------------------------------*
*& 包含               ZMMR0050_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form FRM_REFRESH_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> G_GUI_ALV
*&      --> GS_LAYOUT
*&---------------------------------------------------------------------*
FORM frm_refresh_alv  USING  p_grid TYPE REF TO  cl_gui_alv_grid
                              p_lay  TYPE lvc_s_layo.
  DATA: ls_stable TYPE lvc_s_stbl.
  ls_stable-row = 'X'.
  ls_stable-col = 'X'.

*  CLEAR I_SORT.
*  CLEAR I_SORT[].
*  I_SORT-FIELDNAME = 'ZDZDH'.
*  I_SORT-SPOS = 1.
*  I_SORT-UP = 'X'.
*  I_SORT-SUBTOT = 'X'.
*  APPEND I_SORT  .

  CALL METHOD p_grid->set_frontend_layout
    EXPORTING
      is_layout = p_lay.

*  CALL METHOD P_GRID->SET_FRONTEND_FIELDCATALOG
*    EXPORTING
*      IT_FIELDCATALOG = GT_FIELDCAT.
*
*  CALL METHOD P_GRID->SET_FLAG_TOTAL_COLUMN
*    EXPORTING
*      I_UCOMM = '&SUBTOT'.

  CALL METHOD p_grid->refresh_table_display
    EXPORTING
      is_stable      = ls_stable
      i_soft_refresh = 'X'.


*  DATA LS_FCAT TYPE LVC_S_FCAT .
*  DATA LT_FCAT TYPE LVC_T_FCAT .
*  DATA LS_LAYOUT TYPE LVC_S_LAYO .
*  CALL METHOD P_GRID->GET_FRONTEND_FIELDCATALOG
*    IMPORTING
*      ET_FIELDCATALOG = LT_FCAT[].
*  LOOP AT LT_FCAT INTO LS_FCAT .
*    IF LS_FCAT-FIELDNAME = 'PAYMENTSUM' .
*      LS_FCAT-NO_OUT = SPACE .
*      MODIFY LT_FCAT FROM LS_FCAT .
*    ENDIF .
*  ENDLOOP .
*  CALL METHOD P_GRID->SET_FRONTEND_FIELDCATALOG
*    EXPORTING
*      IT_FIELDCATALOG = LT_FCAT[].
*
*  CALL METHOD P_GRID->GET_FRONTEND_LAYOUT
*    IMPORTING
*      ES_LAYOUT = LS_LAYOUT.
*  LS_LAYOUT-GRID_TITLE = 'Flights (with Payment Sums)' .
*  CALL METHOD P_GRID->SET_FRONTEND_LAYOUT
*    EXPORTING
*      IS_LAYOUT = LS_LAYOUT.






ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_data .
  DATA : ls_t001w TYPE t001w .
  DATA : lt_t001w TYPE TABLE OF t001w .

  CLEAR lt_t001w .
  SELECT *
    INTO TABLE lt_t001w
    FROM t001w
    WHERE werks IN s_werks .

  LOOP AT lt_t001w INTO ls_t001w .
    AUTHORITY-CHECK OBJECT 'M_BEST_WRK'
*     ID  'WERKS' FIELD S_WERKS .
       ID  'WERKS' FIELD ls_t001w-werks .
    IF sy-subrc <> 0 .
      MESSAGE '没有对应工厂的查看权限'  TYPE 'S' DISPLAY LIKE 'E' .
      LEAVE LIST-PROCESSING .
    ENDIF .
  ENDLOOP .


  PERFORM frm_get_zdzztms.

  CASE 'X'.
    WHEN rb_01 .

      PERFORM frm_get_ekbe.

      PERFORM frm_get_rseg .

      PERFORM frm_get_mseg. "获取冲销凭证

      PERFORM frm_get_dzxx.  "获取以对账信息

      PERFORM frm_get_cabn.  "获取长批号

      PERFORM frm_get_zsl.  " 获取税率

      PERFORM frm_get_out.   "最终数据数据处理

    WHEN rb_03 OR rb_02.
      PERFORM frm_get_data2.

    WHEN rb_04.
      PERFORM frm_get_data3.

  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_DEAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_deal_data .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_EKBE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_ekbe .
  DATA: gt_out2 TYPE STANDARD TABLE OF ty_tab WITH HEADER LINE.
  SELECT ekbe~ebeln
         ekbe~ebelp
         ekbe~gjahr
   	     ekbe~belnr
         ekbe~buzei
*         EKPO~EBELN
         ekko~ernam AS nampo
         ekko~lifnr
         ekko~bukrs
         ekbe~werks
         ekbe~matnr
         ekbe~charg
         ekbe~bewtp
         ekbe~bwart
         ekbe~shkzg
         ekbe~budat
         ekbe~menge
         ekpo~meins
         ekpo~umrez
         ekpo~umren
         ekpo~retpo
         ekbe~hswae
         ekpo~mwskz
         ekbe~dmbtr
         ekbe~waers
         makt~maktx
         ekbe~lfgja
         ekbe~lfbnr
         ekbe~lfpos
         FROM ekbe
         INNER JOIN ekpo
         ON  ekbe~ebeln  = ekpo~ebeln
         AND ekbe~ebelp  = ekpo~ebelp
         INNER JOIN ekko
         ON  ekbe~ebeln = ekko~ebeln
         INNER JOIN makt
         ON ekpo~matnr = makt~matnr
         INTO CORRESPONDING FIELDS OF TABLE gt_out
         WHERE ekko~ernam IN  s_ernam
         AND   ekko~lifnr IN   s_lifnr
         AND   ekbe~budat IN   s_budat
         AND   ekbe~ebeln IN   s_ebeln
         AND   ekbe~charg IN   s_charg
         AND   ekbe~waers IN   s_waers
         AND   ekko~bsart IN   s_bsart
         AND   ekbe~werks IN   s_werks
         AND   ekpo~matnr IN   s_matnr
         AND   ekpo~repos <> ''
         AND   ekpo~knttp <> 'A'
*         AND   EKPO~PSTYP <> '2'
         AND   ekpo~pstyp IN   s_pstyp
         AND   ekbe~bewtp = 'E'
*         AND   EXISTS ( SELECT * FROM MSEG WHERE MJAHR   = EKBE-GJAHR
*                                             AND   MJAHR   = EKBE-BELNR
*                                             AND   ZEILE   = EKBE-BUZEI
*                                             AND   SMBLN   = ''  ). "排除已经冲销的凭证
*         AND NOT EXISTS ( SELECT * FROM EKBE WHERE  BWART = ''
*                                             AND    LFGJA   = EKBE-GJAHR
*                                             AND   LFBNR   = EKBE-BELNR
*                                             AND   LFPOS   = EKBE-BUZEI   )"排除已经发票检验过的
*       AND NOT EXISTS ( SELECT * FROM RSEG
*                              INNER JOIN RBKP
*                              ON RBKP~BELNR =  RSEG~BELNR
*                             AND RBKP~GJAHR =  RSEG~GJAHR
*                               WHERE RSEG~LFGJA = EKBE~GJAHR
*                                AND  RSEG~LFBNR = EKBE~BELNR
*                                AND  RSEG~LFPOS = EKBE~BUZEI
*                                AND  RBKP~STBLG = ''   )
    .

***************Seashell Huang 20210311 ********
***********(1) 在原先的基础上遍历内表，加上同个物料同个工厂同个批次同个采购订单的数据以及不同采购订单但是
  "同个物料同个工厂同个批次的数据
  MOVE gt_out[] TO gt_out2[].
  CLEAR gt_out.
  IF gt_out2 IS NOT INITIAL.

  SELECT ekbe~ebeln
         ekbe~ebelp
         ekbe~gjahr
   	     ekbe~belnr
         ekbe~buzei
*         EKPO~EBELN
         ekko~ernam AS nampo
         ekko~lifnr
         ekko~bukrs
         ekbe~werks
         ekbe~matnr
         ekbe~charg
         ekbe~bewtp
         ekbe~bwart
         ekbe~shkzg
         ekbe~budat
         ekbe~menge
         ekpo~meins
         ekpo~umrez
         ekpo~umren
         ekpo~retpo
         ekbe~hswae
         ekpo~mwskz
         ekbe~dmbtr
         ekbe~waers
         makt~maktx
         ekbe~lfgja
         ekbe~lfbnr
         ekbe~lfpos
         FROM ekbe
         INNER JOIN ekpo
         ON  ekbe~ebeln  = ekpo~ebeln
         AND ekbe~ebelp  = ekpo~ebelp
         INNER JOIN ekko
         ON  ekbe~ebeln = ekko~ebeln
         INNER JOIN makt
         ON ekpo~matnr = makt~matnr
         APPENDING CORRESPONDING FIELDS OF TABLE gt_out
         FOR ALL ENTRIES IN gt_out2
         WHERE ekbe~matnr = gt_out2-matnr
           AND ekbe~werks = gt_out2-werks
           AND ekbe~charg = gt_out2-charg.

  ENDIF.

******************20210311 END ************

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_data2 .


  IF  rb_02 = 'X'.
    SELECT  zmmt0004~zdzdh
            zmmt0004~zdzxh
            zmmt0004~zdzzt
            zmmt0004~ebeln
            zmmt0004~ebelp
            zmmt0004~gjahr
            zmmt0004~belnr
            zmmt0004~buzei
            zmmt0004~lifnr
            zmmt0004~name1
            zmmt0004~werks
            zmmt0004~matnr
            zmmt0004~maktx
            zmmt0004~atwrt
            zmmt0004~charg
            zmmt0004~bewtp
            zmmt0004~bwart
            zmmt0004~shkzg
            zmmt0004~budat
            zmmt0004~menge
            zmmt0004~meins
            zmmt0004~zdwhsl
            zmmt0004~zshsl
            zmmt0004~hswae
            zmmt0004~mwskz
            zmmt0004~dmbtr
            zmmt0004~zsl
            zmmt0004~zhsdj
            zmmt0004~zsjhj
            zmmt0004~zsjzt
            zmmt0004~ernam
            zmmt0004~erdat
            zmmt0004~zernam_d
            zmmt0004~zerdat_d
            zmmt0004~zernam_z
            zmmt0004~zerdat_z
           zmmt0004~lfgja
           zmmt0004~lfbnr
           zmmt0004~lfpos
           zmmt0004~nampo
           zmmt0004~zfkts
           zmmt0004~zfkrq
           zmmt0004~zshui
           zmmt0004~zcanz
        FROM zmmt0004
         INNER JOIN ekpo
         ON zmmt0004~ebeln = ekpo~ebeln
         AND zmmt0004~ebelp = ekpo~ebelp
         INTO CORRESPONDING FIELDS OF TABLE gt_alv
           WHERE zmmt0004~lifnr IN   s_lifnr
*           AND   zmmt0004~budat IN   s_budat“2021.03.22修改，自定义表筛选的是前台的创建日期，而不是过账日期Seashell Huang
           AND   zmmt0004~erdat IN   s_erdat
           AND   zmmt0004~zdzzt IN   s_zdzzt
           AND   zmmt0004~ebeln IN   s_ebeln
           AND   zmmt0004~werks IN   s_werks
           AND   zmmt0004~namla IN   s_namla
           AND   zmmt0004~charg IN   s_charg
           AND   zmmt0004~waers IN   s_waers
           AND   zmmt0004~matnr IN   s_matnr
           AND   zmmt0004~zdzdh IN   s_zdzdh
           AND   zmmt0004~zdzzt NOT IN  ('Y','J')
           AND   zmmt0004~nampo IN s_ernam
           AND   ekpo~pstyp IN   s_pstyp
           AND EXISTS ( SELECT * FROM ekko WHERE ebeln = zmmt0004~ebeln AND bsart IN s_bsart ).
  ELSE.
    SELECT  zmmt0004~zdzdh
            zmmt0004~zdzxh
            zmmt0004~zdzzt
            zmmt0004~ebeln
            zmmt0004~ebelp
            zmmt0004~gjahr
            zmmt0004~belnr
            zmmt0004~buzei
            zmmt0004~lifnr
            zmmt0004~name1
            zmmt0004~werks
            zmmt0004~matnr
            zmmt0004~maktx
            zmmt0004~atwrt
            zmmt0004~charg
            zmmt0004~bewtp
            zmmt0004~bwart
            zmmt0004~shkzg
            zmmt0004~budat
            zmmt0004~menge
            zmmt0004~meins
            zmmt0004~zdwhsl
            zmmt0004~zshsl
            zmmt0004~hswae
            zmmt0004~mwskz
            zmmt0004~dmbtr
            zmmt0004~zsl
            zmmt0004~zhsdj
            zmmt0004~zsjhj
            zmmt0004~zsjzt
            zmmt0004~ernam
            zmmt0004~erdat
            zmmt0004~zernam_d
            zmmt0004~zerdat_d
            zmmt0004~zernam_z
            zmmt0004~zerdat_z
           zmmt0004~lfgja
           zmmt0004~lfbnr
           zmmt0004~lfpos
           zmmt0004~nampo
           zmmt0004~zfkts
           zmmt0004~zfkrq
           zmmt0004~zshui
           zmmt0004~zcanz
       FROM zmmt0004
       INNER JOIN ekpo
       ON zmmt0004~ebeln = ekpo~ebeln
       AND zmmt0004~ebelp = ekpo~ebelp
       INTO CORRESPONDING FIELDS OF TABLE gt_alv
         WHERE zmmt0004~lifnr IN   s_lifnr
*         AND   zmmt0004~erdat IN   s_budat“2021.03.22修改，自定义表筛选的是前台的创建日期，而不是过账日期Seashell Huang
         AND   zmmt0004~erdat IN   s_erdat
         AND   zmmt0004~zdzzt IN   s_zdzzt
         AND   zmmt0004~ebeln IN   s_ebeln
         AND   zmmt0004~werks IN   s_werks
           AND   zmmt0004~namla IN   s_namla
           AND   zmmt0004~charg IN   s_charg
           AND   zmmt0004~waers IN   s_waers
         AND   zmmt0004~matnr IN   s_matnr
           AND   zmmt0004~zdzdh IN   s_zdzdh
           AND   zmmt0004~nampo IN s_ernam
         AND   ekpo~pstyp IN   s_pstyp
         AND EXISTS ( SELECT * FROM ekko WHERE ebeln = zmmt0004~ebeln AND bsart IN s_bsart ).
  ENDIF.

  "一个对账单可能存在不同的批次和不同的最后修改人 因此重新进行获取对账单
  IF s_charg[] IS NOT INITIAL
  OR s_namla[] IS NOT INITIAL .
    PERFORM frm_re_get_zmmt0004 TABLES gt_alv ."
  ENDIF .

  LOOP AT  gt_alv INTO gs_alv .

    READ TABLE gt_dom_apstt INTO DATA(ls_dom_apstt) WITH KEY domvalue_l = gs_alv-zdzzt.
    IF  sy-subrc = 0.
      gs_alv-zdzztms  = ls_dom_apstt-ddtext.
    ENDIF.

    MODIFY gt_alv FROM gs_alv.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_SHOW_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_show_alv .

  CALL SCREEN 0100.

ENDFORM.



*&---------------------------------------------------------------------*
*& Form INIT_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init_alv .
  CLEAR gt_fieldcat[] .
  IF  rb_04 = 'X'.
    fill_field_list  'SEL'    '选择'  '' ''  ''  'X'   'X' '' '' .
    fill_field_list  'MESSAGE'  '消息'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZDZDH'  '对账单号'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZDZZT'  '对账状态'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZDZZTMS'  '对账状态描述'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'LIFNR'  '供应商编号'  'LFA1' 'LIFNR'  ''  ''   '' '' '' .
    fill_field_list  'NAME1'  '供应商'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'NAMPO'  '制单人'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZSHSL'  '数量'  '' ''  'X'  ''   '' '' '' .
    fill_field_list  'ZSHUI'  '税额'  '' ''  'X'  ''   '' '' '' .
    fill_field_list  'DMBTR'  '净值'  '' ''  'X'  ''   '' '' '' .
    fill_field_list  'ZSJHJ'  '价税合计'  '' ''  'X'  ''   '' '' '' .
    fill_field_list  'ZBUDAT_Q'  '发票校验过账日期'  'ZMMT0004' 'BUDAT'  ''  ''   'X' '' '' .
    fill_field_list  'BLINE_DATE'  '起算日期'  'ZMMT0004' 'BUDAT'  ''  ''   'X' '' '' .
    fill_field_list  'PMNTTRMS'  '付款方式'  '' ''  ''  ''   'X' 'X' '' .
    fill_field_list  'ZFKTS'  '付款天数'  '' ''  ''  ''   'X' '' '' .
    fill_field_list  'ZFKRQ'  '付款日期'  'ZMMT0004' 'BUDAT'  ''  ''   '' '' '' .
    fill_field_list  'ZDZWB'  '凭证文本'  '' ''  ''  ''   'X' '' '200' .
    fill_field_list  'ZCANZ'  '参照' 'ZMMT0004'  'ZCANZ'  '' ''   'X' '' '100' .
    fill_field_list  'ERNAM'  '对账单创建人'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ERDAT'  '对账单创建日期'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZERNAM_D'  '对账确认人'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZERDAT_D'  '对账确认时间'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZERNAM_Z'  '取消对账人'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZERDAT_Z'  '取消对账日期'  '' ''  ''  ''   '' '' '' .
*    FILL_FIELD_LIST  'WAERS'  '币种'  '' ''  ''  ''   '' '' '' .
*    FILL_FIELD_LIST  'CHARG'  '批次'  '' ''  ''  ''   '' '' '' .


  ELSE.

    IF rb_01 = 'X' OR rb_02 = 'X'.
      fill_field_list  'SEL'    '选择'  '' ''  ''  'X'   'X' '' '' .
    ENDIF.
    fill_field_list  'ZDZDH'  '对账单号'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZDZXH'  '对账单行项目'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZDZZT'  '对账状态'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZDZZTMS'  '对账状态描述'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZSJZT'  '数据状态'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'EBELN'  '采购凭证'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'EBELP'  '项目'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'BELNR'  '物料凭证'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'BUZEI'  '物料凭证项目'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'LIFNR'  '供应商编号'  'LFA1' 'LIFNR'  ''  ''   '' '' '' .
    fill_field_list  'NAME1'  '供应商'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'WERKS'  '工厂'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'NAMPO'  '制单人'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'MATNR'  '物料编码'  'MARA' 'MATNR'  ''  ''   '' '' '' .
    fill_field_list  'MAKTX'  '物料描述'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ATWRT'  '长批号'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'CHARG'  'SAP批号'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'WAERS'  '币种'  '' ''  ''  ''   '' '' '' .
*    FILL_FIELD_LIST  'CHARG'  '批次'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'BEWTP'  '采购订单历史记录类别'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'BWART'  '移动类型'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'SHKZG'  '借/贷标识'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'BUDAT'  '过账日期'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'MENGE'  '收货数量'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'MEINS'  '单位'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZDWHSL' '单位换算率'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZSHSL'  '收货数量(采购单位)'  '' ''  'X'  ''   '' '' '' .
    fill_field_list  'HSWAE'  '币别'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'MWSKZ'  '税码'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'DMBTR'  '未含税金额'  '' ''  'X'  ''   '' '' '' .
    fill_field_list  'ZSL'    '税率'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZSHUI'    '税额'  '' ''  'X'  ''   '' '' '' .
    fill_field_list  'ZHSDJ'  '含税单价'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZSJHJ'  '税价合计'  '' ''  'X'  ''   '' '' '' .
    fill_field_list  'LFGJA'  '参考凭证年度'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'LFBNR'  '参考凭证'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'LFPOS'  '参考凭证行项目'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ERNAM'  '对账单创建人'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ERDAT'  '对账单创建日期'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZERNAM_D'  '对账确认人'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZERDAT_D'  '对账确认时间'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZERNAM_Z'  '取消对账人'  '' ''  ''  ''   '' '' '' .
    fill_field_list  'ZERDAT_Z'  '取消对账日期'  '' ''  ''  ''   '' '' '' .
  ENDIF.



*  LOOP AT GT_OUT INTO GS_OUT.
*    GV_TABIX = SY-TABIX.
*
*    CLEAR:GS_OUT-STYLE.
*    LOOP AT GT_FIELDCAT INTO GS_FIELDCAT WHERE  FIELDNAME = 'SEL'.
*      GS_EDIT-FIELDNAME = GS_FIELDCAT-FIELDNAME.
*      GS_EDIT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*      INSERT GS_EDIT INTO TABLE GS_OUT-STYLE.
*    ENDLOOP.
*    MODIFY GT_OUT FROM GS_OUT INDEX GV_TABIX TRANSPORTING STYLE.
*    CLEAR : GS_OUT.
*  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form FRM_PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GS_LAYOUT
*&---------------------------------------------------------------------*
FORM frm_prepare_layout  CHANGING gw_layout TYPE lvc_s_layo .
  gw_layout-zebra         = 'X' .
  gw_layout-cwidth_opt    = 'X'.
  gw_layout-keyhot        = 'X'.
  gw_layout-stylefname    = 'STYLE'.
  gw_layout-box_fname     = 'MARK'.
  gw_layout-sel_mode      = 'A'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_MSEG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_mseg .

  DATA:lt_alv LIKE TABLE OF gs_alv.


  APPEND LINES OF gt_out TO lt_alv.

  SORT lt_alv BY gjahr
                 belnr
                 buzei.
  DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING gjahr
                                                   belnr
                                                   buzei.

  IF  lt_alv IS NOT INITIAL .
**取出该凭证是否是冲销其他凭证的物料凭证
    SELECT mblnr
           mjahr
           zeile
           sjahr
           smbln
           smblp
           FROM mseg INTO TABLE gt_mseg
           FOR ALL ENTRIES IN lt_alv
           WHERE sjahr  = lt_alv-gjahr
           AND   smbln  = lt_alv-belnr
           AND   smblp  = lt_alv-buzei.
****取出凭证已经被冲销的凭证
    SELECT mblnr
         mjahr
         zeile
         sjahr
         smbln
         smblp
         FROM mseg INTO TABLE gt_mseg
         FOR ALL ENTRIES IN lt_alv
         WHERE sjahr  = lt_alv-gjahr
         AND   smbln  = lt_alv-belnr
         AND   smblp  =  lt_alv-buzei
         AND   sjahr  NE '0000'
         AND   smbln  NE ''
         AND   smblp  NE '0000'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DZXX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_dzxx .

  DATA:lt_alv TYPE TABLE OF ty_tab.
  APPEND LINES OF gt_out TO lt_alv.

  SORT lt_alv BY gjahr belnr buzei.

  DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING gjahr belnr buzei.

  IF  lt_alv IS NOT INITIAL .

    SELECT * FROM zmmt0004 INTO TABLE gt_zmmt0004
       FOR ALL ENTRIES IN lt_alv
       WHERE  gjahr   = lt_alv-gjahr
       AND    belnr   = lt_alv-belnr
       AND    buzei   = lt_alv-buzei
       AND    zdzzt   IN ( 'Y' , 'N' , 'Q' ).
  ENDIF.
  FREE lt_alv.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_OUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_out .


  DATA:BEGIN OF ls_lfa1,
         lifnr TYPE  lfa1-lifnr,
         name1 TYPE  lfa1-name1,
       END OF ls_lfa1,
       lt_lfa1 LIKE TABLE OF ls_lfa1.

  DATA : lv_menge TYPE ekbe-menge .

  DATA:lt_alv TYPE TABLE OF ty_tab.

  APPEND LINES OF gt_out TO lt_alv.

  SORT lt_alv BY lifnr.

  DELETE ADJACENT DUPLICATES FROM lt_alv  COMPARING lifnr.

  IF  lt_alv IS NOT INITIAL .
    SELECT lifnr
           name1
           FROM lfa1
           INTO TABLE lt_lfa1
           FOR ALL ENTRIES IN lt_alv
           WHERE lifnr = lt_alv-lifnr.

    SORT lt_lfa1 BY lifnr.
  ENDIF.

  FREE lt_alv.


  SORT gt_zmmt0004 BY gjahr belnr buzei.
  SORT gt_mseg BY sjahr
                  smbln
                  smblp.


  gt_mseg1[] = gt_mseg[] .
  SORT gt_mseg1 BY mblnr
                   mjahr
                   zeile.

  DATA : lt_outs TYPE TABLE OF ty_tab.
  FIELD-SYMBOLS : <ls_outh> TYPE ty_tab.
  DATA : lt_outh TYPE TABLE OF ty_tab.

  lt_outs[] = gt_out[] .
  lt_outh[] = gt_out[] .
  DELETE lt_outs WHERE shkzg <> 'S' .
  DELETE lt_outh WHERE shkzg <> 'H' .
*********************************************************************************
  LOOP AT  lt_outs INTO gs_out ."运算S

    "检查是否已开票
    CLEAR:gs_zmmt0004 .
    READ TABLE gt_zmmt0004 INTO gs_zmmt0004 WITH KEY gjahr = gs_out-gjahr belnr = gs_out-belnr buzei = gs_out-buzei BINARY SEARCH .
    IF  sy-subrc = 0.

      CONTINUE.
    ENDIF.

    "检查是否被冲销
    CLEAR:gs_mseg.
    READ TABLE gt_mseg INTO gs_mseg WITH KEY sjahr  = gs_out-gjahr
                                             smbln  = gs_out-belnr
                                             smblp  = gs_out-buzei BINARY SEARCH .
    IF  sy-subrc = 0.
      CONTINUE.

    ENDIF.

    "检查是否为冲销凭证
    CLEAR:gs_mseg.
    READ TABLE gt_mseg1 INTO gs_mseg WITH KEY mjahr  = gs_out-gjahr
                                              mblnr  = gs_out-belnr
                                              zeile  = gs_out-buzei BINARY SEARCH.
    IF  sy-subrc = 0.
      CONTINUE.
    ENDIF.


    "计算开票剩余数量
    CLEAR lv_menge .
    lv_menge = gs_out-menge .
    IF gs_out-shkzg =  'H'.
      lv_menge = 0 - lv_menge .
    ENDIF .

    CLEAR gs_rseg .
    LOOP AT gt_rseg INTO gs_rseg   WHERE lfgja = gs_out-gjahr
                                     AND lfbnr = gs_out-belnr
                                     AND lfpos = gs_out-buzei .
      IF gs_rseg-shkzg =  'H'.
        gs_rseg-menge = 0 - gs_rseg-menge .
      ENDIF .
      lv_menge = lv_menge - gs_rseg-menge .

    ENDLOOP .
    CHECK lv_menge <> 0 .

    LOOP AT lt_outh ASSIGNING <ls_outh> WHERE ebeln = gs_out-ebeln
                                    AND ebelp = gs_out-ebelp
                                    AND charg = gs_out-charg .

      " 排除冲销凭证的数据 ADD BY LANJINGCUN 20210121-S711
      "检查是否被冲销
      CLEAR:gs_mseg.
      READ TABLE gt_mseg INTO gs_mseg WITH KEY sjahr  = <ls_outh>-gjahr
                                               smbln  = <ls_outh>-belnr
                                               smblp  = <ls_outh>-buzei BINARY SEARCH .
      IF  sy-subrc = 0.
        CONTINUE.

      ENDIF.

      "检查是否为冲销凭证
      CLEAR:gs_mseg.
      READ TABLE gt_mseg1 INTO gs_mseg WITH KEY mjahr  = <ls_outh>-gjahr
                                                mblnr  = <ls_outh>-belnr
                                                zeile  = <ls_outh>-buzei BINARY SEARCH.
      IF  sy-subrc = 0.
        CONTINUE.
      ENDIF.
      " 排除冲销凭证的数据 ADD BY LANJINGCUN 20210121-E711
      IF lv_menge >= <ls_outh>-menge  .
        lv_menge = lv_menge - <ls_outh>-menge .
        <ls_outh>-menge  = 0 .
      ELSE .
        <ls_outh>-menge = <ls_outh>-menge - lv_menge .
        lv_menge = 0 .
      ENDIF .
      IF lv_menge = 0 .
        EXIT .
      ENDIF .
    ENDLOOP .
    CHECK lv_menge <> 0 .

    MOVE-CORRESPONDING gs_out TO gs_alv.

    CLEAR:ls_lfa1.
    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = gs_out-lifnr BINARY SEARCH .
    IF  sy-subrc = 0.
      gs_alv-name1 = ls_lfa1-name1.
    ENDIF.

    CLEAR:gs_mcha.
    READ TABLE gt_mcha INTO gs_mcha WITH KEY matnr = gs_alv-matnr werks = gs_alv-werks charg = gs_alv-charg BINARY SEARCH.
    IF  sy-subrc = 0.
      gs_alv-cuobj_bm = gs_mcha-cuobj_bm.
    ENDIF.


    CLEAR:gs_ausp.

    READ TABLE gt_ausp INTO gs_ausp WITH KEY objek = gs_alv-cuobj_bm BINARY SEARCH.
    IF  sy-subrc = 0.
      gs_alv-atwrt = gs_ausp-atwrt.


    ENDIF.

    CLEAR:gs_a003.
    READ TABLE gt_a003 INTO gs_a003 WITH KEY mwskz = gs_alv-mwskz  BINARY SEARCH .
    IF  sy-subrc = 0.
      gs_alv-kbetr = gs_a003-kbetr.
    ENDIF.
    IF gs_alv-umren NE 0 .


      gs_alv-zdwhsl = gs_alv-umrez / gs_alv-umren.
    ENDIF.

    lv_menge = abs( lv_menge ) .
    gs_alv-zsl = gs_alv-kbetr / 1000.

    gs_alv-zhsdj = ( 1 + gs_alv-zsl ) * gs_alv-dmbtr / gs_alv-menge.

    gs_alv-zsjhj = ( 1 + gs_alv-zsl ) * gs_alv-dmbtr / gs_alv-menge * lv_menge .

*    GS_ALV-ZSHUI = GS_ALV-ZSJHJ - GS_ALV-DMBTR.
    gs_alv-zshui = gs_alv-zsl  * gs_alv-dmbtr / gs_alv-menge * lv_menge .
    gs_alv-menge = lv_menge .
    gs_alv-zsjzt = 'N'.
    IF  gs_alv-zdwhsl = 0  .
      gs_alv-zshsl =   gs_alv-menge.
    ELSE .
      gs_alv-zshsl =   gs_alv-menge  / gs_alv-zdwhsl.
    ENDIF.

    gs_alv-dmbtr = gs_alv-zsjhj - gs_alv-zshui .

    gs_alv-zdzzt = 'N'.
    IF  gs_alv-shkzg =  'H'.
      gs_alv-zsjhj  =  0  -  gs_alv-zsjhj .
      gs_alv-dmbtr  =  0  -  gs_alv-dmbtr .
      gs_alv-zshui  =  0  -  gs_alv-zshui .
      gs_alv-zshsl  =  0  -  gs_alv-zshsl .
      gs_alv-menge  =  0  -  gs_alv-menge .
    ENDIF.

    gs_alv-ernam       =  sy-uname.
    gs_alv-erdat       =  sy-datum.

    READ TABLE gt_dom_apstt INTO DATA(ls_dom_apstt) WITH KEY domvalue_l = gs_alv-zdzzt.
    IF  sy-subrc = 0.
      gs_alv-zdzztms  = ls_dom_apstt-ddtext.
    ENDIF.

    APPEND gs_alv TO gt_alv.
    CLEAR:gs_alv.
  ENDLOOP.
**************************************************************************
  LOOP AT  lt_outh INTO gs_out WHERE retpo <> '' ."运算H 退货的才需要参与运算

    "检查是否已开票
    CLEAR:gs_zmmt0004 .
    READ TABLE gt_zmmt0004 INTO gs_zmmt0004 WITH KEY gjahr = gs_out-gjahr belnr = gs_out-belnr buzei = gs_out-buzei BINARY SEARCH .
    IF  sy-subrc = 0.

      CONTINUE.
    ENDIF.

    "检查是否被冲销
    CLEAR:gs_mseg.
    READ TABLE gt_mseg INTO gs_mseg WITH KEY sjahr  = gs_out-gjahr
                                             smbln  = gs_out-belnr
                                             smblp  = gs_out-buzei BINARY SEARCH .
    IF  sy-subrc = 0.
      CONTINUE.

    ENDIF.

    "检查是否为冲销凭证
    CLEAR:gs_mseg.
    READ TABLE gt_mseg1 INTO gs_mseg WITH KEY mjahr  = gs_out-gjahr
                                              mblnr  = gs_out-belnr
                                              zeile  = gs_out-buzei BINARY SEARCH.
    IF  sy-subrc = 0.
      CONTINUE.
    ENDIF.


    "计算开票剩余数量
    CLEAR lv_menge .
    lv_menge = gs_out-menge .
    IF gs_out-shkzg =  'H'.
      lv_menge = 0 - lv_menge .
    ENDIF .

    CLEAR gs_rseg .
    LOOP AT gt_rseg INTO gs_rseg   WHERE lfgja = gs_out-gjahr
                                     AND lfbnr = gs_out-belnr
                                     AND lfpos = gs_out-buzei .
      IF gs_rseg-shkzg =  'H'.
        gs_rseg-menge = 0 - gs_rseg-menge .
      ENDIF .
      lv_menge = lv_menge - gs_rseg-menge .

    ENDLOOP .
    CHECK lv_menge <> 0 .

    MOVE-CORRESPONDING gs_out TO gs_alv.

    CLEAR:ls_lfa1.
    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = gs_out-lifnr BINARY SEARCH .
    IF  sy-subrc = 0.
      gs_alv-name1 = ls_lfa1-name1.
    ENDIF.

    CLEAR:gs_mcha.
    READ TABLE gt_mcha INTO gs_mcha WITH KEY matnr = gs_alv-matnr werks = gs_alv-werks charg = gs_alv-charg BINARY SEARCH.
    IF  sy-subrc = 0.
      gs_alv-cuobj_bm = gs_mcha-cuobj_bm.
    ENDIF.


    CLEAR:gs_ausp.

    READ TABLE gt_ausp INTO gs_ausp WITH KEY objek = gs_alv-cuobj_bm BINARY SEARCH.
    IF  sy-subrc = 0.
      gs_alv-atwrt = gs_ausp-atwrt.


    ENDIF.

    CLEAR:gs_a003.
    READ TABLE gt_a003 INTO gs_a003 WITH KEY mwskz = gs_alv-mwskz  BINARY SEARCH .
    IF  sy-subrc = 0.
      gs_alv-kbetr = gs_a003-kbetr.
    ENDIF.
    IF gs_alv-umren NE 0 .


      gs_alv-zdwhsl = gs_alv-umrez / gs_alv-umren.
    ENDIF.

    lv_menge = abs( lv_menge ) .
    gs_alv-zsl = gs_alv-kbetr / 1000.

    gs_alv-zhsdj = ( 1 + gs_alv-zsl ) * gs_alv-dmbtr / gs_alv-menge.

    gs_alv-zsjhj = ( 1 + gs_alv-zsl ) * gs_alv-dmbtr / gs_alv-menge * lv_menge .

*    GS_ALV-ZSHUI = GS_ALV-ZSJHJ - GS_ALV-DMBTR.
    gs_alv-zshui = gs_alv-zsl  * gs_alv-dmbtr / gs_alv-menge * lv_menge .
    gs_alv-menge = lv_menge .
    gs_alv-zsjzt = 'N'.
    IF  gs_alv-zdwhsl = 0  .
      gs_alv-zshsl =   gs_alv-menge.
    ELSE .
      gs_alv-zshsl =   gs_alv-menge  / gs_alv-zdwhsl.
    ENDIF.

    gs_alv-dmbtr = gs_alv-zsjhj - gs_alv-zshui .

    gs_alv-zdzzt = 'N'.
    IF  gs_alv-shkzg =  'H'.
      gs_alv-zsjhj  =  0  -  gs_alv-zsjhj .
      gs_alv-dmbtr  =  0  -  gs_alv-dmbtr .
      gs_alv-zshui  =  0  -  gs_alv-zshui .
      gs_alv-zshsl  =  0  -  gs_alv-zshsl .
      gs_alv-menge  =  0  -  gs_alv-menge .
    ENDIF.

    gs_alv-ernam       =  sy-uname.
    gs_alv-erdat       =  sy-datum.

    READ TABLE gt_dom_apstt INTO DATA(ls_dom_apstt1) WITH KEY domvalue_l = gs_alv-zdzzt.
    IF  sy-subrc = 0.
      gs_alv-zdzztms  = ls_dom_apstt1-ddtext.
    ENDIF.

    APPEND gs_alv TO gt_alv.
    CLEAR:gs_alv.
  ENDLOOP.
************************************************************


  SORT gt_alv BY lifnr ebeln ebelp.

  FREE:lt_lfa1 ,gt_ausp,gt_zmmt0004,gt_a003,gt_mseg.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_CABN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_cabn .

  DATA:BEGIN OF ls_objek,
         objek TYPE ausp-objek,
       END OF ls_objek,
       lt_objek LIKE TABLE OF ls_objek.

  DATA:lv_atinn TYPE cabn-atinn.

  DATA:lt_alv LIKE TABLE OF gs_alv.

  APPEND LINES OF gt_out TO lt_alv.

  SORT lt_alv BY matnr werks charg.
  DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING  matnr werks charg.
  IF  lt_alv IS NOT INITIAL .
    SELECT matnr
           werks
           charg
           cuobj_bm
           FROM mcha
           INTO TABLE gt_mcha
           FOR ALL ENTRIES IN lt_alv
           WHERE matnr   = lt_alv-matnr
           AND   werks   = lt_alv-werks
           AND   charg   = lt_alv-charg .
  ENDIF.


  lt_objek = CORRESPONDING #( gt_mcha MAPPING objek = cuobj_bm ).

  SORT lt_objek BY objek  .

  DELETE ADJACENT DUPLICATES FROM lt_objek  COMPARING objek .


  SELECT SINGLE atinn INTO lv_atinn FROM cabn WHERE atnam = 'ZCHARGL'.


  IF  lt_objek IS NOT INITIAL .

    SELECT objek
           atinn
           atwrt
           FROM ausp
           INTO TABLE gt_ausp
           FOR ALL ENTRIES IN lt_objek
           WHERE objek = lt_objek-objek
           AND   atinn = lv_atinn.
  ENDIF.

  "AUSP
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZSL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_zsl .
**因为税率固定 故 将所有税率取出


  SELECT a003~kschl
         a003~aland
         a003~mwskz
         a003~knumh
         konp~kbetr
         FROM a003
         INNER JOIN konp
         ON  a003~knumh = konp~knumh
         INTO TABLE gt_a003
         WHERE a003~kschl = 'MWVS'
         AND   a003~aland = 'CN'.

  SORT gt_a003 BY mwskz.


ENDFORM.



*&---------------------------------------------------------------------*
*& Form FRM_GET_ZSAVE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_zsave .

  DATA:lv_zdzdh TYPE  zmmt0004-zdzdh,
       lv_zdzxh TYPE  zmmt0004-zdzxh.

  DATA:ls_zmmt0004 TYPE zmmt0004,
       lt_zmmt0004 LIKE TABLE OF ls_zmmt0004.


  DATA:BEGIN OF ls_lifnr,
         lifnr TYPE lfa1-lifnr,
         mwskz TYPE zmmt0004-mwskz,
         hswae TYPE zmmt0004-hswae,
       END OF ls_lifnr,
       lt_lifnr LIKE TABLE OF ls_lifnr.


  DATA:lt_alv LIKE TABLE OF gs_alv.


  APPEND LINES OF gt_alv TO lt_alv.

  DELETE lt_alv WHERE sel = ''  OR zdzdh IS NOT INITIAL .

  lt_lifnr = CORRESPONDING #( lt_alv MAPPING lifnr = lifnr mwskz = mwskz  hswae = hswae ).

  SORT lt_lifnr BY lifnr mwskz hswae.
  DELETE ADJACENT DUPLICATES FROM lt_lifnr COMPARING lifnr mwskz hswae .
****对选择的数据  按照供应商为维度生成对账单号  “新增税率跟货币类型
  LOOP AT  lt_lifnr INTO ls_lifnr.
    CLEAR:lv_zdzxh , lv_zdzdh.
    PERFORM frm_get_snro USING lv_zdzdh.

    LOOP AT  gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE  lifnr = ls_lifnr-lifnr
                                                      AND    mwskz = ls_lifnr-mwskz
                                                      AND   hswae  = ls_lifnr-hswae
                                                      AND    sel = 'X'.
      lv_zdzxh = lv_zdzxh + 1.
      <fs_alv>-zdzdh  = lv_zdzdh.
      <fs_alv>-zdzxh  = lv_zdzxh.
      IF  <fs_alv>-zdzzt IS INITIAL .
        <fs_alv>-zdzzt = 'N'.
      ENDIF.

      READ TABLE gt_dom_apstt INTO DATA(ls_dom_apstt) WITH KEY domvalue_l = <fs_alv>-zdzzt.
      IF  sy-subrc = 0.
        <fs_alv>-zdzztms  = ls_dom_apstt-ddtext.
      ENDIF.
      <fs_alv>-namla = sy-uname.
      <fs_alv>-budla = sy-datum.

*      <FS_ALV>-ZDEL = 'X'.
      MOVE-CORRESPONDING <fs_alv> TO ls_zmmt0004.
      APPEND ls_zmmt0004 TO lt_zmmt0004.
      CLEAR:ls_zmmt0004.
    ENDLOOP.
  ENDLOOP.
  IF  rb_01 = 'X'.
    DELETE  gt_alv  WHERE  sel = ''.
  ENDIF.
  IF  lt_zmmt0004 IS NOT INITIAL .
    MODIFY zmmt0004 FROM TABLE lt_zmmt0004.
    COMMIT WORK AND WAIT .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_SNRO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_ALV_ZDZDH
*&---------------------------------------------------------------------*
FORM frm_get_snro  USING    o_zdzdh.

  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
    EXPORTING
      object           = 'ZMM050'   "Create with SNUM
    EXCEPTIONS
      foreign_lock     = 1
      object_not_found = 2
      system_failure   = 3
      OTHERS           = 4.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZMM050'
    IMPORTING
      number                  = o_zdzdh
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      internal_overflow       = 6
      OTHERS                  = 7.

  CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
    EXPORTING
      object = 'ZMM050'.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZMOD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_zmod .

  READ TABLE gt_alv INTO gs_alv WITH KEY sel = 'X' zsjzt = 'Y'.
  IF  sy-subrc = 0.
    gv_tabix1 = sy-tabix .
    CALL  SCREEN 200 STARTING AT 5 5 ENDING AT  55 23.
  ELSE.
    MESSAGE '您选择的数据不能修改！' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form FRM_GET_ZDEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_zdel .

  DATA:ls_zmmt0004 TYPE zmmt0004,
       lt_zmmt0004 LIKE TABLE OF ls_zmmt0004.

*  LOOP AT  GT_ALV INTO GS_ALV WHERE ( SEL = 'X' AND ZSJZT = 'Y'  AND ZDZZT NE 'J') OR ( SEL = 'X' AND ZSJZT = 'N' AND ZDZZT NE 'J' ).
*  LOOP AT  GT_ALV INTO GS_ALV WHERE  SEL = 'X' AND ZSJZT <> 'Y'  AND ZDZZT NE 'J'.
  LOOP AT  gt_alv INTO gs_alv WHERE  sel = 'X'   AND zdzzt NE 'J'.
*J  已发票校验
*C  取消对账
*N  暂存
*Y  已对账
*Q  取消发票校验
*Z  作废
    IF gs_alv-zdzzt = 'J'
    OR gs_alv-zdzzt = 'Y'
    OR gs_alv-zdzzt = 'Q'  .

    ELSE .

      gs_alv-zdel = 'X'.
      MODIFY gt_alv FROM gs_alv.

      MOVE-CORRESPONDING gs_alv TO ls_zmmt0004.
      APPEND ls_zmmt0004 TO lt_zmmt0004.
      CLEAR:ls_zmmt0004.
    ENDIF .
  ENDLOOP.


  DELETE gt_alv WHERE zdel = 'X'.

**如果是修改 需要对采购对账表对应的数据进行删除
  IF  lt_zmmt0004 IS INITIAL.
    MESSAGE '请选择正确需要删除的数据！'  TYPE  'S' DISPLAY LIKE 'E'.
  ENDIF.


  IF  rb_02 = 'X' AND lt_zmmt0004 IS NOT INITIAL .
    DELETE zmmt0004 FROM TABLE lt_zmmt0004.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZQRDZ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_zqrdz .

  DATA:ls_zmmt0004 TYPE zmmt0004,
       lt_zmmt0004 LIKE TABLE OF ls_zmmt0004.

  LOOP AT  gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv1>) WHERE  sel = 'X' AND zdzzt = 'N'.
    <fs_alv1>-zdzzt = 'Y'.
    READ TABLE gt_dom_apstt INTO DATA(ls_dom_apstt) WITH KEY domvalue_l = <fs_alv1>-zdzzt.
    IF  sy-subrc = 0.
      <fs_alv1>-zdzztms  = ls_dom_apstt-ddtext.
    ENDIF.

    <fs_alv1>-namla = sy-uname.
    <fs_alv1>-budla = sy-datum.

    MOVE-CORRESPONDING <fs_alv1> TO ls_zmmt0004.
    APPEND ls_zmmt0004 TO lt_zmmt0004..
    CLEAR:ls_zmmt0004.
  ENDLOOP.

  IF  lt_zmmt0004 IS NOT INITIAL .
    MODIFY zmmt0004 FROM TABLE lt_zmmt0004.
    IF  sy-subrc = 0.
      MESSAGE '确认成功！'  TYPE 'S' .
    ENDIF.
  ELSE.
    MESSAGE '只有暂存的数据才能对账！'  TYPE 'S' .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZCXDZ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_zcxdz .
  DATA:ls_zmmt0004 TYPE zmmt0004,
       lt_zmmt0004 LIKE TABLE OF ls_zmmt0004.

  DATA : lv_marks(1) TYPE c .
  DATA : lv_mess(220) TYPE c .
  DATA : ls_alv TYPE ty_tab .

  CLEAR ls_alv .
  CLEAR lv_marks .
*  LOOP AT GT_ALV INTO LS_ALV WHERE  SEL = 'X' AND ZDZZT = 'Y'
*                                 AND ( BLINE_DATE IS INITIAL  OR ZDZWB IS INITIAL ) .
*
*    CONCATENATE '对账单：'  LS_ALV-ZDZDH '起算日期和抬头文本必填' INTO LV_MESS .
*    MESSAGE LV_MESS TYPE 'S' DISPLAY LIKE 'E' .
*    EXIT .
*  ENDLOOP .

  CHECK lv_marks IS INITIAL .

  LOOP AT  gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv1>) WHERE  sel = 'X' AND zdzzt <> 'Y' AND zdzdh <> ''.
    <fs_alv1>-zdzzt = 'Z'.
    <fs_alv1>-zdzztms = '作废'.
    <fs_alv1>-zernam_d  = sy-uname.
    <fs_alv1>-zerdat_d  = sy-datum.

    <fs_alv1>-namla = sy-uname.
    <fs_alv1>-budla = sy-datum.

    MOVE-CORRESPONDING <fs_alv1> TO ls_zmmt0004.
    APPEND ls_zmmt0004 TO lt_zmmt0004.
    CLEAR:ls_zmmt0004.
  ENDLOOP.

  IF  lt_zmmt0004 IS NOT INITIAL .
    MODIFY zmmt0004 FROM TABLE lt_zmmt0004.
    IF  sy-subrc = 0.
      MESSAGE '变更成功！'  TYPE 'S' .
    ENDIF.
  ELSE.
    MESSAGE '只有已保存的数据才能对账！'  TYPE 'S' .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZQXDZ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_zqxdz .
  DATA:ls_zmmt0004 TYPE zmmt0004,
       lt_zmmt0004 LIKE TABLE OF ls_zmmt0004.

  LOOP AT  gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv1>) WHERE ( sel = 'X' AND  zdzzt = 'Y' ) OR ( sel = 'X' AND  zdzzt = 'N' ) .
    <fs_alv1>-zdzzt = 'Z'.
    <fs_alv1>-zernam_z  = sy-uname.
    <fs_alv1>-zerdat_z  = sy-datum.
    MOVE-CORRESPONDING <fs_alv1> TO ls_zmmt0004.
    APPEND ls_zmmt0004 TO lt_zmmt0004..
    CLEAR:ls_zmmt0004.
  ENDLOOP.

  IF  lt_zmmt0004 IS NOT INITIAL .
    MODIFY zmmt0004 FROM TABLE lt_zmmt0004.
    IF  sy-subrc = 0.
      MESSAGE '对账作废成功！'  TYPE 'S' .
    ENDIF.
  ELSE.
    MESSAGE '请选择需要对账作废的数据！'  TYPE 'S' .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA3
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_data3 .
  DATA:lt_alv TYPE TABLE OF ty_tab.
  DATA : ls_t052 TYPE t052 .

  SELECT   zmmt0004~zdzdh
           zmmt0004~zdzxh
           zmmt0004~zdzzt
           zmmt0004~ebeln
           zmmt0004~ebelp
           zmmt0004~gjahr
           zmmt0004~belnr
           zmmt0004~buzei
           zmmt0004~lifnr
           zmmt0004~name1
           zmmt0004~werks
           zmmt0004~matnr
           zmmt0004~maktx
           zmmt0004~atwrt
           zmmt0004~charg
           zmmt0004~bewtp
           zmmt0004~bwart
           zmmt0004~shkzg
           zmmt0004~budat
           zmmt0004~menge
           zmmt0004~meins
           zmmt0004~zdwhsl
           zmmt0004~zshsl
           zmmt0004~hswae
           zmmt0004~mwskz
           zmmt0004~dmbtr
           zmmt0004~zsl
           zmmt0004~zhsdj
           zmmt0004~zsjhj
           zmmt0004~zsjzt
           zmmt0004~ernam
           zmmt0004~erdat
           zmmt0004~zernam_d
           zmmt0004~zerdat_d
           zmmt0004~zernam_z
           zmmt0004~zerdat_z
           zmmt0004~lfgja
           zmmt0004~lfbnr
           zmmt0004~lfpos
           zmmt0004~pmnttrms
           zmmt0004~bline_date
           zmmt0004~zbudat_q
           zmmt0004~zdzwb
           zmmt0004~nampo
              zmmt0004~zfkts
           zmmt0004~zfkrq
           zmmt0004~zshui
           zmmt0004~zcanz
           ekko~ekorg
     FROM zmmt0004
     INNER JOIN ekpo
     ON zmmt0004~ebeln = ekpo~ebeln
     AND zmmt0004~ebelp = ekpo~ebelp
     INNER JOIN ekko
     ON zmmt0004~ebeln = ekko~ebeln
     INTO CORRESPONDING FIELDS OF TABLE gt_alv
     WHERE zmmt0004~lifnr IN   s_lifnr
*     AND   zmmt0004~erdat IN   s_budat“2021.03.22修改，自定义表筛选的是前台的创建日期，而不是过账日期Seashell Huang
     AND   zmmt0004~erdat IN   s_erdat
     AND   zmmt0004~zdzzt IN   s_zdzzt
     AND   zmmt0004~ebeln IN   s_ebeln
     AND   zmmt0004~werks IN   s_werks
     AND   zmmt0004~namla IN   s_namla
     AND   zmmt0004~charg IN   s_charg
     AND   zmmt0004~waers IN   s_waers
     AND   zmmt0004~matnr IN   s_matnr
     AND   zmmt0004~zdzdh IN   s_zdzdh
     AND   zmmt0004~zdzzt NOT IN (  'J', 'Z' )
     AND   zmmt0004~nampo IN s_ernam
     AND   ekpo~pstyp IN   s_pstyp
     AND EXISTS ( SELECT * FROM ekko WHERE ebeln = zmmt0004~ebeln AND bsart IN s_bsart ).
  IF  gt_alv IS INITIAL .
    MESSAGE '您的查询数据无有效数据！'  TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE  LIST-PROCESSING AND RETURN TO SCREEN 0.
  ENDIF.

  "一个对账单可能存在不同的批次和不同的最后修改人 因此重新进行获取对账单
  IF s_charg[] IS NOT INITIAL
  OR s_namla[] IS NOT INITIAL .
    PERFORM frm_re_get_zmmt0004 TABLES gt_alv ."
  ENDIF .

  APPEND LINES OF gt_alv TO gt_alv1.


  SORT gt_alv BY zdzdh lifnr.

  DELETE ADJACENT DUPLICATES FROM gt_alv COMPARING zdzdh lifnr.


  APPEND LINES OF gt_alv TO lt_alv.
  SORT lt_alv BY lifnr ekorg.
  DELETE   ADJACENT DUPLICATES FROM lt_alv COMPARING lifnr ekorg.

  IF  lt_alv IS NOT INITIAL .
    SELECT lifnr,
           ekorg,
           zterm
           FROM lfm1
           INTO TABLE @DATA(lt_lfm1)
           FOR ALL ENTRIES IN @lt_alv
           WHERE lifnr = @lt_alv-lifnr
           AND   ekorg = @lt_alv-ekorg.

  ENDIF.

  LOOP AT  gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv_4>).
    IF  <fs_alv_4>-pmnttrms IS INITIAL .
      READ TABLE lt_lfm1 INTO DATA(ls_lfm1) WITH KEY  lifnr = <fs_alv_4>-lifnr ekorg = <fs_alv_4>-ekorg.
      IF  sy-subrc = 0.
        <fs_alv_4>-pmnttrms = ls_lfm1-zterm.
      ELSE.
        <fs_alv_4>-message = '供应商组数据没有维护付款方式'.
      ENDIF.
    ENDIF.

    IF  <fs_alv_4>-zbudat_q IS INITIAL .
      <fs_alv_4>-zbudat_q    = sy-datum.
    ENDIF.

    CLEAR <fs_alv_4>-zshui .
    CLEAR <fs_alv_4>-dmbtr .
    CLEAR <fs_alv_4>-zsjhj .
    CLEAR <fs_alv_4>-zshsl .
    CLEAR gs_alv1 .
    LOOP AT gt_alv1 INTO gs_alv1 WHERE zdzdh = <fs_alv_4>-zdzdh .
      <fs_alv_4>-zshui = <fs_alv_4>-zshui  +   gs_alv1-zshui  .
      <fs_alv_4>-dmbtr = <fs_alv_4>-dmbtr  +   gs_alv1-dmbtr  .
      <fs_alv_4>-zsjhj = <fs_alv_4>-zsjhj  +   gs_alv1-zsjhj  .
      <fs_alv_4>-zshsl = <fs_alv_4>-zshsl  +   gs_alv1-zshsl  .
    ENDLOOP .

*    IF <FS_ALV_4>-BLINE_DATE IS INITIAL  .
*      <FS_ALV_4>-BLINE_DATE  = SY-DATUM.
*    ENDIF.

    IF <fs_alv_4>-pmnttrms <> '' .

      CLEAR ls_t052 .
      CALL FUNCTION 'FI_FIND_PAYMENT_CONDITIONS'
        EXPORTING
          i_zterm            = <fs_alv_4>-pmnttrms
          i_bldat            = <fs_alv_4>-zbudat_q
          i_budat            = <fs_alv_4>-zbudat_q
        IMPORTING
          e_t052             = ls_t052
        EXCEPTIONS
          terms_incorrect    = 1
          terms_not_found    = 2
          no_date_entered    = 3
          no_day_limit_found = 4
          OTHERS             = 5.

      <fs_alv_4>-zfkts  = ls_t052-ztag1 .

    ENDIF .

    IF <fs_alv_4>-bline_date IS NOT INITIAL  .
      <fs_alv_4>-zfkrq = <fs_alv_4>-bline_date + <fs_alv_4>-zfkts .
    ENDIF .

    READ TABLE gt_dom_apstt INTO DATA(ls_dom_apstt) WITH KEY domvalue_l = <fs_alv_4>-zdzzt.
    IF  sy-subrc = 0.
      <fs_alv_4>-zdzztms  = ls_dom_apstt-ddtext.
    ENDIF.

  ENDLOOP.

  FREE:lt_alv.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZQRDZ1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_zqrdz1 .

  DATA:ls_zmmt0004 TYPE zmmt0004,
       lt_zmmt0004 LIKE TABLE OF ls_zmmt0004.

  DATA:BEGIN OF ls_zdzdh,
         zdzdh      TYPE zmmt0004-zdzdh,
         pmnttrms   TYPE zmmt0004-pmnttrms,
         bline_date TYPE zmmt0004-bline_date,
         zdzwb      TYPE zmmt0004-zdzwb,
         zbudat_q   TYPE zmmt0004-zbudat_q  , "过账期间
       END OF ls_zdzdh,
       lt_zdzdh LIKE TABLE OF ls_zdzdh.

  DATA : lv_marks(1) TYPE c .
  DATA : lv_mess(220) TYPE c .
  DATA : ls_alv TYPE ty_tab .

  CLEAR ls_alv .
  CLEAR lv_marks .
  LOOP AT gt_alv INTO ls_alv WHERE  sel = 'X' AND ( zdzzt = 'N' OR  zdzzt = 'Q' ) " OR  ZDZZT = 'C' )
                                 AND ( bline_date IS INITIAL  OR zdzwb IS INITIAL ) .

    lv_marks  = 'X' .
    CONCATENATE '对账单：'  ls_alv-zdzdh '起算日期和抬头文本必填' INTO lv_mess .
    MESSAGE lv_mess TYPE 'S' DISPLAY LIKE 'E' .
    EXIT .

  ENDLOOP .

  CHECK lv_marks IS INITIAL .
  READ TABLE gt_alv INTO ls_alv WITH KEY  sel = 'X'
                                         zdzzt = 'C'  .
  IF sy-subrc = 0 .
    lv_marks  = 'X' .
*    CONCATENATE '对账单：'  LS_ALV-ZDZDH '起算日期和抬头文本必填' INTO LV_MESS .
    lv_mess = '勾选的状态中存在取消对账的数据，请检查' .
    MESSAGE lv_mess TYPE 'S' DISPLAY LIKE 'E' .
  ENDIF .

  CHECK lv_marks IS INITIAL .

  LOOP AT  gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv1>) WHERE  sel = 'X' AND ( zdzzt = 'N' OR  zdzzt = 'Q' OR  zdzzt = 'C' ).
    <fs_alv1>-zdzzt = 'Y'.
    "   MOVE-CORRESPONDING <FS_ALV1> TO LS_ZMMT0004.
    READ TABLE gt_dom_apstt INTO DATA(ls_dom_apstt) WITH KEY domvalue_l = <fs_alv1>-zdzzt.
    IF  sy-subrc = 0.
      <fs_alv1>-zdzztms  = ls_dom_apstt-ddtext.
    ENDIF.


    <fs_alv1>-zernam_d  = sy-uname.
    <fs_alv1>-zerdat_d  = sy-datum.
    ls_zdzdh-zdzdh      = <fs_alv1>-zdzdh.
    ls_zdzdh-pmnttrms   = <fs_alv1>-pmnttrms  .
    ls_zdzdh-bline_date = <fs_alv1>-bline_date.
    ls_zdzdh-zdzwb      = <fs_alv1>-zdzwb     .
    ls_zdzdh-zbudat_q   = <fs_alv1>-zbudat_q  .
    APPEND ls_zdzdh TO lt_zdzdh.
  ENDLOOP.

  IF  lt_zdzdh IS NOT INITIAL .
    SELECT * FROM zmmt0004 INTO TABLE lt_zmmt0004
             FOR ALL ENTRIES IN lt_zdzdh
             WHERE zdzdh = lt_zdzdh-zdzdh.
  ENDIF.

  IF  lt_zmmt0004 IS NOT INITIAL .
    LOOP AT  lt_zmmt0004  INTO ls_zmmt0004 .
      ls_zmmt0004-zdzzt = 'Y'.
      ls_zmmt0004-zernam_d = sy-uname.
      ls_zmmt0004-zerdat_d = sy-datum.
      ls_zmmt0004-namla = sy-uname.
      ls_zmmt0004-budla = sy-datum.

      CLEAR:ls_zdzdh.
      READ TABLE lt_zdzdh INTO ls_zdzdh WITH KEY zdzdh = ls_zmmt0004-zdzdh.
      IF  sy-subrc = 0.
        ls_zmmt0004-pmnttrms   = ls_zdzdh-pmnttrms.
        ls_zmmt0004-bline_date = ls_zdzdh-bline_date.
        ls_zmmt0004-zdzwb      = ls_zdzdh-zdzwb.
        ls_zmmt0004-zbudat_q   = ls_zdzdh-zbudat_q.
      ENDIF.
      MODIFY lt_zmmt0004 FROM ls_zmmt0004.
    ENDLOOP.

    MODIFY zmmt0004 FROM TABLE lt_zmmt0004.
    IF  sy-subrc = 0.
      MESSAGE '确认成功！'  TYPE 'S' .
    ENDIF.
  ELSE.
    MESSAGE '只有暂存的数据才能对账！'  TYPE 'S' .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZQXDZ1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_zqxdz1 .
  DATA:ls_zmmt0004 TYPE zmmt0004,
       lt_zmmt0004 LIKE TABLE OF ls_zmmt0004.

  DATA:BEGIN OF ls_zdzdh,
         zdzdh TYPE zmmt0004-zdzdh,
       END OF ls_zdzdh,
       lt_zdzdh LIKE TABLE OF ls_zdzdh.

  LOOP AT  gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv1>) WHERE  sel = 'X' AND ( zdzzt = 'Y' OR  zdzzt = 'Q'  ).
*    <FS_ALV1>-ZDZZT = 'C'.
    <fs_alv1>-zdzzt = 'N'."取消对账改为N状态，以便后续的更改 20210130
    <fs_alv1>-message = '取消对账成功'.
    READ TABLE gt_dom_apstt INTO DATA(ls_dom_apstt) WITH KEY domvalue_l = <fs_alv1>-zdzzt.
    IF  sy-subrc = 0.
      <fs_alv1>-zdzztms  = ls_dom_apstt-ddtext.
    ENDIF.
    <fs_alv1>-zernam_z = sy-uname.
    <fs_alv1>-zerdat_z = sy-datum.
    ls_zdzdh-zdzdh = <fs_alv1>-zdzdh.
    APPEND ls_zdzdh TO lt_zdzdh.
  ENDLOOP.

  IF  lt_zdzdh IS NOT INITIAL .
    SELECT * FROM zmmt0004 INTO TABLE lt_zmmt0004
             FOR ALL ENTRIES IN lt_zdzdh
             WHERE zdzdh = lt_zdzdh-zdzdh.
  ENDIF.

  IF  lt_zmmt0004 IS NOT INITIAL .
    LOOP AT  lt_zmmt0004  INTO ls_zmmt0004 .
*      LS_ZMMT0004-ZDZZT = 'C'.
      ls_zmmt0004-zdzzt = 'N'."取消对账改为N状态，以便后续的更改 20210130
      ls_zmmt0004-zernam_d = sy-uname.
      ls_zmmt0004-zerdat_d = sy-datum.
      ls_zmmt0004-namla = sy-uname.
      ls_zmmt0004-budla = sy-datum.
      MODIFY lt_zmmt0004 FROM ls_zmmt0004.
    ENDLOOP.

    MODIFY zmmt0004 FROM TABLE lt_zmmt0004.
    IF  sy-subrc = 0.
      MESSAGE '取消对账成功！'  TYPE 'S' .
    ENDIF.
  ELSE.
    MESSAGE '只有已对账的数据才能作废！'  TYPE 'S' .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZSAVE4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_zsave4 .

  DATA:ls_zmmt0004 TYPE zmmt0004,
       lt_zmmt0004 LIKE TABLE OF ls_zmmt0004.

  DATA:BEGIN OF ls_zdzdh,
         zdzdh      TYPE zmmt0004-zdzdh,
         pmnttrms   TYPE zmmt0004-pmnttrms,
         bline_date TYPE zmmt0004-bline_date,
         zdzwb      TYPE zmmt0004-zdzwb,
         zbudat_q   TYPE zmmt0004-zbudat_q  , "过账期间
       END OF ls_zdzdh,
       lt_zdzdh LIKE TABLE OF ls_zdzdh.

  LOOP AT  gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv1>) WHERE  sel = 'X' .
    "   <FS_ALV1>-ZDZZT = 'Y'.
*    <FS_ALV1>-ZERNAM_D = SY-UNAME.
*    <FS_ALV1>-ZERDAT_D = SY-DATUM.
    ls_zdzdh-zdzdh       = <fs_alv1>-zdzdh.
    ls_zdzdh-pmnttrms    = <fs_alv1>-pmnttrms  .
    ls_zdzdh-bline_date  = <fs_alv1>-bline_date .
    ls_zdzdh-zdzwb       = <fs_alv1>-zdzwb      .
    ls_zdzdh-zbudat_q    = <fs_alv1>-zbudat_q   .
*          GS_ALV-NAMLA = SY-UNAME.
*          GS_ALV-BUDLA = SY-DATUM.
    APPEND ls_zdzdh TO lt_zdzdh.
    CLEAR:ls_zdzdh.
  ENDLOOP.

  IF  lt_zdzdh IS NOT INITIAL .
    SELECT * FROM zmmt0004 INTO TABLE lt_zmmt0004
             FOR ALL ENTRIES IN lt_zdzdh
             WHERE zdzdh = lt_zdzdh-zdzdh.
  ENDIF.

  IF  lt_zmmt0004 IS NOT INITIAL .
    LOOP AT  lt_zmmt0004  INTO ls_zmmt0004 .
      CLEAR:ls_zdzdh.
      READ TABLE lt_zdzdh INTO ls_zdzdh WITH KEY zdzdh = ls_zmmt0004-zdzdh .
      IF sy-subrc = 0 .
        ls_zmmt0004-pmnttrms   = ls_zdzdh-pmnttrms.
        ls_zmmt0004-bline_date = ls_zdzdh-bline_date.
        ls_zmmt0004-zdzwb      = ls_zdzdh-zdzwb.
        ls_zmmt0004-zbudat_q   = ls_zdzdh-zbudat_q.
        ls_zmmt0004-namla = sy-uname.
        ls_zmmt0004-budla = sy-datum.
      ENDIF.
      MODIFY lt_zmmt0004 FROM ls_zmmt0004.
    ENDLOOP.

    MODIFY zmmt0004 FROM TABLE lt_zmmt0004.
    IF  sy-subrc = 0.
      MESSAGE '保存成功！'  TYPE 'S' .
    ENDIF.
  ELSE.
    MESSAGE '请选择需要保存的数据！'  TYPE 'S' .
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_screen .

  LOOP AT SCREEN.
    CASE  screen-group1 .
      WHEN 'Z01'.
        CASE 'X'.
          WHEN rb_01.
            screen-active = '0'.
          WHEN OTHERS.
            screen-active = '1'.
        ENDCASE.
        "Seashell Huang 新增创建日期选择屏幕，创建的时候显示过账日期字段
      WHEN 'Z02'.
        CASE 'X'.
          WHEN rb_01.
            screen-active = '0'.
          WHEN OTHERS.
            screen-active = '1'.
        ENDCASE.
      WHEN 'Z03'.
        CASE 'X'.
          WHEN rb_01.
            screen-active = '1'.
          WHEN OTHERS.
            screen-active = '0'.
        ENDCASE.
        "END
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ZDZZTMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_zdzztms .

**获取允差描述
  DATA: lv_return    TYPE sy-subrc,
        lv_wa_domtab TYPE dd07v.

  IF gt_dom_apstt IS INITIAL.
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZMMO_DZZT'   "域名
        text           = 'X'
        langu          = sy-langu
      IMPORTING
        rc             = lv_return
      TABLES
        dd07v_tab      = gt_dom_apstt
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_item .
  CLEAR:gt_alv_item.

  APPEND LINES OF gt_alv1 TO gt_alv_item.

  DELETE gt_alv_item WHERE zdzdh NE gs_alv-zdzdh.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SHOW_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_show_item .
  CLEAR:gt_fieldcat1.
  fill_field_list1  'ZDZDH'  '对账单号'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'ZDZXH'  '对账单行项目'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'ZDZZT'  '对账状态'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'ZSJZT'  '数据状态'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'EBELN'  '采购凭证'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'EBELP'  '项目'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'BELNR'  '物料凭证'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'BUZEI'  '物料凭证项目'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'LIFNR'  '供应商编号'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'NAME1'  '供应商'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'WERKS'  '工厂'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'MATNR'  '物料编码'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'MAKTX'  '物料描述'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'ATWRT'  '长批号'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'CHARG'  'SAP批号'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'BEWTP'  '采购订单历史记录类别'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'BWART'  '移动类型'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'SHKZG'  '借/贷标识'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'BUDAT'  '过账日期'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'MENGE'  '收货数量'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'MEINS'  '单位'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'ZDWHSL' '单位换算率'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'ZSHSL'  '收货数量(采购单位)'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'HSWAE'  '币别'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'MWSKZ'  '税码'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'DMBTR'  '未含税金额'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'ZSL'    '税率'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'ZHSDJ'  '含税单价'  '' ''  ''  ''   '' '' '' .
  fill_field_list1  'ZSJHJ'  '税价合计'  '' ''  ''  ''   '' '' '' .


  DATA lv_layout_lvc TYPE  lvc_s_layo.
  lv_layout_lvc-cwidth_opt   = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
      is_layout_lvc      = lv_layout_lvc
      it_fieldcat_lvc    = gt_fieldcat1
      i_default          = 'X'
      i_save             = 'U'
    TABLES
      t_outtab           = gt_alv_item.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CALL_LONG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_call_long .
  CALL   SCREEN 2000 STARTING AT 20 10
                       ENDING AT 60 20 .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_RSEG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_rseg .


  SELECT
  rseg~belnr
  rseg~gjahr
  rseg~buzei
  rseg~shkzg
  rseg~menge
  rseg~bstme
  rseg~bpmng
  rseg~bprme
  rseg~lfgja
  rseg~lfbnr
  rseg~lfpos
  INTO TABLE gt_rseg
  FROM rseg
  INNER JOIN rbkp
  ON rbkp~belnr =  rseg~belnr
  AND rbkp~gjahr =  rseg~gjahr
  FOR ALL ENTRIES IN gt_out
  WHERE rseg~lfgja = gt_out-gjahr
    AND rseg~lfbnr = gt_out-belnr
    AND rseg~lfpos = gt_out-buzei
    AND rseg~tbtkz = ''
    AND rbkp~stblg = '' .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_RE_GET_ZMMT004
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_re_get_zmmt0004  TABLES t_out STRUCTURE gs_alv .

  DATA : lt_out TYPE TABLE OF ty_tab .

  SORT t_out BY zdzdh .
  DELETE ADJACENT DUPLICATES FROM t_out
                COMPARING zdzdh .

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_out
    FROM zmmt0004
    FOR ALL ENTRIES IN t_out
    WHERE zdzdh = t_out-zdzdh .

  t_out[] = lt_out[] .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_alv .

  IF g_container IS INITIAL.
    PERFORM init_alv.
    CREATE OBJECT g_container
      EXPORTING
        container_name = 'ALV1'
        repid          = sy-repid
        dynnr          = '0100'
*       SIDE           = G_CONTAINER->DOCK_AT_LEFT
**       side      = cl_gui_docking_container=>dock_at_right
*       EXTENSION      = 1500.
      .

    CREATE OBJECT g_gui_alv " 功能A: 基本显示
      EXPORTING
        i_parent          = g_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    DATA: lt_f4 TYPE lvc_t_f4 WITH HEADER LINE.
    DATA: lt_f4_data TYPE lvc_s_f4.
    CLEAR: lt_f4_data.
    lt_f4_data-fieldname = 'PMNTTRMS'.
    lt_f4_data-register = 'X' .
    lt_f4_data-getbefore = 'X' .
    lt_f4_data-chngeafter  ='X'.
    INSERT lt_f4_data INTO TABLE lt_f4.

    CALL METHOD g_gui_alv->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    CREATE OBJECT lcl_event.
    SET HANDLER lcl_event->handle_toolbar FOR g_gui_alv.
    SET HANDLER lcl_event->handle_command FOR g_gui_alv.
    SET HANDLER lcl_event->handle_onf4    FOR g_gui_alv.
    SET HANDLER lcl_event->handle_double_click FOR g_gui_alv.
    SET HANDLER lcl_event->handle_data_changed        FOR g_gui_alv.
    CALL METHOD g_gui_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter. "回车事件

    CALL METHOD g_gui_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    PERFORM frm_prepare_layout CHANGING gs_layout.
    g_variant1-report = sy-repid.

    CASE 'X' .
      WHEN rb_01 .
        g_variant1-variant = '/布局1' .
      WHEN rb_02 .
        g_variant1-variant = '/布局2' .
      WHEN rb_03 .
        g_variant1-variant = '/布局3' .
      WHEN rb_04 .
        g_variant1-variant = '/布局4' .
    ENDCASE .

    CALL METHOD g_gui_alv->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = g_variant1
        i_save               = 'A'
        it_toolbar_excluding = gt_exclude " 功能G: 设定屏蔽标准ALV 工具栏的按钮
      CHANGING
        it_sort              = i_sort[]
        it_fieldcatalog      = gt_fieldcat[]
        it_outtab            = gt_alv.
*    CALL METHOD g_gui_alv->set_ready_for_input
*      EXPORTING
*        i_ready_for_input = 1.
*    CALL METHOD g_gui_alv->set_toolbar_interactive.

*    PERFORM FRM_REFRESH_ALV USING G_GUI_ALV GS_LAYOUT.
  ELSE.
    PERFORM frm_refresh_alv USING g_gui_alv gs_layout.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_FREE_CREAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_free_creat .


  CALL METHOD g_container->free .
**   FREE  G_CONTAINER .
*    FREE  G_GUI_ALV .
  FREE  g_container .
  PERFORM frm_set_alv .


ENDFORM.