*&---------------------------------------------------------------------*
*& 包含               ZMMR0065_F01.
*&---------------------------------------------------------------------*

FORM frm_get_data .

  " 定义所需的结构体

  DATA: lt_marc     TYPE TABLE OF marc,
        ls_mrac     TYPE marc,

        lt_mara     TYPE TABLE OF mara,
        ls_mara     TYPE mara,

        lt_makt     TYPE TABLE OF makt,
        ls_makt     TYPE makt,

        lt_mapl     TYPE TABLE OF mapl,
        ls_mapl     TYPE mapl,

        lt_ekpo     TYPE TABLE OF ekpo,
        lt_ekpo_tmp TYPE TABLE OF ekpo,
        ls_ekpo     TYPE ekpo,

        lt_ekko     TYPE TABLE OF ekko,
        ls_ekko     TYPE ekko,

        lt_ekbe	    TYPE TABLE OF ekbe,
        lt_ekbe2    TYPE TABLE OF ekbe,
        ls_ekbe     TYPE ekbe,

        lt_mard	    TYPE TABLE OF mard,
        ls_mard     TYPE mard,

        lt_mbew	    TYPE TABLE OF mbew,
        ls_mbew     TYPE mbew,

        lt_afko	    TYPE TABLE OF afko,
        ls_afko     TYPE afko,

        lt_aufk	    TYPE TABLE OF aufk,
        ls_aufk     TYPE aufk,

        lt_rkpf	    TYPE TABLE OF rkpf,
        ls_rkpf     TYPE rkpf,


        lt_mseg     TYPE TABLE OF mseg,
        ls_mseg     TYPE mseg.

  DATA: lv_name TYPE thead-tdname,
        lv_id   TYPE string.


  TYPES:BEGIN OF lv_afko1,
          idat2 TYPE aufk-idat2,
          werks TYPE aufk-werks.
          INCLUDE TYPE afko.
        TYPES END OF lv_afko1.

  DATA: lt_afko1 TYPE TABLE OF lv_afko1,
        ls_afko1 TYPE lv_afko1.

  TYPES:BEGIN OF lv_mseg1,
          budat TYPE mkpf-budat.
          INCLUDE TYPE mseg.
        TYPES END OF lv_mseg1.

  DATA: lt_mseg1 TYPE TABLE OF lv_mseg1,
        ls_mseg1 TYPE lv_mseg1.

  "物料特征字段结构定义
  DATA: ltoutput   TYPE TABLE OF zmaterial_tx_value.
  DATA: lt_tline TYPE TABLE OF tline.

  DATA: num1 TYPE i.
  DATA: num2 TYPE i.
  DATA: num3 TYPE i.
  DATA: num4 TYPE i.

  DATA:lt_ekpo1 LIKE TABLE OF lt_ekpo,
       ls_ekpo1 LIKE LINE OF lt_ekpo1.
  TYPES:BEGIN OF ty_objek,
          matnr TYPE ausp-objek,
        END OF ty_objek.
  DATA:lt_objek TYPE TABLE OF ty_objek,
       lv_atinn TYPE ausp-atinn.


  " 取出 报表所需系统表的字段

  SELECT
c~werks
c~matnr
a~mtart
a~lvorm
c~lvorm
c~mmsta
c~mmstd
a~ersda

  FROM marc AS c INNER JOIN mara AS a ON a~matnr = c~matnr
      INTO CORRESPONDING FIELDS OF TABLE lt_alv
      WHERE c~werks IN s_werks
      AND c~matnr IN s_matnr
      AND a~mtart IN s_mtart
      AND a~lvorm IN s_lvorm1
      AND c~lvorm IN s_lvorm2
    AND c~mmsta IN s_mmsta
    AND c~mmstd IN s_mmstd.


  IF lt_alv IS NOT INITIAL.

    "取出MAKT物料描述表所需的字段

    SELECT matnr
      maktx
        INTO CORRESPONDING FIELDS OF TABLE lt_makt
        FROM makt
        FOR ALL ENTRIES IN lt_alv
        WHERE matnr = lt_alv-matnr.

    "取出采购订单数据所需的字段
    SELECT ekpo~ebeln ,ekpo~ebelp ,ekpo~elikz, ekko~aedat, ekpo~werks, ekpo~matnr
       FROM ekpo  INNER JOIN ekko ON ekko~ebeln = ekpo~ebeln
         INTO CORRESPONDING FIELDS OF TABLE @lt_ekpo
      FOR ALL ENTRIES IN @lt_alv
           WHERE ekpo~matnr = @lt_alv-matnr
           AND ekpo~werks = @lt_alv-werks
           AND ekpo~loekz = ''
           AND ekko~aedat IN @s_cxrq
           AND ekpo~elikz <> 'X'.

    "采购订单未收货完成的数量
    SELECT
      matnr,
      werks,
      ebeln,
      COUNT(*) AS menge
    FROM ekpo
    WHERE ekpo~elikz <> 'X'
      AND ekpo~werks IN @s_werks
      AND ekpo~matnr IN @s_matnr
      AND ekpo~loekz = ''
      AND ekpo~aedat IN @s_cxrq
    GROUP BY matnr,werks,ebeln
    INTO TABLE @DATA(lt_ekpo2).

    LOOP AT lt_ekpo2 INTO DATA(ls_ekpo2).
      MOVE-CORRESPONDING ls_ekpo2 TO ls_po_un.
      COLLECT ls_po_un INTO lt_po_un.
    ENDLOOP.



    SELECT ekbe~ebeln ekbe~ebelp ekbe~vgabe ekbe~shkzg ekbe~matnr ekbe~werks SUM( ekbe~menge ) AS menge
    INTO CORRESPONDING FIELDS OF TABLE lt_ekbe
    FROM ekbe
    INNER JOIN ekpo
    ON ekpo~ebeln = ekbe~ebeln
    AND ekpo~ebelp = ekbe~ebelp
    WHERE vgabe IN ('1','2')
      AND shkzg IN ('S','H')
      AND ekbe~werks IN s_werks
      AND ekbe~matnr IN s_matnr
      AND ekpo~aedat IN s_cxrq
    GROUP BY ekbe~ebeln ekbe~ebelp ekbe~vgabe ekbe~shkzg ekbe~matnr ekbe~werks.



*    IF lt_ekpo IS NOT INITIAL.
*
**      SELECT ekbe~ebeln ekbe~ebelp ekbe~vgabe ekbe~shkzg ekbe~menge
**         INTO CORRESPONDING FIELDS OF TABLE lt_ekbe2
**        FROM ekbe
**           FOR ALL ENTRIES IN lt_ekpo
**           WHERE ebeln = lt_ekpo-ebeln
**           AND ebelp = lt_ekpo-ebelp.
*      "判断发票校验未完成
*
*      SELECT ekko~ebeln ekko~bukrs
*        INTO CORRESPONDING FIELDS OF TABLE lt_ekpo
*       FROM ekko
*          FOR ALL ENTRIES IN lt_ekpo
*          WHERE ebeln = lt_ekpo-ebeln.
*
*
*
*    ENDIF.

    "取出库存数据所需的字段


    SELECT matnr bwkey lbkum
    INTO CORRESPONDING FIELDS OF TABLE lt_mbew
    FROM mbew
    FOR ALL ENTRIES IN lt_alv
    WHERE matnr = lt_alv-matnr AND bwkey = lt_alv-werks.

    "取出流程订单所需的字段


    SELECT afko~aufnr afko~gstri afko~plnbez aufk~idat2 aufk~werks
             FROM afko  INNER JOIN aufk ON afko~aufnr = aufk~aufnr
    INTO CORRESPONDING FIELDS OF TABLE lt_afko1
    FOR ALL ENTRIES IN lt_alv
    WHERE afko~plnbez = lt_alv-matnr
      AND aufk~werks = lt_alv-werks
      AND gstri IN s_cxrq.

    "取出物料凭证所需的字段


    SELECT mseg~mblnr mseg~bwart mseg~matnr mseg~erfmg mseg~werks mkpf~budat
    FROM mseg  INNER JOIN mkpf ON mkpf~mblnr = mseg~mblnr AND mkpf~mjahr = mseg~mjahr
    INTO CORRESPONDING FIELDS OF TABLE lt_mseg1
    FOR ALL ENTRIES IN lt_alv
    WHERE mseg~matnr = lt_alv-matnr
      AND mseg~werks = lt_alv-werks
      AND mkpf~budat IN s_cxrq
      AND mseg~bwart = '261'
      OR mseg~bwart = '201'.

    "取出销售订单所需要的数据
    "未发货完成，总体交货状态不为C
    SELECT
       matnr,
       werks,
       vbak~vbeln
    INTO TABLE @DATA(lt_vbap)
    FROM vbap
    INNER JOIN vbak
    ON vbak~vbeln = vbap~vbeln
    FOR ALL ENTRIES IN @lt_alv
    WHERE matnr = @lt_alv-matnr
      AND werks = @lt_alv-werks
      AND lfgsk <> 'C'
      AND auart NOT LIKE '%ZB%'
      AND kwmeng > '0.01'
      AND vbak~erdat IN @s_cxrq.
    "进行数量合并，统计未发货的订单数量
    LOOP AT lt_vbap INTO DATA(ls_vbap).
      MOVE-CORRESPONDING ls_vbap TO ls_so.
      ls_so-count = 1.
      COLLECT ls_so INTO lt_so.
      CLEAR:ls_vbap,ls_so.
    ENDLOOP.
    "未开票完成的订单
    SELECT
       matnr,
       werks,
       vbak~vbeln
    INTO TABLE @DATA(lt_vbap2)
    FROM vbap
    INNER JOIN vbak
    ON vbak~vbeln = vbap~vbeln
    FOR ALL ENTRIES IN @lt_alv
    WHERE matnr = @lt_alv-matnr
      AND werks = @lt_alv-werks
      AND gbstk <> 'C'
      AND auart NOT LIKE '%ZB%'
      AND kwmeng > '0.01'
      AND vbak~erdat IN @s_cxrq.

    LOOP AT lt_vbap2 INTO ls_vbap.
      MOVE-CORRESPONDING ls_vbap TO ls_so.
      ls_so-count = 1.
      COLLECT ls_so INTO lt_so2.
      CLEAR:ls_vbap,ls_so.
    ENDLOOP.


  ENDIF.

  "先循环，对判断发票校验未完成进行归类处理
  MOVE lt_ekpo TO lt_ekpo_tmp.
  SORT lt_ekpo_tmp BY ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM lt_ekpo_tmp COMPARING ebeln ebelp.
  LOOP AT lt_ekpo_tmp INTO ls_ekpo.
    READ TABLE lt_ekbe INTO DATA(ls_ekbea) WITH KEY matnr = ls_alv-matnr
                                                    werks = ls_alv-werks
                                                    vgabe = '1'
                                                    shkzg = 'S'.
    READ TABLE lt_ekbe INTO DATA(ls_ekbeb) WITH KEY matnr = ls_alv-matnr
                                                    werks = ls_alv-werks
                                                    vgabe = '1'
                                                    shkzg = 'H'.
    READ TABLE lt_ekbe INTO DATA(ls_ekbec) WITH KEY matnr = ls_alv-matnr
                                                    werks = ls_alv-werks
                                                    vgabe = '2'
                                                    shkzg = 'S'.
    READ TABLE lt_ekbe INTO DATA(ls_ekbed) WITH KEY matnr = ls_alv-matnr
                                                    werks = ls_alv-werks
                                                    vgabe = '2'
                                                    shkzg = 'H'.
    IF ( ls_ekbea-menge - ls_ekbeb-menge ) - ( ls_ekbec-menge - ls_ekbed-menge ) > 0.
      ls_po-matnr = ls_ekbe-matnr.
      ls_po-werks = ls_ekbe-werks.
      ls_po-count = 1.
      COLLECT ls_po INTO lt_po.
    ENDIF.
    CLEAR:ls_ekbe,ls_ekbea,ls_ekbeb,ls_ekbec,ls_ekbed,ls_po,ls_ekpo.
  ENDLOOP.
  "获取物料特性
  MOVE-CORRESPONDING lt_alv TO lt_objek.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'ZCUSCODE'
    IMPORTING
      output = lv_atinn.
  SORT lt_objek BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_objek COMPARING ALL FIELDS.
  SELECT
    objek,
    atinn,
    atwrt
    INTO TABLE @DATA(lt_ausp)
    FROM ausp
    FOR ALL ENTRIES IN @lt_objek
    WHERE ausp~objek = @lt_objek-matnr
      AND ausp~atinn = @lv_atinn.




  " LOOP循环取数
  LOOP AT lt_alv INTO ls_alv.
    MOVE-CORRESPONDING ls_alv TO gs_alv.

    "将物料描述表字段赋予
    READ TABLE lt_makt INTO ls_makt WITH KEY matnr = gs_alv-matnr.
    IF sy-subrc = 0 .
      gs_alv-maktx = ls_makt-maktx.
    ENDIF.

    "将库存信息字段赋予
    READ TABLE lt_mbew INTO ls_mbew WITH KEY matnr = gs_alv-matnr
                                             bwkey = gs_alv-werks.
    IF sy-subrc = 0 .

      IF ls_mbew-lbkum > '0'.
        gs_alv-kcqk = '有库存'.
      ELSEIF
        gs_alv-kcqk = '无库存'.

      ENDIF.
    ELSEIF
      gs_alv-kcqk = '无库存'.

    ENDIF.

    "将领料信息字段赋予
    SORT lt_mseg1 BY budat DESCENDING.
    READ TABLE lt_mseg1 INTO ls_mseg1 WITH KEY matnr = gs_alv-matnr
                                             werks = gs_alv-werks.


    IF sy-subrc = 0 .

      gs_alv-zjll = '最近的物料凭证日期:' && ls_mseg1-budat && '物料凭证号:'
      && ls_mseg1-mblnr && '移动类型:' && ls_mseg1-bwart && '过账数量' && ls_mseg1-erfmg.

    ELSEIF

   gs_alv-zjll = '无'.

    ENDIF.

    "将流程订单信息字段赋予
    LOOP AT lt_afko1 INTO ls_afko1.

      IF ls_afko1-plnbez = gs_alv-matnr AND ls_afko1-werks = gs_alv-werks.

*      READ TABLE lt_afko1 INTO ls_afko1 WITH KEY plnbez = gs_alv-matnr
*                                                  werks = gs_alv-werks.

        IF sy-subrc = 0 .
          num1 = num1 + 1.
        ENDIF.

      ENDIF.

      IF ls_afko1-plnbez = gs_alv-matnr AND ls_afko1-werks = gs_alv-werks AND ls_afko1-idat2 = ''..

*      READ TABLE lt_afko1 INTO ls_afko1 WITH KEY plnbez = gs_alv-matnr
*                                                  werks = gs_alv-werks
*                                                  idat2 = ''.

        IF sy-subrc = 0 .
          num2 = num2 + 1.
        ENDIF.

      ENDIF.

    ENDLOOP.

    IF num1 > 0.

      gs_alv-lcdd = '共有'&& num1 && '张工单,其中有'&& num2 && '张未TECO'.
    ELSE.
      gs_alv-lcdd = '无'.
    ENDIF.

    CLEAR num1.
    CLEAR num2.

    "将采购订单信息字段赋予
    READ TABLE lt_po_un INTO ls_po_un WITH KEY matnr = ls_alv-matnr
                                               werks = ls_alv-werks.
    IF sy-subrc = 0.
      num1 = ls_po_un-menge.
    ENDIF.
    READ TABLE lt_po INTO ls_po WITH KEY matnr = ls_alv-matnr
                                         werks = ls_alv-werks.
    IF sy-subrc = 0.
      num2 = ls_po-count.
    ENDIF.
    CLEAR:ls_ekpo2,ls_po.
    IF num1 IS NOT INITIAL OR num2 IS NOT INITIAL.
      gs_alv-cgdd = '共有'&& num1 && '张采购订单未收货完成；共有' && num2 && '张未发票过账。'.
      IF num1 IS NOT INITIAL.
        gs_alv-cgdd = gs_alv-cgdd && '未收货完成:'.
        LOOP AT lt_ekpo INTO ls_ekpo WHERE matnr = ls_alv-matnr AND werks = ls_alv-werks.
          gs_alv-cgdd = gs_alv-cgdd && ls_ekpo-ebeln && '/'.
          CLEAR ls_ekpo.
        ENDLOOP.
      ENDIF.
      IF num2 IS NOT INITIAL.
        gs_alv-cgdd = gs_alv-cgdd && '未发票过账:'.
        LOOP AT lt_po INTO ls_po WHERE matnr = ls_alv-matnr AND werks = ls_alv-werks.
          gs_alv-cgdd = gs_alv-cgdd && ls_po-ebeln && '/'.
        ENDLOOP.
      ENDIF.
    ENDIF.
    DATA(lv_lenth2) = cl_abap_list_utilities=>dynamic_output_length( gs_alv-cgdd ).
    IF lv_lenth2 >= 128.
      gs_alv-cgdd = gs_alv-cgdd+(127).
      gs_alv-cgdd = gs_alv-cgdd && '*'.
    ENDIF.
    CLEAR:num1,num2,ls_po,ls_ekpo2.
*    LOOP AT lt_ekko INTO ls_ekko.
*
*
*
*      READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ematn = gs_alv-matnr
*                                               werks = gs_alv-werks
*                                               ebeln = ls_ekko-ebeln
*                                               elikz = ''.
*
*      IF sy-subrc = 0 .
*        num1 = num1 + 1.
*      ENDIF.
*
*
*    ENDLOOP.
*
*    IF num1 > 0.
*
*      gs_alv-cgdd = '共有'&& num1 && '张采购订单未收货完成'.
*    ENDIF.
*
*    CLEAR num1.
*    CLEAR num2.

    "处理销售订单列
    READ TABLE lt_so INTO ls_so WITH KEY matnr = ls_alv-matnr
                                         werks = ls_alv-werks.
    IF sy-subrc = 0.
      num1 = ls_so-count.
    ENDIF.
    CLEAR ls_so.
    "处理销售订单列
    READ TABLE lt_so2 INTO ls_so WITH KEY matnr = ls_alv-matnr
                                          werks = ls_alv-werks.
    IF sy-subrc = 0.
      num2 = ls_so-count.
    ENDIF.
    CLEAR ls_so.
    IF num1 IS NOT INITIAL OR num2 IS NOT INITIAL.
      gs_alv-xsdd = '共有'&& num1 && '张单未发货完成；共有' && num2 && '张未发票过账。'.
      IF num1 IS NOT INITIAL.
        gs_alv-xsdd = gs_alv-xsdd && '未发货完成订单：'.
        LOOP AT lt_vbap INTO ls_vbap WHERE matnr = ls_alv-matnr AND werks = ls_alv-werks.
          gs_alv-xsdd = gs_alv-xsdd && ls_vbap-vbeln && '/'.
          CLEAR ls_vbap.
        ENDLOOP.
      ENDIF.
      IF num2 IS NOT INITIAL.
        gs_alv-xsdd = gs_alv-xsdd && '未发票过账订单：'.
        LOOP AT lt_vbap2 INTO ls_vbap WHERE matnr = ls_alv-matnr AND werks = ls_alv-werks.
          gs_alv-xsdd = gs_alv-xsdd && ls_vbap-vbeln && '/'.
          CLEAR ls_vbap.
        ENDLOOP.
      ENDIF.
    ENDIF.
    DATA(lv_lenth) = cl_abap_list_utilities=>dynamic_output_length( gs_alv-xsdd ).
    IF lv_lenth >= 128.
      gs_alv-xsdd = gs_alv-xsdd+(127).
      gs_alv-xsdd = gs_alv-xsdd && '*'.
    ENDIF.
    CLEAR:num1,num2.


    " 获取批次属性和物料特征
    READ TABLE lt_ausp INTO DATA(ls_ausp) WITH KEY objek = ls_alv-matnr.
    IF sy-subrc = 0.
      gs_alv-zcuscode = ls_ausp-atwrt.
    ENDIF.
*    IF gs_alv-matnr IS NOT INITIAL.
*
*      CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
*        EXPORTING
*          matnr  = gs_alv-matnr
**         ATINN  =
*        TABLES
*          output = ltoutput.
**         " 开始取物料特征  " 客户检索项（客户代码）
*      READ TABLE  ltoutput WITH KEY atnam = 'ZCUSCODE' INTO DATA(ls_output).
*      IF sy-subrc = 0.
*        gs_alv-zcuscode = ls_output-atwrt.
*      ENDIF.
*    ENDIF.

*    " 取物料长文本
*
*    lv_name = gs_alv-matnr.
*    PERFORM frm_read_text USING 'GRUN' 'MATERIAL' lv_name sy-langu
*                          CHANGING gs_alv-maktxl.

    "去除前导零 top.
    IF gs_alv-matnr IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = gs_alv-matnr
        IMPORTING
          output = gs_alv-matnr.
    ENDIF.
    "去除前导零 end.


    APPEND gs_alv TO gt_alv.

    CLEAR gs_alv.

  ENDLOOP.
  SORT gt_alv BY matnr.
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
    l_colpos 'MATNR'    '物料编码'     '' ' ' ' ' '' '' '',
    l_colpos 'MAKTX'    '物料描述'     '' ' ' ' ' '' '' '',
    l_colpos 'WERKS'    '工厂'     '' ' ' ' ' '' '' '',
    l_colpos 'MTART'    '物料类型'     '' ' ' ' ' '' '' '',
    l_colpos 'LVORM1'    '删除标记（集团）'     '' ' ' ' ' '' '' '',
    l_colpos 'LVORM2'    '删除标记（工厂）'     '' ' ' ' ' '' '' '',
    l_colpos 'MMSTA'    '特定工厂的物料状态'     '' ' ' ' ' '' '' '',
    l_colpos 'MMSTD'    '有效起始期'     '' ' ' ' ' '' '' '',
    l_colpos 'ZCUSCODE' '客户检索项（客户代码）'     '' ' ' ' ' '' '' '',
    l_colpos 'ERSDA '    '物料创建时间'     '' ' ' ' ' '' '' '',
    l_colpos 'CGDD '    '采购订单'     '' ' ' ' ' '' '' '',
    l_colpos 'XSDD'    '销售订单'     '' ' ' ' ' '' '' '',
    l_colpos 'KCQK'    '库存情况'     '' ' ' ' ' '' '' '',
    l_colpos 'LCDD'    '流程订单'     '' ' ' ' ' '' '' '',
    l_colpos 'ZJLL'    '最近领料'     '' ' ' ' ' '' '' ''.


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
   ID 'WERKS' FIELD s_werks.
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
FORM frm_read_text USING id TYPE thead-tdid
                         object TYPE thead-tdobject
                         name TYPE thead-tdname
                         langu TYPE thead-tdspras
                   CHANGING ret TYPE char200.
  ret = ''.
  DATA: lt_tline TYPE TABLE OF tline.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = id
      language                = langu
      name                    = name
      object                  = object
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
*   IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      lines                   = lt_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc EQ 0.
    LOOP AT lt_tline ASSIGNING FIELD-SYMBOL(<fs_line>).
      ret = ret && <fs_line>-tdline.
    ENDLOOP.
  ENDIF.

ENDFORM.