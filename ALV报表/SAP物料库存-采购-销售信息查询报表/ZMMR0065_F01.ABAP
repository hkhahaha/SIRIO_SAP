*&---------------------------------------------------------------------*
*& 包含               ZMMR0065_F01.
*&---------------------------------------------------------------------*

FORM frm_get_data .

    " 定义所需的结构体
  
    DATA: lt_marc TYPE TABLE OF marc,
          ls_mrac TYPE marc,
  
          lt_mara TYPE TABLE OF mara,
          ls_mara TYPE mara,
  
          lt_makt TYPE TABLE OF makt,
          ls_makt TYPE makt,
  
          lt_mapl TYPE TABLE OF mapl,
          ls_mapl TYPE mapl,
  
          lt_ekpo TYPE TABLE OF ekpo,
          ls_ekpo TYPE ekpo,
  
          lt_ekko TYPE TABLE OF ekko,
          ls_ekko TYPE ekko,
  
          lt_ekbe	TYPE TABLE OF ekbe,
          ls_ekbe TYPE ekbe,
  
          lt_mard	TYPE TABLE OF mard,
          ls_mard TYPE mard,
  
          lt_mbew	TYPE TABLE OF mbew,
          ls_mbew TYPE mbew,
  
          lt_afko	TYPE TABLE OF afko,
          ls_afko TYPE afko,
  
          lt_aufk	TYPE TABLE OF aufk,
          ls_aufk TYPE aufk,
  
          lt_rkpf	TYPE TABLE OF rkpf,
          ls_rkpf TYPE rkpf,
  
  
          lt_mseg TYPE TABLE OF mseg,
          ls_mseg TYPE mseg.
  
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
  
    DATA:lt_ekpo1 LIKE TABLE OF lt_ekpo,
         ls_ekpo1 LIKE LINE OF lt_ekpo1.
  
  
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
      SELECT ekpo~ebeln ekpo~ebelp ekpo~elikz ekko~aedat ekpo~werks
         FROM ekpo  INNER JOIN ekko ON ekko~ebeln = ekpo~ebeln
           INTO CORRESPONDING FIELDS OF TABLE lt_ekpo
             FOR ALL ENTRIES IN lt_alv
             WHERE ekpo~ematn = lt_alv-matnr
             AND ekpo~werks = lt_alv-werks
             AND ekpo~loekz = ''
             AND ekko~aedat IN s_cxrq.
  
      IF lt_ekpo IS NOT INITIAL.
  
        SELECT ekbe~ebeln ekbe~ebelp ekbe~vgabe ekbe~shkzg ekbe~menge
           INTO CORRESPONDING FIELDS OF TABLE lt_ekbe
          FROM ekbe
             FOR ALL ENTRIES IN lt_ekpo
             WHERE ebeln = lt_ekpo-ebeln
             AND ebelp = lt_ekpo-ebelp.
  
        SELECT ekko~ebeln ekko~bukrs
          INTO CORRESPONDING FIELDS OF TABLE lt_ekpo
         FROM ekko
            FOR ALL ENTRIES IN lt_ekpo
            WHERE ebeln = lt_ekpo-ebeln.
  
  
  
  
  
      ENDIF.
  
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
  
  
    ENDIF.
  
  
  
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
      LOOP AT lt_ekko INTO ls_ekko.
  
  
  
        READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ematn = gs_alv-matnr
                                                 werks = gs_alv-werks
                                                 ebeln = ls_ekko-ebeln
                                                 elikz = ''.
  
        IF sy-subrc = 0 .
          num1 = num1 + 1.
        ENDIF.
  
  
      ENDLOOP.
  
      IF num1 > 0.
  
        gs_alv-cgdd = '共有'&& num1 && '张采购订单未收货完成'.
      ENDIF.
  
      CLEAR num1.
      CLEAR num2.
  
  
  
      " 获取批次属性和物料特征
  
      IF gs_alv-matnr IS NOT INITIAL.
  
        CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
          EXPORTING
            matnr  = gs_alv-matnr
  *         ATINN  =
          TABLES
            output = ltoutput.
  *         " 开始取物料特征  " 客户检索项（客户代码）
        READ TABLE  ltoutput WITH KEY atnam = 'ZCUSCODE' INTO DATA(ls_output).
        IF sy-subrc = 0.
          gs_alv-zcuscode = ls_output-atwrt.
        ENDIF.
  
      ENDIF.
  
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
      l_colpos 'mtart'    '物料类型'     '' ' ' ' ' '' '' '',
      l_colpos 'lvorm1'    '删除标记（集团）'     '' ' ' ' ' '' '' '',
      l_colpos 'lvorm2'    '删除标记（工厂）'     '' ' ' ' ' '' '' '',
      l_colpos 'mmsta'    '特定工厂的物料状态'     '' ' ' ' ' '' '' '',
      l_colpos 'mmstd'    '有效起始期'     '' ' ' ' ' '' '' '',
      l_colpos 'zcuscode' '客户检索项（客户代码）'     '' ' ' ' ' '' '' '',
      l_colpos 'ersda '    '物料创建时间'     '' ' ' ' ' '' '' '',
      l_colpos 'cgdd '    '采购订单'     '' ' ' ' ' '' '' '',
      l_colpos 'xsdd'    '销售订单'     '' ' ' ' ' '' '' '',
      l_colpos 'kcqk'    '库存情况'     '' ' ' ' ' '' '' '',
      l_colpos 'lcdd'    '流程订单'     '' ' ' ' ' '' '' '',
      l_colpos 'zjll'    '最近领料'     '' ' ' ' ' '' '' ''.
  
  
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