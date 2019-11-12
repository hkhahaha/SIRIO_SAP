*----------------------------------------------------------------------*
***INCLUDE ZQMR0005_F01.
*---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_DISPALY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_dispaly_alv .

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_fieldcat TYPE lvc_s_fcat,
        lt_sort     TYPE lvc_t_sort,
        lt_events   TYPE slis_t_event,
        ls_layout   TYPE lvc_s_layo,
        ls_variant  TYPE disvariant,
        ls_s_glay   TYPE lvc_s_glay,
        l_pos       TYPE i VALUE 0.

  DEFINE setfieldcat.
    l_pos = l_pos + 1.
    wa_fieldcat-col_pos = l_pos.
    wa_fieldcat-fieldname = &1.
    wa_fieldcat-coltext   = &2.
    wa_fieldcat-no_zero = 'X'.
    wa_fieldcat-edit = &3.
    wa_fieldcat-outputlen = &4.

    APPEND wa_fieldcat TO lt_fieldcat. CLEAR: wa_fieldcat.
  END-OF-DEFINITION.

  setfieldcat 'MATNR' '物料' '' ''.
  setfieldcat 'MAKTX' '物料描述' '' ''.
  setfieldcat 'PLNNR' '组' '' ''.
  setfieldcat 'PLNAL' '组计数器' '' ''.
  setfieldcat 'KTEXT' '任务清单描述' '' ''.
  setfieldcat 'VERSION' '版本号' 'X' '40'.

  ls_layout-sel_mode = 'B'.
  ls_layout-box_fname     = 'SEL'.
  "ls_layout-CWIDTH_OPT = 'X'.
  ls_s_glay-edt_cll_cb = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_PF_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND'
      i_grid_settings          = ls_s_glay
      it_fieldcat_lvc          = lt_fieldcat[]
*     it_events                = lt_events
*     it_sort_lvc              = lt_sort
      is_layout_lvc            = ls_layout
      is_variant               = ls_variant
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_output.

ENDFORM.

FORM frm_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'S1000' EXCLUDING rt_extab.
ENDFORM.

FORM frm_user_command USING r_ucomm LIKE sy-ucomm rs_selfield TYPE slis_selfield.

  DATA:lo_guid     TYPE REF TO cl_gui_alv_grid.

  "将alv上编辑的内容更新到内表中
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lo_guid.
  CALL METHOD lo_guid->check_changed_data.

  CASE r_ucomm.
    WHEN 'P'.

      CLEAR gt_qpmk_print.

      "判断是否有选中记录
      IF NOT line_exists( gt_output[ sel = 'X' ] ).
        MESSAGE '请选择一条记录！' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.


      gs_output = gt_output[ sel = 'X' ].


      "获取规格标准需要打印的数据

      "获取规格，根据matnr物料编码从mara表找出规格字段
      SELECT SINGLE mara~groes, mara~matkl, mara~mtart
        INTO CORRESPONDING FIELDS OF @gs_output
        FROM mara
        WHERE matnr = @gs_output-matnr.

      IF gs_output-mtart EQ 'Z030'. "物料类型属于半成品，获取物料特征值，取001分类中物料组特征ZSTDNAME的特征值
        data(lv_zmold) = zcl_bc_public=>get_class_value( atnam = 'ZMOLD'
                                               key = zcl_bc_public=>conv_by_ddic( i_input = gs_output-matnr i_abap = 'matnr' )
                                               classtype = '001'
                                               classnum = gs_output-matkl
                                               objtable = 'MARA' ).
        gs_output-GROES_TEXT = gs_output-groes.
        IF lv_zmold is not INITIAL.
          gs_output-groes_text = gs_output-GROES_TEXT && '(' && lv_zmold && ')'.
        ENDIF.

      ELSE.
        gs_output-groes_text = '/'.
      ENDIF.

      gs_output-stanard_name = zcl_bc_public=>get_class_value( atnam = 'ZSTDNAME' key = zcl_bc_public=>conv_by_ddic( i_input = gs_output-matnr i_abap = 'matnr' ) classtype = '001' classnum = gs_output-matkl objtable = 'MARA' ).
      IF gs_output-stanard_name IS INITIAL.
        gs_output-stanard_name = '/'.
      ENDIF.

      "获取主检验特性
      SELECT plmk~qdynregel,  "动态修改规则编号
        plmk~pmethode,        "检验方法编号
        plmk~pmtversion,      "检验方法版本
        qpmk~mkmnr,           "主检验特性编号
        qpmk~dummy40,         "主检验特性信息字段3
        qpmk~masseinhsw,      "计量单位
        qpmk~version,          "主检验特性版本
        qpmk~steuerkz          "控制标识符
        INTO CORRESPONDING FIELDS OF TABLE @gt_qpmk
        FROM plas INNER JOIN plmk ON plas~plnnr = plmk~plnnr AND plas~plnkn = plmk~plnkn
          INNER JOIN qpmk ON plmk~qpmk_zaehl = qpmk~zaehler
          AND plmk~verwmerkm = qpmk~mkmnr AND plmk~mkversion = qpmk~version
        WHERE plmk~plnty = 'Q' AND plmk~loekz = '' AND plmk~plnnr = @gs_output-plnnr
          AND plas~plnty = 'Q' AND plas~plnal = @gs_output-plnal and plas~loekz = '' order by plmk~merknr.


      "查询主检验特性的ZH和Z1长文本
      DATA lv_z1 TYPE thead-tdspras.
      lv_z1 = zcl_bc_public=>conv_by_ddic( i_input = 'Z1' i_abap = 'sylangu').
      SELECT *
        INTO TABLE @DATA(lt_qpmt)
        FROM qpmt
        FOR ALL ENTRIES IN @gt_qpmk
        WHERE mkmnr = @gt_qpmk-mkmnr AND version = @gt_qpmk-version AND ( sprache = @sy-langu OR sprache = @lv_z1 ).

      "检查检验方法是否有长文本
      SELECT DISTINCT *
        INTO TABLE @DATA(lt_qmtt)
        FROM qmtt
        FOR ALL ENTRIES IN @gt_qpmk
        WHERE pmtnr = @gt_qpmk-pmethode AND version = @gt_qpmk-pmtversion AND sprache = @sy-langu.

      DATA: lv_name TYPE thead-tdname,
            ls_qpmt TYPE qpmt,
            ls_qmtt TYPE qmtt,
            lv_id   TYPE string.

      DATA: lr_qddrt TYPE RANGE OF qddrt-dynregel,
            lw_qddrt LIKE LINE OF lr_qddrt.

*     宏定义
      DEFINE set_range_qddrt.
        lw_qddrt-sign   = 'I'.
        lw_qddrt-option = 'EQ'.
        lw_qddrt-low    = &1.
        APPEND lw_qddrt TO lr_qddrt.
      END-OF-DEFINITION.

      "查询单位表t006,根据gt_qpmk内表中每条记录的MASSEINHSW字段进行过滤
      SELECT *
        INTO  TABLE @DATA(lt_t006a)
        FROM t006a
        FOR ALL ENTRIES IN @gt_qpmk
        WHERE msehi = @gt_qpmk-masseinhsw
          AND spras = @sy-langu.


      LOOP AT gt_qpmk ASSIGNING FIELD-SYMBOL(<fs_qpmk>).

        "判断主检验特性中的控制标识符中的打印选项。如果打印选项<fs_qpmk>-steuerkz第26位=*，则该检验特性不在smartforms里面显示，
        DATA(temp) = <fs_qpmk>-steuerkz+25(1).
        IF temp = '*'.
          CONTINUE.
        ENDIF.

        READ TABLE lt_t006a INTO DATA(ls_t006a) WITH KEY msehi = <fs_qpmk>-masseinhsw.
        IF sy-subrc EQ 0.
          <fs_qpmk>-mseh6 = ls_t006a-mseht.
        ENDIF.

        "判断动态修改规则编号是否为空，不为空则将编号放入查询条件lr_qddrt，后续根据条件查询出动态修改规则的中文描述
        IF <fs_qpmk>-qdynregel IS NOT INITIAL.
          set_range_qddrt <fs_qpmk>-qdynregel.
        ENDIF.


        IF <fs_qpmk>-mkmnr IS NOT INITIAL AND <fs_qpmk>-version IS NOT INITIAL.

          "主获取检验特性ZH长文本
          ls_qpmt = lt_qpmt[ mkmnr = <fs_qpmk>-mkmnr version = <fs_qpmk>-version sprache = sy-langu ].

          IF ls_qpmt-ltextkz = 'X'.
            "读取主检验特征长文本
            CLEAR lv_name.

            lv_name = sy-mandt && gs_output-werks && |{ <fs_qpmk>-mkmnr PAD = ' ' WIDTH = 8 }| && <fs_qpmk>-version && sy-langu.

            PERFORM frm_read_text USING 'QPMT' 'QPMERKMAL' lv_name sy-langu
                                  CHANGING <fs_qpmk>-verwmerkm_text.
          ELSE.
            "读取短文本
            <fs_qpmk>-verwmerkm_text = ls_qpmt-kurztext.
          ENDIF.

          "主获取检验特性Z1长文本
          READ TABLE lt_qpmt INTO ls_qpmt WITH KEY mkmnr = <fs_qpmk>-mkmnr version = <fs_qpmk>-version sprache = lv_z1.
          IF sy-subrc EQ 0.
            IF ls_qpmt-ltextkz = 'X'.
              "读取主检验特征长文本
              CLEAR lv_name.

              lv_name = sy-mandt && gs_output-werks && |{ <fs_qpmk>-mkmnr PAD = ' ' WIDTH = 8 }| && <fs_qpmk>-version && lv_z1.

              PERFORM frm_read_text USING 'QPMT' 'QPMERKMAL' lv_name lv_z1
                                    CHANGING <fs_qpmk>-verwmerkm_text_z1.
            ELSE.
              "读取短文本
              <fs_qpmk>-verwmerkm_text_z1 = ls_qpmt-kurztext.
            ENDIF.
          ENDIF.

        ENDIF.

        IF <fs_qpmk>-pmethode IS NOT INITIAL AND <fs_qpmk>-pmtversion IS NOT INITIAL.
          ls_qmtt = lt_qmtt[ pmtnr = <fs_qpmk>-pmethode version = <fs_qpmk>-pmtversion ].

          IF ls_qmtt-ltextkz = 'X'.
            CLEAR lv_name.

            lv_name = sy-mandt && gs_output-werks && |{ <fs_qpmk>-pmethode PAD = ' ' WIDTH = 8 }| && <fs_qpmk>-pmtversion && sy-langu.
            "读取检验方法长文本
            PERFORM frm_read_text USING 'QMTT' 'QPMETHODE' lv_name sy-langu
                                CHANGING <fs_qpmk>-qpmethode_text.
          ELSE.
            "读取短文本
            <fs_qpmk>-qpmethode_text = ls_qmtt-kurztext.
          ENDIF.
        ENDIF.

        APPEND <fs_qpmk> TO gt_qpmk_print.

      ENDLOOP.

      "根据动态修改规则编号获取相应描述
      IF lr_qddrt IS NOT INITIAL.
        SELECT *
        INTO  TABLE @gt_qddrt
        FROM qddrt
        WHERE dynregel IN @lr_qddrt.

      ENDIF.
*      append VALUE #( kurztext = '未标识的检验项目为每批必检' ) to gt_qddrt.


      SELECT SINGLE *
        INTO @DATA(ls_zqmt007)
        FROM zqmt007
        WHERE werks = @p_werks AND zbddh = @sy-tcode.

      IF sy-subrc EQ 0.
        gs_output-file_num = ls_zqmt007-ztxbh.
      ENDIF.

      PERFORM frm_print.




  ENDCASE.

ENDFORM.


FORM frm_print.
  DATA: lv_fm_name TYPE rs38l_fnam.


  "获取SMARTFOMRS函数
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZQMR0005' "Smart Form名称
    IMPORTING
      fm_name            = lv_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  CALL FUNCTION lv_fm_name
    EXPORTING
*     ARCHIVE_INDEX              =
*     ARCHIVE_INDEX_TAB          =
*     ARCHIVE_PARAMETERS         =
*     CONTROL_PARAMETERS         =
*     MAIL_APPL_OBJ              =
*     MAIL_RECIPIENT             =
*     MAIL_SENDER                =
*     OUTPUT_OPTIONS             =
*     USER_SETTINGS              = 'X'
      gs_head   = gs_output
      znkbz     = p_znkbz
*   IMPORTING
*     DOCUMENT_OUTPUT_INFO       =
*     JOB_OUTPUT_INFO            =
*     JOB_OUTPUT_OPTIONS         =
    TABLES
      gt_detail = gt_qpmk_print
      gt_qddrt  = gt_qddrt
*   EXCEPTIONS
*     FORMATTING_ERROR           = 1
*     INTERNAL_ERROR             = 2
*     SEND_ERROR                 = 3
*     USER_CANCELED              = 4
*     OTHERS    = 5
    .



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
      client   = sy-mandt
      id       = id
      language = langu
      name     = name
      object   = object
*     ARCHIVE_HANDLE                = 0
*     LOCAL_CAT                     = ' '
*   IMPORTING
*     HEADER   =
*     OLD_LINE_COUNTER              =
    TABLES
      lines    = lt_tline
*   EXCEPTIONS
*     ID       = 1
*     LANGUAGE = 2
*     NAME     = 3
*     NOT_FOUND                     = 4
*     OBJECT   = 5
*     REFERENCE_CHECK               = 6
*     WRONG_ACCESS_TO_ARCHIVE       = 7
*     OTHERS   = 8
    .
  IF sy-subrc EQ 0.
    LOOP AT lt_tline ASSIGNING FIELD-SYMBOL(<fs_line>).
      ret = ret && <fs_line>-tdline.
    ENDLOOP.
  ENDIF.

ENDFORM.