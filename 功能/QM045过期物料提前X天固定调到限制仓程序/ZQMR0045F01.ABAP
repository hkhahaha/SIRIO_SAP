*&---------------------------------------------------------------------*
*& 包含               ZQMR0045F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_GET_EXT_ST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*

FORM frm_get_ext_st .
    CLEAR: gt_ext_st.
    SELECT * FROM zmmwms_ext_st INTO TABLE gt_ext_st.
  
    t001w-werks = '8100'.
    IF s_matnr IS NOT INITIAL.
      SELECT
      mara~matnr,
      mard~werks,
      mchb~charg,
      mard~lgort
  
      FROM mara
      INNER JOIN mard
      ON mard~matnr = mara~matnr
      INNER JOIN mchb
      ON mchb~matnr = mara~matnr
      AND mchb~werks = mard~werks
      AND mchb~lgort = mard~lgort
      WHERE mara~matnr IN @s_matnr
      AND mtart IN @s_mtart
      AND mtart IN ('Z010','Z020','Z050')
      AND mard~werks = @p_werks
      AND mard~lgort IN ( SELECT lgort FROM zmm_lgort )
      AND mchb~charg IN @s_charg
      INTO TABLE @DATA(lt_mara).
  
      SELECT
      a~matnr,
      a~werks,
      a~charg,
      a~lgort
      FROM @lt_mara AS a
      INNER JOIN mseg
      ON mseg~matnr = a~matnr
      AND mseg~werks = a~werks
      AND mseg~lgort = a~lgort
      AND mseg~charg = a~charg
      WHERE mseg~bwart = @p_bwart
      INTO TABLE @DATA(lt_all).
  
      LOOP AT lt_all INTO DATA(ls_all).
        ls_all-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_all-matnr ).
        gs_item-matnr = ls_all-matnr.
        APPEND gs_item TO gt_item.
        CLEAR gs_item.
        CLEAR ls_all.
      ENDLOOP.
    ELSE.
  *    MESSAGE '物料号为空' TYPE 'I'.
    ENDIF.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_AUTHORITY_CHECK_01
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_authority_check_01 .
    IF p_bwart IS INITIAL.
      MESSAGE '创建的时候，移动类型必输输入' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  
    IF p_werks IS INITIAL.
      MESSAGE '创建的时候，工厂必输输入' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  
  *  AUTHORITY-CHECK OBJECT 'ZMM030'
  *  ID 'WERKS' FIELD p_werks
  *  ID 'BWART' FIELD p_bwart.
    IF sy-subrc <> 0.
      DATA(lv_message) = |{ p_werks }/{ p_bwart }|.
      MESSAGE s002(zauthority) DISPLAY LIKE 'E' WITH '工厂/移动类型' lv_message.
      STOP.
    ENDIF.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_SAVE
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_save .
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form ADDRECORD
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM addrecord.
    CLEAR gs_item.
    APPEND gs_item TO gt_item.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form CATALOG
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM catalog .
    w_repid = sy-repid.
    CLEAR fieldcat.
  
    DEFINE fieldcatset."宏定义
      fieldcat-just = 'C'."字段居中显示
      fieldcat-outputlen = 10."自定义字段的长度
      fieldcat-fieldname = &1."透明表字段名
      fieldcat-seltext_l = &2."ALV列名
      fieldcat-col_pos = &3."列位置
      APPEND fieldcat.
    END-OF-DEFINITION.
  
  
    fieldcatset 'BUKRS' '公司代码' sy-tabix.
    fieldcatset 'gjahr' '会计年度' sy-tabix.
    fieldcatset 'BUKRS' '会计期间' sy-tabix.
    fieldcatset 'BELNR' '凭证编号' sy-tabix.
    fieldcatset 'BLART' '凭证类型' sy-tabix.
    fieldcatset 'BLDAT' '凭证日期' sy-tabix.
    fieldcatset 'BUDAT' '过账日期' sy-tabix.
    fieldcatset 'XSTOV' '冲销标识' sy-tabix.
    fieldcatset 'STBLG' '冲销凭证' sy-tabix.
    fieldcatset 'PPNAM' '制单人' sy-tabix.
    fieldcatset 'USNAM' '过账人' sy-tabix.
  
  *layout-colwidth_optimize = 'X'.
    layout-zebra = 'X'."斑马线的样式
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_AUTHORITY_CHECK
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_authority_check .
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_SCREEN_CHECK
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_screen_check .
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form GETDATA
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM getdata .
    SELECT
      *
    INTO CORRESPONDING FIELDS OF TABLE lt_alv
    FROM zmm_changeno.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form SHOWALV
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM showalv .
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
  *     I_INTERFACE_CHECK        = ' '
  *     I_BYPASSING_BUFFER       = ' '
  *     I_BUFFER_ACTIVE          = ' '
        i_callback_program       = w_repid "程序名称
        i_callback_pf_status_set = 'FRM_SET_PF_STATUS'
  *     i_callback_user_command  = 'ALV_USER_COMMAND' "对ALV操作的时候触发所定义的子程序
  *     I_CALLBACK_TOP_OF_PAGE   = ' '
  *     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
  *     I_CALLBACK_HTML_END_OF_LIST       = ' '
  *     I_STRUCTURE_NAME         =
  *     I_BACKGROUND_ID          = ' '
  *     i_grid_title             = '会计凭证'"标题名
  *     I_GRID_SETTINGS          =
        is_layout                = layout "程序所定义的layout名称
        it_fieldcat              = fieldcat[] "定义fieldcat数据
  *     IT_EXCLUDING             =
  *     IT_SPECIAL_GROUPS        =
  *     IT_SORT                  =
  *     IT_FILTER                =
  *     IS_SEL_HIDE              =
  *     I_DEFAULT                = 'X'
  *     I_SAVE                   = ' '
  *     IS_VARIANT               =
  *     IT_EVENTS                =
  *     IT_EVENT_EXIT            =
  *     IS_PRINT                 =
  *     IS_REPREP_ID             =
  *     I_SCREEN_START_COLUMN    = 0
  *     I_SCREEN_START_LINE      = 0
  *     I_SCREEN_END_COLUMN      = 0
  *     I_SCREEN_END_LINE        = 0
  *     I_HTML_HEIGHT_TOP        = 0
  *     I_HTML_HEIGHT_END        = 0
  *     IT_ALV_GRAPHICS          =
  *     IT_HYPERLINK             =
  *     IT_ADD_FIELDCAT          =
  *     IT_EXCEPT_QINFO          =
  *     IR_SALV_FULLSCREEN_ADAPTER        =
  * IMPORTING
  *     E_EXIT_CAUSED_BY_CALLER  =
  *     ES_EXIT_CAUSED_BY_USER   =
      TABLES
        t_outtab                 = lt_alv
      EXCEPTIONS "下面都是默认的
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
  * Implement suitable error handling here
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  
  ENDFORM.
  
  FORM frm_set_pf_status USING pt_extab TYPE slis_t_extab.
    SET PF-STATUS 'ZGUI2'.
  ENDFORM.