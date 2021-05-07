*&---------------------------------------------------------------------*
*& 包含               ZMMR0064_F01
*&---------------------------------------------------------------------*

FORM frm_change_screen .

    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'M1'.
          IF p_r01 = 'X'.
            screen-active = '1'.
          ELSE.
            screen-active = '0'.
          ENDIF.
        WHEN 'M2'.
          IF p_r02 = 'X'.
            screen-active = '1'.
          ELSE.
            screen-active = '0'.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_INIT
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_init .
    CONCATENATE icon_xxl TEXT-i01 INTO sscrfields-functxt_01.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_SCREEN_CHECK
  *&---------------------------------------------------------------------*
  *& 屏幕检查
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_screen_check .
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_AUTHORITY_CHECK
  *&---------------------------------------------------------------------*
  *& 权限检查
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_authority_check .
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_DATA
  *&---------------------------------------------------------------------*
  *& 获取数据
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_get_data .
    "A功能：APS送货需求匹配
    PERFORM frm_get_data_a.
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_ALV_A
  *&---------------------------------------------------------------------*
  *& A功能的ALV处理
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_alv_a .
  
  ENDFORM.
  *&---------------------------------------------------------------------*
  *& Form FRM_GET_DATA_A
  *&---------------------------------------------------------------------*
  *& A功能的数据处理
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_get_data_a .
    SELECT
      mandt
      carrid
      connid
      fldate
      price
      currency
      planetype
      seatsmax
      seatsocc
      paymentsum
      seatsmax_b
      seatsocc_b
      seatsmax_f
      seatsocc_f
  
  
      INTO CORRESPONDING FIELDS OF TABLE gt_sflight
      FROM sflight.
  
    SELECT
      carrid
      connid
      cityfrom
      cityto
      INTO CORRESPONDING FIELDS OF TABLE gt_spfli
      FROM spfli.
  
  
    gs_field_cat1-col_pos = 1.
    gs_field_cat1-fieldname = 'CARRID'.
    gs_field_cat1-scrtext_m = 'airline code'.
    APPEND gs_field_cat1 TO gt_field_cat1.
    gs_field_cat1-col_pos = 2.
    gs_field_cat1-fieldname = 'CONNID'.
    gs_field_cat1-scrtext_m = 'connection number'.
    APPEND gs_field_cat1 TO gt_field_cat1.
    gs_field_cat1-col_pos = 3.
    gs_field_cat1-fieldname = 'FLDATE'.
    gs_field_cat1-scrtext_m = 'flight date'.
    APPEND gs_field_cat1 TO gt_field_cat1.
    gs_field_cat1-col_pos = 4.
    gs_field_cat1-fieldname = 'PRICE'.
    gs_field_cat1-scrtext_m = 'flight price'.
    APPEND gs_field_cat1 TO gt_field_cat1.
    gs_field_cat1-col_pos = 5.
    gs_field_cat1-fieldname = 'PRICE'.
    gs_field_cat1-scrtext_m = 'flight price'.
    APPEND gs_field_cat1 TO gt_field_cat1.
    gs_field_cat1-col_pos = 6.
    gs_field_cat1-fieldname = 'PRICE'.
    gs_field_cat1-scrtext_m = 'flight price'.
    APPEND gs_field_cat1 TO gt_field_cat1.
    gs_field_cat1-col_pos = 7.
    gs_field_cat1-fieldname = 'PRICE'.
    gs_field_cat1-scrtext_m = 'flight price'.
    APPEND gs_field_cat1 TO gt_field_cat1.
    gs_field_cat1-col_pos = 8.
    gs_field_cat1-fieldname = 'PRICE'.
    gs_field_cat1-scrtext_m = 'flight price'.
    APPEND gs_field_cat1 TO gt_field_cat1.
  
  
  
  *   Build field catelog 2
    gs_field_cat2-col_pos = 1.
    gs_field_cat2-fieldname = 'CARRID'.
    gs_field_cat2-scrtext_m = 'airline code'.
    APPEND gs_field_cat2 TO gt_field_cat2.
    gs_field_cat2-col_pos = 2.
    gs_field_cat2-fieldname = 'CONNID'.
    gs_field_cat2-scrtext_m = 'connection number'.
    APPEND gs_field_cat2 TO gt_field_cat2.
    gs_field_cat2-col_pos = 3.
    gs_field_cat2-fieldname = 'CITYFROM'.
    gs_field_cat2-scrtext_m = 'city from'.
    APPEND gs_field_cat2 TO gt_field_cat2.
    gs_field_cat2-col_pos = 4.
    gs_field_cat2-fieldname = 'CITYTO'.
    gs_field_cat2-scrtext_m = 'city to'.
    APPEND gs_field_cat2 TO gt_field_cat2.
  ENDFORM.