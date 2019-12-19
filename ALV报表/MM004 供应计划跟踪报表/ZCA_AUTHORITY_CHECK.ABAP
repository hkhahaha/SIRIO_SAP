*&---------------------------------------------------------------------*
*& 包含               ZCA_AUTHORITY_CHECK
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_M_BEST_WRK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_WERKS  text
*----------------------------------------------------------------------*
FORM frm_check_m_best_wrk TABLES it_werks.

    SELECT werks
      INTO TABLE @DATA(lt_werks)
      FROM t001w
     WHERE werks IN @it_werks.
    IF lt_werks[] IS INITIAL.
      MESSAGE s001(zauthority) DISPLAY LIKE 'E' WITH '工厂'.
      STOP.
    ENDIF.
 
    LOOP AT lt_werks INTO DATA(ls_werks).
      AUTHORITY-CHECK OBJECT 'M_BEST_WRK'
               ID 'WERKS' FIELD ls_werks-werks.
      IF sy-subrc <> 0.
        MESSAGE s002(zauthority) DISPLAY LIKE 'E' WITH '工厂' ls_werks-werks.
        STOP.
      ENDIF.
    ENDLOOP.
 
  ENDFORM.
 
 *&---------------------------------------------------------------------*
 *&      Form  FRM_CHECK_M_BEST_EKG
 *&---------------------------------------------------------------------*
 *       text
 *----------------------------------------------------------------------*
 *      -->it_ekgrp  text
 *----------------------------------------------------------------------*
  FORM frm_check_m_best_ekg TABLES it_ekgrp .
 
    SELECT ekgrp
      INTO TABLE @DATA(lt_ekgrp)
      FROM t024
     WHERE ekgrp IN @it_ekgrp.
    IF lt_ekgrp[] IS INITIAL.
      MESSAGE s001(zauthority) DISPLAY LIKE 'E' WITH '采购组'.
      STOP.
    ENDIF.
 
    LOOP AT lt_ekgrp INTO DATA(ls_ekgrp).
      AUTHORITY-CHECK OBJECT 'M_BEST_EKG'
               ID 'EKGRP' FIELD ls_ekgrp-ekgrp.
      IF sy-subrc <> 0.
        MESSAGE s002(zauthority) DISPLAY LIKE 'E' WITH '采购组' ls_ekgrp-ekgrp.
        STOP.
      ENDIF.
    ENDLOOP.
 
  ENDFORM.
 
 *&---------------------------------------------------------------------*
 *&      Form  FRM_CHECK_M_BEST_EKG1
 *&---------------------------------------------------------------------*
 *       text
 *----------------------------------------------------------------------*
  FORM frm_check_m_best_ekg1 USING uv_ekgrp CHANGING cv_success .
 
    CLEAR cv_success.
 
    AUTHORITY-CHECK OBJECT 'M_BEST_EKG'
             ID 'EKGRP' FIELD uv_ekgrp.
    IF sy-subrc <> 0.
      cv_success = 'E'.
    ENDIF.
 
  ENDFORM.