*----------------------------------------------------------------------*
***INCLUDE LZQMT052F05.
*----------------------------------------------------------------------*
FORM zqmt052f2.
    IF zqmt052-id IS INITIAL.
      zqmt052-id = zcl_bc_public=>get_guid( ).
    ENDIF.
  
    CLEAR zqmt052-maktx.
    SELECT SINGLE
      maktx
    FROM makt
    INTO zqmt052-maktx
    WHERE makt~matnr = zqmt052-matnr
      AND makt~spras = '1'.
  
    IF zqmt052-zcreate_date IS INITIAL.
      zqmt052-zcreate_account = sy-uname.
      zqmt052-zcreate_date = sy-datum.
    ENDIF.
    IF zqmt052-zcreate_date IS NOT INITIAL.
      zqmt052-zlast_account = sy-uname.
      zqmt052-zlast_date = sy-datum.
    ENDIF.
  ENDFORM.