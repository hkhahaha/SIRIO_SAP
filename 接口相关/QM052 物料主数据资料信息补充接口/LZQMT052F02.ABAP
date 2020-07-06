*----------------------------------------------------------------------*
***INCLUDE LZQMT052F02.
*----------------------------------------------------------------------*
FORM zqmt052f01.
    SELECT SINGLE
    maktx
    INTO zqmt052-maktx
    FROM makt
    WHERE makt~spras = '1'
    AND makt~matnr = zqmt052-matnr.
  
    zqmt052-zlast_account = sy-uname.
    zqmt052-zlast_date = sy-datum.
    zqmt052-id = zcl_bc_public=>get_guid( ).
  
    IF zqmt052-zcreate_date IS INITIAL.
      zqmt052-zcreate_account = sy-uname.
      zqmt052-zcreate_date = sy-datum.
    ENDIF.
  ENDFORM.