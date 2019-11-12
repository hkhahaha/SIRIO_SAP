*&---------------------------------------------------------------------*
*& Report ZQMR0005
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zqmr0005.

INCLUDE zqmr0005_top.
INCLUDE zqmr0005_src.
INCLUDE zqmr0005_f01.



START-OF-SELECTION.
*  PERFORM frm_authority_check.
  AUTHORITY-CHECK OBJECT 'Q_INSPLOT'
    ID 'WERKS' FIELD p_werks.
  IF sy-subrc NE 0.
    MESSAGE '无权限操作当前工厂！' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  SELECT DISTINCT makt~maktx, mapl~werks, mapl~plnnr,mapl~matnr, mapl~plnal,plko~ktext",plmk~qdynregel,plmk~dummy40
    FROM mapl
    JOIN makt ON mapl~matnr = makt~matnr
    JOIN plko ON plko~plnty = mapl~plnty AND plko~plnnr = mapl~plnnr AND plko~plnal = mapl~plnal "AND plko~zaehl = mapl~zaehl
    WHERE mapl~matnr IN @s_matnr
    AND mapl~werks = @p_werks
    AND mapl~plnty = 'Q'
    AND mapl~plnnr IN @s_plnnr
    AND mapl~loekz = ''
    AND plko~loekz = ''

    INTO CORRESPONDING FIELDS OF TABLE @gt_output.

END-OF-SELECTION.
  "cl_demo_output=>display( gt_output ).
  PERFORM frm_dispaly_alv.