FUNCTION z_mm_lgort_chang.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(INPUT) TYPE  ZMT_OA2OTHER_MDM_RET
*"  EXPORTING
*"     VALUE(FLAG) TYPE  CHAR01
*"----------------------------------------------------------------------


  DATA:i_headdata             LIKE  bapimathead,
       i_storagelocationdata  LIKE  bapi_mard,
       i_storagelocationdatax LIKE  bapi_mardx,
       i_return               LIKE  bapiret2.

  SELECT SINGLE
    mtart
  INTO @DATA(lv_mtart)
  FROM mara
  WHERE mara~matnr = @input-mt_oa2other_mdm_ret-zmd001-msg_matnr.

  SELECT
    matnr,
    werks
  INTO TABLE @DATA(lt_marc)
  FROM marc
  WHERE marc~matnr = @input-mt_oa2other_mdm_ret-zmd001-msg_matnr.

  LOOP AT lt_marc INTO DATA(ls_marc).

    SELECT
      *
    INTO TABLE @DATA(lt_zmmt046)
    FROM zmmt046.

    SELECT
      mard~matnr,
      werks,
      lgort,
      mtart
    INTO TABLE @DATA(lt_mard)
    FROM mard
    INNER JOIN mara
    ON mard~matnr = mara~matnr
    WHERE mard~matnr = @ls_marc-matnr
      AND mard~werks = @ls_marc-werks.

    LOOP AT lt_zmmt046 INTO DATA(ls_zmmt046) WHERE mtart = lv_mtart.
      READ TABLE lt_mard INTO DATA(ls_mard) WITH KEY werks = ls_zmmt046-werks
                                                     mtart = lv_mtart
                                                     lgort = ls_zmmt046-lgort.
      IF sy-subrc <> 0.
        CLEAR i_headdata.
        i_headdata-material = input-mt_oa2other_mdm_ret-zmd001-msg_matnr.
        i_headdata-storage_view = 'X'.
        CLEAR i_storagelocationdata.
        CLEAR i_storagelocationdatax.
        i_storagelocationdata-plant = ls_zmmt046-werks.
        i_storagelocationdatax-plant = ls_zmmt046-werks.
        i_storagelocationdata-stge_loc = ls_zmmt046-lgort.
        i_storagelocationdatax-stge_loc = ls_zmmt046-lgort.

        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
          EXPORTING
            headdata             = i_headdata
            storagelocationdata  = i_storagelocationdata
            storagelocationdatax = i_storagelocationdatax
          IMPORTING
            return               = i_return.
        IF i_return-type = 'S'.
          flag = 'X'.
          COMMIT WORK.
        ELSE.
          flag = 'E'.
        ENDIF.

      ENDIF.
      CLEAR ls_zmmt046.
      CLEAR ls_mard.
    ENDLOOP.
    CLEAR ls_marc.
  ENDLOOP.
ENDFUNCTION.