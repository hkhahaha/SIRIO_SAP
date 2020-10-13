*&---------------------------------------------------------------------*
*& Report ZBOM_CHECK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZHKDEMO.


TYPES : BEGIN OF ltype_bommat,
  matnr TYPE mara-matnr,
  werks TYPE marc-werks,
END OF ltype_bommat.

DATA : lt_bommat TYPE TABLE OF ltype_bommat,
      ls_bommat LIKE LINE OF lt_bommat.

DATA: it_wultb   LIKE stpov OCCURS 0 WITH HEADER LINE,
      it_wultb2  LIKE stpov OCCURS 0 WITH HEADER LINE,
      it_wultb3  LIKE stpov OCCURS 0 WITH HEADER LINE,
      it_wultb4  LIKE stpov OCCURS 0 WITH HEADER LINE,
      it_wultb5  LIKE stpov OCCURS 0 WITH HEADER LINE,
      it_wultb6  LIKE stpov OCCURS 0 WITH HEADER LINE,
      it_wultb7  LIKE stpov OCCURS 0 WITH HEADER LINE,
      it_equict  LIKE cscequi OCCURS 0 WITH HEADER LINE,
      it_kncat   LIKE cscknd OCCURS 0 WITH HEADER LINE,
      it_matcat  LIKE cscmat OCCURS 0 WITH HEADER LINE,
      it_stdct   LIKE cscstd OCCURS 0 WITH HEADER LINE,
      it_tplca   LIKE csctpl OCCURS 0 WITH HEADER LINE,
      it_bom     LIKE bom_header_api01 OCCURS 0 WITH HEADER LINE,
      it_bom_rev LIKE bom_header_api01 OCCURS 0 WITH HEADER LINE.

PARAMETERS : p_werks LIKE marc-werks.
PARAMETERS : p_matnr LIKE marc-matnr.

IF p_werks IS NOT INITIAL AND p_matnr IS NOT INITIAL.

  CLEAR:it_wultb,it_wultb[].
  CALL FUNCTION 'CS_WHERE_USED_MAT'
  EXPORTING
    datub                      = sy-datum
    datuv                      = sy-datum
    matnr                      = p_matnr
    werks                      = p_werks
  TABLES
    wultb                      = it_wultb
    equicat                    = it_equict
    kndcat                     = it_kncat
    matcat                     = it_matcat
    stdcat                     = it_stdct
    tplcat                     = it_tplca
  EXCEPTIONS
    call_invalid               = 1
    material_not_found         = 2
    no_where_used_rec_found    = 3
    no_where_used_rec_selected = 4
    no_where_used_rec_valid    = 5
    OTHERS                     = 6.

  IF it_wultb IS INITIAL.
    CLEAR ls_bommat.
    ls_bommat-matnr = p_matnr.
    ls_bommat-werks = p_werks.
    APPEND ls_bommat TO lt_bommat.

  ELSE.

    LOOP AT it_wultb.

      CLEAR:it_wultb2,it_wultb2[].
      CALL FUNCTION 'CS_WHERE_USED_MAT'
      EXPORTING
        datub                      = sy-datum
        datuv                      = sy-datum
        matnr                      = it_wultb-matnr
        werks                      = it_wultb-werks
      TABLES
        wultb                      = it_wultb2
        equicat                    = it_equict
        kndcat                     = it_kncat
        matcat                     = it_matcat
        stdcat                     = it_stdct
        tplcat                     = it_tplca
      EXCEPTIONS
        call_invalid               = 1
        material_not_found         = 2
        no_where_used_rec_found    = 3
        no_where_used_rec_selected = 4
        no_where_used_rec_valid    = 5
        OTHERS                     = 6.

      IF it_wultb2 IS INITIAL.
        CLEAR ls_bommat.
        ls_bommat-matnr = it_wultb-matnr.
        ls_bommat-werks = it_wultb-werks.
        APPEND ls_bommat TO lt_bommat.

      ELSE.

        LOOP AT it_wultb2.
          CLEAR:it_wultb3,it_wultb3[].
          CALL FUNCTION 'CS_WHERE_USED_MAT'
          EXPORTING
            datub                      = sy-datum
            datuv                      = sy-datum
            matnr                      = it_wultb2-matnr
            werks                      = it_wultb2-werks
          TABLES
            wultb                      = it_wultb3
            equicat                    = it_equict
            kndcat                     = it_kncat
            matcat                     = it_matcat
            stdcat                     = it_stdct
            tplcat                     = it_tplca
          EXCEPTIONS
            call_invalid               = 1
            material_not_found         = 2
            no_where_used_rec_found    = 3
            no_where_used_rec_selected = 4
            no_where_used_rec_valid    = 5
            OTHERS                     = 6.

          IF it_wultb3 IS INITIAL.

            CLEAR ls_bommat.
            ls_bommat-matnr = it_wultb2-matnr.
            ls_bommat-werks = it_wultb2-werks.
            APPEND ls_bommat TO lt_bommat.

          ELSE.

            LOOP AT it_wultb3.
              CLEAR:it_wultb4,it_wultb4[].
              CALL FUNCTION 'CS_WHERE_USED_MAT'
              EXPORTING
                datub                      = sy-datum
                datuv                      = sy-datum
                matnr                      = it_wultb3-matnr
                werks                      = it_wultb3-werks
              TABLES
                wultb                      = it_wultb4
                equicat                    = it_equict
                kndcat                     = it_kncat
                matcat                     = it_matcat
                stdcat                     = it_stdct
                tplcat                     = it_tplca
              EXCEPTIONS
                call_invalid               = 1
                material_not_found         = 2
                no_where_used_rec_found    = 3
                no_where_used_rec_selected = 4
                no_where_used_rec_valid    = 5
                OTHERS                     = 6.

              IF it_wultb4 IS INITIAL.
                CLEAR ls_bommat.
                ls_bommat-matnr = it_wultb3-matnr.
                ls_bommat-werks = it_wultb3-werks.
                APPEND ls_bommat TO lt_bommat.

              ELSE.

                LOOP AT it_wultb4.
                  CLEAR:it_wultb5,it_wultb5[].
                  CALL FUNCTION 'CS_WHERE_USED_MAT'
                  EXPORTING
                    datub                      = sy-datum
                    datuv                      = sy-datum
                    matnr                      = it_wultb4-matnr
                    werks                      = it_wultb4-werks
                  TABLES
                    wultb                      = it_wultb5
                    equicat                    = it_equict
                    kndcat                     = it_kncat
                    matcat                     = it_matcat
                    stdcat                     = it_stdct
                    tplcat                     = it_tplca
                  EXCEPTIONS
                    call_invalid               = 1
                    material_not_found         = 2
                    no_where_used_rec_found    = 3
                    no_where_used_rec_selected = 4
                    no_where_used_rec_valid    = 5
                    OTHERS                     = 6.

                  IF it_wultb5 IS INITIAL.
                    CLEAR ls_bommat.
                    ls_bommat-matnr = it_wultb4-matnr.
                    ls_bommat-werks = it_wultb4-werks.
                    APPEND ls_bommat TO lt_bommat.

                  ELSE.

                    LOOP AT it_wultb5.
                      CLEAR:it_wultb6,it_wultb6[].
                      CALL FUNCTION 'CS_WHERE_USED_MAT'
                      EXPORTING
                        datub                      = sy-datum
                        datuv                      = sy-datum
                        matnr                      = it_wultb5-matnr
                        werks                      = it_wultb5-werks
                      TABLES
                        wultb                      = it_wultb6
                        equicat                    = it_equict
                        kndcat                     = it_kncat
                        matcat                     = it_matcat
                        stdcat                     = it_stdct
                        tplcat                     = it_tplca
                      EXCEPTIONS
                        call_invalid               = 1
                        material_not_found         = 2
                        no_where_used_rec_found    = 3
                        no_where_used_rec_selected = 4
                        no_where_used_rec_valid    = 5
                        OTHERS                     = 6.

                      IF it_wultb6 IS INITIAL.

                        CLEAR ls_bommat.
                        ls_bommat-matnr = it_wultb5-matnr.
                        ls_bommat-werks = it_wultb5-werks.
                        APPEND ls_bommat TO lt_bommat.

                      ELSE.

                        LOOP AT it_wultb6.
                          CLEAR:it_wultb7,it_wultb7[].
                          CALL FUNCTION 'CS_WHERE_USED_MAT'
                          EXPORTING
                            datub                      = sy-datum
                            datuv                      = sy-datum
                            matnr                      = it_wultb6-matnr
                            werks                      = it_wultb6-werks
                          TABLES
                            wultb                      = it_wultb7
                            equicat                    = it_equict
                            kndcat                     = it_kncat
                            matcat                     = it_matcat
                            stdcat                     = it_stdct
                            tplcat                     = it_tplca
                          EXCEPTIONS
                            call_invalid               = 1
                            material_not_found         = 2
                            no_where_used_rec_found    = 3
                            no_where_used_rec_selected = 4
                            no_where_used_rec_valid    = 5
                            OTHERS                     = 6.

                        ENDLOOP.


                        IF it_wultb7 IS INITIAL.

                          CLEAR ls_bommat.
                          ls_bommat-matnr = it_wultb6-matnr.
                          ls_bommat-werks = it_wultb6-werks.
                          APPEND ls_bommat TO lt_bommat.

                        ELSE.

                          MESSAGE 'BOM层数过多' TYPE 'E'.

                        ENDIF.

                      ENDIF.

                    ENDLOOP.

                  ENDIF.

                ENDLOOP.

              ENDIF.

            ENDLOOP.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDLOOP.
  ENDIF.
ENDIF.
TYPE-POOLS: slis.
DATA: it_fieldcat TYPE slis_t_fieldcat_alv,
      it_layout   TYPE slis_layout_alv.
DATA:   lv_repid LIKE sy-repid.
DATA:   it_ls_fieldcat TYPE slis_fieldcat_alv.

PERFORM frm_alv_display.

*&---------------------------------------------------------------------*
*&      Form  frm_alv_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_alv_display .

  lv_repid = sy-repid.
  it_layout-detail_popup = 'X'.
  it_layout-info_fieldname = 'LINECOLOR'.
  REFRESH :it_fieldcat[].
  PERFORM frm_fieldcat_init.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_callback_program = lv_repid
    it_fieldcat        = it_fieldcat[]
    is_layout          = it_layout
    i_default          = 'X'
    i_save             = 'A'
*     i_grid_title       = result
  TABLES
    t_outtab           = lt_bommat.
ENDFORM.                    " FRM_ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_fieldcat_init.
  DEFINE fieldcat.
    CLEAR  it_ls_fieldcat.
    it_ls_fieldcat-col_pos       =  1.
    it_ls_fieldcat-fieldname     = &1.
    it_ls_fieldcat-seltext_l     = &2.
    it_ls_fieldcat-outputlen     = &3.
    it_ls_fieldcat-KEY           = &4.
    it_ls_fieldcat-seltext_m = it_ls_fieldcat-seltext_l.
    it_ls_fieldcat-seltext_s = it_ls_fieldcat-seltext_l.
    APPEND it_ls_fieldcat TO it_fieldcat.
    CLEAR  it_ls_fieldcat.
  END-OF-DEFINITION.
  fieldcat          'MATNR'              '物料'              '20'          'X'  .
  fieldcat          'WERKS'              '工厂'                '8'          'X'  .
ENDFORM.                    "frm_fieldcat_init