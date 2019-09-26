  METHOD if_ex_me_process_po_cust~check.
    DATA ls_header TYPE mepoheader.
    ls_header = im_header->get_data( ).
    IF ls_header-bsart <> 'ZNF'.
      IF sy-tcode = 'ME29N' OR sy-tcode = 'ME28'.
        CALL FUNCTION 'Z_MM_SAP2WMS_OUT_FUN_OUT'
          EXPORTING
            im_header = im_header
          IMPORTING
            ch_failed = ch_failed.
      ENDIF.
    ENDIF.
  ENDMETHOD.