METHOD if_ex_smod_qpl10001~exit_saplqpl1_002.
*    BREAK abap05.
        CALL FUNCTION 'ZQM015_UPD_SAMPQTY'
          EXPORTING
            i_qals   = i_qals
          IMPORTING
            e_qals   = e_qals
            e_active = e_active.
    
      ENDMETHOD.