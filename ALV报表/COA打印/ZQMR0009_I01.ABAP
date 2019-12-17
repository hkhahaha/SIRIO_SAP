*&---------------------------------------------------------------------*
*& 包含               ZQMR0009_I01
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE gv_ok_code.
    WHEN 'CANC'.
      CLEAR gv_ok_code.
      LEAVE TO SCREEN 0.
    WHEN 'BACK' OR 'EXIT' .
      IF NOT grf_document IS INITIAL.
        CALL METHOD grf_document->release_document.
        FREE grf_document.
      ENDIF.
      IF NOT grf_control IS INITIAL.
        CALL METHOD grf_control->destroy_control.
        FREE grf_control.
      ENDIF.
      CLEAR gv_ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.