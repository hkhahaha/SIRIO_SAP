*&---------------------------------------------------------------------*
*& 包含               ZQMR0010_I01
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE gv_ok_code.
    WHEN 'CANC' OR 'BACK' OR 'EXIT'.
      CLEAR gv_ok_code.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.