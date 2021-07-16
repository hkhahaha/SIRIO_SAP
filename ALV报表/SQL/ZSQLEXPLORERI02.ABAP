************************************************************************
*^ Written By      : Hxy
*^ Date Written    : 2014/08/18
*^ Include Name    : ZSQLEXPLORERI02
*^ Used in Programs: <Programs referencing this include>
*^ Purpose         : To Define Screen 200 PAI
*
*^ Other           :
************************************************************************


******************************************************
*&      Module  USER_COMMAND_0200  INPUT
******************************************************


MODULE user_command_0200 INPUT.

  CASE g_ucomm .

  WHEN 'CONF'.
    CASE 'X'.
    WHEN l_case_01 .
      g_case = 1 .
    WHEN l_case_02 .
      g_case = 2 .
    WHEN l_case_03 .
      g_case = 3 .
    ENDCASE .

    CASE 'X'.
    WHEN l_label_01 .
      g_label = 1 .
    WHEN l_label_02 .
      g_label = 2 .
    ENDCASE .

  WHEN 'CANC'.
  ENDCASE .

  CLEAR : g_ucomm .

ENDMODULE. " USER_COMMAND_0200 INPUT