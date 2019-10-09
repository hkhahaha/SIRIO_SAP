*Report ZQMR0010
*黄铠
*QM_019 设计规范 打印-取样单

INCLUDE zqmr0010_top.
*INCLUDE ZQMR0009_TOP.                         .    " Global Data
INCLUDE zqmr0010_o01.
*INCLUDE ZQMR0009_O01                            .  " PBO-Modules
INCLUDE zqmr0010_i01.
*INCLUDE ZQMR0009_I01                            .  " PAI-Modules
INCLUDE zqmr0010_f01.
*INCLUDE ZQMR0009_F01                            .  " FORM-Routines
AT SELECTION-SCREEN.
*  PERFORM frm_check_message.

START-OF-SELECTION.
  PERFORM frm_authority_check.
  CHECK gv_sel_error IS INITIAL."权限检查标识
  PERFORM frm_get_data.
  PERFORM catalog.
  PERFORM alvshow.
*  PERFORM output.

END-OF-SELECTION.