*Report ZQMR0010
*黄铠
*QM_019 设计规范 打印-取样单
*BEGIN OF @001
*                    _ooOoo_
*                   o8888888o
*                   88" . "88
*                   (| -_- |)
*                    O\ = /O
*                ____/`---'\____
*              .   ' \\| |// `.
*               / \\||| : |||// \
*             / _||||| -:- |||||- \
*               | | \\\ - /// | |
*             | \_| ''\---/'' | |
*              \ .-\__ `-` ___/-. /
*           ___`. .' /--.--\ `. . __
*        ."" '< `.___\_<|>_/___.' >'"".
*       | | : `- \`.;`\ _ /`;.`/ - ` : | |
*         \ \ `-. \_ __\ /__ _/ .-` / /
* ======`-.____`-.___\_____/___.-`____.-'======
*                    `=---='
*
* .............................................
*          佛祖保佑             永无BUG
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
  PERFORM frm_get_data.
  PERFORM catalog.
  PERFORM alvshow.
*  PERFORM output.

END-OF-SELECTION.