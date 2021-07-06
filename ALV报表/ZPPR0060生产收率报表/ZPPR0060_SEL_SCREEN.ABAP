*&---------------------------------------------------------------------*
*& 包含               ZPPR0058_SEL_SCREEN
*&---------------------------------------------------------------------*
"选择屏幕
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_dwerk FOR zppsprofis-dwerk OBLIGATORY, "工厂
                s_matnr FOR zppsprofis-matnr , "物料
                s_matkl FOR zppsprofis-matkl , "物料组
                s_charg FOR zppsprofis-charg, "批号
                s_gstri FOR zppsprofis-gstri, "实际开始日期
                s_zchej FOR zppsprofis-zchej, "车间
                s_zchanx FOR zppsprofis-zchanx. "产线
SELECTION-SCREEN END OF BLOCK blk1.