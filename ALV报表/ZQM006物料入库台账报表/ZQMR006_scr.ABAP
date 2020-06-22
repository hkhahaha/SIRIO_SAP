*&---------------------------------------------------------------------*
*& 包含               ZQMR006_SCR
*&---------------------------------------------------------------------*
*初始化时下拉框赋值
DATA: vid   TYPE vrm_id , "屏幕字段(可以是单个的I/O空间或者是Table Control中的一个单元格)
      list  TYPE vrm_values,
      value LIKE LINE OF list.

SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE t0.
SELECT-OPTIONS: s_matnr FOR  qals-matnr, " 物料编码
                s_maktx FOR  makt-maktx, "物料品名
                s_charg FOR qals-charg, "批次
*                s_werk  FOR   qals-werk," 工厂
*                s_iifnr FOR  qals-sellifnr, "供应商
                 s_lifnr FOR  qals-lifnr, "供应商
                s_herst FOR  qals-hersteller, "制造商
                s_endat FOR  qals-enstehdat."检验批创建日期
PARAMETERS s_werk TYPE werks_d OBLIGATORY. " 工厂
PARAMETERS s_lsbox(2) TYPE c  AS LISTBOX VISIBLE LENGTH 20 DEFAULT ''. " 取样状态
*                s_samstatus FOR  ausp-samstatus."取样状态


SELECTION-SCREEN END OF BLOCK blk.

*初始化时下拉框赋值
INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

  REFRESH list .

  value-key = '10' . "这个就是变量P_LIST的值
  value-text = '已取样' . "这个是text
  APPEND value TO list .

  value-key = '20' .
  value-text = '未取样' .
  APPEND value TO list .

**调用下拉框赋值函数
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'S_LSBOX'
      values = list.





START-OF-SELECTION.

  ......

END-OF-SELECTION.