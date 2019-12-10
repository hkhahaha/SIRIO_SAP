*&---------------------------------------------------------------------*
*& 包含               ZQMR008_SCR
*&---------------------------------------------------------------------*

*初始化时下拉框赋值
DATA: vid   TYPE vrm_id , "屏幕字段(可以是单个的I/O空间或者是Table Control中的一个单元格)
      list  TYPE vrm_values,
      value LIKE LINE OF list.
*&---------------------------------------------------------------------*
*& 程序名：ZQMR008_SCR
*&作者：Seashell Huang
*&模块：
*&创建日期：02.12.2019 15:35:02
*&功能描述：
*&---------------------------------------------------------------------*
*&修改记录：添加前台工作重心字段的查询
*&
*&---------------------------------------------------------------------*
DATA:init.
*********************修改结束位置02.12.2019 15:36:47*************

SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE t0.
SELECT-OPTIONS:
s_aendt FOR qprs-aendt, "接检日期
s_kdauf FOR afpo-kdauf,"销售订单号
s_prfql FOR qpmk-prfql,"检验岗位
s_lagort FOR qals-lagortchrg,"库存地
s_art FOR qals-art,"检验类型
s_matnr FOR  qals-matnr, " 物料编码
*s_werk FOR   qals-werk," 工厂
s_flos FOR  qals-prueflos, "检验批
s_charg FOR   qals-charg."批次
PARAMETERS s_werk TYPE werks_d OBLIGATORY. " 工厂
PARAMETERS s_lsbox(2) TYPE c  AS LISTBOX VISIBLE LENGTH 20 DEFAULT ''. " 取样状态
PARAMETERS:p_lang(20) AS LISTBOX VISIBLE LENGTH 22 DEFAULT 'QC'OBLIGATORY.

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

  PERFORM setlist.



START-OF-SELECTION.

  ......

END-OF-SELECTION.

*子程序用于加载下拉框的数据
FORM setlist.
  TYPE-POOLS vrm.
  DATA myvalue TYPE vrm_values WITH HEADER LINE.
  DATA myvalue2 TYPE vrm_values WITH HEADER LINE.

*对内表加载值
  myvalue-key = 'QA'. myvalue-text = 'QA'. APPEND myvalue.

  myvalue-key = 'QC'. myvalue-text = 'QC'. APPEND myvalue.


  IF init IS INITIAL.
    CALL FUNCTION 'VRM_SET_VALUES' "调用函数对下拉框对象传递数据
      EXPORTING
        id              = 'P_LANG' "下拉框对象名
        values          = myvalue[]  "下拉框中加载的数据
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.

  ENDIF.

  init = 'X'. "记录初始化状态

ENDFORM.