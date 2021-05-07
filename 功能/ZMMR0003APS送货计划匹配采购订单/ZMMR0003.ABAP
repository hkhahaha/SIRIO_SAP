*-----------------------------------------------------------------------
* TITLE           : APS送货计划匹配采购订单
* AUTHOR          : Huxy
* DATE            : 2019.08.08
* DEVELOPMENT ID  :
* DESCRIPTION     : APS送货计划匹配采购订单
*                 :
*-----------------------------------------------------------------------
* CHANGE HISTORY LOG
*-----------------------------------------------------------------------
*  DATE  | AUTHOR     | CHANGE REF | DESCRIPTION
*-----------------------------------------------------------------------
* 20200420 |   lanjingcun         |            | 对于收货数量相关的调整
*-----------------------------------------------------------------------
* 20200426 |   lanjingcun         |            | 新增备注 及调整答复日期 发送行不做合并(取消)
*-----------------------------------------------------------------------
* 20200519 |   lanjingcun         |            | 送货数量大于等于分配数量下发的时候不提示成功
*-----------------------------------------------------------------------
* 20200522 |   lanjingcun         |            | 自动分配计算逻辑不正确处理 设置筛选器（因为直接排除数据导致计算已分配数据等出现不完整）
*-----------------------------------------------------------------------
* 20200520 |   lanjingcun         |            | 增加可收数量字段
*-----------------------------------------------------------------------
* 20200705 |   lanjingcun         |            | 按供应商判断是否进行拆分
*-----------------------------------------------------------------------
* 20201103 |   lanjingcun         |            | 未发送下发不再需要合并的功能
*-----------------------------------------------------------------------


REPORT zmmr0003.

INCLUDE zmmr0003_top.  "变量定义
INCLUDE zmmr0003_class."ALV监听类
INCLUDE zmmr0003_scr.  "选择屏幕
INCLUDE zmmr0003_excel."EXCEL处理相关
INCLUDE zmmr0003_pbo.  "PBO处理
INCLUDE zmmr0003_pai.  "PAI处理
INCLUDE zmmr0003_f01.  "分配PO子程序
INCLUDE zmmr0003_f02.  "ALV OO相关
INCLUDE zmmr0003_f03.  "EXCEL上传功能相关子程序
INCLUDE zmmr0003_f05.  "供应商送货计划关闭相关子程序
INCLUDE zmmr0003_f04.  "普通ALV相关
INCLUDE zca_authority_check.

*** initial data
INITIALIZATION.
  PERFORM frm_init.

*** prepare report data
START-OF-SELECTION.

  PERFORM frm_screen_check.    "屏幕检查
  PERFORM frm_authority_check. "权限检查
  PERFORM frm_get_data.        "获取数据

  CASE 'X'.
    WHEN p_r02.
      CALL SCREEN gv_screen.
    WHEN p_r05.
      PERFORM frm_show_data TABLES gt_alv.
    WHEN p_r06.
      PERFORM frm_show_data TABLES gt_pdlv_data.
  ENDCASE.

*** output report
END-OF-SELECTION.